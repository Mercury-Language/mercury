%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001,2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Module: transform_llds.
% Main authors: petdr.

% This module does source to source transformations of the llds data
% structure.  This is sometimes necessary to avoid limits in some compilers.
%
% This module currently transforms computed gotos into a binary search down to
% smaller computed gotos.  This avoids a limitation in the lcc compiler.
%
% If accurate GC is enabled, we also append a module containing an end label
% to the list of comp_gen_c_modules.

%-----------------------------------------------------------------------------%

:- module ll_backend__transform_llds.
:- interface.

:- import_module ll_backend.llds.
:- import_module io.

:- pred transform_llds(c_file::in, c_file::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.opt_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

transform_llds(!LLDS, !IO) :-
    globals__io_get_globals(Globals, !IO),
    transform_c_file(!LLDS, Globals).

%-----------------------------------------------------------------------------%

:- pred transform_c_file(c_file::in, c_file::out, globals::in) is det.

transform_c_file(CFile0, CFile, Globals) :-
    CFile0 = c_file(ModuleName, _, _, _, _, _, Modules0, _, _),
    % split up large computed gotos
    globals__lookup_int_option(Globals, max_jump_table_size, MaxSize),
    ( MaxSize = 0 ->
        Modules1 = Modules0
    ;
        transform_c_module_list(Modules0, Modules1, MaxSize)
    ),
    % append an end label for accurate GC
    globals__get_gc_method(Globals, GC),
    ( GC = accurate, Modules1 \= [] ->
        list__last_det(Modules1, LastModule),
        LastModule = comp_gen_c_module(LastModuleName, _),
        Modules = Modules1 ++
            [gen_end_label_module(ModuleName, LastModuleName)]
    ;
        Modules = Modules1
    ),
    CFile = CFile0 ^ cfile_code := Modules.

%
% For LLDS native GC, we need to add a dummy comp_gen_c_module at the end of
% the list.  This dummy module contains only a single dummy procedure which
% in turn contains only a single label, for which there is no stack layout
% structure.  The point of this is to ensure that the address of this label
% gets inserted into the entry table, so that we know where the preceding
% procedure finishes when mapping from instruction pointer values to stack
% layout entries.
%
% Without this, we might think that the following C function was
% actually part of the last Mercury procedure in the preceding module,
% and then incorrectly use the stack layout of the Mercury procedure
% if we happened to get a heap overflow signal (SIGSEGV) while in that
% C function.
%
% Note that it is not sufficient to generate a label at end of the module,
% because GCC (e.g. GCC 3.2) sometimes reorders code within a single C
% function, so that a label declared at the end of the module might not
% be actually have highest address.  So we generate a new module (which
% corresponds to a new C function).  XXX Hopefully GCC won't mess with the
% order of the functions...
%

:- func gen_end_label_module(module_name, string) = comp_gen_c_module.

gen_end_label_module(ModuleName, LastModule) = EndLabelModule :-
    Arity = 0,
    ProcId = hlds_pred__initial_proc_id,
    PredId = hlds_pred__initial_pred_id,
    PredName = "ACCURATE_GC_END_LABEL",
    ProcLabel = proc(ModuleName, predicate, ModuleName, PredName,
        Arity, proc_id_to_int(ProcId)),
    Instrs = [label(entry(local, ProcLabel)) -
        "label to indicate end of previous procedure"],
    DummyProc = c_procedure(PredName, Arity, proc(PredId, ProcId),
        Instrs, ProcLabel, counter__init(0), must_not_alter_rtti),
    EndLabelModule = comp_gen_c_module(LastModule ++ "_END", [DummyProc]).

%-----------------------------------------------------------------------------%

:- pred transform_c_module_list(list(comp_gen_c_module)::in,
    list(comp_gen_c_module)::out, int::in) is det.

transform_c_module_list([], [], _MaxSize).
transform_c_module_list([Module0 | Module0s], [Module | Modules], MaxSize) :-
    transform_c_module(Module0, Module, MaxSize),
    transform_c_module_list(Module0s, Modules, MaxSize).

%-----------------------------------------------------------------------------%

:- pred transform_c_module(comp_gen_c_module::in, comp_gen_c_module::out,
    int::in) is det.

transform_c_module(comp_gen_c_module(Name, Procedures0),
        comp_gen_c_module(Name, Procedures), MaxSize) :-
    transform_c_procedure_list(Procedures0, Procedures, MaxSize).

%-----------------------------------------------------------------------------%

:- pred transform_c_procedure_list(list(c_procedure)::in,
    list(c_procedure)::out, int::in) is det.

transform_c_procedure_list([], [], _MaxSize).
transform_c_procedure_list([Proc0 | Proc0s], [Proc | Procs], MaxSize) :-
    transform_c_procedure(Proc0, Proc, MaxSize),
    transform_c_procedure_list(Proc0s, Procs, MaxSize).

%-----------------------------------------------------------------------------%

:- pred transform_c_procedure(c_procedure::in, c_procedure::out, int::in)
    is det.

transform_c_procedure(Proc0, Proc, MaxSize) :-
    Proc0 = c_procedure(_, _, _, Instrs0, ProcLabel, C0, _),
    transform_instructions(Instrs0, Instrs, C0, C, ProcLabel, MaxSize),
    Proc = (Proc0 ^ cproc_code := Instrs) ^ cproc_label_nums := C.

:- pred transform_instructions(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, proc_label::in, int::in) is det.

transform_instructions([], [], !C, _, _).
transform_instructions([Instr0 | Instrs0], Instrs, !C, ProcLabel, MaxSize) :-
    transform_instructions(Instrs0, InstrsTail, !C, ProcLabel, MaxSize),
    (
        Instr0 = computed_goto(Rval, Labels) - Comment,
        list__length(Labels, NumLabels),
        NumLabels > MaxSize
    ->
        split_computed_goto(Rval, Labels, Comment, InstrsHead, !C,
            MaxSize, NumLabels, ProcLabel),
        list__append(InstrsHead, InstrsTail, Instrs)
    ;
        Instrs = [Instr0 | InstrsTail]
    ).

%-----------------------------------------------------------------------------%

    % Given the pieces of a computed_goto instruction, split the table
    % in half as many times as necessary to bring the jump table size
    % below MaxSize, doing a binary search on the way.
    %
:- pred split_computed_goto(rval::in, list(label)::in, string::in,
    list(instruction)::out, counter::in, counter::out, int::in, int::in,
    proc_label::in) is det.

split_computed_goto(Rval, Labels, Comment, Instrs, !C, MaxSize, NumLabels,
        ProcLabel) :-
    ( NumLabels =< MaxSize ->
        Instrs = [computed_goto(Rval, Labels) - Comment]
    ;
        counter__allocate(LabelNum, !C),
        Mid = NumLabels // 2,
        ( list__split_list(Mid, Labels, StartPrime, EndPrime) ->
            Start = StartPrime,
            End = EndPrime
        ;
            unexpected(this_file, "split_computed_goto: list__split_list")
        ),

        Index     = binop(int_sub, Rval, const(int_const(Mid))),
        Test      = binop(int_ge,  Rval, const(int_const(Mid))),
        ElseAddr  = label(internal(LabelNum, ProcLabel)),
        IfInstr   = if_val(Test, ElseAddr) - "binary search",
        ElseInstr = label(internal(LabelNum, ProcLabel)) - "",

        split_computed_goto(Rval, Start, Comment ++ " then",
            ThenInstrs, !C, MaxSize, Mid, ProcLabel),
        split_computed_goto(Index, End, Comment ++ " else",
            ElseInstrs, !C, MaxSize, NumLabels - Mid, ProcLabel),

        list__append([IfInstr | ThenInstrs], [ElseInstr | ElseInstrs],
            Instrs)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "transform_llds.m".

%-----------------------------------------------------------------------------%
