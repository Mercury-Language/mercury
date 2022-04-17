%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001,2003-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: transform_llds.
% Main author: petdr.
%
% This module does source to source transformations of the llds data
% structure. This is sometimes necessary to avoid limits in some compilers.
%
% This module currently transforms computed gotos into a binary search down to
% smaller computed gotos. This avoids a limitation in the lcc compiler.
%
% If accurate GC is enabled, we also append a module containing an end label
% to the list of comp_gen_c_modules.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.transform_llds.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.llds.

%-----------------------------------------------------------------------------%

:- pred transform_llds(globals::in, c_file::in, c_file::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

transform_llds(Globals, !CFile) :-
    ModuleName = !.CFile ^ cfile_modulename,
    Modules0 = !.CFile ^ cfile_code,
    % Split up large computed gotos.
    globals.lookup_int_option(Globals, max_jump_table_size, MaxSize),
    ( if MaxSize = 0 then
        Modules1 = Modules0
    else
        transform_c_module_list(Modules0, Modules1, MaxSize)
    ),
    % Append an end label for accurate GC.
    globals.get_gc_method(Globals, GC),
    ( if
        GC = gc_accurate,
        Modules1 = [_ | _]
    then
        list.det_last(Modules1, LastModule),
        LastModule = comp_gen_c_module(LastModuleName, _),
        Modules = Modules1 ++
            [gen_end_label_module(ModuleName, LastModuleName)]
    else
        Modules = Modules1
    ),
    !CFile ^ cfile_code := Modules.

    % For LLDS native GC, we need to add a dummy comp_gen_c_module at the end
    % of the list. This dummy module contains only a single dummy procedure
    % which in turn contains only a single label, for which there is no
    % stack layout structure. The point of this is to ensure that the
    % address of this label gets inserted into the entry table, so that
    % we know where the preceding procedure finishes when mapping from
    % instruction pointer values to stack layout entries.
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
    % be actually have highest address. So we generate a new module (which
    % corresponds to a new C function). XXX Hopefully GCC won't mess with the
    % order of the functions ...
    %
:- func gen_end_label_module(module_name, string) = comp_gen_c_module.

gen_end_label_module(ModuleName, LastModule) = EndLabelModule :-
    Arity = 0,
    ProcId = hlds_pred.initial_proc_id,
    PredId = hlds_pred.initial_pred_id,
    PredName = "ACCURATE_GC_END_LABEL",
    ProcLabel = ordinary_proc_label(ModuleName, pf_predicate, ModuleName,
        PredName, Arity, proc_id_to_int(ProcId)),
    Instrs = [llds_instr(label(entry_label(entry_label_local, ProcLabel)),
        "label to indicate end of previous procedure")],
    DummyProc = c_procedure(PredName, Arity, proc(PredId, ProcId), ProcLabel,
        model_det, Instrs, counter.init(0), must_not_alter_rtti, set.init),
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

transform_c_module(Module0, Module, MaxSize) :-
    Module0 = comp_gen_c_module(Name, Procedures0),
    transform_c_procedure_list(Procedures0, Procedures, MaxSize),
    Module = comp_gen_c_module(Name, Procedures).

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

transform_c_procedure(!Proc, MaxSize) :-
    ProcLabel = !.Proc ^ cproc_proc_label,
    Instrs0 = !.Proc ^ cproc_code,
    C0 = !.Proc ^ cproc_label_nums,
    transform_instructions(Instrs0, Instrs, C0, C, ProcLabel, MaxSize),
    !Proc ^ cproc_code := Instrs,
    !Proc ^ cproc_label_nums := C.

:- pred transform_instructions(list(instruction)::in, list(instruction)::out,
    counter::in, counter::out, proc_label::in, int::in) is det.

transform_instructions([], [], !C, _, _).
transform_instructions([Instr0 | Instrs0], Instrs, !C, ProcLabel, MaxSize) :-
    transform_instructions(Instrs0, InstrsTail, !C, ProcLabel, MaxSize),
    ( if
        Instr0 = llds_instr(computed_goto(Rval, Targets), Comment),
        list.length(Targets, NumTargets),
        NumTargets > MaxSize
    then
        split_computed_goto(Rval, Targets, Comment, InstrsHead, !C,
            MaxSize, NumTargets, ProcLabel),
        list.append(InstrsHead, InstrsTail, Instrs)
    else
        Instrs = [Instr0 | InstrsTail]
    ).

%-----------------------------------------------------------------------------%

    % Given the pieces of a computed_goto instruction, split the table
    % in half as many times as necessary to bring the jump table size
    % below MaxSize, doing a binary search on the way.
    %
:- pred split_computed_goto(rval::in, list(maybe(label))::in, string::in,
    list(instruction)::out, counter::in, counter::out, int::in, int::in,
    proc_label::in) is det.

split_computed_goto(Rval, Targets, Comment, Instrs, !C, MaxSize, NumTargets,
        ProcLabel) :-
    ( if NumTargets =< MaxSize then
        Instrs = [llds_instr(computed_goto(Rval, Targets), Comment)]
    else
        counter.allocate(LabelNum, !C),
        Mid = NumTargets // 2,
        list.det_split_list(Mid, Targets, StartTargets, EndTargets),
        Index = binop(int_sub(int_type_int), Rval, const(llconst_int(Mid))),
        Test = binop(int_ge(int_type_int),  Rval, const(llconst_int(Mid))),
        ElseAddr  = code_label(internal_label(LabelNum, ProcLabel)),
        IfInstr   = llds_instr(if_val(Test, ElseAddr), "binary search"),
        ElseInstr = llds_instr(label(internal_label(LabelNum, ProcLabel)), ""),

        split_computed_goto(Rval, StartTargets, Comment ++ " then",
            ThenInstrs, !C, MaxSize, Mid, ProcLabel),
        split_computed_goto(Index, EndTargets, Comment ++ " else",
            ElseInstrs, !C, MaxSize, NumTargets - Mid, ProcLabel),

        Instrs = [IfInstr | ThenInstrs] ++ [ElseInstr | ElseInstrs]
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.transform_llds.
%-----------------------------------------------------------------------------%
