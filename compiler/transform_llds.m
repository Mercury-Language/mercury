%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001,2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: transform_llds
%
% Main authors: petdr
%
% This module does source to source transformations of the llds data
% structure.  This is sometimes necessary to avoid limits in some
% compilers.
%
% This module currently transforms computed gotos into a binary search
% down to smaller computed gotos.  This avoids a limitation in the lcc
% compiler.
%
% If accurate GC is enabled, we also append a module containing an end label
% to the list of comp_gen_c_modules.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__transform_llds.

:- interface.

:- import_module ll_backend__llds.
:- import_module io.

:- pred transform_llds(c_file, c_file, io__state, io__state).
:- mode transform_llds(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_pred.
:- import_module backend_libs__builtin_ops.
:- import_module backend_libs__proc_label.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__opt_util.
:- import_module parse_tree__prog_data.

:- import_module bool, int, string, list, require, std_util, counter.

transform_llds(LLDS0, LLDS) -->
	transform_c_file(LLDS0, LLDS).

%-----------------------------------------------------------------------------%

:- pred transform_c_file(c_file, c_file, io__state, io__state).
:- mode transform_c_file(in, out, di, uo) is det.

transform_c_file(c_file(ModuleName, HeaderInfo, A, B, C, D, Modules0),
		c_file(ModuleName, HeaderInfo, A, B, C, D, Modules)) -->
	% split up large computed gotos
	globals__io_lookup_int_option(max_jump_table_size, MaxJumpTableSize),
	( { MaxJumpTableSize = 0 } ->
		{ Modules1 = Modules0 }
	;
		transform_c_module_list(Modules0, Modules1)
	),
	% append an end label for accurate GC
	globals__io_get_gc_method(GC),
	{ GC = accurate, Modules1 \= [] ->
		list__last_det(Modules1, LastModule),
		LastModule = comp_gen_c_module(LastModuleName, _),
		Modules = Modules1 ++
			[gen_end_label_module(ModuleName, LastModuleName)]
	;
		Modules = Modules1
	}.

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
		Arity, ProcId),
	Instrs = [label(local(ProcLabel)) -
		"label to indicate end of previous procedure"],
	DummyProc = c_procedure(PredName, Arity, proc(PredId, ProcId),
				Instrs, ProcLabel,
				counter__init(0), must_not_alter_rtti),
	EndLabelModule = comp_gen_c_module(LastModule ++ "_END", [DummyProc]).

%-----------------------------------------------------------------------------%

:- pred transform_c_module_list(list(comp_gen_c_module),
	list(comp_gen_c_module), io__state, io__state).
:- mode transform_c_module_list(in, out, di, uo) is det.

transform_c_module_list([], []) --> [].
transform_c_module_list([M0 | M0s], [M | Ms]) -->
	transform_c_module(M0, M),
	transform_c_module_list(M0s, Ms).

%-----------------------------------------------------------------------------%

:- pred transform_c_module(comp_gen_c_module, comp_gen_c_module,
	io__state, io__state).
:- mode transform_c_module(in, out, di, uo) is det.

transform_c_module(comp_gen_c_module(Name, Procedures0),
		comp_gen_c_module(Name, Procedures)) -->
	transform_c_procedure_list(Procedures0, Procedures).

%-----------------------------------------------------------------------------%

:- pred transform_c_procedure_list(list(c_procedure), list(c_procedure),
		io__state, io__state).
:- mode transform_c_procedure_list(in, out, di, uo) is det.

transform_c_procedure_list([], []) --> [].
transform_c_procedure_list([P0 | P0s], [P | Ps]) -->
	transform_c_procedure(P0, P),
	transform_c_procedure_list(P0s, Ps).

%-----------------------------------------------------------------------------%

:- pred transform_c_procedure(c_procedure, c_procedure, io__state, io__state).
:- mode transform_c_procedure(in, out, di, uo) is det.

transform_c_procedure(Proc0, Proc) -->
	{ Proc0 = c_procedure(Name, Arity, PPId, Instrs0,
		ProcLabel, C0, Recons) },
	{ Proc  = c_procedure(Name, Arity, PPId, Instrs,
		ProcLabel, C, Recons) },
	transform_instructions(Instrs0, ProcLabel, C0, C, Instrs).

%-----------------------------------------------------------------------------%

:- pred transform_instructions(list(instruction), proc_label, counter, counter,
		list(instruction), io__state, io__state).
:- mode transform_instructions(in, in, in, out, out, di, uo) is det.

transform_instructions(Instrs0, ProcLabel, C0, C, Instrs) -->
	transform_instructions_2(Instrs0, ProcLabel, C0, C, Instrs).

:- pred transform_instructions_2(list(instruction), proc_label,
		counter, counter, list(instruction), io__state, io__state).
:- mode transform_instructions_2(in, in, in, out, out, di, uo) is det.

transform_instructions_2([], _, C, C, []) --> [].
transform_instructions_2([Instr0 | Instrs0], ProcLabel, C0, C, Instrs) -->
	transform_instruction(Instr0, ProcLabel, C0, InstrsA, C1),
	transform_instructions_2(Instrs0, ProcLabel, C1, C, InstrsB),
	{ list__append(InstrsA, InstrsB, Instrs) }.

%-----------------------------------------------------------------------------%

:- pred transform_instruction(instruction, proc_label, counter,
		list(instruction), counter, io__state, io__state).
:- mode transform_instruction(in, in, in, out, out, di, uo) is det.

transform_instruction(Instr0, ProcLabel, C0, Instrs, C) -->
	globals__io_lookup_int_option(max_jump_table_size, Size),
	(
		{ Size \= 0 },
		{ Instr0 = computed_goto(_Rval, Labels) - _},
		{ list__length(Labels, L) },
		{ L > Size }
	->
		split_computed_goto(Instr0, Size, L, ProcLabel, C0, Instrs, C)
	;
		{ Instrs = [Instr0] },
		{ C = C0 }
	).

%-----------------------------------------------------------------------------%

	%
	% split_computed_goto(I, S, L, P, N0, Is, N)
	%
	% If instruction, I, is a computed_goto whose jump_table size is
	% greater then S, then split the table in half and insert the
	% instructions, Is, to do a binary search down to a jump_table
	% whose size is sufficiently small.
	%
:- pred split_computed_goto(instruction, int, int, proc_label, counter,
		list(instruction), counter, io__state, io__state).
:- mode split_computed_goto(in, in, in, in, in, out, out, di, uo) is det.

split_computed_goto(Instr0, MaxSize, Length, ProcLabel, C0, Instrs, C) -->
	(
		{ Length =< MaxSize }
	->
		{ Instrs = [Instr0] },
		{ C = C0 }
	;
		{ Instr0 = computed_goto(Rval, Labels) - _Comment }
	->
		{ counter__allocate(N, C0, C1) },
		{ Mid = Length // 2 },

		(
			{ list__split_list(Mid, Labels, Start0, End0) }
		->
			{ Start = Start0, End = End0 }
		;
			{ error("split_computed_goto: list__split_list") }
		),

		{ Index     = binop((-), Rval, const(int_const(Mid))) },
		{ Test      = binop((>=), Rval, const(int_const(Mid))) },
		{ ElseAddr  = label(local(N, ProcLabel)) },
		{ ElseLabel = label(local(N, ProcLabel)) - ""},
		{ IfInstr   = if_val(Test, ElseAddr ) - "Binary search"},

		{ ThenInstr = computed_goto(Rval, Start) - "Then section" },
		{ ElseInstr = computed_goto(Index, End) - "Else section" },

		split_computed_goto(ThenInstr, MaxSize, Mid, ProcLabel, C1,
				ThenInstrs, C2),
		split_computed_goto(ElseInstr, MaxSize, Length - Mid,
				ProcLabel, C2, ElseInstrs, C),

		{ list__append(ThenInstrs, [ElseLabel | ElseInstrs], InstrsA) },
		{ Instrs = [IfInstr | InstrsA] }
	;
		{ error("split_computed_goto") }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
