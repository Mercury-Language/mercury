%-----------------------------------------------------------------------------%
% Copyright (C) 1998-1999 The University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- module transform_llds.

:- interface.

:- import_module llds.

:- pred transform_llds(c_file, c_file, io__state, io__state).
:- mode transform_llds(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, options, opt_util, prog_data.
:- import_module bool, int, list, require, std_util.

transform_llds(LLDS0, LLDS) -->
	globals__io_lookup_int_option(max_jump_table_size, Size),
	(
		{ Size = 0 }
	->
		{ LLDS = LLDS0 }
	;
		transform_c_file(LLDS0, LLDS)
	).

%-----------------------------------------------------------------------------%

:- pred transform_c_file(c_file, c_file, io__state, io__state).
:- mode transform_c_file(in, out, di, uo) is det.

transform_c_file(c_file(ModuleName, HeaderInfo, A, B, C, Modules0),
		c_file(ModuleName, HeaderInfo, A, B, C, Modules)) -->
	transform_c_module_list(Modules0, Modules).

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

transform_c_procedure(c_procedure(Name, Arity, PredProcId, Instructions0),
		c_procedure(Name, Arity, PredProcId, Instructions)) -->
	transform_instructions(Instructions0, Instructions).

%-----------------------------------------------------------------------------%

:- pred transform_instructions(list(instruction), list(instruction),
		io__state, io__state).
:- mode transform_instructions(in, out, di, uo) is det.

transform_instructions(Instrs0, Instrs) -->
	{ opt_util__get_prologue(Instrs0, ProcLabel, _, _, _) },
	{ max_label_int(Instrs0, 0, N) },
	transform_instructions_2(Instrs0, ProcLabel, N, Instrs).

:- pred transform_instructions_2(list(instruction), proc_label, int,
		list(instruction), io__state, io__state).
:- mode transform_instructions_2(in, in, in, out, di, uo) is det.

transform_instructions_2([], _, _, []) --> [].
transform_instructions_2([Instr0 | Instrs0], ProcLabel, N0, Instrs) -->
	transform_instruction(Instr0, ProcLabel, N0, InstrsA, N),
	transform_instructions_2(Instrs0, ProcLabel, N, InstrsB),
	{ list__append(InstrsA, InstrsB, Instrs) }.


%-----------------------------------------------------------------------------%

:- pred transform_instruction(instruction, proc_label, int,
		list(instruction), int, io__state, io__state).
:- mode transform_instruction(in, in, in, out, out, di, uo) is det.

transform_instruction(Instr0, ProcLabel, N0, Instrs, N) -->
	globals__io_lookup_int_option(max_jump_table_size, Size),
	(
		{ Instr0 = computed_goto(_Rval, Labels) - _},
		{ list__length(Labels, L) },
		{ L > Size }
	->
		split_computed_goto(Instr0, Size, L, ProcLabel, N0, Instrs, N)
	;
		{ Instrs = [Instr0] },
		{ N = N0 }
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
:- pred split_computed_goto(instruction, int, int, proc_label, int,
		list(instruction), int, io__state, io__state).
:- mode split_computed_goto(in, in, in, in, in, out, out, di, uo) is det.

split_computed_goto(Instr0, MaxSize, Length, ProcLabel, N0, Instrs, N) -->
	(
		{ Length =< MaxSize }
	->
		{ Instrs = [Instr0] },
		{ N = N0 }
	;
		{ Instr0 = computed_goto(Rval, Labels) - _Comment }
	->
		{ N1 is N0 + 1},
		{ N2 is N1 + 1},
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
		{ ElseAddr  = label(local(ProcLabel, N1)) },
		{ ElseLabel = label(local(ProcLabel, N1)) - ""},
		{ IfInstr   = if_val(Test, ElseAddr ) - "Binary search"},

		{ ThenInstr = computed_goto(Rval, Start) - "Then section" },
		{ ElseInstr = computed_goto(Index, End) - "Else section" },

		split_computed_goto(ThenInstr, MaxSize, Mid, ProcLabel, N2,
				ThenInstrs, N3),
		split_computed_goto(ElseInstr, MaxSize, Length - Mid,
				ProcLabel, N3, ElseInstrs, N),

		{ list__append(ThenInstrs, [ElseLabel | ElseInstrs], InstrsA) },
		{ Instrs = [IfInstr | InstrsA] }
	;
		{ error("split_computed_goto") }
	).

%-----------------------------------------------------------------------------%

	%
	% max_label_int(Is, M0, M)
	%
	% Find the highest integer, M, used in local labels from the list of
	% intructions, Is, where M0 is the highest integer found so far.
	%
:- pred max_label_int(list(instruction), int, int).
:- mode max_label_int(in, in, out) is det.

max_label_int([], N, N).
max_label_int([Instr - _Comment | Instrs], N0, N) :-
	(
		Instr = label(local(_, Num)),
		Num > N0
	->
		max_label_int(Instrs, Num, N)
	;
		max_label_int(Instrs, N0, N)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
