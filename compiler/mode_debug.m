%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_debug.m.
% Main author: fjh.
%
% This module contains code for tracing the actions of the mode checker.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mode_debug.

:- interface.

:- import_module mode_info, hlds_goal.

	% Print a debugging message which includes the port, message string,
	% and the current instmap (but only if `--debug-modes' was enabled).
	%
:- pred mode_checkpoint(port, string, hlds_goal_info, mode_info, mode_info).
:- mode mode_checkpoint(in, in, in, mode_info_di, mode_info_uo) is det.

:- type port
	--->	enter
	;	exit
	;	wakeup.

:- type mode_debug_info.

:- pred mode_debug_info_init(mode_debug_info :: out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, list, assoc_list, io, bool, map.
:- import_module term, varset, string, require.
:- import_module modes, options, mercury_to_mercury, passes_aux.
:- import_module globals, hlds_goal, instmap, prog_data, (inst).
:- import_module hlds_module, hlds_data.

%-----------------------------------------------------------------------------%

:- type mode_debug_info
	--->	mode_debug_info(
			assoc_list(prog_var, inst),	% Last checkpoint insts
			inst_key_table_mark
		).

mode_debug_info_init(mode_debug_info([], BlankMark)) :-
	inst_key_table_mark_init(BlankMark).

%-----------------------------------------------------------------------------%

	% This code is used to trace the actions of the mode checker.

mode_checkpoint(Port, Msg, GoalInfo, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
	globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
		IOState0, IOState1),
	( DoCheckPoint = yes ->
		mode_info_get_mode_debug_info(ModeInfo0, OldDebugInfo),
		OldDebugInfo = mode_debug_info(OldInsts, OldMark),
		mode_checkpoint_2(Port, Msg, GoalInfo, OldInsts, NewInsts,
			OldMark, ModeInfo0, IOState1, IOState),
		mode_info_get_inst_table(ModeInfo0, InstTable),
		inst_table_get_inst_key_table(InstTable, IKT),
		inst_key_table_get_mark(IKT, NewMark),
		NewDebugInfo = mode_debug_info(NewInsts, NewMark),
		mode_info_set_mode_debug_info(NewDebugInfo, ModeInfo0,
			ModeInfo1)
	;
		ModeInfo1 = ModeInfo0,
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo1, IOState, ModeInfo).

:- pred mode_checkpoint_2(port, string, hlds_goal_info,
	assoc_list(prog_var, inst), assoc_list(prog_var, inst),
	inst_key_table_mark, mode_info, io__state, io__state).
:- mode mode_checkpoint_2(in, in, in, in, out, in, mode_info_ui, di, uo) is det.

mode_checkpoint_2(Port, Msg, GoalInfo, OldInstList, NewInstList, Mark,
		ModeInfo) -->
	{ mode_info_get_errors(ModeInfo, Errors) },
	( { Port = enter } ->
		io__write_string("Enter "),
		{ Detail = yes }
	; { Port = wakeup } ->
		io__write_string("Wake "),
		{ Detail = no }
	; { Errors = [] } ->
		io__write_string("Exit "),
		{ Detail = yes }
	;
		io__write_string("Delay "),
		{ Detail = no }
	),
	io__write_string(Msg),
	( { Detail = yes } ->
		{ goal_info_get_context(GoalInfo, Context) },
		{
			term__context_init(Context)
		->
			Where = ""
		;
			term__context_file(Context, File),
			term__context_line(Context, Line),
			string__format(" (%s:%d)", [s(File), i(Line)], Where)
		},
		io__write_strings([":", Where, "\n"]),
		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),
		maybe_flush_output(Statistics),
		{ mode_info_get_instmap(ModeInfo, InstMap) },
		{ mode_info_get_inst_table(ModeInfo, InstTable) },
		globals__io_lookup_bool_option(debug_inst_keys, DetailedIKT),
		( { instmap__is_reachable(InstMap) } ->
			{ instmap__to_assoc_list(InstMap, NewInstList) },
			{ mode_info_get_varset(ModeInfo, VarSet) },
			{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
			{ DetailedIKT = yes ->
				Expand = dont_expand
			;
				Expand = expand_noisily
			},
			write_var_insts(Expand, NewInstList, OldInstList,
				VarSet, InstVarSet, InstMap, InstTable)
		;
			{ NewInstList = [] },
			io__write_string("\tUnreachable\n")
		),
		( { DetailedIKT = yes } ->
			{ inst_table_get_inst_key_table(InstTable, IKT) },
			inst_key_table_print_since_mark(IKT, Mark)
		;
			[]
		)
	;
		{ NewInstList = OldInstList }
	),
	io__write_string("\n"),
	io__flush_output.

:- pred write_var_insts(expand_inst_alias,
		assoc_list(prog_var, inst), assoc_list(prog_var, inst),
		prog_varset, inst_varset, instmap, inst_table,
		io__state, io__state).
:- mode write_var_insts(in, in, in, in, in, in, in, di, uo) is det.

write_var_insts(_, [], _, _, _, _, _) --> [].
write_var_insts(Expand, [Var - Inst | NewInstList], OldInstList, VarSet,
		InstVarSet, InstMap, InstTable) -->
	io__write_string("\t"),
	mercury_output_var(Var, VarSet, no),
	io__write_string(" ::"),
	(
		{ assoc_list__search(OldInstList, Var, OldInst) },
		{ Inst = OldInst }
	->
		io__write_string(" unchanged\n")
	;
		io__write_string("\n"),
		mercury_output_structured_inst(Expand, Inst, 2,
			InstVarSet, InstMap, InstTable)
	),
	write_var_insts(Expand, NewInstList, OldInstList, VarSet, InstVarSet,
		InstMap, InstTable).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
