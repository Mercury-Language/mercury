%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1997, 2003-2004 The University of Melbourne.
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

:- module check_hlds__mode_debug.

:- interface.

:- import_module check_hlds__mode_info.
:- import_module io.

	% Print a debugging message which includes the port, message string,
	% and the current instmap (but only if `--debug-modes' was enabled).
	%
:- pred mode_checkpoint(port::in, string::in, mode_info::in, mode_info::out,
	io::di, io::uo) is det.

:- type port
	--->	enter
	;	exit
	;	wakeup.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__modes.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.

:- import_module std_util, list, assoc_list, bool, map.
:- import_module term, varset.

%-----------------------------------------------------------------------------%

	% This code is used to trace the actions of the mode checker.

mode_checkpoint(Port, Msg, !ModeInfo, !IO) :-
	mode_info_get_module_info(!.ModeInfo, ModuleInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, debug_modes, DoCheckPoint),
	( DoCheckPoint = yes ->
		mode_checkpoint_2(Port, Msg, !ModeInfo, !IO)
	;
		true
	).

:- pred mode_checkpoint_2(port::in, string::in, mode_info::in, mode_info::out,
	io__state::di, io__state::uo) is det.

mode_checkpoint_2(Port, Msg, !ModeInfo, !IO) :-
	mode_info_get_last_checkpoint_insts(!.ModeInfo, OldInsts),
	mode_info_get_errors(!.ModeInfo, Errors),
	( Port = enter ->
		io__write_string("Enter ", !IO),
		Detail = yes
	; Port = wakeup ->
		io__write_string("Wake ", !IO),
		Detail = no
	; Errors = [] ->
		io__write_string("Exit ", !IO),
		Detail = yes
	;
		io__write_string("Delay ", !IO),
		Detail = no
	),
	io__write_string(Msg, !IO),
	( Detail = yes ->
		io__write_string(":\n", !IO),
		globals__io_lookup_bool_option(statistics, Statistics, !IO),
		maybe_report_stats(Statistics, !IO),
		maybe_flush_output(Statistics, !IO),
		mode_info_get_instmap(!.ModeInfo, InstMap),
		( instmap__is_reachable(InstMap) ->
			instmap__to_assoc_list(InstMap, NewInsts),
			mode_info_get_varset(!.ModeInfo, VarSet),
			mode_info_get_instvarset(!.ModeInfo, InstVarSet),
			write_var_insts(NewInsts, OldInsts, VarSet, InstVarSet,
				!IO)
		;
			NewInsts = [],
			io__write_string("\tUnreachable\n", !IO)
		),
		mode_info_set_last_checkpoint_insts(NewInsts, !ModeInfo)
	;
		true
	),
	io__write_string("\n", !IO),
	io__flush_output(!IO).

:- pred write_var_insts(assoc_list(prog_var, inst)::in,
	assoc_list(prog_var, inst)::in, prog_varset::in, inst_varset::in,
	io__state::di, io__state::uo) is det.

write_var_insts([], _, _, _, !IO).
write_var_insts([Var - Inst | VarInsts], OldInsts, VarSet, InstVarSet, !IO) :-
	io__write_string("\t", !IO),
	mercury_output_var(Var, VarSet, no, !IO),
	io__write_string(" ::", !IO),
	(
		assoc_list__search(OldInsts, Var, OldInst),
		Inst = OldInst
	->
		io__write_string(" unchanged\n", !IO)
	;
		io__write_string("\n", !IO),
		mercury_output_structured_inst(Inst, 2, InstVarSet, !IO)
	),
	write_var_insts(VarInsts, OldInsts, VarSet, InstVarSet, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
