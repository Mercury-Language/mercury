%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: io_action.m
% Author: zs.
%
% This module defines the representation of I/O actions used by the
% declarative debugger.

%-----------------------------------------------------------------------------%

:- module mdb__io_action.

:- interface.

:- import_module mdb__util.
:- import_module bool, list, map, std_util, io.

:- type io_action
	--->	io_action(
			io_action_proc_name	:: string,
			io_action_pf		:: pred_or_func,
			io_action_args		:: list(univ)
		).

:- type io_seq_num	== int.
:- type io_action_map	== map(io_seq_num, io_action).

:- pred make_io_action_map(int::in, int::in, io_action_map::out,
	io__state::di, io__state::uo) is det.

:- pred io_action_to_synthetic_term(io_action::in, string::out,
	list(univ)::out, bool::out) is det.

:- implementation.

:- import_module require, int.

io_action_to_synthetic_term(IoAction, ProcName, Args, IsFunc) :-
	IoAction = io_action(ProcName, PredFunc, Args),
	(
		PredFunc = predicate,
		IsFunc = no
	;
		PredFunc = function,
		IsFunc = yes
	).

make_io_action_map(Start, End, IoActionMap) -->
	make_io_action_map_2(Start, End, map__init, IoActionMap).

:- pred make_io_action_map_2(int::in, int::in,
	io_action_map::in, io_action_map::out, io__state::di, io__state::uo)
	is det.

make_io_action_map_2(Cur, End, IoActionMap0, IoActionMap) -->
	( { Cur = End } ->
		{ IoActionMap = IoActionMap0 }
	;
		pickup_io_action(Cur, ProcName, IsFunc, Args),
		{ update_io_action_map(Cur, ProcName, IsFunc, Args,
			IoActionMap0, IoActionMap1) },
		make_io_action_map_2(Cur + 1, End, IoActionMap1, IoActionMap)
	).

:- pred update_io_action_map(int::in, string::in, bool::in, list(univ)::in,
	io_action_map::in, io_action_map::out) is det.

update_io_action_map(IoActionNum, ProcName, IsFunc, Args,
		IoActionMap0, IoActionMap) :-
	(
		IsFunc = no,
		PredFunc = predicate
	;
		IsFunc = yes,
		PredFunc = function
	),
	IoAction = io_action(ProcName, PredFunc, Args),
	map__det_insert(IoActionMap0, IoActionNum, IoAction, IoActionMap).

:- pred pickup_io_action(int::in, string::out, bool::out, list(univ)::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	pickup_io_action(SeqNum::in, ProcName::out, IsFunc::out, Args::out,
		S0::di, S::uo),
	[thread_safe, promise_pure, tabled_for_io],
"{
	const char	*problem;
	const char	*proc_name;

	MR_save_transient_hp();
	problem = MR_trace_get_action(SeqNum, &proc_name, &IsFunc, &Args);
	MR_restore_transient_hp();
	if (problem != NULL) {
		MR_fatal_error(""pickup_io_action: MR_trace_get_action"");
	}

	/* cast away const */
	ProcName = (MR_String) (MR_Integer) proc_name;

	S = S0;
}").
