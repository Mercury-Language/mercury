%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2004 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: pd_debug.m
% Main author: stayl.
%
% Debugging routines for partial deduction.
%-----------------------------------------------------------------------------%

:- module transform_hlds__pd_debug.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.
:- import_module transform_hlds__pd_info.

:- import_module io, list, string.

:- pred pd_debug__do_io(pred(io, io)::pred(di, uo) is det,
	io::di, io::uo) is det.

:- pred pd_debug__output_goal(pd_info::in, string::in, hlds_goal::in,
	io::di, io::uo) is det.

:- pred pd_debug__search_version_result(pd_info::in, maybe_version::in,
	io::di, io::uo) is det.

:- pred pd_debug__register_version(pd_info::in, pred_proc_id::in,
	version_info::in, io::di, io::uo) is det.

:- pred pd_debug__write_instmap(pd_info::in, io::di, io::uo) is det.

:- pred pd_debug__message(string::in, list(string__poly_type)::in,
	io::di, io::uo) is det.

:- pred pd_debug__message(prog_context::in, string::in,
	list(string__poly_type)::in, io::di, io::uo) is det.

:- pred pd_debug__write(T::in, io::di, io::uo) is det.

:- pred pd_debug__write_pred_proc_id_list(pd_info::in, list(pred_proc_id)::in,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__goal_util.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_out.
:- import_module hlds__instmap.
:- import_module hlds__instmap.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_out.

:- import_module bool, set, std_util.

pd_debug__do_io(Pred, !IO) :-
	globals__io_lookup_bool_option(debug_pd, DoDebug, !IO),
	(
		DoDebug = yes,
		call(Pred, !IO),
		io__flush_output(!IO)
	;
		DoDebug = no
	).

%-----------------------------------------------------------------------------%

pd_debug__search_version_result(PDInfo, MaybeVersion, !IO) :-
	pd_info_get_module_info(PDInfo, ModuleInfo),
	pd_debug__do_io(pd_debug__search_version_result_2(ModuleInfo,
		MaybeVersion), !IO).

:- pred pd_debug__search_version_result_2(module_info::in, maybe_version::in,
	io::di, io::uo) is det.

pd_debug__search_version_result_2(ModuleInfo, MaybeVersion, !IO) :-
	(
		MaybeVersion = no_version,
		io__write_string("Specialised version not found.\n", !IO)
	;
		MaybeVersion = version(exact, _, _, _, _),
		io__write_string("Exact match found.\n", !IO)
	;
		MaybeVersion = version(more_general, PredProcId, Version,
			_, _),
		io__write_string("More general version.\n", !IO),
		pd_debug__output_version(ModuleInfo, PredProcId, Version, no,
			!IO)
	).

%-----------------------------------------------------------------------------%

pd_debug__register_version(PDInfo, PredProcId, Version, !IO) :-
	pd_info_get_module_info(PDInfo, ModuleInfo),
	pd_debug__do_io(
		pd_debug__register_version_2(ModuleInfo, PredProcId, Version),
		!IO).

:- pred pd_debug__register_version_2(module_info::in, pred_proc_id::in,
	version_info::in, io::di, io::uo) is det.

pd_debug__register_version_2(ModuleInfo, PredProcId, Version, !IO) :-
	io__write_string("Registering version:\n", !IO),
	pd_debug__output_version(ModuleInfo, PredProcId, Version, no, !IO).

%-----------------------------------------------------------------------------%

:- pred pd_debug__output_version(module_info::in, pred_proc_id::in,
	version_info::in, bool::in, io::di, io::uo) is det.

pd_debug__output_version(ModuleInfo, PredProcId, Version, WriteUnfoldedGoal,
		!IO) :-
	Version = version_info(Goal - GoalInfo, _, Args, _, InstMap,
		InitialCost, CostDelta, Parents, _),
	predicate_name(ModuleInfo, PredId, PredName),
	io__write_string(PredName, !IO),
	io__write_string(": (PredProcId :", !IO),
	PredProcId = proc(PredId, ProcId),
	pred_id_to_int(PredId, PredInt),
	proc_id_to_int(ProcId, ProcInt),
	io__write_int(PredInt, !IO),
	io__write_string("-", !IO),
	io__write_int(ProcInt, !IO),
	io__write_string(")", !IO),
	io__nl(!IO),
	io__write_string(" initial cost: ", !IO),
	io__write_int(InitialCost, !IO),
	io__nl(!IO),
	io__write_string(" cost delta: ", !IO),
	io__write_int(CostDelta, !IO),
	io__nl(!IO),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	instmap__restrict(InstMap, NonLocals, InstMap1),
	io__write_string(" args: ", !IO),
	mercury_output_vars(Args, VarSet, yes, !IO),
	io__nl(!IO),
	hlds_out__write_instmap(InstMap1, VarSet, yes, 1, !IO),
	io__nl(!IO),
	hlds_out__write_goal(Goal - GoalInfo, ModuleInfo, VarSet, yes, 1, "\n",
		!IO),
	io__nl(!IO),
	io__write_string("Parents: ", !IO),
	set__to_sorted_list(Parents, ParentsList),
	io__write_list(ParentsList, ", ",
		pd_debug__write_pred_proc_id(ModuleInfo), !IO),
	io__nl(!IO),
	(
		WriteUnfoldedGoal = yes,
		proc_info_goal(ProcInfo, ProcGoal),
		io__write_string("Unfolded goal\n", !IO),
		hlds_out__write_goal(ProcGoal,
			ModuleInfo, VarSet, yes, 1, "\n", !IO),
		io__nl(!IO)
	;
		WriteUnfoldedGoal = no
	).

%-----------------------------------------------------------------------------%

pd_debug__write_instmap(PDInfo, !IO) :-
	pd_info_get_instmap(PDInfo, InstMap),
	pd_info_get_proc_info(PDInfo, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	pd_debug__do_io(hlds_out__write_instmap(InstMap, VarSet, yes, 1), !IO).

%-----------------------------------------------------------------------------%

pd_debug__write_pred_proc_id_list(PDInfo, PredProcIds, !IO) :-
	pd_info_get_module_info(PDInfo, ModuleInfo),
	pd_debug__do_io(
		pd_debug__write_pred_proc_id_list_2(ModuleInfo, PredProcIds),
		!IO).

:- pred pd_debug__write_pred_proc_id_list_2(module_info::in,
	list(pred_proc_id)::in, io::di, io::uo) is det.

pd_debug__write_pred_proc_id_list_2(ModuleInfo, PredProcIds, !IO) :-
	io__write_list(PredProcIds, ", ",
		pd_debug__write_pred_proc_id(ModuleInfo), !IO).

:- pred pd_debug__write_pred_proc_id(module_info::in, pred_proc_id::in,
	io::di, io::uo) is det.

pd_debug__write_pred_proc_id(ModuleInfo, proc(PredId, ProcId), !IO) :-
	hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId, !IO).

%-----------------------------------------------------------------------------%

pd_debug__output_goal(PDInfo, Msg, Goal, !IO) :-
	pd_debug__do_io(pd_debug__output_goal(PDInfo, Msg, Goal), !IO).

:- pred pd_debug__output_goal_2(pd_info::in, string::in, hlds_goal::in,
	io::di, io::uo) is det.

pd_debug__output_goal_2(PDInfo, Msg, GoalExpr - GoalInfo, !IO) :-
	pd_info_get_proc_info(PDInfo, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	pd_info_get_instmap(PDInfo, InstMap),
	pd_info_get_module_info(PDInfo, ModuleInfo),
	io__write_string(Msg, !IO),
	goal_util__goal_vars(GoalExpr - GoalInfo, Vars),
	instmap__restrict(InstMap, Vars, InstMap1),
	hlds_out__write_instmap(InstMap1, VarSet, yes, 1, !IO),
	io__nl(!IO),
	hlds_out__write_goal(GoalExpr - GoalInfo, ModuleInfo, VarSet,
		yes, 1, "\n", !IO),
	io__nl(!IO),
	io__flush_output(!IO).

%-----------------------------------------------------------------------------%

pd_debug__message(Fmt, Args, !IO) :-
	pd_debug__do_io(io__format(Fmt, Args), !IO).

pd_debug__message(Context, Fmt, Args, !IO) :-
	pd_debug__do_io(prog_out__write_context(Context), !IO),
	pd_debug__do_io(io__format(Fmt, Args), !IO).

%-----------------------------------------------------------------------------%

pd_debug__write(Thing, !IO) :-
	pd_debug__do_io(io__write(Thing), !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
