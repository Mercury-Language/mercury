%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: pd_debug.m
% Main author: stayl.
%
% Debugging routines for partial deduction.
%-----------------------------------------------------------------------------%
:- module pd_debug.

:- interface.

:- import_module pd_info, hlds_goal, hlds_pred, prog_data.
:- import_module list, string.

:- pred pd_debug__do_io(pred(io__state, io__state)::pred(di, uo) is det,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

:- pred pd_debug__output_goal(string::in, hlds_goal::in,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

:- pred pd_debug__search_version_result(maybe_version::in,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

:- pred pd_debug__register_version(pred_proc_id::in, version_info::in,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

:- pred pd_debug__write_instmap(pd_info::pd_info_di,
		pd_info::pd_info_uo) is det.

:- pred pd_debug__message(string::in, list(string__poly_type)::in,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

:- pred pd_debug__message(prog_context::in, string::in, 
		list(string__poly_type)::in,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

:- pred pd_debug__write(T::in, pd_info::pd_info_di, 
		pd_info::pd_info_uo) is det.

:- pred pd_debug__write_pred_proc_id_list(list(pred_proc_id)::in, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module globals, hlds_module, hlds_out, instmap, options.
:- import_module instmap, prog_out, goal_util, mercury_to_mercury.
:- import_module bool, io, set, std_util.

pd_debug__do_io(Pred) -->
	pd_debug__do_output(DoOutput),
	( { DoOutput = yes } ->
		pd_info_get_io_state(IO0),
		{ call(Pred, IO0, IO1) },
		{ io__flush_output(IO1, IO) },
		pd_info_set_io_state(IO)
	;
		[]
	).

%-----------------------------------------------------------------------------%

pd_debug__search_version_result(MaybeVersion) -->
	pd_info_get_module_info(ModuleInfo),
	pd_debug__do_io(pd_debug__search_version_result_2(ModuleInfo, 
		MaybeVersion)).

:- pred pd_debug__search_version_result_2(module_info::in, maybe_version::in,
		io__state::di, io__state::uo) is det.

pd_debug__search_version_result_2(ModuleInfo, MaybeVersion) -->
	(
		{ MaybeVersion = no_version },
		io__write_string("Specialised version not found.\n")
	;
		{ MaybeVersion = version(exact, _, _, _, _) },
		io__write_string("Exact match found.\n")
	;
		{ MaybeVersion = version(more_general,
			PredProcId, Version, _, _) },
		io__write_string("More general version.\n"),
		pd_debug__output_version(ModuleInfo, PredProcId, Version, no)
	).

%-----------------------------------------------------------------------------%

pd_debug__register_version(PredProcId, Version) -->
	pd_info_get_module_info(ModuleInfo),
	{ Register = lambda([IO0::di, IO::uo] is det, (
		io__write_string("Registering version:\n", IO0, IO1),
		pd_debug__output_version(ModuleInfo, PredProcId, Version,
			no, IO1, IO)
		)) },
	pd_debug__do_io(Register).

%-----------------------------------------------------------------------------%

:- pred pd_debug__output_version(module_info::in, pred_proc_id::in,
	version_info::in, bool::in, io__state::di, io__state::uo) is det.

pd_debug__output_version(ModuleInfo, PredProcId,
		Version, WriteUnfoldedGoal) -->
	{ Version = version_info(Goal - GoalInfo, _, Args, _, InstMap, 
			InitialCost, CostDelta, Parents, _) }, 
	{ predicate_name(ModuleInfo, PredId, PredName) },
	io__write_string(PredName),
	io__write_string(": (PredProcId :"),
	{ PredProcId = proc(PredId, ProcId) },
	{ pred_id_to_int(PredId, PredInt) },
	{ proc_id_to_int(ProcId, ProcInt) },
	io__write_int(PredInt),
	io__write_string("-"),
	io__write_int(ProcInt),
	io__write_string(")"),
	io__nl,
	io__write_string(" initial cost: "),
	io__write_int(InitialCost),
	io__nl,
	io__write_string(" cost delta: "),
	io__write_int(CostDelta),
	io__nl,
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ module_info_pred_proc_info(ModuleInfo, 
		PredId, ProcId, _, ProcInfo) },
	{ proc_info_varset(ProcInfo, VarSet) },
	{ instmap__restrict(InstMap, NonLocals, InstMap1) },
	io__write_string(" args: "),
	mercury_output_vars(Args, VarSet, yes),
	io__nl,
	hlds_out__write_instmap(InstMap1, VarSet, yes, 1),
	io__nl,
	hlds_out__write_goal(Goal - GoalInfo, ModuleInfo, VarSet, yes, 1, "\n"),
	io__nl,
	io__write_string("Parents: "),
	{ set__to_sorted_list(Parents, ParentsList) },
	io__write_list(ParentsList, ", ", 
		pd_debug__write_pred_proc_id(ModuleInfo)), 
	io__nl,
	( { WriteUnfoldedGoal = yes } ->
		{ proc_info_goal(ProcInfo, ProcGoal) },
		io__write_string("Unfolded goal\n"),
		hlds_out__write_goal(ProcGoal, 
			ModuleInfo, VarSet, yes, 1, "\n"),
		io__nl
	;
		[]
	).

%-----------------------------------------------------------------------------%

pd_debug__write_instmap -->
	pd_info_get_instmap(InstMap),
	pd_info_get_proc_info(ProcInfo),
	{ proc_info_varset(ProcInfo, VarSet) },
	pd_debug__do_io(hlds_out__write_instmap(InstMap, VarSet, yes, 1)).

%-----------------------------------------------------------------------------%

pd_debug__write_pred_proc_id_list(PredProcIds) -->
	pd_info_get_module_info(ModuleInfo),
	pd_debug__do_io(pred(di, uo) is det -->
		io__write_list(PredProcIds, ", ",
			pd_debug__write_pred_proc_id(ModuleInfo))
	).

:- pred pd_debug__write_pred_proc_id(module_info::in, pred_proc_id::in, 
		io__state::di, io__state::uo) is det.

pd_debug__write_pred_proc_id(ModuleInfo, proc(PredId, ProcId)) -->
	hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId).

%-----------------------------------------------------------------------------%

pd_debug__output_goal(Msg, Goal - GoalInfo) -->
	pd_debug__do_output(DoOutput),
	( { DoOutput = yes } ->
		pd_info_get_proc_info(ProcInfo),
		{ proc_info_varset(ProcInfo, VarSet) },
		pd_info_get_instmap(InstMap),
		pd_info_get_io_state(IO0),
		pd_info_get_module_info(ModuleInfo),
		{
		io__write_string(Msg, IO0, IO1),
		goal_util__goal_vars(Goal - GoalInfo, Vars),
		instmap__restrict(InstMap, Vars, InstMap1),
		hlds_out__write_instmap(InstMap1, VarSet, yes, 1, IO1, IO2),
		io__nl(IO2, IO3),
		hlds_out__write_goal(Goal - GoalInfo, ModuleInfo,
			VarSet, yes, 1, "\n", IO3, IO4),
		io__nl(IO4, IO5),
		io__flush_output(IO5, IO)
		},
		pd_info_set_io_state(IO)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred pd_debug__do_output(bool::out, pd_info::pd_info_di,
		pd_info::pd_info_uo) is det.

pd_debug__do_output(DoDebug) -->
	pd_info_get_io_state(IO0),
	{ globals__io_lookup_bool_option(debug_pd, DoDebug, IO0, IO1) },
	pd_info_set_io_state(IO1).

%-----------------------------------------------------------------------------%

pd_debug__message(Context, Fmt, Args) -->
	pd_debug__do_io(prog_out__write_context(Context)),	
	pd_debug__do_io(io__format(Fmt, Args)).

pd_debug__message(Fmt, Args) -->
	pd_debug__do_io(io__format(Fmt, Args)).

%-----------------------------------------------------------------------------%

pd_debug__write(Thing) -->
	pd_debug__do_io(io__write(Thing)).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
