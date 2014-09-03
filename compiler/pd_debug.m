%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pd_debug_m
% Main author: stayl.
%
% Debugging routines for partial deduction.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.pd_debug.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.pd_info.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pred pd_debug_do_io(bool::in, pred(io, io)::pred(di, uo) is det,
    io::di, io::uo) is det.

:- pred pd_debug_output_goal(pd_info::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

:- pred pd_debug_search_version_result(pd_info::in, maybe_version::in,
    io::di, io::uo) is det.

:- pred pd_debug_register_version(pd_info::in, pred_proc_id::in,
    version_info::in, io::di, io::uo) is det.

:- pred pd_debug_write_instmap(pd_info::in, io::di, io::uo) is det.

:- pred pd_debug_message(bool::in, string::in,
    list(string.poly_type)::in, io::di, io::uo) is det.

:- pred pd_debug_message_context(bool::in, prog_context::in, string::in,
    list(string.poly_type)::in, io::di, io::uo) is det.

:- pred pd_debug_write(bool::in, T::in, io::di, io::uo) is det.

:- pred pd_debug_write_pred_proc_id_list(pd_info::in, list(pred_proc_id)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.set_of_var.

:- import_module set.

%-----------------------------------------------------------------------------%

pd_debug_do_io(DebugPD, Pred, !IO) :-
    (
        DebugPD = yes,
        call(Pred, !IO),
        io.flush_output(!IO)
    ;
        DebugPD = no
    ).

%-----------------------------------------------------------------------------%

pd_debug_search_version_result(PDInfo, MaybeVersion, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    pd_debug_do_io(DebugPD,
        pd_debug_search_version_result_2(ModuleInfo, MaybeVersion), !IO).

:- pred pd_debug_search_version_result_2(module_info::in, maybe_version::in,
    io::di, io::uo) is det.

pd_debug_search_version_result_2(ModuleInfo, MaybeVersion, !IO) :-
    (
        MaybeVersion = no_version,
        io.write_string("Specialised version not found.\n", !IO)
    ;
        MaybeVersion = version(exact, _, _, _, _),
        io.write_string("Exact match found.\n", !IO)
    ;
        MaybeVersion = version(more_general, PredProcId, Version, _, _),
        io.write_string("More general version.\n", !IO),
        pd_debug_output_version(ModuleInfo, PredProcId, Version, no, !IO)
    ).

%-----------------------------------------------------------------------------%

pd_debug_register_version(PDInfo, PredProcId, Version, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    pd_debug_do_io(DebugPD,
        pd_debug_register_version_2(ModuleInfo, PredProcId, Version), !IO).

:- pred pd_debug_register_version_2(module_info::in, pred_proc_id::in,
    version_info::in, io::di, io::uo) is det.

pd_debug_register_version_2(ModuleInfo, PredProcId, Version, !IO) :-
    io.write_string("Registering version:\n", !IO),
    pd_debug_output_version(ModuleInfo, PredProcId, Version, no, !IO).

%-----------------------------------------------------------------------------%

:- pred pd_debug_output_version(module_info::in, pred_proc_id::in,
    version_info::in, bool::in, io::di, io::uo) is det.

pd_debug_output_version(ModuleInfo, PredProcId, Version, WriteUnfoldedGoal,
        !IO) :-
    Version = version_info(Goal, _, Args, _, InstMap,
        InitialCost, CostDelta, Parents, _),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    PredName = predicate_name(ModuleInfo, PredId),
    io.write_string(PredName, !IO),
    io.write_string(": (PredProcId :", !IO),
    PredProcId = proc(PredId, ProcId),
    pred_id_to_int(PredId, PredInt),
    proc_id_to_int(ProcId, ProcInt),
    io.write_int(PredInt, !IO),
    io.write_string("-", !IO),
    io.write_int(ProcInt, !IO),
    io.write_string(")", !IO),
    io.nl(!IO),
    io.write_string(" initial cost: ", !IO),
    io.write_int(InitialCost, !IO),
    io.nl(!IO),
    io.write_string(" cost delta: ", !IO),
    io.write_int(CostDelta, !IO),
    io.nl(!IO),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    instmap_restrict(NonLocals, InstMap, InstMap1),
    io.write_string(" args: ", !IO),
    mercury_output_vars(VarSet, yes, Args, !IO),
    io.nl(!IO),
    write_instmap(InstMap1, VarSet, yes, 1, !IO),
    io.nl(!IO),
    module_info_get_globals(ModuleInfo, Globals),
    OutInfo = init_hlds_out_info(Globals),
    write_goal(OutInfo, Goal, ModuleInfo, VarSet, yes, 1, "\n", !IO),
    io.nl(!IO),
    io.write_string("Parents: ", !IO),
    set.to_sorted_list(Parents, ParentsList),
    io.write_list(ParentsList, ", ", write_pred_proc_id(ModuleInfo), !IO),
    io.nl(!IO),
    (
        WriteUnfoldedGoal = yes,
        proc_info_get_goal(ProcInfo, ProcGoal),
        io.write_string("Unfolded goal\n", !IO),
        write_goal(OutInfo, ProcGoal, ModuleInfo, VarSet, yes, 1, "\n", !IO),
        io.nl(!IO)
    ;
        WriteUnfoldedGoal = no
    ).

%-----------------------------------------------------------------------------%

pd_debug_write_instmap(PDInfo, !IO) :-
    pd_info_get_instmap(PDInfo, InstMap),
    pd_info_get_proc_info(PDInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    pd_debug_do_io(DebugPD, write_instmap(InstMap, VarSet, yes, 1), !IO).

%-----------------------------------------------------------------------------%

pd_debug_write_pred_proc_id_list(PDInfo, PredProcIds, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    pd_debug_do_io(DebugPD,
        pd_debug_write_pred_proc_id_list_2(ModuleInfo, PredProcIds),
        !IO).

:- pred pd_debug_write_pred_proc_id_list_2(module_info::in,
    list(pred_proc_id)::in, io::di, io::uo) is det.

pd_debug_write_pred_proc_id_list_2(ModuleInfo, PredProcIds, !IO) :-
    io.write_list(PredProcIds, ", ", write_pred_proc_id(ModuleInfo), !IO).

%-----------------------------------------------------------------------------%

pd_debug_output_goal(PDInfo, Msg, Goal, !IO) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    pd_debug_do_io(DebugPD, pd_debug_output_goal_2(PDInfo, Msg, Goal), !IO).

:- pred pd_debug_output_goal_2(pd_info::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

pd_debug_output_goal_2(PDInfo, Msg, Goal, !IO) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    pd_info_get_proc_info(PDInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    pd_info_get_instmap(PDInfo, InstMap),
    pd_info_get_module_info(PDInfo, ModuleInfo),
    io.write_string(Msg, !IO),
    goal_util.goal_vars(hlds_goal(GoalExpr, GoalInfo), Vars),
    instmap_restrict(Vars, InstMap, InstMap1),
    write_instmap(InstMap1, VarSet, yes, 1, !IO),
    io.nl(!IO),
    module_info_get_globals(ModuleInfo, Globals),
    OutInfo = init_hlds_out_info(Globals),
    write_goal(OutInfo, Goal, ModuleInfo, VarSet, yes, 1, "\n", !IO),
    io.nl(!IO),
    io.flush_output(!IO).

%-----------------------------------------------------------------------------%

pd_debug_message(DebugPD, Fmt, Args, !IO) :-
    pd_debug_do_io(DebugPD, io.format(Fmt, Args), !IO).

pd_debug_message_context(DebugPD, Context, Fmt, Args, !IO) :-
    pd_debug_do_io(DebugPD, prog_out.write_context(Context), !IO),
    pd_debug_do_io(DebugPD, io.format(Fmt, Args), !IO).

%-----------------------------------------------------------------------------%

pd_debug_write(DebugPD, Thing, !IO) :-
    pd_debug_do_io(DebugPD, io.write(Thing), !IO).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.pd_debug.
%-----------------------------------------------------------------------------%
