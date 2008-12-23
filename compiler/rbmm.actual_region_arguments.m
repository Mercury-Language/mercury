% -----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rbmm.actual_region_arguments.m.
% Main author: Quan Phan.
%
% We will pass regions as extra arguments in procedure calls. The extra formal
% region arguments are already known from live region analysis.
% This module derives the corresponding actual region arguments at call sites
% in each procedure. This information will be used to extend the argument
% lists of calls in the HLDS.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.actual_region_arguments.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_liveness_info.
:- import_module transform_hlds.smm_common.

:- import_module list.
:- import_module map.

:- type proc_pp_actual_region_args_table
    ==  map(
                pred_proc_id,
                pp_actual_region_args_table
        ).

:- type pp_actual_region_args_table
    ==  map(
                program_point,
                actual_region_args
        ).

:- type actual_region_args
    --->    actual_region_args(
                list(rptg_node),    % constant (carried) region arguments.
                list(rptg_node),    % inputs (removed).
                list(rptg_node)     % outputs (created).
            ).
:- pred record_actual_region_arguments(module_info::in, rpta_info_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_pp_actual_region_args_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.goal_path.
:- import_module hlds.hlds_goal.
:- import_module libs.
:- import_module libs.compiler_util.

:- import_module set.
:- import_module string.
:- import_module svmap.

record_actual_region_arguments(ModuleInfo, RptaInfoTable, ConstantRTable,
        DeadRTable, BornRTable, ActualRegionArgTable) :-
    module_info_predids(PredIds, ModuleInfo, _),
    list.foldl(record_actual_region_arguments_pred(ModuleInfo,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
        PredIds, map.init, ActualRegionArgTable).

:- pred record_actual_region_arguments_pred(module_info::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, pred_id::in,
    proc_pp_actual_region_args_table::in,
    proc_pp_actual_region_args_table::out) is det.

record_actual_region_arguments_pred(ModuleInfo, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, PredId,
        !ActualRegionArgTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.foldl(record_actual_region_arguments_proc(ModuleInfo, PredId,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable), ProcIds,
        !ActualRegionArgTable).

:- pred record_actual_region_arguments_proc(module_info::in, pred_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, proc_id::in,
    proc_pp_actual_region_args_table::in,
    proc_pp_actual_region_args_table::out) is det.

record_actual_region_arguments_proc(ModuleInfo, PredId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, ProcId,
        !ActualRegionArgTable) :-
    PPId = proc(PredId, ProcId),
    ( if    some_are_special_preds([PPId], ModuleInfo)
      then  true
      else
            module_info_proc_info(ModuleInfo, PPId, ProcInfo0),
            fill_goal_path_slots(ModuleInfo, ProcInfo0, ProcInfo),
            proc_info_get_goal(ProcInfo, Body),
            record_actual_region_arguments_goal(ModuleInfo, PPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Body,
                map.init, ActualRegionArgProc),
            svmap.set(PPId, ActualRegionArgProc, !ActualRegionArgTable)
    ).

:- pred record_actual_region_arguments_goal(module_info::in,
    pred_proc_id::in, rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, hlds_goal::in,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_goal(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, Goal, !ActualRegionArgProc) :-
    Goal = hlds_goal(Expr, Info),
    record_actual_region_arguments_expr(Expr, Info, ModuleInfo, PPId,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable,
        !ActualRegionArgProc).

:- pred record_actual_region_arguments_expr(hlds_goal_expr::in,
    hlds_goal_info::in, module_info::in, pred_proc_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_expr(GoalExpr, GoalInfo, ModuleInfo, CallerPPId,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable,
        !ActualRegionArgProc) :-
    (
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        CalleePPId = proc(PredId, ProcId),
        ( some_are_special_preds([CalleePPId], ModuleInfo) ->
            true
        ;
            CallSite = program_point_init(GoalInfo),
            record_actual_region_arguments_call_site(CallerPPId, CallSite,
                CalleePPId, RptaInfoTable, ConstantRTable, DeadRTable,
                BornRTable, !ActualRegionArgProc)
        )
    ;
        GoalExpr = conj(_, Conjuncts),
        list.foldl(
            record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
            Conjuncts, !ActualRegionArgProc)
    ;
        GoalExpr = disj(Disjuncts),
        list.foldl(
            record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
            Disjuncts, !ActualRegionArgProc)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Cond,
            !ActualRegionArgProc),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Then,
            !ActualRegionArgProc),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Else,
            !ActualRegionArgProc)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl(
            record_actual_region_arguments_case(ModuleInfo, CallerPPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
            Cases, !ActualRegionArgProc)
    ;
        GoalExpr = generic_call(_, _, _, _),
        sorry(this_file,
            "record_actual_region_arguments_expr: generic_call NYI")
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        sorry(this_file,
            "record_actual_region_arguments_expr: call_foreign_proc NYI")
    ;
        GoalExpr = negation(SubGoal),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, SubGoal,
            !ActualRegionArgProc)
    ;
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = scope(_, SubGoal),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, SubGoal,
            !ActualRegionArgProc)
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file,
            "record_actual_region_arguments_expr: shorthand")
    ).

:- pred record_actual_region_arguments_case(module_info::in,
    pred_proc_id::in, rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, case::in,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_case(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, Case, !ActualRegionArgProc) :-
    Case = case(_, _, Goal),
    record_actual_region_arguments_goal(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, Goal, !ActualRegionArgProc).

:- pred record_actual_region_arguments_call_site(pred_proc_id::in,
    program_point::in, pred_proc_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_call_site(CallerPPId, CallSite,
        CalleePPId, RptaInfoTable, ConstantRTable, DeadRTable,
        BornRTable, !ActualRegionArgProc) :-
    map.lookup(ConstantRTable, CalleePPId, CalleeConstantR),
    map.lookup(DeadRTable, CalleePPId, CalleeDeadR),
    map.lookup(BornRTable, CalleePPId, CalleeBornR),

    map.lookup(RptaInfoTable, CallerPPId, CallerRptaInfo),
    CallerRptaInfo = rpta_info(_, CallerAlpha),
    map.lookup(CallerAlpha, CallSite, AlphaAtCallSite),

    % Actual constant region arguments.
    set.to_sorted_list(CalleeConstantR, LCalleeConstantR),
    list.foldl(find_actual_param(AlphaAtCallSite), LCalleeConstantR, [],
        LActualConstantR0),
    list.reverse(LActualConstantR0, LActualConstantR),

    % Actual dead region arguments.
    set.to_sorted_list(CalleeDeadR, LCalleeDeadR),
    list.foldl(find_actual_param(AlphaAtCallSite), LCalleeDeadR, [],
        LActualDeadR0),
    list.reverse(LActualDeadR0, LActualDeadR),

    % Actual born region arguments.
    set.to_sorted_list(CalleeBornR, LCalleeBornR),
    list.foldl(find_actual_param(AlphaAtCallSite), LCalleeBornR, [],
        LActualBornR0),
    list.reverse(LActualBornR0, LActualBornR),

    svmap.det_insert(CallSite,
        actual_region_args(LActualConstantR, LActualDeadR, LActualBornR),
        !ActualRegionArgProc).

:- pred find_actual_param(map(rptg_node, rptg_node)::in, rptg_node::in,
    list(rptg_node)::in, list(rptg_node)::out) is det.

find_actual_param(Alpha_PP, Formal, Actuals0, Actuals) :-
    map.lookup(Alpha_PP, Formal, Actual),
    Actuals = [Actual | Actuals0].

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "rbmm.actual_region_arguments.m".

%-----------------------------------------------------------------------------%
