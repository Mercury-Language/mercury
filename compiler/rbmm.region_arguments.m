%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: rbmm.region_arguments.m.
% Main author: qph.
%
% We will pass regions as extra arguments in procedure calls.
% After the region liveness analysis we can decide on what region variables
% need to be region arguments (for procedures and calls).
% This module derives the formal region arguments for procedures and
% the actual region arguments at call sites in each procedure.
% This information will be used to extend the argument lists of procedures
% and calls in the HLDS.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module transform_hlds.rbmm.region_arguments.
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

:- type proc_formal_region_args_table
    ==  map(
                pred_proc_id,
                region_args
        ).

:- type proc_pp_actual_region_args_table
    ==  map(
                pred_proc_id,
                pp_actual_region_args_table
        ).

:- type pp_actual_region_args_table
    ==  map(
                program_point,
                region_args
        ).

:- type region_args
    --->    region_args(
                list(rptg_node),    % constant (carried) region arguments.
                list(rptg_node),    % inputs (removed).
                list(rptg_node)     % outputs (created).
            ).

:- pred record_region_arguments(module_info::in, rpta_info_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_formal_region_args_table::out,
    proc_pp_actual_region_args_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.hlds_goal.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module bool.
:- import_module require.
:- import_module set.

record_region_arguments(ModuleInfo, RptaInfoTable, ConstantRTable,
        DeadRTable, BornRTable, FormalRegionArgTable, ActualRegionArgTable) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    list.foldl2(record_actual_region_arguments_pred(ModuleInfo,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
        PredIds, map.init, FormalRegionArgTable,
        map.init, ActualRegionArgTable).

:- pred record_actual_region_arguments_pred(module_info::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, pred_id::in,
    proc_formal_region_args_table::in,
    proc_formal_region_args_table::out,
    proc_pp_actual_region_args_table::in,
    proc_pp_actual_region_args_table::out) is det.

record_actual_region_arguments_pred(ModuleInfo, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, PredId,
        !FormalRegionArgTable, !ActualRegionArgTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_non_imported_procids(PredInfo),
    list.foldl2(record_region_arguments_proc(ModuleInfo, PredId,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable), ProcIds,
        !FormalRegionArgTable, !ActualRegionArgTable).

:- pred record_region_arguments_proc(module_info::in, pred_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, proc_id::in,
    proc_formal_region_args_table::in,
    proc_formal_region_args_table::out,
    proc_pp_actual_region_args_table::in,
    proc_pp_actual_region_args_table::out) is det.

record_region_arguments_proc(ModuleInfo, PredId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, ProcId,
        !FormalRegionArgTable, !ActualRegionArgTable) :-
    PPId = proc(PredId, ProcId),
    ( if some_are_special_preds([PPId], ModuleInfo) then
        true
    else
        record_formal_region_arguments_proc(ModuleInfo, PPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable,
            !FormalRegionArgTable),

        module_info_proc_info(ModuleInfo, PPId, ProcInfo0),
        fill_goal_path_slots_in_proc(ModuleInfo, ProcInfo0, ProcInfo),
        proc_info_get_goal(ProcInfo, Body),
        record_actual_region_arguments_goal(ModuleInfo, PPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Body,
            !FormalRegionArgTable, map.init, ActualRegionArgProc),
        map.set(PPId, ActualRegionArgProc, !ActualRegionArgTable)
    ).

:- pred record_formal_region_arguments_proc(module_info::in, pred_proc_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_formal_region_args_table::in, proc_formal_region_args_table::out)
    is det.

record_formal_region_arguments_proc(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, !FormalRegionArgTable) :-
    ( if map.search(!.FormalRegionArgTable, PPId, _) then
        true
    else
        map.lookup(ConstantRTable, PPId, ConstantR),
        map.lookup(DeadRTable, PPId, DeadR),
        map.lookup(BornRTable, PPId, BornR),

        map.lookup(RptaInfoTable, PPId, RptaInfo),
        RptaInfo = rpta_info(Graph, _),

        % Formal constant, allocated-into region arguments.
        set.to_sorted_list(ConstantR, LConstantR),

        % When emulating the rbmm2 system in the paper and in Quan's thesis,
        % we omit the test for allocated-into for constant regions.
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, use_alloc_regions,
            UseAllocRegions),
        (
            UseAllocRegions = yes,
            list.filter(rptg_is_allocated_node(Graph),
                LConstantR, LFormalConstantAllocR)
        ;
            UseAllocRegions = no,
            LFormalConstantAllocR = LConstantR
        ),

        % Formal dead region arguments.
        set.to_sorted_list(DeadR, FormalDeadR),

        % Formal born region arguments.
        set.to_sorted_list(BornR, FormalBornR),

        RegionArgs =
            region_args(LFormalConstantAllocR, FormalDeadR, FormalBornR),
        map.det_insert(PPId, RegionArgs, !FormalRegionArgTable)
    ).

:- pred record_actual_region_arguments_goal(module_info::in,
    pred_proc_id::in, rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, hlds_goal::in,
    proc_formal_region_args_table::in, proc_formal_region_args_table::out,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_goal(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, Goal,
        !FormalRegionArgTable, !ActualRegionArgProc) :-
    Goal = hlds_goal(Expr, Info),
    record_actual_region_arguments_expr(ModuleInfo, Expr, Info, PPId,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable,
        !FormalRegionArgTable, !ActualRegionArgProc).

:- pred record_actual_region_arguments_expr(module_info::in,
    hlds_goal_expr::in, hlds_goal_info::in, pred_proc_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_formal_region_args_table::in, proc_formal_region_args_table::out,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_expr(ModuleInfo, GoalExpr, GoalInfo, CallerPPId,
        RptaInfoTable, ConstantRTable, DeadRTable, BornRTable,
        !FormalRegionArgTable, !ActualRegionArgProc) :-
    (
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        CalleePPId = proc(PredId, ProcId),
        ( if some_are_special_preds([CalleePPId], ModuleInfo) then
            true
        else
            CallSite = program_point_init(GoalInfo),
            record_actual_region_arguments_call_site(ModuleInfo, CallerPPId,
                CallSite, CalleePPId, RptaInfoTable, ConstantRTable,
                DeadRTable, BornRTable,
                !FormalRegionArgTable, !ActualRegionArgProc)
        )
    ;
        GoalExpr = conj(_, Conjuncts),
        list.foldl2(
            record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
            Conjuncts, !FormalRegionArgTable, !ActualRegionArgProc)
    ;
        GoalExpr = disj(Disjuncts),
        list.foldl2(
            record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
            Disjuncts, !FormalRegionArgTable, !ActualRegionArgProc)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Cond,
            !FormalRegionArgTable, !ActualRegionArgProc),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Then,
            !FormalRegionArgTable, !ActualRegionArgProc),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, Else,
            !FormalRegionArgTable, !ActualRegionArgProc)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl2(
            record_actual_region_arguments_case(ModuleInfo, CallerPPId,
                RptaInfoTable, ConstantRTable, DeadRTable, BornRTable),
            Cases, !FormalRegionArgTable, !ActualRegionArgProc)
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        sorry($pred, "generic_call NYI")
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        sorry($pred, "call_foreign_proc NYI")
    ;
        GoalExpr = negation(SubGoal),
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, SubGoal,
            !FormalRegionArgTable, !ActualRegionArgProc)
    ;
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = scope(_, SubGoal),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        record_actual_region_arguments_goal(ModuleInfo, CallerPPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable, SubGoal,
            !FormalRegionArgTable, !ActualRegionArgProc)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred record_actual_region_arguments_case(module_info::in,
    pred_proc_id::in, rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, case::in,
    proc_formal_region_args_table::in, proc_formal_region_args_table::out,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_case(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, Case,
        !FormalRegionArgTable, !ActualRegionArgProc) :-
    Case = case(_, _, Goal),
    record_actual_region_arguments_goal(ModuleInfo, PPId, RptaInfoTable,
        ConstantRTable, DeadRTable, BornRTable, Goal,
        !FormalRegionArgTable, !ActualRegionArgProc).

    % Region variables in deadR and in bornR are passed as arguments.
    % Out of the region variables in constantR (constant in the sense that
    % their bindings will not change during the call) we only pass ones that
    % may be allocated into as arguments. The actual region arguments are
    % computed according to these lines.
    %
:- pred record_actual_region_arguments_call_site(module_info::in,
    pred_proc_id::in, program_point::in, pred_proc_id::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_formal_region_args_table::in, proc_formal_region_args_table::out,
    pp_actual_region_args_table::in, pp_actual_region_args_table::out) is det.

record_actual_region_arguments_call_site(ModuleInfo, CallerPPId, CallSite,
        CalleePPId, RptaInfoTable, ConstantRTable, DeadRTable,
        BornRTable, !FormalRegionArgTable, !ActualRegionArgProc) :-
    ( if
        map.search(!.FormalRegionArgTable, CalleePPId, FormalRegionArgCallee)
    then
        % If the formal region arguments of the called procedure have been
        % computed, the corresponding actual ones can be straightforwardly
        % derived using the call site's alpha mapping.
        FormalRegionArgCallee =
            region_args(FormalConstants, FormalDeads, FormalBorns),
        map.lookup(RptaInfoTable, CallerPPId, CallerRptaInfo),
        CallerRptaInfo = rpta_info(_CallerGraph, CallerAlpha),
        map.lookup(CallerAlpha, CallSite, AlphaAtCallSite),
        list.foldr(find_actual_param(AlphaAtCallSite), FormalConstants,
            [], ActualConstants),
        list.foldr(find_actual_param(AlphaAtCallSite), FormalDeads,
            [], ActualDeads),
        list.foldr(find_actual_param(AlphaAtCallSite), FormalBorns,
            [], ActualBorns),
        map.det_insert(CallSite,
            region_args(ActualConstants, ActualDeads, ActualBorns),
            !ActualRegionArgProc)
    else
        % The formal region arguments of the called procedure haven't been
        % recorded, so do it now.
        record_formal_region_arguments_proc(ModuleInfo, CalleePPId,
            RptaInfoTable, ConstantRTable, DeadRTable, BornRTable,
            !FormalRegionArgTable),

        % We try again at this call site after the formal region arguments
        % are recorded.
        disable_warning [suspicious_recursion] (
            record_actual_region_arguments_call_site(ModuleInfo, CallerPPId,
                CallSite, CalleePPId, RptaInfoTable, ConstantRTable,
                DeadRTable, BornRTable,
                !FormalRegionArgTable, !ActualRegionArgProc)
        )
    ).

:- pred find_actual_param(rpt_call_alpha_mapping::in, rptg_node::in,
    list(rptg_node)::in, list(rptg_node)::out) is det.

find_actual_param(Alpha_PP, Formal, Actuals0, Actuals) :-
    map.lookup(Alpha_PP, Formal, Actual),
    Actuals = [Actual | Actuals0].

%---------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.region_arguments.
%---------------------------------------------------------------------------%
