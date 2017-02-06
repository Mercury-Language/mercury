%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File rbmm.interproc_region_lifetime.m.
% Main author: Quan Phan.
%
% This module detects lifetime of regions across procedure boundary. It
% updates the initial bornR and deadR, then computing constantR for each
% procedure. It also provides a predicate to eliminate primitive regions
% (i.e., ones that do not actually exist if primitive values are not boxed)
% from analysis information.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.interproc_region_lifetime.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_liveness_info.

%-----------------------------------------------------------------------------%

    % This predicate reasons about lifetime of regions across procedure
    % boundary. It will update the initial deadR and bornR sets and compute
    % constantR set.
    %
:- pred compute_interproc_region_lifetime(module_info::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::out, proc_region_set_table::in,
    proc_region_set_table::out, proc_region_set_table::in,
    proc_region_set_table::out) is det.

    % This predicate removes regions of primitive types from the input data
    % structures.
    % The reason for this is the assumption that primitive values are not
    % boxed (i.e., not store in regions).
    %
:- pred ignore_primitive_regions(module_info::in, rpta_info_table::in,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_region_set_table::in, proc_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.

%-----------------------------------------------------------------------------%
%
% Computing across procedure region lifetime.
%

compute_interproc_region_lifetime(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, InputRTable, OutputRTable,
        ConstantRTable, !BornRTable, !DeadRTable) :-

    apply_live_region_born_removal_rules(ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, !BornRTable),
    apply_live_region_dead_removal_rules(ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, !DeadRTable),
    map.foldl(compute_constantR(InputRTable, OutputRTable, !.BornRTable),
        !.DeadRTable, map.init, ConstantRTable).

:- pred compute_constantR(proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    pred_proc_id::in, region_set::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.

compute_constantR(InputRTable, OutputRTable, BornRTable, PPId, DeadR,
        !ConstantRTable) :-
    map.lookup(InputRTable, PPId, InputR),
    map.lookup(OutputRTable, PPId, OutputR),
    map.lookup(BornRTable, PPId, BornR),
    set.union(InputR, OutputR, InputOutputR0),
    set.difference(InputOutputR0, BornR, InputOutputR),
    set.difference(InputOutputR, DeadR, ConstantR),
    map.set(PPId, ConstantR, !ConstantRTable).

    % Apply the live region analysis rules to update bornR and deadR
    % sets of each procedure.
    %
:- pred apply_live_region_dead_removal_rules(module_info::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::out) is det.

apply_live_region_dead_removal_rules(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, !DeadRTable) :-
    apply_live_region_rule(dead_removal_rules, ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, !DeadRTable).

:- pred apply_live_region_born_removal_rules(module_info::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::out) is det.

apply_live_region_born_removal_rules(ModuleInfo, RptaInfoTable, ExecPathTable,
    LRBeforeTable, LRAfterTable, !BornRTable) :-
    apply_live_region_rule(born_removal_rules, ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, !BornRTable).

:- type rule_pred == (
        pred(pred_proc_id, region_set, region_set, proc_region_set_table,
            rpt_call_alpha_mapping, region_set)
    ).
:- inst rule_pred == ( pred(in, in, in, in, in, out) is det ).

:- pred apply_live_region_rule(rule_pred::in(rule_pred), module_info::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::out) is det.

apply_live_region_rule(Rule, ModuleInfo, RptaInfoTable, ExecPathTable,
    LRBeforeTable, LRAfterTable, !ProcRegionSetTable) :-
    module_info_ensure_dependency_info(ModuleInfo, ModuleInfo1),
    module_info_get_maybe_dependency_info(ModuleInfo1, MaybeDepInfo),
    (
        MaybeDepInfo = yes(DepInfo),
        hlds.hlds_module.hlds_dependency_info_get_dependency_ordering(
            DepInfo, DepOrdering),
        run_with_dependencies(Rule, DepOrdering, ModuleInfo1,
            RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
            !ProcRegionSetTable)
    ;
        MaybeDepInfo = no,
        unexpected($module, $pred, "no dependency info")
    ).

:- pred run_with_dependencies(rule_pred::in(rule_pred),
    dependency_ordering::in, module_info::in, rpta_info_table::in,
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.

run_with_dependencies(Rule, Deps, ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, !ProcRegionSetTable) :-
    % We want to proceed the SCC graph top-down so reverse the list
    % (the process is foldr2, but it is not yet in list module)
    list.reverse(Deps, Deps1),
    list.foldl(
        run_with_dependency(Rule, ModuleInfo, RptaInfoTable,
            ExecPathTable, LRBeforeTable, LRAfterTable),
        Deps1, !ProcRegionSetTable).

:- pred run_with_dependency(rule_pred::in(rule_pred),
    module_info::in, rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    list(pred_proc_id)::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.

run_with_dependency(Rule, ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, SCC, !ProcRegionSetTable) :-
    % Ignores special predicates.
    ( some_are_special_preds(SCC, ModuleInfo) ->
        true
    ;
        % Perform a fixpoint computation for each strongly connected
        % component.
        run_with_dependency_until_fixpoint(Rule, SCC, ModuleInfo,
            RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
            !ProcRegionSetTable)
    ).

:- pred run_with_dependency_until_fixpoint(rule_pred::in(rule_pred),
    list(pred_proc_id)::in, module_info::in, rpta_info_table::in,
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.

run_with_dependency_until_fixpoint(Rule, SCC, ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, !ProcRegionSetTable) :-
    % This call calculates the region set for each procedure in SCC
    list.foldl(apply_rule_pred_proc(Rule, ModuleInfo, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable),
        SCC, !.ProcRegionSetTable, ProcRegionSetTable1),
    (
        proc_region_set_table_equal(ProcRegionSetTable1, !.ProcRegionSetTable)
    ->
        % If all region_set's in the FPTable are intact update the main
        % ProcRegionSetTable.
        !:ProcRegionSetTable = ProcRegionSetTable1
    ;
        % Some is not fixed, start all over again
        run_with_dependency_until_fixpoint(Rule, SCC, ModuleInfo,
            RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
            ProcRegionSetTable1, !:ProcRegionSetTable)
    ).

:- pred apply_rule_pred_proc(rule_pred::in(rule_pred),
    module_info::in, rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    pred_proc_id::in, proc_region_set_table::in, proc_region_set_table::out)
    is det.

apply_rule_pred_proc(Rule, ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, PPId, !ProcRegionSetTable) :-
    % We need to follow each execution path and apply the two rules when
    % possible
    map.lookup(RptaInfoTable, PPId, RptaInfo),
    map.lookup(ExecPathTable, PPId, EPs),
    map.lookup(LRBeforeTable, PPId, ProcLRBefore),
    map.lookup(LRAfterTable, PPId, ProcLRAfter),

    % Here we analysing a caller but will update the region sets of its
    % callees.
    apply_live_region_rules_exec_paths(Rule, EPs, ExecPathTable, ModuleInfo,
        PPId, RptaInfo, RptaInfoTable, ProcLRBefore, ProcLRAfter,
        !ProcRegionSetTable).

:- pred apply_live_region_rules_exec_paths(rule_pred::in(rule_pred),
    list(execution_path)::in, execution_path_table::in, module_info::in,
    pred_proc_id::in, rpta_info::in, rpta_info_table::in,
    pp_region_set_table::in, pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::out) is det.

apply_live_region_rules_exec_paths(_Rule, [], _, _, _, _, _, _, _,
        !ProcRegionSetTable).
apply_live_region_rules_exec_paths(Rule, [ExecPath|ExecPaths], ExecPathTable,
        ModuleInfo, PPId, RptaInfo, RptaInfoTable, ProcLRBefore,
        ProcLRAfter, !ProcRegionSetTable) :-
    apply_live_region_rules_exec_path(Rule, ExecPath, ExecPathTable,
        ModuleInfo, PPId, RptaInfo, RptaInfoTable, ProcLRBefore, ProcLRAfter,
        !ProcRegionSetTable),
    apply_live_region_rules_exec_paths(Rule, ExecPaths, ExecPathTable,
        ModuleInfo, PPId, RptaInfo, RptaInfoTable, ProcLRBefore, ProcLRAfter,
        !ProcRegionSetTable).

    % Follow each execution path of a procedure and update deadR and bornR
    % sets
    %
:- pred apply_live_region_rules_exec_path(rule_pred::in(rule_pred),
    execution_path::in, execution_path_table::in, module_info::in,
    pred_proc_id::in, rpta_info::in, rpta_info_table::in,
    pp_region_set_table::in, pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::out) is det.

apply_live_region_rules_exec_path(_Rule, [], _, _, _, _, _, _, _,
        !ProcRegionSetTable).
apply_live_region_rules_exec_path(Rule, [ProgPoint - Goal | ProgPoint_Goals],
        ExecPathTable, ModuleInfo, PPId, RptaInfo, RptaInfoTable,
        ProcLRBefore, ProcLRAfter, !ProcRegionSetTable) :-
    Goal = hlds_goal(Expr, _),
    % The updating will only happen at call sites, i.e., when the goal
    % at the program point is a procedure call.
    ( if
        Expr = plain_call(CalleePredId, CalleeProcId, _, _, _, _)
      then
        CalleePPId = proc(CalleePredId, CalleeProcId),
        ( if
            some_are_special_preds([CalleePPId], ModuleInfo)
          then
            true
          else
            RptaInfo = rpta_info(_, AlphaMapping),
            map.lookup(AlphaMapping, ProgPoint, AlphaAtProgPoint),

            map.lookup(ProcLRBefore, ProgPoint, LRBefore),
            map.lookup(ProcLRAfter, ProgPoint, LRAfter),

            % apply a rule
            call(Rule, CalleePPId, LRBefore, LRAfter,
                !.ProcRegionSetTable, AlphaAtProgPoint, RegionSet),

            map.lookup(!.ProcRegionSetTable, CalleePPId, RegionSet0),
            ( if
                set.equal(RegionSet, RegionSet0)
              then
                % i.e., no region is removed, so everything is the same
                % as before
                true
              else
                % some regions are removed, record the new set for q and ...
                map.set(CalleePPId, RegionSet, !ProcRegionSetTable),

                % ... those removals need to be propagated to the ones
                % called by q
                set.difference(RegionSet0, RegionSet, ToBeRemoved),
                list.foldl(
                    remove_this_region_from_callees_of_proc(CalleePPId,
                        ExecPathTable, ModuleInfo, RptaInfoTable),
                    set.to_sorted_list(ToBeRemoved), !ProcRegionSetTable)
            )
        )
      else
        % ignore other sorts of goal
        true
    ),
    apply_live_region_rules_exec_path(Rule, ProgPoint_Goals, ExecPathTable,
        ModuleInfo, PPId, RptaInfo, RptaInfoTable, ProcLRBefore, ProcLRAfter,
        !ProcRegionSetTable).

%-----------------------------------------------------------------------------%
%
% Live region analysis rules.
%
% Those rules ensure that:
% 1. when it is not safe for a procedure to remove a region, that region must
% not be in the procedure's deadR seti,
% 2. when a region exists before the procedure is called, that region must not
% be in the procedure's bornR set.
%

    % Rules for eliminating regions from deadR set.
    %
:- pred dead_removal_rules(pred_proc_id::in, region_set::in, region_set::in,
    proc_region_set_table::in, rpt_call_alpha_mapping::in, region_set::out)
    is det.

dead_removal_rules(Q_Id, LRBefore, LRAfter, DeadRTable, AlphaAtPP, DeadR_q) :-
    % The current deadR of q.
    map.lookup(DeadRTable, Q_Id, DeadR_q0),

    % Apply dead removal rule L1 for r that is live before and after the
    % call to q.
    set.intersect(LRBefore, LRAfter, Rule1_Candidate),
    set.fold(dead_removal_rule_1(AlphaAtPP), Rule1_Candidate,
        DeadR_q0, DeadR_q1),

    % Remove deadR rule L2.
    targets_with_more_than_one_source(AlphaAtPP, Targets),
    set.fold(dead_removal_rule_2(AlphaAtPP), Targets, DeadR_q1, DeadR_q).

:- pred dead_removal_rule_1(rpt_call_alpha_mapping::in, rptg_node::in,
    region_set::in, region_set::out) is det.

dead_removal_rule_1(AlphaAtCallSite, Region, !DeadR_q) :-
    % Find r' such that alpha(r') = Region.
    solutions(map.inverse_search(AlphaAtCallSite, Region), SourceList),
    set.list_to_set(SourceList, RPrimes),

    % Remove any r' that is in deadR(q).
    set.difference(!.DeadR_q, RPrimes, !:DeadR_q).

:- pred dead_removal_rule_2(rpt_call_alpha_mapping::in, rptg_node::in,
    set(rptg_node)::in, set(rptg_node)::out) is det.

dead_removal_rule_2(AlphaAtCallSite, Region, !DeadR_q) :-
    solutions(map.inverse_search(AlphaAtCallSite, Region), SourceList),
    set.list_to_set(SourceList, RPrimes),
    set.difference(!.DeadR_q, RPrimes, !:DeadR_q).

    % rules for eliminating regions from bornR set.
    %
:- pred born_removal_rules(pred_proc_id::in, region_set::in, region_set::in,
    proc_region_set_table::in, rpt_call_alpha_mapping::in,
    region_set::out) is det.

born_removal_rules(Q_Id, LRBefore, _, BornRTable, AlphaAtCallSite, BornR_q) :-
    % The current bornR of q.
    map.lookup(BornRTable, Q_Id, BornR_q0),

    % Apply born removal rule L3 for r that is live before and after the
    % call to q.
    set.fold(born_removal_rule_1(AlphaAtCallSite), LRBefore,
        BornR_q0, BornR_q1),

    % remove bornR rule L4,
    targets_with_more_than_one_source(AlphaAtCallSite, Targets),
    set.fold(born_removal_rule_2(AlphaAtCallSite), Targets,
        BornR_q1, BornR_q).

:- pred born_removal_rule_1(rpt_call_alpha_mapping::in, rptg_node::in,
    set(rptg_node)::in, set(rptg_node)::out) is det.

born_removal_rule_1(AlphaAtCallSite, Region, !BornR_q) :-
    solutions(map.inverse_search(AlphaAtCallSite, Region), SourceList),
    set.list_to_set(SourceList, RPrimes),
    set.difference(!.BornR_q, RPrimes, !:BornR_q).

    % alpha(r') = r, alpha(r'') = r, r', r'' in bornR(q) imply remove r',
    % r'' from bornR(q).
    %
:- pred born_removal_rule_2(rpt_call_alpha_mapping::in, rptg_node::in,
    set(rptg_node)::in, set(rptg_node)::out) is det.

born_removal_rule_2(AlphaAtCallSite, Region, !BornR_q) :-
    solutions(map.inverse_search(AlphaAtCallSite, Region), SourceList),
    set.list_to_set(SourceList, RPrimes),
    set.difference(!.BornR_q, RPrimes, !:BornR_q).

    % Find targets of alpha mapping that are mapped to by more than one
    % source.
    %
:- pred targets_with_more_than_one_source(rpt_call_alpha_mapping::in,
    region_set::out) is det.

targets_with_more_than_one_source(AlphaAtCallSite, Targets) :-
    map.foldl2(process_one_mapping, AlphaAtCallSite, set.init,
        _Processed, set.init, Targets).

:- pred process_one_mapping(rptg_node::in, rptg_node::in, region_set::in,
    region_set::out, region_set::in, region_set::out) is det.

process_one_mapping(_Source, Target, !Candidates, !Targets) :-
    ( if
        set.contains(!.Candidates, Target)
      then
        set.insert(Target, !Targets)
      else
        set.insert(Target, !Candidates)
    ).

    % This predicate propagates the removal of a region from a deadR or
    % bornR sets of a procedure to the ones it calls, i.e., also remove
    % the region from the corresponding sets of them.
    %
:- pred remove_this_region_from_callees_of_proc(pred_proc_id::in,
    execution_path_table::in, module_info::in, rpta_info_table::in,
    rptg_node::in, proc_region_set_table::in, proc_region_set_table::out)
    is det.

remove_this_region_from_callees_of_proc(PPId, ExecPathTable, ModuleInfo,
        RptaInfoTable, Region, !ProcRegionSetTable) :-
    % to have the goal at each pp
    map.lookup(ExecPathTable, PPId, ExecPaths),

    % Follow execution paths of this procedure and remove the region from
    % this procedure's callees.
    remove_this_from_eps(ExecPaths, PPId, Region, ExecPathTable,
        ModuleInfo, RptaInfoTable, !ProcRegionSetTable).

    % Follow each execution path of a procedure and update deadR and bornR
    % sets.
    %
:- pred remove_this_from_eps(list(execution_path)::in, pred_proc_id::in,
    rptg_node::in, execution_path_table::in, module_info::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.
remove_this_from_eps([], _, _, _, _, _, !ProcRegionSetTable).
remove_this_from_eps([ExecPath | ExecPaths], PPId, Region, ExecPathTable,
        ModuleInfo, RptaInfoTable, !ProcRegionSetTable) :-
    remove_this_from_ep(ExecPath, PPId, Region, ExecPathTable, ModuleInfo,
        RptaInfoTable, !ProcRegionSetTable),
    remove_this_from_eps(ExecPaths, PPId, Region, ExecPathTable, ModuleInfo,
        RptaInfoTable, !ProcRegionSetTable).

:- pred remove_this_from_ep(execution_path::in, pred_proc_id::in,
    rptg_node::in, execution_path_table::in, module_info::in,
    rpta_info_table::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.
remove_this_from_ep([], _, _, _, _, _, !ProcRegionSetTable).
remove_this_from_ep([ProgPoint - Goal|ProgPoint_Goals], PPId,
        ToBeRemovedRegion, ExecPathTable, ModuleInfo, RptaInfoTable,
        !ProcRegionSetTable) :-
    Goal = hlds_goal(Expr, _Info),
    ( if
        Expr = plain_call(CalleePredId, CalleeProcId, _, _, _, _)
      then
        CalleePPId = proc(CalleePredId, CalleeProcId),
        ( if
            some_are_special_preds([CalleePPId], ModuleInfo)
          then
            true
          else
            % Find the alpha mapping: alpha(_, R) = ToBeRemovedRegion
            map.lookup(RptaInfoTable, PPId, RptaInfo_p),
            RptaInfo_p = rpta_info(_Graph_p, AlphaMapping),
            map.lookup(AlphaMapping, ProgPoint, AlphaAtCallSite),

            map.foldl(find_alpha_source(ToBeRemovedRegion),
                AlphaAtCallSite, set.init, Rs),

            % Remove the sources from the RegionSet (either deadR or
            % bornR) of this callee.
            map.lookup(!.ProcRegionSetTable, CalleePPId,
                RegionSet0),
            set.difference(RegionSet0, Rs, RegionSet1),

            % update the table and continue
            ( if
                set.equal(RegionSet0, RegionSet1)
              then
                % no need to update
                true
              else
                % Some is removed from deadR or bornR of this callee,
                % so we update the entry of this called and analyse it.
                map.set(CalleePPId, RegionSet1, !ProcRegionSetTable),
                set.difference(RegionSet0, RegionSet1, RemovedFromQ),

                % Call this one mutually recursively.
                list.foldl(
                    remove_this_region_from_callees_of_proc(CalleePPId,
                        ExecPathTable, ModuleInfo, RptaInfoTable),
                    set.to_sorted_list(RemovedFromQ), !ProcRegionSetTable)
            )
        )
      else
            % ignore other sorts of goals
            %
        true
    ),
    remove_this_from_ep(ProgPoint_Goals, PPId, ToBeRemovedRegion,
        ExecPathTable, ModuleInfo, RptaInfoTable, !ProcRegionSetTable).

:- pred find_alpha_source(rptg_node::in, rptg_node::in, rptg_node::in,
    set(rptg_node)::in, set(rptg_node)::out) is det.

find_alpha_source(ToBeRemovedRegion, Source, Target, !Rs) :-
    ( if
        ToBeRemovedRegion = Target
      then
        set.insert(Source, !Rs)
      else
        true
    ).

%-----------------------------------------------------------------------------%
%
% Eliminating primitive regions from live region analysis's information.
%

ignore_primitive_regions(ModuleInfo, RptaInfoTable, !BornRTable,
        !DeadRTable, !ConstantRTable, !LocalRTable, !LRBeforeTable,
        !LRAfterTable, !VoidVarRegionTable) :-
    map.foldl(eliminate_primitive_regions(ModuleInfo, RptaInfoTable),
        !.BornRTable, !BornRTable),
    map.foldl(eliminate_primitive_regions(ModuleInfo, RptaInfoTable),
        !.DeadRTable, !DeadRTable),
    map.foldl(eliminate_primitive_regions(ModuleInfo, RptaInfoTable),
        !.ConstantRTable, !ConstantRTable),
    map.foldl(eliminate_primitive_regions(ModuleInfo, RptaInfoTable),
        !.LocalRTable, !LocalRTable),

    map.foldl(eliminate_primitive_regions_2(ModuleInfo, RptaInfoTable),
        !.LRBeforeTable, !LRBeforeTable),
    map.foldl(eliminate_primitive_regions_2(ModuleInfo, RptaInfoTable),
        !.LRAfterTable, !LRAfterTable),
    map.foldl(eliminate_primitive_regions_2(ModuleInfo, RptaInfoTable),
        !.VoidVarRegionTable, !VoidVarRegionTable).

    % Eliminate regions of primitive types from the proc_region_set_table.
    %
:- pred eliminate_primitive_regions(module_info::in, rpta_info_table::in,
    pred_proc_id::in, region_set::in, proc_region_set_table::in,
    proc_region_set_table::out) is det.

eliminate_primitive_regions(ModuleInfo, RptaInfoTable, PPId, RegionSet0,
        !RegionSetTable) :-
    map.lookup(RptaInfoTable, PPId, RptaInfo),
    RptaInfo = rpta_info(Graph, _Alpha),
    set.fold(retain_non_primitive_regions(ModuleInfo, Graph), RegionSet0,
        set.init, RegionSet),
    map.det_update(PPId, RegionSet, !RegionSetTable).

:- pred retain_non_primitive_regions(module_info::in, rpt_graph::in,
    rptg_node::in, region_set::in, region_set::out) is det.

retain_non_primitive_regions(ModuleInfo, Graph, Region, !RegionSet) :-
    NodeType = rptg_lookup_node_type(Graph, Region),
    ( type_not_stored_in_region(NodeType, ModuleInfo) ->
        true
    ;
        set.insert(Region, !RegionSet)
    ).

    % Eliminate regions of primitive types from the proc_pp_region_set_table.
    %
:- pred eliminate_primitive_regions_2(module_info::in, rpta_info_table::in,
    pred_proc_id::in, pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::out) is det.

eliminate_primitive_regions_2(ModuleInfo, RptaInfoTable, PPId, LRProc0,
        !LRTable) :-
    map.lookup(RptaInfoTable, PPId, RptaInfo),
    RptaInfo = rpta_info(Graph, _Alpha),
    map.foldl(retain_non_primitive_regions_at_pp(ModuleInfo, Graph),
        LRProc0, map.init, LRProc),
    map.det_update(PPId, LRProc, !LRTable).

:- pred retain_non_primitive_regions_at_pp(module_info::in, rpt_graph::in,
    program_point::in, region_set::in, pp_region_set_table::in,
    pp_region_set_table::out) is det.

retain_non_primitive_regions_at_pp(ModuleInfo, Graph, ProgPoint,
        RegionSet0, !LRProc) :-
    set.fold(retain_non_primitive_regions(ModuleInfo, Graph), RegionSet0,
        set.init, RegionSet),
    map.set(ProgPoint, RegionSet, !LRProc).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.interproc_region_lifetime.
%-----------------------------------------------------------------------------%
