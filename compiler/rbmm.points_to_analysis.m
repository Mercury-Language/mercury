%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File rbmm.points_to_analysis.m.
% Main author: Quan Phan.
%
% This module implements the region points-to analysis (rpta), which collects
% for each procedure a region points-to graph representing the splitting of
% the heap used by the procedure into regions, i.e., which variables are
% stored in which regions. Because the region model is polymorphic, i.e., we
% can pass different actual regions for region arguments, the analysis also
% gathers the alpha mapping, which maps formal region parameters to actual
% ones at each call site in a procedure. So there are 2 sorts of information:
% region points-to graph (rptg) and alpha mapping.
%
% The analysis is composed of 2 phases:
%
%   1. intraprocedural analysis: only analyses unifications and compute only
%      rptgs.
%   2. interprocedural analysis: only analyses (plain) procedure calls,
%      compute both rptgs and alpha mappings.
%
% Currently the analysis ONLY collects the information, do NOT record it into
% the HLDS.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.points_to_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.rbmm.points_to_info.

%-----------------------------------------------------------------------------%

:- pred region_points_to_analysis(rpta_info_table::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.
:- import_module transform_hlds.ctgc.
:- import_module transform_hlds.ctgc.fixpoint_table.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

region_points_to_analysis(InfoTable, !ModuleInfo) :-
    rpta_info_table_init = InfoTable0,
    intra_proc_rpta(!.ModuleInfo, InfoTable0, InfoTable1),
    inter_proc_rpta(!.ModuleInfo, InfoTable1, InfoTable).

%----------------------------------------------------------------------------%
%
% Phase 1: intraprocedural region points-to analysis
%

:- pred intra_proc_rpta(module_info::in,
    rpta_info_table::in, rpta_info_table::out) is det.

intra_proc_rpta(ModuleInfo, !InfoTable) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    list.foldl(intra_proc_rpta_pred(ModuleInfo), PredIds, !InfoTable).

:- pred intra_proc_rpta_pred(module_info::in, pred_id::in,
    rpta_info_table::in, rpta_info_table::out) is det.

intra_proc_rpta_pred(ModuleInfo, PredId, !InfoTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl(intra_proc_rpta_proc(ModuleInfo, PredId), ProcIds, !InfoTable).

:- pred intra_proc_rpta_proc(module_info::in, pred_id::in, proc_id::in,
    rpta_info_table::in, rpta_info_table::out) is det.

intra_proc_rpta_proc(ModuleInfo, PredId, ProcId, !InfoTable) :-
    PPId = proc(PredId, ProcId),
    ( if some_are_special_preds([PPId], ModuleInfo) then
        true
    else
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        RptaInfo0 = rpta_info_init(ProcInfo),
        proc_info_get_goal(ProcInfo, Goal),
        intra_analyse_goal(Goal, RptaInfo0, RptaInfo),
        rpta_info_table_set_rpta_info(PPId, RptaInfo, !InfoTable)
    ).

:- pred intra_analyse_goal(hlds_goal::in, rpta_info::in, rpta_info::out)
    is det.

intra_analyse_goal(Goal, !RptaInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    intra_analyse_goal_expr(GoalExpr, !RptaInfo).

:- pred intra_analyse_goal_expr(hlds_goal_expr::in,
    rpta_info::in, rpta_info::out) is det.

intra_analyse_goal_expr(conj(_ConjType, Goals), !RptaInfo) :-
    list.foldl(intra_analyse_goal, Goals, !RptaInfo).

    % Calls (of all types) are not considered during the intraprocedural
    % analysis.
    %
intra_analyse_goal_expr(plain_call(_, _, _, _, _, _), !RptaInfo).
intra_analyse_goal_expr(generic_call(_, _, _, _, _), !RptaInfo).
intra_analyse_goal_expr(call_foreign_proc(_, _, _, _, _, _, _), !RptaInfo).

intra_analyse_goal_expr(switch(_, _, Cases), !RptaInfo) :-
    list.foldl(intra_analyse_case, Cases, !RptaInfo).
intra_analyse_goal_expr(disj(Goals), !RptaInfo) :-
    list.foldl(intra_analyse_goal, Goals, !RptaInfo).
intra_analyse_goal_expr(negation(Goal), !RptaInfo) :-
    intra_analyse_goal(Goal, !RptaInfo).
intra_analyse_goal_expr(unify(_, _, _, Unification, _), !RptaInfo) :-
    intra_analyse_unification(Unification, !RptaInfo).

    % scope
    % XXX: only analyse the goal. May need to take into account the Reason.
    %
intra_analyse_goal_expr(scope(_Reason, Goal), !RptaInfo) :-
%    (
%       ( Reason = exist_quant(_)
%        ; Reason = promise_solutions(_, _)      % XXX ???
%        ; Reason = promise_purity(_, _)
%        ; Reason = commit(_)                    % XXX ???
%        ; Reason = barrier(_)
%        ; Reason = trace_goal(_, _, _, _, _)
%        ; Reason = from_ground_term(_)
%        ),
        intra_analyse_goal(Goal, !RptaInfo).
%    ;
%        Msg = "intra_analyse_goal_expr: Scope's reason of from_ground_term "
%            ++ "not handled",
%        unexpected($pred, Msg)
%    ).

intra_analyse_goal_expr(if_then_else(_Vars, If, Then, Else), !RptaInfo) :-
    intra_analyse_goal(If, !RptaInfo),
    intra_analyse_goal(Then, !RptaInfo),
    intra_analyse_goal(Else, !RptaInfo).

intra_analyse_goal_expr(shorthand(_), _, _) :-
    % These should have been expanded out by now.
    unexpected($pred, "shorthand").

:- pred intra_analyse_case(case::in, rpta_info::in, rpta_info::out) is det.

intra_analyse_case(Case, !RptaInfo) :-
    Case = case(_, _, Goal),
    intra_analyse_goal(Goal, !RptaInfo).

%-----------------------------------------------------------------------------%

    % For construction and deconstruction unifications, add an edge from
    % the node of the variable on the LHS to that of each variable on the RHS.
    % For construction, also mark the node of lhs variable allocated.
    %
    % For assignment unifications we merge the nodes corresponding to
    % the variables on either side.
    %
    % For simple test unifications we do nothing.
    %
:- pred intra_analyse_unification(unification::in,
    rpta_info::in, rpta_info::out) is det.

intra_analyse_unification(construct(LVar, ConsId, RVars, _, _, _, _),
        rpta_info(!.Graph, AlphaMapping), rpta_info(!:Graph, AlphaMapping)) :-
    % Mark allocated.
    rptg_get_node_by_variable(!.Graph, LVar, LNode),
    LNodeContent0 = rptg_get_node_content(!.Graph, LNode),
    LNodeContent0 = rptg_node_content(A, B, C, D, _IsAlloc),
    LNodeContent = rptg_node_content(A, B, C, D, bool.yes),
    rptg_set_node_content(LNode, LNodeContent, !Graph),

    % Add edges.
    list.foldl2(process_cons_and_decons(LVar, ConsId), RVars, 1, _, !Graph).

intra_analyse_unification(deconstruct(LVar, ConsId, RVars, _, _, _),
        rpta_info(!.Graph, AlphaMapping), rpta_info(!:Graph, AlphaMapping)) :-
    list.foldl2(process_cons_and_decons(LVar, ConsId), RVars, 1, _, !Graph).
intra_analyse_unification(assign(ToVar, FromVar),
    rpta_info(!.Graph, AlphaMapping), rpta_info(!:Graph, AlphaMapping)) :-
    rptg_get_node_by_variable(!.Graph, ToVar, ToNode),
    rptg_get_node_by_variable(!.Graph, FromVar, FromNode),
    ( if ToNode = FromNode then
        true
    else
        unify_operator(ToNode, FromNode, !Graph),
        % After merging the two nodes, apply rule P1 to restore the
        % RPTG's invariants.
        rule_1(ToNode, !Graph)
    ).
intra_analyse_unification(simple_test(_, _), !RptaInfo).
intra_analyse_unification(complicated_unify(_, _, _), _, _) :-
    unexpected($pred, "complicated_unify").

:- pred process_cons_and_decons(prog_var::in, cons_id::in, prog_var::in,
    int::in, int::out, rpt_graph::in, rpt_graph::out) is det.

process_cons_and_decons(LVar, ConsId, RVar, !Component, !Graph) :-
    rptg_get_node_by_variable(!.Graph, LVar, LNode),
    rptg_get_node_by_variable(!.Graph, RVar, RNode),
    Sel = [termsel(ConsId, !.Component)],
    EdgeLabel = rptg_edge_content(Sel),

    % Only add the edge if it is not in the graph.
    % It is more suitable to the edge_operator's semantics if we check
    % this inside the edge_operator. But we also want to know if the edge
    % is actually added or not so it is convenient to check the edge's
    % existence outside edge_operator. Otherwise we can extend edge_operator
    % with one more argument to indicate that.
    ( if rptg_edge_in_graph(LNode, EdgeLabel, RNode, !.Graph) then
        true
    else
        edge_operator(LNode, RNode, EdgeLabel, !Graph),

        % After an edge is added, rules P2 and P3 are applied to ensure
        % the invariants of the graph.
        rule_2(LNode, RNode, ConsId, !.Component, !Graph),

        % In case the node containing RVar might have changed.
        rptg_get_node_by_variable(!.Graph, RVar, RVarNode),
        rule_3(RVarNode, !Graph)
    ),
    !:Component = !.Component + 1.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Phase 2: interprocedural region points-to analysis
%

    % The interprocedural analysis requires fixpoint computation,
    % so we will compute a fixpoint for each strongly connected component.
    %
:- pred inter_proc_rpta(module_info::in,
    rpta_info_table::in, rpta_info_table::out) is det.

inter_proc_rpta(ModuleInfo0, !InfoTable) :-
    module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo, DepInfo),
    BottomUpSCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    run_with_dependencies(BottomUpSCCs, ModuleInfo, !InfoTable).

:- pred run_with_dependencies(hlds_bottom_up_dependency_sccs::in,
    module_info::in, rpta_info_table::in, rpta_info_table::out) is det.

run_with_dependencies(BottomUpSCCs, ModuleInfo, !InfoTable) :-
    list.foldl(run_with_dependency(ModuleInfo), BottomUpSCCs, !InfoTable).

:- pred run_with_dependency(module_info::in, set(pred_proc_id)::in,
    rpta_info_table::in, rpta_info_table::out) is det.

run_with_dependency(ModuleInfo, SCC, !InfoTable) :-
    set.to_sorted_list(SCC, SCCProcs),
    ( if some_are_special_preds(SCCProcs, ModuleInfo) then
        % Analysis ignores special predicates.
        true
    else
        % Run the fixpoint computation on the SCC.
        FPTable = init_rpta_fixpoint_table(SCCProcs, !.InfoTable),
        run_with_dependency_until_fixpoint(SCC, FPTable, ModuleInfo,
            !InfoTable)
    ).

:- pred run_with_dependency_until_fixpoint(scc::in,
    rpta_fixpoint_table::in, module_info::in, rpta_info_table::in,
    rpta_info_table::out) is det.

run_with_dependency_until_fixpoint(SCC, FPTable0, ModuleInfo, !InfoTable) :-
    set.foldl(inter_analyse_proc(ModuleInfo, !.InfoTable), SCC,
        FPTable0, FPTable1),
    ( if fixpoint_reached(FPTable1) then
        % If we have reached a fixpoint for this SCC then update the
        % RPTA info table.
        set.foldl(update_rpta_info_in_rpta_info_table(FPTable1), SCC,
            !InfoTable)
    else
        % Otherwise, begin the next iteration.
        new_run(FPTable1, FPTable),
        run_with_dependency_until_fixpoint(SCC, FPTable, ModuleInfo,
            !InfoTable)
    ).

:- pred inter_analyse_proc(module_info::in, rpta_info_table::in,
    pred_proc_id::in, rpta_fixpoint_table::in, rpta_fixpoint_table::out)
    is det.

inter_analyse_proc(ModuleInfo, InfoTable, PPId, !FPTable) :-
    % Look up the procedure's rpta_info.
    % If this is the first iteration then the rtpa_info we use is the
    % one computed for this procedure during the intraprocedural analysis.
    %
    lookup_rpta_info(PPId, InfoTable, !FPTable, ProcRptaInfo0, _),

    % Start the analysis of the procedure's body.
    %
    % We will need the information about program point for storing alpha
    % mapping.
    %
    % XXX we should only fill goal path slots once, not once per iteration.
    %
    module_info_proc_info(ModuleInfo, PPId, ProcInfo0),
    fill_goal_path_slots_in_proc(ModuleInfo, ProcInfo0, ProcInfo),

    proc_info_get_goal(ProcInfo, Goal),
    inter_analyse_goal(ModuleInfo, InfoTable, Goal, !FPTable,
        ProcRptaInfo0, ProcRptaInfo),

    % Put the result of this iteration into the fixpoint table.
    %
    rpta_fixpoint_table_new_rpta_info(PPId, ProcRptaInfo, !FPTable).

%-----------------------------------------------------------------------------%
%
% Code for interprocedural analysis of goals.
%

    % Analyse a given goal, with module_info and fixpoint table
    % to lookup extra information, starting from an initial abstract
    % substitution, and creating a new one. During this process,
    % the fixpoint table might change (when recursive predicates are
    % encountered).
    %
:- pred inter_analyse_goal(module_info::in,
    rpta_info_table::in, hlds_goal::in,
    rpta_fixpoint_table::in, rpta_fixpoint_table::out,
    rpta_info::in, rpta_info::out) is det.

inter_analyse_goal(ModuleInfo, InfoTable, Goal, !FPtable, !RptaInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    inter_analyse_goal_expr(GoalExpr, GoalInfo, ModuleInfo, InfoTable,
        !FPtable, !RptaInfo).

:- pred inter_analyse_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    module_info::in, rpta_info_table::in, rpta_fixpoint_table::in,
    rpta_fixpoint_table::out, rpta_info::in, rpta_info::out) is det.

inter_analyse_goal_expr(conj(_ConjType, Goals), _, ModuleInfo,
        InfoTable, !FPTable, !RptaInfo) :-
    list.foldl2(inter_analyse_goal(ModuleInfo, InfoTable), Goals,
        !FPTable, !RptaInfo).

    % There are two rpta_info's:
    % one is of the currently-analysed procedure (caller) which we are going
    % to update, the other is of the called procedure (callee).
    %
    % The input RptaInfo is caller's, if the procedure calls itself then
    % this is also that of the callee but we will retrieve it again from the
    % InfoTable.
    %
inter_analyse_goal_expr(Goal, GoalInfo, ModuleInfo, InfoTable,
        !FPTable, !CallerRptaInfo) :-
    Goal = plain_call(PredId, ProcId, ActualParams, _, _, _),
    CalleePPId = proc(PredId, ProcId),

    % Get callee's rpta_info.
    % As what I assume now, after the intraprocedural analysis we have all
    % the rpta_info's of all the procedures in the InfoTable, therefore
    % this lookup cannot fail. But it sometimes fails because the callee
    % can be imported procedures, built-ins and so forth which are not
    % analysed by the intraprocedural analysis. In such cases, I assume that
    % the rpta_info of the caller is not updated, because no information is
    % available from the callee.
    % When IsInit = no, the CalleeRptaInfo is dummy.
    lookup_rpta_info(CalleePPId, InfoTable, !FPTable, CalleeRptaInfo, IsInit),
    (
        IsInit = yes
    ;
        IsInit = no,
        CallSite = program_point_init(GoalInfo),
        CalleeRptaInfo = rpta_info(CalleeGraph, _),

        % Collect alpha mapping at this call site.
        module_info_proc_info(ModuleInfo, CalleePPId, CalleeProcInfo),
        proc_info_get_headvars(CalleeProcInfo, FormalParams),
        !.CallerRptaInfo = rpta_info(CallerGraph0, CallerAlphaMappings0),
        alpha_mapping_at_call_site(FormalParams, ActualParams, CalleeGraph,
            CallerGraph0, CallerGraph,
            map.init, CallerAlphaMappingAtCallSite),
        map.set(CallSite, CallerAlphaMappingAtCallSite,
            CallerAlphaMappings0, CallerAlphaMappings),
        CallerRptaInfo1 = rpta_info(CallerGraph, CallerAlphaMappings),

        % Follow the edges from the nodes rooted at the formal parameters
        % (in the callee's graph) and apply the interprocedural rules to
        % complete the alpha mapping and update the caller's graph with
        % the information from the callee's graph.
        map.keys(CallerAlphaMappingAtCallSite, FormalNodes),
        apply_rules(FormalNodes, CallSite, [], CalleeRptaInfo,
            CallerRptaInfo1, !:CallerRptaInfo)
    ).

inter_analyse_goal_expr(generic_call(_, _, _, _, _), _, _, _, !FPTable,
        !RptaInfo) :-
    sorry($pred, "generic_call not handled").

inter_analyse_goal_expr(switch(_, _, Cases), _, ModuleInfo, InfoTable,
        !FPTable, !RptaInfo) :-
    list.foldl2(inter_analyse_case(ModuleInfo, InfoTable), Cases,
        !FPTable, !RptaInfo).

    % Unifications are ignored in interprocedural analysis
    %
inter_analyse_goal_expr(unify(_, _, _, _, _), _, _, _, !FPTable, !RptaInfo).

inter_analyse_goal_expr(disj(Disjs), _, ModuleInfo, InfoTable,
        !FPTable, !RptaInfo) :-
    list.foldl2(inter_analyse_goal(ModuleInfo, InfoTable), Disjs,
        !FPTable, !RptaInfo).

inter_analyse_goal_expr(negation(Goal), _, ModuleInfo, InfoTable,
        !FPTable, !RptaInfo) :-
    inter_analyse_goal(ModuleInfo, InfoTable, Goal, !FPTable, !RptaInfo).

    % XXX: may need to take into account the Reason.
    % for now just analyse the goal.
    %
inter_analyse_goal_expr(scope(_Reason, Goal), _, ModuleInfo, InfoTable,
        !FPTable, !RptaInfo) :-
%    (
%        ( Reason = exist_quant(_)
%        ; Reason = promise_solutions(_, _)      % XXX ???
%        ; Reason = promise_purity(_, _)
%        ; Reason = commit(_)                    % XXX ???
%        ; Reason = barrier(_)
%        ; Reason = trace_goal(_, _, _, _, _)
%        ; Reason = from_ground_term(_)
%        ),
        inter_analyse_goal(ModuleInfo, InfoTable, Goal, !FPTable, !RptaInfo).
%    ;
%        Msg = "inter_analyse_goal_expr: Scope's reason of from_ground_term "
%            ++ "not handled",
%        unexpected($pred, Msg)
%    ).

inter_analyse_goal_expr(if_then_else(_Vars, If, Then, Else), _, ModuleInfo,
        InfoTable, !FPTable, !RptaInfo) :-
    inter_analyse_goal(ModuleInfo, InfoTable, If, !FPTable, !RptaInfo),
    inter_analyse_goal(ModuleInfo, InfoTable, Then, !FPTable, !RptaInfo),
    inter_analyse_goal(ModuleInfo, InfoTable, Else, !FPTable, !RptaInfo).

inter_analyse_goal_expr(GoalExpr, _, _, _, !FPTable, !RptaInfo) :-
    GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
    sorry($pred, "foreign code").

inter_analyse_goal_expr(shorthand(_), _, _, _, !FPTable, !RptaInfo) :-
    unexpected($pred, "shorthand").

:- pred inter_analyse_case(module_info::in,
    rpta_info_table::in, case::in, rpta_fixpoint_table::in,
    rpta_fixpoint_table::out, rpta_info::in, rpta_info::out) is det.

inter_analyse_case(ModuleInfo, InfoTable, Case, !FPtable, !RptaInfo) :-
    Case = case(_, _, Goal),
    inter_analyse_goal(ModuleInfo, InfoTable, Goal, !FPtable, !RptaInfo).

%-----------------------------------------------------------------------------%

    % As said above, the rpta_info of a procedure when it is looked
    % up in interprocedural analysis is either in the InfoTable or in the
    % fixpoint table. If the procedure happens to be imported ones, built-ins,
    % and so on, we returns no and initialize the lookup value to a dummy
    % value.
    %
:- pred lookup_rpta_info(pred_proc_id::in, rpta_info_table::in,
    rpta_fixpoint_table::in, rpta_fixpoint_table::out,
    rpta_info::out, bool::out) is det.

lookup_rpta_info(PPId, InfoTable, !FPtable, RptaInfo, Init) :-
    ( if
        % First look up in the current fixpoint table, ...
        get_from_fixpoint_table(PPId, RptaInfo0, !.FPtable, FPtable1)
    then
        RptaInfo  = RptaInfo0,
        !:FPtable = FPtable1,
        Init = bool.no
    else
        % ... second look up among already recorded rpta_info.
        ( if
            RptaInfo0 = rpta_info_table_search_rpta_info(PPId, InfoTable)
        then
            RptaInfo = RptaInfo0,
            Init = bool.no
        else
            % Initialize a dummy.
            RptaInfo = rpta_info(rpt_graph_init, map.init),
            Init = bool.yes
        )
    ).

:- pred update_rpta_info_in_rpta_info_table(rpta_fixpoint_table::in,
    pred_proc_id::in, rpta_info_table::in, rpta_info_table::out) is det.

update_rpta_info_in_rpta_info_table(FPTable, PPId, !InfoTable) :-
    RptaInfo = get_from_fixpoint_table_final(PPId, FPTable),
    rpta_info_table_set_rpta_info(PPId, RptaInfo, !InfoTable).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Invariants for RPTGs.
%

    % Rule P1:
    % After two nodes are unified, it can happen that the unified node has
    % two edges with the same label pointing to 2 different nodes. This rule
    % ensures that if it happens the 2 nodes will also be unified.
    %
    % The algorithm is as follows.
    % 1. If the node has no or one outedge we have to do nothing and the
    %    predicate quits.
    % 2. The node has > 1 outedges, take one of them,
    %    find in the rest another edge that has the same label,
    %    unify the end nodes of the two edges.
    %    Because of this unification of the end nodes,
    %    more unifications are probably triggered.
    % 3. If a unification happens, start all over again with the same node
    %    and the *updated* graph that has one less outedge from the node.
    %
:- pred rule_1(rptg_node::in, rpt_graph::in, rpt_graph::out) is det.

rule_1(Node, !Graph) :-
    % XXX This might not be needed because we have just unified into Node.
    rptg_get_node_by_node(!.Graph, Node, UnifiedNode),

    OutEdgesOfUnifiedNode = rptg_lookup_list_outedges(!.Graph, UnifiedNode),
    (
        OutEdgesOfUnifiedNode = [E | Es],
        merge_nodes_reached_by_same_labelled_edges(E, Es, Es, !Graph,
            Happened),
        (
            Happened = bool.no
        ;
            % Some nodes have been merged, so size of !:Graph is strictly
            % smaller than that of !.Graph and at some point this predicate
            % will end up in the then-branch.
            Happened = bool.yes,
            rule_1(UnifiedNode, !Graph)
        )
    ;
        OutEdgesOfUnifiedNode = []
    ).

    % merge_nodes_reached_by_same_labelled_edges(Edge, EdgeList, Rest,
    % !Graph, Happened) unifies the end nodes of Edge and of an edge
    % in EdgeList that has the same label as the input edge.
    % When one such an edge found,
    % the predicate will not look further in the list.
    % The unification of nodes, if happends (Happened = yes),
    % will be propagated by calling rule_1 predicate mutually recursively.
    %
    % The loop in this predicate is similar to
    % for i = ... to N - 1
    %    for j = i+1 to N ...
    % ...
    %
:- pred merge_nodes_reached_by_same_labelled_edges(rptg_edge::in,
    list(rptg_edge)::in, list(rptg_edge)::in, rpt_graph::in, rpt_graph::out,
    bool::out) is det.

    % This clause is reached when no unification of nodes happened and
    % all the out-edges have been considered (Rest = []).
    %
merge_nodes_reached_by_same_labelled_edges(_, [], [], !Graph, bool.no).

    % This clause is reached when some out-edges still need to be processed.
    %
merge_nodes_reached_by_same_labelled_edges(_, [], [E | Es], !Graph,
        Happened) :-
    merge_nodes_reached_by_same_labelled_edges(E, Es, Es, !Graph, Happened).

merge_nodes_reached_by_same_labelled_edges(Edge, [Ed | Eds], Rest, !Graph,
        Happened) :-
    % We do not allow two edges with the same label from one node to another.
    % So End and E below must be definitely different nodes and we only need
    % to compare labels.
    rptg_get_edge_contents(!.Graph, Edge, _Start, End, EdgeContent),
    rptg_get_edge_contents(!.Graph, Ed, _S, E, EdC),
    ( if
        EdgeContent = EdC
    then
        % Unify the two end nodes.
        unify_operator(End, E, !.Graph, Graph1),

        % Apply rule 1 after the above unification.
        rule_1(End, Graph1, !:Graph),
        Happened = bool.yes
    else
        % Still not found an edge with the same label, continue the
        % inner loop.
        merge_nodes_reached_by_same_labelled_edges(Edge, Eds, Rest, !Graph,
            Happened)
    ).

    % Rule P2:
    % After an edge (N, Label, M) is added to a graph, it may happen
    % that there exists another edge from N with the same label but
    % pointing to a node different from M. This rule ensures that if that
    % the case the node will be unified with M.
    %
    % This predicate is called whenever a new edge has been added to the
    % graph. So when it is called there is at most one existing edge with
    % the same label to a different node. Because of that the predicate
    % need not be recursive.
    %
:- pred rule_2(rptg_node::in, rptg_node::in, cons_id::in, int::in,
    rpt_graph::in, rpt_graph::out) is det.

rule_2(Node1, Node2, ConsId, Component, !Graph) :-
    rptg_get_node_by_node(!.Graph, Node1, N),
    rptg_get_node_by_node(!.Graph, Node2, M),
    Sel = [termsel(ConsId, Component)],
    OutEdgeList = rptg_lookup_list_outedges(!.Graph, N),
    merge_nodes_reached_by_same_labelled_edge(Sel, M, OutEdgeList, !Graph).

    % If an edge (E) in OutEdgeList has the same label Sel then merge M
    % with the node (MPrime) that E points to.
    %
:- pred merge_nodes_reached_by_same_labelled_edge(selector::in,
    rptg_node::in, list(rptg_edge)::in, rpt_graph::in, rpt_graph::out) is det.

merge_nodes_reached_by_same_labelled_edge(_, _, [], !Graph).
merge_nodes_reached_by_same_labelled_edge(Sel, M, [Ed | Eds], !Graph) :-
    rptg_get_edge_contents(!.Graph, Ed, _, MPrime, EdgeContent),
    ( if
        EdgeContent = rptg_edge_content(Selector),
        Selector = Sel,
        MPrime \= M
    then
        unify_operator(M, MPrime, !Graph),
        rule_1(M, !Graph)
    else
        % Still not found an edge with the same label, continue the loop.
        merge_nodes_reached_by_same_labelled_edge(Sel, M, Eds, !Graph)
    ).

    % Rule P3:
    % This rule is applied after an edge is added TO the Node to enforce
    % the invariant that a subterm of the same type as the compounding
    % term is stored in the same region as the compounding term. In
    % the context of region points-to graph it means that there exists
    % a path between 2 nodes of the same type. In that case, this rule
    % will unify the 2 nodes.
    %
    % This algorithm may not be an efficient one because it checks all
    % the nodes in the graph one by one to see if a node can reach the
    % node or not.
    %
    % We enforce the invariant (in the sense that whenever the invariant
    % is made invalid this rule will correct it) therefore whenever we
    % find a satisfied node and unify it with Node we can stop. This is
    % indicated by Happened.
    %
:- pred rule_3(rptg_node::in, rpt_graph::in, rpt_graph::out) is det.

rule_3(Node, !Graph) :-
    NodeMap = rptg_get_nodes(!.Graph),
    map.keys(NodeMap, Nodes),
    (
        Nodes = [_N | _NS],
        % The graph has some node(s), so check each node to see if it
        % satisfies the condition of rule 3 or not, if yes unify it
        % with NY. (NY is the node that Node may be merged into.)
        rptg_get_node_by_node(!.Graph, Node, NY),
        rule_3_2(Nodes, NY, !Graph, Happened),

        % This predicate will quit when Happened = no, i.e. no more
        % nodes need to be unified.
        (
            Happened = bool.yes,
            % A node in Nodes has been unified with NY, so we start all
            % over again. Note that the node that has been unified has
            % been removed, so it will not be in the Graph1 in the below
            % call. So this predicate can terminate at some point (due
            % to the fact that the "size" of !.Graph is smaller than that
            % of !:Graph).
            disable_warning [suspicious_recursion] (
                rule_3(Node, !Graph)
            )
          ;
            % No node in Nodes has been unified with NY, which means that
            % no more nodes need to be unified, so just quit.
            Happened = bool.no
        )
    ;
        Nodes = [],
        % No node in the graph, impossible.
        unexpected($pred, "impossible having no node in graph")
    ).

    % Check each node in the list to see if it satisfies the condition of
    % rule 3 or not, i.e., link to another node with the same type.
    %   1. If the predicate finds out such a node, it unifies it with NY
    %   (also apply rule 1 here) and quit with Happend = 1.
    %   2. if no such a node found, it processes the rest of the list. The
    %   process continues like that until either 1. happens (the case above)
    %   or the list becomes empty and the predicate quits with Happened = 0.
    %
:- pred rule_3_2(list(rptg_node)::in, rptg_node::in, rpt_graph::in,
    rpt_graph::out, bool::out) is det.

rule_3_2([], _, !Graph, bool.no).
rule_3_2([NZ | NZs], NY, !Graph, Happened) :-
    ( if
        rule_3_condition(NZ, NY, !.Graph, NZ1)
    then
        unify_operator(NZ, NZ1, !Graph),

        % Apply rule 1.
        rule_1(NZ, !Graph),
        Happened = bool.yes
    else
        % Try with the rest, namely NS.
        rule_3_2(NZs, NY, !Graph, Happened)
    ).

:- pred rule_3_condition(rptg_node::in, rptg_node::in, rpt_graph::in,
    rptg_node::out) is semidet.

rule_3_condition(NZ, NY, Graph, NZ1) :-
    rptg_path(Graph, NZ, NY, _),
    rptg_lookup_node_type(Graph, NZ) = NZType,
    % A node reachable from NY, with the same type as NZ, the node can
    % be exactly NY.
    rptg_reachable_and_having_type(Graph, NY, NZType, NZ1),
    NZ \= NZ1.

%-----------------------------------------------------------------------------%
%
% Rule P4 and alpha mapping.
%

    % Build up the alpha mapping (node -> node) and apply rule P4
    % to ensure that it is actually a function.
    %
:- pred alpha_mapping_at_call_site(list(prog_var)::in, list(prog_var)::in,
    rpt_graph::in, rpt_graph::in, rpt_graph::out,
    rpt_call_alpha_mapping::in, rpt_call_alpha_mapping::out) is det.

alpha_mapping_at_call_site([], [], _, !CallerGraph, !AlphaMap).
alpha_mapping_at_call_site([], [_ | _], _, _, _, _, _) :-
    unexpected($pred, "mismatched lists").
alpha_mapping_at_call_site([_ | _], [], _, _, _, _, _) :-
    unexpected($pred, "mismatched lists").
alpha_mapping_at_call_site([Xi | Xs], [Yi | Ys], CalleeGraph,
        !CallerGraph, !AlphaMap) :-
    % Xi's are formal arguments, Yi's are actual arguments at the call site.
    rptg_get_node_by_variable(CalleeGraph, Xi, N_Xi),
    rptg_get_node_by_variable(!.CallerGraph, Yi, N_Yi),
    ( if map.search(!.AlphaMap, N_Xi, N_Y) then
        % alpha(N_Xi) = N_Y, alpha(N_Xi) = N_Yi, N_Y != N_Yi.
        %
        ( if N_Y = N_Yi then
            true
        else
            % Apply rule P4.
            unify_operator(N_Y, N_Yi, !CallerGraph),

            % Apply rule P1 after some nodes are unified.
            rule_1(N_Y, !CallerGraph)
        )
    else
        map.set(N_Xi, N_Yi, !AlphaMap),

        % If N_Xi's is_allocated then N_Yi is also allocated.
        % Otherwise leave N_Yi alone.
        ( if rptg_is_allocated_node(CalleeGraph, N_Xi) then
            rptg_set_node_is_allocated(N_Yi, bool.yes, !CallerGraph)
        else
           true
        )
    ),
    alpha_mapping_at_call_site(Xs, Ys, CalleeGraph, !CallerGraph, !AlphaMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Rules P5-P8 complete the alpha mapping at a call site and integrate the
% parts rooted at the formal parameters in the callee's graph into the
% caller's graph.
%
% The application of those rules happens at a call site, so related to a
% caller and a callee.
%
% We will start from the rooted nodes, follow each outcoming edge in the
% callee's graph exactly once and apply the rules.
%

:- pred apply_rules(list(rptg_node)::in, program_point::in,
    list(rptg_node)::in, rpta_info::in, rpta_info::in,
    rpta_info::out) is det.

apply_rules([], _, _, _, !CallerRptaInfo).
apply_rules([CalleeNode | CalleeNodes0], CallSite, Processed, CalleeRptaInfo,
        !CallerRptaInfo) :-
    % The caller node corresponding to the callee node at this call site.
    !.CallerRptaInfo = rpta_info(_, CallerAlphaMapping0),
    map.lookup(CallerAlphaMapping0, CallSite, AlphaAtCallSite),
    map.lookup(AlphaAtCallSite, CalleeNode, CallerNode),

    % Follow CalleeNode and apply rules when traversing its edges.
    apply_rules_node(CallSite, CalleeNode, CalleeRptaInfo, CallerNode,
        !CallerRptaInfo),

    % Continue with the nodes reached from Callee Node.
    CalleeRptaInfo = rpta_info(CalleeGraph, _),
    SuccessorsCalleeNode = rptg_successors(CalleeGraph, CalleeNode),
    set.to_sorted_list(SuccessorsCalleeNode, SsList),
    list.delete_elems(SsList, Processed, ToBeProcessed),
    CalleeNodes = ToBeProcessed ++ CalleeNodes0,
    apply_rules(CalleeNodes, CallSite, [CalleeNode | Processed],
        CalleeRptaInfo, !CallerRptaInfo).

:- pred apply_rules_node(program_point::in, rptg_node::in, rpta_info::in,
    rptg_node::in, rpta_info::in, rpta_info::out) is det.

apply_rules_node(CallSite, CalleeNode, CalleeRptaInfo, CallerNode,
        !CallerRptaInfo) :-
    CalleeRptaInfo = rpta_info(CalleeGraph, _),

    % Apply rules P5-P8 for each out-edge of CalleeNode.
    CalleeNodeOutEdges = rptg_lookup_list_outedges(CalleeGraph, CalleeNode),
    apply_rules_outedges(CalleeNodeOutEdges, CallerNode, CallSite,
        CalleeRptaInfo, !CallerRptaInfo).

:- pred apply_rules_outedges(list(rptg_edge)::in, rptg_node::in,
    program_point::in, rpta_info::in, rpta_info::in, rpta_info::out) is det.

apply_rules_outedges([], _, _, _, !RptaInfoR).
apply_rules_outedges([Edge | Edges], CallerNode, CallSite, CalleeRptaInfo,
        !CallerRptaInfo) :-
    rule_5(Edge, CallSite, CalleeRptaInfo, CallerNode, !CallerRptaInfo),
    rule_6(Edge, CallSite, CalleeRptaInfo, CallerNode, !CallerRptaInfo),
    rule_7(Edge, CallSite, CalleeRptaInfo, CallerNode, !CallerRptaInfo),
    rule_8(Edge, CallSite, CalleeRptaInfo, CallerNode, !CallerRptaInfo),
    apply_rules_outedges(Edges, CallerNode, CallSite, CalleeRptaInfo,
        !CallerRptaInfo).

:- pred rule_5(rptg_edge::in, program_point::in, rpta_info::in,
    rptg_node::in, rpta_info::in, rpta_info::out) is det.

rule_5(Edge, CallSite, CalleeRptaInfo, CallerNode,
        rpta_info(!.CallerGraph, CallerAlphaMapping),
        rpta_info(!:CallerGraph, CallerAlphaMapping)) :-
    % Find an out-edge in the caller's graph that has a same label
    % the label of the out-edge in callee's graph.
    CalleeRptaInfo = rpta_info(CalleeGraph, _),
    rptg_get_edge_contents(CalleeGraph, Edge, _CalleeNode, CalleeM, Label),
    %!.CallerRptaInfo = rpta_info(CallerGraph0, CallerAlphaMapping),
    rptg_get_node_by_node(!.CallerGraph, CallerNode, RealCallerNode),
    ( if
        rptg_find_edge_from_node_with_same_content(RealCallerNode, Label,
            !.CallerGraph, CallerMPrime),
        map.search(CallerAlphaMapping, CallSite, AlphaAtCallSite),
        map.search(AlphaAtCallSite, CalleeM, CallerM),
        rptg_get_node_by_node(!.CallerGraph, CallerM, RealCallerM),
        CallerMPrime \= RealCallerM
    then
        % When the conditions of rule P5 are satisfied, nodes are unified and
        % rule P1 applied to ensure invariants.
        unify_operator(RealCallerM, CallerMPrime, !CallerGraph),

        % Apply rule P1 after the unification.
        rule_1(RealCallerM, !CallerGraph)
    else
        true
    ).

:- pred rule_6(rptg_edge::in, program_point::in, rpta_info::in,
    rptg_node::in, rpta_info::in, rpta_info::out) is det.

rule_6(Edge, CallSite, CalleeRptaInfo, CallerNode,
        rpta_info(!.CallerGraph, !.CallerAlphaMapping),
        rpta_info(!:CallerGraph, !:CallerAlphaMapping)) :-
    % Find an out-edge in the caller's graph that has a same label
    % the label of the out-edge in callee's graph.
    CalleeRptaInfo = rpta_info(CalleeGraph, _),
    rptg_get_edge_contents(CalleeGraph, Edge, _CalleeNode, CalleeM, Label),
    rptg_get_node_by_node(!.CallerGraph, CallerNode, RealCallerNode),
    ( if
        rptg_find_edge_from_node_with_same_content(RealCallerNode, Label,
            !.CallerGraph, CallerM)
    then
        % (CallerNode, sel, CallerM) in the graph.
        map.lookup(!.CallerAlphaMapping, CallSite, AlphaAtCallSite0),
        ( if map.search(AlphaAtCallSite0, CalleeM, _) then
            % alpha(CalleeM) = CallerM so ignore.
            true
        else
            % Apply rule P6: when its conditions are satisfied
            % record alpha(CalleeM) = CallerM.
            map.set(CalleeM, CallerM, AlphaAtCallSite0, AlphaAtCallSite1),
            map.set(CallSite, AlphaAtCallSite1, !CallerAlphaMapping),

            % If CalleeM's is_allocated then CallerM is also allocated.
            % Otherwise leave CallerM alone.
            ( if rptg_is_allocated_node(CalleeGraph, CalleeM) then
                rptg_set_node_is_allocated(CallerM, bool.yes, !CallerGraph)
            else
                true
            )
        )
    else
        true
    ).

:- pred rule_7(rptg_edge::in, program_point::in, rpta_info::in,
    rptg_node::in, rpta_info::in, rpta_info::out) is det.

rule_7(Edge, CallSite, CalleeRptaInfo, CallerNode,
        rpta_info(!.CallerGraph, CallerAlphaMapping),
        rpta_info(!:CallerGraph, CallerAlphaMapping)) :-
    % Find an out-edge in the caller's graph that has a same label
    % the label of the out-edge in callee's graph.
    CalleeRptaInfo = rpta_info(CalleeGraph, _),
    rptg_get_edge_contents(CalleeGraph, Edge, _CalleeNode, CalleeM, Label),
    rptg_get_node_by_node(!.CallerGraph, CallerNode, RealCallerNode),
    ( if
        rptg_find_edge_from_node_with_same_content(RealCallerNode, Label,
            !.CallerGraph, _)
    then
        true
    else
        % No edge from CallerNode with the label exists.
        ( if
            map.lookup(CallerAlphaMapping, CallSite, AlphaAtCallSite),
            map.search(AlphaAtCallSite, CalleeM, CallerM)
        then
            % Reach here means all the conditions of rule P7 are satisfied,
            % add (CallerNode, sel, CallerM).
            rptg_get_node_by_node(!.CallerGraph, CallerM, RealCallerM),
            edge_operator(RealCallerNode, RealCallerM, Label, !CallerGraph),

            % Need to apply rule 3.
            rule_3(RealCallerM, !CallerGraph)
        else
            true
        )
    ).

:- pred rule_8(rptg_edge::in, program_point::in, rpta_info::in,
    rptg_node::in, rpta_info::in, rpta_info::out) is det.

rule_8(Edge, CallSite, CalleeRptaInfo, CallerNode,
        rpta_info(!.CallerGraph, !.CallerAlphaMapping),
        rpta_info(!:CallerGraph, !:CallerAlphaMapping)) :-
    % Find an out-edge in the caller's graph that has a same label
    % the label of the out-edge in callee's graph.
    CalleeRptaInfo = rpta_info(CalleeGraph, _),
    rptg_get_edge_contents(CalleeGraph, Edge, _CalleeNode, CalleeM, Label),
    rptg_get_node_by_node(!.CallerGraph, CallerNode, RealCallerNode),
    ( if
        rptg_find_edge_from_node_with_same_content(RealCallerNode, Label,
            !.CallerGraph, _)
    then
        true
    else
        % No edge from CallerNode with the label exists.
        ( if
            map.lookup(!.CallerAlphaMapping, CallSite, AlphaAtCallSite0),
            map.search(AlphaAtCallSite0, CalleeM, _)
        then
            true
        else
            % rule 8: add node CallerM, alpha(CalleeM) = CallerM,
            % edge(CallerNode, sel, CallerM)
            %
            CallerNextNodeId = rptg_get_next_node_id(!.CallerGraph),
            string.append("R", string.int_to_string(CallerNextNodeId),
                RegName),
            CallerMContent = rptg_node_content(set.init, RegName, set.init,
                rptg_lookup_node_type(CalleeGraph, CalleeM),
                rptg_lookup_node_is_allocated(CalleeGraph, CalleeM)),
            rptg_add_node(CallerMContent, CallerM, !CallerGraph),
            edge_operator(RealCallerNode, CallerM, Label, !CallerGraph),

            map.lookup(!.CallerAlphaMapping, CallSite, AlphaAtCallSite0),
            map.set(CalleeM, CallerM, AlphaAtCallSite0, AlphaAtCallSite),
            map.set(CallSite, AlphaAtCallSite, !CallerAlphaMapping),

            rule_3(CallerM, !CallerGraph)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Fixpoint table used in region points-to analysis.
%

    % The fixpoint table used by the region points-to analysis.
    %
:- type rpta_fixpoint_table == fixpoint_table(pred_proc_id, rpta_info).

    % Initialise the fixpoint table for the given set of pred_proc_ids.
    %
:- func init_rpta_fixpoint_table(list(pred_proc_id), rpta_info_table)
    = rpta_fixpoint_table.

init_rpta_fixpoint_table(Keys, InfoTable) = Table :-
    Table = init_fixpoint_table(wrapped_init(InfoTable), Keys).

    % Enter the newly computed region points-to information for a given
    % procedure.
    % If the description is different from the one that was already stored
    % for that procedure, the stability of the fixpoint table is set to
    % "unstable".
    % Aborts if the procedure is not already in the fixpoint table.
    %
:- pred rpta_fixpoint_table_new_rpta_info(
    pred_proc_id::in, rpta_info::in,
    rpta_fixpoint_table::in, rpta_fixpoint_table::out) is det.

rpta_fixpoint_table_new_rpta_info(PPId, RptaInfo, !Table) :-
    EqualityTest =
        ( pred(TabledElem::in, Elem::in) is semidet :-
            rpta_info_equal(Elem, TabledElem)
        ),
    add_to_fixpoint_table(EqualityTest, PPId, RptaInfo, !Table).

:- func wrapped_init(rpta_info_table, pred_proc_id) = rpta_info.

wrapped_init(InfoTable, PPId) = Entry :-
    ( if Entry0 = rpta_info_table_search_rpta_info(PPId, InfoTable) then
        Entry = Entry0
    else
        % The information we are looking for should be there after the
        % intraprocedural analysis.
        unexpected($pred, "no rpta_info")
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.points_to_analysis.
%-----------------------------------------------------------------------------%
