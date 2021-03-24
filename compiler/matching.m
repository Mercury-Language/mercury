%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: matching.m.
% Authors: pjs, zs.
%
% Module `matching' - performs bipartite graph maximal matching computation
% specialized for the stack slot optimization. The structure of the graph
% on which the algorithm operates is documented in the paper "Using the heap
% to eliminate stack accesses" by Zoltan Somogyi and Peter Stuckey.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.matching.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % This structure stores the adjustable parameters of the matching
    % operation.
    %
    % The first four fields give the relative costs of the four kinds
    % of operations this optimization concerns itself with.
    %
    % one_path_op_ratio and one_path_op_ratio are tuning parameters;
    % find_via_cell_vars will say that the optimization is inapplicable
    % (i.e. that none of the candidates should be accessed via the cell)
    % unless the ratios of benefit operations to cost operations and
    % benefit nodes to cost nodes are at or above the percentage
    % thresholds specified by these two fields respectively.
    %
    % The include_all_candidates field says whether this thresholding
    % operation is to count the benefits obtainable from the candidate
    % variables that do not happen to be accessed in the AfterFlush
    % argument of find_via_cell_vars.

:- type matching_params
    --->    matching_params(
                cell_var_store_cost     :: int,
                cell_var_load_cost      :: int,
                field_var_store_cost    :: int,
                field_var_load_cost     :: int,
                one_path_op_ratio       :: int,
                one_path_node_ratio     :: int,
                include_all_candidates  :: maybe_opt_svcell_all_candidates
            ).

:- type benefit_node.
:- type cost_node.

    % find_via_cell_vars(Globals, ModuleName, MatchingParams,
    %   CellVar, CandidateFieldVars, CellVarFlushedLater,
    %   BeforeFlush, AfterFlush,
    %   RealizedBenefitNodes, RealizedCostNodes, ViaCellVars):
    %
    % CellVar gives a variable that corresponds to a memory cell, while
    % CandidateArgVars gives a subset of the variables that are the fields
    % of that cell. BeforeFlush gives the set of variables the program
    % accesses in the segment before the first stack flush, while each
    % element of AfterFlush corresponds to a segment, and gives the set
    % of variables accessed in that segment.
    %
    % The output ViaCellVars, gives the subset of CandidateArgVars that
    % should be accesed via CellVar. The outputs RealizedBenefitNodes
    % and RealizedCostNodes give the benefit and cost nodes realized
    % by this choice.
    %
    % The Globals and ModuleName arguments are needed only so that
    % we can get the right debug output stream.
    %
:- pred find_via_cell_vars(globals::in, module_name::in, matching_params::in,
    prog_var::in, set_of_progvar::in, bool::in, set_of_progvar::in,
    list(set_of_progvar)::in,
    set(benefit_node)::out, set(cost_node)::out, set_of_progvar::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module string.
:- import_module term.

    % The stack optimization graph is a bipartite graph, whose two node types
    % are cost nodes and benefit nodes. Each node represents a copy of an
    % operation, a load or a store. We have LoadCost copies of each load
    % operation and StoreCost copies of each store operation, where LoadCost
    % and StoreCost are parameters of find_via_cell_vars.
    %
    % We represent the stack optimization graph in the usual manner:
    % as two maps, with each map one kind of node to the set of nodes
    % of the other types to which it is adjacent.
:- type stack_slot_graph
    --->    stack_slot_graph(
                map(cost_node, set(benefit_node)),
                map(benefit_node, set(cost_node))
            ).

:- type cost_operation
    --->    cell_var_load(int)      % segment number, >= 2
    ;       cell_var_store.         % in initial segment

:- type benefit_operation
    --->    field_var_load(prog_var)    % in initial segment
    ;       field_var_store(prog_var).  % in initial segment

% The integers differentiate the different copies of an operation.
:- type cost_node --->      cost_node(cost_operation, int).
:- type benefit_node --->   benefit_node(benefit_operation, int).

    % The field_costs_benefits structure records, for a given field variable,
    % the nodes of the cost we incur and the benefits we gain if we access that
    % field variable via the cell instead of via the stack.
:- type field_costs_benefits
    --->    field_costs_benefits(
                prog_var,
                set(cost_node),
                set(benefit_node)
            ).

    % Matchings are sets of edges, in which each node in the graph can occur at
    % most once. We represent the matching by mapping each node that is an
    % endpoint of an edge in the matching to the node at the other end of the
    % edge. If a node is not in the matching, it will not occur in the relevant
    % map.
:- type matching
    --->    matching(
                map(cost_node, benefit_node),
                map(benefit_node, cost_node)
            ).

%-----------------------------------------------------------------------------%

find_via_cell_vars(Globals, ModuleName, MatchingParams, CellVar,
        CandidateFieldVars, CellVarFlushedLater, BeforeFlush, AfterFlush,
        RealizedBenefitNodes, RealizedCostNodes, ViaCellVars) :-
    InclAllCand = MatchingParams ^ include_all_candidates,
    (
        InclAllCand = do_not_opt_svcell_all_candidates,
        AllSegmentVars = set_of_var.union_list([BeforeFlush | AfterFlush]),
        set_of_var.intersect(CandidateFieldVars, AllSegmentVars,
            OccurringCandidateFieldVars),
        set_of_var.difference(CandidateFieldVars, OccurringCandidateFieldVars,
            NonOccurringCandidateFieldVars)
    ;
        InclAllCand = opt_svcell_all_candidates,
        OccurringCandidateFieldVars = CandidateFieldVars,
        NonOccurringCandidateFieldVars = set_of_var.init
    ),
    OccurringCandidateFieldVarList =
        set_of_var.to_sorted_list(OccurringCandidateFieldVars),
    list.filter_map(simplify_segment(CellVar, OccurringCandidateFieldVars),
        AfterFlush, FilteredAfterFlush),
    NumberedAfterFlush = number_segments(2, FilteredAfterFlush),
    CostsBenefits = list.map(
        find_costs_benefits(CellVar, BeforeFlush, NumberedAfterFlush,
            CellVarFlushedLater, MatchingParams),
        OccurringCandidateFieldVarList),
    list.foldl(gather_benefits, CostsBenefits, set.init, BenefitNodes),
    list.foldl(gather_costs, CostsBenefits, set.init, CostNodes),
    set.to_sorted_list(BenefitNodes, BenefitNodeList),
    set.to_sorted_list(CostNodes, CostNodeList),
    Graph = create_graph(CostsBenefits),
    MaximalMatching = maximal_matching(BenefitNodeList, Graph),
    MaximalMatching = matching(MaximalMatchingCostToBenefit, _),
    UnMatchedCostNodes = get_unmatched_cost_nodes(CostNodeList,
        MaximalMatchingCostToBenefit),
    MarkedBenefitNodes = reachable_by_alternating_path(UnMatchedCostNodes,
        Graph, MaximalMatching),
    ViaCellOccurringVars0 =
        compute_via_cell_vars(CostsBenefits, MarkedBenefitNodes),
    list.filter(realized_costs_benefits(ViaCellOccurringVars0),
        CostsBenefits, RealizedCostsBenefits),
    list.foldl(gather_benefits, RealizedCostsBenefits,
        set.init, RealizedBenefitNodes),
    list.foldl(gather_costs, RealizedCostsBenefits,
        set.init, RealizedCostNodes),
    RealizedBenefitOps = set.map(project_benefit_op, RealizedBenefitNodes),
    RealizedCostOps = set.map(project_cost_op, RealizedCostNodes),
    set.to_sorted_list(RealizedBenefitNodes, RealizedBenefitNodeList),
    set.to_sorted_list(RealizedCostNodes, RealizedCostNodeList),
    set.to_sorted_list(RealizedBenefitOps, RealizedBenefitOpList),
    set.to_sorted_list(RealizedCostOps, RealizedCostOpList),
    list.length(RealizedBenefitNodeList, RealizedBenefitNodeCount),
    list.length(RealizedBenefitOpList, RealizedBenefitOpCount),
    list.length(RealizedCostNodeList, RealizedCostNodeCount),
    list.length(RealizedCostOpList, RealizedCostOpCount),
    OpRatio = MatchingParams ^ one_path_op_ratio,
    NodeRatio = MatchingParams ^ one_path_node_ratio,
    ( if
        RealizedBenefitOpCount * 100 >= RealizedCostOpCount * OpRatio,
        RealizedBenefitNodeCount * 100 >= RealizedCostNodeCount * NodeRatio
    then
        ViaCellOccurringVars = ViaCellOccurringVars0,
        Nullified = no
    else
        ViaCellOccurringVars = set_of_var.init,
        Nullified = yes
    ),
    ViaCellVars = set_of_var.union(ViaCellOccurringVars,
        NonOccurringCandidateFieldVars),
    % Enable if you want to dump performance information into the .err file.
    trace [compile_time(flag("debug_matching")), io(!IO)] (
        get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
        dump_results(DebugStream, CellVar, CandidateFieldVars,
            OccurringCandidateFieldVarList, ViaCellOccurringVars0,
            Nullified, BeforeFlush, NumberedAfterFlush,
            RealizedBenefitNodeList, RealizedBenefitOpList,
            RealizedCostNodeList, RealizedCostOpList, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Simplify_segment fails if the CellVar is in the SegmentVars since the
    % cost of executing such segments doesn't depend on whether we access
    % field vars via the cell var or via the stack. If CellVar is not in
    % SegmentVars, them simplify_segment succeeds after removing the
    % non-candidate variables from SegmentVars0.
    %
:- pred simplify_segment(prog_var::in, set_of_progvar::in, set_of_progvar::in,
    set_of_progvar::out) is semidet.

simplify_segment(CellVar, CandidateArgVars, SegmentVars0, SegmentVars) :-
    not set_of_var.member(SegmentVars0, CellVar),
    SegmentVars = set_of_var.intersect(SegmentVars0, CandidateArgVars).

:- func number_segments(int, list(set_of_progvar)) =
    assoc_list(int, set_of_progvar).

number_segments(_N, []) = [].
number_segments(N, [Segment | Segments]) =
    [N - Segment | number_segments(N + 1, Segments)].

%-----------------------------------------------------------------------------%

    % Find_costs_benefits computes the costs and benefits of accessing the
    % given field variable FieldVar via the cell variable CellVar.
    %
:- func find_costs_benefits(prog_var, set_of_progvar,
    assoc_list(int, set_of_progvar), bool, matching_params, prog_var)
    = field_costs_benefits.

find_costs_benefits(CellVar, BeforeFlush, AfterFlush, CellVarFlushedLater,
        MatchingParams, FieldVar) = FieldCostsBenefits :-
    find_cell_var_loads_for_field(AfterFlush, FieldVar, [], CostOps0),
    (
        CellVarFlushedLater = yes,
        CostOps = CostOps0
    ;
        CellVarFlushedLater = no,
        CostOps = [cell_var_store | CostOps0]
    ),
    BenefitOps0 = [field_var_store(FieldVar)],
    ( if set_of_var.member(BeforeFlush, CellVar) then
        BenefitOps = BenefitOps0
    else
        BenefitOps = [field_var_load(FieldVar) | BenefitOps0]
    ),

    CellVarStoreCost = MatchingParams ^ cell_var_store_cost,
    CellVarLoadCost = MatchingParams ^ cell_var_load_cost,
    CostNodeLists = list.map(
        replicate_cost_op(CellVarStoreCost, CellVarLoadCost),
        CostOps),
    list.condense(CostNodeLists, CostNodes),
    set.list_to_set(CostNodes, CostNodeSet),
    FieldVarStoreCost = MatchingParams ^ field_var_store_cost,
    FieldVarLoadCost = MatchingParams ^ field_var_load_cost,
    BenefitNodeLists = list.map(
        replicate_benefit_op(FieldVarStoreCost, FieldVarLoadCost),
        BenefitOps),
    list.condense(BenefitNodeLists, BenefitNodes),
    set.list_to_set(BenefitNodes, BenefitNodeSet),
    FieldCostsBenefits = field_costs_benefits(FieldVar,
        CostNodeSet, BenefitNodeSet).

:- pred find_cell_var_loads_for_field(assoc_list(int, set_of_progvar)::in,
    prog_var::in, list(cost_operation)::in, list(cost_operation)::out) is det.

find_cell_var_loads_for_field([], _, !CostOps).
find_cell_var_loads_for_field([SegmentNum - SegmentVars | AfterFlush],
        FieldVar, !CostOps) :-
    ( if set_of_var.member(SegmentVars, FieldVar) then
        !:CostOps = [cell_var_load(SegmentNum) | !.CostOps]
    else
        true
    ),
    find_cell_var_loads_for_field(AfterFlush, FieldVar, !CostOps).

%-----------------------------------------------------------------------------%

:- func replicate_cost_op(int, int, cost_operation) = list(cost_node).

replicate_cost_op(_StoreCost, LoadCost, cell_var_load(Segment)) =
    make_cost_op_copies(LoadCost, cell_var_load(Segment)).
replicate_cost_op(StoreCost, _LoadCost, cell_var_store) =
    make_cost_op_copies(StoreCost, cell_var_store).

:- func make_cost_op_copies(int, cost_operation) = list(cost_node).

make_cost_op_copies(Cur, Op) =
    ( if Cur > 0 then
        [cost_node(Op, Cur) | make_cost_op_copies(Cur - 1, Op)]
    else
        []
    ).

:- func replicate_benefit_op(int, int, benefit_operation) = list(benefit_node).

replicate_benefit_op(_StoreCost, LoadCost, field_var_load(FieldVar)) =
    make_benefit_op_copies(LoadCost, field_var_load(FieldVar)).
replicate_benefit_op(StoreCost, _LoadCost, field_var_store(FieldVar)) =
    make_benefit_op_copies(StoreCost, field_var_store(FieldVar)).

:- func make_benefit_op_copies(int, benefit_operation) = list(benefit_node).

make_benefit_op_copies(Cur, Op) =
    ( if Cur > 0 then
        [benefit_node(Op, Cur) | make_benefit_op_copies(Cur - 1, Op)]
    else
        []
    ).

%-----------------------------------------------------------------------------%

    % Accumulate all the benefit nodes.
    %
:- pred gather_benefits(field_costs_benefits::in, set(benefit_node)::in,
    set(benefit_node)::out) is det.

gather_benefits(field_costs_benefits(_, _, FieldBenefits), !Benefits) :-
    set.union(FieldBenefits, !Benefits).

    % Accumulate all the cost nodes.
    %
:- pred gather_costs(field_costs_benefits::in, set(cost_node)::in,
    set(cost_node)::out) is det.

gather_costs(field_costs_benefits(_, FieldCosts, _), !Costs) :-
    set.union(FieldCosts, !Costs).

%-----------------------------------------------------------------------------%

    % Create the stack slot optimization graph described in the paper.
    %
:- func create_graph(list(field_costs_benefits)) = stack_slot_graph.

create_graph(CostsBenefits) = Graph :-
    list.foldl2(create_graph_links, CostsBenefits,
        map.init, CostToBenefitsMap, map.init, BenefitToCostsMap),
    Graph = stack_slot_graph(CostToBenefitsMap, BenefitToCostsMap).

:- pred create_graph_links(field_costs_benefits::in,
    map(cost_node, set(benefit_node))::in,
    map(cost_node, set(benefit_node))::out,
    map(benefit_node, set(cost_node))::in,
    map(benefit_node, set(cost_node))::out) is det.

create_graph_links(field_costs_benefits(_FieldVar, Costs, Benefits),
        !CostToBenefitsMap, !BenefitToCostsMap) :-
    list.foldl(add_cost_benefit_links(Benefits),
        set.to_sorted_list(Costs), !CostToBenefitsMap),
    list.foldl(add_benefit_cost_links(Costs),
        set.to_sorted_list(Benefits), !BenefitToCostsMap).

:- pred add_cost_benefit_links(set(benefit_node)::in, cost_node::in,
    map(cost_node, set(benefit_node))::in,
    map(cost_node, set(benefit_node))::out) is det.

add_cost_benefit_links(Benefits, Cost, !CostToBenefitsMap) :-
    ( if map.search(!.CostToBenefitsMap, Cost, CostBenefits0) then
        set.union(CostBenefits0, Benefits, CostBenefits),
        map.det_update(Cost, CostBenefits, !CostToBenefitsMap)
    else
        map.det_insert(Cost, Benefits, !CostToBenefitsMap)
    ).

:- pred add_benefit_cost_links(set(cost_node)::in, benefit_node::in,
    map(benefit_node, set(cost_node))::in,
    map(benefit_node, set(cost_node))::out) is det.

add_benefit_cost_links(Costs, Benefit, !BenefitToCostsMap) :-
    ( if map.search(!.BenefitToCostsMap, Benefit, BenefitCosts0) then
        set.union(BenefitCosts0, Costs, BenefitCosts),
        map.det_update(Benefit, BenefitCosts, !BenefitToCostsMap)
    else
        map.det_insert(Benefit, Costs, !BenefitToCostsMap)
    ).

%-----------------------------------------------------------------------------%

    % Find a maximal matching in the given graph.
    %
:- func maximal_matching(list(benefit_node), stack_slot_graph) = matching.

maximal_matching(BenefitNodes, Graph) = Matching :-
    Matching0 = matching(map.init, map.init),
    maximize_matching(BenefitNodes, Graph, Matching0, Matching).

:- pred maximize_matching(list(benefit_node)::in, stack_slot_graph::in,
    matching::in, matching::out) is det.

maximize_matching(BenefitNodes, Graph, !Matching) :-
    ( if Path = find_augmenting_path(BenefitNodes, Graph, !.Matching) then
        !:Matching = update_matches(Path, !.Matching),
        disable_warning [suspicious_recursion] (
            maximize_matching(BenefitNodes, Graph, !Matching)
        )
    else
        true
    ).

:- func update_matches(edge_list, matching) = matching.

update_matches([], Matching0) = Matching0.
update_matches([BenefitNode - CostNode | Path], Matching0) = Matching :-
    Matching0 = matching(CostToBenefitMap0, BenefitToCostMap0),
    map.set(CostNode, BenefitNode, CostToBenefitMap0, CostToBenefitMap1),
    map.set(BenefitNode, CostNode, BenefitToCostMap0, BenefitToCostMap1),
    Matching1 = matching(CostToBenefitMap1, BenefitToCostMap1),
    Matching = update_matches(Path, Matching1).

:- func find_augmenting_path(list(benefit_node), stack_slot_graph, matching)
    = edge_list is semidet.

find_augmenting_path(BenefitNodes, Graph, Matching) = Path :-
    Matching = matching(_, MatchingBenefitToCost),
    UnMatchedBenefitNodes = get_unmatched_benefit_nodes(BenefitNodes,
        MatchingBenefitToCost),
    Path = find_first_path_bf(UnMatchedBenefitNodes, Graph, Matching).

%-----------------------------------------------------------------------------%

:- type edge_list == assoc_list(benefit_node, cost_node).

:- type benefit_node_and_edge_list
    --->    benefit_node_and_edge_list(
                benefit_node,
                edge_list
            ).

    % Breadth-first search for an augmenting path.

    % Build an initial queue of all the unmatched benefit nodes, with empty
    % paths. Take the first element of the queue and see what nodes are
    % reachable from there. If one is unmatched return the path, otherwise add
    % these nodes to the queue if they haven't been visited before.
    %
:- func find_first_path_bf(list(benefit_node), stack_slot_graph, matching)
    = edge_list is semidet.

find_first_path_bf(BenefitNodes, Graph, Matching) = Path :-
    Queue = initial_queue(BenefitNodes, queue.init),
    Path = augpath_bf(Queue, BenefitNodes, Graph, Matching).

:- func initial_queue(list(benefit_node), queue(benefit_node_and_edge_list))
    = queue(benefit_node_and_edge_list).

initial_queue([], Q) = Q.
initial_queue([N | Ns], Q0) = Q :-
    Q1 = queue.put(Q0, benefit_node_and_edge_list(N, [])),
    Q = initial_queue(Ns, Q1).

:- func augpath_bf(queue(benefit_node_and_edge_list), list(benefit_node),
    stack_slot_graph, matching) = edge_list is semidet.

augpath_bf(Queue0, Seen0, Graph, Matching) = Path :-
    queue.get(NodePath, Queue0, Queue1),
    NodePath = benefit_node_and_edge_list(BenefitNode, Path0),
    Graph = stack_slot_graph(_, BenefitToCostsMap),
    map.lookup(BenefitToCostsMap, BenefitNode, AdjCostNodes),
    Matching = matching(CostToBenefitMap, _),
    CostMatches = map_adjs_to_matched_cost(
        set.to_sorted_list(AdjCostNodes), CostToBenefitMap),
    ( if find_unmatched_cost(CostMatches) = UnmatchedCostNode then
        Path = [BenefitNode - UnmatchedCostNode | Path0]
    else
        add_alternates(CostMatches, Seen0, Seen, BenefitNode, Path0,
            Queue1, Queue2),
        Path = augpath_bf(Queue2, Seen, Graph, Matching)
    ).

:- func find_unmatched_cost(assoc_list(cost_node, maybe(benefit_node)))
    = cost_node is semidet.

find_unmatched_cost([CostNode - MaybeBenefitNode | Matches]) = Unmatched :-
    (
        MaybeBenefitNode = no,
        Unmatched = CostNode
    ;
        MaybeBenefitNode = yes(_),
        Unmatched = find_unmatched_cost(Matches)
    ).

    % For each node CostNode adjacent to BenefitNode, we have determined
    % whether they are matched (that information is in MaybeAdjBenefitNode).
    % If AdjBenefitNode has not been visited before (it is not in Seen0),
    % we add it to the queue with the path extended by the last arc
    % (BenefitNode - CostNode).
    %
:- pred add_alternates(assoc_list(cost_node, maybe(benefit_node))::in,
    list(benefit_node)::in, list(benefit_node)::out,
    benefit_node::in, edge_list::in,
    queue(benefit_node_and_edge_list)::in,
    queue(benefit_node_and_edge_list)::out) is det.

add_alternates([], !Seen, _, _, !Queue).
add_alternates([CostMatch | CostMatches], !Seen, BenefitNode, Path, !Queue) :-
    CostMatch = CostNode - MaybeAdjBenefitNode,
    ( if
        MaybeAdjBenefitNode = yes(AdjBenefitNode),
        not list.member(AdjBenefitNode, !.Seen)
    then
        !:Seen = [AdjBenefitNode | !.Seen],
        NewPath = [BenefitNode - CostNode | Path],
        BenefitNodeAndEdgeList =
            benefit_node_and_edge_list(AdjBenefitNode, NewPath),
        queue.put(BenefitNodeAndEdgeList, !Queue)
    else
        true
    ),
    add_alternates(CostMatches, !Seen, BenefitNode, Path, !Queue).

%-----------------------------------------------------------------------------%

    % Find all the benefit nodes reachable from the cost nodes in the first
    % argument via alternating paths. The SelectedCostNodes are not matched,
    % so first we look for matched benefit nodes adjacent to them, since those
    % nodes are reachable. We then look at the cost nodes matched to those
    % benefit nodes, since the benefit nodes reachable from there are also
    % reachable from the original cost nodes.
    %
    % To ensure termination, we follow the matched link from a benefit node
    % only when that benefit node is first put into the reachable set.
    %
:- func reachable_by_alternating_path(list(cost_node), stack_slot_graph,
    matching) = set(benefit_node).

reachable_by_alternating_path(SelectedCostNodes, Graph, Matching)
        = ReachableBenefitNodes :-
    reachable_by_alternating_path(SelectedCostNodes, Graph, Matching,
        set.init, ReachableBenefitNodes).

:- pred reachable_by_alternating_path(list(cost_node)::in,
    stack_slot_graph::in, matching::in, set(benefit_node)::in,
    set(benefit_node)::out) is det.

reachable_by_alternating_path(SelectedCostNodes, Graph, Matching,
        !BenefitNodes) :-
    (
        SelectedCostNodes = []
    ;
        SelectedCostNodes = [_ | _],
        Graph = stack_slot_graph(CostToBenefitsMap, _),
        list.foldl(adjacents(CostToBenefitsMap), SelectedCostNodes,
            set.init, AdjBenefitNodes),
        set.difference(!.BenefitNodes, AdjBenefitNodes, NewBenefitNodes),
        set.union(AdjBenefitNodes, !BenefitNodes),
        set.to_sorted_list(NewBenefitNodes, NewBenefitNodeList),
        Matching = matching(_, BenefitToCostMap),
        LinkedCostNodes = list.map(map.lookup(BenefitToCostMap),
            NewBenefitNodeList),
        reachable_by_alternating_path(LinkedCostNodes, Graph, Matching,
            !BenefitNodes)
    ).

:- pred adjacents(map(cost_node, set(benefit_node))::in, cost_node::in,
    set(benefit_node)::in, set(benefit_node)::out) is det.

adjacents(CostToBenefitsMap, CostNode, !BenefitNodes) :-
    map.lookup(CostToBenefitsMap, CostNode, AdjBenefitNodes),
    set.union(AdjBenefitNodes, !BenefitNodes).

%-----------------------------------------------------------------------------%

    % Given a list of cost nodes adjacent to a benefit node, find out
    % for each of those cost nodes whether it is linked to a benefit node
    % by the given matching, and if yes, to which one.
    %
:- func map_adjs_to_matched_cost(list(cost_node), map(cost_node, benefit_node))
    = assoc_list(cost_node, maybe(benefit_node)).

map_adjs_to_matched_cost(AdjCostNodes, CostToBenefitMap) = CostMatches :-
    CostMatches = list.map(adj_to_matched_cost(CostToBenefitMap),
        AdjCostNodes).

:- func adj_to_matched_cost(map(cost_node, benefit_node), cost_node) =
    pair(cost_node, maybe(benefit_node)).

adj_to_matched_cost(CostToBenefitMap, CostNode) = Match :-
    ( if map.search(CostToBenefitMap, CostNode, BenefitNode) then
        Match = CostNode - yes(BenefitNode)
    else
        Match = CostNode - no
    ).

%-----------------------------------------------------------------------------%

:- func compute_via_cell_vars(list(field_costs_benefits), set(benefit_node))
    = set_of_progvar.

compute_via_cell_vars([], _MarkedBenefits) = set_of_var.init.
compute_via_cell_vars([FieldCostsBenefits | FieldsCostsBenefits],
        MarkedBenefits) = ViaCellVars :-
    ViaCellVars1 = compute_via_cell_vars(FieldsCostsBenefits, MarkedBenefits),
    FieldCostsBenefits = field_costs_benefits(FieldVar, _, FieldBenefits),
    set.intersect(FieldBenefits, MarkedBenefits, MarkedFieldBenefits),
    ( if set.is_empty(MarkedFieldBenefits) then
        set_of_var.insert(FieldVar, ViaCellVars1, ViaCellVars)
    else if set.equal(MarkedFieldBenefits, FieldBenefits) then
        ViaCellVars = ViaCellVars1
    else
        unexpected($pred,
            "theorem violation: intersection neither empty nor full")
    ).

%-----------------------------------------------------------------------------%

    % Get the set of benefit nodes in the first argument that are not matched
    % by a cost node in the given matching.
    %
:- func get_unmatched_benefit_nodes(list(benefit_node),
    map(benefit_node, cost_node)) = list(benefit_node).

get_unmatched_benefit_nodes([], _) = [].
get_unmatched_benefit_nodes([Node | Nodes], MatchingBC) = UnmatchedNodes :-
    UnmatchedNodes1 = get_unmatched_benefit_nodes(Nodes, MatchingBC),
    ( if map.search(MatchingBC, Node, _Match) then
        UnmatchedNodes = UnmatchedNodes1
    else
        UnmatchedNodes = [Node | UnmatchedNodes1]
    ).

    % Get the set of cost nodes in the first argument that are not matched
    % by a benefit node in the given matching.
    %
:- func get_unmatched_cost_nodes(list(cost_node),
    map(cost_node, benefit_node)) = list(cost_node).

get_unmatched_cost_nodes([], _) = [].
get_unmatched_cost_nodes([Node | Nodes], MatchingCB) = UnmatchedNodes :-
    UnmatchedNodes1 = get_unmatched_cost_nodes(Nodes, MatchingCB),
    ( if map.search(MatchingCB, Node, _Match) then
        UnmatchedNodes = UnmatchedNodes1
    else
        UnmatchedNodes = [Node | UnmatchedNodes1]
    ).

%-----------------------------------------------------------------------------%

    % Dump the results of the matching process to standard output to assist in
    % tracking down any correctness and performance problems with this module.
    % Using this predicate requires uncommenting the import of module unsafe,
    % the call to dump_results, and two lines computing one of the arguments of
    % that call.
    %
:- pred dump_results(io.text_output_stream::in,
    prog_var::in, set_of_progvar::in, list(prog_var)::in,
    set_of_progvar::in, bool::in, set_of_progvar::in,
    assoc_list(int, set_of_progvar)::in,
    list(benefit_node)::in, list(benefit_operation)::in,
    list(cost_node)::in, list(cost_operation)::in, io::di, io::uo) is det.

dump_results(Stream, CellVar,
        CandidateFieldVars, OccurringCandidateFieldVarList,
        ViaCellOccurringVars, Nullified, BeforeFlush, AfterFlush,
        BenefitNodes, BenefitOps, CostNodes, CostOps, !IO) :-
    term.var_to_int(CellVar, CellVarInt),
    CandidateFieldVarList = set_of_var.to_sorted_list(CandidateFieldVars),
    ViaCellVarList = set_of_var.to_sorted_list(ViaCellOccurringVars),
    BeforeFlushList = set_of_var.to_sorted_list(BeforeFlush),
    list.map(term.var_to_int, CandidateFieldVarList,
        CandidateFieldVarInts),
    list.map(term.var_to_int, OccurringCandidateFieldVarList,
        OccurringCandidateFieldVarInts),
    list.map(term.var_to_int, ViaCellVarList, ViaCellVarInts),
    list.map(term.var_to_int, BeforeFlushList, BeforeFlushInts),
    CandidateFieldVarsStr = string.join_list(", ",
        list.map(string.int_to_string, CandidateFieldVarInts)),
    OccurringCandidateFieldVarsStr = string.join_list(", ",
        list.map(string.int_to_string, OccurringCandidateFieldVarInts)),
    ViaCellVarsStr = string.join_list(", ",
        list.map(string.int_to_string, ViaCellVarInts)),
    (
        Nullified = no,
        NullifiedSuffix = ""
    ;
        Nullified = yes,
        NullifiedSuffix = " nullified"
    ),
    BeforeFlushIntsStr = string.join_list(", ",
        list.map(string.int_to_string, BeforeFlushInts)),

    io.format(Stream, "%%\n%% FIND_VIA_CELL_VARS %d => f(%s)\n",
        [i(CellVarInt), s(CandidateFieldVarsStr)], !IO),
    io.format(Stream, "%% occurring [%s]\n",
        [s(OccurringCandidateFieldVarsStr)], !IO),
    io.format(Stream, "%% via cell [%s]%s\n",
        [s(ViaCellVarsStr), s(NullifiedSuffix)], !IO),
    io.format(Stream, "%% before flush, segment 1: [%s]\n",
        [s(BeforeFlushIntsStr)], !IO),
    list.foldl(dump_after_flush(Stream), AfterFlush, !IO),
    io.format(Stream, "%% realized benefits: %d ops, %d nodes:\n",
        [i(list.length(BenefitOps)), i(list.length(BenefitNodes))], !IO),
    io.write_string(Stream, "% ", !IO),
    io.write_line(Stream, BenefitOps, !IO),

    io.format(Stream, "%% realized costs: %d ops, %d nodes:",
        [i(list.length(CostOps)), i(list.length(CostNodes))], !IO),
    io.write_string(Stream, "% ", !IO),
    io.write_line(Stream, CostOps, !IO),
    io.write_string(Stream, "%\n", !IO).

:- pred dump_after_flush(io.text_output_stream::in,
    pair(int, set_of_progvar)::in, io::di, io::uo) is det.

dump_after_flush(Stream, SegmentNum - SegmentVars, !IO) :-
    SegmentVarList = set_of_var.to_sorted_list(SegmentVars),
    list.map(term.var_to_int, SegmentVarList, SegmentVarInts),
    list.map(string.int_to_string, SegmentVarInts, SegmentVarStrs),
    SegmentVarsStr = string.join_list(", ", SegmentVarStrs),
    io.format(Stream, "%% after flush, segment %d: [%s]\n",
        [i(SegmentNum), s(SegmentVarsStr)], !IO).

%-----------------------------------------------------------------------------%

:- pred realized_costs_benefits(set_of_progvar::in, field_costs_benefits::in)
    is semidet.

realized_costs_benefits(ViaCellOccurringVars, FieldCostsBenefits) :-
    FieldCostsBenefits = field_costs_benefits(FieldVar, _, _),
    set_of_var.member(ViaCellOccurringVars, FieldVar).

:- func project_benefit_op(benefit_node) = benefit_operation.

project_benefit_op(benefit_node(BenefitOp, _CopyNum)) = BenefitOp.

:- func project_cost_op(cost_node) = cost_operation.

project_cost_op(cost_node(CostOp, _CopyNum)) = CostOp.

%-----------------------------------------------------------------------------%
:- end_module backend_libs.matching.
%-----------------------------------------------------------------------------%
