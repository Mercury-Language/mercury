%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: pjs, zs
%
% Module `matching' - performs bipartite graph maximal matching computation
% specialized for the stack slot optimization. The structure of the graph
% on which the algorithm operates is documented in the paper "Using the heap
% to eliminate stack accesses" by Zoltan Somogyi and Peter Stuckey.
%
%-----------------------------------------------------------------------------%

:- module backend_libs__matching.

:- interface.
:- import_module parse_tree__prog_data.

:- import_module bool, list, set.

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
	--->	matching_params(
			cell_var_store_cost	:: int,
			cell_var_load_cost	:: int,
			field_var_store_cost	:: int,
			field_var_load_cost	:: int,
			one_path_op_ratio	:: int,
			one_path_node_ratio	:: int,
			include_all_candidates	:: bool
		).

	% find_via_cell_vars(CellVar, CandidateFieldVars, CellVarFlushedLater,
	%	BeforeFlush, AfterFlush, MatchingParams,
	%	RealizedBenefitNodes, RealizedCostNodes, ViaCellVars)
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

:- type benefit_node.
:- type cost_node.

:- pred find_via_cell_vars(prog_var::in, set(prog_var)::in, bool::in,
	set(prog_var)::in, list(set(prog_var))::in, matching_params::in,
	set(benefit_node)::out, set(cost_node)::out, set(prog_var)::out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

% Uncomment if you want to dump performance information into the .err file.
% :- import_module unsafe.

:- import_module term.
:- import_module int, assoc_list, map, queue, std_util, require, io.

% The stack optimization graph is a bipartite graph, whose two node types
% are cost nodes and benefit nodes. Each node represents a copy of an
% operation, a load or a store. We have LoadCost copies of each load operation
% and StoreCost copies of each store operation, where LoadCost and StoreCost
% are parameters of find_via_cell_vars.
%
% We represent the stack optimization graph in the usual manner: as two maps,
% with each map one kind of node to the set of nodes of the other types to
% which it is adjacent.

:- type stack_slot_graph
	--->	stack_slot_graph(
			map(cost_node, set(benefit_node)),
			map(benefit_node, set(cost_node))
		).

:- type cost_operation
	--->	cell_var_load(int)		% segment number, >= 2
	;	cell_var_store.			% in initial segment

:- type benefit_operation
	--->	field_var_load(prog_var)	% in initial segment
	;	field_var_store(prog_var).	% in initial segment

% The integers differentiate the different copies of an operation.
:- type cost_node --->		cost_node(cost_operation, int).
:- type benefit_node --->	benefit_node(benefit_operation, int).

% The field_costs_benefits structure records, for a given field variable,
% the nodes of the cost we incur and the benefits we gain if we access that
% field variable via the cell instead of via the stack.

:- type	field_costs_benefits
	--->	field_costs_benefits(
			prog_var,
			set(cost_node),
			set(benefit_node)
		).

% Matchings are sets of edges, in which each node in the graph can occur at
% most once. We represent the matching by mapping each node that is an endpoint
% of an edge in the matching to the node at the other end of the edge.
% If a node is not in the matching, it will not occur in the relevant map.

:- type matching
	--->	matching(
			map(cost_node, benefit_node),
			map(benefit_node, cost_node)
		).

%-----------------------------------------------------------------------------%

% Uncomment if you want to dump performance information into the .err file.
% :- pragma promise_pure(find_via_cell_vars/15).

find_via_cell_vars(CellVar, CandidateFieldVars, CellVarFlushedLater,
		BeforeFlush, AfterFlush, MatchingParams,
		RealizedBenefitNodes, RealizedCostNodes, ViaCellVars) :-
	InclAllCand = MatchingParams ^ include_all_candidates,
	(
		InclAllCand = no,
		AllSegmentVars = set__union_list([BeforeFlush | AfterFlush]),
		set__intersect(CandidateFieldVars, AllSegmentVars,
			OccurringCandidateFieldVars),
		set__difference(CandidateFieldVars, OccurringCandidateFieldVars,
			NonOccurringCandidateFieldVars)
	;
		InclAllCand = yes,
		OccurringCandidateFieldVars = CandidateFieldVars,
		NonOccurringCandidateFieldVars = set__init
	),
	set__to_sorted_list(OccurringCandidateFieldVars,
		OccurringCandidateFieldVarList),
	list__filter_map(
		simplify_segment(CellVar, OccurringCandidateFieldVars),
		AfterFlush, FilteredAfterFlush),
	NumberedAfterFlush = number_segments(2, FilteredAfterFlush),
	CostsBenefits = list__map(
		find_costs_benefits(CellVar, BeforeFlush, NumberedAfterFlush,
			CellVarFlushedLater, MatchingParams),
		OccurringCandidateFieldVarList),
	list__foldl(gather_benefits, CostsBenefits, set__init, BenefitNodes),
	list__foldl(gather_costs, CostsBenefits, set__init, CostNodes),
	set__to_sorted_list(BenefitNodes, BenefitNodeList),
	set__to_sorted_list(CostNodes, CostNodeList),
	Graph = create_graph(CostsBenefits),
	MaximalMatching = maximal_matching(BenefitNodeList, Graph),
	MaximalMatching = matching(MaximalMatchingCostToBenefit, _),
	UnMatchedCostNodes = get_unmatched_cost_nodes(CostNodeList,
		MaximalMatchingCostToBenefit),
	MarkedBenefitNodes = reachable_by_alternating_path(UnMatchedCostNodes,
		Graph, MaximalMatching),
	ViaCellOccurringVars0 =
		compute_via_cell_vars(CostsBenefits, MarkedBenefitNodes),
	list__filter(realized_costs_benefits(ViaCellOccurringVars0),
		CostsBenefits, RealizedCostsBenefits),
	list__foldl(gather_benefits, RealizedCostsBenefits,
		set__init, RealizedBenefitNodes),
	list__foldl(gather_costs, RealizedCostsBenefits,
		set__init, RealizedCostNodes),
	RealizedBenefitOps =
		set__map(project_benefit_op, RealizedBenefitNodes),
	RealizedCostOps =
		set__map(project_cost_op, RealizedCostNodes),
	set__to_sorted_list(RealizedBenefitNodes, RealizedBenefitNodeList),
	set__to_sorted_list(RealizedCostNodes, RealizedCostNodeList),
	set__to_sorted_list(RealizedBenefitOps, RealizedBenefitOpList),
	set__to_sorted_list(RealizedCostOps, RealizedCostOpList),
	list__length(RealizedBenefitNodeList, RealizedBenefitNodeCount),
	list__length(RealizedBenefitOpList, RealizedBenefitOpCount),
	list__length(RealizedCostNodeList, RealizedCostNodeCount),
	list__length(RealizedCostOpList, RealizedCostOpCount),
	OpRatio = MatchingParams ^ one_path_op_ratio,
	NodeRatio = MatchingParams ^ one_path_node_ratio,
	(
		RealizedBenefitOpCount * 100 >=
			RealizedCostOpCount * OpRatio,
		RealizedBenefitNodeCount * 100 >=
			RealizedCostNodeCount * NodeRatio
	->
		ViaCellOccurringVars = ViaCellOccurringVars0
		% Uncomment if you want to dump performance information into
		% the .err file.
		% Nullified = no
	;
		ViaCellOccurringVars = set__init
		% Uncomment if you want to dump performance information into
		% the .err file.
		% Nullified = yes
	),
	ViaCellVars = set__union(ViaCellOccurringVars,
		NonOccurringCandidateFieldVars).
	% Uncomment if you want to dump performance information into
	% the .err file.
	% impure unsafe_perform_io(dump_results(CellVar, CandidateFieldVars,
	% 	OccurringCandidateFieldVarList, ViaCellOccurringVars0,
	% 	Nullified, BeforeFlush, NumberedAfterFlush,
	% 	RealizedBenefitNodeList, RealizedBenefitOpList,
	% 	RealizedCostNodeList, RealizedCostOpList)).

%-----------------------------------------------------------------------------%

% Simplify_segment fails if the CellVar is in the SegmentVars since the cost
% of executing such segments doesn't depend on whether we access field vars
% via the cell var or via the stack. If CellVar is not in SegmentVars,
% them simplify_segment succeeds after removing the non-candidate variables
% from SegmentVars0.

:- pred simplify_segment(prog_var::in, set(prog_var)::in, set(prog_var)::in,
	set(prog_var)::out) is semidet.

simplify_segment(CellVar, CandidateArgVars, SegmentVars0, SegmentVars) :-
	\+ set__member(CellVar, SegmentVars0),
	SegmentVars = set__intersect(SegmentVars0, CandidateArgVars).

:- func number_segments(int, list(set(prog_var))) =
	assoc_list(int, set(prog_var)).

number_segments(_N, []) = [].
number_segments(N, [Segment | Segments]) =
	[N - Segment | number_segments(N + 1, Segments)].

%-----------------------------------------------------------------------------%

% find_costs_benefits computes the costs and benefits of accessing the given
% field variable FieldVar via the cell variable CellVar.

:- func find_costs_benefits(prog_var, set(prog_var),
	assoc_list(int, set(prog_var)), bool, matching_params, prog_var)
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
	( set__member(CellVar, BeforeFlush) ->
		BenefitOps = BenefitOps0
	;
		BenefitOps = [field_var_load(FieldVar) | BenefitOps0]
	),

	CellVarStoreCost = MatchingParams ^ cell_var_store_cost,
	CellVarLoadCost = MatchingParams ^ cell_var_load_cost,
	CostNodeLists = list__map(
		replicate_cost_op(CellVarStoreCost, CellVarLoadCost),
		CostOps),
	list__condense(CostNodeLists, CostNodes),
	set__list_to_set(CostNodes, CostNodeSet),
	FieldVarStoreCost = MatchingParams ^ field_var_store_cost,
	FieldVarLoadCost = MatchingParams ^ field_var_load_cost,
	BenefitNodeLists = list__map(
		replicate_benefit_op(FieldVarStoreCost, FieldVarLoadCost),
		BenefitOps),
	list__condense(BenefitNodeLists, BenefitNodes),
	set__list_to_set(BenefitNodes, BenefitNodeSet),
	FieldCostsBenefits = field_costs_benefits(FieldVar,
		CostNodeSet, BenefitNodeSet).

:- pred find_cell_var_loads_for_field(assoc_list(int, set(prog_var))::in,
	prog_var::in, list(cost_operation)::in, list(cost_operation)::out)
	is det.

find_cell_var_loads_for_field([], _, CostOps, CostOps).
find_cell_var_loads_for_field([SegmentNum - SegmentVars | AfterFlush],
		FieldVar, CostOps0, CostOps) :-
	( set__member(FieldVar, SegmentVars) ->
		CostOps1 = [cell_var_load(SegmentNum) | CostOps0]
	;
		CostOps1 = CostOps0
	),
	find_cell_var_loads_for_field(AfterFlush, FieldVar, CostOps1, CostOps).

%-----------------------------------------------------------------------------%

:- func replicate_cost_op(int, int, cost_operation) = list(cost_node).

replicate_cost_op(_StoreCost, LoadCost, cell_var_load(Segment)) =
	make_cost_op_copies(LoadCost, cell_var_load(Segment)).
replicate_cost_op(StoreCost, _LoadCost, cell_var_store) =
	make_cost_op_copies(StoreCost, cell_var_store).

:- func make_cost_op_copies(int, cost_operation) = list(cost_node).

make_cost_op_copies(Cur, Op) =
	( Cur > 0 ->
		[cost_node(Op, Cur) | make_cost_op_copies(Cur - 1, Op)]
	;
		[]
	).

:- func replicate_benefit_op(int, int, benefit_operation) = list(benefit_node).

replicate_benefit_op(_StoreCost, LoadCost, field_var_load(FieldVar)) =
	make_benefit_op_copies(LoadCost, field_var_load(FieldVar)).
replicate_benefit_op(StoreCost, _LoadCost, field_var_store(FieldVar)) =
	make_benefit_op_copies(StoreCost, field_var_store(FieldVar)).

:- func make_benefit_op_copies(int, benefit_operation) = list(benefit_node).

make_benefit_op_copies(Cur, Op) =
	( Cur > 0 ->
		[benefit_node(Op, Cur) | make_benefit_op_copies(Cur - 1, Op)]
	;
		[]
	).

%-----------------------------------------------------------------------------%

% Accumulate all the benefit nodes.

:- pred gather_benefits(field_costs_benefits::in, set(benefit_node)::in,
	set(benefit_node)::out) is det.

gather_benefits(field_costs_benefits(_, _, FieldBenefits),
		Benefits0, Benefits) :-
	set__union(Benefits0, FieldBenefits, Benefits).

% Accumulate all the cost nodes.

:- pred gather_costs(field_costs_benefits::in, set(cost_node)::in,
	set(cost_node)::out) is det.

gather_costs(field_costs_benefits(_, FieldCosts, _), Costs0, Costs) :-
	set__union(Costs0, FieldCosts, Costs).

%-----------------------------------------------------------------------------%

% Create the stack slot optimization graph described in the paper.

:- func create_graph(list(field_costs_benefits)) = stack_slot_graph.

create_graph(CostsBenefits) = Graph :-
	list__foldl2(create_graph_links, CostsBenefits,
		map__init, CostToBenefitsMap, map__init, BenefitToCostsMap),
	Graph = stack_slot_graph(CostToBenefitsMap, BenefitToCostsMap).

:- pred create_graph_links(field_costs_benefits::in,
	map(cost_node, set(benefit_node))::in,
	map(cost_node, set(benefit_node))::out,
	map(benefit_node, set(cost_node))::in,
	map(benefit_node, set(cost_node))::out) is det.

create_graph_links(field_costs_benefits(_FieldVar, Costs, Benefits),
		CostToBenefitsMap0, CostToBenefitsMap,
		BenefitToCostsMap0, BenefitToCostsMap) :-
	list__foldl(add_cost_benefit_links(Benefits),
		set__to_sorted_list(Costs),
		CostToBenefitsMap0, CostToBenefitsMap),
	list__foldl(add_benefit_cost_links(Costs),
		set__to_sorted_list(Benefits),
		BenefitToCostsMap0, BenefitToCostsMap).

:- pred add_cost_benefit_links(set(benefit_node)::in, cost_node::in,
	map(cost_node, set(benefit_node))::in,
	map(cost_node, set(benefit_node))::out) is det.

add_cost_benefit_links(Benefits, Cost, CostToBenefitsMap0, CostToBenefitsMap) :-
	( map__search(CostToBenefitsMap0, Cost, CostBenefits0) ->
		set__union(CostBenefits0, Benefits, CostBenefits),
		map__det_update(CostToBenefitsMap0, Cost, CostBenefits,
			CostToBenefitsMap)
	;
		map__det_insert(CostToBenefitsMap0, Cost, Benefits,
			CostToBenefitsMap)
	).

:- pred add_benefit_cost_links(set(cost_node)::in, benefit_node::in,
	map(benefit_node, set(cost_node))::in,
	map(benefit_node, set(cost_node))::out) is det.

add_benefit_cost_links(Costs, Benefit, BenefitToCostsMap0, BenefitToCostsMap) :-
	( map__search(BenefitToCostsMap0, Benefit, BenefitCosts0) ->
		set__union(BenefitCosts0, Costs, BenefitCosts),
		map__det_update(BenefitToCostsMap0, Benefit, BenefitCosts,
			BenefitToCostsMap)
	;
		map__det_insert(BenefitToCostsMap0, Benefit, Costs,
			BenefitToCostsMap)
	).

%-----------------------------------------------------------------------------%

% Find a maximal matching in the given graph.

:- func maximal_matching(list(benefit_node), stack_slot_graph) = matching.

maximal_matching(BenefitNodes, Graph) = Matching :-
	Matching0 = matching(map__init, map__init),
	maximize_matching(BenefitNodes, Graph, Matching0, Matching).

:- pred maximize_matching(list(benefit_node)::in, stack_slot_graph::in,
	matching::in, matching::out) is det.

maximize_matching(BenefitNodes, Graph, Matching0, Matching) :-
	( Path = find_augmenting_path(BenefitNodes, Graph, Matching0) ->
		Matching1 = update_matches(Path, Matching0),
		maximize_matching(BenefitNodes, Graph, Matching1, Matching)
	;
		Matching = Matching0
	).

:- func update_matches(edge_list, matching) = matching.

update_matches([], Matching0) = Matching0.
update_matches([BenefitNode - CostNode | Path], Matching0) = Matching :-
	Matching0 = matching(CostToBenefitMap0, BenefitToCostMap0),
	map__set(CostToBenefitMap0, CostNode, BenefitNode, CostToBenefitMap1),
	map__set(BenefitToCostMap0, BenefitNode, CostNode, BenefitToCostMap1),
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

% Breadth-first search for an augmenting path.

% Build an initial queue of all the unmatched benefit nodes, with empty paths.
% Take the first element of the queue and see what nodes are reachable
% from there. If one is unmatched return the path, otherwise add these nodes
% to the queue if they haven't been visited before.

:- type edge_list == assoc_list(benefit_node, cost_node).

:- type benefit_node_and_edge_list == pair(benefit_node, edge_list).

:- func find_first_path_bf(list(benefit_node), stack_slot_graph, matching)
	= edge_list is semidet.

find_first_path_bf(BenefitNodes, Graph, Matching) = Path :-
	Queue = initial_queue(BenefitNodes, queue__init),
	Path = augpath_bf(Queue, BenefitNodes, Graph, Matching).

:- func initial_queue(list(benefit_node), queue(benefit_node_and_edge_list))
	= queue(benefit_node_and_edge_list).

initial_queue([], Q) = Q.
initial_queue([N | Ns], Q0) = Q :-
	Q1 = queue__put(Q0, N - []),
	Q = initial_queue(Ns, Q1).

:- func augpath_bf(queue(benefit_node_and_edge_list), list(benefit_node),
	stack_slot_graph, matching) = edge_list is semidet.

augpath_bf(Queue0, Seen0, Graph, Matching) = Path :-
	queue__get(Queue0, NodePath, Queue1),
	NodePath = BenefitNode - Path0,
	Graph = stack_slot_graph(_, BenefitToCostsMap),
	map__lookup(BenefitToCostsMap, BenefitNode, AdjCostNodes),
	Matching = matching(CostToBenefitMap, _),
	CostMatches = map_adjs_to_matched_cost(
		set__to_sorted_list(AdjCostNodes), CostToBenefitMap),
	( find_unmatched_cost(CostMatches) = UnmatchedCostNode ->
		Path = [BenefitNode - UnmatchedCostNode | Path0]
	;
		add_alternates(CostMatches, Seen0, Seen, BenefitNode, Path0,
			Queue1, Queue2),
		Path = augpath_bf(Queue2, Seen, Graph, Matching)
	).

:- func find_unmatched_cost(assoc_list(cost_node, maybe(benefit_node)))
	= cost_node is semidet.

find_unmatched_cost([CostNode - MaybeBenefitNode | Matches]) = Unmatched :-
	( MaybeBenefitNode = no ->
		Unmatched = CostNode
	;
		Unmatched = find_unmatched_cost(Matches)
	).

% For each node CostNode adjacent to BenefitNode, we have determined whether
% they are matched (that information is in MaybeAdjBenefitNode).
% If AdjBenefitNode has not been visited before (it is not in Seen0),
% we add it to the queue with the path extended by the last arc
% (BenefitNode - CostNode)

:- pred add_alternates(assoc_list(cost_node, maybe(benefit_node))::in,
	list(benefit_node)::in, list(benefit_node)::out, benefit_node::in,
	edge_list::in, queue(benefit_node_and_edge_list)::in,
	queue(benefit_node_and_edge_list)::out) is det.

add_alternates([], Seen, Seen, _, _, Queue, Queue).
add_alternates([CostNode - MaybeAdjBenefitNode | CostMatches], Seen0, Seen,
		BenefitNode, Path, Queue0, Queue) :-
	(
		MaybeAdjBenefitNode = yes(AdjBenefitNode),
		not list__member(AdjBenefitNode, Seen0)
	->
		Seen1 = [AdjBenefitNode | Seen0],
		Queue1 = queue__put(Queue0,
			AdjBenefitNode - [BenefitNode - CostNode | Path])
	;
		Seen1 = Seen0,
		Queue1 = Queue0
	),
	add_alternates(CostMatches, Seen1, Seen, BenefitNode, Path,
		Queue1, Queue).

%-----------------------------------------------------------------------------%

% Find all the benefit nodes reachable from the cost nodes in the first
% argument via alternating paths. The SelectedCostNodes are not matched,
% so first we look for matched benefit nodes adjacent to them, since those
% nodes are reachable. We then look at the cost nodes matched to those benefit
% nodes, since the benefit nodes reachable from there are also reachable from
% the original cost nodes.
%
% To ensure termination, we follow the matched link from a benefit node
% only when that benefit node is first put into the reachable set.

:- func reachable_by_alternating_path(list(cost_node), stack_slot_graph,
	matching) = set(benefit_node).

reachable_by_alternating_path(SelectedCostNodes, Graph, Matching)
		= ReachableBenefitNodes :-
	reachable_by_alternating_path(SelectedCostNodes, Graph, Matching,
		set__init, ReachableBenefitNodes).

:- pred reachable_by_alternating_path(list(cost_node)::in,
	stack_slot_graph::in, matching::in, set(benefit_node)::in,
	set(benefit_node)::out) is det.

reachable_by_alternating_path(SelectedCostNodes, Graph, Matching,
		BenefitNodes0, BenefitNodes) :-
	( SelectedCostNodes = [] ->
		BenefitNodes = BenefitNodes0
	;
		Graph = stack_slot_graph(CostToBenefitsMap, _),
		list__foldl(adjacents(CostToBenefitsMap), SelectedCostNodes,
			set__init, AdjBenefitNodes),
		set__union(AdjBenefitNodes, BenefitNodes0, BenefitNodes1),
		set__difference(BenefitNodes0, AdjBenefitNodes,
			NewBenefitNodes),
		set__to_sorted_list(NewBenefitNodes, NewBenefitNodeList),
		Matching = matching(_, BenefitToCostMap),
		LinkedCostNodes = list__map(map__lookup(BenefitToCostMap),
			NewBenefitNodeList),
		reachable_by_alternating_path(LinkedCostNodes, Graph, Matching,
			BenefitNodes1, BenefitNodes)
	).

:- pred adjacents(map(cost_node, set(benefit_node))::in, cost_node::in,
	set(benefit_node)::in, set(benefit_node)::out) is det.

adjacents(CostToBenefitsMap, CostNode, BenefitNodes0, BenefitNodes) :-
	map__lookup(CostToBenefitsMap, CostNode, AdjBenefitNodes),
	set__union(BenefitNodes0, AdjBenefitNodes, BenefitNodes).

%-----------------------------------------------------------------------------%

% Given a list of cost nodes adjacent to a benefit node, find out for each of
% those cost nodes whether it is linked to a benefit node by the given
% matching, and if yes, to which one.

:- func map_adjs_to_matched_cost(list(cost_node), map(cost_node, benefit_node))
	= assoc_list(cost_node, maybe(benefit_node)).

map_adjs_to_matched_cost(AdjCostNodes, CostToBenefitMap) = CostMatches :-
	CostMatches = list__map(adj_to_matched_cost(CostToBenefitMap),
		AdjCostNodes).

:- func adj_to_matched_cost(map(cost_node, benefit_node), cost_node) =
	pair(cost_node, maybe(benefit_node)).

adj_to_matched_cost(CostToBenefitMap, CostNode) = Match :-
	( map__search(CostToBenefitMap, CostNode, BenefitNode) ->
		Match = CostNode - yes(BenefitNode)
	;
		Match = CostNode - no
	).

%-----------------------------------------------------------------------------%

:- func compute_via_cell_vars(list(field_costs_benefits), set(benefit_node))
	= set(prog_var).

compute_via_cell_vars([], _MarkedBenefits) = set__init.
compute_via_cell_vars([FieldCostsBenefits | FieldsCostsBenefits],
		MarkedBenefits) = ViaCellVars :-
	ViaCellVars1 = compute_via_cell_vars(FieldsCostsBenefits,
		MarkedBenefits),
	FieldCostsBenefits = field_costs_benefits(FieldVar, _, FieldBenefits),
	set__intersect(FieldBenefits, MarkedBenefits, MarkedFieldBenefits),
	( set__empty(MarkedFieldBenefits) ->
		set__insert(ViaCellVars1, FieldVar, ViaCellVars)
	; set__equal(MarkedFieldBenefits, FieldBenefits) ->
		ViaCellVars = ViaCellVars1
	;
		error("compute_via_cell_vars: theorem violation: intersection neither empty nor full")
	).

%-----------------------------------------------------------------------------%

% Get the set of benefit nodes in the first argument that are not matched
% by a cost node in the given matching.

:- func get_unmatched_benefit_nodes(list(benefit_node),
	map(benefit_node, cost_node)) = list(benefit_node).

get_unmatched_benefit_nodes([], _) = [].
get_unmatched_benefit_nodes([Node | Nodes], MatchingBC) = UnmatchedNodes :-
	UnmatchedNodes1 = get_unmatched_benefit_nodes(Nodes, MatchingBC),
	( map__search(MatchingBC, Node, _Match) ->
		UnmatchedNodes = UnmatchedNodes1
	;
		UnmatchedNodes = [Node | UnmatchedNodes1]
	).

% Get the set of cost nodes in the first argument that are not matched
% by a benefit node in the given matching.

:- func get_unmatched_cost_nodes(list(cost_node),
	map(cost_node, benefit_node)) = list(cost_node).

get_unmatched_cost_nodes([], _) = [].
get_unmatched_cost_nodes([Node | Nodes], MatchingCB) = UnmatchedNodes :-
	UnmatchedNodes1 = get_unmatched_cost_nodes(Nodes, MatchingCB),
	( map__search(MatchingCB, Node, _Match) ->
		UnmatchedNodes = UnmatchedNodes1
	;
		UnmatchedNodes = [Node | UnmatchedNodes1]
	).

%-----------------------------------------------------------------------------%

% Dump the results of the matching process to standard output to assist in
% tracking down any correctness and performance problems with this module.
% Using this predicate requires uncommenting the import of module unsafe,
% the call to dump_results, and two lines computing one of the arguments of
% that call.

:- pred dump_results(prog_var::in, set(prog_var)::in, list(prog_var)::in,
	set(prog_var)::in, bool::in, set(prog_var)::in,
	assoc_list(int, set(prog_var))::in,
	list(benefit_node)::in, list(benefit_operation)::in,
	list(cost_node)::in, list(cost_operation)::in,
	io__state::di, io__state::uo) is det.

dump_results(CellVar, CandidateFieldVars, OccurringCandidateFieldVarList,
		ViaCellOccurringVars, Nullified, BeforeFlush, AfterFlush,
		BenefitNodes, BenefitOps, CostNodes, CostOps) -->
	{ term__var_to_int(CellVar, CellVarInt) },
	{ set__to_sorted_list(CandidateFieldVars, CandidateFieldVarList) },
	{ set__to_sorted_list(ViaCellOccurringVars, ViaCellVarList) },
	{ set__to_sorted_list(BeforeFlush, BeforeFlushList) },
	{ list__map(term__var_to_int, CandidateFieldVarList,
		CandidateFieldVarInts) },
	{ list__map(term__var_to_int, OccurringCandidateFieldVarList,
		OccurringCandidateFieldVarInts) },
	{ list__map(term__var_to_int, ViaCellVarList, ViaCellVarInts) },
	{ list__map(term__var_to_int, BeforeFlushList, BeforeFlushInts) },
	io__write_string("%\n% FIND_VIA_CELL_VARS "),
	io__write_int(CellVarInt),
	io__write_string(" => f("),
	io__write_list(CandidateFieldVarInts, ", ", io__write_int),
	io__write_string(")\n"),
	io__write_string("% occurring ["),
	io__write_list(OccurringCandidateFieldVarInts, ", ", io__write_int),
	io__write_string("]\n"),
	io__write_string("% via cell ["),
	io__write_list(ViaCellVarInts, ", ", io__write_int),
	io__write_string("]"),
	(
		{ Nullified = no },
		io__write_string("\n")
	;
		{ Nullified = yes },
		io__write_string(" nullified\n")
	),
	io__write_string("% before flush, segment 1: ["),
	io__write_list(BeforeFlushInts, ", ", io__write_int),
	io__write_string("]\n"),
	list__foldl(dump_after_flush, AfterFlush),
	io__write_string("% realized benefits: "),
	io__write_int(list__length(BenefitOps)),
	io__write_string(" ops, "),
	io__write_int(list__length(BenefitNodes)),
	io__write_string(" nodes:\n"),
	io__write_string("% "),
	io__write(BenefitOps),
	io__write_string("\n"),
	io__write_string("% realized costs: "),
	io__write_int(list__length(CostOps)),
	io__write_string(" ops, "),
	io__write_int(list__length(CostNodes)),
	io__write_string(" nodes:\n"),
	io__write_string("% "),
	io__write(CostOps),
	io__write_string("\n%\n").

:- pred dump_after_flush(pair(int, set(prog_var))::in,
	io__state::di, io__state::uo) is det.

dump_after_flush(SegmentNum - SegmentVars) -->
	{ set__to_sorted_list(SegmentVars, SegmentVarList) },
	{ list__map(term__var_to_int, SegmentVarList, SegmentVarInts) },
	io__write_string("% after flush, segment "),
	io__write_int(SegmentNum),
	io__write_string(": ["),
	io__write_list(SegmentVarInts, ", ", io__write_int),
	io__write_string("]\n").

%-----------------------------------------------------------------------------%

:- pred realized_costs_benefits(set(prog_var)::in, field_costs_benefits::in)
	is semidet.

realized_costs_benefits(ViaCellOccurringVars, FieldCostsBenefits) :-
	FieldCostsBenefits = field_costs_benefits(FieldVar, _, _),
	set__member(FieldVar, ViaCellOccurringVars).

:- func project_benefit_op(benefit_node) = benefit_operation.

project_benefit_op(benefit_node(BenefitOp, _CopyNum)) = BenefitOp.

:- func project_cost_op(cost_node) = cost_operation.

project_cost_op(cost_node(CostOp, _CopyNum)) = CostOp.

%-----------------------------------------------------------------------------%
