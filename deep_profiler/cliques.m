%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module allows you build a description of a directed graph (represented
% as a set of arcs between nodes identified by dense small integers) and then
% find the strongly connected components (cliques) of that graph.

:- module cliques.

:- interface.

:- type graph.

:- import_module list, set.

	% Create a graph with no edges.
:- pred init(graph::out) is det.

	% Add an arc from one node to another.
:- pred add_arc(graph::in, int::in, int::in, graph::out) is det.

	% Perform a topological sort on the graph. Each set of integers in the
	% resulting list gives the ids of the nodes in a clique. The list
	% contains the cliques in top-down order: if there is an arc from
	% node A to node B and the two nodes are not in the same clique,
	% then the clique containing node A will be before the clique
	% containing node B.
:- pred topological_sort(graph::in, list(set(int))::out) is det.

:- implementation.

:- import_module array_util, dense_bitset.
:- import_module array, int.

:- type graph
	--->	graph(
			int,
			array(set(int))
		).

:- type visit == dense_bitset.

init(graph(1, Array)) :-
	% The initial array size doesn't really matter.
	array__init(16, set__init, Array).

add_arc(graph(Size0, Array0), From, To, Graph) :-
	( array__in_bounds(Array0, From) ->
		array__lookup(Array0, From, Tos0),
		set__insert(Tos0, To, Tos),
		array__set(u(Array0), From, Tos, Array),
		Size = int__max(int__max(From, To), Size0),
		Graph = graph(Size, Array)
	;
		array__size(Array0, Size),
		array__resize(u(Array0), Size * 2, init, Array1),
		add_arc(graph(Size0, Array1), From, To, Graph)
	).

:- pred successors(graph::in, int::in, set(int)::out) is det.

successors(graph(_Size, Array), From, Tos) :-
	( array__in_bounds(Array, From) ->
		array__lookup(Array, From, Tos)
	;
		Tos = set__init
	).

:- pred mklist(int::in, list(int)::in, list(int)::out) is det.

mklist(N, Acc0, Acc) :-
	( N < 0 ->
		Acc = Acc0
	;
		Acc1 = [N | Acc0],
		mklist(N - 1, Acc1, Acc)
	).

topological_sort(Graph, TSort) :-
	dfs_graph(Graph, Dfs),
	inverse(Graph, InvGraph),
	Visit = dense_bitset__init,
	tsort(Dfs, InvGraph, Visit, [], TSort0),
	reverse(TSort0, TSort).

:- pred tsort(list(int)::in, graph::in, visit::array_di, list(set(int))::in,
	list(set(int))::out) is det.

tsort([], _InvGraph, _Visit, TSort, TSort).
tsort([Node | Nodes], InvGraph, Visit0, TSort0, TSort) :-
	( dense_bitset__member(Node, Visit0) ->
		tsort(Nodes, InvGraph, Visit0, TSort0, TSort)
	;
		dfs([Node], InvGraph, Visit0, [], Visit, CliqueList),
		set__list_to_set(CliqueList, Clique),
		tsort(Nodes, InvGraph, Visit, [Clique | TSort0], TSort)
	).

% Return a list containing all the nodes of the graph. The list is effectively
% computed by randomly breaking all cycles, doing a pre-order traversal of
% the resulting trees, and concatenating the resulting lists in a random order.

:- pred dfs_graph(graph::in, list(int)::out) is det.

dfs_graph(Graph, Dfs) :-
	Graph = graph(Size, _Array),
	mklist(Size, [], NodeList),
	Visit = dense_bitset__init,
	dfs_graph_2(NodeList, Graph, Visit, [], Dfs).

:- pred dfs_graph_2(list(int)::in, graph::in, visit::array_di,
	list(int)::in, list(int)::out) is det.

dfs_graph_2([], _Graph, _Visit, Dfs, Dfs).
dfs_graph_2([Node | Nodes], Graph, Visit0, Dfs0, Dfs) :-
	dfs([Node], Graph, Visit0, Dfs0, Visit, Dfs1),
	dfs_graph_2(Nodes, Graph, Visit, Dfs1, Dfs).

% dfs(NodeList, Graph, Visit0, Dfs0, Visit, Dfs):
% For every node in NodeList, add the node and all its successors to the front
% of Dfs0, giving Dfs. The descendants of a node will in general be after that
% node in Dfs. The only situation where that may not be the case is when two
% nodes are descendants of each other. We detect such situations by passing
% along the set of nodes that have been visited already.

:- pred dfs(list(int)::in, graph::in, visit::array_di, list(int)::in,
	visit::array_uo, list(int)::out) is det.

dfs([], _Graph, Visit, Dfs, Visit, Dfs).
dfs([Node | Nodes], Graph, Visit0, Dfs0, Visit, Dfs) :-
	( dense_bitset__member(Node, Visit0) ->
		dfs(Nodes, Graph, Visit0, Dfs0, Visit, Dfs)
	;
		Visit1 = dense_bitset__insert(Visit0, Node),
		successors(Graph, Node, Succ),
		set__to_sorted_list(Succ, SuccList),
		dfs(SuccList, Graph, Visit1, Dfs0, Visit2, Dfs1),
		Dfs2 = [Node | Dfs1],
		dfs(Nodes, Graph, Visit2, Dfs2, Visit, Dfs)
	).

:- pred inverse(graph::in, graph::out) is det.

inverse(Graph, InvGraph) :-
	init(InvGraph0),
	Graph = graph(Size, _Array),
	inverse_2(Size, Graph, InvGraph0, InvGraph).

:- pred inverse_2(int::in, graph::in, graph::in, graph::out) is det.

inverse_2(To, Graph, InvGraph0, InvGraph) :-
	( To >= 0 ->
		successors(Graph, To, Froms),
		set__to_sorted_list(Froms, FromList),
		add_arcs_to(FromList, To, InvGraph0, InvGraph1),
		inverse_2(To - 1, Graph, InvGraph1, InvGraph)
	;
		InvGraph = InvGraph0
	).

:- pred add_arcs_to(list(int)::in, int::in, graph::in, graph::out) is det.

add_arcs_to([], _, Graph, Graph).
add_arcs_to([From | FromList], To, Graph0, Graph) :-
	add_arc(Graph0, From, To, Graph1),
	add_arcs_to(FromList, To, Graph1, Graph).
