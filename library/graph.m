%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999, 2003, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% File: graph.m.
% Main author: conway.
% Stability: low.
%
% This module defines a directed graph data type. The type graph(N, A)
% stores information of type N in the nodes, and information of type A
% in the arcs.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module graph.

:- interface.
:- import_module list, set, std_util.

	% graph(Node, Arc) represents a directed graph with information of
	% type Node associated with each node, and information of type Arc
	% associated with each arc.
	%
:- type graph(N, A).

:- type node(N).

:- type arc(A).

	% Lots of graphs don't need to store anything in the arcs so here's
	% a type equivalence that only has `real' information in the nodes.
	%
:- type graph(N)	== graph(N, unit).

:- type arc		== arc(unit).

	% graph__init(Graph) binds Graph to an empty graph
	% containing no nodes and no arcs. (The graph contains
	% a counter of the number of nodes allocated in it, so
	% it is possible for a graph to contain no nodes or arcs
	% and still fail to unify with the binding of Graph from
	% graph__init.)
	%
:- pred graph__init(graph(N, A)).
:- mode graph__init(out) is det.

:- func graph__init = graph(N, A).

	% graph__set_node(OldGraph, NodeInfo, Node, NewGraph) takes
	% OldGraph and NodeInfo which is the information to be stored
	% in a new node, and returns a key "Node" which refers to that
	% node, and the new graph NewGraph containing all of the nodes
	% and arcs in OldGraph as well as the new node.
	% It is possible to have two nodes in the graph with the
	% same information stored in them.
	%
	% This operation is O(lgN) for a graph containing N nodes.
	%
:- pred graph__set_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__set_node(in, in, out, out) is det.

	% graph__insert_node/4 is the same as graph__set_node/4 except
	% that if the information to be stored in the node is stored
	% in another node, then the graph__insert_node/4 fails.
	%
	% This operation is O(N) for a graph containing N nodes since
	% this predicate has to check that the node data isn't in an
	% existing node.
	%
:- pred graph__insert_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__insert_node(in, in, out, out) is semidet.

	% graph__det_insert_node/4 is like graph__insert_node, except
	% that if the insertion would fail, it calls error/1.
	%
:- pred graph__det_insert_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__det_insert_node(in, in, out, out) is det.

	% graph__search_node(Graph, NodeInfo, Node) nondeterministically
	% produces bindings of Node such that Node is a node in Graph
	% that has the information NodeInfo attatched to it.
	%
	% This operation is O(lgN) for the first solution for a graph
	% containing N nodes.
	%
:- pred graph__search_node(graph(N, A), N, node(N)).
:- mode graph__search_node(in, in, out) is nondet.

	% graph__find_matching_nodes(Graph, NodeInfo, Nodes) takes a graph
	% Graph and the information NodeInfo and returns the set of nodes
	% Nodes which have the information NodeInfo stored in them. (The set
	% Nodes will of course be empty if there are no matching nodes.)
	%
	% This operation is O(NlgN) for a graph containing N nodes.
	%
:- pred graph__find_matching_nodes(graph(N, A), N, set(node(N))).
:- mode graph__find_matching_nodes(in, in, out) is det.

:- func graph__find_matching_nodes(graph(N, A), N) = set(node(N)).

	% graph__node_contents(Graph, Node, NodeInfo) takes Graph and
	% Node and returns the information NodeInfo stored in Node.
	%
	% This operation is O(lgN) for a graph containing N nodes.
	%
:- pred graph__node_contents(graph(N, A), node(N), N).
:- mode graph__node_contents(in, in, out) is det.

:- func graph__node_contents(graph(N, A), node(N)) = N.

	% graph__successors(Graph, Node, Nodes) takes a graph Graph and
	% a node Node and returns the set of nodes Nodes that are reachable
	% (directly - not transitively) from Node.
	%
	% This operation is O(NlgN) for a graph containing N nodes.
	%
:- pred graph__successors(graph(N, A), node(N), set(node(N))).
:- mode graph__successors(in, in, out) is det.

:- func graph__successors(graph(N, A), node(N)) = set(node(N)).

	% graph__nodes(Graph, Nodes) binds Nodes to the set of nodes in Graph.
	%
:- pred graph__nodes(graph(N, A), set(node(N))).
:- mode graph__nodes(in, out) is det.

:- func graph__nodes(graph(N, A)) = set(node(N)).

	% graph__set_edge(OldGraph, Start, End, ArcInfo, Arc, NewGraph)
	% takes a graph OldGraph and adds an arc from Start to End with
	% the information ArcInfo stored in it, and returns a key for
	% that arc Arc, and the new graph NewGraph.
	% If an identical arc already exists then this operation has
	% no effect.
	%
	% This operation is O(lgN+lgM) for a graph with N nodes and M arcs.
	%
:- pred graph__set_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__set_edge(in, in, in, in, out, out) is det.

	% graph__insert_edge/6 is the same as graph__set_edge/6 except that
	% if an identical arc already exists in the graph the operation fails.
	% This is O(N) for a graph with N edges between the two nodes.
	%
:- pred graph__insert_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__insert_edge(in, in, in, in, out, out) is semidet.

	% graph__det_insert_edge/6 is like graph__insert_edge except
	% than instead of failing, it calls error/1.
	%
:- pred graph__det_insert_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__det_insert_edge(in, in, in, in, out, out) is det.

	% graph__arc_contents(Graph, Arc, Start, End, ArcInfo) takes a
	% graph Graph and an arc Arc and returns the start and end nodes
	% and the information stored in that arc.
	%
:- pred graph__arc_contents(graph(N, A), arc(A), node(N), node(N), A).
:- mode graph__arc_contents(in, in, out, out, out) is det.

	% graph__path(Graph, Start, End, Path) is true iff there is a path
	% from the node Start to the node End in Graph that goes through
	% the sequence of arcs Arcs.
	% The algorithm will return paths containing at most one cycle.
	%
:- pred graph__path(graph(N, A), node(N), node(N), list(arc(A))).
:- mode graph__path(in, in, in, out) is nondet.
:- mode graph__path(in, in, out, out) is nondet.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, int, std_util, list.
:- import_module require.

:- type graph(N, A)	--->
		graph(
			graph__node_supply,
			graph__arc_supply,
			map(node(N), N),
			map(arc(A), arc_info(N, A)),
			map(node(N), map(arc(A), node(N)))
		).

:- type graph__node_supply	==	int.

:- type graph__arc_supply	==	int.

:- type node(N)			--->	node(int).

:- type arc(A)			--->	arc(int).

:- type arc_info(N, A)	--->	arc_info(node(N), node(N), A).

%------------------------------------------------------------------------------%

graph__init(Graph) :-
	Graph = graph(0, 0, Nodes, Arcs, Edges),
	map__init(Nodes),
	map__init(Arcs),
	map__init(Edges).

%------------------------------------------------------------------------------%

graph__set_node(G0, NInfo, node(N), G) :-
	graph__get_node_supply(G0, NS0),
	NS = NS0 + 1,
	N = NS,
	graph__set_node_supply(G0, NS, G1),

	graph__get_nodes(G1, Nodes0),
	map__set(Nodes0, node(N), NInfo, Nodes),
	graph__set_nodes(G1, Nodes, G2),

	graph__get_edges(G2, Edges0),
	map__init(EdgeMap),
	map__set(Edges0, node(N), EdgeMap, Edges),
	graph__set_edges(G2, Edges, G).

graph__det_insert_node(G0, NInfo, N, G) :-
	(
		graph__insert_node(G0, NInfo, N1, G1)
	->
		N = N1,
		G = G1
	;
		error("graph__det_insert_node: node already exists.")
	).

graph__insert_node(G0, NInfo, node(N), G) :-
		% Make sure that the graph doesn't contain
		% NInfo already.
	graph__get_nodes(G0, Nodes0),
	\+ map__member(Nodes0, _, NInfo),

	graph__get_node_supply(G0, NS0),
	NS = NS0 + 1,
	N = NS,
	graph__set_node_supply(G0, NS, G1),

	graph__get_nodes(G1, Nodes1),
	map__set(Nodes1, node(N), NInfo, Nodes),
	graph__set_nodes(G1, Nodes, G2),

	graph__get_edges(G2, Edges0),
	map__init(EdgeSet),
	map__set(Edges0, node(N), EdgeSet, Edges),
	graph__set_edges(G2, Edges, G).

%------------------------------------------------------------------------------%

graph__search_node(Graph, NodeInfo, Node) :-
	graph__get_nodes(Graph, NodeTable),
	map__member(NodeTable, Node, NodeInfo).

%------------------------------------------------------------------------------%

graph__find_matching_nodes(Graph, NodeInfo, NodeSet) :-
	graph__get_nodes(Graph, NodeTable),
%	SolnGoal = lambda([Node::out] is nondet,
%			map__member(NodeTable, Node, NodeInfo)),
%	solutions(SolnGoal, NodeList),
	solutions(graph__select_node(NodeTable, NodeInfo), NodeList),
	set__sorted_list_to_set(NodeList, NodeSet).

:- pred graph__select_node(map(node(N), N), N, node(N)).
:- mode graph__select_node(in, in, out) is nondet.

graph__select_node(NodeTable, NodeInfo, Node) :-
	map__member(NodeTable, Node, NodeInfo).

%------------------------------------------------------------------------------%

graph__node_contents(G, N, I) :-
	graph__get_nodes(G, Ns),
	map__lookup(Ns, N, I).

%------------------------------------------------------------------------------%

graph__successors(G, N, Ss) :-
	graph__get_edges(G, Es),
	map__lookup(Es, N, E),
	map__values(E, SsList),
	set__list_to_set(SsList, Ss).

%------------------------------------------------------------------------------%

graph__nodes(G, Ns) :-
	graph__get_nodes(G, Ns0),
	map__keys(Ns0, Ns1),
	set__list_to_set(Ns1, Ns).

%------------------------------------------------------------------------------%

graph__set_edge(G0, Start, End, Info, Arc, G) :-
	graph__get_arc_supply(G0, AS0),
	AS = AS0 + 1,
	Arc = arc(AS),
	graph__set_arc_supply(G0, AS, G1),

	graph__get_arcs(G1, Arcs0),
	map__set(Arcs0, Arc, arc_info(Start, End, Info), Arcs),
	graph__set_arcs(G1, Arcs, G2),

	graph__get_edges(G2, Es0),
	map__lookup(Es0, Start, EdgeMap0),
	map__set(EdgeMap0, Arc, End, EdgeMap),
	map__set(Es0, Start, EdgeMap, Es),
	graph__set_edges(G2, Es, G).

%------------------------------------------------------------------------------%

graph__det_insert_edge(G0, Start, End, Info, Arc, G) :-
	(
		graph__insert_edge(G0, Start, End, Info, Arc1, G1)
	->
		Arc = Arc1,
		G = G1
	;
		error("graph__det_insert_edge: this edge is already in the graph.")
	).

graph__insert_edge(G0, Start, End, Info, Arc, G) :-
	graph__get_arc_supply(G0, AS0),
	AS = AS0 + 1,
	Arc = arc(AS),
	graph__set_arc_supply(G0, AS, G1),

	graph__get_arcs(G1, Arcs0),
	map__insert(Arcs0, Arc, arc_info(Start, End, Info), Arcs),
	graph__set_arcs(G1, Arcs, G2),

	graph__get_edges(G2, Es0),
	map__lookup(Es0, Start, EdgeMap0),
	map__set(EdgeMap0, Arc, End, EdgeMap),
	map__set(Es0, Start, EdgeMap, Es),
	graph__set_edges(G2, Es, G).

%------------------------------------------------------------------------------%

graph__arc_contents(G, N, S, E, A) :-
	graph__get_arcs(G, Ns),
	map__lookup(Ns, N, I),
	I = arc_info(S, E, A).

%------------------------------------------------------------------------------%

graph__path(G, S, E, Path) :-
	graph__path_2(G, S, E, [], Path).

:- pred graph__path_2(graph(N, A), node(N), node(N),
				list(node(N)), list(arc(A))).
:- mode graph__path_2(in, in, in, in, out) is nondet.
:- mode graph__path_2(in, in, out, in, out) is nondet.

graph__path_2(G, S, E, Nodes0, Path) :-
	graph__get_edges(G, Es),
	map__lookup(Es, S, Arcs),
	(
		map__member(Arcs, A, E),
		\+ list__member(E, Nodes0),
		Path = [A]
	;
		map__member(Arcs, A, N),
		\+ list__member(N, Nodes0),
		graph__path_2(G, N, E, [N|Nodes0], Path0),
		Path = [A|Path0]
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- pred graph__get_node_supply(graph(N, A), graph__node_supply).
:- mode graph__get_node_supply(in, out) is det.

graph__get_node_supply(G, NS) :-
	G = graph(NS, _AS, _N, _A, _E).

:- pred graph__get_arc_supply(graph(N, A), graph__arc_supply).
:- mode graph__get_arc_supply(in, out) is det.

graph__get_arc_supply(G, AS) :-
	G = graph(_NS, AS, _N, _A, _E).

:- pred graph__get_nodes(graph(N, A), map(node(N), N)).
:- mode graph__get_nodes(in, out) is det.

graph__get_nodes(G, N) :-
	G = graph(_NS, _AS, N, _A, _E).

:- pred graph__get_arcs(graph(N, A), map(arc(A), arc_info(N, A))).
:- mode graph__get_arcs(in, out) is det.

graph__get_arcs(G, A) :-
	G = graph(_NS, _AS, _N, A, _E).

:- pred graph__get_edges(graph(N, A), map(node(N), map(arc(A), node(N)))).
:- mode graph__get_edges(in, out) is det.

graph__get_edges(G, E) :-
	G = graph(_NS, _AS, _N, _A, E).

:- pred graph__set_node_supply(graph(N, A), graph__node_supply, graph(N, A)).
:- mode graph__set_node_supply(in, in, out) is det.

graph__set_node_supply(G0, NS, G) :-
	G0 = graph(_, AS, N, A, E),
	G = graph(NS, AS, N, A, E).

:- pred graph__set_arc_supply(graph(N, A), graph__arc_supply, graph(N, A)).
:- mode graph__set_arc_supply(in, in, out) is det.

graph__set_arc_supply(G0, AS, G) :-
	G0 = graph(NS, _, N, A, E),
	G = graph(NS, AS, N, A, E).

:- pred graph__set_nodes(graph(N, A), map(node(N), N), graph(N, A)).
:- mode graph__set_nodes(in, in, out) is det.

graph__set_nodes(G0, N, G) :-
	G0 = graph(NS, AS, _, A, E),
	G = graph(NS, AS, N, A, E).

:- pred graph__set_arcs(graph(N, A), map(arc(A), arc_info(N, A)), graph(N, A)).
:- mode graph__set_arcs(in, in, out) is det.

graph__set_arcs(G0, A, G) :-
	G0 = graph(NS, AS, N, _, E),
	G = graph(NS, AS, N, A, E).

:- pred graph__set_edges(graph(N, A), map(node(N), map(arc(A), node(N))), graph(N, A)).
:- mode graph__set_edges(in, in, out) is det.

graph__set_edges(G0, E, G) :-
	G0 = graph(NS, AS, N, A, _),
	G = graph(NS, AS, N, A, E).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%       Functional forms added.

graph__init = G :-
	graph__init(G).

graph__find_matching_nodes(G, N) = S :-
	graph__find_matching_nodes(G, N, S).

graph__node_contents(G, N) = NI :-
	graph__node_contents(G, N, NI).

graph__successors(G, N) = S :-
	graph__successors(G, N, S).

graph__nodes(G) = S :-
	graph__nodes(G,S).

