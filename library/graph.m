%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% File: graph.m.
% Main author: conway.
% Stability: low.
%
% This module defines a directed graph data type.
%
% graph__insert_node is O(log N) for N nodes.
% graph__insert_edge is O(log N+log A) for N nodes and A arcs.
% graph__path(in, in, in) is O(log N) + O(set__member) for N nodes.
%	The cost of the set__member operation is in the number of
%	edges leaving the node.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module graph.

:- interface.
:- import_module set, std_util.

:- type graph(N, A).

:- type graph(N)	== graph(N, unit).

:- type node(N).

:- type arc(A).

:- type arc		== arc(unit).

:- pred graph__init(graph(N, A)).
:- mode graph__init(out) is det.

:- pred graph__set_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__set_node(in, in, out, out) is det.

:- pred graph__det_insert_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__det_insert_node(in, in, out, out) is det.

:- pred graph__insert_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__insert_node(in, in, out, out) is semidet.

:- pred graph__lookup_node(graph(N, A), N, node(N)).
:- mode graph__lookup_node(in, in, out) is det.

:- pred graph__search_node(graph(N, A), N, node(N)).
:- mode graph__search_node(in, in, out) is semidet.

:- pred graph__node_contents(graph(N, A), node(N), N).
:- mode graph__node_contents(in, in, out) is det.

:- pred graph__successors(graph(N, A), node(N), set(node(N))).
:- mode graph__successors(in, in, out) is det.

:- pred graph__nodes(graph(N, A), set(node(N))).
:- mode graph__nodes(in, out) is det.

:- pred graph__set_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__set_edge(in, in, in, in, out, out) is det.

:- pred graph__det_insert_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__det_insert_edge(in, in, in, in, out, out) is det.

:- pred graph__insert_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__insert_edge(in, in, in, in, out, out) is semidet.

:- pred graph__edge_contents(graph(N, A), arc(A), node(N), node(N), A).
:- mode graph__edge_contents(in, in, out, out, out) is det.

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

:- type node(N)			==	int.

:- type arc(A)			==	int.

:- type arc_info(N, A)	--->	arc_info(node(N), node(N), A).

%------------------------------------------------------------------------------%

graph__init(Graph) :-
	Graph = graph(0, 0, Nodes, Arcs, Edges),
	map__init(Nodes),
	map__init(Arcs),
	map__init(Edges).

%------------------------------------------------------------------------------%

graph__set_node(G0, NInfo, N, G) :-
	graph__get_node_supply(G0, NS0),
	NS is NS0 + 1,
	N = NS,
	graph__set_node_supply(G0, NS, G1),

	graph__get_nodes(G1, Nodes0),
	map__set(Nodes0, N, NInfo, Nodes),
	graph__set_nodes(G1, Nodes, G2),

	graph__get_edges(G2, Edges0),
	map__init(EdgeMap),
	map__set(Edges0, N, EdgeMap, Edges),
	graph__set_edges(G2, Edges, G).

graph__det_insert_node(G0, NInfo, N, G) :-
	graph__set_node(G0, NInfo, N, G).

graph__insert_node(G0, NInfo, N, G) :-
	graph__get_node_supply(G0, NS0),
	NS is NS0 + 1,
	N = NS,
	graph__set_node_supply(G0, NS, G1),

	graph__get_nodes(G1, Nodes0),
	map__insert(Nodes0, N, NInfo, Nodes),
	graph__set_nodes(G1, Nodes, G2),

	graph__get_edges(G2, Edges0),
	map__init(EdgeSet),
	map__set(Edges0, N, EdgeSet, Edges),
	graph__set_edges(G2, Edges, G).

%------------------------------------------------------------------------------%

graph__lookup_node(G, N, I) :-
	(
		graph__search_node(G, N, I0)
	->
		I = I0
	;
		error("graph__lookup_node: Node not found.")
	).

%------------------------------------------------------------------------------%

graph__search_node(G, N, I) :-
	graph__get_nodes(G, Ns),
	map__to_assoc_list(Ns, List),
	graph__search_node_2(List, N, I).

:- pred graph__search_node_2(assoc_list(node(N), N), N, node(N)).
:- mode graph__search_node_2(in, in, out) is semidet.

graph__search_node_2([I0 - N0|Rest], N, I) :-
	(
		N0 = N
	->
		I = I0
	;
		graph__search_node_2(Rest, N, I)
	).

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
	AS is AS0 + 1,
	Arc = AS,
	graph__set_arc_supply(G0, AS, G1),

	graph__get_arcs(G1, Arcs0),
	map__set(Arcs0, Arc, arc_info(Start, End, Info), Arcs),
	graph__set_arcs(G1, Arcs, G2),

	graph__get_edges(G2, Es0),
	map__lookup(Es0, Start, EdgeMap0),
	map__set(EdgeMap0, Arc, End, EdgeMap),
	map__set(Es0, Start, EdgeMap, Es),
	graph__set_edges(G2, Es, G).

graph__det_insert_edge(G0, Start, End, Info, Arc, G) :-
	graph__set_edge(G0, Start, End, Info, Arc, G).

%------------------------------------------------------------------------------%

graph__insert_edge(G0, Start, End, Info, Arc, G) :-
	graph__get_arc_supply(G0, AS0),
	AS is AS0 + 1,
	Arc = AS,
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

graph__edge_contents(G, N, S, E, A) :-
	graph__get_arcs(G, Ns),
	map__lookup(Ns, N, I),
	I = arc_info(S, E, A).

%------------------------------------------------------------------------------%

graph__path(G, S, E, Path) :-
	graph__get_edges(G, Es),
	map__lookup(Es, S, Arcs),
	(
		map__member(Arcs, A, E),
		Path = [A]
	;
		map__member(Arcs, A, N),
		graph__path(G, N, E, Path0),
		Path = [A|Path0]
	).

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

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
