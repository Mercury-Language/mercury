%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
%
% file: graph.nl
%
% main author: conway.
%
% This module defines a directed graph data type.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module graph.

:- interface.

:- type graph(N, A).

:- type node(N).

:- type arc(A).

:- pred graph__init(graph(N, A)).
:- mode graph__init(out) is det.

:- pred graph__insert_node(graph(N, A), N, node(N), graph(N, A)).
:- mode graph__insert_node(in, in, out, out) is det.

:- pred graph__insert_edge(graph(N, A), node(N), node(N), A,
						arc(A), graph(N, A)).
:- mode graph__insert_edge(in, in, in, in, out, out) is det.

:- pred graph__path(graph(N, A), node(N), node(N)).
:- mode graph__path(in, in, in) is semidet.
:- mode graph__path(in, in, out) is nondet.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, set, int.

:- type graph(N, A)	--->
		graph(
			graph__node_supply,
			graph__arc_supply,
			map(node(N), N),
			map(arc(A), arc_info(N, A)),
			map(node(N), set(edge(N, A)))
		).

:- type graph__node_supply	==	int.

:- type graph__arc_supply	==	int.

:- type node(N)			==	int.

:- type arc(A)			==	int.

:- type edge(N, A)	--->	edge(arc(A), node(N)).

:- type arc_info(N, A)	--->	arc_info(node(N), node(N), A).

%------------------------------------------------------------------------------%

graph__init(Graph) :-
	Graph = graph(0, 0, Nodes, Arcs, Edges),
	map__init(Nodes),
	map__init(Arcs),
	map__init(Edges).

%------------------------------------------------------------------------------%

graph__insert_node(G0, NInfo, N, G) :-
	graph__get_node_supply(G0, NS0),
	NS is NS0 + 1,
	N = NS,
	graph__set_node_supply(G0, NS, G1),

	graph__get_nodes(G1, Nodes0),
	map__set(Nodes0, N, NInfo, Nodes),
	graph__set_nodes(G1, Nodes, G2),

	graph__get_edges(G2, Edges0),
	set__init(EdgeSet),
	map__set(Edges0, N, EdgeSet, Edges),
	graph__set_edges(G2, Edges, G).

%------------------------------------------------------------------------------%

graph__insert_edge(G0, Start, End, Info, Arc, G) :-
	graph__get_arc_supply(G0, AS0),
	AS is AS0 + 1,
	Arc = AS,
	graph__set_arc_supply(G0, AS, G1),

	graph__get_arcs(G1, Arcs0),
	map__set(Arcs0, Arc, arc_info(Start, End, Info), Arcs),
	graph__set_arcs(G1, Arcs, G2),

	graph__get_edges(G2, Es0),
	map__lookup(Es0, Start, EdgeSet0),
	set__insert(EdgeSet0, edge(Arc, End), EdgeSet),
	map__set(Es0, Start, EdgeSet, Es),
	graph__set_edges(G2, Es, G).

%------------------------------------------------------------------------------%

graph__path(G, S, E) :-
	graph__get_edges(G, Es),
	map__lookup(Es, S, Arcs),
	(
		set__member(edge(_, E), Arcs)
	;
		set__member(edge(_, N), Arcs),
		graph__path(G, N, E)
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

:- pred graph__get_edges(graph(N, A), map(node(N), set(edge(N, A)))).
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

:- pred graph__set_edges(graph(N, A), map(node(N), set(edge(N, A))), graph(N, A)).
:- mode graph__set_edges(in, in, out) is det.

graph__set_edges(G0, E, G) :-
	G0 = graph(NS, AS, N, A, _),
	G = graph(NS, AS, N, A, E).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
