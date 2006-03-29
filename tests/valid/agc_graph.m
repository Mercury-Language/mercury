%
% Regression test.
%
% Name: agc_graph.m
%
% Description of bug:
% 	The liveness of polymorphic type variables for accurate gc
% 	wasn't being computed correctly.
% 	It was being removed from the initial_liveness for not being
% 	part of the non-locals (like an unused variable).
%
% Symptom(s) of bug:
% 	% Allocating storage locations for live vars in predicate
% 		`agc_graph:insert_node/4' mode 0
% 	Software error: variable TypeInfo_for_A (18) not found
%
% Date bug existed: 1-July-1997
%
% Author: trd
%
% Note: This code was taken from library/graph.m and simplified.
%

:- module agc_graph.

:- interface.
:- import_module unit.

:- type graph(N, A).

:- type node(N).

:- type arc(A).

:- type graph(N)	== graph(N, unit).

:- type arc		== arc(unit).

:- pred agc_graph__insert_node(graph(N, A), N, node(N), graph(N, A)).
:- mode agc_graph__insert_node(in, in, out, out) is semidet.

%------------------------------------------------------------------------------%

:- type node(N)			==	int.

:- type arc(A)			==	int.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, int, std_util, list.
:- import_module require.

:- type graph(N, A)	--->
		graph(
			agc_graph__node_supply,
			agc_graph__arc_supply,
			map(node(N), N),
			map(arc(A), arc_info(N, A)),
			map(node(N), map(arc(A), node(N)))
		).

:- type agc_graph__node_supply	==	int.

:- type agc_graph__arc_supply	==	int.

:- type arc_info(N, A)	--->	arc_info(node(N), node(N), A).

%------------------------------------------------------------------------------%

agc_graph__insert_node(G, NInfo, N, G) :-
	G = graph(_, _, Nodes0, _, _),

	agc_graph__get_node_supply(G, N),
	agc_graph__get_edges(G, Edges0),

	\+ map__member(Nodes0, _, NInfo),
	map__init(Edges0).

%------------------------------------------------------------------------------%

:- pred agc_graph__get_node_supply(graph(N, A), agc_graph__node_supply).
:- mode agc_graph__get_node_supply(in, out) is det.

agc_graph__get_node_supply(G, NS) :-
	G = graph(NS, _AS, _N, _A, _E).

:- pred agc_graph__get_edges(graph(N, A), map(node(N), map(arc(A), node(N)))).
:- mode agc_graph__get_edges(in, out) is det.

agc_graph__get_edges(G, E) :-
	G = graph(_NS, _AS, _N, _A, E).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
