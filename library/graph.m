%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: graph.nl. 
% Main author: fjh.
%
% This file provides facilities for manipulating graphs.
% A graph is a set of nodes, each of which may contain a value of
% any type, and which are identified by node_ids.
%
% Currently graphs are implemented as maps from node_ids to values,
% where node_ids are just integers, but we should re-implement this
% for unique graphs using addresses as ids.
%
% Graphs may be used to implement cyclic data structures such as
% circular linked lists, etc.
%
% Theoretical problem: we would like to allow heterogenous graphs,
% with types graph and node_id(T) instead of graph(T) and node_id(T).
% This would be completely type-safe as far as the user of the graphs
% is concerned, but it isn't possible to implement it except as a builtin.
% Graphs are a pretty fundamental type, so it might make sense to do so,
% but I'm still not sure whether this would cause any theoretical problems.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module graph.
:- interface.
:- export_pred	graph__init, graph__new_node, graph__set_node,
		graph__lookup_node.

% Axioms:
%	% (also determinism of graph__init, graph__new_node, graph__set_node)
%
%	all [G,V,N]
%	    (
%		graph__new_node(G, V, N) =>
%		    (
%			graph__lookup(G, N, V),
%			all [V2] graph_lookup(G, N, V2) => V2 = V
%		    )
%	    ).
%	all [G,V,N]
%	    (
%		graph__new_node(G, V, N) =>
%		    (
%			graph__lookup(G, N, V),
%			all [V2] graph_lookup(G, N, V2) => V2 = V
%		    )
%	    ).

%			graph__lookup(G, N, V2)
%	    =>
%		V = V2
%	    ),
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, map.

:- type node_id(_T)	==	integer.

:- type graph(T)	--->	graph(node_id(T), map(node_id(T), T)).

%-----------------------------------------------------------------------------%

	% initialize a graph
:- pred graph__init(graph(_T)).
:- mode graph__init(output).
graph__init(graph(0, Nodes)) :-
	map__init(Nodes).

%-----------------------------------------------------------------------------%

	% create a new node with the specified value
:- pred graph__new_node(graph(T), T, node_id(T), graph(T)).
:- mode graph__new_node(input, input, output, output).
graph__new_node(graph(MaxId0, Nodes0), Val, MaxId0, graph(MaxId, Nodes)) :-
	MaxId is MaxId0 + 1,
	map__set(Nodes0, MaxId0, Val, Nodes).

%-----------------------------------------------------------------------------%

	% replace the value stored in a given node
:- pred graph__set_node(graph(T), node_id(T), T, graph(T)).
:- mode graph__set_node(input, input, input, output).
graph__set_node(graph(MaxId, Nodes0), Id, Val, graph(MaxId, Nodes)) :-
	map__set(Nodes0, Id, Val, Nodes).

%-----------------------------------------------------------------------------%

	% lookup the value stored in a given node
:- pred graph__lookup_node(graph(T), node_id(T), T).
:- mode graph__lookup_node(input, input, output).
graph__lookup_node(graph(_, Nodes), Id, Val) :-
	map__search(Nodes, Id, Val).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
