%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: store.m. 
% Main author: fjh.
% Stability: low.
%
% This module is not stable: it is highly likely that the interface
% will change in future releases.  For the time being, we recommend
% that you do not use this module.
%
% This file provides facilities for manipulating stores.
% A store is a set of nodes, each of which may contain a value of
% any type, and which are identified by node_ids.
%
% Currently stores are implemented as maps from node_ids to values,
% where node_ids are just integers, but we should re-implement this
% for unique stores using addresses as ids.
%
% Graphs may be used to implement cyclic data structures such as
% circular linked lists, etc.
%
% Theoretical problem: we would like to allow heterogenous stores,
% with types store and node_id(T) instead of store(T) and node_id(T).
% This would be completely type-safe as far as the user of the stores
% is concerned, but it isn't possible to implement it except as a builtin.
% Stores are a pretty fundamental type, so it might make sense to do so,
% but I'm still not sure whether this would cause any theoretical problems.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module store.
:- interface.

:- type store(T).
:- type node_id(T).

	% initialize a store
:- pred store__init(store(_T)).
:- mode store__init(out) is det.

	% create a new node with the specified value
:- pred store__new_node(store(T), T, node_id(T), store(T)).
:- mode store__new_node(in, in, out, out) is det.

	% replace the value stored in a given node
:- pred store__set_node(store(T), node_id(T), T, store(T)).
:- mode store__set_node(in, in, in, out) is det.

	% lookup the value stored in a given node
:- pred store__lookup_node(store(T), node_id(T), T).
:- mode store__lookup_node(in, in, out) is det.

% Axioms:
%	% (also determinism of store__init, store__new_node, store__set_node)
%
%	all [G,V,N]
%	    (
%		store__new_node(G, V, N) =>
%		    (
%			store__lookup(G, N, V),
%			all [V2] store_lookup(G, N, V2) => V2 = V
%		    )
%	    ).
%	all [G,V,N]
%	    (
%		store__new_node(G, V, N) =>
%		    (
%			store__lookup(G, N, V),
%			all [V2] store_lookup(G, N, V2) => V2 = V
%		    )
%	    ).
%  etc.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, map.

:- type node_id(_T)	==	int.

:- type store(T)	--->	store(node_id(T), map(node_id(T), T)).

%-----------------------------------------------------------------------------%

	% initialize a store
store__init(store(0, Nodes)) :-
	map__init(Nodes).

%-----------------------------------------------------------------------------%

	% create a new node with the specified value
store__new_node(store(MaxId0, Nodes0), Val, MaxId0, store(MaxId, Nodes)) :-
	MaxId is MaxId0 + 1,
	map__set(Nodes0, MaxId0, Val, Nodes).

%-----------------------------------------------------------------------------%

	% replace the value stored in a given node
store__set_node(store(MaxId, Nodes0), Id, Val, store(MaxId, Nodes)) :-
	map__set(Nodes0, Id, Val, Nodes).

%-----------------------------------------------------------------------------%

	% lookup the value stored in a given node
store__lookup_node(store(_, Nodes), Id, Val) :-
	map__lookup(Nodes, Id, Val).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
