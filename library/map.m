%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% This file provides the 'map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key,Data) pairs which allows you to look up any Data item given the
% Key.
%
% The implementation is using balanced binary trees, as provided by
% bintree.nl.  Virtually all the predicates in this file just
% forward the work to the corresponding predicate in bintree.nl.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module map.
:- interface.
:- import_module list, std_util.

%-----------------------------------------------------------------------------%

:- type map(_K, _V).

%-----------------------------------------------------------------------------%

	% Initialize an empty map.
:- pred map__init(map(_,_)).
:- mode map__init(output).

	% Check whether map contains key
:- pred map__contains(map(K,_V), K).
:- mode map__contains(input, input).

	% Search map for key.
:- pred map__search(map(K,V), K, V).
:- mode map__search(input, input, output).

	% Search map for data.
:- pred map__inverse_search(map(K,V), V, K).
:- mode map__inverse_search(input, input, output).

	% Insert a new key and corresponding value into a map.
:- pred map__insert(map(K,V), K, V, map(K,V)).
:- mode map__insert(input, input, input, output).

	% Insert a new key and corresponding value into a map,
	% or unify value with existing value in map.
:- pred map__search_insert(map(K,V), K, V, map(K,V)).
:- mode map__search_insert(input, input, input, output).

	% Update the value corresponding to a given key
:- pred map__update(map(K,V), K, V, map(K,V)).
:- mode map__update(input, input, input, output).

	% Update value if it's already there, otherwise insert it
:- pred map__set(map(K,V), K, V, map(K,V)).
:- mode map__set(input, input, input, output).

	% Given a map, return a list of all the keys in the map
:- pred map__keys(map(K, _V), list(K)).
:- mode map__keys(input, output).

	% convert a map to an associate list
:- pred map__to_assoc_list(map(K,V), list(pair(K,V))).
:- mode map__to_assoc_list(input, output).

	% convert a map to an associate list
:- pred map__from_assoc_list(list(pair(K,V)), map(K,V)).
:- mode map__from_assoc_list(input, output).

	% delete a key-value pair from a map
:- pred map__delete(map(K,V), K, map(K,V)).
:- mode map__delete(input, input, output).

:- pred map__from_corresponding_lists(list(K), list(V), map(K, V)).
:- mode map__from_corresponding_lists(input, input, output).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bintree.

%-----------------------------------------------------------------------------%

:- type map(K,V)	==	bintree(K,V).

%-----------------------------------------------------------------------------%

map__init(M) :-
	bintree__init(M).

map__contains(Map, K) :-
	some [V] map__search(Map, K, V).

map__search(Map, K, V) :-
	bintree__search(Map, K, V).

map__insert(Map0, K, V, Map) :-
	bintree__insert(Map0, K, V, Map).
 
map__search_insert(Map0, K, V, Map) :-
	bintree__search_insert(Map0, K, V, Map).

map__update(Map0, K, V, Map) :-
	bintree__update(Map0, K, V, Map).

map__set(Map0, K, V, Map) :-
	bintree__set(Map0, K, V, Map).

map__keys(Map, KeyList) :-
	bintree__keys(Map, KeyList).

map__to_assoc_list(M, L) :-
	bintree__to_list(M, L).

map__from_assoc_list(L, M) :-
	bintree__from_list(L, M).

map__delete(Map0, Key, Map) :-
	bintree__delete(Map0, Key, Map).

%-----------------------------------------------------------------------------%

	% XXX innefficient

map__inverse_search(Map, V, K) :-
	bintree__to_list(Map, AssocList),
	assoc_list_member(K-V, AssocList).

%-----------------------------------------------------------------------------%

	% This is just a version of member/2 with a complicated mode.
	% The reason we don't just use member/2 is that we want to
	% bootstrap this thing before we implement polymorphic modes.

:- pred assoc_list_member(pair(K,V), list(pair(K,V))).
:- mode assoc_list_member(bound(ground - free) -> ground, input).
:- mode assoc_list_member(bound(free - ground) -> ground, input).
:- mode assoc_list_member(bound(free - free) -> ground, input).
assoc_list_member(X, [X|_]).
assoc_list_member(X, [_|Xs]) :-
	assoc_list_member(X, Xs).

%-----------------------------------------------------------------------------%

map__from_corresponding_lists(Keys, Values, Map) :-
	bintree__from_corresponding_lists(Keys, Values, Map).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
