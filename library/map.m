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
:- export_pred	map__init/1, map__search/3, map__search_insert/4,
		map__update/4, map__set/3, map__keys/2, map__to_assoc_list/2,
		map__contains/2, map__inverse_search/3, map__member/3.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bintree, list, std_util.

:- type map(K,V)	==	bintree(K,V).

%-----------------------------------------------------------------------------%

	% Initialize an empty map.
:- pred map__init(map(_,_)).
:- mode map__init(output).
map__init(M) :- bintree__init(M).

%-----------------------------------------------------------------------------%

	% Check whether map contains key
:- pred map__contains(map(K,_V), K).
:- mode map__contains(input, input).
map__contains(Map, K) :-
	some [V] map__search(Map, K, V).

%-----------------------------------------------------------------------------%

	% Search map for key.
:- pred map__search(map(K,V), K, V).
:- mode map__search(input, input, output).
map__search(Map, K, V) :-
	bintree__search(Map, K, V).

%-----------------------------------------------------------------------------%

	% Search map for data.
	% XXX innefficient

:- pred map__inverse_search(map(K,V), V, K).
:- mode map__inverse_search(input, input, output).
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

	% Insert a new key and corresponding value into a map.

:- pred map__insert(map(K,V), K, V, map(K,V)).
:- mode map__insert(input, input, input, output).
map__insert(Map0, K, V, Map) :-
	bintree__insert(Map0, K, V, Map).
 
%-----------------------------------------------------------------------------%

:- pred map__search_insert(map(K,V), K, V, map(K,V)).
:- mode map__search_insert(input, input, input, output).
map__search_insert(Map0, K, V, Map) :-
	bintree__search_insert(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

	% Update the value corresponding to a given key

:- pred map__update(map(K,V), K, V, map(K,V)).
:- mode map__update(input, input, input, output).
map__update(Map0, K, V, Map) :-
	bintree__update(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

	% Update value if it's already there, otherwise insert it

:- pred map__set(map(K,V), K, V, map(K,V)).
:- mode map__set(input, input, input, output).
map__set(Map0, K, V, Map) :-
	bintree__set(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

:- pred map__keys(map(K, _V), list(K)).
:- mode map__keys(input, output).

map__keys(Map, KeyList) :-
	bintree__keys(Map, KeyList).

%-----------------------------------------------------------------------------%

	% convert a map to an associate list

:- pred map__to_assoc_list(map(K,V), list(pair(K,V))).
:- mode map__to_assoc_list(input, output).

map__to_assoc_list(M, L) :-
	bintree__to_list(M, L).

%-----------------------------------------------------------------------------%

	% delete a key-value pair from a map

:- pred map__delete(map(K,V), K, map(K,V)).
:- mode map__delete(input, input, output).

map__delete(Map0, Key, Map) :-
	bintree__delete(Map0, Key, Map).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
