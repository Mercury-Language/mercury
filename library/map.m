%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% This file provides the 'map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key,Data) pairs which allows you to look up any Data item given the
% Key.
%
% The implementation is using an association list, which gives
% O(1) insert but O(n) update and search. Should provide a more
% efficient implementation someday...
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module map.
:- import_module list.
:- export_pred	map__init/1, map__search/3, map__search_insert/4,
		map__update/4, map__set/3, map__keys/2, map__to_assoc_list/2.
:- export_type	map__pair.

%-----------------------------------------------------------------------------%

:- type map__pair(K,V)	--->	K - V.	% pair is a system type in NU-Prolog.

:- type map(K,V)	==	list(map__pair(K,V)).

%	where
%	(L : all [K]
%		if some [V1,R] delete(K-V1,L,R) then
%			not some V member(K-V,R)).
%

:- type found(X)	--->	found(X) ; not_found.

%-----------------------------------------------------------------------------%

	% Initialize an empty map.
:- pred map__init(map(_,_)).
:- mode map__init(output).
map__init([]).

%-----------------------------------------------------------------------------%

	% Search map for key.
:- pred map__search(map(K,V), K, V).
:- mode map__search(input, input, output).
map__search(Map, K, V) :-
	member(K-V, Map).

%-----------------------------------------------------------------------------%

	% Insert a new key and corresponding value into a map.
:- pred map__insert(map(K,V), K, V, map(K,V)).
:- mode map__insert(input, input, input, output).
map__insert(Map, K, V, (K - V).Map).
 
%-----------------------------------------------------------------------------%

 	% Search map for key. If found, unify old value with new val.
	% otherwise insert key and new val.
:- pred map__search_insert(map(K,V), K, V, map(K,V)).
:- mode map__search_insert(input, input, input, output).
map__search_insert(Map0, K, V, Map) :-
 	(if some [Val] map__search(Map0, K, Val) then
 		V = Val,
		Map = Map0
	else
 		map__insert(Map0, K, V, Map)
	).

%-----------------------------------------------------------------------------%

	% Update the value corresponding to a given key
:- pred map__update(map(K,V), K, V, map(K,V)).
:- mode map__update(input, input, input, output).
map__update((K - _).Map, K, V, (K - V).Map).
map__update(KV.Map0, K, V, KV.Map) :-
	map__update(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

	% Update value if it's already there, otherwise insert it
:- pred map__set(map(K,V), K, V, map(K,V)).
:- mode map__set(input, input, input, output).
map__set(Map0, K, V, Map) :-
	(if some [Map1] map__update(Map0, K, V, Map1) then
		Map = Map1
	else
		map__insert(Map0, K, V, Map)
	).

:- pred map__keys(map(K, V), list(K)).
:- mode map__keys(input, output).
map__keys(Map, KeyList) :-
	findall(K, member(K-V, Map), KeyList).

%-----------------------------------------------------------------------------%

	% convert a map to an associate list

:- pred map__to_assoc_list(map(K,V), list(map_pair(K,V))).
:- mode map__to_assoc_list(input, output).
map__to_assoc_list(M, M).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
