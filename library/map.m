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
% The implementation is using an association list, which gives
% O(1) insert but O(n) update and search. Should provide a more
% efficient implementation someday...
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module map.
:- import_module list.
:- export_pred	map__init/1, map__search/3, map__search_insert/4,
		map__update/4, map__set/3, map__keys/2, map__to_assoc_list/2,
		map__contains/2, map__inverse_search/3, map__member/3.
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

	% Check whether map contains key
:- pred map__contains(map(K,_V), K).
:- mode map__contains(input, input).
map__contains(Map, K) :-
	some [V] assoc_list_member(K-V, Map).

%-----------------------------------------------------------------------------%

	% Search map for key.
:- pred map__search(map(K,V), K, V).
:- mode map__search(input, input, output).
map__search(Map, K, V) :-
	assoc_list_member(K-V, Map).

%-----------------------------------------------------------------------------%

	% Search map for data.
	% This could be just a different mode of map__search, but
	% with some data structures that would require mode-dependant
	% code, and we don't want to rely on that feature (at least
	% until we've implemented it) so we can bootstrap.

:- pred map__inverse_search(map(K,V), V, K).
:- mode map__inverse_search(input, input, output).
map__inverse_search(Map, V, K) :-
	assoc_list_member(K-V, Map).

%-----------------------------------------------------------------------------%

	% Nondeterministically return all the different key-value
	% pairs contained in the map.
	% The same as map__search except with a different mode.

:- pred map__member(map(K,V), K, V).
:- mode map__member(input, output, output).
map__member(Map, K, V) :-
	assoc_list_member(K-V, Map).

%-----------------------------------------------------------------------------%

	% This is just a version of member/2 with a complicated mode.
	% The reason we don't just use member/2 is that we want to
	% bootstrap this thing before we implement polymorphic modes.

:- pred assoc_list_member(map__pair(K,V), list(map__pair(K,V))).
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

:- pred map__keys(map(K, _V), list(K)).
:- mode map__keys(input, output).
map__keys(Map, KeyList) :-
	map__keys_2(Map, [], KeyList).

:- pred map__keys_2(map(K, _V), list(K), list(K)).
:- mode map__keys_2(input, input, output).
map__keys_2([], Keys, Keys).
map__keys_2([Key - _ | Rest], Keys0, Keys) :-
	map__keys_2(Rest, [Key | Keys0], Keys).

%-----------------------------------------------------------------------------%

	% convert a map to an associate list

:- pred map__to_assoc_list(map(K,V), list(map__pair(K,V))).
:- mode map__to_assoc_list(input, output).
map__to_assoc_list(M, M).

%-----------------------------------------------------------------------------%

	% delete a key-value pair from a map

:- pred map__delete(map(K,V), K, map(K,V)).
:- mode map__delete(input, input, output).

map__delete([K-V | Map0], Key, Map) :-
	(if
		K = Key
	then
		Map = Map0
	else
		map__delete(Map0, Key, Map1),
		Map = [K-V|Map1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
