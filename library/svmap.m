%---------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: svmap.m.
% Author: zs.
% Stability: high.
%
% This file provides an interface to the 'map' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the map module; the only difference is the order of the
% arguments.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module svmap.
:- interface.
:- import_module list, assoc_list, map.

%-----------------------------------------------------------------------------%

	% Insert a new key and corresponding value into a map.
	% Fail if the key already exists.
	%
:- pred svmap__insert(K::in, V::in, map(K, V)::in, map(K, V)::out) is semidet.

	% Insert a new key and corresponding value into a map.
	% Abort if the key already exists.
	%
:- pred svmap__det_insert(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

	% Apply map__det_insert to key - value pairs from corresponding lists.
	%
:- pred svmap__det_insert_from_corresponding_lists(list(K)::in, list(V)::in,
	map(K, V)::in, map(K, V)::out) is det.

	% Apply map__det_insert to key - value pairs from the assoc_lists.
	%
:- pred svmap__det_insert_from_assoc_list(assoc_list(K, V)::in,
	map(K, V)::in, map(K, V)::out) is det.

	% Apply map__set to key - value pairs from corresponding lists.
	%
:- pred svmap__set_from_corresponding_lists(list(K)::in, list(V)::in,
	map(K, V)::in, map(K, V)::out) is det.

:- pred svmap__set_from_assoc_list(assoc_list(K, V)::in,
	map(K, V)::in, map(K, V)::out) is det.

	% Update the value corresponding to a given key
	% Fail if the key doesn't already exist.
	%
:- pred svmap__update(K::in, V::in, map(K, V)::in, map(K, V)::out) is semidet.

	% Update the value corresponding to a given key
	% Abort if the key doesn't already exist.
	%
:- pred svmap__det_update(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

	% Update value if the key is already present, otherwise
	% insert new key and value.
	%
:- pred svmap__set(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

	% Delete a key-value pair from a map.
	% If the key is not present, leave the map unchanged.
	%
:- pred svmap__delete(K::in, map(K, V)::in, map(K, V)::out) is det.

	% Apply map__delete/3 to a list of keys.
	%
:- pred svmap__delete_list(list(K)::in, map(K, V)::in, map(K, V)::out) is det.

	% Delete a key-value pair from a map and return the value.
	% Fail if the key is not present.
	%
:- pred svmap__remove(K::in, V::out, map(K, V)::in, map(K, V)::out) is semidet.

	% Delete a key-value pair from a map and return the value.
	% Abort if the key is not present.
	%
:- pred svmap__det_remove(K::in, V::out, map(K, V)::in, map(K, V)::out) is det.

	% Remove the smallest item from the map, fail if
	% the map is empty.
	%
:- pred svmap__remove_smallest(K::out, V::out, map(K, V)::in, map(K, V)::out)
	is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

svmap__insert(K, V, Map0, Map) :-
	map__insert(Map0, K, V, Map).

svmap__det_insert(K, V, Map0, Map) :-
	map__det_insert(Map0, K, V, Map).

svmap__det_insert_from_corresponding_lists(Ks, Vs, Map0, Map) :-
	map__det_insert_from_corresponding_lists(Map0, Ks, Vs, Map).

svmap__det_insert_from_assoc_list(As, Map0, Map) :-
	map__det_insert_from_assoc_list(Map0, As, Map).

svmap__set_from_corresponding_lists(Ks, Vs, Map0, Map) :-
	map__set_from_corresponding_lists(Map0, Ks, Vs, Map).

svmap__set_from_assoc_list(As, Map0, Map) :-
	map__set_from_assoc_list(Map0, As, Map).

svmap__update(K, V, Map0, Map) :-
	map__update(Map0, K, V, Map).

svmap__det_update(K, V, Map0, Map) :-
	map__det_update(Map0, K, V, Map).

svmap__set(K, V, Map0, Map) :-
	map__set(Map0, K, V, Map).

svmap__delete(Key, Map0, Map) :-
	map__delete(Map0, Key, Map).

svmap__delete_list(Keys, Map0, Map) :-
	map__delete_list(Map0, Keys, Map).

svmap__remove(Key, Value, Map0, Map) :-
	map__remove(Map0, Key, Value, Map).

svmap__det_remove(Key, Value, Map0, Map) :-
	map__det_remove(Map0, Key, Value, Map).

svmap__remove_smallest(K, V, Map0, Map) :-
	map__remove_smallest(Map0, K, V, Map).

%-----------------------------------------------------------------------------%
