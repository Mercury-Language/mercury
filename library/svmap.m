%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: svmap.m.
% Author: zs.
% Stability: high.

% This file provides an interface to the 'map' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the map module; the only difference is the order of the
% arguments.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module svmap.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

    % Insert a new key and corresponding value into a map.
    % Fail if the key already exists.
    %
:- pred svmap.insert(K::in, V::in, map(K, V)::in, map(K, V)::out) is semidet.

    % Insert a new key and corresponding value into a map.
    % Abort if the key already exists.
    %
:- pred svmap.det_insert(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply map.det_insert to key - value pairs from corresponding lists.
    %
:- pred svmap.det_insert_from_corresponding_lists(list(K)::in, list(V)::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Apply map.det_insert to key - value pairs from the assoc_lists.
    %
:- pred svmap.det_insert_from_assoc_list(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Apply map.set to key - value pairs from corresponding lists.
    %
:- pred svmap.set_from_corresponding_lists(list(K)::in, list(V)::in,
    map(K, V)::in, map(K, V)::out) is det.

:- pred svmap.set_from_assoc_list(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Update the value corresponding to a given key
    % Fail if the key doesn't already exist.
    %
:- pred svmap.update(K::in, V::in, map(K, V)::in, map(K, V)::out) is semidet.

    % Update the value corresponding to a given key
    % Abort if the key doesn't already exist.
    %
:- pred svmap.det_update(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % Update value if the key is already present, otherwise
    % insert new key and value.
    %
:- pred svmap.set(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % Delete a key-value pair from a map.
    % If the key is not present, leave the map unchanged.
    %
:- pred svmap.delete(K::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply map.delete/3 to a list of keys.
    %
:- pred svmap.delete_list(list(K)::in, map(K, V)::in, map(K, V)::out) is det.

    % Delete a key-value pair from a map and return the value.
    % Fail if the key is not present.
    %
:- pred svmap.remove(K::in, V::out, map(K, V)::in, map(K, V)::out) is semidet.

    % Delete a key-value pair from a map and return the value.
    % Abort if the key is not present.
    %
:- pred svmap.det_remove(K::in, V::out, map(K, V)::in, map(K, V)::out) is det.

    % Remove the smallest item from the map, fail if
    % the map is empty.
    %
:- pred svmap.remove_smallest(K::out, V::out, map(K, V)::in, map(K, V)::out)
    is semidet.

%-----------------------------------------------------------------------------%

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term. % for var/1.

:- pragma type_spec(svmap.insert(in, in, in, out), K = var(_)).
:- pragma type_spec(svmap.insert(in, in, in, out), K = int).

:- pragma type_spec(svmap.det_insert(in, in, in, out), K = var(_)).
:- pragma type_spec(svmap.det_insert(in, in, in, out), K = int).

:- pragma type_spec(svmap.set(in, in, in, out), K = var(_)).
:- pragma type_spec(svmap.set(in, in, in, out), K = var(_)).

:- pragma type_spec(svmap.update(in, in, in, out), K = var(_)).
:- pragma type_spec(svmap.update(in, in, in, out), K = int).

:- pragma type_spec(svmap.det_update(in, in, in, out), K = var(_)).
:- pragma type_spec(svmap.det_update(in, in, in, out), K = int).

:- implementation.

%-----------------------------------------------------------------------------%

svmap.insert(K, V, Map0, Map) :-
    map.insert(Map0, K, V, Map).

svmap.det_insert(K, V, Map0, Map) :-
    map.det_insert(Map0, K, V, Map).

svmap.det_insert_from_corresponding_lists(Ks, Vs, Map0, Map) :-
    map.det_insert_from_corresponding_lists(Map0, Ks, Vs, Map).

svmap.det_insert_from_assoc_list(As, Map0, Map) :-
    map.det_insert_from_assoc_list(Map0, As, Map).

svmap.set_from_corresponding_lists(Ks, Vs, Map0, Map) :-
    map.set_from_corresponding_lists(Map0, Ks, Vs, Map).

svmap.set_from_assoc_list(As, Map0, Map) :-
    map.set_from_assoc_list(Map0, As, Map).

svmap.update(K, V, Map0, Map) :-
    map.update(Map0, K, V, Map).

svmap.det_update(K, V, Map0, Map) :-
    map.det_update(Map0, K, V, Map).

svmap.set(K, V, Map0, Map) :-
    map.set(Map0, K, V, Map).

svmap.delete(Key, Map0, Map) :-
    map.delete(Map0, Key, Map).

svmap.delete_list(Keys, Map0, Map) :-
    map.delete_list(Map0, Keys, Map).

svmap.remove(Key, Value, Map0, Map) :-
    map.remove(Map0, Key, Value, Map).

svmap.det_remove(Key, Value, Map0, Map) :-
    map.det_remove(Map0, Key, Value, Map).

svmap.remove_smallest(K, V, Map0, Map) :-
    map.remove_smallest(Map0, K, V, Map).

%-----------------------------------------------------------------------------%
