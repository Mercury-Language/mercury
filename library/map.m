%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: map.m.
% Main author: fjh, conway.
% Stability: high.
%
% This file provides the 'map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key, Data) pairs which allows you to look up any Data item given the
% Key.
%
% The implementation is using balanced binary trees, as provided by
% tree234.m.  Virtually all the predicates in this file just
% forward the work to the corresponding predicate in tree234.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module map.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type map(_K, _V).

%-----------------------------------------------------------------------------%

    % Initialize an empty map.
    %
:- pred map__init(map(_, _)::uo) is det.
:- func map__init = (map(K, V)::uo) is det.

    % Check whether a map is empty.
    %
:- pred map__is_empty(map(_, _)::in) is semidet.

    % Check whether map contains key
    %
:- pred map__contains(map(K, _V)::in, K::in) is semidet.

:- pred map__member(map(K, V)::in, K::out, V::out) is nondet.

    % Search map for key.
    %
:- pred map__search(map(K, V)::in, K::in, V::out) is semidet.
:- func map__search(map(K, V), K) = V is semidet.

    % Search map for key, but abort if search fails.
    %
:- pred map__lookup(map(K, V)::in, K::in, V::out) is det.
:- func map__lookup(map(K, V), K) = V.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred map__lower_bound_search(map(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Aborts if there is no key with the given or lower value.
    %
:- pred map__lower_bound_lookup(map(K, V)::in, K::in, K::out, V::out) is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred map__upper_bound_search(map(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Aborts if there is no key with the given or higher value.
    %
:- pred map__upper_bound_lookup(map(K, V)::in, K::in, K::out, V::out) is det.

    % Return the largest key in the map, if there is one.
    %
:- func map__max_key(map(K,V)) = K is semidet.

    % Return the smallest key in the map, if there is one.
    %
:- func map__min_key(map(K,V)) = K is semidet.

    % Search map for data.
    %
:- pred map__inverse_search(map(K, V)::in, V::in, K::out) is nondet.

    % Insert a new key and corresponding value into a map.
    % Fail if the key already exists.
    %
:- pred map__insert(map(K, V)::in, K::in, V::in, map(K, V)::out) is semidet.
:- func map__insert(map(K, V), K, V) = map(K, V) is semidet.

    % Insert a new key and corresponding value into a map.
    % Abort if the key already exists.
    %
:- pred map__det_insert(map(K, V)::in, K::in, V::in, map(K, V)::out) is det.
:- func map__det_insert(map(K, V), K, V) = map(K, V).

    % Apply map__det_insert to key - value pairs from corresponding lists.
    %
:- pred map__det_insert_from_corresponding_lists(map(K, V)::in, list(K)::in,
    list(V)::in, map(K, V)::out) is det.

:- func map__det_insert_from_corresponding_lists(map(K, V), list(K), list(V))
    = map(K, V).

    % Apply map__det_insert to key - value pairs from the assoc_lists.
    %
:- pred map__det_insert_from_assoc_list(map(K, V)::in, assoc_list(K, V)::in,
    map(K, V)::out) is det.
:- func map__det_insert_from_assoc_list(map(K, V), assoc_list(K, V))
    = map(K, V).

    % Apply map__set to key - value pairs from corresponding lists.
    %
:- pred map__set_from_corresponding_lists(map(K, V)::in, list(K)::in,
    list(V)::in, map(K, V)::out) is det.
:- func map__set_from_corresponding_lists(map(K, V), list(K), list(V))
    = map(K, V).

:- pred map__set_from_assoc_list(map(K, V)::in, assoc_list(K, V)::in,
    map(K, V)::out) is det.
:- func map__set_from_assoc_list(map(K, V), assoc_list(K, V)) = map(K, V).

    % Update the value corresponding to a given key
    % Fail if the key doesn't already exist.
    %
:- pred map__update(map(K, V)::in, K::in, V::in, map(K, V)::out) is semidet.
:- func map__update(map(K, V), K, V) = map(K, V) is semidet.

    % Update the value corresponding to a given key
    % Abort if the key doesn't already exist.
    %
:- pred map__det_update(map(K, V)::in, K::in, V::in, map(K, V)::out) is det.
:- func map__det_update(map(K, V), K, V) = map(K, V).

    % Update the value at the given key by applying the supplied
    % transformation to it.  Fails if the key is not found.  This is faster
    % than first searching for the value and then updating it.
    %
:- pred map__transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    map(K, V)::in, map(K, V)::out) is semidet.

    % Same as transform_value/4, but aborts instead of failing if the
    % key is not found.
    %
:- pred map__det_transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    map(K, V)::in, map(K, V)::out) is det.
:- func map__det_transform_value(func(V) = V, K, map(K, V)) = map(K, V).

    % Update value if the key is already present, otherwise
    % insert new key and value.
    %
:- func map__set(map(K, V), K, V) = map(K, V).
:- pred map__set(map(K, V)::in, K::in, V::in, map(K, V)::out) is det.

    % Given a map, return a list of all the keys in the map.
    %
:- func map__keys(map(K, _V)) = list(K).
:- pred map__keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % in sorted order.
    %
:- func map__sorted_keys(map(K, _V)) = list(K).
:- pred map__sorted_keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the data values in the map.
    %
:- func map__values(map(_K, V)) = list(V).
:- pred map__values(map(_K, V)::in, list(V)::out) is det.

    % Convert a map to an association list.
    %
:- func map__to_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred map__to_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert a map to an association list which is sorted on the keys.
    %
:- func map__to_sorted_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred map__to_sorted_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert an association list to a map.
    %
:- func map__from_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred map__from_assoc_list(assoc_list(K, V)::in, map(K, V)::out) is det.

    % Convert a sorted association list to a map.
    %
:- func map__from_sorted_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred map__from_sorted_assoc_list(assoc_list(K, V)::in, map(K, V)::out)
    is det.

    % Delete a key-value pair from a map.
    % If the key is not present, leave the map unchanged.
    %
:- func map__delete(map(K, V), K) = map(K, V).
:- pred map__delete(map(K, V)::in, K::in, map(K, V)::out) is det.

    % Apply map__delete/3 to a list of keys.
    %
:- func map__delete_list(map(K, V), list(K)) = map(K, V).
:- pred map__delete_list(map(K, V)::in, list(K)::in, map(K, V)::out) is det.

    % Delete a key-value pair from a map and return the value.
    % Fail if the key is not present.
    %
:- pred map__remove(map(K, V)::in, K::in, V::out, map(K, V)::out) is semidet.

    % Delete a key-value pair from a map and return the value.
    % Abort if the key is not present.
    %
:- pred map__det_remove(map(K, V)::in, K::in, V::out, map(K, V)::out) is det.

    % Count the number of elements in the map.
    %
:- func map__count(map(K, V)) = int.
:- pred map__count(map(K, V)::in, int::out) is det.

    % Convert a pair of lists (which must be of the same length) to a map.
    %
:- func map__from_corresponding_lists(list(K), list(V)) = map(K, V).
:- pred map__from_corresponding_lists(list(K)::in, list(V)::in, map(K, V)::out)
    is det.

    % For map__merge(MapA, MapB, Map), MapA and MapB must not both contain
    % the same key.
    %
:- func map__merge(map(K, V), map(K, V)) = map(K, V).
:- pred map__merge(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % For map__overlay(MapA, MapB, Map), if MapA and MapB both contain the
    % same key, then Map will map that key to the value from MapB.
    % In other words, MapB takes precedence over MapA.
    %
:- func map__overlay(map(K, V), map(K, V)) = map(K, V).
:- pred map__overlay(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % map__overlay_large_map(MapA, MapB, Map) performs the same task as
    % map__overlay(MapA, MapB, Map). However, while map__overlay takes time
    % proportional to the size of MapB, map__overlay_large_map takes time
    % proportional to the size of MapA. In other words, it preferable when
    % MapB is a large map.
    %
:- func map__overlay_large_map(map(K, V), map(K, V)) = map(K, V).
:- pred map__overlay_large_map(map(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.

    % map__select takes a map and a set of keys and returns a map
    % containing the keys in the set and their corresponding values.
    %
:- func map__select(map(K, V), set(K)) = map(K, V).
:- pred map__select(map(K, V)::in, set(K)::in, map(K, V)::out) is det.

    % Given a list of keys, produce a list of their corresponding
    % values in a specified map.
    %
:- func map__apply_to_list(list(K), map(K, V)) = list(V).
:- pred map__apply_to_list(list(K)::in, map(K, V)::in, list(V)::out) is det.

    % Declaratively, a NOP.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the map in the expectation
    % of a number of lookups but few or no modifications.
    %
:- func map__optimize(map(K, V)) = map(K, V).
:- pred map__optimize(map(K, V)::in, map(K, V)::out) is det.

    % Remove the smallest item from the map, fail if the map is empty.
    %
:- pred map__remove_smallest(map(K, V)::in, K::out, V::out, map(K, V)::out)
    is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate for each key-value pair.
    %
:- func map__foldl(func(K, V, A) = A, map(K, V), A) = A.
:- pred map__foldl(pred(K, V, A, A), map(K, V), A, A).
:- mode map__foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode map__foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode map__foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with two accumulators for each key-value pair.
    % (Although no more expressive than map__foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred map__foldl2(pred(K, V, A, A, B, B), map(K, V), A, A, B, B).
:- mode map__foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode map__foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode map__foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode map__foldl2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with three accumulators for each key-value pair.
    % (Although no more expressive than map__foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred map__foldl3(pred(K, V, A, A, B, B, C, C), map(K, V), A, A, B, B, C, C).
:- mode map__foldl3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode map__foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode map__foldl3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode map__foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode map__foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with four accumulators for each key-value pair.
    % (Although no more expressive than map__foldl, this is often
    % a more convenient format, and a little more efficient).
:- pred map__foldl4(pred(K, V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode map__foldl4(pred(in, in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode map__foldl4(pred(in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode map__foldl4(pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode map__foldl4(pred(in, in, in, out, in, out, di, uo, di, uo) is det,
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode map__foldl4(pred(in, in, in, out, di, uo, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode map__foldl4(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo, di, uo) is det.

    % Apply a transformation predicate to all the values in a map.
    %
:- func map__map_values(func(K, V) = W, map(K, V)) = map(K, W).
:- pred map__map_values(pred(K, V, W), map(K, V), map(K, W)).
:- mode map__map_values(pred(in, in, out) is det, in, out) is det.
:- mode map__map_values(pred(in, in, out) is semidet, in, out) is semidet.

    % Apply a transformation predicate to all the values in a map,
    % while continuously updating an accumulator.
    %
:- pred map__map_foldl(pred(K, V, W, A, A), map(K, V), map(K, W), A, A).
:- mode map__map_foldl(pred(in, in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode map__map_foldl(pred(in, in, out, in, out) is det, in, out, in, out)
    is det.
:- mode map__map_foldl(pred(in, in, out, in, out) is semidet, in, out, in, out)
    is semidet.

    % As map__map_foldl, but with two accumulators.
    %
:- pred map__map_foldl2(pred(K, V, W, A, A, B, B), map(K, V), map(K, W),
    A, A, B, B).
:- mode map__map_foldl2(pred(in, in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map__map_foldl2(pred(in, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.

    % Given two maps M1 and M2, create a third map M3 that has only the
    % keys that occur in both M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- pred map__intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map__intersect(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode map__intersect(pred(in, in, out) is det, in, in, out) is det.

:- func map__intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).

    % Calls map__intersect. Aborts if map__intersect fails.
    %
:- pred map__det_intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map__det_intersect(pred(in, in, out) is semidet, in, in, out) is det.

:- func map__det_intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode map__det_intersect(func(in, in) = out is semidet, in, in) = out is det.

    % Given two maps M1 and M2, create a third map M3 that has only the
    % keys that occur in both M1 and M2. For keys that occur in both M1
    % and M2, compute the corresponding values. If they are the same,
    % include the key/value pair in M3. If they differ, do not include the
    % key in M3.
    %
    % This predicate effectively considers the input maps to be sets of
    % key/value pairs, computes the intersection of those two sets, and
    % returns the map corresponding to the intersection.
    %
    % map__common_subset is very similar to map__intersect, but can succeed
    % even with an output map that does not contain an entry for a key
    % value that occurs in both input maps.
    %
:- func map__common_subset(map(K, V), map(K, V)) = map(K, V).

    % Given two maps M1 and M2, create a third map M3 that all the keys
    % that occur in either M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- func map__union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- pred map__union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map__union(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode map__union(pred(in, in, out) is det, in, in, out) is det.

    % Calls map__union. Aborts if map__union fails.
    %
:- pred map__det_union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map__det_union(pred(in, in, out) is semidet, in, in, out) is det.

:- func map__det_union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode map__det_union(func(in, in) = out is semidet, in, in) = out is det.

    % Consider the original map a set of key-value pairs. This predicate
    % returns a map that maps each value to the set of keys it is paired
    % with in the original map.
    %
:- func map__reverse_map(map(K, V)) = map(V, set(K)).

    % Field selection for maps.

    % Map ^ elem(Key) = map__search(Map, Key).
    %
:- func map__elem(K, map(K, V)) = V is semidet.

    % Map ^ det_elem(Key) = map__lookup(Map, Key).
    %
:- func map__det_elem(K, map(K, V)) = V.

    % Field update for maps.

    % (Map ^ elem(Key) := Value) = map__set(Map, Key, Value).
    %
:- func 'map__elem :='(K, map(K, V), V) = map(K, V).

    % (Map ^ det_elem(Key) := Value) = map__det_update(Map, Key, Value).
    %
:- func 'map__det_elem :='(K, map(K, V), V) = map(K, V).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term. % for var/1.
:- import_module tree234.

:- type map(K, V)   ==  tree234(K, V).

%-----------------------------------------------------------------------------%

:- pragma type_spec(map__search/3, K = var(_)).
:- pragma type_spec(map__search/3, K = int).

:- pragma type_spec(map__search/2, K = var(_)).
:- pragma type_spec(map__search/2, K = int).

:- pragma type_spec(map__lookup/3, K = var(_)).
:- pragma type_spec(map__lookup/3, K = int).

:- pragma type_spec(map__lookup/2, K = var(_)).
:- pragma type_spec(map__lookup/2, K = int).

:- pragma type_spec(map__insert(in, in, in, out), K = var(_)).
:- pragma type_spec(map__insert(in, in, in, out), K = int).

:- pragma type_spec(map__det_insert(in, in, in, out), K = var(_)).
:- pragma type_spec(map__det_insert(in, in, in, out), K = int).

:- pragma type_spec(map__set(in, in, in, out), K = var(_)).
:- pragma type_spec(map__set(in, in, in, out), K = int).

:- pragma type_spec(map__update(in, in, in, out), K = var(_)).
:- pragma type_spec(map__update(in, in, in, out), K = int).

:- pragma type_spec(map__det_update/4, K = var(_)).
:- pragma type_spec(map__det_update/4, K = int).

:- pragma type_spec(map__overlay/2, K = var(_)).
:- pragma type_spec(map__overlay/3, K = var(_)).

:- pragma type_spec(map__select/2, K = var(_)).
:- pragma type_spec(map__select/3, K = var(_)).

:- pragma type_spec(map__elem/2, K = int).
:- pragma type_spec(map__elem/2, K = var(_)).

:- pragma type_spec(map__det_elem/2, K = int).
:- pragma type_spec(map__det_elem/2, K = var(_)).

:- pragma type_spec('map__elem :='/3, K = int).
:- pragma type_spec('map__elem :='/3, K = var(_)).

:- pragma type_spec('map__det_elem :='/3, K = int).
:- pragma type_spec('map__det_elem :='/3, K = var(_)).


:- implementation.

:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module svset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

map__init(M) :-
    tree234__init(M).

map__is_empty(M) :-
    tree234__is_empty(M).

map__contains(Map, K) :-
    map__search(Map, K, _).

map__member(Map, K, V) :-
    tree234__member(Map, K, V).

map__search(Map, K, V) :-
    tree234__search(Map, K, V).

map__lookup(Map, K, V) :-
    ( tree234__search(Map, K, V1) ->
        V = V1
    ;
        report_lookup_error("map__lookup: key not found", K, V)
    ).

map__lower_bound_search(Map, SearchK, K, V) :-
    tree234__lower_bound_search(Map, SearchK, K, V).

map__lower_bound_lookup(Map, SearchK, K, V) :-
    ( tree234__lower_bound_search(Map, SearchK, K1, V1) ->
        K = K1,
        V = V1
    ;
        report_lookup_error("map__lower_bound_lookup: key not found",
            SearchK, V)
    ).

map__upper_bound_search(Map, SearchK, K, V) :-
    tree234__upper_bound_search(Map, SearchK, K, V).

map__upper_bound_lookup(Map, SearchK, K, V) :-
    ( tree234__upper_bound_search(Map, SearchK, K1, V1) ->
        K = K1,
        V = V1
    ;
        report_lookup_error("map__upper_bound_lookup: key not found",
            SearchK, V)
    ).

map__max_key(M) = tree234__max_key(M).

map__min_key(M) = tree234__min_key(M).

map__insert(Map0, K, V, Map) :-
    tree234__insert(Map0, K, V, Map).

map__det_insert(Map0, K, V, Map) :-
    ( tree234__insert(Map0, K, V, Map1) ->
        Map = Map1
    ;
        report_lookup_error("map__det_insert: key already present", K, V)
    ).

map__det_insert_from_corresponding_lists(Map0, Ks, Vs, Map) :-
    (
        Ks = [Key | Keys],
        Vs = [Value | Values]
    ->
        map__det_insert(Map0, Key, Value, Map1),
        map__det_insert_from_corresponding_lists(Map1, Keys, Values,
            Map)
    ;
        Ks = [],
        Vs = []
    ->
        Map = Map0
    ;
        error("map__det_insert_from_corresponding_lists - " ++
            "lists do not correspond")
    ).

map__det_insert_from_assoc_list(Map, [], Map).
map__det_insert_from_assoc_list(Map0, [K - V | KVs], Map) :-
    map__det_insert(Map0, K, V, Map1),
    map__det_insert_from_assoc_list(Map1, KVs, Map).

map__set_from_corresponding_lists(Map0, Ks, Vs, Map) :-
    (
        Ks = [Key | Keys],
        Vs = [Value | Values]
    ->
        map__set(Map0, Key, Value, Map1),
        map__set_from_corresponding_lists(Map1, Keys, Values, Map)
    ;
        Ks = [],
        Vs = []
    ->
        Map = Map0
    ;
        error("map__set_from_corresponding_lists - " ++
            "lists do not correspond")
    ).

map__set_from_assoc_list(Map, [], Map).
map__set_from_assoc_list(Map0, [K - V | KVs], Map) :-
    map__set(Map0, K, V, Map1),
    map__set_from_assoc_list(Map1, KVs, Map).

map__update(Map0, K, V, Map) :-
    tree234__update(Map0, K, V, Map).

map__det_update(Map0, K, V, Map) :-
    ( tree234__update(Map0, K, V, Map1) ->
        Map = Map1
    ;
        report_lookup_error("map__det_update: key not found", K, V)
    ).

map__transform_value(P, K, !Map) :-
    tree234__transform_value(P, K, !Map).

map__det_transform_value(P, K, !Map) :-
    (
        map__transform_value(P, K, !.Map, NewMap)
    ->
        !:Map = NewMap
    ;
        report_lookup_error("map__det_transform_value: key not found",
            K)
    ).

map__det_transform_value(F, K, Map0) = Map :-
    map__det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), K,
        Map0, Map).

map__set(Map0, K, V, Map) :-
    tree234__set(Map0, K, V, Map).

map__keys(Map, KeyList) :-
    tree234__keys(Map, KeyList).

map__sorted_keys(Map, KeyList) :-
    % Guaranteed to yield sorted lists.
    tree234__keys(Map, KeyList).

map__values(Map, KeyList) :-
    tree234__values(Map, KeyList).

map__to_assoc_list(M, L) :-
    tree234__tree234_to_assoc_list(M, L).

map__to_sorted_assoc_list(M, L) :-
    % Guaranteed to yield sorted lists.
    tree234__tree234_to_assoc_list(M, L).

map__from_assoc_list(L, M) :-
    tree234__assoc_list_to_tree234(L, M).

map__from_sorted_assoc_list(L, M) :-
    tree234__assoc_list_to_tree234(L, M).

map__delete(Map0, Key, Map) :-
    tree234__delete(Map0, Key, Map).

map__delete_list(Map, [], Map).
map__delete_list(Map0, [Key | Keys], Map) :-
    map__delete(Map0, Key, Map1),
    map__delete_list(Map1, Keys, Map).

map__remove(Map0, Key, Value, Map) :-
    tree234__remove(Map0, Key, Value, Map).

map__det_remove(Map0, Key, Value, Map) :-
    ( tree234__remove(Map0, Key, Value1, Map1) ->
        Value = Value1,
        Map = Map1
    ;
        report_lookup_error("map__det_remove: key not found", Key, Value)
    ).

map__count(Map, Count) :-
    tree234__count(Map, Count).

%-----------------------------------------------------------------------------%

map__inverse_search(Map, V, K) :-
    map__member(Map, K, V).

%-----------------------------------------------------------------------------%

map__from_corresponding_lists(Keys, Values, Map) :-
    assoc_list__from_corresponding_lists(Keys, Values, AssocList),
    tree234__assoc_list_to_tree234(AssocList, Map).

%-----------------------------------------------------------------------------%

map__merge(M0, M1, M) :-
    map__to_assoc_list(M0, ML0),
    map__to_assoc_list(M1, ML1),
    list__merge(ML0, ML1, ML),
    map__from_sorted_assoc_list(ML, M).

%-----------------------------------------------------------------------------%

map__optimize(Map, Map).

%-----------------------------------------------------------------------------%

map__overlay(Map0, Map1, Map) :-
    map__to_assoc_list(Map1, AssocList),
    map__overlay_2(AssocList, Map0, Map).

:- pred map__overlay_2(assoc_list(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.
:- pragma type_spec(map__overlay_2/3, K = var(_)).

map__overlay_2([], Map, Map).
map__overlay_2([K - V | AssocList], Map0, Map) :-
    map__set(Map0, K, V, Map1),
    map__overlay_2(AssocList, Map1, Map).

map__overlay_large_map(Map0, Map1, Map) :-
    map__to_assoc_list(Map0, AssocList),
    map__overlay_large_map_2(AssocList, Map1, Map).

:- pred map__overlay_large_map_2(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.
:- pragma type_spec(map__overlay_large_map_2/3, K = var(_)).

map__overlay_large_map_2([], Map, Map).
map__overlay_large_map_2([K - V | AssocList], Map0, Map) :-
    ( map__insert(Map0, K, V, Map1) ->
        Map2 = Map1
    ;
        Map2 = Map0
    ),
    map__overlay_large_map_2(AssocList, Map2, Map).

%-----------------------------------------------------------------------------%

map__select(Original, KeySet, NewMap) :-
    set__to_sorted_list(KeySet, KeyList),
    map__init(NewMap0),
    map__select_2(KeyList, Original, NewMap0, NewMap).

:- pred map__select_2(list(K)::in, map(K, V)::in, map(K, V)::in,
    map(K, V)::out) is det.
:- pragma type_spec(map__select_2/4, K = var(_)).

map__select_2([], _Original, New, New).
map__select_2([K|Ks], Original, New0, New) :-
    ( map__search(Original, K, V) ->
        map__set(New0, K, V, New1)
    ;
        New1 = New0
    ),
    map__select_2(Ks, Original, New1, New).

%-----------------------------------------------------------------------------%

map__apply_to_list([], _, []).
map__apply_to_list([K | Ks], Map, [V | Vs]) :-
    map__lookup(Map, K, V),
    map__apply_to_list(Ks, Map, Vs).

%-----------------------------------------------------------------------------%

map__remove_smallest(Map0, K, V, Map) :-
    tree234__remove_smallest(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

map__foldl(Pred, Map, !A) :-
    tree234__foldl(Pred, Map, !A).

map__foldl2(Pred, Map, !A, !B) :-
    tree234__foldl2(Pred, Map, !A, !B).

map__foldl3(Pred, Map, !A, !B, !C) :-
    tree234__foldl3(Pred, Map, !A, !B, !C).

map__foldl4(Pred, Map, !A, !B, !C, !D) :-
    tree234__foldl4(Pred, Map, !A, !B, !C, !D).

%-----------------------------------------------------------------------------%

map__map_values(Pred, Map0, Map) :-
    tree234__map_values(Pred, Map0, Map).

map__map_foldl(Pred, !Map, !Acc) :-
    tree234__map_foldl(Pred, !Map, !Acc).

map__map_foldl2(Pred, !Map, !Acc1, !Acc2) :-
    tree234__map_foldl2(Pred, !Map, !Acc1, !Acc2).

%-----------------------------------------------------------------------------%

map__intersect(CommonPred, Map1, Map2, Common) :-
    map__to_sorted_assoc_list(Map1, AssocList1),
    map__to_sorted_assoc_list(Map2, AssocList2),
    map__init(Common0),
    map__intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred map__intersect_2(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    map(K, V), map(K, V)).
:- mode map__intersect_2(in, in, pred(in, in, out) is semidet, in, out)
    is semidet.
:- mode map__intersect_2(in, in, pred(in, in, out) is det, in, out)
    is det.

map__intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
    (
        AssocList1 = [],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [_ | _],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [],
        AssocList2 = [_ | _],
        Common = Common0
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            call(CommonPred, Value1, Value2, Value),
            map__det_insert(Common0, Key1, Value, Common1),
            map__intersect_2(AssocTail1, AssocTail2, CommonPred,
                Common1, Common)
        ;
            R = (<),
            map__intersect_2(AssocTail1, AssocList2, CommonPred,
                Common0, Common)
        ;
            R = (>),
            map__intersect_2(AssocList1, AssocTail2, CommonPred,
                Common0, Common)
        )
    ).

map__det_intersect(CommonPred, Map1, Map2, Common) :-
    ( map__intersect(CommonPred, Map1, Map2, CommonPrime) ->
        Common = CommonPrime
    ;
        error("map__det_intersect: map__intersect failed")
    ).

%-----------------------------------------------------------------------------%

map__common_subset(Map1, Map2) = Common :-
    map__to_sorted_assoc_list(Map1, AssocList1),
    map__to_sorted_assoc_list(Map2, AssocList2),
    map__init(Common0),
    map__common_subset_2(AssocList1, AssocList2, Common0) = Common.

:- func map__common_subset_2(assoc_list(K, V), assoc_list(K, V),
    map(K, V)) = map(K, V).

map__common_subset_2(AssocList1, AssocList2, Common0) = Common :-
    (
        AssocList1 = [],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [_ | _],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [],
        AssocList2 = [_ | _],
        Common = Common0
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            ( Value1 = Value2 ->
                map__det_insert(Common0, Key1, Value1, Common1)
            ;
                Common1 = Common0
            ),
            Common = map__common_subset_2(AssocTail1, AssocTail2,
                Common1)
        ;
            R = (<),
            Common = map__common_subset_2(AssocTail1, AssocList2,
                Common0)
        ;
            R = (>),
            Common = map__common_subset_2(AssocList1, AssocTail2,
                Common0)
        )
    ).

%-----------------------------------------------------------------------------%

map__union(CommonPred, Map1, Map2, Common) :-
    map__to_sorted_assoc_list(Map1, AssocList1),
    map__to_sorted_assoc_list(Map2, AssocList2),
    map__init(Common0),
    map__union_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred map__union_2(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    map(K, V), map(K, V)).
:- mode map__union_2(in, in, pred(in, in, out) is semidet, in, out)
    is semidet.
:- mode map__union_2(in, in, pred(in, in, out) is det, in, out)
    is det.

map__union_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
    (
        AssocList1 = [],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [_ | _],
        AssocList2 = [],
        map__det_insert_from_assoc_list(Common0, AssocList1, Common)
    ;
        AssocList1 = [],
        AssocList2 = [_ | _],
        map__det_insert_from_assoc_list(Common0, AssocList2, Common)
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            call(CommonPred, Value1, Value2, Value),
            map__det_insert(Common0, Key1, Value, Common1),
            map__union_2(AssocTail1, AssocTail2, CommonPred, Common1, Common)
        ;
            R = (<),
            map__det_insert(Common0, Key1, Value1, Common1),
            map__union_2(AssocTail1, AssocList2, CommonPred, Common1, Common)
        ;
            R = (>),
            map__det_insert(Common0, Key2, Value2, Common1),
            map__union_2(AssocList1, AssocTail2, CommonPred, Common1, Common)
        )
    ).

map__det_union(CommonPred, Map1, Map2, Union) :-
    ( map__union(CommonPred, Map1, Map2, UnionPrime) ->
        Union = UnionPrime
    ;
        error("map__det_union: map__union failed")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

map__init = M :-
    map__init(M).

map__search(M, K) = V :-
    map__search(M, K, V).

map__lookup(M, K) = V :-
    map__lookup(M, K, V).

map__insert(M1, K, V) = M2 :-
    map__insert(M1, K, V, M2).

map__det_insert(M1, K, V) = M2 :-
    map__det_insert(M1, K, V, M2).

map__det_insert_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    map__det_insert_from_corresponding_lists(M1, Ks, Vs, M2).

map__det_insert_from_assoc_list(M1, AL) = M2 :-
    map__det_insert_from_assoc_list(M1, AL, M2).

map__set_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    map__set_from_corresponding_lists(M1, Ks, Vs, M2).

map__set_from_assoc_list(M1, AL) = M2 :-
    map__set_from_assoc_list(M1, AL, M2).

map__update(M1, K, V) = M2 :-
    map__update(M1, K, V, M2).

map__det_update(M1, K, V) = M2 :-
    map__det_update(M1, K, V, M2).

map__set(M1, K, V) = M2 :-
    map__set(M1, K, V, M2).

map__keys(M) = Ks :-
    map__keys(M, Ks).

map__sorted_keys(M) = Ks :-
    map__sorted_keys(M, Ks).

map__values(M) = Vs :-
    map__values(M, Vs).

map__to_assoc_list(M) = AL :-
    map__to_assoc_list(M, AL).

map__to_sorted_assoc_list(M) = AL :-
    map__to_sorted_assoc_list(M, AL).

map__from_assoc_list(AL) = M :-
    map__from_assoc_list(AL, M).

map__from_sorted_assoc_list(AL) = M :-
    map__from_sorted_assoc_list(AL, M).

map__delete(M1, K) = M2 :-
    map__delete(M1, K, M2).

map__delete_list(M1, Ks) = M2 :-
    map__delete_list(M1, Ks, M2).

map__count(M) = N :-
    map__count(M, N).

map__from_corresponding_lists(Ks, Vs) = M :-
    map__from_corresponding_lists(Ks, Vs, M).

map__merge(M1, M2) = M3 :-
    map__merge(M1, M2, M3).

map__overlay(M1, M2) = M3 :-
    map__overlay(M1, M2, M3).

map__overlay_large_map(M1, M2) = M3 :-
    map__overlay_large_map(M1, M2, M3).

map__select(M1, S) = M2 :-
    map__select(M1, S, M2).

map__apply_to_list(Ks, M) = Vs :-
    map__apply_to_list(Ks, M, Vs).

map__optimize(M1) = M2 :-
    map__optimize(M1, M2).

map__foldl(F, M, A) = B :-
    P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    map__foldl(P, M, A, B).

map__map_values(F, M1) = M2 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map__map_values(P, M1, M2).

map__intersect(F, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map__intersect(P, M1, M2, M3).

map__det_intersect(PF, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    map__det_intersect(P, M1, M2, M3).

map__union(F, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map__union(P, M1, M2, M3).

map__det_union(F, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is semidet :- Z = F(X, Y) ),
    map__det_union(P, M1, M2, M3).

map__reverse_map(Map) = RevMap :-
    map__foldl(map__reverse_map_2, Map, map__init, RevMap).

:- pred map__reverse_map_2(K::in, V::in,
    map(V, set(K))::in, map(V, set(K))::out) is det.

map__reverse_map_2(Key, Value, !RevMap) :-
    ( map__search(!.RevMap, Value, Keys0) ->
        svset__insert(Key, Keys0, Keys),
        svmap__det_update(Value, Keys, !RevMap)
    ;
        svmap__det_insert(Value, set__make_singleton_set(Key), !RevMap)
    ).

map__elem(Key, Map) = map__search(Map, Key).

map__det_elem(Key, Map) = map__lookup(Map, Key).

'map__elem :='(Key, Map, Value) = map__set(Map, Key, Value).

'map__det_elem :='(Key, Map, Value) = map__det_update(Map, Key, Value).
