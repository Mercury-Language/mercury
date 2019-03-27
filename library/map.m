%--------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
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
% The implementation is using balanced binary trees, as provided by tree234.m.
% Virtually all the predicates in this file just forward the work
% to the corresponding predicate in tree234.m.
%
% Note: 2-3-4 trees do not have a canonical representation for any given map.
% Therefore, two maps with the same set of key-value pairs may have
% different internal representations. This means that two maps with the
% same set of key-value pairs that may fail to unify and may compare as
% unequal, for example if items were inserted into one of the maps in a
% different order. See equal/2 below which can be used to test if two
% maps have the same set of key-value pairs.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module map.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type map(_K, _V).

%---------------------------------------------------------------------------%

    % Initialize an empty map.
    %
:- func init = (map(K, V)::uo) is det.
:- pred init(map(_, _)::uo) is det.

    % Initialize a map containing the given key-value pair.
    %
:- func singleton(K, V) = map(K, V).

    % Check whether a map is empty.
    %
:- pred is_empty(map(_, _)::in) is semidet.

    % True if both maps have the same set of key-value pairs, regardless of
    % how the maps were constructed.
    %
    % Unifying maps does not work as one might expect because the internal
    % structures of two maps that contain the same set of key-value pairs
    % may be different.
    %
:- pred equal(map(K, V)::in, map(K, V)::in) is semidet.

    % Succeed iff the map contains the given key.
    %
:- pred contains(map(K, _V)::in, K::in) is semidet.

:- pred member(map(K, V)::in, K::out, V::out) is nondet.

    % Return the value associated with the given key in the map.
    % Fail if the map does not contain that key.
    %
:- func search(map(K, V), K) = V is semidet.
:- pred search(map(K, V)::in, K::in, V::out) is semidet.

    % Return the value associated with the given key in the map.
    % Throw an exception if the map does not contain that key.
    %
:- func lookup(map(K, V), K) = V.
:- pred lookup(map(K, V)::in, K::in, V::out) is det.

    % Search the map for key-value pairs with the given value.
    %
:- pred inverse_search(map(K, V)::in, V::in, K::out) is nondet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred lower_bound_search(map(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Throws an exception if there is no key with the given or lower value.
    %
:- pred lower_bound_lookup(map(K, V)::in, K::in, K::out, V::out) is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred upper_bound_search(map(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Throws an exception if there is no key with the given or higher value.
    %
:- pred upper_bound_lookup(map(K, V)::in, K::in, K::out, V::out) is det.

    % Return the largest key in the map, if there is one.
    %
:- func max_key(map(K, V)) = K is semidet.

    % As above, but throw an exception if there is no largest key.
    %
:- func det_max_key(map(K, V)) = K.

    % Return the smallest key in the map, if there is one.
    %
:- func min_key(map(K,V)) = K is semidet.

    % As above, but throw an exception if there is no smallest key.
    %
:- func det_min_key(map(K, V)) = K.

    % Insert a new key and corresponding value into a map.
    % Fail if the key already exists.
    %
:- func insert(map(K, V), K, V) = map(K, V) is semidet.
:- pred insert(K::in, V::in, map(K, V)::in, map(K, V)::out) is semidet.

    % Insert a new key and corresponding value into a map.
    % Throw an exception if the key already exists.
    %
:- func det_insert(map(K, V), K, V) = map(K, V).
:- pred det_insert(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply det_insert to key - value pairs from corresponding lists.
    %
:- func det_insert_from_corresponding_lists(map(K, V), list(K), list(V))
    = map(K, V).
:- pred det_insert_from_corresponding_lists(list(K)::in,
    list(V)::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply det_insert to key - value pairs from the assoc_lists.
    %
:- func det_insert_from_assoc_list(map(K, V), assoc_list(K, V))
    = map(K, V).
:- pred det_insert_from_assoc_list(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Apply set to key - value pairs from corresponding lists.
    %
:- func set_from_corresponding_lists(map(K, V), list(K), list(V))
    = map(K, V).
:- pred set_from_corresponding_lists(list(K)::in, list(V)::in,
    map(K, V)::in, map(K, V)::out) is det.

:- func set_from_assoc_list(map(K, V), assoc_list(K, V)) = map(K, V).
:- pred set_from_assoc_list(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Update the value corresponding to a given key
    % Fail if the key doesn't already exist.
    %
:- func update(map(K, V), K, V) = map(K, V) is semidet.
:- pred update(K::in, V::in, map(K, V)::in, map(K, V)::out) is semidet.

    % Update the value corresponding to a given key
    % Throw an exception if the key doesn't already exist.
    %
:- func det_update(map(K, V), K, V) = map(K, V).
:- pred det_update(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % search_insert(K, V, MaybeOldV, !Map):
    %
    % Search for the key K in the map. If the key is already in the map,
    % with corresponding value OldV, set MaybeOldV to yes(OldV). If it
    % is not in the map, then insert it into the map with value V.
    %
:- pred search_insert(K::in, V::in, maybe(V)::out,
    map(K, V)::in, map(K, V)::out) is det.

    % Update the value at the given key by applying the supplied
    % transformation to it. Fails if the key is not found. This is faster
    % than first searching for the value and then updating it.
    %
:- pred transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    map(K, V)::in, map(K, V)::out) is semidet.

    % Same as transform_value/4, but throws an exception if the key is not
    % found.
    %
:- func det_transform_value(func(V) = V, K, map(K, V)) = map(K, V).
:- pred det_transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Update value if the key is already present, otherwise
    % insert new key and value.
    %
:- func set(map(K, V), K, V) = map(K, V).
:- pred set(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % Given a map, return a list of all the keys in the map.
    %
:- func keys(map(K, _V)) = list(K).
:- pred keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % in sorted order.
    %
:- func sorted_keys(map(K, _V)) = list(K).
:- pred sorted_keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the data values in the map.
    %
:- func values(map(_K, V)) = list(V).
:- pred values(map(_K, V)::in, list(V)::out) is det.

:- pred keys_and_values(map(K, V)::in, list(K)::out, list(V)::out) is det.

    % Convert a map to an association list.
    %
:- func to_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred to_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert a map to an association list which is sorted on the keys.
    %
:- func to_sorted_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred to_sorted_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert an association list to a map.
    %
:- func from_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred from_assoc_list(assoc_list(K, V)::in, map(K, V)::out) is det.

    % Convert a sorted association list with no duplicated keys to a map.
    %
:- func from_sorted_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred from_sorted_assoc_list(assoc_list(K, V)::in, map(K, V)::out)
    is det.

    % Convert a reverse sorted association list with no duplicated keys
    % to a map.
    %
:- func from_rev_sorted_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred from_rev_sorted_assoc_list(assoc_list(K, V)::in, map(K, V)::out)
    is det.

    % Delete a key-value pair from a map.
    % If the key is not present, leave the map unchanged.
    %
:- func delete(map(K, V), K) = map(K, V).
:- pred delete(K::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply delete/3 to a list of keys.
    %
:- func delete_list(map(K, V), list(K)) = map(K, V).
:- pred delete_list(list(K)::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply delete/3 to a sorted list of keys. The fact that the list
    % is sorted may make this more efficient. (If the list is not sorted,
    % the predicate or function will either throw an exception or return
    % incorrect output.)
    %
:- func delete_sorted_list(map(K, V), list(K)) = map(K, V).
:- pred delete_sorted_list(list(K)::in, map(K, V)::in, map(K, V)::out)
    is det.

    % Delete a key-value pair from a map and return the value.
    % Fail if the key is not present.
    %
:- pred remove(K::in, V::out, map(K, V)::in, map(K, V)::out) is semidet.

    % Delete a key-value pair from a map and return the value.
    % Throw an exception if the key is not present.
    %
:- pred det_remove(K::in, V::out, map(K, V)::in, map(K, V)::out) is det.

    % Remove the smallest item from the map, fail if the map is empty.
    %
:- pred remove_smallest(K::out, V::out, map(K, V)::in, map(K, V)::out)
    is semidet.

    % Count the number of elements in the map.
    %
:- func count(map(K, V)) = int.
:- pred count(map(K, V)::in, int::out) is det.

    % Convert a pair of lists (which must be of the same length) to a map.
    %
:- func from_corresponding_lists(list(K), list(V)) = map(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in, map(K, V)::out)
    is det.

    % Merge the contents of the two maps.
    % Throws an exception if both sets of keys are not disjoint.
    %
    % The cost of this predicate is proportional to the number of elements
    % in the second map, so for efficiency, you want to put the bigger map
    % first and the smaller map second.
    %
:- func merge(map(K, V), map(K, V)) = map(K, V).
:- pred merge(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % For overlay(MapA, MapB, Map), if MapA and MapB both contain the
    % same key, then Map will map that key to the value from MapB.
    % In other words, MapB takes precedence over MapA.
    %
:- func overlay(map(K, V), map(K, V)) = map(K, V).
:- pred overlay(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % overlay_large_map(MapA, MapB, Map) performs the same task as
    % overlay(MapA, MapB, Map). However, while overlay takes time
    % proportional to the size of MapB, overlay_large_map takes time
    % proportional to the size of MapA. In other words, it preferable when
    % MapB is the larger map.
    %
:- func overlay_large_map(map(K, V), map(K, V)) = map(K, V).
:- pred overlay_large_map(map(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.

    % select takes a map and a set of keys, and returns a map
    % containing the keys in the set and their corresponding values.
    %
:- func select(map(K, V), set(K)) = map(K, V).
:- pred select(map(K, V)::in, set(K)::in, map(K, V)::out) is det.

    % select_sorted_list takes a map and a sorted list of keys, and returns
    % a map containing the keys in the list and their corresponding values.
    %
:- func select_sorted_list(map(K, V), list(K)) = map(K, V).
:- pred select_sorted_list(map(K, V)::in, list(K)::in, map(K, V)::out) is det.

    % Given a list of keys, produce a list of their corresponding
    % values in a specified map.
    %
:- func apply_to_list(list(K), map(K, V)) = list(V).
:- pred apply_to_list(list(K)::in, map(K, V)::in, list(V)::out) is det.

    % Declaratively, a NOP.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the map in the expectation
    % of a number of lookups but few or no modifications.
    %
:- func optimize(map(K, V)) = map(K, V).
:- pred optimize(map(K, V)::in, map(K, V)::out) is det.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate for each key-value pair.
    %
:- func foldl(func(K, V, A) = A, map(K, V), A) = A.
:- pred foldl(pred(K, V, A, A), map(K, V), A, A).
:- mode foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldl(pred(in, in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldl(pred(in, in, mdi, muo) is cc_multi, in, mdi, muo) is cc_multi.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with two accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl2(pred(K, V, A, A, B, B), map(K, V), A, A, B, B).
:- mode foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldl2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.
:- mode foldl2(pred(in, in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is cc_multi,
    in, in, out, mdi, muo) is cc_multi.
:- mode foldl2(pred(in, in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode foldl2(pred(in, in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with three accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl3(pred(K, V, A, A, B, B, C, C), map(K, V), A, A, B, B, C, C).
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with four accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl4(pred(K, V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldl4(pred(in, in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(pred(in, in, in, out, in, out, di, uo, di, uo) is det,
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldl4(pred(in, in, in, out, di, uo, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldl4(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with five accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl5(pred(K, V, A, A, B, B, C, C, D, D, E, E), map(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(pred(in, in,in, out,  in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % Perform an inorder traversal by key of the map, applying an accumulator
    % predicate for value.
    %
:- pred foldl_values(pred(V, A, A), map(K, V), A, A).
:- mode foldl_values(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl_values(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl_values(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl_values(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl_values(pred(in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode foldl_values(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl_values(pred(in, in, out) is cc_multi, in, in, out)
    is cc_multi.
:- mode foldl_values(pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldl_values(pred(in, mdi, muo) is cc_multi, in, mdi, muo)
    is cc_multi.

    % As above, but with two accumulators.
    %
:- pred foldl2_values(pred(V, A, A, B, B), map(K, V), A, A, B, B).
:- mode foldl2_values(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is det, in,
    in, out, mdi, muo) is det.
:- mode foldl2_values(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode foldl2_values(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldl2_values(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.
:- mode foldl2_values(pred(in, in, out, in, out) is cc_multi, in,
    in, out, in, out) is cc_multi.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is cc_multi, in,
    in, out, mdi, muo) is cc_multi.
:- mode foldl2_values(pred(in, in, out, di, uo) is cc_multi, in,
    in, out, di, uo) is cc_multi.

    % As above, but with three accumulators.
    %
:- pred foldl3_values(pred(V, A, A, B, B, C, C), map(K, V),
    A, A, B, B, C, C).
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is cc_multi, in,
    in, out, in, out, in, out) is cc_multi.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is cc_multi, in,
    in, out, in, out, mdi, muo) is cc_multi.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is cc_multi, in,
    in, out, in, out, di, uo) is cc_multi.

:- func foldr(func(K, V, A) = A, map(K, V), A) = A.
:- pred foldr(pred(K, V, A, A), map(K, V), A, A).
:- mode foldr(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldr(pred(in, in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldr(pred(in, in, mdi, muo) is cc_multi, in, mdi, muo)
    is cc_multi.
:- mode foldr(pred(in, in, di, uo) is cc_multi, in, di, uo) is cc_multi.

:- pred foldr2(pred(K, V, A, A, B, B), map(K, V), A, A, B, B).
:- mode foldr2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldr2(pred(in, in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode foldr2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldr2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldr2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldr2(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode foldr2(pred(in, in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

:- pred foldr3(pred(K, V, A, A, B, B, C, C), map(K, V), A, A, B, B, C, C).
:- mode foldr3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldr3(pred(in, in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldr3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldr3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode foldr3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.
:- mode foldr3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldr3(pred(in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldr3(pred(in, in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.

:- pred foldr4(pred(K, V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldr4(pred(in, in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldr4(pred(in, in, in, out, in, out, di, uo, di, uo) is det,
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldr4(pred(in, in, in, out, di, uo, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldr4(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldr5(pred(K, V, A, A, B, B, C, C, D, D, E, E), map(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % Apply a transformation predicate to all the values in a map.
    %
:- func map_values(func(K, V) = W, map(K, V)) = map(K, W).
:- pred map_values(pred(K, V, W), map(K, V), map(K, W)).
:- mode map_values(pred(in, in, out) is det, in, out) is det.
:- mode map_values(pred(in, in, out) is semidet, in, out) is semidet.

    % Same as map_values, but do not pass the key to the given predicate.
    %
:- func map_values_only(func(V) = W, map(K, V)) = map(K, W).
:- pred map_values_only(pred(V, W), map(K, V), map(K, W)).
:- mode map_values_only(pred(in, out) is det, in, out) is det.
:- mode map_values_only(pred(in, out) is semidet, in, out) is semidet.

    % Perform an inorder traversal by key of the map, applying a transformation
    % predicate to each value while updating an accumulator.
    %
:- pred map_foldl(pred(K, V, W, A, A), map(K, V), map(K, W), A, A).
:- mode map_foldl(pred(in, in, out, in, out) is det, in, out, in, out)
    is det.
:- mode map_foldl(pred(in, in, out, mdi, muo) is det, in, out, mdi, muo)
    is det.
:- mode map_foldl(pred(in, in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode map_foldl(pred(in, in, out, in, out) is semidet, in, out,
    in, out) is semidet.
:- mode map_foldl(pred(in, in, out, mdi, muo) is semidet, in, out,
    mdi, muo) is semidet.
:- mode map_foldl(pred(in, in, out, di, uo) is semidet, in, out,
    di, uo) is semidet.

    % As map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(K, V, W, A, A, B, B), map(K, V), map(K, W),
    A, A, B, B).
:- mode map_foldl2(pred(in, in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map_foldl2(pred(in, in, out, in, out, mdi, muo) is det,
    in, out, in, out, mdi, muo) is det.
:- mode map_foldl2(pred(in, in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map_foldl2(pred(in, in, out, di, uo, di, uo) is det,
    in, out, di, uo, di, uo) is det.
:- mode map_foldl2(pred(in, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.
:- mode map_foldl2(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl2(pred(in, in, out, in, out, di, uo) is semidet,
    in, out, in, out, di, uo) is semidet.

    % As map_foldl, but with three accumulators.
    %
:- pred map_foldl3(pred(K, V, W, A, A, B, B, C, C), map(K, V), map(K, W),
    A, A, B, B, C, C).
:- mode map_foldl3(pred(in, in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode map_foldl3(pred(in, in, out, di, uo, di, uo, di, uo) is det,
    in, out, di, uo, di, uo, di, uo) is det.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl3(pred(in, in, out, in, out, di, uo, di, uo) is det,
    in, out, in, out, di, uo, di, uo) is det.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, out, in, out, in, out, di, uo) is semidet.

    % As map_foldl, but without passing the key to the predicate.
    %
:- pred map_values_foldl(pred(V, W, A, A), map(K, V), map(K, W), A, A).
:- mode map_values_foldl(pred(in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode map_values_foldl(pred(in, out, in, out) is det,
    in, out, in, out) is det.
:- mode map_values_foldl(pred(in, out, in, out) is semidet,
    in, out, in, out) is semidet.

    % As map_values_foldl, but with two accumulators.
    %
:- pred map_values_foldl2(pred(V, W, A, A, B, B), map(K, V), map(K, W),
    A, A, B, B).
:- mode map_values_foldl2(pred(in, out, di, uo, di, uo) is det,
    in, out, di, uo, di, uo) is det.
:- mode map_values_foldl2(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map_values_foldl2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map_values_foldl2(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.

    % As map_values_foldl, but with three accumulators.
    %
:- pred map_values_foldl3(pred(V, W, A, A, B, B, C, C),
    map(K, V), map(K, W), A, A, B, B, C, C).
:- mode map_values_foldl3(pred(in, out, di, uo, di, uo, di, uo) is det,
    in, out, di, uo, di, uo, di, uo) is det.
:- mode map_values_foldl3(pred(in, out, in, out, di, uo, di, uo) is det,
    in, out, in, out, di, uo, di, uo) is det.
:- mode map_values_foldl3(pred(in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode map_values_foldl3(pred(in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode map_values_foldl3(
    pred(in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out) is semidet.

    % Given two maps M1 and M2, create a third map M3 that has only the
    % keys that occur in both M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- func intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).

:- pred intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode intersect(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode intersect(pred(in, in, out) is det, in, in, out) is det.

    % Calls intersect. Throws an exception if intersect fails.
    %
:- func det_intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode det_intersect(func(in, in) = out is semidet, in, in) = out is det.

:- pred det_intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode det_intersect(pred(in, in, out) is semidet, in, in, out) is det.

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
    % common_subset is very similar to intersect, but can succeed
    % even with an output map that does not contain an entry for a key
    % value that occurs in both input maps.
    %
:- func common_subset(map(K, V), map(K, V)) = map(K, V).

    % Given two maps M1 and M2, create a third map M3 that contains all
    % the keys that occur in either M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % closure to the values associated with the key in M1 and M2.
    % Fail if and only if this closure fails on the values associated
    % with some common key.
    %
:- func union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- pred union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode union(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode union(pred(in, in, out) is det, in, in, out) is det.

    % Calls union. Throws an exception if union fails.
    %
:- func det_union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode det_union(func(in, in) = out is semidet, in, in) = out is det.

:- pred det_union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode det_union(pred(in, in, out) is semidet, in, in, out) is det.

    % Consider the original map a set of key-value pairs. This predicate
    % returns a map that maps each value to the set of keys it is paired
    % with in the original map.
    %
:- func reverse_map(map(K, V)) = map(V, set(K)).

    % Field selection for maps.

    % Map ^ elem(Key) = search(Map, Key).
    %
:- func elem(K, map(K, V)) = V is semidet.

    % Map ^ det_elem(Key) = lookup(Map, Key).
    %
:- func det_elem(K, map(K, V)) = V.

    % Field update for maps.

    % (Map ^ elem(Key) := Value) = set(Map, Key, Value).
    %
:- func 'elem :='(K, map(K, V), V) = map(K, V).

    % (Map ^ det_elem(Key) := Value) = det_update(Map, Key, Value).
    %
:- func 'det_elem :='(K, map(K, V), V) = map(K, V).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term. % for var/1.
:- import_module tree234.

:- type map(K, V)   ==  tree234(K, V).

%---------------------------------------------------------------------------%

% Note to implementors:
%
% This is the old version of map.merge/3. It is buggy in the sense that if the
% sets of keys of the input maps are not disjoint it won't throw an exception
% but will insert the key and the smallest of the two corresponding values into
% the output map. Eventually we would like to get rid of this version but some
% of the code in the compiler currently assumes this behaviour and fixing it is
% non-trivial.

:- func map.old_merge(map(K, V), map(K, V)) = map(K, V).
:- pred map.old_merge(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma type_spec(map.search/3, K = var(_)).
:- pragma type_spec(map.search/3, K = int).

:- pragma type_spec(map.search/2, K = var(_)).
:- pragma type_spec(map.search/2, K = int).

:- pragma type_spec(map.lookup/3, K = var(_)).
:- pragma type_spec(map.lookup/3, K = int).

:- pragma type_spec(map.lookup/2, K = var(_)).
:- pragma type_spec(map.lookup/2, K = int).

:- pragma type_spec(map.insert(in, in, in, out), K = var(_)).
:- pragma type_spec(map.insert(in, in, in, out), K = int).

:- pragma type_spec(map.det_insert(in, in, in, out), K = var(_)).
:- pragma type_spec(map.det_insert(in, in, in, out), K = int).

:- pragma type_spec(map.set(in, in, in, out), K = var(_)).
:- pragma type_spec(map.set(in, in, in, out), K = int).

:- pragma type_spec(map.update(in, in, in, out), K = var(_)).
:- pragma type_spec(map.update(in, in, in, out), K = int).

:- pragma type_spec(map.det_update/4, K = var(_)).
:- pragma type_spec(map.det_update/4, K = int).

:- pragma type_spec(map.search_insert/5, K = var(_)).
:- pragma type_spec(map.search_insert/5, K = int).

:- pragma type_spec(map.overlay/2, K = var(_)).
:- pragma type_spec(map.overlay/3, K = var(_)).

:- pragma type_spec(map.select/2, K = var(_)).
:- pragma type_spec(map.select/3, K = var(_)).

:- pragma type_spec(map.select_sorted_list/2, K = var(_)).
:- pragma type_spec(map.select_sorted_list/3, K = var(_)).

:- pragma type_spec(map.elem/2, K = int).
:- pragma type_spec(map.elem/2, K = var(_)).

:- pragma type_spec(map.det_elem/2, K = int).
:- pragma type_spec(map.det_elem/2, K = var(_)).

:- pragma type_spec('elem :='/3, K = int).
:- pragma type_spec('elem :='/3, K = var(_)).

:- pragma type_spec('det_elem :='/3, K = int).
:- pragma type_spec('det_elem :='/3, K = var(_)).

:- implementation.

:- import_module int.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

map.init = M :-
    map.init(M).

map.init(M) :-
    tree234.init(M).

map.singleton(K, V) =
    tree234.singleton(K, V).

map.is_empty(M) :-
    tree234.is_empty(M).

map.equal(MapA, MapB) :-
    tree234.equal(MapA, MapB).

map.contains(Map, K) :-
    map.search(Map, K, _).

map.member(Map, K, V) :-
    tree234.member(Map, K, V).

map.search(M, K) = V :-
    map.search(M, K, V).

map.search(Map, K, V) :-
    tree234.search(Map, K, V).

map.lookup(M, K) = V :-
    map.lookup(M, K, V).

map.lookup(Map, K, V) :-
    ( if tree234.search(Map, K, VPrime) then
        V = VPrime
    else
        report_lookup_error("map.lookup: key not found", K, V)
    ).

map.inverse_search(Map, V, K) :-
    map.member(Map, K, V).

map.lower_bound_search(Map, SearchK, K, V) :-
    tree234.lower_bound_search(Map, SearchK, K, V).

map.lower_bound_lookup(Map, SearchK, K, V) :-
    ( if tree234.lower_bound_search(Map, SearchK, KPrime, VPrime) then
        K = KPrime,
        V = VPrime
    else
        report_lookup_error("map.lower_bound_lookup: key not found",
            SearchK, V)
    ).

map.upper_bound_search(Map, SearchK, K, V) :-
    tree234.upper_bound_search(Map, SearchK, K, V).

map.upper_bound_lookup(Map, SearchK, K, V) :-
    ( if tree234.upper_bound_search(Map, SearchK, KPrime, VPrime) then
        K = KPrime,
        V = VPrime
    else
        report_lookup_error("map.upper_bound_lookup: key not found",
            SearchK, V)
    ).

map.max_key(M) = tree234.max_key(M).

map.det_max_key(M) =
    ( if K = map.max_key(M) then
        K
    else
        unexpected($pred, "map.max_key failed")
    ).

map.min_key(M) = tree234.min_key(M).

map.det_min_key(M) =
    ( if K = map.min_key(M) then
        K
    else
        unexpected($pred, "map.min_key failed")
    ).

map.insert(M1, K, V) = M2 :-
    map.insert(K, V, M1, M2).

map.insert(K, V, !Map) :-
    tree234.insert(K, V, !Map).

map.det_insert(M1, K, V) = M2 :-
    map.det_insert(K, V, M1, M2).

map.det_insert(K, V, !Map) :-
    ( if tree234.insert(K, V, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("map.det_insert: key already present", K, V)
    ).

map.det_insert_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    map.det_insert_from_corresponding_lists(Ks, Vs, M1, M2).

map.det_insert_from_corresponding_lists([], [], !Map).
map.det_insert_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
map.det_insert_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
map.det_insert_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    map.det_insert(K, V, !Map),
    map.det_insert_from_corresponding_lists(Ks, Vs, !Map).

map.det_insert_from_assoc_list(M1, AL) = M2 :-
    map.det_insert_from_assoc_list(AL, M1, M2).

map.det_insert_from_assoc_list([], !Map).
map.det_insert_from_assoc_list([K - V | KVs], !Map) :-
    map.det_insert(K, V, !Map),
    map.det_insert_from_assoc_list(KVs, !Map).

map.set_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    map.set_from_corresponding_lists(Ks, Vs, M1, M2).

map.set_from_corresponding_lists([], [], !Map).
map.set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
map.set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
map.set_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    map.set(K, V, !Map),
    map.set_from_corresponding_lists(Ks, Vs, !Map).

map.set_from_assoc_list(M1, AL) = M2 :-
    map.set_from_assoc_list(AL, M1, M2).

map.set_from_assoc_list([], !Map).
map.set_from_assoc_list([K - V | KVs], !Map) :-
    map.set(K, V, !Map),
    map.set_from_assoc_list(KVs, !Map).

map.update(M0, K, V) = M :-
    map.update(K, V, M0, M).

map.update(K, V, !Map) :-
    tree234.update(K, V, !Map).

map.det_update(M0, K, V) = M :-
    map.det_update(K, V, M0, M).

map.det_update(K, V, !Map) :-
    ( if tree234.update(K, V, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("map.det_update: key not found", K, V)
    ).

map.search_insert(K, V, MaybeOldV, !Map) :-
    tree234.search_insert(K, V, MaybeOldV, !Map).

map.transform_value(P, K, !Map) :-
    tree234.transform_value(P, K, !Map).

map.det_transform_value(F, K, !.Map) = !:Map :-
    map.det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), K,
        !Map).

map.det_transform_value(P, K, !Map) :-
    ( if map.transform_value(P, K, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("map.det_transform_value: key not found", K)
    ).

map.set(M1, K, V) = M2 :-
    map.set(K, V, M1, M2).

map.set(K, V, !Map) :-
    tree234.set(K, V, !Map).

map.keys(M) = Ks :-
    map.keys(M, Ks).

map.keys(Map, KeyList) :-
    tree234.keys(Map, KeyList).

map.sorted_keys(M) = Ks :-
    map.sorted_keys(M, Ks).

map.sorted_keys(Map, KeyList) :-
    % Guaranteed to yield sorted lists.
    tree234.keys(Map, KeyList).

map.values(M) = Vs :-
    map.values(M, Vs).

map.values(Map, KeyList) :-
    tree234.values(Map, KeyList).

map.keys_and_values(Map, KeyList, ValueList) :-
    tree234.keys_and_values(Map, KeyList, ValueList).

map.to_assoc_list(M) = AL :-
    map.to_assoc_list(M, AL).

map.to_assoc_list(M, L) :-
    tree234.tree234_to_assoc_list(M, L).

map.to_sorted_assoc_list(M) = AL :-
    map.to_sorted_assoc_list(M, AL).

map.to_sorted_assoc_list(M, L) :-
    % Guaranteed to yield sorted lists.
    tree234.tree234_to_assoc_list(M, L).

map.from_assoc_list(AL) = M :-
    map.from_assoc_list(AL, M).

map.from_assoc_list(L, M) :-
    tree234.assoc_list_to_tree234(L, M).

map.from_sorted_assoc_list(AL) = M :-
    map.from_sorted_assoc_list(AL, M).

map.from_sorted_assoc_list(L, M) :-
    tree234.from_sorted_assoc_list(L, M).

map.from_rev_sorted_assoc_list(AL) = M :-
    map.from_rev_sorted_assoc_list(AL, M).

map.from_rev_sorted_assoc_list(L, M) :-
    tree234.from_rev_sorted_assoc_list(L, M).

map.delete(M0, K) = M :-
    map.delete(K, M0, M).

map.delete(Key, !Map) :-
    tree234.delete(Key, !Map).

map.delete_list(M0, Ks) = M :-
    map.delete_list(Ks, M0, M).

map.delete_list([], !Map).
map.delete_list([DeleteKey | DeleteKeys], !Map) :-
    map.delete(DeleteKey, !Map),
    map.delete_list(DeleteKeys, !Map).

map.delete_sorted_list(M0, Ks) = M :-
    map.delete_sorted_list(Ks, M0, M).

map.delete_sorted_list(DeleteKeys, !Map) :-
    list.length(DeleteKeys, NumDeleteKeys),
    find_min_size_based_on_depth(!.Map, MinSize),
    ( if NumDeleteKeys * 5 < MinSize then
        % Use this technique when we delete fewer than 20% of the keys.
        map.delete_list(DeleteKeys, !Map)
    else
        % Use this technique when we delete at least 20% of the keys.
        map.to_assoc_list(!.Map, Pairs0),
        map.delete_sorted_list_loop(DeleteKeys, Pairs0, [], RevPairs,
            LeftOverPairs),
        reverse_list_acc(RevPairs, LeftOverPairs, Pairs),
        % Pairs = list.reverse(RevPairs) ++ LeftOverPairs,
        map.from_assoc_list(Pairs, !:Map)
    ).

:- pred map.delete_sorted_list_loop(list(K)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::in, assoc_list(K, V)::out,
    assoc_list(K, V)::out) is det.

map.delete_sorted_list_loop([], Pairs, !RevPairs, Pairs).
map.delete_sorted_list_loop([_ | _], [], !RevPairs, []).
map.delete_sorted_list_loop([DeleteKey | DeleteKeys], [Pair0 | Pairs0],
        !RevPairs, LeftOverPairs) :-
    Pair0 = Key0 - _,
    compare(Result, DeleteKey, Key0),
    (
        Result = (<),
        map.delete_sorted_list_loop(DeleteKeys, [Pair0 | Pairs0],
            !RevPairs, LeftOverPairs)
    ;
        Result = (=),
        map.delete_sorted_list_loop(DeleteKeys, Pairs0,
            !RevPairs, LeftOverPairs)
    ;
        Result = (>),
        !:RevPairs = [Pair0 | !.RevPairs],
        map.delete_sorted_list_loop([DeleteKey | DeleteKeys], Pairs0,
            !RevPairs, LeftOverPairs)
    ).

:- pred reverse_list_acc(list(T)::in, list(T)::in, list(T)::out) is det.

reverse_list_acc([], L, L).
reverse_list_acc([X | Xs], L0, L) :-
    reverse_list_acc(Xs, [X | L0], L).

map.remove(Key, Value, !Map) :-
    tree234.remove(Key, Value, !Map).

map.det_remove(Key, Value, !Map) :-
    ( if tree234.remove(Key, ValuePrime, !.Map, MapPrime) then
        Value = ValuePrime,
        !:Map = MapPrime
    else
        report_lookup_error("map.det_remove: key not found", Key, Value)
    ).

map.remove_smallest(K, V, !Map) :-
    tree234.remove_smallest(K, V, !Map).

map.count(M) = N :-
    map.count(M, N).

map.count(Map, Count) :-
    tree234.count(Map, Count).

%---------------------------------------------------------------------------%

map.from_corresponding_lists(Ks, Vs) = M :-
    map.from_corresponding_lists(Ks, Vs, M).

map.from_corresponding_lists(Keys, Values, Map) :-
    assoc_list.from_corresponding_lists(Keys, Values, AssocList),
    tree234.assoc_list_to_tree234(AssocList, Map).

%---------------------------------------------------------------------------%

map.merge(M1, M2) = M3 :-
    map.merge(M1, M2, M3).

map.merge(MA, MB, M) :-
    % You may wish to compare this to old_merge below.
    map.to_assoc_list(MB, MBList),
    map.det_insert_from_assoc_list(MBList, MA, M).

%---------------------------------------------------------------------------%

map.overlay(M1, M2) = M3 :-
    map.overlay(M1, M2, M3).

map.overlay(Map0, Map1, Map) :-
    map.to_assoc_list(Map1, AssocList),
    map.overlay_2(AssocList, Map0, Map).

:- pred map.overlay_2(assoc_list(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.
:- pragma type_spec(map.overlay_2/3, K = var(_)).

map.overlay_2([], !Map).
map.overlay_2([K - V | AssocList], !Map) :-
    map.set(K, V, !Map),
    map.overlay_2(AssocList, !Map).

map.overlay_large_map(M1, M2) = M3 :-
    map.overlay_large_map(M1, M2, M3).

map.overlay_large_map(Map0, Map1, Map) :-
    map.to_assoc_list(Map0, AssocList),
    map.overlay_large_map_2(AssocList, Map1, Map).

:- pred map.overlay_large_map_2(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.
:- pragma type_spec(map.overlay_large_map_2/3, K = var(_)).

map.overlay_large_map_2([], Map, Map).
map.overlay_large_map_2([K - V | AssocList], Map0, Map) :-
    ( if map.insert(K, V, Map0, Map1) then
        Map2 = Map1
    else
        Map2 = Map0
    ),
    map.overlay_large_map_2(AssocList, Map2, Map).

%---------------------------------------------------------------------------%

map.select(M1, S) = M2 :-
    map.select(M1, S, M2).

map.select(Original, KeySet, NewMap) :-
    set.to_sorted_list(KeySet, Keys),
    map.init(NewMap0),
    map.select_loop(Keys, Original, NewMap0, NewMap).

map.select_sorted_list(M1, S) = M2 :-
    map.select_sorted_list(M1, S, M2).

map.select_sorted_list(Original, Keys, NewMap) :-
    map.init(NewMap0),
    map.select_loop(Keys, Original, NewMap0, NewMap).

:- pred map.select_loop(list(K)::in, map(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.
:- pragma type_spec(map.select_loop/4, K = var(_)).

map.select_loop([], _Original, !New).
map.select_loop([K | Ks], Original, !New) :-
    ( if map.search(Original, K, V) then
        map.det_insert(K, V, !New)
    else
        true
    ),
    map.select_loop(Ks, Original, !New).

%---------------------------------------------------------------------------%

map.apply_to_list(Ks, M) = Vs :-
    map.apply_to_list(Ks, M, Vs).

map.apply_to_list([], _, []).
map.apply_to_list([K | Ks], Map, [V | Vs]) :-
    map.lookup(Map, K, V),
    map.apply_to_list(Ks, Map, Vs).

%---------------------------------------------------------------------------%

map.optimize(M1) = M2 :-
    map.optimize(M1, M2).

map.optimize(Map, Map).

%---------------------------------------------------------------------------%

map.foldl(F, M, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    map.foldl(P, M, A, B).

map.foldl(Pred, Map, !A) :-
    tree234.foldl(Pred, Map, !A).

map.foldl2(Pred, Map, !A, !B) :-
    tree234.foldl2(Pred, Map, !A, !B).

map.foldl3(Pred, Map, !A, !B, !C) :-
    tree234.foldl3(Pred, Map, !A, !B, !C).

map.foldl4(Pred, Map, !A, !B, !C, !D) :-
    tree234.foldl4(Pred, Map, !A, !B, !C, !D).

map.foldl5(Pred, Map, !A, !B, !C, !D, !E) :-
    tree234.foldl5(Pred, Map, !A, !B, !C, !D, !E).

map.foldl_values(Pred, Map, !A) :-
    tree234.foldl_values(Pred, Map, !A).

map.foldl2_values(Pred, Map, !A, !B) :-
    tree234.foldl2_values(Pred, Map, !A, !B).

map.foldl3_values(Pred, Map, !A, !B, !C) :-
    tree234.foldl3_values(Pred, Map, !A, !B, !C).

map.foldr(F, M, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    map.foldr(P, M, A, B).

map.foldr(Pred, Map, !A) :-
    tree234.foldr(Pred, Map, !A).

map.foldr2(Pred, Map, !A, !B) :-
    tree234.foldr2(Pred, Map, !A, !B).

map.foldr3(Pred, Map, !A, !B, !C) :-
    tree234.foldr3(Pred, Map, !A, !B, !C).

map.foldr4(Pred, Map, !A, !B, !C, !D) :-
    tree234.foldr4(Pred, Map, !A, !B, !C, !D).

map.foldr5(Pred, Map, !A, !B, !C, !D, !E) :-
    tree234.foldr5(Pred, Map, !A, !B, !C, !D, !E).

%---------------------------------------------------------------------------%

map.map_values(F, M1) = M2 :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.map_values(P, M1, M2).

map.map_values(Pred, Map0, Map) :-
    tree234.map_values(Pred, Map0, Map).

map.map_values_only(F, M1) = M2 :-
    P = (pred(Y::in, Z::out) is det :- Z = F(Y) ),
    map.map_values_only(P, M1, M2).

map.map_values_only(Pred, Map0, Map) :-
    tree234.map_values_only(Pred, Map0, Map).

map.map_foldl(Pred, !Map, !AccA) :-
    tree234.map_foldl(Pred, !Map, !AccA).

map.map_foldl2(Pred, !Map, !AccA, !AccB) :-
    tree234.map_foldl2(Pred, !Map, !AccA, !AccB).

map.map_foldl3(Pred, !Map, !AccA, !AccB, !AccC) :-
    tree234.map_foldl3(Pred, !Map, !AccA, !AccB, !AccC).

map.map_values_foldl(Pred, !Map, !AccA) :-
    tree234.map_values_foldl(Pred, !Map, !AccA).

map.map_values_foldl2(Pred, !Map, !AccA, !AccB) :-
    tree234.map_values_foldl2(Pred, !Map, !AccA, !AccB).

map.map_values_foldl3(Pred, !Map, !AccA, !AccB, !AccC) :-
    tree234.map_values_foldl3(Pred, !Map, !AccA, !AccB, !AccC).

%---------------------------------------------------------------------------%

map.intersect(F, M1, M2) = M3 :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.intersect(P, M1, M2, M3).

map.intersect(CommonPred, Map1, Map2, Common) :-
    map.to_sorted_assoc_list(Map1, AssocList1),
    map.to_sorted_assoc_list(Map2, AssocList2),
    map.intersect_loop(AssocList1, AssocList2, CommonPred,
        [], RevCommonAssocList),
    map.from_rev_sorted_assoc_list(RevCommonAssocList, Common).

:- pred map.intersect_loop(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    assoc_list(K, V), assoc_list(K, V)).
:- mode map.intersect_loop(in, in, pred(in, in, out) is semidet, in, out)
    is semidet.
:- mode map.intersect_loop(in, in, pred(in, in, out) is det, in, out)
    is det.

map.intersect_loop(AssocList1, AssocList2, CommonPred, !RevCommonAssocList) :-
    (
        AssocList1 = [],
        AssocList2 = []
    ;
        AssocList1 = [_ | _],
        AssocList2 = []
    ;
        AssocList1 = [],
        AssocList2 = [_ | _]
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            CommonPred(Value1, Value2, Value),
            !:RevCommonAssocList = [Key1 - Value | !.RevCommonAssocList],
            map.intersect_loop(AssocTail1, AssocTail2, CommonPred,
                !RevCommonAssocList)
        ;
            R = (<),
            map.intersect_loop(AssocTail1, AssocList2, CommonPred,
                !RevCommonAssocList)
        ;
            R = (>),
            map.intersect_loop(AssocList1, AssocTail2, CommonPred,
                !RevCommonAssocList)
        )
    ).

map.det_intersect(PF, M1, M2) = M3 :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    map.det_intersect(P, M1, M2, M3).

map.det_intersect(CommonPred, Map1, Map2, Common) :-
    ( if map.intersect(CommonPred, Map1, Map2, CommonPrime) then
        Common = CommonPrime
    else
        unexpected($pred, "map.intersect failed")
    ).

%---------------------------------------------------------------------------%

map.common_subset(Map1, Map2) = Common :-
    map.to_sorted_assoc_list(Map1, AssocList1),
    map.to_sorted_assoc_list(Map2, AssocList2),
    map.common_subset_loop(AssocList1, AssocList2, [], RevCommonAssocList),
    map.from_rev_sorted_assoc_list(RevCommonAssocList, Common).

:- pred map.common_subset_loop(assoc_list(K, V)::in, assoc_list(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

map.common_subset_loop(AssocList1, AssocList2, !RevCommonAssocList) :-
    (
        AssocList1 = [],
        AssocList2 = []
    ;
        AssocList1 = [_ | _],
        AssocList2 = []
    ;
        AssocList1 = [],
        AssocList2 = [_ | _]
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            ( if Value1 = Value2 then
                !:RevCommonAssocList = [Key1 - Value1 | !.RevCommonAssocList]
            else
                true
            ),
            map.common_subset_loop(AssocTail1, AssocTail2, !RevCommonAssocList)
        ;
            ( R = (<)
            ; R = (>)
            ),
            map.common_subset_loop(AssocList1, AssocTail2, !RevCommonAssocList)
        )
    ).

%---------------------------------------------------------------------------%

map.union(F, M1, M2) = M3 :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.union(P, M1, M2, M3).

map.union(CommonPred, Map1, Map2, Common) :-
    map.to_sorted_assoc_list(Map1, AssocList1),
    map.to_sorted_assoc_list(Map2, AssocList2),
    map.union_loop(AssocList1, AssocList2, CommonPred, [], RevCommonAssocList),
    map.from_rev_sorted_assoc_list(RevCommonAssocList, Common).

    % The real intended modes of this predicate are the last two.
    % The first four modes are just specialized versions for use by
    % recursive calls after it has been determined that one or other input
    % list has run out of elements. These specialized versions don't do
    % redundant tests to see whether the known-empty list is empty or not.
    %
:- pred map.union_loop(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    assoc_list(K, V), assoc_list(K, V)).
:- mode map.union_loop(in(bound([])), in, pred(in, in, out) is semidet,
    in, out) is semidet.
:- mode map.union_loop(in(bound([])), in, pred(in, in, out) is det, in, out)
    is det.
:- mode map.union_loop(in, in(bound([])), pred(in, in, out) is semidet,
    in, out) is semidet.
:- mode map.union_loop(in, in(bound([])), pred(in, in, out) is det, in, out)
    is det.
:- mode map.union_loop(in, in, pred(in, in, out) is semidet, in, out)
    is semidet.
:- mode map.union_loop(in, in, pred(in, in, out) is det, in, out)
    is det.

map.union_loop(AssocList1, AssocList2, CommonPred, !RevCommonAssocList) :-
    (
        AssocList1 = [],
        AssocList2 = []
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [],
        !:RevCommonAssocList = [Key1 - Value1 | !.RevCommonAssocList],
        map.union_loop(AssocTail1, AssocList2, CommonPred, !RevCommonAssocList)
    ;
        AssocList1 = [],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        !:RevCommonAssocList = [Key2 - Value2 | !.RevCommonAssocList],
        map.union_loop(AssocList1, AssocTail2, CommonPred, !RevCommonAssocList)
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            CommonPred(Value1, Value2, Value),
            !:RevCommonAssocList = [Key1 - Value | !.RevCommonAssocList],
            map.union_loop(AssocTail1, AssocTail2, CommonPred,
                !RevCommonAssocList)
        ;
            R = (<),
            !:RevCommonAssocList = [Key1 - Value1 | !.RevCommonAssocList],
            map.union_loop(AssocTail1, AssocList2, CommonPred,
                !RevCommonAssocList)
        ;
            R = (>),
            !:RevCommonAssocList = [Key2 - Value2 | !.RevCommonAssocList],
            map.union_loop(AssocList1, AssocTail2, CommonPred,
                !RevCommonAssocList)
        )
    ).

map.det_union(F, M1, M2) = M3 :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = F(X, Y) ),
    map.det_union(P, M1, M2, M3).

map.det_union(CommonPred, Map1, Map2, Union) :-
    ( if map.union(CommonPred, Map1, Map2, UnionPrime) then
        Union = UnionPrime
    else
        unexpected($pred, "map.union failed")
    ).

%-----------------------------------------------------------------------------%

map.reverse_map(Map) = RevMap :-
    map.foldl(map.reverse_map_2, Map, map.init, RevMap).

:- pred map.reverse_map_2(K::in, V::in,
    map(V, set(K))::in, map(V, set(K))::out) is det.

map.reverse_map_2(Key, Value, !RevMap) :-
    ( if map.search(!.RevMap, Value, Keys0) then
        set.insert(Key, Keys0, Keys),
        map.det_update(Value, Keys, !RevMap)
    else
        map.det_insert(Value, set.make_singleton_set(Key), !RevMap)
    ).

map.elem(Key, Map) = map.search(Map, Key).

map.det_elem(Key, Map) = map.lookup(Map, Key).

'elem :='(Key, Map, Value) = map.set(Map, Key, Value).

'det_elem :='(Key, Map, Value) = map.det_update(Map, Key, Value).

%---------------------------------------------------------------------------%

map.old_merge(M1, M2) = M3 :-
    map.old_merge(M1, M2, M3).

map.old_merge(M0, M1, M) :-
    map.to_assoc_list(M0, ML0),
    map.to_assoc_list(M1, ML1),
    list.merge(ML0, ML1, ML),
    % ML may be sorted, but it may contain duplicates.
    map.from_assoc_list(ML, M).

%---------------------------------------------------------------------------%
:- end_module map.
%---------------------------------------------------------------------------%
