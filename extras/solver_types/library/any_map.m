%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% any_map.m
% Ralph Becket <rafe@cs.mu.oz.au>
%
% A copy of map.m adapted for maps from ground keys to values with inst any.
%
%---------------------------------------------------------------------------%

:- module any_map.
:- interface.

:- import_module any_assoc_list.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type any_map(K, V).

%---------------------------------------------------------------------------%

    % Initialize an empty map.
    %
:- func init = (any_map(K, V)::oa) is det.
:- pred init(any_map(K, V)::oa) is det.

    % Check whether a map is empty.
    %
:- pred is_empty(any_map(K, V)::ia) is semidet.

    % Check whether map contains key
    %
:- pred contains(any_map(K, V)::ia, K::in) is semidet.

:- pred member(any_map(K, V)::ia, K::out, V::oa) is nondet.

    % Search map for key.
    %
:- func search(any_map(K, V)::ia, K::in) = (V::oa) is semidet.
:- pred search(any_map(K, V)::ia, K::in, V::oa) is semidet.

    % Search map for key, but abort if search fails.
    %
:- func lookup(any_map(K, V)::ia, K::in) = (V::oa) is det.
:- pred lookup(any_map(K, V)::ia, K::in, V::oa) is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred lower_bound_search(any_map(K, V)::ia, K::in, K::out, V::oa) is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Aborts if there is no key with the given or lower value.
    %
:- pred lower_bound_lookup(any_map(K, V)::ia, K::in, K::out, V::oa) is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred upper_bound_search(any_map(K, V)::ia, K::in, K::out, V::oa) is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Aborts if there is no key with the given or higher value.
    %
:- pred upper_bound_lookup(any_map(K, V)::ia, K::in, K::out, V::oa) is det.

    % Return the largest key in the map, if there is one.
    %
:- func max_key(any_map(K,V)::ia) = (K::out) is semidet.

    % Return the smallest key in the map, if there is one.
    %
:- func min_key(any_map(K,V)::ia) = (K::out) is semidet.

    % Search map for data.
    %
:- pred inverse_search(any_map(K, V)::ia, V::ia, K::out) is nondet.

    % Insert a new key and corresponding value into a map.
    % Fail if the key already exists.
    %
:- func insert(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
    is semidet.
:- pred insert(K::in, V::ia, any_map(K, V)::ia, any_map(K, V)::oa) is semidet.

    % Insert a new key and corresponding value into a map.
    % Abort if the key already exists.
    %
:- func det_insert(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
    is det.
:- pred det_insert(K::in, V::ia, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Apply det_insert to key - value pairs from corresponding lists.
    % NOTE: this is not defined if the key values are not (semantically)
    % ground.
    %
:- func det_insert_from_corresponding_lists(any_map(K, V)::ia,
    list(K)::in, list(V)::ia) = (any_map(K, V)::oa) is det.
:- pred det_insert_from_corresponding_lists(list(K)::in, list(V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Apply det_insert to key - value pairs from the any_assoc_lists.
    % NOTE: this is not defined if the key values are not (semantically)
    % ground.
    %
:- func det_insert_from_any_assoc_list(any_map(K, V)::ia,
    any_assoc_list(K, V)::ia) = (any_map(K, V)::oa) is det.
:- pred det_insert_from_any_assoc_list(any_assoc_list(K, V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Apply set to key - value pairs from corresponding lists.
    %
:- func set_from_corresponding_lists(any_map(K, V)::ia, list(K)::in,
    list(V)::ia) = (any_map(K, V)::oa) is det.
:- pred set_from_corresponding_lists(list(K)::in, list(V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

:- func set_from_any_assoc_list(any_map(K, V)::ia,
    any_assoc_list(K, V)::ia) = (any_map(K, V)::oa) is det.
:- pred set_from_any_assoc_list(any_assoc_list(K, V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Update the value corresponding to a given key
    % Fail if the key doesn't already exist.
    %
:- func update(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
    is semidet.
:- pred update(K::in, V::ia, any_map(K, V)::ia, any_map(K, V)::oa) is semidet.

    % Update the value corresponding to a given key
    % Abort if the key doesn't already exist.
    %
:- func det_update(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
    is det.
:- pred det_update(K::in, V::ia, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Update the value at the given key by applying the supplied
    % transformation to it. Fails if the key is not found. This is faster
    % than first searching for the value and then updating it.
    %
:- pred transform_value(pred(V, V)::in(pred(ia, oa) is det), K::in,
    any_map(K, V)::ia, any_map(K, V)::oa) is semidet.

    % Same as transform_value/4, but aborts instead of failing if the
    % key is not found.
    %
:- func det_transform_value((func(V) = V)::in(func(ia) = oa is det),
    K::in, any_map(K, V)::ia) = (any_map(K, V)::oa) is det.
:- pred det_transform_value(pred(V, V)::in(pred(ia, oa) is det),
    K::in, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Update value if the key is already present, otherwise
    % insert new key and value.
    %
:- func set(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa) is det.
:- pred set(K::in, V::ia, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Given a map, return a list of all the keys in the map.
    %
:- func keys(any_map(K, V)::ia) = (list(K)::out) is det.
:- pred keys(any_map(K, V)::ia, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % in sorted order.
    %
:- func sorted_keys(any_map(K, V)::ia) = (list(K)::out) is det.
:- pred sorted_keys(any_map(K, V)::ia, list(K)::out) is det.

    % Given a map, return a list of all the data values in the map.
    %
:- func values(any_map(K, V)::ia) = (list(V)::oa) is det.
:- pred values(any_map(K, V)::ia, list(V)::oa) is det.

    % Convert a map to an association list.
    %
:- func to_any_assoc_list(any_map(K, V)::ia) = (any_assoc_list(K, V)::oa)
    is det.
:- pred to_any_assoc_list(any_map(K, V)::ia, any_assoc_list(K, V)::oa) is det.

    % Convert a map to an association list which is sorted on the keys.
    %
:- func to_sorted_any_assoc_list(any_map(K, V)::ia)
    = (any_assoc_list(K, V)::oa) is det.
:- pred to_sorted_any_assoc_list(any_map(K, V)::ia, any_assoc_list(K, V)::oa)
    is det.

    % Convert an association list to a map.
    %
:- func from_any_assoc_list(any_assoc_list(K, V)::ia)
    = (any_map(K, V)::oa) is det.
:- pred from_any_assoc_list(any_assoc_list(K, V)::ia, any_map(K, V)::oa)
    is det.

    % Convert a sorted association list to a map.
    %
:- func from_sorted_any_assoc_list(any_assoc_list(K, V)::ia)
    = (any_map(K, V)::oa) is det.
:- pred from_sorted_any_assoc_list(any_assoc_list(K, V)::ia,
    any_map(K, V)::oa) is det.

    % Delete a key-value pair from a map.
    % If the key is not present, leave the map unchanged.
    %
:- func delete(any_map(K, V)::ia, K::in) = (any_map(K, V)::oa) is det.
:- pred delete(K::in, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Apply delete/3 to a list of keys.
    %
:- func delete_list(any_map(K, V)::ia, list(K)::in)
    = (any_map(K, V)::oa) is det.
:- pred delete_list(list(K)::in, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Delete a key-value pair from a map and return the value.
    % Fail if the key is not present.
    %
:- pred remove(K::in, V::oa, any_map(K, V)::ia, any_map(K, V)::oa) is semidet.

    % Delete a key-value pair from a map and return the value.
    % Abort if the key is not present.
    %
:- pred det_remove(K::in, V::oa, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Count the number of elements in the map.
    %
:- func count(any_map(K, V)::ia) = (int::out) is det.
:- pred count(any_map(K, V)::ia, int::out) is det.

    % Convert a pair of lists (which must be of the same length)
    % to a map.
    %
:- func from_corresponding_lists(list(K)::in, list(V)::ia)
    = (any_map(K, V)::oa) is det.
:- pred from_corresponding_lists(list(K)::in, list(V)::ia,
    any_map(K, V)::oa) is det.

    % For merge(MapA, MapB, Map), MapA and MapB must
    % not both contain the same key.
    %
:- func merge(any_map(K, V)::ia, any_map(K, V)::ia) = (any_map(K, V)::oa)
    is det.
:- pred merge(any_map(K, V)::ia, any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % For overlay(MapA, MapB, Map), if MapA and MapB both contain the same key,
    % then Map will map that key to the value from MapB.
    % In otherwords, MapB takes precedence over MapA.
    %
:- func overlay(any_map(K, V)::ia, any_map(K, V)::ia) = (any_map(K, V)::oa)
    is det.
:- pred overlay(any_map(K, V)::ia, any_map(K, V)::ia, any_map(K, V)::oa)
    is det.

    % overlay_large_map(MapA, MapB, Map) performs the same task as
    % overlay(MapA, MapB, Map). However, while overlay takes time that is
    % proportional to the size of MapB, overlay_large_map takes time
    % proportional to the size of MapA. In other words, it is preferable
    % when MapB is a large map.
    %
:- func overlay_large_map(any_map(K, V)::ia, any_map(K, V)::ia)
    = (any_map(K, V)::oa) is det.
:- pred overlay_large_map(any_map(K, V)::ia, any_map(K, V)::ia,
    any_map(K, V)::oa) is det.

    % select takes a map and a set of keys, and returns a map containing
    % the keys in the set and their corresponding values.
    %
:- func select(any_map(K, V)::ia, set(K)::in) = (any_map(K, V)::oa) is det.
:- pred select(any_map(K, V)::ia, set(K)::in, any_map(K, V)::oa) is det.

    % Given a list of keys, produce a list of their corresponding
    % values in a specified map.
    %
:- func apply_to_list(list(K)::in, any_map(K, V)::ia) = (list(V)::oa) is det.
:- pred apply_to_list(list(K)::in, any_map(K, V)::ia, list(V)::oa) is det.

    % Declaratively, a NOP.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the map in the expectation
    % of a number of lookups but few or no modifications.
    %
:- func optimize(any_map(K, V)::ia) = (any_map(K, V)::oa) is det.
:- pred optimize(any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Remove the smallest item from the map, fail if
    % the map is empty.
    %
:- pred remove_smallest(K::out, V::oa, any_map(K, V)::ia, any_map(K, V)::oa)
    is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate for each key-value pair.
    %
:- func foldl(func(K, V, T) = T, any_map(K, V), T) = T.
:- mode foldl(func(in, ia, in) = out is det, ia, in) = out is det.
:- mode foldl(func(in, ia, ia) = oa is det, ia, ia) = oa is det.

:- pred foldl(pred(K, V, T, T), any_map(K, V), T, T).
:- mode foldl(pred(in, ia, di, uo) is det, ia, di, uo) is det.
:- mode foldl(pred(in, ia, in, out) is det, ia, in, out) is det.
:- mode foldl(pred(in, ia, in, out) is semidet, ia, in, out) is semidet.
:- mode foldl(pred(in, ia, ia, oa) is det, ia, ia, oa) is det.
:- mode foldl(pred(in, ia, ia, oa) is semidet, ia, ia, oa) is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate with two accumulators for
    % each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl2(pred(K, V, T, T, U, U), any_map(K, V), T, T, U, U).
:- mode foldl2(pred(in, ia, di, uo, di, uo) is det,
    ia, di, uo, di, uo) is det.
:- mode foldl2(pred(in, ia, in, out, di, uo) is det,
    ia, in, out, di, uo) is det.
:- mode foldl2(pred(in, ia, ia, oa, di, uo) is det,
    ia, ia, oa, di, uo) is det.
:- mode foldl2(pred(in, ia, in, out, in, out) is det,
    ia, in, out, in, out) is det.
:- mode foldl2(pred(in, ia, in, out, in, out) is semidet,
    ia, in, out, in, out) is semidet.
:- mode foldl2(pred(in, ia, ia, oa, in, out) is det,
    ia, ia, oa, in, out) is det.
:- mode foldl2(pred(in, ia, ia, oa, in, out) is semidet,
    ia, ia, oa, in, out) is semidet.
:- mode foldl2(pred(in, ia, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa) is det.
:- mode foldl2(pred(in, ia, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa) is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate with three accumulators for
    % each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl3(pred(K, V, T, T, U, U, W, W),
    any_map(K, V), T, T, U, U, W, W).
:- mode foldl3(pred(in, ia, di, uo, di, uo, di, uo) is det,
    ia, di, uo, di, uo, di, uo) is det.
:- mode foldl3(pred(in, ia, in, out, di, uo, di, uo) is det,
    ia, in, out, di, uo, di, uo) is det.
:- mode foldl3(pred(in, ia, in, out, in, out, di, uo) is det,
    ia, in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, ia, in, out, in, out, in, out) is det,
    ia, in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, ia, in, out, in, out, in, out) is semidet,
    ia, in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, ia, ia, oa, in, out, in, out) is det,
    ia, ia, oa, in, out, in, out) is det.
:- mode foldl3(pred(in, ia, ia, oa, in, out, in, out) is semidet,
    ia, ia, oa, in, out, in, out) is semidet.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, in, out) is det,
    ia, ia, oa, ia, oa, in, out) is det.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, in, out) is semidet,
    ia, ia, oa, ia, oa, in, out) is semidet.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa, ia, oa) is det.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa, ia, oa) is semidet.

    % Apply a transformation predicate to all the values
    % in a map.
    %
:- func map_values(func(K, V) = W, any_map(K, V)) = any_map(K, W).
:- mode map_values(func(in, ia) = oa is det, ia) = oa is det.

:- pred map_values(pred(K, V, W), any_map(K, V), any_map(K, W)).
:- mode map_values(pred(in, ia, oa) is det, ia, oa) is det.
:- mode map_values(pred(in, ia, oa) is semidet, ia, oa) is semidet.

    % Apply a transformation predicate to all the values
    % in a map, while continuously updating an accumulator.
    %
:- pred map_foldl(pred(K, V, W, A, A), any_map(K, V), any_map(K, W), A, A).
:- mode map_foldl(pred(in, ia, oa, di, uo) is det, ia, oa,
    di, uo) is det.
:- mode map_foldl(pred(in, ia, oa, in, out) is det, ia, oa,
    in, out) is det.
:- mode map_foldl(pred(in, ia, oa, in, out) is semidet, ia, oa,
    in, out) is semidet.
:- mode map_foldl(pred(in, ia, oa, ia, oa) is det, ia, oa,
    ia, oa) is det.
:- mode map_foldl(pred(in, ia, oa, ia, oa) is semidet, ia, oa,
    ia, oa) is semidet.

    % As map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(K, V, W, A, A, B, B), any_map(K, V), any_map(K, W),
    A, A, B, B).
:- mode map_foldl2(pred(in, ia, oa, in, out, di, uo) is det,
    ia, oa, in, out, di, uo) is det.
:- mode map_foldl2(pred(in, ia, oa, in, out, in, out) is det,
    ia, oa, in, out, in, out) is det.
:- mode map_foldl2(pred(in, ia, oa, in, out, in, out) is semidet,
    ia, oa, in, out, in, out) is semidet.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, in, out) is det,
    ia, oa, ia, oa, in, out) is det.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, in, out) is semidet,
    ia, oa, ia, oa, in, out) is semidet.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, ia, oa) is det,
    ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, ia, oa) is semidet,
    ia, oa, ia, oa, ia, oa) is semidet.

    % Given two maps M1 and M2, create a third map M3 that has only the
    % keys that occur in both M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- func intersect(func(V, V) = V, any_map(K, V), any_map(K, V))
    = any_map(K, V).
:- mode intersect(func(ia, ia) = oa is det, ia, ia) = oa is det.
:- pred intersect(pred(V, V, V), any_map(K, V), any_map(K, V), any_map(K, V)).
:- mode intersect(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode intersect(pred(ia, ia, oa) is det, ia, ia, oa) is det.

    % Calls intersect. Aborts if intersect fails.
    %
:- func det_intersect(func(V, V) = V, any_map(K, V), any_map(K, V))
    = any_map(K, V).
:- mode det_intersect(func(ia, ia) = oa is semidet, ia, ia) = oa is det.
:- pred det_intersect(pred(V, V, V), any_map(K, V), any_map(K, V),
    any_map(K, V)).
:- mode det_intersect(pred(ia, ia, oa) is semidet, ia, ia, oa) is det.

    % Given two maps M1 and M2, create a third map M3 that all the keys
    % that occur in either M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- func union(func(V, V) = V, any_map(K, V), any_map(K, V)) = any_map(K, V).
:- mode union(func(ia, ia) = oa is det, ia, ia) = oa is det.
:- pred union(pred(V, V, V), any_map(K, V), any_map(K, V), any_map(K, V)).
:- mode union(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode union(pred(ia, ia, oa) is det, ia, ia, oa) is det.

    % Calls union. Aborts if union fails.
    %
:- func det_union(func(V, V) = V, any_map(K, V), any_map(K, V))
    = any_map(K, V).
:- mode det_union(func(ia, ia) = oa is semidet, ia, ia) = oa is det.
:- pred det_union(pred(V, V, V), any_map(K, V), any_map(K, V),
    any_map(K, V)).
:- mode det_union(pred(ia, ia, oa) is semidet, ia, ia, oa) is det.

    % Field selection for maps.

    % Map ^ elem(Key) = search(Map, Key).
    %
:- func elem(K::in, any_map(K, V)::ia) = (V::oa) is semidet.

    % Map ^ det_elem(Key) = lookup(Map, Key).
    %
:- func det_elem(K::in, any_map(K, V)::ia) = (V::oa) is det.

    % Field update for maps.

    % (Map ^ elem(Key) := Value) = set(Map, Key, Value).
    %
:- func 'elem :='(K::in, any_map(K, V)::ia, V::ia) = (any_map(K, V)::oa)
    is det.

    % (Map ^ det_elem(Key) := Value) = det_update(Map, Key, Value).
    %
:- func 'det_elem :='(K::in, any_map(K, V)::ia, V::ia) = (any_map(K, V)::oa)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module any_tree234.
:- import_module any_util.
:- import_module pair.
:- import_module require.
:- import_module string.

:- type any_map(K, V) == any_tree234(K, V).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init(M) :-
    any_tree234.init(M).

is_empty(M) :-
    any_tree234.is_empty(M).

contains(Map, K) :-
    any_map.search(Map, K, _).

member(Map, K, V) :-
    any_tree234.member(Map, K, V).

search(Map, K, V) :-
    any_tree234.search(Map, K, V).

lookup(Map, K, V) :-
    promise_pure (
        ( if any_tree234.search(Map, K, V1) then
            V = V1
        else
            report_lookup_error("any_map.lookup: key not found", K, V)
        )
    ).

lower_bound_search(Map, SearchK, K, V) :-
    any_tree234.lower_bound_search(Map, SearchK, K, V).

lower_bound_lookup(Map, SearchK, K, V) :-
    promise_pure (
        ( if any_tree234.lower_bound_search(Map, SearchK, K1, V1) then
            K = K1,
            V = V1
        else
            report_lookup_error("any_map.lower_bound_lookup: key not found",
                SearchK, V)
        )
    ).

upper_bound_search(Map, SearchK, K, V) :-
    any_tree234.upper_bound_search(Map, SearchK, K, V).

upper_bound_lookup(Map, SearchK, K, V) :-
    promise_pure (
        ( if any_tree234.upper_bound_search(Map, SearchK, K1, V1) then
            K = K1,
            V = V1
        else
            report_lookup_error("any_map.upper_bound_lookup: key not found",
                SearchK, V)
        )
    ).

max_key(M) = any_tree234.max_key(M).

min_key(M) = any_tree234.min_key(M).

insert(K, V, !Map) :-
    any_tree234.insert(K, V, !Map).

det_insert(K, V, !Map) :-
    promise_pure (
        ( if any_tree234.insert(K, V, !Map) then
            true
        else
            report_lookup_error("any_map.det_insert: key already present",
                K, V)
        )
    ).

det_insert_from_corresponding_lists(Ks, Vs, !Map) :-
    promise_pure (
        ( if
            Ks = [Key | Keys],
            Vs = [Value | Values]
        then
            any_map.det_insert(Key, Value, !Map),
            any_map.det_insert_from_corresponding_lists(Keys, Values, !Map)
        else if
            Ks = [],
            Vs = []
        then
            true
        else
            error("any_map.det_insert_from_corresponding_lists - " ++
                "lists do not correspond")
        )
    ).

det_insert_from_any_assoc_list([], !Map).
det_insert_from_any_assoc_list([K - V | KVs], !Map) :-
    unsafe_cast_to_ground(K),
    any_map.det_insert(K, V, !Map),
    any_map.det_insert_from_any_assoc_list(KVs, !Map).

set_from_corresponding_lists(Ks, Vs, !Map) :-
    promise_pure (
        ( if
            Ks = [Key | Keys],
            Vs = [Value | Values]
        then
            any_map.set(Key, Value, !Map),
            any_map.set_from_corresponding_lists(Keys, Values, !Map)
        else if
            Ks = [],
            Vs = []
        then
            true
        else
            error("any_map.set_from_corresponding_lists - " ++
                "lists do not correspond")
        )
    ).

set_from_any_assoc_list([], !Map).
set_from_any_assoc_list([K - V | KVs], !Map) :-
    unsafe_cast_to_ground(K),
    any_map.set(K, V, !Map),
    any_map.set_from_any_assoc_list(KVs, !Map).

update(K, V, !Map) :-
    any_tree234.update(K, V, !Map).

det_update(K, V, !Map) :-
    promise_pure (
        ( if any_tree234.update(K, V, !Map) then
            true
        else
            report_lookup_error("any_map.det_update: key not found", K, V)
        )
    ).

transform_value(P, K, !Map) :-
    any_tree234.transform_value(P, K, !Map).

det_transform_value(P, K, !Map) :-
    promise_pure (
        ( if any_map.transform_value(P, K, !.Map, NewMap) then
            !:Map = NewMap
        else
            report_lookup_error("any_map.det_transform_value: key not found",
                K)
        )
    ).

det_transform_value(F, K, Map0) = Map :-
    any_map.det_transform_value(pred(V0::ia, V::oa) is det :- V = F(V0), K,
        Map0, Map).

set(K, V, !Map) :-
    any_tree234.set(K, V, !Map).

keys(Map, KeyList) :-
    any_tree234.keys(Map, KeyList).

sorted_keys(Map, KeyList) :-
    % Guaranteed to yield sorted lists.
    any_tree234.keys(Map, KeyList).

values(Map, KeyList) :-
    any_tree234.values(Map, KeyList).

to_any_assoc_list(M, L) :-
    any_tree234.any_tree234_to_any_assoc_list(M, L).

to_sorted_any_assoc_list(M, L) :-
    % Guaranteed to yield sorted lists.
    any_tree234.any_tree234_to_any_assoc_list(M, L).

from_any_assoc_list(L, M) :-
    any_tree234.any_assoc_list_to_any_tree234(L, M).

from_sorted_any_assoc_list(L, M) :-
    any_tree234.any_assoc_list_to_any_tree234(L, M).

delete(Key, !Map) :-
    any_tree234.delete(Key, !Map).

delete_list([], !Map).
delete_list([Key | Keys], !Map) :-
    any_map.delete(Key, !Map),
    any_map.delete_list(Keys, !Map).

remove(Key, Value, !Map) :-
    any_tree234.remove(Key, Value, !Map).

det_remove(Key, Value, !Map) :-
    promise_pure (
        ( if any_tree234.remove(Key, Value1, !Map) then
            Value = Value1
        else
            report_lookup_error("any_map.det_remove: key not found",
                Key, Value)
        )
    ).

count(Map, Count) :-
    any_tree234.count(Map, Count).

%---------------------------------------------------------------------------%

inverse_search(Map, V, K) :-
    any_map.member(Map, K, V).

%---------------------------------------------------------------------------%

from_corresponding_lists(Keys, Values, Map) :-
    any_assoc_list.from_corresponding_lists(Keys, Values, AssocList),
    any_tree234.any_assoc_list_to_any_tree234(AssocList, Map).

%---------------------------------------------------------------------------%

merge(M0, M1, M) :-
    any_map.overlay(M0, M1, M).

%---------------------------------------------------------------------------%

optimize(Map, Map).

%---------------------------------------------------------------------------%

overlay(Map0, Map1, Map) :-
    any_map.to_any_assoc_list(Map1, AssocList),
    any_map.overlay_loop(AssocList, Map0, Map).

:- pred overlay_loop(any_assoc_list(K, V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

overlay_loop([], !Map).
overlay_loop([K - V | AssocList], !Map) :-
    unsafe_cast_to_ground(K),
    any_map.set(K, V, !Map),
    any_map.overlay_loop(AssocList, !Map).

overlay_large_map(Map0, Map1, Map) :-
    any_map.to_any_assoc_list(Map0, AssocList),
    any_map.overlay_large_map_loop(AssocList, Map1, Map).

:- pred any_map.overlay_large_map_loop(any_assoc_list(K, V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

overlay_large_map_loop([], !Map).
overlay_large_map_loop([K - V | AssocList], !Map) :-
    unsafe_cast_to_ground(K),
    promise_pure (
        ( if any_map.insert(K, V, !Map) then
            true
        else
            true
        )
    ),
    any_map.overlay_large_map_loop(AssocList, !Map).

%---------------------------------------------------------------------------%

select(Original, KeySet, NewMap) :-
    set.to_sorted_list(KeySet, KeyList),
    any_map.init(NewMap0),
    select_loop(KeyList, Original, NewMap0, NewMap).

:- pred select_loop(list(K)::in, any_map(K, V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

select_loop([], _Original, !New).
select_loop([K | Ks], Original, !New) :-
    promise_pure (
        ( if any_map.search(Original, K, V) then
            any_map.set(K, V, !New)
        else
            true
        )
    ),
    any_map.select_loop(Ks, Original, !New).

%---------------------------------------------------------------------------%

apply_to_list([], _, []).
apply_to_list([K | Ks], Map, [V | Vs]) :-
    any_map.lookup(Map, K, V),
    any_map.apply_to_list(Ks, Map, Vs).

%---------------------------------------------------------------------------%

remove_smallest(K, V, Map0, Map) :-
    any_tree234.remove_smallest(K, V, Map0, Map).

%---------------------------------------------------------------------------%

foldl(Pred, Map, !A) :-
    any_tree234.foldl(Pred, Map, !A).

foldl2(Pred, Map, !A, !B) :-
    any_tree234.foldl2(Pred, Map, !A, !B).

foldl3(Pred, Map, !A, !B, !C) :-
    any_tree234.foldl3(Pred, Map, !A, !B, !C).

%---------------------------------------------------------------------------%

map_values(Pred, Map0, Map) :-
    any_tree234.map_values(Pred, Map0, Map).

map_foldl(Pred, !Map, !Acc) :-
    any_tree234.map_foldl(Pred, !Map, !Acc).

map_foldl2(Pred, !Map, !Acc1, !Acc2) :-
    any_tree234.map_foldl2(Pred, !Map, !Acc1, !Acc2).

%---------------------------------------------------------------------------%

intersect(CommonPred, Map1, Map2, Common) :-
    any_map.to_sorted_any_assoc_list(Map1, AssocList1),
    any_map.to_sorted_any_assoc_list(Map2, AssocList2),
    any_map.init(Common0),
    any_map.intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred any_map.intersect_2(any_assoc_list(K, V), any_assoc_list(K, V),
    pred(V, V, V), any_map(K, V), any_map(K, V)).
:- mode any_map.intersect_2(ia, ia, pred(ia, ia, oa) is semidet, ia, oa)
    is semidet.
:- mode any_map.intersect_2(ia, ia, pred(ia, ia, oa) is det, ia, oa)
    is det.

intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
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
        unsafe_cast_to_ground(Key1),
        unsafe_cast_to_ground(Key2),
        compare(R, Key1, Key2),
        (
            R = (=),
            call(CommonPred, Value1, Value2, Value),
            any_map.det_insert(Key1, Value, Common0, Common1),
            any_map.intersect_2(AssocTail1, AssocTail2, CommonPred,
                Common1, Common)
        ;
            R = (<),
            any_map.intersect_2(AssocTail1, AssocList2, CommonPred,
                Common0, Common)
        ;
            R = (>),
            any_map.intersect_2(AssocList1, AssocTail2, CommonPred,
                Common0, Common)
        )
    ).

det_intersect(CommonPred, Map1, Map2, Common) :-
    promise_pure (
        ( if any_map.intersect(CommonPred, Map1, Map2, CommonPrime) then
            Common = CommonPrime
        else
            error("any_map.det_intersect: any_map.intersect failed")
        )
    ).

%---------------------------------------------------------------------------%

union(CommonPred, Map1, Map2, Common) :-
    any_map.to_sorted_any_assoc_list(Map1, AssocList1),
    any_map.to_sorted_any_assoc_list(Map2, AssocList2),
    any_map.init(Common0),
    any_map.union_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred any_map.union_2(any_assoc_list(K, V), any_assoc_list(K, V),
    pred(V, V, V), any_map(K, V), any_map(K, V)).
:- mode any_map.union_2(ia, ia, pred(ia, ia, oa) is semidet, ia, oa)
    is semidet.
:- mode any_map.union_2(ia, ia, pred(ia, ia, oa) is det, ia, oa)
    is det.

union_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
    (
        AssocList1 = [],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [_ | _],
        AssocList2 = [],
        any_map.det_insert_from_any_assoc_list(AssocList1, Common0, Common)
    ;
        AssocList1 = [],
        AssocList2 = [_ | _],
        any_map.det_insert_from_any_assoc_list(AssocList2, Common0, Common)
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        unsafe_cast_to_ground(Key1),
        unsafe_cast_to_ground(Key2),
        compare(R, Key1, Key2),
        (
            R = (=),
            call(CommonPred, Value1, Value2, Value),
            any_map.det_insert(Key1, Value, Common0, Common1),
            any_map.union_2(AssocTail1, AssocTail2, CommonPred,
                Common1, Common)
        ;
            R = (<),
            any_map.det_insert(Key1, Value1, Common0, Common1),
            any_map.union_2(AssocTail1, AssocList2, CommonPred,
                Common1, Common)
        ;
            R = (>),
            any_map.det_insert(Key2, Value2, Common0, Common1),
            any_map.union_2(AssocList1, AssocTail2, CommonPred,
                Common1, Common)
        )
    ).

det_union(CommonPred, Map1, Map2, Union) :-
    promise_pure (
        ( if any_map.union(CommonPred, Map1, Map2, UnionPrime) then
            Union = UnionPrime
        else
            error("any_map.det_union: any_map.union failed")
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

init = M :-
    any_map.init(M).

search(M, K) = V :-
    any_map.search(M, K, V).

lookup(M, K) = V :-
    any_map.lookup(M, K, V).

insert(M0, K, V) = M :-
    any_map.insert(K, V, M0, M).

det_insert(M0, K, V) = M :-
    any_map.det_insert(K, V, M0, M).

det_insert_from_corresponding_lists(M0, Ks, Vs) = M :-
    any_map.det_insert_from_corresponding_lists(Ks, Vs, M0, M).

det_insert_from_any_assoc_list(M0, AL) = M :-
    any_map.det_insert_from_any_assoc_list(AL, M0, M).

set_from_corresponding_lists(M0, Ks, Vs) = M :-
    any_map.set_from_corresponding_lists(Ks, Vs, M0, M).

set_from_any_assoc_list(M0, AL) = M :-
    any_map.set_from_any_assoc_list(AL, M0, M).

update(M0, K, V) = M :-
    any_map.update(K, V, M0, M).

det_update(M0, K, V) = M :-
    any_map.det_update(K, V, M0, M).

set(M0, K, V) = M :-
    any_map.set(K, V, M0, M).

keys(M) = Ks :-
    any_map.keys(M, Ks).

sorted_keys(M) = Ks :-
    any_map.sorted_keys(M, Ks).

values(M) = Vs :-
    any_map.values(M, Vs).

to_any_assoc_list(M) = AL :-
    any_map.to_any_assoc_list(M, AL).

to_sorted_any_assoc_list(M) = AL :-
    any_map.to_sorted_any_assoc_list(M, AL).

from_any_assoc_list(AL) = M :-
    any_map.from_any_assoc_list(AL, M).

from_sorted_any_assoc_list(AL) = M :-
    any_map.from_sorted_any_assoc_list(AL, M).

delete(M0, K) = M :-
    any_map.delete(K, M0, M).

delete_list(M0, Ks) = M :-
    any_map.delete_list(Ks, M0, M).

count(M) = N :-
    any_map.count(M, N).

from_corresponding_lists(Ks, Vs) = M :-
    any_map.from_corresponding_lists(Ks, Vs, M).

merge(M1, M2) = M3 :-
    any_map.merge(M1, M2, M3).

overlay(M1, M2) = M3 :-
    any_map.overlay(M1, M2, M3).

overlay_large_map(M1, M2) = M3 :-
    any_map.overlay_large_map(M1, M2, M3).

select(M0, S) = M :-
    any_map.select(M0, S, M).

apply_to_list(Ks, M) = Vs :-
    any_map.apply_to_list(Ks, M, Vs).

optimize(M0) = M :-
    any_map.optimize(M0, M).

:- pragma promise_equivalent_clauses(any_map.foldl/3).

foldl(F::in(func(in, ia, in) = out is det), M::ia, A::in) = (B::out) :-
    P = ( pred(W::in, X::ia, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    any_map.foldl(P, M, A, B).
foldl(F::in(func(in, ia, ia) = oa is det), M::ia, A::ia) = (B::oa) :-
    P = ( pred(W::in, X::ia, Y::ia, Z::oa) is det :- Z = F(W, X, Y) ),
    any_map.foldl(P, M, A, B).

map_values(F, M1) = M2 :-
    P = ( pred(X::in, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_map.map_values(P, M1, M2).

intersect(F, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_map.intersect(P, M1, M2, M3).

det_intersect(PF, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is semidet :- Z = PF(X, Y) ),
    any_map.det_intersect(P, M1, M2, M3).

union(F, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_map.union(P, M1, M2, M3).

det_union(F, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is semidet :- Z = F(X, Y) ),
    any_map.det_union(P, M1, M2, M3).

elem(Key, Map) = any_map.search(Map, Key).

det_elem(Key, Map) = any_map.lookup(Map, Key).

'elem :='(Key, Map, Value) = any_map.set(Map, Key, Value).

'det_elem :='(Key, Map, Value) = any_map.det_update(Map, Key, Value).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
