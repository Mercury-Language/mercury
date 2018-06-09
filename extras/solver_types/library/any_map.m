% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% any_map.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Sep  5 18:23:35 EST 2005
%
% A copy of map.m adapted for maps from ground keys to values with inst any.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module any_map.
:- interface.

:- import_module any_assoc_list.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type any_map(K, V).

%-----------------------------------------------------------------------------%

    % Initialize an empty map.
    %
:- pred any_map__init(any_map(K, V)::oa) is det.
:- func any_map__init = (any_map(K, V)::oa) is det.

    % Check whether a map is empty.
    %
:- pred any_map__is_empty(any_map(K, V)::ia) is semidet.

    % Check whether map contains key
    %
:- pred any_map__contains(any_map(K, V)::ia, K::in) is semidet.

:- pred any_map__member(any_map(K, V)::ia, K::out, V::oa) is nondet.

    % Search map for key.
    %
:- pred any_map__search(any_map(K, V)::ia, K::in, V::oa) is semidet.
:- func any_map__search(any_map(K, V)::ia, K::in) = (V::oa) is semidet.

    % Search map for key, but abort if search fails.
    %
:- pred any_map__lookup(any_map(K, V)::ia, K::in, V::oa) is det.
:- func any_map__lookup(any_map(K, V)::ia, K::in) = (V::oa) is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred any_map__lower_bound_search(any_map(K, V)::ia, K::in, K::out, V::oa)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Aborts if there is no key with the given or lower value.
    %
:- pred any_map__lower_bound_lookup(any_map(K, V)::ia, K::in, K::out, V::oa)
        is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred any_map__upper_bound_search(any_map(K, V)::ia, K::in, K::out, V::oa)
        is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Aborts if there is no key with the given or higher value.
    %
:- pred any_map__upper_bound_lookup(any_map(K, V)::ia, K::in, K::out, V::oa)
        is det.

    % Return the largest key in the map, if there is one.
    %
:- func any_map__max_key(any_map(K,V)::ia) = (K::out) is semidet.

    % Return the smallest key in the map, if there is one.
    %
:- func any_map__min_key(any_map(K,V)::ia) = (K::out) is semidet.

    % Search map for data.
    %
:- pred any_map__inverse_search(any_map(K, V)::ia, V::ia, K::out) is nondet.

    % Insert a new key and corresponding value into a map.
    % Fail if the key already exists.
    %
:- pred any_map__insert(any_map(K, V)::ia, K::in, V::ia, any_map(K, V)::oa)
        is semidet.
:- func any_map__insert(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
        is semidet.

    % Insert a new key and corresponding value into a map.
    % Abort if the key already exists.
    %
:- pred any_map__det_insert(any_map(K, V)::ia, K::in, V::ia, any_map(K, V)::oa)
        is det.
:- func any_map__det_insert(any_map(K, V)::ia, K::in, V::ia)
        = (any_map(K, V)::oa) is det.

    % Apply any_map__det_insert to key - value pairs from corresponding lists.
    % NOTE: this is not defined if the key values are not (semantically)
    % ground.
    %
:- pred any_map__det_insert_from_corresponding_lists(any_map(K, V)::ia,
        list(K)::in, list(V)::ia, any_map(K, V)::oa) is det.
:- func any_map__det_insert_from_corresponding_lists(any_map(K, V)::ia,
        list(K)::in, list(V)::ia) = (any_map(K, V)::oa) is det.

    % Apply any_map__det_insert to key - value pairs from the any_assoc_lists.
    % NOTE: this is not defined if the key values are not (semantically)
    % ground.
    %
:- pred any_map__det_insert_from_any_assoc_list(any_map(K, V)::ia,
    any_assoc_list(K, V)::ia, any_map(K, V)::oa) is det.
:- func any_map__det_insert_from_any_assoc_list(any_map(K, V)::ia,
        any_assoc_list(K, V)::ia) = (any_map(K, V)::oa) is det.

    % Apply any_map__set to key - value pairs from corresponding lists.
    %
:- pred any_map__set_from_corresponding_lists(any_map(K, V)::ia, list(K)::in,
    list(V)::ia, any_map(K, V)::oa) is det.
:- func any_map__set_from_corresponding_lists(any_map(K, V)::ia, list(K)::in,
    list(V)::ia) = (any_map(K, V)::oa) is det.

:- pred any_map__set_from_any_assoc_list(any_map(K, V)::ia,
        any_assoc_list(K, V)::ia, any_map(K, V)::oa) is det.
:- func any_map__set_from_any_assoc_list(any_map(K, V)::ia,
        any_assoc_list(K, V)::ia) = (any_map(K, V)::oa) is det.

    % Update the value corresponding to a given key
    % Fail if the key doesn't already exist.
    %
:- pred any_map__update(any_map(K, V)::ia, K::in, V::ia, any_map(K, V)::oa)
        is semidet.
:- func any_map__update(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
        is semidet.

    % Update the value corresponding to a given key
    % Abort if the key doesn't already exist.
    %
:- pred any_map__det_update(any_map(K, V)::ia, K::in, V::ia, any_map(K, V)::oa)
        is det.
:- func any_map__det_update(any_map(K, V)::ia, K::in, V::ia)
        = (any_map(K, V)::oa) is det.

    % Update the value at the given key by applying the supplied
    % transformation to it.  Fails if the key is not found.  This is faster
    % than first searching for the value and then updating it.
    %
:- pred any_map__transform_value(pred(V, V)::in(pred(ia, oa) is det), K::in,
    any_map(K, V)::ia, any_map(K, V)::oa) is semidet.

    % Same as transform_value/4, but aborts instead of failing if the
    % key is not found.
    %
:- pred any_map__det_transform_value(pred(V, V)::in(pred(ia, oa) is det),
        K::in, any_map(K, V)::ia, any_map(K, V)::oa) is det.
:- func any_map__det_transform_value((func(V) = V)::in(func(ia) = oa is det),
        K::in, any_map(K, V)::ia) = (any_map(K, V)::oa) is det.

    % Update value if the key is already present, otherwise
    % insert new key and value.
    %
:- func any_map__set(any_map(K, V)::ia, K::in, V::ia) = (any_map(K, V)::oa)
        is det.
:- pred any_map__set(any_map(K, V)::ia, K::in, V::ia, any_map(K, V)::oa)
        is det.

    % Given a map, return a list of all the keys in the map.
    %
:- func any_map__keys(any_map(K, V)::ia) = (list(K)::out) is det.
:- pred any_map__keys(any_map(K, V)::ia, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % in sorted order.
    %
:- func any_map__sorted_keys(any_map(K, V)::ia) = (list(K)::out) is det.
:- pred any_map__sorted_keys(any_map(K, V)::ia, list(K)::out) is det.

    % Given a map, return a list of all the data values in the map.
    %
:- func any_map__values(any_map(K, V)::ia) = (list(V)::oa) is det.
:- pred any_map__values(any_map(K, V)::ia, list(V)::oa) is det.

    % Convert a map to an association list.
    %
:- func any_map__to_any_assoc_list(any_map(K, V)::ia) =
        (any_assoc_list(K, V)::oa) is det.
:- pred any_map__to_any_assoc_list(any_map(K, V)::ia,
        any_assoc_list(K, V)::oa) is det.

    % Convert a map to an association list which is sorted on the keys.
    %
:- func any_map__to_sorted_any_assoc_list(any_map(K, V)::ia)
        = (any_assoc_list(K, V)::oa) is det.
:- pred any_map__to_sorted_any_assoc_list(any_map(K, V)::ia,
        any_assoc_list(K, V)::oa) is det.

    % Convert an association list to a map.
    %
:- func any_map__from_any_assoc_list(any_assoc_list(K, V)::ia)
        = (any_map(K, V)::oa) is det.
:- pred any_map__from_any_assoc_list(any_assoc_list(K, V)::ia,
        any_map(K, V)::oa) is det.

    % Convert a sorted association list to a map.
    %
:- func any_map__from_sorted_any_assoc_list(any_assoc_list(K, V)::ia)
        = (any_map(K, V)::oa) is det.
:- pred any_map__from_sorted_any_assoc_list(any_assoc_list(K, V)::ia,
        any_map(K, V)::oa) is det.

    % Delete a key-value pair from a map.
    % If the key is not present, leave the map unchanged.
    %
:- func any_map__delete(any_map(K, V)::ia, K::in) = (any_map(K, V)::oa) is det.
:- pred any_map__delete(any_map(K, V)::ia, K::in, any_map(K, V)::oa) is det.

    % Apply any_map__delete/3 to a list of keys.
    %
:- func any_map__delete_list(any_map(K, V)::ia, list(K)::in)
        = (any_map(K, V)::oa) is det.
:- pred any_map__delete_list(any_map(K, V)::ia, list(K)::in,
        any_map(K, V)::oa) is det.

    % Delete a key-value pair from a map and return the value.
    % Fail if the key is not present.
    %
:- pred any_map__remove(any_map(K, V)::ia, K::in, V::oa, any_map(K, V)::oa)
        is semidet.

    % Delete a key-value pair from a map and return the value.
    % Abort if the key is not present.
    %
:- pred any_map__det_remove(any_map(K, V)::ia, K::in, V::oa, any_map(K, V)::oa)
        is det.

    % Count the number of elements in the map.
    %
:- func any_map__count(any_map(K, V)::ia) = (int::out) is det.
:- pred any_map__count(any_map(K, V)::ia, int::out) is det.

    % Convert a pair of lists (which must be of the same length)
    % to a map.
    %
:- func any_map__from_corresponding_lists(list(K)::in, list(V)::ia)
        = (any_map(K, V)::oa) is det.
:- pred any_map__from_corresponding_lists(list(K)::in, list(V)::ia,
        any_map(K, V)::oa) is det.

    % For any_map__merge(MapA, MapB, Map), MapA and MapB must
    % not both contain the same key.
    %
:- func any_map__merge(any_map(K, V)::ia, any_map(K, V)::ia)
        = (any_map(K, V)::oa) is det.
:- pred any_map__merge(any_map(K, V)::ia, any_map(K, V)::ia,
        any_map(K, V)::oa) is det.

    % For any_map__overlay(MapA, MapB, Map), if MapA and MapB both
    % contain the same key, then Map will map that key to
    % the value from MapB.  In otherwords, MapB takes precedence
    % over MapA.
    %
:- func any_map__overlay(any_map(K, V)::ia, any_map(K, V)::ia)
        = (any_map(K, V)::oa) is det.
:- pred any_map__overlay(any_map(K, V)::ia, any_map(K, V)::ia,
        any_map(K, V)::oa) is det.

    % any_map__overlay_large_map(MapA, MapB, Map) performs the same task as
    % any_map__overlay(MapA, MapB, Map). However, while any_map__overlay takes
    % time proportional to the size of MapB, any_map__overlay_large_map takes
    % time proportional to the size of MapA. In other words, it preferable when
    % MapB is a large map.
    %
:- func any_map__overlay_large_map(any_map(K, V)::ia, any_map(K, V)::ia)
        = (any_map(K, V)::oa) is det.
:- pred any_map__overlay_large_map(any_map(K, V)::ia, any_map(K, V)::ia,
        any_map(K, V)::oa) is det.

    % any_map__select takes a map and a set of keys and returns
    % a map containing the keys in the set and their corresponding
    % values.
    %
:- func any_map__select(any_map(K, V)::ia, set(K)::in) = (any_map(K, V)::oa)
        is det.
:- pred any_map__select(any_map(K, V)::ia, set(K)::in, any_map(K, V)::oa)
        is det.

    % Given a list of keys, produce a list of their corresponding
    % values in a specified map.
    %
:- func any_map__apply_to_list(list(K)::in, any_map(K, V)::ia)
        = (list(V)::oa) is det.
:- pred any_map__apply_to_list(list(K)::in, any_map(K, V)::ia,
        list(V)::oa) is det.

    % Declaratively, a NOP.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the map in the expectation
    % of a number of lookups but few or no modifications.
    %
:- func any_map__optimize(any_map(K, V)::ia) = (any_map(K, V)::oa) is det.
:- pred any_map__optimize(any_map(K, V)::ia, any_map(K, V)::oa) is det.

    % Remove the smallest item from the map, fail if
    % the map is empty.
    %
:- pred any_map__remove_smallest(any_map(K, V)::ia, K::out, V::oa,
        any_map(K, V)::oa) is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate for each key-value pair.
    %
:- func any_map__foldl(func(K, V, T) = T, any_map(K, V), T) = T.
:- mode any_map__foldl(func(in, ia, in) = out is det, ia, in) = out is det.
:- mode any_map__foldl(func(in, ia, ia) = oa is det, ia, ia) = oa is det.

:- pred any_map__foldl(pred(K, V, T, T), any_map(K, V), T, T).
:- mode any_map__foldl(pred(in, ia, di, uo) is det, ia, di, uo)
        is det.
:- mode any_map__foldl(pred(in, ia, in, out) is det, ia, in, out)
        is det.
:- mode any_map__foldl(pred(in, ia, in, out) is semidet, ia, in, out)
        is semidet.
:- mode any_map__foldl(pred(in, ia, ia, oa) is det, ia, ia, oa)
        is det.
:- mode any_map__foldl(pred(in, ia, ia, oa) is semidet, ia, ia, oa)
        is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate with two accumulators for
    % each key-value pair.
    % (Although no more expressive than any_map__foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred any_map__foldl2(pred(K, V, T, T, U, U), any_map(K, V), T, T, U, U).
:- mode any_map__foldl2(pred(in, ia, di, uo, di, uo) is det,
        ia, di, uo, di, uo) is det.
:- mode any_map__foldl2(pred(in, ia, in, out, di, uo) is det,
        ia, in, out, di, uo) is det.
:- mode any_map__foldl2(pred(in, ia, ia, oa, di, uo) is det,
        ia, ia, oa, di, uo) is det.
:- mode any_map__foldl2(pred(in, ia, in, out, in, out) is det,
        ia, in, out, in, out) is det.
:- mode any_map__foldl2(pred(in, ia, in, out, in, out) is semidet,
        ia, in, out, in, out) is semidet.
:- mode any_map__foldl2(pred(in, ia, ia, oa, in, out) is det,
        ia, ia, oa, in, out) is det.
:- mode any_map__foldl2(pred(in, ia, ia, oa, in, out) is semidet,
        ia, ia, oa, in, out) is semidet.
:- mode any_map__foldl2(pred(in, ia, ia, oa, ia, oa) is det,
        ia, ia, oa, ia, oa) is det.
:- mode any_map__foldl2(pred(in, ia, ia, oa, ia, oa) is semidet,
        ia, ia, oa, ia, oa) is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate with three accumulators for
    % each key-value pair.
    % (Although no more expressive than any_map__foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred any_map__foldl3(pred(K, V, T, T, U, U, W, W),
        any_map(K, V), T, T, U, U, W, W).
:- mode any_map__foldl3(pred(in, ia, di, uo, di, uo, di, uo) is det,
        ia, di, uo, di, uo, di, uo) is det.
:- mode any_map__foldl3(pred(in, ia, in, out, di, uo, di, uo) is det,
        ia, in, out, di, uo, di, uo) is det.
:- mode any_map__foldl3(pred(in, ia, in, out, in, out, di, uo) is det,
        ia, in, out, in, out, di, uo) is det.
:- mode any_map__foldl3(pred(in, ia, in, out, in, out, in, out) is det,
        ia, in, out, in, out, in, out) is det.
:- mode any_map__foldl3(pred(in, ia, in, out, in, out, in, out) is semidet,
        ia, in, out, in, out, in, out) is semidet.
:- mode any_map__foldl3(pred(in, ia, ia, oa, in, out, in, out) is det,
        ia, ia, oa, in, out, in, out) is det.
:- mode any_map__foldl3(pred(in, ia, ia, oa, in, out, in, out) is semidet,
        ia, ia, oa, in, out, in, out) is semidet.
:- mode any_map__foldl3(pred(in, ia, ia, oa, ia, oa, in, out) is det,
        ia, ia, oa, ia, oa, in, out) is det.
:- mode any_map__foldl3(pred(in, ia, ia, oa, ia, oa, in, out) is semidet,
        ia, ia, oa, ia, oa, in, out) is semidet.
:- mode any_map__foldl3(pred(in, ia, ia, oa, ia, oa, ia, oa) is det,
        ia, ia, oa, ia, oa, ia, oa) is det.
:- mode any_map__foldl3(pred(in, ia, ia, oa, ia, oa, ia, oa) is semidet,
        ia, ia, oa, ia, oa, ia, oa) is semidet.

    % Apply a transformation predicate to all the values
    % in a map.
    %
:- func any_map__map_values(func(K, V) = W, any_map(K, V)) = any_map(K, W).
:- mode any_map__map_values(func(in, ia) = oa is det, ia) = oa is det.

:- pred any_map__map_values(pred(K, V, W), any_map(K, V), any_map(K, W)).
:- mode any_map__map_values(pred(in, ia, oa) is det, ia, oa) is det.
:- mode any_map__map_values(pred(in, ia, oa) is semidet, ia, oa) is semidet.

    % Apply a transformation predicate to all the values
    % in a map, while continuously updating an accumulator.
    %
:- pred any_map__map_foldl(pred(K, V, W, A, A), any_map(K, V), any_map(K, W),
        A, A).
:- mode any_map__map_foldl(pred(in, ia, oa, di, uo) is det, ia, oa,
        di, uo) is det.
:- mode any_map__map_foldl(pred(in, ia, oa, in, out) is det, ia, oa,
        in, out) is det.
:- mode any_map__map_foldl(pred(in, ia, oa, in, out) is semidet, ia, oa,
        in, out) is semidet.
:- mode any_map__map_foldl(pred(in, ia, oa, ia, oa) is det, ia, oa,
        ia, oa) is det.
:- mode any_map__map_foldl(pred(in, ia, oa, ia, oa) is semidet, ia, oa,
        ia, oa) is semidet.

    % As any_map__map_foldl, but with two accumulators.
    %
:- pred any_map__map_foldl2(pred(K, V, W, A, A, B, B),
        any_map(K, V), any_map(K, W), A, A, B, B).
:- mode any_map__map_foldl2(pred(in, ia, oa, in, out, di, uo) is det,
        ia, oa, in, out, di, uo) is det.
:- mode any_map__map_foldl2(pred(in, ia, oa, in, out, in, out) is det,
        ia, oa, in, out, in, out) is det.
:- mode any_map__map_foldl2(pred(in, ia, oa, in, out, in, out) is semidet,
        ia, oa, in, out, in, out) is semidet.
:- mode any_map__map_foldl2(pred(in, ia, oa, ia, oa, in, out) is det,
        ia, oa, ia, oa, in, out) is det.
:- mode any_map__map_foldl2(pred(in, ia, oa, ia, oa, in, out) is semidet,
        ia, oa, ia, oa, in, out) is semidet.
:- mode any_map__map_foldl2(pred(in, ia, oa, ia, oa, ia, oa) is det,
        ia, oa, ia, oa, ia, oa) is det.
:- mode any_map__map_foldl2(pred(in, ia, oa, ia, oa, ia, oa) is semidet,
        ia, oa, ia, oa, ia, oa) is semidet.

    % Given two maps M1 and M2, create a third map M3 that has only the
    % keys that occur in both M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- pred any_map__intersect(pred(V, V, V), any_map(K, V), any_map(K, V),
        any_map(K, V)).
:- mode any_map__intersect(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode any_map__intersect(pred(ia, ia, oa) is det, ia, ia, oa) is det.

:- func any_map__intersect(func(V, V) = V, any_map(K, V), any_map(K, V))
        = any_map(K, V).
:- mode any_map__intersect(func(ia, ia) = oa is det, ia, ia) = oa is det.

    % Calls any_map__intersect. Aborts if any_map__intersect fails.
    %
:- pred any_map__det_intersect(pred(V, V, V), any_map(K, V), any_map(K, V),
        any_map(K, V)).
:- mode any_map__det_intersect(pred(ia, ia, oa) is semidet, ia, ia, oa) is det.

:- func any_map__det_intersect(func(V, V) = V, any_map(K, V), any_map(K, V))
        = any_map(K, V).
:- mode any_map__det_intersect(func(ia, ia) = oa is semidet, ia, ia) = oa
        is det.

    % Given two maps M1 and M2, create a third map M3 that all the keys
    % that occur in either M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- func any_map__union(func(V, V) = V, any_map(K, V), any_map(K, V))
        = any_map(K, V).
:- mode any_map__union(func(ia, ia) = oa is det, ia, ia) = oa is det.

:- pred any_map__union(pred(V, V, V), any_map(K, V), any_map(K, V),
        any_map(K, V)).
:- mode any_map__union(pred(ia, ia, oa) is semidet, ia, ia, oa) is semidet.
:- mode any_map__union(pred(ia, ia, oa) is det, ia, ia, oa) is det.

    % Calls any_map__union. Aborts if any_map__union fails.
    %
:- pred any_map__det_union(pred(V, V, V), any_map(K, V), any_map(K, V),
        any_map(K, V)).
:- mode any_map__det_union(pred(ia, ia, oa) is semidet, ia, ia, oa) is det.

:- func any_map__det_union(func(V, V) = V, any_map(K, V), any_map(K, V))
        = any_map(K, V).
:- mode any_map__det_union(func(ia, ia) = oa is semidet, ia, ia) = oa is det.

    % Field selection for maps.

    % Map ^ elem(Key) = any_map__search(Map, Key).
    %
:- func any_map__elem(K::in, any_map(K, V)::ia) = (V::oa) is semidet.

    % Map ^ det_elem(Key) = any_map__lookup(Map, Key).
    %
:- func any_map__det_elem(K::in, any_map(K, V)::ia) = (V::oa) is det.

    % Field update for maps.

    % (Map ^ elem(Key) := Value) = any_map__set(Map, Key, Value).
    %
:- func 'any_map__elem :='(K::in, any_map(K, V)::ia, V::ia)
        = (any_map(K, V)::oa) is det.

    % (Map ^ det_elem(Key) := Value) = any_map__det_update(Map, Key, Value).
    %
:- func 'any_map__det_elem :='(K::in, any_map(K, V)::ia, V::ia)
        = (any_map(K, V)::oa) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module any_tree234.
:- import_module any_util.
:- import_module pair.
:- import_module require.
:- import_module string.

:- type any_map(K, V)   ==  any_tree234(K, V).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

any_map__init(M) :-
    any_tree234__init(M).

any_map__is_empty(M) :-
    any_tree234__is_empty(M).

any_map__contains(Map, K) :-
    any_map__search(Map, K, _).

any_map__member(Map, K, V) :-
    any_tree234__member(Map, K, V).

any_map__search(Map, K, V) :-
    any_tree234__search(Map, K, V).

any_map__lookup(Map, K, V) :-
    promise_pure (
        any_tree234__search(Map, K, V1)
    ->
        V = V1
    ;
        report_lookup_error("any_map__lookup: key not found", K, V)
    ).

any_map__lower_bound_search(Map, SearchK, K, V) :-
    any_tree234__lower_bound_search(Map, SearchK, K, V).

any_map__lower_bound_lookup(Map, SearchK, K, V) :-
    promise_pure (
        any_tree234__lower_bound_search(Map, SearchK, K1, V1)
    ->
        K = K1,
        V = V1
    ;
        report_lookup_error("any_map__lower_bound_lookup: key not found",
            SearchK, V)
    ).

any_map__upper_bound_search(Map, SearchK, K, V) :-
    any_tree234__upper_bound_search(Map, SearchK, K, V).

any_map__upper_bound_lookup(Map, SearchK, K, V) :-
    promise_pure (
        any_tree234__upper_bound_search(Map, SearchK, K1, V1)
    ->
        K = K1,
        V = V1
    ;
        report_lookup_error("any_map__upper_bound_lookup: key not found",
            SearchK, V)
    ).

any_map__max_key(M) = any_tree234__max_key(M).

any_map__min_key(M) = any_tree234__min_key(M).

any_map__insert(Map0, K, V, Map) :-
    any_tree234__insert(Map0, K, V, Map).

any_map__det_insert(Map0, K, V, Map) :-
    promise_pure (
        any_tree234__insert(Map0, K, V, Map1)
    ->
        Map = Map1
    ;
        report_lookup_error("any_map__det_insert: key already present",
            K, V)
    ).

any_map__det_insert_from_corresponding_lists(Map0, Ks, Vs, Map) :-
    promise_pure
    (
        Ks = [Key | Keys],
        Vs = [Value | Values]
    ->
        any_map__det_insert(Map0, Key, Value, Map1),
        any_map__det_insert_from_corresponding_lists(Map1, Keys, Values,
            Map)
    ;
        Ks = [],
        Vs = []
    ->
        Map = Map0
    ;
        error("any_map__det_insert_from_corresponding_lists - " ++
            "lists do not correspond")
    ).

any_map__det_insert_from_any_assoc_list(Map, [], Map).
any_map__det_insert_from_any_assoc_list(Map0, [K - V | KVs], Map) :-
    unsafe_cast_to_ground(K),
    any_map__det_insert(Map0, K, V, Map1),
    any_map__det_insert_from_any_assoc_list(Map1, KVs, Map).

any_map__set_from_corresponding_lists(Map0, Ks, Vs, Map) :-
    promise_pure
    (
        Ks = [Key | Keys],
        Vs = [Value | Values]
    ->
        any_map__set(Map0, Key, Value, Map1),
        any_map__set_from_corresponding_lists(Map1, Keys, Values, Map)
    ;
        Ks = [],
        Vs = []
    ->
        Map = Map0
    ;
        error("any_map__set_from_corresponding_lists - " ++
            "lists do not correspond")
    ).

any_map__set_from_any_assoc_list(Map, [], Map).
any_map__set_from_any_assoc_list(Map0, [K - V | KVs], Map) :-
    unsafe_cast_to_ground(K),
    any_map__set(Map0, K, V, Map1),
    any_map__set_from_any_assoc_list(Map1, KVs, Map).

any_map__update(Map0, K, V, Map) :-
    any_tree234__update(Map0, K, V, Map).

any_map__det_update(Map0, K, V, Map) :-
    promise_pure (
        any_tree234__update(Map0, K, V, Map1)
    ->
        Map = Map1
    ;
        report_lookup_error("any_map__det_update: key not found", K, V)
    ).

any_map__transform_value(P, K, !Map) :-
    any_tree234__transform_value(P, K, !Map).

any_map__det_transform_value(P, K, !Map) :-
    promise_pure (
        any_map__transform_value(P, K, !.Map, NewMap)
    ->
        !:Map = NewMap
    ;
        report_lookup_error("any_map__det_transform_value: key not found",
            K)
    ).

any_map__det_transform_value(F, K, Map0) = Map :-
    any_map__det_transform_value(pred(V0::ia, V::oa) is det :- V = F(V0), K,
        Map0, Map).

any_map__set(Map0, K, V, Map) :-
    any_tree234__set(Map0, K, V, Map).

any_map__keys(Map, KeyList) :-
    any_tree234__keys(Map, KeyList).

any_map__sorted_keys(Map, KeyList) :-
    % Guaranteed to yield sorted lists.
    any_tree234__keys(Map, KeyList).

any_map__values(Map, KeyList) :-
    any_tree234__values(Map, KeyList).

any_map__to_any_assoc_list(M, L) :-
    any_tree234__any_tree234_to_any_assoc_list(M, L).

any_map__to_sorted_any_assoc_list(M, L) :-
    % Guaranteed to yield sorted lists.
    any_tree234__any_tree234_to_any_assoc_list(M, L).

any_map__from_any_assoc_list(L, M) :-
    any_tree234__any_assoc_list_to_any_tree234(L, M).

any_map__from_sorted_any_assoc_list(L, M) :-
    any_tree234__any_assoc_list_to_any_tree234(L, M).

any_map__delete(Map0, Key, Map) :-
    any_tree234__delete(Map0, Key, Map).

any_map__delete_list(Map, [], Map).
any_map__delete_list(Map0, [Key | Keys], Map) :-
    any_map__delete(Map0, Key, Map1),
    any_map__delete_list(Map1, Keys, Map).

any_map__remove(Map0, Key, Value, Map) :-
    any_tree234__remove(Map0, Key, Value, Map).

any_map__det_remove(Map0, Key, Value, Map) :-
    promise_pure (
        any_tree234__remove(Map0, Key, Value1, Map1)
    ->
        Value = Value1,
        Map = Map1
    ;
        report_lookup_error("any_map__det_remove: key not found",
            Key, Value)
    ).

any_map__count(Map, Count) :-
    any_tree234__count(Map, Count).

%-----------------------------------------------------------------------------%

any_map__inverse_search(Map, V, K) :-
    any_map__member(Map, K, V).

%-----------------------------------------------------------------------------%

any_map__from_corresponding_lists(Keys, Values, Map) :-
    any_assoc_list__from_corresponding_lists(Keys, Values, AssocList),
    any_tree234__any_assoc_list_to_any_tree234(AssocList, Map).

%-----------------------------------------------------------------------------%

any_map__merge(M0, M1, M) :-
    any_map__overlay(M0, M1, M).

%-----------------------------------------------------------------------------%

any_map__optimize(Map, Map).

%-----------------------------------------------------------------------------%

any_map__overlay(Map0, Map1, Map) :-
    any_map__to_any_assoc_list(Map1, AssocList),
    any_map__overlay_2(AssocList, Map0, Map).

:- pred any_map__overlay_2(any_assoc_list(K, V)::ia, any_map(K, V)::ia,
        any_map(K, V)::oa) is det.

any_map__overlay_2([], Map, Map).
any_map__overlay_2([K - V | AssocList], Map0, Map) :-
    unsafe_cast_to_ground(K),
    any_map__set(Map0, K, V, Map1),
    any_map__overlay_2(AssocList, Map1, Map).

any_map__overlay_large_map(Map0, Map1, Map) :-
    any_map__to_any_assoc_list(Map0, AssocList),
    any_map__overlay_large_map_2(AssocList, Map1, Map).

:- pred any_map__overlay_large_map_2(any_assoc_list(K, V)::ia,
    any_map(K, V)::ia, any_map(K, V)::oa) is det.

any_map__overlay_large_map_2([], Map, Map).
any_map__overlay_large_map_2([K - V | AssocList], Map0, Map) :-
    unsafe_cast_to_ground(K),
    promise_pure (
        any_map__insert(Map0, K, V, Map1)
    ->
        Map2 = Map1
    ;
        Map2 = Map0
    ),
    any_map__overlay_large_map_2(AssocList, Map2, Map).

%-----------------------------------------------------------------------------%

any_map__select(Original, KeySet, NewMap) :-
    set__to_sorted_list(KeySet, KeyList),
    any_map__init(NewMap0),
    any_map__select_2(KeyList, Original, NewMap0, NewMap).

:- pred any_map__select_2(list(K)::in, any_map(K, V)::ia, any_map(K, V)::ia,
    any_map(K, V)::oa) is det.

any_map__select_2([], _Original, New, New).
any_map__select_2([K|Ks], Original, New0, New) :-
    promise_pure (
        any_map__search(Original, K, V)
    ->
        any_map__set(New0, K, V, New1)
    ;
        New1 = New0
    ),
    any_map__select_2(Ks, Original, New1, New).

%-----------------------------------------------------------------------------%

any_map__apply_to_list([], _, []).
any_map__apply_to_list([K | Ks], Map, [V | Vs]) :-
    any_map__lookup(Map, K, V),
    any_map__apply_to_list(Ks, Map, Vs).

%-----------------------------------------------------------------------------%

any_map__remove_smallest(Map0, K, V, Map) :-
    any_tree234__remove_smallest(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

any_map__foldl(Pred, Map, !A) :-
    any_tree234__foldl(Pred, Map, !A).

any_map__foldl2(Pred, Map, !A, !B) :-
    any_tree234__foldl2(Pred, Map, !A, !B).

any_map__foldl3(Pred, Map, !A, !B, !C) :-
    any_tree234__foldl3(Pred, Map, !A, !B, !C).

%-----------------------------------------------------------------------------%

any_map__map_values(Pred, Map0, Map) :-
    any_tree234__map_values(Pred, Map0, Map).

any_map__map_foldl(Pred, !Map, !Acc) :-
    any_tree234__map_foldl(Pred, !Map, !Acc).

any_map__map_foldl2(Pred, !Map, !Acc1, !Acc2) :-
    any_tree234__map_foldl2(Pred, !Map, !Acc1, !Acc2).

%-----------------------------------------------------------------------------%

any_map__intersect(CommonPred, Map1, Map2, Common) :-
    any_map__to_sorted_any_assoc_list(Map1, AssocList1),
    any_map__to_sorted_any_assoc_list(Map2, AssocList2),
    any_map__init(Common0),
    any_map__intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred any_map__intersect_2(any_assoc_list(K, V), any_assoc_list(K, V),
    pred(V, V, V), any_map(K, V), any_map(K, V)).
:- mode any_map__intersect_2(ia, ia, pred(ia, ia, oa) is semidet, ia, oa)
    is semidet.
:- mode any_map__intersect_2(ia, ia, pred(ia, ia, oa) is det, ia, oa)
    is det.

any_map__intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
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
            any_map__det_insert(Common0, Key1, Value, Common1),
            any_map__intersect_2(AssocTail1, AssocTail2, CommonPred,
                Common1, Common)
        ;
            R = (<),
            any_map__intersect_2(AssocTail1, AssocList2, CommonPred,
                Common0, Common)
        ;
            R = (>),
            any_map__intersect_2(AssocList1, AssocTail2, CommonPred,
                Common0, Common)
        )
    ).

any_map__det_intersect(CommonPred, Map1, Map2, Common) :-
    promise_pure (
        any_map__intersect(CommonPred, Map1, Map2, CommonPrime)
    ->
        Common = CommonPrime
    ;
        error("any_map__det_intersect: any_map__intersect failed")
    ).

%-----------------------------------------------------------------------------%

any_map__union(CommonPred, Map1, Map2, Common) :-
    any_map__to_sorted_any_assoc_list(Map1, AssocList1),
    any_map__to_sorted_any_assoc_list(Map2, AssocList2),
    any_map__init(Common0),
    any_map__union_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred any_map__union_2(any_assoc_list(K, V), any_assoc_list(K, V),
    pred(V, V, V), any_map(K, V), any_map(K, V)).
:- mode any_map__union_2(ia, ia, pred(ia, ia, oa) is semidet, ia, oa)
    is semidet.
:- mode any_map__union_2(ia, ia, pred(ia, ia, oa) is det, ia, oa)
    is det.

any_map__union_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
    (
        AssocList1 = [],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [_ | _],
        AssocList2 = [],
        any_map__det_insert_from_any_assoc_list(Common0, AssocList1, Common)
    ;
        AssocList1 = [],
        AssocList2 = [_ | _],
        any_map__det_insert_from_any_assoc_list(Common0, AssocList2, Common)
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        unsafe_cast_to_ground(Key1),
        unsafe_cast_to_ground(Key2),
        compare(R, Key1, Key2),
        (
            R = (=),
            call(CommonPred, Value1, Value2, Value),
            any_map__det_insert(Common0, Key1, Value, Common1),
            any_map__union_2(AssocTail1, AssocTail2, CommonPred,
                Common1, Common)
        ;
            R = (<),
            any_map__det_insert(Common0, Key1, Value1, Common1),
            any_map__union_2(AssocTail1, AssocList2, CommonPred,
                Common1, Common)
        ;
            R = (>),
            any_map__det_insert(Common0, Key2, Value2, Common1),
            any_map__union_2(AssocList1, AssocTail2, CommonPred,
                Common1, Common)
        )
    ).

any_map__det_union(CommonPred, Map1, Map2, Union) :-
    promise_pure (
        any_map__union(CommonPred, Map1, Map2, UnionPrime)
    ->
        Union = UnionPrime
    ;
        error("any_map__det_union: any_map__union failed")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

any_map__init = M :-
    any_map__init(M).

any_map__search(M, K) = V :-
    any_map__search(M, K, V).

any_map__lookup(M, K) = V :-
    any_map__lookup(M, K, V).

any_map__insert(M1, K, V) = M2 :-
    any_map__insert(M1, K, V, M2).

any_map__det_insert(M1, K, V) = M2 :-
    any_map__det_insert(M1, K, V, M2).

any_map__det_insert_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    any_map__det_insert_from_corresponding_lists(M1, Ks, Vs, M2).

any_map__det_insert_from_any_assoc_list(M1, AL) = M2 :-
    any_map__det_insert_from_any_assoc_list(M1, AL, M2).

any_map__set_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    any_map__set_from_corresponding_lists(M1, Ks, Vs, M2).

any_map__set_from_any_assoc_list(M1, AL) = M2 :-
    any_map__set_from_any_assoc_list(M1, AL, M2).

any_map__update(M1, K, V) = M2 :-
    any_map__update(M1, K, V, M2).

any_map__det_update(M1, K, V) = M2 :-
    any_map__det_update(M1, K, V, M2).

any_map__set(M1, K, V) = M2 :-
    any_map__set(M1, K, V, M2).

any_map__keys(M) = Ks :-
    any_map__keys(M, Ks).

any_map__sorted_keys(M) = Ks :-
    any_map__sorted_keys(M, Ks).

any_map__values(M) = Vs :-
    any_map__values(M, Vs).

any_map__to_any_assoc_list(M) = AL :-
    any_map__to_any_assoc_list(M, AL).

any_map__to_sorted_any_assoc_list(M) = AL :-
    any_map__to_sorted_any_assoc_list(M, AL).

any_map__from_any_assoc_list(AL) = M :-
    any_map__from_any_assoc_list(AL, M).

any_map__from_sorted_any_assoc_list(AL) = M :-
    any_map__from_sorted_any_assoc_list(AL, M).

any_map__delete(M1, K) = M2 :-
    any_map__delete(M1, K, M2).

any_map__delete_list(M1, Ks) = M2 :-
    any_map__delete_list(M1, Ks, M2).

any_map__count(M) = N :-
    any_map__count(M, N).
any_map__from_corresponding_lists(Ks, Vs) = M :-
    any_map__from_corresponding_lists(Ks, Vs, M).

any_map__merge(M1, M2) = M3 :-
    any_map__merge(M1, M2, M3).

any_map__overlay(M1, M2) = M3 :-
    any_map__overlay(M1, M2, M3).

any_map__overlay_large_map(M1, M2) = M3 :-
    any_map__overlay_large_map(M1, M2, M3).

any_map__select(M1, S) = M2 :-
    any_map__select(M1, S, M2).

any_map__apply_to_list(Ks, M) = Vs :-
    any_map__apply_to_list(Ks, M, Vs).

any_map__optimize(M1) = M2 :-
    any_map__optimize(M1, M2).

:- pragma promise_pure(any_map__foldl/3).

any_map__foldl(F::in(func(in, ia, in) = out is det), M::ia, A::in) = (B::out) :-
    P = ( pred(W::in, X::ia, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    any_map__foldl(P, M, A, B).

any_map__foldl(F::in(func(in, ia, ia) = oa is det), M::ia, A::ia) = (B::oa) :-
    P = ( pred(W::in, X::ia, Y::ia, Z::oa) is det :- Z = F(W, X, Y) ),
    any_map__foldl(P, M, A, B).

any_map__map_values(F, M1) = M2 :-
    P = ( pred(X::in, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_map__map_values(P, M1, M2).

any_map__intersect(F, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_map__intersect(P, M1, M2, M3).

any_map__det_intersect(PF, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is semidet :- Z = PF(X, Y) ),
    any_map__det_intersect(P, M1, M2, M3).

any_map__union(F, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_map__union(P, M1, M2, M3).

any_map__det_union(F, M1, M2) = M3 :-
    P = ( pred(X::ia, Y::ia, Z::oa) is semidet :- Z = F(X, Y) ),
    any_map__det_union(P, M1, M2, M3).

any_map__elem(Key, Map) = any_map__search(Map, Key).

any_map__det_elem(Key, Map) = any_map__lookup(Map, Key).

'any_map__elem :='(Key, Map, Value) = any_map__set(Map, Key, Value).

'any_map__det_elem :='(Key, Map, Value) = any_map__det_update(Map, Key, Value).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
