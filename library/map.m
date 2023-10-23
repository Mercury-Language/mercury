%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: map.m.
% Main author: fjh, conway.
% Stability: high.
%
% This file provides the 'map' abstract data type.
%
% A map (also known as a dictionary or an associative array) is a collection
% of (Key, Value) pairs that allows you to look up any Value given its Key.
% Each Key has exactly only one corresponding Value. (If you want the ability
% to store more than one Value for a given Key, use either multi_map.m
% or one_or_more_map.m.)
%
% The implementation uses balanced 2-3-4 trees, as provided by tree234.m.
% Virtually all the predicates in this file just forward the work
% to the corresponding predicate in tree234.m.
%
% Note: 2-3-4 trees do not have a canonical representation for any given map.
% This means that two maps that represent the same set of key-value pairs
% may have different internal representations, and that therefore they
% may fail to unify and may compare as unequal. The reason for the difference
% in the internal representation is usually that the (Key, Value) pairs were
% inserted into the two maps in different orders, or that the two maps
% have a different history of deletions. If you want to know whether
% two maps contain the same set of (Key, Data) pairs, use the map.equal/2
% predicate below.
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
%
% Initial creation of maps.
%

    % Create an empty map.
    %
:- func init = (map(K, V)::uo) is det.
:- pred init(map(_, _)::uo) is det.

    % Create a map containing only the given key-value pair.
    %
:- func singleton(K, V) = map(K, V).

%---------------------------------------------------------------------------%
%
% Emptiness tests.
%

    % Check whether a map is empty.
    %
:- pred is_empty(map(_, _)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Searching for a key.
%

    % Succeed iff the map contains the given key.
    %
:- pred contains(map(K, _V)::in, K::in) is semidet.

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

%---------------------------------------------------------------------------%
%
% Looking for the minimum and maximum keys.
%

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

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

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

    % Apply det_insert to key - value pairs from an assoc_list.
    %
:- func det_insert_from_assoc_list(map(K, V), assoc_list(K, V)) = map(K, V).
:- pred det_insert_from_assoc_list(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.

%---------------------%

    % search_insert(K, V, MaybeOldV, !Map):
    %
    % Search for the key K in the map.
    %
    % If the key is already in !.Map, with corresponding value OldV,
    % then set MaybeOldV to yes(OldV), and leave !Map unchanged.
    %
    % If the key is not already in !.Map, then insert it into !Map
    % with value V, and set MaybeOldV to no.
    %
:- pred search_insert(K::in, V::in, maybe(V)::out,
    map(K, V)::in, map(K, V)::out) is det.

%---------------------%

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

%---------------------%

    % If the key is already present update its corresponding value.
    % If the key is not present, insert it with the given value.
    %
:- func set(map(K, V), K, V) = map(K, V).
:- pred set(K::in, V::in, map(K, V)::in, map(K, V)::out) is det.

    % Apply set to key - value pairs from corresponding lists.
    %
:- func set_from_corresponding_lists(map(K, V), list(K), list(V)) = map(K, V).
:- pred set_from_corresponding_lists(list(K)::in, list(V)::in,
    map(K, V)::in, map(K, V)::out) is det.

    % Apply set to key - value pairs from an assoc_list.
    %
:- func set_from_assoc_list(map(K, V), assoc_list(K, V)) = map(K, V).
:- pred set_from_assoc_list(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.

%---------------------%

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
:- pred delete_sorted_list(list(K)::in, map(K, V)::in, map(K, V)::out) is det.

%---------------------%

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

%---------------------------------------------------------------------------%
%
% Field selection for maps.
%

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
%
% Returning keys and values.
%

    % Return all the keys in the map, and their corresponding values,
    % one key-value pair at a time.
    %
:- pred member(map(K, V)::in, K::out, V::out) is nondet.

    % Given a map, return a list of all the keys in the map.
    %
:- func keys(map(K, _V)) = list(K).
:- pred keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % in sorted order.
    %
:- func sorted_keys(map(K, _V)) = list(K).
:- pred sorted_keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % as a set.
    %
:- func keys_as_set(map(K, _V)) = set(K).
:- pred keys_as_set(map(K, _V)::in, set(K)::out) is det.

    % Given a map, return a list of all the values in the map.
    %
:- func values(map(_K, V)) = list(V).
:- pred values(map(_K, V)::in, list(V)::out) is det.

:- pred keys_and_values(map(K, V)::in, list(K)::out, list(V)::out) is det.

    % Given a map, succeed if and only if the given list is the list
    % of all the keys in the map.
    % `sorted_keys_match(Map, List)' is equivalent to the conjunction,
    % `sorted_keys(Map, Keys), Keys = List", but it allocates no memory,
    % and it traverses Map only up to the first mismatch.
    %
:- pred sorted_keys_match(map(K, V)::in, list(K)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on values.
%

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

%---------------------------------------------------------------------------%
%
% Converting maps to lists.
%

    % Convert an association list to a map.
    %
:- func from_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred from_assoc_list(assoc_list(K, V)::in, map(K, V)::out) is det.

    % Convert a sorted association list with no duplicated keys to a map.
    %
:- func from_sorted_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred from_sorted_assoc_list(assoc_list(K, V)::in, map(K, V)::out) is det.

    % Convert a reverse sorted association list with no duplicated keys
    % to a map.
    %
:- func from_rev_sorted_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred from_rev_sorted_assoc_list(assoc_list(K, V)::in, map(K, V)::out)
    is det.

    % Convert a pair of lists (which must be of the same length) to a map.
    %
:- func from_corresponding_lists(list(K), list(V)) = map(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in, map(K, V)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Converting lists to maps.
%

    % Convert a map to an association list.
    %
:- func to_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred to_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert a map to an association list which is sorted on the keys.
    %
:- func to_sorted_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred to_sorted_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Reversing a map.
%

    % Consider the original map a set of key-value pairs. This predicate
    % returns a map that maps each value to the set of keys it is paired with
    % in the original map.
    %
:- func reverse_map(map(K, V)) = map(V, set(K)).

%---------------------------------------------------------------------------%
%
% Selecting subsets of maps.
%

    % select takes a map and a set of keys, and returns a map
    % containing the keys in the set and their corresponding values.
    %
:- func select(map(K, V), set(K)) = map(K, V).
:- pred select(map(K, V)::in, set(K)::in, map(K, V)::out) is det.

    % select_sorted_list takes a map and a sorted list of keys without
    % duplicates, and returns a map containing the keys in the list
    % and their corresponding values.
    %
:- func select_sorted_list(map(K, V), list(K)) = map(K, V).
:- pred select_sorted_list(map(K, V)::in, list(K)::in, map(K, V)::out) is det.

    % select_unselect takes a map and a set of keys, and returns two maps:
    % the first containing the keys in the set and their corresponding values,
    % the second containing the keys NOT in the set and their corresponding
    % values.
    %
:- pred select_unselect(map(K, V)::in, set(K)::in,
    map(K, V)::out, map(K, V)::out) is det.

    % select_unselect_sorted_list takes a map and a sorted list of keys
    % without duplicates, and returns two maps:
    % the first containing the keys in the list and their corresponding values,
    % the second containing the keys NOT in the list and their corresponding
    % values.
    %
:- pred select_unselect_sorted_list(map(K, V)::in, list(K)::in,
    map(K, V)::out, map(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Selecting subsets of values.
%

    % Given a list of keys, produce a list of their corresponding
    % values in a specified map.
    %
:- func apply_to_list(list(K), map(K, V)) = list(V).
:- pred apply_to_list(list(K)::in, map(K, V)::in, list(V)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations on two or more maps.
%

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

%---------------------%

    % Given two maps MapA and MapB, create a third map CommonMap that
    % has only the keys that occur in both MapA and MapB. For keys
    % that occur in both MapA and MapB, look up the corresponding values.
    % If they are the same, include the key/value pair in CommonMap.
    % If they differ, do not include the key in CommonMap.
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

    % Given two maps MapA and MapB, create a third map, IntersectMap,
    % that has only the keys that occur in both MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied function to the values associated with
    % the key in MapA and MapB.
    % on the values associated with some common key.
    %
:- func intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).

    % Given two maps MapA and MapB, create a third map, IntersectMap,
    % that has only the keys that occur in both MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied predicate to the values associated with
    % the key in MapA and MapB. Fail if and only if this predicate fails
    % on the values associated with some common key.
    %
:- pred intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode intersect(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode intersect(in(pred(in, in, out) is det), in, in, out) is det.

    % Calls intersect. Throws an exception if intersect fails.
    %
:- func det_intersect((func(V, V) = V)::in(func(in, in) = out is semidet),
    map(K, V)::in, map(K, V)::in) = (map(K, V)::out) is det.
:- pred det_intersect((pred(V, V, V))::in(pred(in, in, out) is semidet),
    map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % intersect_list(Pred, M, Ms, ResultM):
    %
    % Take the non-empty list of maps [M | Ms], and intersect pairs of
    % those maps (using map.intersect above) until there is only one map left.
    % Return this map as ResultM. The order of in which those intersect
    % operations are performed is not defined, so the caller should choose
    % a Pred for which the order does not matter.
    %
:- pred intersect_list(pred(V, V, V), map(K, V), list(map(K, V)), map(K, V)).
:- mode intersect_list(in(pred(in, in, out) is semidet),
    in, in, out) is semidet.
:- mode intersect_list(in(pred(in, in, out) is det),
    in, in, out) is det.

%---------------------%

    % Given two maps MapA and MapB, create a third map, UnionMap, that
    % contains all the keys that occur in either MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied function to the values associated with the key
    % in MapA and MapB.
    %
:- func union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).

    % Given two maps MapA and MapB, create a third map, UnionMap, that
    % contains all the keys that occur in either MapA and MapB. For keys
    % that occur in both MapA and MapB, compute the value in the final map
    % by applying the supplied predicate to the values associated with the key
    % in MapA and MapB. Fail if and only if this predicate fails on
    % the values associated with some common key.
    %
:- pred union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode union(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode union(in(pred(in, in, out) is det), in, in, out) is det.

    % Calls union. Throws an exception if union fails.
    %
:- func det_union((func(V, V) = V)::in(func(in, in) = out is semidet),
    map(K, V)::in, map(K, V)::in) = (map(K, V)::out) is det.
:- pred det_union(pred(V, V, V)::in(pred(in, in, out) is semidet),
    map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % union_list(Pred, M, Ms, ResultM):
    %
    % Take the non-empty list of maps [M | Ms], and union pairs of those maps
    % (using union above) until there is only one map left. Return this map
    % as ResultM. The order of in which those union operations are performed
    % is not defined, so the caller should choose a Pred for which the order
    % does not matter.
    %
:- pred union_list(pred(V, V, V), map(K, V), list(map(K, V)), map(K, V)).
:- mode union_list(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode union_list(in(pred(in, in, out) is det), in, in, out) is det.

%---------------------------------------------------------------------------%
%
% Counting.
%

    % Count the number of elements in the map.
    %
:- func count(map(K, V)) = int.
:- pred count(map(K, V)::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Comparisons between maps.
%

    % True if both maps have the same set of key-value pairs, regardless of
    % how the maps were constructed.
    %
    % Unifying maps does not work as one might expect, because the internal
    % structures of two maps that contain the same set of key-value pairs
    % may be different.
    %
:- pred equal(map(K, V)::in, map(K, V)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Optimization.
%

    % Declaratively, a no-operation.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the map in the expectation
    % of a number of lookups but few or no modifications.
    %
    % This operation is here only for "cultural compatibility"
    % with the modules that operation on trees that may be unbalanced.
    % 2-3-4 trees are always guaranteed to be balanced, so they do not need
    % any such optimization.
    %
:- func optimize(map(K, V)) = map(K, V).
:- pred optimize(map(K, V)::in, map(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%


    % Perform an inorder traversal of the map, applying
    % an accumulator predicate for each key-value pair.
    %
:- func foldl(func(K, V, A) = A, map(K, V), A) = A.
:- pred foldl(pred(K, V, A, A), map(K, V), A, A).
:- mode foldl(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldl(in(pred(in, in, in, out) is cc_multi), in, in, out) is cc_multi.
:- mode foldl(in(pred(in, in, di, uo) is cc_multi), in, di, uo) is cc_multi.
:- mode foldl(in(pred(in, in, mdi, muo) is cc_multi), in, mdi, muo)
    is cc_multi.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with two accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl2(pred(K, V, A, A, B, B), map(K, V), A, A, B, B).
:- mode foldl2(in(pred(in, in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode foldl2(in(pred(in, in, di, uo, di, uo) is det),
    in, di, uo, di, uo) is det.
:- mode foldl2(in(pred(in, in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.
:- mode foldl2(in(pred(in, in, in, out, in, out) is cc_multi),
    in, in, out, in, out) is cc_multi.
:- mode foldl2(in(pred(in, in, in, out, mdi, muo) is cc_multi),
    in, in, out, mdi, muo) is cc_multi.
:- mode foldl2(in(pred(in, in, in, out, di, uo) is cc_multi),
    in, in, out, di, uo) is cc_multi.
:- mode foldl2(in(pred(in, in, di, uo, di, uo) is cc_multi),
    in, di, uo, di, uo) is cc_multi.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with three accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl3(pred(K, V, A, A, B, B, C, C), map(K, V), A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(in(pred(in, in, in, out, di, uo, di, uo) is det),
    in, in, out, di, uo, di, uo) is det.
:- mode foldl3(in(pred(in, in, di, uo, di, uo, di, uo) is det),
    in, di, uo, di, uo, di, uo) is det.
:- mode foldl3(in(pred(in, in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(in(pred(in, in, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(in(pred(in, in, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, di, uo) is semidet.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with four accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl4(pred(K, V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldl4(
    in(pred(in, in, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(
    in(pred(in, in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(
    in(pred(in, in, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(
    in(pred(in, in, in, out, in, out, di, uo, di, uo) is det),
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldl4(
    in(pred(in, in, in, out, di, uo, di, uo, di, uo) is det),
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldl4(
    in(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det),
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldl4(
    in(pred(in, in, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(
    in(pred(in, in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(
    in(pred(in, in, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, di, uo) is semidet.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with five accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl5(pred(K, V, A, A, B, B, C, C, D, D, E, E), map(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5(in(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is det),
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(in(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is det),
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(in(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is det),
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(in(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is semidet),
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(in(pred(in, in,in, out,  in, out, in, out, in, out, mdi, muo)
    is semidet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(in(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is semidet),
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with five accumulators for each key-value pair.
    % (Although no more expressive than foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred foldl6(pred(K, V, A, A, B, B, C, C, D, D, E, E, F, F), map(K, V),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode foldl6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl6(in(pred(in, in,in, out,  in, out, in, out, in, out,
    in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

%---------------------%

    % Perform an inorder traversal by key of the map, applying an accumulator
    % predicate for value.
    %
:- pred foldl_values(pred(V, A, A), map(K, V), A, A).
:- mode foldl_values(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl_values(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl_values(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl_values(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl_values(in(pred(in, mdi, muo) is semidet), in, mdi, muo)
    is semidet.
:- mode foldl_values(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldl_values(in(pred(in, in, out) is cc_multi), in, in, out)
    is cc_multi.
:- mode foldl_values(in(pred(in, di, uo) is cc_multi), in, di, uo) is cc_multi.
:- mode foldl_values(in(pred(in, mdi, muo) is cc_multi), in, mdi, muo)
    is cc_multi.

    % As above, but with two accumulators.
    %
:- pred foldl2_values(pred(V, A, A, B, B), map(K, V), A, A, B, B).
:- mode foldl2_values(in(pred(in, in, out, in, out) is det), in,
    in, out, in, out) is det.
:- mode foldl2_values(in(pred(in, in, out, mdi, muo) is det), in,
    in, out, mdi, muo) is det.
:- mode foldl2_values(in(pred(in, in, out, di, uo) is det), in,
    in, out, di, uo) is det.
:- mode foldl2_values(in(pred(in, in, out, in, out) is semidet), in,
    in, out, in, out) is semidet.
:- mode foldl2_values(in(pred(in, in, out, mdi, muo) is semidet), in,
    in, out, mdi, muo) is semidet.
:- mode foldl2_values(in(pred(in, in, out, di, uo) is semidet), in,
    in, out, di, uo) is semidet.
:- mode foldl2_values(in(pred(in, in, out, in, out) is cc_multi), in,
    in, out, in, out) is cc_multi.
:- mode foldl2_values(in(pred(in, in, out, mdi, muo) is cc_multi), in,
    in, out, mdi, muo) is cc_multi.
:- mode foldl2_values(in(pred(in, in, out, di, uo) is cc_multi), in,
    in, out, di, uo) is cc_multi.

    % As above, but with three accumulators.
    %
:- pred foldl3_values(pred(V, A, A, B, B, C, C), map(K, V),
    A, A, B, B, C, C).
:- mode foldl3_values(in(pred(in, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out) is det.
:- mode foldl3_values(in(pred(in, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3_values(in(pred(in, in, out, in, out, di, uo) is det), in,
    in, out, in, out, di, uo) is det.
:- mode foldl3_values(in(pred(in, in, out, in, out, in, out) is semidet), in,
    in, out, in, out, in, out) is semidet.
:- mode foldl3_values(in(pred(in, in, out, in, out, mdi, muo) is semidet), in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_values(in(pred(in, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, di, uo) is semidet.
:- mode foldl3_values(in(pred(in, in, out, in, out, in, out) is cc_multi), in,
    in, out, in, out, in, out) is cc_multi.
:- mode foldl3_values(in(pred(in, in, out, in, out, mdi, muo) is cc_multi), in,
    in, out, in, out, mdi, muo) is cc_multi.
:- mode foldl3_values(in(pred(in, in, out, in, out, di, uo) is cc_multi), in,
    in, out, in, out, di, uo) is cc_multi.

%---------------------%

    % As above, but with four accumulators.
    %
:- pred foldl4_values(pred(V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldl4_values(in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4_values(in(pred(in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4_values(in(pred(in, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4_values(in(pred(in, in, out, in, out, di, uo, di, uo) is det),
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldl4_values(in(pred(in, in, out, di, uo, di, uo, di, uo) is det),
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldl4_values(in(pred(in, di, uo, di, uo, di, uo, di, uo) is det),
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldl4_values(in(pred(in, in, out, in, out, in, out, in, out) is
semidet),
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4_values(in(pred(in, in, out, in, out, in, out, mdi, muo) is
semidet),
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4_values(in(pred(in, in, out, in, out, in, out, di, uo) is
semidet),
    in, in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with five accumulators.
    %
:- pred foldl5_values(pred(V, A, A, B, B, C, C, D, D, E, E), map(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5_values(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is det),
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5_values(in(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is det),
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5_values(in(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is det),
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5_values(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is semidet),
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5_values(in(pred(in,in, out,  in, out, in, out, in, out, mdi, muo)
    is semidet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5_values(in(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is semidet),
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with five accumulators.
    %
:- pred foldl6_values(pred(V, A, A, B, B, C, C, D, D, E, E, F, F), map(K, V),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode foldl6_values(in(pred(in, in, out, in, out, in, out, in, out,
    in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl6_values(in(pred(in, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl6_values(in(pred(in, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl6_values(in(pred(in, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl6_values(in(pred(in,in, out,  in, out, in, out, in, out,
    in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl6_values(in(pred(in, in, out, in, out, in, out, in, out,
    in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- func foldr(func(K, V, A) = A, map(K, V), A) = A.
:- pred foldr(pred(K, V, A, A), map(K, V), A, A).
:- mode foldr(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode foldr(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode foldr(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldr(in(pred(in, in, in, out) is cc_multi), in, in, out) is cc_multi.
:- mode foldr(in(pred(in, in, mdi, muo) is cc_multi), in, mdi, muo)
    is cc_multi.
:- mode foldr(in(pred(in, in, di, uo) is cc_multi), in, di, uo) is cc_multi.

:- pred foldr2(pred(K, V, A, A, B, B), map(K, V), A, A, B, B).
:- mode foldr2(in(pred(in, in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldr2(in(pred(in, in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldr2(in(pred(in, in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode foldr2(in(pred(in, in, di, uo, di, uo) is det),
    in, di, uo, di, uo) is det.
:- mode foldr2(in(pred(in, in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldr2(in(pred(in, in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldr2(in(pred(in, in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

:- pred foldr3(pred(K, V, A, A, B, B, C, C), map(K, V), A, A, B, B, C, C).
:- mode foldr3(in(pred(in, in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode foldr3(in(pred(in, in, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, mdi, muo) is det.
:- mode foldr3(in(pred(in, in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
:- mode foldr3(in(pred(in, in, in, out, di, uo, di, uo) is det),
    in, in, out, di, uo, di, uo) is det.
:- mode foldr3(in(pred(in, in, di, uo, di, uo, di, uo) is det),
    in, di, uo, di, uo, di, uo) is det.
:- mode foldr3(in(pred(in, in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldr3(in(pred(in, in, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldr3(in(pred(in, in, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, di, uo) is semidet.

:- pred foldr4(pred(K, V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldr4(
    in(pred(in, in, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldr4(
    in(pred(in, in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr4(
    in(pred(in, in, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldr4(
    in(pred(in, in, in, out, in, out, di, uo, di, uo) is det),
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldr4(
    in(pred(in, in, in, out, di, uo, di, uo, di, uo) is det),
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldr4(
    in(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det),
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldr4(
    in(pred(in, in, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr4(
    in(pred(in, in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr4(
    in(pred(in, in, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldr5(pred(K, V, A, A, B, B, C, C, D, D, E, E), map(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldr5(in(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is det),
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldr5(in(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is det),
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr5(in(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is det),
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldr5(in(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is semidet),
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr5(in(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr5(in(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is semidet),
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldr6(pred(K, V, A, A, B, B, C, C, D, D, E, E, F, F), map(K, V),
    A, A, B, B, C, C, D, D, E, E, F, F).
:- mode foldr6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldr6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldr6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr6(in(pred(in, in, in, out, in, out, in, out, in, out,
    in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

%---------------------%

    % Apply a transformation predicate to all the values in a map.
    %
:- func map_values(func(K, V) = W, map(K, V)) = map(K, W).
:- pred map_values(pred(K, V, W), map(K, V), map(K, W)).
:- mode map_values(in(pred(in, in, out) is det), in, out) is det.
:- mode map_values(in(pred(in, in, out) is semidet), in, out) is semidet.

    % Same as map_values, but do not pass the key to the given predicate.
    %
:- func map_values_only(func(V) = W, map(K, V)) = map(K, W).
:- pred map_values_only(pred(V, W), map(K, V), map(K, W)).
:- mode map_values_only(in(pred(in, out) is det), in, out) is det.
:- mode map_values_only(in(pred(in, out) is semidet), in, out) is semidet.

    % Apply a transformation predicate to all the values in a map.
    %
:- pred filter_map_values(pred(K, V, W)::in(pred(in, in, out) is semidet),
    map(K, V)::in, map(K, W)::out) is det.

    % Same as map_values, but do not pass the key to the given predicate.
    %
:- pred filter_map_values_only(pred(V, W)::in(pred(in, out) is semidet),
    map(K, V)::in, map(K, W)::out) is det.

%---------------------%

    % Perform an inorder traversal by key of the map, applying a transformation
    % predicate to each value while updating an accumulator.
    %
:- pred map_foldl(pred(K, V, W, A, A), map(K, V), map(K, W), A, A).
:- mode map_foldl(in(pred(in, in, out, in, out) is det), in, out, in, out)
    is det.
:- mode map_foldl(in(pred(in, in, out, mdi, muo) is det), in, out, mdi, muo)
    is det.
:- mode map_foldl(in(pred(in, in, out, di, uo) is det), in, out, di, uo)
    is det.
:- mode map_foldl(in(pred(in, in, out, in, out) is semidet), in, out,
    in, out) is semidet.
:- mode map_foldl(in(pred(in, in, out, mdi, muo) is semidet), in, out,
    mdi, muo) is semidet.
:- mode map_foldl(in(pred(in, in, out, di, uo) is semidet), in, out,
    di, uo) is semidet.

    % As map_foldl, but with two accumulators.
    %
:- pred map_foldl2(pred(K, V, W, A, A, B, B), map(K, V), map(K, W),
    A, A, B, B).
:- mode map_foldl2(in(pred(in, in, out, in, out, in, out) is det),
    in, out, in, out, in, out) is det.
:- mode map_foldl2(in(pred(in, in, out, in, out, mdi, muo) is det),
    in, out, in, out, mdi, muo) is det.
:- mode map_foldl2(in(pred(in, in, out, in, out, di, uo) is det),
    in, out, in, out, di, uo) is det.
:- mode map_foldl2(in(pred(in, in, out, di, uo, di, uo) is det),
    in, out, di, uo, di, uo) is det.
:- mode map_foldl2(in(pred(in, in, out, in, out, in, out) is semidet),
    in, out, in, out, in, out) is semidet.
:- mode map_foldl2(in(pred(in, in, out, in, out, mdi, muo) is semidet),
    in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl2(in(pred(in, in, out, in, out, di, uo) is semidet),
    in, out, in, out, di, uo) is semidet.

    % As map_foldl, but with three accumulators.
    %
:- pred map_foldl3(pred(K, V, W, A, A, B, B, C, C), map(K, V), map(K, W),
    A, A, B, B, C, C).
:- mode map_foldl3(
    in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, out, in, out, in, out, in, out) is det.
:- mode map_foldl3(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is det),
    in, out, in, out, in, out, mdi, muo) is det.
:- mode map_foldl3(
    in(pred(in, in, out, di, uo, di, uo, di, uo) is det),
    in, out, di, uo, di, uo, di, uo) is det.
:- mode map_foldl3(
    in(pred(in, in, out, in, out, in, out, di, uo) is det),
    in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl3(
    in(pred(in, in, out, in, out, di, uo, di, uo) is det),
    in, out, in, out, di, uo, di, uo) is det.
:- mode map_foldl3(
    in(pred(in, in, out, in, out, in, out, in, out) is semidet),
    in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl3(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl3(
    in(pred(in, in, out, in, out, in, out, di, uo) is semidet),
    in, out, in, out, in, out, di, uo) is semidet.

    % As map_foldl, but with four accumulators.
    %
:- pred map_foldl4(pred(K, V, W, A, A, B, B, C, C, D, D), map(K, V), map(K, W),
    A, A, B, B, C, C, D, D).
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is det),
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is det),
    in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode map_foldl4(in(pred(in, in, out, in, out, di, uo, di, uo, di, uo) is
det),
    in, out, in, out, di, uo, di, uo, di, uo) is det.
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, in, out, di, uo) is
det),
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, di, uo, di, uo) is
det),
    in, out, in, out, in, out, di, uo, di, uo) is det.
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, in, out, in, out)
    is semidet),
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet),
    in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl4(in(pred(in, in, out, in, out, in, out, in, out, di, uo)
    is semidet),
    in, out, in, out, in, out, in, out, di, uo) is semidet.

%---------------------%

    % As map_foldl, but without passing the key to the predicate.
    %
:- pred map_values_foldl(pred(V, W, A, A), map(K, V), map(K, W), A, A).
:- mode map_values_foldl(in(pred(in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode map_values_foldl(in(pred(in, out, in, out) is det),
    in, out, in, out) is det.
:- mode map_values_foldl(in(pred(in, out, in, out) is semidet),
    in, out, in, out) is semidet.

    % As map_values_foldl, but with two accumulators.
    %
:- pred map_values_foldl2(pred(V, W, A, A, B, B), map(K, V), map(K, W),
    A, A, B, B).
:- mode map_values_foldl2(in(pred(in, out, di, uo, di, uo) is det),
    in, out, di, uo, di, uo) is det.
:- mode map_values_foldl2(in(pred(in, out, in, out, di, uo) is det),
    in, out, in, out, di, uo) is det.
:- mode map_values_foldl2(in(pred(in, out, in, out, in, out) is det),
    in, out, in, out, in, out) is det.
:- mode map_values_foldl2(in(pred(in, out, in, out, in, out) is semidet),
    in, out, in, out, in, out) is semidet.

    % As map_values_foldl, but with three accumulators.
    %
:- pred map_values_foldl3(pred(V, W, A, A, B, B, C, C),
    map(K, V), map(K, W), A, A, B, B, C, C).
:- mode map_values_foldl3(
    in(pred(in, out, di, uo, di, uo, di, uo) is det),
    in, out, di, uo, di, uo, di, uo) is det.
:- mode map_values_foldl3(
    in(pred(in, out, in, out, di, uo, di, uo) is det),
    in, out, in, out, di, uo, di, uo) is det.
:- mode map_values_foldl3(
    in(pred(in, out, in, out, in, out, di, uo) is det),
    in, out, in, out, in, out, di, uo) is det.
:- mode map_values_foldl3(
    in(pred(in, out, in, out, in, out, in, out) is det),
    in, out, in, out, in, out, in, out) is det.
:- mode map_values_foldl3(
    in(pred(in, out, in, out, in, out, in, out) is semidet),
    in, out, in, out, in, out, in, out) is semidet.

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
% sets of keys of the input maps are not disjoint, it won't throw an exception,
% but will insert the key and the smallest of the two corresponding values into
% the output map. Eventually we would like to get rid of this version but some
% of the code in the compiler currently assumes this behaviour, and
% fixing this is non-trivial.

:- func old_merge(map(K, V), map(K, V)) = map(K, V).
:- pred old_merge(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma type_spec(pred(map.search/3), K = var(_)).
:- pragma type_spec(pred(map.search/3), K = int).

:- pragma type_spec(func(map.search/2), K = var(_)).
:- pragma type_spec(func(map.search/2), K = int).

:- pragma type_spec(pred(map.lookup/3), K = var(_)).
:- pragma type_spec(pred(map.lookup/3), K = int).

:- pragma type_spec(func(map.lookup/2), K = var(_)).
:- pragma type_spec(func(map.lookup/2), K = int).

:- pragma type_spec(map.insert(in, in, in, out), K = var(_)).
:- pragma type_spec(map.insert(in, in, in, out), K = int).

:- pragma type_spec(map.det_insert(in, in, in, out), K = var(_)).
:- pragma type_spec(map.det_insert(in, in, in, out), K = int).

:- pragma type_spec(map.set(in, in, in, out), K = var(_)).
:- pragma type_spec(map.set(in, in, in, out), K = int).

:- pragma type_spec(map.update(in, in, in, out), K = var(_)).
:- pragma type_spec(map.update(in, in, in, out), K = int).

:- pragma type_spec(pred(map.det_update/4), K = var(_)).
:- pragma type_spec(pred(map.det_update/4), K = int).

:- pragma type_spec(pred(map.search_insert/5), K = var(_)).
:- pragma type_spec(pred(map.search_insert/5), K = int).

:- pragma type_spec(func(map.overlay/2), K = var(_)).
:- pragma type_spec(pred(map.overlay/3), K = var(_)).

:- pragma type_spec(func(map.select/2), K = var(_)).
:- pragma type_spec(pred(map.select/3), K = var(_)).

:- pragma type_spec(func(map.select_sorted_list/2), K = var(_)).
:- pragma type_spec(pred(map.select_sorted_list/3), K = var(_)).

:- pragma type_spec(func(map.elem/2), K = int).
:- pragma type_spec(func(map.elem/2), K = var(_)).

:- pragma type_spec(func(map.det_elem/2), K = int).
:- pragma type_spec(func(map.det_elem/2), K = var(_)).

:- pragma type_spec(func('elem :='/3), K = int).
:- pragma type_spec(func('elem :='/3), K = var(_)).

:- pragma type_spec(func('det_elem :='/3), K = int).
:- pragma type_spec(func('det_elem :='/3), K = var(_)).

:- implementation.

:- import_module int.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init = M :-
    map.init(M).

init(M) :-
    tree234.init(M).

singleton(K, V) =
    tree234.singleton(K, V).

%---------------------------------------------------------------------------%

is_empty(M) :-
    tree234.is_empty(M).

%---------------------------------------------------------------------------%

contains(Map, K) :-
    map.search(Map, K, _).

search(M, K) = V :-
    map.search(M, K, V).

search(Map, K, V) :-
    tree234.search(Map, K, V).

lookup(M, K) = V :-
    map.lookup(M, K, V).

lookup(Map, K, V) :-
    ( if tree234.search(Map, K, VPrime) then
        V = VPrime
    else
        report_lookup_error("map.lookup: key not found", K, V)
    ).

inverse_search(Map, V, K) :-
    map.member(Map, K, V).

lower_bound_search(Map, SearchK, K, V) :-
    tree234.lower_bound_search(Map, SearchK, K, V).

lower_bound_lookup(Map, SearchK, K, V) :-
    ( if tree234.lower_bound_search(Map, SearchK, KPrime, VPrime) then
        K = KPrime,
        V = VPrime
    else
        report_lookup_error("map.lower_bound_lookup: key not found",
            SearchK, V)
    ).

upper_bound_search(Map, SearchK, K, V) :-
    tree234.upper_bound_search(Map, SearchK, K, V).

upper_bound_lookup(Map, SearchK, K, V) :-
    ( if tree234.upper_bound_search(Map, SearchK, KPrime, VPrime) then
        K = KPrime,
        V = VPrime
    else
        report_lookup_error("map.upper_bound_lookup: key not found",
            SearchK, V)
    ).

%---------------------------------------------------------------------------%

max_key(M) = tree234.max_key(M).

det_max_key(M) =
    ( if K = map.max_key(M) then
        K
    else
        unexpected($pred, "map.max_key failed")
    ).

min_key(M) = tree234.min_key(M).

det_min_key(M) =
    ( if K = map.min_key(M) then
        K
    else
        unexpected($pred, "map.min_key failed")
    ).

%---------------------------------------------------------------------------%

insert(M0, K, V) = M :-
    map.insert(K, V, M0, M).

insert(K, V, !Map) :-
    tree234.insert(K, V, !Map).

det_insert(M0, K, V) = M :-
    map.det_insert(K, V, M0, M).

det_insert(K, V, !Map) :-
    ( if tree234.insert(K, V, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("map.det_insert: key already present", K, V)
    ).

det_insert_from_corresponding_lists(M0, Ks, Vs) = M :-
    map.det_insert_from_corresponding_lists(Ks, Vs, M0, M).

det_insert_from_corresponding_lists([], [], !Map).
det_insert_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
det_insert_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    map.det_insert(K, V, !Map),
    map.det_insert_from_corresponding_lists(Ks, Vs, !Map).

det_insert_from_assoc_list(M0, AL) = M :-
    map.det_insert_from_assoc_list(AL, M0, M).

det_insert_from_assoc_list([], !Map).
det_insert_from_assoc_list([K - V | KVs], !Map) :-
    map.det_insert(K, V, !Map),
    map.det_insert_from_assoc_list(KVs, !Map).

%---------------------%

search_insert(K, V, MaybeOldV, !Map) :-
    tree234.search_insert(K, V, MaybeOldV, !Map).

%---------------------%

update(M0, K, V) = M :-
    map.update(K, V, M0, M).

update(K, V, !Map) :-
    tree234.update(K, V, !Map).

det_update(M0, K, V) = M :-
    map.det_update(K, V, M0, M).

det_update(K, V, !Map) :-
    ( if tree234.update(K, V, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("map.det_update: key not found", K, V)
    ).

%---------------------%

set(M0, K, V) = M :-
    map.set(K, V, M0, M).

set(K, V, !Map) :-
    tree234.set(K, V, !Map).

set_from_corresponding_lists(M0, Ks, Vs) = M :-
    map.set_from_corresponding_lists(Ks, Vs, M0, M).

set_from_corresponding_lists([], [], !Map).
set_from_corresponding_lists([], [_ | _], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([_ | _], [], _, _) :-
    unexpected($pred, "list length mismatch").
set_from_corresponding_lists([K | Ks], [V | Vs], !Map) :-
    map.set(K, V, !Map),
    map.set_from_corresponding_lists(Ks, Vs, !Map).

set_from_assoc_list(M0, AL) = M :-
    map.set_from_assoc_list(AL, M0, M).

set_from_assoc_list([], !Map).
set_from_assoc_list([K - V | KVs], !Map) :-
    map.set(K, V, !Map),
    map.set_from_assoc_list(KVs, !Map).

%---------------------%

delete(M0, K) = M :-
    map.delete(K, M0, M).

delete(Key, !Map) :-
    tree234.delete(Key, !Map).

delete_list(M0, Ks) = M :-
    map.delete_list(Ks, M0, M).

delete_list([], !Map).
delete_list([DeleteKey | DeleteKeys], !Map) :-
    map.delete(DeleteKey, !Map),
    map.delete_list(DeleteKeys, !Map).

delete_sorted_list(M0, Ks) = M :-
    map.delete_sorted_list(Ks, M0, M).

delete_sorted_list(DeleteKeys, !Map) :-
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

:- pred delete_sorted_list_loop(list(K)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::in, assoc_list(K, V)::out,
    assoc_list(K, V)::out) is det.

delete_sorted_list_loop([], Pairs, !RevPairs, Pairs).
delete_sorted_list_loop([_ | _], [], !RevPairs, []).
delete_sorted_list_loop([DeleteKey | DeleteKeys], [Pair0 | Pairs0],
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

%---------------------%

remove(Key, Value, !Map) :-
    tree234.remove(Key, Value, !Map).

det_remove(Key, Value, !Map) :-
    ( if tree234.remove(Key, ValuePrime, !.Map, MapPrime) then
        Value = ValuePrime,
        !:Map = MapPrime
    else
        report_lookup_error("map.det_remove: key not found", Key, Value)
    ).

remove_smallest(K, V, !Map) :-
    tree234.remove_smallest(K, V, !Map).

%---------------------------------------------------------------------------%

elem(Key, Map) = map.search(Map, Key).

det_elem(Key, Map) = map.lookup(Map, Key).

'elem :='(Key, Map, Value) = map.set(Map, Key, Value).

'det_elem :='(Key, Map, Value) = map.det_update(Map, Key, Value).

%---------------------------------------------------------------------------%

member(Map, K, V) :-
    tree234.member(Map, K, V).

keys(Map) = Keys :-
    map.keys(Map, Keys).

keys(Map, Keys) :-
    tree234.keys(Map, Keys).

sorted_keys(Map) = Keys :-
    map.sorted_keys(Map, Keys).

sorted_keys(Map, Keys) :-
    % Guaranteed to yield sorted lists.
    tree234.keys(Map, Keys).

keys_as_set(Map) = KeySet :-
    keys_as_set(Map, KeySet).

keys_as_set(Map, KeySet) :-
    map.sorted_keys(Map, Keys),
    set.sorted_list_to_set(Keys, KeySet).

values(M) = Vs :-
    map.values(M, Vs).

values(Map, KeyList) :-
    tree234.values(Map, KeyList).

keys_and_values(Map, KeyList, ValueList) :-
    tree234.keys_and_values(Map, KeyList, ValueList).

sorted_keys_match(Map, List) :-
    tree234.sorted_keys_match(Map, List).

%---------------------------------------------------------------------------%

transform_value(P, K, !Map) :-
    tree234.transform_value(P, K, !Map).

det_transform_value(F, K, !.Map) = !:Map :-
    map.det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), K,
        !Map).

det_transform_value(P, K, !Map) :-
    ( if map.transform_value(P, K, !.Map, NewMap) then
        !:Map = NewMap
    else
        report_lookup_error("map.det_transform_value: key not found", K)
    ).

%---------------------------------------------------------------------------%

from_assoc_list(AL) = M :-
    map.from_assoc_list(AL, M).

from_assoc_list(L, M) :-
    tree234.assoc_list_to_tree234(L, M).

from_sorted_assoc_list(AL) = M :-
    map.from_sorted_assoc_list(AL, M).

from_sorted_assoc_list(L, M) :-
    tree234.from_sorted_assoc_list(L, M).

from_rev_sorted_assoc_list(AL) = M :-
    map.from_rev_sorted_assoc_list(AL, M).

from_rev_sorted_assoc_list(L, M) :-
    tree234.from_rev_sorted_assoc_list(L, M).

from_corresponding_lists(Ks, Vs) = M :-
    map.from_corresponding_lists(Ks, Vs, M).

from_corresponding_lists(Keys, Values, Map) :-
    assoc_list.from_corresponding_lists(Keys, Values, AssocList),
    tree234.assoc_list_to_tree234(AssocList, Map).

%---------------------------------------------------------------------------%

to_assoc_list(M) = AL :-
    map.to_assoc_list(M, AL).

to_assoc_list(M, L) :-
    tree234.tree234_to_assoc_list(M, L).

to_sorted_assoc_list(M) = AL :-
    map.to_sorted_assoc_list(M, AL).

to_sorted_assoc_list(M, L) :-
    % Guaranteed to yield sorted lists.
    tree234.tree234_to_assoc_list(M, L).

%---------------------------------------------------------------------------%

reverse_map(Map) = RevMap :-
    map.foldl(map.reverse_map_2, Map, map.init, RevMap).

:- pred reverse_map_2(K::in, V::in,
    map(V, set(K))::in, map(V, set(K))::out) is det.

reverse_map_2(Key, Value, !RevMap) :-
    ( if map.search(!.RevMap, Value, Keys0) then
        set.insert(Key, Keys0, Keys),
        map.det_update(Value, Keys, !RevMap)
    else
        map.det_insert(Value, set.make_singleton_set(Key), !RevMap)
    ).

%---------------------------------------------------------------------------%

select(FullMap, KeySet) = SelectMap :-
    map.select(FullMap, KeySet, SelectMap).

select(FullMap, KeySet, SelectMap) :-
    set.to_sorted_list(KeySet, Keys),
    select_sorted_list(FullMap, Keys, SelectMap).

select_sorted_list(FullMap, Keys) = SelectMap :-
    map.select_sorted_list(FullMap, Keys, SelectMap).

select_sorted_list(FullMap, Keys, SelectMap) :-
    map.select_loop(Keys, FullMap, [], RevSelectAL),
    map.from_rev_sorted_assoc_list(RevSelectAL, SelectMap).

:- pred select_loop(list(K)::in, map(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.
:- pragma type_spec(pred(map.select_loop/4), K = var(_)).

select_loop([], _FullMap, !RevSelectAL).
select_loop([K | Ks], FullMap, !RevSelectAL) :-
    ( if map.search(FullMap, K, V) then
        !:RevSelectAL = [K - V | !.RevSelectAL]
    else
        true
    ),
    map.select_loop(Ks, FullMap, !RevSelectAL).

%---------------------%

select_unselect(FullMap, KeySet, SelectMap, UnselectMap) :-
    set.to_sorted_list(KeySet, Keys),
    select_unselect_sorted_list(FullMap, Keys, SelectMap, UnselectMap).

select_unselect_sorted_list(FullMap, Keys, SelectMap, UnselectMap) :-
    map.to_assoc_list(FullMap, FullAL),
    map.select_unselect_loop(FullAL, Keys, [], RevSelectAL, [], RevUnselectAL),
    map.from_rev_sorted_assoc_list(RevSelectAL, SelectMap),
    map.from_rev_sorted_assoc_list(RevUnselectAL, UnselectMap).

:- pred select_unselect_loop(assoc_list(K, V)::in, list(K)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

select_unselect_loop([], _, !RevSelectAL, !RevUnselectAL).
select_unselect_loop(FullAL @ [FullK - FullV | TailFullAL], KeysAL,
        !RevSelectAL, !RevUnselectAL) :-
    (
        KeysAL = [],
        % There are no keys left in the key set.
        % Move FullK - FullV (and every pair after them) to the unselect list.
        NextFullAL = TailFullAL,
        NextKeysAL = KeysAL,
        !:RevUnselectAL = [FullK - FullV | !.RevUnselectAL]
    ;
        KeysAL = [KeyK | TailKeysAL],
        compare(Result, KeyK, FullK),
        (
            Result = (<),
            % KeyK does not occur in the full map. Consume it.
            NextFullAL = FullAL,
            NextKeysAL = TailKeysAL
        ;
            Result = (=),
            % KeyK does occur in the full map. Consume both it
            % and the matching FullK - FullV pair, and move the pair
            % to the select list.
            NextFullAL = TailFullAL,
            NextKeysAL = TailKeysAL,
            !:RevSelectAL = [FullK - FullV | !.RevSelectAL]
        ;
            Result = (>),
            % We don't yet know whether KeyK occurs in the full map,
            % but we know that FullK is not in the key set.
            % Move the FullK - FullV pair to the unselect list.
            NextFullAL = TailFullAL,
            NextKeysAL = KeysAL,
            !:RevUnselectAL = [FullK - FullV | !.RevUnselectAL]
        )
    ),
    map.select_unselect_loop(NextFullAL, NextKeysAL,
        !RevSelectAL, !RevUnselectAL).

%---------------------------------------------------------------------------%

apply_to_list(Ks, M) = Vs :-
    map.apply_to_list(Ks, M, Vs).

apply_to_list([], _, []).
apply_to_list([K | Ks], Map, [V | Vs]) :-
    map.lookup(Map, K, V),
    map.apply_to_list(Ks, Map, Vs).

%---------------------------------------------------------------------------%

merge(MapA, MapB) = MergedMap :-
    map.merge(MapA, MapB, MergedMap).

merge(MapA, MapB, MergedMap) :-
    % You may wish to compare this to old_merge below.
    map.to_assoc_list(MapB, MapBList),
    map.det_insert_from_assoc_list(MapBList, MapA, MergedMap).

overlay(MapA, MapB) = OverlayMap :-
    map.overlay(MapA, MapB, OverlayMap).

overlay(MapA, MapB, OverlayMap) :-
    map.to_assoc_list(MapB, AssocListB),
    map.overlay_2(AssocListB, MapA, OverlayMap).

:- pred overlay_2(assoc_list(K, V)::in, map(K, V)::in, map(K, V)::out) is det.
:- pragma type_spec(pred(map.overlay_2/3), K = var(_)).

overlay_2([], !Map).
overlay_2([K - V | AssocList], !Map) :-
    map.set(K, V, !Map),
    map.overlay_2(AssocList, !Map).

overlay_large_map(MapA, MapB) = OverlayMap :-
    map.overlay_large_map(MapA, MapB, OverlayMap).

overlay_large_map(MapA, MapB, OverlayMap) :-
    map.to_assoc_list(MapA, AssocListA),
    map.overlay_large_map_2(AssocListA, MapB, OverlayMap).

:- pred overlay_large_map_2(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.
:- pragma type_spec(pred(map.overlay_large_map_2/3), K = var(_)).

overlay_large_map_2([], Map, Map).
overlay_large_map_2([K - V | AssocList], Map0, Map) :-
    ( if map.insert(K, V, Map0, Map1) then
        Map2 = Map1
    else
        Map2 = Map0
    ),
    map.overlay_large_map_2(AssocList, Map2, Map).

%---------------------%

common_subset(MapA, MapB) = CommonMap :-
    map.to_sorted_assoc_list(MapA, AssocListA),
    map.to_sorted_assoc_list(MapB, AssocListB),
    map.common_subset_loop(AssocListA, AssocListB, [], RevCommonAssocList),
    map.from_rev_sorted_assoc_list(RevCommonAssocList, CommonMap).

:- pred common_subset_loop(assoc_list(K, V)::in, assoc_list(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

common_subset_loop(ListA, ListB, !RevCommonList) :-
    (
        ListA = [],
        ListB = []
    ;
        ListA = [_ | _],
        ListB = []
    ;
        ListA = [],
        ListB = [_ | _]
    ;
        ListA = [KeyA - ValueA | TailA],
        ListB = [KeyB - ValueB | TailB],
        compare(R, KeyA, KeyB),
        (
            R = (=),
            ( if ValueA = ValueB then
                !:RevCommonList = [KeyA - ValueA | !.RevCommonList]
            else
                true
            ),
            map.common_subset_loop(TailA, TailB, !RevCommonList)
        ;
            R = (<),
            % KeyA has no match in ListB.
            map.common_subset_loop(TailA, ListB, !RevCommonList)
        ;
            R = (>),
            % KeyB has no match in ListA.
            map.common_subset_loop(ListA, TailB, !RevCommonList)
        )
    ).

%---------------------%

intersect(F, MapA, MapB) = IntersectMap :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.intersect(P, MapA, MapB, IntersectMap).

intersect(CommonPred, MapA, MapB, Common) :-
    map.to_sorted_assoc_list(MapA, AssocListA),
    map.to_sorted_assoc_list(MapB, AssocListB),
    map.intersect_loop(AssocListA, AssocListB, CommonPred,
        [], RevCommonAssocList),
    map.from_rev_sorted_assoc_list(RevCommonAssocList, Common).

:- pred intersect_loop(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    assoc_list(K, V), assoc_list(K, V)).
:- mode intersect_loop(in, in, in(pred(in, in, out) is semidet), in, out)
    is semidet.
:- mode intersect_loop(in, in, in(pred(in, in, out) is det), in, out)
    is det.

intersect_loop(ListA, ListB, CommonPred, !RevCommonList) :-
    (
        ListA = [],
        ListB = []
    ;
        ListA = [_ | _],
        ListB = []
    ;
        ListA = [],
        ListB = [_ | _]
    ;
        ListA = [KeyA - ValueA | TailA],
        ListB = [KeyB - ValueB | TailB],
        compare(R, KeyA, KeyB),
        (
            R = (=),
            CommonPred(ValueA, ValueB, Value),
            !:RevCommonList = [KeyA - Value | !.RevCommonList],
            map.intersect_loop(TailA, TailB, CommonPred,
                !RevCommonList)
        ;
            R = (<),
            map.intersect_loop(TailA, ListB, CommonPred, !RevCommonList)
        ;
            R = (>),
            map.intersect_loop(ListA, TailB, CommonPred, !RevCommonList)
        )
    ).

det_intersect(PF, MapA, MapB) = IntersectMap :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    map.det_intersect(P, MapA, MapB, IntersectMap).

det_intersect(CommonPred, MapA, MapB, CommonMap) :-
    ( if map.intersect(CommonPred, MapA, MapB, CommonMapPrime) then
        CommonMap = CommonMapPrime
    else
        unexpected($pred, "map.intersect failed")
    ).

intersect_list(CommonPred, HeadMap, TailMaps, Common) :-
    map.to_sorted_assoc_list(HeadMap, HeadAssocList),
    list.map(map.to_sorted_assoc_list, TailMaps, TailAssocLists),
    map.intersect_list_passes(HeadAssocList, TailAssocLists, CommonPred,
        CommonAssocList),
    map.from_sorted_assoc_list(CommonAssocList, Common).

:- pred intersect_list_passes(assoc_list(K, V), list(assoc_list(K, V)),
    pred(V, V, V), assoc_list(K, V)).
:- mode intersect_list_passes(in, in, in(pred(in, in, out) is semidet), out)
    is semidet.
:- mode intersect_list_passes(in, in, in(pred(in, in, out) is det), out)
    is det.

intersect_list_passes(HeadAssocList, TailAssocLists, CommonPred,
        CommonAssocList) :-
    (
        TailAssocLists = [],
        CommonAssocList = HeadAssocList
    ;
        TailAssocLists = [_ | _],
        map.intersect_list_pass(HeadAssocList, TailAssocLists, CommonPred,
            FirstAssocList, LaterAssocLists),
        map.intersect_list_passes(FirstAssocList, LaterAssocLists, CommonPred,
            CommonAssocList)
    ).

    % If the list [HeadAssocList | TailAssocLists] has 2k sorted association
    % lists (representing 2k maps), then reduce these to k sorted association
    % lists by intersecting assoclist 2i with assoc list 2i+1 for all i
    % in 0..(k-1). If it has 2k+1 sorted association lists, intersect
    % the first 2k as above, and add the last to the end of the list as is,
    % without intersecting it with anything.
    %
    % If the input has N assoc lists, the output will have ceil(N/2) assoc
    % lists. If invoked with two or more lists, the output will always have
    % fewer assoc lists than the input. This will always be the case, since
    % our caller does not call us when N<2.
    %
:- pred intersect_list_pass(assoc_list(K, V), list(assoc_list(K, V)),
    pred(V, V, V), assoc_list(K, V), list(assoc_list(K, V))).
:- mode intersect_list_pass(in, in, in(pred(in, in, out) is semidet), out, out)
    is semidet.
:- mode intersect_list_pass(in, in, in(pred(in, in, out) is det), out, out)
    is det.

intersect_list_pass(HeadAssocList, TailAssocLists, CommonPred,
        FirstAssocList, LaterAssocLists) :-
    (
        TailAssocLists = [],
        FirstAssocList = HeadAssocList,
        LaterAssocLists = []
    ;
        TailAssocLists = [HeadTailAssocList | TailTailAssocLists],
        map.intersect_loop(HeadAssocList, HeadTailAssocList, CommonPred,
            [], RevFirstAssocList),
        list.reverse(RevFirstAssocList, FirstAssocList),
        (
            TailTailAssocLists = [],
            LaterAssocLists = []
        ;
            TailTailAssocLists =
                [HeadTailTailAssocList | TailTailTailAssocLists],
            map.intersect_list_pass(HeadTailTailAssocList,
                TailTailTailAssocLists, CommonPred,
                HeadLaterAssocList, TailLaterAssocLists),
            LaterAssocLists = [HeadLaterAssocList | TailLaterAssocLists]
        )
    ).

%---------------------%

union(F, MapA, MapB) = UnionMap :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.union(P, MapA, MapB, UnionMap).

union(CommonPred, MapA, MapB, UnionMap) :-
    map.to_sorted_assoc_list(MapA, AssocListA),
    map.to_sorted_assoc_list(MapB, AssocListB),
    map.union_loop(AssocListA, AssocListB, CommonPred, [], RevUnionAssocList),
    map.from_rev_sorted_assoc_list(RevUnionAssocList, UnionMap).

    % The real intended modes of this predicate are the last two.
    % The first four modes are just specialized versions for use by
    % recursive calls after it has been determined that one or other input
    % list has run out of elements. These specialized versions don't do
    % redundant tests to see whether the known-empty list is empty or not.
    %
:- pred union_loop(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    assoc_list(K, V), assoc_list(K, V)).
:- mode union_loop(in(bound([])), in, in(pred(in, in, out) is semidet),
    in, out) is semidet.
:- mode union_loop(in(bound([])), in, in(pred(in, in, out) is det), in, out)
    is det.
:- mode union_loop(in, in(bound([])), in(pred(in, in, out) is semidet),
    in, out) is semidet.
:- mode union_loop(in, in(bound([])), in(pred(in, in, out) is det), in, out)
    is det.
:- mode union_loop(in, in, in(pred(in, in, out) is semidet), in, out)
    is semidet.
:- mode union_loop(in, in, in(pred(in, in, out) is det), in, out)
    is det.

union_loop(AssocList1, AssocList2, CommonPred, !RevCommonAssocList) :-
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

det_union(F, MapA, MapB) = UnionMap :-
    P = (pred(X::in, Y::in, Z::out) is semidet :- Z = F(X, Y) ),
    map.det_union(P, MapA, MapB, UnionMap).

det_union(CommonPred, MapA, MapB, UnionMap) :-
    ( if map.union(CommonPred, MapA, MapB, UnionMapPrime) then
        UnionMap = UnionMapPrime
    else
        unexpected($pred, "map.union failed")
    ).

union_list(CommonPred, HeadMap, TailMaps, Common) :-
    map.to_sorted_assoc_list(HeadMap, HeadAssocList),
    list.map(map.to_sorted_assoc_list, TailMaps, TailAssocLists),
    map.union_list_passes(HeadAssocList, TailAssocLists, CommonPred,
        CommonAssocList),
    map.from_sorted_assoc_list(CommonAssocList, Common).

:- pred union_list_passes(assoc_list(K, V), list(assoc_list(K, V)),
    pred(V, V, V), assoc_list(K, V)).
:- mode union_list_passes(in, in, in(pred(in, in, out) is semidet), out)
    is semidet.
:- mode union_list_passes(in, in, in(pred(in, in, out) is det), out)
    is det.

union_list_passes(HeadAssocList, TailAssocLists, CommonPred,
        CommonAssocList) :-
    (
        TailAssocLists = [],
        CommonAssocList = HeadAssocList
    ;
        TailAssocLists = [_ | _],
        map.union_list_pass(HeadAssocList, TailAssocLists, CommonPred,
            FirstAssocList, LaterAssocLists),
        map.union_list_passes(FirstAssocList, LaterAssocLists, CommonPred,
            CommonAssocList)
    ).

    % This predicate works on the same principle as map.intersect_list_pass.
    % See the documentation of that predicate.
    %
:- pred union_list_pass(assoc_list(K, V), list(assoc_list(K, V)),
    pred(V, V, V), assoc_list(K, V), list(assoc_list(K, V))).
:- mode union_list_pass(in, in, in(pred(in, in, out) is semidet), out, out)
    is semidet.
:- mode union_list_pass(in, in, in(pred(in, in, out) is det), out, out)
    is det.

union_list_pass(HeadAssocList, TailAssocLists, CommonPred,
        FirstAssocList, LaterAssocLists) :-
    (
        TailAssocLists = [],
        FirstAssocList = HeadAssocList,
        LaterAssocLists = []
    ;
        TailAssocLists = [HeadTailAssocList | TailTailAssocLists],
        map.union_loop(HeadAssocList, HeadTailAssocList, CommonPred,
            [], RevFirstAssocList),
        list.reverse(RevFirstAssocList, FirstAssocList),
        (
            TailTailAssocLists = [],
            LaterAssocLists = []
        ;
            TailTailAssocLists =
                [HeadTailTailAssocList | TailTailTailAssocLists],
            map.union_list_pass(HeadTailTailAssocList, TailTailTailAssocLists,
                CommonPred, HeadLaterAssocList, TailLaterAssocLists),
            LaterAssocLists = [HeadLaterAssocList | TailLaterAssocLists]
        )
    ).

%---------------------------------------------------------------------------%

count(M) = N :-
    map.count(M, N).

count(Map, Count) :-
    tree234.count(Map, Count).

%---------------------------------------------------------------------------%

equal(MapA, MapB) :-
    tree234.equal(MapA, MapB).

%---------------------------------------------------------------------------%

optimize(M0) = M :-
    map.optimize(M0, M).

optimize(Map, Map).

%---------------------------------------------------------------------------%

foldl(F, M, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    map.foldl(P, M, A, B).

foldl(Pred, Map, !A) :-
    tree234.foldl(Pred, Map, !A).

foldl2(Pred, Map, !A, !B) :-
    tree234.foldl2(Pred, Map, !A, !B).

foldl3(Pred, Map, !A, !B, !C) :-
    tree234.foldl3(Pred, Map, !A, !B, !C).

foldl4(Pred, Map, !A, !B, !C, !D) :-
    tree234.foldl4(Pred, Map, !A, !B, !C, !D).

foldl5(Pred, Map, !A, !B, !C, !D, !E) :-
    tree234.foldl5(Pred, Map, !A, !B, !C, !D, !E).

foldl6(Pred, Map, !A, !B, !C, !D, !E, !F) :-
    tree234.foldl6(Pred, Map, !A, !B, !C, !D, !E, !F).

%---------------------%

foldl_values(Pred, Map, !A) :-
    tree234.foldl_values(Pred, Map, !A).

foldl2_values(Pred, Map, !A, !B) :-
    tree234.foldl2_values(Pred, Map, !A, !B).

foldl3_values(Pred, Map, !A, !B, !C) :-
    tree234.foldl3_values(Pred, Map, !A, !B, !C).

foldl4_values(Pred, Map, !A, !B, !C, !D) :-
    tree234.foldl4_values(Pred, Map, !A, !B, !C, !D).

foldl5_values(Pred, Map, !A, !B, !C, !D, !E) :-
    tree234.foldl5_values(Pred, Map, !A, !B, !C, !D, !E).

foldl6_values(Pred, Map, !A, !B, !C, !D, !E, !F) :-
    tree234.foldl6_values(Pred, Map, !A, !B, !C, !D, !E, !F).

%---------------------%

foldr(F, M, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    map.foldr(P, M, A, B).

foldr(Pred, Map, !A) :-
    tree234.foldr(Pred, Map, !A).

foldr2(Pred, Map, !A, !B) :-
    tree234.foldr2(Pred, Map, !A, !B).

foldr3(Pred, Map, !A, !B, !C) :-
    tree234.foldr3(Pred, Map, !A, !B, !C).

foldr4(Pred, Map, !A, !B, !C, !D) :-
    tree234.foldr4(Pred, Map, !A, !B, !C, !D).

foldr5(Pred, Map, !A, !B, !C, !D, !E) :-
    tree234.foldr5(Pred, Map, !A, !B, !C, !D, !E).

foldr6(Pred, Map, !A, !B, !C, !D, !E, !F) :-
    tree234.foldr6(Pred, Map, !A, !B, !C, !D, !E, !F).

%---------------------%

map_values(F, M0) = M :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.map_values(P, M0, M).

map_values(Pred, Map0, Map) :-
    tree234.map_values(Pred, Map0, Map).

map_values_only(F, M0) = M :-
    P = (pred(Y::in, Z::out) is det :- Z = F(Y) ),
    map.map_values_only(P, M0, M).

map_values_only(Pred, Map0, Map) :-
    tree234.map_values_only(Pred, Map0, Map).

filter_map_values(Pred, Map0, Map) :-
    tree234.filter_map_values(Pred, Map0, Map).

filter_map_values_only(Pred, Map0, Map) :-
    tree234.filter_map_values_only(Pred, Map0, Map).

%---------------------%

map_foldl(Pred, !Map, !AccA) :-
    tree234.map_foldl(Pred, !Map, !AccA).

map_foldl2(Pred, !Map, !AccA, !AccB) :-
    tree234.map_foldl2(Pred, !Map, !AccA, !AccB).

map_foldl3(Pred, !Map, !AccA, !AccB, !AccC) :-
    tree234.map_foldl3(Pred, !Map, !AccA, !AccB, !AccC).

map_foldl4(Pred, !Map, !AccA, !AccB, !AccC, !AccD) :-
    tree234.map_foldl4(Pred, !Map, !AccA, !AccB, !AccC, !AccD).

%---------------------%

map_values_foldl(Pred, !Map, !AccA) :-
    tree234.map_values_foldl(Pred, !Map, !AccA).

map_values_foldl2(Pred, !Map, !AccA, !AccB) :-
    tree234.map_values_foldl2(Pred, !Map, !AccA, !AccB).

map_values_foldl3(Pred, !Map, !AccA, !AccB, !AccC) :-
    tree234.map_values_foldl3(Pred, !Map, !AccA, !AccB, !AccC).

%---------------------------------------------------------------------------%

old_merge(MapA, MapB) = MergedMap :-
    map.old_merge(MapA, MapB, MergedMap).

old_merge(MapA, MapB, MergedMap) :-
    map.to_assoc_list(MapA, ListA),
    map.to_assoc_list(MapB, ListB),
    list.merge(ListA, ListB, MergedList),
    % MergedList may be sorted, but it may contain duplicates.
    map.from_assoc_list(MergedList, MergedMap).

%---------------------------------------------------------------------------%
:- end_module map.
%---------------------------------------------------------------------------%
