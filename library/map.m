%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
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
:- pred map.init(map(_, _)::uo) is det.
:- func map.init = (map(K, V)::uo) is det.

    % Check whether a map is empty.
    %
:- pred map.is_empty(map(_, _)::in) is semidet.

    % Check whether map contains key
    %
:- pred map.contains(map(K, _V)::in, K::in) is semidet.

:- pred map.member(map(K, V)::in, K::out, V::out) is nondet.

    % Search map for key.
    %
:- pred map.search(map(K, V)::in, K::in, V::out) is semidet.
:- func map.search(map(K, V), K) = V is semidet.

    % Search map for key, but abort if search fails.
    %
:- pred map.lookup(map(K, V)::in, K::in, V::out) is det.
:- func map.lookup(map(K, V), K) = V.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred map.lower_bound_search(map(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Aborts if there is no key with the given or lower value.
    %
:- pred map.lower_bound_lookup(map(K, V)::in, K::in, K::out, V::out) is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred map.upper_bound_search(map(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Aborts if there is no key with the given or higher value.
    %
:- pred map.upper_bound_lookup(map(K, V)::in, K::in, K::out, V::out) is det.

    % Return the largest key in the map, if there is one.
    %
:- func map.max_key(map(K,V)) = K is semidet.

    % Return the smallest key in the map, if there is one.
    %
:- func map.min_key(map(K,V)) = K is semidet.

    % Search map for data.
    %
:- pred map.inverse_search(map(K, V)::in, V::in, K::out) is nondet.

    % Insert a new key and corresponding value into a map.
    % Fail if the key already exists.
    %
:- pred map.insert(map(K, V)::in, K::in, V::in, map(K, V)::out) is semidet.
:- func map.insert(map(K, V), K, V) = map(K, V) is semidet.

    % Insert a new key and corresponding value into a map.
    % Abort if the key already exists.
    %
:- pred map.det_insert(map(K, V)::in, K::in, V::in, map(K, V)::out) is det.
:- func map.det_insert(map(K, V), K, V) = map(K, V).

    % Apply map.det_insert to key - value pairs from corresponding lists.
    %
:- pred map.det_insert_from_corresponding_lists(map(K, V)::in, list(K)::in,
    list(V)::in, map(K, V)::out) is det.

:- func map.det_insert_from_corresponding_lists(map(K, V), list(K), list(V))
    = map(K, V).

    % Apply map.det_insert to key - value pairs from the assoc_lists.
    %
:- pred map.det_insert_from_assoc_list(map(K, V)::in, assoc_list(K, V)::in,
    map(K, V)::out) is det.
:- func map.det_insert_from_assoc_list(map(K, V), assoc_list(K, V))
    = map(K, V).

    % Apply map.set to key - value pairs from corresponding lists.
    %
:- pred map.set_from_corresponding_lists(map(K, V)::in, list(K)::in,
    list(V)::in, map(K, V)::out) is det.
:- func map.set_from_corresponding_lists(map(K, V), list(K), list(V))
    = map(K, V).

:- pred map.set_from_assoc_list(map(K, V)::in, assoc_list(K, V)::in,
    map(K, V)::out) is det.
:- func map.set_from_assoc_list(map(K, V), assoc_list(K, V)) = map(K, V).

    % Update the value corresponding to a given key
    % Fail if the key doesn't already exist.
    %
:- pred map.update(map(K, V)::in, K::in, V::in, map(K, V)::out) is semidet.
:- func map.update(map(K, V), K, V) = map(K, V) is semidet.

    % Update the value corresponding to a given key
    % Abort if the key doesn't already exist.
    %
:- pred map.det_update(map(K, V)::in, K::in, V::in, map(K, V)::out) is det.
:- func map.det_update(map(K, V), K, V) = map(K, V).

    % Update the value at the given key by applying the supplied
    % transformation to it.  Fails if the key is not found.  This is faster
    % than first searching for the value and then updating it.
    %
:- pred map.transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    map(K, V)::in, map(K, V)::out) is semidet.

    % Same as transform_value/4, but aborts instead of failing if the
    % key is not found.
    %
:- pred map.det_transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    map(K, V)::in, map(K, V)::out) is det.
:- func map.det_transform_value(func(V) = V, K, map(K, V)) = map(K, V).

    % Update value if the key is already present, otherwise
    % insert new key and value.
    %
:- func map.set(map(K, V), K, V) = map(K, V).
:- pred map.set(map(K, V)::in, K::in, V::in, map(K, V)::out) is det.

    % Given a map, return a list of all the keys in the map.
    %
:- func map.keys(map(K, _V)) = list(K).
:- pred map.keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the keys in the map,
    % in sorted order.
    %
:- func map.sorted_keys(map(K, _V)) = list(K).
:- pred map.sorted_keys(map(K, _V)::in, list(K)::out) is det.

    % Given a map, return a list of all the data values in the map.
    %
:- func map.values(map(_K, V)) = list(V).
:- pred map.values(map(_K, V)::in, list(V)::out) is det.

    % Convert a map to an association list.
    %
:- func map.to_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred map.to_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert a map to an association list which is sorted on the keys.
    %
:- func map.to_sorted_assoc_list(map(K, V)) = assoc_list(K, V).
:- pred map.to_sorted_assoc_list(map(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert an association list to a map.
    %
:- func map.from_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred map.from_assoc_list(assoc_list(K, V)::in, map(K, V)::out) is det.

    % Convert a sorted association list to a map.
    %
:- func map.from_sorted_assoc_list(assoc_list(K, V)) = map(K, V).
:- pred map.from_sorted_assoc_list(assoc_list(K, V)::in, map(K, V)::out)
    is det.

    % Delete a key-value pair from a map.
    % If the key is not present, leave the map unchanged.
    %
:- func map.delete(map(K, V), K) = map(K, V).
:- pred map.delete(map(K, V)::in, K::in, map(K, V)::out) is det.

    % Apply map.delete/3 to a list of keys.
    %
:- func map.delete_list(map(K, V), list(K)) = map(K, V).
:- pred map.delete_list(map(K, V)::in, list(K)::in, map(K, V)::out) is det.

    % Delete a key-value pair from a map and return the value.
    % Fail if the key is not present.
    %
:- pred map.remove(map(K, V)::in, K::in, V::out, map(K, V)::out) is semidet.

    % Delete a key-value pair from a map and return the value.
    % Abort if the key is not present.
    %
:- pred map.det_remove(map(K, V)::in, K::in, V::out, map(K, V)::out) is det.

    % Count the number of elements in the map.
    %
:- func map.count(map(K, V)) = int.
:- pred map.count(map(K, V)::in, int::out) is det.

    % Convert a pair of lists (which must be of the same length) to a map.
    %
:- func map.from_corresponding_lists(list(K), list(V)) = map(K, V).
:- pred map.from_corresponding_lists(list(K)::in, list(V)::in, map(K, V)::out)
    is det.

    % Merge the contents of the two maps.
    % Throws an exception if both sets of keys are not disjoint.
    %
:- func map.merge(map(K, V), map(K, V)) = map(K, V).
:- pred map.merge(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % For map.overlay(MapA, MapB, Map), if MapA and MapB both contain the
    % same key, then Map will map that key to the value from MapB.
    % In other words, MapB takes precedence over MapA.
    %
:- func map.overlay(map(K, V), map(K, V)) = map(K, V).
:- pred map.overlay(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

    % map.overlay_large_map(MapA, MapB, Map) performs the same task as
    % map.overlay(MapA, MapB, Map). However, while map.overlay takes time
    % proportional to the size of MapB, map.overlay_large_map takes time
    % proportional to the size of MapA. In other words, it preferable when
    % MapB is a large map.
    %
:- func map.overlay_large_map(map(K, V), map(K, V)) = map(K, V).
:- pred map.overlay_large_map(map(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.

    % map.select takes a map and a set of keys and returns a map
    % containing the keys in the set and their corresponding values.
    %
:- func map.select(map(K, V), set(K)) = map(K, V).
:- pred map.select(map(K, V)::in, set(K)::in, map(K, V)::out) is det.

    % Given a list of keys, produce a list of their corresponding
    % values in a specified map.
    %
:- func map.apply_to_list(list(K), map(K, V)) = list(V).
:- pred map.apply_to_list(list(K)::in, map(K, V)::in, list(V)::out) is det.

    % Declaratively, a NOP.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the map in the expectation
    % of a number of lookups but few or no modifications.
    %
:- func map.optimize(map(K, V)) = map(K, V).
:- pred map.optimize(map(K, V)::in, map(K, V)::out) is det.

    % Remove the smallest item from the map, fail if the map is empty.
    %
:- pred map.remove_smallest(map(K, V)::in, K::out, V::out, map(K, V)::out)
    is semidet.

    % Perform an inorder traversal of the map, applying
    % an accumulator predicate for each key-value pair.
    %
:- func map.foldl(func(K, V, A) = A, map(K, V), A) = A.
:- pred map.foldl(pred(K, V, A, A), map(K, V), A, A).
:- mode map.foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode map.foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode map.foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with two accumulators for each key-value pair.
    % (Although no more expressive than map.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred map.foldl2(pred(K, V, A, A, B, B), map(K, V), A, A, B, B).
:- mode map.foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode map.foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode map.foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode map.foldl2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with three accumulators for each key-value pair.
    % (Although no more expressive than map.foldl, this is often
    % a more convenient format, and a little more efficient).
    %
:- pred map.foldl3(pred(K, V, A, A, B, B, C, C), map(K, V), A, A, B, B, C, C).
:- mode map.foldl3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode map.foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode map.foldl3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode map.foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode map.foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.

    % Perform an inorder traversal of the map, applying an accumulator
    % predicate with four accumulators for each key-value pair.
    % (Although no more expressive than map.foldl, this is often
    % a more convenient format, and a little more efficient).
:- pred map.foldl4(pred(K, V, A, A, B, B, C, C, D, D), map(K, V),
    A, A, B, B, C, C, D, D).
:- mode map.foldl4(pred(in, in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode map.foldl4(pred(in, in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode map.foldl4(pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode map.foldl4(pred(in, in, in, out, in, out, di, uo, di, uo) is det,
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode map.foldl4(pred(in, in, in, out, di, uo, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode map.foldl4(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo, di, uo) is det.

    % Apply a transformation predicate to all the values in a map.
    %
:- func map.map_values(func(K, V) = W, map(K, V)) = map(K, W).
:- pred map.map_values(pred(K, V, W), map(K, V), map(K, W)).
:- mode map.map_values(pred(in, in, out) is det, in, out) is det.
:- mode map.map_values(pred(in, in, out) is semidet, in, out) is semidet.

    % Apply a transformation predicate to all the values in a map,
    % while continuously updating an accumulator.
    %
:- pred map.map_foldl(pred(K, V, W, A, A), map(K, V), map(K, W), A, A).
:- mode map.map_foldl(pred(in, in, out, di, uo) is det, in, out, di, uo)
    is det.
:- mode map.map_foldl(pred(in, in, out, in, out) is det, in, out, in, out)
    is det.
:- mode map.map_foldl(pred(in, in, out, in, out) is semidet, in, out, in, out)
    is semidet.

    % As map.map_foldl, but with two accumulators.
    %
:- pred map.map_foldl2(pred(K, V, W, A, A, B, B), map(K, V), map(K, W),
    A, A, B, B).
:- mode map.map_foldl2(pred(in, in, out, di, uo, di, uo) is det,
    in, out, di, uo, di, uo) is det.
:- mode map.map_foldl2(pred(in, in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map.map_foldl2(pred(in, in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map.map_foldl2(pred(in, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.

    % Given two maps M1 and M2, create a third map M3 that has only the
    % keys that occur in both M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- pred map.intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map.intersect(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode map.intersect(pred(in, in, out) is det, in, in, out) is det.

:- func map.intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).

    % Calls map.intersect. Aborts if map.intersect fails.
    %
:- pred map.det_intersect(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map.det_intersect(pred(in, in, out) is semidet, in, in, out) is det.

:- func map.det_intersect(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode map.det_intersect(func(in, in) = out is semidet, in, in) = out is det.

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
    % map.common_subset is very similar to map.intersect, but can succeed
    % even with an output map that does not contain an entry for a key
    % value that occurs in both input maps.
    %
:- func map.common_subset(map(K, V), map(K, V)) = map(K, V).

    % Given two maps M1 and M2, create a third map M3 that all the keys
    % that occur in either M1 and M2. For keys that occur in both M1
    % and M2, compute the value in the final map by applying the supplied
    % predicate to the values associated with the key in M1 and M2.
    % Fail if and only if this predicate fails on the values associated
    % with some common key.
    %
:- func map.union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- pred map.union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map.union(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode map.union(pred(in, in, out) is det, in, in, out) is det.

    % Calls map.union. Aborts if map.union fails.
    %
:- pred map.det_union(pred(V, V, V), map(K, V), map(K, V), map(K, V)).
:- mode map.det_union(pred(in, in, out) is semidet, in, in, out) is det.

:- func map.det_union(func(V, V) = V, map(K, V), map(K, V)) = map(K, V).
:- mode map.det_union(func(in, in) = out is semidet, in, in) = out is det.

    % Consider the original map a set of key-value pairs. This predicate
    % returns a map that maps each value to the set of keys it is paired
    % with in the original map.
    %
:- func map.reverse_map(map(K, V)) = map(V, set(K)).

    % Field selection for maps.

    % Map ^ elem(Key) = map.search(Map, Key).
    %
:- func map.elem(K, map(K, V)) = V is semidet.

    % Map ^ det_elem(Key) = map.lookup(Map, Key).
    %
:- func map.det_elem(K, map(K, V)) = V.

    % Field update for maps.

    % (Map ^ elem(Key) := Value) = map.set(Map, Key, Value).
    %
:- func 'elem :='(K, map(K, V), V) = map(K, V).

    % (Map ^ det_elem(Key) := Value) = map.det_update(Map, Key, Value).
    %
:- func 'det_elem :='(K, map(K, V), V) = map(K, V).

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

% Note to implementors:
%
% This is the old version of map.merge/3.  It is buggy in the sense that if the
% sets of keys of the input maps are not disjoint it won't throw an exception
% but will insert the key and the smallest of the two corresponding values into
% the output map.  Eventually we would like to get rid of this version but some
% of the code in the compiler currently assumes this behaviour and fixing it is
% non-trivial.

:- func map.old_merge(map(K, V), map(K, V)) = map(K, V).
:- pred map.old_merge(map(K, V)::in, map(K, V)::in, map(K, V)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

:- pragma type_spec(map.overlay/2, K = var(_)).
:- pragma type_spec(map.overlay/3, K = var(_)).

:- pragma type_spec(map.select/2, K = var(_)).
:- pragma type_spec(map.select/3, K = var(_)).

:- pragma type_spec(map.elem/2, K = int).
:- pragma type_spec(map.elem/2, K = var(_)).

:- pragma type_spec(map.det_elem/2, K = int).
:- pragma type_spec(map.det_elem/2, K = var(_)).

:- pragma type_spec('elem :='/3, K = int).
:- pragma type_spec('elem :='/3, K = var(_)).

:- pragma type_spec('det_elem :='/3, K = int).
:- pragma type_spec('det_elem :='/3, K = var(_)).

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module svmap.
:- import_module svset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

map.init(M) :-
    tree234.init(M).

map.is_empty(M) :-
    tree234.is_empty(M).

map.contains(Map, K) :-
    map.search(Map, K, _).

map.member(Map, K, V) :-
    tree234.member(Map, K, V).

map.search(Map, K, V) :-
    tree234.search(Map, K, V).

map.lookup(Map, K, V) :-
    ( tree234.search(Map, K, V1) ->
        V = V1
    ;
        report_lookup_error("map.lookup: key not found", K, V)
    ).

map.lower_bound_search(Map, SearchK, K, V) :-
    tree234.lower_bound_search(Map, SearchK, K, V).

map.lower_bound_lookup(Map, SearchK, K, V) :-
    ( tree234.lower_bound_search(Map, SearchK, K1, V1) ->
        K = K1,
        V = V1
    ;
        report_lookup_error("map.lower_bound_lookup: key not found",
            SearchK, V)
    ).

map.upper_bound_search(Map, SearchK, K, V) :-
    tree234.upper_bound_search(Map, SearchK, K, V).

map.upper_bound_lookup(Map, SearchK, K, V) :-
    ( tree234.upper_bound_search(Map, SearchK, K1, V1) ->
        K = K1,
        V = V1
    ;
        report_lookup_error("map.upper_bound_lookup: key not found",
            SearchK, V)
    ).

map.max_key(M) = tree234.max_key(M).

map.min_key(M) = tree234.min_key(M).

map.insert(Map0, K, V, Map) :-
    tree234.insert(Map0, K, V, Map).

map.det_insert(Map0, K, V, Map) :-
    ( tree234.insert(Map0, K, V, Map1) ->
        Map = Map1
    ;
        report_lookup_error("map.det_insert: key already present", K, V)
    ).

map.det_insert_from_corresponding_lists(Map0, Ks, Vs, Map) :-
    (
        Ks = [Key | Keys],
        Vs = [Value | Values]
    ->
        map.det_insert(Map0, Key, Value, Map1),
        map.det_insert_from_corresponding_lists(Map1, Keys, Values,
            Map)
    ;
        Ks = [],
        Vs = []
    ->
        Map = Map0
    ;
        error("map.det_insert_from_corresponding_lists - " ++
            "lists do not correspond")
    ).

map.det_insert_from_assoc_list(Map, [], Map).
map.det_insert_from_assoc_list(Map0, [K - V | KVs], Map) :-
    map.det_insert(Map0, K, V, Map1),
    map.det_insert_from_assoc_list(Map1, KVs, Map).

map.set_from_corresponding_lists(Map0, Ks, Vs, Map) :-
    (
        Ks = [Key | Keys],
        Vs = [Value | Values]
    ->
        map.set(Map0, Key, Value, Map1),
        map.set_from_corresponding_lists(Map1, Keys, Values, Map)
    ;
        Ks = [],
        Vs = []
    ->
        Map = Map0
    ;
        error("map.set_from_corresponding_lists - " ++
            "lists do not correspond")
    ).

map.set_from_assoc_list(Map, [], Map).
map.set_from_assoc_list(Map0, [K - V | KVs], Map) :-
    map.set(Map0, K, V, Map1),
    map.set_from_assoc_list(Map1, KVs, Map).

map.update(Map0, K, V, Map) :-
    tree234.update(Map0, K, V, Map).

map.det_update(Map0, K, V, Map) :-
    ( tree234.update(Map0, K, V, Map1) ->
        Map = Map1
    ;
        report_lookup_error("map.det_update: key not found", K, V)
    ).

map.transform_value(P, K, !Map) :-
    tree234.transform_value(P, K, !Map).

map.det_transform_value(P, K, !Map) :-
    (
        map.transform_value(P, K, !.Map, NewMap)
    ->
        !:Map = NewMap
    ;
        report_lookup_error("map.det_transform_value: key not found",
            K)
    ).

map.det_transform_value(F, K, Map0) = Map :-
    map.det_transform_value(pred(V0::in, V::out) is det :- V = F(V0), K,
        Map0, Map).

map.set(Map0, K, V, Map) :-
    tree234.set(Map0, K, V, Map).

map.keys(Map, KeyList) :-
    tree234.keys(Map, KeyList).

map.sorted_keys(Map, KeyList) :-
    % Guaranteed to yield sorted lists.
    tree234.keys(Map, KeyList).

map.values(Map, KeyList) :-
    tree234.values(Map, KeyList).

map.to_assoc_list(M, L) :-
    tree234.tree234_to_assoc_list(M, L).

map.to_sorted_assoc_list(M, L) :-
    % Guaranteed to yield sorted lists.
    tree234.tree234_to_assoc_list(M, L).

map.from_assoc_list(L, M) :-
    tree234.assoc_list_to_tree234(L, M).

map.from_sorted_assoc_list(L, M) :-
    tree234.assoc_list_to_tree234(L, M).

map.delete(Map0, Key, Map) :-
    tree234.delete(Map0, Key, Map).

map.delete_list(Map, [], Map).
map.delete_list(Map0, [Key | Keys], Map) :-
    map.delete(Map0, Key, Map1),
    map.delete_list(Map1, Keys, Map).

map.remove(Map0, Key, Value, Map) :-
    tree234.remove(Map0, Key, Value, Map).

map.det_remove(Map0, Key, Value, Map) :-
    ( tree234.remove(Map0, Key, Value1, Map1) ->
        Value = Value1,
        Map = Map1
    ;
        report_lookup_error("map.det_remove: key not found", Key, Value)
    ).

map.count(Map, Count) :-
    tree234.count(Map, Count).

%-----------------------------------------------------------------------------%

map.inverse_search(Map, V, K) :-
    map.member(Map, K, V).

%-----------------------------------------------------------------------------%

map.from_corresponding_lists(Keys, Values, Map) :-
    assoc_list.from_corresponding_lists(Keys, Values, AssocList),
    tree234.assoc_list_to_tree234(AssocList, Map).

%-----------------------------------------------------------------------------%

map.merge(M0, M1, M) :-
    map.to_assoc_list(M0, ML0),
    map.to_assoc_list(M1, ML1),
    list.merge(ML0, ML1, ML),
    M = map.det_insert_from_assoc_list(map.init, ML).

%-----------------------------------------------------------------------------%

map.old_merge(M0, M1, M) :-
    map.to_assoc_list(M0, ML0),
    map.to_assoc_list(M1, ML1),
    list.merge(ML0, ML1, ML),
    map.from_sorted_assoc_list(ML, M).

%-----------------------------------------------------------------------------%

map.optimize(Map, Map).

%-----------------------------------------------------------------------------%

map.overlay(Map0, Map1, Map) :-
    map.to_assoc_list(Map1, AssocList),
    map.overlay_2(AssocList, Map0, Map).

:- pred map.overlay_2(assoc_list(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.
:- pragma type_spec(map.overlay_2/3, K = var(_)).

map.overlay_2([], Map, Map).
map.overlay_2([K - V | AssocList], Map0, Map) :-
    map.set(Map0, K, V, Map1),
    map.overlay_2(AssocList, Map1, Map).

map.overlay_large_map(Map0, Map1, Map) :-
    map.to_assoc_list(Map0, AssocList),
    map.overlay_large_map_2(AssocList, Map1, Map).

:- pred map.overlay_large_map_2(assoc_list(K, V)::in,
    map(K, V)::in, map(K, V)::out) is det.
:- pragma type_spec(map.overlay_large_map_2/3, K = var(_)).

map.overlay_large_map_2([], Map, Map).
map.overlay_large_map_2([K - V | AssocList], Map0, Map) :-
    ( map.insert(Map0, K, V, Map1) ->
        Map2 = Map1
    ;
        Map2 = Map0
    ),
    map.overlay_large_map_2(AssocList, Map2, Map).

%-----------------------------------------------------------------------------%

map.select(Original, KeySet, NewMap) :-
    set.to_sorted_list(KeySet, KeyList),
    map.init(NewMap0),
    map.select_2(KeyList, Original, NewMap0, NewMap).

:- pred map.select_2(list(K)::in, map(K, V)::in, map(K, V)::in,
    map(K, V)::out) is det.
:- pragma type_spec(map.select_2/4, K = var(_)).

map.select_2([], _Original, New, New).
map.select_2([K|Ks], Original, New0, New) :-
    ( map.search(Original, K, V) ->
        map.set(New0, K, V, New1)
    ;
        New1 = New0
    ),
    map.select_2(Ks, Original, New1, New).

%-----------------------------------------------------------------------------%

map.apply_to_list([], _, []).
map.apply_to_list([K | Ks], Map, [V | Vs]) :-
    map.lookup(Map, K, V),
    map.apply_to_list(Ks, Map, Vs).

%-----------------------------------------------------------------------------%

map.remove_smallest(Map0, K, V, Map) :-
    tree234.remove_smallest(Map0, K, V, Map).

%-----------------------------------------------------------------------------%

map.foldl(Pred, Map, !A) :-
    tree234.foldl(Pred, Map, !A).

map.foldl2(Pred, Map, !A, !B) :-
    tree234.foldl2(Pred, Map, !A, !B).

map.foldl3(Pred, Map, !A, !B, !C) :-
    tree234.foldl3(Pred, Map, !A, !B, !C).

map.foldl4(Pred, Map, !A, !B, !C, !D) :-
    tree234.foldl4(Pred, Map, !A, !B, !C, !D).

%-----------------------------------------------------------------------------%

map.map_values(Pred, Map0, Map) :-
    tree234.map_values(Pred, Map0, Map).

map.map_foldl(Pred, !Map, !Acc) :-
    tree234.map_foldl(Pred, !Map, !Acc).

map.map_foldl2(Pred, !Map, !Acc1, !Acc2) :-
    tree234.map_foldl2(Pred, !Map, !Acc1, !Acc2).

%-----------------------------------------------------------------------------%

map.intersect(CommonPred, Map1, Map2, Common) :-
    map.to_sorted_assoc_list(Map1, AssocList1),
    map.to_sorted_assoc_list(Map2, AssocList2),
    map.init(Common0),
    map.intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred map.intersect_2(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    map(K, V), map(K, V)).
:- mode map.intersect_2(in, in, pred(in, in, out) is semidet, in, out)
    is semidet.
:- mode map.intersect_2(in, in, pred(in, in, out) is det, in, out)
    is det.

map.intersect_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
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
            CommonPred(Value1, Value2, Value),
            map.det_insert(Common0, Key1, Value, Common1),
            map.intersect_2(AssocTail1, AssocTail2, CommonPred,
                Common1, Common)
        ;
            R = (<),
            map.intersect_2(AssocTail1, AssocList2, CommonPred,
                Common0, Common)
        ;
            R = (>),
            map.intersect_2(AssocList1, AssocTail2, CommonPred,
                Common0, Common)
        )
    ).

map.det_intersect(CommonPred, Map1, Map2, Common) :-
    ( map.intersect(CommonPred, Map1, Map2, CommonPrime) ->
        Common = CommonPrime
    ;
        error("map.det_intersect: map.intersect failed")
    ).

%-----------------------------------------------------------------------------%

map.common_subset(Map1, Map2) = Common :-
    map.to_sorted_assoc_list(Map1, AssocList1),
    map.to_sorted_assoc_list(Map2, AssocList2),
    map.init(Common0),
    map.common_subset_2(AssocList1, AssocList2, Common0) = Common.

:- func map.common_subset_2(assoc_list(K, V), assoc_list(K, V),
    map(K, V)) = map(K, V).

map.common_subset_2(AssocList1, AssocList2, Common0) = Common :-
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
                map.det_insert(Common0, Key1, Value1, Common1)
            ;
                Common1 = Common0
            ),
            Common = map.common_subset_2(AssocTail1, AssocTail2,
                Common1)
        ;
            R = (<),
            Common = map.common_subset_2(AssocTail1, AssocList2,
                Common0)
        ;
            R = (>),
            Common = map.common_subset_2(AssocList1, AssocTail2,
                Common0)
        )
    ).

%-----------------------------------------------------------------------------%

map.union(CommonPred, Map1, Map2, Common) :-
    map.to_sorted_assoc_list(Map1, AssocList1),
    map.to_sorted_assoc_list(Map2, AssocList2),
    map.init(Common0),
    map.union_2(AssocList1, AssocList2, CommonPred, Common0, Common).

:- pred map.union_2(assoc_list(K, V), assoc_list(K, V), pred(V, V, V),
    map(K, V), map(K, V)).
:- mode map.union_2(in, in, pred(in, in, out) is semidet, in, out)
    is semidet.
:- mode map.union_2(in, in, pred(in, in, out) is det, in, out)
    is det.

map.union_2(AssocList1, AssocList2, CommonPred, Common0, Common) :-
    (
        AssocList1 = [],
        AssocList2 = [],
        Common = Common0
    ;
        AssocList1 = [_ | _],
        AssocList2 = [],
        map.det_insert_from_assoc_list(Common0, AssocList1, Common)
    ;
        AssocList1 = [],
        AssocList2 = [_ | _],
        map.det_insert_from_assoc_list(Common0, AssocList2, Common)
    ;
        AssocList1 = [Key1 - Value1 | AssocTail1],
        AssocList2 = [Key2 - Value2 | AssocTail2],
        compare(R, Key1, Key2),
        (
            R = (=),
            CommonPred(Value1, Value2, Value),
            map.det_insert(Common0, Key1, Value, Common1),
            map.union_2(AssocTail1, AssocTail2, CommonPred, Common1, Common)
        ;
            R = (<),
            map.det_insert(Common0, Key1, Value1, Common1),
            map.union_2(AssocTail1, AssocList2, CommonPred, Common1, Common)
        ;
            R = (>),
            map.det_insert(Common0, Key2, Value2, Common1),
            map.union_2(AssocList1, AssocTail2, CommonPred, Common1, Common)
        )
    ).

map.det_union(CommonPred, Map1, Map2, Union) :-
    ( map.union(CommonPred, Map1, Map2, UnionPrime) ->
        Union = UnionPrime
    ;
        error("map.det_union: map.union failed")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

map.init = M :-
    map.init(M).

map.search(M, K) = V :-
    map.search(M, K, V).

map.lookup(M, K) = V :-
    map.lookup(M, K, V).

map.insert(M1, K, V) = M2 :-
    map.insert(M1, K, V, M2).

map.det_insert(M1, K, V) = M2 :-
    map.det_insert(M1, K, V, M2).

map.det_insert_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    map.det_insert_from_corresponding_lists(M1, Ks, Vs, M2).

map.det_insert_from_assoc_list(M1, AL) = M2 :-
    map.det_insert_from_assoc_list(M1, AL, M2).

map.set_from_corresponding_lists(M1, Ks, Vs) = M2 :-
    map.set_from_corresponding_lists(M1, Ks, Vs, M2).

map.set_from_assoc_list(M1, AL) = M2 :-
    map.set_from_assoc_list(M1, AL, M2).

map.update(M1, K, V) = M2 :-
    map.update(M1, K, V, M2).

map.det_update(M1, K, V) = M2 :-
    map.det_update(M1, K, V, M2).

map.set(M1, K, V) = M2 :-
    map.set(M1, K, V, M2).

map.keys(M) = Ks :-
    map.keys(M, Ks).

map.sorted_keys(M) = Ks :-
    map.sorted_keys(M, Ks).

map.values(M) = Vs :-
    map.values(M, Vs).

map.to_assoc_list(M) = AL :-
    map.to_assoc_list(M, AL).

map.to_sorted_assoc_list(M) = AL :-
    map.to_sorted_assoc_list(M, AL).

map.from_assoc_list(AL) = M :-
    map.from_assoc_list(AL, M).

map.from_sorted_assoc_list(AL) = M :-
    map.from_sorted_assoc_list(AL, M).

map.delete(M1, K) = M2 :-
    map.delete(M1, K, M2).

map.delete_list(M1, Ks) = M2 :-
    map.delete_list(M1, Ks, M2).

map.count(M) = N :-
    map.count(M, N).

map.from_corresponding_lists(Ks, Vs) = M :-
    map.from_corresponding_lists(Ks, Vs, M).

map.merge(M1, M2) = M3 :-
    map.merge(M1, M2, M3).

map.old_merge(M1, M2) = M3 :-
    map.old_merge(M1, M2, M3).

map.overlay(M1, M2) = M3 :-
    map.overlay(M1, M2, M3).

map.overlay_large_map(M1, M2) = M3 :-
    map.overlay_large_map(M1, M2, M3).

map.select(M1, S) = M2 :-
    map.select(M1, S, M2).

map.apply_to_list(Ks, M) = Vs :-
    map.apply_to_list(Ks, M, Vs).

map.optimize(M1) = M2 :-
    map.optimize(M1, M2).

map.foldl(F, M, A) = B :-
    P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    map.foldl(P, M, A, B).

map.map_values(F, M1) = M2 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.map_values(P, M1, M2).

map.intersect(F, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.intersect(P, M1, M2, M3).

map.det_intersect(PF, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is semidet :- Z = PF(X, Y) ),
    map.det_intersect(P, M1, M2, M3).

map.union(F, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map.union(P, M1, M2, M3).

map.det_union(F, M1, M2) = M3 :-
    P = ( pred(X::in, Y::in, Z::out) is semidet :- Z = F(X, Y) ),
    map.det_union(P, M1, M2, M3).

map.reverse_map(Map) = RevMap :-
    map.foldl(map.reverse_map_2, Map, map.init, RevMap).

:- pred map.reverse_map_2(K::in, V::in,
    map(V, set(K))::in, map(V, set(K))::out) is det.

map.reverse_map_2(Key, Value, !RevMap) :-
    ( map.search(!.RevMap, Value, Keys0) ->
        svset.insert(Key, Keys0, Keys),
        svmap.det_update(Value, Keys, !RevMap)
    ;
        svmap.det_insert(Value, set.make_singleton_set(Key), !RevMap)
    ).

map.elem(Key, Map) = map.search(Map, Key).

map.det_elem(Key, Map) = map.lookup(Map, Key).

'elem :='(Key, Map, Value) = map.set(Map, Key, Value).

'det_elem :='(Key, Map, Value) = map.det_update(Map, Key, Value).
