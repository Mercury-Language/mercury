%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1995, 1997, 2000, 2002-2006, 2011 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: multi_map.m.
% Main author: dylan.
% Stability: medium.
%
% This file provides the 'multi_map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key, Value) pairs which allows you to look up any Value given the Key.
% A multi_map is similar, but it allows more than one Value for each Key.
%
% This is implemented almost as a special case of map.m.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module multi_map.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

:- type multi_map(K, V) == map(K, list(V)).

%---------------------------------------------------------------------------%

    % Return an empty multi_map.
    %
:- func init = multi_map(_K, _V).
:- pred init(multi_map(_K, _V)::uo) is det.

    % Check whether the multi_map is empty.
    %
:- pred is_empty(multi_map(_K, _V)::in) is semidet.

%---------------------%

    % Check whether the multi_map has an entry for the given key.
    %
:- pred contains(multi_map(K, _V)::in, K::in) is semidet.

    % Succeed once for each key-value pair in the multi_map.
    %
:- pred member(multi_map(K, V)::in, K::out, V::out) is nondet.

    % If the multi_map has an entry for the given key, return the
    % list of corresponding values.
    %
:- pred search(multi_map(K, V)::in, K::in, list(V)::out) is semidet.

    % If the multi_map has an entry for the given key,
    % succeed once for each of the corresponding values.
    %
:- pred nondet_search(multi_map(K, V)::in, K::in, V::out) is nondet.

    % If the multi_map has an entry for the given key,
    % succeed once for each of the corresponding values.
    % Otherwise, throw an exception.
    %
:- func lookup(multi_map(K, V), K) = list(V).
:- pred lookup(multi_map(K, V)::in, K::in, list(V)::out) is det.

    % If the multi_map has an entry for the given key,
    % succeed once for each of the corresponding values.
    % Otherwise, throw an exception.
    %
:- pred nondet_lookup(multi_map(K, V)::in, K::in, V::out) is nondet.

    % If the multi_map has an entry for keys with the given value,
    % succeed once for each of those keys.
    %
    % NOTE: The implementation of this predicate is necessarily inefficient,
    % and so this predicate is intended for non-performance-critical uses only.
    %
:- pred inverse_search(multi_map(K, V)::in, V::in, K::out) is nondet.

%---------------------%

    % Add the given key-value pair to the multi_map.
    % Fail if the key already exists.
    %
:- pred insert(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Add the given key-value pair to the multi_map.
    % Throw an exception if the key already exists.
    %
:- func det_insert(multi_map(K, V), K, V) = multi_map(K, V).
:- pred det_insert(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Add the given key-value pair to the multi_map.
    % Fail if the key does not already exist.
    %
:- pred update(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Add the given key-value pair to the multi_map.
    % Throw an exception if the key does not already exist.
    %
:- func det_update(multi_map(K, V), K, V) = multi_map(K, V).
:- pred det_update(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Replace the list of values corresponding to the given key.
    % Fails if the key does not already exist.
    %
:- pred replace(K::in, list(V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Replace the list of values corresponding to the given key.
    % Throws an exception if the key does not already exist.
    %
:- func det_replace(multi_map(K, V), K, list(V)) = multi_map(K, V).
:- pred det_replace(K::in, list(V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Add the given key-value pair to the multi_map.
    % (`set' is a synonym for `add'.)
    %
:- func add(multi_map(K, V), K, V) = multi_map(K, V).
:- pred add(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.
:- func set(multi_map(K, V), K, V) = multi_map(K, V).
:- pred set(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Add the given value-key pair to the multi_map.
    % (`reverse_set' is a synonym for `reverse_add'.)
    %
:- func reverse_add(multi_map(K, V), V, K) = multi_map(K, V).
:- pred reverse_add(V::in, K::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.
:- func reverse_set(multi_map(K, V), V, K) = multi_map(K, V).
:- pred reverse_set(V::in, K::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

%---------------------%

    % Delete a key and its corresponding values from a multi_map.
    % If the key is not present, leave the multi_map unchanged.
    %
:- func delete(multi_map(K, V), K) = multi_map(K, V).
:- pred delete(K::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Delete the given key-value pair from a multi_map.
    % If the key-value pair is not present, leave the multi_map unchanged.
    %
:- func delete(multi_map(K, V), K, V) = multi_map(K, V).
:- pred delete(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Delete a key from a multi_map and return the list of values
    % previously corresponding to it.
    % Fail if the key is not present.
    %
:- pred remove(K::in, list(V)::out,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Delete a key from a multi_map and return the list of values
    % previously corresponding to it.
    % Throw an exception if the key is not present.
    %
:- pred det_remove(K::in, list(V)::out,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Remove the smallest key and its corresponding values from the multi_map.
    % Fails if the multi_map is empty.
    %
:- pred remove_smallest(K::out, list(V)::out,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

%---------------------%

    % Select takes a multi_map and a set of keys and returns a multi_map
    % containing only the keys in the set, together with their corresponding
    % values.
    %
:- func select(multi_map(K, V), set(K)) = multi_map(K, V).
:- pred select(multi_map(K, V)::in, set(K)::in, multi_map(K, V)::out) is det.

%---------------------%

    % merge(MultiMapA, MultiMapB, MultiMap):
    %
    % Merge MultiMapA and MultiMapB so that
    %
    % - if a key occurs in both MultiMapA and MultiMapB, then the values
    %   corresponding to that key in MultiMap will be the concatenation
    %   of the values to that key from MultiMapA and MultiMapB; while
    % - if a key occurs in only one of MultiMapA and MultiMapB, then
    %   the values corresponding to it in that map will be carried over
    %   to MultiMap.
    %
:- func merge(multi_map(K, V), multi_map(K, V))
    = multi_map(K, V).
:- pred merge(multi_map(K, V)::in, multi_map(K, V)::in,
    multi_map(K, V)::out) is det.

%---------------------%

    % Declaratively, a no-operation.
    % Operationally, a suggestion that the implementation optimize
    % the representation of the multi_map, in the expectation that the
    % following operations will consist of searches and lookups
    % but (almost) no updates.
    %
:- func optimize(multi_map(K, V)) = multi_map(K, V).
:- pred optimize(multi_map(K, V)::in, multi_map(K, V)::out) is det.

%---------------------%

    % Convert a multi_map to an association list.
    %
:- func to_flat_assoc_list(multi_map(K, V)) = assoc_list(K, V).
:- pred to_flat_assoc_list(multi_map(K, V)::in,
    assoc_list(K, V)::out) is det.

    % Convert an association list to a multi_map.
    %
:- func from_flat_assoc_list(assoc_list(K, V)) = multi_map(K, V).
:- pred from_flat_assoc_list(assoc_list(K, V)::in,
    multi_map(K, V)::out) is det.

    % Convert a multi_map to an association list, with all the values
    % for each key in one element of the association list.
    %
:- func to_assoc_list(multi_map(K, V)) = assoc_list(K, list(V)).
:- pred to_assoc_list(multi_map(K, V)::in,
    assoc_list(K, list(V))::out) is det.

    % Convert an association list with all the values for each key
    % in one element of the list to a multi_map.
    %
:- func from_assoc_list(assoc_list(K, list(V))) = multi_map(K, V).
:- pred from_assoc_list(assoc_list(K, list(V))::in,
    multi_map(K, V)::out) is det.

    % Convert a sorted association list to a multi_map.
    %
:- func from_sorted_assoc_list(assoc_list(K, list(V)))
    = multi_map(K, V).
:- pred from_sorted_assoc_list(assoc_list(K, list(V))::in,
    multi_map(K, V)::out) is det.

    % Convert the corresponding elements of a list of keys and a
    % list of values (which must be of the same length) to a multi_map.
    % A key may occur more than once in the list of keys.
    % Throw an exception if the two lists are not the same length.
    %
:- func from_corresponding_lists(list(K), list(V))
    = multi_map(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in,
    multi_map(K, V)::out) is det.

    % Convert the corresponding elements of a list of keys and a
    % *list of lists* of values to a multi_map.
    % A key may *not* occur more than once in the list of keys.
    % Throw an exception if the two lists are not the same length,
    % or if a key does occur more than once in the list of keys.
    %
:- func from_corresponding_list_lists(list(K), list(list(V)))
    = multi_map(K, V).
:- pred from_corresponding_list_lists(list(K)::in, list(list(V))::in,
    multi_map(K, V)::out) is det.

%---------------------%

    % Given a list of keys, produce a list of their values in a
    % specified multi_map.
    %
:- func apply_to_list(list(K), multi_map(K, V)) = list(V).
:- pred apply_to_list(list(K)::in, multi_map(K, V)::in, list(V)::out) is det.

%---------------------%

    % Given a multi_map, return a list of all the keys in it.
    %
:- func keys(multi_map(K, V)) = list(K).
:- pred keys(multi_map(K, V)::in, list(K)::out) is det.

    % Given a multi_map, return a list of all the keys in it
    % in sorted order.
    %
:- func sorted_keys(multi_map(K, V)) = list(K).
:- pred sorted_keys(multi_map(K, V)::in, list(K)::out) is det.

   % Given a multi_map, return a list of all the keys in it
    % as a set
    %
:- func keys_as_set(multi_map(K, V)) = set(K).
:- pred keys_as_set(multi_map(K, V)::in, set(K)::out) is det.

    % Given a multi_map, return a list of all the values in it.
    %
:- func values(multi_map(K, V)) = list(V).
:- pred values(multi_map(K, V)::in, list(V)::out) is det.

%---------------------%

    % Count the number of keys in the multi_map.
    %
:- func count(multi_map(K, V)) = int.
:- pred count(multi_map(K, V)::in, int::out) is det.

    % Count the number of key-value pairs in the multi_map.
    %
:- func all_count(multi_map(K, V)) = int.
:- pred all_count(multi_map(K, V)::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

init = MultiMap :-
    multi_map.init(MultiMap).

init(MultiMap) :-
    map.init(MultiMap).

is_empty(MultiMap) :-
    map.is_empty(MultiMap).

%---------------------------------------------------------------------------%

contains(MultiMap, Key) :-
    map.search(MultiMap, Key, _).

member(MultiMap, Key, Value) :-
    map.member(MultiMap, Key, ValueList),
    list.member(Value, ValueList).

search(MultiMap, Key, Values) :-
    map.search(MultiMap, Key, Values).

nondet_search(MultiMap, Key, Value) :-
    map.search(MultiMap, Key, Values),
    list.member(Value, Values).

lookup(MultiMap, Key) = Value :-
    multi_map.lookup(MultiMap, Key, Value).

lookup(MultiMap, Key, Values) :-
    map.lookup(MultiMap, Key, Values).

nondet_lookup(MultiMap, Key, Value) :-
    map.search(MultiMap, Key, Values),
    list.member(Value, Values).

inverse_search(MultiMap, Value, Key) :-
    map.member(MultiMap, Key, ValueList),
    list.member(Value, ValueList).

%---------------------------------------------------------------------------%

insert(Key, Value, !MultiMap) :-
    map.insert(Key, [Value], !MultiMap).

det_insert(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.det_insert(Key, Value, !MultiMap).

det_insert(Key, Value, !MultiMap) :-
    map.det_insert(Key, [Value], !MultiMap).

update(Key, Value, !MultiMap) :-
    map.search(!.MultiMap, Key, Values0),
    Values = [Value | Values0],
    map.update(Key, Values, !MultiMap).

det_update(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.det_update(Key, Value, !MultiMap).

det_update(Key, Value, !MultiMap) :-
    ( if multi_map.update(Key, Value, !MultiMap) then
        true
    else
         report_lookup_error("multi_map.det_update: key not found", Key)
    ).

replace(Key, Value, !MultiMap) :-
    map.update(Key, Value, !MultiMap).

det_replace(!.MultiMap, Key, Values) = !:MultiMap :-
    multi_map.det_replace(Key, Values, !MultiMap).

det_replace(Key, Values, !MultiMap) :-
    map.det_update(Key, Values, !MultiMap).

add(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.add(Key, Value, !MultiMap).

add(Key, Value, !MultiMap) :-
    ( if map.search(!.MultiMap, Key, Values0) then
        Values = [Value | Values0],
        map.det_update(Key, Values, !MultiMap)
    else
        map.det_insert(Key, [Value], !MultiMap)
    ).

set(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.add(Key, Value, !MultiMap).

set(Key, Value, !MultiMap) :-
    multi_map.add(Key, Value, !MultiMap).

reverse_add(!.MultiMap, Value, Key) = !:MultiMap :-
    multi_map.add(Key, Value, !MultiMap).

reverse_add(Value, Key, !MultiMap) :-
    multi_map.add(Key, Value, !MultiMap).

reverse_set(!.MultiMap, Value, Key) = !:MultiMap :-
    multi_map.add(Key, Value, !MultiMap).

reverse_set(Value, Key, !MultiMap) :-
    multi_map.add(Key, Value, !MultiMap).

%---------------------------------------------------------------------------%

delete(!.MultiMap, Key) = !:MultiMap :-
    multi_map.delete(Key, !MultiMap).

delete(Key, !MultiMap) :-
    map.delete(Key, !MultiMap).

delete(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.delete(Key, Value, !MultiMap).

delete(Key, Value, !MultiMap) :-
    ( if
        map.search(!.MultiMap, Key, Values0),
        list.delete_all(Values0, Value, Values)
    then
        (
            Values = [],
            map.delete(Key, !MultiMap)
        ;
            Values = [_ | _],
            map.det_update(Key, Values, !MultiMap)
        )
    else
        true
    ).

remove(MultiMap0, Key, Values, MultiMap) :-
    map.remove(MultiMap0, Key, Values, MultiMap).

det_remove(MultiMap0, Key, Values, MultiMap) :-
    map.det_remove(MultiMap0, Key, Values, MultiMap).

remove_smallest(MultiMap0, Key, Values, MultiMap) :-
    map.remove_smallest(MultiMap0, Key, Values, MultiMap).

%---------------------------------------------------------------------------%

select(MultiMap0, KeySet) = MultiMap :-
    multi_map.select(MultiMap0, KeySet, MultiMap).

select(MultiMap0, KeySet, MultiMap) :-
    map.select(MultiMap0, KeySet, MultiMap).

%---------------------------------------------------------------------------%

merge(MultiMapA, MultiMapB) = MultiMap :-
    multi_map.merge(MultiMapA, MultiMapB, MultiMap).

merge(M0, M1, M) :-
    multi_map.to_assoc_list(M0, ML0),
    multi_map.to_assoc_list(M1, ML1),
    multi_map.assoc_list_merge(ML0, ML1, ML),
    multi_map.from_sorted_assoc_list(ML, M).

:- pred assoc_list_merge(assoc_list(K, list(V))::in,
    assoc_list(K, list(V))::in, assoc_list(K, list(V))::out) is det.

assoc_list_merge(ListA, ListB, List) :-
    (
        ListA = [],
        List = ListB
    ;
        ListA = [HeadA | TailA],
        (
            ListB = [],
            List = ListA
        ;
            ListB = [HeadB | TailB],
            HeadA = KeyA - ValuesA,
            HeadB = KeyB - ValuesB,
            compare(Res, KeyA, KeyB),
            (
                Res = (<),
                Key = KeyA,
                Values = ValuesA,
                multi_map.assoc_list_merge(TailA, ListB, Tail)
            ;
                Res = (=),
                Key = KeyA,
                list.append(ValuesA, ValuesB, Values),
                multi_map.assoc_list_merge(TailA, TailB, Tail)
            ;
                Res = (>),
                Key = KeyB,
                Values = ValuesB,
                multi_map.assoc_list_merge(ListA, TailB, Tail)
            ),
            List = [Key - Values | Tail]
        )
    ).

%---------------------------------------------------------------------------%

optimize(MultiMap0) = MultiMap :-
    multi_map.optimize(MultiMap0, MultiMap).

optimize(MultiMap0, MultiMap) :-
    map.optimize(MultiMap0, MultiMap).

%---------------------------------------------------------------------------%

to_flat_assoc_list(MultiMap) = AssocList :-
    multi_map.to_flat_assoc_list(MultiMap, AssocList).

to_flat_assoc_list(MultiMap, AssocList) :-
    map.foldl(to_flat_assoc_list_acc, MultiMap, cord.init, Cord),
    AssocList = cord.list(Cord).

:- pred to_flat_assoc_list_acc(K::in, list(V)::in,
    cord(pair(K, V))::in, cord(pair(K, V))::out) is det.

to_flat_assoc_list_acc(Key, Values, !Cord) :-
    KeyValues = list.map((func(Value) = Key - Value), Values),
    !:Cord = !.Cord ++ cord.from_list(KeyValues).

%---------------------%

from_flat_assoc_list(AssocList) = MultiMap :-
    multi_map.from_flat_assoc_list(AssocList, MultiMap).

from_flat_assoc_list(AssocList, MultiMap) :-
    list.foldl(multi_map.add_from_pair, AssocList, map.init, MultiMap).

:- pred add_from_pair(pair(K, V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

add_from_pair(K - V, !MultiMap) :-
    multi_map.add(K, V, !MultiMap).

%---------------------%

to_assoc_list(MultiMap) = AssocList :-
    multi_map.to_assoc_list(MultiMap, AssocList).

to_assoc_list(MultiMap, AssocList) :-
    map.to_assoc_list(MultiMap, AssocList).

from_assoc_list(AssocList) = MultiMap :-
    multi_map.from_assoc_list(AssocList, MultiMap).

from_assoc_list(AssocList, MultiMap) :-
    map.from_assoc_list(AssocList, MultiMap).

from_sorted_assoc_list(AssocList) = MultiMap :-
    multi_map.from_sorted_assoc_list(AssocList, MultiMap).

from_sorted_assoc_list(AssocList, MultiMap) :-
    map.from_sorted_assoc_list(AssocList, MultiMap).

%---------------------%

from_corresponding_lists(Keys, Values) = MultiMap :-
    multi_map.from_corresponding_lists(Keys, Values, MultiMap).

from_corresponding_lists(Keys, Values, MultiMap) :-
    multi_map.init(MultiMap0),
    multi_map.from_corresponding_lists_2(Keys, Values, MultiMap0, MultiMap).

:- pred from_corresponding_lists_2(list(K)::in, list(V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

from_corresponding_lists_2([], [], !MultiMap).
from_corresponding_lists_2([], [_ | _], !MultiMap) :-
    unexpected("from_corresponding_lists", "list length mismatch").
from_corresponding_lists_2([_ | _], [], !MultiMap) :-
    unexpected("from_corresponding_lists", "list length mismatch").
from_corresponding_lists_2([Key | Keys], [Value | Values], !MultiMap) :-
    multi_map.add(Key, Value, !MultiMap),
    multi_map.from_corresponding_lists_2(Keys, Values, !MultiMap).

%---------------------%

from_corresponding_list_lists(Keys, Values) = MultiMap :-
    multi_map.from_corresponding_list_lists(Keys, Values, MultiMap).

from_corresponding_list_lists(Keys, Values, MultiMap) :-
    map.from_corresponding_lists(Keys, Values, MultiMap).

%---------------------------------------------------------------------------%

apply_to_list(Keys, MultiMap) = Values :-
    multi_map.apply_to_list(Keys, MultiMap, Values).

apply_to_list([], _, []).
apply_to_list(Keys @ [_ | _], MultiMap, Values) :-
    map.apply_to_list(Keys, MultiMap, ValueLists),
    list.condense(ValueLists, Values).

%---------------------------------------------------------------------------%

keys(MultiMap) = Keys :-
    multi_map.keys(MultiMap, Keys).

keys(MultiMap, Keys) :-
    map.keys(MultiMap, Keys).

sorted_keys(Map) = Keys :-
    map.sorted_keys(Map, Keys).

sorted_keys(Map, Keys) :-
    map.sorted_keys(Map, Keys).

keys_as_set(Map) = KeySet :-
    multi_map.keys_as_set(Map, KeySet).

keys_as_set(Map, KeySet) :-
    multi_map.sorted_keys(Map, Keys),
    set.sorted_list_to_set(Keys, KeySet).

values(MultiMap) = Values :-
    multi_map.values(MultiMap, Values).

values(MultiMap, Values) :-
    map.values(MultiMap, ValueLists),
    list.condense(ValueLists, Values).

%---------------------------------------------------------------------------%

count(MultiMap0) = Count :-
    multi_map.count(MultiMap0, Count).

count(MultiMap, Count) :-
    map.count(MultiMap, Count).

all_count(MultiMap0) = Count :-
    multi_map.all_count(MultiMap0, Count).

all_count(MultiMap, Count) :-
    map.foldl_values(multi_map.accumulate_length, MultiMap, 0, Count).

:- pred accumulate_length(list(V)::in, int::in, int::out) is det.

accumulate_length(Vs, !Count) :-
    list.length(Vs, NVs),
    !:Count = !.Count + NVs.

%---------------------------------------------------------------------------%
:- end_module multi_map.
%---------------------------------------------------------------------------%
