%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1995, 1997, 2000, 2002-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: multi_map.m.
% Main author: dylan.  Based on map.m, by fjh, conway.
% Stability: low.
% 
% This file provides the 'multi_map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key, Data) pairs which allows you to look up any Data item given the
% Key.  A multi_map is similar, though allows a one to many relationship
% between keys and data.
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

:- type multi_map(Key, Data) == map(Key, list(Data)).

%---------------------------------------------------------------------------%

    % Initialize an empty multi_map.
    %
:- func init = multi_map(_, _).
:- pred init(multi_map(_, _)::uo) is det.

    % Check whether a multi_map is empty.
    %
:- pred is_empty(multi_map(_, _)::in) is semidet.

    % Check whether multi_map contains key.
    %
:- pred contains(multi_map(K, _V)::in, K::in) is semidet.

:- pred member(multi_map(K, V)::in, K::out, V::out) is nondet.

    % Search multi_map for given key.
    %
:- pred search(multi_map(K, V)::in, K::in, list(V)::out) is semidet.

    % Search multi_map for given key.
    %
:- pred nondet_search(multi_map(K, V)::in, K::in, V::out) is nondet.

    % Search multi_map for key, but abort if search fails.
    %
:- func lookup(multi_map(K, V), K) = list(V).
:- pred lookup(multi_map(K, V)::in, K::in, list(V)::out) is det.

    % Search multi_map for key.
    %
:- pred nondet_lookup(multi_map(K, V)::in, K::in, V::out) is nondet.

    % Search multi_map for data.
    %
:- pred inverse_search(multi_map(K, V)::in, V::in, K::out) is nondet.

    % Insert a new key and corresponding value into a multi_map.
    % Fail if the key already exists.
    %
:- pred insert(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Insert a new key and corresponding value into a multi_map.
    % Aborts if the key already exists.
    %
:- func det_insert(multi_map(K, V), K, V) = multi_map(K, V).
:- pred det_insert(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Update (add) the value corresponding to a given key.
    % Fails if the key does not already exist.
    %
:- pred update(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Update (add) the value corresponding to a given key.
    % Aborts if the key does not already exist.
    %
:- func det_update(multi_map(K, V), K, V) = multi_map(K, V).
:- pred det_update(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Update (replace) the value corresponding to a given key.
    % Fails if the key does not already exist.
    %
:- pred replace(K::in, list(V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Update (replace) the value corresponding to a given key.
    % Aborts if the key does not already exist.
    %
:- func det_replace(multi_map(K, V), K, list(V)) = multi_map(K, V).
:- pred det_replace(K::in, list(V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Update (add) value if the key is already present, otherwise
    % insert the new key and value.
    %
:- func set(multi_map(K, V), K, V) = multi_map(K, V).
:- pred set(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

:- func add(multi_map(K, V), K, V) = multi_map(K, V).
:- pred add(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Given a multi_map, return a list of all the keys in the multi_map.
    %
:- func keys(multi_map(K, _V)) = list(K).
:- pred keys(multi_map(K, _V)::in, list(K)::out) is det.

    % Given a multi_map, return a list of all the data values in the
    % multi_map.
    %
:- func values(multi_map(_K, V)) = list(V).
:- pred values(multi_map(_K, V)::in, list(V)::out) is det.

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

    % Convert a multi_map to an association list, with all the
    % values for each key in one element of the association list.
    %
:- func to_assoc_list(multi_map(K, V)) = assoc_list(K, list(V)).
:- pred to_assoc_list(multi_map(K, V)::in,
    assoc_list(K, list(V))::out) is det.

    % Convert an association list with all the values for each
    % key in one element of the list to a multi_map.
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

    % Delete a key and data from a multi_map
    % if the key is not present, leave the multi_map unchanged.
    %
:- func delete(multi_map(K, V), K) = multi_map(K, V).
:- pred delete(K::in, 
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Delete a data value from a key in a multi_map
    % if the key is not present, leave the multi_map unchanged.
    %
:- func delete(multi_map(K, V), K, V) = multi_map(K, V).
:- pred delete(K::in, V::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Delete a key-value pair from a multi_map and return the value.
    % Fails if the key is not present.
    %
:- pred remove(K::in, list(V)::out,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

    % Delete a key-value pair from a multi_map and return the value.
    % Aborts if the key is not present.
    %
:- pred det_remove(K::in, list(V)::out,
    multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Count the number of elements (keys) in the multi_map.
    %
:- func count(multi_map(K, V)) = int.
:- pred count(multi_map(K, V)::in, int::out) is det.

    % Count the number of data elements in the multi_map.
    %
:- func all_count(multi_map(K, V)) = int.
:- pred all_count(multi_map(K, V)::in, int::out) is det.

    % Convert a pair of lists (which must be of the same length)
    % to a multi_map.
    %
:- func from_corresponding_lists(list(K), list(V))
    = multi_map(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in,
    multi_map(K, V)::out) is det.

    % Convert a pair of lists (which must be of the same length)
    % to a multi_map.
    %
:- func from_corresponding_list_lists(list(K), list(list(V)))
    = multi_map(K, V).
:- pred from_corresponding_list_lists(list(K)::in, list(list(V))::in,
    multi_map(K, V)::out) is det.

    % merge(MultiMapA, MultiMapB, MultiMap).
    % Merge `MultiMapA' and `MultiMapB' so that if a key occurs in
    % both `MultiMapA' and `MultiMapB' then the values corresponding
    % to that key in `MultiMap' will be the concatenation of
    % the values corresponding to that key from `MultiMapA' and
    % `MultiMapB'.
    %
:- func merge(multi_map(K, V), multi_map(K, V))
    = multi_map(K, V).
:- pred merge(multi_map(K, V)::in, multi_map(K, V)::in,
    multi_map(K, V)::out) is det.

    % select takes a multi_map and a set of keys and returns
    % a multi_map containing the keys in the set and their corresponding
    % values.
    %
:- func select(multi_map(K, V), set(K)) = multi_map(K, V).
:- pred select(multi_map(K, V)::in, set(K)::in,
    multi_map(K, V)::out) is det.

    % Given a list of keys, produce a list of their values in a
    % specified multi_map.
    %
:- func apply_to_list(list(K), multi_map(K, V)) = list(V).
:- pred apply_to_list(list(K)::in, multi_map(K, V)::in,
    list(V)::out) is det.

    % Declaratively, a NOP.
    % Operationally, a suggestion that the implementation
    % optimize the representation of the multi_map in the expectation
    % of a number of lookups but few or no modifications.
    %
:- func optimize(multi_map(K, V)) = multi_map(K, V).
:- pred optimize(multi_map(K, V)::in, multi_map(K, V)::out) is det.

    % Remove the smallest item from the multi_map.
    % Fails if the multi_map is empty.
    %
:- pred remove_smallest(K::out, list(V)::out,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

multi_map.init(M) :-
    map.init(M).

multi_map.is_empty(M) :-
    map.is_empty(M).

multi_map.contains(MultiMap, Key) :-
    map.search(MultiMap, Key, _).

multi_map.member(MultiMap, Key, Value) :-
    map.member(MultiMap, Key, ValueList),
    list.member(Value, ValueList).

multi_map.search(MultiMap, Key, Values) :-
    map.search(MultiMap, Key, Values).

multi_map.nondet_search(MultiMap, Key, Value) :-
    map.search(MultiMap, Key, Values),
    list.member(Value, Values).

multi_map.lookup(MultiMap, Key, Values) :-
    map.lookup(MultiMap, Key, Values).

multi_map.nondet_lookup(MultiMap, Key, Value) :-
    map.search(MultiMap, Key, Values),
    list.member(Value, Values).

multi_map.insert(Key, Value, !MultiMap) :-
    map.insert(Key, [Value], !MultiMap).

multi_map.det_insert(Key, Value, !MultiMap) :-
    map.det_insert(Key, [Value], !MultiMap).

multi_map.update(Key, Value, !MultiMap) :-
    map.search(!.MultiMap, Key, Values0),
    Values = [Value | Values0],
    map.update(Key, Values, !MultiMap).

multi_map.det_update(Key, Value, !MultiMap) :-
    ( if multi_map.update(Key, Value, !MultiMap) then
        true
      else
         report_lookup_error("multi_map.det_update: key not found", Key)
    ).

multi_map.replace(Key, Value, !MultiMap) :-
    map.update(Key, Value, !MultiMap).

multi_map.det_replace(Key, Value, !MultiMap) :-
    map.det_update(Key, Value, !MultiMap).

multi_map.set(Key, Value, !MultiMap) :-
    ( map.search(!.MultiMap, Key, Values0) ->
        Values = [Value | Values0],
        map.set(Key, Values, !MultiMap)
    ;
        map.det_insert(Key, [Value], !MultiMap)
    ).

multi_map.add(MultiMap0, Key, Value, MultiMap) :-
    multi_map.set(MultiMap0, Key, Value, MultiMap).

multi_map.keys(MultiMap, KeyList) :-
    map.keys(MultiMap, KeyList).

multi_map.values(MultiMap, KeyList) :-
    map.values(MultiMap, KeyList0),
    list.condense(KeyList0, KeyList).

multi_map.from_flat_assoc_list(AList, MultiMap) :-
    MultiMap = list.foldl(
        (func(Key - Value, Map0) = Map  :-
            multi_map.set(Key, Value, Map0, Map)
        ),
        AList, map.init).

multi_map.to_flat_assoc_list(MultiMap, AList) :-
    AList = list.reverse(map.foldl(
        (func(Key, Values, AL) =
            list.reverse(
                list.map((func(Value) = Key - Value), Values)
            ) ++ AL
        ),
        MultiMap, [])).

multi_map.from_assoc_list(AList, MultiMap) :-
    map.from_assoc_list(AList, MultiMap).

multi_map.from_sorted_assoc_list(AList, MultiMap) :-
    map.from_sorted_assoc_list(AList, MultiMap).

multi_map.to_assoc_list(MultiMap, AList) :-
    map.to_assoc_list(MultiMap, AList).

multi_map.delete(Key, !MultiMap) :-
    map.delete(Key, !MultiMap).

multi_map.delete(Key, Value, !MultiMap) :-
    (
        map.search(!.MultiMap, Key, Values0),
        list.delete_all(Values0, Value, Values)
    ->
        (
            Values = [],
            map.delete(Key, !MultiMap)
        ;
            Values = [_ | _],
            map.set(Key, Values, !MultiMap)
        )
    ;
        true
    ).

multi_map.remove(MultiMap0, Key, Values, MultiMap) :-
    map.remove(MultiMap0, Key, Values, MultiMap).

multi_map.det_remove(MultiMap0, Key, Values, MultiMap) :-
    map.det_remove(MultiMap0, Key, Values, MultiMap).

multi_map.count(MultiMap, Count) :-
    map.count(MultiMap, Count).

multi_map.all_count(MultiMap, Count) :-
    multi_map.values(MultiMap, List),
    multi_map.count_list(List, 0, Count).

:- pred multi_map.count_list(list(A)::in, int::in, int::out) is det.

multi_map.count_list([], X, X).
multi_map.count_list([_A | As], Count0, Count) :-
    Count1 = Count0 + 1,
    multi_map.count_list(As, Count1, Count).

%---------------------------------------------------------------------------%

    % XXX inefficient

multi_map.inverse_search(MultiMap, Value, Key) :-
    map.member(MultiMap, Key, ValueList),
    list.member(Value, ValueList).

%---------------------------------------------------------------------------%

multi_map.from_corresponding_lists(Keys, Values, MultiMap) :-
    multi_map.init(MultiMap0),
    (
        multi_map.from_corresponding_lists_2(Keys, Values,
            MultiMap0, MultiMap1)
    ->
        MultiMap = MultiMap1
    ;
        error("multi_map.from_corresponding_lists: list length mismatch")
    ).

:- pred multi_map.from_corresponding_lists_2(list(K)::in, list(V)::in,
    multi_map(K, V)::in, multi_map(K, V)::out) is semidet.

multi_map.from_corresponding_lists_2([], [], !MultiMap).
multi_map.from_corresponding_lists_2([Key | Keys], [Value | Values], !MultiMap) :-
    multi_map.set(Key, Value, !MultiMap),
    multi_map.from_corresponding_lists_2(Keys, Values, !MultiMap).

multi_map.from_corresponding_list_lists(Keys, Values, MultiMap) :-
    map.from_corresponding_lists(Keys, Values, MultiMap).

%---------------------------------------------------------------------------%

multi_map.merge(M0, M1, M) :-
    multi_map.to_assoc_list(M0, ML0),
    multi_map.to_assoc_list(M1, ML1),
    multi_map.assoc_list_merge(ML0, ML1, ML),
    multi_map.from_sorted_assoc_list(ML, M).

:- pred multi_map.assoc_list_merge(assoc_list(K, list(V))::in,
    assoc_list(K, list(V))::in, assoc_list(K, list(V))::out) is det.

multi_map.assoc_list_merge([], ListB, ListB).
multi_map.assoc_list_merge([A | ListA], [], [A | ListA]).
multi_map.assoc_list_merge([(KeyA - DataA) | ListA], [(KeyB - DataB) | ListB],
        [(Key - Data) | List]) :-
    compare(Res, KeyA, KeyB),
    (
        Res = (=),
        Key = KeyA,
        list.append(DataA, DataB, Data),
        multi_map.assoc_list_merge(ListA, ListB, List)
    ;
        Res = (<),
        Key = KeyA,
        Data = DataA,
        multi_map.assoc_list_merge(ListA, [(KeyB - DataB) | ListB], List)
    ;
        Res = (>),
        Key = KeyB,
        Data = DataB,
        multi_map.assoc_list_merge([(KeyA - DataA) | ListA], ListB, List)
    ).

%---------------------------------------------------------------------------%

multi_map.optimize(MultiMap0, MultiMap) :-
    map.optimize(MultiMap0, MultiMap).

%---------------------------------------------------------------------------%

multi_map.select(Original, KeySet, NewMultiMap) :-
    map.select(Original, KeySet, NewMultiMap).

%---------------------------------------------------------------------------%

multi_map.apply_to_list([], _, []).
multi_map.apply_to_list([K | Keys], MultiMap, Values) :-
    map.apply_to_list([K | Keys], MultiMap, Values0),
    list.condense(Values0, Values).

%---------------------------------------------------------------------------%

multi_map.remove_smallest(MultiMap0, Key, Values, MultiMap) :-
    map.remove_smallest(MultiMap0, Key, Values, MultiMap).

%---------------------------------------------------------------------------%

% Functional versions.

multi_map.init = M :-
    multi_map.init(M).

multi_map.lookup(MultiMap, Key) = Value :-
    multi_map.lookup(MultiMap, Key, Value).

multi_map.det_insert(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.det_insert(Key, Value, !MultiMap).

multi_map.det_update(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.det_update(Key, Value, !MultiMap).

multi_map.det_replace(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.det_replace(Key, Value, !MultiMap).

multi_map.set(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.set(Key, Value, !MultiMap).

multi_map.add(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.set(Key, Value, !MultiMap).

multi_map.keys(MultiMap) = Keys :-
    multi_map.keys(MultiMap, Keys).

multi_map.values(MultiMap) = Valyes :-
    multi_map.values(MultiMap, Valyes).

multi_map.to_flat_assoc_list(MultiMap) = AssocList :-
    multi_map.to_flat_assoc_list(MultiMap, AssocList).

multi_map.from_flat_assoc_list(AssocList) = MultiMap :-
    multi_map.from_flat_assoc_list(AssocList, MultiMap).

multi_map.to_assoc_list(MultiMap) = AssocList :-
    multi_map.to_assoc_list(MultiMap, AssocList).

multi_map.from_assoc_list(AssocList) = MultiMap :-
    multi_map.from_assoc_list(AssocList, MultiMap).

multi_map.from_sorted_assoc_list(AssocList) = MultiMap :-
    multi_map.from_sorted_assoc_list(AssocList, MultiMap).

multi_map.delete(!.MultiMap, Key) = !:MultiMap :-
    multi_map.delete(Key, !MultiMap).

multi_map.delete(!.MultiMap, Key, Value) = !:MultiMap :-
    multi_map.delete(Key, Value, !MultiMap).

multi_map.count(MultiMap0) = Count :-
    multi_map.count(MultiMap0, Count).

multi_map.all_count(MultiMap0) = Count :-
    multi_map.all_count(MultiMap0, Count).

multi_map.from_corresponding_lists(Keys, Values) = MultiMap :-
    multi_map.from_corresponding_lists(Keys, Values, MultiMap).

multi_map.from_corresponding_list_lists(Keys, Values) = MultiMap :-
    multi_map.from_corresponding_list_lists(Keys, Values, MultiMap).

multi_map.merge(MultiMapA, MultiMapB) = MultiMap :-
    multi_map.merge(MultiMapA, MultiMapB, MultiMap).

multi_map.select(MultiMap, KeySet) = NewMultiMap :-
    multi_map.select(MultiMap, KeySet, NewMultiMap).

multi_map.apply_to_list(Keys, MultiMap) = Values :-
    multi_map.apply_to_list(Keys, MultiMap, Values).

multi_map.optimize(MultiMap0) = MultiMap :-
    multi_map.optimize(MultiMap0, MultiMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
