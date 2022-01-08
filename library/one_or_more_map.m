%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: one_or_more_map.m.
%
% This file provides a version of the 'multi_map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key, Value) pairs which allows you to look up any Value given the Key.
% A multi_map is similar, but it allows more than one Value for each Key.
% A multi_map represents this by using list(V) as the range type, which works,
% but does not express the invariant maintained by the relevant operations,
% which is that these lists are never empty. A one_or_more_map is a multi_map
% in which key the range type is one_or_more(V), which *does* express
% this invariant.
%
%---------------------------------------------------------------------------%

:- module one_or_more_map.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module one_or_more.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

:- type one_or_more_map(K, V) == map(K, one_or_more(V)).

%---------------------------------------------------------------------------%

    % Return an empty one_or_more_map.
    %
:- func init = one_or_more_map(K, V).
:- pred init(one_or_more_map(K, V)::uo) is det.

    % Check whether the one_or_more_map is empty.
    %
:- pred is_empty(one_or_more_map(K, V)::in) is semidet.

%---------------------%

    % Check whether the one_or_more_map has an entry for the given key.
    %
:- pred contains(one_or_more_map(K, V)::in, K::in) is semidet.

    % Succeed once for each key-value pair in the one_or_more_map.
    %
:- pred member(one_or_more_map(K, V)::in, K::out, V::out) is nondet.

    % If the one_or_more_map has an entry for the given key, return the
    % list of corresponding values.
    %
:- pred search(one_or_more_map(K, V)::in, K::in, one_or_more(V)::out)
    is semidet.

    % If the one_or_more_map has an entry for the given key,
    % succeed once for each of the corresponding values.
    %
:- pred nondet_search(one_or_more_map(K, V)::in, K::in, V::out) is nondet.

    % If the one_or_more_map has an entry for the given key,
    % succeed once for each of the corresponding values.
    % Otherwise, throw an exception.
    %
:- func lookup(one_or_more_map(K, V), K) = one_or_more(V).
:- pred lookup(one_or_more_map(K, V)::in, K::in, one_or_more(V)::out) is det.

    % If the one_or_more_map has an entry for the given key,
    % succeed once for each of the corresponding values.
    % Otherwise, throw an exception.
    %
:- pred nondet_lookup(one_or_more_map(K, V)::in, K::in, V::out) is nondet.

    % If the one_or_more_map has an entry for keys with the given value,
    % succeed once for each of those keys.
    %
    % NOTE: The implementation of this predicate is necessarily inefficient,
    % and so this predicate is intended for non-performance-critical uses only.
    %
:- pred inverse_search(one_or_more_map(K, V)::in, V::in, K::out) is nondet.

%---------------------%

    % Add the given key-value pair to the one_or_more_map.
    % Fail if the key already exists.
    %
:- pred insert(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is semidet.

    % Add the given key-value pair to the one_or_more_map.
    % Throw an exception if the key already exists.
    %
:- func det_insert(one_or_more_map(K, V), K, V) = one_or_more_map(K, V).
:- pred det_insert(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Add the given key-value pair to the one_or_more_map.
    % Fail if the key does not already exist.
    %
:- pred update(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is semidet.

    % Add the given key-value pair to the one_or_more_map.
    % Throw an exception if the key does not already exist.
    %
:- func det_update(one_or_more_map(K, V), K, V) = one_or_more_map(K, V).
:- pred det_update(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Replace the list of values corresponding to the given key.
    % Fails if the key does not already exist.
    %
:- pred replace(K::in, one_or_more(V)::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is semidet.

    % Replace the list of values corresponding to the given key.
    % Throws an exception if the key does not already exist.
    %
:- func det_replace(one_or_more_map(K, V), K,
    one_or_more(V)) = one_or_more_map(K, V).
:- pred det_replace(K::in, one_or_more(V)::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Add the given key-value pair to the one_or_more_map.
    % (`add' is a synonym for `set'.)
    %
:- func set(one_or_more_map(K, V), K, V) = one_or_more_map(K, V).
:- pred set(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.
:- func add(one_or_more_map(K, V), K, V) = one_or_more_map(K, V).
:- pred add(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Add the given value-key pair to the one_or_more_map.
    %
:- func reverse_set(one_or_more_map(K, V), V, K) = one_or_more_map(K, V).
:- pred reverse_set(V::in, K::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

%---------------------%

    % Delete a key and its corresponding values from a one_or_more_map.
    % If the key is not present, leave the one_or_more_map unchanged.
    %
:- func delete(one_or_more_map(K, V), K) = one_or_more_map(K, V).
:- pred delete(K::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Delete the given key-value pair from a one_or_more_map.
    % If the key is not present, leave the one_or_more_map unchanged.
    %
:- func delete(one_or_more_map(K, V), K, V) = one_or_more_map(K, V).
:- pred delete(K::in, V::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Delete a key from a one_or_more_map and return the list of values
    % previously corresponding to it.
    % Fail if the key is not present.
    %
:- pred remove(K::in, one_or_more(V)::out,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is semidet.

    % Delete a key from a one_or_more_map and return the list of values
    % previously corresponding to it.
    % Throw an exception if the key is not present.
    %
:- pred det_remove(K::in, one_or_more(V)::out,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

    % Remove the smallest key and its corresponding values from the
    % one_or_more_map.
    % Fails if the one_or_more_map is empty.
    %
:- pred remove_smallest(K::out, one_or_more(V)::out,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is semidet.

%---------------------%

    % Select takes a one_or_more_map and a set of keys and returns
    % a one_or_more_map containing only the keys in the set,
    % together with their corresponding values.
    %
:- func select(one_or_more_map(K, V), set(K)) = one_or_more_map(K, V).
:- pred select(one_or_more_map(K, V)::in, set(K)::in,
    one_or_more_map(K, V)::out) is det.

%---------------------%

    % merge(MultiMapA, MultiMapB, MultiMap):
    %
    % Merge `MultiMapA' and `MultiMapB' so that
    %
    % - if a key occurs in both `MultiMapA' and `MultiMapB', then the values
    %   corresponding to that key in `MultiMap' will be the concatenation
    %   of the values to that key from `MultiMapA' and `MultiMapB'; while
    % - if a key occurs in only one of `MultiMapA' and `MultiMapB', then
    %   the values corresponding to it in that map will be carried over
    %   to `MultiMap'.
    %
:- func merge(one_or_more_map(K, V), one_or_more_map(K, V))
    = one_or_more_map(K, V).
:- pred merge(one_or_more_map(K, V)::in, one_or_more_map(K, V)::in,
    one_or_more_map(K, V)::out) is det.

%---------------------%

    % Declaratively, a no-operation.
    % Operationally, a suggestion that the implementation optimize
    % the representation of the one_or_more_map, in the expectation that the
    % following operations will consist of searches and lookups
    % but (almost) no updates.
    %
:- func optimize(one_or_more_map(K, V)) = one_or_more_map(K, V).
:- pred optimize(one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

%---------------------%

    % Convert a one_or_more_map to an association list.
    %
:- func to_flat_assoc_list(one_or_more_map(K, V)) = assoc_list(K, V).
:- pred to_flat_assoc_list(one_or_more_map(K, V)::in,
    assoc_list(K, V)::out) is det.

    % Convert an association list to a one_or_more_map.
    %
:- func from_flat_assoc_list(assoc_list(K, V)) = one_or_more_map(K, V).
:- pred from_flat_assoc_list(assoc_list(K, V)::in,
    one_or_more_map(K, V)::out) is det.

    % Convert a one_or_more_map to an association list, with all the values
    % for each key in one element of the association list.
    %
:- func to_assoc_list(one_or_more_map(K, V)) = assoc_list(K, one_or_more(V)).
:- pred to_assoc_list(one_or_more_map(K, V)::in,
    assoc_list(K, one_or_more(V))::out) is det.

    % Convert an association list with all the values for each key
    % in one element of the list to a one_or_more_map.
    %
:- func from_assoc_list(assoc_list(K, one_or_more(V))) = one_or_more_map(K, V).
:- pred from_assoc_list(assoc_list(K, one_or_more(V))::in,
    one_or_more_map(K, V)::out) is det.

    % Convert a sorted association list to a one_or_more_map.
    %
:- func from_sorted_assoc_list(assoc_list(K, one_or_more(V)))
    = one_or_more_map(K, V).
:- pred from_sorted_assoc_list(assoc_list(K, one_or_more(V))::in,
    one_or_more_map(K, V)::out) is det.

    % Convert the corresponding elements of a list of keys and a
    % list of values (which must be of the same length) to a one_or_more_map.
    % A key may occur more than once in the list of keys.
    % Throw an exception if the two lists are not the same length.
    %
:- func from_corresponding_lists(list(K), list(V))
    = one_or_more_map(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in,
    one_or_more_map(K, V)::out) is det.

    % Convert the corresponding elements of a list of keys and a
    % *list of lists* of values to a one_or_more_map.
    % A key may *not* occur more than once in the list of keys.
    % Throw an exception if the two lists are not the same length,
    % or if a key does occur more than once in the list of keys.
    %
:- func from_corresponding_list_lists(list(K), list(one_or_more(V)))
    = one_or_more_map(K, V).
:- pred from_corresponding_list_lists(list(K)::in, list(one_or_more(V))::in,
    one_or_more_map(K, V)::out) is det.

%---------------------%

    % Given a list of keys, produce a list of their values in a
    % specified one_or_more_map.
    %
:- func apply_to_list(list(K), one_or_more_map(K, V)) = list(V).
:- pred apply_to_list(list(K)::in, one_or_more_map(K, V)::in, list(V)::out)
    is det.

%---------------------%

    % Given a one_or_more_map, return a list of all the keys in it.
    %
:- func keys(one_or_more_map(K, V)) = list(K).
:- pred keys(one_or_more_map(K, V)::in, list(K)::out) is det.

    % Given a one_or_more_map, return a list of all the keys in it
    % in sorted order.
    %
:- func sorted_keys(one_or_more_map(K, V)) = list(K).
:- pred sorted_keys(one_or_more_map(K, V)::in, list(K)::out) is det.

    % Given a one_or_more_map, return a list of all the keys in it
    % as a set
    %
:- func keys_as_set(one_or_more_map(K, V)) = set(K).
:- pred keys_as_set(one_or_more_map(K, V)::in, set(K)::out) is det.

    % Given a one_or_more_map, return a list of all the values in it.
    %
:- func values(one_or_more_map(K, V)) = list(V).
:- pred values(one_or_more_map(K, V)::in, list(V)::out) is det.

%---------------------%

    % Count the number of keys in the one_or_more_map.
    %
:- func count(one_or_more_map(K, V)) = int.
:- pred count(one_or_more_map(K, V)::in, int::out) is det.

    % Count the number of key-value pairs in the one_or_more_map.
    %
:- func all_count(one_or_more_map(K, V)) = int.
:- pred all_count(one_or_more_map(K, V)::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

init = OneOrMoreMap :-
    one_or_more_map.init(OneOrMoreMap).

init(OneOrMoreMap) :-
    map.init(OneOrMoreMap).

is_empty(OneOrMoreMap) :-
    map.is_empty(OneOrMoreMap).

%---------------------------------------------------------------------------%

contains(OneOrMoreMap, Key) :-
    map.search(OneOrMoreMap, Key, _).

member(OneOrMoreMap, Key, Value) :-
    map.member(OneOrMoreMap, Key, Values),
    one_or_more.member(Value, Values).

search(OneOrMoreMap, Key, Values) :-
    map.search(OneOrMoreMap, Key, Values).

nondet_search(OneOrMoreMap, Key, Value) :-
    map.search(OneOrMoreMap, Key, Values),
    one_or_more.member(Value, Values).

lookup(OneOrMoreMap, Key) = Value :-
    one_or_more_map.lookup(OneOrMoreMap, Key, Value).

lookup(OneOrMoreMap, Key, Values) :-
    map.lookup(OneOrMoreMap, Key, Values).

nondet_lookup(OneOrMoreMap, Key, Value) :-
    map.search(OneOrMoreMap, Key, Values),
    one_or_more.member(Value, Values).

inverse_search(OneOrMoreMap, Value, Key) :-
    map.member(OneOrMoreMap, Key, Values),
    one_or_more.member(Value, Values).

%---------------------------------------------------------------------------%

insert(Key, Value, !OneOrMoreMap) :-
    map.insert(Key, one_or_more(Value, []), !OneOrMoreMap).

det_insert(!.OneOrMoreMap, Key, Value) = !:OneOrMoreMap :-
    one_or_more_map.det_insert(Key, Value, !OneOrMoreMap).

det_insert(Key, Value, !OneOrMoreMap) :-
    map.det_insert(Key, one_or_more(Value, []), !OneOrMoreMap).

update(Key, Value, !OneOrMoreMap) :-
    map.search(!.OneOrMoreMap, Key, Values0),
    Values = one_or_more.cons(Value, Values0),
    map.update(Key, Values, !OneOrMoreMap).

det_update(!.OneOrMoreMap, Key, Value) = !:OneOrMoreMap :-
    one_or_more_map.det_update(Key, Value, !OneOrMoreMap).

det_update(Key, Value, !OneOrMoreMap) :-
    ( if one_or_more_map.update(Key, Value, !OneOrMoreMap) then
        true
    else
         report_lookup_error("one_or_more_map.det_update: key not found", Key)
    ).

replace(Key, Value, !OneOrMoreMap) :-
    map.update(Key, Value, !OneOrMoreMap).

det_replace(!.OneOrMoreMap, Key, Values) = !:OneOrMoreMap :-
    one_or_more_map.det_replace(Key, Values, !OneOrMoreMap).

det_replace(Key, Values, !OneOrMoreMap) :-
    map.det_update(Key, Values, !OneOrMoreMap).

set(!.OneOrMoreMap, Key, Value) = !:OneOrMoreMap :-
    one_or_more_map.set(Key, Value, !OneOrMoreMap).

set(Key, Value, !OneOrMoreMap) :-
    ( if map.search(!.OneOrMoreMap, Key, Values0) then
        Values = one_or_more.cons(Value, Values0),
        map.set(Key, Values, !OneOrMoreMap)
    else
        map.det_insert(Key, one_or_more(Value, []), !OneOrMoreMap)
    ).

add(!.OneOrMoreMap, Key, Value) = !:OneOrMoreMap :-
    one_or_more_map.set(Key, Value, !OneOrMoreMap).

add(Key, Value, !OneOrMoreMap) :-
    one_or_more_map.set(Key, Value, !OneOrMoreMap).

reverse_set(!.OneOrMoreMap, Value, Key) = !:OneOrMoreMap :-
    one_or_more_map.reverse_set(Value, Key, !OneOrMoreMap).

reverse_set(Value, Key, !OneOrMoreMap) :-
    one_or_more_map.set(Key, Value, !OneOrMoreMap).

%---------------------------------------------------------------------------%

delete(!.OneOrMoreMap, Key) = !:OneOrMoreMap :-
    one_or_more_map.delete(Key, !OneOrMoreMap).

delete(Key, !OneOrMoreMap) :-
    map.delete(Key, !OneOrMoreMap).

delete(!.OneOrMoreMap, Key, Value) = !:OneOrMoreMap :-
    one_or_more_map.delete(Key, Value, !OneOrMoreMap).

delete(Key, Value, !OneOrMoreMap) :-
    ( if
        map.search(!.OneOrMoreMap, Key, Values0),
        one_or_more.delete_all(Values0, Value, Values)
    then
        (
            Values = [],
            map.delete(Key, !OneOrMoreMap)
        ;
            Values = [VH | VT],
            map.det_update(Key, one_or_more(VH, VT), !OneOrMoreMap)
        )
    else
        true
    ).

remove(OneOrMoreMap0, Key, Values, OneOrMoreMap) :-
    map.remove(OneOrMoreMap0, Key, Values, OneOrMoreMap).

det_remove(OneOrMoreMap0, Key, Values, OneOrMoreMap) :-
    map.det_remove(OneOrMoreMap0, Key, Values, OneOrMoreMap).

remove_smallest(OneOrMoreMap0, Key, Values, OneOrMoreMap) :-
    map.remove_smallest(OneOrMoreMap0, Key, Values, OneOrMoreMap).

%---------------------------------------------------------------------------%

select(OneOrMoreMap0, KeySet) = OneOrMoreMap :-
    one_or_more_map.select(OneOrMoreMap0, KeySet, OneOrMoreMap).

select(OneOrMoreMap0, KeySet, OneOrMoreMap) :-
    map.select(OneOrMoreMap0, KeySet, OneOrMoreMap).

%---------------------------------------------------------------------------%

merge(OneOrMoreMapA, OneOrMoreMapB) = OneOrMoreMap :-
    one_or_more_map.merge(OneOrMoreMapA, OneOrMoreMapB, OneOrMoreMap).

merge(M0, M1, M) :-
    one_or_more_map.to_assoc_list(M0, ML0),
    one_or_more_map.to_assoc_list(M1, ML1),
    one_or_more_map.assoc_list_merge(ML0, ML1, ML),
    one_or_more_map.from_sorted_assoc_list(ML, M).

:- pred one_or_more_map.assoc_list_merge(assoc_list(K, one_or_more(V))::in,
    assoc_list(K, one_or_more(V))::in, assoc_list(K, one_or_more(V))::out)
    is det.

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
                one_or_more_map.assoc_list_merge(TailA, ListB, Tail)
            ;
                Res = (=),
                Key = KeyA,
                one_or_more.append(ValuesA, ValuesB, Values),
                one_or_more_map.assoc_list_merge(TailA, TailB, Tail)
            ;
                Res = (>),
                Key = KeyB,
                Values = ValuesB,
                one_or_more_map.assoc_list_merge(ListA, TailB, Tail)
            ),
            List = [Key - Values | Tail]
        )
    ).

%---------------------------------------------------------------------------%

optimize(OneOrMoreMap0) = OneOrMoreMap :-
    one_or_more_map.optimize(OneOrMoreMap0, OneOrMoreMap).

optimize(OneOrMoreMap0, OneOrMoreMap) :-
    map.optimize(OneOrMoreMap0, OneOrMoreMap).

%---------------------------------------------------------------------------%

to_flat_assoc_list(OneOrMoreMap) = AssocList :-
    one_or_more_map.to_flat_assoc_list(OneOrMoreMap, AssocList).

to_flat_assoc_list(OneOrMoreMap, AssocList) :-
    map.foldl(to_flat_assoc_list_acc, OneOrMoreMap, cord.init, Cord),
    AssocList = cord.list(Cord).

:- pred to_flat_assoc_list_acc(K::in, one_or_more(V)::in,
    cord(pair(K, V))::in, cord(pair(K, V))::out) is det.

to_flat_assoc_list_acc(Key, Values, !Cord) :-
    KeyValues = list.map((func(Value) = Key - Value),
        one_or_more_to_list(Values)),
    !:Cord = !.Cord ++ cord.from_list(KeyValues).

%---------------------%

from_flat_assoc_list(AssocList) = OneOrMoreMap :-
    one_or_more_map.from_flat_assoc_list(AssocList, OneOrMoreMap).

from_flat_assoc_list(AssocList, OneOrMoreMap) :-
    list.foldl(one_or_more_map.add_from_pair, AssocList,
        map.init, OneOrMoreMap).

:- pred one_or_more_map.add_from_pair(pair(K, V)::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

add_from_pair(K - V, !OneOrMoreMap) :-
    one_or_more_map.add(K, V, !OneOrMoreMap).

%---------------------%

to_assoc_list(OneOrMoreMap) = AssocList :-
    one_or_more_map.to_assoc_list(OneOrMoreMap, AssocList).

to_assoc_list(OneOrMoreMap, AssocList) :-
    map.to_assoc_list(OneOrMoreMap, AssocList).

from_assoc_list(AssocList) = OneOrMoreMap :-
    one_or_more_map.from_assoc_list(AssocList, OneOrMoreMap).

from_assoc_list(AssocList, OneOrMoreMap) :-
    map.from_assoc_list(AssocList, OneOrMoreMap).

from_sorted_assoc_list(AssocList) = OneOrMoreMap :-
    one_or_more_map.from_sorted_assoc_list(AssocList, OneOrMoreMap).

from_sorted_assoc_list(AssocList, OneOrMoreMap) :-
    map.from_sorted_assoc_list(AssocList, OneOrMoreMap).

%---------------------%

from_corresponding_lists(Keys, Values) = OneOrMoreMap :-
    one_or_more_map.from_corresponding_lists(Keys, Values, OneOrMoreMap).

from_corresponding_lists(Keys, Values, OneOrMoreMap) :-
    one_or_more_map.init(OneOrMoreMap0),
    one_or_more_map.from_corresponding_lists_2(Keys, Values,
        OneOrMoreMap0, OneOrMoreMap).

:- pred one_or_more_map.from_corresponding_lists_2(list(K)::in, list(V)::in,
    one_or_more_map(K, V)::in, one_or_more_map(K, V)::out) is det.

from_corresponding_lists_2([], [], !OneOrMoreMap).
from_corresponding_lists_2([], [_ | _], !OneOrMoreMap) :-
    unexpected("from_corresponding_lists", "list length mismatch").
from_corresponding_lists_2([_ | _], [], !OneOrMoreMap) :-
    unexpected("from_corresponding_lists", "list length mismatch").
from_corresponding_lists_2([Key | Keys], [Value | Values], !OneOrMoreMap) :-
    one_or_more_map.add(Key, Value, !OneOrMoreMap),
    one_or_more_map.from_corresponding_lists_2(Keys, Values, !OneOrMoreMap).

%---------------------%

from_corresponding_list_lists(Keys, Values) = OneOrMoreMap :-
    one_or_more_map.from_corresponding_list_lists(Keys, Values, OneOrMoreMap).

from_corresponding_list_lists(Keys, Values, OneOrMoreMap) :-
    map.from_corresponding_lists(Keys, Values, OneOrMoreMap).

%---------------------------------------------------------------------------%

apply_to_list(Keys, OneOrMoreMap) = Values :-
    one_or_more_map.apply_to_list(Keys, OneOrMoreMap, Values).

apply_to_list([], _, []).
apply_to_list(Keys @ [_ | _], OneOrMoreMap, Values) :-
    map.apply_to_list(Keys, OneOrMoreMap, ValueLists),
    one_or_more.condense(ValueLists, Values).

%---------------------------------------------------------------------------%

keys(OneOrMoreMap) = Keys :-
    one_or_more_map.keys(OneOrMoreMap, Keys).

keys(OneOrMoreMap, Keys) :-
    map.keys(OneOrMoreMap, Keys).

sorted_keys(Map) = Keys :-
    map.sorted_keys(Map, Keys).

sorted_keys(Map, Keys) :-
    map.sorted_keys(Map, Keys).

keys_as_set(Map) = KeySet :-
    one_or_more_map.keys_as_set(Map, KeySet).

keys_as_set(Map, KeySet) :-
    one_or_more_map.sorted_keys(Map, Keys),
    set.sorted_list_to_set(Keys, KeySet).

values(OneOrMoreMap) = Values :-
    one_or_more_map.values(OneOrMoreMap, Values).

values(OneOrMoreMap, Values) :-
    map.values(OneOrMoreMap, ValueLists),
    one_or_more.condense(ValueLists, Values).

%---------------------------------------------------------------------------%

count(OneOrMoreMap0) = Count :-
    one_or_more_map.count(OneOrMoreMap0, Count).

count(OneOrMoreMap, Count) :-
    map.count(OneOrMoreMap, Count).

all_count(OneOrMoreMap0) = Count :-
    one_or_more_map.all_count(OneOrMoreMap0, Count).

all_count(OneOrMoreMap, Count) :-
    map.foldl_values(one_or_more_map.accumulate_length, OneOrMoreMap,
        0, Count).

:- pred accumulate_length(one_or_more(V)::in, int::in, int::out) is det.

accumulate_length(Vs, !Count) :-
    one_or_more.length(Vs, NumVs),
    !:Count = !.Count + NumVs.

%---------------------------------------------------------------------------%
:- end_module one_or_more_map.
%---------------------------------------------------------------------------%
