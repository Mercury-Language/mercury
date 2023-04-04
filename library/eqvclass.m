%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999, 2003-2006, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: eqvclass.m.
% Author: zs.
% Stability: low.
%
% A module for handling equivalence classes.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module eqvclass.
:- interface.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type eqvclass(T).
:- type partition_id.

    % Create an empty equivalence class.
    %
:- func init = eqvclass(T).
:- pred init(eqvclass(T)::out) is det.

    % Is this item known to the equivalence class?
    %
:- pred is_member(eqvclass(T)::in, T::in) is semidet.

    % If this item is known to the equivalence class, return the id of its
    % partition. The only use that the caller can make of the partition id
    % is to check whether two items in the same equivalence calls have the
    % same partition id; that test will succeed if and only if the two
    % items are in the same partition. Partition ids are not guaranteed
    % to stay the same as an eqvclass is updated, so such comparisons will
    % work only against the same eqvclass.
    %
    % If you want to check whether two items are in the same equivalence class,
    % using same_eqvclass is more expressive than calling
    % partition_id on both items and comparing the results.
    % However, if you want to perform this check on X and Y1, on X and Y2,
    % ... X and Yn, then calling partition_id on X just once and
    % comparing this with the partition_ids of the Yi will be more efficient.
    %
:- pred partition_id(eqvclass(T)::in, T::in, partition_id::out)
    is semidet.

    % Make an element known to the equivalence class.
    % The element may already be known to the class;
    % if it isn't, it is created without any equivalence relationships.
    %
:- func ensure_element(eqvclass(T), T) = eqvclass(T).
:- pred ensure_element(T::in, eqvclass(T)::in, eqvclass(T)::out)
    is det.

    % Make this item known to the equivalence class if it isn't already,
    % and return the id of its partition. The same proviso applies with
    % respect to partition_ids as with partition_id.
    %
:- pred ensure_element_partition_id(T::in, partition_id::out,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Make an element known to the equivalence class.
    % The element must not already be known to the class;
    % it is created without any equivalence relationships.
    %
:- func new_element(eqvclass(T), T) = eqvclass(T).
:- pred new_element(T::in, eqvclass(T)::in, eqvclass(T)::out) is det.

    % Make two elements of the equivalence class equivalent.
    % It is ok if they already are.
    %
:- func ensure_equivalence(eqvclass(T), T, T) = eqvclass(T).
:- pred ensure_equivalence(T::in, T::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

:- func ensure_corresponding_equivalences(list(T), list(T),
    eqvclass(T)) = eqvclass(T).
:- pred ensure_corresponding_equivalences(list(T)::in, list(T)::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Make two elements of the equivalence class equivalent.
    % It is an error if they are already equivalent.
    %
:- func new_equivalence(eqvclass(T), T, T) = eqvclass(T).
:- pred new_equivalence(T::in, T::in, eqvclass(T)::in, eqvclass(T)::out)
    is det.

    % Test if two elements are equivalent.
    %
:- pred same_eqvclass(eqvclass(T)::in, T::in, T::in) is semidet.

    % Test if a list of elements are equivalent.
    %
:- pred same_eqvclass_list(eqvclass(T)::in, list(T)::in) is semidet.

    % Return the set of the partitions of the equivalence class.
    %
:- func partition_set(eqvclass(T)) = set(set(T)).
:- pred partition_set(eqvclass(T)::in, set(set(T))::out) is det.

    % Return a list of the partitions of the equivalence class.
    %
:- func partition_list(eqvclass(T)) = list(set(T)).
:- pred partition_list(eqvclass(T)::in, list(set(T))::out) is det.

    % Create an equivalence class from a partition set.
    % It is an error if the sets are not disjoint.
    %
:- func partition_set_to_eqvclass(set(set(T))) = eqvclass(T).
:- pred partition_set_to_eqvclass(set(set(T))::in, eqvclass(T)::out) is det.

    % Create an equivalence class from a list of partitions.
    % It is an error if the sets are not disjoint.
    %
:- func partition_list_to_eqvclass(list(set(T))) = eqvclass(T).
:- pred partition_list_to_eqvclass(list(set(T))::in,
    eqvclass(T)::out) is det.

    % Return the set of elements equivalent to the given element.
    % This set will of course include the given element.
    %
:- func get_equivalent_elements(eqvclass(T), T) = set(T).

    % Return the smallest element equivalent to the given element.
    % This may or may not be the given element.
    %
:- func get_minimum_element(eqvclass(T), T) = T.

    % Remove the given element and all other elements equivalent to it
    % from the given equivalence class.
    %
:- func remove_equivalent_elements(eqvclass(T), T) = eqvclass(T).
:- pred remove_equivalent_elements(T::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Given a function, divide each partition in the original equivalence class
    % so that two elements of the original partition end up in the same
    % partition in the new equivalence class if and only if the function maps
    % them to the same value.
    %
:- func divide_equivalence_classes(func(T) = U, eqvclass(T)) = eqvclass(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module require.

%---------------------------------------------------------------------------%

:- type eqvclass(T)
    --->    eqvclass(
                % We use counter to allocate new partition ids.
                next_id_counter :: counter,

                % Maps each partition_id to the set of elements in that
                % partition. The set should never be empty.
                partition_map   :: map(partition_id, set(T)),

                % Maps each item in the eqvclass to the id of the parition
                % to which it belongs.
                element_map     :: map(T, partition_id)
            ).

:- type partition_id    ==  int.

%---------------------------------------------------------------------------%

init = EqvClass :-
    eqvclass.init(EqvClass).

init(EqvClass) :-
    map.init(PartitionMap),
    map.init(ElementMap),
    EqvClass = eqvclass(counter.init(0), PartitionMap, ElementMap).

%---------------------------------------------------------------------------%

is_member(EqvClass, Element) :-
    ElementMap = EqvClass ^ element_map,
    map.search(ElementMap, Element, _).

%---------------------------------------------------------------------------%

partition_id(EqvClass, Element, PartitionId) :-
    ElementMap = EqvClass ^ element_map,
    map.search(ElementMap, Element, PartitionId).

%---------------------------------------------------------------------------%

ensure_element(!.EqvClass, X) = !:EqvClass :-
    ensure_element(X, !EqvClass).

ensure_element(Element, !EqvClass) :-
    ensure_element_partition_id(Element, _, !EqvClass).

%---------------------------------------------------------------------------%

ensure_element_partition_id(Element, Id, !EqvClass) :-
    ElementMap = !.EqvClass ^ element_map,
    ( if map.search(ElementMap, Element, OldId) then
        Id = OldId
    else
        add_element(Element, Id, !EqvClass)
    ).

%---------------------------------------------------------------------------%

new_element(!.EqvClass, X) = !:EqvClass :-
    eqvclass.new_element(X, !EqvClass).

new_element(Element, !EqvClass) :-
    ElementMap0 = !.EqvClass ^ element_map,
    ( if map.search(ElementMap0, Element, _OldId) then
        unexpected($pred, "new element is already in equivalence class")
    else
        eqvclass.add_element(Element, _, !EqvClass)
    ).

:- pred eqvclass.add_element(T::in, partition_id::out,
    eqvclass(T)::in, eqvclass(T)::out) is det.

add_element(Element, Id, !EqvClass) :-
    !.EqvClass = eqvclass(Counter0, PartitionMap0, ElementMap0),
    counter.allocate(Id, Counter0, Counter),
    map.det_insert(Element, Id, ElementMap0, ElementMap),
    Partition = set.make_singleton_set(Element),
    map.det_insert(Id, Partition, PartitionMap0, PartitionMap),
    !:EqvClass = eqvclass(Counter, PartitionMap, ElementMap).

%---------------------------------------------------------------------------%

ensure_equivalence(!.EqvClass, X, Y) = !:EqvClass :-
    ensure_equivalence(X, Y, !EqvClass).

ensure_equivalence(ElementA, ElementB, EqvClass0, EqvClass) :-
    % The following code is logically equivalent to this code:
    %
    % eqvclass.ensure_equivalence(ElementA, ElementB, EqvClass0, EqvClass) :-
    %     eqvclass.ensure_element_2(ElementA, IdA, EqvClass0, EqvClass1),
    %     eqvclass.ensure_element_2(ElementB, IdB, EqvClass1, EqvClass2),
    %     ( if IdA = IdB then
    %         EqvClass = EqvClass2
    %     else
    %         eqvclass.add_equivalence(IdA, IdB, EqvClass2, EqvClass)
    %     ).
    %
    % However, the above code allocates significantly more memory than the code
    % below, because it can create an equivalence class for an element and then
    % just throw that equivalence class away.
    ElementMap0 = EqvClass0 ^ element_map,
    ( if map.search(ElementMap0, ElementA, IdA) then
        ( if map.search(ElementMap0, ElementB, IdB) then
            ( if IdA = IdB then
                EqvClass = EqvClass0
            else
                add_equivalence(IdA, IdB, EqvClass0, EqvClass)
            )
        else
            PartitionMap0 = EqvClass0 ^ partition_map,
            map.lookup(PartitionMap0, IdA, PartitionA),
            set.insert(ElementB, PartitionA, Partition),
            map.det_update(IdA, Partition, PartitionMap0, PartitionMap),
            map.det_insert(ElementB, IdA, ElementMap0, ElementMap),
            Counter0 = EqvClass0 ^ next_id_counter,
            EqvClass = eqvclass(Counter0, PartitionMap, ElementMap)
        )
    else
        ( if map.search(ElementMap0, ElementB, IdB) then
            PartitionMap0 = EqvClass0 ^ partition_map,
            map.lookup(PartitionMap0, IdB, PartitionB),
            set.insert(ElementA, PartitionB, Partition),
            map.det_update(IdB, Partition, PartitionMap0, PartitionMap),
            map.det_insert(ElementA, IdB, ElementMap0, ElementMap),
            Counter0 = EqvClass0 ^ next_id_counter,
            EqvClass = eqvclass(Counter0, PartitionMap, ElementMap)
        else
            Counter0 = EqvClass0 ^ next_id_counter,
            counter.allocate(Id, Counter0, Counter),
            map.det_insert(ElementA, Id, ElementMap0, ElementMap1),
            % We cannot call map.det_insert for ElementB, since it may be
            % that ElementA = ElementB.
            map.set(ElementB, Id, ElementMap1, ElementMap),
            PartitionMap0 = EqvClass0 ^ partition_map,
            set.list_to_set([ElementA, ElementB], Partition),
            map.det_insert(Id, Partition, PartitionMap0, PartitionMap),
            EqvClass = eqvclass(Counter, PartitionMap, ElementMap)
        )
    ).

ensure_corresponding_equivalences(L1, L2, EqvClass0) = EqvClass :-
    ensure_corresponding_equivalences(L1, L2, EqvClass0, EqvClass).

ensure_corresponding_equivalences([], [], !EqvClass).
ensure_corresponding_equivalences([], [_ | _], !EqvClass) :-
    unexpected($pred, "list length mismatch").
ensure_corresponding_equivalences([_ | _], [], !EqvClass) :-
    unexpected($pred, "list length mismatch").
ensure_corresponding_equivalences([H1 | T1], [H2 | T2], !EqvClass) :-
    ensure_equivalence(H1, H2, !EqvClass),
    ensure_corresponding_equivalences(T1, T2, !EqvClass).

%---------------------------------------------------------------------------%

new_equivalence(!.EqvClass, X, Y) = !:EqvClass :-
    new_equivalence(X, Y, !EqvClass).

new_equivalence(ElementA, ElementB, EqvClass0, EqvClass) :-
    % This code is the same as ensure_equivalence, with the exception that
    % we abort if IdA = IdB in EqvClass0.

    ElementMap0 = EqvClass0 ^ element_map,
    ( if map.search(ElementMap0, ElementA, IdA) then
        ( if map.search(ElementMap0, ElementB, IdB) then
            ( if IdA = IdB then
                unexpected($pred, "the two elements are already equivalent")
            else
                add_equivalence(IdA, IdB, EqvClass0, EqvClass)
            )
        else
            PartitionMap0 = EqvClass0 ^ partition_map,
            map.lookup(PartitionMap0, IdA, PartitionA),
            set.insert(ElementB, PartitionA, Partition),
            map.det_update(IdA, Partition, PartitionMap0, PartitionMap),
            map.det_insert(ElementB, IdA, ElementMap0, ElementMap),
            Counter0 = EqvClass0 ^ next_id_counter,
            EqvClass = eqvclass(Counter0, PartitionMap, ElementMap)
        )
    else
        ( if map.search(ElementMap0, ElementB, IdB) then
            PartitionMap0 = EqvClass0 ^ partition_map,
            map.lookup(PartitionMap0, IdB, PartitionB),
            set.insert(ElementA, PartitionB, Partition),
            map.det_update(IdB, Partition, PartitionMap0, PartitionMap),
            map.det_insert(ElementA, IdB, ElementMap0, ElementMap),
            Counter0 = EqvClass0 ^ next_id_counter,
            EqvClass = eqvclass(Counter0, PartitionMap, ElementMap)
        else
            Counter0 = EqvClass0 ^ next_id_counter,
            counter.allocate(Id, Counter0, Counter),
            map.det_insert(ElementA, Id, ElementMap0, ElementMap1),
            map.det_insert(ElementB, Id, ElementMap1, ElementMap),
            PartitionMap0 = EqvClass0 ^ partition_map,
            set.list_to_set([ElementA, ElementB], Partition),
            map.det_insert(Id, Partition, PartitionMap0, PartitionMap),
            EqvClass = eqvclass(Counter, PartitionMap, ElementMap)
        )
    ).

:- pred add_equivalence(partition_id::in, partition_id::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

add_equivalence(IdA, IdB, EqvClass0, EqvClass) :-
    EqvClass0 = eqvclass(Counter0, PartitionMap0, ElementMap0),
    map.lookup(PartitionMap0, IdA, PartitionA),
    map.lookup(PartitionMap0, IdB, PartitionB),
    % We want change_partition to loop over the smaller set.
    ( if set.count(PartitionA) < set.count(PartitionB) then
        map.delete(IdA, PartitionMap0, PartitionMap1),
        set.union(PartitionB, PartitionA, Partition),
        map.set(IdB, Partition, PartitionMap1, PartitionMap),
        set.to_sorted_list(PartitionA, ElementsA),
        change_partition(ElementsA, IdB, ElementMap0, ElementMap)
    else
        map.delete(IdB, PartitionMap0, PartitionMap1),
        set.union(PartitionA, PartitionB, Partition),
        map.set(IdA, Partition, PartitionMap1, PartitionMap),
        set.to_sorted_list(PartitionB, ElementsB),
        change_partition(ElementsB, IdA, ElementMap0, ElementMap)
    ),
    EqvClass = eqvclass(Counter0, PartitionMap, ElementMap).

:- pred change_partition(list(T)::in, partition_id::in,
    map(T, partition_id)::in, map(T, partition_id)::out) is det.

change_partition([], _Id, !ElementMap).
change_partition([Element | Elements], Id, !ElementMap) :-
    map.set(Element, Id, !ElementMap),
    change_partition(Elements, Id, !ElementMap).

%---------------------------------------------------------------------------%

same_eqvclass(EqvClass0, Element1, Element2) :-
    ElementMap0 = EqvClass0 ^ element_map,
    map.search(ElementMap0, Element1, Id1),
    map.search(ElementMap0, Element2, Id2),
    Id1 = Id2.

same_eqvclass_list(_, []).
same_eqvclass_list(EqvClass, [Element | Elements]) :-
    ElementMap = EqvClass ^ element_map,
    map.search(ElementMap, Element, Id),
    same_eqvclass_list_2(ElementMap, Elements, Id).

:- pred same_eqvclass_list_2(map(T, partition_id)::in,
    list(T)::in, partition_id::in) is semidet.

same_eqvclass_list_2(_, [], _).
same_eqvclass_list_2(ElementMap, [Element | Elements], Id) :-
    map.search(ElementMap, Element, Id),
    same_eqvclass_list_2(ElementMap, Elements, Id).

%---------------------------------------------------------------------------%

partition_set(EqvClass) = PartitionSet :-
    partition_set(EqvClass, PartitionSet).

partition_set(EqvClass, PartitionSet) :-
    partition_ids(EqvClass, Ids),
    partitions(EqvClass, Ids, PartitionList),
    set.list_to_set(PartitionList, PartitionSet).

partition_list(EqvClass) = PartitionList :-
    partition_list(EqvClass, PartitionList).

partition_list(EqvClass, PartitionList) :-
    partition_ids(EqvClass, Ids),
    partitions(EqvClass, Ids, PartitionList).

    % Convert a list of partition ids to a list of partitions.
    %
:- pred partitions(eqvclass(T)::in, list(partition_id)::in,
    list(set(T))::out) is det.

partitions(_EqvClass0, [], []).
partitions(EqvClass0, [Id | Ids], [Partition | Partitions]) :-
    id_to_partition(EqvClass0, Id, Partition),
    partitions(EqvClass0, Ids, Partitions).

    % Get the ids of all the partitions.
    %
:- pred partition_ids(eqvclass(T)::in, list(partition_id)::out) is det.

partition_ids(EqvClass0, Ids) :-
    PartitionMap0 = EqvClass0 ^ partition_map,
    map.keys(PartitionMap0, Ids).

    % Given a partition id, get the elements of the partition.
    %
:- pred id_to_partition(eqvclass(T)::in, partition_id::in, set(T)::out) is det.

id_to_partition(EqvClass0, Id, Partition) :-
    PartitionMap0 = EqvClass0 ^ partition_map,
    ( if map.search(PartitionMap0, Id, PartitionPrime) then
        Partition = PartitionPrime
    else
        unexpected($pred, "partition id not known to equivalence class")
    ).

%---------------------------------------------------------------------------%

partition_set_to_eqvclass(Set) = EqvClass :-
    partition_set_to_eqvclass(Set, EqvClass).

partition_set_to_eqvclass(SetSet, EqvClass) :-
    set.to_sorted_list(SetSet, ListSet),
    partition_list_to_eqvclass(ListSet, EqvClass).

partition_list_to_eqvclass(Xs) = EqvClass :-
    partition_list_to_eqvclass(Xs, EqvClass).

partition_list_to_eqvclass([], EqvClass) :-
    eqvclass.init(EqvClass).
partition_list_to_eqvclass([Partition | Partitions], EqvClass) :-
    % XXX This is not tail recursive.
    partition_list_to_eqvclass(Partitions, EqvClass0),
    EqvClass0 = eqvclass(Counter0, PartitionMap0, ElementMap0),
    set.to_sorted_list(Partition, Elements),
    (
        Elements = [],
        EqvClass = EqvClass0
    ;
        Elements = [_ | _],
        counter.allocate(Id, Counter0, Counter),
        record_partition_for_elements(Id, Elements, ElementMap0, ElementMap),
        map.det_insert(Id, Partition, PartitionMap0, PartitionMap),
        EqvClass = eqvclass(Counter, PartitionMap, ElementMap)
    ).

:- pred record_partition_for_elements(partition_id::in, list(T)::in,
    map(T, partition_id)::in, map(T, partition_id)::out) is det.

record_partition_for_elements(_Id, [], !ElementMap).
record_partition_for_elements(Id, [Element | Elements], !ElementMap) :-
    map.det_insert(Element, Id, !ElementMap),
    record_partition_for_elements(Id, Elements, !ElementMap).

%---------------------------------------------------------------------------%

get_equivalent_elements(EqvClass, X) = EqvElements :-
    EqvClass = eqvclass(_, PartitionMap, ElementMap),
    ( if
        map.search(ElementMap, X, PartitionOfX),
        map.search(PartitionMap, PartitionOfX, ElementsInPartitionOfX)
    then
        EqvElements = ElementsInPartitionOfX
    else
        EqvElements = set.make_singleton_set(X)
    ).

get_minimum_element(EqvClass, X) = MinElement :-
    EqvElements = get_equivalent_elements(EqvClass, X),
    MinElement = list.det_head(set.to_sorted_list(EqvElements)).

%---------------------------------------------------------------------------%

remove_equivalent_elements(EqvClass0, X) = EqvClass :-
    remove_equivalent_elements(X, EqvClass0, EqvClass).

remove_equivalent_elements(X, !EqvClass) :-
    !.EqvClass = eqvclass(Counter, PartitionMap0, ElementMap0),
    ( if map.search(ElementMap0, X, PartitionOfX) then
        map.det_remove(PartitionOfX, EqvsOfX, PartitionMap0, PartitionMap),
        map.delete_list(set.to_sorted_list(EqvsOfX), ElementMap0, ElementMap),
        !:EqvClass = eqvclass(Counter, PartitionMap, ElementMap)
    else
        true
    ).

%---------------------------------------------------------------------------%

divide_equivalence_classes(DivideFunc, EqvClass0) = EqvClass :-
    EqvClass0 = eqvclass(Counter0, PartitionMap0, ElementMap0),
    map.foldl3(divide_equivalence_classes_2(DivideFunc), PartitionMap0,
        Counter0, Counter, PartitionMap0, PartitionMap,
        ElementMap0, ElementMap),
    EqvClass = eqvclass(Counter, PartitionMap, ElementMap).

:- pred divide_equivalence_classes_2((func(T) = U)::in,
    partition_id::in, set(T)::in,
    counter::in, counter::out,
    map(partition_id, set(T))::in, map(partition_id, set(T))::out,
    map(T, partition_id)::in, map(T, partition_id)::out) is det.

divide_equivalence_classes_2(DivideFunc, Id, ItemSet,
        !Counter, !PartitionMap, !ElementMap) :-
    set.to_sorted_list(ItemSet, ItemList),
    (
        ItemList = [],
        unexpected($pred, "empty partition")
    ;
        ItemList = [Item | Items],
        MainValue = DivideFunc(Item),
        DivValueMap1 = map.singleton(MainValue, Id),
        list.foldl4(divide_equivalence_classes_3(DivideFunc, Id), Items,
            DivValueMap1, _DivValueMap, !Counter, !PartitionMap, !ElementMap)
    ).

:- pred divide_equivalence_classes_3((func(T) = U)::in, partition_id::in,
    T::in, map(U, partition_id)::in, map(U, partition_id)::out,
    counter::in, counter::out,
    map(partition_id, set(T))::in, map(partition_id, set(T))::out,
    map(T, partition_id)::in, map(T, partition_id)::out) is det.

divide_equivalence_classes_3(DivideFunc, MainId, Item, !DivValueMap,
        !Counter, !PartitionMap, !ElementMap) :-
    Value = DivideFunc(Item),
    ( if map.search(!.DivValueMap, Value, Id) then
        ( if Id = MainId then
            true
        else
            map.lookup(!.PartitionMap, MainId, MainSet0),
            set.delete(Item, MainSet0, MainSet),
            map.det_update(MainId, MainSet, !PartitionMap),

            map.lookup(!.PartitionMap, Id, Set0),
            set.insert(Item, Set0, Set),
            map.det_update(Id, Set, !PartitionMap),

            map.det_update(Item, Id, !ElementMap)
        )
    else
        counter.allocate(NewId, !Counter),
        map.det_insert(Value, NewId, !DivValueMap),

        map.lookup(!.PartitionMap, MainId, MainSet0),
        set.delete(Item, MainSet0, MainSet),
        map.det_update(MainId, MainSet, !PartitionMap),

        Set = set.make_singleton_set(Item),
        map.det_insert(NewId, Set, !PartitionMap),

        map.det_update(Item, NewId, !ElementMap)
    ).

%---------------------------------------------------------------------------%
:- end_module eqvclass.
%---------------------------------------------------------------------------%
