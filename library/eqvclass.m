%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: eqvclass.m.
% Author: zs.
% Stability: low.
%
% A module for handling equivalence classes.
%
%---------------------------------------------------------------------------%

:- module eqvclass.

:- interface.

:- import_module set, list.

:- type eqvclass(T).
:- type partition_id.

    % Create an empty equivalance class.
    %
:- func eqvclass__init = eqvclass(T).
:- pred eqvclass__init(eqvclass(T)::out) is det.

    % Is this item known to the equivalence class?
    %
:- pred eqvclass__is_member(eqvclass(T)::in, T::in) is semidet.

    % If this item is known to the equivalence class, return the id of its
    % partition. The only use that the caller can make of the partition id 
    % is to check whether two items in the same equivalence calls have the
    % same partition id; that test will succeed if and only if the two
    % items are in the same partition. Partition ids are not guaranteed
    % to stay the same as an eqvclass is updated, so such comparisons will
    % work only against the same eqvclass.
    %
    % If you want to check whether two items are in the same equivalence class,
    % using eqvclass__same_eqvclass is more expressive than calling
    % eqvclass__partition_id on both items and comparing the results.
    % However, if you want to perform this check on X and Y1, on X and Y2,
    % ... X and Yn, then calling eqvclass__partition_id on X just once and
    % comparing this with the partition_ids of the Yi will be more efficient.
    %
:- pred eqvclass__partition_id(eqvclass(T)::in, T::in, partition_id::out)
    is semidet.

    % Make this item known to the equivalence class if it isn't already,
    % and return the id of its partition. The same proviso applies with
    % respect to partition_ids as with eqvclass__partition_id.
    %
:- pred eqvclass__ensure_element_partition_id(T::in, partition_id::out,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Make an element known to the equivalence class.
    % The element may already be known to the class;
    % if it isn't, it is created without any equivalence relationships.
    %
:- func eqvclass__ensure_element(eqvclass(T), T) = eqvclass(T).
:- pred eqvclass__ensure_element(eqvclass(T)::in, T::in, eqvclass(T)::out)
    is det.

    % Make an element known to the equivalence class.
    % The element must not already be known to the class;
    % it is created without any equivalence relationships.
    %
:- func eqvclass__new_element(eqvclass(T), T) = eqvclass(T).
:- pred eqvclass__new_element(eqvclass(T)::in, T::in, eqvclass(T)::out) is det.

    % Make two elements of the equivalence class equivalent.
    % It is ok if they already are.
    %
:- func eqvclass__ensure_equivalence(eqvclass(T), T, T) = eqvclass(T).
:- pred eqvclass__ensure_equivalence(eqvclass(T)::in, T::in, T::in,
    eqvclass(T)::out) is det.

:- func eqvclass__ensure_corresponding_equivalences(list(T), list(T),
    eqvclass(T)) = eqvclass(T).
:- pred eqvclass__ensure_corresponding_equivalences(list(T)::in, list(T)::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

    % Make two elements of the equivalence class equivalent.
    % It is an error if they are already equivalent.
    %
:- func eqvclass__new_equivalence(eqvclass(T), T, T) = eqvclass(T).
:- pred eqvclass__new_equivalence(eqvclass(T)::in, T::in, T::in,
    eqvclass(T)::out) is det.

    % Test if two elements are equivalent.
    %
:- pred eqvclass__same_eqvclass(eqvclass(T)::in, T::in, T::in) is semidet.

    % Test if a list of elements are equivalent.
    %
:- pred eqvclass__same_eqvclass_list(eqvclass(T)::in, list(T)::in) is semidet.

    % Return the set of the partitions of the equivalence class.
    %
:- func eqvclass__partition_set(eqvclass(T)) = set(set(T)).
:- pred eqvclass__partition_set(eqvclass(T)::in, set(set(T))::out) is det.

    % Return a list of the partitions of the equivalence class.
    %
:- func eqvclass__partition_list(eqvclass(T)) = list(set(T)).
:- pred eqvclass__partition_list(eqvclass(T)::in, list(set(T))::out) is det.

    % Create an equivalence class from a partition set.
    % It is an error if the sets are not disjoint.
    %
:- func eqvclass__partition_set_to_eqvclass(set(set(T))) = eqvclass(T).
:- pred eqvclass__partition_set_to_eqvclass(set(set(T))::in, eqvclass(T)::out)
    is det.

    % Create an equivalence class from a list of partitions.
    % It is an error if the sets are not disjoint.
    %
:- func eqvclass__partition_list_to_eqvclass(list(set(T))) = eqvclass(T).
:- pred eqvclass__partition_list_to_eqvclass(list(set(T))::in,
    eqvclass(T)::out) is det.

    % Return the set of elements equivalent to the given element.
    % This set will of course include the given element.
    %
:- func eqvclass__get_equivalent_elements(eqvclass(T), T) = set(T).

    % Return the smallest element equivalent to the given element.
    % This may or may not be the given element.
    %
:- func eqvclass__get_minimum_element(eqvclass(T), T) = T.

    % Remove the given element and all other elements equivalent to it
    % from the given equivalence class.
    %
:- func eqvclass__remove_equivalent_elements(eqvclass(T), T) = eqvclass(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, counter, require, map, svmap, set.

:- type eqvclass(T)
    --->    eqvclass(
                next_id     :: counter,
                partitions  :: map(partition_id, set(T)),
                keys        :: map(T, partition_id)
            ).

:- type partition_id    ==  int.

eqvclass__init(EqvClass) :-
    map__init(PartitionMap),
    map__init(ElementMap),
    EqvClass = eqvclass(counter__init(0), PartitionMap, ElementMap).

eqvclass__is_member(EqvClass, Element) :-
    ElementMap = EqvClass ^ keys,
    map__search(ElementMap, Element, _).

eqvclass__partition_id(EqvClass, Element, PartitionId) :-
    ElementMap = EqvClass ^ keys,
    map__search(ElementMap, Element, PartitionId).

eqvclass__ensure_element(EqvClass0, Element, EqvClass) :-
    eqvclass__ensure_element_partition_id(Element, _, EqvClass0, EqvClass).

eqvclass__ensure_element_partition_id(Element, Id, !EqvClass) :-
    ElementMap = !.EqvClass ^ keys,
    ( map__search(ElementMap, Element, OldId) ->
        Id = OldId
    ;
        eqvclass__add_element(Element, Id, !EqvClass)
    ).

eqvclass__new_element(EqvClass0, Element, EqvClass) :-
    ElementMap0 = EqvClass0 ^ keys,
    ( map__search(ElementMap0, Element, _OldId) ->
        error("new element is already in equivalence class")
    ;
        eqvclass__add_element(Element, _, EqvClass0, EqvClass)
    ).

:- pred eqvclass__add_element(T::in, partition_id::out,
    eqvclass(T)::in, eqvclass(T)::out) is det.

eqvclass__add_element(Element, Id, !EqvClass) :-
    !.EqvClass = eqvclass(Counter0, PartitionMap0, ElementMap0),
    counter__allocate(Id, Counter0, Counter),
    map__det_insert(ElementMap0, Element, Id, ElementMap),
    set__singleton_set(Partition, Element),
    map__det_insert(PartitionMap0, Id, Partition, PartitionMap),
    !:EqvClass = eqvclass(Counter, PartitionMap, ElementMap).

    % The following code is logically equivalent to this code:
    %
    % eqvclass__ensure_equivalence(EqvClass0, ElementA, ElementB, EqvClass) :-
    %     eqvclass__ensure_element_2(ElementA, IdA, EqvClass0, EqvClass1),
    %     eqvclass__ensure_element_2(ElementB, IdB, EqvClass1, EqvClass2),
    %     ( IdA = IdB ->
    %         EqvClass = EqvClass2
    %     ;
    %         eqvclass__add_equivalence(IdA, IdB, EqvClass2, EqvClass)
    %     ).
    %
    % However, the above code allocates significantly more memory than the code
    % below, because it can create an equivalence class for an element and then
    % just throw that equivalence class away.

eqvclass__ensure_equivalence(EqvClass0, ElementA, ElementB, EqvClass) :-
    ElementMap0 = EqvClass0 ^ keys,
    ( map__search(ElementMap0, ElementA, IdA) ->
        ( map__search(ElementMap0, ElementB, IdB) ->
            ( IdA = IdB ->
                EqvClass = EqvClass0
            ;
                eqvclass__add_equivalence(IdA, IdB, EqvClass0, EqvClass)
            )
        ;
            PartitionMap0 = EqvClass0 ^ partitions,
            map__lookup(PartitionMap0, IdA, PartitionA),
            set__insert(PartitionA, ElementB, Partition),
            map__det_update(PartitionMap0, IdA, Partition, PartitionMap),
            map__det_insert(ElementMap0, ElementB, IdA, ElementMap),
            NextId0 = EqvClass0 ^ next_id,
            EqvClass = eqvclass(NextId0, PartitionMap, ElementMap)
        )
    ;
        ( map__search(ElementMap0, ElementB, IdB) ->
            PartitionMap0 = EqvClass0 ^ partitions,
            map__lookup(PartitionMap0, IdB, PartitionB),
            set__insert(PartitionB, ElementA, Partition),
            map__det_update(PartitionMap0, IdB, Partition, PartitionMap),
            map__det_insert(ElementMap0, ElementA, IdB, ElementMap),
            NextId0 = EqvClass0 ^ next_id,
            EqvClass = eqvclass(NextId0, PartitionMap, ElementMap)
        ;
            NextId0 = EqvClass0 ^ next_id,
            counter__allocate(Id, NextId0, NextId),
            map__det_insert(ElementMap0, ElementA, Id, ElementMap1),
            map__det_insert(ElementMap1, ElementB, Id, ElementMap),
            PartitionMap0 = EqvClass0 ^ partitions,
            set__list_to_set([ElementA, ElementB], Partition),
            map__det_insert(PartitionMap0, Id, Partition, PartitionMap),
            EqvClass = eqvclass(NextId, PartitionMap, ElementMap)
        )
    ).

    % This code is the same as eqvclass__ensure_equivalence, with the
    % exception that we abort if IdA = IdB in EqvClass0.

eqvclass__new_equivalence(EqvClass0, ElementA, ElementB, EqvClass) :-
    ElementMap0 = EqvClass0 ^ keys,
    ( map__search(ElementMap0, ElementA, IdA) ->
        ( map__search(ElementMap0, ElementB, IdB) ->
            ( IdA = IdB ->
                error("two elements are already equivalent")
            ;
                eqvclass__add_equivalence(IdA, IdB, EqvClass0, EqvClass)
            )
        ;
            PartitionMap0 = EqvClass0 ^ partitions,
            map__lookup(PartitionMap0, IdA, PartitionA),
            set__insert(PartitionA, ElementB, Partition),
            map__det_update(PartitionMap0, IdA, Partition, PartitionMap),
            map__det_insert(ElementMap0, ElementB, IdA, ElementMap),
            NextId0 = EqvClass0 ^ next_id,
            EqvClass = eqvclass(NextId0, PartitionMap, ElementMap)
        )
    ;
        ( map__search(ElementMap0, ElementB, IdB) ->
            PartitionMap0 = EqvClass0 ^ partitions,
            map__lookup(PartitionMap0, IdB, PartitionB),
            set__insert(PartitionB, ElementA, Partition),
            map__det_update(PartitionMap0, IdB, Partition, PartitionMap),
            map__det_insert(ElementMap0, ElementA, IdB, ElementMap),
            NextId0 = EqvClass0 ^ next_id,
            EqvClass = eqvclass(NextId0, PartitionMap, ElementMap)
        ;
            NextId0 = EqvClass0 ^ next_id,
            counter__allocate(Id, NextId0, NextId),
            map__det_insert(ElementMap0, ElementA, Id, ElementMap1),
            map__det_insert(ElementMap1, ElementB, Id, ElementMap),
            PartitionMap0 = EqvClass0 ^ partitions,
            set__list_to_set([ElementA, ElementB], Partition),
            map__det_insert(PartitionMap0, Id, Partition, PartitionMap),
            EqvClass = eqvclass(NextId, PartitionMap, ElementMap)
        )
    ).

eqvclass__ensure_corresponding_equivalences([], [], !EqvClass).
eqvclass__ensure_corresponding_equivalences([], [_ | _], !EqvClass) :-
    error("eqvclass__ensure_corresponding_equivalences: list mismatch").
eqvclass__ensure_corresponding_equivalences([_ | _], [], !EqvClass) :-
    error("eqvclass__ensure_corresponding_equivalences: list mismatch").
eqvclass__ensure_corresponding_equivalences([H1 | T1], [H2 | T2], !EqvClass) :-
    eqvclass__ensure_equivalence(!.EqvClass, H1, H2, !:EqvClass),
    eqvclass__ensure_corresponding_equivalences(T1, T2, !EqvClass).

eqvclass__ensure_corresponding_equivalences(L1, L2, EqvClass0) = EqvClass :-
    eqvclass__ensure_corresponding_equivalences(L1, L2,
        EqvClass0, EqvClass).

:- pred eqvclass__add_equivalence(partition_id::in, partition_id::in,
    eqvclass(T)::in, eqvclass(T)::out) is det.

eqvclass__add_equivalence(IdA, IdB, EqvClass0, EqvClass) :-
    EqvClass0 = eqvclass(NextId0, PartitionMap0, ElementMap0),
    map__lookup(PartitionMap0, IdA, PartitionA),
    map__lookup(PartitionMap0, IdB, PartitionB),
    % We want eqvclass__change_partition to loop over the smaller set.
    ( set__count(PartitionA) < set__count(PartitionB) ->
        map__delete(PartitionMap0, IdA, PartitionMap1),
        set__union(PartitionB, PartitionA, Partition),
        map__set(PartitionMap1, IdB, Partition, PartitionMap),
        set__to_sorted_list(PartitionA, ElementsA),
        eqvclass__change_partition(ElementsA, IdB, ElementMap0, ElementMap)
    ;
        map__delete(PartitionMap0, IdB, PartitionMap1),
        set__union(PartitionA, PartitionB, Partition),
        map__set(PartitionMap1, IdA, Partition, PartitionMap),
        set__to_sorted_list(PartitionB, ElementsB),
        eqvclass__change_partition(ElementsB, IdA, ElementMap0, ElementMap)
    ),
    EqvClass = eqvclass(NextId0, PartitionMap, ElementMap).

:- pred eqvclass__change_partition(list(T)::in, partition_id::in,
    map(T, partition_id)::in, map(T, partition_id)::out) is det.

eqvclass__change_partition([], _Id, !ElementMap).
eqvclass__change_partition([Element | Elements], Id, !ElementMap) :-
    svmap__set(Element, Id, !ElementMap),
    eqvclass__change_partition(Elements, Id, !ElementMap).

eqvclass__same_eqvclass(EqvClass0, Element1, Element2) :-
    ElementMap0 = EqvClass0 ^ keys,
    map__search(ElementMap0, Element1, Id1),
    map__search(ElementMap0, Element2, Id2),
    Id1 = Id2.

eqvclass__same_eqvclass_list(_, []).
eqvclass__same_eqvclass_list(EqvClass, [Element | Elements]) :-
    ElementMap = EqvClass ^ keys,
    map__search(ElementMap, Element, Id),
    eqvclass__same_eqvclass_list_2(ElementMap, Elements, Id).

:- pred eqvclass__same_eqvclass_list_2(map(T, partition_id)::in, 
    list(T)::in, partition_id::in) is semidet.

eqvclass__same_eqvclass_list_2(_, [], _).
eqvclass__same_eqvclass_list_2(ElementMap, [Element | Elements], Id) :-
    map__search(ElementMap, Element, Id),
    eqvclass__same_eqvclass_list_2(ElementMap, Elements, Id).

eqvclass__partition_set(EqvClass0, PartitionSet) :-
    eqvclass__partition_ids(EqvClass0, Ids),
    eqvclass__partitions(EqvClass0, Ids, PartitionList),
    set__list_to_set(PartitionList, PartitionSet).

eqvclass__partition_list(EqvClass0, PartitionList) :-
    eqvclass__partition_ids(EqvClass0, Ids),
    eqvclass__partitions(EqvClass0, Ids, PartitionList).

    % Convert a list of partition ids to a list of partitions.

:- pred eqvclass__partitions(eqvclass(T)::in, list(partition_id)::in,
    list(set(T))::out) is det.

eqvclass__partitions(_EqvClass0, [], []).
eqvclass__partitions(EqvClass0, [Id | Ids], [Partition | Partitions]) :-
    eqvclass__id_to_partition(EqvClass0, Id, Partition),
    eqvclass__partitions(EqvClass0, Ids, Partitions).

    % Get the ids of all the partitions.

:- pred eqvclass__partition_ids(eqvclass(T)::in, list(partition_id)::out)
    is det.

eqvclass__partition_ids(EqvClass0, Ids) :-
    PartitionMap0 = EqvClass0 ^ partitions,
    map__keys(PartitionMap0, Ids).

    % Given a partition id, get the elements of the partition.

:- pred eqvclass__id_to_partition(eqvclass(T)::in, partition_id::in,
    set(T)::out) is det.

eqvclass__id_to_partition(EqvClass0, Id, Partition) :-
    PartitionMap0 = EqvClass0 ^ partitions,
    ( map__search(PartitionMap0, Id, PartitionPrime) ->
        Partition = PartitionPrime
    ;
        error("partition id not known to equivalence class")
    ).

%---------------------------------------------------------------------------%

eqvclass__partition_set_to_eqvclass(SetSet, EqvClass) :-
    set__to_sorted_list(SetSet, ListSet),
    eqvclass__partition_list_to_eqvclass(ListSet, EqvClass).

eqvclass__partition_list_to_eqvclass([], EqvClass) :-
    eqvclass__init(EqvClass).
eqvclass__partition_list_to_eqvclass([Partition | Ps], EqvClass) :-
    eqvclass__partition_list_to_eqvclass(Ps, EqvClass0),
    EqvClass0 = eqvclass(Counter0, PartitionMap0, ElementMap0),
    set__to_sorted_list(Partition, Elements),
    ( Elements = [] ->
        Counter = Counter0,
        ElementMap0 = ElementMap,
        PartitionMap0 = PartitionMap
    ;
        counter__allocate(Id, Counter0, Counter),
        eqvclass__make_partition(Elements, Id, ElementMap0, ElementMap),
        map__det_insert(PartitionMap0, Id, Partition, PartitionMap)
    ),
    EqvClass = eqvclass(Counter, PartitionMap, ElementMap).

:- pred eqvclass__make_partition(list(T)::in, partition_id::in,
    map(T, partition_id)::in, map(T, partition_id)::out) is det.

eqvclass__make_partition([], _Id, !ElementMap).
eqvclass__make_partition([Element | Elements], Id, !ElementMap) :-
    svmap__det_insert(Element, Id, !ElementMap),
    eqvclass__make_partition(Elements, Id, !ElementMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

eqvclass__init = EC :-
    eqvclass__init(EC).

eqvclass__ensure_element(EC1, X) = EC2 :-
    eqvclass__ensure_element(EC1, X, EC2).

eqvclass__new_element(EC1, X) = EC2 :-
    eqvclass__new_element(EC1, X, EC2).

eqvclass__ensure_equivalence(EC1, X, Y) = EC2 :-
    eqvclass__ensure_equivalence(EC1, X, Y, EC2).

eqvclass__new_equivalence(EC1, X, Y) = EC2 :-
    eqvclass__new_equivalence(EC1, X, Y, EC2).

eqvclass__partition_set(EC) = S :-
    eqvclass__partition_set(EC, S).

eqvclass__partition_list(EC) = Xs :-
    eqvclass__partition_list(EC, Xs).

eqvclass__partition_set_to_eqvclass(S) = EC :-
    eqvclass__partition_set_to_eqvclass(S, EC).

eqvclass__partition_list_to_eqvclass(Xs) = EC :-
    eqvclass__partition_list_to_eqvclass(Xs, EC).

eqvclass__get_equivalent_elements(eqvclass(_, PartitionMap, ElementMap), X) =
    ( Eqv = map__search(PartitionMap, map__search(ElementMap, X)) ->
        Eqv
    ;
        set__make_singleton_set(X)
    ).

eqvclass__get_minimum_element(EC, X) =
    list__det_head(set__to_sorted_list(
            eqvclass__get_equivalent_elements(EC, X))).

eqvclass__remove_equivalent_elements(eqvclass(Id, P0, E0), X) =
        eqvclass(Id, P, E) :-
    ( map__search(E0, X, Partition) ->
        map__det_remove(P0, Partition, Eq, P),
        map__delete_list(E0, set__to_sorted_list(Eq), E)
    ;
        P = P0,
        E = E0
    ).
