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

	% Create an empty equivalance class.

:- pred eqvclass__init(eqvclass(T)::out) is det.
:- func eqvclass__init = eqvclass(T).

	% Is this item known to the equivalence class?

:- pred eqvclass__is_member(eqvclass(T)::in, T::in) is semidet.

	% Make an element known to the equivalence class.
	% The element may already be known to the class;
	% if it isn't, it is created without any equivalence relationships.

:- pred eqvclass__ensure_element(eqvclass(T)::in, T::in, eqvclass(T)::out)
	is det.
:- func eqvclass__ensure_element(eqvclass(T), T) = eqvclass(T).

	% Make an element known to the equivalence class.
	% The element must not already be known to the class;
	% it is created without any equivalence relationships.

:- pred eqvclass__new_element(eqvclass(T)::in, T::in, eqvclass(T)::out) is det.
:- func eqvclass__new_element(eqvclass(T), T) = eqvclass(T).

	% Make two elements of the equivalence class equivalent.
	% It is ok if they already are.

:- pred eqvclass__ensure_equivalence(eqvclass(T)::in, T::in, T::in,
	eqvclass(T)::out) is det.
:- func eqvclass__ensure_equivalence(eqvclass(T), T, T) = eqvclass(T).

	% Make two elements of the equivalence class equivalent.
	% It is an error if they are already equivalent.

:- pred eqvclass__new_equivalence(eqvclass(T)::in, T::in, T::in,
	eqvclass(T)::out) is det.
:- func eqvclass__new_equivalence(eqvclass(T), T, T) = eqvclass(T).

	% Test if two elements are equivalent.

:- pred eqvclass__same_eqvclass(eqvclass(T)::in, T::in, T::in) is semidet.

	% Test if a list of elements are equivalent.

:- pred eqvclass__same_eqvclass_list(eqvclass(T)::in, list(T)::in) is semidet.

	% Return the set of the partitions of the equivalence class.

:- pred eqvclass__partition_set(eqvclass(T)::in, set(set(T))::out) is det.
:- func eqvclass__partition_set(eqvclass(T)) = set(set(T)).

	% Return a list of the partitions of the equivalence class.

:- pred eqvclass__partition_list(eqvclass(T)::in, list(set(T))::out) is det.
:- func eqvclass__partition_list(eqvclass(T)) = list(set(T)).

	% Create an equivalence class from a partition set.
	% It is an error if the sets are not disjoint.

:- pred eqvclass__partition_set_to_eqvclass(set(set(T))::in, eqvclass(T)::out)
	is det.
:- func eqvclass__partition_set_to_eqvclass(set(set(T))) = eqvclass(T).

	% Create an equivalence class from a list of partitions.
	% It is an error if the sets are not disjoint.

:- pred eqvclass__partition_list_to_eqvclass(list(set(T))::in,
	eqvclass(T)::out) is det.
:- func eqvclass__partition_list_to_eqvclass(list(set(T))) = eqvclass(T).

	% Return the set of elements equivalent to the given element.
	% This set will of course include the given element.

:- func eqvclass__get_equivalent_elements(eqvclass(T), T) = set(T).

	% Return the smallest element equivalent to the given element.
	% This may or may not be the given element.

:- func eqvclass__get_minimum_element(eqvclass(T), T) = T.

	% Remove the given element and all other elements equivalent to it
	% from the given equivalence class.

:- func eqvclass__remove_equivalent_elements(eqvclass(T), T) = eqvclass(T).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, counter, require, map, svmap, set.

:- type eqvclass(T)
	--->	eqvclass(
			next_id		:: counter,
			partitions	:: map(partition_id, set(T)),
			keys		:: map(T, partition_id)
		).

:- type partition_id	==	int.

eqvclass__init(EqvClass) :-
	map__init(PartitionMap),
	map__init(ElementMap),
	EqvClass = eqvclass(counter__init(0), PartitionMap, ElementMap).

eqvclass__is_member(EqvClass, Element) :-
	ElementMap = EqvClass ^ keys,
	map__search(ElementMap, Element, _).

eqvclass__ensure_element(EqvClass0, Element, EqvClass) :-
	eqvclass__ensure_element_2(Element, _, EqvClass0, EqvClass).

:- pred eqvclass__ensure_element_2(T::in, partition_id::out,
	eqvclass(T)::in, eqvclass(T)::out) is det.

eqvclass__ensure_element_2(Element, Id, !EqvClass) :-
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

eqvclass__ensure_equivalence(EqvClass0, Element1, Element2, EqvClass) :-
	eqvclass__ensure_element_2(Element1, Id1, EqvClass0, EqvClass1),
	eqvclass__ensure_element_2(Element2, Id2, EqvClass1, EqvClass2),
	( Id1 = Id2 ->
		EqvClass = EqvClass2
	;
		eqvclass__add_equivalence(Id1, Id2, EqvClass2, EqvClass)
	).

eqvclass__new_equivalence(EqvClass0, Element1, Element2, EqvClass) :-
	eqvclass__ensure_element_2(Element1, Id1, EqvClass0, EqvClass1),
	eqvclass__ensure_element_2(Element2, Id2, EqvClass1, EqvClass2),
	( Id1 = Id2 ->
		error("two elements are already equivalent")
	;
		eqvclass__add_equivalence(Id1, Id2, EqvClass2, EqvClass)
	).

:- pred eqvclass__add_equivalence(partition_id::in, partition_id::in,
	eqvclass(T)::in, eqvclass(T)::out) is det.

eqvclass__add_equivalence(Id1, Id2, EqvClass0, EqvClass) :-
	EqvClass0 = eqvclass(NextId0, PartitionMap0, ElementMap0),
	map__det_remove(PartitionMap0, Id2, Partition2, PartitionMap1),
	map__lookup(PartitionMap1, Id1, Partition1),
	set__union(Partition1, Partition2, Partition),
	map__set(PartitionMap1, Id1, Partition, PartitionMap),
	set__to_sorted_list(Partition2, Elements2),
	eqvclass__change_partition(Elements2, Id1, ElementMap0, ElementMap),
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
		eqvclass__make_partition(Elements, Id,
			ElementMap0, ElementMap),
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
% 	Function forms added.

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
