%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- pred eqvclass__init(eqvclass(T)).
:- mode eqvclass__init(out) is det.

	% Is this item known to the equivalence class?

:- pred eqvclass__is_member(eqvclass(T), T).
:- mode eqvclass__is_member(in, in) is semidet.

	% Make an element known to the equivalence class.
	% The element may already be known to the class;
	% if it isn't, it is created without any equivalence relationships.

:- pred eqvclass__ensure_element(eqvclass(T), T, eqvclass(T)).
:- mode eqvclass__ensure_element(in, in, out) is det.

	% Make an element known to the equivalence class.
	% The element must not already be known to the class;
	% it is created without any equivalence relationships.

:- pred eqvclass__new_element(eqvclass(T), T, eqvclass(T)).
:- mode eqvclass__new_element(in, in, out) is det.

	% Make two elements of the equivalence class equivalent.
	% It is ok if they already are.

:- pred eqvclass__ensure_equivalence(eqvclass(T), T, T, eqvclass(T)).
:- mode eqvclass__ensure_equivalence(in, in, in, out) is det.

	% Make two elements of the equivalence class equivalent.
	% It is an error if they are already equivalent.

:- pred eqvclass__new_equivalence(eqvclass(T), T, T, eqvclass(T)).
:- mode eqvclass__new_equivalence(in, in, in, out) is det.

	% Test if two elements are equivalent.

:- pred eqvclass__same_eqvclass(eqvclass(T), T, T).
:- mode eqvclass__same_eqvclass(in, in, in) is semidet.

	% Return the set of the partitions of the equivalence class.

:- pred eqvclass__partition_set(eqvclass(T), set(set(T))).
:- mode eqvclass__partition_set(in, out) is det.

	% Return a list of the partitions of the equivalence class.

:- pred eqvclass__partition_list(eqvclass(T), list(set(T))).
:- mode eqvclass__partition_list(in, out) is det.

	% Create an equivalence class from a partition set.
	% It is an error if the sets are not disjoint.

:- pred eqvclass__partition_set_to_eqvclass(set(set(T)), eqvclass(T)).
:- mode eqvclass__partition_set_to_eqvclass(in, out) is det.

	% Create an equivalence class from a list of partitions.
	% It is an error if the sets are not disjoint.

:- pred eqvclass__partition_list_to_eqvclass(list(set(T)), eqvclass(T)).
:- mode eqvclass__partition_list_to_eqvclass(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module map, int, require, set.

:- type eqvclass(T)	--->
		eqvclass(
			partition_id,
			map(partition_id, set(T)),
			map(T, partition_id)
		).

:- type partition_id	==	int.

eqvclass__init(EqvClass) :-
	map__init(PartitionMap),
	map__init(ElementMap),
	EqvClass = eqvclass(0, PartitionMap, ElementMap).

eqvclass__is_member(EqvClass, Element) :-
	EqvClass = eqvclass(_NextId, _PartitionMap, ElementMap),
	map__search(ElementMap, Element, _).

eqvclass__ensure_element(EqvClass0, Element, EqvClass) :-
	eqvclass__ensure_element_2(EqvClass0, Element, _, EqvClass).

:- pred eqvclass__ensure_element_2(eqvclass(T), T, partition_id, eqvclass(T)).
:- mode eqvclass__ensure_element_2(in, in, out, out) is det.

eqvclass__ensure_element_2(EqvClass0, Element, Id, EqvClass) :-
	EqvClass0 = eqvclass(_NextId0, _PartitionMap0, ElementMap0),
	( map__search(ElementMap0, Element, OldId) ->
		EqvClass = EqvClass0,
		Id = OldId
	;
		eqvclass__add_element(EqvClass0, Element, Id, EqvClass)
	).

eqvclass__new_element(EqvClass0, Element, EqvClass) :-
	EqvClass0 = eqvclass(_NextId0, _PartitionMap0, ElementMap0),
	( map__search(ElementMap0, Element, _OldId) ->
		error("new element is already in equivalence class")
	;
		eqvclass__add_element(EqvClass0, Element, _, EqvClass)
	).

:- pred eqvclass__add_element(eqvclass(T), T, partition_id, eqvclass(T)).
:- mode eqvclass__add_element(in, in, out, out) is det.

eqvclass__add_element(EqvClass0, Element, Id, EqvClass) :-
	EqvClass0 = eqvclass(NextId0, PartitionMap0, ElementMap0),
	Id = NextId0,
	NextId is NextId0 + 1,
	map__det_insert(ElementMap0, Element, Id, ElementMap),
	set__singleton_set(Partition, Element),
	map__det_insert(PartitionMap0, NextId0, Partition, PartitionMap),
	EqvClass = eqvclass(NextId, PartitionMap, ElementMap).

eqvclass__ensure_equivalence(EqvClass0, Element1, Element2, EqvClass) :-
	eqvclass__ensure_element_2(EqvClass0, Element1, Id1, EqvClass1),
	eqvclass__ensure_element_2(EqvClass1, Element2, Id2, EqvClass2),
	( Id1 = Id2 ->
		EqvClass = EqvClass2
	;
		eqvclass__add_equivalence(EqvClass2, Id1, Id2, EqvClass)
	).

eqvclass__new_equivalence(EqvClass0, Element1, Element2, EqvClass) :-
	eqvclass__ensure_element_2(EqvClass0, Element1, Id1, EqvClass1),
	eqvclass__ensure_element_2(EqvClass1, Element2, Id2, EqvClass2),
	( Id1 = Id2 ->
		error("two elements are already equivalent")
	;
		eqvclass__add_equivalence(EqvClass2, Id1, Id2, EqvClass)
	).

:- pred eqvclass__add_equivalence(eqvclass(T), partition_id, partition_id,
	eqvclass(T)).
:- mode eqvclass__add_equivalence(in, in, in, out) is det.

eqvclass__add_equivalence(EqvClass0, Id1, Id2, EqvClass) :-
	EqvClass0 = eqvclass(NextId0, PartitionMap0, ElementMap0),
	map__det_remove(PartitionMap0, Id2, Partition2, PartitionMap1),
	map__lookup(PartitionMap1, Id1, Partition1),
	set__union(Partition1, Partition2, Partition),
	map__set(PartitionMap1, Id1, Partition, PartitionMap),
	set__to_sorted_list(Partition2, Elements2),
	eqvclass__change_partition(Elements2, Id1, ElementMap0, ElementMap),
	EqvClass = eqvclass(NextId0, PartitionMap, ElementMap).

:- pred eqvclass__change_partition(list(T), partition_id,
	map(T, partition_id), map(T, partition_id)).
:- mode eqvclass__change_partition(in, in, in, out) is det.

eqvclass__change_partition([], _Id, ElementMap, ElementMap).
eqvclass__change_partition([Element | Elements], Id, ElementMap0, ElementMap) :-
	map__set(ElementMap0, Element, Id, ElementMap1),
	eqvclass__change_partition(Elements, Id, ElementMap1, ElementMap).

eqvclass__same_eqvclass(EqvClass0, Element1, Element2) :-
	EqvClass0 = eqvclass(_NextId0, _PartitionMap0, ElementMap0),
	map__search(ElementMap0, Element1, Id1),
	map__search(ElementMap0, Element2, Id2),
	Id1 = Id2.

eqvclass__partition_set(EqvClass0, PartitionSet) :-
	eqvclass__partition_ids(EqvClass0, Ids),
	eqvclass__partitions(EqvClass0, Ids, PartitionList),
	set__list_to_set(PartitionList, PartitionSet).

eqvclass__partition_list(EqvClass0, PartitionList) :-
	eqvclass__partition_ids(EqvClass0, Ids),
	eqvclass__partitions(EqvClass0, Ids, PartitionList).

	% Convert a list of partition ids to a list of partitions.

:- pred eqvclass__partitions(eqvclass(T), list(partition_id), list(set(T))).
:- mode eqvclass__partitions(in, in, out) is det.

eqvclass__partitions(_EqvClass0, [], []).
eqvclass__partitions(EqvClass0, [Id | Ids], [Partition | Partitions]) :-
	eqvclass__id_to_partition(EqvClass0, Id, Partition),
	eqvclass__partitions(EqvClass0, Ids, Partitions).

	% Get the ids of all the partitions.

:- pred eqvclass__partition_ids(eqvclass(T), list(partition_id)).
:- mode eqvclass__partition_ids(in, out) is det.

eqvclass__partition_ids(EqvClass0, Ids) :-
	EqvClass0 = eqvclass(_NextId0, PartitionMap0, _ElementMap0),
	map__keys(PartitionMap0, Ids).

	% Given an element, get the id of the partition containing that element.

:- pred eqvclass__find_partition(eqvclass(T), T, partition_id).
:- mode eqvclass__find_partition(in, in, out) is det.

eqvclass__find_partition(EqvClass0, Element, Id) :-
	EqvClass0 = eqvclass(_NextId0, _PartitionMap0, ElementMap0),
	( map__search(ElementMap0, Element, IdPrime) ->
		Id = IdPrime
	;
		error("element not known to equivalence class")
	).

	% Given a partition id, get the elements of the partition.

:- pred eqvclass__id_to_partition(eqvclass(T), partition_id, set(T)).
:- mode eqvclass__id_to_partition(in, in, out) is det.

eqvclass__id_to_partition(EqvClass0, Id, Partition) :-
	EqvClass0 = eqvclass(_NextId0, PartitionMap0, _ElementMap0),
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
	EqvClass0 = eqvclass(NextId0, PartitionMap0, ElementMap0),
	set__to_sorted_list(Partition, Elements),
	( Elements = [] ->
	    NextId = NextId0,
	    ElementMap0 = ElementMap,
	    PartitionMap0 = PartitionMap
	;
	    Id = NextId0,
	    NextId is NextId0 + 1,
	    eqvclass__make_partition(Elements, Id, ElementMap0, ElementMap),
	    map__det_insert(PartitionMap0, Id, Partition, PartitionMap)
	),
	EqvClass = eqvclass(NextId, PartitionMap, ElementMap).

:- pred eqvclass__make_partition(list(T), partition_id,
	map(T, partition_id), map(T, partition_id)).
:- mode eqvclass__make_partition(in, in, in, out) is det.

eqvclass__make_partition([], _Id, ElementMap, ElementMap).
eqvclass__make_partition([Element | Elements], Id, ElementMap0, ElementMap) :-
	map__det_insert(ElementMap0, Element, Id, ElementMap1),
	eqvclass__make_partition(Elements, Id, ElementMap1, ElementMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
