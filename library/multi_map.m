%---------------------------------------------------------------------------%
% Copyright (C) 1995, 1997, 2000, 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: multi_map.m.
% Main author: dylan.  Based on map.m, by fjh, conway.
% Stability: low.
%
% This file provides the 'multi_map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key,Data) pairs which allows you to look up any Data item given the
% Key.  A multi_map is similar, though allows a one to many relationship
% between keys and data.
%
% This is implemented almost as a special case of map.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module multi_map.
:- interface.
:- import_module map, list, assoc_list, set.

%-----------------------------------------------------------------------------%

:- type multi_map(Key, Data)	==	map(Key, list(Data)).

%-----------------------------------------------------------------------------%

	% Initialize an empty multi_map.
:- pred multi_map__init(multi_map(_,_)).
:- mode multi_map__init(uo) is det.

	% Check whether a multi_map is empty.
:- pred multi_map__is_empty(multi_map(_,_)).
:- mode multi_map__is_empty(in) is semidet.

	% Check whether multi_map contains key
:- pred multi_map__contains(multi_map(K,_V), K).
:- mode multi_map__contains(in, in) is semidet.

:- pred multi_map__member(multi_map(K,V), K, V).
:- mode multi_map__member(in, out, out) is nondet.

	% Search multi_map for given key.
:- pred multi_map__search(multi_map(K,V), K, list(V)).
:- mode multi_map__search(in, in, out) is semidet.

	% Search multi_map for given key.
:- pred multi_map__nondet_search(multi_map(K,V), K, V).
:- mode multi_map__nondet_search(in, in, out) is nondet.

	% Search multi_map for key, but abort if search fails.
:- pred multi_map__lookup(multi_map(K,V), K, list(V)).
:- mode multi_map__lookup(in, in, out) is det.

	% Search multi_map for key.
:- pred multi_map__nondet_lookup(multi_map(K,V), K, V).
:- mode multi_map__nondet_lookup(in, in, out) is nondet.

	% Search multi_map for data.
:- pred multi_map__inverse_search(multi_map(K,V), V, K).
:- mode multi_map__inverse_search(in, in, out) is nondet.

	% Insert a new key and corresponding value into a multi_map.
	% Fail if the key already exists.
:- pred multi_map__insert(multi_map(K,V), K, V, multi_map(K,V)).
:- mode multi_map__insert(in, in, in, out) is semidet.

	% Insert a new key and corresponding value into a multi_map.
	% Abort if the key already exists.
:- pred multi_map__det_insert(multi_map(K,V), K, V, multi_map(K,V)).
:- mode multi_map__det_insert(in, in, in, out) is det.

	% Update (add) the value corresponding to a given key
	% Fail if the key doesn't already exist.
:- pred multi_map__update(multi_map(K,V), K, V, multi_map(K,V)).
:- mode multi_map__update(in, in, in, out) is semidet.

	% Update (add) the value corresponding to a given key
	% Abort if the key doesn't already exist.
:- pred multi_map__det_update(multi_map(K,V), K, V, multi_map(K,V)).
:- mode multi_map__det_update(in, in, in, out) is det.

	% Update (replace) the value corresponding to a given key
	% Abort if the key doesn't already exist.
:- pred multi_map__det_replace(multi_map(K,V), K, list(V), multi_map(K,V)).
:- mode multi_map__det_replace(in, in, in, out) is det.

	% Update (add) value if the key is already present, otherwise
	% insert new key and value.
:- pred multi_map__set(multi_map(K,V), K, V, multi_map(K,V)).
:- mode multi_map__set(in, in, in, out) is det.

	% Given a multi_map, return a list of all the keys in the multi_map
:- pred multi_map__keys(multi_map(K, _V), list(K)).
:- mode multi_map__keys(in, out) is det.

	% Given a multi_map, return a list of all the data values in the
	% multi_map
:- pred multi_map__values(multi_map(_K, V), list(V)).
:- mode multi_map__values(in, out) is det.

	% convert a multi_map to an association list
:- pred multi_map__to_assoc_list(multi_map(K,V), assoc_list(K,list(V))).
:- mode multi_map__to_assoc_list(in, out) is det.

	% convert an association list to a multi_map
:- pred multi_map__from_assoc_list(assoc_list(K,list(V)), multi_map(K,V)).
:- mode multi_map__from_assoc_list(in, out) is det.

	% convert a sorted association list to a multi_map
:- pred multi_map__from_sorted_assoc_list(assoc_list(K, list(V)), 
			multi_map(K, V)).
:- mode multi_map__from_sorted_assoc_list(in, out) is det.

	% delete a key and data from a multi_map
	% if the key is not present, leave the multi_map unchanged
:- pred multi_map__delete(multi_map(K,V), K, multi_map(K,V)).
:- mode multi_map__delete(in, in, out) is det.

	% delete a data value from a key in a multi_map
	% if the key is not present, leave the multi_map unchanged
:- pred multi_map__delete(multi_map(K,V), K, V, multi_map(K,V)).
:- mode multi_map__delete(in, in, in, out) is det.

	% delete a key-value pair from a multi_map and return the value.
	% fail if the key is not present
:- pred multi_map__remove(multi_map(K,V), K, list(V), multi_map(K,V)).
:- mode multi_map__remove(in, in, out, out) is semidet.

	% delete a key-value pair from a multi_map and return the value.
	% abort if the key is not present
:- pred multi_map__det_remove(multi_map(K,V), K, list(V), multi_map(K,V)).
:- mode multi_map__det_remove(in, in, out, out) is det.

	% Count the number of elements (keys) in the multi_map.
:- pred multi_map__count(multi_map(K, V), int).
:- mode multi_map__count(in, out) is det.

	% Count the number of data elements in the multi_map.
:- pred multi_map__all_count(multi_map(K, V), int).
:- mode multi_map__all_count(in, out) is det.

	% Convert a pair of lists (which must be of the same length)
	% to a multi_map.
:- pred multi_map__from_corresponding_lists(list(K), list(V),
				multi_map(K, V)).
:- mode multi_map__from_corresponding_lists(in, in, out) is det.

	% Convert a pair of lists (which must be of the same length)
	% to a multi_map.
:- pred multi_map__from_corresponding_list_lists(list(K), list(list(V)),
				multi_map(K, V)).
:- mode multi_map__from_corresponding_list_lists(in, in, out) is det.

	% For multi_map__merge(MultiMapA, MultiMapB, MultiMap).
:- pred multi_map__merge(multi_map(K, V), multi_map(K, V), multi_map(K, V)).
:- mode multi_map__merge(in, in, out) is det.

	% multi_map__select takes a multi_map and a set of keys and returns
	% a multi_map containing the keys in the set and their corresponding
	% values.
:- pred multi_map__select(multi_map(K,V), set(K), multi_map(K,V)).
:- mode multi_map__select(in, in, out) is det.

	% Given a list of keys, produce a list of their values in a
	% specified multi_map.
:- pred multi_map__apply_to_list(list(K), multi_map(K, V), list(V)).
:- mode multi_map__apply_to_list(in, in, out) is det.

	% Declaratively, a NOP.
	% Operationally, a suggestion that the implemention
	% optimize the representation of the multi_map in the expectation
	% of a number of lookups but few or no modifications.
:- pred multi_map__optimize(multi_map(K, V), multi_map(K, V)).
:- mode multi_map__optimize(in, out) is det.

	% Remove the smallest item from the multi_map, fail if 
	% the multi_map is empty.
:- pred multi_map__remove_smallest(multi_map(K, V), K, list(V),
			multi_map(K, V)).
:- mode multi_map__remove_smallest(in, out, out, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module std_util, int, require.

%-----------------------------------------------------------------------------%

multi_map__init(M) :-
	map__init(M).

multi_map__is_empty(M) :-
	map__is_empty(M).

multi_map__contains(MultiMap, Key) :-
	map__search(MultiMap, Key, _).

multi_map__member(MultiMap, Key, Value) :-
	map__member(MultiMap, Key, ValueList),
	list__member(Value, ValueList).

multi_map__search(MultiMap, Key, Values) :-
	map__search(MultiMap, Key, Values).

multi_map__nondet_search(MultiMap, Key, Value) :-
	map__search(MultiMap, Key, Values),
	list__member(Value, Values).

multi_map__lookup(MultiMap, Key, Values) :-
	map__lookup(MultiMap, Key, Values).

multi_map__nondet_lookup(MultiMap, Key, Value) :-
	map__search(MultiMap, Key, Values),
	list__member(Value, Values).

multi_map__insert(MultiMap0, Key, Value, MultiMap) :-
	map__insert(MultiMap0, Key, [Value], MultiMap).
 
multi_map__det_insert(MultiMap0, Key, Value, MultiMap) :-
	map__det_insert(MultiMap0, Key, [Value], MultiMap).
 
multi_map__update(MultiMap0, Key, Value, MultiMap) :-
	map__search(MultiMap0, Key, Values0),
	Values = [Value|Values0],
	map__update(MultiMap0, Key, Values, MultiMap).

multi_map__det_update(MultiMap0, Key, Value, MultiMap) :-
	map__det_update(MultiMap0, Key, [Value], MultiMap).

multi_map__det_replace(MultiMap0, Key, Value, MultiMap) :-
	map__det_update(MultiMap0, Key, Value, MultiMap).

multi_map__set(MultiMap0, Key, Value, MultiMap) :-
	( map__search(MultiMap0, Key, Values0) ->
		Values = [Value|Values0],
		map__set(MultiMap0, Key, Values, MultiMap)
	;
		map__det_insert(MultiMap0, Key, [Value], MultiMap)
	).

multi_map__keys(MultiMap, KeyList) :-
	map__keys(MultiMap, KeyList).

multi_map__values(MultiMap, KeyList) :-
	map__values(MultiMap, KeyList0),
	list__condense(KeyList0, KeyList).

multi_map__to_assoc_list(MultiMap, AList) :-
	map__to_assoc_list(MultiMap, AList).

multi_map__from_assoc_list(AList, MultiMap) :-
	map__from_assoc_list(AList, MultiMap).

multi_map__from_sorted_assoc_list(AList, MultiMap) :-
	map__from_sorted_assoc_list(AList, MultiMap).

multi_map__delete(MultiMap0, Key, MultiMap) :-
	map__delete(MultiMap0, Key, MultiMap).

multi_map__delete(MultiMap0, Key, Value, MultiMap) :-
	(
		map__search(MultiMap0, Key, Values0), 
		list__delete_all(Values0, Value, Values)
	->	
		(
			Values = []
		->
			map__delete(MultiMap0, Key, MultiMap)
		;
			map__set(MultiMap0, Key, Values, MultiMap)
		)
	;
		MultiMap = MultiMap0
	).

multi_map__remove(MultiMap0, Key, Values, MultiMap) :-
	map__remove(MultiMap0, Key, Values, MultiMap).

multi_map__det_remove(MultiMap0, Key, Values, MultiMap) :-
	map__det_remove(MultiMap0, Key, Values, MultiMap).

multi_map__count(MultiMap, Count) :-
	map__count(MultiMap, Count).

multi_map__all_count(MultiMap, Count) :-
	multi_map__values(MultiMap, List),
	multi_map__count_list(List, 0, Count).

:- pred multi_map__count_list(list(A), int, int).
:- mode multi_map__count_list(in, in, out) is det.

multi_map__count_list([], X, X).
multi_map__count_list([_A|As], Count0, Count) :-
	Count1 is Count0 + 1,
	multi_map__count_list(As, Count1, Count).

%-----------------------------------------------------------------------------%

	% XXX inefficient

multi_map__inverse_search(MultiMap, Value, Key) :-
	map__to_assoc_list(MultiMap, AssocList),
	multi_map__assoc_list_member(Value, AssocList, Key).

:- pred multi_map__assoc_list_member(Value, assoc_list(Key, list(Value)), Key).
:- mode multi_map__assoc_list_member(in, in, out) is nondet.
multi_map__assoc_list_member(Value, [(AKey - AValues) | AList], Key) :-
	(
		list__member(Value, AValues),
		Key = AKey
	;
		multi_map__assoc_list_member(Value, AList, Key)
	).

%-----------------------------------------------------------------------------%

multi_map__from_corresponding_lists(Keys, Values, MultiMap) :-
	multi_map__init(MultiMap0),
	(
		multi_map__from_corresponding_lists_2(MultiMap0, Keys, Values,
			MultiMap1)
	->
		MultiMap = MultiMap1
	;
		error("multi_map__from_corresponding_lists: list length mismatch")
	).

:- pred multi_map__from_corresponding_lists_2(multi_map(K, V), list(K), list(V),
	multi_map(K, V)).
:- mode multi_map__from_corresponding_lists_2(in, in, in, out) is semidet.

multi_map__from_corresponding_lists_2(MultiMap, [], [], MultiMap).
multi_map__from_corresponding_lists_2(MultiMap0, [Key | Keys], [Value | Values],
	MultiMap) :-

	multi_map__set(MultiMap0, Key, Value, MultiMap1),
	multi_map__from_corresponding_lists_2(MultiMap1, Keys, Values,
		MultiMap).

multi_map__from_corresponding_list_lists(Keys, Values, MultiMap) :-
	map__from_corresponding_lists(Keys, Values, MultiMap).

%-----------------------------------------------------------------------------%

multi_map__merge(M0, M1, M) :-
	multi_map__to_assoc_list(M0, ML0),
	multi_map__to_assoc_list(M1, ML1),
	multi_map__assoc_list_merge(ML0, ML1, ML),
	multi_map__from_sorted_assoc_list(ML, M).

:- pred multi_map__assoc_list_merge(assoc_list(K, list(V)), 
		assoc_list(K, list(V)), assoc_list(K, list(V))).
:- mode multi_map__assoc_list_merge(in, in, out) is det.

multi_map__assoc_list_merge([], ListB, ListB).
multi_map__assoc_list_merge([A|ListA], [], [A|ListA]).
multi_map__assoc_list_merge([(KeyA - DataA) | ListA], [(KeyB - DataB) | ListB],
				[(Key - Data) | List]) :-
	compare(Res, KeyA, KeyB),
	(
		Res = (=),
		Key = KeyA,
		list__append(DataA, DataB, Data),
		multi_map__assoc_list_merge(ListA, ListB, List)
	;
		Res = (<),
		Key = KeyA,
		Data = DataA,
		multi_map__assoc_list_merge(ListA, [(KeyB - DataB) | ListB], 
				List)
	;
		Res = (>),
		Key = KeyB,
		Data = DataB,
		multi_map__assoc_list_merge([(KeyA - DataA) | ListA], ListB, 
				List)
	).

%-----------------------------------------------------------------------------%

multi_map__optimize(MultiMap0, MultiMap) :-
	map__optimize(MultiMap0, MultiMap).

%-----------------------------------------------------------------------------%

multi_map__select(Original, KeySet, NewMultiMap) :-
	map__select(Original, KeySet, NewMultiMap).

%-----------------------------------------------------------------------------%

multi_map__apply_to_list([], _, []).
multi_map__apply_to_list([K|Keys], MultiMap, Values) :-
	map__apply_to_list([K|Keys], MultiMap, Values0),
	list__condense(Values0, Values).

%-----------------------------------------------------------------------------%

multi_map__remove_smallest(MultiMap0, Key, Values, MultiMap) :-
	map__remove_smallest(MultiMap0, Key, Values, MultiMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
