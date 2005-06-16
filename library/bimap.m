%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 1999, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: bimap.m.
% Main author: conway.
% Stability: medium.
%
% This file provides a bijective map ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key, Data) pairs which allows you to look up any Data item given the
% Key.  A bimap also allows you to look up the Key given the Data.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module bimap.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

:- type bimap(K, V).

%-----------------------------------------------------------------------------%

	% Initialize an empty bimap.
	%
:- func bimap__init = bimap(K, V).
:- pred bimap__init(bimap(K, V)::out) is det.

	% Check whether a bimap is empty.
	%	
:- pred bimap__is_empty(bimap(K, V)::in) is semidet.

	% Search the bimap.
	% The first mode searches for a value given a key and the second
	% mode searches for a key given a value.
	%
:- pred bimap__search(bimap(K, V), K, V).
:- mode bimap__search(in, in, out) is semidet.
:- mode bimap__search(in, out, in) is semidet.

	% Search the bimap for the value corresponding to a given key.
	%
:- func bimap__forward_search(bimap(K, V), K) = V is semidet.
:- pred bimap__forward_search(bimap(K, V)::in, K::in, V::out) is semidet.

	% Search the bimap for the key corresponding to the given value.
	%
:- func bimap__reverse_search(bimap(K, V), V) = K is semidet.
:- pred bimap__reverse_search(bimap(K, V)::in, K::out, V::in) is semidet.

	% Look up the value in the bimap corresponding to the given key.
	% Throws an exception if the key is not present in the bimap.
	%
:- func bimap__lookup(bimap(K, V), K) = V.
:- pred bimap__lookup(bimap(K, V)::in, K::in, V::out) is det.

	% Look up the key in the bimap corresponding to the given value.
	% Throws an exception if the value is not present in the bimap.
	%
:- func bimap__reverse_lookup(bimap(K, V), V) = K.
:- pred bimap__reverse_lookup(bimap(K, V)::in, K::out, V::in) is det.

	% Given a bimap, return a list of all the keys in the bimap.
	%
:- func bimap__ordinates(bimap(K, V)) = list(K).
:- pred bimap__ordinates(bimap(K, V)::in, list(K)::out) is det.

	% Given a bimap, return a list of all the data values in the bimap.
	%
:- func bimap__coordinates(bimap(K, V)) = list(V).
:- pred bimap__coordinates(bimap(K, V)::in, list(V)::out) is det.

	% Succeeds iff the bimap contains the given key.
	%
:- pred bimap__contains_key(bimap(K, V)::in, K::in) is semidet.

	% Succeeds iff the bimap contains the given value.
	%
:- pred bimap__contains_value(bimap(K, V)::in, V::in) is semidet.

	% Insert a new key-value pair into the bimap.
	% Fails if either the key or value already exists.
	%
:- func bimap__insert(bimap(K, V), K, V) = bimap(K, V) is semidet.
:- pred bimap__insert(bimap(K, V)::in, K::in, V::in, bimap(K, V)::out)
	is semidet.

	% As above but throws an exception if the key or value already
	% exists.
	%
:- func bimap__det_insert(bimap(K, V), K, V) = bimap(K, V).
:- pred bimap__det_insert(bimap(K, V)::in, K::in, V::in, bimap(K, V)::out)
	is det.

	% Update the key and value if already present, otherwise insert the
	% new key and value.
	%
	% NOTE: setting the key-value pair (K, V) will remove the
	%       key-value pairs (K, V1) and (K1, V) if they exist.
	%
:- func bimap__set(bimap(K, V), K, V) = bimap(K, V).
:- pred bimap__set(bimap(K, V)::in, K::in, V::in, bimap(K, V)::out) is det.

	% Insert key-value pairs from an association list into the given
	% bimap.  Fails if the contents of the association list and the
	% initial bimap do not implicitly form a bijection.
	%
:- func bimap__insert_from_assoc_list(assoc_list(K, V), bimap(K, V)) =
	bimap(K, V) is semidet.
:- pred bimap__insert_from_assoc_list(assoc_list(K, V)::in,
	bimap(K, V)::in, bimap(K, V)::out) is semidet.

	% As above but throws an exception if the association list and
	% initial bimap are not implicitly bijective.
	%
:- func bimap__det_insert_from_assoc_list(assoc_list(K, V), bimap(K, V))
	= bimap(K, V).
:- pred bimap__det_insert_from_assoc_list(assoc_list(K, V)::in,
	bimap(K, V)::in, bimap(K, V)::out) is det.

	% Insert key-value pairs from a pair of corresponding lists.
	% Throws an exception if the lists are not of equal lengths
	% or if they do not implicitly define a bijection.
	%
:- func bimap__det_insert_from_corresponding_lists(list(K), list(V),
	bimap(K, V)) = bimap(K, V).
:- pred bimap__det_insert_from_corresponding_lists(list(K)::in, list(V)::in,
	bimap(K, V)::in, bimap(K, V)::out) is det.

	% Apply bimap.set to each key-value pair in the association
	% list.  The key-value pairs from the association list may
	% update existing keys and values in the bimap.
	%
:- func bimap__set_from_assoc_list(assoc_list(K, V), bimap(K, V))
	= bimap(K, V).
:- pred bimap__set_from_assoc_list(assoc_list(K, V)::in,
	bimap(K, V)::in, bimap(K, V)::out) is det.

	% As above but with a pair of corresponding lists in place
	% of an association list.  Throws an exception if the lists
	% are not of equal length.
	%
:- func bimap__set_from_corresponding_lists(list(K), list(V),
	bimap(K, V)) = bimap(K, V).
:- pred bimap__set_from_corresponding_lists(list(K)::in, list(V)::in,
	bimap(K, V)::in, bimap(K, V)::out) is det.

	% Delete a key-value pair from a bimap. If the key is not present,
	% leave the bimap unchanged.
	%
:- func bimap__delete_key(bimap(K, V), K) = bimap(K, V).
:- pred bimap__delete_key(K::in, bimap(K, V)::in, bimap(K, V)::out) is det.

	% Delete a key-value pair from a bimap. If the value is not present,
	% leave the bimap unchanged.
	%
:- func bimap__delete_value(bimap(K, V), V) = bimap(K, V).
:- pred bimap__delete_value(V::in, bimap(K, V)::in, bimap(K, V)::out) is det.

	% Apply bimap.delete_key to a list of keys.
	%
:- func bimap__delete_keys(bimap(K, V), list(K)) = bimap(K, V).
:- pred bimap__delete_keys(list(K)::in, bimap(K, V)::in, bimap(K, V)::out)
	is det.

	% Apply bimap.delete_value to a list of values.
	%
:- func bimap__delete_values(bimap(K, V), list(V)) = bimap(K, V).
:- pred bimap__delete_values(list(V)::in, bimap(K, V)::in, bimap(K, V)::out)
	is det.

	% bimap.overlay(BIMapA, BIMapB, BIMap):
	% Apply map__overlay to the forward maps of BIMapA and BIMapB,
	% and compute the reverse map from the resulting map.
	%
:- func bimap__overlay(bimap(K, V), bimap(K, V)) = bimap(K, V).
:- pred bimap__overlay(bimap(K, V)::in, bimap(K, V)::in, bimap(K, V)::out)
	is det.

	% Convert a bimap to an association list.
	%
:- func bimap__to_assoc_list(bimap(K, V)) = assoc_list(K, V).
:- pred bimap__to_assoc_list(bimap(K, V)::in, assoc_list(K, V)::out) is det.

	% Convert an association list to a bimap.
	% Fails if the association list does not implicitly define
	% a bijection, i.e. a key or value occurs multiple times in the
	% association list.
	%
:- func bimap__from_assoc_list(assoc_list(K, V)) = bimap(K, V) is semidet.
:- pred bimap__from_assoc_list(assoc_list(K, V)::in, bimap(K, V)::out)
	is semidet.

	% As above but throws an exception instead of failing if the
	% association list does not implicitly defined a bijection.
	%
:- func bimap__det_from_assoc_list(assoc_list(K, V)) = bimap(K, V).
:- pred bimap__det_from_assoc_list(assoc_list(K, V)::in, bimap(K, V)::out) is det.

	% Convert a pair of lists into a bimap.
	% Fails if the lists do not implicitly define a bijection or
	% if the lists are of unequal length.
	%
:- func bimap__from_corresponding_lists(list(K), list(V)) = bimap(K, V)
	is semidet.
:- pred bimap__from_corresponding_lists(list(K)::in, list(V)::in,
	bimap(K, V)::out) is semidet.

	% As above but throws an exception instead of failing if the
	% lists do not implicitly define a bijection or are of unequal
	% length.
	%
:- func bimap__det_from_corresponding_lists(list(K), list(V)) = bimap(K, V).
:- pred bimap__det_from_corresponding_lists(list(K)::in, list(V)::in,
	bimap(K, V)::out) is det.

:- func bimap__apply_forward_map_to_list(bimap(K, V), list(K)) = list(V).
:- pred bimap__apply_forward_map_to_list(bimap(K, V)::in, list(K)::in,
	list(V)::out) is det.

:- func bimap__apply_reverse_map_to_list(bimap(K, V), list(V)) = list(K).
:- pred bimap__apply_reverse_map_to_list(bimap(K, V)::in, list(V)::in,
	list(K)::out) is det.

	% Apply a transformation predicate to all the keys.
	% Throws an exception if the resulting bimap is not bijective.
	%
:- func bimap__map_keys(func(V, K) = L, bimap(K, V)) = bimap(L, V).
:- pred bimap__map_keys(pred(V, K, L)::in(pred(in, in, out) is det),
	bimap(K, V)::in, bimap(L, V)::out) is det.

	% Apply a transformation predicate to all the values.
	% Throws an exception if the resulting bimap is not bijective.
	%
:- func bimap__map_values(func(K, V) = W, bimap(K, V)) = bimap(K, W).
:- pred bimap__map_values(pred(K, V, W)::in(pred(in, in, out) is det),
	bimap(K, V)::in, bimap(K, W)::out) is det.

	% Perform a traversal of the bimap, applying an accumulator
	% predicate for each key-value pair.
	%
:- func bimap__foldl(func(K, V, T) = T, bimap(K, V), T) = T.
:- pred bimap__foldl(pred(K, V, T, T), bimap(K, V), T, T).
:- mode bimap__foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode bimap__foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode bimap__foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

	% Perform a traversal of the bimap, applying an accumulator
	% predicate with two accumulators for each key-value pair.
	% (Although no more expressive than bimap.foldl, this is often
	% a more convenient format, and a little more efficient).
	%
:- pred bimap__foldl2(pred(K, V, T, T, U, U), bimap(K, V), T, T, U, U).
:- mode bimap__foldl2(pred(in, in, in, out, in, out) is det,
	in, in, out, in, out) is det.
:- mode bimap__foldl2(pred(in, in, in, out, in, out) is semidet,
	in, in, out, in, out) is semidet.
:- mode bimap__foldl2(pred(in, in, in, out, di, uo) is det,
	in, in, out, di, uo) is det.
:- mode bimap__foldl2(pred(in, in, di, uo, di, uo) is det,
	in, di, uo, di, uo) is det.

	% Perform a traversal of the bimap, applying an accumulator
	% predicate with three accumulators for each key-value pair.
	% (Although no more expressive than bimap.foldl, this is often
	% a more convenient format, and a little more efficient).
	%
:- pred bimap__foldl3(pred(K, V, T, T, U, U, W, W), bimap(K, V),
	T, T, U, U, W, W).
:- mode bimap__foldl3(pred(in, in, in, out, in, out, in, out) is det,
	in, in, out, in, out, in, out) is det.
:- mode bimap__foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
	in, in, out, in, out, in, out) is semidet.
:- mode bimap__foldl3(pred(in, in, in, out, in, out, di, uo) is det,
	in, in, out, in, out, di, uo) is det.
:- mode bimap__foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
	in, in, out, di, uo, di, uo) is det.
:- mode bimap__foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
	in, di, uo, di, uo, di, uo) is det.

	% Extract a the forward map from the bimap, the map
	% from key to value.
	%
:- func bimap__forward_map(bimap(K, V)) = map(K, V).

	% Extract the reverse map from the bimap, the map
	% from value to key.
	%
:- func bimap__reverse_map(bimap(K, V)) = map(V, K).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module std_util.

:- type bimap(K, V)	--->	bimap(map(K, V), map(V, K)).

%-----------------------------------------------------------------------------%

bimap__init(B) :-
	map__init(Forward),
	map__init(Reverse),
	B = bimap(Forward, Reverse).

bimap__is_empty(bimap(Forward, _)) :-
	map__is_empty(Forward). % by inference == map__is_empty(Reverse).

bimap__search(bimap(Forward, Reverse), K, V) :-
	map__search(Forward, K, V),
	map__search(Reverse, V, K).

bimap__forward_search(bimap(Forward, _), K, V) :-
	map__search(Forward, K, V).

bimap__reverse_search(bimap(_, Reverse), K, V) :-
	map__search(Reverse, V, K).

bimap__contains_key(bimap(Forward, _), K) :-
	map__contains(Forward, K).

bimap__contains_value(bimap(_, Reverse), V) :-
	map__contains(Reverse, V).

bimap__lookup(bimap(Forward, _), K, V) :-
	map__lookup(Forward, K, V).

bimap__reverse_lookup(bimap(_, Reverse), K, V) :-
	map__lookup(Reverse, V, K).

bimap__ordinates(bimap(Forward, _), Os) :-
	map__keys(Forward, Os).

bimap__coordinates(bimap(_, Reverse), Cs) :-
	map__keys(Reverse, Cs).

bimap__insert(bimap(Forward0, Reverse0), K, V, bimap(Forward, Reverse)) :-
	map__insert(Forward0, K, V, Forward),
	map__insert(Reverse0, V, K, Reverse).

bimap__det_insert(bimap(Forward0, Reverse0), K, V, bimap(Forward, Reverse)) :-
	map__det_insert(Forward0, K, V, Forward),
	map__det_insert(Reverse0, V, K, Reverse).

bimap__set(bimap(Forward0, Reverse0), K, V, bimap(Forward, Reverse)) :-
	( map__search(Forward0, K, KVal) ->
		( V \= KVal ->
			map__det_update(Forward0, K, V, Forward1),
			map__delete(Reverse0, KVal, Reverse1)
		;	
			Forward1 = Forward0,
			Reverse1 = Reverse0
		)
	;
		map__det_insert(Forward0, K, V, Forward1),
		Reverse0 = Reverse1
	),
	( map__search(Reverse0, V, VKey) ->
		( K \= VKey ->
			map__det_update(Reverse1, V, K, Reverse),		
			map__delete(Forward1, VKey, Forward)
		;
			Forward = Forward1,
			Reverse = Reverse1
		)
	;
		map__det_insert(Reverse1, V, K, Reverse),
		Forward = Forward1
	).

bimap__insert_from_assoc_list(List, BM0) = BM :-
	bimap__insert_from_assoc_list(List, BM0, BM).	

bimap__insert_from_assoc_list([], !BM).
bimap__insert_from_assoc_list([ Key - Value | KeyValues], !BM) :-
	bimap__insert(!.BM, Key, Value, !:BM),
	bimap__insert_from_assoc_list(KeyValues, !BM).

bimap__det_insert_from_assoc_list([], !BM).
bimap__det_insert_from_assoc_list([Key - Value | KeysValues], !BM) :-
	bimap__det_insert(!.BM, Key, Value, !:BM),
	bimap__det_insert_from_assoc_list(KeysValues, !BM).

bimap__det_insert_from_corresponding_lists([], [], !BM).
bimap__det_insert_from_corresponding_lists([], [_ | _], !BM) :-
	error("bimap__det_insert_from_corresponding_lists: length mismatch").
bimap__det_insert_from_corresponding_lists([_ | _], [], !BM) :-
	error("bimap__det_insert_from_corresponding_lists: length mismatch").
bimap__det_insert_from_corresponding_lists([Key | Keys], [Value | Values],
		!BM) :-
	bimap__det_insert(!.BM, Key, Value, !:BM),
	bimap__det_insert_from_corresponding_lists(Keys, Values, !BM).

bimap__set_from_assoc_list([], !BM).
bimap__set_from_assoc_list([Key - Value | KeysValues], !BM) :-
	bimap__set(!.BM, Key, Value, !:BM),
	bimap__set_from_assoc_list(KeysValues, !BM).

bimap__set_from_corresponding_lists([], [], !BM).
bimap__set_from_corresponding_lists([], [_ | _], !BM) :-
	error("bimap__set_from_corresponding_lists: length mismatch").
bimap__set_from_corresponding_lists([_ | _], [], !BM) :-
	error("bimap__set_from_corresponding_lists: length mismatch").
bimap__set_from_corresponding_lists([Key | Keys], [Value | Values],
		!BM) :-
	bimap__set(!.BM, Key, Value, !:BM),
	bimap__set_from_corresponding_lists(Keys, Values, !BM).

bimap__delete_key(K, BM0, BM) :-
	BM0 = bimap(Forward0, Reverse0),
	( map__search(Forward0, K, V) ->
		map__delete(Forward0, K, Forward),
		map__delete(Reverse0, V, Reverse),
		BM = bimap(Forward, Reverse)
	;
		BM = BM0
	).

bimap__delete_value(V, BM0, BM) :-
	BM0 = bimap(Forward0, Reverse0),
	( map__search(Reverse0, V, K) ->
		map__delete(Forward0, K, Forward),
		map__delete(Reverse0, V, Reverse),
		BM = bimap(Forward, Reverse)
	;
		BM = BM0
	).

bimap__delete_keys([], !BM).
bimap__delete_keys([Key | Keys], !BM) :-
	bimap__delete_key(Key, !BM),
	bimap__delete_keys(Keys, !BM).

bimap__delete_values([], !BM).
bimap__delete_values([Value | Values], !BM) :-
	bimap__delete_value(Value, !BM),
	bimap__delete_values(Values, !BM).

bimap__overlay(BMA, BMB, BM) :-
	bimap__to_assoc_list(BMB, KVBs),
	bimap__overlay_2(KVBs, BMA, BM).

:- pred bimap__overlay_2(assoc_list(K, V)::in, bimap(K, V)::in,
	bimap(K, V)::out) is det.

bimap__overlay_2([], !BM).
bimap__overlay_2([Key - Value | KeysValues], !BM) :-
	bimap__set(!.BM, Key, Value, !:BM),
	bimap__overlay_2(KeysValues, !BM).

bimap__to_assoc_list(bimap(Forward, _), L) :-
	map__to_assoc_list(Forward, L).

bimap__from_assoc_list(L, Bimap) :-
	bimap__insert_from_assoc_list(L, bimap.init, Bimap).

bimap__det_from_assoc_list(L) = Bimap :-
	bimap__det_from_assoc_list(L, Bimap).

bimap__det_from_assoc_list(L, Bimap) :-
	bimap__det_insert_from_assoc_list(L, bimap.init, Bimap).

bimap__from_corresponding_lists(Ks, Vs, BM) :-
	assoc_list__from_corresponding_lists(Ks, Vs, L),
	bimap__from_assoc_list(L, BM).

bimap__det_from_corresponding_lists(Ks, Vs) = BM :-
	bimap__det_from_corresponding_lists(Ks, Vs, BM).

bimap__det_from_corresponding_lists(Ks, Vs, BM) :-
	assoc_list__from_corresponding_lists(Ks, Vs, L),
	bimap__det_from_assoc_list(L, BM).

bimap__apply_forward_map_to_list(bimap(Forward, _), Ks, Vs) :-
	map__apply_to_list(Ks, Forward, Vs).

bimap__apply_reverse_map_to_list(bimap(_, Reverse), Vs, Ks) :-
	map__apply_to_list(Vs, Reverse, Ks).

bimap__map_keys(KeyMap, BM0, BM) :-
	bimap__to_assoc_list(BM0, L0),
	bimap__map_keys_2(KeyMap, L0, [], L),
	bimap__det_from_assoc_list(L, BM).

bimap__map_keys(KeyMap, BM0) = BM :-
	bimap__to_assoc_list(BM0, L0),
	bimap__map_keys_func_2(KeyMap, L0, [], L),
	bimap__det_from_assoc_list(L, BM).

bimap__map_values(ValueMap, BM0, BM) :-
	bimap__to_assoc_list(BM0, L0),
	bimap__map_values_2(ValueMap, L0, [], L),
	bimap__det_from_assoc_list(L, BM).

bimap__map_values(ValueMap, BM0) = BM :-
	bimap__to_assoc_list(BM0, L0),
	bimap__map_values_func_2(ValueMap, L0, [], L),
	bimap__det_from_assoc_list(L, BM).

:- pred bimap__map_keys_2(pred(V, K, L)::in(pred(in, in, out) is det),
	assoc_list(K, V)::in, assoc_list(L, V)::in, assoc_list(L, V)::out)
	is det.

bimap__map_keys_2(_KeyMap, [], !List).
bimap__map_keys_2(KeyMap, [Key0 - Value | Tail0], !List) :-
	KeyMap(Value, Key0, Key),
	!:List = [Key - Value | !.List],
	bimap__map_keys_2(KeyMap, Tail0, !List).

:- pred bimap__map_keys_func_2(func(V, K) = L::in(func(in, in) = out is det),
	assoc_list(K, V)::in, assoc_list(L, V)::in, assoc_list(L, V)::out)
	is det.

bimap__map_keys_func_2(_KeyMap, [], !List).
bimap__map_keys_func_2(KeyMap, [Key0 - Value | Tail0], !List) :-
	Key = KeyMap(Value, Key0),
	!:List = [Key - Value | !.List],
	bimap__map_keys_func_2(KeyMap, Tail0, !List).

:- pred bimap__map_values_2(pred(K, V, W)::in(pred(in, in, out) is det),
	assoc_list(K, V)::in, assoc_list(K, W)::in, assoc_list(K, W)::out)
	is det.

bimap__map_values_2(_ValueMap, [], !List).
bimap__map_values_2(ValueMap, [Key - Value0 | Tail0], !List) :-
	ValueMap(Key, Value0, Value),
	!:List = [Key - Value | !.List],
	bimap__map_values_2(ValueMap, Tail0, !List).

:- pred bimap__map_values_func_2(func(K, V) = W::in(func(in, in) = out is det),
	assoc_list(K, V)::in, assoc_list(K, W)::in, assoc_list(K, W)::out)
	is det.

bimap__map_values_func_2(_ValueMap, [], !List).
bimap__map_values_func_2(ValueMap, [Key - Value0 | Tail0], !List) :-
	Value = ValueMap(Key, Value0),
	!:List = [Key - Value | !.List],
	bimap__map_values_func_2(ValueMap, Tail0, !List).

bimap__foldl(Pred, bimap(Forward, _), List0, List) :-
	map__foldl(Pred, Forward, List0, List).

bimap__foldl2(Pred, bimap(Forward, _), !A, !B) :-
	map__foldl2(Pred, Forward, !A, !B).

bimap__foldl3(Pred, bimap(Forward, _), !A, !B, !C) :-
	map__foldl3(Pred, Forward, !A, !B, !C).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Functional forms added.

bimap__init = BM :-
	bimap__init(BM).

bimap__forward_search(BM, K) = V :-
	bimap__forward_search(BM, K, V).

bimap__reverse_search(BM, V) = K :-
	bimap__reverse_search(BM, K, V).

bimap__lookup(BM, K) = V :-
	bimap__lookup(BM, K, V).

bimap__reverse_lookup(BM, V) = K :-
	bimap__reverse_lookup(BM, K, V).

bimap__ordinates(BM) = Ks :-
	bimap__ordinates(BM, Ks).

bimap__coordinates(BM) = Vs :-
	bimap__coordinates(BM, Vs).

bimap__insert(BM1, K, V) = BM2 :-
	bimap__insert(BM1, K, V, BM2).

bimap__det_insert(BM1, K, V) = BM2 :-
	bimap__det_insert(BM1, K, V, BM2).

bimap__det_insert_from_assoc_list(KVs, BM0) = BM :-
	bimap__det_insert_from_assoc_list(KVs, BM0, BM).

bimap__det_insert_from_corresponding_lists(Ks, Vs, BM0) = BM :-
	bimap__det_insert_from_corresponding_lists(Ks, Vs, BM0, BM).

bimap__set_from_assoc_list(KVs, BM0) = BM :-
	bimap__set_from_assoc_list(KVs, BM0, BM).

bimap__set_from_corresponding_lists(Ks, Vs, BM0) = BM :-
	bimap__set_from_corresponding_lists(Ks, Vs, BM0, BM).

bimap__set(BM1, K, V) = BM2 :-
	bimap__set(BM1, K, V, BM2).

bimap__delete_key(BM0, K) = BM :-
	bimap__delete_key(K, BM0, BM).

bimap__delete_value(BM0, V) = BM :-
	bimap__delete_value(V, BM0, BM).

bimap__delete_keys(BM0, Ks) = BM :-
	bimap__delete_keys(Ks, BM0, BM).

bimap__delete_values(BM0, Vs) = BM :-
	bimap__delete_values(Vs, BM0, BM).

bimap__overlay(BMA, BMB) = BM :-
	bimap__overlay(BMA, BMB, BM).

bimap__to_assoc_list(BM) = AL :-
	bimap__to_assoc_list(BM, AL).

bimap__from_assoc_list(AL) = BM :-
	bimap__from_assoc_list(AL, BM).

bimap__from_corresponding_lists(Ks, Vs) = BM :-
	bimap__from_corresponding_lists(Ks, Vs, BM).

bimap__apply_forward_map_to_list(BM, Ks) = Vs :-
	bimap__apply_forward_map_to_list(BM, Ks, Vs).

bimap__apply_reverse_map_to_list(BM, Vs) = Ks :-
	bimap__apply_reverse_map_to_list(BM, Vs, Ks).

bimap__foldl(Func, bimap(Forward, _), List0) =
	map__foldl(Func, Forward, List0).

bimap__forward_map(bimap(Forward, _)) = Forward.

bimap__reverse_map(bimap(_, Reverse)) = Reverse.

%----------------------------------------------------------------------------%
:- end_module bimap.
%----------------------------------------------------------------------------%
