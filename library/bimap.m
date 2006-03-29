%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 1999, 2004-2006 The University of Melbourne.
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
% Key.  A bimap also allows you to efficiently look up the Key given the Data.
% This time efficiency comes at the expense of using twice as much space.
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
:- func bimap.init = bimap(K, V).
:- pred bimap.init(bimap(K, V)::out) is det.

    % Check whether a bimap is empty.
    %
:- pred bimap.is_empty(bimap(K, V)::in) is semidet.

    % Search the bimap. The first mode searches for a value given a key
    % and the second mode searches for a key given a value.
    %
:- pred bimap.search(bimap(K, V), K, V).
:- mode bimap.search(in, in, out) is semidet.
:- mode bimap.search(in, out, in) is semidet.

    % Search the bimap for the value corresponding to a given key.
    %
:- func bimap.forward_search(bimap(K, V), K) = V is semidet.
:- pred bimap.forward_search(bimap(K, V)::in, K::in, V::out) is semidet.

    % Search the bimap for the key corresponding to the given value.
    %
:- func bimap.reverse_search(bimap(K, V), V) = K is semidet.
:- pred bimap.reverse_search(bimap(K, V)::in, K::out, V::in) is semidet.

    % Look up the value in the bimap corresponding to the given key.
    % Throws an exception if the key is not present in the bimap.
    %
:- func bimap.lookup(bimap(K, V), K) = V.
:- pred bimap.lookup(bimap(K, V)::in, K::in, V::out) is det.

    % Look up the key in the bimap corresponding to the given value.
    % Throws an exception if the value is not present in the bimap.
    %
:- func bimap.reverse_lookup(bimap(K, V), V) = K.
:- pred bimap.reverse_lookup(bimap(K, V)::in, K::out, V::in) is det.

    % Given a bimap, return a list of all the keys in the bimap.
    %
:- func bimap.ordinates(bimap(K, V)) = list(K).
:- pred bimap.ordinates(bimap(K, V)::in, list(K)::out) is det.

    % Given a bimap, return a list of all the data values in the bimap.
    %
:- func bimap.coordinates(bimap(K, V)) = list(V).
:- pred bimap.coordinates(bimap(K, V)::in, list(V)::out) is det.

    % Succeeds iff the bimap contains the given key.
    %
:- pred bimap.contains_key(bimap(K, V)::in, K::in) is semidet.

    % Succeeds iff the bimap contains the given value.
    %
:- pred bimap.contains_value(bimap(K, V)::in, V::in) is semidet.

    % Insert a new key-value pair into the bimap.
    % Fails if either the key or value already exists.
    %
:- func bimap.insert(bimap(K, V), K, V) = bimap(K, V) is semidet.
:- pred bimap.insert(bimap(K, V)::in, K::in, V::in, bimap(K, V)::out)
    is semidet.

    % As above but throws an exception if the key or value already
    % exists.
    %
:- func bimap.det_insert(bimap(K, V), K, V) = bimap(K, V).
:- pred bimap.det_insert(bimap(K, V)::in, K::in, V::in, bimap(K, V)::out)
    is det.

    % Update the key and value if already present, otherwise insert the
    % new key and value.
    %
    % NOTE: setting the key-value pair (K, V) will remove the key-value pairs
    % (K, V1) and (K1, V) if they exist.
    %
:- func bimap.set(bimap(K, V), K, V) = bimap(K, V).
:- pred bimap.set(bimap(K, V)::in, K::in, V::in, bimap(K, V)::out) is det.

    % Insert key-value pairs from an association list into the given bimap.
    % Fails if the contents of the association list and the initial bimap
    % do not implicitly form a bijection.
    %
:- func bimap.insert_from_assoc_list(assoc_list(K, V), bimap(K, V)) =
    bimap(K, V) is semidet.
:- pred bimap.insert_from_assoc_list(assoc_list(K, V)::in,
    bimap(K, V)::in, bimap(K, V)::out) is semidet.

    % As above but throws an exception if the association list and
    % initial bimap are not implicitly bijective.
    %
:- func bimap.det_insert_from_assoc_list(assoc_list(K, V), bimap(K, V))
    = bimap(K, V).
:- pred bimap.det_insert_from_assoc_list(assoc_list(K, V)::in,
    bimap(K, V)::in, bimap(K, V)::out) is det.

    % Insert key-value pairs from a pair of corresponding lists.
    % Throws an exception if the lists are not of equal lengths
    % or if they do not implicitly define a bijection.
    %
:- func bimap.det_insert_from_corresponding_lists(list(K), list(V),
    bimap(K, V)) = bimap(K, V).
:- pred bimap.det_insert_from_corresponding_lists(list(K)::in, list(V)::in,
    bimap(K, V)::in, bimap(K, V)::out) is det.

    % Apply bimap.set to each key-value pair in the association list.
    % The key-value pairs from the association list may update existing keys
    % and values in the bimap.
    %
:- func bimap.set_from_assoc_list(assoc_list(K, V), bimap(K, V))
    = bimap(K, V).
:- pred bimap.set_from_assoc_list(assoc_list(K, V)::in,
    bimap(K, V)::in, bimap(K, V)::out) is det.

    % As above but with a pair of corresponding lists in place of an
    % association list. Throws an exception if the lists are not of
    % equal length.
    %
:- func bimap.set_from_corresponding_lists(list(K), list(V),
    bimap(K, V)) = bimap(K, V).
:- pred bimap.set_from_corresponding_lists(list(K)::in, list(V)::in,
    bimap(K, V)::in, bimap(K, V)::out) is det.

    % Delete a key-value pair from a bimap. If the key is not present,
    % leave the bimap unchanged.
    %
:- func bimap.delete_key(bimap(K, V), K) = bimap(K, V).
:- pred bimap.delete_key(K::in, bimap(K, V)::in, bimap(K, V)::out) is det.

    % Delete a key-value pair from a bimap. If the value is not present,
    % leave the bimap unchanged.
    %
:- func bimap.delete_value(bimap(K, V), V) = bimap(K, V).
:- pred bimap.delete_value(V::in, bimap(K, V)::in, bimap(K, V)::out) is det.

    % Apply bimap.delete_key to a list of keys.
    %
:- func bimap.delete_keys(bimap(K, V), list(K)) = bimap(K, V).
:- pred bimap.delete_keys(list(K)::in, bimap(K, V)::in, bimap(K, V)::out)
    is det.

    % Apply bimap.delete_value to a list of values.
    %
:- func bimap.delete_values(bimap(K, V), list(V)) = bimap(K, V).
:- pred bimap.delete_values(list(V)::in, bimap(K, V)::in, bimap(K, V)::out)
    is det.

    % bimap.overlay(BIMapA, BIMapB, BIMap):
    % Apply map.overlay to the forward maps of BIMapA and BIMapB,
    % and compute the reverse map from the resulting map.
    %
:- func bimap.overlay(bimap(K, V), bimap(K, V)) = bimap(K, V).
:- pred bimap.overlay(bimap(K, V)::in, bimap(K, V)::in, bimap(K, V)::out)
    is det.

    % Convert a bimap to an association list.
    %
:- func bimap.to_assoc_list(bimap(K, V)) = assoc_list(K, V).
:- pred bimap.to_assoc_list(bimap(K, V)::in, assoc_list(K, V)::out) is det.

    % Convert an association list to a bimap. Fails if the association list
    % does not implicitly define a bijection, i.e. a key or value occurs
    % multiple times in the association list.
    %
:- func bimap.from_assoc_list(assoc_list(K, V)) = bimap(K, V) is semidet.
:- pred bimap.from_assoc_list(assoc_list(K, V)::in, bimap(K, V)::out)
    is semidet.

    % As above but throws an exception instead of failing if the
    % association list does not implicitly defined a bijection.
    %
:- func bimap.det_from_assoc_list(assoc_list(K, V)) = bimap(K, V).
:- pred bimap.det_from_assoc_list(assoc_list(K, V)::in, bimap(K, V)::out)
    is det.

    % Convert a pair of lists into a bimap. Fails if the lists do not
    % implicitly define a bijection or if the lists are of unequal length.
    %
:- func bimap.from_corresponding_lists(list(K), list(V)) = bimap(K, V)
    is semidet.
:- pred bimap.from_corresponding_lists(list(K)::in, list(V)::in,
    bimap(K, V)::out) is semidet.

    % As above but throws an exception instead of failing if the lists
    % do not implicitly define a bijection or are of unequal length.
    %
:- func bimap.det_from_corresponding_lists(list(K), list(V)) = bimap(K, V).
:- pred bimap.det_from_corresponding_lists(list(K)::in, list(V)::in,
    bimap(K, V)::out) is det.

:- func bimap.apply_forward_map_to_list(bimap(K, V), list(K)) = list(V).
:- pred bimap.apply_forward_map_to_list(bimap(K, V)::in, list(K)::in,
    list(V)::out) is det.

:- func bimap.apply_reverse_map_to_list(bimap(K, V), list(V)) = list(K).
:- pred bimap.apply_reverse_map_to_list(bimap(K, V)::in, list(V)::in,
    list(K)::out) is det.

    % Apply a transformation predicate to all the keys.
    % Throws an exception if the resulting bimap is not bijective.
    %
:- func bimap.map_keys(func(V, K) = L, bimap(K, V)) = bimap(L, V).
:- pred bimap.map_keys(pred(V, K, L)::in(pred(in, in, out) is det),
    bimap(K, V)::in, bimap(L, V)::out) is det.

    % Apply a transformation predicate to all the values.
    % Throws an exception if the resulting bimap is not bijective.
    %
:- func bimap.map_values(func(K, V) = W, bimap(K, V)) = bimap(K, W).
:- pred bimap.map_values(pred(K, V, W)::in(pred(in, in, out) is det),
    bimap(K, V)::in, bimap(K, W)::out) is det.

    % Perform a traversal of the bimap, applying an accumulator predicate
    % for each key-value pair.
    %
:- func bimap.foldl(func(K, V, A) = A, bimap(K, V), A) = A.
:- pred bimap.foldl(pred(K, V, A, A), bimap(K, V), A, A).
:- mode bimap.foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode bimap.foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode bimap.foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

    % Perform a traversal of the bimap, applying an accumulator predicate
    % with two accumulators for each key-value pair. (Although no more
    % expressive than bimap.foldl, this is often a more convenient format,
    % and a little more efficient).
    %
:- pred bimap.foldl2(pred(K, V, A, A, B, B), bimap(K, V), A, A, B, B).
:- mode bimap.foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode bimap.foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode bimap.foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode bimap.foldl2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.

    % Perform a traversal of the bimap, applying an accumulator predicate
    % with three accumulators for each key-value pair. (Although no more
    % expressive than bimap.foldl, this is often a more convenient format,
    % and a little more efficient).
    %
:- pred bimap.foldl3(pred(K, V, A, A, B, B, C, C), bimap(K, V),
    A, A, B, B, C, C).
:- mode bimap.foldl3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode bimap.foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode bimap.foldl3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode bimap.foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode bimap.foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.

    % Extract a the forward map from the bimap, the map from key to value.
    %
:- func bimap.forward_map(bimap(K, V)) = map(K, V).

    % Extract the reverse map from the bimap, the map from value to key.
    %
:- func bimap.reverse_map(bimap(K, V)) = map(V, K).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module require.

:- type bimap(K, V) --->    bimap(map(K, V), map(V, K)).

%-----------------------------------------------------------------------------%

bimap.init(B) :-
    map.init(Forward),
    map.init(Reverse),
    B = bimap(Forward, Reverse).

bimap.is_empty(bimap(Forward, _)) :-
    map.is_empty(Forward). % by inference == map.is_empty(Reverse).

bimap.search(bimap(Forward, Reverse), K, V) :-
    map.search(Forward, K, V),
    map.search(Reverse, V, K).

bimap.forward_search(bimap(Forward, _), K, V) :-
    map.search(Forward, K, V).

bimap.reverse_search(bimap(_, Reverse), K, V) :-
    map.search(Reverse, V, K).

bimap.contains_key(bimap(Forward, _), K) :-
    map.contains(Forward, K).

bimap.contains_value(bimap(_, Reverse), V) :-
    map.contains(Reverse, V).

bimap.lookup(bimap(Forward, _), K, V) :-
    map.lookup(Forward, K, V).

bimap.reverse_lookup(bimap(_, Reverse), K, V) :-
    map.lookup(Reverse, V, K).

bimap.ordinates(bimap(Forward, _), Os) :-
    map.keys(Forward, Os).

bimap.coordinates(bimap(_, Reverse), Cs) :-
    map.keys(Reverse, Cs).

bimap.insert(bimap(Forward0, Reverse0), K, V, bimap(Forward, Reverse)) :-
    map.insert(Forward0, K, V, Forward),
    map.insert(Reverse0, V, K, Reverse).

bimap.det_insert(bimap(Forward0, Reverse0), K, V, bimap(Forward, Reverse)) :-
    map.det_insert(Forward0, K, V, Forward),
    map.det_insert(Reverse0, V, K, Reverse).

bimap.set(bimap(Forward0, Reverse0), K, V, bimap(Forward, Reverse)) :-
    ( map.search(Forward0, K, KVal) ->
        ( V \= KVal ->
            map.det_update(Forward0, K, V, Forward1),
            map.delete(Reverse0, KVal, Reverse1)
        ;
            Forward1 = Forward0,
            Reverse1 = Reverse0
        )
    ;
        map.det_insert(Forward0, K, V, Forward1),
        Reverse0 = Reverse1
    ),
    ( map.search(Reverse0, V, VKey) ->
        ( K \= VKey ->
            map.det_update(Reverse1, V, K, Reverse),
            map.delete(Forward1, VKey, Forward)
        ;
            Forward = Forward1,
            Reverse = Reverse1
        )
    ;
        map.det_insert(Reverse1, V, K, Reverse),
        Forward = Forward1
    ).

bimap.insert_from_assoc_list(List, BM0) = BM :-
    bimap.insert_from_assoc_list(List, BM0, BM).

bimap.insert_from_assoc_list([], !BM).
bimap.insert_from_assoc_list([ Key - Value | KeyValues], !BM) :-
    bimap.insert(!.BM, Key, Value, !:BM),
    bimap.insert_from_assoc_list(KeyValues, !BM).

bimap.det_insert_from_assoc_list([], !BM).
bimap.det_insert_from_assoc_list([Key - Value | KeysValues], !BM) :-
    bimap.det_insert(!.BM, Key, Value, !:BM),
    bimap.det_insert_from_assoc_list(KeysValues, !BM).

bimap.det_insert_from_corresponding_lists([], [], !BM).
bimap.det_insert_from_corresponding_lists([], [_ | _], !BM) :-
    error("bimap.det_insert_from_corresponding_lists: length mismatch").
bimap.det_insert_from_corresponding_lists([_ | _], [], !BM) :-
    error("bimap.det_insert_from_corresponding_lists: length mismatch").
bimap.det_insert_from_corresponding_lists([Key | Keys], [Value | Values],
        !BM) :-
    bimap.det_insert(!.BM, Key, Value, !:BM),
    bimap.det_insert_from_corresponding_lists(Keys, Values, !BM).

bimap.set_from_assoc_list([], !BM).
bimap.set_from_assoc_list([Key - Value | KeysValues], !BM) :-
    bimap.set(!.BM, Key, Value, !:BM),
    bimap.set_from_assoc_list(KeysValues, !BM).

bimap.set_from_corresponding_lists([], [], !BM).
bimap.set_from_corresponding_lists([], [_ | _], !BM) :-
    error("bimap.set_from_corresponding_lists: length mismatch").
bimap.set_from_corresponding_lists([_ | _], [], !BM) :-
    error("bimap.set_from_corresponding_lists: length mismatch").
bimap.set_from_corresponding_lists([Key | Keys], [Value | Values],
        !BM) :-
    bimap.set(!.BM, Key, Value, !:BM),
    bimap.set_from_corresponding_lists(Keys, Values, !BM).

bimap.delete_key(K, BM0, BM) :-
    BM0 = bimap(Forward0, Reverse0),
    ( map.search(Forward0, K, V) ->
        map.delete(Forward0, K, Forward),
        map.delete(Reverse0, V, Reverse),
        BM = bimap(Forward, Reverse)
    ;
        BM = BM0
    ).

bimap.delete_value(V, BM0, BM) :-
    BM0 = bimap(Forward0, Reverse0),
    ( map.search(Reverse0, V, K) ->
        map.delete(Forward0, K, Forward),
        map.delete(Reverse0, V, Reverse),
        BM = bimap(Forward, Reverse)
    ;
        BM = BM0
    ).

bimap.delete_keys([], !BM).
bimap.delete_keys([Key | Keys], !BM) :-
    bimap.delete_key(Key, !BM),
    bimap.delete_keys(Keys, !BM).

bimap.delete_values([], !BM).
bimap.delete_values([Value | Values], !BM) :-
    bimap.delete_value(Value, !BM),
    bimap.delete_values(Values, !BM).

bimap.overlay(BMA, BMB, BM) :-
    bimap.to_assoc_list(BMB, KVBs),
    bimap.overlay_2(KVBs, BMA, BM).

:- pred bimap.overlay_2(assoc_list(K, V)::in, bimap(K, V)::in,
    bimap(K, V)::out) is det.

bimap.overlay_2([], !BM).
bimap.overlay_2([Key - Value | KeysValues], !BM) :-
    bimap.set(!.BM, Key, Value, !:BM),
    bimap.overlay_2(KeysValues, !BM).

bimap.to_assoc_list(bimap(Forward, _), L) :-
    map.to_assoc_list(Forward, L).

bimap.from_assoc_list(L, Bimap) :-
    bimap.insert_from_assoc_list(L, bimap.init, Bimap).

bimap.det_from_assoc_list(L) = Bimap :-
    bimap.det_from_assoc_list(L, Bimap).

bimap.det_from_assoc_list(L, Bimap) :-
    bimap.det_insert_from_assoc_list(L, bimap.init, Bimap).

bimap.from_corresponding_lists(Ks, Vs, BM) :-
    assoc_list.from_corresponding_lists(Ks, Vs, L),
    bimap.from_assoc_list(L, BM).

bimap.det_from_corresponding_lists(Ks, Vs) = BM :-
    bimap.det_from_corresponding_lists(Ks, Vs, BM).

bimap.det_from_corresponding_lists(Ks, Vs, BM) :-
    assoc_list.from_corresponding_lists(Ks, Vs, L),
    bimap.det_from_assoc_list(L, BM).

bimap.apply_forward_map_to_list(bimap(Forward, _), Ks, Vs) :-
    map.apply_to_list(Ks, Forward, Vs).

bimap.apply_reverse_map_to_list(bimap(_, Reverse), Vs, Ks) :-
    map.apply_to_list(Vs, Reverse, Ks).

bimap.map_keys(KeyMap, BM0, BM) :-
    bimap.to_assoc_list(BM0, L0),
    bimap.map_keys_2(KeyMap, L0, [], L),
    bimap.det_from_assoc_list(L, BM).

bimap.map_keys(KeyMap, BM0) = BM :-
    bimap.to_assoc_list(BM0, L0),
    bimap.map_keys_func_2(KeyMap, L0, [], L),
    bimap.det_from_assoc_list(L, BM).

bimap.map_values(ValueMap, BM0, BM) :-
    bimap.to_assoc_list(BM0, L0),
    bimap.map_values_2(ValueMap, L0, [], L),
    bimap.det_from_assoc_list(L, BM).

bimap.map_values(ValueMap, BM0) = BM :-
    bimap.to_assoc_list(BM0, L0),
    bimap.map_values_func_2(ValueMap, L0, [], L),
    bimap.det_from_assoc_list(L, BM).

:- pred bimap.map_keys_2(pred(V, K, L)::in(pred(in, in, out) is det),
    assoc_list(K, V)::in, assoc_list(L, V)::in, assoc_list(L, V)::out)
    is det.

bimap.map_keys_2(_KeyMap, [], !List).
bimap.map_keys_2(KeyMap, [Key0 - Value | Tail0], !List) :-
    KeyMap(Value, Key0, Key),
    !:List = [Key - Value | !.List],
    bimap.map_keys_2(KeyMap, Tail0, !List).

:- pred bimap.map_keys_func_2(func(V, K) = L::in(func(in, in) = out is det),
    assoc_list(K, V)::in, assoc_list(L, V)::in, assoc_list(L, V)::out)
    is det.

bimap.map_keys_func_2(_KeyMap, [], !List).
bimap.map_keys_func_2(KeyMap, [Key0 - Value | Tail0], !List) :-
    Key = KeyMap(Value, Key0),
    !:List = [Key - Value | !.List],
    bimap.map_keys_func_2(KeyMap, Tail0, !List).

:- pred bimap.map_values_2(pred(K, V, W)::in(pred(in, in, out) is det),
    assoc_list(K, V)::in, assoc_list(K, W)::in, assoc_list(K, W)::out) is det.

bimap.map_values_2(_ValueMap, [], !List).
bimap.map_values_2(ValueMap, [Key - Value0 | Tail0], !List) :-
    ValueMap(Key, Value0, Value),
    !:List = [Key - Value | !.List],
    bimap.map_values_2(ValueMap, Tail0, !List).

:- pred bimap.map_values_func_2(func(K, V) = W::in(func(in, in) = out is det),
    assoc_list(K, V)::in, assoc_list(K, W)::in, assoc_list(K, W)::out) is det.

bimap.map_values_func_2(_ValueMap, [], !List).
bimap.map_values_func_2(ValueMap, [Key - Value0 | Tail0], !List) :-
    Value = ValueMap(Key, Value0),
    !:List = [Key - Value | !.List],
    bimap.map_values_func_2(ValueMap, Tail0, !List).

bimap.foldl(Pred, bimap(Forward, _), List0, List) :-
    map.foldl(Pred, Forward, List0, List).

bimap.foldl2(Pred, bimap(Forward, _), !A, !B) :-
    map.foldl2(Pred, Forward, !A, !B).

bimap.foldl3(Pred, bimap(Forward, _), !A, !B, !C) :-
    map.foldl3(Pred, Forward, !A, !B, !C).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Functional forms added.

bimap.init = BM :-
    bimap.init(BM).

bimap.forward_search(BM, K) = V :-
    bimap.forward_search(BM, K, V).

bimap.reverse_search(BM, V) = K :-
    bimap.reverse_search(BM, K, V).

bimap.lookup(BM, K) = V :-
    bimap.lookup(BM, K, V).

bimap.reverse_lookup(BM, V) = K :-
    bimap.reverse_lookup(BM, K, V).

bimap.ordinates(BM) = Ks :-
    bimap.ordinates(BM, Ks).

bimap.coordinates(BM) = Vs :-
    bimap.coordinates(BM, Vs).

bimap.insert(BM1, K, V) = BM2 :-
    bimap.insert(BM1, K, V, BM2).

bimap.det_insert(BM1, K, V) = BM2 :-
    bimap.det_insert(BM1, K, V, BM2).

bimap.det_insert_from_assoc_list(KVs, BM0) = BM :-
    bimap.det_insert_from_assoc_list(KVs, BM0, BM).

bimap.det_insert_from_corresponding_lists(Ks, Vs, BM0) = BM :-
    bimap.det_insert_from_corresponding_lists(Ks, Vs, BM0, BM).

bimap.set_from_assoc_list(KVs, BM0) = BM :-
    bimap.set_from_assoc_list(KVs, BM0, BM).

bimap.set_from_corresponding_lists(Ks, Vs, BM0) = BM :-
    bimap.set_from_corresponding_lists(Ks, Vs, BM0, BM).

bimap.set(BM1, K, V) = BM2 :-
    bimap.set(BM1, K, V, BM2).

bimap.delete_key(BM0, K) = BM :-
    bimap.delete_key(K, BM0, BM).

bimap.delete_value(BM0, V) = BM :-
    bimap.delete_value(V, BM0, BM).

bimap.delete_keys(BM0, Ks) = BM :-
    bimap.delete_keys(Ks, BM0, BM).

bimap.delete_values(BM0, Vs) = BM :-
    bimap.delete_values(Vs, BM0, BM).

bimap.overlay(BMA, BMB) = BM :-
    bimap.overlay(BMA, BMB, BM).

bimap.to_assoc_list(BM) = AL :-
    bimap.to_assoc_list(BM, AL).

bimap.from_assoc_list(AL) = BM :-
    bimap.from_assoc_list(AL, BM).

bimap.from_corresponding_lists(Ks, Vs) = BM :-
    bimap.from_corresponding_lists(Ks, Vs, BM).

bimap.apply_forward_map_to_list(BM, Ks) = Vs :-
    bimap.apply_forward_map_to_list(BM, Ks, Vs).

bimap.apply_reverse_map_to_list(BM, Vs) = Ks :-
    bimap.apply_reverse_map_to_list(BM, Vs, Ks).

bimap.foldl(Func, bimap(Forward, _), List0) =
    map.foldl(Func, Forward, List0).

bimap.forward_map(bimap(Forward, _)) = Forward.

bimap.reverse_map(bimap(_, Reverse)) = Reverse.

%----------------------------------------------------------------------------%
:- end_module bimap.
%----------------------------------------------------------------------------%
