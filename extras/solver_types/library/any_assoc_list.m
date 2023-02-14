%-----------------------------------------------------------------------------%
% vim ft=mercury ts=4 sw=4 et
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% any_any_assoc_list.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Sep  7 14:53:47 EST 2005
%
% A port of any_assoc_list.m for values with inst any.
% This is needed by any_map.m.
%-----------------------------------------------------------------------------%

:- module any_assoc_list.

:- interface.

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type any_assoc_list(K,V) ==  list(pair(K,V)).

:- type any_assoc_list(T)   ==  list(pair(T,T)).

    % Zip together two lists; abort if they are of different lengths.
    %
:- pred any_assoc_list.from_corresponding_lists(list(K)::in, list(V)::ia,
        any_assoc_list(K,V)::oa) is det.
:- func any_assoc_list.from_corresponding_lists(list(K)::in, list(V)::ia)
        = (any_assoc_list(K,V)::oa) is det.

    % Return the first member of each pair.
    %
:- pred any_assoc_list.keys(any_assoc_list(K, V)::ia, list(K)::out) is det.
:- func any_assoc_list.keys(any_assoc_list(K, V)::ia) = (list(K)::out) is det.

    % Return the second member of each pair.
    %
:- pred any_assoc_list.values(any_assoc_list(K, V)::ia, list(V)::oa)
        is det.
:- func any_assoc_list.values(any_assoc_list(K, V)::ia) = (list(V)::oa)
        is det.

    % Return the two lists contain respectively the first and second member
    % of each pair in the any_assoc_list.
    %
:- pred any_assoc_list.keys_and_values(any_assoc_list(K, V)::ia,
        list(K)::out, list(V)::oa) is det.

    % Find the first element of the association list that matches
    % the given key, and return the associated value.
    %
:- pred any_assoc_list.search(any_assoc_list(K, V)::ia, K::in, V::oa)
        is semidet.

    % An alternative version of any_assoc_list.search.
    %
:- func (any_assoc_list(K, V)::ia) ^ elem(K::in) = (V::oa) is semidet.

    % An alternative version of any_assoc_list.search that throws an
    % exception if the key in question does not appear in the
    % any_assoc_list.
    %
:- func (any_assoc_list(K, V)::ia) ^ det_elem(K::in) = (V::oa) is det.

    % Find the first element of the association list that matches
    % the given key. Return the associated value, and the original
    % list with the selected element removed.
    %
:- pred any_assoc_list.remove(any_assoc_list(K, V)::ia, K::in, V::oa,
        any_assoc_list(K, V)::oa) is semidet.

:- func any_assoc_list.map_values(func(K, V) = W, any_assoc_list(K, V))
        = any_assoc_list(K, W).
:- mode any_assoc_list.map_values(func(in, ia) = oa is det, ia) = oa is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module any_util.
:- import_module require.
:- import_module string.
:- import_module type_desc.

any_assoc_list.from_corresponding_lists(Ks, Vs, KVs) :-
    promise_pure (
        any_assoc_list.from_corresponding_2(Ks, Vs, KVs0)
    ->
        KVs = KVs0
    ;
        KeyType = type_name(type_of(Ks)),
        list.length(Ks, KeyLength),
        string.int_to_string(KeyLength, KeyLengthString),
        ValueType = type_name(type_of(Vs)),
        ValueLength = any_list.length(Vs),
        string.int_to_string(ValueLength, ValueLengthString),
        string.append_list(
            ["any_assoc_list.from_corresponding_lists: " ++
                "lists have different lengths.\n",
            "\tKey list type: ",
            KeyType,
            "\n\tKey list length: ",
            KeyLengthString,
            "\n\tValue list type: ",
            ValueType,
            "\n\tValue list length: ",
            ValueLengthString
            ],
            ErrorString),
        error(ErrorString)
    ).

:- pred any_assoc_list.from_corresponding_2(list(K)::in, list(V)::ia,
    any_assoc_list(K,V)::oa) is semidet.

any_assoc_list.from_corresponding_2([], [], []).
any_assoc_list.from_corresponding_2([A | As], [B | Bs], [A - B | ABs]) :-
    any_assoc_list.from_corresponding_2(As, Bs, ABs).

any_assoc_list.keys([], []).
any_assoc_list.keys([K - _ | KVs], [K | Ks]) :-
    unsafe_cast_to_ground(K),
    any_assoc_list.keys(KVs, Ks).

any_assoc_list.values([], []).
any_assoc_list.values([_ - V | KVs], [V | Vs]) :-
    any_assoc_list.values(KVs, Vs).

any_assoc_list.keys_and_values([], [], []).
any_assoc_list.keys_and_values([K - V | KVs], [K | Ks], [V | Vs]) :-
    unsafe_cast_to_ground(K),
    any_assoc_list.keys_and_values(KVs, Ks, Vs).

any_assoc_list.search([K - V | KVs], Key, Value) :-
    unsafe_cast_to_ground(K),
    ( K = Key ->
        Value = V
    ;
        any_assoc_list.search(KVs, Key, Value)
    ).

any_assoc_list.remove([K - V | KVs], Key, Value, Rest) :-
    unsafe_cast_to_ground(K),
    ( K = Key ->
        Value = V,
        Rest = KVs
    ;
        any_assoc_list.remove(KVs, Key, Value, Rest1),
        Rest = [K - V | Rest1]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

any_assoc_list.from_corresponding_lists(Ks, Vs) = AL :-
    any_assoc_list.from_corresponding_lists(Ks, Vs, AL).

any_assoc_list.keys(AL) = Ks :-
    any_assoc_list.keys(AL, Ks).

any_assoc_list.values(AL) = Vs :-
    any_assoc_list.values(AL, Vs).

any_assoc_list.map_values(_F, []) = [].
any_assoc_list.map_values(F, [K - V0 | KVs0]) = [K - V | KVs] :-
    unsafe_cast_to_ground(K),
    V = apply(F, K, V0),
    KVs = any_assoc_list.map_values(F, KVs0).

AL ^ elem(K) = V :-
    any_assoc_list.search(AL, K, V).

AL ^ det_elem(K) = V :-
    promise_pure (
      if   any_assoc_list.search(AL, K, V0)
      then V = V0
      else report_lookup_error("any_assoc_list.det_elem: key not found", K)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
