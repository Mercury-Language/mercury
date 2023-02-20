%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999-2001, 2004-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: assoc_list.m.
% Main authors: fjh, zs.
% Stability: medium to high.
%
% This file defines the type assoc_list(K, V), which holds a list of
% key-value pairs, and some predicates which operate on assoc_lists.
%
% Another module of the Mercury standard library, kv_list.m, defines
% another data type that does the same job, but makes different tradeoffs.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module assoc_list.
:- interface.

:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type assoc_list(K, V) == list(pair(K, V)).

:- type assoc_list(T) == list(pair(T, T)).

%---------------------------------------------------------------------------%

    % These instantiation states can be used for instantiation
    % state subtyping.
    %
:- inst assoc_list(I1, I2) == list(pair(I1, I2)).

:- inst assoc_list(I) == list(pair(I, I)).

%---------------------------------------------------------------------------%
%
% Creating assoc_lists from lists of keys and values.
%

    % Zip together a list of keys and a list of values.
    % Throw an exception if they are of different lengths.
    %
:- func from_corresponding_lists(list(K), list(V)) = assoc_list(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in,
    assoc_list(K, V)::out) is det.

    % Zip together a list of keys and a list of values.
    % Fail if they are of different lengths.
    %
:- pred maybe_from_corresponding_lists(list(K)::in, list(V)::in,
    assoc_list(K, V)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on lists of keys and/or values.
%

    % Swap the two sides of the pairs in each member of the list.
    %
:- func reverse_members(assoc_list(K, V)) = assoc_list(V, K).
:- pred reverse_members(assoc_list(K, V)::in, assoc_list(V, K)::out) is det.

    % Return the first member of each pair.
    %
:- func keys(assoc_list(K, V)) = list(K).
:- pred keys(assoc_list(K, V)::in, list(K)::out) is det.

    % Return the second member of each pair.
    %
:- func values(assoc_list(K, V)) = list(V).
:- pred values(assoc_list(K, V)::in, list(V)::out) is det.

    % Return two lists containing respectively the first and the second member
    % of each pair in the assoc_list.
    %
:- pred keys_and_values(assoc_list(K, V)::in,
    list(K)::out, list(V)::out) is det.

%---------------------------------------------------------------------------%
%
% Searching assoc_lists.
%

    % Find the first element of the association list that matches
    % the given key, and return the associated value.
    % Fail if there is no matching key.
    %
:- pred search(assoc_list(K, V)::in, K::in, V::out) is semidet.

    % Find the first element of the association list that matches
    % the given key, and return the associated value.
    % Throw an exception if there is no matching key.
    %
:- pred lookup(assoc_list(K, V)::in, K::in, V::out) is det.

    % A field access version of search.
    %
:- func assoc_list(K, V) ^ elem(K) = V is semidet.

    % A field access version of lookup.
    %
:- func assoc_list(K, V) ^ det_elem(K) = V is det.

%---------------------------------------------------------------------------%
%
% Updating elements in assoc_lists.
%

    % Find the first element of the assoc_list list that matches
    % the given key, and update the associated value.
    % Fail if there is no matching key.
    %
:- pred update(K::in, V::in, assoc_list(K, V)::in, assoc_list(K, V)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Removing elements from assoc_lists.
%

    % Find the first element of the association list that matches
    % the given key. Return the associated value, and the original list
    % with the selected element removed.
    %
:- pred remove(assoc_list(K, V)::in, K::in, V::out, assoc_list(K, V)::out)
    is semidet.

    % As above, but with an argument ordering that is more conducive to
    % the use of state variable notation.
    %
:- pred svremove(K::in, V::out, assoc_list(K, V)::in, assoc_list(K, V)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Mapping keys or values.
%

:- func map_keys_only(func(K) = L, assoc_list(K, V)) = assoc_list(L, V).
:- pred map_keys_only(pred(K, L), assoc_list(K, V), assoc_list(L, V)).
:- mode map_keys_only(pred(in, out) is det, in, out) is det.

:- func map_values_only(func(V) = W, assoc_list(K, V)) = assoc_list(K, W).
:- pred map_values_only(pred(V, W), assoc_list(K, V), assoc_list(K, W)).
:- mode map_values_only(pred(in, out) is det, in, out) is det.

:- func map_values(func(K, V) = W, assoc_list(K, V)) = assoc_list(K, W).
:- pred map_values(pred(K, V, W), assoc_list(K, V), assoc_list(K, W)).
:- mode map_values(pred(in, in, out) is det, in, out) is det.

%---------------------------------------------------------------------------%
%
% Filtering elements in assoc_lists.
%

    % filter(Pred, List, TrueList) takes a closure with one input argument,
    % and for each key-value pair in List, calls the closure on the key K.
    % The key-value pair is included in TrueList iff Pred(K) is true.
    %
:- func filter(pred(K)::in(pred(in) is semidet),
    assoc_list(K, V)::in) = (assoc_list(K, V)::out) is det.
:- pred filter(pred(K)::in(pred(in) is semidet),
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

    % negated_filter(Pred, List, FalseList) takes a closure with one
    % input argument, and for each key-value pair in List, calls the closure
    % on the key K. The key-value pair is included in TrueList iff
    % Pred(K) is false.
    %
:- func negated_filter(pred(K)::in(pred(in) is semidet),
    assoc_list(K, V)::in) = (assoc_list(K, V)::out) is det.
:- pred negated_filter(pred(K)::in(pred(in) is semidet),
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

    % filter(Pred, List, TrueList, FalseList) takes a closure with
    % one input argument, and for each key-value pair in List,
    % calls the closure on the key K. If Pred(K) is true, the key-value pair
    % is included in TrueList; otherwise, it is included in FalseList.
    %
:- pred filter(pred(K)::in(pred(in) is semidet),
    assoc_list(K, V)::in, assoc_list(K, V)::out, assoc_list(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations on two assoc_lists.
%

    % merge(ListA, ListB, ListAB):
    %
    % Given two lists ListA and ListB, which must both be sorted
    % in ascending order on the keys, return ListAB, which is the result
    % of merging the elements of ListA and ListB. It will also be sorted
    % in ascending order.
    %
:- func merge(assoc_list(K, V), assoc_list(K, V)) = assoc_list(K, V).
:- pred merge(assoc_list(K, V)::in, assoc_list(K, V)::in,
    assoc_list(K, V)::out) is det.

    % common_subset(ListA, ListB, CommonList):
    %
    % Given two lists ListA and ListB, which must both be sorted
    % in ascending order on the keys, neither of which contains any key
    % more than once, return CommonList, which will consist of only the
    % key-value pairs that occur in *both* ListA and ListB.
    % It will also be sorted in ascending order on the keys.
    %
:- func common_subset(assoc_list(K, V), assoc_list(K, V)) = assoc_list(K, V).

%---------------------------------------------------------------------------%
%
% Folding over assoc_lists.
%

    % foldl(Pred, List, Start End) calls Pred
    % with each key-value pair in List, working left-to-right,
    % and an accumulator whose initial value is Start,
    % and returns the final value in End.
    %
:- pred foldl(pred(K, V, A, A), assoc_list(K, V), A, A).
:- mode foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, in, out) is nondet, in, in, out) is nondet.

    % foldl_keys(Func List, Start) = End calls Func
    % with each key in List, working left-to-right, and an accumulator
    % whose initial value is Start, and returns the final value in End.
    %
:- func foldl_keys(func(K, A) = A, assoc_list(K, V), A) = A.

    % foldl_keys(Pred, List, Start End) calls Pred
    % with each key in List, working left-to-right, and an accumulator
    % whose initial value is Start, and returns the final value in End.
    %
:- pred foldl_keys(pred(K, A, A), assoc_list(K, V), A, A).
:- mode foldl_keys(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl_keys(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl_keys(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl_keys(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl_keys(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl_keys(pred(in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl_keys(pred(in, in, out) is multi, in, in, out) is multi.
:- mode foldl_keys(pred(in, in, out) is nondet, in, in, out) is nondet.

    % foldl_values(Func List, Start) = End calls Func
    % with each value in List, working left-to-right, and an accumulator
    % whose initial value is Start, and returns the final value in End.
    %
:- func foldl_values(func(V, A) = A, assoc_list(K, V), A) = A.

    % foldl_values(Pred, List, Start End) calls Pred
    % with each value in List, working left-to-right, and an accumulator
    % whose initial value is Start, and returns the final value in End.
    %
:- pred foldl_values(pred(V, A, A), assoc_list(K, V), A, A).
:- mode foldl_values(pred(in, in, out) is det, in,
    in, out) is det.
:- mode foldl_values(pred(in, mdi, muo) is det, in,
    mdi, muo) is det.
:- mode foldl_values(pred(in, di, uo) is det, in,
    di, uo) is det.
:- mode foldl_values(pred(in, in, out) is semidet, in,
    in, out) is semidet.
:- mode foldl_values(pred(in, mdi, muo) is semidet, in,
    mdi, muo) is semidet.
:- mode foldl_values(pred(in, di, uo) is semidet, in,
    di, uo) is semidet.
:- mode foldl_values(pred(in, in, out) is multi, in,
    in, out) is multi.
:- mode foldl_values(pred(in, in, out) is nondet, in,
    in, out) is nondet.

    % As foldl, but with two accumulators.
    %
:- pred foldl2(pred(K, V, A, A, B, B), assoc_list(K, V), A, A, B, B).
:- mode foldl2(pred(in, in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode foldl2(pred(in, in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode foldl2(pred(in, in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is semidet, in,in, out,
    mdi, muo) is semidet.
:- mode foldl2(pred(in, in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.
:- mode foldl2(pred(in, in, in, out, in, out) is nondet, in, in, out,
    in, out) is nondet.

    % As foldl_values, but with two accumulators.
    %
:- pred foldl2_values(pred(V, A, A, B, B), assoc_list(K, V),
    A, A, B, B).
:- mode foldl2_values(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is det, in,
    in, out, mdi, muo) is det.
:- mode foldl2_values(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode foldl2_values(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldl2_values(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.
:- mode foldl2_values(pred(in, in, out, in, out) is multi, in,
    in, out, in, out) is multi.
:- mode foldl2_values(pred(in, in, out, in, out) is nondet, in,
    in, out, in, out) is nondet.

    % As foldl, but with three accumulators.
    %
:- pred foldl3(pred(K, V, A, A, B, B, C, C), assoc_list(K, V),
    A, A, B, B, C, C).
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, mdi, muo) is semidet, in,
        in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is nondet, in,
    in, out, in, out, in, out) is nondet.

    % As foldl_values, but with three accumulators.
    %
:- pred foldl3_values(pred(V, A, A, B, B, C, C), assoc_list(K, V),
    A, A, B, B, C, C).
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is multi,
    in, in, out, in, out, in, out) is multi.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is nondet,
    in, in, out, in, out, in, out) is nondet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.
:- import_module type_desc.

%---------------------------------------------------------------------------%

from_corresponding_lists(Ks, Vs) = AL :-
    assoc_list.from_corresponding_lists(Ks, Vs, AL).

from_corresponding_lists(Ks, Vs, KVs) :-
    ( if assoc_list.from_corresponding_loop(Ks, Vs, KVsPrime) then
        KVs = KVsPrime
    else
        KeyType = type_name(type_of(Ks)),
        list.length(Ks, KeyLength),
        string.int_to_string(KeyLength, KeyLengthString),
        ValueType = type_name(type_of(Vs)),
        list.length(Vs, ValueLength),
        string.int_to_string(ValueLength, ValueLengthString),
        ErrorString =
            "Lists have different lengths.\n"
            ++ "\tKey list type: " ++ KeyType
            ++ "\n\tKey list length: " ++ KeyLengthString
            ++ "\n\tValue list type: " ++ ValueType
            ++ "\n\tValue list length: " ++ ValueLengthString,
        unexpected($pred, ErrorString)
    ).

maybe_from_corresponding_lists(Ks, Vs, KVs) :-
    assoc_list.from_corresponding_loop(Ks, Vs, KVs).

:- pred from_corresponding_loop(list(K)::in, list(V)::in,
    assoc_list(K,V)::out) is semidet.

from_corresponding_loop([], [], []).
from_corresponding_loop([A | As], [B | Bs], [A - B | ABs]) :-
    assoc_list.from_corresponding_loop(As, Bs, ABs).

%---------------------------------------------------------------------------%

reverse_members(AL1) = AL2 :-
    assoc_list.reverse_members(AL1, AL2).

reverse_members([], []).
reverse_members([K - V | KVs], [V - K | VKs]) :-
    assoc_list.reverse_members(KVs, VKs).

keys(AL) = Ks :-
    assoc_list.keys(AL, Ks).

keys([], []).
keys([K - _ | KVs], [K | Ks]) :-
    assoc_list.keys(KVs, Ks).

values(AL) = Vs :-
    assoc_list.values(AL, Vs).

values([], []).
values([_ - V | KVs], [V | Vs]) :-
    assoc_list.values(KVs, Vs).

keys_and_values([], [], []).
keys_and_values([K - V | KVs], [K | Ks], [V | Vs]) :-
    assoc_list.keys_and_values(KVs, Ks, Vs).

%---------------------------------------------------------------------------%

search([K - V | KVs], Key, Value) :-
    ( if K = Key then
        Value = V
    else
        assoc_list.search(KVs, Key, Value)
    ).

lookup(KVs, K, V) :-
    ( if assoc_list.search(KVs, K, VPrime) then
        V = VPrime
    else
        report_lookup_error("assoc_list.lookup: key not found", K)
    ).

AL ^ elem(K) = V :-
    assoc_list.search(AL, K, V).

AL ^ det_elem(K) = V :-
    assoc_list.lookup(AL, K, V).

%---------------------------------------------------------------------------%

update(Key, Value, KVs0, KVs) :-
    require_complete_switch [KVs0]
    (
        KVs0 = [],
        fail
    ;
        KVs0 = [K - V | TailKVs0],
        ( if Key = K then
            KVs = [K - Value | TailKVs0]
        else
            update(Key, Value, TailKVs0, TailKVs),
            KVs = [K - V | TailKVs]
        )
    ).

%---------------------------------------------------------------------------%

remove([K - V | KVs], Key, Value, Filtered) :-
    ( if K = Key then
        Value = V,
        Filtered = KVs
    else
        assoc_list.remove(KVs, Key, Value, FilteredTail),
        Filtered = [K - V | FilteredTail]
    ).

svremove(Key, Value, !AL) :-
    assoc_list.remove(!.AL, Key, Value, !:AL).

%---------------------------------------------------------------------------%

map_keys_only(_F, []) = [].
map_keys_only(F, [K0 - V | KVs0]) = [K - V | KVs] :-
    K = F(K0),
    KVs = assoc_list.map_keys_only(F, KVs0).

map_keys_only(_P, [], []).
map_keys_only(P, [K0 - V | KVs0], [K - V | KVs]) :-
    P(K0, K),
    assoc_list.map_keys_only(P, KVs0, KVs).

map_values_only(_F, []) = [].
map_values_only(F, [K - V0 | KVs0]) = [K - V | KVs] :-
    V = F(V0),
    KVs = assoc_list.map_values_only(F, KVs0).

map_values_only(_P, [], []).
map_values_only(P, [K - V0 | KVs0], [K - V | KVs]) :-
    P(V0, V),
    assoc_list.map_values_only(P, KVs0, KVs).

map_values(_F, []) = [].
map_values(F, [K - V0 | KVs0]) = [K - V | KVs] :-
    V = F(K, V0),
    KVs = assoc_list.map_values(F, KVs0).

map_values(_P, [], []).
map_values(P, [K - V0 | KVs0], [K - V | KVs]) :-
    P(K, V0, V),
    assoc_list.map_values(P, KVs0, KVs).

%---------------------------------------------------------------------------%

filter(P, List) = Trues :-
    assoc_list.filter(P, List, Trues).

filter(_, [],  []).
filter(P, [HK - HV | T], True) :-
    ( if P(HK) then
        assoc_list.filter(P, T, TrueTail),
        True = [HK - HV | TrueTail]
    else
        assoc_list.filter(P, T, True)
    ).

negated_filter(P, List) = Falses :-
    assoc_list.negated_filter(P, List, Falses).

negated_filter(_, [],  []).
negated_filter(P, [HK - HV | T], False) :-
    ( if P(HK) then
        assoc_list.negated_filter(P, T, False)
    else
        assoc_list.negated_filter(P, T, FalseTail),
        False = [HK - HV | FalseTail]
    ).

filter(_, [],  [], []).
filter(P, [HK - HV | T], True, False) :-
    ( if P(HK) then
        assoc_list.filter(P, T, TrueTail, False),
        True = [HK - HV | TrueTail]
    else
        assoc_list.filter(P, T, True, FalseTail),
        False = [HK - HV | FalseTail]
    ).

%---------------------------------------------------------------------------%

merge(As, Bs) = ABs :-
    assoc_list.merge(As, Bs, ABs).

merge([], Bs, Bs).
merge([A | As], [], [A | As]).
merge([A | As], [B | Bs], Cs) :-
    ( if
        A = AK - _AV,
        B = BK - _BV,
        compare(>, AK, BK)
    then
        assoc_list.merge([A | As], Bs, Cs0),
        Cs = [B | Cs0]
    else
        % If compare((=), AK, BK), take A first.
        assoc_list.merge(As, [B | Bs], Cs0),
        Cs = [A | Cs0]
    ).

common_subset(ListA, ListB) = CommonList :-
    assoc_list.common_subset_loop(ListA, ListB, [], RevCommonList),
    list.reverse(RevCommonList, CommonList).

:- pred common_subset_loop(assoc_list(K, V)::in, assoc_list(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

common_subset_loop(ListA, ListB, !RevCommonList) :-
    (
        ListA = [],
        ListB = []
    ;
        ListA = [_ | _],
        ListB = []
    ;
        ListA = [],
        ListB = [_ | _]
    ;
        ListA = [KeyA - ValueA | TailA],
        ListB = [KeyB - ValueB | TailB],
        compare(R, KeyA, KeyB),
        (
            R = (=),
            ( if ValueA = ValueB then
                !:RevCommonList = [KeyA - ValueA | !.RevCommonList]
            else
                true
            ),
            assoc_list.common_subset_loop(TailA, TailB, !RevCommonList)
        ;
            R = (<),
            % KeyA has no match in ListB.
            assoc_list.common_subset_loop(TailA, ListB, !RevCommonList)
        ;
            R = (>),
            % KeyB has no match in ListA.
            assoc_list.common_subset_loop(ListA, TailB, !RevCommonList)
        )
    ).

%---------------------------------------------------------------------------%

foldl(_P, [], !A).
foldl(P, [K - V | KVs], !A) :-
    P(K, V, !A),
    foldl(P, KVs, !A).

foldl_keys(_F, [], A) = A.
foldl_keys(F, [KV | KVs], !.A) = !:A :-
    KV = K - _V,
    !:A = F(K, !.A),
    !:A = assoc_list.foldl_keys(F, KVs, !.A).

foldl_keys(_P, [], !A).
foldl_keys(P, [KV | KVs], !A) :-
    KV = K - _V,
    P(K, !A),
    assoc_list.foldl_keys(P, KVs, !A).

foldl_values(_F, [], A) = A.
foldl_values(F, [KV | KVs], !.A) = !:A :-
    KV = _K - V,
    !:A = F(V, !.A),
    !:A = assoc_list.foldl_values(F, KVs, !.A).

foldl_values(_P, [], !A).
foldl_values(P, [KV | KVs], !A) :-
    KV = _K - V,
    P(V, !A),
    assoc_list.foldl_values(P, KVs, !A).

foldl2(_P, [], !A, !B).
foldl2(P, [K - V | KVs], !A, !B) :-
    P(K, V, !A, !B),
    foldl2(P, KVs, !A, !B).

foldl2_values(_P, [], !A, !B).
foldl2_values(P, [KV | KVs], !A, !B) :-
    KV = _K - V,
    P(V, !A, !B),
    assoc_list.foldl2_values(P, KVs, !A, !B).

foldl3(_P, [], !A, !B, !C).
foldl3(P, [K - V | KVs], !A, !B, !C) :-
    P(K, V, !A, !B, !C),
    foldl3(P, KVs, !A, !B, !C).

foldl3_values(_P, [], !A, !B, !C).
foldl3_values(P, [KV | KVs], !A, !B, !C) :-
    KV = _K - V,
    P(V, !A, !B, !C),
    assoc_list.foldl3_values(P, KVs, !A, !B, !C).

%---------------------------------------------------------------------------%
:- end_module assoc_list.
%---------------------------------------------------------------------------%
