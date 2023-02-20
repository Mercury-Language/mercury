%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: kv_list.m.
% Main author: zs.
% Stability: medium to high.
%
% This file defines the type kv_list(K, V), which represents lists of
% key-value pairs, and provides a range of operations on such lists.
%
% The kv_list module resembles the assoc_list module quite closely.
% The data type it defines stores the same information, and the set
% of operations they provide is the same, modulo the fact that the
% operations that convert between the two representations are here.
% The difference is that kv_list uses one memory cell, not two,
% to represent one key-value pair, which should mean that it requires
% fewer memory allocations. On the other hand, values of type assoc_list
% may be operated on as plain lists, while this cannot be done on values
% of type kv_list.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module kv_list.
:- interface.

:- import_module assoc_list.
:- import_module list.

%---------------------------------------------------------------------------%

:- type kv_list(K, V)
    --->    kv_nil
    ;       kv_cons(K, V, kv_list(K, V)).

%---------------------------------------------------------------------------%
%
% Creating kv_lists from lists of keys and values.
%

    % Zip together a list of keys and a list of values.
    % Throw an exception if they are of different lengths.
    %
:- func from_corresponding_lists(list(K), list(V)) = kv_list(K, V).
:- pred from_corresponding_lists(list(K)::in, list(V)::in,
    kv_list(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to and from assoc_lists.
%

:- func assoc_list_to_kv_list(assoc_list(K, V)) = kv_list(K, V).
:- func kv_list_to_assoc_list(kv_list(K, V)) = assoc_list(K, V).

%---------------------------------------------------------------------------%
%
% Operations on lists of keys and/or values.
%

    % Swap the two sides of the pairs in each member of the list.
    %
:- func reverse_members(kv_list(K, V)) = kv_list(V, K).
:- pred reverse_members(kv_list(K, V)::in, kv_list(V, K)::out) is det.

    % Return the first member of each pair.
    %
:- func keys(kv_list(K, V)) = list(K).
:- pred keys(kv_list(K, V)::in, list(K)::out) is det.

    % Return the second member of each pair.
    %
:- func values(kv_list(K, V)) = list(V).
:- pred values(kv_list(K, V)::in, list(V)::out) is det.

    % Return two lists containing respectively the first and the second member
    % of each pair in the kv_list.
    %
:- pred keys_and_values(kv_list(K, V)::in, list(K)::out, list(V)::out) is det.

%---------------------------------------------------------------------------%
%
% Searching kv_lists.
%

    % Find the first element of the kv_list list that matches
    % the given key, and return the associated value.
    % Fail if there is no matching key.
    %
:- pred search(kv_list(K, V)::in, K::in, V::out) is semidet.

    % Find the first element of the kv_list list that matches
    % the given key, and return the associated value.
    % Throw an exception if there is no matching key.
    %
:- pred lookup(kv_list(K, V)::in, K::in, V::out) is det.

    % A field access version of search.
    %
:- func kv_list(K, V) ^ elem(K) = V is semidet.

    % A field access version of lookup.
    %
:- func kv_list(K, V) ^ det_elem(K) = V is det.

%---------------------------------------------------------------------------%
%
% Updating elements in kv_lists.
%

    % Find the first element of the kv_list list that matches
    % the given key, and update the associated value.
    % Fail if there is no matching key.
    %
:- pred update(K::in, V::in, kv_list(K, V)::in, kv_list(K, V)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Removing elements from kv_lists.
%

    % Find the first element of the association list that matches the given
    % key. Return the associated value, and the original list with the selected
    % element removed.
    %
:- pred remove(kv_list(K, V)::in, K::in, V::out, kv_list(K, V)::out)
    is semidet.

    % As above, but with an argument ordering that is more conducive to
    % the use of state variable notation.
    %
:- pred svremove(K::in, V::out, kv_list(K, V)::in, kv_list(K, V)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Mapping keys or values.
%

:- func map_keys_only(func(K) = L, kv_list(K, V)) = kv_list(L, V).
:- pred map_keys_only(pred(K, L), kv_list(K, V), kv_list(L, V)).
:- mode map_keys_only(pred(in, out) is det, in, out) is det.

:- func map_values_only(func(V) = W, kv_list(K, V)) = kv_list(K, W).
:- pred map_values_only(pred(V, W), kv_list(K, V), kv_list(K, W)).
:- mode map_values_only(pred(in, out) is det, in, out) is det.

:- func map_values(func(K, V) = W, kv_list(K, V)) = kv_list(K, W).
:- pred map_values(pred(K, V, W), kv_list(K, V), kv_list(K, W)).
:- mode map_values(pred(in, in, out) is det, in, out) is det.

%---------------------------------------------------------------------------%
%
% Filtering elements in kv_lists.
%

    % filter(Pred, List, TrueList) takes a closure with one input argument,
    % and for each key-value pair in List, calls the closure on the key K.
    % The key-value pair is included in TrueList iff Pred(K) is true.
    %
:- func filter(pred(K)::in(pred(in) is semidet),
    kv_list(K, V)::in) = (kv_list(K, V)::out) is det.
:- pred filter(pred(K)::in(pred(in) is semidet),
    kv_list(K, V)::in, kv_list(K, V)::out) is det.

    % negated_filter(Pred, List, FalseList) takes a closure with one
    % input argument, and for each key-value pair in List, calls the closure
    % on the key K. The key-value pair is included in TrueList iff Pred(K)
    % is false.
    %
:- func negated_filter(pred(K)::in(pred(in) is semidet),
    kv_list(K, V)::in) = (kv_list(K, V)::out) is det.
:- pred negated_filter(pred(K)::in(pred(in) is semidet),
    kv_list(K, V)::in, kv_list(K, V)::out) is det.

    % filter(Pred, List, TrueList, FalseList) takes a closure with
    % one input argument, and for each key-value pair in List,
    % calls the closure on the key K. If Pred(K) is true, the key-value pair
    % is included in TrueList; otherwise, it is included in FalseList.
    %
:- pred filter(pred(K)::in(pred(in) is semidet),
    kv_list(K, V)::in, kv_list(K, V)::out, kv_list(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Merging kv_lists.
%

    % merge(L1, L2, L):
    %
    % L is the result of merging the elements of L1 and L2, in ascending order.
    % L1 and L2 must be sorted on the keys.
    %
:- func merge(kv_list(K, V), kv_list(K, V)) = kv_list(K, V).
:- pred merge(kv_list(K, V)::in, kv_list(K, V)::in,
    kv_list(K, V)::out) is det.

%---------------------------------------------------------------------------%
%
% Folding over kv_lists.
%

    % foldl(Func, List, Start) = End calls Func
    % with each key-value in List, working left-to-right,
    % and an accumulator whose initial value is Start,
    % and returns the final value in End.
    %
:- func foldl(func(K, V, A) = A, kv_list(K, V), A) = A.

    % foldl(Pred, List, Start End) calls Pred
    % with each key-value pair in List, working left-to-right,
    % and an accumulator whose initial value is Start,
    % and returns the final value in End.
    %
:- pred foldl(pred(K, V, A, A), kv_list(K, V), A, A).
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
:- func foldl_keys(func(K, A) = A, kv_list(K, V), A) = A.

    % foldl_keys(Pred, List, Start End) calls Pred
    % with each key in List, working left-to-right, and an accumulator
    % whose initial value is Start, and returns the final value in End.
    %
:- pred foldl_keys(pred(K, A, A), kv_list(K, V), A, A).
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
:- func foldl_values(func(V, A) = A, kv_list(K, V), A) = A.

    % foldl_values(Pred, List, Start End) calls Pred
    % with each value in List, working left-to-right, and an accumulator
    % whose initial value is Start, and returns the final value in End.
    %
:- pred foldl_values(pred(V, A, A), kv_list(K, V), A, A).
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
:- pred foldl2(pred(K, V, A, A, B, B), kv_list(K, V), A, A, B, B).
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
:- pred foldl2_values(pred(V, A, A, B, B), kv_list(K, V),
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
:- pred foldl3(pred(K, V, A, A, B, B, C, C), kv_list(K, V),
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
:- pred foldl3_values(pred(V, A, A, B, B, C, C), kv_list(K, V),
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

:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module type_desc.

%---------------------------------------------------------------------------%

from_corresponding_lists(Ks, Vs) = KVs :-
    kv_list.from_corresponding_lists(Ks, Vs, KVs).

from_corresponding_lists(Ks, Vs, KVs) :-
    ( if kv_list.from_corresponding_loop(Ks, Vs, KVsPrime) then
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

:- pred from_corresponding_loop(list(K)::in, list(V)::in,
    kv_list(K,V)::out) is semidet.

from_corresponding_loop([], [], kv_nil).
from_corresponding_loop([K | Ks], [V | Vs], kv_cons(K, V, KVs)) :-
    kv_list.from_corresponding_loop(Ks, Vs, KVs).

%---------------------------------------------------------------------------%

assoc_list_to_kv_list([]) = kv_nil.
assoc_list_to_kv_list([K - V | KVs0]) = kv_cons(K, V, KVs) :-
    KVs = assoc_list_to_kv_list(KVs0).

kv_list_to_assoc_list(kv_nil) = [].
kv_list_to_assoc_list(kv_cons(K, V, KVs0)) = [K - V | KVs] :-
    KVs = kv_list_to_assoc_list(KVs0).

%---------------------------------------------------------------------------%

reverse_members(KVs) = VKs :-
    kv_list.reverse_members(KVs, VKs).

reverse_members(kv_nil, kv_nil).
reverse_members(kv_cons(K, V, KVs), kv_cons(V, K, VKs)) :-
    kv_list.reverse_members(KVs, VKs).

keys(KVs) = Ks :-
    kv_list.keys(KVs, Ks).

keys(kv_nil, []).
keys(kv_cons(K, _, KVs), [K | Ks]) :-
    kv_list.keys(KVs, Ks).

values(AL) = Vs :-
    kv_list.values(AL, Vs).

values(kv_nil, []).
values(kv_cons(_, V, KVs), [V | Vs]) :-
    kv_list.values(KVs, Vs).

keys_and_values(kv_nil, [], []).
keys_and_values(kv_cons(K, V, KVs), [K | Ks], [V | Vs]) :-
    kv_list.keys_and_values(KVs, Ks, Vs).

%---------------------------------------------------------------------------%

search(kv_cons(K, V, KVs), Key, Value) :-
    ( if K = Key then
        Value = V
    else
        kv_list.search(KVs, Key, Value)
    ).

lookup(KVs, K, V) :-
    ( if kv_list.search(KVs, K, VPrime) then
        V = VPrime
    else
        report_lookup_error("kv_list.lookup: key not found", K)
    ).

KVs ^ elem(K) = V :-
    kv_list.search(KVs, K, V).

KVs ^ det_elem(K) = V :-
    kv_list.lookup(KVs, K, V).

%---------------------------------------------------------------------------%

update(Key, Value, KVs0, KVs) :-
    require_complete_switch [KVs0]
    (
        KVs0 = kv_nil,
        fail
    ;
        KVs0 = kv_cons(K, V, TailKVs0),
        ( if Key = K then
            KVs = kv_cons(K, Value, TailKVs0)
        else
            update(Key, Value, TailKVs0, TailKVs),
            KVs = kv_cons(K, V, TailKVs)
        )
    ).

%---------------------------------------------------------------------------%

remove(kv_cons(HeadK, HeadV, TailKVs), Key, Value, FilteredKVs) :-
    ( if HeadK = Key then
        Value = HeadV,
        FilteredKVs = TailKVs
    else
        kv_list.remove(TailKVs, Key, Value, FilteredTailKVs),
        FilteredKVs = kv_cons(HeadK, HeadV, FilteredTailKVs)
    ).

svremove(Key, Value, !KVs) :-
    kv_list.remove(!.KVs, Key, Value, !:KVs).

%---------------------------------------------------------------------------%

map_keys_only(_F, kv_nil) = kv_nil.
map_keys_only(F, kv_cons(HeadK0, HeadV, TailKVs0))
        = kv_cons(HeadK, HeadV, TailKVs) :-
    HeadK = F(HeadK0),
    TailKVs = kv_list.map_keys_only(F, TailKVs0).

map_keys_only(_P, kv_nil, kv_nil).
map_keys_only(P,
        kv_cons(HeadK0, HeadV, TailKVs0), kv_cons(HeadK, HeadV, TailKVs)) :-
    P(HeadK0, HeadK),
    kv_list.map_keys_only(P, TailKVs0, TailKVs).

map_values_only(_F, kv_nil) = kv_nil.
map_values_only(F, kv_cons(HeadK, HeadV0, TailKVs0))
        = kv_cons(HeadK, HeadV, TailKVs) :-
    HeadV = F(HeadV0),
    TailKVs = kv_list.map_values_only(F, TailKVs0).

map_values_only(_P, kv_nil, kv_nil).
map_values_only(P,
        kv_cons(HeadK, HeadV0, TailKVs0), kv_cons(HeadK, HeadV, TailKVs)) :-
    P(HeadV0, HeadV),
    kv_list.map_values_only(P, TailKVs0, TailKVs).

map_values(_F, kv_nil) = kv_nil.
map_values(F, kv_cons(HeadK, HeadV0, TailKVs0)) =
        kv_cons(HeadK, HeadV, TailKVs) :-
    HeadV = F(HeadK, HeadV0),
    TailKVs = kv_list.map_values(F, TailKVs0).

map_values(_P, kv_nil, kv_nil).
map_values(P,
        kv_cons(HeadK, HeadV0, TailKVs0), kv_cons(HeadK, HeadV, TailKVs)) :-
    P(HeadK, HeadV0, HeadV),
    kv_list.map_values(P, TailKVs0, TailKVs).

%---------------------------------------------------------------------------%

filter(P, List) = Trues :-
    kv_list.filter(P, List, Trues).

filter(_, kv_nil,  kv_nil).
filter(P, kv_cons(HeadK, HeadV, TailKVs), TrueKVs) :-
    ( if P(HeadK) then
        kv_list.filter(P, TailKVs, TailTrueKVs),
        TrueKVs = kv_cons(HeadK, HeadV, TailTrueKVs)
    else
        kv_list.filter(P, TailKVs, TrueKVs)
    ).

negated_filter(P, List) = Falses :-
    kv_list.negated_filter(P, List, Falses).

negated_filter(_, kv_nil,  kv_nil).
negated_filter(P, kv_cons(HeadK, HeadV, TailKVs), FalseKVs) :-
    ( if P(HeadK) then
        kv_list.negated_filter(P, TailKVs, FalseKVs)
    else
        kv_list.negated_filter(P, TailKVs, TailFalseKVs),
        FalseKVs = kv_cons(HeadK, HeadV, TailFalseKVs)
    ).

filter(_, kv_nil,  kv_nil, kv_nil).
filter(P, kv_cons(HeadK, HeadV, TailKVs), TrueKVs, FalseKVs) :-
    ( if P(HeadK) then
        kv_list.filter(P, TailKVs, TailTrueKVs, FalseKVs),
        TrueKVs = kv_cons(HeadK, HeadV, TailTrueKVs)
    else
        kv_list.filter(P, TailKVs, TrueKVs, TailFalseKVs),
        FalseKVs = kv_cons(HeadK, HeadV, TailFalseKVs)
    ).

%---------------------------------------------------------------------------%

merge(As, Bs) = ABs :-
    kv_list.merge(As, Bs, ABs).

merge(kv_nil, B, B).
merge(A @ kv_cons(_, _, _), kv_nil, A).
merge(A @ kv_cons(AK, AV, AKVs), B @ kv_cons(BK, BV, BKVs), C) :-
    ( if compare(>, AK, BK) then
        kv_list.merge(A, BKVs, CKVs),
        C = kv_cons(BK, BV, CKVs)
    else
        % If compare((=), AK, BK), take A first.
        kv_list.merge(AKVs, B, CKVs),
        C = kv_cons(AK, AV, CKVs)
    ).

%---------------------------------------------------------------------------%

foldl(_F, kv_nil, Acc) = Acc.
foldl(F, kv_cons(K, V, TailKVs), !.Acc) = !:Acc :-
    !:Acc = F(K, V, !.Acc),
    !:Acc = foldl(F, TailKVs, !.Acc).

foldl(_P, kv_nil, !Acc).
foldl(P, kv_cons(K, V, TailKVs), !Acc) :-
    P(K, V, !Acc),
    foldl(P, TailKVs, !Acc).

foldl_keys(_F, kv_nil, Acc) = Acc.
foldl_keys(F, kv_cons(K, _V, KVs), !.Acc) = !:Acc :-
    !:Acc = F(K, !.Acc),
    !:Acc = kv_list.foldl_keys(F, KVs, !.Acc).

foldl_keys(_P, kv_nil, !Acc).
foldl_keys(P, kv_cons(K, _V, KVs), !Acc) :-
    P(K, !Acc),
    kv_list.foldl_keys(P, KVs, !Acc).

foldl_values(_F, kv_nil, Acc) = Acc.
foldl_values(F, kv_cons(_K, V, KVs), !.Acc) = !:Acc :-
    !:Acc = F(V, !.Acc),
    !:Acc = kv_list.foldl_values(F, KVs, !.Acc).

foldl_values(_P, kv_nil, !Acc).
foldl_values(P, kv_cons(_K, V, KVs), !Acc) :-
    P(V, !Acc),
    kv_list.foldl_values(P, KVs, !Acc).

foldl2(_P, kv_nil, !A, !B).
foldl2(P, kv_cons(K, V, TailKVs), !A, !B) :-
    P(K, V, !A, !B),
    foldl2(P, TailKVs, !A, !B).

foldl2_values(_P, kv_nil, !Acc1, !Acc2).
foldl2_values(P, kv_cons(_K, V, KVs), !Acc1, !Acc2) :-
    P(V, !Acc1, !Acc2),
    kv_list.foldl2_values(P, KVs, !Acc1, !Acc2).

foldl3(_P, kv_nil, !A, !B, !C).
foldl3(P, kv_cons(K, V, TailKVs), !A, !B, !C) :-
    P(K, V, !A, !B, !C),
    foldl3(P, TailKVs, !A, !B, !C).

foldl3_values(_P, kv_nil, !Acc1, !Acc2, !Acc3).
foldl3_values(P, kv_cons(_K, V, KVs), !Acc1, !Acc2, !Acc3) :-
    P(V, !Acc1, !Acc2, !Acc3),
    kv_list.foldl3_values(P, KVs, !Acc1, !Acc2, !Acc3).

%---------------------------------------------------------------------------%
:- end_module kv_list.
%---------------------------------------------------------------------------%
