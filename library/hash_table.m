%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2001, 2003-2006, 2010-2012 The University of Melbourne
% Copyright (C) 2013-2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: hash_table.m.
% Main author: rafe, wangp.
% Stability: low.
%
% Hash table implementation.
%
% This implementation requires the user to supply a predicate that
% computes a hash value for any given key.
%
% Default hash functions are provided for ints, strings and generic values.
%
% The number of buckets in the hash table is always a power of 2.
%
% When the occupancy reaches a level set by the user, we create automatically
% a new hash table with double the number of buckets, insert the contents
% of the old table into it, and use it to replace the old one.
%
% CAVEAT: The warning at the head of array.m about the use of unique objects
% also applies here. Briefly, the problem is that the compiler does not yet
% properly understand unique modes, hence we fake it using non-unique modes.
% This means that care must be taken not to use an old version of a
% destructively updated structure (such as a hash_table) since the
% compiler will not currently detect such errors.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hash_table.
:- interface.

:- import_module array.
:- import_module assoc_list.
:- import_module char.

%---------------------------------------------------------------------------%

:- type hash_table(K, V).

    % XXX This is all fake until the compiler can handle nested unique modes.
    %
:- inst hash_table for hash_table/2
    == bound(ht(ground, ground, hash_pred, array)).
:- mode hash_table_ui == in(hash_table).
:- mode hash_table_di == di(hash_table).
:- mode hash_table_uo == out(hash_table).

:- type hash_pred(K) == ( pred(K, int) ).
:- inst hash_pred    == ( pred(in, out) is det ).

%---------------------------------------------------------------------------%

    % init(HashPred, N, MaxOccupancy):
    %
    % Constructs a new hash table whose initial size is 2 ^ N, and whose
    % size is doubled whenever MaxOccupancy is achieved. Elements are
    % indexed using HashPred.
    %
    % HashPred must compute a hash for a given key.
    % N must be greater than 0.
    % MaxOccupancy must be in (0.0, 1.0).
    %
    % XXX Values too close to the limits may cause bad things to happen.
    %
:- func init(hash_pred(K), int, float) = hash_table(K, V).
:- mode init(in(hash_pred), in, in) = hash_table_uo is det.

    % init_default(HashFn) constructs a hash table with default size and
    % occupancy arguments.
    %
:- func init_default(hash_pred(K)) = hash_table(K, V).
:- mode init_default(in(hash_pred)) = hash_table_uo is det.

%---------------------------------------------------------------------------%

    % Retrieve the hash_pred associated with a hash table.
    %
:- func hash_pred(hash_table(K, V)) = hash_pred(K).
:- mode hash_pred(hash_table_ui) = out(hash_pred) is det.

    % Returns the number of buckets in a hash table.
    %
:- func num_buckets(hash_table(K, V)) = int.
:- mode num_buckets(hash_table_ui) = out is det.
% :- mode num_buckets(in) = out is det.

    % Returns the number of occupants in a hash table.
    %
:- func num_occupants(hash_table(K, V)) = int.
:- mode num_occupants(hash_table_ui) = out is det.
% :- mode num_occupants(in) = out is det.

%---------------------------------------------------------------------------%

    % Copy the hash table.
    %
    % This is not a deep copy, it copies only enough of the structure to
    % create a new unique table.
    %
:- func copy(hash_table(K, V)) = hash_table(K, V).
:- mode copy(hash_table_ui) = hash_table_uo is det.

    % Insert key-value binding into a hash table; if one is already there,
    % then overwrite the previous value.
    %
:- func set(hash_table(K, V), K, V) = hash_table(K, V).
:- mode set(hash_table_di, in, in) = hash_table_uo is det.

:- pred set(K::in, V::in,
    hash_table(K, V)::hash_table_di, hash_table(K, V)::hash_table_uo) is det.

    % Field update for hash tables.
    % HT ^ elem(K) := V is equivalent to set(HT, K, V).
    %
:- func 'elem :='(K, hash_table(K, V), V) = hash_table(K, V).
:- mode 'elem :='(in, hash_table_di, in) = hash_table_uo is det.

    % Insert a key-value binding into a hash table. Throw an exception
    % if a binding for the key is already present.
    %
:- func det_insert(hash_table(K, V), K, V) = hash_table(K, V).
:- mode det_insert(hash_table_di, in, in) = hash_table_uo is det.

:- pred det_insert(K::in, V::in,
    hash_table(K, V)::hash_table_di, hash_table(K, V)::hash_table_uo) is det.

    % Change a key-value binding in a hash table. Throw an exception
    % if a binding for the key does not already exist.
    %
:- func det_update(hash_table(K, V), K, V) = hash_table(K, V).
:- mode det_update(hash_table_di, in, in) = hash_table_uo is det.

:- pred det_update(K::in, V::in,
    hash_table(K, V)::hash_table_di, hash_table(K, V)::hash_table_uo) is det.

    % Delete the entry for the given key, leaving the hash table
    % unchanged if there is no such entry.
    %
:- func delete(hash_table(K, V), K) = hash_table(K, V).
:- mode delete(hash_table_di, in) = hash_table_uo is det.

:- pred delete(K::in,
    hash_table(K, V)::hash_table_di, hash_table(K, V)::hash_table_uo) is det.

%---------------------------------------------------------------------------%

    % Lookup the value associated with the given key.
    % Fail if there is no entry for the key.
    %
:- func search(hash_table(K, V), K) = V.
:- mode search(hash_table_ui, in) = out is semidet.
% :- mode search(in, in, out) is semidet.

:- pred search(hash_table(K, V), K, V).
:- mode search(hash_table_ui, in, out) is semidet.
% :- mode search(in, in, out) is semidet.

    % Lookup the value associated with the given key.
    % Throw an exception if there is no entry for the key.
    %
:- func lookup(hash_table(K, V), K) = V.
:- mode lookup(hash_table_ui, in) = out is det.
% :- mode lookup(in, in) = out is det.

    % Field access for hash tables.
    % HT ^ elem(K) is equivalent to lookup(HT, K).
    %
:- func elem(K, hash_table(K, V)) = V.
:- mode elem(in, hash_table_ui) = out is det.
% :- mode elem(in, in) = out is det.

%---------------------------------------------------------------------------%

    % Convert a hash table into an association list.
    %
:- func to_assoc_list(hash_table(K, V)) = assoc_list(K, V).
:- mode to_assoc_list(hash_table_ui) = out is det.
% :- mode to_assoc_list(in) = out is det.

    % from_assoc_list(HashPred, N, MaxOccupancy, AssocList) = Table:
    %
    % Convert an association list into a hash table. The first three
    % parameters are the same as for init/3 above.
    %
:- func from_assoc_list(hash_pred(K), int, float, assoc_list(K, V)) =
    hash_table(K, V).
:- mode from_assoc_list(in(hash_pred), in, in, in) = hash_table_uo is det.

    % A simpler version of from_assoc_list/4, the values for N and
    % MaxOccupancy are configured with defaults such as in init_default/1
    %
:- func from_assoc_list(hash_pred(K)::in(hash_pred), assoc_list(K, V)::in) =
    (hash_table(K, V)::hash_table_uo) is det.

    % Fold a function over the key-value bindings in a hash table.
    %
:- func fold(func(K, V, T) = T, hash_table(K, V), T) = T.
:- mode fold(func(in, in, in) = out is det, hash_table_ui, in) = out is det.
:- mode fold(func(in, in, di) = uo is det, hash_table_ui, di) = uo is det.

    % Fold a predicate over the key-value bindings in a hash table.
    %
:- pred fold(pred(K, V, A, A), hash_table(K, V), A, A).
:- mode fold(in(pred(in, in, in, out) is det), hash_table_ui,
    in, out) is det.
:- mode fold(in(pred(in, in, mdi, muo) is det), hash_table_ui,
    mdi, muo) is det.
:- mode fold(in(pred(in, in, di, uo) is det), hash_table_ui,
    di, uo) is det.
:- mode fold(in(pred(in, in, in, out) is semidet), hash_table_ui,
    in, out) is semidet.
:- mode fold(in(pred(in, in, mdi, muo) is semidet), hash_table_ui,
    mdi, muo) is semidet.
:- mode fold(in(pred(in, in, di, uo) is semidet), hash_table_ui,
    di, uo) is semidet.

:- pred fold2(pred(K, V, A, A, B, B), hash_table(K, V), A, A, B, B).
:- mode fold2(in(pred(in, in, in, out, in, out) is det), hash_table_ui,
    in, out, in, out) is det.
:- mode fold2(in(pred(in, in, in, out, mdi, muo) is det), hash_table_ui,
    in, out, mdi, muo) is det.
:- mode fold2(in(pred(in, in, in, out, di, uo) is det), hash_table_ui,
    in, out, di, uo) is det.
:- mode fold2(in(pred(in, in, in, out, in, out) is semidet), hash_table_ui,
    in, out, in, out) is semidet.
:- mode fold2(in(pred(in, in, in, out, mdi, muo) is semidet), hash_table_ui,
    in, out, mdi, muo) is semidet.
:- mode fold2(in(pred(in, in, in, out, di, uo) is semidet), hash_table_ui,
    in, out, di, uo) is semidet.

:- pred fold3(pred(K, V, A, A, B, B, C, C), hash_table(K, V), A, A, B, B,
    C, C).
:- mode fold3(in(pred(in, in, in, out, in, out, in, out) is det),
    hash_table_ui, in, out, in, out, in, out) is det.
:- mode fold3(in(pred(in, in, in, out, in, out, mdi, muo) is det),
    hash_table_ui, in, out, in, out, mdi, muo) is det.
:- mode fold3(in(pred(in, in, in, out, in, out, di, uo) is det),
    hash_table_ui, in, out, in, out, di, uo) is det.
:- mode fold3(in(pred(in, in, in, out, in, out, in, out) is semidet),
    hash_table_ui, in, out, in, out, in, out) is semidet.
:- mode fold3(in(pred(in, in, in, out, in, out, mdi, muo) is semidet),
    hash_table_ui, in, out, in, out, mdi, muo) is semidet.
:- mode fold3(in(pred(in, in, in, out, in, out, di, uo) is semidet),
    hash_table_ui, in, out, in, out, di, uo) is semidet.

%---------------------------------------------------------------------------%

    % Default hash_preds for ints and strings and everything (buwahahaha!)
    %
:- pragma obsolete(int_hash/2, [int.hash/2]).
:- pred int_hash(int::in, int::out) is det.
:- pragma obsolete(uint_hash/2, [uint.hash/2]).
:- pred uint_hash(uint::in, int::out) is det.
:- pragma obsolete(float_hash/2, [float.hash/2]).
:- pred float_hash(float::in, int::out) is det.
:- pragma obsolete(char_hash/2, [char.hash/2]).
:- pred char_hash(char::in, int::out) is det.
:- pragma obsolete(string_hash/2, [string.hash/2]).
:- pred string_hash(string::in, int::out) is det.
:- pragma obsolete(generic_hash/2).
:- pred generic_hash(T::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module deconstruct.
:- import_module float.
:- import_module int.
:- import_module kv_list.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module univ.

%---------------------------------------------------------------------------%

:- interface.

    % This should be abstract, but needs to be exported for insts.
    % We should consider using a mutable for num_occupants.
    %
:- type hash_table(K, V)
    --->    ht(
                num_occupants           :: int,
                max_occupants           :: int,
                hash_pred               :: hash_pred(K),
                buckets                 :: array(hash_bucket(K, V))
            ).

    % This needs to be exported for use in the export of hash_table(K, V).
    %
:- type hash_bucket(K, V).

%---------------------------------------------------------------------------%

:- implementation.

    % We use a custom association list representation for better performance.
    %
    % Array bounds checks may be omitted in this module because the array
    % indices are computed by: hash(Key) mod size(Array)
    %
:- type hash_bucket_array(K, V) == array(hash_bucket(K, V)).

    % Assuming a decent hash function, there should be few collisions,
    % so each bucket will usually contain an empty list or a singleton.
    % Including a singleton constructor therefore reduces memory consumption.
    %
:- type hash_bucket(K, V)
    --->    hb_zero
    ;       hb_one(K, V)
    ;       hb_two_plus(K, V, K, V, kv_list(K, V)).

:- inst hb_two_plus for hash_bucket/2
    --->    hb_two_plus(ground, ground, ground, ground, ground).

%---------------------------------------------------------------------------%

init(HashPred, N, MaxOccupancy) = HT :-
    ( if N =< 0 then
        error($pred, "N =< 0")
    else if N >= int.bits_per_int then
        error($pred, "N >= int.bits_per_int")
    else if MaxOccupancy =< 0.0 then
        error($pred, "MaxOccupancy =< 0.0")
    else
        NumBuckets = 1 << N,
        MaxOccupants = ceiling_to_int(float(NumBuckets) * MaxOccupancy),
        Buckets = init(NumBuckets, hb_zero),
        HT = ht(0, MaxOccupants, HashPred, Buckets)
    ).

%---------------------------------------------------------------------------%

    % The initial numbers 7 and .9 were picked out of thin air.
    %
    % We now use .875 (7/8) instead .9 because it is exactly representable
    % in binary. This avoids differences in rounding between 32 and 64 bit
    % platforms, which can show up as differences between the stage 2 and 3
    % versions of the code we generate for this module during a bootcheck
    % in the C# and Java grades.
    %
init_default(HashPred) = init(HashPred, 7, 0.875).

%---------------------------------------------------------------------------%

num_buckets(HT) = size(HT ^ buckets).

% num_occupants is generated automatically, as a field get function.

%---------------------------------------------------------------------------%

copy(Orig) = Copy :-
    Orig = ht(NumOccupants, MaxOccupants, HashPred, Buckets0),
    array.copy(Buckets0, Buckets),
    Copy = ht(NumOccupants, MaxOccupants, HashPred, Buckets).

%---------------------------------------------------------------------------%

set(HT0, Key, Value) = HT :-
    set(Key, Value, HT0, HT).

set(Key, Value, HT0, HT) :-
    HashSlot = find_slot(HT0, Key),
    HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0),
    array.unsafe_lookup(Buckets0, HashSlot, HB0),
    (
        HB0 = hb_zero,
        HB = hb_one(Key, Value),
        InsertedNew = yes
    ;
        HB0 = hb_one(K0, V0),
        ( if K0 = Key then
            HB = hb_one(K0, Value),
            InsertedNew = no
        else
            HB = hb_two_plus(Key, Value, K0, V0, kv_nil),
            InsertedNew = yes
        )
    ;
        HB0 = hb_two_plus(K0, V0, K1, V1, KVs0),
        ( if update_item_in_bucket(Key, Value, HB0, HB1) then
            HB = HB1,
            InsertedNew = no
        else
            HB = hb_two_plus(Key, Value, K0, V0, kv_cons(K1, V1, KVs0)),
            InsertedNew = yes
        )
    ),
    array.unsafe_set(HashSlot, HB, Buckets0, Buckets),
    (
        InsertedNew = no,
        HT = ht(NumOccupants0, MaxOccupants, HashPred, Buckets)
    ;
        InsertedNew = yes,
        NumOccupants = NumOccupants0 + 1,
        ( if NumOccupants > MaxOccupants then
            HT = expand(NumOccupants, MaxOccupants, HashPred, Buckets)
        else
            HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
        )
    ).

'elem :='(K, HT, V) = set(HT, K, V).

:- func find_slot(hash_table(K, V), K) = int.
:- mode find_slot(hash_table_ui, in) = out is det.
% :- mode find_slot(in, in) = out is det.
:- pragma inline(find_slot/2).

find_slot(HT, K) = HashSlot :-
    find_slot_2(HT ^ hash_pred, K, HT ^ num_buckets, HashSlot).

:- pred find_slot_2(hash_pred(K)::in(hash_pred), K::in, int::in, int::out)
    is det.
:- pragma inline(find_slot_2/4).

find_slot_2(HashPred, K, NumBuckets, HashSlot) :-
    HashPred(K, Hash),
    % Since NumBuckets is a power of two, we can avoid mod.
    HashSlot = Hash /\ (NumBuckets - 1).

:- pred update_item_in_bucket(K, V, hash_bucket(K, V), hash_bucket(K, V)).
:- mode update_item_in_bucket(in, in, in(hb_two_plus), out) is semidet.
:- mode update_item_in_bucket(in, in, in, out) is semidet.

update_item_in_bucket(Key, Value, HB0, HB) :-
    % The procedure body here is a NOT switch on HB0 in the first mode.
    % This is because mode analysis eliminates the first two disjuncts,
    % since they cannot possibly generate any solutions, and then removes
    % the disj() wrapper around what has become a one-disjunct disjunction.
    %
    % require_complete_switch [HB0]
    (
        HB0 = hb_zero,
        fail
    ;
        HB0 = hb_one(K, _),
        ( if K = Key then
            HB = hb_one(K, Value)
        else
            fail
        )
    ;
        HB0 = hb_two_plus(K0, V0, K1, V1, KVs0),
        ( if K0 = Key then
            HB = hb_two_plus(K0, Value, K1, V1, KVs0)
        else if K1 = Key then
            HB = hb_two_plus(K0, V0, K1, Value, KVs0)
        else
            kv_list.update(Key, Value, KVs0, KVs),
            HB = hb_two_plus(K0, V0, K1, V1, KVs)
        )
    ).

%---------------------------------------------------------------------------%

det_insert(HT0, Key, Value) = HT :-
    det_insert(Key, Value, HT0, HT).

det_insert(Key, Value, HT0, HT) :-
    HashSlot = find_slot(HT0, Key),
    HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0),
    array.unsafe_lookup(Buckets0, HashSlot, HB0),
    (
        HB0 = hb_zero,
        HB = hb_one(Key, Value)
    ;
        HB0 = hb_one(K0, V0),
        ( if K0 = Key then
            error($pred, "key already present")
        else
            HB = hb_two_plus(Key, Value, K0, V0, kv_nil)
        )
    ;
        HB0 = hb_two_plus(K0, V0, K1, V1, KVs),
        ( if K0 = Key then
            error($pred, "key already present")
        else if K1 = Key then
            error($pred, "key already present")
        else
            HB = hb_two_plus(Key, Value, K0, V0, kv_cons(K1, V1, KVs))
        )
    ),
    array.unsafe_set(HashSlot, HB, Buckets0, Buckets),
    NumOccupants = NumOccupants0 + 1,
    ( if NumOccupants > MaxOccupants then
        HT = expand(NumOccupants, MaxOccupants, HashPred, Buckets)
    else
        HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
    ).

%---------------------------------------------------------------------------%

det_update(!.HT, Key, Value) = !:HT :-
    det_update(Key, Value, !HT).

det_update(Key, Value, !HT) :-
    HashSlot = find_slot(!.HT, Key),
    Buckets0 = !.HT ^ buckets,
    array.unsafe_lookup(Buckets0, HashSlot, HB0),
    ( if update_item_in_bucket(Key, Value, HB0, HB1) then
        HB = HB1
    else
        error($pred, "key not found")
    ),
    array.unsafe_set(HashSlot, HB, Buckets0, Buckets),
    !HT ^ buckets := Buckets.

%---------------------------------------------------------------------------%

delete(HT0, Key) = HT :-
    delete(Key, HT0, HT).

delete(Key, HT0, HT) :-
    HashSlot = find_slot(HT0, Key),
    array.unsafe_lookup(HT0 ^ buckets, HashSlot, HB0),
    ( if hash_bucket_remove(Key, HB0, HB) then
        HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0),
        NumOccupants = NumOccupants0 - 1,
        array.unsafe_set(HashSlot, HB, Buckets0, Buckets),
        HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
    else
        HT = HT0
    ).

:- pred hash_bucket_remove(K::in,
    hash_bucket(K, V)::in, hash_bucket(K, V)::out) is semidet.

hash_bucket_remove(Key, HB0, HB) :-
    require_complete_switch [HB0]
    (
        HB0 = hb_zero,
        fail
    ;
        HB0 = hb_one(K, _),
        ( if K = Key then
            HB = hb_zero
        else
            fail
        )
    ;
        HB0 = hb_two_plus(K0, V0, K1, V1, KVs0),
        ( if K0 = Key then
            (
                KVs0 = kv_nil,
                HB = hb_one(K1, V1)
            ;
                KVs0 = kv_cons(K2, V2, TailKVs),
                HB = hb_two_plus(K1, V1, K2, V2, TailKVs)
            )
        else if K1 = Key then
            (
                KVs0 = kv_nil,
                HB = hb_one(K0, V0)
            ;
                KVs0 = kv_cons(K2, V2, TailKVs),
                HB = hb_two_plus(K0, V0, K2, V2, TailKVs)
            )
        else
            kv_list.svremove(Key, _Value, KVs0, KVs),
            HB = hb_two_plus(K0, V0, K1, V1, KVs)
        )
    ).

%---------------------------------------------------------------------------%

search(HT, Key) = Value :-
    search(HT, Key, Value).

search(HT, Key, Value) :-
    HashSlot = find_slot(HT, Key),
    array.unsafe_lookup(HT ^ buckets, HashSlot, HB),
    hash_bucket_search(HB, Key, Value).

:- pred hash_bucket_search(hash_bucket(K, V)::in, K::in, V::out) is semidet.

hash_bucket_search(HB, Key, Value) :-
    require_complete_switch [HB]
    (
        HB = hb_zero,
        fail
    ;
        HB = hb_one(K, V),
        ( if K = Key then
            Value = V
        else
            fail
        )
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        ( if K0 = Key then
            Value = V0
        else if K1 = Key then
            Value = V1
        else
            kv_list.search(KVs, Key, Value)
        )
    ).

%---------------------------------------------------------------------------%

lookup(HT, K) =
    ( if V = search(HT, K) then
        V
    else
        func_error($pred, "key not found")
    ).

% XXX The convention in other library modules is that
% - elem is shorthand for search, NOT lookup, and
% - det_elem is shorthand for lookup.
elem(K, HT) = lookup(HT, K).

%---------------------------------------------------------------------------%

to_assoc_list(HT) = AL :-
    array.foldl(acc_assoc_list, HT ^ buckets, [], AL).

:- pred acc_assoc_list(hash_bucket(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

acc_assoc_list(HB, !AL) :-
    (
        HB = hb_zero
    ;
        HB = hb_one(K, V),
        !:AL = [K - V | !.AL]
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        !:AL = [K0 - V0 | !.AL],
        !:AL = [K1 - V1 | !.AL],
        kv_acc_assoc_list(KVs, !AL)
    ).

:- pred kv_acc_assoc_list(kv_list(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

kv_acc_assoc_list(KVs, !AL) :-
    (
        KVs = kv_nil
    ;
        KVs = kv_cons(K, V, TailKVs),
        !:AL = [K - V | !.AL],
        kv_acc_assoc_list(TailKVs, !AL)
    ).

from_assoc_list(HashPred, N, MaxOccupants, AL) = HT :-
    HT0 = init(HashPred, N, MaxOccupants),
    from_assoc_list_loop(AL, HT0, HT).

from_assoc_list(HashPred, AL) = HT :-
    HT0 = init_default(HashPred),
    from_assoc_list_loop(AL, HT0, HT).

:- pred from_assoc_list_loop(assoc_list(K, V)::in,
    hash_table(K, V)::hash_table_di, hash_table(K, V)::hash_table_uo) is det.

from_assoc_list_loop([], !HT).
from_assoc_list_loop([K - V | T], !HT) :-
    set(K, V, !HT),
    from_assoc_list_loop(T, !HT).

%---------------------------------------------------------------------------%

    % Hash tables expand by doubling in size.
    %
    % Ensuring expand/4 is _not_ inlined into hash_table.det_insert, etc.
    % actually makes those predicates more efficient.
    % expand calls array.init, which implicitly takes a type_info for
    % hash_bucket(K, V) that has to be created dynamically.
    % array.init is not fully opt-exported so the unused type_info
    % argument is not eliminated, nor is the creation of the type_info
    % delayed until the (rare) call to expand.
    %
:- func expand(int::in, int::in, hash_pred(K)::in(hash_pred),
    hash_bucket_array(K, V)::in) = (hash_table(K, V)::hash_table_uo) is det.
:- pragma no_inline(expand/4).

expand(NumOccupants, MaxOccupants0, HashPred, Buckets0) = HT :-
    NumBuckets0 = array.size(Buckets0),
    NumBuckets = NumBuckets0 + NumBuckets0,
    MaxOccupants = MaxOccupants0 + MaxOccupants0,
    array.init(NumBuckets, hb_zero, Buckets1),
    unsafe_insert_hash_buckets(0, Buckets0, HashPred, NumBuckets,
        Buckets1, Buckets),
    HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets).

:- pred unsafe_insert_hash_buckets(int::in, hash_bucket_array(K, V)::array_ui,
    hash_pred(K)::in(hash_pred), int::in,
    hash_bucket_array(K, V)::array_di, hash_bucket_array(K, V)::array_uo)
    is det.

unsafe_insert_hash_buckets(I, OldBuckets, HashPred, NumBuckets, !Buckets) :-
    ( if I >= array.size(OldBuckets) then
        true
    else
        array.unsafe_lookup(OldBuckets, I, HB),
        unsafe_insert_hash_bucket(HB, HashPred, NumBuckets, !Buckets),
        unsafe_insert_hash_buckets(I + 1, OldBuckets, HashPred, NumBuckets,
            !Buckets)
    ).

:- pred unsafe_insert_hash_bucket(hash_bucket(K, V)::in,
    hash_pred(K)::in(hash_pred), int::in,
    hash_bucket_array(K, V)::array_di, hash_bucket_array(K, V)::array_uo)
    is det.

unsafe_insert_hash_bucket(HB, HashPred, NumBuckets, !Buckets) :-
    (
        HB = hb_zero
    ;
        HB = hb_one(K, V),
        unsafe_insert(K, V, HashPred, NumBuckets, !Buckets)
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        unsafe_insert(K0, V0, HashPred, NumBuckets, !Buckets),
        unsafe_insert(K1, V1, HashPred, NumBuckets, !Buckets),
        unsafe_insert_kv_list(KVs, HashPred, NumBuckets, !Buckets)
    ).

:- pred unsafe_insert_kv_list(kv_list(K, V)::in,
    hash_pred(K)::in(hash_pred), int::in,
    hash_bucket_array(K, V)::array_di, hash_bucket_array(K, V)::array_uo)
    is det.

unsafe_insert_kv_list(KVs, HashPred, NumBuckets, !Buckets) :-
    (
        KVs = kv_nil
    ;
        KVs = kv_cons(K, V, TailKVs),
        unsafe_insert(K, V, HashPred, NumBuckets, !Buckets),
        unsafe_insert_kv_list(TailKVs, HashPred, NumBuckets, !Buckets)
    ).

:- pred unsafe_insert(K::in, V::in, hash_pred(K)::in(hash_pred), int::in,
    hash_bucket_array(K, V)::array_di, hash_bucket_array(K, V)::array_uo)
    is det.

unsafe_insert(Key, Value, HashPred, NumBuckets, !Buckets) :-
    find_slot_2(HashPred, Key, NumBuckets, HashSlot),
    array.unsafe_lookup(!.Buckets, HashSlot, HB0),
    % Unlike det_insert, we *assume* that Key is not already in HB0.
    % This assumption is justified in that "no duplicate keys"
    % is an invariant that the old hash table whose size we are now
    % doubling should have maintained.
    (
        HB0 = hb_zero,
        HB = hb_one(Key, Value)
    ;
        HB0 = hb_one(K0, V0),
        HB = hb_two_plus(Key, Value, K0, V0, kv_nil)
    ;
        HB0 = hb_two_plus(K0, V0, K1, V1, KVs),
        HB = hb_two_plus(Key, Value, K0, V0, kv_cons(K1, V1, KVs))
    ),
    array.unsafe_set(HashSlot, HB, !Buckets).

%---------------------------------------------------------------------------%

fold(F, HT, A0) = A :-
    array.foldl(fold_f(F), HT ^ buckets, A0, A).

:- pred fold_f(func(K, V, T) = T, hash_bucket(K, V), T, T).
:- mode fold_f(func(in, in, in) = out is det, in, in, out) is det.
:- mode fold_f(func(in, in, di) = uo is det, in, di, uo) is det.

fold_f(F, HB, !A) :-
    (
        HB = hb_zero
    ;
        HB = hb_one(K, V),
        !:A = F(K, V, !.A)
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        !:A = F(K0, V0, !.A),
        !:A = F(K1, V1, !.A),
        foldlf(F, KVs, !A)
    ).

:- pred foldlf(func(K, V, T) = T, kv_list(K, V), T, T).
:- mode foldlf(func(in, in, in) = out is det, in, in, out) is det.
:- mode foldlf(func(in, in, di) = uo is det, in, di, uo) is det.

foldlf(F, KVs, !A) :-
    (
        KVs = kv_nil
    ;
        KVs = kv_cons(K, V, TailKVs),
        !:A = F(K, V, !.A),
        foldlf(F, TailKVs, !A)
    ).

%---------------------------------------------------------------------------%

fold(P, HT, !A) :-
    array.foldl(fold_p(P), HT ^ buckets, !A).

:- pred fold_p(pred(K, V, A, A), hash_bucket(K, V), A, A).
:- mode fold_p(pred(in, in, in, out) is det, in, in, out) is det.
:- mode fold_p(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold_p(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode fold_p(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_p(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold_p(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.

fold_p(P, HB, !A) :-
    (
        HB = hb_zero
    ;
        HB = hb_one(K, V),
        P(K, V, !A)
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        P(K0, V0, !A),
        P(K1, V1, !A),
        foldl(P, KVs, !A)
    ).

%---------------------------------------------------------------------------%

fold2(P, HT, !A, !B) :-
    array.foldl2(fold2_p(P), HT ^ buckets, !A, !B).

:- pred fold2_p(pred(K, V, A, A, B, B), hash_bucket(K, V), A, A, B, B).
:- mode fold2_p(pred(in, in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode fold2_p(pred(in, in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode fold2_p(pred(in, in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode fold2_p(pred(in, in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode fold2_p(pred(in, in, in, out, mdi, muo) is semidet, in,in, out,
    mdi, muo) is semidet.
:- mode fold2_p(pred(in, in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.

fold2_p(P, HB, !A, !B) :-
    (
        HB = hb_zero
    ;
        HB = hb_one(K, V),
        P(K, V, !A, !B)
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        P(K0, V0, !A, !B),
        P(K1, V1, !A, !B),
        foldl2(P, KVs, !A, !B)
    ).

%---------------------------------------------------------------------------%

fold3(P, HT, !A, !B, !C) :-
    array.foldl3(fold3_p(P), HT ^ buckets, !A, !B, !C).

:- pred fold3_p(pred(K, V, A, A, B, B, C, C), hash_bucket(K, V),
    A, A, B, B, C, C).
:- mode fold3_p(pred(in, in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode fold3_p(pred(in, in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode fold3_p(pred(in, in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode fold3_p(pred(in, in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode fold3_p(pred(in, in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode fold3_p(pred(in, in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.

fold3_p(P, HB, !A, !B, !C) :-
    (
        HB = hb_zero
    ;
        HB = hb_one(K, V),
        P(K, V, !A, !B, !C)
    ;
        HB = hb_two_plus(K0, V0, K1, V1, KVs),
        P(K0, V0, !A, !B, !C),
        P(K1, V1, !A, !B, !C),
        foldl3(P, KVs, !A, !B, !C)
    ).

%---------------------------------------------------------------------------%

int_hash(Key, Hash) :-
    UKey = uint.cast_from_int(Key),
    uint_hash(UKey, Hash).

uint_hash(Key, Hash) :-
    uint.hash(Key, Hash).

float_hash(F, Hash) :-
    float.hash(F, Hash).

char_hash(C, Hash) :-
    char.hash(C, Hash).

string_hash(S, Hash) :-
    string.hash(S, Hash).

%---------------------------------------------------------------------------%

generic_hash(T, Hash) :-
    % This, again, is straight off the top of [rafe's] head.
    %
    ( if dynamic_cast(T, Int) then
        int_hash(Int, Hash)
    else if dynamic_cast(T, String) then
        string_hash(String, Hash)
    else if dynamic_cast(T, Float) then
        float_hash(Float, Hash)
    else if dynamic_cast(T, Char) then
        char_hash(Char, Hash)
    else if dynamic_cast(T, Univ) then
        generic_hash(univ_value(Univ), Hash)
    else if dynamic_cast_to_array(T, Array) then
        array.foldl(hash_and_accumulate_hash_value, Array, 0, Hash)
    else
        deconstruct(T, canonicalize, FunctorName, Arity, Args),
        string_hash(FunctorName, Hash0),
        accumulate_hash_value(Arity, Hash0, Hash1),
        list.foldl(hash_and_accumulate_hash_value, Args, Hash1, Hash)
    ).

:- pragma obsolete(hash_and_accumulate_hash_value/3).
:- pred hash_and_accumulate_hash_value(T::in, int::in, int::out) is det.

hash_and_accumulate_hash_value(T, !HashAcc) :-
    generic_hash(T, HashValue),
    accumulate_hash_value(HashValue, !HashAcc).

:- pred accumulate_hash_value(int::in, int::in, int::out) is det.

accumulate_hash_value(HashValue, HashAcc0, HashAcc) :-
    % XXX This is a REALLY BAD algorithm, with shift amounts that
    % will routinely exceed the word size.
    HashAcc =
        (HashAcc0 `unchecked_left_shift` HashValue) `xor`
        (HashAcc0 `unchecked_right_shift` (int.bits_per_int - HashValue)).

%---------------------------------------------------------------------------%
:- end_module hash_table.
%---------------------------------------------------------------------------%
