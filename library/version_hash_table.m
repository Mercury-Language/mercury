%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2024 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_hash_table.m.
% Main author: rafe, wangp.
% Stability: low.
%
% (See the header comments in version_array.m for an explanation of version
% types.)
%
% Version hash tables. The "latest" version of the hash table provides
% roughly the same performance as the unique hash table implementation.
% "Older" versions of the hash table are still accessible, but will incur
% a performance penalty that grows as more updates are made to the hash table.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module version_hash_table.
:- interface.

:- import_module assoc_list.

%---------------------------------------------------------------------------%

:- type version_hash_table(K, V).

:- type hash_pred(K) == (pred(K,  int)).
:- inst hash_pred    == (pred(in, out) is det).

    % init(HashPred, N, MaxOccupancy):
    %
    % Construct a new hash table with initial size 2 ^ N that is doubled
    % whenever MaxOccupancy is achieved. Elements are indexed using HashPred.
    %
    % HashPred must compute a hash for a given key.
    % N must be greater than 0.
    % MaxOccupancy must be in (0.0, 1.0).
    %
    % XXX Values too close to the limits may cause bad things to happen.
    %
:- func init(hash_pred(K)::in(hash_pred), int::in, float::in) =
    (version_hash_table(K, V)::out) is det.

    % unsafe_init(HashPred, N, MaxOccupancy):
    %
    % Like init/3, but the constructed hash table is backed by a
    % non-thread-safe version array. It is unsafe to concurrently access
    % or update the hash table from different threads, or any two hash tables
    % which were produced from operations on the same original hash table.
    % However, if the hash table or its descendants will not be used in such a
    % manner, a non-thread-safe hash table can be much faster than a thread
    % safe one.
    %
:- func unsafe_init(hash_pred(K)::in(hash_pred), int::in, float::in) =
    (version_hash_table(K, V)::out) is det.

    % init_default(HashFn) constructs a hash table with default size and
    % occupancy arguments.
    %
:- func init_default(hash_pred(K)::in(hash_pred)) =
    (version_hash_table(K, V)::out) is det.

    % unsafe_init_default(HashFn)
    %
    % Like init_default/3 but the constructed hash table is backed by a
    % non-thread-safe version array. See the description of unsafe_init/3
    % above.
    %
:- func unsafe_init_default(hash_pred(K)::in(hash_pred)) =
    (version_hash_table(K, V)::out) is det.

    % Retrieve the hash_pred associated with a hash table.
    %
% :- func hash_pred(version_hash_table(K, V)) = hash_pred(K).

    % Return the number of buckets in a hash table.
    %
:- func num_buckets(version_hash_table(K, V)) = int.

    % Return the number of occupants in a hash table.
    %
:- func num_occupants(version_hash_table(K, V)) = int.

    % Copy the hash table explicitly.
    %
    % An explicit copy allows programmers to control the cost of copying
    % the table. For more information see the comments at the top of the
    % version_array module.
    %
    % This is not a deep copy: it copies only the structure.
    %
:- func copy(version_hash_table(K, V)) = version_hash_table(K, V).

    % Search for the value associated with the given key.
    % Fail if there is no entry for the key.
    %
:- func search(version_hash_table(K, V), K) = V is semidet.
% NOTE_TO_IMPLEMENTORS CFF :- pragma obsolete(func(search/2), [search/3]).
:- pred search(version_hash_table(K, V)::in, K::in, V::out) is semidet.

    % Lookup the value associated with the given key.
    % Throw an exception if there is no entry for the key.
    %
:- func lookup(version_hash_table(K, V), K) = V.
:- pred lookup(version_hash_table(K, V)::in, K::in, V::out) is det.

    % Field access for hash tables.
    % `HT ^ elem(K)' is equivalent to `lookup(HT, K)'.
    %
:- func elem(K, version_hash_table(K, V)) = V.

    % Insert key-value binding into a hash table.
    % If one is already there, then overwrite the previous value.
    %
:- func set(version_hash_table(K, V), K, V) = version_hash_table(K, V).
:- pred set(K::in, V::in,
    version_hash_table(K, V)::in, version_hash_table(K, V)::out) is det.

    % Field update for hash tables.
    % `HT ^ elem(K) := V' is equivalent to `set(HT, K, V)'.
    %
:- func 'elem :='(K, version_hash_table(K, V), V) = version_hash_table(K, V).

    % Insert a key-value binding into a hash table.
    % Throw an exception if a binding for the key is already present.
    %
:- func det_insert(version_hash_table(K, V), K, V) = version_hash_table(K, V).
:- pred det_insert(K::in, V::in,
    version_hash_table(K, V)::in, version_hash_table(K, V)::out) is det.

    % Change a key-value binding in a hash table.
    % Throw exception if a binding for the key does not already exist.
    %
:- func det_update(version_hash_table(K, V), K, V) = version_hash_table(K, V).
:- pred det_update(K::in, V::in,
    version_hash_table(K, V)::in, version_hash_table(K, V)::out) is det.

    % Delete the entry for the given key. If there is no such entry,
    % leave the hash table unchanged.
    %
:- func delete(version_hash_table(K, V), K) = version_hash_table(K, V).
:- pred delete(K::in,
    version_hash_table(K, V)::in, version_hash_table(K, V)::out) is det.

    % Convert a hash table into an association list.
    %
:- func to_assoc_list(version_hash_table(K, V)) = assoc_list(K, V).

    % from_assoc_list(HashPred, N, MaxOccupancy, AssocList) = Table:
    %
    % Convert an association list into a hash table. The first three parameters
    % are the same as for init/3 above.
    %
:- func from_assoc_list(hash_pred(K)::in(hash_pred), int::in, float::in,
    assoc_list(K, V)::in) = (version_hash_table(K, V)::out) is det.

    % A simpler version of from_assoc_list/4, in which the values for N and
    % MaxOccupancy are configured with defaults such as in init_default/1
    %
:- func from_assoc_list(hash_pred(K)::in(hash_pred), assoc_list(K, V)::in) =
    (version_hash_table(K, V)::out) is det.

    % Fold a function over the key-value bindings in the given hash table.
    %
:- func fold(func(K, V, A) = A, version_hash_table(K, V), A) = A.

    % Fold a predicate over the key-value bindings in the given hash table.
    %
:- pred fold(pred(K, V, A, A), version_hash_table(K, V), A, A).
:- mode fold(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode fold(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode fold(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode fold(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.

%---------------------------------------------------------------------------%

    % Test if two version_hash_tables are equal.
    % Unifications on the version_hash_table type are defined
    % by this predicate.
    %
:- pred equal(version_hash_table(K, V)::in, version_hash_table(K, V)::in)
    is semidet.
% This pragma is required because termination analysis can't analyse
% the use of higher order code.
:- pragma terminates(pred(equal/2)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module unit.
:- import_module version_array.

%---------------------------------------------------------------------------%

:- type version_hash_table(K, V)
    --->    ht(
                ht_num_occupants        :: int,
                ht_max_occupants        :: int,
                ht_hash_pred            :: pred(K::in, int::out) is det,
                ht_buckets              :: buckets(K, V)
            )
    where equality is version_hash_table.equal.

:- type buckets(K, V) == version_array(hash_table_alist(K, V)).

    % Assuming a decent hash function, there should be few collisions,
    % so each bucket will usually contain an empty list or a singleton.
    % Including a singleton constructor therefore reduces memory consumption.
    %
:- type hash_table_alist(K, V)
    --->    ht_nil
    ;       ht_single(K, V)
    ;       ht_cons(K, V, hash_table_alist(K, V)).

%---------------------------------------------------------------------------%

init(HashPred, N, MaxOccupancy) = HT :-
    do_init(HashPred, N, MaxOccupancy, yes, HT).

unsafe_init(HashPred, N, MaxOccupancy) = HT :-
    do_init(HashPred, N, MaxOccupancy, no, HT).

:- pred do_init(hash_pred(K)::in(hash_pred), int::in, float::in, bool::in,
    version_hash_table(K, V)::out) is det.

do_init(HashPred, N, MaxOccupancy, NeedSafety, HT) :-
    ( if N =< 0 then
        error("version_hash_table.init: N =< 0")
    else if N >= int.bits_per_int then
        error("version_hash_table.init: N >= int.bits_per_int")
    else if MaxOccupancy =< 0.0 then
        error("version_hash_table.init: MaxOccupancy =< 0.0")
    else
        NumBuckets = 1 << N,
        MaxOccupants = ceiling_to_int(float(NumBuckets) * MaxOccupancy),
        (
            NeedSafety = yes,
            Buckets = version_array.init(NumBuckets, ht_nil)
        ;
            NeedSafety = no,
            Buckets = version_array.unsafe_init(NumBuckets, ht_nil)
        ),
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

unsafe_init_default(HashPred) = unsafe_init(HashPred, 7, 0.875).

%---------------------------------------------------------------------------%

num_buckets(HT) = NumBuckets :-
    promise_equivalent_solutions [NumBuckets] (
        NumBuckets = size(HT ^ ht_buckets)
    ).

num_occupants(HT) = NumOccupants :-
    promise_equivalent_solutions [NumOccupants] (
        NumOccupants = HT ^ ht_num_occupants
    ).

%---------------------------------------------------------------------------%

copy(HT0) = HT :-
    promise_equivalent_solutions [HT] (
        HT0 = ht(NumOccupants, MaxOccupants, HashPred, Buckets0),
        Buckets = version_array.copy(Buckets0),
        HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
    ).

%---------------------------------------------------------------------------%

search(HT, K) = V :-
    search(HT, K, V).

search(HT, K, V) :-
    H = find_slot(HT, K),
    promise_equivalent_solutions [Buckets] (
        Buckets = HT ^ ht_buckets
    ),
    version_array.lookup(Buckets, H, AL),
    alist_search(AL, K, V).

:- pred alist_search(hash_table_alist(K, V)::in, K::in, V::out) is semidet.

alist_search(AL, K, V) :-
    require_complete_switch [AL]
    (
        AL = ht_nil,
        fail
    ;
        AL = ht_single(K, V)
    ;
        AL = ht_cons(HK, HV, T),
        ( if HK = K then
            HV = V
        else
            alist_search(T, K, V)
        )
    ).

%---------------------------------------------------------------------------%

lookup(HT, K) = V :-
    lookup(HT, K, V).

lookup(HT, K, V) :-
    ( if search(HT, K, V0) then
        V = V0
    else
        error($pred, "key not found")
    ).

elem(K, HT) = V :-
    lookup(HT, K, V).

%---------------------------------------------------------------------------%

set(HT0, K, V) = HT :-
    set(K, V, HT0, HT).

set(K, V, HT0, HT) :-
    H = find_slot(HT0, K),
    promise_equivalent_solutions
        [NumOccupants0, MaxOccupants, HashPred, Buckets0]
    (
        HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0)
    ),
    version_array.lookup(Buckets0, H, AL0),
    (
        AL0 = ht_nil,
        AL = ht_single(K, V),
        MayExpand = yes
    ;
        AL0 = ht_single(K0, _V0),
        ( if K0 = K then
            AL = ht_single(K0, V),
            MayExpand = no
        else
            AL = ht_cons(K, V, AL0),
            MayExpand = yes
        )
    ;
        AL0 = ht_cons(_, _, _),
        ( if alist_replace(AL0, K, V, AL1) then
            AL = AL1,
            MayExpand = no
        else
            AL = ht_cons(K, V, AL0),
            MayExpand = yes
        )
    ),
    version_array.set(H, AL, Buckets0, Buckets),
    (
        MayExpand = no,
        HT = ht(NumOccupants0, MaxOccupants, HashPred, Buckets)
    ;
        MayExpand = yes,
        NumOccupants = NumOccupants0 + 1,
        ( if NumOccupants > MaxOccupants then
            HT = expand(NumOccupants, MaxOccupants, HashPred, Buckets)
        else
            HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
        )
    ).

'elem :='(K, HT0, V) = HT :-
    set(K, V, HT0, HT).

:- pred alist_replace(hash_table_alist(K, V)::in, K::in, V::in,
    hash_table_alist(K, V)::out) is semidet.

alist_replace(AL0, K, V, AL) :-
    require_complete_switch [AL0]
    (
        AL0 = ht_nil,
        fail
    ;
        AL0 = ht_single(K, _),
        AL = ht_single(K, V)
    ;
        AL0 = ht_cons(K0, V0, T0),
        ( if K0 = K then
            AL = ht_cons(K0, V, T0)
        else
            alist_replace(T0, K, V, T),
            AL = ht_cons(K0, V0, T)
        )
    ).

%---------------------------------------------------------------------------%

det_insert(HT0, K, V) = HT :-
    H = find_slot(HT0, K),
    promise_equivalent_solutions
        [NumOccupants0, MaxOccupants, HashPred, Buckets0]
    (
        HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0)
    ),
    version_array.lookup(Buckets0, H, AL0),
    (
        AL0 = ht_nil,
        AL = ht_single(K, V)
    ;
        ( AL0 = ht_single(_, _)
        ; AL0 = ht_cons(_, _, _)
        ),
        ( if alist_search(AL0, K, _) then
            throw(software_error(
                "version_hash_table.det_insert: key already present"))
        else
            AL = ht_cons(K, V, AL0)
        )
    ),
    version_array.set(H, AL, Buckets0, Buckets),
    NumOccupants = NumOccupants0 + 1,
    ( if NumOccupants > MaxOccupants then
        HT = expand(NumOccupants, MaxOccupants, HashPred, Buckets)
    else
        HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
    ).

det_insert(K, V, HT, det_insert(HT, K, V)).

%---------------------------------------------------------------------------%

det_update(HT0, K, V) = HT :-
    det_update(K, V, HT0, HT).

det_update(K, V, HT0, HT) :-
    H = find_slot(HT0, K),
    promise_equivalent_solutions [Buckets0] (
        Buckets0 = HT0 ^ ht_buckets
    ),
    version_array.lookup(Buckets0, H, AL0),
    ( if alist_replace(AL0, K, V, AL1) then
        AL = AL1
    else
        error($pred, "key not found")
    ),
    version_array.set(H, AL, Buckets0, Buckets),
    promise_equivalent_solutions [HT] (
        HT = HT0 ^ ht_buckets := Buckets
    ).

%---------------------------------------------------------------------------%

delete(HT0, K) = HT :-
    delete(K, HT0, HT).

delete(K, HT0, HT) :-
    H = find_slot(HT0, K),
    promise_equivalent_solutions
        [NumOccupants0, MaxOccupants, HashPred, Buckets0]
    (
        HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0)
    ),
    version_array.lookup(Buckets0, H, AL0),
    ( if alist_remove(AL0, K, AL) then
        version_array.set(H, AL, Buckets0, Buckets),
        NumOccupants = NumOccupants0 - 1,
        HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
    else
        HT = HT0
    ).

:- pred alist_remove(hash_table_alist(K, V)::in, K::in,
    hash_table_alist(K, V)::out) is semidet.

alist_remove(AL0, K, AL) :-
    require_complete_switch [AL0]
    (
        AL0 = ht_nil,
        fail
    ;
        AL0 = ht_single(K, _),
        % The preceding list node remains ht_cons but that is acceptable.
        AL = ht_nil
    ;
        AL0 = ht_cons(K0, V0, T0),
        ( if K0 = K then
            AL = T0
        else
            alist_remove(T0, K, T),
            AL = ht_cons(K0, V0, T)
        )
    ).

%---------------------------------------------------------------------------%

to_assoc_list(HT) = AL :-
    promise_equivalent_solutions [Buckets] (
        Buckets = HT ^ ht_buckets
    ),
    version_array.foldl(to_assoc_list_2, Buckets, [], AL).

:- pred to_assoc_list_2(hash_table_alist(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

to_assoc_list_2(X, AL0, AL) :-
    (
        X = ht_nil,
        AL = AL0
    ;
        X = ht_single(K, V),
        AL = [K - V | AL0]
    ;
        X = ht_cons(K, V, T),
        AL1 = [K - V | AL0],
        to_assoc_list_2(T, AL1, AL)
    ).

from_assoc_list(HashPred, N, MaxOccupants, AList) = HT :-
    HT0 = init(HashPred, N, MaxOccupants),
    from_assoc_list_2(AList, HT0, HT).

from_assoc_list(HashPred, AList) = HT :-
    HT0 = init_default(HashPred),
    from_assoc_list_2(AList, HT0, HT).

:- pred from_assoc_list_2(assoc_list(K, V)::in,
    version_hash_table(K, V)::in, version_hash_table(K, V)::out) is det.

from_assoc_list_2([], !HT).
from_assoc_list_2([K - V | T], !HT) :-
    version_hash_table.set(K, V, !HT),
    from_assoc_list_2(T, !HT).

%---------------------------------------------------------------------------%

    % Hash tables expand by doubling the number of buckets.
    %
    % Ensuring expand/4 is _not_ inlined into version_hash_table.det_insert,
    % etc. actually makes those predicates more efficient.
    % expand calls version_array.init, which implicitly takes a type_info for
    % version_hash_table_alist(K, V) that has to be created dynamically.
    % version_array.init is not fully opt-exported so the unused type_info
    % argument is not eliminated, nor is the creation of the type_info
    % delayed until the (rare) call to expand.
    %
:- func expand(int::in, int::in, hash_pred(K)::in(hash_pred),
    buckets(K, V)::in) = (version_hash_table(K, V)::out) is det.
:- pragma no_inline(func(expand/4)).

expand(NumOccupants, MaxOccupants0, HashPred, Buckets0) = HT :-
    NumBuckets0 = size(Buckets0),
    NumBuckets = NumBuckets0 + NumBuckets0,
    MaxOccupants = MaxOccupants0 + MaxOccupants0,
    ( if version_array.has_lock(Buckets0) then
        Buckets1 = version_array.init(NumBuckets, ht_nil)
    else
        Buckets1 = version_array.unsafe_init(NumBuckets, ht_nil)
    ),
    reinsert_bindings(0, Buckets0, HashPred, NumBuckets, Buckets1, Buckets),
    HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets).

:- pred reinsert_bindings(int::in, buckets(K, V)::in,
    hash_pred(K)::in(hash_pred), int::in,
    buckets(K, V)::in, buckets(K, V)::out) is det.

reinsert_bindings(I, OldBuckets, HashPred, NumBuckets, !Buckets) :-
    ( if I >= size(OldBuckets) then
        true
    else
        version_array.lookup(OldBuckets, I, AL),
        reinsert_alist(AL, HashPred, NumBuckets, !Buckets),
        reinsert_bindings(I + 1, OldBuckets, HashPred, NumBuckets, !Buckets)
    ).

:- pred reinsert_alist(hash_table_alist(K, V)::in, hash_pred(K)::in(hash_pred),
    int::in, buckets(K, V)::in, buckets(K, V)::out) is det.

reinsert_alist(AL, HashPred, NumBuckets, !Buckets) :-
    (
        AL = ht_nil
    ;
        AL = ht_single(K, V),
        unsafe_insert(K, V, HashPred, NumBuckets, !Buckets)
    ;
        AL = ht_cons(K, V, T),
        unsafe_insert(K, V, HashPred, NumBuckets, !Buckets),
        reinsert_alist(T, HashPred, NumBuckets, !Buckets)
    ).

:- pred unsafe_insert(K::in, V::in, hash_pred(K)::in(hash_pred), int::in,
    buckets(K, V)::in, buckets(K, V)::out) is det.

unsafe_insert(K, V, HashPred, NumBuckets, Buckets0, Buckets) :-
    compute_slot_number(HashPred, NumBuckets, K, H),
    version_array.lookup(Buckets0, H, AL0),
    (
        AL0 = ht_nil,
        AL = ht_single(K, V)
    ;
        ( AL0 = ht_single(_, _)
        ; AL0 = ht_cons(_, _, _)
        ),
        AL = ht_cons(K, V, AL0)
    ),
    version_array.set(H, AL, Buckets0, Buckets).

%---------------------------------------------------------------------------%

fold(F, HT, X0) = X :-
    promise_equivalent_solutions [Buckets] (
        Buckets = HT ^ ht_buckets
    ),
    version_array.foldl(fold_f(F), Buckets, X0, X).

:- pred fold_f(func(K, V, A) = A, hash_table_alist(K, V), A, A).
:- mode fold_f(in(func(in, in, in) = out is det), in, in, out) is det.
:- mode fold_f(in(func(in, in, di) = uo is det), in, di, uo) is det.

fold_f(F, List, A0, A) :-
    (
        List = ht_nil,
        A = A0
    ;
        List = ht_single(K, V),
        A = F(K, V, A0)
    ;
        List = ht_cons(K, V, KVs),
        A1 = F(K, V, A0),
        fold_f(F, KVs, A1, A)
    ).

fold(P, HT, !A) :-
    promise_equivalent_solutions [Buckets] (
        Buckets = HT ^ ht_buckets
    ),
    version_array.foldl(fold_p(P), Buckets, !A).

:- pred fold_p(pred(K, V, A, A), hash_table_alist(K, V), A, A).
:- mode fold_p(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode fold_p(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold_p(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode fold_p(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode fold_p(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode fold_p(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.

fold_p(P, List, !A) :-
    (
        List = ht_nil
    ;
        List = ht_single(K, V),
        P(K, V, !A)
    ;
        List = ht_cons(K, V, KVs),
        P(K, V, !A),
        fold_p(P, KVs, !A)
    ).

%---------------------------------------------------------------------------%

:- func find_slot(version_hash_table(K, V), K) = int.
:- pragma inline(func(find_slot/2)).

find_slot(HT, K) = H :-
    promise_equivalent_solutions [HashPred] (
        HashPred = HT ^ ht_hash_pred
    ),
    compute_slot_number(HashPred, HT ^ num_buckets, K, H).

:- pred compute_slot_number(hash_pred(K)::in(hash_pred), int::in, K::in,
    int::out) is det.
:- pragma inline(pred(compute_slot_number/4)).

compute_slot_number(HashPred, NumBuckets, K, H) :-
    HashPred(K, Hash),
    % Since NumBuckets is a power of two, we can avoid mod.
    H = Hash /\ (NumBuckets - 1).

%---------------------------------------------------------------------------%

equal(HashTableA, HashTableB) :-
    ( if private_builtin.pointer_equal(HashTableA, HashTableB) then
        true
    else
        % This is an all-solutions context, because the unification
        % and the call to fold may fail. We therefore cannot deconstruct
        % HashTableA and HashTableB, whose type is non-canonical.
        % This is why we call num_occupants.
        NumA = num_occupants(HashTableA),
        NumB = num_occupants(HashTableB),
        NumA = NumB,
        % Test whether each item in HashTableA also occurs in HashTableB.
        % Since HashTableA and HashTableB have the same number of items,
        % if the fold succeeds, then we also know that there is no item
        % in HashTableB that does not also occur in HashTableA.
        version_hash_table.fold(compare_item(HashTableB), HashTableA, unit, _)
    ).

:- pred compare_item(version_hash_table(K, V)::in, K::in, V::in,
    unit::in, unit::out) is semidet.

compare_item(HashTableB, KeyA, ValueA, unit, unit) :-
    % Fail
    % - if the key from HashTableA does not occur in HashTableB, or
    % - if the key does occcur in HashTableB, but with a different value.
    version_hash_table.search(HashTableB, KeyA, ValueA).

%---------------------------------------------------------------------------%
:- end_module version_hash_table.
%---------------------------------------------------------------------------%
