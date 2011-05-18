%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: version_hash_table.m.
% Main author: rafe, wangp.
% Stability: low.
% 
% (See the header comments in version_array.m for an explanation of version
% types.)
%
% Version hash tables.  The "latest" version of the hash table provides roughly
% the same performance as the unique hash table implementation.  "Older"
% versions of the hash table are still accessible, but will incurr a growing
% performance penalty as more updates are made to the hash table.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module version_hash_table.
:- interface.

:- import_module assoc_list.
:- import_module char.

%-----------------------------------------------------------------------------%

:- type version_hash_table(K, V).

:- type hash_pred(K) == ( pred(K,  int)        ).
:- inst hash_pred    == ( pred(in, out) is det ).

    % init(HashPred, N, MaxOccupancy)
    % constructs a new hash table with initial size 2 ^ N that is
    % doubled whenever MaxOccupancy is achieved; elements are
    % indexed using HashPred.
    %
    % HashPred must compute a hash for a given key.
    % N must be greater than 0.
    % MaxOccupancy must be in (0.0, 1.0).
    %
    % XXX Values too close to the limits may cause bad things
    % to happen.
    %
:- func init(hash_pred(K)::in(hash_pred), int::in, float::in) = 
    (version_hash_table(K, V)::out) is det.

:- pragma obsolete(new/3).
:- func new(hash_pred(K)::in(hash_pred), int::in, float::in) =
    (version_hash_table(K, V)::out) is det.

    % unsafe_init(HashPred, N, MaxOccupancy)
    %
    % Like init/3, but the constructed hash table is backed by a non-thread safe
    % version array. It is unsafe to concurrently access or update the hash
    % table from different threads, or any two hash tables which were produced
    % from operations on the same original hash table.
    % However, if the hash table or its descendents will not be used in such a
    % manner, a non-thread safe hash table can be much faster than a thread
    % safe one.
    %
:- func unsafe_init(hash_pred(K)::in(hash_pred), int::in, float::in) =
   (version_hash_table(K, V)::out) is det.

:- pragma obsolete(unsafe_new/3).
:- func unsafe_new(hash_pred(K)::in(hash_pred), int::in, float::in) =
   (version_hash_table(K, V)::out) is det.

    % init_default(HashFn) constructs a hash table with default size and
    % occupancy arguments.
    %
:- func init_default(hash_pred(K)::in(hash_pred)) =
   (version_hash_table(K, V)::out) is det.

:- pragma obsolete(new_default/1).
:- func new_default(hash_pred(K)::in(hash_pred)) =
   (version_hash_table(K, V)::out) is det.

    % unsafe_init_default(HashFn)
    %
    % Like init_default/3 but the constructed hash table is backed by a
    % non-thread safe version array. See the description of unsafe_init/3 above.
    %
:- func unsafe_init_default(hash_pred(K)::in(hash_pred)) =
   (version_hash_table(K, V)::out) is det.

:- pragma obsolete(unsafe_new_default/1).
:- func unsafe_new_default(hash_pred(K)::in(hash_pred)) =
   (version_hash_table(K, V)::out) is det.

    % Retrieve the hash_pred associated with a hash table.
    %
% :- func hash_pred(version_hash_table(K, V)) = hash_pred(K).

    % Default hash_preds for ints and strings and everything (buwahahaha!)
    %
:- pred int_hash(int::in, int::out) is det.
:- pred string_hash(string::in, int::out) is det.
:- pred char_hash(char::in, int::out) is det.
:- pred float_hash(float::in, int::out) is det.
:- pred generic_hash(T::in, int::out) is det.

    % Returns the number of buckets in a hash table.
    %
:- func num_buckets(version_hash_table(K, V)) = int.

    % Returns the number of occupants in a hash table.
    %
:- func num_occupants(version_hash_table(K, V)) = int.

    % Insert key-value binding into a hash table; if one is
    % already there then the previous value is overwritten.
    % A predicate version is also provided.
    %
:- func set(version_hash_table(K, V), K, V) = version_hash_table(K, V).
:- pred set(K::in, V::in,
            version_hash_table(K, V)::in, version_hash_table(K, V)::out)
                is det.

    % Field update for hash tables.
    % HT ^ elem(K) := V  is equivalent to  set(HT, K, V).
    %
:- func 'elem :='(K, version_hash_table(K, V), V) = version_hash_table(K, V).

    % Insert a key-value binding into a hash table.  An
    % exception is thrown if a binding for the key is already
    % present.  A predicate version is also provided.
    %
:- func det_insert(version_hash_table(K, V), K, V) = version_hash_table(K, V).
:- pred det_insert(K::in, V::in,
            version_hash_table(K, V)::in, version_hash_table(K, V)::out)
                is det.

    % Change a key-value binding in a hash table.  An
    % exception is thrown if a binding for the key does not
    % already exist.  A predicate version is also provided.
    %
:- func det_update(version_hash_table(K, V), K, V) = version_hash_table(K, V).
:- pred det_update(K::in, V::in,
            version_hash_table(K, V)::in, version_hash_table(K, V)::out)
                is det.

    % Delete the entry for the given key, leaving the hash table
    % unchanged if there is no such entry.  A predicate version is also
    % provided.
    %
:- func delete(version_hash_table(K, V), K) = version_hash_table(K, V).
:- pred delete(K::in, version_hash_table(K, V)::in,
            version_hash_table(K, V)::out) is det.

    % Lookup the value associated with the given key.  An exception
    % is raised if there is no entry for the key.
    %
:- func lookup(version_hash_table(K, V), K) = V.

    % Field access for hash tables.
    % HT ^ elem(K)  is equivalent to  lookup(HT, K).
    %
:- func version_hash_table(K, V) ^ elem(K) = V.

    % Like lookup, but just fails if there is no entry for the key.
    %
:- func search(version_hash_table(K, V), K) = V is semidet.
:- pred search(version_hash_table(K, V)::in, K::in, V::out) is semidet.

    % Convert a hash table into an association list.
    %
:- func to_assoc_list(version_hash_table(K, V)) = assoc_list(K, V).

    % Convert an association list into a hash table.
    %
:- func from_assoc_list(hash_pred(K)::in(hash_pred), assoc_list(K, V)::in) =
    (version_hash_table(K, V)::out) is det.

    % Fold a function over the key-value bindings in a hash table.
    %
:- func fold(func(K, V, T) = T, version_hash_table(K, V), T) = T.

    % Fold a predicate over the key-value bindings in a hash table.
    %
:- pred fold(pred(K, V, T, T), version_hash_table(K, V), T, T).
:- mode fold(in(pred(in, in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold(in(pred(in, in, di, uo) is det), in, di, uo) is det.
:- mode fold(in(pred(in, in, in, out) is semidet), in, in, out) is semidet.
:- mode fold(in(pred(in, in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode fold(in(pred(in, in, di, uo) is semidet), in, di, uo) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module deconstruct.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module type_desc.
:- import_module univ.
:- import_module version_array.

%-----------------------------------------------------------------------------%

:- type version_hash_table(K, V)
    --->    ht(
                num_occupants           :: int,
                max_occupants           :: int,
                hash_pred               :: hash_pred(K),
                buckets                 :: buckets(K, V)
            ).

:- type buckets(K, V) == version_array(hash_table_alist(K, V)).

:- type hash_table_alist(K, V)
    --->    ht_nil
    ;       ht_cons(K, V, hash_table_alist(K, V)).

%-----------------------------------------------------------------------------%

init(HashPred, N, MaxOccupancy) = init_2(HashPred, N, MaxOccupancy, yes).

new(HashPred, N, MaxOccupancy) = init_2(HashPred, N, MaxOccupancy, yes).

unsafe_init(HashPred, N, MaxOccupancy) = init_2(HashPred, N, MaxOccupancy, no).

unsafe_new(HashPred, N, MaxOccupancy) = init_2(HashPred, N, MaxOccupancy, no).

:- func init_2(hash_pred(K)::in(hash_pred), int::in, float::in, bool::in) =
    (version_hash_table(K, V)::out) is det.

init_2(HashPred, N, MaxOccupancy, NeedSafety) = HT :-
    (      if N =< 0 then
            throw(software_error("version_hash_table.new_hash_table: N =< 0"))
      else if N >= int.bits_per_int then
            throw(software_error(
                "version_hash_table.new: N >= int.bits_per_int"))
      else if MaxOccupancy =< 0.0 then
            throw(software_error(
                "version_hash_table.new: MaxOccupancy =< 0.0"))
      else
            NumBuckets = 1 << N,
            MaxOccupants = ceiling_to_int(float(NumBuckets) * MaxOccupancy),
            (
                NeedSafety = yes,
                Buckets = version_array.init(NumBuckets, ht_nil)
            ;
                NeedSafety = no,
                Buckets = version_array.unsafe_new(NumBuckets, ht_nil)
            ),
            HT = ht(0, MaxOccupants, HashPred, Buckets)
    ).

%-----------------------------------------------------------------------------%

    % These numbers are picked out of thin air.
    %
init_default(HashPred) = init(HashPred, 7, 0.9).
new_default(HashPred) = init(HashPred, 7, 0.9).

unsafe_init_default(HashPred) = unsafe_init(HashPred, 7, 0.9).
unsafe_new_default(HashPred) = unsafe_init(HashPred, 7, 0.9).

%-----------------------------------------------------------------------------%

num_buckets(HT) = size(HT ^ buckets).

%-----------------------------------------------------------------------------%

:- func find_slot(version_hash_table(K, V), K) = int.

find_slot(HT, K) = H :-
    unsafe_hash_pred_cast(HT ^ hash_pred, HashPred),
    find_slot_2(HashPred, K, HT ^ num_buckets, H).

:- pred find_slot_2(hash_pred(K)::in(hash_pred), K::in, int::in, int::out)
    is det.

find_slot_2(HashPred, K, NumBuckets, H) :-
    HashPred(K, Hash),
    % Since NumBuckets is a power of two we can avoid mod.
    H = Hash /\ (NumBuckets - 1).

:- pred unsafe_hash_pred_cast(hash_pred(K)::in, hash_pred(K)::out(hash_pred))
    is det.

:- pragma foreign_proc("C",
    unsafe_hash_pred_cast(HashPred0::in, HashPred::out(hash_pred)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    HashPred = HashPred0;
").

:- pragma foreign_proc("C#",
    unsafe_hash_pred_cast(HashPred0::in, HashPred::out(hash_pred)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    HashPred = HashPred0;
").

:- pragma foreign_proc("Java",
    unsafe_hash_pred_cast(HashPred0::in, HashPred::out(hash_pred)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    HashPred = HashPred0;
").

%-----------------------------------------------------------------------------%

set(!.HT, K, V) = !:HT :-
    H = find_slot(!.HT, K),
    AL0 = !.HT ^ buckets ^ elem(H),
    ( if alist_replace(AL0, K, V, AL1) then
        AL = AL1,
        MayExpand = no
      else
        AL = ht_cons(K, V, AL0),
        MayExpand = yes
    ),
    !HT ^ buckets ^ elem(H) := AL,
    (
        MayExpand = no
    ;
        MayExpand = yes,
        increase_occupants(!HT)
    ).

'elem :='(K, HT, V) = set(HT, K, V).

set(K, V, HT, set(HT, K, V)).

:- pred alist_replace(hash_table_alist(K, V)::in, K::in, V::in,
    hash_table_alist(K, V)::out) is semidet.

alist_replace(ht_cons(HK, HV, T), K, V, AList) :-
    ( if HK = K then
        AList = ht_cons(K, V, T)
      else
        alist_replace(T, K, V, AList0),
        AList = ht_cons(HK, HV, AList0)
    ).

%-----------------------------------------------------------------------------%

search(HT, K, search(HT, K)).

search(HT, K) = V :-
    H = find_slot(HT, K),
    AL = HT ^ buckets ^ elem(H),
    alist_search(AL, K, V).

:- pred alist_search(hash_table_alist(K, V)::in, K::in, V::out) is semidet.

alist_search(ht_cons(HK, HV, T), K, V) :-
    ( if HK = K then
        HV = V
      else
        alist_search(T, K, V)
    ).

%-----------------------------------------------------------------------------%

det_insert(!.HT, K, V) = !:HT :-
    H = find_slot(!.HT, K),
    AL0 = !.HT ^ buckets ^ elem(H),
    ( if alist_search(AL0, K, _) then
        throw(software_error(
            "version_hash_table.det_insert: key already present"))
      else
        AL = ht_cons(K, V, AL0)
    ),
    !HT ^ buckets ^ elem(H) := AL,
    increase_occupants(!HT).

det_insert(K, V, HT, det_insert(HT, K, V)).

%-----------------------------------------------------------------------------%

det_update(HT0, K, V) = HT :-
    H = find_slot(HT0, K),
    AL0 = HT0 ^ buckets ^ elem(H),
    ( if alist_replace(AL0, K, V, AL1) then
        AL = AL1
      else
        throw(software_error("version_hash_table.det_update: key not found"))
    ),
    HT = HT0 ^ buckets ^ elem(H) := AL.

det_update(K, V, HT, det_update(HT, K, V)).

%-----------------------------------------------------------------------------%

lookup(HT, K) =
    ( if   V = search(HT, K)
      then V
      else func_error("version_hash_table.lookup: key not found")
    ).

elem(K, HT) = lookup(HT, K).

%-----------------------------------------------------------------------------%

delete(HT0, K) = HT :-
    H = find_slot(HT0, K),
    AL0 = HT0 ^ buckets ^ elem(H),
    ( if alist_remove(AL0, K, AL) then
        HT0 = ht(NumOccupants0, MaxOccupants, HashPred, Buckets0),
        Buckets = Buckets0 ^ elem(H) := AL,
        NumOccupants = NumOccupants0 - 1,
        HT = ht(NumOccupants, MaxOccupants, HashPred, Buckets)
      else
        HT = HT0
    ).

delete(K, HT, delete(HT, K)).

:- pred alist_remove(hash_table_alist(K, V)::in, K::in,
    hash_table_alist(K, V)::out) is semidet.

alist_remove(ht_cons(HK, HV, T), K, AList) :-
    ( if HK = K then
        AList = T
      else
        alist_remove(T, K, AList0),
        AList = ht_cons(HK, HV, AList0)
    ).

%-----------------------------------------------------------------------------%

to_assoc_list(HT) =
    foldl(to_assoc_list_2, HT ^ buckets, []).

:- func to_assoc_list_2(hash_table_alist(K, V), assoc_list(K, V))
    = assoc_list(K, V).

to_assoc_list_2(ht_nil, AList) = AList.
to_assoc_list_2(ht_cons(K, V, T), AList) =
    to_assoc_list_2(T, [K - V | AList]).


from_assoc_list(HP, AList) = from_assoc_list_2(AList, init_default(HP)).

:- func from_assoc_list_2(assoc_list(K, V), version_hash_table(K, V))
    = version_hash_table(K, V).

from_assoc_list_2([], HT) = HT.
from_assoc_list_2([K - V | AList], HT) =
    from_assoc_list_2(AList, HT ^ elem(K) := V).

%-----------------------------------------------------------------------------%

:- pred increase_occupants(version_hash_table(K, V), version_hash_table(K, V)).
:- mode increase_occupants(in, out) is det.

increase_occupants(!HT) :-
    NumOccupants = !.HT ^ num_occupants,
    MaxOccupants = !.HT ^ max_occupants,
    ( if NumOccupants = MaxOccupants then
        expand(!HT)
      else
        !HT ^ num_occupants := NumOccupants + 1
    ).

    % Hash tables expand by doubling in size.
    %
:- pred expand(version_hash_table(K, V), version_hash_table(K, V)).
:- mode expand(in, out) is det.

expand(HT0, HT) :-
    HT0 = ht(NumOccupants0, MaxOccupants0, HashPred0, Buckets0),

    NumBuckets0 = size(Buckets0),
    NumBuckets = NumBuckets0 + NumBuckets0,
    MaxOccupants = MaxOccupants0 + MaxOccupants0,

    unsafe_hash_pred_cast(HashPred0, HashPred),
    Buckets1 = init(NumBuckets, ht_nil),
    reinsert_bindings(0, Buckets0, HashPred, NumBuckets, Buckets1, Buckets),

    HT = ht(NumOccupants0 + 1, MaxOccupants, HashPred, Buckets).

:- pred reinsert_bindings(int::in, buckets(K, V)::in,
    hash_pred(K)::in(hash_pred), int::in,
    buckets(K, V)::in, buckets(K, V)::out) is det.

reinsert_bindings(I, OldBuckets, HashPred, NumBuckets, !Buckets) :-
    ( if I >= size(OldBuckets) then
        true
      else
        AL = OldBuckets ^ elem(I),
        reinsert_alist(AL, HashPred, NumBuckets, !Buckets),
        reinsert_bindings(I + 1, OldBuckets, HashPred, NumBuckets, !Buckets)
    ).

:- pred reinsert_alist(hash_table_alist(K, V)::in, hash_pred(K)::in(hash_pred),
    int::in, buckets(K, V)::in, buckets(K, V)::out) is det.

reinsert_alist(AL, HashPred, NumBuckets, !Buckets) :-
    (
        AL = ht_nil
    ;
        AL = ht_cons(K, V, T),
        unsafe_insert(K, V, HashPred, NumBuckets, !Buckets),
        reinsert_alist(T, HashPred, NumBuckets, !Buckets)
    ).

:- pred unsafe_insert(K::in, V::in, hash_pred(K)::in(hash_pred), int::in,
    buckets(K, V)::in, buckets(K, V)::out) is det.

unsafe_insert(K, V, HashPred, NumBuckets, Buckets0, Buckets) :-
    find_slot_2(HashPred, K, NumBuckets, H),
    AL0 = Buckets0 ^ elem(H),
    Buckets = Buckets0 ^ elem(H) := ht_cons(K, V, AL0).

%-----------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
int_hash(N, N).

    % From http://www.concentric.net/~Ttwang/tech/inthash.htm
    %   public int hash32shift(int key)
    %   public long hash64shift(long key)
    %
:- pragma foreign_proc("C",
    int_hash(N::in, H::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const int c2 = 0x27d4eb2d; /* a prime or an odd constant */
    MR_Unsigned key;

    key = N;

    if (sizeof(MR_Word) == 4) {
        key = (key ^ 61) ^ (key >> 16);
        key = key + (key << 3);
        key = key ^ (key >> 4);
        key = key * c2;
        key = key ^ (key >> 15);
    } else {
        key = (~key) + (key << 21); /* key = (key << 21) - key - 1; */
        key = key ^ (key >> 24);
        key = (key + (key << 3)) + (key << 8); /* key * 265 */
        key = key ^ (key >> 14);
        key = (key + (key << 2)) + (key << 4); /* key * 21 */
        key = key ^ (key >> 28);
        key = key + (key << 31);
    }

    H = key;
").

%-----------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
string_hash(S, string.hash(S)).

%-----------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
float_hash(F, float.hash(F)).

%-----------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
char_hash(C, H) :-
    int_hash(char.to_int(C), H).

%-----------------------------------------------------------------------------%

    % This, again, is straight off the top of my head.
    %
generic_hash(T, H) :-
    ( if      dynamic_cast(T, Int) then

        int_hash(Int, H)

      else if dynamic_cast(T, String) then

        string_hash(String, H)

      else if dynamic_cast(T, Float) then

        float_hash(Float, H)

      else if dynamic_cast(T, Char) then

        char_hash(Char, H)

      else if dynamic_cast(T, Univ) then

        generic_hash(univ_value(Univ), H)

      else if dynamic_cast_to_array(T, Array) then

        H = array.foldl(
                ( func(X, HA0) = HA :-
                    generic_hash(X, HX),
                    munge(HX, HA0) = HA
                ),
                Array,
                0
            )

      else

        deconstruct(T, canonicalize, FunctorName, Arity, Args),
        string_hash(FunctorName, H0),
        munge(Arity, H0) = H1,
        list.foldl(
            ( pred(U::in, HA0::in, HA::out) is det :-
                generic_hash(U, HUA),
                munge(HUA, HA0) = HA
            ),
            Args,
            H1, H
        )
    ).

%-----------------------------------------------------------------------------%

:- func munge(int, int) = int.

munge(N, X) =
    (X `unchecked_left_shift` N) `xor`
    (X `unchecked_right_shift` (int.bits_per_int - N)).

%-----------------------------------------------------------------------------%

fold(F, HT, X0) = X :-
    foldl(fold_f(F), HT ^ buckets, X0, X).

:- pred fold_f(func(K, V, T) = T, hash_table_alist(K, V), T, T).
:- mode fold_f(func(in, in, in) = out is det, in, in, out) is det.
:- mode fold_f(func(in, in, di) = uo is det, in, di, uo) is det.

fold_f(_F, ht_nil, !A).
fold_f(F, ht_cons(K, V, KVs), !A) :-
    F(K, V, !.A) = !:A,
    fold_f(F, KVs, !A).


fold(P, HT, !A) :-
    foldl(fold_p(P), HT ^ buckets, !A).

:- pred fold_p(pred(K, V, T, T), hash_table_alist(K, V), T, T).
:- mode fold_p(pred(in, in, in, out) is det, in, in, out) is det.
:- mode fold_p(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold_p(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode fold_p(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_p(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold_p(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.

fold_p(_P, ht_nil, !A).
fold_p(P, ht_cons(K, V, KVs), !A) :-
    P(K, V, !A),
    fold_p(P, KVs, !A).

%-----------------------------------------------------------------------------%

    % XXX To go into array.m
    %
    % dynamic_cast/2 won't work for arbitrary arrays since array/1 is
    % not a ground type (that is, dynamic_cast/2 will work when the
    % target type is e.g. array(int), but not when it is array(T)).
    %
:- some [T2] pred dynamic_cast_to_array(T1::in, array(T2)::out) is semidet.

dynamic_cast_to_array(X, A) :-

        % If X is an array then it has a type with one type argument.
        %
    [ArgTypeDesc] = type_args(type_of(X)),

        % Convert ArgTypeDesc to a type variable ArgType.
        %
    (_ `with_type` ArgType) `has_type` ArgTypeDesc,

        % Constrain the type of A to be array(ArgType) and do the
        % cast.
        %
    dynamic_cast(X, A `with_type` array(ArgType)).

%-----------------------------------------------------------------------------%
:- end_module version_hash_table.
%-----------------------------------------------------------------------------%
