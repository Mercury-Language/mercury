%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% File: version_hash_table.m
% Main author: rafe
% Stability: low
%
% (See the header comments in version_types.m for an explanation of version
% types.)
%
% Version hash tables.  The "latest" version of the hash table
% provides roughly the same performance as the unique hash table
% implementation.  "Older" versions of the hash table are still
% accessible, but will incurr a growing performance penalty as
% more updates are made to the hash table.
%
%-----------------------------------------------------------------------------%

:- module version_hash_table.

:- interface.

:- import_module assoc_list.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module string.

:- type version_hash_table(K, V).

:- type hash_pred(K) == ( pred(K,  int, int)        ).
:- inst hash_pred    == ( pred(in, out, out) is det ).

    % new(HashFunc, N, MaxOccupancy)
    % constructs a new hash table with initial size 2 ^ N that is
    % doubled whenever MaxOccupancy is achieved; elements are
    % indexed using HashFunc.
    %
    % HashFunc must compute two *independent* hashes for a given
    % key - that is, one hash should not be a function of the other.
    % Otherwise poor performance will result.
    %
    % N must be greater than 1.
    % MaxOccupancy must be in (0.0, 1.0).
    %
    % XXX Values too close to the limits may cause bad things
    % to happen.
    %
:- func new(hash_pred(K)::in(hash_pred), int::in, float::in) =
            (version_hash_table(K, V)::out) is det.

    % new_default(HashFn) constructs a hash table with default size and
    % occupancy arguments.
    %
:- func new_default(hash_pred(K)::in(hash_pred)) =
            (version_hash_table(K, V)::out) is det.

    % Retrieve the hash_pred associated with a hash table.
    %
% :- func hash_pred(version_hash_table(K, V)) = hash_pred(K).

    % Default hash_preds for ints and strings and everything (buwahahaha!)
    %
:- pred int_double_hash     `with_type` hash_pred(int)    `with_inst` hash_pred.
:- pred string_double_hash  `with_type` hash_pred(string) `with_inst` hash_pred.
:- pred char_double_hash    `with_type` hash_pred(char)   `with_inst` hash_pred.
:- pred float_double_hash   `with_type` hash_pred(float)  `with_inst` hash_pred.
:- pred generic_double_hash `with_type` hash_pred(T)      `with_inst` hash_pred.

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

    % Fold a function over the key-value bindings in a hash table.
    %
:- func fold(func(K, V, T) = T, version_hash_table(K, V), T) = T.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module array.
:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module math.
:- import_module require.
:- import_module std_util.
:- import_module version_array.

:- type version_hash_table(K, V) 
    --->    ht(
                num_buckets             :: int,
                num_occupants           :: int,
                max_occupants           :: int,
                hash_func               :: hash_func(K),
                buckets                 :: buckets(K, V)
            ).

:- type hash_func(K) == (func(K) = {int, int}).

:- type buckets(K, V) == version_array(bucket(K, V)).

:- type bucket(K, V)
    --->    empty
    ;       full(K, V).

%-----------------------------------------------------------------------------%

    % THE HASHING SCHEME
    %
    % The user provided hashing function computes two independent
    % hashes H1 and H2 for a given key K.
    %
    % We calculate D = 2*H2 + 1 (to ensure that D is non-zero and
    % odd and is therefore coprime to the number of buckets) and
    % probe the table where the Kth probe examines slot
    %
    %   (H1 + K * H2) `mod` num_buckets
    %
    % The search is guaranteed to terminate because table occupancy
    % must be less than 1.0.

%-----------------------------------------------------------------------------%

new(HashPred, N, MaxOccupancy) = HT :-
    (      if N =< 1 then
            throw(software_error("version_hash_table__new: N =< 1"))
      else if N >= int__bits_per_int then
            throw(software_error(
                "version_hash_table__new: N >= int__bits_per_int"))
      else if MaxOccupancy =< 0.0 ; 1.0 =< MaxOccupancy  then
            throw(software_error(
                "version_hash_table__new: MaxOccupancy not in (0.0, 1.0)"))
      else
            NumBuckets   = 1 << N,
            MaxOccupants = ceiling_to_int(float(NumBuckets) * MaxOccupancy),
            HashFunc     = (func(X) = {I, J} :- HashPred(X, I, J)),
            VArray       = version_array.init(NumBuckets, empty),
            HT = ht(NumBuckets, 0, MaxOccupants, HashFunc, VArray)
    ).

%-----------------------------------------------------------------------------%

    % These numbers are picked out of thin air.
    %
new_default(HashPred) = new(HashPred, 7, 0.9).

%-----------------------------------------------------------------------------%

    % find_slot(HT, K) looks up key K in hash table HT and
    % returns the index for the entry K in H.  This is either the
    % first free slot identified (K is not in the table, but here
    % is where it would go) or the slot for K (K is in the table
    % and this is its slot).
    %
    % Whether or not K is actually in the table can be decided
    % by checking to see whether its bit in the vbitmap is set
    % or clear.
    %
:- func find_slot(version_hash_table(K, V), K) = int.

find_slot(HT, K) = H :-
    {Hash1, Hash2} = (HT ^ hash_func)(K),
    H0    = Hash1 mod HT ^ num_buckets,
            % Have to ensure it's odd and non-zero.
    Delta = Hash2 + Hash2 + 1,
    H     = find_slot_2(HT, K, H0, Delta).

:- func find_slot_2(version_hash_table(K, V), K, int, int) = int.

find_slot_2(HT, K, H0, Delta) = H :-
    B = HT ^ buckets ^ elem(H0),
    (
        B = empty,
        H = H0
    ;
        B = full(Key, _Value),
        ( if K = Key then
            H  = H0
          else
            H1 = (H0 + Delta) mod HT ^ num_buckets,
            H  = find_slot_2(HT, K, H1, Delta)
        )
    ).

%-----------------------------------------------------------------------------%

set(HT0, K, V) = HT :-

        % If this is the first entry in the hash table, then we use it to set
        % up the hash table (the version_arrays are currently empty because we
        % need values to initialise them with).
        %
    H = find_slot(HT0, K),
    B = HT0 ^ buckets ^ elem(H),
    (
        B  = empty,
        HT =
            ( if HT0 ^ num_occupants = HT0 ^ max_occupants then
                set(expand(HT0), K, V)
              else
                (( HT0 ^ num_occupants     := HT0 ^ num_occupants + 1 )
                       ^ buckets ^ elem(H) := full(K, V)              )
            )
    ;
        B  = full(_, _),
        HT = ( HT0 ^ buckets ^ elem(H) := full(K, V) )
    ).

'elem :='(K, HT, V) = set(HT, K, V).

set(K, V, HT, set(HT, K, V)).

%-----------------------------------------------------------------------------%

search(HT, K, search(HT, K)).

search(HT, K) = V :-
    H = find_slot(HT, K),
    HT ^ buckets ^ elem(H) = full(K, V).

%-----------------------------------------------------------------------------%

det_insert(HT0, K, V) = HT :-
    H = find_slot(HT0, K),
    B = HT0 ^ buckets ^ elem(H),
    (
        B  = full(_, _),
        error("version_hash_table__det_update: key already present")
    ;
        B  = empty,
        HT =
            ( if HT0 ^ num_occupants = HT0 ^ max_occupants then
                set(expand(HT0), K, V)
              else
                (( HT0 ^ num_occupants     := HT0 ^ num_occupants + 1 )
                       ^ buckets ^ elem(H) := full(K, V)              )
            )
    ).

det_insert(K, V, HT, det_insert(HT, K, V)).

%-----------------------------------------------------------------------------%

det_update(HT0, K, V) = HT :-
    H = find_slot(HT0, K),
    B = HT0 ^ buckets ^ elem(H),
    (
        B = empty,
        error("version_hash_table__det_update: key not found")
    ;
        B = full(_, _),
        HT = ( HT0 ^ buckets ^ elem(H) := full(K, V) )
    ).

det_update(K, V, HT, det_update(HT, K, V)).

%-----------------------------------------------------------------------------%

lookup(HT, K) =
    ( if   V = search(HT, K)
      then V
      else func_error("version_hash_table__lookup: key not found")
    ).

HT ^ elem(K) = lookup(HT, K).

%-----------------------------------------------------------------------------%

delete(HT, K) =
    HT ^ buckets ^ elem(find_slot(HT, K)) := empty.

delete(K, HT, delete(HT, K)).

%-----------------------------------------------------------------------------%

to_assoc_list(HT) =
    fold_up(cons_k_v(HT ^ buckets), 0, HT ^ num_buckets - 1, []).

:- func cons_k_v(version_array(bucket(K, V)), int, assoc_list(K, V)) =
            assoc_list(K, V).

cons_k_v(Bs, I, KVs) =
    ( if   Bs ^ elem(I) = full(K, V)
      then [K - V | KVs]
      else KVs
    ).

%-----------------------------------------------------------------------------%

    % Hash tables expand by doubling in size.
    %
:- func expand(version_hash_table(K, V)) = version_hash_table(K, V).

expand(HT0) = HT :-

    HT0 = ht(NBs0, _NOs, MOs0, HF, Bs0),

    NBs = NBs0 + NBs0,
    MOs = MOs0 + MOs0,
    Bs1 = version_array.init(NBs, empty),

    HT1 = ht(NBs, 0, MOs, HF, Bs1),

    HT  = fold_up(reinsert_k_v(Bs0), 0, NBs0 - 1, HT1).

:- func reinsert_k_v(buckets(K, V), int, version_hash_table(K, V)) =
            version_hash_table(K, V).

reinsert_k_v(Bs, I, HT) =
    ( if   Bs ^ elem(I) = full(K, V)
      then HT ^ elem(K) := V
      else HT
    ).

%-----------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
    % NOTE that H1 \= N since neither of H1 or H2 should be a function
    % of the other under machine integer arithmetic.
    %
int_double_hash(N, H1, H2) :-
    H1 = N * N,
    H2 = N `xor` (N + N).

%-----------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
string_double_hash(S, H1, H2) :-
    H1 = string__hash(S),
    H2 = string__foldl(func(C, N) = char__to_int(C) + N, S, 0).

%------------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
float_double_hash(F, H1, H2) :-
    H1 = float__hash(F),
    H2 = float__hash(F * F).

%------------------------------------------------------------------------------%

    % There are almost certainly better ones out there...
    %
char_double_hash(C, H1, H2) :-
    int_double_hash(char__to_int(C), H1, H2).

%-----------------------------------------------------------------------------%

    % This, again, is straight off the top of my head.
    %
generic_double_hash(T, Ha, Hb) :-
    ( if      dynamic_cast(T, Int) then

        int_double_hash(Int, Ha, Hb)

      else if dynamic_cast(T, String) then

        string_double_hash(String, Ha, Hb)

      else if dynamic_cast(T, Float) then

        float_double_hash(Float, Ha, Hb)

      else if dynamic_cast(T, Char) then

        char_double_hash(Char, Ha, Hb)

      else if dynamic_cast(T, Univ) then

        generic_double_hash(univ_value(Univ), Ha, Hb)

      else if dynamic_cast_to_array(T, Array) then

        {Ha, Hb} =
            array__foldl(
                ( func(X, {HA0, HB0}) = {HA, HB} :-
                    generic_double_hash(X, HXA, HXB),
                    double_munge(HXA, HA0, HA, HXB, HB0, HB)
                ),
                Array,
                {0, 0}
            )

      else

        deconstruct(T, FunctorName, Arity, Args),
        string_double_hash(FunctorName, Ha0, Hb0),
        double_munge(Arity, Ha0, Ha1, Arity, Hb0, Hb1),
        list__foldl2(
            ( pred(U::in, HA0::in, HA::out, HB0::in, HB::out) is det :-
                generic_double_hash(U, HUA, HUB),
                double_munge(HUA, HA0, HA, HUB, HB0, HB)
            ),
            Args,
            Ha1, Ha,
            Hb1, Hb
        )
    ).

%-----------------------------------------------------------------------------%

:- func munge_factor_a = int.
munge_factor_a = 5.

:- func munge_factor_b = int.
munge_factor_b = 3.

:- pred double_munge(int::in, int::in, int::out, int::in, int::in, int::out)
            is det.

double_munge(X, Ha0, Ha, Y, Hb0, Hb) :-
    Ha = munge(munge_factor_a, Ha0, X),
    Hb = munge(munge_factor_b, Hb0, Y).

:- func munge(int, int, int) = int.

munge(N, X, Y) =
    (X `unchecked_left_shift` N) `xor`
    (X `unchecked_right_shift` (int__bits_per_int - N)) `xor`
    Y.

%-----------------------------------------------------------------------------%

fold(Fn, HT, X) = fold_up(apply_k_v(Fn, HT ^ buckets), 0, HT ^ num_buckets, X).

:- func apply_k_v(func(K, V, T) = T, buckets(K, V), int, T) = T.

apply_k_v(Fn, Bs, I, A) =
    ( if   Bs ^ elem(I) = full(K, V)
      then Fn(K, V, A)
      else A
    ).

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
%-----------------------------------------------------------------------------%
