% ---------------------------------------------------------------------------- %
% hash_table.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Tue Jan 30 15:21:45 GMT 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% THIS MODULE IS HEREBY DONATED TO THE MERCURY PROJECT TO BE
% RELEASED UNDER WHATEVER LICENCES ARE DEEMED APPROPRIATE BY
% THE MERCURY PROJECT TEAM.
%
% Hash table implementation.
%
% This implementation uses double hashing and requires the user to
% supply a predicate that will compute two independent hash values
% for any given key.
%
% Default double-hash functions are provided for ints, strings and
% generic values.
%
% The number of buckets in the hash table is always a power of 2.
%
% When a user set occupancy level is achieved, the number of buckets
% in the table is doubled and the previous contents reinserted into
% the new hash table.
%
% CAVEAT: the user is referred to the warning at the head of array.m
% with regard to the current use of unique objects.  Briefly, the
% problem is that the compiler does not yet properly understand
% unique modes, hence we fake it using non-unique modes.
% This means that care must be taken not to use an old version of a
% destructively updated structure (such as a hash_table) since the
% compiler will not currently detect such errors.
%
% ---------------------------------------------------------------------------- %

:- module hash_table.

:- interface.

:- import_module int, assoc_list, float, string, array, bitmap, char.



:- type hash_table(K, V).

    % XXX This is all fake until the compiler can handle nested unique modes.
    %
:- inst hash_table ==
            bound(ht(ground, ground, ground, hash_pred, bitmap, array, array)).
:- mode hash_table_ui == in(hash_table).
:- mode hash_table_di == di(hash_table).
:- mode hash_table_uo == out(hash_table).

:- type hash_pred(K) == ( pred(K, int, int) ).
:- inst hash_pred    == ( pred(in, out, out) is det ).

    % new(HashPred, N, MaxOccupancy)
    % constructs a new hash table with initial size 2 ^ N that is
    % doubled whenever MaxOccupancy is achieved; elements are
    % indexed using HashPred.
    %
    % HashPred must compute two *independent* hashes for a given
    % key - that is, one hash should not be a function of the other.
    % Otherwise poor performance will result.
    %
    % N must be greater than 1.
    % MaxOccupancy must be in (0.0, 1.0).
    %
    % XXX Values too close to the limits may cause bad things
    % to happen.
    %
:- func new(hash_pred(K), int, float) = hash_table(K, V).
:- mode new(in(hash_pred), in, in) = hash_table_uo is det.

    % new_default(HashFn) constructs a hash table with default size and
    % occupancy arguments.
    %
:- func new_default(hash_pred(K)) = hash_table(K, V).
:- mode new_default(in(hash_pred)) = hash_table_uo is det.

    % Retrieve the hash_pred associated with a hash table.
    %
:- func hash_pred(hash_table(K, V)) = hash_pred(K).
:- mode hash_pred(hash_table_ui) = out(hash_pred) is det.

    % Default hash_preds for ints and strings and everything (buwahahaha!)
    %
:- pred int_double_hash(int, int, int).
:- mode int_double_hash(in, out, out) is det.

:- pred string_double_hash(string, int, int).
:- mode string_double_hash(in, out, out) is det.

:- pred char_double_hash(char, int, int).
:- mode char_double_hash(in, out, out) is det.

:- pred float_double_hash(float, int, int).
:- mode float_double_hash(in, out, out) is det.

:- pred generic_double_hash(T, int, int).
:- mode generic_double_hash(in, out, out) is det.

    % Returns the number of buckets in a hash table.
    %
:- func num_buckets(hash_table(K, V)) = int.
:- mode num_buckets(hash_table_ui) = out is det.
%:- mode num_buckets(in) = out is det.

    % Returns the number of occupants in a hash table.
    %
:- func num_occupants(hash_table(K, V)) = int.
:- mode num_occupants(hash_table_ui) = out is det.
%:- mode num_occupants(in) = out is det.

    % Insert key-value binding into a hash table; if one is
    % already there then the previous value is overwritten.
    %
:- func set(hash_table(K, V), K, V) = hash_table(K, V).
:- mode set(hash_table_di, in, in) = hash_table_uo is det.

    % Field update for hash tables.
    % HT ^ elem(K) := V  is equivalent to  set(HT, K, V).
    %
:- func 'elem :='(K, hash_table(K, V), V) = hash_table(K, V).
:- mode 'elem :='(in, hash_table_di, in) = hash_table_uo is det.

    % Insert a key-value binding into a hash table.  An
    % exception is thrown if a binding for the key is already
    % present.
    %
:- func det_insert(hash_table(K, V), K, V) = hash_table(K, V).
:- mode det_insert(hash_table_di, in, in) = hash_table_uo is det.

    % Change a key-value binding in a hash table.  An
    % exception is thrown if a binding for the key does not
    % already exist.
    %
:- func det_update(hash_table(K, V), K, V) = hash_table(K, V).
:- mode det_update(hash_table_di, in, in) = hash_table_uo is det.

    % Delete the entry for the given key, leaving the hash table
    % unchanged if there is no such entry.
    %
:- func delete(hash_table(K, V), K) = hash_table(K, V).
:- mode delete(hash_table_di, in) = hash_table_uo is det.

    % Lookup the value associated with the given key.  An exception
    % is raised if there is no entry for the key.
    %
:- func lookup(hash_table(K, V), K) = V.
:- mode lookup(hash_table_ui, in) = out is det.
%:- mode lookup(in, in) = out is det.

    % Field access for hash tables.
    % HT ^ elem(K)  is equivalent to  lookup(HT, K).
    %
:- func elem(K, hash_table(K, V)) = V.
:- mode elem(in, hash_table_ui) = out is det.
%:- mode elem(in, in) = out is det.

    % Like lookup, but just fails if there is no entry for the key.
    %
:- pred search(hash_table(K, V), K, V).
:- mode search(hash_table_ui, in, out) is semidet.
%:- mode search(in, in, out) is semidet.

    % Convert a hash table into an association list.
    %
:- func to_assoc_list(hash_table(K, V)) = assoc_list(K, V).
:- mode to_assoc_list(hash_table_ui) = out is det.
%:- mode to_assoc_list(in) = out is det.

    % Fold a function over the key-value bindings in a hash table.
    %
:- func fold(func(K, V, T) = T, hash_table(K, V), T) = T.
:- mode fold(func(in, in, in) = out is det, hash_table_ui, in) = out is det.
:- mode fold(func(in, in, di) = uo is det, hash_table_ui, di) = uo is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module math, bool, exception, list, require, std_util.



:- type hash_table(K, V) 
    --->    ht(
                num_buckets             :: int,
                num_occupants           :: int,
                max_occupants           :: int,
                hash_pred               :: hash_pred(K),
                bitmap                  :: bitmap,
                keys                    :: array(K),
                values                  :: array(V)
            ).

% ---------------------------------------------------------------------------- %

    % THE HASHING SCHEME
    %
    % The user provided hashing function computes two independent
    % hashes H1 and H2 for a given key K.
    %
    % We calculate D = 2*H2 + 1 (to ensure that D is non-zero and
    % odd and is therefore coprime to the number of buckets) and
    % probe the table where the Kth probe examines slot
    %
    %   (H1 + K * H2) `rem` num_buckets
    %
    % The search is guaranteed to terminate because table occupancy
    % must be less than 1.0.
    %
    % The bitmap keeps track of which slots are occupied.
    %
    % If a slot is occupied, then the corresponding element in
    % the keys array is used to decide whether this probe has
    % found the right slot.
    %
    % Once the right slot has been found, the value can be found
    % in the corresponding element in the values array.
    %
    % The keys and values arrays cannot be initialised until at
    % least one binding is inserted.  This is handled by checking
    % to see whether the keys array has non-zero size.

% ---------------------------------------------------------------------------- %

new(HashPred, N, MaxOccupancy) = HT :-
    (      if N =< 1 then
            throw(software_error("hash_table__new_hash_table: N =< 1"))
      else if N >= int__bits_per_int then
            throw(software_error(
                "hash_table__new_hash_table: N >= int__bits_per_int"))
      else if MaxOccupancy =< 0.0 ; 1.0 =< MaxOccupancy  then
            throw(software_error(
                "hash_table__new_hash_table: MaxOccupancy not in (0.0, 1.0)"))
      else
            NumBuckets   = 1 << N,
            MaxOccupants = ceiling_to_int(float(NumBuckets) * MaxOccupancy),
            Bitmap       = bitmap__new(NumBuckets, no),
            Keys         = array__make_empty_array,
            Values       = array__make_empty_array,
            HT = ht(NumBuckets, 0, MaxOccupants, HashPred, Bitmap, Keys, Values)
    ).

% ---------------------------------------------------------------------------- %

    % These numbers are picked out of thin air.
    %
new_default(HashPred) = new(HashPred, 7, 0.9).

% ---------------------------------------------------------------------------- %

    % find_slot(HT, K) looks up key K in hash table HT and
    % returns the index for the entry K in H.  This is either the
    % first free slot identified (K is not in the table, but here
    % is where it would go) or the slot for K (K is in the table
    % and this is its slot).
    %
    % Whether or not K is actually in the table can be decided
    % by checking to see whether its bit in the bitmap is set
    % or clear.
    %
:- func find_slot(hash_table(K, V), K) = int.
:- mode find_slot(hash_table_ui, in) = out is det.
%:- mode find_slot(in, in) = out is det.

find_slot(HT, K) = H :-
    (HT ^ hash_pred)(K, Hash1a, Hash2a),
    int__abs(Hash1a, Hash1),
    int__abs(Hash2a, Hash2),
    H0    = Hash1 `rem` HT ^ num_buckets,
    Delta = Hash2 + Hash2 + 1,          % Have to ensure it's odd and non-zero.
    H     = find_slot_2(HT, K, H0, Delta).



:- func find_slot_2(hash_table(K, V), K, int, int) = int.
:- mode find_slot_2(hash_table_ui, in, in, in) = out is det.
%:- mode find_slot_2(in, in, in, in) = out is det.

find_slot_2(HT, K, H0, Delta) = H :-
    ( if bitmap__is_clear(HT ^ bitmap, H0) then
        H  = H0
      else if HT ^ keys ^ elem(H0) = K then
        H  = H0
      else
        H1 = (H0 + Delta) `rem` HT ^ num_buckets,
        H  = find_slot_2(HT, K, H1, Delta)
    ).

% ---------------------------------------------------------------------------- %

set(HT0, K, V) = HT :-

        % If this is the first entry in the hash table, then we use it to
        % set up the hash table (the arrays are currently empty because we
        % need values to initialise them with).
        %
    ( if array__size(HT0 ^ values) = 0 then
        HT = set(
                (( HT0
                        ^ keys   := array__init(HT0 ^ num_buckets, K) )
                        ^ values := array__init(HT0 ^ num_buckets, V) ),
                K, V
             )
      else
        H = find_slot(HT0, K),
        ( if bitmap__is_set(HT0 ^ bitmap, H) then
            HT = ( HT0 ^ values ^ elem(H) := V )
          else
            HT =
                ( if HT0 ^ num_occupants = HT0 ^ max_occupants then
                    set(expand(HT0), K, V)
                  else
                    (((( HT0
                            ^ num_occupants    := HT0 ^ num_occupants + 1 )
                            ^ bitmap           := bitmap__set(HT0 ^ bitmap, H) )
                            ^ keys ^ elem(H)   := K )
                            ^ values ^ elem(H) := V )
                )
        )
    ).

'elem :='(K, HT, V) = set(HT, K, V).

% ---------------------------------------------------------------------------- %

search(HT, K, V) :-
    H = find_slot(HT, K),
    bitmap__is_set(HT ^ bitmap, H),
    V = HT ^ values ^ elem(H).

% ---------------------------------------------------------------------------- %

det_insert(HT, K, V) =
    ( if bitmap__is_set(HT ^ bitmap, H) then
        throw(software_error("hash_table__det_insert: key already present"))
      else if HT ^ num_occupants = HT ^ max_occupants then
        set(expand(HT), K, V)
      else
        (((( HT
                ^ num_occupants    := HT ^ num_occupants + 1 )
                ^ bitmap           := bitmap__set(HT ^ bitmap, H) )
                ^ keys ^ elem(H)   := K )
                ^ values ^ elem(H) := V )
    )
 :-
    H = find_slot(HT, K).

% ---------------------------------------------------------------------------- %

det_update(HT, K, V) =
    ( if bitmap__is_clear(HT ^ bitmap, H) then
        throw(software_error("hash_table__det_update: key not found"))
      else
        HT ^ values ^ elem(H) := V
    )
 :-
    H = find_slot(HT, K).

% ---------------------------------------------------------------------------- %

lookup(HT, K) =
    ( if search(HT, K, V)
      then V
      else throw(software_error("hash_table__lookup: key not found"))
    ).

elem(K, HT) = lookup(HT, K).

% ---------------------------------------------------------------------------- %

delete(HT, K) =
    HT ^ bitmap := bitmap__clear(HT ^ bitmap, find_slot(HT, K)).

% ---------------------------------------------------------------------------- %

to_assoc_list(HT) = to_assoc_list_2(0, HT, []).



:- func to_assoc_list_2(int,hash_table(K,V),assoc_list(K,V)) = assoc_list(K,V).
:- mode to_assoc_list_2(in, hash_table_ui, in) = out is det.
%:- mode to_assoc_list_2(in, in, in) = out is det.

to_assoc_list_2(I, HT, AList) =
    ( if I >= HT ^ num_buckets then
        AList
      else if bitmap__is_clear(HT ^ bitmap, I) then
        to_assoc_list_2(I + 1, HT, AList)
      else
        to_assoc_list_2(I + 1, HT,
                [(HT ^ keys ^ elem(I)) - (HT ^ values ^ elem(I)) | AList])
    ).

% ---------------------------------------------------------------------------- %

    % Hash tables expand by doubling in size.
    %
:- func expand(hash_table(K, V)) = hash_table(K, V).
:- mode expand(hash_table_di) = hash_table_uo is det.

expand(HT0) = HT :-

    HT0 = ht(NBs0, _NOs, MOs0, HP, BM0, Ks0, Vs0),

    NBs = NBs0 + NBs0,
    MOs = MOs0 + MOs0,
    BM  = bitmap__new(NBs, no),
    Ks  = array__make_empty_array,
    Vs  = array__make_empty_array,

    HT1 = ht(NBs, 0, MOs, HP, BM, Ks, Vs),

    HT  = reinsert_bindings(0, NBs0, BM0, Ks0, Vs0, HT1).

% ---------------------------------------------------------------------------- %

:- func reinsert_bindings(int, int, bitmap, array(K), array(V),
            hash_table(K, V)) = hash_table(K, V).
:- mode reinsert_bindings(in, in, bitmap_ui, array_ui, array_ui,
            hash_table_di) = hash_table_uo is det.

reinsert_bindings(I, NumBuckets, Bitmap, Keys, Values, HT) =
    ( if I >= NumBuckets then
        HT
      else if bitmap__is_clear(Bitmap, I) then
        reinsert_bindings(I + 1, NumBuckets, Bitmap, Keys, Values, HT)
      else
        reinsert_bindings(I + 1, NumBuckets, Bitmap, Keys, Values,
                set(HT, Keys ^ elem(I), Values ^ elem(I)))
    ).

% ---------------------------------------------------------------------------- %

    % There are almost certainly better ones out there...
    %
    % NOTE that H1 \= N since neither of H1 or H2 should be a function
    % of the other under machine integer arithmetic.
    %
int_double_hash(N, H1, H2) :-
    H1 = N * N,
    H2 = N `xor` (N + N).

% ---------------------------------------------------------------------------- %

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

% ---------------------------------------------------------------------------- %

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

% ---------------------------------------------------------------------------- %

:- func munge_factor_a = int.
munge_factor_a = 5.

:- func munge_factor_b = int.
munge_factor_b = 3.

:- pred double_munge(int, int, int, int, int, int).
:- mode double_munge(in, in, out, in, in, out) is det.

double_munge(X, Ha0, Ha, Y, Hb0, Hb) :-
    Ha = munge(munge_factor_a, Ha0, X),
    Hb = munge(munge_factor_b, Hb0, Y).

:- func munge(int, int, int) = int.

munge(N, X, Y) =
    (X `unchecked_left_shift` N) `xor`
    (X `unchecked_right_shift` (int__bits_per_int - N)) `xor`
    Y.

% ---------------------------------------------------------------------------- %

fold(Fn, HT, X) = fold_0(0, Fn, HT, X).



:- func fold_0(int, func(K, V, T) = T, hash_table(K,V), T) = T.
:- mode fold_0(in, func(in,in,in) = out is det, hash_table_ui, in) = out is det.
:- mode fold_0(in, func(in,in,di) = uo is det, hash_table_ui, di) = uo is det.
% :- mode fold_0(in, func(in,in,in) = out is det, in, in) = out is det.
% :- mode fold_0(in, func(in,in,di) = uo is det, in, di) = uo is det.

fold_0(I, Fn, HT, X) =
    ( if I >= HT ^ num_buckets then
        X
      else if bitmap__is_clear(HT ^ bitmap, I) then
        fold_0(I + 1, Fn, HT, X)
      else
        fold_0(I + 1, Fn, HT, Fn(HT ^ keys ^ elem(I), HT ^ values ^ elem(I), X))
    ).

% ---------------------------------------------------------------------------- %

    % XXX To go into array.m
    %
    % dynamic_cast/2 won't work for arbitrary arrays since array/1 is
    % not a ground type (that is, dynamic_cast/2 will work when the
    % target type is e.g. array(int), but not when it is array(T)).
    %
:- some [T2] pred dynamic_cast_to_array(T1, array(T2)).
:-           mode dynamic_cast_to_array(in, out) is semidet.

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

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
