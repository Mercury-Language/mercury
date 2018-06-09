%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: injection.m.
% Author: mark.
% Stability: low.
%
% This module provides the `injection' ADT.  An injection is like a `map'
% (see map.m) but it allows efficient reverse lookups, similarly to `bimap'.
% This time efficiency comes at the expense of using twice as much space
% or more.  The difference between an injection and a bimap is that there
% can be values in the range of the injection that are not returned for any
% key, but for which a reverse lookup will still return a valid key.
%
% The invariants on this data structure, which are enforced by this module,
% are as follows:
%
% 1) For any key K, if a forward lookup succeeds with value V then a reverse
% lookup of value V will succeed with key K.
%
% 2) For any value V, if a reverse lookup succeeds with key K then a forward
% lookup of key K will succeed with some value (not necessarily V).
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module injection.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type injection(K, V).

%---------------------------------------------------------------------------%

    % Initialize an empty injection.
    %
:- func init = injection(K, V).
:- pred init(injection(K, V)::out) is det.

    % Initialize an injection with the given key-value pair.
    %
:- func singleton(K, V) = injection(K, V).

    % Check whether an injection is empty.
    %
:- pred is_empty(injection(K, V)::in) is semidet.

    % Search the injection for the value corresponding to a given key.
    %
:- func forward_search(injection(K, V), K) = V is semidet.
:- pred forward_search(injection(K, V)::in, K::in, V::out)
    is semidet.

    % Search the injection for the key corresponding to a given value.
    %
:- func reverse_search(injection(K, V), V) = K is semidet.
:- pred reverse_search(injection(K, V)::in, K::out, V::in)
    is semidet.

    % Combined forward/reverse search.
    % (Declaratively equivalent to reverse_search.)
    %
:- pred search(injection(K, V), K, V).
:- mode search(in, in, out) is cc_nondet.
:- mode search(in, out, in) is semidet.

    % Look up the value for a given key, but throw an exception if it
    % is not present.
    %
:- func lookup(injection(K, V), K) = V.
:- pred lookup(injection(K, V)::in, K::in, V::out) is det.

    % Look up the key for a given value, but throw an exception if it
    % is not present.
    %
:- func reverse_lookup(injection(K, V), V) = K.
:- pred reverse_lookup(injection(K, V)::in, K::out, V::in) is det.

    % Return the list of all keys in the injection.
    %
:- func keys(injection(K, V)) = list(K).
:- pred keys(injection(K, V)::in, list(K)::out) is det.

    % Return the list of all values in the injection.
    %
:- func values(injection(K, V)) = list(V).
:- pred values(injection(K, V)::in, list(V)::out) is det.

    % Succeeds if the injection contains the given key.
    %
:- pred contains_key(injection(K, V)::in, K::in) is semidet.

    % Succeeds if the injection contains the given value.
    %
:- pred contains_value(injection(K, V)::in, V::in) is semidet.

    % Insert a new key-value pair into the injection.  Fails if either
    % the key or value already exists.
    %
:- func insert(injection(K, V), K, V) = injection(K, V) is semidet.
:- pred insert(injection(K, V)::in, K::in, V::in,
    injection(K, V)::out) is semidet.

    % As above but throws an exception if the key or the value already
    % exists.
    %
:- func det_insert(injection(K, V), K, V) = injection(K, V).
:- pred det_insert(injection(K, V)::in, K::in, V::in,
    injection(K, V)::out) is det.

    % Update the value associated with a given key.  Fails if the key
    % does not already exist, or if the value is already associated
    % with a key.
    %
:- func update(injection(K, V), K, V) = injection(K, V) is semidet.
:- pred update(injection(K, V)::in, K::in, V::in,
    injection(K, V)::out) is semidet.

    % As above, but throws an exception if the key does not already exist,
    % or if the value is already associated with a key.
    %
:- func det_update(injection(K, V), K, V) = injection(K, V).
:- pred det_update(injection(K, V)::in, K::in, V::in,
    injection(K, V)::out) is det.

    % Sets the value associated with a given key, regardless of whether
    % the key exists already or not.  Fails if the value is already
    % associated with a key that is different from the given key.
    %
:- func set(injection(K, V), K, V) = injection(K, V) is semidet.
:- pred set(injection(K, V)::in, K::in, V::in,
    injection(K, V)::out) is semidet.

    % As above, but throws an exception if the value is already associated
    % with a key that is different from the given key.
    %
:- func det_set(injection(K, V), K, V) = injection(K, V).
:- pred det_set(injection(K, V)::in, K::in, V::in,
    injection(K, V)::out) is det.

    % Insert key-value pairs from an assoc_list into the given injection.
    % Fails if any of the individual inserts would fail.
    %
:- func insert_from_assoc_list(assoc_list(K, V), injection(K, V)) =
    injection(K, V) is semidet.
:- pred insert_from_assoc_list(assoc_list(K, V)::in,
    injection(K, V)::in, injection(K, V)::out) is semidet.

    % As above, but throws an exception if any of the individual
    % inserts would fail.
    %
:- func det_insert_from_assoc_list(assoc_list(K, V),
    injection(K, V)) = injection(K, V).
:- pred det_insert_from_assoc_list(assoc_list(K, V)::in,
    injection(K, V)::in, injection(K, V)::out) is det.

    % Set key-value pairs from an assoc_list into the given injection.
    % Fails of any of the individual sets would fail.
    %
:- func set_from_assoc_list(assoc_list(K, V), injection(K, V)) =
    injection(K, V) is semidet.
:- pred set_from_assoc_list(assoc_list(K, V)::in,
    injection(K, V)::in, injection(K, V)::out) is semidet.

    % As above, but throws an exception if any of the individual sets
    % would fail.
    %
:- func det_set_from_assoc_list(assoc_list(K, V), injection(K, V)) =
    injection(K, V).
:- pred det_set_from_assoc_list(assoc_list(K, V)::in,
    injection(K, V)::in, injection(K, V)::out) is det.

    % Insert key-value pairs from corresponding lists into the given
    % injection.  Fails if any of the individual inserts would fail.
    % Throws an exception if the lists are not of equal length.
    %
:- func insert_from_corresponding_lists(list(K), list(V),
    injection(K, V)) = injection(K, V) is semidet.
:- pred insert_from_corresponding_lists(list(K)::in, list(V)::in,
    injection(K, V)::in, injection(K, V)::out) is semidet.

    % As above, but throws an exception if any of the individual
    % inserts would fail.
    %
:- func det_insert_from_corresponding_lists(list(K), list(V),
    injection(K, V)) = injection(K, V).
:- pred det_insert_from_corresponding_lists(list(K)::in, list(V)::in,
    injection(K, V)::in, injection(K, V)::out) is det.

    % Set key-value pairs from corresponding lists into the given
    % injection.  Fails of any of the individual sets would fail.
    % Throws an exception if the lists are not of equal length.
    %
:- func set_from_corresponding_lists(list(K), list(V),
    injection(K, V)) = injection(K, V) is semidet.
:- pred set_from_corresponding_lists(list(K)::in, list(V)::in,
    injection(K, V)::in, injection(K, V)::out) is semidet.

    % As above, but throws an exception if any of the individual sets
    % would fail.
    %
:- func det_set_from_corresponding_lists(list(K), list(V),
    injection(K, V)) = injection(K, V).
:- pred det_set_from_corresponding_lists(list(K)::in, list(V)::in,
    injection(K, V)::in, injection(K, V)::out) is det.

    % Delete a key from an injection.  Also deletes any values that
    % correspond to that key.  If the key is not present, leave the
    % injection unchanged.
    %
:- func delete_key(injection(K, V), K) = injection(K, V).
:- pred delete_key(K::in, injection(K, V)::in, injection(K, V)::out) is det.

    % Delete a value from an injection.  Throws an exception if there is
    % a key that maps to this value.  If the value is not present, leave
    % the injection unchanged.
    %
:- func delete_value(injection(K, V), V) = injection(K, V).
:- pred delete_value(V::in, injection(K, V)::in, injection(K, V)::out) is det.

    % Apply delete_key to a list of keys.
    %
:- func delete_keys(injection(K, V), list(K)) = injection(K, V).
:- pred delete_keys(list(K)::in,
    injection(K, V)::in, injection(K, V)::out) is det.

    % Apply delete_value to a list of values.
    %
:- func delete_values(injection(K, V), list(V)) = injection(K, V).
:- pred delete_values(list(V)::in,
    injection(K, V)::in, injection(K, V)::out) is det.

    % Merge the contents of the two injections.  Both sets of keys must
    % be disjoint, and both sets of values must be disjoint.
    %
:- func merge(injection(K, V), injection(K, V)) = injection(K, V).
:- pred merge(injection(K, V)::in, injection(K, V)::in, injection(K, V)::out)
    is det.

    % Merge the contents of the two injections.  For keys that occur in
    % both injections, map them to the value in the second argument.
    % Both sets of values must be disjoint.
    %
:- func overlay(injection(K, V), injection(K, V)) = injection(K, V).
:- pred overlay(injection(K, V)::in, injection(K, V)::in, injection(K, V)::out)
    is det.

    % Apply an injection to a list of keys.
    % Throws an exception if any of the keys are not present.
    %
:- func apply_forward_map_to_list(injection(K, V), list(K)) = list(V).
:- pred apply_forward_map_to_list(injection(K, V)::in, list(K)::in,
    list(V)::out) is det.

    % Apply the inverse of an injection to a list of values.
    % Throws an exception if any of the values are not present.
    %
:- func apply_reverse_map_to_list(injection(K, V), list(V)) = list(K).
:- pred apply_reverse_map_to_list(injection(K, V)::in, list(V)::in,
    list(K)::out) is det.

    % Apply a transformation to all the keys in the injection.  If two
    % distinct keys become equal under this transformation then the
    % value associated with the greater of these two keys is used in the
    % result.
    %
:- func map_keys(func(V, K) = L, injection(K, V)) = injection(L, V).
:- pred map_keys(pred(V, K, L)::in(pred(in, in, out) is det),
    injection(K, V)::in, injection(L, V)::out) is det.

    % Same as map_keys, but deletes any keys for which the
    % transformation fails.
    %
:- pred filter_map_keys(pred(V, K, L)::in(pred(in, in, out) is semidet),
    injection(K, V)::in, injection(L, V)::out) is det.

    % Apply a transformation to all the values in the injection.  If two
    % distinct values become equal under this transformation then the
    % reverse search of these two values in the original map must lead
    % to the same key.  If it doesn't, then throw an exception.
    %
:- func map_values(func(K, V) = W, injection(K, V)) = injection(K, W).
:- pred map_values(pred(K, V, W)::in(pred(in, in, out) is det),
    injection(K, V)::in, injection(K, W)::out) is det.

    % Extract the forward map from an injection.
    %
:- func forward_map(injection(K, V)) = map(K, V).
:- pred forward_map(injection(K, V)::in, map(K, V)::out) is det.

    % Extract the reverse map from an injection.
    %
:- func reverse_map(injection(K, V)) = map(V, K).
:- pred reverse_map(injection(K, V)::in, map(V, K)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module string.

:- type injection(K, V)
    --->    injection(map(K, V), map(V, K)).

%---------------------------------------------------------------------------%

init = injection(F, R) :-
    map.init(F),
    map.init(R).

init(injection.init).

singleton(K, V) = injection(F, R) :-
    F = map.singleton(K, V),
    R = map.singleton(V, K).

is_empty(injection(F, _)) :-
    map.is_empty(F).

forward_search(injection(F, _), K) = map.search(F, K).

forward_search(I, K, injection.forward_search(I, K)).

reverse_search(injection(_, R), V) = map.search(R, V).

reverse_search(I, injection.reverse_search(I, V), V).

:- pragma promise_equivalent_clauses(injection.search/3).

search(injection(F, _)::in, K::in, V::out) :-
    map.search(F, K, V0),
    cc_multi_equal(V0, V).
search(injection(_, R)::in, K::out, V::in) :-
    map.search(R, V, K).

lookup(injection(F, _), K) = map.lookup(F, K).

lookup(I, K, injection.lookup(I, K)).

reverse_lookup(injection(_, R), V) = map.lookup(R, V).

reverse_lookup(I, injection.reverse_lookup(I, V), V).

keys(injection(F, _)) = map.keys(F).

keys(I, injection.keys(I)).

values(injection(_, R)) = map.keys(R).

values(I, injection.values(I)).

contains_key(injection(F, _), K) :-
    map.contains(F, K).

contains_value(injection(_, R), V) :-
    map.contains(R, V).

insert(injection(!.F, !.R), K, V) = injection(!:F, !:R) :-
    map.insert(K, V, !F),
    map.insert(V, K, !R).

insert(I, K, V, injection.insert(I, K, V)).

det_insert(injection(!.F, !.R), K, V) = injection(!:F, !:R) :-
    map.det_insert(K, V, !F),
    map.det_insert(V, K, !R).

det_insert(I, K, V, injection.det_insert(I, K, V)).

update(injection(!.F, !.R), K, V) = injection(!:F, !:R) :-
    map.update(K, V, !F),
    map.insert(V, K, !R).

update(I, K, V, injection.update(I, K, V)).

det_update(injection(!.F, !.R), K, V) = injection(!:F, !:R) :-
    map.det_update(K, V, !F),
    map.det_insert(V, K, !R).

det_update(I, K, V, injection.det_update(I, K, V)).

set(injection(!.F, !.R), K, V) = injection(!:F, !:R) :-
    injection.set_2(K, V, !F, !R).

set(I, K, V, injection.set(I, K, V)).

:- pred injection.set_2(K::in, V::in, map(K, V)::in, map(K, V)::out,
    map(V, K)::in, map(V, K)::out) is semidet.

set_2(K, V, !F, !R) :-
    map.set(K, V, !F),
    ( if map.search(!.R, V, OrigK) then
        % Fail if the existing key is not the same as the given key.
        K = OrigK
    else
        map.det_insert(V, K, !R)
    ).

det_set(injection(!.F, !.R), K, V) = injection(!:F, !:R) :-
    injection.det_set_2(K, V, !F, !R).

det_set(I, K, V, injection.det_set(I, K, V)).

:- pred injection.det_set_2(K::in, V::in, map(K, V)::in, map(K, V)::out,
    map(V, K)::in, map(V, K)::out) is det.

det_set_2(K, V, !F, !R) :-
    map.set(K, V, !F),
    ( if map.search(!.R, V, OrigK) then
        % Abort if the existing key is not the same as the given key.
        ( if K = OrigK then
            true
        else
            error("injection.det_set: " ++
                "value is already associated with another key")
        )
    else
        map.det_insert(V, K, !R)
    ).

insert_from_assoc_list(A, injection(F0, R0)) = injection(F, R) :-
    P = ( pred(KV::in, !.F::in, !:F::out, !.R::in, !:R::out) is semidet :-
            KV = K - V,
            map.insert(K, V, !F),
            map.insert(V, K, !R)
        ),
    list.foldl2(P, A, F0, F, R0, R).

insert_from_assoc_list(A, I, injection.insert_from_assoc_list(A, I)).

det_insert_from_assoc_list(A, injection(F0, R0)) = injection(F, R) :-
    P = ( pred(KV::in, !.F::in, !:F::out, !.R::in, !:R::out) is det :-
            KV = K - V,
            map.det_insert(K, V, !F),
            map.det_insert(V, K, !R)
        ),
    list.foldl2(P, A, F0, F, R0, R).

det_insert_from_assoc_list(A, I,
    injection.det_insert_from_assoc_list(A, I)).

set_from_assoc_list(A, injection(F0, R0)) = injection(F, R) :-
    P = ( pred(KV::in, !.F::in, !:F::out, !.R::in, !:R::out) is semidet :-
            KV = K - V,
            injection.set_2(K, V, !F, !R)
        ),
    list.foldl2(P, A, F0, F, R0, R).

set_from_assoc_list(A, I, injection.set_from_assoc_list(A, I)).

det_set_from_assoc_list(A, injection(F0, R0)) = injection(F, R) :-
    P = ( pred(KV::in, !.F::in, !:F::out, !.R::in, !:R::out) is det :-
            KV = K - V,
            injection.det_set_2(K, V, !F, !R)
        ),
    list.foldl2(P, A, F0, F, R0, R).

det_set_from_assoc_list(A, I,
    injection.det_set_from_assoc_list(A, I)).

insert_from_corresponding_lists(As, Bs, injection(F0, R0)) =
        injection(F, R) :-
    P = ( pred(K::in, V::in, !.F::in, !:F::out, !.R::in, !:R::out)
                is semidet :-
            map.insert(K, V, !F),
            map.insert(V, K, !R)
        ),
    list.foldl2_corresponding(P, As, Bs, F0, F, R0, R).

insert_from_corresponding_lists(As, Bs, I,
    injection.insert_from_corresponding_lists(As, Bs, I)).

det_insert_from_corresponding_lists(As, Bs, injection(F0, R0)) =
        injection(F, R) :-
    P = ( pred(K::in, V::in, !.F::in, !:F::out, !.R::in, !:R::out) is det :-
            map.det_insert(K, V, !F),
            map.det_insert(V, K, !R)
        ),
    list.foldl2_corresponding(P, As, Bs, F0, F, R0, R).

det_insert_from_corresponding_lists(As, Bs, I,
    injection.det_insert_from_corresponding_lists(As, Bs, I)).

set_from_corresponding_lists(As, Bs, injection(!.F, !.R)) =
        injection(!:F, !:R) :-
    list.foldl2_corresponding(injection.set_2, As, Bs, !F, !R).

set_from_corresponding_lists(As, Bs, I,
    injection.set_from_corresponding_lists(As, Bs, I)).

det_set_from_corresponding_lists(As, Bs, injection(!.F, !.R)) =
        injection(!:F, !:R) :-
    list.foldl2_corresponding(injection.det_set_2, As, Bs, !F, !R).

det_set_from_corresponding_lists(As, Bs, I,
    injection.det_set_from_corresponding_lists(As, Bs, I)).

delete_key(injection(!.F, !.R), K) = injection(!:F, !:R) :-
    ( if map.remove(K, _, !F) then
        map.foldl(filter_values_with_key(K), !.R, map.init, !:R)
    else
        true
    ).

delete_key(K, I, injection.delete_key(I, K)).

:- pred filter_values_with_key(K::in, V::in, K::in, map(V, K)::in,
    map(V, K)::out) is det.

filter_values_with_key(FilterKey, V, K, !Map) :-
    ( if K = FilterKey then
        true
    else
        map.det_insert(V, K, !Map)
    ).

delete_value(injection(!.F, !.R), V) = injection(!:F, !:R) :-
    ( if map.remove(V, K, !R) then
        % Only K could possibly be associated with V.  If it is,
        % then we throw an exception.
        ( if map.lookup(!.F, K, V) then
            error("injection.delete_value: value is associated with a key")
        else
            true
        )
    else
        true
    ).

delete_value(V, I, injection.delete_value(I, V)).

delete_keys(I0, Ks) = I :-
    injection.delete_keys(Ks, I0, I).

delete_keys(Ks, !I) :-
    list.foldl(injection.delete_key, Ks, !I).

delete_values(I0, Vs) = I :-
    injection.delete_values(Vs, I0, I).

delete_values(Vs, !I) :-
    list.foldl(injection.delete_value, Vs, !I).

merge(injection(FA, RA), injection(FB, RB)) = injection(F, R) :-
    map.merge(FA, FB, F),
    map.merge(RA, RB, R).

merge(A, B, injection.merge(A, B)).

overlay(injection(FA, RA), injection(FB, RB)) = injection(F, R) :-
    map.overlay(FA, FB, F),
    map.merge(RA, RB, R).

overlay(A, B, injection.overlay(A, B)).

apply_forward_map_to_list(injection(F, _), Ks) =
    map.apply_to_list(Ks, F).

apply_forward_map_to_list(I, Ks,
    injection.apply_forward_map_to_list(I, Ks)).

apply_reverse_map_to_list(injection(_, R), Vs) =
    map.apply_to_list(Vs, R).

apply_reverse_map_to_list(I, Vs,
    injection.apply_reverse_map_to_list(I, Vs)).

map_keys(Func, injection(F0, R0)) = injection(F, R) :-
    F = map.foldl(insert_transformed_key_f(Func), F0, map.init),
    R = map.map_values(Func, R0).

:- func insert_transformed_key_f(func(V, K) = L, K, V, map(L, V)) = map(L, V).

insert_transformed_key_f(Func, K, V, !.Map) = !:Map :-
    map.set(Func(V, K), V, !Map).

map_keys(Pred, injection(!.F, !.R), injection(!:F, !:R)) :-
    map.foldl(insert_transformed_key_p(Pred), !.F, map.init, !:F),
    map.map_values(Pred, !R).

:- pred insert_transformed_key_p(pred(V, K, L)::in(pred(in, in, out) is det),
    K::in, V::in, map(L, V)::in, map(L, V)::out) is det.

insert_transformed_key_p(Pred, K, V, !Map) :-
    Pred(V, K, L),
    map.set(L, V, !Map).

filter_map_keys(Pred, injection(F0, R0), injection(F, R)) :-
    F = map.foldl(maybe_set_transformed_key(Pred), F0, map.init),
    map.to_assoc_list(R0, AL0),
    list.filter_map(maybe_transform_key(Pred), AL0, AL),
    map.from_assoc_list(AL, R).

:- func maybe_set_transformed_key(pred(V, K, L), K, V, map(L, V)) = map(L, V).
:- mode maybe_set_transformed_key(in(pred(in, in, out) is semidet), in, in, in)
    = out is det.

maybe_set_transformed_key(Pred, K, V, !.Map) = !:Map :-
    ( if Pred(V, K, L) then
        map.set(L, V, !Map)
    else
        true
    ).

:- pred maybe_transform_key(pred(V, K, L)::in(pred(in, in, out) is semidet),
    pair(V, K)::in, pair(V, L)::out) is semidet.

maybe_transform_key(Pred, V - K, V - L) :-
    Pred(V, K, L).

map_values(Func, injection(F0, R0)) = injection(F, R) :-
    F = map.map_values(Func, F0),
    R = map.foldl(insert_transformed_value_f(Func), R0, map.init).

:- func insert_transformed_value_f(func(K, V) = W, V, K, map(W, K)) =
    map(W, K).

insert_transformed_value_f(Func, V, K, !.Map) = !:Map :-
    W = Func(K, V),
    ( if map.insert(W, K, !Map) then
        true
    else
        % Another value in the original was already mapped to this value.
        % We ensure that it had the same key.
        ( if map.lookup(!.Map, W, K) then
            true
        else
            error("injection.map_values: " ++
                "merged two values with different keys")
        )
    ).

map_values(Pred, I0, I) :-
    Func = (func(K, V) = W :- Pred(K, V, W)),
    I = injection.map_values(Func, I0).

forward_map(injection(F, _)) = F.
forward_map(injection(F, _), F).

reverse_map(injection(_, R)) = R.
reverse_map(injection(_, R), R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
