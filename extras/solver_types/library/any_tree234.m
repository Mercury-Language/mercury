% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018, 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% any_tree234.m
% Ralph Becket <rafe@cs.mu.oz.au>
%
% A version of tree234.m for use with values with inst any.
% This is needed by any_map.m.
%
%---------------------------------------------------------------------------%

:- module any_tree234.

:- interface.

:- import_module any_assoc_list.
:- import_module list.

:- type any_tree234(K, V).

:- func init = (any_tree234(K, V)::oa) is det.
:- pred init(any_tree234(K, V)::oa) is det.

:- pred is_empty(any_tree234(K, V)::ia) is semidet.

:- pred member(any_tree234(K, V)::ia, K::out, V::oa) is nondet.

:- pred search(any_tree234(K, V)::ia, K::in, V::oa) is semidet.

:- func lookup(any_tree234(K, V)::ia, K::in) = (V::oa) is det.
:- pred lookup(any_tree234(K, V)::ia, K::in, V::oa) is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred lower_bound_search(any_tree234(K, V)::ia, K::in, K::out, V::oa)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Aborts if there is no key with the given or lower value.
    %
:- pred lower_bound_lookup(any_tree234(K, V)::ia, K::in, K::out, V::oa) is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred upper_bound_search(any_tree234(K, V)::ia, K::in, K::out, V::oa)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Aborts if there is no key with the given or higher value.
    %
:- pred upper_bound_lookup(any_tree234(K, V)::ia, K::in, K::out, V::oa) is det.

:- func max_key(any_tree234(K, V)::ia) = (K::out) is semidet.

:- func min_key(any_tree234(K, V)::ia) = (K::out) is semidet.

:- pred insert(K::in, V::ia,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa) is semidet.

:- func set(any_tree234(K, V)::ia, K::in, V::ia) = (any_tree234(K, V)::oa)
    is det.
:- pred set(K::in, V::ia, any_tree234(K, V)::ia, any_tree234(K, V)::oa) is det.

:- func delete(any_tree234(K, V)::ia, K::in) = (any_tree234(K, V)::oa) is det.
:- pred delete(K::in, any_tree234(K, V)::ia, any_tree234(K, V)::oa) is det.

:- pred remove(K::in, V::oa, any_tree234(K, V)::ia, any_tree234(K, V)::oa)
    is semidet.

:- pred remove_smallest(K::out, V::oa,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa) is semidet.

    % Given a any_tree234, return a list of all the keys ia the tree.
    % The list that is returned is ia sorted order.
    %
:- func keys(any_tree234(K, V)::ia) = (list(K)::out) is det.
:- pred keys(any_tree234(K, V)::ia, list(K)::out) is det.

:- func values(any_tree234(K, V)::ia) = (list(V)::oa) is det.
:- pred values(any_tree234(K, V)::ia, list(V)::oa) is det.

:- pred update(K::in, V::ia,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa) is semidet.

    % Update the value at the given key by applying the supplied
    % transformation to it.  This is faster than first searching for
    % the value and then updating it.
    %
:- pred transform_value(pred(V, V)::in(pred(ia, oa) is det),
    K::in, any_tree234(K, V)::ia, any_tree234(K, V)::oa) is semidet.

    % Count the number of elements ia a tree.
    %
:- func count(any_tree234(K, V)::ia) = (int::out) is det.
:- pred count(any_tree234(K, V)::ia, int::oa) is det.

:- func any_assoc_list_to_any_tree234(any_assoc_list(K, V)::ia)
    = (any_tree234(K, V)::oa) is det.
:- pred any_assoc_list_to_any_tree234(any_assoc_list(K, V)::ia,
    any_tree234(K, V)::oa) is det.

    % Given a any_tree234, return an association list of all the
    % keys and values ia the tree.  The association list that
    % is returned is sorted on the keys.
    %
:- func any_tree234_to_any_assoc_list(any_tree234(K, V)::ia)
    = (any_assoc_list(K, V)::oa) is det.
:- pred any_tree234_to_any_assoc_list(any_tree234(K, V)::ia,
    any_assoc_list(K, V)::oa) is det.

:- func foldl(func(K, V, T) = T, any_tree234(K, V), T) = T.
:- mode foldl(func(in, ia, in) = out is det, ia, in) = out is det.
:- mode foldl(func(in, ia, ia) = oa is det, ia, ia) = oa is det.

:- pred foldl(pred(K, V, T, T), any_tree234(K, V), T, T).
:- mode foldl(pred(in, ia, di, uo) is det, ia, di, uo) is det.
:- mode foldl(pred(in, ia, in, out) is det, ia, in, out) is det.
:- mode foldl(pred(in, ia, in, out) is semidet, ia, in, out) is semidet.
:- mode foldl(pred(in, ia, ia, oa) is det, ia, ia, oa) is det.
:- mode foldl(pred(in, ia, ia, oa) is semidet, ia, ia, oa) is semidet.

:- pred foldl2(pred(K, V, T, T, U, U),
    any_tree234(K, V), T, T, U, U).
:- mode foldl2(pred(in, ia, di, uo, di, uo) is det,
    ia, di, uo, di, uo) is det.
:- mode foldl2(pred(in, ia, in, out, di, uo) is det,
    ia, in, out, di, uo) is det.
:- mode foldl2(pred(in, ia, ia, oa, di, uo) is det,
    ia, ia, oa, di, uo) is det.
:- mode foldl2(pred(in, ia, in, out, in, out) is det,
    ia, in, out, in, out) is det.
:- mode foldl2(pred(in, ia, in, out, in, out) is semidet,
    ia, in, out, in, out) is semidet.
:- mode foldl2(pred(in, ia, ia, oa, in, out) is det,
    ia, ia, oa, in, out) is det.
:- mode foldl2(pred(in, ia, ia, oa, in, out) is semidet,
    ia, ia, oa, in, out) is semidet.
:- mode foldl2(pred(in, ia, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa) is det.
:- mode foldl2(pred(in, ia, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa) is semidet.

:- pred foldl3(pred(K, V, T, T, U, U, W, W), any_tree234(K, V),
    T, T, U, U, W, W).
:- mode foldl3(pred(in, ia, di, uo, di, uo, di, uo) is det,
    ia, di, uo, di, uo, di, uo) is det.
:- mode foldl3(pred(in, ia, in, out, di, uo, di, uo) is det,
    ia, in, out, di, uo, di, uo) is det.
:- mode foldl3(pred(in, ia, in, out, in, out, di, uo) is det,
    ia, in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, ia, in, out, in, out, in, out) is det,
    ia, in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, ia, in, out, in, out, in, out) is semidet,
    ia, in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, ia, ia, oa, in, out, in, out) is det,
    ia, ia, oa, in, out, in, out) is det.
:- mode foldl3(pred(in, ia, ia, oa, in, out, in, out) is semidet,
    ia, ia, oa, in, out, in, out) is semidet.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, in, out) is det,
    ia, ia, oa, ia, oa, in, out) is det.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, in, out) is semidet,
    ia, ia, oa, ia, oa, in, out) is semidet.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, ia, oa) is det,
    ia, ia, oa, ia, oa, ia, oa) is det.
:- mode foldl3(pred(in, ia, ia, oa, ia, oa, ia, oa) is semidet,
    ia, ia, oa, ia, oa, ia, oa) is semidet.

:- func map_values(func(K, V) = W, any_tree234(K, V))
    = any_tree234(K, W).
:- mode map_values(func(in, ia) = oa is det, ia)
    = oa is det.

:- pred map_values(pred(K, V, W), any_tree234(K, V),
    any_tree234(K, W)).
:- mode map_values(pred(in, ia, oa) is det, ia, oa)
    is det.
:- mode map_values(pred(in, ia, oa) is semidet, ia, oa)
    is semidet.

:- pred map_foldl(pred(K, V, W, A, A),
    any_tree234(K, V), any_tree234(K, W), A, A).
:- mode map_foldl(pred(in, ia, oa, di, uo) is det, ia, oa,
    di, uo) is det.
:- mode map_foldl(pred(in, ia, oa, in, out) is det, ia, oa,
    in, out) is det.
:- mode map_foldl(pred(in, ia, oa, in, out) is semidet, ia, oa,
    in, out) is semidet.
:- mode map_foldl(pred(in, ia, oa, ia, oa) is det, ia, oa,
    ia, oa) is det.
:- mode map_foldl(pred(in, ia, oa, ia, oa) is semidet, ia, oa,
    ia, oa) is semidet.

:- pred map_foldl2(pred(K, V, W, A, A, B, B),
    any_tree234(K, V), any_tree234(K, W), A, A, B, B).
:- mode map_foldl2(pred(in, ia, oa, in, out, di, uo) is det,
    ia, oa, in, out, di, uo) is det.
:- mode map_foldl2(pred(in, ia, oa, in, out, in, out) is det,
    ia, oa, in, out, in, out) is det.
:- mode map_foldl2(pred(in, ia, oa, in, out, in, out) is semidet,
    ia, oa, in, out, in, out) is semidet.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, in, out) is det,
    ia, oa, ia, oa, in, out) is det.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, in, out) is semidet,
    ia, oa, ia, oa, in, out) is semidet.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, ia, oa) is det,
    ia, oa, ia, oa, ia, oa) is det.
:- mode map_foldl2(pred(in, ia, oa, ia, oa, ia, oa) is semidet,
    ia, oa, ia, oa, ia, oa) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module any_util.
:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module require.

:- type any_tree234(K, V)
    --->    empty
    ;       two(K, V,
                any_tree234(K, V), any_tree234(K, V))
    ;       three(K, V, K, V,
                any_tree234(K, V), any_tree234(K, V), any_tree234(K, V))
    ;       four(K, V, K, V, K, V,
                any_tree234(K, V), any_tree234(K, V),
                any_tree234(K, V), any_tree234(K, V)).

%---------------------------------------------------------------------------%

init(empty).

is_empty(Tree) :-
    Tree = empty.

%---------------------------------------------------------------------------%

member(empty, _K, _V) :-
    fail.
member(two(K0, V0, T0, T1), K, V) :-
    (
        unsafe_cast_to_ground(K0),
        K = K0,
        V = V0
    ;
        member(T0, K, V)
    ;
        member(T1, K, V)
    ).
member(three(K0, V0, K1, V1, T0, T1, T2), K, V) :-
    (
        unsafe_cast_to_ground(K0),
        K = K0,
        V = V0
    ;
        unsafe_cast_to_ground(K1),
        K = K1,
        V = V1
    ;
        member(T0, K, V)
    ;
        member(T1, K, V)
    ;
        member(T2, K, V)
    ).
member(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V) :-
    (
        unsafe_cast_to_ground(K0),
        K = K0,
        V = V0
    ;
        unsafe_cast_to_ground(K1),
        K = K1,
        V = V1
    ;
        unsafe_cast_to_ground(K2),
        K = K2,
        V = V2
    ;
        member(T0, K, V)
    ;
        member(T1, K, V)
    ;
        member(T2, K, V)
    ;
        member(T3, K, V)
    ).

%---------------------------------------------------------------------------%

search(T, K, V) :-
    (
        T = empty,
        fail
    ;
        T = two(K0, V0, T0, T1),
        unsafe_cast_to_ground(K0),
        compare(Result, K, K0),
        (
            Result = (<),
            search(T0, K, V)
        ;
            Result = (=),
            V = V0
        ;
            Result = (>),
            search(T1, K, V)
        )
    ;
        T = three(K0, V0, K1, V1, T0, T1, T2),
        unsafe_cast_to_ground(K0),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            search(T0, K, V)
        ;
            Result0 = (=),
            V = V0
        ;
            Result0 = (>),
            unsafe_cast_to_ground(K1),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                search(T1, K, V)
            ;
                Result1 = (=),
                V = V1
            ;
                Result1 = (>),
                search(T2, K, V)
            )
        )
    ;
        T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        unsafe_cast_to_ground(K1),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                search(T0, K, V)
            ;
                Result0 = (=),
                V = V0
            ;
                Result0 = (>),
                search(T1, K, V)
            )
        ;
            Result1 = (=),
            V = V1
        ;
            Result1 = (>),
            unsafe_cast_to_ground(K2),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                search(T2, K, V)
            ;
                Result2 = (=),
                V = V2
            ;
                Result2 = (>),
                search(T3, K, V)
            )
        )
    ).

lookup(T, K, V) :-
    promise_pure (
        ( if search(T, K, V0) then
            V = V0
        else
            report_lookup_error("any_tree234.lookup: key not found.", K, V)
        )
    ).

%---------------------------------------------------------------------------%

lower_bound_search(T, SearchK, K, V) :-
    promise_pure (
        (
            T = empty,
            fail
        ;
            T = two(K0, V0, T0, T1),
            unsafe_cast_to_ground(K0),
            compare(Result, SearchK, K0),
            (
                Result = (<),
                lower_bound_search(T0, SearchK, K, V)
            ;
                Result = (=),
                K = SearchK,
                V = V0
            ;
                Result = (>),
                ( if lower_bound_search(T1, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    T = two(_, V0, _, _),
                    K = K0,
                    V = V0
                )
            )
        ;
            T = three(K0, V0, K1, V1, T0, T1, T2),
            unsafe_cast_to_ground(K0),
            compare(Result0, SearchK, K0),
            (
                Result0 = (<),
                lower_bound_search(T0, SearchK, K, V)
            ;
                Result0 = (=),
                K = SearchK,
                V = V0
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, SearchK, K1),
                (
                    Result1 = (<),
                    ( if lower_bound_search(T1, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        T = three(_, V0, _, _, _, _, _),
                        K = K0,
                        V = V0
                    )
                ;
                    Result1 = (=),
                    K = SearchK,
                    V = V1
                ;
                    Result1 = (>),
                    ( if lower_bound_search(T2, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K1,
                        V = V1
                    )
                )
            )
        ;
            T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
            unsafe_cast_to_ground(K1),
            compare(Result1, SearchK, K1),
            (
                Result1 = (<),
                unsafe_cast_to_ground(K0),
                compare(Result0, SearchK, K0),
                (
                    Result0 = (<),
                    lower_bound_search(T0, SearchK, K, V)
                ;
                    Result0 = (=),
                    K = SearchK,
                    V = V0
                ;
                    Result0 = (>),
                    ( if lower_bound_search(T1, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K0,
                        V = V0
                    )
                )
            ;
                Result1 = (=),
                K = SearchK,
                V = V1
            ;
                Result1 = (>),
                unsafe_cast_to_ground(K2),
                compare(Result2, SearchK, K2),
                (
                    Result2 = (<),
                    ( if lower_bound_search(T2, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K1,
                        V = V1
                    )
                ;
                    Result2 = (=),
                    K = SearchK,
                    V = V2
                ;
                    Result2 = (>),
                    ( if lower_bound_search(T3, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K2,
                        V = V2
                    )
                )
            )
        )
    ).

lower_bound_lookup(T, SearchK, K, V) :-
    promise_pure (
        ( if lower_bound_search(T, SearchK, K0, V0) then
            K = K0,
            V = V0
        else
            report_lookup_error(
                "any_tree234.lower_bound_lookup: key not found.",
                SearchK, V)
        )
    ).

%---------------------------------------------------------------------------%

upper_bound_search(T, SearchK, K, V) :-
    promise_pure (
        (
            T = empty,
            fail
        ;
            T = two(K0, V0, T0, T1),
            unsafe_cast_to_ground(K0),
            compare(Result, SearchK, K0),
            (
                Result = (<),
                ( if upper_bound_search(T0, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    T = two(_, V0, _, _),
                    K = K0,
                    V = V0
                )
            ;
                Result = (=),
                K = SearchK,
                V = V0
            ;
                Result = (>),
                upper_bound_search(T1, SearchK, K, V)
            )
        ;
            T = three(K0, V0, K1, V1, T0, T1, T2),
            unsafe_cast_to_ground(K0),
            compare(Result0, SearchK, K0),
            (
                Result0 = (<),
                ( if upper_bound_search(T0, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K0,
                    V = V0
                )
            ;
                Result0 = (=),
                K = SearchK,
                V = V0
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, SearchK, K1),
                (
                    Result1 = (<),
                    ( if upper_bound_search(T1, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K1,
                        V = V1
                    )
                ;
                    Result1 = (=),
                    K = SearchK,
                    V = V1
                ;
                    Result1 = (>),
                    upper_bound_search(T2, SearchK, K, V)
                )
            )
        ;
            T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
            unsafe_cast_to_ground(K1),
            compare(Result1, SearchK, K1),
            (
                Result1 = (<),
                unsafe_cast_to_ground(K0),
                compare(Result0, SearchK, K0),
                (
                    Result0 = (<),
                    ( if upper_bound_search(T0, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K0,
                        V = V0
                    )
                ;
                    Result0 = (=),
                    K = SearchK,
                    V = V0
                ;
                    Result0 = (>),
                    ( if upper_bound_search(T1, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K1,
                        V = V1
                    )
                )
            ;
                Result1 = (=),
                K = SearchK,
                V = V1
            ;
                Result1 = (>),
                unsafe_cast_to_ground(K2),
                compare(Result2, SearchK, K2),
                (
                    Result2 = (<),
                    ( if upper_bound_search(T2, SearchK, Kp, Vp) then
                        K = Kp,
                        V = Vp
                    else
                        K = K2,
                        V = V2
                    )
                ;
                    Result2 = (=),
                    K = SearchK,
                    V = V2
                ;
                    Result2 = (>),
                    upper_bound_search(T3, SearchK, K, V)
                )
            )
        )
    ).

upper_bound_lookup(T, SearchK, K, V) :-
    promise_pure (
        ( if upper_bound_search(T, SearchK, K0, V0) then
            K = K0,
            V = V0
        else
            report_lookup_error(
                "any_tree234.upper_bound_lookup: key not found.",
                SearchK, V)
        )
    ).

%---------------------------------------------------------------------------%

max_key(T0) = Result :-
    ( T0 = two(NodeMaxKey, _, _, NodeMaxSubtree)
    ; T0 = three(_, _, NodeMaxKey, _, _, _, NodeMaxSubtree)
    ; T0 = four(_, _, _, _, NodeMaxKey, _, _, _, _, NodeMaxSubtree)
    ),
    promise_pure (
        ( if MaxSubtreeKey = max_key(NodeMaxSubtree) then
            MaxKey = MaxSubtreeKey
        else
            MaxKey = NodeMaxKey
        )
    ),
    % XXX This needs to be written this way in order to avoid errors due to
    % limitations in the current mode analysis and/or intermodule optimization
    % framework.
    unsafe_cast_to_ground(MaxKey),
    Result = MaxKey.

min_key(T0) = Result :-
    ( T0 = two(NodeMinKey, _, NodeMinSubtree, _)
    ; T0 = three(NodeMinKey, _, _, _, NodeMinSubtree, _, _)
    ; T0 = four(NodeMinKey, _, _, _, _, _, NodeMinSubtree, _, _, _)
    ),
    promise_pure (
        ( if MinSubtreeKey = min_key(NodeMinSubtree) then
            MinKey = MinSubtreeKey
        else
            MinKey = NodeMinKey
        )
    ),
    % XXX This needs to be written this way in order to avoid errors due to
    % limitations in the current mode analysis and/or intermodule optimization
    % framework.
    unsafe_cast_to_ground(MinKey),
    Result = MinKey.

%---------------------------------------------------------------------------%

update(K, V, Tin, Tout) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        unsafe_cast_to_ground(K0),
        compare(Result, K, K0),
        (
            Result = (<),
            update(K, V, T0, NewT0),
            Tout = two(K0, V0, NewT0, T1)
        ;
            Result = (=),
            Tout = two(K0, V, T0, T1)
        ;
            Result = (>),
            update(K, V, T1, NewT1),
            Tout = two(K0, V0, T0, NewT1)
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        unsafe_cast_to_ground(K0),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            update(K, V, T0, NewT0),
            Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
        ;
            Result0 = (=),
            Tout = three(K0, V, K1, V1, T0, T1, T2)
        ;
            Result0 = (>),
            unsafe_cast_to_ground(K1),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                update(K, V, T1, NewT1),
                Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
            ;
                Result1 = (=),
                Tout = three(K0, V0, K1, V, T0, T1, T2)
            ;
                Result1 = (>),
                update(K, V, T2, NewT2),
                Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        unsafe_cast_to_ground(K1),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                update(K, V, T0, NewT0),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    NewT0, T1, T2, T3)
            ;
                Result0 = (=),
                Tout = four(K0, V, K1, V1, K2, V2,
                    T0, T1, T2, T3)
            ;
                Result0 = (>),
                update(K, V, T1, NewT1),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    T0, NewT1, T2, T3)
            )
        ;
            Result1 = (=),
            Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            unsafe_cast_to_ground(K2),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                update(K, V, T2, NewT2),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    T0, T1, NewT2, T3)
            ;
                Result2 = (=),
                Tout = four(K0, V0, K1, V1, K2, V,
                    T0, T1, T2, T3)
            ;
                Result2 = (>),
                update(K, V, T3, NewT3),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    T0, T1, T2, NewT3)
            )
        )
    ).

%---------------------------------------------------------------------------%

transform_value(P, K, Tin, Tout) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        unsafe_cast_to_ground(K0),
        compare(Result, K, K0),
        (
            Result = (<),
            transform_value(P, K, T0, NewT0),
            Tout = two(K0, V0, NewT0, T1)
        ;
            Result = (=),
            P(V0, VNew),
            Tout = two(K0, VNew, T0, T1)
        ;
            Result = (>),
            transform_value(P, K, T1, NewT1),
            Tout = two(K0, V0, T0, NewT1)
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        unsafe_cast_to_ground(K0),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            transform_value(P, K, T0, NewT0),
            Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
        ;
            Result0 = (=),
            P(V0, VNew),
            Tout = three(K0, VNew, K1, V1, T0, T1, T2)
        ;
            Result0 = (>),
            unsafe_cast_to_ground(K1),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                transform_value(P, K, T1, NewT1),
                Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
            ;
                Result1 = (=),
                P(V1, VNew),
                Tout = three(K0, V0, K1, VNew, T0, T1, T2)
            ;
                Result1 = (>),
                transform_value(P, K, T2, NewT2),
                Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        unsafe_cast_to_ground(K1),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                transform_value(P, K, T0, NewT0),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    NewT0, T1, T2, T3)
            ;
                Result0 = (=),
                P(V0, VNew),
                Tout = four(K0, VNew, K1, V1, K2, V2,
                    T0, T1, T2, T3)
            ;
                Result0 = (>),
                transform_value(P, K, T1, NewT1),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    T0, NewT1, T2, T3)
            )
        ;
            Result1 = (=),
            P(V1, VNew),
            Tout = four(K0, V0, K1, VNew, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            unsafe_cast_to_ground(K2),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                transform_value(P, K, T2, NewT2),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    T0, T1, NewT2, T3)
            ;
                Result2 = (=),
                P(V2, VNew),
                Tout = four(K0, V0, K1, V1, K2, VNew,
                    T0, T1, T2, T3)
            ;
                Result2 = (>),
                transform_value(P, K, T3, NewT3),
                Tout = four(K0, V0, K1, V1, K2, V2,
                    T0, T1, T2, NewT3)
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- inst two(K, V, T) for any_tree234/2
    --->    two(K, V, T, T).

:- inst three(K, V, T) for any_tree234/2
     --->   three(K, V, K, V, T, T, T).

:- inst four(K, V, T) for any_tree234/2
     --->   four(K, V, K, V, K, V, T, T, T, T).

%---------------------------------------------------------------------------%

:- pred split_four(any_tree234(K, V)::in(four(any, any, any)),
    K::oa, V::oa,
    any_tree234(K, V)::out(two(any, any, any)),
    any_tree234(K, V)::out(two(any, any, any))) is det.

split_four(Tin, MidK, MidV, Sub0, Sub1) :-
    Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
    Sub0 = two(K0, V0, T0, T1),
    MidK = K1,
    MidV = V1,
    Sub1 = two(K2, V2, T2, T3).

%---------------------------------------------------------------------------%

% insert is implemented using the simple top-down
% approach described ia eg Sedgwick which splits 4 nodes into
% two 2 nodes on the downward traversal of the tree as we
% search for the right place to insert the new key-value pair.
% We know we have the right place if the subtrees of the node
% are empty (ia which case we expand the node - which will always
% work because we already split 4 nodes into 2 nodes), or if the
% tree itself is empty.
% This algorithm is O(lgN).

insert(K, V, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(K, V, empty, empty)
    ;
        Tin = two(_, _, _, _),
        insert2(K, V, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _, _, _),
        insert3(K, V, Tin, Tout)
    ;
        Tin = four(_, _, _, _, _, _, _, _, _, _),
        split_four(Tin, MidK, MidV, Sub0, Sub1),
        unsafe_cast_to_ground(MidK),
        compare(Result1, K, MidK),
        (
            Result1 = (<),
            insert2(K, V, Sub0, NewSub0),
            Tout = two(MidK, MidV, NewSub0, Sub1)
        ;
            Result1 = (=),
            fail
        ;
            Result1 = (>),
            insert2(K, V, Sub1, NewSub1),
            Tout = two(MidK, MidV, Sub0, NewSub1)
        )
    ).

:- pred insert2(K::in, V::ia,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa) is semidet.

insert2(K, V, two(K0, V0, T0, T1), Tout) :-
    promise_pure (
        ( if
            T0 = empty
            % T1 = empty implied by T0 = empty
        then
            unsafe_cast_to_ground(K0),
            compare(Result, K, K0),
            (
                Result = (<),
                Tout = three(K, V, K0, V0, empty, empty, empty)
            ;
                Result = (=),
                fail
            ;
                Result = (>),
                Tout = three(K0, V0, K, V, empty, empty, empty)
            )
        else
            unsafe_cast_to_ground(K0),
            compare(Result, K, K0),
            (
                Result = (<),
                (
                    T0 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T0, MT0K, MT0V, T00, T01),
                    unsafe_cast_to_ground(MT0K),
                    compare(Result1, K, MT0K),
                    (
                        Result1 = (<),
                        insert2(K, V, T00, NewT00),
                        Tout = three(MT0K, MT0V, K0, V0,
                            NewT00, T01, T1)
                    ;
                        Result1 = (=),
                        fail
                    ;
                        Result1 = (>),
                        insert2(K, V, T01, NewT01),
                        Tout = three(MT0K, MT0V, K0, V0,
                            T00, NewT01, T1)
                    )
                ;
                    T0 = three(_, _, _, _, _, _, _),
                    insert3(K, V, T0, NewT0),
                    Tout = two(K0, V0, NewT0, T1)
                ;
                    T0 = two(_, _, _, _),
                    insert2(K, V, T0, NewT0),
                    Tout = two(K0, V0, NewT0, T1)
                ;
                    T0 = empty,
                    NewT0 = two(K, V, empty, empty),
                    Tout = two(K0, V0, NewT0, T1)
                )
            ;
                Result = (=),
                fail
            ;
                Result = (>),
                (
                    T1 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T1, MT1K, MT1V, T10, T11),
                    unsafe_cast_to_ground(MT1K),
                    compare(Result1, K, MT1K),
                    (
                        Result1 = (<),
                        insert2(K, V, T10, NewT10),
                        Tout = three(K0, V0, MT1K, MT1V,
                            T0, NewT10, T11)
                    ;
                        Result1 = (=),
                        fail
                    ;
                        Result1 = (>),
                        insert2(K, V, T11, NewT11),
                        Tout = three(K0, V0, MT1K, MT1V,
                            T0, T10, NewT11)
                    )
                ;
                    T1 = three(_, _, _, _, _, _, _),
                    insert3(K, V, T1, NewT1),
                    Tout = two(K0, V0, T0, NewT1)
                ;
                    T1 = two(_, _, _, _),
                    insert2(K, V, T1, NewT1),
                    Tout = two(K0, V0, T0, NewT1)
                ;
                    T1 = empty,
                    NewT1 = two(K, V, empty, empty),
                    Tout = two(K0, V0, T0, NewT1)
                )
            )
        )
    ).

:- pred insert3(K::in, V::ia,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa) is semidet.

insert3(K, V, three(K0, V0, K1, V1, T0, T1, T2), Tout) :-
    promise_pure (
        ( if
            T0 = empty
            % T1 = empty implied by T0 = empty
            % T2 = empty implied by T0 = empty
        then
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                Tout = four(K, V, K0, V0, K1, V1,
                    empty, empty, empty, empty)
            ;
                Result0 = (=),
                fail
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, K, K1),
                (
                    Result1 = (<),
                    Tout = four(K0, V0, K, V, K1, V1,
                        empty, empty, empty, empty)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    Tout = four(K0, V0, K1, V1, K, V,
                        empty, empty, empty, empty)
                )
            )
        else
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                (
                    T0 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T0, MT0K, MT0V, T00, T01),
                    unsafe_cast_to_ground(MT0K),
                    compare(ResultM, K, MT0K),
                    (
                        ResultM = (<),
                        insert2(K, V, T00, NewT00),
                        Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                            NewT00, T01, T1, T2)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        insert2(K, V, T01, NewT01),
                        Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                            T00, NewT01, T1, T2)
                    )
                ;
                    T0 = three(_, _, _, _, _, _, _),
                    insert3(K, V, T0, NewT0),
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                ;
                    T0 = two(_, _, _, _),
                    insert2(K, V, T0, NewT0),
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                ;
                    T0 = empty,
                    NewT0 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                )
            ;
                Result0 = (=),
                fail
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, K, K1),
                (
                    Result1 = (<),
                    (
                        T1 = four(_, _, _, _, _, _, _, _, _, _),
                        split_four(T1, MT1K, MT1V, T10, T11),
                        unsafe_cast_to_ground(MT1K),
                        compare(ResultM, K, MT1K),
                        (
                            ResultM = (<),
                            insert2(K, V, T10, NewT10),
                            Tout = four(K0, V0, MT1K, MT1V,
                                K1, V1,
                                T0, NewT10, T11, T2)
                        ;
                            ResultM = (=),
                            fail
                        ;
                            ResultM = (>),
                            insert2(K, V, T11, NewT11),
                            Tout = four(K0, V0, MT1K, MT1V,
                                K1, V1,
                                T0, T10, NewT11, T2)
                        )
                    ;
                        T1 = three(_, _, _, _, _, _, _),
                        insert3(K, V, T1, NewT1),
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2)
                    ;
                        T1 = two(_, _, _, _),
                        insert2(K, V, T1, NewT1),
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2)
                    ;
                        T1 = empty,
                        NewT1 = two(K, V, empty, empty),
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2)
                    )
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    (
                        T2 = four(_, _, _, _, _, _, _, _, _, _),
                        split_four(T2, MT2K, MT2V,
                            T20, T21),
                        unsafe_cast_to_ground(MT2K),
                        compare(ResultM, K, MT2K),
                        (
                            ResultM = (<),
                            insert2(K, V, T20, NewT20),
                            Tout = four(K0, V0, K1, V1,
                                MT2K, MT2V,
                                T0, T1, NewT20, T21)
                        ;
                            ResultM = (=),
                            fail
                        ;
                            ResultM = (>),
                            insert2(K, V, T21, NewT21),
                            Tout = four(K0, V0, K1, V1,
                                MT2K, MT2V,
                                T0, T1, T20, NewT21)
                        )
                    ;
                        T2 = three(_, _, _, _, _, _, _),
                        insert3(K, V, T2, NewT2),
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2)
                    ;
                        T2 = two(_, _, _, _),
                        insert2(K, V, T2, NewT2),
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2)
                    ;
                        T2 = empty,
                        NewT2 = two(K, V, empty, empty),
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2)
                    )
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

% set uses the same algorithm as used for insert,
% except that instead of failing for equal keys, we replace the value.

set(K, V, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(K, V, empty, empty)
    ;
        Tin = two(_, _, _, _),
        set2(K, V, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _, _, _),
        set3(K, V, Tin, Tout)
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        unsafe_cast_to_ground(K1),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            Sub0 = two(K0, V0, T0, T1),
            Sub1 = two(K2, V2, T2, T3),
            set2(K, V, Sub0, NewSub0),
            Tout = two(K1, V1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            Sub0 = two(K0, V0, T0, T1),
            Sub1 = two(K2, V2, T2, T3),
            set2(K, V, Sub1, NewSub1),
            Tout = two(K1, V1, Sub0, NewSub1)
        )
    ).

:- pred set2(K::in, V::ia,
    any_tree234(K, V)::in(two(any, any, any)), any_tree234(K, V)::oa) is det.

set2(K, V, two(K0, V0, T0, T1), Tout) :-
    promise_pure (
        ( if
            T0 = empty
            % T1 = empty implied by T0 = empty
        then
            unsafe_cast_to_ground(K0),
            compare(Result, K, K0),
            (
                Result = (<),
                Tout = three(K, V, K0, V0, empty, empty, empty)
            ;
                Result = (=),
                Tout = two(K, V, T0, T1)
            ;
                Result = (>),
                Tout = three(K0, V0, K, V, empty, empty, empty)
            )
        else
            unsafe_cast_to_ground(K0),
            compare(Result, K, K0),
            (
                Result = (<),
                (
                    T0 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T0, MT0K, MT0V, T00, T01),
                    unsafe_cast_to_ground(MT0K),
                    compare(Result1, K, MT0K),
                    (
                        Result1 = (<),
                        set2(K, V, T00, NewT00),
                        Tout = three(MT0K, MT0V, K0, V0,
                            NewT00, T01, T1)
                    ;
                        Result1 = (=),
                        Tout = three(MT0K, V, K0, V0,
                            T00, T01, T1)
                    ;
                        Result1 = (>),
                        set2(K, V, T01, NewT01),
                        Tout = three(MT0K, MT0V, K0, V0,
                            T00, NewT01, T1)
                    )
                ;
                    T0 = three(_, _, _, _, _, _, _),
                    set3(K, V, T0, NewT0),
                    Tout = two(K0, V0, NewT0, T1)
                ;
                    T0 = two(_, _, _, _),
                    set2(K, V, T0, NewT0),
                    Tout = two(K0, V0, NewT0, T1)
                ;
                    T0 = empty,
                    NewT0 = two(K, V, empty, empty),
                    Tout = two(K0, V0, NewT0, T1)
                )
            ;
                Result = (=),
                Tout = two(K, V, T0, T1)
            ;
                Result = (>),
                (
                    T1 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T1, MT1K, MT1V, T10, T11),
                    unsafe_cast_to_ground(MT1K),
                    compare(Result1, K, MT1K),
                    (
                        Result1 = (<),
                        set2(K, V, T10, NewT10),
                        Tout = three(K0, V0, MT1K, MT1V,
                            T0, NewT10, T11)
                    ;
                        Result1 = (=),
                        Tout = three(K0, V0, MT1K, V,
                            T0, T10, T11)
                    ;
                        Result1 = (>),
                        set2(K, V, T11, NewT11),
                        Tout = three(K0, V0, MT1K, MT1V,
                            T0, T10, NewT11)
                    )
                ;
                    T1 = three(_, _, _, _, _, _, _),
                    set3(K, V, T1, NewT1),
                    Tout = two(K0, V0, T0, NewT1)
                ;
                    T1 = two(_, _, _, _),
                    set2(K, V, T1, NewT1),
                    Tout = two(K0, V0, T0, NewT1)
                ;
                    T1 = empty,
                    NewT1 = two(K, V, empty, empty),
                    Tout = two(K0, V0, T0, NewT1)
                )
            )
        )
    ).

:- pred set3(K::in, V::ia,
    any_tree234(K, V)::in(three(any, any, any)), any_tree234(K, V)::oa) is det.

set3(K, V, three(K0, V0, K1, V1, T0, T1, T2), Tout) :-
    promise_pure (
        ( if
            T0 = empty
            % T1 = empty implied by T0 = empty
            % T2 = empty implied by T0 = empty
        then
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                Tout = four(K, V, K0, V0, K1, V1,
                    empty, empty, empty, empty)
            ;
                Result0 = (=),
                Tout = three(K0, V, K1, V1,
                    empty, empty, empty)
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, K, K1),
                (
                    Result1 = (<),
                    Tout = four(K0, V0, K, V, K1, V1,
                        empty, empty, empty, empty)
                ;
                    Result1 = (=),
                    Tout = three(K0, V0, K1, V,
                        empty, empty, empty)
                ;
                    Result1 = (>),
                    Tout = four(K0, V0, K1, V1, K, V,
                        empty, empty, empty, empty)
                )
            )
        else
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                (
                    T0 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T0, MT0K, MT0V, T00, T01),
                    unsafe_cast_to_ground(MT0K),
                    compare(ResultM, K, MT0K),
                    (
                        ResultM = (<),
                        set2(K, V, T00, NewT00),
                        Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                            NewT00, T01, T1, T2)
                    ;
                        ResultM = (=),
                        Tout = four(MT0K, V, K0, V0, K1, V1,
                            T00, T01, T1, T2)
                    ;
                        ResultM = (>),
                        set2(K, V, T01, NewT01),
                        Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                            T00, NewT01, T1, T2)
                    )
                ;
                    T0 = three(_, _, _, _, _, _, _),
                    set3(K, V, T0, NewT0),
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                ;
                    T0 = two(_, _, _, _),
                    set2(K, V, T0, NewT0),
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                ;
                    T0 = empty,
                    NewT0 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                )
            ;
                Result0 = (=),
                Tout = three(K0, V, K1, V1, T0, T1, T2)
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, K, K1),
                (
                    Result1 = (<),
                    (
                        T1 = four(_, _, _, _, _, _, _, _, _, _),
                        split_four(T1, MT1K, MT1V,
                            T10, T11),
                        unsafe_cast_to_ground(MT1K),
                        compare(ResultM, K, MT1K),
                        (
                            ResultM = (<),
                            set2(K, V, T10, NewT10),
                            Tout = four(K0, V0, MT1K, MT1V,
                                K1, V1,
                                T0, NewT10, T11, T2)
                        ;
                            ResultM = (=),
                            Tout = four(K0, V0, MT1K, V,
                                K1, V1,
                                T0, T10, T11, T2)
                        ;
                            ResultM = (>),
                            set2(K, V, T11, NewT11),
                            Tout = four(K0, V0, MT1K, MT1V,
                                K1, V1,
                                T0, T10, NewT11, T2)
                        )
                    ;
                        T1 = three(_, _, _, _, _, _, _),
                        set3(K, V, T1, NewT1),
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2)
                    ;
                        T1 = two(_, _, _, _),
                        set2(K, V, T1, NewT1),
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2)
                    ;
                        T1 = empty,
                        NewT1 = two(K, V, empty, empty),
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2)
                    )
                ;
                    Result1 = (=),
                    Tout = three(K0, V0, K, V, T0, T1, T2)
                ;
                    Result1 = (>),
                    (
                        T2 = four(_, _, _, _, _, _, _, _, _, _),
                        split_four(T2, MT2K, MT2V,
                            T20, T21),
                        unsafe_cast_to_ground(MT2K),
                        compare(ResultM, K, MT2K),
                        (
                            ResultM = (<),
                            set2(K, V, T20, NewT20),
                            Tout = four(K0, V0, K1, V1,
                                MT2K, MT2V,
                                T0, T1, NewT20, T21)
                        ;
                            ResultM = (=),
                            Tout = four(K0, V0, K1, V1,
                                MT2K, V,
                                T0, T1, T20, T21)
                        ;
                            ResultM = (>),
                            set2(K, V, T21, NewT21),
                            Tout = four(K0, V0, K1, V1,
                                MT2K, MT2V,
                                T0, T1, T20, NewT21)
                        )
                    ;
                        T2 = three(_, _, _, _, _, _, _),
                        set3(K, V, T2, NewT2),
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2)
                    ;
                        T2 = two(_, _, _, _),
                        set2(K, V, T2, NewT2),
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2)
                    ;
                        T2 = empty,
                        NewT2 = two(K, V, empty, empty),
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2)
                    )
                )
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

delete(K, Tin, Tout) :-
    delete_2(K, Tin, Tout, _).

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.

:- pred delete_2( K::in,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa, bool::out) is det.

delete_2(K, Tin, Tout, RH) :-
    promise_pure (
        (
            Tin = empty,
            Tout = empty,
            RH = no
        ;
            Tin = two(K0, V0, T0, T1),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                delete_2(K, T0, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = two(K0, V0, NewT0, T1),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if
                    remove_smallest_2(ST1K, ST1V, T1, NewT1, RHT1)
                then
                    (
                        RHT1 = yes,
                        fix_2node_t1(ST1K, ST1V, T0, NewT1,
                            Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = two(ST1K, ST1V, T0, NewT1),
                        RH = no
                    )
                else
                    % T1 must be empty
                    Tout = T0,
                    RH = yes
                )
            ;
                Result0 = (>),
                delete_2(K, T1, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(K0, V0, T0, NewT1),
                    RH = no
                )
            )
        ;
            Tin = three(K0, V0, K1, V1, T0, T1, T2),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                delete_2(K, T0, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
                        Tout, RH)
                ;
                    RHT0 = no,
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if
                    remove_smallest_2(ST1K, ST1V, T1, NewT1, RHT1)
                then
                    (
                        RHT1 = yes,
                        fix_3node_t1(ST1K, ST1V, K1, V1,
                            T0, NewT1, T2, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = three(ST1K, ST1V, K1, V1,
                            T0, NewT1, T2),
                        RH = no
                    )
                else
                    % T1 must be empty
                    Tout = two(K1, V1, T0, T2),
                    RH = no
                )
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, K, K1),
                (
                    Result1 = (<),
                    delete_2(K, T1, NewT1, RHT1),
                    (
                        RHT1 = yes,
                        fix_3node_t1(K0, V0, K1, V1,
                            T0, NewT1, T2, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2),
                        RH = no
                    )
                ;
                    Result1 = (=),
                    ( if
                        remove_smallest_2(ST2K, ST2V, T2, NewT2, RHT2)
                    then
                        (
                            RHT2 = yes,
                            fix_3node_t2(K0, V0, ST2K, ST2V,
                                T0, T1, NewT2, Tout, RH)
                        ;
                            RHT2 = no,
                            Tout = three(K0, V0, ST2K, ST2V,
                                T0, T1, NewT2),
                            RH = no
                        )
                    else
                        % T2 must be empty
                        Tout = two(K0, V0, T0, T1),
                        RH = no
                    )
                ;
                    Result1 = (>),
                    delete_2(K, T2, NewT2, RHT2),
                    (
                        RHT2 = yes,
                        fix_3node_t2(K0, V0, K1, V1,
                            T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2),
                        RH = no
                    )
                )
            )
        ;
            Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
            unsafe_cast_to_ground(K1),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                unsafe_cast_to_ground(K0),
                compare(Result0, K, K0),
                (
                    Result0 = (<),
                    delete_2(K, T0, NewT0, RHT0),
                    (
                        RHT0 = yes,
                        fix_4node_t0(K0, V0, K1, V1, K2, V2,
                            NewT0, T1, T2, T3, Tout, RH)
                    ;
                        RHT0 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            NewT0, T1, T2, T3),
                        RH = no
                    )
                ;
                    Result0 = (=),
                    ( if
                        remove_smallest_2(ST1K, ST1V, T1, NewT1, RHT1)
                    then
                        (
                            RHT1 = yes,
                            fix_4node_t1(ST1K, ST1V, K1, V1,
                                K2, V2,
                                T0, NewT1, T2, T3,
                                Tout, RH)
                        ;
                            RHT1 = no,
                            Tout = four(ST1K, ST1V, K1, V1,
                                K2, V2,
                                T0, NewT1, T2, T3),
                            RH = no
                        )
                    else
                        % T1 must be empty
                        Tout = three(K1, V1, K2, V2,
                            T0, T2, T3),
                        RH = no
                    )
                ;
                    Result0 = (>),
                    delete_2(K, T1, NewT1, RHT1),
                    (
                        RHT1 = yes,
                        fix_4node_t1(K0, V0, K1, V1, K2, V2,
                            T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            T0, NewT1, T2, T3),
                        RH = no
                    )
                )
            ;
                Result1 = (=),
                ( if
                    remove_smallest_2(ST2K, ST2V, T2, NewT2, RHT2)
                then
                    (
                        RHT2 = yes,
                        fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
                            T0, T1, NewT2, T3, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = four(K0, V0, ST2K, ST2V, K2, V2,
                            T0, T1, NewT2, T3),
                        RH = no
                    )
                else
                    % T2 must be empty
                    Tout = three(K0, V0, K2, V2, T0, T1, T3),
                    RH = no
                )
            ;
                Result1 = (>),
                unsafe_cast_to_ground(K2),
                compare(Result2, K, K2),
                (
                    Result2 = (<),
                    delete_2(K, T2, NewT2, RHT2),
                    (
                        RHT2 = yes,
                        fix_4node_t2(K0, V0, K1, V1, K2, V2,
                            T0, T1, NewT2, T3, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            T0, T1, NewT2, T3),
                        RH = no
                    )
                ;
                    Result2 = (=),
                    ( if
                        remove_smallest_2(ST3K, ST3V, T3, NewT3, RHT3)
                    then
                        (
                            RHT3 = yes,
                            fix_4node_t3(K0, V0, K1, V1,
                                ST3K, ST3V,
                                T0, T1, T2, NewT3,
                                Tout, RH)
                        ;
                            RHT3 = no,
                            Tout = four(K0, V0, K1, V1,
                                ST3K, ST3V,
                                T0, T1, T2, NewT3),
                            RH = no
                        )
                    else
                        % T3 must be empty
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, T2),
                        RH = no
                    )
                ;
                    Result2 = (>),
                    delete_2(K, T3, NewT3, RHT3),
                    (
                        RHT3 = yes,
                        fix_4node_t3(K0, V0, K1, V1, K2, V2,
                            T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            T0, T1, T2, NewT3),
                        RH = no
                    )
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

remove(K, V, Tin, Tout) :-
    % We use the same algorithm as delete.
    remove_2(K, V, Tin, Tout, _).

:- pred remove_2(K::in, V::oa,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa, bool::out) is semidet.

remove_2(K, V, Tin, Tout, RH) :-
    promise_pure (
        (
            Tin = empty,
            fail
        ;
            Tin = two(K0, V0, T0, T1),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                remove_2(K, V, T0, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = two(K0, V0, NewT0, T1),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if
                    remove_smallest_2(ST1K, ST1V, T1, NewT1, RHT1)
                then
                    (
                        RHT1 = yes,
                        fix_2node_t1(ST1K, ST1V, T0, NewT1,
                            Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = two(ST1K, ST1V, T0, NewT1),
                        RH = no
                    )
                else
                    % T1 must be empty
                    Tout = T0,
                    RH = yes
                ),
                V = V0
            ;
                Result0 = (>),
                remove_2(K, V, T1, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(K0, V0, T0, NewT1),
                    RH = no
                )
            )
        ;
            Tin = three(K0, V0, K1, V1, T0, T1, T2),
            unsafe_cast_to_ground(K0),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                remove_2(K, V, T0, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
                        Tout, RH)
                ;
                    RHT0 = no,
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if
                    remove_smallest_2(ST1K, ST1V, T1, NewT1, RHT1)
                then
                    (
                        RHT1 = yes,
                        fix_3node_t1(ST1K, ST1V, K1, V1,
                            T0, NewT1, T2, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = three(ST1K, ST1V, K1, V1,
                            T0, NewT1, T2),
                        RH = no
                    )
                else
                    % T1 must be empty
                    Tout = two(K1, V1, T0, T2),
                    RH = no
                ),
                V = V0
            ;
                Result0 = (>),
                unsafe_cast_to_ground(K1),
                compare(Result1, K, K1),
                (
                    Result1 = (<),
                    remove_2(K, V, T1, NewT1, RHT1),
                    (
                        RHT1 = yes,
                        fix_3node_t1(K0, V0, K1, V1,
                            T0, NewT1, T2, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = three(K0, V0, K1, V1,
                            T0, NewT1, T2),
                        RH = no
                    )
                ;
                    Result1 = (=),
                    ( if
                        remove_smallest_2(ST2K, ST2V, T2, NewT2, RHT2)
                    then
                        (
                            RHT2 = yes,
                            fix_3node_t2(K0, V0, ST2K, ST2V,
                                T0, T1, NewT2, Tout, RH)
                        ;
                            RHT2 = no,
                            Tout = three(K0, V0, ST2K, ST2V,
                                T0, T1, NewT2),
                            RH = no
                        )
                    else
                        % T2 must be empty
                        Tout = two(K0, V0, T0, T1),
                        RH = no
                    ),
                    V = V1
                ;
                    Result1 = (>),
                    remove_2(K, V, T2, NewT2, RHT2),
                    (
                        RHT2 = yes,
                        fix_3node_t2(K0, V0, K1, V1,
                            T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, NewT2),
                        RH = no
                    )
                )
            )
        ;
            Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
            unsafe_cast_to_ground(K1),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                unsafe_cast_to_ground(K0),
                compare(Result0, K, K0),
                (
                    Result0 = (<),
                    remove_2(K, V, T0, NewT0, RHT0),
                    (
                        RHT0 = yes,
                        fix_4node_t0(K0, V0, K1, V1, K2, V2,
                            NewT0, T1, T2, T3, Tout, RH)
                    ;
                        RHT0 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            NewT0, T1, T2, T3),
                        RH = no
                    )
                ;
                    Result0 = (=),
                    ( if
                        remove_smallest_2(ST1K, ST1V, T1, NewT1, RHT1)
                    then
                        (
                            RHT1 = yes,
                            fix_4node_t1(ST1K, ST1V, K1, V1,
                                K2, V2,
                                T0, NewT1, T2, T3,
                                Tout, RH)
                        ;
                            RHT1 = no,
                            Tout = four(ST1K, ST1V, K1, V1,
                                K2, V2,
                                T0, NewT1, T2, T3),
                            RH = no
                        )
                    else
                        % T1 must be empty
                        Tout = three(K1, V1, K2, V2,
                            T0, T2, T3),
                        RH = no
                    ),
                    V = V0
                ;
                    Result0 = (>),
                    remove_2(K, V, T1, NewT1, RHT1),
                    (
                        RHT1 = yes,
                        fix_4node_t1(K0, V0, K1, V1, K2, V2,
                            T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            T0, NewT1, T2, T3),
                        RH = no
                    )
                )
            ;
                Result1 = (=),
                ( if
                    remove_smallest_2(ST2K, ST2V, T2, NewT2, RHT2)
                then
                    (
                        RHT2 = yes,
                        fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
                            T0, T1, NewT2, T3, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = four(K0, V0, ST2K, ST2V, K2, V2,
                            T0, T1, NewT2, T3),
                        RH = no
                    )
                else
                    % T2 must be empty
                    Tout = three(K0, V0, K2, V2, T0, T1, T3),
                    RH = no
                ),
                V = V1
            ;
                Result1 = (>),
                unsafe_cast_to_ground(K2),
                compare(Result2, K, K2),
                (
                    Result2 = (<),
                    remove_2(K, V, T2, NewT2, RHT2),
                    (
                        RHT2 = yes,
                        fix_4node_t2(K0, V0, K1, V1, K2, V2,
                            T0, T1, NewT2, T3, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            T0, T1, NewT2, T3),
                        RH = no
                    )
                ;
                    Result2 = (=),
                    ( if
                        remove_smallest_2(ST3K, ST3V, T3, NewT3, RHT3)
                    then
                        (
                            RHT3 = yes,
                            fix_4node_t3(K0, V0, K1, V1,
                                ST3K, ST3V,
                                T0, T1, T2, NewT3,
                                Tout, RH)
                        ;
                            RHT3 = no,
                            Tout = four(K0, V0, K1, V1,
                                ST3K, ST3V,
                                T0, T1, T2, NewT3),
                            RH = no
                        )
                    else
                        % T3 must be empty
                        Tout = three(K0, V0, K1, V1,
                            T0, T1, T2),
                        RH = no
                    ),
                    V = V2
                ;
                    Result2 = (>),
                    remove_2(K, V, T3, NewT3, RHT3),
                    (
                        RHT3 = yes,
                        fix_4node_t3(K0, V0, K1, V1, K2, V2,
                            T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(K0, V0, K1, V1, K2, V2,
                            T0, T1, T2, NewT3),
                        RH = no
                    )
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

    % The algorithm we use similar to delete, except that we
    % always go down the left subtree.

remove_smallest(K, V, Tin, Tout) :-
    remove_smallest_2(K0, V, Tin, Tout, _),
    % XXX This needs to be written this way in order to avoid errors due to
    % limitations in the current mode analysis and/or intermodule optimization
    % framework.
    unsafe_cast_to_ground(K0),
    K = K0.

:- pred remove_smallest_2(K::oa, V::oa,
    any_tree234(K, V)::ia, any_tree234(K, V)::oa, bool::out) is semidet.

remove_smallest_2(K, V, Tin, Tout, RH) :-
    promise_pure
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        ( if
            T0 = empty
        then
            K = K0,
            V = V0,
            Tout = T1,
            RH = yes
        else
            remove_smallest_2(K, V, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(K0, V0, NewT0, T1),
                RH = no
            )
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        ( if
            T0 = empty
        then
            K = K0,
            V = V0,
            Tout = two(K1, V1, T1, T2),
            RH = no
        else
            remove_smallest_2(K, V, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
                    Tout, RH)
            ;
                RHT0 = no,
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                RH = no
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        ( if
            T0 = empty
        then
            K = K0,
            V = V0,
            Tout = three(K1, V1, K2, V2, T1, T2, T3),
            RH = no
        else
            remove_smallest_2(K, V, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_4node_t0(K0, V0, K1, V1, K2, V2,
                    NewT0, T1, T2, T3, Tout, RH)
            ;
                RHT0 = no,
                Tout = four(K0, V0, K1, V1, K2, V2,
                    NewT0, T1, T2, T3),
                RH = no
            )
        )
    ).

%---------------------------------------------------------------------------%

    % The input to the following group of predicates are the components
    % of a two-, three- or four-node ia which the height of the indicated
    % subtree is one less that it should be. If it is possible to increase
    % the height of that subtree by moving into it elements from its
    % neighboring subtrees, do so, and return the resulting tree with RH
    % set to no. Otherwise, return a balanced tree whose height is reduced
    % by one, with RH set to yes to indicate the reduced height.

:- pred fix_2node_t0(K::ia, V::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::oa, bool::out) is det.

fix_2node_t0(K0, V0, T0, T1, Tout, RH) :-
    (
        % steal T1's leftmost subtree and combine it with T0
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
        Node = two(K0, V0, T0, T10),
        Tout = two(K10, V10, Node, NewT1),
        RH = no
    ;
        % steal T1's leftmost subtree and combine it with T0
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K11, V11, T11, T12),
        Node = two(K0, V0, T0, T10),
        Tout = two(K10, V10, Node, NewT1),
        RH = no
    ;
        % move T0 one level down and combine it with the subtrees of T1
        % this reduces the depth of the tree
        T1 = two(K10, V10, T10, T11),
        Tout = three(K0, V0, K10, V10, T0, T10, T11),
        RH = yes
    ;
        T1 = empty,
        error("unbalanced 234 tree")
        % Tout = two(K0, V0, T0, T1),
        % RH = yes
    ).

:- pred fix_2node_t1(K::ia, V::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::oa, bool::out) is det.

fix_2node_t1(K0, V0, T0, T1, Tout, RH) :-
    (
        % steal T0's leftmost subtree and combine it with T1
        T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
        NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
        Node = two(K0, V0, T03, T1),
        Tout = two(K02, V02, NewT0, Node),
        RH = no
    ;
        % steal T0's leftmost subtree and combine it with T1
        T0 = three(K00, V00, K01, V01, T00, T01, T02),
        NewT0 = two(K00, V00, T00, T01),
        Node = two(K0, V0, T02, T1),
        Tout = two(K01, V01, NewT0, Node),
        RH = no
    ;
        % move T1 one level down and combine it with the subtrees of T0
        % this reduces the depth of the tree
        T0 = two(K00, V00, T00, T01),
        Tout = three(K00, V00, K0, V0, T00, T01, T1),
        RH = yes
    ;
        T0 = empty,
        error("unbalanced 234 tree")
        % Tout = two(K0, V0, T0, T1),
        % RH = yes
    ).

:- pred fix_3node_t0(K::ia, V::ia, K::ia, V::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia, any_tree234(K, V)::oa,
        bool::out) is det.

fix_3node_t0(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
    (
        % steal T1's leftmost subtree and combine it with T0
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
        Node = two(K0, V0, T0, T10),
        Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
        RH = no
    ;
        % steal T1's leftmost subtree and combine it with T0
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K11, V11, T11, T12),
        Node = two(K0, V0, T0, T10),
        Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
        RH = no
    ;
        % move T0 one level down to become the leftmost subtree of T1
        T1 = two(K10, V10, T10, T11),
        NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
        Tout = two(K1, V1, NewT1, T2),
        RH = no
    ;
        T1 = empty,
        error("unbalanced 234 tree")
        % Tout = three(K0, V0, K1, V1, T0, T1, T2),
        % The heights of T1 and T2 are unchanged
        % RH = no
    ).

:- pred fix_3node_t1(K::ia, V::ia, K::ia, V::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia, any_tree234(K, V)::oa,
        bool::out) is det.

fix_3node_t1(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
    (
        % steal T0's rightmost subtree and combine it with T1
        T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
        NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
        Node = two(K0, V0, T03, T1),
        Tout = three(K02, V02, K1, V1, NewT0, Node, T2),
        RH = no
    ;
        % steal T0's rightmost subtree and combine it with T1
        T0 = three(K00, V00, K01, V01, T00, T01, T02),
        NewT0 = two(K00, V00, T00, T01),
        Node = two(K0, V0, T02, T1),
        Tout = three(K01, V01, K1, V1, NewT0, Node, T2),
        RH = no
    ;
        % move T1 one level down to become the rightmost subtree of T0
        T0 = two(K00, V00, T00, T01),
        NewT0 = three(K00, V00, K0, V0, T00, T01, T1),
        Tout = two(K1, V1, NewT0, T2),
        RH = no
    ;
        T0 = empty,
        error("unbalanced 234 tree")
        % Tout = three(K0, V0, K1, V1, T0, T1, T2),
        % The heights of T0 and T2 are unchanged
        % RH = no
    ).

:- pred fix_3node_t2(K::ia, V::ia, K::ia, V::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia, any_tree234(K, V)::oa,
        bool::out) is det.

fix_3node_t2(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
    (
        % steal T1's rightmost subtree and combine it with T2
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K10, V10, K11, V11, T10, T11, T12),
        Node = two(K1, V1, T13, T2),
        Tout = three(K0, V0, K12, V12, T0, NewT1, Node),
        RH = no
    ;
        % steal T1's rightmost subtree and combine it with T2
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K10, V10, T10, T11),
        Node = two(K1, V1, T12, T2),
        Tout = three(K0, V0, K11, V11, T0, NewT1, Node),
        RH = no
    ;
        % move T2 one level down to become the rightmost subtree of T1
        T1 = two(K10, V10, T10, T11),
        NewT1 = three(K10, V10, K1, V1, T10, T11, T2),
        Tout = two(K0, V0, T0, NewT1),
        RH = no
    ;
        T1 = empty,
        error("unbalanced 234 tree")
        % Tout = three(K0, V0, K1, V1, T0, T1, T2),
        % The heights of T0 and T1 are unchanged
        % RH = no
    ).

:- pred fix_4node_t0(K::ia, V::ia, K::ia, V::ia, K::ia, V::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::oa, bool::out) is det.

fix_4node_t0(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T1's leftmost subtree and combine it with T0
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
        Node = two(K0, V0, T0, T10),
        Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
        RH = no
    ;
        % steal T1's leftmost subtree and combine it with T0
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K11, V11, T11, T12),
        Node = two(K0, V0, T0, T10),
        Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
        RH = no
    ;
        % move T0 one level down to become the leftmost subtree of T1
        T1 = two(K10, V10, T10, T11),
        NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
        Tout = three(K1, V1, K2, V2, NewT1, T2, T3),
        RH = no
    ;
        T1 = empty,
        error("unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T1, T2 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t1(K::ia, V::ia, K::ia, V::ia, K::ia, V::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::oa, bool::out) is det.

fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T2's leftmost subtree and combine it with T1
        T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
        NewT2 = three(K21, V21, K22, V22, T21, T22, T23),
        Node = two(K1, V1, T1, T20),
        Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
        RH = no
    ;
        % steal T2's leftmost subtree and combine it with T1
        T2 = three(K20, V20, K21, V21, T20, T21, T22),
        NewT2 = two(K21, V21, T21, T22),
        Node = two(K1, V1, T1, T20),
        Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
        RH = no
    ;
        % move T1 one level down to become the leftmost subtree of T2
        T2 = two(K20, V20, T20, T21),
        NewT2 = three(K1, V1, K20, V20, T1, T20, T21),
        Tout = three(K0, V0, K2, V2, T0, NewT2, T3),
        RH = no
    ;
        T2 = empty,
        error("unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T0, T2 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t2(K::ia, V::ia, K::ia, V::ia, K::ia, V::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::oa, bool::out) is det.

fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T3's leftmost subtree and combine it with T2
        T3 = four(K30, V30, K31, V31, K32, V32, T30, T31, T32, T33),
        NewT3 = three(K31, V31, K32, V32, T31, T32, T33),
        Node = two(K2, V2, T2, T30),
        Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
        RH = no
    ;
        % steal T3's leftmost subtree and combine it with T2
        T3 = three(K30, V30, K31, V31, T30, T31, T32),
        NewT3 = two(K31, V31, T31, T32),
        Node = two(K2, V2, T2, T30),
        Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
        RH = no
    ;
        % move T2 one level down to become the leftmost subtree of T3
        T3 = two(K30, V30, T30, T31),
        NewT3 = three(K2, V2, K30, V30, T2, T30, T31),
        Tout = three(K0, V0, K1, V1, T0, T1, NewT3),
        RH = no
    ;
        T3 = empty,
        error("unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T0, T1 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t3(K::ia, V::ia, K::ia, V::ia, K::ia, V::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::ia,
        any_tree234(K, V)::oa, bool::out) is det.

fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T2's rightmost subtree and combine it with T3
        T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
        NewT2 = three(K20, V20, K21, V21, T20, T21, T22),
        Node = two(K2, V2, T23, T3),
        Tout = four(K0, V0, K1, V1, K22, V22, T0, T1, NewT2, Node),
        RH = no
    ;
        % steal T2's rightmost subtree and combine it with T3
        T2 = three(K20, V20, K21, V21, T20, T21, T22),
        NewT2 = two(K20, V20, T20, T21),
        Node = two(K2, V2, T22, T3),
        Tout = four(K0, V0, K1, V1, K21, V21, T0, T1, NewT2, Node),
        RH = no
    ;
        % move T3 one level down to become the rightmost subtree of T2
        T2 = two(K20, V20, T20, T21),
        NewT2 = three(K20, V20, K2, V2, T20, T21, T3),
        Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
        RH = no
    ;
        T2 = empty,
        error("unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T0, T1 and T2 are unchanged
        % RH = no
    ).

%---------------------------------------------------------------------------%

keys(Tree, Keys) :-
    keys_2(Tree, [], Keys0),
    % XXX This needs to be written this way in order to avoid errors due to
    % limitations in the current mode analysis and/or intermodule optimization
    % framework.
    unsafe_cast_to_ground(Keys0),
    Keys = Keys0.

:- pred keys_2(any_tree234(K, V)::ia, list(K)::ia,
        list(K)::oa) is det.

keys_2(empty, List, List).
keys_2(two(K0, _V0, T0, T1), L0, L) :-
    keys_2(T1, L0, L1),
    keys_2(T0, [K0 | L1], L).
keys_2(three(K0, _V0, K1, _V1, T0, T1, T2), L0, L) :-
    keys_2(T2, L0, L1),
    keys_2(T1, [K1 | L1], L2),
    keys_2(T0, [K0 | L2], L).
keys_2(four(K0, _V0, K1, _V1, K2, _V2, T0, T1, T2, T3), L0, L) :-
    keys_2(T3, L0, L1),
    keys_2(T2, [K2 | L1], L2),
    keys_2(T1, [K1 | L2], L3),
    keys_2(T0, [K0 | L3], L).

%---------------------------------------------------------------------------%

values(Tree, Values) :-
    values_2(Tree, [], Values).

:- pred values_2(any_tree234(K, V)::ia, list(V)::ia,
        list(V)::oa) is det.

values_2(empty, List, List).
values_2(two(_K0, V0, T0, T1), L0, L) :-
    values_2(T1, L0, L1),
    values_2(T0, [V0 | L1], L).
values_2(three(_K0, V0, _K1, V1, T0, T1, T2), L0, L) :-
    values_2(T2, L0, L1),
    values_2(T1, [V1 | L1], L2),
    values_2(T0, [V0 | L2], L).
values_2(four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), L0, L) :-
    values_2(T3, L0, L1),
    values_2(T2, [V2 | L1], L2),
    values_2(T1, [V1 | L2], L3),
    values_2(T0, [V0 | L3], L).

%---------------------------------------------------------------------------%

any_assoc_list_to_any_tree234(AssocList, Tree) :-
    any_assoc_list_to_any_tree234_2(AssocList, empty, Tree).

:- pred any_assoc_list_to_any_tree234_2(any_assoc_list(K, V)::ia,
        any_tree234(K, V)::ia, any_tree234(K, V)::oa) is det.

any_assoc_list_to_any_tree234_2([], Tree, Tree).
any_assoc_list_to_any_tree234_2([K - V | Rest], Tree0, Tree) :-
    unsafe_cast_to_ground(K),
    set(K, V, Tree0, Tree1),
    any_assoc_list_to_any_tree234_2(Rest, Tree1, Tree).

%---------------------------------------------------------------------------%

any_tree234_to_any_assoc_list(Tree, AssocList) :-
    any_tree234_to_any_assoc_list_2(Tree, [], AssocList).

:- pred any_tree234_to_any_assoc_list_2(any_tree234(K, V)::ia,
        any_assoc_list(K, V)::ia, any_assoc_list(K, V)::oa) is det.

any_tree234_to_any_assoc_list_2(empty, List, List).
any_tree234_to_any_assoc_list_2(two(K0, V0, T0, T1), L0, L) :-
    any_tree234_to_any_assoc_list_2(T1, L0, L1),
    any_tree234_to_any_assoc_list_2(T0, [K0 - V0 | L1], L).
any_tree234_to_any_assoc_list_2(three(K0, V0, K1, V1, T0, T1, T2),
        L0, L) :-
    any_tree234_to_any_assoc_list_2(T2, L0, L1),
    any_tree234_to_any_assoc_list_2(T1, [K1 - V1 | L1], L2),
    any_tree234_to_any_assoc_list_2(T0, [K0 - V0 | L2], L).
any_tree234_to_any_assoc_list_2(four(K0, V0, K1, V1, K2, V2,
        T0, T1, T2, T3), L0, L) :-
    any_tree234_to_any_assoc_list_2(T3, L0, L1),
    any_tree234_to_any_assoc_list_2(T2, [K2 - V2 | L1], L2),
    any_tree234_to_any_assoc_list_2(T1, [K1 - V1 | L2], L3),
    any_tree234_to_any_assoc_list_2(T0, [K0 - V0 | L3], L).

%---------------------------------------------------------------------------%

foldl(_Pred, empty, !A).
foldl(Pred, two(K, V, T0, T1), !A) :-
    unsafe_cast_to_ground(K),
    foldl(Pred, T0, !A),
    call(Pred, K, V, !A),
    foldl(Pred, T1, !A).
foldl(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A) :-
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    foldl(Pred, T0, !A),
    call(Pred, K0, V0, !A),
    foldl(Pred, T1, !A),
    call(Pred, K1, V1, !A),
    foldl(Pred, T2, !A).
foldl(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), !A) :-
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    unsafe_cast_to_ground(K2),
    foldl(Pred, T0, !A),
    call(Pred, K0, V0, !A),
    foldl(Pred, T1, !A),
    call(Pred, K1, V1, !A),
    foldl(Pred, T2, !A),
    call(Pred, K2, V2, !A),
    foldl(Pred, T3, !A).

foldl2(_Pred, empty, !A, !B).
foldl2(Pred, two(K, V, T0, T1), !A, !B) :-
    unsafe_cast_to_ground(K),
    foldl2(Pred, T0, !A, !B),
    call(Pred, K, V, !A, !B),
    foldl2(Pred, T1, !A, !B).
foldl2(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B) :-
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    foldl2(Pred, T0, !A, !B),
    call(Pred, K0, V0, !A, !B),
    foldl2(Pred, T1, !A, !B),
    call(Pred, K1, V1, !A, !B),
    foldl2(Pred, T2, !A, !B).
foldl2(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B) :-
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    unsafe_cast_to_ground(K2),
    foldl2(Pred, T0, !A, !B),
    call(Pred, K0, V0, !A, !B),
    foldl2(Pred, T1, !A, !B),
    call(Pred, K1, V1, !A, !B),
    foldl2(Pred, T2, !A, !B),
    call(Pred, K2, V2, !A, !B),
    foldl2(Pred, T3, !A, !B).

foldl3(_Pred, empty, !A, !B, !C).
foldl3(Pred, two(K, V, T0, T1), !A, !B, !C) :-
    unsafe_cast_to_ground(K),
    foldl3(Pred, T0, !A, !B, !C),
    call(Pred, K, V, !A, !B, !C),
    foldl3(Pred, T1, !A, !B, !C).
foldl3(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C) :-
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    foldl3(Pred, T0, !A, !B, !C),
    call(Pred, K0, V0, !A, !B, !C),
    foldl3(Pred, T1, !A, !B, !C),
    call(Pred, K1, V1, !A, !B, !C),
    foldl3(Pred, T2, !A, !B, !C).
foldl3(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C) :-
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    unsafe_cast_to_ground(K2),
    foldl3(Pred, T0, !A, !B, !C),
    call(Pred, K0, V0, !A, !B, !C),
    foldl3(Pred, T1, !A, !B, !C),
    call(Pred, K1, V1, !A, !B, !C),
    foldl3(Pred, T2, !A, !B, !C),
    call(Pred, K2, V2, !A, !B, !C),
    foldl3(Pred, T3, !A, !B, !C).

%---------------------------------------------------------------------------%

map_values(_Pred, empty, empty).
map_values(Pred, Tree0, Tree) :-
    Tree0 = two(K0, V0, Left0, Right0),
    unsafe_cast_to_ground(K0),
    call(Pred, K0, V0, W0),
    map_values(Pred, Left0, Left),
    map_values(Pred, Right0, Right),
    Tree  = two(K0, W0, Left, Right).
map_values(Pred, Tree0, Tree) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    call(Pred, K0, V0, W0),
    call(Pred, K1, V1, W1),
    map_values(Pred, Left0, Left),
    map_values(Pred, Middle0, Middle),
    map_values(Pred, Right0, Right),
    Tree  = three(K0, W0, K1, W1, Left, Middle, Right).
map_values(Pred, Tree0, Tree) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    unsafe_cast_to_ground(K2),
    call(Pred, K0, V0, W0),
    call(Pred, K1, V1, W1),
    call(Pred, K2, V2, W2),
    map_values(Pred, Left0, Left),
    map_values(Pred, LMid0, LMid),
    map_values(Pred, RMid0, RMid),
    map_values(Pred, Right0, Right),
    Tree  = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

%---------------------------------------------------------------------------%

map_foldl(_Pred, empty, empty, !A).
map_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = two(K0, V0, Left0, Right0),
    Tree  = two(K0, W0, Left, Right),
    unsafe_cast_to_ground(K0),
    map_foldl(Pred, Left0, Left, !A),
    call(Pred, K0, V0, W0, !A),
    map_foldl(Pred, Right0, Right, !A).
map_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    Tree  = three(K0, W0, K1, W1, Left, Middle, Right),
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    map_foldl(Pred, Left0, Left, !A),
    call(Pred, K0, V0, W0, !A),
    map_foldl(Pred, Middle0, Middle, !A),
    call(Pred, K1, V1, W1, !A),
    map_foldl(Pred, Right0, Right, !A).
map_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    Tree  = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right),
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    unsafe_cast_to_ground(K2),
    map_foldl(Pred, Left0, Left, !A),
    call(Pred, K0, V0, W0, !A),
    map_foldl(Pred, LMid0, LMid, !A),
    call(Pred, K1, V1, W1, !A),
    map_foldl(Pred, RMid0, RMid, !A),
    call(Pred, K2, V2, W2, !A),
    map_foldl(Pred, Right0, Right, !A).

map_foldl2(_Pred, empty, empty, !A, !B).
map_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = two(K0, V0, Left0, Right0),
    Tree  = two(K0, W0, Left, Right),
    unsafe_cast_to_ground(K0),
    map_foldl2(Pred, Left0, Left, !A, !B),
    call(Pred, K0, V0, W0, !A, !B),
    map_foldl2(Pred, Right0, Right, !A, !B).
map_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    Tree  = three(K0, W0, K1, W1, Left, Middle, Right),
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    map_foldl2(Pred, Left0, Left, !A, !B),
    call(Pred, K0, V0, W0, !A, !B),
    map_foldl2(Pred, Middle0, Middle, !A, !B),
    call(Pred, K1, V1, W1, !A, !B),
    map_foldl2(Pred, Right0, Right, !A, !B).
map_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    Tree  = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right),
    unsafe_cast_to_ground(K0),
    unsafe_cast_to_ground(K1),
    unsafe_cast_to_ground(K2),
    map_foldl2(Pred, Left0, Left, !A, !B),
    call(Pred, K0, V0, W0, !A, !B),
    map_foldl2(Pred, LMid0, LMid, !A, !B),
    call(Pred, K1, V1, W1, !A, !B),
    map_foldl2(Pred, RMid0, RMid, !A, !B),
    call(Pred, K2, V2, W2, !A, !B),
    map_foldl2(Pred, Right0, Right, !A, !B).

%---------------------------------------------------------------------------%

    % count the number of elements ia a tree
count(empty, 0).
count(two(_, _, T0, T1), N) :-
    count(T0, N0),
    count(T1, N1),
    N = 1 + N0 + N1.
count(three(_, _, _, _, T0, T1, T2), N) :-
    count(T0, N0),
    count(T1, N1),
    count(T2, N2),
    N = 2 + N0 + N1 + N2.
count(four(_, _, _, _, _, _, T0, T1, T2, T3), N) :-
    count(T0, N0),
    count(T1, N1),
    count(T2, N2),
    count(T3, N3),
    N = 3 + N0 + N1 + N2 + N3.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%       Function forms added.

init = T :-
    init(T).

lookup(T, K) = V :-
    lookup(T, K, V).

set(T1, K, V) = T2 :-
    set(K, V, T1, T2).

delete(T1, K) = T2 :-
    delete(K, T1, T2).

keys(T) = Ks :-
    keys(T, Ks).

values(T) = Vs :-
    values(T, Vs).

count(T) = N :-
    count(T, N).

any_assoc_list_to_any_tree234(AL) = T :-
    any_assoc_list_to_any_tree234(AL, T).

any_tree234_to_any_assoc_list(T) = AL :-
    any_tree234_to_any_assoc_list(T, AL).

:- pragma promise_equivalent_clauses(foldl/3).

foldl(F::in(func(in, ia, in) = out is det), M::ia, A::in) =
        (B::out) :-
    P = ( pred(W::in, X::ia, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    foldl(P, M, A, B).
foldl(F::in(func(in, ia, ia) = oa is det), M::ia, A::ia) =
        (B::oa) :-
    P = ( pred(W::in, X::ia, Y::ia, Z::oa) is det :- Z = F(W, X, Y) ),
    foldl(P, M, A, B).

map_values(F, T1) = T2 :-
    P = ( pred(X::in, Y::ia, Z::oa) is det :- Z = F(X, Y) ),
    any_tree234.map_values(P, T1, T2).
