%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997,1999-2000,2002, 2004, 2006 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% tree234_cc implements a map (dictionary) using 2-3-4 trees. This is
% a cut down version of the standard library module library/tree234.m,
% with the additional change that it uses compare_representation instead
% of the builtin unification and comparison predicates. It is thus able
% to work with keys that contain non-canonical terms, such as higher order
% terms.
%
% The drawback of using compare_representation is that sometimes entries
% that have been inserted in the map will not later be found when looked up.
% This can happen when the lookup uses a different (but equivalent)
% representation of the key than the insertion. However, for some
% applications (for example, the declarative debugging oracle) this
% behaviour may be acceptable.
%
% A flow on effect is that most of the det predicates are now cc_multi,
% since this is the determinism of compare_representation. Even predicates
% that used to be semidet are now cc_multi, since they need to be called
% just after calls to other committed choice procedures which means that
% they are not allowed to fail. They return all outputs in a maybe type,
% which indicates success or failure.
%
% See library/map.m for documentation of the predicates.
%
%---------------------------------------------------------------------------%

:- module mdb.tree234_cc.

:- interface.

:- import_module maybe.

:- type tree234_cc(K, V).

:- pred init(tree234_cc(K, V)::uo) is det.

:- pred is_empty(tree234_cc(K, V)::in) is semidet.

:- pred search(tree234_cc(K, V)::in, K::in, maybe(V)::out) is cc_multi.

:- pred set(tree234_cc(K, V)::in, K::in, V::in, tree234_cc(K, V)::out)
    is cc_multi.

:- pred delete(tree234_cc(K, V)::in, K::in, tree234_cc(K, V)::out) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module require.

:- type tree234_cc(K, V)
    --->    empty
    ;       two(K, V, tree234_cc(K, V), tree234_cc(K, V))
    ;       three(K, V, K, V, tree234_cc(K, V), tree234_cc(K, V),
                tree234_cc(K, V))
    ;       four(K, V, K, V, K, V, tree234_cc(K, V), tree234_cc(K, V),
                tree234_cc(K, V), tree234_cc(K, V)).

%---------------------------------------------------------------------------%

init(empty).

is_empty(Tree) :-
    Tree = empty.

%---------------------------------------------------------------------------%

search(T, K, MaybeV) :-
    (
        T = empty,
        MaybeV = no
    ;
        T = two(K0, V0, T0, T1),
        compare_representation(Result, K, K0),
        (
            Result = (<),
            search(T0, K, MaybeV)
        ;
            Result = (=),
            MaybeV = yes(V0)
        ;
            Result = (>),
            search(T1, K, MaybeV)
        )
    ;
        T = three(K0, V0, K1, V1, T0, T1, T2),
        compare_representation(Result0, K, K0),
        (
            Result0 = (<),
            search(T0, K, MaybeV)
        ;
            Result0 = (=),
            MaybeV = yes(V0)
        ;
            Result0 = (>),
            compare_representation(Result1, K, K1),
            (
                Result1 = (<),
                search(T1, K, MaybeV)
            ;
                Result1 = (=),
                MaybeV = yes(V1)
            ;
                Result1 = (>),
                search(T2, K, MaybeV)
            )
        )
    ;
        T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare_representation(Result1, K, K1),
        (
            Result1 = (<),
            compare_representation(Result0, K, K0),
            (
                Result0 = (<),
                search(T0, K, MaybeV)
            ;
                Result0 = (=),
                MaybeV = yes(V0)
            ;
                Result0 = (>),
                search(T1, K, MaybeV)
            )
        ;
            Result1 = (=),
            MaybeV = yes(V1)
        ;
            Result1 = (>),
            compare_representation(Result2, K, K2),
            (
                Result2 = (<),
                search(T2, K, MaybeV)
            ;
                Result2 = (=),
                MaybeV = yes(V2)
            ;
                Result2 = (>),
                search(T3, K, MaybeV)
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- inst two(K, V, T) for tree234_cc/2
    --->    two(K, V, T, T).
:- inst three(K, V, T) for tree234_cc/2
    --->    three(K, V, K, V, T, T, T).
:- inst four(K, V, T) for tree234_cc/2
    --->    four(K, V, K, V, K, V, T, T, T, T).

:- mode out_two  == out(two(ground, ground, ground)).
:- mode in_two   == in(two(ground, ground, ground)).
:- mode in_three == in(three(ground, ground, ground)).
:- mode in_four  == in(four(ground, ground, ground)).

%---------------------------------------------------------------------------%

:- pred split_four(tree234_cc(K, V)::in_four, K::out, V::out,
    tree234_cc(K, V)::out_two, tree234_cc(K, V)::out_two) is det.

split_four(Tin, MidK, MidV, Sub0, Sub1) :-
    Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
    Sub0 = two(K0, V0, T0, T1),
    MidK = K1,
    MidV = V1,
    Sub1 = two(K2, V2, T2, T3).

%---------------------------------------------------------------------------%

% set is implemented using the simple top-down approach
% described in eg Sedgewick which splits 4 nodes into two 2 nodes on the
% downward traversal of the tree as we search for the right place to
% insert the new key-value pair. We know we have the right place if the
% subtrees of the node are empty (in which case we expand the node - which
% will always work because we already split 4 nodes into 2 nodes), or if
% the tree itself is empty. This algorithm is O(lgN).

set(Tin, K, V, Tout) :-
    (
        Tin = empty,
        Tout = two(K, V, empty, empty)
    ;
        Tin = two(_, _, _, _),
        set2(Tin, K, V, Tout)
    ;
        Tin = three(_, _, _, _, _, _, _),
        set3(Tin, K, V, Tout)
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare_representation(Result1, K, K1),
        (
            Result1 = (<),
            Sub0 = two(K0, V0, T0, T1),
            Sub1 = two(K2, V2, T2, T3),
            set2(Sub0, K, V, NewSub0),
            Tout = two(K1, V1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            Sub0 = two(K0, V0, T0, T1),
            Sub1 = two(K2, V2, T2, T3),
            set2(Sub1, K, V, NewSub1),
            Tout = two(K1, V1, Sub0, NewSub1)
        )
    ).

:- pred set2(tree234_cc(K, V)::in_two, K::in, V::in, tree234_cc(K, V)::out)
    is cc_multi.

set2(two(K0, V0, T0, T1), K, V, Tout) :-
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
    then
        compare_representation(Result, K, K0),
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
        compare_representation(Result, K, K0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                split_four(T0, MT0K, MT0V, T00, T01),
                compare_representation(Result1, K, MT0K),
                (
                    Result1 = (<),
                    set2(T00, K, V, NewT00),
                    Tout = three(MT0K, MT0V, K0, V0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    Tout = three(MT0K, V, K0, V0, T00, T01, T1)
                ;
                    Result1 = (>),
                    set2(T01, K, V, NewT01),
                    Tout = three(MT0K, MT0V, K0, V0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _, _, _),
                set3(T0, K, V, NewT0),
                Tout = two(K0, V0, NewT0, T1)
            ;
                T0 = two(_, _, _, _),
                set2(T0, K, V, NewT0),
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
                compare_representation(Result1, K, MT1K),
                (
                    Result1 = (<),
                    set2(T10, K, V, NewT10),
                    Tout = three(K0, V0, MT1K, MT1V, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    Tout = three(K0, V0, MT1K, V, T0, T10, T11)
                ;
                    Result1 = (>),
                    set2(T11, K, V, NewT11),
                    Tout = three(K0, V0, MT1K, MT1V, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _, _, _),
                set3(T1, K, V, NewT1),
                Tout = two(K0, V0, T0, NewT1)
            ;
                T1 = two(_, _, _, _),
                set2(T1, K, V, NewT1),
                Tout = two(K0, V0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(K, V, empty, empty),
                Tout = two(K0, V0, T0, NewT1)
            )
        )
    ).

:- pred set3(tree234_cc(K, V), K, V, tree234_cc(K, V)).
:- mode set3(in_three, in, in, out) is cc_multi.

set3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    then
        compare_representation(Result0, K, K0),
        (
            Result0 = (<),
            Tout = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            Tout = three(K0, V, K1, V1, empty, empty, empty)
        ;
            Result0 = (>),
            compare_representation(Result1, K, K1),
            (
                Result1 = (<),
                Tout = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                Tout = three(K0, V0, K1, V, empty, empty, empty)
            ;
                Result1 = (>),
                Tout = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty)
            )
        )
    else
        compare_representation(Result0, K, K0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                split_four(T0, MT0K, MT0V, T00, T01),
                compare_representation(ResultM, K, MT0K),
                (
                    ResultM = (<),
                    set2(T00, K, V, NewT00),
                    Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                        NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    Tout = four(MT0K, V, K0, V0, K1, V1,
                        T00, T01, T1, T2)
                ;
                    ResultM = (>),
                    set2(T01, K, V, NewT01),
                    Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                        T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _, _, _),
                set3(T0, K, V, NewT0),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _, _),
                set2(T0, K, V, NewT0),
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
            compare_representation(Result1, K, K1),
            (
                Result1 = (<),
                (
                    T1 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T1, MT1K, MT1V, T10, T11),
                    compare_representation(ResultM, K, MT1K),
                    (
                        ResultM = (<),
                        set2(T10, K, V, NewT10),
                        Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                            T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        Tout = four(K0, V0, MT1K, V, K1, V1,
                            T0, T10, T11, T2)
                    ;
                        ResultM = (>),
                        set2(T11, K, V, NewT11),
                        Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                            T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _, _, _),
                    set3(T1, K, V, NewT1),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _, _),
                    set2(T1, K, V, NewT1),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                ;
                    T1 = empty,
                    NewT1 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                Tout = three(K0, V0, K, V, T0, T1, T2)
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _, _, _, _),
                    split_four(T2, MT2K, MT2V, T20, T21),
                    compare_representation(ResultM, K, MT2K),
                    (
                        ResultM = (<),
                        set2(T20, K, V, NewT20),
                        Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                            T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        Tout = four(K0, V0, K1, V1, MT2K, V,
                            T0, T1, T20, T21)
                    ;
                        ResultM = (>),
                        set2(T21, K, V, NewT21),
                        Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                            T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _, _, _),
                    set3(T2, K, V, NewT2),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _, _),
                    set2(T2, K, V, NewT2),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

delete(Tin, K, Tout) :-
    delete_2(Tin, K, Tout, _).

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.

:- pred delete_2(tree234_cc(K, V)::in, K::in, tree234_cc(K, V)::out, bool::out)
    is cc_multi.

delete_2(Tin, K, Tout, RH) :-
    (
        Tin = empty,
        Tout = empty,
        RH = no
    ;
        Tin = two(K0, V0, T0, T1),
        compare_representation(Result0, K, K0),
        (
            Result0 = (<),
            delete_2(T0, K, NewT0, RHT0),
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
            remove_smallest(T1, Removed),
            (
                Removed = yes({ST1K, ST1V, NewT1, RHT1}),
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1K, ST1V, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1K, ST1V, T0, NewT1),
                    RH = no
                )
            ;
                Removed = no,
                % T1 must be empty
                Tout = T0,
                RH = yes
            )
        ;
            Result0 = (>),
            delete_2(T1, K, NewT1, RHT1),
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
        compare_representation(Result0, K, K0),
        (
            Result0 = (<),
            delete_2(T0, K, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                RH = no
            )
        ;
            Result0 = (=),
            remove_smallest(T1, Removed),
            (
                Removed = yes({ST1K, ST1V, NewT1, RHT1}),
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1K, ST1V, K1, V1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1K, ST1V, K1, V1, T0, NewT1, T2),
                    RH = no
                )
            ;
                Removed = no,
                % T1 must be empty
                Tout = two(K1, V1, T0, T2),
                RH = no
            )
        ;
            Result0 = (>),
            compare_representation(Result1, K, K1),
            (
                Result1 = (<),
                delete_2(T1, K, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_3node_t1(K0, V0, K1, V1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2),
                    RH = no
                )
            ;
                Result1 = (=),
                remove_smallest(T2, Removed),
                (
                    Removed = yes({ST2K, ST2V, NewT2, RHT2}),
                    (
                        RHT2 = yes,
                        fix_3node_t2(K0, V0, ST2K, ST2V,
                            T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(K0, V0, ST2K, ST2V, T0, T1, NewT2),
                        RH = no
                    )
                ;
                    Removed = no,
                    % T2 must be empty
                    Tout = two(K0, V0, T0, T1),
                    RH = no
                )
            ;
                Result1 = (>),
                delete_2(T2, K, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_3node_t2(K0, V0, K1, V1, T0, T1, NewT2, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
                    RH = no
                )
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare_representation(Result1, K, K1),
        (
            Result1 = (<),
            compare_representation(Result0, K, K0),
            (
                Result0 = (<),
                delete_2(T0, K, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_4node_t0(K0, V0, K1, V1, K2, V2,
                        NewT0, T1, T2, T3, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
                    RH = no
                )
            ;
                Result0 = (=),
                remove_smallest(T1, Removed),
                (
                    Removed = yes({ST1K, ST1V, NewT1, RHT1}),
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1K, ST1V, K1, V1, K2, V2,
                            T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1K, ST1V, K1, V1, K2, V2,
                            T0, NewT1, T2, T3),
                        RH = no
                    )
                ;
                    Removed = no,
                    % T1 must be empty
                    Tout = three(K1, V1, K2, V2, T0, T2, T3),
                    RH = no
                )
            ;
                Result0 = (>),
                delete_2(T1, K, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_4node_t1(K0, V0, K1, V1, K2, V2,
                        T0, NewT1, T2, T3, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3),
                    RH = no
                )
            )
        ;
            Result1 = (=),
            remove_smallest(T2, Removed),
            (
                Removed = yes({ST2K, ST2V, NewT2, RHT2}),
                (
                    RHT2 = yes,
                    fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
                        T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(K0, V0, ST2K, ST2V, K2, V2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                Removed = no,
                % T2 must be empty
                Tout = three(K0, V0, K2, V2, T0, T1, T3),
                RH = no
            )
        ;
            Result1 = (>),
            compare_representation(Result2, K, K2),
            (
                Result2 = (<),
                delete_2(T2, K, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_4node_t2(K0, V0, K1, V1, K2, V2,
                        T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                Result2 = (=),
                remove_smallest(T3, Removed),
                (
                    Removed = yes({ST3K, ST3V, NewT3,
                            RHT3}),
                    (
                        RHT3 = yes,
                        fix_4node_t3(K0, V0, K1, V1, ST3K, ST3V,
                            T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(K0, V0, K1, V1, ST3K, ST3V,
                            T0, T1, T2, NewT3),
                        RH = no
                    )
                ;
                    Removed = no,
                    % T3 must be empty
                    Tout = three(K0, V0, K1, V1, T0, T1, T2),
                    RH = no
                )
            ;
                Result2 = (>),
                delete_2(T3, K, NewT3, RHT3),
                (
                    RHT3 = yes,
                    fix_4node_t3(K0, V0, K1, V1, K2, V2,
                        T0, T1, T2, NewT3, Tout, RH)
                ;
                    RHT3 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3),
                    RH = no
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

    % The algorithm we use similar to delete, except that we
    % always go down the left subtree.

:- pred remove_smallest(tree234_cc(K, V)::in,
    maybe({K, V, tree234_cc(K, V), bool})::out) is cc_multi.

remove_smallest(Tin, Result) :-
    (
        Tin = empty,
        Result = no
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
            remove_smallest(T0, Removed),
            (
                Removed = no,
                error("remove_smallest: failure two")
            ;
                Removed = yes({K, V, NewT0, RHT0})
            ),
            (
                RHT0 = yes,
                fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(K0, V0, NewT0, T1),
                RH = no
            )
        ),
        Result = yes({K, V, Tout, RH})
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
            remove_smallest(T0, ThreeResult),
            (
                ThreeResult = no,
                error("remove_smallest: failure three")
            ;
                ThreeResult = yes({K, V, NewT0, RHT0})
            ),
            (
                RHT0 = yes,
                fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                RH = no
            )
        ),
        Result = yes({K, V, Tout, RH})
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
            remove_smallest(T0, FourResult),
            (
                FourResult = no,
                error("remove_smallest: failure four")
            ;
                FourResult = yes({K, V, NewT0, RHT0})
            ),
            (
                RHT0 = yes,
                fix_4node_t0(K0, V0, K1, V1, K2, V2,
                    NewT0, T1, T2, T3, Tout, RH)
            ;
                RHT0 = no,
                Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
                RH = no
            )
        ),
        Result = yes({K, V, Tout, RH})
    ).

%---------------------------------------------------------------------------%

    % The input to the following group of predicates are the components
    % of a two-, three- or four-node in which the height of the indicated
    % subtree is one less that it should be. If it is possible to increase
    % the height of that subtree by moving into it elements from its
    % neighboring subtrees, do so, and return the resulting tree with RH
    % set to no. Otherwise, return a balanced tree whose height is reduced
    % by one, with RH set to yes to indicate the reduced height.

:- pred fix_2node_t0(K::in, V::in, tree234_cc(K, V)::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::out, bool::out) is det.

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

:- pred fix_2node_t1(K::in, V::in, tree234_cc(K, V)::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::out, bool::out) is det.

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

:- pred fix_3node_t0(K::in, V::in, K::in, V::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::out,
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

:- pred fix_3node_t1(K::in, V::in, K::in, V::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::out,
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

:- pred fix_3node_t2(K::in, V::in, K::in, V::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::out,
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

:- pred fix_4node_t0(K::in, V::in, K::in, V::in, K::in, V::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::out, bool::out) is det.

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

:- pred fix_4node_t1(K::in, V::in, K::in, V::in, K::in, V::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::out, bool::out) is det.

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

:- pred fix_4node_t2(K::in, V::in, K::in, V::in, K::in, V::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::out, bool::out) is det.

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

:- pred fix_4node_t3(K::in, V::in, K::in, V::in, K::in, V::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, tree234_cc(K, V)::in,
    tree234_cc(K, V)::in, tree234_cc(K, V)::in, bool::in) is det.

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
