%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% Copyright (C) 2012-2014 YesLogic Pty. Ltd.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: diet.m.
% Author: wangp.
% Stability: medium.
%
% Discrete Interval Encoding Trees are a highly efficient set implementation
% for fat sets, i.e. densely populated sets over a discrete linear order.
%
% M. Erwig: Diets for Fat Sets
% Journal of Functional Programming, Vol. 8, No. 6, pp. 627-632
%
% O. Friedmann, M. Lange: More on Balanced Diets
% Journal of Functional Programming, volume 21, issue 02, pp. 135-157
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module diet.
:- interface.

:- import_module bool.
:- import_module enum.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type diet(T). % <= enum(T).

    % Return an empty set.
    %
:- func init = diet(T).
:- pred init(diet(T)::out) is det.

:- pred empty(diet(T)).
:- mode empty(in) is semidet.
:- mode empty(out) is det.

:- pred is_empty(diet(T)::in) is semidet.

:- pred is_non_empty(diet(T)::in) is semidet.

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB' contain the same
    % elements.
    %
:- pred equal(diet(T)::in, diet(T)::in) is semidet <= enum(T).

    % `make_singleton_set(Elem)' returns a set containing just the single
    % element `Elem'.
    %
:- func make_singleton_set(T) = diet(T) <= enum(T).

    % `make_interval_set(X, Y)' returns a set containing just the elements in
    % the interval [X, Y]. Throws an exception if Y < X.
    %
:- func make_interval_set(T, T) = diet(T) <= enum(T).

    % `is_singleton(Set, X)' is true iff `Set' is a singleton containing the
    % element `X'.
    %
:- pred is_singleton(diet(T)::in, T::out) is semidet <= enum(T).

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pred contains(diet(T)::in, T::in) is semidet <= enum(T).

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred member(T, diet(T)) <= enum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `subset(Subset, Set)' is true iff `Subset' is a subset of `Set'.
    %
:- pred subset(diet(T)::in, diet(T)::in) is semidet.

    % `superset(Superset, Set)' is true iff `Superset' is a superset of `Set'.
    %
:- pred superset(diet(T)::in, diet(T)::in) is semidet.

    % `insert(X, Set0, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only `X'.
    %
:- func insert(diet(T), T) = diet(T) <= enum(T).
:- pred insert(T::in, diet(T)::in, diet(T)::out) is det <= enum(T).

    % `insert_new(X, Set0, Set)' is true iff `Set0' does not contain
    % `X', and `Set' is the union of `Set0' and the set containing only `X'.
    %
:- pred insert_new(T::in, diet(T)::in, diet(T)::out) is semidet <= enum(T).

    % `insert_list(Xs, Set0, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only the members of `Xs'.
    %
:- func insert_list(diet(T), list(T)) = diet(T) <= enum(T).
:- pred insert_list(list(T)::in, diet(T)::in, diet(T)::out) is det <= enum(T).

    % `insert_interval(X, Y, Set0, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only the elements of the interval [X, Y].
    % Throws an exception if Y < X.
    %
:- pred insert_interval(T::in, T::in, diet(T)::in, diet(T)::out) is det
    <= enum(T).

    % `delete(X, Set0, Set)' is true iff `Set' is the relative
    % complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- func delete(diet(T), T) = diet(T) <= enum(T).
:- pred delete(T::in, diet(T)::in, diet(T)::out) is det <= enum(T).

    % `delete_list(Set, X)' returns the difference of `Set' and the set
    % containing only the members of `X'. Same as
    % `difference(Set, list_to_set(X))', but may be more efficient.
    %
:- func delete_list(diet(T), list(T)) = diet(T) <= enum(T).
:- pred delete_list(list(T)::in, diet(T)::in, diet(T)::out) is det <= enum(T).

    % `remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
:- pred remove(T::in, diet(T)::in, diet(T)::out) is semidet <= enum(T).

    % `remove_list(X, Set0, Set)' returns in `Set' the difference of `Set0'
    % and the set containing all the elements of `X', failing if any element
    % of `X' is not in `Set0'. Same as `subset(list_to_set(X), Set0),
    % difference(Set0, list_to_set(X), Set)', but may be more efficient.
    %
:- pred remove_list(list(T)::in, diet(T)::in, diet(T)::out) is semidet
    <= enum(T).

    % `remove_least(X, Set0, Set)' is true iff `X' is the least element in
    % `Set0', and `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred remove_least(T::out, diet(T)::in, diet(T)::out) is semidet <= enum(T).

    % `split(X, Set, Lesser, IsPresent, Greater)' is true iff `Lesser' is the
    % set of elements in `Set' which are less than `X' and `Greater' is the set
    % of elements in `Set' which are greater than `X'.
    % `IsPresent' is `yes' if `Set' contains `X', and `no' otherwise.
    %
:- pred split(T::in, diet(T)::in, diet(T)::out, bool::out, diet(T)::out) is det
    <= enum(T).

    % `union(SetA, SetB, Set)' is true iff `Set' is the union of
    % `SetA' and `SetB'.
    %
:- func union(diet(T), diet(T)) = diet(T).
:- pred union(diet(T)::in, diet(T)::in, diet(T)::out) is det.

    % `union_list(Sets, Set)' returns the union of all the sets in Sets.
    %
:- func union_list(list(diet(T))) = diet(T).
:- pred union_list(list(diet(T))::in, diet(T)::out) is det.

    % `intersect(SetA, SetB, Set)' is true iff `Set' is the
    % intersection of `SetA' and `SetB'.
    %
:- func intersect(diet(T), diet(T)) = diet(T).
:- pred intersect(diet(T)::in, diet(T)::in, diet(T)::out) is det.

    % `intersect_list(Sets, Set)' returns the intersection of all the sets
    % in Sets.
    %
:- func intersect_list(list(diet(T))) = diet(T).
:- pred intersect_list(list(diet(T))::in, diet(T)::out) is det.

    % `difference(SetA, SetB)' returns the set containing all the elements
    % of `SetA' except those that occur in `SetB'.
    %
:- func difference(diet(T), diet(T)) = diet(T).
:- pred difference(diet(T)::in, diet(T)::in, diet(T)::out) is det.

    % divide(Pred, Set, InPart, OutPart):
    % InPart consists of those elements of Set for which Pred succeeds;
    % OutPart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet), diet(T)::in,
    diet(T)::out, diet(T)::out) is det <= enum(T).

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in DivideBySet;
    % OutPart consists of those elements of Set which are not in DivideBySet.
    %
:- pred divide_by_set(diet(T)::in, diet(T)::in, diet(T)::out, diet(T)::out)
    is det <= enum(T).

    % `count(Set)' returns the number of elements in Set.
    %
:- func count(diet(T)) = int <= enum(T).

    % `foldl_intervals(Pred, Set, Start)' calls Pred with each interval of
    % `Set' (in sorted order) and an accumulator (with the initial value of
    % `Start'), and returns the final value.
    %
:- pred foldl_intervals(pred(T, T, A, A), diet(T), A, A) <= enum(T).
:- mode foldl_intervals(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl_intervals(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl_intervals(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.

    % `foldr_intervals(Pred, Set, Start)' calls Pred with each interval of
    % `Set' (in reverse sorted order) and an accumulator (with the initial
    % value of `Start'), and returns the final value.
    %
:- pred foldr_intervals(pred(T, T, A, A), diet(T), A, A) <= enum(T).
:- mode foldr_intervals(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldr_intervals(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldr_intervals(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.

    % `foldl(Func, Set, Start)' calls Func with each element of `Set'
    % (in sorted order) and an accumulator (with the initial value of `Start'),
    % and returns the final value.
    %
:- func foldl(func(T, A) = A, diet(T), A) = A <= enum(T).

:- pred foldl(pred(T, A, A), diet(T), A, A) <= enum(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- pred foldl2(pred(T, A, A, B, B), diet(T), A, A, B, B) <= enum(T).
:- mode foldl2(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode foldl2(pred(in, in, out, mdi, muo) is det, in,
    in, out, mdi, muo) is det.
:- mode foldl2(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

:- pred foldl3(pred(T, A, A, B, B, C, C), diet(T),
    A, A, B, B, C, C) <= enum(T).
:- mode foldl3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.

:- pred foldl4(pred(T, A, A, B, B, C, C, D, D), diet(T),
    A, A, B, B, C, C, D, D) <= enum(T).
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl5(pred(T, A, A, B, B, C, C, D, D, E, E), diet(T),
    A, A, B, B, C, C, D, D, E, E) <= enum(T).
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- func foldr(func(T, A) = A, diet(T), A) = A <= enum(T).

:- pred foldr(pred(T, A, A), diet(T), A, A) <= enum(T).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), diet(T)::in) is semidet
    <= enum(T).

    % `filter(Pred, Set) = TrueSet' returns the elements of Set for which
    % Pred succeeds.
    %
:- func filter(pred(T), diet(T)) = diet(T) <= enum(T).
:- mode filter(pred(in) is semidet, in) = out is det.

    % `filter(Pred, Set, TrueSet, FalseSet)' returns the elements of Set
    % for which Pred succeeds, and those for which it fails.
    %
:- pred filter(pred(T), diet(T), diet(T), diet(T)) <= enum(T).
:- mode filter(pred(in) is semidet, in, out, out) is det.

    % `list_to_set(List)' returns a set containing only the members of `List'.
    %
:- func list_to_set(list(T)) = diet(T) <= enum(T).
:- pred list_to_set(list(T)::in, diet(T)::out) is det <= enum(T).

:- func from_list(list(T)) = diet(T) <= enum(T).
:- pred from_list(list(T)::in, diet(T)::out) is det <= enum(T).

    % `sorted_list_to_set(List)' returns a set containing only the members
    % of `List'. `List' must be sorted.
    %
:- func sorted_list_to_set(list(T)) = diet(T) <= enum(T).
:- pred sorted_list_to_set(list(T)::in, diet(T)::out) is det <= enum(T).

    % `to_sorted_list(Set)' returns a list containing all the members of `Set',
    % in sorted order.
    %
:- func to_sorted_list(diet(T)) = list(T) <= enum(T).
:- pred to_sorted_list(diet(T)::in, list(T)::out) is det <= enum(T).

    % `to_sorted_interval_list(Set)' returns a list of intervals in `Set'
    % in sorted order, where each interval is represented by a tuple.
    % The intervals do not overlap.
    %
:- pred to_sorted_interval_list(diet(T)::in, list({T, T})::out) is det
    <= enum(T).

    % `from_interval_list(Intervals, Set)' returns a Set containing the
    % elements of all intervals [X, Y] in Intervals, where each interval is
    % represented by a tuple. Throws an exception if any interval has Y < X.
    % The intervals may overlap.
    %
:- pred from_interval_list(list({T, T})::in, diet(T)::out) is det <= enum(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% This file is, in large parts, a translation of Caml Diet.
% https://github.com/tcsprojects/camldiets

/***********************************************************
 *                      CAML DIET                          *
 *                                                         *
 *                  Copyright (c) 2010                     *
 *           Distributed under the BSD license.            *
 *                                                         *
 *                   Oliver Friedmann                      *
 *              Oliver.Friedmann@gmail.com                 *
 *                 University of Munich                    *
 *                                                         *
 *                    Martin Lange                         *
 *                Martin.Lange@gmail.com                   *
 *                 University of Kassel                    *
 *                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                         *
 * The code for handling the AVL trees is borrowed from    *
 * the Objective Caml Standard Library Set module.         *
 *                                                         *
 * (c) Xavier Leroy, projet Cristal, INRIA Rocquencourt    *
 *                                                         *
 ***********************************************************/

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type diet(T)
    --->    empty
    ;       node(
                interval    :: interval,
                node_height :: int,
                left        :: diet(T),
                right       :: diet(T)
            ).

    % It may be better to fold the arguments into node but it should be
    % verified with benchmarking. The change would affect the arguments of
    % take_min, etc.
    %
:- type interval == {int, int}. % inclusive

:- inst node
    --->    node(ground, ground, ground, ground).

%-----------------------------------------------------------------------------%

:- func pre(int) = int.

pre(X) = X - 1.

:- func succ(int) = int.

succ(X) = X + 1.

:- func safe_pre(int, int) = int.

safe_pre(Limit, X) = ( if Limit < X then pre(X) else X ).

:- func bal_const = int.

bal_const = 1.

:- func singleton(interval) = diet(T).

singleton(Z) = node(Z, 1, empty, empty).

:- func height(diet(T)) = int.

height(empty) = 0.
height(node(_, H, _, _)) = H.

:- func height_join(diet(T), diet(T)) = int.

height_join(L, R) = 1 + max(height(L), height(R)).

:- func create(interval, diet(T), diet(T)) = diet(T).

create(X, L, R) = node(X, height_join(L, R), L, R).

:- func balance(interval, diet(T), diet(T)) = diet(T).

balance(X, L, R) = T :-
    HL = height(L),
    HR = height(R),
    ( HL > HR + bal_const ->
        (
            L = empty,
            unexpected($module, $pred, "L empty")
        ;
            L = node(LVX, _, LL, LR),
            ( height(LL) >= height(LR) ->
                T = create(LVX, LL, create(X, LR, R))
            ;
                (
                    LR = empty,
                    unexpected($module, $pred, "LR empty")
                ;
                    LR = node(LRX, _, LRL, LRR),
                    T = create(LRX, create(LVX, LL, LRL), create(X, LRR, R))
                )
            )
        )
    ; HR > HL + bal_const ->
        (
            R = empty,
            unexpected($module, $pred, "R empty")
        ;
            R = node(RVX, _, RL, RR),
            ( height(RR) >= height(RL) ->
                T = create(RVX, create(X, L, RL), RR)
            ;
                (
                    RL = empty,
                    unexpected($module, $pred, "RL empty")
                ;
                    RL = node(RLX, _, RLL, RLR),
                    T = create(RLX, create(X, L, RLL), create(RVX, RLR, RR))
                )
            )
        )
    ;
        HT = (HL >= HR -> HL + 1 ; HR + 1),
        T = node(X, HT, L, R)
    ).

:- func join(interval, diet(T), diet(T)) = diet(T).

join(V, L, R) = T :-
    (
        L = empty,
        T = myadd(yes, V, R)
    ;
        R = empty,
        L = node(_, _, _, _),
        T = myadd(no, V, L)
    ;
        L = node(LX, LH, LL, LR),
        R = node(RX, RH, RL, RR),
        ( LH > RH + bal_const ->
            T = balance(LX, LL, join(V, LR, R))
        ; RH > LH + bal_const ->
            T = balance(RX, join(V, L, RL), RR)
        ;
            T = create(V, L, R)
        )
    ).

:- func myadd(bool, interval, diet(T)) = diet(T).

myadd(IsLeft, X, T0) = T :-
    (
        T0 = empty,
        T = node(X, 1, empty, empty)
    ;
        T0 = node(VX, _, L, R),
        (
            IsLeft = yes,
            T = balance(VX, myadd(IsLeft, X, L), R)
        ;
            IsLeft = no,
            T = balance(VX, L, myadd(IsLeft, X, R))
        )
    ).

:- pred take_min(diet(T)::in(node), interval::out, diet(T)::out) is det.

take_min(T0, X, T) :-
    (
        T0 = node(X, _, empty, T)
    ;
        T0 = node(X0, _, node(_, _, _, _) @ L, R),
        take_min(L, X, L1),
        T = join(X0, L1, R)
    ).

:- pred take_max(diet(T)::in(node), interval::out, diet(T)::out) is det.

take_max(T0, X, T) :-
    (
        T0 = node(X, _, T, empty)
    ;
        T0 = node(X0, _, L, node(_, _, _, _) @ R),
        take_max(R, X, R1),
        T = join(X0, L, R1)
    ).

:- func reroot(diet(T), diet(T)) = diet(T).

reroot(L, R) = T :-
    ( height(L) > height(R) ->
        (
            L = empty,
            unexpected($module, $pred, "L empty")
        ;
            L = node(_, _, _, _),
            take_max(L, I, L1),
            T = join(I, L1, R)
        )
    ;
        R = empty,
        T = empty
    ;
        R = node(_, _, _, _),
        take_min(R, I, R1),
        T = join(I, L, R1)
    ).

:- pred take_min_iter(diet(T)::in(node), interval::out, diet(T)::out) is det.

take_min_iter(T0, X, T) :-
    (
        T0 = node(X, _, empty, T)
    ;
        T0 = node(X0, _, node(A, _, L, M), R),
        N0 = node(X0, height_join(M, R), M, R),
        N1 = node(A, height_join(L, N0), L, N0),
        take_min_iter(N1, X, T)
    ).

:- pred take_min_iter2(diet(T)::in, maybe(interval)::out, diet(T)::out) is det.

take_min_iter2(T0, MaybeX, T) :-
    (
        T0 = empty,
        MaybeX = no,
        T = empty
    ;
        T0 = node(_, _, _, _),
        take_min_iter(T0, X, T),
        MaybeX = yes(X)
    ).

%-----------------------------------------------------------------------------%

:- pred unexpected_interval(string::in, T::in, T::in) is erroneous.

unexpected_interval(PredName, X, Y) :-
    unexpected($module, PredName,
        "bad interval [" ++ string(X) ++ ", " ++ string(Y) ++ "]").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

init = empty.

init(empty).

empty(init).

is_empty(empty).

is_non_empty(node(_, _, _, _)).

%-----------------------------------------------------------------------------%

equal(T1, T2) :-
    (
        T1 = empty,
        T2 = empty
    ;
        T1 = node(_, _, _, _),
        T2 = node(_, _, _, _),
        take_min_iter(T1, {X, Y}, R1),
        take_min_iter(T2, {X, Y}, R2),
        equal(R1, R2)
    ).

%-----------------------------------------------------------------------------%

make_singleton_set(X) = T :-
    I = to_int(X),
    T = singleton({I, I}).

make_interval_set(X, Y) = T :-
    P = to_int(X),
    Q = to_int(Y),
    ( P =< Q ->
        T = singleton({P, Q})
    ;
        unexpected_interval($pred, X, Y)
    ).

is_singleton(Set, Elem) :-
    Set = node({X, X}, _, empty, empty),
    Elem = det_from_int(X).

%-----------------------------------------------------------------------------%

contains(T, Z) :-
    contains_2(T, to_int(Z)).

:- pred contains_2(diet(T)::in, int::in) is semidet <= enum(T).

contains_2(T, Z) :-
    T = node({X, Y}, _, L, R),
    ( Z < X ->
        contains_2(L, Z)
    ; Z > Y ->
        contains_2(R, Z)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(member/2).

member(Elem::in, Set::in) :-
    contains(Set, Elem).
member(Elem::out, Set::in) :-
    Set = node({X, Y}, _, Left, Right),
    (
        member(Elem, Left)
    ;
        int.nondet_int_in_range(to_int(X), to_int(Y), Int),
        Elem = from_int(Int)
    ;
        member(Elem, Right)
    ).

%-----------------------------------------------------------------------------%

subset(T1, T2) :-
    (
        T1 = empty
    ;
        T1 = node(_, _, _, _),
        T2 = node(_, _, _, _),
        take_min_iter(T1, XY1, R1),
        take_min_iter(T2, XY2, R2),
        subset_2(XY1, R1, XY2, R2, yes)
    ).

:- pred subset_2(interval::in, diet(T)::in, interval::in, diet(T)::in,
    bool::out) is det.

subset_2({X1, Y1}, R1, {X2, Y2}, R2, IsSubset) :-
    ( X1 < X2 ->
        IsSubset = no
    ; X1 > Y2 ->
        (
            R2 = empty,
            IsSubset = no
        ;
            R2 = node(_, _, _, _),
            take_min_iter(R2, Min2, MinR2),
            subset_2({X1, Y1}, R1, Min2, MinR2, IsSubset)
        )
    ;
        compare(Upper, Y1, Y2),
        (
            Upper = (<),
            (
                R1 = empty,
                IsSubset = yes
            ;
                R1 = node(_, _, _, _),
                take_min_iter(R1, Min1, MinR1),
                subset_2(Min1, MinR1, {X2, Y2}, R2, IsSubset)
            )
        ;
            Upper = (=),
            (
                R1 = empty,
                IsSubset = yes
            ;
                R1 = node(_, _, _, _),
                R2 = empty,
                IsSubset = no
            ;
                R1 = node(_, _, _, _),
                R2 = node(_, _, _, _),
                take_min_iter(R1, Min1, MinR1),
                take_min_iter(R2, Min2, MinR2),
                subset_2(Min1, MinR1, Min2, MinR2, IsSubset)
            )
        ;
            Upper = (>),
            IsSubset = no
        )
    ).

%-----------------------------------------------------------------------------%

superset(Superset, Set) :-
    subset(Set, Superset).

%-----------------------------------------------------------------------------%

insert(Set0, Elem) = Set :-
    insert(Elem, Set0, Set).

insert(Elem, Set0, Set) :-
    Set = add(to_int(Elem), Set0).

:- func add(int, diet(T)) = diet(T).

add(P, T0) = T :-
    (
        T0 = empty,
        T = node({P, P}, 1, empty, empty)
    ;
        T0 = node({X, Y}, H, Left, Right),
        ( P >= X ->
            ( P =< Y ->
                T = T0
            ; P > succ(Y) ->
                T = join({X, Y}, Left, add(P, Right))
            ;
                (
                    Right = empty,
                    T = node({X, P}, H, Left, Right)
                ;
                    Right = node(_, _, _, _),
                    take_min(Right, {U, V}, R),
                    ( pre(U) = P ->
                        T = join({X, V}, Left, R)
                    ;
                        T = node({X, P}, H, Left, Right)
                    )
                )
            )
        ; P < pre(X) ->
            T = join({X, Y}, add(P, Left), Right)
        ;
            (
                Left = empty,
                T = node({P, Y}, H, Left, Right)
            ;
                Left = node(_, _, _, _),
                take_max(Left, {U, V}, L),
                ( succ(V) = P ->
                    T = join({U, Y}, L, Right)
                ;
                    T = node({P, Y}, H, Left, Right)
                )
            )
        )
    ).

%-----------------------------------------------------------------------------%

insert_new(Elem, Set0, Set) :-
    add_new(to_int(Elem), Set0, Set).

:- pred add_new(int::in, diet(T)::in, diet(T)::out) is semidet.

add_new(P, T0, T) :-
    (
        T0 = empty,
        T = node({P, P}, 1, empty, empty)
    ;
        T0 = node({X, Y}, H, Left, Right),
        ( P >= X ->
            ( P =< Y ->
                % Already exists.
                fail
            ; P > succ(Y) ->
                add_new(P, Right, R),
                T = join({X, Y}, Left, R)
            ;
                (
                    Right = empty,
                    T = node({X, P}, H, Left, Right)
                ;
                    Right = node(_, _, _, _),
                    take_min(Right, {U, V}, R),
                    ( pre(U) = P ->
                        T = join({X, V}, Left, R)
                    ;
                        T = node({X, P}, H, Left, Right)
                    )
                )
            )
        ; P < pre(X) ->
            add_new(P, Left, L),
            T = join({X, Y}, L, Right)
        ;
            (
                Left = empty,
                T = node({P, Y}, H, Left, Right)
            ;
                Left = node(_, _, _, _),
                take_max(Left, {U, V}, L),
                ( succ(V) = P ->
                    T = join({U, V}, L, Right)
                ;
                    T = node({P, Y}, H, Left, Right)
                )
            )
        )
    ).

%-----------------------------------------------------------------------------%

insert_list(Set0, Elems) = Set :-
    insert_list(Elems, Set0, Set).

insert_list(Elems, Set0, Set) :-
    foldl(insert, Elems, Set0, Set).

%-----------------------------------------------------------------------------%

insert_interval(X, Y, Set0, Set) :-
    P = to_int(X),
    Q = to_int(Y),
    ( P =< Q ->
        Set = do_insert({P, Q}, Set0)
    ;
        unexpected_interval($pred, X, Y)
    ).

:- pred insert_interval({T, T}::in, diet(T)::in, diet(T)::out) is det
    <= enum(T).

insert_interval({X, Y}, Set0, Set) :-
    insert_interval(X, Y, Set0, Set).

:- func do_insert(interval, diet(T)) = diet(T).

do_insert(PQ, T0) = T :-
    PQ = {P, Q},
    (
        T0 = empty,
        T = singleton(PQ)
    ;
        T0 = node({X0, Y0}, _, Left0, Right0),
        ( Q < pre(X0) ->
            T = join({X0, Y0}, do_insert(PQ, Left0), Right0)
        ; P > succ(Y0) ->
            T = join({X0, Y0}, Left0, do_insert(PQ, Right0))
        ;
            ( P >= X0 ->
                X1 = X0,
                Left1 = Left0
            ;
                find_del_left(P, Left0, X1, Left1)
            ),
            ( Q =< Y0 ->
                Y1 = Y0,
                Right1 = Right0
            ;
                find_del_right(Q, Right0, Y1, Right1)
            ),
            T = join({X1, Y1}, Left1, Right1)
        )
    ).

:- pred find_del_left(int::in, diet(T)::in, int::out, diet(T)::out) is det.

find_del_left(P0, T0, P, T) :-
    (
        T0 = empty,
        P = P0,
        T = empty
    ;
        T0 = node({X, Y}, _, Left, Right0),
        ( P0 > succ(Y) ->
            find_del_left(P0, Right0, P, Right1),
            T = join({X, Y}, Left, Right1)
        ;
            P = X,
            T = Left
        )
    ).

:- pred find_del_right(int::in, diet(T)::in, int::out, diet(T)::out) is det.

find_del_right(P0, T0, P, T) :-
    (
        T0 = empty,
        P = P0,
        T = empty
    ;
        T0 = node({X, Y}, _, Left0, Right),
        ( P0 < pre(X) ->
            find_del_right(P0, Left0, P, Left1),
            T = join({X, Y}, Left1, Right)
        ;
            P = Y,
            T = Right
        )
    ).

%-----------------------------------------------------------------------------%

delete(Set0, Elem) = Set :-
    delete(Elem, Set0, Set).

delete(Elem, Set0, Set) :-
    ( remove(Elem, Set0, Set1) ->
        Set = Set1
    ;
        Set = Set0
    ).

%-----------------------------------------------------------------------------%

delete_list(Set0, List) = Set :-
    delete_list(List, Set0, Set).

delete_list(List, Set0, Set) :-
    difference(Set0, list_to_set(List), Set).

%-----------------------------------------------------------------------------%

remove(Elem, Set0, Set) :-
    remove_2(to_int(Elem), Set0, Set).

:- pred remove_2(int::in, diet(T)::in, diet(T)::out) is semidet.

remove_2(_Z, empty, _T) :-
    fail.
remove_2(Z, T0, T) :-
    T0 = node({X, Y}, H, Left, Right),
    compare(CZX, Z, X),
    (
        CZX = (<),
        remove_2(Z, Left, L),
        T = join({X, Y}, L, Right)
    ;
        ( CZX = (=)
        ; CZX = (>)
        ),
        compare(CZY, Z, Y),
        (
            CZY = (=),
            (
                CZX = (=),
                T = reroot(Left, Right)
            ;
                CZX = (>),
                T = node({X, pre(Y)}, H, Left, Right)
            )
        ;
            CZY = (<),
            (
                CZX = (=),
                T = node({succ(X), Y}, H, Left, Right)
            ;
                CZX = (>),
                T = do_insert({succ(Z), Y}, node({X, pre(Z)}, H, Left, Right))
            )
        ;
            CZY = (>),
            remove_2(Z, Right, R),
            T = join({X, Y}, Left, R)
        )
    ).

%-----------------------------------------------------------------------------%

remove_list(X, Set0, Set) :-
    list_to_set(X, SetX),
    subset(SetX, Set0),
    difference(Set0, SetX, Set).

%-----------------------------------------------------------------------------%

remove_least(det_from_int(X), Set0, Set) :-
    (
        Set0 = empty,
        fail
    ;
        Set0 = node(_, _, _, _),
        take_min(Set0, {X, Y}, Stream),
        ( X = Y ->
            Set = Stream
        ;
            Set = do_insert({succ(X), Y}, Stream)
        )
    ).

%-----------------------------------------------------------------------------%

split(Elem, Set, Lesser, IsPresent, Greater) :-
    split_2(to_int(Elem), Set, Lesser, IsPresent, Greater).

:- pred split_2(int::in, diet(T)::in, diet(T)::out, bool::out, diet(T)::out)
    is det.

split_2(X, Set, Lesser, IsPresent, Greater) :-
    (
        Set = empty,
        IsPresent = no,
        Lesser = empty,
        Greater = empty
    ;
        Set = node({A, B}, _, L, R),
        ( X < A ->
            split_2(X, L, Lesser, IsPresent, RL),
            Greater = join({A, B}, RL, R)
        ; B < X ->
			split_2(X, R, LR, IsPresent, Greater),
            Lesser = join({A, B}, L, LR)
        ;
            IsPresent = yes,
            ( X = A ->
                Lesser = L
            ;
                Lesser = do_insert({A, pre(X)}, L)
            ),
            ( X = B ->
                Greater = R
            ;
                Greater = do_insert({succ(X), B}, R)
            )
        )
    ).

%-----------------------------------------------------------------------------%

union(DietA, DietB, union(DietA, DietB)).

union(Input, Stream0) = Result :-
    ( height(Stream0) > height(Input) ->
        Result = union(Stream0, Input)
    ;
        take_min_iter2(Stream0, Head1, Stream1),
        union_2(Input, no, Head1, Stream1, Left2, Head2, Stream2),
        (
            Head2 = no,
            Result = Left2
        ;
            Head2 = yes(I),
            Result = join(I, Left2, Stream2)
        )
    ).

:- pred union_2(diet(T)::in, maybe(int)::in, maybe({int, int})::in,
    diet(T)::in, diet(T)::out, maybe({int, int})::out, diet(T)::out) is det.

union_2(Input, Limit, Head0, Stream0, Left, Head, Stream) :-
    (
        Head0 = no,
        Left = Input,
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, _Y}),
        (
            Input = empty,
            Left = empty,
            Head = Head0,
            Stream = Stream0
        ;
            Input = node({A, B}, _, Left0, Right0),
            ( X < A ->
                union_2(Left0, yes(pre(A)), Head0, Stream0,
                    Left1, Head1, Stream1)
            ;
                Left1 = Left0,
                Head1 = Head0,
                Stream1 = Stream0
            ),
            union_helper(Left1, {A, B}, Right0, Limit, Head1, Stream1,
                Left, Head, Stream)
        )
    ).

:- pred union_helper(diet(T)::in, {int, int}::in, diet(T)::in,
    maybe(int)::in, maybe({int, int})::in, diet(T)::in,
    diet(T)::out, maybe({int, int})::out, diet(T)::out) is det.

union_helper(Left0, {A, B}, Right0, Limit, Head0, Stream0,
        Left, Head, Stream) :-
    (
        Head0 = no,
        Left = join({A, B}, Left0, Right0),
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, Y}),
        (
            Y < A,
            Y < pre(A)
        ->
            Left1 = do_insert({X, Y}, Left0),
            take_min_iter2(Stream0, Head1, Stream1),
            union_helper(Left1, {A, B}, Right0, Limit, Head1, Stream1,
                Left, Head, Stream)
        ;
            X > B,
            X > succ(B)
        ->
            union_2(Right0, Limit, Head0, Stream0,
                Right1, Head1, Stream1),
            Left = join({A, B}, Left0, Right1),
            Head = Head1,
            Stream = Stream1
        ;
            B >= Y
        ->
            take_min_iter2(Stream0, Head1, Stream1),
            union_helper(Left0, {min(A, X), B}, Right0, Limit, Head1, Stream1,
                Left, Head, Stream)
        ;
            greater_or_equal(Y, Limit)
        ->
            Left = Left0,
            Head = yes({min(A, X), Y}),
            Stream = Stream0
        ;
            union_2(Right0, Limit, yes({min(A, X), Y}), Stream0,
                Right1, Head1, Stream1),
            Left = reroot(Left0, Right1),
            Head = Head1,
            Stream = Stream1
        )
    ).

:- pred greater_or_equal(int::in, maybe(int)::in) is semidet.

greater_or_equal(Z, yes(U)) :-
    Z >= U.

%-----------------------------------------------------------------------------%

union_list(Sets) = Set :-
    union_list(Sets, Set).

union_list(Sets, Set) :-
    (
        Sets = [],
        Set = empty
    ;
        Sets = [SetA | SetBs],
        foldl(union, SetBs, SetA, Set)
    ).

%-----------------------------------------------------------------------------%

intersect(SetA, SetB) = inter(SetA, SetB).

intersect(SetA, SetB, inter(SetA, SetB)).

:- func inter(diet(T), diet(T)) = diet(T).

inter(Input, Stream0) = Result :-
    ( height(Stream0) > height(Input) ->
        Result = inter(Stream0, Input)
    ;
        Stream0 = empty,
        Result = empty
    ;
        Stream0 = node(_, _, _, _),
        take_min_iter(Stream0, Head, Stream),
        inter_2(Input, yes(Head), Stream, Result, _, _)
    ).

:- pred inter_2(diet(T)::in, maybe({int, int})::in, diet(T)::in,
    diet(T)::out, maybe({int, int})::out, diet(T)::out) is det.

inter_2(Input, Head0, Stream0, Result, Head, Stream) :-
    (
        Head0 = no,
        Result = empty,
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, _Y}),
        (
            Input = empty,
            Result = empty,
            Head = Head0,
            Stream = Stream0
        ;
            Input = node({A, B}, _, Left0, Right0),
            ( X < A ->
                inter_2(Left0, Head0, Stream0, Left1, Head1, Stream1)
            ;
                Left1 = empty,
                Head1 = Head0,
                Stream1 = Stream0
            ),
            inter_help({A, B}, Right0, Left1, Head1, Stream1,
                Result, Head, Stream)
        )
    ).

:- pred inter_help({int, int}::in, diet(T)::in,
    diet(T)::in, maybe({int, int})::in, diet(T)::in,
    diet(T)::out, maybe({int, int})::out, diet(T)::out) is det.

inter_help({A, B}, Right0, Left0, Head0, Stream0,
        Result, Head, Stream) :-
    (
        Head0 = no,
        Result = Left0,
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, Y}),
        ( Y < A ->
            (
                Stream0 = empty,
                Result = Left0,
                Head = no,
                Stream = empty
            ;
                Stream0 = node(_, _, _, _),
                take_min_iter(Stream0, Head1, Stream1),
                inter_help({A, B}, Right0, Left0, yes(Head1), Stream1,
                    Result, Head, Stream)
            )
        ; B < X ->
            inter_2(Right0, Head0, Stream0, Right1, Head1, Stream1),
            Result = reroot(Left0, Right1),
            Head = Head1,
            Stream = Stream1
        ; Y >= safe_pre(Y, B) ->
            inter_2(Right0, Head0, Stream0, Right1, Head1, Stream1),
            Result = join({max(X, A), min(Y, B)}, Left0, Right1),
            Head = Head1,
            Stream = Stream1
        ;
            Left1 = do_insert({max(X, A), Y}, Left0),
            inter_help({succ(Y), B}, Right0, Left1, Head0, Stream0,
                Result, Head, Stream)
        )
    ).

%-----------------------------------------------------------------------------%

intersect_list(Sets) = Set :-
    intersect_list(Sets, Set).

intersect_list([], init).
intersect_list([Set0 | Sets], Set) :-
    foldl(intersect, Sets, Set0, Set).

%-----------------------------------------------------------------------------%

difference(SetA, SetB) = Set :-
    difference(SetA, SetB, Set).

difference(SetA, SetB, Set) :-
    (
        SetA = empty,
        Set = empty
    ;
        SetA = node(_, _, _, _),
        SetB = empty,
        Set = SetA
    ;
        SetA = node(_, _, _, _),
        SetB = node(_, _, _, _),
        take_min_iter(SetB, Head, Stream),
        diff(SetA, yes(Head), Stream, Set, _RemHead, _RemStream)
    ).

:- pred diff(diet(T)::in, maybe(interval)::in, diet(T)::in,
    diet(T)::out, maybe(interval)::out, diet(T)::out) is det.

diff(Input, Head0, Stream0, Output, Head, Stream) :-
    (
        Head0 = no,
        Output = Input,
        Head = no,
        Stream = empty
    ;
        Head0 = yes(_),
        Input = empty,
        Output = empty,
        Head = Head0,
        Stream = Stream0
    ;
        Head0 = yes({X, _Y}),
        Input = node({A, B}, _, Left0, Right0),
        ( X < A ->
            diff(Left0, Head0, Stream0, Left1, Head1, Stream1)
        ;
            Left1 = Left0,
            Head1 = Head0,
            Stream1 = Stream0
        ),
        diff_helper({A, B}, Right0, Left1, Head1, Stream1,
            Output, Head, Stream)
    ).

:- pred diff_helper(interval::in, diet(T)::in, diet(T)::in,
    maybe(interval)::in, diet(T)::in, diet(T)::out, maybe(interval)::out,
    diet(T)::out) is det.

diff_helper({A, B}, Right0, Left0, Head0, Stream0,
        Output, Head, Stream) :-
    (
        Head0 = no,
        Output = join({A, B}, Left0, Right0),
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, Y}),
        ( Y < A ->
            take_min_iter2(Stream0, Head1, Stream1),
            diff_helper({A, B}, Right0, Left0, Head1, Stream1,
                Output, Head, Stream)
        ; B < X ->
            diff(Right0, Head0, Stream0, Right1, Head, Stream),
            Output = join({A, B}, Left0, Right1)
        ; A < X ->
            Left1 = do_insert({A, pre(X)}, Left0),
            diff_helper({X, B}, Right0, Left1, Head0, Stream0,
                Output, Head, Stream)
        ; Y < B ->
            take_min_iter2(Stream0, Head1, Stream1),
            diff_helper({succ(Y), B}, Right0, Left0, Head1, Stream1,
                Output, Head, Stream)
        ;
            diff(Right0, Head0, Stream0, Right1, Head, Stream),
            Output = reroot(Left0, Right1)
        )
    ).

%-----------------------------------------------------------------------------%

divide(Pred, Set, TrueSet, FalseSet) :-
    % Can do better.
    foldl2(divide_2(Pred), Set, init, TrueSet, init, FalseSet).

:- pred divide_2(pred(T), T, diet(T), diet(T), diet(T), diet(T)) <= enum(T).
:- mode divide_2(pred(in) is semidet, in, in, out, in, out) is det.

divide_2(Pred, Elem, !TrueSet, !FalseSet) :-
    ( Pred(Elem) ->
        insert(Elem, !TrueSet)
    ;
        insert(Elem, !FalseSet)
    ).

%-----------------------------------------------------------------------------%

divide_by_set(DivideBySet, Set, InPart, OutPart) :-
    intersect(Set, DivideBySet, InPart),
    difference(Set, DivideBySet, OutPart).

%-----------------------------------------------------------------------------%

count(T) = Count :-
    count(T, 0, Count).

:- pred count(diet(T)::in, int::in, int::out) is det.

count(T, Acc0, Acc) :-
    (
        T = empty,
        Acc = Acc0
    ;
        T = node({X, Y}, _, L, R),
        Acc1 = Acc0 + (1 + Y - X),
        count(L, Acc1, Acc2),
        count(R, Acc2, Acc)
    ).

%-----------------------------------------------------------------------------%

foldl_intervals(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl_intervals(P, L, !Acc),
        P(det_from_int(X), det_from_int(Y), !Acc),
        foldl_intervals(P, R, !Acc)
    ).

%-----------------------------------------------------------------------------%

foldr_intervals(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldr_intervals(P, R, !Acc),
        P(det_from_int(X), det_from_int(Y), !Acc),
        foldr_intervals(P, L, !Acc)
    ).

%-----------------------------------------------------------------------------%

foldl(F, Set, Acc0) = Acc :-
    P = (pred(E::in, PAcc0::in, PAcc::out) is det :-
        PAcc = F(E, PAcc0)
    ),
    foldl(P, Set, Acc0, Acc).

%-----------------------------------------------------------------------------%

foldl(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl(P, L, !Acc),
        foldl_2(P, X, Y, !Acc),
        foldl(P, R, !Acc)
    ).

:- pred foldl_2(pred(T, Acc, Acc), int, int, Acc, Acc) <= enum(T).
:- mode foldl_2(pred(in, in, out) is det, in, in, in, out) is det.
:- mode foldl_2(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode foldl_2(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode foldl_2(pred(in, in, out) is semidet, in, in, in, out) is semidet.
:- mode foldl_2(pred(in, mdi, muo) is semidet, in, in, mdi, muo) is semidet.
:- mode foldl_2(pred(in, di, uo) is semidet, in, in, di, uo) is semidet.

foldl_2(P, Lo, Hi, !Acc) :-
    ( Lo =< Hi ->
        P(det_from_int(Lo), !Acc),
        foldl_2(P, Lo + 1, Hi, !Acc)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

foldl2(P, T, !Acc1, !Acc2) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl2(P, L, !Acc1, !Acc2),
        fold_up2(P, X, Y, !Acc1, !Acc2),
        foldl2(P, R, !Acc1, !Acc2)
    ).

:- pred fold_up2(pred(T, Acc1, Acc1, Acc2, Acc2), int, int,
    Acc1, Acc1, Acc2, Acc2) <= enum(T).
:- mode fold_up2(pred(in, in, out, in, out) is det, in, in,
    in, out, in, out) is det.
:- mode fold_up2(pred(in, in, out, mdi, muo) is det, in, in,
    in, out, mdi, muo) is det.
:- mode fold_up2(pred(in, in, out, di, uo) is det, in, in,
    in, out, di, uo) is det.
:- mode fold_up2(pred(in, in, out, in, out) is semidet, in, in,
    in, out, in, out) is semidet.
:- mode fold_up2(pred(in, in, out, mdi, muo) is semidet, in, in,
    in, out, mdi, muo) is semidet.
:- mode fold_up2(pred(in, in, out, di, uo) is semidet, in, in,
    in, out, di, uo) is semidet.

fold_up2(P, Lo, Hi, !A, !B) :-
    ( Lo =< Hi ->
        P(det_from_int(Lo), !A, !B),
        fold_up2(P, Lo + 1, Hi, !A, !B)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

foldl3(P, T, !Acc1, !Acc2, !Acc3) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl3(P, L, !Acc1, !Acc2, !Acc3),
        fold_up3(P, X, Y, !Acc1, !Acc2, !Acc3),
        foldl3(P, R, !Acc1, !Acc2, !Acc3)
    ).

:- pred fold_up3(pred(T, Acc1, Acc1, Acc2, Acc2, Acc3, Acc3), int, int,
    Acc1, Acc1, Acc2, Acc2, Acc3, Acc3) <= enum(T).
:- mode fold_up3(pred(in, in, out, in, out, in, out) is det, in, in,
    in, out, in, out, in, out) is det.
:- mode fold_up3(pred(in, in, out, in, out, mdi, muo) is det, in, in,
    in, out, in, out, mdi, muo) is det.
:- mode fold_up3(pred(in, in, out, in, out, di, uo) is det, in, in,
    in, out, in, out, di, uo) is det.
:- mode fold_up3(pred(in, in, out, in, out, in, out) is semidet, in, in,
    in, out, in, out, in, out) is semidet.
:- mode fold_up3(pred(in, in, out, in, out, mdi, muo) is semidet, in, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode fold_up3(pred(in, in, out, in, out, di, uo) is semidet, in, in,
    in, out, in, out, di, uo) is semidet.

fold_up3(P, Lo, Hi, !A, !B, !C) :-
    ( Lo =< Hi ->
        P(det_from_int(Lo), !A, !B, !C),
        fold_up3(P, Lo + 1, Hi, !A, !B, !C)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

foldl4(P, T, !A, !B, !C, !D) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl4(P, L, !A, !B, !C, !D),
        fold_up4(P, X, Y, !A, !B, !C, !D),
        foldl4(P, R, !A, !B, !C, !D)
    ).

:- pred fold_up4(pred(T, A, A, B, B, C, C, D, D),
    int, int, A, A, B, B, C, C, D, D) <= enum(T).
:- mode fold_up4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out, in, out) is det.
:- mode fold_up4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold_up4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, in, out, di, uo) is det.
:- mode fold_up4(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out, in, out) is semidet.
:- mode fold_up4(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold_up4(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, in, out, di, uo) is semidet.

fold_up4(P, Lo, Hi, !A, !B, !C, !D) :-
    ( Lo =< Hi ->
        P(det_from_int(Lo), !A, !B, !C, !D),
        fold_up4(P, Lo + 1, Hi, !A, !B, !C, !D)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

foldl5(P, T, !A, !B, !C, !D, !E) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl5(P, L, !A, !B, !C, !D, !E),
        fold_up5(P, X, Y, !A, !B, !C, !D, !E),
        foldl5(P, R, !A, !B, !C, !D, !E)
    ).

:- pred fold_up5(
    pred(T, A, A, B, B, C, C, D, D, E, E),
    int, int, A, A, B, B, C, C, D, D, E, E) <= enum(T).
:- mode fold_up5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold_up5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold_up5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold_up5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold_up5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold_up5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, in, out, in, out, di, uo) is semidet.

fold_up5(P, Lo, Hi, !A, !B, !C, !D, !E) :-
    ( Lo =< Hi ->
        P(det_from_int(Lo), !A, !B, !C, !D, !E),
        fold_up5(P, Lo + 1, Hi, !A, !B, !C, !D, !E)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

foldr(F, Set, Acc0) = Acc :-
    P = (pred(E::in, PAcc0::in, PAcc::out) is det :-
        PAcc = F(E, PAcc0)
    ),
    foldr(P, Set, Acc0, Acc).

foldr(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldr(P, R, !Acc),
        fold_down(P, X, Y, !Acc),
        foldr(P, L, !Acc)
    ).

:- pred fold_down(pred(T, A, A), int, int, A, A) <= enum(T).
:- mode fold_down(pred(in, in, out) is det, in, in, in, out) is det.
:- mode fold_down(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode fold_down(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode fold_down(pred(in, in, out) is semidet, in, in, in, out) is semidet.
:- mode fold_down(pred(in, mdi, muo) is semidet, in, in, mdi, muo) is semidet.
:- mode fold_down(pred(in, di, uo) is semidet, in, in, di, uo) is semidet.

fold_down(P, Lo, Hi, !A) :-
    ( Lo =< Hi ->
        P(det_from_int(Hi), !A),
        fold_down(P, Lo, Hi - 1, !A)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

all_true(P, Set) :-
    (
        Set = empty
    ;
        Set = node({X, Y}, _, L, R),
        all_true(P, L),
        all_true_interval(P, X, Y),
        all_true(P, R)
    ).

:- pred all_true_interval(pred(T)::in(pred(in) is semidet), int::in, int::in)
    is semidet <= enum(T).

all_true_interval(P, Lo, Hi) :-
    ( Lo =< Hi ->
        P(det_from_int(Lo)),
        all_true_interval(P, Lo + 1, Hi)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

filter(Pred, Set) = TrueSet :-
    divide(Pred, Set, TrueSet, _FalseSet).

filter(Pred, Set, TrueSet, FalseSet) :-
    divide(Pred, Set, TrueSet, FalseSet).

%-----------------------------------------------------------------------------%

list_to_set(List) = Set :-
    list_to_set(List, Set).

list_to_set(List, Set) :-
    foldl(insert, List, init, Set).

from_list(List) = Set :-
    list_to_set(List, Set).

from_list(List, Set) :-
    list_to_set(List, Set).

sorted_list_to_set(List) = Set :-
    sorted_list_to_set(List, Set).

sorted_list_to_set(List, Set) :-
    list_to_set(List, Set).

to_sorted_list(T) = List :-
    to_sorted_list(T, List).

to_sorted_list(T, List) :-
    foldr(list.cons, T, [], List).

to_sorted_interval_list(Set, List) :-
    foldr_intervals(cons_interval, Set, [], List).

:- pred cons_interval(T::in, T::in, list({T, T})::in, list({T, T})::out)
    is det.

cons_interval(X, Y, L, [{X, Y} | L]).

from_interval_list(List, Set) :-
    list.foldl(insert_interval, List, init, Set).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
