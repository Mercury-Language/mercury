%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2012-2014 YesLogic Pty. Ltd.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: diet.m.
% Author: wangp.
% Stability: medium.
%
% Discrete Interval Encoding Trees are a highly efficient set implementation
% for fat sets, i.e. densely populated sets over a discrete linear order.
%
% M. Erwig: Diets for Fat Sets,
% Journal of Functional Programming, Vol. 8, No. 6, pp. 627-632.
%
% O. Friedmann, M. Lange: More on Balanced Diets,
% Journal of Functional Programming, volume 21, issue 02, pp. 135-157.
%
%---------------------------------------------------------------------------%

:- module diet.
:- interface.

:- import_module bool.
:- import_module enum.
:- import_module list.

%---------------------------------------------------------------------------%

:- type diet(T). % <= diet_element(T).

:- typeclass diet_element(T) where [
    % less_than(X, Y) succeeds iff X < Y.
    pred less_than(T::in, T::in) is semidet,

    % successor(X) returns the successor of X, e.g. X + 1.
    func successor(T) = T,

    % predecessor(X) returns the predecessor of X, e.g. X - 1.
    func predecessor(T) = T
].

:- instance diet_element(int).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % Return an empty set.
    %
:- func init = diet(T).
:- pred init(diet(T)::out) is det.

    % make_singleton_set(Elem) returns a set containing just the single
    % element Elem.
    %
:- func make_singleton_set(T) = diet(T) <= diet_element(T).

    % make_interval_set(X, Y) returns a set containing just the elements in
    % the interval [X, Y]. Throws an exception if Y < X.
    %
:- func make_interval_set(T, T) = diet(T) <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

:- pred empty(diet(T)).
:- mode empty(in) is semidet.
:- mode empty(out) is det.
:- pragma obsolete(pred(empty/1), [init/0, init/1, is_empty/1]).

:- pred is_empty(diet(T)::in) is semidet.

:- pred is_non_empty(diet(T)::in) is semidet.

    % is_singleton(Set, X) is true iff Set is a singleton containing the
    % element X.
    %
:- pred is_singleton(diet(T)::in, T::out) is semidet <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % member(X, Set) is true iff X is a member of Set.
    %
:- pred member(T, diet(T)) <= diet_element(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % contains(Set, X) is true iff X is a member of Set.
    %
:- pred contains(diet(T)::in, T::in) is semidet <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % insert(X, Set0, Set) is true iff Set is the union of
    % Set0 and the set containing only X.
    %
:- func insert(diet(T), T) = diet(T) <= diet_element(T).
:- pred insert(T::in, diet(T)::in, diet(T)::out) is det <= diet_element(T).

    % insert_interval(X, Y, Set0, Set) is true iff Set is the union of
    % Set0 and the set containing only the elements of the interval [X, Y].
    % Throws an exception if Y < X.
    %
:- pred insert_interval(T::in, T::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

    % insert_new(X, Set0, Set) is true iff Set0 does not contain
    % X, and Set is the union of Set0 and the set containing only X.
    %
:- pred insert_new(T::in, diet(T)::in, diet(T)::out) is semidet
    <= diet_element(T).

    % insert_list(Xs, Set0, Set) is true iff Set is the union of
    % Set0 and the set containing only the members of Xs.
    %
:- func insert_list(diet(T), list(T)) = diet(T) <= diet_element(T).
:- pred insert_list(list(T)::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

    % delete(X, Set0, Set) is true iff Set is the relative
    % complement of Set0 and the set containing only X, i.e.
    % if Set is the set which contains all the elements of Set0
    % except X.
    %
:- func delete(diet(T), T) = diet(T) <= diet_element(T).
:- pred delete(T::in, diet(T)::in, diet(T)::out) is det <= diet_element(T).

    % delete_list(Set, X) returns the difference of Set and the set
    % containing only the members of X. Same as
    % `difference(Set, list_to_set(X))', but may be more efficient.
    %
:- func delete_list(diet(T), list(T)) = diet(T) <= diet_element(T).
:- pred delete_list(list(T)::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

    % remove(X, Set0, Set) is true iff Set0 contains X,
    % and Set is the relative complement of Set0 and the set
    % containing only X, i.e.  if Set is the set which contains
    % all the elements of Set0 except X.
    %
:- pred remove(T::in, diet(T)::in, diet(T)::out) is semidet <= diet_element(T).

    % remove_list(X, Set0, Set) returns in Set the difference of Set0
    % and the set containing all the elements of X, failing if any element
    % of X is not in Set0. Same as `subset(list_to_set(X), Set0),
    % difference(Set0, list_to_set(X), Set)', but may be more efficient.
    %
:- pred remove_list(list(T)::in, diet(T)::in, diet(T)::out) is semidet
    <= diet_element(T).

    % remove_least(X, Set0, Set) is true iff X is the least element in
    % Set0, and Set is the set which contains all the elements of Set0
    % except X.
    %
:- pred remove_least(T::out, diet(T)::in, diet(T)::out) is semidet
    <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % equal(SetA, SetB) is true iff SetA and SetB contain the same
    % elements.
    %
:- pred equal(diet(T)::in, diet(T)::in) is semidet <= diet_element(T).

    % subset(Subset, Set) is true iff Subset is a subset of Set.
    %
:- pred subset(diet(T)::in, diet(T)::in) is semidet <= diet_element(T).

    % superset(Superset, Set) is true iff Superset is a superset of Set.
    %
:- pred superset(diet(T)::in, diet(T)::in) is semidet <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % union(SetA, SetB, Set) is true iff Set is the union of SetA and SetB.
    %
:- func union(diet(T), diet(T)) = diet(T) <= diet_element(T).
:- pred union(diet(T)::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

    % union_list(Sets, Set) returns the union of all the sets in Sets.
    %
:- func union_list(list(diet(T))) = diet(T) <= diet_element(T).
:- pred union_list(list(diet(T))::in, diet(T)::out) is det <= diet_element(T).

    % intersect(SetA, SetB, Set) is true iff Set is the
    % intersection of SetA and SetB.
    %
:- func intersect(diet(T), diet(T)) = diet(T) <= diet_element(T).
:- pred intersect(diet(T)::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

    % intersect_list(Sets, Set) returns the intersection of all the sets
    % in Sets.
    %
:- func intersect_list(list(diet(T))) = diet(T) <= diet_element(T).
:- pred intersect_list(list(diet(T))::in, diet(T)::out) is det
    <= diet_element(T).

    % difference(SetA, SetB) returns the set containing all the elements
    % of SetA except those that occur in SetB.
    %
:- func difference(diet(T), diet(T)) = diet(T) <= diet_element(T).
:- pred difference(diet(T)::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % split(X, Set, Lesser, IsPresent, Greater) is true iff
    % Lesser is the set of elements in Set which are less than X and
    % Greater is the set of elements in Set which are greater than X.
    % IsPresent is `yes' if Set contains X, and `no' otherwise.
    %
:- pred split(T::in, diet(T)::in, diet(T)::out, bool::out, diet(T)::out) is det
    <= diet_element(T).

    % divide(Pred, Set, InPart, OutPart):
    % InPart consists of those elements of Set for which Pred succeeds;
    % OutPart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet), diet(T)::in,
    diet(T)::out, diet(T)::out) is det <= diet_element(T).

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in DivideBySet;
    % OutPart consists of those elements of Set which are not in DivideBySet.
    %
:- pred divide_by_set(diet(T)::in, diet(T)::in, diet(T)::out, diet(T)::out)
    is det <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % list_to_set(List) returns a set containing only the members of List.
    %
:- func list_to_set(list(T)) = diet(T) <= diet_element(T).
:- pred list_to_set(list(T)::in, diet(T)::out) is det <= diet_element(T).

:- func from_list(list(T)) = diet(T) <= diet_element(T).
:- pred from_list(list(T)::in, diet(T)::out) is det <= diet_element(T).

    % from_interval_list(Intervals, Set) returns a Set containing the
    % elements of all intervals [X, Y] in Intervals, where each interval is
    % represented by a tuple. Throws an exception if any interval has Y < X.
    % The intervals may overlap.
    %
:- pred from_interval_list(list({T, T})::in, diet(T)::out) is det
    <= diet_element(T).

    % sorted_list_to_set(List) returns a set containing only the members
    % of List. List must be sorted.
    %
:- func sorted_list_to_set(list(T)) = diet(T) <= diet_element(T).
:- pred sorted_list_to_set(list(T)::in, diet(T)::out) is det
    <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % to_sorted_list(Set) returns a list containing all the members of Set,
    % in sorted order.
    %
:- func to_sorted_list(diet(T)) = list(T) <= diet_element(T).
:- pred to_sorted_list(diet(T)::in, list(T)::out) is det <= diet_element(T).

    % to_sorted_interval_list(Set) returns a list of intervals in Set
    % in sorted order, where each interval is represented by a tuple.
    % The intervals do not overlap.
    %
:- pred to_sorted_interval_list(diet(T)::in, list({T, T})::out) is det
    <= diet_element(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

    % count(Set) returns the number of elements in Set.
    %
:- func count(diet(T)) = int <= enum(T).

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), diet(T)::in) is semidet
    <= diet_element(T).

    % filter(Pred, Set) returns the elements of Set for which Pred succeeds.
    %
:- func filter(pred(T), diet(T)) = diet(T) <= diet_element(T).
:- mode filter(pred(in) is semidet, in) = out is det.

    % filter(Pred, Set, TrueSet, FalseSet) returns the elements of Set
    % for which Pred succeeds, and those for which it fails.
    %
:- pred filter(pred(T), diet(T), diet(T), diet(T)) <= diet_element(T).
:- mode filter(pred(in) is semidet, in, out, out) is det.

    % foldl_intervals(Pred, Set, Start) calls Pred with each interval of
    % Set (in sorted order) and an accumulator (with the initial value of
    % Start), and returns the final value.
    %
:- pred foldl_intervals(pred(T, T, A, A), diet(T), A, A) <= diet_element(T).
:- mode foldl_intervals(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl_intervals(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl_intervals(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.

    % foldr_intervals(Pred, Set, Start) calls Pred with each interval of
    % Set (in reverse sorted order) and an accumulator (with the initial
    % value of Start), and returns the final value.
    %
:- pred foldr_intervals(pred(T, T, A, A), diet(T), A, A) <= diet_element(T).
:- mode foldr_intervals(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldr_intervals(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldr_intervals(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.

    % foldl(Func, Set, Start) calls Func with each element of Set
    % (in sorted order) and an accumulator (with the initial value of Start),
    % and returns the final value.
    %
:- func foldl(func(T, A) = A, diet(T), A) = A <= diet_element(T).

:- pred foldl(pred(T, A, A), diet(T), A, A) <= diet_element(T).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- pred foldl2(pred(T, A, A, B, B), diet(T), A, A, B, B) <= diet_element(T).
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
    A, A, B, B, C, C) <= diet_element(T).
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
    A, A, B, B, C, C, D, D) <= diet_element(T).
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
    A, A, B, B, C, C, D, D, E, E) <= diet_element(T).
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

:- func foldr(func(T, A) = A, diet(T), A) = A <= diet_element(T).

:- pred foldr(pred(T, A, A), diet(T), A, A) <= diet_element(T).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, di, uo) is semidet, in, di, uo) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type diet(T)
    --->    empty
    ;       node(
                interval    :: interval(T),
                node_height :: int,
                left        :: diet(T),
                right       :: diet(T)
            ).

    % It may be better to fold the arguments into node but it should be
    % verified with benchmarking. The change would affect the arguments of
    % take_min, etc.
    %
:- type interval(T) == {T, T}. % inclusive

:- inst node for diet/1
    --->    node(ground, ground, ground, ground).

%---------------------------------------------------------------------------%

:- instance diet_element(int) where [
    less_than(X, Y) :- int.'<'(X, Y),
    successor(X) = X + 1,
    predecessor(X) = X - 1
].

:- func safe_predecessor(T, T) = T <= diet_element(T).

safe_predecessor(Limit, X) =
    ( if less_than(Limit, X) then predecessor(X) else X ).

:- pred T < T <= diet_element(T).
:- mode in < in is semidet.

X < Y :-
    less_than(X, Y).

:- pred T > T <= diet_element(T).
:- mode in > in is semidet.

X > Y :-
    less_than(Y, X).

:- pred T >= T <= diet_element(T).
:- mode in >= in is semidet.

X >= Y :-
    not less_than(X, Y).

:- pred T =< T <= diet_element(T).
:- mode in =< in is semidet.

X =< Y :-
    not less_than(Y, X).

:- func min_elem(T, T) = T <= diet_element(T).

min_elem(X, Y) = ( if X < Y then X else Y ).

:- func max_elem(T, T) = T <= diet_element(T).

max_elem(X, Y) = ( if X > Y then X else Y ).

:- pred int_gt(int::in, int::in) is semidet.

int_gt(X, Y) :-
    int.'>'(X, Y).

:- pred int_ge(int::in, int::in) is semidet.

int_ge(X, Y) :-
    int.'>='(X, Y).

%---------------------------------------------------------------------------%

:- func bal_const = int.

bal_const = 1.

:- func singleton(interval(T)) = diet(T).

singleton(Z) = node(Z, 1, empty, empty).

:- func height(diet(T)) = int.

height(empty) = 0.
height(node(_, H, _, _)) = H.

:- func height_join(diet(T), diet(T)) = int.

height_join(L, R) = 1 + max(height(L), height(R)).

:- func create(interval(T), diet(T), diet(T)) = diet(T).

create(X, L, R) = node(X, height_join(L, R), L, R).

:- func balance(interval(T), diet(T), diet(T)) = diet(T).

balance(X, L, R) = T :-
    HL = height(L),
    HR = height(R),
    ( if int_gt(HL, HR + bal_const) then
        (
            L = empty,
            unexpected($pred, "L empty")
        ;
            L = node(LVX, _, LL, LR),
            ( if int_ge(height(LL), height(LR)) then
                T = create(LVX, LL, create(X, LR, R))
            else
                (
                    LR = empty,
                    unexpected($pred, "LR empty")
                ;
                    LR = node(LRX, _, LRL, LRR),
                    T = create(LRX, create(LVX, LL, LRL), create(X, LRR, R))
                )
            )
        )
    else if int_gt(HR, HL + bal_const) then
        (
            R = empty,
            unexpected($pred, "R empty")
        ;
            R = node(RVX, _, RL, RR),
            ( if int_ge(height(RR), height(RL)) then
                T = create(RVX, create(X, L, RL), RR)
            else
                (
                    RL = empty,
                    unexpected($pred, "RL empty")
                ;
                    RL = node(RLX, _, RLL, RLR),
                    T = create(RLX, create(X, L, RLL), create(RVX, RLR, RR))
                )
            )
        )
    else
        HT = 1 + max(HL, HR),
        T = node(X, HT, L, R)
    ).

:- func join(interval(T), diet(T), diet(T)) = diet(T) <= diet_element(T).

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
        ( if int_gt(LH, RH + bal_const) then
            T = balance(LX, LL, join(V, LR, R))
        else if int_gt(RH, LH + bal_const) then
            T = balance(RX, join(V, L, RL), RR)
        else
            T = create(V, L, R)
        )
    ).

:- func myadd(bool, interval(T), diet(T)) = diet(T).

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

:- pred take_min(diet(T)::in(node), interval(T)::out, diet(T)::out) is det
    <= diet_element(T).

take_min(T0, X, T) :-
    (
        T0 = node(X, _, empty, T)
    ;
        T0 = node(X0, _, node(_, _, _, _) @ L, R),
        take_min(L, X, L1),
        T = join(X0, L1, R)
    ).

:- pred take_max(diet(T)::in(node), interval(T)::out, diet(T)::out) is det
    <= diet_element(T).

take_max(T0, X, T) :-
    (
        T0 = node(X, _, T, empty)
    ;
        T0 = node(X0, _, L, node(_, _, _, _) @ R),
        take_max(R, X, R1),
        T = join(X0, L, R1)
    ).

:- func reroot(diet(T), diet(T)) = diet(T) <= diet_element(T).

reroot(L, R) = T :-
    ( if int_gt(height(L), height(R)) then
        (
            L = empty,
            unexpected($pred, "L empty")
        ;
            L = node(_, _, _, _),
            take_max(L, I, L1),
            T = join(I, L1, R)
        )
    else
        (
            R = empty,
            T = empty
        ;
            R = node(_, _, _, _),
            take_min(R, I, R1),
            T = join(I, L, R1)
        )
    ).

:- pred take_min_iter(diet(T)::in(node), interval(T)::out, diet(T)::out)
    is det <= diet_element(T).

take_min_iter(T0, X, T) :-
    (
        T0 = node(X, _, empty, T)
    ;
        T0 = node(X0, _, node(A, _, L, M), R),
        N0 = node(X0, height_join(M, R), M, R),
        N1 = node(A, height_join(L, N0), L, N0),
        take_min_iter(N1, X, T)
    ).

:- pred take_min_iter2(diet(T)::in, maybe(interval(T))::out, diet(T)::out)
    is det <= diet_element(T).

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

%---------------------------------------------------------------------------%

:- pred unexpected_interval(string::in, T::in, T::in) is erroneous.

unexpected_interval(PredName, X, Y) :-
    unexpected($module, PredName,
        "bad interval [" ++ string(X) ++ ", " ++ string(Y) ++ "]").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init = empty.

init(empty).

make_singleton_set(X) = singleton({X, X}).

make_interval_set(X, Y) = T :-
    ( if X =< Y then
        T = singleton({X, Y})
    else
        unexpected_interval($pred, X, Y)
    ).

%---------------------------------------------------------------------------%

empty(empty).

is_empty(empty).

is_non_empty(node(_, _, _, _)).

is_singleton(Set, X) :-
    Set = node({X, X}, _, empty, empty).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(Elem::in, Set::in) :-
    contains(Set, Elem).
member(Elem::out, Set::in) :-
    Set = node({X, Y}, _, Left, Right),
    (
        member(Elem, Left)
    ;
        member_in_range(X, Y, Elem)
    ;
        member(Elem, Right)
    ).

:- pred member_in_range(T::in, T::in, T::out) is multi <= diet_element(T).

member_in_range(Lo, Hi, Elem) :-
    % Leave a choice point only if there is at least one solution
    % to find on backtracking.
    ( if Lo < Hi then
        (
            Elem = Lo
        ;
            member_in_range(successor(Lo), Hi, Elem)
        )
    else
        Elem = Lo
    ).

contains(T, Z) :-
    T = node({X, Y}, _, L, R),
    ( if Z < X then
        contains(L, Z)
    else if Z > Y then
        contains(R, Z)
    else
        true
    ).

%---------------------------------------------------------------------------%

insert(Set0, Elem) = Set :-
    insert(Elem, Set0, Set).

insert(Elem, Set0, Set) :-
    Set = add(Elem, Set0).

:- func add(T, diet(T)) = diet(T) <= diet_element(T).

add(P, T0) = T :-
    (
        T0 = empty,
        T = node({P, P}, 1, empty, empty)
    ;
        T0 = node({X, Y}, H, Left, Right),
        ( if P >= X then
            ( if P =< Y then
                T = T0
            else if P > successor(Y) then
                T = join({X, Y}, Left, add(P, Right))
            else
                (
                    Right = empty,
                    T = node({X, P}, H, Left, Right)
                ;
                    Right = node(_, _, _, _),
                    take_min(Right, {U, V}, R),
                    ( if predecessor(U) = P then
                        T = join({X, V}, Left, R)
                    else
                        T = node({X, P}, H, Left, Right)
                    )
                )
            )
        else if P < predecessor(X) then
            T = join({X, Y}, add(P, Left), Right)
        else
            (
                Left = empty,
                T = node({P, Y}, H, Left, Right)
            ;
                Left = node(_, _, _, _),
                take_max(Left, {U, V}, L),
                ( if successor(V) = P then
                    T = join({U, Y}, L, Right)
                else
                    T = node({P, Y}, H, Left, Right)
                )
            )
        )
    ).

%---------------------%

insert_interval(X, Y, Set0, Set) :-
    ( if X =< Y then
        Set = do_insert({X, Y}, Set0)
    else
        unexpected_interval($pred, X, Y)
    ).

:- pred insert_interval({T, T}::in, diet(T)::in, diet(T)::out) is det
    <= diet_element(T).

insert_interval({X, Y}, Set0, Set) :-
    insert_interval(X, Y, Set0, Set).

:- func do_insert(interval(T), diet(T)) = diet(T) <= diet_element(T).

do_insert(PQ, T0) = T :-
    PQ = {P, Q},
    (
        T0 = empty,
        T = singleton(PQ)
    ;
        T0 = node({X0, Y0}, _, Left0, Right0),
        ( if Q < predecessor(X0) then
            T = join({X0, Y0}, do_insert(PQ, Left0), Right0)
        else if P > successor(Y0) then
            T = join({X0, Y0}, Left0, do_insert(PQ, Right0))
        else
            ( if P >= X0 then
                X1 = X0,
                Left1 = Left0
            else
                find_del_left(P, Left0, X1, Left1)
            ),
            ( if Q =< Y0 then
                Y1 = Y0,
                Right1 = Right0
            else
                find_del_right(Q, Right0, Y1, Right1)
            ),
            T = join({X1, Y1}, Left1, Right1)
        )
    ).

:- pred find_del_left(T::in, diet(T)::in, T::out, diet(T)::out) is det
    <= diet_element(T).

find_del_left(P0, T0, P, T) :-
    (
        T0 = empty,
        P = P0,
        T = empty
    ;
        T0 = node({X, Y}, _, Left, Right0),
        ( if P0 > successor(Y) then
            find_del_left(P0, Right0, P, Right1),
            T = join({X, Y}, Left, Right1)
        else
            P = X,
            T = Left
        )
    ).

:- pred find_del_right(T::in, diet(T)::in, T::out, diet(T)::out) is det
    <= diet_element(T).

find_del_right(P0, T0, P, T) :-
    (
        T0 = empty,
        P = P0,
        T = empty
    ;
        T0 = node({X, Y}, _, Left0, Right),
        ( if P0 < predecessor(X) then
            find_del_right(P0, Left0, P, Left1),
            T = join({X, Y}, Left1, Right)
        else
            P = Y,
            T = Right
        )
    ).

%---------------------%

insert_new(P, T0, T) :-
    (
        T0 = empty,
        T = node({P, P}, 1, empty, empty)
    ;
        T0 = node({X, Y}, H, Left, Right),
        ( if P >= X then
            ( if P =< Y then
                % Already exists.
                fail
            else if P > successor(Y) then
                insert_new(P, Right, R),
                T = join({X, Y}, Left, R)
            else
                (
                    Right = empty,
                    T = node({X, P}, H, Left, Right)
                ;
                    Right = node(_, _, _, _),
                    take_min(Right, {U, V}, R),
                    ( if predecessor(U) = P then
                        T = join({X, V}, Left, R)
                    else
                        T = node({X, P}, H, Left, Right)
                    )
                )
            )
        else if P < predecessor(X) then
            insert_new(P, Left, L),
            T = join({X, Y}, L, Right)
        else
            (
                Left = empty,
                T = node({P, Y}, H, Left, Right)
            ;
                Left = node(_, _, _, _),
                take_max(Left, {U, V}, L),
                ( if successor(V) = P then
                    T = join({U, V}, L, Right)
                else
                    T = node({P, Y}, H, Left, Right)
                )
            )
        )
    ).

%---------------------%

insert_list(Set0, Elems) = Set :-
    insert_list(Elems, Set0, Set).

insert_list(Elems, Set0, Set) :-
    foldl(insert, Elems, Set0, Set).

%---------------------%

delete(Set0, Elem) = Set :-
    delete(Elem, Set0, Set).

delete(Elem, Set0, Set) :-
    ( if remove(Elem, Set0, Set1) then
        Set = Set1
    else
        Set = Set0
    ).

%---------------------%

delete_list(Set0, List) = Set :-
    delete_list(List, Set0, Set).

delete_list(List, Set0, Set) :-
    difference(Set0, list_to_set(List), Set).

%---------------------------------------------------------------------------%

remove(_Z, empty, _T) :-
    fail.
remove(Z, T0, T) :-
    T0 = node({X, Y}, H, Left, Right),
    compare(CZX, Z, X),
    (
        CZX = (<),
        remove(Z, Left, L),
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
                T = node({X, predecessor(Y)}, H, Left, Right)
            )
        ;
            CZY = (<),
            (
                CZX = (=),
                T = node({successor(X), Y}, H, Left, Right)
            ;
                CZX = (>),
                T = do_insert({successor(Z), Y},
                        node({X, predecessor(Z)}, H, Left, Right))
            )
        ;
            CZY = (>),
            remove(Z, Right, R),
            T = join({X, Y}, Left, R)
        )
    ).

%---------------------%

remove_list(X, Set0, Set) :-
    list_to_set(X, SetX),
    subset(SetX, Set0),
    difference(Set0, SetX, Set).

%---------------------%

remove_least(X, Set0, Set) :-
    (
        Set0 = empty,
        fail
    ;
        Set0 = node(_, _, _, _),
        take_min(Set0, {X, Y}, Stream),
        ( if X = Y then
            Set = Stream
        else
            Set = do_insert({successor(X), Y}, Stream)
        )
    ).

%---------------------------------------------------------------------------%

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

:- pred subset_2(interval(T)::in, diet(T)::in, interval(T)::in, diet(T)::in,
    bool::out) is det <= diet_element(T).

subset_2({X1, Y1}, R1, {X2, Y2}, R2, IsSubset) :-
    ( if X1 < X2 then
        IsSubset = no
    else if X1 > Y2 then
        (
            R2 = empty,
            IsSubset = no
        ;
            R2 = node(_, _, _, _),
            take_min_iter(R2, Min2, MinR2),
            subset_2({X1, Y1}, R1, Min2, MinR2, IsSubset)
        )
    else
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

superset(Superset, Set) :-
    subset(Set, Superset).

%---------------------------------------------------------------------------%

union(DietA, DietB) = DietAB :-
    union(DietA, DietB, DietAB).

union(Input, Stream0, Result) :-
    ( if int_gt(height(Stream0), height(Input)) then
        Result = union(Stream0, Input)
    else
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

:- pred union_2(diet(T)::in, maybe(T)::in, maybe({T, T})::in,
    diet(T)::in, diet(T)::out, maybe({T, T})::out, diet(T)::out) is det
    <= diet_element(T).

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
            ( if X < A then
                union_2(Left0, yes(predecessor(A)), Head0, Stream0,
                    Left1, Head1, Stream1)
            else
                Left1 = Left0,
                Head1 = Head0,
                Stream1 = Stream0
            ),
            union_helper(Left1, {A, B}, Right0, Limit, Head1, Stream1,
                Left, Head, Stream)
        )
    ).

:- pred union_helper(diet(T)::in, {T, T}::in, diet(T)::in,
    maybe(T)::in, maybe({T, T})::in, diet(T)::in,
    diet(T)::out, maybe({T, T})::out, diet(T)::out) is det <= diet_element(T).

union_helper(Left0, {A, B}, Right0, Limit, Head0, Stream0,
        Left, Head, Stream) :-
    (
        Head0 = no,
        Left = join({A, B}, Left0, Right0),
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, Y}),
        ( if
            Y < A,
            Y < predecessor(A)
        then
            Left1 = do_insert({X, Y}, Left0),
            take_min_iter2(Stream0, Head1, Stream1),
            union_helper(Left1, {A, B}, Right0, Limit, Head1, Stream1,
                Left, Head, Stream)
        else if
            X > B,
            X > successor(B)
        then
            union_2(Right0, Limit, Head0, Stream0,
                Right1, Head1, Stream1),
            Left = join({A, B}, Left0, Right1),
            Head = Head1,
            Stream = Stream1
        else if
            B >= Y
        then
            take_min_iter2(Stream0, Head1, Stream1),
            union_helper(Left0, {min_elem(A, X), B}, Right0, Limit, Head1,
                Stream1, Left, Head, Stream)
        else if
            Limit = yes(LimitValue),
            Y >= LimitValue
        then
            Left = Left0,
            Head = yes({min_elem(A, X), Y}),
            Stream = Stream0
        else
            union_2(Right0, Limit, yes({min_elem(A, X), Y}), Stream0,
                Right1, Head1, Stream1),
            Left = reroot(Left0, Right1),
            Head = Head1,
            Stream = Stream1
        )
    ).

%---------------------%

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

%---------------------%

intersect(SetA, SetB) = inter(SetA, SetB).

intersect(SetA, SetB, inter(SetA, SetB)).

:- func inter(diet(T), diet(T)) = diet(T) <= diet_element(T).

inter(Input, Stream0) = Result :-
    ( if int_gt(height(Stream0), height(Input)) then
        disable_warning [suspicious_recursion] (
            Result = inter(Stream0, Input)
        )
    else
        (
            Stream0 = empty,
            Result = empty
        ;
            Stream0 = node(_, _, _, _),
            take_min_iter(Stream0, Head, Stream),
            inter_2(Input, yes(Head), Stream, Result, _, _)
        )
    ).

:- pred inter_2(diet(T)::in, maybe({T, T})::in, diet(T)::in,
    diet(T)::out, maybe({T, T})::out, diet(T)::out) is det <= diet_element(T).

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
            ( if X < A then
                inter_2(Left0, Head0, Stream0, Left1, Head1, Stream1)
            else
                Left1 = empty,
                Head1 = Head0,
                Stream1 = Stream0
            ),
            inter_help({A, B}, Right0, Left1, Head1, Stream1,
                Result, Head, Stream)
        )
    ).

:- pred inter_help({T, T}::in, diet(T)::in,
    diet(T)::in, maybe({T, T})::in, diet(T)::in,
    diet(T)::out, maybe({T, T})::out, diet(T)::out) is det <= diet_element(T).

inter_help({A, B}, Right0, Left0, Head0, Stream0,
        Result, Head, Stream) :-
    (
        Head0 = no,
        Result = Left0,
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, Y}),
        ( if Y < A then
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
        else if B < X then
            inter_2(Right0, Head0, Stream0, Right1, Head1, Stream1),
            Result = reroot(Left0, Right1),
            Head = Head1,
            Stream = Stream1
        else if Y >= safe_predecessor(Y, B) then
            inter_2(Right0, Head0, Stream0, Right1, Head1, Stream1),
            Result = join({max_elem(X, A), min_elem(Y, B)}, Left0, Right1),
            Head = Head1,
            Stream = Stream1
        else
            Left1 = do_insert({max_elem(X, A), Y}, Left0),
            inter_help({successor(Y), B}, Right0, Left1, Head0, Stream0,
                Result, Head, Stream)
        )
    ).

%---------------------%

intersect_list(Sets) = Set :-
    intersect_list(Sets, Set).

intersect_list([], init).
intersect_list([Set0 | Sets], Set) :-
    foldl(intersect, Sets, Set0, Set).

%---------------------%

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

:- pred diff(diet(T)::in, maybe(interval(T))::in, diet(T)::in,
    diet(T)::out, maybe(interval(T))::out, diet(T)::out) is det
    <= diet_element(T).

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
        ( if X < A then
            diff(Left0, Head0, Stream0, Left1, Head1, Stream1)
        else
            Left1 = Left0,
            Head1 = Head0,
            Stream1 = Stream0
        ),
        diff_helper({A, B}, Right0, Left1, Head1, Stream1,
            Output, Head, Stream)
    ).

:- pred diff_helper(interval(T)::in, diet(T)::in, diet(T)::in,
    maybe(interval(T))::in, diet(T)::in, diet(T)::out, maybe(interval(T))::out,
    diet(T)::out) is det <= diet_element(T).

diff_helper({A, B}, Right0, Left0, Head0, Stream0,
        Output, Head, Stream) :-
    (
        Head0 = no,
        Output = join({A, B}, Left0, Right0),
        Head = no,
        Stream = empty
    ;
        Head0 = yes({X, Y}),
        ( if Y < A then
            take_min_iter2(Stream0, Head1, Stream1),
            diff_helper({A, B}, Right0, Left0, Head1, Stream1,
                Output, Head, Stream)
        else if B < X then
            diff(Right0, Head0, Stream0, Right1, Head, Stream),
            Output = join({A, B}, Left0, Right1)
        else if A < X then
            Left1 = do_insert({A, predecessor(X)}, Left0),
            diff_helper({X, B}, Right0, Left1, Head0, Stream0,
                Output, Head, Stream)
        else if Y < B then
            take_min_iter2(Stream0, Head1, Stream1),
            diff_helper({successor(Y), B}, Right0, Left0, Head1, Stream1,
                Output, Head, Stream)
        else
            diff(Right0, Head0, Stream0, Right1, Head, Stream),
            Output = reroot(Left0, Right1)
        )
    ).

%---------------------------------------------------------------------------%

split(X, Set, Lesser, IsPresent, Greater) :-
    (
        Set = empty,
        IsPresent = no,
        Lesser = empty,
        Greater = empty
    ;
        Set = node({A, B}, _, L, R),
        ( if X < A then
            split(X, L, Lesser, IsPresent, RL),
            Greater = join({A, B}, RL, R)
        else if B < X then
            split(X, R, LR, IsPresent, Greater),
            Lesser = join({A, B}, L, LR)
        else
            IsPresent = yes,
            ( if X = A then
                Lesser = L
            else
                Lesser = do_insert({A, predecessor(X)}, L)
            ),
            ( if X = B then
                Greater = R
            else
                Greater = do_insert({successor(X), B}, R)
            )
        )
    ).

%---------------------%

divide(Pred, Set, TrueSet, FalseSet) :-
    % Can do better.
    foldl2(divide_2(Pred), Set, init, TrueSet, init, FalseSet).

:- pred divide_2(pred(T), T, diet(T), diet(T), diet(T), diet(T))
    <= diet_element(T).
:- mode divide_2(pred(in) is semidet, in, in, out, in, out) is det.

divide_2(Pred, Elem, !TrueSet, !FalseSet) :-
    ( if Pred(Elem) then
        insert(Elem, !TrueSet)
    else
        insert(Elem, !FalseSet)
    ).

%---------------------%

divide_by_set(DivideBySet, Set, InPart, OutPart) :-
    intersect(Set, DivideBySet, InPart),
    difference(Set, DivideBySet, OutPart).

%---------------------------------------------------------------------------%

list_to_set(List) = Set :-
    list_to_set(List, Set).

list_to_set(List, Set) :-
    foldl(insert, List, init, Set).

from_list(List) = Set :-
    list_to_set(List, Set).

from_list(List, Set) :-
    list_to_set(List, Set).

from_interval_list(List, Set) :-
    list.foldl(insert_interval, List, init, Set).

sorted_list_to_set(List) = Set :-
    sorted_list_to_set(List, Set).

sorted_list_to_set(List, Set) :-
    list_to_set(List, Set).

%---------------------------------------------------------------------------%

to_sorted_list(T) = List :-
    to_sorted_list(T, List).

to_sorted_list(T, List) :-
    foldr(list.cons, T, [], List).

to_sorted_interval_list(Set, List) :-
    foldr_intervals(cons_interval, Set, [], List).

:- pred cons_interval(T::in, T::in, list({T, T})::in, list({T, T})::out)
    is det.

cons_interval(X, Y, L, [{X, Y} | L]).

%---------------------------------------------------------------------------%

count(T) = Count :-
    count(T, 0, Count).

:- pred count(diet(T)::in, int::in, int::out) is det <= enum(T).

count(T, Acc0, Acc) :-
    (
        T = empty,
        Acc = Acc0
    ;
        T = node({X, Y}, _, L, R),

        Acc1 = Acc0 + (to_int(Y) - to_int(X)) + 1,
        count(L, Acc1, Acc2),
        count(R, Acc2, Acc)
    ).

%---------------------------------------------------------------------------%

all_true(P, Set) :-
    (
        Set = empty
    ;
        Set = node({X, Y}, _, L, R),
        all_true(P, L),
        all_true_interval(P, X, Y),
        all_true(P, R)
    ).

:- pred all_true_interval(pred(T)::in(pred(in) is semidet), T::in, T::in)
    is semidet <= diet_element(T).

all_true_interval(P, Lo, Hi) :-
    ( if Lo =< Hi then
        P(Lo),
        all_true_interval(P, successor(Lo), Hi)
    else
        true
    ).

%---------------------%

filter(Pred, Set) = TrueSet :-
    divide(Pred, Set, TrueSet, _FalseSet).

filter(Pred, Set, TrueSet, FalseSet) :-
    divide(Pred, Set, TrueSet, FalseSet).

%---------------------%

foldl_intervals(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl_intervals(P, L, !Acc),
        P(X, Y, !Acc),
        foldl_intervals(P, R, !Acc)
    ).

%---------------------%

foldr_intervals(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldr_intervals(P, R, !Acc),
        P(X, Y, !Acc),
        foldr_intervals(P, L, !Acc)
    ).

%---------------------%

foldl(F, Set, Acc0) = Acc :-
    P = (pred(E::in, PAcc0::in, PAcc::out) is det :-
        PAcc = F(E, PAcc0)
    ),
    foldl(P, Set, Acc0, Acc).

%---------------------%

foldl(P, T, !Acc) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl(P, L, !Acc),
        foldl_2(P, X, Y, !Acc),
        foldl(P, R, !Acc)
    ).

:- pred foldl_2(pred(T, Acc, Acc), T, T, Acc, Acc) <= diet_element(T).
:- mode foldl_2(pred(in, in, out) is det, in, in, in, out) is det.
:- mode foldl_2(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode foldl_2(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode foldl_2(pred(in, in, out) is semidet, in, in, in, out) is semidet.
:- mode foldl_2(pred(in, mdi, muo) is semidet, in, in, mdi, muo) is semidet.
:- mode foldl_2(pred(in, di, uo) is semidet, in, in, di, uo) is semidet.

foldl_2(P, Lo, Hi, !Acc) :-
    ( if Lo =< Hi then
        P(Lo, !Acc),
        foldl_2(P, successor(Lo), Hi, !Acc)
    else
        true
    ).

%---------------------%

foldl2(P, T, !Acc1, !Acc2) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl2(P, L, !Acc1, !Acc2),
        fold_up2(P, X, Y, !Acc1, !Acc2),
        foldl2(P, R, !Acc1, !Acc2)
    ).

:- pred fold_up2(pred(T, Acc1, Acc1, Acc2, Acc2), T, T,
    Acc1, Acc1, Acc2, Acc2) <= diet_element(T).
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
    ( if Lo =< Hi then
        P(Lo, !A, !B),
        fold_up2(P, successor(Lo), Hi, !A, !B)
    else
        true
    ).

%---------------------%

foldl3(P, T, !Acc1, !Acc2, !Acc3) :-
    (
        T = empty
    ;
        T = node({X, Y}, _, L, R),
        foldl3(P, L, !Acc1, !Acc2, !Acc3),
        fold_up3(P, X, Y, !Acc1, !Acc2, !Acc3),
        foldl3(P, R, !Acc1, !Acc2, !Acc3)
    ).

:- pred fold_up3(pred(T, Acc1, Acc1, Acc2, Acc2, Acc3, Acc3), T, T,
    Acc1, Acc1, Acc2, Acc2, Acc3, Acc3) <= diet_element(T).
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
    ( if Lo =< Hi then
        P(Lo, !A, !B, !C),
        fold_up3(P, successor(Lo), Hi, !A, !B, !C)
    else
        true
    ).

%---------------------%

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
    T, T, A, A, B, B, C, C, D, D) <= diet_element(T).
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
    ( if Lo =< Hi then
        P(Lo, !A, !B, !C, !D),
        fold_up4(P, successor(Lo), Hi, !A, !B, !C, !D)
    else
        true
    ).

%---------------------%

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
    T, T, A, A, B, B, C, C, D, D, E, E) <= diet_element(T).
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
    ( if Lo =< Hi then
        P(Lo, !A, !B, !C, !D, !E),
        fold_up5(P, successor(Lo), Hi, !A, !B, !C, !D, !E)
    else
        true
    ).

%---------------------%

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

:- pred fold_down(pred(T, A, A), T, T, A, A) <= diet_element(T).
:- mode fold_down(pred(in, in, out) is det, in, in, in, out) is det.
:- mode fold_down(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode fold_down(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode fold_down(pred(in, in, out) is semidet, in, in, in, out) is semidet.
:- mode fold_down(pred(in, mdi, muo) is semidet, in, in, mdi, muo) is semidet.
:- mode fold_down(pred(in, di, uo) is semidet, in, in, di, uo) is semidet.

fold_down(P, Lo, Hi, !A) :-
    ( if Lo =< Hi then
        P(Hi, !A),
        fold_down(P, Lo, predecessor(Hi), !A)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module diet.
%---------------------------------------------------------------------------%
