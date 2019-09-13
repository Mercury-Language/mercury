%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: set_tree234.m.
% Author: zs.
% Stability: high.
%
% This module implements sets using 2-3-4 trees.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module set_tree234.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type set_tree234(_T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % `init = Set' is true iff `Set' is an empty set.
    %
:- func init = set_tree234(T).

    % `singleton_set(Elem, Set)' is true iff `Set' is the set containing just
    % the single element `Elem'.
    %
:- pred singleton_set(T, set_tree234(T)).
:- mode singleton_set(in, out) is det.
:- mode singleton_set(out, in) is semidet.

:- func make_singleton_set(T) = set_tree234(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

    % `empty(Set)' is true iff `Set' is an empty set.
    % `is_empty' is a synonym for `empty'.
    %
:- pred empty(set_tree234(_T)::in) is semidet.
:- pred is_empty(set_tree234(_T)::in) is semidet.
:- pragma obsolete(empty/1, [is_empty/1]).

    % `non_empty(Set)' is true iff `Set' is not an empty set.
    % `is_non_empty' is a synonym for `non_empty'.
    %
:- pred non_empty(set_tree234(T)::in) is semidet.
:- pred is_non_empty(set_tree234(T)::in) is semidet.
:- pragma obsolete(non_empty/1, [is_non_empty/1]).

:- pred is_singleton(set_tree234(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred member(T, set_tree234(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `is_member(Set, X, Result)' returns `Result = yes' iff
    % `X' is a member of `Set'.
    %
:- func is_member(set_tree234(T), T) = bool.
:- pred is_member(set_tree234(T)::in, T::in, bool::out) is det.

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pred contains(set_tree234(T)::in, T::in) is semidet.

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % `insert(X, Set0, Set)' is true iff `Set' is the union of `Set0' and the
    % set containing only `X'.
    %
:- func insert(T, set_tree234(T)) = set_tree234(T).
:- pred insert(T::in, set_tree234(T)::in, set_tree234(T)::out) is det.

    % `insert_new(X, Set0, Set)' is true iff `Set0' does not contain `X', while
    % `Set' is the union of `Set0' and the set containing only `X'.
    %
:- pred insert_new(T::in, set_tree234(T)::in, set_tree234(T)::out) is semidet.

    % `insert_list(Xs, Set0, Set)' is true iff `Set' is the union of `Set0' and
    % the set containing only the members of `Xs'.
    %
:- func insert_list(list(T), set_tree234(T)) = set_tree234(T).
:- pred insert_list(list(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % `delete(X, Set0, Set)' is true iff `Set' is the relative complement of
    % `Set0' and the set containing only `X', i.e.  if `Set' is the set which
    % contains all the elements of `Set0' except `X'.
    %
:- func delete(T, set_tree234(T)) = set_tree234(T).
:- pred delete(T::in, set_tree234(T)::in, set_tree234(T)::out) is det.

    % `delete_list(Xs, Set0, Set)' is true iff `Set' is the relative complement
    % of `Set0' and the set containing only the members of `Xs'.
    %
:- func delete_list(list(T), set_tree234(T)) = set_tree234(T).
:- pred delete_list(list(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % `remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
    % The det_remove version throws an exception instead of failing.
    %
:- pred remove(T::in, set_tree234(T)::in, set_tree234(T)::out) is semidet.
:- pred det_remove(T::in, set_tree234(T)::in, set_tree234(T)::out) is det.

    % `remove_list(Xs, Set0, Set)' is true iff Xs does not contain any
    % duplicates, `Set0' contains every member of `Xs', and `Set' is the
    % relative complement of `Set0' and the set containing only the members of
    % `Xs'.
    %
    % The det_remove_list version throws an exception instead of failing.
    %
:- pred remove_list(list(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is semidet.
:- pred det_remove_list(list(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % `remove_least(X, Set0, Set)' is true iff `X' is the least element in
    % `Set0', and `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred remove_least(T::out, set_tree234(T)::in, set_tree234(T)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB' contain the same
    % elements.
    %
:- pred equal(set_tree234(T)::in, set_tree234(T)::in) is semidet.

    % `subset(SetA, SetB)' is true iff `SetA' is a subset of `SetB'.
    %
:- pred subset(set_tree234(T)::in, set_tree234(T)::in) is semidet.

    % `superset(SetA, SetB)' is true iff `SetA' is a superset of `SetB'.
    %
:- pred superset(set_tree234(T)::in, set_tree234(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % `union(SetA, SetB) = Set' is true iff `Set' is the union of `SetA' and
    % `SetB'.
    %
:- func union(set_tree234(T), set_tree234(T)) = set_tree234(T).
:- pred union(set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % `union_list(A, B)' is true iff `B' is the union of all the sets in `A'
    %
:- func union_list(list(set_tree234(T))) = set_tree234(T).
:- pred union_list(list(set_tree234(T))::in, set_tree234(T)::out) is det.

    % `power_union(A) = B' is true iff `B' is the union of
    % all the sets in `A'
    %
:- func power_union(set_tree234(set_tree234(T))) = set_tree234(T).
:- pred power_union(set_tree234(set_tree234(T))::in, set_tree234(T)::out)
    is det.

    % `intersect(SetA, SetB) = Set' is true iff `Set' is the intersection of
    % `SetA' and `SetB'.
    %
:- func intersect(set_tree234(T), set_tree234(T)) = set_tree234(T).
:- pred intersect(set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % `intersect_list(A, B)' is true iff `B' is the intersection of all the
    % sets in `A'.
    %
:- func intersect_list(list(set_tree234(T))) = set_tree234(T).
:- pred intersect_list(list(set_tree234(T))::in, set_tree234(T)::out) is det.

    % `power_intersect(A, B)' is true iff `B' is the intersection of all the
    % sets in `A'.
    %
:- func power_intersect(set_tree234(set_tree234(T))) = set_tree234(T).
:- pred power_intersect(set_tree234(set_tree234(T))::in, set_tree234(T)::out)
    is det.

    % `difference(SetA, SetB, Set)' is true iff `Set' is the set containing all
    % the elements of `SetA' except those that occur in `SetB'.
    %
:- func difference(set_tree234(T), set_tree234(T)) = set_tree234(T).
:- pred difference(set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % intersection_and_differences(SetA, SetB, InAandB, OnlyInA, OnlyInB):
    % Given SetA and SetB, return the elements that occur in both sets,
    % and those that occur only in one or the other.
    %
:- pred intersection_and_differences(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out, set_tree234(T)::out, set_tree234(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in, set_tree234(T)::out, set_tree234(T)::out) is det.

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in
    % DivideBySet; OutPart consists of those elements of which are
    % not in DivideBySet.
    %
:- pred divide_by_set(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out, set_tree234(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % `list_to_set(List) = Set' is true iff `Set' is the set containing
    % only the members of `List'.
    %
:- func list_to_set(list(T)) = set_tree234(T).
:- pred list_to_set(list(T)::in, set_tree234(T)::out) is det.

:- func from_list(list(T)) = set_tree234(T).
:- pred from_list(list(T)::in, set_tree234(T)::out) is det.

    % `sorted_list_to_set(List) = Set' is true iff `Set' is the set
    % containing only the members of `List'. `List' must be sorted
    % in ascending order and must not contain duplicates.
    %
:- func sorted_list_to_set(list(T)) = set_tree234(T).
:- pred sorted_list_to_set(list(T)::in, set_tree234(T)::out) is det.

    % `rev_sorted_list_to_set(List) = Set' is true iff `Set' is the set
    % containing only the members of `List'. `List' must be sorted
    % in descending order and must not contain duplicates.
    %
:- func rev_sorted_list_to_set(list(T)) = set_tree234(T).
:- pred rev_sorted_list_to_set(list(T)::in, set_tree234(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % `to_sorted_list(Set) = List' is true iff `List' is the list of all the
    % members of `Set', in sorted order.
    %
:- func to_sorted_list(set_tree234(T)) = list(T).
:- pred to_sorted_list(set_tree234(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting between different kinds of sets.
%

    % `from_set(Set)' returns a set_tree234 containing only
    % the members of `Set'. Takes O(card(Set)) time and space.
    %
:- func from_set(set.set(T)) = set_tree234(T).

    % `to_sorted_list(Set)' returns a set.set containing all the members of
    % `Set', in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_set(set_tree234(T)) = set.set(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

    % `count(Set, Count)' is true iff `Set' has `Count' elements.
    %
:- func count(set_tree234(T)) = int.

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds for all the
    % elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in) is semidet.

    % Return the set of items for which the predicate succeeds.
    %
:- func filter(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in) = (set_tree234(T)::out) is det.
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in, set_tree234(T)::out) is det.

    % Return the set of items for which the predicate succeeds,
    % and the set for which it fails.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in, set_tree234(T)::out, set_tree234(T)::out) is det.

:- func filter_map(func(T1) = T2, set_tree234(T1)) = set_tree234(T2).
:- mode filter_map(func(in) = out is semidet, in) = out is det.
:- pred filter_map(pred(T1, T2)::in(pred(in, out) is semidet),
    set_tree234(T1)::in, set_tree234(T2)::out) is det.

:- func map(func(T1) = T2, set_tree234(T1)) = set_tree234(T2).
:- pred map(pred(T1, T2)::in(pred(in, out) is det),
    set_tree234(T1)::in, set_tree234(T2)::out) is det.

:- func fold(func(T1, T2) = T2, set_tree234(T1), T2) = T2.
:- pred fold(pred(T1, T2, T2), set_tree234(T1), T2, T2).
:- mode fold(pred(in, in, out) is det, in, in, out) is det.
:- mode fold(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- func foldl(func(T1, T2) = T2, set_tree234(T1), T2) = T2.
:- pred foldl(pred(T1, T2, T2), set_tree234(T1), T2, T2).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out)
    is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo)
    is semidet.

:- pred fold2(pred(T1, T2, T2, T3, T3), set_tree234(T1),
    T2, T2, T3, T3).
:- mode fold2(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode fold2(pred(in, in, out, mdi, muo) is det, in,
    in, out, mdi, muo) is det.
:- mode fold2(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode fold2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode fold2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode fold2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

:- pred foldl2(pred(T1, T2, T2, T3, T3), set_tree234(T1),
    T2, T2, T3, T3).
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

:- pred fold3(
    pred(T1, T2, T2, T3, T3, T4, T4), set_tree234(T1),
    T2, T2, T3, T3, T4, T4).
:- mode fold3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode fold3(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode fold3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode fold3(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode fold3(pred(in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode fold3(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.

:- pred foldl3(
    pred(T1, T2, T2, T3, T3, T4, T4), set_tree234(T1),
    T2, T2, T3, T3, T4, T4).
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

:- pred fold4(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), set_tree234(T1),
    T2, T2, T3, T3, T4, T4, T5, T5).
:- mode fold4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode fold4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode fold4(
    pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode fold4(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold4(
    pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl4(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), set_tree234(T1),
    T2, T2, T3, T3, T4, T4, T5, T5).
:- mode foldl4(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred fold5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_tree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_tree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
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

:- pred fold6(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_tree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
:- mode fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl6(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_tree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
:- mode foldl6(
    pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl6(
    pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl6(
    pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl6(
    pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl6(
    pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl6(
    pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.  % for var/1.

:- pragma type_spec(sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(sorted_list_to_set/2, T = var(_)).
:- pragma type_spec(rev_sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(rev_sorted_list_to_set/2, T = var(_)).
:- pragma type_spec(do_from_sorted_list/6, E = var(_)).
:- pragma type_spec(do_from_rev_sorted_list/6, E = var(_)).
:- pragma type_spec(rev_sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(contains(in, in), T = var(_)).
:- pragma type_spec(insert/3, T = var(_)).
:- pragma type_spec(insert_list/3, T = var(_)).
:- pragma type_spec(union/2, T = var(_)).
:- pragma type_spec(union/3, T = var(_)).
:- pragma type_spec(intersect/2, T = var(_)).
:- pragma type_spec(intersect/3, T = var(_)).
:- pragma type_spec(difference/2, T = var(_)).
:- pragma type_spec(difference/3, T = var(_)).

:- type set_tree234(T)
    --->    empty
    ;       two(T, set_tree234(T), set_tree234(T))
    ;       three(T, T, set_tree234(T), set_tree234(T), set_tree234(T))
    ;       four(T, T, T, set_tree234(T), set_tree234(T),
                set_tree234(T), set_tree234(T)).

:- inst two(E, T) for set_tree234/1
    --->    two(E, T, T).
:- inst three(E, T) for set_tree234/1
    --->    three(E, E, T, T, T).
:- inst four(E, T) for set_tree234/1
    --->    four(E, E, E, T, T, T, T).

:- mode out_two == out(two(ground, ground)).
:- mode in_two  == in(two(ground, ground)).
:- mode in_three  == in(three(ground, ground)).
:- mode in_four  == in(four(ground, ground)).

% XXX
% :- mode uo_two  == out(uniq_two(unique, unique)).
% :- mode suo_two == out(uniq_two(ground, uniq_tree234_gg)).
%
% :- mode di_two  == di(uniq_two(unique, unique)).
% :- mode sdi_two == di(uniq_two(ground, uniq_tree234_gg)).
%
% :- mode di_three  == di(uniq_three(unique, unique)).
% :- mode sdi_three == di(uniq_three(ground, uniq_tree234_gg)).
%
% :- mode di_four  == di(uniq_four(unique, unique)).
% :- mode sdi_four == di(uniq_four(ground, uniq_tree234_gg)).

% :- inst uniq_set_tree234(T) == unique(
%     (
%         empty
%     ;
%         two(T, uniq_set_tree234(T), uniq_set_tree234(T))
%     ;
%         three(T, T, uniq_set_tree234(T), uniq_set_tree234(T),
%             uniq_set_tree234(T))
%     ;
%         four(T, T, T, uniq_set_tree234(T), uniq_set_tree234(T),
%             uniq_set_tree234(T), uniq_set_tree234(T))
%     )).
%
% :- inst uniq_set_tree234_gg == unique(
%     (
%         empty
%     ;
%         two(ground, ground, uniq_set_tree234_gg, uniq_set_tree234_gg)
%     ;
%         three(ground, ground, ground, ground,
%             uniq_set_tree234_gg, uniq_set_tree234_gg,
%             uniq_set_tree234_gg)
%     ;
%         four(ground, ground, ground, ground, ground, ground,
%             uniq_set_tree234_gg, uniq_set_tree234_gg,
%             uniq_set_tree234_gg, uniq_set_tree234_gg)
%     )).
%
% :- mode di_set_tree234(T) == uniq_set_tree234(T) >> dead.
% :- mode di_set_tree234    == uniq_set_tree234(ground) >> dead.
% :- mode uo_set_tree234(T) == free >> uniq_set_tree234(T).
% :- mode uo_set_tree234    == free >> uniq_set_tree234(ground).

%---------------------------------------------------------------------------%

init = empty.

singleton_set(X, two(X, empty, empty)).

make_singleton_set(X) = two(X, empty, empty).

%---------------------------------------------------------------------------%

empty(empty).

is_empty(empty).

non_empty(two(_, _, _)).
non_empty(three(_, _, _, _, _)).
non_empty(four(_, _, _, _, _, _, _)).

is_non_empty(two(_, _, _)).
is_non_empty(three(_, _, _, _, _)).
is_non_empty(four(_, _, _, _, _, _, _)).

is_singleton(two(X, empty, empty), X).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(member/2).

member(Element::out, Set::in) :-
    all_members(Set, Element).
member(Element::in, Set::in) :-
    is_member(Set, Element) = yes.

:- pred all_members(set_tree234(T)::in, T::out) is nondet.

all_members(empty, _) :- fail.
all_members(two(E0, T0, T1), E) :-
    (
        E = E0
    ;
        all_members(T0, E)
    ;
        all_members(T1, E)
    ).
all_members(three(E0, E1, T0, T1, T2), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        all_members(T0, E)
    ;
        all_members(T1, E)
    ;
        all_members(T2, E)
    ).
all_members(four(E0, E1, E2, T0, T1, T2, T3), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        E = E2
    ;
        all_members(T0, E)
    ;
        all_members(T1, E)
    ;
        all_members(T2, E)
    ;
        all_members(T3, E)
    ).

is_member(T, E) = R :-
    is_member(T, E, R).

is_member(T, E, R) :-
    (
        T = empty,
        R = no
    ;
        T = two(E0, T0, T1),
        compare(Result, E, E0),
        (
            Result = (<),
            is_member(T0, E, R)
        ;
            Result = (=),
            R = yes
        ;
            Result = (>),
            is_member(T1, E, R)
        )
    ;
        T = three(E0, E1, T0, T1, T2),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            is_member(T0, E, R)
        ;
            Result0 = (=),
            R = yes
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                is_member(T1, E, R)
            ;
                Result1 = (=),
                R = yes
            ;
                Result1 = (>),
                is_member(T2, E, R)
            )
        )
    ;
        T = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            compare(Result0, E, E0),
            (
                Result0 = (<),
                is_member(T0, E, R)
            ;
                Result0 = (=),
                R = yes
            ;
                Result0 = (>),
                is_member(T1, E, R)
            )
        ;
            Result1 = (=),
            R = yes
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                is_member(T2, E, R)
            ;
                Result2 = (=),
                R = yes
            ;
                Result2 = (>),
                is_member(T3, E, R)
            )
        )
    ).

contains(T, E) :-
    is_member(T, E, yes).

%---------------------------------------------------------------------------%

insert(E, Tin) = Tout :-
    insert(E, Tin, Tout).

insert(E, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(E, empty, empty)
    ;
        Tin = two(_, _, _),
        insert2(E, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _),
        insert3(E, Tin, Tout)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            insert2(E, Sub0, NewSub0),
            Tout = two(E1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Tout = Tin
        ;
            Result1 = (>),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            insert2(E, Sub1, NewSub1),
            Tout = two(E1, Sub0, NewSub1)
        )
    ).

:- pred insert2(T::in, set_tree234(T)::in_two, set_tree234(T)::out) is det.
:- pragma type_spec(insert2(in, in_two, out), T = var(_)).

insert2(E, Tin, Tout) :-
    Tin = two(E0, T0, T1),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
    then
        compare(Result, E, E0),
        (
            Result = (<),
            Tout = three(E, E0, empty, empty, empty)
        ;
            Result = (=),
            Tout = Tin
        ;
            Result = (>),
            Tout = three(E0, E, empty, empty, empty)
        )
    else
        compare(Result, E, E0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                split_four(T0, MT0E, T00, T01),
                compare(Result1, E, MT0E),
                (
                    Result1 = (<),
                    insert2(E, T00, NewT00),
                    Tout = three(MT0E, E0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = three(MT0E, E0, T00, T01, T1)
                ;
                    Result1 = (>),
                    insert2(E, T01, NewT01),
                    Tout = three(MT0E, E0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _),
                insert3(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = two(_, _, _),
                insert2(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = empty,
                NewT0 = two(E, empty, empty),
                Tout = two(E0, NewT0, T1)
            )
        ;
            Result = (=),
            Tout = two(E, T0, T1)
        ;
            Result = (>),
            (
                T1 = four(_, _, _, _, _, _, _),
                split_four(T1, MT1E, T10, T11),
                compare(Result1, E, MT1E),
                (
                    Result1 = (<),
                    insert2(E, T10, NewT10),
                    Tout = three(E0, MT1E, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = three(E0, MT1E, T0, T10, T11)
                ;
                    Result1 = (>),
                    insert2(E, T11, NewT11),
                    Tout = three(E0, MT1E, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _),
                insert3(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = two(_, _, _),
                insert2(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(E, empty, empty),
                Tout = two(E0, T0, NewT1)
            )
        )
    ).

:- pred insert3(T::in, set_tree234(T)::in_three, set_tree234(T)::out) is det.
:- pragma type_spec(insert3(in, in_three, out), T = var(_)).

insert3(E, Tin, Tout) :-
    Tin = three(E0, E1, T0, T1, T2),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    then
        compare(Result0, E, E0),
        (
            Result0 = (<),
            Tout = four(E, E0, E1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            Tout = Tin
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                Tout = four(E0, E, E1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                Tout = Tin
            ;
                Result1 = (>),
                Tout = four(E0, E1, E, empty, empty, empty, empty)
            )
        )
    else
        compare(Result0, E, E0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                split_four(T0, MT0E, T00, T01),
                compare(ResultM, E, MT0E),
                (
                    ResultM = (<),
                    insert2(E, T00, NewT00),
                    Tout = four(MT0E, E0, E1, NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = four(MT0E, E0, E1, T00, T01, T1, T2)
                ;
                    ResultM = (>),
                    insert2(E, T01, NewT01),
                    Tout = four(MT0E, E0, E1, T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _),
                insert3(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _),
                insert2(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = empty,
                NewT0 = two(E, empty, empty),
                Tout = three(E0, E1, NewT0, T1, T2)
            )
        ;
            Result0 = (=),
            Tout = Tin
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                (
                    T1 = four(_, _, _, _, _, _, _),
                    split_four(T1, MT1E, T10, T11),
                    compare(ResultM, E, MT1E),
                    (
                        ResultM = (<),
                        insert2(E, T10, NewT10),
                        Tout = four(E0, MT1E, E1, T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        % The Tout we are returning does not have the same
                        % shape as Tin, but it contains the same elements.
                        Tout = four(E0, MT1E, E1, T0, T10, T11, T2)
                    ;
                        ResultM = (>),
                        insert2(E, T11, NewT11),
                        Tout = four(E0, MT1E, E1, T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _),
                    insert3(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _),
                    insert2(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = empty,
                    NewT1 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                Tout = Tin
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _),
                    split_four(T2, MT2E, T20, T21),
                    compare(ResultM, E, MT2E),
                    (
                        ResultM = (<),
                        insert2(E, T20, NewT20),
                        Tout = four(E0, E1, MT2E, T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        % The Tout we are returning does not have the same
                        % shape as Tin, but it contains the same elements.
                        Tout = four(E0, E1, MT2E, T0, T1, T20, T21)
                    ;
                        ResultM = (>),
                        insert2(E, T21, NewT21),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _),
                    insert3(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _),
                    insert2(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------%

insert_new(E, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(E, empty, empty)
    ;
        Tin = two(_, _, _),
        insert_new2(E, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _),
        insert_new3(E, Tin, Tout)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            insert_new2(E, Sub0, NewSub0),
            Tout = two(E1, NewSub0, Sub1)
        ;
            Result1 = (=),
            fail
        ;
            Result1 = (>),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            insert_new2(E, Sub1, NewSub1),
            Tout = two(E1, Sub0, NewSub1)
        )
    ).

:- pred insert_new2(T::in,
    set_tree234(T)::in_two, set_tree234(T)::out) is semidet.
:- pragma type_spec(insert_new2(in, in_two, out), T = var(_)).

insert_new2(E, Tin, Tout) :-
    Tin = two(E0, T0, T1),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
    then
        compare(Result, E, E0),
        (
            Result = (<),
            Tout = three(E, E0, empty, empty, empty)
        ;
            Result = (=),
            fail
        ;
            Result = (>),
            Tout = three(E0, E, empty, empty, empty)
        )
    else
        compare(Result, E, E0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                split_four(T0, MT0E, T00, T01),
                compare(Result1, E, MT0E),
                (
                    Result1 = (<),
                    insert_new2(E, T00, NewT00),
                    Tout = three(MT0E, E0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    insert_new2(E, T01, NewT01),
                    Tout = three(MT0E, E0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _),
                insert_new3(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = two(_, _, _),
                insert_new2(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = empty,
                NewT0 = two(E, empty, empty),
                Tout = two(E0, NewT0, T1)
            )
        ;
            Result = (=),
            fail
        ;
            Result = (>),
            (
                T1 = four(_, _, _, _, _, _, _),
                split_four(T1, MT1E, T10, T11),
                compare(Result1, E, MT1E),
                (
                    Result1 = (<),
                    insert_new2(E, T10, NewT10),
                    Tout = three(E0, MT1E, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    insert_new2(E, T11, NewT11),
                    Tout = three(E0, MT1E, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _),
                insert_new3(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = two(_, _, _),
                insert_new2(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(E, empty, empty),
                Tout = two(E0, T0, NewT1)
            )
        )
    ).

:- pred insert_new3(T::in,
    set_tree234(T)::in_three, set_tree234(T)::out) is semidet.
:- pragma type_spec(insert_new3(in, in_three, out), T = var(_)).

insert_new3(E, Tin, Tout) :-
    Tin = three(E0, E1, T0, T1, T2),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    then
        compare(Result0, E, E0),
        (
            Result0 = (<),
            Tout = four(E, E0, E1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            fail
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                Tout = four(E0, E, E1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                fail
            ;
                Result1 = (>),
                Tout = four(E0, E1, E, empty, empty, empty, empty)
            )
        )
    else
        compare(Result0, E, E0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                split_four(T0, MT0E, T00, T01),
                compare(ResultM, E, MT0E),
                (
                    ResultM = (<),
                    insert_new2(E, T00, NewT00),
                    Tout = four(MT0E, E0, E1, NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    fail
                ;
                    ResultM = (>),
                    insert_new2(E, T01, NewT01),
                    Tout = four(MT0E, E0, E1, T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _),
                insert_new3(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _),
                insert_new2(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = empty,
                NewT0 = two(E, empty, empty),
                Tout = three(E0, E1, NewT0, T1, T2)
            )
        ;
            Result0 = (=),
            fail
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                (
                    T1 = four(_, _, _, _, _, _, _),
                    split_four(T1, MT1E, T10, T11),
                    compare(ResultM, E, MT1E),
                    (
                        ResultM = (<),
                        insert_new2(E, T10, NewT10),
                        Tout = four(E0, MT1E, E1, T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        insert_new2(E, T11, NewT11),
                        Tout = four(E0, MT1E, E1, T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _),
                    insert_new3(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _),
                    insert_new2(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = empty,
                    NewT1 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                fail
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _),
                    split_four(T2, MT2E, T20, T21),
                    compare(ResultM, E, MT2E),
                    (
                        ResultM = (<),
                        insert_new2(E, T20, NewT20),
                        Tout = four(E0, E1, MT2E, T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        insert_new2(E, T21, NewT21),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _),
                    insert_new3(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _),
                    insert_new2(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, T1, NewT2)
                )
            )
        )
    ).

:- pred split_four(set_tree234(E)::in_four, E::out,
    set_tree234(E)::out_two, set_tree234(E)::out_two) is det.

split_four(Tin, MidE, Sub0, Sub1) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    Sub0 = two(E0, T0, T1),
    MidE = E1,
    Sub1 = two(E2, T2, T3).

%---------------------%

insert_list(Es, Set0) = Set :-
    insert_list(Es, Set0, Set).

insert_list([], !Set).
insert_list([E | Es], !Set) :-
    insert(E, !Set),
    insert_list(Es, !Set).

%---------------------%

delete(E, Tin) = Tout :-
    delete(E, Tin, Tout).

delete(E, Tin, Tout) :-
    delete_2(E, Tin, Tout, _).

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.
    %
:- pred delete_2(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is det.

delete_2(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        Tout = empty,
        RH = no
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            delete_2(E, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(E0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(E0, NewT0, T1),
                RH = no
            )
        ;
            Result0 = (=),
            ( if do_remove_least(T1, ST1E,  NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1E, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1E, T0, NewT1),
                    RH = no
                )
            else
                % T1 must be empty
                Tout = T0,
                RH = yes
            )
        ;
            Result0 = (>),
            delete_2(E, T1, NewT1, RHT1),
            (
                RHT1 = yes,
                fix_2node_t1(E0, T0, NewT1, Tout, RH)
            ;
                RHT1 = no,
                Tout = two(E0, T0, NewT1),
                RH = no
            )
        )
    ;
        Tin = three(E0, E1, T0, T1, T2),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            delete_2(E, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(E0, E1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(E0, E1, NewT0, T1, T2),
                RH = no
            )
        ;
            Result0 = (=),
            ( if do_remove_least(T1, ST1E, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1E, E1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1E, E1, T0, NewT1, T2),
                    RH = no
                )
            else
                % T1 must be empty
                Tout = two(E1, T0, T2),
                RH = no
            )
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                delete_2(E, T1, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_3node_t1(E0, E1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(E0, E1, T0, NewT1, T2),
                    RH = no
                )
            ;
                Result1 = (=),
                ( if do_remove_least(T2, ST2E, NewT2, RHT2) then
                    (
                        RHT2 = yes,
                        fix_3node_t2(E0, ST2E, T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(E0, ST2E, T0, T1, NewT2),
                        RH = no
                    )
                else
                    % T2 must be empty
                    Tout = two(E0, T0, T1),
                    RH = no
                )
            ;
                Result1 = (>),
                delete_2(E, T2, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_3node_t2(E0, E1, T0, T1, NewT2, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = three(E0, E1, T0, T1, NewT2),
                    RH = no
                )
            )
        )
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            compare(Result0, E, E0),
            (
                Result0 = (<),
                delete_2(E, T0, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_4node_t0(E0, E1, E2, NewT0, T1, T2, T3, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = four(E0, E1, E2, NewT0, T1, T2, T3),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if do_remove_least(T1, ST1E, NewT1, RHT1) then
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1E, E1, E2, T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1E, E1, E2, T0, NewT1, T2, T3),
                        RH = no
                    )
                else
                    % T1 must be empty
                    Tout = three(E1, E2, T0, T2, T3),
                    RH = no
                )
            ;
                Result0 = (>),
                delete_2(E, T1, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_4node_t1(E0, E1, E2, T0, NewT1, T2, T3, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = four(E0, E1, E2, T0, NewT1, T2, T3),
                    RH = no
                )
            )
        ;
            Result1 = (=),
            ( if do_remove_least(T2, ST2E, NewT2, RHT2) then
                (
                    RHT2 = yes,
                    fix_4node_t2(E0, ST2E, E2, T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(E0, ST2E, E2, T0, T1, NewT2, T3),
                    RH = no
                )
            else
                % T2 must be empty
                Tout = three(E0, E2, T0, T1, T3),
                RH = no
            )
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                delete_2(E, T2, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_4node_t2(E0, E1, E2, T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(E0, E1, E2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                Result2 = (=),
                ( if do_remove_least(T3, ST3E, NewT3, RHT3) then
                    (
                        RHT3 = yes,
                        fix_4node_t3(E0, E1, ST3E, T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(E0, E1, ST3E, T0, T1, T2, NewT3),
                        RH = no
                    )
                else
                    % T3 must be empty
                    Tout = three(E0, E1, T0, T1, T2),
                    RH = no
                )
            ;
                Result2 = (>),
                delete_2(E, T3, NewT3, RHT3),
                (
                    RHT3 = yes,
                    fix_4node_t3(E0, E1, E2, T0, T1, T2, NewT3, Tout, RH)
                ;
                    RHT3 = no,
                    Tout = four(E0, E1, E2, T0, T1, T2, NewT3),
                    RH = no
                )
            )
        )
    ).

%---------------------%

delete_list(SetA, SetB) = Set:-
    delete_list(SetA, SetB, Set).

delete_list([], !Set).
delete_list([E | Es], !Set) :-
    delete(E, !Set),
    delete_list(Es, !Set).

%---------------------%

remove(E, Tin, Tout) :-
    % We use the same algorithm as delete.
    remove_2(E, Tin, Tout, _).

:- pred remove_2(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is semidet.

remove_2(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            remove_2(E, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(E0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(E0, NewT0, T1),
                RH = no
            )
        ;
            Result0 = (=),
            ( if do_remove_least(T1, ST1E, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1E, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1E, T0, NewT1),
                    RH = no
                )
            else
                % T1 must be empty
                Tout = T0,
                RH = yes
            )
        ;
            Result0 = (>),
            remove_2(E, T1, NewT1, RHT1),
            (
                RHT1 = yes,
                fix_2node_t1(E0, T0, NewT1, Tout, RH)
            ;
                RHT1 = no,
                Tout = two(E0, T0, NewT1),
                RH = no
            )
        )
    ;
        Tin = three(E0, E1, T0, T1, T2),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            remove_2(E, T0, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(E0, E1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(E0, E1, NewT0, T1, T2),
                RH = no
            )
        ;
            Result0 = (=),
            ( if do_remove_least(T1, ST1E, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1E, E1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1E, E1, T0, NewT1, T2),
                    RH = no
                )
            else
                % T1 must be empty
                Tout = two(E1, T0, T2),
                RH = no
            )
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                remove_2(E, T1, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_3node_t1(E0, E1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(E0, E1, T0, NewT1, T2),
                    RH = no
                )
            ;
                Result1 = (=),
                ( if do_remove_least(T2, ST2E, NewT2, RHT2) then
                    (
                        RHT2 = yes,
                        fix_3node_t2(E0, ST2E, T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(E0, ST2E, T0, T1, NewT2),
                        RH = no
                    )
                else
                    % T2 must be empty
                    Tout = two(E0, T0, T1),
                    RH = no
                )
            ;
                Result1 = (>),
                remove_2(E, T2, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_3node_t2(E0, E1, T0, T1, NewT2, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = three(E0, E1, T0, T1, NewT2),
                    RH = no
                )
            )
        )
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            compare(Result0, E, E0),
            (
                Result0 = (<),
                remove_2(E, T0, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_4node_t0(E0, E1, E2, NewT0, T1, T2, T3, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = four(E0, E1, E2, NewT0, T1, T2, T3),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if do_remove_least(T1, ST1E, NewT1, RHT1) then
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1E, E1, E2, T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1E, E1, E2, T0, NewT1, T2, T3),
                        RH = no
                    )
                else
                    % T1 must be empty
                    Tout = three(E1, E2, T0, T2, T3),
                    RH = no
                )
            ;
                Result0 = (>),
                remove_2(E, T1, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_4node_t1(E0, E1, E2, T0, NewT1, T2, T3, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = four(E0, E1, E2, T0, NewT1, T2, T3),
                    RH = no
                )
            )
        ;
            Result1 = (=),
            ( if do_remove_least(T2, ST2E, NewT2, RHT2) then
                (
                    RHT2 = yes,
                    fix_4node_t2(E0, ST2E, E2, T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(E0, ST2E, E2, T0, T1, NewT2, T3),
                    RH = no
                )
            else
                % T2 must be empty
                Tout = three(E0, E2, T0, T1, T3),
                RH = no
            )
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                remove_2(E, T2, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_4node_t2(E0, E1, E2, T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(E0, E1, E2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                Result2 = (=),
                ( if do_remove_least(T3, ST3E, NewT3, RHT3) then
                    (
                        RHT3 = yes,
                        fix_4node_t3(E0, E1, ST3E, T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(E0, E1, ST3E, T0, T1, T2, NewT3),
                        RH = no
                    )
                else
                    % T3 must be empty
                    Tout = three(E0, E1, T0, T1, T2),
                    RH = no
                )
            ;
                Result2 = (>),
                remove_2(E, T3, NewT3, RHT3),
                (
                    RHT3 = yes,
                    fix_4node_t3(E0, E1, E2, T0, T1, T2, NewT3, Tout, RH)
                ;
                    RHT3 = no,
                    Tout = four(E0, E1, E2, T0, T1, T2, NewT3),
                    RH = no
                )
            )
        )
    ).

det_remove(X, !Set) :-
    ( if set_tree234.remove(X, !Set) then
        true
    else
        unexpected($pred, "remove failed")
    ).

remove_list([], !Set).
remove_list([E | Es], !Set) :-
    remove(E, !Set),
    remove_list(Es, !Set).

det_remove_list(List, !Set) :-
    ( if set_tree234.remove_list(List, !Set) then
        true
    else
        unexpected($pred, "remove_list failed")
    ).

%---------------------%

remove_least(E, Tin, Tout) :-
    % The algorithm we use similar to delete, except that we
    % always go down the left subtree.
    do_remove_least(Tin, E, Tout, _).

:- pred do_remove_least(set_tree234(E)::in, E::out,
    set_tree234(E)::out, bool::out) is semidet.

do_remove_least(Tin, E, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(E0, T0, T1),
        ( if T0 = empty then
            E = E0,
            Tout = T1,
            RH = yes
        else
            do_remove_least(T0, E, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(E0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(E0, NewT0, T1),
                RH = no
            )
        )
    ;
        Tin = three(E0, E1, T0, T1, T2),
        ( if T0 = empty then
            E = E0,
            Tout = two(E1, T1, T2),
            RH = no
        else
            do_remove_least(T0, E, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(E0, E1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(E0, E1, NewT0, T1, T2),
                RH = no
            )
        )
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        ( if T0 = empty then
            E = E0,
            Tout = three(E1, E2, T1, T2, T3),
            RH = no
        else
            do_remove_least(T0, E, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_4node_t0(E0, E1, E2, NewT0, T1, T2, T3, Tout, RH)
            ;
                RHT0 = no,
                Tout = four(E0, E1, E2, NewT0, T1, T2, T3),
                RH = no
            )
        )
    ).

%---------------------%
%
% The input to the following group of predicates are the components
% of a two-, three- or four-node in which the height of the indicated
% subtree is one less that it should be. If it is possible to increase
% the height of that subtree by moving into it elements from its
% neighboring subtrees, do so, and return the resulting tree with RH
% set to no. Otherwise, return a balanced tree whose height is reduced
% by one, with RH set to yes to indicate the reduced height.
%

:- pred fix_2node_t0(E::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::out, bool::out) is det.

fix_2node_t0(E0, T0, T1, Tout, RH) :-
    (
        % steal T1's leftmost subtree and combine it with T0
        T1 = four(E10, E11, E12, T10, T11, T12, T13),
        NewT1 = three(E11, E12, T11, T12, T13),
        Node = two(E0, T0, T10),
        Tout = two(E10, Node, NewT1),
        RH = no
    ;
        % steal T1's leftmost subtree and combine it with T0
        T1 = three(E10, E11, T10, T11, T12),
        NewT1 = two(E11, T11, T12),
        Node = two(E0, T0, T10),
        Tout = two(E10, Node, NewT1),
        RH = no
    ;
        % move T0 one level down and combine it with the subtrees of T1
        % this reduces the depth of the tree
        T1 = two(E10, T10, T11),
        Tout = three(E0, E10, T0, T10, T11),
        RH = yes
    ;
        T1 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = two(E0, T0, T1),
        % RH = yes
    ).

:- pred fix_2node_t1(E::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::out, bool::out) is det.

fix_2node_t1(E0, T0, T1, Tout, RH) :-
    (
        % steal T0's leftmost subtree and combine it with T1
        T0 = four(E00, E01, E02, T00, T01, T02, T03),
        NewT0 = three(E00, E01, T00, T01, T02),
        Node = two(E0, T03, T1),
        Tout = two(E02, NewT0, Node),
        RH = no
    ;
        % steal T0's leftmost subtree and combine it with T1
        T0 = three(E00, E01, T00, T01, T02),
        NewT0 = two(E00, T00, T01),
        Node = two(E0, T02, T1),
        Tout = two(E01, NewT0, Node),
        RH = no
    ;
        % move T1 one level down and combine it with the subtrees of T0
        % this reduces the depth of the tree
        T0 = two(E00, T00, T01),
        Tout = three(E00, E0, T00, T01, T1),
        RH = yes
    ;
        T0 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = two(E0, T0, T1),
        % RH = yes
    ).

:- pred fix_3node_t0(E::in, E::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_3node_t0(E0, E1, T0, T1, T2, Tout, RH) :-
    (
        % steal T1's leftmost subtree and combine it with T0
        T1 = four(E10, E11, E12, T10, T11, T12, T13),
        NewT1 = three(E11, E12, T11, T12, T13),
        Node = two(E0, T0, T10),
        Tout = three(E10, E1, Node, NewT1, T2),
        RH = no
    ;
        % steal T1's leftmost subtree and combine it with T0
        T1 = three(E10, E11, T10, T11, T12),
        NewT1 = two(E11, T11, T12),
        Node = two(E0, T0, T10),
        Tout = three(E10, E1, Node, NewT1, T2),
        RH = no
    ;
        % move T0 one level down to become the leftmost subtree of T1
        T1 = two(E10, T10, T11),
        NewT1 = three(E0, E10, T0, T10, T11),
        Tout = two(E1, NewT1, T2),
        RH = no
    ;
        T1 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = three(E0, E1, T0, T1, T2),
        % The heights of T1 and T2 are unchanged
        % RH = no
    ).

:- pred fix_3node_t1(E::in, E::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_3node_t1(E0, E1, T0, T1, T2, Tout, RH) :-
    (
        % steal T0's rightmost subtree and combine it with T1
        T0 = four(E00, E01, E02, T00, T01, T02, T03),
        NewT0 = three(E00, E01, T00, T01, T02),
        Node = two(E0, T03, T1),
        Tout = three(E02, E1, NewT0, Node, T2),
        RH = no
    ;
        % steal T0's rightmost subtree and combine it with T1
        T0 = three(E00, E01, T00, T01, T02),
        NewT0 = two(E00, T00, T01),
        Node = two(E0, T02, T1),
        Tout = three(E01, E1, NewT0, Node, T2),
        RH = no
    ;
        % move T1 one level down to become the rightmost subtree of T0
        T0 = two(E00, T00, T01),
        NewT0 = three(E00, E0, T00, T01, T1),
        Tout = two(E1, NewT0, T2),
        RH = no
    ;
        T0 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = three(E0, E1, T0, T1, T2),
        % The heights of T0 and T2 are unchanged
        % RH = no
    ).

:- pred fix_3node_t2(E::in, E::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_3node_t2(E0, E1, T0, T1, T2, Tout, RH) :-
    (
        % steal T1's rightmost subtree and combine it with T2
        T1 = four(E10, E11, E12, T10, T11, T12, T13),
        NewT1 = three(E10, E11, T10, T11, T12),
        Node = two(E1, T13, T2),
        Tout = three(E0, E12, T0, NewT1, Node),
        RH = no
    ;
        % steal T1's rightmost subtree and combine it with T2
        T1 = three(E10, E11, T10, T11, T12),
        NewT1 = two(E10, T10, T11),
        Node = two(E1, T12, T2),
        Tout = three(E0, E11, T0, NewT1, Node),
        RH = no
    ;
        % move T2 one level down to become the rightmost subtree of T1
        T1 = two(E10, T10, T11),
        NewT1 = three(E10, E1, T10, T11, T2),
        Tout = two(E0, T0, NewT1),
        RH = no
    ;
        T1 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = three(E0, E1, T0, T1, T2),
        % The heights of T0 and T1 are unchanged
        % RH = no
    ).

:- pred fix_4node_t0(E::in, E::in, E::in,
    set_tree234(E)::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_4node_t0(E0, E1, E2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T1's leftmost subtree and combine it with T0
        T1 = four(E10, E11, E12, T10, T11, T12, T13),
        NewT1 = three(E11, E12, T11, T12, T13),
        Node = two(E0, T0, T10),
        Tout = four(E10, E1, E2, Node, NewT1, T2, T3),
        RH = no
    ;
        % steal T1's leftmost subtree and combine it with T0
        T1 = three(E10, E11, T10, T11, T12),
        NewT1 = two(E11, T11, T12),
        Node = two(E0, T0, T10),
        Tout = four(E10, E1, E2, Node, NewT1, T2, T3),
        RH = no
    ;
        % move T0 one level down to become the leftmost subtree of T1
        T1 = two(E10, T10, T11),
        NewT1 = three(E0, E10, T0, T10, T11),
        Tout = three(E1, E2, NewT1, T2, T3),
        RH = no
    ;
        T1 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = four(E0, E1, E2, T0, T1, T2, T3),
        % The heights of T1, T2 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t1(E::in, E::in, E::in,
    set_tree234(E)::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_4node_t1(E0, E1, E2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T2's leftmost subtree and combine it with T1
        T2 = four(E20, E21, E22, T20, T21, T22, T23),
        NewT2 = three(E21, E22, T21, T22, T23),
        Node = two(E1, T1, T20),
        Tout = four(E0, E20, E2, T0, Node, NewT2, T3),
        RH = no
    ;
        % steal T2's leftmost subtree and combine it with T1
        T2 = three(E20, E21, T20, T21, T22),
        NewT2 = two(E21, T21, T22),
        Node = two(E1, T1, T20),
        Tout = four(E0, E20, E2, T0, Node, NewT2, T3),
        RH = no
    ;
        % move T1 one level down to become the leftmost subtree of T2
        T2 = two(E20, T20, T21),
        NewT2 = three(E1, E20, T1, T20, T21),
        Tout = three(E0, E2, T0, NewT2, T3),
        RH = no
    ;
        T2 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = four(E0, E1, E2, T0, T1, T2, T3),
        % The heights of T0, T2 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t2(E::in, E::in, E::in,
    set_tree234(E)::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_4node_t2(E0, E1, E2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T3's leftmost subtree and combine it with T2
        T3 = four(E30, E31, E32, T30, T31, T32, T33),
        NewT3 = three(E31, E32, T31, T32, T33),
        Node = two(E2, T2, T30),
        Tout = four(E0, E1, E30, T0, T1, Node, NewT3),
        RH = no
    ;
        % steal T3's leftmost subtree and combine it with T2
        T3 = three(E30, E31, T30, T31, T32),
        NewT3 = two(E31, T31, T32),
        Node = two(E2, T2, T30),
        Tout = four(E0, E1, E30, T0, T1, Node, NewT3),
        RH = no
    ;
        % move T2 one level down to become the leftmost subtree of T3
        T3 = two(E30, T30, T31),
        NewT3 = three(E2, E30, T2, T30, T31),
        Tout = three(E0, E1, T0, T1, NewT3),
        RH = no
    ;
        T3 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = four(E0, E1, E2, T0, T1, T2, T3),
        % The heights of T0, T1 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t3(E::in, E::in, E::in,
    set_tree234(E)::in, set_tree234(E)::in, set_tree234(E)::in,
    set_tree234(E)::in, set_tree234(E)::out, bool::out) is det.

fix_4node_t3(E0, E1, E2, T0, T1, T2, T3, Tout, RH) :-
    (
        % steal T2's rightmost subtree and combine it with T3
        T2 = four(E20, E21, E22, T20, T21, T22, T23),
        NewT2 = three(E20, E21, T20, T21, T22),
        Node = two(E2, T23, T3),
        Tout = four(E0, E1, E22, T0, T1, NewT2, Node),
        RH = no
    ;
        % steal T2's rightmost subtree and combine it with T3
        T2 = three(E20, E21, T20, T21, T22),
        NewT2 = two(E20, T20, T21),
        Node = two(E2, T22, T3),
        Tout = four(E0, E1, E21, T0, T1, NewT2, Node),
        RH = no
    ;
        % move T3 one level down to become the rightmost subtree of T2
        T2 = two(E20, T20, T21),
        NewT2 = three(E20, E2, T20, T21, T3),
        Tout = three(E0, E1, T0, T1, NewT2),
        RH = no
    ;
        T2 = empty,
        unexpected($pred, "unbalanced 234 tree")
        % Tout = four(E0, E1, E2, T0, T1, T2, T3),
        % The heights of T0, T1 and T2 are unchanged
        % RH = no
    ).

%---------------------------------------------------------------------------%

equal(SetA, SetB) :-
    to_sorted_list(SetA, ListA),
    to_sorted_list(SetB, ListB),
    ListA = ListB.

subset(empty, _Set).
subset(two(E, T0, T1), Set) :-
    subset(T0, Set),
    contains(Set, E),
    subset(T1, Set).
subset(three(E0, E1, T0, T1, T2), Set) :-
    subset(T0, Set),
    contains(Set, E0),
    subset(T1, Set),
    contains(Set, E1),
    subset(T2, Set).
subset(four(E0, E1, E2, T0, T1, T2, T3), Set) :-
    subset(T0, Set),
    contains(Set, E0),
    subset(T1, Set),
    contains(Set, E1),
    subset(T2, Set),
    contains(Set, E2),
    subset(T3, Set).

superset(SuperSet, Set) :-
    subset(Set, SuperSet).

%---------------------------------------------------------------------------%

union(SetA, SetB) = Set :-
    union(SetA, SetB, Set).

union(SetA, SetB, Set) :-
    % The amount of work that do_union has to do is proportional to the
    % number of elements in its first argument. We therefore want to pick
    % the smaller input set to be the first argument.
    %
    % We could count the number of arguments in both sets, but computing the
    % tree height is *much* faster, and almost as precise.
    height(SetA, HeightA),
    height(SetB, HeightB),
    ( if HeightA =< HeightB then
        do_union(SetA, SetB, Set)
    else
        do_union(SetB, SetA, Set)
    ).

:- pred do_union(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

do_union(empty, !Set).
do_union(two(E0, T0, T1), !Set) :-
    do_union(T0, !Set),
    insert(E0, !Set),
    do_union(T1, !Set).
do_union(three(E0, E1, T0, T1, T2), !Set) :-
    do_union(T0, !Set),
    insert(E0, !Set),
    do_union(T1, !Set),
    insert(E1, !Set),
    do_union(T2, !Set).
do_union(four(E0, E1, E2, T0, T1, T2, T3), !Set) :-
    do_union(T0, !Set),
    insert(E0, !Set),
    do_union(T1, !Set),
    insert(E1, !Set),
    do_union(T2, !Set),
    insert(E2, !Set),
    do_union(T3, !Set).

union_list(Sets) = Union :-
    union_list(Sets, Union).

union_list([], empty).
union_list([Set | Sets], Union) :-
    union_list(Sets, Union1),
    union(Set, Union1, Union).

power_union(Sets) = Union :-
    power_union(Sets, Union).

power_union(Sets, Union) :-
    power_union_2(Sets, empty, Union).

:- pred power_union_2(set_tree234(set_tree234(T))::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.

power_union_2(empty, !Union).
power_union_2(two(E0, T0, T1), !Union) :-
    power_union_2(T0, !Union),
    union(E0, !Union),
    power_union_2(T1, !Union).
power_union_2(three(E0, E1, T0, T1, T2), !Union) :-
    power_union_2(T0, !Union),
    union(E0, !Union),
    power_union_2(T1, !Union),
    union(E1, !Union),
    power_union_2(T2, !Union).
power_union_2(four(E0, E1, E2, T0, T1, T2, T3), !Union) :-
    power_union_2(T0, !Union),
    union(E0, !Union),
    power_union_2(T1, !Union),
    union(E1, !Union),
    power_union_2(T2, !Union),
    union(E2, !Union),
    power_union_2(T3, !Union).

%---------------------%

intersect(SetA, SetB) = Set :-
    intersect(SetA, SetB, Set).

intersect(SetA, SetB, Intersect) :-
    % The amount of work that do_intersect has to do is proportional to the
    % number of elements in its first argument. We therefore want to pick
    % the smaller input set to be the first argument.
    %
    % We could count the number of arguments in both sets, but computing the
    % tree height is *much* faster, and almost as precise.
    height(SetA, HeightA),
    height(SetB, HeightB),
    ( if HeightA =< HeightB then
        do_intersect(SetA, SetB, empty, Intersect)
    else
        do_intersect(SetB, SetA, empty, Intersect)
    ).

:- pred do_intersect(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.

do_intersect(empty, _SetB, !Intersect).
do_intersect(two(E0, T0, T1), SetB, !Intersect) :-
    do_intersect(T0, SetB, !Intersect),
    ( if contains(SetB, E0) then
        insert(E0, !Intersect)
    else
        true
    ),
    do_intersect(T1, SetB, !Intersect).
do_intersect(three(E0, E1, T0, T1, T2), SetB, !Intersect) :-
    do_intersect(T0, SetB, !Intersect),
    ( if contains(SetB, E0) then
        insert(E0, !Intersect)
    else
        true
    ),
    do_intersect(T1, SetB, !Intersect),
    ( if contains(SetB, E1) then
        insert(E1, !Intersect)
    else
        true
    ),
    do_intersect(T2, SetB, !Intersect).
do_intersect(four(E0, E1, E2, T0, T1, T2, T3), SetB, !Intersect) :-
    do_intersect(T0, SetB, !Intersect),
    ( if contains(SetB, E0) then
        insert(E0, !Intersect)
    else
        true
    ),
    do_intersect(T1, SetB, !Intersect),
    ( if contains(SetB, E1) then
        insert(E1, !Intersect)
    else
        true
    ),
    do_intersect(T2, SetB, !Intersect),
    ( if contains(SetB, E2) then
        insert(E2, !Intersect)
    else
        true
    ),
    do_intersect(T3, SetB, !Intersect).

intersect_list(Sets) = Intersect :-
    intersect_list(Sets, Intersect).

intersect_list([], empty).
intersect_list([Set | Sets], Intersect) :-
    Intersect = intersect_list_2(Set, Sets).

:- func intersect_list_2(set_tree234(T), list(set_tree234(T)))
    = set_tree234(T).

intersect_list_2(Set, []) = Set.
intersect_list_2(Set, [Head | Tail]) =
    ( if Set = empty then
        empty
    else
        intersect_list_2(intersect(Set, Head), Tail)
    ).

power_intersect(Sets) = Intersect :-
    power_intersect(Sets, Intersect).

power_intersect(Sets, Intersect) :-
    Intersect = intersect_list(to_sorted_list(Sets)).

%---------------------%

difference(SetA, SetB) = Diff :-
    difference(SetA, SetB, Diff).

difference(SetA, SetB, Diff) :-
    difference_2(SetB, SetA, Diff).

:- pred difference_2(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

difference_2(empty, !Set).
difference_2(two(E0, T0, T1), !Set) :-
    difference_2(T0, !Set),
    delete(E0, !Set),
    difference_2(T1, !Set).
difference_2(three(E0, E1, T0, T1, T2), !Set) :-
    difference_2(T0, !Set),
    delete(E0, !Set),
    difference_2(T1, !Set),
    delete(E1, !Set),
    difference_2(T2, !Set).
difference_2(four(E0, E1, E2, T0, T1, T2, T3), !Set) :-
    difference_2(T0, !Set),
    delete(E0, !Set),
    difference_2(T1, !Set),
    delete(E1, !Set),
    difference_2(T2, !Set),
    delete(E2, !Set),
    difference_2(T3, !Set).

%---------------------%

intersection_and_differences(SetA, SetB, InAandB, OnlyInA, OnlyInB) :-
    ListA = to_sorted_list(SetA),
    ListB = to_sorted_list(SetB),
    intersection_and_differences_loop(ListA, ListB,
        cord.init, CordInAandB,
        cord.init, CordOnlyInA,
        cord.init, CordOnlyInB),
    InAandB = sorted_list_to_set(cord.list(CordInAandB)),
    OnlyInA = sorted_list_to_set(cord.list(CordOnlyInA)),
    OnlyInB = sorted_list_to_set(cord.list(CordOnlyInB)).

:- pred intersection_and_differences_loop(list(T)::in, list(T)::in,
    cord(T)::in, cord(T)::out,
    cord(T)::in, cord(T)::out,
    cord(T)::in, cord(T)::out) is det.

intersection_and_differences_loop(As, Bs, !InAandB, !OnlyInA, !OnlyInB) :-
    (
        As = [],
        Bs = []
    ;
        As = [],
        Bs = [_ | _],
        !:OnlyInB = !.OnlyInB ++ cord.from_list(Bs)
    ;
        As = [_ | _],
        Bs = [],
        !:OnlyInA = !.OnlyInA ++ cord.from_list(As)
    ;
        As = [HeadA | TailAs],
        Bs = [HeadB | TailBs],
        compare(Cmp, HeadA, HeadB),
        (
            Cmp = (=),
            !:InAandB = cord.snoc(!.InAandB, HeadA),
            intersection_and_differences_loop(TailAs, TailBs,
                !InAandB, !OnlyInA, !OnlyInB)
        ;
            Cmp = (<),
            !:OnlyInA = cord.snoc(!.OnlyInA, HeadA),
            intersection_and_differences_loop(TailAs, Bs,
                !InAandB, !OnlyInA, !OnlyInB)
        ;
            Cmp = (>),
            !:OnlyInB = cord.snoc(!.OnlyInB, HeadB),
            intersection_and_differences_loop(As, TailBs,
                !InAandB, !OnlyInA, !OnlyInB)
        )
    ).

%---------------------------------------------------------------------------%

divide(Pred, Set, TrueSet, FalseSet) :-
    do_divide(Pred, Set, [], RevTrues, [], RevFalses),
    TrueSet = rev_sorted_list_to_set(RevTrues),
    FalseSet = rev_sorted_list_to_set(RevFalses).

:- pred do_divide(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in,
    list(T)::in, list(T)::out, list(T)::in, list(T)::out) is det.

do_divide(Pred, Tin, !RevTrues, !RevFalses) :-
    (
        Tin = empty
    ;
        Tin = two(E0, T0, T1),
        do_divide(Pred, T0, !RevTrues, !RevFalses),
        ( if Pred(E0) then
            !:RevTrues = [E0 | !.RevTrues]
        else
            !:RevFalses = [E0 | !.RevFalses]
        ),
        do_divide(Pred, T1, !RevTrues, !RevFalses)
    ;
        Tin = three(E0, E1, T0, T1, T2),
        do_divide(Pred, T0, !RevTrues, !RevFalses),
        ( if Pred(E0) then
            !:RevTrues = [E0 | !.RevTrues]
        else
            !:RevFalses = [E0 | !.RevFalses]
        ),
        do_divide(Pred, T1, !RevTrues, !RevFalses),
        ( if Pred(E1) then
            !:RevTrues = [E1 | !.RevTrues]
        else
            !:RevFalses = [E1 | !.RevFalses]
        ),
        do_divide(Pred, T2, !RevTrues, !RevFalses)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        do_divide(Pred, T0, !RevTrues, !RevFalses),
        ( if Pred(E0) then
            !:RevTrues = [E0 | !.RevTrues]
        else
            !:RevFalses = [E0 | !.RevFalses]
        ),
        do_divide(Pred, T1, !RevTrues, !RevFalses),
        ( if Pred(E1) then
            !:RevTrues = [E1 | !.RevTrues]
        else
            !:RevFalses = [E1 | !.RevFalses]
        ),
        do_divide(Pred, T2, !RevTrues, !RevFalses),
        ( if Pred(E2) then
            !:RevTrues = [E2 | !.RevTrues]
        else
            !:RevFalses = [E2 | !.RevFalses]
        ),
        do_divide(Pred, T3, !RevTrues, !RevFalses)
    ).

divide_by_set(DivideBySet, Set, TrueSet, FalseSet) :-
    % XXX This should be more efficient.
    divide(contains(DivideBySet), Set, TrueSet, FalseSet).

%---------------------------------------------------------------------------%

list_to_set(List) = Tree :-
    list_to_set(List, Tree).

list_to_set(List, Tree) :-
    list.sort_and_remove_dups(List, SortedList),
    sorted_list_to_set(SortedList, Tree).

% We used to use this loop to implement list_to_set, but sorting the list
% and then building the tree directly from the result is faster.
%
% :- pred list_to_set_loop(list(E)::in,
%     set_tree234(E)::in, set_tree234(E)::out) is det.
%
% list_to_set_loop([], !Tree).
% list_to_set_loop([E | Es], !Tree) :-
%     insert(E, !Tree),
%     list_to_set_loop(Es, !Tree).

from_list(List) = list_to_set(List).

from_list(List, Tree) :-
    Tree = list_to_set(List).

%---------------------%

sorted_list_to_set(List) = Tree :-
    sorted_list_to_set(List, Tree).

sorted_list_to_set(List, Tree) :-
    list.length(List, Len),
    ( if Len = 0 then
        % We can handle the Len = 0 case here just once, or we can handle it
        % lots of times in do_from_sorted_list. The former is more efficient.
        Tree = empty
    else
        find_num_234_levels(Len, Level, AllThrees),
        do_from_sorted_list(Len, List, LeftOver, Level, AllThrees, Tree),
        trace [compiletime(flag("set_tree234_sanity_checks"))] (
            expect(unify(LeftOver, []), $pred, "leftovers")
        )
    ).

:- pred do_from_sorted_list(int::in, list(E)::in, list(E)::out,
    int::in, int::in, set_tree234(E)::out) is det.

do_from_sorted_list(Len, !List, Level0, AllThrees0, Tree) :-
    ( if Level0 = 1 then
        ( if Len = 1 then
            (
                !.List = [E1 | !:List],
                Tree = two(E1, empty, empty)
            ;
                !.List = [],
                unexpected($pred, "len 1 nil")
            )
        else if Len = 2 then
            trace [compiletime(flag("set_tree234_sanity_checks"))] (
                expect(unify(Level0, 1), $pred, "Len = 2 but Level != 1")
            ),
            (
                !.List = [E1, E2 | !:List],
                Tree = three(E1, E2, empty, empty, empty)
            ;
                !.List = [_],
                unexpected($pred, "len 2 one")
            ;
                !.List = [],
                unexpected($pred, "len 2 nil")
            )
        else
            unexpected($pred, "level 1, but len not 1 or 2")
        )
    else
        Level = Level0 - 1,
        AllThrees = (AllThrees0 - 2) / 3,
        ( if Len > 2 * AllThrees then
            BaseSubLen = (Len / 3),
            Diff = Len - (BaseSubLen * 3),
            ( if Diff = 0 then
                % Len = BaseSubLen * 3:
                % (BaseSubLen) + 1 + (BaseSubLen - 1) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1,
                SubLen3 = BaseSubLen - 1
            else if Diff = 1 then
                % Len = BaseSubLen * 3 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen - 1
            else
                trace [compiletime(flag("set_tree234_sanity_checks"))] (
                    expect(unify(Diff, 2), $pred, "Diff != 2")
                ),
                % Len = BaseSubLen * 3 + 2:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_sorted_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into three: %d, %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2), i(SubLen3)], !IO)
            ),

            do_from_sorted_list(SubLen1, !List, Level, AllThrees, SubTree1),
            (
                !.List = [E1 | !:List]
            ;
                !.List = [],
                unexpected($pred, "tree E1 nil")
            ),
            do_from_sorted_list(SubLen2, !List, Level, AllThrees, SubTree2),
            (
                !.List = [E2 | !:List]
            ;
                !.List = [],
                unexpected($pred, "tree E2 nil")
            ),
            do_from_sorted_list(SubLen3, !List, Level, AllThrees, SubTree3),
            Tree = three(E1, E2, SubTree1, SubTree2, SubTree3),
            trace [io(!IO), compile_time(flag("from_sorted_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        else
            BaseSubLen = (Len) / 2,
            Diff = Len - (BaseSubLen * 2),
            ( if Diff = 0 then
                % Len = BaseSubLen * 2:
                % (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1
            else
                trace [compiletime(flag("set_tree234_sanity_checks"))] (
                    expect(unify(Diff, 1), $pred, "Diff != 1")
                ),
                % Len = BaseSubLen * 2 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_sorted_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into two: %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2)], !IO)
            ),

            do_from_sorted_list(SubLen1, !List, Level, AllThrees, SubTree1),
            (
                !.List = [E1 | !:List]
            ;
                !.List = [],
                unexpected($pred, "two E1 nil")
            ),
            do_from_sorted_list(SubLen2, !List, Level, AllThrees, SubTree2),
            Tree = two(E1, SubTree1, SubTree2),
            trace [io(!IO), compile_time(flag("from_sorted_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        )
    ).

%---------------------%

rev_sorted_list_to_set(List) = Tree :-
    rev_sorted_list_to_set(List, Tree).

rev_sorted_list_to_set(List, Tree) :-
    list.length(List, Len),
    ( if Len = 0 then
        % We can handle the Len = 0 case here just once, or we can handle it
        % lots of times in do_from_sorted_list. The former is more efficient.
        Tree = empty
    else
        find_num_234_levels(Len, Level, AllThrees),
        do_from_rev_sorted_list(Len, List, LeftOver, Level, AllThrees, Tree),
        trace [compiletime(flag("set_tree234_sanity_checks"))] (
            expect(unify(LeftOver, []), $pred, "leftovers")
        )
    ).

:- pred do_from_rev_sorted_list(int::in, list(E)::in, list(E)::out,
    int::in, int::in, set_tree234(E)::out) is det.

do_from_rev_sorted_list(Len, !List, Level0, AllThrees0, Tree) :-
    ( if Level0 = 1 then
        ( if Len = 1 then
            (
                !.List = [E1 | !:List],
                Tree = two(E1, empty, empty)
            ;
                !.List = [],
                unexpected($pred, "len 1 nil")
            )
        else if Len = 2 then
            trace [compiletime(flag("set_tree234_sanity_checks"))] (
                expect(unify(Level0, 1), $pred, "Len = 2 but Level != 1")
            ),
            (
                !.List = [E2, E1 | !:List],
                Tree = three(E1, E2, empty, empty, empty)
            ;
                !.List = [_],
                unexpected($pred, "len 2 one")
            ;
                !.List = [],
                unexpected($pred, "len 2 nil")
            )
        else
            unexpected($pred, "level 1, but len not 1 or 2")
        )
    else
        Level = Level0 - 1,
        AllThrees = (AllThrees0 - 2) / 3,
        ( if Len > 2 * AllThrees then
            BaseSubLen = (Len / 3),
            Diff = Len - (BaseSubLen * 3),
            ( if Diff = 0 then
                % Len = BaseSubLen * 3:
                % (BaseSubLen) + 1 + (BaseSubLen - 1) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1,
                SubLen3 = BaseSubLen - 1
            else if Diff = 1 then
                % Len = BaseSubLen * 3 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen - 1
            else
                trace [compiletime(flag("set_tree234_sanity_checks"))] (
                    expect(unify(Diff, 2), $pred, "Diff != 2")
                ),
                % Len = BaseSubLen * 3 + 2:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_rev_sorted_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into three: %d, %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2), i(SubLen3)], !IO)
            ),

            do_from_rev_sorted_list(SubLen3, !List, Level, AllThrees,
                SubTree3),
            (
                !.List = [E2 | !:List]
            ;
                !.List = [],
                unexpected($pred, "tree E2 nil")
            ),
            do_from_rev_sorted_list(SubLen2, !List, Level, AllThrees,
                SubTree2),
            (
                !.List = [E1 | !:List]
            ;
                !.List = [],
                unexpected($pred, "tree E1 nil")
            ),
            do_from_rev_sorted_list(SubLen1, !List, Level, AllThrees,
                SubTree1),
            Tree = three(E1, E2, SubTree1, SubTree2, SubTree3),
            trace [io(!IO), compile_time(flag("from_rev_sorted_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        else
            BaseSubLen = (Len) / 2,
            Diff = Len - (BaseSubLen * 2),
            ( if Diff = 0 then
                % Len = BaseSubLen * 2:
                % (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1
            else
                trace [compiletime(flag("set_tree234_sanity_checks"))] (
                    expect(unify(Diff, 1), $pred, "Diff != 1")
                ),
                % Len = BaseSubLen * 2 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_rev_sorted_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into two: %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2)], !IO)
            ),

            do_from_rev_sorted_list(SubLen2, !List, Level, AllThrees,
                SubTree2),
            (
                !.List = [E1 | !:List]
            ;
                !.List = [],
                unexpected($pred, "two E1 nil")
            ),
            do_from_rev_sorted_list(SubLen1, !List, Level, AllThrees,
                SubTree1),
            Tree = two(E1, SubTree1, SubTree2),
            trace [io(!IO), compile_time(flag("from_rev_sorted_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        )
    ).

%---------------------%

:- pred find_num_234_levels(int::in, int::out, int::out) is det.

find_num_234_levels(Len, Level, AllThrees) :-
    find_num_234_levels_loop(Len, 0, Level, 0, AllThrees).

:- pred find_num_234_levels_loop(int::in,
    int::in, int::out, int::in, int::out) is det.

find_num_234_levels_loop(Len, Level0, Level, !AllThrees) :-
    ( if Len =< !.AllThrees then
        Level = Level0
    else
        Level1 = Level0 + 1,
        !:AllThrees = !.AllThrees * 3 + 2,
        find_num_234_levels_loop(Len, Level1, Level, !AllThrees)
    ).

%---------------------------------------------------------------------------%

to_sorted_list(Tree) = List :-
    to_sorted_list_2(Tree, [], List).

to_sorted_list(Tree, List) :-
    to_sorted_list_2(Tree, [], List).

:- pred to_sorted_list_2(set_tree234(T)::in,
    list(T)::in, list(T)::out) is det.

to_sorted_list_2(empty, L, L).
to_sorted_list_2(two(E0, T0, T1), L0, L) :-
    to_sorted_list_2(T1, L0, L1),
    to_sorted_list_2(T0, [E0 | L1], L).
to_sorted_list_2(three(E0, E1, T0, T1, T2), L0, L) :-
    to_sorted_list_2(T2, L0, L1),
    to_sorted_list_2(T1, [E1 | L1], L2),
    to_sorted_list_2(T0, [E0 | L2], L).
to_sorted_list_2(four(E0, E1, E2, T0, T1, T2, T3), L0, L) :-
    to_sorted_list_2(T3, L0, L1),
    to_sorted_list_2(T2, [E2 | L1], L2),
    to_sorted_list_2(T1, [E1 | L2], L3),
    to_sorted_list_2(T0, [E0 | L3], L).

%---------------------------------------------------------------------------%

from_set(Set) =
    sorted_list_to_set(set.to_sorted_list(Set)).

to_set(Tree) =
    set.sorted_list_to_set(to_sorted_list(Tree)).

%---------------------------------------------------------------------------%

count(empty) = 0.
count(two(_, T0, T1)) = N :-
    N0 = count(T0),
    N1 = count(T1),
    N = 1 + N0 + N1.
count(three(_, _, T0, T1, T2)) = N :-
    N0 = count(T0),
    N1 = count(T1),
    N2 = count(T2),
    N = 2 + N0 + N1 + N2.
count(four(_, _, _, T0, T1, T2, T3)) = N :-
    N0 = count(T0),
    N1 = count(T1),
    N2 = count(T2),
    N3 = count(T3),
    N = 3 + N0 + N1 + N2 + N3.

%---------------------------------------------------------------------------%

all_true(Pred, T) :-
    (
        T = empty
    ;
        T = two(E0, T0, T1),
        all_true(Pred, T0),
        Pred(E0),
        all_true(Pred, T1)
    ;
        T = three(E0, E1, T0, T1, T2),
        all_true(Pred, T0),
        Pred(E0),
        all_true(Pred, T1),
        Pred(E1),
        all_true(Pred, T2)
    ;
        T = four(E0, E1, E2, T0, T1, T2, T3),
        all_true(Pred, T0),
        Pred(E0),
        all_true(Pred, T1),
        Pred(E1),
        all_true(Pred, T2),
        Pred(E2),
        all_true(Pred, T3)
    ).

%---------------------%

filter(Pred, Set) = TrueSet :-
    filter(Pred, Set, TrueSet).

filter(Pred, Set, TrueSet) :-
    % XXX This should be more efficient.
    divide(Pred, Set, TrueSet, _FalseSet).

filter(Pred, Set, TrueSet, FalseSet) :-
    divide(Pred, Set, TrueSet, FalseSet).

%---------------------%

filter_map(Func, SetA) = SetB :-
    filter_map_func(Func, SetA, [], ListB),
    SetB = list_to_set(ListB).

:- pred filter_map_func(func(T1) = T2, set_tree234(T1), list(T2), list(T2)).
:- mode filter_map_func(in(func(in) = out is semidet), in, in, out) is det.

filter_map_func(_Func, empty, !List).
filter_map_func(Func, Tin, !List) :-
    Tin = two(E0, T0, T1),
    filter_map_func(Func, T0, !List),
    ( if N0 = Func(E0) then
        !:List = [N0 | !.List]
    else
        true
    ),
    filter_map_func(Func, T1, !List).
filter_map_func(Func, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    filter_map_func(Func, T0, !List),
    ( if N0 = Func(E0) then
        !:List = [N0 | !.List]
    else
        true
    ),
    filter_map_func(Func, T1, !List),
    ( if N1 = Func(E1) then
        !:List = [N1 | !.List]
    else
        true
    ),
    filter_map_func(Func, T2, !List).
filter_map_func(Func, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    filter_map_func(Func, T0, !List),
    ( if N0 = Func(E0) then
        !:List = [N0 | !.List]
    else
        true
    ),
    filter_map_func(Func, T1, !List),
    ( if N1 = Func(E1) then
        !:List = [N1 | !.List]
    else
        true
    ),
    filter_map_func(Func, T2, !List),
    ( if N2 = Func(E2) then
        !:List = [N2 | !.List]
    else
        true
    ),
    filter_map_func(Func, T3, !List).

filter_map(Pred, SetA, SetB) :-
    filter_map_pred(Pred, SetA, [], ListB),
    SetB = list_to_set(ListB).

:- pred filter_map_pred(
    pred(T1, T2)::in(pred(in, out) is semidet), set_tree234(T1)::in,
    list(T2)::in, list(T2)::out) is det.

filter_map_pred(_Pred, empty, !List).
filter_map_pred(Pred, Tin, !List) :-
    Tin = two(E0, T0, T1),
    filter_map_pred(Pred, T0, !List),
    ( if Pred(E0, N0) then
        !:List = [N0 | !.List]
    else
        true
    ),
    filter_map_pred(Pred, T1, !List).
filter_map_pred(Pred, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    filter_map_pred(Pred, T0, !List),
    ( if Pred(E0, N0) then
        !:List = [N0 | !.List]
    else
        true
    ),
    filter_map_pred(Pred, T1, !List),
    ( if Pred(E1, N1) then
        !:List = [N1 | !.List]
    else
        true
    ),
    filter_map_pred(Pred, T2, !List).
filter_map_pred(Pred, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    filter_map_pred(Pred, T0, !List),
    ( if Pred(E0, N0) then
        !:List = [N0 | !.List]
    else
        true
    ),
    filter_map_pred(Pred, T1, !List),
    ( if Pred(E1, N1) then
        !:List = [N1 | !.List]
    else
        true
    ),
    filter_map_pred(Pred, T2, !List),
    ( if Pred(E2, N2) then
        !:List = [N2 | !.List]
    else
        true
    ),
    filter_map_pred(Pred, T3, !List).

%---------------------%

map(Func, SetA) = SetB :-
    map_func(Func, SetA, [], ListB),
    SetB = list_to_set(ListB).

:- pred map_func(func(T1) = T2, set_tree234(T1),
    list(T2), list(T2)).
:- mode map_func(in(func(in) = out is det), in, in, out) is det.

map_func(_Func, empty, !List).
map_func(Func, Tin, !List) :-
    Tin = two(E0, T0, T1),
    map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    map_func(Func, T1, !List).
map_func(Func, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    map_func(Func, T1, !List),
    N1 = Func(E1),
    !:List = [N1 | !.List],
    map_func(Func, T2, !List).
map_func(Func, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    map_func(Func, T1, !List),
    N1 = Func(E1),
    !:List = [N1 | !.List],
    map_func(Func, T2, !List),
    N2 = Func(E2),
    !:List = [N2 | !.List],
    map_func(Func, T3, !List).

map(Pred, SetA, SetB) :-
    map_pred(Pred, SetA, [], ListB),
    SetB = list_to_set(ListB).

:- pred map_pred(pred(T1, T2)::in(pred(in, out) is det),
    set_tree234(T1)::in, list(T2)::in, list(T2)::out) is det.

map_pred(_Pred, empty, !List).
map_pred(Pred, Tin, !List) :-
    Tin = two(E0, T0, T1),
    map_pred(Pred, T0, !List),
    Pred(E0, N0),
    !:List = [N0 | !.List],
    map_pred(Pred, T1, !List).
map_pred(Pred, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    map_pred(Pred, T0, !List),
    Pred(E0, N0),
    !:List = [N0 | !.List],
    map_pred(Pred, T1, !List),
    Pred(E1, N1),
    !:List = [N1 | !.List],
    map_pred(Pred, T2, !List).
map_pred(Pred, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    map_pred(Pred, T0, !List),
    Pred(E0, N0),
    !:List = [N0 | !.List],
    map_pred(Pred, T1, !List),
    Pred(E1, N1),
    !:List = [N1 | !.List],
    map_pred(Pred, T2, !List),
    Pred(E2, N2),
    !:List = [N2 | !.List],
    map_pred(Pred, T3, !List).

%---------------------%

fold(Func, Tree, A0) =
    foldl(Func, Tree, A0).

fold(Pred, Tree, !A) :-
    foldl(Pred, Tree, !A).

foldl(_Func, empty, A) = A.
foldl(Func, two(E, T0, T1), !.A) = !:A :-
    foldl(Func, T0, !.A) = !:A,
    !:A = Func(E, !.A),
    foldl(Func, T1, !.A) = !:A.
foldl(Func, three(E0, E1, T0, T1, T2), !.A) = !:A :-
    foldl(Func, T0, !.A) = !:A,
    !:A = Func(E0, !.A),
    foldl(Func, T1, !.A) = !:A,
    !:A = Func(E1, !.A),
    foldl(Func, T2, !.A) = !:A.
foldl(Func, four(E0, E1, E2, T0, T1, T2, T3), !.A) = !:A :-
    foldl(Func, T0, !.A) = !:A,
    !:A = Func(E0, !.A),
    foldl(Func, T1, !.A) = !:A,
    !:A = Func(E1, !.A),
    foldl(Func, T2, !.A) = !:A,
    !:A = Func(E2, !.A),
    foldl(Func, T3, !.A) = !:A.

foldl(_Pred, empty, !A).
foldl(Pred, two(E, T0, T1), !A) :-
    foldl(Pred, T0, !A),
    Pred(E, !A),
    foldl(Pred, T1, !A).
foldl(Pred, three(E0, E1, T0, T1, T2), !A) :-
    foldl(Pred, T0, !A),
    Pred(E0, !A),
    foldl(Pred, T1, !A),
    Pred(E1, !A),
    foldl(Pred, T2, !A).
foldl(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A) :-
    foldl(Pred, T0, !A),
    Pred(E0, !A),
    foldl(Pred, T1, !A),
    Pred(E1, !A),
    foldl(Pred, T2, !A),
    Pred(E2, !A),
    foldl(Pred, T3, !A).

fold2(Pred, Tree, !A, !B) :-
    foldl2(Pred, Tree, !A, !B).

foldl2(_Pred, empty, !A, !B).
foldl2(Pred, two(E, T0, T1), !A, !B) :-
    foldl2(Pred, T0, !A, !B),
    Pred(E, !A, !B),
    foldl2(Pred, T1, !A, !B).
foldl2(Pred, three(E0, E1, T0, T1, T2), !A, !B) :-
    foldl2(Pred, T0, !A, !B),
    Pred(E0, !A, !B),
    foldl2(Pred, T1, !A, !B),
    Pred(E1, !A, !B),
    foldl2(Pred, T2, !A, !B).
foldl2(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B) :-
    foldl2(Pred, T0, !A, !B),
    Pred(E0, !A, !B),
    foldl2(Pred, T1, !A, !B),
    Pred(E1, !A, !B),
    foldl2(Pred, T2, !A, !B),
    Pred(E2, !A, !B),
    foldl2(Pred, T3, !A, !B).

fold3(Pred, Tree, !A, !B, !C) :-
    foldl3(Pred, Tree, !A, !B, !C).

foldl3(_Pred, empty, !A, !B, !C).
foldl3(Pred, two(E, T0, T1), !A, !B, !C) :-
    foldl3(Pred, T0, !A, !B, !C),
    Pred(E, !A, !B, !C),
    foldl3(Pred, T1, !A, !B, !C).
foldl3(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C) :-
    foldl3(Pred, T0, !A, !B, !C),
    Pred(E0, !A, !B, !C),
    foldl3(Pred, T1, !A, !B, !C),
    Pred(E1, !A, !B, !C),
    foldl3(Pred, T2, !A, !B, !C).
foldl3(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C) :-
    foldl3(Pred, T0, !A, !B, !C),
    Pred(E0, !A, !B, !C),
    foldl3(Pred, T1, !A, !B, !C),
    Pred(E1, !A, !B, !C),
    foldl3(Pred, T2, !A, !B, !C),
    Pred(E2, !A, !B, !C),
    foldl3(Pred, T3, !A, !B, !C).

fold4(Pred, Tree, !A, !B, !C, !D) :-
    foldl4(Pred, Tree, !A, !B, !C, !D).

foldl4(_Pred, empty, !A, !B, !C, !D).
foldl4(Pred, two(E, T0, T1), !A, !B, !C, !D) :-
    foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(E, !A, !B, !C, !D),
    foldl4(Pred, T1, !A, !B, !C, !D).
foldl4(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D) :-
    foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(E0, !A, !B, !C, !D),
    foldl4(Pred, T1, !A, !B, !C, !D),
    Pred(E1, !A, !B, !C, !D),
    foldl4(Pred, T2, !A, !B, !C, !D).
foldl4(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C, !D) :-
    foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(E0, !A, !B, !C, !D),
    foldl4(Pred, T1, !A, !B, !C, !D),
    Pred(E1, !A, !B, !C, !D),
    foldl4(Pred, T2, !A, !B, !C, !D),
    Pred(E2, !A, !B, !C, !D),
    foldl4(Pred, T3, !A, !B, !C, !D).

fold5(Pred, Tree, !A, !B, !C, !D, !E) :-
    foldl5(Pred, Tree, !A, !B, !C, !D, !E).

foldl5(_Pred, empty, !A, !B, !C, !D, !E).
foldl5(Pred, two(E, T0, T1), !A, !B, !C, !D, !E) :-
    foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E, !A, !B, !C, !D, !E),
    foldl5(Pred, T1, !A, !B, !C, !D, !E).
foldl5(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D, !E) :-
    foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E0, !A, !B, !C, !D, !E),
    foldl5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(E1, !A, !B, !C, !D, !E),
    foldl5(Pred, T2, !A, !B, !C, !D, !E).
foldl5(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C, !D, !E) :-
    foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E0, !A, !B, !C, !D, !E),
    foldl5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(E1, !A, !B, !C, !D, !E),
    foldl5(Pred, T2, !A, !B, !C, !D, !E),
    Pred(E2, !A, !B, !C, !D, !E),
    foldl5(Pred, T3, !A, !B, !C, !D, !E).

fold6(Pred, Tree, !A, !B, !C, !D, !E, !F) :-
    foldl6(Pred, Tree, !A, !B, !C, !D, !E, !F).

foldl6(_Pred, empty, !A, !B, !C, !D, !E, !F).
foldl6(Pred, two(E, T0, T1), !A, !B, !C, !D, !E, !F) :-
    foldl6(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E, !A, !B, !C, !D, !E, !F),
    foldl6(Pred, T1, !A, !B, !C, !D, !E, !F).
foldl6(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D, !E, !F) :-
    foldl6(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E0, !A, !B, !C, !D, !E, !F),
    foldl6(Pred, T1, !A, !B, !C, !D, !E, !F),
    Pred(E1, !A, !B, !C, !D, !E, !F),
    foldl6(Pred, T2, !A, !B, !C, !D, !E, !F).
foldl6(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C, !D, !E, !F) :-
    foldl6(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E0, !A, !B, !C, !D, !E, !F),
    foldl6(Pred, T1, !A, !B, !C, !D, !E, !F),
    Pred(E1, !A, !B, !C, !D, !E, !F),
    foldl6(Pred, T2, !A, !B, !C, !D, !E, !F),
    Pred(E2, !A, !B, !C, !D, !E, !F),
    foldl6(Pred, T3, !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%

:- pred height(set_tree234(T)::in, int::out) is det.

height(Tree, Height) :-
    (
        Tree = empty,
        Height = 0
    ;
        ( Tree = two(_, T0, _)
        ; Tree = three(_, _, T0, _, _)
        ; Tree = four(_, _, _, T0, _, _, _)
        ),
        height(T0, T0Height),
        Height = T0Height + 1
    ).

%---------------------------------------------------------------------------%
