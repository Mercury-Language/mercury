%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

:- pred is_singleton(set_tree234(T)::in, T::out) is semidet.

    % `empty(Set)' is true iff `Set' is an empty set.
    % `is_empty' is a synonym for `empty'.
    %
:- pred empty(set_tree234(_T)::in) is semidet.
:- pred is_empty(set_tree234(_T)::in) is semidet.

    % `non_empty(Set)' is true iff `Set' is not an empty set.
    % `is_non_empty' is a synonym for `non_empty'.
    %
:- pred non_empty(set_tree234(T)::in) is semidet.
:- pred is_non_empty(set_tree234(T)::in) is semidet.

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred member(T, set_tree234(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `is_member(Set, X, Result)' returns `Result = yes' iff `X' is a member of
    % `Set'.
    %
:- func is_member(set_tree234(T), T) = bool.
:- pred is_member(set_tree234(T)::in, T::in, bool::out) is det.

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pred contains(set_tree234(T)::in, T::in) is semidet.

    % `list_to_set(List) = Set' is true iff `Set' is the set containing only
    % the members of `List'.
    %
:- func list_to_set(list(T)) = set_tree234(T).
:- pred list_to_set(list(T)::in, set_tree234(T)::out) is det.

:- func from_list(list(T)) = set_tree234(T).
:- pred from_list(list(T)::in, set_tree234(T)::out) is det.

    % `sorted_list_to_set(List) = Set' is true iff `Set' is the set containing
    % only the members of `List'. `List' must be sorted.
    %
:- func sorted_list_to_set(list(T)) = set_tree234(T).
:- pred sorted_list_to_set(list(T)::in, set_tree234(T)::out) is det.

    % `from_set(Set)' returns a set_tree234 containing only the members of
    % `Set'. Takes O(card(Set)) time and space.
    %
:- func from_set(set.set(T)) = set_tree234(T).

    % `to_sorted_list(Set) = List' is true iff `List' is the list of all the
    % members of `Set', in sorted order.
    %
:- func to_sorted_list(set_tree234(T)) = list(T).
:- pred to_sorted_list(set_tree234(T)::in, list(T)::out) is det.

    % `to_sorted_list(Set)' returns a set.set containing all the members of
    % `Set', in sorted order. Takes O(card(Set)) time and space.
    %
:- func to_set(set_tree234(T)) = set.set(T).

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
:- pred remove(T::in, set_tree234(T)::in, set_tree234(T)::out)
    is semidet.

    % `remove_list(Xs, Set0, Set)' is true iff Xs does not contain any
    % duplicates, `Set0' contains every member of `Xs', and `Set' is the
    % relative complement of `Set0' and the set containing only the members of
    % `Xs'.
    %
:- pred remove_list(list(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is semidet.

    % `remove_least(X, Set0, Set)' is true iff `X' is the least element in
    % `Set0', and `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred remove_least(T::out, set_tree234(T)::in, set_tree234(T)::out)
    is semidet.

    % `union(SetA, SetB) = Set' is true iff `Set' is the union of `SetA' and
    % `SetB'.
    %
:- func union(set_tree234(T), set_tree234(T)) = set_tree234(T).
:- pred union(set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.

    % `union_list(A, B)' is true iff `B' is the union of all the sets in `A'
    %
:- func union_list(list(set_tree234(T))) = set_tree234(T).
:- pred union_list(list(set_tree234(T))::in, set_tree234(T)::out)
    is det.

    % `power_union(A) = B' is true iff `B' is the union of
    % all the sets in `A'
    %
:- func power_union(set_tree234(set_tree234(T))) = set_tree234(T).
:- pred power_union(set_tree234(set_tree234(T))::in,
    set_tree234(T)::out) is det.

    % `intersect(SetA, SetB) = Set' is true iff `Set' is the intersection of
    % `SetA' and `SetB'.
    %
:- func intersect(set_tree234(T), set_tree234(T)) = set_tree234(T).
:- pred intersect(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

    % `power_intersect(A, B)' is true iff `B' is the intersection of all the
    % sets in `A'.
    %
:- func power_intersect(set_tree234(set_tree234(T))) = set_tree234(T).
:- pred power_intersect(set_tree234(set_tree234(T))::in,
    set_tree234(T)::out) is det.

    % `intersect_list(A, B)' is true iff `B' is the intersection of all the
    % sets in `A'.
    %
:- func intersect_list(list(set_tree234(T))) = set_tree234(T).
:- pred intersect_list(list(set_tree234(T))::in,
    set_tree234(T)::out) is det.

    % `difference(SetA, SetB, Set)' is true iff `Set' is the set containing all
    % the elements of `SetA' except those that occur in `SetB'.
    %
:- func difference(set_tree234(T), set_tree234(T)) = set_tree234(T).
:- pred difference(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

    % `count(Set, Count)' is true iff `Set' has `Count' elements.
    %
:- func count(set_tree234(T)) = int.

:- func map(func(T1) = T2, set_tree234(T1)) = set_tree234(T2).
:- pred map(pred(T1, T2)::in(pred(in, out) is det),
    set_tree234(T1)::in, set_tree234(T2)::out) is det.

:- pred filter_map(pred(T1, T2)::in(pred(in, out) is semidet),
    set_tree234(T1)::in, set_tree234(T2)::out) is det.

:- func filter_map(func(T1) = T2, set_tree234(T1))
    = set_tree234(T2).
:- mode filter_map(func(in) = out is semidet, in) = out is det.

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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module term.  % for var/1.

:- pragma type_spec(set_tree234.sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(set_tree234.contains(in, in), T = var(_)).
:- pragma type_spec(set_tree234.insert/3, T = var(_)).
:- pragma type_spec(set_tree234.insert_list/3, T = var(_)).
:- pragma type_spec(set_tree234.union/2, T = var(_)).
:- pragma type_spec(set_tree234.union/3, T = var(_)).
:- pragma type_spec(set_tree234.intersect/2, T = var(_)).
:- pragma type_spec(set_tree234.intersect/3, T = var(_)).
:- pragma type_spec(set_tree234.difference/2, T = var(_)).
:- pragma type_spec(set_tree234.difference/3, T = var(_)).

:- type set_tree234(T)
    --->    empty
    ;       two(T, set_tree234(T), set_tree234(T))
    ;       three(T, T, set_tree234(T), set_tree234(T), set_tree234(T))
    ;       four(T, T, T, set_tree234(T), set_tree234(T),
                set_tree234(T), set_tree234(T)).

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

set_tree234.init = empty.

set_tree234.singleton_set(X, two(X, empty, empty)).

set_tree234.make_singleton_set(X) = two(X, empty, empty).

set_tree234.is_singleton(two(X, empty, empty), X).

set_tree234.empty(empty).

set_tree234.is_empty(empty).

set_tree234.non_empty(two(_, _, _)).
set_tree234.non_empty(three(_, _, _, _, _)).
set_tree234.non_empty(four(_, _, _, _, _, _, _)).

set_tree234.is_non_empty(two(_, _, _)).
set_tree234.is_non_empty(three(_, _, _, _, _)).
set_tree234.is_non_empty(four(_, _, _, _, _, _, _)).

:- pragma promise_equivalent_clauses(set_tree234.member/2).

set_tree234.member(Element::out, Set::in) :-
    set_tree234.all_members(Set, Element).
set_tree234.member(Element::in, Set::in) :-
    set_tree234.is_member(Set, Element) = yes.

:- pred set_tree234.all_members(set_tree234(T)::in, T::out) is nondet.

set_tree234.all_members(empty, _) :- fail.
set_tree234.all_members(two(E0, T0, T1), E) :-
    (
        E = E0
    ;
        set_tree234.all_members(T0, E)
    ;
        set_tree234.all_members(T1, E)
    ).
set_tree234.all_members(three(E0, E1, T0, T1, T2), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        set_tree234.all_members(T0, E)
    ;
        set_tree234.all_members(T1, E)
    ;
        set_tree234.all_members(T2, E)
    ).
set_tree234.all_members(four(E0, E1, E2, T0, T1, T2, T3), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        E = E2
    ;
        set_tree234.all_members(T0, E)
    ;
        set_tree234.all_members(T1, E)
    ;
        set_tree234.all_members(T2, E)
    ;
        set_tree234.all_members(T3, E)
    ).

set_tree234.is_member(T, E, R) :-
    (
        T = empty,
        R = no
    ;
        T = two(E0, T0, T1),
        compare(Result, E, E0),
        (
            Result = (<),
            set_tree234.is_member(T0, E, R)
        ;
            Result = (=),
            R = yes
        ;
            Result = (>),
            set_tree234.is_member(T1, E, R)
        )
    ;
        T = three(E0, E1, T0, T1, T2),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            set_tree234.is_member(T0, E, R)
        ;
            Result0 = (=),
            R = yes
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                set_tree234.is_member(T1, E, R)
            ;
                Result1 = (=),
                R = yes
            ;
                Result1 = (>),
                set_tree234.is_member(T2, E, R)
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
                set_tree234.is_member(T0, E, R)
            ;
                Result0 = (=),
                R = yes
            ;
                Result0 = (>),
                set_tree234.is_member(T1, E, R)
            )
        ;
            Result1 = (=),
            R = yes
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                set_tree234.is_member(T2, E, R)
            ;
                Result2 = (=),
                R = yes
            ;
                Result2 = (>),
                set_tree234.is_member(T3, E, R)
            )
        )
    ).

set_tree234.is_member(T, E) = R :-
    set_tree234.is_member(T, E, R).

set_tree234.contains(T, E) :-
    set_tree234.is_member(T, E, yes).

%---------------------------------------------------------------------------%

set_tree234.list_to_set(List) = Tree :-
    set_tree234.list_to_set_2(List, empty, Tree).

set_tree234.list_to_set(List, Tree) :-
    set_tree234.list_to_set_2(List, empty, Tree).

set_tree234.from_list(List) = set_tree234.list_to_set(List).

set_tree234.from_list(List, Tree) :-
    Tree = set_tree234.list_to_set(List).

set_tree234.sorted_list_to_set(List) = Tree :-
        % XXX We should exploit the sortedness of List.
    set_tree234.list_to_set_2(List, empty, Tree).

set_tree234.sorted_list_to_set(List, Tree) :-
        % XXX We should exploit the sortedness of List.
    set_tree234.list_to_set_2(List, empty, Tree).

:- pred set_tree234.list_to_set_2(list(E)::in, set_tree234(E)::in,
    set_tree234(E)::out) is det.

set_tree234.list_to_set_2([], !Tree).
set_tree234.list_to_set_2([E | Es], !Tree) :-
    set_tree234.insert(E, !Tree),
    set_tree234.list_to_set_2(Es, !Tree).

set_tree234.to_set(Tree) =
    set.sorted_list_to_set(set_tree234.to_sorted_list(Tree)).

%---------------------------------------------------------------------------%

set_tree234.to_sorted_list(Tree) = List :-
    set_tree234.to_sorted_list_2(Tree, [], List).

set_tree234.to_sorted_list(Tree, List) :-
    set_tree234.to_sorted_list_2(Tree, [], List).

:- pred set_tree234.to_sorted_list_2(set_tree234(T)::in,
    list(T)::in, list(T)::out) is det.

set_tree234.to_sorted_list_2(empty, L, L).
set_tree234.to_sorted_list_2(two(E0, T0, T1), L0, L) :-
    set_tree234.to_sorted_list_2(T1, L0, L1),
    set_tree234.to_sorted_list_2(T0, [E0 | L1], L).
set_tree234.to_sorted_list_2(three(E0, E1, T0, T1, T2), L0, L) :-
    set_tree234.to_sorted_list_2(T2, L0, L1),
    set_tree234.to_sorted_list_2(T1, [E1 | L1], L2),
    set_tree234.to_sorted_list_2(T0, [E0 | L2], L).
set_tree234.to_sorted_list_2(four(E0, E1, E2, T0, T1, T2, T3), L0, L) :-
    set_tree234.to_sorted_list_2(T3, L0, L1),
    set_tree234.to_sorted_list_2(T2, [E2 | L1], L2),
    set_tree234.to_sorted_list_2(T1, [E1 | L2], L3),
    set_tree234.to_sorted_list_2(T0, [E0 | L3], L).

set_tree234.from_set(Set) =
    set_tree234.sorted_list_to_set(set.to_sorted_list(Set)).

%---------------------------------------------------------------------------%

set_tree234.equal(SetA, SetB) :-
    set_tree234.to_sorted_list(SetA, ListA),
    set_tree234.to_sorted_list(SetB, ListB),
    ListA = ListB.

set_tree234.subset(empty, _Set).
set_tree234.subset(two(E, T0, T1), Set) :-
    set_tree234.subset(T0, Set),
    set_tree234.contains(Set, E),
    set_tree234.subset(T1, Set).
set_tree234.subset(three(E0, E1, T0, T1, T2), Set) :-
    set_tree234.subset(T0, Set),
    set_tree234.contains(Set, E0),
    set_tree234.subset(T1, Set),
    set_tree234.contains(Set, E1),
    set_tree234.subset(T2, Set).
set_tree234.subset(four(E0, E1, E2, T0, T1, T2, T3), Set) :-
    set_tree234.subset(T0, Set),
    set_tree234.contains(Set, E0),
    set_tree234.subset(T1, Set),
    set_tree234.contains(Set, E1),
    set_tree234.subset(T2, Set),
    set_tree234.contains(Set, E2),
    set_tree234.subset(T3, Set).

set_tree234.superset(SuperSet, Set) :-
    set_tree234.subset(Set, SuperSet).

%---------------------------------------------------------------------------%

:- inst two(E, T)   ---> two(E, T, T).
:- inst three(E, T) ---> three(E, E, T, T, T).
:- inst four(E, T)  ---> four(E, E, E, T, T, T, T).

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

%---------------------------------------------------------------------------%

set_tree234.insert(E, Tin) = Tout :-
    set_tree234.insert(E, Tin, Tout).

set_tree234.insert(E, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(E, empty, empty)
    ;
        Tin = two(_, _, _),
        set_tree234.insert2(E, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _),
        set_tree234.insert3(E, Tin, Tout)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            set_tree234.insert2(E, Sub0, NewSub0),
            Tout = two(E1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Tout = Tin
        ;
            Result1 = (>),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            set_tree234.insert2(E, Sub1, NewSub1),
            Tout = two(E1, Sub0, NewSub1)
        )
    ).

:- pragma type_spec(set_tree234.insert2(in, in_two, out), T = var(_)).

:- pred set_tree234.insert2(T::in,
    set_tree234(T)::in_two, set_tree234(T)::out) is det.

set_tree234.insert2(E, Tin, Tout) :-
    Tin = two(E0, T0, T1),
    (
        T0 = empty
        % T1 = empty implied by T0 = empty
    ->
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
    ;
        compare(Result, E, E0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                set_tree234.split_four(T0, MT0E, T00, T01),
                compare(Result1, E, MT0E),
                (
                    Result1 = (<),
                    set_tree234.insert2(E, T00, NewT00),
                    Tout = three(MT0E, E0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = three(MT0E, E0, T00, T01, T1)
                ;
                    Result1 = (>),
                    set_tree234.insert2(E, T01, NewT01),
                    Tout = three(MT0E, E0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _),
                set_tree234.insert3(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = two(_, _, _),
                set_tree234.insert2(E, T0, NewT0),
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
                set_tree234.split_four(T1, MT1E, T10, T11),
                compare(Result1, E, MT1E),
                (
                    Result1 = (<),
                    set_tree234.insert2(E, T10, NewT10),
                    Tout = three(E0, MT1E, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = three(E0, MT1E, T0, T10, T11)
                ;
                    Result1 = (>),
                    set_tree234.insert2(E, T11, NewT11),
                    Tout = three(E0, MT1E, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _),
                set_tree234.insert3(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = two(_, _, _),
                set_tree234.insert2(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(E, empty, empty),
                Tout = two(E0, T0, NewT1)
            )
        )
    ).

:- pragma type_spec(set_tree234.insert3(in, in_three, out), T = var(_)).

:- pred set_tree234.insert3(T::in,
    set_tree234(T)::in_three, set_tree234(T)::out) is det.

set_tree234.insert3(E, Tin, Tout) :-
    Tin = three(E0, E1, T0, T1, T2),
    (
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    ->
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
    ;
        compare(Result0, E, E0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                set_tree234.split_four(T0, MT0E, T00, T01),
                compare(ResultM, E, MT0E),
                (
                    ResultM = (<),
                    set_tree234.insert2(E, T00, NewT00),
                    Tout = four(MT0E, E0, E1, NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = four(MT0E, E0, E1, T00, T01, T1, T2)
                ;
                    ResultM = (>),
                    set_tree234.insert2(E, T01, NewT01),
                    Tout = four(MT0E, E0, E1, T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _),
                set_tree234.insert3(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _),
                set_tree234.insert2(E, T0, NewT0),
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
                    set_tree234.split_four(T1, MT1E, T10, T11),
                    compare(ResultM, E, MT1E),
                    (
                        ResultM = (<),
                        set_tree234.insert2(E, T10, NewT10),
                        Tout = four(E0, MT1E, E1, T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        % The Tout we are returning does not have the same
                        % shape as Tin, but it contains the same elements.
                        Tout = four(E0, MT1E, E1, T0, T10, T11, T2)
                    ;
                        ResultM = (>),
                        set_tree234.insert2(E, T11, NewT11),
                        Tout = four(E0, MT1E, E1, T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _),
                    set_tree234.insert3(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _),
                    set_tree234.insert2(E, T1, NewT1),
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
                    set_tree234.split_four(T2, MT2E, T20, T21),
                    compare(ResultM, E, MT2E),
                    (
                        ResultM = (<),
                        set_tree234.insert2(E, T20, NewT20),
                        Tout = four(E0, E1, MT2E, T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        % The Tout we are returning does not have the same
                        % shape as Tin, but it contains the same elements.
                        Tout = four(E0, E1, MT2E, T0, T1, T20, T21)
                    ;
                        ResultM = (>),
                        set_tree234.insert2(E, T21, NewT21),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _),
                    set_tree234.insert3(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _),
                    set_tree234.insert2(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

set_tree234.insert_new(E, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(E, empty, empty)
    ;
        Tin = two(_, _, _),
        set_tree234.insert_new2(E, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _),
        set_tree234.insert_new3(E, Tin, Tout)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            set_tree234.insert_new2(E, Sub0, NewSub0),
            Tout = two(E1, NewSub0, Sub1)
        ;
            Result1 = (=),
            fail
        ;
            Result1 = (>),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            set_tree234.insert_new2(E, Sub1, NewSub1),
            Tout = two(E1, Sub0, NewSub1)
        )
    ).

:- pragma type_spec(set_tree234.insert_new2(in, in_two, out), T = var(_)).

:- pred set_tree234.insert_new2(T::in,
    set_tree234(T)::in_two, set_tree234(T)::out) is semidet.

set_tree234.insert_new2(E, Tin, Tout) :-
    Tin = two(E0, T0, T1),
    (
        T0 = empty
        % T1 = empty implied by T0 = empty
    ->
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
    ;
        compare(Result, E, E0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                set_tree234.split_four(T0, MT0E, T00, T01),
                compare(Result1, E, MT0E),
                (
                    Result1 = (<),
                    set_tree234.insert_new2(E, T00, NewT00),
                    Tout = three(MT0E, E0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    set_tree234.insert_new2(E, T01, NewT01),
                    Tout = three(MT0E, E0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _),
                set_tree234.insert_new3(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = two(_, _, _),
                set_tree234.insert_new2(E, T0, NewT0),
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
                set_tree234.split_four(T1, MT1E, T10, T11),
                compare(Result1, E, MT1E),
                (
                    Result1 = (<),
                    set_tree234.insert_new2(E, T10, NewT10),
                    Tout = three(E0, MT1E, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    set_tree234.insert_new2(E, T11, NewT11),
                    Tout = three(E0, MT1E, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _),
                set_tree234.insert_new3(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = two(_, _, _),
                set_tree234.insert_new2(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(E, empty, empty),
                Tout = two(E0, T0, NewT1)
            )
        )
    ).

:- pragma type_spec(set_tree234.insert_new3(in, in_three, out), T = var(_)).

:- pred set_tree234.insert_new3(T::in,
    set_tree234(T)::in_three, set_tree234(T)::out) is semidet.

set_tree234.insert_new3(E, Tin, Tout) :-
    Tin = three(E0, E1, T0, T1, T2),
    (
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    ->
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
    ;
        compare(Result0, E, E0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _),
                set_tree234.split_four(T0, MT0E, T00, T01),
                compare(ResultM, E, MT0E),
                (
                    ResultM = (<),
                    set_tree234.insert_new2(E, T00, NewT00),
                    Tout = four(MT0E, E0, E1, NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    fail
                ;
                    ResultM = (>),
                    set_tree234.insert_new2(E, T01, NewT01),
                    Tout = four(MT0E, E0, E1, T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _),
                set_tree234.insert_new3(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _),
                set_tree234.insert_new2(E, T0, NewT0),
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
                    set_tree234.split_four(T1, MT1E, T10, T11),
                    compare(ResultM, E, MT1E),
                    (
                        ResultM = (<),
                        set_tree234.insert_new2(E, T10, NewT10),
                        Tout = four(E0, MT1E, E1, T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        set_tree234.insert_new2(E, T11, NewT11),
                        Tout = four(E0, MT1E, E1, T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _),
                    set_tree234.insert_new3(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _),
                    set_tree234.insert_new2(E, T1, NewT1),
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
                    set_tree234.split_four(T2, MT2E, T20, T21),
                    compare(ResultM, E, MT2E),
                    (
                        ResultM = (<),
                        set_tree234.insert_new2(E, T20, NewT20),
                        Tout = four(E0, E1, MT2E, T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        set_tree234.insert_new2(E, T21, NewT21),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _),
                    set_tree234.insert_new3(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _),
                    set_tree234.insert_new2(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

set_tree234.insert_list(Es, Set0) = Set :-
    set_tree234.insert_list(Es, Set0, Set).

set_tree234.insert_list([], !Set).
set_tree234.insert_list([E | Es], !Set) :-
    set_tree234.insert(E, !Set),
    set_tree234.insert_list(Es, !Set).

%---------------------------------------------------------------------------%

:- pred set_tree234.split_four(set_tree234(E)::in_four, E::out,
    set_tree234(E)::out_two, set_tree234(E)::out_two) is det.

set_tree234.split_four(Tin, MidE, Sub0, Sub1) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    Sub0 = two(E0, T0, T1),
    MidE = E1,
    Sub1 = two(E2, T2, T3).

%---------------------------------------------------------------------------%

set_tree234.delete(E, Tin) = Tout :-
    set_tree234.delete(E, Tin, Tout).

set_tree234.delete(E, Tin, Tout) :-
    set_tree234.delete_2(E, Tin, Tout, _).

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.

:- pred set_tree234.delete_2(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is det.

set_tree234.delete_2(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        Tout = empty,
        RH = no
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            set_tree234.delete_2(E, T0, NewT0, RHT0),
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
            (
                set_tree234.remove_least_2(T1, ST1E,  NewT1, RHT1)
            ->
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1E, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1E, T0, NewT1),
                    RH = no
                )
            ;
                % T1 must be empty
                Tout = T0,
                RH = yes
            )
        ;
            Result0 = (>),
            set_tree234.delete_2(E, T1, NewT1, RHT1),
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
            set_tree234.delete_2(E, T0, NewT0, RHT0),
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
            (
                set_tree234.remove_least_2(T1, ST1E, NewT1, RHT1)
            ->
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1E, E1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1E, E1, T0, NewT1, T2),
                    RH = no
                )
            ;
                % T1 must be empty
                Tout = two(E1, T0, T2),
                RH = no
            )
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                set_tree234.delete_2(E, T1, NewT1, RHT1),
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
                (
                    set_tree234.remove_least_2(T2, ST2E, NewT2, RHT2)
                ->
                    (
                        RHT2 = yes,
                        fix_3node_t2(E0, ST2E, T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(E0, ST2E, T0, T1, NewT2),
                        RH = no
                    )
                ;
                    % T2 must be empty
                    Tout = two(E0, T0, T1),
                    RH = no
                )
            ;
                Result1 = (>),
                set_tree234.delete_2(E, T2, NewT2, RHT2),
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
                set_tree234.delete_2(E, T0, NewT0, RHT0),
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
                (
                    set_tree234.remove_least_2(T1, ST1E, NewT1, RHT1)
                ->
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1E, E1, E2, T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1E, E1, E2, T0, NewT1, T2, T3),
                        RH = no
                    )
                ;
                    % T1 must be empty
                    Tout = three(E1, E2, T0, T2, T3),
                    RH = no
                )
            ;
                Result0 = (>),
                set_tree234.delete_2(E, T1, NewT1, RHT1),
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
            (
                set_tree234.remove_least_2(T2, ST2E, NewT2, RHT2)
            ->
                (
                    RHT2 = yes,
                    fix_4node_t2(E0, ST2E, E2, T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(E0, ST2E, E2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                % T2 must be empty
                Tout = three(E0, E2, T0, T1, T3),
                RH = no
            )
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                set_tree234.delete_2(E, T2, NewT2, RHT2),
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
                (
                    set_tree234.remove_least_2(T3, ST3E, NewT3, RHT3)
                ->
                    (
                        RHT3 = yes,
                        fix_4node_t3(E0, E1, ST3E, T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(E0, E1, ST3E, T0, T1, T2, NewT3),
                        RH = no
                    )
                ;
                    % T3 must be empty
                    Tout = three(E0, E1, T0, T1, T2),
                    RH = no
                )
            ;
                Result2 = (>),
                set_tree234.delete_2(E, T3, NewT3, RHT3),
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

set_tree234.delete_list(SetA, SetB) = Set:-
    set_tree234.delete_list(SetA, SetB, Set).

set_tree234.delete_list([], !Set).
set_tree234.delete_list([E | Es], !Set) :-
    set_tree234.delete(E, !Set),
    set_tree234.delete_list(Es, !Set).

%---------------------------------------------------------------------------%

    % We use the same algorithm as set_tree234.delete.

set_tree234.remove(E, Tin, Tout) :-
    set_tree234.remove_2(E, Tin, Tout, _).

:- pred set_tree234.remove_2(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is semidet.

set_tree234.remove_2(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            set_tree234.remove_2(E, T0, NewT0, RHT0),
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
            (
                set_tree234.remove_least_2(T1, ST1E, NewT1, RHT1)
            ->
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1E, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1E, T0, NewT1),
                    RH = no
                )
            ;
                % T1 must be empty
                Tout = T0,
                RH = yes
            )
        ;
            Result0 = (>),
            set_tree234.remove_2(E, T1, NewT1, RHT1),
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
            set_tree234.remove_2(E, T0, NewT0, RHT0),
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
            (
                set_tree234.remove_least_2(T1, ST1E, NewT1, RHT1)
            ->
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1E, E1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1E, E1, T0, NewT1, T2),
                    RH = no
                )
            ;
                % T1 must be empty
                Tout = two(E1, T0, T2),
                RH = no
            )
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                set_tree234.remove_2(E, T1, NewT1, RHT1),
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
                (
                    set_tree234.remove_least_2(T2, ST2E, NewT2, RHT2)
                ->
                    (
                        RHT2 = yes,
                        fix_3node_t2(E0, ST2E, T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(E0, ST2E, T0, T1, NewT2),
                        RH = no
                    )
                ;
                    % T2 must be empty
                    Tout = two(E0, T0, T1),
                    RH = no
                )
            ;
                Result1 = (>),
                set_tree234.remove_2(E, T2, NewT2, RHT2),
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
                set_tree234.remove_2(E, T0, NewT0, RHT0),
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
                (
                    set_tree234.remove_least_2(T1, ST1E, NewT1, RHT1)
                ->
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1E, E1, E2, T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1E, E1, E2, T0, NewT1, T2, T3),
                        RH = no
                    )
                ;
                    % T1 must be empty
                    Tout = three(E1, E2, T0, T2, T3),
                    RH = no
                )
            ;
                Result0 = (>),
                set_tree234.remove_2(E, T1, NewT1, RHT1),
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
            (
                set_tree234.remove_least_2(T2, ST2E, NewT2, RHT2)
            ->
                (
                    RHT2 = yes,
                    fix_4node_t2(E0, ST2E, E2, T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(E0, ST2E, E2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                % T2 must be empty
                Tout = three(E0, E2, T0, T1, T3),
                RH = no
            )
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                set_tree234.remove_2(E, T2, NewT2, RHT2),
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
                (
                    set_tree234.remove_least_2(T3, ST3E, NewT3, RHT3)
                ->
                    (
                        RHT3 = yes,
                        fix_4node_t3(E0, E1, ST3E, T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(E0, E1, ST3E, T0, T1, T2, NewT3),
                        RH = no
                    )
                ;
                    % T3 must be empty
                    Tout = three(E0, E1, T0, T1, T2),
                    RH = no
                )
            ;
                Result2 = (>),
                set_tree234.remove_2(E, T3, NewT3, RHT3),
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

set_tree234.remove_list([], !Set).
set_tree234.remove_list([E | Es], !Set) :-
    set_tree234.remove(E, !Set),
    set_tree234.remove_list(Es, !Set).

%---------------------------------------------------------------------------%

    % The algorithm we use similar to set_tree234.delete, except that we
    % always go down the left subtree.

set_tree234.remove_least(E, Tin, Tout) :-
    set_tree234.remove_least_2(Tin, E, Tout, _).

:- pred set_tree234.remove_least_2(set_tree234(E)::in, E::out,
    set_tree234(E)::out, bool::out) is semidet.

set_tree234.remove_least_2(Tin, E, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(E0, T0, T1),
        (
            T0 = empty
        ->
            E = E0,
            Tout = T1,
            RH = yes
        ;
            set_tree234.remove_least_2(T0, E, NewT0, RHT0),
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
        (
            T0 = empty
        ->
            E = E0,
            Tout = two(E1, T1, T2),
            RH = no
        ;
            set_tree234.remove_least_2(T0, E, NewT0, RHT0),
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
        (
            T0 = empty
        ->
            E = E0,
            Tout = three(E1, E2, T1, T2, T3),
            RH = no
        ;
            set_tree234.remove_least_2(T0, E, NewT0, RHT0),
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

%---------------------------------------------------------------------------%

    % The input to the following group of predicates are the components
    % of a two-, three- or four-node in which the height of the indicated
    % subtree is one less that it should be. If it is possible to increase
    % the height of that subtree by moving into it elements from its
    % neighboring subtrees, do so, and return the resulting tree with RH
    % set to no. Otherwise, return a balanced tree whose height is reduced
    % by one, with RH set to yes to indicate the reduced height.

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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
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
        error("unbalanced 234 tree")
        % Tout = four(E0, E1, E2, T0, T1, T2, T3),
        % The heights of T0, T1 and T2 are unchanged
        % RH = no
    ).

%---------------------------------------------------------------------------%

set_tree234.union(SetA, SetB) = Set :-
    set_tree234.union(SetA, SetB, Set).

set_tree234.union(SetA, SetB, Set) :-
    % The amount of work that do_union has to do is proportional to the
    % number of elements in its first argument. We therefore want to pick
    % the smaller input set to be the first argument.
    %
    % We could count the number of arguments in both sets, but computing the
    % tree height is *much* faster, and almost as precise.
    set_tree234.height(SetA, HeightA),
    set_tree234.height(SetB, HeightB),
    ( HeightA =< HeightB ->
        set_tree234.do_union(SetA, SetB, Set)
    ;
        set_tree234.do_union(SetB, SetA, Set)
    ).

:- pred set_tree234.do_union(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

set_tree234.do_union(empty, !Set).
set_tree234.do_union(two(E0, T0, T1), !Set) :-
    set_tree234.do_union(T0, !Set),
    set_tree234.insert(E0, !Set),
    set_tree234.do_union(T1, !Set).
set_tree234.do_union(three(E0, E1, T0, T1, T2), !Set) :-
    set_tree234.do_union(T0, !Set),
    set_tree234.insert(E0, !Set),
    set_tree234.do_union(T1, !Set),
    set_tree234.insert(E1, !Set),
    set_tree234.do_union(T2, !Set).
set_tree234.do_union(four(E0, E1, E2, T0, T1, T2, T3), !Set) :-
    set_tree234.do_union(T0, !Set),
    set_tree234.insert(E0, !Set),
    set_tree234.do_union(T1, !Set),
    set_tree234.insert(E1, !Set),
    set_tree234.do_union(T2, !Set),
    set_tree234.insert(E2, !Set),
    set_tree234.do_union(T3, !Set).

set_tree234.union_list(Sets) = Union :-
    set_tree234.union_list(Sets, Union).

set_tree234.union_list([], empty).
set_tree234.union_list([Set | Sets], Union) :-
    set_tree234.union_list(Sets, Union1),
    set_tree234.union(Set, Union1, Union).

set_tree234.power_union(Sets) = Union :-
    set_tree234.power_union(Sets, Union).

set_tree234.power_union(Sets, Union) :-
    set_tree234.power_union_2(Sets, empty, Union).

:- pred set_tree234.power_union_2(set_tree234(set_tree234(T))::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.

set_tree234.power_union_2(empty, !Union).
set_tree234.power_union_2(two(E0, T0, T1), !Union) :-
    set_tree234.power_union_2(T0, !Union),
    set_tree234.union(E0, !Union),
    set_tree234.power_union_2(T1, !Union).
set_tree234.power_union_2(three(E0, E1, T0, T1, T2), !Union) :-
    set_tree234.power_union_2(T0, !Union),
    set_tree234.union(E0, !Union),
    set_tree234.power_union_2(T1, !Union),
    set_tree234.union(E1, !Union),
    set_tree234.power_union_2(T2, !Union).
set_tree234.power_union_2(four(E0, E1, E2, T0, T1, T2, T3), !Union) :-
    set_tree234.power_union_2(T0, !Union),
    set_tree234.union(E0, !Union),
    set_tree234.power_union_2(T1, !Union),
    set_tree234.union(E1, !Union),
    set_tree234.power_union_2(T2, !Union),
    set_tree234.union(E2, !Union),
    set_tree234.power_union_2(T3, !Union).

%---------------------------------------------------------------------------%

set_tree234.intersect(SetA, SetB) = Set :-
    set_tree234.intersect(SetA, SetB, Set).

set_tree234.intersect(SetA, SetB, Intersect) :-
    % The amount of work that do_intersect has to do is proportional to the
    % number of elements in its first argument. We therefore want to pick
    % the smaller input set to be the first argument.
    %
    % We could count the number of arguments in both sets, but computing the
    % tree height is *much* faster, and almost as precise.
    set_tree234.height(SetA, HeightA),
    set_tree234.height(SetB, HeightB),
    ( HeightA =< HeightB ->
        set_tree234.do_intersect(SetA, SetB, empty, Intersect)
    ;
        set_tree234.do_intersect(SetB, SetA, empty, Intersect)
    ).

:- pred set_tree234.do_intersect(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.

set_tree234.do_intersect(empty, _SetB, !Intersect).
set_tree234.do_intersect(two(E0, T0, T1), SetB, !Intersect) :-
    set_tree234.do_intersect(T0, SetB, !Intersect),
    ( set_tree234.contains(SetB, E0) ->
        set_tree234.insert(E0, !Intersect)
    ;
        true
    ),
    set_tree234.do_intersect(T1, SetB, !Intersect).
set_tree234.do_intersect(three(E0, E1, T0, T1, T2), SetB, !Intersect) :-
    set_tree234.do_intersect(T0, SetB, !Intersect),
    ( set_tree234.contains(SetB, E0) ->
        set_tree234.insert(E0, !Intersect)
    ;
        true
    ),
    set_tree234.do_intersect(T1, SetB, !Intersect),
    ( set_tree234.contains(SetB, E1) ->
        set_tree234.insert(E1, !Intersect)
    ;
        true
    ),
    set_tree234.do_intersect(T2, SetB, !Intersect).
set_tree234.do_intersect(four(E0, E1, E2, T0, T1, T2, T3), SetB, !Intersect) :-
    set_tree234.do_intersect(T0, SetB, !Intersect),
    ( set_tree234.contains(SetB, E0) ->
        set_tree234.insert(E0, !Intersect)
    ;
        true
    ),
    set_tree234.do_intersect(T1, SetB, !Intersect),
    ( set_tree234.contains(SetB, E1) ->
        set_tree234.insert(E1, !Intersect)
    ;
        true
    ),
    set_tree234.do_intersect(T2, SetB, !Intersect),
    ( set_tree234.contains(SetB, E2) ->
        set_tree234.insert(E2, !Intersect)
    ;
        true
    ),
    set_tree234.do_intersect(T3, SetB, !Intersect).

set_tree234.intersect_list(Sets) = Intersect :-
    set_tree234.intersect_list(Sets, Intersect).

set_tree234.intersect_list([], empty).
set_tree234.intersect_list([Set | Sets], Intersect) :-
    Intersect = set_tree234.intersect_list_2(Set, Sets).

:- func set_tree234.intersect_list_2(set_tree234(T), list(set_tree234(T)))
    = set_tree234(T).

set_tree234.intersect_list_2(Set, []) = Set.
set_tree234.intersect_list_2(Set, [Head | Tail]) =
    ( Set = empty ->
        empty
    ;
        set_tree234.intersect_list_2(set_tree234.intersect(Set, Head), Tail)
    ).

set_tree234.power_intersect(Sets) = Intersect :-
    set_tree234.power_intersect(Sets, Intersect).

set_tree234.power_intersect(Sets, Intersect) :-
    Intersect = set_tree234.intersect_list(set_tree234.to_sorted_list(Sets)).

%---------------------------------------------------------------------------%

    % `set_tree234.difference(SetA, SetB, Set)' is true iff `Set' is the
    % set containing all the elements of `SetA' except those that
    % occur in `SetB'.

set_tree234.difference(SetA, SetB) = Diff :-
    set_tree234.difference(SetA, SetB, Diff).

set_tree234.difference(SetA, SetB, Diff) :-
    set_tree234.difference_2(SetB, SetA, Diff).

:- pred set_tree234.difference_2(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

set_tree234.difference_2(empty, !Set).
set_tree234.difference_2(two(E0, T0, T1), !Set) :-
    set_tree234.difference_2(T0, !Set),
    set_tree234.delete(E0, !Set),
    set_tree234.difference_2(T1, !Set).
set_tree234.difference_2(three(E0, E1, T0, T1, T2), !Set) :-
    set_tree234.difference_2(T0, !Set),
    set_tree234.delete(E0, !Set),
    set_tree234.difference_2(T1, !Set),
    set_tree234.delete(E1, !Set),
    set_tree234.difference_2(T2, !Set).
set_tree234.difference_2(four(E0, E1, E2, T0, T1, T2, T3), !Set) :-
    set_tree234.difference_2(T0, !Set),
    set_tree234.delete(E0, !Set),
    set_tree234.difference_2(T1, !Set),
    set_tree234.delete(E1, !Set),
    set_tree234.difference_2(T2, !Set),
    set_tree234.delete(E2, !Set),
    set_tree234.difference_2(T3, !Set).

%---------------------------------------------------------------------------%

set_tree234.count(empty) = 0.
set_tree234.count(two(_, T0, T1)) = N :-
    N0 = set_tree234.count(T0),
    N1 = set_tree234.count(T1),
    N = 1 + N0 + N1.
set_tree234.count(three(_, _, T0, T1, T2)) = N :-
    N0 = set_tree234.count(T0),
    N1 = set_tree234.count(T1),
    N2 = set_tree234.count(T2),
    N = 2 + N0 + N1 + N2.
set_tree234.count(four(_, _, _, T0, T1, T2, T3)) = N :-
    N0 = set_tree234.count(T0),
    N1 = set_tree234.count(T1),
    N2 = set_tree234.count(T2),
    N3 = set_tree234.count(T3),
    N = 3 + N0 + N1 + N2 + N3.

:- pred set_tree234.height(set_tree234(T)::in, int::out) is det.

set_tree234.height(Tree, Height) :-
    (
        Tree = empty,
        Height = 0
    ;
        ( Tree = two(_, T0, _)
        ; Tree = three(_, _, T0, _, _)
        ; Tree = four(_, _, _, T0, _, _, _)
        ),
        set_tree234.height(T0, T0Height),
        Height = T0Height + 1
    ).

%---------------------------------------------------------------------------%

set_tree234.fold(Pred, Tree, !A) :-
    set_tree234.foldl(Pred, Tree, !A).

set_tree234.foldl(_Pred, empty, !A).
set_tree234.foldl(Pred, two(E, T0, T1), !A) :-
    set_tree234.foldl(Pred, T0, !A),
    Pred(E, !A),
    set_tree234.foldl(Pred, T1, !A).
set_tree234.foldl(Pred, three(E0, E1, T0, T1, T2), !A) :-
    set_tree234.foldl(Pred, T0, !A),
    Pred(E0, !A),
    set_tree234.foldl(Pred, T1, !A),
    Pred(E1, !A),
    set_tree234.foldl(Pred, T2, !A).
set_tree234.foldl(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A) :-
    set_tree234.foldl(Pred, T0, !A),
    Pred(E0, !A),
    set_tree234.foldl(Pred, T1, !A),
    Pred(E1, !A),
    set_tree234.foldl(Pred, T2, !A),
    Pred(E2, !A),
    set_tree234.foldl(Pred, T3, !A).

set_tree234.fold(Func, Tree, A0) =
    set_tree234.foldl(Func, Tree, A0).

set_tree234.foldl(_Func, empty, A) = A.
set_tree234.foldl(Func, two(E, T0, T1), !.A) = !:A :-
    set_tree234.foldl(Func, T0, !.A) = !:A,
    !:A = Func(E, !.A),
    set_tree234.foldl(Func, T1, !.A) = !:A.
set_tree234.foldl(Func, three(E0, E1, T0, T1, T2), !.A) = !:A :-
    set_tree234.foldl(Func, T0, !.A) = !:A,
    !:A = Func(E0, !.A),
    set_tree234.foldl(Func, T1, !.A) = !:A,
    !:A = Func(E1, !.A),
    set_tree234.foldl(Func, T2, !.A) = !:A.
set_tree234.foldl(Func, four(E0, E1, E2, T0, T1, T2, T3), !.A) = !:A :-
    set_tree234.foldl(Func, T0, !.A) = !:A,
    !:A = Func(E0, !.A),
    set_tree234.foldl(Func, T1, !.A) = !:A,
    !:A = Func(E1, !.A),
    set_tree234.foldl(Func, T2, !.A) = !:A,
    !:A = Func(E2, !.A),
    set_tree234.foldl(Func, T3, !.A) = !:A.

set_tree234.fold2(Pred, Tree, !A, !B) :-
    set_tree234.foldl2(Pred, Tree, !A, !B).

set_tree234.foldl2(_Pred, empty, !A, !B).
set_tree234.foldl2(Pred, two(E, T0, T1), !A, !B) :-
    set_tree234.foldl2(Pred, T0, !A, !B),
    Pred(E, !A, !B),
    set_tree234.foldl2(Pred, T1, !A, !B).
set_tree234.foldl2(Pred, three(E0, E1, T0, T1, T2), !A, !B) :-
    set_tree234.foldl2(Pred, T0, !A, !B),
    Pred(E0, !A, !B),
    set_tree234.foldl2(Pred, T1, !A, !B),
    Pred(E1, !A, !B),
    set_tree234.foldl2(Pred, T2, !A, !B).
set_tree234.foldl2(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B) :-
    set_tree234.foldl2(Pred, T0, !A, !B),
    Pred(E0, !A, !B),
    set_tree234.foldl2(Pred, T1, !A, !B),
    Pred(E1, !A, !B),
    set_tree234.foldl2(Pred, T2, !A, !B),
    Pred(E2, !A, !B),
    set_tree234.foldl2(Pred, T3, !A, !B).

set_tree234.fold3(Pred, Tree, !A, !B, !C) :-
    set_tree234.foldl3(Pred, Tree, !A, !B, !C).

set_tree234.foldl3(_Pred, empty, !A, !B, !C).
set_tree234.foldl3(Pred, two(E, T0, T1), !A, !B, !C) :-
    set_tree234.foldl3(Pred, T0, !A, !B, !C),
    Pred(E, !A, !B, !C),
    set_tree234.foldl3(Pred, T1, !A, !B, !C).
set_tree234.foldl3(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C) :-
    set_tree234.foldl3(Pred, T0, !A, !B, !C),
    Pred(E0, !A, !B, !C),
    set_tree234.foldl3(Pred, T1, !A, !B, !C),
    Pred(E1, !A, !B, !C),
    set_tree234.foldl3(Pred, T2, !A, !B, !C).
set_tree234.foldl3(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C) :-
    set_tree234.foldl3(Pred, T0, !A, !B, !C),
    Pred(E0, !A, !B, !C),
    set_tree234.foldl3(Pred, T1, !A, !B, !C),
    Pred(E1, !A, !B, !C),
    set_tree234.foldl3(Pred, T2, !A, !B, !C),
    Pred(E2, !A, !B, !C),
    set_tree234.foldl3(Pred, T3, !A, !B, !C).

set_tree234.fold4(Pred, Tree, !A, !B, !C, !D) :-
    set_tree234.foldl4(Pred, Tree, !A, !B, !C, !D).

set_tree234.foldl4(_Pred, empty, !A, !B, !C, !D).
set_tree234.foldl4(Pred, two(E, T0, T1), !A, !B, !C, !D) :-
    set_tree234.foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(E, !A, !B, !C, !D),
    set_tree234.foldl4(Pred, T1, !A, !B, !C, !D).
set_tree234.foldl4(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D) :-
    set_tree234.foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(E0, !A, !B, !C, !D),
    set_tree234.foldl4(Pred, T1, !A, !B, !C, !D),
    Pred(E1, !A, !B, !C, !D),
    set_tree234.foldl4(Pred, T2, !A, !B, !C, !D).
set_tree234.foldl4(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C, !D) :-
    set_tree234.foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(E0, !A, !B, !C, !D),
    set_tree234.foldl4(Pred, T1, !A, !B, !C, !D),
    Pred(E1, !A, !B, !C, !D),
    set_tree234.foldl4(Pred, T2, !A, !B, !C, !D),
    Pred(E2, !A, !B, !C, !D),
    set_tree234.foldl4(Pred, T3, !A, !B, !C, !D).

set_tree234.fold5(Pred, Tree, !A, !B, !C, !D, !E) :-
    set_tree234.foldl5(Pred, Tree, !A, !B, !C, !D, !E).

set_tree234.foldl5(_Pred, empty, !A, !B, !C, !D, !E).
set_tree234.foldl5(Pred, two(E, T0, T1), !A, !B, !C, !D, !E) :-
    set_tree234.foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E, !A, !B, !C, !D, !E),
    set_tree234.foldl5(Pred, T1, !A, !B, !C, !D, !E).
set_tree234.foldl5(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D, !E) :-
    set_tree234.foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E0, !A, !B, !C, !D, !E),
    set_tree234.foldl5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(E1, !A, !B, !C, !D, !E),
    set_tree234.foldl5(Pred, T2, !A, !B, !C, !D, !E).
set_tree234.foldl5(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C, !D,
        !E) :-
    set_tree234.foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E0, !A, !B, !C, !D, !E),
    set_tree234.foldl5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(E1, !A, !B, !C, !D, !E),
    set_tree234.foldl5(Pred, T2, !A, !B, !C, !D, !E),
    Pred(E2, !A, !B, !C, !D, !E),
    set_tree234.foldl5(Pred, T3, !A, !B, !C, !D, !E).

set_tree234.fold6(Pred, Tree, !A, !B, !C, !D, !E, !F) :-
    set_tree234.foldl6(Pred, Tree, !A, !B, !C, !D, !E, !F).

set_tree234.foldl6(_Pred, empty, !A, !B, !C, !D, !E, !F).
set_tree234.foldl6(Pred, two(E, T0, T1), !A, !B, !C, !D, !E, !F) :-
    set_tree234.foldl6(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E, !A, !B, !C, !D, !E, !F),
    set_tree234.foldl6(Pred, T1, !A, !B, !C, !D, !E, !F).
set_tree234.foldl6(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D, !E, !F) :-
    set_tree234.foldl6(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E0, !A, !B, !C, !D, !E, !F),
    set_tree234.foldl6(Pred, T1, !A, !B, !C, !D, !E, !F),
    Pred(E1, !A, !B, !C, !D, !E, !F),
    set_tree234.foldl6(Pred, T2, !A, !B, !C, !D, !E, !F).
set_tree234.foldl6(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B, !C, !D,
        !E, !F) :-
    set_tree234.foldl6(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E0, !A, !B, !C, !D, !E, !F),
    set_tree234.foldl6(Pred, T1, !A, !B, !C, !D, !E, !F),
    Pred(E1, !A, !B, !C, !D, !E, !F),
    set_tree234.foldl6(Pred, T2, !A, !B, !C, !D, !E, !F),
    Pred(E2, !A, !B, !C, !D, !E, !F),
    set_tree234.foldl6(Pred, T3, !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%

set_tree234.all_true(Pred, T) :-
    (
        T = empty
    ;
        T = two(E0, T0, T1),
        set_tree234.all_true(Pred, T0),
        Pred(E0),
        set_tree234.all_true(Pred, T1)
    ;
        T = three(E0, E1, T0, T1, T2),
        set_tree234.all_true(Pred, T0),
        Pred(E0),
        set_tree234.all_true(Pred, T1),
        Pred(E1),
        set_tree234.all_true(Pred, T2)
    ;
        T = four(E0, E1, E2, T0, T1, T2, T3),
        set_tree234.all_true(Pred, T0),
        Pred(E0),
        set_tree234.all_true(Pred, T1),
        Pred(E1),
        set_tree234.all_true(Pred, T2),
        Pred(E2),
        set_tree234.all_true(Pred, T3)
    ).

%---------------------------------------------------------------------------%

set_tree234.map(Pred, SetA, SetB) :-
    set_tree234.map_pred(Pred, SetA, [], ListB),
    SetB = set_tree234.list_to_set(ListB).

:- pred set_tree234.map_pred(pred(T1, T2)::in(pred(in, out) is det),
    set_tree234(T1)::in, list(T2)::in, list(T2)::out) is det.

set_tree234.map_pred(_Pred, empty, !List).
set_tree234.map_pred(Pred, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234.map_pred(Pred, T0, !List),
    Pred(E0, N0),
    !:List = [N0 | !.List],
    set_tree234.map_pred(Pred, T1, !List).
set_tree234.map_pred(Pred, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234.map_pred(Pred, T0, !List),
    Pred(E0, N0),
    !:List = [N0 | !.List],
    set_tree234.map_pred(Pred, T1, !List),
    Pred(E1, N1),
    !:List = [N1 | !.List],
    set_tree234.map_pred(Pred, T2, !List).
set_tree234.map_pred(Pred, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234.map_pred(Pred, T0, !List),
    Pred(E0, N0),
    !:List = [N0 | !.List],
    set_tree234.map_pred(Pred, T1, !List),
    Pred(E1, N1),
    !:List = [N1 | !.List],
    set_tree234.map_pred(Pred, T2, !List),
    Pred(E2, N2),
    !:List = [N2 | !.List],
    set_tree234.map_pred(Pred, T3, !List).

set_tree234.map(Func, SetA) = SetB :-
    set_tree234.map_func(Func, SetA, [], ListB),
    SetB = set_tree234.list_to_set(ListB).

:- pred set_tree234.map_func(func(T1) = T2, set_tree234(T1),
    list(T2), list(T2)).
:- mode set_tree234.map_func(in(func(in) = out is det), in, in, out) is det.

set_tree234.map_func(_Func, empty, !List).
set_tree234.map_func(Func, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234.map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    set_tree234.map_func(Func, T1, !List).
set_tree234.map_func(Func, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234.map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    set_tree234.map_func(Func, T1, !List),
    N1 = Func(E1),
    !:List = [N1 | !.List],
    set_tree234.map_func(Func, T2, !List).
set_tree234.map_func(Func, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234.map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    set_tree234.map_func(Func, T1, !List),
    N1 = Func(E1),
    !:List = [N1 | !.List],
    set_tree234.map_func(Func, T2, !List),
    N2 = Func(E2),
    !:List = [N2 | !.List],
    set_tree234.map_func(Func, T3, !List).

%---------------------------------------------------------------------------%

set_tree234.filter_map(Pred, SetA, SetB) :-
    set_tree234.filter_map_pred(Pred, SetA, [], ListB),
    SetB = set_tree234.list_to_set(ListB).

:- pred set_tree234.filter_map_pred(
    pred(T1, T2)::in(pred(in, out) is semidet), set_tree234(T1)::in,
    list(T2)::in, list(T2)::out) is det.

set_tree234.filter_map_pred(_Pred, empty, !List).
set_tree234.filter_map_pred(Pred, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234.filter_map_pred(Pred, T0, !List),
    ( Pred(E0, N0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_pred(Pred, T1, !List).
set_tree234.filter_map_pred(Pred, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234.filter_map_pred(Pred, T0, !List),
    ( Pred(E0, N0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_pred(Pred, T1, !List),
    ( Pred(E1, N1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_pred(Pred, T2, !List).
set_tree234.filter_map_pred(Pred, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234.filter_map_pred(Pred, T0, !List),
    ( Pred(E0, N0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_pred(Pred, T1, !List),
    ( Pred(E1, N1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_pred(Pred, T2, !List),
    ( Pred(E2, N2) ->
        !:List = [N2 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_pred(Pred, T3, !List).

set_tree234.filter_map(Func, SetA) = SetB :-
    set_tree234.filter_map_func(Func, SetA, [], ListB),
    SetB = set_tree234.list_to_set(ListB).

:- pred set_tree234.filter_map_func(func(T1) = T2, set_tree234(T1),
    list(T2), list(T2)).
:- mode set_tree234.filter_map_func(in(func(in) = out is semidet),
    in, in, out) is det.

set_tree234.filter_map_func(_Func, empty, !List).
set_tree234.filter_map_func(Func, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234.filter_map_func(Func, T0, !List),
    ( N0 = Func(E0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_func(Func, T1, !List).
set_tree234.filter_map_func(Func, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234.filter_map_func(Func, T0, !List),
    ( N0 = Func(E0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_func(Func, T1, !List),
    ( N1 = Func(E1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_func(Func, T2, !List).
set_tree234.filter_map_func(Func, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234.filter_map_func(Func, T0, !List),
    ( N0 = Func(E0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_func(Func, T1, !List),
    ( N1 = Func(E1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_func(Func, T2, !List),
    ( N2 = Func(E2) ->
        !:List = [N2 | !.List]
    ;
        true
    ),
    set_tree234.filter_map_func(Func, T3, !List).

%---------------------------------------------------------------------------%

set_tree234.filter(Pred, Set) = TrueSet :-
    % XXX This should be more efficient.
    set_tree234.divide(Pred, Set, TrueSet, _FalseSet).

set_tree234.filter(Pred, Set, TrueSet) :-
    % XXX This should be more efficient.
    set_tree234.divide(Pred, Set, TrueSet, _FalseSet).

set_tree234.filter(Pred, Set, TrueSet, FalseSet) :-
    set_tree234.divide(Pred, Set, TrueSet, FalseSet).

set_tree234.divide(Pred, Set, TrueSet, FalseSet) :-
    set_tree234.divide_2(Pred, Set, empty, TrueSet, empty, FalseSet).

:- pred set_tree234.divide_2(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

    % XXX This should be more efficient.
set_tree234.divide_2(_Pred, empty, !TrueSet, !FalseSet).
set_tree234.divide_2(Pred, Tin, !TrueSet, !FalseSet) :-
    Tin = two(E0, T0, T1),
    set_tree234.divide_2(Pred, T0, !TrueSet, !FalseSet),
    ( Pred(E0) ->
        set_tree234.insert(E0, !TrueSet)
    ;
        set_tree234.insert(E0, !FalseSet)
    ),
    set_tree234.divide_2(Pred, T1, !TrueSet, !FalseSet).
set_tree234.divide_2(Pred, Tin, !TrueSet, !FalseSet) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234.divide_2(Pred, T0, !TrueSet, !FalseSet),
    ( Pred(E0) ->
        set_tree234.insert(E0, !TrueSet)
    ;
        set_tree234.insert(E0, !FalseSet)
    ),
    set_tree234.divide_2(Pred, T1, !TrueSet, !FalseSet),
    ( Pred(E1) ->
        set_tree234.insert(E1, !TrueSet)
    ;
        set_tree234.insert(E1, !FalseSet)
    ),
    set_tree234.divide_2(Pred, T2, !TrueSet, !FalseSet).
set_tree234.divide_2(Pred, Tin, !TrueSet, !FalseSet) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234.divide_2(Pred, T0, !TrueSet, !FalseSet),
    ( Pred(E0) ->
        set_tree234.insert(E0, !TrueSet)
    ;
        set_tree234.insert(E0, !FalseSet)
    ),
    set_tree234.divide_2(Pred, T1, !TrueSet, !FalseSet),
    ( Pred(E1) ->
        set_tree234.insert(E1, !TrueSet)
    ;
        set_tree234.insert(E1, !FalseSet)
    ),
    set_tree234.divide_2(Pred, T2, !TrueSet, !FalseSet),
    ( Pred(E2) ->
        set_tree234.insert(E2, !TrueSet)
    ;
        set_tree234.insert(E2, !FalseSet)
    ),
    set_tree234.divide_2(Pred, T3, !TrueSet, !FalseSet).

set_tree234.divide_by_set(DivideBySet, Set, TrueSet, FalseSet) :-
    % XXX This should be more efficient.
    set_tree234.divide(set_tree234.contains(DivideBySet), Set,
        TrueSet, FalseSet).

%---------------------------------------------------------------------------%
