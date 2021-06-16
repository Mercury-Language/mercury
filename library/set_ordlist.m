%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-1997,1999-2002, 2004-2006, 2008-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: set_ordlist.m.
% Main authors: conway, fjh.
% Stability: medium.
%
% This file contains a `set' ADT.
% Sets are implemented here as sorted lists without duplicates.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module set_ordlist.
:- interface.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

:- type set_ordlist(_T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % `init(Set)' is true iff `Set' is an empty set.
    %
:- func init = set_ordlist(T).
:- pred init(set_ordlist(_T)::uo) is det.

    % `singleton_set(Elem, Set)' is true iff `Set' is the set containing just
    % the single element `Elem'.
    %
:- pred singleton_set(T, set_ordlist(T)).
:- mode singleton_set(in, out) is det.
:- mode singleton_set(out, in) is semidet.

:- func make_singleton_set(T) = set_ordlist(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

    % `empty(Set)' is true iff `Set' is an empty set.
    % `is_empty' is a synonym for `empty'.
    %
:- pred empty(set_ordlist(_T)::in) is semidet.
:- pred is_empty(set_ordlist(T)::in) is semidet.
:- pragma obsolete(pred(empty/1), [is_empty/1]).

    % `non_empty(Set)' is true iff `Set' is not an empty set.
    % `is_non_empty' is a synonym for `non_empty'.
    %
:- pred non_empty(set_ordlist(T)::in) is semidet.
:- pred is_non_empty(set_ordlist(T)::in) is semidet.
:- pragma obsolete(pred(non_empty/1), [is_non_empty/1]).

:- pred is_singleton(set_ordlist(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred member(T, set_ordlist(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `is_member(X, Set, Result)' returns `Result = yes' iff `X' is a member of
    % `Set'.
    %
:- pred is_member(T::in, set_ordlist(T)::in, bool::out) is det.

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pred contains(set_ordlist(T)::in, T::in) is semidet.

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % `insert(X, Set0, Set)' is true iff `Set' is the union
    % of `Set0' and the set containing only `X'.
    %
:- func insert(set_ordlist(T), T) = set_ordlist(T).
:- pred insert(T::in, set_ordlist(T)::in, set_ordlist(T)::out) is det.

    % `insert_new(X, Set0, Set)' is true iff `Set0' does not contain `X', while
    % `Set' is the union of `Set0' and the set containing only `X'.
    %
:- pred insert_new(T::in,
    set_ordlist(T)::in, set_ordlist(T)::out) is semidet.

    % `insert_list(Xs, Set0, Set)' is true iff `Set' is the union of `Set0' and
    % the set containing only the members of `Xs'.
    %
:- func insert_list(set_ordlist(T), list(T)) = set_ordlist(T).
:- pred insert_list(list(T)::in, set_ordlist(T)::in, set_ordlist(T)::out)
    is det.

    % `delete(X, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- func delete(set_ordlist(T), T) = set_ordlist(T).
:- pred delete(T::in, set_ordlist(T)::in, set_ordlist(T)::out) is det.

    % `delete_list(Xs, Set0, Set)' is true iff `Set' is the relative complement
    % of `Set0' and the set containing only the members of `Xs'.
    %
:- func delete_list(set_ordlist(T), list(T)) = set_ordlist(T).
:- pred delete_list(list(T)::in, set_ordlist(T)::in, set_ordlist(T)::out)
    is det.

    % `remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e. if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
    % The det_remove version throws an exception instead of failing.
    %
:- pred remove(T::in, set_ordlist(T)::in, set_ordlist(T)::out) is semidet.
:- pred det_remove(T::in, set_ordlist(T)::in, set_ordlist(T)::out) is det.

    % `remove_list(Xs, Set0, Set)' is true iff Xs does not contain any
    % duplicates, `Set0' contains every member of `Xs', and `Set' is the
    % relative complement of `Set0' and the set containing only the members of
    % `Xs'.
    %
    % The det_remove_list version throws an exception instead of failing.
    %
:- pred remove_list(list(T)::in, set_ordlist(T)::in, set_ordlist(T)::out)
    is semidet.
:- pred det_remove_list(list(T)::in, set_ordlist(T)::in, set_ordlist(T)::out)
    is det.

    % `remove_least(X, Set0, Set)' is true iff `X' is the least element in
    % `Set0', and `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred remove_least(T::out, set_ordlist(T)::in, set_ordlist(T)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB' contain the same
    % elements.
    %
:- pred equal(set_ordlist(T)::in, set_ordlist(T)::in) is semidet.

    % `subset(SetA, SetB)' is true iff `SetA' is a subset of `SetB'.
    %
:- pred subset(set_ordlist(T)::in, set_ordlist(T)::in) is semidet.

    % `superset(SetA, SetB)' is true iff `SetA' is a superset of `SetB'.
    %
:- pred superset(set_ordlist(T)::in, set_ordlist(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % `union(SetA, SetB, Set)' is true iff `Set' is the union
    % of `SetA' and `SetB'. The efficiency of the union operation is
    % O(card(SetA)+card(SetB)) and is not sensitive to the argument
    % ordering.
    %
:- func union(set_ordlist(T), set_ordlist(T)) = set_ordlist(T).
:- pred union(set_ordlist(T)::in, set_ordlist(T)::in, set_ordlist(T)::out)
    is det.

    % `union_list(A, B)' is true iff `B' is the union of all the sets in `A'
    %
:- func union_list(list(set_ordlist(T))) = set_ordlist(T).
:- pred union_list(list(set_ordlist(T))::in, set_ordlist(T)::out) is det.

    % `power_union(A, B)' is true iff `B' is the union of
    % all the sets in `A'
    %
:- func power_union(set_ordlist(set_ordlist(T))) = set_ordlist(T).
:- pred power_union(set_ordlist(set_ordlist(T))::in,
    set_ordlist(T)::out) is det.

    % `intersect(SetA, SetB, Set)' is true iff `Set' is the intersection of
    % `SetA' and `SetB'. The efficiency of the intersection operation is not
    % influenced by the argument order.
    %
:- func intersect(set_ordlist(T), set_ordlist(T)) = set_ordlist(T).
:- pred intersect(set_ordlist(T), set_ordlist(T), set_ordlist(T)).
:- mode intersect(in, in, out) is det.
:- mode intersect(in, in, in) is semidet.

    % `intersect_list(A) = B' is true iff `B' is the intersection of all the
    % sets in `A'.
    %
:- func intersect_list(list(set_ordlist(T))) = set_ordlist(T).
:- pred intersect_list(list(set_ordlist(T))::in, set_ordlist(T)::out) is det.

    % `power_intersect(A, B)' is true iff `B' is the intersection of all the
    % sets in `A'.
    %
:- func power_intersect(set_ordlist(set_ordlist(T)))
    = set_ordlist(T).
:- pred power_intersect(set_ordlist(set_ordlist(T))::in,
    set_ordlist(T)::out) is det.

    % `difference(SetA, SetB, Set)' is true iff `Set' is the
    % set containing all the elements of `SetA' except those that
    % occur in `SetB'.
    %
:- func difference(set_ordlist(T), set_ordlist(T)) = set_ordlist(T).
:- pred difference(set_ordlist(T)::in, set_ordlist(T)::in,
    set_ordlist(T)::out) is det.

    % intersection_and_differences(SetA, SetB, InAandB, OnlyInA, OnlyInB):
    % Given SetA and SetB, return the elements that occur in both sets,
    % and those that occur only in one or the other.
    %
:- pred intersection_and_differences(set_ordlist(T)::in, set_ordlist(T)::in,
    set_ordlist(T)::out, set_ordlist(T)::out, set_ordlist(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    %
:- pred divide(pred(T)::in(pred(in) is semidet),
    set_ordlist(T)::in, set_ordlist(T)::out, set_ordlist(T)::out) is det.

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in DivideBySet;
    % OutPart consists of those elements of Set which are not in DivideBySet.
    %
:- pred divide_by_set(set_ordlist(T)::in, set_ordlist(T)::in,
    set_ordlist(T)::out, set_ordlist(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % `list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'.
    %
:- func list_to_set(list(T)) = set_ordlist(T).
:- pred list_to_set(list(T)::in, set_ordlist(T)::out) is det.

    % A synonym for list_to_set/1.
    %
:- func from_list(list(T)) = set_ordlist(T).

    % `sorted_list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'. `List' must be sorted
    % in ascending order.
    % NOTE_TO_IMPLEMENTORS We do *not* say "must not contain any duplicates",
    % NOTE_TO_IMPLEMENTORS since the implementation does remove duplicates.
    %
:- func sorted_list_to_set(list(T)) = set_ordlist(T).
:- pred sorted_list_to_set(list(T)::in, set_ordlist(T)::out) is det.

    % A synonym for sorted_list_to_set/1.
    %
:- func from_sorted_list(list(T)) = set_ordlist(T).

    % `rev_sorted_list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'. `List' must be sorted
    % in descending order and must not contain any duplicates.
    % NOTE_TO_IMPLEMENTORS Unlike sorted_list_to_set, this predicate has
    % NOTE_TO_IMPLEMENTORS never existed without requiring "no duplicates".
    %
:- func rev_sorted_list_to_set(list(T)) = set_ordlist(T).
:- pred rev_sorted_list_to_set(list(T)::in, set_ordlist(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % `to_sorted_list(Set, List)' is true iff `List' is the list of all the
    % members of `Set', in sorted order.
    %
:- func to_sorted_list(set_ordlist(T)) = list(T).
:- pred to_sorted_list(set_ordlist(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Counting.
%

    % `count(Set, Count)' is true iff `Set' has `Count' elements.
    %
:- func count(set_ordlist(T)) = int.
:- pred count(set_ordlist(T)::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds for all the
    % elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), set_ordlist(T)::in)
    is semidet.

    % Return the set of items for which the given predicate succeeds.
    %
:- func filter(pred(T1), set_ordlist(T1)) = set_ordlist(T1).
:- mode filter(pred(in) is semidet, in) = out is det.
:- pred filter(pred(T1), set_ordlist(T1), set_ordlist(T1)).
:- mode filter(pred(in) is semidet, in, out) is det.

    % Return the set of items for which the given predicate succeeds, and the
    % set of items for which it fails.
    %
:- pred filter(pred(T1), set_ordlist(T1), set_ordlist(T1), set_ordlist(T1)).
:- mode filter(pred(in) is semidet, in, out, out) is det.

:- func filter_map(func(T1) = T2, set_ordlist(T1)) = set_ordlist(T2).
:- mode filter_map(func(in) = out is semidet, in) = out is det.
:- pred filter_map(pred(T1, T2), set_ordlist(T1), set_ordlist(T2)).
:- mode filter_map(pred(in, out) is semidet, in, out) is det.

:- func map(func(T1) = T2, set_ordlist(T1)) = set_ordlist(T2).

:- func fold(func(T1, T2) = T2, set_ordlist(T1), T2) = T2.
:- pred fold(pred(T1, T2, T2), set_ordlist(T1), T2, T2).
:- mode fold(pred(in, in, out) is det, in, in, out) is det.
:- mode fold(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- func foldl(func(T1, T2) = T2, set_ordlist(T1), T2) = T2.
:- pred foldl(pred(T1, T2, T2), set_ordlist(T1), T2, T2).
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- pred fold2(pred(T1, T2, T2, T3, T3), set_ordlist(T1),
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

:- pred foldl2(pred(T1, T2, T2, T3, T3), set_ordlist(T1),
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

:- pred fold3(pred(T1, T2, T2, T3, T3, T4, T4),
    set_ordlist(T1), T2, T2, T3, T3, T4, T4).
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

:- pred foldl3(pred(T1, T2, T2, T3, T3, T4, T4),
    set_ordlist(T1), T2, T2, T3, T3, T4, T4).
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

:- pred fold4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5),
    set_ordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5).
:- mode fold4(
    pred(in, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out) is det.
:- mode fold4(
    pred(in, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode fold4(
    pred(in, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, di, uo) is det.
:- mode fold4(
    pred(in, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out) is semidet.
:- mode fold4(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold4(
    pred(in, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5),
    set_ordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5).
:- mode foldl4(
    pred(in, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out) is det.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(
    pred(in, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, di, uo) is semidet.

:- pred fold5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_ordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_ordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred fold6(pred(T, A, A, B, B, C, C, D, D, E, E, F, F),
    set_ordlist(T), A, A, B, B, C, C, D, D, E, E, F, F).
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

:- pred foldl6(pred(T, A, A, B, B, C, C, D, D, E, E, F, F),
    set_ordlist(T), A, A, B, B, C, C, D, D, E, E, F, F).
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

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.  % for var/1.

:- pragma type_spec(pred(list_to_set/2),    T = var(_)).
:- pragma type_spec(member(in, in),         T = var(_)).
:- pragma type_spec(contains(in, in),       T = var(_)).
:- pragma type_spec(pred(insert/3),         T = var(_)).
:- pragma type_spec(pred(insert_list/3),    T = var(_)).
:- pragma type_spec(pred(delete/3),         T = var(_)).
:- pragma type_spec(pred(delete_list/3),    T = var(_)).
:- pragma type_spec(pred(remove/3),         T = var(_)).
:- pragma type_spec(pred(remove_list/3),    T = var(_)).
:- pragma type_spec(pred(union/3),          T = var(_)).
:- pragma type_spec(pred(union_list/2),     T = var(_)).
:- pragma type_spec(pred(intersect/3),      T = var(_)).
:- pragma type_spec(pred(intersect_list/2), T = var(_)).
:- pragma type_spec(pred(difference/3),     T = var(_)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module require.

%---------------------------------------------------------------------------%

    % We use a d.u. type to work around spurious type ambiguity errors
    % that could arise if the type equivalence `set_ordlist(T) == list(T)'
    % is exposed for intermodule optimisation.
    %
    % XXX Intermodule optimization *should not* affect the set of modules
    % that unqualified predicate names are resolved against.
    %
:- type set_ordlist(T)
    --->    sol(list(T)).

%---------------------------------------------------------------------------%

init = S :-
    init(S).

init(sol([])).

singleton_set(X, sol([X])).

make_singleton_set(T) = S :-
    singleton_set(T, S).

%---------------------------------------------------------------------------%

empty(sol([])).
is_empty(sol([])).

non_empty(sol([_ | _])).
is_non_empty(sol([_ | _])).

is_singleton(sol([X]), X).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(E::out, sol(S)::in) :-
    list.member(E, S).
member(E::in, S::in) :-
    is_member(E, S, yes).

is_member(E, sol(L), R) :-
    is_member_loop(E, L, R).

:- pred is_member_loop(T::in, list(T)::in, bool::out) is det.

is_member_loop(_E, [], no).
is_member_loop(E, [H | T], R) :-
    compare(Res, H, E),
    (
        Res = (<),
        is_member_loop(E, T, R)
    ;
        Res = (=),
        R = yes
    ;
        Res = (>),
        R = no
    ).

contains(S, E) :-
    member(E, S).

%---------------------------------------------------------------------------%

insert(!.S, T) = !:S :-
    insert(T, !S).

insert(NewItem, sol(List0), sol(List)) :-
    insert_loop(List0, NewItem, List).

:- pred insert_loop(list(T)::in, T::in, list(T)::out) is det.

insert_loop([], NewItem, [NewItem]).
insert_loop([Head | Tail], NewItem, UpdatedList) :-
    compare(R, Head, NewItem),
    (
        R = (<),
        insert_loop(Tail, NewItem, UpdatedTail),
        UpdatedList = [Head | UpdatedTail]
    ;
        R = (=),
        UpdatedList = [Head | Tail]
    ;
        R = (>),
        UpdatedList = [NewItem, Head | Tail]
    ).

insert_new(NewItem, sol(List0), sol(List)) :-
    insert_new_loop(List0, NewItem, List).

:- pred insert_new_loop(list(T)::in, T::in, list(T)::out) is semidet.

insert_new_loop([], NewItem, [NewItem]).
insert_new_loop([Head | Tail], NewItem, UpdatedList) :-
    compare(R, Head, NewItem),
    (
        R = (<),
        insert_new_loop(Tail, NewItem, UpdatedTail),
        UpdatedList = [Head | UpdatedTail]
    ;
        R = (=),
        fail
    ;
        R = (>),
        UpdatedList = [NewItem, Head | Tail]
    ).

insert_list(!.S, Xs) = !:S :-
    insert_list(Xs, !S).

insert_list(List0, !Set) :-
    list.sort_and_remove_dups(List0, List),
    union(sol(List), !Set).

%---------------------%

delete(!.S, T) = !:S :-
    delete(T, !S).

delete(Elem, !Set) :-
    difference(!.Set, sol([Elem]), !:Set).

delete_list(!.S, Xs) = !:S :-
    delete_list(Xs, !S).

delete_list(D, !Set) :-
    list.sort_and_remove_dups(D, DS),
    difference(!.Set, sol(DS), !:Set).

    % sort_no_dups(List, Set) is true iff
    % List is a list with the same elements as Set and
    % List contains no duplicates.
    %
:- pred sort_no_dups(list(T)::in, set_ordlist(T)::out) is semidet.

sort_no_dups(List, sol(Set)) :-
    list.sort(List, Set),
    (
        Set = []
    ;
        Set = [Elem | Elems],
        no_dups(Elem, Elems)
    ).

    % no_dups(Elem, Set) is true iff Set does not contain Elem,
    % and Set does not contains duplicates.
    %
:- pred no_dups(T::in, list(T)::in) is semidet.

no_dups(_, []).
no_dups(Elem, [Elem0 | Elems]) :-
    Elem \= Elem0,
    no_dups(Elem0, Elems).

remove(Elem, sol(Set0), sol(Set)) :-
    list.delete_first(Set0, Elem, Set).

det_remove(X, !Set) :-
    ( if set_ordlist.remove(X, !Set) then
        true
    else
        unexpected($pred, "remove failed")
    ).

remove_list(Elems, !Set) :-
    sort_no_dups(Elems, ElemSet),
    subset(ElemSet, !.Set),
    difference(!.Set, ElemSet, !:Set).

det_remove_list(List, !Set) :-
    ( if set_ordlist.remove_list(List, !Set) then
        true
    else
        unexpected($pred, "remove_list failed")
    ).

remove_least(Elem, sol([Elem | Set]), sol(Set)).

%---------------------------------------------------------------------------%

equal(Set, Set).

subset(Subset, Set) :-
    intersect(Set, Subset, Subset).

superset(Superset, Set) :-
    subset(Set, Superset).

%---------------------------------------------------------------------------%

union(S1, S2) = S3 :-
    union(S1, S2, S3).

union(sol(Set0), sol(Set1), sol(Set)) :-
    list.merge_and_remove_dups(Set0, Set1, Set).

union_list(ListofSets) = Set :-
    init(Set0),
    union_list_loop(ListofSets, Set0, Set).

union_list(ListofSets, Set) :-
    Set = union_list(ListofSets).

power_union(SS) = S :-
    power_union(SS, S).

power_union(sol(ListofSets), Set) :-
    Set = union_list(ListofSets).

:- pred union_list_loop(list(set_ordlist(T))::in,
    set_ordlist(T)::in, set_ordlist(T)::out) is det.

union_list_loop([], !UnionSet).
union_list_loop([Set | Sets], !UnionSet) :-
    union(Set, !UnionSet),
    union_list_loop(Sets, !UnionSet).

%---------------------%

intersect(Xs, Ys) = Intersection :-
    intersect(Xs, Ys, Intersection).

intersect(sol(Xs), sol(Ys), sol(Intersection)) :-
    intersect_loop(Xs, Ys, Intersection).

:- pred intersect_loop(list(T), list(T), list(T)).
:- mode intersect_loop(in, in, out) is det.
:- mode intersect_loop(in, in, in) is semidet.

intersect_loop([], _, []).
intersect_loop([_ | _], [], []).
intersect_loop([X | Xs], [Y | Ys], Intersection) :-
    compare(R, X, Y),
    (
        R = (<),
        intersect_loop(Xs, [Y | Ys], Intersection)
    ;
        R = (=),
        intersect_loop(Xs, Ys, Intersection0),
        Intersection = [X | Intersection0]
    ;
        R = (>),
        intersect_loop([X | Xs], Ys, Intersection)
    ).

intersect_list([]) = sol([]).
intersect_list([S0 | Ss]) = S :-
    (
        Ss = [],
        S = S0
    ;
        Ss = [_ | _],
        S1 = intersect_list(Ss),
        intersect(S1, S0, S)
    ).

intersect_list(ListofSets, Set) :-
    Set = intersect_list(ListofSets).

power_intersect(SS) = S :-
    power_intersect(SS, S).

power_intersect(sol(S0), S) :-
    intersect_list(S0) = S.

%---------------------%

difference(Xs, Ys) = Diff :-
    difference(Xs, Ys, Diff).

difference(sol(Xs), sol(Ys), sol(Diff)) :-
    difference_loop(Xs, Ys, Diff).

:- pred difference_loop(list(T)::in, list(T)::in, list(T)::out) is det.

difference_loop([], _, []).
difference_loop([X | Xs], [], [X | Xs]).
difference_loop([X | Xs], [Y | Ys], Diff) :-
    compare(R, X, Y),
    (
        R = (<),
        difference_loop(Xs, [Y | Ys], Diff0),
        Diff = [X | Diff0]
    ;
        R = (=),
        difference_loop(Xs, Ys, Diff)
    ;
        R = (>),
        difference_loop([X | Xs], Ys, Diff)
    ).

%---------------------%

intersection_and_differences(SetA, SetB, InAandB, OnlyInA, OnlyInB) :-
    SetA = sol(ListA),
    SetB = sol(ListB),
    intersection_and_differences_loop(ListA, ListB,
        cord.init, CordInAandB,
        cord.init, CordOnlyInA,
        cord.init, CordOnlyInB),
    InAandB = sol(cord.list(CordInAandB)),
    OnlyInA = sol(cord.list(CordOnlyInA)),
    OnlyInB = sol(cord.list(CordOnlyInB)).

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

divide(Pred, sol(Set), sol(TruePart), sol(FalsePart)) :-
    % The calls to reverse allow us to make divide_2 tail recursive.
    % This costs us a higher constant factor, but allows divide to work
    % in constant stack space.
    divide_loop(Pred, Set, [], RevTruePart, [], RevFalsePart),
    list.reverse(RevTruePart, TruePart),
    list.reverse(RevFalsePart, FalsePart).

:- pred divide_loop(pred(T)::in(pred(in) is semidet), list(T)::in,
    list(T)::in, list(T)::out, list(T)::in, list(T)::out) is det.

divide_loop(_Pred, [], RevTrue, RevTrue, RevFalse, RevFalse).
divide_loop(Pred, [H | T], RevTrue0, RevTrue, RevFalse0, RevFalse) :-
    ( if Pred(H) then
        RevTrue1 = [H | RevTrue0],
        RevFalse1 = RevFalse0
    else
        RevTrue1 = RevTrue0,
        RevFalse1 = [H | RevFalse0]
    ),
    divide_loop(Pred, T, RevTrue1, RevTrue, RevFalse1, RevFalse).

divide_by_set(sol(DivideBySet), sol(Set), sol(TruePart), sol(FalsePart)) :-
    divide_by_set_loop(DivideBySet, Set,
        [], RevTruePart, [], RevFalsePart),
    list.reverse(RevTruePart, TruePart),
    list.reverse(RevFalsePart, FalsePart).

:- pred divide_by_set_loop(list(T1)::in, list(T1)::in,
    list(T1)::in, list(T1)::out,
    list(T1)::in, list(T1)::out) is det.

divide_by_set_loop([], [], !RevTrue, !RevFalse).
divide_by_set_loop([], [H | T], !RevTrue, !RevFalse) :-
    list.append(list.reverse([H | T]), !RevFalse).
divide_by_set_loop([_ | _], [], !RevTrue, !RevFalse).
divide_by_set_loop([Div | Divs], [H | T], !RevTrue, !RevFalse) :-
    compare(R, Div, H),
    (
        R = (=),
        !:RevTrue = [H | !.RevTrue],
        divide_by_set_loop(Divs, T, !RevTrue, !RevFalse)
    ;
        R = (<),
        divide_by_set_loop(Divs, [H | T], !RevTrue, !RevFalse)
    ;
        R = (>),
        !:RevFalse = [H | !.RevFalse],
        divide_by_set_loop([Div | Divs], T, !RevTrue, !RevFalse)
    ).

%---------------------------------------------------------------------------%

list_to_set(List) = Set :-
    list_to_set(List, Set).

list_to_set(List, sol(SortedList)) :-
    list.sort_and_remove_dups(List, SortedList).

from_list(List) = Set :-
    list_to_set(List, Set).

sorted_list_to_set(List) = Set :-
    sorted_list_to_set(List, Set).

sorted_list_to_set(SortedList0, sol(SortedList)) :-
    list.remove_adjacent_dups(SortedList0, SortedList).

from_sorted_list(List) = Set :-
    sorted_list_to_set(List, Set).

rev_sorted_list_to_set(RevSortedList) = Set :-
    rev_sorted_list_to_set(RevSortedList, Set).

rev_sorted_list_to_set(RevSortedList, sol(SortedList)) :-
    list.reverse(RevSortedList, SortedList).

%---------------------------------------------------------------------------%

to_sorted_list(S) = Xs :-
    to_sorted_list(S, Xs).

to_sorted_list(sol(List), List).

%---------------------------------------------------------------------------%

count(S) = N :-
    count(S, N).

count(sol(Set), Count) :-
    list.length(Set, Count).

%---------------------------------------------------------------------------%

all_true(P, sol(L)) :-
    list.all_true(P, L).

%---------------------%

filter(P, Set) = TrueSet :-
    List = to_sorted_list(Set),
    list.filter(P, List, TrueList),
    sorted_list_to_set(TrueList, TrueSet).

filter(P, Set, TrueSet) :-
    TrueSet = filter(P, Set).

filter(P, Set, TrueSet, FalseSet) :-
    List = to_sorted_list(Set),
    list.filter(P, List, TrueList, FalseList),
    sorted_list_to_set(TrueList, TrueSet),
    sorted_list_to_set(FalseList, FalseSet).

%---------------------%

filter_map(PF, Set) = TransformedTrueSet :-
    to_sorted_list(Set, List),
    TransformedTrueList = list.filter_map(PF, List),
    list_to_set(TransformedTrueList, TransformedTrueSet).

filter_map(PF, Set, TransformedTrueSet) :-
    to_sorted_list(Set, List),
    list.filter_map(PF, List, TransformedTrueList),
    list_to_set(TransformedTrueList, TransformedTrueSet).

map(F, Set) = TransformedSet :-
    List = to_sorted_list(Set),
    TransformedList = list.map(F, List),
    list_to_set(TransformedList, TransformedSet).

%---------------------%

fold(F, S, A) =
    foldl(F, S, A).

fold(P, S, !A) :-
    foldl(P, S, !A).

foldl(F, S, A) = B :-
    B = list.foldl(F, to_sorted_list(S), A).

foldl(P, S, !A) :-
    list.foldl(P, to_sorted_list(S), !A).

fold2(P, S, !A, !B) :-
    foldl2(P, S, !A, !B).

foldl2(P, S, !A, !B) :-
    list.foldl2(P, to_sorted_list(S), !A, !B).

fold3(P, S, !A, !B, !C) :-
    foldl3(P, S, !A, !B, !C).

foldl3(P, S, !A, !B, !C) :-
    list.foldl3(P, to_sorted_list(S), !A, !B, !C).

fold4(P, S, !A, !B, !C, !D) :-
    foldl4(P, S, !A, !B, !C, !D).

foldl4(P, S, !A, !B, !C, !D) :-
    list.foldl4(P, to_sorted_list(S), !A, !B, !C, !D).

fold5(P, S, !A, !B, !C, !D, !E) :-
    foldl5(P, S, !A, !B, !C, !D, !E).

foldl5(P, S, !A, !B, !C, !D, !E) :-
    list.foldl5(P, to_sorted_list(S), !A, !B, !C, !D, !E).

fold6(P, S, !A, !B, !C, !D, !E, !F) :-
    foldl6(P, S, !A, !B, !C, !D, !E, !F).

foldl6(P, S, !A, !B, !C, !D, !E, !F) :-
    list.foldl6(P, to_sorted_list(S), !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%
:- end_module set_ordlist.
%---------------------------------------------------------------------------%
