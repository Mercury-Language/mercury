%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2019, 2021-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: set_ctree234.m.
% Author: zs.
% Stability: high.
%
% This module implements sets using 2-3-4 trees extended with element counts.
% This representation has higher constant factors for most operations than
% ordered lists, but it has much better worst-case complexity, and is likely
% to be faster for large sets. Specifically,
%
% - the cost of lookups is only logarithmic in the size of the set, not linear;
%
% - for operations that are intrinsically linear in the size of one input
%   operand or the other, the counts allow us to choose to be linear in the
%   size of the smaller set.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module set_ctree234.
:- interface.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

:- type set_ctree234(_T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % init = Set is true iff Set is an empty set.
    %
:- func init = set_ctree234(T).

    % singleton_set(Elem, Set) is true iff Set is the set containing just
    % the single element Elem.
    %
:- pred singleton_set(T, set_ctree234(T)).
:- mode singleton_set(in, out) is det.
:- mode singleton_set(out, in) is semidet.

:- func make_singleton_set(T) = set_ctree234(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

    % is_empty(Set) is true iff Set is an empty set.
    %
:- pred is_empty(set_ctree234(_T)::in) is semidet.

    % is_non_empty(Set) is true iff Set is not an empty set.
    %
:- pred is_non_empty(set_ctree234(T)::in) is semidet.

:- pred is_singleton(set_ctree234(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % member(X, Set) is true iff X is a member of Set.
    %
:- pred member(T, set_ctree234(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % one_member(Set, X) is true iff X is a member of Set.
    %
:- pred one_member(set_ctree234(T)::in, T::out) is nondet.

    % is_member(Set, X, Result) returns `Result = yes' iff
    % X is a member of Set.
    %
:- func is_member(set_ctree234(T), T) = bool.
:- pred is_member(set_ctree234(T)::in, T::in, bool::out) is det.

    % contains(Set, X) is true iff X is a member of Set.
    %
:- pred contains(set_ctree234(T)::in, T::in) is semidet.

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % insert(X, Set0, Set) is true iff Set is the union of Set0
    % and the set containing only X.
    %
:- func insert(T, set_ctree234(T)) = set_ctree234(T).
:- pred insert(T::in, set_ctree234(T)::in, set_ctree234(T)::out) is det.

    % insert_new(X, Set0, Set) is true iff Set0 does not contain X,
    % and Set is the union of Set0 and the set containing only X.
    %
:- pred insert_new(T::in, set_ctree234(T)::in, set_ctree234(T)::out)
    is semidet.

    % insert_list(Xs, Set0, Set) is true iff Set is the union of Set0
    % and the set containing only the members of Xs.
    %
:- func insert_list(list(T), set_ctree234(T)) = set_ctree234(T).
:- pred insert_list(list(T)::in, set_ctree234(T)::in, set_ctree234(T)::out)
    is det.

    % delete(X, Set0, Set) is true iff Set is the
    % relative complement of Set0 and the set containing only X, i.e.
    % if Set is the set which contains all the elements of Set0
    % except X.
    %
:- func delete(T, set_ctree234(T)) = set_ctree234(T).
:- pred delete(T::in, set_ctree234(T)::in, set_ctree234(T)::out) is det.

    % delete_list(Xs, Set0, Set) is true iff Set is the relative complement
    % of Set0 and the set containing only the members of Xs.
    %
:- func delete_list(list(T), set_ctree234(T)) = set_ctree234(T).
:- pred delete_list(list(T)::in, set_ctree234(T)::in, set_ctree234(T)::out)
    is det.

    % remove(X, Set0, Set) is true iff Set0 contains X,
    % and Set is the relative complement of Set0 and the set containing
    % only X, i.e. if Set is the set which contains all the elements
    % of Set0 except X.
    %
    % The det_remove version throws an exception instead of failing.
    %
:- pred remove(T::in, set_ctree234(T)::in, set_ctree234(T)::out) is semidet.
:- pred det_remove(T::in, set_ctree234(T)::in, set_ctree234(T)::out) is det.

    % remove_list(Xs, Set0, Set) is true iff Xs does not contain any
    % duplicates, Set0 contains every member of Xs, and Set is the
    % relative complement of Set0 and the set containing only the members of
    % Xs.
    %
    % The det_remove_list version throws an exception instead of failing.
    %
:- pred remove_list(list(T)::in, set_ctree234(T)::in, set_ctree234(T)::out)
    is semidet.
:- pred det_remove_list(list(T)::in, set_ctree234(T)::in, set_ctree234(T)::out)
    is det.

    % remove_least(X, Set0, Set) is true iff X is the least element in
    % Set0, and Set is the set which contains all the elements of Set0
    % except X.
    %
:- pred remove_least(T::out, set_ctree234(T)::in, set_ctree234(T)::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % equal(SetA, SetB) is true iff SetA and SetB contain
    % the same elements.
    %
:- pred equal(set_ctree234(T)::in, set_ctree234(T)::in) is semidet.

    % subset(SetA, SetB) is true iff SetA is a subset of SetB.
    %
:- pred subset(set_ctree234(T)::in, set_ctree234(T)::in) is semidet.

    % superset(SetA, SetB) is true iff SetA is a superset of SetB.
    %
:- pred superset(set_ctree234(T)::in, set_ctree234(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % union(SetA, SetB) = Set is true iff Set is the union of SetA and
    % SetB.
    %
:- func union(set_ctree234(T), set_ctree234(T)) = set_ctree234(T).
:- pred union(set_ctree234(T)::in, set_ctree234(T)::in, set_ctree234(T)::out)
    is det.

    % union_list(A, B) is true iff B is the union of all the sets in A.
    %
:- func union_list(list(set_ctree234(T))) = set_ctree234(T).
:- pred union_list(list(set_ctree234(T))::in, set_ctree234(T)::out) is det.

    % power_union(A) = B is true iff B is the union of all the sets in A.
    %
:- func power_union(set_ctree234(set_ctree234(T))) = set_ctree234(T).
:- pred power_union(set_ctree234(set_ctree234(T))::in, set_ctree234(T)::out)
    is det.

    % intersect(SetA, SetB) = Set is true iff Set is the intersection of
    % SetA and SetB.
    %
:- func intersect(set_ctree234(T), set_ctree234(T)) = set_ctree234(T).
:- pred intersect(set_ctree234(T)::in, set_ctree234(T)::in,
    set_ctree234(T)::out) is det.

    % intersect_list(A) = B is true iff B is the intersection
    % of all the sets in A.
    %
:- func intersect_list(list(set_ctree234(T))) = set_ctree234(T).

    % power_intersect(A, B) is true iff B is the intersection
    % of all the sets in A.
    %
:- func power_intersect(set_ctree234(set_ctree234(T))) = set_ctree234(T).

    % difference(SetA, SetB, Set) is true iff Set is the set containing
    % all the elements of SetA except those that occur in SetB.
    %
:- func difference(set_ctree234(T), set_ctree234(T)) = set_ctree234(T).
:- pred difference(set_ctree234(T)::in, set_ctree234(T)::in,
    set_ctree234(T)::out) is det.

    % intersection_and_differences(SetA, SetB, InAandB, OnlyInA, OnlyInB):
    % Given SetA and SetB, return the elements that occur in both sets,
    % and those that occur only in one or the other.
    %
:- pred intersection_and_differences(set_ctree234(T)::in, set_ctree234(T)::in,
    set_ctree234(T)::out, set_ctree234(T)::out, set_ctree234(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    % NOTE: This is the same as filter/4.
    %
:- pred divide(pred(T)::in(pred(in) is semidet),
    set_ctree234(T)::in, set_ctree234(T)::out, set_ctree234(T)::out) is det.

    % divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in
    % DivideBySet; OutPart consists of those elements of which are
    % not in DivideBySet.
    %
:- pred divide_by_set(set_ctree234(T)::in, set_ctree234(T)::in,
    set_ctree234(T)::out, set_ctree234(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % list_to_set(List) = Set is true iff Set is the set
    % containing only the members of List.
    %
    % `from_list' is a synonym for `list_to_set'.
    %
:- func list_to_set(list(T)) = set_ctree234(T).
:- func from_list(list(T)) = set_ctree234(T).

    % sorted_list_to_set(List) = Set is true iff Set is the set
    % containing only the members of List. List must be sorted
    % in ascending order and must not contain any duplicates.
    %
:- func sorted_list_to_set(list(T)) = set_ctree234(T).

    % rev_sorted_list_to_set(List) = Set is true iff Set is the set
    % containing only the members of List. List must be sorted
    % in descending order and must not contain any duplicates.
    %
:- func rev_sorted_list_to_set(list(T)) = set_ctree234(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % to_sorted_list(Set) = List is true iff List is the list
    % of all the members of Set, in sorted order.
    %
:- func to_sorted_list(set_ctree234(T)) = list(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

    % count(Set, Count) is true iff Set has Count elements.
    %
:- func count(set_ctree234(T)) = int.

:- pred verify_depths(set_ctree234(T)::in, list(int)::out) is det.

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds for all the
    % elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet),
    set_ctree234(T)::in) is semidet.

    % Return the set of items for which the predicate succeeds.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_ctree234(T)::in, set_ctree234(T)::out) is det.

    % Return the set of items for which the predicate succeeds,
    % and the set for which it fails.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_ctree234(T)::in, set_ctree234(T)::out, set_ctree234(T)::out) is det.

:- func filter_map(func(T1) = T2, set_ctree234(T1)) = set_ctree234(T2).
:- mode filter_map(in(func(in) = out is semidet), in) = out is det.

:- pred filter_map(pred(T1, T2)::in(pred(in, out) is semidet),
    set_ctree234(T1)::in, set_ctree234(T2)::out) is det.

:- func map(func(T1) = T2, set_ctree234(T1)) = set_ctree234(T2).
:- pred map(pred(T1, T2)::in(pred(in, out) is det),
    set_ctree234(T1)::in, set_ctree234(T2)::out) is det.

:- func fold(func(T1, T2) = T2, set_ctree234(T1), T2) = T2.
:- pred fold(pred(T1, T2, T2), set_ctree234(T1), T2, T2).
:- mode fold(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode fold(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode fold(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode fold(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

:- pred fold2(pred(T1, T2, T2, T3, T3), set_ctree234(T1),
    T2, T2, T3, T3).
:- mode fold2(in(pred(in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode fold2(in(pred(in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode fold2(in(pred(in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode fold2(in(pred(in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode fold2(in(pred(in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode fold2(in(pred(in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

:- pred fold3(
    pred(T1, T2, T2, T3, T3, T4, T4), set_ctree234(T1),
    T2, T2, T3, T3, T4, T4).
:- mode fold3(in(pred(in, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out) is det.
:- mode fold3(in(pred(in, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, mdi, muo) is det.
:- mode fold3(in(pred(in, in, out, in, out, di, uo) is det), in,
    in, out, in, out, di, uo) is det.
:- mode fold3(in(pred(in, in, out, in, out, in, out) is semidet), in,
    in, out, in, out, in, out) is semidet.
:- mode fold3(in(pred(in, in, out, in, out, mdi, muo) is semidet), in,
    in, out, in, out, mdi, muo) is semidet.
:- mode fold3(in(pred(in, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, di, uo) is semidet.

:- pred fold4(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), set_ctree234(T1),
    T2, T2, T3, T3, T4, T4, T5, T5).
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out) is det.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, di, uo) is det.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred fold5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_ctree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred fold6(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_ctree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
:- mode fold6(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold6(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold6(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold6(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, in, out)
        is semidet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold6(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo)
        is semidet),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold6(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, di, uo)
        is semidet),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.  % for var/1.

:- pragma type_spec(pred(contains/2),           T = var(_)).
:- pragma type_spec(pred(do_is_member/3),       T = var(_)).
:- pragma type_spec(pred(do_list_to_set/5),     T = var(_)).
:- pragma type_spec(pred(do_to_sorted_list/3),  T = var(_)).
:- pragma type_spec(pred(subset/2),             T = var(_)).
:- pragma type_spec(pred(do_subset/2),          T = var(_)).
:- pragma type_spec(pred(insert/3),             T = var(_)).
:- pragma type_spec(pred(do_insert/4),          T = var(_)).
:- pragma type_spec(pred(insert2/4),            T = var(_)).
:- pragma type_spec(pred(insert3/4),            T = var(_)).
:- pragma type_spec(pred(insert_list/3),        T = var(_)).
:- pragma type_spec(pred(do_insert_list/5),     T = var(_)).
:- pragma type_spec(pred(delete/3),             T = var(_)).
:- pragma type_spec(pred(do_delete/5),          T = var(_)).
:- pragma type_spec(pred(remove/3),             T = var(_)).
:- pragma type_spec(pred(do_remove/4),          T = var(_)).
:- pragma type_spec(pred(remove_least/3),       T = var(_)).
:- pragma type_spec(pred(do_remove_least/4),    T = var(_)).
:- pragma type_spec(func(union/2),              T = var(_)).
:- pragma type_spec(pred(union/3),              T = var(_)).
:- pragma type_spec(pred(do_union/5),           T = var(_)).
:- pragma type_spec(pred(union_list/2),         T = var(_)).
:- pragma type_spec(pred(do_union_list/3),      T = var(_)).
:- pragma type_spec(pred(power_union/2),        T = var(_)).
:- pragma type_spec(pred(do_power_union/5),     T = var(_)).
:- pragma type_spec(func(intersect/2),          T = var(_)).
:- pragma type_spec(pred(intersect/3),          T = var(_)).
:- pragma type_spec(pred(do_intersect/6),       T = var(_)).
:- pragma type_spec(func(difference/2),         T = var(_)).
:- pragma type_spec(pred(difference/3),         T = var(_)).
:- pragma type_spec(pred(do_difference/5),      T = var(_)).
:- pragma type_spec(pred(divide/4),             T = var(_)).
:- pragma type_spec(pred(do_divide/6),          T = var(_)).
:- pragma type_spec(pred(divide_by_set/4),      T = var(_)).

:- type set_ctree234(T)
    --->    ct(int, set_tree234(T)).

% The type set_ctree234.set_tree234 defined here is a copy of the type
% set_tree234.set_tree234. The code that does lookups on set_ctree234s
% duplicates similar code in set_tree234.m, while code that outputs
% set_ctree234s *almost* duplicates similar code in set_tree234.m,
% but with the variations necessary to compute or update the count.
%
% NOTE It would be nice if a set_ctree234 were a count wrapped around
% a set_tree234.set_tree234, but we cannot do that while
% set_tree234.set_tree234 is an abstract exported type, since the predicates
% that output set_ctree234s need access to the internals of the tree.
% For now, we have chosen code duplication as a lesser evil than
% to exporting the internals of set_tree234s. The lack of module qualification
% on code that works with these trees makes duplicating code between the
% modules a bit easier.

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

:- mode out_two  == out(two(ground, ground)).
:- mode in_two   == in(two(ground, ground)).
:- mode in_three == in(three(ground, ground)).
:- mode in_four  == in(four(ground, ground)).

%---------------------------------------------------------------------------%

init = ct(0, empty).

singleton_set(X, ct(1, two(X, empty, empty))).

make_singleton_set(X) = ct(1, two(X, empty, empty)).

%---------------------------------------------------------------------------%

is_empty(ct(0, _)).

is_non_empty(ct(N, _)) :- N \= 0.

is_singleton(ct(1, two(X, empty, empty)), X).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(E::in, Set::in) :-
    contains(Set, E).
member(E::out, Set::in) :-
    one_member(Set, E).

one_member(ct(_, Tin), E) :-
    do_one_member(Tin, E).

:- pred do_one_member(set_tree234(T)::in, T::out) is nondet.

do_one_member(empty, _) :- fail.
do_one_member(two(E0, T0, T1), E) :-
    (
        E = E0
    ;
        do_one_member(T0, E)
    ;
        do_one_member(T1, E)
    ).
do_one_member(three(E0, E1, T0, T1, T2), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        do_one_member(T0, E)
    ;
        do_one_member(T1, E)
    ;
        do_one_member(T2, E)
    ).
do_one_member(four(E0, E1, E2, T0, T1, T2, T3), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        E = E2
    ;
        do_one_member(T0, E)
    ;
        do_one_member(T1, E)
    ;
        do_one_member(T2, E)
    ;
        do_one_member(T3, E)
    ).

is_member(ct(_, T), E) = R :-
    do_is_member(T, E, R).

is_member(ct(_, Tin), E, R) :-
    do_is_member(Tin, E, R).

:- pred do_is_member(set_tree234(T)::in, T::in, bool::out)
    is det.

do_is_member(T, E, R) :-
    (
        T = empty,
        R = no
    ;
        T = two(E0, T0, T1),
        compare(Result, E, E0),
        (
            Result = (<),
            do_is_member(T0, E, R)
        ;
            Result = (=),
            R = yes
        ;
            Result = (>),
            do_is_member(T1, E, R)
        )
    ;
        T = three(E0, E1, T0, T1, T2),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            do_is_member(T0, E, R)
        ;
            Result0 = (=),
            R = yes
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                do_is_member(T1, E, R)
            ;
                Result1 = (=),
                R = yes
            ;
                Result1 = (>),
                do_is_member(T2, E, R)
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
                do_is_member(T0, E, R)
            ;
                Result0 = (=),
                R = yes
            ;
                Result0 = (>),
                do_is_member(T1, E, R)
            )
        ;
            Result1 = (=),
            R = yes
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                do_is_member(T2, E, R)
            ;
                Result2 = (=),
                R = yes
            ;
                Result2 = (>),
                do_is_member(T3, E, R)
            )
        )
    ).

contains(ct(_, T), E) :-
    do_contains(T, E).

:- pred do_contains(set_tree234(T)::in, T::in) is semidet.
:- pragma inline(pred(do_contains/2)).

do_contains(Tree, E) :-
    do_is_member(Tree, E, yes).

%---------------------------------------------------------------------------%

insert(E, Tin) = Tout :-
    insert(E, Tin, Tout).

insert(E, ct(Sizein, Tin), ct(Sizeout, Tout)) :-
    do_insert(E, Incr, Tin, Tout),
    Sizeout = Sizein + Incr.

:- pred do_insert(T::in, int::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

do_insert(E, Incr, Tin, Tout) :-
    (
        Tin = empty,
        Incr = 1,
        Tout = two(E, empty, empty)
    ;
        Tin = two(_, _, _),
        insert2(E, Incr, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _),
        insert3(E, Incr, Tin, Tout)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            insert2(E, Incr, Sub0, NewSub0),
            Tout = two(E1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Incr = 0,
            Tout = Tin
        ;
            Result1 = (>),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            insert2(E, Incr, Sub1, NewSub1),
            Tout = two(E1, Sub0, NewSub1)
        )
    ).

:- pred insert2(T::in, int::out,
    set_tree234(T)::in_two, set_tree234(T)::out) is det.

insert2(E, Incr, Tin, Tout) :-
    Tin = two(E0, T0, T1),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
    then
        compare(Result, E, E0),
        (
            Result = (<),
            Incr = 1,
            Tout = three(E, E0, empty, empty, empty)
        ;
            Result = (=),
            Incr = 0,
            Tout = Tin
        ;
            Result = (>),
            Incr = 1,
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
                    insert2(E, Incr, T00, NewT00),
                    Tout = three(MT0E, E0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    Incr = 0,
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Tout = three(MT0E, E0, T00, T01, T1)
                ;
                    Result1 = (>),
                    insert2(E, Incr, T01, NewT01),
                    Tout = three(MT0E, E0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _),
                insert3(E, Incr, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = two(_, _, _),
                insert2(E, Incr, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = empty,
                Incr = 1,
                NewT0 = two(E, empty, empty),
                Tout = two(E0, NewT0, T1)
            )
        ;
            Result = (=),
            Incr = 0,
            Tout = two(E, T0, T1)
        ;
            Result = (>),
            (
                T1 = four(_, _, _, _, _, _, _),
                split_four(T1, MT1E, T10, T11),
                compare(Result1, E, MT1E),
                (
                    Result1 = (<),
                    insert2(E, Incr, T10, NewT10),
                    Tout = three(E0, MT1E, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Incr = 0,
                    Tout = three(E0, MT1E, T0, T10, T11)
                ;
                    Result1 = (>),
                    insert2(E, Incr, T11, NewT11),
                    Tout = three(E0, MT1E, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _),
                insert3(E, Incr, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = two(_, _, _),
                insert2(E, Incr, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = empty,
                Incr = 1,
                NewT1 = two(E, empty, empty),
                Tout = two(E0, T0, NewT1)
            )
        )
    ).

:- pred insert3(T::in, int::out,
    set_tree234(T)::in_three, set_tree234(T)::out) is det.

insert3(E, Incr, Tin, Tout) :-
    Tin = three(E0, E1, T0, T1, T2),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    then
        compare(Result0, E, E0),
        (
            Result0 = (<),
            Incr = 1,
            Tout = four(E, E0, E1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            Incr = 0,
            Tout = Tin
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                Incr = 1,
                Tout = four(E0, E, E1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                Incr = 0,
                Tout = Tin
            ;
                Result1 = (>),
                Incr = 1,
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
                    insert2(E, Incr, T00, NewT00),
                    Tout = four(MT0E, E0, E1, NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    % The Tout we are returning does not have the same
                    % shape as Tin, but it contains the same elements.
                    Incr = 0,
                    Tout = four(MT0E, E0, E1, T00, T01, T1, T2)
                ;
                    ResultM = (>),
                    insert2(E, Incr, T01, NewT01),
                    Tout = four(MT0E, E0, E1, T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _),
                insert3(E, Incr, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _),
                insert2(E, Incr, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = empty,
                Incr = 1,
                NewT0 = two(E, empty, empty),
                Tout = three(E0, E1, NewT0, T1, T2)
            )
        ;
            Result0 = (=),
            Incr = 0,
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
                        insert2(E, Incr, T10, NewT10),
                        Tout = four(E0, MT1E, E1, T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        % The Tout we are returning does not have the same
                        % shape as Tin, but it contains the same elements.
                        Incr = 0,
                        Tout = four(E0, MT1E, E1, T0, T10, T11, T2)
                    ;
                        ResultM = (>),
                        insert2(E, Incr, T11, NewT11),
                        Tout = four(E0, MT1E, E1, T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _),
                    insert3(E, Incr, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _),
                    insert2(E, Incr, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = empty,
                    Incr = 1,
                    NewT1 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                Incr = 0,
                Tout = Tin
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _),
                    split_four(T2, MT2E, T20, T21),
                    compare(ResultM, E, MT2E),
                    (
                        ResultM = (<),
                        insert2(E, Incr, T20, NewT20),
                        Tout = four(E0, E1, MT2E, T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        % The Tout we are returning does not have the same
                        % shape as Tin, but it contains the same elements.
                        Incr = 0,
                        Tout = four(E0, E1, MT2E, T0, T1, T20, T21)
                    ;
                        ResultM = (>),
                        insert2(E, Incr, T21, NewT21),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _),
                    insert3(E, Incr, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _),
                    insert2(E, Incr, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    Incr = 1,
                    NewT2 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------%

insert_new(E, ct(Sizein, Tin), ct(Sizeout, Tout)) :-
    do_insert_new(E, Tin, Tout),
    Sizeout = Sizein + 1.

:- pred do_insert_new(T::in,
    set_tree234(T)::in, set_tree234(T)::out) is semidet.

do_insert_new(E, Tin, Tout) :-
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

:- pred split_four(set_tree234(T)::in_four, T::out,
    set_tree234(T)::out_two, set_tree234(T)::out_two) is det.
:- pragma inline(pred(split_four/4)).

split_four(Tin, MidE, Sub0, Sub1) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    Sub0 = two(E0, T0, T1),
    MidE = E1,
    Sub1 = two(E2, T2, T3).

%---------------------%

insert_list(Es, Set0) = Set :-
    insert_list(Es, Set0, Set).

insert_list(Es, ct(Size0, Tree0), ct(Size, Tree)) :-
    do_insert_list(Es, Size0, Size, Tree0, Tree).

:- pred do_insert_list(list(T)::in, int::in, int::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

do_insert_list([], !Size, !Set).
do_insert_list([E | Es], !Size, !Set) :-
    do_insert(E, Incr, !Set),
    !:Size = !.Size + Incr,
    do_insert_list(Es, !Size, !Set).

%---------------------------------------------------------------------------%

delete(E, Tin) = Tout :-
    delete(E, Tin, Tout).

delete(E, ct(Sizein, Tin), ct(Sizeout, Tout)) :-
    do_delete(E, Decr, Tin, Tout, _),
    Sizeout = Sizein - Decr.

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.
    %
:- pred do_delete(T::in, int::out, set_tree234(T)::in,
    set_tree234(T)::out, bool::out) is det.

do_delete(E, Decr, Tin, Tout, RH) :-
    (
        Tin = empty,
        Decr = 0,
        Tout = empty,
        RH = no
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            do_delete(E, Decr, T0, NewT0, RHT0),
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
            ),
            Decr = 1
        ;
            Result0 = (>),
            do_delete(E, Decr, T1, NewT1, RHT1),
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
            do_delete(E, Decr, T0, NewT0, RHT0),
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
            ),
            Decr = 1
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                do_delete(E, Decr, T1, NewT1, RHT1),
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
                ( if
                    do_remove_least(T2, ST2E, NewT2, RHT2)
                then
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
                ),
                Decr = 1
            ;
                Result1 = (>),
                do_delete(E, Decr, T2, NewT2, RHT2),
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
                do_delete(E, Decr, T0, NewT0, RHT0),
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
                ),
                Decr = 1
            ;
                Result0 = (>),
                do_delete(E, Decr, T1, NewT1, RHT1),
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
            ),
            Decr = 1
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                do_delete(E, Decr, T2, NewT2, RHT2),
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
                ),
                Decr = 1
            ;
                Result2 = (>),
                do_delete(E, Decr, T3, NewT3, RHT3),
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

delete_list(Es, ct(Size0, Tree0), ct(Size, Tree)) :-
    do_delete_list(Es, Size0, Size, Tree0, Tree).

:- pred do_delete_list(list(T)::in, int::in, int::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

do_delete_list([], !Size, !Set).
do_delete_list([E | Es], !Size, !Set) :-
    do_delete(E, Decr, !Set, _),
    !:Size = !.Size - Decr,
    do_delete_list(Es, !Size, !Set).

%---------------------%

remove(E, ct(Sizein, Tin), ct(Sizeout, Tout)) :-
    % We use the same algorithm as delete.
    do_remove(E, Tin, Tout, _),
    Sizeout = Sizein - 1.

:- pred do_remove(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is semidet.

do_remove(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            do_remove(E, T0, NewT0, RHT0),
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
            do_remove(E, T1, NewT1, RHT1),
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
            do_remove(E, T0, NewT0, RHT0),
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
                do_remove(E, T1, NewT1, RHT1),
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
                do_remove(E, T2, NewT2, RHT2),
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
                do_remove(E, T0, NewT0, RHT0),
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
                do_remove(E, T1, NewT1, RHT1),
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
                do_remove(E, T2, NewT2, RHT2),
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
                do_remove(E, T3, NewT3, RHT3),
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
    ( if set_ctree234.remove(X, !Set) then
        true
    else
        unexpected($pred, "remove failed")
    ).

remove_list(Es, ct(Size0, Tree0), ct(Size, Tree)) :-
    do_remove_list(Es, Size0, Size, Tree0, Tree).

:- pred do_remove_list(list(T)::in, int::in, int::out,
    set_tree234(T)::in, set_tree234(T)::out) is semidet.

do_remove_list([], !Size, !Set).
do_remove_list([E | Es], !Size, !Set) :-
    do_remove(E, !Set, _),
    !:Size = !.Size - 1,
    do_remove_list(Es, !Size, !Set).

det_remove_list(List, !Set) :-
    ( if set_ctree234.remove_list(List, !Set) then
        true
    else
        unexpected($pred, "remove_list failed")
    ).

%---------------------%

remove_least(E, ct(Sizein, Tin), ct(Sizeout, Tout)) :-
    % The algorithm we use is similar to delete, except that
    % we always go down the left subtree.
    do_remove_least(Tin, E, Tout, _),
    Sizeout = Sizein - 1.

:- pred do_remove_least(set_tree234(T)::in, T::out,
    set_tree234(T)::out, bool::out) is semidet.

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

:- pred fix_2node_t0(T::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out, bool::out) is det.

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

:- pred fix_2node_t1(T::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out, bool::out) is det.

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

:- pred fix_3node_t0(T::in, T::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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

:- pred fix_3node_t1(T::in, T::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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

:- pred fix_3node_t2(T::in, T::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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

:- pred fix_4node_t0(T::in, T::in, T::in,
    set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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

:- pred fix_4node_t1(T::in, T::in, T::in,
    set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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

:- pred fix_4node_t2(T::in, T::in, T::in,
    set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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

:- pred fix_4node_t3(T::in, T::in, T::in,
    set_tree234(T)::in, set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out, bool::out) is det.

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
    SetA = ct(SizeA, TreeA),
    SetB = ct(SizeB, TreeB),
    SizeA = SizeB,
    do_to_sorted_list(TreeA, [], ListA),
    do_to_sorted_list(TreeB, [], ListB),
    ListA = ListB.

subset(ct(SizeA, TreeA), ct(SizeB, TreeB)) :-
    SizeA =< SizeB,
    do_subset(TreeA, TreeB).

:- pred do_subset(set_tree234(T)::in, set_tree234(T)::in) is semidet.

    % XXX We could take advantage of the sortedness of TreeA to speed up
    % lookups on TreeB, but doing so is difficult because their structures
    % are not isomorphic.
do_subset(empty, _Set).
do_subset(two(E, T0, T1), Set) :-
    do_subset(T0, Set),
    do_is_member(Set, E, yes),
    do_subset(T1, Set).
do_subset(three(E0, E1, T0, T1, T2), Set) :-
    do_subset(T0, Set),
    do_is_member(Set, E0, yes),
    do_subset(T1, Set),
    do_is_member(Set, E1, yes),
    do_subset(T2, Set).
do_subset(four(E0, E1, E2, T0, T1, T2, T3), Set) :-
    do_subset(T0, Set),
    do_is_member(Set, E0, yes),
    do_subset(T1, Set),
    do_is_member(Set, E1, yes),
    do_subset(T2, Set),
    do_is_member(Set, E2, yes),
    do_subset(T3, Set).

superset(SuperSet, Set) :-
    subset(Set, SuperSet).

%---------------------------------------------------------------------------%

union(SetA, SetB) = Set :-
    union(SetA, SetB, Set).

union(ct(SizeA, TreeA), ct(SizeB, TreeB), ct(Size, Tree)) :-
    ( if SizeA < SizeB then
        do_union(TreeA, SizeB, Size, TreeB, Tree)
    else
        do_union(TreeB, SizeA, Size, TreeA, Tree)
    ).

:- pred do_union(set_tree234(T)::in, int::in, int::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

do_union(empty, !Size, !Tree).
do_union(two(E0, T0, T1), !Size, !Tree) :-
    do_union(T0, !Size, !Tree),
    do_insert(E0, Incr0, !Tree),
    !:Size = !.Size + Incr0,
    do_union(T1, !Size, !Tree).
do_union(three(E0, E1, T0, T1, T2), !Size, !Tree) :-
    do_union(T0, !Size, !Tree),
    do_insert(E0, Incr0, !Tree),
    !:Size = !.Size + Incr0,
    do_union(T1, !Size, !Tree),
    do_insert(E1, Incr1, !Tree),
    !:Size = !.Size + Incr1,
    do_union(T2, !Size, !Tree).
do_union(four(E0, E1, E2, T0, T1, T2, T3), !Size, !Tree) :-
    do_union(T0, !Size, !Tree),
    do_insert(E0, Incr0, !Tree),
    !:Size = !.Size + Incr0,
    do_union(T1, !Size, !Tree),
    do_insert(E1, Incr1, !Tree),
    !:Size = !.Size + Incr1,
    do_union(T2, !Size, !Tree),
    do_insert(E2, Incr2, !Tree),
    !:Size = !.Size + Incr2,
    do_union(T3, !Size, !Tree).

union_list(Sets) = Union :-
    union_list(Sets, Union).

union_list(Sets, Union) :-
    list.sort(Sets, SortedSets),
    do_union_list(SortedSets, Size, Tree),
    Union = ct(Size, Tree).

:- pred do_union_list(list(set_ctree234(T))::in,
    int::out, set_tree234(T)::out) is det.

do_union_list([], 0, empty).
do_union_list([ct(_Size0, Tree0) | Sets], Size, Tree) :-
    do_union_list(Sets, Size1, Tree1),
    do_union(Tree0, Size1, Size, Tree1, Tree).

power_union(Sets) = Union :-
    power_union(Sets, Union).

power_union(ct(_, SetTree), Union) :-
    do_power_union(SetTree, 0, Size, empty, Tree),
    Union = ct(Size, Tree).

:- pred do_power_union(set_tree234(set_ctree234(T))::in,
    int::in, int::out, set_tree234(T)::in, set_tree234(T)::out) is det.

do_power_union(empty, !Size, !Tree).
do_power_union(two(E0, T0, T1), !Size, !Tree) :-
    do_power_union(T0, !Size, !Tree),
    E0 = ct(_, ET0),
    do_union(ET0, !Size, !Tree),
    do_power_union(T1, !Size, !Tree).
do_power_union(three(E0, E1, T0, T1, T2), !Size, !Tree) :-
    do_power_union(T0, !Size, !Tree),
    E0 = ct(_, ET0),
    do_union(ET0, !Size, !Tree),
    do_power_union(T1, !Size, !Tree),
    E1 = ct(_, ET1),
    do_union(ET1, !Size, !Tree),
    do_power_union(T2, !Size, !Tree).
do_power_union(four(E0, E1, E2, T0, T1, T2, T3), !Size, !Tree) :-
    do_power_union(T0, !Size, !Tree),
    E0 = ct(_, ET0),
    do_union(ET0, !Size, !Tree),
    do_power_union(T1, !Size, !Tree),
    E1 = ct(_, ET1),
    do_union(ET1, !Size, !Tree),
    do_power_union(T2, !Size, !Tree),
    E2 = ct(_, ET2),
    do_union(ET2, !Size, !Tree),
    do_power_union(T3, !Size, !Tree).

%---------------------%

intersect(SetA, SetB) = Set :-
    intersect(SetA, SetB, Set).

intersect(ct(SizeA, TreeA), ct(SizeB, TreeB), ct(Size, Tree)) :-
    ( if SizeA < SizeB then
        do_intersect(TreeA, TreeB, 0, Size, empty, Tree)
    else
        do_intersect(TreeB, TreeA, 0, Size, empty, Tree)
    ).

:- pred do_intersect(set_tree234(T)::in, set_tree234(T)::in,
    int::in, int::out, set_tree234(T)::in, set_tree234(T)::out) is det.

do_intersect(empty, _SetB, !Size, !Tree).
do_intersect(two(E0, T0, T1), SetB, !Size, !Tree) :-
    do_intersect(T0, SetB, !Size, !Tree),
    ( if do_is_member(SetB, E0, yes) then
        do_insert(E0, _, !Tree),
        !:Size = !.Size + 1
    else
        true
    ),
    do_intersect(T1, SetB, !Size, !Tree).
do_intersect(three(E0, E1, T0, T1, T2), SetB, !Size, !Tree) :-
    do_intersect(T0, SetB, !Size, !Tree),
    ( if do_is_member(SetB, E0, yes) then
        do_insert(E0, _, !Tree),
        !:Size = !.Size + 1
    else
        true
    ),
    do_intersect(T1, SetB, !Size, !Tree),
    ( if do_is_member(SetB, E1, yes) then
        do_insert(E1, _, !Tree),
        !:Size = !.Size + 1
    else
        true
    ),
    do_intersect(T2, SetB, !Size, !Tree).
do_intersect(four(E0, E1, E2, T0, T1, T2, T3), SetB, !Size, !Tree) :-
    do_intersect(T0, SetB, !Size, !Tree),
    ( if do_is_member(SetB, E0, yes) then
        do_insert(E0, _, !Tree),
        !:Size = !.Size + 1
    else
        true
    ),
    do_intersect(T1, SetB, !Size, !Tree),
    ( if do_is_member(SetB, E1, yes) then
        do_insert(E1, _, !Tree),
        !:Size = !.Size + 1
    else
        true
    ),
    do_intersect(T2, SetB, !Size, !Tree),
    ( if do_is_member(SetB, E2, yes) then
        do_insert(E2, _, !Tree),
        !:Size = !.Size + 1
    else
        true
    ),
    do_intersect(T3, SetB, !Size, !Tree).

intersect_list(Sets) = Intersect :-
    list.sort(Sets, SortedSets),
    (
        SortedSets = [],
        Intersect = init
    ;
        SortedSets = [Head | Tail],
        Head = ct(HeadSize, HeadTree),
        do_intersect_list(HeadSize, HeadTree, Tail,
            IntersectSize, IntersectTree),
        Intersect = ct(IntersectSize, IntersectTree)
    ).

:- pred do_intersect_list(int::in, set_tree234(T)::in,
    list(set_ctree234(T))::in, int::out, set_tree234(T)::out) is det.

do_intersect_list(SizeIn, TreeIn, [], SizeIn, TreeIn).
do_intersect_list(SizeIn, TreeIn, [Head | Tail], Size, Tree) :-
    ( if SizeIn = 0 then
        Size = SizeIn,
        Tree = TreeIn
    else
        Head = ct(_HeadSize, HeadTree),
        do_intersect(TreeIn, HeadTree, 0, Size1, empty, Tree1),
        do_intersect_list(Size1, Tree1, Tail, Size, Tree)
    ).

power_intersect(Sets) =
    % XXX We could implement this without converting the tree to a sorted list.
    intersect_list(to_sorted_list(Sets)).

%---------------------%

difference(SetA, SetB) = Diff :-
    difference(SetA, SetB, Diff).

difference(ct(SizeA, TreeA), ct(_SizeB, TreeB),
        ct(Size, Tree)) :-
    do_difference(TreeB, SizeA, Size, TreeA, Tree).

:- pred do_difference(set_tree234(T)::in,
    int::in, int::out, set_tree234(T)::in, set_tree234(T)::out) is det.

do_difference(empty, !Size, !Tree).
do_difference(two(E0, T0, T1), !Size, !Tree) :-
    do_difference(T0, !Size, !Tree),
    do_delete(E0, Decr0, !Tree, _),
    !:Size = !.Size - Decr0,
    do_difference(T1, !Size, !Tree).
do_difference(three(E0, E1, T0, T1, T2), !Size, !Tree) :-
    do_difference(T0, !Size, !Tree),
    do_delete(E0, Decr0, !Tree, _),
    !:Size = !.Size - Decr0,
    do_difference(T1, !Size, !Tree),
    do_delete(E1, Decr1, !Tree, _),
    !:Size = !.Size - Decr1,
    do_difference(T2, !Size, !Tree).
do_difference(four(E0, E1, E2, T0, T1, T2, T3), !Size, !Tree) :-
    do_difference(T0, !Size, !Tree),
    do_delete(E0, Decr0, !Tree, _),
    !:Size = !.Size - Decr0,
    do_difference(T1, !Size, !Tree),
    do_delete(E1, Decr1, !Tree, _),
    !:Size = !.Size - Decr1,
    do_difference(T2, !Size, !Tree),
    do_delete(E2, Decr2, !Tree, _),
    !:Size = !.Size - Decr2,
    do_difference(T3, !Size, !Tree).

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

divide(Pred, ct(_, Tree), TrueSet, FalseSet) :-
    do_divide(Pred, Tree, [], RevTrues, [], RevFalses),
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

list_to_set(List) = ct(Size, Tree) :-
    do_list_to_set(List, 0, Size, empty, Tree).

:- pred do_list_to_set(list(T)::in, int::in, int::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

do_list_to_set([], !Size, !Tree).
do_list_to_set([E | Es], !Size, !Tree) :-
    do_insert(E, Incr, !Tree),
    !:Size = !.Size + Incr,
    do_list_to_set(Es, !Size, !Tree).

from_list(List) = list_to_set(List).

%---------------------%

sorted_list_to_set(List) = ct(Len, Tree) :-
    list.length(List, Len),
    ( if Len = 0 then
        % We can handle the Len = 0 case here just once, or we can handle it
        % lots of times in do_from_sorted_list. The former is more efficient.
        Tree = empty
    else
        find_num_234_levels(Len, Level, AllThrees),
        do_from_sorted_list(Len, List, LeftOver, Level, AllThrees, Tree),
        trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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
            trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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
                trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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
                trace [compiletime(flag("set_ctree234_sanity_checks"))] (
                    expect(unify(Diff, 1), $pred, "Diff != 1")
                ),
                % Len = BaseSubLen * 2 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_sorted_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream, "splitting %d into two: %d, %d\n",
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

rev_sorted_list_to_set(List) = ct(Len, Tree) :-
    list.length(List, Len),
    ( if Len = 0 then
        % We can handle the Len = 0 case here just once, or we can handle it
        % lots of times in do_from_sorted_list. The former is more efficient.
        Tree = empty
    else
        find_num_234_levels(Len, Level, AllThrees),
        do_from_rev_sorted_list(Len, List, LeftOver, Level, AllThrees, Tree),
        trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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
            trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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
                trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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
                trace [compiletime(flag("set_ctree234_sanity_checks"))] (
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

to_sorted_list(ct(_, Tree)) = List :-
    do_to_sorted_list(Tree, [], List).

:- pred do_to_sorted_list(set_tree234(T)::in,
    list(T)::in, list(T)::out) is det.

do_to_sorted_list(empty, L, L).
do_to_sorted_list(two(E0, T0, T1), L0, L) :-
    do_to_sorted_list(T1, L0, L1),
    do_to_sorted_list(T0, [E0 | L1], L).
do_to_sorted_list(three(E0, E1, T0, T1, T2), L0, L) :-
    do_to_sorted_list(T2, L0, L1),
    do_to_sorted_list(T1, [E1 | L1], L2),
    do_to_sorted_list(T0, [E0 | L2], L).
do_to_sorted_list(four(E0, E1, E2, T0, T1, T2, T3), L0, L) :-
    do_to_sorted_list(T3, L0, L1),
    do_to_sorted_list(T2, [E2 | L1], L2),
    do_to_sorted_list(T1, [E1 | L2], L3),
    do_to_sorted_list(T0, [E0 | L3], L).

%---------------------------------------------------------------------------%

count(ct(N, Tree)) = N :-
    trace [compile_time(flag("verify_set_ctree234"))] (
        expect(unify(N, do_count(Tree)), $pred, "mismatch")
    ).

:- func do_count(set_tree234(T)) = int.

do_count(empty) = 0.
do_count(two(_, T0, T1)) = N :-
    N0 = do_count(T0),
    N1 = do_count(T1),
    N = 1 + N0 + N1.
do_count(three(_, _, T0, T1, T2)) = N :-
    N0 = do_count(T0),
    N1 = do_count(T1),
    N2 = do_count(T2),
    N = 2 + N0 + N1 + N2.
do_count(four(_, _, _, T0, T1, T2, T3)) = N :-
    N0 = do_count(T0),
    N1 = do_count(T1),
    N2 = do_count(T2),
    N3 = do_count(T3),
    N = 3 + N0 + N1 + N2 + N3.

verify_depths(ct(_, Tree), Depths) :-
    do_verify_depths(Tree, 0, [], Depths).

:- pred do_verify_depths(set_tree234(T)::in, int::in,
    list(int)::in, list(int)::out) is det.

do_verify_depths(empty, Depth, !Depths) :-
    ( if list.member(Depth, !.Depths) then
        true
    else
        !:Depths = [Depth | !.Depths]
    ).
do_verify_depths(two(_, T0, T1), Depth, !Depths) :-
    do_verify_depths(T0, Depth + 1, !Depths),
    do_verify_depths(T1, Depth + 1, !Depths).
do_verify_depths(three(_, _, T0, T1, T2), Depth, !Depths) :-
    do_verify_depths(T0, Depth + 1, !Depths),
    do_verify_depths(T1, Depth + 1, !Depths),
    do_verify_depths(T2, Depth + 1, !Depths).
do_verify_depths(four(_, _, _, T0, T1, T2, T3), Depth, !Depths) :-
    do_verify_depths(T0, Depth + 1, !Depths),
    do_verify_depths(T1, Depth + 1, !Depths),
    do_verify_depths(T2, Depth + 1, !Depths),
    do_verify_depths(T3, Depth + 1, !Depths).

%---------------------------------------------------------------------------%

all_true(Pred, ct(_, T)) :-
    all_true_tree(Pred, T).

:- pred all_true_tree(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in) is semidet.

all_true_tree(Pred, T) :-
    (
        T = empty
    ;
        T = two(E0, T0, T1),
        all_true_tree(Pred, T0),
        Pred(E0),
        all_true_tree(Pred, T1)
    ;
        T = three(E0, E1, T0, T1, T2),
        all_true_tree(Pred, T0),
        Pred(E0),
        all_true_tree(Pred, T1),
        Pred(E1),
        all_true_tree(Pred, T2)
    ;
        T = four(E0, E1, E2, T0, T1, T2, T3),
        all_true_tree(Pred, T0),
        Pred(E0),
        all_true_tree(Pred, T1),
        Pred(E1),
        all_true_tree(Pred, T2),
        Pred(E2),
        all_true_tree(Pred, T3)
    ).

%---------------------%

filter(Pred, Set, TrueSet) :-
    % XXX This should be more efficient.
    divide(Pred, Set, TrueSet, _FalseSet).

filter(Pred, Set, TrueSet, FalseSet) :-
    divide(Pred, Set, TrueSet, FalseSet).

filter_map(Func, ct(_, TreeA)) = SetB :-
    filter_map_func(Func, TreeA, [], ListB),
    SetB = list_to_set(ListB).

filter_map(Pred, ct(_, TreeA), SetB) :-
    filter_map_pred(Pred, TreeA, [], ListB),
    SetB = list_to_set(ListB).

:- pred filter_map_func(
    (func(T1) = T2)::in((func(in) = out) is semidet),
    set_tree234(T1)::in, list(T2)::in, list(T2)::out) is det.

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

map(Func, ct(_, TreeA)) = SetB :-
    map_func(Func, TreeA, [], ListB),
    SetB = list_to_set(ListB).

:- pred map_func((func(T1) = T2)::in((func(in) = out) is det),
    set_tree234(T1)::in, list(T2)::in, list(T2)::out) is det.

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

map(Pred, ct(_, TreeA), SetB) :-
    map_pred(Pred, TreeA, [], ListB),
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

fold(Pred, ct(_, Tin), A0) = A :-
    do_fold_func(Pred, Tin, A0, A).

:- pred do_fold_func(
    (func(T1, T2) = T2)::in((func(in, in) = out) is det),
    set_tree234(T1)::in, T2::in, T2::out) is det.

do_fold_func(_Func, empty, !A).
do_fold_func(Func, two(E, T0, T1), !A) :-
    do_fold_func(Func, T0, !A),
    !:A = Func(E, !.A),
    do_fold_func(Func, T1, !A).
do_fold_func(Func, three(E0, E1, T0, T1, T2), !A) :-
    do_fold_func(Func, T0, !A),
    !:A = Func(E0, !.A),
    do_fold_func(Func, T1, !A),
    !:A = Func(E1, !.A),
    do_fold_func(Func, T2, !A).
do_fold_func(Func, four(E0, E1, E2, T0, T1, T2, T3), !A) :-
    do_fold_func(Func, T0, !A),
    !:A = Func(E0, !.A),
    do_fold_func(Func, T1, !A),
    !:A = Func(E1, !.A),
    do_fold_func(Func, T2, !A),
    !:A = Func(E2, !.A),
    do_fold_func(Func, T3, !A).

fold(Pred, ct(_, Tin), !A) :-
    do_fold_pred(Pred, Tin, !A).

:- pred do_fold_pred(pred(T1, T2, T2), set_tree234(T1),
    T2, T2).
:- mode do_fold_pred(in(pred(in, in, out) is det), in,
    in, out) is det.
:- mode do_fold_pred(in(pred(in, mdi, muo) is det), in,
    mdi, muo) is det.
:- mode do_fold_pred(in(pred(in, di, uo) is det), in,
    di, uo) is det.
:- mode do_fold_pred(in(pred(in, in, out) is semidet), in,
    in, out) is semidet.
:- mode do_fold_pred(in(pred(in, mdi, muo) is semidet), in,
    mdi, muo) is semidet.
:- mode do_fold_pred(in(pred(in, di, uo) is semidet), in,
    di, uo) is semidet.

do_fold_pred(_Pred, empty, !A).
do_fold_pred(Pred, two(E, T0, T1), !A) :-
    do_fold_pred(Pred, T0, !A),
    Pred(E, !A),
    do_fold_pred(Pred, T1, !A).
do_fold_pred(Pred, three(E0, E1, T0, T1, T2), !A) :-
    do_fold_pred(Pred, T0, !A),
    Pred(E0, !A),
    do_fold_pred(Pred, T1, !A),
    Pred(E1, !A),
    do_fold_pred(Pred, T2, !A).
do_fold_pred(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A) :-
    do_fold_pred(Pred, T0, !A),
    Pred(E0, !A),
    do_fold_pred(Pred, T1, !A),
    Pred(E1, !A),
    do_fold_pred(Pred, T2, !A),
    Pred(E2, !A),
    do_fold_pred(Pred, T3, !A).

fold2(Pred, ct(_, Tin), !A, !B) :-
    do_fold2_pred(Pred, Tin, !A, !B).

:- pred do_fold2_pred(
    pred(T1, T2, T2, T3, T3), set_tree234(T1), T2, T2, T3, T3).
:- mode do_fold2_pred(in(pred(in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode do_fold2_pred(in(pred(in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode do_fold2_pred(in(pred(in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode do_fold2_pred(in(pred(in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode do_fold2_pred(in(pred(in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode do_fold2_pred(in(pred(in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

do_fold2_pred(_Pred, empty, !A, !B).
do_fold2_pred(Pred, two(E, T0, T1), !A, !B) :-
    do_fold2_pred(Pred, T0, !A, !B),
    Pred(E, !A, !B),
    do_fold2_pred(Pred, T1, !A, !B).
do_fold2_pred(Pred, three(E0, E1, T0, T1, T2), !A, !B) :-
    do_fold2_pred(Pred, T0, !A, !B),
    Pred(E0, !A, !B),
    do_fold2_pred(Pred, T1, !A, !B),
    Pred(E1, !A, !B),
    do_fold2_pred(Pred, T2, !A, !B).
do_fold2_pred(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B) :-
    do_fold2_pred(Pred, T0, !A, !B),
    Pred(E0, !A, !B),
    do_fold2_pred(Pred, T1, !A, !B),
    Pred(E1, !A, !B),
    do_fold2_pred(Pred, T2, !A, !B),
    Pred(E2, !A, !B),
    do_fold2_pred(Pred, T3, !A, !B).

fold3(Pred, ct(_, Tin), !A, !B, !C) :-
    do_fold3_pred(Pred, Tin, !A, !B, !C).

:- pred do_fold3_pred(
    pred(T1, T2, T2, T3, T3, T4, T4), set_tree234(T1),
    T2, T2, T3, T3, T4, T4).
:- mode do_fold3_pred(
    in(pred(in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode do_fold3_pred(
    in(pred(in, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, mdi, muo) is det.
:- mode do_fold3_pred(
    in(pred(in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
:- mode do_fold3_pred(
    in(pred(in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode do_fold3_pred(
    in(pred(in, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, mdi, muo) is semidet.
:- mode do_fold3_pred(
    in(pred(in, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, di, uo) is semidet.

do_fold3_pred(_Pred, empty, !A, !B, !C).
do_fold3_pred(Pred, two(E, T0, T1), !A, !B, !C) :-
    do_fold3_pred(Pred, T0, !A, !B, !C),
    Pred(E, !A, !B, !C),
    do_fold3_pred(Pred, T1, !A, !B, !C).
do_fold3_pred(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C) :-
    do_fold3_pred(Pred, T0, !A, !B, !C),
    Pred(E0, !A, !B, !C),
    do_fold3_pred(Pred, T1, !A, !B, !C),
    Pred(E1, !A, !B, !C),
    do_fold3_pred(Pred, T2, !A, !B, !C).
do_fold3_pred(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B,
        !C) :-
    do_fold3_pred(Pred, T0, !A, !B, !C),
    Pred(E0, !A, !B, !C),
    do_fold3_pred(Pred, T1, !A, !B, !C),
    Pred(E1, !A, !B, !C),
    do_fold3_pred(Pred, T2, !A, !B, !C),
    Pred(E2, !A, !B, !C),
    do_fold3_pred(Pred, T3, !A, !B, !C).

fold4(Pred, ct(_, Tin), !A, !B, !C, !D) :-
    do_fold4_pred(Pred, Tin, !A, !B, !C, !D).

:- pred do_fold4_pred(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5), set_tree234(T1),
    T2, T2, T3, T3, T4, T4, T5, T5).
:- mode do_fold4_pred(
    in(pred(in, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out) is det.
:- mode do_fold4_pred(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_fold4_pred(
    in(pred(in, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, di, uo) is det.
:- mode do_fold4_pred(
    in(pred(in, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode do_fold4_pred(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_fold4_pred(
    in(pred(in, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, di, uo) is semidet.

do_fold4_pred(_Pred, empty, !A, !B, !C, !D).
do_fold4_pred(Pred, two(E, T0, T1), !A, !B, !C, !D) :-
    do_fold4_pred(Pred, T0, !A, !B, !C, !D),
    Pred(E, !A, !B, !C, !D),
    do_fold4_pred(Pred, T1, !A, !B, !C, !D).
do_fold4_pred(Pred, three(E0, E1, T0, T1, T2), !A, !B, !C, !D) :-
    do_fold4_pred(Pred, T0, !A, !B, !C, !D),
    Pred(E0, !A, !B, !C, !D),
    do_fold4_pred(Pred, T1, !A, !B, !C, !D),
    Pred(E1, !A, !B, !C, !D),
    do_fold4_pred(Pred, T2, !A, !B, !C, !D).
do_fold4_pred(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B,
        !C, !D) :-
    do_fold4_pred(Pred, T0, !A, !B, !C, !D),
    Pred(E0, !A, !B, !C, !D),
    do_fold4_pred(Pred, T1, !A, !B, !C, !D),
    Pred(E1, !A, !B, !C, !D),
    do_fold4_pred(Pred, T2, !A, !B, !C, !D),
    Pred(E2, !A, !B, !C, !D),
    do_fold4_pred(Pred, T3, !A, !B, !C, !D).

fold5(Pred, ct(_, Tin), !A, !B, !C, !D, !E) :-
    do_fold5_pred(Pred, Tin, !A, !B, !C, !D, !E).

:- pred do_fold5_pred(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6), set_tree234(T1),
    T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode do_fold5_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode do_fold5_pred(
    in(pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_fold5_pred(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode do_fold5_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode do_fold5_pred(
    in(pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_fold5_pred(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

do_fold5_pred(_Pred, empty, !A, !B, !C, !D, !E).
do_fold5_pred(Pred, two(E, T0, T1), !A, !B, !C, !D, !E) :-
    do_fold5_pred(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E, !A, !B, !C, !D, !E),
    do_fold5_pred(Pred, T1, !A, !B, !C, !D, !E).
do_fold5_pred(Pred, three(E0, E1, T0, T1, T2), !A, !B,
        !C, !D, !E) :-
    do_fold5_pred(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E0, !A, !B, !C, !D, !E),
    do_fold5_pred(Pred, T1, !A, !B, !C, !D, !E),
    Pred(E1, !A, !B, !C, !D, !E),
    do_fold5_pred(Pred, T2, !A, !B, !C, !D, !E).
do_fold5_pred(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B,
        !C, !D, !E) :-
    do_fold5_pred(Pred, T0, !A, !B, !C, !D, !E),
    Pred(E0, !A, !B, !C, !D, !E),
    do_fold5_pred(Pred, T1, !A, !B, !C, !D, !E),
    Pred(E1, !A, !B, !C, !D, !E),
    do_fold5_pred(Pred, T2, !A, !B, !C, !D, !E),
    Pred(E2, !A, !B, !C, !D, !E),
    do_fold5_pred(Pred, T3, !A, !B, !C, !D, !E).

fold6(Pred, ct(_, Tin), !A, !B, !C, !D, !E, !F) :-
    do_fold6_pred(Pred, Tin, !A, !B, !C, !D, !E, !F).

:- pred do_fold6_pred(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_tree234(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
:- mode do_fold6_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode do_fold6_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode do_fold6_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode do_fold6_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, in, out)
        is semidet),
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode do_fold6_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo)
        is semidet),
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode do_fold6_pred(
    in(pred(in, in, out, in, out, in, out, in, out, in, out, di, uo)
        is semidet),
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

do_fold6_pred(_Pred, empty, !A, !B, !C, !D, !E, !F).
do_fold6_pred(Pred, two(E, T0, T1), !A, !B, !C, !D, !E, !F) :-
    do_fold6_pred(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E, !A, !B, !C, !D, !E, !F),
    do_fold6_pred(Pred, T1, !A, !B, !C, !D, !E, !F).
do_fold6_pred(Pred, three(E0, E1, T0, T1, T2), !A, !B,
        !C, !D, !E, !F) :-
    do_fold6_pred(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E0, !A, !B, !C, !D, !E, !F),
    do_fold6_pred(Pred, T1, !A, !B, !C, !D, !E, !F),
    Pred(E1, !A, !B, !C, !D, !E, !F),
    do_fold6_pred(Pred, T2, !A, !B, !C, !D, !E, !F).
do_fold6_pred(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A, !B,
        !C, !D, !E, !F) :-
    do_fold6_pred(Pred, T0, !A, !B, !C, !D, !E, !F),
    Pred(E0, !A, !B, !C, !D, !E, !F),
    do_fold6_pred(Pred, T1, !A, !B, !C, !D, !E, !F),
    Pred(E1, !A, !B, !C, !D, !E, !F),
    do_fold6_pred(Pred, T2, !A, !B, !C, !D, !E, !F),
    Pred(E2, !A, !B, !C, !D, !E, !F),
    do_fold6_pred(Pred, T3, !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%
:- end_module set_ctree234.
%---------------------------------------------------------------------------%
