%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 1999-2006, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: set_bbbtree.m.
% Main authors: benyi.
% Stability: low.
%
% This module implements sets using bounded balanced binary trees.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module set_bbbtree.
:- interface.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

:- type set_bbbtree(T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % `init(Set)' returns an initialized empty set.
    %
:- func init = set_bbbtree(T).
:- pred init(set_bbbtree(T)::uo) is det.

    % `singleton_set(X, Set)' is true iff `Set' is the set
    % containing just the single element `X'.
    %
:- pred singleton_set(T, set_bbbtree(T)).
:- mode singleton_set(in, out) is det.
:- mode singleton_set(in, in) is semidet.
:- mode singleton_set(out, in) is semidet.

:- func make_singleton_set(T) = set_bbbtree(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

    % `empty(Set)' is true iff `Set' is an empty set.
    % `is_empty' is a synonym for `empty'.
    %
:- pred empty(set_bbbtree(T)::in) is semidet.
:- pred is_empty(set_bbbtree(T)::in) is semidet.

    % `non_empty(Set)' is true iff `Set' is not an empty set.
    % `is_non_empty' is a synonym for `non_empty'.
    %
:- pred non_empty(set_bbbtree(T)::in) is semidet.
:- pred is_non_empty(set_bbbtree(T)::in) is semidet.

:- pred is_singleton(set_bbbtree(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % `member(X, Set)' is true iff `X' is a member of `Set'.
    % O(lg n) for (in, in) and O(1) for (out, in).
    %
:- pred member(T, set_bbbtree(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % `is_member(X, Set, Result)' is true iff `X' is a member
    % of `Set'.
    %
:- pred is_member(T::in, set_bbbtree(T)::in, bool::out) is det.

    % `contains(Set, X)' is true iff `X' is a member of `Set'.
    % O(lg n).
    %
:- pred contains(set_bbbtree(T)::in, T::in) is semidet.

    % `least(Set, X)' is true iff `X' is smaller than all
    % the other members of `Set'.
    %
:- pred least(set_bbbtree(T), T).
:- mode least(in, out) is semidet.
:- mode least(in, in) is semidet.

    % `largest(Set, X)' is true iff `X' is larger than all
    % the other members of `Set'.
    %
:- pred largest(set_bbbtree(T), T).
:- mode largest(in, out) is semidet.
:- mode largest(in, in) is semidet.

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % `insert(X, Set0, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only `X'.
    %
:- func insert(set_bbbtree(T), T) = set_bbbtree(T).
:- pred insert(T, set_bbbtree(T), set_bbbtree(T)).
:- mode insert(di, di, uo) is det.
:- mode insert(in, in, out) is det.

    % `insert_new(X, Set0, Set)' is true iff `Set0' does not
    % contain `X', and `Set' is the union of `Set0' and the set containing
    % only `X'.
    %
:- pred insert_new(T::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.

    % `insert_list(Xs, Set0, Set)' is true iff `Set' is
    % the union of `Set0' and the set containing only the members of `Xs'.
    %
:- func insert_list(set_bbbtree(T), list(T)) = set_bbbtree(T).
:- pred insert_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

    % `delete(X, Set0, Set)' is true iff `Set' is the relative
    % complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- func delete(set_bbbtree(T), T) = set_bbbtree(T).
:- pred delete(T, set_bbbtree(T), set_bbbtree(T)).
:- mode delete(in, di, uo) is det.
:- mode delete(in, in, out) is det.

    % `delete_list(Xs, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only the members
    % of `Xs'.
    %
:- func delete_list(set_bbbtree(T), list(T)) = set_bbbtree(T).
:- pred delete_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

    % `remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
    % The det_remove version throws an exception instead of failing.
    %
:- pred remove(T::in, set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.
:- pred det_remove(T::in, set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

    % `remove_list(Xs, Set0, Set)' is true iff Xs does not
    % contain any duplicates, `Set0' contains every member of `Xs',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only the members of `Xs'.
    %
    % The det_remove_list version throws an exception instead of failing.
    %
:- pred remove_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.
:- pred det_remove_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

    % `remove_least(X, Set0, Set)' is true iff the union if
    % `X' and `Set' is `Set0' and `X' is smaller than all the elements of
    % `Set'.
    %
:- pred remove_least(T::out,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.

    % `remove_largest(X, Set0, Set)' is true iff the union if
    % `X' and `Set' is `Set0' and `X' is larger than all the elements of
    % `Set'.
    %
:- pred remove_largest(T::out,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB'
    % contain the same elements.
    %
:- pred equal(set_bbbtree(T)::in, set_bbbtree(T)::in) is semidet.

    % `subset(SetA, SetB)' is true iff all the elements of
    % `SetA' are also elements of `SetB'.
    %
:- pred subset(set_bbbtree(T)::in, set_bbbtree(T)::in) is semidet.

    % `superset(SetA, SetB)' is true iff all the elements of
    % `SetB' are also elements of `SetA'.
    %
:- pred superset(set_bbbtree(T)::in, set_bbbtree(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % `union(SetA, SetB, Set)' is true iff `Set' is the union
    % of `SetA' and `SetB'.
    %
:- func union(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).
:- pred union(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

    % `union_list(Sets) = Set' is true iff `Set' is the union
    % of all the sets in `Sets'
    %
:- func union_list(list(set_bbbtree(T))) = set_bbbtree(T).

    % `power_union(Sets, Set)' is true iff `Set' is the union
    % of all the sets in `Sets'
    %
:- func power_union(set_bbbtree(set_bbbtree(T))) = set_bbbtree(T).
:- pred power_union(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::out) is det.

    % `intersect(SetA, SetB, Set)' is true iff `Set' is the
    % intersection of `SetA' and `SetB'.
    %
:- func intersect(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).
:- pred intersect(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

    % `intersect_list(Sets) = Set is true iff `Set' is the
    % intersection of the sets in `Sets'.
    %
:- func intersect_list(list(set_bbbtree(T))) = set_bbbtree(T).

    % `power_intersect(Sets, Set) is true iff `Set' is the
    % intersection of the sets in `Sets'.
    %
:- func power_intersect(set_bbbtree(set_bbbtree(T))) = set_bbbtree(T).
:- pred power_intersect(set_bbbtree(set_bbbtree(T))::in, set_bbbtree(T)::out)
    is det.

    % `set_bbtree.difference(SetA, SetB, Set)' is true iff `Set' is the
    %  set containing all the elements of `SetA' except those that
    % occur in `SetB'.
    %
:- func difference(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).
:- pred difference(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % `list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'. O(n lg n)
    %
:- func list_to_set(list(T)) = set_bbbtree(T).
:- pred list_to_set(list(T)::in, set_bbbtree(T)::out) is det.

    % A synonym for set_bbtree.list_to_set/1.
    %
:- func from_list(list(T)) = set_bbbtree(T).

    % `sorted_list_to_set(List, Set)' is true iff `Set' is the
    % set containing only the members of `List'.
    % `List' must be sorted in ascending order, and must not contain
    % any duplicates. O(n).
    %
    % The sorted_list_to_set_len version allows the caller to provide
    % the length of List, which avoids the cost of computing it again.
    % This version will throw an exception if the length is incorrect.
    %
:- func sorted_list_to_set(list(T)) = set_bbbtree(T).
:- pred sorted_list_to_set(list(T)::in, set_bbbtree(T)::out) is det.
:- pred sorted_list_to_set_len(list(T)::in, set_bbbtree(T)::out,
    int::in) is det.

    % `rev_sorted_list_to_set(List) = Set' is true iff `Set' is the set
    % containing only the members of `List'. `List' must be sorted
    % in descending order.
    %
:- func rev_sorted_list_to_set(list(T)) = set_bbbtree(T).
:- pred rev_sorted_list_to_set(list(T)::in, set_bbbtree(T)::out) is det.
:- pred rev_sorted_list_to_set_len(list(T)::in, set_bbbtree(T)::out,
    int::in) is det.

    % A synonym for sorted_list_to_set/1.
    %
:- func from_sorted_list(list(T)) = set_bbbtree(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % `to_sorted_list(Set, List)' is true iff `List' is the
    % list of all the members of `Set', in sorted order. O(n).
    %
:- func to_sorted_list(set_bbbtree(T)) = list(T).
:- pred to_sorted_list(set_bbbtree(T), list(T)).
:- mode to_sorted_list(di, uo) is det.
:- mode to_sorted_list(in, out) is det.

%---------------------------------------------------------------------------%
%
% Counting.
%

    % `count(Set, Count)' is true iff `Set' has `Count' elements.
    % i.e. `Count' is the cardinality (size) of the set.
    %
:- func count(set_bbbtree(T)) = int.
:- pred count(set_bbbtree(T)::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), set_bbbtree(T)::in)
    is semidet.

    % filter(Pred, Items, Trues):
    % Return the set of items for which Pred succeeds.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

    % filter(Pred, Items, Trues, Falses):
    % Return the set of items for which Pred succeeds,
    % and the set for which it fails.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_bbbtree(T)::in, set_bbbtree(T)::out, set_bbbtree(T)::out) is det.

:- func filter_map(func(T1) = T2, set_bbbtree(T1)) = set_bbbtree(T2).
:- mode filter_map(func(in) = out is semidet, in) = out is det.

:- func map(func(T1) = T2, set_bbbtree(T1)) = set_bbbtree(T2).

:- func fold(func(T1, T2) = T2, set_bbbtree(T1), T2) = T2.
:- pred fold(pred(T1, T2, T2), set_bbbtree(T1), T2, T2).
:- mode fold(pred(in, in, out) is det, in, in, out) is det.
:- mode fold(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold(pred(in, di, uo) is semidet, in, di, uo) is semidet.

:- pred fold2(pred(T1, T2, T2, T3, T3), set_bbbtree(T1),
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

:- pred fold3(pred(T1, T2, T2, T3, T3, T4, T4),
    set_bbbtree(T1), T2, T2, T3, T3, T4, T4).
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

:- pred fold4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5),
    set_bbbtree(T1), T2, T2, T3, T3, T4, T4, T5, T5).
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

:- pred fold5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_bbbtree(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
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

:- pred fold6(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_bbbtree(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module int.

% Implementation based on "Functional Pearls: Efficient sets - a balancing act"
% by Stephen Adams, J. Functional Programming 3 (4): 553-561, Oct 1993.
%
% Note: concat4 and not concat3 represents the function concat3 mentioned
% in the report.
%
% Predicates with the suffix `_r' in the names are predicates that accept an
% integer as an additional last argument. This integer is a ratio that is
% used to measure how much larger one tree is allowed to be compared to the
% other before some rotations are performed to rebalance them. These predicates
% are currently not exported but it maybe useful to do so to allow users to
% influence the rebalancing process.
%
% NOTE:
% The size of trees are measured in terms of number of elements and not height.
% Also the fact that ratio is an integer seems counter intuitive but it should
% be realized that this property is true at all levels of the tree. Hence the
% default ratio of 5 will not allow the two trees to differ in height by more
% that a few units.
%
% BUGS:
% Due to the presence of bugs in the compiler concerning destructive input and
% unique modes some predicates and modes are commented out. Once the bugs are
% removed the modes should be uncommented. Although some commented out
% predicates can simply be uncommented and their hacked versions deleted
% others will need to be rewritten. Three examples are the _r versions of
% union, intersection and difference. They all make the calls split_lt
% followed by split_gt with the right tree. If the right tree is declared
% destructive input then either the compiler must be smart enough to call
% split_lt non-destructively followed by the call to split_gt destructively
% or some rewriting must be done.
%
% IMPROVEMENTS:
% Speed improvements may be possible by the implementation of specific
% predicates for predicates such as `equal' as opposed to the
% use of other set operations.
%
%---------------------------------------------------------------------------%

:- type set_bbbtree(T)
    --->    empty
    ;       tree(T, int, set_bbbtree(T), set_bbbtree(T)).

    % def_ratio(Ratio) returns the ratio that is used in deciding
    % whether two trees require re-balancing.
    %
:- pred def_ratio(int::uo) is det.

def_ratio(5).

%---------------------------------------------------------------------------%

init = S :-
    init(S).

init(empty).

singleton_set(V, tree(V, 1, empty, empty)).

make_singleton_set(T) = S :-
    singleton_set(T, S).

%---------------------------------------------------------------------------%

empty(empty).
is_empty(empty).

non_empty(tree(_, _, _, _)).
is_non_empty(tree(_, _, _, _)).

is_singleton(tree(V, 1, empty, empty), V).

%---------------------------------------------------------------------------%

member(X, tree(V, _N, L, R)) :-
    compare(Result, X, V),
    (
        Result = (<),
        member(X, L)   % search left subtree
    ;
        Result = (>),
        member(X, R)   % search right subtree
    ;
        Result = (=),
        X = V
    ).

is_member(X, Set, Result) :-
    ( if member(X, Set) then
        Result = yes
    else
        Result = no
    ).

contains(Set, X) :-
    member(X, Set).

least(tree(V, _N, L, _R), X) :-
    (
        % Found least element.
        L = empty,
        X = V
    ;
        % Search further in left subtree.
        L = tree(_V0, _N0, _L0, _R0),
        least(L, X)
    ).

largest(tree(V, _N, _L, R), X) :-
    (
        % Found largest element.
        R = empty,
        X = V
    ;
        % Search further in right subtree.
        R = tree(_V0, _N0, _L0, _R0),
        largest(R, X)
    ).

%---------------------------------------------------------------------------%

insert(!.S, T) = !:S :-
    insert(T, !S).

insert(X, !Set) :-
    % This is a hack to handle the bugs with unique and destructive
    % input modes.
    def_ratio(Ratio),
    insert_r(!.Set, X, !:Set, Ratio),
    % When destructive input and unique modes are fixed, remove this.
    unsafe_promise_unique(!Set).

:- pred insert_r(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out,
    int::in) is det.

insert_r(empty, X, tree(X, 1, empty, empty), _Ratio).
    % X was not in the set so make new node with X as the value.
insert_r(tree(V, N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        % Insert X into left subtree and re-balance it.
        Result = (<),
        insert_r(L, X, NewL, Ratio),
        balance(V, NewL, R, Set, Ratio)
    ;
        % Insert X into right subtree and re-balance it.
        Result = (>),
        insert_r(R, X, NewR, Ratio),
        balance(V, L, NewR, Set, Ratio)
    ;
        % X already in tree.
        Result = (=),
        Set = tree(V, N, L, R)
    ).

insert_new(X, !Set) :-
    % This is a hack to handle the bugs with unique and destructive
    % input modes.
    def_ratio(Ratio),
    insert_new_r(!.Set, X, !:Set, Ratio).

:- pred insert_new_r(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out, int::in)
    is semidet.

insert_new_r(empty, X, tree(X, 1, empty, empty), _Ratio).
    % X was not in the set so make new node with X as the value.
insert_new_r(tree(V, _N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        % Insert X into left subtree and re-balance it.
        Result = (<),
        insert_new_r(L, X, NewL, Ratio),
        balance(V, NewL, R, Set, Ratio)
    ;
        % Insert X into right subtree and re-balance it.
        Result = (>),
        insert_new_r(R, X, NewR, Ratio),
        balance(V, L, NewR, Set, Ratio)
    ;
        % X already in tree.
        Result = (=),
        fail
    ).

%---------------------%

insert_list(!.S, Xs) = !:S :-
    insert_list(Xs, !S).

insert_list(List, !Set) :-
    def_ratio(Ratio),
    insert_list_r(!.Set, List, !:Set, Ratio).

:- pred insert_list_r(set_bbbtree(T)::in, list(T)::in,
    set_bbbtree(T)::out, int::in) is det.

insert_list_r(Set, [], Set, _Ratio).
insert_list_r(!.Set, [X | Xs], !:Set, Ratio) :-
    insert_r(!.Set, X, !:Set, Ratio),
    insert_list_r(!.Set, Xs, !:Set, Ratio).

%---------------------------------------------------------------------------%

% delete(empty, _X, empty).
% delete(tree(V, N, L, R), X, Set) :-
%   compare(Result, X, V),
%   (
%       % Delete X from left subtree.
%       Result = (<),
%       delete(L, X, NewL), % X in left tree
%       NewN = N - 1,
%       Set = tree(V, NewN, NewL, R)
%   ;
%       % Delete X from right subtree.
%       Result = (>),
%       delete(R, X, NewR), % X in right tree
%       NewN = N - 1,
%       Set = tree(V, NewN, L, NewR)
%   ;
%       % Found X so just concatenate its two subtrees together.
%       Result = (=),
%       concat3(L, R, Set)
%   ).

delete(!.S, T) = !:S :-
    delete(T, !S).

delete(X, !Set) :-
    ( if remove(X, !.Set, NewSet) then
        !:Set = NewSet
    else
        true
    ),
    unsafe_promise_unique(!Set).

%---------------------%

delete_list(!.S, Xs) = !:S :-
    delete_list(Xs, !S).

delete_list([], !Set).
delete_list([X | Xs], !Set) :-
    delete(X, !Set),
    delete_list(Xs, !Set).

%---------------------%

remove(X, tree(V, N, L, R), Set) :-
    compare(Result, X, V),
    (
        % Remove X from left subtree.
        Result = (<),
        remove(X, L, NewL), % X in left tree
        NewN = N - 1,
        Set = tree(V, NewN, NewL, R)
    ;
        % Remove X from right subtree.
        Result = (>),
        remove(X, R, NewR), % X in right tree
        NewN = N - 1,
        Set = tree(V, NewN, L, NewR)
    ;
        % Found X so just concatenate its two subtrees together.
        Result = (=),
        concat3(L, R, Set)
    ).

det_remove(X, !Set) :-
    ( if set_bbbtree.remove(X, !Set) then
        true
    else
        unexpected($pred, "remove failed")
    ).

%---------------------%

remove_list([], !Set).
remove_list([X | Xs], !Set) :-
    remove(X, !Set),
    remove_list(Xs, !Set).

det_remove_list(List, !Set) :-
    ( if set_bbbtree.remove_list(List, !Set) then
        true
    else
        unexpected($pred, "remove_list failed")
    ).

%---------------------%

remove_least(X, tree(V, N, L, R), Set) :-
    % The tree is not rebalanced as the removal of one element
    % will not cause the tree to become much more unbalanced.
    (
        % Found the least element.
        L = empty,
        X = V,
        Set = R
    ;
        % Search further in the left subtree.
        L = tree(_V, _N, _L, _R),
        remove_least(X, L, NewL),
        NewN = N - 1,
        Set = tree(V, NewN, NewL, R)
    ).

%---------------------%

remove_largest(X, tree(V, N, L, R), Set) :-
    % The tree is not rebalanced as the removal of one element
    % will not cause the tree to become much more unbalanced.
    (
        % Found the largest element.
        R = empty,
        X = V,
        Set = L
    ;
        % Search further in the right subtree.
        R = tree(_V, _N, _L, _R),
        remove_largest(X, R, NewR),
        NewN = N - 1,
        Set = tree(V, NewN, L, NewR)
    ).

%---------------------------------------------------------------------------%

    % Given two trees L and R, such that all the elements of L are less than
    % those of R, and an element V that lies between the values of L and the
    % values of R construct a new tree consisting of V, L and R that is
    % balanced by the use of single and double left and right rotations.
    %
:- pred balance(T::in, set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

balance(V, L, R, Set, Ratio) :-
    count(L, LSize),
    count(R, RSize),
    ( if
        Val = LSize + RSize,
        Val < 2
    then
        % The two trees are too small to bother rebalancing.
        build_node(V, L, R, Set)
    else if
        Val = Ratio * LSize,
        RSize > Val
    then
        (
            R = tree(_V0, _N0, RL, RR),
            count(RL, RLSize),  % Right side too big.
            count(RR, RRSize),
            ( if RLSize < RRSize then
                single_rot_l(V, L, R, Set)
            else
                double_rot_l(V, L, R, Set)
            )
        ;
            R = empty,
            error($pred, "balance.1")
        )
    else if
        Val = Ratio * RSize,
        LSize > Val
    then
        (
            L = tree(_V1, _N1, LL, LR),
            count(LL, LLSize),  % Left side too big.
            count(LR, LRSize),
            ( if LRSize < LLSize then
                single_rot_r(V, L, R, Set)
            else
                double_rot_r(V, L, R, Set)
            )
        ;
            L = empty,
            error($pred, "balance.2")
        )
    else
        build_node(V, L, R, Set)   % Already balanced
    ).

%---------------------%

    % Single rotation to the left.
    %
    %     A                        B
    %   /   \                    /   \
    %  X     B      ----->      A     Z
    %      /   \              /   \
    %     Y     Z            X     Y
    %
:- pred single_rot_l(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

single_rot_l(_A, _X, empty, _Set) :-
    error($pred, "single_rot_l").
single_rot_l(A, X, tree(B, _N, Y, Z), Set) :-
    build_node(A, X, Y, A_X_and_Y),
    build_node(B, A_X_and_Y, Z, Set).

    % Single rotation to the right.
    %
:- pred single_rot_r(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

single_rot_r(_B, empty, _Z, _Set) :-
    error($pred, "single_rot_r").
single_rot_r(B, tree(A, _N, X, Y), Z, Set) :-
    build_node(B, Y, Z, B_Y_and_Z),
    build_node(A, X, B_Y_and_Z, Set).

%---------------------%

    % Double rotation to the left.
    %
    %        A
    %      /   \                         B
    %     X     C                      /   \
    %         /   \     ----->       A       C
    %        B     Z               /   \   /   \
    %      /   \                  X    Y1 Y2    Z
    %    Y1     Y2
    %
:- pred double_rot_l(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

double_rot_l(_A, _X, empty, _Set) :-
    error($pred, "double_rot_l.1").
double_rot_l(A, X, tree(C, _N0, Y, Z), Set) :-
    (
        Y = tree(B, _N1, Y1, Y2),
        build_node(A, X, Y1, A_X_and_Y1),
        build_node(C, Y2, Z, C_Y2_and_Z),
        build_node(B, A_X_and_Y1, C_Y2_and_Z, Set)
    ;
        Y = empty,
        error($pred, "double_rot_l.2")
    ).

    % Double rotation to the right.
    %
:- pred double_rot_r(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

double_rot_r(_B, empty, _Z, _Set) :-
    error($pred, "double_rot_r.1").
double_rot_r(C, tree(A, _N0, X, Y), Z, Set) :-
    (
        Y = tree(B, _N1, Y1, Y2),
        build_node(A, X, Y1, A_X_and_Y1),
        build_node(C, Y2, Z, C_Y2_and_Z),
        build_node(B, A_X_and_Y1, C_Y2_and_Z, Set)
    ;
        Y = empty,
        error($pred, "double_rot_r.2")
    ).

%---------------------%

    % Given X, L and R create a new tree who's root is X,
    % left subtree is L and right subtree is R.
    %
:- pred build_node(T::in, set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

build_node(X, L, R, Tree) :-
    count(L, LSize),
    count(R, RSize),
    N = 1 + LSize + RSize,
    Tree0 = tree(X, N, L, R),
    unsafe_promise_unique(Tree0, Tree).

%---------------------%

    % Given two trees concatenate them by removing the greatest element
    % from the left subtree if it is larger than the right subtree by a
    % factor of ratio, else the smallest element from the right subtree,
    % and make it the root along with the two remaining trees as its subtrees.
    % No rebalancing is performed on the resultant tree. `concat3'
    % is largely used for deletion of elements. This predicate should not be
    % confused with the predicate `concat3' in the paper for that is `concat4'.
    %
:- pred concat3(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

concat3(L, R, Set) :-
    count(L, LSize),
    ( if LSize = 0 then
        % Left tree empty so just return the right tree.
        Set = R
    else
        count(R, RSize),
        ( if RSize = 0 then
            % Right tree empty so just return the left tree.
            Set = L
        else
            % If the left tree is the larger of the two, then remove its
            % largest value and make it the root of the new left and
            % the right trees. Otherwise, remove the smallest value from the
            % right tree and make it the root of the left and the
            % new right trees.
            ( if LSize > RSize then
                ( if remove_largest(X, L, NewL) then
                    build_node(X, NewL, R, Set)
                else
                    error($pred, "concat3.1")
                )
            else
                ( if remove_least(X, R, NewR) then
                    build_node(X, L, NewR, Set)
                else
                    error($pred, "concat3.2")
                )
            )
        )
    ).

%---------------------%

    % Given two trees L and R, such that all the elements of L are less than
    % those of R, and an element V that lies between the values of L and the
    % values of R construct a new tree consisting of V and the elements of
    % L and R. This predicate is the equivalent of concat3 in the paper.
    %
:- pred concat4(set_bbbtree(T)::in, set_bbbtree(T)::in, T::in,
    set_bbbtree(T)::out, int::in) is det.

concat4(empty, R, V, Set, Ratio) :-
    insert_r(R, V, Set, Ratio).
concat4(tree(LV, LN, LL, LR), R, V, Set, Ratio) :-
    (
        R = empty,
        insert_r(tree(LV, LN, LL, LR), V, Set, Ratio)
    ;
        R = tree(RV, RN, RL, RR),
        ( if
            Val = Ratio * LN,   % Right too big
            Val < RN
        then
            concat4(tree(LV, LN, LL, LR), RL, V, NewL, Ratio),
            balance(RV, NewL, RR, Set, Ratio)
        else if
            Val = Ratio * RN,   % Left too big
            Val < LN
        then
            concat4(LR, tree(RV, RN, RL, RR), V, NewR, Ratio),
            balance(LV, LL, NewR, Set, Ratio)
        else
            build_node(V, tree(LV, LN, LL, LR), R, Set)
        )
    ).

%---------------------------------------------------------------------------%

equal(SetA, SetB) :-
    subset(SetA, SetB),
    subset(SetB, SetA).

subset(SetA, SetB) :-
    difference(SetA, SetB, Set),
    empty(Set).

superset(SetA, SetB) :-
    subset(SetB, SetA).

%---------------------------------------------------------------------------%

% elem(x, A)  implies
%
% A union B =
%                   ( { elem(x, A) | x < a } union { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } union { elem(x, B) | x > a } )

union(S1, S2) = S3 :-
    union(S1, S2, S3).

union(SetA, SetB, Set) :-
    def_ratio(Ratio),
    union_r(SetA, SetB, Set, Ratio).

:- pred union_r(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

union_r(empty, Set, Set, _Ratio).
union_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
    split_lt(R, V, NewRL, Ratio),
    split_gt(R, V, NewRR, Ratio),
    union_r(LL, NewRL, LSet, Ratio),
    union_r(LR, NewRR, RSet, Ratio),
    concat4(LSet, RSet, V, Set, Ratio).

union_list(ListofSets) =
    list.foldl(union, ListofSets, init).

%---------------------%

% `power_union' is a divide and conquer algorithm. The power union
% of the two subtrees is determined and then unioned. Then the root set is
% unioned into the result. The choice to perform the power union of the two
% subtrees is due to the computational expense of union being proportional
% to the difference in height of the two subtrees. This way the two power
% union calls will likely create trees of similar sizes, and hence their
% union will be inexpensive. Unfortunately as the union grows it will
% increasing dwarf the tree that is the root node and hence this cost will
% increase, but this cannot be avoided.

power_union(SS) = S :-
    power_union(SS, S).

power_union(Sets, Set) :-
    def_ratio(Ratio),
    power_union_r(Sets, Set, Ratio).

:- pred power_union_r(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::out, int::in) is det.

power_union_r(empty, empty, _Ratio).
power_union_r(tree(V, _N, L, R), Set, Ratio) :-
    power_union_r(L, LUnion, Ratio),
    power_union_r(R, RUnion, Ratio),
    union_r(LUnion, RUnion, Union, Ratio),
    union_r(V, Union, Set, Ratio).

%---------------------%

% elem(x, A) and elem(x, B) implies
%
% A intersection B =
%                   ( { elem(x, A) | x < a } intersect { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } intersect { elem(x, B) | x > a } )

intersect(S1, S2) = S3 :-
    intersect(S1, S2, S3).

intersect(SetA, SetB, Set) :-
    def_ratio(Ratio),
    intersect_r(SetA, SetB, Set, Ratio).

:- pred intersect_r(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

intersect_r(empty, _Set, empty, _Ratio).
intersect_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
    split_lt(R, V, NewRL, Ratio),
    split_gt(R, V, NewRR, Ratio),
    intersect_r(LL, NewRL, LSet, Ratio),
    intersect_r(LR, NewRR, RSet, Ratio),
    ( if member(V, R) then
        concat4(LSet, RSet, V, Set, Ratio)
    else
        concat3(LSet, RSet, Set)
    ).

intersect_list([]) = init.
intersect_list([Set | Sets]) = intersect_list_r(Set, Sets, Ratio) :-
    def_ratio(Ratio).

:- func intersect_list_r(set_bbbtree(T), list(set_bbbtree(T)), int)
    = set_bbbtree(T).

intersect_list_r(Intersect, [], _Ratio) = Intersect.
intersect_list_r(Intersect0, [Set | Sets], Ratio) =
        intersect_list_r(Intersect1, Sets, Ratio) :-
    intersect_r(Intersect0, Set, Intersect1, Ratio).

% `power_intersect' is an accumulator based algorithm. Initially
% the accumulator is seeded with the tree at the root. Then the tree is
% traversed inorder and each tree at each node is respectively intersected with
% the accumulator. The aim of the algorithm is to rapidly reduce the size of
% the accumulator, possibly of the empty set in which case the call immediately
% returns.

power_intersect(SS) = S :-
    power_intersect(SS, S).

power_intersect(Sets, Set) :-
    def_ratio(Ratio),
    power_intersect_r(Sets, Set, Ratio).

:- pred power_intersect_r(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::out, int::in) is det.

power_intersect_r(empty, empty, _Ratio).
power_intersect_r(tree(V, _N, L, R), Set, Ratio) :-
    power_intersect_r2(L, V, Intersection0, Ratio),
    power_intersect_r2(R, Intersection0, Set, Ratio).

:- pred power_intersect_r2(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out, int::in) is det.

power_intersect_r2(empty, empty, empty, _Ratio).
power_intersect_r2(empty, tree(_V, _N, _L, _R), empty, _Ratio).
power_intersect_r2(tree(_V, _N, _L, _R), empty, empty, _Ratio).
power_intersect_r2(tree(V, _N, L, R), tree(AccV, AccN, AccL, AccR), Set,
        Ratio) :-
    intersect_r(V, tree(AccV, AccN, AccL, AccR), Intersection0, Ratio),
    power_intersect_r2(L, Intersection0, Intersection1, Ratio),
    power_intersect_r2(R, Intersection1, Set, Ratio).

%---------------------%

% elem(x, A) and not elem(x, B) implies
%
% A difference B =
%               ( { elem(x, A) | x < a } difference { elem(x, B) | x < a } )
%         union ( { a } )
%         union ( { elem(x, A) | x > a } difference { elem(x, B) | x > a } )

difference(S1, S2) = S3 :-
    difference(S1, S2, S3).

difference(SetA, SetB, Set) :-
    def_ratio(Ratio),
    difference_r(SetA, SetB, Set, Ratio).

:- pred difference_r(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

difference_r(empty, _Set, empty, _Ratio).
difference_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
    split_lt(R, V, NewRL, Ratio),
    split_gt(R, V, NewRR, Ratio),
    difference_r(LL, NewRL, LSet, Ratio),
    difference_r(LR, NewRR, RSet, Ratio),
    ( if member(V, R) then
        concat3(LSet, RSet, Set)
    else
        concat4(LSet, RSet, V, Set, Ratio)
    ).

%---------------------%

    % Given a set return the subset that is less that X.
    %
:- pred split_lt(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out, int::in)
    is det.

split_lt(empty, _X, empty, _Ratio).
split_lt(tree(V, _N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        Result = (<),
        split_lt(L, X, Set, Ratio)
    ;
        Result = (>),
        split_lt(R, X, Set0, Ratio),
        concat4(L, Set0, V, Set, Ratio)
    ;
        Result = (=),
        Set = L
    ).

    % Given a set return the subset of it that is greater than X.
    %
:- pred split_gt(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out,
    int::in) is det.

split_gt(empty, _X, empty, _Ratio).
split_gt(tree(V, _N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        Result = (>),
        split_gt(R, X, Set, Ratio)
    ;
        Result = (<),
        split_gt(L, X, Set0, Ratio),
        concat4(Set0, R, V, Set, Ratio)
    ;
        Result = (=),
        Set = R
    ).

%---------------------------------------------------------------------------%

list_to_set(Xs) = S :-
    list_to_set(Xs, S).

list_to_set(List, Set) :-
    def_ratio(Ratio),
    list_to_set_r(List, Set, Ratio).

:- pred list_to_set_r(list(T)::in, set_bbbtree(T)::out, int::in) is det.

list_to_set_r(List, Set, Ratio) :-
    init(InitSet),
    insert_list_r(InitSet, List, Set, Ratio).

from_list(List) = Set :-
    list_to_set(List, Set).

%---------------------%

% The tree is created by first determining it's length. All lists of length
% N have the same structure. The root of the tree is the N // 2 element
% of the list. Elements 1 to N // 2 - 1 make up the left subtree and elements
% N // 2 + 1 to N make up the right subtree. The structure, which is known
% due to the length of the list, is just created inorder while passing the
% list around in an accumulator and taking off elements as needed.
%
% The predicate sorted_list_to_set_len2 has been unfolded up to
% trees of size 3 so as to avoid recursive calls all the way to the leaves.
% This alone approximately halves execution time and stack usage.
% Unfolding further becomes a case of diminishing returns.
%
% Cases N = 3, N = 2 and N = 1 could safely be removed without change to
% computed results as long as the 3 in N > 3 is adjusted appropriately.

sorted_list_to_set(Xs) = S :-
    sorted_list_to_set(Xs, S).

sorted_list_to_set(List, Set) :-
    list.length(List, N),
    sorted_list_to_set_len(List, Set, N).

sorted_list_to_set_len([], empty, _N).
sorted_list_to_set_len([X | Xs], Set, N) :-
    sorted_list_to_set_len2([X | Xs], RestOfList, N, Set),
    % The list should be exhausted on completion.
    (
        RestOfList = []
    ;
        RestOfList = [_ | _],
        % Should never happen. Here only to satisfy det checker.
        error($pred, "impossible")
    ).

:- pred sorted_list_to_set_len2(list(T)::in, list(T)::out,
    int::in, set_bbbtree(T)::out) is det.

sorted_list_to_set_len2(List, RestOfList, N, Set) :-
    ( if N > 3 then
        NL = N//2,
        NR = N - NL - 1,
        sorted_list_to_set_len2(List, RestOfList0, NL, L),
        (
            RestOfList0 = [V | RestOfList1],
            sorted_list_to_set_len2(RestOfList1, RestOfList, NR, R),
            Set = tree(V, N, L, R)
        ;
            % Should never occur. Here only to satisfy det checker.
            RestOfList0 = [],
            error($pred, "impossible 1")
        )
    else if N = 3 then
        ( if List = [X, Y, Z | RestOfList0] then
            RestOfList = RestOfList0,
            Set = tree(Y, N,
                tree(X, 1, empty, empty),
                tree(Z, 1, empty, empty))
        else
            % Should never occur. Here only to satisfy det checker.
            error($pred, "impossible 2")
        )
    else if N = 2 then
        ( if List = [X, Y | RestOfList0] then
            RestOfList = RestOfList0,
            Set = tree(Y, N, tree(X, 1, empty, empty), empty)
        else
            % Should never occur. Here only to satisfy det checker.
            error($pred, "impossible 3")
        )
    else if N = 1 then
        ( if List = [X | RestOfList0] then
            RestOfList = RestOfList0,
            Set = tree(X, N, empty, empty)
        else
            % Should never occur. Here only to satisfy det checker.
            error($pred, "impossible 4")
        )
    else
            % N = 0.
        RestOfList = List,
        Set = empty
    ).

rev_sorted_list_to_set(RevList) = Set :-
    rev_sorted_list_to_set(RevList, Set).

rev_sorted_list_to_set(RevList, Set) :-
    % It should be possible to do better than this.
    list.reverse(RevList, List),
    sorted_list_to_set(List, Set).

rev_sorted_list_to_set_len(RevList, Set, N) :-
    % It should be possible to do better than this.
    list.reverse(RevList, List),
    sorted_list_to_set_len(List, Set, N).

from_sorted_list(List) = Set :-
    sorted_list_to_set(List, Set).

%---------------------------------------------------------------------------%

to_sorted_list(S) = Xs :-
    to_sorted_list(S, Xs).

to_sorted_list(Set, List) :-
    % Flatten the tree by an accumulator based tree traversal
    % traversing the tree in a right-to-left post order manner. O(n).
    to_sorted_list2(Set, [], List).

:- pred to_sorted_list2(set_bbbtree(T), list(T), list(T)).
:- mode to_sorted_list2(di, di, uo) is det.
:- mode to_sorted_list2(in, in, out) is det.

to_sorted_list2(empty, List, List).
to_sorted_list2(tree(V, _N, L, R), Acc, List) :-
    to_sorted_list2(R, Acc, List0),
    to_sorted_list2(L, [V|List0], List).

%---------------------------------------------------------------------------%

count(Set) = Count :-
    count(Set, Count).

count(empty, 0).
count(tree(_V, N, _L, _R), N).

%---------------------------------------------------------------------------%

all_true(P, S) :-
    to_sorted_list(S, L),
    list.all_true(P, L).

%---------------------%

filter(P, Set, TrueSet) :-
    to_sorted_list(Set, List),
    list.filter(P, List, TrueList),
    list_to_set(TrueList, TrueSet).

filter(P, Set, TrueSet, FalseSet) :-
    to_sorted_list(Set, List),
    list.filter(P, List, TrueList, FalseList),
    list_to_set(TrueList, TrueSet),
    list_to_set(FalseList, FalseSet).

filter_map(PF, S1) = S2 :-
    to_sorted_list(S1, L1),
    L2 = list.filter_map(PF, L1),
    list_to_set(L2, S2).

map(F, S1) = S2 :-
    to_sorted_list(S1, L1),
    L2 = list.map(F, L1),
    list_to_set(L2, S2).

%---------------------%

fold(F, S, A) = B :-
    % XXX we should just traverse the tree directly instead of converting
    % to a sorted list first.
    B = list.foldl(F, to_sorted_list(S), A).

fold(P, S, !A) :-
    list.foldl(P, to_sorted_list(S), !A).

fold2(P, S, !A, !B) :-
    list.foldl2(P, to_sorted_list(S), !A, !B).

fold3(P, S, !A, !B, !C) :-
    list.foldl3(P, to_sorted_list(S), !A, !B, !C).

fold4(P, S, !A, !B, !C, !D) :-
    list.foldl4(P, to_sorted_list(S), !A, !B, !C, !D).

fold5(P, S, !A, !B, !C, !D, !E) :-
    list.foldl5(P, to_sorted_list(S), !A, !B, !C, !D, !E).

fold6(P, S, !A, !B, !C, !D, !E, !F) :-
    list.foldl6(P, to_sorted_list(S), !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%
:- end_module set_bbbtree.
%---------------------------------------------------------------------------%
