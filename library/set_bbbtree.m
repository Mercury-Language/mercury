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

    % `init(Set)' returns an initialized empty set.
    %
:- func init = set_bbbtree(T).
:- pred init(set_bbbtree(T)::uo) is det.

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

    % `count(Set, Count)' is true iff `Set' has `Count' elements.
    % i.e. `Count' is the cardinality (size) of the set.
    %
:- func count(set_bbbtree(T)) = int.
:- pred count(set_bbbtree(T)::in, int::out) is det.

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

    % `singleton_set(X, Set)' is true iff `Set' is the set
    % containing just the single element `X'.
    %
:- pred singleton_set(T, set_bbbtree(T)).
:- mode singleton_set(in, out) is det.
:- mode singleton_set(in, in) is semidet.
:- mode singleton_set(out, in) is semidet.

:- func make_singleton_set(T) = set_bbbtree(T).

:- pred is_singleton(set_bbbtree(T)::in, T::out) is semidet.

    % `equal(SetA, SetB)' is true iff `SetA' and `SetB'
    % contain the same elements.
    %
:- pred equal(set_bbbtree(T)::in, set_bbbtree(T)::in) is semidet.

    % `insert(X, Set0, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only `X'.
    %
:- pred insert(T, set_bbbtree(T), set_bbbtree(T)).
:- mode insert(di, di, uo) is det.
:- mode insert(in, in, out) is det.

:- func insert(set_bbbtree(T), T) = set_bbbtree(T).

    % `insert_new(X, Set0, Set)' is true iff `Set0' does not
    % contain `X', and `Set' is the union of `Set0' and the set containing
    % only `X'.
    %
:- pred insert_new(T::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.

    % `insert_list(Xs, Set0, Set)' is true iff `Set' is
    % the union of `Set0' and the set containing only the members of `Xs'.
    %
:- pred insert_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

:- func insert_list(set_bbbtree(T), list(T)) = set_bbbtree(T).

    % `delete(X, Set0, Set)' is true iff `Set' is the relative
    % complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred delete(T, set_bbbtree(T), set_bbbtree(T)).
:- mode delete(in, di, uo) is det.
:- mode delete(in, in, out) is det.

:- func delete(set_bbbtree(T), T) = set_bbbtree(T).

    % `delete_list(Xs, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only the members
    % of `Xs'.
    %
:- pred delete_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

:- func delete_list(set_bbbtree(T), list(T)) = set_bbbtree(T).

    % `remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
:- pred remove(T::in, set_bbbtree(T)::in, set_bbbtree(T)::out)
    is semidet.

    % `remove_list(Xs, Set0, Set)' is true iff Xs does not
    % contain any duplicates, `Set0' contains every member of `Xs',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only the members of `Xs'.
    %
:- pred remove_list(list(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is semidet.

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

    % `list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'. O(n lg n)
    %
:- pred list_to_set(list(T)::in, set_bbbtree(T)::out) is det.

:- func list_to_set(list(T)) = set_bbbtree(T).

    % A synonym for set_bbtree.list_to_set/1.
    %
:- func from_list(list(T)) = set_bbbtree(T).

    % `sorted_list_to_set(List, Set)' is true iff `Set' is the
    % set containing only the members of `List'.
    % `List' must be sorted. O(n).
    %
:- pred sorted_list_to_set(list(T)::in, set_bbbtree(T)::out)
    is det.

:- func sorted_list_to_set(list(T)) = set_bbbtree(T).

    % A synonym for sorted_list_to_set/1.
    %
:- func from_sorted_list(list(T)) = set_bbbtree(T).

    % `sorted_list_to_set_len(List, Set, N)' is true iff
    % `Set' is the set containing only the members of `List' and `N'
    % is the length of the list. If the length of the list is already known
    % then a noticeable speed improvement can be expected over
    % `sorted_list_to_set' as a significant cost involved
    % with `sorted_list_to_set' is the call to list.length.
    % `List' must be sorted. O(n).
    %
:- pred sorted_list_to_set_len(list(T)::in, set_bbbtree(T)::out,
    int::in) is det.

    % `to_sorted_list(Set, List)' is true iff `List' is the
    % list of all the members of `Set', in sorted order. O(n).
    %
:- pred to_sorted_list(set_bbbtree(T), list(T)).
:- mode to_sorted_list(di, uo) is det.
:- mode to_sorted_list(in, out) is det.

:- func to_sorted_list(set_bbbtree(T)) = list(T).

    % `union(SetA, SetB, Set)' is true iff `Set' is the union
    % of `SetA' and `SetB'.
    %
:- pred union(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

:- func union(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).

    % `union_list(Sets) = Set' is true iff `Set' is the union
    % of all the sets in `Sets'
    %
:- func union_list(list(set_bbbtree(T))) = set_bbbtree(T).

    % `power_union(Sets, Set)' is true iff `Set' is the union
    % of all the sets in `Sets'
    %
:- pred power_union(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::out) is det.

:- func power_union(set_bbbtree(set_bbbtree(T))) = set_bbbtree(T).

    % `intersect(SetA, SetB, Set)' is true iff `Set' is the
    % intersection of `SetA' and `SetB'.
    %
:- pred intersect(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

:- func intersect(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).

    % `power_intersect(Sets, Set) is true iff `Set' is the
    % intersection of the sets in `Sets'.
    %
:- pred power_intersect(set_bbbtree(set_bbbtree(T))::in, set_bbbtree(T)::out)
    is det.

:- func power_intersect(set_bbbtree(set_bbbtree(T))) = set_bbbtree(T).

    % `intersect_list(Sets) = Set is true iff `Set' is the
    % intersection of the sets in `Sets'.
    %
:- func intersect_list(list(set_bbbtree(T))) = set_bbbtree(T).

    % `set_bbtree.difference(SetA, SetB, Set)' is true iff `Set' is the
    %  set containing all the elements of `SetA' except those that
    % occur in `SetB'.
    %
:- pred difference(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

:- func difference(set_bbbtree(T), set_bbbtree(T)) = set_bbbtree(T).

    % `subset(SetA, SetB)' is true iff all the elements of
    % `SetA' are also elements of `SetB'.
    %
:- pred subset(set_bbbtree(T)::in, set_bbbtree(T)::in) is semidet.

    % `superset(SetA, SetB)' is true iff all the elements of
    % `SetB' are also elements of `SetA'.
    %
:- pred superset(set_bbbtree(T)::in, set_bbbtree(T)::in) is semidet.

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

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), set_bbbtree(T)::in)
    is semidet.

:- func map(func(T1) = T2, set_bbbtree(T1)) = set_bbbtree(T2).

:- func filter_map(func(T1) = T2, set_bbbtree(T1))
    = set_bbbtree(T2).
:- mode filter_map(func(in) = out is semidet, in) = out is det.

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module int.

% Implementation based on "Functional Pearls: Efficient sets - a balancing act"
% by Stephen Adams, J. Functional Programming 3 (4): 553-561, Oct 1993.
%
% Note: set_bbbtree.concat4 and not set_bbbtree.concat3 represents the
% function concat3 mentioned in the report.
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
% union, intersection and difference. They all make the calls
% set_bbbtree.split_lt followed by set_bbbtree.split_gt with the right
% tree. If the right tree is declared destructive input then either the
% compiler must be smart enough to call set_bbbtree.split_lt
% non-destructively followed by the call to set_bbbtree.split_gt
% destructively or some rewriting must be done.
%
% IMPROVEMENTS:
% Speed improvements may be possible by the implementation of specific
% predicates for predicates such as `set_bbbtree.equal' as opposed to the
% use of other set operations.
%
%---------------------------------------------------------------------------%

:- type set_bbbtree(T)
    --->    empty
    ;       tree(T, int, set_bbbtree(T), set_bbbtree(T)).

    % `set_bbbtree.def_ratio(Ratio)' returns the ratio that is used in
    % deciding whether two trees require re-balancing.

:- pred set_bbbtree.def_ratio(int::uo) is det.

set_bbbtree.def_ratio(5).

%---------------------------------------------------------------------------%

set_bbbtree.init = S :-
    set_bbbtree.init(S).

set_bbbtree.init(empty).

%---------------------------------------------------------------------------%

set_bbbtree.empty(empty).
set_bbbtree.is_empty(empty).

set_bbbtree.non_empty(tree(_, _, _, _)).
set_bbbtree.is_non_empty(tree(_, _, _, _)).

%---------------------------------------------------------------------------%

set_bbbtree.count(Set) = Count :-
    set_bbbtree.count(Set, Count).

set_bbbtree.count(empty, 0).
set_bbbtree.count(tree(_V, N, _L, _R), N).

%---------------------------------------------------------------------------%

% set_bbbtree.member(X, empty) :- fail.
set_bbbtree.member(X, tree(V, _N, L, R)) :-
    compare(Result, X, V),
    (
        Result = (<),
        set_bbbtree.member(X, L)   % search left subtree
    ;
        Result = (>),
        set_bbbtree.member(X, R)   % search right subtree
    ;
        Result = (=),
        X = V
    ).

set_bbbtree.contains(Set, X) :-
    set_bbbtree.member(X, Set).

%---------------------------------------------------------------------------%

set_bbbtree.is_member(X, Set, Result) :-
    ( if set_bbbtree.member(X, Set) then
        Result = yes
    else
        Result = no
    ).

%---------------------------------------------------------------------------%

% set_bbbtree.least(empty, _) :- fail.
set_bbbtree.least(tree(V, _N, L, _R), X) :-
    (
        % Found least element.
        L = empty,
        X = V
    ;
        % Search further in left subtree.
        L = tree(_V0, _N0, _L0, _R0),
        set_bbbtree.least(L, X)
    ).

%---------------------------------------------------------------------------%

% set_bbbtree.largest(empty, _) :- fail.
set_bbbtree.largest(tree(V, _N, _L, R), X) :-
    (
        % Found largest element.
        R = empty,
        X = V
    ;
        % Search further in right subtree.
        R = tree(_V0, _N0, _L0, _R0),
        set_bbbtree.largest(R, X)
    ).

%---------------------------------------------------------------------------%

set_bbbtree.make_singleton_set(T) = S :-
    set_bbbtree.singleton_set(T, S).

set_bbbtree.singleton_set(V, tree(V, 1, empty, empty)).

set_bbbtree.is_singleton(tree(V, 1, empty, empty), V).

%---------------------------------------------------------------------------%

set_bbbtree.equal(SetA, SetB) :-
    set_bbbtree.subset(SetA, SetB),
    set_bbbtree.subset(SetB, SetA).

%---------------------------------------------------------------------------%

set_bbbtree.insert(!.S, T) = !:S :-
    set_bbbtree.insert(T, !S).

set_bbbtree.insert(X, !Set) :-
    % This is a hack to handle the bugs with unique and destructive
    % input modes.
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.insert_r(!.Set, X, !:Set, Ratio),
    % When destructive input and unique modes are fixed, remove this.
    unsafe_promise_unique(!Set).

:- pred set_bbbtree.insert_r(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out,
    int::in) is det.

set_bbbtree.insert_r(empty, X, tree(X, 1, empty, empty), _Ratio).
    % X was not in the set so make new node with X as the value.
set_bbbtree.insert_r(tree(V, N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        % Insert X into left subtree and re-balance it.
        Result = (<),
        set_bbbtree.insert_r(L, X, NewL, Ratio),
        set_bbbtree.balance(V, NewL, R, Set, Ratio)
    ;
        % Insert X into right subtree and re-balance it.
        Result = (>),
        set_bbbtree.insert_r(R, X, NewR, Ratio),
        set_bbbtree.balance(V, L, NewR, Set, Ratio)
    ;
        % X already in tree.
        Result = (=),
        Set = tree(V, N, L, R)
    ).

set_bbbtree.insert_new(X, !Set) :-
    % This is a hack to handle the bugs with unique and destructive
    % input modes.
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.insert_new_r(!.Set, X, !:Set, Ratio).

:- pred set_bbbtree.insert_new_r(
    set_bbbtree(T)::in, T::in, set_bbbtree(T)::out, int::in) is semidet.

set_bbbtree.insert_new_r(empty, X, tree(X, 1, empty, empty), _Ratio).
    % X was not in the set so make new node with X as the value.
set_bbbtree.insert_new_r(tree(V, _N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        % Insert X into left subtree and re-balance it.
        Result = (<),
        set_bbbtree.insert_new_r(L, X, NewL, Ratio),
        set_bbbtree.balance(V, NewL, R, Set, Ratio)
    ;
        % Insert X into right subtree and re-balance it.
        Result = (>),
        set_bbbtree.insert_new_r(R, X, NewR, Ratio),
        set_bbbtree.balance(V, L, NewR, Set, Ratio)
    ;
        % X already in tree.
        Result = (=),
        fail
    ).

%---------------------------------------------------------------------------%

set_bbbtree.insert_list(!.S, Xs) = !:S :-
    set_bbbtree.insert_list(Xs, !S).

set_bbbtree.insert_list(List, !Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.insert_list_r(!.Set, List, !:Set, Ratio).

:- pred set_bbbtree.insert_list_r(set_bbbtree(T)::in, list(T)::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.insert_list_r(Set, [], Set, _Ratio).
set_bbbtree.insert_list_r(!.Set, [X | Xs], !:Set, Ratio) :-
    set_bbbtree.insert_r(!.Set, X, !:Set, Ratio),
    set_bbbtree.insert_list_r(!.Set, Xs, !:Set, Ratio).

%---------------------------------------------------------------------------%

% set_bbbtree.delete(empty, _X, empty).
% set_bbbtree.delete(tree(V, N, L, R), X, Set) :-
%   compare(Result, X, V),
%   (
%       % Delete X from left subtree.
%       Result = (<),
%       set_bbbtree.delete(L, X, NewL), % X in left tree
%       NewN = N - 1,
%       Set = tree(V, NewN, NewL, R)
%   ;
%       % Delete X from right subtree.
%       Result = (>),
%       set_bbbtree.delete(R, X, NewR), % X in right tree
%       NewN = N - 1,
%       Set = tree(V, NewN, L, NewR)
%   ;
%       % Found X so just concatenate its two subtrees together.
%       Result = (=),
%       set_bbbtree.concat3(L, R, Set)
%   ).
%
% set_bbbtree.delete(Set0, X, Set) :-
%   ( if set_bbbtree.remove(Set0, X, Set1) then
%       Set = Set1
%   else
%       Set = Set0
%   ).

set_bbbtree.delete(!.S, T) = !:S :-
    set_bbbtree.delete(T, !S).

set_bbbtree.delete(X, !Set) :-
    ( if set_bbbtree.remove(X, !.Set, NewSet) then
        !:Set = NewSet
    else
        true
    ),
    unsafe_promise_unique(!Set).

%---------------------------------------------------------------------------%

set_bbbtree.delete_list(!.S, Xs) = !:S :-
    set_bbbtree.delete_list(Xs, !S).

set_bbbtree.delete_list([], !Set).
set_bbbtree.delete_list([X | Xs], !Set) :-
    set_bbbtree.delete(X, !Set),
    set_bbbtree.delete_list(Xs, !Set).

%---------------------------------------------------------------------------%

% set_bbbtree.remove(X, empty, _):- fail.
set_bbbtree.remove(X, tree(V, N, L, R), Set) :-
    compare(Result, X, V),
    (
        % Remove X from left subtree.
        Result = (<),
        set_bbbtree.remove(X, L, NewL), % X in left tree
        NewN = N - 1,
        Set = tree(V, NewN, NewL, R)
    ;
        % Remove X from right subtree.
        Result = (>),
        set_bbbtree.remove(X, R, NewR), % X in right tree
        NewN = N - 1,
        Set = tree(V, NewN, L, NewR)
    ;
        % Found X so just concatenate its two subtrees together.
        Result = (=),
        set_bbbtree.concat3(L, R, Set)
    ).

%---------------------------------------------------------------------------%

set_bbbtree.remove_list([], !Set).
set_bbbtree.remove_list([X | Xs], !Set) :-
    set_bbbtree.remove(X, !Set),
    set_bbbtree.remove_list(Xs, !Set).

%---------------------------------------------------------------------------%

% The tree is not rebalanced as the removal of one element will not cause the
% tree to become much more unbalanced.

% set_bbbtree.remove_least(X, empty, _) :- fail.
set_bbbtree.remove_least(X, tree(V, N, L, R), Set) :-
    (
        % Found the least element.
        L = empty,
        X = V,
        Set = R
    ;
        % Search further in the left subtree.
        L = tree(_V, _N, _L, _R),
        set_bbbtree.remove_least(X, L, NewL),
        NewN = N - 1,
        Set = tree(V, NewN, NewL, R)
    ).

%---------------------------------------------------------------------------%

% The tree is not rebalanced as the removal of one element will not cause the
% tree to become much more unbalanced.

% set_bbbtree.remove_largest(X, empty, _) :- fail.
set_bbbtree.remove_largest(X, tree(V, N, L, R), Set) :-
    (
        % Found the largest element.
        R = empty,
        X = V,
        Set = L
    ;
        % Search further in the right subtree.
        R = tree(_V, _N, _L, _R),
        set_bbbtree.remove_largest(X, R, NewR),
        NewN = N - 1,
        Set = tree(V, NewN, L, NewR)
    ).

%---------------------------------------------------------------------------%

set_bbbtree.list_to_set(Xs) = S :-
    set_bbbtree.list_to_set(Xs, S).

set_bbbtree.list_to_set(List, Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.list_to_set_r(List, Set, Ratio).

:- pred set_bbbtree.list_to_set_r(list(T)::in, set_bbbtree(T)::out, int::in)
    is det.

set_bbbtree.list_to_set_r(List, Set, Ratio) :-
    set_bbbtree.init(InitSet),
    set_bbbtree.insert_list_r(InitSet, List, Set, Ratio).

set_bbbtree.from_list(List) = Set :-
    set_bbbtree.list_to_set(List, Set).

%---------------------------------------------------------------------------%

% The tree is created by first determining it's length. All lists of length
% N have the same structure. The root of the tree is the N // 2 element
% of the list. Elements 1 to N // 2 - 1 make up the left subtree and elements
% N // 2 + 1 to N make up the right subtree. The structure, which is known
% due to the length of the list, is just created inorder while passing the
% list around in an accumulator and taking off elements as needed.
%
% The predicate set_bbbtree.sorted_list_to_set_len2 has been unfolded up to
% trees of size 3 so as to avoid recursive calls all the way to the leaves.
% This alone approximately halves execution time and stack usage.
% Unfolding further becomes a case of diminishing returns.
%
% Cases N = 3, N = 2 and N = 1 could safely be removed without change to
% computed results as long as the 3 in N > 3 is adjusted appropriately.

set_bbbtree.sorted_list_to_set(Xs) = S :-
    set_bbbtree.sorted_list_to_set(Xs, S).

set_bbbtree.sorted_list_to_set(List, Set) :-
    list.length(List, N),
    set_bbbtree.sorted_list_to_set_len(List, Set, N).

set_bbbtree.sorted_list_to_set_len([], empty, _N).
set_bbbtree.sorted_list_to_set_len([X | Xs], Set, N) :-
    set_bbbtree.sorted_list_to_set_len2([X | Xs], RestOfList, N, Set),
    % The list should be exhausted on completion.
    (
        RestOfList = []
    ;
        RestOfList = [_ | _],
        % Should never happen. Here only to satisfy det checker.
        error("set_bbbtree.sorted_list_to_set_r")
    ).

:- pred set_bbbtree.sorted_list_to_set_len2(list(T)::in, list(T)::out,
    int::in, set_bbbtree(T)::out) is det.

set_bbbtree.sorted_list_to_set_len2(List, RestOfList, N, Set) :-
    ( if N > 3 then
        NL = N//2,
        NR = N - NL - 1,
        set_bbbtree.sorted_list_to_set_len2(List, RestOfList0, NL, L),
        (
            RestOfList0 = [V | RestOfList1],
            set_bbbtree.sorted_list_to_set_len2(RestOfList1, RestOfList,
                NR, R),
            Set = tree(V, N, L, R)
        ;
            % Should never occur. Here only to satisfy det checker.
            RestOfList0 = [],
            error("set_bbbtree.sorted_list_to_set_len2.1")
        )
    else if N = 3 then
        ( if List = [X, Y, Z | RestOfList0] then
            RestOfList = RestOfList0,
            Set = tree(Y, N,
                tree(X, 1, empty, empty),
                tree(Z, 1, empty, empty))
        else
            % Should never occur. Here only to satisfy det checker
            error("set_bbbtree.sorted_list_to_set_len2.2")
        )
    else if N = 2 then
        ( if List = [X, Y | RestOfList0] then
            RestOfList = RestOfList0,
            Set = tree(Y, N, tree(X, 1, empty, empty), empty)
        else
            % Should never occur. Here only to satisfy det checker
            error("set_bbbtree.sorted_list_to_set_len2.3")
        )
    else if N = 1 then
        ( if List = [X | RestOfList0] then
            RestOfList = RestOfList0,
            Set = tree(X, N, empty, empty)
        else
            % Should never occur. Here only to satisfy det checker
            error("set_bbbtree.sorted_list_to_set_len2.4")
        )
    else
            % N = 0.
        RestOfList = List,
        Set = empty
    ).

set_bbbtree.from_sorted_list(List) = Set :-
    set_bbbtree.sorted_list_to_set(List, Set).

%---------------------------------------------------------------------------%

set_bbbtree.to_sorted_list(S) = Xs :-
    set_bbbtree.to_sorted_list(S, Xs).

    % Flatten the tree by an accumulator based tree traversal
    % traversing the tree in a right-to-left post order manner. O(n).

set_bbbtree.to_sorted_list(Set, List) :-
    set_bbbtree.to_sorted_list2(Set, [], List).

:- pred set_bbbtree.to_sorted_list2(set_bbbtree(T), list(T), list(T)).
:- mode set_bbbtree.to_sorted_list2(di, di, uo) is det.
:- mode set_bbbtree.to_sorted_list2(in, in, out) is det.

set_bbbtree.to_sorted_list2(empty, List, List).

set_bbbtree.to_sorted_list2(tree(V, _N, L, R), Acc, List) :-
    set_bbbtree.to_sorted_list2(R, Acc, List0),
    set_bbbtree.to_sorted_list2(L, [V|List0], List).

%---------------------------------------------------------------------------%

% elem(x, A)  implies
%
% A union B =
%                   ( { elem(x, A) | x < a } union { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } union { elem(x, B) | x > a } )

set_bbbtree.union(SetA, SetB, Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.union_r(SetA, SetB, Set, Ratio).

:- pred set_bbbtree.union_r(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.union_r(empty, Set, Set, _Ratio).

set_bbbtree.union_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
    set_bbbtree.split_lt(R, V, NewRL, Ratio),
    set_bbbtree.split_gt(R, V, NewRR, Ratio),
    set_bbbtree.union_r(LL, NewRL, LSet, Ratio),
    set_bbbtree.union_r(LR, NewRR, RSet, Ratio),
    set_bbbtree.concat4(LSet, RSet, V, Set, Ratio).

%---------------------------------------------------------------------------%

set_bbbtree.union(S1, S2) = S3 :-
    set_bbbtree.union(S1, S2, S3).

set_bbbtree.union_list(ListofSets) =
    list.foldl(set_bbbtree.union, ListofSets, set_bbbtree.init).

%---------------------------------------------------------------------------%

% `set_bbbtree.power_union' is a divide and conquer algorithm. The power union
% of the two subtrees is determined and then unioned. Then the root set is
% unioned into the result. The choice to perform the power union of the two
% subtrees is due to the computational expense of union being proportional
% to the difference in height of the two subtrees. This way the two power
% union calls will likely create trees of similar sizes, and hence their
% union will be inexpensive. Unfortunately as the union grows it will
% increasing dwarf the tree that is the root node and hence this cost will
% increase, but this cannot be avoided.

set_bbbtree.power_union(SS) = S :-
    set_bbbtree.power_union(SS, S).

set_bbbtree.power_union(Sets, Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.power_union_r(Sets, Set, Ratio).

:- pred set_bbbtree.power_union_r(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.power_union_r(empty, empty, _Ratio).
set_bbbtree.power_union_r(tree(V, _N, L, R), Set, Ratio) :-
    set_bbbtree.power_union_r(L, LUnion, Ratio),
    set_bbbtree.power_union_r(R, RUnion, Ratio),
    set_bbbtree.union_r(LUnion, RUnion, Union, Ratio),
    set_bbbtree.union_r(V, Union, Set, Ratio).

%---------------------------------------------------------------------------%

% elem(x, A) and elem(x, B) implies
%
% A intersection B =
%                   ( { elem(x, A) | x < a } intersect { elem(x, B) | x < a } )
%             union ( { a } )
%             union ( { elem(x, A) | x > a } intersect { elem(x, B) | x > a } )

set_bbbtree.intersect(S1, S2) = S3 :-
    set_bbbtree.intersect(S1, S2, S3).

set_bbbtree.intersect(SetA, SetB, Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.intersect_r(SetA, SetB, Set, Ratio).

:- pred set_bbbtree.intersect_r(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.intersect_r(empty, _Set, empty, _Ratio).
set_bbbtree.intersect_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
    set_bbbtree.split_lt(R, V, NewRL, Ratio),
    set_bbbtree.split_gt(R, V, NewRR, Ratio),
    set_bbbtree.intersect_r(LL, NewRL, LSet, Ratio),
    set_bbbtree.intersect_r(LR, NewRR, RSet, Ratio),
    ( if set_bbbtree.member(V, R) then
        set_bbbtree.concat4(LSet, RSet, V, Set, Ratio)
    else
        set_bbbtree.concat3(LSet, RSet, Set)
    ).

%---------------------------------------------------------------------------%

% `set_bbbtree.power_intersect' is an accumulator based algorithm. Initially
% the accumulator is seeded with the tree at the root. Then the tree is
% traversed inorder and each tree at each node is respectively intersected with
% the accumulator. The aim of the algorithm is to rapidly reduce the size of
% the accumulator, possibly of the empty set in which case the call immediately
% returns.

set_bbbtree.power_intersect(SS) = S :-
    set_bbbtree.power_intersect(SS, S).

set_bbbtree.power_intersect(Sets, Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.power_intersect_r(Sets, Set, Ratio).

:- pred set_bbbtree.power_intersect_r(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.power_intersect_r(empty, empty, _Ratio).
set_bbbtree.power_intersect_r(tree(V, _N, L, R), Set, Ratio) :-
    set_bbbtree.power_intersect_r2(L, V, Intersection0, Ratio),
    set_bbbtree.power_intersect_r2(R, Intersection0, Set, Ratio).

:- pred set_bbbtree.power_intersect_r2(set_bbbtree(set_bbbtree(T))::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out, int::in) is det.

set_bbbtree.power_intersect_r2(empty, empty, empty, _Ratio).
set_bbbtree.power_intersect_r2(empty, tree(_V, _N, _L, _R), empty, _Ratio).
set_bbbtree.power_intersect_r2(tree(_V, _N, _L, _R), empty, empty, _Ratio).
set_bbbtree.power_intersect_r2(tree(V, _N, L, R), tree(AccV, AccN, AccL, AccR),
        Set, Ratio) :-
    set_bbbtree.intersect_r(V, tree(AccV, AccN, AccL, AccR),
        Intersection0, Ratio),
    set_bbbtree.power_intersect_r2(L, Intersection0,
        Intersection1, Ratio),
    set_bbbtree.power_intersect_r2(R, Intersection1, Set, Ratio).

set_bbbtree.intersect_list([]) = set_bbbtree.init.
set_bbbtree.intersect_list([Set | Sets]) =
        set_bbbtree.intersect_list_r(Set, Sets, Ratio) :-
    set_bbbtree.def_ratio(Ratio).

:- func set_bbbtree.intersect_list_r(set_bbbtree(T), list(set_bbbtree(T)),
    int) = set_bbbtree(T).

set_bbbtree.intersect_list_r(Intersect, [], _Ratio) = Intersect.
set_bbbtree.intersect_list_r(Intersect0, [Set | Sets], Ratio) =
        set_bbbtree.intersect_list_r(Intersect1, Sets, Ratio) :-
    set_bbbtree.intersect_r(Intersect0, Set, Intersect1, Ratio).

%---------------------------------------------------------------------------%

% elem(x, A) and not elem(x, B) implies
%
% A difference B =
%               ( { elem(x, A) | x < a } difference { elem(x, B) | x < a } )
%         union ( { a } )
%         union ( { elem(x, A) | x > a } difference { elem(x, B) | x > a } )

set_bbbtree.difference(S1, S2) = S3 :-
    set_bbbtree.difference(S1, S2, S3).

set_bbbtree.difference(SetA, SetB, Set) :-
    set_bbbtree.def_ratio(Ratio),
    set_bbbtree.difference_r(SetA, SetB, Set, Ratio).

:- pred set_bbbtree.difference_r(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.difference_r(empty, _Set, empty, _Ratio).
set_bbbtree.difference_r(tree(V, _N, LL, LR), R, Set, Ratio) :-
    set_bbbtree.split_lt(R, V, NewRL, Ratio),
    set_bbbtree.split_gt(R, V, NewRR, Ratio),
    set_bbbtree.difference_r(LL, NewRL, LSet, Ratio),
    set_bbbtree.difference_r(LR, NewRR, RSet, Ratio),
    ( if set_bbbtree.member(V, R) then
        set_bbbtree.concat3(LSet, RSet, Set)
    else
        set_bbbtree.concat4(LSet, RSet, V, Set, Ratio)
    ).

%---------------------------------------------------------------------------%

set_bbbtree.subset(SetA, SetB) :-
    set_bbbtree.difference(SetA, SetB, Set),
    set_bbbtree.empty(Set).

%---------------------------------------------------------------------------%

set_bbbtree.superset(SetA, SetB) :-
    set_bbbtree.subset(SetB, SetA).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Given X, L and R create a new tree who's root is X,
    % left subtree is L and right subtree is R.
    %
:- pred set_bbbtree.build_node(T::in, set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

set_bbbtree.build_node(X, L, R, Tree) :-
    set_bbbtree.count(L, LSize),
    set_bbbtree.count(R, RSize),
    N = 1 + LSize + RSize,
    Tree0 = tree(X, N, L, R),
    unsafe_promise_unique(Tree0, Tree).

%---------------------------------------------------------------------------%

    % Single rotation to the left.
    %
    %     A                        B
    %   /   \                    /   \
    %  X     B      ----->      A     Z
    %      /   \              /   \
    %     Y     Z            X     Y
    %
:- pred set_bbbtree.single_rot_l(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

set_bbbtree.single_rot_l(_A, _X, empty, _Set) :-
    error("set_bbbtree.single_rot_l").
set_bbbtree.single_rot_l(A, X, tree(B, _N, Y, Z), Set) :-
    set_bbbtree.build_node(A, X, Y, A_X_and_Y),
    set_bbbtree.build_node(B, A_X_and_Y, Z, Set).

%---------------------------------------------------------------------------%

    % Single rotation to the right.
    %
:- pred set_bbbtree.single_rot_r(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

set_bbbtree.single_rot_r(_B, empty, _Z, _Set) :-
    error("set_bbbtree.single_rot_r").
set_bbbtree.single_rot_r(B, tree(A, _N, X, Y), Z, Set) :-
    set_bbbtree.build_node(B, Y, Z, B_Y_and_Z),
    set_bbbtree.build_node(A, X, B_Y_and_Z, Set).

%---------------------------------------------------------------------------%

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
:- pred set_bbbtree.double_rot_l(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

set_bbbtree.double_rot_l(_A, _X, empty, _Set) :-
    error("set_bbbtree.double_rot_l.1").
set_bbbtree.double_rot_l(A, X, tree(C, _N0, Y, Z), Set) :-
    (
        Y = tree(B, _N1, Y1, Y2),
        set_bbbtree.build_node(A, X, Y1, A_X_and_Y1),
        set_bbbtree.build_node(C, Y2, Z, C_Y2_and_Z),
        set_bbbtree.build_node(B, A_X_and_Y1, C_Y2_and_Z, Set)
    ;
        Y = empty,
        error("set_bbbtree.double_rot_l.2")
    ).

%---------------------------------------------------------------------------%

    % Double rotation to the right.
    %
:- pred set_bbbtree.double_rot_r(T::in, set_bbbtree(T)::in,
    set_bbbtree(T)::in, set_bbbtree(T)::out) is det.

set_bbbtree.double_rot_r(_B, empty, _Z, _Set) :-
    error("set_bbbtree.double_rot_r.1").
set_bbbtree.double_rot_r(C, tree(A, _N0, X, Y), Z, Set) :-
    (
        Y = tree(B, _N1, Y1, Y2),
        set_bbbtree.build_node(A, X, Y1, A_X_and_Y1),
        set_bbbtree.build_node(C, Y2, Z, C_Y2_and_Z),
        set_bbbtree.build_node(B, A_X_and_Y1, C_Y2_and_Z, Set)
    ;
        Y = empty,
        error("set_bbbtree.double_rot_r.2")
    ).

%---------------------------------------------------------------------------%

    % Given two trees L and R, such that all the elements of L are less than
    % those of R, and an element V that lies between the values of L and the
    % values of R construct a new tree consisting of V, L and R that is
    % balanced by the use of single and double left and right rotations.
    %
:- pred set_bbbtree.balance(T::in, set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.balance(V, L, R, Set, Ratio) :-
    set_bbbtree.count(L, LSize),
    set_bbbtree.count(R, RSize),
    ( if
        Val = LSize + RSize,
        Val < 2
    then
        % the two trees are too small to bother rebalancing.
        set_bbbtree.build_node(V, L, R, Set)
    else if
        Val = Ratio * LSize,
        RSize > Val
    then
        (
            R = tree(_V0, _N0, RL, RR),
            set_bbbtree.count(RL, RLSize),  % Right side too big.
            set_bbbtree.count(RR, RRSize),
            ( if RLSize < RRSize then
                set_bbbtree.single_rot_l(V, L, R, Set)
            else
                set_bbbtree.double_rot_l(V, L, R, Set)
            )
        ;
            R = empty,
            error("set_bbbtree.balance.1")
        )
    else if
        Val = Ratio * RSize,
        LSize > Val
    then
        (
            L = tree(_V1, _N1, LL, LR),
            set_bbbtree.count(LL, LLSize),  % Left side too big.
            set_bbbtree.count(LR, LRSize),
            ( if LRSize < LLSize then
                set_bbbtree.single_rot_r(V, L, R, Set)
            else
                set_bbbtree.double_rot_r(V, L, R, Set)
            )
        ;
            L = empty,
            error("set_bbbtree.balance.2")
        )
    else
        set_bbbtree.build_node(V, L, R, Set)   % Already balanced
    ).

%---------------------------------------------------------------------------%

    % Given two trees concatenate them by removing the greatest element
    % from the left subtree if it is larger than the right subtree by a
    % factor of ratio, else the smallest element from the right subtree,
    % and make it the root along with the two remaining trees as its subtrees.
    % No rebalancing is performed on the resultant tree. `set_bbbtree.concat3'
    % is largely used for deletion of elements. This predicate should not be
    % confused with the predicate `concat3' in the paper for that is
    % `set_bbbtree.concat4'.
    %
:- pred set_bbbtree.concat3(set_bbbtree(T)::in, set_bbbtree(T)::in,
    set_bbbtree(T)::out) is det.

set_bbbtree.concat3(L, R, Set) :-
    set_bbbtree.count(L, LSize),
    ( if LSize = 0 then
        % Left tree empty so just return the right tree.
        Set = R
    else
        set_bbbtree.count(R, RSize),
        ( if RSize = 0 then
            % Right tree empty so just return the left tree.
            Set = L
        else
            % If the left tree is the larger of the two then
            % remove its largest value and make it the root
            % of the new left and the right trees.
            % Otherwise remove the smallest value from the
            % right tree and make it the root of the left and
            % the new right trees.
            ( if LSize > RSize then
                ( if set_bbbtree.remove_largest(X, L, NewL) then
                    set_bbbtree.build_node(X, NewL, R, Set)
                else
                    error("set_bbbtree.concat3.1")
                )
            else
                ( if set_bbbtree.remove_least(X, R, NewR) then
                    set_bbbtree.build_node(X, L, NewR, Set)
                else
                    error("set_bbbtree.concat3.2")
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

    % Given two trees L and R, such that all the elements of L are less than
    % those of R, and an element V that lies between the values of L and the
    % values of R construct a new tree consisting of V and the elements of
    % L and R. This predicate is the equivalent of concat3 in the paper.
    %
:- pred set_bbbtree.concat4(set_bbbtree(T)::in, set_bbbtree(T)::in, T::in,
    set_bbbtree(T)::out, int::in) is det.

set_bbbtree.concat4(empty, R, V, Set, Ratio) :-
    set_bbbtree.insert_r(R, V, Set, Ratio).

set_bbbtree.concat4(tree(LV, LN, LL, LR), R, V, Set, Ratio) :-
    (
        R = empty,
        set_bbbtree.insert_r(tree(LV, LN, LL, LR), V, Set, Ratio)
    ;
        R = tree(RV, RN, RL, RR),
        ( if
            Val = Ratio * LN,   % Right too big
            Val < RN
        then
            set_bbbtree.concat4(tree(LV, LN, LL, LR), RL, V, NewL, Ratio),
            set_bbbtree.balance(RV, NewL, RR, Set, Ratio)
        else if
            Val = Ratio * RN,   % Left too big
            Val < LN
        then
            set_bbbtree.concat4(LR, tree(RV, RN, RL, RR), V, NewR, Ratio),
            set_bbbtree.balance(LV, LL, NewR, Set, Ratio)
        else
            set_bbbtree.build_node(V, tree(LV, LN, LL, LR), R, Set)
        )
    ).

%---------------------------------------------------------------------------%

% Given a set return the subset that is less that X

:- pred set_bbbtree.split_lt(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out,
    int::in) is det.

set_bbbtree.split_lt(empty, _X, empty, _Ratio).
set_bbbtree.split_lt(tree(V, _N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        Result = (<),
        set_bbbtree.split_lt(L, X, Set, Ratio)
    ;
        Result = (>),
        set_bbbtree.split_lt(R, X, Set0, Ratio),
        set_bbbtree.concat4(L, Set0, V, Set, Ratio)
    ;
        Result = (=),
        Set = L
    ).

%---------------------------------------------------------------------------%

    % Given a set return the subset of it that is greater than X.
    %
:- pred set_bbbtree.split_gt(set_bbbtree(T)::in, T::in, set_bbbtree(T)::out,
    int::in) is det.

set_bbbtree.split_gt(empty, _X, empty, _Ratio).
set_bbbtree.split_gt(tree(V, _N, L, R), X, Set, Ratio) :-
    compare(Result, X, V),
    (
        Result = (>),
        set_bbbtree.split_gt(R, X, Set, Ratio)
    ;
        Result = (<),
        set_bbbtree.split_gt(L, X, Set0, Ratio),
        set_bbbtree.concat4(Set0, R, V, Set, Ratio)
    ;
        Result = (=),
        Set = R
    ).

%---------------------------------------------------------------------------%

% XXX we should just traverse the tree directly instead of converting
%     to a sorted list first.

set_bbbtree.fold(F, S, A) = B :-
    B = list.foldl(F, set_bbbtree.to_sorted_list(S), A).

set_bbbtree.fold(P, S, !A) :-
    list.foldl(P, set_bbbtree.to_sorted_list(S), !A).

set_bbbtree.fold2(P, S, !A, !B) :-
    list.foldl2(P, set_bbbtree.to_sorted_list(S), !A, !B).

set_bbbtree.fold3(P, S, !A, !B, !C) :-
    list.foldl3(P, set_bbbtree.to_sorted_list(S), !A, !B, !C).

set_bbbtree.fold4(P, S, !A, !B, !C, !D) :-
    list.foldl4(P, set_bbbtree.to_sorted_list(S), !A, !B, !C, !D).

set_bbbtree.fold5(P, S, !A, !B, !C, !D, !E) :-
    list.foldl5(P, set_bbbtree.to_sorted_list(S), !A, !B, !C, !D, !E).

set_bbbtree.fold6(P, S, !A, !B, !C, !D, !E, !F) :-
    list.foldl6(P, set_bbbtree.to_sorted_list(S), !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%

set_bbbtree.all_true(P, S) :-
    set_bbbtree.to_sorted_list(S, L),
    list.all_true(P, L).

%---------------------------------------------------------------------------%

set_bbbtree.map(F, S1) = S2 :-
    set_bbbtree.to_sorted_list(S1, L1),
    L2 = list.map(F, L1),
    set_bbbtree.list_to_set(L2, S2).

set_bbbtree.filter_map(PF, S1) = S2 :-
    set_bbbtree.to_sorted_list(S1, L1),
    L2 = list.filter_map(PF, L1),
    set_bbbtree.list_to_set(L2, S2).

%---------------------------------------------------------------------------%

set_bbbtree.filter(P, Set, TrueSet) :-
    set_bbbtree.to_sorted_list(Set, List),
    list.filter(P, List, TrueList),
    set_bbbtree.list_to_set(TrueList, TrueSet).

set_bbbtree.filter(P, Set, TrueSet, FalseSet) :-
    set_bbbtree.to_sorted_list(Set, List),
    list.filter(P, List, TrueList, FalseList),
    set_bbbtree.list_to_set(TrueList, TrueSet),
    set_bbbtree.list_to_set(FalseList, FalseSet).

%---------------------------------------------------------------------------%
:- end_module set_bbbtree.
%---------------------------------------------------------------------------%
