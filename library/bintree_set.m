%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 1999-2000, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%--------------------------------------------------------------------------%
%
% Main authors: fjh.
% Stability: medium (obsolete).
%
% This file provides an alternate implementation of the `set' ADT defined
% in module `set'. See that file for comments about the semantics of the
% predicates. This file implements sets as binary sorted trees, using module
% `bintree', and so provides different performance characteristics.
%
% bintree_set.is_member is a version of bintree_set.member with a more
% restricted mode, which is implemented much more efficiently using
% bintree.search.

:- module bintree_set.

%--------------------------------------------------------------------------%

:- interface.
:- import_module list.

:- type bintree_set(_T).

    % `bintree_set.list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'.
    %
:- pragma obsolete(bintree_set.list_to_set/2).
:- pred bintree_set.list_to_set(list(T)::in, bintree_set(T)::out) is det.
:- pragma obsolete(bintree_set.list_to_set/1).
:- func bintree_set.list_to_set(list(T)) = bintree_set(T).

    % `bintree_set.sorted_list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'. `List' must be sorted.
    %
:- pragma obsolete(bintree_set.sorted_list_to_set/2).
:- pred bintree_set.sorted_list_to_set(list(T)::in, bintree_set(T)::out)
    is det.
:- pragma obsolete(bintree_set.sorted_list_to_set/1).
:- func bintree_set.sorted_list_to_set(list(T)) = bintree_set(T).

    % `bintree_set.list_to_bintree_set(Set, List)' is true iff `List' is
    % the list of all the members of `Set', in sorted order.
    %
:- pragma obsolete(bintree_set.to_sorted_list/2).
:- pred bintree_set.to_sorted_list(bintree_set(T)::in, list(T)::out) is det.
:- pragma obsolete(bintree_set.to_sorted_list/1).
:- func bintree_set.to_sorted_list(bintree_set(T)) = list(T).

    % `bintree_set.init(Set)' is true iff `Set' is an empty set.
    %
:- pragma obsolete(bintree_set.init/1).
:- pred bintree_set.init(bintree_set(T)::uo) is det.
:- pragma obsolete(bintree_set.init/0).
:- func bintree_set.init = bintree_set(T).

:- pragma obsolete(bintree_set.singleton_set/2).
:- pred bintree_set.singleton_set(bintree_set(T)::out, T::in) is det.

    % `bintree_set.equal(SetA, SetB)' is true iff `SetA' and `SetB'
    % contain the same elements.
    %
:- pragma obsolete(bintree_set.equal/2).
:- pred bintree_set.equal(bintree_set(T)::in, bintree_set(T)::in) is semidet.

    % `bintree_set.subset(SetA, SetB)' is true iff `SetA' is a subset
    % of `SetB'.
    %
:- pragma obsolete(bintree_set.subset/2).
:- pred bintree_set.subset(bintree_set(T)::in, bintree_set(T)::in) is semidet.

    % `bintree_set.superset(SetA, SetB)' is true iff `SetA' is a superset
    % of `SetB'.
    %
:- pragma obsolete(bintree_set.superset/2).
:- pred bintree_set.superset(bintree_set(T)::in, bintree_set(T)::in)
    is semidet.

    % `bintree_set_member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pragma obsolete(bintree_set.member/2).
:- pred bintree_set.member(T, bintree_set(T)).
:- mode bintree_set.member(in, in) is semidet.
:- mode bintree_set.member(out, in) is nondet.

    % `bintree_set.is_member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pragma obsolete(bintree_set.is_member/2).
:- pred bintree_set.is_member(T::in, bintree_set(T)::in) is semidet.

    % `bintree_set.contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pragma obsolete(bintree_set.contains/2).
:- pred bintree_set.contains(bintree_set(T)::in, T::in) is semidet.

    % `bintree_set.insert(Set0, X, Set)' is true iff `Set' is the union of
    % `Set0' and the set containing only `X'.
    %
:- pragma obsolete(bintree_set.insert/3).
:- pred bintree_set.insert(bintree_set(T), T, bintree_set(T)).
:- mode bintree_set.insert(di, di, uo) is det.
:- mode bintree_set.insert(in, in, out) is det.

:- pragma obsolete(bintree_set.insert/2).
:- func bintree_set.insert(bintree_set(T), T) = bintree_set(T).

    % `bintree_set.insert_list(Set0, Xs, Set)' is true iff `Set' is the union
    % of `Set0' and the set containing only the members of `Xs'.
    %
:- pragma obsolete(bintree_set.insert_list/3).
:- pred bintree_set.insert_list(bintree_set(T), list(T), bintree_set(T)).
:- mode bintree_set.insert_list(di, di, uo) is det.
:- mode bintree_set.insert_list(in, in, out) is det.

:- pragma obsolete(bintree_set.insert_list/2).
:- func bintree_set.insert_list(bintree_set(T), list(T)) = bintree_set(T).

    % `bintree_set.remove(Set0, X, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set containing
    % only `X', i.e.  if `Set' is the set which contains all the elements
    % of `Set0' except `X'.
    %
:- pragma obsolete(bintree_set.remove/3).
:- pred bintree_set.remove(bintree_set(T), T, bintree_set(T)).
:- mode bintree_set.remove(in, in, out) is semidet.
% The following mode could be implemented, but hasn't been:
% :- mode bintree_set.remove(in, out, out) is nondet.

    % `bintree_set.remove_list(Set0, Xs, Set)' is true iff Xs does not contain
    % any duplicates, `Set0' contains every member of % `Xs', and `Set' is
    % the relative complement of `Set0' and the set containing only the
    % members of `Xs'.
    %
:- pragma obsolete(bintree_set.remove_list/3).
:- pred bintree_set.remove_list(bintree_set(T)::in, list(T)::in,
    bintree_set(T)::out) is semidet.

    % `bintree_set.delete(Set0, X, Set)' is true iff `Set' is the relative
    % complement of `Set0' and the set containing only `X', i.e. if `Set'
    % is the set which contains all the elements of `Set0' except `X'.
    %
:- pragma obsolete(bintree_set.delete/3).
:- pred bintree_set.delete(bintree_set(T)::in, T::in, bintree_set(T)::out)
    is det.
:- pragma obsolete(bintree_set.delete/2).
:- func bintree_set.delete(bintree_set(T), T) = bintree_set(T).

    % `bintree_set.delete_list(Set0, Xs, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only the members
    % of `Xs'.
    %
:- pragma obsolete(bintree_set.delete_list/3).
:- pred bintree_set.delete_list(bintree_set(T)::in, list(T)::in,
    bintree_set(T)::out) is det.

:- pragma obsolete(bintree_set.delete_list/2).
:- func bintree_set.delete_list(bintree_set(T), list(T)) = bintree_set(T).

    % `set_union(SetA, SetB, Set)' is true iff `Set' is the union of
    % `SetA' and `SetB'. If the sets are known to be of different sizes,
    % then for efficiency make `SetA' the larger of the two.
    %
:- pragma obsolete(bintree_set.union/3).
:- pred bintree_set.union(bintree_set(T)::in, bintree_set(T)::in,
    bintree_set(T)::out) is det.
:- pragma obsolete(bintree_set.union/2).
:- func bintree_set.union(bintree_set(T), bintree_set(T)) = bintree_set(T).

    % `set_intersect(SetA, SetB, Set)' is true iff `Set' is the
    % intersection of `SetA' and `SetB'.
    %
:- pragma obsolete(bintree_set.intersect/3).
:- pred bintree_set.intersect(bintree_set(T)::in, bintree_set(T)::in,
    bintree_set(T)::out) is det.
:- pragma obsolete(bintree_set.intersect/2).
:- func bintree_set.intersect(bintree_set(T), bintree_set(T))
    = bintree_set(T).

%--------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bintree.
:- import_module std_util.

:- type bintree_set(T)          ==      bintree(T, unit).

%--------------------------------------------------------------------------%

bintree_set.list_to_set(List, Set) :-
    list.sort_and_remove_dups(List, SortedList),
    bintree_set.sorted_list_to_set(SortedList, Set).

bintree_set.sorted_list_to_set(List, Set) :-
    assoc_unit(List, AssocList),
    bintree.from_sorted_list(AssocList, Set).

bintree_set.to_sorted_list(Set, List) :-
    bintree.keys(Set, List).

:- pred assoc_unit(list(T)::in, assoc_list(T, unit)::out) is det.

assoc_unit([], []).
assoc_unit([X | Xs], [X - unit | Ys]) :-
    assoc_unit(Xs, Ys).

%--------------------------------------------------------------------------%

bintree_set.init(Set) :-
    bintree.init(Set).

bintree_set.singleton_set(Set, Elem) :-
    bintree.init(Set0),
    bintree.set(Set0, Elem, unit, Set).

bintree_set.equal(SetA, SetB) :-
    bintree.keys(SetA, SortedElements),
    bintree.keys(SetB, SortedElements).

%--------------------------------------------------------------------------%

bintree_set.subset(S0, S1) :-
    bintree.keys(S0, SortedElements0),
    bintree_set.contains_list(SortedElements0, S1).

:- pred bintree_set.contains_list(list(T)::in, bintree_set(T)::in) is semidet.

bintree_set.contains_list([E|Es], S) :-
    bintree.search(S, E, _),
    bintree_set.contains_list(Es, S).

bintree_set.superset(S0, S1) :-
    bintree_set.subset(S1, S0).

%--------------------------------------------------------------------------%

bintree_set.member(E, S) :-
    bintree.keys(S, Elements),
    list.member(E, Elements).

bintree_set.is_member(E, S) :-
    bintree.search(S, E, _).

bintree_set.contains(S, E) :-
    bintree_set.is_member(E, S).

%--------------------------------------------------------------------------%

bintree_set.insert_list(S, [], S).
bintree_set.insert_list(S0, [E|Es], S) :-
    bintree_set.insert(S0, E, S1),
    bintree_set.insert_list(S1, Es, S).

bintree_set.insert(S0, E, S) :-
    bintree.set(S0, E, unit, S).

%--------------------------------------------------------------------------%

bintree_set.remove_list(S, [], S).
bintree_set.remove_list(S0, [X | Xs], S) :-
    bintree_set.member(X, S0),
    bintree_set.remove(S0, X, S1),
    bintree_set.remove_list(S1, Xs, S).

bintree_set.remove(S0, E, S) :-
    bintree.remove(S0, E, _, S).

%--------------------------------------------------------------------------%

bintree_set.delete_list(S, [], S).
bintree_set.delete_list(S0, [X | Xs], S) :-
    bintree_set.delete(S0, X, S1),
    bintree_set.delete_list(S1, Xs, S).

bintree_set.delete(S0, E, S) :-
    bintree.delete(S0, E, S).

%--------------------------------------------------------------------------%

bintree_set.union(S0, S1, S) :-
    bintree.to_list(S0, L0),
    bintree.to_list(S1, L1),
    list.merge(L0, L1, L),
    bintree.from_sorted_list(L, S).

bintree_set.intersect(S0, S1, S) :-
    bintree.keys(S1, L1),
    bintree_set.delete_list(S0, L1, S).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

bintree_set.init = BT :-
    bintree_set.init(BT).

bintree_set.list_to_set(Xs) = BT :-
    bintree_set.list_to_set(Xs, BT).

bintree_set.sorted_list_to_set(Xs) = BT :-
    bintree_set.sorted_list_to_set(Xs, BT).

bintree_set.to_sorted_list(BT) = Xs :-
    bintree_set.to_sorted_list(BT, Xs).

bintree_set.insert(BT1, X) = BT2 :-
    bintree_set.insert(BT1, X, BT2).

bintree_set.insert_list(BT1, Xs) = BT2 :-
    bintree_set.insert_list(BT1, Xs, BT2).

bintree_set.delete(BT1, X) = BT2 :-
    bintree_set.delete(BT1, X, BT2).

bintree_set.delete_list(BT1, Xs) = BT2 :-
    bintree_set.delete_list(BT1, Xs, BT2).

bintree_set.union(BT1, BT2) = BT3 :-
    bintree_set.union(BT1, BT2, BT3).

bintree_set.intersect(BT1, BT2) = BT3 :-
    bintree_set.intersect(BT1, BT2, BT3).
