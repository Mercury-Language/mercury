%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997,1999-2002, 2004-2006, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: set_unordlist.m.
% Main authors: conway, fjh.
% Stability: medium.
%
% This file contains a `set' ADT.
% Sets are implemented here as unsorted lists, which may contain duplicates.
%
%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- module set_unordlist.
:- interface.

:- import_module bool.
:- import_module list.

%--------------------------------------------------------------------------%

:- type set_unordlist(_T).

    % `set_unordlist.init(Set)' is true iff `Set' is an empty set.
    %
:- func set_unordlist.init = set_unordlist(T).
:- pred set_unordlist.init(set_unordlist(_T)::uo) is det.

    % `set_unordlist.list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'.
    %
:- func set_unordlist.list_to_set(list(T)) = set_unordlist(T).
:- pred set_unordlist.list_to_set(list(T)::in, set_unordlist(T)::out) is det.

    % A synonym for set_unordlist.list_to_set/1.
    %
:- func set_unordlist.from_list(list(T)) = set_unordlist(T).

    % `set_unordlist.sorted_list_to_set(List, Set)' is true iff `Set' is
    % the set containing only the members of `List'.  `List' must be sorted.
    %
:- pred set_unordlist.sorted_list_to_set(list(T)::in, set_unordlist(T)::out)
    is det.
:- func set_unordlist.sorted_list_to_set(list(T)) = set_unordlist(T).

    % A synonym for set_unordlist.sorted_list_to_set/1.
    %
:- func set_unordlist.from_sorted_list(list(T)) = set_unordlist(T).

    % `set_unordlist.to_sorted_list(Set, List)' is true iff `List' is the
    % list of all the members of `Set', in sorted order.
    %
:- pred set_unordlist.to_sorted_list(set_unordlist(T)::in, list(T)::out)
    is det.
:- func set_unordlist.to_sorted_list(set_unordlist(T)) = list(T).

    % `set_unordlist.singleton_set(Set, Elem)' is true iff `Set' is the set
    % containing just the single element `Elem'.
    %
:- pred set_unordlist.singleton_set(set_unordlist(T), T).
:- mode set_unordlist.singleton_set(in, out) is semidet.
:- mode set_unordlist.singleton_set(in, in) is semidet.
:- mode set_unordlist.singleton_set(out, in) is det.

:- func set_unordlist.make_singleton_set(T) = set_unordlist(T).

:- pred set_unordlist.is_singleton(set_unordlist(T)::in, T::out) is semidet.

    % `set_unordlist.equal(SetA, SetB)' is true iff
    % `SetA' and `SetB' contain the same elements.
    %
:- pred set_unordlist.equal(set_unordlist(T)::in, set_unordlist(T)::in)
    is semidet.

    % `set_unordlist.empty(Set)' is true iff `Set' is an empty set.
    %
:- pred set_unordlist.empty(set_unordlist(_T)::in) is semidet.

:- pred set_unordlist.non_empty(set_unordlist(_T)::in) is semidet.

:- pred set_unordlist.is_empty(set_unordlist(_T)::in) is semidet.

    % `set_unordlist.subset(SetA, SetB)' is true iff `SetA' is a subset of
    % `SetB'.
    %
:- pred set_unordlist.subset(set_unordlist(T)::in, set_unordlist(T)::in)
    is semidet.

    % `set_unordlist.superset(SetA, SetB)' is true iff `SetA' is a
    % superset of `SetB'.
    %
:- pred set_unordlist.superset(set_unordlist(T)::in, set_unordlist(T)::in)
    is semidet.

    % `set_unordlist.member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred set_unordlist.member(T, set_unordlist(T)).
:- mode set_unordlist.member(in, in) is semidet.
:- mode set_unordlist.member(out, in) is nondet.

    % `set_unordlist.is_member(X, Set, Result)' returns
    % `Result = yes' iff `X' is a member of `Set'.
    %
:- pred set_unordlist.is_member(T::in, set_unordlist(T)::in, bool::out)
    is det.

    % `set_unordlist.contains(Set, X)' is true iff
    % `X' is a member of `Set'.
    %
:- pred set_unordlist.contains(set_unordlist(T)::in, T::in) is semidet.

    % `set_unordlist.insert(X, Set0, Set)' is true iff `Set' is the union
    % of `Set0' and the set containing only `X'.
    %
:- pred set_unordlist.insert(T, set_unordlist(T), set_unordlist(T)).
:- mode set_unordlist.insert(di, di, uo) is det.
:- mode set_unordlist.insert(in, in, out) is det.

:- func set_unordlist.insert(set_unordlist(T), T) = set_unordlist(T).

    % `set_unordlist.insert_list(Xs, Set0, Set)' is true iff `Set' is the
    % union of `Set0' and the set containing only the members of `Xs'.
    %
:- pred set_unordlist.insert_list(list(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

:- func set_unordlist.insert_list(set_unordlist(T), list(T))
    = set_unordlist(T).

    % `set_unordlist.delete(X, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred set_unordlist.delete(T, set_unordlist(T), set_unordlist(T)).
:- mode set_unordlist.delete(in, di, uo) is det.
:- mode set_unordlist.delete(in, in, out) is det.

:- func set_unordlist.delete(set_unordlist(T), T) = set_unordlist(T).

    % `set_unordlist.delete_list(Xs, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only the members
    % of `Xs'.
    %
:- pred set_unordlist.delete_list(list(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

:- func set_unordlist.delete_list(set_unordlist(T), list(T))
    = set_unordlist(T).

    % `set_unordlist.remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
:- pred set_unordlist.remove(T::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is semidet.

    % `set_unordlist.remove_list(Xs, Set0, Set)' is true iff Xs does not
    % contain any duplicates, `Set0' contains every member of `Xs',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only the members of `Xs'.
    %
:- pred set_unordlist.remove_list(list(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is semidet.

    % `set_unordlist.remove_least(X, Set0, Set)' is true iff `X' is the
    % least element in `Set0', and `Set' is the set which contains all the
    % elements of `Set0' except `X'.
    %
:- pred set_unordlist.remove_least(T::out,
    set_unordlist(T)::in, set_unordlist(T)::out) is semidet.

    % `set_unordlist.union(SetA, SetB, Set)' is true iff `Set' is the union
    % of `SetA' and `SetB'.  If the sets are known to be of different
    % sizes, then for efficiency make `SetA' the larger of the two.
    %
:- pred set_unordlist.union(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.union(set_unordlist(T), set_unordlist(T))
    = set_unordlist(T).

    % `set_unordlist.union_list(A) = B' is true iff `B' is the union of
    % all the sets in `A'
    %
:- func set_unordlist.union_list(list(set_unordlist(T))) = set_unordlist(T).

    % `set_unordlist.power_union(A, B)' is true iff `B' is the union of
    % all the sets in `A'
    %
:- pred set_unordlist.power_union(set_unordlist(set_unordlist(T))::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.power_union(set_unordlist(set_unordlist(T)))
    = set_unordlist(T).

    % `set_unordlist.intersect(SetA, SetB, Set)' is true iff `Set' is the
    % intersection of `SetA' and `SetB'.
    %
:- pred set_unordlist.intersect(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.intersect(set_unordlist(T), set_unordlist(T))
    = set_unordlist(T).

    % `set_unordlist.power_intersect(A, B)' is true iff `B' is the
    % intersection of all the sets in `A'
    %
:- pred set_unordlist.power_intersect(set_unordlist(set_unordlist(T))::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.power_intersect(set_unordlist(set_unordlist(T)))
    = set_unordlist(T).

    % `set_unordlist.intersect_list(A, B)' is true iff `B' is the
    % intersection of all the sets in `A'
    %
:- func set_unordlist.intersect_list(list(set_unordlist(T)))
    = set_unordlist(T).

    % `set_unordlist.difference(SetA, SetB, Set)' is true iff `Set' is the
    % set containing all the elements of `SetA' except those that
    % occur in `SetB'
    %
:- pred set_unordlist.difference(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.difference(set_unordlist(T), set_unordlist(T))
    = set_unordlist(T).

:- func set_unordlist.count(set_unordlist(T)) = int.
:- pred set_unordlist.count(set_unordlist(T)::in, int::out) is det.

:- func set_unordlist.map(func(T1) = T2, set_unordlist(T1))
    = set_unordlist(T2).

:- func set_unordlist.filter_map(func(T1) = T2, set_unordlist(T1))
    = set_unordlist(T2).
:- mode set_unordlist.filter_map(func(in) = out is semidet, in) = out is det.

:- func set_unordlist.fold(func(T1, T2) = T2, set_unordlist(T1), T2) = T2.
:- pred set_unordlist.fold(pred(T1, T2, T2), set_unordlist(T1), T2, T2).
:- mode set_unordlist.fold(pred(in, in, out) is det, in, in, out) is det.
:- mode set_unordlist.fold(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode set_unordlist.fold(pred(in, di, uo) is det, in, di, uo) is det.
:- mode set_unordlist.fold(pred(in, in, out) is semidet, in, in, out)
    is semidet.
:- mode set_unordlist.fold(pred(in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode set_unordlist.fold(pred(in, di, uo) is semidet, in, di, uo)
    is semidet.

:- pred set_unordlist.fold2(pred(T1, T2, T2, T3, T3), set_unordlist(T1),
    T2, T2, T3, T3).
:- mode set_unordlist.fold2(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode set_unordlist.fold2(pred(in, in, out, mdi, muo) is det, in,
    in, out, mdi, muo) is det.
:- mode set_unordlist.fold2(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode set_unordlist.fold2(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode set_unordlist.fold2(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode set_unordlist.fold2(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.

:- pred set_unordlist.fold3(pred(T1, T2, T2, T3, T3, T4, T4),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4).
:- mode set_unordlist.fold3(pred(in, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out) is det.
:- mode set_unordlist.fold3(pred(in, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, mdi, muo) is det.
:- mode set_unordlist.fold3(pred(in, in, out, in, out, di, uo) is det, in,
    in, out, in, out, di, uo) is det.
:- mode set_unordlist.fold3(pred(in, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out) is semidet.
:- mode set_unordlist.fold3(pred(in, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode set_unordlist.fold3(pred(in, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, di, uo) is semidet.

:- pred set_unordlist.fold4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5).
:- mode set_unordlist.fold4(
    pred(in, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out) is det.
:- mode set_unordlist.fold4(
    pred(in, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode set_unordlist.fold4(
    pred(in, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, di, uo) is det.
:- mode set_unordlist.fold4(
    pred(in, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out) is semidet.
:- mode set_unordlist.fold4(
    pred(in, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode set_unordlist.fold4(
    pred(in, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, di, uo) is semidet.

:- pred set_unordlist.fold5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode set_unordlist.fold5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det, in,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode set_unordlist.fold5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det, in,
    in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode set_unordlist.fold5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det, in,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode set_unordlist.fold5(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet, in,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode set_unordlist.fold5(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet, in,
    in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode set_unordlist.fold5(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet, in,
    in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred set_unordlist.fold6(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
:- mode set_unordlist.fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode set_unordlist.fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode set_unordlist.fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode set_unordlist.fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode set_unordlist.fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode set_unordlist.fold6(
    pred(in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % Return the set of items for which the predicate succeeds.
    %
:- pred set_unordlist.filter(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

    % Return the set of items for which the predicate succeeds,
    % and the set for which it fails.
    %
:- pred set_unordlist.filter(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in, set_unordlist(T)::out, set_unordlist(T)::out) is det.

    % set_unordlist.divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    % NOTE: this is the same as filter/4.
    %
:- pred set_unordlist.divide(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in, set_unordlist(T)::out, set_unordlist(T)::out) is det.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

%--------------------------------------------------------------------------%

:- type set_unordlist(T)
    --->    sul(list(T)).

set_unordlist.list_to_set(List, sul(List)).

set_unordlist.from_list(List) = sul(List).

set_unordlist.sorted_list_to_set(List, sul(List)).

set_unordlist.from_sorted_list(List) = sul(List).

set_unordlist.to_sorted_list(sul(Set), List) :-
    list.sort_and_remove_dups(Set, List).

set_unordlist.insert_list(List, sul(!.Set), sul(!:Set)) :-
    list.append(List, !Set).

set_unordlist.insert(E, sul(S0), sul([E | S0])).

set_unordlist.init(sul([])).

:- pragma promise_equivalent_clauses(set_unordlist.singleton_set/2).

set_unordlist.singleton_set(Set::in, X::out) :-
    Set = sul(Xs),
    list.sort_and_remove_dups(Xs, [X]).    

set_unordlist.singleton_set(Set::in, X::in) :-
    Set = sul(Xs),
    list.sort_and_remove_dups(Xs, [X]).

set_unordlist.singleton_set(Set::out, X::in) :-
    Set = sul([X]).

set_unordlist.is_singleton(sul(Xs), X) :-
    list.sort_and_remove_dups(Xs, [X]).

set_unordlist.equal(SetA, SetB) :-
    set_unordlist.subset(SetA, SetB),
    set_unordlist.subset(SetB, SetA).

set_unordlist.empty(sul([])).

set_unordlist.is_empty(sul([])).

set_unordlist.non_empty(sul([_ | _])).

set_unordlist.subset(sul([]), _).
set_unordlist.subset(sul([E | S0]), S1) :-
    set_unordlist.member(E, S1),
    set_unordlist.subset(sul(S0), S1).

set_unordlist.superset(S0, S1) :-
    set_unordlist.subset(S1, S0).

set_unordlist.member(E, sul(S)) :-
    list.member(E, S).

set_unordlist.is_member(E, S, R) :-
    ( set_unordlist.member(E, S) ->
        R = yes
    ;
        R = no
    ).

set_unordlist.contains(S, E) :-
    set_unordlist.member(E, S).

set_unordlist.delete_list([], !S).
set_unordlist.delete_list([X | Xs], !S) :-
    set_unordlist.delete(X, !S),
    set_unordlist.delete_list(Xs, !S).

set_unordlist.delete(E, sul(!.S), sul(!:S)) :-
    list.delete_all(!.S, E, !:S).

set_unordlist.remove_list([], !S).
set_unordlist.remove_list([X | Xs], !S) :-
    set_unordlist.remove(X, !S),
    set_unordlist.remove_list(Xs, !S).

set_unordlist.remove(E, sul(S0), sul(S)) :-
    list.member(E, S0),
    set_unordlist.delete(E, sul(S0), sul(S)).

set_unordlist.remove_least(E, Set0, sul(Set)) :-
    Set0 = sul([_ | _]),   % Fail early on an empty set.
    set_unordlist.to_sorted_list(Set0, [E | Set]).

set_unordlist.union(sul(Set0), sul(Set1), sul(Set)) :-
    list.append(Set1, Set0, Set).

set_unordlist.union_list(LS) = S :-
    set_unordlist.power_union(sul(LS), S).

set_unordlist.power_union(sul(PS), sul(S)) :-
    set_unordlist.init(S0),
    set_unordlist.power_union_2(PS, S0, sul(S1)),
    list.sort_and_remove_dups(S1, S).

:- pred set_unordlist.power_union_2(list(set_unordlist(T))::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

set_unordlist.power_union_2([], !S).
set_unordlist.power_union_2([T | Ts], !S) :-
    set_unordlist.union(!.S, T, !:S),
    set_unordlist.power_union_2(Ts, !S).

set_unordlist.intersect(sul(S0), sul(S1), sul(S)) :-
    set_unordlist.intersect_2(S0, S1, [], S).

:- pred set_unordlist.intersect_2(list(T)::in, list(T)::in,
    list(T)::in, list(T)::out) is det.

set_unordlist.intersect_2([], _, S, S).
set_unordlist.intersect_2([E | S0], S1, S2, S) :-
    ( list.member(E, S1) ->
        S3 = [E | S2]
    ;
        S3 = S2
    ),
    set_unordlist.intersect_2(S0, S1, S3, S).

set_unordlist.power_intersect(sul([]), sul([])).
set_unordlist.power_intersect(sul([S0 | Ss]), S) :-
    (
        Ss = [],
        S = S0
    ;
        Ss = [_ | _],
        set_unordlist.power_intersect(sul(Ss), S1),
        set_unordlist.intersect(S1, S0, S)
    ).

set_unordlist.intersect_list(Sets) =
    set_unordlist.power_intersect(sul(Sets)).

%--------------------------------------------------------------------------%

set_unordlist.difference(A, B, C) :-
    set_unordlist.difference_2(B, A, C).

:- pred set_unordlist.difference_2(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

set_unordlist.difference_2(sul([]), C, C).
set_unordlist.difference_2(sul([E | Es]), A, C) :-
    set_unordlist.delete(E, A, B),
    set_unordlist.difference_2(sul(Es), B, C).

%-----------------------------------------------------------------------------%

set_unordlist.count(Set) = Count :-
    set_unordlist.count(Set, Count).

set_unordlist.count(sul(Set), Count) :-
    list.remove_dups(Set, Elems),
    list.length(Elems, Count).

%-----------------------------------------------------------------------------%

set_unordlist.fold(F, S, A) = B :-
    B = list.foldl(F, set_unordlist.to_sorted_list(S), A).

set_unordlist.fold(P, S, !A) :-
    list.foldl(P, set_unordlist.to_sorted_list(S), !A).

set_unordlist.fold2(P, S, !A, !B) :-
    list.foldl2(P, set_unordlist.to_sorted_list(S), !A, !B).

set_unordlist.fold3(P, S, !A, !B, !C) :-
    list.foldl3(P, set_unordlist.to_sorted_list(S), !A, !B, !C).

set_unordlist.fold4(P, S, !A, !B, !C, !D) :-
    list.foldl4(P, set_unordlist.to_sorted_list(S), !A, !B, !C, !D).

set_unordlist.fold5(P, S, !A, !B, !C, !D, !E) :-
    list.foldl5(P, set_unordlist.to_sorted_list(S), !A, !B, !C, !D, !E).

set_unordlist.fold6(P, S, !A, !B, !C, !D, !E, !F) :-
    list.foldl6(P, set_unordlist.to_sorted_list(S), !A, !B, !C, !D, !E, !F).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

set_unordlist.list_to_set(Xs) = S :-
    set_unordlist.list_to_set(Xs, S).

set_unordlist.sorted_list_to_set(Xs) = S :-
    set_unordlist.sorted_list_to_set(Xs, S).

set_unordlist.to_sorted_list(S) = Xs :-
    set_unordlist.to_sorted_list(S, Xs).

set_unordlist.init = S :-
    set_unordlist.init(S).

set_unordlist.make_singleton_set(T) = S :-
    set_unordlist.singleton_set(S, T).

set_unordlist.insert(!.S, T) = !:S :-
    set_unordlist.insert(T, !S).

set_unordlist.insert_list(!.S, Xs) = !:S :-
    set_unordlist.insert_list(Xs, !S).

set_unordlist.delete(!.S, T) = !:S :-
    set_unordlist.delete(T, !S).

set_unordlist.delete_list(!.S, Xs) = !:S :-
    set_unordlist.delete_list(Xs, !S).

set_unordlist.union(S1, S2) = S3 :-
    set_unordlist.union(S1, S2, S3).

set_unordlist.power_union(SS) = S :-
    set_unordlist.power_union(SS, S).

set_unordlist.intersect(S1, S2) = S3 :-
    set_unordlist.intersect(S1, S2, S3).

set_unordlist.power_intersect(SS) = S :-
    set_unordlist.power_intersect(SS, S).

set_unordlist.difference(S1, S2) = S3 :-
    set_unordlist.difference(S1, S2, S3).

set_unordlist.map(F, S1) = S2 :-
    S2 = set_unordlist.list_to_set(list.map(F,
        set_unordlist.to_sorted_list(S1))).

set_unordlist.filter_map(PF, S1) = S2 :-
    S2 = set_unordlist.list_to_set(list.filter_map(PF,
        set_unordlist.to_sorted_list(S1))).

%-----------------------------------------------------------------------------%

set_unordlist.filter(Pred, Set, TrueSet) :-
    % XXX This should be more efficient.
    set_unordlist.divide(Pred, Set, TrueSet, _FalseSet).

set_unordlist.filter(Pred, Set, TrueSet, FalseSet) :-
    set_unordlist.divide(Pred, Set, TrueSet, FalseSet).

set_unordlist.divide(Pred, sul(Set), sul(RevTruePart), sul(RevFalsePart)) :-
    set_unordlist.divide_2(Pred, Set, [], RevTruePart, [], RevFalsePart).

:- pred set_unordlist.divide_2(pred(T1)::in(pred(in) is semidet),
    list(T1)::in,
    list(T1)::in, list(T1)::out,
    list(T1)::in, list(T1)::out) is det.

set_unordlist.divide_2(_Pred, [], !RevTrue, !RevFalse).
set_unordlist.divide_2(Pred, [H | T], !RevTrue, !RevFalse) :-
    ( Pred(H) ->
        !:RevTrue = [H | !.RevTrue]
    ;
        !:RevFalse = [H | !.RevFalse]
    ),
    set_unordlist.divide_2(Pred, T, !RevTrue, !RevFalse).

%-----------------------------------------------------------------------------%
:- end_module set_unordlist.
%-----------------------------------------------------------------------------%
