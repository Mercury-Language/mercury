%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997,1999-2002, 2004-2006 The University of Melbourne.
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

    % `set_unordlist.list_to_set(List, Set)' is true iff `Set' is the set
    % containing only the members of `List'.
    %
:- pred set_unordlist.list_to_set(list(T)::in, set_unordlist(T)::out) is det.
:- func set_unordlist.list_to_set(list(T)) = set_unordlist(T).

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

    % `set_unordlist.init(Set)' is true iff `Set' is an empty set.
    %
:- pred set_unordlist.init(set_unordlist(_T)::uo) is det.
:- func set_unordlist.init = set_unordlist(T).

    % `set_unordlist.singleton_set(Set, Elem)' is true iff `Set' is the set
    % containing just the single element `Elem'.
    %
:- pred set_unordlist.singleton_set(set_unordlist(T), T).
:- mode set_unordlist.singleton_set(in, out) is semidet.
:- mode set_unordlist.singleton_set(out, in) is det.

:- func set_unordlist.make_singleton_set(T) = set_unordlist(T).

    % `set_unordlist.equal(SetA, SetB)' is true iff
    % `SetA' and `SetB' contain the same elements.
    %
:- pred set_unordlist.equal(set_unordlist(T)::in, set_unordlist(T)::in)
    is semidet.

    % `set_unordlist.empty(Set)' is true iff `Set' is an empty set.
    %
:- pred set_unordlist.empty(set_unordlist(_T)::in) is semidet.

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

    % `set_unordlist.insert(Set0, X, Set)' is true iff `Set' is the union
    % of `Set0' and the set containing only `X'.
    %
:- pred set_unordlist.insert(set_unordlist(T), T, set_unordlist(T)).
:- mode set_unordlist.insert(di, di, uo) is det.
:- mode set_unordlist.insert(in, in, out) is det.

:- func set_unordlist.insert(set_unordlist(T), T) = set_unordlist(T).

    % `set_unordlist.insert_list(Set0, Xs, Set)' is true iff `Set' is the
    % union of `Set0' and the set containing only the members of `Xs'.
    %
:- pred set_unordlist.insert_list(set_unordlist(T)::in, list(T)::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.insert_list(set_unordlist(T), list(T))
    = set_unordlist(T).

    % `set_unordlist.delete(Set0, X, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred set_unordlist.delete(set_unordlist(T), T, set_unordlist(T)).
:- mode set_unordlist.delete(di, in, uo) is det.
:- mode set_unordlist.delete(in, in, out) is det.

:- func set_unordlist.delete(set_unordlist(T), T) = set_unordlist(T).

    % `set_unordlist.delete_list(Set0, Xs, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only the members
    % of `Xs'.
    %
:- pred set_unordlist.delete_list(set_unordlist(T)::in, list(T)::in,
    set_unordlist(T)::out) is det.

:- func set_unordlist.delete_list(set_unordlist(T), list(T))
    = set_unordlist(T).

    % `set_unordlist.remove(Set0, X, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
:- pred set_unordlist.remove(set_unordlist(T)::in, T::in,
    set_unordlist(T)::out) is semidet.

    % `set_unordlist.remove_list(Set0, Xs, Set)' is true iff Xs does not
    % contain any duplicates, `Set0' contains every member of `Xs',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only the members of `Xs'.
    %
:- pred set_unordlist.remove_list(set_unordlist(T)::in, list(T)::in,
    set_unordlist(T)::out) is semidet.

    % `set_unordlist.remove_least(Set0, X, Set)' is true iff `X' is the
    % least element in `Set0', and `Set' is the set which contains all the
    % elements of `Set0' except `X'.
    %
:- pred set_unordlist.remove_least(set_unordlist(T)::in, T::out,
    set_unordlist(T)::out) is semidet.

    % `set_unordlist_union(SetA, SetB, Set)' is true iff `Set' is the union
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

:- func set_unordlist.map(func(T1) = T2, set_unordlist(T1))
    = set_unordlist(T2).

:- func set_unordlist.filter_map(func(T1) = T2, set_unordlist(T1))
    = set_unordlist(T2).
:- mode set_unordlist.filter_map(func(in) = out is semidet, in) = out is det.

:- func set_unordlist.fold(func(T1, T2) = T2, set_unordlist(T1), T2) = T2.

    % set_unordlist.divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    %
:- pred set_unordlist.divide(pred(T1), set_unordlist(T1), set_unordlist(T1),
    set_unordlist(T1)).
:- mode set_unordlist.divide(pred(in) is semidet, in, out, out) is det.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

%--------------------------------------------------------------------------%

:- type set_unordlist(T) ==   list(T).

set_unordlist.list_to_set(List, List).

set_unordlist.from_list(List) = List.

set_unordlist.sorted_list_to_set(List, List).

set_unordlist.from_sorted_list(List) = List.

set_unordlist.to_sorted_list(Set, List) :-
    list.sort_and_remove_dups(Set, List).

set_unordlist.insert_list(Set0, List, Set) :-
    list.append(List, Set0, Set).

set_unordlist.insert(S0, E, [E | S0]).

set_unordlist.init([]).

set_unordlist.singleton_set([X], X).

set_unordlist.equal(SetA, SetB) :-
    set_unordlist.subset(SetA, SetB),
    set_unordlist.subset(SetB, SetA).

set_unordlist.empty([]).

set_unordlist.subset([], _).
set_unordlist.subset([E | S0], S1) :-
    set_unordlist.member(E, S1),
    set_unordlist.subset(S0, S1).

set_unordlist.superset(S0, S1) :-
    set_unordlist.subset(S1, S0).

set_unordlist.member(E, S) :-
    list.member(E, S).

set_unordlist.is_member(E, S, R) :-
    ( set_unordlist.member(E, S) ->
        R = yes
    ;
        R = no
    ).

set_unordlist.contains(S, E) :-
    set_unordlist.member(E, S).

set_unordlist.delete_list(S, [], S).
set_unordlist.delete_list(S0, [X | Xs], S) :-
    set_unordlist.delete(S0, X, S1),
    set_unordlist.delete_list(S1, Xs, S).

set_unordlist.delete(S0, E, S) :-
    list.delete_all(S0, E, S).

set_unordlist.remove_list(S, [], S).
set_unordlist.remove_list(S0, [X | Xs], S) :-
    set_unordlist.remove(S0, X, S1),
    set_unordlist.remove_list(S1, Xs, S).

set_unordlist.remove(S0, E, S) :-
    list.member(E, S0),
    set_unordlist.delete(S0, E, S).

set_unordlist.remove_least(Set0, E, Set) :-
    Set0 = [_ | _],   % fail early on an empty set
    set_unordlist.to_sorted_list(Set0, [E | Set]).

set_unordlist.union(Set0, Set1, Set) :-
    list.append(Set1, Set0, Set).

set_unordlist.union_list(LS) = S :-
    set_unordlist.power_union(LS, S).

set_unordlist.power_union(PS, S) :-
    set_unordlist.init(S0),
    set_unordlist.power_union_2(PS, S0, S1),
    list.sort_and_remove_dups(S1, S).

:- pred set_unordlist.power_union_2(list(set_unordlist(T))::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

set_unordlist.power_union_2([], !S).
set_unordlist.power_union_2([T | Ts], !S) :-
    set_unordlist.union(!.S, T, !:S),
    set_unordlist.power_union_2(Ts, !S).

set_unordlist.intersect(S0, S1, S) :-
    set_unordlist.intersect_2(S0, S1, [], S).

:- pred set_unordlist.intersect_2(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

set_unordlist.intersect_2([], _, S, S).
set_unordlist.intersect_2([E | S0], S1, S2, S) :-
    ( list.member(E, S1) ->
        S3 = [E | S2]
    ;
        S3 = S2
    ),
    set_unordlist.intersect_2(S0, S1, S3, S).

set_unordlist.power_intersect([], []).
set_unordlist.power_intersect([S0 | Ss], S) :-
    (
        Ss = [],
        S = S0
    ;
        Ss = [_ | _],
        set_unordlist.power_intersect(Ss, S1),
        set_unordlist.intersect(S1, S0, S)
    ).

set_unordlist.intersect_list(Sets) =
    set_unordlist.power_intersect(Sets).

%--------------------------------------------------------------------------%

set_unordlist.difference(A, B, C) :-
    set_unordlist.difference_2(B, A, C).

:- pred set_unordlist.difference_2(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

set_unordlist.difference_2([], C, C).
set_unordlist.difference_2([E | Es], A, C) :-
    set_unordlist.delete(A, E, B),
    set_unordlist.difference_2(Es, B, C).

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

set_unordlist.insert(S1, T) = S2 :-
    set_unordlist.insert(S1, T, S2).

set_unordlist.insert_list(S1, Xs) = S2 :-
    set_unordlist.insert_list(S1, Xs, S2).

set_unordlist.delete(S1, T) = S2 :-
    set_unordlist.delete(S1, T, S2).

set_unordlist.delete_list(S1, Xs) = S2 :-
    set_unordlist.delete_list(S1, Xs, S2).

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

set_unordlist.fold(F, S, A) = B :-
    B = list.foldl(F, set_unordlist.to_sorted_list(S), A).

set_unordlist.divide(Pred, Set, RevTruePart, RevFalsePart) :-
    set_unordlist.divide_2(Pred, Set, [], RevTruePart, [], RevFalsePart).

:- pred set_unordlist.divide_2(pred(T1)::in(pred(in) is semidet),
    set_unordlist(T1)::in,
    set_unordlist(T1)::in, set_unordlist(T1)::out,
    set_unordlist(T1)::in, set_unordlist(T1)::out) is det.

set_unordlist.divide_2(_Pred, [], !RevTrue, !RevFalse).
set_unordlist.divide_2(Pred, [H | T], !RevTrue, !RevFalse) :-
    ( Pred(H) ->
        !:RevTrue = [H | !.RevTrue]
    ;
        !:RevFalse = [H | !.RevFalse]
    ),
    set_unordlist.divide_2(Pred, T, !RevTrue, !RevFalse).
