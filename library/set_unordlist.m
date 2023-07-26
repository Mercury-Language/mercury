%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997,1999-2002, 2004-2006, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2018-2019, 2021-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: set_unordlist.m.
% Main authors: conway, fjh.
% Stability: medium.
%
% This file contains a `set' ADT.
% Sets are implemented here as unsorted lists, which may contain duplicates.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module set_unordlist.
:- interface.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

:- type set_unordlist(_T).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

    % init(Set) is true iff Set is an empty set.
    %
:- func init = set_unordlist(T).
:- pred init(set_unordlist(_T)::uo) is det.

    % singleton_set(Elem, Set) is true iff Set is the set
    % containing just the single element Elem.
    %
:- pred singleton_set(T, set_unordlist(T)).
:- mode singleton_set(in, out) is det.
:- mode singleton_set(in, in) is semidet.     % Implied.
:- mode singleton_set(out, in) is semidet.

:- func make_singleton_set(T) = set_unordlist(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

    % is_empty(Set) is true iff Set is an empty set.
    %
:- pred is_empty(set_unordlist(_T)::in) is semidet.

    % is_non_empty(Set) is true iff Set is not an empty set.
    %
:- pred is_non_empty(set_unordlist(_T)::in) is semidet.

:- pred is_singleton(set_unordlist(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Membership tests.
%

    % member(X, Set) is true iff X is a member of Set.
    %
:- pred member(T, set_unordlist(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

    % is_member(X, Set, Result) returns `Result = yes' iff X is a member of
    % Set.
    %
:- pred is_member(T::in, set_unordlist(T)::in, bool::out) is det.

    % contains(Set, X) is true iff X is a member of Set.
    %
:- pred contains(set_unordlist(T)::in, T::in) is semidet.

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

    % insert(X, Set0, Set) is true iff Set is the union of Set0 and the
    % set containing only X.
    %
:- func insert(set_unordlist(T), T) = set_unordlist(T).
:- pred insert(T, set_unordlist(T), set_unordlist(T)).
:- mode insert(di, di, uo) is det.
:- mode insert(in, in, out) is det.

    % insert_new(X, Set0, Set) is true iff Set0 does not contain X, and
    % Set is the union of Set0 and the set containing only X.
    %
:- pred insert_new(T::in, set_unordlist(T)::in, set_unordlist(T)::out)
    is semidet.

    % insert_list(Xs, Set0, Set) is true iff Set is the
    % union of Set0 and the set containing only the members of Xs.
    %
:- func insert_list(set_unordlist(T), list(T))
    = set_unordlist(T).
:- pred insert_list(list(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

    % delete(X, Set0, Set) is true iff Set is the relative complement of
    % Set0 and the set containing only X, i.e. if Set is the set which
    % contains all the elements of Set0 except X.
    %
:- func delete(set_unordlist(T), T) = set_unordlist(T).
:- pred delete(T, set_unordlist(T), set_unordlist(T)).
:- mode delete(in, di, uo) is det.
:- mode delete(in, in, out) is det.

    % delete_list(Xs, Set0, Set) is true iff Set is the relative complement
    % of Set0 and the set containing only the members of Xs.
    %
:- func delete_list(set_unordlist(T), list(T)) = set_unordlist(T).
:- pred delete_list(list(T)::in, set_unordlist(T)::in, set_unordlist(T)::out)
    is det.

    % remove(X, Set0, Set) is true iff Set0 contains X,
    % and Set is the relative complement of Set0 and the set
    % containing only X, i.e. if Set is the set which contains
    % all the elements of Set0 except X.
    %
    % The det_remove version throws an exception instead of failing.
    %
:- pred remove(T::in, set_unordlist(T)::in, set_unordlist(T)::out) is semidet.
:- pred det_remove(T::in, set_unordlist(T)::in, set_unordlist(T)::out) is det.

    % remove_list(Xs, Set0, Set) is true iff Xs does not contain any
    % duplicates, Set0 contains every member of Xs, and Set is the
    % relative complement of Set0 and the set containing only the members of
    % Xs.
    %
    % The det_remove_list version throws an exception instead of failing.
    %
:- pred remove_list(list(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is semidet.
:- pred det_remove_list(list(T)::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

    % remove_least(X, Set0, Set) is true iff X is the least element in
    % Set0, and Set is the set which contains all the elements of Set0
    % except X.
    %
:- pred remove_least(T::out,
    set_unordlist(T)::in, set_unordlist(T)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

    % equal(SetA, SetB) is true iff SetA and SetB contain the same elements.
    %
:- pred equal(set_unordlist(T)::in, set_unordlist(T)::in) is semidet.

    % subset(SetA, SetB) is true iff SetA is a subset of SetB.
    %
:- pred subset(set_unordlist(T)::in, set_unordlist(T)::in) is semidet.

    % superset(SetA, SetB) is true iff SetA is a superset of SetB.
    %
:- pred superset(set_unordlist(T)::in, set_unordlist(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

    % union(SetA, SetB, Set) is true iff Set is the union of SetA and SetB.
    % If the sets are known to be of different sizes, then for
    % efficiency's make SetA the larger of the two.
    %
:- func union(set_unordlist(T), set_unordlist(T)) = set_unordlist(T).
:- pred union(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

    % union_list(A) = B is true iff B is the union of all the sets in A.
    %
:- func union_list(list(set_unordlist(T))) = set_unordlist(T).

    % power_union(A, B) is true iff B is the union of all the sets in A.
    %
:- func power_union(set_unordlist(set_unordlist(T))) = set_unordlist(T).
:- pred power_union(set_unordlist(set_unordlist(T))::in,
    set_unordlist(T)::out) is det.

    % intersect(SetA, SetB, Set) is true iff Set is the intersection of
    % SetA and SetB.
    %
:- func intersect(set_unordlist(T), set_unordlist(T)) = set_unordlist(T).
:- pred intersect(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

    % intersect_list(A, B) is true iff B is the intersection of all the
    % sets in A.
    %
:- func intersect_list(list(set_unordlist(T))) = set_unordlist(T).

    % power_intersect(A, B) is true iff B is the intersection of all the
    % sets in A.
    %
:- func power_intersect(set_unordlist(set_unordlist(T))) = set_unordlist(T).
:- pred power_intersect(set_unordlist(set_unordlist(T))::in,
    set_unordlist(T)::out) is det.

    % difference(SetA, SetB, Set) is true iff Set is the set containing all
    % the elements of SetA except those that occur in SetB
    %
:- func difference(set_unordlist(T), set_unordlist(T)) = set_unordlist(T).
:- pred difference(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

    % divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    % NOTE: this is the same as filter/4.
    %
:- pred divide(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in, set_unordlist(T)::out, set_unordlist(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

    % list_to_set(List, Set) is true iff Set is the set
    % containing only the members of List.
    %
:- func list_to_set(list(T)) = set_unordlist(T).
:- pred list_to_set(list(T)::in, set_unordlist(T)::out) is det.

    % A synonym for list_to_set/1.
    %
:- func from_list(list(T)) = set_unordlist(T).

    % sorted_list_to_set(List, Set) is true iff Set is the set
    % containing only the members of List. List must be sorted
    % in ascending order.
    %
:- func sorted_list_to_set(list(T)) = set_unordlist(T).
:- pred sorted_list_to_set(list(T)::in, set_unordlist(T)::out) is det.

    % A synonym for sorted_list_to_set/1.
    %
:- func from_sorted_list(list(T)) = set_unordlist(T).

    % rev_sorted_list_to_set(List, Set) is true iff Set is the set
    % containing only the members of List. List must be sorted
    % in descending order.
    %
:- func rev_sorted_list_to_set(list(T)) = set_unordlist(T).
:- pred rev_sorted_list_to_set(list(T)::in, set_unordlist(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

    % to_sorted_list(Set, List) is true iff List is the list of all the
    % members of Set, in sorted order.
    %
:- func to_sorted_list(set_unordlist(T)) = list(T).
:- pred to_sorted_list(set_unordlist(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Counting.
%

:- func count(set_unordlist(T)) = int.
:- pred count(set_unordlist(T)::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds for all the
    % elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in) is semidet.

    % Return the set of items for which the predicate succeeds.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

    % Return the set of items for which the predicate succeeds,
    % and the set for which it fails.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    set_unordlist(T)::in, set_unordlist(T)::out, set_unordlist(T)::out) is det.

:- func filter_map(func(T1) = T2, set_unordlist(T1)) = set_unordlist(T2).
:- mode filter_map(in(func(in) = out is semidet), in) = out is det.

:- func map(func(T1) = T2, set_unordlist(T1)) = set_unordlist(T2).

:- func fold(func(T1, T2) = T2, set_unordlist(T1), T2) = T2.
:- pred fold(pred(T1, T2, T2), set_unordlist(T1), T2, T2).
:- mode fold(in(pred(in, in, out) is det), in, in, out) is det.
:- mode fold(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode fold(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode fold(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode fold(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode fold(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

:- pred fold2(pred(T1, T2, T2, T3, T3), set_unordlist(T1),
    T2, T2, T3, T3).
:- mode fold2(in(pred(in, in, out, in, out) is det), in,
    in, out, in, out) is det.
:- mode fold2(in(pred(in, in, out, mdi, muo) is det), in,
    in, out, mdi, muo) is det.
:- mode fold2(in(pred(in, in, out, di, uo) is det), in,
    in, out, di, uo) is det.
:- mode fold2(in(pred(in, in, out, in, out) is semidet), in,
    in, out, in, out) is semidet.
:- mode fold2(in(pred(in, in, out, mdi, muo) is semidet), in,
    in, out, mdi, muo) is semidet.
:- mode fold2(in(pred(in, in, out, di, uo) is semidet), in,
    in, out, di, uo) is semidet.

:- pred fold3(pred(T1, T2, T2, T3, T3, T4, T4),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4).
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

:- pred fold4(pred(T1, T2, T2, T3, T3, T4, T4, T5, T5),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5).
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out, in, out) is det.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, di, uo) is det), in,
    in, out, in, out, in, out, di, uo) is det.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, in, out) is semidet), in,
    in, out, in, out, in, out, in, out) is semidet.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, mdi, muo) is semidet), in,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold4(
    in(pred(in, in, out, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, in, out, di, uo) is semidet.

:- pred fold5(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6).
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is det), in,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, in, out) is semidet), in,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet), in,
    in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold5(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred fold6(
    pred(T1, T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7),
    set_unordlist(T1), T2, T2, T3, T3, T4, T4, T5, T5, T6, T6, T7, T7).
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

:- import_module require.

%---------------------------------------------------------------------------%

:- type set_unordlist(T)
    --->    sul(list(T)).

%---------------------------------------------------------------------------%

init = S :-
    set_unordlist.init(S).

init(sul([])).

:- pragma promise_equivalent_clauses(pred(set_unordlist.singleton_set/2)).

singleton_set(X::in, Set::out) :-
    Set = sul([X]).
singleton_set(X::in, Set::in) :-
    Set = sul(Xs),
    list.sort_and_remove_dups(Xs, [X]).
singleton_set(X::out, Set::in) :-
    Set = sul(Xs),
    list.sort_and_remove_dups(Xs, [X]).

make_singleton_set(T) = S :-
    set_unordlist.singleton_set(T, S).

%---------------------------------------------------------------------------%

is_empty(sul([])).

is_non_empty(sul([_ | _])).

is_singleton(sul(Xs), X) :-
    list.sort_and_remove_dups(Xs, [X]).

%---------------------------------------------------------------------------%

member(E, sul(S)) :-
    list.member(E, S).

is_member(E, S, R) :-
    ( if set_unordlist.member(E, S) then
        R = yes
    else
        R = no
    ).

contains(S, E) :-
    set_unordlist.member(E, S).

%---------------------------------------------------------------------------%

insert(!.S, T) = !:S :-
    set_unordlist.insert(T, !S).

insert(E, sul(S0), sul([E | S0])).

insert_new(E, sul(S0), sul(S)) :-
    ( if list.member(E, S0) then
        fail
    else
        S = [E | S0]
    ).

insert_list(!.S, Xs) = !:S :-
    set_unordlist.insert_list(Xs, !S).

insert_list(List, sul(!.Set), sul(!:Set)) :-
    list.append(List, !Set).

%---------------------%

delete(!.S, T) = !:S :-
    set_unordlist.delete(T, !S).

delete(E, sul(!.S), sul(!:S)) :-
    list.delete_all(!.S, E, !:S).

delete_list(!.S, Xs) = !:S :-
    set_unordlist.delete_list(Xs, !S).

delete_list([], !S).
delete_list([X | Xs], !S) :-
    set_unordlist.delete(X, !S),
    set_unordlist.delete_list(Xs, !S).

%---------------------%

remove(E, sul(S0), sul(S)) :-
    list.member(E, S0),
    set_unordlist.delete(E, sul(S0), sul(S)).

det_remove(X, !Set) :-
    ( if set_unordlist.remove(X, !Set) then
        true
    else
        unexpected($pred, "remove failed")
    ).

remove_list([], !S).
remove_list([X | Xs], !S) :-
    set_unordlist.remove(X, !S),
    set_unordlist.remove_list(Xs, !S).

det_remove_list(List, !Set) :-
    ( if set_unordlist.remove_list(List, !Set) then
        true
    else
        unexpected($pred, "remove_list failed")
    ).

remove_least(E, Set0, sul(Set)) :-
    Set0 = sul([_ | _]),   % Fail early on an empty set.
    set_unordlist.to_sorted_list(Set0, [E | Set]).

%---------------------------------------------------------------------------%

equal(SetA, SetB) :-
    set_unordlist.subset(SetA, SetB),
    set_unordlist.subset(SetB, SetA).

subset(sul([]), _).
subset(sul([E | S0]), S1) :-
    set_unordlist.member(E, S1),
    set_unordlist.subset(sul(S0), S1).

superset(S0, S1) :-
    set_unordlist.subset(S1, S0).

%---------------------------------------------------------------------------%

union(S1, S2) = S3 :-
    set_unordlist.union(S1, S2, S3).

union(sul(Set0), sul(Set1), sul(Set)) :-
    list.append(Set1, Set0, Set).

union_list(LS) = S :-
    set_unordlist.power_union(sul(LS), S).

power_union(SS) = S :-
    set_unordlist.power_union(SS, S).

power_union(sul(PS), sul(S)) :-
    set_unordlist.init(S0),
    set_unordlist.power_union_2(PS, S0, sul(S1)),
    list.sort_and_remove_dups(S1, S).

:- pred set_unordlist.power_union_2(list(set_unordlist(T))::in,
    set_unordlist(T)::in, set_unordlist(T)::out) is det.

power_union_2([], !S).
power_union_2([T | Ts], !S) :-
    set_unordlist.union(!.S, T, !:S),
    set_unordlist.power_union_2(Ts, !S).

%---------------------%

intersect(S1, S2) = S3 :-
    set_unordlist.intersect(S1, S2, S3).

intersect(sul(S0), sul(S1), sul(S)) :-
    set_unordlist.intersect_2(S0, S1, [], S).

:- pred set_unordlist.intersect_2(list(T)::in, list(T)::in,
    list(T)::in, list(T)::out) is det.

intersect_2([], _, S, S).
intersect_2([E | S0], S1, S2, S) :-
    ( if list.member(E, S1) then
        S3 = [E | S2]
    else
        S3 = S2
    ),
    set_unordlist.intersect_2(S0, S1, S3, S).

intersect_list(Sets) =
    set_unordlist.power_intersect(sul(Sets)).

power_intersect(SS) = S :-
    set_unordlist.power_intersect(SS, S).

power_intersect(sul([]), sul([])).
power_intersect(sul([S0 | Ss]), S) :-
    (
        Ss = [],
        S = S0
    ;
        Ss = [_ | _],
        set_unordlist.power_intersect(sul(Ss), S1),
        set_unordlist.intersect(S1, S0, S)
    ).

%---------------------%

difference(S1, S2) = S3 :-
    set_unordlist.difference(S1, S2, S3).

difference(A, B, C) :-
    set_unordlist.difference_2(B, A, C).

:- pred set_unordlist.difference_2(set_unordlist(T)::in, set_unordlist(T)::in,
    set_unordlist(T)::out) is det.

difference_2(sul([]), C, C).
difference_2(sul([E | Es]), A, C) :-
    set_unordlist.delete(E, A, B),
    set_unordlist.difference_2(sul(Es), B, C).

%---------------------------------------------------------------------------%

divide(Pred, sul(Set), sul(RevTruePart), sul(RevFalsePart)) :-
    set_unordlist.divide_2(Pred, Set, [], RevTruePart, [], RevFalsePart).

:- pred set_unordlist.divide_2(pred(T1)::in(pred(in) is semidet),
    list(T1)::in,
    list(T1)::in, list(T1)::out,
    list(T1)::in, list(T1)::out) is det.

divide_2(_Pred, [], !RevTrue, !RevFalse).
divide_2(Pred, [H | T], !RevTrue, !RevFalse) :-
    ( if Pred(H) then
        !:RevTrue = [H | !.RevTrue]
    else
        !:RevFalse = [H | !.RevFalse]
    ),
    set_unordlist.divide_2(Pred, T, !RevTrue, !RevFalse).

%---------------------------------------------------------------------------%

list_to_set(Xs) = S :-
    set_unordlist.list_to_set(Xs, S).

list_to_set(List, sul(List)).

from_list(List) = sul(List).

sorted_list_to_set(List) = Set :-
    sorted_list_to_set(List, Set).

sorted_list_to_set(List, sul(List)).

from_sorted_list(List) = sul(List).

rev_sorted_list_to_set(List) = Set :-
    rev_sorted_list_to_set(List, Set).

rev_sorted_list_to_set(List, sul(List)).

%---------------------------------------------------------------------------%

to_sorted_list(Set) = SortedList :-
    to_sorted_list(Set, SortedList).

to_sorted_list(sul(Set), SortedList) :-
    list.sort_and_remove_dups(Set, SortedList).

%---------------------------------------------------------------------------%

count(Set) = Count :-
    set_unordlist.count(Set, Count).

count(sul(Set), Count) :-
    list.remove_dups(Set, Elems),
    list.length(Elems, Count).

%---------------------------------------------------------------------------%

all_true(P, sul(L)) :-
    list.all_true(P, L).

filter(Pred, Set, TrueSet) :-
    % XXX This should be more efficient.
    set_unordlist.divide(Pred, Set, TrueSet, _FalseSet).

filter(Pred, Set, TrueSet, FalseSet) :-
    set_unordlist.divide(Pred, Set, TrueSet, FalseSet).

filter_map(PF, S1) = S2 :-
    S2 = set_unordlist.list_to_set(list.filter_map(PF,
        set_unordlist.to_sorted_list(S1))).

map(F, S1) = S2 :-
    S2 = set_unordlist.list_to_set(list.map(F,
        set_unordlist.to_sorted_list(S1))).

fold(F, S, A) = B :-
    B = list.foldl(F, set_unordlist.to_sorted_list(S), A).

fold(P, S, !A) :-
    list.foldl(P, set_unordlist.to_sorted_list(S), !A).

fold2(P, S, !A, !B) :-
    list.foldl2(P, set_unordlist.to_sorted_list(S), !A, !B).

fold3(P, S, !A, !B, !C) :-
    list.foldl3(P, set_unordlist.to_sorted_list(S), !A, !B, !C).

fold4(P, S, !A, !B, !C, !D) :-
    list.foldl4(P, set_unordlist.to_sorted_list(S), !A, !B, !C, !D).

fold5(P, S, !A, !B, !C, !D, !E) :-
    list.foldl5(P, set_unordlist.to_sorted_list(S), !A, !B, !C, !D, !E).

fold6(P, S, !A, !B, !C, !D, !E, !F) :-
    list.foldl6(P, set_unordlist.to_sorted_list(S), !A, !B, !C, !D, !E, !F).

%---------------------------------------------------------------------------%
:- end_module set_unordlist.
%---------------------------------------------------------------------------%
