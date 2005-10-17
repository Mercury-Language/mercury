%---------------------------------------------------------------------------%
% vim:ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% set_tree234.m - implements a set using 2-3-4 trees.

%--------------------------------------------------------------------------%

:- module set_tree234.
:- interface.

:- import_module bool.
:- import_module list.

:- type set_tree234(_T).

    % `set_tree234__init = Set' is true iff `Set' is an empty set.
    %
:- func set_tree234__init = set_tree234(T).

    % `set_tree234__singleton_set(Elem, Set)' is true iff `Set' is the set
    % containing just the single element `Elem'.
    %
:- pred set_tree234__singleton_set(T, set_tree234(T)).
:- mode set_tree234__singleton_set(in, out) is det.
:- mode set_tree234__singleton_set(out, in) is semidet.

:- func set_tree234__make_singleton_set(T) = set_tree234(T).

    % `set_tree234__empty(Set)' is true iff `Set' is an empty set.
    %
:- pred set_tree234__empty(set_tree234(_T)::in) is semidet.

    % `set_tree234__member(X, Set)' is true iff `X' is a member of `Set'.
    %
:- pred set_tree234__member(set_tree234(T)::in, T::out) is nondet.

    % `set_tree234__is_member(Set, X, Result)' returns
    % `Result = yes' iff `X' is a member of `Set'.
    %
:- pred set_tree234__is_member(set_tree234(T)::in, T::in, bool::out) is det.
:- func set_tree234__is_member(set_tree234(T), T) = bool.

    % `set_tree234__contains(Set, X)' is true iff `X' is a member of `Set'.
    %
:- pred set_tree234__contains(set_tree234(T)::in, T::in) is semidet.

    % `set_tree234__list_to_set(List) = Set' is true iff `Set' is the set
    % containing only the members of `List'.
    %
:- func set_tree234__list_to_set(list(T)) = set_tree234(T).

    % `set_tree234__sorted_list_to_set(List) = Set' is true iff `Set' is
    % the set containing only the members of `List'. `List' must be sorted.
    %
:- func set_tree234__sorted_list_to_set(list(T)) = set_tree234(T).

    % `set_tree234__to_sorted_list(Set) = List' is true iff `List' is the
    % list of all the members of `Set', in sorted order.
    %
:- func set_tree234__to_sorted_list(set_tree234(T)) = list(T).

    % `set_tree234__equal(SetA, SetB)' is true iff
    % `SetA' and `SetB' contain the same elements.
    %
:- pred set_tree234__equal(set_tree234(T)::in, set_tree234(T)::in) is semidet.

    % `set_tree234__subset(SetA, SetB)' is true iff `SetA' is a subset of
    % `SetB'.
    %
:- pred set_tree234__subset(set_tree234(T)::in, set_tree234(T)::in) is semidet.

    % `set_tree234__superset(SetA, SetB)' is true iff `SetA' is a
    % superset of `SetB'.
    %
:- pred set_tree234__superset(set_tree234(T)::in, set_tree234(T)::in)
    is semidet.

    % `set_tree234__insert(X, Set0, Set)' is true iff `Set' is the union
    % of `Set0' and the set containing only `X'.
    %
:- pred set_tree234__insert(T::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.
:- func set_tree234__insert(T, set_tree234(T)) = set_tree234(T).

    % `set_tree234__insert_list(Xs, Set0, Set)' is true iff `Set' is the
    % union of `Set0' and the set containing only the members of `Xs'.
    %
:- pred set_tree234__insert_list(list(T)::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.
:- func set_tree234__insert_list(list(T), set_tree234(T)) = set_tree234(T).

    % `set_tree234__delete(X, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only `X', i.e.
    % if `Set' is the set which contains all the elements of `Set0'
    % except `X'.
    %
:- pred set_tree234__delete(T::in, set_tree234(T)::in, set_tree234(T)::out)
    is det.
:- func set_tree234__delete(T, set_tree234(T)) = set_tree234(T).

    % `set_tree234__delete_list(Xs, Set0, Set)' is true iff `Set' is the
    % relative complement of `Set0' and the set containing only the members
    % of `Xs'.
    %
:- pred set_tree234__delete_list(list(T)::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.
:- func set_tree234__delete_list(list(T), set_tree234(T)) = set_tree234(T).

    % `set_tree234__remove(X, Set0, Set)' is true iff `Set0' contains `X',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only `X', i.e.  if `Set' is the set which contains
    % all the elements of `Set0' except `X'.
    %
:- pred set_tree234__remove(T::in, set_tree234(T)::in, set_tree234(T)::out)
    is semidet.

    % `set_tree234__remove_list(Xs, Set0, Set)' is true iff Xs does not
    % contain any duplicates, `Set0' contains every member of `Xs',
    % and `Set' is the relative complement of `Set0' and the set
    % containing only the members of `Xs'.
    %
:- pred set_tree234__remove_list(list(T)::in,
    set_tree234(T)::in, set_tree234(T)::out) is semidet.

    % `set_tree234__remove_least(X, Set0, Set)' is true iff `X' is the
    % least element in `Set0', and `Set' is the set which contains all the
    % elements of `Set0' except `X'.
    %
:- pred set_tree234__remove_least(T::out,
    set_tree234(T)::in, set_tree234(T)::out) is semidet.

    % `set_tree234_union(SetA, SetB) = Set' is true iff `Set' is the union
    % of `SetA' and `SetB'.
    %
:- pred set_tree234__union(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.
:- func set_tree234__union(set_tree234(T), set_tree234(T)) = set_tree234(T).

    % `set_tree234__union_list(A, B)' is true iff `B' is the union of
    % all the sets in `A'
    %
:- pred set_tree234__union_list(list(set_tree234(T))::in, set_tree234(T)::out)
    is det.
:- func set_tree234__union_list(list(set_tree234(T))) = set_tree234(T).

    % `set_tree234__power_union(A) = B' is true iff `B' is the union of
    % all the sets in `A'
    %
:- pred set_tree234__power_union(set_tree234(set_tree234(T))::in,
    set_tree234(T)::out) is det.
:- func set_tree234__power_union(set_tree234(set_tree234(T))) = set_tree234(T).

    % `set_tree234__intersect(SetA, SetB) = Set' is true iff `Set' is the
    % intersection of `SetA' and `SetB'.
    %
:- pred set_tree234__intersect(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.
:- func set_tree234__intersect(set_tree234(T), set_tree234(T))
    = set_tree234(T).

    % `set_tree234__power_intersect(A, B)' is true iff `B' is the
    % intersection of all the sets in `A'.
    %
:- func set_tree234__power_intersect(set_tree234(set_tree234(T)))
    = set_tree234(T).

    % `set_tree234__intersect_list(A, B)' is true iff `B' is the
    % intersection of all the sets in `A'.
    %
:- func set_tree234__intersect_list(list(set_tree234(T))) = set_tree234(T).

    % `set_tree234__difference(SetA, SetB, Set)' is true iff `Set' is the
    % set containing all the elements of `SetA' except those that
    % occur in `SetB'.
    %
:- pred set_tree234__difference(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.
:- func set_tree234__difference(set_tree234(T), set_tree234(T))
    = set_tree234(T).

    % `set_tree234__count(Set, Count)' is true iff `Set' has
    % `Count' elements.
    %
:- func set_tree234__count(set_tree234(T)) = int.

:- pred set_tree234__map(pred(T1, T2)::in(pred(in, out) is det),
    set_tree234(T1)::in, set_tree234(T2)::out) is det.
:- func set_tree234__map(func(T1) = T2, set_tree234(T1)) = set_tree234(T2).

:- pred set_tree234__filter_map(pred(T1, T2)::in(pred(in, out) is semidet),
    set_tree234(T1)::in, set_tree234(T2)::out) is det.

:- func set_tree234__filter_map(func(T1) = T2, set_tree234(T1))
    = set_tree234(T2).
:- mode set_tree234__filter_map(func(in) = out is semidet, in) = out is det.

:- pred set_tree234__fold(pred(T1, T2, T2)::in(pred(in, in, out) is det),
    set_tree234(T1)::in, T2::in, T2::out) is det.
:- func set_tree234__fold(func(T1, T2) = T2, set_tree234(T1), T2) = T2.

    % set_tree234__divide(Pred, Set, TruePart, FalsePart):
    % TruePart consists of those elements of Set for which Pred succeeds;
    % FalsePart consists of those elements of Set for which Pred fails.
    %
:- pred set_tree234__divide(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in, set_tree234(T)::out, set_tree234(T)::out) is det.

    % set_tree234__divide_by_set(DivideBySet, Set, InPart, OutPart):
    % InPart consists of those elements of Set which are also in
    % DivideBySet; OutPart consists of those elements of which are
    % not in DivideBySet.
    %
:- pred set_tree234__divide_by_set(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out, set_tree234(T)::out) is det.

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module term.  % for var/1.

:- pragma type_spec(set_tree234__sorted_list_to_set/1, T = var(_)).
:- pragma type_spec(set_tree234__contains(in, in), T = var(_)).
:- pragma type_spec(set_tree234__insert/3, T = var(_)).
:- pragma type_spec(set_tree234__insert_list/3, T = var(_)).
:- pragma type_spec(set_tree234__union/2, T = var(_)).
:- pragma type_spec(set_tree234__union/3, T = var(_)).
:- pragma type_spec(set_tree234__intersect/2, T = var(_)).
:- pragma type_spec(set_tree234__intersect/3, T = var(_)).
:- pragma type_spec(set_tree234__difference/2, T = var(_)).
:- pragma type_spec(set_tree234__difference/3, T = var(_)).

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

%-----------------------------------------------------------------------------%

set_tree234__init = empty.

set_tree234__singleton_set(X, two(X, empty, empty)).

set_tree234__make_singleton_set(X) = two(X, empty, empty).

set_tree234__empty(empty).

set_tree234__member(empty, _) :- fail.
set_tree234__member(two(E0, T0, T1), E) :-
    (
        E = E0
    ;
        set_tree234__member(T0, E)
    ;
        set_tree234__member(T1, E)
    ).
set_tree234__member(three(E0, E1, T0, T1, T2), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        set_tree234__member(T0, E)
    ;
        set_tree234__member(T1, E)
    ;
        set_tree234__member(T2, E)
    ).
set_tree234__member(four(E0, E1, E2, T0, T1, T2, T3), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        E = E2
    ;
        set_tree234__member(T0, E)
    ;
        set_tree234__member(T1, E)
    ;
        set_tree234__member(T2, E)
    ;
        set_tree234__member(T3, E)
    ).

set_tree234__is_member(T, E, R) :-
    (
        T = empty,
        R = no
    ;
        T = two(E0, T0, T1),
        compare(Result, E, E0),
        (
            Result = (<),
            set_tree234__is_member(T0, E, R)
        ;
            Result = (=),
            R = yes
        ;
            Result = (>),
            set_tree234__is_member(T1, E, R)
        )
    ;
        T = three(E0, E1, T0, T1, T2),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            set_tree234__is_member(T0, E, R)
        ;
            Result0 = (=),
            R = yes
        ;
            Result0 = (>),
            compare(Result1, E, E1),
            (
                Result1 = (<),
                set_tree234__is_member(T1, E, R)
            ;
                Result1 = (=),
                R = yes
            ;
                Result1 = (>),
                set_tree234__is_member(T2, E, R)
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
                set_tree234__is_member(T0, E, R)
            ;
                Result0 = (=),
                R = yes
            ;
                Result0 = (>),
                set_tree234__is_member(T1, E, R)
            )
        ;
            Result1 = (=),
            R = yes
        ;
            Result1 = (>),
            compare(Result2, E, E2),
            (
                Result2 = (<),
                set_tree234__is_member(T2, E, R)
            ;
                Result2 = (=),
                R = yes
            ;
                Result2 = (>),
                set_tree234__is_member(T3, E, R)
            )
        )
    ).

set_tree234__is_member(T, E) = R :-
    set_tree234__is_member(T, E, R).

set_tree234__contains(T, E) :-
    set_tree234__is_member(T, E, yes).

%------------------------------------------------------------------------------%

set_tree234__list_to_set(List) = Tree :-
    set_tree234__list_to_set_2(List, empty, Tree).

set_tree234__sorted_list_to_set(List) = Tree :-
        % XXX We should exploit the sortedness of List.
    set_tree234__list_to_set_2(List, empty, Tree).

:- pred set_tree234__list_to_set_2(list(E)::in, set_tree234(E)::in,
    set_tree234(E)::out) is det.

set_tree234__list_to_set_2([], !Tree).
set_tree234__list_to_set_2([E | Es], !Tree) :-
    set_tree234__insert(E, !Tree),
    set_tree234__list_to_set_2(Es, !Tree).

%------------------------------------------------------------------------------%

set_tree234__to_sorted_list(Tree) = List :-
    set_tree234__to_sorted_list_2(Tree, [], List).

:- pred set_tree234__to_sorted_list_2(set_tree234(T)::in,
    list(T)::in, list(T)::out) is det.

set_tree234__to_sorted_list_2(empty, L, L).
set_tree234__to_sorted_list_2(two(E0, T0, T1), L0, L) :-
    set_tree234__to_sorted_list_2(T1, L0, L1),
    set_tree234__to_sorted_list_2(T0, [E0 | L1], L).
set_tree234__to_sorted_list_2(three(E0, E1, T0, T1, T2), L0, L) :-
    set_tree234__to_sorted_list_2(T2, L0, L1),
    set_tree234__to_sorted_list_2(T1, [E1 | L1], L2),
    set_tree234__to_sorted_list_2(T0, [E0 | L2], L).
set_tree234__to_sorted_list_2(four(E0, E1, E2, T0, T1, T2, T3), L0, L) :-
    set_tree234__to_sorted_list_2(T3, L0, L1),
    set_tree234__to_sorted_list_2(T2, [E2 | L1], L2),
    set_tree234__to_sorted_list_2(T1, [E1 | L2], L3),
    set_tree234__to_sorted_list_2(T0, [E0 | L3], L).

%------------------------------------------------------------------------------%

set_tree234__equal(SetA, SetB) :-
    ListA = set_tree234__to_sorted_list(SetA),
    ListB = set_tree234__to_sorted_list(SetB),
    ListA = ListB.

set_tree234__subset(empty, _Set).
set_tree234__subset(two(E, T0, T1), Set) :-
    set_tree234__subset(T0, Set),
    set_tree234__contains(Set, E),
    set_tree234__subset(T1, Set).
set_tree234__subset(three(E0, E1, T0, T1, T2), Set) :-
    set_tree234__subset(T0, Set),
    set_tree234__contains(Set, E0),
    set_tree234__subset(T1, Set),
    set_tree234__contains(Set, E1),
    set_tree234__subset(T2, Set).
set_tree234__subset(four(E0, E1, E2, T0, T1, T2, T3), Set) :-
    set_tree234__subset(T0, Set),
    set_tree234__contains(Set, E0),
    set_tree234__subset(T1, Set),
    set_tree234__contains(Set, E1),
    set_tree234__subset(T2, Set),
    set_tree234__contains(Set, E2),
    set_tree234__subset(T3, Set).

set_tree234__superset(SuperSet, Set) :-
    set_tree234__subset(Set, SuperSet).

%------------------------------------------------------------------------------%

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

%------------------------------------------------------------------------------%

set_tree234__insert(E, Tin) = Tout :-
    set_tree234__insert(E, Tin, Tout).

set_tree234__insert(E, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(E, empty, empty)
    ;
        Tin = two(_, _, _),
        set_tree234__insert2(E, Tin, Tout)
    ;
        Tin = three(_, _, _, _, _),
        set_tree234__insert3(E, Tin, Tout)
    ;
        Tin = four(E0, E1, E2, T0, T1, T2, T3),
        compare(Result1, E, E1),
        (
            Result1 = (<),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            set_tree234__insert2(E, Sub0, NewSub0),
            Tout = two(E1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Tout = Tin
        ;
            Result1 = (>),
            Sub0 = two(E0, T0, T1),
            Sub1 = two(E2, T2, T3),
            set_tree234__insert2(E, Sub1, NewSub1),
            Tout = two(E1, Sub0, NewSub1)
        )
    ).

:- pragma type_spec(set_tree234__insert2(in, in_two, out), T = var(_)).

:- pred set_tree234__insert2(T::in, set_tree234(T)::in_two,
    set_tree234(T)::out) is det.

set_tree234__insert2(E, Tin, Tout) :-
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
                set_tree234__split_four(T0, MT0E, T00, T01),
                compare(Result1, E, MT0E),
                (
                    Result1 = (<),
                    set_tree234__insert2(E, T00, NewT00),
                    Tout = three(MT0E, E0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    Tout = three(MT0E, E0, T00, T01, T1)
                ;
                    Result1 = (>),
                    set_tree234__insert2(E, T01, NewT01),
                    Tout = three(MT0E, E0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _),
                set_tree234__insert3(E, T0, NewT0),
                Tout = two(E0, NewT0, T1)
            ;
                T0 = two(_, _, _),
                set_tree234__insert2(E, T0, NewT0),
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
                set_tree234__split_four(T1, MT1E, T10, T11),
                compare(Result1, E, MT1E),
                (
                    Result1 = (<),
                    set_tree234__insert2(E, T10, NewT10),
                    Tout = three(E0, MT1E, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    Tout = three(E0, MT1E, T0, T10, T11)
                ;
                    Result1 = (>),
                    set_tree234__insert2(E, T11, NewT11),
                    Tout = three(E0, MT1E, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _),
                set_tree234__insert3(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = two(_, _, _),
                set_tree234__insert2(E, T1, NewT1),
                Tout = two(E0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(E, empty, empty),
                Tout = two(E0, T0, NewT1)
            )
        )
    ).

:- pragma type_spec(set_tree234__insert3(in, in_three, out), T = var(_)).

:- pred set_tree234__insert3(T::in, set_tree234(T)::in_three,
    set_tree234(T)::out) is det.

set_tree234__insert3(E, Tin, Tout) :-
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
                set_tree234__split_four(T0, MT0E, T00, T01),
                compare(ResultM, E, MT0E),
                (
                    ResultM = (<),
                    set_tree234__insert2(E, T00, NewT00),
                    Tout = four(MT0E, E0, E1, NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    Tout = four(MT0E, E0, E1, T00, T01, T1, T2)
                ;
                    ResultM = (>),
                    set_tree234__insert2(E, T01, NewT01),
                    Tout = four(MT0E, E0, E1, T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _),
                set_tree234__insert3(E, T0, NewT0),
                Tout = three(E0, E1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _),
                set_tree234__insert2(E, T0, NewT0),
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
                    set_tree234__split_four(T1, MT1E, T10, T11),
                    compare(ResultM, E, MT1E),
                    (
                        ResultM = (<),
                        set_tree234__insert2(E, T10, NewT10),
                        Tout = four(E0, MT1E, E1, T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        Tout = four(E0, MT1E, E1, T0, T10, T11, T2)
                    ;
                        ResultM = (>),
                        set_tree234__insert2(E, T11, NewT11),
                        Tout = four(E0, MT1E, E1, T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _),
                    set_tree234__insert3(E, T1, NewT1),
                    Tout = three(E0, E1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _),
                    set_tree234__insert2(E, T1, NewT1),
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
                    set_tree234__split_four(T2, MT2E, T20, T21),
                    compare(ResultM, E, MT2E),
                    (
                        ResultM = (<),
                        set_tree234__insert2(E, T20, NewT20),
                        Tout = four(E0, E1, MT2E, T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, T21)
                    ;
                        ResultM = (>),
                        set_tree234__insert2(E, T21, NewT21),
                        Tout = four(E0, E1, MT2E, T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _),
                    set_tree234__insert3(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _),
                    set_tree234__insert2(E, T2, NewT2),
                    Tout = three(E0, E1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(E, empty, empty),
                    Tout = three(E0, E1, T0, T1, NewT2)
                )
            )
        )
    ).

set_tree234__insert_list(Es, Set0) = Set :-
    set_tree234__insert_list(Es, Set0, Set).

set_tree234__insert_list([], !Set).
set_tree234__insert_list([E | Es], !Set) :-
    set_tree234__insert(E, !Set),
    set_tree234__insert_list(Es, !Set).

%------------------------------------------------------------------------------%

:- pred set_tree234__split_four(set_tree234(E)::in_four, E::out,
    set_tree234(E)::out_two, set_tree234(E)::out_two) is det.

set_tree234__split_four(Tin, MidE, Sub0, Sub1) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    Sub0 = two(E0, T0, T1),
    MidE = E1,
    Sub1 = two(E2, T2, T3).

%------------------------------------------------------------------------------%

set_tree234__delete(E, Tin) = Tout :-
    set_tree234__delete(E, Tin, Tout).

set_tree234__delete(E, Tin, Tout) :-
    set_tree234__delete_2(E, Tin, Tout, _).

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.

:- pred set_tree234__delete_2(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is det.

set_tree234__delete_2(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        Tout = empty,
        RH = no
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            set_tree234__delete_2(E, T0, NewT0, RHT0),
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
                set_tree234__remove_least_2(T1, ST1E,  NewT1, RHT1)
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
            set_tree234__delete_2(E, T1, NewT1, RHT1),
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
            set_tree234__delete_2(E, T0, NewT0, RHT0),
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
                set_tree234__remove_least_2(T1, ST1E, NewT1, RHT1)
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
                set_tree234__delete_2(E, T1, NewT1, RHT1),
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
                    set_tree234__remove_least_2(T2, ST2E, NewT2, RHT2)
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
                set_tree234__delete_2(E, T2, NewT2, RHT2),
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
                set_tree234__delete_2(E, T0, NewT0, RHT0),
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
                    set_tree234__remove_least_2(T1, ST1E, NewT1, RHT1)
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
                set_tree234__delete_2(E, T1, NewT1, RHT1),
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
                set_tree234__remove_least_2(T2, ST2E, NewT2, RHT2)
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
                set_tree234__delete_2(E, T2, NewT2, RHT2),
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
                    set_tree234__remove_least_2(T3, ST3E, NewT3, RHT3)
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
                set_tree234__delete_2(E, T3, NewT3, RHT3),
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

set_tree234__delete_list(SetA, SetB) = Set:-
    set_tree234__delete_list(SetA, SetB, Set).

set_tree234__delete_list([], !Set).
set_tree234__delete_list([E | Es], !Set) :-
    set_tree234__delete(E, !Set),
    set_tree234__delete_list(Es, !Set).

%------------------------------------------------------------------------------%

    % We use the same algorithm as set_tree234__delete.

set_tree234__remove(E, Tin, Tout) :-
    set_tree234__remove_2(E, Tin, Tout, _).

:- pred set_tree234__remove_2(T::in, set_tree234(T)::in, set_tree234(T)::out,
    bool::out) is semidet.

set_tree234__remove_2(E, Tin, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(E0, T0, T1),
        compare(Result0, E, E0),
        (
            Result0 = (<),
            set_tree234__remove_2(E, T0, NewT0, RHT0),
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
                set_tree234__remove_least_2(T1, ST1E, NewT1, RHT1)
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
            set_tree234__remove_2(E, T1, NewT1, RHT1),
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
            set_tree234__remove_2(E, T0, NewT0, RHT0),
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
                set_tree234__remove_least_2(T1, ST1E, NewT1, RHT1)
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
                set_tree234__remove_2(E, T1, NewT1, RHT1),
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
                    set_tree234__remove_least_2(T2, ST2E, NewT2, RHT2)
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
                set_tree234__remove_2(E, T2, NewT2, RHT2),
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
                set_tree234__remove_2(E, T0, NewT0, RHT0),
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
                    set_tree234__remove_least_2(T1, ST1E, NewT1, RHT1)
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
                set_tree234__remove_2(E, T1, NewT1, RHT1),
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
                set_tree234__remove_least_2(T2, ST2E, NewT2, RHT2)
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
                set_tree234__remove_2(E, T2, NewT2, RHT2),
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
                    set_tree234__remove_least_2(T3, ST3E, NewT3, RHT3)
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
                set_tree234__remove_2(E, T3, NewT3, RHT3),
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

set_tree234__remove_list([], !Set).
set_tree234__remove_list([E | Es], !Set) :-
    set_tree234__remove(E, !Set),
    set_tree234__remove_list(Es, !Set).

%------------------------------------------------------------------------------%

    % The algorithm we use similar to set_tree234__delete, except that we
    % always go down the left subtree.

set_tree234__remove_least(E, Tin, Tout) :-
    set_tree234__remove_least_2(Tin, E, Tout, _).

:- pred set_tree234__remove_least_2(set_tree234(E)::in, E::out,
    set_tree234(E)::out, bool::out) is semidet.

set_tree234__remove_least_2(Tin, E, Tout, RH) :-
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
            set_tree234__remove_least_2(T0, E, NewT0, RHT0),
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
            set_tree234__remove_least_2(T0, E, NewT0, RHT0),
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
            set_tree234__remove_least_2(T0, E, NewT0, RHT0),
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

%------------------------------------------------------------------------------%

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

%------------------------------------------------------------------------------%

set_tree234__union(SetA, SetB) = Set :-
    set_tree234__union(SetA, SetB, Set).

set_tree234__union(empty, !Set).
set_tree234__union(two(E0, T0, T1), !Set) :-
    set_tree234__union(T0, !Set),
    set_tree234__insert(E0, !Set),
    set_tree234__union(T1, !Set).
set_tree234__union(three(E0, E1, T0, T1, T2), !Set) :-
    set_tree234__union(T0, !Set),
    set_tree234__insert(E0, !Set),
    set_tree234__union(T1, !Set),
    set_tree234__insert(E1, !Set),
    set_tree234__union(T2, !Set).
set_tree234__union(four(E0, E1, E2, T0, T1, T2, T3), !Set) :-
    set_tree234__union(T0, !Set),
    set_tree234__insert(E0, !Set),
    set_tree234__union(T1, !Set),
    set_tree234__insert(E1, !Set),
    set_tree234__union(T2, !Set),
    set_tree234__insert(E2, !Set),
    set_tree234__union(T3, !Set).

set_tree234__union_list(Sets) = Union :-
    set_tree234__union_list(Sets, Union).

set_tree234__union_list([], empty).
set_tree234__union_list([Set | Sets], Union) :-
    set_tree234__union_list(Sets, Union1),
    set_tree234__union(Set, Union1, Union).

set_tree234__power_union(Sets) = Union :-
    set_tree234__power_union(Sets, Union).

set_tree234__power_union(Sets, Union) :-
    set_tree234__power_union_2(Sets, empty, Union).

:- pred set_tree234__power_union_2(set_tree234(set_tree234(T))::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.

set_tree234__power_union_2(empty, !Union).
set_tree234__power_union_2(two(E0, T0, T1), !Union) :-
    set_tree234__power_union_2(T0, !Union),
    set_tree234__union(E0, !Union),
    set_tree234__power_union_2(T1, !Union).
set_tree234__power_union_2(three(E0, E1, T0, T1, T2), !Union) :-
    set_tree234__power_union_2(T0, !Union),
    set_tree234__union(E0, !Union),
    set_tree234__power_union_2(T1, !Union),
    set_tree234__union(E1, !Union),
    set_tree234__power_union_2(T2, !Union).
set_tree234__power_union_2(four(E0, E1, E2, T0, T1, T2, T3), !Union) :-
    set_tree234__power_union_2(T0, !Union),
    set_tree234__union(E0, !Union),
    set_tree234__power_union_2(T1, !Union),
    set_tree234__union(E1, !Union),
    set_tree234__power_union_2(T2, !Union),
    set_tree234__union(E2, !Union),
    set_tree234__power_union_2(T3, !Union).

%------------------------------------------------------------------------------%

set_tree234__intersect(SetA, SetB) = Set :-
    set_tree234__intersect(SetA, SetB, Set).

set_tree234__intersect(SetA, SetB, Intersect) :-
    set_tree234__intersect_2(SetA, SetB, empty, Intersect).

:- pred set_tree234__intersect_2(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out) is det.

set_tree234__intersect_2(empty, _SetB, !Intersect).
set_tree234__intersect_2(two(E0, T0, T1), SetB, !Intersect) :-
    set_tree234__intersect_2(T0, SetB, !Intersect),
    ( set_tree234__contains(SetB, E0) ->
        set_tree234__insert(E0, !Intersect)
    ;
        true
    ),
    set_tree234__intersect_2(T1, SetB, !Intersect).
set_tree234__intersect_2(three(E0, E1, T0, T1, T2), SetB, !Intersect) :-
    set_tree234__intersect_2(T0, SetB, !Intersect),
    ( set_tree234__contains(SetB, E0) ->
        set_tree234__insert(E0, !Intersect)
    ;
        true
    ),
    set_tree234__intersect_2(T1, SetB, !Intersect),
    ( set_tree234__contains(SetB, E1) ->
        set_tree234__insert(E1, !Intersect)
    ;
        true
    ),
    set_tree234__intersect_2(T2, SetB, !Intersect).
set_tree234__intersect_2(four(E0, E1, E2, T0, T1, T2, T3), SetB, !Intersect) :-
    set_tree234__intersect_2(T0, SetB, !Intersect),
    ( set_tree234__contains(SetB, E0) ->
        set_tree234__insert(E0, !Intersect)
    ;
        true
    ),
    set_tree234__intersect_2(T1, SetB, !Intersect),
    ( set_tree234__contains(SetB, E1) ->
        set_tree234__insert(E1, !Intersect)
    ;
        true
    ),
    set_tree234__intersect_2(T2, SetB, !Intersect),
    ( set_tree234__contains(SetB, E2) ->
        set_tree234__insert(E2, !Intersect)
    ;
        true
    ),
    set_tree234__intersect_2(T3, SetB, !Intersect).

set_tree234__intersect_list([]) = empty.
set_tree234__intersect_list([Set | Sets]) =
    set_tree234__intersect_list_2(Set, Sets).

:- func set_tree234__intersect_list_2(set_tree234(T), list(set_tree234(T)))
    = set_tree234(T).

set_tree234__intersect_list_2(Set, []) = Set.
set_tree234__intersect_list_2(Set, [Head | Tail]) =
    ( Set = empty ->
        empty
    ;
        set_tree234__intersect_list_2(set_tree234__intersect(Set, Head), Tail)
    ).

set_tree234__power_intersect(Sets) =
    set_tree234__intersect_list(set_tree234__to_sorted_list(Sets)).

%------------------------------------------------------------------------------%

    % `set_tree234__difference(SetA, SetB, Set)' is true iff `Set' is the
    % set containing all the elements of `SetA' except those that
    % occur in `SetB'.

set_tree234__difference(SetA, SetB) = Diff :-
    set_tree234__difference(SetA, SetB, Diff).

set_tree234__difference(SetA, SetB, Diff) :-
    set_tree234__difference_2(SetB, SetA, Diff).

:- pred set_tree234__difference_2(set_tree234(T)::in, set_tree234(T)::in,
    set_tree234(T)::out) is det.

set_tree234__difference_2(empty, !Set).
set_tree234__difference_2(two(E0, T0, T1), !Set) :-
    set_tree234__difference_2(T0, !Set),
    set_tree234__delete(E0, !Set),
    set_tree234__difference_2(T1, !Set).
set_tree234__difference_2(three(E0, E1, T0, T1, T2), !Set) :-
    set_tree234__difference_2(T0, !Set),
    set_tree234__delete(E0, !Set),
    set_tree234__difference_2(T1, !Set),
    set_tree234__delete(E1, !Set),
    set_tree234__difference_2(T2, !Set).
set_tree234__difference_2(four(E0, E1, E2, T0, T1, T2, T3), !Set) :-
    set_tree234__difference_2(T0, !Set),
    set_tree234__delete(E0, !Set),
    set_tree234__difference_2(T1, !Set),
    set_tree234__delete(E1, !Set),
    set_tree234__difference_2(T2, !Set),
    set_tree234__delete(E2, !Set),
    set_tree234__difference_2(T3, !Set).

%------------------------------------------------------------------------------%

    % count the number of elements in a tree
set_tree234__count(empty) = 0.
set_tree234__count(two(_, T0, T1)) = N :-
    N0 = set_tree234__count(T0),
    N1 = set_tree234__count(T1),
    N = 1 + N0 + N1.
set_tree234__count(three(_, _, T0, T1, T2)) = N :-
    N0 = set_tree234__count(T0),
    N1 = set_tree234__count(T1),
    N2 = set_tree234__count(T2),
    N = 2 + N0 + N1 + N2.
set_tree234__count(four(_, _, _, T0, T1, T2, T3)) = N :-
    N0 = set_tree234__count(T0),
    N1 = set_tree234__count(T1),
    N2 = set_tree234__count(T2),
    N3 = set_tree234__count(T3),
    N = 3 + N0 + N1 + N2 + N3.

%------------------------------------------------------------------------------%

set_tree234__fold(_Pred, empty, !A).
set_tree234__fold(Pred, two(E, T0, T1), !A) :-
    set_tree234__fold(Pred, T0, !A),
    call(Pred, E, !A),
    set_tree234__fold(Pred, T1, !A).
set_tree234__fold(Pred, three(E0, E1, T0, T1, T2), !A) :-
    set_tree234__fold(Pred, T0, !A),
    call(Pred, E0, !A),
    set_tree234__fold(Pred, T1, !A),
    call(Pred, E1, !A),
    set_tree234__fold(Pred, T2, !A).
set_tree234__fold(Pred, four(E0, E1, E2, T0, T1, T2, T3), !A) :-
    set_tree234__fold(Pred, T0, !A),
    call(Pred, E0, !A),
    set_tree234__fold(Pred, T1, !A),
    call(Pred, E1, !A),
    set_tree234__fold(Pred, T2, !A),
    call(Pred, E2, !A),
    set_tree234__fold(Pred, T3, !A).

set_tree234__fold(_Func, empty, A) = A.
set_tree234__fold(Func, two(E, T0, T1), !.A) = !:A :-
    set_tree234__fold(Func, T0, !.A) = !:A,
    !:A = Func(E, !.A),
    set_tree234__fold(Func, T1, !.A) = !:A.
set_tree234__fold(Func, three(E0, E1, T0, T1, T2), !.A) = !:A :-
    set_tree234__fold(Func, T0, !.A) = !:A,
    !:A = Func(E0, !.A),
    set_tree234__fold(Func, T1, !.A) = !:A,
    !:A = Func(E1, !.A),
    set_tree234__fold(Func, T2, !.A) = !:A.
set_tree234__fold(Func, four(E0, E1, E2, T0, T1, T2, T3), !.A) = !:A :-
    set_tree234__fold(Func, T0, !.A) = !:A,
    !:A = Func(E0, !.A),
    set_tree234__fold(Func, T1, !.A) = !:A,
    !:A = Func(E1, !.A),
    set_tree234__fold(Func, T2, !.A) = !:A,
    !:A = Func(E2, !.A),
    set_tree234__fold(Func, T3, !.A) = !:A.

%------------------------------------------------------------------------------%

set_tree234__map(Pred, SetA, SetB) :-
    set_tree234__map_pred(Pred, SetA, [], ListB),
    SetB = set_tree234__list_to_set(ListB).

:- pred set_tree234__map_pred(pred(T1, T2)::in(pred(in, out) is det),
    set_tree234(T1)::in, list(T2)::in, list(T2)::out) is det.

set_tree234__map_pred(_Pred, empty, !List).
set_tree234__map_pred(Pred, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234__map_pred(Pred, T0, !List),
    call(Pred, E0, N0),
    !:List = [N0 | !.List],
    set_tree234__map_pred(Pred, T1, !List).
set_tree234__map_pred(Pred, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234__map_pred(Pred, T0, !List),
    call(Pred, E0, N0),
    !:List = [N0 | !.List],
    set_tree234__map_pred(Pred, T1, !List),
    call(Pred, E1, N1),
    !:List = [N1 | !.List],
    set_tree234__map_pred(Pred, T2, !List).
set_tree234__map_pred(Pred, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234__map_pred(Pred, T0, !List),
    call(Pred, E0, N0),
    !:List = [N0 | !.List],
    set_tree234__map_pred(Pred, T1, !List),
    call(Pred, E1, N1),
    !:List = [N1 | !.List],
    set_tree234__map_pred(Pred, T2, !List),
    call(Pred, E2, N2),
    !:List = [N2 | !.List],
    set_tree234__map_pred(Pred, T3, !List).

set_tree234__map(Func, SetA) = SetB :-
    set_tree234__map_func(Func, SetA, [], ListB),
    SetB = set_tree234__list_to_set(ListB).

:- pred set_tree234__map_func(func(T1) = T2, set_tree234(T1),
    list(T2), list(T2)).
:- mode set_tree234__map_func(in(func(in) = out is det), in, in, out) is det.

set_tree234__map_func(_Func, empty, !List).
set_tree234__map_func(Func, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234__map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    set_tree234__map_func(Func, T1, !List).
set_tree234__map_func(Func, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234__map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    set_tree234__map_func(Func, T1, !List),
    N1 = Func(E1),
    !:List = [N1 | !.List],
    set_tree234__map_func(Func, T2, !List).
set_tree234__map_func(Func, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234__map_func(Func, T0, !List),
    N0 = Func(E0),
    !:List = [N0 | !.List],
    set_tree234__map_func(Func, T1, !List),
    N1 = Func(E1),
    !:List = [N1 | !.List],
    set_tree234__map_func(Func, T2, !List),
    N2 = Func(E2),
    !:List = [N2 | !.List],
    set_tree234__map_func(Func, T3, !List).

%------------------------------------------------------------------------------%

set_tree234__filter_map(Pred, SetA, SetB) :-
    set_tree234__filter_map_pred(Pred, SetA, [], ListB),
    SetB = set_tree234__list_to_set(ListB).

:- pred set_tree234__filter_map_pred(
    pred(T1, T2)::in(pred(in, out) is semidet), set_tree234(T1)::in,
    list(T2)::in, list(T2)::out) is det.

set_tree234__filter_map_pred(_Pred, empty, !List).
set_tree234__filter_map_pred(Pred, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234__filter_map_pred(Pred, T0, !List),
    ( Pred(E0, N0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_pred(Pred, T1, !List).
set_tree234__filter_map_pred(Pred, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234__filter_map_pred(Pred, T0, !List),
    ( Pred(E0, N0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_pred(Pred, T1, !List),
    ( Pred(E1, N1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_pred(Pred, T2, !List).
set_tree234__filter_map_pred(Pred, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234__filter_map_pred(Pred, T0, !List),
    ( Pred(E0, N0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_pred(Pred, T1, !List),
    ( Pred(E1, N1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_pred(Pred, T2, !List),
    ( Pred(E2, N2) ->
        !:List = [N2 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_pred(Pred, T3, !List).

set_tree234__filter_map(Func, SetA) = SetB :-
    set_tree234__filter_map_func(Func, SetA, [], ListB),
    SetB = set_tree234__list_to_set(ListB).

:- pred set_tree234__filter_map_func(func(T1) = T2, set_tree234(T1),
    list(T2), list(T2)).
:- mode set_tree234__filter_map_func(in(func(in) = out is semidet),
    in, in, out) is det.

set_tree234__filter_map_func(_Func, empty, !List).
set_tree234__filter_map_func(Func, Tin, !List) :-
    Tin = two(E0, T0, T1),
    set_tree234__filter_map_func(Func, T0, !List),
    ( N0 = Func(E0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_func(Func, T1, !List).
set_tree234__filter_map_func(Func, Tin, !List) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234__filter_map_func(Func, T0, !List),
    ( N0 = Func(E0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_func(Func, T1, !List),
    ( N1 = Func(E1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_func(Func, T2, !List).
set_tree234__filter_map_func(Func, Tin, !List) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234__filter_map_func(Func, T0, !List),
    ( N0 = Func(E0) ->
        !:List = [N0 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_func(Func, T1, !List),
    ( N1 = Func(E1) ->
        !:List = [N1 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_func(Func, T2, !List),
    ( N2 = Func(E2) ->
        !:List = [N2 | !.List]
    ;
        true
    ),
    set_tree234__filter_map_func(Func, T3, !List).

%------------------------------------------------------------------------------%

set_tree234__divide(Pred, Set, TrueSet, FalseSet) :-
    set_tree234__divide_2(Pred, Set, empty, TrueSet, empty, FalseSet).

:- pred set_tree234__divide_2(pred(T)::in(pred(in) is semidet),
    set_tree234(T)::in,
    set_tree234(T)::in, set_tree234(T)::out,
    set_tree234(T)::in, set_tree234(T)::out) is det.

set_tree234__divide_2(_Pred, empty, !TrueSet, !FalseSet).
set_tree234__divide_2(Pred, Tin, !TrueSet, !FalseSet) :-
    Tin = two(E0, T0, T1),
    set_tree234__divide_2(Pred, T0, !TrueSet, !FalseSet),
    ( Pred(E0) ->
        set_tree234__insert(E0, !TrueSet)
    ;
        set_tree234__insert(E0, !FalseSet)
    ),
    set_tree234__divide_2(Pred, T1, !TrueSet, !FalseSet).
set_tree234__divide_2(Pred, Tin, !TrueSet, !FalseSet) :-
    Tin = three(E0, E1, T0, T1, T2),
    set_tree234__divide_2(Pred, T0, !TrueSet, !FalseSet),
    ( Pred(E0) ->
        set_tree234__insert(E0, !TrueSet)
    ;
        set_tree234__insert(E0, !FalseSet)
    ),
    set_tree234__divide_2(Pred, T1, !TrueSet, !FalseSet),
    ( Pred(E1) ->
        set_tree234__insert(E1, !TrueSet)
    ;
        set_tree234__insert(E1, !FalseSet)
    ),
    set_tree234__divide_2(Pred, T2, !TrueSet, !FalseSet).
set_tree234__divide_2(Pred, Tin, !TrueSet, !FalseSet) :-
    Tin = four(E0, E1, E2, T0, T1, T2, T3),
    set_tree234__divide_2(Pred, T0, !TrueSet, !FalseSet),
    ( Pred(E0) ->
        set_tree234__insert(E0, !TrueSet)
    ;
        set_tree234__insert(E0, !FalseSet)
    ),
    set_tree234__divide_2(Pred, T1, !TrueSet, !FalseSet),
    ( Pred(E1) ->
        set_tree234__insert(E1, !TrueSet)
    ;
        set_tree234__insert(E1, !FalseSet)
    ),
    set_tree234__divide_2(Pred, T2, !TrueSet, !FalseSet),
    ( Pred(E2) ->
        set_tree234__insert(E2, !TrueSet)
    ;
        set_tree234__insert(E2, !FalseSet)
    ),
    set_tree234__divide_2(Pred, T3, !TrueSet, !FalseSet).

set_tree234__divide_by_set(DivideBySet, Set, TrueSet, FalseSet) :-
    set_tree234__divide(set_tree234__contains(DivideBySet), Set,
        TrueSet, FalseSet).

%------------------------------------------------------------------------------%
