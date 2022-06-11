%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997,2000,2003,2005-2006 The University of Melbourne.
% Copyright (C) 2014-2015,2021-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: ra_list.m
% Main author: bromage.
% Stability: medium-low
%
% This module implements `random access lists', or ra_lists for short.
% It is very similar to a list data type, and it supports O(1) head/tail/cons
% operations, but it also supports O(log n) lookup and update.
% The representation is a list of perfectly balanced binary trees.
%
% For more details on the implementation:
%
%   Chris Okasaki, "Purely Functional Random-Access Lists".
%   Functional Programming Languages and Computer Architecture,
%   June 1995, pp 86-95.
%
%---------------------------------------------------------------------------%

:- module ra_list.
:- interface.

:- import_module list.

:- type ra_list(T).

%---------------------------------------------------------------------------%
%
% Constructing ra_lists.
%

    % Return an empty random access list.
    %
:- pred init(ra_list(T)::uo) is det.

    % Return an random access list with the given head and tail.
    %
:- pred cons(T::in, ra_list(T)::in, ra_list(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Deconstructing ra_lists.
%

    % Return the head of the given random access list.
    % Fail if it is empty.
    %
:- pred head(ra_list(T)::in, T::out) is semidet.

    % Return the tail of the given random access list.
    % Fail if it is empty.
    %
:- pred tail(ra_list(T)::in, ra_list(T)::out) is semidet.

    % Return the head and the tail of the given random access list.
    % Fail if it is empty.
    %
:- pred head_tail(ra_list(T)::in, T::out, ra_list(T)::out) is semidet.

%---------------------------------------------------------------------------%
%
% Random access on ra_lists.
%

    % Return the item at the given index in the given random access list.
    % The number at the end of the predicate name gives the index of the first
    % element.
    %
    % Fail if the list is not long enough to have an element
    % at the given index.
    %
:- pred index0(ra_list(T)::in, int::in, T::out) is semidet.
:- pred index1(ra_list(T)::in, int::in, T::out) is semidet.

    % Return the item at the given index in the given random access list.
    % The number at the end of the predicate name gives the index of the first
    % element.
    %
    % Fail if the list is not long enough to have an element
    % at the given index.
    %
:- pred det_index0(ra_list(T)::in, int::in, T::out) is det.
:- pred det_index1(ra_list(T)::in, int::in, T::out) is det.

    % Replace the item at the given index in the given random access list.
    % The first element is at index 0.
    %
    % Fail if the list is not long enough to have an element
    % at the given index.
    %
:- pred update(int::in, T::in, ra_list(T)::in, ra_list(T)::out)
    is semidet.

%---------------------------------------------------------------------------%

    % Append two random access lists.
    %
:- pred append(ra_list(T)::in, ra_list(T)::in, ra_list(T)::out) is det.

    % Drop the given number of initial items from the given random access list.
    %
    % Returns the list unchanged if the number of elements to drop is zero
    % or negative.
    %
    % Fail if the list does not have at least that number of elements.
    %
:- pred drop(int::in, ra_list(T)::in, ra_list(T)::out) is semidet.

    % Convert a random access list to a plain list.
    %
:- pred ra_list_to_list(ra_list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type ra_list(T)
    --->    nil
            % The list is empty.
    ;       cons(int, ra_list_bintree(T), ra_list(T)).
            % cons(Size, BinTree, Rest):
            % The list contains the elements of BinTree, followed
            % the elements of Rest. BinTree must be a perfect balanced
            % binary tree, i.e. every level it has, including the last,
            % must be completely filled. The Size gives the number of elements
            % in BinTree. Due to BinTree being perflectly balanced, Size
            % must have the form (2^k) - 1.

:- type ra_list_bintree(T)
    --->    leaf(T)
            % This tree contains only this item.
    ;       node(T, ra_list_bintree(T), ra_list_bintree(T)).
            % node(I, TL, TR):
            % This tree contains item I, followed by the items in TL,
            % followed by the items in TR. In other words, the order of
            % the items in the tree is given a *preorder* traversal of
            % the binary tree.

%---------------------------------------------------------------------------%

:- pragma inline(pred(init/1)).

init(nil).

:- pragma inline(pred(cons/3)).

cons(X, List0, List) :-
    ( if
        List0 = cons(Size1, T1, cons(Size2, T2, Rest)),
        Size1 = Size2
    then
        NewSize = 1 + Size1 + Size2,
        List = cons(NewSize, node(X, T1, T2), Rest)
    else
        List = cons(1, leaf(X), List0)
    ).

%---------------------------------------------------------------------------%

:- pragma inline(pred(head/2)).

head(cons(_, leaf(X), _), X).
head(cons(_, node(X, _, _), _), X).

:- pragma inline(pred(tail/2)).

tail(cons(_, leaf(_), Tail), Tail).
tail(cons(Size, node(_, T1, T2), Rest), Tail) :-
    Size2 = Size // 2,
    Tail = cons(Size2, T1, cons(Size2, T2, Rest)).

:- pragma inline(pred(head_tail/3)).

head_tail(cons(_, leaf(X), Tail), X, Tail).
head_tail(cons(Size, node(X, T1, T2), Rest), X, Tail) :-
    Size2 = Size // 2,
    Tail = cons(Size2, T1, cons(Size2, T2, Rest)).

%---------------------------------------------------------------------------%

:- pragma inline(pred(index0/3)).

index0(List, I0, X) :-
    I0 >= 0,
    index0_list(List, I0, X).

:- pragma inline(pred(index1/3)).

index1(List, I1, X) :-
    I0 = I1 - 1,
    I0 >= 0,
    index0_list(List, I0, X).

:- pred index0_list(ra_list(T)::in, int::in, T::out) is semidet.

index0_list(cons(Size, T, Rest), I, X) :-
    ( if I < Size then
        index0_bintree(Size, T, I, X)
    else
        NewI = I - Size,
        index0_list(Rest, NewI, X)
    ).

:- pred index0_bintree(int::in, ra_list_bintree(T)::in, int::in,
    T::out) is semidet.

index0_bintree(_, leaf(X), 0, X).
index0_bintree(Size, node(X0, T1, T2), I, X) :-
    ( if I = 0 then
        X = X0
    else
        Size2 = Size // 2,
        ( if I =< Size2 then
            NewI = I - 1,
            index0_bintree(Size2, T1, NewI, X)
        else
            NewI = I - 1 - Size2,
            index0_bintree(Size2, T2, NewI, X)
        )
    ).

det_index0(List, I, X) :-
    ( if index0(List, I, XPrime) then
        X = XPrime
    else
        unexpected($pred, "index0 failed")
    ).

det_index1(List, I, X) :-
    ( if index1(List, I, XPrime) then
        X = XPrime
    else
        unexpected($pred, "index1 failed")
    ).

%---------------------------------------------------------------------------%

:- pragma inline(pred(update/4)).

update(I, X, List0, List) :-
    I >= 0,
    update_2(I, X, List0, List).

:- pred update_2(int::in, T::in, ra_list(T)::in, ra_list(T)::out)
    is semidet.

update_2(I, X, List0, List) :-
    List0 = cons(Size, T0, Rest), 
    ( if I < Size then
        update_bintree(Size, I, X, T0, T),
        List = cons(Size, T, Rest)
    else
        NewI = I - Size,
        update_2(NewI, X, Rest, List0),
        List = cons(Size, T0, List0)
    ).

:- pred update_bintree(int::in, int::in, T::in,
    ra_list_bintree(T)::in, ra_list_bintree(T)::out) is semidet.

update_bintree(_, 0, X, leaf(_), leaf(X)).
update_bintree(Size, I, X, node(X0, T1, T2), T) :-
    ( if I = 0 then
        T = node(X, T1, T2)
    else
        Size2 = Size // 2,
        ( if I =< Size2 then
            NewI = I - 1,
            update_bintree(Size2, NewI, X, T1, T0),
            T = node(X0, T0, T2)
        else
            NewI = I - 1 - Size2,
            update_bintree(Size2, NewI, X, T2, T0),
            T = node(X0, T1, T0)
        )
    ).

%---------------------------------------------------------------------------%

append(As, Bs, ABs) :-
    ( if head_tail(As, AH, AT) then
        append(AT, Bs, ATBs),
        cons(AH, ATBs, ABs)
    else
        ABs = Bs
    ).

%---------------------------------------------------------------------------%

drop(N, As, Bs) :-
    ( if N > 0 then
        As = cons(Size, _, Cs),
        ( if Size < N then
            N1 = N - Size,
            drop(N1, Cs, Bs)
        else
            slow_drop(N, As, Bs)
        )
    else
        As = Bs
    ).

:- pred slow_drop(int::in, ra_list(T)::in, ra_list(T)::out) is semidet.

slow_drop(N, As, Bs) :-
    ( if N > 0 then
        N1 = N - 1,
        tail(As, Cs),
        slow_drop(N1, Cs, Bs)
    else
        As = Bs
    ).

%---------------------------------------------------------------------------%

ra_list_to_list(RAList, List) :-
    ( if head_tail(RAList, RAHead, RATail) then
        ra_list_to_list(RATail, Tail),
        List = [RAHead | Tail]
    else
        List = []
    ).

%---------------------------------------------------------------------------%
:- end_module ra_list.
%---------------------------------------------------------------------------%
