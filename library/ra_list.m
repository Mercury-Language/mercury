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

    % Return a random access list containing only the given item.
    %
:- func singleton(T) = ra_list(T).

    % Return a random access list with the given head and tail.
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
% Tests on ra_lists.
%

    % Succeed iff the given random access list is empty.
    %
:- pred is_empty(ra_list(T)::in) is semidet.

    % Succeed iff the given random access list is not empty.
    %
:- pred is_not_empty(ra_list(T)::in) is semidet.

    % Succeed iff the given random access list contains only one item.
    % Return that item.
    %
:- pred is_singleton(ra_list(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Counting items in ra_lists.
%

    % Return the number of items in the given random access list.
    %
:- func length(ra_list(T)) = int.
:- pred length(ra_list(T)::in, int::out) is det.

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
:- pred update(int::in, T::in, ra_list(T)::in, ra_list(T)::out) is semidet.

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

%---------------------------------------------------------------------------%

    % Convert a list to a random access list.
    %
:- pred list_to_ra_list(list(T)::in, ra_list(T)::out) is det.

    % Convert a random access list to a plain list.
    %
:- pred ra_list_to_list(ra_list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%

    % map(F, L) = M:
    % map(P, L, M):
    %
    % Apply the function F or predicate P to transform the elements of L
    % into the elements of M.
    %
:- func map(func(X) = Y, ra_list(X)) = ra_list(Y).
:- pred map(pred(X, Y), ra_list(X), ra_list(Y)).
:- mode map(in(pred(in, out) is det), in, out) is det.
:- mode map(in(pred(in, out) is cc_multi), in, out) is cc_multi.
:- mode map(in(pred(in, out) is semidet), in, out) is semidet.
:- mode map(in(pred(in, out) is multi), in, out) is multi.
:- mode map(in(pred(in, out) is nondet), in, out) is nondet.
:- mode map(in(pred(in, in) is semidet), in, in) is semidet.

    % foldl(Func, List, Start) = End:
    % foldl(Pred, List, Start, End):
    %
    % Calls Func or Pred on each element of List, working left-to-right.
    % Each call to Func or Pred will have a pair of arguments that represent
    % respectively the current and the next value of a piece of state.
    % (Such current-next argument pairs are usually called an accumulator,
    % because the usual use case is that the successive calls to Func or Pred
    % accumulate pieces of information.) The initial value of the accumulator
    % is Start, each call to Func or Pred updates it to the next value, and
    % foldl returns its final value as End.
    %
:- func foldl(func(L, A) = A, ra_list(L), A) = A.
:- pred foldl(pred(L, A, A), ra_list(L), A, A).
:- mode foldl(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldl(in(pred(in, in, out) is multi), in, in, out) is multi.
:- mode foldl(in(pred(in, in, out) is nondet), in, in, out) is nondet.
:- mode foldl(in(pred(in, mdi, muo) is nondet), in, mdi, muo) is nondet.
:- mode foldl(in(pred(in, in, out) is cc_multi), in, in, out) is cc_multi.
:- mode foldl(in(pred(in, di, uo) is cc_multi), in, di, uo) is cc_multi.

    % foldr(Func, List, Start) = End:
    % foldr(Pred, List, Start, End):
    %
    % Calls Func or Pred on each element of List, working right-to-left.
    % Each call to Func or Pred will have a pair of arguments that represent
    % respectively the current and the next value of a piece of state.
    % (Such current-next argument pairs are usually called an accumulator,
    % because the usual use case is that the successive calls to Func or Pred
    % accumulate pieces of information.) The initial value of the accumulator
    % is Start, each call to Func or Pred updates it to the next value, and
    % foldl returns its final value as End.
    %
:- func foldr(func(L, A) = A, ra_list(L), A) = A.
:- pred foldr(pred(L, A, A), ra_list(L), A, A).
:- mode foldr(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.
:- mode foldr(in(pred(in, in, out) is multi), in, in, out) is multi.
:- mode foldr(in(pred(in, in, out) is nondet), in, in, out) is nondet.
:- mode foldr(in(pred(in, mdi, muo) is nondet), in, mdi, muo) is nondet.
:- mode foldr(in(pred(in, di, uo) is cc_multi), in, di, uo) is cc_multi.
:- mode foldr(in(pred(in, in, out) is cc_multi), in, in, out) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Okasaki's paper represents random access lists as a list of
    % <bintree_size, bintree> pairs. We use a fat-list-style variation
    % of that representation, in which a single three-argument cons cell
    % replaces both a standard two-argument cons cell and a pair cell
    % in Okasaki's representation. This requires fewer memory allocations
    % when constructing ra_lists, and fewer dependent loads when traversing
    % existing ra_lists. (It may also require less memory, though with Boehm,
    % each three argument cons cell requires a four word block.)
    % 
:- type ra_list(T)
    --->    nil
            % The list is empty.
    ;       cons(int, ra_list_bintree(T), ra_list(T)).
            % cons(Size, BinTree, Rest):
            %
            % The list contains the elements of BinTree, followed
            % the elements of Rest. Each BinTree must be a perfect balanced
            % binary tree, which means that all their levels, including their
            % lowest levels, must be completely filled. The Size in each cons
            % gives the number of elements in the BinTree next to it.
            %
            % Due to BinTrees being perfectly balanced, each Size must have
            % the form (2^k) - 1; Okasaki calls such integers "skew-binary
            % terms".
            %
            % With one exception, every ra_list_bintree in the third field
            % of a cons must be strictly bigger than the ra_list_bintree
            % in the second field. The exception is that the first two cons
            % cells in an ra_list may contain bintrees of equal size.

:- type ra_list_bintree(T)
    --->    leaf(T)
            % This tree contains only this item.
    ;       node(T, ra_list_bintree(T), ra_list_bintree(T)).
            % node(I, TL, TR):
            % This tree contains item I, followed by the items in TL,
            % followed by the items in TR. In other words, the order of
            % the items in the tree is given by the *preorder* traversal of
            % the binary tree, not the usual *inorder* traversal.

%---------------------------------------------------------------------------%

:- pragma inline(pred(init/1)).

init(nil).

:- pragma inline(func(singleton/1)).

singleton(X) = cons(1, leaf(X), nil).

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

is_empty(RA) :-
    RA = nil.

is_not_empty(RA) :-
    RA = cons(_, _, _).

is_singleton(RA, X) :-
    RA = cons(1, leaf(X), nil).

%---------------------------------------------------------------------------%

length(RAList) = Len :-
    length_acc(RAList, 0, Len).

length(RAList, Len) :-
    length_acc(RAList, 0, Len).

:- pred length_acc(ra_list(T)::in, int::in, int::out) is det.

length_acc(nil, !Len).
length_acc(cons(BinTreeSize, _BinTree, Tail), !Len) :-
    !:Len = !.Len + BinTreeSize,
    length_acc(Tail, !Len).

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

list_to_ra_list(List, RAList) :-
    % We reverse the list and count its elements using tail recursive code.
    reverse_prepend_and_count(List, [], RevList, 0, Size),
    ( if Size = 0 then
        RAList = nil
    else
        % Find the size of the biggest bintree in the RAList we need to
        % construct.
        get_max_bintree_size(Size, 1, MaxSkewBinary),
        % Construct RAList back to front, by repeatedly "chomping off"
        % a bintree's worth of items from RevList.
        acc_ra_list(RevList, Size, MaxSkewBinary, nil, RAList)
    ).

    % Reverse the given list (using non-native reverse) and count
    % the number of items in the list at the same time.
    %
:- pred reverse_prepend_and_count(list(T)::in, list(T)::in, list(T)::out,
    int::in, int::out) is det.

reverse_prepend_and_count([], L, L, !Size).
reverse_prepend_and_count([H | T], L0, L, !Size) :-
    !:Size = !.Size + 1,
    reverse_prepend_and_count(T, [H | L0], L, !Size).

    % Given CurSkewBinary, which is a skew-binary term that is guaranteed
    % not to be bigger than Size, return the largest skew-binary term
    % that is not bigger than Size.
    %
    % This will be the size of the last (and therefore largest) bintree
    % in the ra_list we need to construct for a list with Size items.
    %
:- pred get_max_bintree_size(int::in, int::in, int::out) is det.

get_max_bintree_size(Size, CurSkewBinary, MaxSkewBinary) :-
    NextSkewBinary = (2 * CurSkewBinary) + 1,
    ( if Size >= NextSkewBinary then
        get_max_bintree_size(Size, NextSkewBinary, MaxSkewBinary)
    else
        MaxSkewBinary = CurSkewBinary
    ).

    % acc_ra_list(RevList0, SizeLeftover0, SkewBinary0, RAList0, RAList):
    %
    % We have processed some of the items at the end of the original list
    % (and hence at the start of its reversed form) into RAList0. Convert
    % the remaining elements of the reversed list, RevList0, into one or more
    % cons cells to put in front of RAList0. SizeLeftover0 gives the number
    % of items in RevList0, and SkewBinary0, which must be a skew-binary term,
    % gives the size of the largest bintree in the cons cells we are to put
    % in front of RAList0.
    %
:- pred acc_ra_list(list(T)::in, int::in, int::in,
    ra_list(T)::in, ra_list(T)::out) is det.

acc_ra_list(RevList0, SizeLeftover0, SkewBinary0, RAList0, RAList) :-
    ( if SizeLeftover0 > SkewBinary0 then
        sized_prefix_to_bintree(SkewBinary0, BinTree0, RevList0, RevList1),
        SizeLeftover1 = SizeLeftover0 - SkewBinary0,
        SkewBinary1 = SkewBinary0 // 2,
        RAList1 = cons(SkewBinary0, BinTree0, RAList0),
        ( if SizeLeftover1 < SkewBinary0 then
            % This is the usual case when the cons cells in RevList1 and the
            % next cons cell we put immediately in front of it contain
            % bintrees of different sizes: the sum of all the (2^k) - 1
            % over k in 1..(i-1) is strictly less than (2^i) - 1.
            %
            % For example:
            %
            % for i=2: 1 < 3
            % for i=3: 1 + 3 < 7
            % for i=4: 1 + 3 + 7 < 15
            acc_ra_list(RevList1, SizeLeftover1, SkewBinary1, RAList1, RAList)
        else if SizeLeftover1 = SkewBinary0 then
            % This is how acc_ra_list terminates when the RAList we construct
            % starts with two cons cells that contain bintrees of equal sizes.
            sized_prefix_to_bintree(SkewBinary0, BinTree1, RevList1, RevList),
            RAList = cons(SkewBinary0, BinTree1, RAList0),
            expect(unify(RevList, []), $pred, "RevList != []")
        else
            unexpected($pred, "SizeLeftover0 > SkewBinary0")
        )
    else if SizeLeftover0 < SkewBinary0 then
        SkewBinary1 = SkewBinary0 // 2,
        acc_ra_list(RevList0, SizeLeftover0, SkewBinary1, RAList0, RAList)
    else
        % This is how acc_ra_list terminates when the RAList we construct
        % starts with two cons cells that contain bintrees of non-equal sizes.
        sized_prefix_to_bintree(SkewBinary0, BinTree, RevList0, RevList1),
        SizeLeftover1 = SizeLeftover0 - SkewBinary0,
        RAList = cons(SkewBinary0, BinTree, RAList0),
        expect(unify(RevList1, []), $pred, "RevList1 != []"),
        expect(unify(SizeLeftover1, 0), $pred, "SizeLeftover1 != 0")
    ).

    % sized_prefix_to_bintree(BinTreeSize, BinTree, !RevList) :-
    %
    % Take the first BinTreeSize elements of !RevList, and construct
    % a perfectly balanced binary tree out of them, while undoing the reversal.
    %
    % BinTreeSize must be a skew-binary term.
    %
:- pred sized_prefix_to_bintree(int::in, ra_list_bintree(T)::out,
    list(T)::in, list(T)::out) is det.

sized_prefix_to_bintree(BinTreeSize, BinTree, !RevList) :-
    ( if BinTreeSize = 1 then
        get_next_item_from_rev_list(Head, !RevList),
        BinTree = leaf(Head)
    else
        % Given BinTreeSize = (2^k) - 1, this sets BinTreeSize2
        % to (2^(k-1)) - 1, i.e. to the next lower skew-binary term.
        BinTreeSize2 = BinTreeSize // 2,
        sized_prefix_to_bintree(BinTreeSize2, R, !RevList),
        sized_prefix_to_bintree(BinTreeSize2, L, !RevList),
        get_next_item_from_rev_list(Head, !RevList),
        BinTree = node(Head, L, R)
    ).

:- pred get_next_item_from_rev_list(T::out, list(T)::in, list(T)::out) is det.

get_next_item_from_rev_list(Head, !RevList) :-
    (
        !.RevList = [Head | !:RevList]
    ;
        !.RevList = [],
        unexpected($pred, "!.RevList = []")
    ).

%---------------------------------------------------------------------------%

ra_list_to_list(RAList, List) :-
    (
        RAList = nil,
        List = []
    ;
        RAList = cons(_Size, BinTree, RAListTail),
        ra_list_to_list(RAListTail, Tail),
        ra_list_to_list_acc_bintree(BinTree, Tail, List)
    ).

:- pred ra_list_to_list_acc_bintree(ra_list_bintree(T)::in,
    list(T)::in, list(T)::out) is det.

ra_list_to_list_acc_bintree(BinTree, Tail0, List) :-
    (
        BinTree = leaf(X),
        List = [X | Tail0]
    ;
        BinTree = node(X, L, R),
        ra_list_to_list_acc_bintree(R, Tail0, Tail1),
        ra_list_to_list_acc_bintree(L, Tail1, Tail2),
        List = [X | Tail2]
    ).

%---------------------------------------------------------------------------%

map(_F, nil) = nil.
map(F, cons(Size, BinTree0, RAList0)) = cons(Size, BinTree, RAList) :-
    BinTree = map_bintree(F, BinTree0),
    RAList = map(F, RAList0).

map(_P, nil, nil).
map(P, cons(Size, BinTree0, RAList0), cons(Size, BinTree, RAList)) :-
    map_bintree(P, BinTree0, BinTree),
    map(P, RAList0, RAList).

:- func map_bintree(func(X) = Y, ra_list_bintree(X)) = ra_list_bintree(Y).
:- pred map_bintree(pred(X, Y), ra_list_bintree(X), ra_list_bintree(Y)).
:- mode map_bintree(in(pred(in, out) is det), in, out) is det.
:- mode map_bintree(in(pred(in, out) is cc_multi), in, out) is cc_multi.
:- mode map_bintree(in(pred(in, out) is semidet), in, out) is semidet.
:- mode map_bintree(in(pred(in, out) is multi), in, out) is multi.
:- mode map_bintree(in(pred(in, out) is nondet), in, out) is nondet.
:- mode map_bintree(in(pred(in, in) is semidet), in, in) is semidet.

map_bintree(F, BinTree0) = BinTree :-
    (
        BinTree0 = leaf(X0),
        X = F(X0),
        BinTree = leaf(X)
    ;
        BinTree0 = node(X0, L0, R0),
        X = F(X0),
        L = map_bintree(F, L0),
        R = map_bintree(F, R0),
        BinTree = node(X, L, R)
    ).

map_bintree(P, BinTree0, BinTree) :-
    (
        BinTree0 = leaf(X0),
        P(X0, X),
        BinTree = leaf(X)
    ;
        BinTree0 = node(X0, L0, R0),
        P(X0, X),
        map_bintree(P, L0, L),
        map_bintree(P, R0, R),
        BinTree = node(X, L, R)
    ).

%---------------------------------------------------------------------------%

foldl(_F, nil, !.A) = !.A.
foldl(F, cons(_Size, BinTree, RAList), !.A) = !:A :-
    !:A = foldl_bintree(F, BinTree, !.A),
    !:A = foldl(F, RAList, !.A).

foldl(_P, nil, !A).
foldl(P, cons(_Size, BinTree, RAList), !A) :-
    foldl_bintree(P, BinTree, !A),
    foldl(P, RAList, !A).

:- func foldl_bintree(func(L, A) = A, ra_list_bintree(L), A) = A.
:- pred foldl_bintree(pred(L, A, A), ra_list_bintree(L), A, A).
:- mode foldl_bintree(in(pred(in, in, out) is det),
    in, in, out) is det.
:- mode foldl_bintree(in(pred(in, mdi, muo) is det),
    in, mdi, muo) is det.
:- mode foldl_bintree(in(pred(in, di, uo) is det),
    in, di, uo) is det.
:- mode foldl_bintree(in(pred(in, in, out) is semidet),
    in, in, out) is semidet.
:- mode foldl_bintree(in(pred(in, mdi, muo) is semidet),
    in, mdi, muo) is semidet.
:- mode foldl_bintree(in(pred(in, di, uo) is semidet),
    in, di, uo) is semidet.
:- mode foldl_bintree(in(pred(in, in, out) is multi),
    in, in, out) is multi.
:- mode foldl_bintree(in(pred(in, in, out) is nondet),
    in, in, out) is nondet.
:- mode foldl_bintree(in(pred(in, mdi, muo) is nondet),
    in, mdi, muo) is nondet.
:- mode foldl_bintree(in(pred(in, in, out) is cc_multi),
    in, in, out) is cc_multi.
:- mode foldl_bintree(in(pred(in, di, uo) is cc_multi),
    in, di, uo) is cc_multi.

foldl_bintree(F, BinTree, !.A) = !:A :-
    (
        BinTree = leaf(X),
        !:A = F(X, !.A)
    ;
        BinTree = node(X, L, R),
        !:A = F(X, !.A),
        !:A = foldl_bintree(F, L, !.A),
        !:A = foldl_bintree(F, R, !.A)
    ).

foldl_bintree(P, BinTree, !A) :-
    (
        BinTree = leaf(X),
        P(X, !A)
    ;
        BinTree = node(X, L, R),
        P(X, !A),
        foldl_bintree(P, L, !A),
        foldl_bintree(P, R, !A)
    ).

%---------------------------------------------------------------------------%

foldr(_F, nil, !.A) = !.A.
foldr(F, cons(_Size, BinTree, RAList), !.A) = !:A :-
    !:A = foldr(F, RAList, !.A),
    !:A = foldr_bintree(F, BinTree, !.A).

foldr(_P, nil, !A).
foldr(P, cons(_Size, BinTree, RAList), !A) :-
    foldr(P, RAList, !A),
    foldr_bintree(P, BinTree, !A).

:- func foldr_bintree(func(L, A) = A, ra_list_bintree(L), A) = A.
:- pred foldr_bintree(pred(L, A, A), ra_list_bintree(L), A, A).
:- mode foldr_bintree(in(pred(in, in, out) is det),
    in, in, out) is det.
:- mode foldr_bintree(in(pred(in, mdi, muo) is det),
    in, mdi, muo) is det.
:- mode foldr_bintree(in(pred(in, di, uo) is det),
    in, di, uo) is det.
:- mode foldr_bintree(in(pred(in, in, out) is semidet),
    in, in, out) is semidet.
:- mode foldr_bintree(in(pred(in, mdi, muo) is semidet),
    in, mdi, muo) is semidet.
:- mode foldr_bintree(in(pred(in, di, uo) is semidet),
    in, di, uo) is semidet.
:- mode foldr_bintree(in(pred(in, in, out) is multi),
    in, in, out) is multi.
:- mode foldr_bintree(in(pred(in, in, out) is nondet),
    in, in, out) is nondet.
:- mode foldr_bintree(in(pred(in, mdi, muo) is nondet),
    in, mdi, muo) is nondet.
:- mode foldr_bintree(in(pred(in, in, out) is cc_multi),
    in, in, out) is cc_multi.
:- mode foldr_bintree(in(pred(in, di, uo) is cc_multi),
    in, di, uo) is cc_multi.

foldr_bintree(F, BinTree, !.A) = !:A :-
    (
        BinTree = leaf(X),
        !:A = F(X, !.A)
    ;
        BinTree = node(X, L, R),
        !:A = foldr_bintree(F, R, !.A),
        !:A = foldr_bintree(F, L, !.A),
        !:A = F(X, !.A)
    ).

foldr_bintree(P, BinTree, !A) :-
    (
        BinTree = leaf(X),
        P(X, !A)
    ;
        BinTree = node(X, L, R),
        foldr_bintree(P, R, !A),
        foldr_bintree(P, L, !A),
        P(X, !A)
    ).

%---------------------------------------------------------------------------%
:- end_module ra_list.
%---------------------------------------------------------------------------%
