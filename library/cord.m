%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% Copyright (C) 2013-2018, 2021-2022, 2024-2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: cord.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
% Stability: high.
%
% Like lists, cords contain a sequence of elements. The difference is that
% many operations that construct cords (such as appending two cords together,
% or adding a new element to the end of a cord) are O(1) operations, not O(N).
% In general, if you want to construct a list in any order other than
% strictly back-to-front, then you should consider constructing a cord instead,
% and then converting the final cord to a list.
%
% The reason why such lower asymptotic complexities are possible for many
% operations is that cords are essentially binary trees that store elements
% in their leaf nodes.
%
% The price of lower complexity for cord-construction operations is
% (a) higher complexity for some inspection operations, such as head_tail/3,
% and (b) higher constant factors for most operations.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module cord.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % Conceptually, a cord contains a list of elements of type T.
    %
    % Cords that contain the same elements in the same order will not
    % necessarily have the same representation. Therefore it is possible
    % that they may not unify, and that comparing them may return a result
    % other than "equal".
    %
    % The exception to this rule is that the empty cord does have a
    % unique representation.
    %
    % You can test two cords for equality using the cord.equal predicate below.
    %
:- type cord(T).

%---------------------------------------------------------------------------%
%
% Constructing a cord from scratch.
%

    % The init and empty functions do the same job: return the empty cord.
    %
    % list(init) = [].
    % list(empty) = [].
    %
:- func init = cord(T).
:- func empty = cord(T).

    % Return a cord containing just the given element.
    %
    % list(singleton(X)) = [X].
    %
:- func singleton(T) = cord(T).

%---------------------------------------------------------------------------%
%
% Constructing a new cord from one existing cord.
%

    % cons(Element, Cord0) = Cord:
    % cons(Element, Cord0, Cord):
    %
    % Return Cord, which is the cord you get when you add Element
    % to the front of Cord0.
    %
    % list(cons(Element, Cord0)) = [Element | list(Cord0)]
    %
    % This is an O(1) operation.
    %
:- func cons(T, cord(T)) = cord(T).
:- pred cons(T::in, cord(T)::in, cord(T)::out) is det.

    % cons_list(List, Cord0) = Cord:
    % cons_list(List, Cord0, Cord):
    %
    % Return Cord, which is the cord you get when you add List
    % to the front of Cord0.
    %
    % list(cons_list(List, Cord0)) = List ++ list(Cord0)
    %
    % This is an O(1) operation.
    %
:- func cons_list(list(T), cord(T)) = cord(T).
:- pred cons_list(list(T)::in, cord(T)::in, cord(T)::out) is det.

    % snoc(Cord0, Element) = Cord:
    % snoc(Element, Cord0, Cord):
    %
    % Return Cord, which is the cord you get when you add Element
    % to the end of Cord0. (The argument order of the predicate version
    % simplifies its use when the cord is represented by a state variable.)
    %
    % list(snoc(Cord0, Element)) = list(Cord0) ++ [Element]
    %
    % This is an O(1) operation.
    %
:- func snoc(cord(T), T) = cord(T).
:- pred snoc(T::in, cord(T)::in, cord(T)::out) is det.

    % snoc_list(Cord0, List) = Cord:
    % snoc_list(List, Cord0, Cord):
    %
    % Return Cord, which is the cord you get when you add List
    % to the end of Cord0. (The argument order of the predicate version
    % simplifies its use when the cord is represented by a state variable.)
    %
    % list(snoc_list(Cord0, List)) = list(Cord0) ++ List
    %
    % This is an O(1) operation.
    %
:- func snoc_list(cord(T), list(T)) = cord(T).
:- pred snoc_list(list(T)::in, cord(T)::in, cord(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Constructing a new cord from two or more existing cords.
%

    % CA ++ CB = C:
    %
    % Return C, which is the cord you get when you append CB to the end of CA.
    %
    % list(CA ++ CB) = list(CA) ++ list(CB)
    %
    % This is an O(1) operation.
    % (With lists, the complexity would be O(len(CA)).)
    %
:- func cord(T) ++ cord(T) = cord(T).

    % Append together a list of cords.
    %
:- func cord_list_to_cord(list(cord(T))) = cord(T).

    % Reverse the given list (of cords), and then
    % append together the resulting list of cords.
    %
:- func rev_cord_list_to_cord(list(cord(T))) = cord(T).

    % Cord = condense(CordOfCords):
    %
    % Cord is the result of concatenating all the elements of CordOfCords.
    %
:- func condense(cord(cord(T))) = cord(T).

%---------------------------------------------------------------------------%
%
% Simple tests on cords.
%

    % Succeed if-and-only-if the given cord is empty.
    %
:- pred is_empty(cord(T)::in) is semidet.

    % Succeed if-and-only-if the given cord is not empty.
    %
:- pred is_non_empty(cord(T)::in) is semidet.

    % If the given cord contains exactly one element, then return that element.
    % Otherwise, fail.
    %
:- pred is_singleton(cord(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Getting single elements out of cords.
%

    % head(Cord, Head):
    % get_first(Cord, Head):
    %
    % Return just the first element in Cord, if Cord contains any elements.
    % Otherwise, fail.
    %
    %     head(Cord, Head)  =>  some [Tail]: list(Cord) = [Head] ++ Tail.
    % not head(Cord, _)     =>  Cord = empty
    %
    % This is an O(n) operation.
    %
:- pred head(cord(T)::in, T::out) is semidet.
:- pred get_first(cord(T)::in, T::out) is semidet.

    % head_tail(Cord, Head, Tail):
    %
    % If the cord Cord is not empty, then return its first element as Head,
    % and the cord containing all the remaining elements as Tail.
    % If the cord is empty, then fail.
    %
    %     head_tail(Cord, Head, Tail)  =>  list(Cord) = [Head | list(Tail)]
    % not head_tail(Cord, _, _)        =>  Cord = empty
    %
    % This is an O(n) operation, although traversing an entire cord with
    % head_tail/3 gives O(1) amortized cost for each call.
    %
:- pred head_tail(cord(T)::in, T::out, cord(T)::out) is semidet.

    % get_last(Cord, Last):
    %
    % Return just the last element in Cord, if Cord contains any elements.
    % Otherwise, fail.
    %
    %     get_last(Cord, Last)  =>  some [List]: list(Cord) = List ++ [Last].
    % not get_last(Cord, _)     =>  Cord = empty
    %
    % This is an O(n) operation.
    %
:- pred get_last(cord(T)::in, T::out) is semidet.

    % split_last(Cord, Prev, Last):
    %
    % If the cord Cord is not empty, then return its last element as Last,
    % and the cord containing all the previous elements as Prev.
    % If the cord is empty, then fail.
    %
    %     split_last(Cord, Prev, Last)  =>  list(Cord) = list(Prev) ++ [Last].
    % not split_last(Cord, _, _)        =>  Cord = empty
    %
    % This is an O(n) operation, although traversing an entire cord with
    % split_last/3 gives O(1) amortized cost for each call.
    %
:- pred split_last(cord(T)::in, cord(T)::out, T::out) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on whole cords.
%

    % length(C) = list.length(list(C))
    %
    % This is an O(n) operation.
    %
:- func length(cord(T)) = int.

    % member(X, C) <=> list.member(X, list(C)).
    %
:- pred member(T::out, cord(T)::in) is nondet.

    % equal(CA, CB):
    %
    % Succeed if-and-only-if CA and CB contain the same elements
    % in the same order.
    %
    % equal(CA, CB)  <=>  list(CA) = list(CB).
    % (Note: the current implementation works exactly this way.)
    %
    % This is an O(n) operation where n = length(CA) + length(CB).
    %
:- pred equal(cord(T)::in, cord(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Converting lists to cords.
%

    % from_list(List) = Cord:
    %
    % Return a cord containing the same elements in the same order as List.
    %
    % list(from_list(Xs)) = Xs
    %
    % This is an O(1) operation.
    %
:- func from_list(list(T)) = cord(T).

%---------------------------------------------------------------------------%
%
% Converting cords to lists.
%

    % list(Cord) = List:
    % to_list(Cord) = List:
    %
    % Return a list containing the same elements in the same order as Cord.
    %
    % The list of data in a cord:
    %
    %   list(empty        ) = []
    %   list(from_list(Xs)) = Xs
    %   list(cons(X, C)   ) = [X | list(C)]
    %   list(TA ++ TB     ) = list(TA) ++ list(TB)
    %
:- func list(cord(T)) = list(T).
:- func to_list(cord(T)) = list(T).

    % rev_list(Cord) = RevList:
    % to_rev_list(Cord) = RevList:
    %
    % Return a list containing the same elements as Cord,
    % but in the reverse order.
    %
    % rev_list(Cord) = list.reverse(list(Cord)).
    %
:- func rev_list(cord(T)) = list(T).
:- func to_rev_list(cord(T)) = list(T).

    % Append together a list of cords, and return the result as a list.
    %
:- func cord_list_to_list(list(cord(T))) = list(T).

    % Reverse the given list (of cords), append together
    % the resulting list of cords, and return it as a list.
    %
:- func rev_cord_list_to_list(list(cord(T))) = list(T).

%---------------------------------------------------------------------------%
%
% Some standard higher order operations.
%

    % find_first_match(Pred, Cord, FirstMatch):
    %
    % Return as FirstMatch the first element E in Cord
    % for which Pred(E) is true. If there is no such element, fail.
    %
:- pred find_first_match(pred(T)::in(pred(in) is semidet),
    cord(T)::in, T::out) is semidet.

    % map(Func, Cord) = MappedCord:
    %
    % Apply Func to every element of Cord, and return the result.
    %
    % list(map(Func, Cord)) = list.map(Func, list(Cord))
    %
:- func map(func(T) = U, cord(T)) = cord(U).

    % map_pred(Pred, Cord, MappedCord):
    %
    % Apply Pred to every element of Cord, and return the result.
    %
    % cord.map_pred(Pred, Cord, MappedCord), MappedList = cord.list(MappedCord)
    % is equivalent to
    % list.map(Pred, cord.list(Cord), MappedList)
    %
:- pred map_pred(pred(T, U)::in(pred(in, out) is det),
    cord(T)::in, cord(U)::out) is det.

    % filter(Pred, Cord, TrueCord):
    %
    % For each member E of Cord,
    % - if Pred(E) is true, then include E in TrueCord.
    %
    % The order of the included elements is preserved.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    cord(T)::in, cord(T)::out) is det.

    % filter(Pred, Cord, TrueCord, FalseCord):
    %
    % For each member E of Cord,
    % - if Pred(E) is true, then include E in TrueCord.
    % - if Pred(E) is false, then include E in FalseCord.
    %
    % The order of the included elements is preserved.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    cord(T)::in, cord(T)::out, cord(T)::out) is det.

%---------------------------------------------------------------------------%
%
% Foldl operations.
%

    % foldl(F, C, A) = list.foldl(F, list(C), A).
    %
:- func foldl(func(T, A) = A, cord(T), A) = A.

    % foldl_pred(P, C, !AccA)
    %
    % Equivalent to list.foldl(P, list(C), !AccA), but faster.
    %
:- pred foldl_pred(pred(T, A, A), cord(T), A, A).
:- mode foldl_pred(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl_pred(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl_pred(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl_pred(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl_pred(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl_pred(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

    % foldl2(P, C, !AccA, !AccB)
    %
    % Equivalent to list.foldl2(P, list(C), !AccA, !AccB), but faster.
    %
:- pred foldl2(pred(T, A, A, B, B), cord(T), A, A, B, B).
:- mode foldl2(in(pred(in, in, out, in, out) is det),
    in, in, out, in, out) is det.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is det),
    in, in, out, mdi, muo) is det.
:- mode foldl2(in(pred(in, in, out, di, uo) is det),
    in, in, out, di, uo) is det.
:- mode foldl2(in(pred(in, in, out, in, out) is semidet),
    in, in, out, in, out) is semidet.
:- mode foldl2(in(pred(in, in, out, mdi, muo) is semidet),
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(in(pred(in, in, out, di, uo) is semidet),
    in, in, out, di, uo) is semidet.

    % foldl3(P, C, !AccA, !AccB, !AccC)
    %
    % Equivalent to list.foldl3(P, list(C), !AccA, !AccB, !AccC), but faster.
    %
:- pred foldl3(pred(T, A, A, B, B, C, C), cord(T), A, A, B, B, C, C).
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is det),
    in, in, out, in, out, in, out) is det.
:- mode foldl3(in(pred(in, in, out, in, out, mdi, muo) is det),
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(in(pred(in, in, out, in, out, in, out) is semidet),
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(in(pred(in, in, out, in, out, mdi, muo) is semidet),
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(in(pred(in, in, out, in, out, di, uo) is semidet),
    in, in, out, in, out, di, uo) is semidet.

%---------------------------------------------------------------------------%
%
% Foldr operations.
%

    % foldr(F, C, A) = list.foldr(F, list(C), A).
    %
:- func foldr(func(T, A) = A, cord(T), A) = A.

    % foldr_pred(P, C, !AccA):
    %
    % Equivalent to list.foldr(P, list(C), !AccA), but faster.
    %
:- pred foldr_pred(pred(T, A, A), cord(T), A, A).
:- mode foldr_pred(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr_pred(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr_pred(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr_pred(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr_pred(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr_pred(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

    % foldr2(P, C, !AccA, !AccB):
    %
    % Equivalent to list.foldr2(P, list(C), !AccA, !AccB), but faster.
    %
:- pred foldr2(pred(T, A, A, B, B), cord(T), A, A, B, B).
:- mode foldr2(in(pred(in, in, out, in, out) is det), in, in, out,
    in, out) is det.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is det), in, in, out,
    mdi, muo) is det.
:- mode foldr2(in(pred(in, in, out, di, uo) is det), in, in, out,
    di, uo) is det.
:- mode foldr2(in(pred(in, in, out, in, out) is semidet), in, in, out,
    in, out) is semidet.
:- mode foldr2(in(pred(in, in, out, mdi, muo) is semidet), in, in, out,
    mdi, muo) is semidet.
:- mode foldr2(in(pred(in, in, out, di, uo) is semidet), in, in, out,
    di, uo) is semidet.

    % foldr3(P, C, !AccA, !AccB, !AccC):
    %
    % Equivalent to list.foldr3(P, list(C), !AccA, !AccB, !AccC), but faster.
    %
:- pred foldr3(pred(T, A, A, B, B, C, C), cord(T), A, A, B, B, C, C).
:- mode foldr3(in(pred(in, in, out, in, out, in, out) is det), in,
    in, out, in, out, in, out) is det.
:- mode foldr3(in(pred(in, in, out, in, out, mdi, muo) is det), in,
    in, out, in, out, mdi, muo) is det.
:- mode foldr3(in(pred(in, in, out, in, out, di, uo) is det), in,
    in, out, in, out, di, uo) is det.
:- mode foldr3(in(pred(in, in, out, in, out, in, out) is semidet), in,
    in, out, in, out, in, out) is semidet.
:- mode foldr3(in(pred(in, in, out, in, out, mdi, muo) is semidet), in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldr3(in(pred(in, in, out, in, out, di, uo) is semidet), in,
    in, out, in, out, di, uo) is semidet.

%---------------------------------------------------------------------------%
%
% Map_foldl operations.
%

    % map_foldl(P, CA, CB, !Acc):
    %
    % This predicate calls P on each element of the input cord, working
    % left to right. Each call to P transforms an element of the input cord
    % to the corresponding element of the output cord, and updates the
    % accumulator.
    %
:- pred map_foldl(pred(T1, T2, A, A), cord(T1), cord(T2), A, A).
:- mode map_foldl(in(pred(in, out, in, out) is det), in, out, in, out)
    is det.
:- mode map_foldl(in(pred(in, out, mdi, muo) is det), in, out, mdi, muo)
    is det.
:- mode map_foldl(in(pred(in, out, di, uo) is det), in, out, di, uo)
    is det.
:- mode map_foldl(in(pred(in, out, in, out) is semidet), in, out, in, out)
    is semidet.
:- mode map_foldl(in(pred(in, out, mdi, muo) is semidet), in, out, mdi, muo)
    is semidet.
:- mode map_foldl(in(pred(in, out, di, uo) is semidet), in, out, di, uo)
    is semidet.

    % As above, but with two accumulators.
    %
:- pred map_foldl2(pred(T1, T2, A, A, B, B)::
    in(pred(in, out, in, out, in, out) is det),
    cord(T1)::in, cord(T2)::out, A::in, A::out, B::in, B::out) is det.

    % As above, but with three accumulators.
    %
:- pred map_foldl3(pred(T1, T2, A, A, B, B, C, C)::
    in(pred(in, out, in, out, in, out, in, out) is det),
    cord(T1)::in, cord(T2)::out, A::in, A::out, B::in, B::out, C::in, C::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

% The original implementation of the cord/1 type had four function symbols
% in one type: empty, unit, node and branch. However, this representation
% requires code to handle the "empty" case when we look at *every* part
% of the cord. This code is annoying to write, annoying to read, and the
% tests for empty at these points also reduce performance.

:- type cord(T)
    --->    empty_cord
    ;       nonempty_cord(cord_node(T)).

    % We used to have unit_node(T) as one of three function symbols
    % in the cord_node(T) type. It was equivalent to list_node(T, list(T))
    % with an empty list as the second argument, so it could be deleted.
    %
    % Having unit_node(T) can make the representation of some cords smaller,
    % and its presence can also avoid the cost of filling in and later reading
    % the second argument of list_node/2. However, it increases the cost of
    % all traversals of cord nodes by forcing a choice between three
    % function symbols, not two. In some cases, but not all, using list_node
    % instead of unit_node will then require a test of the second argument
    % for nil vs cons, so using only list_nodes lowers the overall number
    % of tests required.
    %
    % The extra memory accesses required by using only list_nodes will
    % always be cache hits, since the Boehm allocator always puts two-word
    % heap cells at two-word boundaries, so the second word will always be
    % in the same cache block as the first word. This makes them cheap.
    % On the other hand, branch prediction for the extra tests required
    % by the presence of unit_nodes will necessarily be less than perfect,
    % and mispredicted branches have been the other one of the two most
    % costly operations (besides cache misses) on CPUs for many years now.
    %
    % The overall effect is that deleting the unit_node function symbol
    % gets a speedup on tools/speedtest of approximately 0.7%.
:- type cord_node(T)
    --->    list_node(T, list(T))
    ;       branch_node(cord_node(T), cord_node(T)).

%---------------------------------------------------------------------------%

init = empty_cord.

empty = empty_cord.

singleton(X) = nonempty_cord(list_node(X, [])).

%---------------------------------------------------------------------------%

cons(X, C) = XC :-
    (
        C = empty_cord,
        XC = nonempty_cord(list_node(X, []))
    ;
        C = nonempty_cord(N),
        XC = nonempty_cord(branch_node(list_node(X, []), N))
    ).

cons(X, !C) :-
    !:C = cons(X, !.C).

%---------------------%

cons_list(L, C0) = C :-
    cons_list(L, C0, C).

cons_list(L, !C) :-
    (
        L = []
    ;
        L = [H | T],
        LN = list_node(H, T),
        (
            !.C = empty_cord,
            !:C = nonempty_cord(LN)
        ;
            !.C = nonempty_cord(CN0),
            CN = branch_node(LN, CN0),
            !:C = nonempty_cord(CN)
        )
    ).

%---------------------%

snoc(C, X) = CX :-
    (
        C = empty_cord,
        CX = nonempty_cord(list_node(X, []))
    ;
        C = nonempty_cord(N),
        CX = nonempty_cord(branch_node(N, list_node(X, [])))
    ).

snoc(X, !C) :-
    !:C = snoc(!.C, X).

%---------------------%

snoc_list(C0, L) = C :-
    snoc_list(L, C0, C).

snoc_list(L, !C) :-
    (
        L = []
    ;
        L = [H | T],
        LN = list_node(H, T),
        (
            !.C = empty_cord,
            !:C = nonempty_cord(LN)
        ;
            !.C = nonempty_cord(CN0),
            CN = branch_node(CN0, LN),
            !:C = nonempty_cord(CN)
        )
    ).

%---------------------------------------------------------------------------%

A ++ B = C :-
    (
        A = empty_cord,
        C = B
    ;
        A = nonempty_cord(_),
        B = empty_cord,
        C = A
    ;
        A = nonempty_cord(AN),
        B = nonempty_cord(BN),
        C = nonempty_cord(branch_node(AN, BN))
    ).

%---------------------%

cord_list_to_cord(Cords) = Cord :-
    % For tail recursion.
    list.reverse(Cords, RevCords),
    Cord = rev_cord_list_to_cord(RevCords).

%---------------------%

rev_cord_list_to_cord(RevCords) = Cord :-
    Cord = list.foldl(cord.(++), RevCords, empty_cord).

%---------------------%

condense(empty_cord) = empty_cord.
condense(nonempty_cord(C0)) = condense_node(C0).

:- func condense_node(cord_node(cord(T))) = cord(T).

condense_node(list_node(C, L)) = C ++ cord_list_to_cord(L).
condense_node(branch_node(Left0, Right0)) = Left ++ Right :-
    Left = condense_node(Left0),
    Right = condense_node(Right0).

%---------------------------------------------------------------------------%

is_empty(empty_cord).

is_non_empty(nonempty_cord(_)).

is_singleton(C, X) :-
    C = nonempty_cord(list_node(X, [])).

%---------------------------------------------------------------------------%

head(nonempty_cord(N), Head) :-
    get_first_node(N, Head).

get_first(nonempty_cord(N), Head) :-
    get_first_node(N, Head).

:- pred get_first_node(cord_node(T)::in, T::out) is det.

get_first_node(Node, Head) :-
    (
        Node = list_node(Head, _)
    ;
        Node = branch_node(A, _),
        get_first_node(A, Head)
    ).

%---------------------%

head_tail(nonempty_cord(N), H, T) :-
    head_tail_node(N, H, T).

:- pred head_tail_node(cord_node(T)::in, T::out, cord(T)::out) is det.

head_tail_node(Node, Head, Tail) :-
    (
        Node = list_node(H, T),
        Head = H,
        (
            T = [],
            Tail = empty_cord
        ;
            T = [TH | TT],
            Tail = nonempty_cord(list_node(TH, TT))
        )
    ;
        Node = branch_node(A0, B),
        head_tail_node(A0, Head, AC),
        (
            AC = empty_cord,
            Tail = nonempty_cord(B)
        ;
            AC = nonempty_cord(A),
            Tail = nonempty_cord(branch_node(A, B))
        )
    ).

%---------------------%

get_last(nonempty_cord(N), Last) :-
    get_last_node(N, Last).

:- pred get_last_node(cord_node(T)::in, T::out) is det.

get_last_node(Node, Last) :-
    (
        Node = list_node(Head, Tail),
        (
            Tail = [],
            Last = Head
        ;
            Tail = [_ | _],
            list.det_last(Tail, Last)
        )
    ;
        Node = branch_node(_, B),
        get_last_node(B, Last)
    ).

%---------------------%

split_last(nonempty_cord(N), AllButLast, Last) :-
    split_last_node(N, AllButLast, Last).

:- pred split_last_node(cord_node(T)::in, cord(T)::out, T::out) is det.

split_last_node(Node, AllButLast, Last) :-
    (
        Node = list_node(H, T),
        split_list_last(H, T, AllButLastList, Last),
        (
            AllButLastList = [],
            AllButLast = empty_cord
        ;
            AllButLastList = [AllButLastHead | AllButLastTail],
            AllButLast = nonempty_cord(
                list_node(AllButLastHead, AllButLastTail))
        )
    ;
        Node = branch_node(A, B0),
        split_last_node(B0, B, Last),
        AllButLast = nonempty_cord(A) ++ B
    ).

:- pred split_list_last(T::in, list(T)::in, list(T)::out, T::out) is det.

split_list_last(Prev, [], [], Prev).
split_list_last(Prev, [H | T], AllButLast, Last) :-
    split_list_last(H, T, AllButLast0, Last),
    AllButLast = [Prev | AllButLast0].

%---------------------------------------------------------------------------%

length(C) = foldl(func(_, N) = N + 1, C, 0).

%---------------------%

member(X, nonempty_cord(N)) :-
    member_node(X, N).

:- pred member_node(T::out, cord_node(T)::in) is nondet.

member_node(X, Node) :-
    (
        Node = list_node(H, T),
        (
            X = H
        ;
            member(X, T)
        )
    ;
        Node = branch_node(A, B),
        (
            member_node(X, A)
        ;
            member_node(X, B)
        )
    ).

%---------------------%

equal(CA, CB) :-
    % A more efficient algorithm would also be *much* more complex.
    list(CA) = list(CB).

%---------------------------------------------------------------------------%

from_list(Xs) = C :-
    (
        Xs = [],
        C = empty_cord
    ;
        Xs = [H | T],
        C = nonempty_cord(list_node(H, T))
    ).

%---------------------------------------------------------------------------%

list(C) =
    to_list(C).

to_list(empty_cord) = [].
to_list(nonempty_cord(N)) = to_list_2([N], []).

    % to_list_2(Ns, L0) = L:
    %
    % L is the reverse list of items in Ns appended in front of L0.
    %
:- func to_list_2(list(cord_node(T)), list(T)) = list(T).

to_list_2([], L) = L.
to_list_2([N | Ns], L0) = L :-
    (
        N = list_node(H, T),
        L = to_list_2(Ns, [H | T ++ L0])
    ;
        N = branch_node(A, B),
        L = to_list_2([B, A | Ns], L0)
    ).

%---------------------%

rev_list(C) =
    to_rev_list(C).

to_rev_list(empty_cord) = [].
to_rev_list(nonempty_cord(N)) = to_rev_list_nodes([N], []).

    % to_rev_list_nodes(Ns, L0) = L:
    %
    % L is the reverse list of items in Ns appended in front of L0.
    %
:- func to_rev_list_nodes(list(cord_node(T)), list(T)) = list(T).

to_rev_list_nodes([], L) = L.
to_rev_list_nodes([N | Ns], L0) = L :-
    (
        N = list_node(H, T),
        L = to_rev_list_nodes(Ns, list_reverse_2(T, [H | L0]))
    ;
        N = branch_node(A, B),
        L = to_rev_list_nodes([A, B | Ns], L0)
    ).

    % list_reverse_2(A, L0) = L:
    %
    % L is the reverse list of items in A appended in front of L0.
    %
:- func list_reverse_2(list(A), list(A)) = list(A).

list_reverse_2([], L) = L.
list_reverse_2([X | Xs], L0) =
    list_reverse_2(Xs, [X | L0]).

%---------------------%

cord_list_to_list(Cords) = List :-
    % For tail recursion.
    list.reverse(Cords, RevCords),
    List = rev_cord_list_to_list(RevCords).

%---------------------%

rev_cord_list_to_list(RevCords) = List :-
    List = list.foldl(cord_list_to_list_2, RevCords, []).

:- func cord_list_to_list_2(cord(T), list(T)) = list(T).

cord_list_to_list_2(empty_cord, L) = L.
cord_list_to_list_2(nonempty_cord(N), L) = to_list_2([N], L).

%---------------------------------------------------------------------------%

find_first_match(P, nonempty_cord(NX), FirstMatch) :-
    find_first_match_node(P, NX, FirstMatch).

:- pred find_first_match_node(pred(T)::in(pred(in) is semidet),
    cord_node(T)::in, T::out) is semidet.

find_first_match_node(P, Node, FirstMatch) :-
    (
        Node = list_node(XH, XT),
        ( if P(XH) then
            FirstMatch = XH
        else
            list.find_first_match(P, XT, FirstMatch)
        )
    ;
        Node = branch_node(XA, XB),
        ( if find_first_match_node(P, XA, FirstMatchPrime) then
            FirstMatch = FirstMatchPrime
        else
            find_first_match_node(P, XB, FirstMatch)
        )
    ).

%---------------------------------------------------------------------------%

map(_, empty_cord) = empty_cord.
map(F, nonempty_cord(N)) = nonempty_cord(map_func_node(F, N)).

:- func map_func_node(func(T) = U, cord_node(T)) = cord_node(U).

map_func_node(F, Node) = PNode :-
    (
        Node = list_node(H, T),
        PNode = list_node(F(H), list.map(F, T))
    ;
        Node = branch_node(A, B),
        PNode = branch_node(map_func_node(F, A), map_func_node(F, B))
    ).

map_pred(_, empty_cord, empty_cord).
map_pred(P, nonempty_cord(N), nonempty_cord(PN)) :-
    map_pred_node(P, N, PN).

:- pred map_pred_node(pred(T, U)::in(pred(in, out) is det),
    cord_node(T)::in, cord_node(U)::out) is det.

map_pred_node(P, Node, PNode) :-
    (
        Node = list_node(H, T),
        P(H, PH),
        list.map(P, T, PT),
        PNode = list_node(PH, PT)
    ;
        Node = branch_node(A, B),
        cord.map_pred_node(P, A, PA),
        cord.map_pred_node(P, B, PB),
        PNode = branch_node(PA, PB)
    ).

%---------------------------------------------------------------------------%

filter(_, empty_cord, empty_cord).
filter(P, nonempty_cord(N), Trues) :-
    filter_node(P, N, Trues).

:- pred filter_node(pred(T)::in(pred(in) is semidet),
    cord_node(T)::in, cord(T)::out) is det.

filter_node(P, Node, Trues) :-
    (
        Node = list_node(H, T),
        list.filter(P, [H | T], TrueList),
        (
            TrueList = [],
            Trues = empty_cord
        ;
            TrueList = [TH | TT],
            Trues = nonempty_cord(list_node(TH, TT))
        )
    ;
        Node = branch_node(A, B),
        filter_node(P, A, CATrues),
        filter_node(P, B, CBTrues),
        Trues = CATrues ++ CBTrues
    ).

%---------------------------------------------------------------------------%

filter(_, empty_cord, empty_cord, empty_cord).
filter(P, nonempty_cord(N), Trues, Falses) :-
    filter_node(P, N, Trues, Falses).

:- pred filter_node(pred(T)::in(pred(in) is semidet),
    cord_node(T)::in, cord(T)::out, cord(T)::out) is det.

filter_node(P, Node, Trues, Falses) :-
    (
        Node = list_node(H, T),
        list.filter(P, [H | T], TrueList, FalseList),
        (
            TrueList = [],
            Trues = empty_cord
        ;
            TrueList = [TH | TT],
            Trues = nonempty_cord(list_node(TH, TT))
        ),
        (
            FalseList = [],
            Falses = empty_cord
        ;
            FalseList = [FH | FT],
            Falses = nonempty_cord(list_node(FH, FT))
        )
    ;
        Node = branch_node(A, B),
        filter_node(P, A, CATrues, CAFalses),
        filter_node(P, B, CBTrues, CBFalses),
        Trues = CATrues ++ CBTrues,
        Falses = CAFalses ++ CBFalses
    ).

%---------------------------------------------------------------------------%

foldl(_, empty_cord, AccA) = AccA.
foldl(F, nonempty_cord(N), AccA0) = AccA :-
    foldl_node(F, N, [], AccA0, AccA).

:- pred foldl_node(func(T, U) = U, cord_node(T), list(cord_node(T)), U, U).
:- mode foldl_node(in(func(in, in) = out is det), in, in, in, out) is det.

foldl_node(F, C, Cs, !AccA) :-
    (
        C = list_node(H, T),
        list.foldl(F, [H | T], !.AccA) = !:AccA,
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldl_node(F, Y, Ys, !AccA)
        )
    ;
        C = branch_node(A, B),
        foldl_node(F, A, [B | Cs], !AccA)
    ).

foldl_pred(_P, empty_cord, !AccA).
foldl_pred(P, nonempty_cord(N), !AccA) :-
    foldl_node_pred(P, N, [], !AccA).

:- pred foldl_node_pred(pred(T, A, A), cord_node(T), list(cord_node(T)), A, A).
:- mode foldl_node_pred(in(pred(in, in, out) is det),
    in, in, in, out) is det.
:- mode foldl_node_pred(in(pred(in, mdi, muo) is det),
    in, in, mdi, muo) is det.
:- mode foldl_node_pred(in(pred(in, di, uo) is det),
    in, in, di, uo) is det.
:- mode foldl_node_pred(in(pred(in, in, out) is semidet),
    in, in, in, out) is semidet.
:- mode foldl_node_pred(in(pred(in, mdi, muo) is semidet),
    in, in, mdi, muo) is semidet.
:- mode foldl_node_pred(in(pred(in, di, uo) is semidet),
    in, in, di, uo) is semidet.

foldl_node_pred(P, C, Cs, !AccA) :-
    (
        C = list_node(H, T),
        list.foldl(P, [H | T], !AccA),
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldl_node_pred(P, Y, Ys, !AccA)
        )
    ;
        C = branch_node(A, B),
        foldl_node_pred(P, A, [B | Cs], !AccA)
    ).

foldl2(_P, empty_cord, !AccA, !AccB).
foldl2(P, nonempty_cord(N), !AccA, !AccB) :-
    foldl2_node(P, N, [], !AccA, !AccB).

:- pred foldl2_node(pred(T, A, A, B, B), cord_node(T), list(cord_node(T)),
    A, A, B, B).
:- mode foldl2_node(in(pred(in, in, out, in, out) is det),
    in, in, in, out, in, out) is det.
:- mode foldl2_node(in(pred(in, in, out, mdi, muo) is det),
    in, in, in, out, mdi, muo) is det.
:- mode foldl2_node(in(pred(in, in, out, di, uo) is det),
    in, in, in, out, di, uo) is det.
:- mode foldl2_node(in(pred(in, in, out, in, out) is semidet),
    in, in, in, out, in, out) is semidet.
:- mode foldl2_node(in(pred(in, in, out, mdi, muo) is semidet),
    in, in, in, out, mdi, muo) is semidet.
:- mode foldl2_node(in(pred(in, in, out, di, uo) is semidet),
    in, in, in, out, di, uo) is semidet.

foldl2_node(P, C, Cs, !AccA, !AccB) :-
    (
        C = list_node(H, T),
        list.foldl2(P, [H | T], !AccA, !AccB),
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldl2_node(P, Y, Ys, !AccA, !AccB)
        )
    ;
        C = branch_node(A, B),
        foldl2_node(P, A, [B | Cs], !AccA, !AccB)
    ).

foldl3(_P, empty_cord, !AccA, !AccB, !AccC).
foldl3(P, nonempty_cord(N), !AccA, !AccB, !AccC) :-
    foldl3_node(P, N, [], !AccA, !AccB, !AccC).

:- pred foldl3_node(pred(T, A, A, B, B, C, C),
    cord_node(T), list(cord_node(T)), A, A, B, B, C, C).
:- mode foldl3_node(in(pred(in, in, out, in, out, in, out) is det),
    in, in, in, out, in, out, in, out) is det.
:- mode foldl3_node(in(pred(in, in, out, in, out, mdi, muo) is det),
    in, in, in, out, in, out, mdi, muo) is det.
:- mode foldl3_node(in(pred(in, in, out, in, out, di, uo) is det),
    in, in, in, out, in, out, di, uo) is det.
:- mode foldl3_node(in(pred(in, in, out, in, out, in, out) is semidet),
    in, in, in, out, in, out, in, out) is semidet.
:- mode foldl3_node(in(pred(in, in, out, in, out, mdi, muo) is semidet),
    in, in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_node(in(pred(in, in, out, in, out, di, uo) is semidet),
    in, in, in, out, in, out, di, uo) is semidet.

foldl3_node(P, C, Cs, !AccA, !AccB, !AccC) :-
    (
        C = list_node(H, T),
        list.foldl3(P, [H | T], !AccA, !AccB, !AccC),
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldl3_node(P, Y, Ys, !AccA, !AccB, !AccC)
        )
    ;
        C = branch_node(A, B),
        foldl3_node(P, A, [B | Cs], !AccA, !AccB, !AccC)
    ).

%---------------------------------------------------------------------------%

foldr(_, empty_cord, Acc) = Acc.
foldr(F, nonempty_cord(N), Acc0) = Acc :-
    foldr_node(F, N, [], Acc0, Acc).

:- pred foldr_node(func(T, A) = A, cord_node(T), list(cord_node(T)), A, A).
:- mode foldr_node(in(func(in, in) = out is det), in, in, in, out) is det.

foldr_node(F, C, Cs, !Acc) :-
    (
        C = list_node(H, T),
        list.foldr(F, [H | T], !.Acc) = !:Acc,
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldr_node(F, Y, Ys, !Acc)
        )
    ;
        C = branch_node(A, B),
        foldr_node(F, B, [A | Cs], !Acc)
    ).

foldr_pred(_P, empty_cord, !Acc).
foldr_pred(P, nonempty_cord(N), !Acc) :-
    foldr_node_pred(P, N, [], !Acc).

:- pred foldr_node_pred(pred(T, A, A), cord_node(T), list(cord_node(T)), A, A).
:- mode foldr_node_pred(in(pred(in, in, out) is det), in, in, in, out) is det.
:- mode foldr_node_pred(in(pred(in, mdi, muo) is det), in, in, mdi, muo)
    is det.
:- mode foldr_node_pred(in(pred(in, di, uo) is det), in, in, di, uo) is det.
:- mode foldr_node_pred(in(pred(in, in, out) is semidet), in, in, in, out)
    is semidet.
:- mode foldr_node_pred(in(pred(in, mdi, muo) is semidet), in, in, mdi, muo)
    is semidet.
:- mode foldr_node_pred(in(pred(in, di, uo) is semidet), in, in, di, uo)
    is semidet.

foldr_node_pred(P, C, Cs, !Acc) :-
    (
        C = list_node(H, T),
        list.foldr(P, [H | T], !Acc),
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldr_node_pred(P, Y, Ys, !Acc)
        )
    ;
        C = branch_node(A, B),
        foldr_node_pred(P, B, [A | Cs], !Acc)
    ).

foldr2(_P, empty_cord, !Acc1, !Acc2).
foldr2(P, nonempty_cord(N), !Acc1, !Acc2) :-
    foldr2_node(P, N, [], !Acc1, !Acc2).

:- pred foldr2_node(pred(T, A, A, B, B), cord_node(T), list(cord_node(T)),
    A, A, B, B).
:- mode foldr2_node(in(pred(in, in, out, in, out) is det), in, in,
    in, out, in, out) is det.
:- mode foldr2_node(in(pred(in, in, out, mdi, muo) is det), in, in,
    in, out, mdi, muo) is det.
:- mode foldr2_node(in(pred(in, in, out, di, uo) is det), in, in,
    in, out, di, uo) is det.
:- mode foldr2_node(in(pred(in, in, out, in, out) is semidet), in, in,
    in, out, in, out) is semidet.
:- mode foldr2_node(in(pred(in, in, out, mdi, muo) is semidet), in, in,
    in, out, mdi, muo) is semidet.
:- mode foldr2_node(in(pred(in, in, out, di, uo) is semidet), in, in,
    in, out, di, uo) is semidet.

foldr2_node(P, C, Cs, !Acc1, !Acc2) :-
    (
        C = list_node(H, T),
        list.foldr2(P, [H | T], !Acc1, !Acc2),
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldr2_node(P, Y, Ys, !Acc1, !Acc2)
        )
    ;
        C = branch_node(A, B),
        foldr2_node(P, B, [A | Cs], !Acc1, !Acc2)
    ).

foldr3(_P, empty_cord, !Acc1, !Acc2, !Acc3).
foldr3(P, nonempty_cord(N), !Acc1, !Acc2, !Acc3) :-
    foldr3_node(P, N, [], !Acc1, !Acc2, !Acc3).

:- pred foldr3_node(pred(T, A, A, B, B, C, C), cord_node(T),
    list(cord_node(T)), A, A, B, B, C, C).
:- mode foldr3_node(in(pred(in, in, out, in, out, in, out) is det), in,
    in, in, out, in, out, in, out) is det.
:- mode foldr3_node(in(pred(in, in, out, in, out, mdi, muo) is det), in,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldr3_node(in(pred(in, in, out, in, out, di, uo) is det), in,
    in, in, out, in, out, di, uo) is det.
:- mode foldr3_node(in(pred(in, in, out, in, out, in, out) is semidet), in,
    in, in, out, in, out, in, out) is semidet.
:- mode foldr3_node(in(pred(in, in, out, in, out, mdi, muo) is semidet), in,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldr3_node(in(pred(in, in, out, in, out, di, uo) is semidet), in,
    in, in, out, in, out, di, uo) is semidet.

foldr3_node(P, C, Cs, !Acc1, !Acc2, !Acc3) :-
    (
        C = list_node(H, T),
        list.foldr3(P, [H | T], !Acc1, !Acc2, !Acc3),
        (
            Cs = []
        ;
            Cs = [Y | Ys],
            foldr3_node(P, Y, Ys, !Acc1, !Acc2, !Acc3)
        )
    ;
        C = branch_node(A, B),
        foldr3_node(P, B, [A | Cs], !Acc1, !Acc2, !Acc3)
    ).

%---------------------------------------------------------------------------%

map_foldl(_P, empty_cord, empty_cord, !A).
map_foldl(P, nonempty_cord(NX), nonempty_cord(NY), !A) :-
    map_foldl_node(P, NX, NY, !A).

:- pred map_foldl_node(pred(A, B, C, C), cord_node(A), cord_node(B), C, C).
:- mode map_foldl_node(in(pred(in, out, in, out) is det), in, out, in, out)
    is det.
:- mode map_foldl_node(in(pred(in, out, mdi, muo) is det), in, out, mdi, muo)
    is det.
:- mode map_foldl_node(in(pred(in, out, di, uo) is det), in, out, di, uo)
    is det.
:- mode map_foldl_node(in(pred(in, out, in, out) is semidet), in, out,
    in, out) is semidet.
:- mode map_foldl_node(in(pred(in, out, mdi, muo) is semidet), in, out,
    mdi, muo) is semidet.
:- mode map_foldl_node(in(pred(in, out, di, uo) is semidet), in, out,
    di, uo) is semidet.

map_foldl_node(P, list_node(XH, XT), list_node(YH, YT), !A) :-
    P(XH, YH, !A),
    list.map_foldl(P, XT, YT, !A).
map_foldl_node(P, branch_node(XA, XB), branch_node(YA, YB), !A) :-
    map_foldl_node(P, XA, YA, !A),
    map_foldl_node(P, XB, YB, !A).

%---------------------------------------------------------------------------%

map_foldl2(_P, empty_cord, empty_cord, !A, !B).
map_foldl2(P, nonempty_cord(NX), nonempty_cord(NY), !A, !B) :-
    map_foldl2_node(P, NX, NY, !A, !B).

:- pred map_foldl2_node(pred(A, B, C, C, D, D)::
    in(pred(in, out, in, out, in, out) is det),
    cord_node(A)::in, cord_node(B)::out, C::in, C::out, D::in, D::out) is det.

map_foldl2_node(P, list_node(XH, XT), list_node(YH, YT), !A, !B) :-
    P(XH, YH, !A, !B),
    list.map_foldl2(P, XT, YT, !A, !B).
map_foldl2_node(P, branch_node(XA, XB), branch_node(YA, YB), !A, !B) :-
    map_foldl2_node(P, XA, YA, !A, !B),
    map_foldl2_node(P, XB, YB, !A, !B).

%---------------------------------------------------------------------------%

map_foldl3(_P, empty_cord, empty_cord, !A, !B, !C).
map_foldl3(P, nonempty_cord(NX), nonempty_cord(NY), !A, !B, !C) :-
    map_foldl3_node(P, NX, NY, !A, !B, !C).

:- pred map_foldl3_node(pred(T1, T2, A, A, B, B, C, C)::
    in(pred(in, out, in, out, in, out, in, out) is det),
    cord_node(T1)::in, cord_node(T2)::out, A::in, A::out, B::in, B::out,
    C::in, C::out) is det.

map_foldl3_node(P, list_node(XH, XT), list_node(YH, YT), !A, !B, !C) :-
    P(XH, YH, !A, !B, !C),
    list.map_foldl3(P, XT, YT, !A, !B, !C).
map_foldl3_node(P, branch_node(XA, XB), branch_node(YA, YB), !A, !B, !C) :-
    map_foldl3_node(P, XA, YA, !A, !B, !C),
    map_foldl3_node(P, XB, YB, !A, !B, !C).

%---------------------------------------------------------------------------%
:- end_module cord.
%---------------------------------------------------------------------------%
