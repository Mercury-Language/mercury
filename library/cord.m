%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: cord.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
% Stability: medium.
%
% A cord is a sequence type supporting O(1) consing and concatenation.
% A cord is essentially a tree structure with data stored in the leaf nodes.
% Joining two cords together to construct a new cord is therefore an O(1)
% operation.
%
% This data type is intended for situations where efficient, linearised
% collection of data is required.
%
% While this data type presents a list-like interface, calls to list/1 and
% head_tail/3 in particular are O(n) in the size of the cord.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module cord.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % Cords that contain the same members in the same order will not
    % necessarily have the same representation and will, therefore,
    % not necessarily either unify or compare as equal.
    %
    % The exception to this rule is that the empty cord does have a
    % unique representation.
    %
:- type cord(T).

    % Return the empty cord.
    %
:- func init = cord(T).

    % The unique representation for the empty cord:
    %
    %   list(empty) = []
    %
:- func empty = cord(T).

    % Succeed iff the given cord is empty.
    %
:- pred is_empty(cord(T)::in) is semidet.

    % list(singleton(X)) = [X]
    %
:- func singleton(T) = cord(T).

    % list(from_list(Xs)) = Xs
    % An O(1) operation.
    %
:- func from_list(list(T)) = cord(T).

    % The list of data in a cord:
    %
    %   list(empty        ) = []
    %   list(from_list(Xs)) = Xs
    %   list(cons(X, C)   ) = [X | list(C)]
    %   list(TA ++ TB     ) = list(TA) ++ list(TB)
    %
:- func list(cord(T)) = list(T).

    % A synonym for the list/1.
    %
:- func to_list(cord(T)) = list(T).

    % rev_list(Cord) = list.reverse(list(Cord).
    %
:- func rev_list(cord(T)) = list(T).

    % A synonym for rev_list/1.
    %
:- func to_rev_list(cord(T)) = list(T).

    % Cord = condense(CordOfCords):
    %
    % `Cord' is the result of concatenating all the elements of `CordOfCords'.
    %
:- func condense(cord(cord(T))) = cord(T).

    % list(cons(X, C)) = [X | list(C)]
    % An O(1) operation.
    %
:- func cons(T, cord(T)) = cord(T).
:- pred cons(T::in, cord(T)::in, cord(T)::out) is det.

    % list(snoc(C, X)) = list(C) ++ [X]
    % An O(1) operation.
    %
:- func snoc(cord(T), T) = cord(T).
:- pred snoc(T::in, cord(T)::in, cord(T)::out) is det.

    % list(CA ++ CB) = list(CA) ++ list(CB)
    % An O(1) operation.
    %
:- func cord(T) ++ cord(T) = cord(T).

    % Append together a list of cords.
    %
:- func cord_list_to_cord(list(cord(T))) = cord(T).

    % Append together a list of cords, and return the result as a list.
    %
:- func cord_list_to_list(list(cord(T))) = list(T).

    %     head_tail(C0, X, C)  =>  list(C0) = [X | list(C)]
    % not head_tail(C0, _, _)  =>  C0 = empty
    % An O(n) operation, although traversing an entire cord with
    % head_tail/3 gives O(1) amortized cost for each call.
    %
:- pred head_tail(cord(T)::in, T::out, cord(T)::out) is semidet.

    %     split_last(C0, C, X)  =>  list(C0) = C ++ [X].
    % not split_last(C0, _, _)  =>  C0 = empty
    % An O(n) operation, although traversing an entire cord with
    % split_last/3 gives O(1) amortized cost for each call.
    %
:- pred split_last(cord(T)::in, cord(T)::out, T::out) is semidet.

    %     get_first(C0, X)  =>  some [C]: list(C0) = [X] ++ C.
    % not get_first(C0, _)  =>  C0 = empty
    %
:- pred get_first(cord(T)::in, T::out) is semidet.

    %     get_last(C0, X)  =>  some [C]: list(C0) = C ++ [X].
    % not get_last(C0, _)  =>  C0 = empty
    %
:- pred get_last(cord(T)::in, T::out) is semidet.

    % length(C) = list.length(list(C))
    % An O(n) operation.
    %
:- func length(cord(T)) = int.

    % member(X, C) <=> list.member(X, list(C)).
    %
:- pred member(T::out, cord(T)::in) is nondet.

    % list(map(F, C)) = list.map(F, list(C))
    %
:- func map(func(T) = U, cord(T)) = cord(U).
:- pred map_pred(pred(T, U)::in(pred(in, out) is det),
    cord(T)::in, cord(U)::out) is det.

    % filter(Pred, Cord, TrueCord):
    %
    % Pred is a closure with one input argument.
    % For each member X of Cord,
    % - if Pred(X) is true, then X is included in TrueCord.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    cord(T)::in, cord(T)::out) is det.

    % filter(Pred, Cord, TrueCord, FalseCord):
    %
    % Pred is a closure with one input argument.
    % For each member X of Cord,
    % - if Pred(X) is true, then X is included in TrueCord.
    % - if Pred(X) is false, then X is included in FalseCord.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    cord(T)::in, cord(T)::out, cord(T)::out) is det.

    % foldl(F, C, A) = list.foldl(F, list(C), A).
    %
:- func foldl(func(T, U) = U, cord(T), U) = U.
:- pred foldl_pred(pred(T, U, U), cord(T), U, U).
:- mode foldl_pred(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl_pred(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldl_pred(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldl_pred(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldl_pred(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldl_pred(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

    % foldr(F, C, A) = list.foldr(F, list(C), A).
    %
:- func foldr(func(T, U) = U, cord(T), U) = U.
:- pred foldr_pred(pred(T, U, U), cord(T), U, U).
:- mode foldr_pred(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldr_pred(in(pred(in, mdi, muo) is det), in, mdi, muo) is det.
:- mode foldr_pred(in(pred(in, di, uo) is det), in, di, uo) is det.
:- mode foldr_pred(in(pred(in, in, out) is semidet), in, in, out) is semidet.
:- mode foldr_pred(in(pred(in, mdi, muo) is semidet), in, mdi, muo) is semidet.
:- mode foldr_pred(in(pred(in, di, uo) is semidet), in, di, uo) is semidet.

    % map_foldl(P, CA, CB, !Acc):
    %
    % This predicate calls P on each element of the input cord, working
    % left to right. Each call to P transforms an element of the input cord
    % to the corresponding element of the output cord, and updates the
    % accumulator.
    %
:- pred map_foldl(pred(A, B, C, C), cord(A), cord(B), C, C).
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
:- pred map_foldl2(pred(A, B, C, C, D, D)::
    in(pred(in, out, in, out, in, out) is det),
    cord(A)::in, cord(B)::out, C::in, C::out, D::in, D::out) is det.

    % As above, but with three accumulators.
    %
:- pred map_foldl3(pred(A, B, C, C, D, D, E, E)::
    in(pred(in, out, in, out, in, out, in, out) is det),
    cord(A)::in, cord(B)::out, C::in, C::out, D::in, D::out, E::in, E::out)
    is det.

    % find_first_match(Pred, List, FirstMatch) takes a closure with one
    % input argument. It returns the first element X of the cord (if any)
    % for which Pred(X) is true.
    %
:- pred find_first_match(pred(X)::in(pred(in) is semidet),
    cord(X)::in, X::out) is semidet.

    % equal(CA, CB)  <=>  list(CA) = list(CB).
    % An O(n) operation where n = length(CA) + length(CB).
    %
    % (Note: the current implementation works exactly this way.)
    %
:- pred equal(cord(T)::in, cord(T)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- type cord(T)
    --->    empty_cord
    ;       nonempty_cord(cord_node(T)).

:- type cord_node(T)
    --->    unit_node(T)
    ;       list_node(T, list(T))
    ;       branch_node(cord_node(T), cord_node(T)).

%---------------------------------------------------------------------------%

init = empty_cord.

empty = empty_cord.

is_empty(empty_cord).

%---------------------------------------------------------------------------%

singleton(X) = nonempty_cord(unit_node(X)).

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
        N = unit_node(X),
        L = to_list_2(Ns, [X | L0])
    ;
        N = list_node(H, T),
        L = to_list_2(Ns, [H | T ++ L0])
    ;
        N = branch_node(A, B),
        L = to_list_2([B, A | Ns], L0)
    ).

rev_list(C) =
    to_rev_list(C).

to_rev_list(empty_cord) = [].
to_rev_list(nonempty_cord(N)) = to_rev_list_2([N], []).

    % to_rev_list_2(Ns, L0) = L:
    %
    % L is the reverse list of items in Ns appended in front of L0.
    %
:- func to_rev_list_2(list(cord_node(T)), list(T)) = list(T).

to_rev_list_2([], L) = L.
to_rev_list_2([N | Ns], L0) = L :-
    (
        N = unit_node(X),
        L = to_rev_list_2(Ns, [X | L0])
    ;
        N = list_node(H, T),
        L = to_rev_list_2(Ns, list_reverse_2(T, [H | L0]))
    ;
        N = branch_node(A, B),
        L = to_rev_list_2([A, B | Ns], L0)
    ).

    % list_reverse_2(A, L0) = L:
    %
    % L is the reverse list of items in A appended in front of L0.
    %
:- func list_reverse_2(list(A), list(A)) = list(A).

list_reverse_2([], L) = L.
list_reverse_2([X | Xs], L0) =
    list_reverse_2(Xs, [X | L0]).

%---------------------------------------------------------------------------%

condense(empty_cord) = empty_cord.
condense(nonempty_cord(C0)) = condense_2(C0).

:- func condense_2(cord_node(cord(T))) = cord(T).

condense_2(unit_node(C)) = C.
condense_2(list_node(C, L)) = C ++ cord_list_to_cord(L).
condense_2(branch_node(Left0, Right0)) = Left ++ Right :-
    Left = condense_2(Left0),
    Right = condense_2(Right0).

%---------------------------------------------------------------------------%

cons(X, C) = XC :-
    (
        C = empty_cord,
        XC = nonempty_cord(unit_node(X))
    ;
        C = nonempty_cord(N),
        XC = nonempty_cord(branch_node(unit_node(X), N))
    ).

cons(X, !C) :-
    !:C = cons(X, !.C).

%---------------------------------------------------------------------------%

snoc(C, X) = CX :-
    (
        C = empty_cord,
        CX = nonempty_cord(unit_node(X))
    ;
        C = nonempty_cord(N),
        CX = nonempty_cord(branch_node(N, unit_node(X)))
    ).

snoc(X, !C) :-
    !:C = snoc(!.C, X).

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

%---------------------------------------------------------------------------%

cord_list_to_cord(Cords) = Cord :-
    % For tail recursion.
    list.reverse(Cords, RevCords),
    Cord = list.foldl((++), RevCords, empty_cord).

cord_list_to_list(Cords) = List :-
    % For tail recursion.
    list.reverse(Cords, RevCords),
    List = list.foldl(cord_list_to_list_2, RevCords, []).

:- func cord_list_to_list_2(cord(T), list(T)) = list(T).

cord_list_to_list_2(empty_cord, L) = L.
cord_list_to_list_2(nonempty_cord(N), L) = to_list_2([N], L).

%---------------------------------------------------------------------------%

head_tail(nonempty_cord(N), H, T) :-
    head_tail_node(N, H, T).

:- pred head_tail_node(cord_node(T)::in, T::out, cord(T)::out) is det.

head_tail_node(Node, Head, Tail) :-
    (
        Node = unit_node(Head),
        Tail = empty_cord
    ;
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

%---------------------------------------------------------------------------%

split_last(nonempty_cord(N), AllButLast, Last) :-
    split_last_node(N, AllButLast, Last).

:- pred split_last_node(cord_node(T)::in, cord(T)::out, T::out) is det.

split_last_node(Node, AllButLast, Last) :-
    (
        Node = unit_node(Last),
        AllButLast = empty_cord
    ;
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

get_first(nonempty_cord(N), Head) :-
    get_first_node(N, Head).

:- pred get_first_node(cord_node(T)::in, T::out) is det.

get_first_node(Node, Head) :-
    (
        Node = unit_node(Head)
    ;
        Node = list_node(Head, _)
    ;
        Node = branch_node(A, _),
        get_first_node(A, Head)
    ).

%---------------------------------------------------------------------------%

get_last(nonempty_cord(N), Last) :-
    get_last_node(N, Last).

:- pred get_last_node(cord_node(T)::in, T::out) is det.

get_last_node(Node, Last) :-
    (
        Node = unit_node(Last)
    ;
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

%---------------------------------------------------------------------------%

length(C) = foldl(func(_, N) = N + 1, C, 0).

%---------------------------------------------------------------------------%

member(X, nonempty_cord(N)) :-
    member_node(X, N).

:- pred member_node(T::out, cord_node(T)::in) is nondet.

member_node(X, Node) :-
    (
        Node = unit_node(X)
    ;
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

%---------------------------------------------------------------------------%

map(_, empty_cord) = empty_cord.
map(F, nonempty_cord(N)) = nonempty_cord(map_node(F, N)).

:- func map_node(func(T) = U, cord_node(T)) = cord_node(U).

map_node(F, Node) = PNode :-
    (
        Node = unit_node(X),
        PNode = unit_node(F(X))
    ;
        Node = list_node(H, T),
        PNode = list_node(F(H), list.map(F, T))
    ;
        Node = branch_node(A, B),
        PNode = branch_node(map_node(F, A), map_node(F, B))
    ).

map_pred(_, empty_cord, empty_cord).
map_pred(P, nonempty_cord(N), nonempty_cord(PN)) :-
    map_pred_node(P, N, PN).

:- pred map_pred_node(pred(T, U)::in(pred(in, out) is det),
    cord_node(T)::in, cord_node(U)::out) is det.

map_pred_node(P, Node, PNode) :-
    (
        Node = unit_node(X),
        P(X, PX),
        PNode = unit_node(PX)
    ;
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
        Node = unit_node(X),
        ( if P(X) then
            Trues = nonempty_cord(unit_node(X))
        else
            Trues = empty_cord
        )
    ;
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
        Node = unit_node(X),
        ( if P(X) then
            Trues = nonempty_cord(unit_node(X)),
            Falses = empty_cord
        else
            Trues = empty_cord,
            Falses = nonempty_cord(unit_node(X))
        )
    ;
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

foldl(_, empty_cord, Acc) = Acc.
foldl(F, nonempty_cord(N), Acc0) = Acc :-
    foldl_node(F, N, [], Acc0, Acc).

:- pred foldl_node(func(T, U) = U, cord_node(T), list(cord_node(T)), U, U).
:- mode foldl_node(in(func(in, in) = out is det), in, in, in, out) is det.

foldl_node(F, C, Cs, !Acc) :-
    (
        C = unit_node(X),
        F(X, !.Acc) = !:Acc
    ;
        C = list_node(H, T),
        list.foldl(F, [H | T], !.Acc) = !:Acc
    ),
    (
        Cs = []
    ;
        Cs = [Y | Ys],
        foldl_node(F, Y, Ys, !Acc)
    ).
foldl_node(F, branch_node(A, B), Cs, !Acc) :-
    foldl_node(F, A, [B | Cs], !Acc).

foldl_pred(_P, empty_cord, !Acc).
foldl_pred(P, nonempty_cord(N), !Acc) :-
    foldl_node_pred(P, N, [], !Acc).

:- pred foldl_node_pred(pred(T, U, U), cord_node(T), list(cord_node(T)), U, U).
:- mode foldl_node_pred(in(pred(in, in, out) is det), in, in, in, out) is det.
:- mode foldl_node_pred(in(pred(in, mdi, muo) is det), in, in, mdi, muo)
    is det.
:- mode foldl_node_pred(in(pred(in, di, uo) is det), in, in, di, uo) is det.
:- mode foldl_node_pred(in(pred(in, in, out) is semidet), in, in, in, out)
    is semidet.
:- mode foldl_node_pred(in(pred(in, mdi, muo) is semidet), in, in, mdi, muo)
    is semidet.
:- mode foldl_node_pred(in(pred(in, di, uo) is semidet), in, in, di, uo)
    is semidet.

foldl_node_pred(P, C, Cs, !Acc) :-
    (
        C = unit_node(X),
        P(X, !Acc)
    ;
        C = list_node(H, T),
        list.foldl(P, [H | T], !Acc)
    ),
    (
        Cs = []
    ;
        Cs = [Y | Ys],
        foldl_node_pred(P, Y, Ys, !Acc)
    ).
foldl_node_pred(P, branch_node(A, B), Cs, !Acc) :-
    foldl_node_pred(P, A, [B | Cs], !Acc).

%---------------------------------------------------------------------------%

foldr(_, empty_cord, Acc) = Acc.
foldr(F, nonempty_cord(N), Acc0) = Acc :-
    foldr_node(F, N, [], Acc0, Acc).

:- pred foldr_node(func(T, U) = U, cord_node(T), list(cord_node(T)), U, U).
:- mode foldr_node(in(func(in, in) = out is det), in, in, in, out) is det.

foldr_node(F, C, Cs, !Acc) :-
    (
        C = unit_node(X),
        F(X, !.Acc) = !:Acc
    ;
        C = list_node(H, T),
        list.foldr(F, [H | T], !.Acc) = !:Acc
    ),
    (
        Cs = []
    ;
        Cs = [Y | Ys],
        foldr_node(F, Y, Ys, !Acc)
    ).
foldr_node(F, branch_node(A, B), Cs, !Acc) :-
    foldr_node(F, B, [A | Cs], !Acc).

foldr_pred(_P, empty_cord, !Acc).
foldr_pred(P, nonempty_cord(N), !Acc) :-
    foldr_node_pred(P, N, [], !Acc).

:- pred foldr_node_pred(pred(T, U, U), cord_node(T), list(cord_node(T)), U, U).
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
        C = unit_node(X),
        P(X, !Acc)
    ;
        C = list_node(H, T),
        list.foldr(P, [H | T], !Acc)
    ),
    (
        Cs = []
    ;
        Cs = [Y | Ys],
        foldr_node_pred(P, Y, Ys, !Acc)
    ).
foldr_node_pred(P, branch_node(A, B), Cs, !Acc) :-
    foldr_node_pred(P, B, [A | Cs], !Acc).

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

map_foldl_node(P, unit_node(X), unit_node(Y), !A) :-
    P(X, Y, !A).
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

map_foldl2_node(P, unit_node(X), unit_node(Y), !A, !B) :-
    P(X, Y, !A, !B).
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

:- pred map_foldl3_node(pred(A, B, C, C, D, D, E, E)::
    in(pred(in, out, in, out, in, out, in, out) is det),
    cord_node(A)::in, cord_node(B)::out, C::in, C::out, D::in, D::out,
    E::in, E::out) is det.

map_foldl3_node(P, unit_node(X), unit_node(Y), !A, !B, !C) :-
    P(X, Y, !A, !B, !C).
map_foldl3_node(P, list_node(XH, XT), list_node(YH, YT), !A, !B, !C) :-
    P(XH, YH, !A, !B, !C),
    list.map_foldl3(P, XT, YT, !A, !B, !C).
map_foldl3_node(P, branch_node(XA, XB), branch_node(YA, YB), !A, !B, !C) :-
    map_foldl3_node(P, XA, YA, !A, !B, !C),
    map_foldl3_node(P, XB, YB, !A, !B, !C).

%---------------------------------------------------------------------------%

find_first_match(P, nonempty_cord(NX), FirstMatch) :-
    find_first_match_node(P, NX, FirstMatch).

:- pred find_first_match_node(pred(X)::in(pred(in) is semidet),
    cord_node(X)::in, X::out) is semidet.

find_first_match_node(P, Node, FirstMatch) :-
    (
        Node = unit_node(X),
        ( if P(X) then
            FirstMatch = X
        else
            fail
        )
    ;
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

equal(CA, CB) :-
    % A more efficient algorithm would also be *much* more complex.
    list(CA) = list(CB).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
