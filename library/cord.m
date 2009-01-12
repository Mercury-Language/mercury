%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
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

    % The list of data in a cord:
    %
    %   list(empty        ) = []
    %   list(from_list(Xs)) = Xs
    %   list(cons(X, C)   ) = [X | list(C)]
    %   list(TA ++ TB     ) = list(TA) ++ list(TB)
    %
:- func list(cord(T)) = list(T).

    % rev_list(Cord) = list.reverse(list(Cord).
    %
:- func rev_list(cord(T)) = list(T).

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

    % list(cons(X, C)) = [X | list(C)]
    % An O(1) operation.
    %
:- func cons(T, cord(T)) = cord(T).

    % list(snoc(C, X)) = list(C) ++ [X]
    % An O(1) operation.
    %
:- func snoc(cord(T), T) = cord(T).

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
    % - Pred(X) is true, then X is included in TrueCord.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    cord(T)::in, cord(T)::out) is det.

    % filter(Pred, Cord, TrueCord, FalseCord):
    %
    % Pred is a closure with one input argument.
    % For each member X of Cord,
    % - Pred(X) is true, then X is included in TrueCord.
    % - Pred(X) is false, then X is included in FalseCord.
    %
:- pred filter(pred(T)::in(pred(in) is semidet),
    cord(T)::in, cord(T)::out, cord(T)::out) is det.

    % foldl(F, C, A) = list.foldl(F, list(C), A).
    %
:- func foldl(func(T, U) = U, cord(T), U) = U.
:- pred foldl_pred(pred(T, U, U)::in(pred(in, in, out) is det), cord(T)::in,
    U::in, U::out) is det.

    % foldr(F, C, A) = list.foldr(F, list(C), A).
    %
:- func foldr(func(T, U) = U, cord(T), U) = U.
:- pred foldr_pred(pred(T, U, U)::in(pred(in, in, out) is det), cord(T)::in,
    U::in, U::out) is det.

    % map_foldl(P, CA, CB, !Acc):
    %
    % This predicate calls P on each element of the input cord, working
    % left to right. Each call to P transforms an element of the input cord
    % to the corresponding element of the output cord, and updates the
    % accumulator(s).
    %
:- pred map_foldl(pred(A, B, C, C)::in(pred(in, out, in, out) is det),
    cord(A)::in, cord(B)::out, C::in, C::out) is det.
:- pred map_foldl2(pred(A, B, C, C, D, D)::
    in(pred(in, out, in, out, in, out) is det),
    cord(A)::in, cord(B)::out, C::in, C::out, D::in, D::out) is det.
:- pred map_foldl3(pred(A, B, C, C, D, D, E, E)::
    in(pred(in, out, in, out, in, out, in, out) is det),
    cord(A)::in, cord(B)::out, C::in, C::out, D::in, D::out, E::in, E::out)
    is det.

    % equal(CA, CB)  <=>  list(CA) = list(CB).
    % An O(n) operation where n = length(CA) + length(CB).
    %
    % (Note: the current implementation works exactly this way.)
    %
:- pred equal(cord(T)::in, cord(T)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

    % We impose the following invariants to ensure we have a unique
    % representation for the empty cord (this makes the implementation
    % simpler.)
    %
    %   all [C] not C = leaves([])
    %   all [C] not C = branch(nil, _)
    %   all [C] not C = branch(_, nil)
    %
:- type cord(T)
    --->    nil
    ;       leaf(T)
    ;       leaves(list(T))
    ;       branch(cord(T), cord(T)).

%-----------------------------------------------------------------------------%

empty = nil.

%-----------------------------------------------------------------------------%

is_empty(nil).

%-----------------------------------------------------------------------------%

singleton(X) = leaf(X).

%-----------------------------------------------------------------------------%

from_list(Xs) = C :-
    (
        Xs = [],
        C = nil
    ;
        Xs = [_ | _],
        C = leaves(Xs)
    ).

%-----------------------------------------------------------------------------%

list(C) = list_2(C, []).

    % list_2(C, L0) = L:
    %
    % L is the list of items in C appended in front of L0.
    %
:- func list_2(cord(T), list(T)) = list(T).

list_2(nil,            Xs) = Xs.
list_2(leaf(Y),        Xs) = [Y | Xs].
list_2(leaves(Ys),     Xs) = Ys ++ Xs.
list_2(branch(CA, CB), Xs) = list_2(CA, list_2(CB, Xs)).

rev_list(C) = rev_list_2(C, []).

    % rev_list_2(C, L0) = L:
    %
    % L is the reverse list of items in C appended in front of L0.
    %
:- func rev_list_2(cord(T), list(T)) = list(T).

rev_list_2(nil,            Xs) = Xs.
rev_list_2(leaf(Y),        Xs) = [Y | Xs].
rev_list_2(leaves(Ys),     Xs) = list_reverse_2(Ys, Xs).
rev_list_2(branch(CA, CB), Xs) = rev_list_2(CB, list_2(CA, Xs)).

    % list_reverse_2(A, L0) = L:
    %
    % L is the reverse list of items in A appended in front of L0.
    %
:- func list_reverse_2(list(A), list(A)) = list(A).

list_reverse_2([], L) = L.
list_reverse_2([X | Xs], L0) =
    list_reverse_2(Xs, [X | L0]).

%-----------------------------------------------------------------------------%

cons(X, C) = XC :-
    (
        C = nil,
        XC = leaf(X)
    ;
        ( C = leaf(_)
        ; C = leaves(_)
        ; C = branch(_, _)
        ),
        XC = branch(leaf(X), C)
    ).

%-----------------------------------------------------------------------------%

snoc(C, X) = CX :-
    (
        C = nil,
        CX = leaf(X)
    ;
        ( C = leaf(_)
        ; C = leaves(_)
        ; C = branch(_, _)
        ),
        CX = branch(C, leaf(X))
    ).

%-----------------------------------------------------------------------------%

CA ++ CB = (      if CA = nil then CB
             else if CB = nil then CA
             else             branch(CA, CB)
           ).

%-----------------------------------------------------------------------------%

cord_list_to_cord([]) = nil.
cord_list_to_cord([HeadCord | TailCords]) =
    HeadCord ++ cord_list_to_cord(TailCords).

cord_list_to_list([]) = [].
cord_list_to_list([HeadCord | TailCords]) =
    list(HeadCord) ++ cord_list_to_list(TailCords).

%-----------------------------------------------------------------------------%

head_tail(leaf(X),          X, nil).
head_tail(leaves([X | Xs]), X, C  ) :-
    (
        Xs = [],
        C = nil
    ;
        Xs = [_ | _],
        C = leaves(Xs)
    ).
head_tail(branch(CA0, CB),  X, C  ) :-
    head_tail(CA0, X, CA),
    C = CA ++ CB.

%-----------------------------------------------------------------------------%

split_last(leaf(X),         nil, X).
split_last(leaves(Xs0),     C,   X) :-
    split_list_last(Xs0, C, X).
split_last(branch(CA, CB0), C,   X) :-
    split_last(CB0, CB, X),
    C = CA ++ CB.

    % split_list_last(Xs0, C, X):
    %
    % Given a nonempty list Xs0, returns its last element as X and all the
    % elements before it as the cord C. This has a similar effect to
    %
    %   list.split_last(Xs0, Xs, X),
    %   C = ( if Xs = [] then nil else leaves(Xs) ).
    %
    % but while repeatedly taking the last element of a list is an O(n^2)
    % operation (since a list is a right stick), we return C as a left stick,
    % on which repeatedly taking the last element of a list is an O(n)
    % operation. This ensures O(1) amortized cost for each call to
    % split_last/3 when an entire cord is traversed with that predicate.
    %
:- pred split_list_last(list(T)::in, cord(T)::out, T::out) is det.

split_list_last([], _, _) :-
    % This violates the invariant that an empty cord is represented by nil,
    % not by leaves([]).
    error("cord.m: split_list_last finds []").
split_list_last([H | T], InitialCord, Last) :-
    (
        T = [],
        InitialCord = nil,
        Last = H
    ;
        T = [TH | TT],
        split_list_last_2(leaf(H), TH, TT, InitialCord, Last)
    ).

:- pred split_list_last_2(cord(T)::in, T::in, list(T)::in,
    cord(T)::out, T::out) is det.

split_list_last_2(BeforeCord, H, T, InitialCord, Last) :-
    (
        T = [],
        InitialCord = BeforeCord,
        Last = H
    ;
        T = [TH | TT],
        split_list_last_2(BeforeCord ++ leaf(H), TH, TT, InitialCord, Last)
    ).

%-----------------------------------------------------------------------------%

get_first(leaf(X),         X).
get_first(leaves(Xs),      X) :-
    Xs = [X | _].
get_first(branch(CA, _CB), X) :-
    get_first(CA, X).

%-----------------------------------------------------------------------------%

get_last(leaf(X),         X).
get_last(leaves(Xs),      X) :-
    list.last(Xs, X).
get_last(branch(_CA, CB), X) :-
    get_last(CB, X).

%-----------------------------------------------------------------------------%

length(C) = foldl(func(_, N) = N + 1, C, 0).

%-----------------------------------------------------------------------------%

member(X, leaf(X)).
member(X, leaves(Xs)) :-
    member(X, Xs).
member(X, branch(CA, _)) :-
    member(X, CA).
member(X, branch(_, CB)) :-
    member(X, CB).

%-----------------------------------------------------------------------------%

map(_, nil           ) = nil.
map(F, leaf(X)       ) = leaf(F(X)).
map(F, leaves(Xs)    ) = leaves(map(F, Xs)).
map(F, branch(CA, CB)) = branch(map(F, CA), map(F, CB)).

map_pred(P, !Cord) :-
    (
        !.Cord = nil,
        !:Cord = nil
    ;
        !.Cord = leaf(X),
        P(X, PX),
        !:Cord = leaf(PX)
    ;
        !.Cord = leaves(Xs),
        list.map(P, Xs, PXs),
        !:Cord = leaves(PXs)
    ;
        !.Cord = branch(CA, CB),
        cord.map_pred(P, CA, PCA),
        cord.map_pred(P, CB, PCB),
        !:Cord = branch(PCA, PCB)
    ).

%-----------------------------------------------------------------------------%

filter(_P, nil, nil).
filter(P, leaf(X), Trues) :-
    ( P(X) ->
        Trues = leaf(X)
    ;
        Trues = nil
    ).
filter(P, leaves(Xs), Trues) :-
    list.filter(P, Xs, TrueList),
    (
        TrueList = [],
        Trues = nil
    ;
        TrueList = [_ | _],
        Trues = leaves(TrueList)
    ).
filter(P, branch(CA, CB), Trues) :-
    filter(P, CA, CATrues),
    filter(P, CB, CBTrues),
    Trues = CATrues ++ CBTrues.

filter(_P, nil, nil, nil).
filter(P, leaf(X), Trues, Falses) :-
    ( P(X) ->
        Trues = leaf(X),
        Falses = nil
    ;
        Trues = nil,
        Falses = leaf(X)
    ).
filter(P, leaves(Xs), Trues, Falses) :-
    list.filter(P, Xs, TrueList, FalseList),
    (
        TrueList = [],
        Trues = nil
    ;
        TrueList = [_ | _],
        Trues = leaves(TrueList)
    ),
    (
        FalseList = [],
        Falses = nil
    ;
        FalseList = [_ | _],
        Falses = leaves(FalseList)
    ).
filter(P, branch(CA, CB), Trues, Falses) :-
    filter(P, CA, CATrues, CAFalses),
    filter(P, CB, CBTrues, CBFalses),
    Trues = CATrues ++ CBTrues,
    Falses = CAFalses ++ CBFalses.

%-----------------------------------------------------------------------------%

foldl(_, nil,            A) = A.
foldl(F, leaf(X),        A) = F(X, A).
foldl(F, leaves(Xs),     A) = foldl(F, Xs, A).
foldl(F, branch(CA, CB), A) = foldl(F, CB, foldl(F, CA, A)).

foldl_pred(_P, nil, !A).
foldl_pred(P, leaf(X), !A) :-
    P(X, !A).
foldl_pred(P, leaves(Xs), !A) :-
    list.foldl(P, Xs, !A).
foldl_pred(P, branch(XA, XB), !A) :-
    foldl_pred(P, XA, !A),
    foldl_pred(P, XB, !A).

%-----------------------------------------------------------------------------%

foldr(_, nil,            A) = A.
foldr(F, leaf(X),        A) = F(X, A).
foldr(F, leaves(Xs),     A) = foldr(F, Xs, A).
foldr(F, branch(CA, CB), A) = foldr(F, CA, foldr(F, CB, A)).

foldr_pred(_P, nil, !A).
foldr_pred(P, leaf(X), !A) :-
    P(X, !A).
foldr_pred(P, leaves(Xs), !A) :-
    list.foldr(P, Xs, !A).
foldr_pred(P, branch(XA, XB), !A) :-
    foldr_pred(P, XB, !A),
    foldr_pred(P, XA, !A).

%-----------------------------------------------------------------------------%

map_foldl(_P, nil, nil, !A).
map_foldl(P, leaf(X), leaf(Y), !A) :-
    P(X, Y, !A).
map_foldl(P, leaves(Xs), leaves(Ys), !A) :-
    list.map_foldl(P, Xs, Ys, !A).
map_foldl(P, branch(XA, XB), branch(YA, YB), !A) :-
    map_foldl(P, XA, YA, !A),
    map_foldl(P, XB, YB, !A).

map_foldl2(_P, nil, nil, !A, !B).
map_foldl2(P, leaf(X), leaf(Y), !A, !B) :-
    P(X, Y, !A, !B).
map_foldl2(P, leaves(Xs), leaves(Ys), !A, !B) :-
    list.map_foldl2(P, Xs, Ys, !A, !B).
map_foldl2(P, branch(XA, XB), branch(YA, YB), !A, !B) :-
    map_foldl2(P, XA, YA, !A, !B),
    map_foldl2(P, XB, YB, !A, !B).

map_foldl3(_P, nil, nil, !A, !B, !C).
map_foldl3(P, leaf(X), leaf(Y), !A, !B, !C) :-
    P(X, Y, !A, !B, !C).
map_foldl3(P, leaves(Xs), leaves(Ys), !A, !B, !C) :-
    list.map_foldl3(P, Xs, Ys, !A, !B, !C).
map_foldl3(P, branch(XA, XB), branch(YA, YB), !A, !B, !C) :-
    map_foldl3(P, XA, YA, !A, !B, !C),
    map_foldl3(P, XB, YB, !A, !B, !C).

%-----------------------------------------------------------------------------%

equal(CA, CB) :-
    list(CA) = list(CB).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
