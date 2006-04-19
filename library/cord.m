%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: cord.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
% Stability: medium.
% 
% A cord is a sequence type supporting O(1) consing and
% concatenation.  A cord is essentially a tree structure with data stored
% in the leaf nodes.  Joining two cords together to construct a new cord
% is therefore an O(1) operation.
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

:- import_module int.
:- import_module list.

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

    % The unique representation for the empty cord:
    %
    %   list(empty) = []
    %
:- func empty = cord(T).

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

    %     head_tail(C0, X, C)  =>  list(C0) = [X | list(C)]
    % not head_tail(C0, _, _)  =>  C0 = empty
    % An O(n) operation, although traversing an entire cord with
    % head_tail/3 gives O(1) amortized cost for each call.
    %
:- pred head_tail(cord(T)::in, T::out, cord(T)::out) is semidet.

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

    % foldl(F, C, A) = list.foldl(F, list(C), A).
    %
:- func foldl(func(T, U) = U, cord(T), U) = U.

    % foldr(F, C, A) = list.foldr(F, list(C), A).
    %
:- func foldr(func(T, U) = U, cord(T), U) = U.

    % equal(CA, CB)  <=>  list(CA) = list(CB).
    % An O(n) operation where n = length(CA) + length(CB).
    %
    % (Note: the current implementation works exactly this way.)
    %
:- pred equal(cord(T)::in, cord(T)::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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

singleton(X) = leaf(X).

%-----------------------------------------------------------------------------%

from_list(Xs) = ( if Xs = [] then nil else leaves(Xs) ).

%-----------------------------------------------------------------------------%

list(C) = list_2(C, []).

:- func list_2(cord(T), list(T)) = list(T).

list_2(nil,            Xs) = Xs.
list_2(leaf(Y),        Xs) = [Y | Xs].
list_2(leaves(Ys),     Xs) = Ys ++ Xs.
list_2(branch(CA, CB), Xs) = list_2(CA, list_2(CB, Xs)).

%-----------------------------------------------------------------------------%

cons(X, C) = ( if C = nil then leaf(X) else branch(leaf(X), C) ).

%-----------------------------------------------------------------------------%

snoc(C, X) = ( if C = nil then leaf(X) else branch(C, leaf(X)) ).

%-----------------------------------------------------------------------------%

CA ++ CB = (      if CA = nil then CB
             else if CB = nil then CA
             else             branch(CA, CB)
           ).

%-----------------------------------------------------------------------------%

head_tail(leaf(X),          X, nil ).

head_tail(leaves([X | Xs]), X, C   ) :-
    C = ( if Xs = [] then nil else leaves(Xs) ).

head_tail(branch(CA0, CB),  X, C   ) :-
    head_tail(CA0, X, CA),
    C = CA ++ CB.

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

%-----------------------------------------------------------------------------%

foldl(_, nil,            A) = A.
foldl(F, leaf(X),        A) = F(X, A).
foldl(F, leaves(Xs),     A) = foldl(F, Xs, A).
foldl(F, branch(CA, CB), A) = foldl(F, CB, foldl(F, CA, A)).

%-----------------------------------------------------------------------------%

foldr(_, nil,            A) = A.
foldr(F, leaf(X),        A) = F(X, A).
foldr(F, leaves(Xs),     A) = foldr(F, Xs, A).
foldr(F, branch(CA, CB), A) = foldr(F, CA, foldr(F, CB, A)).

%-----------------------------------------------------------------------------%

equal(CA, CB) :-
    list(CA) = list(CB).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
