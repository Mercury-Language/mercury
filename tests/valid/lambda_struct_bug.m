%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug in simplify/common.m where
% the pos(X, Y) deconstruction in the head of adj/2 was being propagated
% into the body of the lambda expression, changing the non-locals of the
% lambda expresssion, but the uni-modes field of the lambda construction
% unification was not being updated, resulting in an abort during lambda
% expansion.

:- module lambda_struct_bug.

:- interface.

:- import_module list.
:- import_module pair.
:- import_module set.

:- type pos
    --->    pos(int, int).

:- type adj
    --->    adj(pos, pos).

:- type maze == set(pair(pos, pos)).

:- pred adj(pos, list(adj)).
:- mode adj(in, out) is det.

:- implementation.

:- import_module int.
:- import_module require.
:- import_module solutions.

adj(pos(X, Y), Adjs) :-
    Pred =
        ( pred(Adj::out) is nondet :-
            (
                X1 = X - 1,
                Adj = adj(pos(X1, Y), pos(X, Y))
            ;
                X1 = X + 1,
                Adj = adj(pos(X1, Y), pos(X, Y))
            ;
                Y1 = Y + 1,
                Adj = adj(pos(X, Y1), pos(X, Y))
            ;
                Y1 = Y - 1,
                Adj = adj(pos(X, Y1), pos(X, Y))
            ),
            Adj = adj(pos(A, B), _),
            A >= 0, A =< 10,    % XXX
            B >= 0, B =< 10     % XXX
        ),
    solutions(Pred, Adjs).
