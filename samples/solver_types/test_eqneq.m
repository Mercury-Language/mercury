%-----------------------------------------------------------------------------%
% test_eqneq.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Mon Feb 12 12:33:57 EST 2007
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Test case.
%
%-----------------------------------------------------------------------------%

:- module test_eqneq.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module eqneq.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ( if solve(Solution) then
        io.print(Solution, !IO),
        io.nl(!IO)
      else
        io.print("no solution\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred solve(list(int)::out) is nondet.

solve(Solution) :-
    eqneq.n_new(9, Xs),
    Xs = [X1, X2, X3, X4, X5, X6, X7, X8, X9],
    X1 = X4, X4 = X7,
    X2 = X5, X5 = X8,
    X3 = X6, X6 = X9,
    neq(X1, X2),
    neq(X1, X3),
    neq(X2, X3),
    label(Xs, Solution).

%-----------------------------------------------------------------------------%

:- pred label(list(eqneq(int))::ia, list(int)::out) is nondet.

label([], []).

label([EqNeq | EqNeqs], [X | Xs]) :-
    (   X = 1   ;   X = 2   ;   X = 3
    ;   X = 4   ;   X = 5   ;   X = 6
    ;   X = 7   ;   X = 8   ;   X = 9
    ),
    eqneq.bind(EqNeq, X),
    label(EqNeqs, Xs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
