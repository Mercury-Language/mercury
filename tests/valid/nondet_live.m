%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nondet_live.
:- interface.

:- pred a1(int::in, int::in, int::out) is nondet.
:- pred a2(int::in, int::in, int::out) is nondet.
:- pred a3(int::in, int::in, int::out) is nondet.

:- implementation.
:- import_module int.

a1(X, _, Y) :-
    A = 42,
    (
        b(X, X),
        V = 10
    ;
        b(A, A),
        V = 20
    ),
    some [W] (
        c(V, W),
        d(W, Y)
    ).

a2(X, _, Y) :-
    A = 42,
    (
        X = 45,
        b(X, X),
        V = 10
    ;
        X = 47,
        b(A, A),
        V = 20
    ),
    some [W] (
        c(V, W),
        d(W, Y)
    ).

a3(X, _, Y) :-
    A = 42,
    ( if
        X = 45
    then
        b(X, X),
        V = 10
    else
        b(A, A),
        V = 20
    ),
    some [W] (
        c(V, W),
        d(W, Y)
    ).

:- pred b(int::in, int::out) is nondet.
:- pred c(int::in, int::out) is nondet.
:- pred d(int::in, int::out) is nondet.

:- pragma no_inline(b/2).
:- pragma no_inline(c/2).
:- pragma no_inline(d/2).

b(X, X) :- semidet_true.
b(X, X) :- semidet_true.

c(X, X) :- semidet_true.
c(X, X) :- semidet_true.

d(X, X) :- semidet_true.
d(X, X) :- semidet_true.
