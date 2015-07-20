%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module easy_nondet_test.

:- interface.

:- pred q(int::out) is nondet.
:- pred p(int::out) is multi.
:- pred r(int::in, int::out) is nondet.
:- pred s(int::in, int::out) is nondet.

:- implementation.

q(X) :-
    p(X).

p(X) :-
    ( X = 1 ; X = 2 ).

r(X, Y) :-
    ( if X = 3 then
        p(Y)
    else
        q(Y)
    ).

s(X, Y) :-
    ( if r(X, Z) then
        s(Z, Y)
    else
        q(Y)
    ).
