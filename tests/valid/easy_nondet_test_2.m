%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module easy_nondet_test_2.

:- interface.

:- pred p(int::out) is nondet.
:- pred q(int::out) is nondet.

:- implementation.

q(1).
q(2).

:- pred r(int::out) is nondet.

r(3).
r(4).

p(X) :-
    q(X) ; r(X).
