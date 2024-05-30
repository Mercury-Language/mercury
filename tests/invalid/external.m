%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the error message for a predicate that has one or more
% clauses despite being marked as external.
%

:- module external2.

:- interface.

:- pred p(int::in, int::out) is semidet.

:- func f(int) = int.

:- implementation.

:- pragma external_pred(p/2).
:- pragma external_func(f/1).

p(0, 42).
p(1, 43).

f(A) = X :-
    ( if p(A, B) then
        X = B
    else
        X = 0
    ).
