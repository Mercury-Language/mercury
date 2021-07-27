%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test for a compiler abort on unifications
% of the form `X = X'.

:- module id_type_bug.

:- interface.

:- pred p(int::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

p(A, B) :-
    ( if q(A, X0), Z = Z then
        X = X0
    else
        X = 1
    ),
    p(X, B).

:- pred q(int::in, int::out) is semidet.

q(1, 5).

%---------------------------------------------------------------------------%
