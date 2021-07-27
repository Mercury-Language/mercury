%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module complex_constraint_err.
:- interface.

:- import_module list.
:- import_module map.
:- import_module pair.

:- typeclass foo(A, B, C) <= (A -> B) where [].
:- typeclass bar(B, C) <= (B -> C) where [].

    % Error: C is not determined.
:- pred t(A) <= (foo(A, B, C), bar(B, C1)).
:- mode t(in) is semidet.

    % Error: X and Z are not determined.
:- pred u(A) <= (foo(A, pair(W, X1), map(Y, Z)), bar(W, Y), bar(X, list(Z))).
:- mode u(in) is semidet.

:- implementation.

t(_) :-
    semidet_succeed.

u(_) :-
    semidet_succeed.
