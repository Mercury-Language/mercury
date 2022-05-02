%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_inf.
:- interface.

:- pred p(int, int).
:- mode p(out, out) is det.

:- implementation.

p(X, Y) :-
    q(X, Y).

:- pred q(int, int).

q(Z, Z).
% Note that using the same variable names in q as in p can obscure
% possible confusion in the compiler about which predicate's record
% of variable names to use in the expected error message.
