%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module implied_mode.

:- interface.

:- pred p(int::out, int::out) is det.

:- implementation.

p(5, 6).

:- pred q(int::out) is semidet.

q(X) :-
    Y = 2,
    p(X, Y).
