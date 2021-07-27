%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module prog_io_erroneous.

:- pred p(int, int, int).
:- mode p(in, out, out).

p(_, X, Y) :-
    q(Y, Y),
    X = 1.

p(_, 2, 3).

:- pred q(int, int).
:- mode q(in, out).
