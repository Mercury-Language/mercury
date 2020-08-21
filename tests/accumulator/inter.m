%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is an interleaved version of reverse and length.
% Tests if we can introduce more than one accumulator.
%

:- module inter.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_string("rl: ", !IO),
    rl([1, 10, 100], Length, Reverse),
    io.write(Length, !IO),
    io.write_string(" ", !IO),
    io.write_line(Reverse, !IO).

:- pred rl(list(T)::in, int::out, list(T)::out) is det.

rl(X, L, R) :-
    X = [],
    L = 0,
    R = [].
rl(X, L, R) :-
    X = [H | T],
    rl(T, L0, R0),
    L = L0 + 1,
    Tmp = [H],
    append(R0, Tmp, R).
