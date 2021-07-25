%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module solns.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module solutions.

main(!IO) :-
    p(1, Ss),
    io.write_line(Ss, !IO).

:- pred p(int::in, list(int)::out) is det.

p(N, Ss) :-
    solutions(q(N), Ss).

:- pred q(int::in, int::out) is nondet.

q(0, 0).
q(1, 1).
q(1, 2).
q(1, 3).
q(2, 2).
q(2, 4).
