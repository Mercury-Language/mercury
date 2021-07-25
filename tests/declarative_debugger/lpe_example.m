%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lpe_example.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions.
:- import_module int.

main(!IO) :-
    solutions(p(1), Ss),
    io.write_line(Ss, !IO).

:- pred p(int::in, int::out) is nondet.

p(0, 0).
p(1, X) :-
    q(A),
    (
        r(A, X)
    ;
        X = A
    ).

:- pred q(int::out) is det.

q(3).

:- pred r(int::in, int::out) is multi.

r(X, X + 10).
r(X, X + 20).
