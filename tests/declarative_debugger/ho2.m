%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    p(0, 3, Y0),
    p(1, 3, Y1),
    p(2, 4, Y2),
    io.write_int(Y0, !IO),
    io.write_string(", ", !IO),
    io.write_int(Y1, !IO),
    io.write_string(", ", !IO),
    io.write_int(Y2, !IO),
    io.nl(!IO).

:- pred p(int::in, int::in, int::out) is det.

p(_, X, Y) :-
    C = (pred(A::in, B::out) is det :- B = A * X),
    q(C, X, Y).

:- pred q(pred(int, int)::in(pred(in, out) is det), int::in, int::out) is det.

q(C, X, Y) :-
    C(X * X, Y).
