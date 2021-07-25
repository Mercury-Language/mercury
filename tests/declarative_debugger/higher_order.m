%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    p(3, Y),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- pred p(int::in, int::out) is det.

p(X, Y) :-
    C = (pred(A::in, B::out) is det :- B = A * A),
    q(C, X, Y).

:- pred q(pred(int, int)::in(pred(in, out) is det), int::in, int::out) is det.

q(C, X, Y) :-
    C(X * X, Y).
