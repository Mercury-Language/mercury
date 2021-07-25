%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module skip.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    a(1, X),
    a(2, Y),
    a(3, Z),
    io.write_int(X + Y + Z, !IO),
    io.nl(!IO).

:- pred a(int::in, int::out) is det.

a(Y, X) :-
    b(Y, A),
    c(A, B),
    d(B, X).

:- pred b(int::in, int::out) is det.

b(X, X + 69).

:- pred c(int::in, int::out) is det.

c(A, B) :-
    e(A, C),
    f(C, B).

:- pred d(int::in, int::out) is det.

d(X, X).

:- pred e(int::in, int::out) is det.

e(X, X).

:- pred f(int::in, int::out) is det.

f(X, X).
