%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module big.
:- interface.
:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module bool.
:- import_module library_forwarding.

main(!IO) :-
    ( if p(1) then
        R = yes
    else
        R = no
    ),
    io.write(R, !IO),
    io.write_string(".\n", !IO).

:- pred p(int::out) is nondet.

p(X) :-
    a(A),
    (
        b(A, B),
        ( if
            c(B, C),
            not (
                d(C, D),
                D > 5
            )
        then
            c(C, E)
        else
            e(B, E)
        )
    ;
        f(A, F),
        b(F, B),
        (
            B = 0,
            g(1, E)
        ;
            B = 1,
            g(10, E)
        ),
        not (
            f(E, H),
            c(H, I),
            g(I, J),
            J > 0
        )
    ),
    f(E, X).

:- pred a(int::out) is det.

a(0).

:- pred b(int::in, int::out) is multi.

b(X, X).
b(X, X+1).

:- pred c(int::in, int::out) is nondet.

c(0, 2).
c(0, 3).
c(1, 15).
c(2, 6).
c(2, 7).
c(3, 17).
c(4, 8).
c(4, 9).

:- pred d(int::in, int::out) is semidet.

d(X, X*3) :-
    X mod 2 = 1.

:- pred e(int::in, int::out) is multi.

e(X, X*10).
e(X, X*11).

:- pred f(int::in, int::out) is det.

f(X, X * -2).

:- pred g(int::in, int::out) is semidet.

g(1, -1).
g(6, -10).
g(7, -11).
g(8, -12).
g(9, 99).
g(10, -2).

