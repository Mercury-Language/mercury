% vim: ft=mercury ts=4 sw=4 et

:- module consume_in_some_branches.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    (
        X1 = 1
    &
        Y1 = 2,
        q(X1, Y1, Z1)
    ),
    io.write_int(Z1, !IO),
    io.nl(!IO),
    (
        X2 = 1
    &
        Y2 = 2,
        r(X2, Y2, Z2)
    ),
    io.write_int(Z2, !IO),
    io.nl(!IO),
    (
        X3 = 1
    &
        Y3 = 3,
        s(X3, Y3, Z3)
    ),
    io.write_int(Z3, !IO),
    io.nl(!IO).

:- pred q(int::in, int::in, int::out) is det.
:- pragma no_inline(q/3).

q(X, Y, Z) :-
    ( Y = 2 ->
        Z = Y
    ;
        Z = X
    ).

:- pred r(int::in, int::in, int::out) is det.
:- pragma no_inline(r/3).

r(X, Y, Z) :-
    ( Y = 2 ->
        A = Y,
        Z = A
    ;
        A = X,
        Z = A
    ).

:- pred s(int::in, int::in, int::out) is det.
:- pragma no_inline(s/3).

s(X, Y, Z) :-
    ( Y = 2 ->
        (
            A = Y
        &
            Z = A
        )
    ;
        (
            A = X
        &
            B = X
        &
            C = Y
        ),
        Z = A + B + C
    ).
