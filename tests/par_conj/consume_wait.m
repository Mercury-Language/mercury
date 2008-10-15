% vim: ft=mercury ts=4 sw=4 et

:- module consume_wait.

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
        X3 = t2
    &
        Y3 = t3,
        s(X3, Y3, Z3)
    ),
    io.write_int(Z3, !IO),
    io.nl(!IO).

:- pred q(int::in, int::in, int::out) is det.
:- pragma no_inline(q/3).

q(X, Y, Z) :-
    ( X = 2 ->
        Z = Y
    ;
        Z = X
    ).

:- pred r(int::in, int::in, int::out) is det.
:- pragma no_inline(r/3).

r(X, Y, Z) :-
    (
        not (X = Y)
    ;
        true
    ),
    Z = Y.

:- type t
    --->    t1
    ;       t2
    ;       t3.

:- pred s(t::in, t::in, int::out) is det.
:- pragma no_inline(s/3).

s(X, Y, Z) :-
    ( not (X = Y) ->
        (
            X = t1,
            Z = val(Y)
        ;
            X = t2,
            Z = val(X) + val(Y)
        ;
            X = t3,
            (
                Y = t1,
                Z = 10 * val(X)
            ;
                Y = t2,
                Z = 20 * val(X)
            ;
                Y = t3,
                Z = 30 * val(X)
            )
        )
    ;
        Z = val(X)
    ).

:- func val(t) = int.

val(t1) = 1.
val(t2) = 2.
val(t3) = 3.
