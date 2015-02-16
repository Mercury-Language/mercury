%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module det_errors.

:- interface.

:- pred p1(int::in) is det.
:- pred p2(int::in) is det.
:- pred p3(int::in) is det.
:- pred p4(int::in) is det.
:- pred p5(int::in) is det.

:- type t
    --->    a
    ;       b
    ;       c
    ;       d
    ;       e
    ;       f
    ;       g
    ;       h(int)
    ;       i(int).

:- pred q(t::in, int::out) is det.

:- type u
    --->    u1
    ;       u2
    ;       u3(t)
    ;       u4(t).

:- pred r(u::in, int::out) is det.
:- pred s(u::in, int::out) is det.
:- pred t(int::out) is det.

:- implementation.
:- import_module int.
:- import_module require.

p1(42).
p2(X) :- X = 42.
p3(X) :- X = 42.
p4(X) :- X = 21 + 21.
p5(_) :- true.

q(a, 1).
q(b, 2).
q(c, 3).

r(U, X) :-
    (
        U = u1,
        X = 11
    ;
        U = u3(a),
        X = 31
    ;
        U = u3(b),
        X = 32
    ;
        U = u3(c),
        X = 33
    ;
        U = u4(a),
        X = 41
    ;
        U = u4(b),
        X = 42
    ;
        U = u4(c),
        X = 43
    ;
        U = u4(d),
        X = 441
    ;
        U = u4(d),
        X = 442
    ;
        U = u4(e),
        X = 45
    ;
        U = u4(f),
        X = 46
    ;
        U = u4(g),
        X = 47
    ).

s(U, X) :-
    (
        U = u1,
        X = 11
    ;
        U = u3(a),
        X = 31
    ;
        U = u3(b),
        X = 32
    ;
        U = u3(c),
        X = 33
    ;
        U = u4(V),
        (
            V = a,
            X = 41
        ;
            V = b,
            X = 42
        ;
            V = c,
            X = 43
        ;
            V = d,
            X = 441
        ;
            V = d,
            X = 442
        ;
            ( V = e
            ; V = f
            ),
            ( X = 461
            ; X = 462
            )
        ;
            V = g,
            X = 47
        ;
            ( V = h(_)
            ; V = i(_)
            ),
            ( X = 481
            ; X = 482
            )
        )
    ).

:- pragma no_determinism_warning(t/1).

t(_) :-
    error("t called").
