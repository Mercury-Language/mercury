% vim: sw=4 ts=4 expandtab ft=mercury

:- module ambiguity.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    t1
    ;       t2.

:- type t(T)
    --->    u1
    ;       u2(T).

main(!IO) :-
    io.write(p(2.5), !IO),
    io.nl(!IO),
    io.write(p(1, t1), !IO),
    io.nl(!IO),
    io.write(p(0, 1, u2(42)), !IO),
    io.nl(!IO),
    p(1, X),
    io.write(X, !IO),
    io.nl(!IO).

:- func p(float) = float.

p(F) = F.

:- func p(int, t) = t.

p(_, T) = T.

:- func p(int, int, t(T)) = t(T).

p(_, _, T) = T.

:- pred p(int::in, int::out) is det.

p(I, I).
