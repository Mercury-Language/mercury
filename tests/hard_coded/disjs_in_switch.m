%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module disjs_in_switch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    f(int)
    ;       g(int, int)
    ;       h(float)
    ;       i(string).

:- type x
    --->    xa
    ;       xb
    ;       xc
    ;       xd.

main(!IO) :-
    make_t(xa, T1),
    test(T1, !IO),
    make_t(xb, T2),
    test(T2, !IO),
    make_t(xc, T3),
    test(T3, !IO),
    make_t(xd, T4),
    test(T4, !IO).

:- pred test(t::in, io::di, io::uo) is det.

test(T, !IO) :-
    p(T, U),
    io.write_string(U, !IO),
    io.nl(!IO).

:- pred make_t(x::in, t::out) is det.

make_t(xa, f(0)).
make_t(xb, g(1, 1)).
make_t(xc, h(2.2)).
make_t(xd, i("three")).

:- pred p(t::in, string::out) is det.

p(T, U) :-
    (
        ( T = f(_)
        ; T = g(_, _)
        ),
        U = "f or g"
    ;
        ( T = h(_)
        ; T = i(_)
        ),
        U = "h or i"
    ).
