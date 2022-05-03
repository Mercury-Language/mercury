%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module semi_disj.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

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
    make_t(xa, 1, T1),
    test(T1, 1, "a", !IO),
    make_t(xb, 2, T2),
    test(T2, 2, "b", !IO),
    make_t(xc, 3, T3),
    test(T3, 3, "c", !IO),
    make_t(xd, 4, T4),
    test(T4, 4, "d", !IO).

:- pred test(t::in, int::in, string::in, io::di, io::uo) is det.

test(T, I, S, !IO) :-
    ( if p(T, I, S, X) then
        io.format("success: %d\n", [i(X)], !IO)
    else
        io.write_string("failure\n", !IO)
    ).

:- pred make_t(x::in, int::in, t::out) is det.

make_t(xa, _, f(0)).
make_t(xb, I, g(I, 1)).
make_t(xc, _, h(2.2)).
make_t(xd, _, i("three")).

:- pred p(t::in, int::in, string::in, int::out) is semidet.

p(T, I, S, X) :-
    (
        T = f(N),
        P = [N],
        list.length(P) < 2
    ;
        I = 3
    ;
        S = "cc"
    ),
    X = I + 10.
