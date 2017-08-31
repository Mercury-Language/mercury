%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tail_rec_scc.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    p(42, 0, C1),
    io.format("p(42, 0) = %d\n", [i(C1)], !IO),
    ( if s(0, 1, C2) then
        io.format("s(0, 1) = succeeded %d\n", [i(C2)], !IO)
    else
        io.format("s(0, 1) = failed\n", [], !IO)
    ),
    ( if s(1, 2, C3) then
        io.format("s(1, 2) = succeeded %d\n", [i(C3)], !IO)
    else
        io.format("s(1, 2) = failed\n", [], !IO)
    ).

:- pred p(int::in, int::in, int::out) is det.
:- pred q(int::in, int::in, int::out) is det.
:- pred r(int::in, int::in, int::out) is det.
:- pragma no_inline(p/3).
:- pragma no_inline(q/3).
:- pragma no_inline(r/3).

p(A, B, C) :-
    ( if A = 0 then
        C = B
    else
        q(A - 1, B + 1, C)
    ).

q(A, B, C) :-
    ( if A = 0 then
        C = B
    else
        r(A - 1, B + 1, C)
    ).

r(A, B, C) :-
    ( if A = 0 then
        C = B
    else
        p(A - 1, B + 1, C)
    ).

:- pred s(int::in, int::in, int::out) is semidet.
:- pred t(int::in, int::in, int::out) is semidet.
:- pred u(int::in, int::in, int::out) is semidet.
:- pragma no_inline(s/3).
:- pragma no_inline(t/3).
:- pragma no_inline(u/3).

s(A, B, C) :-
    ( if A = 0 then
        ( if B mod 2 = 0 then
            fail
        else
            C = B
        )
    else
        t(A - 1, B + 1, C)
    ).

t(A, B, C) :-
    ( if A = 0 then
        ( if B mod 3 = 0 then
            fail
        else
            C = B
        )
    else
        u(A - 1, B + 1, C)
    ).

u(A, B, C) :-
    ( if A = 0 then
        ( if B mod 5 = 0 then
            fail
        else
            C = B
        )
    else
        s(A - 1, B + 1, C)
    ).
