%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module print_table.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    p(5, 5, P55),
    p(4, 3, P43),
    p(2, 2, P22),
    p(1, 0, P10),
    ( if q(3, 2, Q32) then
        MaybeQ32 = yes(Q32)
    else
        MaybeQ32 = no
    ),
    ( if q(4, 2, Q42) then
        MaybeQ42 = yes(Q42)
    else
        MaybeQ42 = no
    ),
    r(3, R3),
    s(3.5, 1, "abc", 1, SA, TA),
    s(3.5, 2, "abc", 2, SB, TB),
    s(3.5, 2, "xyz", 3, SC, TC),
    s(3.5, 2, "xyz", 4, SD, TD),
    s(9.2, 2, "def", 5, SE, TE),
    solutions(t(1, 2), T12),
    solutions(t(2, 2), T22),
    tdone,
    solutions(u(1, 2, 2), U12),
    solutions(u(2, 2, 2), U22),
    udone,
    io.write_int(P55, !IO),
    io.nl(!IO),
    io.write_int(P43, !IO),
    io.nl(!IO),
    io.write_int(P22, !IO),
    io.nl(!IO),
    io.write_int(P10, !IO),
    io.nl(!IO),
    io.write(MaybeQ32, !IO),
    io.nl(!IO),
    io.write(MaybeQ42, !IO),
    io.nl(!IO),
    io.write_int(R3, !IO),
    io.nl(!IO),
    io.write_string(SA, !IO),
    io.write_string(" ", !IO),
    io.write_float(TA, !IO),
    io.nl(!IO),
    io.write_string(SB, !IO),
    io.write_string(" ", !IO),
    io.write_float(TB, !IO),
    io.nl(!IO),
    io.write_string(SC, !IO),
    io.write_string(" ", !IO),
    io.write_float(TC, !IO),
    io.nl(!IO),
    io.write_string(SD, !IO),
    io.write_string(" ", !IO),
    io.write_float(TD, !IO),
    io.nl(!IO),
    io.write_string(SE, !IO),
    io.write_string(" ", !IO),
    io.write_float(TE, !IO),
    io.nl(!IO),
    io.write(T12, !IO),
    io.nl(!IO),
    io.write(T22, !IO),
    io.nl(!IO),
    io.write(U12, !IO),
    io.nl(!IO),
    io.write(U22, !IO),
    io.nl(!IO).

:- pred p(int::in, int::in, int::out) is det.
:- pragma memo(p/3).

p(A, B, S) :-
    ( if B = 0 then
        S = 0
    else
        p(A, B - 1, S0),
        S = A * B + S0
    ).

:- pred q(int::in, int::in, int::out) is semidet.
:- pragma memo(q/3).

q(A, B, S) :-
    ( if B = 0 then
        S = 0
    else if A = 4 * B then
        fail
    else
        q(A, B - 1, S0),
        S = A * B + S0
    ).

:- pred r(int::in, int::out) is det.
:- pragma memo(r/2).

r(A, S) :-
    ( if A = 0 then
        S = 0
    else
        r(A - 1, S0),
        S = A + S0
    ).

:- pred s(float::in, int::in, string::in, int::in, string::out, float::out)
    is det.
:- pragma memo(s/6).

s(A, B, C, D, S, T) :-
    string.format("%3.1f", [f(A)], AS),
    string.int_to_string(B, BS),
    string.append_list(["[", AS, C, BS, "]"], S0),
    S = from_char_list(list.condense(list.duplicate(D, to_char_list(S0)))),
    T = A + float(D).

:- pred t(int::in, int::in, int::out) is nondet.
:- pragma memo(t/3).

t(A, B, C) :-
    ( if A = 1 then
        (
            C = (A * 100) + (B * 10)
        ;
            C = (B * 100) + (A * 10)
        )
    else
        fail
    ).

:- pred tdone is det.

tdone.

:- pred u(int::in, int::in, int::in, int::out) is nondet.
:- pragma memo(u/4, [specified([value, value, promise_implied, output])]).

u(A, B, Bcopy, C) :-
    ( if A = 1 then
        (
            C = (A * 100) + (B * 10)
        ;
            C = (Bcopy * 100) + (A * 10)
        )
    else
        fail
    ).

:- pred udone is det.

udone.
