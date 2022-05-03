%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lookup_disj.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    solutions(test_p, PSolns),
    list.foldl(write_p_solution, PSolns, !IO),
    solutions(test_q, QSolns),
    list.foldl(write_q_solution, QSolns, !IO).

:- type t
    --->    a
    ;       b
    ;       c
    ;       f(t)
    ;       g(t).

:- type p_soln
    --->    p_soln(
                pair(float, string),
                int,
                t,
                float
            ).

:- type q_soln
    --->    q_soln(
                pair(float, string),
                int,
                t,
                int,
                t
            ).

:- pred test_p(p_soln::out) is multi.

test_p(Soln) :-
    p(Pair, Int0, T, F),
    peek_at_p_solution(Int0, Int),
    Soln = p_soln(Pair, Int, T, F).

:- pred test_q(q_soln::out) is multi.

test_q(Soln) :-
    q(Pair, IntA0, TA, IntB0, TB),
    peek_at_q_solution(IntA0, IntA, IntB0, IntB),
    Soln = q_soln(Pair, IntA, TA, IntB, TB).

:- pred p(pair(float, string)::out, int::out, t::out, float::out) is multi.

p(A, B, C, D) :-
    ( A = 1.1 - "one",   B = 1, C = a, D = 10.1
    ; A = 2.2 - "two",   B = 2, C = b, D = 20.2
    ; A = 3.3 - "three", B = 3, C = c, D = 30.3
    ; A = 4.4 - "four",  B = 4, C = f(a), D = 40.4
    ; A = 5.5 - "five",  B = 5, C = g(a), D = 50.5
    ).

:- pred q(pair(float, string)::out, int::out, t::out, int::out, t::out)
    is multi.

q(A, B, C, D, E) :-
    ( A = 1.1 - "one",   B = 1, C = a
    ; A = 2.2 - "two",   B = 2, C = b
    ; A = 3.3 - "three", B = 3, C = c
    ; A = 4.4 - "four",  B = 4, C = f(a)
    ; A = 5.5 - "five",  B = 5, C = g(a)
    ),
    ( D = 11, E = a
    ; D = 22, E = b
    ; D = 33, E = c
    ; D = 44, E = f(a)
    ; D = 55, E = g(a)
    ).

:- pred peek_at_p_solution(int::in, int::out) is det.

peek_at_p_solution(Int0, Int) :-
    trace [io(!IO)] (
        io.format("peek p %d\n", [i(Int0)], !IO)
    ),
    Int = Int0 + 10.

:- pred peek_at_q_solution(int::in, int::out, int::in, int::out) is det.

peek_at_q_solution(IntA0, IntA, IntB0, IntB) :-
    trace [io(!IO)] (
        io.format("peek q %d %d\n", [i(IntA0), i(IntB0)], !IO)
    ),
    IntA = IntA0 + 10,
    IntB = IntB0 + 100.

:- pred write_p_solution(p_soln::in, io::di, io::uo) is det.

write_p_solution(Soln, !IO) :-
    Soln = p_soln(Float - Str, Int, T, Float2),
    io.format("p soln %.2f - %5s, %d, %.2f, %s\n",
        [f(Float), s(Str), i(Int), f(Float2), s(string.string(T))], !IO).

:- pred write_q_solution(q_soln::in, io::di, io::uo) is det.

write_q_solution(Soln, !IO) :-
    Soln = q_soln(Float - Str, IntA, TA, IntB, TB),
    TAStr = string.string(TA),
    TBStr = string.string(TB),
    io.format("q soln %.2f - %5s, %d, %s, %d, %s\n",
        [f(Float), s(Str), i(IntA), s(TAStr), i(IntB), s(TBStr)], !IO).
