% vim: ts=4 sw=4 et ft=mercury

:- module lookup_disj.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module pair.
:- import_module string.

main(!IO) :-
    solutions(p, Solns),
    list.foldl(write_solution, Solns, !IO).

:- type t
    --->    a
    ;       b
    ;       c
    ;       f(t)
    ;       g(t).

:- type soln
    --->    soln(
                pair(float, string),
                int,
                t
            ).

:- pred p(soln::out) is multi.

p(Soln) :-
    q(Pair, Int0, T),
    peek_at_solution(Int0, Int),
    Soln = soln(Pair, Int, T).

:- pred q(pair(float, string)::out, int::out, t::out) is multi.

q(1.1 - "one",    1, a).
q(2.2 - "two",    2, b).
q(3.3 - "three",  3, c).
q(4.4 - "four",   4, f(a)).
q(5.5 - "five",   5, g(a)).

:- pred peek_at_solution(int::in, int::out) is det.

peek_at_solution(Int0, Int) :-
    trace [io(!IO)] (
        io.write_string("peek ", !IO),
        io.write_int(Int0, !IO),
        io.nl(!IO)
    ),
    Int = Int0 + 10.

:- pred write_solution(soln::in, io::di, io::uo) is det.

write_solution(Soln, !IO) :-
    Soln = soln(Float - Str, Int, T),
    io.format("solution %.2f - %5s, %d, ", [f(Float), s(Str), i(Int)], !IO),
    io.write(T, !IO),
    io.nl(!IO).
