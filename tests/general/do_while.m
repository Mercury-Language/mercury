%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% tests/general/do_while.m:
%   A test case for solutions.do_while/4.
%
% Adapted from tests/general/nondet_ite.m.

:- module do_while.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    do_while(r(100), write_answer("foo"), !IO),
    do_while(r(100), collect_answer(201), [], L),
    io.print_line(L, !IO),
    do_while(r(100), collect_answer(555), [], L2),
    io.print_line(L2, !IO).

:- pred write_answer(string::in, int::in, bool::out,
    io::di, io::uo) is det.

write_answer(S, R, More, !IO) :-
    io.print_line(S, !IO),
    io.print_line(R, !IO),
    More = (if R = 200 then no else yes).

:- pred collect_answer(int::in, int::in, bool::out,
    list(int)::in, list(int)::out) is det.

collect_answer(Limit, R, More, Rs0, [R | Rs0]) :-
    More = (if R = Limit then no else yes).

:- pred r(int::in, int::out) is nondet.

r(Mult, Z) :-
    q(X, Y),
    Z = X * Mult + Y.

:- pred q(int::out, int::out) is nondet.

q(X, Y) :-
    p(X),
    ( if some [Y1] p(Y1) then
        Y = Y1
    else
        Y = 42
    ).

:- pred p(int::out) is nondet.

p(0).
p(1).
p(2).
