% tests/general/do_while.m:
%	A test case for std_util__do_while/4.
%
% Adapted from tests/general/nondet_ite.m.

:- module do_while.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module list, int, std_util, bool.

main -->
	do_while(r(100), write_answer("foo")),
	{ do_while(r(100), collect_answer(201), [], L) },
	print(L), nl,
	{ do_while(r(100), collect_answer(555), [], L2) },
	print(L2), nl.

:- pred write_answer(string::in, int::in, bool::out,
	io__state::di, io__state::uo) is det.

write_answer(S, R, More) -->
	print(S), nl,
	print(R), nl,
	{ More = (if R = 200 then no else yes) }.

:- pred collect_answer(int, int, bool, list(int), list(int)).
:- mode collect_answer(in, in, out, in, out) is det.

collect_answer(Limit, R, More, Rs0, [R|Rs0]) :-
	More = (if R = Limit then no else yes).

:- pred r(int::in, int::out) is nondet.

r(Mult, Z) :-
	q(X, Y),
	Z is X * Mult + Y.

:- pred q(int::out, int::out) is nondet.

q(X, Y) :-
	p(X),
	(if some [Y1] p(Y1) then
		Y = Y1
	else
		Y = 42
	).

:- pred p(int::out) is nondet.

p(0).
p(1).
p(2).
