:- module shallow.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module shallow_2.

main -->
	{ test(p("t1", 5), P1) },
	{ test(p("t2", 37), P2) },
	{ test(q("t3", 2), Q1) },
	{ test(q("t4", -1), Q2) },
	{ test(r("t5", 3), R) },
	io__write_int(P1),
	io__nl,
	io__write_int(P2),
	io__nl,
	io__write_int(Q1),
	io__nl,
	io__write_int(Q2),
	io__nl,
	io__write_int(R),
	io__nl.

:- pred test(pred(int), int).
:- mode test(pred(out) is det, out) is det.

test(P, N) :-
	P(N).

