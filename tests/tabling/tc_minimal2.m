% Tests all 3 types of transitive closure.

:- module tc_minimal2.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, list.

main -->
	{ solutions(tc_l(1), SolnsL) },
	io__write_string("L = "),
	io__write(SolnsL),
	io__write_string("\n"),
	{ solutions(tc_r(1), SolnsR) },
	io__write_string("R = "),
	io__write(SolnsR),
	io__write_string("\n"),
	{ solutions(tc_d(1), SolnsD) },
	io__write_string("D = "),
	io__write(SolnsD),
	io__write_string("\n").

:- pred tc_l(int::in, int::out) is nondet.
:- pragma minimal_model(tc_l/2).

tc_l(A, B) :-
	(
		edge(A, B)
	;
		tc_l(A, C), edge(C, B)
	).

:- pred tc_r(int::in, int::out) is nondet.
:- pragma minimal_model(tc_r/2).

tc_r(A, B) :-
	edge(A, C),
	(
		B = C
	;
		tc_r(C, B)
	).

:- pred tc_d(int::in, int::out) is nondet.
:- pragma minimal_model(tc_d/2).

tc_d(A, B) :-
	(
		edge(A, B)
	;
		tc_d(A, C), tc_d(C, B)
	).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 7).
edge(7, 8).
edge(8, 7).
edge(8, 9).
edge(8,10).
edge(2, 3).
edge(3, 4).
edge(4, 5).
edge(4, 1).
edge(5, 1).
edge(1, 1).
