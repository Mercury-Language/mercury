% Tests all 3 types of transitive closure.

:- module tc_minimal2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module std_util, list.

main(!IO) :-
	solutions(tc_l(1), SolnsL),
	io__write_string("L = ", !IO),
	io__write(SolnsL, !IO),
	io__write_string("\n", !IO),
	solutions(tc_r(1), SolnsR),
	io__write_string("R = ", !IO),
	io__write(SolnsR, !IO),
	io__write_string("\n", !IO),
	solutions(tc_d(1), SolnsD),
	io__write_string("D = ", !IO),
	io__write(SolnsD, !IO),
	io__write_string("\n", !IO).

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
