% This is a regression test; the version of the compiler in early May 2004
% generated a wrong answer for this code: it claimed that tc(2, 2) fails.
% (It got the right answer if we reversed the order of the disjuncts in tc/2.)

:- module tc_minimal_semidet.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string, std_util, list.

main(!IO) :-
	test(1, 1, !IO),
	test(1, 2, !IO),
	test(2, 2, !IO),
	test(2, 3, !IO),
	test(3, 3, !IO),
	test(2, 4, !IO).

:- pred test(int::in, int::in, io::di, io::uo) is det.

test(A, B, !IO) :-
	io__write_string("tc(" ++ int_to_string(A) ++ ", "
		++ int_to_string(B) ++ "): ", !IO),
	( tc(A, B) ->
		io__write_string("succeeded\n", !IO)
	;
		io__write_string("failed\n", !IO)
	).

:- pred tc(int::in, int::in) is semidet.
:- pragma minimal_model(tc/2).

tc(A, B) :-
	edge(A, C),
	(
		tc(C, B)
	;
		B = C
	).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 3).
edge(2, 1).
edge(3, 4).
