:- module tc_loop.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, list.

main -->
	{ solutions(tc(1), Solns) },
	( { Solns = [] } ->
		io__write_string("loopcheck failed, tc has no solutions\n")
	;
		io__write_string("loopcheck failed, tc has solutions\n")
	).

:- pred tc(int::in, int::out) is nondet.
:- pragma loop_check(tc/2).

tc(A, B) :-
	(
		edge(A, B)
	;
		edge(A, C),
		tc(C, B)
	).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 3).
edge(2, 1).
edge(3, 4).
