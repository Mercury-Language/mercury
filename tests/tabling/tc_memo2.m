:- module tc_memo2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module std_util, list.

main(!IO) :-
	(
		tc(1, 4),
		tc(3, 4)
	->
		io__write_string("yes\n", !IO)
	;
		io__write_string("no\n", !IO)
	).

:- pred tc(int::in, int::out) is nondet.
:- pragma memo(tc/2).

tc(A, B) :-
	edge(A, C),
	(
		B = C
	;
		tc(C, B)
	).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 3).
edge(3, 4).
