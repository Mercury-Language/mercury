:- module failure_unify.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type f ---> f(int).

main -->
	(
		{ X = f(1) },
		{ Y = f(2) },
		{ X = Y }
	->
		io__write_string("test failed\n")
	;
		io__write_string("test succeeded\n")
	).

