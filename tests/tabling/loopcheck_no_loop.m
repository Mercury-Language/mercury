% Check that loopcheck isn't overzealous.

:- module loopcheck_no_loop.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
	{ sum(3, Sum3) },
	io__write_int(Sum3),
	io__write_string("\n"),
	{ sum(2, Sum2) },
	io__write_int(Sum2),
	io__write_string("\n").

:- pragma loop_check(sum/2).
:- pred sum(int::in, int::out) is det.

sum(N, SumN) :-
	( N = 0 ->
		SumN = 0
	;
		sum(N - 1, Sum1),
		SumN = Sum1 + N
	).
