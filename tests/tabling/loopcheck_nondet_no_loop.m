% Check that loopcheck isn't overzealous.

:- module loopcheck_nondet_no_loop.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, std_util.

main(!IO) :-
	solutions(non(10), N),
	io__write(N, !IO),
	io__write_string("\n", !IO),
	solutions(mul(20), M),
	io__write(M, !IO),
	io__write_string("\n", !IO).

:- pred non(int::in, int::out) is nondet.
:- pragma loop_check(non/2).

non(A, B) :-
	( A < 0 ->
		fail
	;
		(
			B = A
		;
			B = A + 1
		;
			A > 1,
			non(A / 2, B)
		)
	).

:- pred mul(int::in, int::out) is nondet.
:- pragma loop_check(mul/2).

mul(A, B) :-
	( A < 0 ->
		B = -1
	;
		(
			B = A
		;
			B = A + 1
		;
			A > 1,
			mul(A / 2, B)
		)
	).
