:- module loopcheck_nondet.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, std_util.

main(!IO) :-
	solutions(loop(2), Y),
	io__write(Y, !IO),
	io__write_string("\n", !IO).

:- pragma loop_check(loop/2).
:- pred loop(int::in, int::out) is nondet.

loop(X, Y) :-
	( X < 0 ->
		fail
	; X > 100 ->
		fail
	;
		(
			Y = X
		;
			loop(X - 2, Y)
		;
			loop(X * 2, Y)
		)
	).
