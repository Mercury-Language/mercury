:- module exception_cmd.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require, int.

main --> { test(42, X) }, print(X).

:- pred test(int::in, int::out) is det.

test(X, Y) :-
	( X > 0 ->
		error("oops")
	;
		Y = X + 1
	).
