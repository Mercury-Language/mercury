:- module x.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is multidet.

:- implementation.

main -->
	{ q(X, Y) },
	io__write_string("X = "),
	io__write_int(X),
	io__write_string(", Y = "),
	io__write_int(Y),
	io__write_string("\n").

:- pred q(int::out, int::out) is multidet.

q(X, Y) :-
	( X = 1 ; X = 2 ),
	( Y = 41 ; Y = 42).

