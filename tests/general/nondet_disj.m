:- module nondet_disj.
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
	p(X),
	(
		Y = 41
	;
		Y = 42
	).

:- pred p(int::out) is multidet.

p(1).
p(2).

