:- module if_then_else.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	{ ite(0, X) },
	io__write_string("ite(0, "),
	io__write_int(X),
	io__write_string(").\n"),
	{ ite(1, Y) },
	io__write_string("ite(1, "),
	io__write_int(Y),
	io__write_string(").\n").

:- pred ite(int::in, int::out) is det.

ite(A, B) :-
	( a(A) ->
		b(B)
	;
		a(B)
	).

:- pred a(int).
:- mode a(in) is semidet.
:- mode a(out) is det.

a(0).

:- pred b(int).
:- mode b(out) is det.

b(1).

