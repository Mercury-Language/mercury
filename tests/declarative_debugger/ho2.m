:- module ho2.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int.

main -->
	{ p(0, 3, Y0) },
	{ p(1, 3, Y1) },
	{ p(2, 4, Y2) },
	io__write_int(Y0),
	io__write_string(", "),
	io__write_int(Y1),
	io__write_string(", "),
	io__write_int(Y2),
	io__nl.

:- pred p(int::in, int::in, int::out) is det.

p(_, X, Y) :-
	C = (pred(A::in, B::out) is det :- B = A * X),
	q(C, X, Y).

:- pred q(pred(int, int)::in(pred(in, out) is det), int::in, int::out) is det.

q(C, X, Y) :-
	C(X * X, Y).

