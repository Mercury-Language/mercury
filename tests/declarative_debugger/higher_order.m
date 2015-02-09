:- module higher_order.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int.

main -->
	{ p(3, Y) },
	io__write_int(Y),
	io__nl.

:- pred p(int::in, int::out) is det.

p(X, Y) :-
	C = (pred(A::in, B::out) is det :- B = A * A),
	q(C, X, Y).

:- pred q(pred(int, int)::in(pred(in, out) is det), int::in, int::out) is det.

q(C, X, Y) :-
	C(X * X, Y).

