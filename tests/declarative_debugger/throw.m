:- module throw.
:- interface.
:- import_module io.
:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

:- implementation.
:- import_module int, exception.

main -->
	{ try(p, X) },
	io__write(X),
	io__nl,
	{ try(q, Y) },
	io__write(Y),
	io__nl.

:- pred p(int::out) is cc_nondet.

p(X) :-
	a(A),
	b(A, X),
	X < 0.

:- pred a(int::out) is multi.

a(2).
a(3).

:- pred b(int::in, int::out) is multi.

b(A, B) :-
	(
		B = A * 3
	;
		B = A * 4
	),
	(
		B > 10
	->
		throw("Too big")
	;
		true
	).

:- pred q(int::out) is semidet.

q(1) :-
	not (
		a2(A),
		not (
			b2(A, 0)
		),
		A < 0
	).

:- pred a2(int::out) is multi.

a2(2).
a2(3).

:- pred b2(int::in, int::out) is multi.

b2(A, B) :-
	(
		B = A * 3
	;
		B = A * 4
	),
	(
		B > 10
	->
		throw("Too big")
	;
		true
	).

