% Test the tracking of a subterm through an explicit supertree.

:- module explicit_subtree.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, exception.

main(!IO) :-
	p1(10, Q),
	write_int(Q, !IO),
	nl(!IO).

:- pred p1(int::in, int::out) is det.
:- pred p2(int::in, int::out) is det.
:- pred p3(int::in, int::out) is det.

p1(X, Y) :- p2(X, Y).
p2(X, Y) :- p3(X, Y).
p3(X, Y) :- calc(X, Y).

:- pred calc(int::in, int::out) is det.

calc(X, Y) :-
	(
		X > 0
	->
		a(Z)
	;
		b(Z)
	),
	divide2(X, Z, Y).

:- pred divide2(int::in, int::in, int::out) is det.

divide2(N, D, Q) :-
	(
		D = 0
	->
		throw("zero denominator")
	;
		Q = N // D
	).

:- pred b(int::out) is det.

b(-1).

:- pred a(int::out) is det.

a(X + Y - 100) :-
	q(49, 0, X), q(51, 0, Y).

:- pred q(int::in, int::in, int::out) is det.

q(A, B, C) :-
	(
		A =< 0
	->
		B = C
	;
		q(A-1, B+1, C)
	).
