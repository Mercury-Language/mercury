:- module explicit_subtree.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list.

main(!IO) :-
	p(X, Y),
	write({X, Y}, !IO),
	nl(!IO).

:- pred p(int::out, int::out) is det.

p(X, Y) :- q(100, 10, X),q(200, 30, Y).

:- pred q(int::in, int::in, int::out) is det.

q(A, B, C) :-
	(
		A =< 0
	->
		B = C
	;
		q(A-1, B+1, C)
	).
