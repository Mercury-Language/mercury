:- module p.

:- pred p(int, int, int).
:- mode p(in, out, out).

p(_, X, Y) :-
	q(Y, Y),
	X = 1.

p(_, 2, 3).

:- pred q(int, int).
:- mode q(in, out).
