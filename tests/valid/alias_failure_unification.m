:- module alias_failure_unification.
:- interface.

:- pred test(int :: in) is failure.

:- implementation.

:- type f(T1,T2) ---> f(T1,T2).

test(X) :-
	P = 1,
	Q = 2,
	Y = f(P, Q),
	q(Y, P, Q),
	Y = f(X, X).

:- pred q(f(int, int), int, int).
:- mode q(in, in, in) is det.

q(_, _, _).
	
