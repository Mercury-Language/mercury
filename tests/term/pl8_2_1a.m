:- module pl8_2_1a.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred mergesort(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

mergesort([], []).
mergesort([E], [E]).
mergesort([X, Y | Z], U) :-
	s(Z, V, W),
	mergesort([X | V], X1),
	mergesort([Y | W], Y1),
	merge(X1, Y1, U).

:- pred merge(list(int), list(int), list(int)).
:- mode merge(in, in, out).

merge([], X, X).
merge([A | X], [B | Y], [A | Z]) :-
	A =< B,
	merge(X, [B | Y], Z).
merge([A | X], [B | Y], [B | Z]) :-
	A > B,
	merge([A | X], Y, Z).

:- pred s(list(T), list(T), list(T)).
:- mode s(in, out, out).

s([], [], []).
s([E | U], [E | V], W) :-
	s(U, W, V).
