:- module pl8_2_1.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred mergesort(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

mergesort([], []).
mergesort([E], [E]).
mergesort([E, F | U], V) :-
	s([E, F | U], W, Y),
	mergesort(W, X),
	mergesort(Y, Z),
	merge(X, Z, V).

:- pred merge(list(int), list(int), list(int)).
:- mode merge(in, in, out).

merge(X, [], X).
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
