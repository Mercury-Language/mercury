:- module dds1_2.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred permute(list(T)::in, list(T)::out) is nondet.

:- implementation.

permute([], []).
permute([X|Y], [U|V]) :-
	delete(U, [X|Y], W),
	permute(W, V).

:- pred delete(T, list(T), list(T)).
:- mode delete(out, in, out).

delete(X, [X|Y], Y).
delete(U, [X|Y], [X|Z]) :-
	delete(U, Y, Z).
