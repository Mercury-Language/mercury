:- module pl4_4_6a.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred perm(list(T)::in, list(T)::out) is nondet.

:- implementation.

perm([], []).
perm([X | L], Z) :-
	perm(L, Y),
	insert(X, Y, Z).

:- pred insert(T, list(T), list(T)).
:- mode insert(in, in, out).

insert(X, [], [X]).
insert(X, L, [X | L]).
insert(X, [H | L1], [H | L2]) :-
	insert(X, L1, L2).
