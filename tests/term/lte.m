:- module lte.

:- interface.

:- pred goal is semidet.

:- implementation.

:- type nat	--->	zero ; s(nat).

goal :-
	lte(X, s(s(s(s(zero))))),
	even(X).

:- pred lte(nat, nat).
:- mode lte(out, in).

lte(s(X), s(Y)) :-
	lte(X, Y).
lte(zero, _Y).

:- pred even(nat).
:- mode even(in).

even(s(s(X))) :-
	even(X).
even(zero).
