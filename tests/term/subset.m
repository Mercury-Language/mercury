:- module subset.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred subset(list(T), list(T)).
:- mode subset(in, in) is semidet.
:- mode subset(out, in) is multi.	% gets warning

:- implementation.

subset([X | Xs], Ys) :-
	member(X, Ys),
	subset(Xs, Ys).
subset([], _Ys).

:- pred member(T, list(T)).
:- mode member(out, in).

member(X, [_Y | Xs]) :-
	member(X, Xs).
member(X, [X | _Xs]).
