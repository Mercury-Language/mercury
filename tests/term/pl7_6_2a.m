:- module pl7_6_2a.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred reach(T::in, T::in, list(list(T))::in) is semidet.

:- implementation.

reach(X, Y, Edges) :-
	member([X, Y], Edges).
reach(X, Z, Edges) :-
	member([X, Y], Edges),
	reach(Y, Z, Edges).

:- pred member(T, list(T)).
:- mode member(in, in).
:- mode member(out, in).

member(H, [H | _L]).
member(X, [_H | L]) :-
	member(X, L).
