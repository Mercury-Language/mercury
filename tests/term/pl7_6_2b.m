:- module pl7_6_2b.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred reach(T::in, T::in, list(list(T))::in, list(T)::in) is semidet.

:- implementation.

reach(X, Y, Edges, _Visited) :-
	member([X, Y], Edges).
reach(X, Z, Edges, Visited) :-
	member([X, Y], Edges),
	\+ member(Y, Visited),
	reach(Y, Z, Edges, [Y | Visited]).

:- pred member(T, list(T)).
:- mode member(in, in).
:- mode member(out, in).

member(H, [H | _L]).
member(X, [_H | L]) :-
	member(X, L).
