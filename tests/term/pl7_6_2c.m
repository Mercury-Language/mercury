:- module pl7_6_2c.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred reach(T::in, T::in, list(list(T))::in, list(T)::in) is semidet.

:- implementation.

reach(X, Y, Edges, _Not_Visited) :-
	member([X, Y], Edges).
reach(X, Z, Edges, Not_Visited) :-
	member([X, Y], Edges),
	member(Y, Not_Visited),
	delete(Y, Not_Visited, V1),
	reach(Y, Z, Edges, V1).

:- pred member(T, list(T)).
:- mode member(in, in).
:- mode member(out, in).

member(H, [H | _L]).
member(X, [_H | L]) :-
	member(X, L).

:- pred delete(T, list(T), list(T)).
:- mode delete(out, in, out).

delete(X, [X | Y], Y).
delete(X, [H | T1], [H | T2]) :-
	delete(X, T1, T2).
