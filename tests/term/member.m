:- module member.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred member(T::out, list(T)::in) is nondet.

:- implementation.

member(X, [_Y| Xs]) :-
	member(X, Xs).
member(X, [X| _Xs]).
