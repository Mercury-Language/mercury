:- module dds3_13.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred duplicate(list(T)::in, list(T)::out) is det.

:- implementation.

duplicate([], []).
duplicate([X | Y], [X, X | Z]) :-
	duplicate(Y, Z).
