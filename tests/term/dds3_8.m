:- module dds3_8.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred reverse(list(T)::in, list(T)::out, list(T)::in) is det.

:- implementation.

reverse([], X, X).
reverse([X | Y], Z, U) :-
	reverse(Y, Z, [X | U]).
