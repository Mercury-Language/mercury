:- module pl4_01.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred append3(list(T), list(T), list(T), list(T)).
:- mode append3(in, in, in, out) is det.
:- mode append3(out, out, out, in) is multi.

:- implementation.

append3(A, B, C, D) :-
	append(A, B, E),
	append(E, C, D).

:- pred append(list(T), list(T), list(T)).
:- mode append(in, in, out).
:- mode append(out, out, in).

append([], L, L).
append([H | L1], L2, [H | L3]) :-
	append(L1, L2, L3).
