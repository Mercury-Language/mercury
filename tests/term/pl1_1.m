:- module pl1_1.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred append(list(T), list(T), list(T)).
:- mode append(in, in, out) is det.
:- mode append(out, out, in) is multi.
% :- mode append(out, in, out) is nondet. NOT WELL MODED

:- implementation.

append([], L, L).
append([H | L1], L2, [H | L3]) :-
	append(L1, L2, L3).

