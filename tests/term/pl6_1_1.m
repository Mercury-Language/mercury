:- module pl6_1_1.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred qsort(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

qsort([], []).
qsort([H | L], S) :-
	split(L, H, A, B),
	qsort(A, A1),
	qsort(B, B1),
	append(A1, [H | B1], S).

:- pred split(list(int), int, list(int), list(int)).
:- mode split(in, in, out, out).

split([], _Y, [], []).
split([X | Xs], Y, [X | Ls], Bs) :-
	X =< Y,
	split(Xs, Y, Ls, Bs).
split([X | Xs], Y, Ls, [X | Bs]) :-
	X > Y,
	split(Xs, Y, Ls, Bs).

:- pred append(list(T), list(T), list(T)).
:- mode append(in, in, out).

append([], L, L).
append([H | L1], L2, [H | L3]) :-
	append(L1, L2, L3).
