:- module pl8_3_1a.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred xminsort(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

xminsort([], []).
xminsort(L, [X | L1]) :-
	xmin1(X, L),
	remove(X, L, L2),
	xminsort(L2, L1).

:- pred xmin1(int, list(int)).
:- mode xmin1(out, in).

xmin1(M, [X | L]) :-
	xmin2(X, M, L).

:- pred xmin2(int, int, list(int)).
:- mode xmin2(in, out, in).

xmin2(X, X, []).
xmin2(X, A, [M | L]) :-
	xmin(X, M, B),
	xmin2(B, A, L).

:- pred xmin(int, int, int).
:- mode xmin(in, in, out).

xmin(X, Y, X) :-
	X =< Y.
xmin(X, Y, Y) :-
	X > Y.

:- pred remove(T, list(T), list(T)).
:- mode remove(in, in, out).

%remove(N, [], []).   (this case cannot occur in our program)
remove(N, [N | L], L).
remove(N, [M | L], [M | L1]) :-
	\+ N = M,
	remove(N, L, L1).
