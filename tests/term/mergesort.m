:- module mergesort.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred mergesort(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

mergesort([], []).
mergesort([X], [X]).
mergesort([X, Y | Xs], Ys) :-
	split([X, Y | Xs], X1s, X2s),
	mergesort(X1s, Y1s),
	mergesort(X2s, Y2s),
	merge(Y1s, Y2s, Ys).

:- pred split(list(T), list(T), list(T)).
:- mode split(in, out, out).

split([], [], []).
split([X | Xs], [X | Ys], Zs) :-
	split(Xs, Zs, Ys).

:- pred merge(list(int), list(int), list(int)).
:- mode merge(in, in, out).

merge([], Xs, Xs).
merge(Xs, [], Xs).
merge([X | Xs], [Y | Ys], [X | Zs]) :-
	X =< Y,
	merge(Xs, [Y | Ys], Zs).
merge([X | Xs], [Y | Ys], [Y | Zs]) :-
	X > Y,
	merge([X | Xs], Ys, Zs).
