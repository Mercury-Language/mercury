:- module mergesort_t.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred mergesort(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

mergesort([], []).
mergesort([X], [X]).
mergesort([X, Y | Xs], Ys) :-
	split2([X, Y | Xs], X1s, X2s),
	mergesort(X1s, Y1s),
	mergesort(X2s, Y2s),
	merge(Y1s, Y2s, Ys).

:- pred split(list(T), list(T), list(T)).
:- mode split(in, out, out).

split(Xs, Ys, Zs) :-
	split0(Xs, Ys, Zs).
split(Xs, Ys, Zs) :-
	split1(Xs, Ys, Zs).
split(Xs, Ys, Zs) :-
	split2(Xs, Ys, Zs).

:- pred split0(list(T), list(T), list(T)).
:- mode split0(in, out, out).

split0([], [], []).

:- pred split1(list(T), list(T), list(T)).
:- mode split1(in, out, out).

split1([X], [X], []).

:- pred split2(list(T), list(T), list(T)).
:- mode split2(in, out, out).

split2([X, Y | Xs], [X | Ys], [Y | Zs]) :-
	split(Xs, Ys, Zs).

:- pred merge(list(int), list(int), list(int)).
:- mode merge(in, in, out).

merge([], Xs, Xs).
merge(Xs, [], Xs).
merge([X | Xs], [Y | Ys], [X | Zs]) :-
	X=<Y,
	merge(Xs, [Y | Ys], Zs).
merge([X | Xs], [Y | Ys], [X | Zs]) :-
	X>Y,
	merge([X | Xs], Ys, Zs).
