% this is a version of mergesort that appears in
% K. A. Apt and D. Pedreschi, Modular Termination Proofs for Logic and Pure
% Prolog Programs, Dipartimento di Informatica, Universita di Pisa, 1993
% mergesort(Xs, Ys, Xs) if Ys is an ordered permutation of the list Xs

:- module mergesort_ap.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred mergesort(list(int)::in, list(int)::out, list(int)::in) is nondet.

:- implementation.

:- import_module int.

mergesort([], [], _Ls).
mergesort([X], [X], _Ls).
mergesort([X, Y | Xs], Ys, [H | Ls]) :-
	split([X, Y | Xs], X1s, X2s, [H | Ls]),
	mergesort(X1s, Y1s, Ls),
	mergesort(X2s, Y2s, Ls), 
	merge(Y1s, Y2s, Ys, [H | Ls]).

:- pred split(list(T), list(T), list(T), list(T)).
:- mode split(in, out, out, in).

split([], [], [], _Ls).
split([X | Xs], [X | Ys], Zs, [_H | Ls]) :-
	split(Xs, Zs, Ys, Ls).

:- pred merge(list(int), list(int), list(int), list(int)).
:- mode merge(in, in, out, in).

merge([], Xs, Xs, _Ls).
merge(Xs, [], Xs, _Ls).
merge([X | Xs], [Y | Ys], [X | Zs], [_H | Ls]) :-
	X =< Y,
	merge(Xs, [Y | Ys], Zs, Ls).
merge([X | Xs], [Y | Ys], [Y | Zs], [_H | Ls]) :-
	X > Y,
	merge([X | Xs], Ys, Zs, Ls).
