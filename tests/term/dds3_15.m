:- module dds3_15.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred merge(list(int)::in, list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

merge([], X, X).
merge(X, [], X).
merge([X | Xs], [Y | Ys], [X | Zs]) :-
	X =< Y,
	merge(Xs, [Y | Ys], Zs).
merge([X | Xs], [Y | Ys], [Y | Zs]) :-
	Y > X,
	merge([X | Xs], Ys, Zs).
