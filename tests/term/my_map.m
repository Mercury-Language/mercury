:- module my_map.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred map(list(x)::in, list(x)::out) is semidet.

:- type x	--->	val_i ; val_j ; val_k.

:- implementation.

:- pred p(x, x).
:- mode p(in, out).

p(val_i, val_j).

map([X | Xs], [Y | Ys]) :-
	p(X, Y),
	map(Xs, Ys).
map([], []).
