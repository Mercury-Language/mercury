:- module fold.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- type sym	--->	a ; b ; c ; d.

:- pred fold(sym, list(sym), sym).
:- mode fold(in, in, out) is semidet.

:- implementation.

fold(X, [Y | Ys], Z) :-
	xop(X, Y, V),
	fold(V, Ys, Z).
fold(X, [], X).

:- pred xop(sym, sym, sym).
:- mode xop(in, in, out).

xop(a, b, c).
