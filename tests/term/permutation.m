:- module permutation.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred perm(list(T)::in, list(T)::out) is nondet.

:- implementation.

perm(Xs, [X | Ys]) :-
	app(X1s, [X | X2s], Xs),
	app(X1s, X2s, Zs),
	perm(Zs, Ys).
perm([], []).

:- pred app(list(T), list(T), list(T)).
:- mode app(out, out, in).
:- mode app(in, in, out).

app([X | Xs], Ys, [X | Zs]) :-
	app(Xs, Ys, Zs).
app([], Ys, Ys).
