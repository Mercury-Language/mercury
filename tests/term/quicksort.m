:- module quicksort.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred qs(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.

qs([X | Xs], Ys) :-
	part(X, Xs, Littles, Bigs),
	qs(Littles, Ls),
	qs(Bigs, Bs),
	app(Ls, [X | Bs], Ys).
qs([], []).

:- pred part(int, list(int), list(int), list(int)).
:- mode part(in, in, out, out).

part(X, [Y | Xs], [Y | Ls], Bs) :-
	X > Y,
	part(X, Xs, Ls, Bs).
part(X, [Y | Xs], Ls, [Y | Bs]) :-
	X =< Y,
	part(X, Xs, Ls, Bs).
part(_X, [], [], []).

:- pred app(list(int), list(int), list(int)).
:- mode app(in, in, out).

app([X | Xs], Ys, [X | Zs]) :-
	app(Xs, Ys, Zs).
app([], Ys, Ys).
