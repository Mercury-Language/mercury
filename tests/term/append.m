:- module append.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred app(list(T), list(T), list(T)).
:- mode app(out, out, in) is multi.

:- implementation.

app([X|Xs], Ys, [X|Zs]) :-
	app(Xs, Ys, Zs).
app([], Ys, Ys).
