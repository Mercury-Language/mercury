:- module overlap.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred overlap(list(T)::in, list(T)::in) is semidet.

:- implementation.

overlap(Xs, Ys) :-
	member(X, Xs),
	member(X, Ys).

% has_a_or_b(Xs) :- overlap(Xs, [a, b]).

:- pred member(T, list(T)).
:- mode member(out, in).

member(X, [_Y | Xs]) :-
	member(X, Xs).
member(X, [X | _Xs]).
