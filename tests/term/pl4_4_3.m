:- module pl4_4_3.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred merge(list(int)::in, list(int)::in, list(int)::out) is nondet.

:- import_module int.

:- implementation.

merge(X, [], X).
merge([], X, X).
merge([A | X], [B | Y], [A | Z]) :-
	A =< B,
	merge(X, [B | Y], Z).
merge([A | X], [B | Y], [B | Z]) :-
	A > B,
	merge([A | X], Y, Z).
