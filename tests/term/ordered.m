:- module ordered.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred ordered(list(int)::in) is semidet.

:- implementation.

:- import_module int.

ordered([]).
ordered([_X]).
ordered([X,Y | Xs]) :-
	X =< Y,
	ordered([Y | Xs]).
