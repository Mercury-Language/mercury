:- module list.
:- import_module integer.
:- interface.

:- type list(T) ---> [] ; T.list(T).

:- pred append(list(T), list(T), list(T)).
:- pred member(T, list(T), list(T)).
:- pred member(T, list(T)).
:- pred length(list(T), integer).
:- pred condense(list(list(T)),list(T)).

:- implementation.

condense([], []).
condense([L|Ls], R) :-
	condense(Ls, R1),
	append(L, R1, R).

