:- module pl3_5_6a.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred p(list(int)::out) is nondet.

:- implementation.

p([A]) :-
	l([A]).

:- pred q(list(T)).
:- mode q(in).

q([_A]).

:- pred r(int).
:- mode r(out).

r(1).

:- pred l(list(int)).
:- mode l(out).		% DIAGNOSED BY COMPILER

l([]).
l([H | T]) :-
	r(H),
	l(T).
