:- module pl8_4_1.

:- interface.

:- type nat	--->	zero ; s(nat).

:- pred even(nat::in) is semidet.
:- pred odd(nat::in) is semidet.

:- implementation.

even(zero).
even(s(X)) :-
	odd(X).

odd(s(X)) :-
	even(X).
