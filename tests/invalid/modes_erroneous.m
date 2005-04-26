:- module modes_erroneous.

:- interface.

:- type foo.

:- implementation.

:- type foo ---> foo.

:- pred p(foo, foo).
:- mode p(ground >> ground, free >> ground).

p(_, X) :-
	p(_, X).
p(_, _).
