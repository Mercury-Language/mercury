:- module modes_erroneous.

:- type foo.

:- pred p(foo, foo).
:- mode p(ground >> ground, free >> ground).

p(_, X) :-
	p(_, X).
p(_, _).
