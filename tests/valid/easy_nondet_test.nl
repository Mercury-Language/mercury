:- module t3.

:- pred q(int::out) is nondet.

q(X) :-
	p(X).

:- pred p(int::out) is nondet.

p(X) :- 
	X = 1 ; X = 2.
