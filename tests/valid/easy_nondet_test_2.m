:- module easy_nondet_test_2.

:- pred p(int::out) is nondet.

:- pred q(int::out) is nondet.

q(1).
q(2).

:- pred r(int::out) is nondet.

r(3).
r(4).

p(X) :-
	q(X) ; r(X).
