:- module easy_nondet_test.

:- pred q(int::out) is nondet.

q(X) :-
	p(X).

:- pred p(int::out) is multi.

p(X) :- 
	X = 1 ; X = 2.

:- pred r(int::in, int::out) is nondet.

r(X, Y) :-
	if X = 3 then
		p(Y)
	else
		q(Y).

:- pred s(int::in, int::out) is nondet.

s(X, Y) :-
	if r(X, Z) then
		s(Z, Y)
	else
		q(Y).

