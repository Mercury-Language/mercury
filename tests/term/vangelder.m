% The vangelder example from Lindenstrauss and Sagiv
% (takes them more than four minutes to crack).

:- module vangelder.

:- interface.

:- type foo	--->	a ; b ; c ; f(foo).

:- pred q(foo, foo).
:- mode q(in, in) is semidet.

:- implementation.

q(X, Y) :-
	e(X, Y).
q(X, f(f(X))) :-
	p(X, f(f(X))),
	q(X, f(X)).
q(X, f(f(Y))) :-
	p(X, f(Y)).

:- pred p(foo, foo).
:- mode p(in, in) is semidet.

p(X, Y) :-
	e(X, Y).
p(X, f(Y)) :-
	r(X, f(Y)),
	p(X, Y).

:- pred r(foo, foo).
:- mode r(in, in) is semidet.

r(X, Y) :-
	e(X, Y).
r(X, f(Y)) :-
	q(X, Y),
	r(X, Y).
r(f(X), f(X)) :-
	t(f(X), f(X)).

:- pred t(foo, foo).
:- mode t(in, in) is semidet.

t(X, Y) :-
	e(X, Y).
t(f(X), f(Y)) :-
	q(f(X), f(Y)),
	t(X, Y).

:- pred e(foo, foo).
:- mode e(in, in) is semidet.

e(a, b).
