:- module arit_exp.

:- interface.

:- type expr	--->	expr + expr ; expr * expr ; - expr ; int(int).

:- pred e(expr).
:- mode e(in) is semidet.

:- implementation.

e(X+Y) :-
	f(X),
	e(Y).
e(X) :-
	f(X).

:- pred f(expr).
:- mode f(in) is semidet.

f(X*Y) :-
	g(X),
	f(Y).
f(X) :-
	g(X).

:- pred g(expr).
:- mode g(in) is semidet.

g(-(X)) :-
	e(X).
g(int(_X)).
