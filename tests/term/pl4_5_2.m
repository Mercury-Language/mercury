:- module pl4_5_2.

:- interface.

:- type expr	--->	expr + expr
		;	num(int).

:- pred s(expr::in, expr::out) is nondet.

:- implementation.

:- import_module int, prolog.

s(A+(B+C), D) :-
	s((A+B)+C, D).
s(A+B, C) :-
	s(B+A, C).
s(X+num(0), X).
s(X+Y, Z) :-
	s(X, A),
	s(Y, B),
	s(A+B, Z).
s(A+B, C) :-
	A = num(Anum),
	B = num(Bnum),
	Cnum is Anum + Bnum,
	C = num(Cnum).
