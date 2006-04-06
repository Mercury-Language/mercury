:- module combine.
	
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, solutions.

main(!IO) :-
	solutions(a, Solns),
	io__write(Solns, !IO),
	io__nl(!IO).

:- pred a(int::out) is nondet.
:- pred b(int::out) is multi.
:- pred c(int::out) is multi.
:- pred d(int::out) is nondet.
:- pred e(int::out) is nondet.

:- pragma minimal_model(d/1).
:- pragma minimal_model(e/1).

a(A) :-
	d(B),
	e(C),
	A = B * 10 + C.

d(A) :-
	b(A).

e(A) :-
	c(A).

b(3).
b(4).

c(5).
c(6).
