:- module combine.
	
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, std_util.

main -->
	{ solutions(a, Solns) },
	io__write(Solns),
	io__nl.

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
