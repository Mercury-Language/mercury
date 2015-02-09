:- module gcf.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module maybe, int.

main -->
	{
		a(X),
		X > 15
	->
		R = yes(X)
	;
		R = no
	},
	io__write(R),
	io__nl.


:- pred a(int::out) is nondet.
:- pred g(int::out) is multi.
:- pred c(int::in, int::out) is nondet.
:- pred f(int::in) is semidet.

a(X) :-
	g(Y),
	c(Y, X),
	f(X).

g(1).
g(2).
g(3).
g(4).

c(2, 10).
c(2, 11).
c(2, 12).
c(3, 20).

f(X) :-
	X > 10.

