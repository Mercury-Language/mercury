:- module big.
:- interface.
:- import_module io.
:- pred main(io__state, io__state).
:- mode main(di, uo) is det.
:- implementation.
:- import_module bool.
:- import_module library_forwarding.

main -->
	{
		p(1)
	->
		R = yes
	;
		R = no
	},
	io__write(R),
	io__write_string(".\n").


:- pred p(int::out) is nondet.

p(X) :-
	a(A),
	(
		b(A, B),
		(
			c(B, C),
			\+ (
				d(C, D),
				D > 5
			)
		->
			c(C, E)
		;
			e(B, E)
		)
	;
		f(A, F),
		b(F, B),
		(
			B = 0,
			g(1, E)
		;
			B = 1,
			g(10, E)
		),
		\+ (
			f(E, H),
			c(H, I),
			g(I, J),
			J > 0
		)
	),
	f(E, X).


:- pred a(int::out) is det.
:- pred b(int::in, int::out) is multi.
:- pred c(int::in, int::out) is nondet.
:- pred d(int::in, int::out) is semidet.
:- pred e(int::in, int::out) is multi.
:- pred f(int::in, int::out) is det.
:- pred g(int::in, int::out) is semidet.

a(0).

b(X, X).
b(X, X+1).

c(0, 2).
c(0, 3).
c(1, 15).
c(2, 6).
c(2, 7).
c(3, 17).
c(4, 8).
c(4, 9).

d(X, X*3) :-
	X mod 2 = 1.

e(X, X*10).
e(X, X*11).

f(X, X * -2).

g(1, -1).
g(6, -10).
g(7, -11).
g(8, -12).
g(9, 99).
g(10, -2).

