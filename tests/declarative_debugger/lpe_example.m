:- module lpe_example.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module solutions, int.

main -->
	{ solutions(p(1), Ss) },
	io__write(Ss),
	io__nl.


:- pred p(int, int).
:- mode p(in, out) is nondet.

p(0, 0).
p(1, X) :-
	q(A),
	(
		r(A, X)
	;
		X = A
	).

:- pred q(int).
:- mode q(out) is det.

q(3).

:- pred r(int, int).
:- mode r(in, out) is multi.

r(X, X + 10).
r(X, X + 20).

