:- module solns.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module solutions, list.

main -->
	{ p(1, Ss) },
	io__write(Ss),
	io__nl.

:- pred p(int::in, list(int)::out) is det.

p(N, Ss) :-
	solutions(q(N), Ss).

:- pred q(int::in, int::out) is nondet.

q(0, 0).
q(1, 1).
q(1, 2).
q(1, 3).
q(2, 2).
q(2, 4).

