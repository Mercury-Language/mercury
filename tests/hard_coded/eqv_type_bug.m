:- module try.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module cqueue.

:- import_module int, list, string.

main -->
	{ cqueue__cqueue(CQ0) },
	{ copy(CQ0, CQ0c) },
	{ list__foldl((pred(I::in, Q0::in, Q::out) is det :-
		cqueue__append(Q0, I, Q)), [1,2,3,4,5], CQ0c, CQ1) },
	{ copy(CQ1, CQ1c) },
	write(CQ1c), nl,
	{ cqueue__next(CQ1c, CQ2) },
	{ copy(CQ2, CQ2c) },
	write(CQ2c), nl.


