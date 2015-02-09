:- module multi_constraint_diff_tvar.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- typeclass c(T) where [
	pred p(T::in, io__state::di, io__state::uo) is det
].

:- instance c(int) where [
	pred(p/3) is io__write_int
].

:- pred foo(T1, T2, io__state, io__state) <= (c(T1), c(T2)).
:- mode foo(in, in, di, uo) is det.

foo(X, Y) -->
	p(X),
	p(Y),
	io__nl.

main --> foo(42, 24).
