
:- module multi_constraint_same_tvar.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- typeclass c1(T) where [
	pred p1(T::in, io__state::di, io__state::uo) is det
].

:- instance c1(int) where [
	pred(p1/3) is io__write_int
].

:- typeclass c2(T) where [
	pred p2(T::in, io__state::di, io__state::uo) is det
].

:- instance c2(int) where [
	pred(p2/3) is io__write_int
].

:- pred foo(T, io__state, io__state) <= (c1(T), c2(T)).
:- mode foo(in, di, uo) is det.

foo(X) -->
	p1(X),
	p2(X),
	io__nl.

main --> foo(42).
