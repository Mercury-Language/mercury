
:- module lambda_multi_constraint_same_tvar.

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

main -->
	main2(41, 42).

main2(XX, Y) -->
	{ Foo = (pred(X::in, di, uo) is det -->
		p1(X), p2(X), nl,
		p1(Y), p2(Y), nl
	) },
	Foo(XX).

