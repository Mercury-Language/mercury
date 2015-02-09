
:- module multi_parameter.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module char.

:- typeclass m(A, B) where [
	pred a(A, B),
	mode a(in, out) is det
].

:- instance m(char, int) where [
	pred(a/2) is char__to_int
].

main -->
	{ foo('z', X) },
	io__write_int(X),
	io__nl.

:- pred foo(A, B) <= m(A,B).
:- mode foo(in, out) is det.
:- pragma no_inline(foo/2).

foo(X, Y) :- a(X, Y).
