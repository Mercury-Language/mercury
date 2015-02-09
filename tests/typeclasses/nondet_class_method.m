
:- module nondet_class_method.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module int.

:- typeclass c(T) where [
	pred a(T::out) is multi
].

:- instance c(int) where [
	pred(a/1) is foo
].

:- pred foo(int::out) is multi.
foo(1).
foo(2).
foo(3).
foo(4).
foo(5).
foo(6).

:- pred b(T) <= c(T).
:- mode b(out) is multi.
:- pragma no_inline(b/1).

b(X) :- a(X).

main -->
	(
		{ b(X) },
		{ X > 3 }
	->
		io__write_int(X)
	;
		io__write_string("failed")
	),
	io__nl.
