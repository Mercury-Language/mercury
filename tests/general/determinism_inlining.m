% This is a regression test.  The compiler inlined foo/1 without
% inlining the correct determinism, and the result was a nondet
% unification.  -- bromage 30/5/1997

:- module determinism_inlining.
:- interface.

:- import_module io.

:- pred main(io__state :: di, io__state :: uo) is cc_multi.
:- implementation.

:- import_module int.

main -->
	{ foo(X) },
	io__write_int(X),
	io__nl.

:- pred foo(int :: out) is multi.

foo(42).

