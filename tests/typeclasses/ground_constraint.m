:- module ground_constraint.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list.

main -->
	{ S1 = f(0) },
	io__write_string(S1),
	io__nl,
	{ p(0, S2) },
	io__write_string(S2),
	io__nl,
	{ q([0], S3) },
	io__write_string(S3),
	io__nl.

:- typeclass foo(T) where [
	func s(T) = string
].

:- instance foo(int) where [
	(s(_) = "bar")
].

:- func f(int) = string <= foo(int).
f(N) = s(N).

:- pred p(int::in, string::out) is det <= foo(int).
p(N, s(N)).

:- instance foo(list(T)) <= foo(T) where [
	(s(_) = "baz")
].

:- pred q(list(int)::in, string::out) is det <= foo(list(int)).
q(Ns, s(Ns)).

