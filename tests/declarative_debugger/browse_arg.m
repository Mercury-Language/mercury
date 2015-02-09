:- module browse_arg.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	{ p(1, X) },
	io__write(X),
	io__nl.

:- type foo
	--->	bar
	;	baz(int, foo).

:- pred p(int, foo).
:- mode p(in, out) is det.

p(N, baz(N, bar)).

