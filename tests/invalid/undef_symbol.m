:- module undef_symbol.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	p,
	q.

:- pred p(io__state::di, io__state::uo) is det.

p -->
	{ string__append("hello ", "world.\n", Str) },
	io__write_string(Str).

:- pred q(io__state::di, io__state::uo) is det.

q -->
	{ Context = term__context("random", 17) },
	write(Context), nl.

