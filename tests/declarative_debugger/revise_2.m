:- module revise_2.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	{ p(X) },
	io__write_int(X),
	io__nl.

:- pred p(int::out) is det.

p(41).

