:- module intermod_c_code.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module intermod_c_code2.

main -->
	{ c_code("Hello, world\n", Y) },
	io__write(Y),
	io__nl.

