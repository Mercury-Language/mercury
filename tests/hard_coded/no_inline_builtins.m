:- module no_inline_builtins.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
	io__write_int(40 + 2),
	io__nl.
