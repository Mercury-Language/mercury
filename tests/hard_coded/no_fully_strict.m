:- module no_fully_strict.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module require.

main -->
	{ error("oops") }.
