% test taking the address of builtins such as '<'/2.

:- module address_of_builtins.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, require.

main -->
	{ X = 3 },
	{ require(X > 0, "oops, X is not positive!") },
	{ require(1 > 0, "oops, 1 > 0 failed!") },
	{ require(1 < X, "oops, 1 < X failed!") },
	io__write_string("\n").
