:- module intermod_foreign_type.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module intermod_foreign_type2.
:- import_module std_util.

main -->
	{ C = new(4, 5) },
	io__write_string("X:"),
	io__write_int(x(C)),
	io__nl,
	io__write_string("Y:"),
	io__write_int(y(C)),
	io__nl,
	io__write(coord(1, 2)),
	io__nl.

:- type coord2
	---> coord2(int, int).

:- func coord(int, int) = coord2.

coord(X, Y) = coord2(X, Y).
