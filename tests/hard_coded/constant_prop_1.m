:- module constant_prop_1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, int, float.

main -->
	io.write_string("foo" ++ "bar"), io.nl,
	io.write_int(1 * 1000 + 2 * 100 + 3 * 10 + 4), io.nl,
	io.write_float(5.0 * 1000.0 + 6.0 * 100.0 + 7.0 * 10.0 + 8.0), io.nl.
