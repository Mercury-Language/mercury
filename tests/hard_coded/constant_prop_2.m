:- module constant_prop_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, int, float.

main -->
	( { "abc" ++ "xyz" = "abcxyz" } ->
		io.write_string("yes"), io.nl
	;
		link_error
	),
	( { 5 * 10000 + 4 * 1000 + 3 * 100 + 2 * 10 + 1 = 54321 } ->
		io.write_string("yes"), io.nl
	;
		link_error
	),
	( { 4.0 * 1000.0 + 3.0 * 100.0 + 2.0 * 10.0 + 1.0 = 4321.0 } ->
		io.write_string("yes"), io.nl
	;
		link_error
	).

	% We should be able to optimize away all calls to this procedure
	% at compile time, so we should not even emit a reference to it.
:- pred link_error(io::di, io::uo) is det.
:- external(link_error/2).
