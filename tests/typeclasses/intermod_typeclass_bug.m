:- module intermod_typeclass_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module intermod_typeclass_bug2.
:- import_module char.

main -->
	{ p('a', Int1) },
	( { char__to_int(Char1, Int1) } ->
		io__write_string("Test 1 succeeded: "),
		io__write_char(Char1),
		io__nl
	;
		io__write_string("Test 1 failed: "),
		io__write_int(Int1),
		io__nl
	),

	( { p(Char2, char__to_int('b')) } ->
		io__write_string("Test 2 succeeded: "),
		io__write_char(Char2),
		io__nl
	;
		io__write_string("Test 2 failed"),
		io__nl
	),

	( { Int3 = q('c'), char__to_int(Char3, Int3) } ->
		io__write_string("Test 3 succeeded: "),
		io__write_char(Char3),
		io__nl
	;
		io__write_string("Test 3 failed"),
		io__nl
	).


