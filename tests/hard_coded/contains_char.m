:- module contains_char.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module char, string.

main -->
	(
		{ char__to_int(Nul, 0) },
		{ string__contains_char("", Nul) }
	->
		io__write_string("test failed\n")
	;
		io__write_string("test succeeded\n")
	).

