% This is a regression test for a bug with duplicate call elimination
% not taking types into account.

:- module dupcall_types_bug.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string.

main -->
	{ FileName = "string",
	  String = "1. ",
	  string__length(String, Len),
	  Posn0 = posn(1, 0, 0),
	  read_from_string(FileName, String, Len, Int, Posn0, _),
	  read_from_string(FileName, String, Len, Str, Posn0, _) },
	( { Int = ok(I), Str = ok(S) } ->
		io__write_int(I),
		io__write_string(S),
		io__nl
	;
		io__write_string("Syntax error.\n")
	).
