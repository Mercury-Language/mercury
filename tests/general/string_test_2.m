:- module string_test_2.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string.

main -->
	{ string__append("abc", "def", S1) },
	io__write_string(S1),
	io__write_string("\n"),
	{ string__append("", "abcdef", S2) },
	io__write_string(S2),
	io__write_string("\n"),
	{ string__append("abcdef", "", S3) },
	io__write_string(S3),
	io__write_string("\n"),
	( { string__append("", S4, "abcdef") } ->
		io__write_string(S4),
		io__write_string("\n")
	;
		io__write_string("failed\n")
	),
	( { string__append("abc", S5, "abcdef") } ->
		io__write_string(S5),
		io__write_string("\n")
	;
		io__write_string("failed\n")
	).



/*
	( { string__append(S6, "", "abcdef") } ->
		io__write_string(S6),
		io__write_string("\n")
	;
		io__write_string("failed\n")
	),
	( { string__append(S7, "def", "abcdef") } ->
		io__write_string(S7),
		io__write_string("\n")
	;
		io__write_string("failed\n")
	).
*/

