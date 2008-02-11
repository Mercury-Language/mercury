:- module string_test_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
	string.append("abc", "def", S1),
	io.write_string(S1, !IO),
	io.write_string("\n", !IO),
	string.append("", "abcdef", S2),
	io.write_string(S2, !IO),
	io.write_string("\n", !IO),
	string.append("abcdef", "", S3),
	io.write_string(S3, !IO),
	io.write_string("\n", !IO),
	( string.append("", S4, "abcdef") ->
		io.write_string(S4, !IO),
		io.write_string("\n", !IO)
	;
		io.write_string("failed\n", !IO)
	),
	( string.append("abc", S5, "abcdef") ->
		io.write_string(S5, !IO),
		io.write_string("\n", !IO)
	;
		io.write_string("failed\n", !IO)
	),
	( string.remove_prefix("abc", "abcdef", S6) ->
		io.write_string(S6, !IO),
		io.nl(!IO)
	;
		io.write_string("failed\n", !IO)
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

