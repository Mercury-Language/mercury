% This is a regression test for bugs relating to handling of non-ASCII
% characters.

:- module nonascii.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, char, string, list.

main(!IO) :-
	io__open_input("nonascii.data", Result, !IO),
	(
		Result = ok(Stream),
		test1(Stream, !IO),
		test2(Stream, 255, !IO)
	;
		Result = error(Error),
		io__error_message(Error, Msg),
		io__write_string(Msg, !IO)
	).

:- pred test1(io__input_stream::in, io::di, io::uo) is det.

test1(Stream, !IO) :-
	io__read_line_as_string(Stream, Result, !IO),
	(
		Result = ok(Line),
		Chars = string__to_char_list(Line),
		Ints = list__map(char__to_int, Chars),
		io__write_list(Ints, ",\n", io__write_int, !IO),
		io__nl(!IO)
	;
		Result = eof,
		io__write_string("premature EOF\n", !IO)
	;
		Result = error(Error),
		io__error_message(Error, Msg),
		io__write_string(Msg, !IO)
	).

:- pred test2(io__input_stream::in, int::in, io::di, io::uo) is det.

test2(Stream, N, !IO) :-
	( N > 0 ->
		io__read_char(Stream, Result, !IO),
		(
			Result = ok(Char),
			Int = char__to_int(Char),
			io__write_int(Int, !IO),
			io__nl(!IO)
		;
			Result = eof,
			io__write_string("premature EOF\n", !IO)
		;
			Result = error(Error),
			io__error_message(Error, Msg),
			io__write_string(Msg, !IO)
		),
		test2(Stream, N - 1, !IO)
	;
		true
	).
