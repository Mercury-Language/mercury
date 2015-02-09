% Test the declarative debugger's handling of I/O streams.

:- module io_stream_test.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, char, int.

main -->
	io__open_input("tabled_read_decl.data", Res),
	( { Res = ok(Stream) } ->
		io_stream_test__part_1(Stream),
		io_stream_test__part_2(Stream)
	;
		io__write_string("could not open tabled_read.data\n")
	).

:- pred io_stream_test__part_1(io__input_stream::in,
	io__state::di, io__state::uo) is det. 

io_stream_test__part_1(Stream) -->
	io_stream_test__test(Stream, A),
	io__write_int(A),
	io__nl.

:- pred io_stream_test__part_2(io__input_stream::in,
	io__state::di, io__state::uo) is det.

io_stream_test__part_2(Stream) -->
	io_stream_test__test(Stream, A),
	io__write_int(A),
	io__nl.

:- pred io_stream_test__test(io__input_stream::in, int::out,
	io__state::di, io__state::uo) is det.

io_stream_test__test(Stream, N) -->
		% BUG: the 1 should be 0
	io_stream_test__test_2(Stream, 1, N).

:- pred io_stream_test__test_2(io__input_stream::in, int::in, int::out,
	io__state::di, io__state::uo) is det.

io_stream_test__test_2(Stream, SoFar, N) -->
	io__read_char(Stream, Res),
	(
		{ Res = ok(Char) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		io_stream_test__test_2(Stream, SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).
