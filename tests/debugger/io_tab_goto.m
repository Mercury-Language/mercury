:- module io_tab_goto.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, char, int.

main(!IO) :-
	goto(!IO),
	io_tab_goto.open_input("io_tab_goto.data", Res, Stream, !IO),
	( Res = 0 ->
		io_tab_goto.part_1(Stream, !IO),
		io_tab_goto.part_2(Stream, !IO)
	;
		io.write_string("could not open io_tab_goto.data\n", !IO)
	).

:- pred goto(io::di, io::uo) is det.

:- pragma no_inline(goto/2).

:- pragma foreign_proc(c, goto(IO0::di, IO::uo),
		[tabled_for_io, promise_pure], "
	printf(""should see this printf\\n"");
	goto label;
	printf(""should never see this printf\\n"");
label:
	IO = IO0;
").

:- pred io_tab_goto.part_1(c_pointer::in, io.state::di, io.state::uo)
	is det. 

io_tab_goto.part_1(Stream) -->
	io_tab_goto.test(Stream, 0, A),
	io_tab_goto.write_int(A),
	io_tab_goto.poly_test(Stream, ['a', 'b', 'c'], 0, B),
	io_tab_goto.write_int(B).

:- pred io_tab_goto.part_2(c_pointer::in, io.state::di, io.state::uo)
	is det.

io_tab_goto.part_2(Stream) -->
	io_tab_goto.test(Stream, 0, A),
	io_tab_goto.write_int(A).

:- pred io_tab_goto.test(c_pointer::in, int::in, int::out,
	io.state::di, io.state::uo) is det.

io_tab_goto.test(Stream, SoFar, N) -->
	io_tab_goto.read_char_code(Stream, CharCode),
	(
		{ char.to_int(Char, CharCode) },
		{ char.is_digit(Char) },
		{ char.digit_to_int(Char, CharInt) }
	->
		io_tab_goto.test(Stream, SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pred io_tab_goto.poly_test(c_pointer::in, T::in, int::in, int::out,
	io.state::di, io.state::uo) is det.

io_tab_goto.poly_test(Stream, Unused, SoFar, N) -->
	io_tab_goto.poly_read_char_code(Stream, Unused, CharCode),
	(
		{ char.to_int(Char, CharCode) },
		{ char.is_digit(Char) },
		{ char.digit_to_int(Char, CharInt) }
	->
		io_tab_goto.poly_test(Stream, Unused,
			SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pragma c_header_code("#include <stdio.h>").

:- pred io_tab_goto.open_input(string::in, int::out, c_pointer::out,
	io.state::di, io.state::uo) is det.

:- pragma no_inline(io_tab_goto.open_input/5).

:- pragma foreign_proc("C",
	io_tab_goto.open_input(FileName::in, Res::out, Stream::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) fopen((const char *) FileName, ""r"");
	Res = Stream? 0 : -1;
	goto end1;
end1:
	IO = IO0;
").

:- pred io_tab_goto.read_char_code(c_pointer::in, int::out,
	io.state::di, io.state::uo) is det.

:- pragma no_inline(io_tab_goto.read_char_code/4).

:- pragma foreign_proc("C",
	io_tab_goto.read_char_code(Stream::in, CharCode::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	CharCode = getc((FILE *) Stream);
	goto end2;
end2:
	IO = IO0;
").

:- pred io_tab_goto.poly_read_char_code(c_pointer::in, T::in, int::out,
	io.state::di, io.state::uo) is det.

:- pragma no_inline(io_tab_goto.poly_read_char_code/5).

:- pragma foreign_proc("C",
	io_tab_goto.poly_read_char_code(Stream::in, Unused::in,
		CharCode::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* ignore Unused */
	CharCode = getc((FILE *) Stream);
	goto end3;
end3:
	IO = IO0;
").

:- pred io_tab_goto.write_int(int::in, io.state::di, io.state::uo)
	is det.

:- pragma no_inline(io_tab_goto.write_int/3).

:- pragma foreign_proc("C",
	io_tab_goto.write_int(N::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	printf(""%d\\n"", (int) N);
	goto end4;
end4:
	IO = IO0;
}").
