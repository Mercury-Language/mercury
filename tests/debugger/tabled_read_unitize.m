% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module tabled_read_unitize.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, char, int.

main -->
	tabled_read_unitize__open_input("tabled_read_unitize.data", Res,
		Stream),
	( { Res = 0 } ->
		tabled_read_unitize__read_num(Stream, A),
		tabled_read_unitize__unitize(Stream, B),
		tabled_read_unitize__read_num(Stream, C),
		tabled_read_unitize__write_int(A),
		tabled_read_unitize__write_int(B),
		tabled_read_unitize__write_int(C)
	;
		io__write_string("could not open tabled_read_unitize.data\n")
	).

:- pragma export(tabled_read_unitize__read_num(in, out, di, uo),
	"MT_read_num").

:- pred tabled_read_unitize__read_num(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_unitize__read_num(Stream, Num) -->
	tabled_read_unitize__read_num_2(Stream, 0, Num).

:- pred tabled_read_unitize__read_num_2(c_pointer::in, int::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_unitize__read_num_2(Stream, SoFar, N) -->
	tabled_read_unitize__read_char_code(Stream, CharCode),
	(
		{ char__to_int(Char, CharCode) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		tabled_read_unitize__read_num_2(Stream, SoFar * 10 + CharInt,
			N)
	;
		{ N = SoFar }
	).

:- pred tabled_read_unitize__unitize(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_unitize__unitize(Stream::in, N::out, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io_unitize],
"
	MR_Integer	int1;
	MR_Integer	int2;

	MT_read_num(Stream, &int1);
	MT_read_num(Stream, &int2);
	N = int1 * 100 + int2;
").

:- pragma c_header_code("#include <stdio.h>").

:- pred tabled_read_unitize__open_input(string::in, int::out, c_pointer::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_unitize__open_input(FileName::in, Res::out, Stream::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) fopen((const char *) FileName, ""r"");
	Res = Stream? 0 : -1;
	IO = IO0;
").

:- pred tabled_read_unitize__read_char_code(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_unitize__read_char_code(Stream::in, CharCode::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	CharCode = getc((FILE *) Stream);
	IO = IO0;
").

:- pred tabled_read_unitize__poly_read_char_code(c_pointer::in, T::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_unitize__poly_read_char_code(Stream::in, Unused::in,
		CharCode::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* ignore Unused */
	CharCode = getc((FILE *) Stream);
	IO = IO0;
").

:- pred tabled_read_unitize__write_int(int::in, io__state::di, io__state::uo)
	is det.

:- pragma foreign_proc("C",
	tabled_read_unitize__write_int(N::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	printf(""%d\\n"", (int) N);
	IO = IO0;
}").
