% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module tabled_read.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module char, int.

main -->
	tabled_read__open_input("tabled_read.data", Res, Stream),
	( { Res = 0 } ->
		tabled_read__test(Stream, 0, N),
		tabled_read__write_int(N),
		tabled_read__test(Stream, 0, M),
		tabled_read__write_int(M)
	;
		io__write_string("could not open tabled_read.data\n")
	).

:- pred tabled_read__test(c_pointer::in, int::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read__test(Stream, SoFar, N) -->
	tabled_read__read_char_code(Stream, CharCode),
	(
		{ char__to_int(Char, CharCode) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		tabled_read__test(Stream, SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pragma c_header_code("#include <stdio.h>").

:- pred tabled_read__open_input(string::in, int::out, c_pointer::out,
	io__state::di, io__state::uo) is det.

:- pragma c_code(tabled_read__open_input(FileName::in, Res::out, Stream::out,
		IO0::di, IO::uo), [will_not_call_mercury, tabled_for_io],
"
	Stream = (MR_Word) fopen((const char *) FileName, ""r"");
	Res = Stream? 0 : -1;
	IO = IO0;
").

:- pred tabled_read__read_char_code(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma c_code(tabled_read__read_char_code(Stream::in, CharCode::out,
		IO0::di, IO::uo), [will_not_call_mercury, tabled_for_io],
"
	CharCode = getc((FILE *) Stream);
	IO = IO0;
").

:- pred tabled_read__write_int(int::in, io__state::di, io__state::uo) is det.

:- pragma c_code(tabled_read__write_int(N::in,
		IO0::di, IO::uo), [may_call_mercury, thread_safe],
"{
	printf(""%d\n"", (int) N);
	IO = IO0;
}").
