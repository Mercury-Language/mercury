% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module tabled_read_decl.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, char, int.

main -->
	tabled_read_decl__open_input("tabled_read_decl.data", Res, Stream),
	( { Res = 0 } ->
		tabled_read_decl__part_1(Stream),
		tabled_read_decl__part_2(Stream)
	;
		io__write_string("could not open tabled_read.data\n")
	).

:- pred tabled_read_decl__part_1(c_pointer::in, io__state::di, io__state::uo)
	is det. 

tabled_read_decl__part_1(Stream) -->
	tabled_read_decl__test(Stream, 0, A),
	tabled_read_decl__write_int(A),
	tabled_read_decl__poly_test(Stream, ['a', 'b', 'c'], 0, B),
	tabled_read_decl__write_int(B).

:- pred tabled_read_decl__part_2(c_pointer::in, io__state::di, io__state::uo)
	is det.

tabled_read_decl__part_2(Stream) -->
	tabled_read_decl__test(Stream, 0, A),
	tabled_read_decl__write_int(A).

:- pred tabled_read_decl__test(c_pointer::in, int::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_decl__test(Stream, SoFar, N) -->
	tabled_read_decl__read_char_code(Stream, CharCode),
	(
		{ char__to_int(Char, CharCode) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		tabled_read_decl__test(Stream, SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pred tabled_read_decl__poly_test(c_pointer::in, T::in, int::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_decl__poly_test(Stream, Unused, SoFar, N) -->
	tabled_read_decl__poly_read_char_code(Stream, Unused, CharCode),
	(
		{ char__to_int(Char, CharCode) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		tabled_read_decl__poly_test(Stream, Unused,
			SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pragma c_header_code("#include <stdio.h>").

:- pred tabled_read_decl__open_input(string::in, int::out, c_pointer::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_decl__open_input(FileName::in, Res::out, Stream::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) fopen((const char *) FileName, ""r"");
	Res = Stream? 0 : -1;
	IO = IO0;
").

:- pred tabled_read_decl__read_char_code(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_decl__read_char_code(Stream::in, CharCode::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	CharCode = getc((FILE *) Stream);
	IO = IO0;
").

:- pred tabled_read_decl__poly_read_char_code(c_pointer::in, T::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	tabled_read_decl__poly_read_char_code(Stream::in, Unused::in,
		CharCode::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* ignore Unused */
	CharCode = getc((FILE *) Stream);
	IO = IO0;
").

:- pred tabled_read_decl__write_int(int::in, io__state::di, io__state::uo)
	is det.

:- pragma foreign_proc("C",
	tabled_read_decl__write_int(N::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	printf(""%d\\n"", (int) N);
	IO = IO0;
}").
