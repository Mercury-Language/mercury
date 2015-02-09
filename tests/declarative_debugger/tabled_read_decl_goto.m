% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module tabled_read_decl_goto.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, char, int.

main -->
	tabled_read_decl_goto__open_input("tabled_read_decl_goto.data", Res,
		Stream),
	( { Res = 0 } ->
		tabled_read_decl_goto__part_1(Stream),
		tabled_read_decl_goto__part_2(Stream),
		tabled_read_decl_goto__part_3
	;
		io__write_string("could not open tabled_read.data\n")
	).

:- pred tabled_read_decl_goto__part_1(c_pointer::in, 
	io__state::di, io__state::uo) is det. 

tabled_read_decl_goto__part_1(Stream) -->
	tabled_read_decl_goto__test(Stream, A),
	tabled_read_decl_goto__write_int(A),
	tabled_read_decl_goto__poly_test(Stream, ['a', 'b', 'c'], B),
	tabled_read_decl_goto__write_int(B).

:- pred tabled_read_decl_goto__part_2(c_pointer::in, 
	io__state::di, io__state::uo) is det.

tabled_read_decl_goto__part_2(Stream) -->
	tabled_read_decl_goto__test(Stream, A),
	tabled_read_decl_goto__write_int(A).

:- pred tabled_read_decl_goto__part_3(io__state::di, io__state::uo) is det.

tabled_read_decl_goto__part_3(!IO) :-
	tabled_read_decl_goto__fake_io(X, !IO),
	tabled_read_decl_goto__write_int(X, !IO).

:- pred tabled_read_decl_goto__test(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_decl_goto__test(Stream, N) -->
		% BUG: the 1 should be 0
	tabled_read_decl_goto__test_2(Stream, 1, N).

:- pred tabled_read_decl_goto__test_2(c_pointer::in, int::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_decl_goto__test_2(Stream, SoFar, N) -->
	tabled_read_decl_goto__read_char_code(Stream, CharCode),
	(
		{ char__to_int(Char, CharCode) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		tabled_read_decl_goto__test_2(Stream, SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pred tabled_read_decl_goto__poly_test(c_pointer::in, T::in, int::out,
	io__state::di, io__state::uo) is det.

tabled_read_decl_goto__poly_test(Stream, Unused, N) -->
		% BUG: the 1 should be 0
	tabled_read_decl_goto__poly_test_2(Stream, Unused, 1, N).

:- pred tabled_read_decl_goto__poly_test_2(c_pointer::in, T::in, int::in,
	int::out, io__state::di, io__state::uo) is det.

tabled_read_decl_goto__poly_test_2(Stream, Unused, SoFar, N) -->
	tabled_read_decl_goto__poly_read_char_code(Stream, Unused, CharCode),
	(
		{ char__to_int(Char, CharCode) },
		{ char__is_digit(Char) },
		{ char__digit_to_int(Char, CharInt) }
	->
		tabled_read_decl_goto__poly_test_2(Stream, Unused,
			SoFar * 10 + CharInt, N)
	;
		{ N = SoFar }
	).

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pred tabled_read_decl_goto__open_input(string::in, int::out, c_pointer::out,
	io__state::di, io__state::uo) is det.

:- pragma no_inline(tabled_read_decl_goto__open_input/5).

:- pragma foreign_proc("C",
	tabled_read_decl_goto__open_input(FileName::in, Res::out, Stream::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) fopen((const char *) FileName, ""r"");
	Res = Stream? 0 : -1;
	goto end1;
end1:
	IO = IO0;
").

:- pred tabled_read_decl_goto__read_char_code(c_pointer::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma no_inline(tabled_read_decl_goto__read_char_code/4).

:- pragma foreign_proc("C",
	tabled_read_decl_goto__read_char_code(Stream::in, CharCode::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	CharCode = getc((FILE *) Stream);
	goto end2;
end2:
	IO = IO0;
").

:- pred tabled_read_decl_goto__poly_read_char_code(c_pointer::in, T::in,
	int::out, io__state::di, io__state::uo) is det.

:- pragma no_inline(tabled_read_decl_goto__poly_read_char_code/5).

:- pragma foreign_proc("C",
	tabled_read_decl_goto__poly_read_char_code(Stream::in, Unused::in,
		CharCode::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* ignore Unused */
	CharCode = getc((FILE *) Stream);
	goto end3;
end3:
	IO = IO0;
").

:- pred tabled_read_decl_goto__write_int(int::in, io__state::di, io__state::uo)
	is det.

:- pragma no_inline(tabled_read_decl_goto__write_int/3).

:- pragma foreign_proc("C",
	tabled_read_decl_goto__write_int(N::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	printf(""%d\\n"", (int) N);
	goto end4;
end4:
	IO = IO0;
}").

:- pred tabled_read_decl_goto__fake_io(int::out, io::di, io::uo) is det.

:- pragma no_inline(tabled_read_decl_goto__fake_io/3).

:- pragma foreign_proc("C", 
	tabled_read_decl_goto__fake_io(X::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	X = 1;
	goto end5;
end5:
	IO = IO0;
}").
