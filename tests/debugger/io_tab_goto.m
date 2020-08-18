%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module io_tab_goto.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module char.
:- import_module int.

main(!IO) :-
    goto(!IO),
    io_tab_goto.open_input("io_tab_goto.data", Res, Stream, !IO),
    ( if Res = 0 then
        io_tab_goto.part_1(Stream, !IO),
        io_tab_goto.part_2(Stream, !IO)
    else
        io.write_string("could not open io_tab_goto.data\n", !IO)
    ).

:- pred goto(io::di, io::uo) is det.

:- pragma no_inline(goto/2).

:- pragma foreign_proc(c,
    goto(IO0::di, IO::uo),
    [tabled_for_io, promise_pure],
"
    printf(""should see this printf\\n"");
    goto label;
    printf(""should never see this printf\\n"");
label:
    IO = IO0;
").

:- pred io_tab_goto.part_1(c_pointer::in, io::di, io::uo) is det.

io_tab_goto.part_1(Stream, !IO) :-
    io_tab_goto.test(Stream, 0, A, !IO),
    io_tab_goto.write_int(A, !IO),
    io_tab_goto.poly_test(Stream, ['a', 'b', 'c'], 0, B, !IO),
    io_tab_goto.write_int(B, !IO).

:- pred io_tab_goto.part_2(c_pointer::in, io::di, io::uo) is det.

io_tab_goto.part_2(Stream, !IO) :-
    io_tab_goto.test(Stream, 0, A, !IO),
    io_tab_goto.write_int(A, !IO).

:- pred test(c_pointer::in, int::in, int::out, io::di, io::uo) is det.

test(Stream, SoFar, N, !IO) :-
    io_tab_goto.read_char_code(Stream, CharCode, !IO),
    ( if
        char.to_int(Char, CharCode),
        char.is_decimal_digit(Char),
        char.decimal_digit_to_int(Char, CharInt)
    then
        io_tab_goto.test(Stream, SoFar * 10 + CharInt, N, !IO)
    else
        N = SoFar
    ).

:- pred poly_test(c_pointer::in, T::in, int::in, int::out,
    io::di, io::uo) is det.

poly_test(Stream, Unused, SoFar, N, !IO) :-
    io_tab_goto.poly_read_char_code(Stream, Unused, CharCode, !IO),
    ( if
        char.to_int(Char, CharCode),
        char.is_decimal_digit(Char),
        char.decimal_digit_to_int(Char, CharInt)
    then
        io_tab_goto.poly_test(Stream, Unused, SoFar * 10 + CharInt, N, !IO)
    else
        N = SoFar
    ).

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pred io_tab_goto.open_input(string::in, int::out, c_pointer::out,
    io::di, io::uo) is det.
:- pragma no_inline(io_tab_goto.open_input/5).

:- pragma foreign_proc("C",
    open_input(FileName::in, Res::out, Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = (MR_Word) fopen((const char *) FileName, ""r"");
    Res = Stream? 0 : -1;
    goto end1;
end1:
    IO = IO0;
").

:- pred read_char_code(c_pointer::in, int::out,
    io::di, io::uo) is det.
:- pragma no_inline(io_tab_goto.read_char_code/4).

:- pragma foreign_proc("C",
    read_char_code(Stream::in, CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CharCode = getc((FILE *) Stream);
    goto end2;
end2:
    IO = IO0;
").

:- pred poly_read_char_code(c_pointer::in, T::in, int::out,
    io::di, io::uo) is det.
:- pragma no_inline(io_tab_goto.poly_read_char_code/5).

:- pragma foreign_proc("C",
    poly_read_char_code(Stream::in, Unused::in, CharCode::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* ignore Unused */
    CharCode = getc((FILE *) Stream);
    goto end3;
end3:
    IO = IO0;
").

:- pred write_int(int::in, io::di, io::uo) is det.
:- pragma no_inline(io_tab_goto.write_int/3).

:- pragma foreign_proc("C",
    write_int(N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"{
    printf(""%d\\n"", (int) N);
    goto end4;
end4:
    IO = IO0;
}").
