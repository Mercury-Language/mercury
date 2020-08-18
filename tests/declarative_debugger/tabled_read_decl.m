%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module tabled_read_decl.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.

main(!IO) :-
    tabled_read_decl.open_input("tabled_read_decl.data", Res, Stream, !IO),
    ( if Res = 0 then
        tabled_read_decl.part_1(Stream, !IO),
        tabled_read_decl.part_2(Stream, !IO),
        tabled_read_decl.part_3(!IO)
    else
        io.write_string("could not open tabled_read.data\n", !IO)
    ).

:- pred tabled_read_decl.part_1(c_pointer::in, io::di, io::uo) is det.

tabled_read_decl.part_1(Stream, !IO) :-
    tabled_read_decl.test(Stream, A, !IO),
    tabled_read_decl.write_int(A, !IO),
    tabled_read_decl.poly_test(Stream, ['a', 'b', 'c'], B, !IO),
    tabled_read_decl.write_int(B, !IO).

:- pred tabled_read_decl.part_2(c_pointer::in, io::di, io::uo) is det.

tabled_read_decl.part_2(Stream, !IO) :-
    tabled_read_decl.test(Stream, A, !IO),
    tabled_read_decl.write_int(A, !IO).

:- pred tabled_read_decl.part_3(io::di, io::uo) is det.

tabled_read_decl.part_3(!IO) :-
    tabled_read_decl.fake_io(X, !IO),
    tabled_read_decl.write_int(X, !IO).

:- pred tabled_read_decl.test(c_pointer::in, int::out, io::di, io::uo) is det.

tabled_read_decl.test(Stream, N, !IO) :-
    % BUG: the 1 should be 0
    tabled_read_decl.test_2(Stream, 1, N, !IO).

:- pred tabled_read_decl.test_2(c_pointer::in, int::in, int::out,
    io::di, io::uo) is det.

tabled_read_decl.test_2(Stream, SoFar, N, !IO) :-
    tabled_read_decl.read_char_code(Stream, CharCode, !IO),
    ( if
        char.to_int(Char, CharCode),
        char.is_decimal_digit(Char),
        char.decimal_digit_to_int(Char, CharInt)
    then
        tabled_read_decl.test_2(Stream, SoFar * 10 + CharInt, N, !IO)
    else
        N = SoFar
    ).

:- pred tabled_read_decl.poly_test(c_pointer::in, T::in, int::out,
    io::di, io::uo) is det.

tabled_read_decl.poly_test(Stream, Unused, N, !IO) :-
    % BUG: the 1 should be 0
    tabled_read_decl.poly_test_2(Stream, Unused, 1, N, !IO).

:- pred tabled_read_decl.poly_test_2(c_pointer::in, T::in, int::in, int::out,
    io::di, io::uo) is det.

tabled_read_decl.poly_test_2(Stream, Unused, SoFar, N, !IO) :-
    tabled_read_decl.poly_read_char_code(Stream, Unused, CharCode, !IO),
    ( if
        char.to_int(Char, CharCode),
        char.is_decimal_digit(Char),
        char.decimal_digit_to_int(Char, CharInt)
    then
        tabled_read_decl.poly_test_2(Stream, Unused, SoFar * 10 + CharInt, N,
            !IO)
    else
        N = SoFar
    ).

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pred tabled_read_decl.open_input(string::in, int::out, c_pointer::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    tabled_read_decl.open_input(FileName::in, Res::out, Stream::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = (MR_Word) fopen((const char *) FileName, ""r"");
    Res = Stream? 0 : -1;
    IO = IO0;
").

:- pred tabled_read_decl.read_char_code(c_pointer::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    tabled_read_decl.read_char_code(Stream::in, CharCode::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CharCode = getc((FILE *) Stream);
    IO = IO0;
").

:- pred tabled_read_decl.poly_read_char_code(c_pointer::in, T::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    tabled_read_decl.poly_read_char_code(Stream::in, Unused::in,
        CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* ignore Unused */
    CharCode = getc((FILE *) Stream);
    IO = IO0;
").

:- pred tabled_read_decl.write_int(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    tabled_read_decl.write_int(N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    printf(""%d\\n"", (int) N);
    IO = IO0;
}").

:- pred tabled_read_decl.fake_io(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    tabled_read_decl.fake_io(X::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    X = 1;
    IO = IO0;
}").
