%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module print_io_actions.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    fake_open_input("print_io_actions.data", Res, Stream, !IO),
    ( if Res = 0 then
        fake_read_n_chars(Stream, 40, CharList1, !IO),
        fake_read_n_chars(Stream, 40, CharList2, !IO),
        Str = string.from_char_list(CharList1 ++ CharList2),
        io.write_string(Str, !IO),
        io.nl(!IO)
    else
        io.write_string("could not open print_io_actions.data\n", !IO)
    ).

:- pred fake_read_n_chars(c_pointer::in, int::in, list(char)::out,
    io::di, io::uo) is det.

fake_read_n_chars(Stream, N, Chars, !IO) :-
    ( if N =< 0 then
        Chars = []
    else
        fake_read_char_code(Stream, CharCode, !IO),
        ( if CharCode = -1 then
            Chars = []
        else
            Char = char.det_from_int(CharCode),
            fake_read_n_chars(Stream, N - 1, TailChars, !IO),
            Chars = [Char | TailChars]
        )
    ).

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pred fake_open_input(string::in, int::out, c_pointer::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    fake_open_input(FileName::in, Res::out, Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = (MR_Word) fopen((const char *) FileName, ""r"");
    Res = Stream? 0 : -1;
    IO = IO0;
").

:- pred fake_read_char_code(c_pointer::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fake_read_char_code(Stream::in, CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CharCode = getc((FILE *) Stream);
    IO = IO0;
").

:- pred fake_write_int(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fake_write_int(N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    printf(""%d\\n"", (int) N);
    IO = IO0;
}").

:- pred fake_io(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fake_io(X::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    X = 1;
    IO = IO0;
}").
