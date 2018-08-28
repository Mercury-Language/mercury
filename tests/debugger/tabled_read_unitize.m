%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% We define our own I/O primitives, in case the library was compiled without
% IO tabling.

:- module tabled_read_unitize.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module char.
:- import_module int.

main(!IO) :-
    tabled_read_unitize.open_input("tabled_read_unitize.data", Res, Stream,
        !IO),
    ( if Res = 0 then
        tabled_read_unitize.read_num(Stream, A, !IO),
        tabled_read_unitize.unitize(Stream, B, !IO),
        tabled_read_unitize.read_num(Stream, C, !IO),
        tabled_read_unitize.write_int(A, !IO),
        tabled_read_unitize.write_int(B, !IO),
        tabled_read_unitize.write_int(C, !IO)
    else
        io.write_string("could not open tabled_read_unitize.data\n", !IO)
    ).

:- pragma foreign_export("C", tabled_read_unitize.read_num(in, out, di, uo),
    "MT_read_num").

:- pred read_num(c_pointer::in, int::out, io::di, io::uo) is det.

read_num(Stream, Num, !IO) :-
    tabled_read_unitize.read_num_2(Stream, 0, Num, !IO).

:- pred read_num_2(c_pointer::in, int::in, int::out, io::di, io::uo) is det.

read_num_2(Stream, SoFar, N, !IO) :-
    tabled_read_unitize.read_char_code(Stream, CharCode, !IO),
    ( if
        char.to_int(Char, CharCode),
        char.is_digit(Char),
        char.decimal_digit_to_int(Char, CharInt)
    then
        tabled_read_unitize.read_num_2(Stream, SoFar * 10 + CharInt, N, !IO)
    else
        N = SoFar
    ).

:- pred tabled_read_unitize.unitize(c_pointer::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    tabled_read_unitize.unitize(Stream::in, N::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io_unitize,
    % This needs to be declared as thread safe, otherwise it deadlocks
    % in `.par' grades, since it acquires the global lock and then
    % calls back Mercury code which tries to reacquire that lock.
    thread_safe],
"
    MR_Integer  int1;
    MR_Integer  int2;

    MT_read_num(Stream, &int1);
    MT_read_num(Stream, &int2);
    N = int1 * 100 + int2;
").

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pred open_input(string::in, int::out, c_pointer::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_input(FileName::in, Res::out, Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = (MR_Word) fopen((const char *) FileName, ""r"");
    Res = Stream? 0 : -1;
    IO = IO0;
").

:- pred tabled_read_unitize.read_char_code(c_pointer::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_char_code(Stream::in, CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CharCode = getc((FILE *) Stream);
    IO = IO0;
").

:- pred poly_read_char_code(c_pointer::in, T::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    poly_read_char_code(Stream::in, Unused::in, CharCode::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* ignore Unused */
    CharCode = getc((FILE *) Stream);
    IO = IO0;
").

:- pred write_int(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_int(N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"{
    printf(""%d\\n"", (int) N);
    IO = IO0;
}").
