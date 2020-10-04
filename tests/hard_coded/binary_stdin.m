%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module binary_stdin.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

main(!IO) :-
    int.fold_up(read_byte_test, 0, 255, !IO),
    io.stdin_binary_stream(Stream, !IO),
    int.fold_up(read_byte_test(Stream), 0, 255, !IO),
    expect_eof(Stream, !IO),
    io.write_string("done.\n", !IO).

:- pred read_byte_test(int::in, io::di, io::uo) is det.

read_byte_test(ExpectedByte, !IO) :-
    io.read_byte(Res, !IO),
    (
        Res = ok(ReadByte),
        ( ExpectedByte = ReadByte ->
            true
        ;
            Stderr = io.stderr_stream,
            io.write_string(Stderr, "expected ", !IO),
            io.write_int(Stderr, ExpectedByte, !IO),
            io.write_string(Stderr, ", got ", !IO),
            io.write_int(Stderr, ReadByte, !IO),
            io.nl(Stderr, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        Res = eof,
        io.write_string(io.stderr_stream, "unexpected eof\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Res = error(Error),
        io.write_string(io.stderr_stream, io.error_message(Error), !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred read_byte_test(io.binary_input_stream::in, int::in, io::di, io::uo)
    is det.

read_byte_test(Stream, ExpectedByte, !IO) :-
    io.read_byte(Stream, Res, !IO),
    (
        Res = ok(ReadByte),
        ( if ExpectedByte = ReadByte then
            true
        else
            Stderr = io.stderr_stream,
            io.write_string(Stderr, "expected ", !IO),
            io.write_int(Stderr, ExpectedByte, !IO),
            io.write_string(Stderr, ", got ", !IO),
            io.write_int(Stderr, ReadByte, !IO),
            io.nl(Stderr, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        Res = eof,
        io.write_string(io.stderr_stream, "unexpected eof\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Res = error(Error),
        io.write_string(io.stderr_stream, io.error_message(Error), !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred expect_eof(io.binary_input_stream::in, io::di, io::uo) is det.

expect_eof(Stream, !IO) :-
    io.read_byte(Stream, Res, !IO),
    (
        Res = ok(_),
        io.write_string("expected eof\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        Res = eof
    ;
        Res = error(Error),
        io.write_string(io.stderr_stream, io.error_message(Error), !IO),
        io.set_exit_status(1, !IO)
    ).
