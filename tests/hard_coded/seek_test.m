% Simple test of io.seek_binary_input.

:- module seek_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.open_binary_input("seek_test.data", OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read_byte(Stream, ResultA, !IO),
        io.seek_binary_input(Stream, cur, 4, !IO),
        io.read_byte(Stream, ResultB, !IO),
        io.seek_binary_input(Stream, end, -5, !IO),
        io.read_byte(Stream, ResultC, !IO),
        io.seek_binary_input(Stream, set, 10, !IO),
        io.read_byte(Stream, ResultD, !IO),
        io.binary_input_stream_offset(Stream, Offset, !IO),
        io.close_binary_input(Stream, !IO),
        io.print({ResultA, ResultB, ResultC, ResultD, Offset}, !IO),
        io.nl(!IO)
    ;
        OpenResult = error(Error),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ).

% vim: ft=mercury ts=8 sw=4 et wm=0 tw=0
