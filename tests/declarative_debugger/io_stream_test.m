%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the declarative debugger's handling of I/O streams.
%

:- module io_stream_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.

main(!IO) :-
    io.open_input("tabled_read_decl.data", Res, !IO),
    (
        Res = ok(Stream),
        io_stream_test.part_1(Stream, !IO),
        io_stream_test.part_2(Stream, !IO)
    ;
        Res = error(_),
        io.write_string("could not open tabled_read.data\n", !IO)
    ).

:- pred part_1(io.input_stream::in, io::di, io::uo) is det.

part_1(Stream, !IO) :-
    test(Stream, A, !IO),
    io.write_int(A, !IO),
    io.nl(!IO).

:- pred part_2(io.input_stream::in, io::di, io::uo) is det.

part_2(Stream, !IO) :-
    test(Stream, A, !IO),
    io.write_int(A, !IO),
    io.nl(!IO).

:- pred test(io.input_stream::in, int::out, io::di, io::uo) is det.

test(Stream, N, !IO) :-
    % BUG: the 1 should be 0
    test_2(Stream, 1, N, !IO).

:- pred test_2(io.input_stream::in, int::in, int::out, io::di, io::uo) is det.

test_2(Stream, SoFar, N, !IO) :-
    io.read_char(Stream, Res, !IO),
    ( if
        Res = ok(Char),
        char.is_decimal_digit(Char),
        char.decimal_digit_to_int(Char, CharInt)
    then
        test_2(Stream, SoFar * 10 + CharInt, N, !IO)
    else
        N = SoFar
    ).
