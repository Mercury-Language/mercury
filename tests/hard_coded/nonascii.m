%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for bugs relating to handling of non-ASCII
% characters.

:- module nonascii.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.open_input("nonascii.data", Result, !IO),
    (
        Result = ok(Stream),
        test1(Stream, !IO),
        test2(Stream, 259, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

:- pred test1(io.input_stream::in, io::di, io::uo) is det.

test1(Stream, !IO) :-
    io.read_line_as_string(Stream, Result, !IO),
    (
        Result = ok(Line),
        Chars = string.to_char_list(Line),
        Ints = list.map(char.to_int, Chars),
        io.write_list(Ints, ",\n", io.write_int, !IO),
        io.nl(!IO)
    ;
        Result = eof,
        io.write_string("premature EOF\n", !IO)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

:- pred test2(io.input_stream::in, int::in, io::di, io::uo) is det.

test2(Stream, N, !IO) :-
    ( if N > 0 then
        io.read_char(Stream, Result, !IO),
        (
            Result = ok(Char),
            Int = char.to_int(Char),
            io.write_int(Int, !IO),
            io.nl(!IO)
        ;
            Result = eof,
            io.write_string("premature EOF\n", !IO)
        ;
            Result = error(Error),
            io.error_message(Error, Msg),
            io.write_string(Msg, !IO),
            io.nl(!IO)
        ),
        test2(Stream, N - 1, !IO)
    else
        true
    ).
