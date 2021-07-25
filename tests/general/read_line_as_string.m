%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module read_line_as_string.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.open_input("read_line_as_string.exp", Result, !IO),
    ( Result = ok(Stream) ->
        io.set_input_stream(Stream, _, !IO),
        io.read_line_as_string(Result2, !IO),
        cat(Result2, !IO)
    ;
        io.write_string("Error opening file!", !IO)
    ).

:- pred cat(io.result(string)::in, io::di, io::uo) is det.

cat(Result, !IO) :-
    (
        Result = ok(Line),
        io.write_string(Line, !IO),
        io.read_line_as_string(NextResult, !IO),
        cat(NextResult, !IO)
    ;
        Result = eof
    ;
        Result = error(_Error),
        io.write_string("Error reading file!", !IO)
    ).

