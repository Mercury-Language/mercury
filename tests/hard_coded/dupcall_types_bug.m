%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug with duplicate call elimination
% not taking types into account.

:- module dupcall_types_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
    FileName = "string",
    String = "1. ",
    string.length(String, Len),
    Posn0 = posn(1, 0, 0),
    read_from_string(FileName, String, Len, Int, Posn0, _),
    read_from_string(FileName, String, Len, Str, Posn0, _),
    ( if Int = ok(I), Str = ok(S) then
        io.write_int(I, !IO),
        io.write_string(S, !IO),
        io.nl(!IO)
    else
        io.write_string("Syntax error.\n", !IO)
    ).
