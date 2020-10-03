%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% All this program does is echo the input by recursively calling main
% to read and then write one char.
%
% It is useful both for ensuring that this recursion works ok,
% and as an IO test.
%

:- module recursive_main.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    read_char(Result, !IO),
    (
        Result = ok(Char),
        io.write_char(Char, !IO),
        main(!IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        io.write_string(error_message(Error), !IO)
    ).
