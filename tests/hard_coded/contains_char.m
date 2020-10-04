%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module contains_char.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module string.

main(!IO) :-
    ( if
        char__to_int(Nul, 0),
        string__contains_char("", Nul)
    then
        io.write_string("test failed\n", !IO)
    else
        io.write_string("test succeeded\n", !IO)
    ).

