%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_split_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

main(!IO) :-
    ( if
        string.split("cat", -1, "", "cat"),
        string.split("cat", 0, "", "cat"),
        string.split("cat", 1, "c", "at"),
        string.split("cat", 2, "ca", "t"),
        string.split("cat", 3, "cat", ""),
        string.split("cat", 4, "cat", "")
    then
        io.write_string("test succeeded\n", !IO)
    else
        io.write_string("test failed\n", !IO)
    ).
