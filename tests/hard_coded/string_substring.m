%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_substring.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
    ( if
        string.between("cat", -1, max_int, "cat"),
        string.between("cat", 0, max_int, "cat"),
        string.between("cat", 1, max_int, "at"),
        string.between("cat", 2, max_int, "t"),
        string.between("cat", 3, max_int, ""),
        string.between("cat", 4, max_int, ""),
        string.between("cat", 0, 0, ""),
        string.between("cat", 0, 1, "c"),
        string.between("cat", 0, 2, "ca"),
        string.between("cat", 0, 3, "cat"),
        string.between("cat", 1, -1, ""),
        string.between("cat", 1, 0, ""),
        string.between("cat", 1, 1, ""),
        string.between("cat", 1, 2, "a"),
        string.between("cat", 1, 3, "at"),
        string.between("cat", 1, 4, "at")
    then
        io.write_string("test succeeded\n", !IO)
    else
        io.write_string("test failed\n", !IO)
    ).
