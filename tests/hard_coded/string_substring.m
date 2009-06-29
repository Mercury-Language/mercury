%-----------------------------------------------------------------------------%

:- module string_substring.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    (
        string.substring("cat", -1, max_int, "cat"),
        string.substring("cat", 0, max_int, "cat"),
        string.substring("cat", 1, max_int, "at"),
        string.substring("cat", 2, max_int, "t"),
        string.substring("cat", 3, max_int, ""),
        string.substring("cat", 4, max_int, ""),
        string.substring("cat", 0, 0, ""),
        string.substring("cat", 0, 1, "c"),
        string.substring("cat", 0, 2, "ca"),
        string.substring("cat", 0, 3, "cat"),
        string.substring("cat", 1, -1, ""),
        string.substring("cat", 1, 0, ""),
        string.substring("cat", 1, 1, "a"),
        string.substring("cat", 1, 2, "at"),
        string.substring("cat", 1, 3, "at")
    ->
        io.write_string("test succeeded\n", !IO)
    ;
        io.write_string("test failed\n", !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
