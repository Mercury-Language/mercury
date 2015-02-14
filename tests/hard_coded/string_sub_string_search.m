%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_sub_string_search.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

main(!IO) :-
    (
        string.sub_string_search("", "", 0),
        not string.sub_string_search("", "dog", _),
        not string.sub_string_search("cat", "catdog", _),

        string.sub_string_search("cat", "", 0),
        string.sub_string_search("cat", "cat", 0),
        string.sub_string_search("cat", "at", 1),
        string.sub_string_search("cat", "t", 2),

        string.sub_string_search_start("catcatcat", "cat", 1, 3),
        not string.sub_string_search_start("catcatcat", "cat", 9, _),

        string.sub_string_search("cαtcατcat", "cατ", length("cαt"))
    ->
        io.write_string("test succeeded\n", !IO)
    ;
        io.write_string("test failed\n", !IO)
    ).
