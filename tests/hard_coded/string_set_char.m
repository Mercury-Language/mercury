%-----------------------------------------------------------------------------%

:- module string_set_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ( string.set_char('x', -1, "", _) ->
        error("test failed")
    ;
        true
    ),
    ( string.set_char('x', 0, "", _) ->
        error("test failed")
    ;
        true
    ),
    (  string.set_char('x', 1, "", _) ->
        error("test failed")
    ;
        true
    ),

    ( string.set_char('m', 0, "cat", "mat") ->
        true
    ;
        error("test failed")
    ),
    ( string.set_char('u', 1, "cat", "cut") ->
        true
    ;
        error("test failed")
    ),
    ( string.set_char('b', 2, "cat", "cab") ->
        true
    ;
        error("test failed")
    ),
    ( string.set_char('x', 3, "cat", _) ->
        error("test failed")
    ;
        true
    ),

    io.write_string("test succeeded\n", !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
