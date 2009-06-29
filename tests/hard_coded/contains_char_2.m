%-----------------------------------------------------------------------------%

:- module contains_char_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    (
        string__contains_char("cat", 'c'),
        string__contains_char("cat", 'a'),
        string__contains_char("cat", 't'),
        not string__contains_char("cat", 'm')
    ->
        io.write_string("test succeeded\n", !IO)
    ;
        io.write_string("test failed\n", !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
