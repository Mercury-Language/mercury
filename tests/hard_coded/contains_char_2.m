%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module contains_char_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if
        string__contains_char("cat", 'c'),
        string__contains_char("cat", 'a'),
        string__contains_char("cat", 't'),
        not string__contains_char("cat", 'm'),
        string__contains_char("aßξ啕𐀀.", 'ß'),
        string__contains_char("aßξ啕𐀀.", 'ß'),
        string__contains_char("aßξ啕𐀀.", 'ξ'),
        string__contains_char("aßξ啕𐀀.", '啕'),
        string__contains_char("aßξ啕𐀀.", '.'),
        not string__contains_char("aßξ啕𐀀.", '☿')
    then
        io.write_string("test succeeded\n", !IO)
    else
        io.write_string("test failed\n", !IO)
    ).

%---------------------------------------------------------------------------%
