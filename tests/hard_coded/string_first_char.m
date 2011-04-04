%-----------------------------------------------------------------------------%
% Test all modes of string.first_char.

:- module string_first_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("first_char(in, in, in):\n", !IO),
    test_first_char_iii("aßξ啕𐀀", 'a', "ßξ啕𐀀", !IO),
    test_first_char_iii("ßξ啕𐀀", 'ß', "ξ啕𐀀", !IO),
    test_first_char_iii("ξ啕𐀀", 'ξ', "啕𐀀", !IO),
    test_first_char_iii("啕𐀀", '啕', "𐀀", !IO),
    test_first_char_iii("𐀀", '𐀀', "", !IO),
    test_first_char_iii("", '.', "", !IO),
    test_first_char_iii("abc", 'x', "abc", !IO),

    io.write_string("\nfirst_char(in, uo, in):\n", !IO),
    test_first_char_ioi("aßξ啕𐀀", "ßξ啕𐀀", !IO),
    test_first_char_ioi("ßξ啕𐀀", "ξ啕𐀀", !IO),
    test_first_char_ioi("ξ啕𐀀", "啕𐀀", !IO),
    test_first_char_ioi("啕𐀀", "𐀀", !IO),
    test_first_char_ioi("𐀀", "", !IO),
    test_first_char_ioi("", "", !IO),

    io.write_string("\nfirst_char(in, in, uo):\n", !IO),
    test_first_char_iio("aßξ啕𐀀", 'a', !IO),
    test_first_char_iio("ßξ啕𐀀", 'ß', !IO),
    test_first_char_iio("ξ啕𐀀", 'ξ', !IO),
    test_first_char_iio("啕𐀀", '啕', !IO),
    test_first_char_iio("𐀀", '𐀀', !IO),
    test_first_char_iio("", '.', !IO),

    io.write_string("\nfirst_char(in, uo, uo):\n", !IO),
    test_first_char_ioo("aßξ啕𐀀", !IO),
    test_first_char_ioo("ßξ啕𐀀", !IO),
    test_first_char_ioo("ξ啕𐀀", !IO),
    test_first_char_ioo("啕𐀀", !IO),
    test_first_char_ioo("𐀀", !IO),
    test_first_char_ioo("", !IO),

    io.write_string("\nfirst_char(uo, in, in):\n", !IO),
    test_first_char_oii('a', "ßξ啕𐀀", !IO),
    test_first_char_oii('ß', "ξ啕𐀀", !IO),
    test_first_char_oii('ξ', "啕𐀀", !IO),
    test_first_char_oii('啕', "𐀀", !IO),
    test_first_char_oii('𐀀', "", !IO).

:- pred test_first_char_iii(string::in, char::in, string::in, io::di, io::uo)
    is det.

test_first_char_iii(Str, FirstChar, Rest, !IO) :-
    ( string.first_char(Str, FirstChar, Rest) ->
        io.format("first_char(""%s"", '%c', ""%s"")\n",
            [s(Str), c(FirstChar), s(Rest)], !IO)
    ;
        io.format("not first_char(""%s"", '%c', ""%s"")\n",
            [s(Str), c(FirstChar), s(Rest)], !IO)
    ).

:- pred test_first_char_ioi(string::in, string::in, io::di, io::uo) is det.

test_first_char_ioi(Str, Rest, !IO) :-
    ( string.first_char(Str, FirstChar, Rest) ->
        io.format("first_char(""%s"", '%c', ""%s"")\n",
            [s(Str), c(FirstChar), s(Rest)], !IO)
    ;
        io.format("not first_char(""%s"", _, ""%s"")\n",
            [s(Str), s(Rest)], !IO)
    ).

:- pred test_first_char_iio(string::in, char::in, io::di, io::uo) is det.

test_first_char_iio(Str, FirstChar, !IO) :-
    ( string.first_char(Str, FirstChar, Rest) ->
        io.format("first_char(""%s"", '%c', ""%s"")\n",
            [s(Str), c(FirstChar), s(Rest)], !IO)
    ;
        io.format("not first_char(""%s"", '%c', _)\n",
            [s(Str), c(FirstChar)], !IO)
    ).

:- pred test_first_char_ioo(string::in, io::di, io::uo) is det.

test_first_char_ioo(Str, !IO) :-
    ( string.first_char(Str, FirstChar, Rest) ->
        io.format("first_char(""%s"", '%c', ""%s"")\n",
            [s(Str), c(FirstChar), s(Rest)], !IO)
    ;
        io.format("not first_char(""%s"", _, _)\n",
            [s(Str)], !IO)
    ).

:- pred test_first_char_oii(char::in, string::in, io::di, io::uo) is det.

test_first_char_oii(FirstChar, Rest, !IO) :-
    string.first_char(Str, FirstChar, Rest),
    io.format("first_char(""%s"", '%c', ""%s"")\n",
        [s(Str), c(FirstChar), s(Rest)], !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
