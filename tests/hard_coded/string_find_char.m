%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for backends using UTF-8 string encoding.
% The .exp2 file is for backends using UTF-16 string encoding.
%
%---------------------------------------------------------------------------%

:- module string_find_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Str0 = "**",
    Str1 = Str0 ++ "😀",
    Str2 = Str1 ++ "AB😀",
    Str  = Str2 ++ "|😀AB",

    string.count_code_units(Str0, Happy1),
    string.count_code_units(Str1, A1),
    string.count_code_units(Str2, Bar),
    string.count_code_units(Str, End),

    _Offsets = [
        {-1, End},          % out of range
        {0, End + 1},       % out of range
        {A1, A1},           % zero length span
        {0, End},           % whole string

        {0, A1},            % contains 1st emoji
        {0, A1 - 1},        % 1st emoji truncated, should not be found

        {A1, Bar},          % contains 2nd emoji
        {A1, Bar - 1},      % 2nd emoji truncated, should not be found

        {Happy1, Bar},      % contains 1st A, two emoji
        {Bar, End}          % contains 2nd A, one emoji
    ],

    % Show the string being searched.
    show_string(Str, !IO),
    io.nl(!IO),

    Ascii = 'A',
    Happy = '😀',
    NotPresent = '?',

    % Find first char.
    test_find_first_char(Str, Ascii, !IO),
    test_find_first_char(Str, Happy, !IO),
    test_find_first_char(Str, NotPresent, !IO),
    io.nl(!IO),

    % Find last char.
    test_find_last_char(Str, Ascii, !IO),
    test_find_last_char(Str, Happy, !IO),
    test_find_last_char(Str, NotPresent, !IO),
    io.nl(!IO),

    /*
    % Find first char between, single and multiple code units.
    list.foldl(test_find_first_char_between(Ascii, Str), Offsets, !IO),
    io.nl(!IO),
    list.foldl(test_find_first_char_between(Happy, Str), Offsets, !IO),
    io.nl(!IO),

    % Find last char between, single and multiple code units.
    list.foldl(test_find_last_char_between(Ascii, Str), Offsets, !IO),
    io.nl(!IO),
    list.foldl(test_find_last_char_between(Happy, Str), Offsets, !IO),
    io.nl(!IO),
    */
    true.

%---------------------------------------------------------------------------%

:- pred show_string(string::in, io::di, io::uo) is det.

show_string(Str, !IO) :-
    string.to_char_list(Str, CharList),
    list.foldl(write_char_padded, CharList, !IO),
    io.nl(!IO),
    list.foldl2(write_char_offset, CharList, 0, _Offset, !IO),
    io.nl(!IO).

:- pred write_char_padded(char::in, io::di, io::uo) is det.

write_char_padded(Char, !IO) :-
    ( if char.to_int(Char) >= 0x1F600 then
        io.format("%-3c", [c(Char)], !IO)
    else
        io.format("%-4c", [c(Char)], !IO)
    ).

:- pred write_char_offset(char::in, int::in, int::out, io::di, io::uo) is det.

write_char_offset(Char, Offset0, Offset, !IO) :-
    io.format("%-4d", [i(Offset0)], !IO),
    Str = string.from_char(Char),
    Offset = Offset0 + string.count_code_units(Str).

%---------------------------------------------------------------------------%

:- pred test_find_first_char(string::in, char::in, io::di, io::uo) is det.

test_find_first_char(Str, Char, !IO) :-
    ( if find_first_char(Str, Char, Index) then
        io.format("find_first_char ==> index=%d, char='%c'\n",
            [i(Index), c(Char)], !IO)
    else
        io.write_string("find_first_char failed\n", !IO)
    ).

:- pred test_find_last_char(string::in, char::in, io::di, io::uo) is det.

test_find_last_char(Str, Char, !IO) :-
    ( if find_last_char(Str, Char, Index) then
        io.format("find_last_char ==> index=%d, char='%c'\n",
            [i(Index), c(Char)], !IO)
    else
        io.write_string("find_last_char failed\n", !IO)
    ).
