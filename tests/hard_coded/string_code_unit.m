%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_code_unit.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Strings = [
        % "\u0000",             % NUL
        "\u0001\u007f",         % ASCII
        "\u0080\u07ff",         % UTF-8 2-byte
        "\u0800\uffff",         % UTF-8 3-byte
        "\U00100000\U0010ffff", % UTF-8 4-byte
        "\u0001\ud7ff",         % UTF-16 1 code unit
        "\ue000\uffff",         % UTF-16 1 code unit
        "\U00010000\U0010ffff"  % UTF-16 2 code units
        % "\ud800\udbff",       % leading surrogates
        % "\udc00\udfff",       % trailing surrogates
        % "\U00110000"          % out of range
    ],
    list.foldl(test, Strings, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(String, !IO) :-
    io.write_string("code points:\t", !IO),
    string.to_char_list(String, Chars),
    write_hex_chars(Chars, !IO),
    io.nl(!IO),

    string.to_utf8_code_unit_list(String, UTF8),
    io.write_string("UTF-8:\t\t", !IO),
    write_hex_ints(UTF8, !IO),
    io.nl(!IO),
    ( if string.from_utf8_code_unit_list(UTF8, String) then
        true
    else
        io.write_string("from_utf8_code_unit_list failed\n", !IO)
    ),

    string.to_utf16_code_unit_list(String, UTF16),
    io.write_string("UTF-16:\t\t", !IO),
    write_hex_ints(UTF16, !IO),
    io.nl(!IO),
    ( if string.from_utf16_code_unit_list(UTF16, String) then
        true
    else
        io.write_string("from_utf16_code_unit_list failed\n", !IO)
    ),
    io.nl(!IO).

:- pred write_hex_chars(list(char)::in, io::di, io::uo) is det.

write_hex_chars(Chars, !IO) :-
    write_hex_ints(map(to_int, Chars), !IO).

:- pred write_hex_ints(list(int)::in, io::di, io::uo) is det.

write_hex_ints(Ints, !IO) :-
    io.write_list(Ints, ", ", write_hex_int, !IO).

:- pred write_hex_int(int::in, io::di, io::uo) is det.

write_hex_int(Int, !IO) :-
    io.format("0x%02x", [i(Int)], !IO).

%---------------------------------------------------------------------------%
