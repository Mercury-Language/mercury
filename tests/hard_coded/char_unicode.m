%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module char_unicode.
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
    write_string("to_utf8:\n", !IO),
    test_to_utf8('\u0001', !IO),
    test_to_utf8('\u007f', !IO),
    test_to_utf8('\u07ff', !IO),
    test_to_utf8('\u263f', !IO),
    test_to_utf8('\uffff', !IO),
    test_to_utf8('\U00010000', !IO),
    test_to_utf8('\U0010ffff', !IO),
    test_to_utf8(chr(0xd800), !IO),
    test_to_utf8(chr(0xdbff), !IO),
    test_to_utf8(chr(0xdc00), !IO),
    test_to_utf8(chr(0xdfff), !IO),
    nl(!IO),

    write_string("to_utf16:\n", !IO),
    test_to_utf16('\u0001', !IO),
    test_to_utf16('\u007f', !IO),
    test_to_utf16('\u07ff', !IO),
    test_to_utf16('\u263f', !IO),
    test_to_utf16('\uffff', !IO),
    test_to_utf16('\U00010000', !IO),
    test_to_utf16('\U0010ffff', !IO),
    test_to_utf16(chr(0xd800), !IO),
    test_to_utf16(chr(0xdbff), !IO),
    test_to_utf16(chr(0xdc00), !IO),
    test_to_utf16(chr(0xdfff), !IO),
    nl(!IO),

    ( if
        not is_surrogate('\ud7ff'),
        is_surrogate(chr(0xd800)),
        is_surrogate(chr(0xdc00)),
        is_surrogate(chr(0xdfff)),
        not is_surrogate('\ue000')
    then
        io.write_string("is_surrogate: okay\n\n", !IO)
    else
        io.write_string("is_surrogate: wrong\n\n", !IO)
    ),

    ( if
        not is_noncharacter('\ufdcf'),
            is_noncharacter('\ufdd0'),
            is_noncharacter('\ufdef'),
        not is_noncharacter('\ufdf0'),
        not is_noncharacter('\ufffd'),
            is_noncharacter('\ufffe'),
            is_noncharacter('\uffff'),
        not is_noncharacter('\U0001fffd'),
            is_noncharacter('\U0001fffe'),
            is_noncharacter('\U0001ffff'),
            is_noncharacter('\U000ffffe'),
            is_noncharacter('\U000fffff'),
            is_noncharacter('\U0010fffe'),
            is_noncharacter('\U0010ffff')
    then
        io.write_string("is_noncharacter: okay\n\n", !IO)
    else
        io.write_string("is_noncharacter: incorrect\n\n", !IO)
    ),
    true.

:- func chr(int) = char.

chr(I) = char.det_from_int(I).

:- pred test_to_utf8(char::in, io::di, io::uo) is det.

test_to_utf8(C, !IO) :-
    char.to_int(C, Int),
    ( if to_utf8(C, CodeUnitList) then
        io.format("U+%04x = ", [i(Int)], !IO),
        io.write_line(CodeUnitList, !IO)
    else
        io.format("U+%04x invalid\n", [i(Int)], !IO)
    ).

:- pred test_to_utf16(char::in, io::di, io::uo) is det.

test_to_utf16(C, !IO) :-
    char.to_int(C, Int),
    ( if to_utf16(C, CodeUnitList) then
        io.format("U+%04x = ", [i(Int)], !IO),
        io.write_line(CodeUnitList, !IO)
    else
        io.format("U+%04x invalid\n", [i(Int)], !IO)
    ).
