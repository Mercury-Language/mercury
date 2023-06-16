%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_set_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if string.set_char('x', -1, "", _) then
        error("test failed")
    else
        true
    ),
    ( if string.set_char('x', 0, "", _) then
        error("test failed")
    else
        true
    ),
    ( if  string.set_char('x', 1, "", _) then
        error("test failed")
    else
        true
    ),

    ( if string.set_char('m', 0, "cat", "mat") then
        true
    else
        error("test failed")
    ),
    ( if string.set_char('u', 1, "cat", "cut") then
        true
    else
        error("test failed")
    ),
    ( if string.set_char('b', 2, "cat", "cab") then
        true
    else
        error("test failed")
    ),
    ( if string.set_char('x', 3, "cat", _) then
        error("test failed")
    else
        true
    ),

    % Test variable-width characters.
    % In UTF-8:
    %   y           1 code unit
    %   ý           2 code units
    %   ẏ           3 code units
    %   U+10000     4 code units
    ( if
        set_char_by_cp(".aßξ啕𐀀.", 1, 'y', ".yßξ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 1, 'ý', ".ýßξ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 1, 'ẏ', ".ẏßξ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 1, '𐀀', ".𐀀ßξ啕𐀀.")
    then
        true
    else
        io.write_string("variable-width set_char failed (1)\n", !IO)
    ),
    ( if
        set_char_by_cp(".aßξ啕𐀀.", 2, 'y', ".ayξ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 2, 'ý', ".aýξ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 2, 'ẏ', ".aẏξ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 2, '𐀀', ".a𐀀ξ啕𐀀.")
    then
        true
    else
        io.write_string("variable-width set_char failed (2)\n", !IO)
    ),
    ( if
        set_char_by_cp(".aßξ啕𐀀.", 3, 'y', ".aßy啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 3, 'ý', ".aßý啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 3, 'ẏ', ".aßẏ啕𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 3, '𐀀', ".aß𐀀啕𐀀.")
    then
        true
    else
        io.write_string("variable-width set_char failed (3)\n", !IO)
    ),
    ( if
        set_char_by_cp(".aßξ啕𐀀.", 4, 'y', ".aßξy𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 4, 'ý', ".aßξý𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 4, 'ẏ', ".aßξẏ𐀀."),
        set_char_by_cp(".aßξ啕𐀀.", 4, '𐀀', ".aßξ𐀀𐀀.")
    then
        true
    else
        io.write_string("variable-width set_char failed (4)\n", !IO)
    ),
    ( if
        set_char_by_cp(".aßξ啕𐀀.", 5, 'y', ".aßξ啕y."),
        set_char_by_cp(".aßξ啕𐀀.", 5, 'ý', ".aßξ啕ý."),
        set_char_by_cp(".aßξ啕𐀀.", 5, 'ẏ', ".aßξ啕ẏ."),
        set_char_by_cp(".aßξ啕𐀀.", 5, '𐀀', ".aßξ啕𐀀.")
    then
        true
    else
        io.write_string("variable-width set_char failed (5)\n", !IO)
    ),

    io.write_string("test finished\n", !IO).

:- pred set_char_by_cp(string::in, int::in, char::in, string::out) is semidet.

set_char_by_cp(Str0, CodePoint, Char, Str) :-
    string.codepoint_offset(Str0, CodePoint, Offset),
    string.set_char(Char, Offset, Str0, Str),
    trace [io(!IO), runtime(env("DEBUG"))] (
        io.write_string(Str, !IO),
        io.nl(!IO)
    ).
