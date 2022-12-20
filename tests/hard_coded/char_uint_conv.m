%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test conversion of uints to characters and vice versa.
% The .exp file is for grades where uint is 64 bits.
% The .exp2 file is for grades where uint is 32 bits.
%---------------------------------------------------------------------------%

:- module char_uint_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.
:- import_module uint.
:- import_module term_io.

main(!IO) :-
    io.print_line("Testing uint -> char conversion:", !IO),
    list.foldl(test_from_uint, test_uints, !IO),
    io.nl(!IO),
    io.print_line("Testing char -> uint conversion:", !IO),
    list.foldl(test_to_uint, test_chars, !IO).

:- pred test_from_uint(uint::in, io::di, io::uo) is det.

test_from_uint(UInt, !IO) :-
    ( if char.from_uint(UInt, Char) then
        io.format("from_uint(0x%x) ==> %s\n", [u(UInt), s(quoted_char(Char))], !IO)
    else
        io.format("from_uint(0x%x) ==> FAILED\n", [u(UInt)], !IO)
    ).

:- func test_uints = list(uint).

test_uints = [
    0x0_u,
    1_u,
    0x7f_u,
    0x80_u,
    0x7ff_u,
    0x800_u,
    0xffff_u,
    0x10000_u,
    0x10ffff_u,
    0x110000_u,
    max_uint - 1u,
    max_uint].

:- pred test_to_uint(char::in, io::di, io::uo) is det.

test_to_uint(Char, !IO) :-
    UInt = char.to_uint(Char),
    io.format("to_uint(%s) ==> 0x%x\n", [s(quoted_char(Char)), u(UInt)], !IO).

:- func test_chars = list(char).

test_chars = [
    % C0 Controls and Basic Latin
    % (First BMP block)
    char.det_from_int(0x0),   % NULL CHARACTER
    char.det_from_int(0x1),   % START OF HEADING
    char.det_from_int(0x1f),  % UNIT SEPARATOR
    ' ',
    '0',
    'A',
    'Z',
    'a',
    'z',
    '~',
    char.det_from_int(0x7f), % DELETE

    % C1 Controls and Latin-1 Supplement
    char.det_from_int(0x80),   % PADDING CHARACTER
    char.det_from_int(0x9f),   % APPLICATION PROGRAM COMMAND
    char.det_from_int(0xa0),   % NO-BREAK SPACE
    char.det_from_int(0xbf),   % INVERTED QUESTION MARK
    char.det_from_int(0xc0),   % LATIN CAPITAL LETTER A WITH GRAVE
    char.det_from_int(0xff),   % LATIN SMALL LETTER Y WITH DIAERESIS

    % Linear B Syllabary
    % (First SMP block)
    char.det_from_int(0x10000), % LINEAR B SYLLABLE B008 A
    char.det_from_int(0x1005d), % LINEAR B SYMBOL B089

    % Symbols for Legacy Computing.
    % (Last SMP block)
    char.det_from_int(0x1fb00), % BLOCK SEXTANT-1
    char.det_from_int(0x1fbf9), % SEGEMENTED DIGIT NINE

    % CJK Unified Ideographs Extension B
    % (First SIP block)
    char.det_from_int(0x20000),
    char.det_from_int(0x2a6df),

    % CJK Compatibility Ideographs Supplement
    % (Last SIP block),
    char.det_from_int(0x2f800),
    char.det_from_int(0x2fa1f),

    % Last valid Unicode code point.
    char.det_from_int(0x10ffff)].
