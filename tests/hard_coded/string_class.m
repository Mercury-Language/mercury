%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the string predicates that test whether every code point in a string
% belongs to some class of characters.
%
% Each row of the output gives the result of every such predicate on one
% test string.
%
% The .exp file is for backends using UTF-8 as the string encoding.
% The .exp2 file is for backends using UTF-16 as the string encoding.
% The two outputs differ only in the rows for ill-formed strings: on UTF-8
% backends, an ill-formed code unit is a lone byte, while on UTF-16 backends,
% it is an unpaired surrogate code point.
%
%---------------------------------------------------------------------------%

:- module string_class.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string(legend, !IO),
    write_row("string",
        "E", "A", "AN", "A_", "AN_", "D", "M", "N", !IO),
    list.foldl(test_string, test_strings, !IO).

:- func legend = string.

legend =
    "E  = is_all_empty      A_  = is_all_alpha_or_underscore\n" ++
    "A  = is_all_alpha      AN_ = is_all_alnum_or_underscore\n" ++
    "AN = is_all_alnum      M   = all_match(ascii_alpha)\n" ++
    "D  = is_all_digits     N   = all_match(nonascii)\n" ++
    "\n" ++
    "A and M should agree on every well-formed string.\n" ++
    "\n".

%---------------------------------------------------------------------------%

:- pred test_string(string::in, io::di, io::uo) is det.

test_string(Str, !IO) :-
    write_row(describe(Str),
        tf(is_empty(Str)),
        tf(is_all_alpha(Str)),
        tf(is_all_alnum(Str)),
        tf(is_all_alpha_or_underscore(Str)),
        tf(is_all_alnum_or_underscore(Str)),
        tf(is_all_digits(Str)),
        tf(all_match(ascii_alpha, Str)),
        tf(all_match(nonascii, Str)),
        !IO).

:- pred write_row(string::in, string::in, string::in, string::in, string::in,
    string::in, string::in, string::in, string::in, io::di, io::uo) is det.

write_row(Desc, Empty, Alpha, Alnum, AlphaU, AlnumU, Digit, Match, NonAscii,
        !IO) :-
    io.format("%-32s %-3s %-3s %-3s %-3s %-3s %-3s %-3s %-3s\n",
        [s(Desc), s(Empty), s(Alpha), s(Alnum), s(AlphaU), s(AlnumU),
        s(Digit), s(Match), s(NonAscii)], !IO).

    % Call the given predicate, returning "T" if it succeeds and "F" if it
    % fails.
    %
:- func tf((pred)::in((pred) is semidet)) = (string::out) is det.

tf(Pred) = ( if Pred then "T" else "F" ).

%---------------------------------------------------------------------------%

    % A local definition of the class of characters that is_all_alpha is
    % documented to accept.
    %
:- pred ascii_alpha(char::in) is semidet.

ascii_alpha(Char) :-
    char.to_int(Char, Int),
    ( 0'A =< Int, Int =< 0'Z
    ; 0'a =< Int, Int =< 0'z
    ).

:- pred nonascii(char::in) is semidet.

nonascii(Char) :-
    char.to_int(Char, Int),
    Int > 0x7f.

%---------------------------------------------------------------------------%

:- func describe(string) = string.

describe(Str) = Desc :-
    describe_loop(Str, 0, [], RevPieces),
    Desc = "\"" ++ string.append_list(list.reverse(RevPieces)) ++ "\"".

:- pred describe_loop(string::in, int::in,
    list(string)::in, list(string)::out) is det.

describe_loop(Str, Index, !RevPieces) :-
    ( if
        string.index_next_repl(Str, Index, NextIndex, Char, MaybeReplaced)
    then
        (
            MaybeReplaced = replaced_code_unit(CodeUnit),
            Piece = string.format("\\x%02x\\",
                [i(uint8.to_int(CodeUnit))])
        ;
            MaybeReplaced = not_replaced,
            Piece = describe_char(Char)
        ),
        !:RevPieces = [Piece | !.RevPieces],
        describe_loop(Str, NextIndex, !RevPieces)
    else
        true
    ).

:- func describe_char(char) = string.

describe_char(Char) = Piece :-
    char.to_int(Char, Int),
    ( if
        Char = ('\\')
    then
        Piece = "\\\\"
    else if
        Char = ('"')
    then
        Piece = "\\\""
    else if
        Char = '\t'
    then
        Piece = "\\t"
    else if
        Char = '\r'
    then
        Piece = "\\r"
    else if
        Char = '\n'
    then
        Piece = "\\n"
    else if
        0x20 =< Int, Int =< 0x7e
    then
        Piece = char_to_string(Char)
    else if
        Int =< 0xffff
    then
        Piece = string.format("\\u%04x", [i(Int)])
    else
        Piece = string.format("\\U%08x", [i(Int)])
    ).

%---------------------------------------------------------------------------%

:- func test_strings = list(string).

test_strings = [
    % The empty string.
    "",

    % Strings that look empty, but are not.
    " ",
    "  ",
    "\t",
    "\n",
    "\r",
    " \t\n\r",
    "\u00a0",       % no-break space
    "\u2002",       % en space
    "\u3000",       % ideographic space
    "\u200b",       % zero width space
    "\ufeff",       % zero width no-break space (BOM)
    "\u200e",       % left-to-right mark
    "\u0301",       % combining acute accent — a mark with no base

    % One character from each relevant class.
    "a",
    "Z",
    "0",
    "_",
    ".",
    " ",

    % Whole classes.
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "abcdefghijklmnopqrstuvwxyz",
    "0123456789",
    "___",

    % Mixtures.
    "abc123",
    "123abc",
    "a1b2c3",
    "abc_123",
    "_abc",
    "abc_",
    "a_1",
    "_1",

    % The nonmatching character first, in the middle, and last.
    "!abc",
    "ab!c",
    "abc!",
    "!123",
    "12!3",
    "123!",

    % The characters immediately outside each accepted ASCII range:
    % '@' is 'A'-1, '[' is 'Z'+1, '`' is 'a'-1, '{' is 'z'+1,
    % '/' is '0'-1 and ':' is '9'+1.
    "@",
    "[",
    "`",
    "{",
    "/",
    ":",
    "@A",
    "Z[",
    "`a",
    "z{",
    "/0",
    "9:",

    % Whitespace next to accepted characters.
    " ab",
    "ab ",
    "a b",
    " 12",
    "12 ",
    "1 2",

    % Non-ASCII letters and digits. Every predicate here except the
    % all_match(nonascii) column is documented to accept ASCII only,
    % so all of these should fail.
    "\u00e9",              % e with acute accent
    "\u00df",              % sharp s
    "\u03be",              % greek small letter xi
    "\u5555",              % a CJK ideograph
    "\U00010000",          % a non-BMP code point
    "\u00b2",              % superscript two
    "\u0660",              % arabic-indic digit zero
    "\uff11",              % fullwidth digit one
    "aßξ啕𐀀.",             % from the original version of this test
    "ßξ啕𐀀",               % from the original verison of this test
    "abc\u00e9",
    "\u00e9abc",

    % Ill-formed code unit sequences, alone and adjacent to characters that
    % the predicates do accept.
    ilseq_head,
    ilseq_tail,
    "abc" ++ ilseq_head,
    ilseq_head ++ "abc",
    "ab" ++ ilseq_head ++ "cd",
    "123" ++ ilseq_tail,
    ilseq_head ++ ilseq_tail
].

    % The first code unit of a code point whose encoding needs
    % more than one code unit.
    %
:- func ilseq_head = string.

ilseq_head = string.between(non_bmp, 0, 1).

    % The remaining code units of that same code point above.
    % Each is an ill-formed string on its own, on every backend.
    %
:- func ilseq_tail = string.

ilseq_tail = string.between(non_bmp, 1, string.length(non_bmp)).

:- func non_bmp = string.

non_bmp = "\U0001F600".

%---------------------------------------------------------------------------%
:- end_module string_class.
%---------------------------------------------------------------------------%
