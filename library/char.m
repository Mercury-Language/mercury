%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2008, 2011 The University of Melbourne.
% Copyright (C) 2013-2015, 2017-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: char.m.
% Main author: fjh.
% Stability: high.
%
% This module defines some predicates that manipulate characters.
%
% Originally we used `character' rather than `char' for the type name
% because `char' was used by NU-Prolog to mean something different.
% But now we use `char' and the use of `character' is discouraged.
%
% All predicates and functions exported by this module that deal with
% Unicode conform to version 13 of the Unicode standard.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module char.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % A Unicode code point.
    %
:- type char == character.

:- instance enum(character).
:- instance uenum(character).

    % `to_int'/1 and `to_int(in, out)' convert a character to its
    % corresponding numerical code (integer value).
    %
    % `to_int(out, in)' converts an integer value to a character value.
    % It fails for integer values outside of the Unicode range.
    %
    % Be aware that there is no guarantee that characters can be written to
    % files or to the standard output or standard error streams. Files using an
    % 8-bit national character set would only be able to represent a subset of
    % all possible code points. Currently, the Mercury standard library can
    % only read and write UTF-8 text files, so the entire range is supported
    % (excluding surrogate and noncharacter code points).
    %
    % Note that '\0' is not accepted as a Mercury null character literal.
    % Instead, a null character can be created using `det_from_int(0)'.
    % Null characters are not allowed in Mercury strings in C grades.
    %
:- func to_int(char) = int.
:- pred to_int(char, int).
:- mode to_int(in, out) is det.
:- mode to_int(in, in) is semidet.    % implied
:- mode to_int(out, in) is semidet.

    % Converts an integer to its corresponding character, if any.
    % A more expressive name for the reverse mode of to_int.
    %
:- pred from_int(int::in, char::out) is semidet.

    % Converts an integer to its corresponding character.
    % Throws an exception if there isn't one.
    %
:- func det_from_int(int) = char.
:- pred det_from_int(int::in, char::out) is det.

    % Converts a character to its numerical character code (unsigned integer).
    %
:- func to_uint(char) = uint.

    % Converts an unsigned integer to its corresponding character, if any.
    %
:- pred from_uint(uint::in, char::out) is semidet.

    % Converts an unsigned integer to its corresponding character.
    % Throws an exception if there isn't one.
    %
:- func det_from_uint(uint) = char.

    % Returns the minimum numerical character code.
    %
:- func min_char_value = int.
:- pred min_char_value(int::out) is det.

    % Returns the maximum numerical character code.
    %
:- func max_char_value = int.
:- pred max_char_value(int::out) is det.

%---------------------------------------------------------------------------%

    % True iff the character is a lowercase letter (a-z) in the ASCII range.
    %
:- pred is_lower(char::in) is semidet.

    % True iff the character is an uppercase letter (A-Z) in the ASCII range.
    %
:- pred is_upper(char::in) is semidet.

    % Convert a character to lowercase.
    % Note that this only converts letters (A-Z) in the ASCII range.
    %
:- func to_lower(char) = char.
:- pred to_lower(char::in, char::out) is det.

    % Convert a character to uppercase.
    % Note that this only converts letters (a-z) in the ASCII range.
    %
:- func to_upper(char) = char.
:- pred to_upper(char::in, char::out) is det.

    % lower_upper(Lower, Upper) is true iff
    % Lower is a lowercase letter (a-z) and Upper is the corresponding
    % uppercase letter (A-Z) in the ASCII range.
    %
:- pred lower_upper(char, char).
:- mode lower_upper(in, out) is semidet.
:- mode lower_upper(out, in) is semidet.

%---------------------------------------------------------------------------%

    % True iff the character is in the ASCII range (0-127).
    %
:- pred is_ascii(char::in) is semidet.

    % True iff the character is a whitespace character in the ASCII range:
    %
    %   U+0020  space
    %   U+0009  character tabulation (horizontal tab)
    %   U+000A  line feed
    %   U+000B  line tabulation (vertical tab)
    %   U+000C  form feed
    %   U+000D  carriage return
    %
:- pred is_whitespace(char::in) is semidet.

    % True iff the character is a letter (A-Z, a-z) in the ASCII range.
    %
:- pred is_alpha(char::in) is semidet.

    % True iff the character is a letter (A-Z, a-z) or digit (0-9)
    % in the ASCII range.
    %
:- pred is_alnum(char::in) is semidet.

    % True iff the character is a letter (A-Z, a-z) or an underscore (_)
    % in the ASCII range.
    %
:- pred is_alpha_or_underscore(char::in) is semidet.

    % True iff the character is a letter (A-Z, a-z), a digit (0-9) or an
    % underscore (_) in the ASCII range.
    %
:- pred is_alnum_or_underscore(char::in) is semidet.

%---------------------------------------------------------------------------%

    % True iff the character is a decimal digit (0-9) in the ASCII range.
    %
:- pred is_digit(char::in) is semidet.

    % True iff the character is a binary digit (0 or 1) in the ASCII range.
    %
:- pred is_binary_digit(char::in) is semidet.

    % True iff the character is an octal digit (0-7) in the ASCII range.
    %
:- pred is_octal_digit(char::in) is semidet.

    % True iff the character is a decimal digit (0-9) in the ASCII range.
    % Synonym for is_digit/1.
    %
:- pred is_decimal_digit(char::in) is semidet.

    % True iff the character is a hexadecimal digit (0-9, a-f, A-F) in the
    % ASCII range.
    %
:- pred is_hex_digit(char::in) is semidet.

    % is_base_digit(Base, Digit):
    % True iff Digit is a digit in the given Base (0-9, a-z, A-Z).
    % Throws an exception if Base < 2 or Base > 36.
    %
:- pred is_base_digit(int::in, char::in) is semidet.

%---------------------%

    % binary_digit_to_int(Char, Int):
    % True iff Char is a binary digit (0-1) representing the value Int.
    %
:- pred binary_digit_to_int(char::in, int::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_binary_digit_to_int(char) = int.

    % octal_digit_to_int(Char, Int):
    % True iff Char is an octal digit (0-7) representing the value Int.
    %
:- pred octal_digit_to_int(char::in, int::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_octal_digit_to_int(char) = int.

    % decimal_digit_to_int(Char, Int):
    % True iff Char is a decimal digit (0-9) representing the value Int.
    %
:- pred decimal_digit_to_int(char::in, int::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_decimal_digit_to_int(char) = int.

    % hex_digit_to_int(Char, Int):
    % True iff Char is a hexadecimal digit (0-9, a-z or A-F) representing the
    % value Int.
    %
:- pred hex_digit_to_int(char::in, int::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_hex_digit_to_int(char) = int.

    % base_digit_to_int(Base, Char, Int):
    % True iff Char is a decimal digit (0-9) or a letter (a-z, A-Z)
    % representing the value Int (0-35) in the given base.
    % Throws an exception if Base < 2 or Base > 36.
    %
:- pred base_digit_to_int(int::in, char::in, int::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_base_digit_to_int(int, char) = int.

    % A version of base_digit_to_int that does not check whether
    % Base is in the range 2 to 36. If it is not, the behavior is undefined.
    %
:- pred unsafe_base_digit_to_int(int::in, char::in, int::out) is semidet.

%---------------------%

    % Convert an integer in the range 0-1 to a binary digit (0 or 1) in the
    % ASCII range.
    %
:- pred int_to_binary_digit(int::in, char::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_int_to_binary_digit(int) = char.

    % Convert an integer 0-7 to an octal digit (0-7) in the ASCII range.
    %
:- pred int_to_octal_digit(int::in, char::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_int_to_octal_digit(int) = char.

    % Convert an integer 0-9 to a decimal digit (0-9) in the ASCII range.
    %
:- pred int_to_decimal_digit(int::in, char::out) is semidet.

    % As above, but throw an exception in instead of failing.
    %
:- func det_int_to_decimal_digit(int) = char.

    % Convert an integer 0-15 to an uppercase hexadecimal digit (0-9, A-F) in
    % the ASCII range.
    %
:- pred int_to_hex_digit(int::in, char::out) is semidet.

    % As above, but throw an exception in instead of failing.
    %
:- func det_int_to_hex_digit(int) = char.

    % base_int_to_digit(Base, Int, Char):
    % True iff Char is a decimal digit (0-9) or an uppercase letter (A-Z)
    % representing the value Int (0-35) in the given base.
    % Throws an exception if Base < 2 or Base > 36.
    %
:- pred base_int_to_digit(int::in, int::in, char::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_base_int_to_digit(int, int) = char.

%---------------------------------------------------------------------------%

    % Encode a Unicode code point in UTF-8.
    % Fails for surrogate code points.
    %
:- pred to_utf8(char::in, list(int)::out) is semidet.

    % As above, but represent UTF-8 code units using uint8s.
    %
:- pred to_utf8_uint8(char::in, list(uint8)::out) is semidet.

    % Encode a Unicode code point in UTF-16 (native endianness).
    % Fails for surrogate code points.
    %
:- pred to_utf16(char::in, list(int)::out) is semidet.

    % As above, but represent UTF-16 code units using uint16s.
    %
:- pred to_utf16_uint16(char::in, list(uint16)::out) is semidet.

    % True iff the character is a Unicode Surrogate code point, that is a code
    % point in General Category `Other,surrogate' (`Cs').
    % In UTF-16, a code point with a scalar value greater than 0xffff is
    % encoded with a pair of surrogate code points.
    %
:- pred is_surrogate(char::in) is semidet.

    % True iff the character is a Unicode leading surrogate code point.
    % A leading surrogate code point is in the inclusive range from
    % 0xd800 to 0xdbff.
    %
:- pred is_leading_surrogate(char::in) is semidet.

    % True iff the character is a Unicode trailing surrogate code point.
    % A trailing surrogate code point is in the inclusive range from
    % 0xdc00 to 0xdfff.
    %
:- pred is_trailing_surrogate(char::in) is semidet.

    % True iff the character is a Unicode Noncharacter code point.
    % Sixty-six code points are not used to encode characters.
    % These code points should not be used for interchange, but may be used
    % internally.
    %
:- pred is_noncharacter(char::in) is semidet.

    % True iff the character is a Unicode Control code point, that is a code
    % point in General Category `Other,control' (`Cc').
    %
:- pred is_control(char::in) is semidet.

    % True iff the character is a Unicode Space Separator code point, that is a
    % code point in General Category `Separator,space' (`Zs').
    %
:- pred is_space_separator(char::in) is semidet.

    % True iff the character is a Unicode Line Separator code point, that is a
    % code point in General Category `Separator,line' (`Zl').
    %
:- pred is_line_separator(char::in) is semidet.

    % True iff the character is a Unicode Paragraph Separator code point, that
    % is a code point in General Category `Separator,paragraph' (`Zp').
    %
:- pred is_paragraph_separator(char::in) is semidet.

    % True iff the character is a Unicode Private-use code point, that is a
    % code point in General Category `Other,private use' (`Co').
    %
:- pred is_private_use(char::in) is semidet.

%---------------------------------------------------------------------------%

    % Convert a char to a pretty_printer.doc for formatting.
    %
:- func char_to_doc(char) = pretty_printer.doc.
:- pragma obsolete(func(char_to_doc/1), [pretty_printer.char_to_doc/1]).

%---------------------------------------------------------------------------%

% The following have all been deprecated.

    % Use hex_digit_to_int/2 instead.
    %
:- pred is_hex_digit(char, int).
:- mode is_hex_digit(in, out) is semidet.

    % Convert an integer 0-15 to a hexadecimal digit (0-9, A-F) in the ASCII
    % range.
    %
    % Use int_to_hex_digit/2 instead.
    %
:- pred int_to_hex_char(int, char).
:- mode int_to_hex_char(in, out) is semidet.

%---------------------------------------------------------------------------%
%
% Computing hashes of chars.
%

    % Compute a hash value for a char.
    %
:- func hash(char) = int.
:- pred hash(char::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module uint.
:- import_module uint16.
:- import_module uint8.

:- instance enum(character) where [
    (to_int(X) = Y :-
        to_int(X, Y)),
    (from_int(X) = Y :-
        to_int(Y, X))
].

:- instance uenum(character) where [
    func(to_uint/1) is char.to_uint,
    pred(from_uint/2) is char.from_uint
].

:- pragma foreign_decl("C", "#include <limits.h>").

%---------------------------------------------------------------------------%

to_int(C) = N :-
    to_int(C, N).

:- pragma foreign_proc("C",
    to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Int = (MR_UnsignedChar) Character;
").

:- pragma foreign_proc("C",
    to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = ((MR_UnsignedChar) Character == Int);
").

:- pragma foreign_proc("C",
    to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Character = Int;
    SUCCESS_INDICATOR = (Character >= 0 && Character <= 0x10ffff);
").

:- pragma foreign_proc("C#",
    to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = Character;
").

:- pragma foreign_proc("C#",
    to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Character == Int);
").

:- pragma foreign_proc("C#",
    to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = Int;
    SUCCESS_INDICATOR = (Int >= 0 && Int <= 0x10ffff);
").

:- pragma foreign_proc("Java",
    to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = (int) Character;
").

:- pragma foreign_proc("Java",
    to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((int) Character == Int);
").

:- pragma foreign_proc("Java",
    to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = Int;
    SUCCESS_INDICATOR = (Int >= 0 && Int <= 0x10ffff);
").

from_int(Int, Char) :-
    to_int(Char, Int).

det_from_int(Int) = Char :-
    det_from_int(Int, Char).

det_from_int(Int, Char) :-
    ( if from_int(Int, CharPrime) then
        Char = CharPrime
    else
        unexpected($pred, "conversion failed")
    ).

%---------------------------------------------------------------------------%

to_uint(Char) = UInt :-
    UInt = uint.cast_from_int(char.to_int(Char)).

:- pragma foreign_proc("C",
    from_uint(UInt::in, Character::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Character = (MR_UnsignedChar) UInt;
    SUCCESS_INDICATOR = (UInt <= 0x10ffff);
").

:- pragma foreign_proc("C#",
    from_uint(UInt::in, Character::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = (int) UInt;
    SUCCESS_INDICATOR = (UInt <= 0x10ffff);
").

:- pragma foreign_proc("Java",
    from_uint(UInt::in, Character::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = UInt;
    SUCCESS_INDICATOR = ((UInt & 0xffffffffL) <= (0x10ffff & 0xffffffffL));
").

det_from_uint(UInt) = Char :-
    ( if char.from_uint(UInt, CharPrime) then
        Char = CharPrime
    else
        unexpected($pred, "conversion failed")
    ).

%---------------------------------------------------------------------------%

min_char_value = N :-
    min_char_value(N).

    % We use unsigned character codes, so the minimum character code
    % is always zero.
min_char_value(0).

max_char_value = N :-
    max_char_value(N).

:- pragma foreign_proc("C",
    max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Max = 0x10ffff;
").
:- pragma foreign_proc("C#",
    max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = 0x10ffff;
").
:- pragma foreign_proc("Java",
    max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = 0x10ffff;
").

%---------------------------------------------------------------------------%

is_lower(Lower) :-
    lower_upper(Lower, _).

is_upper(Upper) :-
    ( if lower_upper(_, Upper) then
        true
    else
        fail
    ).

to_lower(C1) = C2 :-
    to_lower(C1, C2).

to_lower(Char, Lower) :-
    ( if lower_upper(LowerChar, Char) then
        Lower = LowerChar
    else
        Lower = Char
    ).

to_upper(C1) = C2 :-
    to_upper(C1, C2).

to_upper(Char, Upper) :-
    ( if lower_upper(Char, UpperChar) then
        Upper = UpperChar
    else
        Upper = Char
    ).

lower_upper('a', 'A').
lower_upper('b', 'B').
lower_upper('c', 'C').
lower_upper('d', 'D').
lower_upper('e', 'E').
lower_upper('f', 'F').
lower_upper('g', 'G').
lower_upper('h', 'H').
lower_upper('i', 'I').
lower_upper('j', 'J').
lower_upper('k', 'K').
lower_upper('l', 'L').
lower_upper('m', 'M').
lower_upper('n', 'N').
lower_upper('o', 'O').
lower_upper('p', 'P').
lower_upper('q', 'Q').
lower_upper('r', 'R').
lower_upper('s', 'S').
lower_upper('t', 'T').
lower_upper('u', 'U').
lower_upper('v', 'V').
lower_upper('w', 'W').
lower_upper('x', 'X').
lower_upper('y', 'Y').
lower_upper('z', 'Z').

%---------------------------------------------------------------------------%

is_ascii(Char) :-
    Code = char.to_int(Char),
    Code >= 0x00,
    Code =< 0x7f.

% The information here is duplicated in lookup_token_action
% in mercury_term_lexer.m. If you update this; you will also need update that.
is_whitespace(' ').
is_whitespace('\t').
is_whitespace('\n').
is_whitespace('\r').
is_whitespace('\f').
is_whitespace('\v').

is_alpha(Char) :-
    ( if is_lower(Char) then
        true
    else if is_upper(Char) then
        true
    else
        fail
    ).

is_alnum(Char) :-
    ( if is_alpha(Char) then
        true
    else if is_digit(Char) then
        true
    else
        fail
    ).

is_alpha_or_underscore(Char) :-
    ( if Char = '_' then
        true
    else
        is_alpha(Char)
    ).

is_alnum_or_underscore(Char) :-
    % We explicitly enumerate here for efficiency.
    % (The information here and in some of the following predicates,
    % e.g. lower_upper, is duplicated in lookup_token_action
    % in mercury_term_lexer.m.)
    %
    % A more concise implementation would be:
    %   ( if is_digit(Char) then
    %       true
    %   else
    %       is_alpha_or_underscore(Char)
    %   ).

    ( Char = '0' ; Char = '1' ; Char = '2' ; Char = '3' ; Char = '4'
    ; Char = '5' ; Char = '6' ; Char = '7' ; Char = '8' ; Char = '9'
    ; Char = 'a' ; Char = 'b' ; Char = 'c' ; Char = 'd' ; Char = 'e'
    ; Char = 'f' ; Char = 'g' ; Char = 'h' ; Char = 'i' ; Char = 'j'
    ; Char = 'k' ; Char = 'l' ; Char = 'm' ; Char = 'n' ; Char = 'o'
    ; Char = 'p' ; Char = 'q' ; Char = 'r' ; Char = 's' ; Char = 't'
    ; Char = 'u' ; Char = 'v' ; Char = 'w' ; Char = 'x' ; Char = 'y'
    ; Char = 'z'
    ; Char = 'A' ; Char = 'B' ; Char = 'C' ; Char = 'D' ; Char = 'E'
    ; Char = 'F' ; Char = 'G' ; Char = 'H' ; Char = 'I' ; Char = 'J'
    ; Char = 'K' ; Char = 'L' ; Char = 'M' ; Char = 'N' ; Char = 'O'
    ; Char = 'P' ; Char = 'Q' ; Char = 'R' ; Char = 'S' ; Char = 'T'
    ; Char = 'U' ; Char = 'V' ; Char = 'W' ; Char = 'X' ; Char = 'Y'
    ; Char = 'Z'
    ; Char = '_'
    ).

%---------------------------------------------------------------------------%

% Lots of big tables.
%
% It is conceivable that there are more efficient implementations,
% but these versions are very portable.

%---------------------------------------------------------------------------%
%
% Digit classification.
%

is_digit(D) :-
    is_decimal_digit(D).

is_binary_digit('0').
is_binary_digit('1').

is_octal_digit('0').
is_octal_digit('1').
is_octal_digit('2').
is_octal_digit('3').
is_octal_digit('4').
is_octal_digit('5').
is_octal_digit('6').
is_octal_digit('7').

is_decimal_digit('0').
is_decimal_digit('1').
is_decimal_digit('2').
is_decimal_digit('3').
is_decimal_digit('4').
is_decimal_digit('5').
is_decimal_digit('6').
is_decimal_digit('7').
is_decimal_digit('8').
is_decimal_digit('9').

is_hex_digit('0').
is_hex_digit('1').
is_hex_digit('2').
is_hex_digit('3').
is_hex_digit('4').
is_hex_digit('5').
is_hex_digit('6').
is_hex_digit('7').
is_hex_digit('8').
is_hex_digit('9').
is_hex_digit('a').
is_hex_digit('b').
is_hex_digit('c').
is_hex_digit('d').
is_hex_digit('e').
is_hex_digit('f').
is_hex_digit('A').
is_hex_digit('B').
is_hex_digit('C').
is_hex_digit('D').
is_hex_digit('E').
is_hex_digit('F').

is_base_digit(Base, Digit) :-
    ( if 2 =< Base, Base =< 36 then
        base_digit_to_int(Base, Digit, _Int)
    else
        error($pred, "invalid base")
    ).

%---------------------------------------------------------------------------%
%
% Digit to integer conversion.
%

binary_digit_to_int('0', 0).
binary_digit_to_int('1', 1).

det_binary_digit_to_int(Digit) = Int :-
    ( if binary_digit_to_int(Digit, IntPrime) then
        Int = IntPrime
    else
        error($pred, "char.binary_digit_to_int failed")
    ).

octal_digit_to_int('0', 0).
octal_digit_to_int('1', 1).
octal_digit_to_int('2', 2).
octal_digit_to_int('3', 3).
octal_digit_to_int('4', 4).
octal_digit_to_int('5', 5).
octal_digit_to_int('6', 6).
octal_digit_to_int('7', 7).

det_octal_digit_to_int(Digit) = Int :-
    ( if octal_digit_to_int(Digit, IntPrime) then
        Int = IntPrime
    else
        error($pred, "char.octal_digit_to_int failed")
    ).

decimal_digit_to_int('0', 0).
decimal_digit_to_int('1', 1).
decimal_digit_to_int('2', 2).
decimal_digit_to_int('3', 3).
decimal_digit_to_int('4', 4).
decimal_digit_to_int('5', 5).
decimal_digit_to_int('6', 6).
decimal_digit_to_int('7', 7).
decimal_digit_to_int('8', 8).
decimal_digit_to_int('9', 9).

det_decimal_digit_to_int(Digit) = Int :-
    ( if decimal_digit_to_int(Digit, IntPrime) then
        Int = IntPrime
    else
        error($pred, "char.decimal_digit_to_int failed")
    ).

hex_digit_to_int('0', 0).
hex_digit_to_int('1', 1).
hex_digit_to_int('2', 2).
hex_digit_to_int('3', 3).
hex_digit_to_int('4', 4).
hex_digit_to_int('5', 5).
hex_digit_to_int('6', 6).
hex_digit_to_int('7', 7).
hex_digit_to_int('8', 8).
hex_digit_to_int('9', 9).
hex_digit_to_int('a', 10).
hex_digit_to_int('b', 11).
hex_digit_to_int('c', 12).
hex_digit_to_int('d', 13).
hex_digit_to_int('e', 14).
hex_digit_to_int('f', 15).
hex_digit_to_int('A', 10).
hex_digit_to_int('B', 11).
hex_digit_to_int('C', 12).
hex_digit_to_int('D', 13).
hex_digit_to_int('E', 14).
hex_digit_to_int('F', 15).

det_hex_digit_to_int(DigitStr) = Int :-
    ( if hex_digit_to_int(DigitStr, IntPrime) then
        Int = IntPrime
    else
        error($pred, "char.hex_digit_to_int failed")
    ).

base_digit_to_int(Base, DigitStr, Int) :-
    ( if 1 < Base, Base < 37 then
        unsafe_base_digit_to_int(Base, DigitStr, Int)
    else
        error($pred, "base is not in the range 2 .. 36")
    ).

det_base_digit_to_int(Base, DigitStr) = Int :-
    ( if base_digit_to_int(Base, DigitStr, IntPrime) then
        Int = IntPrime
    else
        error($pred, "char.base_digit_to_int failed")
    ).

unsafe_base_digit_to_int(Base, DigitStr0, Int) :-
    ( if lower_upper(DigitStr0, UpperStr) then
        DigitStr = UpperStr
    else
        DigitStr = DigitStr0
    ),
    int_to_extended_digit(Int, DigitStr),
    Int < Base.

%---------------------------------------------------------------------------%
%
% Integer to digit conversion.
%

int_to_binary_digit(0, '0').
int_to_binary_digit(1, '1').

det_int_to_binary_digit(Int) = Digit :-
    ( if int_to_binary_digit(Int, DigitPrime) then
        Digit = DigitPrime
    else
        error($pred, "char.int_to_binary_digit failed")
    ).

int_to_octal_digit(0, '0').
int_to_octal_digit(1, '1').
int_to_octal_digit(2, '2').
int_to_octal_digit(3, '3').
int_to_octal_digit(4, '4').
int_to_octal_digit(5, '5').
int_to_octal_digit(6, '6').
int_to_octal_digit(7, '7').

det_int_to_octal_digit(Int) = Digit :-
    ( if int_to_octal_digit(Int, DigitPrime) then
        Digit = DigitPrime
    else
        error($pred, "char.int_to_octal_digit failed")
    ).

int_to_decimal_digit(0, '0').
int_to_decimal_digit(1, '1').
int_to_decimal_digit(2, '2').
int_to_decimal_digit(3, '3').
int_to_decimal_digit(4, '4').
int_to_decimal_digit(5, '5').
int_to_decimal_digit(6, '6').
int_to_decimal_digit(7, '7').
int_to_decimal_digit(8, '8').
int_to_decimal_digit(9, '9').

det_int_to_decimal_digit(Int) = Digit :-
    ( if int_to_decimal_digit(Int, DigitPrime) then
        Digit = DigitPrime
    else
        error($pred, "char.int_to_decimal_digit failed")
    ).

int_to_hex_digit(0, '0').
int_to_hex_digit(1, '1').
int_to_hex_digit(2, '2').
int_to_hex_digit(3, '3').
int_to_hex_digit(4, '4').
int_to_hex_digit(5, '5').
int_to_hex_digit(6, '6').
int_to_hex_digit(7, '7').
int_to_hex_digit(8, '8').
int_to_hex_digit(9, '9').
int_to_hex_digit(10, 'A').
int_to_hex_digit(11, 'B').
int_to_hex_digit(12, 'C').
int_to_hex_digit(13, 'D').
int_to_hex_digit(14, 'E').
int_to_hex_digit(15, 'F').

det_int_to_hex_digit(Int) = Digit :-
    ( if int_to_hex_digit(Int, DigitPrime) then
        Digit = DigitPrime
    else
        error($pred, "char.int_to_hex_digit failed")
    ).

base_int_to_digit(Base, Int, Digit) :-
    ( if 1 < Base, Base < 37 then
        Int < Base,
        int_to_extended_digit(Int, Digit)
    else
        error($pred, "invalid base")
    ).

det_base_int_to_digit(Base, Int) = Digit :-
    ( if base_int_to_digit(Base, Int, DigitPrime) then
        Digit = DigitPrime
    else
        error($pred, "char.base_int_to_digit failed")
    ).

%---------------------------------------------------------------------------%
%
% Conversion to UTF-8 code units.
%

to_utf8(Char, CodeUnits) :-
    to_utf8_code_units(Char, NumCodeUnits, A, B, C, D),
    (
        NumCodeUnits = 1,
        CodeUnits = [uint8.to_int(A)]
    ;
        NumCodeUnits = 2,
        CodeUnits = [uint8.to_int(A), uint8.to_int(B)]
    ;
        NumCodeUnits = 3,
        CodeUnits = [uint8.to_int(A), uint8.to_int(B), uint8.to_int(C)]
    ;
        NumCodeUnits = 4,
        CodeUnits = [uint8.to_int(A), uint8.to_int(B),
            uint8.to_int(C), uint8.to_int(D)]
    ).

to_utf8_uint8(Char, CodeUnits) :-
    to_utf8_code_units(Char, NumCodeUnits, A, B, C, D),
    (
        NumCodeUnits = 1,
        CodeUnits = [A]
    ;
        NumCodeUnits = 2,
        CodeUnits = [A, B]
    ;
        NumCodeUnits = 3,
        CodeUnits = [A, B, C]
    ;
        NumCodeUnits = 4,
        CodeUnits = [A, B, C, D]
    ).

:- pred to_utf8_code_units(char::in, int::out(bound(1 ; 2 ; 3 ; 4)),
    uint8::out, uint8::out, uint8::out, uint8::out) is semidet.

to_utf8_code_units(Char, NumCodeUnits, A, B, C, D) :-
    Int = char.to_int(Char),
    ( if Int =< 0x7f then
        A = uint8.cast_from_int(Int),
        B = 0u8,
        C = 0u8,
        D = 0u8,
        NumCodeUnits = 1
    else if Int =< 0x7ff then
        A = uint8.cast_from_int(0xc0 \/ ((Int >> 6) /\ 0x1f)),
        B = uint8.cast_from_int(0x80 \/  (Int       /\ 0x3f)),
        C = 0u8,
        D = 0u8,
        NumCodeUnits = 2
    else if Int =< 0xffff then
        not is_surrogate(Char),
        A = uint8.cast_from_int(0xe0 \/ ((Int >> 12) /\ 0x0f)),
        B = uint8.cast_from_int(0x80 \/ ((Int >>  6) /\ 0x3f)),
        C = uint8.cast_from_int(0x80 \/  (Int        /\ 0x3f)),
        D = 0u8,
        NumCodeUnits = 3
    else if Int =< 0x10ffff then
        A = uint8.cast_from_int(0xf0 \/ ((Int >> 18) /\ 0x07)),
        B = uint8.cast_from_int(0x80 \/ ((Int >> 12) /\ 0x3f)),
        C = uint8.cast_from_int(0x80 \/ ((Int >>  6) /\ 0x3f)),
        D = uint8.cast_from_int(0x80 \/  (Int        /\ 0x3f)),
        NumCodeUnits = 4
    else
        error($pred, "illegal code point")
    ).

%---------------------------------------------------------------------------%
%
% Conversion to UTF-16 code units.
%

to_utf16(Char, CodeUnits) :-
    to_utf16_code_units(Char, NumCodeUnits, A, B),
    (
        NumCodeUnits = 1,
        CodeUnits = [uint16.to_int(A)]
    ;
        NumCodeUnits = 2,
        CodeUnits = [uint16.to_int(A), uint16.to_int(B)]
    ).

to_utf16_uint16(Char, CodeUnits) :-
    to_utf16_code_units(Char, NumCodeUnits, A, B),
    (
        NumCodeUnits = 1,
        CodeUnits = [A]
    ;
        NumCodeUnits = 2,
        CodeUnits = [A, B]
    ).

:- pred to_utf16_code_units(char::in, int::out(bound(1 ; 2)),
    uint16::out, uint16::out) is semidet.

to_utf16_code_units(Char, NumCodeUnits, A, B) :-
    Int = char.to_int(Char),
    ( if Int < 0xd800 then
        % Common case.
        A = uint16.cast_from_int(Int),
        B = 0u16,
        NumCodeUnits = 1
    else if Int =< 0xdfff then
        % Surrogate.
        fail
    else if Int =< 0xffff then
        A = uint16.cast_from_int(Int),
        B = 0u16,
        NumCodeUnits = 1
    else if Int =< 0x10ffff then
        U = Int - 0x10000,
        A = uint16.cast_from_int(0xd800 \/ (U >> 10)),
        B = uint16.cast_from_int(0xdc00 \/ (U /\ 0x3ff)),
        NumCodeUnits = 2
    else
        error($pred, "illegal code point")
    ).

%---------------------------------------------------------------------------%

is_surrogate(Char) :-
    Int = char.to_int(Char),
    Int >= 0xd800,
    Int =< 0xdfff.

is_leading_surrogate(Char) :-
    Int = char.to_int(Char),
    Int >= 0xd800,
    Int =< 0xdbff.

is_trailing_surrogate(Char) :-
    Int = char.to_int(Char),
    Int >= 0xdc00,
    Int =< 0xdfff.

is_noncharacter(Char) :-
    Int = char.to_int(Char),
    ( 0xfdd0 =< Int, Int =< 0xfdef
    ; Int /\ 0xfffe = 0xfffe
    ).

is_control(Char) :-
    Int = char.to_int(Char),
    ( 0x0000 =< Int, Int =< 0x001f
    ; 0x007f =< Int, Int =< 0x009f
    ).

is_space_separator(Char) :-
    Int = char.to_int(Char),
    ( Int = 0x0020
    ; Int = 0x00a0
    ; Int = 0x1680
    ; 0x2000 =< Int, Int =< 0x200a
    ; Int = 0x202f
    ; Int = 0x205f
    ; Int = 0x3000
    ).

is_line_separator(Char) :-
    0x2028 = char.to_int(Char).

is_paragraph_separator(Char) :-
    0x2029 = char.to_int(Char).

is_private_use(Char) :-
    Int = char.to_int(Char),
    ( 0xe000 =< Int, Int =< 0xf8ff     % Private Use Area.
    ; 0xf0000 =< Int, Int =< 0xffffd   % Supplemental Private Use Area-A.
    ; 0x100000 =< Int, Int =< 0x10fffd % Supplemental Private Use Area-B.
    ).

char_to_doc(C) = pretty_printer.char_to_doc(C).

%---------------------------------------------------------------------------%

is_hex_digit(Digit, Int) :-
    hex_digit_to_int(Digit, Int).

int_to_hex_char(Int, Char) :-
    int_to_hex_digit(Int, Char).

%---------------------------------------------------------------------------%

:- pred int_to_extended_digit(int, char).
:- mode int_to_extended_digit(in, out) is semidet.
:- mode int_to_extended_digit(out, in) is semidet.

int_to_extended_digit(0, '0').
int_to_extended_digit(1, '1').
int_to_extended_digit(2, '2').
int_to_extended_digit(3, '3').
int_to_extended_digit(4, '4').
int_to_extended_digit(5, '5').
int_to_extended_digit(6, '6').
int_to_extended_digit(7, '7').
int_to_extended_digit(8, '8').
int_to_extended_digit(9, '9').
int_to_extended_digit(10, 'A').
int_to_extended_digit(11, 'B').
int_to_extended_digit(12, 'C').
int_to_extended_digit(13, 'D').
int_to_extended_digit(14, 'E').
int_to_extended_digit(15, 'F').
int_to_extended_digit(16, 'G').
int_to_extended_digit(17, 'H').
int_to_extended_digit(18, 'I').
int_to_extended_digit(19, 'J').
int_to_extended_digit(20, 'K').
int_to_extended_digit(21, 'L').
int_to_extended_digit(22, 'M').
int_to_extended_digit(23, 'N').
int_to_extended_digit(24, 'O').
int_to_extended_digit(25, 'P').
int_to_extended_digit(26, 'Q').
int_to_extended_digit(27, 'R').
int_to_extended_digit(28, 'S').
int_to_extended_digit(29, 'T').
int_to_extended_digit(30, 'U').
int_to_extended_digit(31, 'V').
int_to_extended_digit(32, 'W').
int_to_extended_digit(33, 'X').
int_to_extended_digit(34, 'Y').
int_to_extended_digit(35, 'Z').

%---------------------------------------------------------------------------%

hash(C) = H :-
    uint.hash(uint.cast_from_int(char.to_int(C)), H).

hash(C, H) :-
    H = hash(C).

%---------------------------------------------------------------------------%
:- end_module char.
%---------------------------------------------------------------------------%
