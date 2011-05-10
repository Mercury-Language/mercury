%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: char.m.
% Main author: fjh.
% Stability: high.
%
% This module defines some predicates that manipulate characters.
%
% The set of characters which are supported and the mapping from
% characters to integer values are both implementation-dependent.
%
% Originally we used `character' rather than `char' for the type name
% because `char' was used by NU-Prolog to mean something different.
% But now we use `char' and the use of `character' is discouraged.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module char.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module pretty_printer.

%-----------------------------------------------------------------------------%

    % A Unicode code point.
    %
:- type char == character.

:- instance enum(character).

    % `char.to_int'/1 and `char.to_int(in, out)' convert a character to its
    % corresponding numerical code (integer value).
    %
    % `char.to_int(out, in)' converts an integer value to a character value.
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
    % Instead, a null character can be created using `char.det_from_int(0)'.
    % Null characters are not allowed in Mercury strings in C grades.
    %
:- func char.to_int(char) = int.
:- pred char.to_int(char, int).
:- mode char.to_int(in, out) is det.
:- mode char.to_int(in, in) is semidet.    % implied
:- mode char.to_int(out, in) is semidet.

    % Converts an integer to its corresponding character, if any.
    % A more expressive name for the reverse mode of char.to_int.
    %
:- pred char.from_int(int::in, char::out) is semidet.

    % Converts an integer to its corresponding character. Aborts
    % if there isn't one.
    %
:- pred char.det_from_int(int::in, char::out) is det.
:- func char.det_from_int(int) = char.

    % Returns the maximum numerical character code.
    %
:- func char.max_char_value = int.
:- pred char.max_char_value(int::out) is det.

    % Returns the minimum numerical character code.
    %
:- func char.min_char_value = int.
:- pred char.min_char_value(int::out) is det.

    % Convert a character to uppercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func char.to_upper(char) = char.
:- pred char.to_upper(char::in, char::out) is det.

    % Convert a character to lowercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func char.to_lower(char) = char.
:- pred char.to_lower(char::in, char::out) is det.

    % char.lower_upper(Lower, Upper) is true iff
    % Lower is a lower-case letter and Upper is the corresponding
    % upper-case letter, and both Lower and Upper are unaccented
    % Latin letters.
    %
:- pred char.lower_upper(char, char).
:- mode char.lower_upper(in, out) is semidet.
:- mode char.lower_upper(out, in) is semidet.

    % True iff the character is a whitespace character in the ASCII range,
    % i.e. a space, tab, newline, carriage return, form-feed, or vertical tab.
    %
:- pred char.is_whitespace(char::in) is semidet.

    % True iff the character is an uppercase letter.
    %
:- pred char.is_upper(char::in) is semidet.

    % True iff the character is a lowercase letter.
    %
:- pred char.is_lower(char::in) is semidet.

    % True iff the character is a letter.
    %
:- pred char.is_alpha(char::in) is semidet.

    % True iff the character is a letter or digit.
    %
:- pred char.is_alnum(char::in) is semidet.

    % True iff the character is a letter or an underscore.
    %
:- pred char.is_alpha_or_underscore(char::in) is semidet.

    % True iff the character is a letter, a digit or an underscore.
    %
:- pred char.is_alnum_or_underscore(char::in) is semidet.

    % True iff the character is a decimal digit (0-9).
    %
:- pred char.is_digit(char::in) is semidet.

    % True iff the character is a binary digit (0 or 1).
    %
:- pred char.is_binary_digit(char::in) is semidet.

    % True iff the character is a octal digit (0-7).
    %
:- pred char.is_octal_digit(char::in) is semidet.

    % True iff the character is a hexadecimal digit (0-9, a-f, A-F).
    %
:- pred char.is_hex_digit(char::in) is semidet.

:- pred char.is_hex_digit(char, int).
:- mode char.is_hex_digit(in, out) is semidet.

    % Convert an integer 0-15 to a hexadecimal digit 0-9, A-F.
    %
:- pred char.int_to_hex_char(int, char).
:- mode char.int_to_hex_char(in, out) is semidet.

    % Succeeds if char is a decimal digit (0-9) or letter (a-z or A-Z).
    % Returns the character's value as a digit (0-9 or 10-35).
    %
:- pred char.digit_to_int(char::in, int::out) is semidet.

    % char.int_to_uppercase_digit(Int, DigitChar):
    %
    % True iff `Int' is an integer in the range 0-35 and
    % `DigitChar' is a decimal digit or uppercase letter
    % whose value as a digit is `Int'.
    %
:- pred char.int_to_digit(int, char).
:- mode char.int_to_digit(in, out) is semidet.
:- mode char.int_to_digit(out, in) is semidet.

    % Returns a decimal digit or uppercase letter corresponding to the value.
    % Calls error/1 if the integer is not in the range 0-35.
    %
:- func char.det_int_to_digit(int) = char.
:- pred char.det_int_to_digit(int::in, char::out) is det.

    % Convert a char to a pretty_printer.doc for formatting.
    %
:- func char.char_to_doc(char) = pretty_printer.doc.

    % Encode a Unicode code point in UTF-8.
    % Fails for surrogate code points.
    %
:- pred char.to_utf8(char::in, list(int)::out) is semidet.

    % Encode a Unicode code point in UTF-16 (native endianness).
    % Fails for surrogate code points.
    %
:- pred char.to_utf16(char::in, list(int)::out) is semidet.

    % Succeed if `Char' is a Unicode surrogate code point.
    % In UTF-16, a code point with a scalar value greater than 0xffff
    % is encoded with a pair of surrogate code points.
    %
:- pred char.is_surrogate(char::in) is semidet.

    % Succeed if `Char' is a Noncharacter code point.
    % Sixty-six code points are not used to encode characters.
    % These code points should not be used for interchange, but may be used
    % internally.
    %
:- pred char.is_noncharacter(char::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module term_io.

:- instance enum(character) where [
    (to_int(X) = Y :- char.to_int(X, Y)),
    (from_int(X) = Y :- char.to_int(Y, X))
].

% The information here is duplicated in lookup_token_action in lexer.m.
% If you update this; you will also need update that.
char.is_whitespace(' ').
char.is_whitespace('\t').
char.is_whitespace('\n').
char.is_whitespace('\r').
char.is_whitespace('\f').
char.is_whitespace('\v').

char.is_alpha(Char) :-
    ( char.is_lower(Char) ->
        true
    ; char.is_upper(Char) ->
        true
    ;
        fail
    ).

char.is_alnum(Char) :-
    ( char.is_alpha(Char) ->
        true
    ; char.is_digit(Char) ->
        true
    ;
        fail
    ).

char.is_alpha_or_underscore(Char) :-
    ( Char = '_' ->
        true
    ;
        char.is_alpha(Char)
    ).

    % We explicitly enumerate here for efficiency.
    % (The information here and in some of the following predicates,
    % e.g. char.lower_upper, is duplicated in lookup_token_action in lexer.m.)
char.is_alnum_or_underscore(Char) :-
    ( Char = '0'
    ; Char = '1'
    ; Char = '2'
    ; Char = '3'
    ; Char = '4'
    ; Char = '5'
    ; Char = '6'
    ; Char = '7'
    ; Char = '8'
    ; Char = '9'
    ; Char = 'a'
    ; Char = 'b'
    ; Char = 'c'
    ; Char = 'd'
    ; Char = 'e'
    ; Char = 'f'
    ; Char = 'g'
    ; Char = 'h'
    ; Char = 'i'
    ; Char = 'j'
    ; Char = 'k'
    ; Char = 'l'
    ; Char = 'm'
    ; Char = 'n'
    ; Char = 'o'
    ; Char = 'p'
    ; Char = 'q'
    ; Char = 'r'
    ; Char = 's'
    ; Char = 't'
    ; Char = 'u'
    ; Char = 'v'
    ; Char = 'w'
    ; Char = 'x'
    ; Char = 'y'
    ; Char = 'z'
    ; Char = 'A'
    ; Char = 'B'
    ; Char = 'C'
    ; Char = 'D'
    ; Char = 'E'
    ; Char = 'F'
    ; Char = 'G'
    ; Char = 'H'
    ; Char = 'I'
    ; Char = 'J'
    ; Char = 'K'
    ; Char = 'L'
    ; Char = 'M'
    ; Char = 'N'
    ; Char = 'O'
    ; Char = 'P'
    ; Char = 'Q'
    ; Char = 'R'
    ; Char = 'S'
    ; Char = 'T'
    ; Char = 'U'
    ; Char = 'V'
    ; Char = 'W'
    ; Char = 'X'
    ; Char = 'Y'
    ; Char = 'Z'
    ; Char = '_'
    ).
% A more concise implementation is:
%   ( char.is_digit(Char) ->
%       true
%   ;
%       char.is_alpha_or_underscore(Char)
%   ).

char.is_lower(Lower) :-
    char.lower_upper(Lower, _).

char.is_upper(Upper) :-
    ( char.lower_upper(_, Upper) ->
        true
    ;
        fail
    ).

char.to_lower(C1) = C2 :-
    char.to_lower(C1, C2).

char.to_lower(Char, Lower) :-
    ( char.lower_upper(LowerChar, Char) ->
        Lower = LowerChar
    ;
        Lower = Char
    ).

char.to_upper(C1) = C2 :-
    char.to_upper(C1, C2).

char.to_upper(Char, Upper) :-
    ( char.lower_upper(Char, UpperChar) ->
        Upper = UpperChar
    ;
        Upper = Char
    ).

%-----------------------------------------------------------------------------%

% Lots of big tables.
%
% It's conceivable that there are more efficient implementations,
% but these versions are very portable.

%-----------------------------------------------------------------------------%

char.is_binary_digit('0').
char.is_binary_digit('1').

char.is_octal_digit('0').
char.is_octal_digit('1').
char.is_octal_digit('2').
char.is_octal_digit('3').
char.is_octal_digit('4').
char.is_octal_digit('5').
char.is_octal_digit('6').
char.is_octal_digit('7').

char.is_digit('0').
char.is_digit('1').
char.is_digit('2').
char.is_digit('3').
char.is_digit('4').
char.is_digit('5').
char.is_digit('6').
char.is_digit('7').
char.is_digit('8').
char.is_digit('9').

char.is_hex_digit(X) :- char.is_hex_digit(X, _).

char.is_hex_digit('0', 0).
char.is_hex_digit('1', 1).
char.is_hex_digit('2', 2).
char.is_hex_digit('3', 3).
char.is_hex_digit('4', 4).
char.is_hex_digit('5', 5).
char.is_hex_digit('6', 6).
char.is_hex_digit('7', 7).
char.is_hex_digit('8', 8).
char.is_hex_digit('9', 9).
char.is_hex_digit('a', 10).
char.is_hex_digit('b', 11).
char.is_hex_digit('c', 12).
char.is_hex_digit('d', 13).
char.is_hex_digit('e', 14).
char.is_hex_digit('f', 15).
char.is_hex_digit('A', 10).
char.is_hex_digit('B', 11).
char.is_hex_digit('C', 12).
char.is_hex_digit('D', 13).
char.is_hex_digit('E', 14).
char.is_hex_digit('F', 15).

char.int_to_hex_char(0, '0').
char.int_to_hex_char(1, '1').
char.int_to_hex_char(2, '2').
char.int_to_hex_char(3, '3').
char.int_to_hex_char(4, '4').
char.int_to_hex_char(5, '5').
char.int_to_hex_char(6, '6').
char.int_to_hex_char(7, '7').
char.int_to_hex_char(8, '8').
char.int_to_hex_char(9, '9').
char.int_to_hex_char(10, 'A').
char.int_to_hex_char(11, 'B').
char.int_to_hex_char(12, 'C').
char.int_to_hex_char(13, 'D').
char.int_to_hex_char(14, 'E').
char.int_to_hex_char(15, 'F').

%-----------------------------------------------------------------------------%

char.det_int_to_digit(N) = C :-
    char.det_int_to_digit(N, C).

char.det_int_to_digit(Int, Digit) :-
    ( char.int_to_digit(Int, Digit1) ->
        Digit = Digit1
    ;
        error("char.int_to_digit failed")
    ).

char.int_to_digit(0, '0').
char.int_to_digit(1, '1').
char.int_to_digit(2, '2').
char.int_to_digit(3, '3').
char.int_to_digit(4, '4').
char.int_to_digit(5, '5').
char.int_to_digit(6, '6').
char.int_to_digit(7, '7').
char.int_to_digit(8, '8').
char.int_to_digit(9, '9').
char.int_to_digit(10, 'A').
char.int_to_digit(11, 'B').
char.int_to_digit(12, 'C').
char.int_to_digit(13, 'D').
char.int_to_digit(14, 'E').
char.int_to_digit(15, 'F').
char.int_to_digit(16, 'G').
char.int_to_digit(17, 'H').
char.int_to_digit(18, 'I').
char.int_to_digit(19, 'J').
char.int_to_digit(20, 'K').
char.int_to_digit(21, 'L').
char.int_to_digit(22, 'M').
char.int_to_digit(23, 'N').
char.int_to_digit(24, 'O').
char.int_to_digit(25, 'P').
char.int_to_digit(26, 'Q').
char.int_to_digit(27, 'R').
char.int_to_digit(28, 'S').
char.int_to_digit(29, 'T').
char.int_to_digit(30, 'U').
char.int_to_digit(31, 'V').
char.int_to_digit(32, 'W').
char.int_to_digit(33, 'X').
char.int_to_digit(34, 'Y').
char.int_to_digit(35, 'Z').

char.digit_to_int(Digit, Int) :-
    ( char.lower_upper(Digit, Upper) ->
        char.int_to_digit(Int, Upper)
    ;
        char.int_to_digit(Int, Digit)
    ).

%-----------------------------------------------------------------------------%

char.lower_upper('a', 'A').
char.lower_upper('b', 'B').
char.lower_upper('c', 'C').
char.lower_upper('d', 'D').
char.lower_upper('e', 'E').
char.lower_upper('f', 'F').
char.lower_upper('g', 'G').
char.lower_upper('h', 'H').
char.lower_upper('i', 'I').
char.lower_upper('j', 'J').
char.lower_upper('k', 'K').
char.lower_upper('l', 'L').
char.lower_upper('m', 'M').
char.lower_upper('n', 'N').
char.lower_upper('o', 'O').
char.lower_upper('p', 'P').
char.lower_upper('q', 'Q').
char.lower_upper('r', 'R').
char.lower_upper('s', 'S').
char.lower_upper('t', 'T').
char.lower_upper('u', 'U').
char.lower_upper('v', 'V').
char.lower_upper('w', 'W').
char.lower_upper('x', 'X').
char.lower_upper('y', 'Y').
char.lower_upper('z', 'Z').

%-----------------------------------------------------------------------------%

char.from_int(Int, Char) :-
    char.to_int(Char, Int).

char.det_from_int(Int, Char) :-
    ( char.from_int(Int, CharPrime) ->
        Char = CharPrime
    ;
        error("char.det_from_int: conversion failed")
    ).

char.det_from_int(Int) = Char :-
    char.det_from_int(Int, Char).

:- pragma foreign_proc("C",
    char.to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Int = (MR_UnsignedChar) Character;
").

char.to_int(C) = N :-
    char.to_int(C, N).

:- pragma foreign_proc("C",
    char.to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = ((MR_UnsignedChar) Character == Int);
").

:- pragma foreign_proc("C",
    char.to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Character = Int;
    SUCCESS_INDICATOR = (Character >= 0 && Character <= 0x10ffff);
").

:- pragma foreign_proc("C#",
    char.to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = Character;
").

:- pragma foreign_proc("C#",
    char.to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Character == Int);
").

:- pragma foreign_proc("C#",
    char.to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = Int;
    SUCCESS_INDICATOR = (Int >= 0 && Int <= 0x10ffff);
").

:- pragma foreign_proc("Java",
    char.to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = (int) Character;
").

:- pragma foreign_proc("Java",
    char.to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((int) Character == Int);
").

:- pragma foreign_proc("Java",
    char.to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = Int;
    SUCCESS_INDICATOR = (Int >= 0 && Int <= 0x10ffff);
").

:- pragma foreign_proc("Erlang",
    char.to_int(Character::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Int = Character
").

:- pragma foreign_proc("Erlang",
    char.to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Character =:= Int)
").

:- pragma foreign_proc("Erlang",
    char.to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = Int,
    SUCCESS_INDICATOR = (Int >= 0 andalso Int =< 16#10ffff)
").

char.min_char_value = N :-
    char.min_char_value(N).

    % We used unsigned character codes, so the minimum character code
    % is always zero.
char.min_char_value(0).

:- pragma foreign_decl("C", "#include <limits.h>").

char.max_char_value = N :-
    char.max_char_value(N).

:- pragma foreign_proc("C",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Max = 0x10ffff;
").
:- pragma foreign_proc("C#",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = 0x10ffff;
").
:- pragma foreign_proc("Java",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = 0x10ffff;
").
:- pragma foreign_proc("Erlang",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = 16#10ffff
").

char.to_utf8(Char, CodeUnits) :-
    Int = char.to_int(Char),
    ( Int =< 0x7f ->
        CodeUnits = [Int]
    ; Int =< 0x7ff ->
        A = 0xc0 \/ ((Int >> 6) /\ 0x1f),
        B = 0x80 \/  (Int       /\ 0x3f),
        CodeUnits = [A, B]
    ; Int =< 0xffff ->
        not is_surrogate(Char),
        A = 0xe0 \/ ((Int >> 12) /\ 0x0f),
        B = 0x80 \/ ((Int >>  6) /\ 0x3f),
        C = 0x80 \/  (Int        /\ 0x3f),
        CodeUnits = [A, B, C]
    ; Int =< 0x10ffff ->
        A = 0xf0 \/ ((Int >> 18) /\ 0x07),
        B = 0x80 \/ ((Int >> 12) /\ 0x3f),
        C = 0x80 \/ ((Int >>  6) /\ 0x3f),
        D = 0x80 \/  (Int        /\ 0x3f),
        CodeUnits = [A, B, C, D]
    ;
        % Illegal code point.
        fail
    ).

char.to_utf16(Char, CodeUnits) :-
    Int = char.to_int(Char),
    ( Int < 0xd800 ->
        % Common case.
        CodeUnits = [Int]
    ; Int =< 0xdfff ->
        % Surrogate.
        fail
    ; Int =< 0xffff ->
        CodeUnits = [Int]
    ; Int =< 0x10ffff ->
        U = Int - 0x10000,
        A = 0xd800 \/ (U >> 10),
        B = 0xdc00 \/ (U /\ 0x3ff),
        CodeUnits = [A, B]
    ;
        % Illegal code point.
        fail
    ).

char.is_surrogate(Char) :-
    Int = char.to_int(Char),
    Int >= 0xd800,
    Int =< 0xdfff.

char.is_noncharacter(Char) :-
    Int = char.to_int(Char),
    ( 0xfdd0 =< Int, Int =< 0xfdef
    ; Int /\ 0xfffe = 0xfffe
    ).

char.char_to_doc(C) = str(term_io.quoted_char(C)).

%-----------------------------------------------------------------------------%
:- end_module char.
%-----------------------------------------------------------------------------%
