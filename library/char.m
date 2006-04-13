%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: char.m.
% Main author: fjh.
% Stability: high.

% This module defines some predicates that manipulate characters.

% The set of characters which are supported and the mapping from
% characters to integer values are both implementation-dependent.

% Originally we used `character' rather than `char' for the type name
% because `char' was used by NU-Prolog to mean something different.
% But now we use `char' and the use of `character' is discouraged.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module char.
:- interface.
:- import_module enum.

%-----------------------------------------------------------------------------%

:- type char == character.

:- instance enum(character).

    % Convert a character to its corresponding numerical code (integer value).
    % Beware that the mapping from characters to numerical codes is
    % implementation-dependent; there is no guarantee that the integer values
    % for characters will fit in 8 bits. Furthermore, the value returned from
    % char.to_int might be different than the byte(s) used to store the
    % character in a file. There is also no guarantee that characters created
    % using `char.to_int(out, in)' can be written to files or to the standard
    % output or standard error streams. For example, an implementation might
    % represent characters using Unicode, but store files in an 8-bit national
    % character set.
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
    %
:- func char.to_upper(char) = char.
:- pred char.to_upper(char::in, char::out) is det.

    % Convert a character to lowercase.
    %
:- func char.to_lower(char) = char.
:- pred char.to_lower(char::in, char::out) is det.

    % char.lower_upper(Lower, Upper) is true iff
    % Lower is a lower-case letter and Upper is the corresponding
    % upper-case letter.
    %
:- pred char.lower_upper(char, char).
:- mode char.lower_upper(in, out) is semidet.
:- mode char.lower_upper(out, in) is semidet.

    % True iff the character is whitespace, i.e. a space, tab,
    % newline, carriage return, form-feed, or vertical tab.
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- instance enum(character) where [
    (to_int(X) = Y :- char.to_int(X, Y)),
    (from_int(X) = Y :- char.to_int(Y, X))
].

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
    % (this predicate is part of the inner loop of the lexer.)
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

char.to_lower(Char, Lower) :-
    ( char.lower_upper(LowerChar, Char) ->
        Lower = LowerChar
    ;
        Lower = Char
    ).

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

char.is_hex_digit('0').
char.is_hex_digit('1').
char.is_hex_digit('2').
char.is_hex_digit('3').
char.is_hex_digit('4').
char.is_hex_digit('5').
char.is_hex_digit('6').
char.is_hex_digit('7').
char.is_hex_digit('8').
char.is_hex_digit('9').
char.is_hex_digit('a').
char.is_hex_digit('b').
char.is_hex_digit('c').
char.is_hex_digit('d').
char.is_hex_digit('e').
char.is_hex_digit('f').
char.is_hex_digit('A').
char.is_hex_digit('B').
char.is_hex_digit('C').
char.is_hex_digit('D').
char.is_hex_digit('E').
char.is_hex_digit('F').

%-----------------------------------------------------------------------------%

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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Int = (MR_UnsignedChar) Character;
").

:- pragma foreign_proc("C",
    char.to_int(Character::in, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = ((MR_UnsignedChar) Character == Int);
").

:- pragma foreign_proc("C",
    char.to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /*
    ** If the integer doesn't fit into a char, then the assignment
    ** `Character = Int' below will truncate it. SUCCESS_INDICATOR will be set
    ** to true only if the result was not truncated.
    */
    Character = Int;
    SUCCESS_INDICATOR = ((MR_UnsignedChar) Character == Int);
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
    Character = (char) Int;
    SUCCESS_INDICATOR = (Character == Int);
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
    succeeded = ((int) Character == Int);
").

:- pragma foreign_proc("Java",
    char.to_int(Character::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Character = (char) Int;
    succeeded = ((int) Character == Int);
").

    % We used unsigned character codes, so the minimum character code
    % is always zero.
char.min_char_value(0).

:- pragma foreign_decl("C", "#include <limits.h>").

:- pragma foreign_proc("C",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = UCHAR_MAX;
").
:- pragma foreign_proc("C#",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // .NET uses 16-bit 'Unicode'. This might be either UCS-2,
    // where Unicode characters that don't fit in 16 bits are encoded
    // in two 16 bit characters, or it might be just the 16-bit subset,
    // i.e. only the Unicode characters that fit in 16 bits.
    // For our purposes, it doesn't matter.
    Max = 0xffff;
").
:- pragma foreign_proc("Java",
    char.max_char_value(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = (int) java.lang.Character.MAX_VALUE;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

char.to_int(C) = N :-
    char.to_int(C, N).

char.max_char_value = N :-
    char.max_char_value(N).

char.min_char_value = N :-
    char.min_char_value(N).

char.to_upper(C1) = C2 :-
    char.to_upper(C1, C2).

char.to_lower(C1) = C2 :-
    char.to_lower(C1, C2).

char.det_int_to_digit(N) = C :-
    char.det_int_to_digit(N, C).
