%---------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: char.m.
% Main author: fjh.
% Stability: high.

% This module defines some predicates that manipulate characters.

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

:- func char__to_int(char) = int.
:- pred char__to_int(char, int).
:- mode char__to_int(in, out) is det.
:- mode char__to_int(in, in) is semidet.	% implied
:- mode char__to_int(out, in) is semidet.
	% Convert a character to it's corresponding numerical code.

:- func char__max_char_value = int.
:- pred char__max_char_value(int).
:- mode char__max_char_value(out) is det.
	% Returns the maximum numerical character code.

:- func char__min_char_value = int.
:- pred char__min_char_value(int).
:- mode char__min_char_value(out) is det.
	% Returns the minimum numerical character code.

:- func char__to_upper(char) = char.
:- pred char__to_upper(char, char).
:- mode char__to_upper(in, out) is det.
	% Convert a character to uppercase.

:- func char__to_lower(char) = char.
:- pred char__to_lower(char, char).
:- mode char__to_lower(in, out) is det.
	% Convert a character to lowercase.

:- pred char__lower_upper(char, char).
:- mode char__lower_upper(in, out) is semidet.
:- mode char__lower_upper(out, in) is semidet.
	% char__lower_upper(Lower, Upper) is true iff
	% Lower is a lower-case letter and Upper is the corresponding
	% upper-case letter.

:- pred char__is_whitespace(char).
:- mode char__is_whitespace(in) is semidet.
	% True iff the character is whitespace, i.e. a space, tab,
	% newline, carriage return, form-feed, or vertical tab.

:- pred char__is_upper(char).
:- mode char__is_upper(in) is semidet.
	% True iff the character is an uppercase letter.

:- pred char__is_lower(char).
:- mode char__is_lower(in) is semidet.
	% True iff the character is a lowercase letter.

:- pred char__is_alpha(char).
:- mode char__is_alpha(in) is semidet.
	% True iff the character is a letter.

:- pred char__is_alnum(char).
:- mode char__is_alnum(in) is semidet.
	% True iff the character is a letter or digit.

:- pred char__is_alpha_or_underscore(char).
:- mode char__is_alpha_or_underscore(in) is semidet.
	% True iff the character is a letter or an underscore.

:- pred char__is_alnum_or_underscore(char).
:- mode char__is_alnum_or_underscore(in) is semidet.
	% True iff the character is a letter, a digit or an underscore.

:- pred char__is_digit(char).
:- mode char__is_digit(in) is semidet.
	% True iff the character is a decimal digit (0-9).

:- pred char__is_binary_digit(char).
:- mode char__is_binary_digit(in) is semidet.
	% True iff the character is a binary digit (0 or 1).

:- pred char__is_octal_digit(char).
:- mode char__is_octal_digit(in) is semidet.
	% True iff the character is a octal digit (0-7).

:- pred char__is_hex_digit(char).
:- mode char__is_hex_digit(in) is semidet.
	% True iff the character is a hexadecimal digit (0-9, a-f, A-F).

:- pred char__digit_to_int(char, int).
:- mode char__digit_to_int(in, out) is semidet.
	% Succeeds if char is a decimal digit (0-9) or letter (a-z or A-Z).
	% Returns the character's value as a digit (0-9 or 10-35).

:- pred char__int_to_digit(int, char).
:- mode char__int_to_digit(in, out) is semidet.
:- mode char__int_to_digit(out, in) is semidet.
	% char__int_to_uppercase_digit(Int, DigitChar):
	% True iff `Int' is an integer in the range 0-35 and
	% `DigitChar' is a decimal digit or uppercase letter
	% whose value as a digit is `Int'.

:- func char__det_int_to_digit(int) = char.
:- pred char__det_int_to_digit(int, char).
:- mode char__det_int_to_digit(in, out) is det.
	% Returns a decimal digit or uppercase letter corresponding to the
	% value.
	% Calls error/1 if the integer is not in the range 0-35.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

:- instance enum(character) where [
	(to_int(X) = Y :- char__to_int(X, Y)),
	(from_int(X) = Y :- char__to_int(Y, X))
].

char__is_whitespace(' ').
char__is_whitespace('\t').
char__is_whitespace('\n').
char__is_whitespace('\r').
char__is_whitespace('\f').
char__is_whitespace('\v').

char__is_alpha(Char) :-
	( char__is_lower(Char) ->
		true
	; char__is_upper(Char) ->
		true
	;
		fail
	).

char__is_alnum(Char) :-
	( char__is_alpha(Char) ->
		true
	; char__is_digit(Char) ->
		true
	;
		fail
	).

char__is_alpha_or_underscore(Char) :-
	( Char = '_' ->
		true
	;	
		char__is_alpha(Char)
	).

	% We explicitly enumerate here for efficiency.
	% (this predicate is part of the inner loop of the lexer.)
char__is_alnum_or_underscore(Char) :-
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
% A more consise implementation is:
%	( char__is_digit(Char) ->
%		true
%	;	
%		char__is_alpha_or_underscore(Char)
%	).

char__is_lower(Lower) :-
	char__lower_upper(Lower, _).

char__is_upper(Upper) :-
	(
		char__lower_upper(_, Upper)
	->
		true
	;
		fail
	).

char__to_lower(Char, Lower) :-
	(
		char__lower_upper(LowerChar, Char)
	->
		Lower = LowerChar
	;
		Lower = Char
	).

char__to_upper(Char, Upper) :-
	(
		char__lower_upper(Char, UpperChar)
	->
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

char__is_binary_digit('0').
char__is_binary_digit('1').

char__is_octal_digit('0').
char__is_octal_digit('1').
char__is_octal_digit('2').
char__is_octal_digit('3').
char__is_octal_digit('4').
char__is_octal_digit('5').
char__is_octal_digit('6').
char__is_octal_digit('7').

char__is_digit('0').
char__is_digit('1').
char__is_digit('2').
char__is_digit('3').
char__is_digit('4').
char__is_digit('5').
char__is_digit('6').
char__is_digit('7').
char__is_digit('8').
char__is_digit('9').

char__is_hex_digit('0').
char__is_hex_digit('1').
char__is_hex_digit('2').
char__is_hex_digit('3').
char__is_hex_digit('4').
char__is_hex_digit('5').
char__is_hex_digit('6').
char__is_hex_digit('7').
char__is_hex_digit('8').
char__is_hex_digit('9').
char__is_hex_digit('a').
char__is_hex_digit('b').
char__is_hex_digit('c').
char__is_hex_digit('d').
char__is_hex_digit('e').
char__is_hex_digit('f').
char__is_hex_digit('A').
char__is_hex_digit('B').
char__is_hex_digit('C').
char__is_hex_digit('D').
char__is_hex_digit('E').
char__is_hex_digit('F').

%-----------------------------------------------------------------------------%

char__det_int_to_digit(Int, Digit) :-
	( char__int_to_digit(Int, Digit1) ->
		Digit = Digit1
	;
		error("char__int_to_digit failed")
	).

char__int_to_digit(0, '0').
char__int_to_digit(1, '1').
char__int_to_digit(2, '2').
char__int_to_digit(3, '3').
char__int_to_digit(4, '4').
char__int_to_digit(5, '5').
char__int_to_digit(6, '6').
char__int_to_digit(7, '7').
char__int_to_digit(8, '8').
char__int_to_digit(9, '9').
char__int_to_digit(10, 'A').
char__int_to_digit(11, 'B').
char__int_to_digit(12, 'C').
char__int_to_digit(13, 'D').
char__int_to_digit(14, 'E').
char__int_to_digit(15, 'F').
char__int_to_digit(16, 'G').
char__int_to_digit(17, 'H').
char__int_to_digit(18, 'I').
char__int_to_digit(19, 'J').
char__int_to_digit(20, 'K').
char__int_to_digit(21, 'L').
char__int_to_digit(22, 'M').
char__int_to_digit(23, 'N').
char__int_to_digit(24, 'O').
char__int_to_digit(25, 'P').
char__int_to_digit(26, 'Q').
char__int_to_digit(27, 'R').
char__int_to_digit(28, 'S').
char__int_to_digit(29, 'T').
char__int_to_digit(30, 'U').
char__int_to_digit(31, 'V').
char__int_to_digit(32, 'W').
char__int_to_digit(33, 'X').
char__int_to_digit(34, 'Y').
char__int_to_digit(35, 'Z').

char__digit_to_int(Digit, Int) :-
	( char__lower_upper(Digit, Upper) ->
		char__int_to_digit(Int, Upper)
	;
		char__int_to_digit(Int, Digit)
	).

%-----------------------------------------------------------------------------%

char__lower_upper('a', 'A').
char__lower_upper('b', 'B').
char__lower_upper('c', 'C').
char__lower_upper('d', 'D').
char__lower_upper('e', 'E').
char__lower_upper('f', 'F').
char__lower_upper('g', 'G').
char__lower_upper('h', 'H').
char__lower_upper('i', 'I').
char__lower_upper('j', 'J').
char__lower_upper('k', 'K').
char__lower_upper('l', 'L').
char__lower_upper('m', 'M').
char__lower_upper('n', 'N').
char__lower_upper('o', 'O').
char__lower_upper('p', 'P').
char__lower_upper('q', 'Q').
char__lower_upper('r', 'R').
char__lower_upper('s', 'S').
char__lower_upper('t', 'T').
char__lower_upper('u', 'U').
char__lower_upper('v', 'V').
char__lower_upper('w', 'W').
char__lower_upper('x', 'X').
char__lower_upper('y', 'Y').
char__lower_upper('z', 'Z').

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	char__to_int(Character::in, Int::out),
               [will_not_call_mercury, promise_pure, thread_safe] , "
	Int = (MR_UnsignedChar) Character;
").

:- pragma foreign_proc("C",
	char__to_int(Character::in, Int::in),
               [will_not_call_mercury, promise_pure, thread_safe] , "
	SUCCESS_INDICATOR = ((MR_UnsignedChar) Character == Int);
").

:- pragma foreign_proc("C",
	char__to_int(Character::out, Int::in),
               [will_not_call_mercury, promise_pure, thread_safe] , "
	/*
	** If the integer doesn't fit into a char, then
	** the assignment `Character = Int' below will truncate it.
	** SUCCESS_INDICATOR will be set to true only if
	** the result was not truncated.
	*/
	Character = Int;
	SUCCESS_INDICATOR = ((MR_UnsignedChar) Character == Int);
").

:- pragma foreign_proc("MC++",
	char__to_int(Character::in, Int::out),
               [will_not_call_mercury, promise_pure, thread_safe] , "
	Int = Character;
").

:- pragma foreign_proc("MC++",
	char__to_int(Character::in, Int::in),
               [will_not_call_mercury, promise_pure, thread_safe] , "
	SUCCESS_INDICATOR = (Character == Int);
").

:- pragma foreign_proc("MC++",
	char__to_int(Character::out, Int::in),
               [will_not_call_mercury, promise_pure, thread_safe] , "
	Character = Int;
	SUCCESS_INDICATOR = (Character == Int);
").

:- pragma promise_pure(char__to_int/2).
char__to_int(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("char__to_int").


% We used unsigned character codes, so the minimum character code
% is always zero.

char__min_char_value(0).

:- pragma foreign_decl("C", "#include <limits.h>").
:- pragma foreign_proc("C",
		char__max_char_value(Max::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	Max = UCHAR_MAX;
").

char__max_char_value(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("char__max_char_value").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%       Functional forms added.

char__to_int(C) = N :-
	char__to_int(C, N).

char__max_char_value = N :-
	char__max_char_value(N).

char__min_char_value = N :-
	char__min_char_value(N).

char__to_upper(C1) = C2 :-
	char__to_upper(C1, C2).

char__to_lower(C1) = C2 :-
	char__to_lower(C1, C2).

char__det_int_to_digit(N) = C :-
	char__det_int_to_digit(N, C).

