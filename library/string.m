%-----------------------------------------------------------------------------%

:- module string.
	% Beware that char_to_string/2 won't work with NU-Prolog 1.5.33 because
	% of a NU-Prolog bug (fixed in 1.5.35).

% Main author: fjh.

% This modules provides basic string handling facilities.

%-----------------------------------------------------------------------------%

:- interface.

:- pred string__append(string, string, string).
:- mode string__append(input, input, output).
:- mode string__append(output, output, input).

	% append two strings together

:- pred char_to_string(character, string).
:- mode char_to_string(input, output).
:- mode char_to_string(output, input).

%	char_to_string(Char, String).
%		Converts a character (single-character atom) to a string
%		(or vice versa).

:- pred string__int_to_str(int, string).
:- mode string__int_to_str(input, output).

%	Convert an integer to a string.

%-----------------------------------------------------------------------------%

:- implementation.

:- type string == list(integer).

string__append(A, B, C) :-
	append(A, B, C).

:- char_to_string(Char,String) when Char or String.
char_to_string(Char,String) :-
	length(String,1),
	name(Char,String).	

string__int_to_str(N, Str) :-
	(if
		N < 0
	then
		N1 is 0 - N,
		string__int_to_str(N1, Str1),
		string__append("-", Str1, Str)
	else if
		N < 10
	then
		digit_to_string(N,Str)
	else
		N10 is N mod 10,
		N1 is N // 10,
		digit_to_string(N10, Digit),
		string__int_to_str(N1, Str1),
		string__append(Str1, Digit, Str)
	).

% Simple-minded, but extremely portable.

:- pred digit_to_string(int, string).
:- mode digit_to_string(input, output).

digit_to_string(0, "0").
digit_to_string(1, "1").
digit_to_string(2, "2").
digit_to_string(3, "3").
digit_to_string(4, "4").
digit_to_string(5, "5").
digit_to_string(6, "6").
digit_to_string(7, "7").
digit_to_string(8, "8").
digit_to_string(9, "9").

:- pred string__first_char(string, character, string).
:- mode string__first_char(input, output, output).
:- mode string__first_char(output, input, input).

string__first_char([CharCode|String], Char, String) :-
	name(Char, [CharCode]).

:- pred string__capitalize_first(string, string).
:- mode string__capitalize_first(input, output).

string__capitalize_first(S0, S) :-
	string__first_char(S0, C, S1),
	to_upper(C, UpperC),
	string__first_char(S, UpperC, S1).

:- pred string__uncapitalize_first(string, string).
:- mode string__uncapitalize_first(input, output).

string__uncapitalize_first(S0, S) :-
	string__first_char(S0, C, S1),
	to_lower(C, LowerC),
	string__first_char(S, LowerC, S1).

:- pred to_upper(character, character).
:- mode to_upper(input, output).

to_upper(Char, Upper) :-
	(if some [UpperChar]
		upper_lower(Char, UpperChar)
	then
		Upper = UpperChar
	else
		Upper = Char
	).

:- pred to_lower(character, character).
:- mode to_lower(input, output).

to_lower(Char, Lower) :-
	(if some [LowerChar]
		upper_lower(LowerChar, Char)
	then
		Lower = LowerChar
	else
		Lower = Char
	).

:- pred is_upper(character).
:- mode is_upper(input).

is_upper(Upper) :-
	some [Lower] upper_lower(Lower, Upper).

:- pred is_lower(character).
:- mode is_lower(input).

is_lower(Lower) :-
	some [Upper] upper_lower(Lower, Upper).

:- pred upper_lower(character, character).
:- mode upper_lower(input, output).
:- mode upper_lower(output, input).

upper_lower('a', 'A').
upper_lower('b', 'B').
upper_lower('c', 'C').
upper_lower('d', 'D').
upper_lower('e', 'E').
upper_lower('f', 'F').
upper_lower('g', 'G').
upper_lower('h', 'H').
upper_lower('i', 'I').
upper_lower('j', 'J').
upper_lower('k', 'K').
upper_lower('l', 'L').
upper_lower('m', 'M').
upper_lower('n', 'N').
upper_lower('o', 'O').
upper_lower('p', 'P').
upper_lower('q', 'Q').
upper_lower('r', 'R').
upper_lower('s', 'S').
upper_lower('t', 'T').
upper_lower('u', 'U').
upper_lower('v', 'V').
upper_lower('w', 'W').
upper_lower('x', 'X').
upper_lower('y', 'Y').
upper_lower('z', 'Z').

:- type character == atom.

:- end_module string.

%-----------------------------------------------------------------------------%
