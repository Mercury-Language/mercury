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

:- pred char_to_string(char, string).
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

:- end_module string.

%-----------------------------------------------------------------------------%
