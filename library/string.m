%-----------------------------------------------------------------------------%

:- module string.
	% Beware that char_to_string/2 won't work with NU-Prolog 1.5.33 because
	% of a NU-Prolog bug (fixed in 1.5.35).

% Main author: fjh.

% This modules provides basic string handling facilities.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module char.

:- pred string__append(string, string, string).
:- mode string__append(input, input, output).
:- mode string__append(output, output, input).
	% append two strings together

:- pred string__prefix(string, string).
:- mode string__prefix(input, input).
:- mode string__prefix(input, output).
	% string__prefix(String, Prefix) is true iff Prefix is a
	% prefix of String

:- pred string__char_to_string(character, string).
:- mode string__char_to_string(input, output).
:- mode string__char_to_string(output, input).
%	string__char_to_string(Char, String).
%		Converts a character (single-character atom) to a string
%		or vice versa.

:- pred string__int_to_string(int, string).
:- mode string__int_to_string(input, output).
%	Convert an integer to a string.

:- pred string__first_char(string, character, string).
:- mode string__first_char(input, output, output).
:- mode string__first_char(output, input, input).
%	string__first_char(String, Char, Rest) is true iff
%		Char is the first character of String, and Rest is the
%		remainder.

:- pred string__capitalize_first(string, string).
:- mode string__capitalize_first(input, output).
%	Convert the first character (if any) of a string to uppercase.

:- pred string__uncapitalize_first(string, string).
:- mode string__uncapitalize_first(input, output).
%	Convert the first character (if any) of a string to lowercase.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, int.

:- pred string__to_int_list(string, list(int)).
:- mode string__to_int_list(input, output).
:- mode string__to_int_list(output, input).

/*
:- external("NU-Prolog", string__to_int_list).
*/

string__append(A, B, C) :-
	string__to_int_list(A, LA),
	string__to_int_list(B, LB),
	string__to_int_list(C, LC),
	append(LA, LB, LC).

string__prefix(String, Prefix) :-
	string__append(Prefix, _, String).

:- string__char_to_string(Char,String) when Char or String.
string__char_to_string(Char, String) :-
	string__to_int_list(String, [Code]),
	char_to_int(Char, Code).

string__int_to_string(N, Str) :-
	(if
		N < 0
	then
		N1 is 0 - N,
		string__int_to_string(N1, Str1),
		string__append("-", Str1, Str)
	else if
		N < 10
	then
		digit_to_string(N,Str)
	else
		N10 is N mod 10,
		N1 is N // 10,
		digit_to_string(N10, Digit),
		string__int_to_string(N1, Str1),
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

string__first_char(String0, Char, String) :-
	string__to_int_list(String0, List0),
	List0 = [CharCode | List],
	string__to_int_list(String, List),
	char_to_int(Char, CharCode).

string__capitalize_first(S0, S) :-
	string__first_char(S0, C, S1),
	to_upper(C, UpperC),
	string__first_char(S, UpperC, S1).

string__uncapitalize_first(S0, S) :-
	string__first_char(S0, C, S1),
	to_lower(C, LowerC),
	string__first_char(S, LowerC, S1).

:- end_module string.

%-----------------------------------------------------------------------------%
