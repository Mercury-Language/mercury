%-----------------------------------------------------------------------------%

:- module string.
	% Beware that char_to_string/2 won't work with NU-Prolog 1.5.33 because
	% of a NU-Prolog bug (fixed in 1.5.35).

% Main author: fjh.

% This modules provides basic string handling facilities.

% XXX The efficiency of many of these operations is very poor with
%     the current implementation.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module char.

:- pred string__length(string, int).
:- mode string__length(in, out).

:- pred string__append(string, string, string).
:- mode string__append(in, in, out).
:- mode string__append(out, out, in).
	% append two strings together

:- pred string__prefix(string, string).
:- mode string__prefix(in, in).
:- mode string__prefix(in, out).
	% string__prefix(String, Prefix) is true iff Prefix is a
	% prefix of String

:- pred string__char_to_string(character, string).
:- mode string__char_to_string(in, out).
:- mode string__char_to_string(out, in).
%	string__char_to_string(Char, String).
%		Converts a character (single-character atom) to a string
%		or vice versa.

:- pred string__int_to_string(int, string).
:- mode string__int_to_string(in, out).
%	Convert an integer to a string.

:- pred string__int_to_base_string(int, int, string).
:- mode string__int_to_base_string(in, in, out).
%	string__int_to_base_string(Int, Base, String):
%	Convert an integer to a string in a given Base (between 2 and 36).

:- pred string__first_char(string, character, string).
:- mode string__first_char(in, out, out).
:- mode string__first_char(out, in, in).
%	string__first_char(String, Char, Rest) is true iff
%		Char is the first character of String, and Rest is the
%		remainder.

:- pred string__capitalize_first(string, string).
:- mode string__capitalize_first(in, out).
%	Convert the first character (if any) of a string to uppercase.

:- pred string__uncapitalize_first(string, string).
:- mode string__uncapitalize_first(in, out).
%	Convert the first character (if any) of a string to lowercase.

:- pred string__to_char_list(string, list(character)).
:- mode string__to_char_list(in, out).
:- mode string__to_char_list(out, in).

:- pred string__to_int(string, int).
:- mode string__to_int(in, out).
%	Convert a string (of digits) to an int. If the string contains
%	non-digit characters, string__to_int fails.

:- pred string__is_alpha(string).
:- mode string__is_alpha(in).
	% True if string contains only alphabetic characters (letters).

:- pred string__is_alpha_or_underscore(string).
:- mode string__is_alpha_or_underscore(in).
	% True if string contains only alphabetic characters and underscores.

:- pred string__is_alnum_or_underscore(string).
:- mode string__is_alnum_or_underscore(in).
	% True if string contains only letters, digits, and underscores.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, int.

:- pred string__to_int_list(string, list(int)).
:- mode string__to_int_list(in, out).
:- mode string__to_int_list(out, in).

:- pred intToString(int, string).
:- mode intToString(out, in).

/*
:- external("NU-Prolog", string__to_int_list).
:- external("NU-Prolog", intToString).
*/

string__to_int(String, Int) :-
	intToString(Int, String).

string__length(String, Length) :-
	string__to_int_list(String, List),
	length(List, Length).

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
	string__int_to_base_string(N, 10, Str).

string__int_to_base_string(N, Base, Str) :-
	Base >= 2, Base =< 36,
	(
		N < 0
	->
		N1 is 0 - N,
		string__int_to_base_string_2(N1, Base, Str1),
		string__append("-", Str1, Str)
	;
		string__int_to_base_string_2(N, Base, Str)
	).

:- pred string__int_to_base_string_2(int, int, string).
:- mode string__int_to_base_string_2(in, in, out) is det.

string__int_to_base_string_2(N, Base, Str) :-
	(
		N < Base
	->
		digit_to_string(N,Str)
	;
		N10 is N mod Base,
		N1 is N // Base,
		( digit_to_string(N10, Digit0) ->
			Digit = Digit0
		;
			error(
			"string__int_to_base_string_2: digit_to_string failed")
		),
		string__int_to_base_string_2(N1, Base, Str1),
		string__append(Str1, Digit, Str)
	).

% Simple-minded, but extremely portable.

:- pred digit_to_string(int, string).
:- mode digit_to_string(in, out) is semidet.

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
digit_to_string(10, "A").
digit_to_string(11, "B").
digit_to_string(12, "C").
digit_to_string(13, "D").
digit_to_string(14, "E").
digit_to_string(15, "F").
digit_to_string(16, "G").
digit_to_string(17, "H").
digit_to_string(18, "I").
digit_to_string(19, "J").
digit_to_string(20, "K").
digit_to_string(21, "L").
digit_to_string(22, "M").
digit_to_string(23, "N").
digit_to_string(24, "O").
digit_to_string(25, "P").
digit_to_string(26, "Q").
digit_to_string(27, "R").
digit_to_string(28, "S").
digit_to_string(29, "T").
digit_to_string(30, "U").
digit_to_string(31, "V").
digit_to_string(32, "W").
digit_to_string(33, "X").
digit_to_string(34, "Y").
digit_to_string(35, "Z").

string__first_char(String0, Char, String) :-
	string__to_int_list(String0, List0),
	List0 = [CharCode | List],
	string__to_int_list(String, List),
	char_to_int(Char, CharCode).

string__to_char_list(String, CharList) :-
	string__to_int_list(String, IntList),
	string__int_list_to_char_list(IntList, CharList).

:- pred string__int_list_to_char_list(list(int), list(character)).
:- mode string__int_list_to_char_list(in, out).
:- mode string__int_list_to_char_list(out, in).

string__int_list_to_char_list([], []).
string__int_list_to_char_list([Code | Codes], [Char | Chars]) :-
	char_to_int(Char, Code),
	string__int_list_to_char_list(Codes, Chars).

string__capitalize_first(S0, S) :-
	string__first_char(S0, C, S1),
	to_upper(C, UpperC),
	string__first_char(S, UpperC, S1).

string__uncapitalize_first(S0, S) :-
	string__first_char(S0, C, S1),
	to_lower(C, LowerC),
	string__first_char(S, LowerC, S1).

string__is_alpha(S) :-
	( string__first_char(S, C, S1) ->
		is_alpha(C),
		string__is_alpha(S1)
	;
		true
	).

string__is_alpha_or_underscore(S) :-
	( string__first_char(S, C, S1) ->
		is_alpha_or_underscore(C),
		string__is_alpha_or_underscore(S1)
	;
		true
	).

string__is_alnum_or_underscore(S) :-
	( string__first_char(S, C, S1) ->
		is_alnum_or_underscore(C),
		string__is_alnum_or_underscore(S1)
	;
		true
	).

:- end_module string.

%-----------------------------------------------------------------------------%
