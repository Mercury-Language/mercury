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

:- pred string__pad_left(string, character, int, string).
:- mode string__pad_left(in, in, in, out) is det.
%	string__pad_left(String0, PadChar, Width, String):
%	insert `PadChar's at the left of `String0' until it is at least
%	as long as `Width', giving `String'.

:- pred string__pad_right(string, character, int, string).
:- mode string__pad_right(in, in, in, out) is det.
%	string__pad_right(String0, PadChar, Width, String):
%	insert `PadChar's at the right of `String0' until it is at least
%	as long as `Width', giving `String'.

:- pred string__duplicate_char(character, int, string).
:- mode string__duplicate_char(in, in, out) is det.
%	string__duplicate_char(Char, Count, String):
%	construct a string consisting of `Count' occurrences of `Char'
%	in sequence.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, int, require.

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

:- string__char_to_string(Char, String) when Char or String.
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
		string__digit_to_string(N,Str)
	;
		N10 is N mod Base,
		N1 is N // Base,
		( string__digit_to_string(N10, Digit0) ->
			Digit = Digit0
		;
			error("string__int_to_base_string_2: string__digit_to_string failed")
		),
		string__int_to_base_string_2(N1, Base, Str1),
		string__append(Str1, Digit, Str)
	).

% Simple-minded, but extremely portable.

:- pred string__digit_to_string(int, string).
:- mode string__digit_to_string(in, out) is semidet.

string__digit_to_string(0, "0").
string__digit_to_string(1, "1").
string__digit_to_string(2, "2").
string__digit_to_string(3, "3").
string__digit_to_string(4, "4").
string__digit_to_string(5, "5").
string__digit_to_string(6, "6").
string__digit_to_string(7, "7").
string__digit_to_string(8, "8").
string__digit_to_string(9, "9").
string__digit_to_string(10, "A").
string__digit_to_string(11, "B").
string__digit_to_string(12, "C").
string__digit_to_string(13, "D").
string__digit_to_string(14, "E").
string__digit_to_string(15, "F").
string__digit_to_string(16, "G").
string__digit_to_string(17, "H").
string__digit_to_string(18, "I").
string__digit_to_string(19, "J").
string__digit_to_string(20, "K").
string__digit_to_string(21, "L").
string__digit_to_string(22, "M").
string__digit_to_string(23, "N").
string__digit_to_string(24, "O").
string__digit_to_string(25, "P").
string__digit_to_string(26, "Q").
string__digit_to_string(27, "R").
string__digit_to_string(28, "S").
string__digit_to_string(29, "T").
string__digit_to_string(30, "U").
string__digit_to_string(31, "V").
string__digit_to_string(32, "W").
string__digit_to_string(33, "X").
string__digit_to_string(34, "Y").
string__digit_to_string(35, "Z").

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

string__pad_left(String0, PadChar, Width, String) :-
	string__length(String0, Length),
	( Length < Width ->
		Count is Width - Length,
		string__duplicate_char(PadChar, Count, PadString),
		string__append(PadString, String0, String)
	;
		String = String0
	).

string__pad_right(String0, PadChar, Width, String) :-
	string__length(String0, Length),
	( Length < Width ->
		Count is Width - Length,
		string__duplicate_char(PadChar, Count, PadString),
		string__append(String0, PadString, String)
	;
		String = String0
	).

string__duplicate_char(Char, Count, String) :-
	( Count = 0 ->
		String = ""
	;
		Count1 is Count - 1,
		string__first_char(String, Char, String1),
		string__duplicate_char(Char, Count1, String1)
	).

:- end_module string.

%-----------------------------------------------------------------------------%
