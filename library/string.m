%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module string.
	% Beware that char_to_string/2 won't work with NU-Prolog 1.5.33 because
	% of a NU-Prolog bug (fixed in 1.5.35).

% Main author: fjh.

% This modules provides basic string handling facilities.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module char, int, float.

:- pred string__length(string, int).
:- mode string__length(in, out) is det.

:- pred string__append(string, string, string).
:- mode string__append(in, in, out) is det.
:- mode string__append(in, in, in) is semidet.	% implied
:- mode string__append(in, out, in) is semidet.
:- mode string__append(out, out, in) is multidet.
%	Append two strings together.
%
%       The following mode is semidet in the sense that it doesn't
%       succeed more than once - but it does create a choice-point,
%       which means it's inefficient and that the compiler can't deduce
%       that it is semidet.  Use string__remove_suffix instead.
% :- mode string__append(out, in, in) is semidet.

:- pred string__remove_suffix(string, string, string).
:- mode string__remove_suffix(in, in, out) is semidet.
%	string__remove_suffix(String, Suffix, Prefix):
%       The same as string__append(Prefix, Suffix, List) except that
%       this is semidet whereas string__append(out, in, in) is nondet.

:- pred string__prefix(string, string).
:- mode string__prefix(in, in) is semidet.
:- mode string__prefix(in, out) is multidet.
	% string__prefix(String, Prefix) is true iff Prefix is a
	% prefix of String.  Same as string__append(Prefix, _, String).

:- pred string__char_to_string(character, string).
:- mode string__char_to_string(in, out) is det.
:- mode string__char_to_string(out, in) is semidet.
%	string__char_to_string(Char, String).
%		Converts a character (single-character atom) to a string
%		or vice versa.

:- pred string__int_to_string(int, string).
:- mode string__int_to_string(in, out) is det.
%	Convert an integer to a string.

:- pred string__int_to_base_string(int, int, string).
:- mode string__int_to_base_string(in, in, out) is det.
%	string__int_to_base_string(Int, Base, String):
%	Convert an integer to a string in a given Base (between 2 and 36).

:- pred string__float_to_string(float, string).
:- mode string__float_to_string(in, out) is det.
%	Convert an float to a string.

:- pred string__first_char(string, character, string).
:- mode string__first_char(in, in, in) is semidet.	% implied
:- mode string__first_char(in, out, in) is semidet.	% implied
:- mode string__first_char(in, in, out) is semidet.	% implied
:- mode string__first_char(in, out, out) is semidet.
:- mode string__first_char(out, in, in) is det.
%	string__first_char(String, Char, Rest) is true iff
%		Char is the first character of String, and Rest is the
%		remainder.

:- pred string__to_upper(string, string).
:- mode string__to_upper(in, out) is det.
:- mode string__to_upper(in, in) is semidet.		% implied
%	Converts a string to uppercase.

:- pred string__capitalize_first(string, string).
:- mode string__capitalize_first(in, out) is det.
%	Convert the first character (if any) of a string to uppercase.

:- pred string__uncapitalize_first(string, string).
:- mode string__uncapitalize_first(in, out) is det.
%	Convert the first character (if any) of a string to lowercase.

:- pred string__to_char_list(string, list(character)).
:- mode string__to_char_list(in, out) is det.

:- pred string__from_char_list(list(character), string).
:- mode string__from_char_list(in, out) is det.

:- pred string__to_int(string, int).
:- mode string__to_int(in, out) is semidet.
%	Convert a string (of digits) to an int. If the string contains
%	non-digit characters, string__to_int fails.

:- pred string__base_string_to_int(int, string, int).
:- mode string__base_string_to_int(in, in, out) is semidet.
%	Convert a string of digits in the specified base (2-36) to an int.
%	If the string contains invalid characters, the predicate fails.

:- pred string__to_float(string, float).
:- mode string__to_float(in, out) is semidet.
%	Convert a string to an float. If the string is not
%	a syntactically correct float literal, string__to_float fails.

:- pred string__is_alpha(string).
:- mode string__is_alpha(in) is semidet.
	% True if string contains only alphabetic characters (letters).

:- pred string__is_alpha_or_underscore(string).
:- mode string__is_alpha_or_underscore(in) is semidet.
	% True if string contains only alphabetic characters and underscores.

:- pred string__is_alnum_or_underscore(string).
:- mode string__is_alnum_or_underscore(in) is semidet.
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

:- pred string__index(string, int, character).
:- mode string__index(in, in, out) is semidet.
%	string__index(String, Index, Char):
%	`Char' is the (`Index' + 1)-th character of `String'.
%	Fails if `Index' is out of range (negative, or greater than or
%	equal to the length of `String').

:- pred string__index_det(string, int, character).
:- mode string__index_det(in, in, out) is det.
%	string__index_det(String, Index, Char):
%	`Char' is the (`Index' + 1)-th character of `String'.
%	Calls error/1 if `Index' is out of range (negative, or greater than or
%	equal to the length of `String').

:- pred string__split(string, int, string, string).
:- mode string__split(in, in, out, out) is det.
%	string__split(String, Count, LeftSubstring, RightSubstring):
%	`LeftSubstring' is the left-most `Count' characters of `String',
%	and `RightSubstring' is the remainder of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)

:- pred string__left(string, int, string).
:- mode string__left(in, in, out) is det.
%	string__left(String, Count, LeftSubstring):
%	`LeftSubstring' is the left-most `Count' characters of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)

:- pred string__right(string, int, string).
:- mode string__right(in, in, out) is det.
%	string__right(String, Count, RightSubstring):
%	`RightSubstring' is what would remain of `String' if the
%	left-most `Count' characters were removed.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)

:- pred string__substring(string, int, int, string).
:- mode string__substring(in, in, in, out) is det.
%	string__substring(String, Start, Count, Substring):
%	`Substring' is first the `Count' characters in what would
%	remain of `String' after the first `Start' characters were
%	removed.
%	(If `Start' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.
%	If `Count' is out of the range [0, length of `String' - `Start'], it is
%	treated as if it were the nearest end-point of that range.)

:- pred string__append_list(list(string), string).
:- mode string__append_list(in, out) is det.
:- mode string__append_list(out, in) is nondet.
%	Append a list of strings together.

:- pred string__hash(string, int).
:- mode string__hash(in, out) is det.
%	Compute a hash value for a string.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, int, require.

:- pred string__to_int_list(string, list(int)).
:- mode string__to_int_list(in, out) is det.
:- mode string__to_int_list(out, in) is det.
:- mode string__to_int_list(in, in) is semidet. % implied

:- external(string__float_to_string/2).
:- external(string__to_float/2).
:- external(string__to_int_list/2).
:- external(string__index/3).
:- external(string__length/2).
:- external(string__append/3).
:- external(string__split/4).
:- external(string__first_char/3).

string__to_int(String, Int) :-
	string__base_string_to_int(10, String, Int).

string__base_string_to_int(Base, String, Int) :-
	string__base_string_to_int_2(Base, String, 0, Int).

:- pred string__base_string_to_int_2(int, string, int, int).
:- mode string__base_string_to_int_2(in, in, in, out) is semidet.
	
string__base_string_to_int_2(Base, String, Int0, Int) :-
	( string__first_char(String, Char, String1) ->
		char__to_upper(Char, UpperChar),
		string__digit_to_char(Digit, UpperChar),
		Int1 is Base * Int0,
		Int2 is Int1 + Digit,
		string__base_string_to_int_2(Base, String1, Int2, Int) 
	;
		Int = Int0
	).

string__index_det(String, Int, Char) :-
	( string__index(String, Int, Char0) ->
		Char = Char0
	;
		error("string__index_det: index out of range")
	).

string__left(String, Count, LeftString) :-
	string__split(String, Count, LeftString, _RightString).

string__right(String, RightCount, RightString) :-
	string__length(String, Length),
	LeftCount is Length - RightCount,
	string__split(String, LeftCount, _LeftString, RightString).

string__substring(String, Start, Count, Substring) :-
	string__right(String, Start, Right),
	string__left(Right, Count, Substring).

string__remove_suffix(A, B, C) :-
	string__to_int_list(A, LA),
	string__to_int_list(B, LB),
	string__to_int_list(C, LC),
	list__remove_suffix(LA, LB, LC).

string__prefix(String, Prefix) :-
	string__append(Prefix, _, String).

:- string__char_to_string(Char, String) when Char or String.
string__char_to_string(Char, String) :-
	string__to_int_list(String, [Code]),
	char_to_int(Char, Code).

string__int_to_string(N, Str) :-
	string__int_to_base_string(N, 10, Str).

string__int_to_base_string(N, Base, Str) :-
	(
		Base >= 2, Base =< 36
	->
		true
	;
		error("string__int_to_base_string: invalid base")
	),
	string__int_to_base_string_1(N, Base, Str).

:- pred string__int_to_base_string_1(int, int, string).
:- mode string__int_to_base_string_1(in, in, out) is det.

string__int_to_base_string_1(N, Base, Str) :-
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
		string__digit_to_char_det(N, DigitChar),
		string__char_to_string(DigitChar, Str)
	;
		N10 is N mod Base,
		N1 is N // Base,
		string__digit_to_char_det(N10, DigitChar),
		string__char_to_string(DigitChar, DigitString),
		string__int_to_base_string_2(N1, Base, Str1),
		string__append(Str1, DigitString, Str)
	).

:- pred string__digit_to_char_det(int, character).
:- mode string__digit_to_char_det(in, out) is det.

string__digit_to_char_det(Digit, Char) :-
	( string__digit_to_char(Digit, Char0) ->
		Char = Char0
	;
		error("string__digit_to_char failed")
	).

% Simple-minded, but extremely portable.

:- pred string__digit_to_char(int, character).
:- mode string__digit_to_char(in, out) is semidet.
:- mode string__digit_to_char(out, in) is semidet.

string__digit_to_char(0, '0').
string__digit_to_char(1, '1').
string__digit_to_char(2, '2').
string__digit_to_char(3, '3').
string__digit_to_char(4, '4').
string__digit_to_char(5, '5').
string__digit_to_char(6, '6').
string__digit_to_char(7, '7').
string__digit_to_char(8, '8').
string__digit_to_char(9, '9').
string__digit_to_char(10, 'A').
string__digit_to_char(11, 'B').
string__digit_to_char(12, 'C').
string__digit_to_char(13, 'D').
string__digit_to_char(14, 'E').
string__digit_to_char(15, 'F').
string__digit_to_char(16, 'G').
string__digit_to_char(17, 'H').
string__digit_to_char(18, 'I').
string__digit_to_char(19, 'J').
string__digit_to_char(20, 'K').
string__digit_to_char(21, 'L').
string__digit_to_char(22, 'M').
string__digit_to_char(23, 'N').
string__digit_to_char(24, 'O').
string__digit_to_char(25, 'P').
string__digit_to_char(26, 'Q').
string__digit_to_char(27, 'R').
string__digit_to_char(28, 'S').
string__digit_to_char(29, 'T').
string__digit_to_char(30, 'U').
string__digit_to_char(31, 'V').
string__digit_to_char(32, 'W').
string__digit_to_char(33, 'X').
string__digit_to_char(34, 'Y').
string__digit_to_char(35, 'Z').

string__to_char_list(String, CharList) :-
	string__to_int_list(String, IntList),
	string__int_list_to_char_list(IntList, CharList).

string__from_char_list(CharList, String) :-
	string__char_list_to_int_list(CharList, IntList),
	string__to_int_list(String, IntList).

:- pred string__int_list_to_char_list(list(int), list(character)).
:- mode string__int_list_to_char_list(in, out) is det.

string__int_list_to_char_list([], []).
string__int_list_to_char_list([Code | Codes], [Char | Chars]) :-
	( char_to_int(Char0, Code) ->
		Char = Char0
	;
		error("string__int_list_to_char_list: char_to_int failed")
	),
	string__int_list_to_char_list(Codes, Chars).

:- pred string__char_list_to_int_list(list(character), list(int)).
:- mode string__char_list_to_int_list(in, out) is det.

string__char_list_to_int_list([], []).
string__char_list_to_int_list([Char | Chars], [Code | Codes]) :-
	( char_to_int(Char, Code0) ->
		Code = Code0
	;
		error("string__char_list_to_int_list: char_to_int failed")
	),
	string__char_list_to_int_list(Chars, Codes).

string__to_upper(StrIn, StrOut) :-
        string__to_char_list(StrIn, ListLow),
        string__char_list_to_upper(ListLow, ListUpp),
        string__from_char_list(ListUpp, StrOut).

:- pred string__char_list_to_upper(list(char), list(char)).
:- mode string__char_list_to_upper(in, out) is det.
string__char_list_to_upper([], []).
string__char_list_to_upper([X|Xs], [Y|Ys]) :-
        char__to_upper(X,Y),
        string__char_list_to_upper(Xs,Ys).

string__capitalize_first(S0, S) :-
	( string__first_char(S0, C, S1) ->
		char__to_upper(C, UpperC),
		string__first_char(S, UpperC, S1)
	;
		S = S0
	).

string__uncapitalize_first(S0, S) :-
	( string__first_char(S0, C, S1) ->
		char__to_lower(C, LowerC),
		string__first_char(S, LowerC, S1)
	;
		S = S0
	).

string__is_alpha(S) :-
	( string__first_char(S, C, S1) ->
		char__is_alpha(C),
		string__is_alpha(S1)
	;
		true
	).

string__is_alpha_or_underscore(S) :-
	( string__first_char(S, C, S1) ->
		char__is_alpha_or_underscore(C),
		string__is_alpha_or_underscore(S1)
	;
		true
	).

string__is_alnum_or_underscore(S) :-
	( string__first_char(S, C, S1) ->
		char__is_alnum_or_underscore(C),
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

string__append_list([], "").
string__append_list([S | Ss], L) :-
	string__append_list(Ss, L0),
	string__append(S, L0, L).

%-----------------------------------------------------------------------------%

	% Note - string__hash is also defined in code/imp.h
	% The two definitions must be kept identical.

string__hash(String, HashVal) :-
	string__length(String, Length),
	string__to_int_list(String, CodeList),
	string__hash_2(CodeList, 0, HashVal0),
	HashVal is HashVal0 ^ Length.

:- pred string__hash_2(list(int), int, int).
:- mode string__hash_2(in, in, out) is det.

string__hash_2([], HashVal, HashVal).
string__hash_2([X | Xs], HashVal0, HashVal) :-
	string__combine_hash(HashVal0, X, HashVal1),
	string__hash_2(Xs, HashVal1, HashVal).

:- pred string__combine_hash(int, int, int).
:- mode string__combine_hash(in, in, out) is det.

string__combine_hash(H0, X, H) :-
	H1 is H0 << 5,
	H2 is H1 ^ H0,
	H is H2 ^ X.

:- end_module string.

%-----------------------------------------------------------------------------%
