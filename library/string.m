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
:- import_module char, int, float, require, std_util.

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


:- pred string__replace(string, string, string, string).
:- mode string__replace(in, in, in, out) is semidet.
% 	string__replace replaces the first occurence of the second string in 
% 	the first string with the third string to give the fourth string.
% 	It fails if the second string does not occur in the first.

:- pred string__replace_all(string, string, string, string).
:- mode string__replace_all(in, in, in, out) is det.
% 	string__replace_all replaces any occurences of the second string in 
% 	the first string with the third string to give the fourth string.


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


:- pred string__format(string, list(string__poly_type), string).
:- mode string__format(in, in, out) is det.
%
%	A function similar to sprintf() in C.  
%	string__format("%s %i %c %f\n", [s("Square-root of"), i(2), c('='), 
%			f(1.41)], "Square-root of 2 = 1.41\n").
%
%	All the normal options available in C are supported, ie Flags [0+-# ],
%	a field width (or *) '.' precision (could be a '*'), and a length
%	modifier (currently ignored).
%
%	Valid conversion character types are {dioxXucsfeEgGp%}.  %n will not
%	work.  string__format will not return the length of the string.
%
%	conv	var	output form.		effect of '#'.
%	char.	type.
%
%	d	int	signed integer
%	i	int	signed integer
%	o	int	signed octal		with '0' prefix
%	x,X	int	signed hex		with '0x', '0X' prefix
%	u	int	unsigned integer	
%	c	char	character
%	s	string	string
%	f	float	rational number		with '.', if precision 0
%	e,E	float	[-]m.dddddE+-xx		with '.', if precision 0
%	g,G	float	either e or f		with trailing zeros.
%	p	int	integer
%
%	An option of zero will cause any padding to be zeros rather than spaces.
%	A '-' will cause the output to be right-justified in its 'space'. 
%	A '+' forces a sign to be printed.  This is not sensable for string and
%	character output.  A ' ' causes a space to be printed before a string
%	if there is no sign there.  The other option is the '#', which 
%	modifies the output string's format.  These options are normally put 
%	directly after the '%'.
%
%	Note:	A string may print incorretly if you specify a precision longer
%		than the string, and '0' padding.
%
%		%x will print with uppercase hex numbers.
%
%		%g, %G don't convert to %f correctly.  (They effectively go
%		to %f through %#g %#G, IE have trailing zeros.)
%
%		%#.0e, %#.0E won't print a '.' before the 'e' ('#' ignored).
%
%		%.*z is buggy, and doesn't work currently. (z is conv. char.)
%
%		Compiles, but does not run under either Nu-prolog or sicstus.
%


%------------------------------------------------------------------------------%

:- type string__poly_type --->	
			f(float)
		;	i(int)
		;	s(string)
		;	c(char).


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


string__replace(String, SubString0, SubString1, StringOut) :-
	string__to_char_list(String, CharList),
	string__to_char_list(SubString0, SubCharList0),
	find_sub_charlist(CharList, SubCharList0, Before, After),
	string__to_char_list(SubString1, SubCharList1),
	list__append(Before, SubCharList1, Before0),
	list__append(Before0, After, CharListOut),
	string__from_char_list(CharListOut, StringOut).

string__replace_all(String, SubString0, SubString1, StringOut) :-
	string__to_char_list(String, CharList),
        string__to_char_list(SubString0, SubCharList0),
        string__to_char_list(SubString1, SubCharList1),
       	find_all_sub_charlist(CharList, SubCharList0, SubCharList1, 
		CharListOut),
       	string__from_char_list(CharListOut, StringOut).


	% find_all_sub_charlist replaces any occurences of the second list of
	% characters (in order) in the first list of characters with the second
	% list of characters.
:- pred find_all_sub_charlist(list(character), list(character), list(character),
	list(character)).
:- mode find_all_sub_charlist(in, in, in, out) is det.

find_all_sub_charlist(CharList, SubCharList0, SubCharList1, CharList0) :-
		% find the first occurence
		find_sub_charlist(CharList, SubCharList0, BeforeList, AfterList)
	->
		(
			AfterList = []
		->
			% at the end
			list__append(BeforeList, SubCharList1, CharList0)
		;
			% recursively find the rest of the occurences
			find_all_sub_charlist(AfterList, SubCharList0, 
				SubCharList1, AfterList0),
			list__append(BeforeList, SubCharList1, BeforeList0),
			list__append(BeforeList0, AfterList0, CharList0)
		)
	;
		%no occurences left
		CharList0 = CharList.


	% find_sub_charlist(List, SubList, Before, After) is true iff SubList
	% is a sublist of List, and Before is the list of characters before
	% SubList in List, and After is the list after it.
:- pred find_sub_charlist(list(character), list(character), list(character),
	list(character)).
:- mode find_sub_charlist(in, in, out, out) is semidet.

find_sub_charlist(CharList, [], [], CharList).
find_sub_charlist([C|CharList], [S|SubCharList], Before, After) :-
		C = S
	->
		(
			find_rest_of_sub_charlist(CharList, SubCharList, After0)
		->
			Before = [],
			After = After0
		;
			find_sub_charlist(CharList, [S|SubCharList], Before0, 
				After0),
			Before = [C|Before0],
			After = After0

		)
	;
		find_sub_charlist(CharList, [S|SubCharList], Before0, After),
		Before = [C|Before0].

	% find_rest_of_sub_charlist(List, SubList, After) is true iff List
	% begins with all the characters in SubList in order, and end with
	% After.
:- pred find_rest_of_sub_charlist(list(character), list(character), 
	list(character)).
:- mode find_rest_of_sub_charlist(in, in, out) is semidet.

find_rest_of_sub_charlist(CharList, SubCharList, After) :-
	list__append(SubCharList, After, CharList).



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


%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%

%	XXXXX when string__to_lower exits, fix %x option.


string__format( Fstring, Poly_list, Ostring ) :-
	(
		string__length(Fstring, 0)
	->
		Fstring = Ostring
	;
		string__to_char_list(Fstring, Clist),
		string__format_2(Clist, Poly_list, Ostring)
	).


:- pred	string__format_2(list(char), list(string__poly_type), string).
:- mode string__format_2(in, in, out) is det.
%
%	string__format_2( stream, char_f, vars, IO, IO).
%		The main function, with different input types.
%
string__format_2( [], _, "").
string__format_2( [Achar|As], Vars_in, Ostring) :-
	(
	Achar = '%'
	->	
		(
		string__format_top_convert_variable(As, As_out, Vars_in, 
				Vars_out, String_1)
		->
			string__format_2(As_out, Vars_out, String_2),
			string__append(String_1, String_2, Ostring)
		;
			error("string__format: Number|type of arguments.")
		)
	;
		string__format_2(As, Vars_in, Temp_out),
		string__first_char(Ostring, Achar, Temp_out)
	).


:- pred string__format_top_convert_variable( list(char), list(char), 
		list(string__poly_type), list(string__poly_type), string).
:- mode string__format_top_convert_variable(in, out, in, out, out) is semidet.
%
%    string__format_top_convert_variable( formated string in, out, var in, out,
%			Out string)
%		Return a string of the formatted variable.
%
string__format_top_convert_variable(['%'|Bs], Bs, [], [], Out_string) :-
	Out_string is "%".
string__format_top_convert_variable( F_chars_in, F_chars_out, 
			[Var_in|Vars_l_in], Vars_out, Out_string ) :-
	string__format_takewhile1( F_chars_in, [Conv_char_0|F_chars_out],
			Fmt_info),
			     %	Seperate formatting info from formatting string.
			     %	in, out, out
	string__format_get_optional_args( Fmt_info, Flags, Int_width, 
			Int_precis, Conv_modify),
			     %	Parse formatting info.
			     %	in, out, out, out, out.
	string__format_mod_conv_char( Precision, Var_in, Conv_char_1, 
			Conv_char_0),
			     %	Modify(?) conversion character.
			     %	in, in, out, in
	string__format_do_mod_char( Conv_modify, Conv_char_2, Conv_char_1),
			     %	Interperate input conversion modifiers.
			     %	in, out, in
	string__format_read_star( Vars_out, Vars_l_in, Width, Int_width, 
			Precision, Int_precis),
			     %	Do something if a precision or width was '*'
			     %  out, in, out, in, out, in
	string__format_do_conversion(Conv_char_2, Var_in, Ostring, Precision,
			Flags, Move_i0),
			     %	Actually convert a Variable to a string
			     %	in, out, in, in, out, in, in, out
	string__format_add_sign( Ostring2, Ostring, Flags, Var_in, Move_i0, 
			Move_i1),
			     %	Adds an optional '+' or ' ' to string.
			     %	out, in, in, in, in, out
	string__format_pad_width( Ostring2, Width, Flags, Out_string, Move_i1).
			     %	Ensures that the string is at least width.
			     %	in, in, in, out, in



%
%	Conv_c_inhange conversion character.
%
%	Idealy the outer "->" symbols could be removed, the last case given
%	a guard, and the compiler accept thi  as det, rather than non-det.
%
:- pred string__format_mod_conv_char( int, string__poly_type, char, char).
:- mode string__format_mod_conv_char( in, in, out, in) is det.
string__format_mod_conv_char( Precision, Poly_var, Conv_c_out, Conv_c_in) :- 
	(
	Conv_c_in = 'i'
	->
		Conv_c_out = 'd'		% %d = %i
	;
	Conv_c_in = 'g'			%g is either %e of %f
	->
		(Poly_var = f(F)
		->
			string__float_abs(F, Ft),
			int__pow(10, Precision, P),
			int__to_float(P, Pe),
			( builtin_float_gt(Ft, 0.0001), builtin_float_gt(Pe, Ft)
			->
				Conv_c_out = 'f'
			;
				Conv_c_out = 'e'
			)
		;
			error("string__format:  %g without a f(Float).")
		)
	;
	Conv_c_in = 'G'			%G is either %E of %f
	->
		(Poly_var = f(F)
		->
			string__float_abs(F, Ft),
			int__pow(10, Precision, P),
			int__to_float(P, Pe),
			( builtin_float_gt(Ft, 0.0001), builtin_float_gt(Pe, Ft)
			->
				Conv_c_out = 'f'
			;
				Conv_c_out = 'E'
			)
		;
			error("string__format:  %G without a f(float).")
		)
	;
		Conv_c_out = Conv_c_in
	).



%	This function glances at the input-modification flags, only applicable
%	with a more complicated type system
%
%	Another function that would be better off as a switch.
%
:- pred string__format_do_mod_char( char, char, char).
:- mode string__format_do_mod_char( in, out, in) is det.
string__format_do_mod_char( Char_mods, C_out, C_in) :- 
	(
		Char_mods = 'h'
	->
		C_out = C_in
	;
		Char_mods = 'l'
	->
		C_out = C_in
	;
		Char_mods = 'L'
	->
		C_out = C_in
	;
		C_out = C_in
	).


%
%	Change Width or Precision vaalue, if '*' was spcified
%
:- pred string__format_read_star( list(string__poly_type), list(string__poly_type), int, int, int, int).
:- mode string__format_read_star( out, in, out, in, out, in) is semidet.
string__format_read_star( Polys_out, Polys_in, Width, Int_width, Precision, Int_precis) :-
	(string__special_precision_and_width(Int_width)
	->
		Polys_in = [ i(Width) |  Poly_temp]
	;
		Polys_in = Poly_temp,
		Int_width = Width
	),
	(string__special_precision_and_width(Int_precis)
	->
		Poly_temp = [ i(Precision) | Polys_out]
	;
		Polys_out = Poly_temp,
		Int_precis = Precision
	).



%
%	This function did the variable conversion to string.
%	Now done by string__do_conversion_hack/6.
%
%
%	Mv_width records the length of the prefix in front of the number,
%	so that it is more easy to insert width and precision padding and 
%	optional signs, in the correct place.
%
:- pred string__format_do_conversion( char, string__poly_type, string, int, 
		list(char), int).
:- mode string__format_do_conversion( in, in, out, in, in, out)
		is det.
string__format_do_conversion( Conv_c, Poly_t, Ostring, Precision, Flags,
		Mv_width) :-
	(string__do_conversion_hack(Conv_c, Poly_t, Tstring, Precision, Flags,
		TMv_width)
		->
		TMv_width = Mv_width,
		Ostring = Tstring
	;
		string__do_conversion_fail(Conv_c),
		Mv_width = 0,
		Ostring = ""
	).

:- pred string__do_conversion_hack(char, string__poly_type, string, int, 
		list(char), int).
:- mode string__do_conversion_hack(in, in, out, in, in, out) is semidet.
string__do_conversion_hack(Conv_c, Poly_t, Ostring, Precision, Flags, 
		Mv_width) :-
	(
	Conv_c = 'd',
		Poly_t = i(I),
		string__int_to_string(I, S),
		string__format_int_precision(S, Ostring, Precision, _),
		(
		I < 0
		->
			Mv_width is 1
		;
			Mv_width is 0 
		)
			
	; 
	Conv_c = 'o',
		Poly_t = i(I),
		( I = 0
		->
			S = "0",
			string__format_int_precision(S, Ostring, Precision, _),
			Pfix_len = 0
		;
			string__int_to_base_string(I, 8, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member('#', Flags)
			->
				string__first_char(Ostring, '0', SS),
				Pfix_len = 1
			;
				Ostring = SS,
				Pfix_len = 0
			)
		),
		(I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len)
	;
	Conv_c = 'x' ,
		Poly_t = i(I),
		( I = 0
		->
			SS = "0",
			Pfix_len = 0,
			string__format_int_precision(SS, Ostring, Precision, _)
		;
			string__int_to_base_string(I, 16, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member( '#', Flags)
			->
				string__append( "0x", SS, Ostring),
				Pfix_len = 2
			;
				Ostring = SS,
				Pfix_len = 0
			)
		),
		(I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len)
	;
	Conv_c = 'X',
		Poly_t = i(I),
		( I = 0 ->
			SS = "0",
			Pfix_len = 0,
			string__format_int_precision(SS, Ostring, Precision, _)
		;
			string__int_to_base_string(I, 16, Otemp),
			string__to_upper(Otemp, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member( '#', Flags) ->
				string__append( "0X", SS, Ostring),
				Pfix_len = 2
			;
				SS = Ostring,
				Pfix_len = 0
			)
		),
		(I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conv_c = 'u' ,
		Poly_t = i(I),
		int__abs(I, J),
		string__int_to_string(J, S),
		string__format_int_precision(S, Ostring, Precision, Mvt),
		Mv_width = Mvt
	;		
	Conv_c = 'c' ,
		Poly_t = c(C),
		string__char_to_string( C, Ostring),
		Mv_width = 0
	;
	Conv_c = 's' ,
		Poly_t = s(S),
		( string__default_precision_and_width(Precision) ->
			S = Ostring
		;
			string__split(S, Precision, Ostring, _)
		),
		Mv_width = 0
	;
	Conv_c = 'f' ,
		Poly_t = f(F),
		string__float_to_string(F, Fstring),
		string__format_calc_prec(Fstring, Ostring, Precision),
		(builtin_float_lt(F, 0.0)-> Mv_width = 1 ; Mv_width = 0)
	;	
	Conv_c = 'e',
		Poly_t = f(F),
		string__format_calc_exp(F, Ostring, Precision, 0),
		(builtin_float_lt(F, 0.0)-> Mv_width = 1 ; Mv_width = 0)
	;
	Conv_c = 'E' ,
		Poly_t = f(F),
		string__to_upper(Otemp, Ostring),
		string__format_calc_exp(F, Otemp, Precision, 0),
		(builtin_float_lt(F, 0.0)-> Mv_width = 1 ; Mv_width = 0)
	;
	Conv_c = 'p' ,
		Poly_t = i(I),
		string__int_to_string(I, Ostring),
		((I < 0) -> Mv_width = 1 ; Mv_width = 0)
	).


:- pred string__do_conversion_fail(char).
:- mode string__do_conversion_fail(in) is erroneous.
string__do_conversion_fail(Conv_c) :-
	string__format("%s %%%c, without a correct poly-varable.", 
		[s("string__format:  statement has used type"), c(Conv_c)],
		Error_message),
	error(Error_message).


%
%	Use precision information to modify string.  - for integers
%

:- pred string__format_int_precision(string, string, int, int).
:- mode string__format_int_precision( in, out, in, out) is semidet.
string__format_int_precision(S, Ostring, Precision, Added_width) :-
	(
	string__default_precision_and_width(Precision) ->
		Prec = 0
	;
		Prec = Precision
	),
	string__length(S, L),
	(string__first_char(S, '-', _)
		->
		Xzeros is Prec - L + 1
		;
		Xzeros is Prec - L
	),
	Added_width = Xzeros,
	(
	Xzeros > 0
	->	
		string__duplicate_char( '0', Xzeros, Pfix),
		string__first_char(S, C, Rest),
		(
		C \= ('-'),
		C \= ('+')
		->
			string__append(Pfix, S, Ostring)
		;
			string__append( Pfix, Rest, Temps),
			string__first_char( Ostring, C, Temps)
		)
	;
		Ostring = S
	).

%	Function  to calculate exponent for a %e conversion of a float
%
:- pred string__format_calc_exp(float, string, int, int).
:- mode string__format_calc_exp(in, out, in, in) is det.
string__format_calc_exp(F, Fstring, Precision, Exp) :-
	builtin_float_lt(F, 0.0)
	-> 	
		builtin_float_minus( 0.0, F, Tf),
		string__format_calc_exp( Tf, Tst, Precision, Exp),
		string__first_char(Fstring, '-', Tst)
	;
		(
		builtin_float_lt(F, 1.0)
		->
			Texp is Exp - 1,
			builtin_float_times(F, 10.0, FF),
			string__format_calc_exp( FF, Fstring, Precision, Texp)
		;
		(
		builtin_float_ge(F, 10.0)
		->
			Texp is Exp + 1,
			builtin_float_divide(F, 10.0, FF),
			string__format_calc_exp( FF, Fstring, Precision, Texp)
		;
			string__float_to_string(F, Fs),
			string__format_calc_prec(Fs, Fs2, Precision),
			string__int_to_string(Exp, Exps),
			(Exp < 0
			->
				string__append_list([ Fs2, "e", Exps], Fstring)
			;
				string__append_list([ Fs2, "e+", Exps], Fstring)
			)
		)
		).

	
%
%	This precision output-modification predicate handles floats.
%
:- pred string__format_calc_prec(string, string, int).
:- mode string__format_calc_prec(in, out, in) is det.
string__format_calc_prec(Istring, Ostring, Precision) :-
	(string__default_precision_and_width(Precision) ->
		Prec is 6
	;
		Prec is Precision),
	(
	string__find_index( Istring, '.', 1, Index)
	->
		Spa is Prec + Index + 1
	;
		error("stringf:  An error with conversion  float==>string")
	),
	(
	string__length(Istring, L1),
	L1 < Spa
	->
		string__duplicate_char('0', Precision, P0s),
		string__append( Istring, P0s, Mstring)
	;
		Mstring = Istring
	),
	(
	Precision = 0
	->
		Space is Spa - 1
	;
		Space is Spa
	),
	string__split(Mstring, Space, Ostring, _).



%	string__find_index is a funky little predicate to find the first
%	occourance of a particular character in a string.
%
:- pred string__find_index( string, char, int, int).
:- mode string__find_index( in, in, in, out) is semidet.
string__find_index( A, Ch, Check, Ret) :-
	string__length(A, Len),
	Len < Check
	->
		fail
	;
	string__index(A, Check, Ch)
	->
		Ret = Check
	;
		Check2 is Check + 1,
		string__find_index( A, Ch, Check2, Ret)
	.






%	Add a '+' or ' ' sign, if it is needed in this output.
%
:- pred string__format_add_sign( string, string, list(char), string__poly_type, int, int).
:- mode string__format_add_sign( out, in, in, in, in, out) is det.
%			Mvw is the prefix-length in front of the number.
string__format_add_sign( Ostring, Istring, Flags, _V, Mvw1, Mvw2) :-
	T1 is Mvw1 - 1,
	(
	string__index(Istring, T1, '-')
	->
		Ostring = Istring,
		Mvw2 = Mvw1
	;
		string__split(Istring, Mvw1, Lstring, Rstring),
		(
		list__member(('+'), Flags)
		->
			string__append_list( [Lstring, "+", Rstring], Ostring),
			Mvw2 is Mvw1 + 1
		;
			list__member(' ', Flags)
			->
				string__append_list( [Lstring, " ", Rstring], Ostring),
				Mvw2 is Mvw1 + 1
			; 
				Ostring = Istring,
				Mvw2 = Mvw1
		)
	).



%
% This function pads some characters to the left or right of a string that is
% shorter than it's width.
%
:- pred string__format_pad_width( string, int, list(char), string, int).
:- mode string__format_pad_width( in, in, in, out, in) is det.
%		( String in, width, flags, Output string, #Moveables).
string__format_pad_width( Istring, Width, Flags, Out_string, Mv_cs) :-
	string__length(Istring, Len),
	(Len < Width
	->
		% time for some FLAG tests
		Xspace is Width - Len,
		(
		list__member('0', Flags)
		->
			Pad_char = '0'
		;
			Pad_char = ' '
		),
		string__duplicate_char(Pad_char, Xspace, Pad_string),
		(
		list__member('-', Flags)
		->
			string__append(Istring, Pad_string, Out_string)
		;
			list__member('0', Flags)
			->
				string__split(Istring, Mv_cs, B4, After),
				string__append_list( [B4, Pad_string, After], Out_string)
			;
			string__append( Pad_string, Istring,  Out_string)
		)
	;
		Out_string = Istring
	).


:- pred string__format_get_optional_args( list(char), list(char), int, int, char).
:- mode string__format_get_optional_args( in, out, out, out, out) is det.
%	string__format_get_optional_args( format info, flags, width, precision, modifier)
%		format is assumed to be in ANSI C format.
%		p243-4 of Kernighan & Ritchie 2nd Ed. 1988
%		"Parse" format informtion.
%
% A function to do some basic parsing on the optional printf arguments.
%
string__format_get_optional_args( [], Flags, Width, Precision, Mods) :-
		Flags = [],
		Width = 0,
		string__default_precision_and_width(Precision),
		Mods = ' '.
string__format_get_optional_args( [A|As], Flags, Width, Precision, Mods) :-
		(A = (-) ; A = (+) ; A = ' ' ; A = '0' ; A = '#' )
	->
		string__format_get_optional_args( As, Oflags, Width, Precision, Mods),
		UFlags = [A | Oflags],
		list__sort(UFlags, Flags)
	;
		( A = (.) ; A = '1' ; A = '2' ; A = '3' ; A = '4' ;
		  A = '5' ; A = '6' ; A = '7' ; A = '8' ; A = '9' )
	->
		string__format_string_to_ints([A|As], Bs, Numl1, Numl2, yes),
		string__format_int_from_char_list( Numl1, Width),
		string__format_int_from_char_list( Numl2, Prec),
		string__format_get_optional_args( Bs, Flags, _, Ptemp, Mods),
		(Numl2 = [] ->
			Precision = Ptemp
		;
			Precision = Prec
		)
	;
		( A = 'h' ; A = 'l' ; A = 'L' )
	->
		Mods = A,
		string__format_get_optional_args( As, Flags, Width, Precision, _)
	;
		A = ('*')
	->
		string__format_get_optional_args( As, Flags, _W, P, Mods),
		(string__default_precision_and_width(P)
			->
		string__special_precision_and_width(Precision)
			; 
		Precision = P),
		string__special_precision_and_width(Width)
	;
		error("string__format:  Unrecognised formatting information\n")
	.

:- pred string__format_takewhile1(list(char), list(char), list(char)).
:- mode string__format_takewhile1(in, out, out) is det.
%	string__format_takewhile(formatted string in, out, format info).
%		A HACK.  Would be much nicer with a proper string__takewhile.
%		Looses the format info from the front of the first argument,
%		puts this in the last argument, while the second is the
%		remainder of the string.
%
string__format_takewhile1([], [], []).
string__format_takewhile1( [A|As], Rem, Finf) :-
	( A = 'd';A = 'i';A = 'o';A = 'x';A = 'X';A = 'u';A = 's';
	  A = 'c';A = 'f';A = 'e';A = 'E';A = 'g';A = 'G';A = 'p')
	->	
		Rem = [A|As],
		Finf = []
	;
		string__format_takewhile1(As, Rem, F),
		Finf = [A|F].


	
:- pred string__format_string_to_ints(list(char), list(char), list(char), list(char), bool).
:- mode string__format_string_to_ints(in, out, out, out, in) is det.
%	string__format_string_to_ints( String in, out, Number1, Number2, seen '.' yet?)
%		Takes in a char list and splits off the rational number at the 
%		start of the list.  This is split into 2 parts - an int and a
%		fraction.
%
string__format_string_to_ints( [], [], [], [], _).
string__format_string_to_ints( [A|As], Bs, Int1, Int2, Bool) :-
	char__is_digit(A)
	->
	(	Bool = yes
		->
			string__format_string_to_ints( As, Bs, I1, Int2, yes),
			Int1 = [A|I1]
		;
			string__format_string_to_ints(As, Bs, Int1, I2, no),
			Int2 = [A|I2]
	)
	;
	(	A = ('.')
		->
			string__format_string_to_ints( As, Bs, Int1, Int2, no)
		;
			Bs = [A|As],
			Int1 = [],
			Int2 = []
	).

			
			


:- pred string__format_int_from_char_list( list(char), int).
:- mode string__format_int_from_char_list( in, out) is det.
%		Convert a char_list to an int
%
string__format_int_from_char_list( [], 0).
string__format_int_from_char_list( [L|Ls], I) :-
	string__from_char_list( [L|Ls], S),
	string__to_int( S, I_hack)
		->
	I = I_hack
		;
	I = 0.


:- pred string__float_abs(float, float).
:- mode string__float_abs(in, out) is det.
string__float_abs(Fin, Fout) :-
	builtin_float_lt(Fin, 0.0)
	->
		builtin_float_minus(0.0, Fin, Fout)
	;
		Fout = Fin
	.


:- pred string__default_precision_and_width(int).
:- mode string__default_precision_and_width(out) is det.
string__default_precision_and_width(-6).

:- pred string__special_precision_and_width(int).
:- mode string__special_precision_and_width(out) is det.
string__special_precision_and_width(-1).



:- end_module string.

%-----------------------------------------------------------------------------%
