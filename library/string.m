%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module string.

% Main authors: fjh, dylan.
% Stability: medium to high.

% This modules provides basic string handling facilities.

% Beware that char_to_string/2 won't work with NU-Prolog 1.5.33 because
% of a NU-Prolog bug (fixed in NU-Prolog 1.5.35).

%-----------------------------------------------------------------------------%

:- interface.
:- import_module list, char.

:- pred string__length(string, int).
:- mode string__length(in, out) is det.
	% Determine the length of a string.
	% An empty string has length zero.

:- pred string__append(string, string, string).
:- mode string__append(in, in, in) is semidet.	% implied
:- mode string__append(in, out, in) is semidet.
:- mode string__append(in, in, out) is det.
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

:- pred string__char_to_string(char, string).
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

:- pred string__first_char(string, char, string).
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

:- pred string__to_lower(string, string).
:- mode string__to_lower(in, out) is det.
:- mode string__to_lower(in, in) is semidet.		% implied
%	Converts a string to lowercase.

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

:- pred string__to_char_list(string, list(char)).
:- mode string__to_char_list(in, out) is det.

:- pred string__from_char_list(list(char), string).
:- mode string__from_char_list(in, out) is det.
:- mode string__from_char_list(out, in) is semidet.
	% XXX second mode should be det too
	% (but this turns out to be tricky to implement)

:- pred string__from_rev_char_list(list(char), string).
:- mode string__from_rev_char_list(in, out) is det.
%	Same as string__from_char_list, except that it reverses the order
%	of the characters.

:- pred string__to_int(string, int).
:- mode string__to_int(in, out) is semidet.
% 	Convert a string to an int.  The string must contain only digits,
% 	optionally preceded by a plus or minus sign.  If the string does
% 	not match this syntax, string__to_int fails.

:- pred string__base_string_to_int(int, string, int).
:- mode string__base_string_to_int(in, in, out) is semidet.
% 	Convert a string in the specified base (2-36) to an int.  The
% 	string must contain only digits in the specified base, optionally
% 	preceded by a plus or minus sign.  For bases > 10, digits 10 to 35
% 	are repesented by the letters A-Z or a-z.  If the string does not
% 	match this syntax, the predicate fails.

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

:- pred string__pad_left(string, char, int, string).
:- mode string__pad_left(in, in, in, out) is det.
%	string__pad_left(String0, PadChar, Width, String):
%	insert `PadChar's at the left of `String0' until it is at least
%	as long as `Width', giving `String'.

:- pred string__pad_right(string, char, int, string).
:- mode string__pad_right(in, in, in, out) is det.
%	string__pad_right(String0, PadChar, Width, String):
%	insert `PadChar's at the right of `String0' until it is at least
%	as long as `Width', giving `String'.

:- pred string__duplicate_char(char, int, string).
:- mode string__duplicate_char(in, in, out) is det.
%	string__duplicate_char(Char, Count, String):
%	construct a string consisting of `Count' occurrences of `Char'
%	in sequence.

:- pred string__contains_char(string, char).
:- mode string__contains_char(in, in) is semidet.
%	string__contains_char(String, Char):
%	succeed if `Char' occurs in `String'.

:- pred string__index(string, int, char).
:- mode string__index(in, in, out) is semidet.
%	string__index(String, Index, Char):
%	`Char' is the (`Index' + 1)-th character of `String'.
%	Fails if `Index' is out of range (negative, or greater than or
%	equal to the length of `String').

:- pred string__index_det(string, int, char).
:- mode string__index_det(in, in, out) is det.
%	string__index_det(String, Index, Char):
%	`Char' is the (`Index' + 1)-th character of `String'.
%	Calls error/1 if `Index' is out of range (negative, or greater than or
%	equal to the length of `String').

:- pred string__foldl(pred(char, T, T), string, T, T).
:- mode string__foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode string__foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode string__foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode string__foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode string__foldl(pred(in, in, out) is multi, in, in, out) is multi.
%	string__foldl(Closure, String, Acc0, Acc):
%	`Closure' is an accumulator predicate which is to be called for each
%	character of the string `String' in turn. The initial value of the
%	accumulator is `Acc0' and the final value is `Acc'.
%	(string__foldl is equivalent to
%		string__to_char_list(String, Chars),
%		list__foldl(Closure, Chars, Acc0, Acc)
%	but is implemented more efficiently.)

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
%	`RightSubstring' is the right-most `Count' characters of `String'.
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
:- mode string__append_list(out, in) is multidet.
%	Append a list of strings together.

:- pred string__hash(string, int).
:- mode string__hash(in, out) is det.
%	Compute a hash value for a string.

:- pred string__sub_string_search(string, string, int).
:- mode string__sub_string_search(in, in, out) is semidet.
%	string__sub_string_search(String, SubString, Index).
%	`Index' is the position in `String' where the first occurrence of
%	`SubString' begins.
%	Do a brute-force search in the first string for the second string.
%	XXX Note: not the most efficient algorithm.

:- pred string__format(string, list(string__poly_type), string).
:- mode string__format(in, in, out) is det.
%
%	A function similar to sprintf() in C.  
%
%	For example,
%		string__format("%s %i %c %f\n", 
%			[s("Square-root of"), i(2), c('='), f(1.41)], String)
%	will return
%		String = "Square-root of 2 = 1.41\n".
%
%	All the normal options available in C are supported, ie Flags [0+-# ],
%	a field width (or *), '.', precision (could be a '*'), and a length
%	modifier (currently ignored).
%
%	Valid conversion character types are {dioxXucsfeEgGp%}.  %n is not
%	supported.  string__format will not return the length of the string.
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
%	A '-' will cause the output to be left-justified in its 'space'. 
%	(With a `-', the default is for fields to be right-justified.)
%	A '+' forces a sign to be printed.  This is not sensible for string and
%	character output.  A ' ' causes a space to be printed before a thing
%	if there is no sign there.  The other option is the '#', which 
%	modifies the output string's format.  These options are normally put 
%	directly after the '%'.
%
%	Note:
%		%#.0e, %#.0E won't print a '.' before the 'e' ('#' ignored).
%
%		Asking for more precision than a float actually has will
%		result in potentially misleading output.
%
%		If a width or precision is specified, without a `.', a number
%		is assumed to be a width and a `*' is assumed to be a precision.
%		It is always better to include a `.' to remove ambiguity.  This
%		interpretation is non-standard and may change.
%
%		Numbers are truncated by a precision value, not rounded off.

%------------------------------------------------------------------------------%

:- type string__poly_type --->
			f(float)
		;	i(int)
		;	s(string)
		;	c(char).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, std_util, int, float, require.

:- pred string__to_int_list(string, list(int)).
:- mode string__to_int_list(out, in) is det.
:- mode string__to_int_list(in, out) is det.

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
:- pred find_all_sub_charlist(list(char), list(char), list(char), list(char)).
:- mode find_all_sub_charlist(in, in, in, out) is det.

find_all_sub_charlist(CharList, SubCharList0, SubCharList1, CharList0) :-
		% find the first occurence
	(
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
		CharList0 = CharList
	).

	% find_sub_charlist(List, SubList, Before, After) is true iff SubList
	% is a sublist of List, and Before is the list of characters before
	% SubList in List, and After is the list after it.
:- pred find_sub_charlist(list(char), list(char), list(char), list(char)).
:- mode find_sub_charlist(in, in, out, out) is semidet.

find_sub_charlist(CharList, [], [], CharList).
find_sub_charlist([C|CharList], [S|SubCharList], Before, After) :-
	(
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
		Before = [C|Before0]
	).

	% find_rest_of_sub_charlist(List, SubList, After) is true iff List
	% begins with all the characters in SubList in order, and end with
	% After.
:- pred find_rest_of_sub_charlist(list(char), list(char), list(char)).
:- mode find_rest_of_sub_charlist(in, in, out) is semidet.

find_rest_of_sub_charlist(CharList, SubCharList, After) :-
	list__append(SubCharList, After, CharList).

string__to_int(String, Int) :-
	string__base_string_to_int(10, String, Int).

string__base_string_to_int(Base, String, Int) :-
	( string__first_char(String, Char, String1) ->
		( Char = ('-') ->
			string__base_string_to_int_2(Base, String1, 0, Int1),
			Int is 0 - Int1
		; Char = ('+') ->
			string__base_string_to_int_2(Base, String1, 0, Int)
		;
			string__base_string_to_int_2(Base, String, 0, Int)
		)
	;
		Int = 0
	).

:- pred string__base_string_to_int_2(int, string, int, int).
:- mode string__base_string_to_int_2(in, in, in, out) is semidet.

string__base_string_to_int_2(Base, String, Int0, Int) :-
	( string__first_char(String, DigitChar, String1) ->
		char__digit_to_int(DigitChar, DigitValue),
		DigitValue < Base,
		Int1 is Base * Int0,
		Int2 is Int1 + DigitValue,
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

string__foldl(Closure, String, Acc0, Acc) :-
	string__length(String, Length),
	string__foldl2(Closure, String, 0, Length, Acc0, Acc).

:- pred string__foldl2(pred(char, T, T), string, int, int, T, T).
:- mode string__foldl2(pred(in, in, out) is det, in, in, in, in, out) is det.
:- mode string__foldl2(pred(in, di, uo) is det, in, in, in, di, uo) is det.
:- mode string__foldl2(pred(in, in, out) is semidet, in, in, in, in, out)
		is semidet.
:- mode string__foldl2(pred(in, in, out) is nondet, in, in, in, in, out)
		is nondet.
:- mode string__foldl2(pred(in, in, out) is multi, in, in, in, in, out)
		is multi.

string__foldl2(Closure, String, N, Max, Acc0, Acc) :-
	(
		N >= Max
	->
		Acc = Acc0
	;
		string__unsafe_index(String, N, Char),
		call(Closure, Char, Acc0, Acc1),
		N1 is N + 1,
		string__foldl2(Closure, String, N1, Max, Acc1, Acc)
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
	char__to_int(Char, Code).

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
		char__det_int_to_digit(N, DigitChar),
		string__char_to_string(DigitChar, Str)
	;
		N10 is N mod Base,
		N1 is N // Base,
		char__det_int_to_digit(N10, DigitChar),
		string__char_to_string(DigitChar, DigitString),
		string__int_to_base_string_2(N1, Base, Str1),
		string__append(Str1, DigitString, Str)
	).

% NB: it would be more efficient to do this directly (using pragma c_code)
string__to_char_list(String, CharList) :-
	string__to_int_list(String, IntList),
	string__int_list_to_char_list(IntList, CharList).

% NB: it would be more efficient to do this directly (using pragma c_code)
string__from_char_list(CharList, String) :-
	string__char_list_to_int_list(CharList, IntList),
	string__to_int_list(String, IntList).

%
% We could implement from_rev_char_list using list__reverse and from_char_list,
% but the optimized implementation in C below is there for efficiency since
% it improves the overall speed of parsing by about 7%.
%
:- pragma(c_code, string__from_rev_char_list(Chars::in, Str::out), "
{
	Word list_ptr;
	Word size, len;
	Word str_ptr;
/*
** loop to calculate list length + sizeof(Word) in `size' using list in
** `list_ptr' and separately count the length of the string
*/
	size = sizeof(Word);
	len = 1;
	list_ptr = Chars;
	while (!list_is_empty(list_ptr)) {
		size++;
		len++;
		list_ptr = list_tail(list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(Word) - 1) / sizeof(Word) words
*/
	incr_hp_atomic(str_ptr, size / sizeof(Word));
	Str = (char *) str_ptr;
/*
** set size to be the offset of the end of the string
** (ie the \\0) and null terminate the string.
*/
	Str[--len] = '\\0';
/*
** loop to copy the characters from the list_ptr to the string
** in reverse order.
*/
	list_ptr = Chars;
	while (!list_is_empty(list_ptr)) {
		Str[--len] = (char) list_head(list_ptr);
		list_ptr = list_tail(list_ptr);
	}
}").

:- pred string__int_list_to_char_list(list(int), list(char)).
:- mode string__int_list_to_char_list(in, out) is det.

string__int_list_to_char_list([], []).
string__int_list_to_char_list([Code | Codes], [Char | Chars]) :-
	( char__to_int(Char0, Code) ->
		Char = Char0
	;
		error("string__int_list_to_char_list: char__to_int failed")
	),
	string__int_list_to_char_list(Codes, Chars).

:- pred string__char_list_to_int_list(list(char), list(int)).
:- mode string__char_list_to_int_list(in, out) is det.
:- mode string__char_list_to_int_list(out, in) is semidet.

string__char_list_to_int_list([], []).
string__char_list_to_int_list([Char | Chars], [Code | Codes]) :-
	char__to_int(Char, Code),
	string__char_list_to_int_list(Chars, Codes).

string__to_upper(StrIn, StrOut) :-
	string__to_char_list(StrIn, List),
	string__char_list_to_upper(List, ListUpp),
	string__from_char_list(ListUpp, StrOut).

:- pred string__char_list_to_upper(list(char), list(char)).
:- mode string__char_list_to_upper(in, out) is det.
string__char_list_to_upper([], []).
string__char_list_to_upper([X|Xs], [Y|Ys]) :-
	char__to_upper(X,Y),
	string__char_list_to_upper(Xs,Ys).

string__to_lower(StrIn, StrOut) :-
	string__to_char_list(StrIn, List),
	string__char_list_to_lower(List, ListLow),
	string__from_char_list(ListLow, StrOut).

:- pred string__char_list_to_lower(list(char), list(char)).
:- mode string__char_list_to_lower(in, out) is det.
string__char_list_to_lower([], []).
string__char_list_to_lower([X|Xs], [Y|Ys]) :-
	char__to_lower(X,Y),
	string__char_list_to_lower(Xs,Ys).

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
	( Count =< 0 ->
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

%-----------------------------------------------------------------------------%

string__sub_string_search(String, Substring, Index) :- 
	string__length(String, StringLength),
	string__length(Substring, SubstringLength),
	string__sub_string_search2(String, Substring, StringLength, 
			SubstringLength, Index).

:- pred string__sub_string_search2(string, string, int, int, int).
:- mode string__sub_string_search2(in, in, in, in, out) is semidet.

string__sub_string_search2(String0, SString, StrLen0,
			SStrLen, Index) :-
	StrLen0 >= SStrLen,
	(
		string__prefix(String0, SString)
	->
		Index = 0
	;
		string__first_char(String0, _, String),
		StrLen is StrLen0 - 1,
		string__sub_string_search2(String, SString, StrLen, 
				SStrLen, Index0),
		Index is Index0 + 1
	).

%-----------------------------------------------------------------------------%

string__format(Format_string, Poly_list, Out_string ) :-
	string__to_char_list(Format_string, Format_char_list),
	string__format_2(Format_char_list, Poly_list, Out_string) .

:- pred	string__format_2(list(char), list(string__poly_type), string).
:- mode string__format_2(in, in, out) is det.
%
%	string__format_2(stream, char_f, vars, IO, IO).
%		The main function, with different input types.
%
%	Accumulator recursion is not used, as it would involve adding a
%	short string to the end of a long string many times, which I understand
%	is not efficient.  Instead, many short strings are added to the front
%	of a (long) and growing string.
%
string__format_2([], _, "").
string__format_2([Achar|As], Vars_in, Ostring) :-
	(
		Achar = '%'
	->
		(
			As = ['%' | Ass]
		->
			string__format_2(Ass, Vars_in, Temp_out),
			string__first_char(Ostring, '%', Temp_out)
		;
			(
				string__format_top_convert_variable(As, Vars_in,
						As_out, Vars_out, String_1)
			->
				string__format_2(As_out, Vars_out, String_2),
				string__append(String_1, String_2, Ostring)
			;
				error("string__format: Too few variables.")
			)
		)
	;
		string__format_2(As, Vars_in, Temp_out),
		string__first_char(Ostring, Achar, Temp_out)
	).

:- pred string__format_top_convert_variable(list(char), 
			list(string__poly_type), list(char),
			list(string__poly_type), string).
:- mode string__format_top_convert_variable(in, in, out, out, out) is semidet.
%
%    string__format_top_convert_variable(formated string in, var in, formatted
%			string out, var out, Out string)
%		Return a string of the formatted variable.
%
string__format_top_convert_variable(['%'|Bs], [], Bs, [], "%").
			% Above rule should not be executed... defensive rule.
string__format_top_convert_variable(F_chars0, [Var0|Vars_list0],
			F_chars, Vars_list, OutString ) :-
	string__format_takewhile1(F_chars0, [Conv_char_0|F_chars],
			Fmt_info),
			     %	Seperate formatting info from formatting string.
			     %	in, out, out
	string__format_get_optional_args(Fmt_info, Flags, Width0, 
			Precision0, Conv_modify),
			     %	Parse formatting info.
			     %	in, out, out, out, out.
	string__format_read_star(Vars_list0,  Width0, Precision0, Vars_list,
			Width1, Precision1),
			     %	Do something if a precision or width was '*'
			     %  in, in, in, out, out, out
	string__format_mod_conv_char(Precision1, Var0, Conv_char_0, 
			Conv_char_1, Precision),
			     %	Modify(?) conversion character.
			     %  in, in, in, out, out
	string__format_do_mod_char(Conv_modify, Conv_char_1, Conv_char_2),
			     %	Interperate input conversion modifiers.
			     %	in, in, out
	string__format_do_conversion(Conv_char_2, Var0, Precision,
			Flags, Move_i0, OutString0),
			     %	Actually convert a Variable to a string
			     %	in, in, in, in, out, out
	string__format_add_sign(OutString0, Flags, Var0, Move_i0, 
			Move_i1, OutString1),
			     %	Adds an optional '+' or ' ' to string.
			     %	in, in, in, in, out, out
	string__format_pad_width(OutString1, Width1, Flags, Move_i1,
			OutString).
			     %	Ensures that the string is at least width.
			     %	in, in, in, out, in

%
%	Change conversion character.
%
%	Ideally the outer "->" symbols could be removed, the last case given
%	a guard, and the compiler accept this as det, rather than non-det.
%
:- pred string__format_mod_conv_char(maybe(int), string__poly_type, char, 
				char, maybe(int)).
:- mode string__format_mod_conv_char(in, in, in, out, out) is det.
string__format_mod_conv_char(Precision0, Poly_var, Conversion_in,
				Conversion_out, Precision) :- 
	( Precision0 = yes(Prec0) ->
		Prec = Prec0
	;
		Prec = 0
	),
	( Conversion_in = 'i' ->
		Conversion_out = 'd',		% %d = %i
		Precision = Precision0
	; 
	Conversion_in = 'g' ->			%g is either %e of %f
		(Poly_var = f(F) ->
			float__abs(F, Ft),
			int__pow(10, Prec, P),
			int__to_float(P, Pe),
			(
				Ft > 0.0001,
				Pe > Ft
			->
				Conversion_out = 'f',
				Precision = yes(0)
			;
				Conversion_out = 'e',
				Precision = Precision0
			)
		;
			error("string__format:  %g without a f(Float).")
		)
	;
	Conversion_in = 'G' ->		%G is either %E of %f
		(Poly_var = f(F) ->
			float__abs(F, Ft),
			int__pow(10, Prec, P),
			int__to_float(P, Pe),
			(
				Ft > 0.0001,
				Pe > Ft
			->
				Conversion_out = 'f',
				Precision = yes(0)
			;
				Conversion_out = 'E',
				Precision = Precision0
			)
		;
			error("string__format:  %G without a f(Float).")
		)
	;
		Conversion_out = Conversion_in,
		Precision = Precision0
	).

%	This function glances at the input-modification flags, only applicable
%	with a more complicated type system
%
%	Another function that would be better off as a switch.
%
:- pred string__format_do_mod_char(char, char, char).
:- mode string__format_do_mod_char(in, in, out) is det.
string__format_do_mod_char(Char_mods, C_in, C_out) :- 
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
%	Change Width or Precision value, if '*' was spcified
%
:- pred string__format_read_star(list(string__poly_type), int, int,
		list(string__poly_type), maybe(int), maybe(int)).
:- mode string__format_read_star(in, in, in, out, out, out) is semidet.
string__format_read_star(Polys_in, Int_width, Int_precis, Polys_out,
		Width, Precision) :-
	(
		string__special_precision_and_width(Int_width)
	->
		Polys_in = [ i(Width0) |  Poly_temp],
		Width = yes(Width0)
	;
		( string__default_precision_and_width(Int_width) ->
			Width = no
		;
			Width = yes(Int_width)
		),
		Polys_in = Poly_temp
	),
	(
		string__special_precision_and_width(Int_precis)
	->
		Poly_temp = [ i(Precision0) | Polys_out],
		Precision = yes(Precision0)
	;
		( string__default_precision_and_width(Int_precis) ->
			Precision = no
		;
			Precision = yes(Int_precis)
		),
		Polys_out = Poly_temp
	).

%
%	This function did the variable conversion to string.
%	Now done by string__do_conversion_0/6.
%
%
%	Mv_width records the length of the prefix in front of the number,
%	so that it is more easy to insert width and precision padding and 
%	optional signs, in the correct place.
%
:- pred string__format_do_conversion(char, string__poly_type, maybe(int), 
		list(char), int, string).
:- mode string__format_do_conversion(in, in, in, in, out, out)
		is det.
string__format_do_conversion(Conversion, Poly_t, Precision, Flags, Mv_width,
		Ostring) :-
	(
		string__do_conversion_0(Conversion, Poly_t, Ostring0, 
				Precision, Flags, Mv_width0)
	->
		Mv_width = Mv_width0,
		Ostring = Ostring0
	;
		string__do_conversion_fail(Conversion)
	).

:- pred string__do_conversion_0(char, string__poly_type, string, maybe(int), 
		list(char), int).
:- mode string__do_conversion_0(in, in, out, in, in, out) is semidet.
string__do_conversion_0(Conversion, Poly_t, Ostring, Precision, Flags, 
		Mv_width) :-
	(
	Conversion = 'd',
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
	Conversion = 'o',
		Poly_t = i(I),
		( I = 0 ->
			S = "0",
			string__format_int_precision(S, Ostring, Precision, _),
			Pfix_len = 0
		;
			string__int_to_base_string(I, 8, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member('#', Flags) ->
				string__first_char(Ostring, '0', SS),
				Pfix_len = 1
			;
				Ostring = SS,
				Pfix_len = 0
			)
		),
		( I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conversion = 'x' ,
		Poly_t = i(I),
		( I = 0 ->
			SS = "0",
			Pfix_len = 0,
			string__format_int_precision(SS, Ostring, Precision, _)
		;
			string__int_to_base_string(I, 16, S0),
			string__to_lower(S0, S),
			string__format_int_precision(S, SS, Precision, _),
			(
				list__member('#', Flags)
			->
				string__append("0x", SS, Ostring),
				Pfix_len = 2
			;
				Ostring = SS,
				Pfix_len = 0
			)
		),
		( I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conversion = 'X',
		Poly_t = i(I),
		( I = 0 ->
			SS = "0",
			Pfix_len = 0,
			string__format_int_precision(SS, Ostring, Precision, _)
		;
			string__int_to_base_string(I, 16, Otemp),
			string__to_upper(Otemp, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member('#', Flags) ->
				string__append("0X", SS, Ostring),
				Pfix_len = 2
			;
				SS = Ostring,
				Pfix_len = 0
			)
		),
		( I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conversion = 'u' ,
		Poly_t = i(I),
		int__abs(I, J),
		string__int_to_string(J, S),
		string__format_int_precision(S, Ostring, Precision, Mvt),
		Mv_width = Mvt
	;
	Conversion = 'c' ,
		Poly_t = c(C),
		string__char_to_string(C, Ostring),
		Mv_width = 0
	;
	Conversion = 's' ,
		Poly_t = s(S),
		( Precision = yes(Prec) ->
			string__split(S, Prec, Ostring, _)
		;
			S = Ostring
		),
		Mv_width = 0
	;
	Conversion = 'f' ,
		Poly_t = f(F),
		string__float_to_f_string(F, Fstring),
		string__format_calc_prec(Fstring, Ostring, Precision),
		(F < 0.0 -> Mv_width = 1 ; Mv_width = 0)
	;
	Conversion = 'e',
		Poly_t = f(F),
		string__format_calc_exp(F, Ostring, Precision, 0),
		(F < 0.0 -> Mv_width = 1 ; Mv_width = 0)
	;
	Conversion = 'E' ,
		Poly_t = f(F),
		string__format_calc_exp(F, Otemp, Precision, 0),
		string__to_upper(Otemp, Ostring),
		(F < 0.0 -> Mv_width = 1 ; Mv_width = 0)
	;
	Conversion = 'p' ,
		Poly_t = i(I),
		string__int_to_string(I, Ostring),
		((I < 0) -> Mv_width = 1 ; Mv_width = 0)
	).

:- pred string__do_conversion_fail(char).
:- mode string__do_conversion_fail(in) is erroneous.
string__do_conversion_fail(Conversion) :-
	string__format("%s `%%%c', without a correct poly-variable.", 
		[s("string__format: statement has used type"), c(Conversion)],
		Error_message),
	error(Error_message).

%
%	Use precision information to modify string.  - for integers
%
:- pred string__format_int_precision(string, string, maybe(int), int).
:- mode string__format_int_precision(in, out, in, out) is semidet.
string__format_int_precision(S, Ostring, Precision, Added_width) :-
	( Precision = yes(Prec0) ->
		Prec = Prec0
	;
		Prec = 0
	),
	string__length(S, L),
	( string__first_char(S, '-', _) ->
		Xzeros is Prec - L + 1
	;
		Xzeros is Prec - L
	),
	Added_width = Xzeros,
	( Xzeros > 0 ->
		string__duplicate_char('0', Xzeros, Pfix),
		string__first_char(S, C, Rest),
		(
			C \= ('-'),
			C \= ('+')
		->
			string__append(Pfix, S, Ostring)
		;
			string__append(Pfix, Rest, Temps),
			string__first_char(Ostring, C, Temps)
		)
	;
		Ostring = S
	).

%	Function  to calculate exponent for a %e conversion of a float
%
:- pred string__format_calc_exp(float, string, maybe(int), int).
:- mode string__format_calc_exp(in, out, in, in) is det.
string__format_calc_exp(F, Fstring, Precision, Exp) :-
	( F < 0.0 -> 
		Tf is 0.0 - F,
		string__format_calc_exp(Tf, Tst, Precision, Exp),
		string__first_char(Fstring, '-', Tst)
	; F > 0.0, F < 1.0 ->
		Texp is Exp - 1,
		FF is 10.0 * F,
		string__format_calc_exp(FF, Fstring, Precision, Texp)
	; F >= 10.0 ->
		Texp is Exp + 1,
		FF is F / 10.0,
		string__format_calc_exp(FF, Fstring, Precision, Texp)
	;
		string__float_to_f_string(F, Fs),
		string__format_calc_prec(Fs, Fs2, Precision),
		string__int_to_string(Exp, Exps),
		( Exp < 0 ->
			string__append("e", Exps, TFstring),
			string__append(Fs2, TFstring, Fstring)
		;
			string__append("e+", Exps, TFstring),
			string__append(Fs2, TFstring, Fstring)
		)
	).

%
%	This precision output-modification predicate handles floats.
%
:- pred string__format_calc_prec(string, string, maybe(int)).
:- mode string__format_calc_prec(in, out, in) is det.
string__format_calc_prec(Istring0, Ostring, Precision) :-
	(
		Precision = yes(Prec0)
	->
		Prec = Prec0
	;
		Prec = 15
	),
	(
		string__find_index(Istring0, '.', Index)
	->
		TargetLength1 is Prec + Index,
		Istring1 = Istring0
	;
		string__length(Istring0, TargetLength0),
		TargetLength1 is TargetLength0 + 1 + Prec,
		string__append(Istring0, ".", Istring1)

		%  This branch should never be called if mercury is implemented
		%  in ansi-C, according to Kernighan and Ritchie p244, as a 
		%  float converted to a string using sprintf should always have
		%  a decimal point.  (where specified precision != 0.  
		%  string__float_to_string doesn't specify a precision to be
		%  used.)  
		%
		%  Unfortunately, this branch is called.
		%  Often.
	),
	(
		Prec = 0
	->
			%  Forget the '.'.
		TargetLength is TargetLength1 - 1
	;
		TargetLength = TargetLength1
	),
	(
		string__length(Istring1, Length),
		Length < TargetLength
	->
			%  Ensure that there are "enough" chars in Istring.
		string__duplicate_char('0', Prec, Suffix),
		string__append(Istring1, Suffix, Istring)
	;
		Istring = Istring1
	),
	string__split(Istring, TargetLength, Ostring, _).

%	string__find_index is a funky little predicate to find the first
%	occurrence of a particular character in a string.

:- pred string__find_index(string, char, int).
:- mode string__find_index(in, in, out) is semidet.
string__find_index(Str, C, Index) :-
	string__to_char_list(Str, List),
	string__find_index_2(List, C, Index).

:- pred string__find_index_2(list(char), char, int).
:- mode string__find_index_2(in, in, out) is semidet.
string__find_index_2([], _C, _Index) :- fail.
string__find_index_2([X|Xs], C, Index) :-
	(
		X = C
	->
		Index = 1
	;
		string__find_index_2(Xs, C, Index0),
		Index is Index0 + 1
	).

%string__find_index(A, Ch, Check, Ret) :-
%	(
%		string__length(A, Len),
%		Len < Check
%	->
%		fail
%	;
%		string__index(A, Check, Ch)
%	->
%		Ret = Check
%	;
%		Check2 is Check + 1,
%		string__find_index(A, Ch, Check2, Ret)
%	).
%

%	Add a '+' or ' ' sign, if it is needed in this output.
%
:- pred string__format_add_sign(string, list(char), string__poly_type,
			int, int, string).
:- mode string__format_add_sign(in, in, in, in, out, out) is det.
%			Mvw is the prefix-length in front of the number.
string__format_add_sign(Istring, Flags, _V, MoveWidth0, Movewidth, Ostring) :-
	MoveWidth1 is MoveWidth0 - 1,
	(
		string__index(Istring, MoveWidth1, '-')
	->
		Ostring = Istring,
		Movewidth = MoveWidth0
	;
		string__split(Istring, MoveWidth0, Lstring, Rstring),
		(
			list__member(('+'), Flags)
		->
			string__append("+", Rstring, Astring),
			string__append(Lstring, Astring, Ostring),
			Movewidth is MoveWidth0 + 1
		;
			list__member(' ', Flags)
		->
			string__append(" ", Rstring, Astring),
			string__append(Lstring, Astring, Ostring),
			Movewidth is MoveWidth0 + 1
		; 
			Ostring = Istring,
			Movewidth = MoveWidth0
		)
	).

%
% This function pads some characters to the left or right of a string that is
% shorter than it's width.
%
:- pred string__format_pad_width(string, maybe(int), list(char), int,  string).
:- mode string__format_pad_width(in, in, in, in, out) is det.
%		(String in, width, flags, #Moveables, Output string).
string__format_pad_width(Istring, Width0, Flags, Mv_cs, Out_string) :-
	string__length(Istring, Len),
	(
		Width0 = yes(Width),
		Len < Width
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
			string__append(Pad_string, After, Astring),
			string__append(B4, Astring, Out_string)
		;
			string__append(Pad_string, Istring, Out_string)
		)
	;
		Out_string = Istring
	).

:- pred string__format_get_optional_args(list(char), list(char), int, int,
			char).
:- mode string__format_get_optional_args(in, out, out, out, out) is det.
%	string__format_get_optional_args(format info, flags, width, precision, modifier)
%		format is assumed to be in ANSI C format.
%		p243-4 of Kernighan & Ritchie 2nd Ed. 1988
%		"Parse" format informtion.
%
% A function to do some basic parsing on the optional printf arguments.
%
% The ites make this det.  It would be nicer to see a det switch on A, but the
% determinism checker does not `see' the equity tests that are hidden one layer
% further down.
%
string__format_get_optional_args([], Flags, Width, Precision, Mods) :-
		Flags = [],
		Width = 0,
		string__default_precision_and_width(Precision),
		Mods = ' '.
string__format_get_optional_args([A|As], Flags, Width, Precision, Mods) :-
	(
		(A = (-) ; A = (+) ; A = ' ' ; A = '0' ; A = '#' )
	->
		string__format_get_optional_args(As, Oflags, Width, Precision,
				Mods),
		UFlags = [A | Oflags],
		list__sort_and_remove_dups(UFlags, Flags)
	;
	(
		( A = (.) ; A = '1' ; A = '2' ; A = '3' ; A = '4' ;
		  A = '5' ; A = '6' ; A = '7' ; A = '8' ; A = '9' )
	->
		string__format_string_to_ints([A|As], Bs, Numl1, Numl2, yes),
		string__format_int_from_char_list(Numl1, Width),
		string__format_int_from_char_list(Numl2, Prec),
		string__format_get_optional_args(Bs, Flags, _, Ptemp, Mods),
		(Numl2 = [] ->
			Precision = Ptemp
		;
			Precision = Prec
		)
	;
	(	( A = 'h' ; A = 'l' ; A = 'L' )
	->
		Mods = A,
		string__format_get_optional_args(As, Flags, Width, 
			Precision, _)
	;
	(	A = ('*')
	->
		string__format_get_optional_args(As, Flags, W, P, Mods),
		(
			As = [(.)|_]
		->
			Precision = P, 
			string__special_precision_and_width(Width)
		;
			Width = W,
			string__special_precision_and_width(Precision)
		)
%		(
%			string__default_precision_and_width(P)
%		->
%			string__special_precision_and_width(Precision)
%		; 
%			Precision = P
%		),
%		string__special_precision_and_width(Width)
	;
		error("string__format:  Unrecognised formatting information\n")
		)
	))) .

:- pred string__format_takewhile1(list(char), list(char), list(char)).
:- mode string__format_takewhile1(in, out, out) is det.
%	string__format_takewhile(formatted string in, out, format info).
%		A HACK.  Would be much nicer with a proper string__takewhile.
%		Looses the format info from the front of the first argument,
%		puts this in the last argument, while the second is the
%		remainder of the string.
%
%		XXXXXX
%
string__format_takewhile1([], [], []).
string__format_takewhile1([A|As], Rem, Finf) :-
	(
		( A = 'd' ; A = 'i' ; A = 'o' ; A = 'x' ; A = 'X' ; A = 'u' ;
		  A = 's' ; A = '%' ; A = 'c' ; A = 'f' ; A = 'e' ; A = 'E' ;
		  A = 'g' ; A = 'G' ; A = 'p')
	->
		Rem = [A|As],
		Finf = []
	;
		string__format_takewhile1(As, Rem, F),
		Finf = [A|F]
	).

:- pred string__format_string_to_ints(list(char), list(char), list(char),
		list(char), bool).
:- mode string__format_string_to_ints(in, out, out, out, in) is det.
% 			(String in, out, Number1, Number2, seen '.' yet?)
%		Takes in a char list and splits off the rational number at the 
%		start of the list.  This is split into 2 parts - an int and a
%		fraction.
%
string__format_string_to_ints([], [], [], [], _).
string__format_string_to_ints([A|As], Bs, Int1, Int2, Bool) :-
	(char__is_digit(A) ->
		( Bool = yes ->
			string__format_string_to_ints(As, Bs, I1, Int2, yes),
			Int1 = [A|I1]
		;
			string__format_string_to_ints(As, Bs, Int1, I2, no),
			Int2 = [A|I2]
		)
	;
		( A = ('.') ->
			string__format_string_to_ints(As, Bs, Int1, Int2, no)
		;
			Bs = [A|As],
			Int1 = [],
			Int2 = []
		)
	).

:- pred string__format_int_from_char_list(list(char), int).
:- mode string__format_int_from_char_list(in, out) is det.
%		Convert a char_list to an int
%
string__format_int_from_char_list([], 0).
string__format_int_from_char_list([L|Ls], I) :-
	(
		string__from_char_list([L|Ls], S),
		string__to_int(S, I_0)
	->
		I = I_0
	;
		I = 0
	).

:- pred string__default_precision_and_width(int).
:- mode string__default_precision_and_width(out) is det.
string__default_precision_and_width(-15).

:- pred string__special_precision_and_width(int).
:- mode string__special_precision_and_width(out) is det.
string__special_precision_and_width(-1).

%-----------------------------------------------------------------------------%

% The remaining routines are implemented using the C interface.

:- pragma(c_header_code, "
#include <string.h>
#include <stdio.h>
").

%-----------------------------------------------------------------------------%

:- pragma(c_code, string__float_to_string(FloatVal::in, FloatString::out), "{
	char buf[500];
	Word tmp;
	sprintf(buf, ""%#.15g"", FloatVal);
	incr_hp_atomic(tmp, (strlen(buf) + sizeof(Word)) / sizeof(Word));
	FloatString = (char *)tmp;
	strcpy(FloatString, buf);
}").

	% Beware that the implementation of string__format depends
	% on the details of what string__float_to_f_string/2 outputs.

:- pred string__float_to_f_string(float::in, string::out) is det.

:- pragma(c_code, string__float_to_f_string(FloatVal::in, FloatString::out), "{
	char buf[500];
	Word tmp;
	sprintf(buf, ""%.15f"", FloatVal);
	incr_hp_atomic(tmp, (strlen(buf) + sizeof(Word)) / sizeof(Word));
	FloatString = (char *)tmp;
	strcpy(FloatString, buf);
}").

:- pragma(c_code, string__to_float(FloatString::in, FloatVal::out), "{
	/* use a temporary, since we can't don't know whether FloatVal
	   is a double or float */
	double tmp;
	SUCCESS_INDICATOR = (sscanf(FloatString, ""%lf"", &tmp) == 1);
		/* TRUE if sscanf succeeds, FALSE otherwise */
	FloatVal = tmp;
}").

/*-----------------------------------------------------------------------*/

/*
:- pred string__to_int_list(string, list(int)).
:- mode string__to_int_list(in, out) is det.
:- mode string__to_int_list(out, in) is det.
*/

:- pragma(c_code, string__to_int_list(Str::in, IntList::out), "{
	const char *p = Str + strlen(Str);
	IntList = list_empty();
	while (p > Str) {
		p--;
		IntList = list_cons((UnsignedChar) *p, IntList);
	}
}").

:- pragma(c_code, string__to_int_list(Str::out, IntList::in), "{
		/* mode (out, in) is det */
	Word int_list_ptr;
	size_t size;
	Word str_ptr;
/*
** loop to calculate list length + sizeof(Word) in `size' using list in
** `int_list_ptr'
*/
	size = sizeof(Word);
	int_list_ptr = IntList;
	while (!list_is_empty(int_list_ptr)) {
		size++;
		int_list_ptr = list_tail(int_list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(Word) - 1) / sizeof(Word) words
*/
	incr_hp_atomic(str_ptr, size / sizeof(Word));
	Str = (char *) str_ptr;
/*
** loop to copy the characters from the int_list to the string
*/
	size = 0;
	int_list_ptr = IntList;
	while (!list_is_empty(int_list_ptr)) {
		Str[size++] = list_head(int_list_ptr);
		int_list_ptr = list_tail(int_list_ptr);
	}
/*
** null terminate the string
*/
	Str[size] = '\\0';
}").

/*-----------------------------------------------------------------------*/

/*
:- pred string__contains_char(string, char).
:- mode string__contains_char(in, in) is semidet.
*/
:- pragma(c_code, string__contains_char(Str::in, Ch::in), "
	SUCCESS_INDICATOR = (strchr(Str, Ch) != NULL);
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__index(string, int, char).
:- mode string__index(in, in, out) is semidet.
*/
:- pragma(c_code, string__index(Str::in, Index::in, Ch::out), "
	if ((Word) Index >= strlen(Str)) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		SUCCESS_INDICATOR = TRUE;
		Ch = Str[Index];
	}
").

/*-----------------------------------------------------------------------*/

:- pred string__unsafe_index(string, int, char).
:- mode string__unsafe_index(in, in, out) is det.

:- pragma(c_code, string__unsafe_index(Str::in, Index::in, Ch::out), "
	Ch = Str[Index];
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__length(string, int).
:- mode string__length(in, out) is det.
*/
:- pragma(c_code, string__length(Str::in, Length::out), "
	Length = strlen(Str);
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__append(string, string, string).
:- mode string__append(in, in, in) is semidet.	% implied
:- mode string__append(in, out, in) is semidet.
:- mode string__append(in, in, out) is det.
:- mode string__append(out, out, in) is multidet.
*/

/*
:- mode string__append(in, in, in) is semidet.
*/
:- pragma(c_code, string__append(S1::in, S2::in, S3::in), "{
	size_t len_1 = strlen(S1);
	SUCCESS_INDICATOR = (
		strncmp(S1, S3, len_1) == 0 &&
		strcmp(S2, S3 + len_1) == 0
	);
}").

/*
:- mode string__append(in, out, in) is semidet.
*/
:- pragma(c_code, string__append(S1::in, S2::out, S3::in), "{
	Word tmp;
	size_t len_1, len_2, len_3;

	len_1 = strlen(S1);
	if (strncmp(S1, S3, len_1) != 0) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		len_3 = strlen(S3);
		len_2 = len_3 - len_1;
		incr_hp_atomic(tmp, (len_2 + sizeof(Word)) / sizeof(Word));
		S2 = (char *) tmp;
		strcpy(S2, S3 + len_1);
		SUCCESS_INDICATOR = TRUE;
	}
}").

/*
:- mode string__append(in, in, out) is det.
*/
:- pragma(c_code, string__append(S1::in, S2::in, S3::out), "{
	size_t len_1, len_2;
	Word tmp;
	len_1 = strlen(S1);
	len_2 = strlen(S2);
	incr_hp_atomic(tmp, (len_1 + len_2 + sizeof(Word)) / sizeof(Word));
	S3 = (char *) tmp;
	strcpy(S3, S1);
	strcpy(S3 + len_1, S2);
}").

:- pragma(c_code, "

#ifdef	COMPACT_ARGS
#define	string__append_ooi_input_reg	r1
#else
#define	string__append_ooi_input_reg	r3
#endif

Define_extern_entry(mercury__string__append_3_3_xx);
Declare_label(mercury__string__append_3_3_xx_i1);

BEGIN_MODULE(string_append_module)
	init_entry(mercury__string__append_3_3_xx);
	init_label(mercury__string__append_3_3_xx_i1);
BEGIN_CODE
Define_entry(mercury__string__append_3_3_xx);
	mkframe(""string__append/3"", 4,
		LABEL(mercury__string__append_3_3_xx_i1));
	mark_hp(framevar(0));
	framevar(1) = string__append_ooi_input_reg;
	framevar(2) = strlen((char *) string__append_ooi_input_reg);
	framevar(3) = 0;
Define_label(mercury__string__append_3_3_xx_i1);
{
	String	s3;
	size_t	s3_len;
	size_t	count;

	restore_hp(framevar(0));
	s3 = (String) framevar(1);
	s3_len = framevar(2);
	count = framevar(3);
	if (count > s3_len) {
		/* modframe(ENTRY(do_fail)); */
		fail();
	}
	incr_hp_atomic(r1, (count + sizeof(Word)) / sizeof(Word));
	memcpy((char *) r1, s3, count);
	((char *) r1)[count] = '\\0';
	incr_hp_atomic(r2, (s3_len - count + sizeof(Word)) / sizeof(Word));
	strcpy((char *) r2, s3 + count);
	framevar(3) = count + 1;
	succeed();
}
END_MODULE

#undef	string__append_ooi_input_reg

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_string_append_module
*/
/* suppress gcc -Wmissing-decl warning */
void sys_init_string_append_module(void);

void sys_init_string_append_module(void) {
	extern ModuleFunc string_append_module;
	string_append_module();
}

").

% :- mode string__append(out, out, in) is multidet.
:- pragma(c_code, string__append(S1::out, S2::out, S3::in), "
	/*
	** The pragma_c_code will generate a mkframe();
	** we need to pop off that frame before jumping to the hand-coded
	** fragment above.
	**
	** We mention S1, S2 and S3 here to shut up a warning.
	*/

	maxfr = curprevfr;
	curfr = cursuccfr;
	{
		Declare_entry(mercury__string__append_3_3_xx);
		GOTO(ENTRY(mercury__string__append_3_3_xx));
	}
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__split(string, int, string, string).
:- mode string__split(in, in, out, out) is det.
%	string__split(String, Count, LeftSubstring, RightSubstring):
%	`LeftSubstring' is the left-most `Count' characters of `String',
%	and `RightSubstring' is the remainder of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)
*/

:- pragma(c_code, string__split(Str::in, Count::in, Left::out, Right::out), "{
	Integer len;
	Word tmp;
	if (Count <= 0) {
		make_aligned_string(LVALUE_CAST(ConstString, Left), """");
		Right = Str;
	} else {
		len = strlen(Str);
		if (Count > len) Count = len;
		incr_hp_atomic(tmp, (Count + sizeof(Word)) / sizeof(Word));
		Left = (char *) tmp;
		memcpy(Left, Str, Count);
		Left[Count] = '\\0';
		incr_hp_atomic(tmp,
			(len - Count + sizeof(Word)) / sizeof(Word));
		Right = (char *) tmp;
		strcpy(Right, Str + Count);
	}
}").

/*-----------------------------------------------------------------------*/

/*
:- pred string__first_char(string, char, string).
:- mode string__first_char(in, in, in) is semidet.	% implied
:- mode string__first_char(in, out, in) is semidet.	% implied
:- mode string__first_char(in, in, out) is semidet.	% implied
:- mode string__first_char(in, out, out) is semidet.
:- mode string__first_char(out, in, in) is det.
%	string__first_char(String, Char, Rest) is true iff
%		Char is the first character of String, and Rest is the
%		remainder.
*/

/*
:- mode string__first_char(in, in, in) is semidet.	% implied
*/
:- pragma(c_code, string__first_char(Str::in, First::in, Rest::in), "
	SUCCESS_INDICATOR = (
		Str[0] == First &&
		First != '\\0' &&
		strcmp(Str + 1, Rest) == 0
	);
").

/*
:- mode string__first_char(in, out, in) is semidet.	% implied
*/
:- pragma(c_code, string__first_char(Str::in, First::out, Rest::in), "
	First = Str[0];
	SUCCESS_INDICATOR = (First != '\\0' && strcmp(Str + 1, Rest) == 0);
").

/*
:- mode string__first_char(in, in, out) is semidet.	% implied
*/
:- pragma(c_code, string__first_char(Str::in, First::in, Rest::out), "{
	Word tmp;
	if (Str[0] != First || First == '\\0') {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str++;
		incr_hp_atomic(tmp,
			(strlen(Str) + sizeof(Word)) / sizeof(Word));
		Rest = (char *) tmp;
		strcpy(Rest, Str);
		SUCCESS_INDICATOR = TRUE;
	}
}").

/*
:- mode string__first_char(in, out, out) is semidet.
*/
:- pragma(c_code, string__first_char(Str::in, First::out, Rest::out), "{
	Word tmp;
	First = Str[0];
	if (First == '\\0') {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str++;
		incr_hp_atomic(tmp,
			(strlen(Str) + sizeof(Word)) / sizeof(Word));
		Rest = (char *) tmp;
		strcpy(Rest, Str);
		SUCCESS_INDICATOR = TRUE;
	}
}").

/*
:- mode string__first_char(out, in, in) is det.
*/
:- pragma(c_code, string__first_char(Str::out, First::in, Rest::in), "{
	size_t len = strlen(Rest) + 1;
	Word tmp;
	incr_hp_atomic(tmp, (len + sizeof(Word)) / sizeof(Word));
	Str = (char *) tmp;
	Str[0] = First;
	strcpy(Str + 1, Rest);
}").

:- end_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
