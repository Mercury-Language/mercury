%---------------------------------------------------------------------------%
% Copyright (C) 1993-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module string.

% Main authors: fjh, petdr.
% Stability: medium to high.

% This modules provides basic string handling facilities.

% Note that in the current implementation, strings are represented as in C,
% using a null character as the string terminator.  Future implementations,
% however, might allow null characters in strings.  Programmers should
% avoid creating strings that might contain null characters.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module list, char.

:- func string__length(string) = int.
:- mode string__length(in) = uo is det.
:- pred string__length(string, int).
:- mode string__length(in, uo) is det.
:- mode string__length(ui, uo) is det.
	% Determine the length of a string.
	% An empty string has length zero.

:- func string__append(string, string) = string.
:- mode string__append(in, in) = uo is det.

:- pred string__append(string, string, string).
:- mode string__append(in, in, in) is semidet.	% implied
:- mode string__append(in, out, in) is semidet.
:- mode string__append(in, in, uo) is det.
:- mode string__append(out, out, in) is multi.
%	Append two strings together.
%
%       The following mode is semidet in the sense that it doesn't
%       succeed more than once - but it does create a choice-point,
%       which means it's inefficient and that the compiler can't deduce
%       that it is semidet.  Use string__remove_suffix instead.
% :- mode string__append(out, in, in) is semidet.

:- func string ++ string = string.
:- mode in ++ in = uo is det.
%	S1 ++ S2 = S :- string__append(S1, S2, S).
%
%	Nicer syntax.

:- pred string__remove_suffix(string, string, string).
:- mode string__remove_suffix(in, in, out) is semidet.
%	string__remove_suffix(String, Suffix, Prefix):
%       The same as string__append(Prefix, Suffix, String) except that
%       this is semidet whereas string__append(out, in, in) is nondet.

:- pred string__prefix(string, string).
:- mode string__prefix(in, in) is semidet.
:- mode string__prefix(in, out) is multi.
	% string__prefix(String, Prefix) is true iff Prefix is a
	% prefix of String.  Same as string__append(Prefix, _, String).

:- func string__char_to_string(char) = string.
:- pred string__char_to_string(char, string).
:- mode string__char_to_string(in, out) is det.
:- mode string__char_to_string(out, in) is semidet.
%	string__char_to_string(Char, String).
%		Converts a character (single-character atom) to a string
%		or vice versa.

:- func string__int_to_string(int) = string.
:- pred string__int_to_string(int, string).
:- mode string__int_to_string(in, out) is det.
%	Convert an integer to a string.

:- func string__int_to_base_string(int, int) = string.
:- pred string__int_to_base_string(int, int, string).
:- mode string__int_to_base_string(in, in, out) is det.
%	string__int_to_base_string(Int, Base, String):
%	Convert an integer to a string in a given Base (between 2 and 36).

:- func string__float_to_string(float) = string.
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
%
%		WARNING: string__first_char makes a copy of Rest
%		because the garbage collector doesn't handle references
%		into the middle of an object.
%		Repeated use of string__first_char to iterate
%		over a string will result in very poor performance.
%		Use string__foldl or string__to_char_list instead.

:- pred string__replace(string, string, string, string).
:- mode string__replace(in, in, in, out) is semidet.
% string__replace(String0, Search, Replace, String):
% 	string__replace replaces the first occurence of the second string in 
% 	the first string with the third string to give the fourth string.
% 	It fails if the second string does not occur in the first.

:- func string__replace_all(string, string, string) = string.
:- pred string__replace_all(string, string, string, string).
:- mode string__replace_all(in, in, in, out) is det.
% string__replace_all(String0, Search, Replace, String):
% 	string__replace_all replaces any occurences of the second string in 
% 	the first string with the third string to give the fourth string.

:- func string__to_lower(string) = string.
:- pred string__to_lower(string, string).
:- mode string__to_lower(in, out) is det.
:- mode string__to_lower(in, in) is semidet.		% implied
%	Converts a string to lowercase.

:- func string__to_upper(string) = string.
:- pred string__to_upper(string, string).
:- mode string__to_upper(in, out) is det.
:- mode string__to_upper(in, in) is semidet.		% implied
%	Converts a string to uppercase.

:- func string__capitalize_first(string) = string.
:- pred string__capitalize_first(string, string).
:- mode string__capitalize_first(in, out) is det.
%	Convert the first character (if any) of a string to uppercase.

:- func string__uncapitalize_first(string) = string.
:- pred string__uncapitalize_first(string, string).
:- mode string__uncapitalize_first(in, out) is det.
%	Convert the first character (if any) of a string to lowercase.

:- func string__to_char_list(string) = list(char).
:- pred string__to_char_list(string, list(char)).
:- mode string__to_char_list(in, out) is det.
:- mode string__to_char_list(out, in) is det.

:- func string__from_char_list(list(char)) = string.
:- pred string__from_char_list(list(char), string).
:- mode string__from_char_list(in, out) is det.
:- mode string__from_char_list(out, in) is det.

:- func string__from_rev_char_list(list(char)) = string.
:- pred string__from_rev_char_list(list(char), string).
:- mode string__from_rev_char_list(in, out) is det.
%	Same as string__from_char_list, except that it reverses the order
%	of the characters.

:- func string__det_to_int(string) = int.
%	Converts a signed base 10 string to an int;
%	throws an exception if the string argument
%	does not match the regexp [+-]?[0-9]+

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

:- func string__det_base_string_to_int(int, string) = int.
%	Converts a signed base N string to an int;
%	throws an exception if the string argument
%	is not precisely an optional sign followed
%	by a non-empty string of base N digits.

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

:- func string__pad_left(string, char, int) = string.
:- pred string__pad_left(string, char, int, string).
:- mode string__pad_left(in, in, in, out) is det.
%	string__pad_left(String0, PadChar, Width, String):
%	insert `PadChar's at the left of `String0' until it is at least
%	as long as `Width', giving `String'.

:- func string__pad_right(string, char, int) = string.
:- pred string__pad_right(string, char, int, string).
:- mode string__pad_right(in, in, in, out) is det.
%	string__pad_right(String0, PadChar, Width, String):
%	insert `PadChar's at the right of `String0' until it is at least
%	as long as `Width', giving `String'.

:- func string__duplicate_char(char, int) = string.
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

:- func string__index_det(string, int) = char.
:- pred string__index_det(string, int, char).
:- mode string__index_det(in, in, out) is det.
%	string__index_det(String, Index, Char):
%	`Char' is the (`Index' + 1)-th character of `String'.
%	Calls error/1 if `Index' is out of range (negative, or greater than or
%	equal to the length of `String').

:- func string__unsafe_index(string, int) = char.
:- pred string__unsafe_index(string, int, char).
:- mode string__unsafe_index(in, in, out) is det.
%	string__unsafe_index(String, Index, Char):
%	`Char' is the (`Index' + 1)-th character of `String'.
%	WARNING: behavior is UNDEFINED if `Index' is out of range
%	(negative, or greater than or equal to the length of `String').
%	This version is constant time, whereas string__index_det
%	may be linear in the length of the string.
%	Use with care!

:- pred string__set_char(char, int, string, string).
:- mode string__set_char(in, in, in, out) is semidet.
:- mode string__set_char(in, in, di, uo) is semidet.
%	string__set_char(Char, Index, String0, String):
%	`String' is `String0' with the (`Index' + 1)-th character
%	set to `Char'.
%	Fails if `Index' is out of range (negative, or greater than or
%	equal to the length of `String0').

:- func string__set_char_det(char, int, string) = string.
:- pred string__set_char_det(char, int, string, string).
:- mode string__set_char_det(in, in, in, out) is det.
:- mode string__set_char_det(in, in, di, uo) is det.
%	string__set_char_det(Char, Index, String0, String):
%	`String' is `String0' with the (`Index' + 1)-th character
%	set to `Char'.
%	Calls error/1 if `Index' is out of range (negative, or greater than or
%	equal to the length of `String0').

:- func string__unsafe_set_char(char, int, string) = string.
:- mode string__unsafe_set_char(in, in, in) = out is det.
:- mode string__unsafe_set_char(in, in, di) = uo is det.
:- pred string__unsafe_set_char(char, int, string, string).
:- mode string__unsafe_set_char(in, in, in, out) is det.
:- mode string__unsafe_set_char(in, in, di, uo) is det.
%	string__unsafe_set_char(Char, Index, String0, String):
%	`String' is `String0' with the (`Index' + 1)-th character
%	set to `Char'.
%	WARNING: behavior is UNDEFINED if `Index' is out of range
%	(negative, or greater than or equal to the length of `String0').
%	This version is constant time, whereas string__set_char_det
%	may be linear in the length of the string.
%	Use with care!

:- func string__foldl(func(char, T) = T, string, T) = T.
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

:- func string__words(pred(char), string) = list(string).
:- mode string__words(pred(in) is semidet, in) = out is det.
%	string__words(SepP, String) returns the list of
%	non-empty substrings of String (in first to last
%	order) that are delimited by non-empty sequences
%	of chars matched by SepP.  For example,
%
%	string__words(char__is_whitespace, " the cat  sat on the  mat") =
%		["the", "cat", "sat", "on", "the", "mat"]

:- pred string__split(string, int, string, string).
:- mode string__split(in, in, out, out) is det.
%	string__split(String, Count, LeftSubstring, RightSubstring):
%	`LeftSubstring' is the left-most `Count' characters of `String',
%	and `RightSubstring' is the remainder of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)

:- func string__left(string, int) = string.
:- pred string__left(string, int, string).
:- mode string__left(in, in, out) is det.
%	string__left(String, Count, LeftSubstring):
%	`LeftSubstring' is the left-most `Count' characters of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)

:- func string__right(string, int) = string.
:- pred string__right(string, int, string).
:- mode string__right(in, in, out) is det.
%	string__right(String, Count, RightSubstring):
%	`RightSubstring' is the right-most `Count' characters of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)

:- func string__substring(string, int, int) = string.
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

:- func string__unsafe_substring(string, int, int) = string.
:- pred string__unsafe_substring(string, int, int, string).
:- mode string__unsafe_substring(in, in, in, out) is det.
%	string__unsafe_substring(String, Start, Count, Substring):
%	`Substring' is first the `Count' characters in what would
%	remain of `String' after the first `Start' characters were
%	removed.
%	WARNING: if `Start' is out of the range [0, length of `String'],
%	or if `Count' is out of the range [0, length of `String' - `Start'],
%	then the behaviour is UNDEFINED.
%	Use with care!
%	This version takes time proportional to the length of the
%	substring, whereas string__substring may take time proportional
%	to the length of the whole string.

:- func string__append_list(list(string)::in) = (string::uo) is det.
:- pred string__append_list(list(string), string).
:- mode string__append_list(in, uo) is det.
%	Append a list of strings together.

:- func string__join_list(string::in, list(string)::in) = (string::uo) is det.
%	string__join_list(Separator, Strings) = JoinedString:
%	Appends together the strings in Strings, putting Separator between
%	adjacent strings. If Strings is the empty list, returns the empty
%	string.

:- func string__hash(string) = int.
:- pred string__hash(string, int).
:- mode string__hash(in, out) is det.
%	Compute a hash value for a string.

:- pred string__sub_string_search(string, string, int).
:- mode string__sub_string_search(in, in, out) is semidet.
%	string__sub_string_search(String, SubString, Index).
%	`Index' is the position in `String' where the first occurrence of
%	`SubString' begins.

:- func string__format(string, list(string__poly_type)) = string.
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
%	The following options available in C are supported: flags [0+-# ],
%	a field width (or *), and a precision (could be a ".*").
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
%		%#.0e, %#.0E now prints a '.' before the 'e'.
%
%		Asking for more precision than a float actually has will
%		result in potentially misleading output.
%
%		Numbers are now rounded by precision value, not
%		truncated as previously.
%
%		The implementation uses the sprintf() function, so the
%		actual output will depend on the C standard library.


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
	string__index(String, 0, Char),
	Len = string__length(String),
	(      if Char = ('-') then
		string__foldl2(accumulate_int(Base), String, 1, Len, 0, N),
		Int = -N
	  else if Char = ('+') then
		string__foldl2(accumulate_int(Base), String, 1, Len, 0, N),
		Int = N
	  else
		string__foldl2(accumulate_int(Base), String, 0, Len, 0, N),
		Int = N
	).

:- pred accumulate_int(int, char, int, int).
:- mode accumulate_int(in, in, in, out) is semidet.

accumulate_int(Base, Char, N, (Base * N) + M) :-
	char__digit_to_int(Char, M),
	M < Base.



string__index_det(String, Int, Char) :-
	( string__index(String, Int, Char0) ->
		Char = Char0
	;
		error("string__index_det: index out of range")
	).

string__set_char_det(Char, Int, String0, String) :-
	( string__set_char(Char, Int, String0, String1) ->
		String = String1
	;
		error("string__set_char_det: index out of range")
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

string__remove_suffix(A, B, C) :-
	string__to_int_list(A, LA),
	string__to_int_list(B, LB),
	string__to_int_list(C, LC),
	list__remove_suffix(LA, LB, LC).

string__prefix(String, Prefix) :-
	string__append(Prefix, _, String).

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
	% Note that in order to handle MININT correctly,
	% we need to do the conversion of the absolute
	% number into digits using negative numbers
	% (we can't use positive numbers, since -MININT overflows)
	(
		N < 0
	->
		string__int_to_base_string_2(N, Base, Str1),
		string__append("-", Str1, Str)
	;
		N1 is 0 - N,
		string__int_to_base_string_2(N1, Base, Str)
	).

:- pred string__int_to_base_string_2(int, int, string).
:- mode string__int_to_base_string_2(in, in, out) is det.

string__int_to_base_string_2(NegN, Base, Str) :-
	(
		NegN > -Base
	->
		N is -NegN,
		char__det_int_to_digit(N, DigitChar),
		string__char_to_string(DigitChar, Str)
	;
		NegN1 is NegN // Base,
		N10 is (NegN1 * Base) - NegN,
		char__det_int_to_digit(N10, DigitChar),
		string__char_to_string(DigitChar, DigitString),
		string__int_to_base_string_2(NegN1, Base, Str1),
		string__append(Str1, DigitString, Str)
	).

string__from_char_list(CharList, Str) :-
	string__to_char_list(Str, CharList).

/*-----------------------------------------------------------------------*/

/*
:- pred string__to_char_list(string, list(char)).
:- mode string__to_char_list(in, out) is det.
:- mode string__to_char_list(out, in) is det.
*/

:- pragma foreign_proc("C", string__to_char_list(Str::in, CharList::out),
		[will_not_call_mercury, thread_safe], "{
	MR_ConstString p = Str + strlen(Str);
	CharList = MR_list_empty_msg(MR_PROC_LABEL);
	while (p > Str) {
		p--;
		CharList = MR_list_cons_msg((MR_UnsignedChar) *p, CharList,
			MR_PROC_LABEL);
	}
}").

:- pragma foreign_proc("C", string__to_char_list(Str::out, CharList::in),
		[will_not_call_mercury, thread_safe], "{
		/* mode (out, in) is det */
	MR_Word char_list_ptr;
	size_t size;
/*
** loop to calculate list length + sizeof(MR_Word) in `size' using list in
** `char_list_ptr'
*/
	size = sizeof(MR_Word);
	char_list_ptr = CharList;
	while (! MR_list_is_empty(char_list_ptr)) {
		size++;
		char_list_ptr = MR_list_tail(char_list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(MR_Word) - 1) / sizeof(MR_Word) words
*/
	MR_allocate_aligned_string_msg(Str, size, MR_PROC_LABEL);

/*
** loop to copy the characters from the char_list to the string
*/
	size = 0;
	char_list_ptr = CharList;
	while (! MR_list_is_empty(char_list_ptr)) {
		Str[size++] = MR_list_head(char_list_ptr);
		char_list_ptr = MR_list_tail(char_list_ptr);
	}
/*
** null terminate the string
*/
	Str[size] = '\\0';
}").

/*-----------------------------------------------------------------------*/

%
% We could implement from_rev_char_list using list__reverse and from_char_list,
% but the optimized implementation in C below is there for efficiency since
% it improves the overall speed of parsing by about 7%.
%
:- pragma foreign_proc("C", string__from_rev_char_list(Chars::in, Str::out),
		[will_not_call_mercury, thread_safe], "
{
	MR_Word list_ptr;
	MR_Word size, len;
/*
** loop to calculate list length + sizeof(MR_Word) in `size' using list in
** `list_ptr' and separately count the length of the string
*/
	size = sizeof(MR_Word);
	len = 1;
	list_ptr = Chars;
	while (!MR_list_is_empty(list_ptr)) {
		size++;
		len++;
		list_ptr = MR_list_tail(list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(MR_Word) - 1) / sizeof(MR_Word) words
*/
	MR_allocate_aligned_string_msg(Str, size, MR_PROC_LABEL);

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
	while (!MR_list_is_empty(list_ptr)) {
		Str[--len] = (MR_Char) MR_list_head(list_ptr);
		list_ptr = MR_list_tail(list_ptr);
	}
}").

:- pragma foreign_proc("MC++", string__to_char_list(Str::in, CharList::out),
		[will_not_call_mercury, thread_safe], "{
        MR_Integer length, i; 
        MR_Word tmp;
        MR_Word prev;

        length = Str->get_Length();
      
        MR_list_nil(prev);

        for (i = length - 1; i >= 0; i--) {
		MR_list_cons(tmp, __box(Str->get_Chars(i)), prev);
		prev = tmp;
        }
        CharList = tmp;
}").

:- pragma foreign_proc("MC++", string__to_char_list(Str::out, CharList::in),
		[will_not_call_mercury, thread_safe], "{
        System::Text::StringBuilder *tmp;
	MR_Char c;
       
        tmp = new System::Text::StringBuilder();
        while (1) {
            if (MR_list_is_cons(CharList)) {
		c = System::Convert::ToChar(MR_list_head(CharList));
                tmp->Append(c);
                CharList = MR_list_tail(CharList);
            } else {
                break;
            }
        }
        Str = tmp->ToString();
}").

string__from_rev_char_list(Chars::in, Str::out) :- 
	Str = string__from_char_list(list__reverse(Chars)).

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



:- pred string__all_match(pred(char), string).
:- mode string__all_match(pred(in) is semidet, in) is semidet.

string__all_match(P, String) :-
	all_match_2(string__length(String) - 1, P, String).

:- pred all_match_2(int, pred(char), string).
:- mode all_match_2(in, pred(in) is semidet, in) is semidet.

string__all_match_2(I, P, String) :-
	( if I >= 0 then
		P(string__unsafe_index(String, I)),
		string__all_match_2(I - 1, P, String)
	  else
	  	true
	).



string__is_alpha(S) :-
	string__all_match(char__is_alpha, S).

string__is_alpha_or_underscore(S) :-
	string__all_match(char__is_alpha_or_underscore, S).

string__is_alnum_or_underscore(S) :-
	string__all_match(char__is_alnum_or_underscore, S).



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
	String = string__from_char_list(list__duplicate(Count, Char)).

%-----------------------------------------------------------------------------%

string__append_list(Lists, string__append_list(Lists)).

	% Implementation of string__append_list that uses C as this
	% minimises the amount of garbage created.
:- pragma foreign_proc("C", string__append_list(Strs::in) = (Str::uo),
		[will_not_call_mercury, thread_safe], "{
	MR_Word	list = Strs;
	MR_Word	tmp;
	size_t	len;

		/* Determine the total length of all strings */
	len = 0;
	while (!MR_list_is_empty(list)) {
		len += strlen((MR_String) MR_list_head(list));
		list = MR_list_tail(list);
	}

		/* Allocate enough word aligned memory for the string */
	MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);

		/* Copy the strings into the new memory */
	len = 0;
	list = Strs;
	while (!MR_list_is_empty(list)) {
		strcpy((MR_String) Str + len, (MR_String) MR_list_head(list));
		len += strlen((MR_String) MR_list_head(list));
		list = MR_list_tail(list);
	}

		/* Set the last character to the null char */
	Str[len] = '\\0';
}").

	% Implementation of string__join_list that uses C as this
	% minimises the amount of garbage created.
:- pragma foreign_proc("C", string__join_list(Sep::in, Strs::in) = (Str::uo),
		[will_not_call_mercury, thread_safe], "{
	MR_Word	list = Strs;
	MR_Word	tmp;
	size_t	len = 0;
	size_t	sep_len;
	bool	add_sep;

	sep_len = strlen(Sep);

		/* Determine the total length of all strings */
	len = 0;
	add_sep = FALSE;
	while (!MR_list_is_empty(list)) {
		if (add_sep) {
			len += sep_len;
		}
		
		len += strlen((MR_String) MR_list_head(list));
		list = MR_list_tail(list);
		add_sep = TRUE;
	}

	MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);

		/* Copy the strings into the new memory */
	len = 0;
	list = Strs;
	add_sep = FALSE;
	while (!MR_list_is_empty(list)) {
		if (add_sep) {
			strcpy((MR_String) Str + len, Sep);
			len += sep_len;
		}

		strcpy((MR_String) Str + len, (MR_String) MR_list_head(list));
		len += strlen((MR_String) MR_list_head(list));
		list = MR_list_tail(list);
		add_sep = TRUE;
	}

		/* Set the last character to the null char */
	Str[len] = '\\0';
}").

:- pragma foreign_proc("C#",
		string__append_list(Strs::in) = (Str::uo),
		[will_not_call_mercury, thread_safe], "
{
        System.Text.StringBuilder tmp = new System.Text.StringBuilder();

	while (mercury.runtime.LowLevelData.list_is_cons(Strs)) {
		tmp.Append(mercury.runtime.LowLevelData.list_get_head(Strs));
		Strs = mercury.runtime.LowLevelData.list_get_tail(Strs);
	}
	Str = tmp.ToString();
}
").

string__append_list(Strs::in) = (Str::uo) :-
	( Strs = [X | Xs] ->
		Str = X ++ append_list(Xs)
	;
		Str = ""
	).

:- pragma foreign_proc("C#",
		string__join_list(Sep::in, Strs::in) = (Str::uo),
		[will_not_call_mercury, thread_safe], "
{	
	System.Text.StringBuilder tmpStr = new System.Text.StringBuilder();

	while(mercury.runtime.LowLevelData.list_is_cons(Strs)) {
		tmpStr.Append(mercury.runtime.LowLevelData.list_get_head(Strs));
		Strs = mercury.runtime.LowLevelData.list_get_tail(Strs);

		if (mercury.runtime.LowLevelData.list_is_cons(Strs)) {
			tmpStr.Append(Sep);
		}
	}

	Str = tmpStr.ToString();
}").

%-----------------------------------------------------------------------------%

	% Note - string__hash is also defined in code/imp.h
	% The two definitions must be kept identical.

string__hash(String, HashVal) :-
	string__length(String, Length),
	string__to_int_list(String, CodeList),
	string__hash_2(CodeList, 0, HashVal0),
	HashVal = HashVal0 `xor` Length.

:- pred string__hash_2(list(int), int, int).
:- mode string__hash_2(in, in, out) is det.

string__hash_2([], HashVal, HashVal).
string__hash_2([X | Xs], HashVal0, HashVal) :-
	string__combine_hash(HashVal0, X, HashVal1),
	string__hash_2(Xs, HashVal1, HashVal).

:- pred string__combine_hash(int, int, int).
:- mode string__combine_hash(in, in, out) is det.

string__combine_hash(H0, X, H) :-
	H1 = H0 << 5,
	H2 = H1 `xor` H0,
	H = H2 `xor` X.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", 
	string__sub_string_search(WholeString::in, SubString::in,
			Index::out) , [will_not_call_mercury, thread_safe],
"{
	char *match;
	match = strstr(WholeString, SubString);
	if (match) {
		Index = match - WholeString;
		SUCCESS_INDICATOR = TRUE;
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}").

:- pragma foreign_proc("MC++", 
	string__sub_string_search(WholeString::in, SubString::in,
			Index::out) , [will_not_call_mercury, thread_safe],
"{
	Index = WholeString->IndexOf(SubString);
}").

%-----------------------------------------------------------------------------%

	% This predicate has been optimised to produce the least memory
	% possible -- memory usage is a significant problem for programs
	% which do a lot of formatted IO.
string__format(FormatString, PolyList, String) :-
	(
		format_string(Specifiers, PolyList, [],
				to_char_list(FormatString), [])
	->
		String = string__append_list(
				list__map(specifier_to_string, Specifiers))
	;
		error("string__format: format string invalid.")
	).

:- type specifier
	--->	conv(
			flags 		:: list(char),
			width		:: maybe(list(char)),
			precision	:: maybe(list(char)),
			spec		:: spec
		)
	;	string(list(char)).

	%
	% A format string is parsed into alternate sections.
	% We alternate between the list of characters which don't
	% represent a conversion specifier and those that do.
	%
:- pred format_string(list(specifier)::out,
		list(string__poly_type)::in, list(string__poly_type)::out,
		list(char)::in, list(char)::out) is det.

format_string(Results, PolyTypes0, PolyTypes) -->
	other(NonConversionSpecChars),
	( conversion_specification(ConversionSpec, PolyTypes0, PolyTypes1) ->
		format_string(Results0, PolyTypes1, PolyTypes),
		{ Results = [string(NonConversionSpecChars),
				ConversionSpec | Results0] }
	;
		{ Results = [string(NonConversionSpecChars)] },
		{ PolyTypes = PolyTypes0 }
	).

	%
	% Parse a string which doesn't contain any conversion
	% specifications.
	%
:- pred other(list(char)::out, list(char)::in, list(char)::out) is det.

other(Result) -->
	( [Char], { Char \= '%' } ->
		other(Result0),
		{ Result = [Char | Result0] }
	;
		{ Result = [] }
	).

	%
	% Each conversion specification is introduced by the character
	% '%',  and ends with a conversion specifier.  In between there
	% may be (in this order)  zero  or more  flags,  an optional
	% minimum field width, and an optional precision.
	%
:- pred conversion_specification(specifier::out,
		list(string__poly_type)::in, list(string__poly_type)::out,
		list(char)::in, list(char)::out) is semidet.

conversion_specification(Specificier, PolyTypes0, PolyTypes) -->
	['%'],
	flags(Flags),
	optional(width, MaybeWidth, PolyTypes0, PolyTypes1),
	optional(prec, MaybePrec, PolyTypes1, PolyTypes2),
	( spec(Spec, PolyTypes2, PolyTypes3) ->
		{ Specificier = conv(Flags, MaybeWidth, MaybePrec, Spec) },
		{ PolyTypes = PolyTypes3 }
	;
		{ error("string__format: invalid conversion specifier.") }
	).
	
:- pred optional(pred(T, U, U, V, V), maybe(T), U, U, V, V).
:- mode optional(pred(out, in, out, in, out) is semidet, out, in, out,
		in, out) is det.

optional(P, MaybeOutput, Init, Final) -->
	( P(Output, Init, Final0) ->
		{ MaybeOutput = yes(Output) },
		{ Final = Final0 }
	;
		{ MaybeOutput = no },
		{ Final = Init }
	).

:- pred flags(list(char)::out, list(char)::in, list(char)::out) is semidet.

flags(Result) -->
	( [Char], { flag(Char) } ->
		flags(Result0),
		{ Result = [Char | Result0] }
	;
		{ Result = [] }
	).

	%
	% Is it a valid flag character?
	%
:- pred flag(char::in) is semidet.

flag('#').
flag('0').
flag('-').
flag(' ').
flag('+').

	%
	% Do we have a minimum field width?
	%
:- pred width(list(char)::out,
		list(string__poly_type)::in, list(string__poly_type)::out,
		list(char)::in, list(char)::out) is semidet.

width(Width, PolyTypes0, PolyTypes) --> 
	( ['*'] ->
		{ PolyTypes0 = [i(Width0) | PolyTypes1] ->
				% XXX maybe better done in C.
			Width = to_char_list(int_to_string(Width0)),
			PolyTypes = PolyTypes1
		;
			error("string__format: `*' width modifer not associated with an integer.")
		}
	;
		=(Init),
		non_zero_digit,
		zero_or_more_occurences(digit),
		=(Final),

		{ list__remove_suffix(Init, Final, Width) },
		{ PolyTypes = PolyTypes0 }
	).

	%
	% Do we have a precision?
	%
:- pred prec(list(char)::out,
		list(string__poly_type)::in, list(string__poly_type)::out,
		list(char)::in, list(char)::out) is semidet.

prec(Prec, PolyTypes0, PolyTypes) --> 
	['.'],
	( ['*'] ->
		{ PolyTypes0 = [i(Prec0) | PolyTypes1] ->
				% XXX Best done in C
			Prec = to_char_list(int_to_string(Prec0)),
			PolyTypes = PolyTypes1
		;
			error("string__format: `*' precision modifer not associated with an integer.")
		}
	;
		=(Init),
		digit,
		zero_or_more_occurences(digit),
		=(Final)
	->
		{ list__remove_suffix(Init, Final, Prec) },
		{ PolyTypes = PolyTypes0 }
	;
			% When no number follows the '.' the precision
			% defaults to 0.
		{ Prec = ['0'] },
		{ PolyTypes = PolyTypes0 }
	).

% NB the capital letter specifiers are proceeded with a 'c'.
:- type spec
		% valid integer specifiers
	--->	d(int)
	;	i(int)
	;	o(int)
	;	u(int)
	;	x(int)
	;	cX(int)
	;	p(int)

		% valid float specifiers
	;	e(float)
	;	cE(float)
	;	f(float)
	;	cF(float)
	;	g(float)
	;	cG(float)

		% valid char specifiers
	;	c(char)

		% valid string specifiers
	;	s(string)

		% specifier representing "%%"
	;	percent
	.
		
	%
	% Do we have a valid conversion specifier?
	% We check to ensure that the specifier also matches the type
	% from the input list.
	%
:- pred spec(spec::out,
		list(string__poly_type)::in, list(string__poly_type)::out,
		list(char)::in, list(char)::out) is semidet.

	% valid integer conversion specifiers
spec(d(Int), [i(Int) | Ps], Ps) --> ['d'].
spec(i(Int), [i(Int) | Ps], Ps) --> ['i'].
spec(o(Int), [i(Int) | Ps], Ps) --> ['o'].
spec(u(Int), [i(Int) | Ps], Ps) --> ['u'].
spec(x(Int), [i(Int) | Ps], Ps) --> ['x'].
spec(cX(Int), [i(Int) | Ps], Ps) --> ['X'].
spec(p(Int), [i(Int) | Ps], Ps) --> ['p'].

	% valid float conversion specifiers
spec(e(Float), [f(Float) | Ps], Ps) --> ['e'].
spec(cE(Float), [f(Float) | Ps], Ps) --> ['E'].
spec(f(Float), [f(Float) | Ps], Ps) --> ['f'].
spec(cF(Float), [f(Float) | Ps], Ps) --> ['F'].
spec(g(Float), [f(Float) | Ps], Ps) --> ['g'].
spec(cG(Float), [f(Float) | Ps], Ps) --> ['G'].

	% valid char conversion specifiers
spec(c(Char), [c(Char) | Ps], Ps) --> ['c'].

	% valid string conversion specifiers
spec(s(Str), [s(Str) | Ps], Ps) --> ['s'].

	% conversion specifier representing the "%" sign
spec(percent, Ps, Ps) --> ['%'].

	% A digit in the range [1-9]
:- pred non_zero_digit(list(char)::in, list(char)::out) is semidet.

non_zero_digit -->
	[ Char ],
	{ char__is_digit(Char) },
	{ Char \= '0' }.

	% A digit in the range [0-9]
:- pred digit(list(char)::in, list(char)::out) is semidet.

digit -->
	[ Char ],
	{ char__is_digit(Char) }.


	% Zero or more occurences of the string parsed by the ho pred.
:- pred zero_or_more_occurences(pred(list(T), list(T)), list(T), list(T)).
:- mode zero_or_more_occurences(pred(in, out) is semidet, in, out) is det.

zero_or_more_occurences(P) -->
	( P ->
		zero_or_more_occurences(P)
	;
		[]
	).

:- func specifier_to_string(specifier) = string. 

specifier_to_string(conv(Flags, Width, Prec, Spec)) = String :-
	(
			% valid int conversion specifiers
		Spec = d(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "d"), Int)
	;
		Spec = i(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "i"), Int)
	;
		Spec = o(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "o"), Int)
	;
		Spec = u(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "u"), Int)
	;
		Spec = x(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "x"), Int)
	;
		Spec = cX(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "X"), Int)
	;
		Spec = p(Int),
		String = format_int(
				make_format(Flags, Width,
					Prec, int_length_modifer, "p"), Int)
	;
			% valid float conversion specifiers
		Spec = e(Float),
		String = format_float(
			make_format(Flags, Width, Prec, "", "e"), Float)
	;
		Spec = cE(Float),
		String = format_float(
			make_format(Flags, Width, Prec, "", "E"), Float)
	;
		Spec = f(Float),
		String = format_float(
			make_format(Flags, Width, Prec, "", "f"), Float)
	;
		Spec = cF(Float),
		String = format_float(
			make_format(Flags, Width, Prec, "", "F"), Float)
	;
		Spec = g(Float),
		String = format_float(
			make_format(Flags, Width, Prec, "", "g"), Float)
	;
		Spec = cG(Float),
		String = format_float(
			make_format(Flags, Width, Prec, "", "G"), Float)
	;
			% valid char conversion Specifiers
		Spec = c(Char),
		String = format_char(
				make_format(Flags, Width, Prec, "", "c"), Char)
	;
			% valid string conversion Spec = ifiers
		Spec = s(Str),
		String = format_string(
				make_format(Flags, Width, Prec, "", "s"), Str)
	;
			% conversion specifier representing the "%" sign
		Spec = percent,
		String = "%"
	).
specifier_to_string(string(Chars)) = from_char_list(Chars).


	% Construct a format string.
:- func make_format(list(char), maybe(list(char)),
		maybe(list(char)), string, string) = string.

make_format(Flags, MaybeWidth, MaybePrec, LengthMod, Spec) = 
	( using_sprintf ->
		make_format_sprintf(Flags, MaybeWidth, MaybePrec, LengthMod,
			Spec)
	;
		make_format_dotnet(Flags, MaybeWidth, MaybePrec, LengthMod,
			Spec)
	).


:- pred using_sprintf is semidet.

:- pragma foreign_proc("C", using_sprintf,
	[will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = TRUE;
").
:- pragma foreign_proc("MC++", using_sprintf,
	[will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = FALSE;
").
		

	% Construct a format string suitable to passing to sprintf.
:- func make_format_sprintf(list(char), maybe(list(char)),
		maybe(list(char)), string, string) = string.

make_format_sprintf(Flags, MaybeWidth, MaybePrec, LengthMod, Spec) = String :-
	(
		MaybeWidth = yes(Width)
	;
		MaybeWidth = no,
		Width = []
	),
	(
		MaybePrec = yes(Prec0),
		Prec = ['.' | Prec0]
	;
		MaybePrec = no,
		Prec = []
	),
	String = string__append_list(["%", from_char_list(Flags),
				from_char_list(Width),
				from_char_list(Prec), LengthMod, Spec]).


	% Construct a format string suitable to passing to .NET's formatting
	% functions.
	% XXX this code is not yet complete.  We need to do a lot more work
	% to make this work perfectly.
:- func make_format_dotnet(list(char), maybe(list(char)),
		maybe(list(char)), string, string) = string.

make_format_dotnet(_Flags, MaybeWidth, MaybePrec, _LengthMod, Spec0) = String :-
	(
		MaybeWidth = yes(Width0),
		Width = [',' | Width0]
	;
		MaybeWidth = no,
		Width = []
	),
	(
		MaybePrec = yes(Prec)
	;
		MaybePrec = no,
		Prec = []
	),
	( 	Spec0 = "i" -> Spec = "d"
	;	Spec0 = "f" -> Spec = "e"
	;	Spec = Spec0
	),
	String = string__append_list([
		"{0", 
		from_char_list(Width),
		":",
		Spec,
		from_char_list(Prec),
%		LengthMod,
%		from_char_list(Flags),
		"}"]).


:- func int_length_modifer = string.
:- pragma foreign_proc("C", 
	int_length_modifer = (LengthModifier::out),
		[will_not_call_mercury, thread_safe], "{
	MR_make_aligned_string(LengthModifier,
		(MR_String) (MR_Word) MR_INTEGER_LENGTH_MODIFIER);
}").

:- pragma foreign_proc("C#", 
	int_length_modifer = (LengthModifier::out),
		[will_not_call_mercury, thread_safe], "{
	LengthModifier = """";
}").


	% Create a string from a float using the format string.
	% Note it is the responsibility of the caller to ensure that the
	% format string is valid.
:- func format_float(string, float) = string.
:- pragma foreign_proc("C",
	format_float(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	MR_save_transient_hp();
	Str = MR_make_string(MR_PROC_LABEL, FormatStr, (double) Val);
	MR_restore_transient_hp();
}").
:- pragma foreign_proc("C#",
	format_float(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	Str = System.String.Format(FormatStr, Val);
}").

	% Create a string from a int using the format string.
	% Note it is the responsibility of the caller to ensure that the
	% format string is valid.
:- func format_int(string, int) = string.
:- pragma foreign_proc("C",
	format_int(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	MR_save_transient_hp();
	Str = MR_make_string(MR_PROC_LABEL, FormatStr, Val);
	MR_restore_transient_hp();
}").
:- pragma foreign_proc("C#",
	format_int(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	Str = System.String.Format(FormatStr, Val);
}").

	% Create a string from a string using the format string.
	% Note it is the responsibility of the caller to ensure that the
	% format string is valid.
:- func format_string(string, string) = string.
:- pragma foreign_proc("C", 
	format_string(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	Str = MR_make_string(MR_PROC_LABEL, FormatStr, Val);
}").
:- pragma foreign_proc("C#", 
	format_string(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	Str = System.String.Format(FormatStr, Val);
}").

	% Create a string from a char using the format string.
	% Note it is the responsibility of the caller to ensure that the
	% format string is valid.
:- func format_char(string, char) = string.
:- pragma foreign_proc("C", 
	format_char(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	MR_save_transient_hp();
	Str = MR_make_string(MR_PROC_LABEL, FormatStr, Val);
	MR_restore_transient_hp();
}").
:- pragma foreign_proc("C#", 
	format_char(FormatStr::in, Val::in) = (Str::out),
		[will_not_call_mercury, thread_safe], "{
	Str = System.String.Format(FormatStr, Val);
}").


%-----------------------------------------------------------------------------%

% The remaining routines are implemented using the C interface.

:- pragma c_header_code("
#include <string.h>
#include <stdio.h>

#include ""mercury_string.h""	/* for MR_allocate_aligned_string*() etc. */
#include ""mercury_tags.h""	/* for MR_list_cons*() */
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	string__float_to_string(FloatVal::in, FloatString::out),
		[will_not_call_mercury, thread_safe], "{
	char buf[500];
	sprintf(buf, ""%#.15g"", FloatVal);
	MR_allocate_aligned_string_msg(FloatString, strlen(buf), MR_PROC_LABEL);
	strcpy(FloatString, buf);
}").

:- pragma foreign_proc("MC++",
	string__float_to_string(FloatVal::in, FloatString::out),
		[will_not_call_mercury, thread_safe], "{
	FloatString = System::Convert::ToString(FloatVal);
}").

	% Beware that the implementation of string__format depends
	% on the details of what string__float_to_f_string/2 outputs.

:- pred string__float_to_f_string(float::in, string::out) is det.

:- pragma foreign_proc("C",
	string__float_to_f_string(FloatVal::in, FloatString::out),
		[will_not_call_mercury, thread_safe], "{
	char buf[500];
	sprintf(buf, ""%.15f"", FloatVal);
	MR_allocate_aligned_string_msg(FloatString, strlen(buf), MR_PROC_LABEL);
	strcpy(FloatString, buf);
}").

:- pragma foreign_proc("C",
	string__to_float(FloatString::in, FloatVal::out),
		[will_not_call_mercury, thread_safe], "{
	/* use a temporary, since we can't don't know whether FloatVal
	   is a double or float */
	double tmp;
	SUCCESS_INDICATOR = (sscanf(FloatString, ""%lf"", &tmp) == 1);
		/* TRUE if sscanf succeeds, FALSE otherwise */
	FloatVal = tmp;
}").

:- pragma foreign_proc("MC++",
	string__float_to_f_string(FloatVal::in, FloatString::out),
		[will_not_call_mercury, thread_safe], "{
	FloatString = System::Convert::ToString(FloatVal);
}").

:- pragma foreign_proc("MC++",
	string__to_float(FloatString::in, FloatVal::out),
		[will_not_call_mercury, thread_safe], "{
	SUCCESS_INDICATOR = TRUE;
	try {
	    FloatVal = System::Convert::ToDouble(FloatString);
	} catch (System::InvalidCastException *e) {
	     SUCCESS_INDICATOR = FALSE;
	}
}").

/*-----------------------------------------------------------------------*/

/*
:- pred string__to_int_list(string, list(int)).
:- mode string__to_int_list(in, out) is det.
:- mode string__to_int_list(out, in) is det.
*/

:- pragma foreign_proc("C",
	string__to_int_list(Str::in, IntList::out),
		[will_not_call_mercury, thread_safe], "{
	MR_ConstString p = Str + strlen(Str);
	IntList = MR_list_empty_msg(MR_PROC_LABEL);
	while (p > Str) {
		p--;
		IntList = MR_list_cons_msg((MR_UnsignedChar) *p, IntList,
			MR_PROC_LABEL);
	}
}").

:- pragma foreign_proc("C",
	string__to_int_list(Str::out, IntList::in),
		[will_not_call_mercury, thread_safe], "{
		/* mode (out, in) is det */
	MR_Word int_list_ptr;
	size_t size;
	MR_Word str_ptr;
/*
** loop to calculate list length + sizeof(MR_Word) in `size' using list in
** `int_list_ptr'
*/
	size = sizeof(MR_Word);
	int_list_ptr = IntList;
	while (! MR_list_is_empty(int_list_ptr)) {
		size++;
		int_list_ptr = MR_list_tail(int_list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(MR_Word) - 1) / sizeof(MR_Word) words
*/
	MR_allocate_aligned_string_msg(Str, size, MR_PROC_LABEL);

/*
** loop to copy the characters from the int_list to the string
*/
	size = 0;
	int_list_ptr = IntList;
	while (! MR_list_is_empty(int_list_ptr)) {
		Str[size++] = MR_list_head(int_list_ptr);
		int_list_ptr = MR_list_tail(int_list_ptr);
	}
/*
** null terminate the string
*/
	Str[size] = '\\0';
}").

:- pragma foreign_proc("MC++",
	string__to_int_list(Str::in, IntList::out),
		[will_not_call_mercury, thread_safe], "{
        MR_Integer length, i; 
        MR_Word tmp;
        MR_Word prev;

        length = Str->get_Length();
      
        MR_list_nil(prev);

        for (i = length - 1; i >= 0; i--) {
		MR_list_cons(tmp, __box((MR_Integer) Str->get_Chars(i)), prev);
		prev = tmp;
        }
        IntList = tmp;
}").

:- pragma foreign_proc("MC++",
	string__to_int_list(Str::out, IntList::in),
		[will_not_call_mercury, thread_safe], "{
        System::Text::StringBuilder *tmp;
       
        tmp = new System::Text::StringBuilder();
        while (1) {
            if (System::Convert::ToInt32(IntList->GetValue(0))) {
                tmp->Append(System::Convert::ToChar(
			IntList->GetValue(1)));
                IntList = dynamic_cast<MR_Word>(IntList->GetValue(2));
            } else {
                break;
            }
        }
        Str = tmp->ToString();
}").


/*-----------------------------------------------------------------------*/

/*
:- pred string__contains_char(string, char).
:- mode string__contains_char(in, in) is semidet.
*/
:- pragma foreign_proc("C", string__contains_char(Str::in, Ch::in),
		[will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = (strchr(Str, Ch) != NULL);
").
:- pragma foreign_proc("MC++", string__contains_char(Str::in, Ch::in),
		[will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = (Str->IndexOf(Ch) != -1);
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__index(string, int, char).
:- mode string__index(in, in, out) is semidet.
*/
:- pragma foreign_proc("C", string__index(Str::in, Index::in, Ch::out),
		[will_not_call_mercury, thread_safe], "

                /*
		** We do not test for negative values of Index
                ** because (a) MR_Word is unsigned and hence a
                ** negative argument will appear as a very large
                ** positive one after the cast and (b) anybody
                ** dealing with the case where strlen(Str) > MAXINT
                ** is clearly barking mad (and one may well
                ** get an integer overflow error in this case).
                */

	if ((MR_Unsigned) Index >= strlen(Str)) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		SUCCESS_INDICATOR = TRUE;
		Ch = Str[Index];
	}
").
:- pragma foreign_proc("MC++", string__index(Str::in, Index::in, Ch::out),
		[will_not_call_mercury, thread_safe], "
	if (Index < 0 || Index >= Str->get_Length()) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		SUCCESS_INDICATOR = TRUE;
		Ch = Str->get_Chars(Index);
	}
").

/*-----------------------------------------------------------------------*/

:- pragma foreign_proc("C", 
	string__unsafe_index(Str::in, Index::in, Ch::out),
		[will_not_call_mercury, thread_safe], "
	Ch = Str[Index];
").
:- pragma foreign_proc("MC++", 
	string__unsafe_index(Str::in, Index::in, Ch::out),
		[will_not_call_mercury, thread_safe], "
	Ch = Str->get_Chars(Index);
").

/*-----------------------------------------------------------------------*/

:- pragma c_header_code("
#ifdef USE_GCC_GLOBAL_REGISTERS
	/*
	** GNU C version egcs-1.1.2 crashes with `fixed or forbidden
	** register spilled' in grade asm_fast.gc.tr.debug
	** if we write this inline.
	*/
	static void MR_set_char(MR_String str, MR_Integer ind, MR_Char ch)
	{
		str[ind] = ch;
	}
#else
	#define MR_set_char(str, ind, ch) \\
		((str)[ind] = (ch))
#endif
").

/*
:- pred string__set_char(char, int, string, string).
:- mode string__set_char(in, in, in, out) is semidet.
*/
:- pragma foreign_proc("C",
	string__set_char(Ch::in, Index::in, Str0::in, Str::out),
		[will_not_call_mercury, thread_safe], "
	size_t len = strlen(Str0);
	if ((MR_Unsigned) Index >= len) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		SUCCESS_INDICATOR = TRUE;
		MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);
		strcpy(Str, Str0);
		MR_set_char(Str, Index, Ch);
	}
").
:- pragma foreign_proc("MC++",
	string__set_char(Ch::in, Index::in, Str0::in, Str::out),
		[will_not_call_mercury, thread_safe], "
	if (Index >= Str0->get_Length()) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str = System::String::Concat(Str0->Substring(0, Index),
			System::Convert::ToString(Ch), 
			Str0->Substring(Index + 1));
		SUCCESS_INDICATOR = TRUE;
	}
").

/*
:- pred string__set_char(char, int, string, string).
:- mode string__set_char(in, in, di, uo) is semidet.
*/
:- pragma foreign_proc("C",
	string__set_char(Ch::in, Index::in, Str0::di, Str::uo),
		[will_not_call_mercury, thread_safe], "
	if ((MR_Unsigned) Index >= strlen(Str0)) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		SUCCESS_INDICATOR = TRUE;
		Str = Str0;
		MR_set_char(Str, Index, Ch);
	}
").

:- pragma foreign_proc("MC++",
	string__set_char(Ch::in, Index::in, Str0::di, Str::uo),
		[will_not_call_mercury, thread_safe], "
	if (Index >= Str0->get_Length()) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str = System::String::Concat(Str0->Substring(0, Index),
			System::Convert::ToString(Ch), 
			Str0->Substring(Index + 1));
		SUCCESS_INDICATOR = TRUE;
	}
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__unsafe_set_char(char, int, string, string).
:- mode string__unsafe_set_char(in, in, in, out) is det.
*/
:- pragma foreign_proc("C",
	string__unsafe_set_char(Ch::in, Index::in, Str0::in, Str::out),
		[will_not_call_mercury, thread_safe], "
	size_t len = strlen(Str0);
	MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);
	strcpy(Str, Str0);
	MR_set_char(Str, Index, Ch);
").
:- pragma foreign_proc("MC++",
	string__unsafe_set_char(Ch::in, Index::in, Str0::in, Str::out),
		[will_not_call_mercury, thread_safe], "
	Str = System::String::Concat(Str0->Substring(0, Index),
		System::Convert::ToString(Ch), 
		Str0->Substring(Index + 1));
").

/*
:- pred string__unsafe_set_char(char, int, string, string).
:- mode string__unsafe_set_char(in, in, di, uo) is det.
*/
:- pragma foreign_proc("C",
	string__unsafe_set_char(Ch::in, Index::in, Str0::di, Str::uo),
		[will_not_call_mercury, thread_safe], "
	Str = Str0;
	MR_set_char(Str, Index, Ch);
").
:- pragma foreign_proc("MC++",
	string__unsafe_set_char(Ch::in, Index::in, Str0::di, Str::uo),
		[will_not_call_mercury, thread_safe], "
	Str = System::String::Concat(Str0->Substring(0, Index),
		System::Convert::ToString(Ch), 
		Str0->Substring(Index + 1));
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__length(string, int).
:- mode string__length(in, uo) is det.
*/
:- pragma foreign_proc("C",
	string__length(Str::in, Length::uo),
		[will_not_call_mercury, thread_safe], "
	Length = strlen(Str);
").
:- pragma foreign_proc("MC++",
	string__length(Str::in, Length::uo),
		[will_not_call_mercury, thread_safe], "
	Length = Str->get_Length();
").

/*
:- pred string__length(string, int).
:- mode string__length(ui, uo) is det.
*/
:- pragma foreign_proc("C",
	string__length(Str::ui, Length::uo),
		[will_not_call_mercury, thread_safe], "
	Length = strlen(Str);
").
:- pragma foreign_proc("MC++",
	string__length(Str::ui, Length::uo),
		[will_not_call_mercury, thread_safe], "
	Length = Str->get_Length();
").

/*-----------------------------------------------------------------------*/

:- pragma promise_pure(string__append/3).

string__append(S1::in, S2::in, S3::in) :-
	string__append_iii(S1, S2, S3).
string__append(S1::in, S2::out, S3::in) :-
	string__append_ioi(S1, S2, S3).
string__append(S1::in, S2::in, S3::uo) :-
	string__append_iio(S1, S2, S3).
string__append(S1::out, S2::out, S3::in) :-
	string__append_ooi(S1, S2, S3).

:- pred string__append_iii(string::in, string::in, string::in) is semidet.

:- pragma foreign_proc("C",
	string__append_iii(S1::in, S2::in, S3::in),
		[will_not_call_mercury, thread_safe], "{
	size_t len_1 = strlen(S1);
	SUCCESS_INDICATOR = (
		strncmp(S1, S3, len_1) == 0 &&
		strcmp(S2, S3 + len_1) == 0
	);
}").

:- pragma foreign_proc("MC++",
	string__append_iii(S1::in, S2::in, S3::in),
		[will_not_call_mercury, thread_safe], "{
	SUCCESS_INDICATOR = S3->Equals(System::String::Concat(S1, S2));
}").

:- pred string__append_ioi(string::in, string::out, string::in) is semidet.

:- pragma foreign_proc("C",
	string__append_ioi(S1::in, S2::out,S3::in),
		[will_not_call_mercury, thread_safe], "{
	size_t len_1, len_2, len_3;

	len_1 = strlen(S1);
	if (strncmp(S1, S3, len_1) != 0) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		len_3 = strlen(S3);
		len_2 = len_3 - len_1;
		/*
		** We need to make a copy to ensure that the pointer is
		** word-aligned.
		*/
		MR_allocate_aligned_string_msg(S2, len_2, MR_PROC_LABEL);
		strcpy(S2, S3 + len_1);
		SUCCESS_INDICATOR = TRUE;
	}
}").

:- pragma foreign_proc("MC++",
	string__append_ioi(S1::in, S2::out, S3::in),
		[will_not_call_mercury, thread_safe], "{
	if (S3->StartsWith(S1)) {
		S2 = S3->Remove(0, S1->Length);
		SUCCESS_INDICATOR = TRUE;
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}").

:- pred string__append_iio(string::in, string::in, string::uo) is det.

:- pragma foreign_proc("C",
	string__append_iio(S1::in, S2::in, S3::uo),
		[will_not_call_mercury, thread_safe], "{
	size_t len_1, len_2;
	len_1 = strlen(S1);
	len_2 = strlen(S2);
	MR_allocate_aligned_string_msg(S3, len_1 + len_2, MR_PROC_LABEL);
	strcpy(S3, S1);
	strcpy(S3 + len_1, S2);
}").

:- pragma foreign_proc("MC++",
	string__append_iio(S1::in, S2::in, S3::uo),
		[will_not_call_mercury, thread_safe], "{
	S3 = System::String::Concat(S1, S2);
}").

:- pred string__append_ooi(string::out, string::out, string::in) is multi.

string__append_ooi(S1, S2, S3) :-
	S3Len = string__length(S3),
	string__append_ooi_2(0, S3Len, S1, S2, S3).

:- pred string__append_ooi_2(int::in, int::in, string::out, string::out,
	string::in) is multi.

string__append_ooi_2(NextS1Len, S3Len, S1, S2, S3) :-
	( NextS1Len = S3Len ->
		string__append_ooi_3(NextS1Len, S3Len, S1, S2, S3)
	;
		(
			string__append_ooi_3(NextS1Len, S3Len,
				S1, S2, S3)
		;
			string__append_ooi_2(NextS1Len + 1, S3Len,
				S1, S2, S3)
		)
	).

:- pred string__append_ooi_3(int::in, int::in, string::out,
	string::out, string::in) is det.

:- pragma foreign_proc("C",
	string__append_ooi_3(S1Len::in, S3Len::in, S1::out, S2::out, S3::in),
		[will_not_call_mercury, thread_safe], "{
	MR_allocate_aligned_string_msg(S1, S1Len, MR_PROC_LABEL);
	memcpy(S1, S3, S1Len);
	S1[S1Len] = '\\0';
	MR_allocate_aligned_string_msg(S2, S3Len - S1Len, MR_PROC_LABEL);
	strcpy(S2, S3 + S1Len);
}").

:- pragma foreign_proc("MC++",
	string__append_ooi_3(S1Len::in, _S3Len::in,
			S1::out, S2::out, S3::in),
		[will_not_call_mercury, thread_safe], "
	S1 = S3->Substring(0, S1Len);
	S2 = S3->Substring(S1Len);
").

/*-----------------------------------------------------------------------*/

/*
:- pred string__substring(string, int, int, string).
:- mode string__substring(in, in, in, out) is det.
%	string__substring(String, Start, Count, Substring):
*/

string__substring(Str::in, Start::in, Count::in, SubStr::out) :-
	End = min(Start + Count, string__length(Str)),
	SubStr = string__from_char_list(strchars(Start, End, Str)).

:- func strchars(int, int, string) = list(char).
strchars(I, End, Str) =
	( if ( I < 0 ; End =< I )
		then []
		else [string__index_det(Str, I) | 
			strchars(I + 1, End, Str)]
	).

:- pragma foreign_proc("C",
	string__substring(Str::in, Start::in, Count::in,
		SubString::out),
		[will_not_call_mercury, thread_safe],
"{
	MR_Integer len;
	MR_Word tmp;
	if (Start < 0) Start = 0;
	if (Count <= 0) {
		MR_make_aligned_string(
			MR_LVALUE_CAST(MR_ConstString, SubString),
			"""");
	} else {
		len = strlen(Str);
		if (Start > len) Start = len;
		if (Count > len - Start) Count = len - Start;
		MR_allocate_aligned_string_msg(SubString, Count, MR_PROC_LABEL);
		memcpy(SubString, Str + Start, Count);
		SubString[Count] = '\\0';
	}
}").

/*
:- pred string__unsafe_substring(string, int, int, string).
:- mode string__unsafe_substring(in, in, in, out) is det.
%	string__unsafe_substring(String, Start, Count, Substring):
*/

:- pragma foreign_proc("C",
	string__unsafe_substring(Str::in, Start::in, Count::in,
		SubString::out),
		[will_not_call_mercury, thread_safe],
"{
	MR_Integer len;
	MR_allocate_aligned_string_msg(SubString, Count, MR_PROC_LABEL);
	memcpy(SubString, Str + Start, Count);
	SubString[Count] = '\\0';
}").
:- pragma foreign_proc("MC++",
	string__unsafe_substring(Str::in, Start::in, Count::in,
		SubString::out),
		[will_not_call_mercury, thread_safe],
"{
	SubString = Str->Substring(Start, Count);
}").



/*
:- pred string__split(string, int, string, string).
:- mode string__split(in, in, out, out) is det.
%	string__split(String, Count, LeftSubstring, RightSubstring):
%	`LeftSubstring' is the left-most `Count' characters of `String',
%	and `RightSubstring' is the remainder of `String'.
%	(If `Count' is out of the range [0, length of `String'], it is
%	treated as if it were the nearest end-point of that range.)
*/

:- pragma foreign_proc("C",
	string__split(Str::in, Count::in, Left::out, Right::out),
		[will_not_call_mercury, thread_safe], "{
	MR_Integer len;
	MR_Word tmp;
	if (Count <= 0) {
		MR_make_aligned_string(MR_LVALUE_CAST(MR_ConstString, Left),
			"""");
		Right = Str;
	} else {
		len = strlen(Str);
		if (Count > len) Count = len;
		MR_allocate_aligned_string_msg(Left, Count, MR_PROC_LABEL);
		memcpy(Left, Str, Count);
		Left[Count] = '\\0';
		/*
		** We need to make a copy to ensure that the pointer is
		** word-aligned.
		*/
		MR_allocate_aligned_string_msg(Right, len - Count,
			MR_PROC_LABEL);
		strcpy(Right, Str + Count);
	}
}").

:- pragma foreign_proc("MC++",
	string__split(Str::in, Count::in, Left::out, Right::out),
		[will_not_call_mercury, thread_safe], "{
	MR_Integer len;
	MR_Word tmp;
	if (Count <= 0) {
		Left = """";
		Right = Str;
	} else {
		len = Str->get_Length();
		if (Count > len) {
			Count = len;
		}
		Left = Str->Substring(0, Count);
		Right = Str->Substring(Count);
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
:- pragma foreign_proc("C",
	string__first_char(Str::in, First::in, Rest::in),
		[will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = (
		Str[0] == First &&
		First != '\\0' &&
		strcmp(Str + 1, Rest) == 0
	);
").
:- pragma foreign_proc("MC++",
	string__first_char(Str::in, First::in, Rest::in),
		[will_not_call_mercury, thread_safe], "
	MR_Integer len = Str->get_Length();
	SUCCESS_INDICATOR = (
		len > 0 &&
		Str->get_Chars(0) == First &&
		System::String::Compare(Str, 1, Rest, 0, len) == 0
	);
").

/*
:- mode string__first_char(in, out, in) is semidet.	% implied
*/
:- pragma foreign_proc("C",
	string__first_char(Str::in, First::out, Rest::in),
		[will_not_call_mercury, thread_safe], "
	First = Str[0];
	SUCCESS_INDICATOR = (First != '\\0' && strcmp(Str + 1, Rest) == 0);
").
:- pragma foreign_proc("MC++",
	string__first_char(Str::in, First::out, Rest::in),
		[will_not_call_mercury, thread_safe], "
	MR_Integer len = Str->get_Length();
	if (len > 0) {
		SUCCESS_INDICATOR = 
			(System::String::Compare(Str, 1, Rest, 0, len) == 0);
		First = Str->get_Chars(0);
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
").

/*
:- mode string__first_char(in, in, out) is semidet.	% implied
*/
:- pragma foreign_proc("C",
	string__first_char(Str::in, First::in, Rest::out),
		[will_not_call_mercury, thread_safe], "{
	if (Str[0] != First || First == '\\0') {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str++;
		/*
		** We need to make a copy to ensure that the pointer is
		** word-aligned.
		*/
		MR_allocate_aligned_string_msg(Rest, strlen(Str),
			MR_PROC_LABEL);
		strcpy(Rest, Str);
		SUCCESS_INDICATOR = TRUE;
	}
}").
:- pragma foreign_proc("MC++",
	string__first_char(Str::in, First::in, Rest::out),
		[will_not_call_mercury, thread_safe], "{
	MR_Integer len = Str->get_Length();
	if (len > 0) {
		SUCCESS_INDICATOR = (First == Str->get_Chars(0));
		Rest = (Str)->Substring(1);
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}").

/*
:- mode string__first_char(in, out, out) is semidet.
*/
:- pragma foreign_proc("C", 
	string__first_char(Str::in, First::out, Rest::out),
		[will_not_call_mercury, thread_safe], "{
	First = Str[0];
	if (First == '\\0') {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str++;
		/*
		** We need to make a copy to ensure that the pointer is
		** word-aligned.
		*/
		MR_allocate_aligned_string_msg(Rest, strlen(Str),
			MR_PROC_LABEL);
		strcpy(Rest, Str);
		SUCCESS_INDICATOR = TRUE;
	}
}").
:- pragma foreign_proc("MC++", 
	string__first_char(Str::in, First::out, Rest::out),
		[will_not_call_mercury, thread_safe], "{
	if (Str->get_Length() == 0) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		First = Str->get_Chars(0);
		Rest = (Str)->Substring(1);
		SUCCESS_INDICATOR = TRUE;
        }
}").


/*
:- mode string__first_char(out, in, in) is det.
*/
:- pragma foreign_proc("C",
	string__first_char(Str::out, First::in, Rest::in),
		[will_not_call_mercury, thread_safe], "{
	size_t len = strlen(Rest) + 1;
	MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);
	Str[0] = First;
	strcpy(Str + 1, Rest);
}").
:- pragma foreign_proc("MC++",
	string__first_char(Str::out, First::in, Rest::in),
		[will_not_call_mercury, thread_safe], "{
	MR_String FirstStr;
	FirstStr = new System::String(First, 1);
	Str = System::String::Concat(FirstStr, Rest);
}").


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%       Functional forms added.

string__length(S) = L :-
	string__length(S, L).

string__append(S1, S2) = S3 :-
	string__append(S1, S2, S3).

string__char_to_string(C) = S1 :-
	string__char_to_string(C, S1).

string__int_to_string(N) = S1 :-
	string__int_to_string(N, S1).

string__int_to_base_string(N1, N2) = S2 :-
	string__int_to_base_string(N1, N2, S2).

string__float_to_string(R) = S2 :-
	string__float_to_string(R, S2).

string__replace_all(S1, S2, S3) = S4 :-
	string__replace_all(S1, S2, S3, S4).

string__to_lower(S1) = S2 :-
	string__to_lower(S1, S2).

string__to_upper(S1) = S2 :-
	string__to_upper(S1, S2).

string__capitalize_first(S1) = S2 :-
	string__capitalize_first(S1, S2).

string__uncapitalize_first(S1) = S2 :-
	string__uncapitalize_first(S1, S2).

string__to_char_list(S) = Cs :-
	string__to_char_list(S, Cs).

string__from_char_list(Cs) = S :-
	string__from_char_list(Cs, S).

string__from_rev_char_list(Cs) = S :-
	string__from_rev_char_list(Cs, S).

string__pad_left(S1, C, N) = S2 :-
	string__pad_left(S1, C, N, S2).

string__pad_right(S1, C, N) = S2 :-
	string__pad_right(S1, C, N, S2).

string__duplicate_char(C, N) = S :-
	string__duplicate_char(C, N, S).

string__index_det(S, N) = C :-
	string__index_det(S, N, C).

string__unsafe_index(S, N) = C :-
	string__unsafe_index(S, N, C).

string__set_char_det(C, N, S0) = S :-
	string__set_char_det(C, N, S0, S).

string__unsafe_set_char(C, N, S0) = S :-
	string__unsafe_set_char(C, N, S0, S).

string__foldl(F, S, A) = B :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	string__foldl(P, S, A, B).

string__left(S1, N) = S2 :-
	string__left(S1, N, S2).

string__right(S1, N) = S2 :-
	string__right(S1, N, S2).

string__substring(S1, N1, N2) = S2 :-
	string__substring(S1, N1, N2, S2).

string__unsafe_substring(S1, N1, N2) = S2 :-
	string__unsafe_substring(S1, N1, N2, S2).

string__hash(S) = N :-
	string__hash(S, N).

string__format(S1, PT) = S2 :-
	string__format(S1, PT, S2).

% ---------------------------------------------------------------------------- %

string__words(SepP, String) = Words :-
	I = preceding_boundary(isnt(SepP), String, string__length(String) - 1),
	Words = words_2(SepP, String, I, []).

% ---------------------------------------------------------------------------- %

:- func words_2(pred(char), string, int, list(string)) = list(string).
:- mode words_2(pred(in) is semidet, in, in, in) = out is det.

words_2(SepP, String, WordEnd, Words0) = Words :-
	( if WordEnd < 0 then
		Words = Words0
	  else
		WordPre = preceding_boundary(SepP, String, WordEnd),
		Word = string__unsafe_substring(String, WordPre + 1,
				WordEnd - WordPre),
		PrevWordEnd = preceding_boundary(isnt(SepP), String, WordPre),
		Words = words_2(SepP, String, PrevWordEnd, [Word | Words0])
	).

% ---------------------------------------------------------------------------- %

	% preceding_boundary(SepP, String, I) returns the largest index J =< I
	% in String of the char that is SepP and min(-1, I) if there is no
	% such J.  preceding_boundary/3 is intended for finding (in reverse)
	% consecutive maximal sequences of chars satisfying some property.
	% Note that I *must not* exceed the largest valid index for String.

:- func preceding_boundary(pred(char), string, int) = int.
:- mode preceding_boundary(pred(in) is semidet, in, in) = out is det.

preceding_boundary(SepP, String, I) =
	( if I < 0 then
		I
	  else if SepP(string__unsafe_index(String, I)) then
		I
	  else
		preceding_boundary(SepP, String, I - 1)
	).

% ---------------------------------------------------------------------------- %

S1 ++ S2 = string__append(S1, S2).

% ---------------------------------------------------------------------------- %

string__det_to_int(S) = string__det_base_string_to_int(10, S).

% ---------------------------------------------------------------------------- %

string__det_base_string_to_int(Base, S) = N :-
	( if string__base_string_to_int(Base, S, N0) then
		N = N0
	  else
	  	error("string__det_base_string_to_int/2: conversion failed")
	).

% ---------------------------------------------------------------------------- %

:- end_module string.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
