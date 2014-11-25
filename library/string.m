%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: string.m.
% Main authors: fjh, petdr.
% Stability: medium to high.
%
% This modules provides basic string handling facilities.
%
% Unexpected null characters embedded in the middle of strings can be a source
% of security vulnerabilities, so the Mercury library predicates and functions
% which create strings from (lists of) characters throw an exception if a null
% character is detected. Programmers must not create strings that might
% contain null characters using the foreign language interface.
%
% When Mercury is compiled to C, strings are UTF-8 encoded, using a null
% character as the string terminator. A single code point requires one to four
% bytes (code units) to encode.
%
% When Mercury is compiled to Java, strings are represented as Java `String's.
% When Mercury is compiled to C# code, strings are represented as
% `System.String's. In both cases, strings are UTF-16 encoded.
% A single code point requires one or two 16-bit integers (code units)
% to encode.
%
% When Mercury is compiled to Erlang, strings are represented as Erlang
% binaries using UTF-8 encoding.
%
% The builtin comparison operation on strings is also implementation
% dependent. The current implementation performs string comparison using
%
% - C's strcmp() function, when compiling to C;
% - Java's String.compareTo() method, when compiling to Java; and
% - C#'s System.String.CompareOrdinal() method, when compiling to C#.
%
%---------------------------------------------------------------------------%
%
% This module is divided into several sections. These sections are:
%
% - Wrapper types that associate particular semantics with raw strings.
% - Converting between strings and lists of characters.
% - Reading characters from strings.
% - Writing characters to strings.
% - Determining the lengths of strings.
% - Computing hashes of strings.
% - Tests on strings.
% - Appending strings.
% - Splitting up strings.
% - Dealing with prefixes and suffixes.
% - Transformations of strings.
% - Folds over the characters in strings.
% - Formatting tables.
% - Converting strings to docs.
% - Converting strings to values of builtin types.
% - Converting values of builtin types to strings.
% - Converting values of arbitrary types to strings.
% - Converting values to strings based on a format string.
%
%---------------------------------------------------------------------------%

:- module string.
:- interface.

:- include_module builder.

:- import_module assoc_list.
:- import_module char.
:- import_module deconstruct.
:- import_module list.
:- import_module maybe.
:- import_module ops.
:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Wrapper types that associate particular semantics with raw strings.
%
% These types are used for defining stream typeclass instances
% where you want different instances for strings representing different
% semantic entities. Using the string type itself, without a wrapper,
% would be ambiguous in such situations.
%
% While each module that associates semantics with strings could define
% its own wrapper types, the notions of lines and text files are so common
% that it is simpler to defined them just once, and this is the logical
% place to do that.
%

    % A line is:
    %
    % - a possibly empty sequence of non-newline characters terminated by a
    %   newline character; or
    % - a non-empty sequence of non-newline characters terminated by the end
    %   of the file.
    %
:- type line
    --->    line(string).

    % A text file is a possibly empty sequence of characters
    % terminated by the end of the file.
    %
:- type text_file
    --->    text_file(string).

%---------------------------------------------------------------------------%
%
% Conversions between strings and lists of characters.
%

    % Convert the string to a list of characters (code points).
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
:- func to_char_list(string) = list(char).
:- pred to_char_list(string, list(char)).
:- mode to_char_list(in, out) is det.
:- mode to_char_list(uo, in) is det.

    % Convert a list of characters (code points) to a string.
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
:- func from_char_list(list(char)::in) = (string::uo) is det.
:- pred from_char_list(list(char), string).
:- mode from_char_list(in, uo) is det.
:- mode from_char_list(out, in) is det.

    % As above, but fail instead of throwing an exception if the list contains
    % a null character.
    %
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
:- pred semidet_from_char_list(list(char)::in, string::uo) is semidet.

    % Same as from_char_list, except that it reverses the order
    % of the characters.
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
:- func from_rev_char_list(list(char)::in) = (string::uo) is det.
:- pred from_rev_char_list(list(char)::in, string::uo) is det.

    % As above, but fail instead of throwing an exception if the list contains
    % a null character.
    %
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
:- pred semidet_from_rev_char_list(list(char)::in, string::uo) is semidet.

    % Convert a string into a list of code units.
    %
:- pred to_code_unit_list(string::in, list(int)::out) is det.

    % Convert a list of code units to a string.
    % Fails if the list does not contain a valid encoding of a string,
    % in the encoding expected by the current process.
    %
:- pred from_code_unit_list(list(int)::in, string::uo) is semidet.

    % duplicate_char(Char, Count, String):
    %
    % Construct a string consisting of `Count' occurrences of `Char'
    % code points in sequence.
    %
:- func duplicate_char(char::in, int::in) = (string::uo) is det.
:- pred duplicate_char(char::in, int::in, string::uo) is det.

%---------------------------------------------------------------------------%
%
% Reading characters from strings.
%

    % index(String, Index, Char):
    %
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'. Fails if `Index' is out of range (negative, or
    % greater than or equal to the length of `String').
    %
    % Calls error/1 if an illegal sequence is detected.
    %
:- pred index(string::in, int::in, char::uo) is semidet.

    % det_index(String, Index, Char):
    %
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'.
    % Calls error/1 if `Index' is out of range (negative, or greater than
    % or equal to the length of `String'), or if an illegal sequence is
    % detected.
    %
:- func det_index(string, int) = char.
:- pred det_index(string::in, int::in, char::uo) is det.

    % unsafe_index(String, Index, Char):
    %
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    % This version is constant time, whereas det_index
    % may be linear in the length of the string. Use with care!
    %
:- func unsafe_index(string, int) = char.
:- pred unsafe_index(string::in, int::in, char::uo) is det.

    % A synonym for det_index/2:
    % String ^ elem(Index) = det_index(String, Index).
    %
:- func string ^ elem(int) = char.

    % A synonym for unsafe_index/2:
    % String ^ unsafe_elem(Index) = unsafe_index(String, Index).
    %
:- func string ^ unsafe_elem(int) = char.

    % index_next(String, Index, NextIndex, Char):
    %
    % Like `index'/3 but also returns the position of the code unit
    % that follows the code point beginning at `Index',
    % i.e. NextIndex = Index + num_code_units_to_encode(Char).
    %
:- pred index_next(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_index_next(String, Index, NextIndex, Char):
    %
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'. `NextIndex' is the offset following the encoding
    % of `Char'. Fails if `Index' is equal to the length of `String'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than the length of `String').
    %
:- pred unsafe_index_next(string::in, int::in, int::out, char::uo) is semidet.

    % prev_index(String, Index, CharIndex, Char):
    %
    % `Char' is the character (code point) in `String' immediately _before_
    % the code unit `Index'. Fails if `Index' is out of range (non-positive,
    % or greater than the length of `String').
    %
:- pred prev_index(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_prev_index(String, Index, CharIndex, Char):
    %
    % `Char' is the character (code point) in `String' immediately _before_
    % the code unit `Index'. `CharIndex' is the offset of the beginning of
    % `Char'. Fails if `Index' is zero.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    %
:- pred unsafe_prev_index(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_index_code_unit(String, Index, CodeUnit):
    %
    % `CodeUnit' is the code unit in `String' at the offset `Index'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    %
:- pred unsafe_index_code_unit(string::in, int::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Writing characters to strings.
%

    % set_char(Char, Index, String0, String):
    %
    % `String' is `String0', with the code point beginning at code unit
    % `Index' removed and replaced by `Char'.
    % Fails if `Index' is out of range (negative, or greater than or equal to
    % the length of `String0').
    %
:- pred set_char(char, int, string, string).
:- mode set_char(in, in, in, out) is semidet.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode set_char(in, in, di, uo) is semidet.

    % det_set_char(Char, Index, String0, String):
    %
    % `String' is `String0', with the code point beginning at code unit
    % `Index' removed and replaced by `Char'.
    % Calls error/1 if `Index' is out of range (negative, or greater than
    % or equal to the length of `String0').
    %
:- func det_set_char(char, int, string) = string.
:- pred det_set_char(char, int, string, string).
:- mode det_set_char(in, in, in, out) is det.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode det_set_char(in, in, di, uo) is det.

    % unsafe_set_char(Char, Index, String0, String):
    %
    % `String' is `String0', with the code point beginning at code unit
    % `Index' removed and replaced by `Char'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String0').
    % This version is constant time, whereas det_set_char
    % may be linear in the length of the string. Use with care!
    %
:- func unsafe_set_char(char, int, string) = string.
:- mode unsafe_set_char(in, in, in) = out is det.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode unsafe_set_char(in, in, di) = uo is det.
:- pred unsafe_set_char(char, int, string, string).
:- mode unsafe_set_char(in, in, in, out) is det.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode unsafe_set_char(in, in, di, uo) is det.

%---------------------------------------------------------------------------%
%
% Determining the lengths of strings.
%

    % Determine the length of a string, in code units.
    % An empty string has length zero.
    %
    % NOTE: code points (characters) are encoded using one or more code units,
    % i.e. bytes for UTF-8; 16-bit integers for UTF-16.
    %
:- func length(string::in) = (int::uo) is det.
:- pred length(string, int).
:- mode length(in, uo) is det.
:- mode length(ui, uo) is det.

    % Synonyms for length.
    %
:- func count_code_units(string) = int.
:- pred count_code_units(string::in, int::out) is det.

    % Determine the number of code points in a string.
    %
    % NOTE The names of this predicate and several other predicates
    % may be changed in the future to refer to code_points, not codepoints,
    % for consistency with predicate names that talk about code_units.
    %
:- func count_codepoints(string) = int.
:- pred count_codepoints(string::in, int::out) is det.

    % Determine the number of code units required to represent a string in
    % UTF-8 encoding.
    %
:- func count_utf8_code_units(string) = int.

    % codepoint_offset(String, StartOffset, CodePointCount, CodePointOffset):
    %
    % Return the offset into `String' where, starting from `StartOffset',
    % `CodePointCount' code points are skipped. Fails if either `StartOffset'
    % or `CodePointOffset' are out of range.
    %
:- pred codepoint_offset(string::in, int::in, int::in, int::out) is semidet.

    % codepoint_offset(String, CodePointCount, CodePointOffset):
    %
    % Same as `codepoint_offset(String, 0, CodePointCount, CodePointOffset)'.
    %
:- pred codepoint_offset(string::in, int::in, int::out) is semidet.

%---------------------------------------------------------------------------%
%
% Computing hashes of strings.
%

    % Compute a hash value for a string.
    %
:- func hash(string) = int.
:- pred hash(string::in, int::out) is det.

    % Two other hash functions for strings.
    %
:- func hash2(string) = int.
:- func hash3(string) = int.

%---------------------------------------------------------------------------%
%
% Tests on strings.
%

    % True if string contains only alphabetic characters [A-Za-z].
    %
:- pred is_all_alpha(string::in) is semidet.

    % True if string contains only alphabetic characters [A-Za-z] and digits
    % [0-9].
    %
:- pred is_all_alnum(string::in) is semidet.

    % True if string contains only alphabetic characters [A-Za-z] and
    % underscores.
    %
:- pred is_all_alpha_or_underscore(string::in) is semidet.

    % True if string contains only alphabetic characters [A-Za-z],
    % digits [0-9], and underscores.
    %
:- pred is_all_alnum_or_underscore(string::in) is semidet.

    % True if the string contains only decimal digits (0-9).
    %
:- pred is_all_digits(string::in) is semidet.

    % all_match(TestPred, String):
    %
    % True if TestPred is true when applied to each character (code point) in
    % String or if String is the empty string.
    %
:- pred all_match(pred(char)::in(pred(in) is semidet), string::in) is semidet.

    % contains_char(String, Char):
    %
    % Succeed if the code point `Char' occurs in `String'.
    %
:- pred contains_char(string::in, char::in) is semidet.

    % prefix_length(Pred, String):
    %
    % The length (in code units) of the maximal prefix of `String' consisting
    % entirely of characters (code points) satisfying Pred.
    %
:- func prefix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % suffix_length(Pred, String):
    %
    % The length (in code units) of the maximal suffix of `String' consisting
    % entirely of characters (code points) satisfying Pred.
    %
:- func suffix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % sub_string_search(String, SubString, Index):
    %
    % `Index' is the code unit position in `String' where the first
    % occurrence of `SubString' begins. Indices start at zero, so if
    % `SubString' is a prefix of `String', this will return Index = 0.
    %
:- pred sub_string_search(string::in, string::in, int::out) is semidet.

    % sub_string_search_start(String, SubString, BeginAt, Index):
    %
    % `Index' is the code unit position in `String' where the first
    % occurrence of `SubString' occurs such that 'Index' is greater than or
    % equal to `BeginAt'. Indices start at zero.
    %
:- pred sub_string_search_start(string::in, string::in, int::in, int::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Appending strings.
%

    % Append two strings together.
    %
:- func append(string::in, string::in) = (string::uo) is det.

:- pred append(string, string, string).
:- mode append(in, in, in) is semidet.  % implied
:- mode append(in, uo, in) is semidet.
:- mode append(in, in, uo) is det.
:- mode append(out, out, in) is multi.
% The following mode is semidet in the sense that it doesn't succeed more
% than once - but it does create a choice-point, which means that the
% compiler can't deduce that it is semidet. (It is also inefficient.)
% Use remove_suffix instead.
% :- mode append(out, in, in) is semidet.

    % S1 ++ S2 = S :- append(S1, S2, S).
    %
    % Append two strings together using nicer inline syntax.
    %
:- func string ++ string = string.
:- mode in ++ in = uo is det.

    % Append a list of strings together.
    %
:- func append_list(list(string)::in) = (string::uo) is det.
:- pred append_list(list(string)::in, string::uo) is det.

    % join_list(Separator, Strings) = JoinedString:
    %
    % Append together the strings in Strings, putting Separator between
    % each pair of adjacent strings. If Strings is the empty list,
    % return the empty string.
    %
:- func join_list(string::in, list(string)::in) = (string::uo) is det.

%---------------------------------------------------------------------------%
%
% Splitting up strings.
%

    % first_char(String, Char, Rest) is true iff Char is the first character
    % (code point) of String, and Rest is the remainder.
    %
    % WARNING: first_char makes a copy of Rest because the garbage collector
    % doesn't handle references into the middle of an object, at least not the
    % way we use it. This means that repeated use of first_char to iterate
    % over a string will result in very poor performance. If you want to
    % iterate over the characters in a string, use foldl or to_char_list
    % instead.
    %
:- pred first_char(string, char, string).
:- mode first_char(in, in, in) is semidet.  % implied
:- mode first_char(in, uo, in) is semidet.  % implied
:- mode first_char(in, in, uo) is semidet.  % implied
:- mode first_char(in, uo, uo) is semidet.
:- mode first_char(uo, in, in) is det.

    % split(String, Index, LeftSubstring, RightSubstring):
    %
    % Split a string into two substrings, at the code unit `Index'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred split(string::in, int::in, string::out, string::out) is det.

    % split_by_codepoint(String, Count, LeftSubstring, RightSubstring):
    %
    % `LeftSubstring' is the left-most `Count' characters (code points) of
    % `String', and `RightSubstring' is the remainder of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred split_by_codepoint(string::in, int::in, string::out, string::out)
    is det.

    % left(String, Count, LeftSubstring):
    %
    % `LeftSubstring' is the left-most `Count' code _units_ of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func left(string::in, int::in) = (string::out) is det.
:- pred left(string::in, int::in, string::out) is det.

    % left_by_codepoint(String, Count, LeftSubstring):
    %
    % `LeftSubstring' is the left-most `Count' characters (code points) of
    % `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func left_by_codepoint(string::in, int::in) = (string::out) is det.
:- pred left_by_codepoint(string::in, int::in, string::out) is det.

    % right(String, Count, RightSubstring):
    %
    % `RightSubstring' is the right-most `Count' code _units_ of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func right(string::in, int::in) = (string::out) is det.
:- pred right(string::in, int::in, string::out) is det.

    % right_by_codepoint(String, Count, RightSubstring):
    %
    % `RightSubstring' is the right-most `Count' characters (code points) of
    % `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func right_by_codepoint(string::in, int::in) = (string::out) is det.
:- pred right_by_codepoint(string::in, int::in, string::out) is det.

    % between(String, Start, End, Substring):
    %
    % `Substring' consists of the segment of `String' within the half-open
    % interval [Start, End), where `Start' and `End' are code unit offsets.
    % (If `Start' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.
    % If `End' is out of the range [`Start', length of `String'],
    % it is treated as if it were the nearest end-point of that range.)
    %
:- func between(string::in, int::in, int::in) = (string::uo) is det.
:- pred between(string::in, int::in, int::in, string::uo) is det.

    % substring(String, Start, Count, Substring):
    % Please use `between' instead.
    %
:- pragma obsolete(substring/3).
:- pragma obsolete(substring/4).
:- func substring(string::in, int::in, int::in) = (string::uo) is det.
:- pred substring(string::in, int::in, int::in, string::uo) is det.

    % between_codepoints(String, Start, End, Substring):
    %
    % `Substring' is the part of `String' between the code point positions
    % `Start' and `End'.
    % (If `Start' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.
    % If `End' is out of the range [`Start', length of `String'],
    % it is treated as if it were the nearest end-point of that range.)
    %
:- func between_codepoints(string::in, int::in, int::in)
    = (string::uo) is det.
:- pred between_codepoints(string::in, int::in, int::in, string::uo) is det.

    % unsafe_between(String, Start, End, Substring):
    %
    % `Substring' consists of the segment of `String' within the half-open
    % interval [Start, End), where `Start' and `End' are code unit offsets.
    % WARNING: if `Start' is out of the range [0, length of `String'] or
    % `End' is out of the range [`Start', length of `String']
    % then the behaviour is UNDEFINED. Use with care!
    % This version takes time proportional to the length of the substring,
    % whereas substring may take time proportional to the length
    % of the whole string.
    %
:- func unsafe_between(string::in, int::in, int::in) = (string::uo) is det.
:- pred unsafe_between(string::in, int::in, int::in, string::uo) is det.

    % unsafe_substring(String, Start, Count, Substring):
    % Please use unsafe_between instead.
    %
:- pragma obsolete(unsafe_substring/3).
:- pragma obsolete(unsafe_substring/4).
:- func unsafe_substring(string::in, int::in, int::in) = (string::uo) is det.
:- pred unsafe_substring(string::in, int::in, int::in, string::uo) is det.

    % words_separator(SepP, String) returns the list of non-empty
    % substrings of String (in first to last order) that are delimited
    % by non-empty sequences of characters (code points) matched by SepP.
    % For example,
    %
    % words_separator(char.is_whitespace, " the cat  sat on the  mat") =
    %   ["the", "cat", "sat", "on", "the", "mat"]
    %
    % Note the difference to split_at_separator.
    %
:- func words_separator(pred(char), string) = list(string).
:- mode words_separator(pred(in) is semidet, in) = out is det.

    % words(String) =
    %   words_separator(char.is_whitespace, String).
    %
:- func words(string) = list(string).

    % split_at_separator(SepP, String) returns the list of
    % substrings of String (in first to last order) that are delimited
    % by characters (code points) matched by SepP. For example,
    %
    % split_at_separator(char.is_whitespace, " a cat  sat on the  mat")
    %   = ["", "a", "cat", "", "sat", "on", "the", "", "mat"]
    %
    % Note the difference to words_separator.
    %
:- func split_at_separator(pred(char), string) = list(string).
:- mode split_at_separator(pred(in) is semidet, in) = out is det.

    % split_at_char(Char, String) =
    %     split_at_separator(unify(Char), String)
    %
:- func split_at_char(char, string) = list(string).

    % split_at_string(Separator, String) returns the list of substrings
    % of String that are delimited by Separator. For example,
    %
    % split_at_string("|||", "|||fld2|||fld3") = ["", "fld2", [fld3"]
    %
    % Always the first match of Separator is used to break the String, for
    % example: split_at_string("aa", "xaaayaaaz") = ["x", "ay", "az"]
    %
:- func split_at_string(string, string) = list(string).

%---------------------------------------------------------------------------%
%
% Dealing with prefixes and suffixes.
%

    % prefix(String, Prefix) is true iff Prefix is a prefix of String.
    % Same as append(Prefix, _, String).
    %
:- pred prefix(string, string).
:- mode prefix(in, in) is semidet.
:- mode prefix(in, out) is multi.

    % suffix(String, Suffix) is true iff Suffix is a suffix of String.
    % Same as append(_, Suffix, String).
    %
:- pred suffix(string, string).
:- mode suffix(in, in) is semidet.
:- mode suffix(in, out) is multi.

    % remove_prefix(Prefix, String, Suffix):
    %
    % This is a synonym for append(Prefix, Suffix, String) but with the
    % arguments in a more convenient order for use with higher-order code.
    %
:- pred remove_prefix(string::in, string::in, string::out) is semidet.

    % remove_prefix_if_present(Prefix, String) = Suffix returns `String' minus
    % `Prefix' if `String' begins with `Prefix', and `String' if it doesn't.
    %
:- func remove_prefix_if_present(string, string) = string.

    % remove_suffix(String, Suffix, Prefix):
    %
    % The same as append(Prefix, Suffix, String) except that this is semidet
    % whereas append(out, in, in) is nondet.
    %
:- pred remove_suffix(string::in, string::in, string::out) is semidet.

    % det_remove_suffix(String, Suffix) returns the same value as
    % remove_suffix, except it aborts if String does not end with Suffix.
    %
:- func det_remove_suffix(string, string) = string.

    % remove_suffix_if_present(Suffix, String) returns `String' minus `Suffix'
    % if `String' ends with `Suffix', and `String' if it doesn't.
    %
:- func remove_suffix_if_present(string, string) = string.

%---------------------------------------------------------------------------%
%
% Transformations of strings.
%

    % Convert the first character (if any) of a string to uppercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func capitalize_first(string) = string.
:- pred capitalize_first(string::in, string::out) is det.

    % Convert the first character (if any) of a string to lowercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func uncapitalize_first(string) = string.
:- pred uncapitalize_first(string::in, string::out) is det.

    % Converts a string to uppercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func to_upper(string::in) = (string::uo) is det.
:- pred to_upper(string, string).
:- mode to_upper(in, uo) is det.
:- mode to_upper(in, in) is semidet.        % implied

    % Converts a string to lowercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func to_lower(string::in) = (string::uo) is det.
:- pred to_lower(string, string).
:- mode to_lower(in, uo) is det.
:- mode to_lower(in, in) is semidet.        % implied

    % pad_left(String0, PadChar, Width, String):
    %
    % Insert `PadChar's at the left of `String0' until it is at least as long
    % as `Width', giving `String'. Width is currently measured as the number
    % of code points.
    %
:- func pad_left(string, char, int) = string.
:- pred pad_left(string::in, char::in, int::in, string::out) is det.

    % pad_right(String0, PadChar, Width, String):
    %
    % Insert `PadChar's at the right of `String0' until it is at least as long
    % as `Width', giving `String'. Width is currently measured as the number
    % of code points.
    %
:- func pad_right(string, char, int) = string.
:- pred pad_right(string::in, char::in, int::in, string::out) is det.

    % chomp(String):
    %
    % Return `String' minus any single trailing newline character.
    %
:- func chomp(string) = string.

    % lstrip(String):
    %
    % Return `String' minus any initial whitespace characters
    % in the ASCII range.
    %
:- func lstrip(string) = string.

    % rstrip(String):
    %
    % Returns `String' minus any trailing whitespace characters
    % in the ASCII range.
    %
:- func rstrip(string) = string.

    % strip(String):
    %
    % Returns `String' minus any initial and trailing whitespace characters
    % in the ASCII range.
    %
:- func strip(string) = string.

    % lstrip_pred(Pred, String):
    %
    % Returns `String' minus the maximal prefix consisting entirely
    % of characters (code points) satisfying `Pred'.
    %
:- func lstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % rstrip_pred(Pred, String):
    %
    % Returns `String' minus the maximal suffix consisting entirely
    % of characters (code points) satisfying `Pred'.
    %
:- func rstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % replace(String0, Search, Replace, String):
    %
    % Replace replaces the first occurrence of Search in String0
    % with Replace to give String. It fails if Search does not occur
    % in String0.
    %
:- pred replace(string::in, string::in, string::in, string::uo) is semidet.

    % replace_all(String0, Search, Replace, String):
    %
    % Replaces any occurrences of Search in String0 with Replace to give
    % String.
    %
:- func replace_all(string::in, string::in, string::in) = (string::uo) is det.
:- pred replace_all(string::in, string::in, string::in, string::uo) is det.

    % word_wrap(Str, N) = Wrapped:
    %
    % Wrapped is Str with newlines inserted between words (separated by ASCII
    % space characters) so that at most N code points appear on any line,
    % and each line contains as many whole words as possible subject to that
    % constraint. If any one word exceeds N code points in length, then
    % it will be broken over two (or more) lines. Sequences of whitespace
    % characters are replaced by a single space.
    %
:- func word_wrap(string, int) = string.

    % word_wrap_separator(Str, N, WordSeparator) = Wrapped:
    %
    % word_wrap_separator/3 is like word_wrap/2, except that words that
    % need to be broken up over multiple lines have WordSeparator inserted
    % between each piece. If the length of WordSeparator is greater than
    % or equal to N code points, then no separator is used.
    %
:- func word_wrap_separator(string, int, string) = string.

%---------------------------------------------------------------------------%
%
% Folds over the characters in strings.
%

    % foldl(Closure, String, !Acc):
    %
    % `Closure' is an accumulator predicate which is to be called for each
    % character (code point) of the string `String' in turn. The initial
    % value of the accumulator is `!.Acc' and the final value is `!:Acc'.
    % (foldl(Closure, String, !Acc)  is equivalent to
    %   to_char_list(String, Chars),
    %   list.foldl(Closure, Chars, !Acc)
    % but is implemented more efficiently.)
    %
:- func foldl(func(char, A) = A, string, A) = A.
:- pred foldl(pred(char, A, A), string, A, A).
:- mode foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldl(pred(in, in, out) is multi, in, in, out) is multi.

    % foldl2(Closure, String, !Acc1, !Acc2):
    % A variant of foldl with two accumulators.
    %
:- pred foldl2(pred(char, A, A, B, B), string, A, A, B, B).
:- mode foldl2(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldl2(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode foldl2(pred(in, in, out, in, out) is multi,
    in, in, out, in, out) is multi.

    % foldl_between(Closure, String, Start, End, !Acc)
    % is equivalent to foldl(Closure, SubString, !Acc)
    % where SubString = between(String, Start, End).
    %
    % `Start' and `End' are in terms of code units.
    %
:- func foldl_between(func(char, A) = A, string, int, int, A) = A.
:- pred foldl_between(pred(char, A, A), string, int, int, A, A).
:- mode foldl_between(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode foldl_between(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode foldl_between(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode foldl_between(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode foldl_between(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % foldl2_between(Closure, String, Start, End, !Acc1, !Acc2)
    % A variant of foldl_between with two accumulators.
    %
    % `Start' and `End' are in terms of code units.
    %
:- pred foldl2_between(pred(char, A, A, B, B),
    string, int, int, A, A, B, B).
:- mode foldl2_between(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode foldl2_between(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode foldl2_between(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode foldl2_between(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode foldl2_between(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode foldl2_between(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

    % foldl_substring(Closure, String, Start, Count, !Acc)
    % Please use foldl_between instead.
    %
:- pragma obsolete(foldl_substring/5).
:- pragma obsolete(foldl_substring/6).
:- func foldl_substring(func(char, A) = A, string, int, int, A) = A.
:- pred foldl_substring(pred(char, A, A), string, int, int, A, A).
:- mode foldl_substring(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode foldl_substring(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode foldl_substring(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode foldl_substring(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode foldl_substring(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % foldl2_substring(Closure, String, Start, Count, !Acc1, !Acc2)
    % Please use foldl2_between instead.
    %
:- pragma obsolete(foldl2_substring/8).
:- pred foldl2_substring(pred(char, A, A, B, B),
    string, int, int, A, A, B, B).
:- mode foldl2_substring(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode foldl2_substring(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode foldl2_substring(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode foldl2_substring(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode foldl2_substring(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode foldl2_substring(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

    % foldr(Closure, String, !Acc):
    % As foldl/4, except that processing proceeds right-to-left.
    %
:- func foldr(func(char, T) = T, string, T) = T.
:- pred foldr(pred(char, T, T), string, T, T).
:- mode foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode foldr(pred(in, in, out) is multi, in, in, out) is multi.

    % foldr_between(Closure, String, Start, End, !Acc)
    % is equivalent to foldr(Closure, SubString, !Acc)
    % where SubString = between(String, Start, End).
    %
    % `Start' and `End' are in terms of code units.
    %
:- func foldr_between(func(char, T) = T, string, int, int, T) = T.
:- pred foldr_between(pred(char, T, T), string, int, int, T, T).
:- mode foldr_between(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode foldr_between(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode foldr_between(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode foldr_between(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode foldr_between(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % foldr_substring(Closure, String, Start, Count, !Acc)
    % Please use foldr_between instead.
    %
:- pragma obsolete(foldr_substring/5).
:- pragma obsolete(foldr_substring/6).
:- func foldr_substring(func(char, T) = T, string, int, int, T) = T.
:- pred foldr_substring(pred(char, T, T), string, int, int, T, T).
:- mode foldr_substring(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode foldr_substring(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode foldr_substring(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode foldr_substring(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode foldr_substring(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

%---------------------------------------------------------------------------%
%
% Formatting tables.
%

:- type justified_column
    --->    left(list(string))
    ;       right(list(string)).

    % format_table(Columns, Separator) = Table:
    %
    % This function takes a list of columns and a column separator,
    % and returns a formatted table, where each field in each column
    % has been aligned and fields are separated with Separator.
    % There will be a newline character between each pair of rows.
    % Throws an exception If the columns are not all the same length.
    % Lengths are currently measured in terms of code points.
    %
    % For example:
    %
    % format_table([right(["a", "bb", "ccc"]), left(["1", "22", "333"])],
    %   " * ")
    % would return the table:
    %   a * 1
    %  bb * 22
    % ccc * 333
    %
:- func format_table(list(justified_column), string) = string.

    % format_table_max(Columns, Separator) does the same job as format_table,
    % but allows the caller to associate an maximum width with each column.
    %
:- func format_table_max(assoc_list(justified_column, maybe(int)), string)
    = string.

%---------------------------------------------------------------------------%
%
% Converting strings to docs.
%

    % Convert a string to a pretty_printer.doc for formatting.
    %
:- func string_to_doc(string) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%
% Converting strings to values of builtin types.
%

    % Convert a string to an int. The string must contain only digits [0-9],
    % optionally preceded by a plus or minus sign. If the string does
    % not match this syntax or the number is not in the range
    % [min_int + 1, max_int], to_int fails.
    %
:- pred to_int(string::in, int::out) is semidet.

    % Convert a signed base 10 string to an int. Throws an exception if the
    % string argument does not match the regexp [+-]?[0-9]+ or the number is
    % not in the range [min_int + 1, max_int].
    %
:- func det_to_int(string) = int.

    % Convert a string in the specified base (2-36) to an int. The string
    % must contain one or more digits in the specified base, optionally
    % preceded by a plus or minus sign. For bases > 10, digits 10 to 35
    % are represented by the letters A-Z or a-z. If the string does not match
    % this syntax or the base is 10 and the number is not in the range
    % [min_int, max_int], the predicate fails.
    %
:- pred base_string_to_int(int::in, string::in, int::out) is semidet.

    % Convert a signed base N string to an int. Throws an exception
    % if the string argument is not precisely an optional sign followed by
    % a non-empty string of base N digits and, if the base is 10, the number
    % is in the range [min_int, max_int].
    %
:- func det_base_string_to_int(int, string) = int.

    % Convert a string to a float. If the string is not a syntactically
    % correct float literal, to_float fails.
    %
:- pred to_float(string::in, float::out) is semidet.

    % Convert a string to a float. Throws an exception if the string is not
    % a syntactically correct float literal.
    %
:- func det_to_float(string) = float.

%---------------------------------------------------------------------------%
%
% Converting values of builtin types to strings.
%

    % char_to_string(Char, String):
    %
    % Converts a character (code point) to a string, or vice versa.
    %
:- func char_to_string(char::in) = (string::uo) is det.
:- pred char_to_string(char, string).
:- mode char_to_string(in, uo) is det.
:- mode char_to_string(out, in) is semidet.

    % A synonym for char_to_string/1.
    %
:- func from_char(char::in) = (string::uo) is det.

    % Convert an integer to a string.
    %
:- func int_to_string(int::in) = (string::uo) is det.
:- pred int_to_string(int::in, string::uo) is det.

    % A synonym for int_to_string/1.
    %
:- func from_int(int::in) = (string::uo) is det.

    % int_to_base_string(Int, Base, String):
    %
    % Convert an integer to a string in a given Base.
    %
    % Base must be between 2 and 36, both inclusive; if it isn't,
    % the predicate will throw an exception.
    %
:- func int_to_base_string(int::in, int::in) = (string::uo) is det.
:- pred int_to_base_string(int::in, int::in, string::uo) is det.

    % Convert an integer to a string with commas as thousand separators.
    %
:- func int_to_string_thousands(int::in) = (string::uo) is det.

    % int_to_base_string_group(Int, Base, GroupLength, Separator, String):
    %
    % Convert an integer to a string in a given Base
    % and insert Separator between every GroupLength digits.
    % If GroupLength is less than one, no separators will appear
    % in the output. Useful for formatting numbers like "1,300,000".
    %
    % Base must be between 2 and 36, both inclusive; if it isn't,
    % the predicate will throw an exception.
    %
:- func int_to_base_string_group(int, int, int, string) = string.
:- mode int_to_base_string_group(in, in, in, in) = uo is det.

    % Convert a float to a string.
    % In the current implementation, the resulting float will be in the form
    % that it was printed using the format string "%#.<prec>g".
    % <prec> will be in the range p to (p+2)
    % where p = floor(mantissa_digits * log2(base_radix) / log2(10)).
    % The precision chosen from this range will be such as to allow
    % a successful decimal -> binary conversion of the float.
    %
:- func float_to_string(float::in) = (string::uo) is det.
:- pred float_to_string(float::in, string::uo) is det.

    % A synonym for float_to_string/1.
    %
:- func from_float(float::in) = (string::uo) is det.

    % Convert a c_pointer to a string. The format is "c_pointer(0xXXXX)"
    % where XXXX is the hexadecimal representation of the pointer.
    %
:- func c_pointer_to_string(c_pointer::in) = (string::uo) is det.
:- pred c_pointer_to_string(c_pointer::in, string::uo) is det.

    % A synonym for c_pointer_to_string/1.
    %
:- func from_c_pointer(c_pointer::in) = (string::uo) is det.

%---------------------------------------------------------------------------%
%
% Converting values of arbitrary types to strings.
%

    % string(X): Returns a canonicalized string representation of the value X
    % using the standard Mercury operators.
    %
:- func string(T) = string.

    % As above, but using the supplied table of operators.
    %
:- func string_ops(ops.table, T) = string.

    % string_ops_noncanon(NonCanon, OpsTable, X, String)
    %
    % As above, but the caller specifies what behaviour should occur for
    % non-canonical terms (i.e. terms where multiple representations
    % may compare as equal):
    %
    % - `do_not_allow' will throw an exception if (any subterm of)
    %    the argument is not canonical;
    % - `canonicalize' will substitute a string indicating the presence
    %    of a non-canonical subterm;
    % - `include_details_cc' will show the structure of any non-canonical
    %   subterms, but can only be called from a committed choice context.
    %
:- pred string_ops_noncanon(noncanon_handling, ops.table, T, string).
:- mode string_ops_noncanon(in(do_not_allow), in, in, out) is det.
:- mode string_ops_noncanon(in(canonicalize), in, in, out) is det.
:- mode string_ops_noncanon(in(include_details_cc), in, in, out) is cc_multi.
:- mode string_ops_noncanon(in, in, in, out) is cc_multi.

%---------------------------------------------------------------------------%
%
% Converting values to strings based on a format string.
%

% NOTE_TO_IMPLEMENTORS If you modify this type, you will also need to modify
% NOTE_TO_IMPLEMENTORS the type that represents abstract poly_types,
% NOTE_TO_IMPLEMENTORS abstract_poly_type in compiler/parse_string_format.m,
% NOTE_TO_IMPLEMENTORS as well as the predicates that parse concrete and
% NOTE_TO_IMPLEMENTORS abstract poly_types, in library/string.parse_runtime.m
% NOTE_TO_IMPLEMENTORS and in compiler/parse_string_format.m.
:- type poly_type
    --->    f(float)
    ;       i(int)
    ;       s(string)
    ;       c(char).

    % A function similar to sprintf() in C.
    %
    % For example,
    %   format("%s %i %c %f\n",
    %       [s("Square-root of"), i(2), c('='), f(1.41)], String)
    % will return
    %   String = "Square-root of 2 = 1.41\n".
    %
    % The following options available in C are supported: flags [0+-# ],
    % a field width (or *), and a precision (could be a ".*").
    %
    % Valid conversion character types are {dioxXucsfeEgGp%}. %n is not
    % supported. format will not return the length of the string.
    %
    % conv  var     output form.        effect of '#'.
    % char. type.
    %
    % d     int     signed integer
    % i     int     signed integer
    % o     int     signed octal        with '0' prefix
    % x,X   int     signed hex          with '0x', '0X' prefix
    % u     int     unsigned integer
    % c     char    character
    % s     string  string
    % f     float   rational number     with '.', if precision 0
    % e,E   float   [-]m.dddddE+-xx     with '.', if precision 0
    % g,G   float   either e or f       with trailing zeros.
    % p     int     integer
    %
    % An option of zero will cause any padding to be zeros rather than spaces.
    % A '-' will cause the output to be left-justified in its 'space'.
    % (Without a `-', the default is for fields to be right-justified.)
    % A '+' forces a sign to be printed. This is not sensible for string
    % and character output. A ' ' causes a space to be printed before a thing
    % if there is no sign there. The other option is the '#', which modifies
    % the output string's format. These options are normally put directly
    % after the '%'.
    %
    % Notes:
    %
    % %#.0e, %#.0E now prints a '.' before the 'e'.
    %
    % Asking for more precision than a float actually has will result in
    % potentially misleading output.
    %
    % Numbers are now rounded by precision value, not truncated as previously.
    %
    % The implementation uses the sprintf() function in C grades,
    % so the actual output will depend on the C standard library.
    %
:- func format(string, list(poly_type)) = string.
:- pred format(string::in, list(poly_type)::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% The modules string.format and string.parse_util have to be visible
% from outside the string module, since they need to be visible to the
% compiler (specifically, to format_call.m and its submodule
% parse_format_string.m.). However, they should not be part of the
% publically documented interface of the Mercury standard library,
% since we don't want any user code to depend on the implementation
% details they contain.
:- interface.
:- include_module format.
:- include_module parse_util.
:- implementation.

:- include_module parse_runtime.
:- include_module to_string.

:- import_module array.
:- import_module bitmap.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.format.
:- import_module string.to_string.
:- import_module type_desc.
:- import_module univ.
:- import_module version_array.

% Many routines in this module are implemented using foreign language code.

:- pragma foreign_decl("C",
"
#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include ""mercury_string.h""   /* for MR_allocate_aligned_string*() etc. */
#include ""mercury_tags.h""     /* for MR_list_cons*() */
").

%---------------------------------------------------------------------------%
%
% Conversions between strings and lists of characters.
%

string.to_char_list(S) = Cs :-
    string.to_char_list(S, Cs).

:- pragma promise_equivalent_clauses(string.to_char_list/2).

string.to_char_list(Str::in, CharList::out) :-
    string.to_char_list_forward(Str, CharList).
string.to_char_list(Str::uo, CharList::in) :-
    string.from_char_list(CharList, Str).

:- pred string.to_char_list_forward(string::in, list(char)::out) is det.

:- pragma foreign_proc("C",
    string.to_char_list_forward(Str::in, CharList::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_Integer pos = strlen(Str);
    int c;

    CharList = MR_list_empty_msg(MR_ALLOC_ID);
    for (;;) {
        c = MR_utf8_prev_get(Str, &pos);
        if (c <= 0) {
            break;
        }
        CharList = MR_char_list_cons_msg((MR_UnsignedChar) c, CharList,
            MR_ALLOC_ID);
    }
}").
:- pragma foreign_proc("C#",
    string.to_char_list_forward(Str::in, CharList::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    list.List_1 lst = list.empty_list();
    for (int i = Str.Length - 1; i >= 0; i--) {
        int c;
        char c2 = Str[i];
        if (System.Char.IsLowSurrogate(c2)) {
            try {
                char c1 = Str[i - 1];
                c = System.Char.ConvertToUtf32(c1, c2);
                i--;
            } catch (System.ArgumentOutOfRangeException) {
                c = 0xfffd;
            } catch (System.IndexOutOfRangeException) {
                c = 0xfffd;
            }
        } else if (System.Char.IsHighSurrogate(c2)) {
            c = 0xfffd;
        } else {
            c = c2;
        }
        lst = list.cons(c, lst);
    }
    CharList = lst;
").
:- pragma foreign_proc("Java",
    string.to_char_list_forward(Str::in, CharList::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    list.List_1<Integer> lst = list.empty_list();
    for (int i = Str.length(); i > 0; ) {
        int c = Str.codePointBefore(i);
        lst = list.cons(c, lst);
        i -= java.lang.Character.charCount(c);
    }
    CharList = lst;
").
:- pragma foreign_proc("Erlang",
    string.to_char_list_forward(Str::in, CharList::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    CharList = unicode:characters_to_list(Str)
").

string.to_char_list_forward(Str, CharList) :-
    string.foldr(list.cons, Str, [], CharList).

%---------------------%

string.from_char_list(Cs) = S :-
    string.from_char_list(Cs, S).

:- pragma promise_equivalent_clauses(string.from_char_list/2).

string.from_char_list(Chars::out, Str::in) :-
    string.to_char_list(Str, Chars).
string.from_char_list(Chars::in, Str::uo) :-
    ( string.semidet_from_char_list(Chars, Str0) ->
        Str = Str0
    ;
        error("string.from_char_list: null character in list")
    ).

:- pragma foreign_proc("C",
    string.semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    /* mode (uo, in) is det */
    MR_Word char_list_ptr;
    size_t size;
    MR_Char c;

    /*
    ** Loop to calculate list length + sizeof(MR_Word) in `size'
    ** using list in `char_list_ptr'.
    */
    size = 0;
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        c = (MR_Char) MR_list_head(char_list_ptr);
        if (MR_is_ascii(c)) {
            size++;
        } else {
            size += MR_utf8_width(c);
        }
        char_list_ptr = MR_list_tail(char_list_ptr);
    }

    /*
    ** Allocate heap space for string
    */
    MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

    /*
    ** Loop to copy the characters from the char_list to the string.
    */
    SUCCESS_INDICATOR = MR_TRUE;
    size = 0;
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        c = (MR_Char) MR_list_head(char_list_ptr);
        /*
        ** It is an error to put a null character in a string
        ** (see the comments at the top of this file).
        */
        if (c == '\\0') {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
        if (MR_is_ascii(c)) {
            Str[size] = c;
            size++;
        } else {
            size += MR_utf8_encode(Str + size, c);
        }
        char_list_ptr = MR_list_tail(char_list_ptr);
    }

    Str[size] = '\\0';
}").
:- pragma foreign_proc("C#",
    string.semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = true;
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    while (!list.is_empty(CharList)) {
        int cp = (int) list.det_head(CharList);
        if (cp == 0x0000) {
            SUCCESS_INDICATOR = false;
            break;
        } else if (cp <= 0xffff) {
            sb.Append((char) cp);
        } else {
            sb.Append(System.Char.ConvertFromUtf32(cp));
        }
        CharList = list.det_tail(CharList);
    }
    Str = sb.ToString();
").
:- pragma foreign_proc("Java",
    string.semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    java.lang.StringBuilder sb = new StringBuilder();
    Iterable<Integer> iterable = new list.ListIterator<Integer>(CharList);
    SUCCESS_INDICATOR = true;
    for (int c : iterable) {
        if (c == 0x0000) {
            SUCCESS_INDICATOR = false;
            break;
        } else if (c <= 0xffff) {
            /* Fast path. */
            sb.append((char) c);
        } else {
            sb.append(java.lang.Character.toChars(c));
        }
    }
    Str = sb.toString();
").
:- pragma foreign_proc("Erlang",
    string.semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = unicode:characters_to_binary(CharList),
    SUCCESS_INDICATOR = true
").

string.semidet_from_char_list(CharList, Str) :-
    (
        CharList = [],
        Str = ""
    ;
        CharList = [C | Cs],
        \+ char.to_int(C, 0),
        string.semidet_from_char_list(Cs, Str0),
        string.first_char(Str, C, Str0)
    ).

%---------------------%
%
% We could implement from_rev_char_list using list.reverse and from_char_list,
% but the optimized implementation in C below is there for efficiency.
% At the time this predicate was added, it improved the overall speed of
% parsing by about 7%.
%

string.from_rev_char_list(Cs) = S :-
    string.from_rev_char_list(Cs, S).

string.from_rev_char_list(Chars, Str) :-
    ( string.semidet_from_rev_char_list(Chars, Str0) ->
        Str = Str0
    ;
        error("string.from_rev_char_list: null character in list")
    ).

:- pragma foreign_proc("C",
    string.semidet_from_rev_char_list(Chars::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list_ptr;
    MR_Word size;
    MR_Char c;

    /*
    ** Loop to calculate list length in `size' using list in `list_ptr'
    */
    size = 0;
    list_ptr = Chars;
    while (!MR_list_is_empty(list_ptr)) {
        c = (MR_Char) MR_list_head(list_ptr);
        if (MR_is_ascii(c)) {
            size++;
        } else {
            size += MR_utf8_width(c);
        }
        list_ptr = MR_list_tail(list_ptr);
    }

    /*
    ** Allocate heap space for string
    */
    MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

    /*
    ** Set size to be the offset of the end of the string
    ** (ie the \\0) and null terminate the string.
    */
    Str[size] = '\\0';

    /*
    ** Loop to copy the characters from the list_ptr to the string
    ** in reverse order.
    */
    list_ptr = Chars;
    SUCCESS_INDICATOR = MR_TRUE;
    while (!MR_list_is_empty(list_ptr)) {
        c = (MR_Char) MR_list_head(list_ptr);
        if (c == '\\0') {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
        if (MR_is_ascii(c)) {
            size--;
            Str[size] = c;
        } else {
            size -= MR_utf8_width(c);
            MR_utf8_encode(Str + size, c);
        }
        list_ptr = MR_list_tail(list_ptr);
    }
}").
:- pragma foreign_proc("C#",
    string.semidet_from_rev_char_list(Chars::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    int size = 0;
    list.List_1 list_ptr = Chars;
    while (!list.is_empty(list_ptr)) {
        int c = (int) list.det_head(list_ptr);
        if (c <= 0xffff) {
            size++;
        } else {
            size += 2;
        }
        list_ptr = list.det_tail(list_ptr);
    }

    char[] arr = new char[size];
    list_ptr = Chars;
    SUCCESS_INDICATOR = true;
    while (!list.is_empty(list_ptr)) {
        int c = (int) list.det_head(list_ptr);
        if (c == 0x0000) {
            SUCCESS_INDICATOR = false;
            break;
        } else if (c <= 0xffff) {
            arr[--size] = (char) c;
        } else {
            string s = System.Char.ConvertFromUtf32(c);
            arr[--size] = s[1];
            arr[--size] = s[0];
        }
        list_ptr = list.det_tail(list_ptr);
    }

    Str = new string(arr);
").

string.semidet_from_rev_char_list(Chars::in, Str::uo) :-
    string.semidet_from_char_list(list.reverse(Chars), Str).

%---------------------%

string.to_code_unit_list(String, List) :-
    string.to_code_unit_list_loop(String, 0, string.length(String), List).

:- pred string.to_code_unit_list_loop(string::in, int::in, int::in,
    list(int)::out) is det.

string.to_code_unit_list_loop(String, Index, End, List) :-
    ( Index >= End ->
        List = []
    ;
        string.unsafe_index_code_unit(String, Index, Code),
        string.to_code_unit_list_loop(String, Index + 1, End, Tail),
        List = [Code | Tail]
    ).

%---------------------%

:- pragma foreign_proc("C",
    string.from_code_unit_list(CodeList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    MR_Word list_ptr;
    size_t  size;

    size = 0;
    list_ptr = CodeList;
    while (! MR_list_is_empty(list_ptr)) {
        size++;
        list_ptr = MR_list_tail(list_ptr);
    }

    MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

    SUCCESS_INDICATOR = MR_TRUE;
    size = 0;
    list_ptr = CodeList;
    while (! MR_list_is_empty(list_ptr)) {
        int c;
        c = MR_list_head(list_ptr);
        /*
        ** It is an error to put a null character in a string
        ** (see the comments at the top of this file).
        */
        if (c == '\\0' || c > 0xff) {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
        Str[size] = c;
        size++;
        list_ptr = MR_list_tail(list_ptr);
    }

    Str[size] = '\\0';

    SUCCESS_INDICATOR = SUCCESS_INDICATOR && MR_utf8_verify(Str);
").
:- pragma foreign_proc("Java",
    string.from_code_unit_list(CodeList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    java.lang.StringBuilder sb = new java.lang.StringBuilder();
    boolean prev_high = false;

    SUCCESS_INDICATOR = true;

    Iterable<Integer> iterable = new list.ListIterator<Integer>(CodeList);
    for (int i : iterable) {
        char c = (char) i;
        if (prev_high) {
            if (!java.lang.Character.isLowSurrogate(c)) {
                SUCCESS_INDICATOR = false;
                break;
            }
            prev_high = false;
        } else if (java.lang.Character.isHighSurrogate(c)) {
            prev_high = true;
        } else if (java.lang.Character.isLowSurrogate(c)) {
            SUCCESS_INDICATOR = false;
            break;
        }
        sb.append(c);
    }

    SUCCESS_INDICATOR = SUCCESS_INDICATOR && !prev_high;

    if (SUCCESS_INDICATOR) {
        Str = sb.toString();
    } else {
        Str = """";
    }
").
:- pragma foreign_proc("C#",
    string.from_code_unit_list(CodeList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    bool prev_high = false;

    SUCCESS_INDICATOR = true;

    while (!list.is_empty(CodeList)) {
        /* Both casts are required. */
        char c = (char) (int) list.det_head(CodeList);
        if (prev_high) {
            if (!System.Char.IsLowSurrogate(c)) {
                SUCCESS_INDICATOR = false;
                break;
            }
            prev_high = false;
        } else if (System.Char.IsHighSurrogate(c)) {
            prev_high = true;
        } else if (System.Char.IsLowSurrogate(c)) {
            SUCCESS_INDICATOR = false;
            break;
        }
        sb.Append(c);
        CodeList = list.det_tail(CodeList);
    }

    SUCCESS_INDICATOR = SUCCESS_INDICATOR && !prev_high;

    if (SUCCESS_INDICATOR) {
        Str = sb.ToString();
    } else {
        Str = """";
    }
").
:- pragma foreign_proc("Erlang",
    string.from_code_unit_list(CodeList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = list_to_binary(CodeList),
    % XXX validate the string
    SUCCESS_INDICATOR = true
").

%---------------------%

string.duplicate_char(C, N) = S :-
    string.duplicate_char(C, N, S).

string.duplicate_char(Char, Count, String) :-
    String = string.from_char_list(list.duplicate(Count, Char)).

%---------------------------------------------------------------------------%
%
% Reading characters from strings.
%

% It is important to inline predicates that index into strings,
% so that the compiler can do loop invariant hoisting on calls to them
% that occur in loops.

:- pragma inline(string.index/3).
:- pragma inline(string.det_index/3).
:- pragma inline(string.index_next/4).
:- pragma inline(string.prev_index/4).

string.index(Str, Index, Char) :-
    Len = string.length(Str),
    ( string.index_check(Index, Len) ->
        string.unsafe_index(Str, Index, Char)
    ;
        fail
    ).

string.det_index(S, N) = C :-
    string.det_index(S, N, C).

string.det_index(String, Int, Char) :-
    ( string.index(String, Int, Char0) ->
        Char = Char0
    ;
        error("string.det_index: index out of range")
    ).

string.unsafe_index(S, N) = C :-
    string.unsafe_index(S, N, C).

string.unsafe_index(Str, Index, Char) :-
    ( string.unsafe_index_2(Str, Index, CharPrime) ->
        Char = CharPrime
    ;
        error("string.unsafe_index: illegal sequence")
    ).

:- pred string.unsafe_index_2(string::in, int::in, char::uo) is semidet.

:- pragma foreign_proc("C",
    string.unsafe_index_2(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Ch = Str[Index];
    if (!MR_is_ascii(Ch)) {
        int width;
        Ch = MR_utf8_get_mb(Str, Index, &width);
    }
    SUCCESS_INDICATOR = (Ch > 0);
").
:- pragma foreign_proc("C#",
    string.unsafe_index_2(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    char c1 = Str[Index];
    if (System.Char.IsSurrogate(c1)) {
        try {
            char c2 = Str[Index + 1];
            Ch = System.Char.ConvertToUtf32(c1, c2);
        } catch (System.ArgumentOutOfRangeException) {
            Ch = -1;
        } catch (System.IndexOutOfRangeException) {
            Ch = -1;
        }
    } else {
        /* Common case. */
        Ch = c1;
    }
    SUCCESS_INDICATOR = (Ch >= 0);
").
:- pragma foreign_proc("Java",
    string.unsafe_index_2(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ch = Str.codePointAt(Index);
    SUCCESS_INDICATOR =
        !java.lang.Character.isHighSurrogate((char) Ch) &&
        !java.lang.Character.isLowSurrogate((char) Ch);
").
:- pragma foreign_proc("Erlang",
    string.unsafe_index_2(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<_:Index/binary, Ch/utf8, _/binary>> = Str,
    SUCCESS_INDICATOR = true
").

%---------------------%

String ^ elem(Index) = det_index(String, Index).
String ^ unsafe_elem(Index) = unsafe_index(String, Index).

%---------------------%

string.index_next(Str, Index, NextIndex, Char) :-
    Len = string.length(Str),
    ( string.index_check(Index, Len) ->
        string.unsafe_index_next(Str, Index, NextIndex, Char)
    ;
        fail
    ).

:- pragma foreign_proc("C",
    string.unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Ch = Str[Index];
    if (MR_is_ascii(Ch)) {
        NextIndex = Index + 1;
        SUCCESS_INDICATOR = (Ch != 0);
    } else {
        NextIndex = Index;
        Ch = MR_utf8_get_next_mb(Str, &NextIndex);
        SUCCESS_INDICATOR = (Ch > 0);
    }
").
:- pragma foreign_proc("C#",
    string.unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index < Str.Length) {
        Ch = System.Char.ConvertToUtf32(Str, Index);
        if (Ch <= 0xffff) {
            NextIndex = Index + 1;
        } else {
            NextIndex = Index + 2;
        }
        SUCCESS_INDICATOR = true;
    } else {
        Ch = -1;
        NextIndex = Index;
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Java",
    string.unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index < Str.length()) {
        Ch = Str.codePointAt(Index);
        SUCCESS_INDICATOR =
            !java.lang.Character.isHighSurrogate((char) Ch) &&
            !java.lang.Character.isLowSurrogate((char) Ch);
        if (SUCCESS_INDICATOR) {
            NextIndex = Index + java.lang.Character.charCount(Ch);
        } else {
            Ch = -1;
            NextIndex = Index;
        }
    } else {
        Ch = -1;
        NextIndex = Index;
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Erlang",
    string.unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    case Str of
        << _:Index/binary, Ch/utf8, _/binary >> ->
            if
                Ch =< 16#7f ->
                    NextIndex = Index + 1;
                Ch =< 16#7ff ->
                    NextIndex = Index + 2;
                Ch =< 16#ffff ->
                    NextIndex = Index + 3;
                true ->
                    NextIndex = Index + 4
            end,
            SUCCESS_INDICATOR = true;
        _ ->
            Ch = -1,
            NextIndex = Index,
            SUCCESS_INDICATOR = false
    end
").

string.prev_index(Str, Index, CharIndex, Char) :-
    Len = string.length(Str),
    ( string.index_check(Index - 1, Len) ->
        string.unsafe_prev_index(Str, Index, CharIndex, Char)
    ;
        fail
    ).

:- pragma foreign_proc("C",
    string.unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index > 0) {
        PrevIndex = Index - 1;
        Ch = Str[PrevIndex];
        if (MR_is_ascii(Ch)) {
            SUCCESS_INDICATOR = (Ch != 0);
        } else {
            Ch = MR_utf8_prev_get(Str, &PrevIndex);
            SUCCESS_INDICATOR = (Ch > 0);
        }
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").
:- pragma foreign_proc("C#",
    string.unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index > 0) {
        char c2 = Str[Index - 1];
        if (System.Char.IsSurrogate(c2)) {
            try {
                char c1 = Str[Index - 2];
                Ch = System.Char.ConvertToUtf32(c1, c2);
                PrevIndex = Index - 2;
            } catch (System.IndexOutOfRangeException) {
                Ch = -1;
                PrevIndex = Index;
                SUCCESS_INDICATOR = false;
            } catch (System.ArgumentOutOfRangeException) {
                Ch = -1;
                PrevIndex = Index;
                SUCCESS_INDICATOR = false;
            }
        } else {
            /* Common case. */
            Ch = (int) c2;
            PrevIndex = Index - 1;
        }
        SUCCESS_INDICATOR = true;
    } else {
        Ch = -1;
        PrevIndex = Index;
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Java",
    string.unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index > 0) {
        Ch = Str.codePointBefore(Index);
        PrevIndex = Index - java.lang.Character.charCount(Ch);
        SUCCESS_INDICATOR = true;
    } else {
        Ch = -1;
        PrevIndex = Index;
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Erlang",
    string.unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    {PrevIndex, Ch} = do_unsafe_prev_index(Str, Index - 1),
    SUCCESS_INDICATOR = (Ch =/= -1)
").

:- pragma foreign_code("Erlang", "
do_unsafe_prev_index(Str, Index) ->
    if Index >= 0 ->
        case Str of
            <<_:Index/binary, Ch/integer, _/binary>> ->
                if
                    (Ch band 16#80) =:= 0 ->
                        {Index, Ch};
                    (Ch band 16#C0) == 16#80 ->
                        do_unsafe_prev_index(Str, Index - 1);
                    true ->
                        <<_:Index/binary, Ch2/utf8, _/binary>> = Str,
                        {Index, Ch2}
                end;
            true ->
                {Index, -1}
        end
    ; true ->
        {Index, -1}
    end.
").

    % XXX We should consider making this routine a compiler built-in.
    %
:- pred string.index_check(int::in, int::in) is semidet.

:- pragma foreign_proc("C",
    string.index_check(Index::in, Length::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    /*
    ** We do not test for negative values of Index because (a) MR_Unsigned
    ** is unsigned and hence a negative argument will appear as a very large
    ** positive one after the cast and (b) anybody dealing with the case
    ** where strlen(Str) > MAXINT is clearly barking mad (and one may well get
    ** an integer overflow error in this case).
    */
    SUCCESS_INDICATOR = ((MR_Unsigned) Index < (MR_Unsigned) Length);
").
:- pragma foreign_proc("C#",
    string.index_check(Index::in, Length::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((uint) Index < (uint) Length);
").

string.index_check(Index, Length) :-
    Index >= 0,
    Index < Length.

%---------------------%

:- pragma foreign_proc("C",
    string.unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const unsigned char *s = (const unsigned char *)Str;
    Code = s[Index];
").
:- pragma foreign_proc("C#",
    string.unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Code = Str[Index];
").
:- pragma foreign_proc("Java",
    string.unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Code = Str.charAt(Index);
").
:- pragma foreign_proc("Erlang",
    string.unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<_:Index/binary, Code/integer, _/binary>> = Str
").

%---------------------------------------------------------------------------%
%
% Writing characters to strings.
%

string.set_char(Char, Index, !Str) :-
    ( char.to_int(Char, 0) ->
        error("string.set_char: null character")
    ;
        string.set_char_non_null(Char, Index, !Str)
    ).

:- pred string.set_char_non_null(char, int, string, string).
:- mode string.set_char_non_null(in, in, in, out) is semidet.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode string.set_char_non_null(in, in, di, uo) is semidet.

:- pragma foreign_proc("C",
    string.set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    size_t len = strlen(Str0);
    if ((MR_Unsigned) Index >= len) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (MR_is_ascii(Str0[Index]) && MR_is_ascii(Ch)) {
        /* Fast path. */
        SUCCESS_INDICATOR = MR_TRUE;
        MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);
        strcpy(Str, Str0);
        Str[Index] = Ch;
    } else {
        int oldc = MR_utf8_get(Str0, Index);
        if (oldc < 0) {
            SUCCESS_INDICATOR = MR_FALSE;
        } else {
            size_t oldwidth = MR_utf8_width(oldc);
            size_t newwidth = MR_utf8_width(Ch);
            size_t newlen;

            newlen = len - oldwidth + newwidth;
            MR_allocate_aligned_string_msg(Str, newlen, MR_ALLOC_ID);
            MR_memcpy(Str, Str0, Index);
            MR_utf8_encode(Str + Index, Ch);
            strcpy(Str + Index + newwidth, Str0 + Index + oldwidth);
            SUCCESS_INDICATOR = MR_TRUE;
        }
    }
").
:- pragma foreign_proc("C#",
    string.set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Index < 0 || Index >= Str0.Length) {
        Str = null;
        SUCCESS_INDICATOR = false;
    } else {
        System.Text.StringBuilder sb = new System.Text.StringBuilder(Str0);
        if (!System.Char.IsHighSurrogate(Str0, Index) && Ch <= 0xffff) {
            /* Fast path. */
            sb[Index] = (char) Ch;
        } else {
            if (Str0.Length > Index + 1 &&
                System.Char.IsLowSurrogate(Str0, Index + 1))
            {
                sb.Remove(Index, 2);
            } else {
                sb.Remove(Index, 1);
            }
            sb.Insert(Index, System.Char.ConvertFromUtf32(Ch));
        }
        Str = sb.ToString();
        SUCCESS_INDICATOR = true;
    }
").
:- pragma foreign_proc("Java",
    string.set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Index < 0 || Index >= Str0.length()) {
        Str = null;
        SUCCESS_INDICATOR = false;
    } else {
        java.lang.StringBuilder sb = new StringBuilder(Str0);

        int oldc = sb.codePointAt(Index);
        int oldwidth = java.lang.Character.charCount(oldc);
        int newwidth = java.lang.Character.charCount(Ch);
        if (oldwidth == 1 && newwidth == 1) {
            sb.setCharAt(Index, (char) Ch);
        } else {
            char[] buf = java.lang.Character.toChars(Ch);
            sb.replace(Index, Index + oldwidth, new String(buf));
        }

        Str = sb.toString();
        SUCCESS_INDICATOR = true;
    }
").
:- pragma foreign_proc("Erlang",
    string.set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case Str0 of
        <<Left:Index/binary, _/utf8, Right/binary>> ->
            Str = unicode:characters_to_binary([Left, Ch, Right]),
            SUCCESS_INDICATOR = true;
        _ ->
            Str = <<>>,
            SUCCESS_INDICATOR = false
    end
").

string.det_set_char(C, N, S0) = S :-
    string.det_set_char(C, N, S0, S).

string.det_set_char(Char, Int, String0, String) :-
    ( string.set_char(Char, Int, String0, String1) ->
        String = String1
    ;
        error("string.det_set_char: index out of range")
    ).

%---------------------%

string.unsafe_set_char(C, N, S0) = S :-
    string.unsafe_set_char(C, N, S0, S).

string.unsafe_set_char(Char, Index, !Str) :-
    ( char.to_int(Char, 0) ->
        error("string.unsafe_set_char: null character")
    ;
        string.unsafe_set_char_non_null(Char, Index, !Str)
    ).

:- pred string.unsafe_set_char_non_null(char, int, string, string).
:- mode string.unsafe_set_char_non_null(in, in, in, out) is det.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode string.unsafe_set_char_non_null(in, in, di, uo) is det.

:- pragma foreign_proc("C",
    string.unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    size_t len = strlen(Str0);
    if (MR_is_ascii(Str0[Index]) && MR_is_ascii(Ch)) {
        /* Fast path. */
        MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);
        strcpy(Str, Str0);
        Str[Index] = Ch;
    } else {
        int oldc = MR_utf8_get(Str0, Index);
        size_t oldwidth = MR_utf8_width(oldc);
        size_t newwidth = MR_utf8_width(Ch);
        size_t newlen;
        size_t tailofs;

        newlen = len - oldwidth + newwidth;
        MR_allocate_aligned_string_msg(Str, newlen, MR_ALLOC_ID);
        MR_memcpy(Str, Str0, Index);
        MR_utf8_encode(Str + Index, Ch);
        strcpy(Str + Index + newwidth, Str0 + Index + oldwidth);
    }
").
:- pragma foreign_proc("C#",
    string.unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (System.Char.IsHighSurrogate(Str0, Index)) {
        Str = System.String.Concat(
            Str0.Substring(0, Index),
            System.Char.ConvertFromUtf32(Ch),
            Str0.Substring(Index + 2)
        );
    } else {
        Str = System.String.Concat(
            Str0.Substring(0, Index),
            System.Char.ConvertFromUtf32(Ch),
            Str0.Substring(Index + 1)
        );
    }
").
:- pragma foreign_proc("Java",
    string.unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int oldc = Str0.codePointAt(Index);
    int oldwidth = java.lang.Character.charCount(oldc);
    Str = Str0.subSequence(0, Index)
        + new String(Character.toChars(Ch))
        + Str0.subSequence(Index + oldwidth, Str0.length());
").
:- pragma foreign_proc("Erlang",
    string.unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<Left:Index/binary, _/utf8, Right/binary>> = Str0,
    Str = unicode:characters_to_binary([Left, Ch, Right])
").

%---------------------------------------------------------------------------%
%
% Determining the lengths of strings.
%

string.length(S) = L :-
    string.length(S, L).

:- pragma promise_equivalent_clauses(string.length/2).

:- pragma foreign_proc("C",
    string.length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Length = strlen(Str);
").
:- pragma foreign_proc("C#",
    string.length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.Length;
").
:- pragma foreign_proc("Java",
    string.length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.length();
").
:- pragma foreign_proc("Erlang",
    string.length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = size(Str)
").

:- pragma foreign_proc("C",
    string.length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Length = strlen(Str);
").
:- pragma foreign_proc("C#",
    string.length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.Length;
").
:- pragma foreign_proc("Java",
    string.length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.length();
").
:- pragma foreign_proc("Erlang",
    string.length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = size(Str)
").

string.length(Str, Len) :-
    string.to_code_unit_list(Str, CodeList),
    list.length(CodeList, Len).

string.count_code_units(Str) = string.length(Str).

string.count_code_units(Str, Length) :-
    string.length(Str, Length).

%---------------------%

string.count_codepoints(String) = Count :-
    string.count_codepoints(String, Count).

:- pragma foreign_proc("C",
    string.count_codepoints(String::in, Count::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    unsigned char   b;
    int             i;

    Count = 0;
    for (i = 0; ; i++) {
        b = String[i];
        if (b == '\\0') {
            break;
        }
        if (MR_utf8_is_single_byte(b) || MR_utf8_is_lead_byte(b)) {
            Count++;
        }
    }
").
:- pragma foreign_proc("C#",
    string.count_codepoints(String::in, Count::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Count = 0;
    foreach (char c in String) {
        if (!System.Char.IsLowSurrogate(c)) {
            Count++;
        }
    }
").
:- pragma foreign_proc("Java",
    string.count_codepoints(String::in, Count::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Count = String.codePointCount(0, String.length());
").

string.count_codepoints(String, Count) :-
    count_codepoints_loop(String, 0, 0, Count).

:- pred count_codepoints_loop(string::in, int::in, int::in, int::out) is det.

count_codepoints_loop(String, I, Count0, Count) :-
    ( string.unsafe_index_next(String, I, J, _) ->
        count_codepoints_loop(String, J, Count0 + 1, Count)
    ;
        Count = Count0
    ).

%---------------------%

:- pragma foreign_proc("C",
    string.count_utf8_code_units(Str::in) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = strlen(Str);
").
:- pragma foreign_proc("Erlang",
    string.count_utf8_code_units(Str::in) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = size(Str)
").

string.count_utf8_code_units(String) = Length :-
    string.foldl(count_utf8_code_units_2, String, 0, Length).

:- pred count_utf8_code_units_2(char::in, int::in, int::out) is det.

count_utf8_code_units_2(Char, !Length) :-
    char.to_int(Char, CharInt),
    ( CharInt =< 0x7f ->
        !:Length = !.Length + 1
    ; char.to_utf8(Char, UTF8) ->
        !:Length = !.Length + list.length(UTF8)
    ;
        error($pred, "char.to_utf8 failed")
    ).

%---------------------%

:- pragma foreign_proc("C",
    string.codepoint_offset(String::in, StartOffset::in, N::in, Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    size_t          len;
    unsigned char   b;

    SUCCESS_INDICATOR = MR_FALSE;
    len = strlen(String);
    for (Index = StartOffset; Index < len; Index++) {
        b = String[Index];
        if (MR_utf8_is_single_byte(b) || MR_utf8_is_lead_byte(b)) {
            if (N-- == 0) {
                SUCCESS_INDICATOR = MR_TRUE;
                break;
            }
        }
    }
").
:- pragma foreign_proc("C#",
    string.codepoint_offset(String::in, StartOffset::in, N::in, Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = false;
    for (Index = StartOffset; Index < String.Length; Index++) {
        if (!System.Char.IsLowSurrogate(String, Index)) {
            if (N-- == 0) {
                SUCCESS_INDICATOR = true;
                break;
            }
        }
    }
").
:- pragma foreign_proc("Java",
    string.codepoint_offset(String::in, StartOffset::in, N::in, Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    try {
        Index = String.offsetByCodePoints(StartOffset, N);
        SUCCESS_INDICATOR = (Index < String.length());
    } catch (IndexOutOfBoundsException e) {
        Index = -1;
        SUCCESS_INDICATOR = false;
    }
").

string.codepoint_offset(String, N, Index) :-
    % Note: we do not define what happens with unpaired surrogates.
    %
    string.codepoint_offset(String, 0, N, Index).

string.codepoint_offset(String, StartOffset, N, Index) :-
    StartOffset >= 0,
    Length = string.length(String),
    string.codepoint_offset_loop(String, StartOffset, Length, N, Index).

:- pred codepoint_offset_loop(string::in, int::in, int::in, int::in, int::out)
    is semidet.

codepoint_offset_loop(String, Offset, Length, N, Index) :-
    Offset < Length,
    ( N = 0 ->
        Index = Offset
    ;
        string.unsafe_index_next(String, Offset, NextOffset, _),
        string.codepoint_offset_loop(String, NextOffset, Length, N - 1, Index)
    ).

%---------------------------------------------------------------------------%
%
% Computing hashes of strings.
%
%
% NOTE: string.hash, hash2 and hash3 are also defined as MR_hash_string,
% MR_hash_string2 and MR_hash_string3 in runtime/mercury_string.h.
% The corresponding definitions must be kept identical.
%

string.hash(String, HashVal) :-
    HashVal = string.hash(String).

string.hash(String) = HashVal :-
    string.length(String, Length),
    string.hash_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred string.hash_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

string.hash_loop(String, Index, Length, !HashVal) :-
    ( Index < Length ->
        string.unsafe_index_code_unit(String, Index, C),
        !:HashVal = !.HashVal `xor` (!.HashVal `unchecked_left_shift` 5),
        !:HashVal = !.HashVal `xor` C,
        string.hash_loop(String, Index + 1, Length, !HashVal)
    ;
        true
    ).

string.hash2(String) = HashVal :-
    string.length(String, Length),
    string.hash2_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred string.hash2_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

string.hash2_loop(String, Index, Length, !HashVal) :-
    ( Index < Length ->
        string.unsafe_index_code_unit(String, Index, C),
        !:HashVal = !.HashVal * 37,
        !:HashVal= !.HashVal + C,
        string.hash2_loop(String, Index + 1, Length, !HashVal)
    ;
        true
    ).

string.hash3(String) = HashVal :-
    string.length(String, Length),
    string.hash3_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred string.hash3_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

string.hash3_loop(String, Index, Length, !HashVal) :-
    ( Index < Length ->
        string.unsafe_index_code_unit(String, Index, C),
        !:HashVal = !.HashVal * 49,
        !:HashVal= !.HashVal + C,
        string.hash3_loop(String, Index + 1, Length, !HashVal)
    ;
        true
    ).

%---------------------------------------------------------------------------%
%
% Tests on strings.
%
% For speed, most of these predicates have C versions as well as
% Mercury versions. XXX why not all?

:- pragma foreign_proc("C",
    is_all_alpha(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_duplicate, no_sharing],
"
    const char  *p;

    SUCCESS_INDICATOR = MR_TRUE;
    for (p = S; *p != '\\0'; p++) {
        switch (*p) {
            case 'a': case 'b': case 'c': case 'd': case 'e':
            case 'f': case 'g': case 'h': case 'i': case 'j':
            case 'k': case 'l': case 'm': case 'n': case 'o':
            case 'p': case 'q': case 'r': case 's': case 't':
            case 'u': case 'v': case 'w': case 'x': case 'y':
            case 'z':

            case 'A': case 'B': case 'C': case 'D': case 'E':
            case 'F': case 'G': case 'H': case 'I': case 'J':
            case 'K': case 'L': case 'M': case 'N': case 'O':
            case 'P': case 'Q': case 'R': case 'S': case 'T':
            case 'U': case 'V': case 'W': case 'X': case 'Y':
            case 'Z':
                continue;

            default:
                SUCCESS_INDICATOR = MR_FALSE;
                break;
        }
    }
").

is_all_alpha(S) :-
    all_match(char.is_alpha, S).

is_all_alnum(S) :-
    all_match(char.is_alnum, S).

:- pragma foreign_proc("C",
    is_all_alpha_or_underscore(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_duplicate, no_sharing],
"
    const char  *p;

    SUCCESS_INDICATOR = MR_TRUE;
    for (p = S; *p != '\\0'; p++) {
        switch (*p) {
            case 'a': case 'b': case 'c': case 'd': case 'e':
            case 'f': case 'g': case 'h': case 'i': case 'j':
            case 'k': case 'l': case 'm': case 'n': case 'o':
            case 'p': case 'q': case 'r': case 's': case 't':
            case 'u': case 'v': case 'w': case 'x': case 'y':
            case 'z':

            case 'A': case 'B': case 'C': case 'D': case 'E':
            case 'F': case 'G': case 'H': case 'I': case 'J':
            case 'K': case 'L': case 'M': case 'N': case 'O':
            case 'P': case 'Q': case 'R': case 'S': case 'T':
            case 'U': case 'V': case 'W': case 'X': case 'Y':
            case 'Z':

            case '_':
                continue;

            default:
                SUCCESS_INDICATOR = MR_FALSE;
                break;
        }
    }
").

is_all_alpha_or_underscore(S) :-
    all_match(char.is_alpha_or_underscore, S).

:- pragma foreign_proc("C",
    is_all_alnum_or_underscore(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_duplicate, no_sharing],
"
    const char  *p;

    SUCCESS_INDICATOR = MR_TRUE;
    for (p = S; *p != '\\0'; p++) {
        switch (*p) {
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':

            case 'a': case 'b': case 'c': case 'd': case 'e':
            case 'f': case 'g': case 'h': case 'i': case 'j':
            case 'k': case 'l': case 'm': case 'n': case 'o':
            case 'p': case 'q': case 'r': case 's': case 't':
            case 'u': case 'v': case 'w': case 'x': case 'y':
            case 'z':

            case 'A': case 'B': case 'C': case 'D': case 'E':
            case 'F': case 'G': case 'H': case 'I': case 'J':
            case 'K': case 'L': case 'M': case 'N': case 'O':
            case 'P': case 'Q': case 'R': case 'S': case 'T':
            case 'U': case 'V': case 'W': case 'X': case 'Y':
            case 'Z':

            case '_':
                continue;

            default:
                SUCCESS_INDICATOR = MR_FALSE;
                break;
        }
    }
").

is_all_alnum_or_underscore(S) :-
    all_match(char.is_alnum_or_underscore, S).

    % The C version is faster than the Mercury version.
:- pragma foreign_proc("C",
    is_all_digits(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_duplicate, no_sharing],
"
    const char  *p;

    SUCCESS_INDICATOR = MR_TRUE;
    for (p = S; *p != '\\0'; p++) {
        switch (*p) {
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                continue;

            default:
                SUCCESS_INDICATOR = MR_FALSE;
                break;
        }
    }
").

is_all_digits(S) :-
    all_match(char.is_digit, S).

%---------------------%

all_match(P, String) :-
    all_match_loop(P, String, 0).

:- pred all_match_loop(pred(char)::in(pred(in) is semidet), string::in,
    int::in) is semidet.

all_match_loop(P, String, Cur) :-
    ( string.unsafe_index_next(String, Cur, Next, Char) ->
        P(Char),
        all_match_loop(P, String, Next)
    ;
        true
    ).

%---------------------%

    % strchr always returns true when searching for '\0',
    % but the '\0' is an implementation detail which really
    % shouldn't be considered to be part of the string itself.
:- pragma foreign_proc("C",
    string.contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char    buf[5];
    size_t  len;
    if (MR_is_ascii(Ch)) {
        /* Fast path. */
        SUCCESS_INDICATOR = (strchr(Str, Ch) != NULL) && Ch != '\\0';
    } else {
        len = MR_utf8_encode(buf, Ch);
        buf[len] = '\\0';
        SUCCESS_INDICATOR = (strstr(Str, buf) != NULL);
    }
").
:- pragma foreign_proc("C#",
    string.contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Ch <= 0xffff) {
        SUCCESS_INDICATOR = (Str.IndexOf((char) Ch) != -1);
    } else {
        string s = System.Char.ConvertFromUtf32(Ch);
        SUCCESS_INDICATOR = Str.Contains(s);
    }
").
:- pragma foreign_proc("Java",
    string.contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // indexOf(int) handles supplementary characters correctly.
    SUCCESS_INDICATOR = (Str.indexOf((int) Ch) != -1);
").

string.contains_char(String, Char) :-
    string.contains_char(String, Char, 0).

:- pred string.contains_char(string::in, char::in, int::in) is semidet.

string.contains_char(Str, Char, I) :-
    ( string.unsafe_index_next(Str, I, J, IndexChar) ->
        ( IndexChar = Char ->
            true
        ;
            string.contains_char(Str, Char, J)
        )
    ;
        fail
    ).

%---------------------%

prefix_length(P, S) = Index :-
    prefix_length_loop(P, S, 0, Index).

:- pred prefix_length_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

prefix_length_loop(P, S, I, Index) :-
    (
        string.unsafe_index_next(S, I, J, Char),
        P(Char)
    ->
        prefix_length_loop(P, S, J, Index)
    ;
        Index = I
    ).

suffix_length(P, S) = End - Index :-
    End = string.length(S),
    suffix_length_loop(P, S, End, Index).

:- pred suffix_length_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

suffix_length_loop(P, S, I, Index) :-
    (
        string.unsafe_prev_index(S, I, J, Char),
        P(Char)
    ->
        suffix_length_loop(P, S, J, Index)
    ;
        Index = I
    ).

%---------------------%

string.sub_string_search(WholeString, Pattern, Index) :-
    sub_string_search_start(WholeString, Pattern, 0, Index).

:- pragma foreign_proc("C",
    sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    char *match;
    if ((MR_Unsigned) BeginAt > strlen(WholeString)) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        match = strstr(WholeString + BeginAt, Pattern);
        if (match) {
            Index = match - WholeString;
            SUCCESS_INDICATOR = MR_TRUE;
        } else {
            SUCCESS_INDICATOR = MR_FALSE;
        }
    }
}").
:- pragma foreign_proc("C#",
    sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Index = WholeString.IndexOf(Pattern, BeginAt);
    SUCCESS_INDICATOR = (Index >= 0);
}").
:- pragma foreign_proc("Java",
    sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Index = WholeString.indexOf(Pattern, BeginAt);
    SUCCESS_INDICATOR = (Index >= 0);
").
:- pragma foreign_proc("Erlang",
    sub_string_search_start(String::in, SubString::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Index = mercury__string:sub_string_search_start_2(String, SubString,
        BeginAt, size(String), size(SubString)),
    SUCCESS_INDICATOR = (Index =/= -1)
").

:- pragma foreign_decl("Erlang", local, "
-export([sub_string_search_start_2/5]).
").

:- pragma foreign_code("Erlang", "
sub_string_search_start_2(String, SubString, I, Length, SubLength) ->
    case I + SubLength =< Length of
        true ->
            case String of
                <<_:I/binary, SubString:SubLength/binary, _/binary>> ->
                    I;
                _ ->
                    sub_string_search_start_2(String, SubString, I + 1,
                        Length, SubLength)
            end;
        false ->
            -1
    end.
").

sub_string_search_start(String, SubString, BeginAt, Index) :-
    sub_string_search_start_loop(String, SubString, BeginAt,
        length(String), length(SubString), Index).

    % Brute force string searching. For short Strings this is good;
    % for longer strings Boyer-Moore is much better.
    %
:- pred sub_string_search_start_loop(string::in, string::in, int::in, int::in,
    int::in, int::out) is semidet.

sub_string_search_start_loop(String, SubString, I, Len, SubLen, Index) :-
    I < Len,
    (
        % XXX This is inefficient -- there is no (in, in, in) = in is semidet
        % mode of between, so this ends up calling the (in, in, in) = out
        % mode and then doing the unification. This will create a lot of
        % unnecessary garbage.
        % XXX This will abort if either index is not at a code point boundary.
        between(String, I, I + SubLen) = SubString
    ->
        Index = I
    ;
        sub_string_search_start_loop(String, SubString, I + 1, Len, SubLen,
            Index)
    ).

%---------------------------------------------------------------------------%
%
% Appending strings.
%

string.append(S1, S2) = S3 :-
    string.append(S1, S2, S3).

S1 ++ S2 = string.append(S1, S2).

:- pragma promise_equivalent_clauses(string.append/3).

string.append(S1::in, S2::in, S3::in) :-
    string.append_iii(S1, S2, S3).
string.append(S1::in, S2::uo, S3::in) :-
    string.append_ioi(S1, S2, S3).
string.append(S1::in, S2::in, S3::uo) :-
    string.append_iio(S1, S2, S3).
string.append(S1::out, S2::out, S3::in) :-
    string.append_ooi(S1, S2, S3).

:- pred string.append_iii(string::in, string::in, string::in) is semidet.

:- pragma foreign_proc("C",
    string.append_iii(S1::in, S2::in, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    size_t len_1 = strlen(S1);
    SUCCESS_INDICATOR = (
        strncmp(S1, S3, len_1) == 0 &&
        strcmp(S2, S3 + len_1) == 0
    );
}").
:- pragma foreign_proc("C#",
    string.append_iii(S1::in, S2::in, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    SUCCESS_INDICATOR = S3.Equals(System.String.Concat(S1, S2));
}").
:- pragma foreign_proc("Java",
    string.append_iii(S1::in, S2::in, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = S3.equals(S1.concat(S2));
").
:- pragma foreign_proc("Erlang",
    string.append_iii(S1::in, S2::in, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S1_length = size(S1),
    S2_length = size(S2),
    case S1_length + S2_length =:= size(S3) of
        true ->
            <<Left:S1_length/binary, Right/binary>> = S3,
            SUCCESS_INDICATOR = (Left =:= S1 andalso Right =:= S2);
        false ->
            SUCCESS_INDICATOR = false
    end
").

string.append_iii(X, Y, Z) :-
    string.mercury_append(X, Y, Z).

:- pred string.append_ioi(string::in, string::uo, string::in) is semidet.

:- pragma foreign_proc("C",
    string.append_ioi(S1::in, S2::uo, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    size_t len_1, len_2, len_3;

    len_1 = strlen(S1);
    if (strncmp(S1, S3, len_1) != 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        len_3 = strlen(S3);
        len_2 = len_3 - len_1;
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(S2, len_2, MR_ALLOC_ID);
        strcpy(S2, S3 + len_1);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").
:- pragma foreign_proc("C#",
    string.append_ioi(S1::in, S2::uo, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (S3.StartsWith(S1)) {
        // .Substring() better?
        S2 = S3.Remove(0, S1.Length);
        SUCCESS_INDICATOR = true;
    } else {
        S2 = null;
        SUCCESS_INDICATOR = false;
    }
}").
:- pragma foreign_proc("Java",
    string.append_ioi(S1::in, S2::uo, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (S3.startsWith(S1)) {
        S2 = S3.substring(S1.length());
        SUCCESS_INDICATOR = true;
    } else {
        S2 = null;
        SUCCESS_INDICATOR = false;
    }
").

string.append_ioi(X, Y, Z) :-
    string.mercury_append(X, Y, Z).

:- pred string.append_iio(string::in, string::in, string::uo) is det.

:- pragma foreign_proc("C",
    string.append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    size_t len_1, len_2;
    len_1 = strlen(S1);
    len_2 = strlen(S2);
    MR_allocate_aligned_string_msg(S3, len_1 + len_2, MR_ALLOC_ID);
    strcpy(S3, S1);
    strcpy(S3 + len_1, S2);
}").
:- pragma foreign_proc("C#",
    string.append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    S3 = System.String.Concat(S1, S2);
}").
:- pragma foreign_proc("Java",
    string.append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S3 = S1.concat(S2);
").
:- pragma foreign_proc("Erlang",
    string.append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S3 = list_to_binary([S1, S2])
").

string.append_iio(X, Y, Z) :-
    string.mercury_append(X, Y, Z).

:- pred string.append_ooi(string::out, string::out, string::in) is multi.

string.append_ooi(S1, S2, S3) :-
    S3Len = string.length(S3),
    string.append_ooi_2(0, S3Len, S1, S2, S3).

:- pred string.append_ooi_2(int::in, int::in, string::out, string::out,
    string::in) is multi.

string.append_ooi_2(NextS1Len, S3Len, S1, S2, S3) :-
    ( NextS1Len = S3Len ->
        string.append_ooi_3(NextS1Len, S3Len, S1, S2, S3)
    ;
        (
            string.append_ooi_3(NextS1Len, S3Len, S1, S2, S3)
        ;
            string.unsafe_index_next(S3, NextS1Len, AdvS1Len, _),
            string.append_ooi_2(AdvS1Len, S3Len, S1, S2, S3)
        )
    ).

:- pred string.append_ooi_3(int::in, int::in, string::out,
    string::out, string::in) is det.

:- pragma foreign_proc("C",
    string.append_ooi_3(S1Len::in, S3Len::in, S1::out, S2::out, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_allocate_aligned_string_msg(S1, S1Len, MR_ALLOC_ID);
    MR_memcpy(S1, S3, S1Len);
    S1[S1Len] = '\\0';
    MR_allocate_aligned_string_msg(S2, S3Len - S1Len, MR_ALLOC_ID);
    strcpy(S2, S3 + S1Len);
}").
:- pragma foreign_proc("C#",
    string.append_ooi_3(S1Len::in, _S3Len::in, S1::out, S2::out, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S1 = S3.Substring(0, S1Len);
    S2 = S3.Substring(S1Len);
").
:- pragma foreign_proc("Java",
    string.append_ooi_3(S1Len::in, _S3Len::in, S1::out, S2::out, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S1 = S3.substring(0, S1Len);
    S2 = S3.substring(S1Len);
").
:- pragma foreign_proc("Erlang",
    string.append_ooi_3(S1Len::in, _S3Len::in, S1::out, S2::out, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    << S1:S1Len/binary, S2/binary >> = S3
").

string.append_ooi_3(S1Len, _S3Len, S1, S2, S3) :-
    string.split(S3, S1Len, S1, S2).

:- pred string.mercury_append(string, string, string).
:- mode string.mercury_append(in, in, in) is semidet.  % implied
:- mode string.mercury_append(in, uo, in) is semidet.
:- mode string.mercury_append(in, in, uo) is det.
:- mode string.mercury_append(uo, uo, in) is multi.

string.mercury_append(X, Y, Z) :-
    string.to_char_list(X, XList),
    string.to_char_list(Y, YList),
    string.to_char_list(Z, ZList),
    list.append(XList, YList, ZList).

%---------------------%
%
% We implement string.append_list in foreign code as the Mercury version
% creates significant amounts of unnecessary garbage.
%

:- pragma foreign_proc("C",
    string.append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list = Strs;
    size_t  len;

    /* Determine the total length of all strings */
    len = 0;
    while (!MR_list_is_empty(list)) {
        len += strlen((MR_String) MR_list_head(list));
        list = MR_list_tail(list);
    }

    /* Allocate enough word aligned memory for the string */
    MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);

    /* Copy the strings into the new memory */
    len = 0;
    list = Strs;
    while (!MR_list_is_empty(list)) {
        strcpy((MR_String) Str + len, (MR_String) MR_list_head(list));
        len += strlen((MR_String) MR_list_head(list));
        list = MR_list_tail(list);
    }

    Str[len] = '\\0';
}").
:- pragma foreign_proc("C#",
    string.append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    while (!list.is_empty(Strs)) {
        sb.Append((string) list.det_head(Strs));
        Strs = list.det_tail(Strs);
    }
    Str = sb.ToString();
").
:- pragma foreign_proc("Java",
    string.append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    java.lang.StringBuilder sb = new java.lang.StringBuilder();

    Iterable<String> iterable = new list.ListIterator<String>(Strs);
    for (String s : iterable) {
        sb.append(s);
    }

    Str = sb.toString();
").
:- pragma foreign_proc("Erlang",
    string.append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = list_to_binary(Strs)
").

string.append_list(Strs::in) = (Str::uo) :-
    (
        Strs = [X | Xs],
        Str = X ++ append_list(Xs)
    ;
        Strs = [],
        Str = ""
    ).

string.append_list(Lists, string.append_list(Lists)).

%---------------------%
%
% We implement string.join_list in foreign code as the Mercury version
% creates significant amounts of unnecessary garbage.
%

:- pragma foreign_proc("C",
    string.join_list(Sep::in, Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list;
    size_t  len;
    size_t  sep_len;
    MR_bool add_sep;

    list = Strs;
    len = 0;
    sep_len = strlen(Sep);

    /* Determine the total length of all strings */
    len = 0;
    add_sep = MR_FALSE;
    while (!MR_list_is_empty(list)) {
        if (add_sep) {
            len += sep_len;
        }

        len += strlen((MR_String) MR_list_head(list));
        list = MR_list_tail(list);
        add_sep = MR_TRUE;
    }

    MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);

    /* Copy the strings into the new memory */
    len = 0;
    list = Strs;
    add_sep = MR_FALSE;
    while (!MR_list_is_empty(list)) {
        if (add_sep) {
            strcpy((MR_String) Str + len, Sep);
            len += sep_len;
        }

        strcpy((MR_String) Str + len, (MR_String) MR_list_head(list));
        len += strlen((MR_String) MR_list_head(list));
        list = MR_list_tail(list);
        add_sep = MR_TRUE;
    }

    Str[len] = '\\0';
}").
:- pragma foreign_proc("C#",
    string.join_list(Sep::in, Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    bool add_sep = false;

    while (!list.is_empty(Strs)) {
        if (add_sep) {
            sb.Append(Sep);
        }
        sb.Append((string) list.det_head(Strs));
        add_sep = true;
        Strs = list.det_tail(Strs);
    }

    Str = sb.ToString();
").
:- pragma foreign_proc("Java",
    string.join_list(Sep::in, Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    java.lang.StringBuilder sb = new java.lang.StringBuilder();
    boolean add_sep = false;

    Iterable<String> iterable = new list.ListIterator<String>(Strs);
    for (String s : iterable) {
        if (add_sep) {
            sb.append(Sep);
        }
        sb.append(s);
        add_sep = true;
    }

    Str = sb.toString();
").

string.join_list(_, []) = "".
string.join_list(Sep, [H | T]) = H ++ string.join_list_loop(Sep, T).

:- func join_list_loop(string::in, list(string)::in) = (string::uo) is det.

join_list_loop(_, []) = "".
join_list_loop(Sep, [H | T]) = Sep ++ H ++ join_list_loop(Sep, T).

%---------------------------------------------------------------------------%
%
% Splitting up strings.
%

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    MR_Integer pos = 0;
    int c = MR_utf8_get_next(Str, &pos);
    SUCCESS_INDICATOR = (
        c == First &&
        First != '\\0' &&
        strcmp(Str + pos, Rest) == 0
    );
").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int len = Str.Length;
    if (First <= 0xffff) {
        SUCCESS_INDICATOR = (
            len > 0 &&
            Str[0] == First &&
            System.String.CompareOrdinal(Str, 1, Rest, 0, len) == 0
        );
    } else {
        string firstchars = System.Char.ConvertFromUtf32(First);
        SUCCESS_INDICATOR = (
            len > 1 &&
            Str[0] == firstchars[0] &&
            Str[1] == firstchars[1] &&
            System.String.CompareOrdinal(Str, 2, Rest, 0, len) == 0
        );
    }
").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int toffset = java.lang.Character.charCount(First);
    SUCCESS_INDICATOR = (
        Str.length() > 0 &&
        Str.codePointAt(0) == First &&
        Str.regionMatches(toffset, Rest, 0, Rest.length())
    );
").
:- pragma foreign_proc("Erlang",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case Str of
        <<First/utf8, Rest/binary>> ->
            SUCCESS_INDICATOR = true;
        _ ->
            SUCCESS_INDICATOR = false
    end
").

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    MR_Integer pos = 0;
    First = MR_utf8_get_next(Str, &pos);
    SUCCESS_INDICATOR = (First > 0 && strcmp(Str + pos, Rest) == 0);
").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        int len = Str.Length;
        char c1 = Str[0];
        if (System.Char.IsHighSurrogate(c1)) {
            char c2 = Str[1];
            First = System.Char.ConvertToUtf32(c1, c2);
            SUCCESS_INDICATOR =
                (System.String.CompareOrdinal(Str, 2, Rest, 0, len) == 0);
        } else {
            First = c1;
            SUCCESS_INDICATOR =
                (System.String.CompareOrdinal(Str, 1, Rest, 0, len) == 0);
        }
    } catch (System.IndexOutOfRangeException) {
        SUCCESS_INDICATOR = false;
        First = (char) 0;
    } catch (System.ArgumentOutOfRangeException) {
        SUCCESS_INDICATOR = false;
        First = (char) 0;
    }
").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int toffset;
    if (Str.length() > 0) {
        First = Str.codePointAt(0);
        toffset = java.lang.Character.charCount(First);
        SUCCESS_INDICATOR =
            Str.regionMatches(toffset, Rest, 0, Rest.length());
    } else {
        SUCCESS_INDICATOR = false;
        // XXX to avoid uninitialized var warning
        First = 0;
    }
").
:- pragma foreign_proc("Erlang",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case Str of
        <<First/utf8, Rest/binary>> ->
            SUCCESS_INDICATOR = true;
        _ ->
            SUCCESS_INDICATOR = false,
            First = 0
    end
").

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_Integer pos = 0;
    int c = MR_utf8_get_next(Str, &pos);
    if (c != First || First == '\\0') {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Str += pos;
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(Rest, strlen(Str), MR_ALLOC_ID);
        strcpy(Rest, Str);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    int len = Str.Length;

    if (len > 0) {
        if (First <= 0xffff) {
            SUCCESS_INDICATOR = (First == Str[0]);
            Rest = Str.Substring(1);
        } else {
            string firststr = System.Char.ConvertFromUtf32(First);
            SUCCESS_INDICATOR =
                (System.String.CompareOrdinal(Str, 0, firststr, 0, 2) == 0);
            Rest = Str.Substring(2);
        }
    } else {
        SUCCESS_INDICATOR = false;
        Rest = null;
    }
}").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    int len = Str.length();

    if (len > 0) {
        SUCCESS_INDICATOR = (First == Str.codePointAt(0));
        Rest = Str.substring(java.lang.Character.charCount(First));
    } else {
        SUCCESS_INDICATOR = false;
        // XXX to avoid uninitialized var warning
        Rest = null;
    }
}").
:- pragma foreign_proc("Erlang",
    string.first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case Str of
        <<First/utf8, Rest/binary>> ->
            SUCCESS_INDICATOR = true;
        _ ->
            SUCCESS_INDICATOR = false,
            Rest = <<>>
    end
").

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_Integer pos = 0;
    First = MR_utf8_get_next(Str, &pos);
    if (First < 1) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Str += pos;
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(Rest, strlen(Str), MR_ALLOC_ID);
        strcpy(Rest, Str);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    try {
        char c1 = Str[0];
        if (System.Char.IsHighSurrogate(c1)) {
            char c2 = Str[1];
            First = System.Char.ConvertToUtf32(c1, c2);
            Rest = Str.Substring(2);
        } else {
            First = Str[0];
            Rest = Str.Substring(1);
        }
        SUCCESS_INDICATOR = true;
    } catch (System.IndexOutOfRangeException) {
        SUCCESS_INDICATOR = false;
        First = (char) 0;
        Rest = null;
    } catch (System.ArgumentOutOfRangeException) {
        SUCCESS_INDICATOR = false;
        First = (char) 0;
        Rest = null;
    }
}").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (Str.length() == 0) {
        SUCCESS_INDICATOR = false;
        First = (char) 0;
        Rest = null;
    } else {
        First = Str.codePointAt(0);
        Rest = Str.substring(java.lang.Character.charCount(First));
        SUCCESS_INDICATOR = true;
    }
}").
:- pragma foreign_proc("Erlang",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case Str of
        <<First/utf8, Rest/binary>> ->
            SUCCESS_INDICATOR = true;
        _ ->
            SUCCESS_INDICATOR = false,
            First = 0,
            Rest = <<>>
    end
").

:- pragma foreign_proc("C",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    size_t firstw = MR_utf8_width(First);
    size_t len = firstw + strlen(Rest);
    MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);
    MR_utf8_encode(Str, First);
    strcpy(Str + firstw, Rest);
}").
:- pragma foreign_proc("C#",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    string FirstStr;
    if (First <= 0xffff) {
        FirstStr = new System.String((char) First, 1);
    } else {
        FirstStr = System.Char.ConvertFromUtf32(First);
    }
    Str = FirstStr + Rest;
}").
:- pragma foreign_proc("Java",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    String FirstStr = new String(Character.toChars(First));
    Str = FirstStr.concat(Rest);
}").
:- pragma foreign_proc("Erlang",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = unicode:characters_to_binary([First, Rest])
").

%---------------------%
%
% For some Str and Count inputs, we may return Str as either Left or Right.
% Since Str has mode `in', both Left or Right must have mode 'out', not `uo'.
%

:- pragma foreign_proc("C",
    string.split(Str::in, Count::in, Left::out, Right::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"{
    MR_Integer  len;

    if (Count <= 0) {
        MR_make_aligned_string(Left, """");
        Right = Str;
    } else {
        len = strlen(Str);

        if (Count > len) {
            Count = len;
        }

        MR_allocate_aligned_string_msg(Left, Count, MR_ALLOC_ID);
        MR_memcpy(Left, Str, Count);
        Left[Count] = '\\0';
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(Right, len - Count, MR_ALLOC_ID);
        strcpy(Right, Str + Count);
    }
}").
:- pragma foreign_proc("C#",
    string.split(Str::in, Count::in, Left::out, Right::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    int len;

    if (Count <= 0) {
        Left = """";
        Right = Str;
    } else {
        len = Str.Length;
        if (Count > len) {
            Count = len;
        }
        Left = Str.Substring(0, Count);
        Right = Str.Substring(Count);
    }
}").
:- pragma foreign_proc("Java",
    string.split(Str::in, Count::in, Left::out, Right::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Count <= 0) {
        Left = """";
        Right = Str;
    } else {
        int len = Str.length();
        if (Count > len) {
            Count = len;
        }
        Left = Str.substring(0, Count);
        Right = Str.substring(Count);
    }
").
:- pragma foreign_proc("Erlang",
    string.split(Str::in, Count::in, Left::out, Right::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if
        Count =< 0 ->
            Left = <<>>,
            Right = Str;
        Count > size(Str) ->
            Left = Str,
            Right = <<>>;
        true ->
            << Left:Count/binary, Right/binary >> = Str
    end
").

string.split(Str, Count, Left, Right) :-
    ( Count =< 0 ->
        Left = "",
        Right = Str
    ;
        string.to_code_unit_list(Str, List),
        Len = string.length(Str),
        ( Count > Len ->
            Num = Len
        ;
            Num = Count
        ),
        (
            list.split_list(Num, List, LeftList, RightList),
            string.from_code_unit_list(LeftList, LeftPrime),
            string.from_code_unit_list(RightList, RightPrime)
        ->
            Left = LeftPrime,
            Right = RightPrime
        ;
            unexpected($pred, "split_list failed")
        )
    ).

string.split_by_codepoint(Str, Count, Left, Right) :-
    ( string.codepoint_offset(Str, Count, Offset) ->
        string.split(Str, Offset, Left, Right)
    ; Count =< 0 ->
        Left = "",
        Right = Str
    ;
        Left = Str,
        Right = ""
    ).

%---------------------%

string.left(S1, N) = S2 :-
    string.left(S1, N, S2).

string.left(String, Count, LeftString) :-
    string.split(String, Count, LeftString, _RightString).

string.left_by_codepoint(String, Count) = LeftString :-
    string.left_by_codepoint(String, Count, LeftString).

string.left_by_codepoint(String, Count, LeftString) :-
    string.split_by_codepoint(String, Count, LeftString, _RightString).

string.right(S1, N) = S2 :-
    string.right(S1, N, S2).

string.right(String, RightCount, RightString) :-
    string.length(String, Length),
    LeftCount = Length - RightCount,
    string.split(String, LeftCount, _LeftString, RightString).

string.right_by_codepoint(String, RightCount) = RightString :-
    string.right_by_codepoint(String, RightCount, RightString).

string.right_by_codepoint(String, RightCount, RightString) :-
    string.count_codepoints(String, TotalCount),
    LeftCount = TotalCount - RightCount,
    string.split_by_codepoint(String, LeftCount, _LeftString, RightString).

%---------------------%

string.between(Str, Start, End) = SubString :-
    string.between(Str, Start, End, SubString).

:- pragma foreign_proc("C",
    string.between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Integer  len;
    MR_Integer  Count;

    if (Start < 0) Start = 0;
    if (End <= Start) {
        MR_make_aligned_string(SubString, """");
    } else {
        len = strlen(Str);
        if (Start > len) {
            Start = len;
        }
        if (End > len) {
            End = len;
        }
        Count = End - Start;
        MR_allocate_aligned_string_msg(SubString, Count, MR_ALLOC_ID);
        MR_memcpy(SubString, Str + Start, Count);
        SubString[Count] = '\\0';
    }
}").
:- pragma foreign_proc("C#",
    string.between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    if (Start < 0) Start = 0;
    if (End <= Start) {
        SubString = """";
    } else {
        int len = Str.Length;
        if (Start > len) {
            Start = len;
        }
        if (End > len) {
            End = len;
        }
        SubString = Str.Substring(Start, End - Start);
    }
").
:- pragma foreign_proc("Java",
    string.between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    if (Start < 0) Start = 0;
    if (End <= Start) {
        SubString = """";
    } else {
        int len = Str.length();
        if (Start > len) {
            Start = len;
        }
        if (End > len) {
            End = len;
        }
        SubString = Str.substring(Start, End);
    }
").
:- pragma foreign_proc("Erlang",
    string.between(Str::in, Start0::in, End0::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Start = max(Start0, 0),
    End = min(End0, size(Str)),
    Count = End - Start,
    if Count =< 0 ->
        SubString = <<>>
    ; true ->
        <<_:Start/binary, SubString:Count/binary, _/binary>> = Str
    end
").

between(Str, Start, End, SubStr) :-
    ( Start >= End ->
        SubStr = ""
    ;
        Len = string.length(Str),
        max(0, Start, ClampStart),
        min(Len, End, ClampEnd),
        CharList = between_loop(ClampStart, ClampEnd, Str),
        SubStr = string.from_char_list(CharList)
    ).

:- func between_loop(int, int, string) = list(char).

between_loop(I, End, Str) = Chars :-
    (
        I < End,
        string.unsafe_index_next(Str, I, J, C),
        J =< End
    ->
        Cs = between_loop(J, End, Str),
        Chars = [C | Cs]
    ;
        Chars = []
    ).

%---------------------%

string.substring(Str, Start, Count) = SubString :-
    string.substring(Str, Start, Count, SubString).

string.substring(Str, Start, Count, SubString) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.between(Str, ClampStart, ClampEnd, SubString).

:- pred convert_endpoints(int::in, int::in, int::out, int::out) is det.

convert_endpoints(Start, Count, ClampStart, ClampEnd) :-
    ClampStart = int.max(0, Start),
    ( Count =< 0 ->
        ClampEnd = ClampStart
    ;
        % Check for overflow.
        ( ClampStart > max_int - Count ->
            ClampEnd = max_int
        ;
            ClampEnd = ClampStart + Count
        )
    ).

%---------------------%

string.between_codepoints(Str, Start, End) = SubString :-
    string.between_codepoints(Str, Start, End, SubString).

string.between_codepoints(Str, Start, End, SubString) :-
    ( string.codepoint_offset(Str, Start, StartOffset0) ->
        StartOffset = StartOffset0
    ;
        StartOffset = 0
    ),
    ( string.codepoint_offset(Str, End, EndOffset0) ->
        EndOffset = EndOffset0
    ;
        EndOffset = string.length(Str)
    ),
    string.between(Str, StartOffset, EndOffset, SubString).

%---------------------%

string.unsafe_between(Str, Start, End) = SubString :-
    string.unsafe_between(Str, Start, End, SubString).

:- pragma foreign_proc("C",
    string.unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_Integer Count;

    Count = End - Start;
    MR_allocate_aligned_string_msg(SubString, Count, MR_ALLOC_ID);
    MR_memcpy(SubString, Str + Start, Count);
    SubString[Count] = '\\0';
}").
:- pragma foreign_proc("C#",
    string.unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    SubString = Str.Substring(Start, End - Start);
}").
:- pragma foreign_proc("Java",
    string.unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SubString = Str.substring(Start, End);
").
:- pragma foreign_proc("Erlang",
    string.unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Count = End - Start,
    << _:Start/binary, SubString:Count/binary, _/binary >> = Str
").

string.unsafe_substring(Str, Start, Count) = SubString :-
    string.unsafe_between(Str, Start, Start + Count) = SubString.

string.unsafe_substring(Str, Start, Count, SubString) :-
    string.unsafe_between(Str, Start, Start + Count, SubString).

%---------------------%

string.words_separator(SepP, String) = Words :-
    next_boundary(SepP, String, 0, WordStart),
    words_loop(SepP, String, WordStart, Words).

string.words(String) = string.words_separator(char.is_whitespace, String).

:- pred words_loop(pred(char)::in(pred(in) is semidet), string::in, int::in,
    list(string)::out) is det.

words_loop(SepP, String, WordStart, Words) :-
    next_boundary(isnt(SepP), String, WordStart, WordEnd),
    ( WordEnd = WordStart ->
        Words = []
    ;
        string.unsafe_between(String, WordStart, WordEnd, Word),
        next_boundary(SepP, String, WordEnd, NextWordStart),
        ( WordEnd = NextWordStart ->
            Words = [Word]
        ;
            words_loop(SepP, String, NextWordStart, Words0),
            Words = [Word | Words0]
        )
    ).

    % Return the smallest I >= I0 such that `not P(String[I])'.
    %
:- pred next_boundary(pred(char)::in(pred(in) is semidet), string::in,
    int::in, int::out) is det.

next_boundary(P, String, Cur, NextWordStart) :-
    (
        string.unsafe_index_next(String, Cur, Next, Char),
        P(Char)
    ->
        next_boundary(P, String, Next, NextWordStart)
    ;
        NextWordStart = Cur
    ).

%---------------------%

string.split_at_separator(DelimP, String) = Substrings :-
    Len = string.length(String),
    split_at_separator_loop(DelimP, String, Len, Len, [], Substrings).

:- pred split_at_separator_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::in, list(string)::in, list(string)::out) is det.

split_at_separator_loop(DelimP, Str, I, SegEnd, Acc0, Acc) :-
    % Walk Str backwards extending the accumulated list of chunks as code
    % points matching DelimP are found.
    %
    % Invariant: 0 =< I =< length(Str)
    % SegEnd is one past the last index of the current segment.
    %
    ( string.unsafe_prev_index(Str, I, J, C) ->
        ( DelimP(C) ->
            % Chop here.
            SegStart = I,
            Seg = string.unsafe_between(Str, SegStart, SegEnd),
            split_at_separator_loop(DelimP, Str, J, J, [Seg | Acc0], Acc)
        ;
            % Extend current segment.
            split_at_separator_loop(DelimP, Str, J, SegEnd, Acc0, Acc)
        )
    ;
        % We have reached the beginning of the string.
        Seg = string.unsafe_between(Str, 0, SegEnd),
        Acc = [Seg | Acc0]
    ).

%---------------------%

string.split_at_char(C, String) =
    string.split_at_separator(unify(C), String).

split_at_string(Needle, Total) =
    split_at_string_loop(0, length(Needle), Needle, Total).

:- func split_at_string_loop(int, int, string, string) = list(string).

split_at_string_loop(StartAt, NeedleLen, Needle, Total) = Out :-
    ( sub_string_search_start(Total, Needle, StartAt, NeedlePos) ->
        BeforeNeedle = between(Total, StartAt, NeedlePos),
        Tail = split_at_string_loop(NeedlePos+NeedleLen, NeedleLen,
            Needle, Total),
        Out = [BeforeNeedle | Tail]
    ;
        string.split(Total, StartAt, _Skip, Last),
        Out = [Last]
    ).

%---------------------------------------------------------------------------%
%
% Dealing with prefixes and suffixes.
%

:- pragma promise_equivalent_clauses(string.prefix/2).

string.prefix(String::in, Prefix::in) :-
    Len    = length(String),
    PreLen = length(Prefix),
    PreLen =< Len,
    prefix_2_iii(String, Prefix, PreLen - 1).
string.prefix(String::in, Prefix::out) :-
    prefix_2_ioi(String, Prefix, 0).

:- pred prefix_2_iii(string::in, string::in, int::in) is semidet.

prefix_2_iii(String, Prefix, I) :-
    ( 0 =< I ->
        string.unsafe_index_code_unit(String, I, C),
        string.unsafe_index_code_unit(Prefix, I, C),
        prefix_2_iii(String, Prefix, I - 1)
    ;
        true
    ).

:- pred prefix_2_ioi(string::in, string::out, int::in) is multi.

prefix_2_ioi(String, Prefix, Cur) :-
    (
        Prefix = unsafe_between(String, 0, Cur)
    ;
        string.unsafe_index_next(String, Cur, Next, _),
        prefix_2_ioi(String, Prefix, Next)
    ).

:- pragma promise_equivalent_clauses(string.suffix/2).

string.suffix(String::in, Suffix::in) :-
    Len    = length(String),
    PreLen = length(Suffix),
    PreLen =< Len,
    suffix_2_iiii(String, Suffix, 0, Len - PreLen, PreLen).
string.suffix(String::in, Suffix::out) :-
    Len = length(String),
    suffix_2_ioii(String, Suffix, Len, Len).

:- pred suffix_2_iiii(string::in, string::in, int::in, int::in, int::in)
    is semidet.

suffix_2_iiii(String, Suffix, I, Offset, Len) :-
    ( I < Len ->
        string.unsafe_index_code_unit(String, I + Offset, C),
        string.unsafe_index_code_unit(Suffix, I, C),
        suffix_2_iiii(String, Suffix, I + 1, Offset, Len)
    ;
        true
    ).

:- pred suffix_2_ioii(string::in, string::out, int::in, int::in) is multi.

suffix_2_ioii(String, Suffix, Cur, Len) :-
    (
        string.unsafe_between(String, Cur, Len, Suffix)
    ;
        string.unsafe_prev_index(String, Cur, Prev, _),
        suffix_2_ioii(String, Suffix, Prev, Len)
    ).

%---------------------%

string.remove_prefix(Prefix, String, Suffix) :-
    string.append(Prefix, Suffix, String).

string.remove_prefix_if_present(Prefix, String) = Out :-
    ( string.remove_prefix(Prefix, String, Suffix) ->
        Out = Suffix
    ;
        Out = String
    ).

string.remove_suffix(String, Suffix, StringWithoutSuffix) :-
    string.suffix(String, Suffix),
    string.left(String, length(String) - length(Suffix), StringWithoutSuffix).

string.det_remove_suffix(String, Suffix) = StringWithoutSuffix :-
    ( string.remove_suffix(String, Suffix, StringWithoutSuffixPrime) ->
        StringWithoutSuffix = StringWithoutSuffixPrime
    ;
        error("string.det_remove_suffix: string does not have given suffix")
    ).

string.remove_suffix_if_present(Suffix, String) = Out :-
    LeftCount = length(String) - length(Suffix),
    string.split(String, LeftCount, LeftString, RightString),
    ( RightString = Suffix ->
        Out = LeftString
    ;
        Out = String
    ).

%---------------------------------------------------------------------------%
%
% Transformations of strings.
%

string.capitalize_first(S1) = S2 :-
    string.capitalize_first(S1, S2).

string.capitalize_first(S0, S) :-
    ( string.first_char(S0, C, S1) ->
        char.to_upper(C, UpperC),
        string.first_char(S, UpperC, S1)
    ;
        S = S0
    ).

string.uncapitalize_first(S1) = S2 :-
    string.uncapitalize_first(S1, S2).

string.uncapitalize_first(S0, S) :-
    ( string.first_char(S0, C, S1) ->
        char.to_lower(C, LowerC),
        string.first_char(S, LowerC, S1)
    ;
        S = S0
    ).

string.to_upper(S1) = S2 :-
    string.to_upper(S1, S2).

string.to_upper(StrIn, StrOut) :-
    string.to_char_list(StrIn, List),
    string.char_list_to_upper(List, ListUpp),
    string.from_char_list(ListUpp, StrOut).

:- pred string.char_list_to_upper(list(char)::in, list(char)::out) is det.

string.char_list_to_upper([], []).
string.char_list_to_upper([X | Xs], [Y | Ys]) :-
    char.to_upper(X, Y),
    string.char_list_to_upper(Xs, Ys).

string.to_lower(S1) = S2 :-
    string.to_lower(S1, S2).

string.to_lower(StrIn, StrOut) :-
    string.to_char_list(StrIn, List),
    string.char_list_to_lower(List, ListLow),
    string.from_char_list(ListLow, StrOut).

:- pred string.char_list_to_lower(list(char)::in, list(char)::out) is det.

string.char_list_to_lower([], []).
string.char_list_to_lower([X | Xs], [Y | Ys]) :-
    char.to_lower(X, Y),
    string.char_list_to_lower(Xs, Ys).

%---------------------%

string.pad_left(S1, C, N) = S2 :-
    string.pad_left(S1, C, N, S2).

string.pad_left(String0, PadChar, Width, String) :-
    string.count_codepoints(String0, Length),
    ( Length < Width ->
        Count = Width - Length,
        string.duplicate_char(PadChar, Count, PadString),
        string.append(PadString, String0, String)
    ;
        String = String0
    ).

string.pad_right(S1, C, N) = S2 :-
    string.pad_right(S1, C, N, S2).

string.pad_right(String0, PadChar, Width, String) :-
    string.count_codepoints(String0, Length),
    ( Length < Width ->
        Count = Width - Length,
        string.duplicate_char(PadChar, Count, PadString),
        string.append(String0, PadString, String)
    ;
        String = String0
    ).

chomp(S) = Chomp :-
    ( prev_index(S, length(S), Offset, '\n') ->
        Chomp = left(S, Offset)
    ;
        Chomp = S
    ).

strip(S0) = S :-
    L = prefix_length(is_whitespace, S0),
    R = suffix_length(is_whitespace, S0),
    S = between(S0, L, length(S0) - R).

rstrip(S) = rstrip_pred(is_whitespace, S).

lstrip(S) = lstrip_pred(is_whitespace, S).

rstrip_pred(P, S) = left(S, length(S) - suffix_length(P, S)).

lstrip_pred(P, S) = right(S, length(S) - prefix_length(P, S)).

%---------------------%

string.replace(Str, Pat, Subst, Result) :-
    sub_string_search(Str, Pat, Index),

    Initial = string.unsafe_between(Str, 0, Index),

    BeginAt = Index + string.length(Pat),
    EndAt = string.length(Str),
    Final = string.unsafe_between(Str, BeginAt, EndAt),

    Result = string.append_list([Initial, Subst, Final]).

string.replace_all(S1, S2, S3) = S4 :-
    string.replace_all(S1, S2, S3, S4).

string.replace_all(Str, Pat, Subst, Result) :-
    ( Pat = "" ->
        F = (func(C, L) = [char_to_string(C) ++ Subst | L]),
        Foldl = string.foldl(F, Str, []),
        Result = append_list([Subst | list.reverse(Foldl)])
    ;
        PatLength = string.length(Pat),
        replace_all_loop(Str, Pat, Subst, PatLength, 0, [], ReversedChunks),
        Chunks = list.reverse(ReversedChunks),
        Result = string.append_list(Chunks)
    ).

:- pred string.replace_all_loop(string::in, string::in, string::in,
    int::in, int::in, list(string)::in, list(string)::out) is det.

string.replace_all_loop(Str, Pat, Subst, PatLength, BeginAt,
        RevChunks0, RevChunks) :-
    ( sub_string_search_start(Str, Pat, BeginAt, Index) ->
        Initial = string.unsafe_between(Str, BeginAt, Index),
        Start = Index + PatLength,
        string.replace_all_loop(Str, Pat, Subst, PatLength, Start,
            [Subst, Initial | RevChunks0], RevChunks)
    ;
        EndString = string.unsafe_between(Str, BeginAt, length(Str)),
        RevChunks = [EndString | RevChunks0]
    ).

%---------------------%

word_wrap(Str, N) = word_wrap_separator(Str, N, "").

word_wrap_separator(Str, N, WordSep0) = Wrapped :-
    Words = string.words_separator(char.is_whitespace, Str),
    SepLen0 = string.count_codepoints(WordSep0),
    ( SepLen0 < N ->
        WordSep = WordSep0,
        SepLen = SepLen0
    ;
        WordSep = "",
        SepLen = 0
    ),
    CurCol = 1,
    MaxCol = N,
    RevWordsSpacesNls0 = [],
    word_wrap_loop(Words, WordSep, SepLen, CurCol, MaxCol,
        RevWordsSpacesNls0, RevWordsSpacesNls),
    list.reverse(RevWordsSpacesNls, WordsSpacesNls),
    Wrapped = string.append_list(WordsSpacesNls).

    % word_wrap_loop(Words, WordSep, SepLen, CurCol, MaxCol,
    %   !RevWordsSpacesNls):
    %
    % This predicate loops over a list of words to wrap and returns
    % a list of strings, with each containing a word, a spaces or a newline.
    % When this list of strings (which we build as an accumulator)
    % is reversed and appended together, the result should be
    % the linewrapped version of the original word stream.
    %
    % Words is the list of words to process. WordSep is the string to use
    % as a separator of a word has to split between two lines, because it is
    % too long to fit on one line. SepLin is the length of WordSep.
    %
    % CurCol is the column where the next character should be written
    % if there is space for a whole word, and MaxCol is the number of
    % columns in a line.
    %
:- pred word_wrap_loop(list(string)::in, string::in, int::in,
    int::in, int::in, list(string)::in, list(string)::out) is det.

word_wrap_loop([], _, _, _, _, !RevWordsSpacesNls).
word_wrap_loop([Word | Words], WordSep, SepLen, CurCol, MaxCol,
        !RevWordsSpacesNls) :-
    WordLen = string.count_codepoints(Word),
    (
        % We are on the first column and the length of the word
        % is less than the line length.
        CurCol = 1,
        WordLen < MaxCol
    ->
        NewWords = Words,
        NewCol = CurCol + WordLen,
        !:RevWordsSpacesNls = [Word | !.RevWordsSpacesNls]
    ;
        % The word takes up the whole line.
        CurCol = 1,
        WordLen = MaxCol
    ->
        NewWords = Words,
        NewCol = 1,
        % We only add a newline if there are more words to follow.
        (
            NewWords = [],
            !:RevWordsSpacesNls = [Word | !.RevWordsSpacesNls]
        ;
            NewWords = [_ | _],
            !:RevWordsSpacesNls = ["\n", Word | !.RevWordsSpacesNls]
        )
    ;
        % If we add a space and the current word to the line,
        % we will still be within the line length limit.
        CurCol + WordLen < MaxCol
    ->
        NewWords = Words,
        NewCol = CurCol + WordLen + 1,
        !:RevWordsSpacesNls = [Word, " " | !.RevWordsSpacesNls]
    ;
        % Adding the word and a space takes us to the end of the line exactly.
        CurCol + WordLen = MaxCol
    ->
        NewWords = Words,
        NewCol = 1,
        % We only add a newline if there are more words to follow.
        (
            NewWords = [],
            !:RevWordsSpacesNls = [Word, " " | !.RevWordsSpacesNls]
        ;
            NewWords = [_ | _],
            !:RevWordsSpacesNls = ["\n", Word, " " | !.RevWordsSpacesNls]
        )
    ;
        % Adding the word would take us over the line limit.
        ( CurCol = 1 ->
            % Break up words that are too big to fit on a line.
            RevPieces = break_up_string_reverse(Word, MaxCol - SepLen, []),
            (
                RevPieces = [LastPiece | Rest]
            ;
                RevPieces = [],
                unexpected($pred, "no pieces")
            ),
            NewWords = [LastPiece | Words],
            NewCol = 1,
            RestWithSep = list.map(func(S) = S ++ WordSep ++ "\n", Rest),
            !:RevWordsSpacesNls = RestWithSep ++ !.RevWordsSpacesNls
        ;
            NewWords = [Word | Words],
            NewCol = 1,
            !:RevWordsSpacesNls = ["\n" | !.RevWordsSpacesNls]
        )
    ),
    word_wrap_loop(NewWords, WordSep, SepLen, NewCol, MaxCol,
        !RevWordsSpacesNls).

:- func break_up_string_reverse(string, int, list(string)) = list(string).

break_up_string_reverse(Str, N, Prev) = Strs :-
    ( string.count_codepoints(Str) =< N ->
        Strs = [Str | Prev]
    ;
        string.split_by_codepoint(Str, N, Left, Right),
        Strs = break_up_string_reverse(Right, N, [Left | Prev])
    ).

%---------------------------------------------------------------------------%
%
% Folds over the characters in strings.
%

string.foldl(F, S, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldl(P, S, A, B).

string.foldl(Closure, String, !Acc) :-
    string.length(String, Length),
    string.foldl_between(Closure, String, 0, Length, !Acc).

string.foldl2(Closure, String, !Acc1, !Acc2) :-
    string.length(String, Length),
    string.foldl2_between(Closure, String, 0, Length, !Acc1, !Acc2).

string.foldl_between(Closure, String, Start0, End0, !Acc) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    string.foldl_between_2(Closure, String, Start, End, !Acc).

string.foldl_between(F, S, Start, End, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldl_between(P, S, Start, End, A, B).

string.foldl2_between(Closure, String, Start0, End0, !Acc1, !Acc2) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    string.foldl2_between_2(Closure, String, Start, End, !Acc1, !Acc2).

:- pred string.foldl_between_2(pred(char, A, A), string, int, int, A, A).
:- mode string.foldl_between_2(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldl_between_2(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldl_between_2(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldl_between_2(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldl_between_2(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

string.foldl_between_2(Closure, String, I, End, !Acc) :-
    (
        I < End,
        string.unsafe_index_next(String, I, J, Char),
        J =< End
    ->
        Closure(Char, !Acc),
        string.foldl_between_2(Closure, String, J, End, !Acc)
    ;
        true
    ).

:- pred string.foldl2_between_2(pred(char, A, A, B, B), string, int, int,
    A, A, B, B).
:- mode string.foldl2_between_2(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode string.foldl2_between_2(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode string.foldl2_between_2(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode string.foldl2_between_2(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode string.foldl2_between_2(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode string.foldl2_between_2(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

string.foldl2_between_2(Closure, String, I, End, !Acc1, !Acc2) :-
    (
        I < End,
        string.unsafe_index_next(String, I, J, Char),
        J =< End
    ->
        Closure(Char, !Acc1, !Acc2),
        string.foldl2_between_2(Closure, String, J, End, !Acc1, !Acc2)
    ;
        true
    ).

string.foldl_substring(F, String, Start, Count, Acc0) = Acc :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    Acc = string.foldl_between(F, String, ClampStart, ClampEnd, Acc0).

string.foldl_substring(Closure, String, Start, Count, !Acc) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.foldl_between(Closure, String, ClampStart, ClampEnd, !Acc).

string.foldl2_substring(Closure, String, Start, Count, !Acc1, !Acc2) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.foldl2_between(Closure, String, ClampStart, ClampEnd,
        !Acc1, !Acc2).

%---------------------%

string.foldr(F, String, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y)),
    string.foldr(Closure, String, Acc0, Acc).

string.foldr(Closure, String, !Acc) :-
    string.foldr_between(Closure, String, 0, length(String), !Acc).

string.foldr_between(F, String, Start, Count, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldr_between(Closure, String, Start, Count, Acc0, Acc).

string.foldr_between(Closure, String, Start0, End0, !Acc) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    string.foldr_between_2(Closure, String, Start, End, !Acc).

:- pred string.foldr_between_2(pred(char, T, T), string, int, int, T, T).
:- mode string.foldr_between_2(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldr_between_2(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldr_between_2(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldr_between_2(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldr_between_2(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

string.foldr_between_2(Closure, String, Start, I, !Acc) :-
    (
        I > Start,
        string.unsafe_prev_index(String, I, J, Char),
        J >= Start
    ->
        Closure(Char, !Acc),
        string.foldr_between_2(Closure, String, Start, J, !Acc)
    ;
        true
    ).

string.foldr_substring(F, String, Start, Count, Acc0) = Acc :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    Acc = string.foldr_between(F, String, ClampStart, ClampEnd, Acc0).

string.foldr_substring(Closure, String, Start, Count, !Acc) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.foldr_between(Closure, String, ClampStart, ClampEnd, !Acc).

%---------------------------------------------------------------------------%
%
% Formating tables.
%
% Currently, string.format_table simply assumes each code point occupies
% a single column in a fixed-width output device. Thus the output will
% only be aligned if limited to an (important) subset of characters,
% namely ASCII and European characters (excluding combining characters).
% It would be relatively easy to support CJK double-width characters
% and zero-width characters (see wcswidth), which would be enough
% to cover the needs of very many people.
%
% These considerations may also apply to predicates such as string.pad_left,
% string.pad_right, string.format (with field widths), string.word_wrap, etc.
%

string.format_table(Columns, Separator) = Table :-
    MaxWidths = list.map(find_max_length, Columns),
    % Maybe the code below should be replaced by the code of format_table_max,
    % with all maybe widths set to "no". They do the same job; the code of
    % format_table_max just does it more directly, without the excessive use
    % of higher order calls.
    PaddedColumns = list.map_corresponding(pad_column, MaxWidths, Columns),
    (
        PaddedColumns = [PaddedHead | PaddedTail],
        Rows = list.foldl(list.map_corresponding(
            string.join_rev_columns(Separator)), PaddedTail, PaddedHead)
    ;
        PaddedColumns = [],
        Rows = []
    ),
    Table = string.join_list("\n", Rows).

string.format_table_max(ColumnsLimits, Separator) = Table :-
    MaxWidthsSenses = list.map(find_max_length_with_limit, ColumnsLimits),
    Columns = list.map(project_column_strings, ColumnsLimits),
    SepLen = string.count_codepoints(Separator),
    generate_rows(MaxWidthsSenses, Separator, SepLen, Columns, [], RevRows),
    list.reverse(RevRows, Rows),
    Table = string.join_list("\n", Rows).

:- func project_column_strings(pair(justified_column, maybe(int)))
    = list(string).

project_column_strings(left(Strings) - _) = Strings.
project_column_strings(right(Strings) - _) = Strings.

:- pred generate_rows(assoc_list(justify_sense, int)::in, string::in, int::in,
    list(list(string))::in, list(string)::in, list(string)::out) is det.

generate_rows(MaxWidthsSenses, Separator, SepLen, Columns0, !RevRows) :-
    ( all_empty(Columns0) ->
        true
    ;
        get_next_line(Columns0, Line, Columns),
        pad_row(MaxWidthsSenses, Line, Separator, SepLen, 0, Row),
        !:RevRows = [Row | !.RevRows],
        generate_rows(MaxWidthsSenses, Separator, SepLen, Columns, !RevRows)
    ).

:- pred all_empty(list(list(string))::in) is semidet.

all_empty([]).
all_empty([List | Lists]) :-
    List = [],
    all_empty(Lists).

:- pred get_next_line(list(list(string))::in,
    list(string)::out, list(list(string))::out) is det.

get_next_line([], [], []).
get_next_line([Column | Columns], [ColumnTop | ColumnTops],
        [ColumnRest | ColumnRests]) :-
    (
        Column = [],
        error($pred, "list length mismatch")
    ;
        Column = [ColumnTop | ColumnRest]
    ),
    get_next_line(Columns, ColumnTops, ColumnRests).

:- pred pad_row(assoc_list(justify_sense, int)::in, list(string)::in,
    string::in, int::in, int::in, string::out) is det.

pad_row([], [], _, _, _, "").
pad_row([Justify - MaxWidth | JustifyWidths], [ColumnStr0 | ColumnStrs0],
        Separator, SepLen, CurColumn, Line) :-
    NextColumn = CurColumn + MaxWidth + SepLen,
    pad_row(JustifyWidths, ColumnStrs0, Separator, SepLen, NextColumn,
        LineRest),
    ( string.count_codepoints(ColumnStr0) =< MaxWidth ->
        (
            Justify = just_left,
            ColumnStr = string.pad_right(ColumnStr0, ' ', MaxWidth)
        ;
            Justify = just_right,
            ColumnStr = string.pad_left(ColumnStr0, ' ', MaxWidth)
        ),
        (
            JustifyWidths = [],
            Line = ColumnStr
        ;
            JustifyWidths = [_ | _],
            Line = ColumnStr ++ Separator ++ LineRest
        )
    ;
        (
            JustifyWidths = [],
            Line = ColumnStr0
        ;
            JustifyWidths = [_ | _],
            Line = ColumnStr0 ++ Separator ++ "\n" ++
                string.duplicate_char(' ', NextColumn) ++ LineRest
        )
    ).
pad_row([], [_ | _], _, _, _, _) :-
    error($pred, "list length mismatch").
pad_row([_ | _], [], _, _, _, _) :-
    error($pred, "list length mismatch").

:- func join_rev_columns(string, string, string) = string.

join_rev_columns(Separator, Col1, Col2) = Col2 ++ Separator ++ Col1.

:- func find_max_length(justified_column) = int.

find_max_length(JustColumn) = MaxLength :-
    ( JustColumn = left(Strings)
    ; JustColumn = right(Strings)
    ),
    list.foldl(max_str_length, Strings, 0, MaxLength).

:- type justify_sense
    --->    just_left
    ;       just_right.

:- func find_max_length_with_limit(pair(justified_column, maybe(int)))
    = pair(justify_sense, int).

find_max_length_with_limit(JustColumn - MaybeLimit) = Sense - MaxLength :-
    (
        JustColumn = left(Strings),
        Sense = just_left
    ;
        JustColumn = right(Strings),
        Sense = just_right
    ),
    list.foldl(max_str_length, Strings, 0, MaxLength0),
    (
        MaybeLimit = yes(Limit),
        ( MaxLength0 > Limit ->
            MaxLength = Limit
        ;
            MaxLength = MaxLength0
        )
    ;
        MaybeLimit = no,
        MaxLength = MaxLength0
    ).

:- func pad_column(int, justified_column) = list(string).

pad_column(Width, left(Strings)) =
    list.map(string.rpad(' ', Width), Strings).
pad_column(Width, right(Strings)) =
    list.map(string.lpad(' ', Width), Strings).

:- func rpad(char, int, string) = string.

rpad(Chr, N, Str) = string.pad_right(Str, Chr, N).

:- func lpad(char, int, string) = string.

lpad(Chr, N, Str) = string.pad_left(Str, Chr, N).

:- pred max_str_length(string::in, int::in, int::out) is det.

max_str_length(Str, PrevMaxLen, MaxLen) :-
    Length = string.count_codepoints(Str),
    ( Length > PrevMaxLen ->
        MaxLen = Length
    ;
        MaxLen = PrevMaxLen
    ).

%---------------------------------------------------------------------------%
%
% Converting strings to docs.
%

string.string_to_doc(S) = docs([str("\""), str(S), str("\"")]).

%---------------------------------------------------------------------------%
%
% Converting strings to values of builtin types.
%

string.to_int(String, Int) :-
    string.base_string_to_int(10, String, Int).

string.det_to_int(S) = string.det_base_string_to_int(10, S).

string.base_string_to_int(Base, String, Int) :-
    string.index(String, 0, Char),
    End = string.count_code_units(String),
    ( Char = ('-') ->
        End > 1,
        foldl_between(accumulate_negative_int(Base), String, 1, End, 0, Int)
    ; Char = ('+') ->
        End > 1,
        foldl_between(accumulate_int(Base), String, 1, End, 0, Int)
    ;
        foldl_between(accumulate_int(Base), String, 0, End, 0, Int)
    ).

string.det_base_string_to_int(Base, S) = N :-
    ( string.base_string_to_int(Base, S, N0) ->
        N = N0
    ;
        error("string.det_base_string_to_int: conversion failed")
    ).

:- pred accumulate_int(int::in, char::in, int::in, int::out) is semidet.

accumulate_int(Base, Char, N0, N) :-
    char.base_digit_to_int(Base, Char, M),
    N = (Base * N0) + M,
    ( N0 =< N ; Base \= 10 ).       % Fail on overflow for base 10 numbers.

:- pred accumulate_negative_int(int::in, char::in,
    int::in, int::out) is semidet.

accumulate_negative_int(Base, Char, N0, N) :-
    char.base_digit_to_int(Base, Char, M),
    N = (Base * N0) - M,
    ( N =< N0 ; Base \= 10 ).       % Fail on underflow for base 10 numbers.

%---------------------%

:- pragma foreign_export("C", string.to_float(in, out),
    "ML_string_to_float").
:- pragma foreign_export("IL", string.to_float(in, out),
    "ML_string_to_float").

:- pragma foreign_proc("C",
    string.to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    /*
    ** The %c checks for any erroneous characters appearing after the float;
    ** if there are then sscanf() will return 2 rather than 1.
    */
    char    tmpc;
    SUCCESS_INDICATOR =
        (!MR_isspace(FloatString[0])) &&
        (sscanf(FloatString, MR_FLT_FMT ""%c"", &FloatVal, &tmpc) == 1);
        /* MR_TRUE if sscanf succeeds, MR_FALSE otherwise */
}").
:- pragma foreign_proc("C#",
    string.to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    FloatVal = 0.0;     // FloatVal must be initialized to suppress
                        // error messages when the predicate fails.

    // leading or trailing whitespace is not allowed
    if (FloatString.Length == 0 ||
        System.Char.IsWhiteSpace(FloatString, 0) ||
        System.Char.IsWhiteSpace(FloatString, FloatString.Length - 1))
    {
        SUCCESS_INDICATOR = false;
    } else {
        /*
        ** XXX should we also catch System.OverflowException?
        */
        try {
            FloatVal = System.Convert.ToDouble(FloatString);
            SUCCESS_INDICATOR = true;
        } catch (System.FormatException) {
            SUCCESS_INDICATOR = false;
        }
    }
}").
:- pragma foreign_proc("Java",
    string.to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = 0.0;     // FloatVal must be initialized to suppress
                        // error messages when the predicate fails.

    // leading or trailing whitespace is not allowed
    if (FloatString.length() == 0 || FloatString.trim() != FloatString) {
        SUCCESS_INDICATOR = false;
    } else {
        try {
            FloatVal = java.lang.Double.parseDouble(FloatString);
            SUCCESS_INDICATOR = true;
        } catch(java.lang.NumberFormatException e) {
            // At this point it *should* in theory be safe just to set
            // SUCCESS_INDICATOR = false, since the Java API claims that
            // Double.parseDouble() will handle all the cases we require.
            // However, it turns out that in practice (tested with Sun's
            // Java 2 SDK, Standard Edition, version 1.3.1_04) Java actually
            // throws a NumberFormatException when you give it NaN or
            // infinity, so we handle these cases below.

            if (FloatString.equalsIgnoreCase(""nan"")) {
                FloatVal = java.lang.Double.NaN;
                SUCCESS_INDICATOR = true;
            } else if (FloatString.equalsIgnoreCase(""infinity"")) {
                FloatVal = java.lang.Double.POSITIVE_INFINITY;
                SUCCESS_INDICATOR = true;
            } else if (FloatString.substring(1).
                equalsIgnoreCase(""infinity""))
            {
                if (FloatString.charAt(0) == '+') {
                    FloatVal = java.lang.Double.POSITIVE_INFINITY;
                    SUCCESS_INDICATOR = true;
                } else if (FloatString.charAt(0) == '-') {
                    FloatVal = java.lang.Double.NEGATIVE_INFINITY;
                    SUCCESS_INDICATOR = true;
                } else {
                    SUCCESS_INDICATOR = false;
                }
            } else {
                SUCCESS_INDICATOR = false;
            }
        }
    }
").
:- pragma foreign_proc("Erlang",
    string.to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    S = binary_to_list(FloatString),
    % string:to_float fails on integers, so tack on a trailing '.0' string.
    case string:to_float(S ++ "".0"") of
        {FloatVal, []} ->
            SUCCESS_INDICATOR = true;
        {FloatVal, "".0""} ->
            SUCCESS_INDICATOR = true;
        _ ->
            SUCCESS_INDICATOR = false,
            FloatVal = -1.0
    end
").

string.det_to_float(FloatString) = Float :-
    ( string.to_float(FloatString, FloatPrime) ->
        Float = FloatPrime
    ;
        error("string.det_to_float/1: conversion failed.")
    ).

%---------------------------------------------------------------------------%
%
% Converting values of builtin types to strings.
%

string.char_to_string(Char, String) :-
    string.to_char_list(String, [Char]).

string.char_to_string(C) = S1 :-
    string.char_to_string(C, S1).

string.from_char(Char) = string.char_to_string(Char).

%---------------------%

string.int_to_string(N, Str) :-
    string.int_to_base_string(N, 10, Str).

string.int_to_string(N) = S1 :-
    string.int_to_string(N, S1).

string.from_int(N) = string.int_to_string(N).

string.int_to_base_string(N1, N2) = S2 :-
    string.int_to_base_string(N1, N2, S2).

string.int_to_base_string(N, Base, Str) :-
    ( 2 =< Base, Base =< 36 ->
        true
    ;
        error("string.int_to_base_string: invalid base")
    ),
    string.int_to_base_string_1(N, Base, Str).

:- pred string.int_to_base_string_1(int::in, int::in, string::uo) is det.

string.int_to_base_string_1(N, Base, Str) :-
    % Note that in order to handle MININT correctly, we need to do the
    % conversion of the absolute number into digits using negative numbers;
    % we can't use positive numbers, since -MININT overflows.
    ( N < 0 ->
        string.int_to_base_string_2(N, Base, ['-'], RevChars)
    ;
        NegN = 0 - N,
        string.int_to_base_string_2(NegN, Base, [], RevChars)
    ),
    string.from_rev_char_list(RevChars, Str).

:- pred string.int_to_base_string_2(int::in, int::in,
    list(char)::in, list(char)::out) is det.

string.int_to_base_string_2(NegN, Base, !RevChars) :-
    % string.int_to_base_string_2/3 is almost identical to
    % string.int_to_base_string_group_2/6 below so any changes here might
    % also need to be applied to string.int_to_base_string_group_2/3.
    ( NegN > -Base ->
        N = -NegN,
        DigitChar = char.det_base_int_to_digit(Base, N),
        !:RevChars = [DigitChar | !.RevChars]
    ;
        NegN1 = NegN // Base,
        N10 = (NegN1 * Base) - NegN,
        DigitChar = char.det_base_int_to_digit(Base, N10),
        string.int_to_base_string_2(NegN1, Base, !RevChars),
        !:RevChars = [DigitChar | !.RevChars]
    ).

string.int_to_string_thousands(N) =
    string.int_to_base_string_group(N, 10, 3, ",").

string.int_to_base_string_group(N, Base, GroupLength, Sep) = Str :-
    ( 2 =< Base, Base =< 36 ->
        true
    ;
        error("string.int_to_base_string_group: invalid base")
    ),
    string.int_to_base_string_group_1(N, Base, GroupLength, Sep, Str).

:- pred string.int_to_base_string_group_1(int::in, int::in, int::in,
    string::in, string::uo) is det.

string.int_to_base_string_group_1(N, Base, GroupLength, Sep, Str) :-
    % Note that in order to handle MININT correctly, we need to do
    % the conversion of the absolute number into digits using negative numbers
    % (we can't use positive numbers, since -MININT overflows)
    ( N < 0 ->
        string.int_to_base_string_group_2(N, Base, 0, GroupLength, Sep, Str1),
        string.append("-", Str1, Str)
    ;
        N1 = 0 - N,
        string.int_to_base_string_group_2(N1, Base, 0, GroupLength, Sep, Str)
    ).

    % int_to_base_string_group_2(NegN, Base, Curr, GroupLength, Sep, Str):
    %
    % GroupLength is how many digits there should be between separators.
    % Curr is how many digits have been processed since the last separator
    % was inserted.
    % string.int_to_base_string_group_2/6 is almost identical to
    % string.int_to_base_string_2/3 above so any changes here might also
    % need to be applied to string.int_to_base_string_2/3.
    %
:- pred string.int_to_base_string_group_2(int::in, int::in, int::in, int::in,
    string::in, string::uo) is det.

string.int_to_base_string_group_2(NegN, Base, Curr, GroupLength, Sep, Str) :-
    (
        Curr = GroupLength,
        GroupLength > 0
    ->
        string.int_to_base_string_group_2(NegN, Base, 0, GroupLength,
            Sep, Str1),
        string.append(Str1, Sep, Str)
    ;
        ( NegN > -Base ->
            N = -NegN,
            DigitChar = char.det_base_int_to_digit(Base, N),
            string.char_to_string(DigitChar, Str)
        ;
            NegN1 = NegN // Base,
            N10 = (NegN1 * Base) - NegN,
            DigitChar = char.det_base_int_to_digit(Base, N10),
            string.char_to_string(DigitChar, DigitString),
            string.int_to_base_string_group_2(NegN1, Base, Curr + 1,
                GroupLength, Sep, Str1),
            string.append(Str1, DigitString, Str)
        )
    ).

%---------------------%

:- pragma foreign_proc("C",
    string.float_to_string(Flt::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    /*
    ** For efficiency reasons we duplicate the C implementation
    ** of string.lowlevel_float_to_string.
    */
    MR_float_to_string(Flt, Str, MR_ALLOC_ID);
}").
:- pragma foreign_proc("C#",
    string.float_to_string(Flt::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (System.Double.IsNaN(Flt)) {
        Str = ""nan"";
    } else if (System.Double.IsPositiveInfinity(Flt)) {
        Str = ""infinity"";
    } else if (System.Double.IsNegativeInfinity(Flt)) {
        Str = ""-infinity"";
    } else {

        Str = Flt.ToString(""R"");

        /* Append '.0' if there is no 'e' or '.' in the string. */
        bool contains = false;
        foreach (char c in Str) {
            if (c == 'e' || c == 'E' || c == '.') {
                contains = true;
                break;
            }
        }
        if (!contains) {
            Str = Str + "".0"";
        }
    }
").

:- pragma foreign_proc("Java",
    string.float_to_string(Flt::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Double.isNaN(Flt)) {
        Str = ""nan"";
    } else if (Double.isInfinite(Flt)) {
        if (Flt < 0.0) {
            Str = ""-infinity"";
        } else {
            Str = ""infinity"";
        }
    } else {
        Str = java.lang.Double.toString(Flt);
    }
").

string.float_to_string(Float, unsafe_promise_unique(String)) :-
    % XXX This implementation has problems when the mantissa
    % cannot fit in an int.
    %
    % XXX The unsafe_promise_unique is needed because in
    % string.float_to_string_loop the call to string.to_float doesn't
    % have a (ui, out) mode hence the output string cannot be unique.
    String = string.float_to_string_loop(min_precision, Float).

:- func string.float_to_string_loop(int, float) = (string) is det.

string.float_to_string_loop(Prec, Float) = String :-
    string.format("%#." ++ int_to_string(Prec) ++ "g", [f(Float)], Tmp),
    ( Prec = max_precision ->
        String = Tmp
    ;
        ( string.to_float(Tmp, Float) ->
            String = Tmp
        ;
            String = string.float_to_string_loop(Prec + 1, Float)
        )
    ).

    % XXX For efficiency reasons, we assume that on non-C backends we use
    % double precision floats. However the commented out code provides
    % a general mechanism for calculating the required precision.
    %
:- func min_precision = int.

min_precision = 15.
% min_precision =
%   floor_to_int(float(mantissa_digits) * log2(float(radix)) / log2(10.0)).

:- func max_precision = int.

max_precision = min_precision + 2.

string.float_to_string(Float) = S2 :-
    string.float_to_string(Float, S2).

string.from_float(Float) = string.float_to_string(Float).

%---------------------%

string.c_pointer_to_string(P) = S :-
    string.c_pointer_to_string(P, S).

:- pragma foreign_proc("C#",
    string.c_pointer_to_string(C_Pointer::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Within the spirit of the function, at least. */
    if (C_Pointer == null) {
        Str = ""null"";
    } else {
        Str = C_Pointer.ToString();
    }
").
:- pragma foreign_proc("Java",
    string.c_pointer_to_string(C_Pointer::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Within the spirit of the function, at least. */
    if (C_Pointer == null) {
        Str = ""null"";
    } else {
        Str = C_Pointer.toString();
    }
").

string.c_pointer_to_string(C_Pointer, Str) :-
    private_builtin.unsafe_type_cast(C_Pointer, Int),
    Str = "c_pointer(0x" ++ string.int_to_base_string(Int, 16) ++ ")".

string.from_c_pointer(P) = S :-
    string.c_pointer_to_string(P, S).

%---------------------------------------------------------------------------%
%
% Converting values of arbitrary types to strings.
%

string.string(X) =
    string.to_string.string_impl(X).

string.string_ops(OpsTable, X) =
    string.to_string.string_ops_impl(OpsTable, X).

string.string_ops_noncanon(NonCanon, OpsTable, X, String) :-
    string.to_string.string_ops_noncanon_impl(NonCanon, OpsTable, X, String).

%---------------------------------------------------------------------------%
%
% Converting values to strings based on a format string.
%

string.format(FormatString, PolyList, String) :-
    string.format.format_impl(FormatString, PolyList, String).

string.format(S1, PT) = S2 :-
    string.format(S1, PT, S2).

%---------------------------------------------------------------------------%
