%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.m.
% Main authors: fjh, petdr.
% Stability: medium to high.
%
% This modules provides basic string handling facilities.
%
% Mercury strings are Unicode strings in either UTF-8 or UTF-16 encoding
% depending on the target language.
%
% When Mercury is compiled to C, strings are UTF-8 encoded, with a null
% character as the string terminator. A single code point requires one to four
% bytes (code units) to encode.
%
% When Mercury is compiled to Java or C#, strings are represented using the
% Java `String' or C# `System.String' types, both using UTF-16 encoding.
% A single code point requires one or two 16-bit integers (code units)
% to encode.
%
% When Mercury is compiled to Erlang, strings are represented as Erlang
% binaries using UTF-8 encoding.
%
% The Mercury compiler will only allow well-formed UTF-8 or UTF-16 string
% constants. However, it is possible to produce strings containing invalid
% UTF-8 or UTF-16 via I/O, foreign code, and substring operations.
% Predicates or functions that inspect strings may fail, throw an exception,
% or else behave in some special way when an ill-formed code unit sequence is
% encountered. Handling ill-formed sequences consistently in this module is
% ongoing work.
%
% Unexpected null characters embedded in the middle of strings can be a source
% of security vulnerabilities, so the Mercury library predicates and functions
% which create strings from (lists of) characters throw an exception if a null
% character is detected. Programmers must not create strings that might
% contain null characters using the foreign language interface.
%
% The builtin comparison operation on strings is also implementation
% dependent. The current implementation performs string comparison using
%
% - C's strcmp() function, when compiling to C;
% - Java's String.compareTo() method, when compiling to Java;
% - C#'s System.String.CompareOrdinal() method, when compiling to C#; and
% - Erlang's term comparison operator, when compiling to Erlang.
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
% that it is simpler to define them just once, and this is the logical
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
    %
    % In the forward mode:
    % If strings use UTF-8 encoding then each code unit in an ill-formed
    % sequence is replaced by U+FFFD REPLACEMENT CHARACTER in the list.
    % If strings use UTF-16 encoding then each unpaired surrogate code point
    % is returned as a separate code point in the list.
    %
    % The reverse mode of the predicate throws an exception if
    % the list of characters contains a null character.
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
    % The reverse mode of to_char_list/2 is deprecated because the implied
    % ability to round trip convert a string to a list then back to the same
    % string does not hold in the presence of ill-formed code unit sequences.
    %
%:- pragma obsolete_proc(to_char_list(uo, in), [from_char_list/2]).
:- func to_char_list(string) = list(char).
:- pred to_char_list(string, list(char)).
:- mode to_char_list(in, out) is det.
:- mode to_char_list(uo, in) is det.

    % Convert the string to a list of characters (code points) in reverse
    % order.
    %
    % In the forward mode:
    % If strings use UTF-8 encoding then each code unit in an ill-formed
    % sequence is replaced by U+FFFD REPLACEMENT CHARACTER in the list.
    % If strings use UTF-16 encoding then each unpaired surrogate code point
    % is returned as a separate code point in the list.
    %
    % The reverse mode of the predicate throws an exception if
    % the list of characters contains a null character.
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
    % The reverse mode of to_rev_char_list/2 is deprecated because the implied
    % ability to round trip convert a string to a list then back to the same
    % string does not hold in the presence of ill-formed code unit sequences.
    %
%:- pragma obsolete_proc(to_rev_char_list(uo, in), [from_rev_char_list/2]).
:- func to_rev_char_list(string) = list(char).
:- pred to_rev_char_list(string, list(char)).
:- mode to_rev_char_list(in, out) is det.
:- mode to_rev_char_list(uo, in) is det.

    % Convert a list of characters (code points) to a string.
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: In the future we may also throw an exception if the list contains
    % a surrogate code point.
    %
    % The forward mode of from_char_list/2 is deprecated because the implied
    % ability to round trip convert a string to a list then back to the same
    % string does not hold in the presence of ill-formed code unit sequences.
    %
%:- pragma obsolete_proc(from_char_list(out, in), [to_char_list/2]).
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

    % Convert a string into a list of code units of the string encoding used
    % by the current process.
    %
:- pred to_code_unit_list(string::in, list(int)::out) is det.

    % Convert a string into a list of UTF-8 code units.
    %
:- pred to_utf8_code_unit_list(string::in, list(int)::out) is det.

    % Convert a string into a list of UTF-16 code units.
    %
:- pred to_utf16_code_unit_list(string::in, list(int)::out) is det.

    % Convert a list of code units to a string.
    % Fails if the list does not contain a valid encoding of a string,
    % in the encoding expected by the current process.
    %
:- pred from_code_unit_list(list(int)::in, string::uo) is semidet.

    % Convert a list of UTF-8 code units to a string.
    % Fails if the list does not contain a valid encoding of a string.
    %
:- pred from_utf8_code_unit_list(list(int)::in, string::uo) is semidet.

    % Convert a list of UTF-8 code units to a string.
    % Fails if the list does not contain a valid encoding of a string.
    %
:- pred from_utf16_code_unit_list(list(int)::in, string::uo) is semidet.

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
    % If `Index' is the initial code unit offset of a well-formed code unit
    % sequence in `String' then `Char' is the code point encoded by that
    % sequence.
    %
    % If the code unit in `String' at `Index' is part of an ill-formed sequence
    % then `Char' is either a U+FFFD REPLACEMENT CHARACTER (when strings are
    % UTF-8 encoded) or the unpaired surrogate code point at `Index' (when
    % strings are UTF-16 encoded).
    %
    % Fails if `Index' is out of range (negative, or greater than or equal to
    % the length of `String').
    %
:- pred index(string::in, int::in, char::uo) is semidet.

    % det_index(String, Index, Char):
    %
    % Like index/3 but throws an exception if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    %
:- func det_index(string, int) = char.
:- pred det_index(string::in, int::in, char::uo) is det.

    % unsafe_index(String, Index, Char):
    %
    % Like index/3 but does not check that `Index' is in range.
    %
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
    % If `Index' is the initial code unit offset of a well-formed code unit
    % sequence in `String' then `Char' is the code point encoded by that
    % sequence, and `NextIndex' is the offset immediately following that
    % sequence.
    %
    % If the code unit in `String' at `Index' is part of an ill-formed sequence
    % then `Char' is either a U+FFFD REPLACEMENT CHARACTER (when strings are
    % UTF-8 encoded) or the unpaired surrogate code point at `Index' (when
    % strings are UTF-16 encoded), and `NextIndex' is Index + 1.
    %
    % Fails if `Index' is out of range (negative, or greater than or equal to
    % the length of `String').
    %
:- pred index_next(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_index_next(String, Index, NextIndex, Char):
    %
    % Like index_next/4 but does not check that `Index' is in range.
    % Fails if `Index' is equal to the length of `String'.
    %
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than the length of `String').
    %
:- pred unsafe_index_next(string::in, int::in, int::out, char::uo) is semidet.

    % prev_index(String, Index, PrevIndex, Char):
    %
    % If `Index - 1' is the final code unit offset of a well-formed sequence in
    % `String' then `Char' is the code point encoded by that sequence, and
    % `PrevIndex' is the initial code unit offset of that sequence.
    %
    % If the code unit in `String' at `Index - 1' is part of an ill-formed
    % sequence then `Char' is either a U+FFFD REPLACEMENT CHARACTER (when
    % strings are UTF-8 encoded) or the unpaired surrogate code point at
    % `Index - 1' (when strings are UTF-16 encoded), and `PrevIndex' is
    % `Index - 1'.
    %
    % Fails if `Index' is out of range (non-positive, or greater than the
    % length of `String').
    %
:- pred prev_index(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_prev_index(String, Index, PrevIndex, Char):
    %
    % Like prev_index/4 but does not check that `Index' is in range.
    % Fails if `Index' is zero.
    %
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than the length of `String').
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
    % Each valid code point, and each code unit that is part of an ill-formed
    % sequence, contributes one to the result.
    % (This matches the number of steps it would take to iterate over the
    % string using string.index_next or string.prev_index.)
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

    % codepoint_offset(String, StartOffset, Count, Offset):
    %
    % Let `S' be the substring of `String' from code unit `StartOffset' to the
    % end of the string. `Offset' is code unit offset after advancing `Count'
    % steps in `S', where each step skips over either:
    %  - one encoding of a Unicode code point, or
    %  - one code unit that is part of an ill-formed sequence.
    %
    % Fails if `StartOffset' is out of range (negative, or greater than the
    % length of `String'), or if there are fewer than `Count' steps possible
    % in `S'.
    %
:- pred codepoint_offset(string::in, int::in, int::in, int::out) is semidet.

    % codepoint_offset(String, Count, Offset):
    %
    % Same as `codepoint_offset(String, 0, Count, Offset)'.
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

    % Cross-compilation-friendly versions of hash, hash2 and hash3
    % respectively.
:- func hash4(string) = int.
:- func hash5(string) = int.
:- func hash6(string) = int.

%---------------------------------------------------------------------------%
%
% Tests on strings.
%

    % True if string is the empty string.
    %
:- pred is_empty(string::in) is semidet.

    % True if the string is a valid UTF-8 or UTF-16 string.
    % In target languages that use UTF-8 string encoding, `is_well_formed(S)'
    % is true iff S consists of a well-formed UTF-8 code unit sequence.
    % In target languages that use UTF-16 string encoding, `is_well_formed(S)'
    % is true iff S consists of a well-formed UTF-16 code unit sequence.
    %
:- pred is_well_formed(string::in) is semidet.

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

    % compare_substrings(Res, X, StartX, Y, StartY, Length):
    %
    % Compare two substrings by code unit order. The two substrings are
    % the substring of `X' between `StartX' and `StartX + Length', and
    % the substring of `Y' between `StartY' and `StartY + Length'.
    % `StartX', `StartY' and `Length' are all in terms of code units.
    %
    % Fails if `StartX' or `StartX + Length' are not within [0, length(X)],
    % or if `StartY' or `StartY + Length' are not within [0, length(Y)],
    % or if `Length' is negative.
    %
:- pred compare_substrings(comparison_result::uo, string::in, int::in,
    string::in, int::in, int::in) is semidet.

    % unsafe_compare_substrings(Res, X, StartX, Y, StartY, Length):
    %
    % Same as compare_between/4 but without range checks.
    % WARNING: if any of `StartX', `StartY', `StartX + Length' or
    % `StartY + Length' are out of range, or if `Length' is negative,
    % then the behaviour is UNDEFINED. Use with care!
    %
:- pred unsafe_compare_substrings(comparison_result::uo, string::in, int::in,
    string::in, int::in, int::in) is det.

    % compare_ignore_case_ascii(Res, X, Y):
    %
    % Compare two strings by code point order, ignoring the case of letters
    % (A-Z, a-z) in the ASCII range.
    % Equivalent to `compare(Res, to_lower(X), to_lower(Y))'
    % but more efficient.
    %
:- pred compare_ignore_case_ascii(comparison_result::uo,
    string::in, string::in) is det.

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

    % append(S1, S2, S3):
    %
    % Append two strings together. S3 consists of the code units of S1
    % followed by the code units of S2, in order.
    %
    % An ill-formed code unit sequence at the end of S1 may join with an
    % ill-formed code unit sequence at the start of S2 to produce a valid
    % encoding of a code point in S3.
    %
    % The append(out, out, in) mode is deprecated because it does not match
    % the semantics of the forwards modes in the presence of ill-formed code
    % unit sequences. Use nondet_append/3 instead.
    %
%:- pragma obsolete_proc(append(out, out, in), [nondet_append/3]).
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

    % nondet_append(S1, S2, S3):
    %
    % Non-deterministically return S1 and S2, where S1 ++ S2 = S3.
    % S3 is split after each code point or code unit in an ill-formed sequence.
    %
:- pred nondet_append(string, string, string).
:- mode nondet_append(out, out, in) is multi.

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
    % Split a string into two substrings at the code unit offset `Index'.
    % (If `Index' is out of the range [0, length of `String'], it is treated
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
    % `Start' and `End'. The result is equivalent to:
    %
    %   between(String, StartOffset, EndOffset, Substring)
    %
    % where:
    %
    %   StartOffset is from codepoint_offset(String, Start, StartOffset)
    %     if Start is in [0, count_codepoints(String)],
    %   StartOffset = 0 if Start < 0,
    %   StartOffset = length(String) otherwise;
    %
    %   EndOffset is from codepoint_offset(String, End, EndOffset)
    %     if End is in [0, count_codepoints(String)],
    %   EndOffset = 0 if End < 0,
    %   EndOffset = length(String) otherwise.
    %
    % between/4 will enforce StartOffset =< EndOffset.
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

    % split_at_separator(SepP, String) returns the list of (possibly empty)
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

    % det_remove_prefix(Prefix, String, Suffix):
    %
    % This is a synonym for append(Prefix, Suffix, String) but with the
    % arguments in a more convenient order for use with higher-order code.
    %
:- pred det_remove_prefix(string::in, string::in, string::out) is det.

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
    % remove_suffix, except it throws an exception if String does not end
    % with Suffix.
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
    % Only letters (a-z) in the ASCII range are converted.
    %
    % This function transforms the initial code point of a string,
    % whether or not the code point occurs as part of a combining sequence.
    %
:- func capitalize_first(string) = string.
:- pred capitalize_first(string::in, string::out) is det.

    % Convert the first character (if any) of a string to lowercase.
    % Only letters (A-Z) in the ASCII range are converted.
    %
    % This function transforms the initial code point of a string,
    % whether or not the code point occurs as part of a combining sequence.
    %
:- func uncapitalize_first(string) = string.
:- pred uncapitalize_first(string::in, string::out) is det.

    % Converts a string to uppercase.
    % Only letters (A-Z) in the ASCII range are converted.
    %
    % This function transforms each code point individually.
    % Letters that occur within a combining sequence will be converted,
    % whereas the precomposed character equivalent to the combining
    % sequence would not be converted. For example:
    %
    %   to_upper("a\u0301") ==> "A\u0301"   % á decomposed
    %   to_upper("\u00E1")  ==> "\u00E1"    % á precomposed
    %
:- func to_upper(string::in) = (string::uo) is det.
:- pred to_upper(string, string).
:- mode to_upper(in, uo) is det.
:- mode to_upper(in, in) is semidet.        % implied

    % Converts a string to lowercase.
    % Only letters (a-z) in the ASCII range are converted.
    %
    % This function transforms each code point individually.
    % Letters that occur within a combining sequence will be converted,
    % whereas the precomposed character equivalent to the combining
    % sequence would not be converted. For example:
    %
    %   to_lower("A\u0301") ==> "a\u0301"   % Á decomposed
    %   to_lower("\u00C1")  ==> "\u00C1"    % Á precomposed
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

    % strip(String):
    %
    % Returns `String' minus any initial and trailing ASCII whitespace
    % characters, i.e. characters satisfying `char.is_whitespace'.
    %
:- func strip(string) = string.

    % lstrip(String):
    %
    % Return `String' minus any initial ASCII whitespace characters,
    % i.e. characters satisfying `char.is_whitespace'.
    %
:- func lstrip(string) = string.

    % rstrip(String):
    %
    % Returns `String' minus any trailing ASCII whitespace characters,
    % i.e. characters satisfying `char.is_whitespace'.
    %
:- func rstrip(string) = string.

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
    % See `char.is_whitespace' for the definition of whitespace characters
    % used by this predicate.
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
    % character (code point) of the string `String' in turn.
    % If `String' contains ill-formed sequences, `Closure' is called for each
    % code unit in an ill-formed sequence. If strings use UTF-8 encoding,
    % U+FFFD is passed to `Closure' in place of each such code unit.
    % If strings use UTF-16 encoding, each code unit in an ill-formed sequence
    % is an unpaired surrogate code point, which will be passed to `Closure'.
    %
    % The initial value of the accumulator is `!.Acc' and the final value is
    % `!:Acc'.
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
    % Throws an exception if the columns are not all the same length.
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
    % but allows the caller to associate a maximum width with each column.
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
    % this syntax or the number is not in the range [min_int, max_int],
    % the predicate fails.
    %
:- pred base_string_to_int(int::in, string::in, int::out) is semidet.

    % Convert a signed base N string to an int. Throws an exception
    % if the string argument is not precisely an optional sign followed by
    % a non-empty string of base N digits and the number is in the range
    % [min_int, max_int].
    %
:- func det_base_string_to_int(int, string) = int.

    % Convert a string to a float, returning infinity or -infinity if the
    % conversion overflows. Fails if the string is not a syntactically correct
    % float literal.
    %
:- pred to_float(string::in, float::out) is semidet.

    % Convert a string to a float, returning infinity or -infinity if the
    % conversion overflows. Throws an exception if the string is not a
    % syntactically correct float literal.
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
    % Base must be between 2 and 36, both inclusive; if it is not,
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
    % Base must be between 2 and 36, both inclusive; if it is not,
    % the predicate will throw an exception.
    %
:- func int_to_base_string_group(int, int, int, string) = string.
:- mode int_to_base_string_group(in, in, in, in) = uo is det.

    % Convert an unsigned integer to a string.
    %
:- func uint_to_string(uint::in) = (string::uo) is det.

    % Convert a signed/unsigned 8/16/32/64 bit integer to a string.
    %
:- func int8_to_string(int8::in) = (string::uo) is det.
:- func uint8_to_string(uint8::in) = (string::uo) is det.
:- func int16_to_string(int16::in) = (string::uo) is det.
:- func uint16_to_string(uint16::in) = (string::uo) is det.
:- func int32_to_string(int32::in) = (string::uo) is det.
:- func uint32_to_string(uint32::in) = (string::uo) is det.
:- func int64_to_string(int64::in) = (string::uo) is det.
:- func uint64_to_string(uint64::in) = (string::uo) is det.

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
    % o     int     unsigned octal      with '0' prefix
    % x,X   int     unsigned hex        with '0x', '0X' prefix
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

%---------------------------------------------------------------------------%

:- implementation.

:- include_module parse_runtime.
:- include_module to_string.

:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.format.
:- import_module string.to_string.
:- import_module term_io.

% Many routines in this module are implemented using foreign language code.

:- pragma foreign_decl("C",
"
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include ""mercury_string.h""   // for MR_allocate_aligned_string*() etc.
#include ""mercury_tags.h""     // for MR_list_cons*()
").

%---------------------------------------------------------------------------%
%
% String encoding.
%

    % Succeed if the internal string encoding is UTF-8, fail if it is UTF-16.
    % No other encodings are supported.
    %
:- pred internal_encoding_is_utf8 is semidet.

:- pragma foreign_proc("C",
    internal_encoding_is_utf8,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = MR_TRUE;
").
:- pragma foreign_proc("C#",
    internal_encoding_is_utf8,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Java",
    internal_encoding_is_utf8,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Erlang",
    internal_encoding_is_utf8,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true
").

%---------------------------------------------------------------------------%
%
% Conversions between strings and lists of characters.
%

to_char_list(S) = Cs :-
    to_char_list(S, Cs).

:- pragma promise_equivalent_clauses(to_char_list/2).

to_char_list(Str::in, CharList::out) :-
    do_to_char_list(Str, CharList).
to_char_list(Str::uo, CharList::in) :-
    from_char_list(CharList, Str).

:- pred do_to_char_list(string::in, list(char)::out) is det.

do_to_char_list(Str, CharList) :-
    do_to_char_list_loop(Str, length(Str), [], CharList).

:- pred do_to_char_list_loop(string::in, int::in,
    list(char)::in, list(char)::out) is det.

do_to_char_list_loop(Str, Index0, !CharList) :-
    ( if string.unsafe_prev_index(Str, Index0, Index1, C) then
        !:CharList = [C | !.CharList],
        do_to_char_list_loop(Str, Index1, !CharList)
    else
        true
    ).

%---------------------%

to_rev_char_list(S) = Cs :-
    to_rev_char_list(S, Cs).

:- pragma promise_equivalent_clauses(to_rev_char_list/2).

to_rev_char_list(Str::in, CharList::out) :-
    do_to_rev_char_list(Str, CharList).
to_rev_char_list(Str::uo, CharList::in) :-
    from_rev_char_list(CharList, Str).

:- pred do_to_rev_char_list(string::in, list(char)::out) is det.

do_to_rev_char_list(Str, RevCharList) :-
    do_to_rev_char_list_loop(Str, 0, [], RevCharList).

:- pred do_to_rev_char_list_loop(string::in, int::in,
    list(char)::in, list(char)::out) is det.

do_to_rev_char_list_loop(Str, Index0, !RevCharList) :-
    ( if string.unsafe_index_next(Str, Index0, Index1, C) then
        !:RevCharList = [C | !.RevCharList],
        do_to_rev_char_list_loop(Str, Index1, !RevCharList)
    else
        true
    ).

%---------------------%

from_char_list(Cs) = S :-
    from_char_list(Cs, S).

:- pragma promise_equivalent_clauses(from_char_list/2).

from_char_list(Chars::out, Str::in) :-
    to_char_list(Str, Chars).
from_char_list(Chars::in, Str::uo) :-
    ( if semidet_from_char_list(Chars, Str0) then
        Str = Str0
    else
        unexpected($pred, "null character in list")
    ).

:- pragma foreign_proc("C",
    semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    // mode (uo, in) is det
    MR_Word char_list_ptr;
    size_t size;
    MR_Char c;

    // Loop to calculate list length + sizeof(MR_Word) in `size'
    // using list in `char_list_ptr'.
    size = 0;
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        c = (MR_Char) MR_list_head(char_list_ptr);
        if (MR_is_ascii(c)) {
            size++;
        } else {
            // XXX ILSEQ Do something if c is a surrogate code point.
            size += MR_utf8_width(c);
        }
        char_list_ptr = MR_list_tail(char_list_ptr);
    }

    // Allocate heap space for string.
    MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

    // Loop to copy the characters from the char_list to the string.
    SUCCESS_INDICATOR = MR_TRUE;
    size = 0;
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        c = (MR_Char) MR_list_head(char_list_ptr);
        // It is an error to put a null character in a string
        // (see the comments at the top of this file).
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
    semidet_from_char_list(CharList::in, Str::uo),
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
    semidet_from_char_list(CharList::in, Str::uo),
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
            // Fast path.
            sb.append((char) c);
        } else {
            sb.append(java.lang.Character.toChars(c));
        }
    }
    Str = sb.toString();
").
:- pragma foreign_proc("Erlang",
    semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = unicode:characters_to_binary(CharList),
    SUCCESS_INDICATOR = true
").

semidet_from_char_list(CharList, Str) :-
    (
        CharList = [],
        Str = ""
    ;
        CharList = [C | Cs],
        not char.to_int(C, 0),
        semidet_from_char_list(Cs, Str0),
        first_char(Str, C, Str0)
    ).

%---------------------%
%
% We could implement from_rev_char_list using list.reverse and from_char_list,
% but the optimized implementation in C below is there for efficiency.
% At the time this predicate was added, it improved the overall speed of
% parsing by about 7%.
%

from_rev_char_list(Cs) = S :-
    from_rev_char_list(Cs, S).

from_rev_char_list(Chars, Str) :-
    ( if semidet_from_rev_char_list(Chars, Str0) then
        Str = Str0
    else
        unexpected($pred, "null character in list")
    ).

:- pragma foreign_proc("C",
    semidet_from_rev_char_list(Chars::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list_ptr;
    MR_Word size;
    MR_Char c;

    // Loop to calculate list length in `size' using list in `list_ptr'.
    size = 0;
    list_ptr = Chars;
    while (!MR_list_is_empty(list_ptr)) {
        c = (MR_Char) MR_list_head(list_ptr);
        if (MR_is_ascii(c)) {
            size++;
        } else {
            // XXX ILSEQ Do something if c is a surrogate code point.
            size += MR_utf8_width(c);
        }
        list_ptr = MR_list_tail(list_ptr);
    }

    // Allocate heap space for string.
    MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

    // Set size to be the offset of the end of the string
    // (ie the \\0) and null terminate the string.
    Str[size] = '\\0';

    // Loop to copy the characters from the list_ptr to the string
    // in reverse order.
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
    semidet_from_rev_char_list(Chars::in, Str::uo),
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

semidet_from_rev_char_list(Chars::in, Str::uo) :-
    semidet_from_char_list(list.reverse(Chars), Str).

%---------------------%

to_code_unit_list(String, List) :-
    to_code_unit_list_loop(String, 0, length(String), List).

:- pred to_code_unit_list_loop(string::in, int::in, int::in,
    list(int)::out) is det.

to_code_unit_list_loop(String, Index, End, List) :-
    ( if Index >= End then
        List = []
    else
        unsafe_index_code_unit(String, Index, Code),
        to_code_unit_list_loop(String, Index + 1, End, Tail),
        List = [Code | Tail]
    ).

%---------------------%

% XXX ILSEQ Behaviour differs according to target language.
%   - java: throws exception on unpaired surrogate (correct as written)
%   - csharp: infinite loop on string containing unpaired surrogate

to_utf8_code_unit_list(String, CodeList) :-
    ( if internal_encoding_is_utf8 then
        to_code_unit_list(String, CodeList)
    else
        foldr(encode_utf8, String, [], CodeList)
    ).

:- pred encode_utf8(char::in, list(int)::in, list(int)::out) is det.

encode_utf8(Char, CodeList0, CodeList) :-
    ( if char.to_utf8(Char, CharCodes) then
        CodeList = CharCodes ++ CodeList0
    else
        unexpected($pred, "char.to_utf8 failed")
    ).

%---------------------%

% XXX ILSEQ On C backend, to_utf16_code_unit_list stops at the first
% ill-formed sequence from the end of the string (except that the C version of
% unsafe_prev_index currently may skip extraneous trailing bytes).

to_utf16_code_unit_list(String, CodeList) :-
    ( if internal_encoding_is_utf8 then
        foldr(encode_utf16, String, [], CodeList)
    else
        to_code_unit_list(String, CodeList)
    ).

:- pred encode_utf16(char::in, list(int)::in, list(int)::out) is det.

encode_utf16(Char, CodeList0, CodeList) :-
    ( if char.to_utf16(Char, CharCodes) then
        CodeList = CharCodes ++ CodeList0
    else
        unexpected($pred, "char.to_utf16 failed")
    ).

%---------------------%

% XXX ILSEQ to_code_unit_list(S0, L), from_code_unit_list(L, S) may fail
% because the original string contained an ill-formed sequence.
% Perhaps we should provide a predicate that can produce the original string.

:- pragma foreign_proc("C",
    from_code_unit_list(CodeList::in, Str::uo),
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
        // It is an error to put a null character in a string
        // (see the comments at the top of this file).
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
    from_code_unit_list(CodeList::in, Str::uo),
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
    from_code_unit_list(CodeList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    bool prev_high = false;

    SUCCESS_INDICATOR = true;

    while (!list.is_empty(CodeList)) {
        // Both casts are required.
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
    from_code_unit_list(CodeList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = list_to_binary(CodeList),
    % XXX validate the string
    SUCCESS_INDICATOR = true
").

%---------------------%

from_utf8_code_unit_list(CodeList, String) :-
    ( if internal_encoding_is_utf8 then
        from_code_unit_list(CodeList, String)
    else
        decode_utf8(CodeList, [], RevChars),
        from_rev_char_list(RevChars, String)
    ).

:- pred decode_utf8(list(int)::in, list(char)::in, list(char)::out) is semidet.

decode_utf8([], RevChars, RevChars).
decode_utf8([A | FollowA], RevChars0, RevChars) :-
    ( if A < 0 then
        fail
    else if A =< 0x7f then  % 1-byte sequence
        CharInt = A,
        Rest = FollowA
    else if A =< 0xc1 then
        fail
    else if A =< 0xdf then  % 2-byte sequence
        FollowA = [B | Rest],
        utf8_is_trail_byte(B),
        CharInt = (A /\ 0x1f) << 6
               \/ (B /\ 0x3f),
        CharInt >= 0x80
    else if A =< 0xef then  % 3-byte sequence
        FollowA = [B, C | Rest],
        utf8_is_trail_byte(B),
        utf8_is_trail_byte(C),
        CharInt = (A /\ 0x0f) << 12
               \/ (B /\ 0x3f) << 6
               \/ (C /\ 0x3f),
        CharInt >= 0x800
    else if A =< 0xf4 then  % 4-byte sequence
        FollowA = [B, C, D | Rest],
        utf8_is_trail_byte(B),
        utf8_is_trail_byte(C),
        utf8_is_trail_byte(D),
        CharInt = (A /\ 0x07) << 18
               \/ (B /\ 0x3f) << 12
               \/ (C /\ 0x3f) << 6
               \/ (D /\ 0x3f),
        CharInt >= 0x10000
    else
        fail
    ),
    char.from_int(CharInt, Char),
    decode_utf8(Rest, [Char | RevChars0], RevChars).

:- pred utf8_is_trail_byte(int::in) is semidet.

utf8_is_trail_byte(C) :-
    (C /\ 0xc0) = 0x80.

%---------------------%

from_utf16_code_unit_list(CodeList, String) :-
    ( if internal_encoding_is_utf8 then
        decode_utf16(CodeList, [], RevChars),
        from_rev_char_list(RevChars, String)
    else
        from_code_unit_list(CodeList, String)
    ).

:- pred decode_utf16(list(int)::in, list(char)::in, list(char)::out)
    is semidet.

decode_utf16([], RevChars, RevChars).
decode_utf16([A | FollowA], RevChars0, RevChars) :-
    ( if A < 0 then
        fail
    else if A < 0xd800 then
        CharInt = A,
        Rest = FollowA
    else if A < 0xdc00 then
        FollowA = [B | Rest],
        B >= 0xdc00,
        B =< 0xdfff,
        CharInt = (A << 10) + B - 0x35fdc00
    else if A =< 0xffff then
        CharInt = A,
        Rest = FollowA
    else
        fail
    ),
    char.from_int(CharInt, Char),
    decode_utf16(Rest, [Char | RevChars0], RevChars).

%---------------------%

duplicate_char(C, N) = S :-
    duplicate_char(C, N, S).

duplicate_char(Char, Count, String) :-
    String = from_char_list(list.duplicate(Count, Char)).

%---------------------------------------------------------------------------%
%
% Reading characters from strings.
%

% It is important to inline predicates that index into strings,
% so that the compiler can do loop invariant hoisting on calls to them
% that occur in loops.

% XXX ILSEQ
% We should allow the possibility of working with strings containing ill-formed
% sequences. That would require predicates that can return either a code point
% when possible, or else code units from ill-formed sequences.

:- pragma inline(index/3).
:- pragma inline(det_index/3).
:- pragma inline(index_next/4).
:- pragma inline(prev_index/4).

index(Str, Index, Char) :-
    Len = length(Str),
    ( if index_check(Index, Len) then
        unsafe_index(Str, Index, Char)
    else
        fail
    ).

det_index(S, N) = C :-
    det_index(S, N, C).

det_index(String, Int, Char) :-
    ( if index(String, Int, Char0) then
        Char = Char0
    else
        unexpected($pred, "index out of range")
    ).

unsafe_index(S, N) = C :-
    unsafe_index(S, N, C).

:- pragma foreign_proc("C",
    unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Ch = Str[Index];
    if (!MR_is_ascii(Ch)) {
        int width;
        Ch = MR_utf8_get_mb(Str, Index, &width);
        if (Ch < 0) {
            Ch = 0xFFFD;
        }
    }
").
:- pragma foreign_proc("C#",
    unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    char c1 = Str[Index];
    Ch = c1;
    if (System.Char.IsSurrogate(c1)) {
        try {
            char c2 = Str[Index + 1];
            Ch = System.Char.ConvertToUtf32(c1, c2);
        } catch (System.ArgumentOutOfRangeException) {
            // Return unpaired surrogate code point.
        } catch (System.IndexOutOfRangeException) {
            // Return unpaired surrogate code point.
        }
    }
").
:- pragma foreign_proc("Java",
    unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ch = Str.codePointAt(Index);
").
:- pragma foreign_proc("Erlang",
    unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX does not handle ill-formed sequences as described
    <<_:Index/binary, Ch/utf8, _/binary>> = Str
").

%---------------------%

String ^ elem(Index) = det_index(String, Index).
String ^ unsafe_elem(Index) = unsafe_index(String, Index).

%---------------------%

index_next(Str, Index, NextIndex, Char) :-
    Len = length(Str),
    ( if index_check(Index, Len) then
        unsafe_index_next(Str, Index, NextIndex, Char)
    else
        fail
    ).

:- pragma foreign_proc("C",
    unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
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
        if (Ch < 0) {
            Ch = 0xfffd;
            NextIndex = Index + 1;
        }
        SUCCESS_INDICATOR = MR_TRUE;
    }
").
:- pragma foreign_proc("C#",
    unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    try {
        Ch = System.Char.ConvertToUtf32(Str, Index);
        if (Ch <= 0xffff) {
            NextIndex = Index + 1;
        } else {
            NextIndex = Index + 2;
        }
        SUCCESS_INDICATOR = true;
    } catch (System.ArgumentOutOfRangeException) {
        Ch = 0;
        NextIndex = Index;
        SUCCESS_INDICATOR = false;
    } catch (System.ArgumentException) {
        // Return unpaired surrogate code point.
        Ch = Str[Index];
        NextIndex = Index + 1;
        SUCCESS_INDICATOR = true;
    }
").
:- pragma foreign_proc("Java",
    unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    try {
        Ch = Str.codePointAt(Index);
        NextIndex = Index + java.lang.Character.charCount(Ch);
        SUCCESS_INDICATOR = true;
    } catch (IndexOutOfBoundsException e) {
        Ch = 0;
        NextIndex = Index;
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Erlang",
    unsafe_index_next(Str::in, Index::in, NextIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    % XXX does not handle ill-formed sequences as described
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

prev_index(Str, Index, PrevIndex, Char) :-
    Len = length(Str),
    ( if index_check(Index - 1, Len) then
        unsafe_prev_index(Str, Index, PrevIndex, Char)
    else
        fail
    ).

:- pragma foreign_proc("C",
    unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index <= 0) {
        PrevIndex = Index;
        Ch = 0;
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        PrevIndex = Index - 1;
        Ch = Str[PrevIndex];
        if (! MR_is_ascii(Ch)) {
            Ch = MR_utf8_prev_get(Str, &PrevIndex);
            // XXX MR_utf8_prev_get currently just scans backwards to find a
            // lead byte, so we need a separate check to ensure no bytes are
            // unaccounted for.
            if (Ch < 0 || PrevIndex + MR_utf8_width(Ch) != Index) {
                Ch = 0xfffd;
                PrevIndex = Index - 1;
            }
        }
        SUCCESS_INDICATOR = MR_TRUE;
    }
").
:- pragma foreign_proc("C#",
    unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    if (Index <= 0) {
        Ch = 0;
        PrevIndex = Index;
        SUCCESS_INDICATOR = false;
    } else {
        char c2 = Str[Index - 1];
        if (System.Char.IsLowSurrogate(c2)) {
            try {
                char c1 = Str[Index - 2];
                Ch = System.Char.ConvertToUtf32(c1, c2);
                PrevIndex = Index - 2;
            } catch (System.ArgumentOutOfRangeException) {
                // Return unpaired surrogate code point.
                Ch = (int) c2;
                PrevIndex = Index - 1;
            } catch (System.IndexOutOfRangeException) {
                // Return unpaired surrogate code point.
                Ch = (int) c2;
                PrevIndex = Index - 1;
            }
        } else {
            Ch = (int) c2;
            PrevIndex = Index - 1;
        }
        SUCCESS_INDICATOR = true;
    }
").
:- pragma foreign_proc("Java",
    unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    try {
        Ch = Str.codePointBefore(Index);
        PrevIndex = Index - java.lang.Character.charCount(Ch);
        SUCCESS_INDICATOR = true;
    } catch (IndexOutOfBoundsException e) {
        Ch = 0;
        PrevIndex = Index;
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Erlang",
    unsafe_prev_index(Str::in, Index::in, PrevIndex::out, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    % XXX does not handle ill-formed sequences as described
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
:- pred index_check(int::in, int::in) is semidet.

:- pragma foreign_proc("C",
    index_check(Index::in, Length::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    // We do not test for negative values of Index because (a) MR_Unsigned
    // is unsigned and hence a negative argument will appear as a very large
    // positive one after the cast and (b) anybody dealing with the case
    // where strlen(Str) > MAXINT is clearly barking mad (and one may well get
    // an integer overflow error in this case).
    SUCCESS_INDICATOR = ((MR_Unsigned) Index < (MR_Unsigned) Length);
").
:- pragma foreign_proc("C#",
    index_check(Index::in, Length::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((uint) Index < (uint) Length);
").

index_check(Index, Length) :-
    Index >= 0,
    Index < Length.

%---------------------%

:- pragma foreign_proc("C",
    unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    const unsigned char *s = (const unsigned char *) Str;
    Code = s[Index];
").
:- pragma foreign_proc("C#",
    unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Code = Str[Index];
").
:- pragma foreign_proc("Java",
    unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Code = Str.charAt(Index);
").
:- pragma foreign_proc("Erlang",
    unsafe_index_code_unit(Str::in, Index::in, Code::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<_:Index/binary, Code/integer, _/binary>> = Str
").

%---------------------------------------------------------------------------%
%
% Writing characters to strings.
%

% XXX ILSEQ If Index does not point to the start of a well-formed sequence,
% consider making set_char/unsafe_set_char replace the code unit at Index with
% the encoding of Char, i.e. treat the code unit as its own pseudo code point.

set_char(Char, Index, !Str) :-
    ( if char.to_int(Char, 0) then
        unexpected($pred, "null character")
    else
        set_char_non_null(Char, Index, !Str)
    ).

:- pred set_char_non_null(char, int, string, string).
:- mode set_char_non_null(in, in, in, out) is semidet.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode set_char_non_null(in, in, di, uo) is semidet.

:- pragma foreign_proc("C",
    set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    size_t len = strlen(Str0);
    if ((MR_Unsigned) Index >= len) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (MR_is_ascii(Str0[Index]) && MR_is_ascii(Ch)) {
        // Fast path.
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
    set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Index < 0 || Index >= Str0.Length) {
        Str = null;
        SUCCESS_INDICATOR = false;
    } else {
        System.Text.StringBuilder sb = new System.Text.StringBuilder(Str0);
        if (!System.Char.IsHighSurrogate(Str0, Index) && Ch <= 0xffff) {
            // Fast path.
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
    set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
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
    set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
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

det_set_char(C, N, S0) = S :-
    det_set_char(C, N, S0, S).

det_set_char(Char, Int, String0, String) :-
    ( if set_char(Char, Int, String0, String1) then
        String = String1
    else
        unexpected($pred, "index out of range")
    ).

%---------------------%

unsafe_set_char(C, N, S0) = S :-
    unsafe_set_char(C, N, S0, S).

unsafe_set_char(Char, Index, !Str) :-
    ( if char.to_int(Char, 0) then
        unexpected($pred, "null character")
    else
        unsafe_set_char_non_null(Char, Index, !Str)
    ).

:- pred unsafe_set_char_non_null(char, int, string, string).
:- mode unsafe_set_char_non_null(in, in, in, out) is det.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode unsafe_set_char_non_null(in, in, di, uo) is det.

:- pragma foreign_proc("C",
    unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    size_t len = strlen(Str0);
    if (MR_is_ascii(Str0[Index]) && MR_is_ascii(Ch)) {
        // Fast path.
        MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);
        strcpy(Str, Str0);
        Str[Index] = Ch;
    } else {
        // XXX ILSEQ Fail for surrogate code points.
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
    unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
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
    unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int oldc = Str0.codePointAt(Index);
    int oldwidth = java.lang.Character.charCount(oldc);
    Str = Str0.subSequence(0, Index)
        + new String(Character.toChars(Ch))
        + Str0.subSequence(Index + oldwidth, Str0.length());
").
:- pragma foreign_proc("Erlang",
    unsafe_set_char_non_null(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<Left:Index/binary, _/utf8, Right/binary>> = Str0,
    Str = unicode:characters_to_binary([Left, Ch, Right])
").

%---------------------------------------------------------------------------%
%
% Determining the lengths of strings.
%

length(S) = L :-
    length(S, L).

:- pragma promise_equivalent_clauses(length/2).

:- pragma foreign_proc("C",
    length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Length = strlen(Str);
").
:- pragma foreign_proc("C#",
    length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.Length;
").
:- pragma foreign_proc("Java",
    length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.length();
").
:- pragma foreign_proc("Erlang",
    length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = size(Str)
").

:- pragma foreign_proc("C",
    length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Length = strlen(Str);
").
:- pragma foreign_proc("C#",
    length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.Length;
").
:- pragma foreign_proc("Java",
    length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = Str.length();
").
:- pragma foreign_proc("Erlang",
    length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = size(Str)
").

length(Str, Len) :-
    to_code_unit_list(Str, CodeList),
    list.length(CodeList, Len).

count_code_units(Str) = length(Str).

count_code_units(Str, Length) :-
    length(Str, Length).

%---------------------%

count_codepoints(String) = Count :-
    count_codepoints(String, Count).

:- pragma foreign_proc("Java",
    count_codepoints(String::in, Count::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Count = String.codePointCount(0, String.length());
").

count_codepoints(String, Count) :-
    count_codepoints_loop(String, 0, 0, Count).

:- pred count_codepoints_loop(string::in, int::in, int::in, int::out) is det.

count_codepoints_loop(String, I, Count0, Count) :-
    ( if unsafe_index_next(String, I, J, _) then
        count_codepoints_loop(String, J, Count0 + 1, Count)
    else
        Count = Count0
    ).

%---------------------%

% XXX ILSEQ Behaviour depends on target language.
% In UTF-16 grades, count_utf8_code_units uses string.foldl which (currently)
% stops at the first ill-formed sequence.

:- pragma foreign_proc("C",
    count_utf8_code_units(Str::in) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = strlen(Str);
").
:- pragma foreign_proc("Erlang",
    count_utf8_code_units(Str::in) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = size(Str)
").

count_utf8_code_units(String) = Length :-
    foldl(count_utf8_code_units_2, String, 0, Length).

:- pred count_utf8_code_units_2(char::in, int::in, int::out) is det.

count_utf8_code_units_2(Char, !Length) :-
    char.to_int(Char, CharInt),
    ( if CharInt =< 0x7f then
        !:Length = !.Length + 1
    else if char.to_utf8(Char, UTF8) then
        !:Length = !.Length + list.length(UTF8)
    else
        error($pred, "char.to_utf8 failed")
    ).

%---------------------%

:- pragma foreign_proc("Java",
    codepoint_offset(String::in, StartOffset::in, N::in, Index::out),
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

codepoint_offset(String, StartOffset, N, Index) :-
    StartOffset >= 0,
    Length = length(String),
    codepoint_offset_loop(String, StartOffset, Length, N, Index).

:- pred codepoint_offset_loop(string::in, int::in, int::in, int::in, int::out)
    is semidet.

codepoint_offset_loop(String, Offset, Length, N, Index) :-
    Offset < Length,
    ( if N = 0 then
        Index = Offset
    else
        unsafe_index_next(String, Offset, NextOffset, _),
        codepoint_offset_loop(String, NextOffset, Length, N - 1, Index)
    ).

%---------------------------------------------------------------------------%

codepoint_offset(String, N, Index) :-
    codepoint_offset(String, 0, N, Index).

%---------------------------------------------------------------------------%
%
% Computing hashes of strings.
%
% Note that these functions are also defined in runtime/mercury_string.h.
% The definition here and in mercury_string.h must be kept equivalent.
%

hash(String) = HashVal :-
    hash(String, HashVal).

hash(String, HashVal) :-
    length(String, Length),
    hash_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred hash_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

hash_loop(String, Index, Length, !HashVal) :-
    ( if Index < Length then
        unsafe_index_code_unit(String, Index, C),
        !:HashVal = !.HashVal `xor` (!.HashVal `unchecked_left_shift` 5),
        !:HashVal = !.HashVal `xor` C,
        hash_loop(String, Index + 1, Length, !HashVal)
    else
        true
    ).

hash2(String) = HashVal :-
    length(String, Length),
    hash2_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred hash2_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

hash2_loop(String, Index, Length, !HashVal) :-
    ( if Index < Length then
        unsafe_index_code_unit(String, Index, C),
        !:HashVal = !.HashVal * 37,
        !:HashVal = !.HashVal + C,
        hash2_loop(String, Index + 1, Length, !HashVal)
    else
        true
    ).

hash3(String) = HashVal :-
    length(String, Length),
    hash3_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred hash3_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

hash3_loop(String, Index, Length, !HashVal) :-
    ( if Index < Length then
        unsafe_index_code_unit(String, Index, C),
        !:HashVal = !.HashVal * 49,
        !:HashVal = !.HashVal + C,
        hash3_loop(String, Index + 1, Length, !HashVal)
    else
        true
    ).

:- func keep_30_bits(int) = int.

keep_30_bits(N) = N /\ ((1 `unchecked_left_shift` 30) - 1).

hash4(String) = HashVal :-
    length(String, Length),
    hash4_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred hash4_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

hash4_loop(String, Index, Length, !HashVal) :-
    ( if Index < Length then
        unsafe_index_code_unit(String, Index, C),
        !:HashVal = keep_30_bits(!.HashVal `xor`
            (!.HashVal `unchecked_left_shift` 5)),
        !:HashVal = !.HashVal `xor` C,
        hash4_loop(String, Index + 1, Length, !HashVal)
    else
        true
    ).

hash5(String) = HashVal :-
    length(String, Length),
    hash5_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred hash5_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

hash5_loop(String, Index, Length, !HashVal) :-
    ( if Index < Length then
        unsafe_index_code_unit(String, Index, C),
        !:HashVal = keep_30_bits(!.HashVal * 37),
        !:HashVal = keep_30_bits(!.HashVal + C),
        hash5_loop(String, Index + 1, Length, !HashVal)
    else
        true
    ).

hash6(String) = HashVal :-
    length(String, Length),
    hash6_loop(String, 0, Length, 0, HashVal1),
    HashVal = HashVal1 `xor` Length.

:- pred hash6_loop(string::in, int::in, int::in, int::in, int::out)
    is det.

hash6_loop(String, Index, Length, !HashVal) :-
    ( if Index < Length then
        unsafe_index_code_unit(String, Index, C),
        !:HashVal = keep_30_bits(!.HashVal * 49),
        !:HashVal = keep_30_bits(!.HashVal + C),
        hash6_loop(String, Index + 1, Length, !HashVal)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Tests on strings.
%

is_empty("").

%---------------------%

:- pragma foreign_proc("C",
    is_well_formed(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = MR_utf8_verify(S);
").
:- pragma foreign_proc("Java",
    is_well_formed(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = true;
    for (int i = 0; i < S.length(); i++) {
        if (java.lang.Character.isLowSurrogate(S.charAt(i))) {
            SUCCESS_INDICATOR = false;
            break;
        }
        if (java.lang.Character.isHighSurrogate(S.charAt(i))) {
            i++;
            if (i >= S.length() ||
                !java.lang.Character.isLowSurrogate(S.charAt(i)))
            {
                SUCCESS_INDICATOR = false;
                break;
            }
        }
    }
").
:- pragma foreign_proc("C#",
    is_well_formed(S::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = true;
    for (int i = 0; i < S.Length; i++) {
        if (System.Char.IsLowSurrogate(S[i])) {
            SUCCESS_INDICATOR = false;
            break;
        }
        if (System.Char.IsHighSurrogate(S[i])) {
            i++;
            if (i >= S.Length || !System.Char.IsLowSurrogate(S[i])) {
                SUCCESS_INDICATOR = false;
                break;
            }
        }
    }
").

is_well_formed(_) :-
    sorry($module, "string.is_well_formed/1").

%---------------------%

% For speed, most of these predicates have C versions as well as
% Mercury versions. XXX why not all?

% XXX ILSEQ Behaviour depends on target language.
% The generic versions use all_match which currently uses unsafe_index_next and
% ignores the first ill-formed sequence and everything thereafter.

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

% XXX ILSEQ all_match should fail if it encounters an ill-formed sequence;
% instead it acts as if the String ends there.

all_match(P, String) :-
    all_match_loop(P, String, 0).

:- pred all_match_loop(pred(char)::in(pred(in) is semidet), string::in,
    int::in) is semidet.

all_match_loop(P, String, Cur) :-
    ( if unsafe_index_next(String, Cur, Next, Char) then
        P(Char),
        all_match_loop(P, String, Next)
    else
        true
    ).

%---------------------%

% XXX ILSEQ Behaviour depends on target language.
%  - C/C#/Java: ill-formed sequences don't prevent matching of later chars
%  - generic: unsafe_index_next stops at first ill-formed sequence

    % strchr always returns true when searching for '\0',
    % but the '\0' is an implementation detail which really
    % shouldn't be considered to be part of the string itself.
:- pragma foreign_proc("C",
    contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char    buf[5];
    size_t  len;
    if (MR_is_ascii(Ch)) {
        // Fast path.
        SUCCESS_INDICATOR = (strchr(Str, Ch) != NULL) && Ch != '\\0';
    } else {
        // XXX ILSEQ Handle Ch being surrogate or invalid valid.
        len = MR_utf8_encode(buf, Ch);
        buf[len] = '\\0';
        SUCCESS_INDICATOR = (strstr(Str, buf) != NULL);
    }
").
:- pragma foreign_proc("C#",
    contains_char(Str::in, Ch::in),
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
    contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // indexOf(int) handles supplementary characters correctly.
    SUCCESS_INDICATOR = (Str.indexOf((int) Ch) != -1);
").

contains_char(String, Char) :-
    contains_char(String, Char, 0).

:- pred contains_char(string::in, char::in, int::in) is semidet.

contains_char(Str, Char, I) :-
    ( if unsafe_index_next(Str, I, J, IndexChar) then
        ( if IndexChar = Char then
            true
        else
            contains_char(Str, Char, J)
        )
    else
        fail
    ).

%---------------------%

compare_substrings(Res, X, StartX, Y, StartY, Length) :-
    LengthX = length(X),
    LengthY = length(Y),
    ( if
        Length >= 0,
        StartX >= 0,
        StartY >= 0,
        StartX + Length =< LengthX,
        StartY + Length =< LengthY
    then
        unsafe_compare_substrings(Res, X, StartX, Y, StartY, Length)
    else
        fail
    ).

:- pragma foreign_proc("C",
    unsafe_compare_substrings(Res::uo, X::in, StartX::in, Y::in, StartY::in,
        Length::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    int res = memcmp(X + StartX, Y + StartY, Length);
    Res = ((res < 0) ? MR_COMPARE_LESS
        : (res == 0) ? MR_COMPARE_EQUAL
        : MR_COMPARE_GREATER);
").
:- pragma foreign_proc("C#",
    unsafe_compare_substrings(Res::uo, X::in, StartX::in, Y::in, StartY::in,
        Length::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    int res = System.String.CompareOrdinal(X, StartX, Y, StartY, Length);
    Res = ((res < 0) ? builtin.COMPARE_LESS
        : (res == 0) ? builtin.COMPARE_EQUAL
        : builtin.COMPARE_GREATER);
").

unsafe_compare_substrings(Res, X, StartX, Y, StartY, Length) :-
    unsafe_compare_substrings_loop(X, Y, StartX, StartY, Length, Res).

:- pred unsafe_compare_substrings_loop(string::in, string::in,
    int::in, int::in, int::in, comparison_result::uo) is det.

unsafe_compare_substrings_loop(X, Y, IX, IY, Rem, Res) :-
    ( if Rem = 0 then
        Res = (=)
    else
        unsafe_index_code_unit(X, IX, CodeX),
        unsafe_index_code_unit(Y, IY, CodeY),
        compare(Res0, CodeX, CodeY),
        (
            Res0 = (=),
            unsafe_compare_substrings_loop(X, Y, IX + 1, IY + 1, Rem - 1, Res)
        ;
            ( Res0 = (<)
            ; Res0 = (>)
            ),
            Res = Res0
        )
    ).

%---------------------%

% XXX ILSEQ unsafe_index_next effectively truncates either or both strings
% at the first ill-formed sequence.

compare_ignore_case_ascii(Res, X, Y) :-
    compare_ignore_case_ascii_loop(X, Y, 0, Res).

:- pred compare_ignore_case_ascii_loop(string::in, string::in, int::in,
    comparison_result::uo) is det.

compare_ignore_case_ascii_loop(X, Y, I, Res) :-
    ( if unsafe_index_next(X, I, IX, CharX) then
        ( if unsafe_index_next(Y, I, _IY, CharY) then
            char.to_lower(CharX, LowerCharX),
            char.to_lower(CharY, LowerCharY),
            compare(CharRes, LowerCharX, LowerCharY),
            (
                CharRes = (=),
                % CharX = CharY, or both are in the ASCII range.
                % In either case, we must have IX = IY.
                compare_ignore_case_ascii_loop(X, Y, IX, Res)
            ;
                ( CharRes = (<)
                ; CharRes = (>)
                ),
                Res = CharRes
            )
        else
            % X longer than Y.
            Res = (>)
        )
    else if unsafe_index_next(Y, I, _IY, _CharY) then
        % X shorter than Y.
        Res = (<)
    else
        Res = (=)
    ).

%---------------------%

    % XXX ILSEQ unsafe_index_next effectively truncates the string at the first
    % ill-formed sequence.
    %
prefix_length(P, S) = Index :-
    prefix_length_loop(P, S, 0, Index).

:- pred prefix_length_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

prefix_length_loop(P, S, I, Index) :-
    ( if
        unsafe_index_next(S, I, J, Char),
        P(Char)
    then
        prefix_length_loop(P, S, J, Index)
    else
        Index = I
    ).

    % XXX ILSEQ unsafe_index_next effectively truncates the string at the first
    % ill-formed sequence.
    %
suffix_length(P, S) = End - Index :-
    End = length(S),
    suffix_length_loop(P, S, End, Index).

:- pred suffix_length_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

suffix_length_loop(P, S, I, Index) :-
    ( if
        unsafe_prev_index(S, I, J, Char),
        P(Char)
    then
        suffix_length_loop(P, S, J, Index)
    else
        Index = I
    ).

%---------------------%

% XXX ILSEQ Behaviour depends on target language.
%   - C: works at code unit level so ill-formed sequences are no problem
%   - Java: works
%   - C#: searching for low/high surrogate returns same index

sub_string_search(WholeString, Pattern, Index) :-
    sub_string_search_start(WholeString, Pattern, 0, Index).

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
    ( if
        % XXX This is inefficient -- there is no (in, in, in) = in is semidet
        % mode of between, so this ends up calling the (in, in, in) = out
        % mode and then doing the unification. This will create a lot of
        % unnecessary garbage.
        % XXX This will abort if either index is not at a code point boundary.
        between(String, I, I + SubLen) = SubString
    then
        Index = I
    else
        sub_string_search_start_loop(String, SubString, I + 1, Len, SubLen,
            Index)
    ).

%---------------------------------------------------------------------------%
%
% Appending strings.
%

append(S1, S2) = S3 :-
    append(S1, S2, S3).

:- pragma promise_equivalent_clauses(append/3).

append(S1::in, S2::in, S3::in) :-
    append_iii(S1, S2, S3).
append(S1::in, S2::uo, S3::in) :-
    append_ioi(S1, S2, S3).
append(S1::in, S2::in, S3::uo) :-
    append_iio(S1, S2, S3).
append(S1::out, S2::out, S3::in) :-
    nondet_append(S1, S2, S3).

:- pred append_iii(string::in, string::in, string::in) is semidet.

append_iii(S1, S2, S3) :-
    Len1 = length(S1),
    Len2 = length(S2),
    Len3 = length(S3),
    ( if Len3 = Len1 + Len2 then
        unsafe_compare_substrings((=), S1, 0, S3, 0, Len1),
        unsafe_compare_substrings((=), S2, 0, S3, Len1, Len2)
    else
        fail
    ).

:- pred append_ioi(string::in, string::uo, string::in) is semidet.

append_ioi(S1, S2, S3) :-
    Len1 = length(S1),
    Len3 = length(S3),
    ( if
        Len1 =< Len3,
        unsafe_compare_substrings((=), S1, 0, S3, 0, Len1)
    then
        unsafe_between(S3, Len1, Len3, S2)
    else
        fail
    ).

:- pred append_iio(string::in, string::in, string::uo) is det.

:- pragma foreign_proc("C",
    append_iio(S1::in, S2::in, S3::uo),
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
    append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    S3 = System.String.Concat(S1, S2);
}").
:- pragma foreign_proc("Java",
    append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S3 = S1.concat(S2);
").
:- pragma foreign_proc("Erlang",
    append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S3 = list_to_binary([S1, S2])
").

nondet_append(S1, S2, S3) :-
    Len3 = length(S3),
    nondet_append_2(0, Len3, S1, S2, S3).

:- pred nondet_append_2(int::in, int::in, string::out, string::out,
    string::in) is multi.

nondet_append_2(Start2, Len3, S1, S2, S3) :-
    (
        unsafe_between(S3, 0, Start2, S1),
        unsafe_between(S3, Start2, Len3, S2)
    ;
        unsafe_index_next(S3, Start2, NextStart2, _Char),
        nondet_append_2(NextStart2, Len3, S1, S2, S3)
    ).

S1 ++ S2 = append(S1, S2).

%---------------------%
%
% We implement append_list in foreign code as the Mercury version
% creates significant amounts of unnecessary garbage.
%

:- pragma foreign_proc("C",
    append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list = Strs;
    size_t  len;

    // Determine the total length of all strings.
    len = 0;
    while (!MR_list_is_empty(list)) {
        len += strlen((MR_String) MR_list_head(list));
        list = MR_list_tail(list);
    }

    // Allocate enough word aligned memory for the string.
    MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);

    // Copy the strings into the new memory.
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
    append_list(Strs::in) = (Str::uo),
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
    append_list(Strs::in) = (Str::uo),
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
    append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = list_to_binary(Strs)
").

append_list(Strs::in) = (Str::uo) :-
    (
        Strs = [X | Xs],
        Str = X ++ append_list(Xs)
    ;
        Strs = [],
        Str = ""
    ).

append_list(Lists, append_list(Lists)).

%---------------------%
%
% We implement join_list in foreign code as the Mercury version
% creates significant amounts of unnecessary garbage.
%

:- pragma foreign_proc("C",
    join_list(Sep::in, Strs::in) = (Str::uo),
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

    // Determine the total length of all strings.
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

    // Copy the strings into the new memory.
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
    join_list(Sep::in, Strs::in) = (Str::uo),
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
    join_list(Sep::in, Strs::in) = (Str::uo),
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

join_list(_, []) = "".
join_list(Sep, [H | T]) = H ++ join_list_loop(Sep, T).

:- func join_list_loop(string::in, list(string)::in) = (string::uo) is det.

join_list_loop(_, []) = "".
join_list_loop(Sep, [H | T]) = Sep ++ H ++ join_list_loop(Sep, T).

%---------------------------------------------------------------------------%
%
% Splitting up strings.
%

% XXX ILSEQ Behaviour depends on target language.
%  - C: fails if the string begins with ill-formed sequence
%  - Java/C#: succeeds if the string begins with an unpaired surrogate

:- pragma foreign_proc("C",
    first_char(Str::in, First::in, Rest::in),
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
    first_char(Str::in, First::in, Rest::in),
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
    first_char(Str::in, First::in, Rest::in),
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
    first_char(Str::in, First::in, Rest::in),
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
    first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    MR_Integer pos = 0;
    First = MR_utf8_get_next(Str, &pos);
    SUCCESS_INDICATOR = (First > 0 && strcmp(Str + pos, Rest) == 0);
").
:- pragma foreign_proc("C#",
    first_char(Str::in, First::uo, Rest::in),
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
    first_char(Str::in, First::uo, Rest::in),
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
    first_char(Str::in, First::uo, Rest::in),
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
    first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_Integer pos = 0;
    int c = MR_utf8_get_next(Str, &pos);
    if (c != First || First == '\\0') {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Str += pos;
        // We need to make a copy to ensure that the pointer is word-aligned.
        MR_allocate_aligned_string_msg(Rest, strlen(Str), MR_ALLOC_ID);
        strcpy(Rest, Str);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").
:- pragma foreign_proc("C#",
    first_char(Str::in, First::in, Rest::uo),
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
    first_char(Str::in, First::in, Rest::uo),
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
    first_char(Str::in, First::in, Rest::uo),
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
    first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_Integer pos = 0;
    First = MR_utf8_get_next(Str, &pos);
    if (First < 1) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Str += pos;
        // We need to make a copy to ensure that the pointer is word-aligned.
        MR_allocate_aligned_string_msg(Rest, strlen(Str), MR_ALLOC_ID);
        strcpy(Rest, Str);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").
:- pragma foreign_proc("C#",
    first_char(Str::in, First::uo, Rest::uo),
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
    first_char(Str::in, First::uo, Rest::uo),
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
    first_char(Str::in, First::uo, Rest::uo),
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
    first_char(Str::uo, First::in, Rest::in),
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
    first_char(Str::uo, First::in, Rest::in),
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
    first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    String FirstStr = new String(Character.toChars(First));
    Str = FirstStr.concat(Rest);
}").
:- pragma foreign_proc("Erlang",
    first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = unicode:characters_to_binary([First, Rest])
").

%---------------------%

split(Str, Index, Left, Right) :-
    ( if Index =< 0 then
        Left = "",
        Right = Str
    else
        Len = length(Str),
        ( if Index >= Len then
            Left = Str,
            Right = ""
        else
            unsafe_between(Str, 0, Index, Left),
            unsafe_between(Str, Index, Len, Right)
        )
    ).

split_by_codepoint(Str, Count, Left, Right) :-
    ( if codepoint_offset(Str, Count, Offset) then
        split(Str, Offset, Left, Right)
    else if Count =< 0 then
        Left = "",
        Right = Str
    else
        Left = Str,
        Right = ""
    ).

%---------------------%

left(S1, N) = S2 :-
    left(S1, N, S2).

left(String, Count, LeftString) :-
    between(String, 0, Count, LeftString).

left_by_codepoint(String, Count) = LeftString :-
    left_by_codepoint(String, Count, LeftString).

left_by_codepoint(String, Count, LeftString) :-
    split_by_codepoint(String, Count, LeftString, _RightString).

right(S1, N) = S2 :-
    right(S1, N, S2).

right(String, RightCount, RightString) :-
    length(String, Length),
    Start = Length - RightCount,
    between(String, Start, Length, RightString).

right_by_codepoint(String, RightCount) = RightString :-
    right_by_codepoint(String, RightCount, RightString).

right_by_codepoint(String, RightCount, RightString) :-
    count_codepoints(String, TotalCount),
    LeftCount = TotalCount - RightCount,
    split_by_codepoint(String, LeftCount, _LeftString, RightString).

%---------------------%

between(Str, Start, End) = SubString :-
    between(Str, Start, End, SubString).

between(Str, Start, End, SubStr) :-
    Len = length(Str),
    ( if Start =< 0 then
        ClampStart = 0
    else if Start >= Len then
        ClampStart = Len
    else
        ClampStart = Start
    ),
    ( if End =< ClampStart then
        ClampEnd = ClampStart
    else if End >= Len then
        ClampEnd = Len
    else
        ClampEnd = End
    ),
    unsafe_between(Str, ClampStart, ClampEnd, SubStr).

%---------------------%

substring(Str, Start, Count) = SubString :-
    substring(Str, Start, Count, SubString).

substring(Str, Start, Count, SubString) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    between(Str, ClampStart, ClampEnd, SubString).

:- pred convert_endpoints(int::in, int::in, int::out, int::out) is det.

convert_endpoints(Start, Count, ClampStart, ClampEnd) :-
    ClampStart = int.max(0, Start),
    ( if Count =< 0 then
        ClampEnd = ClampStart
    else
        % Check for overflow.
        ( if ClampStart > max_int - Count then
            ClampEnd = max_int
        else
            ClampEnd = ClampStart + Count
        )
    ).

%---------------------%

between_codepoints(Str, Start, End) = SubString :-
    between_codepoints(Str, Start, End, SubString).

between_codepoints(Str, Start, End, SubString) :-
    ( if Start < 0 then
        StartOffset = 0
    else if codepoint_offset(Str, Start, StartOffset0) then
        StartOffset = StartOffset0
    else
        StartOffset = length(Str)
    ),
    ( if End < 0 then
        EndOffset = 0
    else if codepoint_offset(Str, End, EndOffset0) then
        EndOffset = EndOffset0
    else
        EndOffset = length(Str)
    ),
    % between/4 will enforce StartOffset =< EndOffset.
    between(Str, StartOffset, EndOffset, SubString).

%---------------------%

unsafe_between(Str, Start, End) = SubString :-
    unsafe_between(Str, Start, End, SubString).

:- pragma foreign_proc("C",
    unsafe_between(Str::in, Start::in, End::in, SubString::uo),
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
    unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    SubString = Str.Substring(Start, End - Start);
}").
:- pragma foreign_proc("Java",
    unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SubString = Str.substring(Start, End);
").
:- pragma foreign_proc("Erlang",
    unsafe_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Count = End - Start,
    << _:Start/binary, SubString:Count/binary, _/binary >> = Str
").

unsafe_substring(Str, Start, Count) = SubString :-
    unsafe_between(Str, Start, Start + Count) = SubString.

unsafe_substring(Str, Start, Count, SubString) :-
    unsafe_between(Str, Start, Start + Count, SubString).

%---------------------%

% XXX ILSEQ unsafe_index_next causes truncation at first ill-formed sequence.

words_separator(SepP, String) = Words :-
    skip_to_next_word_start(SepP, String, 0, WordStart),
    words_loop(SepP, String, WordStart, Words).

words(String) = words_separator(char.is_whitespace, String).

:- pred words_loop(pred(char)::in(pred(in) is semidet), string::in, int::in,
    list(string)::out) is det.

words_loop(SepP, String, WordStartPos, Words) :-
    skip_to_word_end(SepP, String, WordStartPos, PastWordEndPos),
    ( if PastWordEndPos = WordStartPos then
        Words = []
    else
        unsafe_between(String, WordStartPos, PastWordEndPos, HeadWord),
        skip_to_next_word_start(SepP, String, PastWordEndPos,
            NextWordStartPos),
        ( if PastWordEndPos = NextWordStartPos then
            Words = [HeadWord]
        else
            words_loop(SepP, String, NextWordStartPos, TailWords),
            Words = [HeadWord | TailWords]
        )
    ).

    % Return the smallest NextWordStartPos >= CurPos such that
    % `not SepP(String[NextWordStartPos])'.
    %
:- pred skip_to_next_word_start(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

skip_to_next_word_start(SepP, String, CurPos, NextWordStartPos) :-
    ( if
        unsafe_index_next(String, CurPos, NextPos, Char),
        SepP(Char)
    then
        skip_to_next_word_start(SepP, String, NextPos, NextWordStartPos)
    else
        NextWordStartPos = CurPos
    ).

    % Return the smallest NextWordStartPos >= CurPos such that
    % `SepP(String[NextWordStartPos])'.
    %
:- pred skip_to_word_end(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

skip_to_word_end(SepP, String, CurPos, PastWordEndPos) :-
    ( if
        unsafe_index_next(String, CurPos, NextPos, Char)
    then
        ( if SepP(Char) then
            PastWordEndPos = CurPos
        else
            skip_to_word_end(SepP, String, NextPos, PastWordEndPos)
        )
    else
        PastWordEndPos = CurPos
    ).

%---------------------%

split_at_separator(DelimP, Str) = Segments :-
    Len = length(Str),
    split_at_separator_loop(DelimP, Str, Len, Len, [], Segments).

:- pred split_at_separator_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::in, list(string)::in, list(string)::out) is det.

split_at_separator_loop(DelimP, Str, CurPos, PastSegEnd, !Segments) :-
    % We walk Str backwards, extending the accumulated list of segments
    % as we find code points matching DelimP.
    %
    % Invariant: 0 =< CurPos =< length(Str).
    % PastSegEnd is one past the last index of the current segment.
    %
    % XXX ILSEQ unsafe_prev_index fails at an ill-formed sequence.
    % Ideally code units in an ill-form sequence are skipped over
    % since they cannot be delimiters.
    %
    ( if unsafe_prev_index(Str, CurPos, PrevPos, Char) then
        ( if DelimP(Char) then
            % Chop here.
            SegStart = CurPos,
            Segment = unsafe_between(Str, SegStart, PastSegEnd),
            !:Segments = [Segment | !.Segments],
            split_at_separator_loop(DelimP, Str, PrevPos, PrevPos, !Segments)
        else
            % Extend current segment.
            split_at_separator_loop(DelimP, Str, PrevPos, PastSegEnd,
                !Segments)
        )
    else
        % We have reached the beginning of the string.
        Segment = unsafe_between(Str, 0, PastSegEnd),
        !:Segments = [Segment | !.Segments]
    ).

%---------------------%

split_at_char(C, String) =
    split_at_separator(unify(C), String).

split_at_string(Needle, Total) =
    split_at_string_loop(0, length(Needle), Needle, Total).

:- func split_at_string_loop(int, int, string, string) = list(string).

split_at_string_loop(StartAt, NeedleLen, Needle, Total) = Out :-
    % XXX ILSEQ Behaviour of sub_string_search_start currently differs
    % across targets.
    ( if sub_string_search_start(Total, Needle, StartAt, NeedlePos) then
        BeforeNeedle = between(Total, StartAt, NeedlePos),
        Tail = split_at_string_loop(NeedlePos+NeedleLen, NeedleLen,
            Needle, Total),
        Out = [BeforeNeedle | Tail]
    else
        split(Total, StartAt, _Skip, Last),
        Out = [Last]
    ).

%---------------------------------------------------------------------------%
%
% Dealing with prefixes and suffixes.
%

:- pragma promise_equivalent_clauses(prefix/2).

prefix(String::in, Prefix::in) :-
    Len    = length(String),
    PreLen = length(Prefix),
    PreLen =< Len,
    prefix_2_iii(String, Prefix, PreLen - 1).
prefix(String::in, Prefix::out) :-
    prefix_2_ioi(String, Prefix, 0).

:- pred prefix_2_iii(string::in, string::in, int::in) is semidet.

prefix_2_iii(String, Prefix, I) :-
    ( if 0 =< I then
        unsafe_index_code_unit(String, I, C),
        unsafe_index_code_unit(Prefix, I, C),
        prefix_2_iii(String, Prefix, I - 1)
    else
        true
    ).

:- pred prefix_2_ioi(string::in, string::out, int::in) is multi.

prefix_2_ioi(String, Prefix, Cur) :-
    (
        Prefix = unsafe_between(String, 0, Cur)
    ;
        % XXX ILSEQ unsafe_index_next stops at ill-formed sequence
        unsafe_index_next(String, Cur, Next, _),
        prefix_2_ioi(String, Prefix, Next)
    ).

:- pragma promise_equivalent_clauses(suffix/2).

suffix(String::in, Suffix::in) :-
    Len    = length(String),
    PreLen = length(Suffix),
    PreLen =< Len,
    suffix_2_iiii(String, Suffix, 0, Len - PreLen, PreLen).
suffix(String::in, Suffix::out) :-
    Len = length(String),
    suffix_2_ioii(String, Suffix, Len, Len).

:- pred suffix_2_iiii(string::in, string::in, int::in, int::in, int::in)
    is semidet.

suffix_2_iiii(String, Suffix, I, Offset, Len) :-
    ( if I < Len then
        unsafe_index_code_unit(String, I + Offset, C),
        unsafe_index_code_unit(Suffix, I, C),
        suffix_2_iiii(String, Suffix, I + 1, Offset, Len)
    else
        true
    ).

:- pred suffix_2_ioii(string::in, string::out, int::in, int::in) is multi.

suffix_2_ioii(String, Suffix, Cur, Len) :-
    (
        unsafe_between(String, Cur, Len, Suffix)
    ;
        % XXX ILSEQ unsafe_prev_index stops at ill-formed sequence
        unsafe_prev_index(String, Cur, Prev, _),
        suffix_2_ioii(String, Suffix, Prev, Len)
    ).

%---------------------%

remove_prefix(Prefix, String, Suffix) :-
    append(Prefix, Suffix, String).

det_remove_prefix(Prefix, String, Suffix) :-
    ( if remove_prefix(Prefix, String, SuffixPrime) then
        Suffix = SuffixPrime
    else
        unexpected($pred, "string does not have the given prefix")
    ).

remove_prefix_if_present(Prefix, String) = Out :-
    ( if remove_prefix(Prefix, String, Suffix) then
        Out = Suffix
    else
        Out = String
    ).

remove_suffix(String, Suffix, Prefix) :-
    suffix(String, Suffix),
    left(String, length(String) - length(Suffix), Prefix).

det_remove_suffix(String, Suffix) = Prefix :-
    ( if remove_suffix(String, Suffix, PrefixPrime) then
        Prefix = PrefixPrime
    else
        unexpected($pred, "string does not have given suffix")
    ).

remove_suffix_if_present(Suffix, String) = Out :-
    LeftCount = length(String) - length(Suffix),
    split(String, LeftCount, LeftString, RightString),
    ( if RightString = Suffix then
        Out = LeftString
    else
        Out = String
    ).

%---------------------------------------------------------------------------%
%
% Transformations of strings.
%

capitalize_first(S1) = S2 :-
    capitalize_first(S1, S2).

capitalize_first(S0, S) :-
    ( if first_char(S0, C, S1) then
        char.to_upper(C, UpperC),
        first_char(S, UpperC, S1)
    else
        S = S0
    ).

uncapitalize_first(S1) = S2 :-
    uncapitalize_first(S1, S2).

uncapitalize_first(S0, S) :-
    ( if first_char(S0, C, S1) then
        char.to_lower(C, LowerC),
        first_char(S, LowerC, S1)
    else
        S = S0
    ).

%---------------------%

to_upper(S1) = S2 :-
    to_upper(S1, S2).

:- pragma promise_equivalent_clauses(to_upper/2).

to_upper(StrIn::in, StrOut::uo) :-
    % XXX ILSEQ to_char_list and from_char_list cannot handle ill-formed
    % sequences.
    to_char_list(StrIn, List),
    char_list_to_upper(List, ListUpp),
    from_char_list(ListUpp, StrOut).

to_upper(X::in, Y::in) :-
    length(X, LenX),
    length(Y, LenY),
    ( if LenX = LenY then
        check_upper_loop(X, Y, 0, LenX)
    else
        fail
    ).

:- pragma foreign_proc("C",
    to_upper(StrIn::in, StrOut::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    MR_Integer  i;

    MR_make_aligned_string_copy_msg(StrOut, StrIn, MR_ALLOC_ID);

    for (i = 0; StrOut[i] != '\\0'; i++) {
        if (StrOut[i] >= 'a' && StrOut[i] <= 'z') {
            StrOut[i] = StrOut[i] - 'a' + 'A';
        }
    }
").
:- pragma foreign_proc("C#",
    to_upper(StrIn::in, StrOut::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char[] cs = StrIn.ToCharArray();
    for (int i = 0; i < cs.Length; i++) {
        if (cs[i] >= 'a' && cs[i] <= 'z') {
            cs[i] = (char)(cs[i] - 'a' + 'A');
        }
    }
    StrOut = new System.String(cs);
").
:- pragma foreign_proc("Java",
    to_upper(StrIn::in, StrOut::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char[] cs = StrIn.toCharArray();
    for (int i = 0; i < cs.length; i++) {
        if (cs[i] >= 'a' && cs[i] <= 'z') {
            cs[i] = (char)(cs[i] - 'a' + 'A');
        }
    }
    StrOut = new String(cs);
").

:- pred char_list_to_upper(list(char)::in, list(char)::out) is det.

char_list_to_upper([], []).
char_list_to_upper([X | Xs], [Y | Ys]) :-
    char.to_upper(X, Y),
    char_list_to_upper(Xs, Ys).

:- pred check_upper_loop(string::in, string::in, int::in, int::in) is semidet.

check_upper_loop(X, Y, Index, End) :-
    ( if Index = End then
        true
    else
        unsafe_index_code_unit(X, Index, CodeX),
        unsafe_index_code_unit(Y, Index, CodeY),
        to_upper_code_unit(CodeX, CodeY),
        check_upper_loop(X, Y, Index + 1, End)
    ).

:- pred to_upper_code_unit(int::in, int::out) is det.

to_upper_code_unit(Code0, Code) :-
    ( if
        Code0 >= to_int('a'),
        Code0 =< to_int('z')
    then
        Code = Code0 - to_int('a') + to_int('A')
    else
        Code = Code0
    ).

%---------------------%

to_lower(S1) = S2 :-
    to_lower(S1, S2).

:- pragma promise_equivalent_clauses(to_lower/2).

to_lower(StrIn::in, StrOut::uo) :-
    % XXX ILSEQ to_char_list and from_char_list cannot handle ill-formed
    % sequences.
    to_char_list(StrIn, List),
    char_list_to_lower(List, ListLow),
    from_char_list(ListLow, StrOut).

to_lower(X::in, Y::in) :-
    length(X, LenX),
    length(Y, LenY),
    ( if LenX = LenY then
        check_lower_loop(X, Y, 0, LenX)
    else
        fail
    ).

:- pragma foreign_proc("C",
    to_lower(StrIn::in, StrOut::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    MR_Integer  i;

    MR_make_aligned_string_copy_msg(StrOut, StrIn, MR_ALLOC_ID);

    for (i = 0; StrOut[i] != '\\0'; i++) {
        if (StrOut[i] >= 'A' && StrOut[i] <= 'Z') {
            StrOut[i] = StrOut[i] - 'A' + 'a';
        }
    }
").
:- pragma foreign_proc("C#",
    to_lower(StrIn::in, StrOut::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char[] cs = StrIn.ToCharArray();
    for (int i = 0; i < cs.Length; i++) {
        if (cs[i] >= 'A' && cs[i] <= 'Z') {
            cs[i] = (char)(cs[i] - 'A' + 'a');
        }
    }
    StrOut = new System.String(cs);
").
:- pragma foreign_proc("Java",
    to_lower(StrIn::in, StrOut::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char[] cs = StrIn.toCharArray();
    for (int i = 0; i < cs.length; i++) {
        if (cs[i] >= 'A' && cs[i] <= 'Z') {
            cs[i] = (char)(cs[i] - 'A' + 'a');
        }
    }
    StrOut = new String(cs);
").

:- pred char_list_to_lower(list(char)::in, list(char)::out) is det.

char_list_to_lower([], []).
char_list_to_lower([X | Xs], [Y | Ys]) :-
    char.to_lower(X, Y),
    char_list_to_lower(Xs, Ys).

:- pred check_lower_loop(string::in, string::in, int::in, int::in) is semidet.

check_lower_loop(X, Y, Index, End) :-
    ( if Index = End then
        true
    else
        unsafe_index_code_unit(X, Index, CodeX),
        unsafe_index_code_unit(Y, Index, CodeY),
        to_lower_code_unit(CodeX, CodeY),
        check_lower_loop(X, Y, Index + 1, End)
    ).

:- pred to_lower_code_unit(int::in, int::out) is det.

to_lower_code_unit(Code0, Code) :-
    ( if
        Code0 >= to_int('A'),
        Code0 =< to_int('Z')
    then
        Code = Code0 - to_int('A') + to_int('a')
    else
        Code = Code0
    ).

%---------------------%

pad_left(S1, C, N) = S2 :-
    pad_left(S1, C, N, S2).

pad_left(String0, PadChar, Width, String) :-
    count_codepoints(String0, Length),
    ( if Length < Width then
        Count = Width - Length,
        duplicate_char(PadChar, Count, PadString),
        append(PadString, String0, String)
    else
        String = String0
    ).

pad_right(S1, C, N) = S2 :-
    pad_right(S1, C, N, S2).

pad_right(String0, PadChar, Width, String) :-
    count_codepoints(String0, Length),
    ( if Length < Width then
        Count = Width - Length,
        duplicate_char(PadChar, Count, PadString),
        append(String0, PadString, String)
    else
        String = String0
    ).

chomp(S) = Chomp :-
    ( if prev_index(S, length(S), Offset, '\n') then
        Chomp = left(S, Offset)
    else
        Chomp = S
    ).

% XXX ILSEQ Once the problems with prefix_length, suffix_length are fixed,
% there should be no problem stripping strings containing ill-formed sequences.

strip(S0) = S :-
    L = prefix_length(char.is_whitespace, S0),
    R = suffix_length(char.is_whitespace, S0),
    S = between(S0, L, length(S0) - R).

lstrip(S) = lstrip_pred(char.is_whitespace, S).

rstrip(S) = rstrip_pred(char.is_whitespace, S).

lstrip_pred(P, S) = right(S, length(S) - prefix_length(P, S)).

rstrip_pred(P, S) = left(S, length(S) - suffix_length(P, S)).

%---------------------%

replace(Str, Pat, Subst, Result) :-
    sub_string_search(Str, Pat, Index),

    Initial = unsafe_between(Str, 0, Index),

    BeginAt = Index + length(Pat),
    EndAt = length(Str),
    Final = unsafe_between(Str, BeginAt, EndAt),

    Result = append_list([Initial, Subst, Final]).

replace_all(S1, S2, S3) = S4 :-
    replace_all(S1, S2, S3, S4).

replace_all(Str, Pat, Subst, Result) :-
    ( if Pat = "" then
        % XXX ILSEQ foldl cannot handle ill-formed sequences.
        F = (func(C, L) = [char_to_string(C) ++ Subst | L]),
        Foldl = foldl(F, Str, []),
        Result = append_list([Subst | list.reverse(Foldl)])
    else
        PatLength = length(Pat),
        replace_all_loop(Str, Pat, Subst, PatLength, 0, [], ReversedChunks),
        Chunks = list.reverse(ReversedChunks),
        Result = append_list(Chunks)
    ).

:- pred replace_all_loop(string::in, string::in, string::in,
    int::in, int::in, list(string)::in, list(string)::out) is det.

replace_all_loop(Str, Pat, Subst, PatLength, BeginAt,
        RevChunks0, RevChunks) :-
    ( if sub_string_search_start(Str, Pat, BeginAt, Index) then
        Initial = unsafe_between(Str, BeginAt, Index),
        Start = Index + PatLength,
        replace_all_loop(Str, Pat, Subst, PatLength, Start,
            [Subst, Initial | RevChunks0], RevChunks)
    else
        EndString = unsafe_between(Str, BeginAt, length(Str)),
        RevChunks = [EndString | RevChunks0]
    ).

%---------------------%

word_wrap(Str, N) = word_wrap_separator(Str, N, "").

word_wrap_separator(Str, N, WordSep0) = Wrapped :-
    Words = words_separator(char.is_whitespace, Str),
    SepLen0 = count_codepoints(WordSep0),
    ( if SepLen0 < N then
        WordSep = WordSep0,
        SepLen = SepLen0
    else
        WordSep = "",
        SepLen = 0
    ),
    CurCol = 1,
    MaxCol = N,
    RevWordsSpacesNls0 = [],
    word_wrap_loop(Words, WordSep, SepLen, CurCol, MaxCol,
        RevWordsSpacesNls0, RevWordsSpacesNls),
    list.reverse(RevWordsSpacesNls, WordsSpacesNls),
    Wrapped = append_list(WordsSpacesNls).

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
    WordLen = count_codepoints(Word),
    ( if
        % We are on the first column and the length of the word
        % is less than the line length.
        CurCol = 1,
        WordLen < MaxCol
    then
        NewWords = Words,
        NewCol = CurCol + WordLen,
        !:RevWordsSpacesNls = [Word | !.RevWordsSpacesNls]
    else if
        % The word takes up the whole line.
        CurCol = 1,
        WordLen = MaxCol
    then
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
    else if
        % If we add a space and the current word to the line,
        % we will still be within the line length limit.
        CurCol + WordLen < MaxCol
    then
        NewWords = Words,
        NewCol = CurCol + WordLen + 1,
        !:RevWordsSpacesNls = [Word, " " | !.RevWordsSpacesNls]
    else if
        % Adding the word and a space takes us to the end of the line exactly.
        CurCol + WordLen = MaxCol
    then
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
    else
        % Adding the word would take us over the line limit.
        ( if CurCol = 1 then
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
        else
            NewWords = [Word | Words],
            NewCol = 1,
            !:RevWordsSpacesNls = ["\n" | !.RevWordsSpacesNls]
        )
    ),
    word_wrap_loop(NewWords, WordSep, SepLen, NewCol, MaxCol,
        !RevWordsSpacesNls).

:- func break_up_string_reverse(string, int, list(string)) = list(string).

break_up_string_reverse(Str, N, Prev) = Strs :-
    ( if count_codepoints(Str) =< N then
        Strs = [Str | Prev]
    else
        split_by_codepoint(Str, N, Left, Right),
        Strs = break_up_string_reverse(Right, N, [Left | Prev])
    ).

%---------------------------------------------------------------------------%
%
% Folds over the characters in strings.
%

foldl(F, S, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    foldl(P, S, A, B).

foldl(Closure, String, !Acc) :-
    length(String, Length),
    foldl_between(Closure, String, 0, Length, !Acc).

foldl2(Closure, String, !Acc1, !Acc2) :-
    length(String, Length),
    foldl2_between(Closure, String, 0, Length, !Acc1, !Acc2).

foldl_between(F, S, Start, End, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    foldl_between(P, S, Start, End, A, B).

foldl_between(Closure, String, Start0, End0, !Acc) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    foldl_between_2(Closure, String, Start, End, !Acc).

foldl2_between(Closure, String, Start0, End0, !Acc1, !Acc2) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    foldl2_between_2(Closure, String, Start, End, !Acc1, !Acc2).

:- pred foldl_between_2(pred(char, A, A), string, int, int, A, A).
:- mode foldl_between_2(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode foldl_between_2(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode foldl_between_2(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode foldl_between_2(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode foldl_between_2(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

foldl_between_2(Closure, String, I, End, !Acc) :-
    ( if
        I < End,
        unsafe_index_next(String, I, J, Char),
        J =< End
    then
        Closure(Char, !Acc),
        foldl_between_2(Closure, String, J, End, !Acc)
    else
        true
    ).

:- pred foldl2_between_2(pred(char, A, A, B, B), string, int, int,
    A, A, B, B).
:- mode foldl2_between_2(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode foldl2_between_2(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode foldl2_between_2(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode foldl2_between_2(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode foldl2_between_2(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode foldl2_between_2(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

foldl2_between_2(Closure, String, I, End, !Acc1, !Acc2) :-
    ( if
        I < End,
        unsafe_index_next(String, I, J, Char),
        J =< End
    then
        Closure(Char, !Acc1, !Acc2),
        foldl2_between_2(Closure, String, J, End, !Acc1, !Acc2)
    else
        true
    ).

foldl_substring(F, String, Start, Count, Acc0) = Acc :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    Acc = foldl_between(F, String, ClampStart, ClampEnd, Acc0).

foldl_substring(Closure, String, Start, Count, !Acc) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    foldl_between(Closure, String, ClampStart, ClampEnd, !Acc).

foldl2_substring(Closure, String, Start, Count, !Acc1, !Acc2) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    foldl2_between(Closure, String, ClampStart, ClampEnd,
        !Acc1, !Acc2).

%---------------------%

foldr(F, String, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y)),
    foldr(Closure, String, Acc0, Acc).

foldr(Closure, String, !Acc) :-
    foldr_between(Closure, String, 0, length(String), !Acc).

foldr_between(F, String, Start, Count, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    foldr_between(Closure, String, Start, Count, Acc0, Acc).

foldr_between(Closure, String, Start0, End0, !Acc) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    foldr_between_2(Closure, String, Start, End, !Acc).

:- pred foldr_between_2(pred(char, T, T), string, int, int, T, T).
:- mode foldr_between_2(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode foldr_between_2(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode foldr_between_2(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode foldr_between_2(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode foldr_between_2(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

foldr_between_2(Closure, String, Start, I, !Acc) :-
    ( if
        I > Start,
        unsafe_prev_index(String, I, J, Char),
        J >= Start
    then
        Closure(Char, !Acc),
        foldr_between_2(Closure, String, Start, J, !Acc)
    else
        true
    ).

foldr_substring(F, String, Start, Count, Acc0) = Acc :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    Acc = foldr_between(F, String, ClampStart, ClampEnd, Acc0).

foldr_substring(Closure, String, Start, Count, !Acc) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    foldr_between(Closure, String, ClampStart, ClampEnd, !Acc).

%---------------------------------------------------------------------------%
%
% Formatting tables.
%
% Currently, format_table simply assumes each code point occupies
% a single column in a fixed-width output device. Thus the output will
% only be aligned if limited to an (important) subset of characters,
% namely ASCII and European characters (excluding combining characters).
% It would be relatively easy to support CJK double-width characters
% and zero-width characters (see wcswidth), which would be enough
% to cover the needs of very many people.
%
% These considerations may also apply to predicates such as pad_left,
% pad_right, format (with field widths), word_wrap, etc.
%

format_table(Columns, Separator) = Table :-
    MaxWidths = list.map(find_max_length, Columns),
    % Maybe the code below should be replaced by the code of format_table_max,
    % with all maybe widths set to "no". They do the same job; the code of
    % format_table_max just does it more directly, without the excessive use
    % of higher order calls.
    PaddedColumns = list.map_corresponding(pad_column, MaxWidths, Columns),
    (
        PaddedColumns = [PaddedHead | PaddedTail],
        Rows = list.foldl(list.map_corresponding(
            join_rev_columns(Separator)), PaddedTail, PaddedHead)
    ;
        PaddedColumns = [],
        Rows = []
    ),
    Table = join_list("\n", Rows).

format_table_max(ColumnsLimits, Separator) = Table :-
    MaxWidthsSenses = list.map(find_max_length_with_limit, ColumnsLimits),
    Columns = list.map(project_column_strings, ColumnsLimits),
    SepLen = count_codepoints(Separator),
    generate_rows(MaxWidthsSenses, Separator, SepLen, Columns, [], RevRows),
    list.reverse(RevRows, Rows),
    Table = join_list("\n", Rows).

:- func project_column_strings(pair(justified_column, maybe(int)))
    = list(string).

project_column_strings(left(Strings) - _) = Strings.
project_column_strings(right(Strings) - _) = Strings.

:- pred generate_rows(assoc_list(justify_sense, int)::in, string::in, int::in,
    list(list(string))::in, list(string)::in, list(string)::out) is det.

generate_rows(MaxWidthsSenses, Separator, SepLen, Columns0, !RevRows) :-
    ( if all_empty(Columns0) then
        true
    else
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
    ( if count_codepoints(ColumnStr0) =< MaxWidth then
        (
            Justify = just_left,
            ColumnStr = pad_right(ColumnStr0, ' ', MaxWidth)
        ;
            Justify = just_right,
            ColumnStr = pad_left(ColumnStr0, ' ', MaxWidth)
        ),
        (
            JustifyWidths = [],
            Line = ColumnStr
        ;
            JustifyWidths = [_ | _],
            Line = ColumnStr ++ Separator ++ LineRest
        )
    else
        (
            JustifyWidths = [],
            Line = ColumnStr0
        ;
            JustifyWidths = [_ | _],
            Line = ColumnStr0 ++ Separator ++ "\n" ++
                duplicate_char(' ', NextColumn) ++ LineRest
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
        ( if MaxLength0 > Limit then
            MaxLength = Limit
        else
            MaxLength = MaxLength0
        )
    ;
        MaybeLimit = no,
        MaxLength = MaxLength0
    ).

:- func pad_column(int, justified_column) = list(string).

pad_column(Width, left(Strings)) =
    list.map(rpad(' ', Width), Strings).
pad_column(Width, right(Strings)) =
    list.map(lpad(' ', Width), Strings).

:- func rpad(char, int, string) = string.

rpad(Chr, N, Str) = pad_right(Str, Chr, N).

:- func lpad(char, int, string) = string.

lpad(Chr, N, Str) = pad_left(Str, Chr, N).

:- pred max_str_length(string::in, int::in, int::out) is det.

max_str_length(Str, PrevMaxLen, MaxLen) :-
    Length = count_codepoints(Str),
    ( if Length > PrevMaxLen then
        MaxLen = Length
    else
        MaxLen = PrevMaxLen
    ).

%---------------------------------------------------------------------------%
%
% Converting strings to docs.
%

string_to_doc(S) = docs([str(term_io.quoted_string(S))]).

%---------------------------------------------------------------------------%
%
% Converting strings to values of builtin types.
%

% XXX ILSEQ Behaviour on ill-formed sequences depends on foldl_between.
% Strings containing ill-formed sequences should fail to convert.

to_int(String, Int) :-
    base_string_to_int(10, String, Int).

det_to_int(S) = det_base_string_to_int(10, S).

base_string_to_int(Base, String, Int) :-
    index(String, 0, Char),
    End = count_code_units(String),
    ( if Char = ('-') then
        End > 1,
        foldl_between(base_negative_accumulator(Base), String, 1, End, 0, Int)
    else if Char = ('+') then
        End > 1,
        foldl_between(base_accumulator(Base), String, 1, End, 0, Int)
    else
        foldl_between(base_accumulator(Base), String, 0, End, 0, Int)
    ).

det_base_string_to_int(Base, S) = N :-
    ( if base_string_to_int(Base, S, N0) then
        N = N0
    else
        unexpected($pred, "conversion failed")
    ).

:- func base_accumulator(int) = pred(char, int, int).
:- mode base_accumulator(in) = out(pred(in, in, out) is semidet) is det.

base_accumulator(Base) = Pred :-
    % Avoid allocating a closure for the common bases. A more general, but
    % finicky, way to avoid the allocation is to inline foldl_between so that
    % the higher-order calls in base_string_to_int can be specialised.
    % The redundant closures will also need to be deleted by unused argument
    % elimination.
    ( if Base = 10 then
        Pred = accumulate_int(10)
    else if Base = 16 then
        Pred = accumulate_int(16)
    else if Base = 8 then
        Pred = accumulate_int(8)
    else if Base = 2 then
        Pred = accumulate_int(2)
    else
        Pred = accumulate_int(Base)
    ).

:- pred accumulate_int(int::in, char::in, int::in, int::out) is semidet.

accumulate_int(Base, Char, N0, N) :-
    char.base_digit_to_int(Base, Char, M),
    N = (Base * N0) + M,
    % Fail on overflow.
    % XXX depends on undefined behaviour
    N0 =< N.

:- func base_negative_accumulator(int) = pred(char, int, int).
:- mode base_negative_accumulator(in) = out(pred(in, in, out) is semidet)
    is det.

base_negative_accumulator(Base) = Pred :-
    % Avoid allocating a closure for the common bases.
    ( if Base = 10 then
        Pred = accumulate_negative_int(10)
    else if Base = 16 then
        Pred = accumulate_negative_int(16)
    else if Base = 8 then
        Pred = accumulate_negative_int(8)
    else if Base = 2 then
        Pred = accumulate_negative_int(2)
    else
        Pred = accumulate_negative_int(Base)
    ).

:- pred accumulate_negative_int(int::in, char::in,
    int::in, int::out) is semidet.

accumulate_negative_int(Base, Char, N0, N) :-
    char.base_digit_to_int(Base, Char, M),
    N = (Base * N0) - M,
    % Fail on overflow.
    % XXX depends on undefined behaviour
    N =< N0.

%---------------------%

:- pragma foreign_export("C", to_float(in, out),
    "ML_string_to_float").

:- pragma foreign_proc("C",
    to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    // The %c checks for any erroneous characters appearing after the float;
    // if there are then sscanf() will return 2 rather than 1.
    char    tmpc;
    SUCCESS_INDICATOR =
        (!MR_isspace(FloatString[0])) &&
        (sscanf(FloatString, MR_FLT_FMT ""%c"", &FloatVal, &tmpc) == 1);
        // MR_TRUE if sscanf succeeds, MR_FALSE otherwise.
}").
:- pragma foreign_proc("C#",
    to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    FloatVal = 0.0;     // FloatVal must be initialized to suppress
                        // error messages when the predicate fails.

    // Leading or trailing whitespace is not allowed.
    if (FloatString.Length == 0 ||
        System.Char.IsWhiteSpace(FloatString, 0) ||
        System.Char.IsWhiteSpace(FloatString, FloatString.Length - 1))
    {
        SUCCESS_INDICATOR = false;
    } else {
        try {
            FloatVal = System.Convert.ToDouble(FloatString);
            SUCCESS_INDICATOR = true;
        } catch (System.FormatException) {
            SUCCESS_INDICATOR = false;
        } catch (System.OverflowException) {
            if (FloatString[0] == '-') {
                FloatVal = System.Double.NegativeInfinity;
            } else {
                FloatVal = System.Double.PositiveInfinity;
            }
            SUCCESS_INDICATOR = true;
        }
    }
}").
:- pragma foreign_proc("Java",
    to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = 0.0;     // FloatVal must be initialized to suppress
                        // error messages when the predicate fails.

    // Leading or trailing whitespace is not allowed.
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
    to_float(FloatString::in, FloatVal::out),
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

det_to_float(FloatString) = Float :-
    ( if to_float(FloatString, FloatPrime) then
        Float = FloatPrime
    else
        unexpected($pred, "conversion failed.")
    ).

%---------------------------------------------------------------------------%
%
% Converting values of builtin types to strings.
%

char_to_string(C) = S1 :-
    char_to_string(C, S1).

char_to_string(Char, String) :-
    % XXX ILSEQ Should fail when String is not a well-formed encoding of a
    % single code point.
    to_char_list(String, [Char]).

from_char(Char) = char_to_string(Char).

%---------------------%

int_to_string(N) = S1 :-
    int_to_string(N, S1).

int_to_string(N, Str) :-
    int_to_base_string(N, 10, Str).

from_int(N) = int_to_string(N).

int_to_base_string(N1, N2) = S2 :-
    int_to_base_string(N1, N2, S2).

int_to_base_string(N, Base, Str) :-
    ( if 2 =< Base, Base =< 36 then
        true
    else
        unexpected($pred, "invalid base")
    ),
    int_to_base_string_1(N, Base, Str).

:- pred int_to_base_string_1(int::in, int::in, string::uo) is det.

int_to_base_string_1(N, Base, Str) :-
    % Note that in order to handle MININT correctly, we need to do the
    % conversion of the absolute number into digits using negative numbers;
    % we can't use positive numbers, since -MININT overflows.
    ( if N < 0 then
        int_to_base_string_2(N, Base, ['-'], RevChars)
    else
        NegN = 0 - N,
        int_to_base_string_2(NegN, Base, [], RevChars)
    ),
    from_rev_char_list(RevChars, Str).

:- pred int_to_base_string_2(int::in, int::in,
    list(char)::in, list(char)::out) is det.

int_to_base_string_2(NegN, Base, !RevChars) :-
    % int_to_base_string_2/3 is almost identical to
    % int_to_base_string_group_2/6 below so any changes here might
    % also need to be applied to int_to_base_string_group_2/3.
    ( if NegN > -Base then
        N = -NegN,
        DigitChar = char.det_base_int_to_digit(Base, N),
        !:RevChars = [DigitChar | !.RevChars]
    else
        NegN1 = NegN // Base,
        N10 = (NegN1 * Base) - NegN,
        DigitChar = char.det_base_int_to_digit(Base, N10),
        int_to_base_string_2(NegN1, Base, !RevChars),
        !:RevChars = [DigitChar | !.RevChars]
    ).

int_to_string_thousands(N) =
    int_to_base_string_group(N, 10, 3, ",").

int_to_base_string_group(N, Base, GroupLength, Sep) = Str :-
    ( if 2 =< Base, Base =< 36 then
        true
    else
        unexpected($pred, "invalid base")
    ),
    int_to_base_string_group_1(N, Base, GroupLength, Sep, Str).

:- pred int_to_base_string_group_1(int::in, int::in, int::in,
    string::in, string::uo) is det.

int_to_base_string_group_1(N, Base, GroupLength, Sep, Str) :-
    % Note that in order to handle MININT correctly, we need to do
    % the conversion of the absolute number into digits using negative numbers
    % (we can't use positive numbers, since -MININT overflows)
    ( if N < 0 then
        int_to_base_string_group_2(N, Base, 0, GroupLength, Sep, Str1),
        append("-", Str1, Str)
    else
        N1 = 0 - N,
        int_to_base_string_group_2(N1, Base, 0, GroupLength, Sep, Str)
    ).

    % int_to_base_string_group_2(NegN, Base, Curr, GroupLength, Sep, Str):
    %
    % GroupLength is how many digits there should be between separators.
    % Curr is how many digits have been processed since the last separator
    % was inserted.
    % int_to_base_string_group_2/6 is almost identical to
    % int_to_base_string_2/3 above so any changes here might also
    % need to be applied to int_to_base_string_2/3.
    %
:- pred int_to_base_string_group_2(int::in, int::in, int::in, int::in,
    string::in, string::uo) is det.

int_to_base_string_group_2(NegN, Base, Curr, GroupLength, Sep, Str) :-
    ( if
        Curr = GroupLength,
        GroupLength > 0
    then
        int_to_base_string_group_2(NegN, Base, 0, GroupLength,
            Sep, Str1),
        append(Str1, Sep, Str)
    else
        ( if NegN > -Base then
            N = -NegN,
            DigitChar = char.det_base_int_to_digit(Base, N),
            char_to_string(DigitChar, Str)
        else
            NegN1 = NegN // Base,
            N10 = (NegN1 * Base) - NegN,
            DigitChar = char.det_base_int_to_digit(Base, N10),
            char_to_string(DigitChar, DigitString),
            int_to_base_string_group_2(NegN1, Base, Curr + 1,
                GroupLength, Sep, Str1),
            append(Str1, DigitString, Str)
        )
    ).

%---------------------%

:- pragma foreign_proc("C",
    uint_to_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char buffer[21];
    sprintf(buffer, ""%"" MR_INTEGER_LENGTH_MODIFIER ""u"", U);
    MR_allocate_aligned_string_msg(Str, strlen(buffer), MR_ALLOC_ID);
    strcpy(Str, buffer);
").

:- pragma foreign_proc("C#",
    uint_to_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = U.ToString();
").

:- pragma foreign_proc("Java",
    uint_to_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = java.lang.Long.toString(U & 0xffffffffL);
").

uint_to_string(_) = _ :-
    sorry($module, "string.uint_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    int8_to_string(I8::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[5]; // 1 for sign, 3 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRId8, I8);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    int8_to_string(I8::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I8.ToString();
").

:- pragma foreign_proc("Java",
    int8_to_string(I8::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(I8);
").

int8_to_string(_) = _ :-
    sorry($module, "string.int8_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    uint8_to_string(U8::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // Use a larger buffer than necessary (3 bytes for digits, 1 for nul)
    // to avoid spurious warning from gcc -Werror=format-overflow.
    char buffer[24];
    sprintf(buffer, ""%"" PRIu8, U8);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    uint8_to_string(U8::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U8.ToString();
").

:- pragma foreign_proc("Java",
    uint8_to_string(U8::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(U8 & 0xff);
").

uint8_to_string(_) = _ :-
    sorry($module, "string.uint8_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    int16_to_string(I16::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[7]; // 1 for sign, 5 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRId16, I16);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    int16_to_string(I16::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I16.ToString();
").

:- pragma foreign_proc("Java",
    int16_to_string(I16::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(I16);
").

int16_to_string(_) = _ :-
    sorry($module, "string.int16_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    uint16_to_string(U16::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[6]; // 5 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIu16, U16);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    uint16_to_string(U16::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U16.ToString();
").

:- pragma foreign_proc("Java",
    uint16_to_string(U16::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(U16 & 0xffff);
").

uint16_to_string(_) = _ :-
    sorry($module, "string.uint16_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    int32_to_string(I32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[12]; // 1 for sign, 10 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRId32, I32);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    int32_to_string(I32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I32.ToString();
").

:- pragma foreign_proc("Java",
    int32_to_string(I32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(I32);
").

int32_to_string(_) = _ :-
    sorry($module, "string.int32_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    uint32_to_string(U32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[11]; // 10 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIu32, U32);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    uint32_to_string(U32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U32.ToString();
").

:- pragma foreign_proc("Java",
    uint32_to_string(U32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toString(U32 & 0xffffffffL);
").

uint32_to_string(_) = _ :-
    sorry($module, "string.uint32_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    int64_to_string(I64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[21]; // 1 for sign, 19 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRId64, I64);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    int64_to_string(I64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I64.ToString();
").

:- pragma foreign_proc("Java",
    int64_to_string(I64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toString(I64);
").

int64_to_string(_) = _ :-
    sorry($module, "string.int64_to_string/1").

%---------------------%

:- pragma foreign_proc("C",
    uint64_to_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[21]; // 20 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIu64, U64);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    uint64_to_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U64.ToString();
").

:- pragma foreign_proc("Java",
    uint64_to_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toUnsignedString(U64);
").

uint64_to_string(_) = _ :-
    sorry($module, "string.uint64_to_string/1").

%---------------------%

float_to_string(Float) = S2 :-
    float_to_string(Float, S2).

:- pragma foreign_proc("C",
    float_to_string(Flt::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    // For efficiency reasons we duplicate the C implementation
    // of lowlevel_float_to_string.
    MR_float_to_string(Flt, Str, MR_ALLOC_ID);
}").
:- pragma foreign_proc("C#",
    float_to_string(Flt::in, Str::uo),
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

        // Append '.0' if there is no 'e' or '.' in the string.
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
    float_to_string(Flt::in, Str::uo),
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

float_to_string(Float, unsafe_promise_unique(String)) :-
    % XXX This implementation has problems when the mantissa
    % cannot fit in an int.
    %
    % XXX The unsafe_promise_unique is needed because in
    % float_to_string_loop the call to to_float doesn't
    % have a (ui, out) mode hence the output string cannot be unique.
    String = float_to_string_loop(min_precision, Float).

:- func float_to_string_loop(int, float) = (string) is det.

float_to_string_loop(Prec, Float) = String :-
    format("%#." ++ int_to_string(Prec) ++ "g", [f(Float)], Tmp),
    ( if Prec = max_precision then
        String = Tmp
    else
        ( if to_float(Tmp, Float) then
            String = Tmp
        else
            String = float_to_string_loop(Prec + 1, Float)
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

from_float(Float) = float_to_string(Float).

%---------------------%

c_pointer_to_string(P) = S :-
    c_pointer_to_string(P, S).

:- pragma foreign_proc("C#",
    c_pointer_to_string(C_Pointer::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Within the spirit of the function, at least.
    if (C_Pointer == null) {
        Str = ""null"";
    } else {
        Str = C_Pointer.ToString();
    }
").
:- pragma foreign_proc("Java",
    c_pointer_to_string(C_Pointer::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Within the spirit of the function, at least.
    if (C_Pointer == null) {
        Str = ""null"";
    } else {
        Str = C_Pointer.toString();
    }
").

c_pointer_to_string(C_Pointer, Str) :-
    private_builtin.unsafe_type_cast(C_Pointer, Int),
    Str = "c_pointer(0x" ++ int_to_base_string(Int, 16) ++ ")".

from_c_pointer(P) = S :-
    c_pointer_to_string(P, S).

%---------------------------------------------------------------------------%
%
% Converting values of arbitrary types to strings.
%

string(X) =
    to_string.string_impl(X).

string_ops(OpsTable, X) =
    to_string.string_ops_impl(OpsTable, X).

string_ops_noncanon(NonCanon, OpsTable, X, String) :-
    to_string.string_ops_noncanon_impl(NonCanon, OpsTable, X, String).

%---------------------------------------------------------------------------%
%
% Converting values to strings based on a format string.
%

format(S1, PT) = S2 :-
    format(S1, PT, S2).

format(FormatString, PolyList, String) :-
    format.format_impl(FormatString, PolyList, String).

%---------------------------------------------------------------------------%
