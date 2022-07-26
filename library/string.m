%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.m.
% Main authors: fjh, petdr, wangp.
% Stability: medium to high.
%
% This module provides basic string handling facilities.
%
% Mercury strings are Unicode strings. They use either the UTF-8 or UTF-16
% encoding, depending on the target language.
%
% When Mercury is compiled to C, strings are UTF-8 encoded, with a null
% character as the string terminator. A single code point requires one to four
% bytes (code units) to encode.
%
% When Mercury is compiled to Java, strings are represented using Java's
% String type. When Mercury is compiled to C#, strings are represented using
% C#'s `System.String' type. Both of these types use the UTF-16 encoding.
% A single code point requires one or two 16-bit integers (code units)
% to encode.
%
% The Mercury compiler will only allow well-formed UTF-8 or UTF-16 string
% constants. However, it is possible to produce strings containing invalid
% UTF-8 or UTF-16 via I/O, foreign code, and substring operations.
% Predicates or functions that inspect strings may fail, throw an exception,
% or else behave in some special way when they encounter an ill-formed
% code unit sequence.
%
% Unexpected null characters embedded in the middle of strings can be a source
% of security vulnerabilities, so the Mercury library predicates and functions
% which create strings from (lists of) characters throw an exception if they
% detect such a null character. Programmers must not create strings that might
% contain null characters using the foreign language interface.
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
    % If strings use UTF-8 encoding, then each code unit in an ill-formed
    % sequence is replaced by U+FFFD REPLACEMENT CHARACTER in the list.
    % If strings use UTF-16 encoding, then each unpaired surrogate code point
    % is returned as a separate code point in the list.
    %
:- func to_char_list(string) = list(char).
:- pred to_char_list(string::in, list(char)::out) is det.

    % Convert the string to a list of characters (code points) in reverse
    % order.
    %
    % If strings use UTF-8 encoding, then each code unit in an ill-formed
    % sequence is replaced by U+FFFD REPLACEMENT CHARACTER in the list.
    % If strings use UTF-16 encoding, then each unpaired surrogate code point
    % is returned as a separate code point in the list.
    %
:- func to_rev_char_list(string) = list(char).
:- pred to_rev_char_list(string::in, list(char)::out) is det.

    % Convert a list of characters (code points) to a string.
    % Throws an exception if the list contains a null character or code point
    % that cannot be encoded in a string (namely, surrogate code points cannot
    % be encoded in UTF-8 strings).
    %
:- func from_char_list(list(char)::in) = (string::uo) is det.
:- pred from_char_list(list(char)::in, string::uo) is det.

    % As above, but fail instead of throwing an exception if the list contains
    % a null character or code point that cannot be encoded in a string.
    %
:- pred semidet_from_char_list(list(char)::in, string::uo) is semidet.

    % Same as from_char_list, except that it reverses the order
    % of the characters.
    % Throws an exception if the list contains a null character or code point
    % that cannot be encoded in a string (namely, surrogate code points cannot
    % be encoded in UTF-8 strings).
    %
:- func from_rev_char_list(list(char)::in) = (string::uo) is det.
:- pred from_rev_char_list(list(char)::in, string::uo) is det.

    % As above, but fail instead of throwing an exception if the list contains
    % a null character or code point that cannot be encoded in a string.
    %
:- pred semidet_from_rev_char_list(list(char)::in, string::uo) is semidet.

    % Convert a string into a list of code units of the string encoding used
    % by the current process.
    %
:- pred to_code_unit_list(string::in, list(int)::out) is det.

    % Convert a string into a list of UTF-8 code units.
    % Throws an exception if the string contains an unpaired surrogate code
    % point, as the encoding of surrogate code points is prohibited in UTF-8.
    %
:- pred to_utf8_code_unit_list(string::in, list(int)::out) is det.

    % Convert a string into a list of UTF-16 code units.
    % Throws an exception if strings use UTF-8 encoding and the given string
    % contains an ill-formed code unit sequence, as arbitrary bytes cannot be
    % represented in UTF-16 (even allowing for ill-formed sequences).
    %
:- pred to_utf16_code_unit_list(string::in, list(int)::out) is det.

    % Convert a list of code units to a string.
    % Fails if the list does not contain a valid encoding of a string
    % (in the encoding expected by the current process),
    % or if the string would contain a null character.
    %
:- pred from_code_unit_list(list(int)::in, string::uo) is semidet.

    % Convert a list of code units to a string.
    % The resulting string may contain ill-formed sequences.
    % Fails if the list contains a code unit that is out of range
    % or if the string would contain a null character.
    %
:- pred from_code_unit_list_allow_ill_formed(list(int)::in, string::uo)
    is semidet.

    % Convert a list of UTF-8 code units to a string.
    % Fails if the list does not contain a valid encoding of a string
    % or if the string would contain a null character.
    %
:- pred from_utf8_code_unit_list(list(int)::in, string::uo) is semidet.

    % Convert a list of UTF-16 code units to a string.
    % Fails if the list does not contain a valid encoding of a string
    % or if the string would contain a null character.
    %
:- pred from_utf16_code_unit_list(list(int)::in, string::uo) is semidet.

    % duplicate_char(Char, Count, String):
    %
    % Construct a string consisting of Count occurrences of Char code points
    % in sequence, returning the empty string if Count is less than or equal
    % to zero. Throws an exception if Char is a null character or code point
    % that cannot be encoded in a string (namely, surrogate code points cannot
    % be encoded in UTF-8 strings).
    %
:- func duplicate_char(char::in, int::in) = (string::uo) is det.
:- pred duplicate_char(char::in, int::in, string::uo) is det.

%---------------------------------------------------------------------------%
%
% Reading characters from strings.
%

    % This type is used by the _repl indexing predicates to distinguish a
    % U+FFFD code point that is actually in a string from a U+FFFD code point
    % generated when the predicate encounters an ill-formed code unit sequence
    % in a UTF-8 string.
    %
:- type maybe_replaced
    --->    not_replaced
    ;       replaced_code_unit(uint8).

    % index(String, Index, Char):
    %
    % If Index is the initial code unit offset of a well-formed code unit
    % sequence in String then Char is the code point encoded by that
    % sequence.
    %
    % Otherwise, if Index is in range, Char is either a U+FFFD REPLACEMENT
    % CHARACTER (when strings are UTF-8 encoded) or the unpaired surrogate
    % code point at Index (when strings are UTF-16 encoded).
    %
    % Fails if Index is out of range (negative, or greater than or equal to
    % the length of String).
    %
:- pred index(string::in, int::in, char::uo) is semidet.

    % det_index(String, Index, Char):
    %
    % Like index/3 but throws an exception if Index is out of range
    % (negative, or greater than or equal to the length of String).
    %
:- func det_index(string, int) = char.
:- pred det_index(string::in, int::in, char::uo) is det.

    % unsafe_index(String, Index, Char):
    %
    % Like index/3 but does not check that Index is in range.
    %
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than or equal to the length of String).
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
    % If Index is the initial code unit offset of a well-formed code unit
    % sequence in String then Char is the code point encoded by that
    % sequence, and NextIndex is the offset immediately following that
    % sequence.
    %
    % Otherwise, if Index is in range, Char is either a U+FFFD REPLACEMENT
    % CHARACTER (when strings are UTF-8 encoded) or the unpaired surrogate
    % code point at Index (when strings are UTF-16 encoded), and NextIndex
    % is Index + 1.
    %
    % Fails if Index is out of range (negative, or greater than or equal to
    % the length of String).
    %
:- pred index_next(string::in, int::in, int::out, char::uo) is semidet.

    % index_next_repl(String, Index, NextIndex, Char, MaybeReplaced):
    %
    % Like index_next/4 but also returns MaybeReplaced on success.
    % When Char is not U+FFFD, then MaybeReplaced is always `not_replaced'.
    % When Char is U+FFFD (the Unicode replacement character), then there are
    % two cases:
    %
    % - If there is a U+FFFD code point encoded in String at
    %   [Index, NextIndex) then MaybeReplaced is `not_replaced'.
    %
    % - Otherwise, MaybeReplaced is `replaced_code_unit(CodeUnit)' where
    %   CodeUnit is the code unit in String at Index.
    %
:- pred index_next_repl(string::in, int::in, int::out, char::uo,
    maybe_replaced::out) is semidet.

    % unsafe_index_next(String, Index, NextIndex, Char):
    %
    % Like index_next/4 but does not check that Index is in range.
    % Fails if Index is equal to the length of String.
    %
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than the length of String).
    %
:- pred unsafe_index_next(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_index_next_repl(String, Index, NextIndex, Char, MaybeReplaced):
    %
    % Like index_next_repl/5 but does not check that Index is in range.
    % Fails if Index is equal to the length of String.
    %
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than the length of String).
    %
:- pred unsafe_index_next_repl(string::in, int::in, int::out, char::uo,
    maybe_replaced::out) is semidet.

    % prev_index(String, Index, PrevIndex, Char):
    %
    % If Index - 1 is the final code unit offset of a well-formed sequence in
    % String then Char is the code point encoded by that sequence, and
    % PrevIndex is the initial code unit offset of that sequence.
    %
    % Otherwise, if Index is in range, Char is either a U+FFFD REPLACEMENT
    % CHARACTER (when strings are UTF-8 encoded) or the unpaired surrogate
    % code point at Index - 1 (when strings are UTF-16 encoded), and
    % PrevIndex is Index - 1.
    %
    % Fails if Index is out of range (non-positive, or greater than the
    % length of String).
    %
:- pred prev_index(string::in, int::in, int::out, char::uo) is semidet.

    % prev_index_repl(String, Index, PrevIndex, Char, MaybeReplaced):
    %
    % Like prev_index/4 but also returns MaybeReplaced on success.
    % When Char is not U+FFFD, then MaybeReplaced is always `not_replaced'.
    % When Char is U+FFFD (the Unicode replacement character), then there are
    % two cases:
    %
    % - If there is a U+FFFD code point encoded in String at
    %   [PrevIndex, Index) then MaybeReplaced is `not_replaced'.
    %
    % - Otherwise, MaybeReplaced is `replaced_code_unit(CodeUnit)' where
    %   CodeUnit is the code unit in String at Index - 1.
    %
:- pred prev_index_repl(string::in, int::in, int::out, char::uo,
    maybe_replaced::out) is semidet.

    % unsafe_prev_index(String, Index, PrevIndex, Char):
    %
    % Like prev_index/4 but does not check that Index is in range.
    % Fails if Index is zero.
    %
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than the length of String).
    %
:- pred unsafe_prev_index(string::in, int::in, int::out, char::uo) is semidet.

    % unsafe_prev_index_repl(String, Index, PrevIndex, Char, MaybeReplaced):
    %
    % Like prev_index_repl/5 but does not check that Index is in range.
    % Fails if Index is zero.
    %
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than the length of String).
    %
:- pred unsafe_prev_index_repl(string::in, int::in, int::out, char::uo,
    maybe_replaced::out) is semidet.

    % unsafe_index_code_unit(String, Index, CodeUnit):
    %
    % CodeUnit is the code unit in String at the offset Index.
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than or equal to the length of String).
    %
:- pred unsafe_index_code_unit(string::in, int::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Writing characters to strings.
%

    % set_char(Char, Index, String0, String):
    %
    % String is String0, with the code unit sequence beginning at Index
    % replaced by the encoding of Char. If the code unit at Index is the
    % initial code unit in a valid encoding of a code point, then that entire
    % code unit sequence is replaced. Otherwise, only the code unit at Index
    % is replaced.
    %
    % Fails if Index is out of range (negative, or greater than or equal to
    % the length of String0).
    %
    % Throws an exception if Char is the null character or a code point that
    % cannot be encoded in a string (namely, surrogate code points cannot be
    % encoded in UTF-8 strings).
    %
:- pred set_char(char, int, string, string).
:- mode set_char(in, in, in, out) is semidet.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode set_char(in, in, di, uo) is semidet.

    % det_set_char(Char, Index, String0, String):
    %
    % Same as set_char/4 but throws an exception if Index is out of range
    % (negative, or greater than or equal to the length of String0).
    %
:- func det_set_char(char, int, string) = string.
:- pred det_set_char(char, int, string, string).
:- mode det_set_char(in, in, in, out) is det.
% NOTE This mode is disabled because the compiler puts constant strings
% into static data even when they might be updated.
% :- mode det_set_char(in, in, di, uo) is det.

    % unsafe_set_char(Char, Index, String0, String):
    %
    % Same as set_char/4 but does not check if Index is in range.
    % WARNING: behavior is UNDEFINED if Index is out of range
    % (negative, or greater than or equal to the length of String0).
    % Use with care!
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

    % count_utf8_code_units(String) = Length:
    %
    % Return the number of code units required to represent a string in
    % UTF-8 encoding (with allowance for ill-formed sequences).
    % Equivalent to Length = length(to_utf8_code_unit_list(String)).
    %
    % Throws an exception if strings use UTF-16 encoding but the given string
    % contains an unpaired surrogate code point. Surrogate code points cannot
    % be represented in UTF-8.
    %
:- func count_utf8_code_units(string) = int.

    % codepoint_offset(String, StartOffset, Count, Offset):
    %
    % Let S be the substring of String from code unit StartOffset to the
    % end of the string. Offset is code unit offset after advancing Count
    % steps in S, where each step skips over either:
    %  - one encoding of a Unicode code point, or
    %  - one code unit that is part of an ill-formed sequence.
    %
    % Fails if StartOffset is out of range (negative, or greater than the
    % length of String), or if there are fewer than Count steps possible
    % in S.
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
    % True iff all code points in String satisfy TestPred, and String contains
    % no ill-formed code unit sequences.
    %
:- pred all_match(pred(char)::in(pred(in) is semidet), string::in) is semidet.

    % contains_match(TestPred, String):
    %
    % True iff String contains at least one code point that satisfies
    % TestPred. Any ill-formed code unit sequences in String are ignored
    % as they do not encode code points.
    %
:- pred contains_match(pred(char)::in(pred(in) is semidet), string::in)
    is semidet.

    % contains_char(String, Char):
    %
    % Succeed if the code point Char occurs in String.
    % Any ill-formed code unit sequences within String are ignored
    % as they will not contain Char.
    %
:- pred contains_char(string::in, char::in) is semidet.

    % compare_substrings(Res, X, StartX, Y, StartY, Length):
    %
    % Compare two substrings by code unit order. The two substrings are
    % the substring of X between StartX and StartX + Length, and
    % the substring of Y between StartY and StartY + Length.
    % StartX, StartY and Length are all in terms of code units.
    %
    % Fails if StartX or StartX + Length are not within [0, length(X)],
    % or if StartY or StartY + Length are not within [0, length(Y)],
    % or if Length is negative.
    %
:- pred compare_substrings(comparison_result::uo, string::in, int::in,
    string::in, int::in, int::in) is semidet.

    % unsafe_compare_substrings(Res, X, StartX, Y, StartY, Length):
    %
    % Same as compare_between/4 but without range checks.
    % WARNING: if any of StartX, StartY, StartX + Length or
    % StartY + Length are out of range, or if Length is negative,
    % then the behaviour is UNDEFINED. Use with care!
    %
:- pred unsafe_compare_substrings(comparison_result::uo, string::in, int::in,
    string::in, int::in, int::in) is det.

    % compare_ignore_case_ascii(Res, X, Y):
    %
    % Compare two strings by code unit order, ignoring the case of letters
    % (A-Z, a-z) in the ASCII range.
    % Equivalent to `compare(Res, to_lower(X), to_lower(Y))'
    % but more efficient.
    %
:- pred compare_ignore_case_ascii(comparison_result::uo,
    string::in, string::in) is det.

    % prefix_length(Pred, String):
    %
    % The length (in code units) of the maximal prefix of String consisting
    % entirely of code points satisfying Pred.
    %
:- func prefix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % suffix_length(Pred, String):
    %
    % The length (in code units) of the maximal suffix of String consisting
    % entirely of code points satisfying Pred.
    %
:- func suffix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % sub_string_search(String, SubString, Index):
    %
    % Index is the code unit position in String where the first
    % occurrence of SubString begins. Indices start at zero, so if
    % SubString is a prefix of String, this will return Index = 0.
    %
:- pred sub_string_search(string::in, string::in, int::out) is semidet.

    % sub_string_search_start(String, SubString, BeginAt, Index):
    %
    % Index is the code unit position in String where the first
    % occurrence of SubString occurs such that 'Index' is greater than or
    % equal to BeginAt. Indices start at zero.
    % Fails if either BeginAt is negative, or greater than
    % length(String) - length(SubString).
    %
:- pred sub_string_search_start(string::in, string::in, int::in, int::out)
    is semidet.

    % unsafe_sub_string_search_start(String, SubString, BeginAt, Index):
    %
    % Same as sub_string_search_start/4 but does not check that BeginAt
    % is in range.
    % WARNING: if BeginAt is either negative, or greater than length(String),
    % then the behaviour is UNDEFINED. Use with care!
    %
:- pred unsafe_sub_string_search_start(string::in, string::in, int::in,
    int::out) is semidet.

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
:- pred append(string, string, string).
:- mode append(in, in, in) is semidet.  % implied
:- mode append(in, uo, in) is semidet.
:- mode append(in, in, uo) is det.
:- mode append(uo, in, in) is semidet.

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
% Making strings from smaller pieces.
%

:- type string_piece
    --->    string(string)
    ;       substring(string, int, int).    % string, start, end offset

    % append_string_pieces(Pieces, String):
    %
    % Append together the strings and substrings in Pieces into a string.
    % Throws an exception if Pieces contains an element
    % `substring(S, Start, End)' where Start or End are not within
    % the range [0, length(S)], or if Start > End.
    %
:- pred append_string_pieces(list(string_piece)::in, string::uo) is det.

    % Same as append_string_pieces/2 but without range checks.
    % WARNING: if any piece `substring(S, Start, End)' has Start or End
    % outside the range [0, length(S)], or if Start > End,
    % then the behaviour is UNDEFINED. Use with care!
    %
:- pred unsafe_append_string_pieces(list(string_piece)::in, string::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Splitting up strings.
%

    % first_char(String, Char, Rest) is true iff String begins with a
    % well-formed code unit sequence, Char is the code point encoded by
    % that sequence, and Rest is the rest of String after that sequence.
    %
    % The (uo, in, in) mode throws an exception if Char cannot be encoded in
    % a string, or if Char is a surrogate code point (for consistency with
    % the other modes).
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
    % Split a string into two substrings at the code unit offset Index.
    % (If Index is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred split(string::in, int::in, string::out, string::out) is det.

    % split_by_codepoint(String, Count, LeftSubstring, RightSubstring):
    %
    % LeftSubstring is the left-most Count code points of String,
    % and RightSubstring is the remainder of String.
    % (If Count is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred split_by_codepoint(string::in, int::in, string::out, string::out)
    is det.

    % left(String, Count, LeftSubstring):
    %
    % LeftSubstring is the left-most Count code units of String.
    % (If Count is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func left(string::in, int::in) = (string::out) is det.
:- pred left(string::in, int::in, string::out) is det.

    % left_by_codepoint(String, Count, LeftSubstring):
    %
    % LeftSubstring is the left-most Count code points of String.
    % (If Count is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func left_by_codepoint(string::in, int::in) = (string::out) is det.
:- pred left_by_codepoint(string::in, int::in, string::out) is det.

    % right(String, Count, RightSubstring):
    %
    % RightSubstring is the right-most Count code units of String.
    % (If Count is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func right(string::in, int::in) = (string::out) is det.
:- pred right(string::in, int::in, string::out) is det.

    % right_by_codepoint(String, Count, RightSubstring):
    %
    % RightSubstring is the right-most Count code points of String.
    % (If Count is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func right_by_codepoint(string::in, int::in) = (string::out) is det.
:- pred right_by_codepoint(string::in, int::in, string::out) is det.

    % between(String, Start, End, Substring):
    %
    % Substring consists of the segment of String within the half-open
    % interval [Start, End), where Start and End are code unit offsets.
    % (If Start is out of the range [0, length of String], it is treated
    % as if it were the nearest end-point of that range.
    % If End is out of the range [Start, length of String],
    % it is treated as if it were the nearest end-point of that range.)
    %
:- func between(string::in, int::in, int::in) = (string::uo) is det.
:- pred between(string::in, int::in, int::in, string::uo) is det.

    % between_codepoints(String, Start, End, Substring):
    %
    % Substring is the part of String between the code point positions
    % Start and End. The result is equivalent to:
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
    % Substring consists of the segment of String within the half-open
    % interval [Start, End), where Start and End are code unit offsets.
    % WARNING: if Start is out of the range [0, length of String] or
    % End is out of the range [Start, length of String]
    % then the behaviour is UNDEFINED. Use with care!
    % This version takes time proportional to the length of the substring,
    % whereas substring may take time proportional to the length
    % of the whole string.
    %
:- func unsafe_between(string::in, int::in, int::in) = (string::uo) is det.
:- pred unsafe_between(string::in, int::in, int::in, string::uo) is det.

    % words_separator(SepP, String) returns the list of non-empty
    % substrings of String (in first to last order) that are delimited
    % by non-empty sequences of code points matched by SepP.
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
    % by code points matched by SepP. For example,
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

    % split_into_lines(String) breaks String into a sequence of lines,
    % with each line consisting of a possibly empty sequence of non-newline
    % characters, followed either by a newline character, or by the end
    % of the string. The string returned for a line will not contain
    % the newline character.
    %
:- func split_into_lines(string) = list(string).

%---------------------------------------------------------------------------%
%
% Dealing with prefixes and suffixes.
%

    % prefix(String, Prefix) is true iff Prefix is a prefix of String.
    % Same as append(Prefix, _, String).
    %
:- pred prefix(string, string).
:- mode prefix(in, in) is semidet.

    % suffix(String, Suffix) is true iff Suffix is a suffix of String.
    % Same as append(_, Suffix, String).
    %
:- pred suffix(string, string).
:- mode suffix(in, in) is semidet.

    % remove_prefix(Prefix, String, Suffix):
    %
    % This is a synonym for append(Prefix, Suffix, String) but with the
    % arguments in a more convenient order for use with higher-order code.
    %
    % WARNING: the argument order differs from remove_suffix.
    %
:- pred remove_prefix(string::in, string::in, string::out) is semidet.

    % det_remove_prefix(Prefix, String, Suffix):
    %
    % This is a synonym for append(Prefix, Suffix, String) but with the
    % arguments in a more convenient order for use with higher-order code.
    %
    % WARNING: the argument order differs from remove_suffix.
    %
:- pred det_remove_prefix(string::in, string::in, string::out) is det.

    % remove_prefix_if_present(Prefix, String) = Suffix returns String minus
    % Prefix if String begins with Prefix, and String if it doesn't.
    %
:- func remove_prefix_if_present(string, string) = string.

    % remove_suffix(String, Suffix, Prefix):
    %
    % The same as append(Prefix, Suffix, String).
    %
    % WARNING: the argument order differs from both remove_prefix and
    % remove_suffix_if_present.
    %
:- pred remove_suffix(string::in, string::in, string::out) is semidet.

    % det_remove_suffix(String, Suffix) returns the same value as
    % remove_suffix, except it throws an exception if String does not end
    % with Suffix.
    %
    % WARNING: the argument order differs from both remove_prefix and
    % remove_suffix_if_present.
    %
:- func det_remove_suffix(string, string) = string.

    % remove_suffix_if_present(Suffix, String) returns String minus Suffix
    % if String ends with Suffix, and String if it doesn't.
    %
    % WARNING: the argument order differs from remove_suffix and
    % det_remove_suffix.
    %
:- func remove_suffix_if_present(string, string) = string.

    % add_suffix(Suffix, Str) = StrSuffix:
    %
    % Does the same job as Str ++ Suffix = StrSuffix, but allows
    % using list.map to add the same suffix to many strings.
    %
:- func add_suffix(string, string) = string.

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
    % Insert PadChars at the left of String0 until it is at least as long
    % as Width, giving String. Width is currently measured as the number
    % of code points.
    %
:- func pad_left(string, char, int) = string.
:- pred pad_left(string::in, char::in, int::in, string::out) is det.

    % pad_right(String0, PadChar, Width, String):
    %
    % Insert PadChars at the right of String0 until it is at least as long
    % as Width, giving String. Width is currently measured as the number
    % of code points.
    %
:- func pad_right(string, char, int) = string.
:- pred pad_right(string::in, char::in, int::in, string::out) is det.

    % chomp(String):
    %
    % Return String minus any single trailing newline character.
    %
:- func chomp(string) = string.

    % strip(String):
    %
    % Returns String minus any initial and trailing ASCII whitespace
    % characters, i.e. characters satisfying char.is_whitespace.
    %
:- func strip(string) = string.

    % lstrip(String):
    %
    % Return String minus any initial ASCII whitespace characters,
    % i.e. characters satisfying char.is_whitespace.
    %
:- func lstrip(string) = string.

    % rstrip(String):
    %
    % Returns String minus any trailing ASCII whitespace characters,
    % i.e. characters satisfying char.is_whitespace.
    %
:- func rstrip(string) = string.

    % lstrip_pred(Pred, String):
    %
    % Returns String minus the maximal prefix consisting entirely
    % of code points satisfying Pred.
    %
:- func lstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % rstrip_pred(Pred, String):
    %
    % Returns String minus the maximal suffix consisting entirely
    % of code points satisfying Pred.
    %
:- func rstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % replace(String0, Pattern, Subst, String):
    %
    % Replaces the first occurrence of Pattern in String0 with Subst to give
    % String. Fails if Pattern does not occur in String0.
    %
:- pred replace(string::in, string::in, string::in, string::uo) is semidet.

    % replace_all(String0, Pattern, Subst, String):
    %
    % Replaces any occurrences of Pattern in String0 with Subst to give
    % String.
    %
    % If Pattern is the empty string then Subst is inserted at every point
    % in String0 except between two code units in an encoding of a code point.
    % For example, these are true:
    %
    %   replace_all("", "", "|", "|")
    %   replace_all("a", "", "|", "|a|")
    %   replace_all("ab", "", "|", "|a|b|")
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
    % See char.is_whitespace for the definition of whitespace characters
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
    % Closure is an accumulator predicate which is to be called for each
    % code point of the string String in turn.
    % If String contains ill-formed sequences, Closure is called for each
    % code unit in an ill-formed sequence. If strings use UTF-8 encoding,
    % U+FFFD is passed to Closure in place of each such code unit.
    % If strings use UTF-16 encoding, each code unit in an ill-formed sequence
    % is an unpaired surrogate code point, which will be passed to Closure.
    %
    % The initial value of the accumulator is !.Acc and the final value is
    % !:Acc.
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
    % Start and End are in terms of code units.
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
    % Start and End are in terms of code units.
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
    % Start and End are in terms of code units.
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
    % a non-empty string of base N digits, or if the number is not in
    % the range [min_int, max_int].
    %
:- func det_base_string_to_int(int, string) = int.

%---------------------%

    % Convert a string to a uint. The string must contain only digits [0-9].
    % If the string does not match this syntax or the number is not
    % in the range [0, max_uint], to_uint fails.
    %
:- pred to_uint(string::in, uint::out) is semidet.

    % Convert a signed base 10 string to a uint. Throws an exception if the
    % string argument does not match the regexp [0-9]+ or the number is
    % not in the range [0, max_uint].
    %
:- func det_to_uint(string) = uint.

    % Convert a string in the specified base (2-36) to a uint. The string
    % must contain one or more digits in the specified base. For bases > 10,
    % digits 10 to 35 are represented by the letters A-Z or a-z. If the string
    % does not match this syntax or the number is not in the range
    % [0, max_uint], the predicate fails.
    %
:- pred base_string_to_uint(int::in, string::in, uint::out) is semidet.

    % Convert a signed base N string to a uint. Throws an exception
    % if the string argument is not precisely a non-empty string of base N
    % digits, or if the number is not in the range [0, max_uint].
    %
:- func det_base_string_to_uint(int, string) = uint.

%---------------------%

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
    % Converts a character to a string, or vice versa.
    % True if String is the well-formed string that encodes the code point
    % Char; or, if strings are UTF-16 encoded, Char is a surrogate code
    % point and String is the string that contains only that surrogate code
    % point. Otherwise, `char_to_string(Char, String)' is false.
    %
    % Throws an exception if Char is the null character or a code point that
    % cannot be encoded in a string (namely, surrogate code points cannot be
    % encoded in UTF-8 strings).
    %
:- func char_to_string(char::in) = (string::uo) is det.
:- pred char_to_string(char, string).
:- mode char_to_string(in, uo) is det.
:- mode char_to_string(out, in) is semidet.

    % A synonym for char_to_string/1.
    %
:- func from_char(char::in) = (string::uo) is det.

    % Convert an integer to a string in base 10.
    % See int_to_base_string for the string format.
    %
:- func int_to_string(int::in) = (string::uo) is det.
:- pred int_to_string(int::in, string::uo) is det.

    % A synonym for int_to_string/1.
    %
:- func from_int(int::in) = (string::uo) is det.

    % int_to_base_string(Int, Base, String):
    %
    % Convert an integer to a string in a given Base.
    % String will consist of a minus sign (U+002D HYPHEN-MINUS)
    % if Int is negative, followed by one or more decimal digits (0-9)
    % or uppercase letters (A-Z). There will be no leading zeros.
    %
    % Base must be between 2 and 36, both inclusive; if it is not,
    % the predicate will throw an exception.
    %
:- func int_to_base_string(int::in, int::in) = (string::uo) is det.
:- pred int_to_base_string(int::in, int::in, string::uo) is det.

    % Convert an integer to a string in base 10 with commas as thousand
    % separators.
    %
:- func int_to_string_thousands(int::in) = (string::uo) is det.

    % int_to_base_string_group(Int, Base, GroupLength, Separator, String):
    %
    % Convert an integer to a string in a given Base,
    % in the same format as int_to_base_string,
    % with Separator inserted between every GroupLength digits
    % (grouping from the end of the string).
    % If GroupLength is less than one, no separators will appear
    % in the output. Useful for formatting numbers like "1,300,000".
    %
    % Base must be between 2 and 36, both inclusive; if it is not,
    % the predicate will throw an exception.
    %
:- func int_to_base_string_group(int, int, int, string) = string.
:- mode int_to_base_string_group(in, in, in, in) = uo is det.

    % Convert an unsigned integer to a string in base 10.
    %
:- func uint_to_string(uint::in) = (string::uo) is det.

    % Convert an unsigned integer to a string in base 16.
    % Alphabetic digits will be lowercase (e.g. a-f).
    %
:- func uint_to_hex_string(uint::in) = (string::uo) is det.
:- func uint_to_lc_hex_string(uint::in) = (string::uo) is det.

    % Convert an unsigned integer to a string in base 16.
    % Alphabetic digits will be uppercase (e.g. A-F).
    %
:- func uint_to_uc_hex_string(uint::in) = (string::uo) is det.

    % Convert an unsigned integer to a string in base 8.
    %
:- func uint_to_octal_string(uint::in) = (string::uo) is det.

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

    % Convert an unsigned 64-bit integer to a string in base 16.
    % Alphabetic digits will be lowercase (e.g. a-f).
    %
:- func uint64_to_hex_string(uint64::in) = (string::uo) is det.
:- func uint64_to_lc_hex_string(uint64::in) = (string::uo) is det.

    % Convert an unsigned 64-bit integer to a string in base 16.
    % Alphabetic digits will be uppercase (e.g. A-F).
    %
:- func uint64_to_uc_hex_string(uint64::in) = (string::uo) is det.

    % Convert an unsigned 64-bit integer to a string in base 8.
    %
:- func uint64_to_octal_string(uint64::in) = (string::uo) is det.

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
    ;       i8(int8)
    ;       i16(int16)
    ;       i32(int32)
    ;       i64(int64)
    ;       u(uint)
    ;       u8(uint8)
    ;       u16(uint16)
    ;       u32(uint32)
    ;       u64(uint64)
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
    % conv  var         output form.      effect of '#'.
    % char. type(s).
    %
    % d     int         signed integer
    % i     int         signed integer
    % o     int, uint   unsigned octal    with '0' prefix
    % x,X   int, uint   unsigned hex      with '0x', '0X' prefix
    % u     int, uint   unsigned integer
    % c     char        character
    % s     string      string
    % f     float       rational number   with '.', if precision 0
    % e,E   float       [-]m.dddddE+-xx   with '.', if precision 0
    % g,G   float       either e or f     with trailing zeros.
    % p     int, uint   integer
    %
    % The valid conversion characters for int8, int16, int32 and int64
    % are the same as for int, and the valid conversion characters for
    % uint8, uint16, uint32 and uint64 are the same as for uint.
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

:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.format.
:- import_module string.to_string.
:- import_module term_io.
:- import_module uint.
:- import_module uint8.

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

%---------------------------------------------------------------------------%
%
% Conversions between strings and lists of characters.
%

to_char_list(S) = Cs :-
    to_char_list(S, Cs).

to_char_list(Str, CharList) :-
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

to_rev_char_list(Str, RevCharList) :-
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
%
% XXX There is an inconsistency in that from_char_list/from_rev_char_list
% throw exceptions unlike from_code_unit_list/from_{utf8,utf16}_code_unit_list
% which fail when the list of code points cannot be encoded in a string.

from_char_list(Cs) = S :-
    from_char_list(Cs, S).

from_char_list(Chars, Str) :-
    ( if semidet_from_char_list(Chars, Str0) then
        Str = Str0
    else
        unexpected($pred, "null character or surrogate code point in list")
    ).

:- pragma foreign_proc("C",
    semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word char_list_ptr;
    size_t size;

    // Loop to calculate list length + sizeof(MR_Word) in `size'
    // using list in `char_list_ptr'.
    SUCCESS_INDICATOR = MR_TRUE;
    size = 0;
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        MR_Char c = (MR_Char) MR_list_head(char_list_ptr);
        if (c == '\\0') {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
        if (MR_is_ascii(c)) {
            size++;
        } else {
            size_t csize = MR_utf8_width(c);
            if (csize == 0) {
                // c is a surrogate code point (or even out of range,
                // but that is not supposed to happen).
                SUCCESS_INDICATOR = MR_FALSE;
                break;
            }
            size += csize;
        }
        char_list_ptr = MR_list_tail(char_list_ptr);
    }

    if (SUCCESS_INDICATOR) {
        // Allocate heap space for string.
        MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

        // Loop to copy the characters from the char_list to the string.
        size = 0;
        char_list_ptr = CharList;
        while (! MR_list_is_empty(char_list_ptr)) {
            MR_Char c = (MR_Char) MR_list_head(char_list_ptr);
            if (MR_is_ascii(c)) {
                Str[size] = c;
                size++;
            } else {
                size += MR_utf8_encode(Str + size, c);
            }
            char_list_ptr = MR_list_tail(char_list_ptr);
        }

        Str[size] = '\\0';
    }
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

semidet_from_char_list(CharList, Str) :-
    (
        CharList = [],
        Str = ""
    ;
        CharList = [C | Cs],
        not char.to_int(C, 0),
        internal_encoding_is_utf8 => not char.is_surrogate(C),
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
        unexpected($pred, "null character or surrogate code point in list")
    ).

:- pragma foreign_proc("C",
    semidet_from_rev_char_list(Chars::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list_ptr;
    MR_Word size;

    // Loop to calculate list length in `size' using list in `list_ptr'.
    SUCCESS_INDICATOR = MR_TRUE;
    size = 0;
    list_ptr = Chars;
    while (!MR_list_is_empty(list_ptr)) {
        MR_Char c = (MR_Char) MR_list_head(list_ptr);
        if (c == '\\0') {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
        if (MR_is_ascii(c)) {
            size++;
        } else {
            size_t csize = MR_utf8_width(c);
            if (csize == 0) {
                // c is a surrogate code point (or even out of range,
                // but that is not supposed to happen).
                SUCCESS_INDICATOR = MR_FALSE;
                break;
            }
            size += csize;
        }
        list_ptr = MR_list_tail(list_ptr);
    }

    if (SUCCESS_INDICATOR) {
        // Allocate heap space for string.
        MR_allocate_aligned_string_msg(Str, size, MR_ALLOC_ID);

        // Set size to be the offset of the end of the string
        // (ie the \\0) and null terminate the string.
        Str[size] = '\\0';

        // Loop to copy the characters from the list_ptr to the string
        // in reverse order.
        list_ptr = Chars;
        while (! MR_list_is_empty(list_ptr)) {
            MR_Char c = (MR_Char) MR_list_head(list_ptr);
            if (MR_is_ascii(c)) {
                size--;
                Str[size] = c;
            } else {
                size -= MR_utf8_width(c);
                MR_utf8_encode(Str + size, c);
            }
            list_ptr = MR_list_tail(list_ptr);
        }
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

semidet_from_rev_char_list(Chars, Str) :-
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
        unexpected($pred, "surrogate code point")
    ).

%---------------------%

to_utf16_code_unit_list(String, CodeList) :-
    ( if internal_encoding_is_utf8 then
        utf8_to_utf16_code_units_loop(String, length(String), [], CodeList)
    else
        to_code_unit_list(String, CodeList)
    ).

:- pred utf8_to_utf16_code_units_loop(string::in, int::in,
    list(int)::in, list(int)::out) is det.

utf8_to_utf16_code_units_loop(String, Index, CodeList0, CodeList) :-
    ( if
        unsafe_prev_index_repl(String, Index, PrevIndex, Char, MaybeReplaced)
    then
        (
            MaybeReplaced = replaced_code_unit(_),
            unexpected($pred, "ill-formed code unit sequence")
        ;
            MaybeReplaced = not_replaced,
            ( if char.to_utf16(Char, CharCodes) then
                CodeList1 = CharCodes ++ CodeList0
            else
                unexpected($pred, "char.to_utf16 failed")
            )
        ),
        utf8_to_utf16_code_units_loop(String, PrevIndex, CodeList1, CodeList)
    else
        CodeList = CodeList0
    ).

%---------------------%

from_code_unit_list(CodeList, Str) :-
    Verify = yes,
    do_from_code_unit_list(CodeList, Verify, Str).

from_code_unit_list_allow_ill_formed(CodeList, Str) :-
    Verify = no,
    do_from_code_unit_list(CodeList, Verify, Str).

:- pred do_from_code_unit_list(list(int)::in, bool::in, string::uo) is semidet.

:- pragma foreign_proc("C",
    do_from_code_unit_list(CodeList::in, Verify::in, Str::uo),
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
        unsigned c = (unsigned) MR_list_head(list_ptr);
        // Check for null character or invalid code unit.
        if (c == 0 || c > 0xff) {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
        Str[size] = c;
        size++;
        list_ptr = MR_list_tail(list_ptr);
    }

    Str[size] = '\\0';

    if (SUCCESS_INDICATOR && Verify == MR_YES) {
        SUCCESS_INDICATOR = MR_utf8_verify(Str);
    }
").
:- pragma foreign_proc("Java",
    do_from_code_unit_list(CodeList::in, Verify::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    java.lang.StringBuilder sb = new java.lang.StringBuilder();

    SUCCESS_INDICATOR = true;

    if (Verify == bool.YES) {
        boolean prev_high = false;
        Iterable<Integer> iterable = new list.ListIterator<Integer>(CodeList);
        for (int i : iterable) {
            // Check for null character or invalid code unit.
            if (i <= 0 || i > 0xffff) {
                SUCCESS_INDICATOR = false;
                break;
            }
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
    } else {
        Iterable<Integer> iterable = new list.ListIterator<Integer>(CodeList);
        for (int i : iterable) {
            // Check for null character or invalid code unit.
            if (i <= 0 || i > 0xffff) {
                SUCCESS_INDICATOR = false;
                break;
            }
            char c = (char) i;
            sb.append(c);
        }
    }

    if (SUCCESS_INDICATOR) {
        Str = sb.toString();
    } else {
        Str = """";
    }
").
:- pragma foreign_proc("C#",
    do_from_code_unit_list(CodeList::in, Verify::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    System.Text.StringBuilder sb = new System.Text.StringBuilder();

    SUCCESS_INDICATOR = true;

    if (Verify == mr_bool.YES) {
        bool prev_high = false;
        while (!list.is_empty(CodeList)) {
            int i = (int) list.det_head(CodeList);
            // Check for null character or invalid code unit.
            if (i <= 0 || i > 0xffff) {
                SUCCESS_INDICATOR = false;
                break;
            }
            char c = (char) i;
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
    } else {
        while (!list.is_empty(CodeList)) {
            int i = (int) list.det_head(CodeList);
            // Check for null character or invalid code unit.
            if (i <= 0 || i > 0xffff) {
                SUCCESS_INDICATOR = false;
                break;
            }
            char c = (char) i;
            sb.Append(c);
            CodeList = list.det_tail(CodeList);
        }
    }

    if (SUCCESS_INDICATOR) {
        Str = sb.ToString();
    } else {
        Str = """";
    }
").

%---------------------%

from_utf8_code_unit_list(CodeList, String) :-
    ( if internal_encoding_is_utf8 then
        from_code_unit_list(CodeList, String)
    else
        decode_utf8(CodeList, [], RevChars),
        semidet_from_rev_char_list(RevChars, String)
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
        semidet_from_rev_char_list(RevChars, String)
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

:- pragma inline(pred(index/3)).
:- pragma inline(pred(det_index/3)).
:- pragma inline(pred(index_next/4)).
:- pragma inline(pred(index_next_repl/5)).
:- pragma inline(pred(unsafe_index_next/4)).
:- pragma inline(pred(unsafe_index_next_repl/5)).
:- pragma inline(pred(unsafe_index_next_repl_2/5)).
:- pragma inline(pred(prev_index/4)).
:- pragma inline(pred(prev_index_repl/5)).
:- pragma inline(pred(unsafe_prev_index/4)).
:- pragma inline(pred(unsafe_prev_index_repl/5)).
:- pragma inline(pred(unsafe_prev_index_repl_2/5)).

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
    Ch = (unsigned char) Str[Index];
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

%---------------------%

String ^ elem(Index) = det_index(String, Index).
String ^ unsafe_elem(Index) = unsafe_index(String, Index).

%---------------------%

index_next(Str, Index, NextIndex, Char) :-
    index_next_repl(Str, Index, NextIndex, Char, _MaybeReplaced).

index_next_repl(Str, Index, NextIndex, Char, MaybeReplaced) :-
    Len = length(Str),
    ( if index_check(Index, Len) then
        unsafe_index_next_repl(Str, Index, NextIndex, Char, MaybeReplaced)
    else
        fail
    ).

unsafe_index_next(Str, Index, NextIndex, Ch) :-
    unsafe_index_next_repl_2(Str, Index, NextIndex, Ch, _ReplacedCodeUnit).

unsafe_index_next_repl(Str, Index, NextIndex, Ch, MaybeReplaced) :-
    unsafe_index_next_repl_2(Str, Index, NextIndex, Ch, ReplacedCodeUnit),
    ( if ReplacedCodeUnit = -1 then
        MaybeReplaced = not_replaced
    else
        CodeUnit = uint8.cast_from_int(ReplacedCodeUnit),
        MaybeReplaced = replaced_code_unit(CodeUnit)
    ).

:- pred unsafe_index_next_repl_2(string::in, int::in, int::out, char::uo,
    int::out) is semidet.

:- pragma foreign_proc("C",
    unsafe_index_next_repl_2(Str::in, Index::in, NextIndex::out, Ch::uo,
        ReplacedCodeUnit::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Ch = (unsigned char) Str[Index];
    ReplacedCodeUnit = -1;
    if (MR_is_ascii(Ch)) {
        NextIndex = Index + 1;
        SUCCESS_INDICATOR = (Ch != 0);
    } else {
        NextIndex = Index;
        Ch = MR_utf8_get_next_mb(Str, &NextIndex);
        if (Ch < 0) {
            Ch = 0xfffd;
            ReplacedCodeUnit = (unsigned char) Str[Index];
            NextIndex = Index + 1;
        }
        SUCCESS_INDICATOR = MR_TRUE;
    }
").
:- pragma foreign_proc("C#",
    unsafe_index_next_repl_2(Str::in, Index::in, NextIndex::out, Ch::uo,
        ReplacedCodeUnit::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ReplacedCodeUnit = -1;
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
    unsafe_index_next_repl_2(Str::in, Index::in, NextIndex::out, Ch::uo,
        ReplacedCodeUnit::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ReplacedCodeUnit = -1;
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

%---------------------%

prev_index(Str, Index, PrevIndex, Char) :-
    prev_index_repl(Str, Index, PrevIndex, Char, _MaybeReplaced).

prev_index_repl(Str, Index, PrevIndex, Char, MaybeReplaced) :-
    Len = length(Str),
    ( if index_check(Index - 1, Len) then
        unsafe_prev_index_repl(Str, Index, PrevIndex, Char, MaybeReplaced)
    else
        fail
    ).

unsafe_prev_index(Str, Index, PrevIndex, Ch) :-
    unsafe_prev_index_repl_2(Str, Index, PrevIndex, Ch, _ReplacedCodeUnit).

unsafe_prev_index_repl(Str, Index, PrevIndex, Ch, MaybeReplaced) :-
    unsafe_prev_index_repl_2(Str, Index, PrevIndex, Ch, ReplacedCodeUnit),
    ( if ReplacedCodeUnit = -1 then
        MaybeReplaced = not_replaced
    else
        CodeUnit = uint8.cast_from_int(ReplacedCodeUnit),
        MaybeReplaced = replaced_code_unit(CodeUnit)
    ).

:- pred unsafe_prev_index_repl_2(string::in, int::in, int::out, char::uo,
    int::out) is semidet.

:- pragma foreign_proc("C",
    unsafe_prev_index_repl_2(Str::in, Index::in, PrevIndex::out, Ch::uo,
        ReplacedCodeUnit::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ReplacedCodeUnit = -1;
    if (Index <= 0) {
        PrevIndex = Index;
        Ch = 0;
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        PrevIndex = Index - 1;
        Ch = (unsigned char) Str[PrevIndex];
        if (! MR_is_ascii(Ch)) {
            Ch = MR_utf8_prev_get(Str, &PrevIndex);
            // XXX MR_utf8_prev_get currently just scans backwards to find a
            // lead byte, so we need a separate check to ensure no bytes are
            // unaccounted for.
            if (Ch < 0 || PrevIndex + MR_utf8_width(Ch) != Index) {
                Ch = 0xfffd;
                ReplacedCodeUnit = (unsigned char) Str[Index - 1];
                PrevIndex = Index - 1;
            }
        }
        SUCCESS_INDICATOR = MR_TRUE;
    }
").
:- pragma foreign_proc("C#",
    unsafe_prev_index_repl_2(Str::in, Index::in, PrevIndex::out, Ch::uo,
        ReplacedCodeUnit::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ReplacedCodeUnit = -1;
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
    unsafe_prev_index_repl_2(Str::in, Index::in, PrevIndex::out, Ch::uo,
        ReplacedCodeUnit::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    ReplacedCodeUnit = -1;
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

%---------------------%

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

%---------------------------------------------------------------------------%
%
% Writing characters to strings.
%

set_char(Char, Index, Str0, Str) :-
    ( if char.to_int(Char, 0) then
        unexpected($pred, "null character")
    else if
        internal_encoding_is_utf8,
        char.is_surrogate(Char)
    then
        unexpected($pred, "surrogate code point")
    else
        Len0 = length(Str0),
        ( if index_check(Index, Len0) then
            unsafe_set_char_copy_string(Char, Index, Len0, Str0, Str)
        else
            fail
        )
    ).

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

unsafe_set_char(Char, Index, Str0, Str) :-
    ( if char.to_int(Char, 0) then
        unexpected($pred, "null character")
    else if
        internal_encoding_is_utf8,
        char.is_surrogate(Char)
    then
        unexpected($pred, "surrogate code point")
    else
        Len0 = length(Str0),
        unsafe_set_char_copy_string(Char, Index, Len0, Str0, Str)
    ).

:- pred unsafe_set_char_copy_string(char, int, int, string, string).
:- mode unsafe_set_char_copy_string(in, in, in, in, uo) is det.

:- pragma foreign_proc("C",
    unsafe_set_char_copy_string(Ch::in, Index::in, Len0::in,
        Str0::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    int b;
    size_t oldlen;
    size_t oldwidth;
    size_t newwidth;
    size_t newlen;

    // The cast to (unsigned char *) is to prevent sign extension.
    b = ((const unsigned char *) Str0)[Index];
    if (MR_utf8_is_lead_byte(b)) {
        MR_Integer next_index = Index;
        int oldc = MR_utf8_get_next_mb(Str0, &next_index);
        if (oldc < 0) {
            oldwidth = 1;
        } else {
            oldwidth = next_index - Index;
        }
    } else {
        oldwidth = 1;
    }

    if (MR_is_ascii(Ch)) {
        // Fast path.
        newwidth = 1;
    } else {
        newwidth = MR_utf8_width(Ch);
    }

    oldlen = Len0;
    newlen = oldlen - oldwidth + newwidth;

    MR_allocate_aligned_string_msg(Str, newlen, MR_ALLOC_ID);
    MR_memcpy(Str, Str0, Index);
    if (MR_is_ascii(Ch)) {
        // Fast path.
        Str[Index] = Ch;
    } else {
        MR_utf8_encode(Str + Index, Ch);
    }
    MR_memcpy(Str + Index + newwidth,
        Str0 + Index + oldwidth,
        oldlen - Index - oldwidth + 1);
").
:- pragma foreign_proc("C#",
    unsafe_set_char_copy_string(Ch::in, Index::in, _Len0::in,
        Str0::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int oldwidth;
    if (System.Char.IsHighSurrogate(Str0, Index)
        && Index + 1 < Str0.Length
        && System.Char.IsLowSurrogate(Str0, Index + 1))
    {
        oldwidth = 2;
    } else {
        oldwidth = 1;
    }
    Str = Str0.Substring(0, Index)
        + System.Char.ConvertFromUtf32(Ch)
        + Str0.Substring(Index + oldwidth);
").
:- pragma foreign_proc("Java",
    unsafe_set_char_copy_string(Ch::in, Index::in, _Len0::in,
        Str0::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int oldc = Str0.codePointAt(Index);
    int oldwidth = java.lang.Character.charCount(oldc);
    Str = Str0.subSequence(0, Index)
        + new String(Character.toChars(Ch))
        + Str0.subSequence(Index + oldwidth, Str0.length());
").

%---------------------------------------------------------------------------%
%
% Determining the lengths of strings.
%

length(S) = L :-
    length(S, L).

:- pragma promise_equivalent_clauses(pred(length/2)).

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

length(Str, Len) :-
    to_code_unit_list(Str, CodeList),
    list.length(CodeList, Len0),
    Len = unsafe_promise_unique(Len0).

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

count_utf8_code_units(String) = Length :-
    ( if internal_encoding_is_utf8 then
        Length = length(String)
    else
        foldl(count_utf16_to_utf8_code_units, String, 0, Length)
    ).

:- pred count_utf16_to_utf8_code_units(char::in, int::in, int::out) is det.

count_utf16_to_utf8_code_units(Char, !Length) :-
    char.to_int(Char, CharInt),
    ( if CharInt =< 0x7f then
        !:Length = !.Length + 1
    else if char.to_utf8(Char, UTF8) then
        !:Length = !.Length + list.length(UTF8)
    else
        error($pred, "surrogate code point")
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

%---------------------%

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
    ( if unsafe_index_next_repl(String, Cur, Next, Char, MaybeReplaced) then
        MaybeReplaced = not_replaced,
        P(Char),
        all_match_loop(P, String, Next)
    else
        true
    ).

%---------------------%

contains_match(P, String) :-
    contains_match_loop(P, String, 0).

:- pred contains_match_loop(pred(char)::in(pred(in) is semidet), string::in,
    int::in) is semidet.

contains_match_loop(P, String, Cur) :-
    unsafe_index_next_repl(String, Cur, Next, Char, MaybeReplaced),
    ( if
        MaybeReplaced = not_replaced,
        P(Char)
    then
        true
    else
        contains_match_loop(P, String, Next)
    ).

%---------------------%

:- pragma foreign_proc("C",
    contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char    buf[5];
    size_t  len;
    if (MR_is_ascii(Ch)) {
        // Fast path.
        // strchr always returns true when searching for NUL,
        // but the NUL is not part of the string itself.
        SUCCESS_INDICATOR = (Ch != '\\0') && (strchr(Str, Ch) != NULL);
    } else {
        len = MR_utf8_encode(buf, Ch);
        buf[len] = '\\0';
        SUCCESS_INDICATOR = (len > 0) && (strstr(Str, buf) != NULL);
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
    contains_char_loop(String, Char, 0).

:- pred contains_char_loop(string::in, char::in, int::in) is semidet.

contains_char_loop(Str, Char, I) :-
    unsafe_index_next_repl(Str, I, J, IndexChar, MaybeReplaced),
    ( if
        MaybeReplaced = not_replaced,
        IndexChar = Char
    then
        true
    else
        contains_char_loop(Str, Char, J)
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

compare_ignore_case_ascii(Res, X, Y) :-
    LenX = length(X),
    LenY = length(Y),
    CommonLen = min(LenX, LenY),
    compare_ignore_case_ascii_loop(X, Y, 0, CommonLen, Res0),
    (
        Res0 = (=),
        compare(Res, LenX, LenY)
    ;
        ( Res0 = (<)
        ; Res0 = (>)
        ),
        Res = Res0
    ).

:- pred compare_ignore_case_ascii_loop(string::in, string::in,
    int::in, int::in, comparison_result::uo) is det.

compare_ignore_case_ascii_loop(X, Y, I, CommonLen, Res) :-
    ( if I = CommonLen then
        Res = (=)
    else
        unsafe_index_code_unit(X, I, CodeX),
        unsafe_index_code_unit(Y, I, CodeY),
        to_lower_code_unit(CodeX, LowerCodeX),
        to_lower_code_unit(CodeY, LowerCodeY),
        compare(Res0, LowerCodeX, LowerCodeY),
        (
            Res0 = (=),
            compare_ignore_case_ascii_loop(X, Y, I + 1, CommonLen, Res)
        ;
            ( Res0 = (<)
            ; Res0 = (>)
            ),
            Res = Res0
        )
    ).

%---------------------%

prefix_length(P, S) = Index :-
    prefix_length_loop(P, S, 0, Index).

:- pred prefix_length_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

prefix_length_loop(P, S, I, Index) :-
    ( if
        unsafe_index_next_repl(S, I, J, Char, not_replaced),
        P(Char)
    then
        prefix_length_loop(P, S, J, Index)
    else
        Index = I
    ).

suffix_length(P, S) = End - Index :-
    End = length(S),
    suffix_length_loop(P, S, End, Index).

:- pred suffix_length_loop(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

suffix_length_loop(P, S, I, Index) :-
    ( if
        unsafe_prev_index_repl(S, I, J, Char, not_replaced),
        P(Char)
    then
        suffix_length_loop(P, S, J, Index)
    else
        Index = I
    ).

%---------------------%

sub_string_search(WholeString, Pattern, Index) :-
    sub_string_search_start(WholeString, Pattern, 0, Index).

sub_string_search_start(WholeString, Pattern, BeginAt, Index) :-
    ( if
        (
            BeginAt = 0
        ;
            BeginAt > 0,
            BeginAt =< length(WholeString)
        )
    then
        unsafe_sub_string_search_start(WholeString, Pattern, BeginAt, Index)
    else
        fail
    ).

:- pragma foreign_proc("C",
    unsafe_sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    char *match = strstr(WholeString + BeginAt, Pattern);
    if (match) {
        Index = match - WholeString;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
}").
:- pragma foreign_proc("C#",
    unsafe_sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    Index = WholeString.IndexOf(Pattern, BeginAt,
        System.StringComparison.Ordinal);
    SUCCESS_INDICATOR = (Index >= 0);
}").
:- pragma foreign_proc("Java",
    unsafe_sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Index = WholeString.indexOf(Pattern, BeginAt);
    SUCCESS_INDICATOR = (Index >= 0);
").

unsafe_sub_string_search_start(String, SubString, BeginAt, Index) :-
    Len = length(String),
    SubLen = length(SubString),
    LastStart = Len - SubLen,
    unsafe_sub_string_search_start_loop(String, SubString, BeginAt, LastStart,
        SubLen, Index).

    % Brute force string searching. For short Strings this is good;
    % for longer strings Boyer-Moore is much better.
    %
:- pred unsafe_sub_string_search_start_loop(string::in, string::in, int::in,
    int::in, int::in, int::out) is semidet.

unsafe_sub_string_search_start_loop(String, SubString, I, LastI, SubLen, Index)
        :-
    I =< LastI,
    ( if unsafe_compare_substrings((=), String, I, SubString, 0, SubLen) then
        Index = I
    else
        unsafe_sub_string_search_start_loop(String, SubString, I + 1, LastI,
            SubLen, Index)
    ).

%---------------------------------------------------------------------------%
%
% Appending strings.
%

append(S1, S2) = S3 :-
    append(S1, S2, S3).

:- pragma promise_equivalent_clauses(pred(append/3)).

append(S1::in, S2::in, S3::in) :-
    append_iii(S1, S2, S3).
append(S1::in, S2::uo, S3::in) :-
    append_ioi(S1, S2, S3).
append(S1::in, S2::in, S3::uo) :-
    append_iio(S1, S2, S3).
append(S1::uo, S2::in, S3::in) :-
    append_oii(S1, S2, S3).

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

:- pred append_oii(string::uo, string::in, string::in) is semidet.

append_oii(S1, S2, S3) :-
    Len2 = length(S2),
    Len3 = length(S3),
    ( if
        Len2 =< Len3,
        Len1 = Len3 - Len2,
        compare_substrings((=), S3, Len1, S2, 0, Len2)
    then
        unsafe_between(S3, 0, Len1, S1)
    else
        fail
    ).

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
% creates some unnecessary garbage.
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

append_list(Strs) = Str :-
    append_list(Strs, Str).

append_list(Strs, Str) :-
    Pieces = map(make_string_piece, Strs),
    unsafe_append_string_pieces(Pieces, Str).

:- func make_string_piece(string) = string_piece.

make_string_piece(S) = substring(S, 0, length(S)).

%---------------------%
%
% We implement join_list in foreign code as the Mercury version
% creates some unnecessary garbage.
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

join_list(_Sep, []) = "".
join_list(Sep, [H | T]) = Str :-
    join_list_loop(make_string_piece(Sep), T, TailPieces),
    Pieces = [make_string_piece(H) | TailPieces],
    unsafe_append_string_pieces(Pieces, Str).

:- pred join_list_loop(string_piece::in, list(string)::in,
    list(string_piece)::out) is det.

join_list_loop(_Sep, [], []).
join_list_loop(Sep, [H | T], Pieces) :-
    join_list_loop(Sep, T, TailPieces),
    Pieces = [Sep, make_string_piece(H) | TailPieces].

%---------------------------------------------------------------------------%
%
% Making strings from smaller pieces.
%

:- type string_buffer
    --->    string_buffer(string).

:- pragma foreign_type("C", string_buffer, "char *",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", string_buffer, "char[]").
:- pragma foreign_type("Java", string_buffer, "java.lang.StringBuilder").

:- pred alloc_buffer(int::in, string_buffer::uo) is det.

:- pragma foreign_proc("C",
    alloc_buffer(Size::in, Buffer::uo),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    MR_allocate_aligned_string_msg(Buffer, Size, MR_ALLOC_ID);
    Buffer[Size] = '\\0';
").
:- pragma foreign_proc("C#",
    alloc_buffer(Size::in, Buffer::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = new char[Size];
").
:- pragma foreign_proc("Java",
    alloc_buffer(Size::in, Buffer::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = new java.lang.StringBuilder(Size);
").

alloc_buffer(_Size, Buffer) :-
    Buffer = string_buffer("").

:- pred buffer_to_string(string_buffer::di, string::uo) is det.

:- pragma foreign_proc("C",
    buffer_to_string(Buffer::di, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Str = Buffer;
").
:- pragma foreign_proc("C#",
    buffer_to_string(Buffer::di, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = new string(Buffer);
").
:- pragma foreign_proc("Java",
    buffer_to_string(Buffer::di, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = Buffer.toString();
").

buffer_to_string(Buffer, Str) :-
    Buffer = string_buffer(Str).

:- pred copy_into_buffer(string_buffer::di, string_buffer::uo,
    int::in, int::out, string::in, int::in, int::in) is det.

:- pragma foreign_proc("C",
    copy_into_buffer(Dest0::di, Dest::uo, DestOffset0::in, DestOffset::out,
        Src::in, SrcStart::in, SrcEnd::in),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    size_t count;

    MR_CHECK_EXPR_TYPE(Dest0, char *);
    MR_CHECK_EXPR_TYPE(Dest, char *);

    count = SrcEnd - SrcStart;
    Dest = Dest0;
    MR_memcpy(Dest + DestOffset0, Src + SrcStart, count);
    DestOffset = DestOffset0 + count;
").
:- pragma foreign_proc("C#",
    copy_into_buffer(Dest0::di, Dest::uo, DestOffset0::in, DestOffset::out,
        Src::in, SrcStart::in, SrcEnd::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int count = SrcEnd - SrcStart;
    Dest = Dest0;
    Src.CopyTo(SrcStart, Dest, DestOffset0, count);
    DestOffset = DestOffset0 + count;
").
:- pragma foreign_proc("Java",
    copy_into_buffer(Dest0::di, Dest::uo, DestOffset0::in, DestOffset::out,
        Src::in, SrcStart::in, SrcEnd::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // The Java implementation does not actually use the dest offsets.
    Dest = Dest0;
    Dest.append(Src, SrcStart, SrcEnd);
    DestOffset = DestOffset0 + (SrcEnd - SrcStart);
").

copy_into_buffer(Dest0, Dest, DestOffset0, DestOffset, Src, SrcStart, SrcEnd)
        :-
    Dest0 = string_buffer(Buffer0),
    Buffer = Buffer0 ++ unsafe_between(Src, SrcStart, SrcEnd),
    DestOffset = DestOffset0 + (SrcEnd - SrcStart),
    Dest = string_buffer(Buffer).

%---------------------%

append_string_pieces(Pieces, String) :-
    DoCheck = yes,
    sum_piece_lengths($pred, DoCheck, Pieces, 0, BufferLen),
    do_append_string_pieces(Pieces, BufferLen, String).

unsafe_append_string_pieces(Pieces, String) :-
    DoCheck = no,
    sum_piece_lengths($pred, DoCheck, Pieces, 0, BufferLen),
    do_append_string_pieces(Pieces, BufferLen, String).

:- pred sum_piece_lengths(string::in, bool::in, list(string_piece)::in,
    int::in, int::out) is det.

sum_piece_lengths(PredName, DoCheck, Pieces, Len0, Len) :-
    (
        Pieces = [],
        Len = Len0
    ;
        Pieces = [Piece | TailPieces],
        (
            Piece = string(Str),
            PieceLen = length(Str)
        ;
            Piece = substring(BaseStr, Start, End),
            (
                DoCheck = yes,
                BaseLen = length(BaseStr),
                ( if
                    Start >= 0,
                    Start =< BaseLen,
                    End >= Start,
                    End =< BaseLen
                then
                    true
                else
                    unexpected(PredName, "substring index out of range")
                )
            ;
                DoCheck = no
            ),
            PieceLen = End - Start
        ),
        Len1 = Len0 + PieceLen,
        sum_piece_lengths(PredName, DoCheck, TailPieces, Len1, Len)
    ).

:- pred do_append_string_pieces(list(string_piece)::in, int::in, string::uo)
    is det.

do_append_string_pieces(Pieces, BufferLen, String) :-
    alloc_buffer(BufferLen, Buffer0),
    list.foldl2(copy_piece_into_buffer, Pieces, 0, End, Buffer0, Buffer),
    expect(unify(End, BufferLen), $pred, "End != BufferLen"),
    buffer_to_string(Buffer, String).

:- pred copy_piece_into_buffer(string_piece::in, int::in, int::out,
    string_buffer::di, string_buffer::uo) is det.

copy_piece_into_buffer(Piece, !DestOffset, !DestBuffer) :-
    (
        Piece = string(Src),
        SrcStart = 0,
        SrcEnd = length(Src)
    ;
        Piece = substring(Src, SrcStart, SrcEnd)
    ),
    copy_into_buffer(!DestBuffer, !DestOffset, Src, SrcStart, SrcEnd).

%---------------------------------------------------------------------------%
%
% Splitting up strings.
%

:- pragma promise_equivalent_clauses(pred(first_char/3)).

first_char(Str::in, First::in, Rest::in) :-
    first_char_rest_in(Str, First, Rest).
first_char(Str::in, First::uo, Rest::in) :-
    first_char_rest_in(Str, First, Rest).
first_char(Str::in, First::in, Rest::uo) :-
    first_char_rest_out(Str, First, Rest).
first_char(Str::in, First::uo, Rest::uo) :-
    first_char_rest_out(Str, First, Rest).
first_char(Str::uo, First::in, Rest::in) :-
    first_char_str_out(Str, First, Rest).

:- pred first_char_rest_in(string, char, string).
:- mode first_char_rest_in(in, in, in) is semidet.
:- mode first_char_rest_in(in, uo, in) is semidet.

first_char_rest_in(Str, First, Rest) :-
    index_next_repl(Str, 0, NextIndex, First0, not_replaced),
    not is_surrogate(First0),
    unsafe_promise_unique(First0, First),
    unsafe_compare_substrings((=), Str, NextIndex, Rest, 0, length(Rest)).

:- pred first_char_rest_out(string, char, string).
:- mode first_char_rest_out(in, in, uo) is semidet.
:- mode first_char_rest_out(in, uo, uo) is semidet.

first_char_rest_out(Str, First, Rest) :-
    index_next_repl(Str, 0, NextIndex, First0, not_replaced),
    not is_surrogate(First0),
    unsafe_promise_unique(First0, First),
    unsafe_between(Str, NextIndex, length(Str), Rest).

:- pred first_char_str_out(string, char, string).
:- mode first_char_str_out(uo, in, in) is det.

first_char_str_out(Str, First, Rest) :-
    ( if char.to_int(First, 0) then
        unexpected($pred, "null character")
    else if char.is_surrogate(First) then
        unexpected($pred, "surrogate code point")
    else
        Str = char_to_string(First) ++ Rest
    ).

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

%---------------------%

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
        unsafe_index_next_repl(String, CurPos, NextPos, Char, not_replaced),
        SepP(Char)
    then
        skip_to_next_word_start(SepP, String, NextPos, NextWordStartPos)
    else
        NextWordStartPos = CurPos
    ).

    % Return the smallest NextWordStartPos >= CurPos such that
    % SepP(String[NextWordStartPos]).
    %
:- pred skip_to_word_end(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

skip_to_word_end(SepP, String, CurPos, PastWordEndPos) :-
    ( if
        unsafe_index_next_repl(String, CurPos, NextPos, Char, MaybeReplaced)
    then
        ( if
            MaybeReplaced = not_replaced,
            SepP(Char)
        then
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
    ( if unsafe_prev_index_repl(Str, CurPos, PrevPos, Char, MaybeReplaced) then
        ( if
            MaybeReplaced = not_replaced,
            DelimP(Char)
        then
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

%---------------------%

split_at_string(Separator, Str) = Segments :-
    split_at_string_loop(Separator, string.length(Separator), Str, 0,
        Segments).

:- pred split_at_string_loop(string::in, int::in, string::in, int::in,
    list(string)::out) is det.

split_at_string_loop(Separator, SeparatorLen, Str, CurPos, Segments) :-
    ( if unsafe_sub_string_search_start(Str, Separator, CurPos, SepPos) then
        HeadSegment = unsafe_between(Str, CurPos, SepPos),
        % This call is tail recursive when targeting C because we compile
        % all library modules with --optimize-constructor-last-call.
        %
        % When targeting languages other than C, that option has no effect,
        % but that is ok, because in the vast majority of cases, Str is
        % not very long.
        split_at_string_loop(Separator, SeparatorLen,
            Str, SepPos + SeparatorLen, TailSegments),
        Segments = [HeadSegment | TailSegments]
    else
        unsafe_between(Str, CurPos, string.length(Str), LastSegment),
        Segments = [LastSegment]
    ).

%---------------------%

split_into_lines(Str) = Lines :-
    split_into_lines_loop(Str, 0, [], RevLines),
    list.reverse(RevLines, Lines).

:- pred split_into_lines_loop(string::in, int::in,
    list(string)::in, list(string)::out) is det.

split_into_lines_loop(Str, CurPos, !RevLines) :-
    ( if unsafe_sub_string_search_start(Str, "\n", CurPos, SepPos) then
        Line = unsafe_between(Str, CurPos, SepPos),
        !:RevLines = [Line | !.RevLines],
        % Unlike split_at_string, split_into_lines can absolutely be expected
        % to be invoked on huge strings fairly frequently, so we want this
        % to be tail recursive even if we are not targeting C. This is why
        % this is a tail call. The price of making this a tail call is
        % the call to list.reverse in our parent.
        split_into_lines_loop(Str, SepPos + 1, !RevLines)
    else
        StrLen = string.length(Str),
        ( if CurPos = StrLen then
            true
        else
            unsafe_between(Str, CurPos, StrLen, LastLine),
            !:RevLines = [LastLine | !.RevLines]
        )
    ).

%---------------------------------------------------------------------------%
%
% Dealing with prefixes and suffixes.
%

prefix(String, Prefix) :-
    compare_substrings((=), String, 0, Prefix, 0, length(Prefix)).

suffix(String, Suffix) :-
    StringLength = length(String),
    SuffixLength = length(Suffix),
    StringStart = StringLength - SuffixLength,
    compare_substrings((=), String, StringStart, Suffix, 0, SuffixLength).

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
    append(Prefix, Suffix, String).

det_remove_suffix(String, Suffix) = Prefix :-
    ( if remove_suffix(String, Suffix, PrefixPrime) then
        Prefix = PrefixPrime
    else
        unexpected($pred, "string does not have given suffix")
    ).

remove_suffix_if_present(Suffix, String) = Out :-
    ( if remove_suffix(String, Suffix, Prefix) then
        Out = Prefix
    else
        Out = String
    ).

add_suffix(Suffix, Str) = Str ++ Suffix.

%---------------------------------------------------------------------------%
%
% Transformations of strings.
%

capitalize_first(S1) = S2 :-
    capitalize_first(S1, S2).

capitalize_first(S0, S) :-
    ( if
        unsafe_index_next(S0, 0, _NextIndex, C),
        char.to_upper(C, UpperC),
        C \= UpperC
    then
        unsafe_set_char(UpperC, 0, S0, S)
    else
        S = S0
    ).

uncapitalize_first(S1) = S2 :-
    uncapitalize_first(S1, S2).

uncapitalize_first(S0, S) :-
    ( if
        unsafe_index_next(S0, 0, _NextIndex, C),
        char.to_lower(C, LowerC),
        C \= LowerC
    then
        unsafe_set_char(LowerC, 0, S0, S)
    else
        S = S0
    ).

%---------------------%

to_upper(S1) = S2 :-
    to_upper(S1, S2).

:- pragma promise_equivalent_clauses(pred(to_upper/2)).

to_upper(StrIn::in, StrOut::uo) :-
    % Use to_code_unit_list instead of to_char_list to preserve ill-formed
    % sequences.
    to_code_unit_list(StrIn, CodeList0),
    list.map(to_upper_code_unit, CodeList0, CodeList),
    ( if from_code_unit_list_allow_ill_formed(CodeList, StrPrime) then
        StrOut = StrPrime
    else
        unexpected($pred, "string.from_code_unit_list_allow_ill_formed failed")
    ).

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

:- pragma promise_equivalent_clauses(pred(to_lower/2)).

to_lower(StrIn::in, StrOut::uo) :-
    % Use to_code_unit_list instead of to_char_list to preserve ill-formed
    % sequences.
    to_code_unit_list(StrIn, CodeList0),
    list.map(to_lower_code_unit, CodeList0, CodeList),
    ( if from_code_unit_list_allow_ill_formed(CodeList, StrPrime) then
        StrOut = StrPrime
    else
        unexpected($pred, "string.from_code_unit_list_allow_ill_formed failed")
    ).

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
    ( if prev_index(S, length(S), Index, '\n') then
        Chomp = unsafe_between(S, 0, Index)
    else
        Chomp = S
    ).

strip(S0) = S :-
    L = prefix_length(char.is_whitespace, S0),
    R = suffix_length(char.is_whitespace, S0),
    Start = L,
    End = max(L, length(S0) - R),
    S = unsafe_between(S0, Start, End).

lstrip(S) = lstrip_pred(char.is_whitespace, S).

rstrip(S) = rstrip_pred(char.is_whitespace, S).

lstrip_pred(P, S0) = S :-
    L = prefix_length(P, S0),
    S = unsafe_between(S0, L, length(S0)).

rstrip_pred(P, S0) = S :-
    R = suffix_length(P, S0),
    S = unsafe_between(S0, 0, length(S0) - R).

%---------------------%

replace(Str, Pat, Subst, Result) :-
    sub_string_search(Str, Pat, PatStart),
    Pieces = [
        substring(Str, 0, PatStart),
        substring(Subst, 0, length(Subst)),
        substring(Str, PatStart + length(Pat), length(Str))
    ],
    unsafe_append_string_pieces(Pieces, Result).

replace_all(Str, Pat, Subst) = Result :-
    replace_all(Str, Pat, Subst, Result).

replace_all(Str, Pat, Subst, Result) :-
    ( if Pat = "" then
        replace_all_empty_pat(Str, Subst, Result)
    else
        % Using substring instead of string avoids two calls to string.length
        % every time that SubstPiece appears in Pieces.
        SubstPiece = substring(Subst, 0, length(Subst)),
        replace_all_loop(Str, Pat, length(Pat), SubstPiece, 0, [], RevPieces),
        list.reverse(RevPieces, Pieces),
        unsafe_append_string_pieces(Pieces, Result)
    ).

:- pred replace_all_empty_pat(string::in, string::in, string::uo) is det.

replace_all_empty_pat(Str, Subst, Result) :-
    % This implementation is not the most efficient, but it is not expected
    % to be used much in practice.
    to_code_unit_list(Subst, SubstCodes),
    Codes0 = SubstCodes,
    replace_all_empty_pat_loop(Str, SubstCodes, length(Str), Codes0, Codes),
    ( if from_code_unit_list_allow_ill_formed(Codes, ResultPrime) then
        Result = ResultPrime
    else
        unexpected($pred, "string.from_code_unit_list_allow_ill_formed failed")
    ).

:- pred replace_all_empty_pat_loop(string::in, list(int)::in, int::in,
    list(int)::in, list(int)::out) is det.

replace_all_empty_pat_loop(Str, Subst, Index, Codes0, Codes) :-
    ( if unsafe_prev_index(Str, Index, PrevIndex, Char) then
        char.to_int(Char, CharInt),
        ( if CharInt =< 0x7f then
            % Fast path for single code unit code points.
            Codes1 = [CharInt | Codes0]
        else
            prepend_code_units(Str, PrevIndex, Index - 1, Codes0, Codes1)
        ),
        Codes2 = Subst ++ Codes1,
        replace_all_empty_pat_loop(Str, Subst, PrevIndex, Codes2, Codes)
    else
        Codes = Codes0
    ).

:- pred prepend_code_units(string::in, int::in, int::in,
    list(int)::in, list(int)::out) is det.

prepend_code_units(Str, FirstIndex, Index, Codes0, Codes) :-
    unsafe_index_code_unit(Str, Index, Code),
    Codes1 = [Code | Codes0],
    ( if Index = FirstIndex then
        Codes = Codes1
    else
        prepend_code_units(Str, FirstIndex, Index - 1, Codes1, Codes)
    ).

:- pred replace_all_loop(string::in, string::in, int::in, string_piece::in,
    int::in, list(string_piece)::in, list(string_piece)::out) is det.

replace_all_loop(Str, Pat, PatLength, SubstPiece, BeginAt,
        RevPieces0, RevPieces) :-
    ( if unsafe_sub_string_search_start(Str, Pat, BeginAt, PatStart) then
        InitialPiece = substring(Str, BeginAt, PatStart),
        RevPieces1 = [SubstPiece, InitialPiece | RevPieces0],
        replace_all_loop(Str, Pat, PatLength, SubstPiece, PatStart + PatLength,
            RevPieces1, RevPieces)
    else
        TailPiece = substring(Str, BeginAt, length(Str)),
        RevPieces = [TailPiece | RevPieces0]
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

to_int(String, Int) :-
    base_string_to_int(10, String, Int).

det_to_int(S) = det_base_string_to_int(10, S).

base_string_to_int(Base, String, Int) :-
    string.index(String, 0, Char),
    End = string.count_code_units(String),
    ( if Char = ('-') then
        End > 1,
        foldl_between(base_negative_int_accumulator(Base), String,
            1, End, 0, Int)
    else if Char = ('+') then
        End > 1,
        foldl_between(base_positive_int_accumulator(Base), String,
            1, End, 0, Int)
    else
        foldl_between(base_positive_int_accumulator(Base), String,
            0, End, 0, Int)
    ).

det_base_string_to_int(Base, S) = N :-
    ( if base_string_to_int(Base, S, N0) then
        N = N0
    else
        unexpected($pred, "conversion failed")
    ).

%---------------------%

:- func base_positive_int_accumulator(int) = pred(char, int, int).
:- mode base_positive_int_accumulator(in) =
    out(pred(in, in, out) is semidet) is det.

base_positive_int_accumulator(Base) = Pred :-
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
    else if 2 =< Base, Base =< 36 then
        Pred = accumulate_int(Base)
    else
        unexpected($pred, "base is not in the range 2 .. 36")
    ).

:- pred accumulate_int(int::in, char::in, int::in, int::out) is semidet.

accumulate_int(Base, Char, N0, N) :-
    char.unsafe_base_digit_to_int(Base, Char, M),
    N = (Base * N0) + M,
    % Fail on overflow.
    % XXX depends on undefined behaviour
    N0 =< N.

:- func base_negative_int_accumulator(int) = pred(char, int, int).
:- mode base_negative_int_accumulator(in) = out(pred(in, in, out) is semidet)
    is det.

base_negative_int_accumulator(Base) = Pred :-
    % Avoid allocating a closure for the common bases.
    ( if Base = 10 then
        Pred = accumulate_negative_int(10)
    else if Base = 16 then
        Pred = accumulate_negative_int(16)
    else if Base = 8 then
        Pred = accumulate_negative_int(8)
    else if Base = 2 then
        Pred = accumulate_negative_int(2)
    else if 2 =< Base, Base =< 36 then
        Pred = accumulate_negative_int(Base)
    else
        unexpected($pred, "base is not in the range 2 .. 36")
    ).

:- pred accumulate_negative_int(int::in, char::in,
    int::in, int::out) is semidet.

accumulate_negative_int(Base, Char, N0, N) :-
    char.unsafe_base_digit_to_int(Base, Char, M),
    N = (Base * N0) - M,
    % Fail on overflow.
    % XXX depends on undefined behaviour
    N =< N0.

%---------------------%

to_uint(String, UInt) :-
    base_string_to_uint(10, String, UInt).

det_to_uint(S) = det_base_string_to_uint(10, S).

base_string_to_uint(Base, String, UInt) :-
    End = string.count_code_units(String),
    foldl_between(base_uint_accumulator(Base), String,
        0, End, 0u, UInt).

det_base_string_to_uint(Base, S) = N :-
    ( if base_string_to_uint(Base, S, N0) then
        N = N0
    else
        unexpected($pred, "conversion failed")
    ).

%---------------------%

:- func base_uint_accumulator(int) = pred(char, uint, uint).
:- mode base_uint_accumulator(in) =
    out(pred(in, in, out) is semidet) is det.

base_uint_accumulator(Base) = Pred :-
    % Avoid allocating a closure for the common bases. A more general, but
    % finicky, way to avoid the allocation is to inline foldl_between so that
    % the higher-order calls in base_string_to_int can be specialised.
    % The redundant closures will also need to be deleted by unused argument
    % elimination.
    ( if Base = 10 then
        Pred = accumulate_uint(10u, 10)
    else if Base = 16 then
        Pred = accumulate_uint(16u, 16)
    else if Base = 8 then
        Pred = accumulate_uint(8u, 8)
    else if Base = 2 then
        Pred = accumulate_uint(2u, 2)
    else if 2 =< Base, Base =< 36 then
        Pred = accumulate_uint(uint.det_from_int(Base), Base)
    else
        unexpected($pred, "base is not in the range 2 .. 36")
    ).

:- pred accumulate_uint(uint::in, int::in, char::in, uint::in, uint::out)
    is semidet.

accumulate_uint(Base, BaseInt, Char, N0, N) :-
    char.unsafe_base_digit_to_int(BaseInt, Char, M),
    N = (Base * N0) + uint.det_from_int(M),
    % Fail on overflow.
    % XXX depends on undefined behaviour
    N0 =< N.

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

:- pragma promise_equivalent_clauses(pred(char_to_string/2)).

char_to_string(Char::in, String::uo) :-
    from_char_list([Char], String).
char_to_string(Char::out, String::in) :-
    index_next_repl(String, 0, NextIndex, Char, not_replaced),
    length(String, NextIndex).

from_char(Char) = char_to_string(Char).

%---------------------%

:- pragma foreign_proc("C",
    int_to_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[21]; // 1 for sign, 19 for digits, 1 for nul.
    sprintf(buffer, ""%"" MR_INTEGER_LENGTH_MODIFIER ""d"", I);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    int_to_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I.ToString();
").

:- pragma foreign_proc("Java",
    int_to_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(I);
").

int_to_string(N, Str) :-
    Str = int_to_string(N).

from_int(N) = int_to_string(N).

%---------------------%

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
    char buffer[21]; // 20 for digits, 1 for nul.
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

%---------------------%

uint_to_hex_string(UInt) =
    uint_to_lc_hex_string(UInt).

:- pragma inline(func(uint_to_lc_hex_string/1)).

:- pragma foreign_proc("C",
    uint_to_lc_hex_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char buffer[17]; // 16 for digits, 1 for nul.
    sprintf(buffer, ""%"" MR_INTEGER_LENGTH_MODIFIER ""x"", U);
    MR_allocate_aligned_string_msg(Str, strlen(buffer), MR_ALLOC_ID);
    strcpy(Str, buffer);
").

:- pragma foreign_proc("C#",
    uint_to_lc_hex_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = U.ToString(""x"");
").

:- pragma foreign_proc("Java",
    uint_to_lc_hex_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = java.lang.Integer.toHexString(U);
").

%---------------------%

:- pragma foreign_proc("C",
    uint_to_uc_hex_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char buffer[17]; // 16 for digits, 1 for nul.
    sprintf(buffer, ""%"" MR_INTEGER_LENGTH_MODIFIER ""X"", U);
    MR_allocate_aligned_string_msg(Str, strlen(buffer), MR_ALLOC_ID);
    strcpy(Str, buffer);
").

:- pragma foreign_proc("C#",
    uint_to_uc_hex_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = U.ToString(""X"");
").

:- pragma foreign_proc("Java",
    uint_to_uc_hex_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = java.lang.Integer.toHexString(U).toUpperCase();
").

%---------------------%

:- pragma foreign_proc("C",
    uint_to_octal_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char buffer[23]; // 22 for digits, 1 for nul.
    sprintf(buffer, ""%"" MR_INTEGER_LENGTH_MODIFIER ""o"", U);
    MR_allocate_aligned_string_msg(Str, strlen(buffer), MR_ALLOC_ID);
    strcpy(Str, buffer);
").

:- pragma foreign_proc("C#",
    uint_to_octal_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = System.Convert.ToString(U, 8);
").

:- pragma foreign_proc("Java",
    uint_to_octal_string(U::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = java.lang.Integer.toOctalString(U);
").

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

%---------------------%

:- pragma foreign_proc("C",
    uint32_to_string(U32::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int num_digits;
    if (U32 < 10) {
        num_digits = 1;
    } else if (U32 < 100) {
        num_digits = 2;
    } else if (U32 < 1000) {
        num_digits = 3;
    } else if (U32 < 10000) {
        num_digits = 4;
    } else if (U32 < 100000) {
        num_digits = 5;
    } else if (U32 < 1000000) {
        num_digits = 6;
    } else if (U32 < 10000000) {
        num_digits = 7;
    } else if (U32 < 100000000) {
        num_digits = 8;
    } else if (U32 < 1000000000) {
        num_digits = 9;
    } else {
        num_digits = 10;
    }

    MR_allocate_aligned_string_msg(S, num_digits, MR_ALLOC_ID);
    S[num_digits] = '\\0';
    int i = num_digits - 1;
    do {
        S[i] = \"0123456789\"[U32 % 10];
        i--;
        U32 /= 10;
    } while(U32 > 0);
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

%---------------------%

uint64_to_hex_string(UInt) =
    uint64_to_lc_hex_string(UInt).

:- pragma inline(func(uint64_to_lc_hex_string/1)).

:- pragma foreign_proc("C",
    uint64_to_lc_hex_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[17]; // 16 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIx64, U64);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    uint64_to_lc_hex_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U64.ToString(""x"");
").

:- pragma foreign_proc("Java",
    uint64_to_lc_hex_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toHexString(U64);
").

%---------------------%

:- pragma foreign_proc("C",
    uint64_to_uc_hex_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[17]; // 16 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIX64, U64);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    uint64_to_uc_hex_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U64.ToString(""X"");
").

:- pragma foreign_proc("Java",
    uint64_to_uc_hex_string(U64::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toHexString(U64).toUpperCase();
").

%---------------------%

:- pragma foreign_proc("C",
    uint64_to_octal_string(U64::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    char buffer[23]; // 22 for digits, 1 for nul.
    sprintf(buffer, ""%"" PRIo64, U64);
    MR_allocate_aligned_string_msg(Str, strlen(buffer), MR_ALLOC_ID);
    strcpy(Str, buffer);
").

:- pragma foreign_proc("C#",
    uint64_to_octal_string(U64::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // We need to cast to a long here since C# does not provide an overloading
    // of ToString() for ulongs.  This works since ToString() will use the
    // unsigned representation for non-decimal bases.
    Str = System.Convert.ToString((long) U64, 8);
").

:- pragma foreign_proc("Java",
    uint64_to_octal_string(U64::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = java.lang.Long.toOctalString(U64);
").

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
:- end_module string.
%---------------------------------------------------------------------------%
