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
% character is detected.  Programmers must not create strings that might
% contain null characters using the foreign language interface.
%
% When Mercury is compiled to C, strings are UTF-8 encoded, using a null
% character as the string terminator.  A single code point requires one to four
% bytes (code units) to encode.
%
% When Mercury is compiled to Java, strings are represented as Java `String's.
% When Mercury is compiled to C# code, strings are represented as
% `System.String's.  In both cases, strings are UTF-16 encoded.  A single code
% point requires one or two 16-bit integers (code units) to encode.
%
% When Mercury is compiled to Erlang, strings are represented as Erlang
% binaries using UTF-8 encoding.
%
% The builtin comparison operation on strings is also implementation dependent.
% In the current implementation, when Mercury is compiled to C, string
% comparison is implemented using C's strcmp() function.  When Mercury
% is compiled to Java, string comparison is implemented using Java's
% String.compareTo() method.  When Mercury is compiled to C#, string comparison
% is implemented using C#'s System.String.CompareOrdinal() method.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

    % This type is used for defining stream typeclass instances where the raw
    % string type would be ambiguous. A line is:
    %
    % - a possibly empty sequence of non-newline characters terminated by a
    %   newline character; or
    % - a non-empty sequence of non-newline characters terminated by the end
    %   of the file.
    %
:- type line
    --->    line(string).

    % This type is used for defining stream typeclass instances where the raw
    % string type would be ambiguous. A text file is a possibly empty sequence
    % of characters terminated by the end of file.
    %
:- type text_file
    --->    text_file(string).

    % Determine the length of a string, in code units.
    % An empty string has length zero.
    %
    % NOTE: code points (characters) are encoded using one or more code units,
    % i.e. bytes for UTF-8; 16-bit integers for UTF-16.
    %
:- func string.length(string::in) = (int::uo) is det.
:- pred string.length(string, int).
:- mode string.length(in, uo) is det.
:- mode string.length(ui, uo) is det.

    % Synonyms for string.length.
    %
:- func string.count_code_units(string) = int.
:- pred string.count_code_units(string::in, int::out) is det.

    % Determine the number of code points in a string.
    %
:- func string.count_codepoints(string) = int.
:- pred string.count_codepoints(string::in, int::out) is det.

    % Determine the number of code units required to represent a string
    % in UTF-8 encoding.
    %
:- func string.count_utf8_code_units(string) = int.

    % string.codepoint_offset(String, CodePointCount, CodePointOffset):
    % Equivalent to `string.codepoint_offset(String, 0, CodePointCount,
    % CodePointOffset)'.
    %
:- pred string.codepoint_offset(string::in, int::in, int::out) is semidet.

    % string.codepoint_offset(String, StartOffset, CodePointCount,
    %   CodePointOffset):
    %
    % Return the offset into `String' where, starting from `StartOffset',
    % `CodePointCount' code points are skipped.  Fails if either `StartOffset'
    % or `CodePointOffset' are out of range.
    %
:- pred string.codepoint_offset(string::in, int::in, int::in, int::out)
    is semidet.

    % Append two strings together.
    %
:- func string.append(string::in, string::in) = (string::uo) is det.

:- pred string.append(string, string, string).
:- mode string.append(in, in, in) is semidet.  % implied
:- mode string.append(in, uo, in) is semidet.
:- mode string.append(in, in, uo) is det.
:- mode string.append(out, out, in) is multi.
% The following mode is semidet in the sense that it doesn't succeed more
% than once - but it does create a choice-point, which means it's inefficient
% and that the compiler can't deduce that it is semidet.
% Use string.remove_suffix instead.
% :- mode string.append(out, in, in) is semidet.

    % S1 ++ S2 = S :- string.append(S1, S2, S).
    %
    % Nicer syntax.
:- func string ++ string = string.
:- mode in ++ in = uo is det.

    % string.remove_suffix(String, Suffix, Prefix):
    % The same as string.append(Prefix, Suffix, String) except that
    % this is semidet whereas string.append(out, in, in) is nondet.
    %
:- pred string.remove_suffix(string::in, string::in, string::out) is semidet.

    % string.det_remove_suffix(String, Suffix) returns the same value
    % as string.remove_suffix, except it aborts if String does not end
    % with Suffix.
    %
:- func string.det_remove_suffix(string, string) = string.

    % string.remove_suffix_if_present(Suffix, String) returns `String' minus
    % `Suffix' if `String' ends with `Suffix', `String' otherwise.
    %
:- func string.remove_suffix_if_present(string, string) = string.

    % string.remove_prefix(Prefix, String, Suffix):
    % This is a synonym for string.append(Prefix, Suffix, String) but with
    % the arguments in a more convenient order for use with higher-order code.
    %
:- pred string.remove_prefix(string::in, string::in, string::out) is semidet.

    % string.remove_prefix_if_present(Prefix, String) = Suffix returns `String'
    % minus `Prefix' if `String' begins with `Prefix', `String' otherwise.
    %
:- func string.remove_prefix_if_present(string, string) = string.

    % string.prefix(String, Prefix) is true iff Prefix is a prefix of String.
    % Same as string.append(Prefix, _, String).
    %
:- pred string.prefix(string, string).
:- mode string.prefix(in, in) is semidet.
:- mode string.prefix(in, out) is multi.

    % string.suffix(String, Suffix) is true iff Suffix is a suffix of String.
    % Same as string.append(_, Suffix, String).
    %
:- pred string.suffix(string, string).
:- mode string.suffix(in, in) is semidet.
:- mode string.suffix(in, out) is multi.

    % string.string(X): Returns a canonicalized string representation
    % of the value X using the standard Mercury operators.
    %
:- func string.string(T) = string.

    % As above, but using the supplied table of operators.
    %
:- func string.string_ops(ops.table, T) = string.

    % string.string_ops_noncanon(NonCanon, OpsTable, X, String)
    %
    % As above, but the caller specifies what behaviour should occur for
    % non-canonical terms (i.e. terms where multiple representations
    % may compare as equal):
    %
    % - `do_not_allow' will throw an exception if (any subterm of) the argument
    %    is not canonical;
    % - `canonicalize' will substitute a string indicating the presence
    %    of a non-canonical subterm;
    % - `include_details_cc' will show the structure of any non-canonical
    %   subterms, but can only be called from a committed choice context.
    %
:- pred string.string_ops_noncanon(noncanon_handling, ops.table, T, string).
:- mode string.string_ops_noncanon(in(do_not_allow), in, in, out) is det.
:- mode string.string_ops_noncanon(in(canonicalize), in, in, out) is det.
:- mode string.string_ops_noncanon(in(include_details_cc), in, in, out)
    is cc_multi.
:- mode string.string_ops_noncanon(in, in, in, out) is cc_multi.

    % string.char_to_string(Char, String).
    % Converts a character (code point) to a string or vice versa.
    %
:- func string.char_to_string(char::in) = (string::uo) is det.
:- pred string.char_to_string(char, string).
:- mode string.char_to_string(in, uo) is det.
:- mode string.char_to_string(out, in) is semidet.

    % A synonym for string.char_to_string/1.
    %
:- func string.from_char(char::in) = (string::uo) is det.

    % Convert an integer to a string.
    %
:- func string.int_to_string(int::in) = (string::uo) is det.
:- pred string.int_to_string(int::in, string::uo) is det.

    % A synonym for string.int_to_string/1.
    %
:- func string.from_int(int::in) = (string::uo) is det.

    % Convert an integer to a string with commas as thousand separators.
    %
:- func string.int_to_string_thousands(int::in) = (string::uo) is det.

    % string.int_to_base_string(Int, Base, String):
    % Convert an integer to a string in a given Base.
    % An exception is thrown if Base is not between 2 and 36.
    %
:- func string.int_to_base_string(int::in, int::in) = (string::uo) is det.
:- pred string.int_to_base_string(int::in, int::in, string::uo) is det.

    % string.int_to_base_string_group(Int, Base, GroupLength, Separator,
    %   String):
    % Convert an integer to a string in a given Base (between 2 and 36)
    % and insert Separator between every GroupLength digits.
    % If GroupLength is less than one then no separators will appear in the
    % output.  An exception is thrown if Base is not between 2 and 36.
    % Useful for formatting numbers like "1,300,000".
    %
:- func string.int_to_base_string_group(int, int, int, string) = string.
:- mode string.int_to_base_string_group(in, in, in, in) = uo is det.

    % Convert a float to a string.
    % In the current implementation the resulting float will be in the form
    % that it was printed using the format string "%#.<prec>g".
    % <prec> will be in the range p to (p+2)
    % where p = floor(mantissa_digits * log2(base_radix) / log2(10)).
    % The precision chosen from this range will be such to allow a successful
    % decimal -> binary conversion of the float.
    %
:- func string.float_to_string(float::in) = (string::uo) is det.
:- pred string.float_to_string(float::in, string::uo) is det.

    % A synonym for string.float_to_string/1.
    %
:- func string.from_float(float::in) = (string::uo) is det.

    % Convert a c_pointer to a string.  The format is "c_pointer(0xXXXX)"
    % where XXXX is the hexadecimal representation of the pointer.
    %
:- func string.c_pointer_to_string(c_pointer::in) = (string::uo) is det.
:- pred string.c_pointer_to_string(c_pointer::in, string::uo) is det.

    % A synonym for string.c_pointer_to_string/1.
    %
:- func string.from_c_pointer(c_pointer::in) = (string::uo) is det.

    % string.first_char(String, Char, Rest) is true iff Char is the first
    % character (code point) of String, and Rest is the remainder.
    %
    % WARNING: string.first_char makes a copy of Rest because the garbage
    % collector doesn't handle references into the middle of an object,
    % at least not the way we use it. Repeated use of string.first_char
    % to iterate over a string will result in very poor performance.
    % Use string.foldl or string.to_char_list instead.
    %
:- pred string.first_char(string, char, string).
:- mode string.first_char(in, in, in) is semidet.  % implied
:- mode string.first_char(in, uo, in) is semidet.  % implied
:- mode string.first_char(in, in, uo) is semidet.  % implied
:- mode string.first_char(in, uo, uo) is semidet.
:- mode string.first_char(uo, in, in) is det.

    % string.replace(String0, Search, Replace, String):
    % string.replace replaces the first occurrence of Search in String0
    % with Replace to give String. It fails if Search does not occur
    % in String0.
    %
:- pred string.replace(string::in, string::in, string::in, string::uo)
    is semidet.

    % string.replace_all(String0, Search, Replace, String):
    % string.replace_all replaces any occurrences of Search in String0
    % with Replace to give String.
    %
:- func string.replace_all(string::in, string::in, string::in) = (string::uo)
    is det.
:- pred string.replace_all(string::in, string::in, string::in, string::uo)
    is det.

    % Converts a string to lowercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func string.to_lower(string::in) = (string::uo) is det.
:- pred string.to_lower(string, string).
:- mode string.to_lower(in, uo) is det.
:- mode string.to_lower(in, in) is semidet.        % implied

    % Converts a string to uppercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func string.to_upper(string::in) = (string::uo) is det.
:- pred string.to_upper(string, string).
:- mode string.to_upper(in, uo) is det.
:- mode string.to_upper(in, in) is semidet.        % implied

    % Convert the first character (if any) of a string to uppercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func string.capitalize_first(string) = string.
:- pred string.capitalize_first(string::in, string::out) is det.

    % Convert the first character (if any) of a string to lowercase.
    % Note that this only converts unaccented Latin letters.
    %
:- func string.uncapitalize_first(string) = string.
:- pred string.uncapitalize_first(string::in, string::out) is det.

    % Convert the string to a list of characters (code points).
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: in future the same treatment may be afforded surrogate code points.
    %
:- func string.to_char_list(string) = list(char).
:- pred string.to_char_list(string, list(char)).
:- mode string.to_char_list(in, out) is det.
:- mode string.to_char_list(uo, in) is det.

    % Convert a list of characters (code points) to a string.
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: in future the same treatment may be afforded surrogate code points.
    %
:- func string.from_char_list(list(char)::in) = (string::uo) is det.
:- pred string.from_char_list(list(char), string).
:- mode string.from_char_list(in, uo) is det.
:- mode string.from_char_list(out, in) is det.

    % As above, but fail instead of throwing an exception if the
    % list contains a null character.
    %
    % NOTE: in future the same treatment may be afforded surrogate code points.
    %
:- pred string.semidet_from_char_list(list(char)::in, string::uo) is semidet.

    % Same as string.from_char_list, except that it reverses the order
    % of the characters.
    % Throws an exception if the list of characters contains a null character.
    %
    % NOTE: in future the same treatment may be afforded surrogate code points.
    %
:- func string.from_rev_char_list(list(char)::in) = (string::uo) is det.
:- pred string.from_rev_char_list(list(char)::in, string::uo) is det.

    % As above, but fail instead of throwing an exception if the
    % list contains a null character.
    %
    % NOTE: in future the same treatment may be afforded surrogate code points.
    %
:- pred string.semidet_from_rev_char_list(list(char)::in, string::uo)
    is semidet.

    % Convert a string into a list of code units.
    %
:- pred string.to_code_unit_list(string::in, list(int)::out) is det.

    % Convert a list of code units to a string.
    % Fails if the list does not contain a valid encoding of a string,
    % in the encoding expected by the current process.
    %
:- pred string.from_code_unit_list(list(int)::in, string::uo) is semidet.

    % Converts a signed base 10 string to an int; throws an exception
    % if the string argument does not match the regexp [+-]?[0-9]+
    % or the number is not in the range [int.min_int+1, int.max_int].
    %
:- func string.det_to_int(string) = int.

    % Convert a string to an int. The string must contain only digits [0-9],
    % optionally preceded by a plus or minus sign. If the string does
    % not match this syntax or the number is not in the range
    % [int.min_int+1, int.max_int], string.to_int fails.
    %
:- pred string.to_int(string::in, int::out) is semidet.

    % Convert a string in the specified base (2-36) to an int. The string
    % must contain one or more digits in the specified base, optionally
    % preceded by a plus or minus sign. For bases > 10, digits 10 to 35
    % are represented by the letters A-Z or a-z. If the string does not match
    % this syntax or the base is 10 and the number is not in the range
    % [int.min_int, int.max_int], the predicate fails.
    %
:- pred string.base_string_to_int(int::in, string::in, int::out) is semidet.

    % Converts a signed base N string to an int; throws an exception
    % if the string argument is not precisely an optional sign followed by
    % a non-empty string of base N digits and, if the base is 10, the number
    % is in the range [int.min_int, int.max_int].
    %
:- func string.det_base_string_to_int(int, string) = int.

    % Convert a string to a float. Throws an exception if the string is not
    % a syntactically correct float literal.
    %
:- func string.det_to_float(string) = float.

    % Convert a string to a float. If the string is not a syntactically correct
    % float literal, string.to_float fails.
    %
:- pred string.to_float(string::in, float::out) is semidet.

    % True if string contains only alphabetic characters [A-Za-z].
    %
:- pred string.is_all_alpha(string::in) is semidet.

    % True if string contains only alphabetic characters [A-Za-z] and
    % underscores.
    %
:- pred string.is_all_alpha_or_underscore(string::in) is semidet.

    % True if string contains only alphabetic characters [A-Za-z],
    % digits [0-9], and underscores.
    %
:- pred string.is_all_alnum_or_underscore(string::in) is semidet.

    % True if the string contains only decimal digits (0-9).
    %
:- pred string.is_all_digits(string::in) is semidet.

    % string.all_match(TestPred, String):
    %
    % True if TestPred is true when applied to each character (code point) in
    % String or if String is the empty string.
    %
:- pred string.all_match(pred(char)::in(pred(in) is semidet), string::in)
    is semidet.

    % string.pad_left(String0, PadChar, Width, String):
    % Insert `PadChar's at the left of `String0' until it is at least as long
    % as `Width', giving `String'.  Width is currently measured as the number
    % of code points.
    %
:- func string.pad_left(string, char, int) = string.
:- pred string.pad_left(string::in, char::in, int::in, string::out) is det.

    % string.pad_right(String0, PadChar, Width, String):
    % Insert `PadChar's at the right of `String0' until it is at least as long
    % as `Width', giving `String'.  Width is currently measured as the number
    % of code points.
    %
:- func string.pad_right(string, char, int) = string.
:- pred string.pad_right(string::in, char::in, int::in, string::out) is det.

    % string.duplicate_char(Char, Count, String):
    % Construct a string consisting of `Count' occurrences of `Char'
    % code points in sequence.
    %
:- func string.duplicate_char(char::in, int::in) = (string::uo) is det.
:- pred string.duplicate_char(char::in, int::in, string::uo) is det.

    % string.contains_char(String, Char):
    % Succeed if the code point `Char' occurs in `String'.
    %
:- pred string.contains_char(string::in, char::in) is semidet.

    % string.index(String, Index, Char):
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'.  Fails if `Index' is out of range (negative, or
    % greater than or equal to the length of `String').
    %
    % Calls error/1 if an illegal sequence is detected.
    %
:- pred string.index(string::in, int::in, char::uo) is semidet.

    % string.det_index(String, Index, Char):
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'.
    % Calls error/1 if `Index' is out of range (negative, or greater than
    % or equal to the length of `String'), or if an illegal sequence is
    % detected.
    %
:- func string.det_index(string, int) = char.
:- pred string.det_index(string::in, int::in, char::uo) is det.

    % A synonym for det_index/2:
    % String ^ elem(Index) = string.det_index(String, Index).
    %
:- func string ^ elem(int) = char.

    % string.unsafe_index(String, Index, Char):
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    % This version is constant time, whereas string.det_index
    % may be linear in the length of the string. Use with care!
    %
:- func string.unsafe_index(string, int) = char.
:- pred string.unsafe_index(string::in, int::in, char::uo) is det.

    % A synonym for unsafe_index/2:
    % String ^ unsafe_elem(Index) = string.unsafe_index(String, Index).
    %
:- func string ^ unsafe_elem(int) = char.

    % string.index_next(String, Index, NextIndex, Char):
    % Like `string.index'/3 but also returns the position of the code unit
    % that follows the code point beginning at `Index',
    % i.e. NextIndex = Index + num_code_units_to_encode(Char).
    %
:- pred string.index_next(string::in, int::in, int::out, char::uo) is semidet.

    % string.unsafe_index_next(String, Index, NextIndex, Char):
    % `Char' is the character (code point) in `String', beginning at the
    % code unit `Index'. `NextIndex' is the offset following the encoding
    % of `Char'. Fails if `Index' is equal to the length of `String'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than the length of `String').
    %
:- pred string.unsafe_index_next(string::in, int::in, int::out, char::uo)
    is semidet.

    % string.prev_index(String, Index, CharIndex, Char):
    % `Char' is the character (code point) in `String' immediately _before_
    % the code unit `Index'.  Fails if `Index' is out of range (non-positive,
    % or greater than the length of `String').
    %
:- pred string.prev_index(string::in, int::in, int::out, char::uo) is semidet.

    % string.unsafe_prev_index(String, Index, CharIndex, Char):
    % `Char' is the character (code point) in `String' immediately _before_
    % the code unit `Index'. `CharIndex' is the offset of the beginning of
    % `Char'. Fails if `Index' is zero.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    %
:- pred string.unsafe_prev_index(string::in, int::in, int::out, char::uo)
    is semidet.

    % string.unsafe_index_code_unit(String, Index, CodeUnit):
    % `CodeUnit' is the code unit in `String' at the offset `Index'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    %
:- pred string.unsafe_index_code_unit(string::in, int::in, int::out) is det.

    % string.chomp(String):
    % `String' minus any single trailing newline character.
    %
:- func string.chomp(string) = string.

    % string.lstrip(String):
    % `String' minus any initial whitespace characters in the ASCII range.
    %
:- func string.lstrip(string) = string.

    % string.rstrip(String):
    % `String' minus any trailing whitespace characters in the ASCII range.
    %
:- func string.rstrip(string) = string.

    % string.strip(String):
    % `String' minus any initial and trailing whitespace characters in the
    % ASCII range.
    %
:- func string.strip(string) = string.

    % string.lstrip_pred(Pred, String):
    % `String' minus the maximal prefix consisting entirely of characters
    % (code points) satisfying `Pred'.
    %
:- func string.lstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % string.rstrip_pred(Pred, String):
    % `String' minus the maximal suffix consisting entirely of characters
    % (code points) satisfying `Pred'.
    %
:- func string.rstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % string.prefix_length(Pred, String):
    % The length (in code units) of the maximal prefix of `String' consisting
    % entirely of characters (code points) satisfying Pred.
    %
:- func string.prefix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % string.suffix_length(Pred, String):
    % The length (in code units) of the maximal suffix of `String' consisting
    % entirely of characters (code points) satisfying Pred.
    %
:- func string.suffix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % string.set_char(Char, Index, String0, String):
    % `String' is `String0', with the code point beginning at code unit
    % `Index' removed and replaced by `Char'.
    % Fails if `Index' is out of range (negative, or greater than or equal to
    % the length of `String0').
    %
:- pred string.set_char(char, int, string, string).
:- mode string.set_char(in, in, in, out) is semidet.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.set_char(in, in, di, uo) is semidet.

    % string.det_set_char(Char, Index, String0, String):
    % `String' is `String0', with the code point beginning at code unit
    % `Index' removed and replaced by `Char'.
    % Calls error/1 if `Index' is out of range (negative, or greater than
    % or equal to the length of `String0').
    %
:- func string.det_set_char(char, int, string) = string.
:- pred string.det_set_char(char, int, string, string).
:- mode string.det_set_char(in, in, in, out) is det.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.det_set_char(in, in, di, uo) is det.

    % string.unsafe_set_char(Char, Index, String0, String):
    % `String' is `String0', with the code point beginning at code unit
    % `Index' removed and replaced by `Char'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String0').
    % This version is constant time, whereas string.det_set_char
    % may be linear in the length of the string. Use with care!
    %
:- func string.unsafe_set_char(char, int, string) = string.
:- mode string.unsafe_set_char(in, in, in) = out is det.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.unsafe_set_char(in, in, di) = uo is det.
:- pred string.unsafe_set_char(char, int, string, string).
:- mode string.unsafe_set_char(in, in, in, out) is det.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.unsafe_set_char(in, in, di, uo) is det.

    % string.foldl(Closure, String, !Acc):
    % `Closure' is an accumulator predicate which is to be called for each
    % character (code point) of the string `String' in turn. The initial
    % value of the accumulator is `!.Acc' and the final value is `!:Acc'.
    % (string.foldl is equivalent to
    %   string.to_char_list(String, Chars),
    %   list.foldl(Closure, Chars, !Acc)
    % but is implemented more efficiently.)
    %
:- func string.foldl(func(char, A) = A, string, A) = A.
:- pred string.foldl(pred(char, A, A), string, A, A).
:- mode string.foldl(pred(in, di, uo) is det, in, di, uo) is det.
:- mode string.foldl(pred(in, in, out) is det, in, in, out) is det.
:- mode string.foldl(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode string.foldl(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode string.foldl(pred(in, in, out) is multi, in, in, out) is multi.

    % string.foldl2(Closure, String, !Acc1, !Acc2):
    % A variant of string.foldl with two accumulators.
    %
:- pred string.foldl2(pred(char, A, A, B, B), string, A, A, B, B).
:- mode string.foldl2(pred(in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode string.foldl2(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode string.foldl2(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode string.foldl2(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode string.foldl2(pred(in, in, out, in, out) is nondet,
    in, in, out, in, out) is nondet.
:- mode string.foldl2(pred(in, in, out, in, out) is multi,
    in, in, out, in, out) is multi.

    % string.foldl_between(Closure, String, Start, End, !Acc)
    % is equivalent to string.foldl(Closure, SubString, !Acc)
    % where SubString = string.between(String, Start, End).
    %
    % `Start' and `End' are in terms of code units.
    %
:- func string.foldl_between(func(char, A) = A, string, int, int, A) = A.
:- pred string.foldl_between(pred(char, A, A), string, int, int, A, A).
:- mode string.foldl_between(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldl_between(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldl_between(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldl_between(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldl_between(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % string.foldl2_between(Closure, String, Start, End, !Acc1, !Acc2)
    % A variant of string.foldl_between with two accumulators.
    %
    % `Start' and `End' are in terms of code units.
    %
:- pred string.foldl2_between(pred(char, A, A, B, B),
    string, int, int, A, A, B, B).
:- mode string.foldl2_between(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode string.foldl2_between(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode string.foldl2_between(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode string.foldl2_between(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode string.foldl2_between(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode string.foldl2_between(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

    % string.foldr(Closure, String, !Acc):
    % As string.foldl/4, except that processing proceeds right-to-left.
    %
:- func string.foldr(func(char, T) = T, string, T) = T.
:- pred string.foldr(pred(char, T, T), string, T, T).
:- mode string.foldr(pred(in, in, out) is det, in, in, out) is det.
:- mode string.foldr(pred(in, di, uo) is det, in, di, uo) is det.
:- mode string.foldr(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode string.foldr(pred(in, in, out) is nondet, in, in, out) is nondet.
:- mode string.foldr(pred(in, in, out) is multi, in, in, out) is multi.

    % string.foldr_between(Closure, String, Start, End, !Acc)
    % is equivalent to string.foldr(Closure, SubString, !Acc)
    % where SubString = string.between(String, Start, End).
    %
    % `Start' and `End' are in terms of code units.
    %
:- func string.foldr_between(func(char, T) = T, string, int, int, T) = T.
:- pred string.foldr_between(pred(char, T, T), string, int, int, T, T).
:- mode string.foldr_between(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldr_between(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldr_between(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldr_between(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldr_between(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % string.foldl_substring(Closure, String, Start, Count, !Acc)
    % Please use string.foldl_between instead.
    %
:- pragma obsolete(string.foldl_substring/5).
:- pragma obsolete(string.foldl_substring/6).
:- func string.foldl_substring(func(char, A) = A, string, int, int, A) = A.
:- pred string.foldl_substring(pred(char, A, A), string, int, int, A, A).
:- mode string.foldl_substring(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldl_substring(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldl_substring(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldl_substring(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldl_substring(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % string.foldl2_substring(Closure, String, Start, Count, !Acc1, !Acc2)
    % Please use string.foldl2_between instead.
    %
:- pragma obsolete(string.foldl2_substring/8).
:- pred string.foldl2_substring(pred(char, A, A, B, B),
    string, int, int, A, A, B, B).
:- mode string.foldl2_substring(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode string.foldl2_substring(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode string.foldl2_substring(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode string.foldl2_substring(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode string.foldl2_substring(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode string.foldl2_substring(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

    % string.foldr_substring(Closure, String, Start, Count, !Acc)
    % Please use string.foldr_between instead.
    %
:- pragma obsolete(string.foldr_substring/5).
:- pragma obsolete(string.foldr_substring/6).
:- func string.foldr_substring(func(char, T) = T, string, int, int, T) = T.
:- pred string.foldr_substring(pred(char, T, T), string, int, int, T, T).
:- mode string.foldr_substring(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldr_substring(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldr_substring(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldr_substring(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldr_substring(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

    % string.words_separator(SepP, String) returns the list of non-empty
    % substrings of String (in first to last order) that are delimited
    % by non-empty sequences of characters (code points) matched by SepP.
    % For example,
    %
    % string.words_separator(char.is_whitespace, " the cat  sat on the  mat") =
    %   ["the", "cat", "sat", "on", "the", "mat"]
    %
    % Note the difference to string.split_at_separator.
    %
:- func string.words_separator(pred(char), string) = list(string).
:- mode string.words_separator(pred(in) is semidet, in) = out is det.

    % string.words(String) =
    %   string.words_separator(char.is_whitespace, String).
    %
:- func string.words(string) = list(string).

    % string.split_at_separator(SepP, String) returns the list of
    % substrings of String (in first to last order) that are delimited
    % by characters (code points) matched by SepP. For example,
    %
    % string.split_at_separator(char.is_whitespace, " a cat  sat on the  mat")
    %   = ["", "a", "cat", "", "sat", "on", "the", "", "mat"]
    %
    % Note the difference to string.words_separator.
    %
:- func string.split_at_separator(pred(char), string) = list(string).
:- mode string.split_at_separator(pred(in) is semidet, in) = out is det.

    % string.split_at_char(Char, String) =
    %     string.split_at_separator(unify(Char), String)
    %
:- func string.split_at_char(char, string) = list(string).

    % string.split_at_string(Separator, String) returns the list of substrings
    % of String that are delimited by Separator. For example,
    %
    % string.split_at_string("|||", "|||fld2|||fld3") = ["", "fld2", [fld3"]
    %
    % Always the first match of Separator is used to break the String, for
    % example: string.split_at_string("aa", "xaaayaaaz") = ["x", "ay", "az"]
    %
:- func string.split_at_string(string, string) = list(string).

    % string.split(String, Index, LeftSubstring, RightSubstring):
    % Split a string into two substrings, at the code unit `Index'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred string.split(string::in, int::in, string::out, string::out) is det.

    % string.split_by_codepoint(String, Count, LeftSubstring, RightSubstring):
    % `LeftSubstring' is the left-most `Count' characters (code points) of
    % `String', and `RightSubstring' is the remainder of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred string.split_by_codepoint(string::in, int::in, string::out, string::out)
    is det.

    % string.left(String, Count, LeftSubstring):
    % `LeftSubstring' is the left-most `Count' code _units_ of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func string.left(string::in, int::in) = (string::out) is det.
:- pred string.left(string::in, int::in, string::out) is det.

    % string.left_by_codepoint(String, Count, LeftSubstring):
    % `LeftSubstring' is the left-most `Count' characters (code points) of
    % `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func string.left_by_codepoint(string::in, int::in) = (string::out) is det.
:- pred string.left_by_codepoint(string::in, int::in, string::out) is det.

    % string.right(String, Count, RightSubstring):
    % `RightSubstring' is the right-most `Count' code _units_ of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func string.right(string::in, int::in) = (string::out) is det.
:- pred string.right(string::in, int::in, string::out) is det.

    % string.right_by_codepoint(String, Count, RightSubstring):
    % `RightSubstring' is the right-most `Count' characters (code points) of
    % `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func string.right_by_codepoint(string::in, int::in) = (string::out) is det.
:- pred string.right_by_codepoint(string::in, int::in, string::out) is det.

    % string.between(String, Start, End, Substring):
    % `Substring' consists of the segment of `String' within the half-open
    % interval [Start, End), where `Start' and `End' are code unit offsets.
    % (If `Start' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.
    % If `End' is out of the range [`Start', length of `String'],
    % it is treated as if it were the nearest end-point of that range.)
    %
:- func string.between(string::in, int::in, int::in) = (string::uo) is det.
:- pred string.between(string::in, int::in, int::in, string::uo) is det.

    % string.substring(String, Start, Count, Substring):
    % Please use string.between instead.
    %
:- pragma obsolete(string.substring/3).
:- pragma obsolete(string.substring/4).
:- func string.substring(string::in, int::in, int::in) = (string::uo) is det.
:- pred string.substring(string::in, int::in, int::in, string::uo) is det.

    % string.between_codepoints(String, Start, End, Substring):
    % `Substring' is the part of `String' between the code point positions
    % `Start' and `End'.
    % (If `Start' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.
    % If `End' is out of the range [`Start', length of `String'],
    % it is treated as if it were the nearest end-point of that range.)
    %
:- func string.between_codepoints(string::in, int::in, int::in)
    = (string::uo) is det.
:- pred string.between_codepoints(string::in, int::in, int::in, string::uo)
    is det.

    % string.unsafe_between(String, Start, End, Substring):
    % `Substring' consists of the segment of `String' within the half-open
    % interval [Start, End), where `Start' and `End' are code unit offsets.
    % WARNING: if `Start' is out of the range [0, length of `String'] or
    % `End' is out of the range [`Start', length of `String']
    % then the behaviour is UNDEFINED. Use with care!
    % This version takes time proportional to the length of the substring,
    % whereas string.substring may take time proportional to the length
    % of the whole string.
    %
:- func string.unsafe_between(string::in, int::in, int::in) = (string::uo)
    is det.
:- pred string.unsafe_between(string::in, int::in, int::in, string::uo)
    is det.

    % string.unsafe_substring(String, Start, Count, Substring):
    % Please use string.unsafe_between instead.
    %
:- pragma obsolete(string.unsafe_substring/3).
:- pragma obsolete(string.unsafe_substring/4).
:- func string.unsafe_substring(string::in, int::in, int::in) = (string::uo)
    is det.
:- pred string.unsafe_substring(string::in, int::in, int::in, string::uo)
    is det.

    % Append a list of strings together.
    %
:- func string.append_list(list(string)::in) = (string::uo) is det.
:- pred string.append_list(list(string)::in, string::uo) is det.

    % string.join_list(Separator, Strings) = JoinedString:
    % Appends together the strings in Strings, putting Separator between
    % adjacent strings. If Strings is the empty list, returns the empty string.
    %
:- func string.join_list(string::in, list(string)::in) = (string::uo) is det.

    % Compute a hash value for a string.
    %
:- func string.hash(string) = int.
:- pred string.hash(string::in, int::out) is det.

    % Two other hash functions for strings.
    %
:- func string.hash2(string) = int.
:- func string.hash3(string) = int.

    % string.sub_string_search(String, SubString, Index).
    % `Index' is the code unit position in `String' where the first
    % occurrence of `SubString' begins. Indices start at zero, so if
    % `SubString' is a prefix of `String', this will return Index = 0.
    %
:- pred string.sub_string_search(string::in, string::in, int::out) is semidet.

    % string.sub_string_search_start(String, SubString, BeginAt, Index).
    % `Index' is the code unit position in `String' where the first
    % occurrence of `SubString' occurs such that 'Index' is greater than or
    % equal to `BeginAt'.  Indices start at zero.
    %
:- pred string.sub_string_search_start(string::in, string::in, int::in,
    int::out) is semidet.

    % A function similar to sprintf() in C.
    %
    % For example,
    %   string.format("%s %i %c %f\n",
    %       [s("Square-root of"), i(2), c('='), f(1.41)], String)
    % will return
    %   String = "Square-root of 2 = 1.41\n".
    %
    % The following options available in C are supported: flags [0+-# ],
    % a field width (or *), and a precision (could be a ".*").
    %
    % Valid conversion character types are {dioxXucsfeEgGp%}. %n is not
    % supported. string.format will not return the length of the string.
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
    % A '-' will cause the output to be left-justified in its % 'space'.
    % (With a `-', the default is for fields to be right-justified.)
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
    % The implementation uses the sprintf() function in C grades, so the actual
    % output will depend on the C standard library.
    %
:- func string.format(string, list(string.poly_type)) = string.
:- pred string.format(string::in, list(string.poly_type)::in, string::out)
    is det.

:- type string.poly_type
    --->    f(float)
    ;       i(int)
    ;       s(string)
    ;       c(char).

    % format_table(Columns, Separator) = Table
    % format_table/2 takes a list of columns and a column separator and returns
    % a formatted table, where each field in each column has been aligned
    % and fields are separated with Separator. A newline character is inserted
    % between each row. If the columns are not all the same length then
    % an exception is thrown. Lengths are currently measured in terms of code
    % points.
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
:- func string.format_table(list(justified_column), string) = string.

    % format_table_max(Columns, Separator) does the same job as format_table,
    % but allows the caller to associate an maximum width with each column.
    %
:- func string.format_table_max(assoc_list(justified_column, maybe(int)),
    string) = string.

:- type justified_column
    --->    left(list(string))
    ;       right(list(string)).

    % word_wrap(Str, N) = Wrapped.
    % Wrapped is Str with newlines inserted between words (separated by ASCII
    % space characters) so that at most N code points appear on a line and each
    % line contains as many whole words as possible. If any one word exceeds N
    % code point in length then it will be broken over two (or more) lines.
    % Sequences of whitespace characters are replaced by a single space.
    %
:- func string.word_wrap(string, int) = string.

    % word_wrap_separator(Str, N, WordSeparator) = Wrapped.
    % word_wrap_separator/3 is like word_wrap/2, except that words that
    % need to be broken up over multiple lines have WordSeparator inserted
    % between each piece. If the length of WordSeparator is greater than
    % or equal to N code points, then no separator is used.
    %
:- func string.word_wrap_separator(string, int, string) = string.

    % Convert a string to a pretty_printer.doc for formatting.
    %
:- func string.string_to_doc(string) = pretty_printer.doc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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
:- import_module type_desc.
:- import_module univ.
:- import_module version_array.

:- use_module rtti_implementation.
:- use_module term_io.

%-----------------------------------------------------------------------------%

string.replace(Str, Pat, Subst, Result) :-
    sub_string_search(Str, Pat, Index),

    Initial = string.unsafe_between(Str, 0, Index),

    BeginAt = Index + string.length(Pat),
    EndAt = string.length(Str),
    Final = string.unsafe_between(Str, BeginAt, EndAt),

    Result = string.append_list([Initial, Subst, Final]).

string.replace_all(Str, Pat, Subst, Result) :-
    ( Pat = "" ->
        F = (func(C, L) = [char_to_string(C) ++ Subst | L]),
        Foldl = string.foldl(F, Str, []),
        Result = append_list([Subst | list.reverse(Foldl)])
    ;
        PatLength = string.length(Pat),
        ReversedChunks = replace_all_2(Str, Pat, Subst, PatLength, 0, []),
        Chunks = list.reverse(ReversedChunks),
        Result = string.append_list(Chunks)
    ).

:- func string.replace_all_2(string, string, string, int, int, list(string))
    = list(string).

string.replace_all_2(Str, Pat, Subst, PatLength, BeginAt, Result0) = Result :-
    ( sub_string_search_start(Str, Pat, BeginAt, Index) ->
        Initial = string.unsafe_between(Str, BeginAt, Index),
        Start = Index + PatLength,
        Result = string.replace_all_2(Str, Pat, Subst, PatLength, Start,
            [Subst, Initial | Result0])
    ;
        EndString = string.unsafe_between(Str, BeginAt, length(Str)),
        Result = [EndString | Result0]
    ).

string.to_int(String, Int) :-
    string.base_string_to_int(10, String, Int).

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

:- pred accumulate_int(int::in, char::in, int::in, int::out) is semidet.

accumulate_int(Base, Char, N0, N) :-
    char.digit_to_int(Char, M),
    M < Base,
    N = (Base * N0) + M,
    ( N0 =< N ; Base \= 10 ).           % Fail on overflow for base 10 numbers.

:- pred accumulate_negative_int(int::in, char::in,
    int::in, int::out) is semidet.

accumulate_negative_int(Base, Char, N0, N) :-
    char.digit_to_int(Char, M),
    M < Base,
    N = (Base * N0) - M,
    ( N =< N0 ; Base \= 10 ).       % Fail on underflow for base 10 numbers.

% It is important to inline string.index and string.det_index.
% so that the compiler can do loop invariant hoisting
% on calls to string.length that occur in loops.
:- pragma inline(string.det_index/3).

string.det_index(String, Int, Char) :-
    ( string.index(String, Int, Char0) ->
        Char = Char0
    ;
        error("string.det_index: index out of range")
    ).

String ^ elem(Index) = det_index(String, Index).

string.det_set_char(Char, Int, String0, String) :-
    ( string.set_char(Char, Int, String0, String1) ->
        String = String1
    ;
        error("string.det_set_char: index out of range")
    ).

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

string.foldr(F, String, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y)),
    string.foldr(Closure, String, Acc0, Acc).

string.foldr_between(F, String, Start, Count, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldr_between(Closure, String, Start, Count, Acc0, Acc).

string.foldr(Closure, String, Acc0, Acc) :-
    string.foldr_between(Closure, String, 0, length(String), Acc0, Acc).

string.foldr_between(Closure, String, Start0, End0, Acc0, Acc) :-
    Start = max(0, Start0),
    End = min(End0, length(String)),
    string.foldr_between_2(Closure, String, Start, End, Acc0, Acc).

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

string.foldl_substring(F, String, Start, Count, Acc0) = Acc :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    Acc = string.foldl_between(F, String, ClampStart, ClampEnd, Acc0).

string.foldl_substring(Closure, String, Start, Count, !Acc) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.foldl_between(Closure, String, ClampStart, ClampEnd, !Acc).

string.foldl2_substring(Closure, String, Start, Count, !Acc1, !Acc2) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.foldl2_between(Closure, String, ClampStart, ClampEnd, !Acc1, !Acc2).

string.foldr_substring(F, String, Start, Count, Acc0) = Acc :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    Acc = string.foldr_between(F, String, ClampStart, ClampEnd, Acc0).

string.foldr_substring(Closure, String, Start, Count, !Acc) :-
    convert_endpoints(Start, Count, ClampStart, ClampEnd),
    string.foldr_between(Closure, String, ClampStart, ClampEnd, !Acc).

string.left(String, Count, LeftString) :-
    string.split(String, Count, LeftString, _RightString).

string.left_by_codepoint(String, Count) = LeftString :-
    string.left_by_codepoint(String, Count, LeftString).

string.left_by_codepoint(String, Count, LeftString) :-
    string.split_by_codepoint(String, Count, LeftString, _RightString).

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

string.remove_prefix(Prefix, String, Suffix) :-
    string.append(Prefix, Suffix, String).

string.remove_prefix_if_present(Prefix, String) = Out :-
    ( string.remove_prefix(Prefix, String, Suffix) ->
        Out = Suffix
    ;
        Out = String
    ).

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

string.char_to_string(Char, String) :-
    string.to_char_list(String, [Char]).

string.from_char(Char) = string.char_to_string(Char).

string.int_to_string(N, Str) :-
    string.int_to_base_string(N, 10, Str).

string.from_int(N) = string.int_to_string(N).

string.int_to_base_string(N, Base, Str) :-
    (
        Base >= 2,
        Base =< 36
    ->
        true
    ;
        error("string.int_to_base_string: invalid base")
    ),
    string.int_to_base_string_1(N, Base, Str).

:- pred string.int_to_base_string_1(int::in, int::in, string::uo) is det.

string.int_to_base_string_1(N, Base, Str) :-
    % Note that in order to handle MININT correctly, we need to do the
    % conversion of the absolute number into digits using negative numbers
    % (we can't use positive numbers, since -MININT overflows)
    ( N < 0 ->
        string.int_to_base_string_2(N, Base, ['-'], RevChars)
    ;
        N1 = 0 - N,
        string.int_to_base_string_2(N1, Base, [], RevChars)
    ),
    string.from_rev_char_list(RevChars, Str).

:- pred string.int_to_base_string_2(int::in, int::in,
    list(char)::in, list(char)::out) is det.

    % string.int_to_base_string_2/3 is almost identical to
    % string.int_to_base_string_group_2/6 below so any changes here might
    % also need to be applied to string.int_to_base_string_group_2/3.
    %
string.int_to_base_string_2(NegN, Base, !RevChars) :-
    ( NegN > -Base ->
        N = -NegN,
        char.det_int_to_digit(N, DigitChar),
        !:RevChars = [DigitChar | !.RevChars]
    ;
        NegN1 = NegN // Base,
        N10 = (NegN1 * Base) - NegN,
        char.det_int_to_digit(N10, DigitChar),
        string.int_to_base_string_2(NegN1, Base, !RevChars),
        !:RevChars = [DigitChar | !.RevChars]
    ).

string.c_pointer_to_string(C_Pointer, Str) :-
    private_builtin.unsafe_type_cast(C_Pointer, Int),
    Str = "c_pointer(0x" ++ string.int_to_base_string(Int, 16) ++ ")".

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

string.int_to_string_thousands(N) =
    string.int_to_base_string_group(N, 10, 3, ",").

    % Period is how many digits should be between each separator.
    %
string.int_to_base_string_group(N, Base, Period, Sep) = Str :-
    (
        Base >= 2,
        Base =< 36
    ->
        true
    ;
        error("string.int_to_base_string_group: invalid base")
    ),
    string.int_to_base_string_group_1(N, Base, Period, Sep, Str).

:- pred string.int_to_base_string_group_1(int::in, int::in, int::in,
    string::in, string::uo) is det.

    % Period is how many digits should be between each separator.
    %
string.int_to_base_string_group_1(N, Base, Period, Sep, Str) :-
    % Note that in order to handle MININT correctly, we need to do
    % the conversion of the absolute number into digits using negative numbers
    % (we can't use positive numbers, since -MININT overflows)
    ( N < 0 ->
        string.int_to_base_string_group_2(N, Base, 0, Period, Sep, Str1),
        string.append("-", Str1, Str)
    ;
        N1 = 0 - N,
        string.int_to_base_string_group_2(N1, Base, 0, Period, Sep, Str)
    ).

:- pred string.int_to_base_string_group_2(int::in, int::in, int::in, int::in,
    string::in, string::uo) is det.

    % Period is how many digits should be between each separator.
    % Curr is how many digits have been processed since the last separator
    % was inserted.
    % string.int_to_base_string_group_2/6 is almost identical to
    % string.int_to_base_string_2/3 above so any changes here might also
    % need to be applied to string.int_to_base_string_2/3.
    %
string.int_to_base_string_group_2(NegN, Base, Curr, Period, Sep, Str) :-
    (
        Curr = Period,
        Period > 0
    ->
        string.int_to_base_string_group_2(NegN, Base, 0, Period, Sep, Str1),
        string.append(Str1, Sep, Str)
    ;
        ( NegN > -Base ->
            N = -NegN,
            char.det_int_to_digit(N, DigitChar),
            string.char_to_string(DigitChar, Str)
        ;
            NegN1 = NegN // Base,
            N10 = (NegN1 * Base) - NegN,
            char.det_int_to_digit(N10, DigitChar),
            string.char_to_string(DigitChar, DigitString),
            string.int_to_base_string_group_2(NegN1, Base, Curr + 1, Period,
                Sep, Str1),
            string.append(Str1, DigitString, Str)
        )
    ).

/*-----------------------------------------------------------------------*/

% :- pred string.to_char_list(string, list(char)).
% :- mode string.to_char_list(in, uo) is det.
% :- mode string.to_char_list(uo, in) is det.
:- pragma promise_equivalent_clauses(string.to_char_list/2).

string.to_char_list(Str::in, CharList::out) :-
    string.to_char_list_2(Str, CharList).
string.to_char_list(Str::uo, CharList::in) :-
    string.from_char_list(CharList, Str).

:- pred string.to_char_list_2(string, list(char)).
:- mode string.to_char_list_2(in, out) is det.

:- pragma foreign_proc("C",
    string.to_char_list_2(Str::in, CharList::out),
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
    string.to_char_list_2(Str::in, CharList::out),
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
    string.to_char_list_2(Str::in, CharList::out),
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
    string.to_char_list_2(Str::in, CharList::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    CharList = unicode:characters_to_list(Str)
").

string.to_char_list_2(Str, CharList) :-
    string.foldr(list.cons, Str, [], CharList).

%-----------------------------------------------------------------------------%

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
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    while (!list.is_empty(CharList)) {
        int cp = (int) list.det_head(CharList);
        if (cp <= 0xffff) {
            sb.Append((char) cp);
        } else {
            sb.Append(System.Char.ConvertFromUtf32(cp));
        }
        CharList = list.det_tail(CharList);
    }
    Str = sb.ToString();
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Java",
    string.semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    java.lang.StringBuilder sb = new StringBuilder();
    Iterable<Integer> iterable = new list.ListIterator<Integer>(CharList);
    for (int c : iterable) {
        if (c <= 0xffff) {
            /* Fast path. */
            sb.append((char) c);
        } else {
            sb.append(java.lang.Character.toChars(c));
        }
    }
    Str = sb.toString();
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Erlang",
    string.semidet_from_char_list(CharList::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Str = unicode:characters_to_binary(CharList),
    SUCCESS_INDICATOR = true
").

:- pragma promise_equivalent_clauses(string.semidet_from_char_list/2).

string.semidet_from_char_list(CharList::in, Str::uo) :-
    (
        CharList = [],
        Str = ""
    ;
        CharList = [C | Cs],
        \+ char.to_int(C, 0),
        string.semidet_from_char_list(Cs, Str0),
        string.first_char(Str, C, Str0)
    ).

%---------------------------------------------------------------------------%

% We could implement from_rev_char_list using list.reverse and from_char_list,
% but the optimized implementation in C below is there for efficiency since
% it improves the overall speed of parsing by about 7%.

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
    while (!list.is_empty(list_ptr)) {
        int c = (int) list.det_head(list_ptr);
        if (c <= 0xffff) {
            arr[--size] = (char) c;
        } else {
            string s = System.Char.ConvertFromUtf32(c);
            arr[--size] = s[1];
            arr[--size] = s[0];
        }
        list_ptr = list.det_tail(list_ptr);
    }

    Str = new string(arr);
    SUCCESS_INDICATOR = true;
").

string.semidet_from_rev_char_list(Chars::in, Str::uo) :-
    string.semidet_from_char_list(list.reverse(Chars), Str).

%---------------------------------------------------------------------------%

string.to_code_unit_list(String, List) :-
    string.to_code_unit_list_2(String, 0, string.length(String), List).

:- pred string.to_code_unit_list_2(string::in, int::in, int::in,
    list(int)::out) is det.

string.to_code_unit_list_2(String, Index, End, List) :-
    ( Index >= End ->
        List = []
    ;
        string.unsafe_index_code_unit(String, Index, Code),
        string.to_code_unit_list_2(String, Index + 1, End, Tail),
        List = [Code | Tail]
    ).

%-----------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

string.to_upper(StrIn, StrOut) :-
    string.to_char_list(StrIn, List),
    string.char_list_to_upper(List, ListUpp),
    string.from_char_list(ListUpp, StrOut).

:- pred string.char_list_to_upper(list(char)::in, list(char)::out) is det.

string.char_list_to_upper([], []).
string.char_list_to_upper([X | Xs], [Y | Ys]) :-
    char.to_upper(X, Y),
    string.char_list_to_upper(Xs, Ys).

string.to_lower(StrIn, StrOut) :-
    string.to_char_list(StrIn, List),
    string.char_list_to_lower(List, ListLow),
    string.from_char_list(ListLow, StrOut).

:- pred string.char_list_to_lower(list(char)::in, list(char)::out) is det.

string.char_list_to_lower([], []).
string.char_list_to_lower([X | Xs], [Y | Ys]) :-
    char.to_lower(X, Y),
    string.char_list_to_lower(Xs, Ys).

string.capitalize_first(S0, S) :-
    ( string.first_char(S0, C, S1) ->
        char.to_upper(C, UpperC),
        string.first_char(S, UpperC, S1)
    ;
        S = S0
    ).

string.uncapitalize_first(S0, S) :-
    ( string.first_char(S0, C, S1) ->
        char.to_lower(C, LowerC),
        string.first_char(S, LowerC, S1)
    ;
        S = S0
    ).

string.all_match(P, String) :-
    all_match_2(P, String, 0).

:- pred all_match_2(pred(char)::in(pred(in) is semidet), string::in, int::in)
    is semidet.

string.all_match_2(P, String, I) :-
    ( string.unsafe_index_next(String, I, J, Char) ->
        P(Char),
        string.all_match_2(P, String, J)
    ;
        true
    ).

string.is_all_alpha(S) :-
    string.all_match(char.is_alpha, S).

    % The C version is faster than the Mercury version.
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

string.is_all_alpha_or_underscore(S) :-
    string.all_match(char.is_alpha_or_underscore, S).

    % The C version is faster than the Mercury version.
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

string.is_all_alnum_or_underscore(S) :-
    string.all_match(char.is_alnum_or_underscore, S).

    % The C version is faster than the Mercury version.
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

string.is_all_digits(S) :-
    string.all_match(char.is_digit, S).

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

string.pad_left(String0, PadChar, Width, String) :-
    string.count_codepoints(String0, Length),
    ( Length < Width ->
        Count = Width - Length,
        string.duplicate_char(PadChar, Count, PadString),
        string.append(PadString, String0, String)
    ;
        String = String0
    ).

string.pad_right(String0, PadChar, Width, String) :-
    string.count_codepoints(String0, Length),
    ( Length < Width ->
        Count = Width - Length,
        string.duplicate_char(PadChar, Count, PadString),
        string.append(String0, PadString, String)
    ;
        String = String0
    ).

string.duplicate_char(Char, Count, String) :-
    String = string.from_char_list(list.duplicate(Count, Char)).

%-----------------------------------------------------------------------------%

string.append_list(Lists, string.append_list(Lists)).

    % We implement string.append_list in C as this minimises
    % the amount of garbage created.
:- pragma foreign_proc("C",
    string.append_list(Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list = Strs;
    MR_Word tmp;
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

    % We implement string.join_list in C as this minimises the amount of
    % garbage created.
:- pragma foreign_proc("C",
    string.join_list(Sep::in, Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Word list;
    MR_Word tmp;
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
string.join_list(Sep, [H | T]) = H ++ string.join_list_2(Sep, T).

:- func join_list_2(string::in, list(string)::in) = (string::uo) is det.

join_list_2(_, []) = "".
join_list_2(Sep, [H | T]) = Sep ++ H ++ join_list_2(Sep, T).

%-----------------------------------------------------------------------------%
%
% NOTE: string.hash, hash2 and hash3 are also defined as MR_hash_string,
% MR_hash_string2 and MR_hash_string3 in runtime/mercury_string.h.
% The corresponding definitions must be kept identical.

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

%-----------------------------------------------------------------------------%

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
    sub_string_search_start(String::in, SubString::in, BeginAt::in, Index::out),
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

% This is only used if there is no matching foreign_proc definition
sub_string_search_start(String, SubString, BeginAt, Index) :-
    sub_string_search_start_2(String, SubString, BeginAt, length(String),
        length(SubString), Index).

    % Brute force string searching. For short Strings this is good;
    % for longer strings Boyer-Moore is much better.
    %
:- pred sub_string_search_start_2(string::in, string::in, int::in, int::in,
    int::in, int::out) is semidet.

sub_string_search_start_2(String, SubString, I, Length, SubLength, Index) :-
    I < Length,
    (
        % XXX This is inefficient -- there is no (in, in, in) = in is semidet
        % mode of substring, so this ends up calling the (in, in, in) = out
        % mode and then doing the unification. This will create a lot of
        % unnecessary garbage.
        % XXX This will abort if either index is not at a code point boundary.
        between(String, I, I + SubLength) = SubString
    ->
        Index = I
    ;
        sub_string_search_start_2(String, SubString, I + 1, Length, SubLength,
            Index)
    ).

%-----------------------------------------------------------------------------%

string.format(FormatString, PolyList, String) :-
    % This predicate has been optimised to produce the least memory possible
    % -- memory usage is a significant problem for programs which do a lot of
    % formatted IO.
    (
        format_string_to_specifiers(Specifiers, PolyList, [],
            to_char_list(FormatString), [])
    ->
        String = string.append_list(
            list.map(specifier_to_string, Specifiers))
    ;
        error("string.format: format string invalid.")
    ).

:- type string.specifier
    --->    spec_conv(
                flags       :: list(char),
                width       :: maybe(list(char)),
                precision   :: maybe(list(char)),
                spec        :: spec
            )
    ;       spec_string(list(char)).

    % A format string is parsed into alternate sections. We alternate between
    % the list of characters which don't represent a conversion specifier
    % and those that do.
    %
:- pred format_string_to_specifiers(list(string.specifier)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is det.

format_string_to_specifiers(Specifiers, !PolyTypes, !Chars) :-
    other(NonConversionSpecChars, !Chars),
    ( conversion_specification(ConversionSpec, !PolyTypes, !Chars) ->
        format_string_to_specifiers(Specifiers0, !PolyTypes, !Chars),
        Specifiers = [spec_string(NonConversionSpecChars), ConversionSpec
            | Specifiers0]
    ;
        Specifiers = [spec_string(NonConversionSpecChars)]
    ).

    % Parse a string which doesn't contain any conversion specifications.
    %
:- pred other(list(char)::out, list(char)::in, list(char)::out) is det.

other(Result, !Chars) :-
    (
        !.Chars = [Char | !:Chars],
        Char \= '%'
    ->
        other(Result0, !Chars),
        Result = [Char | Result0]
    ;
        Result = []
    ).

    % Each conversion specification is introduced by the character '%',
    % and ends with a conversion specifier. In between there may be
    % (in this order) zero or more flags, an optional minimum field width,
    % and an optional precision.
    %
:- pred conversion_specification(string.specifier::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is semidet.

conversion_specification(Specificier, !PolyTypes, !Chars) :-
    !.Chars = ['%' | !:Chars],
    flags(Flags, !Chars),
    optional(width, MaybeWidth, !PolyTypes, !Chars),
    optional(prec, MaybePrec, !PolyTypes, !Chars),
    ( spec(Spec, !PolyTypes, !Chars) ->
        Specificier = spec_conv(Flags, MaybeWidth, MaybePrec, Spec)
    ;
        error("string.format: invalid conversion specifier.")
    ).

:- pred optional(
    pred(T, U, U, V, V)::in(pred(out, in, out, in, out) is semidet),
    maybe(T)::out, U::in, U::out, V::in, V::out) is det.

optional(P, MaybeOutput, Init, Final, !Acc) :-
    ( P(Output, Init, Final0, !Acc) ->
        MaybeOutput = yes(Output),
        Final = Final0
    ;
        MaybeOutput = no,
        Final = Init
    ).

:- pred flags(list(char)::out, list(char)::in, list(char)::out) is semidet.

flags(Result, !Chars) :-
    (
        !.Chars = [Char | !:Chars],
        flag(Char)
    ->
        flags(Result0, !Chars),
        Result = [Char | Result0]
    ;
        Result = []
    ).

    % Is it a valid flag character?
    %
:- pred flag(char::in) is semidet.

flag('#').
flag('0').
flag('-').
flag(' ').
flag('+').

    % Do we have a minimum field width?
    %
:- pred width(list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is semidet.

width(Width, !PolyTypes, !Chars) :-
    ( !.Chars = ['*' | !:Chars] ->
        ( !.PolyTypes = [i(Width0) | !:PolyTypes] ->
            % XXX may be better done in C.
            Width = to_char_list(int_to_string(Width0))
        ;
            error("string.format: " ++
                "`*' width modifier not associated with an integer.")
        )
    ;
        Init = !.Chars,
        non_zero_digit(!Chars),
        zero_or_more_occurences(digit, !Chars),
        Final = !.Chars,
        list.remove_suffix(Init, Final, Width)
    ).

    % Do we have a precision?
    %
:- pred prec(list(char)::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is semidet.

prec(Prec, !PolyTypes, !Chars) :-
    !.Chars = ['.' | !:Chars],
    ( !.Chars = ['*' | !:Chars] ->
        ( !.PolyTypes = [i(Prec0) | !:PolyTypes] ->
            % XXX Best done in C
            Prec = to_char_list(int_to_string(Prec0))
        ;
            error("string.format: " ++
                "`*' precision modifier not associated with an integer.")
        )
    ;
        Init = !.Chars,
        digit(!Chars),
        zero_or_more_occurences(digit, !Chars),
        Final = !.Chars
    ->
        list.remove_suffix(Init, Final, Prec)
    ;
        % When no number follows the '.' the precision defaults to 0.
        Prec = ['0']
    ).

% NB the capital letter specifiers are proceeded with a 'c'.
:- type spec
            % valid integer specifiers
    --->    d(int)
    ;       i(int)
    ;       o(int)
    ;       u(int)
    ;       x(int)
    ;       cX(int)
    ;       p(int)

            % valid float specifiers
    ;       e(float)
    ;       cE(float)
    ;       f(float)
    ;       cF(float)
    ;       g(float)
    ;       cG(float)

            % valid char specifiers
    ;       c(char)

            % valid string specifiers
    ;       s(string)

            % specifier representing "%%"
    ;       percent.

    % Do we have a valid conversion specifier?
    % We check to ensure that the specifier also matches the type
    % from the input list.
    %
:- pred spec(spec::out,
    list(string.poly_type)::in, list(string.poly_type)::out,
    list(char)::in, list(char)::out) is semidet.

% Valid integer conversion specifiers.
spec(d(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['d' | !:Chars].
spec(i(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['i' | !:Chars].
spec(o(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['o' | !:Chars].
spec(u(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['u' | !:Chars].
spec(x(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['x' | !:Chars].
spec(cX(Int), [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['X' | !:Chars].
spec(p(Int),  [i(Int) | Ps], Ps, !Chars) :- !.Chars = ['p' | !:Chars].

% Valid float conversion specifiers.
spec(e(Float),  [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['e' | !:Chars].
spec(cE(Float), [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['E' | !:Chars].
spec(f(Float),  [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['f' | !:Chars].
spec(cF(Float), [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['F' | !:Chars].
spec(g(Float),  [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['g' | !:Chars].
spec(cG(Float), [f(Float) | Ps], Ps, !Chars) :- !.Chars = ['G' | !:Chars].

% Valid char conversion specifiers.
spec(c(Char), [c(Char) | Ps], Ps, !Chars) :- !.Chars = ['c' | !:Chars].

% Valid string conversion specifiers.
spec(s(Str), [s(Str) | Ps], Ps, !Chars) :- !.Chars = ['s' | !:Chars].

% Conversion specifier representing the "%" sign.
spec(percent, Ps, Ps, !Chars) :- !.Chars = ['%' | !:Chars].

    % A digit in the range [1-9].
    %
:- pred non_zero_digit(list(char)::in, list(char)::out) is semidet.

non_zero_digit(!Chars) :-
    !.Chars = [Char | !:Chars],
    char.is_digit(Char),
    Char \= '0'.

    % A digit in the range [0-9].
    %
:- pred digit(list(char)::in, list(char)::out) is semidet.

digit(!Chars) :-
    !.Chars = [Char | !:Chars],
    char.is_digit(Char).

    % Zero or more occurences of the string parsed by the given pred.
    %
:- pred zero_or_more_occurences(
    pred(list(T), list(T))::in(pred(in, out) is semidet),
    list(T)::in, list(T)::out) is det.

zero_or_more_occurences(P, !Chars) :-
    ( P(!Chars) ->
        zero_or_more_occurences(P, !Chars)
    ;
        true
    ).

:- func specifier_to_string(string.specifier) = string.

specifier_to_string(spec_conv(Flags, Width, Prec, Spec)) = String :-
    (
        % Valid int conversion specifiers.
        Spec = d(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "d"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_int(Flags, conv(Width), conv(Prec), Int)
        )
    ;
        Spec = i(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "i"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_int(Flags, conv(Width), conv(Prec), Int)
        )
    ;
        Spec = o(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "o"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, conv(Width), conv(Prec),
                8, Int, no, "")
        )
    ;
        Spec = u(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "u"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, conv(Width), conv(Prec),
                10, Int, no, "")
        )
    ;
        Spec = x(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "x"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, conv(Width), conv(Prec),
                16, Int, no, "0x")
        )
    ;
        Spec = cX(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "X"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(Flags, conv(Width), conv(Prec),
                16, Int, no, "0X")
        )
    ;
        Spec = p(Int),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, int_length_modifer,
                "p"),
            String = native_format_int(FormatStr, Int)
        ;
            String = format_unsigned_int(['#' | Flags], conv(Width),
                conv(Prec), 16, Int, yes, "0x")
        )
    ;
        % Valid float conversion specifiers.
        Spec = e(Float),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "e"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number(Flags, conv(Width), conv(Prec),
                Float, "e")
        )
    ;
        Spec = cE(Float),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "E"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number(Flags, conv(Width), conv(Prec),
                Float, "E")
        )
    ;
        Spec = f(Float),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "f"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_float(Flags, conv(Width), conv(Prec), Float)
        )
    ;
        Spec = cF(Float),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "F"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_float(Flags, conv(Width), conv(Prec), Float)
        )
    ;
        Spec = g(Float),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "g"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number_g(Flags, conv(Width), conv(Prec),
                Float, "e")
        )
    ;
        Spec = cG(Float),
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "G"),
            String = native_format_float(FormatStr, Float)
        ;
            String = format_scientific_number_g(Flags, conv(Width), conv(Prec),
                Float, "E")
        )
    ;
        % Valid char conversion Specifiers.
        Spec = c(Char),
        ( using_sprintf_for_char(Char) ->
            FormatStr = make_format(Flags, Width, Prec, "", "c"),
            String = native_format_char(FormatStr, Char)
        ;
            String = format_char(Flags, conv(Width), Char)
        )
    ;
        % Valid string conversion Specifiers.
        Spec = s(Str),
        (
            (
                using_sprintf,
                Flags = [],
                Width = no,
                Prec = no
            ;
                using_sprintf_for_string(Str)
            )
        ->
            FormatStr = make_format(Flags, Width, Prec, "", "s"),
            String = native_format_string(FormatStr, Str)
        ;
            String = format_string(Flags, conv(Width), conv(Prec), Str)
        )
    ;
        % Conversion specifier representing the "%" sign.
        Spec = percent,
        String = "%"
    ).
specifier_to_string(spec_string(Chars)) = from_char_list(Chars).

:- func conv(maybe(list(character))) = maybe(int).

conv(no) = no.
conv(yes(X)) = yes(string.det_to_int(from_char_list(X))).

%-----------------------------------------------------------------------------%

    % Construct a format string.
    %
:- func make_format(list(char), maybe(list(char)),
    maybe(list(char)), string, string) = string.

make_format(Flags, MaybeWidth, MaybePrec, LengthMod, Spec) =
    ( using_sprintf ->
        make_format_sprintf(Flags, MaybeWidth, MaybePrec, LengthMod, Spec)
    ;
        make_format_dotnet(Flags, MaybeWidth, MaybePrec, LengthMod, Spec)
    ).

    % Are we using C's sprintf? All backends other than C return false.
    % Note that any backends which return true for using_sprintf/0 must
    % also implement:
    %
    %   int_length_modifer/0
    %   native_format_float/2
    %   native_format_int/2
    %   native_format_string/2
    %   native_format_char/2
    %
:- pred using_sprintf is semidet.

:- pragma foreign_proc("C", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    SUCCESS_INDICATOR = MR_TRUE;
").
:- pragma foreign_proc("C#", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Java", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Erlang", using_sprintf,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false
").

:- pred using_sprintf_for_char(char::in) is semidet.

using_sprintf_for_char(_) :-
    semidet_fail.

:- pragma foreign_proc("C",
    using_sprintf_for_char(Char::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    /* sprintf %c specifier is inadequate for multi-byte UTF-8 characters. */
    SUCCESS_INDICATOR = MR_is_ascii(Char);
").

:- pred using_sprintf_for_string(string::in) is semidet.

using_sprintf_for_string(_) :-
    semidet_fail.

:- pragma foreign_proc("C",
    using_sprintf_for_string(Str::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    const char *s;

    SUCCESS_INDICATOR = MR_TRUE;
    for (s = Str; *s != '\\0'; s++) {
        /* sprintf %s specifier is inadequate for multi-byte UTF-8 characters,
         * if there is a field width or precision specified.
         */
        if (!MR_utf8_is_single_byte(*s)) {
            SUCCESS_INDICATOR = MR_FALSE;
            break;
        }
    }
").

    % Construct a format string suitable to passing to sprintf.
    %
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
    String = string.append_list(["%", from_char_list(Flags),
        from_char_list(Width), from_char_list(Prec), LengthMod, Spec]).

    % Construct a format string suitable to passing to .NET's formatting
    % functions.
    % XXX this code is not yet complete. We need to do a lot more work
    % to make this work perfectly.
    %
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
    (   Spec0 = "i" -> Spec = "d"
    ;   Spec0 = "f" -> Spec = "e"
    ;   Spec = Spec0
    ),
    String = string.append_list([
        "{0",
        from_char_list(Width),
        ":",
        Spec,
        from_char_list(Prec),
%       LengthMod,
%       from_char_list(Flags),
        "}"]).

:- func int_length_modifer = string.

:- pragma foreign_proc("C",
    int_length_modifer = (LengthModifier::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_make_aligned_string(LengthModifier, MR_INTEGER_LENGTH_MODIFIER);
}").

int_length_modifer = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.int_length_modifer/0 not defined").

    % Create a string from a float using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_float(string, float) = string.

:- pragma foreign_proc("C",
    native_format_float(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, (double) Val);
    MR_restore_transient_hp();
}").
native_format_float(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_float/2 not defined").

    % Create a string from a int using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_int(string, int) = string.

:- pragma foreign_proc("C",
    native_format_int(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, Val);
    MR_restore_transient_hp();
}").
native_format_int(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_int/2 not defined").

    % Create a string from a string using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_string(string, string) = string.

:- pragma foreign_proc("C",
    native_format_string(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, Val);
    MR_restore_transient_hp();
}").
native_format_string(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_string/2 not defined").

    % Create a string from a char using the format string.
    % Note it is the responsibility of the caller to ensure that the
    % format string is valid.
    %
:- func native_format_char(string, char) = string.

:- pragma foreign_proc("C",
    native_format_char(FormatStr::in, Val::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_ALLOC_ID, FormatStr, Val);
    MR_restore_transient_hp();
}").
native_format_char(_, _) = _ :-
    % This predicate is only called if using_sprintf/0, so we produce an error
    % by default.
    error("string.native_format_char/2 not defined").

%-----------------------------------------------------------------------------%

:- type flags == list(char).
:- type maybe_width == maybe(int).
:- type maybe_precision == maybe(int).

    % Format a character (c).
    %
:- func format_char(flags, maybe_width, char) = string.

format_char(Flags, Width, Char) = String :-
    CharStr = string.char_to_string(Char),
    String = justify_string(Flags, Width, CharStr).

    % Format a string (s).
    %
:- func format_string(flags, maybe_width, maybe_precision, string) = string.

format_string(Flags, Width, Prec, OldStr) = NewStr :-
    (
        Prec = yes(NumChars),
        PrecStr = string.left_by_codepoint(OldStr, NumChars)
    ;
        Prec = no,
        PrecStr = OldStr
    ),
    NewStr = justify_string(Flags, Width, PrecStr).

:- func format_int(flags, maybe_width, maybe_precision, int) = string.

format_int(Flags, Width, Prec, Int) = String :-
    % Find the integer's absolute value, and take care of the special case
    % of precision zero with an integer of 0.
    (
        Int = 0,
        Prec = yes(0)
    ->
        AbsIntStr = ""
    ;
        Integer = integer(Int),
        AbsInteger = integer.abs(Integer),
        AbsIntStr = integer.to_string(AbsInteger)
    ),
    AbsIntStrLength = string.count_codepoints(AbsIntStr),

    % Do we need to increase precision?
    (
        Prec = yes(Precision),
        Precision > AbsIntStrLength
    ->
        PrecStr = string.pad_left(AbsIntStr, '0', Precision)
    ;
        PrecStr = AbsIntStr
    ),

    % Do we need to pad to the field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecStr),
        member('0', Flags),
        \+ member('-', Flags),
        Prec = no
    ->
        FieldStr = string.pad_left(PrecStr, '0', FieldWidth - 1),
        ZeroPadded = yes
    ;
        FieldStr = PrecStr,
        ZeroPadded = no
    ),

    % Prefix with appropriate sign or zero padding.
    % The previous step has deliberately left room for this.
    SignedStr = add_int_prefix_if_needed(Flags, ZeroPadded, Int, FieldStr),
    String = justify_string(Flags, Width, SignedStr).

    % Format an unsigned int, unsigned octal, or unsigned hexadecimal
    % (u,o,x,X).
    %
:- func format_unsigned_int(flags, maybe_width, maybe_precision,
    int, int, bool, string) = string.

format_unsigned_int(Flags, Width, Prec, Base, Int, IsTypeP, Prefix) = String :-
    % Find the integer's absolute value, and take care of the special case
    % of precision zero with an integer of 0.
    (
        Int = 0,
        Prec = yes(0)
    ->
        AbsIntStr = ""
    ;
        Div = integer.pow(integer(2), integer(int.bits_per_int)),
        UnsignedInteger = integer(Int) mod Div,
        ( Base = 10 ->
            AbsIntStr0 = integer.to_string(UnsignedInteger)
        ; Base = 8 ->
            AbsIntStr0 = to_octal(UnsignedInteger)
        ; Prefix = "0x" ->
            AbsIntStr0 = to_hex(UnsignedInteger)
        ;
            AbsIntStr0 = to_capital_hex(UnsignedInteger)
        ),

        % Just in case Int = 0 (base converters return "").
        ( AbsIntStr0 = "" ->
            AbsIntStr = "0"
        ;
            AbsIntStr = AbsIntStr0
        )
    ),
    AbsIntStrLength = string.count_codepoints(AbsIntStr),

    % Do we need to increase precision?
    (
        Prec = yes(Precision),
        Precision > AbsIntStrLength
    ->
        PrecStr = string.pad_left(AbsIntStr, '0', Precision)
    ;
        PrecStr = AbsIntStr
    ),

    % Do we need to increase the precision of an octal?
    (
        Base = 8,
        member('#', Flags),
        \+ string.prefix(PrecStr, "0")
    ->
        PrecModStr = append("0", PrecStr)
    ;
        PrecModStr = PrecStr
    ),

    % Do we need to pad to the field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecModStr),
        member('0', Flags),
        \+ member('-', Flags),
        Prec = no
    ->
        % Do we need to make room for "0x" or "0X" ?
        (
            Base = 16,
            member('#', Flags),
            ( Int \= 0
            ; IsTypeP = yes
            )
        ->
            FieldStr = string.pad_left(PrecModStr, '0', FieldWidth - 2)
        ;
            FieldStr = string.pad_left(PrecModStr, '0', FieldWidth)
        )
    ;
        FieldStr = PrecModStr
    ),

    % Do we have to prefix "0x" or "0X"?
    (
        Base = 16,
        member('#', Flags),
        ( Int \= 0
        ; IsTypeP = yes
        )
    ->
        FieldModStr = Prefix ++ FieldStr
    ;
        FieldModStr = FieldStr
    ),

    String = justify_string(Flags, Width, FieldModStr).

    % Format a float (f)
    %
:- func format_float(flags, maybe_width, maybe_precision, float) = string.

format_float(Flags, Width, Prec, Float) = NewFloat :-
    % Determine absolute value of string.
    Abs = abs(Float),

    % Change precision (default is 6).
    AbsStr = convert_float_to_string(Abs),
    ( is_nan_or_inf(Abs) ->
        PrecModStr = AbsStr
    ;
        (
            Prec = yes(Precision),
            PrecStr = change_precision(Precision, AbsStr)
        ;
            Prec = no,
            PrecStr = change_precision(6, AbsStr)
        ),

        % Do we need to remove the decimal point?
        (
            \+ member('#', Flags),
            Prec = yes(0)
        ->
            PrecStrLen = string.count_codepoints(PrecStr),
            PrecModStr = string.between(PrecStr, 0, PrecStrLen - 1)
        ;
            PrecModStr = PrecStr
        )
    ),

    % Do we need to change field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecModStr),
        member('0', Flags),
        \+ member('-', Flags)
    ->
        FieldStr = string.pad_left(PrecModStr, '0', FieldWidth - 1),
        ZeroPadded = yes
    ;
        FieldStr = PrecModStr,
        ZeroPadded = no
    ),
    % Finishing up ..
    SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float, FieldStr),
    NewFloat = justify_string(Flags, Width, SignedStr).

    % Format a scientific number to a specified number of significant
    % figures (g,G)
    %
:- func format_scientific_number_g(flags, maybe_width, maybe_precision,
    float, string) = string.

format_scientific_number_g(Flags, Width, Prec, Float, E) = NewFloat :-
    % Determine absolute value of string.
    Abs = abs(Float),

    % Change precision (default is 6).
    AbsStr = convert_float_to_string(Abs),
    ( is_nan_or_inf(Abs) ->
        PrecStr = AbsStr
    ;
        (
            Prec = yes(Precision),
            ( Precision = 0 ->
                PrecStr = change_to_g_notation(AbsStr, 1, E, Flags)
            ;
                PrecStr = change_to_g_notation(AbsStr, Precision, E, Flags)
            )
        ;
            Prec = no,
            PrecStr = change_to_g_notation(AbsStr, 6, E, Flags)
        )
    ),

        %
        % Do we need to change field width?
        %
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecStr),
        member('0', Flags),
        \+ member('-', Flags)
    ->
        FieldStr = string.pad_left(PrecStr, '0', FieldWidth - 1),
        ZeroPadded = yes
    ;
        FieldStr = PrecStr,
        ZeroPadded = no
    ),

    % Finishing up ..
    SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float, FieldStr),
    NewFloat = justify_string(Flags, Width, SignedStr).

    % Format a scientific number (e,E)
    %
:- func format_scientific_number(flags, maybe_width, maybe_precision,
    float, string) = string.

format_scientific_number(Flags, Width, Prec, Float, E) = NewFloat :-
    % Determine absolute value of string.
    Abs = abs(Float),

    % Change precision (default is 6).
    AbsStr = convert_float_to_string(Abs),
    ( is_nan_or_inf(Abs) ->
        PrecModStr = AbsStr
    ;
        (
            Prec = yes(Precision),
            PrecStr = change_to_e_notation(AbsStr, Precision, E)
        ;
            Prec = no,
            PrecStr = change_to_e_notation(AbsStr, 6, E)
        ),

        % Do we need to remove the decimal point?
        (
            \+ member('#', Flags),
            Prec = yes(0)
        ->
            split_at_decimal_point(PrecStr, BaseStr, ExponentStr),
            PrecModStr = BaseStr ++ ExponentStr
        ;
            PrecModStr = PrecStr
        )
    ),

    % Do we need to change field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.count_codepoints(PrecModStr),
        member('0', Flags),
        \+ member('-', Flags)
    ->
        FieldStr = string.pad_left(PrecModStr, '0', FieldWidth - 1),
        ZeroPadded = yes
    ;
        FieldStr = PrecModStr,
        ZeroPadded = no
    ),

    % Finishing up ..
    SignedStr = add_float_prefix_if_needed(Flags, ZeroPadded, Float, FieldStr),
    NewFloat = justify_string(Flags, Width, SignedStr).

:- func add_int_prefix_if_needed(flags, bool, int, string) = string.

add_int_prefix_if_needed(Flags, ZeroPadded, Int, FieldStr) = SignedStr :-
    ( Int < 0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
        SignedStr = " " ++ FieldStr
    ;
        (
            ZeroPadded = yes,
            SignedStr = "0" ++ FieldStr
        ;
            ZeroPadded = no,
            SignedStr = FieldStr
        )
    ).

:- func add_float_prefix_if_needed(flags, bool, float, string) = string.

add_float_prefix_if_needed(Flags, ZeroPadded, Float, FieldStr) = SignedStr :-
    ( Float < 0.0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
        SignedStr = " " ++ FieldStr
    ;
        (
            ZeroPadded = yes,
            SignedStr = "0" ++ FieldStr
        ;
            ZeroPadded = no,
            SignedStr = FieldStr
        )
    ).

:- func justify_string(flags, maybe_width, string) = string.

justify_string(Flags, Width, Str) =
    (
        Width = yes(FWidth),
        FWidth > string.count_codepoints(Str)
    ->
        ( member('-', Flags) ->
            string.pad_right(Str, ' ', FWidth)
        ;
            string.pad_left(Str, ' ', FWidth)
        )
    ;
        Str
    ).

    % Convert an integer to an octal string.
    %
:- func to_octal(integer) = string.

to_octal(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = to_octal(Num // integer(8)),
        Rem = Num rem integer(8),
        RemStr = integer.to_string(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert an integer to a hexadecimal string using a-f.
    %
:- func to_hex(integer) = string.

to_hex(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = to_hex(Num // integer(16)),
        Rem = Num rem integer(16),
        RemStr = get_hex_int(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Convert an integer to a hexadecimal string using A-F.
    %
:- func to_capital_hex(integer) = string.

to_capital_hex(Num) = NumStr :-
    ( Num > integer(0) ->
        Rest = to_capital_hex(Num // integer(16)),
        Rem = Num rem integer(16),
        RemStr = get_capital_hex_int(Rem),
        NumStr = append(Rest, RemStr)
    ;
        NumStr = ""
    ).

    % Given a decimal integer, return the hexadecimal equivalent (using % a-f).
    %
:- func get_hex_int(integer) = string.

get_hex_int(Int) = HexStr :-
    ( Int < integer(10) ->
        HexStr = integer.to_string(Int)
    ; Int = integer(10) ->
        HexStr = "a"
    ; Int = integer(11) ->
        HexStr = "b"
    ; Int = integer(12) ->
        HexStr = "c"
    ; Int = integer(13) ->
        HexStr = "d"
    ; Int = integer(14) ->
        HexStr = "e"
    ;
        HexStr = "f"
    ).

    % Convert an integer to a hexadecimal string using A-F.
    %
:- func get_capital_hex_int(integer) = string.

get_capital_hex_int(Int) = HexStr :-
    ( Int < integer(10) ->
        HexStr = integer.to_string(Int)
    ; Int = integer(10) ->
        HexStr = "A"
    ; Int = integer(11) ->
        HexStr = "B"
    ; Int = integer(12) ->
        HexStr = "C"
    ; Int = integer(13) ->
        HexStr = "D"
    ; Int = integer(14) ->
        HexStr = "E"
    ;
        HexStr = "F"
    ).

    % Unlike the standard library function, this function converts a float
    % to a string without resorting to scientific notation.
    %
    % This predicate relies on the fact that string.float_to_string returns
    % a float which is round-trippable, ie to the full precision needed.
    %
:- func convert_float_to_string(float) = string.

convert_float_to_string(Float) = String :-
    string.lowlevel_float_to_string(Float, FloatStr),

    % Check for scientific representation.
    (
        ( string.contains_char(FloatStr, 'e')
        ; string.contains_char(FloatStr, 'E')
        )
    ->
        split_at_exponent(FloatStr, FloatPtStr, ExpStr),
        split_at_decimal_point(FloatPtStr, MantissaStr, FractionStr),

        % What is the exponent?
        ExpInt = string.det_to_int(ExpStr),
        ( ExpInt >= 0 ->
            % Move decimal pt to the right.
            ExtraDigits = ExpInt,
            PaddedFracStr = string.pad_right(FractionStr, '0', ExtraDigits),
            string.split(PaddedFracStr, ExtraDigits, MantissaRest,
                NewFraction),

            NewMantissa = MantissaStr ++ MantissaRest,
            MantAndPoint = NewMantissa ++ ".",
            ( NewFraction = "" ->
                String = MantAndPoint ++ "0"
            ;
                String = MantAndPoint ++ NewFraction
            )
        ;
            % Move decimal pt to the left.
            ExtraDigits = abs(ExpInt),
            PaddedMantissaStr = string.pad_left(MantissaStr, '0',
                ExtraDigits),
            string.split(PaddedMantissaStr,
                length(PaddedMantissaStr) - ExtraDigits,
                NewMantissa, FractionRest),

            ( NewMantissa = "" ->
                MantAndPoint = "0."
            ;
                MantAndPoint = NewMantissa ++ "."
            ),
            String = MantAndPoint ++ FractionRest ++ FractionStr
        )
    ;
        String = FloatStr
    ).

    % Converts a floating point number to a specified number of standard
    % figures. The style used depends on the value converted; style e (or E)
    % is used only if the exponent resulting from such a conversion is less
    % than -4 or greater than or equal to the precision. Trailing zeros are
    % removed from the fractional portion of the result unless the # flag
    % is specified: a decimal-point character appears only if it is followed
    % by a digit.
    %
:- func change_to_g_notation(string, int, string, flags) = string.

change_to_g_notation(Float, Prec, E, Flags) = FormattedFloat :-
    Exponent = size_of_required_exponent(Float, Prec),
    (
        Exponent >= -4,
        Exponent < Prec
    ->
        % Float will be represented normally.
        % -----------------------------------
        % Need to calculate precision to pass to the change_precision function,
        % because the current precision represents significant figures,
        % not decimal places.
        %
        % Now change float's precision.
        %
        ( Exponent =< 0 ->
            % Deal with floats such as 0.00000000xyz.
            DecimalPos = decimal_pos(Float),
            FormattedFloat0 = change_precision(abs(DecimalPos) - 1 + Prec,
                Float)
        ;
            % Deal with floats such as ddddddd.mmmmmmmm.
            ScientificFloat = change_to_e_notation(Float, Prec - 1, "e"),
            split_at_exponent(ScientificFloat, BaseStr, ExponentStr),
            Exp = string.det_to_int(ExponentStr),
            split_at_decimal_point(BaseStr, MantissaStr, FractionStr),
            RestMantissaStr = between(FractionStr, 0, Exp),
            NewFraction = between(FractionStr, Exp, Prec - 1),
            FormattedFloat0 = MantissaStr ++ RestMantissaStr
                ++ "." ++ NewFraction
        ),

        % Do we remove trailing zeros?
        ( member('#', Flags) ->
            FormattedFloat = FormattedFloat0
        ;
            FormattedFloat = remove_trailing_zeros(FormattedFloat0)
        )
    ;
        % Float will be represented in scientific notation.
        % -------------------------------------------------
        UncheckedFloat = change_to_e_notation(Float, Prec - 1, E),

        % Do we need to remove trailing zeros?
        ( member('#', Flags) ->
            FormattedFloat = UncheckedFloat
        ;
            split_at_exponent(UncheckedFloat, BaseStr, ExponentStr),
            NewBaseStr = remove_trailing_zeros(BaseStr),
            FormattedFloat = NewBaseStr ++ E ++ ExponentStr
        )
    ).

    % Convert floating point notation to scientific notation.
    %
:- func change_to_e_notation(string, int, string) = string.

change_to_e_notation(Float, Prec, E) = ScientificFloat :-
    UnsafeExponent = decimal_pos(Float),
    UnsafeBase = calculate_base_unsafe(Float, Prec),

    % Is mantissa greater than one digit long?
    split_at_decimal_point(UnsafeBase, MantissaStr, _FractionStr),
    ( string.count_codepoints(MantissaStr) > 1 ->
        % Need to append 0, to fix the problem of having no numbers
        % after the decimal point.
        SafeBase = calculate_base_unsafe(string.append(UnsafeBase, "0"),
            Prec),
        SafeExponent = UnsafeExponent + 1
    ;
        SafeBase = UnsafeBase,
        SafeExponent = UnsafeExponent
    ),
    % Creating exponent.
    ( SafeExponent >= 0 ->
        ( SafeExponent < 10 ->
            ExponentStr = string.append_list(
                [E, "+0", string.int_to_string(SafeExponent)])
        ;
            ExponentStr = string.append_list(
                [E, "+", string.int_to_string(SafeExponent)])
        )
    ;
        ( SafeExponent > -10 ->
            ExponentStr = string.append_list(
                [E, "-0", string.int_to_string(int.abs(SafeExponent))])
        ;
            ExponentStr = E ++ string.int_to_string(SafeExponent)
        )
    ),
    ScientificFloat = SafeBase ++ ExponentStr.

    % Given a floating point number, this function calculates the size of
    % the exponent needed to represent the float in scientific notation.
    %
:- func size_of_required_exponent(string, int) = int.

size_of_required_exponent(Float, Prec) = Exponent :-
    UnsafeExponent = decimal_pos(Float),
    UnsafeBase = calculate_base_unsafe(Float, Prec),

    % Is mantissa one digit long?
    split_at_decimal_point(UnsafeBase, MantissaStr, _FractionStr),
    ( string.count_codepoints(MantissaStr) > 1 ->
        % We will need to move decimal pt one place to the left:
        % therefore, increment exponent.
        Exponent = UnsafeExponent + 1
    ;
        Exponent = UnsafeExponent
    ).

    % Given a string representing a floating point number, function returns
    % a string with all trailing zeros removed.
    %
:- func remove_trailing_zeros(string) = string.

remove_trailing_zeros(Float) = TrimmedFloat :-
    FloatCharList = string.to_char_list(Float),
    FloatCharListRev = list.reverse(FloatCharList),
    TrimmedFloatRevCharList = remove_zeros(FloatCharListRev),
    TrimmedFloatCharList = list.reverse(TrimmedFloatRevCharList),
    TrimmedFloat = string.from_char_list(TrimmedFloatCharList).

    % Given a char list, this function removes all leading zeros, including
    % decimal point, if need be.
    %
:- func remove_zeros(list(char)) = list(char).

remove_zeros(CharNum) = TrimmedNum :-
    ( CharNum = ['0' | Rest] ->
        TrimmedNum = remove_zeros(Rest)
    ; CharNum = ['.' | Rest] ->
        TrimmedNum = Rest
    ;
        TrimmedNum = CharNum
    ).

    % Determine the location of the decimal point in the string that
    % represents a floating point number.
    %
:- func decimal_pos(string) = int.

decimal_pos(Float) = Pos :-
    split_at_decimal_point(Float, MantissaStr, _FractionStr),
    NumZeros = string.count_codepoints(MantissaStr) - 1,
    Pos = find_non_zero_pos(string.to_char_list(Float), NumZeros).

    % Given a list of chars representing a floating point number, this
    % function determines the first position containing a non-zero digit.
    % Positions after the decimal point are negative, and those before the
    % decimal point are positive.
    %
:- func find_non_zero_pos(list(char), int) = int.

find_non_zero_pos(L, CurrentPos) = ActualPos :-
    (
        L = [H | T],
        ( is_decimal_point(H) ->
            ActualPos = find_non_zero_pos(T, CurrentPos)
        ; H = '0' ->
            ActualPos = find_non_zero_pos(T, CurrentPos - 1)
        ;
            ActualPos = CurrentPos
        )
    ;
        L = [],
        ActualPos = 0
    ).

    % Representing a floating point number in scientific notation requires
    % a base and an exponent. This function returns the base. But it is unsafe,
    % since particular input result in the base having a mantissa with more
    % than one digit. Therefore, the calling function must check for this
    % problem.
    %
:- func calculate_base_unsafe(string, int) = string.

calculate_base_unsafe(Float, Prec) = Exp :-
    Place = decimal_pos(Float),
    split_at_decimal_point(Float, MantissaStr, FractionStr),
    ( Place < 0 ->
        DecimalPos = abs(Place),
        PaddedMantissaStr = string.between(FractionStr, 0, DecimalPos),

        % Get rid of superfluous zeros.
        MantissaInt = string.det_to_int(PaddedMantissaStr),
        ExpMantissaStr = string.int_to_string(MantissaInt),

        % Create fractional part.
        PaddedFractionStr = pad_right(FractionStr, '0', Prec + 1),
        ExpFractionStr = string.between(PaddedFractionStr, DecimalPos,
            DecimalPos + Prec + 1)
    ; Place > 0 ->
        ExpMantissaStr = string.between(MantissaStr, 0, 1),
        FirstHalfOfFractionStr = string.between(MantissaStr, 1, Place),
        ExpFractionStr = FirstHalfOfFractionStr ++ FractionStr
    ;
        ExpMantissaStr = MantissaStr,
        ExpFractionStr = FractionStr
    ),
    MantissaAndPoint = ExpMantissaStr ++ ".",
    UnroundedExpStr = MantissaAndPoint ++ ExpFractionStr,
    Exp = change_precision(Prec, UnroundedExpStr).

    % Change the precision of a float to a specified number of decimal places.
    %
    % n.b. OldFloat must be positive for this function to work.
    %
:- func change_precision(int, string) = string.

change_precision(Prec, OldFloat) = NewFloat :-
    split_at_decimal_point(OldFloat, MantissaStr, FractionStr),
    FracStrLen = string.count_codepoints(FractionStr),
    ( Prec > FracStrLen ->
        PrecFracStr = string.pad_right(FractionStr, '0', Prec),
        PrecMantissaStr = MantissaStr
    ; Prec < FracStrLen ->
        UnroundedFrac = string.between(FractionStr, 0, Prec),
        NextDigit = string.det_index(FractionStr, Prec),
        (
            UnroundedFrac \= "",
            (char.to_int(NextDigit) - char.to_int('0')) >= 5
        ->
            NewPrecFrac = string.det_to_int(UnroundedFrac) + 1,
            NewPrecFracStrNotOK = string.int_to_string( NewPrecFrac),
            NewPrecFracStr = string.pad_left(NewPrecFracStrNotOK, '0', Prec),
            (
                string.count_codepoints(NewPrecFracStr) >
                    string.count_codepoints(UnroundedFrac)
            ->
                PrecFracStr = between(NewPrecFracStr, 1, 1 + Prec),
                PrecMantissaInt = det_to_int(MantissaStr) + 1,
                PrecMantissaStr = int_to_string(PrecMantissaInt)
            ;
                PrecFracStr = NewPrecFracStr,
                PrecMantissaStr = MantissaStr
            )
        ;
            UnroundedFrac = "",
            (char.to_int(NextDigit) - char.to_int('0')) >= 5
        ->
            PrecMantissaInt = det_to_int(MantissaStr) + 1,
            PrecMantissaStr = int_to_string(PrecMantissaInt),
            PrecFracStr = ""
        ;
            PrecFracStr = UnroundedFrac,
            PrecMantissaStr = MantissaStr
        )
    ;
        PrecFracStr = FractionStr,
        PrecMantissaStr = MantissaStr
    ),
    HalfNewFloat = PrecMantissaStr ++ ".",
    NewFloat = HalfNewFloat ++ PrecFracStr.

:- pred split_at_exponent(string::in, string::out, string::out) is det.

split_at_exponent(Str, Float, Exponent) :-
    FloatAndExponent = string.words_separator(is_exponent, Str),
    list.det_index0(FloatAndExponent, 0, Float),
    list.det_index0(FloatAndExponent, 1, Exponent).

:- pred split_at_decimal_point(string::in, string::out, string::out) is det.

split_at_decimal_point(Str, Mantissa, Fraction) :-
    MantAndFrac = string.words_separator(is_decimal_point, Str),
    list.det_index0(MantAndFrac, 0, Mantissa),
    ( list.index0(MantAndFrac, 1, Fraction0) ->
        Fraction = Fraction0
    ;
        Fraction = ""
    ).

:- pred is_decimal_point(char :: in) is semidet.

is_decimal_point('.').

:- pred is_exponent(char :: in) is semidet.

is_exponent('e').
is_exponent('E').

%-----------------------------------------------------------------------------%

% The remaining routines are implemented using the C interface.

:- pragma foreign_decl("C",
"
#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include ""mercury_string.h""   /* for MR_allocate_aligned_string*() etc. */
#include ""mercury_tags.h"" /* for MR_list_cons*() */
").

%-----------------------------------------------------------------------------%

string.from_float(Flt) = string.float_to_string(Flt).

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
").

:- pragma foreign_proc("Java",
    string.float_to_string(Flt::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    Str = java.lang.Double.toString(Flt);
").

% XXX This implementation has problems when the mantissa cannot fit in an int.

string.float_to_string(Float, unsafe_promise_unique(String)) :-
    % XXX The unsafe_promise_unique is needed because in
    % string.float_to_string_2 the call to string.to_float doesn't
    % have a (ui, out) mode hence the output string cannot be unique.
    String = string.float_to_string_2(min_precision, Float).

:- func string.float_to_string_2(int, float) = (string) is det.

string.float_to_string_2(Prec, Float) = String :-
    string.format("%#." ++ int_to_string(Prec) ++ "g", [f(Float)], Tmp),
    ( Prec = max_precision ->
        String = Tmp
    ;
        ( string.to_float(Tmp, Float) ->
            String = Tmp
        ;
            String = string.float_to_string_2(Prec + 1, Float)
        )
    ).

    % XXX For efficiency reasons we assume that on non-C backends that
    % we are using double precision floats, however the commented out code
    % provides a general mechanism for calculating the required precision.
:- func min_precision = int.

min_precision = 15.

% min_precision =
%   floor_to_int(float(mantissa_digits) * log2(float(radix)) / log2(10.0)).

:- func max_precision = int.
max_precision = min_precision + 2.

% string.lowlevel_float_to_string differs from string.float_to_string in that
% it must be implemented without calling string.format (e.g. by invoking some
% foreign language routine to do the conversion) as this is the predicate
% string.format uses to get the initial string representation of a float.
%
% The string returned must match one of the following regular expression:
%   ^[+-]?[0-9]*\.?[0-9]+((e|E)[0-9]+)?$
%   ^[nN][aA][nN]$
%   ^[+-]?[iI][nN][fF][iI][nN][iI][tT][yY]$
%   ^[+-]?[iI][nN][fF]$
% and the string returned must have sufficient precision for representing
% the float.
%
:- pred string.lowlevel_float_to_string(float::in, string::uo) is det.

:- pragma foreign_proc("C",
    string.lowlevel_float_to_string(Flt::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"{
    /*
    ** Note any changes here will require the same changes in
    ** string.float_to_string.
    */
    MR_float_to_string(Flt, Str, MR_ALLOC_ID);
}").

:- pragma foreign_proc("C#",
    string.lowlevel_float_to_string(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // The R format string prints the double out such that it can be
    // round-tripped.
    // XXX According to the documentation it tries the 15 digits of precision,
    // then 17 digits skipping 16 digits of precision, unlike what we do
    // for the C backend.
    FloatString = FloatVal.ToString(""R"");
").

:- pragma foreign_proc("Java",
    string.lowlevel_float_to_string(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatString = java.lang.Double.toString(FloatVal);
").

:- pragma foreign_proc("Erlang",
    string.lowlevel_float_to_string(FloatVal::in, FloatString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    List = io_lib:format(""~.17g"", [FloatVal]),
    FloatString = list_to_binary(List)
").

string.det_to_float(FloatString) =
    ( string.to_float(FloatString, FloatVal) ->
      FloatVal
    ;
      func_error("string.det_to_float/1 - conversion failed.")
    ).

:- pragma foreign_export("C", string.to_float(in, out), "ML_string_to_float").
:- pragma foreign_export("IL", string.to_float(in, out), "ML_string_to_float").

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
            // throws a NumberFormatException when you give it NaN or infinity,
            // so we handle these cases below.

            if (FloatString.equalsIgnoreCase(""nan"")) {
                FloatVal = java.lang.Double.NaN;
                SUCCESS_INDICATOR = true;
            } else if (FloatString.equalsIgnoreCase(""infinity"")) {
                FloatVal = java.lang.Double.POSITIVE_INFINITY;
                SUCCESS_INDICATOR = true;
            } else if (FloatString.substring(1).equalsIgnoreCase(""infinity""))
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

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

% It's important to inline string.index and string.det_index.
% so that the compiler can do loop invariant hoisting
% on calls to string.length that occur in loops.
:- pragma inline(string.index/3).

string.index(Str, Index, Char) :-
    Len = string.length(Str),
    ( string.index_check(Index, Len) ->
        string.unsafe_index(Str, Index, Char)
    ;
        fail
    ).

:- pragma inline(string.index_next/4).

string.index_next(Str, Index, NextIndex, Char) :-
    Len = string.length(Str),
    ( string.index_check(Index, Len) ->
        string.unsafe_index_next(Str, Index, NextIndex, Char)
    ;
        fail
    ).

:- pragma inline(string.prev_index/4).

string.prev_index(Str, Index, CharIndex, Char) :-
    Len = string.length(Str),
    ( string.index_check(Index - 1, Len) ->
        string.unsafe_prev_index(Str, Index, CharIndex, Char)
    ;
        fail
    ).

:- pred string.index_check(int::in, int::in) is semidet.

% We should consider making this routine a compiler built-in.
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

/*-----------------------------------------------------------------------*/

string.unsafe_index(Str, Index, Char) :-
    ( string.unsafe_index_2(Str, Index, Char0) ->
        Char = Char0
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

String ^ unsafe_elem(Index) = unsafe_index(String, Index).

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

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

:- pragma foreign_decl("C",
"
#ifdef MR_USE_GCC_GLOBAL_REGISTERS
    /*
    ** GNU C version egcs-1.1.2 crashes with `fixed or forbidden register
    ** spilled' in grade asm_fast.gc.tr.debug if we write this inline.
    */
    extern void MR_set_code_unit(MR_String str, MR_Integer ind, char ch);
#else
    #define MR_set_code_unit(str, ind, ch) \\
        ((str)[ind] = (ch))
#endif
").

:- pragma foreign_code("C",
"
#ifdef MR_USE_GCC_GLOBAL_REGISTERS
    /*
    ** GNU C version egcs-1.1.2 crashes with `fixed or forbidden register
    ** spilled' in grade asm_fast.gc.tr.debug if we write this inline.
    */
    void MR_set_code_unit(MR_String str, MR_Integer ind, char ch)
    {
        str[ind] = ch;
    }
#endif
").

string.set_char(Char, Index, !Str) :-
    ( char.to_int(Char, 0) ->
        error("string.set_char: null character")
    ;
        string.set_char_2(Char, Index, !Str)
    ).

:- pred string.set_char_2(char, int, string, string).
:- mode string.set_char_2(in, in, in, out) is semidet.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.set_char_2(in, in, di, uo) is semidet.

:- pragma foreign_proc("C",
    string.set_char_2(Ch::in, Index::in, Str0::in, Str::out),
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
        MR_set_code_unit(Str, Index, Ch);
    } else {
        int oldc = MR_utf8_get(Str0, Index);
        if (oldc < 0) {
            SUCCESS_INDICATOR = MR_FALSE;
        } else {
            size_t oldwidth = MR_utf8_width(oldc);
            size_t newwidth = MR_utf8_width(Ch);
            size_t newlen;
            size_t tailofs;

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
    string.set_char_2(Ch::in, Index::in, Str0::in, Str::out),
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
    string.set_char_2(Ch::in, Index::in, Str0::in, Str::out),
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
    string.set_char_2(Ch::in, Index::in, Str0::in, Str::out),
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

/*-----------------------------------------------------------------------*/

string.unsafe_set_char(Char, Index, !Str) :-
    ( char.to_int(Char, 0) ->
        error("string.unsafe_set_char: null character")
    ;
        string.unsafe_set_char_2(Char, Index, !Str)
    ).

:- pred string.unsafe_set_char_2(char, int, string, string).
:- mode string.unsafe_set_char_2(in, in, in, out) is det.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.unsafe_set_char_2(in, in, di, uo) is det.

:- pragma foreign_proc("C",
    string.unsafe_set_char_2(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    size_t len = strlen(Str0);
    if (MR_is_ascii(Str0[Index]) && MR_is_ascii(Ch)) {
        /* Fast path. */
        MR_allocate_aligned_string_msg(Str, len, MR_ALLOC_ID);
        strcpy(Str, Str0);
        MR_set_code_unit(Str, Index, Ch);
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
    string.unsafe_set_char_2(Ch::in, Index::in, Str0::in, Str::out),
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
    string.unsafe_set_char_2(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int oldc = Str0.codePointAt(Index);
    int oldwidth = java.lang.Character.charCount(oldc);
    Str = Str0.subSequence(0, Index)
        + new String(Character.toChars(Ch))
        + Str0.subSequence(Index + oldwidth, Str0.length());
").
:- pragma foreign_proc("Erlang",
    string.unsafe_set_char_2(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<Left:Index/binary, _/utf8, Right/binary>> = Str0,
    Str = unicode:characters_to_binary([Left, Ch, Right])
").

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

string.count_codepoints(String) = Count :-
    string.count_codepoints(String, Count).

string.count_codepoints(String, Count) :-
    count_codepoints_2(String, 0, 0, Count).

:- pred count_codepoints_2(string::in, int::in, int::in, int::out) is det.

count_codepoints_2(String, I, Count0, Count) :-
    ( string.unsafe_index_next(String, I, J, _) ->
        count_codepoints_2(String, J, Count0 + 1, Count)
    ;   
        Count = Count0
    ).

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

/*-----------------------------------------------------------------------*/

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
        error("string.count_utf8_code_units: char.to_utf8 failed")
    ).

/*-----------------------------------------------------------------------*/

    % Note: we do not define what happens with unpaired surrogates.
    %
string.codepoint_offset(String, N, Index) :-
    string.codepoint_offset(String, 0, N, Index).

string.codepoint_offset(String, StartOffset, N, Index) :-
    StartOffset >= 0,
    Length = string.length(String),
    string.codepoint_offset_2(String, StartOffset, Length, N, Index).

:- pred codepoint_offset_2(string::in, int::in, int::in, int::in, int::out)
    is semidet.

codepoint_offset_2(String, Offset, Length, N, Index) :-
    Offset < Length,
    ( N = 0 ->
        Index = Offset
    ;
        string.unsafe_index_next(String, Offset, NextOffset, _),
        string.codepoint_offset_2(String, NextOffset, Length, N - 1, Index)
    ).

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

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

between(Str, Start, End, SubStr) :-
    ( Start >= End ->
        SubStr = ""
    ;
        Len = string.length(Str),
        max(0, Start, ClampStart),
        min(Len, End, ClampEnd),
        CharList = strchars(ClampStart, ClampEnd, Str),
        SubStr = string.from_char_list(CharList)
    ).

:- func strchars(int, int, string) = list(char).

strchars(I, End, Str) = Chars :-
    (
        I < End,
        string.unsafe_index_next(Str, I, J, C),
        J =< End
    ->
        Cs = strchars(J, End, Str),
        Chars = [C | Cs]
    ;
        Chars = []
    ).

:- pragma foreign_proc("C",
    string.between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"{
    MR_Integer  len;
    MR_Integer  Count;
    MR_Word     tmp;

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

string.between(Str, Start, End) = SubString :-
    string.between(Str, Start, End, SubString).

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

%-----------------------------------------------------------------------------%

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

string.unsafe_between(Str, Start, End) = SubString :-
    string.unsafe_between(Str, Start, End, SubString).

string.unsafe_substring(Str, Start, Count) = SubString :-
    string.unsafe_between(Str, Start, Start + Count) = SubString.

string.unsafe_substring(Str, Start, Count, SubString) :-
    string.unsafe_between(Str, Start, Start + Count, SubString).

%-----------------------------------------------------------------------------%

    % The Right substring may not have mode `uo' because it may return Str,
    % which has mode `in'.
    %
:- pragma foreign_proc("C",
    string.split(Str::in, Count::in, Left::out, Right::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate],
"{
    MR_Integer  len;
    MR_Word     tmp;

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
            string.from_code_unit_list(LeftList, Left0),
            string.from_code_unit_list(RightList, Right0)
        ->
            Left = Left0,
            Right = Right0
        ;
            error("string.split")
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

/*-----------------------------------------------------------------------*/

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
        SUCCESS_INDICATOR = Str.regionMatches(toffset, Rest, 0, Rest.length());
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
% Functional forms added.

string.length(S) = L :-
    string.length(S, L).

string.append(S1, S2) = S3 :-
    string.append(S1, S2, S3).

string.char_to_string(C) = S1 :-
    string.char_to_string(C, S1).

string.int_to_string(N) = S1 :-
    string.int_to_string(N, S1).

string.int_to_base_string(N1, N2) = S2 :-
    string.int_to_base_string(N1, N2, S2).

string.float_to_string(R) = S2 :-
    string.float_to_string(R, S2).

string.c_pointer_to_string(P) = S :-
    string.c_pointer_to_string(P, S).

string.from_c_pointer(P) = S :-
    string.c_pointer_to_string(P, S).

string.replace_all(S1, S2, S3) = S4 :-
    string.replace_all(S1, S2, S3, S4).

string.to_lower(S1) = S2 :-
    string.to_lower(S1, S2).

string.to_upper(S1) = S2 :-
    string.to_upper(S1, S2).

string.capitalize_first(S1) = S2 :-
    string.capitalize_first(S1, S2).

string.uncapitalize_first(S1) = S2 :-
    string.uncapitalize_first(S1, S2).

string.to_char_list(S) = Cs :-
    string.to_char_list(S, Cs).

string.from_char_list(Cs) = S :-
    string.from_char_list(Cs, S).

string.from_rev_char_list(Cs) = S :-
    string.from_rev_char_list(Cs, S).

string.pad_left(S1, C, N) = S2 :-
    string.pad_left(S1, C, N, S2).

string.pad_right(S1, C, N) = S2 :-
    string.pad_right(S1, C, N, S2).

string.duplicate_char(C, N) = S :-
    string.duplicate_char(C, N, S).

string.det_index(S, N) = C :-
    string.det_index(S, N, C).

string.unsafe_index(S, N) = C :-
    string.unsafe_index(S, N, C).

string.det_set_char(C, N, S0) = S :-
    string.det_set_char(C, N, S0, S).

string.unsafe_set_char(C, N, S0) = S :-
    string.unsafe_set_char(C, N, S0, S).

string.foldl(F, S, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldl(P, S, A, B).

string.foldl_between(F, S, Start, End, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldl_between(P, S, Start, End, A, B).

string.left(S1, N) = S2 :-
    string.left(S1, N, S2).

string.right(S1, N) = S2 :-
    string.right(S1, N, S2).

string.format(S1, PT) = S2 :-
    string.format(S1, PT, S2).

%------------------------------------------------------------------------------%

string.words_separator(SepP, String) = Words :-
    next_boundary(SepP, String, 0, WordStart),
    words_2(SepP, String, WordStart, Words).

:- pred words_2(pred(char)::in(pred(in) is semidet), string::in, int::in,
    list(string)::out) is det.

words_2(SepP, String, WordStart, Words) :-
    next_boundary(isnt(SepP), String, WordStart, WordEnd),
    ( WordEnd = WordStart ->
        Words = []
    ;
        string.unsafe_between(String, WordStart, WordEnd, Word),
        next_boundary(SepP, String, WordEnd, NextWordStart),
        ( WordEnd = NextWordStart ->
            Words = [Word]
        ;
            words_2(SepP, String, NextWordStart, Words0),
            Words = [Word | Words0]
        )
    ).

    % Return the smallest I >= I0 such that `not P(String[I])'.
    %
:- pred next_boundary(pred(char)::in(pred(in) is semidet), string::in, int::in,
    int::out) is det.

next_boundary(P, String, I0, I) :-
    (
        string.unsafe_index_next(String, I0, I1, Char),
        P(Char)
    ->
        next_boundary(P, String, I1, I)
    ;
        I = I0
    ).

%------------------------------------------------------------------------------%

string.words(String) = string.words_separator(char.is_whitespace, String).

%------------------------------------------------------------------------------%

string.split_at_separator(DelimP, String) = Substrings :-
    Len = string.length(String),
    split_at_separator_2(DelimP, String, Len, Len, [], Substrings).

:- pred split_at_separator_2(pred(char)::in(pred(in) is semidet), string::in,
    int::in, int::in, list(string)::in, list(string)::out) is det.

split_at_separator_2(DelimP, Str, I, SegEnd, Acc0, Acc) :-
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
            split_at_separator_2(DelimP, Str, J, J, [Seg | Acc0], Acc)
        ;
            % Extend current segment.
            split_at_separator_2(DelimP, Str, J, SegEnd, Acc0, Acc)
        )
    ;
        % We've reached the beginning of the string.
        Seg = string.unsafe_between(Str, 0, SegEnd),
        Acc = [Seg | Acc0]
    ).

%------------------------------------------------------------------------------%

string.split_at_char(C, String) =
    string.split_at_separator(unify(C), String).

%------------------------------------------------------------------------------%

split_at_string(Needle, Total) =
    split_at_string(0, length(Needle), Needle, Total).

:- func split_at_string(int, int, string, string) = list(string).

split_at_string(StartAt, NeedleLen, Needle, Total) = Out :-
    ( sub_string_search_start(Total, Needle, StartAt, NeedlePos) ->
        BeforeNeedle = between(Total, StartAt, NeedlePos),
        Tail = split_at_string(NeedlePos+NeedleLen, NeedleLen, Needle, Total),
        Out = [BeforeNeedle | Tail]
    ;
        string.split(Total, StartAt, _Skip, Last),
        Out = [Last]
    ).

%------------------------------------------------------------------------------%

S1 ++ S2 = string.append(S1, S2).

%------------------------------------------------------------------------------%

string.det_to_int(S) = string.det_base_string_to_int(10, S).

%------------------------------------------------------------------------------%

string.det_base_string_to_int(Base, S) = N :-
    ( string.base_string_to_int(Base, S, N0) ->
        N = N0
    ;
        error("string.det_base_string_to_int/2: conversion failed")
    ).

%-----------------------------------------------------------------------------%

chomp(S) = Chomp :-
    ( prev_index(S, length(S), Offset, '\n') ->
        Chomp = left(S, Offset)
    ;
        Chomp = S
    ).

%-----------------------------------------------------------------------------%

rstrip(S) = rstrip_pred(is_whitespace, S).

%-----------------------------------------------------------------------------%

lstrip(S) = lstrip_pred(is_whitespace, S).

%-----------------------------------------------------------------------------%

strip(S0) = S :-
    L = prefix_length(is_whitespace, S0),
    R = suffix_length(is_whitespace, S0),
    S = between(S0, L, length(S0) - R).

%-----------------------------------------------------------------------------%

rstrip_pred(P, S) = left(S, length(S) - suffix_length(P, S)).

%-----------------------------------------------------------------------------%

lstrip_pred(P, S) = right(S, length(S) - prefix_length(P, S)).

%-----------------------------------------------------------------------------%

prefix_length(P, S) = Index :-
    prefix_length_2(P, S, 0, Index).

:- pred prefix_length_2(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

prefix_length_2(P, S, I, Index) :-
    (
        string.unsafe_index_next(S, I, J, Char),
        P(Char)
    ->
        prefix_length_2(P, S, J, Index)
    ;
        Index = I
    ).

%-----------------------------------------------------------------------------%

suffix_length(P, S) = End - Index :-
    End = string.length(S),
    suffix_length_2(P, S, End, Index).

:- pred suffix_length_2(pred(char)::in(pred(in) is semidet),
    string::in, int::in, int::out) is det.

suffix_length_2(P, S, I, Index) :-
    (
        string.unsafe_prev_index(S, I, J, Char),
        P(Char)
    ->
        suffix_length_2(P, S, J, Index)
    ;
        Index = I
    ).

%------------------------------------------------------------------------------%

    % For efficiency, these predicates collect a list of strings which,
    % when concatenated in reverse order, produce the final output.
    %
:- type revstrings == list(string).

    % Utility predicate.
    %
:- pred add_revstring(string::in, revstrings::in, revstrings::out) is det.

add_revstring(String, RevStrings, [String | RevStrings]).

% Various different versions of univ_to_string.

string.string(Univ) = String :-
    string.string_ops_noncanon(canonicalize, ops.init_mercury_op_table,
        Univ, String).

string.string_ops(OpsTable, Univ) = String :-
    string.string_ops_noncanon(canonicalize, OpsTable, Univ, String).

string.string_ops_noncanon(NonCanon, OpsTable, X, String) :-
    value_to_revstrings(NonCanon, OpsTable, X, [], RevStrings),
    String = string.append_list(list.reverse(RevStrings)).

:- pred value_to_revstrings(noncanon_handling, ops.table, T,
    revstrings, revstrings).
:- mode value_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode value_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode value_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode value_to_revstrings(in, in, in, in, out) is cc_multi.

value_to_revstrings(NonCanon, OpsTable, X, !Rs) :-
    Priority = ops.max_priority(OpsTable) + 1,
    value_to_revstrings_prio(NonCanon, OpsTable, Priority, X, !Rs).

:- pred value_to_revstrings_prio(noncanon_handling, ops.table, ops.priority, T,
    revstrings, revstrings).
:- mode value_to_revstrings_prio(in(do_not_allow), in, in, in, in, out) is det.
:- mode value_to_revstrings_prio(in(canonicalize), in, in, in, in, out) is det.
:- mode value_to_revstrings_prio(in(include_details_cc), in, in, in, in, out)
    is cc_multi.
:- mode value_to_revstrings_prio(in, in, in, in, in, out) is cc_multi.

value_to_revstrings_prio(NonCanon, OpsTable, Priority, X, !Rs) :-
    % We need to special-case the builtin types:
    %   int, char, float, string
    %   type_info, univ, c_pointer, array
    %   and private_builtin.type_info

    ( dynamic_cast(X, String) ->
        add_revstring(term_io.quoted_string(String), !Rs)
    ; dynamic_cast(X, Char) ->
        add_revstring(term_io.quoted_char(Char), !Rs)
    ; dynamic_cast(X, Int) ->
        add_revstring(string.int_to_string(Int), !Rs)
    ; dynamic_cast(X, Float) ->
        add_revstring(string.float_to_string(Float), !Rs)
    ; dynamic_cast(X, Bitmap) ->
        add_revstring(term_io.quoted_string(bitmap.to_string(Bitmap)), !Rs)
    ; dynamic_cast(X, TypeDesc) ->
        type_desc_to_revstrings(TypeDesc, !Rs)
    ; dynamic_cast(X, TypeCtorDesc) ->
        type_ctor_desc_to_revstrings(TypeCtorDesc, !Rs)
    ; dynamic_cast(X, C_Pointer) ->
        add_revstring(c_pointer_to_string(C_Pointer), !Rs)
    ;
        % Check if the type is array.array/1. We can't just use dynamic_cast
        % here since array.array/1 is a polymorphic type.
        %
        % The calls to type_ctor_name and type_ctor_module_name are not really
        % necessary -- we could use dynamic_cast in the condition instead of
        % det_dynamic_cast in the body. However, this way of doing things
        % is probably more efficient in the common case when the thing
        % being printed is *not* of type array.array/1.
        %
        % The ordering of the tests here (arity, then name, then module name,
        % rather than the reverse) is also chosen for efficiency, to find
        % failure cheaply in the common cases, rather than for readability.
        %
        type_ctor_and_args(type_of(X), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "array",
        type_ctor_module_name(TypeCtor) = "array"
    ->
        % Now that we know the element type, we can constrain the type of
        % the variable `Array' so that we can use det_dynamic_cast.
        %
        has_type(Elem, ElemType),
        same_array_elem_type(Array, Elem),
        det_dynamic_cast(X, Array),
        array_to_revstrings(NonCanon, OpsTable, Array, !Rs)
    ;
        type_ctor_and_args(type_of(X), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "version_array",
        type_ctor_module_name(TypeCtor) = "version_array"
    ->
        has_type(Elem, ElemType),
        same_version_array_elem_type(VersionArray, Elem),
        det_dynamic_cast(X, VersionArray),
        version_array_to_revstrings(NonCanon, OpsTable, VersionArray, !Rs)
    ;
        % Check if the type is private_builtin.type_info/1.
        % See the comments above for array.array/1.
        %
        type_ctor_and_args(type_of(X), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "type_info",
        type_ctor_module_name(TypeCtor) = "private_builtin"
    ->
        has_type(Elem, ElemType),
        same_private_builtin_type(PrivateBuiltinTypeInfo, Elem),
        det_dynamic_cast(X, PrivateBuiltinTypeInfo),
        private_builtin_type_info_to_revstrings(PrivateBuiltinTypeInfo, !Rs)
    ;
        ordinary_term_to_revstrings(NonCanon, OpsTable, Priority, X, !Rs)
    ).

:- pred same_array_elem_type(array(T)::unused, T::unused) is det.

same_array_elem_type(_, _).

:- pred same_version_array_elem_type(version_array(T)::unused, T::unused)
    is det.

same_version_array_elem_type(_, _).

:- pred same_private_builtin_type(private_builtin.type_info::unused,
    T::unused) is det.

same_private_builtin_type(_, _).

:- pred ordinary_term_to_revstrings(noncanon_handling, ops.table,
    ops.priority, T, revstrings, revstrings).
:- mode ordinary_term_to_revstrings(in(do_not_allow), in, in, in, in, out)
    is det.
:- mode ordinary_term_to_revstrings(in(canonicalize), in, in, in, in, out)
    is det.
:- mode ordinary_term_to_revstrings(in(include_details_cc), in, in, in, in, out)
    is cc_multi.
:- mode ordinary_term_to_revstrings(in, in, in, in, in, out)
    is cc_multi.

ordinary_term_to_revstrings(NonCanon, OpsTable, Priority, X, !Rs) :-
    deconstruct(X, NonCanon, Functor, _Arity, Args),
    (
        Functor = "[|]",
        Args = [ListHead, ListTail]
    ->
        add_revstring("[", !Rs),
        arg_to_revstrings(NonCanon, OpsTable, ListHead, !Rs),
        univ_list_tail_to_revstrings(NonCanon, OpsTable, ListTail, !Rs),
        add_revstring("]", !Rs)
    ;
        Functor = "[]",
        Args = []
    ->
        add_revstring("[]", !Rs)
    ;
        Functor = "{}"
    ->
        (
            Args = [],
            add_revstring("{}", !Rs)
        ;
            Args = [BracedTerm],
            add_revstring("{ ", !Rs),
            value_to_revstrings(NonCanon, OpsTable, univ_value(BracedTerm),
                !Rs),
            add_revstring(" }", !Rs)
        ;
            Args = [BracedHead | BracedTail],
            BracedTail = [_ | _],
            add_revstring("{", !Rs),
            arg_to_revstrings(NonCanon, OpsTable, BracedHead, !Rs),
            term_args_to_revstrings(NonCanon, OpsTable, BracedTail, !Rs),
            add_revstring("}", !Rs)
        )
    ;
        Args = [Arg]
    ->
        (
            ops.lookup_prefix_op(OpsTable, Functor, OpPriority, OpAssoc)
        ->
            maybe_add_revstring("(", Priority, OpPriority, !Rs),
            add_revstring(term_io.quoted_atom(Functor), !Rs),
            add_revstring(" ", !Rs),
            adjust_priority(OpPriority, OpAssoc, NewPriority),
            value_to_revstrings_prio(NonCanon, OpsTable, NewPriority,
                univ_value(Arg), !Rs),
            maybe_add_revstring(")", Priority, OpPriority, !Rs)
        ;
            ops.lookup_postfix_op(OpsTable, Functor, OpPriority, OpAssoc)
        ->
            maybe_add_revstring("(", Priority, OpPriority, !Rs),
            adjust_priority(OpPriority, OpAssoc, NewPriority),
            value_to_revstrings_prio(NonCanon, OpsTable, NewPriority,
                univ_value(Arg), !Rs),
            add_revstring(" ", !Rs),
            add_revstring(term_io.quoted_atom(Functor), !Rs),
            maybe_add_revstring(")", Priority, OpPriority, !Rs)
        ;
            plain_term_to_revstrings(NonCanon, OpsTable, Priority,
                Functor, Args, !Rs)
        )
    ;
        Args = [Arg1, Arg2]
    ->
        (
            ops.lookup_infix_op(OpsTable, Functor, OpPriority,
                LeftAssoc, RightAssoc)
        ->
            maybe_add_revstring("(", Priority, OpPriority, !Rs),
            adjust_priority(OpPriority, LeftAssoc, LeftPriority),
            value_to_revstrings_prio(NonCanon, OpsTable, LeftPriority,
                univ_value(Arg1), !Rs),
            ( Functor = "," ->
                add_revstring(", ", !Rs)
            ;
                add_revstring(" ", !Rs),
                add_revstring(term_io.quoted_atom(Functor), !Rs),
                add_revstring(" ", !Rs)
            ),
            adjust_priority(OpPriority, RightAssoc, RightPriority),
            value_to_revstrings_prio(NonCanon, OpsTable, RightPriority,
                univ_value(Arg2), !Rs),
            maybe_add_revstring(")", Priority, OpPriority, !Rs)
        ;
            ops.lookup_binary_prefix_op(OpsTable, Functor,
                OpPriority, FirstAssoc, SecondAssoc)
        ->
            maybe_add_revstring("(", Priority, OpPriority, !Rs),
            add_revstring(term_io.quoted_atom(Functor), !Rs),
            add_revstring(" ", !Rs),
            adjust_priority(OpPriority, FirstAssoc, FirstPriority),
            value_to_revstrings_prio(NonCanon, OpsTable, FirstPriority,
                univ_value(Arg1), !Rs),
            add_revstring(" ", !Rs),
            adjust_priority(OpPriority, SecondAssoc, SecondPriority),
            value_to_revstrings_prio(NonCanon, OpsTable, SecondPriority,
                univ_value(Arg2), !Rs),
            maybe_add_revstring(")", Priority, OpPriority, !Rs)
        ;
            plain_term_to_revstrings(NonCanon, OpsTable, Priority,
                Functor, Args, !Rs)
        )
    ;
        plain_term_to_revstrings(NonCanon, OpsTable, Priority, Functor, Args,
            !Rs)
    ).

:- pred plain_term_to_revstrings(noncanon_handling, ops.table,
    ops.priority, string, list(univ), revstrings, revstrings).
:- mode plain_term_to_revstrings(in(do_not_allow), in, in, in, in, in, out)
    is det.
:- mode plain_term_to_revstrings(in(canonicalize), in, in, in, in, in, out)
    is det.
:- mode plain_term_to_revstrings(in(include_details_cc), in, in, in, in,
    in, out) is cc_multi.
:- mode plain_term_to_revstrings(in, in, in, in, in, in, out)
    is cc_multi.

plain_term_to_revstrings(NonCanon, OpsTable, Priority, Functor, Args, !Rs) :-
    (
        Args = [],
        ops.lookup_op(OpsTable, Functor),
        Priority =< ops.max_priority(OpsTable)
    ->
        add_revstring("(", !Rs),
        add_revstring(term_io.quoted_atom(Functor), !Rs),
        add_revstring(")", !Rs)
    ;
        add_revstring(
            term_io.quoted_atom_agt(Functor,
                term_io.maybe_adjacent_to_graphic_token),
            !Rs
        )
    ),
    (
        Args = [Y | Ys],
        add_revstring("(", !Rs),
        arg_to_revstrings(NonCanon, OpsTable, Y, !Rs),
        term_args_to_revstrings(NonCanon, OpsTable, Ys, !Rs),
        add_revstring(")", !Rs)
    ;
        Args = []
    ).

:- pred maybe_add_revstring(string::in, ops.priority::in, ops.priority::in,
    revstrings::in, revstrings::out) is det.

maybe_add_revstring(String, Priority, OpPriority, !Rs) :-
    ( OpPriority > Priority ->
        add_revstring(String, !Rs)
    ;
        true
    ).

:- pred adjust_priority(ops.priority::in, ops.assoc::in, ops.priority::out)
    is det.

adjust_priority(Priority, ops.y, Priority).
adjust_priority(Priority, ops.x, Priority - 1).

:- pred univ_list_tail_to_revstrings(noncanon_handling, ops.table, univ,
    revstrings, revstrings).
:- mode univ_list_tail_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode univ_list_tail_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode univ_list_tail_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode univ_list_tail_to_revstrings(in, in, in, in, out) is cc_multi.

univ_list_tail_to_revstrings(NonCanon, OpsTable, Univ, !Rs) :-
    deconstruct(univ_value(Univ), NonCanon, Functor, _Arity, Args),
    (
        Functor = "[|]",
        Args = [ListHead, ListTail]
    ->
        add_revstring(", ", !Rs),
        arg_to_revstrings(NonCanon, OpsTable, ListHead, !Rs),
        univ_list_tail_to_revstrings(NonCanon, OpsTable, ListTail, !Rs)
    ;
        Functor = "[]",
        Args = []
    ->
        true
    ;
        add_revstring(" | ", !Rs),
        value_to_revstrings(NonCanon, OpsTable, univ_value(Univ), !Rs)
    ).

    % Write the remaining arguments.
    %
:- pred term_args_to_revstrings(noncanon_handling, ops.table, list(univ),
    revstrings, revstrings).
:- mode term_args_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode term_args_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode term_args_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode term_args_to_revstrings(in, in, in, in, out) is cc_multi.

term_args_to_revstrings(_, _, [], !Rs).
term_args_to_revstrings(NonCanon, OpsTable, [X | Xs], !Rs) :-
    add_revstring(", ", !Rs),
    arg_to_revstrings(NonCanon, OpsTable, X, !Rs),
    term_args_to_revstrings(NonCanon, OpsTable, Xs, !Rs).

:- pred arg_to_revstrings(noncanon_handling,
    ops.table, univ, revstrings, revstrings).
:- mode arg_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode arg_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode arg_to_revstrings(in(include_details_cc), in, in, in, out) is cc_multi.
:- mode arg_to_revstrings(in, in, in, in, out) is cc_multi.

arg_to_revstrings(NonCanon, OpsTable, X, !Rs) :-
    Priority = comma_priority(OpsTable),
    value_to_revstrings_prio(NonCanon, OpsTable, Priority, univ_value(X), !Rs).

:- func comma_priority(ops.table) = ops.priority.

% comma_priority(OpsTable) =
%   ( ops.lookup_infix_op(OpTable, ",", Priority, _, _) ->
%       Priority
%   ;
%       func_error("arg_priority: can't find the priority of `,'")
%   ).
% We could implement this as above, but it's more efficient to just
% hard-code it.

comma_priority(_OpTable) = 1000.

:- pred array_to_revstrings(noncanon_handling, ops.table, array(T),
    revstrings, revstrings).
:- mode array_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode array_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode array_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode array_to_revstrings(in, in, in, in, out) is cc_multi.

array_to_revstrings(NonCanon, OpsTable, Array, !Rs) :-
    add_revstring("array(", !Rs),
    value_to_revstrings(NonCanon, OpsTable,
        array.to_list(Array) `with_type` list(T), !Rs),
    add_revstring(")", !Rs).

:- pred version_array_to_revstrings(noncanon_handling, ops.table, version_array(T),
    revstrings, revstrings).
:- mode version_array_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode version_array_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode version_array_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode version_array_to_revstrings(in, in, in, in, out) is cc_multi.

version_array_to_revstrings(NonCanon, OpsTable, Array, !Rs) :-
    add_revstring("version_array(", !Rs),
    value_to_revstrings(NonCanon, OpsTable,
        version_array.to_list(Array) `with_type` list(T), !Rs),
    add_revstring(")", !Rs).

:- pred type_desc_to_revstrings(type_desc::in,
    revstrings::in, revstrings::out) is det.

type_desc_to_revstrings(TypeDesc, !Rs) :-
    add_revstring(term_io.quoted_atom(type_name(TypeDesc)), !Rs).

:- pred type_ctor_desc_to_revstrings(type_ctor_desc::in,
    revstrings::in, revstrings::out) is det.

type_ctor_desc_to_revstrings(TypeCtorDesc, !Rs) :-
    type_desc.type_ctor_name_and_arity(TypeCtorDesc, ModuleName,
        Name0, Arity0),
    Name = term_io.quoted_atom(Name0),
    (
        ModuleName = "builtin",
        Name = "func"
    ->
        % The type ctor that we call `builtin.func/N' takes N + 1 type
        % parameters: N arguments plus one return value. So we need to subtract
        % one from the arity here.
        Arity = Arity0 - 1
    ;
        Arity = Arity0
    ),
    ( ModuleName = "builtin" ->
        String = string.format("%s/%d", [s(Name), i(Arity)])
    ;
        String = string.format("%s.%s/%d", [s(ModuleName), s(Name), i(Arity)])
    ),
    add_revstring(String, !Rs).

:- pred private_builtin_type_info_to_revstrings(
    private_builtin.type_info::in, revstrings::in, revstrings::out) is det.

private_builtin_type_info_to_revstrings(PrivateBuiltinTypeInfo, !Rs) :-
    private_builtin.unsafe_type_cast(PrivateBuiltinTypeInfo, TypeInfo),
    type_desc.type_info_to_type_desc(TypeInfo, TypeDesc),
    type_desc_to_revstrings(TypeDesc, !Rs).

:- pred det_dynamic_cast(T1::in, T2::out) is det.

det_dynamic_cast(X, Y) :-
    det_univ_to_type(univ(X), Y).

%------------------------------------------------------------------------------%

% Currently, string.format_table simply assumes each code point occupies a
% single column in a fixed-width output device.  Thus the output will only be
% aligned if limited to an (important) subset of characters, namely ASCII and
% European characters (excluding combining characters).  It would be relatively
% easy to support CJK double-width characters and zero-width characters (see
% wcswidth), which would be enough to cover the needs of very many people.
%
% These considerations may also apply to predicates such as string.pad_left,
% string.pad_right, string.format (with field widths), string.word_wrap, etc.

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
        error("list length mismatch in get_next_line")
    ;
        Column = [ColumnTop | ColumnRest]
    ),
    get_next_line(Columns, ColumnTops, ColumnRests).

:- pred pad_row(assoc_list(justify_sense, int)::in, list(string)::in,
    string::in, int::in, int::in, string::out) is det.

pad_row([], [], _, _, _, "").
pad_row([Justify - MaxWidth | JustifyWidths], [ColumnString0 | ColumnStrings0],
        Separator, SepLen, CurColumn, Line) :-
    NextColumn = CurColumn + MaxWidth + SepLen,
    pad_row(JustifyWidths, ColumnStrings0, Separator, SepLen, NextColumn,
        LineRest),
    ( string.count_codepoints(ColumnString0) =< MaxWidth ->
        (
            Justify = just_left,
            ColumnString = string.pad_right(ColumnString0, ' ', MaxWidth)
        ;
            Justify = just_right,
            ColumnString = string.pad_left(ColumnString0, ' ', MaxWidth)
        ),
        (
            JustifyWidths = [],
            Line = ColumnString
        ;
            JustifyWidths = [_ | _],
            Line = ColumnString ++ Separator ++ LineRest
        )
    ;
        (
            JustifyWidths = [],
            Line = ColumnString0
        ;
            JustifyWidths = [_ | _],
            Line = ColumnString0 ++ Separator ++ "\n" ++
                string.duplicate_char(' ', NextColumn) ++ LineRest
        )
    ).
pad_row([], [_ | _], _, _, _, _) :-
    error("list length mismatch in pad_row").
pad_row([_ | _], [], _, _, _, _) :-
    error("list length mismatch in pad_row").

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

pad_column(Width, left(Strings)) = list.map(string.rpad(' ', Width), Strings).
pad_column(Width, right(Strings)) = list.map(string.lpad(' ', Width), Strings).

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

%-----------------------------------------------------------------------------%

word_wrap(Str, N) = word_wrap_separator(Str, N, "").

word_wrap_separator(Str, N, WordSep) = Wrapped :-
    Words = string.words_separator(char.is_whitespace, Str),
    SepLen = string.count_codepoints(WordSep),
    ( SepLen < N ->
        word_wrap_2(Words, WordSep, SepLen, 1, N, [], Wrapped)
    ;
        word_wrap_2(Words, "", 0, 1, N, [], Wrapped)
    ).

:- pred word_wrap_2(list(string)::in, string::in, int::in, int::in, int::in,
    list(string)::in, string::out) is det.

word_wrap_2([], _, _, _, _, RevStrs,
    string.join_list("", list.reverse(RevStrs))).

word_wrap_2([Word | Words], WordSep, SepLen, Col, N, Prev, Wrapped) :-
    % Col is the column where the next character should be written if there
    % is space for a whole word.
    WordLen = string.count_codepoints(Word),
    (
        % We are on the first column and the length of the word
        % is less than the line length.
        Col = 1,
        WordLen < N
    ->
        NewCol = Col + WordLen,
        WrappedRev = [Word | Prev],
        NewWords = Words
    ;
        % The word takes up the whole line.
        Col = 1,
        WordLen = N
    ->
        % We only put a newline if there are more words to follow.
        NewCol = 1,
        (
            Words = [],
            WrappedRev = [Word | Prev]
        ;
            Words = [_ | _],
            WrappedRev = ["\n", Word | Prev]
        ),
        NewWords = Words
    ;
        % If we add a space and the current word to the line we'll still be
        % within the line length limit.
        Col + WordLen < N
    ->
        NewCol = Col + WordLen + 1,
        WrappedRev = [Word, " " | Prev],
        NewWords = Words
    ;
        % Adding the word and a space takes us to the end of the line exactly.
        Col + WordLen = N
    ->
        % We only put a newline if there are more words to follow.
        NewCol = 1,
        (
            Words = [],
            WrappedRev = [Word, " " | Prev]
        ;
            Words = [_ | _],
            WrappedRev = ["\n", Word, " " | Prev]
        ),
        NewWords = Words
    ;
        % Adding the word would take us over the line limit.
        ( Col = 1 ->
            % Break up words that are too big to fit on a line.
            RevPieces = break_up_string_reverse(Word, N - SepLen, []),
            (
                RevPieces = [LastPiece | Rest]
            ;
                RevPieces = [],
                error("string.word_wrap_2: no pieces")
            ),
            RestWithSep = list.map(func(S) = S ++ WordSep ++ "\n", Rest),
            NewCol = 1,
            WrappedRev = list.append(RestWithSep, Prev),
            NewWords = [LastPiece | Words]
        ;
            NewCol = 1,
            WrappedRev = ["\n" | Prev],
            NewWords = [Word | Words]
        )
    ),
    word_wrap_2(NewWords, WordSep, SepLen, NewCol, N, WrappedRev, Wrapped).

:- func break_up_string_reverse(string, int, list(string)) = list(string).

break_up_string_reverse(Str, N, Prev) = Strs :-
    ( string.count_codepoints(Str) =< N ->
        Strs = [Str | Prev]
    ;
        string.split_by_codepoint(Str, N, Left, Right),
        Strs = break_up_string_reverse(Right, N, [Left | Prev])
    ).

%-----------------------------------------------------------------------------%

string.string_to_doc(S) = docs([str("\""), str(S), str("\"")]).

%-----------------------------------------------------------------------------%
