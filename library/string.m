%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
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
% Note that in the current implementation, strings are represented as in C,
% using a null character as the string terminator. Future implementations,
% however, might allow null characters in strings. Programmers should
% avoid creating strings that might contain null characters.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module string.
:- interface.

:- import_module char.
:- import_module deconstruct.
:- import_module list.
:- import_module ops.

%-----------------------------------------------------------------------------%

    % Determine the length of a string.
    % An empty string has length zero.
    %
:- func string.length(string::in) = (int::uo) is det.
:- pred string.length(string, int).
:- mode string.length(in, uo) is det.
:- mode string.length(ui, uo) is det.

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
    % Converts a character (single-character atom) to a string or vice versa.
    %
:- func string.char_to_string(char::in) = (string::uo) is det.
:- pred string.char_to_string(char, string).
:- mode string.char_to_string(in, uo) is det.
:- mode string.char_to_string(out, in) is semidet.

    % A synonym for string.int_to_char/1.
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
    % character of String, and Rest is the remainder.
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
    %
:- func string.to_lower(string::in) = (string::uo) is det.
:- pred string.to_lower(string, string).
:- mode string.to_lower(in, uo) is det.
:- mode string.to_lower(in, in) is semidet.        % implied

    % Converts a string to uppercase.
    %
:- func string.to_upper(string::in) = (string::uo) is det.
:- pred string.to_upper(string, string).
:- mode string.to_upper(in, uo) is det.
:- mode string.to_upper(in, in) is semidet.        % implied

    % Convert the first character (if any) of a string to uppercase.
    %
:- func string.capitalize_first(string) = string.
:- pred string.capitalize_first(string::in, string::out) is det.

    % Convert the first character (if any) of a string to lowercase.
    %
:- func string.uncapitalize_first(string) = string.
:- pred string.uncapitalize_first(string::in, string::out) is det.

    % Convert the string to a list of characters.
    %
:- func string.to_char_list(string) = list(char).
:- pred string.to_char_list(string, list(char)).
:- mode string.to_char_list(in, out) is det.
:- mode string.to_char_list(uo, in) is det.

    % Convert a list of characters to a string.
    %
:- func string.from_char_list(list(char)::in) = (string::uo) is det.
:- pred string.from_char_list(list(char), string).
:- mode string.from_char_list(in, uo) is det.
:- mode string.from_char_list(out, in) is det.

    % Same as string.from_char_list, except that it reverses the order
    % of the characters.
    %
:- func string.from_rev_char_list(list(char)::in) = (string::uo) is det.
:- pred string.from_rev_char_list(list(char)::in, string::uo) is det.

    % Converts a signed base 10 string to an int; throws an exception
    % if the string argument does not match the regexp [+-]?[0-9]+
    %
:- func string.det_to_int(string) = int.

    % Convert a string to an int. The string must contain only digits,
    % optionally preceded by a plus or minus sign. If the string does
    % not match this syntax, string.to_int fails.
:- pred string.to_int(string::in, int::out) is semidet.

    % Convert a string in the specified base (2-36) to an int. The string
    % must contain one or more digits in the specified base, optionally
    % preceded by a plus or minus sign. For bases > 10, digits 10 to 35
    % are represented by the letters A-Z or a-z. If the string does not match
    % this syntax, the predicate fails.
    %
:- pred string.base_string_to_int(int::in, string::in, int::out) is semidet.

    % Converts a signed base N string to an int; throws an exception
    % if the string argument is not precisely an optional sign followed by
    % a non-empty string of base N digits.
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

    % True if string contains only alphabetic characters (letters).
    %
:- pred string.is_all_alpha(string::in) is semidet.

    % True if string contains only alphabetic characters and underscores.
    %
:- pred string.is_all_alpha_or_underscore(string::in) is semidet.

    % True if string contains only letters, digits, and underscores.
    %
:- pred string.is_all_alnum_or_underscore(string::in) is semidet.

    % string.pad_left(String0, PadChar, Width, String):
    % Insert `PadChar's at the left of `String0' until it is at least as long
    % as `Width', giving `String'.
    %
:- func string.pad_left(string, char, int) = string.
:- pred string.pad_left(string::in, char::in, int::in, string::out) is det.

    % string.pad_right(String0, PadChar, Width, String):
    % Insert `PadChar's at the right of `String0' until it is at least as long
    % as `Width', giving `String'.
    %
:- func string.pad_right(string, char, int) = string.
:- pred string.pad_right(string::in, char::in, int::in, string::out) is det.

    % string.duplicate_char(Char, Count, String):
    % Construct a string consisting of `Count' occurrences of `Char'
    % in sequence.
    %
:- func string.duplicate_char(char::in, int::in) = (string::uo) is det.
:- pred string.duplicate_char(char::in, int::in, string::uo) is det.

    % string.contains_char(String, Char):
    % Succeed if `Char' occurs in `String'.
    %
:- pred string.contains_char(string::in, char::in) is semidet.

    % string.index(String, Index, Char):
    % `Char' is the (`Index' + 1)-th character of `String'.
    % Fails if `Index' is out of range (negative, or greater than or equal to
    % the length of `String').
    %
:- pred string.index(string::in, int::in, char::uo) is semidet.

    % string.index_det(String, Index, Char):
    % `Char' is the (`Index' + 1)-th character of `String'.
    % Calls error/1 if `Index' is out of range (negative, or greater than
    % or equal to the length of `String').
    %
:- func string.index_det(string, int) = char.
:- pred string.index_det(string::in, int::in, char::uo) is det.

    % A synonym for index_det/2:
    % String ^ elem(Index) = string.index_det(String, Index).
    %
:- func string ^ elem(int) = char.

    % string.unsafe_index(String, Index, Char):
    % `Char' is the (`Index' + 1)-th character of `String'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String').
    % This version is constant time, whereas string.index_det
    % may be linear in the length of the string. Use with care!
    %
:- func string.unsafe_index(string, int) = char.
:- pred string.unsafe_index(string::in, int::in, char::uo) is det.

    % A synonym for unsafe_index/2:
    % String ^ unsafe_elem(Index) = string.unsafe_index(String, Index).
    %
:- func string ^ unsafe_elem(int) = char.

    % string.chomp(String):
    % `String' minus any single trailing newline character.
    %
:- func string.chomp(string) = string.

    % string.lstrip(String):
    % `String' minus any initial whitespace characters.
    %
:- func string.lstrip(string) = string.

    % string.rstrip(String):
    % `String' minus any trailing whitespace characters.
    %
:- func string.rstrip(string) = string.

    % string.strip(String):
    % `String' minus any initial and trailing whitespace characters.
    %
:- func string.strip(string) = string.

    % string.lstrip_pred(Pred, String):
    % `String' minus the maximal prefix consisting entirely of chars
    % satisfying `Pred'.
    %
:- func string.lstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % string.rstrip_pred(Pred, String):
    % `String' minus the maximal suffix consisting entirely of chars
    % satisfying `Pred'.
    %
:- func string.rstrip_pred(pred(char)::in(pred(in) is semidet), string::in)
    = (string::out) is det.

    % string.prefix_length(Pred, String):
    % The length of the maximal prefix of `String' consisting entirely of
    % chars satisfying Pred.
    %
:- func string.prefix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % string.suffix_length(Pred, String):
    % The length of the maximal suffix of `String' consisting entirely of chars
    % satisfying Pred.
    %
:- func suffix_length(pred(char)::in(pred(in) is semidet), string::in)
    = (int::out) is det.

    % string.set_char(Char, Index, String0, String):
    % `String' is `String0' with the (`Index' + 1)-th character set to `Char'.
    % Fails if `Index' is out of range (negative, or greater than or equal to
    % the length of `String0').
    %
:- pred string.set_char(char, int, string, string).
:- mode string.set_char(in, in, in, out) is semidet.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.set_char(in, in, di, uo) is semidet.

    % string.set_char_det(Char, Index, String0, String):
    % `String' is `String0' with the (`Index' + 1)-th character set to `Char'.
    % Calls error/1 if `Index' is out of range (negative, or greater than
    % or equal to the length of `String0').
    %
:- func string.set_char_det(char, int, string) = string.
:- pred string.set_char_det(char, int, string, string).
:- mode string.set_char_det(in, in, in, out) is det.
% XXX This mode is disabled because the compiler puts constant
% strings into static data even when they might be updated.
%:- mode string.set_char_det(in, in, di, uo) is det.

    % string.unsafe_set_char(Char, Index, String0, String):
    % `String' is `String0' with the (`Index' + 1)-th character set to `Char'.
    % WARNING: behavior is UNDEFINED if `Index' is out of range
    % (negative, or greater than or equal to the length of `String0').
    % This version is constant time, whereas string.set_char_det
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
    % character of the string `String' in turn. The initial value of the
    % accumulator is `!.Acc' and the final value is `!:Acc'.
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

    % string.foldl_substring(Closure, String, Start, Count, !Acc)
    % is equivalent to string.foldl(Closure, SubString, !Acc)
    % where SubString = string.substring(String, Start, Count).
    %
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

    % string.foldl_substring2(Closure, String, Start, Count, !Acc1, !Acc2)
    % A variant of string.foldl_substring with two accumulators.
    %
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

    % string.foldr_substring(Closure, String, Start, Count, !Acc)
    % is equivalent to string.foldr(Closure, SubString, !Acc)
    % where SubString = string.substring(String, Start, Count).
    %
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
    % by non-empty sequences of chars matched by SepP. For example,
    %
    % string.words_separator(char.is_whitespace, " the cat  sat on the  mat") =
    %   ["the", "cat", "sat", "on", "the", "mat"]
    %
:- func string.words_separator(pred(char), string) = list(string).
:- mode string.words_separator(pred(in) is semidet, in) = out is det.

    % string.words(String) =
    %   string.words_separator(char.is_whitespace, String).
    %
:- func string.words(string) = list(string).

    % string.split(String, Count, LeftSubstring, RightSubstring):
    % `LeftSubstring' is the left-most `Count' characters of `String',
    % and `RightSubstring' is the remainder of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- pred string.split(string::in, int::in, string::uo, string::uo) is det.

    % string.left(String, Count, LeftSubstring):
    % `LeftSubstring' is the left-most `Count' characters of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func string.left(string::in, int::in) = (string::uo) is det.
:- pred string.left(string::in, int::in, string::uo) is det.

    % string.right(String, Count, RightSubstring):
    % `RightSubstring' is the right-most `Count' characters of `String'.
    % (If `Count' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.)
    %
:- func string.right(string::in, int::in) = (string::uo) is det.
:- pred string.right(string::in, int::in, string::uo) is det.

    % string.substring(String, Start, Count, Substring):
    % `Substring' is first the `Count' characters in what would remain
    % of `String' after the first `Start' characters were removed.
    % (If `Start' is out of the range [0, length of `String'], it is treated
    % as if it were the nearest end-point of that range.
    % If `Count' is out of the range [0, length of `String' - `Start'],
    % it is treated as if it were the nearest end-point of that range.)
    %
:- func string.substring(string::in, int::in, int::in) = (string::uo) is det.
:- pred string.substring(string::in, int::in, int::in, string::uo) is det.

    % string.unsafe_substring(String, Start, Count, Substring):
    % `Substring' is first the `Count' characters in what would remain
    % of `String' after the first `Start' characters were removed.
    % WARNING: if `Start' is out of the range [0, length of `String'],
    % or if `Count' is out of the range [0, length of `String' - `Start'],
    % then the behaviour is UNDEFINED. Use with care!
    % This version takes time proportional to the length of the substring,
    % whereas string.substring may take time proportional to the length
    %% of the whole string.
    %
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

    % string.sub_string_search(String, SubString, Index).
    % `Index' is the position in `String' where the first occurrence of
    % `SubString' begins. Indices start at zero, so if `SubString' is a prefix
    % of `String', this will return Index = 0.
    %
:- pred string.sub_string_search(string::in, string::in, int::out) is semidet.

    % string.sub_string_search_start(String, SubString, BeginAt, Index).
    % `Index' is the position in `String' where the first occurrence of
    % `SubString' occurs such that 'Index' is greater than or equal to
    % `BeginAt'.  Indices start at zero,
    %
:- pred string.sub_string_search_start(string::in, string::in, int::in,
    int::out)
    is semidet.

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
    % The implementation uses the sprintf() function, so the actual output
    % will depend on the C standard library.
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
    % an exception is thrown.
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

:- type justified_column
    --->    left(list(string))
    ;       right(list(string)).

    % word_wrap(Str, N) = Wrapped.
    % Wrapped is Str with newlines inserted between words so that at most
    % N characters appear on a line and each line contains as many whole words
    % as possible. If any one word exceeds N characters in length then it will
    % be broken over two (or more) lines. Sequences of whitespace characters
    % are replaced by a single space.
    %
:- func string.word_wrap(string, int) = string.

    % word_wrap_separator(Str, N, WordSeparator) = Wrapped.
    % word_wrap_separator/3 is like word_wrap/2, except that words that
    % need to be broken up over multiple lines have WordSeparator inserted
    % between each piece. If the length of WordSeparator is greater than
    % or equal to N, then no separator is used.
    %
:- func string.word_wrap_separator(string, int, string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module require.
:- import_module std_util.
:- import_module type_desc.
:- import_module univ.

:- use_module rtti_implementation.
:- use_module term_io.

%-----------------------------------------------------------------------------%

string.replace(Str, Pat, Subst, Result) :-
    sub_string_search(Str, Pat, Index),

    Initial = string.unsafe_substring(Str, 0, Index),

    BeginAt = Index + string.length(Pat),
    Length = string.length(Str) - BeginAt,
    Final = string.unsafe_substring(Str, BeginAt, Length),

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
        Length = Index - BeginAt,
        Initial = string.unsafe_substring(Str, BeginAt, Length),
        Start = Index + PatLength,
        Result = string.replace_all_2(Str, Pat, Subst, PatLength, Start,
            [Subst, Initial | Result0])
    ;
        Length = string.length(Str) - BeginAt,
        EndString = string.unsafe_substring(Str, BeginAt, Length),
        Result = [EndString | Result0]
    ).

string.to_int(String, Int) :-
    string.base_string_to_int(10, String, Int).

string.base_string_to_int(Base, String, Int) :-
    string.index(String, 0, Char),
    Len = string.length(String),
    ( Char = ('-') ->
        Len > 1,
        foldl_substring(accumulate_int(Base), String, 1, Len - 1, 0, N),
        Int = -N
    ; Char = ('+') ->
        Len > 1,
        foldl_substring(accumulate_int(Base), String, 1, Len - 1, 0, N),
        Int = N
    ;
        foldl_substring(accumulate_int(Base), String, 0, Len, 0, N),
        Int = N
    ).

:- pred accumulate_int(int::in, char::in, int::in, int::out) is semidet.

accumulate_int(Base, Char, N, (Base * N) + M) :-
    char.digit_to_int(Char, M),
    M < Base.

% It is important to inline string.index and string.index_det.
% so that the compiler can do loop invariant hoisting
% on calls to string.length that occur in loops.
:- pragma inline(string.index_det/3).

string.index_det(String, Int, Char) :-
    ( string.index(String, Int, Char0) ->
        Char = Char0
    ;
        error("string.index_det: index out of range")
    ).

String ^ elem(Index) = index_det(String, Index).

string.set_char_det(Char, Int, String0, String) :-
    ( string.set_char(Char, Int, String0, String1) ->
        String = String1
    ;
        error("string.set_char_det: index out of range")
    ).

string.foldl(Closure, String, !Acc) :-
    string.length(String, Length),
    string.foldl_substring(Closure, String, 0, Length, !Acc).

string.foldl2(Closure, String, !Acc1, !Acc2) :-
    string.length(String, Length),
    string.foldl2_substring(Closure, String, 0, Length, !Acc1, !Acc2).

string.foldl_substring(Closure, String, Start0, Count0, !Acc) :-
    Start = max(0, Start0),
    Count = min(Count0, length(String) - Start),
    string.foldl_substring_2(Closure, String, Start, Count, !Acc).

string.foldl2_substring(Closure, String, Start0, Count0, !Acc1, !Acc2) :-
    Start = max(0, Start0),
    Count = min(Count0, length(String) - Start),
    string.foldl2_substring_2(Closure, String, Start, Count, !Acc1, !Acc2).

:- pred string.foldl_substring_2(pred(char, A, A), string, int, int, A, A).
:- mode string.foldl_substring_2(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldl_substring_2(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldl_substring_2(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldl_substring_2(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldl_substring_2(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

string.foldl_substring_2(Closure, String, I, Count, !Acc) :-
    ( 0 < Count ->
        Closure(string.unsafe_index(String, I), !Acc),
        string.foldl_substring_2(Closure, String, I + 1, Count - 1, !Acc)
    ;
        true
    ).

:- pred string.foldl2_substring_2(pred(char, A, A, B, B), string, int, int,
    A, A, B, B).
:- mode string.foldl2_substring_2(pred(in, di, uo, di, uo) is det,
    in, in, in, di, uo, di, uo) is det.
:- mode string.foldl2_substring_2(pred(in, in, out, di, uo) is det,
    in, in, in, in, out, di, uo) is det.
:- mode string.foldl2_substring_2(pred(in, in, out, in, out) is det,
    in, in, in, in, out, in, out) is det.
:- mode string.foldl2_substring_2(pred(in, in, out, in, out) is semidet,
    in, in, in, in, out, in, out) is semidet.
:- mode string.foldl2_substring_2(pred(in, in, out, in, out) is nondet,
    in, in, in, in, out, in, out) is nondet.
:- mode string.foldl2_substring_2(pred(in, in, out, in, out) is multi,
    in, in, in, in, out, in, out) is multi.

string.foldl2_substring_2(Closure, String, I, Count, !Acc1, !Acc2) :-
    ( 0 < Count ->
        Closure(string.unsafe_index(String, I), !Acc1, !Acc2),
        string.foldl2_substring_2(Closure, String, I + 1, Count - 1,
            !Acc1, !Acc2)
    ;
        true
    ).

string.foldr(F, String, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y)),
    string.foldr(Closure, String, Acc0, Acc).

string.foldr_substring(F, String, Start, Count, Acc0) = Acc :-
    Closure = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldr_substring(Closure, String, Start, Count, Acc0, Acc).

string.foldr(Closure, String, Acc0, Acc) :-
    string.foldr_substring(Closure, String, 0, length(String), Acc0, Acc).

string.foldr_substring(Closure, String, Start0, Count0, Acc0, Acc) :-
    Start = max(0, Start0),
    Count = min(Count0, length(String) - Start),
    string.foldr_substring_2(Closure, String, Start, Count, Acc0, Acc).

:- pred string.foldr_substring_2(pred(char, T, T), string, int, int, T, T).
:- mode string.foldr_substring_2(pred(in, in, out) is det, in, in, in,
    in, out) is det.
:- mode string.foldr_substring_2(pred(in, di, uo) is det, in, in, in,
    di, uo) is det.
:- mode string.foldr_substring_2(pred(in, in, out) is semidet, in, in, in,
    in, out) is semidet.
:- mode string.foldr_substring_2(pred(in, in, out) is nondet, in, in, in,
    in, out) is nondet.
:- mode string.foldr_substring_2(pred(in, in, out) is multi, in, in, in,
    in, out) is multi.

string.foldr_substring_2(Closure, String, I, Count, !Acc) :-
    ( 0 < Count ->
        Closure(string.unsafe_index(String, I + Count - 1), !Acc),
        string.foldr_substring_2(Closure, String, I, Count - 1, !Acc)
    ;
        true
    ).

string.left(String, Count, LeftString) :-
    string.split(String, Count, LeftString, _RightString).

string.right(String, RightCount, RightString) :-
    string.length(String, Length),
    LeftCount = Length - RightCount,
    string.split(String, LeftCount, _LeftString, RightString).

string.remove_suffix(A, B, C) :-
    string.to_char_list(A, LA),
    string.to_char_list(B, LB),
    string.to_char_list(C, LC),
    char_list_remove_suffix(LA, LB, LC).

:- pragma promise_equivalent_clauses(string.prefix/2).

string.prefix(String::in, Prefix::in) :-
    Len    = length(String),
    PreLen = length(Prefix),
    PreLen =< Len,
    prefix_2_iii(String, Prefix, PreLen - 1).

:- pred prefix_2_iii(string::in, string::in, int::in) is semidet.

prefix_2_iii(String, Prefix, I) :-
    ( 0 =< I ->
        (String `unsafe_index` I) = (Prefix `unsafe_index` I) `with_type` char,
        prefix_2_iii(String, Prefix, I - 1)
    ;
        true
    ).

string.prefix(String::in, Prefix::out) :-
    Len = length(String),
    prefix_2_ioii(String, Prefix, 0, Len).

:- pred prefix_2_ioii(string::in, string::out, int::in, int::in) is multi.

prefix_2_ioii(String, Prefix, PreLen, _Len) :-
    Prefix = unsafe_substring(String, 0, PreLen).

prefix_2_ioii(String, Prefix, PreLen, Len) :-
    PreLen < Len,
    prefix_2_ioii(String, Prefix, PreLen + 1, Len).

:- pragma promise_equivalent_clauses(string.suffix/2).

string.suffix(String::in, Suffix::in) :-
    Len    = length(String),
    PreLen = length(Suffix),
    PreLen =< Len,
    suffix_2_iiii(String, Suffix, 0, Len - PreLen, PreLen).

:- pred suffix_2_iiii(string::in, string::in, int::in, int::in, int::in)
    is semidet.

suffix_2_iiii(String, Suffix, I, Offset, Len) :-
    ( I < Len ->
        (String `unsafe_index` (I + Offset)) =
            (Suffix `unsafe_index` I) `with_type` char,
        suffix_2_iiii(String, Suffix, I + 1, Offset, Len)
    ;
        true
    ).

string.suffix(String::in, Suffix::out) :-
    Len = length(String),
    suffix_2_ioii(String, Suffix, 0, Len).

:- pred suffix_2_ioii(string::in, string::out, int::in, int::in) is multi.

suffix_2_ioii(String, Suffix, SufLen, Len) :-
    Suffix = unsafe_substring(String, Len - SufLen, SufLen).

suffix_2_ioii(String, Suffix, SufLen, Len) :-
    SufLen < Len,
    suffix_2_ioii(String, Suffix, SufLen + 1, Len).

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
        string.int_to_base_string_2(N, Base, Str1),
        string.append("-", Str1, Str)
    ;
        N1 = 0 - N,
        string.int_to_base_string_2(N1, Base, Str)
    ).

:- pred string.int_to_base_string_2(int::in, int::in, string::uo) is det.

    % string.int_to_base_string_2/3 is almost identical to
    % string.int_to_base_string_group_2/6 below so any changes here might
    % also need to be applied to string.int_to_base_string_group_2/3.
    %
string.int_to_base_string_2(NegN, Base, Str) :-
    ( NegN > -Base ->
        N = -NegN,
        char.det_int_to_digit(N, DigitChar),
        string.char_to_string(DigitChar, Str)
    ;
        NegN1 = NegN // Base,
        N10 = (NegN1 * Base) - NegN,
        char.det_int_to_digit(N10, DigitChar),
        string.char_to_string(DigitChar, DigitString),
        string.int_to_base_string_2(NegN1, Base, Str1),
        string.append(Str1, DigitString, Str)
    ).

string.c_pointer_to_string(C_Pointer, Str) :-
    private_builtin.unsafe_type_cast(C_Pointer, Int),
    Str = "c_pointer(0x" ++ string.int_to_base_string(Int, 16) ++ ")".

string.from_char_list(CharList, Str) :-
    string.to_char_list(Str, CharList).

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

:- pragma foreign_proc("C",
    string.to_char_list(Str::in, CharList::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_ConstString p = Str + strlen(Str);
    CharList = MR_list_empty_msg(MR_PROC_LABEL);
    while (p > Str) {
        p--;
        CharList = MR_char_list_cons_msg((MR_UnsignedChar) *p, CharList,
            MR_PROC_LABEL);
    }
}").

:- pragma foreign_proc("C",
    string.to_char_list(Str::uo, CharList::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    /* mode (uo, in) is det */
    MR_Word char_list_ptr;
    size_t size;

    /*
    ** Loop to calculate list length + sizeof(MR_Word) in `size'
    ** using list in `char_list_ptr'.
    */
    size = sizeof(MR_Word);
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        size++;
        char_list_ptr = MR_list_tail(char_list_ptr);
    }

    /*
    ** Allocate (length + 1) bytes of heap space for string
    ** i.e. (length + 1 + sizeof(MR_Word) - 1) / sizeof(MR_Word) words.
    */
    MR_allocate_aligned_string_msg(Str, size, MR_PROC_LABEL);

    /*
    ** Loop to copy the characters from the char_list to the string.
    */
    size = 0;
    char_list_ptr = CharList;
    while (! MR_list_is_empty(char_list_ptr)) {
        Str[size++] = MR_list_head(char_list_ptr);
        char_list_ptr = MR_list_tail(char_list_ptr);
    }

    Str[size] = '\\0';
}").

:- pragma promise_equivalent_clauses(string.to_char_list/2).

string.to_char_list(Str::in, CharList::out) :-
    string.to_char_list_2(Str, 0, CharList).
string.to_char_list(Str::uo, CharList::in) :-
    (
        CharList = [],
        Str = ""
    ;
        CharList = [C | Cs],
        string.to_char_list(Str0, Cs),
        string.first_char(Str, C, Str0)
    ).

:- pred string.to_char_list_2(string::in, int::in, list(char)::uo) is det.

string.to_char_list_2(Str, Index, CharList) :-
    ( string.index(Str, Index, Char) ->
        string.to_char_list_2(Str, Index + 1, CharList0),
        CharList = [Char | CharList0]
    ;
        CharList = []
    ).

%---------------------------------------------------------------------------%

% We could implement from_rev_char_list using list.reverse and from_char_list,
% but the optimized implementation in C below is there for efficiency since
% it improves the overall speed of parsing by about 7%.

:- pragma foreign_proc("C",
    string.from_rev_char_list(Chars::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_Word list_ptr;
    MR_Word size, len;

    /*
    ** Loop to calculate list length + sizeof(MR_Word) in `size'
    ** using list in `list_ptr' and separately count the length of the string.
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
    ** Allocate (length + 1) bytes of heap space for string
    ** i.e. (length + 1 + sizeof(MR_Word) - 1) / sizeof(MR_Word) words.
    */
    MR_allocate_aligned_string_msg(Str, size, MR_PROC_LABEL);

    /*
    ** Set size to be the offset of the end of the string
    ** (ie the \\0) and null terminate the string.
    */
    Str[--len] = '\\0';

    /*
    ** Loop to copy the characters from the list_ptr to the string
    ** in reverse order.
    */
    list_ptr = Chars;
    while (!MR_list_is_empty(list_ptr)) {
        Str[--len] = (MR_Char) MR_list_head(list_ptr);
        list_ptr = MR_list_tail(list_ptr);
    }
}").

string.from_rev_char_list(Chars::in, Str::uo) :-
    Str = string.from_char_list(list.reverse(Chars)).

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

:- pred string.all_match(pred(char)::in(pred(in) is semidet), string::in)
    is semidet.

string.all_match(P, String) :-
    all_match_2(string.length(String) - 1, P, String).

:- pred all_match_2(int::in, pred(char)::in(pred(in) is semidet), string::in)
    is semidet.

string.all_match_2(I, P, String) :-
    ( I >= 0 ->
        P(string.unsafe_index(String, I)),
        string.all_match_2(I - 1, P, String)
    ;
        true
    ).

string.is_all_alpha(S) :-
    string.all_match(char.is_alpha, S).

string.is_all_alpha_or_underscore(S) :-
    string.all_match(char.is_alpha_or_underscore, S).

string.is_all_alnum_or_underscore(S) :-
    string.all_match(char.is_alnum_or_underscore, S).

string.pad_left(String0, PadChar, Width, String) :-
    string.length(String0, Length),
    ( Length < Width ->
        Count = Width - Length,
        string.duplicate_char(PadChar, Count, PadString),
        string.append(PadString, String0, String)
    ;
        String = String0
    ).

string.pad_right(String0, PadChar, Width, String) :-
    string.length(String0, Length),
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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
    MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);

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

    % We implement string.join_list in C as this minimises the amount of
    % garbage created.
:- pragma foreign_proc("C",
    string.join_list(Sep::in, Strs::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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

    MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);

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

string.append_list(Strs::in) = (Str::uo) :-
    (
        Strs = [X | Xs],
        Str = X ++ append_list(Xs)
    ;
        Strs = [],
        Str = ""
    ).

string.join_list(_, []) = "".
string.join_list(Sep, [H | T]) = H ++ string.join_list_2(Sep, T).

:- func join_list_2(string::in, list(string)::in) = (string::uo) is det.

join_list_2(_, []) = "".
join_list_2(Sep, [H | T]) = Sep ++ H ++ join_list_2(Sep, T).

%-----------------------------------------------------------------------------%

    % NOTE: string.hash is also defined as MR_hash_string in 
    %       runtime/mercury_string.h.  The two definitions must be kept
    %       identical.
    %
string.hash(String, HashVal) :-
    string.length(String, Length),
    string.hash_2(String, 0, Length, 0, HashVal0),
    HashVal = HashVal0 `xor` Length.

:- pred string.hash_2(string::in, int::in, int::in, int::in, int::out) is det.

string.hash_2(String, Index, Length, !HashVal) :-
    ( Index < Length ->
        string.combine_hash(char.to_int(string.unsafe_index(String, Index)),
            !HashVal),
        string.hash_2(String, Index + 1, Length, !HashVal)
    ;
        true
    ).

:- pred string.combine_hash(int::in, int::in, int::out) is det.

string.combine_hash(X, H0, H) :-
    H1 = H0 `xor` (H0 << 5),
    H = H1 `xor` X.

%-----------------------------------------------------------------------------%

string.sub_string_search(WholeString, Pattern, Index) :-
    sub_string_search_start(WholeString, Pattern, 0, Index).

:- pragma foreign_proc("C",
    sub_string_search_start(WholeString::in, Pattern::in, BeginAt::in,
        Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    char *match;
    match = strstr(WholeString + BeginAt, Pattern);
    if (match) {
        Index = match - WholeString;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
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
        substring(String, I, SubLength) = SubString
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
    --->    conv(
                flags       :: list(char),
                width       :: maybe(list(char)),
                precision   :: maybe(list(char)),
                spec        :: spec
            )
    ;       string(list(char)).

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
        Specifiers = [string(NonConversionSpecChars), ConversionSpec
            | Specifiers0]
    ;
        Specifiers = [string(NonConversionSpecChars)]
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
        Specificier = conv(Flags, MaybeWidth, MaybePrec, Spec)
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

        char_list_remove_suffix(Init, Final, Width)
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
        char_list_remove_suffix(Init, Final, Prec)
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

specifier_to_string(conv(Flags, Width, Prec, Spec)) = String :-
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
        ( using_sprintf ->
            FormatStr = make_format(Flags, Width, Prec, "", "c"),
            String = native_format_char(FormatStr, Char)
        ;
            String = format_char(Flags, conv(Width), Char)
        )
    ;
        % Valid string conversion Specifiers.
        Spec = s(Str),
        ( using_sprintf ->
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
specifier_to_string(string(Chars)) = from_char_list(Chars).

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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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
    succeeded = false;
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_PROC_LABEL, FormatStr, (double) Val);
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_PROC_LABEL, FormatStr, Val);
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_PROC_LABEL, FormatStr, Val);
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_save_transient_hp();
    Str = MR_make_string(MR_PROC_LABEL, FormatStr, Val);
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
        PrecStr = string.substring(OldStr, 0, NumChars)
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
    AbsIntStrLength = string.length(AbsIntStr),

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
        FieldWidth > string.length(PrecStr),
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
    ( Int < 0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
        SignedStr = " " ++ FieldStr
    ; ZeroPadded = yes ->
        SignedStr = "0" ++ FieldStr
    ;
        SignedStr = FieldStr
    ),

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
    AbsIntStrLength = string.length(AbsIntStr),

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
        FieldWidth > string.length(PrecModStr),
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
            PrecStrLen = string.length(PrecStr),
            PrecModStr = string.substring(PrecStr, 0, PrecStrLen - 1)
        ;
            PrecModStr = PrecStr
        )
    ),

    % Do we need to change field width?
    (
        Width = yes(FieldWidth),
        FieldWidth > string.length(PrecModStr),
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
    ( Float < 0.0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
        SignedStr = " " ++ FieldStr
    ; ZeroPadded = yes ->
        SignedStr = "0" ++ FieldStr
    ;
        SignedStr = FieldStr
    ),

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
        FieldWidth > string.length(PrecStr),
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
    ( Float < 0.0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
        SignedStr = " " ++ FieldStr
    ; ZeroPadded = yes ->
        SignedStr = "0" ++ FieldStr
    ;
        SignedStr = FieldStr
    ),

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
        FieldWidth > string.length(PrecModStr),
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
    ( Float < 0.0 ->
        SignedStr = "-" ++ FieldStr
    ; member('+', Flags) ->
        SignedStr = "+" ++ FieldStr
    ; member(' ', Flags) ->
        SignedStr = " " ++ FieldStr
    ; ZeroPadded = yes ->
        SignedStr = "0" ++ FieldStr
    ;
        SignedStr = FieldStr
    ),

    NewFloat = justify_string(Flags, Width, SignedStr).

:- func justify_string(flags, maybe_width, string) = string.

justify_string(Flags, Width, Str) =
    (
        Width = yes(FWidth),
        FWidth > string.length(Str)
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
            RestMantissaStr = substring(FractionStr, 0, Exp),
            NewFraction = substring(FractionStr, Exp, Prec - Exp - 1),
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
    ( string.length(MantissaStr) > 1 ->
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
    ( string.length(MantissaStr) > 1 ->
        % We will need need to move decimal pt one place to the left:
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
    NumZeros = string.length(MantissaStr) - 1,
    Pos = find_non_zero_pos(string.to_char_list(Float), NumZeros).

    % Given a list of chars representing a floating point number, this
    % function determines the the first position containing a non-zero digit.
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
        PaddedMantissaStr = string.substring(FractionStr, 0, DecimalPos),

        % Get rid of superfluous zeros.
        MantissaInt = string.det_to_int(PaddedMantissaStr),
        ExpMantissaStr = string.int_to_string(MantissaInt),

        % Create fractional part.
        PaddedFractionStr = pad_right(FractionStr, '0', Prec + 1),
        ExpFractionStr = string.substring(PaddedFractionStr, DecimalPos,
            Prec + 1)
    ; Place > 0 ->
        ExpMantissaStr = string.substring(MantissaStr, 0, 1),
        FirstHalfOfFractionStr = string.substring(MantissaStr, 1, Place),
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
    FracStrLen = string.length(FractionStr),
    ( Prec > FracStrLen ->
        PrecFracStr = string.pad_right(FractionStr, '0', Prec),
        PrecMantissaStr = MantissaStr
    ; Prec < FracStrLen ->
        UnroundedFrac = string.substring(FractionStr, 0, Prec),
        NextDigit = string.index_det(FractionStr, Prec),
        (
            UnroundedFrac \= "",
            (char.to_int(NextDigit) - char.to_int('0')) >= 5
        ->
            NewPrecFrac = string.det_to_int(UnroundedFrac) + 1,
            NewPrecFracStrNotOK = string.int_to_string( NewPrecFrac),
            NewPrecFracStr = string.pad_left(NewPrecFracStrNotOK, '0', Prec),
            ( string.length(NewPrecFracStr) > string.length(UnroundedFrac) ->
                PrecFracStr = substring(NewPrecFracStr, 1, Prec),
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
    list.index0_det(FloatAndExponent, 0, Float),
    list.index0_det(FloatAndExponent, 1, Exponent).

:- pred split_at_decimal_point(string::in, string::out, string::out) is det.

split_at_decimal_point(Str, Mantissa, Fraction) :-
    MantAndFrac = string.words_separator(is_decimal_point, Str),
    list.index0_det(MantAndFrac, 0, Mantissa),
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    /*
    ** For efficiency reasons we duplicate the C implementation
    ** of string.lowlevel_float_to_string.
    */
    MR_float_to_string(Flt, Str);
}").

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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    /*
    ** Note any changes here will require the same changes in
    ** string.float_to_string.
    */
    MR_float_to_string(Flt, Str);
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

string.det_to_float(FloatString) =
    ( string.to_float(FloatString, FloatVal) ->
      FloatVal
    ;
      func_error("string.det_to_float/1 - conversion failed.")
    ).

:- pragma foreign_export("C", string.to_float(in, out), "ML_string_to_float").

:- pragma foreign_proc("C",
    string.to_float(FloatString::in, FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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
        } catch (System.FormatException e) {
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
        succeeded = false;
    } else {
        try {
            FloatVal = java.lang.Double.parseDouble(FloatString);
            succeeded = true;
        } catch(java.lang.NumberFormatException e) {
            // At this point it *should* in theory be safe just to set
            // succeeded = false, since the Java API claims that
            // Double.parseDouble() will handle all the cases we require.
            // However, it turns out that in practice (tested with Sun's
            // Java 2 SDK, Standard Edition, version 1.3.1_04) Java actually
            // throws a NumberFormatException when you give it NaN or infinity,
            // so we handle these cases below.

            if (FloatString.equalsIgnoreCase(""nan"")) {
                FloatVal = java.lang.Double.NaN;
                succeeded = true;
            } else if (FloatString.equalsIgnoreCase(""infinity"")) {
                FloatVal = java.lang.Double.POSITIVE_INFINITY;
                succeeded = true;
            } else if (FloatString.substring(1).equalsIgnoreCase(""infinity""))
            {
                if (FloatString.charAt(0) == '+') {
                    FloatVal = java.lang.Double.POSITIVE_INFINITY;
                    succeeded = true;
                } else if (FloatString.charAt(0) == '-') {
                    FloatVal = java.lang.Double.NEGATIVE_INFINITY;
                    succeeded = true;
                } else {
                    succeeded = false;
                }
            } else {
                succeeded = false;
            }
        }
    }
").

/*-----------------------------------------------------------------------*/

    % strchr always returns true when searching for '\0',
    % but the '\0' is an implementation detail which really
    % shouldn't be considered to be part of the string itself.
:- pragma foreign_proc("C",
    string.contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (strchr(Str, Ch) != NULL) && Ch != '\\0';
").
:- pragma foreign_proc("C#",
    string.contains_char(Str::in, Ch::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Str.IndexOf(Ch) != -1);
").
string.contains_char(String, Char) :-
    string.contains_char(String, Char, 0, string.length(String)).

:- pred string.contains_char(string::in, char::in, int::in, int::in)
    is semidet.

string.contains_char(Str, Char, Index, Length) :-
    ( Index < Length ->
        string.unsafe_index(Str, Index, IndexChar),
        ( IndexChar = Char ->
            true
        ;
            string.contains_char(Str, Char, Index + 1, Length)
        )
    ;
        fail
    ).

/*-----------------------------------------------------------------------*/

% It's important to inline string.index and string.index_det.
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

:- pred string.index_check(int::in, int::in) is semidet.

% We should consider making this routine a compiler built-in.
:- pragma foreign_proc("C",
    string.index_check(Index::in, Length::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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

:- pragma foreign_proc("C",
    string.unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Ch = Str[Index];
").
:- pragma foreign_proc("C#",
    string.unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ch = Str[Index];
").
:- pragma foreign_proc("Java",
    string.unsafe_index(Str::in, Index::in, Ch::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ch = Str.charAt(Index);
").
string.unsafe_index(Str, Index, Char) :-
    ( string.first_char(Str, First, Rest) ->
        ( Index = 0 ->
            Char = First
        ;
            string.unsafe_index(Rest, Index - 1, Char)
        )
    ;
        error("string.unsafe_index: out of bounds")
    ).

String ^ unsafe_elem(Index) = unsafe_index(String, Index).

/*-----------------------------------------------------------------------*/

:- pragma foreign_decl("C",
"
#ifdef MR_USE_GCC_GLOBAL_REGISTERS
    /*
    ** GNU C version egcs-1.1.2 crashes with `fixed or forbidden register
    ** spilled' in grade asm_fast.gc.tr.debug if we write this inline.
    */
    extern void MR_set_char(MR_String str, MR_Integer ind, MR_Char ch);
#else
    #define MR_set_char(str, ind, ch) \\
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
    void MR_set_char(MR_String str, MR_Integer ind, MR_Char ch)
    {
        str[ind] = ch;
    }
#endif
").

:- pragma foreign_proc("C",
    string.set_char(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    size_t len = strlen(Str0);
    if ((MR_Unsigned) Index >= len) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
        MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);
        strcpy(Str, Str0);
        MR_set_char(Str, Index, Ch);
    }
").
:- pragma foreign_proc("C#",
    string.set_char(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Index >= Str0.Length) {
        SUCCESS_INDICATOR = false;
    } else {
        Str = System.String.Concat(Str0.Substring(0, Index),
            System.Convert.ToString(Ch),
            Str0.Substring(Index + 1));
        SUCCESS_INDICATOR = true;
    }
").
string.set_char(Ch, Index, Str0, Str) :-
    string.to_char_list(Str0, List0),
    list.replace_nth(List0, Index + 1, Ch, List),
    string.to_char_list(Str, List).

% :- pragma foreign_proc("C",
%   string.set_char(Ch::in, Index::in, Str0::di, Str::uo),
%   [will_not_call_mercury, promise_pure, thread_safe],
% "
%   if ((MR_Unsigned) Index >= strlen(Str0)) {
%       SUCCESS_INDICATOR = MR_FALSE;
%   } else {
%       SUCCESS_INDICATOR = MR_TRUE;
%       Str = Str0;
%       MR_set_char(Str, Index, Ch);
%   }
% ").
%
% :- pragma foreign_proc("C#",
%   string.set_char(Ch::in, Index::in, Str0::di, Str::uo),
%   [will_not_call_mercury, promise_pure, thread_safe],
% "
%   if (Index >= Str0.Length) {
%       SUCCESS_INDICATOR = false;
%   } else {
%       Str = System.String.Concat(Str0.Substring(0, Index),
%           System.Convert.ToString(Ch),
%           Str0.Substring(Index + 1));
%       SUCCESS_INDICATOR = true;
%   }
% ").

/*-----------------------------------------------------------------------*/

:- pragma foreign_proc("C",
    string.unsafe_set_char(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    size_t len = strlen(Str0);
    MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);
    strcpy(Str, Str0);
    MR_set_char(Str, Index, Ch);
").
:- pragma foreign_proc("C#",
    string.unsafe_set_char(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = System.String.Concat(Str0.Substring(0, Index),
        System.Convert.ToString(Ch),
        Str0.Substring(Index + 1));
").
:- pragma foreign_proc("Java",
    string.unsafe_set_char(Ch::in, Index::in, Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = Str0.substring(0, Index) + Ch + Str0.substring(Index + 1);
").

% :- pragma foreign_proc("C",
%   string.unsafe_set_char(Ch::in, Index::in, Str0::di, Str::uo),
%   [will_not_call_mercury, promise_pure, thread_safe],
% "
%   Str = Str0;
%   MR_set_char(Str, Index, Ch);
% ").
% :- pragma foreign_proc("C#",
%   string.unsafe_set_char(Ch::in, Index::in, Str0::di, Str::uo),
%   [will_not_call_mercury, promise_pure, thread_safe],
% "
%   Str = System.String.Concat(Str0.Substring(0, Index),
%       System.Convert.ToString(Ch),
%       Str0.Substring(Index + 1));
% ").
% :- pragma foreign_proc("Java",
%   string.unsafe_set_char(Ch::in, Index::in, Str0::di, Str::uo),
%   [will_not_call_mercury, promise_pure, thread_safe],
% "
%   Str = Str0.substring(0, Index) + Ch + Str0.substring(Index + 1);
% ").

/*-----------------------------------------------------------------------*/

:- pragma foreign_proc("C",
    string.length(Str::in, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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

:- pragma foreign_proc("C",
    string.length(Str::ui, Length::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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
        [will_not_call_mercury, promise_pure, thread_safe], "
    Length = Str.length();
").

:- pragma promise_equivalent_clauses(string.length/2).

string.length(Str0, Len) :-
    % XXX This copy is only necessary because of the ui.
    copy(Str0, Str),
    string.length_2(Str, 0, Len).

:- pred string.length_2(string::in, int::in, int::out) is det.

string.length_2(Str, Index, Length) :-
    ( string.index(Str, Index, _) ->
        string.length_2(Str, Index + 1, Length)
    ;
        Length = Index
    ).

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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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

string.append_iii(X, Y, Z) :-
    string.mercury_append(X, Y, Z).

:- pred string.append_ioi(string::in, string::uo, string::in) is semidet.

:- pragma foreign_proc("C",
    string.append_ioi(S1::in, S2::uo, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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
        MR_allocate_aligned_string_msg(S2, len_2, MR_PROC_LABEL);
        strcpy(S2, S3 + len_1);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").

:- pragma foreign_proc("C#",
    string.append_ioi(S1::in, S2::uo, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (S3.StartsWith(S1)) {
        S2 = S3.Remove(0, S1.Length);
        SUCCESS_INDICATOR = true;
    } else {
        SUCCESS_INDICATOR = false;
    }
}").

string.append_ioi(X, Y, Z) :-
    string.mercury_append(X, Y, Z).

:- pred string.append_iio(string::in, string::in, string::uo) is det.

:- pragma foreign_proc("C",
    string.append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    size_t len_1, len_2;
    len_1 = strlen(S1);
    len_2 = strlen(S2);
    MR_allocate_aligned_string_msg(S3, len_1 + len_2, MR_PROC_LABEL);
    strcpy(S3, S1);
    strcpy(S3 + len_1, S2);
}").

:- pragma foreign_proc("C#",
    string.append_iio(S1::in, S2::in, S3::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    S3 = System.String.Concat(S1, S2);
}").

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
            string.append_ooi_2(NextS1Len + 1, S3Len, S1, S2, S3)
        )
    ).

:- pred string.append_ooi_3(int::in, int::in, string::out,
    string::out, string::in) is det.

:- pragma foreign_proc("C",
    string.append_ooi_3(S1Len::in, S3Len::in, S1::out, S2::out, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_allocate_aligned_string_msg(S1, S1Len, MR_PROC_LABEL);
    MR_memcpy(S1, S3, S1Len);
    S1[S1Len] = '\\0';
    MR_allocate_aligned_string_msg(S2, S3Len - S1Len, MR_PROC_LABEL);
    strcpy(S2, S3 + S1Len);
}").

:- pragma foreign_proc("C#",
    string.append_ooi_3(S1Len::in, _S3Len::in, S1::out, S2::out, S3::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S1 = S3.Substring(0, S1Len);
    S2 = S3.Substring(S1Len);
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

string.substring(Str::in, Start::in, Count::in, SubStr::uo) :-
    End = min(Start + Count, string.length(Str)),
    SubStr = string.from_char_list(strchars(Start, End, Str)).

:- func strchars(int, int, string) = list(char).

strchars(I, End, Str) =
    (
        ( I < 0
        ; End =< I
        )
    ->
        []
    ;
        [string.index_det(Str, I) | strchars(I + 1, End, Str)]
    ).

:- pragma foreign_proc("C",
    string.substring(Str::in, Start::in, Count::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_Integer  len;
    MR_Word     tmp;

    if (Start < 0) Start = 0;
    if (Count <= 0) {
        MR_make_aligned_string(SubString, """");
    } else {
        len = strlen(Str);
        if (Start > len) Start = len;
        if (Count > len - Start) Count = len - Start;
        MR_allocate_aligned_string_msg(SubString, Count, MR_PROC_LABEL);
        MR_memcpy(SubString, Str + Start, Count);
        SubString[Count] = '\\0';
    }
}").

:- pragma foreign_proc("C",
    string.unsafe_substring(Str::in, Start::in, Count::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    MR_Integer len;

    MR_allocate_aligned_string_msg(SubString, Count, MR_PROC_LABEL);
    MR_memcpy(SubString, Str + Start, Count);
    SubString[Count] = '\\0';
}").
:- pragma foreign_proc("C#",
    string.unsafe_substring(Str::in, Start::in, Count::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    SubString = Str.Substring(Start, Count);
}").
:- pragma foreign_proc("Java",
    string.unsafe_substring(Str::in, Start::in, Count::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SubString = Str.substring(Start, Start + Count);
").

:- pragma foreign_proc("C",
    string.split(Str::in, Count::in, Left::uo, Right::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
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

        MR_allocate_aligned_string_msg(Left, Count, MR_PROC_LABEL);
        MR_memcpy(Left, Str, Count);
        Left[Count] = '\\0';
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(Right, len - Count, MR_PROC_LABEL);
        strcpy(Right, Str + Count);
    }
}").

:- pragma foreign_proc("C#",
    string.split(Str::in, Count::in, Left::uo, Right::uo),
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

string.split(Str, Count, Left, Right) :-
    ( Count =< 0 ->
        Left = "",
        copy(Str, Right)
    ;
        string.to_char_list(Str, List),
        Len = string.length(Str),
        ( Count > Len ->
            Num = Len
        ;
            Num = Count
        ),
        ( list.split_list(Num, List, LeftList, RightList) ->
            string.to_char_list(Left, LeftList),
            string.to_char_list(Right, RightList)
        ;
            error("string.split")
        )
    ).

/*-----------------------------------------------------------------------*/

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (
        Str[0] == First &&
        First != '\\0' &&
        strcmp(Str + 1, Rest) == 0
    );
").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int len = Str.Length;
    SUCCESS_INDICATOR = (
        len > 0 &&
        Str[0] == First &&
        System.String.Compare(Str, 1, Rest, 0, len) == 0
    );
").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    succeeded = (Str.length() == Rest.length() + 1 &&
        Str.charAt(0) == First &&
        Str.endsWith(Rest));
").

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    First = Str[0];
    SUCCESS_INDICATOR = (First != '\\0' && strcmp(Str + 1, Rest) == 0);
").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int len = Str.Length;
    if (len > 0) {
        SUCCESS_INDICATOR = (System.String.Compare(Str, 1, Rest, 0, len) == 0);
        First = Str[0];
    } else {
        SUCCESS_INDICATOR = false;
    }
").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::uo, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Str.length() == Rest.length() + 1 && Str.endsWith(Rest)) {
        succeeded = true;
        First = Str.charAt(0);
    } else {
        succeeded = false;
        // XXX to avoid uninitialized var warning
        First = (char) 0;
    }
").

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    if (Str[0] != First || First == '\\0') {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Str++;
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(Rest, strlen(Str), MR_PROC_LABEL);
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
        SUCCESS_INDICATOR = (First == Str[0]);
        Rest = Str.Substring(1);
    } else {
        SUCCESS_INDICATOR = false;
    }
}").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::in, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    int len = Str.length();

    if (len > 0) {
        succeeded = (First == Str.charAt(0));
        Rest = Str.substring(1);
    } else {
        succeeded = false;
        // XXX to avoid uninitialized var warning
        Rest = null;
    }
}").

:- pragma foreign_proc("C",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    First = Str[0];
    if (First == '\\0') {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        Str++;
        /*
        ** We need to make a copy to ensure that the pointer is word-aligned.
        */
        MR_allocate_aligned_string_msg(Rest, strlen(Str), MR_PROC_LABEL);
        strcpy(Rest, Str);
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").
:- pragma foreign_proc("C#",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (Str.Length == 0) {
        SUCCESS_INDICATOR = false;
    } else {
        First = Str[0];
        Rest = Str.Substring(1);
        SUCCESS_INDICATOR = true;
    }
}").
:- pragma foreign_proc("Java",
    string.first_char(Str::in, First::uo, Rest::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (Str.length() == 0) {
        succeeded = false;
        // XXX to avoid uninitialized var warnings:
        First = (char) 0;
        Rest = null;
    } else {
        First = Str.charAt(0);
        Rest = Str.substring(1);
        succeeded = true;
    }
}").

:- pragma foreign_proc("C",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"{
    size_t len = strlen(Rest) + 1;
    MR_allocate_aligned_string_msg(Str, len, MR_PROC_LABEL);
    Str[0] = First;
    strcpy(Str + 1, Rest);
}").
:- pragma foreign_proc("C#",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    string FirstStr;
    FirstStr = new System.String(First, 1);
    Str = System.String.Concat(FirstStr, Rest);
}").
:- pragma foreign_proc("Java",
    string.first_char(Str::uo, First::in, Rest::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    java.lang.String FirstStr = java.lang.String.valueOf(First);
    Str = FirstStr.concat(Rest);
}").

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

string.index_det(S, N) = C :-
    string.index_det(S, N, C).

string.unsafe_index(S, N) = C :-
    string.unsafe_index(S, N, C).

string.set_char_det(C, N, S0) = S :-
    string.set_char_det(C, N, S0, S).

string.unsafe_set_char(C, N, S0) = S :-
    string.unsafe_set_char(C, N, S0, S).

string.foldl(F, S, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldl(P, S, A, B).

string.foldl_substring(F, S, Start, Count, A) = B :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    string.foldl_substring(P, S, Start, Count, A, B).

string.left(S1, N) = S2 :-
    string.left(S1, N, S2).

string.right(S1, N) = S2 :-
    string.right(S1, N, S2).

string.substring(S1, N1, N2) = S2 :-
    string.substring(S1, N1, N2, S2).

string.unsafe_substring(S1, N1, N2) = S2 :-
    string.unsafe_substring(S1, N1, N2, S2).

string.hash(S) = N :-
    string.hash(S, N).

string.format(S1, PT) = S2 :-
    string.format(S1, PT, S2).

%------------------------------------------------------------------------------%

string.words_separator(SepP, String) = Words :-
    I = preceding_boundary(isnt(SepP), String, string.length(String) - 1),
    Words = words_2(SepP, String, I, []).

%------------------------------------------------------------------------------%

:- func words_2(pred(char)::in(pred(in) is semidet), string::in, int::in,
    list(string)::in) = (list(string)::out) is det.

words_2(SepP, String, WordEnd, Words0) = Words :-
    ( WordEnd < 0 ->
        Words = Words0
    ;
        WordPre = preceding_boundary(SepP, String, WordEnd),
        Word = string.unsafe_substring(String, WordPre + 1,
            WordEnd - WordPre),
        PrevWordEnd = preceding_boundary(isnt(SepP), String, WordPre),
        Words = words_2(SepP, String, PrevWordEnd, [Word | Words0])
    ).

%------------------------------------------------------------------------------%

string.words(String) = string.words_separator(char.is_whitespace, String).

%------------------------------------------------------------------------------%

    % preceding_boundary(SepP, String, I) returns the largest index J =< I
    % in String of the char that is SepP and min(-1, I) if there is no such J.
    % preceding_boundary/3 is intended for finding (in reverse) consecutive
    % maximal sequences of chars satisfying some property. Note that I
    % *must not* exceed the largest valid index for String.
    %
:- func preceding_boundary(pred(char)::in(pred(in) is semidet), string::in,
    int::in) = (int::out) is det.

preceding_boundary(SepP, String, I) =
    ( I < 0 ->
        I
    ; SepP(string.unsafe_index(String, I)) ->
        I
    ;
        preceding_boundary(SepP, String, I - 1)
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

chomp(S) =
    ( index(S, length(S) - 1, '\n') ->
        left(S, length(S) - 1)
    ;
        S
    ).

%-----------------------------------------------------------------------------%

rstrip(S) = rstrip_pred(is_whitespace, S).

%-----------------------------------------------------------------------------%

lstrip(S) = lstrip_pred(is_whitespace, S).

%-----------------------------------------------------------------------------%

strip(S0) = S :-
    L = prefix_length(is_whitespace, S0),
    R = suffix_length(is_whitespace, S0),
    S = substring(S0, L, length(S0) - L - R).

%-----------------------------------------------------------------------------%

rstrip_pred(P, S) = left(S, length(S) - suffix_length(P, S)).

%-----------------------------------------------------------------------------%

lstrip_pred(P, S) = right(S, length(S) - prefix_length(P, S)).

%-----------------------------------------------------------------------------%

prefix_length(P, S) = prefix_length_2(0, length(S), P, S).

:- func prefix_length_2(int::in, int::in, pred(char)::in(pred(in) is semidet),
    string::in) = (int::out) is det.

prefix_length_2(I, N, P, S) =
    % XXX We are using if-then-elses to get ordered conjunction.
    ( I < N ->
        ( P(S ^ unsafe_elem(I)) ->
            prefix_length_2(I + 1, N, P, S)
        ;
            I
        )
    ;
        I
    ).

%-----------------------------------------------------------------------------%

suffix_length(P, S) = suffix_length_2(length(S) - 1, length(S), P, S).

:- func suffix_length_2(int::in, int::in, pred(char)::in(pred(in) is semidet),
    string::in) = (int::out) is det.

suffix_length_2(I, N, P, S) =
    % XXX We are using if-then-elses to get ordered conjunction.
    ( 0 =< I ->
        ( P(S ^ unsafe_elem(I)) ->
            suffix_length_2(I - 1, N, P, S)
        ;
            N - (I + 1)
        )
    ;
        N - (I + 1)
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
    %
    % We need to special-case the builtin types:
    %   int, char, float, string
    %   type_info, univ, c_pointer, array
    %   and private_builtin.type_info
    %
    ( dynamic_cast(X, String) ->
        add_revstring(term_io.quoted_string(String), !Rs)
    ; dynamic_cast(X, Char) ->
        add_revstring(term_io.quoted_char(Char), !Rs)
    ; dynamic_cast(X, Int) ->
        add_revstring(string.int_to_string(Int), !Rs)
    ; dynamic_cast(X, Float) ->
        add_revstring(string.float_to_string(Float), !Rs)
    ; dynamic_cast(X, TypeDesc) ->
        type_desc_to_revstrings(TypeDesc, !Rs)
    ; dynamic_cast(X, TypeCtorDesc) ->
        type_ctor_desc_to_revstrings(TypeCtorDesc, !Rs)
    ; dynamic_cast(X, C_Pointer) ->
        add_revstring(c_pointer_to_string(C_Pointer), !Rs)
    ;
        % Check if the type is array:array/1. We can't just use dynamic_cast
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
        Functor = "{}",
        Args = [BracedTerm]
    ->
        add_revstring("{ ", !Rs),
        value_to_revstrings(NonCanon, OpsTable, univ_value(BracedTerm), !Rs),
        add_revstring(" }", !Rs)
    ;
        Functor = "{}",
        Args = [BracedHead | BracedTail]
    ->
        add_revstring("{", !Rs),
        arg_to_revstrings(NonCanon, OpsTable, BracedHead, !Rs),
        term_args_to_revstrings(NonCanon, OpsTable, BracedTail, !Rs),
        add_revstring("}", !Rs)
    ;
        Args = [PrefixArg],
        ops.lookup_prefix_op(OpsTable, Functor, OpPriority, OpAssoc)
    ->
        maybe_add_revstring("(", Priority, OpPriority, !Rs),
        add_revstring(term_io.quoted_atom(Functor), !Rs),
        add_revstring(" ", !Rs),
        adjust_priority(OpPriority, OpAssoc, NewPriority),
        value_to_revstrings_prio(NonCanon, OpsTable, NewPriority,
            univ_value(PrefixArg), !Rs),
        maybe_add_revstring(")", Priority, OpPriority, !Rs)
    ;
        Args = [PostfixArg],
        ops.lookup_postfix_op(OpsTable, Functor, OpPriority, OpAssoc)
    ->
        maybe_add_revstring("(", Priority, OpPriority, !Rs),
        adjust_priority(OpPriority, OpAssoc, NewPriority),
        value_to_revstrings_prio(NonCanon, OpsTable, NewPriority,
            univ_value(PostfixArg), !Rs),
        add_revstring(" ", !Rs),
        add_revstring(term_io.quoted_atom(Functor), !Rs),
        maybe_add_revstring(")", Priority, OpPriority, !Rs)
    ;
        Args = [Arg1, Arg2],
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
        Args = [Arg1, Arg2],
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
        )
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
    TypeDesc = rtti_implementation.unsafe_cast(PrivateBuiltinTypeInfo),
    type_desc_to_revstrings(TypeDesc, !Rs).

:- pred det_dynamic_cast(T1::in, T2::out) is det.

det_dynamic_cast(X, Y) :-
    det_univ_to_type(univ(X), Y).

%-----------------------------------------------------------------------------%

    % char_list_remove_suffix/3: We use this instead of the more general
    % list.remove_suffix so that (for example) string.format will succeed in
    % grade Java, even though unification has not yet been implemented.
    %
:- pred char_list_remove_suffix(list(char)::in, list(char)::in,
    list(char)::out) is semidet.

char_list_remove_suffix(List, Suffix, Prefix) :-
    list.length(List, ListLength),
    list.length(Suffix, SuffixLength),
    PrefixLength = ListLength - SuffixLength,
    list.split_list(PrefixLength, List, Prefix, Rest),
    char_list_equal(Suffix, Rest).

:- pred char_list_equal(list(char)::in, list(char)::in) is semidet.

char_list_equal([], []).
char_list_equal([X | Xs], [X | Ys]) :-
    char_list_equal(Xs, Ys).

%------------------------------------------------------------------------------%

string.format_table(Columns, Separator) = Table :-
    MaxWidths = list.map(find_max_length, Columns),
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

:- func join_rev_columns(string, string, string) = string.

join_rev_columns(Separator, Col1, Col2) = Col2 ++ Separator ++ Col1.

:- func find_max_length(justified_column) = int.

find_max_length(left(Strings)) = MaxLength :-
    list.foldl2(max_str_length, Strings, 0, MaxLength, "", _).
find_max_length(right(Strings)) = MaxLength :-
    list.foldl2(max_str_length, Strings, 0, MaxLength, "", _).

:- func pad_column(int, justified_column) = list(string).

pad_column(Width, left(Strings)) = list.map(string.rpad(' ', Width), Strings).
pad_column(Width, right(Strings)) = list.map(string.lpad(' ', Width), Strings).

:- func rpad(char, int, string) = string.

rpad(Chr, N, Str) = string.pad_right(Str, Chr, N).

:- func lpad(char, int, string) = string.

lpad(Chr, N, Str) = string.pad_left(Str, Chr, N).

:- pred max_str_length(string::in, int::in, int::out, string::in, string::out)
    is det.

max_str_length(Str, PrevMaxLen, MaxLen, PrevMaxStr, MaxStr) :-
    Length = string.length(Str),
    ( Length > PrevMaxLen ->
        MaxLen = Length,
        MaxStr = Str
    ;
        MaxLen = PrevMaxLen,
        MaxStr = PrevMaxStr
    ).

%-----------------------------------------------------------------------------%

word_wrap(Str, N) = word_wrap_separator(Str, N, "").

word_wrap_separator(Str, N, WordSep) = Wrapped :-
    Words = string.words_separator(char.is_whitespace, Str),
    SepLen = string.length(WordSep),
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
    WordLen = string.length(Word),
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
    ( string.length(Str) =< N ->
        Strs = [Str | Prev]
    ;
        string.split(Str, N, Left, Right),
        Strs = break_up_string_reverse(Right, N, [Left | Prev])
    ).

%-----------------------------------------------------------------------------%
