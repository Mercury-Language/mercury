%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: c_util.m
% Main author: fjh.
%
% This module defines utility routines that are useful when generating and/or
% emitting C code.  Some of these routines are also useful with other languages
% whose syntax is similar to C.
%
% NOTE: changes to this module may require changes to be made to java_util.m.
%
%-----------------------------------------------------------------------------%

:- module backend_libs__c_util.
:- interface.

:- import_module aditi_backend.
:- import_module aditi_backend.rl_file.
:- import_module backend_libs.builtin_ops.
:- import_module mdbcomp.prim_data.

:- import_module char.
:- import_module int.
:- import_module io.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Line numbering.

    % set_line_num(FileName, LineNum):
    %
    % Emit a #line directive to set the specified filename and linenumber
    % so that C compiler error messages etc. will refer to the correct location
    % in the original source file location.
    %
:- pred set_line_num(string::in, int::in, io::di, io::uo) is det.

    % Emit a #line directive to cancel the effect of any previous #line
    % directives, so that C compiler error messages etc. will refer to the
    % appropriate location in the generated .c file.
    %
:- pred reset_line_num(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% String and character handling.

    % Print out a string suitably escaped for use as a C string literal.
    % This doesn't actually print out the enclosing double quotes --
    % that is the caller's responsibility.
    %
:- pred output_quoted_string(string::in, io::di, io::uo) is det.

    % Output_quoted_multi_string is like output_quoted_string except that
    % the string may contain embedded NUL characters (i.e. '\0').
    % The int specifies the length of the string.
    %
:- type multi_string == string.
:- pred output_quoted_multi_string(int::in, multi_string::in,
    io::di, io::uo) is det.

    % Print out a char suitably escaped for use as a C char literal.
    % This doesn't actually print out the enclosing single quotes --
    % that is the caller's responsibility.
    %
:- pred output_quoted_char(char::in, io::di, io::uo) is det.

    % Convert a string to a form that is suitably escaped for use as a
    % C string literal. This doesn't actually add the enclosing double quotes
    % -- that is the caller's responsibility.
    %
:- pred quote_string(string::in, string::out) is det.

    % Convert a character to a form that is suitably escaped for use as a
    % C character literal. This doesn't actually add the enclosing single
    % quotes -- that is the caller's responsibility.
    %
:- pred quote_char(char::in, string::out) is det.

%-----------------------------------------------------------------------------%
%
% Float literals.

    % Convert a float to a string suitable for use as a C (or Java, or IL)
    % floating point literal.
    %
:- func make_float_literal(float) = string.

    % As above, but write the string to the current output stream
    % rather than returning it.
    %
:- pred output_float_literal(float::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Operators.
%
% The following predicates all take as input an operator, check if it is
% an operator of the specified kind, and if so, return the name of the
% corresponding C operator that can be used to implement it.

    % The operator returned will be <, >, etc.;
    % it can be used in the form `strcmp(<Arg1>, <Arg2>) <Op> 0'.
    %
:- pred string_compare_op(binary_op::in, string::out) is semidet.

    % The operator returned will be +, *, etc.;
    % the arguments should be floats and the result will be a float.
    %
:- pred float_op(binary_op::in, string::out) is semidet.

    % The operator returned will be <, >, etc.;
    % the arguments should be floats and the result will be a boolean.
    %
:- pred float_compare_op(binary_op::in, string::out) is semidet.

    % The operator returned will be an infix operator. The arguments should be
    % cast to MR_Unsigned, and the result will be a boolean.
    %
:- pred unsigned_compare_op(binary_op::in, string::out) is semidet.

    % The operator returned will be either a prefix operator or a macro
    % or function name. The operand needs to be placed in parentheses
    % after the operator name.
    %
:- pred unary_prefix_op(unary_op::in, string::out) is det.

    % The operator returned will be an infix operator. The arguments should be
    % integer or booleans and the result will be an integer or a boolean.
    %
:- pred binary_infix_op(binary_op::in, string::out) is semidet.

%-----------------------------------------------------------------------------%

    % Currently the `.rlo' files are stored as static data in the executable.
    % It may be better to store them in separate files in a known location
    % and load them at runtime.
    %
:- pred output_rl_file(module_name::in, maybe(rl_file)::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

    % output_c_file_intro_and_grade(SourceFileName, Version):
    %
    % Outputs a comment which includes the settings used to generate
    % the C file. This is used by configure to check the any existing C files
    % are consistent with the current configuration. SourceFileName is the
    % name of the file from which the C is generated, while Version is the
    % version name of the mercury compiler.
    %
:- pred output_c_file_intro_and_grade(string::in, string::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.name_mangle.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%
%
% Line numbering.

set_line_num(File, Line, !IO) :-
    globals__io_lookup_bool_option(line_numbers, LineNumbers, !IO),
    (
        LineNumbers = yes,
        (
            Line > 0,
            File \= ""
        ->
            io__write_string("#line ", !IO),
            io__write_int(Line, !IO),
            io__write_string(" """, !IO),
            output_quoted_string(File, !IO),
            io__write_string("""\n", !IO)
        ;
            reset_line_num(!IO)
        )
    ;
        LineNumbers = no
    ).

reset_line_num(!IO) :-
    % We want to generate another #line directive to reset the C compiler's
    % idea of what it is processing back to the file we are generating.
    io__get_output_line_number(Line, !IO),
    io__output_stream_name(FileName, !IO),
    globals__io_lookup_bool_option(line_numbers, LineNumbers, !IO),
    (
        Line > 0,
        FileName \= "",
        LineNumbers = yes
    ->
        io__write_string("#line ", !IO),
        io__write_int(Line + 1, !IO),
        io__write_string(" """, !IO),
        output_quoted_string(FileName, !IO),
        io__write_string("""\n", !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%
% String and character handling.

output_quoted_string(S0, !IO) :-
    output_quoted_multi_string(string__length(S0), S0, !IO).

output_quoted_multi_string(Len, S, !IO) :-
    output_quoted_multi_string_2(0, Len, S, !IO).

:- pred output_quoted_multi_string_2(int::in, int::in, string::in,
    io::di, io::uo) is det.

output_quoted_multi_string_2(Cur, Len, S, !IO) :-
    ( Cur < Len ->
        % Avoid a limitation in the MSVC compiler where string literals
        % can be no longer then 2048 chars. However if you output the string
        % in chunks, eg "part a" "part b" it will accept a string longer than
        % 2048 chars, go figure!
        (
            Cur \= 0,
            Cur mod 512 = 0
        ->
            io__write_string("\" \"", !IO)
        ;
            true
        ),

        % We must use unsafe index, because we want to be able to access chars
        % beyond the first NUL.
        string__unsafe_index(S, Cur, Char),
        output_quoted_char(Char, !IO),
        
        % Check for trigraph sequences in string literals. We break the
        % trigraph by breaking the string into multiple chunks. For example,
        % "??-" gets converted to "?" "?-".
        (
            Char = '?',
            Cur < Len + 2
        ->
            (
                string__unsafe_index(S, Cur + 1, '?'),
                string__unsafe_index(S, Cur + 2, ThirdChar),
                is_trigraph_char(ThirdChar)
            ->
                io__write_string("\" \"", !IO)
            ;
                true
            )
        ;
            true
        ),

        output_quoted_multi_string_2(Cur + 1, Len, S, !IO)
    ;
        true
    ).

output_quoted_char(Char, !IO) :-
    quote_char(Char, EscapedChars),
    io__write_string(EscapedChars, !IO).

quote_char(Char, QuotedChar) :-
    quote_one_char(Char, [], RevQuotedChar),
    string__from_rev_char_list(RevQuotedChar, QuotedChar).

quote_string(String, QuotedString) :-
    string__foldl(quote_one_char, String, [], RevQuotedChars),
    string__from_rev_char_list(RevQuotedChars, QuotedString).

:- pred quote_one_char(char::in, list(char)::in, list(char)::out) is det.

quote_one_char(Char, RevChars0, RevChars) :-
    ( escape_special_char(Char, EscapeChar) ->
        RevChars = [EscapeChar, '\\' | RevChars0]
    ; is_c_source_char(Char) ->
        RevChars = [Char | RevChars0]
    ; char__to_int(Char, 0) ->
        RevChars = ['0', '\\' | RevChars0]
    ;
        escape_any_char(Char, EscapeChars),
        reverse_append(EscapeChars, RevChars0, RevChars)
    ).

:- pred escape_special_char(char::in, char::out) is semidet.

escape_special_char('"', '"').
escape_special_char('''', '''').
escape_special_char('\\', '\\').
escape_special_char('\n', 'n').
escape_special_char('\t', 't').
escape_special_char('\b', 'b').
escape_special_char('\a', 'a').
escape_special_char('\v', 'v').
escape_special_char('\r', 'r').
escape_special_char('\f', 'f').

    % Succeed if the given character, prefixed with "??", is a trigraph.
    %
:- pred is_trigraph_char(char::in) is semidet.

is_trigraph_char('(').
is_trigraph_char(')').
is_trigraph_char('<').
is_trigraph_char('>').
is_trigraph_char('=').
is_trigraph_char('/').
is_trigraph_char('\'').
is_trigraph_char('!').
is_trigraph_char('-').

    % This succeeds iff the specified character is allowed as an (unescaped)
    % character in standard-conforming C source code.
    %
:- pred is_c_source_char(char::in) is semidet.

is_c_source_char(Char) :-
    ( char__is_alnum(Char)
    ; string__contains_char(c_graphic_chars, Char)
    ).

    % This returns a string containing all the characters that the C standard
    % specifies as being included in the "basic execution character set",
    % except for the letters (a-z A-Z) and digits (0-9).
    %
:- func c_graphic_chars = string.

c_graphic_chars = " !\"#%&'()*+,-./:;<=>?[\\]^_{|}~".

    % reverse_append(Xs, Ys, Zs) <=> Zs = list__reverse(Xs) ++ Ys.
    %
:- pred reverse_append(list(T)::in, list(T)::in, list(T)::out) is det.

reverse_append([], L, L).
reverse_append([X | Xs], L0, L) :-
    reverse_append(Xs, [X | L0], L).

:- pred escape_any_char(char::in, list(char)::out) is det.

    % Convert a character to the corresponding C octal escape code.
    % XXX This assumes that the target language compiler's representation
    % of characters is the same as the Mercury compiler's.
    %
escape_any_char(Char, EscapeCodeChars) :-
    char__to_int(Char, Int),
    string__int_to_base_string(Int, 8, OctalString0),
    string__pad_left(OctalString0, '0', 3, OctalString),
    EscapeCodeChars = ['\\' | string__to_char_list(OctalString)].

%-----------------------------------------------------------------------------%
%
% Floating point literals.
%
% XXX These routines do not yet handle infinities and NaNs properly.

make_float_literal(Float) = string__format("%#.17g", [f(Float)]).
    % This is used by the C, Java, and IL back-ends,
    % so the output must be valid syntax in all three languages.
    %
    % We output literals using 17 digits of precision. This is the minimum
    % needed to be able to convert IEEE double-precision floating point values
    % to strings and back again without losing precision.

output_float_literal(Float, !IO) :-
    io__write_string(make_float_literal(Float), !IO).

%-----------------------------------------------------------------------------%
%
% Operators.

unary_prefix_op(mktag,              "MR_mktag").
unary_prefix_op(tag,                "MR_tag").
unary_prefix_op(unmktag,            "MR_unmktag").
unary_prefix_op(mkbody,             "MR_mkbody").
unary_prefix_op(unmkbody,           "MR_unmkbody").
unary_prefix_op(strip_tag,          "MR_strip_tag").
unary_prefix_op(hash_string,        "MR_hash_string").
unary_prefix_op(bitwise_complement, "~").
unary_prefix_op(logical_not,        "!").

string_compare_op(str_eq, "==").
string_compare_op(str_ne, "!=").
string_compare_op(str_le, "<=").
string_compare_op(str_ge, ">=").
string_compare_op(str_lt, "<").
string_compare_op(str_gt, ">").

unsigned_compare_op(unsigned_le, "<=").

float_op(float_plus, "+").
float_op(float_minus, "-").
float_op(float_times, "*").
float_op(float_divide, "/").

float_compare_op(float_eq, "==").
float_compare_op(float_ne, "!=").
float_compare_op(float_le, "<=").
float_compare_op(float_ge, ">=").
float_compare_op(float_lt, "<").
float_compare_op(float_gt, ">").

binary_infix_op(int_add, "+").
binary_infix_op(int_sub, "-").
binary_infix_op(int_mul, "*").
binary_infix_op(int_div, "/").
binary_infix_op(unchecked_left_shift,  "<<").
binary_infix_op(unchecked_right_shift, ">>").
binary_infix_op(bitwise_and, "&").
binary_infix_op(bitwise_or, "|").
binary_infix_op(bitwise_xor, "^").
binary_infix_op(int_mod, "%").
binary_infix_op(eq, "==").
binary_infix_op(ne, "!=").
binary_infix_op(logical_and, "&&").
binary_infix_op(logical_or, "||").
binary_infix_op(int_lt, "<").
binary_infix_op(int_gt, ">").
binary_infix_op(int_le, "<=").
binary_infix_op(int_ge, ">=").

%-----------------------------------------------------------------------------%

output_rl_file(ModuleName, MaybeRLFile, !IO) :-
    globals__io_lookup_bool_option(aditi, Aditi, !IO),
    (
        Aditi = no
    ;
        Aditi = yes,
        io__write_string("\n\n/* Aditi-RL code for this module. */\n", !IO),
        RLDataConstName = make_rl_data_name(ModuleName),
        io__write_string("const char ", !IO),
        io__write_string(RLDataConstName, !IO),
        io__write_string("[] = {", !IO),
        (
            MaybeRLFile = yes(RLFile),
            rl_file__write_binary(output_rl_byte, RLFile, Length, !IO),
            io__write_string("0};\n", !IO)
        ;
            MaybeRLFile = no,
            io__write_string("};\n", !IO),
            Length = 0
        ),

        % Store the length of the data in
        % mercury__aditi_rl_data__<module>__length.

        string__append(RLDataConstName, "__length", RLDataConstLength),
        io__write_string("const int ", !IO),
        io__write_string(RLDataConstLength, !IO),
        io__write_string(" = ", !IO),
        io__write_int(Length, !IO),
        io__write_string(";\n\n", !IO)
    ).

:- pred output_rl_byte(int::in, io::di, io::uo) is det.

output_rl_byte(Byte, !IO) :-
    io__write_int(Byte, !IO),
    io__write_string(", ", !IO).

%-----------------------------------------------------------------------------%

output_c_file_intro_and_grade(SourceFileName, Version, !IO) :-
    globals__io_lookup_int_option(num_tag_bits, NumTagBits, !IO),
    string__int_to_string(NumTagBits, NumTagBitsStr),
    globals__io_lookup_bool_option(unboxed_float, UnboxedFloat, !IO),
    UnboxedFloatStr = convert_bool_to_string(UnboxedFloat),

    io__write_strings([
        "/*\n",
        "** Automatically generated from `", SourceFileName, "'\n",
        "** by the Mercury compiler,\n",
        "** version ", Version, ".\n",
        "** Do not edit.\n",
        "**\n",
        "** The autoconfigured grade settings governing\n",
        "** the generation of this C file were\n",
        "**\n",
        "** TAG_BITS=", NumTagBitsStr, "\n",
        "** UNBOXED_FLOAT=", UnboxedFloatStr, "\n",
        "**\n",
        "** END_OF_C_GRADE_INFO\n",
        "*/\n",
        "\n"
    ], !IO).

:- func convert_bool_to_string(bool) = string.

convert_bool_to_string(no) = "no".
convert_bool_to_string(yes) = "yes".

%-----------------------------------------------------------------------------%
