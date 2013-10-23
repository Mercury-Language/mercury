%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: c_util.m.
% Main author: fjh.
%
% This module defines utility routines that are useful when generating and/or
% emitting C code.  Some of these routines are also useful with other languages
% whose syntax is similar to C.
%
% NOTE: changes to this module may require changes to be made to java_util.m.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.c_util.
:- interface.

:- import_module backend_libs.builtin_ops.
:- import_module libs.
:- import_module libs.globals.

:- import_module char.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%
%
% Line numbering.
%

    % set_line_num(Globals, FileName, LineNum, !IO):
    %
    % If the line_numbers option is set, emit a #line directive to set the
    % specified filename and linenumber so that C compiler error messages
    % will refer to the correct location in the original source file location.
    %
:- pred set_line_num(globals::in, string::in, int::in, io::di, io::uo) is det.

    % always_set_line_num(FileName, LineNum):
    %
    % As set_line_num, but always generate a #line directive, regardless of
    % the setting of the line_numbers option.
    %
:- pred always_set_line_num(string::in, int::in, io::di, io::uo) is det.

    % If the line_numbers option is set, emit a #line directive to cancel
    % the effect of any previous #line directives, so that C compiler error
    % messages will refer to the appropriate location in the generated .c file.
    %
:- pred reset_line_num(globals::in, io::di, io::uo) is det.

    % As reset_line_num, but always generate a #line directive, regardless of
    % the setting of the line_numbers option.
    %
:- pred always_reset_line_num(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% String and character handling
%

    % Chooses between C and Java literal syntax.
    %
:- type literal_language
    --->    literal_c
    ;       literal_java
    ;       literal_csharp.

    % Print out a string suitably escaped for use as a C string literal.
    % This doesn't actually print out the enclosing double quotes --
    % that is the caller's responsibility.
    %
:- pred output_quoted_string(string::in, io::di, io::uo) is det.

    % As above, but for the specified language.
    %
:- pred output_quoted_string_lang(literal_language, string, io, io).
:- mode output_quoted_string_lang(in(bound(literal_c)), in, di, uo) is det.
:- mode output_quoted_string_lang(in(bound(literal_java)), in, di, uo) is det.
:- mode output_quoted_string_lang(in(bound(literal_csharp)), in, di, uo)
    is det.
:- mode output_quoted_string_lang(in, in, di, uo) is det.

    % output_quoted_multi_string is like list.foldl(output_quoted_string)
    % except that a null character will be written between each string
    % in the list.
    %
:- type multi_string == list(string).
:- pred output_quoted_multi_string(multi_string::in, io::di, io::uo) is det.

    % As above, but for the specified language.
    %
:- pred output_quoted_multi_string_lang(literal_language::in,
    multi_string::in, io::di, io::uo) is det.

    % Print out a char suitably escaped for use as a C char literal.
    % This doesn't actually print out the enclosing single quotes --
    % that is the caller's responsibility.
    %
:- pred output_quoted_char(char::in, io::di, io::uo) is det.

    % Convert a string to a form that is suitably escaped for use as a
    % C string literal. This doesn't actually add the enclosing double quotes
    % -- that is the caller's responsibility.
    %
:- func quote_string(string) = string.

    % Convert a character to a form that is suitably escaped for use as a
    % C character literal. This doesn't actually add the enclosing single
    % quotes -- that is the caller's responsibility.
    %
:- func quote_char(char) = string.

%-----------------------------------------------------------------------------%
%
% Float literals
%

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
% Operators
%
% The following predicates all take as input an operator, and return the name
% of the corresponding C operator that can be used to implement it.

    % The operator returned will be either a prefix operator or a macro
    % or function name. The operand needs to be placed in parentheses
    % after the operator name.
    %
:- pred unary_prefix_op(unary_op::in, string::out) is det.

:- type binop_category
    --->    array_index_binop
    ;       compound_compare_binop
    ;       string_compare_binop
    ;       unsigned_compare_binop
    ;       float_compare_binop
    ;       float_arith_binop
    ;       int_or_bool_binary_infix_binop
    ;       macro_binop
    ;       float_macro_binop.

:- pred binop_category_string(binary_op::in, binop_category::out, string::out)
    is det.

%-----------------------------------------------------------------------------%

    % output_c_file_intro_and_grade(SourceFileName, Version, Fullarch, !IO):
    %
    % Outputs a comment which includes the settings used to generate
    % the C file. This is used by configure to check the any existing C files
    % are consistent with the current configuration. SourceFileName is the
    % name of the file from which the C is generated, while Version is the
    % version name of the mercury compiler.
    %
:- pred output_c_file_intro_and_grade(globals::in, string::in, string::in,
    string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Output #pragma pack directives to change the packing alignment value
    % for MSVC. See MR_Float_Aligned in mercury_float.h.
    %
:- pred output_pragma_pack_push(io::di, io::uo) is det.

:- pred output_pragma_pack_pop(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates for working with C code
%
    % Succeeds iff the given string is a valid C identifier.
    %
:- pred is_valid_c_identifier(string::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.options.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Line numbering.
%

set_line_num(Globals, File, Line, !IO) :-
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    (
        LineNumbers = yes,
        always_set_line_num(File, Line, !IO)
    ;
        LineNumbers = no
    ).

always_set_line_num(File, Line, !IO) :-
    (
        Line > 0,
        File \= ""
    ->
        io.write_string("#line ", !IO),
        io.write_int(Line, !IO),
        io.write_string(" """, !IO),
        can_print_directly(File, CanPrint, !IO),
        (
            CanPrint = yes,
            io.write_string(File, !IO)
        ;
            CanPrint = no,
            output_quoted_string(File, !IO)
        ),
        io.write_string("""\n", !IO)
    ;
        always_reset_line_num(!IO)
    ).

reset_line_num(Globals, !IO) :-
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    (
        LineNumbers = yes,
        always_reset_line_num(!IO)
    ;
        LineNumbers = no
    ).

always_reset_line_num(!IO) :-
    % We want to generate another #line directive to reset the C compiler's
    % idea of what it is processing back to the file we are generating.
    io.get_output_line_number(Line, !IO),
    io.output_stream_name(File, !IO),
    (
        Line > 0,
        File \= ""
    ->
        io.write_string("#line ", !IO),
        io.write_int(Line + 1, !IO),
        io.write_string(" """, !IO),
        can_print_directly(File, CanPrint, !IO),
        (
            CanPrint = yes,
            io.write_string(File, !IO)
        ;
            CanPrint = no,
            output_quoted_string(File, !IO)
        ),
        io.write_string("""\n", !IO)
    ;
        true
    ).

    % Decide whether the given string can be printed directly, using
    % io.write_string, rather than output_quoted_string. The latter can take
    % more than 7% of the compiler's runtime!
    %
:- pred can_print_directly(string::in, bool::out, io::di, io::uo) is det.

can_print_directly(_, no, !IO).

:- pragma foreign_proc("C",
    can_print_directly(Str::in, CanPrintDirectly::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"{
    static  MR_String   last_string;
    static  MR_bool     last_can_print_directly;
    MR_bool             can_print_directly;
    const char          *s;
    int                 len;

    /* We cache the result of the last decision. */
    if (Str == last_string) {
        CanPrintDirectly = last_can_print_directly;
    } else {
        can_print_directly = MR_TRUE;

        for (s = Str; *s != '\\0'; s++) {
            if (! (isalnum((int)*s) || *s == '_' || *s == '/' || *s == '.')) {
                can_print_directly = MR_FALSE;
                break;
            }
        }

        len = s - Str;
        if (len >= 512) {
            can_print_directly = MR_FALSE;
        }

        CanPrintDirectly = can_print_directly;

        last_string = Str;
        last_can_print_directly = CanPrintDirectly;
    }
}").

%-----------------------------------------------------------------------------%
%
% String and character handling.
%

output_quoted_string(S, !IO) :-
    output_quoted_string_lang(literal_c, S, !IO).

:- pragma inline(output_quoted_string_lang/4).

output_quoted_string_lang(Lang, S, !IO) :-
    (
        Lang = literal_c,
        % Avoid a limitation in the MSVC compiler where string literals can be
        % no longer than 2048 chars. However if you output the string in
        % chunks, eg "part a" "part b" it will accept a string longer than 2048
        % chars, go figure!
        string.split_by_codepoint(S, 160, Left, Right),
        do_output_quoted_string(Lang, Left, 0, !IO),
        ( Right = "" ->
            true
        ;
            io.write_string("\" \"", !IO),
            output_quoted_string_lang(Lang, Right, !IO)
        )
    ;
        ( Lang = literal_java
        ; Lang = literal_csharp
        ),
        do_output_quoted_string(Lang, S, 0, !IO)
    ).

output_quoted_multi_string(Ss, !IO) :-
    output_quoted_multi_string_lang(literal_c, Ss, !IO).

output_quoted_multi_string_lang(_Lang, [], !IO).
output_quoted_multi_string_lang(Lang, [S | Ss], !IO) :-
    output_quoted_string_lang(Lang, S, !IO),
    output_quoted_char_lang(Lang, char.det_from_int(0), !IO),
    output_quoted_multi_string_lang(Lang, Ss, !IO).

:- pred do_output_quoted_string(literal_language::in, string::in,
    int::in, io::di, io::uo) is det.

do_output_quoted_string(Lang, S, Cur, !IO) :-
    ( string.unsafe_index_next(S, Cur, Next, Char) ->
        output_quoted_char_lang(Lang, Char, !IO),
        do_output_quoted_string(Lang, S, Next, !IO)
    ;
        true
    ).

output_quoted_char(Char, !IO) :-
    output_quoted_char_lang(literal_c, Char, !IO).

:- pred output_quoted_char_lang(literal_language, char, io, io).
:- mode output_quoted_char_lang(in(bound(literal_c)), in, di, uo) is det.
:- mode output_quoted_char_lang(in(bound(literal_java)), in, di, uo) is det.
:- mode output_quoted_char_lang(in(bound(literal_csharp)), in, di, uo) is det.
:- mode output_quoted_char_lang(in, in, di, uo) is det.

output_quoted_char_lang(Lang, Char, !IO) :-
    EscapedCharStr = quote_char_lang(Lang, Char),
    io.write_string(EscapedCharStr, !IO).

quote_char(Char) = quote_char_lang(literal_c, Char).

:- func quote_char_lang(literal_language, char) = string.
:- mode quote_char_lang(in(bound(literal_c)), in) = out is det.
:- mode quote_char_lang(in(bound(literal_java)), in) = out is det.
:- mode quote_char_lang(in(bound(literal_csharp)), in) = out is det.
:- mode quote_char_lang(in, in) = out is det.

quote_char_lang(Lang, Char) = QuotedCharStr :-
    quote_one_char(Lang, Char, [], RevQuotedCharStr),
    string.from_rev_char_list(RevQuotedCharStr, QuotedCharStr).

quote_string(String) = QuotedString :-
    string.foldl(quote_one_char_c, String, [], RevQuotedChars),
    string.from_rev_char_list(RevQuotedChars, QuotedString).

:- pred quote_one_char(literal_language, char, list(char), list(char)).
:- mode quote_one_char(in(bound(literal_c)), in, in, out) is det.
:- mode quote_one_char(in(bound(literal_java)), in, in, out) is det.
:- mode quote_one_char(in(bound(literal_csharp)), in, in, out) is det.
:- mode quote_one_char(in, in, in, out) is det.

quote_one_char(Lang, Char, RevChars0, RevChars) :-
    % quote_one_char_c is a specialized version of this code.
    (
        Lang = literal_java,
        java_escape_special_char(Char, RevEscapeChars)
    ->
        list.append(RevEscapeChars, RevChars0, RevChars)
    ;
        escape_special_char(Char, EscapeChar)
    ->
        RevChars = [EscapeChar, '\\' | RevChars0]
    ;
        Lang = literal_c,
        Char = '?'
    ->
        % Avoid trigraphs by escaping the question marks.
        RevChars = ['?', '\\' | RevChars0]
    ;
        is_c_source_char(Char)
    ->
        RevChars = [Char | RevChars0]
    ;
        char.to_int(Char, 0)
    ->
        RevChars = ['0', '\\' | RevChars0]
    ;
        Int = char.to_int(Char),
        Int >= 0x80
    ->
        (
            Lang = literal_c,
            ( char.to_utf8(Char, CodeUnits) ->
                list.map(octal_escape_any_int, CodeUnits, EscapeCharss),
                list.condense(EscapeCharss, EscapeChars),
                reverse_append(EscapeChars, RevChars0, RevChars)
            ;
                unexpected($module, $pred, "invalid Unicode code point")
            )
        ;
            Lang = literal_java,
            RevChars = [Char | RevChars0]
        ;
            Lang = literal_csharp,
            RevChars = [Char | RevChars0]
        )
    ;
        (
            Lang = literal_c,
            octal_escape_any_char(Char, EscapeChars)
        ;
            Lang = literal_java,
            octal_escape_any_char(Char, EscapeChars)
        ;
            Lang = literal_csharp,
            unicode_escape_any_char(Char, EscapeChars)
        ),
        reverse_append(EscapeChars, RevChars0, RevChars)
    ).

:- pred quote_one_char_c(char::in, list(char)::in, list(char)::out) is det.

quote_one_char_c(Char, RevChars0, RevChars) :-
    % This is a specialized version of quote_one_char.
    (
        escape_special_char(Char, EscapeChar)
    ->
        RevChars = [EscapeChar, '\\' | RevChars0]
    ;
        Char = '?'
    ->
        % Avoid trigraphs by escaping the question marks.
        RevChars = ['?', '\\' | RevChars0]
    ;
        is_c_source_char(Char)
    ->
        RevChars = [Char | RevChars0]
    ;
        char.to_int(Char, 0)
    ->
        RevChars = ['0', '\\' | RevChars0]
    ;
        Int = char.to_int(Char),
        Int >= 0x80
    ->
        ( char.to_utf8(Char, CodeUnits) ->
            list.map(octal_escape_any_int, CodeUnits, EscapeCharss),
            list.condense(EscapeCharss, EscapeChars),
            reverse_append(EscapeChars, RevChars0, RevChars)
        ;
            unexpected($module, $pred, "invalid Unicode code point")
        )
    ;
        octal_escape_any_char(Char, EscapeChars),
        reverse_append(EscapeChars, RevChars0, RevChars)
    ).

:- pred java_escape_special_char(char::in, list(char)::out) is semidet.

java_escape_special_char('\a', ['7', '0', '0', '\\']).
java_escape_special_char('\v', ['3', '1', '0', '\\']).

:- pred escape_special_char(char::in, char::out) is semidet.

escape_special_char('"', '"').
escape_special_char('''', '''').
escape_special_char('\\', '\\').
escape_special_char('\n', 'n').
escape_special_char('\t', 't').
escape_special_char('\b', 'b').
escape_special_char('\a', 'a'). % not in Java
escape_special_char('\v', 'v'). % not in Java
escape_special_char('\r', 'r').
escape_special_char('\f', 'f').

    % This succeeds iff the specified character is allowed as an (unescaped)
    % character in standard-conforming C source code.
    %
:- pred is_c_source_char(char::in) is semidet.

is_c_source_char(Char) :-
    ( char.is_alnum(Char)
    ; string.contains_char(c_graphic_chars, Char)
    ).

    % This returns a string containing all the characters that the C standard
    % specifies as being included in the "basic execution character set",
    % except for the letters (a-z A-Z) and digits (0-9).
    %
:- func c_graphic_chars = string.

c_graphic_chars = " !\"#%&'()*+,-./:;<=>?[\\]^_{|}~".

    % reverse_append(Xs, Ys, Zs) <=> Zs = list.reverse(Xs) ++ Ys.
    %
:- pred reverse_append(list(T)::in, list(T)::in, list(T)::out) is det.

reverse_append([], L, L).
reverse_append([X | Xs], L0, L) :-
    reverse_append(Xs, [X | L0], L).

:- pred octal_escape_any_char(char::in, list(char)::out) is det.

    % Convert a character to the corresponding C octal escape code.
    % XXX This assumes that the target language compiler's representation
    % of characters is the same as the Mercury compiler's.
    %
octal_escape_any_char(Char, EscapeCodeChars) :-
    char.to_int(Char, Int),
    octal_escape_any_int(Int, EscapeCodeChars).

:- pred octal_escape_any_int(int::in, list(char)::out) is det.

octal_escape_any_int(Int, EscapeCodeChars) :-
    string.int_to_base_string(Int, 8, OctalString0),
    string.pad_left(OctalString0, '0', 3, OctalString),
    EscapeCodeChars = ['\\' | string.to_char_list(OctalString)].

:- pred unicode_escape_any_char(char::in, list(char)::out) is det.

unicode_escape_any_char(Char, EscapeCodeChars) :-
    char.to_int(Char, Int),
    string.format("\\u%04x", [i(Int)], HexString),
    string.to_char_list(HexString, EscapeCodeChars).

%-----------------------------------------------------------------------------%
%
% Floating point literals
%
% XXX These routines do not yet handle infinities and NaNs properly.

make_float_literal(Float) = string.format("%#.17g", [f(Float)]).
    % This is used by the C, Java, and IL back-ends,
    % so the output must be valid syntax in all three languages.
    %
    % We output literals using 17 digits of precision. This is the minimum
    % needed to be able to convert IEEE double-precision floating point values
    % to strings and back again without losing precision.

output_float_literal(Float, !IO) :-
    io.write_string(make_float_literal(Float), !IO).

%-----------------------------------------------------------------------------%
%
% Operators
%

unary_prefix_op(mktag,              "MR_mktag").
unary_prefix_op(tag,                "MR_tag").
unary_prefix_op(unmktag,            "MR_unmktag").
unary_prefix_op(strip_tag,          "MR_strip_tag").
unary_prefix_op(mkbody,             "MR_mkbody").
unary_prefix_op(unmkbody,           "MR_unmkbody").
unary_prefix_op(bitwise_complement, "~").
unary_prefix_op(logical_not,        "!").
unary_prefix_op(hash_string,        "MR_hash_string").
unary_prefix_op(hash_string2,       "MR_hash_string2").
unary_prefix_op(hash_string3,       "MR_hash_string3").

% The operator strings for array_index, compound_lt and compound_eq are
% dummies; they should never be used.

binop_category_string(array_index(_), array_index_binop, "ARRAY_INDEX").

binop_category_string(compound_lt, compound_compare_binop, "COMPOUND_LT").
binop_category_string(compound_eq, compound_compare_binop, "COMPOUND_EQ").

binop_category_string(str_eq, string_compare_binop, "==").
binop_category_string(str_ne, string_compare_binop, "!=").
binop_category_string(str_le, string_compare_binop, "<=").
binop_category_string(str_ge, string_compare_binop, ">=").
binop_category_string(str_lt, string_compare_binop, "<").
binop_category_string(str_gt, string_compare_binop, ">").

binop_category_string(unsigned_le, unsigned_compare_binop, "<=").

binop_category_string(float_plus, float_arith_binop, "+").
binop_category_string(float_minus, float_arith_binop, "-").
binop_category_string(float_times, float_arith_binop, "*").
binop_category_string(float_divide, float_arith_binop, "/").

binop_category_string(float_eq, float_compare_binop, "==").
binop_category_string(float_ne, float_compare_binop, "!=").
binop_category_string(float_le, float_compare_binop, "<=").
binop_category_string(float_ge, float_compare_binop, ">=").
binop_category_string(float_lt, float_compare_binop, "<").
binop_category_string(float_gt, float_compare_binop, ">").

binop_category_string(int_add, int_or_bool_binary_infix_binop, "+").
binop_category_string(int_sub, int_or_bool_binary_infix_binop, "-").
binop_category_string(int_mul, int_or_bool_binary_infix_binop, "*").
binop_category_string(int_div, int_or_bool_binary_infix_binop, "/").
binop_category_string(unchecked_left_shift,  int_or_bool_binary_infix_binop,
    "<<").
binop_category_string(unchecked_right_shift, int_or_bool_binary_infix_binop,
    ">>").
binop_category_string(bitwise_and, int_or_bool_binary_infix_binop, "&").
binop_category_string(bitwise_or, int_or_bool_binary_infix_binop, "|").
binop_category_string(bitwise_xor, int_or_bool_binary_infix_binop, "^").
binop_category_string(int_mod, int_or_bool_binary_infix_binop, "%").
binop_category_string(eq, int_or_bool_binary_infix_binop, "==").
binop_category_string(ne, int_or_bool_binary_infix_binop, "!=").
binop_category_string(logical_and, int_or_bool_binary_infix_binop, "&&").
binop_category_string(logical_or, int_or_bool_binary_infix_binop, "||").
binop_category_string(int_lt, int_or_bool_binary_infix_binop, "<").
binop_category_string(int_gt, int_or_bool_binary_infix_binop, ">").
binop_category_string(int_le, int_or_bool_binary_infix_binop, "<=").
binop_category_string(int_ge, int_or_bool_binary_infix_binop, ">=").

binop_category_string(str_cmp, macro_binop, "MR_strcmp").
binop_category_string(body, macro_binop, "MR_body").

binop_category_string(float_word_bits, float_macro_binop,
    "MR_float_word_bits").
binop_category_string(float_from_dword, float_macro_binop,
    "MR_float_from_dword").

%-----------------------------------------------------------------------------%

output_c_file_intro_and_grade(Globals, SourceFileName, Version, Fullarch,
        !IO) :-
    globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
    string.int_to_string(NumTagBits, NumTagBitsStr),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
    UnboxedFloatStr = convert_bool_to_string(UnboxedFloat),
    globals.lookup_bool_option(Globals, pregenerated_dist, PregeneratedDist),
    PregeneratedDistStr = convert_bool_to_string(PregeneratedDist),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    HighLevelCodeStr = convert_bool_to_string(HighLevelCode),

    io.write_strings([
        "/*\n",
        "** Automatically generated from `", SourceFileName, "'\n",
        "** by the Mercury compiler,\n",
        "** version ", Version, "\n",
        "** configured for ", Fullarch, ".\n",
        "** Do not edit.\n",
        "**\n",
        "** The autoconfigured grade settings governing\n",
        "** the generation of this C file were\n",
        "**\n",
        "** TAG_BITS=", NumTagBitsStr, "\n",
        "** UNBOXED_FLOAT=", UnboxedFloatStr, "\n",
        "** PREGENERATED_DIST=", PregeneratedDistStr, "\n",
        "** HIGHLEVEL_CODE=", HighLevelCodeStr, "\n",
        "**\n",
        "** END_OF_C_GRADE_INFO\n",
        "*/\n",
        "\n"
    ], !IO).

:- func convert_bool_to_string(bool) = string.

convert_bool_to_string(no) = "no".
convert_bool_to_string(yes) = "yes".

%-----------------------------------------------------------------------------%

    % We could hide these blocks behind macros using the __pragma keyword
    % introduced in MSVC 9 (2008):
    %
    %   #define MR_PRAGMA_PACK_PUSH  __pragma(pack(push, MR_BYTES_PER_WORD))
    %   #define MR_PRAGMA_PACK_POP   __pragma(pack(pop))
    %
output_pragma_pack_push(!IO) :-
    io.write_string("\n#ifdef MR_MSVC\n", !IO),
    io.write_string("#pragma pack(push, MR_BYTES_PER_WORD)\n", !IO),
    io.write_string("#endif\n", !IO).

output_pragma_pack_pop(!IO) :-
    io.write_string("#ifdef MR_MSVC\n", !IO),
    io.write_string("#pragma pack(pop)\n", !IO),
    io.write_string("#endif\n", !IO).

%-----------------------------------------------------------------------------%

is_valid_c_identifier(S) :-
    string.index(S, 0, Start),
    char.is_alpha_or_underscore(Start),
    string.is_all_alnum_or_underscore(S).

%-----------------------------------------------------------------------------%
