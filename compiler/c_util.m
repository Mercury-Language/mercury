%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: c_util.m.
% Main author: fjh.
%
% This module defines utility routines that are useful when generating and/or
% emitting C code.  Some of these routines are also useful with other languages
% whose syntax is similar to C.
%
%---------------------------------------------------------------------------%

:- module backend_libs.c_util.
:- interface.

:- import_module backend_libs.builtin_ops.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%
% Line numbering.
%

:- func lookup_line_numbers(globals, option) = maybe_set_line_numbers.

:- type maybe_set_line_numbers
    --->    dont_set_line_numbers
    ;       set_line_numbers.

    % maybe_set_line_num(Stream, MaybeSetLineNumbers, FileName, LineNum, !IO):
    %
    % If MaybeSetLineNumbers = set_line_numbers, emit a #line directive
    % to Stream to set the specified filename and linenumber, so that
    % any error messages from the C compiler will refer to the correct location
    % in the original source file location.
    %
:- pred maybe_set_line_num(io.text_output_stream::in,
    maybe_set_line_numbers::in, string::in, int::in, io::di, io::uo) is det.
:- pred maybe_set_line_num_cur_stream(maybe_set_line_numbers::in,
    string::in, int::in, io::di, io::uo) is det.

    % always_set_line_num(Stream, FileName, LineNum, !IO):
    %
    % As maybe_set_line_num, but always generate a #line directive.
    %
:- pred always_set_line_num(io.text_output_stream::in, string::in, int::in,
    io::di, io::uo) is det.
:- pred always_set_line_num_cur_stream(string::in, int::in,
    io::di, io::uo) is det.

    % maybe_reset_line_num(Stream, MaybeSetLineNumbers, MaybeFileName, !IO):
    %
    % If MaybeSetLineNumbers = set_line_numbers, emit a #line directive
    % to Stream to cancel the effect of any previous #line directives,
    % so that C compiler error messages will refer to the appropriate location
    % in the generated .c file.
    %
    % If MaybeFileName = no, then use the actual name of the file that Stream
    % is writing to as the name of the file we are getting back to.
    % If MaybeFileName = yes(FileName), then use FileName for this purpose,
    % regardless of whether it is the name of the file that Streams writes to.
    % When we write to modname.suffix.tmp with the intention of later
    % moving it to modname.suffix, we want the #line directive to refer to
    % modname.suffix, not modname.suffix.tmp. This can be done by passing
    % yes("modname.suffix") as MaybeFileName.
    %
:- pred maybe_reset_line_num(io.text_output_stream::in,
    maybe_set_line_numbers::in, maybe(string)::in, io::di, io::uo) is det.
:- pred maybe_reset_line_num_cur_stream(maybe_set_line_numbers::in,
    maybe(string)::in, io::di, io::uo) is det.

    % As maybe_reset_line_num, but always generate a #line directive.
    %
:- pred always_reset_line_num(io.text_output_stream::in,
    maybe(string)::in, io::di, io::uo) is det.
:- pred always_reset_line_num_cur_stream(maybe(string)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% String and character handling.
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
:- pred output_quoted_string(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.
:- pred output_quoted_string_cur_stream(string::in,
    io::di, io::uo) is det.

    % As above, but for the specified language.
    %
:- pred output_quoted_string_lang(io.text_output_stream, literal_language,
    string, io, io).
:- mode output_quoted_string_lang(in, in(bound(literal_c)), in, di, uo) is det.
:- mode output_quoted_string_lang(in, in(bound(literal_java)), in, di, uo)
    is det.
:- mode output_quoted_string_lang(in, in(bound(literal_csharp)), in, di, uo)
    is det.
:- mode output_quoted_string_lang(in, in, in, di, uo) is det.

:- pred output_quoted_string_lang_cur_stream(literal_language, string, io, io).
:- mode output_quoted_string_lang_cur_stream(in(bound(literal_c)), in, di, uo)
    is det.
:- mode output_quoted_string_lang_cur_stream(in(bound(literal_java)), in,
    di, uo) is det.
:- mode output_quoted_string_lang_cur_stream(in(bound(literal_csharp)), in,
    di, uo) is det.
:- mode output_quoted_string_lang_cur_stream(in, in, di, uo) is det.

    % output_quoted_multi_string is like list.foldl(output_quoted_string)
    % except that a null character will be written between each string
    % in the list.
    %
:- type multi_string == list(string).
:- pred output_quoted_multi_string(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.
:- pred output_quoted_multi_string_cur_stream(multi_string::in,
    io::di, io::uo) is det.

    % As above, but for the specified language.
    %
:- pred output_quoted_multi_string_lang(io.text_output_stream::in,
    literal_language::in, multi_string::in, io::di, io::uo) is det.
:- pred output_quoted_multi_string_lang_cur_stream(literal_language::in,
    multi_string::in, io::di, io::uo) is det.

    % Print out a char suitably escaped for use as a C char literal.
    % This doesn't actually print out the enclosing single quotes --
    % that is the caller's responsibility.
    %
:- pred output_quoted_char(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.
:- pred output_quoted_char_cur_stream(char::in,
    io::di, io::uo) is det.

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

%---------------------------------------------------------------------------%
%
% Integer literals.
%

    % Write out an int as a C expression.
    %
:- pred output_int_expr(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.
:- pred output_int_expr_cur_stream(int::in, io::di, io::uo) is det.

    % Write out a uint as a C expression.
    %
:- pred output_uint_expr(io.text_output_stream::in, uint::in,
    io::di, io::uo) is det.
:- pred output_uint_expr_cur_stream(uint::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Float literals.
%

    % Convert a float to a string suitable for use as a C (or Java, or C#)
    % floating point literal.
    %
:- func make_float_literal(float) = string.

    % As above, but write the string to the specified output stream
    % rather than returning it.
    %
:- pred output_float_literal(io.text_output_stream::in, float::in,
    io::di, io::uo) is det.
:- pred output_float_literal_cur_stream(float::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Operators.
%
% The following predicates all take as input an operator, and return the name
% of the corresponding C operator that can be used to implement it.

    % The operator returned will be either a prefix operator or a macro
    % or function name. The operand needs to be placed in parentheses
    % after the operator name.
    %
:- pred unary_prefix_op(unary_op::in, string::out) is det.

    % XXX is this type really necessary?  binop_category_string/3's only caller
    % only cares about if the operation is a 'float_arith_binop' or not.
    %
:- type binop_category
    --->    array_index_binop
    ;       string_index_binop
    ;       pointer_compare_binop
    ;       compound_compare_binop
    ;       offset_string_compare_binop(int)
    ;       general_string_compare_binop
    ;       string_compare_binop
    ;       unsigned_compare_binop
    ;       float_compare_binop
    ;       float_arith_binop
    ;       int_or_bool_binary_infix_binop
    ;       int_macro_binop
    ;       float_macro_binop.

:- pred binop_category_string(binary_op::in, binop_category::out, string::out)
    is det.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % Output #pragma pack directives to change the packing alignment value
    % for MSVC. See MR_Float_Aligned in mercury_float.h.
    %
:- pred output_pragma_pack_push(io::di, io::uo) is det.

:- pred output_pragma_pack_pop(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Utility predicates for working with C code.
%
    % Succeeds iff the given string is a valid C identifier.
    %
:- pred is_valid_c_identifier(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module integer.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%
% Line numbering.
%

lookup_line_numbers(Globals, Option) = MaybeSetLineNumbers :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = no,
        MaybeSetLineNumbers = dont_set_line_numbers
    ;
        OptionValue = yes,
        MaybeSetLineNumbers = set_line_numbers
    ).

%---------------------%

maybe_set_line_num(Stream, MaybeSetLineNumbers, File, Line, !IO) :-
    (
        MaybeSetLineNumbers = set_line_numbers,
        always_set_line_num(Stream, File, Line, !IO)
    ;
        MaybeSetLineNumbers = dont_set_line_numbers
    ).

maybe_set_line_num_cur_stream(MaybeSetLineNumbers, File, Line, !IO) :-
    io.output_stream(Stream, !IO),
    maybe_set_line_num(Stream, MaybeSetLineNumbers, File, Line, !IO).

%---------------------%

always_set_line_num(Stream, File, Line, !IO) :-
    ( if
        Line > 0,
        File \= ""
    then
        io.write_string(Stream, "#line ", !IO),
        io.write_int(Stream, Line, !IO),
        io.write_string(Stream, " """, !IO),
        can_print_directly(File, CanPrint, !IO),
        (
            CanPrint = yes,
            io.write_string(Stream, File, !IO)
        ;
            CanPrint = no,
            output_quoted_string(Stream, File, !IO)
        ),
        io.write_string(Stream, """\n", !IO)
    else
        % XXX What is the point of this call?
        always_reset_line_num(Stream, no, !IO)
    ).

always_set_line_num_cur_stream(File, Line, !IO) :-
    io.output_stream(Stream, !IO),
    always_set_line_num(Stream, File, Line, !IO).

%---------------------%

maybe_reset_line_num(Stream, MaybeSetLineNumbers, MaybeFileName, !IO) :-
    (
        MaybeSetLineNumbers = set_line_numbers,
        always_reset_line_num(Stream, MaybeFileName, !IO)
    ;
        MaybeSetLineNumbers = dont_set_line_numbers
    ).

maybe_reset_line_num_cur_stream(MaybeSetLineNumbers, MaybeFileName, !IO) :-
    io.output_stream(Stream, !IO),
    maybe_reset_line_num(Stream, MaybeSetLineNumbers, MaybeFileName, !IO).

%---------------------%

always_reset_line_num(Stream, MaybeFileName, !IO) :-
    % We want to generate another #line directive to reset the C compiler's
    % idea of what it is processing back to the file we are generating.
    io.get_output_line_number(Line, !IO),
    (
        MaybeFileName = yes(FileName)
    ;
        MaybeFileName = no,
        io.output_stream_name(Stream, FileName, !IO)
    ),
    ( if
        Line > 0,
        FileName \= ""
    then
        io.write_string(Stream, "#line ", !IO),
        io.write_int(Stream, Line + 1, !IO),
        io.write_string(Stream, " """, !IO),
        can_print_directly(FileName, CanPrint, !IO),
        (
            CanPrint = yes,
            io.write_string(Stream, FileName, !IO)
        ;
            CanPrint = no,
            output_quoted_string(Stream, FileName, !IO)
        ),
        io.write_string(Stream, """\n", !IO)
    else
        true
    ).

always_reset_line_num_cur_stream(MaybeFileName, !IO) :-
    io.output_stream(Stream, !IO),
    always_reset_line_num(Stream, MaybeFileName, !IO).

%---------------------%

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

%---------------------------------------------------------------------------%
%
% String and character handling.
%

output_quoted_string(Stream, Str, !IO) :-
    output_quoted_string_lang(Stream, literal_c, Str, !IO).

output_quoted_string_cur_stream(Str, !IO) :-
    io.output_stream(Stream, !IO),
    output_quoted_string(Stream, Str, !IO).

%---------------------%

:- pragma inline(output_quoted_string_lang/5).

output_quoted_string_lang(Stream, Lang, Str, !IO) :-
    (
        Lang = literal_c,
        % Avoid a limitation in the MSVC compiler, which requires
        % string literals to be no longer than 2048 chars. However,
        % it will accept a string longer than 2048 chars if we output
        % the string in chunks, as in e.g. "part a" "part b". Go figure!
        string.split_by_codepoint(Str, 160, Left, Right),
        do_output_quoted_string_lang(Stream, Lang, Left, 0, !IO),
        ( if Right = "" then
            true
        else
            io.write_string("\" \"", !IO),
            output_quoted_string_lang(Stream, Lang, Right, !IO)
        )
    ;
        ( Lang = literal_java
        ; Lang = literal_csharp
        ),
        do_output_quoted_string_lang(Stream, Lang, Str, 0, !IO)
    ).

:- pred do_output_quoted_string_lang(io.text_output_stream::in,
    literal_language::in, string::in, int::in, io::di, io::uo) is det.

do_output_quoted_string_lang(Stream, Lang, Str, Cur, !IO) :-
    ( if string.unsafe_index_next(Str, Cur, Next, Char) then
        output_quoted_char_lang(Stream, Lang, Char, !IO),
        do_output_quoted_string_lang(Stream, Lang, Str, Next, !IO)
    else
        true
    ).

output_quoted_string_lang_cur_stream(Lang, S, !IO) :-
    io.output_stream(Stream, !IO),
    output_quoted_string_lang(Stream, Lang, S, !IO).

%---------------------%

output_quoted_multi_string(Stream, Strs, !IO) :-
    output_quoted_multi_string_lang(Stream, literal_c, Strs, !IO).

output_quoted_multi_string_cur_stream(Str, !IO) :-
    io.output_stream(Stream, !IO),
    output_quoted_multi_string(Stream, Str, !IO).

%---------------------%

output_quoted_multi_string_lang(_Stream, _Lang, [], !IO).
output_quoted_multi_string_lang(Stream, Lang, [Str | Strs], !IO) :-
    output_quoted_string_lang(Stream, Lang, Str, !IO),
    output_quoted_char_lang(Stream, Lang, char.det_from_int(0), !IO),
    output_quoted_multi_string_lang(Stream, Lang, Strs, !IO).

output_quoted_multi_string_lang_cur_stream(Lang, S, !IO) :-
    io.output_stream(Stream, !IO),
    output_quoted_multi_string_lang(Stream, Lang, S, !IO).

%---------------------%

output_quoted_char(Stream, Char, !IO) :-
    output_quoted_char_lang(Stream, literal_c, Char, !IO).

:- pred output_quoted_char_lang(io.text_output_stream, literal_language, char,
    io, io).
:- mode output_quoted_char_lang(in, in(bound(literal_c)), in, di, uo) is det.
:- mode output_quoted_char_lang(in, in(bound(literal_java)), in, di, uo)
    is det.
:- mode output_quoted_char_lang(in, in(bound(literal_csharp)), in, di, uo)
    is det.
:- mode output_quoted_char_lang(in, in, in, di, uo) is det.

output_quoted_char_lang(Stream, Lang, Char, !IO) :-
    EscapedCharStr = quote_char_lang(Lang, Char),
    io.write_string(Stream, EscapedCharStr, !IO).

output_quoted_char_cur_stream(Char, !IO) :-
    io.output_stream(Stream, !IO),
    output_quoted_char(Stream, Char, !IO).

%---------------------%

quote_string(String) = QuotedString :-
    string.foldl(quote_one_char_c, String, [], RevQuotedChars),
    string.from_rev_char_list(RevQuotedChars, QuotedString).

%---------------------%

quote_char(Char) = quote_char_lang(literal_c, Char).

:- func quote_char_lang(literal_language, char) = string.
:- mode quote_char_lang(in(bound(literal_c)), in) = out is det.
:- mode quote_char_lang(in(bound(literal_java)), in) = out is det.
:- mode quote_char_lang(in(bound(literal_csharp)), in) = out is det.
:- mode quote_char_lang(in, in) = out is det.

quote_char_lang(Lang, Char) = QuotedCharStr :-
    quote_one_char(Lang, Char, [], RevQuotedCharStr),
    string.from_rev_char_list(RevQuotedCharStr, QuotedCharStr).

:- pred quote_one_char(literal_language, char, list(char), list(char)).
:- mode quote_one_char(in(bound(literal_c)), in, in, out) is det.
:- mode quote_one_char(in(bound(literal_java)), in, in, out) is det.
:- mode quote_one_char(in(bound(literal_csharp)), in, in, out) is det.
:- mode quote_one_char(in, in, in, out) is det.

quote_one_char(Lang, Char, RevChars0, RevChars) :-
    % quote_one_char_c is a specialized version of this code.
    ( if
        Lang = literal_java,
        java_escape_special_char(Char, RevEscapeChars)
    then
        list.append(RevEscapeChars, RevChars0, RevChars)
    else if
        escape_special_char(Char, EscapeChar)
    then
        RevChars = [EscapeChar, '\\' | RevChars0]
    else if
        Lang = literal_c,
        Char = '?'
    then
        % Avoid trigraphs by escaping the question marks.
        RevChars = ['?', '\\' | RevChars0]
    else if
        is_c_source_char(Char)
    then
        RevChars = [Char | RevChars0]
    else if
        char.to_int(Char, 0)
    then
        RevChars = ['0', '\\' | RevChars0]
    else if
        Int = char.to_int(Char),
        Int >= 0x80
    then
        (
            Lang = literal_c,
            ( if char.to_utf8(Char, CodeUnits) then
                list.map(octal_escape_any_int, CodeUnits, EscapeCharss),
                list.condense(EscapeCharss, EscapeChars),
                reverse_prepend(EscapeChars, RevChars0, RevChars)
            else
                unexpected($module, $pred, "invalid Unicode code point")
            )
        ;
            Lang = literal_java,
            RevChars = [Char | RevChars0]
        ;
            Lang = literal_csharp,
            RevChars = [Char | RevChars0]
        )
    else
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
        reverse_prepend(EscapeChars, RevChars0, RevChars)
    ).

:- pred quote_one_char_c(char::in, list(char)::in, list(char)::out) is det.

quote_one_char_c(Char, RevChars0, RevChars) :-
    % This is a specialized version of quote_one_char.
    ( if
        escape_special_char(Char, EscapeChar)
    then
        RevChars = [EscapeChar, '\\' | RevChars0]
    else if
        Char = '?'
    then
        % Avoid trigraphs by escaping the question marks.
        RevChars = ['?', '\\' | RevChars0]
    else if
        is_c_source_char(Char)
    then
        RevChars = [Char | RevChars0]
    else if
        char.to_int(Char, 0)
    then
        RevChars = ['0', '\\' | RevChars0]
    else if
        Int = char.to_int(Char),
        Int >= 0x80
    then
        ( if char.to_utf8(Char, CodeUnits) then
            list.map(octal_escape_any_int, CodeUnits, EscapeCharss),
            list.condense(EscapeCharss, EscapeChars),
            reverse_prepend(EscapeChars, RevChars0, RevChars)
        else
            unexpected($module, $pred, "invalid Unicode code point")
        )
    else
        octal_escape_any_char(Char, EscapeChars),
        reverse_prepend(EscapeChars, RevChars0, RevChars)
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

    % Convert a character to the corresponding C octal escape code.
    % XXX This assumes that the target language compiler's representation
    % of characters is the same as the Mercury compiler's.
    %
:- pred octal_escape_any_char(char::in, list(char)::out) is det.

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

%---------------------------------------------------------------------------%
%
% Integer literals.
%

output_int_expr(Stream, N, !IO) :-
    % We need to cast to (MR_Integer) to ensure things like 1 << 32 work
    % when `MR_Integer' is 64 bits but `int' is 32 bits.
    %
    % C does not have negative integer constants so "(MR_Integer) -nnnn"
    % is the negation of a positive integer constant nnnn, converted to
    % MR_Integer.
    %
    % In C89/C90 an unsuffixed decimal integer constant must be typed
    % `int' or `long int' or `unsigned long int', whichever fits first.
    % The negated result will have the same type. If nnnn > LONG_MAX then
    % it will be typed `unsigned long int'. If MR_Integer is wider than
    % `unsigned long int' then the conversion to MR_Integer yields a positive
    % value, not negative.
    %
    % C99 has different integer constant type rules. The unsuffixed decimal
    % integer constant must be typed `int' or `long int' or `long long int'
    % but not `unsigned long int'. Therefore the same problem does not occur.

    ( if N >= -2147483647 then
        % Write integers in the most readable way as long as the absolute value
        % does not exceed LONG_MAX, otherwise it may be typed `unsigned long'.
        % This is the minimum magnitude of LONG_MAX in C.
        io.write_string(Stream, "(MR_Integer) ", !IO),
        io.write_int(Stream, N, !IO)
    else
        ( if integer.to_string(integer(N)) = "-2147483648" then
            % Write -2^31 without using an integer constant that overflows
            % a 32-bit signed `long' in two's complement representation.
            io.write_string(Stream, "(-(MR_Integer) 2147483647 - 1)", !IO)
        else if integer.to_string(integer(N)) = "-9223372036854775808" then
            % Write -2^63 without using an integer constant that overflows
            % a 64-bit signed `long' in two's complement representation.
            io.write_string(Stream, "(-(MR_Integer) 9223372036854775807 - 1)",
                !IO)
        else if int.min_int(N) then
            % Avoid negating min_int as it would overflow in two's complement
            % representation. In practice, one of the preceding two cases
            % would have been taken.
            io.write_string(Stream, "(-(MR_Integer) ", !IO),
            io.write_int(Stream, -(N + 1), !IO),
            io.write_string(Stream, "- 1)", !IO)
        else
            % Write other negative values as negation of an MR_Integer value.
            io.write_string(Stream, "(-(MR_Integer) ", !IO),
            io.write_int(Stream, -N, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ).

output_int_expr_cur_stream(N, !IO) :-
    io.output_stream(Stream, !IO),
    output_int_expr(Stream, N, !IO).

%---------------------------------------------------------------------------%
%
% Unsigned integer literals.
%

output_uint_expr(Stream, N, !IO) :-
    io.write_uint(Stream, N, !IO),
    io.write_string(Stream, "U", !IO).

output_uint_expr_cur_stream(N, !IO) :-
    io.output_stream(Stream, !IO),
    output_uint_expr(Stream, N, !IO).

%---------------------------------------------------------------------------%
%
% Floating point literals.
%
% XXX These routines do not yet handle infinities and NaNs properly.

make_float_literal(Float) = string.format("%#.17g", [f(Float)]).
    % This is used by the C, Java, and C# back-ends,
    % so the output must be valid syntax in all three languages.
    %
    % We output literals using 17 digits of precision. This is the minimum
    % needed to be able to convert IEEE double-precision floating point values
    % to strings and back again without losing precision.

output_float_literal(Stream, Float, !IO) :-
    io.write_string(Stream, make_float_literal(Float), !IO).

output_float_literal_cur_stream(N, !IO) :-
    io.output_stream(Stream, !IO),
    output_float_literal(Stream, N, !IO).

%---------------------------------------------------------------------------%
%
% Operators.
%

unary_prefix_op(mktag,              "MR_mktag").
unary_prefix_op(tag,                "MR_tag").
unary_prefix_op(unmktag,            "MR_unmktag").
unary_prefix_op(strip_tag,          "MR_strip_tag").
unary_prefix_op(mkbody,             "MR_mkbody").
unary_prefix_op(unmkbody,           "MR_unmkbody").
unary_prefix_op(bitwise_complement(_), "~").
unary_prefix_op(logical_not,        "!").
unary_prefix_op(hash_string,        "MR_hash_string").
unary_prefix_op(hash_string2,       "MR_hash_string2").
unary_prefix_op(hash_string3,       "MR_hash_string3").
unary_prefix_op(hash_string4,       "MR_hash_string4").
unary_prefix_op(hash_string5,       "MR_hash_string5").
unary_prefix_op(hash_string6,       "MR_hash_string6").

% The operator strings for array_index, compound_lt and compound_eq are
% dummies; they should never be used.

binop_category_string(array_index(_), array_index_binop, "ARRAY_INDEX").
binop_category_string(string_unsafe_index_code_unit, string_index_binop,
    "STRING_UNSAFE_INDEX_CODE_UNIT").

binop_category_string(pointer_equal_conservative, pointer_compare_binop, "==").

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

binop_category_string(int_add(_), int_or_bool_binary_infix_binop, "+").
binop_category_string(int_sub(_), int_or_bool_binary_infix_binop, "-").
binop_category_string(int_mul(_), int_or_bool_binary_infix_binop, "*").
binop_category_string(int_div(_), int_or_bool_binary_infix_binop, "/").
binop_category_string(unchecked_left_shift(_),  int_or_bool_binary_infix_binop,
    "<<").
binop_category_string(unchecked_right_shift(_), int_or_bool_binary_infix_binop,
    ">>").
binop_category_string(bitwise_and(_), int_or_bool_binary_infix_binop, "&").
binop_category_string(bitwise_or(_), int_or_bool_binary_infix_binop, "|").
binop_category_string(bitwise_xor(_), int_or_bool_binary_infix_binop, "^").
binop_category_string(int_mod(_), int_or_bool_binary_infix_binop, "%").
binop_category_string(eq(_), int_or_bool_binary_infix_binop, "==").
binop_category_string(ne(_), int_or_bool_binary_infix_binop, "!=").
binop_category_string(logical_and, int_or_bool_binary_infix_binop, "&&").
binop_category_string(logical_or, int_or_bool_binary_infix_binop, "||").
binop_category_string(int_lt(_), int_or_bool_binary_infix_binop, "<").
binop_category_string(int_gt(_), int_or_bool_binary_infix_binop, ">").
binop_category_string(int_le(_), int_or_bool_binary_infix_binop, "<=").
binop_category_string(int_ge(_), int_or_bool_binary_infix_binop, ">=").

binop_category_string(str_cmp, general_string_compare_binop, "MR_strcmp").
binop_category_string(offset_str_eq(N), offset_string_compare_binop(N),
    "MR_offset_streq").
binop_category_string(body, int_macro_binop, "MR_body").

binop_category_string(float_word_bits, float_macro_binop,
    "MR_float_word_bits").
binop_category_string(float_from_dword, float_macro_binop,
    "MR_float_from_dword").

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

is_valid_c_identifier(S) :-
    string.index(S, 0, Start),
    char.is_alpha_or_underscore(Start),
    string.is_all_alnum_or_underscore(S).

%---------------------------------------------------------------------------%
:- end_module backend_libs.c_util.
%---------------------------------------------------------------------------%
