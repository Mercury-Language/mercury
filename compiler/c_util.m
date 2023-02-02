%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: c_util.m.
% Main author: fjh.
%
% This module defines utility routines that are useful when generating and/or
% emitting C code. Some of these routines are also useful with other languages
% whose syntax is similar to C.
%
%---------------------------------------------------------------------------%

:- module backend_libs.c_util.
:- interface.

:- import_module backend_libs.builtin_ops.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

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

    % always_set_line_num(Stream, FileName, LineNum, !IO):
    %
    % As maybe_set_line_num, but always generate a #line directive.
    %
:- pred always_set_line_num(io.text_output_stream::in, string::in, int::in,
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

    % As maybe_reset_line_num, but always generate a #line directive.
    %
:- pred always_reset_line_num(io.text_output_stream::in,
    maybe(string)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% String and character handling.
%

    % Convert a string to a form that is suitably escaped for use as a
    % C string literal, and add double quotes around it.
    %
:- func quote_string_c(string) = string.

    % Convert a string to a form that is suitably escaped for use as a
    % C string literal. This doesn't actually add the enclosing double quotes;
    % that is the caller's responsibility.
    %
:- func prepare_to_quote_string_c(string) = string.

    % Print out a string suitably escaped for use as a string literal in
    % C/Java/C#C/Java/C#, without double quotes around it.
    %
:- pred output_quoted_string_c(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.
:- pred output_quoted_string_java(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.
:- pred output_quoted_string_csharp(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

    % output_quoted_multi_string_LANG does the same job as
    % output_quoted_string_LANG on the concatenation of the elements
    % of the multistring, adding a null character after each string
    % in the list. It will print a single set of double quotes
    % around the whole lot.
    %
:- type multi_string == list(string).
:- pred output_quoted_multi_string_c(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.
:- pred output_quoted_multi_string_java(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.
:- pred output_quoted_multi_string_csharp(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.

    % Convert a character to a form that is suitably escaped for use as a
    % C character literal, and add single quotes around it.
    %
:- func quote_char_c(char) = string.

    % Convert a character to a form that is suitably escaped for use as a
    % C character literal. This doesn't actually add the enclosing single
    % quotes; that is the caller's responsibility.
    %
:- func prepare_to_quote_char_c(char) = string.

    % Print out a char suitably escaped for use as a char literal in C/Java/C#,
    % with single quotes around it.
    %
:- pred output_quoted_char_c(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.
:- pred output_quoted_char_java(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.
:- pred output_quoted_char_csharp(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.

    % Print out a char suitably escaped for use as a char literal in C/Java/C#.
    % This doesn't actually print out the enclosing single quotes;
    % that is the caller's responsibility.
    %
:- pred output_to_be_quoted_char_c(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.
:- pred output_to_be_quoted_char_java(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.
:- pred output_to_be_quoted_char_csharp(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Integer literals.
%

    % Write out an int as a C expression.
    %
:- pred output_int_expr(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Write out a uint as a C expression.
    %
:- pred output_uint_expr(io.text_output_stream::in, uint::in,
    io::di, io::uo) is det.

    % Write out an int8 as a C expression.
    %
:- pred output_int8_expr(io.text_output_stream::in, int8::in,
    io::di, io::uo) is det.

    % Write out a uint8 as a C expression.
    %
:- pred output_uint8_expr(io.text_output_stream::in, uint8::in,
    io::di, io::uo) is det.

    % Write out an int16 as a C expression.
    %
:- pred output_int16_expr(io.text_output_stream::in, int16::in,
    io::di, io::uo) is det.

    % Write out a uint16 as a C expression.
    %
:- pred output_uint16_expr(io.text_output_stream::in, uint16::in,
    io::di, io::uo) is det.

    % Write out an int32 as a C expression.
    %
:- pred output_int32_expr(io.text_output_stream::in, int32::in,
    io::di, io::uo) is det.

    % Write out a uint32 as a C expression.
    %
:- pred output_uint32_expr(io.text_output_stream::in, uint32::in,
    io::di, io::uo) is det.

    % Convert a uint64 to a string suitable for use as a C uint64_t literal.
    % Note that the result is not suitable for use with C# or Java.
    %
:- func make_int64_literal(int64) = string.

    % Write out an int64 as a C expression.
    %
:- pred output_int64_expr(io.text_output_stream::in, int64::in,
    io::di, io::uo) is det.

    % Convert a uint64 to a string suitable for use as a C uint64_t literal.
    % Note that the result is not suitable for use with C# or Java.
    %
:- func make_uint64_literal(uint64) = string.

    % Write out a uint64 as a C expression.
    %
:- pred output_uint64_expr(io.text_output_stream::in, uint64::in,
    io::di, io::uo) is det.

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

%---------------------------------------------------------------------------%
%
% Operators.
%
% The following predicates all take as input an operator, and return the name
% of the corresponding C operator that can be used to implement it.
%

    % The operator returned will be either a prefix operator or a macro
    % or function name. The operand needs to be placed in parentheses
    % after the operator name.
    %
:- pred unary_prefix_op(unary_op::in, string::out) is det.

%---------------------------------------------------------------------------%

    % output_c_file_intro_and_grade(SourceFileName, Stream, Version,
    %   Fullarch, !IO):
    %
    % Outputs a comment which includes the settings used to generate
    % the C file. This is used by configure to check the any existing C files
    % are consistent with the current configuration. SourceFileName is the
    % name of the file from which the C is generated, while Version is the
    % version name of the mercury compiler.
    %
:- pred output_c_file_intro_and_grade(globals::in, io.text_output_stream::in,
    string::in, string::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % output_init_c_comment(Stream, ModuleName,
    %   UserInitPredCNames, UserFinalPredCNames, EnvVarNames, !IO):
    %
    % Output a comment to tell mkinit what initialization functions
    % to call for this module from <main_module>_init.c.
    %
    % The main initialization function for the module records the
    % correspondence between the addresses and the layout structures
    % of labels in a table, for use mainly by the profilers and the debugger,
    % but also to generate meaningful stack traces at exceptions in other
    % grades as well.
    %
    % We generate other initialization functions to implement user defined
    % initializers and finalizers, and to record the values of environment
    % variables for the conditions of trace goals.
    %
:- pred output_init_c_comment(io.text_output_stream::in, module_name::in,
    list(string)::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output #pragma pack directives to change the packing alignment value
    % for MSVC. See MR_Float_Aligned in mercury_float.h.

:- pred output_pragma_pack_push(io.text_output_stream::in,
    io::di, io::uo) is det.

:- pred output_pragma_pack_pop(io.text_output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Utility predicates for working with C code.
%
    % Succeeds iff the given string is a valid C identifier.
    %
:- pred is_valid_c_identifier(string::in) is semidet.

%---------------------------------------------------------------------------%
%
% Utility predicate to shorten overlong identifiers.
%

    % Return hexadecimal encoded hash of a string.
    % The resulting string has a length of 8 characters and will be
    % consistent across different compiler backends and word sizes.
    %
:- func hex_hash32(string) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module int.
:- import_module int32.
:- import_module int64.
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

%---------------------%

always_set_line_num(Stream, FileName, LineNumber, !IO) :-
    ( if
        LineNumber > 0,
        FileName \= ""
    then
        can_print_without_quoting(FileName, CanPrint, !IO),
        (
            CanPrint = yes,
            io.format(Stream, "#line %d ""%s""\n",
                [i(LineNumber), s(FileName)], !IO)
        ;
            CanPrint = no,
            io.format(Stream, "#line %d %s\n",
                [i(LineNumber), s(quote_string_c(FileName))], !IO)
        )
    else
        % XXX What is the point of this call?
        always_reset_line_num(Stream, no, !IO)
    ).

%---------------------%

maybe_reset_line_num(Stream, MaybeSetLineNumbers, MaybeFileName, !IO) :-
    (
        MaybeSetLineNumbers = set_line_numbers,
        always_reset_line_num(Stream, MaybeFileName, !IO)
    ;
        MaybeSetLineNumbers = dont_set_line_numbers
    ).

%---------------------%

always_reset_line_num(Stream, MaybeFileName, !IO) :-
    % We want to generate another #line directive to reset the C compiler's
    % idea of what it is processing back to the file we are generating.
    io.get_output_line_number(Stream, LineNumber, !IO),
    (
        MaybeFileName = yes(FileName)
    ;
        MaybeFileName = no,
        io.output_stream_name(Stream, FileName, !IO)
    ),
    ( if
        LineNumber > 0,
        FileName \= ""
    then
        can_print_without_quoting(FileName, CanPrint, !IO),
        (
            CanPrint = yes,
            io.format(Stream, "#line %d ""%s""\n",
                [i(LineNumber + 1), s(FileName)], !IO)
        ;
            CanPrint = no,
            io.format(Stream, "#line %d %s\n",
                [i(LineNumber + 1), s(quote_string_c(FileName))], !IO)
        )
    else
        true
    ).

%---------------------%

    % Decide whether the given string can be printed directly, using
    % io.write_string, rather than output_quoted_string_LANG. The latter
    % can take more than 7% of the compiler's runtime!
    %
:- pred can_print_without_quoting(string::in, bool::out,
    io::di, io::uo) is det.

can_print_without_quoting(_, no, !IO).

:- pragma foreign_proc("C",
    can_print_without_quoting(Str::in, CanPrintWithoutQuoting::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"{
    static  MR_String   last_string;
    static  MR_bool     last_can_print_without_quoting;
    MR_bool             can_print_without_quoting;
    const char          *s;
    int                 len;

    /* We cache the result of the last decision. */
    if (Str == last_string) {
        CanPrintWithoutQuoting = last_can_print_without_quoting;
    } else {
        can_print_without_quoting = MR_TRUE;

        for (s = Str; *s != '\\0'; s++) {
            if (! (isalnum((int) *s) || *s == '_' || *s == '/' || *s == '.')) {
                can_print_without_quoting = MR_FALSE;
                break;
            }
        }

        len = s - Str;
        if (len >= 512) {
            can_print_without_quoting = MR_FALSE;
        }

        CanPrintWithoutQuoting = can_print_without_quoting;

        last_string = Str;
        last_can_print_without_quoting = CanPrintWithoutQuoting;
    }
}").

%---------------------------------------------------------------------------%
%
% String and character handling.
%

quote_string_c(String) = QuotedString :-
    string.foldl(quote_one_char_acc_c, String, ['"'], RevQuotedChars0),
    RevQuotedChars = ['"' | RevQuotedChars0],
    string.from_rev_char_list(RevQuotedChars, QuotedString).

prepare_to_quote_string_c(String) = QuotedString :-
    string.foldl(quote_one_char_acc_c, String, [], RevQuotedChars),
    string.from_rev_char_list(RevQuotedChars, QuotedString).

%---------------------%

output_quoted_string_c(Stream, Str, !IO) :-
    % Avoid a limitation in the MSVC compiler, which requires
    % string literals to be no longer than 2048 chars. However,
    % it will accept a string longer than 2048 chars if we output
    % the string in chunks, as in e.g. "part a" "part b". Go figure!
    % XXX How does "limit is 2048" translate to "get me 160 codepoints"?
    string.split_by_code_point(Str, 160, LeftSubStr, RightSubStr),
    io.write_char(Stream, '"', !IO),
    output_to_be_quoted_string_loop_c(Stream, LeftSubStr, 0, !IO),
    io.write_char(Stream, '"', !IO),
    ( if RightSubStr = "" then
        true
    else
        io.write_string(Stream, " ", !IO),
        output_quoted_string_c(Stream, RightSubStr, !IO)
    ).

output_quoted_string_java(Stream, Str, !IO) :-
    io.write_char(Stream, '"', !IO),
    output_to_be_quoted_string_loop_java(Stream, Str, 0, !IO),
    io.write_char(Stream, '"', !IO).

output_quoted_string_csharp(Stream, Str, !IO) :-
    io.write_char(Stream, '"', !IO),
    output_to_be_quoted_string_loop_csharp(Stream, Str, 0, !IO),
    io.write_char(Stream, '"', !IO).

%---------------------%

:- pred output_to_be_quoted_string_loop_c(io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

output_to_be_quoted_string_loop_c(Stream, Str, Cur, !IO) :-
    ( if string.unsafe_index_next(Str, Cur, Next, Char) then
        output_to_be_quoted_char_c(Stream, Char, !IO),
        output_to_be_quoted_string_loop_c(Stream, Str, Next, !IO)
    else
        true
    ).

:- pred output_to_be_quoted_string_loop_java(io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

output_to_be_quoted_string_loop_java(Stream, Str, Cur, !IO) :-
    ( if string.unsafe_index_next(Str, Cur, Next, Char) then
        output_to_be_quoted_char_java(Stream, Char, !IO),
        output_to_be_quoted_string_loop_java(Stream, Str, Next, !IO)
    else
        true
    ).

:- pred output_to_be_quoted_string_loop_csharp(io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

output_to_be_quoted_string_loop_csharp(Stream, Str, Cur, !IO) :-
    ( if string.unsafe_index_next(Str, Cur, Next, Char) then
        output_to_be_quoted_char_csharp(Stream, Char, !IO),
        output_to_be_quoted_string_loop_csharp(Stream, Str, Next, !IO)
    else
        true
    ).

%---------------------%

output_quoted_multi_string_c(Stream, Strs, !IO) :-
    io.write_char(Stream, '"', !IO),
    output_to_be_quoted_multi_string_c(Stream, Strs, !IO),
    io.write_char(Stream, '"', !IO).

output_quoted_multi_string_java(Stream, Strs, !IO) :-
    io.write_char(Stream, '"', !IO),
    output_to_be_quoted_multi_string_java(Stream, Strs, !IO),
    io.write_char(Stream, '"', !IO).

output_quoted_multi_string_csharp(Stream, Strs, !IO) :-
    io.write_char(Stream, '"', !IO),
    output_to_be_quoted_multi_string_csharp(Stream, Strs, !IO),
    io.write_char(Stream, '"', !IO).

%---------------------%

:- pred output_to_be_quoted_multi_string_c(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.

output_to_be_quoted_multi_string_c(_Stream, [], !IO).
output_to_be_quoted_multi_string_c(Stream, [Str | Strs], !IO) :-
    output_to_be_quoted_string_loop_c(Stream, Str, 0, !IO),
    output_to_be_quoted_char_c(Stream, char.det_from_int(0), !IO),
    output_to_be_quoted_multi_string_c(Stream, Strs, !IO).

:- pred output_to_be_quoted_multi_string_java(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.

output_to_be_quoted_multi_string_java(_Stream, [], !IO).
output_to_be_quoted_multi_string_java(Stream, [Str | Strs], !IO) :-
    output_to_be_quoted_string_loop_java(Stream, Str, 0, !IO),
    output_to_be_quoted_char_java(Stream, char.det_from_int(0), !IO),
    output_to_be_quoted_multi_string_java(Stream, Strs, !IO).

:- pred output_to_be_quoted_multi_string_csharp(io.text_output_stream::in,
    multi_string::in, io::di, io::uo) is det.

output_to_be_quoted_multi_string_csharp(_Stream, [], !IO).
output_to_be_quoted_multi_string_csharp(Stream, [Str | Strs], !IO) :-
    output_to_be_quoted_string_loop_csharp(Stream, Str, 0, !IO),
    output_to_be_quoted_char_csharp(Stream, char.det_from_int(0), !IO),
    output_to_be_quoted_multi_string_csharp(Stream, Strs, !IO).

%---------------------%

quote_char_c(Char) = QuotedCharStr :-
    quote_one_char_acc_c(Char, [''''], RevQuotedCharStr0),
    RevQuotedCharStr = ['''' | RevQuotedCharStr0],
    string.from_rev_char_list(RevQuotedCharStr, QuotedCharStr).

%---------------------%

prepare_to_quote_char_c(Char) = QuotedCharStr :-
    quote_one_char_acc_c(Char, [], RevQuotedCharStr),
    string.from_rev_char_list(RevQuotedCharStr, QuotedCharStr).

:- func prepare_to_quote_char_java(char) = string.

prepare_to_quote_char_java(Char) = QuotedCharStr :-
    quote_one_char_acc_java(Char, [], RevQuotedCharStr),
    string.from_rev_char_list(RevQuotedCharStr, QuotedCharStr).

:- func prepare_to_quote_char_csharp(char) = string.

prepare_to_quote_char_csharp(Char) = QuotedCharStr :-
    quote_one_char_acc_csharp(Char, [], RevQuotedCharStr),
    string.from_rev_char_list(RevQuotedCharStr, QuotedCharStr).

%---------------------%

output_quoted_char_c(Stream, Char, !IO) :-
    EscapedCharStr = prepare_to_quote_char_c(Char),
    io.write_char(Stream, '''', !IO),
    io.write_string(Stream, EscapedCharStr, !IO),
    io.write_char(Stream, '''', !IO).

output_quoted_char_java(Stream, Char, !IO) :-
    EscapedCharStr = prepare_to_quote_char_java(Char),
    io.write_char(Stream, '''', !IO),
    io.write_string(Stream, EscapedCharStr, !IO),
    io.write_char(Stream, '''', !IO).

output_quoted_char_csharp(Stream, Char, !IO) :-
    EscapedCharStr = prepare_to_quote_char_csharp(Char),
    io.write_char(Stream, '''', !IO),
    io.write_string(Stream, EscapedCharStr, !IO),
    io.write_char(Stream, '''', !IO).

%---------------------%

output_to_be_quoted_char_c(Stream, Char, !IO) :-
    EscapedCharStr = prepare_to_quote_char_c(Char),
    io.write_string(Stream, EscapedCharStr, !IO).

output_to_be_quoted_char_java(Stream, Char, !IO) :-
    EscapedCharStr = prepare_to_quote_char_java(Char),
    io.write_string(Stream, EscapedCharStr, !IO).

output_to_be_quoted_char_csharp(Stream, Char, !IO) :-
    EscapedCharStr = prepare_to_quote_char_csharp(Char),
    io.write_string(Stream, EscapedCharStr, !IO).

%---------------------%

:- pred quote_one_char_acc_c(char::in, list(char)::in, list(char)::out) is det.

quote_one_char_acc_c(Char, RevChars0, RevChars) :-
    ( if escape_special_char_c(Char, RevChars0, RevChars1) then
        RevChars = RevChars1
    else if is_source_char_c_java_csharp(Char) then
        RevChars = [Char | RevChars0]
    else
        char.to_int(Char, CharInt),
        ( if CharInt = 0 then
            RevChars = ['0', '\\' | RevChars0]
        else if CharInt >= 0x80 then
            ( if char.to_utf8(Char, CodeUnits) then
                list.map(octal_escape_any_int, CodeUnits, EscapeCharss),
                list.condense(EscapeCharss, EscapeChars),
                reverse_prepend(EscapeChars, RevChars0, RevChars)
            else
                unexpected($pred, "invalid Unicode code point")
            )
        else
            octal_escape_any_char(Char, EscapeChars),
            reverse_prepend(EscapeChars, RevChars0, RevChars)
        )
    ).

:- pred quote_one_char_acc_java(char::in, list(char)::in, list(char)::out)
    is det.

quote_one_char_acc_java(Char, RevChars0, RevChars) :-
    ( if escape_special_char_java(Char, RevChars0, RevChars1) then
        RevChars = RevChars1
    else if is_source_char_c_java_csharp(Char) then
        RevChars = [Char | RevChars0]
    else
        char.to_int(Char, CharInt),
        ( if CharInt = 0 then
            RevChars = ['0', '\\' | RevChars0]
        else if CharInt >= 0x80 then
            RevChars = [Char | RevChars0]
        else
            octal_escape_any_char(Char, EscapeChars),
            reverse_prepend(EscapeChars, RevChars0, RevChars)
        )
    ).

:- pred quote_one_char_acc_csharp(char::in, list(char)::in, list(char)::out)
    is det.

quote_one_char_acc_csharp(Char, RevChars0, RevChars) :-
    ( if escape_special_char_csharp(Char, RevChars0, RevChars1) then
        RevChars = RevChars1
    else if is_source_char_c_java_csharp(Char) then
        RevChars = [Char | RevChars0]
    else
        char.to_int(Char, CharInt),
        ( if CharInt = 0 then
            RevChars = ['0', '\\' | RevChars0]
        else if CharInt >= 0x80 then
            RevChars = [Char | RevChars0]
        else
            unicode_escape_any_char(CharInt, EscapeChars),
            reverse_prepend(EscapeChars, RevChars0, RevChars)
        )
    ).

%---------------------%

:- pred escape_special_char_c(char::in, list(char)::in, list(char)::out)
    is semidet.
:- pragma inline(pred(escape_special_char_c/3)).

escape_special_char_c(Char, RevChars0, RevChars) :-
    ( Char = '"',    RevChars = ['"', '\\'  | RevChars0]
    ; Char = '''',   RevChars = ['''', '\\' | RevChars0]
    ; Char = ('\\'), RevChars = ['\\', '\\' | RevChars0]
    ; Char = '\n',   RevChars = ['n', '\\'  | RevChars0]
    ; Char = '\t',   RevChars = ['t', '\\'  | RevChars0]
    ; Char = '\b',   RevChars = ['b', '\\'  | RevChars0]
    ; Char = '\a',   RevChars = ['a', '\\'  | RevChars0]
    ; Char = '\v',   RevChars = ['v', '\\'  | RevChars0]
    ; Char = '\r',   RevChars = ['r', '\\'  | RevChars0]
    ; Char = '\f',   RevChars = ['f', '\\'  | RevChars0]
    ; Char = '?',    RevChars = ['?', '\\'  | RevChars0]   % Avoid trigraphs.
    ).

:- pred escape_special_char_java(char::in, list(char)::in, list(char)::out)
    is semidet.
:- pragma inline(pred(escape_special_char_java/3)).

escape_special_char_java(Char, RevChars0, RevChars) :-
    ( Char = '"',    RevChars = ['"', '\\'  | RevChars0]
    ; Char = '''',   RevChars = ['''', '\\' | RevChars0]
    ; Char = ('\\'), RevChars = ['\\', '\\' | RevChars0]
    ; Char = '\n',   RevChars = ['n', '\\'  | RevChars0]
    ; Char = '\t',   RevChars = ['t', '\\'  | RevChars0]
    ; Char = '\b',   RevChars = ['b', '\\'  | RevChars0]
    ; Char = '\a',   RevChars = ['7', '0', '0', '\\' | RevChars0]
    ; Char = '\v',   RevChars = ['3', '1', '0', '\\' | RevChars0]
    ; Char = '\r',   RevChars = ['r', '\\'  | RevChars0]
    ; Char = '\f',   RevChars = ['f', '\\'  | RevChars0]
    ).

:- pred escape_special_char_csharp(char::in, list(char)::in, list(char)::out)
    is semidet.
:- pragma inline(pred(escape_special_char_csharp/3)).

escape_special_char_csharp(Char, RevChars0, RevChars) :-
    ( Char = '"',    RevChars = ['"', '\\'  | RevChars0]
    ; Char = '''',   RevChars = ['''', '\\' | RevChars0]
    ; Char = ('\\'), RevChars = ['\\', '\\' | RevChars0]
    ; Char = '\n',   RevChars = ['n', '\\'  | RevChars0]
    ; Char = '\t',   RevChars = ['t', '\\'  | RevChars0]
    ; Char = '\b',   RevChars = ['b', '\\'  | RevChars0]
    ; Char = '\a',   RevChars = ['a', '\\'  | RevChars0]
    ; Char = '\v',   RevChars = ['v', '\\'  | RevChars0]
    ; Char = '\r',   RevChars = ['r', '\\'  | RevChars0]
    ; Char = '\f',   RevChars = ['f', '\\'  | RevChars0]
    ).

%---------------------%

    % This succeeds iff the specified character is allowed as an (unescaped)
    % character in standard-conforming C source code. The rules happen to be
    % the same for Java and C# (and most other C-family languages).
    % XXX Do they actually have the same rules with respect to graphic
    % characters?
    %
:- pred is_source_char_c_java_csharp(char::in) is semidet.

is_source_char_c_java_csharp(Char) :-
    ( char.is_alnum(Char)
    ; string.contains_char(c_graphic_chars, Char)
    ).

    % This returns a string containing all the characters that the C standard
    % specifies as being included in the "basic execution character set",
    % except for the letters (a-z A-Z) and digits (0-9).
    %
:- func c_graphic_chars = string.

c_graphic_chars = " !\"#%&'()*+,-./:;<=>?[\\]^_{|}~".

%---------------------%

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

:- pred unicode_escape_any_char(int::in, list(char)::out) is det.

unicode_escape_any_char(CharInt, EscapeCodeChars) :-
    string.format("\\u%04x", [i(CharInt)], HexString),
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

%---------------------------------------------------------------------------%
%
% Unsigned integer literals.
%

output_uint_expr(Stream, N, !IO) :-
    % We need to cast to (MR_Unsigned) to ensure things like 1 << 32 work
    % when `MR_Unsigned' is 64 bits but `unsigned int' is 32 bits.
    io.format(Stream, "(MR_Unsigned) %uU", [u(N)], !IO).

%---------------------------------------------------------------------------%
%
% Fixed size integer literals.
%

% The INTn_C and UINTn_C macros used here are defined in stdint.h.

output_int8_expr(Stream, N, !IO) :-
    io.format(Stream, "INT8_C(%d)", [i8(N)], !IO).

output_uint8_expr(Stream, N, !IO) :-
    io.format(Stream, "UINT8_C(%u)", [u8(N)], !IO).

output_int16_expr(Stream, N, !IO) :-
    io.format(Stream, "INT16_C(%d)", [i16(N)], !IO).

output_uint16_expr(Stream, N, !IO) :-
    io.format(Stream, "UINT16_C(%u)", [u16(N)], !IO).

output_int32_expr(Stream, N, !IO) :-
    ( if N = min_int32 then
        io.write_string(Stream, "INT32_MIN", !IO)
    else
        io.format(Stream, "INT32_C(%d)", [i32(N)], !IO)
    ).

output_uint32_expr(Stream, N, !IO) :-
    io.format(Stream, "UINT32_C(%u)", [u32(N)], !IO).

make_int64_literal(N) = Literal :-
    ( if N = min_int64 then
        Literal = "INT64_MIN"
    else
        string.format("INT64_C(%d)", [i64(N)], Literal)
    ).

output_int64_expr(Stream, N, !IO) :-
    ( if N = min_int64 then
        io.write_string(Stream, "INT64_MIN", !IO)
    else
        io.format(Stream, "INT64_C(%d)", [i64(N)], !IO)
    ).

make_uint64_literal(N) = Literal :-
    string.format("UINT64_C(%u)", [u64(N)], Literal).

output_uint64_expr(Stream, N, !IO) :-
    io.format(Stream, "UINT64_C(%u)", [u64(N)], !IO).

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

%---------------------------------------------------------------------------%
%
% Operators.
%

unary_prefix_op(tag,                "MR_tag").
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
unary_prefix_op(dword_float_get_word0,  "MR_dword_float_get_word0").
unary_prefix_op(dword_float_get_word1,  "MR_dword_float_get_word1").
unary_prefix_op(dword_int64_get_word0,  "MR_dword_int64_get_word0").
unary_prefix_op(dword_int64_get_word1,  "MR_dword_int64_get_word1").
unary_prefix_op(dword_uint64_get_word0, "MR_dword_uint64_get_word0").
unary_prefix_op(dword_uint64_get_word1, "MR_dword_uint64_get_word1").

%---------------------------------------------------------------------------%

output_c_file_intro_and_grade(Globals, Stream, SourceFileName, Version,
        Fullarch, !IO) :-
    globals.lookup_int_option(Globals, num_ptag_bits, NumPtagBits),
    string.int_to_string(NumPtagBits, NumPtagBitsStr),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
    UnboxedFloatStr = convert_bool_to_string(UnboxedFloat),
    globals.lookup_bool_option(Globals, unboxed_int64s, UnboxedInt64s),
    UnboxedInt64sStr = convert_bool_to_string(UnboxedInt64s),
    globals.lookup_bool_option(Globals, pregenerated_dist, PregeneratedDist),
    PregeneratedDistStr = convert_bool_to_string(PregeneratedDist),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    HighLevelCodeStr = convert_bool_to_string(HighLevelCode),

    io.write_strings(Stream, [
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
        "** TAG_BITS=", NumPtagBitsStr, "\n",
        "** UNBOXED_FLOAT=", UnboxedFloatStr, "\n",
        "** UNBOXED_INT64S=", UnboxedInt64sStr, "\n",
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

output_init_c_comment(Stream, ModuleName,
        UserInitPredCNames, UserFinalPredCNames, EnvVarNames, !IO) :-
    ModuleInitName = make_init_name(ModuleName),
    io.write_string(Stream, "/*\n", !IO),
    io.format(Stream, "INIT %sinit\n", [s(ModuleInitName)], !IO),
    % We only print out the REQUIRED_INIT and REQUIRED_FINAL comments
    % if there are user initialisation/finalisation predicates.
    (
        UserInitPredCNames = []
    ;
        UserInitPredCNames = [_ | _],
        io.format(Stream, "REQUIRED_INIT %srequired_init\n",
            [s(ModuleInitName)], !IO)
    ),
    (
        UserFinalPredCNames = []
    ;
        UserFinalPredCNames = [_ | _],
        io.format(Stream, "REQUIRED_FINAL %srequired_final\n",
            [s(ModuleInitName)], !IO)
    ),
    list.foldl(output_env_var_init(Stream), EnvVarNames, !IO),
    % We always write out ENDINIT so that mkinit does not scan the whole file.
    io.write_string(Stream, "ENDINIT\n", !IO),
    io.write_string(Stream, "*/\n\n", !IO).

:- pred output_env_var_init(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

output_env_var_init(Stream, EnvVarName, !IO) :-
    io.format(Stream, "ENVVAR %s\n", [s(EnvVarName)], !IO).

%---------------------------------------------------------------------------%
%
% We could hide these blocks behind macros using the __pragma keyword
% introduced in MSVC 9 (2008):
%
%   #define MR_PRAGMA_PACK_PUSH  __pragma(pack(push, MR_BYTES_PER_WORD))
%   #define MR_PRAGMA_PACK_POP   __pragma(pack(pop))
%

output_pragma_pack_push(Stream, !IO) :-
    io.write_string(Stream, "\n#ifdef MR_MSVC\n", !IO),
    io.write_string(Stream, "#pragma pack(push, MR_BYTES_PER_WORD)\n", !IO),
    io.write_string(Stream, "#endif\n", !IO).

output_pragma_pack_pop(Stream, !IO) :-
    io.write_string(Stream, "#ifdef MR_MSVC\n", !IO),
    io.write_string(Stream, "#pragma pack(pop)\n", !IO),
    io.write_string(Stream, "#endif\n", !IO).

%---------------------------------------------------------------------------%

is_valid_c_identifier(S) :-
    string.index(S, 0, Start),
    char.is_alpha_or_underscore(Start),
    string.is_all_alnum_or_underscore(S).

%---------------------------------------------------------------------------%

hex_hash32(S0) = S :-
    Hash = string.hash(S0),
    % Mask off the lower 32 bits without using 0xffffffff in the generated
    % target code, to avoid warnings from the C compiler.
    Hi = (Hash >> 16) /\ 0xffff,
    Lo = Hash /\ 0xffff,
    S = string.format("%04x%04x", [i(Hi), i(Lo)]).

%---------------------------------------------------------------------------%
:- end_module backend_libs.c_util.
%---------------------------------------------------------------------------%
