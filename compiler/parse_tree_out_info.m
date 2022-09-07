%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module provides the basic infrastructure needed to print out
% the parse tree of a Mercury module, in whole or in part.
%
% This infrastructure has two parts.
%
% The first part of the infrastructure is the merc_out_info type. Values of
% this type control those low-level aspects of how parse tree components
% are printed that may differ depending on *why* we want to print them,
% such as whether the goal is to generate valid Mercury code or to print
% as much detail as possible for debugging, even if those details are
% not expressible in Mercury syntax.
%
% The second is the "output" type class. Many (though not all) of the
% procedures that output parts of the parse tree come in groups of three,
% where the three follow the pattern:
%
%   :- pred mercury_output_xyz(io.text_output_stream::in, ...,
%       io::di, io::uo) is det.
%   :- func mercury_xyz_to_string(...) = string.
%   :- pred mercury_format_xyz(..., S::in, U::di, U::uo) is det
%       <= output(S, U).
%
% In most cases, the first two simply forward all the work to the third.
% This is possible because the tuples (io.text_output_stream, io.state)
% and (unit, string) are members of the output(S, U) typeclass.
%
% For the mercury_output_xyz versions, going through a typeclass interface is
% (for now) a slight slowdown, but the time cost is still small compared to
% the cost of I/O itself.
%
% For the mercury_xyz_to_string versions, the cost is acceptable because
% (for now) we only create relatively small strings this way, e.g. strings that
% go into error messages. The typeclass instance for strings has a quadratic
% complexity in the number of strings being appended but a reasonably low
% constant factor. If we ever want to use these functions to create long
% strings (longer than a few lines), then we should use a typeclass
% instance implementation that represents the entity being converted to string
% as a cord of strings that must be concatenated together at the end using
% cord.list and string.append_list. The complexity of an implementation
% like that can be linear in the size of the string being built, although
% it will have a higher constant factor. The biggest problem with using it
% will be the need for explicit conversion step to a plain old string
% at the end.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_info.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module term.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type merc_out_info.

:- type maybe_qualified_item_names
    --->    unqualified_item_names
    ;       qualified_item_names.

:- type maybe_output_line_numbers
    --->    dont_output_line_numbers
    ;       do_output_line_numbers.

:- type type_repn_for
    --->    type_repn_for_machines
    ;       type_repn_for_humans.

    % Are we generating output that has be able to be read back in as
    % valid Mercury, e.g. when the output goes to a .int* or .*opt file,
    % or are we generating output only for humans to read?
    %
    % XXX We should split output for humans into two: one for developers,
    % who won't mind, and will often need, variable numbers, and one
    % for ordinary users, who don't, and shouldn't have to, know about
    % the existence of variable numbers.
    %
    % XXX Since not all combinations of output_lang and var_name_print
    % make sense, we shouldn't pass values of the output_lang and
    % var_name_print types next to each other, as we now do in many places.
    % Instead, each alternative here should *contain* the var_name_print
    % value that we now pass next to it, but *only* if there is more than one
    % var_name_print value that makes sense for the value of output_lang.
    % This would require putting the two types next to each other.
    %
:- type output_lang
    --->    output_mercury
    ;       output_debug.

:- func init_debug_merc_out_info = merc_out_info.
:- func init_merc_out_info(globals, maybe_qualified_item_names, output_lang)
    = merc_out_info.
:- func merc_out_info_disable_line_numbers(merc_out_info) = merc_out_info.

:- func get_maybe_qualified_item_names(merc_out_info)
    = maybe_qualified_item_names.
:- func get_output_line_numbers(merc_out_info) = maybe_output_line_numbers.
:- func get_output_lang(merc_out_info) = output_lang.
:- func get_type_repn_for(merc_out_info) = type_repn_for.
:- func get_human_comma_sep(merc_out_info) = string.

:- pred maybe_output_line_number(merc_out_info::in, prog_context::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred maybe_unqualify_sym_name(merc_out_info::in,
    sym_name::in, sym_name::out) is det.

:- pred write_out_list(
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    string::in, list(T)::in, io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- typeclass output(S, U) <= (U -> S) where [
    pred add_string(string::in, S::in, U::di, U::uo) is det,
    pred add_strings(list(string)::in, S::in, U::di, U::uo) is det,
    pred add_char(char::in, S::in, U::di, U::uo) is det,
    pred add_int(int::in, S::in, U::di, U::uo) is det,
    pred add_uint(uint::in, S::in, U::di, U::uo) is det,
    pred add_int8(int8::in, S::in, U::di, U::uo) is det,
    pred add_uint8(uint8::in, S::in, U::di, U::uo) is det,
    pred add_int16(int16::in, S::in, U::di, U::uo) is det,
    pred add_uint16(uint16::in, S::in, U::di, U::uo) is det,
    pred add_int32(int32::in, S::in, U::di, U::uo) is det,
    pred add_uint32(uint32::in, S::in, U::di, U::uo) is det,
    pred add_int64(int64::in, S::in, U::di, U::uo) is det,
    pred add_uint64(uint64::in, S::in, U::di, U::uo) is det,
    pred add_float(float::in, S::in, U::di, U::uo) is det,
    pred add_purity_prefix(purity::in, S::in, U::di, U::uo) is det,
    pred add_quoted_atom(string::in, S::in, U::di, U::uo) is det,
    pred add_quoted_string(string::in, S::in, U::di, U::uo) is det,
    pred add_constant(const::in, S::in, U::di, U::uo) is det,
    pred add_eval_method(eval_method::in, S::in, U::di, U::uo) is det,
    pred add_lambda_eval_method(lambda_eval_method::in, S::in,
        U::di, U::uo) is det,
    pred add_escaped_string(string::in, S::in, U::di, U::uo) is det,
    % The add_list predicate calls the predicate argument to add each
    % element of the list to the specified stream, printing the specified
    % separator between each pair of elements.
    pred add_list(pred(T, S, U, U)::in(pred(in, in, di, uo) is det),
        string::in, list(T)::in, S::in, U::di, U::uo) is det
].

:- instance output(io.text_output_stream, io.state).
:- instance output(unit, string).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

:- type merc_out_info
    --->    merc_out_info(
                moi_qualify_item_names      :: maybe_qualified_item_names,
                moi_output_line_numbers     :: maybe_output_line_numbers,
                moi_output_lang             :: output_lang,

                % When writing out a comma in a type_repn, or some other
                % output that humans may want to look at, what should
                % we print to separate it from what follows?
                %
                % For humans, ",\n    "; for computers, just ", ".
                moi_type_repn_for           :: type_repn_for,
                moi_human_comma_sep         :: string
            ).

init_debug_merc_out_info = Info :-
    Info = merc_out_info(qualified_item_names, dont_output_line_numbers,
        output_debug, type_repn_for_machines, ", ").

init_merc_out_info(Globals, MaybeQualifiedItemNames, Lang) = Info :-
    globals.lookup_bool_option(Globals, line_numbers, LineNumbersOpt),
    globals.lookup_bool_option(Globals, type_repns_for_humans,
        TypeRepnsForHumans),
    ( LineNumbersOpt = no, LineNumbers = dont_output_line_numbers
    ; LineNumbersOpt = yes, LineNumbers = do_output_line_numbers
    ),
    ( TypeRepnsForHumans = no, For = type_repn_for_machines, CommaSep = ", "
    ; TypeRepnsForHumans = yes, For = type_repn_for_humans, CommaSep = ",\n    "
    ),
    Info = merc_out_info(MaybeQualifiedItemNames, LineNumbers, Lang,
        For, CommaSep).

merc_out_info_disable_line_numbers(Info0) = Info :-
    Info = Info0 ^ moi_output_line_numbers := dont_output_line_numbers.

get_maybe_qualified_item_names(Info) = Info ^ moi_qualify_item_names.
get_output_line_numbers(Info) = Info ^ moi_output_line_numbers.
get_output_lang(Info) = Info ^ moi_output_lang.
get_type_repn_for(Info) = Info ^ moi_type_repn_for.
get_human_comma_sep(Info) = Info ^ moi_human_comma_sep.

%---------------------------------------------------------------------------%

maybe_output_line_number(Info, Context, Stream, !IO) :-
    LineNumbers = get_output_line_numbers(Info),
    (
        LineNumbers = do_output_line_numbers,
        io.write_string(Stream, "\t% ", !IO),
        prog_out.write_context(Stream, Context, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        LineNumbers = dont_output_line_numbers
    ).

maybe_unqualify_sym_name(Info, SymName, OutSymName) :-
    MaybeQualifiedItemNames = get_maybe_qualified_item_names(Info),
    (
        MaybeQualifiedItemNames = qualified_item_names,
        OutSymName = SymName
    ;
        MaybeQualifiedItemNames = unqualified_item_names,
        OutSymName = unqualified(unqualify_name(SymName))
    ).

%---------------------------------------------------------------------------%

:- instance output(io.text_output_stream, io.state) where [
    pred(add_string/4) is write_string,
    pred(add_strings/4) is write_strings,
    pred(add_char/4) is write_char_literal,
    pred(add_int/4) is write_int_literal,
    pred(add_uint/4) is write_uint_literal,
    pred(add_int8/4) is write_int8_literal,
    pred(add_uint8/4) is write_uint8_literal,
    pred(add_int16/4) is write_int16_literal,
    pred(add_uint16/4) is write_uint16_literal,
    pred(add_int32/4) is write_int32_literal,
    pred(add_uint32/4) is write_uint32_literal,
    pred(add_int64/4) is write_int64_literal,
    pred(add_uint64/4) is write_uint64_literal,
    pred(add_float/4) is write_float_literal,
    pred(add_purity_prefix/4) is write_purity_prefix,
    pred(add_quoted_atom/4) is write_quoted_atom,
    pred(add_quoted_string/4) is write_quoted_string,
    pred(add_constant/4) is write_constant,
    pred(add_eval_method/4) is write_eval_eval_method,
    pred(add_lambda_eval_method/4) is write_lambda_eval_method,
    pred(add_escaped_string/4) is write_escaped_string,
    pred(add_list/6) is write_out_list
].

:- instance output(unit, string) where [
    pred(add_string/4) is output_string,
    pred(add_strings/4) is output_strings,
    pred(add_char/4) is output_char,
    pred(add_int/4) is output_int,
    pred(add_uint/4) is output_uint,
    pred(add_int8/4) is output_int8,
    pred(add_uint8/4) is output_uint8,
    pred(add_int16/4) is output_int16,
    pred(add_uint16/4) is output_uint16,
    pred(add_int32/4) is output_int32,
    pred(add_uint32/4) is output_uint32,
    pred(add_int64/4) is output_int64,
    pred(add_uint64/4) is output_uint64,
    pred(add_float/4) is output_float,
    pred(add_purity_prefix/4) is output_purity_prefix,
    pred(add_quoted_atom/4) is output_quoted_atom,
    pred(add_quoted_string/4) is output_quoted_string,
    pred(add_constant/4) is output_constant,
    pred(add_eval_method/4) is output_eval_eval_method,
    pred(add_lambda_eval_method/4) is output_lambda_eval_method,
    pred(add_escaped_string/4) is output_escaped_string,
    pred(add_list/6) is output_list
].

%---------------------------------------------------------------------------%

:- pred write_string(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_string(Str, Stream, !IO) :-
    io.write_string(Stream, Str, !IO).

:- pred write_strings(list(string)::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_strings(Strs, Stream, !IO) :-
    io.write_strings(Stream, Strs, !IO).

:- pred write_char_literal(char::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_char_literal(C, Stream, !IO) :-
    io.write_char(Stream, C, !IO).

:- pred write_int_literal(int::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int_literal(Int, Stream, !IO) :-
    io.write_int(Stream, Int, !IO).

:- pred write_uint_literal(uint::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint_literal(UInt, Stream, !IO) :-
    io.write_uint(Stream, UInt, !IO),
    io.write_char(Stream, 'u', !IO).

:- pred write_int8_literal(int8::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int8_literal(Int8, Stream, !IO) :-
    io.write_int8(Stream, Int8, !IO),
    io.write_string(Stream, "i8", !IO).

:- pred write_uint8_literal(uint8::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint8_literal(UInt8, Stream, !IO) :-
    io.write_uint8(Stream, UInt8, !IO),
    io.write_string(Stream, "u8", !IO).

:- pred write_int16_literal(int16::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int16_literal(Int16, Stream, !IO) :-
    io.write_int16(Stream, Int16, !IO),
    io.write_string(Stream, "i16", !IO).

:- pred write_uint16_literal(uint16::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint16_literal(UInt16, Stream, !IO) :-
    io.write_uint16(Stream, UInt16, !IO),
    io.write_string(Stream, "u16", !IO).

:- pred write_int32_literal(int32::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int32_literal(Int32, Stream, !IO) :-
    io.write_int32(Stream, Int32, !IO),
    io.write_string(Stream, "i32", !IO).

:- pred write_uint32_literal(uint32::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint32_literal(UInt32, Stream, !IO) :-
    io.write_uint32(Stream, UInt32, !IO),
    io.write_string(Stream, "u32", !IO).

:- pred write_int64_literal(int64::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int64_literal(Int64, Stream, !IO) :-
    io.write_int64(Stream, Int64, !IO),
    io.write_string(Stream, "i64", !IO).

:- pred write_uint64_literal(uint64::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint64_literal(UInt64, Stream, !IO) :-
    io.write_uint64(Stream, UInt64, !IO),
    io.write_string(Stream, "u64", !IO).

:- pred write_float_literal(float::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_float_literal(Float, Stream, !IO) :-
    io.write_float(Stream, Float, !IO).

%-------------%

:- pred write_purity_prefix(purity::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_purity_prefix(Purity, Stream, !IO) :-
    PurityPrefixStr = purity_prefix_to_string(Purity),
    io.write_string(Stream, PurityPrefixStr, !IO).

:- pred write_quoted_atom(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_quoted_atom(Atom, Stream, !IO) :-
    term_io.quote_atom(Stream, Atom, !IO).

:- pred write_quoted_string(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_quoted_string(Str, Stream, !IO) :-
    term_io.quote_string(Stream, Str, !IO).

:- pred write_constant(const::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_constant(Const, Stream, !IO) :-
    term_io.write_constant(Stream, Const, !IO).

:- pred write_eval_eval_method(eval_method::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_eval_eval_method(EvalMethod, Stream, !IO) :-
    output_eval_eval_method(EvalMethod, unit, "", EvalMethodStr),
    io.write_string(Stream, EvalMethodStr, !IO).

:- pred write_lambda_eval_method(lambda_eval_method::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_lambda_eval_method(LambdaEvalMethod, Stream, !IO) :-
    output_lambda_eval_method(LambdaEvalMethod, unit,
        "", LambdaEvalMethodStr),
    io.write_string(Stream, LambdaEvalMethodStr, !IO).

:- pred write_escaped_string(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_escaped_string(Str, Stream, !IO) :-
    term_io.write_escaped_string(Stream, Str, !IO).

%-------------%

write_out_list(_, _, [], _, !IO).
write_out_list(WritePred, Separator, [Item | Items], Stream, !IO) :-
    write_out_list_lag(WritePred, Separator, Item, Items, Stream, !IO).

:- pred write_out_list_lag(
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    string::in, T::in, list(T)::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_out_list_lag(WritePred, Separator, Item1, Items2plus, Stream, !IO) :-
    WritePred(Item1, Stream, !IO),
    (
        Items2plus = []
    ;
        Items2plus = [Item2 | Items3plus],
        io.write_string(Stream, Separator, !IO),
        write_out_list_lag(WritePred, Separator, Item2, Items3plus,
            Stream, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_string(string::in, unit::in, string::di, string::uo) is det.

output_string(S, _, Str0, Str) :-
    Str = Str0 ++ S.

:- pred output_strings(list(string)::in, unit::in,
    string::di, string::uo) is det.

output_strings(Strs, _, Str0, Str) :-
    string.append_list([Str0 | Strs], Str).

:- pred output_char(char::in, unit::in, string::di, string::uo) is det.

output_char(C, _, Str0, Str) :-
    Str = Str0 ++ string.char_to_string(C).

:- pred output_int(int::in, unit::in, string::di, string::uo) is det.

output_int(I, _, Str0, Str) :-
    Str = Str0 ++ string.int_to_string(I).

:- pred output_uint(uint::in, unit::in, string::di, string::uo) is det.

output_uint(U, _, Str0, Str) :-
    Str = Str0 ++ string.uint_to_string(U) ++ "u".

:- pred output_int8(int8::in, unit::in, string::di, string::uo) is det.

output_int8(I8, _, Str0, Str) :-
    S = string.int8_to_string(I8) ++ "i8",
    string.append(Str0, S, Str).

:- pred output_uint8(uint8::in, unit::in, string::di, string::uo) is det.

output_uint8(U8, _, Str0, Str) :-
    S = string.uint8_to_string(U8) ++ "u8",
    string.append(Str0, S, Str).

:- pred output_int16(int16::in, unit::in, string::di, string::uo) is det.

output_int16(I16, _, Str0, Str) :-
    S = string.int16_to_string(I16) ++ "i16",
    string.append(Str0, S, Str).

:- pred output_uint16(uint16::in, unit::in, string::di, string::uo) is det.

output_uint16(U16, _, Str0, Str) :-
    S = string.uint16_to_string(U16) ++ "u16",
    string.append(Str0, S, Str).

:- pred output_int32(int32::in, unit::in, string::di, string::uo) is det.

output_int32(I32, _, Str0, Str) :-
    S = string.int32_to_string(I32) ++ "i32",
    string.append(Str0, S, Str).

:- pred output_uint32(uint32::in, unit::in, string::di, string::uo) is det.

output_uint32(U32, _, Str0, Str) :-
    S = string.uint32_to_string(U32) ++ "u32",
    string.append(Str0, S, Str).

:- pred output_int64(int64::in, unit::in, string::di, string::uo) is det.

output_int64(I64, _, Str0, Str) :-
    S = string.int64_to_string(I64) ++ "i64",
    string.append(Str0, S, Str).

:- pred output_uint64(uint64::in, unit::in, string::di, string::uo) is det.

output_uint64(U64, _, Str0, Str) :-
    S = string.uint64_to_string(U64) ++ "u64",
    string.append(Str0, S, Str).

:- pred output_float(float::in, unit::in, string::di, string::uo) is det.

output_float(F, _, Str0, Str) :-
    string.float_to_string(F, S),
    string.append(Str0, S, Str).

:- pred output_purity_prefix(purity::in, unit::in,
    string::di, string::uo) is det.

output_purity_prefix(P, _, Str0, Str) :-
    S = purity_prefix_to_string(P),
    string.append(Str0, S, Str).

:- pred output_quoted_atom(string::in, unit::in,
    string::di, string::uo) is det.

output_quoted_atom(A, _, Str0, Str) :-
    QA = term_io.quoted_atom(A),
    string.append(Str0, QA, Str).

:- pred output_quoted_string(string::in, unit::in,
    string::di, string::uo) is det.

output_quoted_string(A, _, Str0, Str) :-
    QA = term_io.quoted_string(A),
    string.append(Str0, QA, Str).

:- pred output_constant(const::in, unit::in, string::di, string::uo) is det.

output_constant(C, _, Str0, Str) :-
    CS = term_io.format_constant(C),
    string.append(Str0, CS, Str).

:- pred output_escaped_string(string::in, unit::in,
    string::di, string::uo) is det.

output_escaped_string(S, _, Str0, Str) :-
    ES = term_io.escaped_string(S),
    string.append(Str0, ES, Str).

:- pred output_eval_eval_method(eval_method::in, unit::in,
    string::di, string::uo) is det.

output_eval_eval_method(EvalMethod, _, !Str) :-
    output_string("eval_", unit, !Str),
    output_string(eval_method_to_string(EvalMethod), unit, !Str).

:- pred output_lambda_eval_method(lambda_eval_method::in, unit::in,
    string::di, string::uo) is det.

output_lambda_eval_method(lambda_normal, _, !Str) :-
    output_string("normal", unit, !Str).

:- pred output_list(
    pred(T, unit, string, string)::in(pred(in, in, di, uo) is det),
    string::in, list(T)::in, unit::in, string::di, string::uo) is det.

output_list(_, _, [], _, !Str).
output_list(OutputPred, Sep, [Item | Items], _, !Str) :-
    output_list_lag(OutputPred, Sep, Item, Items, !Str).

:- pred output_list_lag(
    pred(T, unit, string, string)::in(pred(in, in, di, uo) is det),
    string::in, T::in, list(T)::in, string::di, string::uo) is det.

output_list_lag(OutputPred, Sep, Item1, Items, !Str) :-
    OutputPred(Item1, unit, !Str),
    (
        Items = []
    ;
        Items = [Item2 | Items3plus],
        output_string(Sep, unit, !Str),
        output_list_lag(OutputPred, Sep, Item2, Items3plus, !Str)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_info.
%---------------------------------------------------------------------------%
