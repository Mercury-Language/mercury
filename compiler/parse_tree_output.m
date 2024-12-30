%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the "pt_output" type class. Many (though not all) of the
% procedures that output parts of the parse tree come in groups of three,
% where the three follow the pattern:
%
%   :- pred mercury_output_xyz(io.text_output_stream::in, ...,
%       io::di, io::uo) is det.
%   :- func mercury_xyz_to_string(...) = string.
%   :- pred mercury_format_xyz(..., S::in, U::di, U::uo) is det
%       <= pt_output(S, U).
%
% In most cases, the first two simply forward all the work to the third.
% This is possible because the tuples (io.text_output_stream, io.state)
% and (string.builder.handle, string.builder.state) are members of the
% pt_output(S, U) typeclass.
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

:- module parse_tree.parse_tree_output.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module stream.
:- import_module string.
:- import_module string.builder.
:- import_module term.

%---------------------------------------------------------------------------%

:- typeclass pt_output(S, U) <= ((U -> S), stream.writer(S, string, U)) where
[
    pred add_char(char::in, S::in, U::di, U::uo) is det,

    pred add_string(string::in, S::in, U::di, U::uo) is det,
    pred add_strings(list(string)::in, S::in, U::di, U::uo) is det,
    pred add_escaped_string(string::in, S::in, U::di, U::uo) is det,
    pred add_quoted_string(string::in, S::in, U::di, U::uo) is det,

    pred add_int(int::in, S::in, U::di, U::uo) is det,
    pred add_int8(int8::in, S::in, U::di, U::uo) is det,
    pred add_int16(int16::in, S::in, U::di, U::uo) is det,
    pred add_int32(int32::in, S::in, U::di, U::uo) is det,
    pred add_int64(int64::in, S::in, U::di, U::uo) is det,

    pred add_uint(uint::in, S::in, U::di, U::uo) is det,
    pred add_uint8(uint8::in, S::in, U::di, U::uo) is det,
    pred add_uint16(uint16::in, S::in, U::di, U::uo) is det,
    pred add_uint32(uint32::in, S::in, U::di, U::uo) is det,
    pred add_uint64(uint64::in, S::in, U::di, U::uo) is det,

    pred add_float(float::in, S::in, U::di, U::uo) is det,

    pred add_purity_prefix(purity::in, S::in, U::di, U::uo) is det,
    pred add_quoted_atom(string::in, S::in, U::di, U::uo) is det,
    pred add_constant(const::in, S::in, U::di, U::uo) is det,
    pred add_eval_method(eval_method::in, S::in, U::di, U::uo) is det,

    % The add_list predicate calls the predicate argument to add each
    % element of the list to the specified stream, printing the specified
    % separator between each pair of elements.
    pred add_list(pred(T, S, U, U)::in(pred(in, in, di, uo) is det),
        string::in, list(T)::in, S::in, U::di, U::uo) is det
].

:- instance pt_output(io.text_output_stream, io.state).
:- instance pt_output(string.builder.handle, string.builder.state).

%---------------------------------------------------------------------------%

:- pred write_out_list(
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    string::in, list(T)::in, io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_misc.

:- import_module term_io.

%---------------------------------------------------------------------------%

:- instance pt_output(io.text_output_stream, io.state) where
[
    pred(add_char/4) is write_char_literal,

    pred(add_string/4) is write_string,
    pred(add_strings/4) is write_strings,
    pred(add_escaped_string/4) is write_escaped_string,
    pred(add_quoted_string/4) is write_quoted_string,

    pred(add_int/4) is write_int_literal,
    pred(add_int8/4) is write_int8_literal,
    pred(add_int16/4) is write_int16_literal,
    pred(add_int32/4) is write_int32_literal,
    pred(add_int64/4) is write_int64_literal,

    pred(add_uint/4) is write_uint_literal,
    pred(add_uint8/4) is write_uint8_literal,
    pred(add_uint16/4) is write_uint16_literal,
    pred(add_uint32/4) is write_uint32_literal,
    pred(add_uint64/4) is write_uint64_literal,

    pred(add_float/4) is write_float_literal,

    pred(add_purity_prefix/4) is write_purity_prefix,
    pred(add_quoted_atom/4) is write_quoted_atom,
    pred(add_constant/4) is write_constant,
    pred(add_eval_method/4) is write_eval_eval_method,

    pred(add_list/6) is write_out_list
].

:- instance pt_output(string.builder.handle, string.builder.state) where
[
    pred(add_char/4) is build_char,

    pred(add_string/4) is build_string,
    pred(add_strings/4) is build_strings,
    pred(add_escaped_string/4) is build_escaped_string,
    pred(add_quoted_string/4) is build_quoted_string,

    pred(add_int/4) is build_int,
    pred(add_int8/4) is build_int8,
    pred(add_int16/4) is build_int16,
    pred(add_int32/4) is build_int32,
    pred(add_int64/4) is build_int64,

    pred(add_uint/4) is build_uint,
    pred(add_uint8/4) is build_uint8,
    pred(add_uint16/4) is build_uint16,
    pred(add_uint32/4) is build_uint32,
    pred(add_uint64/4) is build_uint64,

    pred(add_float/4) is build_float,

    pred(add_purity_prefix/4) is build_purity_prefix,
    pred(add_quoted_atom/4) is build_quoted_atom,
    pred(add_constant/4) is build_constant,
    pred(add_eval_method/4) is build_eval_eval_method,

    pred(add_list/6) is build_list
].

%---------------------------------------------------------------------------%

:- pred write_char_literal(char::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_char_literal(C, Stream, !IO) :-
    io.write_char(Stream, C, !IO).

%---------------------%

:- pred write_string(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_string(Str, Stream, !IO) :-
    io.write_string(Stream, Str, !IO).

:- pred write_strings(list(string)::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_strings(Strs, Stream, !IO) :-
    io.write_strings(Stream, Strs, !IO).

:- pred write_escaped_string(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_escaped_string(Str, Stream, !IO) :-
    term_io.format_escaped_string(Stream, Str, !IO).

:- pred write_quoted_string(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_quoted_string(Str, Stream, !IO) :-
    term_io.format_quoted_string(Stream, Str, !IO).

%---------------------%

:- pred write_int_literal(int::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int_literal(Int, Stream, !IO) :-
    io.write_int(Stream, Int, !IO).

:- pred write_int8_literal(int8::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int8_literal(Int8, Stream, !IO) :-
    io.write_int8(Stream, Int8, !IO),
    io.write_string(Stream, "i8", !IO).

:- pred write_int16_literal(int16::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int16_literal(Int16, Stream, !IO) :-
    io.write_int16(Stream, Int16, !IO),
    io.write_string(Stream, "i16", !IO).

:- pred write_int32_literal(int32::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int32_literal(Int32, Stream, !IO) :-
    io.write_int32(Stream, Int32, !IO),
    io.write_string(Stream, "i32", !IO).

:- pred write_int64_literal(int64::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int64_literal(Int64, Stream, !IO) :-
    io.write_int64(Stream, Int64, !IO),
    io.write_string(Stream, "i64", !IO).

%---------------------%

:- pred write_uint_literal(uint::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint_literal(UInt, Stream, !IO) :-
    io.write_uint(Stream, UInt, !IO),
    io.write_char(Stream, 'u', !IO).

:- pred write_uint8_literal(uint8::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint8_literal(UInt8, Stream, !IO) :-
    io.write_uint8(Stream, UInt8, !IO),
    io.write_string(Stream, "u8", !IO).

:- pred write_uint16_literal(uint16::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint16_literal(UInt16, Stream, !IO) :-
    io.write_uint16(Stream, UInt16, !IO),
    io.write_string(Stream, "u16", !IO).

:- pred write_uint32_literal(uint32::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint32_literal(UInt32, Stream, !IO) :-
    io.write_uint32(Stream, UInt32, !IO),
    io.write_string(Stream, "u32", !IO).

:- pred write_uint64_literal(uint64::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_uint64_literal(UInt64, Stream, !IO) :-
    io.write_uint64(Stream, UInt64, !IO),
    io.write_string(Stream, "u64", !IO).

%---------------------%

:- pred write_float_literal(float::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_float_literal(Float, Stream, !IO) :-
    io.write_float(Stream, Float, !IO).

%---------------------%

:- pred write_purity_prefix(purity::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_purity_prefix(Purity, Stream, !IO) :-
    PurityPrefixStr = purity_prefix_to_string(Purity),
    io.write_string(Stream, PurityPrefixStr, !IO).

:- pred write_quoted_atom(string::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_quoted_atom(Atom, Stream, !IO) :-
    term_io.format_quoted_atom(Stream, Atom, !IO).

:- pred write_constant(const::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_constant(Const, Stream, !IO) :-
    term_io.write_constant(Stream, Const, !IO).

:- pred write_eval_eval_method(eval_method::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_eval_eval_method(EvalMethod, Stream, !IO) :-
    io.write_string(Stream, "eval_", !IO),
    io.write_string(Stream, eval_method_to_string(EvalMethod), !IO).

%---------------------%

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

:- pred build_char(char::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_char(Char, _, !State) :-
    string.builder.append_char(Char, !State).

%---------------------%

:- pred build_string(string::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_string(Str, _, !State) :-
    string.builder.append_string(Str, !State).

:- pred build_strings(list(string)::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_strings(Strs, _, !State) :-
    string.builder.append_strings(Strs, !State).

:- pred build_escaped_string(string::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_escaped_string(S, _, !State) :-
    term_io.format_escaped_string(string.builder.handle, S, !State).

:- pred build_quoted_string(string::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_quoted_string(A, _, !State) :-
    term_io.format_quoted_string(string.builder.handle, A, !State).

%---------------------%

:- pred build_int(int::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_int(I, _, !State) :-
    string.builder.append_string(string.int_to_string(I), !State).

:- pred build_int8(int8::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_int8(I8, _, !State) :-
    string.builder.append_string(string.int8_to_string(I8), !State),
    string.builder.append_string("i8", !State).

:- pred build_int16(int16::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_int16(I16, _, !State) :-
    string.builder.append_string(string.int16_to_string(I16), !State),
    string.builder.append_string("i16", !State).

:- pred build_int32(int32::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_int32(I32, _, !State) :-
    string.builder.append_string(string.int32_to_string(I32), !State),
    string.builder.append_string("i32", !State).

:- pred build_int64(int64::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_int64(I64, _, !State) :-
    string.builder.append_string(string.int64_to_string(I64), !State),
    string.builder.append_string("i64", !State).

%---------------------%

:- pred build_uint(uint::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_uint(U, _, !State) :-
    string.builder.append_string(string.uint_to_string(U), !State),
    string.builder.append_string("u", !State).

:- pred build_uint8(uint8::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_uint8(U8, _, !State) :-
    string.builder.append_string(string.uint8_to_string(U8), !State),
    string.builder.append_string("u8", !State).

:- pred build_uint16(uint16::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_uint16(U16, _, !State) :-
    string.builder.append_string(string.uint16_to_string(U16), !State),
    string.builder.append_string("u16", !State).

:- pred build_uint32(uint32::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_uint32(U32, _, !State) :-
    string.builder.append_string(string.uint32_to_string(U32), !State),
    string.builder.append_string("u32", !State).

:- pred build_uint64(uint64::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_uint64(U64, _, !State) :-
    string.builder.append_string(string.uint64_to_string(U64), !State),
    string.builder.append_string("u64", !State).

%---------------------%

:- pred build_float(float::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_float(F, _, !State) :-
    string.builder.append_string(string.float_to_string(F), !State).

%---------------------%

:- pred build_purity_prefix(purity::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_purity_prefix(P, _, !State) :-
    string.builder.append_string(purity_prefix_to_string(P), !State).

:- pred build_quoted_atom(string::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_quoted_atom(A, _, !State) :-
    term_io.format_quoted_atom(string.builder.handle, A, !State).

:- pred build_constant(const::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_constant(C, _, !State) :-
    term_io.format_constant(string.builder.handle, C, !State).

:- pred build_eval_eval_method(eval_method::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_eval_eval_method(EvalMethod, _, !State) :-
    string.builder.append_string("eval_", !State),
    string.builder.append_string(eval_method_to_string(EvalMethod), !State).

%---------------------%

:- pred build_list(
    pred(T, string.builder.handle, string.builder.state, string.builder.state)
        ::in(pred(in, in, di, uo) is det),
    string::in, list(T)::in, string.builder.handle::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_list(_, _, [], _, !State).
build_list(OutputPred, Sep, [Item | Items], _, !State) :-
    build_list_lag(OutputPred, Sep, Item, Items, !State).

:- pred build_list_lag(
    pred(T, string.builder.handle, string.builder.state, string.builder.state)
        ::in(pred(in, in, di, uo) is det),
    string::in, T::in, list(T)::in,
    string.builder.state::di, string.builder.state::uo) is det.

build_list_lag(OutputPred, Sep, Item1, Items, !State) :-
    OutputPred(Item1, string.builder.handle, !State),
    (
        Items = []
    ;
        Items = [Item2 | Items3plus],
        string.builder.append_string(Sep, !State),
        build_list_lag(OutputPred, Sep, Item2, Items3plus, !State)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_output.
%---------------------------------------------------------------------------%
