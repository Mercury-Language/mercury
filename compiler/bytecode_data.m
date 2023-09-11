%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2000, 2003-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014, 2017, 2019-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: bytecode_data.m.
% Authors: zs, aet, stayl.
%
% This module defines the representation of basic types used by the bytecode
% interpreter.
%
%---------------------------------------------------------------------------%

:- module backend_libs.bytecode_data.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

    % XXX This assumes strings contain 8-bit characters.
    %
:- pred output_string(io.binary_output_stream::in, string::in,
    io::di, io::uo) is det.

:- pred output_byte(io.binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Spit out an `int' in a portable `highest common denominator' format.
    % This format is: big-endian, 64-bit, 2's-complement int.
    %
    % NOTE: We -assume- the machine architecture uses 2's-complement.
    %
:- pred output_int(io.binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Same as output_int, except only use 32 bits.
    %
:- pred output_int32(io.binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Spit out a `short' in a portable format.
    % This format is: big-endian, 16-bit, 2's-complement.
    %
    % NOTE: We -assume- the machine architecture uses 2's-complement.
    %
:- pred output_short(io.binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Spit out a `float' in a portable `highest common denominator format.
    % This format is: big-endian, 64-bit, IEEE-754 floating point value.
    %
    % NOTE: We -assume- the machine architecture uses IEEE-754.
    %
:- pred output_float(io.binary_output_stream::in, float::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

output_string(BinaryOutputStream, Val, !IO) :-
    string_to_byte_list(Val, List),
    list.foldl(io.write_byte(BinaryOutputStream), List, !IO),
    io.write_byte(BinaryOutputStream, 0, !IO).

:- pred string_to_byte_list(string::in, list(int)::out) is det.

string_to_byte_list(Val, List) :-
    % XXX This assumes strings contain 8-bit characters.
    % Using char.to_int here is wrong; the output will depend on the Mercury
    % implementation's representation of chars, so it may be different for
    % different Mercury implementations. In particular, it will do the wrong
    % thing for Mercury implementations which represent characters in Unicode.
    string.to_char_list(Val, Chars),
    ToInt = (pred(C::in, I::out) is det :- char.to_int(C, I)),
    list.map(ToInt, Chars, List0),
    list.append(List0, [0], List).

output_byte(BinaryOutputStream, Val, !IO) :-
    ( if Val < 256 then
        io.write_byte(BinaryOutputStream, Val, !IO)
    else
        unexpected($pred, "byte does not fit in eight bits")
    ).

output_int(BinaryOutputStream, IntVal, !IO) :-
    int.bits_per_int(IntBits),
    ( if IntBits > bytecode_int_bits then
        unexpected($pred,
            "size of int is larger than size of bytecode integer.")
    else
        output_int_general(BinaryOutputStream, io.write_byte,
            bytecode_int_bits, IntVal, !IO)
    ).

output_int32(BinaryOutputStream, IntVal, !IO) :-
    output_int_general(BinaryOutputStream, io.write_byte, 32, IntVal, !IO).

output_short(BinaryOutputStream, Val, !IO) :-
    output_int_general(BinaryOutputStream, io.write_byte, 16, Val, !IO).

:- pred output_int_general(io.binary_output_stream,
    pred(io.binary_output_stream, int, T, T), int, int, T, T).
:- mode output_int_general(in, in(pred(in, in, in, out) is det),
    in, in, in, out) is det.
:- mode output_int_general(in, in(pred(in, in, di, uo) is det),
    in, in, di, uo) is det.

output_int_general(BinaryOutputStream, Writer, Bits, IntVal, !IO) :-
    int.bits_per_int(IntBits),
    ( if
        Bits < IntBits,
        int.pow(2, Bits - 1, MaxVal),
        ( IntVal >= MaxVal
        ; IntVal < -MaxVal
        )
    then
        string.format("%d does not fit in %d bits", [i(IntVal), i(Bits)], Msg),
        unexpected($pred, Msg)
    else
        true
    ),
    ( if Bits > IntBits then
        ZeroPadBytes = (Bits - IntBits) // bits_per_byte
    else
        ZeroPadBytes = 0
    ),
    output_padding_zeros(BinaryOutputStream, Writer, ZeroPadBytes, !IO),
    BytesToDump = Bits // bits_per_byte,
    FirstByteToDump = BytesToDump - ZeroPadBytes - 1,
    output_int_bytes(BinaryOutputStream, Writer, FirstByteToDump, IntVal, !IO).

:- func bytecode_int_bits = int.

bytecode_int_bits = bits_per_byte * bytecode_int_bytes.

:- func bytecode_int_bytes = int.

bytecode_int_bytes = 8.

:- func bits_per_byte = int.

bits_per_byte = 8.

:- pred output_padding_zeros(io.binary_output_stream,
    pred(io.binary_output_stream, int, T, T), int, T, T).
:- mode output_padding_zeros(in, in(pred(in, in, in, out) is det),
    in, in, out) is det.
:- mode output_padding_zeros(in, in(pred(in, in, di, uo) is det),
    in, di, uo) is det.

output_padding_zeros(BinaryOutputStream, Writer, NumBytes, !IO) :-
    ( if NumBytes > 0 then
        Writer(BinaryOutputStream, 0, !IO),
        NumBytes1 = NumBytes - 1,
        output_padding_zeros(BinaryOutputStream, Writer, NumBytes1, !IO)
    else
        true
    ).

:- pred output_int_bytes(io.binary_output_stream,
    pred(io.binary_output_stream, int, T, T), int, int, T, T).
:- mode output_int_bytes(in, in(pred(in, in, in, out) is det),
    in, in, in, out) is det.
:- mode output_int_bytes(in, in(pred(in, in, di, uo) is det),
    in, in, di, uo) is det.

output_int_bytes(BinaryOutputStream, Writer, ByteNum, IntVal, !IO) :-
    ( if ByteNum >= 0 then
        BitShifts = ByteNum * bits_per_byte,
        Byte = (IntVal >> BitShifts) mod (1 << bits_per_byte),
        ByteNum1 = ByteNum - 1,
        Writer(BinaryOutputStream, Byte, !IO),
        output_int_bytes(BinaryOutputStream, Writer, ByteNum1, IntVal, !IO)
    else
        true
    ).

output_float(BinaryOutputStream, Val, !IO) :-
    float_to_float64_bytes(Val, B0, B1, B2, B3, B4, B5, B6, B7),
    output_byte(BinaryOutputStream, B0, !IO),
    output_byte(BinaryOutputStream, B1, !IO),
    output_byte(BinaryOutputStream, B2, !IO),
    output_byte(BinaryOutputStream, B3, !IO),
    output_byte(BinaryOutputStream, B4, !IO),
    output_byte(BinaryOutputStream, B5, !IO),
    output_byte(BinaryOutputStream, B6, !IO),
    output_byte(BinaryOutputStream, B7, !IO).

    % Convert a `float' to the representation used in the bytecode.
    % That is, a sequence of eight bytes.
    %
:- pred float_to_float64_bytes(float::in,
    int::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out) is det.
:- pragma no_determinism_warning(pred(float_to_float64_bytes/9)).

:- pragma foreign_proc("C",
    float_to_float64_bytes(FloatVal::in, B0::out, B1::out, B2::out,
        B3::out, B4::out, B5::out, B6::out, B7::out),
    [promise_pure, will_not_call_mercury],
"
    {
        MR_Float64  float64;
        unsigned char   *raw_mem_p;

        float64 = (MR_Float64) FloatVal;
        raw_mem_p = (unsigned char *) &float64;

        #if defined(MR_BIG_ENDIAN)
            B0 = raw_mem_p[0];
            B1 = raw_mem_p[1];
            B2 = raw_mem_p[2];
            B3 = raw_mem_p[3];
            B4 = raw_mem_p[4];
            B5 = raw_mem_p[5];
            B6 = raw_mem_p[6];
            B7 = raw_mem_p[7];
        #elif defined(MR_LITTLE_ENDIAN)
            B7 = raw_mem_p[0];
            B6 = raw_mem_p[1];
            B5 = raw_mem_p[2];
            B4 = raw_mem_p[3];
            B3 = raw_mem_p[4];
            B2 = raw_mem_p[5];
            B1 = raw_mem_p[6];
            B0 = raw_mem_p[7];
        #else
            #error  ""Weird-endian architecture""
        #endif
    }
").

float_to_float64_bytes(_FloatVal, _B0, _B1, _B2, _B3, _B4, _B5, _B6, _B7) :-
    sorry($pred, "float_to_float64_bytes for non-C target").

%---------------------------------------------------------------------------%
:- end_module backend_libs.bytecode_data.
%---------------------------------------------------------------------------%
