%---------------------------------------------------------------------------%
% Copyright (C) 1999-2000, 2003-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the representation of basic types used by
% the bytecode interpreter and by the Aditi bytecodes.
%
% Note: This file is included in both the Mercury compiler
% and the Aditi bytecode assembler.
%
% Author: zs, aet, stayl.
%
%---------------------------------------------------------------------------%

:- module backend_libs__bytecode_data.

:- interface.

:- import_module io, int, list, string.

	% XXX this assumes strings contain 8-bit characters
:- pred output_string(string::in, io::di, io::uo) is det.
:- pred string_to_byte_list(string::in, list(int)::out) is det.

:- pred output_byte(int::in, io::di, io::uo) is det.

/*
** Spit out an `int' in a portable `highest common denominator' format.
** This format is: big-endian, 64-bit, 2's-complement int.
**
** NOTE: We -assume- the machine architecture uses 2's-complement.
*/

:- pred output_int(int::in, io::di, io::uo) is det.
:- pred int_to_byte_list(int::in, list(int)::out) is det.

/*
** Same as output_int and int_to_byte_list, except only use 32 bits.
*/

:- pred output_int32(int::in, io::di, io::uo) is det.
:- pred int32_to_byte_list(int::in, list(int)::out) is det.

/*
** Spit out a `short' in a portable format.
** This format is: big-endian, 16-bit, 2's-complement.
**
** NOTE: We -assume- the machine architecture uses 2's-complement.
*/

:- pred output_short(int::in, io::di, io::uo) is det.
:- pred short_to_byte_list(int::in, list(int)::out) is det.

/*
** Spit out a `float' in a portable `highest common denominator format.
** This format is: big-endian, 64-bit, IEEE-754 floating point value.
**
** NOTE: We -assume- the machine architecture uses IEEE-754.
*/

:- pred output_float(float::in, io::di, io::uo) is det.
:- pred float_to_byte_list(float::in, list(int)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__error_util.

:- import_module char, require.

output_string(Val, !IO) :-
	% XXX this assumes strings contain 8-bit characters
	%     Using write_bytes here is wrong; the output will depend
	%     on the Mercury implementation's representation of chars,
	%     so it may be different for different Mercury implementations.
	%     In particular, it will do the wrong thing for Mercury
	%     implementations which represent characters in Unicode.
	io__write_bytes(Val, !IO),
	io__write_byte(0, !IO).

string_to_byte_list(Val, List) :-
	% XXX this assumes strings contain 8-bit characters
	%     Using char__to_int here is wrong; the output will depend
	%     on the Mercury implementation's representation of chars,
	%     so it may be different for different Mercury implementations.
	%     In particular, it will do the wrong thing for Mercury
	%     implementations which represent characters in Unicode.
	string__to_char_list(Val, Chars),
	ToInt = (pred(C::in, I::out) is det :- char__to_int(C, I)),
	list__map(ToInt, Chars, List0),
	list__append(List0, [0], List).

output_byte(Val, !IO) :-
	( Val < 256 ->
		io__write_byte(Val, !IO)
	;
		error("byte does not fit in eight bits")
	).

output_short(Val, !IO) :-
	output_int(16, Val, !IO).

short_to_byte_list(Val, Bytes) :-
	int_to_byte_list(16, Val, Bytes).

output_int32(IntVal, !IO) :-
	output_int(32, IntVal, !IO).

int32_to_byte_list(IntVal, List) :-
	int_to_byte_list(32, IntVal, List).

output_int(IntVal, !IO) :-
	int__bits_per_int(IntBits),
	( IntBits > bytecode_int_bits ->
		error("size of int is larger than size of bytecode integer.")
	;
		output_int(bytecode_int_bits, IntVal, !IO)
	).

int_to_byte_list(IntVal, Bytes) :-
	int__bits_per_int(IntBits),
	( IntBits > bytecode_int_bits ->
		error("size of int is larger than size of bytecode integer.")
	;
		int_to_byte_list(bytecode_int_bits, IntVal, Bytes)
	).

:- pred output_int(int::in, int::in, io::di, io::uo) is det.

output_int(Bits, IntVal, !IO) :-
	output_int(io__write_byte, Bits, IntVal, !IO).

:- pred int_to_byte_list(int::in, int::in, list(int)::out) is det.

int_to_byte_list(Bits, IntVal, Bytes) :-
	output_int(cons, Bits, IntVal, [], RevBytes),
	list__reverse(RevBytes, Bytes).

:- pred cons(T::in, list(T)::in, list(T)::out) is det.

cons(T, List, [T | List]).

:- pred output_int(pred(int, T, T), int, int, T, T).
:- mode output_int(pred(in, in, out) is det, in, in, in, out) is det.
:- mode output_int(pred(in, di, uo) is det, in, in, di, uo) is det.

output_int(Writer, Bits, IntVal, !IO) :-
	int__bits_per_int(IntBits),
	( 
		Bits < IntBits,
		int__pow(2, Bits - 1, MaxVal),
		( IntVal >= MaxVal
		; IntVal < -MaxVal
		)
	->
		string__format(
			"error: bytecode_data__output_int: " ++
			"%d does not fit in %d bits",
			[i(IntVal), i(Bits)], Msg),
		error(Msg)
	;
		true
	),
	( Bits > IntBits ->
		ZeroPadBytes = (Bits - IntBits) // bits_per_byte
	;
		ZeroPadBytes = 0
	),
	output_padding_zeros(Writer, ZeroPadBytes, !IO),
	BytesToDump = Bits // bits_per_byte,
	FirstByteToDump = BytesToDump - ZeroPadBytes - 1,
	output_int_bytes(Writer, FirstByteToDump, IntVal, !IO).

:- func bytecode_int_bits = int.

bytecode_int_bits = bits_per_byte * bytecode_int_bytes.

:- func bytecode_int_bytes = int.

bytecode_int_bytes = 8.

:- func bits_per_byte = int.

bits_per_byte = 8.

:- pred output_padding_zeros(pred(int, T, T), int, T, T).
:- mode output_padding_zeros(pred(in, in, out) is det, in, in, out) is det.
:- mode output_padding_zeros(pred(in, di, uo) is det, in, di, uo) is det.

output_padding_zeros(Writer, NumBytes, !IO) :-
	( NumBytes > 0 ->
		call(Writer, 0, !IO),
		NumBytes1 = NumBytes - 1,
		output_padding_zeros(Writer, NumBytes1, !IO)
	;
		true
	).

:- pred output_int_bytes(pred(int, T, T), int, int, T, T).
:- mode output_int_bytes(pred(in, in, out) is det, in, in, in, out) is det.
:- mode output_int_bytes(pred(in, di, uo) is det, in, in, di, uo) is det.

output_int_bytes(Writer, ByteNum, IntVal, !IO) :-
	( ByteNum >= 0 ->
		BitShifts = ByteNum * bits_per_byte,
		Byte = (IntVal >> BitShifts) mod (1 << bits_per_byte),
		ByteNum1 = ByteNum - 1,
		call(Writer, Byte, !IO),
		output_int_bytes(Writer, ByteNum1, IntVal, !IO)
	;
		true
	).

output_float(Val, !IO) :-
	float_to_float64_bytes(Val, B0, B1, B2, B3, B4, B5, B6, B7),
	output_byte(B0, !IO),
	output_byte(B1, !IO),
	output_byte(B2, !IO),
	output_byte(B3, !IO),
	output_byte(B4, !IO),
	output_byte(B5, !IO),
	output_byte(B6, !IO),
	output_byte(B7, !IO).

float_to_byte_list(Val, [B0, B1, B2, B3, B4, B5, B6, B7]) :-
	float_to_float64_bytes(Val, B0, B1, B2, B3, B4, B5, B6, B7).

/*
** Convert a `float' to the representation used in the bytecode.
** That is, a sequence of eight bytes.
*/

:- pred float_to_float64_bytes(float::in, 
	int::out, int::out, int::out, int::out, 
	int::out, int::out, int::out, int::out) is det.

:- pragma c_code(
	float_to_float64_bytes(FloatVal::in, B0::out, B1::out, B2::out,
		B3::out, B4::out, B5::out, B6::out, B7::out),
	[will_not_call_mercury],
"
	{
		MR_Float64	float64;
		unsigned char	*raw_mem_p;

		float64 = (MR_Float64) FloatVal;
		raw_mem_p = (unsigned char*) &float64;

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
			#error	""Weird-endian architecture""
		#endif
	}
").

float_to_float64_bytes(_FloatVal, _B0, _B1, _B2, _B3, _B4, _B5, _B6, _B7) :-
	sorry(this_file, "float_to_float64_bytes for non-C target").

:- func this_file = string.

this_file = "bytecode_data.m".

:- end_module bytecode_data.

%---------------------------------------------------------------------------%
