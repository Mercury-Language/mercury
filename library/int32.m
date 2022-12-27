%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: int32.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with signed 32-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module int32.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Conversion from int.
%

    % from_int(I, I32):
    %
    % Convert an int to an int32.
    % Fails if I is not in [-(2^31), 2^31 - 1].
    %
:- pred from_int(int::in, int32::out) is semidet.

    % det_from_int(I) = I32:
    %
    % Convert an int to an int32.
    % Throws an exception if I is not in [-(2^31), 2^31 - 1].
    %
:- func det_from_int(int) = int32.

    % cast_from_int(I) = I32:
    %
    % Convert an int to an int32.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [-(2^31), 2^31 - 1].
    %
:- func cast_from_int(int) = int32.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % to_int(I32) = I:
    %
    % Convert an int32 to an int. Since an int can be only 32 or 64 bits,
    % this is guaranteed to yield a result that is mathematically equal
    % to the original.
    %
:- func to_int(int32) = int.

    % cast_to_int(I32) = I:
    %
    % Convert an int32 to an int. Since an int can be only 32 or 64 bits,
    % this is guaranteed to yield a result that is mathematically equal
    % to the original.
    %
:- func cast_to_int(int32) = int.

%---------------------------------------------------------------------------%
%
% Conversion to/from int8.
%

    % cast_to_int8(I32) = I8:
    %
    % Convert an int32 to an int8.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I32 only if I32 is in [-(2^7), 2^7 - 1].
    %
:- func cast_to_int8(int32) = int8.

    % cast_from_int8(I8) = I32:
    %
    % Convert an int8 to a int32.
    % Always succeeds, and yields a result that is mathematically equal to I8.
    %
:- func cast_from_int8(int8) = int32.

%---------------------------------------------------------------------------%
%
% Conversion to/from int16.
%

    % cast_to_int16(I32) = I16:
    %
    % Convert an int32 to an int16.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I32 only if I32 is in [-(2^15), 2^15 - 1].
    %
:- func cast_to_int16(int32) = int16.

    % cast_from_int16(I16) = I32:
    %
    % Convert an int16 to a int32.
    % Always succeeds, and yields a result that is mathematically equal to I16.
    %
:- func cast_from_int16(int16) = int32.

%---------------------------------------------------------------------------%
%
% Conversion to/from int64.
%

    % cast_to_int64(I32) = I64:
    %
    % Convert an int32 to an int64.
    % Always succeeds, and always yields a result that is
    % mathematically equal to I32.
    %
:- func cast_to_int64(int32) = int64.

    % cast_from_int64(I64) = I32:
    %
    % Convert an int64 to a int32.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I64 only if I64 is in [-(2^31), 2^31 - 1].
    %
:- func cast_from_int64(int64) = int32.

%---------------------------------------------------------------------------%
%
% Change of signedness.
%

    % cast_from_uint32(U32) = I32:
    %
    % Convert a uint32 to an int32. This will yield a result that is
    % mathematically equal to U32 only if U32 is in [0, 2^31 - 1].
    %
:- func cast_from_uint32(uint32) = int32.

%---------------------------------------------------------------------------%
%
% Conversion from byte sequence.
%

    % from_bytes_le(Byte0, Byte1, Byte2, Byte3) = I32:
    %
    % I32 is the int32 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte3 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8) = int32.

    % from_bytes_be(Byte0, Byte1, Byte2, Byte3) = I32:
    %
    % I32 is the int32 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte3 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8) = int32.

%---------------------------------------------------------------------------%
%
% Comparisons and related operations.
%

    % Less than.
    %
:- pred (int32::in) < (int32::in) is semidet.

    % Greater than.
    %
:- pred (int32::in) > (int32::in) is semidet.

    % Less than or equal.
    %
:- pred (int32::in) =< (int32::in) is semidet.

    % Greater than or equal.
    %
:- pred (int32::in) >= (int32::in) is semidet.

    % Maximum.
    %
:- func max(int32, int32) = int32.

    % Minimum.
    %
:- func min(int32, int32) = int32.

%---------------------------------------------------------------------------%
%
% Absolute values.
%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int32.min_int32.
    %
:- func abs(int32) = int32.

    % unchecked_abs(X) returns the absolute value of X, except that the result
    % is undefined if X = int32.min_int32.
    %
:- func unchecked_abs(int32) = int32.

    % nabs(X) returns the negative of the absolute value of X.
    % Unlike abs/1 this function is defined for X = int32.min_int32.
    %
:- func nabs(int32) = int32.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

    % Unary plus.
    %
:- func + (int32::in) = (int32::uo) is det.

    % Unary minus.
    %
:- func - (int32::in) = (int32::uo) is det.

    % Addition.
    %
:- func int32 + int32 = int32.
:- mode in + in = uo is det.
:- mode uo + in = in is det.
:- mode in + uo = in is det.

:- func plus(int32, int32) = int32.

    % Subtraction.
    %
:- func int32 - int32 = int32.
:- mode in - in = uo is det.
:- mode uo - in = in is det.
:- mode in - uo = in is det.

:- func minus(int32, int32) = int32.

    % Multiplication.
    %
:- func (int32::in) * (int32::in) = (int32::uo) is det.
:- func times(int32, int32) = int32.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i32) div 3_i32 = (-4_i32).
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (int32::in) div (int32::in) = (int32::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i32) // 3_i32 = (-3_i32).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (int32::in) // (int32::in) = (int32::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (int32::in) / (int32::in) = (int32::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int32::in, int32::in) = (int32::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (int32::in) mod (int32::in) = (int32::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `domain_error/` exception if the right operand is zero.
    %
:- func (int32::in) rem (int32::in) = (int32::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int32::in, int32::in) = (int32::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(int32::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(int32::in) is semidet.

%---------------------------------------------------------------------------%
%
% Shift operations.
%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (int32::in) << (int::in) = (int32::uo) is det.
:- func (int32::in) <<u (uint::in) = (int32::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 32).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int32::in, int::in) = (int32::uo) is det.
:- func unchecked_left_ushift(int32::in, uint::in) = (int32::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (int32::in) >> (int::in) = (int32::uo) is det.
:- func (int32::in) >>u (uint::in) = (int32::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_int32).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int32::in, int::in) = (int32::uo) is det.
:- func unchecked_right_ushift(int32::in, uint::in) = (int32::uo) is det.

%---------------------------------------------------------------------------%
%
% Logical operations.
%

    % Bitwise and.
    %
:- func (int32::in) /\ (int32::in) = (int32::uo) is det.

    % Bitwise or.
    %
:- func (int32::in) \/ (int32::in) = (int32::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int32, int32) = int32.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int32::in) = (int32::uo) is det.

%---------------------------------------------------------------------------%
%
% Operations on bits and bytes.
%

    % num_zeros(I) = N:
    %
    % N is the number of zeros in the binary representation of I.
    %
:- func num_zeros(int32) = int.

    % num_ones(I) = N:
    %
    % N is the number of ones in the binary representation of I.
    %
:- func num_ones(int32) = int.

    % num_leading_zeros(I) = N:
    %
    % N is the number of leading zeros in the binary representation of I,
    % starting at the most significant bit position.
    % Note that num_leading_zeros(0i32) = 32.
    %
:- func num_leading_zeros(int32) = int.

    % num_trailing_zeros(I) = N:
    %
    % N is the number of trailing zeros in the binary representation of I,
    % starting at the least significant bit position.
    % Note that num_trailing_zeros(0i32) = 32.
    %
:- func num_trailing_zeros(int32) = int.

    % reverse_bytes(A) = B:
    %
    % B is the value that results from reversing the bytes in the binary
    % representation of A.
    %
:- func reverse_bytes(int32) = int32.

    % reverse_bits(A) = B:
    %
    % B is the is value that results from reversing the bits in the binary
    % representation of A.
    %
:- func reverse_bits(int32) = int32.

%---------------------------------------------------------------------------%
%
% Limits.
%

:- func min_int32 = int32.

:- func max_int32 = int32.

%---------------------------------------------------------------------------%
%
% Prettyprinting.
%

    % Convert an int32 to a pretty_printer.doc for formatting.
    %
:- func int32_to_doc(int32) = pretty_printer.doc.
:- pragma obsolete(func(int32_to_doc/1), [pretty_printer.int32_to_doc/1]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module uint.
:- import_module uint32.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int(I::in, I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I > (MR_Integer) INT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (I < (MR_Integer) INT32_MIN) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        I32 = (int32_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I; // Mercury's 'int' type in the C# grade is 32-bits.
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I; // Mercury's 'int' type in the Java grade is 32-bits.
    SUCCESS_INDICATOR = true;
").

det_from_int(I) = I32 :-
    ( if from_int(I, I32Prime) then
        I32 = I32Prime
    else
        error($pred, "cannot convert int to int32")
    ).

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I32 = (int32_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(I32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = I32;
").

:- pragma foreign_proc("C#",
    to_int(I32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I32;
").

:- pragma foreign_proc("Java",
    to_int(I32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I32;
").

:- pragma foreign_proc("C",
    cast_to_int(I32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = I32;
").

:- pragma foreign_proc("C#",
    cast_to_int(I32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I32;
").

:- pragma foreign_proc("Java",
    cast_to_int(I32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int8(I32::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I8 = (int8_t) I32;
").

:- pragma foreign_proc("C#",
    cast_to_int8(I32::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I8 = (sbyte) I32;
").

:- pragma foreign_proc("Java",
    cast_to_int8(I32::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I8 = (byte) I32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int8(I8::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I32 = I8;
").

:- pragma foreign_proc("C#",
    cast_from_int8(I8::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I8;
").

:- pragma foreign_proc("Java",
    cast_from_int8(I8::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I8;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int16(I32::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I16 = (int16_t) I32;
").

:- pragma foreign_proc("C#",
    cast_to_int16(I32::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) I32;
").

:- pragma foreign_proc("Java",
    cast_to_int16(I32::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) I32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int16(I16::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I32 = I16;
").

:- pragma foreign_proc("C#",
    cast_from_int16(I16::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I16;
").

:- pragma foreign_proc("Java",
    cast_from_int16(I16::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = I16;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int64(I32::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I64 = (int64_t) I32;
").

:- pragma foreign_proc("C#",
    cast_to_int64(I32::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) I32;
").

:- pragma foreign_proc("Java",
    cast_to_int64(I32::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) I32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int64(I64::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I32 = (int32_t) I64;
").

:- pragma foreign_proc("C#",
    cast_from_int64(I64::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = (int) I64;
").

:- pragma foreign_proc("Java",
    cast_from_int64(I64::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = (int) I64;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint32(U32::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I32 = U32;
").

:- pragma foreign_proc("C#",
    cast_from_uint32(U32::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = (int) U32;
").

:- pragma foreign_proc("Java",
    cast_from_uint32(U32::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = U32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    unsigned char *int32_bytes = (unsigned char *) &I32;
#if defined(MR_BIG_ENDIAN)
    int32_bytes[0] = Byte3;
    int32_bytes[1] = Byte2;
    int32_bytes[2] = Byte1;
    int32_bytes[3] = Byte0;
#else
    int32_bytes[0] = Byte0;
    int32_bytes[1] = Byte1;
    int32_bytes[2] = Byte2;
    int32_bytes[3] = Byte3;
#endif
").

:- pragma foreign_proc("Java",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 =
        (Byte3 & 0xff) << 24 |
        (Byte2 & 0xff) << 16 |
        (Byte1 & 0xff) << 8  |
        (Byte0 & 0xff);
").

:- pragma foreign_proc("C#",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in) = (I32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I32 = (Byte3 << 24 | Byte2 << 16 | Byte1 << 8 | Byte0);
").

from_bytes_be(Byte3, Byte2, Byte1, Byte0) =
    from_bytes_le(Byte0, Byte1, Byte2, Byte3).

%---------------------------------------------------------------------------%

% The comparison operations <, >, =< and >= are builtins.

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

abs(Num) =
    ( if Num = int32.min_int32 then
        func_error($pred, "abs(min_int32) would overflow")
    else
        unchecked_abs(Num)
    ).

unchecked_abs(Num) =
    ( if Num < 0i32 then
        0i32 - Num
    else
        Num
    ).

nabs(Num) =
    ( if Num > 0i32 then
        -Num
    else
        Num
    ).

%---------------------------------------------------------------------------%

% The operations + and - (both hand binary), plus, minus, *, and times
% are builtins.

X div Y = Div :-
    Trunc = X // Y,
    ( if
        ( X >= 0i32, Y >= 0i32
        ; X < 0i32, Y < 0i32
        ; X rem Y = 0i32
        )
    then
        Div = Trunc
    else
        Div = Trunc - 1i32
    ).

:- pragma inline(func('//'/2)).
X // Y = Div :-
    ( if Y = 0i32 then
        throw(domain_error("int32.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline(func('/'/2)).
X / Y = X // Y.

X mod Y = X  - (X div Y) * Y.

:- pragma inline(func(rem/2)).
X rem Y = Rem :-
    ( if Y = 0i32 then
        throw(domain_error("int32.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(pred(even/1)).
even(X) :-
    (X /\ 1i32) = 0i32.

:- pragma inline(pred(odd/1)).
odd(X) :-
    (X /\ 1i32) \= 0i32.

%---------------------------------------------------------------------------%

% The unchecked shift operations are builtins.

X << Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int32.(<<): second operand is out of range",
        throw(domain_error(Msg))
    ).

X <<u Y = Result :-
    ( if Y < 32u then
        Result = unchecked_left_ushift(X, Y)
    else
        Msg = "int32.(<<u): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int32.(>>): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >>u Y = Result :-
    ( if Y < 32u then
        Result = unchecked_right_ushift(X, Y)
    else
        Msg = "int32.(>>u): second operand is out of range",
        throw(domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

num_zeros(I) = 32 - num_ones(I).

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U);
").

num_ones(I32) = N :-
    U32 = uint32.cast_from_int32(I32),
    N = uint32.num_ones(U32).

%---------------------%

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfLeadingZeros(U);
").

num_leading_zeros(I32) = N :-
    U32 = uint32.cast_from_int32(I32),
    N = uint32.num_leading_zeros(U32).


:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfTrailingZeros(U);
").

num_trailing_zeros(I32) = N :-
    U32 = uint32.cast_from_int32(I32),
    N = uint32.num_trailing_zeros(U32).

%---------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = (int32_t) MR_uint32_reverse_bytes((uint32_t) A);
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint u_A = (uint) A;

    B = (int)
        ((u_A & 0x000000ffU) << 24 |
        (u_A & 0x0000ff00U) << 8   |
        (u_A & 0x00ff0000U) >> 8   |
        (u_A & 0xff000000U) >> 24);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverseBytes(A);
").

%---------------------%

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverse(A);
").

reverse_bits(I32) = RevI32 :-
    U32 = uint32.cast_from_int32(I32),
    RevU32 = uint32.reverse_bits(U32),
    RevI32 = int32.cast_from_uint32(RevU32).

%---------------------------------------------------------------------------%

min_int32 = -2_147_483_648_i32.

max_int32 = 2_147_483_647_i32.

%---------------------------------------------------------------------------%

int32_to_doc(I) = pretty_printer.int32_to_doc(I).

%---------------------------------------------------------------------------%
:- end_module int32.
%---------------------------------------------------------------------------%
