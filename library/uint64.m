%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2018-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: uint64.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with unsigned 64-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module uint64.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Conversion from int.
%

    % from_int(I, U64):
    %
    % Convert an int into a uint64.
    % Fails if I is not in [0, 2^64 - 1].
    %
:- pred from_int(int::in, uint64::out) is semidet.

    % det_from_int(I) = U64:
    %
    % Convert an int into a uint64.
    % Throws an exception if I is not in [0, 2^64 - 1].
    %
:- func det_from_int(int) = uint64.

    % cast_from_int(I) = U64:
    %
    % Convert an int to a uint64.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [0, 2^64 - 1].
    %
:- func cast_from_int(int) = uint64.

%---------------------------------------------------------------------------%
%
% Conversion from uint.
%

    % cast_from_uint(U) = U64:
    %
    % Convert a uint to a uint64.
    % Always succeeds, and will always yield a result that is
    % mathematically equal U.
    %
:- func cast_from_uint(uint) = uint64.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % cast_to_int(U64) = I:
    %
    % Convert a uint64 to an int.
    % Always succeeds. If ints are 64 bits, I will be mathematically
    % equal to U64 only if U64 is in [0, 2^63 - 1]. If ints are 32
    % bits, I will be mathematically equal to U64 only if U64 is in
    % [0, 2^31 - 1].
    %
:- func cast_to_int(uint64) = int.

%---------------------------------------------------------------------------%
%
% Conversion to uint.
%

    % cast_to_uint(U64) = U:
    %
    % Convert a uint64 to a uint.
    % Always succeeds, but will yield a result that is mathematically equal
    % to U64 only if uints are 64 bits.
    %
:- func cast_to_uint(uint64) = uint.

%---------------------------------------------------------------------------%
%
% Conversion to/from uint8
%

    % cast_to_uint8(U64) = U8:
    %
    % Convert a uint64 to a uint8.
    % Always succeeds, but will yield a result that is mathematically equal
    % to U64 only if U64 is in [0, 2^8 - 1].
    %
:- func cast_to_uint8(uint64) = uint8.

    % cast_from_uint8(U8) = U64:
    %
    % Convert a uint8 to a uint64.
    % Always succeeds, and yields a result that is mathemtically equal
    % to U8.
    %
:- func cast_from_uint8(uint8) = uint64.

%---------------------------------------------------------------------------%
%
% Change of signedness.
%

    % cast_from_int64(I64) = U64:
    %
    % Convert an int64 to a uint64. This will yield a result that is
    % mathematically equal to I64 only if I64 is in [0, 2^63 - 1].
    %
:- func cast_from_int64(int64) = uint64.

%---------------------------------------------------------------------------%
%
% Conversion from byte sequence.
%

    % from_bytes_le(Byte0, Byte1, ..., Byte7) = U64:
    %
    % U64 is the uint64 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte7 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8, uint8, uint8, uint8, uint8)
    = uint64.

    % from_bytes_be(Byte0, Byte1, ..., Byte7) = U64:
    %
    % U64 is the uint64 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte7 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8, uint8, uint8, uint8, uint8)
    = uint64.

%---------------------------------------------------------------------------%
%
% Comparisons and related operations.
%

    % Less than.
    %
:- pred (uint64::in) < (uint64::in) is semidet.

    % Greater than.
    %
:- pred (uint64::in) > (uint64::in) is semidet.

    % Less than or equal.
    %
:- pred (uint64::in) =< (uint64::in) is semidet.

    % Greater than or equal.
    %
:- pred (uint64::in) >= (uint64::in) is semidet.

    % Maximum.
    %
:- func max(uint64, uint64) = uint64.

    % Minimum.
    %
:- func min(uint64, uint64) = uint64.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

    % Addition.
    %
:- func uint64 + uint64 = uint64.
:- mode in + in = uo is det.
:- mode uo + in = in is det.
:- mode in + uo = in is det.

:- func plus(uint64, uint64) = uint64.

    % Subtraction.
    %
:- func uint64 - uint64 = uint64.
:- mode in - in = uo is det.
:- mode uo - in = in is det.
:- mode in - uo = in is det.

:- func minus(uint64, uint64) = uint64.

    % Multiplication.
    %
:- func (uint64::in) * (uint64::in) = (uint64::uo) is det.
:- func times(uint64, uint64) = uint64.

    % Truncating integer division.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint64::in) div (uint64::in) = (uint64::uo) is det.

    % Truncating integer division.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint64::in) // (uint64::in) = (uint64::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (uint64::in) / (uint64::in) = (uint64::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(uint64::in, uint64::in) = (uint64::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint64::in) mod (uint64::in) = (uint64::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `domain_error/` exception if the right operand is zero.
    %
:- func (uint64::in) rem (uint64::in) = (uint64::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint64::in, uint64::in) = (uint64::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint64::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint64::in) is semidet.

%---------------------------------------------------------------------------%
%
% Shift operations.
%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (uint64::in) << (int::in) = (uint64::uo) is det.
:- func (uint64::in) <<u (uint::in) = (uint64::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that
    % the behaviour is undefined if Y is not in [0, 64).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint64::in, int::in) = (uint64::uo) is det.
:- func unchecked_left_ushift(uint64::in, uint::in) = (uint64::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (uint64::in) >> (int::in) = (uint64::uo) is det.
:- func (uint64::in) >>u (uint::in) = (uint64::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that
    % the behaviour is undefined if Y is not in [0, 64).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint64::in, int::in) = (uint64::uo) is det.
:- func unchecked_right_ushift(uint64::in, uint::in) = (uint64::uo) is det.

%---------------------------------------------------------------------------%
%
% Logical operations.
%

    % Bitwise and.
    %
:- func (uint64::in) /\ (uint64::in) = (uint64::uo) is det.

    % Bitwise or.
    %
:- func (uint64::in) \/ (uint64::in) = (uint64::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(uint64, uint64) = uint64.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (uint64::in) = (uint64::uo) is det.

%---------------------------------------------------------------------------%
%
% Operations on bits and bytes.
%

    % num_zeros(U) = N:
    %
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint64) = int.

    % num_ones(U) = N:
    %
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint64) = int.

    % num_leading_zeros(U) = N:
    %
    % N is the number of leading zeros in the binary representation of U.
    %
:- func num_leading_zeros(uint64) = int.

    % num_trailing_zeros(U) = N:
    % N is the number of trailing zeros in the binary representation of U.
    %
:- func num_trailing_zeros(uint64) = int.

    % reverse_bytes(A) = B:
    %
    % B is the value that results from reversing the bytes in the binary
    % representation of A.
    %
:- func reverse_bytes(uint64) = uint64.

    % reverse_bits(A) = B:
    %
    % B is the is value that results from reversing the bits
    % in the binary representation of A.
    %
:- func reverse_bits(uint64) = uint64.

    % rotate_left(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % left by D bits. Throws an exception if D is not in the range [0, 63].
    %
:- func rotate_left(uint64, uint) = uint64.

    % unchecked_rotate_left(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % left by an amount given by the lowest 6 bits of D.
    %
:- func unchecked_rotate_left(uint64, uint) = uint64.

    % rotate_right(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % right by D bits. Throws an exception if D is not in the range [0, 63].
    %
:- func rotate_right(uint64, uint) = uint64.

    % unchecked_rotate_left(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % right by an amount given by the lowest 6 bits of D.
    %
:- func unchecked_rotate_right(uint64, uint) = uint64.

    % set_bit(U, I) = N:
    % N is the value obtained by setting the I'th bit (the bit worth 2^I) of U
    % to one. An exception is thrown if I is not in the range [0, 63].
    %
:- func set_bit(uint64, uint) = uint64.

    % unchecked_set_bit(U, I) = N:
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 63].
    %
:- func unchecked_set_bit(uint64, uint) = uint64.

    % clear_bit(U, I) = N:
    % N is the value obtained by setting the I'th bit (the bit worth 2^I) of U
    % to zero. An exception is thrown if I is not in the range [0, 63].
    %
:- func clear_bit(uint64, uint) = uint64.

    % unchecked_clear_bit(U, I) = N:
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 63].
    %
:- func unchecked_clear_bit(uint64, uint) = uint64.

    % flip_bit(U, I) = N:
    % N is the value obtained by flipping the I'th bit (the bit worth 2^I) of
    % U. An exception is thrown if I is not in the range [0, 63].
    %
:- func flip_bit(uint64, uint) = uint64.

    % unchecked_flip_bit(U, I) = N:
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 63].
    %
:- func unchecked_flip_bit(uint64, uint) = uint64.

    % bit_is_set(U, I):
    % True iff the I'th bit (the bit worth 2^I) of U is one.
    % An exception is thrown if I is not in the range [0, 63].
    %
:- pred bit_is_set(uint64::in, uint::in) is semidet.

    % unchecked_bit_is_set(U, I):
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 63].
    %
:- pred unchecked_bit_is_set(uint64::in, uint::in) is semidet.

    % bit_is_clear(U, I):
    % True iff the I'th bit (the bit worth 2^I) of U is zero.
    % An exception is thrown if I is not in the range [0, 63].
    %
:- pred bit_is_clear(uint64::in, uint::in) is semidet.

    % unchecked_bit_is_clear(U, I):
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 63].
    %
:- pred unchecked_bit_is_clear(uint64::in, uint::in) is semidet.

%---------------------------------------------------------------------------%
%
% Limits.
%

:- func max_uint64 = uint64.

%---------------------------------------------------------------------------%
%
% Prettyprinting.
%

    % Convert a uint64 to a pretty_printer.doc for formatting.
    %
:- func uint64_to_doc(uint64) = pretty_printer.doc.
:- pragma obsolete(func(uint64_to_doc/1), [pretty_printer.uint64_to_doc/1]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int(I::in, U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if ((uint64_t) I > (uint64_t) INT64_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U64 = (uint64_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

det_from_int(I) = U64 :-
    ( if from_int(I, U64Prime) then
        U64 = U64Prime
    else
        error($pred, "cannot convert int to uint64")
    ).

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U64 = (uint64_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = I;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint(U::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U64 = (uint64_t) U;
").

:- pragma foreign_proc("C#",
    cast_from_uint(U::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) U;
").

:- pragma foreign_proc("Java",
    cast_from_uint(U::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (long) U & 0xffffffffL;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int(U64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = (MR_Integer) U64;
").

:- pragma foreign_proc("C#",
    cast_to_int(U64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) U64;
").

:- pragma foreign_proc("Java",
    cast_to_int(U64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) U64;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_uint(U64::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U = (MR_Unsigned) U64;
").

:- pragma foreign_proc("C#",
    cast_to_uint(U64::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) U64;
").

:- pragma foreign_proc("Java",
    cast_to_uint(U64::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (int) U64;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_uint8(U64::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U8 = (uint8_t) U64;
").

:- pragma foreign_proc("C#",
    cast_to_uint8(U64::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = (byte) U64;
").

:- pragma foreign_proc("Java",
    cast_to_uint8(U64::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = (byte) U64;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint8(U8::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U64 = (uint64_t) U8;
").

:- pragma foreign_proc("C#",
    cast_from_uint8(U8::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) U8;
").

:- pragma foreign_proc("Java",
    cast_from_uint8(U8::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (long) (U8 & 0xff);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int64(I64::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U64 = (uint64_t) I64;
").

:- pragma foreign_proc("C#",
    cast_from_int64(I64::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) I64;
").

:- pragma foreign_proc("Java",
    cast_from_int64(I64::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = I64;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in,
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    unsigned char *uint64_bytes = (unsigned char *) &U64;
#if defined(MR_BIG_ENDIAN)
    uint64_bytes[0] = Byte7;
    uint64_bytes[1] = Byte6;
    uint64_bytes[2] = Byte5;
    uint64_bytes[3] = Byte4;
    uint64_bytes[4] = Byte3;
    uint64_bytes[5] = Byte2;
    uint64_bytes[6] = Byte1;
    uint64_bytes[7] = Byte0;
#else
    uint64_bytes[0] = Byte0;
    uint64_bytes[1] = Byte1;
    uint64_bytes[2] = Byte2;
    uint64_bytes[3] = Byte3;
    uint64_bytes[4] = Byte4;
    uint64_bytes[5] = Byte5;
    uint64_bytes[6] = Byte6;
    uint64_bytes[7] = Byte7;
#endif
").

:- pragma foreign_proc("Java",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in,
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 =
        (long) (Byte7 & 0xff) << 56 |
        (long) (Byte6 & 0xff) << 48 |
        (long) (Byte5 & 0xff) << 40 |
        (long) (Byte4 & 0xff) << 32 |
        (long) (Byte3 & 0xff) << 24 |
        (long) (Byte2 & 0xff) << 16 |
        (long) (Byte1 & 0xff) << 8  |
        (long) (Byte0 & 0xff);
").

:- pragma foreign_proc("C#",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in,
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) (
        (ulong) Byte7 << 56 |
        (ulong) Byte6 << 48 |
        (ulong) Byte5 << 40 |
        (ulong) Byte4 << 32 |
        (ulong) Byte3 << 24 |
        (ulong) Byte2 << 16 |
        (ulong) Byte1 << 8  |
        (ulong) Byte0);
").

from_bytes_be(Byte7, Byte6, Byte5,Byte4, Byte3, Byte2, Byte1, Byte0) =
    from_bytes_le(Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7).

%---------------------------------------------------------------------------%

% The comparison operations <, >, =< and >= are builtins.

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

% The operations +, -, plus, minus, *, and times are builtins.

X div Y = X // Y.

:- pragma inline(func('//'/2)).
X // Y = Div :-
    ( if Y = 0u64 then
        throw(domain_error("uint64.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline(func('/'/2)).
X / Y = X // Y.

% The operations unchecked_quotient and unchecked_rem are builtins.

X mod Y = X rem Y.

:- pragma inline(func(rem/2)).
X rem Y = Rem :-
    ( if Y = 0u64 then
        throw(domain_error("uint64.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(pred(even/1)).
even(X) :-
    (X /\ 1u64) = 0u64.

:- pragma inline(pred(odd/1)).
odd(X) :-
    (X /\ 1u64) \= 0u64.

%---------------------------------------------------------------------------%

% The unchecked shift operations are builtins.

X << Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint64.(<<): second operand is out of range",
        throw(domain_error(Msg))
    ).

X <<u Y = Result :-
    ( if Y < 64u then
        Result = unchecked_left_ushift(X, Y)
    else
        Msg = "uint64.(<<u): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint64.(>>): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >>u Y = Result :-
    ( if Y < 64u then
        Result = unchecked_right_ushift(X, Y)
    else
        Msg = "uint64.(>>u): second operand is out of range",
        throw(domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

% The algorithms in this section are adapted from chapter 5 of
% ``Hacker's Delight'' by Henry S. Warren, Jr.

num_zeros(U) = 64 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (defined(MR_GNUC) || defined(MR_CLANG)) && defined(MR_LONG_IS_64_BIT)
    N = __builtin_popcountl(U);
#else
    U = U - ((U >> 1) & UINT64_C(0x5555555555555555));
    U = (U & UINT64_C(0x3333333333333333)) +
        ((U >> 2) & UINT64_C(0x3333333333333333));
    U = (U + (U >> 4)) & UINT64_C(0x0f0f0f0f0f0f0f0f);
    U = U + (U >> 8);
    U = U + (U >> 16);
    U = U + (U >> 32);
    N = U & UINT64_C(0x7f);
#endif
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U - ((U >> 1) & 0x5555555555555555UL);
    U = (U & 0x3333333333333333UL) + ((U >> 2) & 0x3333333333333333UL);
    U = (U + (U >> 4)) & 0x0f0f0f0f0f0f0f0fUL;
    U = U + (U >> 8);
    U = U + (U >> 16);
    U = U + (U >> 32);
    N = (int) (U & 0x7fUL);
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.bitCount(U);
").

%---------------------%

:- pragma foreign_proc("C",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 64;
    } else {
        int32_t n = 1;
        uint32_t x = (uint32_t) (U >> 32);
        if (x == 0) { n += 32; x = (uint32_t) U; }
        if (x >> 16 == 0) { n += 16; x <<= 16; }
        if (x >> 24 == 0) { n +=  8; x <<=  8; }
        if (x >> 28 == 0) { n +=  4; x <<=  4; }
        if (x >> 30 == 0) { n +=  2; x <<=  2; }
        N = n - (x >> 31);
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 64;
    } else {
        int n = 1;
        uint x = (uint) (U >> 32);
        if (x == 0) { n += 32; x = (uint) U; }
        if (x >> 16 == 0) { n += 16; x <<= 16; }
        if (x >> 24 == 0) { n +=  8; x <<=  8; }
        if (x >> 28 == 0) { n +=  4; x <<=  4; }
        if (x >> 30 == 0) { n +=  2; x <<=  2; }
        N = n - (int) (x >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.numberOfLeadingZeros(U);
").

%---------------------%

:- pragma foreign_proc("C",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 64;
    } else {
        uint32_t x, y;
        int n = 63;
        y = (int32_t) U;
        if (y != 0) {
            n -=  32; x = y;
        } else {
            x = (uint32_t) (U >> 32);
        }
        y = x << 16; if (y != 0) { n -= 16; x = y; }
        y = x <<  8; if (y != 0) { n -=  8; x = y; }
        y = x <<  4; if (y != 0) { n -=  4; x = y; }
        y = x <<  2; if (y != 0) { n -=  2; x = y; }
        N = n - (int) ((x << 1) >> 31);
    }
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 64;
    } else {
        uint x, y;
        int n = 63;
        y = (uint) U;
        if (y != 0) {
            n = n - 32; x = y;
        } else {
            x = (uint) (U >> 32);
        }
        y = x << 16; if (y != 0) { n = n -16; x = y; }
        y = x <<  8; if (y != 0) { n = n - 8; x = y; }
        y = x <<  4; if (y != 0) { n = n - 4; x = y; }
        y = x <<  2; if (y != 0) { n = n - 2; x = y; }
        N = n - (int) ((x << 1) >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.numberOfTrailingZeros(U);
").

%---------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = MR_uint64_reverse_bytes(A);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Long.reverseBytes(A);
").

reverse_bytes(A) = B :-
    B =
        ((A /\ 0x_0000_0000_0000_00ff_u64) << 56) \/
        ((A /\ 0x_0000_0000_0000_ff00_u64) << 40) \/
        ((A /\ 0x_0000_0000_00ff_0000_u64) << 24) \/
        ((A /\ 0x_0000_0000_ff00_0000_u64) << 8)  \/
        ((A /\ 0x_0000_00ff_0000_0000_u64) >> 8)  \/
        ((A /\ 0x_0000_ff00_0000_0000_u64) >> 24) \/
        ((A /\ 0x_00ff_0000_0000_0000_u64) >> 40) \/
        ((A /\ 0x_ff00_0000_0000_0000_u64) >> 56).

%---------------------%

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Long.reverse(A);
").

reverse_bits(!.A) = B :-
    !:A = ((!.A /\ 0x_5555_5555_5555_5555_u64) << 1) \/
        ((!.A >> 1) /\ 0x_5555_5555_5555_5555_u64),
    !:A = ((!.A /\ 0x_3333_3333_3333_3333_u64) << 2) \/
        ((!.A >> 2) /\ 0x_3333_3333_3333_3333_u64),
    !:A = ((!.A /\ 0x_0f0f_0f0f_0f0f_0f0f_u64) << 4) \/
        ((!.A >> 4) /\ 0x_0f0f_0f0f_0f0f_0f0f_u64),
    !:A = ((!.A /\ 0x_00ff_00ff_00ff_00ff_u64) << 8) \/
        ((!.A >> 8) /\ 0x_00ff_00ff_00ff_00ff_u64),
    !:A = (!.A << 48) \/ ((!.A /\ 0x_ffff_0000_u64) << 16) \/
        ((!.A >> 16) /\ 0x_ffff_0000_u64) \/ (!.A >> 48),
    B = !.A.

%---------------------------------------------------------------------------%

rotate_left(X, N) =
    ( if N < 64u then
        unchecked_rotate_left(X, N)
    else
        func_error($pred, "rotate amount exceeds 63 bits")
    ).

:- pragma foreign_proc("C",
    unchecked_rotate_left(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 63;
    // XXX clang has intrinsics for rotation -- we should use those instead.
    Result = (X << N) | (X >> (-N & 63));
").

:- pragma foreign_proc("C#",
    unchecked_rotate_left(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 63;
    Result = (X << (int) N) | (X >> (int) (-N & 63));
").

:- pragma foreign_proc("Java",
    unchecked_rotate_left(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = java.lang.Long.rotateLeft(X, N);
").

%---------------------------------------------------------------------------%

rotate_right(X, N) =
    ( if N < 64u then
        unchecked_rotate_right(X, N)
    else
        func_error($pred, "rotate amount exceeds 63 bits")
    ).

:- pragma foreign_proc("C",
    unchecked_rotate_right(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 63;
    Result = (X >> N) | (X << (-N & 63));
").

:- pragma foreign_proc("C#",
    unchecked_rotate_right(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 63;
    Result = (X >> (int) N) | (X << (int) (-N & 63));
").

:- pragma foreign_proc("Java",
    unchecked_rotate_right(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = java.lang.Long.rotateRight(X, N);
").

%---------------------------------------------------------------------------%

set_bit(U, I) =
    ( if I < 64u then
        unchecked_set_bit(U, I)
    else
        func_error($pred, "bit index exceeds 63 bits")
    ).

unchecked_set_bit(U, I) =
    U \/ (1u64 `unchecked_left_shift` cast_to_int(I)).

clear_bit(U, I) =
    ( if I < 64u then
        unchecked_clear_bit(U, I)
    else
        func_error($pred, "bit index exceeds 63 bits")
    ).

unchecked_clear_bit(U, I) =
    U /\ (\ (1u64 `unchecked_left_shift` cast_to_int(I))).

flip_bit(U, I) =
    ( if I < 64u then
        unchecked_flip_bit(U, I)
    else
        func_error($pred, "bit index exceeds 63 bits")
    ).

unchecked_flip_bit(U, I) =
    U `xor` (1u64 `unchecked_left_shift` cast_to_int(I)).

bit_is_set(U, I) :-
    ( if I < 64u then
        unchecked_bit_is_set(U, I)
    else
        error($pred, "bit index exceeds 63 bits")
    ).

unchecked_bit_is_set(U, I) :-
    U /\ (1u64 `unchecked_left_shift` cast_to_int(I)) \= 0u64.

bit_is_clear(U, I) :-
    ( if I < 64u then
        unchecked_bit_is_clear(U, I)
    else
        error($pred, "bit index exceeds 63 bits")
    ).

unchecked_bit_is_clear(U, I) :-
    U /\ (1u64 `unchecked_left_shift` cast_to_int(I)) = 0u64.

%---------------------------------------------------------------------------%

max_uint64 = 18_446_744_073_709_551_615_u64.

%---------------------------------------------------------------------------%

uint64_to_doc(U) = pretty_printer.uint64_to_doc(U).

%---------------------------------------------------------------------------%
:- end_module uint64.
%---------------------------------------------------------------------------%
