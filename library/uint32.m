%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: uint32.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with unsigned 32-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module uint32.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Conversion from int.
%

    % from_int(I, U32):
    %
    % Convert an int into a uint32.
    % Fails if I is not in [0, 2^32 - 1].
    %
:- pred from_int(int::in, uint32::out) is semidet.

    % det_from_int(I) = U32:
    %
    % Convert an int into a uint32.
    % Throws an exception if I is not in [0, 2^32 - 1].
    %
:- func det_from_int(int) = uint32.

    % cast_from_int(I) = U32:
    %
    % Convert an int to a uint32.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [0, 2^32 - 1].
    %
:- func cast_from_int(int) = uint32.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % cast_to_int(U32) = I:
    %
    % Convert a uint32 to an int.
    % Always succeeds. If ints are 64 bits, I will always be
    % mathematically equal to U32. However, if ints are 32 bits,
    % then I will be mathematically equal to U32 only if
    % U32 is in [0, 2^31 - 1].
    %
:- func cast_to_int(uint32) = int.

%---------------------------------------------------------------------------%
%
% Conversion to uint.
%

    % cast_to_uint(U32) = U:
    %
    % Convert a uint32 to a uint.
    % Always succeeds, and always yields a result that is
    % mathematically equal to U32.
    %
:- func cast_to_uint(uint32) = uint.

    % cast_from_uint(U) = U32:
    %
    % Convert a uint to a uint32.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [0, 2^32 - 1].
    %
:- func cast_from_uint(uint) = uint32.

%---------------------------------------------------------------------------%
%
% Conversion to/from uint64.
%

    % cast_to_uint64(U32) = U64:
    %
    % Convert a uint32 to a uint64.
    % Always succeeds, and always yields a result that is
    % mathematically equal to U32.
    %
:- func cast_to_uint64(uint32) = uint64.

    % cast_from_uint64(U64) = U32:
    %
    % Convert a uint64 to a uint32.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [0, 2^32 - 1].
    %
:- func cast_from_uint64(uint64) = uint32.

%---------------------------------------------------------------------------%
%
% Change of signedness.
%

    % cast_from_int32(I32) = U32:
    %
    % Convert an int32 to a uint32. This will yield a result that is
    % mathematically equal to I32 only if I32 is in [0, 2^31 - 1].
    %
:- func cast_from_int32(int32) = uint32.

%---------------------------------------------------------------------------%
%
% Conversion from byte sequence.
%

    % from_bytes_le(Byte0, Byte1, Byte2, Byte3) = U32:
    %
    % U32 is the uint32 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte3 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8) = uint32.

    % from_bytes_be(Byte0, Byte1, Byte2, Byte3) = U32:
    %
    % U32 is the uint32 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte3 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8) = uint32.

%---------------------------------------------------------------------------%
%
% Comparisons and related operations.
%

    % Less than.
    %
:- pred (uint32::in) < (uint32::in) is semidet.

    % Greater than.
    %
:- pred (uint32::in) > (uint32::in) is semidet.

    % Less than or equal.
    %
:- pred (uint32::in) =< (uint32::in) is semidet.

    % Greater than or equal.
    %
:- pred (uint32::in) >= (uint32::in) is semidet.

    % Maximum.
    %
:- func max(uint32, uint32) = uint32.

    % Minimum.
    %
:- func min(uint32, uint32) = uint32.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

    % Addition.
    %
:- func uint32 + uint32 = uint32.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(uint32, uint32) = uint32.

    % Subtraction.
    %
:- func uint32 - uint32 = uint32.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(uint32, uint32) = uint32.

    % Multiplication.
    %
:- func (uint32::in) * (uint32::in) = (uint32::uo) is det.
:- func times(uint32, uint32) = uint32.

    % Truncating integer division.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint32::in) div (uint32::in) = (uint32::uo) is det.

    % Truncating integer division.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint32::in) // (uint32::in) = (uint32::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (uint32::in) / (uint32::in) = (uint32::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(uint32::in, uint32::in) = (uint32::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint32::in) mod (uint32::in) = (uint32::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `domain_error/` exception if the right operand is zero.
    %
:- func (uint32::in) rem (uint32::in) = (uint32::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint32::in, uint32::in) = (uint32::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint32::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint32::in) is semidet.

%---------------------------------------------------------------------------%
%
% Shift operations.
%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (uint32::in) << (int::in) = (uint32::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 32).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint32::in, int::in) = (uint32::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (uint32::in) >> (int::in) = (uint32::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 32).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint32::in, int::in) = (uint32::uo) is det.

%---------------------------------------------------------------------------%
%
% Logical operations.
%

    % Bitwise and.
    %
:- func (uint32::in) /\ (uint32::in) = (uint32::uo) is det.

    % Bitwise or.
    %
:- func (uint32::in) \/ (uint32::in) = (uint32::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(uint32, uint32) = uint32.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (uint32::in) = (uint32::uo) is det.

%---------------------------------------------------------------------------%
%
% Operations on bits and bytes.
%

    % num_zeros(U) = N:
    %
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint32) = int.

    % num_ones(U) = N:
    %
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint32) = int.

    % num_leading_zeros(U) = N:
    %
    % N is the number of leading zeros in the binary representation of U,
    % starting at the most significant bit position.
    % Note that num_leading_zeros(0u32) = 32.
    %
:- func num_leading_zeros(uint32) = int.

    % num_trailing_zeros(U) = N:
    %
    % N is the number of trailing zeros in the binary representation of U,
    % starting at the least significant bit position.
    % Note that num_trailing_zeros(0u32) = 32.
    %
:- func num_trailing_zeros(uint32) = int.

    % reverse_bytes(A) = B:
    %
    % B is the value that results from reversing the bytes in the binary
    % representation of A.
    %
:- func reverse_bytes(uint32) = uint32.

    % reverse_bits(A) = B:
    %
    % B is the is value that results from reversing the bits in the binary
    % representation of A.
    %
:- func reverse_bits(uint32) = uint32.

    % rotate_left(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % left by D bits. Throws an exception if D is not in the range [0, 31].
    %
:- func rotate_left(uint32, uint) = uint32.

    % unchecked_rotate_left(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % left by an amount given by the lowest 5 bits of D.
    %
:- func unchecked_rotate_left(uint32, uint) = uint32.

    % rotate_right(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % right by D bits. Throws an exception if D is not in the range [0, 31].
    %
:- func rotate_right(uint32, uint) = uint32.

    % unchecked_rotate_left(U, D) = N:
    %
    % N is the value obtained by rotating the binary representation of U
    % right by an amount given by the lowest 5 bits of D.
    %
:- func unchecked_rotate_right(uint32, uint) = uint32.

    % set_bit(U, I) = N:
    % N is the value obtained by setting the I'th bit (the bit worth 2^I) of U
    % to one. An exception is thrown if I is not in the range [0, 31].
    %
:- func set_bit(uint32, uint) = uint32.

    % unchecked_set_bit(U, I) = N:
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 31].
    %
:- func unchecked_set_bit(uint32, uint) = uint32.

    % clear_bit(U, I) = N:
    % N is the value obtained by setting the I'th bit (the bit worth 2^I) of U
    % to zero. An exception is thrown if I is not in the range [0, 31].
    %
:- func clear_bit(uint32, uint) = uint32.

    % unchecked_clear_bit(U, I) = N:
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 31].
    %
:- func unchecked_clear_bit(uint32, uint) = uint32.

    % flip_bit(U, I) = N:
    % N is the value obtained by flipping the I'th bit (the bit worth 2^I) of
    % U. An exception is thrown if I is not in the range [0, 31].
    %
:- func flip_bit(uint32, uint) = uint32.

    % unchecked_flip_bit(U, I) = N:
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 31].
    %
:- func unchecked_flip_bit(uint32, uint) = uint32.

    % bit_is_set(U, I):
    % True iff the I'th bit (the bit worth 2^I) of U is one.
    % An exception is thrown if I is not in the range [0, 31].
    %
:- pred bit_is_set(uint32::in, uint::in) is semidet.

    % unchecked_bit_is_set(U, I):
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 31].
    %
:- pred unchecked_bit_is_set(uint32::in, uint::in) is semidet.

    % bit_is_clear(U, I):
    % True iff the I'th bit (the bit worth 2^I) of U is zero.
    % An exception is thrown if I is not in the range [0, 31].
    %
:- pred bit_is_clear(uint32::in, uint::in) is semidet.

    % unchecked_bit_is_clear(U, I):
    % As above, but the behaviour is undefined if I is not in the range
    % [0, 31].
    %
:- pred unchecked_bit_is_clear(uint32::in, uint::in) is semidet.

%---------------------------------------------------------------------------%
%
% Limits.
%

:- func max_uint32 = uint32.

%---------------------------------------------------------------------------%
%
% Prettyprinting.
%

    % Convert a uint32 to a pretty_printer.doc for formatting.
    %
:- func uint32_to_doc(uint32) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int(I::in, U32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if ((uint64_t) I > (uint64_t) UINT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U32 = (uint32_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = (uint) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

det_from_int(I) = U :-
    ( if from_int(I, U0) then
        U = U0
    else
        error($pred, "cannot convert int to uint32")
    ).

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U32 = (uint32_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = (uint) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = I;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int(U32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = (MR_Integer) U32;
").

:- pragma foreign_proc("C#",
    cast_to_int(U32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) U32;
").

:- pragma foreign_proc("Java",
    cast_to_int(U32::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_uint(U32::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U = (MR_Unsigned) U32;
").

:- pragma foreign_proc("C#",
    cast_to_uint(U32::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U32;
").

:- pragma foreign_proc("Java",
    cast_to_uint(U32::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint(U::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U32 = (uint32_t) U;
").

:- pragma foreign_proc("C#",
    cast_from_uint(U::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = U;
").

:- pragma foreign_proc("Java",
    cast_from_uint(U::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = U;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_uint64(U32::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U64 = (uint64_t) U32;
").

:- pragma foreign_proc("C#",
    cast_to_uint64(U32::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) U32;
").

:- pragma foreign_proc("Java",
    cast_to_uint64(U32::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (long) U32 & 0xffffffffL;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint64(U64::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U32 = (uint32_t) U64;
").

:- pragma foreign_proc("C#",
    cast_from_uint64(U64::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = (uint) U64;
").

:- pragma foreign_proc("Java",
    cast_from_uint64(U64::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = (int) U64;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int32(I32::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U32 = (uint32_t) I32;
").

:- pragma foreign_proc("C#",
    cast_from_int32(I32::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = (uint) I32;
").

:- pragma foreign_proc("Java",
    cast_from_int32(I32::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = I32;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    unsigned char *uint32_bytes = (unsigned char *) &U32;
#if defined(MR_BIG_ENDIAN)
    uint32_bytes[0] = Byte3;
    uint32_bytes[1] = Byte2;
    uint32_bytes[2] = Byte1;
    uint32_bytes[3] = Byte0;
#else
    uint32_bytes[0] = Byte0;
    uint32_bytes[1] = Byte1;
    uint32_bytes[2] = Byte2;
    uint32_bytes[3] = Byte3;
#endif
").

:- pragma foreign_proc("Java",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 =
        (Byte3 & 0xff) << 24 |
        (Byte2 & 0xff) << 16 |
        (Byte1 & 0xff) << 8  |
        (Byte0 & 0xff);
").

:- pragma foreign_proc("C#",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in) = (U32::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = (uint) (Byte3 << 24 | Byte2 << 16 | Byte1 << 8 | Byte0);
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

% The operations +, -, plus, minus, *, and times are builtins.

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0u32 then
        throw(domain_error("uint32.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0u32 then
        throw(domain_error("uint32.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(even/1).
even(X) :-
    (X /\ 1u32) = 0u32.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1u32) \= 0u32.

%---------------------------------------------------------------------------%

% The operations unchecked_left_shift and unchecked_right_shift are builtins.

X << Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint32.(<<): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint32.(>>): second operand is out of range",
        throw(domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

% The algorithms in this section are adapted from chapter 5 of
% ``Hacker's Delight'' by Henry S. Warren, Jr.

num_zeros(U) = 32 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (defined(MR_GNUC) || defined(MR_CLANG)) && defined(MR_INT_IS_32_BIT)
    N = __builtin_popcount(U);
#else
    U = U - ((U >> 1) & UINT32_C(0x55555555));
    U = (U & UINT32_C(0x33333333)) + ((U >> 2) & UINT32_C(0x33333333));
    U = (U + (U >> 4)) & UINT32_C(0x0f0f0f0f);
    U = U + (U >> 8);
    U = U + (U >> 16);
    N = (MR_Integer) (U & UINT32_C(0x3f));
#endif
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U - ((U >> 1) & 0x55555555);
    U = (U & 0x33333333) + ((U >> 2) & 0x33333333);
    U = (U + (U >> 4)) & 0x0f0f0f0f;
    U = U + (U >> 8);
    U = U + (U >> 16);
    N = (int) (U & 0x3f);
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U);
").

%---------------------%

:- pragma foreign_proc("C",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 32;
    } else {
    #if (defined(MR_GNUC) || defined(MR_CLANG)) && defined(MR_INT_IS_32_BIT)
        // Note that __builtin_clz(0) is undefined.
        N = __builtin_clz(U);
    #else
        int32_t n = 1;
        if ((U >> 16) == 0) { n += 16; U <<= 16; }
        if ((U >> 24) == 0) { n += 8;  U <<= 8;  }
        if ((U >> 28) == 0) { n += 4;  U <<= 4;  }
        if ((U >> 30) == 0) { n += 2;  U <<= 2;  }
        N = n - (U >> 31);
    #endif
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 32;
    } else {
        int n = 1;
        if ((U >> 16) == 0) { n = n + 16; U = U << 16; }
        if ((U >> 24) == 0) { n = n + 8;  U = U << 8;  }
        if ((U >> 28) == 0) { n = n + 4;  U = U << 4;  }
        if ((U >> 30) == 0) { n = n + 2;  U = U << 2;  }
        N = n - (int) (U >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfLeadingZeros(U);
").

%---------------------%

:- pragma foreign_proc("C",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 32;
    } else {
    #if (defined(MR_GNUC) || defined(MR_CLANG)) && defined(MR_INT_IS_32_BIT)
        N = __builtin_ctz(U);
    #else
        int32_t     n = 31;
        uint32_t    y;
        y = U << 16; if (y != 0) { n -= 16; U = y; }
        y = U <<  8; if (y != 0) { n -= 8;  U = y; }
        y = U <<  4; if (y != 0) { n -= 4;  U = y; }
        y = U <<  2; if (y != 0) { n -= 2;  U = y; }
        y = U <<  1; if (y != 0) { n -= 1; }
        N = n;
    #endif
    }
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 32;
    } else {
        int     n = 31;
        uint    y;
        y = U << 16; if (y != 0) { n = n -16; U = y; }
        y = U <<  8; if (y != 0) { n = n - 8; U = y; }
        y = U <<  4; if (y != 0) { n = n - 4; U = y; }
        y = U <<  2; if (y != 0) { n = n - 2; U = y; }
        y = U <<  1; if (y != 0) { n = n - 1; }
        N = n;
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfTrailingZeros(U);
").

%---------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = MR_uint32_reverse_bytes(A);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverseBytes(A);
").

reverse_bytes(A) = B :-
    B = ((A /\ 0x_0000_00ff_u32) << 24) \/
        ((A /\ 0x_0000_ff00_u32) << 8)  \/
        ((A /\ 0x_00ff_0000_u32) >> 8)  \/
        ((A /\ 0x_ff00_0000_u32) >> 24).

%---------------------%

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    A = ((A & UINT32_C(0x55555555)) << 1) | ((A >> 1) & UINT32_C(0x55555555));
    A = ((A & UINT32_C(0x33333333)) << 2) | ((A >> 2) & UINT32_C(0x33333333));
    A = ((A & UINT32_C(0x0f0f0f0f)) << 4) | ((A >> 4) & UINT32_C(0x0f0f0f0f));
    A = (A << 24) | ((A & UINT32_C(0xff00)) << 8) |
                    ((A >> 8) & UINT32_C(0xff00)) | (A >> 24);
    B = A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    A = (A & 0x55555555) << 1 | (A >> 1) & 0x55555555;
    A = (A & 0x33333333) << 2 | (A >> 2) & 0x33333333;
    A = (A & 0x0f0f0f0f) << 4 | (A >> 4) & 0x0f0f0f0f;
    A = (A << 24) | ((A & 0xff00) << 8) | ((A >> 8) & 0xff00) | (A >> 24);
    B = A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverse(A);
").

%---------------------------------------------------------------------------%

rotate_left(X, N) =
    ( if N < 32u then
        unchecked_rotate_left(X, N)
    else
        func_error($pred, "rotate amount exceeds 31 bits")
    ).

:- pragma foreign_proc("C",
    unchecked_rotate_left(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 31;
    // This implementation is from https://blog.regehr.org/archives/1063.
    // It is intended to avoid undefined behaviour in C and be recognisable by
    // C compilers as a rotate operation. (On architectures that have a rotate
    // instruction, some C compilers can recognise this formulation and replace
    // it with the appropriate machine instruction.)
    // XXX clang has intrinsics for rotation -- we should use those instead.
    Result = (X << N) | (X >> (-N & 31));
").

:- pragma foreign_proc("C#",
    unchecked_rotate_left(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 31;
    Result = (X << (int) N) | (X >> (int) (-N & 31));
").

:- pragma foreign_proc("Java",
    unchecked_rotate_left(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = java.lang.Integer.rotateLeft(X, N);
").

%---------------------------------------------------------------------------%

rotate_right(X, N) =
    ( if N < 32u then
        unchecked_rotate_right(X, N)
    else
        func_error($pred, "rotate amount exceeds 31 bits")
    ).

:- pragma foreign_proc("C",
    unchecked_rotate_right(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 31;
    Result = (X >> N) | (X << (-N & 31));
").

:- pragma foreign_proc("C#",
    unchecked_rotate_right(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N &= 31;
    Result = (X >> (int) N) | (X << (int) (-N & 31));
").

:- pragma foreign_proc("Java",
    unchecked_rotate_right(X::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = java.lang.Integer.rotateRight(X, N);
").

%---------------------------------------------------------------------------%

set_bit(U, I) =
    ( if I < 32u then
        unchecked_set_bit(U, I)
    else
        func_error($pred, "bit index exceeds 31 bits")
    ).

unchecked_set_bit(U, I) =
    U \/ (1u32 `unchecked_left_shift` cast_to_int(I)).

clear_bit(U, I) =
    ( if I < 32u then
        unchecked_clear_bit(U, I)
    else
        func_error($pred, "bit index exceeds 31 bits")
    ).

unchecked_clear_bit(U, I) =
    U /\ (\ (1u32 `unchecked_left_shift` cast_to_int(I))).

flip_bit(U, I) =
    ( if I < 32u then
        unchecked_flip_bit(U, I)
    else
        func_error($pred, "bit index exceeds 31 bits")
    ).

unchecked_flip_bit(U, I) =
    U `xor` (1u32 `unchecked_left_shift` cast_to_int(I)).

bit_is_set(U, I) :-
    ( if I < 32u then
        unchecked_bit_is_set(U, I)
    else
        error($pred, "bit index exceeds 31 bits")
    ).

unchecked_bit_is_set(U, I) :-
    U /\ (1u32 `unchecked_left_shift` cast_to_int(I)) \= 0u32.

bit_is_clear(U, I) :-
    ( if I < 32u then
        unchecked_bit_is_clear(U, I)
    else
        error($pred, "bit index exceeds 31 bits")
    ).

unchecked_bit_is_clear(U, I) :-
    U /\ (1u32 `unchecked_left_shift` cast_to_int(I)) = 0u32.

%---------------------------------------------------------------------------%

max_uint32 = 4_294_967_295_u32.

%---------------------------------------------------------------------------%

uint32_to_doc(X) = str(string.uint32_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint32.
%---------------------------------------------------------------------------%
