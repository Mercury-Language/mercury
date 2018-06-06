%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2018 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: uint16.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with unsigned 16-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module uint16.
:- interface.

:- import_module pretty_printer.

%--------------------------------------------------------------------------%
%
% Conversion from int.
%

    % from_int(I, U16):
    %
    % Convert an int into a uint16.
    % Fails if I is not in [0, 2^15 - 1].
    %
:- pred from_int(int::in, uint16::out) is semidet.

    % from_int(I, U16):
    %
    % Convert an int into a uint16.
    % Throws an exception if I is not in [0, 2^15 - 1].
    %
:- func det_from_int(int) = uint16.

    % cast_from_int(I) = U16:
    %
    % Convert an int to a uint16.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [0, 2^15 - 1].
    %
:- func cast_from_int(int) = uint16.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % to_int(U16) = I:
    %
    % Convert a uint16 to an int.
    % Always succeeds, and yields a result that is mathematically equal
    % to U16.
    %
:- func to_int(uint16) = int.

    % cast_to_int(U16) = I:
    %
    % Convert a uint16 to an int.
    % Always succeeds, and yields a result that is mathematically equal
    % to U16.
    %
:- func cast_to_int(uint16) = int.

%---------------------------------------------------------------------------%
%
% Conversion to uint.
%

    % cast_to_uint(U16) = U:
    %
    % Convert a uint16 to a uint.
    % Always succeeds, and yields a result that is mathematically equal
    % to U16.
    %
:- func cast_to_uint(uint16) = uint.

%---------------------------------------------------------------------------%
%
% Change of signedness.
%

    % cast_from_int16(I16) = U16:
    %
    % Convert an int16 to a uint16. This will yield a result that is
    % mathematically equal to I16 only if I16 is in [0, 2^15 -1].
    %
:- func cast_from_int16(int16) = uint16.

%---------------------------------------------------------------------------%
%
% Conversion from byte sequence.
%

    % from_bytes_le(LSB, MSB) = U16:
    %
    % U16 is the uint16 whose least and most significant bytes are given by the
    % uint8s LSB and MSB respectively.
    %
:- func from_bytes_le(uint8, uint8) = uint16.

    % from_bytes_be(MSB, LSB) = U16:
    %
    % U16 is the uint16 whose least and most significant bytes are given by the
    % uint8s LSB and MSB respectively.
    %
:- func from_bytes_be(uint8, uint8) = uint16.

%--------------------------------------------------------------------------%
%
% Comparisons and related operations.
%

    % Less than.
    %
:- pred (uint16::in) < (uint16::in) is semidet.

    % Greater than.
    %
:- pred (uint16::in) > (uint16::in) is semidet.

    % Less than or equal.
    %
:- pred (uint16::in) =< (uint16::in) is semidet.

    % Greater than or equal.
    %
:- pred (uint16::in) >= (uint16::in) is semidet.

    % Maximum.
    %
:- func max(uint16, uint16) = uint16.

    % Minimum.
    %
:- func min(uint16, uint16) = uint16.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

    % Addition.
    %
:- func uint16 + uint16 = uint16.
:- mode in + in = uo is det.
:- mode uo + in = in is det.
:- mode in + uo = in is det.

:- func plus(uint16, uint16) = uint16.

    % Subtraction.
    %
:- func uint16 - uint16 = uint16.
:- mode in - in = uo is det.
:- mode uo - in = in is det.
:- mode in - uo = in is det.

:- func minus(uint16, uint16) = uint16.

    % Multiplication.
    %
:- func (uint16::in) * (uint16::in) = (uint16::uo) is det.
:- func times(uint16, uint16) = uint16.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint16::in) div (uint16::in) = (uint16::uo) is det.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint16::in) // (uint16::in) = (uint16::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (uint16::in) / (uint16::in) = (uint16::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(uint16::in, uint16::in) = (uint16::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint16::in) mod (uint16::in) = (uint16::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (uint16::in) rem (uint16::in) = (uint16::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint16::in, uint16::in) = (uint16::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint16::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint16::in) is semidet.

%---------------------------------------------------------------------------%
%
% Shift operations.
%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (uint16::in) << (int::in) = (uint16::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint16::in, int::in) = (uint16::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (uint16::in) >> (int::in) = (uint16::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint16::in, int::in) = (uint16::uo) is det.

%---------------------------------------------------------------------------%
%
% Logical operations.
%

    % Bitwise and.
    %
:- func (uint16::in) /\ (uint16::in) = (uint16::uo) is det.

    % Bitwise or.
    %
:- func (uint16::in) \/ (uint16::in) = (uint16::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(uint16, uint16) = uint16.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (uint16::in) = (uint16::uo) is det.

%---------------------------------------------------------------------------%
%
% Operations on bits and bytes.
%

    % num_zeros(U) = N:
    %
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint16) = int.

    % num_ones(U) = N:
    %
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint16) = int.

    % num_leading_zeros(U) = N:
    %
    % N is the number of leading zeros in the binary representation of U,
    % starting at the most significant bit position.
    % Note that num_leading_zeros(0u16) = 16.
    %
:- func num_leading_zeros(uint16) = int.

    % num_trailing_zeros(U) = N:
    %
    % N is the number of trailing zeros in the binary representation of U,
    % starting at the least significant bit position.
    % Note that num_trailing_zeros(0u16) = 16.
    %
:- func num_trailing_zeros(uint16) = int.

    % reverse_bytes(A) = B:
    %
    % B is the value that results from reversing the bytes in the binary
    % representation of A.
    %
:- func reverse_bytes(uint16) = uint16.

    % reverse_bits(A) = B:
    %
    % B is the is value that results from reversing the bits in the binary
    % representation of A.
    %
:- func reverse_bits(uint16) = uint16.

%---------------------------------------------------------------------------%
%
% Limits.
%

:- func max_uint16 = uint16.

%---------------------------------------------------------------------------%
%
% Prettyprinting.
%

    % Convert a uint16 to a pretty_printer.doc for formatting.
    %
:- func uint16_to_doc(uint16) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module math.
:- import_module require.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

from_int(I, U8) :-
    I >= 0,
    I =< 65_535,
    U8 = cast_from_int(I).

det_from_int(I) = U16 :-
    ( if from_int(I, U16Prime) then
        U16 = U16Prime
    else
        error("uint16.det_from_int: cannot convert int to uint16")
    ).

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U16 = (uint16_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (ushort) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (short) I;
").

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "uint16.cast_from_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = U16;
").

:- pragma foreign_proc("C#",
    to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U16;
").

:- pragma foreign_proc("Java",
    to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U16 & 0xffff;
").

:- pragma no_determinism_warning(to_int/1).
to_int(_) = _ :-
    sorry($module, "uint16.to_int/1 NYI for Erlang").

:- pragma foreign_proc("C",
    cast_to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = U16;
").

:- pragma foreign_proc("C#",
    cast_to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U16;
").

:- pragma foreign_proc("Java",
    cast_to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U16 & 0xffff;
").

:- pragma no_determinism_warning(cast_to_int/1).
cast_to_int(_) = _ :-
    sorry($module, "uint16.cast_to_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_uint(U16::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U = (MR_Unsigned) U16;
").

:- pragma foreign_proc("C#",
    cast_to_uint(U16::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) U16;
").

:- pragma foreign_proc("Java",
    cast_to_uint(U16::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX This is probably wrong.
    U = U16;
").

:- pragma no_determinism_warning(cast_to_uint/1).
cast_to_uint(_) = _ :-
    sorry($module, "uint16.cast_to_uint/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int16(I16::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U16 = (uint16_t) I16;
").

:- pragma foreign_proc("C#",
    cast_from_int16(I16::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (ushort) I16;
").

:- pragma foreign_proc("Java",
    cast_from_int16(I16::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = I16;
").

:- pragma no_determinism_warning(cast_from_int16/1).
cast_from_int16(_) = _ :-
    sorry($module, "uint16.cast_from_int16/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bytes_le(LSB::in, MSB::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    unsigned char *uint16_bytes = (unsigned char *) &U16;
#if defined(MR_BIG_ENDIAN)
    uint16_bytes[0] = MSB;
    uint16_bytes[1] = LSB;
#else
    uint16_bytes[0] = LSB;
    uint16_bytes[1] = MSB;
#endif
").

:- pragma foreign_proc("Java",
    from_bytes_le(LSB::in, MSB::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (short) (MSB << java.lang.Byte.SIZE | (LSB & 0x00ff));
").

:- pragma foreign_proc("C#",
    from_bytes_le(LSB::in, MSB::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (ushort) (MSB << 8 | (LSB & 0x00ff));
").

from_bytes_le(_, _) = _ :-
    sorry($module, "uint16.from_bytes_le/2 NYI for Erlang").

from_bytes_be(MSB, LSB) =
    from_bytes_le(LSB, MSB).

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
    ( if Y = 0u16 then
        throw(math.domain_error("uint16.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0u16 then
        throw(math.domain_error("uint16.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(even/1).
even(X) :-
    (X /\ 1u16) = 0u16.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1u16) \= 0u16.

%---------------------------------------------------------------------------%

% The operations unchecked_left_shift and unchecked_right_shift are builtins.

X << Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint16.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint16.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

% The algorithms in this section are adapted from chapter 5 of
% ``Hacker's Delight'' by Henry S. Warren, Jr.

num_zeros(U) = 16 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = (U & 0x5555) + ((U >> 1) & 0x5555);
    U = (U & 0x3333) + ((U >> 2) & 0x3333);
    U = (U & 0x0f0f) + ((U >> 4) & 0x0f0f);
    U = (U & 0x00ff) + ((U >> 8) & 0x00ff);
    N = U;
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (ushort) ((U & 0x5555) + ((U >> 1) & 0x5555));
    U = (ushort) ((U & 0x3333) + ((U >> 2) & 0x3333));
    U = (ushort) ((U & 0x0f0f) + ((U >> 4) & 0x0f0f));
    U = (ushort) ((U & 0x00ff) + ((U >> 8) & 0x00ff));
    N = (int) U;
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U << 16);
").

%---------------------%

:- pragma foreign_proc("C",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 16;
    } else {
        int n = 1;
        if ((U >> 8) == 0) { n = n + 8;   U = U << 8; }
        if ((U >> 12) == 0) { n = n + 4;  U = U << 4; }
        if ((U >> 14) == 0) { n = n + 2;  U = U << 2; }
        if ((U >> 15) == 0) { n = n + 1;  U = U << 1; }
        N = n - (int) (U >> 15);
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 16;
    } else {
        int n = 1;
        if ((U >> 8) == 0)  { n = n + 8; U = (ushort) (U << 8); }
        if ((U >> 12) == 0) { n = n + 4; U = (ushort) (U << 4); }
        if ((U >> 14) == 0) { n = n + 2; U = (ushort) (U << 2); }
        if ((U >> 15) == 0) { n = n + 1; U = (ushort) (U << 1); }
        N = n - (int) (U >> 15);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (I == 0) {
        N = 16;
    } else {
        N = java.lang.Integer.numberOfLeadingZeros(I << 16);
    }
").

%---------------------%

num_trailing_zeros(U) =
    16 - num_leading_zeros(\ U /\ (U - 1u16)).

%---------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = MR_uint16_reverse_bytes(A);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Short.reverseBytes(A);
").

reverse_bytes(A) = B :-
    B = (A >> 8) \/ (A << 8).

%---------------------%

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) (java.lang.Integer.reverse(A << 16) & 0xffff);
").

reverse_bits(!.A) = B :-
    !:A = (((\ 0x5555u16) /\ !.A) >> 1) \/ ((0x5555u16 /\ !.A) << 1),
    !:A = (((\ 0x3333u16) /\ !.A) >> 2) \/ ((0x3333u16 /\ !.A) << 2),
    !:A = (((\ 0x0f0fu16) /\ !.A) >> 4) \/ ((0x0f0fu16 /\ !.A) << 4),
    !:A = (((\ 0x00ffu16) /\ !.A) >> 8) \/ ((0x00ffu16 /\ !.A) << 8),
    B = !.A.

%---------------------------------------------------------------------------%

max_uint16 = 65_535_u16.

%---------------------------------------------------------------------------%

uint16_to_doc(X) = str(string.uint16_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint16.
%---------------------------------------------------------------------------%
