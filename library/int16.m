%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2018 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: int16.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with signed 16-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module int16.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % from_int(I, I16):
    % Convert an int to an int16.
    % Fails if I is not in [-32768, 32767].
    %
:- pred from_int(int::in, int16::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = int16.

:- func cast_from_int(int) = int16.

:- func cast_from_uint16(uint16) = int16.

:- func to_int(int16) = int.

    % from_bytes_le(LSB, MSB) = I16:
    % I16 is the int16 whose least and most significant bytes are given by the
    % uint8s LSB and MSB respectively.
    %
:- func from_bytes_le(uint8, uint8) = int16.

    % from_bytes_be(MSB, LSB) = I16:
    % I16 is the int16 whose least and most significant bytes are given by the
    % uint8s LSB and MSB respectively.
    %
:- func from_bytes_be(uint8, uint8) = int16.

%---------------------------------------------------------------------------%

    % Less than.
    %
:- pred (int16::in) < (int16::in) is semidet.

    % Greater than.
    %
:- pred (int16::in) > (int16::in) is semidet.

    % Less than or equal.
    %
:- pred (int16::in) =< (int16::in) is semidet.

    % Greater than or equal.
    %
:- pred (int16::in) >= (int16::in) is semidet.

%---------------------------------------------------------------------------%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int16.min_int16.
    %
:- func abs(int16) = int16.

    % unchecked_abs(X) returns the absolute value of X, except that the result
    % is undefined if X = int16.min_int16.
    %
:- func unchecked_abs(int16) = int16.

    % nabs(X) returns the negative of the absolute value of X.
    % Unlike abs/1 this function is defined for X = int16.min_int16.
    %
:- func nabs(int16) = int16.

%---------------------------------------------------------------------------%

    % Maximum.
    %
:- func max(int16, int16) = int16.

    % Minimum.
    %
:- func min(int16, int16) = int16.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int16::in) = (int16::uo) is det.

    % Unary minus.
    %
:- func - (int16::in) = (int16::uo) is det.

    % Addition.
    %
:- func int16 + int16 = int16.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(int16, int16) = int16.

    % Subtraction.
    %
:- func int16 - int16 = int16.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(int16, int16) = int16.

    % Multiplication.
    %
:- func (int16::in) * (int16::in) = (int16::uo) is det.
:- func times(int16, int16) = int16.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i16) div 3_i16 = (-4_i16).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int16::in) div (int16::in) = (int16::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i16) // 3_i16 = (-3_i16).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int16::in) // (int16::in) = (int16::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (int16::in) / (int16::in) = (int16::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int16::in, int16::in) = (int16::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int16::in) mod (int16::in) = (int16::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (int16::in) rem (int16::in) = (int16::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int16::in, int16::in) = (int16::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (int16::in) << (int::in) = (int16::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int16::in, int::in) = (int16::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (int16::in) >> (int::in) = (int16::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int16::in, int::in) = (int16::uo) is det.

    % even(X) is equivalent to (X mod 2i16 = 0i16).
    %
:- pred even(int16::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2i16 = 1i16).
    %
:- pred odd(int16::in) is semidet.

    % Bitwise and.
    %
:- func (int16::in) /\ (int16::in) = (int16::uo) is det.

    % Bitwise or.
    %
:- func (int16::in) \/ (int16::in) = (int16::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int16, int16) = int16.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int16::in) = (int16::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(I) = N:
    % N is the number of zeros in the binary representation of I.
    %
:- func num_zeros(int16) = int.

    % num_ones(I) = N:
    % N is the number of ones in the binary representation of I.
    %
:- func num_ones(int16) = int.

    % num_leading_zeros(I) = N:
    % N is the number of leading zeros in the binary representation of I,
    % starting at the most significant bit position.
    % Note that num_leading_zeros(0i16) = 16.
    %
:- func num_leading_zeros(int16) = int.

    % num_trailing_zeros(I) = N:
    % N is the number of trailing zeros in the binary representation of I,
    % starting at the least significant bit position.
    % Note that num_trailing_zeros(0i16) = 16.
    %
:- func num_trailing_zeros(int16) = int.

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the binary
    % representation of A.
    %
:- func reverse_bytes(int16) = int16.

    % reverse_bits(A) = B:
    % B is the is value that results from reversing the bits in the binary
    % representation of A.
    %
:- func reverse_bits(int16) = int16.

%---------------------------------------------------------------------------%

:- func min_int16 = int16.

:- func max_int16 = int16.

    % Convert an int16 to a pretty_printer.doc for formatting.
    %
:- func int16_to_doc(int16) = pretty_printer.doc.

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

from_int(I, I16) :-
    I >= -32_768,
    I =< 32_767,
    I16 = cast_from_int(I).

det_from_int(I) = I16 :-
    ( if from_int(I, I16Prime) then
        I16 = I16Prime
    else
        error("int16.det_from_int: cannot convert int to int16")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I16 = (int16_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) I;
").

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "int16.cast_from_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint16(U16::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I16 = U16;
").

:- pragma foreign_proc("C#",
    cast_from_uint16(U16::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) U16;
").

:- pragma foreign_proc("Java",
    cast_from_uint16(U16::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = U16;
").

:- pragma no_determinism_warning(cast_from_uint16/1).
cast_from_uint16(_) = _ :-
    sorry($module, "int16.cast_from_uint16/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(I16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = I16;
").

:- pragma foreign_proc("C#",
    to_int(I16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I16;
").

:- pragma foreign_proc("Java",
    to_int(I16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I16;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bytes_le(LSB::in, MSB::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    unsigned char *int16_bytes = (unsigned char *) &I16;
#if defined(MR_BIG_ENDIAN)
    int16_bytes[0] = MSB;
    int16_bytes[1] = LSB;
#else
    int16_bytes[0] = LSB;
    int16_bytes[1] = MSB;
#endif
").

:- pragma foreign_proc("Java",
    from_bytes_le(LSB::in, MSB::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) (MSB << java.lang.Byte.SIZE | (LSB & 0x00ff));
").

:- pragma foreign_proc("C#",
    from_bytes_le(LSB::in, MSB::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) (MSB << 8 | (LSB & 0x00ff));
").

from_bytes_le(_, _) = _ :-
    sorry($module, "int16.from_bytes_le/2 NYI for Erlang").

%---------------------------------------------------------------------------%

from_bytes_be(MSB, LSB) =
    from_bytes_le(LSB, MSB).

%---------------------------------------------------------------------------%

abs(Num) =
    ( if Num = int16.min_int16 then
        throw(software_error("int16.abs: abs(min_int16) would overflow"))
    else
        unchecked_abs(Num)
    ).

unchecked_abs(Num) =
    ( if Num < 0i16 then
        0i16 - Num
    else
        Num
    ).

nabs(Num) =
    ( if Num > 0i16 then
        -Num
    else
        Num
    ).

%---------------------------------------------------------------------------%

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

X div Y = Div :-
    Trunc = X // Y,
    ( if
        ( X >= 0i16, Y >= 0i16
        ; X < 0i16, Y < 0i16
        ; X rem Y = 0i16
        )
    then
        Div = Trunc
    else
        Div = Trunc - 1i16
    ).

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0i16 then
        throw(math.domain_error("int16.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X - (X div Y) * Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0i16 then
        throw(math.domain_error("int16.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int16.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int16.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ 1i16) = 0i16.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1i16) \= 0i16.

%---------------------------------------------------------------------------%

% The algorithms in this section are adapted from chapter 5 of
% ``Hacker's Delight'' by Henry S. Warren, Jr.

num_zeros(U) = 16 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint16_t U = I;
    U = (U & 0x5555) + ((U >> 1) & 0x5555);
    U = (U & 0x3333) + ((U >> 2) & 0x3333);
    U = (U & 0x0f0f) + ((U >> 4) & 0x0f0f);
    U = (U & 0x00ff) + ((U >> 8) & 0x00ff);
    N = U;
").

:- pragma foreign_proc("C#",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ushort U = (ushort) I;
    U = (ushort)((U & 0x5555) + ((U >> 1) & 0x5555));
    U = (ushort)((U & 0x3333) + ((U >> 2) & 0x3333));
    U = (ushort)((U & 0x0f0f) + ((U >> 4) & 0x0f0f));
    U = (ushort)((U & 0x00ff) + ((U >> 8) & 0x00ff));
    N = U;
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U << 16);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint16_t U = I;
    if (U == 0) {
        N = 16;
    } else {
        int n = 1;
        if ((U >> 8) == 0) { n = n + 8;   U = U << 8; }
        if ((U >> 12) == 0) { n = n + 4;  U = U << 4; }
        if ((U >> 14) == 0) { n = n + 2;  U = U << 2; }
        if ((U >> 15) == 0) { n = n + 1;  U = U << 1; }
        N = n - (int)(U >> 15);
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ushort U = (ushort) I;
    if (U == 0) {
        N = 16;
    } else {
        int n = 1;
        if ((U >> 8) == 0)  { n = n + 8; U = (ushort)(U << 8); }
        if ((U >> 12) == 0) { n = n + 4; U = (ushort)(U << 4); }
        if ((U >> 14) == 0) { n = n + 2; U = (ushort)(U << 2); }
        if ((U >> 15) == 0) { n = n + 1; U = (ushort)(U << 1); }
        N = n - (int)(U >> 15);
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

num_trailing_zeros(U) =
    16 - num_leading_zeros(\ U /\ (U - 1i16)).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (int16_t) MR_uint16_reverse_bytes((uint16_t)A);
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) ((A & 0x00ffU) << 8 | (A & 0xff00U) >> 8);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Short.reverseBytes(A);
").

:- pragma no_determinism_warning(reverse_bytes/1).
reverse_bytes(_) = _ :-
    sorry($module, "int16.reverse_bytes/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint16_t u_A = A;
    u_A = (((~0x5555) & u_A) >> 1) | ((0x5555 & u_A) << 1);
    u_A = (((~0x3333) & u_A) >> 2) | ((0x3333 & u_A) << 2);
    u_A = (((~0x0f0f) & u_A) >> 4) | ((0x0f0f & u_A) << 4);
    u_A = (((~0x00ff) & u_A) >> 8) | ((0x00ff & u_A) << 8);
    B = u_A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ushort u_A = (ushort) A;
    u_A = (ushort)((((~0x5555) & u_A) >> 1) | ((0x5555 & u_A) << 1));
    u_A = (ushort)((((~0x3333) & u_A) >> 2) | ((0x3333 & u_A) << 2));
    u_A = (ushort)((((~0x0f0f) & u_A) >> 4) | ((0x0f0f & u_A) << 4));
    u_A = (ushort)((((~0x00ff) & u_A) >> 8) | ((0x00ff & u_A) << 8));
    B = (short) u_A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) (java.lang.Integer.reverse(A << 16) & 0xffff);
").


%---------------------------------------------------------------------------%

min_int16 = -32_768_i16.

max_int16 = 32_767_i16.

%---------------------------------------------------------------------------%

int16_to_doc(X) = str(string.int16_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int16.
%---------------------------------------------------------------------------%
