%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: int64.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with signed 64-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module int64.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % from_int(I) = I64:
    % Convert an int to an int64.
    %
:- func from_int(int) = int64.

:- func cast_to_int(int64) = int.

:- func cast_from_uint64(uint64) = int64.

    % from_bytes_le(Byte0, Byte1, ..., Byte7) = I64:
    % I64 is the int64 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte7 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8, uint8, uint8, uint8,
    uint8) = int64.

    % from_bytes_be(Byte0, Byte1, ..., Byte7) = I64:
    % I64 is the int64 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte7 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8, uint8, uint8, uint8,
    uint8) = int64.

%---------------------------------------------------------------------------%

    % Less than.
    %
:- pred (int64::in) < (int64::in) is semidet.

    % Greater than.
    %
:- pred (int64::in) > (int64::in) is semidet.

    % Less than or equal.
    %
:- pred (int64::in) =< (int64::in) is semidet.

    % Greater than or equal.
    %
:- pred (int64::in) >= (int64::in) is semidet.

%---------------------------------------------------------------------------%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int64.min_int64.
    %
:- func abs(int64) = int64.

    % unchecked_abs(X) returns the absolute value of X, except that the result
    % is undefined if X = int64.min_int64.
    %
:- func unchecked_abs(int64) = int64.

    % nabs(X) returns the negative of the absolute value of X.
    % Unlike abs/1 this function is defined for X = int64.min_int64.
    %
:- func nabs(int64) = int64.

%---------------------------------------------------------------------------%

    % Maximum.
    %
:- func max(int64, int64) = int64.

    % Minimum.
    %
:- func min(int64, int64) = int64.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int64::in) = (int64::uo) is det.

    % Unary minus.
    %
:- func - (int64::in) = (int64::uo) is det.

    % Addition.
    %
:- func int64 + int64 = int64.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(int64, int64) = int64.

    % Subtraction.
    %
:- func int64 - int64 = int64.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(int64, int64) = int64.

    % Multiplication.
    %
:- func (int64::in) * (int64::in) = (int64::uo) is det.
:- func times(int64, int64) = int64.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i64) div 3_i64 = (-4_i64).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int64::in) div (int64::in) = (int64::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i64) // 3_i64 = (-3_i64).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int64::in) // (int64::in) = (int64::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (int64::in) / (int64::in) = (int64::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int64::in, int64::in) = (int64::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int64::in) mod (int64::in) = (int64::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (int64::in) rem (int64::in) = (int64::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int64::in, int64::in) = (int64::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (int64::in) << (int::in) = (int64::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 64).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int64::in, int::in) = (int64::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (int64::in) >> (int::in) = (int64::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_int64).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int64::in, int::in) = (int64::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(int64::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(int64::in) is semidet.

    % Bitwise and.
    %
:- func (int64::in) /\ (int64::in) = (int64::uo) is det.

    % Bitwise or.
    %
:- func (int64::in) \/ (int64::in) = (int64::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int64, int64) = int64.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int64::in) = (int64::uo) is det.

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(int64) = int64.

:- func min_int64 = int64.

:- func max_int64 = int64.

    % Convert an int64 to a pretty_printer.doc for formatting.
    %
:- func int64_to_doc(int64) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module math.
:- import_module require.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = (int64_t) A;
").

:- pragma foreign_proc("C#",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (long) A; // Mercury's 'int' type in the C# grade is 32-bits.
").

:- pragma foreign_proc("Java",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (long) A; // Mercury's 'int' type in the Java grade is 32-bits.
").

:- pragma no_determinism_warning(from_int/1).
from_int(_) = _ :-
    sorry($module, "NYI int64.from_int for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int(I64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = (MR_Integer) I64;
").

:- pragma foreign_proc("C#",
    cast_to_int(I64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) I64;
").

:- pragma foreign_proc("Java",
    cast_to_int(I64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) I64;
").

:- pragma no_determinism_warning(cast_to_int/1).
cast_to_int(_) = _ :-
    sorry($module, "NYI int64.cast_to_int for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint64(U64::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = (int64_t) U64;
").

:- pragma foreign_proc("C#",
    cast_from_uint64(U64::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) U64;
").

:- pragma foreign_proc("Java",
    cast_from_uint64(U64::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) U64;
").

:- pragma no_determinism_warning(cast_from_uint64/1).
cast_from_uint64(_) = _ :-
    sorry($module, "NYI int64.cast_from_uint64 for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in,
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    unsigned char *int64_bytes = (unsigned char *) &I64;
#if defined(MR_BIG_ENDIAN)
    int64_bytes[0] = Byte7;
    int64_bytes[1] = Byte6;
    int64_bytes[2] = Byte5;
    int64_bytes[3] = Byte4;
    int64_bytes[4] = Byte3;
    int64_bytes[5] = Byte2;
    int64_bytes[6] = Byte1;
    int64_bytes[7] = Byte0;
#else
    int64_bytes[0] = Byte0;
    int64_bytes[1] = Byte1;
    int64_bytes[2] = Byte2;
    int64_bytes[3] = Byte3;
    int64_bytes[4] = Byte4;
    int64_bytes[5] = Byte5;
    int64_bytes[6] = Byte6;
    int64_bytes[7] = Byte7;
#endif
").

:- pragma foreign_proc("Java",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in,
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long)(Byte7 & 0xff) << 56 |
          (long)(Byte6 & 0xff) << 48 |
          (long)(Byte5 & 0xff) << 40 |
          (long)(Byte4 & 0xff) << 32 |
          (long)(Byte3 & 0xff) << 24 |
          (long)(Byte2 & 0xff) << 16 |
          (long)(Byte1 & 0xff) << 8  |
          (long)(Byte0 & 0xff);
").

:- pragma foreign_proc("C#",
    from_bytes_le(Byte0::in, Byte1::in, Byte2::in, Byte3::in,
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) (
        (ulong)Byte7 << 56 |
        (ulong)Byte6 << 48 |
        (ulong)Byte5 << 40 |
        (ulong)Byte4 << 32 |
        (ulong)Byte3 << 24 |
        (ulong)Byte2 << 16 |
        (ulong)Byte1 << 8  |
        (ulong)Byte0);
").

from_bytes_le(_, _, _, _, _, _, _, _) = _ :-
    sorry($module, "int64.from_bytes_le/8 NYI for Erlang").

%---------------------------------------------------------------------------%

from_bytes_be(Byte7, Byte6, Byte5,Byte4, Byte3, Byte2, Byte1, Byte0) =
    from_bytes_le(Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7).

%---------------------------------------------------------------------------%

abs(Num) = Abs :-
    ( if Num = int64.min_int64 then
        throw(software_error("int64.abs: abs(min_int64) would overflow"))
    else
        Abs = unchecked_abs(Num)
    ).

unchecked_abs(Num) =
    ( if Num < 0i64 then
        0i64 - Num
    else
        Num
    ).

nabs(Num) =
    ( if Num > 0i64 then
        -Num
    else
        Num
    ).

%---------------------------------------------------------------------------%

X div Y = Div :-
    Trunc = X // Y,
    ( if
        ( X >= 0i64, Y >= 0i64
        ; X < 0i64, Y < 0i64
        ; X rem Y = 0i64
        )
    then
        Div = Trunc
    else
        Div = Trunc - 1i64
    ).

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0i64 then
        throw(math.domain_error("int64.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X  - (X div Y) * Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0i64 then
        throw(math.domain_error("int64.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int64.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int64.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ 1i64) = 0i64.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1i64) \= 0i64.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = (int64_t) MR_uint64_reverse_bytes((uint64_t)A);
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ulong u_A = (ulong) A;

    B = (long) ((u_A & 0x00000000000000ffUL) << 56  |
         (u_A & 0x000000000000ff00UL) << 40  |
         (u_A & 0x0000000000ff0000UL) << 24  |
         (u_A & 0x00000000ff000000UL) << 8   |
         (u_A & 0x000000ff00000000UL) >> 8   |
         (u_A & 0x0000ff0000000000UL) >> 24  |
         (u_A & 0x00ff000000000000UL) >> 40  |
         (u_A & 0xff00000000000000UL) >> 56);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Long.reverseBytes(A);
").

:- pragma no_determinism_warning(reverse_bytes/1).
reverse_bytes(_) = _ :-
    sorry($module, "int64.reverse_bytes/1 NYI for Erlang").

%---------------------------------------------------------------------------%

min_int64 = -9_223_372_036_854_775_808_i64.

max_int64 = 9_223_372_036_854_775_807_i64.

%---------------------------------------------------------------------------%

int64_to_doc(X) = str(string.int64_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int64.
%---------------------------------------------------------------------------%
