%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2018, 2020-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: int8.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with signed 8-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module int8.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Conversion from int.
%

    % from_int(I, I8):
    %
    % Convert an int to an int8.
    % Fails if I is not in [-(2^7), 2^7 - 1].
    %
:- pred from_int(int::in, int8::out) is semidet.

    % det_from_int(I) = I8:
    %
    % Convert an int to an int8.
    % Throws an exception if I is not in [-(2^7), 2^7 - 1].
    %
:- func det_from_int(int) = int8.

    % cast_from_int(I) = I8:
    %
    % Convert an int to an int8.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [-(2^7), 2^7 - 1].
    %
:- func cast_from_int(int) = int8.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % to_int(I8) = I:
    %
    % Convert an int8 to an int. Since an int can be only 32 or 64 bits,
    % this is guaranteed to yield a result that is mathematically equal
    % to the original.
    %
:- func to_int(int8) = int.

    % cast_to_int(I8) = I:
    %
    % Convert an int8 to an int. Since an int can be only 32 or 64 bits,
    % this is guaranteed to yield a result that is mathematically equal
    % to the original.
    %
:- func cast_to_int(int8) = int.

%---------------------------------------------------------------------------%
%
% Change of signedness.
%

    % cast_from_uint8(U8) = I8:
    %
    % Convert a uint8 to an int8. This will yield a result that is
    % mathematically equal to U8 only if U8 is in [0, 2^7 - 1].
    %
:- func cast_from_uint8(uint8) = int8.

%---------------------------------------------------------------------------%
%
% Comparisons and related operations.
%

    % Less than.
    %
:- pred (int8::in) < (int8::in) is semidet.

    % Greater than.
    %
:- pred (int8::in) > (int8::in) is semidet.

    % Less than or equal.
    %
:- pred (int8::in) =< (int8::in) is semidet.

    % Greater than or equal.
    %
:- pred (int8::in) >= (int8::in) is semidet.

    % Maximum.
    %
:- func max(int8, int8) = int8.

    % Minimum.
    %
:- func min(int8, int8) = int8.

%---------------------------------------------------------------------------%
%
% Absolute values.
%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int8.min_int8.
    %
:- func abs(int8) = int8.

    % unchecked_abs(X) returns the absolute value of X, except that the result
    % is undefined if X = int8.min_int8.
    %
:- func unchecked_abs(int8) = int8.

    % nabs(X) returns the negative of the absolute value of X.
    % Unlike abs/1 this function is defined for X = int8.min_int8.
    %
:- func nabs(int8) = int8.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

    % Unary plus.
    %
:- func + (int8::in) = (int8::uo) is det.

    % Unary minus.
    %
:- func - (int8::in) = (int8::uo) is det.

    % Addition.
    %
:- func int8 + int8 = int8.
:- mode in + in = uo is det.
:- mode uo + in = in is det.
:- mode in + uo = in is det.

:- func plus(int8, int8) = int8.

    % Subtraction.
    %
:- func int8 - int8 = int8.
:- mode in - in = uo is det.
:- mode uo - in = in is det.
:- mode in - uo = in is det.

:- func minus(int8, int8) = int8.

    % Multiplication.
    %
:- func (int8::in) * (int8::in) = (int8::uo) is det.
:- func times(int8, int8) = int8.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i8) div 3_i8 = (-4_i8).
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (int8::in) div (int8::in) = (int8::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i8) // 3_i8 = (-3_i8).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (int8::in) // (int8::in) = (int8::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (int8::in) / (int8::in) = (int8::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int8::in, int8::in) = (int8::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (int8::in) mod (int8::in) = (int8::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `domain_error/` exception if the right operand is zero.
    %
:- func (int8::in) rem (int8::in) = (int8::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int8::in, int8::in) = (int8::uo) is det.

    % even(X) is equivalent to (X mod 2i8 = 0i8).
    %
:- pred even(int8::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2i8 = 1i8).
    %
:- pred odd(int8::in) is semidet.

%---------------------------------------------------------------------------%
%
% Shift operations.
%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 8).
    %
:- func (int8::in) << (int::in) = (int8::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 8).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int8::in, int::in) = (int8::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 8).
    %
:- func (int8::in) >> (int::in) = (int8::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 8).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int8::in, int::in) = (int8::uo) is det.

%---------------------------------------------------------------------------%
%
% Logical operations.
%

    % Bitwise and.
    %
:- func (int8::in) /\ (int8::in) = (int8::uo) is det.

    % Bitwise or.
    %
:- func (int8::in) \/ (int8::in) = (int8::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int8, int8) = int8.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int8::in) = (int8::uo) is det.

%---------------------------------------------------------------------------%
%
% Operations on bits and bytes.
%

    % num_zeros(I) = N:
    %
    % N is the number of zeros in the binary representation of I.
    %
:- func num_zeros(int8) = int.

    % num_ones(I) = N:
    % N is the number of ones in the binary representation of I.
    %
:- func num_ones(int8) = int.

    % num_leading_zeros(I) = N:
    %
    % N is the number of leading zeros in the binary representation of I,
    % starting at the most significant bit position.
    % Note that num_leading_zeros(0i8) = 8.
    %
:- func num_leading_zeros(int8) = int.

    % num_trailing_zeros(I) = N:
    %
    % N is the number of trailing zeros in the binary representation of I,
    % starting at the least significant bit position.
    % Note that num_trailing_zeros(0i8) = 8.
    %
:- func num_trailing_zeros(int8) = int.

    % reverse_bits(A) = B:
    %
    % B is the is value that results from reversing the bits in the binary
    % representation of A.
    %
:- func reverse_bits(int8) = int8.

%---------------------------------------------------------------------------%
%
% Limits.

:- func min_int8 = int8.

:- func max_int8 = int8.

%---------------------------------------------------------------------------%
%
% Prettyprinting.
%

    % Convert an int8 to a pretty_printer.doc for formatting.
    %
:- func int8_to_doc(int8) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

from_int(I, I8) :-
    I >= -128,
    I =< 127,
    I8 = cast_from_int(I).

det_from_int(I) = I8 :-
    ( if from_int(I, I8Prime) then
        I8 = I8Prime
    else
        error($pred, "cannot convert int to int8")
    ).

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I8 = (int8_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I8 = (sbyte) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I8 = (byte) I;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(I8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = I8;
").

:- pragma foreign_proc("C#",
    to_int(I8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I8;
").

:- pragma foreign_proc("Java",
    to_int(I8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I8;
").

:- pragma foreign_proc("C",
    cast_to_int(I8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = I8;
").

:- pragma foreign_proc("C#",
    cast_to_int(I8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I8;
").

:- pragma foreign_proc("Java",
    cast_to_int(I8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I8;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint8(U8::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I8 = U8;
").

:- pragma foreign_proc("C#",
    cast_from_uint8(U8::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I8 = (sbyte) U8;
").

:- pragma foreign_proc("Java",
    cast_from_uint8(U8::in) = (I8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I8 = U8;
").

%---------------------------------------------------------------------------%

% The comparison operations <, >, =< and >= are builtins.

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

abs(Num) =
    ( if Num = int8.min_int8 then
        throw(software_error("int8.abs: abs(min_int8) would overflow"))
    else
        unchecked_abs(Num)
    ).

unchecked_abs(Num) =
    ( if Num < 0i8 then
        0i8 - Num
    else
        Num
    ).

nabs(Num) =
    ( if Num > 0i8 then
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
        ( X >= 0i8, Y >= 0i8
        ; X < 0i8, Y < 0i8
        ; X rem Y = 0i8
        )
    then
        Div = Trunc
    else
        Div = Trunc - 1i8
    ).

:- pragma inline(func('//'/2)).
X // Y = Div :-
    ( if Y = 0i8 then
        throw(domain_error("int8.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline(func('/'/2)).
X / Y = X // Y.

X mod Y = X - (X div Y) * Y.

:- pragma inline(func(rem/2)).
X rem Y = Rem :-
    ( if Y = 0i8 then
        throw(domain_error("int8.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(pred(even/1)).
even(X) :-
    (X /\ 1i8) = 0i8.

:- pragma inline(pred(odd/1)).
odd(X) :-
    (X /\ 1i8) \= 0i8.

%---------------------------------------------------------------------------%

% The operations unchecked_left_shift and unchecked_right_shift are builtins.

X << Y = Result :-
    ( if cast_from_int(Y) < 8u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int8.(<<): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 8u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int8.(>>): second operand is out of range",
        throw(domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

num_zeros(I) = 8 - num_ones(I).

num_ones(I8) = N :-
    U8 = uint8.cast_from_int8(I8),
    N = uint8.num_ones(U8).

num_leading_zeros(I8) = N :-
    U8 = uint8.cast_from_int8(I8),
    N = uint8.num_leading_zeros(U8).

num_trailing_zeros(I8) = N :-
    U8 = uint8.cast_from_int8(I8),
    N = uint8.num_trailing_zeros(U8).

reverse_bits(I8) = RevI8 :-
    U8 = uint8.cast_from_int8(I8),
    RevU8 = uint8.reverse_bits(U8),
    RevI8 = int8.cast_from_uint8(RevU8).

%---------------------------------------------------------------------------%

min_int8 = -128_i8.

max_int8 = 127_i8.

%---------------------------------------------------------------------------%

int8_to_doc(X) = str(string.int8_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int8.
%---------------------------------------------------------------------------%
