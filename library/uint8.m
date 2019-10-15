%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: uint8.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with unsigned 8-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module uint8.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Conversion from int.
%

    % from_int(I, U8):
    %
    % Convert an int to a uint8.
    % Fails if I is not in [0, 2^8 - 1].
    %
:- pred from_int(int::in, uint8::out) is semidet.

    % det_from_int(I) = U8:
    %
    % Convert an int to a uint8.
    % Throws an exception if I is not in [0, 2^8 - 1].
    %
:- func det_from_int(int) = uint8.

    % cast_from_int(I) = U8:
    %
    % Convert an int to a uint8.
    % Always succeeds, but will yield a result that is mathematically equal
    % to I only if I is in [0, 2^8 - 1].
    %
:- func cast_from_int(int) = uint8.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % to_int(U8) = I:
    %
    % Convert a uint8 to an int.
    % Always succeeds, and yields a result that is mathematically equal
    % to U8.
    %
:- func to_int(uint8) = int.

    % cast_to_int(U8) = I:
    %
    % Convert a uint8 to an int.
    % Always succeeds, and yields a result that is mathematically equal
    % to U8.
    %
:- func cast_to_int(uint8) = int.

%---------------------------------------------------------------------------%
%
% Conversion to uint.
%

    % cast_to_uint(U8) = U:
    %
    % Convert a uint8 to a uint.
    % Always succeeds, and yields a result that is mathematically equal
    % to U8.
    %
:- func cast_to_uint(uint8) = uint.

%---------------------------------------------------------------------------%
%
% Change of signedness.
%

    % cast_from_int8(I8) = U8:
    %
    % Convert an int8 to a uint8. This will yield a result that is
    % mathematically equal to I8 only if I8 is in [0, 2^7 - 1].
    %
:- func cast_from_int8(int8) = uint8.

%---------------------------------------------------------------------------%
%
% Comparisons and related operations.
%

    % Less than.
    %
:- pred (uint8::in) < (uint8::in) is semidet.

    % Greater than.
    %
:- pred (uint8::in) > (uint8::in) is semidet.

    % Less than or equal.
    %
:- pred (uint8::in) =< (uint8::in) is semidet.

    % Greater than or equal.
    %
:- pred (uint8::in) >= (uint8::in) is semidet.

    % Maximum.
    %
:- func max(uint8, uint8) = uint8.

    % Minimum.
    %
:- func min(uint8, uint8) = uint8.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

    % Addition.
    %
:- func uint8 + uint8 = uint8.
:- mode in + in = uo is det.
:- mode uo + in = in is det.
:- mode in + uo = in is det.

:- func plus(uint8, uint8) = uint8.

    % Subtraction.
    %
:- func uint8 - uint8 = uint8.
:- mode in - in = uo is det.
:- mode uo - in = in is det.
:- mode in - uo = in is det.

:- func minus(uint8, uint8) = uint8.

    % Multiplication.
    %
:- func (uint8::in) * (uint8::in) = (uint8::uo) is det.
:- func times(uint8, uint8) = uint8.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint8::in) div (uint8::in) = (uint8::uo) is det.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint8::in) // (uint8::in) = (uint8::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (uint8::in) / (uint8::in) = (uint8::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(uint8::in, uint8::in) = (uint8::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint8::in) mod (uint8::in) = (uint8::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (uint8::in) rem (uint8::in) = (uint8::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint8::in, uint8::in) = (uint8::uo) is det.

    % even(X) is equivalent to (X mod 2u8 = 0u8).
    %
:- pred even(uint8::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2u8 = 1u8).
    %
:- pred odd(uint8::in) is semidet.

%---------------------------------------------------------------------------%
%
% Shift operations.
%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 8).
    %
:- func (uint8::in) << (int::in) = (uint8::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 8).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint8::in, int::in) = (uint8::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 8).
    %
:- func (uint8::in) >> (int::in) = (uint8::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 8).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint8::in, int::in) = (uint8::uo) is det.

%---------------------------------------------------------------------------%
%
% Logical operations.
%

    % Bitwise and.
    %
:- func (uint8::in) /\ (uint8::in) = (uint8::uo) is det.

    % Bitwise or.
    %
:- func (uint8::in) \/ (uint8::in) = (uint8::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(uint8, uint8) = uint8.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (uint8::in) = (uint8::uo) is det.

%---------------------------------------------------------------------------%
%
% Operations on bits and bytes.
%

    % num_zeros(U) = N:
    %
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint8) = int.

    % num_ones(U) = N:
    %
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint8) = int.

    % num_leading_zeros(U) = N:
    %
    % N is the number of leading zeros in the binary representation of U,
    % starting at the most significant bit position.
    % Note that num_leading_zeros(0u8) = 8.
    %
:- func num_leading_zeros(uint8) = int.

    % num_trailing_zeros(U) = N:
    %
    % N is the number of trailing zeros in the binary representation of U,
    % starting at the least significant bit position.
    % Note that num_trailing_zeros(0u8) = 8.
    %
:- func num_trailing_zeros(uint8) = int.

    % reverse_bits(A) = B:
    %
    % B is the is value that results from reversing the bits in the binary
    % representation of A.
    %
:- func reverse_bits(uint8) = uint8.

%---------------------------------------------------------------------------%
%
% Limits.
%

:- func max_uint8 = uint8.

%---------------------------------------------------------------------------%
%
% Prettyprinting.
%

    % Convert an uint8 to a pretty_printer.doc for formatting.
    %
:- func uint8_to_doc(uint8) = pretty_printer.doc.

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
    I =< 255,
    U8 = cast_from_int(I).

det_from_int(I) = U8 :-
    ( if from_int(I, U8Prime) then
        U8 = U8Prime
    else
        error($pred, "cannot convert int to uint8")
    ).

:- pragma no_determinism_warning(cast_from_int/1).

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U8 = (uint8_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = (byte) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = (byte) I;
").

cast_from_int(_) = _ :-
    sorry($module, "uint8.cast_from_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(to_int/1).

:- pragma foreign_proc("C",
    to_int(U8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = U8;
").

:- pragma foreign_proc("C#",
    to_int(U8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U8;
").

:- pragma foreign_proc("Java",
    to_int(U8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U8 & 0xff;
").

to_int(_) = _ :-
    sorry($module, "uint8.to_int/1 NYI for Erlang").

:- pragma no_determinism_warning(cast_to_int/1).

:- pragma foreign_proc("C",
    cast_to_int(U8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = U8;
").

:- pragma foreign_proc("C#",
    cast_to_int(U8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U8;
").

:- pragma foreign_proc("Java",
    cast_to_int(U8::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U8 & 0xff;
").

cast_to_int(_) = _ :-
    sorry($module, "uint8.cast_to_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(cast_to_uint/1).

:- pragma foreign_proc("C",
    cast_to_uint(U8::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U = (MR_Unsigned) U8;
").

:- pragma foreign_proc("C#",
    cast_to_uint(U8::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) U8;
").

:- pragma foreign_proc("Java",
    cast_to_uint(U8::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U8 & 0xff;
").

cast_to_uint(_) = _ :-
    sorry($module, "uint8.cast_to_uint/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(cast_from_int8/1).

:- pragma foreign_proc("C",
    cast_from_int8(I8::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U8 = (uint8_t) I8;
").

:- pragma foreign_proc("C#",
    cast_from_int8(I8::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = (byte) I8;
").

:- pragma foreign_proc("Java",
    cast_from_int8(I8::in) = (U8::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U8 = I8;
").

cast_from_int8(_) = _ :-
    sorry($module, "uint8.cast_from_int8/1 NYI for Erlang").

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
    ( if Y = 0u8 then
        throw(math.domain_error("uint8.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0u8 then
        throw(math.domain_error("uint8.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(even/1).
even(X) :-
    (X /\ 1u8) = 0u8.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1u8) \= 0u8.

%---------------------------------------------------------------------------%

% The operations unchecked_left_shift and unchecked_right_shift are builtins.

X << Y = Result :-
    ( if cast_from_int(Y) < 8u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint8.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 8u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint8.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

num_zeros(U) = 8 - num_ones(U).

:- pragma foreign_decl("C", "
extern const uint8_t ML_uint8_num_ones_table[];
").

:- pragma foreign_code("C", "
const uint8_t ML_uint8_num_ones_table[256] = {
    0,1,1,2,1,2,2,3,
    1,2,2,3,2,3,3,4,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    4,5,5,6,5,6,6,7,
    5,6,6,7,6,7,7,8
};
").

:- pragma foreign_code("C#", "
public static byte[] num_ones_table = {
    0,1,1,2,1,2,2,3,
    1,2,2,3,2,3,3,4,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    4,5,5,6,5,6,6,7,
    5,6,6,7,6,7,7,8
};

").

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    N = ML_uint8_num_ones_table[U];
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = mercury.uint8.num_ones_table[U];
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U << 24);
").

%---------------------%

:- pragma foreign_decl("C", "
extern const uint8_t ML_uint8_nlz_table[];
").

:- pragma foreign_code("C", "
const uint8_t ML_uint8_nlz_table[256] = {
  8,7,6,6,5,5,5,5,
  4,4,4,4,4,4,4,4,
  3,3,3,3,3,3,3,3,
  3,3,3,3,3,3,3,3,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0
};

").

:- pragma foreign_code("C#", "
public static byte[] nlz_table = {
  8,7,6,6,5,5,5,5,
  4,4,4,4,4,4,4,4,
  3,3,3,3,3,3,3,3,
  3,3,3,3,3,3,3,3,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0
};

").

:- pragma foreign_code("Java", "
public static byte[] nlz_table = {
  8,7,6,6,5,5,5,5,
  4,4,4,4,4,4,4,4,
  3,3,3,3,3,3,3,3,
  3,3,3,3,3,3,3,3,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  2,2,2,2,2,2,2,2,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0
};
").

:- pragma foreign_proc("C",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    N = ML_uint8_nlz_table[I];
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = mercury.uint8.nlz_table[U];
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = jmercury.uint8.nlz_table[U & 0xff];
").

num_trailing_zeros(U) =
    8 - num_leading_zeros(\ U /\ (U - 1u8)).

%---------------------%

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (byte) (java.lang.Integer.reverse(A << 24) & 0xff);
").

reverse_bits(!.A) = B :-
    !:A = ((!.A /\ 0xf0_u8) >> 4) \/ ((!.A /\ 0x0f_u8) << 4),
    !:A = ((!.A /\ 0xcc_u8) >> 2) \/ ((!.A /\ 0x33_u8) << 2),
    !:A = ((!.A /\ 0xaa_u8) >> 1) \/ ((!.A /\ 0x55_u8) << 1),
    B = !.A.

%---------------------------------------------------------------------------%

max_uint8 = 255_u8.

%---------------------------------------------------------------------------%

uint8_to_doc(X) = str(string.uint8_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint8.
%---------------------------------------------------------------------------%
