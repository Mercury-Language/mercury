%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % from_int(I, I32):
    % Convert an int to an int32.
    % Fails if not in [-2147483648, 2147483647].
    %
:- pred from_int(int::in, int32::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = int32.

:- func cast_from_int(int) = int32.

:- func cast_from_uint32(uint32) = int32.

:- func to_int(int32) = int.

    % from_bytes_le(Byte0, Byte1, Byte2, Byte3) = I32:
    % I32 is the int32 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte3 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8) = int32.

    % from_bytes_be(Byte0, Byte1, Byte2, Byte3) = I32:
    % I32 is the int32 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte3 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8) = int32.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int32::in) = (int32::uo) is det.

    % Unary minus.
    %
:- func - (int32::in) = (int32::uo) is det.

    % Addition.
    %
:- func int32 + int32 = int32.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(int32, int32) = int32.

    % Subtraction.
    %
:- func int32 - int32 = int32.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(int32, int32) = int32.

    % Multiplication.
    %
:- func (int32::in) * (int32::in) = (int32::uo) is det.
:- func times(int32, int32) = int32.

    % Maximum.
    %
:- func max(int32, int32) = int32.

    % Minimum.
    %
:- func min(int32, int32) = int32.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i32) div 3_i32 = (-4_i32).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int32::in) div (int32::in) = (int32::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i32) // 3_i32 = (-3_i32).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
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
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int32::in) mod (int32::in) = (int32::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (int32::in) rem (int32::in) = (int32::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int32::in, int32::in) = (int32::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (int32::in) << (int::in) = (int32::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 32).
    % It will typically be be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int32::in, int::in) = (int32::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (int32::in) >> (int::in) = (int32::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_int32).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int32::in, int::in) = (int32::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(int32::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(int32::in) is semidet.

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

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(int32) = int32.

:- func min_int32 = int32.

:- func max_int32 = int32.

    % Convert a int32 to a pretty_printer.doc for formatting.
    %
:- func int32_to_doc(int32) = pretty_printer.doc.

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
    from_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (A > (MR_Integer) INT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (A < (MR_Integer) INT32_MIN) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        B = (int32_t) A;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A; // Mercury's 'int' type in the C# grade is 32-bits.
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Java",
    from_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A; // Mercury's 'int' type in the Java grade is 32-bits.
    SUCCESS_INDICATOR = true;
").

det_from_int(I) = U :-
    ( if from_int(I, U0) then
        U = U0
    else
        error("int32.det_from_int: cannot convert int to int32")
    ).

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "int32.cast_from_int/1 NYI for Erlang").

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

:- pragma no_determinism_warning(cast_from_uint32/1).
cast_from_uint32(_) = _ :-
    sorry($module, "int32.cast_from_uint32/1 NYI for Erlang").

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

:- pragma no_determinism_warning(to_int/1).
to_int(_) = _ :-
    sorry($module, "int32.to_int/1 NYI for Erlang").

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
    int32_bytes[3] = Byte2;
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
    I32 = (Byte3 & 0xff) << 24 |
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

from_bytes_le(_, _, _, _) = _ :-
    sorry($module, "int32.from_bytes_le/4 NYI for Erlang").

%---------------------------------------------------------------------------%

from_bytes_be(Byte3, Byte2, Byte1, Byte0) =
    from_bytes_le(Byte0, Byte1, Byte2, Byte3).

%---------------------------------------------------------------------------%

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

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0i32 then
        throw(math.domain_error("int32.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X  - (X div Y) * Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0i32 then
        throw(math.domain_error("int32.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint.(>>): second operand is out of range",
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
    (X /\ 1i32) = 0i32.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1i32) \= 0i32.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = (int32_t) MR_uint32_reverse_bytes((uint32_t)A);
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint u_A = (uint) A;

    B = (int) ((u_A & 0x000000ffU) << 24  |
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

:- pragma no_determinism_warning(reverse_bytes/1).
reverse_bytes(_) = _ :-
    sorry($module, "int32.reverse_bytes/1 NYI for Erlang").

%---------------------------------------------------------------------------%

min_int32 = -2_147_483_648_i32.

max_int32 = 2_147_483_647_i32.

%---------------------------------------------------------------------------%

int32_to_doc(X) = str(string.int32_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int32.
%---------------------------------------------------------------------------%
