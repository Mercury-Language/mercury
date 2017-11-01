%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % from_int(I, U32):
    % Convert an int into a uint32.
    % Fails if I is not in [0, 2147483647].
    %
:- pred from_int(int::in, uint32::out) is semidet.

:- func det_from_int(int) = uint32.

:- func cast_from_int(int) = uint32.

:- func cast_from_int32(int32) = uint32.

:- func cast_to_int(uint32) = int.

    % from_bytes_le(Byte0, Byte1, Byte2, Byte3) = U32:
    % U32 is the uint32 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte3 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8) = uint32.

    % from_bytes_be(Byte0, Byte1, Byte2, Byte3) = U32:
    % U32 is the uint32 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte3 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8) = uint32.

%---------------------------------------------------------------------------%

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

    % Maximum.
    %
:- func max(uint32, uint32) = uint32.

    % Minimum.
    %
:- func min(uint32, uint32) = uint32.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint32::in) div (uint32::in) = (uint32::uo) is det.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
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
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint32::in) mod (uint32::in) = (uint32::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (uint32::in) rem (uint32::in) = (uint32::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint32::in, uint32::in) = (uint32::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 32).
    %
:- func (uint32::in) << (int::in) = (uint32::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 32).
    % It will typically be be implemented more efficiently than X << Y.
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

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint32::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint32::in) is semidet.

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

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(uint32) = uint32.

:- func max_uint32 = uint32.

    % Convert a uint32 to a pretty_printer.doc for formatting.
    %
:- func uint32_to_doc(uint32) = pretty_printer.doc.

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
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if ((uint64_t)I > (uint64_t)UINT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (uint32_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma no_determinism_warning(from_int/2).
from_int(_, _) :-
    sorry($module, "uint32.from_int NYI for Erlang").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("uint32.det_from_int: cannot convert int to uint32")
    ).

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "uint32.cast_from_int/1 NYI for Erlang").

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

:- pragma no_determinism_warning(cast_from_int32/1).
cast_from_int32(_) = _ :-
    sorry($module, "uint32.cast_from_int32/1 NYI for Erlang").

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

:- pragma no_determinism_warning(cast_to_int/1).
cast_to_int(_) = _ :-
    sorry($module, "uint32.cast_to_int/1 NYI for Erlang").

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
    uint32_bytes[3] = Byte2;
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
    U32 = (Byte3 & 0xff) << 24 |
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

from_bytes_le(_, _, _, _) = _ :-
    sorry($module, "uint32.from_bytes_le/4 NYI for Erlang").

%---------------------------------------------------------------------------%

from_bytes_be(Byte3, Byte2, Byte1, Byte0) =
    from_bytes_le(Byte0, Byte1, Byte2, Byte3).

%---------------------------------------------------------------------------%

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0u32 then
        throw(math.domain_error("uint32.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0u32 then
        throw(math.domain_error("uint32.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint32.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 32u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint32.(>>): second operand is out of range",
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
    (X /\ 1u32) = 0u32.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1u32) \= 0u32.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

max_uint32 = 4_294_967_295_u32.

%---------------------------------------------------------------------------%

uint32_to_doc(X) = str(string.uint32_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint32.
%---------------------------------------------------------------------------%
