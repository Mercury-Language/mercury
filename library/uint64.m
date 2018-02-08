%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % from_int(I, U64):
    % Convert an int into a uint64.
    % Fails if I is not in [0, 2^63 - 1].
    %
:- pred from_int(int::in, uint64::out) is semidet.

:- func det_from_int(int) = uint64.

:- func cast_from_int(int) = uint64.

:- func cast_to_int(uint64) = int.

:- func cast_from_int64(int64) = uint64.

    % from_bytes_le(Byte0, Byte1, ..., Byte7) = U64:
    % U64 is the uint64 whose bytes are given in little-endian order by the
    % arguments from left-to-right (i.e. Byte0 is the least significant byte
    % and Byte7 is the most significant byte).
    %
:- func from_bytes_le(uint8, uint8, uint8, uint8, uint8, uint8, uint8,
    uint8) = uint64.

    % from_bytes_be(Byte0, Byte1, ..., Byte7) = U64:
    % U64 is the uint64 whose bytes are given in big-endian order by the
    % arguments in left-to-right order (i.e. Byte0 is the most significant
    % byte and Byte7 is the least significant byte).
    %
:- func from_bytes_be(uint8, uint8, uint8, uint8, uint8, uint8, uint8,
    uint8) = uint64.

%---------------------------------------------------------------------------%

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

    % Addition.
    %
:- func uint64 + uint64 = uint64.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(uint64, uint64) = uint64.

    % Subtraction.
    %
:- func uint64 - uint64 = uint64.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(uint64, uint64) = uint64.

    % Multiplication.
    %
:- func (uint64::in) * (uint64::in) = (uint64::uo) is det.
:- func times(uint64, uint64) = uint64.

    % Maximum.
    %
:- func max(uint64, uint64) = uint64.

    % Minimum.
    %
:- func min(uint64, uint64) = uint64.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint64::in) div (uint64::in) = (uint64::uo) is det.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
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
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint64::in) mod (uint64::in) = (uint64::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (uint64::in) rem (uint64::in) = (uint64::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint64::in, uint64::in) = (uint64::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (uint64::in) << (int::in) = (uint64::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 64).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint64::in, int::in) = (uint64::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (uint64::in) >> (int::in) = (uint64::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 64).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint64::in, int::in) = (uint64::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint64::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint64::in) is semidet.

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

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(uint64) = uint64.

:- func max_uint64 = uint64.

    % Convert a uint64 to a pretty_printer.doc for formatting.
    %
:- func uint64_to_doc(uint64) = pretty_printer.doc.

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
    } else if ((uint64_t)I > (uint64_t)INT64_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (uint64_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (ulong) I;
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
    sorry($module, "uint64.from_int NYI for Erlang").

det_from_int(I) = U :-
    ( if from_int(I, U0) then
        U = U0
    else
        error("uint64.det_from_int: cannot convert int to uint64")
    ).

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "uint64.cast_from_int/1 NYI for Erlang").

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

:- pragma no_determinism_warning(cast_to_int/1).
cast_to_int(_) = _ :-
    sorry($module, "uint64.cast_to_int/1 NYI for Erlang").

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

:- pragma no_determinism_warning(cast_from_int64/1).
cast_from_int64(_) = _ :-
    sorry($module, "uint64.cast_from_int64/1 NYI for Erlang").

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
    U64 = (long)(Byte7 & 0xff) << 56 |
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
        Byte4::in, Byte5::in, Byte6::in, Byte7::in) = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = (ulong) (
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
    sorry($module, "uint64.from_bytes_le/8 NYI for Erlang").

%---------------------------------------------------------------------------%

from_bytes_be(Byte7, Byte6, Byte5,Byte4, Byte3, Byte2, Byte1, Byte0) =
    from_bytes_le(Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7).

%---------------------------------------------------------------------------%

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0u64 then
        throw(math.domain_error("uint64.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0u64 then
        throw(math.domain_error("uint64.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint64.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint64.(>>): second operand is out of range",
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
    (X /\ 1u64) = 0u64.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1u64) \= 0u64.

%---------------------------------------------------------------------------%

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
    B = ((A /\ 0x_0000_0000_0000_00ff_u64) << 56) \/
    ((A /\ 0x_0000_0000_0000_ff00_u64) << 40)     \/
    ((A /\ 0x_0000_0000_00ff_0000_u64) << 24)     \/
    ((A /\ 0x_0000_0000_ff00_0000_u64) << 8)      \/
    ((A /\ 0x_0000_00ff_0000_0000_u64) >> 8)      \/
    ((A /\ 0x_0000_ff00_0000_0000_u64) >> 24)     \/
    ((A /\ 0x_00ff_0000_0000_0000_u64) >> 40)     \/
    ((A /\ 0x_ff00_0000_0000_0000_u64) >> 56).

%---------------------------------------------------------------------------%

max_uint64 = 18_446_744_073_709_551_615_u64.

%---------------------------------------------------------------------------%

uint64_to_doc(X) = str(string.uint64_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint64.
%---------------------------------------------------------------------------%
