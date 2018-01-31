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

%---------------------------------------------------------------------------%

    % from_int(I, U64):
    % Convert an int into a uint64.
    % Fails if I is not in [0, 2^63 - 1].
    %
:- pred from_int(int::in, uint64::out) is semidet.

:- func det_from_int(int) = uint64.

:- func cast_from_int(int) = uint64.

:- func cast_to_int(uint64) = int.

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
    % It will typically be be implemented more efficiently than X << Y.
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

:- func max_uint64 = uint64.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module math.
:- import_module require.
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
    ( if from_int(I, U0)
    then U = U0
    else error("uint64.det_from_int: cannot convert int to uint64")
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

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = cast_from_int(0) then
        throw(math.domain_error("uint64.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = cast_from_int(0) then
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
    (X /\ cast_from_int(1)) = cast_from_int(0).

:- pragma inline(odd/1).
odd(X) :-
    (X /\ cast_from_int(1)) \= cast_from_int(0).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_uint64 = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U64 = UINT64_MAX;
").

:- pragma foreign_proc("C#",
    max_uint64 = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = ulong.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint64 = (U64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = 0xffffffffffffffffL;
").

:- pragma no_determinism_warning(max_uint64/0).
max_uint64 = _ :-
    sorry($module, "uint64.max_uint64/1 NYI for Erlang").

%---------------------------------------------------------------------------%
