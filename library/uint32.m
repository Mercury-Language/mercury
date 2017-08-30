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

:- func cast_to_int(uint32) = int.

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
    } else if (I > (MR_Integer) INT32_MAX) {
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
    max_uint32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_MAX;
").

:- pragma foreign_proc("C#",
    max_uint32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = uint.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0xffffffff;
").

:- pragma no_determinism_warning(max_uint32/0).
max_uint32 = _ :-
    sorry($module, "uint32.max_uint32/0 NYI for Erlang").

%---------------------------------------------------------------------------%

uint32_to_doc(X) = str(string.uint32_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint32.
%---------------------------------------------------------------------------%
