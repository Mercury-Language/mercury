%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % from_int(I, U8):
    % Convert an int to a uint8.
    % Fails if I is not in [0, 255].
    %
:- pred from_int(int::in, uint8::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = uint8.

:- func cast_from_int(int) = uint8.

:- func to_int(uint8) = int.

%---------------------------------------------------------------------------%

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

    % Addition.
    %
:- func uint8 + uint8 = uint8.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(uint8, uint8) = uint8.

    % Subtraction.
    %
:- func uint8 - uint8 = uint8.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(uint8, uint8) = uint8.

    % Multiplication.
    %
:- func (uint8::in) * (uint8::in) = (uint8::uo) is det.
:- func times(uint8, uint8) = uint8.

    % Maximum.
    %
:- func max(uint8, uint8) = uint8.

    % Minimum.
    %
:- func min(uint8, uint8) = uint8.

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

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 8).
    %
:- func (uint8::in) << (int::in) = (uint8::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 8).
    % It will typically be be implemented more efficiently than X << Y.
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

    % even(X) is equivalent to (X mod 2i8 = 0i8).
    %
:- pred even(uint8::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2i8 = 1i8).
    %
:- pred odd(uint8::in) is semidet.

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

:- func max_uint8 = uint8.

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
        error("uint8.det_from_int: cannot convert int to uint8")
    ).

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "uint8.cast_from_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(to_int/1).
to_int(_) = _ :-
    sorry($module, "uint8.to_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0u8 then
        throw(math.domain_error("uint.'//': division by zero"))
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

%---------------------------------------------------------------------------%

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

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ 1u8) = 0u8.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1u8) \= 0u8.

%---------------------------------------------------------------------------%

max_uint8 = 255_u8.

%---------------------------------------------------------------------------%

uint8_to_doc(X) = str(string.uint8_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint8.
%---------------------------------------------------------------------------%
