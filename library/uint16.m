%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: uint16.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with unsigned 16-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module uint16.
:- interface.

:- import_module pretty_printer.

%--------------------------------------------------------------------------%

    % from_int(I, U16):
    % Convert an int into a uint16.
    % Fails if I is not in [0, 65535].
    %
:- pred from_int(int::in, uint16::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = uint16.

:- func cast_from_int(int) = uint16.

:- func to_int(uint16) = int.

    % Less than.
    %
:- pred (uint16::in) < (uint16::in) is semidet.

    % Greater than.
    %
:- pred (uint16::in) > (uint16::in) is semidet.

    % Less than or equal.
    %
:- pred (uint16::in) =< (uint16::in) is semidet.

    % Greater than or equal.
    %
:- pred (uint16::in) >= (uint16::in) is semidet.

    % Addition.
    %
:- func uint16 + uint16 = uint16.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(uint16, uint16) = uint16.

    % Subtraction.
    %
:- func uint16 - uint16 = uint16.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(uint16, uint16) = uint16.

    % Multiplication.
    %
:- func (uint16::in) * (uint16::in) = (uint16::uo) is det.
:- func times(uint16, uint16) = uint16.

    % Maximum.
    %
:- func max(uint16, uint16) = uint16.

    % Minimum.
    %
:- func min(uint16, uint16) = uint16.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint16::in) div (uint16::in) = (uint16::uo) is det.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint16::in) // (uint16::in) = (uint16::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (uint16::in) / (uint16::in) = (uint16::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(uint16::in, uint16::in) = (uint16::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint16::in) mod (uint16::in) = (uint16::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (uint16::in) rem (uint16::in) = (uint16::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint16::in, uint16::in) = (uint16::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (uint16::in) << (int::in) = (uint16::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint16::in, int::in) = (uint16::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (uint16::in) >> (int::in) = (uint16::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint16::in, int::in) = (uint16::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint16::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint16::in) is semidet.

    % Bitwise and.
    %
:- func (uint16::in) /\ (uint16::in) = (uint16::uo) is det.

    % Bitwise or.
    %
:- func (uint16::in) \/ (uint16::in) = (uint16::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(uint16, uint16) = uint16.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (uint16::in) = (uint16::uo) is det.

    % Convert a uint16 to a pretty_printer.doc for formatting.
    %
:- func uint16_to_doc(uint16) = pretty_printer.doc.

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
    I =< 65_535,
    U8 = cast_from_int(I).

det_from_int(I) = U16 :-
    ( if from_int(I, U16Prime) then
        U16 = U16Prime
    else
        error("uint16.det_from_int: cannot convert int to uint16")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U16 = (uint16_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (ushort) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (U16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U16 = (short) I;
").

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "uint16.cast_from_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = U16;
").

:- pragma foreign_proc("C#",
    to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U16;
").

:- pragma foreign_proc("Java",
    to_int(U16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U16 & 0xffff;
").

:- pragma no_determinism_warning(to_int/1).
to_int(_) = _ :-
    sorry($module, "uint16.to_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = cast_from_int(0) then
        throw(math.domain_error("uint16.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = cast_from_int(0) then
        throw(math.domain_error("uint16.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint16.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint16.(>>): second operand is out of range",
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

uint16_to_doc(X) = str(string.uint16_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint16.
%---------------------------------------------------------------------------%
