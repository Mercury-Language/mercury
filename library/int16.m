%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: int16.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with signed 16-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module int16.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % from_int(I, I16):
    % Convert an int to an int16.
    % Fails if I is not in [-32768, 32767].
    %
:- pred from_int(int::in, int16::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = int16.

:- func cast_from_int(int) = int16.

:- func cast_from_uint16(uint16) = int16.

:- func to_int(int16) = int.

%---------------------------------------------------------------------------%

    % Less than.
    %
:- pred (int16::in) < (int16::in) is semidet.

    % Greater than.
    %
:- pred (int16::in) > (int16::in) is semidet.

    % Less than or equal.
    %
:- pred (int16::in) =< (int16::in) is semidet.

    % Greater than or equal.
    %
:- pred (int16::in) >= (int16::in) is semidet.

%---------------------------------------------------------------------------%

    % Maximum.
    %
:- func max(int16, int16) = int16.

    % Minimum.
    %
:- func min(int16, int16) = int16.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int16::in) = (int16::uo) is det.

    % Unary minus.
    %
:- func - (int16::in) = (int16::uo) is det.

    % Addition.
    %
:- func int16 + int16 = int16.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(int16, int16) = int16.

    % Subtraction.
    %
:- func int16 - int16 = int16.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(int16, int16) = int16.

    % Multiplication.
    %
:- func (int16::in) * (int16::in) = (int16::uo) is det.
:- func times(int16, int16) = int16.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i16) div 3_i16 = (-4_i16).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int16::in) div (int16::in) = (int16::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i16) // 3_i16 = (-3_i16).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int16::in) // (int16::in) = (int16::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (int16::in) / (int16::in) = (int16::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int16::in, int16::in) = (int16::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int16::in) mod (int16::in) = (int16::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (int16::in) rem (int16::in) = (int16::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int16::in, int16::in) = (int16::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (int16::in) << (int::in) = (int16::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int16::in, int::in) = (int16::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 16).
    %
:- func (int16::in) >> (int::in) = (int16::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, 16).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int16::in, int::in) = (int16::uo) is det.

    % even(X) is equivalent to (X mod 2i16 = 0i16).
    %
:- pred even(int16::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2i16 = 1i16).
    %
:- pred odd(int16::in) is semidet.

    % Bitwise and.
    %
:- func (int16::in) /\ (int16::in) = (int16::uo) is det.

    % Bitwise or.
    %
:- func (int16::in) \/ (int16::in) = (int16::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int16, int16) = int16.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int16::in) = (int16::uo) is det.

:- func min_int16 = int16.

:- func max_int16 = int16.

    % Convert an int16 to a pretty_printer.doc for formatting.
    %
:- func int16_to_doc(int16) = pretty_printer.doc.

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

from_int(I, I16) :-
    I >= -32_768,
    I =< 32_767,
    I16 = cast_from_int(I).

det_from_int(I) = I16 :-
    ( if from_int(I, I16Prime) then
        I16 = I16Prime
    else
        error("int16.det_from_int: cannot convert int to int16")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I16 = (int16_t) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) I;
").

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "int16.cast_from_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint16(U16::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I16 = U16;
").

:- pragma foreign_proc("C#",
    cast_from_uint16(U16::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = (short) U16;
").

:- pragma foreign_proc("Java",
    cast_from_uint16(U16::in) = (I16::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I16 = U16;
").

:- pragma no_determinism_warning(cast_from_uint16/1).
cast_from_uint16(_) = _ :-
    sorry($module, "int16.cast_from_uint16/1 NYI for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(I16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = I16;
").

:- pragma foreign_proc("C#",
    to_int(I16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I16;
").

:- pragma foreign_proc("Java",
    to_int(I16::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = I16;
").

%---------------------------------------------------------------------------%

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

X div Y = Div :-
    Trunc = X // Y,
    ( if
        ( X >= cast_from_int(0), Y >= cast_from_int(0)
        ; X < cast_from_int(0), Y < cast_from_int(0)
        ; X rem Y = cast_from_int(0)
        )
    then
        Div = Trunc
    else
        Div = Trunc - cast_from_int(1)
    ).

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = cast_from_int(0) then
        throw(math.domain_error("int16.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X - (X div Y) * Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = cast_from_int(0) then
        throw(math.domain_error("int16.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int16.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 16u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int16.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ cast_from_int(1)) = cast_from_int(0).

:- pragma inline(odd/1).
odd(X) :-
    (X /\ cast_from_int(1)) \= cast_from_int(0).

%---------------------------------------------------------------------------%

min_int16 = cast_from_int(-32_768).

max_int16 = cast_from_int(32_767).

%---------------------------------------------------------------------------%

int16_to_doc(X) = str(string.int16_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int16.
%---------------------------------------------------------------------------%
