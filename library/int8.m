%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2017-2018 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % from_int(I, I8):
    % Convert an int to an int8.
    % Fails if I is not in [-128, 127].
    %
:- pred from_int(int::in, int8::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = int8.

:- func cast_from_int(int) = int8.
:- func cast_from_uint8(uint8) = int8.

:- func to_int(int8) = int.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int8.min_int8.
    %
:- func abs(int8) = int8.

    % unchecked_abs(X) returns the absolute value of X, except that the result
    % is undefined if X = int8.min_int8.
    %
:- func unchecked_abs(int8) = int8.

    % nabs(X) returns the negative absolute value of X.
    % Unlike abs/1 this function is defined for X = int8.min_int8.
    %
:- func nabs(int8) = int8.

%---------------------------------------------------------------------------%

    % Maximum.
    %
:- func max(int8, int8) = int8.

    % Minimum.
    %
:- func min(int8, int8) = int8.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int8::in) = (int8::uo) is det.

    % Unary minus.
    %
:- func - (int8::in) = (int8::uo) is det.

    % Addition.
    %
:- func int8 + int8 = int8.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(int8, int8) = int8.

    % Subtraction.
    %
:- func int8 - int8 = int8.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(int8, int8) = int8.

    % Multiplication.
    %
:- func (int8::in) * (int8::in) = (int8::uo) is det.
:- func times(int8, int8) = int8.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i8) div 3_i8 = (-4_i8).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int8::in) div (int8::in) = (int8::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i8) // 3_i8 = (-3_i8).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
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
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int8::in) mod (int8::in) = (int8::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (int8::in) rem (int8::in) = (int8::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int8::in, int8::in) = (int8::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 8).
    %
:- func (int8::in) << (int::in) = (int8::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 8).
    % It will typically be be implemented more efficiently than X << Y.
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

    % even(X) is equivalent to (X mod 2i8 = 0i8).
    %
:- pred even(int8::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2i8 = 1i8).
    %
:- pred odd(int8::in) is semidet.

%---------------------------------------------------------------------------%

    % Bitwise complement.
    %
:- func \ (int8::in) = (int8::uo) is det.

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

%---------------------------------------------------------------------------%

:- func min_int8 = int8.

:- func max_int8 = int8.

%---------------------------------------------------------------------------%

    % Convert an int8 to a pretty_printer.doc for formatting.
    %
:- func int8_to_doc(int8) = pretty_printer.doc.

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

from_int(I, I8) :-
    I >= -128,
    I =< 127,
    I8 = cast_from_int(I).

det_from_int(I) = I8 :-
    ( if from_int(I, I8Prime) then
        I8 = I8Prime
    else
        error("int8.det_from_int: cannot convert int to int8")
    ).

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(cast_from_int/1).
cast_from_int(_) = _ :-
    sorry($module, "int8.cast_from_int/1 NYI for Erlang").

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

:- pragma no_determinism_warning(cast_from_uint8/1).
cast_from_uint8(_) = _ :-
    sorry($module, "int8.cast_from_uint8/1 NYI for Erlang").

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

%---------------------------------------------------------------------------%

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

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0i8 then
        throw(math.domain_error("int8.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X - (X div Y) * Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0i8 then
        throw(math.domain_error("int8.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 8u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int8.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 8u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int8.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

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

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ 1i8) = 0i8.

:- pragma inline(odd/1).
odd(X) :-
    (X /\ 1i8) \= 0i8.

%---------------------------------------------------------------------------%

min_int8 = -128_i8.

max_int8 = 127_i8.

%---------------------------------------------------------------------------%

int8_to_doc(X) = str(string.int8_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int8.
%---------------------------------------------------------------------------%
