%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: int64.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with signed 64-bit integer numbers.
%
%---------------------------------------------------------------------------%

:- module int64.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % from_int(I) = I64:
    % Convert an int to an int64.
    %
:- func from_int(int) = int64.

:- func cast_to_int(int64) = int.

:- func cast_from_uint64(uint64) = int64.

%---------------------------------------------------------------------------%

    % Less than.
    %
:- pred (int64::in) < (int64::in) is semidet.

    % Greater than.
    %
:- pred (int64::in) > (int64::in) is semidet.

    % Less than or equal.
    %
:- pred (int64::in) =< (int64::in) is semidet.

    % Greater than or equal.
    %
:- pred (int64::in) >= (int64::in) is semidet.

%---------------------------------------------------------------------------%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int64.min_int64.
    %
:- func abs(int64) = int64.

    % unchecked_abs(X) retursn the absolute value of X, except that if the
    % result is undefined if X = int64.min_int64.
    %
:- func unchecked_abs(int64) = int64.

    % nabs(X) retursn the negative absolute value of X.
    % Unlike abs/1 this function is defined for X = int64.min_int64.
    %
:- func nabs(int64) = int64.

%---------------------------------------------------------------------------%

    % Maximum.
    %
:- func max(int64, int64) = int64.

    % Minimum.
    %
:- func min(int64, int64) = int64.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int64::in) = (int64::uo) is det.

    % Unary minus.
    %
:- func - (int64::in) = (int64::uo) is det.

    % Addition.
    %
:- func int64 + int64 = int64.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(int64, int64) = int64.

    % Subtraction.
    %
:- func int64 - int64 = int64.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(int64, int64) = int64.

    % Multiplication.
    %
:- func (int64::in) * (int64::in) = (int64::uo) is det.
:- func times(int64, int64) = int64.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10_i64) div 3_i64 = (-4_i64).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int64::in) div (int64::in) = (int64::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10_i64) // 3_i64 = (-3_i64).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int64::in) // (int64::in) = (int64::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (int64::in) / (int64::in) = (int64::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int64::in, int64::in) = (int64::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (int64::in) mod (int64::in) = (int64::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
    %
:- func (int64::in) rem (int64::in) = (int64::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(int64::in, int64::in) = (int64::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (int64::in) << (int::in) = (int64::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, 64).
    % It will typically be be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int64::in, int::in) = (int64::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, 64).
    %
:- func (int64::in) >> (int::in) = (int64::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_int64).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int64::in, int::in) = (int64::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(int64::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(int64::in) is semidet.

    % Bitwise and.
    %
:- func (int64::in) /\ (int64::in) = (int64::uo) is det.

    % Bitwise or.
    %
:- func (int64::in) \/ (int64::in) = (int64::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int64, int64) = int64.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int64::in) = (int64::uo) is det.

:- func min_int64 = int64.

:- func max_int64 = int64.

    % Convert a int64 to a pretty_printer.doc for formatting.
    %
:- func int64_to_doc(int64) = pretty_printer.doc.

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
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = (int64_t) A;
").

:- pragma foreign_proc("C#",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (long) A; // Mercury's 'int' type in the C# grade is 32-bits.
").

:- pragma foreign_proc("Java",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (long) A; // Mercury's 'int' type in the Java grade is 32-bits.
").

:- pragma no_determinism_warning(from_int/1).
from_int(_) = _ :-
    sorry($module, "NYI int64.from_int for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int(I64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = (MR_Integer) I64;
").

:- pragma foreign_proc("C#",
    cast_to_int(I64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) I64;
").

:- pragma foreign_proc("Java",
    cast_to_int(I64::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) I64;
").

:- pragma no_determinism_warning(cast_to_int/1).
cast_to_int(_) = _ :-
    sorry($module, "NYI int64.cast_to_int for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint64(U64::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = (int64_t) U64;
").

:- pragma foreign_proc("C#",
    cast_from_uint64(U64::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) U64;
").

:- pragma foreign_proc("Java",
    cast_from_uint64(U64::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I64 = (long) U64;
").

:- pragma no_determinism_warning(cast_from_uint64/1).
cast_from_uint64(_) = _ :-
    sorry($module, "NYI int64.cast_from_uint64 for Erlang").

%---------------------------------------------------------------------------%

abs(Num) = Abs :-
    ( if Num = int64.min_int64 then
        throw(software_error("int64.abs: abs(min_int64) would overflow"))
    else
        Abs = unchecked_abs(Num)
    ).

unchecked_abs(Num) =
    ( if Num < from_int(0) then
        from_int(0) - Num
    else
        Num
    ).

nabs(Num) =
    ( if Num > from_int(0) then
        -Num
    else
        Num
    ).

%---------------------------------------------------------------------------%

X div Y = Div :-
    Trunc = X // Y,
    ( if
        ( X >= from_int(0), Y >= from_int(0)
        ; X < from_int(0), Y < from_int(0)
        ; X rem Y = from_int(0)
        )
    then
        Div = Trunc
    else
        Div = Trunc - from_int(1)
    ).

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = from_int(0) then
        throw(math.domain_error("int64.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X  - (X div Y) * Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = from_int(0) then
        throw(math.domain_error("int64.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "int64.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < 64u then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "int64.(>>): second operand is out of range",
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
    (X /\ from_int(1)) = from_int(0).

:- pragma inline(odd/1).
odd(X) :-
    (X /\ from_int(1)) \= from_int(0).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min_int64 = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = INT64_MIN;
").

:- pragma foreign_proc("C#",
    min_int64 = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = System.Int64.MinValue;
").

:- pragma foreign_proc("Java",
    min_int64 = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = java.lang.Long.MIN_VALUE;
").

:- pragma no_determinism_warning(min_int64/0).
min_int64 = _ :-
    sorry($module, "NYI min_int64/9 for Erlang").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_int64 = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = INT64_MAX;
").

:- pragma foreign_proc("C#",
    max_int64 = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = System.Int64.MaxValue;
").

:- pragma foreign_proc("Java",
    max_int64 = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I64 = java.lang.Long.MAX_VALUE;
").

:- pragma no_determinism_warning(max_int64/0).
max_int64 = _ :-
    sorry($module, "NYI max_int64/9 for Erlang").

%---------------------------------------------------------------------------%

int64_to_doc(X) = str(string.int64_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module int64.
%---------------------------------------------------------------------------%
