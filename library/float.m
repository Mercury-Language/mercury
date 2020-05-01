%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001-2008,2010, 2012 The University of Melbourne.
% Copyright (C) 2013-2016, 2018-2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: float.m.
% Main author: fjh.
% Stability: medium.
%
% Floating point support.
%
% Floats are double precision, except in .spf grades where they
% are single precision.
%
% Note that implementations which support IEEE floating point
% should ensure that in cases where the only valid answer is a "NaN"
% (the IEEE float representation for "not a number"), the det
% functions here will halt with a runtime error (or throw an exception)
% rather than returning a NaN. Quiet (non-signalling) NaNs have a
% semantics which is not valid in Mercury, since they don't obey the
% axiom "all [X] X = X".
%
% XXX Unfortunately the current Mercury implementation does not
% do that on all platforms, since neither ANSI C nor POSIX provide
% any portable way of ensuring that floating point operations
% whose result is not representable will raise a signal rather
% than returning a NaN. (Maybe C9X will help...?)
% The behaviour is correct on Linux and Digital Unix,
% but not on Solaris, for example.
%
% IEEE floating point also specifies that some functions should
% return different results for +0.0 and -0.0, but that +0.0 and -0.0
% should compare equal. This semantics is not valid in Mercury,
% since it doesn't obey the axiom `all [F, X, Y] X = Y => F(X) = F(Y)'.
% Again, the resolution is that in Mercury, functions which would
% return different results for +0.0 and -0.0 should instead halt
% execution with a run-time error (or throw an exception).
%
% XXX Here too the current Mercury implementation does not
% implement the intended semantics correctly on all platforms.
%
% XXX On machines such as x86 which support extra precision
% for intermediate results, the results may depend on the
% level of optimization, in particular inlining and evaluation
% of constant expressions.
% For example, the goal `1.0/9.0 = std_util.id(1.0)/9.0' may fail.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module float.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%
%
% Arithmetic functions.
%

    % Addition.
    %
:- func (float::in) + (float::in) = (float::uo) is det.

    % Subtraction.
    %
:- func (float::in) - (float::in) = (float::uo) is det.

    % Multiplication.
    %
:- func (float::in) * (float::in) = (float::uo) is det.

    % Division.
    % Throws a `domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % this check.
    %
:- func (float::in) / (float::in) = (float::uo) is det.

    % unchecked_quotient(X, Y) is the same as X / Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(float::in, float::in) = (float::uo) is det.

    % Unary plus.
    %
:- func + (float::in) = (float::uo) is det.

    % Unary minus.
    %
:- func - (float::in) = (float::uo) is det.

%---------------------------------------------------------------------------%
%
% Comparison predicates.
%

    % Less than.
    %
:- pred (float::in) < (float::in) is semidet.

    % Less than or equal.
    %
:- pred (float::in) =< (float::in) is semidet.

    % Greater than or equal.
    %
:- pred (float::in) >= (float::in) is semidet.

    % Greater than.
    %
:- pred (float::in) > (float::in) is semidet.

%---------------------------------------------------------------------------%
%
% Conversion from integer types.
%

    % Convert an int into float.
    %
    % The behaviour when the int exceeds the range of what can be exactly
    % represented by a float is undefined.
    %
:- func float(int) = float.

    % Convert a signed 8-bit integer into a float.
    % Always succeeds as all signed 8-bit integers have an exact
    % floating-point representation.
    %
:- func from_int8(int8) = float.

    % Convert a signed 16-bit integer into a float.
    % Always succeeds as all signed 16-bit integers have an exact
    % floating-point representation.
    %
:- func from_int16(int16) = float.

    % Convert a signed 32-bit integer into a float.
    % The behaviour when the integer exceeds the range of what can be
    % exactly represented by a float is undefined.
    %
:- func cast_from_int32(int32) = float.

    % Convert a signed 64-bit integer into a float.
    % The behaviour when the integer exceeds the range of what can be
    % exactly represented by a float is undefined.
    %
:- func cast_from_int64(int64) = float.

    % Convert an unsigned 8-bit integer into a float.
    % Always succeeds as all unsigned 8-bit integers have an exact
    % floating-point representation.
    %
:- func from_uint8(uint8) =  float.

    % Convert an unsigned 16-bit integer into a float.
    % Always succeeds as all unsigned 16-bit integers have an exact
    % floating-point representation.
    %
:- func from_uint16(uint16) = float.

    % Convert an unsigned 32-bit integer into a float.
    % The behaviour when the integer exceeds the range of what can be
    % exactly represented by a float is undefined.
    %
:- func cast_from_uint32(uint32) = float.

    % Convert an unsigned 64-bit integer into a float.
    % The behaviour when the integer exceeds the range of what can be
    % exactly represented by a float is undefined.
    %
:- func cast_from_uint64(uint64) = float.

%---------------------------------------------------------------------------%
%
% Conversion to int.
%

    % ceiling_to_int(X) returns the smallest integer not less than X.
    %
:- func ceiling_to_int(float) = int.

    % floor_to_int(X) returns the largest integer not greater than X.
    %
:- func floor_to_int(float) = int.

    % round_to_int(X) returns the integer closest to X.
    % If X has a fractional value of 0.5, it is rounded up.
    %
:- func round_to_int(float) = int.

    % truncate_to_int(X) returns the integer closest to X such that
    % |truncate_to_int(X)| =< |X|.
    %
:- func truncate_to_int(float) = int.

%---------------------------------------------------------------------------%
%
% Miscellaneous functions.
%

    % Absolute value.
    %
:- func abs(float) = float.

    % Maximum.
    %
:- func max(float, float) = float.

    % Minimum.
    %
:- func min(float, float) = float.

    % pow(Base, Exponent) returns Base raised to the power Exponent.
    % Fewer domain restrictions than math.pow: works for negative Base,
    % and pow(B, 0) = 1.0 for all B, even B=0.0.
    % Only pow(0, <negative>) throws a `domain_error' exception.
    %
:- func pow(float, int) = float.

    % Compute a non-negative integer hash value for a float.
    %
:- func hash(float) = int.
:- pred hash(float::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Classification.
%

    % True iff the argument is of infinite magnitude.
    %
:- pred is_infinite(float::in) is semidet.

    % Synonym for the above.
    %
:- pred is_inf(float::in) is semidet.

    % True iff the argument is not-a-number (NaN).
    %
:- pred is_nan(float::in) is semidet.

    % True iff the argument is of infinite magnitude or not-a-number (NaN).
    %
:- pred is_nan_or_infinite(float::in) is semidet.

    % Synonym for the above.
    %
:- pred is_nan_or_inf(float::in) is semidet.

    % True iff the argument is not of infinite magnitude and is not a
    % not-a-number (NaN) value.
    %
:- pred is_finite(float::in) is semidet.

    % True iff the argument is of zero magnitude.
    %
:- pred is_zero(float::in) is semidet.

%---------------------------------------------------------------------------%
%
% System constants.
%

    % Maximum finite floating-point number.
    %
    % max = (1 - radix ** mantissa_digits) * radix ** max_exponent
    %
:- func max = float.

    % Minimum normalised positive floating-point number.
    %
    % min = radix ** (min_exponent - 1)
    %
:- func min = float.

    % Positive infinity.
    %
:- func infinity = float.

    % Smallest number x such that 1.0 + x \= 1.0.
    % This represents the largest relative spacing of two consecutive floating
    % point numbers.
    %
    % epsilon = radix ** (1 - mantissa_digits)
    %
:- func epsilon = float.

    % Radix of the floating-point representation.
    % In the literature, this is sometimes referred to as `b'.
    %
:- func radix = int.

    % The number of base-radix digits in the mantissa.
    % In the literature, this is sometimes referred to as `p' or `t'.
    %
:- func mantissa_digits = int.

    % Minimum negative integer such that:
    %   radix ** (min_exponent - 1)
    % is a normalised floating-point number. In the literature,
    % this is sometimes referred to as `e_min'.
    %
:- func min_exponent = int.

    % Maximum integer such that:
    %   radix ** (max_exponent - 1)
    % is a normalised floating-point number.
    % In the literature, this is sometimes referred to as `e_max'.
    %
:- func max_exponent = int.

%---------------------------------------------------------------------------%

    % Convert a float to a pretty_printer.doc for formatting.
    %
:- func float_to_doc(float) = doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

% These functions are hidden for now. `int' is not guaranteed to be able to
% represent a double, so we return strings for now. Endianness and treatment
% of special values also needs to be considered.

    % Convert a float to an IEEE single-precision floating point value, then
    % return the integer representation of the bit layout of that value as a
    % string.
    %
:- func float32_bits_string(float::in) = (string::uo) is det.

    % Convert a float to an IEEE double-precision floating point value, then
    % return the integer representation of the bit layout of that value as a
    % string.
    %
:- func float64_bits_string(float::in) = (string::uo) is det.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module string.

%
% Header files of mathematical significance.
%

:- pragma foreign_decl("C", "
    #include <float.h>
    #include <math.h>

#ifdef MR_HAVE_IEEEFP_H
    #include <ieeefp.h>
#endif
").

%---------------------------------------------------------------------------%

% The other arithmetic and comparison operators are builtins,
% which the compiler expands inline. We don't need to define them here.

:- pragma inline('/'/2).
X / Y = Z :-
    ( if float_domain_checks, Y = 0.0 then
        throw(domain_error("float.'/': division by zero"))
    else
        Z = unchecked_quotient(X, Y)
    ).

    % This code is included here rather than just calling the version in
    % math.m because we currently don't do transitive inter-module inlining,
    % so code which uses `/'/2 but doesn't import math.m couldn't have the
    % domain check optimized away.
    %
:- pred float_domain_checks is semidet.
:- pragma inline(float_domain_checks/0).

:- pragma foreign_proc("C",
    float_domain_checks,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#ifdef ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = MR_FALSE;
#else
    SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("C#",
    float_domain_checks,
    [thread_safe, promise_pure],
"
#if ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = false;
#else
    SUCCESS_INDICATOR = true;
#endif
").

:- pragma foreign_proc("Java",
    float_domain_checks,
    [thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Erlang",
    float_domain_checks,
    [thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true
").

%---------------------------------------------------------------------------%
%
% Conversion from integer types.
%

%   For Java, overflows are not detected, so this must be tested for
%   explicitly. So every time there's a cast to int, the bounds are
%   checked first.

:- pragma foreign_proc("C",
    float(IntVal::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = IntVal;
").

:- pragma foreign_proc("C#",
    float(IntVal::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) IntVal;
").

:- pragma foreign_proc("Java",
    float(IntVal::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) IntVal;
").

:- pragma foreign_proc("Erlang",
    float(IntVal::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(IntVal)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int8(Int8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = Int8Val;
").

:- pragma foreign_proc("C#",
    from_int8(Int8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int8Val;
").

:- pragma foreign_proc("Java",
    from_int8(Int8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int8Val;
").

:- pragma foreign_proc("Erlang",
    from_int8(Int8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(Int8Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_uint8(UInt8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = UInt8Val;
").

:- pragma foreign_proc("C#",
    from_uint8(UInt8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) UInt8Val;
").

:- pragma foreign_proc("Java",
    from_uint8(UInt8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) (UInt8Val & 0xff);
").

:- pragma foreign_proc("Erlang",
    from_uint8(UInt8Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(UInt8Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int16(Int16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = Int16Val;
").

:- pragma foreign_proc("C#",
    from_int16(Int16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int16Val;
").

:- pragma foreign_proc("Java",
    from_int16(Int16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int16Val;
").

:- pragma foreign_proc("Erlang",
    from_int16(Int16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(Int16Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_uint16(UInt16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = UInt16Val;
").

:- pragma foreign_proc("C#",
    from_uint16(UInt16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) UInt16Val;
").

:- pragma foreign_proc("Java",
    from_uint16(UInt16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) (UInt16Val & 0xffff);
").

:- pragma foreign_proc("Erlang",
    from_uint16(UInt16Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(UInt16Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int32(Int32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = Int32Val;
").

:- pragma foreign_proc("C#",
    cast_from_int32(Int32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int32Val;
").

:- pragma foreign_proc("Java",
    cast_from_int32(Int32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int32Val;
").

:- pragma foreign_proc("Erlang",
    cast_from_int32(Int32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(Int32Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint32(UInt32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = UInt32Val;
").

:- pragma foreign_proc("C#",
    cast_from_uint32(UInt32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) UInt32Val;
").

:- pragma foreign_proc("Java",
    cast_from_uint32(UInt32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) (UInt32Val & 0xffffffff);
").

:- pragma foreign_proc("Erlang",
    cast_from_uint32(UInt32Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(UInt32Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int64(Int64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = Int64Val;
").

:- pragma foreign_proc("C#",
    cast_from_int64(Int64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int64Val;
").

:- pragma foreign_proc("Java",
    cast_from_int64(Int64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) Int64Val;
").

:- pragma foreign_proc("Erlang",
    cast_from_int64(Int64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(Int64Val)
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_uint64(UInt64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    FloatVal = UInt64Val;
").

:- pragma foreign_proc("C#",
    cast_from_uint64(UInt64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) UInt64Val;
").

:- pragma foreign_proc("Java",
    cast_from_uint64(UInt64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = (double) UInt64Val;
").

:- pragma foreign_proc("Erlang",
    cast_from_uint64(UInt64Val::in) = (FloatVal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatVal = float(UInt64Val)
").

%---------------------------------------------------------------------------%
%
% Conversion to ints.
%

:- pragma foreign_proc("C",
    ceiling_to_int(X::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Ceil = (MR_Integer) ML_FLOAT_CEIL(X);
").
:- pragma foreign_proc("C#",
    ceiling_to_int(X::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ceil = System.Convert.ToInt32(System.Math.Ceiling(X));
").
:- pragma foreign_proc("Java",
    ceiling_to_int(X::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (X > (double) java.lang.Integer.MAX_VALUE ||
        X <= (double) java.lang.Integer.MIN_VALUE - 1)
    {
        throw new java.lang.RuntimeException(
            ""Overflow converting floating point to int"");
    } else {
        Ceil = (int) java.lang.Math.ceil(X);
    }
").
:- pragma foreign_proc("Erlang",
    ceiling_to_int(X::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = erlang:trunc(X),
    case (X - T) > 0 of
        true  ->
            Ceil = T + 1;
        false ->
            Ceil = T
    end
").

:- pragma foreign_proc("C",
    floor_to_int(X::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Floor = (MR_Integer) ML_FLOAT_FLOOR(X);
").
:- pragma foreign_proc("C#",
    floor_to_int(X::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Floor = System.Convert.ToInt32(System.Math.Floor(X));
").
:- pragma foreign_proc("Java",
    floor_to_int(X :: in) = (Floor :: out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (X >= (double) java.lang.Integer.MAX_VALUE + 1 ||
        X < (double) java.lang.Integer.MIN_VALUE)
    {
        throw new java.lang.RuntimeException(
            ""Overflow converting floating point to int"");
    } else {
        Floor = (int) java.lang.Math.floor(X);
    }
").
:- pragma foreign_proc("Erlang",
    floor_to_int(X::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = erlang:trunc(X),
    case (X - T) < 0 of
        true ->
            Floor = T - 1;
        false ->
            Floor = T
    end
").

:- pragma foreign_proc("C",
    round_to_int(X::in) = (Round::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Round = (MR_Integer) ML_FLOAT_FLOOR(X + (MR_Float)0.5);
").
:- pragma foreign_proc("C#",
    round_to_int(X::in) = (Round::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Round = System.Convert.ToInt32(System.Math.Floor(X + 0.5));
").
:- pragma foreign_proc("Java",
    round_to_int(X::in) = (Round::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (X >= (double) java.lang.Integer.MAX_VALUE + 0.5 ||
        X < (double) java.lang.Integer.MIN_VALUE - 0.5)
    {
        throw new java.lang.RuntimeException(
            ""Overflow converting floating point to int"");
    } else {
        Round = (int) java.lang.Math.round(X);
    }
").
:- pragma foreign_proc("Erlang",
    round_to_int(X::in) = (Round::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX erlang:round returns the closest integer to x, rounding to even when
    % x is halfway between two integers.
    Round = erlang:round(X)
").

:- pragma foreign_proc("C",
    truncate_to_int(X::in) = (Trunc::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Trunc = (MR_Integer) X;
").
:- pragma foreign_proc("C#",
    truncate_to_int(X::in) = (Trunc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Trunc = System.Convert.ToInt32(X);
").
:- pragma foreign_proc("Java",
    truncate_to_int(X::in) = (Trunc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (X >= (double) java.lang.Integer.MAX_VALUE + 1 ||
        X <= (double) java.lang.Integer.MIN_VALUE - 1)
    {
        throw new java.lang.RuntimeException(
            ""Overflow converting floating point to int"");
    } else {
        Trunc = (int) X;
    }
").
:- pragma foreign_proc("Erlang",
    truncate_to_int(X::in) = (Trunc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Trunc = erlang:trunc(X)
").

%---------------------------------------------------------------------------%
%
% Miscellaneous functions.
%

abs(Num) = Abs :-
    ( if Num =< 0.0 then
        Abs = - Num
    else
        Abs = Num
    ).

max(X, Y) = Max :-
    ( if X >= Y then
        Max = X
    else
        Max = Y
    ).

min(X, Y) = Min :-
    ( if X =< Y then
        Min = X
    else
        Min = Y
    ).

pow(Base, Exp) = Ans :-
    ( if Exp >= 0 then
        Ans = multiply_by_pow(1.0, Base, Exp)
    else
        ( if float_domain_checks, Base = 0.0 then
            throw(domain_error("float.pow: zero base"))
        else
            Ans = unchecked_quotient(1.0,
                multiply_by_pow(1.0, Base, -Exp))
            % See below re use of unchecked_quotient.
        )
    ).

    % Returns Scale0 * (Base ** Exp) (where X ** 0 == 1.0 for all X).
    % Requires that Exp >= 0.
    % Uses a simple "Russian peasants" algorithm. O(lg(Exp+1)).
    %
:- func multiply_by_pow(float, float, int) = float.

multiply_by_pow(Scale0, Base, Exp) = Result :-
    ( if Exp = 0 then
        Result = Scale0
    else
        ( if odd(Exp) then
            Scale1 = Scale0 * Base
        else
            Scale1 = Scale0
        ),
        Result = multiply_by_pow(Scale1, Base * Base, Exp div 2)
    ).

    % The reason for using unchecked_quotient in float.pow is so
    % that float.pow(+/-0.5, -1111) gives +/-infinity rather than
    % a domain error. (N.B. This relies on unchecked_quotient(1.0,
    % +/-0.0) giving +/-infinity, whereas the documentation in
    % float.m says that the results are undefined.)
    % Using Result = float.multiply_by_pow(1.0, 1.0 / Base, -Exp)
    % would give the right behaviour for underflow, but isn't
    % generally as accurate.

    % (Efficiency note: An optimization used by `power' in SGI's STL
    % implementation is to test for Exp=0 and (for non-zero Exp) handle
    % low zero bits in Exp before calling this loop: the loop for the low
    % zero bits needs only square Base, it needn't update Acc until the
    % end of that loop at which point Acc can be simply assigned from the
    % then-current value of Base. This optimization would be especially
    % valuable for expensive `*' operations; maybe provide a
    % std_util.monoid_pow(func(T,T)=T MonoidOperator, T Identity, int
    % Exp, T Base) = T Result function to complement the existing
    % std_util.pow function.)

%---------------------------------------------------------------------------%

    % In hashing a float in .NET or Java, we ensure that the value is
    % non-negative, as this condition is not guaranteed by either API.
:- pragma foreign_proc("C",
    hash(F::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    H = MR_hash_float(F);
").
:- pragma foreign_proc("C#",
    hash(F::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int Code = F.GetHashCode();
    if (Code < 0) {
        H = -Code ^ 1;
    } else {
        H = Code;
    }
").
:- pragma foreign_proc("Java",
    hash(F::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int Code = (new java.lang.Double(F)).hashCode();
    if (Code < 0) {
        H = -Code ^ 1;
    } else {
        H = Code;
    }
").
:- pragma foreign_proc("Erlang",
    hash(F::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    H = erlang:phash2(F)
").

hash(F, H) :-
    H = hash(F).

%---------------------------------------------------------------------------%

is_infinite(F) :-
   is_inf(F).

is_nan_or_infinite(Float) :-
    is_nan_or_inf(Float).

is_nan_or_inf(Float) :-
    ( is_nan(Float)
    ; is_inf(Float)
    ).

:- pragma foreign_proc("C",
    is_nan(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = MR_is_nan(Flt);
").
:- pragma foreign_proc("C#",
    is_nan(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = System.Double.IsNaN(Flt);
").
:- pragma foreign_proc("Java",
    is_nan(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Double.isNaN(Flt);
").
:- pragma foreign_proc("Erlang",
    is_nan(_Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX NYI
    SUCCESS_INDICATOR = false
").

:- pragma foreign_proc("C",
    is_inf(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = MR_is_infinite(Flt);
").
:- pragma foreign_proc("C#",
    is_inf(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = System.Double.IsInfinity(Flt);
").
:- pragma foreign_proc("Java",
    is_inf(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Double.isInfinite(Flt);
").
:- pragma foreign_proc("Erlang",
    is_inf(_Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX NYI
    SUCCESS_INDICATOR = false
").

:- pragma foreign_proc("C",
    is_finite(Flt::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = MR_is_finite(Flt);
").

is_finite(F) :-
    not is_infinite(F),
    not is_nan(F).

is_zero(0.0).

%---------------------------------------------------------------------------%
%
% System constants.
%

% For C, the floating-point system constants are derived from <float.h> and
% implemented using the C interface.
%
% For .NET (and Java), the values are mostly hard-coded.
% It's OK to do this, because the ECMA specification for the .NET CLR
% nails down the representation of System.Double as 64-bit IEEE float.
% The Java Language Specification also follows the IEEE standard.
%
% Many of these values are coded in Mercury clauses,
% rather than foreign_proc pragmas; assuming IEEE floating point
% is a reasonable default these days, and doing that might improve
% the compiler's optimization.

:- pragma foreign_decl("C",
"
    #define ML_FLOAT_RADIX  FLT_RADIX   // There is no DBL_RADIX.

    #if defined(MR_USE_SINGLE_PREC_FLOAT)
        #define ML_FLOAT_MAX        FLT_MAX
        #define ML_FLOAT_MIN        FLT_MIN
        #define ML_FLOAT_EPSILON    FLT_EPSILON
        #define ML_FLOAT_MANT_DIG   FLT_MANT_DIG
        #define ML_FLOAT_MIN_EXP    FLT_MIN_EXP
        #define ML_FLOAT_MAX_EXP    FLT_MAX_EXP
    #else
        #define ML_FLOAT_MAX        DBL_MAX
        #define ML_FLOAT_MIN        DBL_MIN
        #define ML_FLOAT_EPSILON    DBL_EPSILON
        #define ML_FLOAT_MANT_DIG   DBL_MANT_DIG
        #define ML_FLOAT_MIN_EXP    DBL_MIN_EXP
        #define ML_FLOAT_MAX_EXP    DBL_MAX_EXP
    #endif

    #if defined(MR_USE_SINGLE_PREC_FLOAT)
        #define ML_FLOAT_FLOOR(X)   floorf(X)
        #define ML_FLOAT_CEIL(X)    ceilf(X)
    #else
        #define ML_FLOAT_FLOOR(X)   floor(X)
        #define ML_FLOAT_CEIL(X)    ceil(X)
    #endif
").

:- pragma foreign_proc("C",
    max = (Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Max = ML_FLOAT_MAX;
").
:- pragma foreign_proc("C#",
    max = (Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = System.Double.MaxValue;
").
:- pragma foreign_proc("Java",
    max = (Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = java.lang.Double.MAX_VALUE;
").
:- pragma foreign_proc("Erlang",
    max = (Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = 1.797693E+308
").

:- pragma foreign_proc("C",
    min = (Min::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Min = ML_FLOAT_MIN;
").
% C#:
%   We can't use System.Double.MinValue, because in v1 of the .NET CLR
%   that means something completely different: the negative number
%   with the greatest absolute value.
%   Instead, we just hard-code the appropriate value (copied from the
%   glibc header files); this is OK, because the ECMA specification
%   nails down the representation of double as 64-bit IEEE.
%
% Java:
%   The Java API's java.lang.Double.MIN_VALUE is not normalized,
%   so can't be used, so we use the same constant as for .NET, as the
%   Java Language Specification also describes Java doubles as 64 bit IEEE.
%
min = 2.2250738585072014e-308.

% Java and C# provide constants for +infinity.
% For C, the situation is more complicated.
% For single precision floats, we use the following
% (1) the C99 INFINITY macro if available
% (2) the HUGE_VALF macro if available
% (3) HUGE_VAL
%
% For double precision floats we just use the HUGE_VAL macro.
% The C standard does not guarantee that this will be +infinity,
% but it actually is on all the systems that we support. It will definitely be
% +infinity if the optional annex F of C99 is supported or if the 2001
% revision of POSIX is supported.

:- pragma foreign_proc("C",
    infinity = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    #if defined(MR_USE_SINGLE_PREC_FLOAT)
        #if defined(INFINITY)
            F = INFINITY;
        #elif defined(HUGE_VALF)
            F = HUGE_VALF;
        #else
            F = HUGE_VAL;
        #endif
    #else
        F = HUGE_VAL;
    #endif
").

:- pragma foreign_proc("Java",
    infinity = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = java.lang.Double.POSITIVE_INFINITY;
").

:- pragma foreign_proc("C#",
    infinity = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = System.Double.PositiveInfinity;
").

infinity = _ :-
    private_builtin.sorry(
        "infinity/0 not currently available for this backend").

:- pragma foreign_proc("C",
    epsilon = (Eps::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Eps = ML_FLOAT_EPSILON;
").
% C#:
%   We can't use System.Double.Epsilon, because in v1 of the .NET CLR,
%   that means something completely different: the smallest (denormal)
%   positive number. I don't know what the people who designed that
%   were smoking; that semantics for 'epsilon' is different from the
%   use of 'epsilon' in C, Lisp, Ada, etc., not to mention Mercury.
%   Instead, we just hard-code the appropriate value (copied from the
%   glibc header files); this is OK, because the ECMA specification
%   nails down the representation of double as 64-bit IEEE.
%
% Java:
%   The Java API doesn't provide an epsilon constant, so we use the
%   same constant, which is ok since Java defines doubles as 64 bit IEEE.
%
epsilon = 2.2204460492503131e-16.

:- pragma foreign_proc("C",
    radix = (Radix::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Radix = ML_FLOAT_RADIX;
").
% C#:
%   The ECMA specification requires that double be 64-bit IEEE.
%   I think that implies that it must have Radix = 2.
%   This is definitely right for x86, anyway.
%
% Java:
%   The Java API doesn't provide this constant either, so we default to the
%   same constant as .NET, which is ok since Java defines doubles as 64 bit
%   IEEE.
%
radix = 2.

:- pragma foreign_proc("C",
    mantissa_digits = (MantDig::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    MantDig = ML_FLOAT_MANT_DIG;
").
% C#:
%   ECMA specifies that System.Double is 64-bit IEEE float
%
% Java:
%   The Java API doesn't provide this constant either, so we default to the
%   same constant as .NET, which is ok since Java defines doubles as 64 bit
%   IEEE.
%
mantissa_digits = 53.

:- pragma foreign_proc("C",
    min_exponent = (MinExp::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    MinExp = ML_FLOAT_MIN_EXP;
").
% C#:
%   ECMA specifies that System.Double is 64-bit IEEE float
%
% Java:
%   The Java API doesn't provide this constant either, so we default to the
%   same constant as .NET, which is ok since Java defines doubles as 64 bit
%   IEEE.
%
min_exponent = -1021.

:- pragma foreign_proc("C",
    max_exponent = (MaxExp::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    MaxExp = ML_FLOAT_MAX_EXP;
").
% C#:
%   ECMA specifies that System.Double is 64-bit IEEE float
%
% Java:
%   The Java API doesn't provide this constant either, so we default to the
%   same constant as .NET, which is ok since Java defines doubles as 64 bit
%   IEEE.
%
max_exponent = 1024.

    % Convert a float to a pretty_printer.doc.
    %
float_to_doc(X) = str(string.float_to_string(X)).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    float32_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    union {
        float f;
        MR_int_least32_t i;
    } u;
    char buf[64];

    u.f = (float) Flt;
    sprintf(buf, ""%d"", u.i);
    MR_make_aligned_string_copy(Str, buf);
").

:- pragma foreign_proc("Java",
    float32_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    float f = (float) Flt;
    int i = Float.floatToIntBits(f);
    Str = Integer.toString(i);
").

:- pragma foreign_proc("C#",
    float32_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    float f = (float) Flt;
    int i = System.BitConverter.ToInt32(System.BitConverter.GetBytes(f), 0);
    Str = i.ToString();
").

:- pragma foreign_proc("Erlang",
    float32_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<Int:32/signed-integer>> = <<Flt:32/float>>,
    Str = integer_to_list(Int)
").

:- pragma foreign_proc("C",
    float64_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    #if defined(MR_INT_LEAST64_TYPE)

        union {
            double f;
            MR_int_least64_t i;
        } u;
        char buf[64];

        u.f = (double) Flt;
        #if defined(MR_MINGW64) || defined(MR_CYGWIN32)
            sprintf(buf, ""%lld"", u.i);
        #elif defined(MR_WIN32)
            // The I64 size prefix is specific to the Microsoft C library
            // -- we use it here since MSVC and (some) versions of 32-bit
            // MinGW GCC do not support the standard ll size prefix.
            sprintf(buf, ""%I64d"", u.i);
        #else
            sprintf(buf, ""%"" MR_INT_LEAST64_LENGTH_MODIFIER ""d"", u.i);
        #endif
        MR_make_aligned_string_copy(Str, buf);
    #else
        MR_fatal_error(
        ""64-bit integers not supported on this platform"");
    #endif
").

:- pragma foreign_proc("Java",
    float64_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    double d = (double) Flt;
    long i = Double.doubleToLongBits(d);
    Str = Long.toString(i);
").

:- pragma foreign_proc("C#",
    float64_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    double d = (double) Flt;
    long i = System.BitConverter.DoubleToInt64Bits(d);
    Str = i.ToString();
").

:- pragma foreign_proc("Erlang",
    float64_bits_string(Flt::in) = (Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    <<Int:64/signed-integer>> = <<Flt:64/float>>,
    Str = integer_to_list(Int)
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
