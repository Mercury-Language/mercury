%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2007, 2011-2012 The University of Melbourne.
% Copyright (C) 2014, 2016-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: math.m.
% Main author: bromage.
% Stability: high.
%
% Higher mathematical operations. (The basics are in float.m.)
%
% By default, domain errors are currently handled by throwing an exception.
% For better performance, each operation in this module that can throw a domain
% exception also has an unchecked version that omits the domain check.
%
% The unchecked operations are semantically safe, since the target math
% library and/or floating point hardware perform these checks for you.
% The benefit of having the Mercury library perform the checks instead is
% that Mercury will tell you in which function or predicate the error
% occurred, as well as giving you a stack trace if that is enabled; with
% the unchecked operations you only have the information that the
% floating-point exception signal handler gives you.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module math.
:- interface.

    % A domain error exception, indicates that the inputs to a function
    % were outside the domain of the function. The string indicates
    % where the error occurred.
    %
:- type domain_error ---> domain_error(string).

%---------------------------------------------------------------------------%
%
% Mathematical constants
%

    % Pythagoras' number.
    %
:- func pi = float.

    % Base of natural logarithms.
    %
:- func e = float.

%---------------------------------------------------------------------------%
%
% "Next integer" operations
%

    % ceiling(X) = Ceil is true if Ceil is the smallest integer
    % not less than X.
    % If X is of infinite magnitude then Ceil = X.
    %
:- func ceiling(float) = float.

    % floor(X) = Floor is true if Floor is the largest integer
    % not greater than X.
    % If X is of infinite magnitude then Floor = X.
    %
:- func floor(float) = float.

    % round(X) = Round is true if Round is the integer closest to X.
    % If X has a fractional value of 0.5, it is rounded up.
    % If X is of infinite magnitude then Round = X.
    %
:- func round(float) = float.

    % truncate(X) = Trunc is true if Trunc is the integer closest to X
    % such that |Trunc| =< |X|.
    % If X is of infinite magnitude then Trunc = X.
    %
:- func truncate(float) = float.

%---------------------------------------------------------------------------%
%
% Polynomial roots
%

    % sqrt(X) = Sqrt is true if Sqrt is the positive square root of X.
    %
    % Domain restriction: X >= 0
    %
:- func sqrt(float) = float.
:- func unchecked_sqrt(float) = float.

:- type quadratic_roots
    --->    no_roots
    ;       one_root(float)
    ;       two_roots(float, float).

    % solve_quadratic(A, B, C) = Roots is true if Roots are
    % the solutions to the equation Ax^2 + Bx + C.
    %
    % Domain restriction: A \= 0
    %
:- func solve_quadratic(float, float, float) = quadratic_roots.

%---------------------------------------------------------------------------%
%
% Power/logarithm operations
%

    % pow(X, Y) = Res is true if Res is X raised to the power of Y.
    %
    % Domain restriction: X >= 0 and (X = 0 implies Y > 0)
    %
:- func pow(float, float) = float.
:- func unchecked_pow(float, float) = float.

    % exp(X) = Exp is true if Exp is e raised to the power of X.
    %
:- func exp(float) = float.

    % ln(X) = Log is true if Log is the natural logarithm of X.
    %
    % Domain restriction: X > 0
    %
:- func ln(float) = float.
:- func unchecked_ln(float) = float.

    % log10(X) = Log is true if Log is the logarithm to base 10 of X.
    %
    % Domain restriction: X > 0
    %
:- func log10(float) = float.
:- func unchecked_log10(float) = float.

    % log2(X) = Log is true if Log is the logarithm to base 2 of X.
    %
    % Domain restriction: X > 0
    %
:- func log2(float) = float.
:- func unchecked_log2(float) = float.

    % log(B, X) = Log is true if Log is the logarithm to base B of X.
    %
    % Domain restriction: X > 0 and B > 0 and B \= 1
    %
:- func log(float, float) = float.
:- func unchecked_log(float, float) = float.

%---------------------------------------------------------------------------%
%
% Trigonometric operations
%

    % sin(X) = Sin is true if Sin is the sine of X.
    %
:- func sin(float) = float.

    % cos(X) = Cos is true if Cos is the cosine of X.
    %
:- func cos(float) = float.

    % tan(X) = Tan is true if Tan is the tangent of X.
    %
:- func tan(float) = float.

    % asin(X) = ASin is true if ASin is the inverse sine of X,
    % where ASin is in the range [-pi/2,pi/2].
    %
    % Domain restriction: X must be in the range [-1,1]
    %
:- func asin(float) = float.
:- func unchecked_asin(float) = float.

    % acos(X) = ACos is true if ACos is the inverse cosine of X,
    % where ACos is in the range [0, pi].
    %
    % Domain restriction: X must be in the range [-1,1]
    %
:- func acos(float) = float.
:- func unchecked_acos(float) = float.

    % atan(X) = ATan is true if ATan is the inverse tangent of X,
    % where ATan is in the range [-pi/2,pi/2].
    %
:- func atan(float) = float.

    % atan2(Y, X) = ATan is true if ATan is the inverse tangent of Y/X,
    % where ATan is in the range [-pi,pi].
    %
:- func atan2(float, float) = float.

%---------------------------------------------------------------------------%
%
% Hyperbolic functions
%

    % sinh(X) = Sinh is true if Sinh is the hyperbolic sine of X.
    %
:- func sinh(float) = float.

    % cosh(X) = Cosh is true if Cosh is the hyperbolic cosine of X.
    %
:- func cosh(float) = float.

    % tanh(X) = Tanh is true if Tanh is the hyperbolic tangent of X.
    %
:- func tanh(float) = float.

%---------------------------------------------------------------------------%
%
% Fused multiply-add operation.
%

    % Succeeds if this grade and platform provide the fused multiply-add
    % operation.
    %
:- pred have_fma is semidet.

    % fma(X, Y, Z) = FMA is true if FMA = (X * Y) + Z, rounded as one
    % floating-point operation.
    %
    % This function is (currently) only available on the C backends and only if
    % the target math library supports it.
    % Use have_fma/0 to check whether it is supported.
    %
:- func fma(float, float, float) = float.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module float.

% These operations are mostly implemented using the C interface.

:- pragma foreign_decl("C", "

    #include <math.h>

    // Mathematical constants.
    //
    // The maximum number of significant decimal digits which can be packed
    // into an IEEE-754 extended precision floating point number is 18.
    // Therefore 20 significant decimal digits for these constants
    // should be plenty.

    #define ML_FLOAT_E      2.7182818284590452354
    #define ML_FLOAT_PI     3.1415926535897932384
    #define ML_FLOAT_LN2    0.69314718055994530941

    // Choose between float or double cmath functions depending on the type of
    // MR_Float
    #if defined(MR_USE_SINGLE_PREC_FLOAT)
        #define ML_EXP(X)       expf(X)
        #define ML_LOG(X)       logf(X)
        #define ML_LOG10(X)     log10f(X)
        #define ML_POW(X, Y)    powf(X, Y)
        #define ML_SQRT(X)      sqrtf(X)
        #define ML_SIN(X)       sinf(X)
        #define ML_COS(X)       cosf(X)
        #define ML_TAN(X)       tanf(X)
        #define ML_ASIN(X)      asinf(X)
        #define ML_ACOS(X)      acosf(X)
        #define ML_ATAN(X)      atanf(X)
        #define ML_ATAN2(X, Y)  atan2f(X, Y)
        #define ML_SINH(X)      sinhf(X)
        #define ML_COSH(X)      coshf(X)
        #define ML_TANH(X)      tanhf(X)
        #if defined(MR_HAVE_FMA)
            #define ML_FMA(X, Y, Z) fmaf(X, Y, Z)
        #endif
    #else
        #define ML_EXP(X)       exp(X)
        #define ML_LOG(X)       log(X)
        #define ML_LOG10(X)     log10(X)
        #define ML_POW(X, Y)    pow(X, Y)
        #define ML_SQRT(X)      sqrt(X)
        #define ML_SIN(X)       sin(X)
        #define ML_COS(X)       cos(X)
        #define ML_TAN(X)       tan(X)
        #define ML_ASIN(X)      asin(X)
        #define ML_ACOS(X)      acos(X)
        #define ML_ATAN(X)      atan(X)
        #define ML_ATAN2(X, Y)  atan2(X, Y)
        #define ML_SINH(X)      sinh(X)
        #define ML_COSH(X)      cosh(X)
        #define ML_TANH(X)      tanh(X)
        #if defined(MR_HAVE_FMA)
            #define ML_FMA(X, Y, Z) fma(X, Y, Z)
        #endif
    #endif

"). % end pragma foreign_decl

:- pragma foreign_code("C#", "

    // This is not defined in the .NET Frameworks.
    // For pi and e we use the constants defined in System.Math.

    public static double ML_FLOAT_LN2 = 0.69314718055994530941;

").

:- pragma foreign_code("Java", "

    // As for .NET, java does not have a built-in ln2

    private static final double ML_FLOAT_LN2 = 0.69314718055994530941;

").

:- pred math_domain_checks is semidet.

:- pragma foreign_proc("C",
    math_domain_checks,
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
    math_domain_checks,
    [thread_safe, promise_pure],
"
#if ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = false;
#else
    SUCCESS_INDICATOR = true;
#endif
").

:- pragma foreign_proc("Java",
    math_domain_checks,
    [thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Erlang",
    math_domain_checks,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = true
").

%
% Mathematical constants from math.m
%
    % Pythagoras' number
:- pragma foreign_proc("C",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Pi = ML_FLOAT_PI;
").
:- pragma foreign_proc("C#",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pi = System.Math.PI;
").
:- pragma foreign_proc("Java",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pi = java.lang.Math.PI;
").
:- pragma foreign_proc("Erlang",
    pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pi = math:pi()
").
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version. We define this with sufficient
    % digits that if the underlying implementation's
    % floating point parsing routines are good, it should
    % to be accurate enough for 128-bit IEEE float.
pi = 3.1415926535897932384626433832795029.

    % Base of natural logarithms
:- pragma foreign_proc("C",
    e = (E::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    E = ML_FLOAT_E;
").
:- pragma foreign_proc("C#",
    e = (E::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    E = System.Math.E;
").
:- pragma foreign_proc("Java",
    e = (E::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    E = java.lang.Math.E;
").
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version. We define this with sufficient
    % digits that if the underlying implementation's
    % floating point parsing routines are good, it should
    % to be accurate enough for 128-bit IEEE float.
e = 2.7182818284590452353602874713526625.

:- pragma foreign_proc("C",
    ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Ceil = ML_FLOAT_CEIL(Num);
").
:- pragma foreign_proc("C#",
    ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ceil = System.Math.Ceiling(Num);
").
:- pragma foreign_proc("Java",
    ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ceil = java.lang.Math.ceil(Num);
").
:- pragma foreign_proc("Erlang",
    ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = erlang:trunc(Num),
    case (Num - T) > 0 of
        true  ->
            Ceil = float(T + 1);
        false ->
            Ceil = float(T)
    end
").

:- pragma foreign_proc("C",
    floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Floor = ML_FLOAT_FLOOR(Num);
").
:- pragma foreign_proc("C#",
    floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Floor = System.Math.Floor(Num);
").
:- pragma foreign_proc("Java",
    floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Floor = java.lang.Math.floor(Num);
").
:- pragma foreign_proc("Erlang",
    floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = erlang:trunc(Num),
    case (Num - T) < 0 of
        true ->
            Floor = float(T - 1);
        false ->
            Floor = float(T)
    end
").

:- pragma foreign_proc("C",
    round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Rounded = ML_FLOAT_FLOOR(Num + (MR_Float)0.5);
").
:- pragma foreign_proc("C#",
    round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX the semantics of System.Math.Round() are not the same as ours.
    // Unfortunately they are better (round to nearest even number).
    Rounded = System.Math.Floor(Num+0.5);
").
:- pragma foreign_proc("Java",
    round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // For +/- infinity Java's round method will return Long.{MAX,MIN}_VALUE.
    // This does not match the documented Mercury behaviour for round with
    // infinities.
    if (java.lang.Double.isInfinite(Num)) {
        Rounded = Num;
    } else {
        Rounded = java.lang.Math.round(Num);
    }
").
:- pragma foreign_proc("Erlang",
    round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Rounded = float(round(Num))
").
round(Num) = math.floor(Num + 0.5).

truncate(X) = ( if X < 0.0 then math.ceiling(X) else math.floor(X)).

sqrt(X) = SquareRoot :-
    ( if math_domain_checks, X < 0.0 then
        throw(domain_error("math.sqrt"))
    else
        SquareRoot = unchecked_sqrt(X)
    ).

:- pragma foreign_proc("C",
    unchecked_sqrt(X::in) = (SquareRoot::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SquareRoot = ML_SQRT(X);
").
:- pragma foreign_proc("C#",
    unchecked_sqrt(X::in) = (SquareRoot::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SquareRoot = System.Math.Sqrt(X);
").
:- pragma foreign_proc("Java",
    unchecked_sqrt(X::in) = (SquareRoot::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SquareRoot = java.lang.Math.sqrt(X);
").
:- pragma foreign_proc("Erlang",
    unchecked_sqrt(X::in) = (SquareRoot::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SquareRoot = math:sqrt(X)
").
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
unchecked_sqrt(X) = math.exp(math.ln(X) / 2.0).

solve_quadratic(A, B, C) = Roots :-
    % This implementation is designed to minimise numerical errors;
    % it is adapted from "Numerical recipes in C".
    DSquared = B * B - 4.0 * A * C,
    compare(CmpD, DSquared, 0.0),
    (
        CmpD = (<),
        Roots = no_roots
    ;
        CmpD = (=),
        Root = -0.5 * B / A,
        Roots = one_root(Root)
    ;
        CmpD = (>),
        D = sqrt(DSquared),
        compare(CmpB, B, 0.0),
        (
            CmpB = (<),
            Q = -0.5 * (B - D),
            Root1 = Q / A,
            Root2 = C / Q
        ;
            CmpB = (=),
            Root1 = -0.5 * D / A,
            Root2 = -Root1
        ;
            CmpB = (>),
            Q = -0.5 * (B + D),
            Root1 = Q / A,
            Root2 = C / Q
        ),
        Roots = two_roots(Root1, Root2)
    ).

pow(X, Y) = Res :-
    ( if math_domain_checks, X < 0.0 then
        throw(domain_error("math.pow"))
    else if X = 0.0 then
        ( if Y =< 0.0 then
            throw(domain_error("math.pow"))
        else
            Res = 0.0
        )
    else
        Res = math.unchecked_pow(X, Y)
    ).

:- pragma foreign_proc("C",
    unchecked_pow(X::in, Y::in) = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Res = ML_POW(X, Y);
").

:- pragma foreign_proc("C#",
    unchecked_pow(X::in, Y::in) = (Res::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Res = System.Math.Pow(X, Y);
").

:- pragma foreign_proc("Java",
    unchecked_pow(X::in, Y::in) = (Res::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Res = java.lang.Math.pow(X, Y);
").

:- pragma foreign_proc("Erlang",
    unchecked_pow(X::in, Y::in) = (Res::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Res = math:pow(X, Y)
").

:- pragma foreign_proc("C",
    exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Exp = ML_EXP(X);
").
:- pragma foreign_proc("C#",
    exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Exp = System.Math.Exp(X);
").
:- pragma foreign_proc("Java",
    exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Exp = java.lang.Math.exp(X);
").
:- pragma foreign_proc("Erlang",
    exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Exp = math:exp(X)
").

ln(X) = Log :-
    ( if math_domain_checks, X =< 0.0 then
        throw(domain_error("math.ln"))
    else
        Log = math.unchecked_ln(X)
    ).

:- pragma foreign_proc("C",
    unchecked_ln(X::in) = (Log::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Log = ML_LOG(X);
").
:- pragma foreign_proc("C#",
    unchecked_ln(X::in) = (Log::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log = System.Math.Log(X);
").
:- pragma foreign_proc("Java",
    unchecked_ln(X::in) = (Log::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log = java.lang.Math.log(X);
").
:- pragma foreign_proc("Erlang",
    unchecked_ln(X::in) = (Log::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log = math:log(X)
").

log10(X) = Log :-
    ( if math_domain_checks, X =< 0.0 then
        throw(domain_error("math.log10"))
    else
        Log = math.unchecked_log10(X)
    ).

:- pragma foreign_proc("C",
    unchecked_log10(X::in) = (Log10::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Log10 = ML_LOG10(X);
").
:- pragma foreign_proc("C#",
    unchecked_log10(X::in) = (Log10::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log10 = System.Math.Log10(X);
").
:- pragma foreign_proc("Java",
    unchecked_log10(X::in) = (Log10::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log10 = java.lang.Math.log10(X);
").
:- pragma foreign_proc("Erlang",
    unchecked_log10(X::in) = (Log10::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log10 = math:log10(X)
").

log2(X) = Log :-
    ( if math_domain_checks, X =< 0.0 then
        throw(domain_error("math.log2"))
    else
        Log = math.unchecked_log2(X)
    ).

:- pragma foreign_proc("C",
    unchecked_log2(X::in) = (Log2::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Log2 = ML_LOG(X) / ML_FLOAT_LN2;
").
:- pragma foreign_proc("C#",
    unchecked_log2(X::in) = (Log2::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log2 = System.Math.Log(X) / ML_FLOAT_LN2;
").
:- pragma foreign_proc("Java",
    unchecked_log2(X::in) = (Log2::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log2 = java.lang.Math.log(X) / ML_FLOAT_LN2;
").
unchecked_log2(X) = math.unchecked_ln(X) / math.unchecked_ln(2.0).

log(B, X) = Log :-
    ( if
        math_domain_checks,
        ( X =< 0.0
        ; B =< 0.0
        ; B = 1.0
        )
    then
        throw(domain_error("math.log"))
    else
        Log = math.unchecked_log(B, X)
    ).

:- pragma foreign_proc("C",
    unchecked_log(B::in, X::in) = (Log::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Log = ML_LOG(X) / ML_LOG(B);
").
:- pragma foreign_proc("C#",
    unchecked_log(B::in, X::in) = (Log::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Log = System.Math.Log(X, B);
").
% Java implementation will default to mercury here.
% Erlang implementation will default to mercury here.
unchecked_log(B, X) = math.unchecked_ln(X) / math.unchecked_ln(B).

:- pragma foreign_proc("C",
    sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Sin = ML_SIN(X);
").
:- pragma foreign_proc("C#",
    sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sin = System.Math.Sin(X);
").
:- pragma foreign_proc("Java",
    sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sin = java.lang.Math.sin(X);
").
:- pragma foreign_proc("Erlang",
    sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sin = math:sin(X)
").

:- pragma foreign_proc("C",
    cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Cos = ML_COS(X);
").
:- pragma foreign_proc("C#",
    cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cos = System.Math.Cos(X);
").
:- pragma foreign_proc("Java",
    cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cos = java.lang.Math.cos(X);
").
:- pragma foreign_proc("Erlang",
    cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cos = math:cos(X)
").

:- pragma foreign_proc("C",
    tan(X::in) = (Tan::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Tan = ML_TAN(X);
").
:- pragma foreign_proc("C#",
    math.tan(X::in) = (Tan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tan = System.Math.Tan(X);
").
:- pragma foreign_proc("Java",
    math.tan(X::in) = (Tan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tan = java.lang.Math.tan(X);
").
:- pragma foreign_proc("Erlang",
    math.tan(X::in) = (Tan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tan = math:tan(X)
").

asin(X) = ASin :-
    ( if
        math_domain_checks,
        ( X < -1.0
        ; X > 1.0
        )
    then
        throw(domain_error("math.asin"))
    else
        ASin = math.unchecked_asin(X)
    ).

:- pragma foreign_proc("C",
    unchecked_asin(X::in) = (ASin::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    ASin = ML_ASIN(X);
").
:- pragma foreign_proc("C#",
    unchecked_asin(X::in) = (ASin::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ASin = System.Math.Asin(X);
").
:- pragma foreign_proc("Java",
    unchecked_asin(X::in) = (ASin::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ASin = java.lang.Math.asin(X);
").
:- pragma foreign_proc("Erlang",
    unchecked_asin(X::in) = (ASin::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ASin = math:asin(X)
").

acos(X) = ACos :-
    ( if
        math_domain_checks,
        ( X < -1.0
        ; X > 1.0
        )
    then
        throw(domain_error("math.acos"))
    else
        ACos = math.unchecked_acos(X)
    ).

:- pragma foreign_proc("C",
    unchecked_acos(X::in) = (ACos::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    ACos = ML_ACOS(X);
").
:- pragma foreign_proc("C#",
    unchecked_acos(X::in) = (ACos::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ACos = System.Math.Acos(X);
").
:- pragma foreign_proc("Java",
    unchecked_acos(X::in) = (ACos::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ACos = java.lang.Math.acos(X);
").
:- pragma foreign_proc("Erlang",
    unchecked_acos(X::in) = (ACos::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ACos = math:acos(X)
").

:- pragma foreign_proc("C",
    math.atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    ATan = ML_ATAN(X);
").
:- pragma foreign_proc("C#",
    atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan = System.Math.Atan(X);
").
:- pragma foreign_proc("Java",
    atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan = java.lang.Math.atan(X);
").
:- pragma foreign_proc("Erlang",
    atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan = math:atan(X)
").

:- pragma foreign_proc("C",
    atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    ATan2 = ML_ATAN2(Y, X);
").
:- pragma foreign_proc("C#",
    atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan2 = System.Math.Atan2(Y, X);
").
:- pragma foreign_proc("Java",
    atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan2 = java.lang.Math.atan2(Y, X);
").
:- pragma foreign_proc("Erlang",
    atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan2 = math:atan2(Y, X)
").

:- pragma foreign_proc("C",
    sinh(X::in) = (Sinh::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Sinh = ML_SINH(X);
").
:- pragma foreign_proc("Java",
    sinh(X::in) = (Sinh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sinh = java.lang.Math.sinh(X);
").
:- pragma foreign_proc("C#",
    sinh(X::in) = (Sinh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sinh = System.Math.Sinh(X);
").
:- pragma foreign_proc("Erlang",
    sinh(X::in) = (Sinh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sinh = math:sinh(X)
").
% Version for back-ends that do not have a foreign_proc version.
sinh(X) = Sinh :-
    Sinh = (exp(X)-exp(-X)) / 2.0.

:- pragma foreign_proc("C",
    cosh(X::in) = (Cosh::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Cosh = ML_COSH(X);
").
:- pragma foreign_proc("Java",
    cosh(X::in) = (Cosh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cosh = java.lang.Math.cosh(X);
").
:- pragma foreign_proc("C#",
    cosh(X::in) = (Cosh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cosh = System.Math.Cosh(X);
").
:- pragma foreign_proc("Erlang",
    cosh(X::in) = (Cosh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cosh = math:cosh(X)
").
% Version for back-ends that do not have a foreign_proc version.
cosh(X) = Cosh :-
    Cosh = (exp(X)+exp(-X)) / 2.0.

:- pragma foreign_proc("C",
    tanh(X::in) = (Tanh::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Tanh = ML_TANH(X);
").
:- pragma foreign_proc("Java",
    tanh(X::in) = (Tanh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tanh = java.lang.Math.tanh(X);
").
:- pragma foreign_proc("C#",
    tanh(X::in) = (Tanh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tanh = System.Math.Tanh(X);
").
:- pragma foreign_proc("Erlang",
    tanh(X::in) = (Tanh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tanh = math:tanh(X)
").
% Version for back-ends that do not have a foreign_proc version.
tanh(X) = Tanh :-
    Tanh = (exp(X)-exp(-X)) / (exp(X)+exp(-X)).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    have_fma,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#if defined(MR_HAVE_FMA)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

have_fma :-
    semidet_false.

:- pragma no_determinism_warning(fma/3).

:- pragma foreign_proc("C",
    fma(X::in, Y::in, Z::in) = (FMA::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#if defined(MR_HAVE_FMA)
    FMA = ML_FMA(X, Y, Z);
#else
    MR_fatal_error(""math.fma not supported"");
#endif
").

fma(_, _, _) = _ :-
    private_builtin.sorry("math.fma").

% NOTE: Java 9 provides Math.fma.
% NOTE: .NET core 3.0 provides System.Math.FusedMultiplyAdd.

%---------------------------------------------------------------------------%
:- end_module math.
%---------------------------------------------------------------------------%
