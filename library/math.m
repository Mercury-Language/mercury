%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: math.m.
% Main author: bromage.
% Stability: high.
%
% Higher mathematical operations.  (The basics are in float.m.)
%
% By default, domain errors are currently handled by throwing an exception.
%
% For better performance, it is possible to disable the Mercury domain
% checking by compiling with `--intermodule-optimization' and the C macro
% symbol `ML_OMIT_MATH_DOMAIN_CHECKS' defined, e.g. by using
% `MCFLAGS=--intermodule-optimization' and
% `MGNUCFLAGS=-DML_OMIT_MATH_DOMAIN_CHECKS' in your Mmakefile,
% or by compiling with the command
% `mmc --intermodule-optimization --cflags -DML_OMIT_MATH_DOMAIN_CHECKS'.
%
% For maximum performance, all Mercury domain checking can be disabled by
% recompiling this module using `MGNUCFLAGS=-DML_OMIT_MATH_DOMAIN_CHECKS'
% or `mmc --cflags -DML_OMIT_MATH_DOMAIN_CHECKS' as above. You can
% either recompile the entire library, or just copy `math.m' to your
% application's source directory and link with it directly instead of as
% part of the library.
%
% Note that the above performance improvements are semantically safe,
% since the C math library and/or floating point hardware perform these
% checks for you.  The benefit of having the Mercury library perform the
% checks instead is that Mercury will tell you in which function or
% predicate the error occurred, as well as giving you a stack trace if
% that is enabled; with the checks disabled you only have the information
% that the floating-point exception signal handler gives you.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module math.
:- interface.

    % A domain error exception, indicates that the inputs to a function
    % were outside the domain of the function.  The string indicates
    % where the error occurred.
    %
    % It is possible to switch domain checking off, in which case,
    % depending on the backend, a domain error may cause a program
    % abort.
    %
:- type domain_error ---> domain_error(string).

%---------------------------------------------------------------------------%
%
% Mathematical constants
%

    % Pythagoras' number.
    %
:- func math.pi = float.

    % Base of natural logarithms.
    %
:- func math.e = float.

%---------------------------------------------------------------------------%
%
% "Next integer" operations
%

    % math.ceiling(X) = Ceil is true if Ceil is the smallest integer
    % not less than X.
    %
:- func math.ceiling(float) = float.

    % math.floor(X) = Floor is true if Floor is the largest integer
    % not greater than X.
    %
:- func math.floor(float) = float.

    % math.round(X) = Round is true if Round is the integer closest to X.
    % If X has a fractional value of 0.5, it is rounded up.
    %
:- func math.round(float) = float.

    % math.truncate(X) = Trunc is true if Trunc is the integer closest to X
    % such that |Trunc| =< |X|.
    %
:- func math.truncate(float) = float.

%---------------------------------------------------------------------------%
%
% Polynomial roots
%

    % math.sqrt(X) = Sqrt is true if Sqrt is the positive square root of X.
    %
    % Domain restriction: X >= 0
    %
:- func math.sqrt(float) = float.

:- type math.quadratic_roots
    --->    no_roots
    ;       one_root(float)
    ;       two_roots(float, float).

    % math.solve_quadratic(A, B, C) = Roots is true if Roots are
    % the solutions to the equation Ax^2 + Bx + C.
    %
    % Domain restriction: A \= 0
    %
:- func math.solve_quadratic(float, float, float) = quadratic_roots.

%---------------------------------------------------------------------------%
%
% Power/logarithm operations
%

    % math.pow(X, Y) = Res is true if Res is X raised to the power of Y.
    %
    % Domain restriction: X >= 0 and (X = 0 implies Y > 0)
    %
:- func math.pow(float, float) = float.

    % math.exp(X) = Exp is true if Exp is e raised to the power of X.
    %
:- func math.exp(float) = float.

    % math.ln(X) = Log is true if Log is the natural logarithm of X.
    %
    % Domain restriction: X > 0
    %
:- func math.ln(float) = float.

    % math.log10(X) = Log is true if Log is the logarithm to base 10 of X.
    %
    % Domain restriction: X > 0
    %
:- func math.log10(float) = float.

    % math.log2(X) = Log is true if Log is the logarithm to base 2 of X.
    %
    % Domain restriction: X > 0
    %
:- func math.log2(float) = float.

    % math.log(B, X) = Log is true if Log is the logarithm to base B of X.
    %
    % Domain restriction: X > 0 and B > 0 and B \= 1
    %
:- func math.log(float, float) = float.

%---------------------------------------------------------------------------%
%
% Trigonometric operations
%

    % math.sin(X) = Sin is true if Sin is the sine of X.
    %
:- func math.sin(float) = float.

    % math.cos(X) = Cos is true if Cos is the cosine of X.
    %
:- func math.cos(float) = float.

    % math.tan(X) = Tan is true if Tan is the tangent of X.
    %
:- func math.tan(float) = float.

    % math.asin(X) = ASin is true if ASin is the inverse sine of X,
    % where ASin is in the range [-pi/2,pi/2].
    %
    % Domain restriction: X must be in the range [-1,1]
    %
:- func math.asin(float) = float.

    % math.acos(X) = ACos is true if ACos is the inverse cosine of X,
    % where ACos is in the range [0, pi].
    %
    % Domain restriction: X must be in the range [-1,1]
    %
:- func math.acos(float) = float.

    % math.atan(X) = ATan is true if ATan is the inverse tangent of X,
    % where ATan is in the range [-pi/2,pi/2].
    %
:- func math.atan(float) = float.

    % math.atan2(Y, X) = ATan is true if ATan is the inverse tangent of Y/X,
    % where ATan is in the range [-pi,pi].
    %
:- func math.atan2(float, float) = float.

%---------------------------------------------------------------------------%
%
% Hyperbolic functions
%

    % math.sinh(X) = Sinh is true if Sinh is the hyperbolic sine of X.
    %
:- func math.sinh(float) = float.

    % math.cosh(X) = Cosh is true if Cosh is the hyperbolic cosine of X.
    %
:- func math.cosh(float) = float.

    % math.tanh(X) = Tanh is true if Tanh is the hyperbolic tangent of X.
    %
:- func math.tanh(float) = float.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module float.

% These operations are mostly implemented using the C interface.

:- pragma foreign_decl("C", "

    #include <math.h>

    /*
    ** Mathematical constants.
    **
    ** The maximum number of significant decimal digits which
    ** can be packed into an IEEE-754 extended precision
    ** floating point number is 18.  Therefore 20 significant
    ** decimal digits for these constants should be plenty.
    */

    #define ML_FLOAT_E      2.7182818284590452354
    #define ML_FLOAT_PI     3.1415926535897932384
    #define ML_FLOAT_LN2        0.69314718055994530941

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

:- pred domain_checks is semidet.

:- pragma foreign_proc("C",
    domain_checks,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#ifdef ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = MR_FALSE;
#else
    SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("C#",
    domain_checks,
    [thread_safe, promise_pure],
"
#if ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = false;
#else
    SUCCESS_INDICATOR = true;
#endif
").

:- pragma foreign_proc("Java",
    domain_checks,
    [thread_safe, promise_pure],
"
    succeeded = true;
").

%
% Mathematical constants from math.m
%
    % Pythagoras' number
:- pragma foreign_proc("C",
    math.pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Pi = ML_FLOAT_PI;
").
:- pragma foreign_proc("C#",
    math.pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pi = System.Math.PI;
").
:- pragma foreign_proc("Java",
    math.pi = (Pi::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pi = java.lang.Math.PI;
").
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.  We define this with sufficient
    % digits that if the underlying implementation's
    % floating point parsing routines are good, it should
    % to be accurate enough for 128-bit IEEE float.
math.pi = 3.1415926535897932384626433832795029.

    % Base of natural logarithms
:- pragma foreign_proc("C",
    math.e = (E::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    E = ML_FLOAT_E;
").
:- pragma foreign_proc("C#",
    math.e = (E::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    E = System.Math.E;
").
:- pragma foreign_proc("Java",
    math.e = (E::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    E = java.lang.Math.E;
").
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.  We define this with sufficient
    % digits that if the underlying implementation's
    % floating point parsing routines are good, it should
    % to be accurate enough for 128-bit IEEE float.
math.e = 2.7182818284590452353602874713526625.

:- pragma foreign_proc("C",
    math.ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Ceil = ceil(Num);
").
:- pragma foreign_proc("C#",
    math.ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ceil = System.Math.Ceiling(Num);
").
:- pragma foreign_proc("Java",
    math.ceiling(Num::in) = (Ceil::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ceil = java.lang.Math.ceil(Num);
").

:- pragma foreign_proc("C",
    math.floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Floor = floor(Num);
").
:- pragma foreign_proc("C#",
    math.floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Floor = System.Math.Floor(Num);
").
:- pragma foreign_proc("Java",
    math.floor(Num::in) = (Floor::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Floor = java.lang.Math.floor(Num);
").

:- pragma foreign_proc("C",
    math.round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Rounded = floor(Num+0.5);
").
:- pragma foreign_proc("C#",
    math.round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX the semantics of System.Math.Round() are not the same as ours.
    // Unfortunately they are better (round to nearest even number).
    Rounded = System.Math.Floor(Num+0.5);
").
:- pragma foreign_proc("Java",
    math.round(Num::in) = (Rounded::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Rounded = java.lang.Math.round(Num);
").
math.round(Num) = math.floor(Num + 0.5).

math.truncate(X) = (X < 0.0 -> math.ceiling(X) ; math.floor(X)).

math.sqrt(X) = SquareRoot :-
    ( domain_checks, X < 0.0 ->
        throw(domain_error("math.sqrt"))
    ;
        SquareRoot = math.sqrt_2(X)
    ).

:- func math.sqrt_2(float) = float.

:- pragma foreign_proc("C",
    math.sqrt_2(X::in) = (SquareRoot::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SquareRoot = sqrt(X);
").
:- pragma foreign_proc("C#",
    math.sqrt_2(X::in) = (SquareRoot::out),
    [thread_safe, promise_pure],
"
    SquareRoot = System.Math.Sqrt(X);
").
:- pragma foreign_proc("Java",
    math.sqrt_2(X::in) = (SquareRoot::out),
    [thread_safe, promise_pure],
"
    SquareRoot = java.lang.Math.sqrt(X);
").
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
math.sqrt_2(X) = math.exp(math.ln(X) / 2.0).

math.solve_quadratic(A, B, C) = Roots :-
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

math.pow(X, Y) = Res :-
    ( domain_checks, X < 0.0 ->
        throw(domain_error("math.pow"))
    ; X = 0.0 ->
        ( Y =< 0.0 ->
            throw(domain_error("math.pow"))
        ;
            Res = 0.0
        )
    ;
        Res = math.pow_2(X, Y)
    ).

:- func math.pow_2(float, float) = float.

:- pragma foreign_proc("C",
    math.pow_2(X::in, Y::in) = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Res = pow(X, Y);
").

:- pragma foreign_proc("C#",
    math.pow_2(X::in, Y::in) = (Res::out),
    [thread_safe, promise_pure],
"
    Res = System.Math.Pow(X, Y);
").

:- pragma foreign_proc("Java",
    math.pow_2(X::in, Y::in) = (Res::out),
    [thread_safe, promise_pure],
"
    Res = java.lang.Math.pow(X, Y);
").

:- pragma foreign_proc("C",
    math.exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Exp = exp(X);
").
:- pragma foreign_proc("C#",
    math.exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Exp = System.Math.Exp(X);
").
:- pragma foreign_proc("Java",
    math.exp(X::in) = (Exp::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Exp = java.lang.Math.exp(X);
").

math.ln(X) = Log :-
    ( domain_checks, X =< 0.0 ->
        throw(domain_error("math.ln"))
    ;
        Log = math.ln_2(X)
    ).

:- func math.ln_2(float) = float.

:- pragma foreign_proc("C",
    math.ln_2(X::in) = (Log::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Log = log(X);
").
:- pragma foreign_proc("C#",
    math.ln_2(X::in) = (Log::out),
    [thread_safe, promise_pure],
"
    Log = System.Math.Log(X);
").
:- pragma foreign_proc("Java",
    math.ln_2(X::in) = (Log::out),
    [thread_safe, promise_pure],
"
    Log = java.lang.Math.log(X);
").

math.log10(X) = Log :-
    ( domain_checks, X =< 0.0 ->
        throw(domain_error("math.log10"))
    ;
        Log = math.log10_2(X)
    ).

:- func math.log10_2(float) = float.

:- pragma foreign_proc("C",
    math.log10_2(X::in) = (Log10::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Log10 = log10(X);
").
:- pragma foreign_proc("C#",
    math.log10_2(X::in) = (Log10::out),
    [thread_safe, promise_pure],
"
    Log10 = System.Math.Log10(X);
").
% Java doesn't have a built-in log10, so default to mercury here.
math.log10_2(X) = math.ln_2(X) / math.ln_2(10.0).

math.log2(X) = Log :-
    ( domain_checks, X =< 0.0 ->
        throw(domain_error("math.log2"))
    ;
        Log = math.log2_2(X)
    ).

:- func math.log2_2(float) = float.

:- pragma foreign_proc("C",
    math.log2_2(X::in) = (Log2::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Log2 = log(X) / ML_FLOAT_LN2;
").
:- pragma foreign_proc("C#",
    math.log2_2(X::in) = (Log2::out),
    [thread_safe, promise_pure],
"
    Log2 = System.Math.Log(X) / ML_FLOAT_LN2;
").
:- pragma foreign_proc("Java",
    math.log2_2(X::in) = (Log2::out),
    [thread_safe, promise_pure],
"
    Log2 = java.lang.Math.log(X) / ML_FLOAT_LN2;
").
math.log2_2(X) = math.ln_2(X) / math.ln_2(2.0).

math.log(B, X) = Log :-
    (
        domain_checks,
        ( X =< 0.0
        ; B =< 0.0
        ; B = 1.0
        )
    ->
        throw(domain_error("math.log"))
    ;
        Log = math.log_2(B, X)
    ).

:- func math.log_2(float, float) = float.

:- pragma foreign_proc("C",
    math.log_2(B::in, X::in) = (Log::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Log = log(X) / log(B);
").
:- pragma foreign_proc("C#",
    math.log_2(B::in, X::in) = (Log::out),
    [thread_safe, promise_pure],
"
    Log = System.Math.Log(X, B);
").
% Java implementation will default to mercury here.
math.log_2(B, X) = math.ln_2(X) / math.ln_2(B).

:- pragma foreign_proc("C",
    math.sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Sin = sin(X);
").
:- pragma foreign_proc("C#",
    math.sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sin = System.Math.Sin(X);
").
:- pragma foreign_proc("Java",
    math.sin(X::in) = (Sin::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sin = java.lang.Math.sin(X);
").

:- pragma foreign_proc("C",
    math.cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Cos = cos(X);
").
:- pragma foreign_proc("C#",
    math.cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cos = System.Math.Cos(X);
").
:- pragma foreign_proc("Java",
    math.cos(X::in) = (Cos::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cos = java.lang.Math.cos(X);
").

:- pragma foreign_proc("C",
    math.tan(X::in) = (Tan::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Tan = tan(X);
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

math.asin(X) = ASin :-
    (
        domain_checks,
        ( X < -1.0
        ; X > 1.0
        )
    ->
        throw(domain_error("math.asin"))
    ;
        ASin = math.asin_2(X)
    ).

:- func math.asin_2(float) = float.

:- pragma foreign_proc("C",
    math.asin_2(X::in) = (ASin::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ASin = asin(X);
").
:- pragma foreign_proc("C#",
    math.asin_2(X::in) = (ASin::out),
    [thread_safe, promise_pure],
"
    ASin = System.Math.Asin(X);
").
:- pragma foreign_proc("Java",
    math.asin_2(X::in) = (ASin::out),
    [thread_safe, promise_pure],
"
    ASin = java.lang.Math.asin(X);
").

math.acos(X) = ACos :-
    (
        domain_checks,
        ( X < -1.0
        ; X > 1.0
        )
    ->
        throw(domain_error("math.acos"))
    ;
        ACos = math.acos_2(X)
    ).

:- func math.acos_2(float) = float.

:- pragma foreign_proc("C",
    math.acos_2(X::in) = (ACos::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ACos = acos(X);
").
:- pragma foreign_proc("C#",
    math.acos_2(X::in) = (ACos::out),
    [thread_safe, promise_pure],
"
    ACos = System.Math.Acos(X);
").
:- pragma foreign_proc("Java",
    math.acos_2(X::in) = (ACos::out),
    [thread_safe, promise_pure],
"
    ACos = java.lang.Math.acos(X);
").


:- pragma foreign_proc("C",
    math.atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ATan = atan(X);
").
:- pragma foreign_proc("C#",
    math.atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan = System.Math.Atan(X);
").
:- pragma foreign_proc("Java",
    math.atan(X::in) = (ATan::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan = java.lang.Math.atan(X);
").

:- pragma foreign_proc("C",
    math.atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    ATan2 = atan2(Y, X);
").
:- pragma foreign_proc("C#",
    math.atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan2 = System.Math.Atan2(Y, X);
").
:- pragma foreign_proc("Java",
    math.atan2(Y::in, X::in) = (ATan2::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ATan2 = java.lang.Math.atan2(Y, X);
").

:- pragma foreign_proc("C",
    math.sinh(X::in) = (Sinh::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Sinh = sinh(X);
").
:- pragma foreign_proc("C#",
    math.sinh(X::in) = (Sinh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sinh = System.Math.Sinh(X);
").
% Java doesn't have any hyperbolic functions built in.
math.sinh(X) = Sinh :-
    Sinh = (exp(X)-exp(-X)) / 2.0.

:- pragma foreign_proc("C",
    math.cosh(X::in) = (Cosh::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Cosh = cosh(X);
").
:- pragma foreign_proc("C#",
    math.cosh(X::in) = (Cosh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Cosh = System.Math.Cosh(X);
").
% Java doesn't have any hyperbolic functions built in.
math.cosh(X) = Cosh :-
    Cosh = (exp(X)+exp(-X)) / 2.0.

:- pragma foreign_proc("C",
    math.tanh(X::in) = (Tanh::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Tanh = tanh(X);
").
:- pragma foreign_proc("C#",
    math.tanh(X::in) = (Tanh::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tanh = System.Math.Tanh(X);
").
% Java doesn't have any hyperbolic functions built in.
math.tanh(X) = Tanh :-
    Tanh = (exp(X)-exp(-X)) / (exp(X)+exp(-X)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
