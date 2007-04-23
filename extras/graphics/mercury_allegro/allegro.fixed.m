%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.fixed.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.fixed.
:- interface.

:- type fixed.

:- func itofix(int) = fixed.
:- func fixtoi(fixed) = int.
:- func fixfloor(fixed) = int.
:- func fixceil(fixed) = int.
:- func ftofix(float) = fixed.
:- func fixtof(fixed) = float.
:- func fixmul(fixed, fixed) = fixed.
:- func fixdiv(fixed, fixed) = fixed.
:- func fixadd(fixed, fixed) = fixed.
:- func fixsub(fixed, fixed) = fixed.
:- func fixtorad_r = fixed.
:- func radtofix_r = fixed.
:- func fixsin(fixed) = fixed.
:- func fixcos(fixed) = fixed.
:- func fixtan(fixed) = fixed.
:- func fixasin(fixed) = fixed.
:- func fixacos(fixed) = fixed.
:- func fixatan(fixed) = fixed.
:- func fixatan2(fixed, fixed) = fixed.
:- func fixsqrt(fixed) = fixed.
:- func fixhypot(fixed, fixed) = fixed.

    % Nicer names using overloading.
    %
:- func floor(fixed) = int.
:- func ceil(fixed) = int.
:- func fixed * fixed = fixed.
:- func fixed / fixed = fixed.
:- func fixed + fixed = fixed.
:- func fixed - fixed = fixed.
:- func sin(fixed) = fixed.
:- func cos(fixed) = fixed.
:- func tan(fixed) = fixed.
:- func asin(fixed) = fixed.
:- func acos(fixed) = fixed.
:- func atan(fixed) = fixed.
:- func atan2(fixed, fixed) = fixed.
:- func sqrt(fixed) = fixed.
:- func hypot(fixed, fixed) = fixed.

    % Additions to C API.
    %
:- func fixed_to_raw_int(fixed) = int.
:- func raw_int_to_fixed(int) = fixed.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", fixed, "fixed").

:- pragma foreign_proc("C",
    itofix(I::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = itofix(I);
").

:- pragma foreign_proc("C",
    fixtoi(X::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = fixtoi(X);
").

:- pragma foreign_proc("C",
    fixfloor(X::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = fixfloor(X);
").

:- pragma foreign_proc("C",
    fixceil(X::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = fixceil(X);
").

:- pragma foreign_proc("C",
    ftofix(F::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = ftofix(F);
").

:- pragma foreign_proc("C",
    fixtof(X::in) = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = fixtof(X);
").

:- pragma foreign_proc("C",
    fixmul(A::in, B::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixmul(A, B);
").

:- pragma foreign_proc("C",
    fixdiv(A::in, B::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* gcc 3.4.3 spits out this error if we use the asm definition of fixdiv()
     *   error: can't find a register in class `GENERAL_REGS' while reloading
     *   `asm'
     */
    /* X = fixdiv(A, B); */

    /* workaround */
    X = ftofix(fixtof(A) / fixtof(B));
").

:- pragma foreign_proc("C",
    fixadd(A::in, B::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixadd(A, B);
").

:- pragma foreign_proc("C",
    fixsub(A::in, B::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixsub(A, B);
").

:- pragma foreign_proc("C",
    fixtorad_r = (FixToRad::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FixToRad = fixtorad_r;
").

:- pragma foreign_proc("C",
    radtofix_r = (RadToFix::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    RadToFix = radtofix_r;
").

:- pragma foreign_proc("C",
    fixsin(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixsin(A);
").

:- pragma foreign_proc("C",
    fixcos(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixcos(A);
").

:- pragma foreign_proc("C",
    fixtan(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixtan(A);
").

:- pragma foreign_proc("C",
    fixasin(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixasin(A);
").

:- pragma foreign_proc("C",
    fixacos(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixacos(A);
").

:- pragma foreign_proc("C",
    fixatan(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixatan(A);
").

:- pragma foreign_proc("C",
    fixatan2(Y::in, X::in) = (R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = fixatan2(Y, X);
").

:- pragma foreign_proc("C",
    fixsqrt(A::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixsqrt(A);
").

:- pragma foreign_proc("C",
    fixhypot(A::in, B::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = fixhypot(A, B);
").

%-----------------------------------------------------------------------------%

floor(X)    = fixfloor(X).
ceil(X)     = fixceil(X).
A * B       = fixmul(A, B).
A / B       = fixdiv(A, B).
A + B       = fixadd(A, B).
A - B       = fixsub(A, B).
sin(X)      = fixsin(X).
cos(X)      = fixcos(X).
tan(X)      = fixtan(X).
asin(X)     = fixasin(X).
acos(X)     = fixacos(X).
atan(X)     = fixatan(X).
atan2(Y, X) = fixatan2(Y, X).
sqrt(X)     = fixsqrt(X).
hypot(X, Y) = fixhypot(X, Y).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    fixed_to_raw_int(X::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = X;
").

:- pragma foreign_proc("C",
    raw_int_to_fixed(I::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = I;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
