%---------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: math.m
% Main author: bromage
% Stability: high
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

:- module math.
:- interface.

%---------------------------------------------------------------------------%
% Mathematical constants

	% Pythagoras' number
:- func math__pi = float.
:- mode math__pi = out is det.

	% Base of natural logarithms
:- func math__e = float.
:- mode math__e = out is det.

%---------------------------------------------------------------------------%
% "Next integer" operations

	% math__ceiling(X) = Ceil is true if Ceil is the smallest integer
	% not less than X.
:- func math__ceiling(float) = float.
:- mode math__ceiling(in) = out is det.

	% math__floor(X) = Floor is true if Floor is the largest integer
	% not greater than X.
:- func math__floor(float) = float.
:- mode math__floor(in) = out is det.

	% math__round(X) = Round is true if Round is the integer
	% closest to X.  If X has a fractional value of 0.5, it
	% is rounded up.
:- func math__round(float) = float.
:- mode math__round(in) = out is det.

	% math__truncate(X) = Trunc is true if Trunc is the integer
	% closest to X such that |Trunc| =< |X|.
:- func math__truncate(float) = float.
:- mode math__truncate(in) = out is det.

%---------------------------------------------------------------------------%
% Polynomial roots

	% math__sqrt(X) = Sqrt is true if Sqrt is the positive square
	% root of X.
	%
	% Domain restriction: X >= 0
:- func math__sqrt(float) = float.
:- mode math__sqrt(in) = out is det.

:- type math__quadratic_roots
	--->	no_roots
	;	one_root(float)
	;	two_roots(float, float).

	% math__solve_quadratic(A, B, C) = Roots is true if Roots are
	% the solutions to the equation Ax^2 + Bx + C.
	%
	% Domain restriction: A \= 0
:- func math__solve_quadratic(float, float, float) = quadratic_roots.
:- mode math__solve_quadratic(in, in, in) = out is det.

%---------------------------------------------------------------------------%
% Power/logarithm operations

	% math__pow(X, Y) = Res is true if Res is X raised to the
	% power of Y.
	%
	% Domain restriction: X >= 0 and (X = 0 implies Y > 0)
:- func math__pow(float, float) = float.
:- mode math__pow(in, in) = out is det.

	% math__exp(X) = Exp is true if Exp is e raised to the
	% power of X.
:- func math__exp(float) = float.
:- mode math__exp(in) = out is det.

	% math__ln(X) = Log is true if Log is the natural logarithm
	% of X.
	%
	% Domain restriction: X > 0
:- func math__ln(float) = float.
:- mode math__ln(in) = out is det.

	% math__log10(X) = Log is true if Log is the logarithm to
	% base 10 of X.
	%
	% Domain restriction: X > 0
:- func math__log10(float) = float.
:- mode math__log10(in) = out is det.

	% math__log2(X) = Log is true if Log is the logarithm to
	% base 2 of X.
	%
	% Domain restriction: X > 0
:- func math__log2(float) = float.
:- mode math__log2(in) = out is det.

	% math__log(B, X) = Log is true if Log is the logarithm to
	% base B of X.
	%
	% Domain restriction: X > 0 and B > 0 and B \= 1
:- func math__log(float, float) = float.
:- mode math__log(in, in) = out is det.

%---------------------------------------------------------------------------%
% Trigonometric operations

	% math__sin(X) = Sin is true if Sin is the sine of X.
:- func math__sin(float) = float.
:- mode math__sin(in) = out is det.

	% math__cos(X) = Cos is true if Cos is the cosine of X.
:- func math__cos(float) = float.
:- mode math__cos(in) = out is det.

	% math__tan(X) = Tan is true if Tan is the tangent of X.
:- func math__tan(float) = float.
:- mode math__tan(in) = out is det.

	% math__asin(X) = ASin is true if ASin is the inverse
	% sine of X, where ASin is in the range [-pi/2,pi/2].
	%
	% Domain restriction: X must be in the range [-1,1]
:- func math__asin(float) = float.
:- mode math__asin(in) = out is det.

	% math__acos(X) = ACos is true if ACos is the inverse
	% cosine of X, where ACos is in the range [0, pi].
	%
	% Domain restriction: X must be in the range [-1,1]
:- func math__acos(float) = float.
:- mode math__acos(in) = out is det.

	% math__atan(X) = ATan is true if ATan is the inverse
	% tangent of X, where ATan is in the range [-pi/2,pi/2].
:- func math__atan(float) = float.
:- mode math__atan(in) = out is det.

	% math__atan2(Y, X) = ATan is true if ATan is the inverse
	% tangent of Y/X, where ATan is in the range [-pi,pi].
:- func math__atan2(float, float) = float.
:- mode math__atan2(in, in) = out is det.

%---------------------------------------------------------------------------%
% Hyperbolic functions

	% math__sinh(X) = Sinh is true if Sinh is the hyperbolic
	% sine of X.
:- func math__sinh(float) = float.
:- mode math__sinh(in) = out is det.

	% math__cosh(X) = Cosh is true if Cosh is the hyperbolic
	% cosine of X.
:- func math__cosh(float) = float.
:- mode math__cosh(in) = out is det.

	% math__tanh(X) = Tanh is true if Tanh is the hyperbolic
	% tangent of X.
:- func math__tanh(float) = float.
:- mode math__tanh(in) = out is det.

	% A domain error exception, indicates that the inputs to a function
	% were outside the domain of the function.  The string indicates
	% where the error occured.
	%
	% It is possible to switch domain checking off, in which case,
	% depending on the backend, a domain error may cause a program
	% abort.
	
:- type domain_error ---> domain_error(string).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
:- import_module float, exception.

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

	#define	ML_FLOAT_E		2.7182818284590452354
	#define	ML_FLOAT_PI		3.1415926535897932384
	#define	ML_FLOAT_LN2		0.69314718055994530941

"). % end pragma foreign_decl

:- pragma foreign_code("C#", "

	// This is not defined in the .NET Frameworks.
	// For pi and e we use the constants defined in System.Math.

	public static double ML_FLOAT_LN2 = 0.69314718055994530941;
	

").

:- pred domain_checks is semidet.

:- pragma foreign_proc("C", domain_checks,
		[will_not_call_mercury, promise_pure, thread_safe], "
#ifdef ML_OMIT_MATH_DOMAIN_CHECKS
	SUCCESS_INDICATOR = MR_FALSE;
#else
	SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("MC++", domain_checks,
		[thread_safe, promise_pure], "
#if ML_OMIT_MATH_DOMAIN_CHECKS
	SUCCESS_INDICATOR = MR_FALSE;
#else
	SUCCESS_INDICATOR = MR_TRUE;
#endif
").

domain_checks :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	semidet_succeed,
	private_builtin__sorry("domain_checks").

%
% Mathematical constants from math.m
%
	% Pythagoras' number
:- pragma foreign_proc("C", math__pi = (Pi::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	Pi = ML_FLOAT_PI;
").
:- pragma foreign_proc("C#", math__pi = (Pi::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Pi = System.Math.PI;
").
math__pi = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__pi").

	% Base of natural logarithms
:- pragma foreign_proc("C", math__e = (E::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	E = ML_FLOAT_E;
").
:- pragma foreign_proc("C#", math__e = (E::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	E = System.Math.E;
").
math__e = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__e").

%
% math__ceiling(X) = Ceil is true if Ceil is the smallest integer
% not less than X.
%
:- pragma foreign_proc("C", math__ceiling(Num::in) = (Ceil::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	Ceil = ceil(Num);
").
:- pragma foreign_proc("C#", math__ceiling(Num::in) = (Ceil::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	Ceil = System.Math.Ceiling(Num);
").
math__ceiling(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__ceiling").

%
% math__floor(X) = Floor is true if Floor is the largest integer
% not greater than X.
%
:- pragma foreign_proc("C", math__floor(Num::in) = (Floor::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	Floor = floor(Num);
").
:- pragma foreign_proc("C#", math__floor(Num::in) = (Floor::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	Floor = System.Math.Floor(Num);
").
math__floor(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__floor").

%
% math__round(X) = Round is true if Round is the integer
% closest to X.  If X has a fractional component of 0.5,
% it is rounded up.
%
:- pragma foreign_proc("C", math__round(Num::in) = (Rounded::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	Rounded = floor(Num+0.5);
").
:- pragma foreign_proc("C#", math__round(Num::in) = (Rounded::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	// XXX the semantics of System.Math.Round() are not the same as ours.
	// Unfortunately they are better (round to nearest even number).
	Rounded = System.Math.Floor(Num+0.5);
").
math__round(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__round").

%
% math__truncate(X) = Trunc is true if Trunc is the integer
% closest to X such that |Trunc| =< |X|.
%
math__truncate(X) = (X < 0.0 -> math__ceiling(X) ; math__floor(X)).

%
% math__sqrt(X) = Sqrt is true if Sqrt is the positive square
% root of X.  
%
% Domain restrictions:
%		X >= 0
%
math__sqrt(X) = SquareRoot :-
	( domain_checks, X < 0.0 ->
		throw(domain_error("math__sqrt"))
	;
		SquareRoot = math__sqrt_2(X)
	).

:- func math__sqrt_2(float) = float.

:- pragma foreign_proc("C", math__sqrt_2(X::in) = (SquareRoot::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	SquareRoot = sqrt(X);
").
:- pragma foreign_proc("C#", math__sqrt_2(X::in) = (SquareRoot::out),
		[thread_safe, promise_pure], "
	SquareRoot = System.Math.Sqrt(X);
").
math__sqrt_2(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__sqrt_2").

%
% math__solve_quadratic(A, B, C) = Roots is true if Roots are
% the solutions to the equation Ax^2 + Bx + C.
%
% Domain restrictions:
% 		A \= 0
%
math__solve_quadratic(A, B, C) = Roots :-
	%
	% This implementation is designed to minimise numerical errors;
	% it is adapted from "Numerical recipes in C".
	%
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

%
% math__pow(X, Y) = Res is true if Res is X raised to the
% power of Y.
%
% Domain restrictions:
%		X >= 0
%		X = 0 implies Y > 0
%
math__pow(X, Y) = Res :-
	( domain_checks, X < 0.0 ->
		throw(domain_error("math__pow"))
	; X = 0.0 ->
		( Y =< 0.0 ->
			throw(domain_error("math__pow"))
		;
			Res = 0.0
		)
	;
		Res = math__pow_2(X, Y)
	).

:- func math__pow_2(float, float) = float.

:- pragma foreign_proc("C", math__pow_2(X::in, Y::in) = (Res::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	Res = pow(X, Y);
").

:- pragma foreign_proc("C#", math__pow_2(X::in, Y::in) = (Res::out),
		[thread_safe, promise_pure], "
	Res = System.Math.Pow(X, Y);
").
math__pow_2(_, _) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__pow_2").


%
% math__exp(X) = Exp is true if Exp is X raised to the
% power of e.
%
:- pragma foreign_proc("C", math__exp(X::in) = (Exp::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Exp = exp(X);
").
:- pragma foreign_proc("C#", math__exp(X::in) = (Exp::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Exp = System.Math.Exp(X);
").
math__exp(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__exp").

%
% math__ln(X) = Log is true if Log is the natural logarithm
% of X.
%
% Domain restrictions:
%		X > 0
%
math__ln(X) = Log :-
	( domain_checks, X =< 0.0 ->
		throw(domain_error("math__ln"))
	;
		Log = math__ln_2(X)
	).

:- func math__ln_2(float) = float.

:- pragma foreign_proc("C", math__ln_2(X::in) = (Log::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	Log = log(X);
").
:- pragma foreign_proc("C#", math__ln_2(X::in) = (Log::out),
		[thread_safe, promise_pure], "
	Log = System.Math.Log(X);
").
math__ln_2(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__ln_2").

%
% math__log10(X) = Log is true if Log is the logarithm to
% base 10 of X.
%
% Domain restrictions:
%		X > 0
%
math__log10(X) = Log :-
	( domain_checks, X =< 0.0 ->
		throw(domain_error("math__log10"))
	;
		Log = math__log10_2(X)
	).

:- func math__log10_2(float) = float.

:- pragma foreign_proc("C", math__log10_2(X::in) = (Log10::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	Log10 = log10(X);
").
:- pragma foreign_proc("C#", math__log10_2(X::in) = (Log10::out),
		[thread_safe, promise_pure], "
	Log10 = System.Math.Log10(X);
").
math__log10_2(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__log10_2").

%
% math__log2(X) = Log is true if Log is the logarithm to
% base 2 of X.
%
% Domain restrictions:
%		X > 0
%
math__log2(X) = Log :-
	( domain_checks, X =< 0.0 ->
		throw(domain_error("math__log2"))
	;
		Log = math__log2_2(X)
	).

:- func math__log2_2(float) = float.

:- pragma foreign_proc("C", math__log2_2(X::in) = (Log2::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	Log2 = log(X) / ML_FLOAT_LN2;
").
:- pragma foreign_proc("C#", math__log2_2(X::in) = (Log2::out),
		[thread_safe, promise_pure], "
	Log2 = System.Math.Log(X) / ML_FLOAT_LN2;
").
math__log2_2(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__log2_2").

%
% math__log(B, X) = Log is true if Log is the logarithm to
% base B of X.
%
% Domain restrictions:
%		X > 0
%		B > 0
%		B \= 1
%
math__log(B, X) = Log :-
	(
		domain_checks,
		( X =< 0.0
		; B =< 0.0
		; B = 1.0
		)
	->
		throw(domain_error("math__log"))
	;
		Log = math__log_2(B, X)
	).

:- func math__log_2(float, float) = float.

:- pragma foreign_proc("C", math__log_2(B::in, X::in) = (Log::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	Log = log(X)/log(B);
").
:- pragma foreign_proc("C#", math__log_2(B::in, X::in) = (Log::out),
		[thread_safe, promise_pure], "
	Log = System.Math.Log(X,B);
").
math__log_2(_, _) = _ -
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__log_2").


%
% math__sin(X) = Sin is true if Sin is the sine of X.
%
:- pragma foreign_proc("C", math__sin(X::in) = (Sin::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Sin = sin(X);
").
:- pragma foreign_proc("C#", math__sin(X::in) = (Sin::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Sin = System.Math.Sin(X);
").
math__sin(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__sin").


%
% math__cos(X) = Sin is true if Cos is the cosine of X.
%
:- pragma foreign_proc("C", math__cos(X::in) = (Cos::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Cos = cos(X);
").
:- pragma foreign_proc("C#", math__cos(X::in) = (Cos::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Cos = System.Math.Cos(X);
").
math__cos(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__cos").

%
% math__tan(X) = Tan is true if Tan is the tangent of X.
%
:- pragma foreign_proc("C", math__tan(X::in) = (Tan::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Tan = tan(X);
").
:- pragma foreign_proc("C#", math__tan(X::in) = (Tan::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Tan = System.Math.Tan(X);
").
math__tan(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__tan").

%
% math__asin(X) = ASin is true if ASin is the inverse
% sine of X, where ASin is in the range [-pi/2,pi/2].
%
% Domain restrictions:
%		X must be in the range [-1,1]
%
math__asin(X) = ASin :-
	(
		domain_checks,
		( X < -1.0
		; X > 1.0
		)
	->
		throw(domain_error("math__asin"))
	;
		ASin = math__asin_2(X)
	).

:- func math__asin_2(float) = float.

:- pragma foreign_proc("C", math__asin_2(X::in) = (ASin::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	ASin = asin(X);
").
:- pragma foreign_proc("C#", math__asin_2(X::in) = (ASin::out),
		[thread_safe, promise_pure], "
	ASin = System.Math.Asin(X);
").
math__asin_2(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__asin_2").

%
% math__acos(X) = ACos is true if ACos is the inverse
% cosine of X, where ACos is in the range [0, pi].
%
% Domain restrictions:
%		X must be in the range [-1,1]
%
math__acos(X) = ACos :-
	(
		domain_checks,
		( X < -1.0
		; X > 1.0
		)
	->
		throw(domain_error("math__acos"))
	;
		ACos = math__acos_2(X)
	).

:- func math__acos_2(float) = float.

:- pragma foreign_proc("C", math__acos_2(X::in) = (ACos::out),
		[will_not_call_mercury, promise_pure, thread_safe], "
	ACos = acos(X);
").
:- pragma foreign_proc("C#", math__acos_2(X::in) = (ACos::out),
		[thread_safe, promise_pure], "
	ACos = System.Math.Acos(X);
").
math__acos_2(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__acos_2").


%
% math__atan(X) = ATan is true if ATan is the inverse
% tangent of X, where ATan is in the range [-pi/2,pi/2].
%
:- pragma foreign_proc("C", math__atan(X::in) = (ATan::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	ATan = atan(X);
").
:- pragma foreign_proc("C#", math__atan(X::in) = (ATan::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	ATan = System.Math.Atan(X);
").
math__atan(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__atan").

%
% math__atan2(Y, X) = ATan is true if ATan is the inverse
% tangent of Y/X, where ATan is in the range [-pi,pi].
%
:- pragma foreign_proc("C", math__atan2(Y::in, X::in) = (ATan2::out), 
		[will_not_call_mercury, promise_pure, thread_safe], "
	ATan2 = atan2(Y, X);
").
:- pragma foreign_proc("C#", math__atan2(Y::in, X::in) = (ATan2::out), 
		[will_not_call_mercury, promise_pure, thread_safe], "
	ATan2 = System.Math.Atan2(Y, X);
").
math__atan2(_, _) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__atan2").

%
% math__sinh(X) = Sinh is true if Sinh is the hyperbolic
% sine of X.
%
:- pragma foreign_proc("C", math__sinh(X::in) = (Sinh::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Sinh = sinh(X);
").
:- pragma foreign_proc("C#", math__sinh(X::in) = (Sinh::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Sinh = System.Math.Sinh(X);
").
math__sinh(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__sinh").

%
% math__cosh(X) = Cosh is true if Cosh is the hyperbolic
% cosine of X.
%
:- pragma foreign_proc("C", math__cosh(X::in) = (Cosh::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Cosh = cosh(X);
").
:- pragma foreign_proc("C#", math__cosh(X::in) = (Cosh::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Cosh = System.Math.Cosh(X);
").
math__cosh(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__cosh").

%
% math__tanh(X) = Tanh is true if Tanh is the hyperbolic
% tangent of X.
%
:- pragma foreign_proc("C", math__tanh(X::in) = (Tanh::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Tanh = tanh(X);
").
:- pragma foreign_proc("C#", math__tanh(X::in) = (Tanh::out),
		[will_not_call_mercury, promise_pure, thread_safe],"
	Tanh = System.Math.Tanh(X);
").
math__tanh(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("math__tanh").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
