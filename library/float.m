%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: float.m.
% Main author: fjh.
% Stability: medium.
%
% Floating point support.
%
% Note that implementations which support IEEE floating point
% should ensure that in cases where the only valid answer is a "NaN"
% (the IEEE float representation for "not a number"), the det
% functions here will halt with a runtime error (or throw an exception)
% rather than returning a NaN.  Quiet (non-signalling) NaNs have a
% semantics which is not valid in Mercury, since they don't obey the
% axiom "all [X] X = X".
%
% XXX Unfortunately the current Mercury implementation does not
% do that on all platforms, since neither ANSI C nor POSIX provide
% any portable way of ensuring that floating point operations
% whose result is not representable will raise a signal rather
% than returning a NaN.  (Maybe C9X will help...?)
% The behaviour is correct on Linux and Digital Unix,
% but not on Solaris, for example.
%
% IEEE floating point also specifies that some functions should
% return different results for +0.0 and -0.0, but that +0.0 and -0.0
% should compare equal.  This semantics is not valid in Mercury,
% since it doesn't obey the axiom `all [F, X, Y] X = Y => F(X) = F(Y)'.
% Again, the resolution is that in Mercury, functions which would
% return different results for +0.0 and -0.0 should instead halt
% execution with a run-time error (or throw an exception).
%
% XXX Here too the current Mercury implementation does not
% implement the intended semantics correctly on all platforms.
%
%---------------------------------------------------------------------------%

:- module float.
:- interface.

%
% Arithmetic functions
%

	% addition
:- func float + float = float.
:- mode in    + in    = uo  is det.

	% subtraction
:- func float - float = float.
:- mode in    - in    = uo  is det.

	% multiplication
:- func float * float = float.
:- mode in    * in    = uo  is det.

	% division
	% Throws an `math__domain_error' exception if the right
	% operand is zero. See the comments at the top of math.m
	% to find out how to disable this check.
:- func float / float = float.
:- mode in    / in    = uo  is det.

	% unchecked_quotient(X, Y) is the same as X / Y, but the
	% behaviour is undefined if the right operand is zero.
:- func unchecked_quotient(float, float) = float.
:- mode unchecked_quotient(in, in)    = uo  is det.

	% unary plus
:- func + float = float.
:- mode + in    = uo  is det.

	% unary minus
:- func - float = float.
:- mode - in    = uo  is det.

%
% Comparison predicates
%

	% less than
:- pred <(float, float).
:- mode <(in, in) is semidet.

	% greater than
:- pred >(float, float).
:- mode >(in, in) is semidet.

	% less than or equal
:- pred =<(float, float).
:- mode =<(in, in) is semidet.

	% greater than or equal
:- pred >=(float, float).
:- mode >=(in, in) is semidet.

%
% Conversion functions
%

	% Convert int to float
:- func float(int) = float.

	% ceiling_to_int(X) returns the
	% smallest integer not less than X.
:- func ceiling_to_int(float) = int.

	% floor_to_int(X) returns the
	% largest integer not greater than X.
:- func floor_to_int(float) = int.

	% round_to_int(X) returns the integer closest to X.
	% If X has a fractional value of 0.5, it is rounded up.
:- func round_to_int(float) = int.

	% truncate_to_int(X) returns 
	% the integer closest to X such that |truncate_to_int(X)| =< |X|.
:- func truncate_to_int(float) = int.

%
% Miscellaneous functions
%

	% absolute value
:- func abs(float) = float.

	% maximum
:- func max(float, float) = float.

	% minimum
:- func min(float, float) = float.

	% pow(Base, Exponent) returns Base raised to the power Exponent.
	% The exponent must be an integer greater or equal to 0.
	% Currently this function runs at O(n), where n is the value
	% of the exponent.
:- func pow(float, int) = float.

	% Compute a non-negative integer hash value for a float.
:- func hash(float) = int.

%
% System constants
%

	% Maximum floating-point number
	%
	% max = (1 - radix ** mantissa_digits) * radix ** max_exponent
	%
:- func float__max = float.

	% Minimum normalised floating-point number
	%
	% min = radix ** (min_exponent - 1)
	%
:- func float__min = float.

	% Smallest number x such that 1.0 + x \= 1.0
	% This represents the largest relative spacing of two
	% consecutive floating point numbers.
	%
	% epsilon = radix ** (1 - mantissa_digits)
:- func float__epsilon = float.

	% Radix of the floating-point representation.
	% In the literature, this is sometimes referred to as `b'.
	%
:- func float__radix = int.

	% The number of base-radix digits in the mantissa.  In the
	% literature, this is sometimes referred to as `p' or `t'.
	%
:- func float__mantissa_digits = int.

	% Minimum negative integer such that:
	%	radix ** (min_exponent - 1)
	% is a normalised floating-point number.  In the literature,
	% this is sometimes referred to as `e_min'.
	%
:- func float__min_exponent = int.

	% Maximum integer such that:
	%	radix ** (max_exponent - 1)
	% is a normalised floating-point number.  In the literature,
	% this is sometimes referred to as `e_max'.
	%
:- func float__max_exponent = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
:- interface.

	% Everything below here will not appear in the
	% Mercury Library Reference Manual.

%---------------------------------------------------------------------------%

%
% Obsolete predicate versions of the functions declared above.
% These were intended for use in programs that need to work
% in both Prolog and Mercury. Running Mercury programs using
% Prolog is no longer supported.
%

%
% Conversion predicates
%

	% float__ceiling_to_int(X, Ceil) is true if Ceil is the
	% smallest integer not less than X.
:- pragma obsolete(float__ceiling_to_int/2).
:- pred float__ceiling_to_int(float, int).
:- mode float__ceiling_to_int(in, out) is det.

	% float__floor_to_int(X, Ceil) is true if Ceil is the
	% largest integer not greater than X.
:- pragma obsolete(float__floor_to_int/2).
:- pred float__floor_to_int(float, int).
:- mode float__floor_to_int(in, out) is det.

	% float__round_to_int(X, Round) is true if Round is the
	% integer closest to X.  If X has a fractional value of
	% 0.5, it is rounded up.
:- pragma obsolete(float__round_to_int/2).
:- pred float__round_to_int(float, int).
:- mode float__round_to_int(in, out) is det.

	% float__truncate_to_int(X, Trunc) is true if Trunc is
	% the integer closest to X such that |Trunc| =< |X|.
:- pragma obsolete(float__truncate_to_int/2).
:- pred float__truncate_to_int(float, int).
:- mode float__truncate_to_int(in, out) is det.

%
% Miscellaneous predicates
%

	% absolute value
:- pragma obsolete(float__abs/2).
:- pred float__abs(float, float).
:- mode float__abs(in, out) is det.

	% maximum
:- pragma obsolete(float__max/3).
:- pred float__max(float, float, float).
:- mode float__max(in, in, out) is det.

	% minimum
:- pragma obsolete(float__min/3).
:- pred float__min(float, float, float).
:- mode float__min(in, in, out) is det.

	% float__pow(Base, Exponent, Answer) is true iff Answer is
	% Base raised to the power Exponent. Currently this function runs
	% at O(n), where n is the value of the exponent.
	% Throws a `math__domain_error' exception if the exponent is negative.
:- pragma obsolete(float__pow/3).
:- pred float__pow(float, int, float).
:- mode float__pow(in, in, out) is det.

	% Compute a non-negative integer hash value for a float.
:- pragma obsolete(float__hash/2).
:- pred float__hash(float, int).
:- mode float__hash(in, out) is det.

%
% System constant predicates
%

	% Maximum floating-point number
:- pragma obsolete(float__max/1).
:- pred float__max(float).
:- mode float__max(out) is det.

	% Minimum normalised floating-point number
:- pragma obsolete(float__min/1).
:- pred float__min(float).
:- mode float__min(out) is det.

	% Smallest number x such that 1.0 + x \= 1.0
:- pragma obsolete(float__epsilon/1).
:- pred float__epsilon(float).
:- mode float__epsilon(out) is det.

	% Radix of the floating-point representation.
:- pragma obsolete(float__radix/1).
:- pred float__radix(int).
:- mode float__radix(out) is det.

	% The number of base-radix digits in the mantissa.
:- pragma obsolete(float__mantissa_digits/1).
:- pred float__mantissa_digits(int).
:- mode float__mantissa_digits(out) is det.

	% Smallest exponent of a normalised floating-point number.
:- pragma obsolete(float__min_exponent/1).
:- pred float__min_exponent(int).
:- mode float__min_exponent(out) is det.

	% Largest exponent of a normalised floating-point number.
:- pragma obsolete(float__max_exponent/1).
:- pred float__max_exponent(int).
:- mode float__max_exponent(out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
:- import_module exception, int, math.

%
% Header files of mathematical significance.
%

:- pragma foreign_decl("C", "

	#include <float.h>
	#include <math.h>

").

%---------------------------------------------------------------------------%

% The other arithmetic and comparison operators are builtins,
% which the compiler expands inline.  We don't need to define them here.

:- pragma inline('/'/2).
X / Y = Z :-
	( domain_checks, Y = 0.0 ->
		throw(math__domain_error("float:'/'"))
	;
		Z = unchecked_quotient(X, Y)
	).

	% This code is included here rather than just calling
	% the version in math.m because we currently don't do
	% transitive inter-module inlining, so code which uses
	% `/'/2 but doesn't import math.m couldn't have the
	% domain check optimized away..
:- pred domain_checks is semidet.
:- pragma inline(domain_checks/0).

:- pragma foreign_proc("C", domain_checks,
		[will_not_call_mercury, thread_safe], "
#ifdef ML_OMIT_MATH_DOMAIN_CHECKS
	SUCCESS_INDICATOR = FALSE;
#else
	SUCCESS_INDICATOR = TRUE;
#endif
").

:- pragma foreign_proc("MC++", domain_checks,
		[thread_safe], "
#if ML_OMIT_MATH_DOMAIN_CHECKS
	SUCCESS_INDICATOR = FALSE;
#else
	SUCCESS_INDICATOR = TRUE;
#endif
").
%---------------------------------------------------------------------------%
%
% Conversion functions
%

float(Int) = Float :-
	int__to_float(Int, Float).

	% float__ceiling_to_int(X) returns the
	% smallest integer not less than X.
:- pragma foreign_proc("C", float__ceiling_to_int(X :: in) = (Ceil :: out),
	[will_not_call_mercury, thread_safe],
"
	Ceil = (MR_Integer) ceil(X);
").
:- pragma foreign_proc("C#", float__ceiling_to_int(X :: in) = (Ceil :: out),
	[will_not_call_mercury, thread_safe],
"
	Ceil = System.Convert.ToInt32(System.Math.Ceiling(X));
").

float__ceiling_to_int(X, float__ceiling_to_int(X)).

	% float__floor_to_int(X) returns the
	% largest integer not greater than X.
:- pragma foreign_proc("C", float__floor_to_int(X :: in) = (Floor :: out),
	[will_not_call_mercury, thread_safe],
"
	Floor = (MR_Integer) floor(X);
").
:- pragma foreign_proc("C#", float__floor_to_int(X :: in) = (Floor :: out),
	[will_not_call_mercury, thread_safe],
"
	Floor = System.Convert.ToInt32(System.Math.Floor(X));
").

float__floor_to_int(X, float__floor_to_int(X)).

	% float__round_to_int(X) returns the integer closest to X.
	% If X has a fractional value of 0.5, it is rounded up.
:- pragma foreign_proc("C", float__round_to_int(X :: in) = (Round :: out),
	[will_not_call_mercury, thread_safe],
"
	Round = (MR_Integer) floor(X + 0.5);
").
:- pragma foreign_proc("C#", float__round_to_int(X :: in) = (Round :: out),
	[will_not_call_mercury, thread_safe],
"
	Round = System.Convert.ToInt32(System.Math.Floor(X + 0.5));
").

float__round_to_int(X, float__round_to_int(X)).

	% float__truncate_to_int(X) returns the integer closest
	% to X such that |float__truncate_to_int(X)| =< |X|.
:- pragma foreign_proc("C", float__truncate_to_int(X :: in) = (Trunc :: out),
	[will_not_call_mercury, thread_safe],
"
	Trunc = (MR_Integer) X;
").
:- pragma foreign_proc("C#", float__truncate_to_int(X :: in) = (Trunc :: out),
	[will_not_call_mercury, thread_safe],
"
	Trunc = System.Convert.ToInt32(X);
").

float__truncate_to_int(X, float__truncate_to_int(X)).

%---------------------------------------------------------------------------%
%
% Miscellaneous functions
%

float__abs(Num) = Abs :-
	(
		Num =< 0.0
	->
		Abs = - Num
	;
		Abs = Num
	).

float__abs(Num, float__abs(Num)).

float__max(X, Y) = Max :-
	(
		X >= Y
	->
		Max = X
	;
		Max = Y
	).

float__max(X, Y, float__max(X, Y)).

float__min(X, Y) = Min :-
	(
		X =< Y
	->
		Min = X
	;
		Min = Y
	).

float__min(X, Y, float__min(X, Y)).

% float_pow(Base, Exponent) = Answer.
%	XXXX This function could be more efficient, with an int_mod pred, to
%	reduce O(N) to O(logN) of the exponent.
float__pow(X, Exp) = Ans :-
	( Exp < 0 ->
		throw(math__domain_error("float__pow"))
	; Exp = 1 ->
		Ans =  X
	; Exp = 0 ->
		Ans = 1.0
	;
		New_e is Exp - 1,
		Ans is X * float__pow(X, New_e)
	).

float__pow(X, Exp, float__pow(X, Exp)).

:- pragma foreign_proc("C", float__hash(F::in) = (H::out),
	[will_not_call_mercury, thread_safe],
"
	H = MR_hash_float(F);
").
:- pragma foreign_proc("C#", float__hash(F::in) = (H::out),
	[will_not_call_mercury, thread_safe],
"
	H = F.GetHashCode();
").

float__hash(F, float__hash(F)).

%---------------------------------------------------------------------------%
%
% System constants
%
% The floating-point system constants are derived from <float.h> and
% implemented using the C interface.

:- pragma c_header_code("

	#define	ML_FLOAT_RADIX	FLT_RADIX	/* There is no DBL_RADIX. */

	#if defined USE_SINGLE_PREC_FLOAT
		#define	ML_FLOAT_MAX		FLT_MAX
		#define	ML_FLOAT_MIN		FLT_MIN
		#define	ML_FLOAT_EPSILON	FLT_EPSILON
		#define	ML_FLOAT_MANT_DIG	FLT_MANT_DIG
		#define	ML_FLOAT_MIN_EXP	FLT_MIN_EXP
		#define	ML_FLOAT_MAX_EXP	FLT_MAX_EXP
	#else
		#define	ML_FLOAT_MAX		DBL_MAX
		#define	ML_FLOAT_MIN		DBL_MIN
		#define	ML_FLOAT_EPSILON	DBL_EPSILON
		#define	ML_FLOAT_MANT_DIG	DBL_MANT_DIG
		#define	ML_FLOAT_MIN_EXP	DBL_MIN_EXP
		#define	ML_FLOAT_MAX_EXP	DBL_MAX_EXP
	#endif

").

	% Maximum floating-point number
:- pragma foreign_proc("C", float__max = (Max::out),
		[will_not_call_mercury, thread_safe],
	"Max = ML_FLOAT_MAX;").
:- pragma foreign_proc("C#", float__max = (Max::out),
		[will_not_call_mercury, thread_safe],
	"Max = System.Double.MaxValue;").


float__max(float__max).

	% Minimum normalised floating-point number */
:- pragma foreign_proc("C", float__min = (Min::out),
		[will_not_call_mercury, thread_safe],
	"Min = ML_FLOAT_MIN;").
:- pragma foreign_proc("C#", float__min = (Min::out),
		[will_not_call_mercury, thread_safe],
	"Min = System.Double.MinValue;").

float__min(float__min).

	% Smallest x such that x \= 1.0 + x
:- pragma foreign_proc("C", float__epsilon = (Eps::out),
		[will_not_call_mercury, thread_safe],
	"Eps = ML_FLOAT_EPSILON;").
:- pragma foreign_proc("C#", float__epsilon = (Eps::out),
		[will_not_call_mercury, thread_safe],
	"Eps = System.Double.Epsilon;").

float__epsilon(float__epsilon).

	% Radix of the floating-point representation.
:- pragma foreign_proc("C", float__radix = (Radix::out),
		[will_not_call_mercury, thread_safe],
	"Radix = ML_FLOAT_RADIX;").
:- pragma foreign_proc("C#", float__radix = (_Radix::out),
		[will_not_call_mercury, thread_safe], "
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
	_Radix = 0;
").

float__radix(float__radix).

	% The number of base-radix digits in the mantissa.
:- pragma foreign_proc("C", float__mantissa_digits = (MantDig::out),
		[will_not_call_mercury, thread_safe],
	"MantDig = ML_FLOAT_MANT_DIG;").
:- pragma foreign_proc("C#", float__mantissa_digits = (_MantDig::out),
		[will_not_call_mercury, thread_safe], "
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
	_MantDig = 0;
").

float__mantissa_digits(float__mantissa_digits).

	% Minimum negative integer such that:
	%	radix ** (min_exponent - 1)
	% is a normalised floating-point number.
:- pragma foreign_proc("C", float__min_exponent = (MinExp::out),
		[will_not_call_mercury, thread_safe],
	"MinExp = ML_FLOAT_MIN_EXP;").
:- pragma foreign_proc("C#", float__min_exponent = (_MinExp::out),
		[will_not_call_mercury, thread_safe], "	
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
	_MinExp = 0;
").

float__min_exponent(float__min_exponent).

	% Maximum integer such that:
	%	radix ** (max_exponent - 1)
	% is a normalised floating-point number.
:- pragma foreign_proc("C", float__max_exponent = (MaxExp::out),
		[will_not_call_mercury, thread_safe],
	"MaxExp = ML_FLOAT_MAX_EXP;").

:- pragma foreign_proc("C#", float__max_exponent = (_MaxExp::out),
		[will_not_call_mercury, thread_safe], "	
	mercury.runtime.Errors.SORRY(""foreign code for this function"");
	_MaxExp = 0;
").


float__max_exponent(float__max_exponent).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
