%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001-2002 The University of Melbourne.
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
	% Throws a `math__domain_error' exception if the right
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
	% Fewer domain restrictions than math__pow: works for negative Base,
	% and float__pow(B, 0) = 1.0 for all B, even B=0.0.
	% Only pow(0, <negative>) throws a `math__domain_error' exception.
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
	private_builtin__sorry("float__domain_checks").

%---------------------------------------------------------------------------%
%
% Conversion functions
%

float(Int) = Float :-
	int__to_float(Int, Float).

	% float__ceiling_to_int(X) returns the
	% smallest integer not less than X.
:- pragma foreign_proc("C", float__ceiling_to_int(X :: in) = (Ceil :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Ceil = (MR_Integer) ceil(X);
").
:- pragma foreign_proc("C#", float__ceiling_to_int(X :: in) = (Ceil :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Ceil = System.Convert.ToInt32(System.Math.Ceiling(X));
").
float__ceiling_to_int(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__ceiling_to_int").

	% float__floor_to_int(X) returns the
	% largest integer not greater than X.
:- pragma foreign_proc("C", float__floor_to_int(X :: in) = (Floor :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Floor = (MR_Integer) floor(X);
").
:- pragma foreign_proc("C#", float__floor_to_int(X :: in) = (Floor :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Floor = System.Convert.ToInt32(System.Math.Floor(X));
").
float__floor_to_int(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__floor_to_int").

	% float__round_to_int(X) returns the integer closest to X.
	% If X has a fractional value of 0.5, it is rounded up.
:- pragma foreign_proc("C", float__round_to_int(X :: in) = (Round :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Round = (MR_Integer) floor(X + 0.5);
").
:- pragma foreign_proc("C#", float__round_to_int(X :: in) = (Round :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Round = System.Convert.ToInt32(System.Math.Floor(X + 0.5));
").
float__round_to_int(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__round_to_int").

	% float__truncate_to_int(X) returns the integer closest
	% to X such that |float__truncate_to_int(X)| =< |X|.
:- pragma foreign_proc("C", float__truncate_to_int(X :: in) = (Trunc :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Trunc = (MR_Integer) X;
").
:- pragma foreign_proc("C#", float__truncate_to_int(X :: in) = (Trunc :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Trunc = System.Convert.ToInt32(X);
").
float__truncate_to_int(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__truncate_to_int").

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

float__max(X, Y) = Max :-
	(
		X >= Y
	->
		Max = X
	;
		Max = Y
	).

float__min(X, Y) = Min :-
	(
		X =< Y
	->
		Min = X
	;
		Min = Y
	).


float__pow(Base, Exp) = Ans :-
	( Exp >= 0 ->
		Ans = float__multiply_by_pow(1.0, Base, Exp)
	;
		( domain_checks, Base = 0.0 ->
			throw(math__domain_error("float:pow"))
		;
			Ans = unchecked_quotient(1.0,
				float__multiply_by_pow(1.0, Base, -Exp))
			% See below re use of unchecked_quotient.
		)
	).

:- func float__multiply_by_pow(float, float, int) = float.
	% Returns Scale0 * (Base ** Exp) (where X ** 0 == 1.0 for all X).
	% Requires that Exp >= 0.
	% Uses a simple "Russian peasants" algorithm.  O(lg(Exp+1)).
float__multiply_by_pow(Scale0, Base, Exp) = Result :-
	( Exp = 0 ->
		Result = Scale0
	;
		( odd(Exp) ->
			Scale1 = Scale0 * Base
		;
			Scale1 = Scale0
		),
		Result = float__multiply_by_pow(Scale1, Base * Base, Exp div 2)
	).

	% The reason for using unchecked_quotient in float__pow is so
	% that float__pow(+/-0.5, -1111) gives +/-infinity rather than
	% a domain error.  (N.B. This relies on unchecked_quotient(1.0,
	% +/-0.0) giving +/-infinity, whereas the documentation in
	% float.m says that the results are undefined.)
	% Using Result = float__multiply_by_pow(1.0, 1.0 / Base, -Exp)
	% would give the right behaviour for underflow, but isn't
	% generally as accurate.

	% (Efficiency note: An optimization used by `power' in SGI's STL
	%  implementation is to test for Exp=0 and (for non-zero Exp) handle
	%  low zero bits in Exp before calling this loop: the loop for the low
	%  zero bits needs only square Base, it needn't update Acc until the
	%  end of that loop at which point Acc can be simply assigned from the
	%  then-current value of Base.  This optimization would be especially
	%  valuable for expensive `*' operations; maybe provide a
	%  std_util__monoid_pow(func(T,T)=T MonoidOperator, T Identity, int
	%  Exp, T Base) = T Result function to complement the existing
	%  std_util__pow function.)

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C", float__hash(F::in) = (H::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	H = MR_hash_float(F);
").
:- pragma foreign_proc("C#", float__hash(F::in) = (H::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	H = F.GetHashCode();
").
float__hash(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__hash").

%---------------------------------------------------------------------------%
%
% System constants
%
% The floating-point system constants are derived from <float.h> and
% implemented using the C interface.

:- pragma c_header_code("

	#define	ML_FLOAT_RADIX	FLT_RADIX	/* There is no DBL_RADIX. */

	#if defined MR_USE_SINGLE_PREC_FLOAT
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
		[will_not_call_mercury, promise_pure, thread_safe],
	"Max = ML_FLOAT_MAX;").
:- pragma foreign_proc("C#", float__max = (Max::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"Max = System.Double.MaxValue;").
float__max = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__max").

	% Minimum normalised floating-point number */
:- pragma foreign_proc("C", float__min = (Min::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"Min = ML_FLOAT_MIN;").
:- pragma foreign_proc("C#", float__min = (Min::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"Min = System.Double.MinValue;").
float__min = _ :=
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__min").

	% Smallest x such that x \= 1.0 + x
:- pragma foreign_proc("C", float__epsilon = (Eps::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"Eps = ML_FLOAT_EPSILON;").
:- pragma foreign_proc("C#", float__epsilon = (Eps::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"Eps = System.Double.Epsilon;").
float__epsilon = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__epsilon").

	% Radix of the floating-point representation.
:- pragma foreign_proc("C", float__radix = (Radix::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"Radix = ML_FLOAT_RADIX;").
float__radix = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__radix").

	% The number of base-radix digits in the mantissa.
:- pragma foreign_proc("C", float__mantissa_digits = (MantDig::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"MantDig = ML_FLOAT_MANT_DIG;").
float__mantissa_digits = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__mantissa_digits").

	% Minimum negative integer such that:
	%	radix ** (min_exponent - 1)
	% is a normalised floating-point number.
:- pragma foreign_proc("C", float__min_exponent = (MinExp::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"MinExp = ML_FLOAT_MIN_EXP;").
float__min_exponent = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__min_exponent").

	% Maximum integer such that:
	%	radix ** (max_exponent - 1)
	% is a normalised floating-point number.
:- pragma foreign_proc("C", float__max_exponent = (MaxExp::out),
		[will_not_call_mercury, promise_pure, thread_safe],
	"MaxExp = ML_FLOAT_MAX_EXP;").
float__max_exponent = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("float__max_exponent").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
