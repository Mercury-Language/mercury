%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001-2007 The University of Melbourne.
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

%
% Arithmetic functions
%

	% addition
	%
:- func (float::in) + (float::in) = (float::uo) is det.

	% subtraction
	%
:- func (float::in) - (float::in) = (float::uo) is det.

	% multiplication
	%
:- func (float::in) * (float::in) = (float::uo) is det.

	% division
	% Throws a `math.domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % this check.
	%
:- func (float::in) / (float::in) = (float::uo) is det.

	% unchecked_quotient(X, Y) is the same as X / Y, but the behaviour
    % is undefined if the right operand is zero.
	%
:- func unchecked_quotient(float::in, float::in) = (float::uo) is det.

	% unary plus
	%
:- func + (float::in) = (float::uo) is det.

	% unary minus
	%
:- func - (float::in) = (float::uo) is det.

%
% Comparison predicates
%

	% less than, greater than, less than or equal, greater than or equal.
	%
:- pred (float::in)  < (float::in) is semidet.
:- pred (float::in) =< (float::in) is semidet.
:- pred (float::in) >= (float::in) is semidet.
:- pred (float::in) >  (float::in) is semidet.

%
% Conversion functions
%

	% Convert int to float
	%
:- func float(int) = float.

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

	% truncate_to_int(X) returns
	% the integer closest to X such that |truncate_to_int(X)| =< |X|.
	%
:- func truncate_to_int(float) = int.

%
% Miscellaneous functions
%

	% absolute value
	%
:- func abs(float) = float.

	% maximum
	%
:- func max(float, float) = float.

	% minimum
	%
:- func min(float, float) = float.

	% pow(Base, Exponent) returns Base raised to the power Exponent.
	% Fewer domain restrictions than math.pow: works for negative Base,
	% and float.pow(B, 0) = 1.0 for all B, even B=0.0.
	% Only pow(0, <negative>) throws a `math.domain_error' exception.
	%
:- func pow(float, int) = float.

	% Compute a non-negative integer hash value for a float.
	%
:- func hash(float) = int.

	% Is the float point number not a number or infinite?
	%
:- pred is_nan_or_inf(float::in) is semidet.

	% Is the floating point number not a number?
	%
:- pred is_nan(float::in) is semidet.

	% Is the floating point number infinite?
	%
:- pred is_inf(float::in) is semidet.

%
% System constants
%

	% Maximum finite floating-point number
	%
	% max = (1 - radix ** mantissa_digits) * radix ** max_exponent
	%
:- func float.max = float.

	% Minimum normalised positive floating-point number
	%
	% min = radix ** (min_exponent - 1)
	%
:- func float.min = float.

	% Smallest number x such that 1.0 + x \= 1.0
	% This represents the largest relative spacing of two
	% consecutive floating point numbers.
	%
	% epsilon = radix ** (1 - mantissa_digits)
	%
:- func float.epsilon = float.

	% Radix of the floating-point representation.
	% In the literature, this is sometimes referred to as `b'.
	%
:- func float.radix = int.

	% The number of base-radix digits in the mantissa.  In the
	% literature, this is sometimes referred to as `p' or `t'.
	%
:- func float.mantissa_digits = int.

	% Minimum negative integer such that:
	%	radix ** (min_exponent - 1)
	% is a normalised floating-point number.  In the literature,
	% this is sometimes referred to as `e_min'.
	%
:- func float.min_exponent = int.

	% Maximum integer such that:
	%	radix ** (max_exponent - 1)
	% is a normalised floating-point number.  In the literature,
	% this is sometimes referred to as `e_max'.
	%
:- func float.max_exponent = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module math.

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
	( float_domain_checks, Y = 0.0 ->
		throw(math.domain_error("float:'/'"))
	;
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
	succeeded = true;
").

%---------------------------------------------------------------------------%
%
% Conversion functions
%
%	For Java, overflows are not detected, so this must be tested for
%	explicitly.  So every time there's a cast to int, the bounds are
%	checked first.

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

:- pragma foreign_proc("C",
	float.ceiling_to_int(X :: in) = (Ceil :: out),
	[will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
	Ceil = (MR_Integer) ceil(X);
").
:- pragma foreign_proc("C#",
	float.ceiling_to_int(X :: in) = (Ceil :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Ceil = System.Convert.ToInt32(System.Math.Ceiling(X));
").
:- pragma foreign_proc("Java",
	float.ceiling_to_int(X :: in) = (Ceil :: out),
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

:- pragma foreign_proc("C",
	float.floor_to_int(X :: in) = (Floor :: out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Floor = (MR_Integer) floor(X);
").
:- pragma foreign_proc("C#",
	float.floor_to_int(X :: in) = (Floor :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Floor = System.Convert.ToInt32(System.Math.Floor(X));
").
:- pragma foreign_proc("Java",
	float.floor_to_int(X :: in) = (Floor :: out),
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

:- pragma foreign_proc("C",
	float.round_to_int(X :: in) = (Round :: out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Round = (MR_Integer) floor(X + 0.5);
").
:- pragma foreign_proc("C#",
	float.round_to_int(X :: in) = (Round :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Round = System.Convert.ToInt32(System.Math.Floor(X + 0.5));
").
:- pragma foreign_proc("Java",
	float.round_to_int(X :: in) = (Round :: out),
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

:- pragma foreign_proc("C",
	float.truncate_to_int(X :: in) = (Trunc :: out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Trunc = (MR_Integer) X;
").
:- pragma foreign_proc("C#",
	float.truncate_to_int(X :: in) = (Trunc :: out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Trunc = System.Convert.ToInt32(X);
").
:- pragma foreign_proc("Java",
	float.truncate_to_int(X :: in) = (Trunc :: out),
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

%---------------------------------------------------------------------------%
%
% Miscellaneous functions
%

float.abs(Num) = Abs :-
	( Num =< 0.0 ->
		Abs = - Num
	;
		Abs = Num
	).

float.max(X, Y) = Max :-
	( X >= Y ->
		Max = X
	;
		Max = Y
	).

float.min(X, Y) = Min :-
	( X =< Y ->
		Min = X
	;
		Min = Y
	).

float.pow(Base, Exp) = Ans :-
	( Exp >= 0 ->
		Ans = float.multiply_by_pow(1.0, Base, Exp)
	;
		( float_domain_checks, Base = 0.0 ->
			throw(math.domain_error("float:pow"))
		;
			Ans = unchecked_quotient(1.0,
				float.multiply_by_pow(1.0, Base, -Exp))
			% See below re use of unchecked_quotient.
		)
	).

	% Returns Scale0 * (Base ** Exp) (where X ** 0 == 1.0 for all X).
	% Requires that Exp >= 0.
	% Uses a simple "Russian peasants" algorithm.  O(lg(Exp+1)).
    %
:- func float.multiply_by_pow(float, float, int) = float.

float.multiply_by_pow(Scale0, Base, Exp) = Result :-
	( Exp = 0 ->
		Result = Scale0
	;
		( odd(Exp) ->
			Scale1 = Scale0 * Base
		;
			Scale1 = Scale0
		),
		Result = float.multiply_by_pow(Scale1, Base * Base, Exp div 2)
	).

	% The reason for using unchecked_quotient in float.pow is so
	% that float.pow(+/-0.5, -1111) gives +/-infinity rather than
	% a domain error.  (N.B. This relies on unchecked_quotient(1.0,
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
	% then-current value of Base.  This optimization would be especially
	% valuable for expensive `*' operations; maybe provide a
	% std_util.monoid_pow(func(T,T)=T MonoidOperator, T Identity, int
	% Exp, T Base) = T Result function to complement the existing
	% std_util.pow function.)

%---------------------------------------------------------------------------%

	% In hashing a float in .NET or Java, we ensure that the value is
	% non-negative, as this condition is not guaranteed by either API.
:- pragma foreign_proc("C",
	float.hash(F::in) = (H::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	H = MR_hash_float(F);
").
:- pragma foreign_proc("C#",
	float.hash(F::in) = (H::out),
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
	float.hash(F::in) = (H::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	int Code = (new java.lang.Double(F)).hashCode();
	if (Code < 0) {
		H = -Code ^ 1;
	} else {
		H = Code;
	}
").

%---------------------------------------------------------------------------%

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
:- pragma foreign_proc(il,
	is_nan(Flt::in),
	[will_not_call_mercury, promise_pure, thread_safe, max_stack_size(1)],
"
	ldloc 'Flt'
	call bool [mscorlib]System.Double::IsNaN(float64)
	stloc 'succeeded'
").
:- pragma foreign_proc("Java",
	is_nan(Flt::in),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	succeeded = java.lang.Double.isNaN(Flt);
").

:- pragma foreign_proc("C",
	is_inf(Flt::in),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	SUCCESS_INDICATOR = MR_is_inf(Flt);
").
:- pragma foreign_proc(il,
	is_inf(Flt::in),
	[will_not_call_mercury, promise_pure, thread_safe, max_stack_size(1)],
"
	ldloc 'Flt'
	call bool [mscorlib]System.Double::IsInfinity(float64)
	stloc 'succeeded'
").
:- pragma foreign_proc("Java",
	is_inf(Flt::in),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	succeeded = java.lang.Double.isInfinite(Flt);
").

%---------------------------------------------------------------------------%
%
% System constants
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

:- pragma foreign_proc("C",
	float.max = (Max::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Max = ML_FLOAT_MAX;
").
:- pragma foreign_proc("C#",
	float.max = (Max::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Max = System.Double.MaxValue;
").
:- pragma foreign_proc("Java",
	float.max = (Max::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Max = java.lang.Double.MAX_VALUE;
").

:- pragma foreign_proc("C",
	float.min = (Min::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Min = ML_FLOAT_MIN;
").
% C#:
%	We can't use System.Double.MinValue, because in v1 of the .NET CLR
%	that means something completely different: the negative number
%	with the greatest absolute value.
%	Instead, we just hard-code the appropriate value (copied from the
%	glibc header files); this is OK, because the ECMA specification
%	nails down the representation of double as 64-bit IEEE.
%
% Java:
%	The Java API's java.lang.Double.MIN_VALUE is not normalized,
%	so can't be used, so we use the same constant as for .NET, as the
%	Java Language Specification also describes Java doubles as 64 bit IEEE.
%
float.min = 2.2250738585072014e-308.

:- pragma foreign_proc("C",
	float.epsilon = (Eps::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Eps = ML_FLOAT_EPSILON;
").
% C#:
%	We can't use System.Double.Epsilon, because in v1 of the .NET CLR,
%	that means something completely different: the smallest (denormal)
%	positive number.  I don't know what the people who designed that
%	were smoking; that semantics for 'epsilon' is different from the
%	use of 'epsilon' in C, Lisp, Ada, etc., not to mention Mercury.
%	Instead, we just hard-code the appropriate value (copied from the
%	glibc header files); this is OK, because the ECMA specification
%	nails down the representation of double as 64-bit IEEE.
%
% Java:
%	The Java API doesn't provide an epsilon constant, so we use the
%	same constant, which is ok since Java defines doubles as 64 bit IEEE.
%
float.epsilon = 2.2204460492503131e-16.

:- pragma foreign_proc("C",
	float.radix = (Radix::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	Radix = ML_FLOAT_RADIX;
").
% C#:
%	The ECMA specification requires that double be 64-bit IEEE.
%	I think that implies that it must have Radix = 2.
%	This is definitely right for x86, anyway.
%
% Java:
%	The Java API doesn't provide this constant either, so we default to the
%	same constant as .NET, which is ok since Java defines doubles as 64 bit
%	IEEE.
%
float.radix = 2.

:- pragma foreign_proc("C",
	float.mantissa_digits = (MantDig::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	MantDig = ML_FLOAT_MANT_DIG;
").
% C#:
%	ECMA specifies that System.Double is 64-bit IEEE float
%
% Java:
%	The Java API doesn't provide this constant either, so we default to the
%	same constant as .NET, which is ok since Java defines doubles as 64 bit
%	IEEE.
%
float.mantissa_digits = 53.

:- pragma foreign_proc("C",
	float.min_exponent = (MinExp::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	MinExp = ML_FLOAT_MIN_EXP;
").
% C#:
%	ECMA specifies that System.Double is 64-bit IEEE float
%
% Java:
%	The Java API doesn't provide this constant either, so we default to the
%	same constant as .NET, which is ok since Java defines doubles as 64 bit
%	IEEE.
%
float.min_exponent = -1021.

:- pragma foreign_proc("C",
	float.max_exponent = (MaxExp::out),
	[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
	MaxExp = ML_FLOAT_MAX_EXP;
").
% C#:
%	ECMA specifies that System.Double is 64-bit IEEE float
%
% Java:
%	The Java API doesn't provide this constant either, so we default to the
%	same constant as .NET, which is ok since Java defines doubles as 64 bit
%	IEEE.
%
float.max_exponent = 1024.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
