%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: float.m.
% Main author: fjh.
% Stability: low.
%
% Floating point support.
%
% The interface provided here is rather low-level and not
% syntactically elegant.  It's a building block, not a finished tool.
%
% XXX - What should we do about unification of two Nan's?
%
%---------------------------------------------------------------------------%

:- module float.
:- interface.
:- import_module int, require.

%---------------------------------------------------------------------------%

:- pred builtin_float_plus(float, float, float).
:- mode builtin_float_plus(in, in, out) is det.

:- pred builtin_float_minus(float, float, float).
:- mode builtin_float_minus(in, in, out) is det.

:- pred builtin_float_times(float, float, float).
:- mode builtin_float_times(in, in, out) is det.

:- pred builtin_float_divide(float, float, float).
:- mode builtin_float_divide(in, in, out) is det.

:- pred builtin_float_gt(float, float).
:- mode builtin_float_gt(in, in) is semidet.

:- pred builtin_float_lt(float, float).
:- mode builtin_float_lt(in, in) is semidet.

:- pred builtin_float_ge(float, float).
:- mode builtin_float_ge(in, in) is semidet.

:- pred builtin_float_le(float, float).
:- mode builtin_float_le(in, in) is semidet.

%---------------------------------------------------------------------------%

:- pred float__pow( float, int, float).
:- mode float__pow( in, in, out) is det.
%	float__pow( Base, Exponent, Answer)
%		A limited way to calculate powers.  The exponent must be an 
%		integer greater or equal to 0.  Currently this function runs
%		at O(n), where n is the value of the exponent.

%---------------------------------------------------------------------------%
% System constants

	% Maximum floating-point number
:- pred float__max(float).
:- mode float__max(out) is det.

	% Minimum normalised floating-point number
:- pred float__min(float).
:- mode float__min(out) is det.

	% Smallest number x such that 1.0 + x \= 1.0
:- pred float__epsilon(float).
:- mode float__epsilon(out) is det.

%---------------------------------------------------------------------------%

:- implementation.

/* All the buitlin_float_* are builtins, which the compiler expands inline. */

% float_pow(Base, Exponent, Answer).
%	XXXX This function could be more efficient, with an int_mod pred, to
%	reduce O(N) to O(logN) of the exponent.
float__pow( X, Exp, Ans) :-
	( Exp < 0 ->
		error("float__pow taken with exponent < 0\n")
	; Exp = 1 ->
		Ans =  X
	; Exp = 0 ->
		Ans = 1.0
	;
		New_e is Exp - 1,
		float__pow(X, New_e, A2),
		builtin_float_times(X, A2, Ans)
	).

%---------------------------------------------------------------------------%

%
% System constants from <float.h>, implemented using the C interface
%

:- pragma(c_header_code, "

	#include <float.h>

	#if defined USE_SINGLE_PREC_FLOAT
		#define	MERCURY_FLOAT_MAX	FLT_MAX
		#define	MERCURY_FLOAT_MIN	FLT_MIN
		#define	MERCURY_FLOAT_EPSILON	FLT_EPSILON
	#else
		#define	MERCURY_FLOAT_MAX	DBL_MAX
		#define	MERCURY_FLOAT_MIN	DBL_MIN
		#define	MERCURY_FLOAT_EPSILON	DBL_EPSILON
	#endif
").

	% Maximum floating-point number
:- pragma(c_code, float__max(Max::out), "Max = MERCURY_FLOAT_MAX;").

	% Minimum normalised floating-point number */
:- pragma(c_code, float__min(Min::out), "Min = MERCURY_FLOAT_MIN;").

	% Smallest x such that x \= 1.0 + x
:- pragma(c_code, float__epsilon(Eps::out), "Eps = MERCURY_FLOAT_EPSILON;").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
