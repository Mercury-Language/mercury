%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
% XXX - What should we do about unification of two Nan's?
%
%---------------------------------------------------------------------------%

:- module float.
:- interface.

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

	% absolute value
:- pred float__abs(float, float).
:- mode float__abs(in, out) is det.

	% maximum
:- pred float__max(float, float, float).
:- mode float__max(in, in, out) is det.

	% minumim
:- pred float__min(float, float, float).
:- mode float__min(in, in, out) is det.

	% addition
:- func float + float = float.
:- mode in    + in    = uo  is det.
:- mode uo  + in  = in  is det.
:- mode in  + uo  = in  is det.

	% subtraction
:- func float - float = float.
:- mode in    - in    = uo  is det.
:- mode uo  - in  = in  is det.
:- mode in  - uo  = in  is det.

	% multiplication
:- func float * float = float.
:- mode in    * in    = uo  is det.
:- mode uo  * in  = in  is det.
:- mode in  * uo  = in  is det.

	% division
:- func float / float = float.
:- mode in    / in    = uo  is det.
:- mode uo  / in  = in  is det.
:- mode in  / uo  = in  is det.

	% unary plus
:- func + float = float.
:- mode + in    = uo  is det.

	% unary minus
:- func - float = float.
:- mode - in    = uo  is det.

%---------------------------------------------------------------------------%

/* The following are predicates which do the same thing as the
   above functions.  They are obsolete.  Don't use them.
   They will eventually disappear in some future release.
*/

:- pred builtin_float_plus(float, float, float).
:- mode builtin_float_plus(in, in, uo) is det.

:- pred builtin_float_minus(float, float, float).
:- mode builtin_float_minus(in, in, uo) is det.

:- pred builtin_float_times(float, float, float).
:- mode builtin_float_times(in, in, uo) is det.

:- pred builtin_float_divide(float, float, float).
:- mode builtin_float_divide(in, in, uo) is det.

:- pred builtin_float_gt(float, float).
:- mode builtin_float_gt(in, in) is semidet.

:- pred builtin_float_lt(float, float).
:- mode builtin_float_lt(in, in) is semidet.

:- pred builtin_float_ge(float, float).
:- mode builtin_float_ge(in, in) is semidet.

:- pred builtin_float_le(float, float).
:- mode builtin_float_le(in, in) is semidet.

%---------------------------------------------------------------------------%

        % float__ceiling_to_int(X, Ceil) is true if Ceil is the
	% smallest integer not less than X.
:- pred float__ceiling_to_int(float, int).
:- mode float__ceiling_to_int(in, out) is det.

        % float__floor_to_int(X, Ceil) is true if Ceil is the
	% largest integer not greater than X.
:- pred float__floor_to_int(float, int).
:- mode float__floor_to_int(in, out) is det.

        % float__round_to_int(X, Round) is true if Round is the
	% integer closest to X.  If X has a fractional value of
	% 0.5, it is rounded up.
:- pred float__round_to_int(float, int).
:- mode float__round_to_int(in, out) is det.

        % float__truncate_to_int(X, Trunc) is true if Trunc is
	% the integer closest to X such that |Trunc| =< |X|.
:- pred float__truncate_to_int(float, int).
:- mode float__truncate_to_int(in, out) is det.

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
% Compute a non-negative integer hash value for a float.
% (Note: the hash values computed by the Mercury implementation of
% this predicate are different to those computed by the Prolog
% implementation.)

:- pred float__hash(float, int).
:- mode float__hash(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int, require.

%
% Header files of mathematical significance.
%

:- pragma(c_header_code, "

	#include <float.h>
	#include <math.h>

").

%---------------------------------------------------------------------------%


% The arithmetic and comparison operators are builtins,
% which the compiler expands inline.  We don't need to define them here.

float__abs(Num, Abs) :-
	(
		Num =< 0.0
	->
		Abs = - Num
	;
		Abs = Num
	).

float__max(X, Y, Max) :-
	(
		X >= Y
	->
		Max = X
	;
		Max = Y
	).

float__min(X, Y, Min) :-
	(
		X =< Y
	->
		Min = X
	;
		Min = Y
	).

%---------------------------------------------------------------------------%

        % float__ceiling_to_int(X, Ceil) is true if Ceil is the
	% smallest integer not less than X.
:- pragma(c_code,
	float__ceiling_to_int(X :: in, Ceil :: out),
"
	Ceil = (Integer)ceil(X);
").

        % float__floor_to_int(X, Floor) is true if Floor is the
	% largest integer not greater than X.
:- pragma(c_code,
	float__floor_to_int(X :: in, Floor :: out),
"
	Floor = (Integer)floor(X);
").

        % float__round_to_int(X, Round) is true if Round is the
	% integer closest to X.  If X has a fractional value of
	% 0.5, it is rounded up.
:- pragma(c_code,
	float__round_to_int(X :: in, Round :: out),
"
	Round = (Integer)floor(X+0.5);
").

        % float__truncate_to_int(X, Trunc) is true if Trunc is
	% the integer closest to X such that |Trunc| =< |X|.
:- pragma(c_code,
	float__truncate_to_int(X :: in, Trunc :: out),
"
	Trunc = (Integer)X;
").

%---------------------------------------------------------------------------%

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
% The floating-point system constants are derived from <float.h> and
% implemented using the C interface.
%

:- pragma(c_header_code, "

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

:- pragma c_code(float__hash(F::in, H::out), will_not_call_mercury, "

	H = hash_float(F);

").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
