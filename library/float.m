%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
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

%
% Arithmetic functions
%

	% addition
:- func float + float = float.
:- mode in    + in    = uo  is det.
:- mode uo    + in    = in  is det.
:- mode in    + uo    = in  is det.

	% subtraction
:- func float - float = float.
:- mode in    - in    = uo  is det.
:- mode uo    - in    = in  is det.
:- mode in    - uo    = in  is det.

	% multiplication
:- func float * float = float.
:- mode in    * in    = uo  is det.
:- mode uo    * in    = in  is det.
:- mode in    * uo    = in  is det.

	% division
:- func float / float = float.
:- mode in    / in    = uo  is det.
:- mode uo    / in    = in  is det.
:- mode in    / uo    = in  is det.

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
	% (Note: the hash values computed by the Mercury implementation of
	% this predicate are different to those computed by the Prolog
	% implementation.)
:- func hash(float) = int.

%
% System constants
%

	% Maximum floating-point number
:- func float__max = float.

	% Minimum normalised floating-point number
:- func float__min = float.

	% Smallest number x such that 1.0 + x \= 1.0
:- func float__epsilon = float.

%---------------------------------------------------------------------------%

% Predicate versions of the functions declared above.
% These are intended for use in programs that need to work
% in both Prolog and Mercury (e.g. for debugging).
% These predicate versions will eventually become obsolete.

%
% Conversion predicates
%

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

%
% Miscellaneous predicates
%

	% absolute value
:- pred float__abs(float, float).
:- mode float__abs(in, out) is det.

	% maximum
:- pred float__max(float, float, float).
:- mode float__max(in, in, out) is det.

	% minimum
:- pred float__min(float, float, float).
:- mode float__min(in, in, out) is det.

	% float__pow(Base, Exponent, Answer) is true iff Answer is
	% Base raised to the power Exponent.  The exponent must be an 
	% integer greater or equal to 0.  Currently this function runs
	% at O(n), where n is the value of the exponent.
:- pred float__pow(float, int, float).
:- mode float__pow(in, in, out) is det.

	% Compute a non-negative integer hash value for a float.
	% (Note: the hash values computed by the Mercury implementation of
	% this predicate are different to those computed by the Prolog
	% implementation.)
:- pred float__hash(float, int).
:- mode float__hash(in, out) is det.

%
% System constant predicates
%

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
%---------------------------------------------------------------------------%

:- implementation.
:- interface.

	% Everything below here will not appear in the
	% Mercury Library Reference Manual.

%---------------------------------------------------------------------------%

/* The following predicates are obsolete.  Don't use them.
   They will eventually disappear in some future release.
*/

% :- pragma obsolete(builtin_float_plus/3).
:- pred builtin_float_plus(float, float, float).
:- mode builtin_float_plus(in, in, uo) is det.

% :- pragma obsolete(builtin_float_minus/3).
:- pred builtin_float_minus(float, float, float).
:- mode builtin_float_minus(in, in, uo) is det.

% :- pragma obsolete(builtin_float_times/3).
:- pred builtin_float_times(float, float, float).
:- mode builtin_float_times(in, in, uo) is det.

% :- pragma obsolete(builtin_float_divide/3).
:- pred builtin_float_divide(float, float, float).
:- mode builtin_float_divide(in, in, uo) is det.

% :- pragma obsolete(builtin_float_gt/2).
:- pred builtin_float_gt(float, float).
:- mode builtin_float_gt(in, in) is semidet.

% :- pragma obsolete(builtin_float_lt/2).
:- pred builtin_float_lt(float, float).
:- mode builtin_float_lt(in, in) is semidet.

% :- pragma obsolete(builtin_float_ge/2).
:- pred builtin_float_ge(float, float).
:- mode builtin_float_ge(in, in) is semidet.

% :- pragma obsolete(builtin_float_le/2).
:- pred builtin_float_le(float, float).
:- mode builtin_float_le(in, in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
:- import_module int, require.

%
% Header files of mathematical significance.
%

:- pragma c_header_code("

	#include <float.h>
	#include <math.h>

").

%---------------------------------------------------------------------------%

% The arithmetic and comparison operators are builtins,
% which the compiler expands inline.  We don't need to define them here.

%---------------------------------------------------------------------------%
%
% Conversion functions
%

float(Int) = Float :-
	int__to_float(Int, Float).

	% float__ceiling_to_int(X, Ceil) is true if Ceil is the
	% smallest integer not less than X.
:- pragma c_code(
	float__ceiling_to_int(X :: in, Ceil :: out),
	will_not_call_mercury,
"
	Ceil = (Integer) ceil(X);
").

float__ceiling_to_int(X) = Ceil :- float__ceiling_to_int(X, Ceil).

	% float__floor_to_int(X, Floor) is true if Floor is the
	% largest integer not greater than X.
:- pragma c_code(
	float__floor_to_int(X :: in, Floor :: out),
	will_not_call_mercury,
"
	Floor = (Integer) floor(X);
").

float__floor_to_int(X) = Floor :- float__floor_to_int(X, Floor).

	% float__round_to_int(X, Round) is true if Round is the
	% integer closest to X.  If X has a fractional value of
	% 0.5, it is rounded up.
:- pragma c_code(
	float__round_to_int(X :: in, Round :: out),
	will_not_call_mercury,
"
	Round = (Integer) floor(X + 0.5);
").

float__round_to_int(X) = Round :- float__round_to_int(X, Round).

	% float__truncate_to_int(X, Trunc) is true if Trunc is
	% the integer closest to X such that |Trunc| =< |X|.
:- pragma c_code(
	float__truncate_to_int(X :: in, Trunc :: out),
	will_not_call_mercury,
"
	Trunc = (Integer) X;
").

float__truncate_to_int(X) = Trunc :- float__truncate_to_int(X, Trunc).

%---------------------------------------------------------------------------%
%
% Miscellaneous functions
%

float__abs(Num, Abs) :-
	(
		Num =< 0.0
	->
		Abs = - Num
	;
		Abs = Num
	).

float__abs(Num) = Abs :- float__abs(Num, Abs).

float__max(X, Y, Max) :-
	(
		X >= Y
	->
		Max = X
	;
		Max = Y
	).

float__max(X, Y) = Max :- float__max(X, Y, Max).

float__min(X, Y, Min) :-
	(
		X =< Y
	->
		Min = X
	;
		Min = Y
	).

float__min(X, Y) = Min :- float__min(X, Y, Min).

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
		Ans is X * A2
	).

float__pow(X, Exp) = Pow :- float__pow(X, Exp, Pow).

:- pragma c_code(
	float__hash(F::in, H::out),
	will_not_call_mercury,
"
	H = hash_float(F);
").

float__hash(F) = H :- float__hash(F, H).

%---------------------------------------------------------------------------%
%
% System constants
%
% The floating-point system constants are derived from <float.h> and
% implemented using the C interface.

:- pragma c_header_code("

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
:- pragma c_code(float__max(Max::out), will_not_call_mercury,
	"Max = MERCURY_FLOAT_MAX;").

float__max = Max :- float__max(Max).

	% Minimum normalised floating-point number */
:- pragma c_code(float__min(Min::out), will_not_call_mercury,
	"Min = MERCURY_FLOAT_MIN;").

float__min = Min :- float__min(Min).

	% Smallest x such that x \= 1.0 + x
:- pragma c_code(float__epsilon(Eps::out), will_not_call_mercury,
	"Eps = MERCURY_FLOAT_EPSILON;").

float__epsilon = Epsilon :- float__epsilon(Epsilon).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
