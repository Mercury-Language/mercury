%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: math.m
% Main author: bromage
% Stability: high (but as yet no Prolog implementation)
%
% Higher mathematical operations.  (The basics are in float.m.)
% The predicates in this module are not yet implemented in Prolog.
%
% Domain errors are currently handled by a program abort.  This is
% because Mercury currently does not have exceptions built in.
% Exception-handling would be nice, but it's kind of low on the
% priority scale.
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
% Power/logarithm operations

	% math__sqrt(X) = Sqrt is true if Sqrt is the positive square
	% root of X.
	%
	% Domain restriction: X >= 0
:- func math__sqrt(float) = float.
:- mode math__sqrt(in) = out is det.

	% math__pow(X, Y) = Res is true if Res is X raised to the
	% power of Y.
	%
	% Domain restriction: X >= 0 and (X = 0 implies Y > 0)
:- func math__pow(float, float) = float.
:- mode math__pow(in, in) = out is det.

	% math__exp(X) = Exp is true if Exp is X raised to the
	% power of e.
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% These operations are all implemented using the C interface.

:- pragma(c_header_code, "

	#include <math.h>

	/*
	** Mathematical constants.
	*/

	#define	MERCURY_FLOAT__E		2.7182818284590452354
	#define	MERCURY_FLOAT__PI		3.1415926535897932384
	#define	MERCURY_FLOAT__LN2		0.69314718055994530941

	void mercury_domain_error(const char *where);

"). % end pragma c_header_code

:- pragma(c_code, "

	#include <stdio.h>

	/*
	** Handle domain errors.
	*/
	void
	mercury_domain_error(const char *where)
	{
		fflush(stdout);
		fprintf(stderr,
			""Software error: Domain error in call to `%s'\n"",
			where);
		exit(1);
	}

"). % end pragma c_code

%
% Mathematical constants from math.m
%
	% Pythagoras' number
:- pragma c_code(math__pi = (Pi::out),will_not_call_mercury,"
	Pi = MERCURY_FLOAT__PI;
").

	% Base of natural logarithms
:- pragma c_code(math__e = (E::out),will_not_call_mercury,"
	E = MERCURY_FLOAT__E;
").

%
% math__ceiling(X) = Ceil is true if Ceil is the smallest integer
% not less than X.
%
:- pragma c_code(math__ceiling(Num::in) = (Ceil::out), will_not_call_mercury,"
	Ceil = ceil(Num);
").

%
% math__floor(X) = Floor is true if Floor is the largest integer
% not greater than X.
%
:- pragma c_code(math__floor(Num::in) = (Floor::out), will_not_call_mercury,"
	Floor = floor(Num);
").

%
% math__round(X) = Round is true if Round is the integer
% closest to X.  If X has a fractional component of 0.5,
% it is rounded up.
%
:- pragma c_code(math__round(Num::in) = (Rounded::out), will_not_call_mercury,"
	Rounded = floor(Num+0.5);
").

%
% math__truncate(X) = Trunc is true if Trunc is the integer
% closest to X such that |Trunc| =< |X|.
%
:- pragma c_code(math__truncate(X::in) = (Trunc::out), will_not_call_mercury,"
	if (X < 0.0) {
	    Trunc = ceil(X);
	} else {
	    Trunc = floor(X);
	}
").

%
% math__sqrt(X) = Sqrt is true if Sqrt is the positive square
% root of X.  
%
% Domain restrictions:
%		X >= 0
%
:- pragma c_code(math__sqrt(X::in) = (SquareRoot::out), will_not_call_mercury,"
	if (X < 0.0) {
	    mercury_domain_error(""math__sqrt"");
	}
	SquareRoot = sqrt(X);
").

%
% math__pow(X, Y) = Res is true if Res is X raised to the
% power of Y.
%
% Domain restrictions:
%		X >= 0
%		X = 0 implies Y > 0
%
:- pragma c_code(math__pow(X::in, Y::in) = (Res::out), will_not_call_mercury,"
	if (X < 0.0) {
	    mercury_domain_error(""math__pow"");
	}
	if (X == 0.0) {
	    if (Y <= 0.0) {
		mercury_domain_error(""math__pow"");
	    }
	    Res = 0.0;
	} else {
	    Res = pow(X, Y);
	}
").

%
% math__exp(X) = Exp is true if Exp is X raised to the
% power of e.
%
:- pragma c_code(math__exp(X::in) = (Exp::out), will_not_call_mercury,"
	Exp = exp(X);
").

%
% math__ln(X) = Log is true if Log is the natural logarithm
% of X.
%
% Domain restrictions:
%		X > 0
%
:- pragma c_code(math__ln(X::in) = (Log::out), will_not_call_mercury,"
	if (X <= 0.0) {
	    mercury_domain_error(""math__ln"");
	}
	Log = log(X);
").

%
% math__log10(X) = Log is true if Log is the logarithm to
% base 10 of X.
%
% Domain restrictions:
%		X > 0
%
:- pragma c_code(math__log10(X::in) = (Log10::out), will_not_call_mercury,"
	if (X <= 0.0)
	    mercury_domain_error(""math__log10"");
	Log10 = log10(X);
").

%
% math__log2(X) = Log is true if Log is the logarithm to
% base 2 of X.
%
% Domain restrictions:
%		X > 0
%
:- pragma c_code(math__log2(X::in) = (Log2::out), will_not_call_mercury,"
	if (X <= 0.0) {
	    mercury_domain_error(""math__log2"");
	}
	Log2 = log(X) / MERCURY_FLOAT__LN2;
").

%
% math__log(B, X) = Log is true if Log is the logarithm to
% base B of X.
%
% Domain restrictions:
%		X > 0
%		B > 0
%		B \= 1
%
:- pragma c_code(math__log(B::in, X::in) = (Log::out), will_not_call_mercury,"
	if (X <= 0.0 || B <= 0.0) {
	    mercury_domain_error(""math__log"");
	}
	if (B == 1.0) {
	    mercury_domain_error(""math__log"");
	}
	Log = log(X)/log(B);
").

%
% math__sin(X) = Sin is true if Sin is the sine of X.
%
:- pragma c_code(math__sin(X::in) = (Sin::out), will_not_call_mercury,"
	Sin = sin(X);
").

%
% math__cos(X) = Sin is true if Cos is the cosine of X.
%
:- pragma c_code(math__cos(X::in) = (Cos::out), will_not_call_mercury,"
	Cos = cos(X);
").

%
% math__tan(X) = Tan is true if Tan is the tangent of X.
%
:- pragma c_code(math__tan(X::in) = (Tan::out), will_not_call_mercury,"
	Tan = tan(X);
").

%
% math__asin(X) = ASin is true if ASin is the inverse
% sine of X, where ASin is in the range [-pi/2,pi/2].
%
% Domain restrictions:
%		X must be in the range [-1,1]
%
:- pragma c_code(math__asin(X::in) = (ASin::out), will_not_call_mercury,"
	if (X < -1.0 || X > 1.0) {
	    mercury_domain_error(""math__asin"");
	}
	ASin = asin(X);
").

%
% math__acos(X) = ACos is true if ACos is the inverse
% cosine of X, where ACos is in the range [0, pi].
%
% Domain restrictions:
%		X must be in the range [-1,1]
%
:- pragma c_code(math__acos(X::in) = (ACos::out), will_not_call_mercury,"
	if (X < -1.0 || X > 1.0) {
	    mercury_domain_error(""math__acos"");
	}
	ACos = asin(X);
").

%
% math__atan(X) = ATan is true if ATan is the inverse
% tangent of X, where ATan is in the range [-pi/2,pi/2].
%
:- pragma c_code(math__atan(X::in) = (ATan::out), will_not_call_mercury,"
	ATan = atan(X);
").

%
% math__atan2(Y, X) = ATan is true if ATan is the inverse
% tangent of Y/X, where ATan is in the range [-pi,pi].
%
:- pragma c_code(math__atan2(Y::in, X::in) = (ATan2::out), 
	will_not_call_mercury, "
	ATan2 = atan2(Y, X);
").

%
% math__sinh(X) = Sinh is true if Sinh is the hyperbolic
% sine of X.
%
:- pragma c_code(math__sinh(X::in) = (Sinh::out), will_not_call_mercury,"
	Sinh = sinh(X);
").

%
% math__cosh(X) = Cosh is true if Cosh is the hyperbolic
% cosine of X.
%
:- pragma c_code(math__cosh(X::in) = (Cosh::out), will_not_call_mercury,"
	Cosh = cosh(X);
").

%
% math__tanh(X) = Tanh is true if Tanh is the hyperbolic
% tangent of X.
%
:- pragma c_code(math__tanh(X::in) = (Tanh::out), will_not_call_mercury,"
	Tanh = tanh(X);
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

/*
** OBSOLETE OBSOLETE OBSOLETE 
**
** The predicate forms of the above functions are now deprecated.
** We provide them for compatibility reasons but they will be removed
** at a later release. Hence they are tagged `obsolete'.
*/

:- interface.

%---------------------------------------------------------------------------%
% Mathematical constants

	% Pythagoras' number
:- pred math__pi(float).
:- mode math__pi(out) is det.
:- pragma obsolete(math__pi/1).

	% Base of natural logarithms
:- pred math__e(float).
:- mode math__e(out) is det.
:- pragma obsolete(math__e/1).

%---------------------------------------------------------------------------%
% "Next integer" operations

	% math__ceiling(X, Ceil) is true if Ceil is the smallest integer
	% not less than X.
:- pred math__ceiling(float, float).
:- mode math__ceiling(in, out) is det.
:- pragma obsolete(math__ceiling/2).

	% math__floor(X, Floor) is true if Floor is the largest integer
	% not greater than X.
:- pred math__floor(float, float).
:- mode math__floor(in, out) is det.
:- pragma obsolete(math__floor/2).

	% math__round(X, Round) is true if Round is the integer
	% closest to X.  If X has a fractional value of 0.5, it
	% is rounded up.
:- pred math__round(float, float).
:- mode math__round(in, out) is det.
:- pragma obsolete(math__round/2).

	% math__truncate(X, Trunc) is true if Trunc is the integer
	% closest to X such that |Trunc| =< |X|.
:- pred math__truncate(float, float).
:- mode math__truncate(in, out) is det.
:- pragma obsolete(math__truncate/2).

%---------------------------------------------------------------------------%
% Power/logarithm operations

	% math__sqrt(X, Sqrt) is true if Sqrt is the positive square
	% root of X.
	%
	% Domain restriction: X >= 0
:- pred math__sqrt(float, float).
:- mode math__sqrt(in, out) is det.
:- pragma obsolete(math__sqrt/2).

	% math__pow(X, Y, Res) is true if Res is X raised to the
	% power of Y.
	%
	% Domain restriction: X >= 0 and (X = 0 implies Y > 0)
:- pred math__pow(float, float, float).
:- mode math__pow(in, in, out) is det.
:- pragma obsolete(math__pow/3).

	% math__exp(X, Exp) is true if Exp is X raised to the
	% power of e.
:- pred math__exp(float, float).
:- mode math__exp(in, out) is det.
:- pragma obsolete(math__exp/2).

	% math__ln(X, Log) is true if Log is the natural logarithm
	% of X.
	%
	% Domain restriction: X > 0
:- pred math__ln(float, float).
:- mode math__ln(in, out) is det.
:- pragma obsolete(math__ln/2).

	% math__log10(X, Log) is true if Log is the logarithm to
	% base 10 of X.
	%
	% Domain restriction: X > 0
:- pred math__log10(float, float).
:- mode math__log10(in, out) is det.
:- pragma obsolete(math__log10/2).

	% math__log2(X, Log) is true if Log is the logarithm to
	% base 2 of X.
	%
	% Domain restriction: X > 0
:- pred math__log2(float, float).
:- mode math__log2(in, out) is det.
:- pragma obsolete(math__log2/2).

	% math__log(B, X, Log) is true if Log is the logarithm to
	% base B of X.
	%
	% Domain restriction: X > 0 and B > 0 and B \= 1
:- pred math__log(float, float, float).
:- mode math__log(in, in, out) is det.
:- pragma obsolete(math__log/3).

%---------------------------------------------------------------------------%
% Trigonometric operations

	% math__sin(X, Sin) is true if Sin is the sine of X.
:- pred math__sin(float, float).
:- mode math__sin(in, out) is det.
:- pragma obsolete(math__sin/2).

	% math__cos(X, Cos) is true if Cos is the cosine of X.
:- pred math__cos(float, float).
:- mode math__cos(in, out) is det.
:- pragma obsolete(math__cos/2).

	% math__tan(X, Tan) is true if Tan is the tangent of X.
:- pred math__tan(float, float).
:- mode math__tan(in, out) is det.
:- pragma obsolete(math__tan/2).

	% math__asin(X, ASin) is true if ASin is the inverse
	% sine of X, where ASin is in the range [-pi/2,pi/2].
	%
	% Domain restriction: X must be in the range [-1,1]
:- pred math__asin(float, float).
:- mode math__asin(in, out) is det.
:- pragma obsolete(math__asin/2).

	% math__acos(X, ACos) is true if ACos is the inverse
	% cosine of X, where ACos is in the range [0, pi].
	%
	% Domain restriction: X must be in the range [-1,1]
:- pred math__acos(float, float).
:- mode math__acos(in, out) is det.
:- pragma obsolete(math__acos/2).

	% math__atan(X, ATan) is true if ATan is the inverse
	% tangent of X, where ATan is in the range [-pi/2,pi/2].
:- pred math__atan(float, float).
:- mode math__atan(in, out) is det.
:- pragma obsolete(math__atan/2).

	% math__atan2(Y, X, ATan) is true if ATan is the inverse
	% tangent of Y/X, where ATan is in the range [-pi,pi].
:- pred math__atan2(float, float, float).
:- mode math__atan2(in, in, out) is det.
:- pragma obsolete(math__atan2/3).

%---------------------------------------------------------------------------%
% Hyperbolic functions

	% math__sinh(X, Sinh) is true if Sinh is the hyperbolic
	% sine of X.
:- pred math__sinh(float, float).
:- mode math__sinh(in, out) is det.
:- pragma obsolete(math__sinh/2).

	% math__cosh(X, Cosh) is true if Cosh is the hyperbolic
	% cosine of X.
:- pred math__cosh(float, float).
:- mode math__cosh(in, out) is det.
:- pragma obsolete(math__cosh/2).

	% math__tanh(X, Tanh) is true if Tanh is the hyperbolic
	% tangent of X.
:- pred math__tanh(float, float).
:- mode math__tanh(in, out) is det.
:- pragma obsolete(math__tanh/2).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% These operations are all implemented using the C interface.


%
% Mathematical constants from math.m
%
	% Pythagoras' number
:- pragma(c_code, math__pi(Pi::out), "Pi = MERCURY_FLOAT__PI;").

	% Base of natural logarithms
:- pragma(c_code, math__e(E::out), "E = MERCURY_FLOAT__E;").

%
% math__ceiling(X, Ceil) is true if Ceil is the smallest integer
% not less than X.
%
:- pragma(c_code, math__ceiling(Num::in, Ceil::out), "Ceil = ceil(Num);").

%
% math__floor(X, Floor) is true if Floor is the largest integer
% not greater than X.
%
:- pragma(c_code, math__floor(Num::in, Floor::out), "Floor = floor(Num);").

%
% math__round(X, Round) is true if Round is the integer
% closest to X.  If X has a fractional component of 0.5,
% it is rounded up.
%
:- pragma(c_code, math__round(Num::in, Rounded::out), "
	Rounded = floor(Num+0.5);
").

%
% math__truncate(X, Trunc) is true if Trunc is the integer
% closest to X such that |Trunc| =< |X|.
%
:- pragma(c_code, math__truncate(X::in, Trunc::out), "
	if (X < 0.0) {
	    Trunc = ceil(X);
	} else {
	    Trunc = floor(X);
	}
").

%
% math__sqrt(X, Sqrt) is true if Sqrt is the positive square
% root of X.  
%
% Domain restrictions:
%		X >= 0
%
:- pragma(c_code, math__sqrt(X::in, SquareRoot::out), "
	if (X < 0.0) {
	    mercury_domain_error(""math__sqrt"");
	}
	SquareRoot = sqrt(X);
").

%
% math__pow(X, Y, Res) is true if Res is X raised to the
% power of Y.
%
% Domain restrictions:
%		X >= 0
%		X = 0 implies Y > 0
%
:- pragma(c_code, math__pow(X::in, Y::in, Res::out), "
	if (X < 0.0) {
	    mercury_domain_error(""math__pow"");
	}
	if (X == 0.0) {
	    if (Y <= 0.0) {
		mercury_domain_error(""math__pow"");
	    }
	    Res = 0.0;
	} else {
	    Res = pow(X, Y);
	}
").

%
% math__exp(X, Exp) is true if Exp is X raised to the
% power of e.
%
:- pragma(c_code, math__exp(X::in, Exp::out), "Exp = exp(X);").

%
% math__ln(X, Log) is true if Log is the natural logarithm
% of X.
%
% Domain restrictions:
%		X > 0
%
:- pragma(c_code, math__ln(X::in, Log::out), "
	if (X <= 0.0) {
	    mercury_domain_error(""math__ln"");
	}
	Log = log(X);
").

%
% math__log10(X, Log) is true if Log is the logarithm to
% base 10 of X.
%
% Domain restrictions:
%		X > 0
%
:- pragma(c_code, math__log10(X::in, Log10::out), "
	if (X <= 0.0)
	    mercury_domain_error(""math__log10"");
	Log10 = log10(X);
").

%
% math__log2(X, Log) is true if Log is the logarithm to
% base 2 of X.
%
% Domain restrictions:
%		X > 0
%
:- pragma(c_code, math__log2(X::in, Log2::out), "
	if (X <= 0.0) {
	    mercury_domain_error(""math__log2"");
	}
	Log2 = log(X) / MERCURY_FLOAT__LN2;
").

%
% math__log(B, X, Log) is true if Log is the logarithm to
% base B of X.
%
% Domain restrictions:
%		X > 0
%		B > 0
%		B \= 1
%
:- pragma(c_code, math__log(B::in, X::in, Log::out), "
	if (X <= 0.0 || B <= 0.0) {
	    mercury_domain_error(""math__log"");
	}
	if (B == 1.0) {
	    mercury_domain_error(""math__log"");
	}
	Log = log(X)/log(B);
").

%
% math__sin(X, Sin) is true if Sin is the sine of X.
%
:- pragma(c_code, math__sin(X::in, Sin::out), "Sin = sin(X);").

%
% math__cos(X, Sin) is true if Cos is the cosine of X.
%
:- pragma(c_code, math__cos(X::in, Cos::out), "Cos = cos(X);").

%
% math__tan(X, Tan) is true if Tan is the tangent of X.
%
:- pragma(c_code, math__tan(X::in, Tan::out), "Tan = tan(X);").

%
% math__asin(X, ASin) is true if ASin is the inverse
% sine of X, where ASin is in the range [-pi/2,pi/2].
%
% Domain restrictions:
%		X must be in the range [-1,1]
%
:- pragma(c_code, math__asin(X::in, ASin::out), "
	if (X < -1.0 || X > 1.0) {
	    mercury_domain_error(""math__asin"");
	}
	ASin = asin(X);
").

%
% math__acos(X, ACos) is true if ACos is the inverse
% cosine of X, where ACos is in the range [0, pi].
%
% Domain restrictions:
%		X must be in the range [-1,1]
%
:- pragma(c_code, math__acos(X::in, ACos::out), "
	if (X < -1.0 || X > 1.0) {
	    mercury_domain_error(""math__acos"");
	}
	ACos = asin(X);
").

%
% math__atan(X, ATan) is true if ATan is the inverse
% tangent of X, where ATan is in the range [-pi/2,pi/2].
%
:- pragma(c_code, math__atan(X::in, ATan::out), "ATan = atan(X);").

%
% math__atan2(Y, X, ATan) is true if ATan is the inverse
% tangent of Y/X, where ATan is in the range [-pi,pi].
%
:- pragma(c_code, math__atan2(Y::in, X::in, ATan2::out), "
	ATan2 = atan2(Y, X);
").

%
% math__sinh(X, Sinh) is true if Sinh is the hyperbolic
% sine of X.
%
:- pragma(c_code, math__sinh(X::in, Sinh::out), "Sinh = sinh(X);").

%
% math__cosh(X, Cosh) is true if Cosh is the hyperbolic
% cosine of X.
%
:- pragma(c_code, math__cosh(X::in, Cosh::out), "Cosh = cosh(X);").

%
% math__tanh(X, Tanh) is true if Tanh is the hyperbolic
% tangent of X.
%
:- pragma(c_code, math__tanh(X::in, Tanh::out), "Tanh = tanh(X);").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
