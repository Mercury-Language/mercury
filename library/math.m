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
:- import_module float.

%---------------------------------------------------------------------------%
% Mathematical constants

	% Pythagoras' number
:- pred math__pi(float).
:- mode math__pi(out) is det.

	% Base of natural logarithms
:- pred math__e(float).
:- mode math__e(out) is det.

%---------------------------------------------------------------------------%
% "Next integer" operations

	% math__ceiling(X, Ceil) is true if Ceil is the smallest integer
	% not less than X.
:- pred math__ceiling(float, float).
:- mode math__ceiling(in, out) is det.

	% math__floor(X, Ceil) is true if Ceil is the largest integer
	% not greater than X.
:- pred math__floor(float, float).
:- mode math__floor(in, out) is det.

	% math__round(X, Round) is true if Round is the integer
	% closest to X.  If X has a fractional value of 0.5, it
	% is rounded up.
:- pred math__round(float, float).
:- mode math__round(in, out) is det.

	% math__truncate(X, Trunc) is true if Trunc is the integer
	% closest to X such that |Trunc| =< |X|.
:- pred math__truncate(float, float).
:- mode math__truncate(in, out) is det.

%---------------------------------------------------------------------------%
% Power/logarithm operations

	% math__sqrt(X, Sqrt) is true if Sqrt is the positive square
	% root of X.
	%
	% Domain restriction: X >= 0
:- pred math__sqrt(float, float).
:- mode math__sqrt(in, out) is det.

	% math__pow(X, Y, Res) is true if Res is X raised to the
	% power of Y.
	%
	% Domain restriction: X >= 0 and (X = 0 implies Y > 0)
:- pred math__pow(float, float, float).
:- mode math__pow(in, in, out) is det.

	% math__exp(X, Exp) is true if Exp is X raised to the
	% power of e.
:- pred math__exp(float, float).
:- mode math__exp(in, out) is det.

	% math__ln(X, Log) is true if Log is the natural logarithm
	% of X.
	%
	% Domain restriction: X > 0
:- pred math__ln(float, float).
:- mode math__ln(in, out) is det.

	% math__log10(X, Log) is true if Log is the logarithm to
	% base 10 of X.
	%
	% Domain restriction: X > 0
:- pred math__log10(float, float).
:- mode math__log10(in, out) is det.

	% math__log2(X, Log) is true if Log is the logarithm to
	% base 2 of X.
	%
	% Domain restriction: X > 0
:- pred math__log2(float, float).
:- mode math__log2(in, out) is det.

	% math__log(B, X, Log) is true if Log is the logarithm to
	% base B of X.
	%
	% Domain restriction: X > 0 and B > 0 and B \= 1
:- pred math__log(float, float, float).
:- mode math__log(in, in, out) is det.

%---------------------------------------------------------------------------%
% Trigonometric operations

	% math__sin(X, Sin) is true if Sin is the sine of X.
:- pred math__sin(float, float).
:- mode math__sin(in, out) is det.

	% math__cos(X, Cos) is true if Cos is the cosine of X.
:- pred math__cos(float, float).
:- mode math__cos(in, out) is det.

	% math__tan(X, Tan) is true if Tan is the tangent of X.
:- pred math__tan(float, float).
:- mode math__tan(in, out) is det.

	% math__asin(X, ASin) is true if ASin is the inverse
	% sine of X, where ASin is in the range [-pi/2,pi/2].
	%
	% Domain restriction: X must be in the range [-1,1]
:- pred math__asin(float, float).
:- mode math__asin(in, out) is det.

	% math__acos(X, ACos) is true if ACos is the inverse
	% cosine of X, where ACos is in the range [0, pi].
	%
	% Domain restriction: X must be in the range [-1,1]
:- pred math__acos(float, float).
:- mode math__acos(in, out) is det.

	% math__atan(X, ATan) is true if ATan is the inverse
	% tangent of X, where ATan is in the range [-pi/2,pi/2].
:- pred math__atan(float, float).
:- mode math__atan(in, out) is det.

	% math__atan2(Y, X, ATan) is true if ATan is the inverse
	% tangent of Y/X, where ATan is in the range [-pi,pi].
:- pred math__atan2(float, float, float).
:- mode math__atan2(in, in, out) is det.

%---------------------------------------------------------------------------%
% Hyperbolic functions

	% math__sinh(X, Sinh) is true if Sinh is the hyperbolic
	% sine of X.
:- pred math__sinh(float, float).
:- mode math__sinh(in, out) is det.

	% math__cosh(X, Cosh) is true if Cosh is the hyperbolic
	% cosine of X.
:- pred math__cosh(float, float).
:- mode math__cosh(in, out) is det.

	% math__tanh(X, Tanh) is true if Tanh is the hyperbolic
	% tangent of X.
:- pred math__tanh(float, float).
:- mode math__tanh(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Currently, these operations are defined in math_rt.mod (see
% the runtime directory).  When a better C interface is written,
% this will change.

:- external(math__pi/1).
:- external(math__e/1).
:- external(math__ceiling/2).
:- external(math__floor/2).
:- external(math__round/2).
:- external(math__truncate/2).
:- external(math__sqrt/2).
:- external(math__pow/3).
:- external(math__exp/2).
:- external(math__ln/2).
:- external(math__log10/2).
:- external(math__log2/2).
:- external(math__log/3).
:- external(math__sin/2).
:- external(math__cos/2).
:- external(math__tan/2).
:- external(math__asin/2).
:- external(math__acos/2).
:- external(math__atan/2).
:- external(math__atan2/3).
:- external(math__sinh/2).
:- external(math__cosh/2).
:- external(math__tanh/2).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
