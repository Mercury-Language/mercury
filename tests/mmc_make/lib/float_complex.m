%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: float_complex.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (float, complex).
%
% See also:
%	complex.m, float.m, complex_float.m.
%
%---------------------------------------------------------------------------%

:- module complex_numbers:float_complex.
:- interface.
:- import_module float, complex_numbers:complex.

	% addition
:- func float + complex = complex.
:- mode in   + in   = uo  is det.

	% subtraction
:- func float - complex = complex.
:- mode in   - in   = uo  is det.

	% multiplication
:- func float * complex = complex.
:- mode in   * in   = uo  is det.

	% division
:- func float / complex = complex.
:- mode in   / in   = uo  is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module complex_numbers:complex_float.

XR + cmplx(YR, YI) = cmplx(XR + YR, + YI).
XR - cmplx(YR, YI) = cmplx(XR - YR, - YI).
XR * cmplx(YR, YI) = cmplx(XR * YR, XR * YI).
XR / cmplx(YR, YI) = cmplx(XR * YR / Div, - XR * YI / Div) :-
	Div = YR * YR + YI * YI.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% Division of float / complex formula obtained by simplifying this one:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%		cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%	Div = (Yr * Yr + Yi * Yi).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
