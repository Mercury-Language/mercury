%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: imag_complex.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (imag, complex).
%
% See also:
%	complex.m, imag.m, complex_imag.m.
%
%---------------------------------------------------------------------------%

:- module complex_numbers:imag_complex.
:- interface.
:- import_module complex_numbers:imag, complex_numbers:complex.

	% addition
:- func imag + complex = complex.
:- mode in   + in   = uo  is det.

	% subtraction
:- func imag - complex = complex.
:- mode in   - in   = uo  is det.

	% multiplication
:- func imag * complex = complex.
:- mode in   * in   = uo  is det.

	% division
:- func imag / complex = complex.
:- mode in   / in   = uo  is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module float.

im(XI) + cmplx(YR, YI) = cmplx(0.0 + YR, XI + YI).
im(XI) - cmplx(YR, YI) = cmplx(0.0 - YR, XI - YI).
im(XI) * cmplx(YR, YI) = cmplx(-XI * YI, XI * YR).
im(XI) / cmplx(YR, YI) = cmplx((XI * YI) / Div, (XI * YR) / Div) :-
	Div = (YR * YR + YI * YI).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% Division of imag / complex formula obtained by simplifying this one:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%		cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%	Div = (Yr * Yr + Yi * Yi).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
