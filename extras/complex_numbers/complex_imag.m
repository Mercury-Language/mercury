%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: complex_imag.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (complex, imag).
%
% See also:
%	complex.m, imag.m, imag_complex.m.
%
%---------------------------------------------------------------------------%

:- module complex_numbers:complex_imag.
:- interface.
:- import_module complex_numbers:complex, complex_numbers:imag.

	% addition
:- func complex + imag = complex.
:- mode in   + in   = uo  is det.

	% subtraction
:- func complex - imag = complex.
:- mode in   - in   = uo  is det.

	% multiplication
:- func complex * imag = complex.
:- mode in   * in   = uo  is det.

	% division
:- func complex / imag = complex.
:- mode in   / in   = uo  is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module float.

cmplx(XR, XI) + im(YI) = cmplx(0.0 + XR, XI + YI).
cmplx(XR, XI) - im(YI) = cmplx(0.0 + XR, XI - YI).
cmplx(XR, XI) * im(YI) = cmplx(0.0 - XI * YI, 0.0 + XR * YI).
cmplx(XR, XI) / im(YI) = cmplx(0.0 + XI / YI, 0.0 - XR / YI).

% Division of complex / imag formula obtained by simplifying this one:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%		cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%	Div = (Yr * Yr + Yi * Yi).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
