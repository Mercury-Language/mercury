%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: complex_float.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (complex, float).
%
% See also:
%	complex, float, complex_float.
%
%---------------------------------------------------------------------------%

:- module complex_numbers:complex_float.
:- interface.
:- import_module complex_numbers:complex, float.

	% addition
:- func complex + float = complex.
:- mode in   + in   = uo  is det.
:- mode uo   + in   = in  is det.
:- mode in   + uo   = in  is det.

	% subtraction
:- func complex - float = complex.
:- mode in   - in   = uo  is det.
:- mode uo   - in   = in  is det.
:- mode in   - uo   = in  is det.

	% multiplication
:- func complex * float = complex.
:- mode in   * in   = uo  is det.
:- mode uo   * in   = in  is det.
:- mode in   * uo   = in  is det.

	% division
:- func complex / float = complex.
:- mode in   / in   = uo  is det.
:- mode uo   / in   = in  is det.
:- mode in   / uo   = in  is det.

	% exponentiation
:- func pow(complex, float) = complex.
:- mode pow(in, in) = out is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module complex_numbers:complex_float.

cmplx(XR, XI) + YR = cmplx(XR + YR, XI).
cmplx(XR, XI) - YR = cmplx(XR - YR, XI).
cmplx(XR, XI) * YR = cmplx(XR * YR, XI * YR).
cmplx(XR, XI) / YR = cmplx(XR / YR, XI / YR).

pow(cmplx(Re0, Im0), P) = cmplx(Re, Im) :-
	cartesian_to_polar(Re0, Im0, L0, Th0),
	L = pow(L0, P),
	Th = Th0 * P,
	polar_to_cartesian(L, Th, Re, Im).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
