%---------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: imag_float.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (imag, float).
%
% See also:
%	complex.m, imag.m, float_imag.m.
%
%---------------------------------------------------------------------------%

:- module imag_float.
:- interface.
:- import_module imag, float, complex.

	% addition
:- func imag + float = complex.
:- mode in   + in   = uo  is det.
:- mode uo   + uo   = in  is det.

	% subtraction
:- func imag - float = complex.
:- mode in   - in   = uo  is det.
:- mode uo   - uo   = in  is det.

	% multiplication
:- func imag * float = imag.
:- mode in   * in   = uo  is det.
:- mode in   * uo   = in  is det.
:- mode uo   * in   = in  is det.

	% division
:- func imag / float = imag.
:- mode in   / in   = uo  is det.
:- mode in   / uo   = in  is det.
:- mode uo   / in   = in  is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module float.

im(XI) + YR = cmplx(0.0 + YR, 0.0 + XI).
im(XI) - YR = cmplx(0.0 - YR, 0.0 + XI).
im(XI) * YR = im(XI * YR).
im(XI) / YR = im(XI / YR).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
