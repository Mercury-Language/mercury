%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2001 The University of Melbourne.
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

:- module complex_numbers:imag_float.
:- interface.
:- import_module complex_numbers:imag, float, complex_numbers:complex.

	% addition
:- func imag + float = complex.
:- mode in   + in   = uo  is det.

	% subtraction
:- func imag - float = complex.
:- mode in   - in   = uo  is det.

	% multiplication
:- func imag * float = imag.
:- mode in   * in   = uo  is det.

	% division
:- func imag / float = imag.
:- mode in   / in   = uo  is det.

%---------------------------------------------------------------------------%

:- implementation.

im(XI) + YR = cmplx(0.0 + YR, 0.0 + XI).
im(XI) - YR = cmplx(0.0 - YR, 0.0 + XI).
im(XI) * YR = im(XI * YR).
im(XI) / YR = im(XI / YR).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
