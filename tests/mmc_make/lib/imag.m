%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: imag.m.
% Main author: fjh.
% Stability: medium.
%
% Imaginary numbers.
%
% There are several reasons for supporting a separate type for imaginary
% numbers rather than just treating them as a special case of complex
% numbers.  It is sometimes more convenient, and can be slightly more
% efficient.   But perhaps the most important reason is to get correct
% handling of infinity and not-a-number on platforms that support IEEE
% floating point.
%
% Note that the overloaded versions of the binary operators which
% provide mixed type arithmetic are defined in different modules.
%
% See also:
%	float.m, imag_float.m, float_imag.m,
%	complex.m, imag_complex.m, complex_imag.m.
%
%---------------------------------------------------------------------------%

:- module complex_numbers:imag.
:- interface.
:- import_module float.

:- type imag ---> im(float).

:- func i = imag.	% i = sqrt(-1)
:- func j = imag.	% another name for `i'

	% addition
:- func imag + imag = imag.
:- mode in   + in   = uo  is det.

	% subtraction
:- func imag - imag = imag.
:- mode in   - in   = uo  is det.

	% multiplication
:- func imag * imag = float.
:- mode in   * in   = uo  is det.

	% division
:- func imag / imag = float.
:- mode in   / in   = uo  is det.

	% unary plus
:- func + imag = imag.
:- mode + in   = uo  is det.

	% unary minus
:- func - imag = imag.
:- mode - in   = uo  is det.

%---------------------------------------------------------------------------%

:- implementation.

i = im(1.0).
j = i.

+im(X) = im(X + 0.0).
-im(X) = im(-X).
im(X) + im(Y) = im(X + Y).
im(X) - im(Y) = im(X - Y).
im(X) * im(Y) = 0.0 - X * Y.
im(X) / im(Y) = X / Y.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
