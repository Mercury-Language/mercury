%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2015, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: imag_float.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (imag, float).
%
% See also: complex.m, imag.m, float_imag.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module complex_numbers.imag_float.
:- interface.

:- import_module complex_numbers.complex.
:- import_module complex_numbers.imag.

:- import_module float.

%-----------------------------------------------------------------------------%

    % Addition.
    %
:- func imag + float = complex.
:- mode in + in   = uo  is det.

    % Subtraction.
    %
:- func imag - float = complex.
:- mode in - in   = uo  is det.

    % Multiplication.
    %
:- func imag * float = imag.
:- mode in * in   = uo  is det.

    % Division.
    %
:- func imag / float = imag.
:- mode in / in   = uo  is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

im(XI) + YR = cmplx(0.0 + YR, 0.0 + XI).
im(XI) - YR = cmplx(0.0 - YR, 0.0 + XI).
im(XI) * YR = im(XI * YR).
im(XI) / YR = im(XI / YR).

%-----------------------------------------------------------------------------%
:- end_module complex_numbers.imag_float.
%-----------------------------------------------------------------------------%
