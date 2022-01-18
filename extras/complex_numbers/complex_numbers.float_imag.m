%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: float_imag.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (float, imag).
%
% See also: complex.m, imag.m, float.m, imag_float.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module complex_numbers.float_imag.
:- interface.

:- import_module complex_numbers.complex.
:- import_module complex_numbers.imag.

:- import_module float.

%-----------------------------------------------------------------------------%

    % Addition.
    %
:- func float + imag = complex.
:- mode in + in   = uo  is det.

    % Subtraction.
    %
:- func float - imag = complex.
:- mode in - in   = uo  is det.

    % Multiplication.
    %
:- func float * imag = imag.
:- mode in * in   = uo  is det.

    % Division.
    %
:- func float / imag = imag.
:- mode in / in   = uo  is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

XR + im(YI) = cmplx(0.0 + XR, 0.0 + YI).
XR - im(YI) = cmplx(0.0 + XR, 0.0 - YI).
XR * im(YI) = im(XR * YI).
XR / im(YI) = im(0.0 - XR / YI).

%-----------------------------------------------------------------------------%
:- end_module complex_numbers.float_imag.
%-----------------------------------------------------------------------------%
