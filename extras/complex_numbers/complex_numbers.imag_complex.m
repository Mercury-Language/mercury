%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2015, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: imag_complex.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (imag, complex).
%
% See also: complex.m, imag.m, complex_imag.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module complex_numbers.imag_complex.
:- interface.

:- import_module complex_numbers.complex.
:- import_module complex_numbers.imag.

%-----------------------------------------------------------------------------%

    % Addition.
    %
:- func imag + complex = complex.
:- mode in + in   = uo  is det.

    % Subtraction.
    %
:- func imag - complex = complex.
:- mode in - in   = uo  is det.

    % Multiplication.
    %
:- func imag * complex = complex.
:- mode in * in   = uo  is det.

    % Division.
    %
:- func imag / complex = complex.
:- mode in / in   = uo  is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

%-----------------------------------------------------------------------------%

im(XI) + cmplx(YR, YI) = cmplx(0.0 + YR, XI + YI).
im(XI) - cmplx(YR, YI) = cmplx(0.0 - YR, XI - YI).
im(XI) * cmplx(YR, YI) = cmplx(-XI * YI, XI * YR).
im(XI) / cmplx(YR, YI) = cmplx((XI * YI) / Div, (XI * YR) / Div) :-
    Div = (YR * YR + YI * YI).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Division of imag / complex formula obtained by simplifying this one:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%       cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%   Div = (Yr * Yr + Yi * Yi).

%-----------------------------------------------------------------------------%
:- end_module complex_numbers.imag_complex.
%-----------------------------------------------------------------------------%
