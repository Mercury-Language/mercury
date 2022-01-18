%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: float_complex.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (float, complex).
%
% See also: complex.m, float.m, complex_float.m.
%
%-----------------------------------------------------------------------------%

:- module complex_numbers.float_complex.
:- interface.

:- import_module complex_numbers.complex.
:- import_module float.

%-----------------------------------------------------------------------------%

    % Addition.
    %
:- func float + complex = complex.
:- mode in + in   = uo  is det.

    % Subtraction.
    %
:- func float - complex = complex.
:- mode in - in   = uo  is det.

    % Multiplication.
    %
:- func float * complex = complex.
:- mode in * in   = uo  is det.

    % Division.
    %
:- func float / complex = complex.
:- mode in / in   = uo  is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module complex_numbers.complex_float.

%-----------------------------------------------------------------------------%

XR + cmplx(YR, YI) = cmplx(XR + YR, + YI).
XR - cmplx(YR, YI) = cmplx(XR - YR, - YI).
XR * cmplx(YR, YI) = cmplx(XR * YR, XR * YI).
XR / cmplx(YR, YI) = cmplx(XR * YR / Div, - XR * YI / Div) :-
    Div = YR * YR + YI * YI.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Division of float / complex formula obtained by simplifying this one:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%       cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%   Div = (Yr * Yr + Yi * Yi).

%-----------------------------------------------------------------------------%
:- end_module complex_numbers.float_complex.
%-----------------------------------------------------------------------------%
