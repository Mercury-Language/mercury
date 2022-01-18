%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: complex_imag.m.
% Main author: fjh.
% Stability: medium.
%
% This module provides binary operators on (complex, imag).
%
% See also: complex.m, imag.m, imag_complex.m.
%
%-----------------------------------------------------------------------------%

:- module complex_numbers.complex_imag.
:- interface.

:- import_module complex_numbers.complex.
:- import_module complex_numbers.imag.

%-----------------------------------------------------------------------------%

    % Addition.
    %
:- func complex + imag = complex.
:- mode in + in   = uo  is det.

    % Subtraction.
    %
:- func complex - imag = complex.
:- mode in - in   = uo  is det.

    % Multiplication.
    %
:- func complex * imag = complex.
:- mode in * in   = uo  is det.

    % Division.
    %
:- func complex / imag = complex.
:- mode in / in   = uo  is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

%-----------------------------------------------------------------------------%

cmplx(XR, XI) + im(YI) = cmplx(0.0 + XR, XI + YI).
cmplx(XR, XI) - im(YI) = cmplx(0.0 + XR, XI - YI).
cmplx(XR, XI) * im(YI) = cmplx(0.0 - XI * YI, 0.0 + XR * YI).
cmplx(XR, XI) / im(YI) = cmplx(0.0 + XI / YI, 0.0 - XR / YI).

% Division of complex / imag formula obtained by simplifying this one:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%       cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%   Div = (Yr * Yr + Yi * Yi).

%-----------------------------------------------------------------------------%
:- end_module complex_numbers.complex_imag.
%-----------------------------------------------------------------------------%
