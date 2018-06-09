%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%

% File: complex_float.m.
% Main author: fjh.
% Stability: medium.

% This module provides binary operators on (complex, float).
%
% See also: complex, float, complex_float.

%-----------------------------------------------------------------------------%

:- module complex_numbers.complex_float.
:- interface.

:- import_module complex_numbers.complex.

:- import_module float.

%-----------------------------------------------------------------------------%

    % Addition.
    %
:- func complex + float = complex.
:- mode in   + in   = uo  is det.

    % Subtraction.
    %
:- func complex - float = complex.
:- mode in   - in   = uo  is det.

    % Multiplication.
    % 
:- func complex * float = complex.
:- mode in   * in   = uo  is det.

    % Division.
    %
:- func complex / float = complex.
:- mode in   / in   = uo  is det.

    % Exponentiation.
    %
:- func pow(complex, float) = complex.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module math.

%-----------------------------------------------------------------------------%

cmplx(XR, XI) + YR = cmplx(XR + YR, + XI).
cmplx(XR, XI) - YR = cmplx(XR - YR, + XI).
cmplx(XR, XI) * YR = cmplx(XR * YR, XI * YR).
cmplx(XR, XI) / YR = cmplx(XR / YR, XI / YR).

pow(Z0, P) = Z :-
    complex_to_polar(Z0, L0, Th0),
    L = math.pow(L0, P),
    Th = Th0 * P,
    Z = polar_to_complex(L, Th).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
