%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2005-2006 The University of Melbourne.
% Copyright (C) 2015, 2018, 2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: complex.m.
% Main author: fjh.
% Stability: medium.
%
% Complex numbers.
%
% Note that the overloaded versions of the binary operators that
% provide mixed-type arithmetic are defined in other modules.
%
% See also:
%   complex_float.m, float_complex.m
%   imag.m, complex_imag.m, imag_complex.m
%
%---------------------------------------------------------------------------%

:- module complex_numbers.complex.
:- interface.

%---------------------------------------------------------------------------%

% The constructor cmplx/2 is made public, but generally it is most convenient
% to use the syntax `X + Y*i' for complex numbers, where `i' is declared in
% module `imag'.  Due to the wonders of logic programming, this works fine for
% both constructing and pattern matching; with intermodule optimization
% enabled, the compiler should generate equally good code for it.

:- type complex
    --->    cmplx(
                float,      % real part
                float       % imag part
            ).

%---------------------------------------------------------------------------%

    % Convert float to complex.
    %
:- func complex(float) = complex.

    % Extract real part.
    %
:- func real(complex) = float.

    % Extract imaginary part.
    %
:- func imag(complex) = float.

    % Square of absolute value.
    %
:- func abs2(complex) = float.

    % Absolute value (a.k.a. modulus).
    %
:- func abs(complex) = float.

    % Argument (a.k.a. phase, or amplitude, or angle).
    % This function returns the principle value:
    %
    %   for all Z, -pi < arg(Z) and arg(Z) =< pi.
    %
:- func arg(complex) = float.

    % Complex conjugate.
    %
:- func conj(complex) = complex.

    % Addition.
    %
:- func complex + complex = complex.
:- mode in  + in  = uo  is det.

    % Subtraction.
    %
:- func complex - complex = complex.
:- mode in  - in  = uo  is det.

    % Multiplication.
    %
:- func complex * complex = complex.
:- mode in  * in  = uo  is det.

    % Division.
    %
:- func complex / complex = complex.
:- mode in  / in  = uo  is det.

    % Unary plus.
    %
:- func + complex = complex.
:- mode + in    = uo  is det.

    % Unary minus.
    %
:- func - complex = complex.
:- mode - in    = uo  is det.

    % sqr(X) = X * X.
    %
:- func sqr(complex) = complex.

    % Square root.
    %
:- func sqrt(complex) = complex.

    % cis(Theta) = cos(Theta) + i * sin(Theta)
    %
:- func cis(float) = complex.

    % polar_to_complex(R, Theta).
    % Conversion from polar coordinates.
    %
:- func polar_to_complex(float, float) = complex.

    % polar_to_complex(Z, R, Theta).
    % Conversion to polar coordinates.
    %
:- pred complex_to_polar(complex::in, float::out, float::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module math.

%---------------------------------------------------------------------------%

complex(Real) = cmplx(Real, 0.0).

real(cmplx(Real, _Imag)) = Real.
imag(cmplx(_Real, Imag)) = Imag.

cmplx(Xr, Xi) + cmplx(Yr, Yi) = cmplx(Xr + Yr, Xi + Yi).
cmplx(Xr, Xi) - cmplx(Yr, Yi) = cmplx(Xr - Yr, Xi - Yi).
cmplx(Xr, Xi) * cmplx(Yr, Yi) =
    cmplx(Xr * Yr - Xi * Yi, Xr * Yi + Xi * Yr).
cmplx(Xr, Xi) / cmplx(Yr, Yi) =
        cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
    Div = (Yr * Yr + Yi * Yi).
% Here's the derivation of the formula for complex division:
% cmplx(Xr, Xi) / cmplx(Yr, Yi) =
%   (cmplx(Xr, Xi) / cmplx(Yr, Yi)) * 1.0 =
%   (cmplx(Xr, Xi) / cmplx(Yr, Yi)) * (cmplx(Yr, -Yi) / cmplx(Yr, -Yi)) =
%   (cmplx(Xr, Xi) * (cmplx(Yr, -Yi)) / (cmplx(Yr, Yi) * cmplx(Yr, -Yi)) =
%   (cmplx(Xr, Xi) * (cmplx(Yr, -Yi)) / (Yr * Yr + Yi * Yi) =
%   cmplx(Xr * Yr + Xi * Yi, Xi * Yr - Xr * Yi) / (Yr * Yr + Yi * Yi) =
%   cmplx((Xr * Yr + Xi * Yi) / Div, (Xi * Yr - Xr * Yi) / Div) :-
%       Div = (Yr * Yr + Yi * Yi).

+ cmplx(R, I) = cmplx(+ R, + I).
- cmplx(R, I) = cmplx(- R, - I).

abs2(cmplx(R, I)) = R*R + I*I.

abs(Z) = sqrt(abs2(Z)).

arg(cmplx(R, I)) = atan2(I, R).

conj(cmplx(R, I)) = cmplx(R, -I).

sqr(cmplx(Re0, Im0)) = cmplx(Re, Im) :-
    Re = Re0 * Re0 - Im0 * Im0,
    Im = 2.0 * Re0 * Im0.

sqrt(Z0) = Z :-
    complex_to_polar(Z0, Magnitude0, Theta0),
    Magnitude = sqrt(Magnitude0),
    Theta = Theta0 / 2.0,
    Z = polar_to_complex(Magnitude, Theta).

complex_to_polar(Z, abs(Z), arg(Z)).

polar_to_complex(Magnitude, Theta) = cmplx(Real, Imag) :-
    Real = Magnitude * cos(Theta),
    Imag = Magnitude * sin(Theta).

cis(Theta) = cmplx(cos(Theta), sin(Theta)).

%---------------------------------------------------------------------------%
:- end_module complex_numbers.complex.
%---------------------------------------------------------------------------%
