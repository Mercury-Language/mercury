:- module op.

:- interface.

:- import_module bool.

:- func op_acos(float) = float.
:- func op_addi(int, int) = int.
:- func op_addf(float, float) = float.
:- func op_asin(float) = float.
:- func op_clampf(float) = float.
:- func op_cos(float) = float.
:- func op_divi(int, int) = int.
:- func op_divf(float, float) = float.
:- func op_eqi(int, int) = bool.
:- func op_eqf(float, float) = bool.
:- func op_floor(float) = int.
:- func op_frac(float) = float.
:- func op_lessi(int, int) = bool.
:- func op_lessf(float, float) = bool.
:- func op_modi(int, int) = int.
:- func op_muli(int, int) = int.
:- func op_mulf(float, float) = float.
:- func op_negi(int) = int.
:- func op_negf(float) = float.
:- func op_real(int) = float.
:- func op_sin(float) = float.
:- func op_sqrt(float) = float.
:- func op_subi(int, int) = int.
:- func op_subf(float, float) = float.

:- func radians(float) = float.
:- func degrees(float) = float.

:- implementation.

:- import_module int, float, math.

op_acos(N) = degrees(math__acos(N)).
op_addi(A, B) = A + B.
op_addf(A, B) = A + B.
op_asin(N) = degrees(math__asin(N)).
op_clampf(N) = ( N < 0.0 -> 0.0 ; N > 1.0 -> 1.0 ; N ).
op_cos(N) = math__cos(radians(N)).
op_divi(N1, N2) = N1 // N2.
op_divf(N1, N2) = N1 / N2.
op_eqi(N1, N2) = ( N1 = N2 -> yes ; no ).
op_eqf(N1, N2) = ( N1 = N2 -> yes ; no ).
op_floor(N) = floor_to_int(N).
op_frac(N) = N - truncate(N).
op_lessi(N1, N2) = ( N1 < N2 -> yes ; no ).
op_lessf(N1, N2) = ( N1 < N2 -> yes ; no ).
op_modi(N1, N2) = N1 `rem` N2.
op_muli(N1, N2) = N1 * N2.
op_mulf(N1, N2) = N1 * N2.
op_negi(N) = -N.
op_negf(N) = -N.
op_real(N) = float(N).
op_sin(N) = math__sin(radians(N)).
op_sqrt(N) = math__sqrt(N).		% XXX should we check that N >= 0.0
op_subi(N1, N2) = N1 - N2.
op_subf(N1, N2) = N1 - N2.

radians(Deg) = Deg*pi/180.0.
degrees(Rad) = Rad*180.0/pi.
