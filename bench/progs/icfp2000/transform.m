% Basic geometric transformations.
:- module transform.

:- interface.

:- import_module eval, vector.

	% translate(Point, Tx, Ty, Tz) = TranslatedPoint.
:- func translate(point, float, float, float) = point.

	% scale(Point, Sx, Sy, Sz) = ScaledPoint
:- func scale(point, float, float, float) = point.

	% isotropic_scale(Point, S) = scale(Point, S, S, S).
:- func isotropic_scale(point, float) = point.

	% rotate_x(Point, AngleInDegrees) = RotatedPoint.
:- func rotate_x(point, float) = point.

	% rotate_y(Point, AngleInDegrees) = RotatedPoint.
:- func rotate_y(point, float) = point.

	% rotate_z(Point, AngleInDegrees) = RotatedPoint.
:- func rotate_z(point, float) = point.

:- implementation.

:- import_module float, math.

translate(point(X, Y, Z), Tx, Ty, Tz) = point(X + Tx, Y + Ty, Z + Tz).

scale(point(X, Y, Z), Sx, Sy, Sz) = point(X * Sx, Y * Sy, Z * Sz).

isotropic_scale(Point, S) = scale(Point, S, S, S).

rotate_x(point(X, Y, Z), Degrees) =
		point(X, Y * Cos - Z * Sin, Y * Sin + Z * Cos) :-
	cos_and_sin(Degrees, Cos, Sin).

rotate_y(point(X, Y, Z), Degrees) =
		point(X * Cos + Z * Sin, Y, - X * Sin + Z * Cos) :-
	cos_and_sin(Degrees, Cos, Sin).

rotate_z(point(X, Y, Z), Degrees) =
		point(X * Cos - Y * Sin, X * Sin + Cos * Y, Z) :-
	cos_and_sin(Degrees, Cos, Sin).

:- pred cos_and_sin(float, float, float).
:- mode cos_and_sin(in, out, out) is det.

cos_and_sin(Degrees, Cos, Sin) :-
	% XXX It might be worth doing this conversion when the angle is
	% read in to avoid doing multiple conversions.
	Radians = Degrees / (2.0 * math__pi),
	Cos = math__cos(Radians),
	Sin = math__sin(Radians).

