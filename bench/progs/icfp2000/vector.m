:- module vector.
% a 3D vector module.

:- interface.

:- import_module float, std_util.

:- type real == float.
:- type vector ---> point(real, real, real).
:- type position == point.
:- type point == vector.

:- func '+'(vector, vector) = vector.

:- func '-'(vector, vector) = vector.

:- func '-'(vector) = vector.

:- func dot(vector, vector) = real.

	% Dot product, but zeroed if not positive.
:- func positive_dot(vector, vector) = real.

:- func cross(vector, vector) = vector.

:- func mag(vector) = real.

	% The square of the magnitude.
:- func mag2(vector) = real.

	% The square of the distance between two points.
:- func distance_squared(point, point) = real.

:- func unit(vector) = vector.

:- func scale(real, vector) = vector.

:- func angle(vector, vector) = real.

	% ProjectedV1 = project(V1, V2)
	% Find the projection of V1 onto V2
:- func project(vector, vector) = vector.

	% Project vector onto XY plane.
:- func projectXY(vector) = vector.

:- func projectXZ(vector) = vector.

:- func projectZY(vector) = vector.

	% Return a pair containing the X and Y coords of the vector.
:- func xy(vector) = pair(real).

:- func xz(vector) = pair(real).

:- func zy(vector) = pair(real).

	% Some useful constants.
:- func zero = vector.
:- func i = vector.
:- func j = vector.
:- func k = vector.

:- func real_epsilon = real.
:- func real_max = real.

:- implementation.

:- import_module math.

point(Ax, Ay, Az) + point(Bx, By, Bz) = point(Ax+Bx, Ay+By, Az+Bz).

point(Ax, Ay, Az) - point(Bx, By, Bz) = point(Ax-Bx, Ay-By, Az-Bz).

-point(X, Y, Z) = point(-X, -Y, -Z).

dot(point(Ax, Ay, Az), point(Bx, By, Bz)) = Ax*Bx + Ay*By + Az*Bz.

positive_dot(A, B) = PosDot :-
	dot(A, B) = Dot,
	( Dot < 0.0 ->
		PosDot = 0.0
	;
		PosDot = Dot
	).

cross(point(Ax, Ay, Az), point(Bx, By, Bz)) = point(Cx, Cy, Cz) :-
	Cx = Ay*Bz - Az*By,
	Cy = Az*Bx - Ax*Bz,
	Cz = Ax*By - Ay*Bx.

mag(V) = math__sqrt(mag2(V)).

:- pragma inline(mag2/1).
mag2(point(Ax, Ay, Az)) = Ax*Ax + Ay*Ay + Az*Az.

distance_squared(point(X1, Y1, Z1), point(X2, Y2, Z2)) = DistanceSquared :-
	DX = X1 - X2,
	DY = Y1 - Y2,
	DZ = Z1 - Z2,
	DistanceSquared = DX * DX + DY * DY + DZ * DZ.

:- pragma inline(unit/1).
unit(point(Ax, Ay, Az)) = point(Ax/Mag, Ay/Mag, Az/Mag) :-
	Mag = mag(point(Ax, Ay, Az)).

scale(S, point(Ax, Ay, Az)) = point(S*Ax, S*Ay, S*Az).

% calculate angle between 2 vectors
angle(V1, V2) = math__acos(dot(V1, V2) / mag(V1) / mag(V2)).

project(V1, V2) = Projection :-
	UnitV2 = unit(V2),
	Projection = scale(dot(V1, UnitV2), UnitV2).


projectXY(point(X, Y, _)) = point(X, Y, 0.0).

projectXZ(point(X, _, Z)) = point(X, 0.0, Z).

projectZY(point(_, Y, Z)) = point(0.0, Y, Z).

xy(point(X, Y, _)) = X - Y.

xz(point(X, _, Z)) = X - Z.

zy(point(_, Y, Z)) = Z - Y.

zero = point(0.0, 0.0, 0.0).
i = point(1.0, 0.0, 0.0).
j = point(0.0, 1.0, 0.0).
k = point(0.0, 0.0, 1.0).

real_epsilon = float__epsilon.
real_max = float__max.
