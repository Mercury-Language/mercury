:- module vec3.
% a 3D vector module.

:- interface.

:- import_module float, mat3.

:- type vec3	--->	vec(float, float, float).

:- func '+'(vec3, vec3) = vec3.

:- func '-'(vec3, vec3) = vec3.

:- func '-'(vec3) = vec3.

:- func dot(vec3, vec3) = float.

:- func cross(vec3, vec3) = vec3.

:- func '*'(vec3, mat3) = vec3.

:- func mag(vec3) = float.

:- func unit(vec3) = vec3.

:- func scale(float, vec3) = vec3.

:- func angle(vec3, vec3) = float.

:- implementation.

:- import_module math.

vec(Ax, Ay, Az) + vec(Bx, By, Bz) = vec(Ax+Bx, Ay+By, Az+Bz).

vec(Ax, Ay, Az) - vec(Bx, By, Bz) = vec(Ax-Bx, Ay-By, Az-Bz).

-vec(X, Y, Z) = vec(-X, -Y, -Z).

dot(vec(Ax, Ay, Az), vec(Bx, By, Bz)) = Ax*Bx + Ay*By + Az*Bz.

cross(vec(Ax, Ay, Az), vec(Bx, By, Bz)) = vec(Cx, Cy, Cz) :-
	Cx = Ay*Bz - Az*By,
	Cy = Az*Bx - Ax*Bz,
	Cz = Ax*By - Ay*Bx.

V * mat(U1, U2, U3) = vec(dot(V,U1), dot(V,U2), dot(V,U3)).

mag(vec(Ax, Ay, Az)) = Mag :-
	Mag = math__sqrt(Ax*Ax + Ay*Ay + Az*Az).

unit(vec(Ax, Ay, Az)) = vec(Ax/Mag, Ay/Mag, Az/Mag) :-
	Mag = mag(vec(Ax, Ay, Az)).

scale(S, vec(Ax, Ay, Az)) = vec(S*Ax, S*Ay, S*Az).

% calculate angle between 2 vectors
angle(V1, V2) = Theta :-
    CosTheta = dot(V1, V2) / mag(V1) / mag(V2),
    Theta = math__acos(CosTheta).
