% ---------------------------------------------------------------------------- %
% trans.m
% Ralph Becket <rbeck@microsoft.com>
% Sun Aug 27 11:22:32  2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% This module computes the composition of the various transformations
% and their inverses.  Also handles intersection between lines and
% transformed geometric primitives.
%
% Specialised composition operators are supplied for each transformation.
%
% There are six transformation matrices and their inverses:
%
% Mt(dx,dy,dz) = (   1    0    0   dx)  Mt-1(dx,dy,dz) = (   1    0    0  -dx)
%                (   0    1    0   dy)                   (   0    1    0  -dy)
%                (   0    0    1   dz)                   (   0    0    1  -dz)
%                (   0    0    0    1)                   (   0    0    0    1)
%
% Ms(sx,sy,sz) = (  sx    0    0    0)  Ms-1(sx,sy,sz) = (1/sx    0    0    0)
%                (   0   sy    0    0)                   (   0 1/sy    0    0)
%                (   0    0   sz    0)                   (   0    0 1/sz    0)
%                (   0    0    0    1)                   (   0    0    0    1)
%
% Mu(s)        = (   s    0    0    0)  Mu-1(s)        = ( 1/s    0    0    0)
%                (   0    s    0    0)                   (   0  1/s    0    0)
%                (   0    0    s    0)                   (   0    0  1/s    0)
%                (   0    0    0    1)                   (   0    0    0    1)
%
% Mrx(a)       = (   1    0    0    0)  Mrx-1(a)       = (   1    0    0    0)
%                (   0   ca  -sa    0)                   (   0   ca   sa    0)
%                (   0   sa   ca    0)                   (   0  -sa   ca    0)
%                (   0    0    0    1)                   (   0    0    0    1)
%
% Mry(a)       = (  ca    0   sa    0)  Mry-1(a)       = (  ca    0  -sa    0)
%                (   0    1    0    0)                   (   0    1    0    0)
%                ( -sa    0   ca    0)                   (  sa    0   ca    0)
%                (   0    0    0    1)                   (   0    0    0    1)
%
% Mrz(a)       = (  ca  -sa    0    0)  Mrz-1(a)       = (  ca   sa    0    0)
%                (  sa   ca    0    0)                   ( -sa   ca    0    0)
%                (   0    0    1    0)                   (   0    0    1    0)
%                (   0    0    0    1)                   (   0    0    0    1)
%
% Where ca = cos(a) and sa = sin(a).
%
% NOTE: any zero scaling factors will result in an exception being raised.
%
% ---------------------------------------------------------------------------- %

:- module trans.

:- interface.

:- import_module float, io, list.
:- import_module vector, eval.



:- type surface_coordinates
        ---> surface_coordinates(
                face                    :: int,
                surface_u               :: real,
                surface_v               :: real
        ).



    % The type of transformation matrices (contains both the
    % forward transformation M and its inverse W).
    %
:- type trans.



    % Apply a transformation matrix to points, vectors and normals.
    %
    % NOTE: when transforming a normal, one has to use
    % the transformation matrices the other way around -
    % that is given an object -> world space transformation M
    % and its inverse W, world space normals have to be
    % transformed into object space using Mt, not W (and vice
    % versa).  (Mt is the transpose of M).  This does work,
    % surprisingly, and handles the case where M includes
    % arbitrary scaling which does not preserve angles.
    %
:- func point_to_object_space(trans, point) = point.
:- func vector_to_object_space(trans, point) = point.
:- func normal_to_object_space(trans, point) = point.

:- func point_to_world_space(trans, point) = point.
:- func vector_to_world_space(trans, point) = point.
:- func normal_to_world_space(trans, point) = point.



    % The identity transformation (i.e. the unit matrix - start here!)
    %
:- func identity = trans.

    % Composition of the various transformations and their matrices.
    %
    %   compose_transformation(Arg, ..., Transformation0) = Transformation.

    % Args = dx, dy, dz
    %
:- func compose_translate(float, float, float, trans) = trans.

    % Args = sx, sy, sz
    %
:- func compose_scale(float, float, float, trans) = trans.

    % Arg  = s
    %
:- func compose_uscale(float, trans) = trans.

    % Arg  = angle (degrees)
    %
:- func compose_rotatex(float, trans) = trans.

    % Arg  = angle (degrees)
    %
:- func compose_rotatey(float, trans) = trans.

    % Arg  = angle (degrees)
    %
:- func compose_rotatez(float, trans) = trans.


:- type intersection
	---> intersection(
        object_id :: object_id,
		intersection_point :: point,
		surface_normal :: vector,
		surface_coordinates :: surface_coordinates,
		surface :: surface
	).

:- type intersections == list(intersection).

    % Given a line (a point P and a direction vector D) decide whether it
    % intersects with a particular geometric primitive with id Id under a given
    % transformation M (the inverse transformation W must also be supplied).
    % If the line does intersect, calculate the point of intersection POI,
    % the texture coordinates TC, and the surface normal for the primitive at
    % that point.  N is the unit surface normal at the point of intersection.
    %
    %   intersects_shape(Id, M, W, P, D, POI, TC, N).
    %   intersects_shape(MandW, P, D, POI, TC, N).
    %
:- pred intersects_plane(object_id, trans, point, vector, surface,
            intersections).
:- mode intersects_plane(in, in, in, in, in, out) is det.

:- pred intersects_sphere(object_id, trans, point, vector, surface,
            intersections).
:- mode intersects_sphere(in, in, in, in, in, out) is det.

:- pred intersects_cube(object_id, trans, point, vector, surface,
            intersections).
:- mode intersects_cube(in, in, in, in, in, out) is det.

:- pred intersects_cylinder(object_id, trans, point, vector, surface,
            intersections).
:- mode intersects_cylinder(in, in, in, in, in, out) is det.

:- pred intersects_cone(object_id, trans, point, vector, surface,
            intersections).
:- mode intersects_cone(in, in, in, in, in, out) is det.


    % Decide whether we're inside a given object.
    %
:- pred inside_sphere(point, trans).
:- mode inside_sphere(in, in) is semidet.

:- pred inside_plane(point, trans).
:- mode inside_plane(in, in) is semidet.

:- pred inside_cube(point, trans).
:- mode inside_cube(in, in) is semidet.

:- pred inside_cylinder(point, trans).
:- mode inside_cylinder(in, in) is semidet.

:- pred inside_cone(point, trans).
:- mode inside_cone(in, in) is semidet.



    % Print out a transformation.
    %
:- pred show_trans(trans, io__state, io__state).
:- mode show_trans(in, di, uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module exception, string, math, std_util.

    % NOTE 1: points and vectors are represented using the point/3 type
    % defined in eval.m.  For the purposes, a point (x, y, z) is
    % treated as a column vector (x, y, z, 1) while a vector (x, y, z)
    % is treated as a column vector (x, y, z, 0), the only difference
    % being that vectors are invariant under translation whereas points
    % are not.
    %
    % NOTE 2: when computing a composition M, we also compute its
    % inverse, W.  The idea is to transform world space into object
    % space via W, find the point of intersection, and transform it
    % back into world space via M.  Hence, if M = M3.M2.M1.I then
    % W = I.W1.W2.W3.
    %
    % NOTE 3: it turns out that the last row of a matrix is always
    % (0 0 0 1) and so we don't need to represent it.
    %
:- type trans
    --->    trans(matrix, matrix).      % ObjToWorldSpace - WorldToObjSpace.

:- type matrix
    --->    matrix(
                float, float, float, float,
                float, float, float, float,
                float, float, float, float
              % 0,    0,    0,    1
            ).



:- func unit_matrix = matrix.

:- func transform_point(matrix, point) = point.
:- func transform_vector(matrix, vector) = vector.
:- func transform_normal(matrix, vector) = vector.

:- func degrees_to_radians(float) = float.

% ---------------------------------------------------------------------------- %

transform_point(M, P0) = P :-

    M  = matrix(M11, M12, M13, M14,
                M21, M22, M23, M24,
                M31, M32, M33, M34),

    P0 = point(X, Y, Z),

    P  = point(M11*X + M12*Y + M13*Z + M14,
               M21*X + M22*Y + M23*Z + M24,
               M31*X + M32*Y + M33*Z + M34).

% ---------------------------------------------------------------------------- %

transform_vector(M, V0) = V :-

    M  = matrix(M11, M12, M13, _M14,
                M21, M22, M23, _M24,
                M31, M32, M33, _M34),

    V0 = point(X, Y, Z),

    V  = point(M11*X + M12*Y + M13*Z,
               M21*X + M22*Y + M23*Z,
               M31*X + M32*Y + M33*Z).

% ---------------------------------------------------------------------------- %

    % Here, we multiply by the transpose of the matrix.
    %
transform_normal(M, N0) = N :-

    M  = matrix(M11, M12, M13, _M14,
                M21, M22, M23, _M24,
                M31, M32, M33, _M34),

    N0 = point(X, Y, Z),

    N  = point(M11*X + M21*Y + M31*Z,
               M12*X + M22*Y + M32*Z,
               M13*X + M23*Y + M33*Z).

% ---------------------------------------------------------------------------- %

point_to_object_space(trans(_M, W), P)   = transform_point(W, P).
vector_to_object_space(trans(_M, W), P)  = transform_vector(W, P).
normal_to_object_space(trans(M, _W), P) = transform_normal(M, P).

% ---------------------------------------------------------------------------- %

point_to_world_space(trans(M, _W), P)    = transform_point(M, P).
vector_to_world_space(trans(M, _W), P)   = transform_vector(M, P).
normal_to_world_space(trans(_M, W), P)  = transform_normal(W, P).

% ---------------------------------------------------------------------------- %

identity = trans(I, I) :-
    I = unit_matrix.

% ---------------------------------------------------------------------------- %

unit_matrix = matrix(1.0, 0.0, 0.0, 0.0,
                     0.0, 1.0, 0.0, 0.0,
                     0.0, 0.0, 1.0, 0.0).

% ---------------------------------------------------------------------------- %

compose_translate(Dx, Dy, Dz, trans(M0, W0)) = trans(M, W) :-

    M0  = matrix(M11, M12, M13, M14,
                 M21, M22, M23, M24,
                 M31, M32, M33, M34),
    M   = matrix(M11, M12, M13, M14 + Dx,
                 M21, M22, M23, M24 + Dy,
                 M31, M32, M33, M34 + Dz),

    W0  = matrix(W11, W12, W13, W14,
                 W21, W22, W23, W24,
                 W31, W32, W33, W34),
    W   = matrix(W11, W12, W13, W14 - Dx * W11 - Dy * W12 - Dz * W13,
                 W21, W22, W23, W24 - Dx * W21 - Dy * W22 - Dz * W23,
                 W31, W32, W33, W34 - Dx * W31 - Dy * W32 - Dz * W33).

% ---------------------------------------------------------------------------- %

compose_scale(Sx, Sy, Sz, trans(M0, W0)) = trans(M, W) :-

    ( if ( Sx = 0.0 ; Sy = 0.0 ; Sz = 0.0 ) then
        throw("trans: compose_scale/7: zero scaling factor")
      else
        M0  = matrix(M11, M12, M13, M14,
                     M21, M22, M23, M24,
                     M31, M32, M33, M34),
        M   = matrix(Sx * M11, Sx * M12, Sx * M13, Sx * M14,
                     Sy * M21, Sy * M22, Sy * M23, Sy * M24,
                     Sz * M31, Sz * M32, Sz * M33, Sz * M34),

        W0  = matrix(W11, W12, W13, W14,
                     W21, W22, W23, W24,
                     W31, W32, W33, W34),
        W   = matrix(W11 / Sx, W12 / Sy, W13 / Sz, W14,
                     W21 / Sx, W22 / Sy, W23 / Sz, W24,
                     W31 / Sx, W32 / Sy, W33 / Sz, W34)
    ).

% ---------------------------------------------------------------------------- %

compose_uscale(S, trans(M0, W0)) = trans(M, W) :-

    ( if S = 0.0 then
        throw("trans: compose_uscale/7: zero scaling factor")
      else
        M0  = matrix(M11, M12, M13, M14,
                     M21, M22, M23, M24,
                     M31, M32, M33, M34),
        M   = matrix(S * M11, S * M12, S * M13, S * M14,
                     S * M21, S * M22, S * M23, S * M24,
                     S * M31, S * M32, S * M33, S * M34),

        W0  = matrix(W11, W12, W13, W14,
                     W21, W22, W23, W24,
                     W31, W32, W33, W34),
        W   = matrix(W11 / S, W12 / S, W13 / S, W14,
                     W21 / S, W22 / S, W23 / S, W24,
                     W31 / S, W32 / S, W33 / S, W34)
    ).

% ---------------------------------------------------------------------------- %

compose_rotatex(Rx, trans(M0, W0)) = trans(M, W) :-

    RRx = degrees_to_radians(Rx),
    C   = cos(RRx),
    S   = sin(RRx),

    M0  = matrix(M11, M12, M13, M14,
                 M21, M22, M23, M24,
                 M31, M32, M33, M34),
    M   = matrix(M11          , M12          , M13          , M14           ,
                 C*M21 - S*M31, C*M22 - S*M32, C*M23 - S*M33, C*M24 - S*M34,
                 S*M21 + C*M31, S*M22 + C*M32, S*M23 + C*M33, S*M24 + C*M34),

    W0  = matrix(W11, W12, W13, W14,
                 W21, W22, W23, W24,
                 W31, W32, W33, W34),
    W   = matrix(W11, C*W12 - S*W13, S*W12 + C*W13, W14,
                 W21, C*W22 - S*W23, S*W22 + C*W23, W24,
                 W31, C*W32 - S*W33, S*W32 + C*W33, W34).

% ---------------------------------------------------------------------------- %

compose_rotatey(Ry, trans(M0, W0)) = trans(M, W) :-

    RRy = degrees_to_radians(Ry),
    C   = cos(RRy),
    S   = sin(RRy),

    M0  = matrix(M11, M12, M13, M14,
                 M21, M22, M23, M24,
                 M31, M32, M33, M34),
    M   = matrix(C*M11 + S*M31, C*M12 + S*M32, C*M13 + S*M33, C*M14 + S*M34,
                 M21          , M22          , M23          , M24          ,
                 C*M31 - S*M11, C*M32 - S*M12, C*M33 - S*M13, C*M34 - S*M14),

    W0  = matrix(W11, W12, W13, W14,
                 W21, W22, W23, W24,
                 W31, W32, W33, W34),
    W   = matrix(C*W11 + S*W13, W12, -S*W11 + C*W13, W14,
                 C*W21 + S*W23, W22, -S*W21 + C*W23, W24,
                 C*W31 + S*W33, W32, -S*W31 + C*W33, W34).

% ---------------------------------------------------------------------------- %

compose_rotatez(Rz, trans(M0, W0)) = trans(M, W) :-

    RRz = degrees_to_radians(Rz),
    C   = cos(RRz),
    S   = sin(RRz),

    M0  = matrix(M11, M12, M13, M14,
                 M21, M22, M23, M24,
                 M31, M32, M33, M34),
    M   = matrix(C*M11 - S*M21, C*M12 - S*M22, C*M13 - S*M23, C*M14 - S*M24,
                 S*M11 + C*M21, S*M12 + C*M22, S*M13 + C*M23, S*M14 + C*M24,
                 M31          , M32          , M33          , M34          ),

    W0  = matrix(W11, W12, W13, W14,
                 W21, W22, W23, W24,
                 W31, W32, W33, W34),
    W   = matrix(-S*W12 + C*W11, S*W11 + C*W12, W13, W14,
                 -S*W22 + C*W21, S*W21 + C*W22, W23, W24,
                 -S*W32 + C*W31, S*W31 + C*W32, W33, W34).

% ---------------------------------------------------------------------------- %

degrees_to_radians(Theta) = Theta * pi / 180.0.

% ---------------------------------------------------------------------------- %

show_trans(trans(M, W)) -->
    { M = matrix(M11, M12, M13, M14,
                 M21, M22, M23, M24,
                 M31, M32, M33, M34) },
    { W = matrix(W11, W12, W13, W14,
                 W21, W22, W23, W24,
                 W31, W32, W33, W34) },
    io__format("object -> world space    world -> object space\n", []),
    io__format("(%4.1f %4.1f %4.1f %4.1f)    ", [f(M11),f(M12),f(M13),f(M14)]),
    io__format("(%4.1f %4.1f %4.1f %4.1f)\n",   [f(W11),f(W12),f(W13),f(W14)]),
    io__format("(%4.1f %4.1f %4.1f %4.1f)    ", [f(M21),f(M22),f(M23),f(M24)]),
    io__format("(%4.1f %4.1f %4.1f %4.1f)\n",   [f(W21),f(W22),f(W23),f(W24)]),
    io__format("(%4.1f %4.1f %4.1f %4.1f)    ", [f(M31),f(M32),f(M33),f(M34)]),
    io__format("(%4.1f %4.1f %4.1f %4.1f)\n",   [f(W31),f(W32),f(W33),f(W34)]),
    io__format("( 0.0  0.0  0.0  1.0)    ( 0.0  0.0  0.0  1.0)\n", []).

% ---------------------------------------------------------------------------- %

    % INTERSECTION WITH THE PLANE y =< 0
    %
    % T    - transformation
    % P, D - point and direction vector defining a line
    % POI  - point of intersection with the plane
    % TC   - texture coordinates
    % N    - unit surface normal
    %
    % NOTE: recall that P and D are in world space, so we transform
    % them via W (the inverse of the transformation M that has been
    % applied to the plane y =< 0) into object space where we can
    % work more easily.  If we find a point of intersection, we have
    % to translate it back into world space via the transformation M.
    %
intersects_plane(Id, T, P, D, Surface, IntersectionResults) :-

        % The plane is defined by y =< 0.

        % First, translate the world space line into object space.
        %
    point(X,  Y,  Z)  = point_to_object_space(T, P),
    point(Dx, Dy, Dz) = vector_to_object_space(T, D),

        % The plane is defined by y =< 0, therefore we need to find
        % k s.t. Y + kDy = 0.  If Dy = 0 then the line does not intersect the
        % plane.  Otherwise the POI in object space is (X + kDx, 0, Z + kDz)
        % which we then translate back into world space.  Note that these are
        % also the texture coordinates.

    (
	Dy \= 0.0,                    % The ray must not parallel the plane.
	K = -Y / Dy,
	K > 0.0                      % The ray must point at the plane.
    ->
	POI = point_to_world_space(T, point(X + K * Dx, 0.0, Z + K * Dz)),
	TC  = surface_coordinates(0, X + K * Dx, Z + K * Dz),

	    % The normal to the plane is the y axis.
	    %
	N = unit(vector_to_world_space(T, point(0.0, 1.0, 0.0))),

	IntersectionResults = [intersection(Id, POI, N, TC, Surface)]
    ;
    	IntersectionResults = []
    ).

% ---------------------------------------------------------------------------- %

    % T    - transformation
    % P, D - point and direction vector defining a line
    % POI  - point of intersection with the plane
    % TC   - texture coordinates
    % N    - unit surface normal
    %
    % NOTE: recall that P and D are in world space, so we transform
    % them via W (the inverse of the transformation M that has been
    % applied to the unit origin sphere) into object space where we can
    % work more easily.  If we find a point of intersection, we have
    % to translate it back into world space via the transformation M.
    %
    % This algorithm is based on the web page
    %   http://www.cs.fit.edu/wds/classes/adv-graphics/raytrace/raytrace.html
    %
intersects_sphere(Id, T, P, D, Surface, IntersectionResults) :-

        % The sphere is defined by x^2 + y^2 + z^2 =< 1.

        % First, translate the world space line into object space.
        %
    point(X,  Y,  Z)  = point_to_object_space(T, P),
    point(Dx, Dy, Dz) = unit(vector_to_object_space(T, D)),

        % If SqrLoc < 1 then the line originates from within the sphere.
        %
    SqrLoc = (X * X) + (Y * Y) + (Z * Z),

    Tca = (-X * Dx + -Y * Dy + -Z * Dz),

        % If SqrLoc > 1 and Tca < 0 then the ray is pointing away from
        % the sphere and not emanating from within it.
        %
    ( not ( SqrLoc > 1.0, Tca < 0.0 ) ->

	SqrTca = Tca * Tca,
	SqrD   = SqrLoc - SqrTca,
	SqrThc = 1.0 - SqrD,

	    % If the half-chord distance is negative then the ray does not
	    % intersect with the sphere.
	    %
	( SqrThc >= 0.0 ->

		% Finally, we compute the point of intersection with the sphere.
		% If SqrLoc >= 1.0 we are outside the sphere, otherwise
		% we are inside the sphere.
		%
	    Thc = sqrt(SqrThc),
	    DistToSurface1 = Tca - Thc,
	    DistToSurface2 = Tca + Thc,

	    intersects_sphere_2(Id, DistToSurface1, T, X, Y, Z, Dx, Dy, Dz,
	    	Surface, IntersectionResult1),
	    intersects_sphere_2(Id, DistToSurface2, T, X, Y, Z, Dx, Dy, Dz,
	    	Surface, IntersectionResult2),
	    IntersectionResults = [IntersectionResult1, IntersectionResult2]

	;
	    IntersectionResults = []
	)
    ;
    	IntersectionResults = []
    ).

:- pred intersects_sphere_2(object_id, real, trans, real, real, real, real,
        real, real, surface, intersection).
:- mode intersects_sphere_2(in, in, in, in, in, in, in, in, in, in, out) is det.

intersects_sphere_2(Id, DistToSurface, T, X, Y, Z, Dx, Dy, Dz, Surface,
		Intersection) :-
    IX = X + DistToSurface * Dx,
    IY = Y + DistToSurface * Dy,
    IZ = Z + DistToSurface * Dz,

    POI = point_to_world_space(T, point(IX, IY, IZ)),

    V = 0.5 * (IY + 1.0),
    % SqrIY = IY * IY,
    % U = positive_asin(IX / sqrt(1.0 - SqrIY)) / (pi + pi),
    U = calc_u(IX, IZ),

    TC  = surface_coordinates(0, U, V),

    % Since the sphere is lying at the origin (after transformation), the
    % surface normal is simply the vector from the center of the sphere in
    % the direction of the point of intersection.
    %
    N = unit(normal_to_world_space(T, point(IX, IY, IZ))),

    Intersection = intersection(Id, POI, N, TC, Surface).

% ---------------------------------------------------------------------------- %

:- type face_intersection               % Face, IX, IY, IZ.
    --->    fi(int, float, float, float).

:- type face_intersections == list(face_intersection).

    % T    - transformation
    % P, D - point and direction vector defining a line
    % POI  - point of intersection with the plane
    % TC   - texture coordinates
    % N    - unit surface normal
    %
    % NOTE: recall that P and D are in world space, so we transform
    % them via W (the inverse of the transformation M that has been
    % applied to the unit origin sphere) into object space where we can
    % work more easily.  If we find a point of intersection, we have
    % to translate it back into world space via the transformation M.
    %
    % This algorithm is based on the web page
    %   http://www.cs.fit.edu/wds/classes/adv-graphics/raytrace/raytrace.html
    %
intersects_cube(Id, T, P, D, Surface, IntersectionResults) :-

        % The cube is defined by 0 =< x,y,z =< 1.

        % First, translate the world space line into object space.
        %
    point(X,  Y,  Z)  = point_to_object_space(T, P),
    point(Dx, Dy, Dz) = vector_to_object_space(T, D),

        % The cube has six faces defined by the planes
        % z = 0 (face 0)
        % z = 1 (face 1)
        % x = 0 (face 2)
        % x = 1 (face 3)
        % y = 1 (face 4)
        % y = 0 (face 5)
        %
        % We need to find the nearest point of intersection
        % if there is one.
        %
    Face0Intersection = z_face_intersection(0, 0.0, X, Y, Z, Dx, Dy, Dz),
    Face1Intersection = z_face_intersection(1, 1.0, X, Y, Z, Dx, Dy, Dz),
    Face2Intersection = x_face_intersection(2, 0.0, X, Y, Z, Dx, Dy, Dz),
    Face3Intersection = x_face_intersection(3, 1.0, X, Y, Z, Dx, Dy, Dz),
    Face4Intersection = y_face_intersection(4, 1.0, X, Y, Z, Dx, Dy, Dz),
    Face5Intersection = y_face_intersection(5, 0.0, X, Y, Z, Dx, Dy, Dz),

    FaceIntersections = list__condense([Face0Intersection,
	Face1Intersection, Face2Intersection, Face3Intersection,
	Face4Intersection, Face5Intersection]),
    IntersectionResults = list__map(process_cube_intersections(Id, T, Surface),
    	FaceIntersections).

:- func process_cube_intersections(object_id, trans, surface,
            face_intersection) = intersection.

process_cube_intersections(Id, T, Surface, fi(Face, IX, IY, IZ)) =
		Intersection :-
    (      if Face = 0 then U = IX, V = IY, NX =  0.0, NY =  0.0, NZ = -1.0
      else if Face = 1 then U = IX, V = IY, NX =  0.0, NY =  0.0, NZ =  1.0
      else if Face = 2 then U = IZ, V = IY, NX = -1.0, NY =  0.0, NZ =  0.0
      else if Face = 3 then U = IZ, V = IY, NX =  1.0, NY =  0.0, NZ =  0.0
      else if Face = 4 then U = IX, V = IZ, NX =  0.0, NY =  1.0, NZ =  0.0
      else if Face = 5 then U = IX, V = IZ, NX =  0.0, NY = -1.0, NZ =  0.0
      else throw("trans: intersects_cube/6: not a face!")
    ),

    POI = point_to_world_space(T, point(IX, IY, IZ)),
    TC  = surface_coordinates(Face, U, V),
    N   = unit(normal_to_world_space(T, point(NX, NY, NZ))),

    Intersection = intersection(Id, POI, N, TC, Surface).

% ---------------------------------------------------------------------------- %

    % There is definitely a wittier way to accomplish this but my
    % brain is too small to encompass it.

:- func z_face_intersection(int,float,float,float,float,float,float,float) =
            face_intersections.

z_face_intersection(Face, Side, X, Y, Z, Dx, Dy, Dz) = MaybeIntersection :-
    ( if Dz = 0.0 then
        MaybeIntersection = []      % Parallel to the face.
      else
        K = (Side - Z) / Dz,
        ( if K < 0.0 then
            MaybeIntersection = []  % Pointing away from the face.
          else
            IX = X + K * Dx,
            IY = Y + K * Dy,
            ( if ( 0.0 =< IX, IX =< 1.0, 0.0 =< IY, IY =< 1.0 ) then
                MaybeIntersection = [fi(Face, IX, IY, Side)]
              else
                MaybeIntersection = []
            )
        )
    ).

% ---------------------------------------------------------------------------- %

:- func x_face_intersection(int,float,float,float,float,float,float,float) =
            face_intersections.

x_face_intersection(Face, Side, X, Y, Z, Dx, Dy, Dz) = MaybeIntersection :-
    ( if Dx = 0.0 then
        MaybeIntersection = []      % Parallel to the face.
      else
        K = (Side - X) / Dx,
        ( if K < 0.0 then
            MaybeIntersection = []  % Pointing away from the face.
          else
            IY = Y + K * Dy,
            IZ = Z + K * Dz,
            ( if ( 0.0 =< IZ, IZ =< 1.0, 0.0 =< IY, IY =< 1.0 ) then
                MaybeIntersection = [fi(Face, Side, IY, IZ)]
              else
                MaybeIntersection = []
            )
        )
    ).

% ---------------------------------------------------------------------------- %

:- func y_face_intersection(int,float,float,float,float,float,float,float) =
            face_intersections.

y_face_intersection(Face, Side, X, Y, Z, Dx, Dy, Dz) = MaybeIntersection :-
    ( if Dy = 0.0 then
        MaybeIntersection = []      % Parallel to the face.
      else
        K = (Side - Y) / Dy,
        ( if K < 0.0 then
            MaybeIntersection = []  % Pointing away from the face.
          else
            IX = X + K * Dx,
            IZ = Z + K * Dz,
            ( if ( 0.0 =< IZ, IZ =< 1.0, 0.0 =< IX, IX =< 1.0 ) then
                MaybeIntersection = [fi(Face, IX, Side, IZ)]
              else
                MaybeIntersection = []
            )
        )
    ).

% ---------------------------------------------------------------------------- %

    % INTERSECTION WITH THE PLANE y =< 0
    %
    % T    - transformation
    % P, D - point and direction vector defining a line
    % POI  - point of intersection with the plane
    % TC   - texture coordinates
    % N    - unit surface normal
    %
    % NOTE: recall that P and D are in world space, so we transform
    % them via W (the inverse of the transformation M that has been
    % applied to the plane y =< 0) into object space where we can
    % work more easily.  If we find a point of intersection, we have
    % to translate it back into world space via the transformation M.
    %
intersects_cylinder(Id, T, P, D, Surface, IntersectionResults) :-

        % The cylinder is defined by 0 =< y =< 1, x^2 + z^2 =< 1.

        % First, translate the world space line into object space.
        %
    point(X,  Y,  Z)  = point_to_object_space(T, P),
    point(Dx, Dy, Dz) = vector_to_object_space(T, D),

        % The three surfaces are
        % x^2 + z^2 = 1 (face 0)
        % y = 1         (face 1)
        % y = 0         (face 2)
        %
    Face0Intersections = cylinder_intersection(0, X, Y, Z, Dx, Dy, Dz),
    Face1Intersections = disc_intersection(1, 1.0, X, Y, Z, Dx, Dy, Dz),
    Face2Intersections = disc_intersection(2, 0.0, X, Y, Z, Dx, Dy, Dz),

    FaceIntersections = list__condense([Face0Intersections,
    		Face1Intersections, Face2Intersections]),
    IntersectionResults = list__map(process_cylinder_intersections(Id, T,
            Surface), FaceIntersections).

:- func process_cylinder_intersections(object_id, trans, surface,
            face_intersection) = intersection.

process_cylinder_intersections(Id, T, Surface, fi(Face, IX, IY, IZ)) =
	Intersection :-
    (      if Face = 0 then
                % U = positive_asin(IX) / (pi + pi),
                U = calc_u(IX, IZ),
                V = IY,
                NX =  IX, NY =  0.0, NZ =  IZ
      else if Face = 1 then
                U = 0.5*IX+0.5, V = 0.5*IZ+0.5,   NX = 0.0, NY =  1.0, NZ = 0.0
      else if Face = 2 then
                U = 0.5*IX+0.5, V = 0.5*IZ+0.5,   NX = 0.0, NY = -1.0, NZ = 0.0
      else
                throw("trans: intersects_cylinder/6: not a face!")
    ),

    POI = point_to_world_space(T, point(IX, IY, IZ)),
    TC  = surface_coordinates(Face, U, V),
    N   = unit(normal_to_world_space(T, point(NX, NY, NZ))),

    Intersection = intersection(Id, POI, N, TC, Surface).

% ---------------------------------------------------------------------------- %

intersects_cone(Id, T, P, D, Surface, IntersectionResults) :-

        % The cone is defined by 0 =< y =< 1, x^2 + z^2 - y^2 =< 1.

        % First, translate the world space line into object space.
        %
    point(X,  Y,  Z)  = point_to_object_space(T, P),
    point(Dx, Dy, Dz) = vector_to_object_space(T, D),

        % The two surfaces are
        % x^2 + z^2 - y^2 = 1 (face 0)
        % y = 1         (face 1)
        %
    Face0Intersections = cone_intersection(0, X, Y, Z, Dx, Dy, Dz),
    Face1Intersections = disc_intersection(1, 1.0, X, Y, Z, Dx, Dy, Dz),

    FaceIntersections = list__append(Face1Intersections, Face0Intersections),
    IntersectionResults = list__map(process_cone_intersections(Id, T, Surface),
    		FaceIntersections).

:- func process_cone_intersections(object_id, trans, surface,
            face_intersection) = intersection.

process_cone_intersections(Id, T, Surface, fi(Face, IX, IY, IZ)) =
		Intersection :-
    (      if Face = 0 then
                % IXIY0 = IX / IY,
                % IXIY = (if IXIY0 > 1.0 then 1.0 else if IXIY0 < -1.0 then -1.0
                		% else IXIY0),
                % U = positive_asin(IXIY) / (pi + pi), V = IY,
                U = calc_u(IX, IZ), V = IY,
                NX =  IX, NY = -sqrt(IX*IX + IZ*IZ), NZ =  IZ
      else if Face = 1 then
                U = 0.5*IX+0.5, V = 0.5*IZ+0.5,   NX = 0.0, NY =  1.0, NZ = 0.0
      else
                throw("trans: intersects_cylinder/6: not a face!")
    ),

    POI = point_to_world_space(T, point(IX, IY, IZ)),
    TC  = surface_coordinates(Face, U, V),
    N   = unit(normal_to_world_space(T, point(NX, NY, NZ))),

    Intersection = intersection(Id, POI, N, TC, Surface).

% ---------------------------------------------------------------------------- %

:- func cylinder_intersection(int, float, float, float, float, float, float) =
            face_intersections.

cylinder_intersection(Face, X, Y, Z, Dx, Dy, Dz) = 
	    solve_quadratic(Face, X, Y, Z, Dx, Dy, Dz, A, B, C)
	:-
    A    = (Dx * Dx) + (Dz * Dz),
    B    = 2.0 * ((X * Dx) + (Z * Dz)),
    C    = (X * X) + (Z * Z) - 1.0.

% ---------------------------------------------------------------------------- %

:- func cone_intersection(int, float, float, float, float, float, float) =
            face_intersections.

cone_intersection(Face, X, Y, Z, Dx, Dy, Dz) = 
	    solve_quadratic(Face, X, Y, Z, Dx, Dy, Dz, A, B, C)
	:-
    A    = (Dx * Dx) + (Dz * Dz) - (Dy * Dy),
    B    = 2.0 * ((X * Dx) + (Z * Dz) - (Y * Dy)),
    C    = (X * X) + (Z * Z) - (Y * Y).

% ---------------------------------------------------------------------------- %

:- func solve_quadratic(int, float, float, float, float, float, float, float,
		float, float) = face_intersections.
solve_quadratic(Face, X, Y, Z, Dx, Dy, Dz, A, B, C) = Intersections :-

        % Optimized quadratic solution from "Numerical Recipes in Pascal".
        %
    BB4AC = B * B - 4.0 * A * C,
    ( if BB4AC < 0.0 then
        Intersections = []
    else
        SgnB = ( if B < 0.0 then -1.0 else 1.0 ),
        Q    = -0.5 * (B + SgnB * sqrt(BB4AC)),
        K1   = Q / A,
        K2   = C / Q,
        maybe_add_intersection(K1, Face, X, Y, Z, Dx, Dy, Dz, [],
                Intersections0),
        maybe_add_intersection(K2, Face, X, Y, Z, Dx, Dy, Dz, Intersections0,
                Intersections)
    ).

:- pred maybe_add_intersection(float::in, int::in, float::in, float::in,
        float::in, float::in, float::in, float::in, face_intersections::in,
        face_intersections::out) is det.

maybe_add_intersection(K, Face, X, Y, Z, Dx, Dy, Dz, Intersections0,
        Intersections) :-
    (
        K >= 0.0,
        IY = Y + K * Dy,
        0.0 =< IY,
        IY =< 1.0
    ->
        IX = X + K * Dx,
        IZ = Z + K * Dz,
	    Intersections = [fi(Face, IX, IY, IZ) | Intersections0]
    ;
        Intersections = Intersections0
    ).

% ---------------------------------------------------------------------------- %

:- func disc_intersection(int,float,float,float,float,float,float,float) =
            face_intersections.

disc_intersection(Face, Side, X, Y, Z, Dx, Dy, Dz) = MaybeIntersection :-
    ( if Dy = 0.0 then
        MaybeIntersection = []          % Parallel to the disc.
      else
        K = (Side - Y) / Dy,
        ( if K < 0.0 then
            MaybeIntersection = []      % Pointing away from the disc.
          else
            IX = X + K * Dx,
            IZ = Z + K * Dz,
            ( if (IX * IX) + (IZ * IZ) =< 1.0 then
                MaybeIntersection = [fi(Face, IX, Side, IZ)]
              else
                MaybeIntersection = []
            )
        )
    ).

% ---------------------------------------------------------------------------- %

inside_sphere(Point0, Trans) :-
    Point = point_to_object_space(Trans, Point0),
    mag2(Point) =< 1.0.

% ---------------------------------------------------------------------------- %

inside_plane(Point, Trans) :-
    point(_X,  Y,  _Z) = point_to_object_space(Trans, Point),
    Y =< 0.0.

% ---------------------------------------------------------------------------- %

inside_cube(Point, Trans) :-
    point(X, Y, Z) = point_to_object_space(Trans, Point),
    X >= 0.0,
    Y >= 0.0,
    Z >= 0.0,
    X =< 1.0,
    Y =< 1.0,
    Z =< 1.0.

% ---------------------------------------------------------------------------- %

inside_cylinder(Point, Trans) :-
    point(X, Y, Z) = point_to_object_space(Trans, Point),
    Y >= 0.0,
    Y =< 1.0,
    X*X + Z*Z =< 1.0.

% ---------------------------------------------------------------------------- %

inside_cone(Point, Trans) :-
    point(X, Y, Z) = point_to_object_space(Trans, Point),
    Y >= 0.0,
    Y =< 1.0,
    X*X + Z*Z - Y*Y =< 0.0.

% ---------------------------------------------------------------------------- %

    % Given X and Z, find U in [0, 1].
    %
    % From section 3.6, we have that
    %       X = sqrt(1-y*y) * sin(2 * pi * U)
    % and   Z = sqrt(1-y*y) * cos(2 * pi * U)
    %
    % therefore X / Z = tan(2 * pi * U)
    % therefore U = atan(X / Z) / (2 * pi)
    %
    % Atan2 returns an angle in the range (-pi, pi), but we need angles
    % in the range (0, 2*pi).  We therefore leave the segment (0, pi) 
    % unchanged, and translate the segment (-pi, 0) by a full rotation.
    %
:- func calc_u(float, float) = float.

calc_u(X, Z) = U :-
    U0 = atan2(X, Z) / (pi + pi),
        % -0.5 =< U0 =< 0.5
    U  = ( if U0 > 0.0 then U0 else 1.0 + U0 ).
        % 0.0 =< U =< 1.0

    % asin(X) returns a value in [-pi, pi]; we want one that
    % returns a value in [0, 2pi].
    %
:- func positive_asin(float) = float.

positive_asin(X) = PosAsinX :-
    AsinX    = asin(X),
    PosAsinX = ( if AsinX < 0.0 then pi + pi - AsinX else AsinX ).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
