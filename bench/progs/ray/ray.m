% vim: sw=4 ts=4 expandtab

:- module ray.
% This module does the actual ray tracing.  It returns a pixel value of type
% maybe(Colour).  This is either 'yes(Colour_to_make_pixel)', if an intersection
% with an object is found or else 'no' if the ray does not intersect an object.

% Note: currently only a simple non-recursive ray-tracer has been implemented.

:- interface.

:- import_module list, vec3, float, int, gfx.

%  ray_trace(Iterations, Camera, Direction, Scene, Ambient, Lights, SpecC, SpecE, Shading, Pixel)
:- pred ray_trace(int, vec3, vec3, scene, float, list(light), float, float, shade_type, colour).
:- mode ray_trace(in, in, in, in, in, in, in, in, in, out) is det.

:- implementation.

:- import_module debug, string, math, require.

% do the ray tracing for a pixel in the image.
% This is currently non-recursive, but could easily be modified to make it
% recursive.
ray_trace(Lim, Camera, Dir, Scene, Ambient, Lights, SpecC, SpecE,
		Shading, Colour) :-
    (
        closest(Scene, Camera, Dir, _Dist, Point, Triangle, FaceNormal, IntrColour)
    ->
        calculate_normal(Point, FaceNormal, Shading, Triangle, PointNormal),

	% calculate ambient component of shading
	AmbColour = Ambient * IntrColour,
	shade_from_light_sources(Lights, IntrColour, Point,
			PointNormal, -Dir, SpecC, SpecE, ShadeColour),
	( Lim < 0 ->
		% calculate ambient component of shading
		Colour = AmbColour + ShadeColour
	;
		Colour = AmbColour + ShadeColour
%		Dir1 = Dir - scale(2.0*dot(Dir, PointNormal), PointNormal),
%		ray_trace(Lim-1, Camera, Dir1, Scene, Ambient, Lights,
%			SpecC, SpecE, Shading, Colour0),
%		Colour0 = colour(R0, G0, B0),
%		IntrColour = colour(R1, G1, B1),
%		Colour = colour(R0*R1, G0*G1, B0*B1) + ShadeColour
	)
		
    ;
        Colour = colour(0.0, 0.0, 0.0)
    ).

% find the closest intersection of an object with the ray
% fail if no intersection is found
:- pred closest(scene, vec3, vec3, float, vec3, triangle, vec3, colour).
:- mode closest(in, in, in, out, out, out, out, out) is semidet.

closest([Poly | Scene1], Camera, Dir, Dist, Intersect, Triangle, FaceNormal, IntrColour) :-
    (
        find_intersection_with_plane(Poly, Camera, Dir, Intersect0, Dist0)
    ->
        (
            closest(Scene1, Camera, Dir, Dist1, Intersect1, Triangle1, FaceNormal1, IntrColour1)
        ->
            (
                Dist0 < Dist1
            ->
                (
                    Poly = polytri(Triangles, _, _),
                    find_intersection_with_polygon(Triangles, Intersect0, Triangle0)
                ->
                    Dist = Dist0,
                    Intersect = Intersect0,
                    Triangle = Triangle0,
                    Poly = polytri(_, FaceNormal, IntrColour)
                ;
                    Dist = Dist1,
                    Intersect = Intersect1,
                    Triangle = Triangle1,
                    FaceNormal = FaceNormal1,
                    IntrColour = IntrColour1
                )
            ;
                Dist = Dist1,
                Intersect = Intersect1,
                Triangle = Triangle1,
                FaceNormal = FaceNormal1,
                IntrColour = IntrColour1
            )
        ;
            Poly = polytri(Triangles, FaceNormal, IntrColour),
            find_intersection_with_polygon(Triangles, Intersect0, Triangle),
            Intersect = Intersect0,
            Dist = Dist0
        )
    ;
        closest(Scene1, Camera, Dir, Dist, Intersect, Triangle, FaceNormal, IntrColour)
    ).

% Use parametric equations of plane and ray to find their intersection.
% The parameter also gives the distance from the camera to the intersection
% point.  
% This predicate fails if the ray and plane are parallel or if the point of
% intersection is behind the camera.
:- pred find_intersection_with_plane(polytri, vec3, vec3, vec3, float).
:- mode find_intersection_with_plane(in, in, in, out, out) is semidet.

find_intersection_with_plane(Poly, Camera, Dir, Intersect, Dist) :-
    (
        Poly = polytri(Triangles, Normal, _),
        % check that ray and plane are not parallel
        Denom = dot(Dir, Normal),
        Denom \= 0.0,

        % get a point on the plane
        Triangles = [tri(V1, _, _, _, _, _) | _],

        % get parameter of point on ray
        Dist = (dot(V1, Normal) - dot(Camera, Normal)) / Denom,
        
        % check that intersection is not behind us
        Dist >= 0.0,

        Intersect = Camera + scale(Dist, Dir)
    ).

% We know that the ray intersects the plane of the polygon.  Now try each
% triangle in the polygon to see if the intersection point is inside one of
% them.
% 
% Try projecting triangle to the XY plane.  If the area of the projection is
% zero then try the XZ and then YZ planes.  The project the intersection point
% to the plane and see if it is inside the projected triangle.
:- pred find_intersection_with_polygon(list(triangle), vec3, triangle).
:- mode find_intersection_with_polygon(in, in, out) is semidet.

find_intersection_with_polygon(Triangles, Intersect, Triangle) :-
    (
        Triangles = [Tri1 | Triangles1],
        (
            Tri1 = tri(T1, T2, T3, _, _, _),
            T1 = vec(X1, Y1, Z1),    
            T2 = vec(X2, Y2, Z2),
            T3 = vec(X3, Y3, Z3),
            Intersect = vec(XI, YI, ZI),
            triangle_area2(vec(X1, Y1, 0.0), vec(X2, Y2, 0.0), vec(X3, Y3, 0.0), AreaXY),
            (
                AreaXY \= 0.0
            ->
%               dump("find_intersection_with_polygon: about to call point_in_triangle\n", []),
                point_in_triangle(vec(X1, Y1, 0.0), vec(X2, Y2, 0.0), vec(X3, Y3, 0.0), vec(XI, YI, 0.0))
            ;
                triangle_area2(vec(X1, 0.0, Z1), vec(X2, 0.0, Z2), vec(X3, 0.0, Z3), AreaXZ),
                (
                    AreaXZ \= 0.0
                ->
                    % point_in_triangle requires triangle to be on X-Y plane
                    % so need to transform co-ordinates.
                    point_in_triangle(vec(X1, Z1, 0.0), vec(X2, Z2, 0.0), vec(X3, Z3, 0.0), vec(XI, ZI, 0.0))
                ;
                    point_in_triangle(vec(Z1, Y1, 0.0), vec(Z2, Y2, 0.0), vec(Z3, Y3, 0.0), vec(ZI, YI, 0.0))
                )
            )
        ->
            Triangle = Tri1
        ;
            find_intersection_with_polygon(Triangles1, Intersect, Triangle)
        )
    ).

/*
% calculate double the square of the area of the triangle
:- pred triangle_area2(vec3, vec3, vec3, float).
:- mode triangle_area2(in, in, in, out) is det.

triangle_area2(V1, V2, V3, A2) :-
%    A = mag(cross(V1 - V2, V3 - V2)). % changed to increase efficiency
     vec(X, Y, Z) = mycross(myminus(V1, V2), myminus(V3, V2)),
     A2 = X*X + Y*Y + Z*Z.  

:- func mycross(vec3, vec3) = vec3.

mycross(vec(Ax, Ay, Az), vec(Bx, By, Bz)) = vec(Cx, Cy, Cz) :-
	Cx = Ay*Bz - Az*By,
	Cy = Az*Bx - Ax*Bz,
	Cz = Ax*By - Ay*Bx.

% put this here so it is inlined to reduce cross-module call overhead
:- func myminus(vec3, vec3) = vec3.

myminus(vec(Ax, Ay, Az), vec(Bx, By, Bz)) = vec(Ax - Bx, Ay - By, Az - Bz).
*/
:- pred triangle_area2(vec3, vec3, vec3, float).
:- mode triangle_area2(in, in, in, out) is det.

triangle_area2(V1, V2, V3, A) :-
	V1 = vec(X1, Y1, Z1),
	V2 = vec(X2, Y2, Z2),
	V3 = vec(X3, Y3, Z3),
	area3(X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3, A).

:- pred area3(float, float,  float,  float,  float,  float,  float,  float,  float,  float).
:- mode area3(in, in,  in,  in,  in,  in,  in,  in,  in,  out) is det.

:- pragma c_code(area3(X1::in, X2::in, X3::in, Y1::in, Y2::in, Y3::in, Z1::in, Z2::in, Z3::in, A::out), "
{
	float X4, Y4, Z4, X5, Y5, Z5;
	float Ax, Bx, Cx, Ay, By, Cy, Az, Bz, Cz;

	X4 = X1 - X2; Y4 = Y1 - Y2; Z4 = Z1 - Z2;
	X5 = X3 - X2; Y5 = Y3 - Y2; Z5 = Z3 - Z2;
	Ax = X4; Ay = Y4; Az = Z4;
	Bx = X5; By = Y5; Bz = Z5;
	Cx = Ay*Bz - Az*By;
	Cy = Az*Bx - Ax*Bz;
	Cz = Ax*By - Ay*Bx;
	A = Cx*Cx + Cy*Cy + Cz*Cz;
}
").


%debugging %%dmo
:- pred dump_triangle(triangle, vec3, colour).
:- mode dump_triangle(in, in, in) is det.

dump_triangle(T, N, C) :-
    (
        dump("============\n", []),
        T = tri(vec(X1, Y1, Z1), vec(X2, Y2, Z2), vec(X3, Y3, Z3), _, _, _),
        dump("    (%f, %f, %f)\n    (%f, %f, %f)\n    (%f, %f, %f)\n", 
            [f(X1), f(Y1), f(Z1), f(X2), f(Y2), f(Z2), f(X3), f(Y3), f(Z3)]),
        N = vec(X, Y, Z),
        dump("norm(%f, %f, %f)\n", [f(X), f(Y), f(Z)]),
        C = colour(R, G, B),
        dump("col (%f, %f, %f)\n", [f(R), f(G), f(B)])
    ).

% calculate_normal(Point, FaceNormal, Shading, Triangle, PointNormal)
% calculate the normal for this point on the face using either full or phong
% shading.
% Method:
% 1.    Workout where vector (V1 - Point) intersects with vector (V3 - V2).
% 2.    Interpolate normal for the intersection point between V2 and V3.
% 3.    Interpolate normal for Point between intersection point and V1.
%
% This method was chosen so that artefacts don't show up from the way I've split
% all the polygons up into triangles.

:- pred calculate_normal(vec3, vec3, shade_type, triangle, vec3).
:- mode calculate_normal(in, in, in, in, out) is det.

calculate_normal(Point, FaceNormal, Shading, Triangle, PointNormal) :-
    (
        Shading = full,
        PointNormal = FaceNormal
    ;
        % for phong shading, interpolate the normals from the vertices of the
        % triangle
        Shading = phong,
        Triangle = tri(V1, V2, V3, N1, N2, N3),
        get_line_intersection(V2, unit(V3 - V2), V1, unit(Point - V1), Intersect),
        N23 = unit(scale(mag(V2 - Intersect), N3) +
                   scale(mag(V3 - Intersect), N2)),
        PointNormal = unit(scale(mag(V1 - Point), N23) +
                           scale(mag(Intersect - Point), N1))
    ).

% Calculate the lambertian component of a pixel's colour
:- pred shade_from_light_sources(list(light), colour, vec3, vec3, vec3, float, float, colour).
:- mode shade_from_light_sources(in, in, in, in, in, in, in, out) is det.

shade_from_light_sources(Lights, IntrColour, Point, PointNormal, V, SpecC, SpecE, ShadColour) :-
    (
        Lights = [],
        ShadColour = colour(0.0, 0.0, 0.0)
    ;
        Lights = [light(Pow, Loc) | Lights1],
        L = unit(Loc - Point),
        Cos = dot(L, PointNormal),
        (
            % make sure light is in front of face, not behind it
            Cos > 0.0
        ->
            Lambertian = Pow * Cos * IntrColour,
            R = scale(2.0 * Cos, PointNormal) - L,
            DotRV = dot(R, V),
            (
                ( DotRV =< 0.0 ; SpecC =< 0.0 )
            ->
                Specular = colour(0.0, 0.0, 0.0)
            ;
                Sp = math__pow(DotRV, SpecE),
                Specular = SpecC * Sp * colour(1.0, 1.0, 1.0)
            ),
            ShadColour0 = Lambertian + Specular
        ;
            ShadColour0 = colour(0.0, 0.0, 0.0)
        ),
        shade_from_light_sources(Lights1, IntrColour, Point, PointNormal, V, SpecC, SpecE, ShadColour1),
        ShadColour = ShadColour0 + ShadColour1
    ).

% Get intersection point of 2 co-planar lines in 3D
:- pred get_line_intersection(vec3, vec3, vec3, vec3, vec3).
:- mode get_line_intersection(in, in, in, in, out) is det.

get_line_intersection(V2, D2, V1, D1, Intersect) :-
    (
        V2 = vec(V2X, V2Y, V2Z),
        D2 = vec(D2X, D2Y, D2Z),
        V1 = vec(V1X, V1Y, V1Z),
        D1 = vec(D1X, D1Y, D1Z),
        (
            D2Y \= 0.0,
            Denom1 = D1X - D1Y*D2X/D2Y,
            Denom1 \= 0.0
        ->
            T = ((V2X - V1X) - D2X/D2Y*(V2Y - V1Y)) / Denom1
        ;
            D2Z \= 0.0,
            Denom2 = D1Y - D1Z*D2Y/D2Z,
            Denom2 \= 0.0
        ->
            T = ((V2Y - V1Y) - D2Y/D2Z*(V2Z - V1Z)) / Denom2
        ;
            D2X \= 0.0,
            Denom3 = D2Z - D1X*D2Z/D2X,
            Denom3 \= 0.0
        ->
            T = ((V2Z - V1Z) - D2Z/D2X*(V2X - V1X)) / Denom3
        ;
            error("get_line_intersection: problem with triangle")
        ),
        Intersect = V1 + scale(T, D1)
    ).
