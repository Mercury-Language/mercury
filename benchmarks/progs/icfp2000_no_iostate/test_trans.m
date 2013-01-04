:- module test_trans.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

% ---------------------------------------------------------------------------- %

:- implementation.
:- import_module vector, trans, string, list, float.

main -->

    test_trans("translation by 10 20 30", compose_translate(10.0, 20.0, 30.0)),
    test_trans("scaling by 2 4 8",        compose_scale(2.0, 4.0, 8.0)),
    test_trans("scaling by 2",            compose_uscale(2.0)),
    test_trans("x-rotation by 90'",       compose_rotatex(90.0)),
    test_trans("y-rotation by 90'",       compose_rotatey(90.0)),
    test_trans("z-rotation by 90'",       compose_rotatez(90.0)),

    test_plane(5.0,  4.0, 3.0, 0.5, -1.0, 0.5),
    test_plane(5.0,  4.0, 3.0, 0.5,  1.0, 0.5),
    test_plane(5.0, -4.0, 3.0, 0.5, -1.0, 0.5),
    test_plane(5.0, -4.0, 3.0, 0.5,  1.0, 0.5),

    test_sphere(5.0, 0.0, 0.0, -1.0, 0.0, 0.0),
    test_sphere(5.0, 0.0, 0.0,  1.0, 0.0, 0.0),
    test_sphere(0.3, 0.0, 0.0, -1.0, 0.0, 0.0),
    test_sphere(0.3, 0.0, 0.0,  1.0, 0.0, 0.0),

    test_sphere(-5.0, 0.0, 0.0, -1.0, 0.0, 0.0),
    test_sphere(-5.0, 0.0, 0.0,  1.0, 0.0, 0.0),
    test_sphere(-0.3, 0.0, 0.0, -1.0, 0.0, 0.0),
    test_sphere(-0.3, 0.0, 0.0,  1.0, 0.0, 0.0),

    test_cube( 3.5,  0.5,  0.5,   -1.0,  0.0,  0.0),
    test_cube(-3.5,  0.5,  0.5,   -1.0,  0.0,  0.0),
    test_cube( 3.5,  0.5,  0.5,    1.0,  0.0,  0.0),
    test_cube(-3.5,  0.5,  0.5,    1.0,  0.0,  0.0),
    test_cube(-3.5,  1.5,  0.5,    1.0,  0.0,  0.0),

    test_cube( 0.5,  3.5,  0.5,    0.0, -1.0,  0.0),
    test_cube( 0.5, -3.5,  0.5,    0.0, -1.0,  0.0),
    test_cube( 0.5,  3.5,  0.5,    0.0,  1.0,  0.0),
    test_cube( 0.5, -3.5,  0.5,    0.0,  1.0,  0.0),
    test_cube( 1.5, -3.5,  0.5,    0.0,  1.0,  0.0),

    test_cube( 0.5,  0.5,  3.5,    0.0,  0.0, -1.0),
    test_cube( 0.5,  0.5, -3.5,    0.0,  0.0, -1.0),
    test_cube( 0.5,  0.5,  3.5,    0.0,  0.0,  1.0),
    test_cube( 0.5,  0.5, -3.5,    0.0,  0.0,  1.0),
    test_cube( 1.5,  0.5, -3.5,    0.0,  0.0,  1.0),

    test_cube( 2.0,  1.5,  0.5,   -1.0, -1.0,  0.0),

    test_cylinder( 0.0,  3.5,  0.0,    0.0, -1.0,  0.0),
    test_cylinder( 0.0, -3.5,  0.0,    0.0, -1.0,  0.0),
    test_cylinder( 0.0,  3.5,  0.0,    0.0,  1.0,  0.0),
    test_cylinder( 0.0, -3.5,  0.0,    0.0,  1.0,  0.0),
    test_cylinder( 1.5, -3.5,  0.0,    0.0,  1.0,  0.0),

    test_cylinder( 3.5,  0.5,  0.0,   -1.0,  0.0,  0.0),
    test_cylinder(-3.5,  0.5,  0.0,   -1.0,  0.0,  0.0),
    test_cylinder( 3.5,  0.5,  0.0,    1.0,  0.0,  0.0),
    test_cylinder(-3.5,  0.5,  0.0,    1.0,  0.0,  0.0),
    test_cylinder(-3.5,  1.5,  0.0,    1.0,  0.0,  0.0),
    test_cylinder(-3.5, -1.5,  0.0,    1.0,  0.0,  0.0),

    test_cylinder( 0.0,  0.5,  3.5,    0.0,  0.0, -1.0),
    test_cylinder( 0.0,  0.5, -3.5,    0.0,  0.0, -1.0),
    test_cylinder( 0.0,  0.5,  3.5,    0.0,  0.0,  1.0),
    test_cylinder( 0.0,  0.5, -3.5,    0.0,  0.0,  1.0),
    test_cylinder( 0.0,  1.5, -3.5,    0.0,  0.0,  1.0),
    test_cylinder( 0.0, -1.5, -3.5,    0.0,  0.0,  1.0).

% ---------------------------------------------------------------------------- %

:- pred test_trans(string, func(trans) = trans,
                io__state, io__state).
:- mode test_trans(in, func(in) = out is det, di, uo) is det.

test_trans(Name, Trans) -->
    { X0 = point(1.0, 0.0, 0.0) },
    { Y0 = point(0.0, 1.0, 0.0) },
    { Z0 = point(0.0, 0.0, 1.0) },
    { R0 = point(1.0, 1.0, 1.0) },
    { T  = Trans(identity) },
    { X1 = point_to_object_space(T, X0), X2 = point_to_world_space(T, X1) },
    { Y1 = point_to_object_space(T, Y0), Y2 = point_to_world_space(T, Y1) },
    { Z1 = point_to_object_space(T, Z0), Z2 = point_to_world_space(T, Z1) },
    { R1 = point_to_object_space(T, R0), R2 = point_to_world_space(T, R1) },

    io__format("\n\nTest %s:\n", [s(Name)]),

    print("\ntransformation:\n"), show_trans(T),

    print("\n\tto_object_space("),show_point(X0),print(") = "),show_point(X1),
    print("\n\tto_world_space( "),show_point(X0),print(") = "),show_point(X2),
    nl,

    print("\n\tto_object_space("),show_point(Y0),print(") = "),show_point(Y1),
    print("\n\tto_world_space( "),show_point(Y0),print(") = "),show_point(Y2),
    nl,

    print("\n\tto_object_space("),show_point(Z0),print(") = "),show_point(Z1),
    print("\n\tto_world_space( "),show_point(Z0),print(") = "),show_point(Z2),
    nl,

    print("\n\tto_object_space("),show_point(R0),print(") = "),show_point(R1),
    print("\n\tto_world_space( "),show_point(R0),print(") = "),show_point(R2),
    nl.

% ---------------------------------------------------------------------------- %

:- pred test_plane(float, float, float, float, float, float,
                io__state, io__state).
:- mode test_plane(in, in, in, in, in, in, di, uo) is det.

test_plane(X, Y, Z, Dx, Dy, Dz) -->
    { P = point(X, Y, Z) },
    { D = point(Dx, Dy, Dz) },
    print("\n\nThe line "), show_point(P), print(" + t"), show_point(D), nl,
    ( if { intersects_plane(identity, P, D, POI, _TC, _N) } then
        print("intersects the plane y = 0 at "), show_point(POI), nl
      else
        print("DOES NOT intersect the plane y = 0\n")
    ).

% ---------------------------------------------------------------------------- %

:- pred test_sphere(float, float, float, float, float, float,
                io__state, io__state).
:- mode test_sphere(in, in, in, in, in, in, di, uo) is det.

test_sphere(X, Y, Z, Dx, Dy, Dz) -->
    { P = point(X, Y, Z) },
    { D = point(Dx, Dy, Dz) },
    print("\n\nThe line "), show_point(P), print(" + t"), show_point(D), nl,
    ( if { intersects_sphere(identity, P, D, POI, TC, _N) } then
        print("intersects the unit origin sphere at "), show_point(POI),
        { TC = surface_coordinates(S, U, V) },
        print("\n\t(face  u  v) = "), show_point(point(float(S), U, V)), nl
      else
        print("DOES NOT intersect the unit origin sphere\n")
    ).

% ---------------------------------------------------------------------------- %

:- pred test_cube(float, float, float, float, float, float,
                io__state, io__state).
:- mode test_cube(in, in, in, in, in, in, di, uo) is det.

test_cube(X, Y, Z, Dx, Dy, Dz) -->
    { P = point(X, Y, Z) },
    { D = point(Dx, Dy, Dz) },
    print("\n\nThe line "), show_point(P), print(" + t"), show_point(D), nl,
    ( if { intersects_cube(identity, P, D, POI, TC, N) } then
        print("intersects the unit origin cube at "), show_point(POI),
        { TC = surface_coordinates(S, U, V) },
        print("\n\t(face  u  v) = "), show_point(point(float(S), U, V)),
        print("\n\tnormal       = "), show_point(N), nl
      else
        print("DOES NOT intersect the unit origin cube\n")
    ).

% ---------------------------------------------------------------------------- %

:- pred test_cylinder(float, float, float, float, float, float,
                io__state, io__state).
:- mode test_cylinder(in, in, in, in, in, in, di, uo) is det.

test_cylinder(X, Y, Z, Dx, Dy, Dz) -->
    { P = point(X, Y, Z) },
    { D = point(Dx, Dy, Dz) },
    print("\n\nThe line "), show_point(P), print(" + t"), show_point(D), nl,
    ( if { intersects_cylinder(identity, P, D, POI, TC, N) } then
        print("intersects the unit origin cylinder at "), show_point(POI),
        { TC = surface_coordinates(S, U, V) },
        print("\n\t(face  u  v) = "), show_point(point(float(S), U, V)),
        print("\n\tnormal       = "), show_point(N), nl
      else
        print("DOES NOT intersect the unit origin cylinder\n")
    ).

% ---------------------------------------------------------------------------- %

:- pred show_point(point, io__state, io__state).
:- mode show_point(in, di, uo) is det.

show_point(point(X, Y, Z)) -->
    io__format("(%4.1f %4.1f %4.1f)", [f(X), f(Y), f(Z)]).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
    
