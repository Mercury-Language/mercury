% This is a regression test. Some versions of the compiler reported
% an internal error when compiling with value numbering.

% This code comes from Tom Conway's ray tracer.

:- module vn_float.

:- interface.

:- import_module list, array.

:- type vec3	--->	vec(float, float, float).

:- pred get_planar_coords(list(int), array(vec3), list(vec3), vec3).
:- mode get_planar_coords(in, in, out, out) is det.

:- implementation.

:- import_module require, math, float, int, std_util.

:- type mat3    --->    mat(vec3, vec3, vec3).

get_planar_coords(IndList, VertArr, PlaneList, Norm) :-
    (
        IndList = [Ind1, Ind2, Ind3 | _IndList1]
    ->
        % lookup first 3 vertices and calculate normal
        array__lookup(VertArr, Ind1+1, V1),
        array__lookup(VertArr, Ind2+1, V2),
        array__lookup(VertArr, Ind3+1, V3),
        Norm = unit(cross(V3 - V2, V1 - V2)),
        get_angles_from_z_axis(Norm, Theta, Phi),

        % need to rotate -Theta about Z axis then -Phi about Y axis
        CosZ = math__cos(-Theta),
        SinZ = math__sin(-Theta),
        CosY = math__cos(-Phi),
        SinY = math__sin(-Phi),
        MZ = mat(vec(CosZ,-SinZ,0.0), vec(SinZ,CosZ,0.0), vec(0.0,0.0,1.0)),
        MY = mat(vec(CosY,0.0,SinY), vec(0.0,1.0,0.0), vec(-SinY,0.0,CosY)),
        M =  matmult(MZ, MY),
        move_vertices_to_plane(IndList, VertArr, -V1, M, PlaneList)
    ;
        error("Something strange has happend to the vertex list")
    ).

:- pred get_angles_from_z_axis(vec3, float, float).
:- mode get_angles_from_z_axis(in, out, out) is det.

get_angles_from_z_axis(Vec, Theta, Phi) :-
    (
        Vec = vec(X, Y, Z),
        Vector_radius = mag(Vec),
        XY_radius = mag(vec(X, Y, 0.0)), % magnitude of xy projection
        Pi = math__pi,   % get a useful constant
        (
            Vector_radius = 0.0
        ->
            error("get_angles_from_z_axis: vector should not be zero-length")
        ;
            % check if vector is already on z axis
            (
                XY_radius = 0.0
            ->
                Theta = 0.0,
                Phi = 0.0
            ;
                Xabs = float__abs(X),
                Theta1 = math__asin(Xabs / XY_radius),
                Phi1 = math__asin(XY_radius / Vector_radius),

                % angles have been calculated for the first octant
                % they need to be corrected for the octant they are actually in
                (
                    X =< 0.0
                ->
                    (
                        Y =< 0.0
                    ->
                        Theta = Pi + Theta1
                    ;
                        Theta = Pi - Theta1
                    )
                ;
                    (
                        Y =< 0.0
                    ->
                        Theta = -Theta1
                    ;
                        Theta = Theta1
                    )
                ),
                (
                    Z =< 0.0
                ->
                    Phi = Phi1 + Pi / 2.0
                ;
                    Phi = Phi1
                )
            )
        )
    ).

:- pred move_vertices_to_plane(list(int), array(vec3), vec3, mat3, list(vec3)).
:- mode move_vertices_to_plane(in, in, in, in, out) is det.

:- external(move_vertices_to_plane/5).

:- func unit(vec3) = vec3.

:- external(unit/1).

:- func cross(vec3, vec3) = vec3.

:- external(cross/2).

:- func '-'(vec3, vec3) = vec3.

:- external(('-')/2).

:- func '-'(vec3) = vec3.

:- external(('-')/1).

:- func mag(vec3) = float.

:- external(mag/1).

:- func '*'(vec3, mat3) = vec3.

:- external('*'/2).

:- func matmult(mat3, mat3) = mat3.

:- external(matmult/2).
