%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Some versions of the compiler reported
% an internal error when compiling with value numbering.

% This code comes from Tom Conway's ray tracer.

:- module vn_float.

:- interface.

:- import_module array.
:- import_module list.

:- type vec3
    --->    vec(float, float, float).

:- pred get_planar_coords(list(int)::in, array(vec3)::in,
    list(vec3)::out, vec3::out) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module math.
:- import_module require.
:- import_module std_util.

:- type mat3
    --->    mat(vec3, vec3, vec3).

get_planar_coords(IndList, VertArr, PlaneList, Norm) :-
    ( if IndList = [Ind1, Ind2, Ind3 | _IndList1] then
        % lookup first 3 vertices and calculate normal
        array.lookup(VertArr, Ind1+1, V1),
        array.lookup(VertArr, Ind2+1, V2),
        array.lookup(VertArr, Ind3+1, V3),
        Norm = unit(cross(V3 - V2, V1 - V2)),
        get_angles_from_z_axis(Norm, Theta, Phi),

        % need to rotate -Theta about Z axis then -Phi about Y axis
        CosZ = math.cos(-Theta),
        SinZ = math.sin(-Theta),
        CosY = math.cos(-Phi),
        SinY = math.sin(-Phi),
        MZ = mat(vec(CosZ, -SinZ, 0.0), vec(SinZ, CosZ, 0.0),
            vec(0.0, 0.0, 1.0)),
        MY = mat(vec(CosY, 0.0, SinY), vec(0.0, 1.0, 0.0),
            vec(-SinY, 0.0, CosY)),
        M =  matmult(MZ, MY),
        move_vertices_to_plane(IndList, VertArr, -V1, M, PlaneList)
    else
        error("Something strange has happend to the vertex list")
    ).

:- pred get_angles_from_z_axis(vec3, float, float).
:- mode get_angles_from_z_axis(in, out, out) is det.

get_angles_from_z_axis(Vec, Theta, Phi) :-
    (
        Vec = vec(X, Y, Z),
        Vector_radius = mag(Vec),
        XY_radius = mag(vec(X, Y, 0.0)), % magnitude of xy projection
        Pi = math.pi,   % get a useful constant
        ( if Vector_radius = 0.0 then
            error("get_angles_from_z_axis: vector should not be zero-length")
        else
            % check if vector is already on z axis
            ( if XY_radius = 0.0 then
                Theta = 0.0,
                Phi = 0.0
            else
                Xabs = float.abs(X),
                Theta1 = math.asin(Xabs / XY_radius),
                Phi1 = math.asin(XY_radius / Vector_radius),

                % angles have been calculated for the first octant
                % they need to be corrected for the octant they are actually in
                ( if X =< 0.0 then
                    ( if Y =< 0.0 then
                        Theta = Pi + Theta1
                    else
                        Theta = Pi - Theta1
                    )
                else
                    ( if Y =< 0.0 then
                        Theta = -Theta1
                    else
                        Theta = Theta1
                    )
                ),
                ( if Z =< 0.0 then
                    Phi = Phi1 + Pi / 2.0
                else
                    Phi = Phi1
                )
            )
        )
    ).

:- pred move_vertices_to_plane(list(int)::in, array(vec3)::in,
    vec3::in, mat3::in, list(vec3)::out) is det.

:- pragma no_inline(move_vertices_to_plane/5).

move_vertices_to_plane(_, _, _, _, []).

:- func unit(vec3) = vec3.
:- pragma no_inline(unit/1).

unit(V) = V.

:- func cross(vec3, vec3) = vec3.
:- pragma no_inline(cross/2).

cross(V, _) = V.

:- func '-'(vec3, vec3) = vec3.
:- pragma no_inline(('-')/2).

V - _ = V.

:- func '-'(vec3) = vec3.
:- pragma no_inline(('-')/1).

-V = V.

:- func mag(vec3) = float.
:- pragma no_inline(mag/1).

mag(_) = 42.0.

:- func '*'(vec3, mat3) = vec3.
:- pragma no_inline('*'/2).

V * _ = V.

:- func matmult(mat3, mat3) = mat3.
:- pragma no_inline(matmult/2).

matmult(M, _) = M.
