:- module scene.

% Read in the scene description file
% Read in OFF objects and transform
% Output 3d scene description

:- interface.

:- import_module io, gfx.

% the scene returned to the rest of the program is a list(polytri).  Each
% polytri gives all the triangles from a particular face of the
% polyhedron.
% They are grouped like this to take advantage of the fact that they are all on
% the same plane to improve efficiency later on.

%  get_scene(Filename, Scene) read scene description from filename and output as
%  scene
:- pred get_scene(string, scene, io__state, io__state).
:- mode get_scene(in, out, di, uo) is det.

:- implementation.

:- import_module string, char, float, int, array, list, assoc_list.
:- import_module require, math, std_util, map.
:- import_module debug, table, split_line, off, vec3, mat3.

% object with normal vectors for vertices and faces
%:- type norm_object --->
%        obj(
%                table(norm_vertex),  % array of vertices
%                list(norm_face)      % list of faces
%            ).

:- type norm_vertex --->
        vert(
                vec3,   % vertex position vector
                vec3    % vertex normal vector
            ).

:- type norm_face --->
        face(
                list(face_vertex),  % list of vertex indices
                colour,     % intrinsic colour of face
                vec3        % normal vector of face
            ).

% description of a vertex for a particular polygon
:- type face_vertex --->
        vert(
                int,    % index into vertex list
                vec3,   % world co-ords of vertex
                vec3    % plane co-ords of vertex for this polygon
            ).

get_scene(Filename, Scene) -->
    (
        (
            { Filename = "-" }  % "-" = read from stdin
        ->
            { true }
        ;
            io__see(Filename, Result),
                (
                    { Result = error(ErrorCode) },
                    { io__error_message(ErrorCode, ErrorString) },
                    { error(ErrorString) }
                ;
                    { Result = ok }
                )
        ),
        read_scene_file(Scene),
        (
            { Filename = "-" }
        ->
            { true }
        ;
            io__seen
        )
    ).

% Read the scene description file and return a Scene
:- pred read_scene_file(scene, io__state, io__state).
:- mode read_scene_file(out, di, uo) is det.

read_scene_file(Scene) -->
    (
        io__read_line(Result),
            (
                { Result = eof },   % end of file
%		{ dump("Finished reading scene file\n", []) },
                { Scene = [] }
            ;
                { Result = error(ErrorCode) },
%		{ dump("Error in read_scene_file\n", []) },
                { io__error_message(ErrorCode, ErrorString) },
                { error(ErrorString) }
            ;
                { Result = ok(Line) },
                triangles_from_scene_line(Line, Scene0),
                read_scene_file(Scene1),
                { list__append(Scene0, Scene1, Scene) }
            )
    ).

% process a line of the scene description file
% return a list of triangles
:- pred triangles_from_scene_line(list(char), scene, io__state, io__state).
:- mode triangles_from_scene_line(in, out, di, uo) is det.

triangles_from_scene_line(Line, Scene) -->
    (
        { split_line(Line, Params) },               % split line into a list of parameters
            (
                { Params = [Filename | Params0] }
            ->
                read_off(Filename, Object),          % get normalised polyhedron
                { Params1 = Params0 }
            ;
                { error("Scene file format incorrect") }
            ),
        { get_trans_vec(Params1, Trans, Params2) },
        { get_rot_matrix(Params2, Rot, Params3) },
        { get_scale_matrix(Params3, Scale, _Params4) },
        { transform_object(Object, Rot, Scale, Trans, Object1) },
        { calculate_normals(Object1, NormVertArr, NormFaceList) },
        { triangles_from_object(0, NormVertArr, NormFaceList, Scene) }
    ).

% Get the translation vector from the first three parameters in the list.
:- pred get_trans_vec(list(string), vec3, list(string)).
:- mode get_trans_vec(in, out, out) is det.

get_trans_vec(Pin, V, Pout) :-
    (
        Pin = [S1, S2, S3 | P1]
    ->
        (
            string__to_float(S1, X),
            string__to_float(S2, Y),
            string__to_float(S3, Z)
        ->
            V = vec(X, Y, Z),
            Pout = P1
        ;
            error("Scene file error: translation parameter format incorrect")
        )
    ;
        error("Scene file error: not enough paramters for translation")
    ).

% Get the rotation matrix from the first three parameters in the list
:- pred get_rot_matrix(list(string), mat3, list(string)).
:- mode get_rot_matrix(in, out, out) is det.

get_rot_matrix(Pin, M, Pout) :-
    (
        Pin = [S1, S2, S3 | P1]
    ->
        (
            string__to_float(S1, Xd),
            string__to_float(S2, Yd),
            string__to_float(S3, Zd)
        ->
            deg_to_rad(Xd, X),
            deg_to_rad(Yd, Y),
            deg_to_rad(Zd, Z),
            CosX = math__cos(X),
            SinX = math__sin(X), 
            CosY = math__cos(Y),
            SinY = math__sin(Y),
            CosZ = math__cos(Z),
            SinZ = math__sin(Z),
            MX = mat(vec(1.0,0.0,0.0), vec(0.0,CosX,-SinX), vec(0.0,SinX,CosX)),
            MY = mat(vec(CosY,0.0,SinY), vec(0.0,1.0,0.0), vec(-SinY,0.0,CosY)),
            MZ = mat(vec(CosZ,-SinZ,0.0), vec(SinZ,CosZ,0.0), vec(0.0,0.0,1.0)),
            M = MX * MY * MZ,
            Pout = P1
        ;
            error("Scene file error: rotation parameter format incorrect")
        )
    ;
        M = mat_identity,   % default is no rotation
        Pout = []           % can be no more parameters
    ).

% Get the scale matrix from the first three parameters in the list
:- pred get_scale_matrix(list(string), mat3, list(string)).
:- mode get_scale_matrix(in, out, out) is det.

get_scale_matrix(Pin, M, Pout) :-
    (
        Pin = [S1, S2, S3 | P1]
    ->
        (
            string__to_float(S1, X),
            string__to_float(S2, Y),
            string__to_float(S3, Z)
        ->
            M = mat(vec(X,0.0,0.0), vec(0.0,Y,0.0), vec(0.0,0.0,Z)),
            Pout = P1
        ;
            error("Scene file error: scale parameter format incorrect")
        )
    ;
        Pin = [S1 | P1]
    ->
        (
            string__to_float(S1, X)
        -> 
            M = scale(X, mat_identity),   % use same scale factor for all axes
            Pout = P1
        ;
            error("Scene file error: scale parameter format incorrect")
        )
    ;
        M = mat_identity,   % default is no scaling
        Pout = []
    ).


% transform_object(Object, Rot, Scale, Trans, Object1)
% Rotate, scale and translate the object
:- pred transform_object(off_object, mat3, mat3, vec3, off_object).
:- mode transform_object(in, in, in, in, out) is det.

transform_object(obj(Vertices, Faces), Rot, Scale, Trans, obj(Vertices1, Faces)) :-
        RotScale = Rot * Scale,     % matrix multiplication
        transform_vertlist(Vertices, RotScale, Trans, Vertices1).
        
% transform the list of vertices
:- pred transform_vertlist(list(vec3), mat3, vec3, list(vec3)).
:- mode transform_vertlist(in, in, in, out) is det.

transform_vertlist(VLin, RotScale, Trans, VLout) :-
    (
        VLin = [],
        VLout = []
    ;
        VLin = [V | VL1],
        Vtrans = V * RotScale + Trans,
        transform_vertlist(VL1, RotScale, Trans, VL2),
        VLout = [Vtrans | VL2]
    ).

% convert off_object to ``normal'' object which includes normal vectors for vertices
% and faces.
:- pred calculate_normals(off_object, table(norm_vertex), list(norm_face)).
:- mode calculate_normals(in, out, out) is det.

calculate_normals(OffObj, NormVertArray, NormFaceList) :-
    (
        OffObj = obj(VertList, FaceList),

        %convert to an array so we can do indexed lookups more easily
        table__mktable(VertList, VertArray),

        calculate_face_normals(FaceList, VertArray, NormFaceList),

	map__init(VertFaces0),
	precompute_vertex_faces(0, NormFaceList, VertFaces0, VertFaces),

        table__mktable(NormFaceList, NormFaceArray),

	map__values(VertFaces, VertFacesList),
	table__mktable(VertFacesList, VertFacesArray),

        calculate_vert_normals(VertList, 0, VertFacesArray,
		NormFaceArray, NormVertList),

        table__mktable(NormVertList, NormVertArray)
    ).

:- pred precompute_vertex_faces(int, list(norm_face),
		map(int, list(int)), map(int, list(int))).
:- mode precompute_vertex_faces(in, in, in, out) is det.

precompute_vertex_faces(_N, [], Map, Map).
precompute_vertex_faces(N, [Face|Faces], Map0, Map) :-
	Face = face(Vertices, _, _),
%	dump("inserting stuff for vertex %d\n", [i(N)]),
	insert_vertices(N, Vertices, Map0, Map1),
	precompute_vertex_faces(N+1, Faces, Map1, Map).

:- pred insert_vertices(int, list(face_vertex),
		map(int, list(int)), map(int, list(int))).
:- mode insert_vertices(in, in, in, out) is det.

insert_vertices(_N, [], Map, Map).
insert_vertices(N, [FV|Vs], Map0, Map) :-
	FV = vert(V, _, _),
	(
		map__search(Map0, V, Fs0)
	->
		Fs1 = Fs0
	;
		Fs1 = []
	),
	map__set(Map0, V, [N|Fs1], Map1),
	insert_vertices(N, Vs, Map1, Map).

% convert degress to radians
:- pred deg_to_rad(float, float).
:- mode deg_to_rad(in, out) is det.
% :- mode deg_to_rad(out, in) is det. % don't need this mode

deg_to_rad(Deg, Rad) :- 
    Pi = math__pi,
    Rad = Deg / 180.0 * Pi.

% Caculate normals for each face.  The OOGL standard states that the normal
% should point towards the direction from which the points appear in
% anti-clockwise order.  I've noticed many of the examples don't follow this,
% but haven't had time to do anything about it.
% Procedure: 1. Place a copy of the first element on the end of the index list
%            2. Obtain 2d co-ords in the plane of the face, and the WC Normal
%               vector
%            3. Calculate area of polygon
%            4. If area < 0 vertices were the wrong way around -- need to
%               reverse
%            5. Now that we know the walking order of vertices in the plane, if
%               the vertex we used to calculate the normal is concave, normal
%               direction needs to be reversed.
%            6. 
%            
:- pred calculate_face_normals(list(off_face), table(vec3), list(norm_face)).
:- mode calculate_face_normals(in, in, out) is det.

calculate_face_normals(FaceList, VertArray, NormFaceList) :-
    (
        FaceList = [],
        NormFaceList = []
    ;
        FaceList = [Face | FaceList1],
        Face = face(_, IndexList, Colour),
        get_planar_coords(IndexList, VertArray, PlanePoly0, Norm),
            (
                PlanePoly0 = [],
                error("Face has no vertices")
            ;
                % get_area requires the first vertex to be repeated at the end
                % of the list
                PlanePoly0 = [P | _],
                list__append(PlanePoly0, [P], PlanePolyA),
                get_area(PlanePolyA, Area),
                (
                    Area < 0.0
                ->
                    list__reverse(PlanePoly0, PlanePoly1),
                    list__reverse(IndexList, IndexList1)
                ;
                    
                    PlanePoly1 = PlanePoly0,
                    IndexList1 = IndexList
                ),
                (
                    PlanePoly1 = [P1, P2, P3 | _]
                ->
                    vec(_, _, Z) = cross(P3 - P2, P1 - P2),
                    (
                        Z < 0.0
                    ->
                        Norm1 = -Norm       % if vertex we used to get normal was
                                            % concave, need to reverse direction
                    ;
                        Norm1 = Norm
                    ),
                    create_face_vert_list(PlanePoly1, IndexList1, VertArray, FaceVertList),
                    NormFace = face(FaceVertList, Colour, Norm1),
                    calculate_face_normals(FaceList1, VertArray, NormFaceList1),
                    NormFaceList = [NormFace | NormFaceList1]
                ;
                    error("Plane polygon does not have enough vertices!!")
                )
            )
    ).

% get planar coords and WC normal vector for polygon
% Note: we don't yet know which vertices are concave so this normal may have the
% wrong sign.  Need to check this later.
:- pred get_planar_coords(list(int), table(vec3), list(vec3), vec3).
:- mode get_planar_coords(in, in, out, out) is det.

get_planar_coords(IndList, VertArr, PlaneList, Norm) :-
    (
        IndList = [Ind1, Ind2, Ind3 | _IndList1]
    ->
        % lookup first 3 vertices and calculate normal
        table__lookup(VertArr, Ind1, V1),
        table__lookup(VertArr, Ind2, V2),
        table__lookup(VertArr, Ind3, V3),
        Norm = unit(cross(V3 - V2, V1 - V2)),
        get_angles_from_z_axis(Norm, Theta, Phi),

        % need to rotate -Theta about Z axis then -Phi about Y axis
        CosZ = math__cos(-Theta),
        SinZ = math__sin(-Theta),
        CosY = math__cos(-Phi),
        SinY = math__sin(-Phi),
        MZ = mat(vec(CosZ,-SinZ,0.0), vec(SinZ,CosZ,0.0), vec(0.0,0.0,1.0)),
        MY = mat(vec(CosY,0.0,SinY), vec(0.0,1.0,0.0), vec(-SinY,0.0,CosY)),
        M =  MZ * MY,
        move_vertices_to_plane(IndList, VertArr, -V1, M, PlaneList)
    ;
        error("Something strange has happend to the vertex list")
    ).

% calculate angles of vector from z axis
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


% move_vertices_to_plane(IndList, VertArr, T, R, PlaneList)
% translate by T and rotate by R to get vertices into the xy plane
:- pred move_vertices_to_plane(list(int), table(vec3), vec3, mat3, list(vec3)).
:- mode move_vertices_to_plane(in, in, in, in, out) is det.

move_vertices_to_plane(IndList, VertArr, T, R, PlaneList) :-
    (
        IndList = [],
        PlaneList = []
    ;
        IndList = [Ind | IndList1],
        table__lookup(VertArr, Ind, V1),
        vec(X, Y, _Z) = (V1 + T) * R,
        V2 = vec(X, Y, 0.0),    % remove any erroneous Z component
        move_vertices_to_plane(IndList1, VertArr, T, R, PlaneList1),
        PlaneList = [V2 | PlaneList1]
    ).

% calculate area of a polygon in the xy plane
% area is negative if vertices are clockwise and positive if they are
% anticlockwise.  This actually calculates double the area, but that's ok for
% what we need.
:- pred get_area(list(vec3), float).
:- mode get_area(in, out) is det.

get_area(PlaneList, Area) :-
    (
        PlaneList = [vec(X1, Y1, _), vec(X2, Y2, _) | PlaneList1]
    ->
        get_area([vec(X2, Y2, 0.0) | PlaneList1], Area1),
        Area = Area1 + X1 * Y2 - X2 * Y1
    ;
        Area = 0.0
    ).

% combine all these lists into a list of face_vertex that we can stick inside
% a norm_face
:- pred create_face_vert_list(list(vec3), list(int), table(vec3), list(face_vertex)).
:- mode create_face_vert_list(in, in, in, out) is det.

create_face_vert_list(PlaneList, IndList, VertArr, FaceVertList) :-
    (
        IndList = [],
        FaceVertList = []
    ;
        IndList = [Ind | IndList1],
            (
                PlaneList = [PVert | PlaneList1]
            ->
                table__lookup(VertArr, Ind, WCVert),
                FaceVert = vert(Ind, WCVert, PVert),
                create_face_vert_list(PlaneList1, IndList1, VertArr, FaceVertList1),
                FaceVertList = [FaceVert | FaceVertList1]
            ;
                error("create_face_vert_list: lists are not long enough")
            )
    ).
    
% calculate the ``normal'' vector at each vertex
% this is the average of the normal vectors for each face that the vertex
% belongs to.
:- pred calculate_vert_normals(list(vec3), int, table(list(int)),
		table(norm_face), list(norm_vertex)).
:- mode calculate_vert_normals(in, in, in, in, out) is det.

calculate_vert_normals(VertList, VertNum, VertFaceArray, NormFaceArray,
	NormVertList) :-
    (
        VertList = [],
        NormVertList = []
    ;
        VertList = [V1 | VertList1], 
        %NormVertList = []  %% dmo
%	dump("calculated vertex normal %d\n", [i(VertNum)]),
	table__lookup(VertFaceArray, VertNum, VertFaces),
        calc_vert_normal(VertNum, VertFaces, NormFaceArray, NormVec),
        calculate_vert_normals(VertList1, VertNum+1, VertFaceArray,
		NormFaceArray, NormVertList1),
        NormVert = vert(V1, unit(NormVec)),
        NormVertList = [NormVert | NormVertList1]
    ).

% find normal vector for a particular vertex, given by its number
% note: NormVert returned here is not necessarily a unit vector
:- pred calc_vert_normal(int, list(int), table(norm_face), vec3).
:- mode calc_vert_normal(in, in, in, out) is det.

calc_vert_normal(VertNum, FaceList, NormFaceArray, NormVert) :-
    (
        FaceList = [],
        NormVert = vec(0.0, 0.0, 0.0)
    ;
        FaceList = [FaceId | FaceList1],
	table__lookup(NormFaceArray, FaceId, Face),
        Face = face(_, _, NormFace),
	calc_vert_normal(VertNum, FaceList1, NormFaceArray, NormVert1),
	NormVert = NormFace + NormVert1
    ).
        

% break up an object into a part of the scene
% (scene == list(polytri)
:- pred triangles_from_object(int, table(norm_vertex), list(norm_face), scene).
:- mode triangles_from_object(in, in, in, out) is det.

triangles_from_object(N, VertArray, FaceList, Scene) :-
    (
        FaceList = [],
	% dump("finished triangulation\n", []),
        Scene = []
    ;
        FaceList = [Face | FaceList1],
%	dump("Splitting face %d\n", [i(N)]),
        triangles_from_face(Face, VertArray, PolyTri),
        triangles_from_object(N+1, VertArray, FaceList1, Scene1),
        Scene = [PolyTri | Scene1]
    ).

% break up a face into a list of triangles
% Method:
%   1.  We already have the polygon rotated to the XY plane and in a
%   counter-clockwise walking order when viewed from the positve Z side of the
%   plane.
%   2.  Classify each vertex as convex or concave using the cross product.
%   3.  For each convex vertex:
%       -   Check each concave vertex and see if it lies in a triangle make by
%           the convex vertex and its two adjacent verticies,
%       -   If none are found, clip this triangle off the polygon and start
%           again at step 2.  Otherwise go on to the next convex vertex.
:- pred triangles_from_face(norm_face, table(norm_vertex), polytri).
:- mode triangles_from_face(in, in, out) is det.

triangles_from_face(Face, VertArr, PolyTri) :-
    (
        Face = face(VertList, Colour, NormVec),
        break_poly(VertList, VertArr, Triangles),
        PolyTri = polytri(Triangles, NormVec, Colour)
    ).


:- pred break_poly(list(face_vertex), table(norm_vertex), list(triangle)).
:- mode break_poly(in, in, out) is det.

break_poly([], _VertArr, _Triangles) :-
	error("no vertices in polygon").
break_poly([V|Vs], VertArr, Triangles) :-
	break_poly(V, Vs, VertArr, Triangles).

:- pred break_poly(face_vertex, list(face_vertex), table(norm_vertex),
		list(triangle)).
:- mode break_poly(in, in, in, out) is det.

break_poly(_Vert, [], _VertArr, _Triangles) :-
	error("only one vertex in polygon!").
break_poly(Vert0, [Vert1|Verts], VertArr, Triangles) :-
	list__length(Verts, Len),
	break_poly(Vert0, Vert1, Verts, VertArr, Len+2, Triangles).

:- pred break_poly(face_vertex, face_vertex, list(face_vertex),
		table(norm_vertex), int, list(triangle)).
:- mode break_poly(in, in, in, in, in, out) is det.

break_poly(_Vert0, _Vert1, [], _VertArr, _, _Triangles) :-
	error("only two vertices in polygon!").
break_poly(Vert0, Vert1, [Vert2|Verts], VertArr, Tries, Triangles) :-
	Vert0 = vert(I0, W0, V0),
	Vert1 = vert(I1, W1, V1),
	Vert2 = vert(I2, W2, V2),
	vec(_, _, Z) = cross(V1 - V0, V1 - V2),
	( Z < 0.0 ->
		% convex triangle
		list__length(Verts, _Len),
%		dump("%d vertices remaining.\n", [i(Len)]),
		(
			% are any points inside?
			list__member(Vert, Verts),
			Vert = vert(_, _, V),
			point_in_triangle(V0, V1, V2, V)
		->
			% there was a point inside
%			dump("vertex inside triangle....\n", []),
			list__append(Verts, [Vert0], Verts1),
			break_poly(Vert1, Vert2, Verts1, VertArr, Tries, Triangles)
		;
			% This triangle is okay
			table__lookup(VertArr, I0, T0),
			T0 = vert(_, N0),
			table__lookup(VertArr, I1, T1),
			T1 = vert(_, N1),
			table__lookup(VertArr, I2, T2),
			T2 = vert(_, N2),
			Triangle = tri(W0, W1, W2, N0, N1, N2),
			(
				Verts = [],
				Triangles1 = []
			;
				Verts = [_|_],
				break_poly(Vert0, Vert2, Verts, VertArr, Tries-1, Triangles1)
			),
			Triangles = [Triangle|Triangles1]
		)
	;
		% concave
%		dump("concave poly. keep trying....\n", []),
		( Tries < 0 ->
			list__reverse([Vert0, Vert1, Vert2|Verts], Verts1),
			break_poly(Verts1, VertArr, Triangles)
		;
			list__append(Verts, [Vert0], Verts1),
			break_poly(Vert1, Vert2, Verts1, VertArr, Tries-1, Triangles)
		)
	).

