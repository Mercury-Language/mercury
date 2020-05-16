%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module renderer.
:- interface.

:- import_module eval.
:- import_module space_partition.
:- import_module trans.
:- import_module tree.
:- import_module vector.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.

:- type render_params
    --->    render_params(
                amb             :: color,       % the ambient light
                lights          :: array,       % array(light)
                scene           :: scene,       % the scene to render
                depth           :: int,
                fov             :: real,        % the field of view
                wid             :: int,         % the width, in pixels
                ht              :: int,         % the height, in pixels
                file            :: string
            ).

:- pred render(render_params::in, io::di, io::uo) is det.

:- type unimplemented_object
    --->    unimplemented_object(object).

% mea culpa - this is needed by precompute_lights.m
:- func light_unit_vector(light, position) = vector.

:- pred set_target_parallelism(int::in, io::di, io::uo) is det.

:- pred set_time_filename(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module gml.
:- import_module op.
:- import_module precompute_lights.
:- import_module transform_object.

:- import_module array.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module math.
:- import_module require.
:- import_module string.

render(Params1, !IO) :-
    Wid = Params1 ^ wid,
    Ht = Params1 ^ ht,
    FileName = Params1 ^ file,

    % XXX Note that we first create an array, and then output it.
    % It would be slightly more efficient to just output the values
    % as we go along.

    io.tell(FileName, Result, !IO),
    (
        Result = ok,
        output_stream(Stream, !IO),
        private_builtin.unsafe_type_cast(Stream, BinStream),
        set_binary_output_stream(BinStream, _Old, !IO),
        % Write the ppm header.
        io.format("P6\n", [], !IO),
        io.format("# Merry Mercuryians )O+\n", [], !IO),
        io.format("%d %d\n", [i(Wid), i(Ht)], !IO),
        io.format("255\n", [], !IO),

        % work in progress : pde
        % Pre-compute lighting for "cleanly illuminated" objects
        % first unfold the set of objects that might cast shadows

        % scene_list(Obj, ObjList),

        % now use this to find object/light pairs which are clear of 'em

        % pre_compute_lighting(Obj, ObjList, Params1 ^ lights, NewScene),
        % Params2 = Params1 ^ scene := NewScene,
        Params2 = Params1,

        % Render the image, and time the process.
        gettimeofday(T0, !IO),
        render_rows_par(Params2, Wid, Ht, !IO),
        gettimeofday(T1, !IO),
        io.told(!IO),

        get_time_filename(TimeFileName, !IO),
        ( TimeFileName = "" ->
            true
        ;
            io.open_output(TimeFileName, TimeResult, !IO),
            (
                TimeResult = ok(TimeStream),
                io.write_int(TimeStream, T1 - T0, !IO),
                io.nl(TimeStream, !IO),
                io.close_output(TimeStream, !IO)
            ;
                TimeResult = error(_),
                throw(TimeResult)
            )
        )
    ;
        Result = error(_),
        throw(Result)
    ).

:- pred gettimeofday(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gettimeofday(T::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    struct timeval tv;
    gettimeofday(&tv, NULL);
    T = tv.tv_sec*1000000 + tv.tv_usec;
    IO = IO0;
").

:- pragma foreign_code("C",
"
    MR_Integer   target_parallelism = 0;
    MR_String    time_filename = (MR_String) (MR_Integer) """";
").

:- pragma foreign_proc("C",
    set_target_parallelism(TargetParallelism::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    extern MR_Integer   target_parallelism;

    target_parallelism = TargetParallelism;
    IO = IO0;
").

:- pred get_target_parallelism(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_target_parallelism(TargetParallelism::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    extern MR_Integer   target_parallelism;

    TargetParallelism = target_parallelism;
    /* fprintf(stderr, ""target parallelism %ld\\n"", (long) ** target_parallelism); */
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_time_filename(TimeFileName::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    extern MR_String    time_filename;

    time_filename = TimeFileName;
    IO = IO0;
").

:- pred get_time_filename(string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_time_filename(TimeFileName::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    extern MR_String    time_filename;

    TimeFileName = time_filename;
    /* fprintf(stderr, ""time filename %s\\n"", time_filename); */
    IO = IO0;
").

:- pred render_rows_par(render_params::in, int::in, int::in,
    io::di, io::uo) is det.

render_rows_par(Params, Width, Height, !IO) :-
    get_target_parallelism(TargetParallelism, !IO),
    ( TargetParallelism = 0 ->
        render_rows(Params, 0, Width, Height, [], RevPixels),
        write_rev_bytes(RevPixels, !IO)
    ;
        % Since Height / TargetParallelism may not be an integer, we need to
        % decide which way to round it. The formula below rounds it up,
        % since e.g. dividing 43 rows for 4 CPUs is better done as
        % (11,11,11,10) than as (10,10,10,10,3).
        BunchLines = (Height + TargetParallelism - 1) / TargetParallelism,
        io.open_output("Bunchlines", Result, !IO),
        (
            Result = ok(Stream),
            io.format("bunchlines = %d\n", [i(BunchLines)], !IO),
            io.close_output(Stream, !IO)
        ;
            Result = error(_)
        ),
        render_rows_par_2(Params, BunchLines, 0, Width, Height, !IO)
    ).

:- pred render_rows_par_2(render_params::in, int::in, int::in,
    int::in, int::in, io::di, io::uo) is det.

render_rows_par_2(Params, BunchLines, J, Width, Height, !IO) :-
    ( J >= Height ->
        true
    ;
        % XXX we do a block of rows at a time.  The number of rows
        % needs to be such that there is enough granularity for the
        % processors to be loaded, but not so fine grain that all the
        % sparks end up being scheduled locally and therefore not being
        % worked on in parallel.  This is an example of what happens
        % when the choice of whether to spark locally or globally is
        % done at fork time.  Even if another engine becomes idle it
        % isn't able to get access to locally scheduled sparks.
        %
        % If we set --max-contexts-per-thread to a large number (e.g. 500)
        % and set --small-detstack-size to a small number,
        % then BunchLines = 1 works.

        (
            BunchHeight = int.min(J + BunchLines, Height),
            % io.format("bunch: rows %d to %d\n", [i(J), i(BunchHeight)], !IO),
            render_rows(Params, J, Width, BunchHeight, [], RevPixels),
            write_rev_bytes(RevPixels, !IO)
        &
            render_rows_par_2(Params, BunchLines, J + BunchLines,
                Width, Height, !IO)
        )
    ).

:- pred render_rows(render_params::in, int::in, int::in, int::in,
    list(int)::in, list(int)::out) is det.

render_rows(Params, J, Width, JLimit, !RevPixels) :-
    ( J = JLimit ->
        true
    ;
        render_pixel_loop(Params, 0, J, Width, !RevPixels),
        render_rows(Params, J+1, Width, JLimit, !RevPixels)
    ).

:- pred write_bytes(list(int)::in, io::di, io::uo) is det.

write_bytes([], !IO).
write_bytes([B | Bs], !IO) :-
    write_byte(B, !IO),
    write_bytes(Bs, !IO).

:- pred write_rev_bytes(list(int)::in, io::di, io::uo) is det.

write_rev_bytes([], !IO).
write_rev_bytes([B | Bs], !IO) :-
    write_rev_bytes(Bs, !IO),
    write_byte(B, !IO).

:- pred render_pixel_loop(render_params::in, int::in, int::in, int::in,
    list(int)::in, list(int)::out) is det.

render_pixel_loop(Params, I, J, Width, RevPixels0, RevPixels) :-
    ( I = Width ->
        RevPixels = RevPixels0
    ;
        Pixel = pixel_coords(I, J),
        render_pixel(Params, Pixel, Intensity),

        Intensity = point(R0, G0, B0),
        R = round_to_int(R0 * 255.0),
        G = round_to_int(G0 * 255.0),
        B = round_to_int(B0 * 255.0),
        RevPixels1 = [B, G, R | RevPixels0],

        render_pixel_loop(Params, I + 1, J, Width, RevPixels1, RevPixels)
    ).

% ALGORITHM OVERVIEW
%
% for each pixel in image
%   1. compute direction of ray through pixel
%   2. figure out where the ray first intersects with an object
%   3. compute the object surface coordinates (face, u, v) for
%      the intersection point
%   4. call the surface function to compute the object's texture at that point
%   5. compute the pixel color

:- type pixel_coords
    --->    pixel_coords(
                pixel_i :: int,
                pixel_j :: int
            ).

:- pred render_pixel(render_params::in, pixel_coords::in, color::out) is det.

render_pixel(RenderParams, PixelCoords, Intensity) :-
    RayOrigin = eye_position,
    RayDirection = pixel_direction(RenderParams, PixelCoords),
    fire_ray(RenderParams, no_object, RayOrigin, RayDirection, Intensity).

% Returns an object to ignore for intersections.
% Used to avoid surface acne.
:- func no_object = int.

no_object = 0.

    % Finds the intensity from a ray.
:- pred fire_ray(render_params::in, object_id::in, point::in, vector::in,
    color::out) is det.

fire_ray(RenderParams, IgnoreId, RayOrigin, RayDirection, Intensity) :-
    (
        find_intersection(RenderParams, IgnoreId, RayOrigin, RayDirection,
            HitId, IntersectionPoint, UnitSurfaceNormal,
            SurfaceCoords, SurfaceTextureFunc)
    ->
        compute_texture(SurfaceTextureFunc, SurfaceCoords,
            SurfaceProperties),
        compute_intensity(RenderParams, RayDirection, HitId,
            IntersectionPoint, UnitSurfaceNormal,
            SurfaceProperties, Intensity)
    ;
        % XXX I made this up.
        Intensity = point(0.0, 0.0, 0.0)
    ).

%-----------------------------------------------------------------------------%
%
% step 2: find the intersection point
% step 3: compute the surface coordinates
% (these two are now combined)
%
% figure out where the ray first intersects with an object;
% then compute the object surface coordinates (face, u, v) for
% the intersection point
%

:- interface.

% XXX clean up the module hierarchy.

    % The real is the square of the distance from the ray
    % origin to the intersection point.
:- type intersection_result == tree(pair(real, intersection)).

    % Combine two results into a tree, avoiding allocating memory
    % where possible.
    %
:- func make_tree(intersection_result, intersection_result)
    = intersection_result.

:- pred find_object_intersection(object::in, maybe(transformation)::in,
    point::in, vector::in, intersection_result::out) is det.

:- pred find_object_list_intersection(list(object)::in, point::in, vector::in,
    intersection_result::in, intersection_result::out) is det.

:- func maybe_transformation_to_trans(maybe(transformation)) = trans.

:- implementation.

    % find_intersection(Params, IgnoreId, ViewPoint, ViewVector,
    %   HitId, IntersectionPoint, IntersectionNormalVector,
    %   PixelCoords, SurfaceFunc).
    %
:- pred find_intersection(render_params::in, object_id::in, point::in,
    vector::in, object_id::out, point::out, vector::out,
    surface_coordinates::out, surface::out) is semidet.

find_intersection(RenderParams, IgnoreId, RayOrigin, RayDirection, Id,
        IntersectionPoint, UnitSurfaceNormal, SurfaceCoords, Surface) :-
    find_scene_intersection(RenderParams ^ scene,
        RayOrigin, RayDirection, Intersections),
    choose_closest_intersection(RayOrigin, RayDirection, Intersections,
        IgnoreId, MaybeIntersection),
    MaybeIntersection = yes(_ - Intersection),
%   sanity_check_intersection(RayOrigin, RayDirection, Intersection0,
%       Intersection),
    Intersection = intersection(Id, IntersectionPoint, UnitSurfaceNormal,
        SurfaceCoords, Surface).

% :- pred sanity_check_intersection(position::in, vector::in, intersection::in,
%   intersection::out) is det.
%
% sanity_check_intersection(Position, Direction, Intersection0, Intersection) :-
%   Int = Intersection0^intersection_point,
%   ( dot(Direction, Int - Position) > 0.0 ->
%       Intersection = Intersection0
%   ;
%       throw(intersection_is_behind_vantage_point(Position,
%           Direction, Intersection0))
%   ).
%
% :- type my_excp
%   --->    intersection_is_behind_vantage_point(
%               position,
%               vector,
%               intersection
%           ).

:- pred find_scene_intersection(scene::in, point::in, vector::in,
    intersection_result::out) is det.

find_scene_intersection(scene(Partition, OtherObjects),
        RayOrigin, RayDirection, IntersectionResult) :-
    traverse_space_tree(Partition, RayOrigin, RayDirection,
        IntersectionResult1),
    find_object_list_intersection(OtherObjects, RayOrigin, RayDirection,
        empty, IntersectionResult2),
    IntersectionResult =
        make_tree(IntersectionResult1, IntersectionResult2).

find_object_list_intersection([], _, _, Results, Results).
find_object_list_intersection([Obj | Objs], Point, Vector,
        Results0, Results) :-
    MaybeTrans = no,
    find_object_intersection(Obj, MaybeTrans, Point, Vector, Result),
    find_object_list_intersection(Objs, Point, Vector,
        make_tree(Result, Results0), Results).

    % Choose the closest intersection in the intersection_result
    % which is in front of the viewer.
    %
:- pred choose_closest_intersection(position::in, vector::in,
    intersection_result::in, object_id::in,
    maybe(best_intersection)::out) is det.

choose_closest_intersection(Pos, Dir, tree(Left, Right), IgnoreId, Best) :-
    choose_closest_intersection(Pos, Dir, Left, IgnoreId, LeftBest),
    choose_closest_intersection(Pos, Dir, Right, IgnoreId, RightBest),
    choose_maybe_intersection(LeftBest, RightBest, Best).
choose_closest_intersection(Pos, Dir, node(Intersection), IgnoreId,
        MaybeIntersection) :-
    (
        snd(Intersection)^object_id \= IgnoreId,
        % Check that the intersection is not behind us:
        Point = snd(Intersection)^intersection_point,
        dot(Dir, Point - Pos) > 0.0
    ->
        MaybeIntersection = yes(Intersection)
    ;
        MaybeIntersection = no
    ).
choose_closest_intersection(_Pos, _Dir, empty, _IgnoreId, no).

:- type best_intersection == pair(real, intersection).

:- pred choose_maybe_intersection(maybe(best_intersection)::in,
    maybe(best_intersection)::in, maybe(best_intersection)::out) is det.

choose_maybe_intersection(no, no, no).
choose_maybe_intersection(yes(Best), no, yes(Best)).
choose_maybe_intersection(no, yes(Best), yes(Best)).
choose_maybe_intersection(MaybeBest1, MaybeBest2, MaybeBest) :-
    MaybeBest1 = yes(Best1),
    MaybeBest2 = yes(Best2),
    Best1 = Distance1 - _,
    Best2 = Distance2 - _,
    ( Distance1 < Distance2 ->
        MaybeBest = MaybeBest1
    ;
        MaybeBest = MaybeBest2
    ).

%
% find the points where a ray intersects with an object
%
find_object_intersection(basic_object(Id, Obj, _List), MaybeTransformation,
        RayOrigin, RayDirection, IntersectionResult) :-
    find_basic_object_intersection(Id, Obj, MaybeTransformation,
        RayOrigin, RayDirection, Intersections),
    IntersectionResult =
        intersection_list_to_tree(RayOrigin, Intersections).

find_object_intersection(transform(Object, Transformation1),
        MaybeTransformation, RayOrigin, RayDirection, Intersection) :-
    ( MaybeTransformation = yes(_) ->
        % All transformations should have been collapsed into
        % a single transformation.
        throw(invalid_transformation(Transformation1))
    ;
        true
    ),
    find_object_intersection(Object, yes(Transformation1),
        RayOrigin, RayDirection, Intersection).
find_object_intersection(union(Object1, Object2), MaybeTransformation,
        RayOrigin, RayDirection, Intersection) :-
    % find the points that intersect the surface of Object1
    % and that are NOT inside Object2
    find_surface_and_not_object_intersection(Object1, Object2,
        MaybeTransformation, RayOrigin, RayDirection,
        Intersection1),

    % find the points that intersect the surface of Object2
    % and that are NOT inside Object1
    find_surface_and_not_object_intersection(Object2, Object1,
        MaybeTransformation, RayOrigin, RayDirection,
        Intersection2),

    % Take the union of those.
    Intersection = make_tree(Intersection1, Intersection2).

/***********
    %% XXX old code; this causes wings on the flag in examples/golf.gml
    find_object_intersection(Object1, MaybeTransformation,
            RayOrigin, RayDirection, Intersection1),
    find_object_intersection(Object2, MaybeTransformation,
            RayOrigin, RayDirection, Intersection2),
    Intersection = make_tree(Intersection1, Intersection2).
***********/

find_object_intersection(intersect(Object1, Object2), MaybeTransformation,
        RayOrigin, RayDirection, Intersection) :-
    % find the points that intersect the surface of Object1
    % and that are inside Object2
    find_surface_and_object_intersection(Object1, Object2,
        MaybeTransformation, RayOrigin, RayDirection,
        Intersection1),

    % find the points that intersect the surface of Object2
    % and that are inside Object1
    find_surface_and_object_intersection(Object2, Object1,
        MaybeTransformation, RayOrigin, RayDirection,
        Intersection2),

    % take the union of those
    Intersection = make_tree(Intersection1, Intersection2).

find_object_intersection(difference(Object1, Object2), MaybeTransformation,
        RayOrigin, RayDirection, IntersectionResult) :-
    % find the points that intersect the surface of Object1
    % and that are NOT inside Object2
    find_surface_and_not_object_intersection(Object1, Object2,
        MaybeTransformation, RayOrigin, RayDirection,
        Intersection1),

    % find the points that intersect the surface of Object2
    % and that ARE inside Object1, and then reverse the
    % normals of the intersection points
    find_surface_and_object_intersection(Object2, Object1,
        MaybeTransformation, RayOrigin, RayDirection, Intersection2),
    reverse_normals(Intersection2, Intersection3),

    % take the union of those
    IntersectionResult = tree(Intersection1, Intersection3).

    % Find the points where a ray intersects the surface of one object
    % and is inside another object.
    %
:- pred find_surface_and_object_intersection(object::in, object::in,
    maybe(transformation)::in, point::in, vector::in,
    intersection_result::out) is det.

find_surface_and_object_intersection(Object1, Object2, MaybeTransformation,
        RayOrigin, RayDirection, IntersectionResult) :-
    find_object_intersection(Object1, MaybeTransformation,
       RayOrigin, RayDirection, IntersectionResult0),
    select_intersection_points_in_object(IntersectionResult0,
        Object2, MaybeTransformation, IntersectionResult).

    % Find the points where a ray intersects the surface of one object
    % and is NOT inside the other object.
    %
    % XXX should avoid code duplication
    %
:- pred find_surface_and_not_object_intersection(object::in, object::in,
    maybe(transformation)::in, point::in, vector::in,
    intersection_result::out) is det.

find_surface_and_not_object_intersection(Object1, Object2, MaybeTransformation,
        RayOrigin, RayDirection, IntersectionResult) :-
    find_object_intersection(Object1, MaybeTransformation,
       RayOrigin, RayDirection, IntersectionResult0),
    select_intersection_points_NOT_in_object(IntersectionResult0,
        Object2, MaybeTransformation, IntersectionResult).

:- pred select_intersection_points_in_object(intersection_result::in,
    object::in, maybe(transformation)::in, intersection_result::out) is det.

select_intersection_points_in_object(empty, _, _, empty).
select_intersection_points_in_object(node(I), Object,
        MaybeTransformation, Tree) :-
    I = _Dist - Intersection,
    (
        point_is_in_object(Intersection ^ intersection_point,
            Object, MaybeTransformation)
    ->
        Tree = node(I)
    ;
        Tree = empty
    ).
select_intersection_points_in_object(tree(I1, I2), Object, MT, Tree) :-
    select_intersection_points_in_object(I1, Object, MT, N1),
    select_intersection_points_in_object(I2, Object, MT, N2),
    Tree = make_tree(N1, N2).

    % XXX should avoid code duplication
    %
:- pred select_intersection_points_NOT_in_object(intersection_result::in,
    object::in, maybe(transformation)::in, intersection_result::out) is det.

select_intersection_points_NOT_in_object(empty, _, _, empty).
select_intersection_points_NOT_in_object(node(I), Object,
        MaybeTransformation, Tree) :-
    I = _Dist - Intersection,
    (
        \+ point_is_in_object(Intersection^intersection_point,
            Object, MaybeTransformation)
    ->
        Tree = node(I)
    ;
        Tree = empty
    ).
select_intersection_points_NOT_in_object(tree(I1, I2), Object, MT, Tree) :-
    select_intersection_points_NOT_in_object(I1, Object, MT, N1),
    select_intersection_points_NOT_in_object(I2, Object, MT, N2),
    Tree = make_tree(N1, N2).

:- pred reverse_normals(intersection_result::in, intersection_result::out)
    is det.

reverse_normals(empty, empty).
reverse_normals(node(Dist - Intersection0), node(Dist - Intersection)) :-
    Intersection = Intersection0^surface_normal :=
        - (Intersection0 ^ surface_normal).
reverse_normals(tree(I1, I2), make_tree(N1, N2)) :-
    reverse_normals(I1, N1),
    reverse_normals(I2, N2).

:- pragma inline(make_tree/2).

make_tree(Result1, Result2) = Result :-
    ( Result1 = empty ->
        Result = Result2
    ; Result2 = empty ->
        Result = Result1
    ;
        Result = tree(Result1, Result2)
    ).

:- pred find_basic_object_intersection(object_id::in, basic_object::in,
    maybe(transformation)::in, point::in, vector::in, intersections::out)
    is det.

find_basic_object_intersection(Id, Obj, MaybeTrans,
        RayOrigin, RayDirection, Intersections) :-
    Trans = maybe_transformation_to_trans(MaybeTrans),
    find_basic_object_intersection_2(Id, Obj, Trans,
        RayOrigin, RayDirection, Intersections).

:- pred find_basic_object_intersection_2(object_id::in, basic_object::in,
    trans::in, point::in, vector::in, intersections::out) is det.

find_basic_object_intersection_2(Id, sphere(Surface), Trans, RayOrigin,
        RayDirection, Intersections) :-
    intersects_sphere(Id, Trans, RayOrigin, RayDirection,
        Surface, Intersections).
find_basic_object_intersection_2(Id, Obj, Trans, RayOrigin, RayDirection,
        Intersections) :-
    Obj = plane(Surface),
    intersects_plane(Id, Trans, RayOrigin, RayDirection,
        Surface, Intersections).
find_basic_object_intersection_2(Id, Obj, Trans, RayOrigin, RayDirection,
        Intersections) :-
    Obj = cube(Surface),
    intersects_cube(Id, Trans, RayOrigin, RayDirection,
        Surface, Intersections).
find_basic_object_intersection_2(Id, Obj, Trans, RayOrigin, RayDirection,
        Intersections) :-
    Obj = cylinder(Surface),
    intersects_cylinder(Id, Trans, RayOrigin, RayDirection,
        Surface, Intersections).
find_basic_object_intersection_2(Id, Obj, Trans, RayOrigin, RayDirection,
        Intersections) :-
    Obj = cone(Surface),
    intersects_cone(Id, Trans, RayOrigin, RayDirection,
        Surface, Intersections).

%-----------------------------------------------------------------------------%
%
% step 4:
% given the surface coordinates, use the interpreter to
% call the surface function to compute the object's texture at that point
%

:- pred compute_texture(surface::in, surface_coordinates::in,
    surface_properties::out) is det.

compute_texture(Surface, Coords, Properties) :-
    Surface = surface(Env0, Code),
    Coords = surface_coordinates(Face, U, V),
    Stack0 = [real(V), real(U), int(Face)],
    interpret(Code, Env0, _Env, Stack0, Stack, global_object_counter(1), _),
    ( Stack = [real(N), real(Ks), real(Kd), point(C)] ->
        Properties = surface_properties(C, Kd, Ks, N)
    ;
        throw("surface texture function returned wrong number/type of values")
    ).
compute_texture(Surface, _Coords, Properties) :-
    Surface = constant(Properties).

%-----------------------------------------------------------------------------%
%
% step 5:
% compute the pixel color at a given intersection point,
% given the surface properties at that point
%

:- pred compute_intensity(render_params::in, vector::in, object_id::in,
    point::in, vector::in, surface_properties::in, color::out).

compute_intensity(RenderParams, RayDirection, IgnoreId, IntersectionPoint,
        UnitSurfaceNormal, SurfaceProperties, I) :-
    % see equation (10) in the spec

    I0 = AmbientI + DirectionalOrPositionalI + ReflectedI,
    I = clamp(I0),
    AmbientI = scale(Kd, Ia * C),

    Ia = RenderParams ^ amb,

    C = SurfaceProperties ^ surface_c,
    Kd = SurfaceProperties ^ surface_kd,
    Ks = SurfaceProperties ^ surface_ks,
    PhongExp = SurfaceProperties ^ surface_n,

    Depth = RenderParams ^ depth,
    ( Depth > 0 ->
        NewRenderParams = RenderParams ^ depth := Depth - 1,
        N = UnitSurfaceNormal,
        ReflectionDirection = RayDirection +
            scale(2.0, scale(dot(N, -RayDirection), N)),
        fire_ray(NewRenderParams, IgnoreId, IntersectionPoint,
            ReflectionDirection, Is),
        ReflectedI = scale(Ks, Is * C)
    ;
        ReflectedI = point(0.0, 0.0, 0.0)
    ),

    % This is simple but could be done more
    % efficiently.
    %
    % XXX check both LightContribution1
    % and LightContribution2 for correctness.

    Lights = RenderParams ^ lights,
    LightContribution1 = (
        func(LightVal) = Result1 :-
            Light = light_value_to_light(LightVal),
            Lj = light_unit_vector(Light, IntersectionPoint),
            DotProd = dot(UnitSurfaceNormal, Lj),
            ( DotProd =< 0.0 ->
                Result1 = zero
            ;
                Ij = light_intensity(Light, IntersectionPoint),
                Result1 = scale(DotProd, Ij * C)
            )),

    array.to_list(Lights, LightList0),

    list.filter((pred(LightVal::in) is semidet :-
            position_not_in_shadow(RenderParams, IgnoreId,
                    IntersectionPoint, LightVal)
        ), LightList0, LightList),

    ContributionList1 = list.map(LightContribution1, LightList),

        % do we need unit?
    LightContribution2 = (
        func(LightVal) = Result :-
            % compute the direction to the light
            Light = light_value_to_light(LightVal),
            Lj = light_unit_vector(Light, IntersectionPoint),

            % check that light is on the right side of the surface
            SanityCheck = dot(UnitSurfaceNormal, Lj),
            ( SanityCheck =< 0.0 ->
                Result = zero
            ;
                Hj = unit(Lj - unit(RayDirection)),
                ReflectionBase = dot(UnitSurfaceNormal, Hj),
                ( ReflectionBase =< 0.0 ->
                    Result = zero
                ;
                    Ij = light_intensity(Light, IntersectionPoint),
                    Result = scale(math.pow(ReflectionBase, PhongExp), Ij * C)
                )
            )
        ),

    ContributionList2 = list.map(LightContribution2, LightList),

    SumContributions1 = list.foldl(+, ContributionList1,
        point(0.0, 0.0, 0.0)),
    SumContributions2 = list.foldl(+, ContributionList2,
        point(0.0, 0.0, 0.0)),

    DirectionalOrPositionalI = scale(Kd, SumContributions1) +
        scale(Ks, SumContributions2).

    % Filter out all lights which are blocked by some
    % object, not including the object on whose surface Position is.
    %
:- pred position_not_in_shadow(render_params::in, object_id::in, position::in,
    value::in) is semidet.

position_not_in_shadow(RenderParams, IgnoreId, Position, LightVal) :-
    Light = light_value_to_light(LightVal),
    Lj = light_unit_vector(Light, Position),
    not (some [Intersection] (
        find_intersection(RenderParams, IgnoreId, Position, Lj, _Id,
            Intersection, _, _, _),
        light_is_behind_point(Position, Light, Intersection)
    )).

    % light_is_behind_point(VantagePoint, Light, Intersection) is true
    % if Intersection is in between the Light and the Vantage.
    %
:- pred light_is_behind_point(position::in, light::in, position::in)
    is semidet.

light_is_behind_point(Vantage, directional(Dir, _), Point) :-
    % Ensure that the Point is in the same direction as the light
    % source.
    dot(Vantage - Point, Dir) > 0.0.
light_is_behind_point(Vantage, pointlight(LightPos, _), Point) :-
    % Ensure that the Point is between the Vantage and the Light.
    dot(LightPos - Point, Vantage - Point) < 0.0.
light_is_behind_point(Vantage, spotlight(LightPos, _, _, _, _), Point) :-
    % Ensure that the Point is between the Vantage and the Light.
    dot(LightPos - Point, Vantage - Point) < 0.0.

:- func light_value_to_light(value) = light.

light_value_to_light(Val) = Light :-
    ( Val = light(Light0) ->
        Light = Light0
    ;
        throw(program_error("incorrect type for element of light array"))
    ).

    % Return the intensity of this light at the given position.
    %
:- func light_intensity(light, position) = color.

light_intensity(directional(_Dir, Intensity), _Point) = Intensity.
light_intensity(pointlight(Pos, Intensity), Point) =
    scale(distance_attenuation(Pos, Point), Intensity).
light_intensity(spotlight(Pos, At, Intensity, Cutoff, Exp), Point) =
    scale(DistanceAttenuation * AngleAttenuation, Intensity) :-

    DistanceAttenuation = distance_attenuation(Pos, Point),
    AngleCos = dot(unit(At - Pos), unit(Point - Pos)),
    ( AngleCos > math.cos(radians(Cutoff)) ->
        AngleAttenuation = math.pow(AngleCos, Exp)
    ;
        AngleAttenuation = 0.0
    ).

:- func distance_attenuation(position, position) = real.

distance_attenuation(Pos1, Pos2) = 100.0 / (99.0 + D2) :-
    D2 = mag2(Pos2 - Pos1).

    % Return Lj -- the direction to the light source.
    % For directional lights, the unit vector is essentially constant.
    %
light_unit_vector(directional(Dir, _Intensity), _Point) = unit(-Dir).
light_unit_vector(pointlight(Pos, _Intensity), Point) = unit(Pos - Point).
light_unit_vector(spotlight(Pos, _At, _Intensity, _Cutoff, _Exp), Point) =
    unit(Pos - Point).

:- func *(color, color) = color. % XYZ is really RGB
point(AX, AY, AZ) * point(BX, BY, BZ) = point(AX * BX, AY * BY, AZ * BZ).

:- func clamp(color) = color.
clamp(point(R0, G0, B0)) = point(R, G, B) :-
    R = op_clampf(R0),
    G = op_clampf(G0),
    B = op_clampf(B0).

%-----------------------------------------------------------------------------%
%
% misc stuff that may be useful
%

:- func eye_position = point.
eye_position = point(0.0, 0.0, -1.0).

    % given the fov (in degrees),
    % return the image width in world space
:- func image_width(degrees) = real.
image_width(Fov) = 2.0 * image_halfwidth(Fov).

    % given the fov (in degrees),
    % return half the image width in world space
:- func image_halfwidth(degrees) = real.
image_halfwidth(Fov) = tan(0.5 * radians(Fov)).

    % return half the image height in world space
:- func image_halfheight(real, int, int) = real.
image_halfheight(HalfWidth, WidthInPixels, HeightInPixels) =
    HalfWidth * float(HeightInPixels) / float(WidthInPixels).

:- func image_topleft(real, int, int) = point.
image_topleft(Fov, WidthInPixels, HeightInPixels) = TopLeft :-
    HalfWidth = image_halfwidth(Fov),
    HalfHeight = image_halfheight(HalfWidth, WidthInPixels, HeightInPixels),
    TopLeft = point(-HalfWidth, HalfHeight, 0.0).

    % return the ray direction for the specified pixel
:- func pixel_direction(render_params, pixel_coords) = vector.
pixel_direction(RenderParams, Pixel) = Direction :-
    Fov = RenderParams ^ fov,
    WidthInPixels = RenderParams ^ wid,
    HeightInPixels = RenderParams ^ ht,
    I = Pixel ^ pixel_i,
    J = Pixel ^ pixel_j,
    Direction = pixel_direction_2(Fov, WidthInPixels, HeightInPixels, I, J).

    % return the direction of the Jth pixel in the Ith row
    %
:- func pixel_direction_2(real, int, int, int, int) = vector.

pixel_direction_2(Fov, WidthInPixels, HeightInPixels, I, J) = Direction :-
    TopLeft = image_topleft(Fov, WidthInPixels, HeightInPixels),
    TopLeft = point(TopLeftX, TopLeftY, _),
    Delta = image_width(Fov) / float(WidthInPixels),
    XV = TopLeftX + (float(I) + 0.5) * Delta,
    YV = TopLeftY - (float(J) + 0.5) * Delta,
    ZV = 1.0,
    Direction = point(XV, YV, ZV).

:- pred point_is_in_object(point::in, object::in, maybe(transformation)::in)
    is semidet.

point_is_in_object(Point, basic_object(_Id, Obj, _List),
        MaybeTransformation) :-
    point_is_in_basic_object(Point, Obj, MaybeTransformation).
point_is_in_object(Point, transform(Obj, Transformation1),
        MaybeTransformation) :-
    ( MaybeTransformation = yes(_) ->
        % All transformations should have been collapsed into
        % a single transformation.
        throw(invalid_transformation(Transformation1))
    ;
        true
    ),
    point_is_in_object(Point, Obj, yes(Transformation1)).
point_is_in_object(Point, union(Obj1, Obj2), MaybeTransformation) :-
    (
        point_is_in_object(Point, Obj1, MaybeTransformation)
    ;
        point_is_in_object(Point, Obj2, MaybeTransformation)
    ).
point_is_in_object(Point, intersect(Obj1, Obj2), MaybeTransformation) :-
    point_is_in_object(Point, Obj1, MaybeTransformation),
    point_is_in_object(Point, Obj2, MaybeTransformation).
point_is_in_object(Point, difference(Obj1, Obj2), MaybeTransformation) :-
    point_is_in_object(Point, Obj1, MaybeTransformation),
    \+ point_is_in_object(Point, Obj2, MaybeTransformation).

:- pred point_is_in_basic_object(point::in, basic_object::in,
    maybe(transformation)::in) is semidet.

point_is_in_basic_object(Point, Obj, MaybeTrans) :-
    Trans = maybe_transformation_to_trans(MaybeTrans),
    point_is_in_basic_object_2(Point, Obj, Trans).

maybe_transformation_to_trans(MaybeTrans) = Trans :-
    ( MaybeTrans = yes(Trans0) ->
        ( Trans0 = matrix(Trans1) ->
            Trans = Trans1
        ;
            % All transformations should now be matrices
            % because push_transformations was applied to the object.
            throw(invalid_transformation(Trans0))
        )
    ;
        Trans = identity
    ).

:- pred point_is_in_basic_object_2(point::in, basic_object::in, trans::in)
    is semidet.

point_is_in_basic_object_2(Point, sphere(_Surface), Trans) :-
    inside_sphere(Point, Trans).
point_is_in_basic_object_2(Point, plane(_Surface), Trans) :-
    inside_plane(Point, Trans).
point_is_in_basic_object_2(Point, cube(_Surface), Trans) :-
    inside_cube(Point, Trans).
point_is_in_basic_object_2(Point, cylinder(_Surface), Trans) :-
    inside_cylinder(Point, Trans).
point_is_in_basic_object_2(Point, cone(_Surface), Trans) :-
    inside_cone(Point, Trans).

%-----------------------------------------------------------------------------%

:- func intersection_list_to_tree(point, intersections) = intersection_result.

intersection_list_to_tree(_, []) = empty.
intersection_list_to_tree(RayOrigin, [Int | Ints]) =
        tree(node(DistanceSquared - Int), Tree0) :-
    Tree0 = intersection_list_to_tree(RayOrigin, Ints),
    DistanceSquared = distance_squared(RayOrigin, Int ^ intersection_point).

%-----------------------------------------------------------------------------%
