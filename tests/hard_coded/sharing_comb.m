%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for bug which showed up when --structure-sharing-widening
% was used.
%

:- module sharing_comb.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%---------------------------------------------------------------------------%

:- type render_params
    --->    render_params(
                scene :: scene
            ).

:- type scene
    --->    scene(space_tree).

:- type space_tree
    --->    space_tree(list(space_tree_node)).

:- type space_tree_node
    --->    leaf(space_tree_object).

:- type space_tree_object
    --->    space_tree_object(object).

:- type object
    --->    basic_object(object_id, basic_object).

:- type object_id == int.

:- type basic_object
    --->    sphere(surface).

:- type surface
    --->    constant(surface_properties).

:- type surface_properties
    --->    surface_properties(
                surface_c   :: int,
                surface_kd  :: int,
                surface_ks  :: int,
                surface_n   :: int
            ).

:- type intersection
    --->    intersection(
                object_id   :: object_id,
                surface     :: surface
            ).

:- type tree(T)
    --->    empty
    ;       node(T).

%---------------------------------------------------------------------------%

% BUG: with --structure-sharing-widening set to a number < 7, the sharing
% for find_intersection was inferred to be `bottom' (i.e. no sharing) whereas
% the first and third arguments may share.

:- pred find_intersection(render_params::in, object_id::out, surface::out)
    is semidet.
:- pragma no_inline(find_intersection/3).

find_intersection(RenderParams, Id, Surface) :-
    find_scene_intersection(RenderParams ^ scene, Intersections),
    Intersections = node(Intersection),
    Intersection = intersection(Id, Surface).

:- pred find_scene_intersection(scene::in, tree(intersection)::out) is det.
:- pragma no_inline(find_scene_intersection/2).

find_scene_intersection(scene(Partition), IntersectionResult) :-
    Partition = space_tree(List),
    (
        List = [],
        IntersectionResult = empty
    ;
        List = [Node | _],
        Node = leaf(SpaceTreeObject),
        SpaceTreeObject = space_tree_object(Object),
        Object = basic_object(ObjectId, Sphere),
        Sphere = sphere(Surface),
        Intersection = intersection(ObjectId, Surface),
        IntersectionResult = node(Intersection)
    ).

%---------------------------------------------------------------------------%

main(!IO) :-
    RP0 = render_params(scene(space_tree([leaf(space_tree_object(basic_object(
        11, sphere(constant(surface_properties(22, 33, 44, 55))))))]))),
    copy(RP0, RP),

    io.write_string("RP = ", !IO),
    io.write(RP, !IO),
    io.nl(!IO),

    ( if find_intersection(RP, _, Surface) then
        io.write_string("Surface = ", !IO),
        io.write(Surface, !IO),
        io.nl(!IO),

        % Reconstruction.
        Surface = constant(surface_properties(A, B, C, D)),
        NewSurface = constant(surface_properties(D, C, B, A)),
        io.write_string("NewSurface = ", !IO),
        io.write(NewSurface, !IO),
        io.nl(!IO)
    else
        io.write_string("find_intersection failed!\n", !IO)
    ),

    io.write_string("RP = ", !IO),
    io.write(RP, !IO),
    io.nl(!IO).
