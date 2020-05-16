%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module precompute_lights.

% add information to the scene structure indicating which object/light pairs
% are guaranteed not to need shadow calculations.  Blame pde & rejj.

:- interface.

:- import_module eval.
:- import_module space_partition.
:- import_module vector.

:- import_module list.

:- type bounding_sphere
    --->    bsphere(centre::point, radius::real).

% scene_list : 	take a composite scene, return a list of the bounding
%		boxes for objects inside it.

:- pred scene_list(scene::in, list(bounding_sphere)::out) is det.

% pre_compute_lighting calculates the "cleanly illuminated" (ie no chance of
% any shadows) light list for every basic object in the scene

:- pred pre_compute_lighting(scene::in, list(bounding_sphere)::in,
    list(light)::in, scene::out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module renderer.

:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module std_util.

%------------------------------------------------------------------------------%

% X_list - these functions go through an X,
% and return a list of all the objects therein

% top level scene -> object list function
scene_list(scene(Part, Objs), List) :-
	list.map(object_list, Objs, Tmp),
	list.condense(Tmp, L1),
	partition_list(Part, L2),
	List = list.append(L1, L2).

% now iterate down the partition tree looking for objects
:- pred partition_list(space_tree::in, list(bounding_sphere)::out) is det.

partition_list(space_tree(_Pt, _N, Nodes), List) :-
	list.map(space_tree_node_list, Nodes, ObjectLists),
	List = condense(ObjectLists).

:- pred space_tree_node_list(space_tree_node::in, list(bounding_sphere)::out)
    is det.

space_tree_node_list(Node, List) :-
	(
		Node = leaf(SpaceObj),
		SpaceObj = space_tree_object(_, _, Object),
		object_list(Object, List)
	;
		Node = node(Tree),
		partition_list(Tree, List)
	).

%------------------------------------------------------------------------------%

% okay - we've got to the object tree; just flatten it into a list
:- pred object_list(object::in, list(bounding_sphere)::out) is det.

object_list(Obj, BSList) :-
    (
        Obj = basic_object(_, _, _),
        Box = find_object_bounding_box(Obj),
        BS = bounding_sphere(Box),
        BSList = [BS]
    ;
        Obj = union(Object1, Object2),
        object_list(Object1, BSList1),
        object_list(Object2, BSList2),
        list.append(BSList1, BSList2, BSList)
    ;
        Obj = intersect(Object1, Object2),
        object_list(Object1, BSList1),
        object_list(Object2, BSList2),
        list.append(BSList1, BSList2, BSList)
    ;
        Obj = difference(Object1, Object2),
        object_list(Object1, BSList1),
        object_list(Object2, BSList2),
        list.append(BSList1, BSList2, BSList)
    ;
        Obj = transform(SubObj, Trans),
	    ( SubObj = basic_object(_, _, _) ->
            % handle a transform applied to a basic object
            Box0 = find_object_bounding_box(SubObj),
            Trans2 = maybe_transformation_to_trans(yes(Trans)),
            Box = transform_bounding_box(Box0, Trans2),
            BS = bounding_sphere(Box),
            BSList = [BS]
        ;
            % choke on complex transforms
            throw("object_list can't handle complex transform objects!")
        )
    ).

%------------------------------------------------------------------------------%

% this section is a bit like the stuff above, but this time, we actually
% rebuild the entire scene with the useful shadow information inside

% top level scene rebuilder
pre_compute_lighting(scene(Part, Objs), BSList, Lights, NewScene) :-
	traverse_part(Part, BSList, Lights, NewPart),
	list.map(traverse_objects(BSList, Lights), Objs, NewObjs),
	NewScene = scene(NewPart, NewObjs).

% rebuild the partition tree
:- pred traverse_part(space_tree::in, list(bounding_sphere)::in,
    list(light)::in, space_tree::out) is det.

traverse_part(space_tree(Box, Area, Nodes0), BSList, Lights,
		space_tree(Box, Area, Nodes)) :-
	list.map(
		(pred(Node0::in, Node::out) is det :-
			(
				Node0 = leaf(space_tree_object(A, B, Obj0)),
				traverse_objects(BSList, Lights, Obj0, Obj),
				Node = leaf(space_tree_object(A, B, Obj))
			;
				Node0 = node(Tree0),
				traverse_part(Tree0, BSList, Lights, Tree),
				Node = node(Tree)
			)
		), Nodes0, Nodes).

%------------------------------------------------------------------------------%

% rebuild an object tree

:- pred traverse_objects(list(bounding_sphere)::in, list(light)::in,
    object::in, object::out) is det.

traverse_objects(BSList, Lights, Obj, NewObject) :-
    (
        Obj = basic_object(_, _, _),
        calc_light_list(Obj, no, BSList, Lights, NewObject)
    ;
        Obj = union(Obj1, Obj2),
        traverse_objects(BSList, Lights, Obj1, NewObject1),
        traverse_objects(BSList, Lights, Obj2, NewObject2),
        NewObject = union(NewObject1, NewObject2)
    ;
        Obj = intersect(Obj1, Obj2),
        traverse_objects(BSList, Lights, Obj1, NewObject1),
        traverse_objects(BSList, Lights, Obj2, NewObject2),
        NewObject = intersect(NewObject1, NewObject2)
    ;
        Obj = difference(Obj1, Obj2),
        traverse_objects(BSList, Lights, Obj1, NewObject1),
        traverse_objects(BSList, Lights, Obj2, NewObject2),
        NewObject = difference(NewObject1, NewObject2)
    ;
        Obj = transform(SubObj, Trans),
        % The only kind of transform we can handle
        % is one wrapping a basic object.
        ( SubObj = basic_object(_, _, _) ->
            calc_light_list(SubObj, yes(Trans), BSList, Lights, NewObject)
        ;
            % If we get another kind of transform, spit the dummy....
            throw("traverse_objects can't handle complex transform objects!")
        )
    ).

%------------------------------------------------------------------------------%

% calc_light_list adds the "clearly illuminating light" list to an object

:- pred calc_light_list(object::in(basic_object_inst),
	maybe(transformation)::in, list(bounding_sphere)::in, list(light)::in,
	object::out) is det.

% planes *might* go in here, if all objects are on the opposite side of them...
% but it's probably not worth the effort

calc_light_list(Obj, MaybeTrans, BSList, Lights, NewObject) :-
	Obj = basic_object(Id, Obj2, LList0),
	( if (Obj2 = plane(_S)) then
		NewObject = Obj   % for now, do nothing for planes
	else
		(
			MaybeTrans = no,
			BBox = find_object_bounding_box(Obj)
		;
			MaybeTrans = yes(Trans),
			BBox0 = find_object_bounding_box(Obj),
			Trans2 = maybe_transformation_to_trans(yes(Trans)),
			BBox = transform_bounding_box(BBox0,Trans2)
		),
		bsphere(Centre, Radius) = bounding_sphere(BBox),
		list.filter(is_clear(Centre, Radius, BSList), Lights, LList1),
		list.append(LList0, LList1, LList2),
		NewObject = basic_object( Id, Obj2, LList2 )
	).

%------------------------------------------------------------------------------%

% is_clear is true if none of the bounding spheres in BSList could cast a
% shadow on the object (Centre, Radius) against the light Light.

:- pred is_clear(point::in, real::in, list(bounding_sphere)::in, light::in)
    is semidet.

is_clear(Centre, Radius, BSList, Light) :-
	list.filter(single_is_clear(Centre, Radius, Light), BSList, BadList),
	list.length(BadList, Len),
	Len =< 1.  % the only "non clear" bounding sphere is our own...

% single_is_clear is true if we can guarantee that one object does not
% enshadow another
% SCentre, SRadius - the object which might cast the shadow
% Centre, Radius - the object which might be shadowed

:- pred single_is_clear(point, real, light, bounding_sphere).
:- mode single_is_clear(in, in, in, in) is semidet.

single_is_clear(Centre, Radius, Light, bsphere(SCentre, SRadius)) :-
	ObjOffset = SCentre - Centre,
	LightOffset = light_pos(Light) - Centre,
	LightwardsComponent = project( ObjOffset, LightOffset ),
	ObjClearance2 = mag2(LightOffset - LightwardsComponent),
	(
		ReqClearance = Radius + SRadius,
		ObjClearance2 > (ReqClearance * ReqClearance)
	;
		dot(LightwardsComponent,LightOffset) < 0.0,
		mag(LightwardsComponent) > (SRadius + Radius)
	).

% light_pos - find the position of a light source
:- func light_pos(light) = point.
light_pos(pointlight(Pos, _)) = Pos.
light_pos(spotlight(Pos, _, _, _, _)) = Pos.
light_pos(Light) = Pos :-
	Light = directional(_, _),
	Pos =scale(real_max / 2.0, light_unit_vector(Light,point(0.0,0.0,0.0))).

%------------------------------------------------------------------------------%

% bounding_sphere takes a pair of points and returns their "centre" and "radius"

:- func bounding_sphere(bounding_box) = bounding_sphere.

bounding_sphere(P1 - P2) = bsphere(Centre, Radius) :-
	Centre = scale( 0.5, P1 + P2 ),
	Radius = mag( P1 - P2 ) / 2.0.
