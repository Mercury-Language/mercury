% Test case for bug 130.  Addapted from the ICFP2000 ray-tracer.

:- module bug_130.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, float, std_util, exception, string.
:- import_module require.
:- import_module bool, list, array, map.
:- import_module pair.

:- type value
	% base values
	--->	boolean(bool)
	;	int(int)
	;	real(real)
	;	string(string)
	% non-base values
	;	closure(env, code)
	;	array(array)
	;	point(point)
	;	object(object)
	;	light(light).

:- type point == vector.
:- type vector ---> point(real, real, real).

:- type color == point. % components restricted to range [0.0, 1.0]

:- type array == array(value).

:- type light
	--->	directional(
			dir::vector,
			directional_intensity::color
		)
	;	pointlight(	% Tier 2
			pointlight_pos::position,
			pointlight_intensity::color
		)
	;	spotlight(	% Tier 3
			spotlight_pos::position,
			at::position,
			spotlight_intensity::color,
			cutoff::degrees,
			exp::real
		).

:- type position == point.

:- type degrees == real.

:- type object_id == int.

	% XXX this is very tentative
:- type object
	--->	basic_object(object_id, basic_object, list(light))
	
		% XXX should these be applied when they
		% are found, or done lazily.
	;	transform(object, transformation)

	;	union(object, object)
	;	intersect(object, object)	% Tier 3
	;	difference(object, object)	% Tier 3
	.

:- inst basic_object_inst == bound( basic_object(ground, ground, ground)).

:- type basic_object	
	--->	sphere(surface)
	;	cube(surface)			% Tier 2
	;	cylinder(surface)		% Tier 2
	;	cone(surface)			% Tier 2
	;	plane(surface).

:- type transformation
	---> 	translate(tx::real, ty::real, tz::real)
	;	scale(sx::real, sy::real, sz::real)
	;	uscale(s::real)
	;	rotatex(rotatex_theta::degrees)
	;	rotatey(rotatey_theta::degrees)
	;	rotatez(rotatez_theta::degrees)
	;	matrix(trans)
	.

:- type trans
    --->    trans(matrix, matrix).      % ObjToWorldSpace - WorldToObjSpace.

:- type matrix
    --->    matrix(
                float, float, float, float,
                float, float, float, float,
                float, float, float, float
              % 0,    0,    0,    1
            ).

:- type surface
	--->	surface(env, code)		% The surface function
	;	constant(surface_properties).	% surface function is constant

:- type surface_properties --->
	surface_properties(
		surface_c :: color,
		surface_kd :: real,		% diffuse reflection coeff
		surface_ks :: real,		% specular reflection coeff
		surface_n :: real		% Phong exp
	).

    % Interpreter state.
    %
:- type state
    --->    state(
        s_global_object_counter     :: object_id,
        s_render_commands           :: list(render_params)
    ).

:- type render_params
    ---> render_params(
        amb :: color,        % the ambient light
        lights :: array,    % array(light)
        scene :: scene,        % the scene to render
        depth :: int,
        fov :: real,        % the field of view
        wid :: int,        % the width, in pixels
        ht :: int,        % the height, in pixels
        file :: string
    ).

:- type real == float.

:- type scene
	---> scene(
		space_tree,
		list(object)	% objects which can't be partitioned
				% (e.g. planes)
	).

:- type space_tree
	---> 	space_tree(
			bounding_box,
			surface_area,
			list(space_tree_node)
		).

:- type space_tree_node
	--->	node(
			space_tree
		)
	;
		leaf(
			space_tree_object
		)	
	.

:- type space_tree_object
	--->	space_tree_object(
			bounding_box,
			surface_area,
			object
		).

:- type surface_area == real.
:- type bounding_box == pair(point).

:- func new_interpreter_state = bug_130.state.

:- type env == map(id, value).

:- type id == string.

:- type stack == list(value).

:- type token_list == list(token_group).

:- type token_group
	--->	single_token(token)
	;	function(token_list)
	;	array(token_list).

:- type token
	--->	operator(operator)
	;	identifier(string)
	;	binder(string)
	;	boolean(bool)
	;	number(number)
	;	string(string)
		
		% Not part of the spec
		% these are extra operators which make interpretation
		% more efficient.
	;	extra(extra_operator).

:- type operator
	--->	acos
	;	addi
	;	addf
	;	apply
	;	asin
	;	clampf
	;	cone			% Tier-2
	;	cos
	;	cube			% Tier-2
	;	cylinder		% Tier-2
	;	difference		% Tier-3
	;	divi
	;	divf
	;	eqi
	;	eqf
	;	floor
	;	frac
	;	get
	;	getx
	;	gety
	;	getz
	;	(if)
	;	intersect		% Tier-3
	;	length
	;	lessi
	;	lessf
	;	light
	;	modi
	;	muli
	;	mulf
	;	negi
	;	negf
	;	plane
	;	point
	;	pointlight		% Tier-2
	;	real
	;	render
	;	rotatex
	;	rotatey
	;	rotatez
	;	scale
	;	sin
	;	sphere
	;	spotlight		% Tier-3
	;	sqrt
	;	subi
	;	subf
	;	translate
	;	union
	;	uscale
	.

:- type number
	--->	integer(int)
	;	real(float).

:- type extra_operator
	--->	popn(int)		% discard top n elements of stack
	;	dup			% duplicate the topmost element
	;	constant_sphere(
			surface_properties
		)
	;	constant_plane(
			surface_properties
		)
	;	constant_cone(
			surface_properties
		)
	;	constant_cube(
			surface_properties
		)
	;	constant_cylinder(
			surface_properties
		)
	;	constant_point(
			point
		)
			% an `if' whose arms are just constants
	;	constant_if(
			value,
			value
		)
	;	mercury_closure(
			pred(env, stack,
				env, stack,
				bug_130.state, bug_130.state
			)
		)
	.

:- type code == token_list.

	% Some exceptions we might throw.
:- type stack_env_exception --->
	stack_env_exception(string, env, stack).

:- type stack_env_token_exception --->
	stack_env_token_exception(string, env, stack, token).

	% An error in the program itself.
:- type program_error 
	---> 	program_error(string) 
	;	program_error(string, stack).


main(!IO) :-
    State0 = new_interpreter_state,
    interpret([], State0, State),
    io.write(State, !IO).

new_interpreter_state = 
    state(
        1,      % Global object counter
        []      % Render commands.
    ).

:- pred interpret(code::in, bug_130.state::in, bug_130.state::out) is det.

interpret(Code, !State) :-
    map__init(Env0),
    Stack0 = [],
	interpret(Code, Env0, Stack0, _Env, _Stack, !State).

:- pred interpret(code::in, env::in, stack::in, env::out, stack::out,
    bug_130.state::in, bug_130.state::out) is det.

interpret([], Env, Stack, Env, Stack) --> [].
interpret(Tokens0, Env0, Stack0, Env, Stack) -->
    { Tokens0 = [Token | Tokens] },
    (
        do_token_group(Token, Env0, Stack0, Env1, Stack1)
    &
	    interpret(Tokens, Env1, Stack1, Env, Stack)
    ).

:- pred do_token_group(token_group::in, env::in, stack::in,
		env::out, stack::out, bug_130.state::in, bug_130.state::out) 
    is det.

do_token_group(_, Env, Stack, Env, Stack, State, State) :-
    error("Predicate not implemented").

