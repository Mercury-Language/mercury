% This module defines the stuff for interpreting GML programs.

:- module eval.
:- interface.
:- import_module bool, list, array, map.
:- import_module gml, trans, vector.
:- import_module maybe.
:- import_module renderer.

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

:- inst basic_object_inst for object/0 == bound( basic_object(ground, ground, ground)).

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

:- func new_interpreter_state = state.

:- type env == map(id, value).

:- type id == string.

:- type stack == list(value).

:- type code == token_list.

:- pred interpret(code::in, state::in, state::out) is det.

:- pred interpret(code::in, env::in, stack::in,
		env::out, stack::out, state::in, state::out) is det.

	% Some exceptions we might throw.
:- type stack_env_exception --->
	stack_env_exception(string, env, stack).

:- type stack_env_token_exception --->
	stack_env_token_exception(string, env, stack, token).

	% An error in the program itself.
:- type program_error 
	---> 	program_error(string) 
	;	program_error(string, stack).


	% Peephole needs this to build closures that do evaluation.
:- func push(value, stack) = stack.
:- pred pop(stack::in, value::out, stack::out) is semidet.
:- pred eval_error(env::in, stack::in) is erroneous.

	% args(Op, In, Out)
	%	The number of args operator takes off the stack and Out
	%	maybe holds the number of results the operator puts back
	%	onto the stack.
:- pred args(operator::in, int::out, maybe(int)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, float, std_util, exception, string.
:- import_module transform_object, space_partition.
:- import_module globals.
:- import_module op.
:- import_module peephole.

new_interpreter_state = 
    state(
        1,      % Global object counter
        []      % Render commands.
    ).

:- pred state_add_render_command(render_params::in, state::in, state::out) is det.

state_add_render_command(Params, !State) :-
    some [!RenderCommands] (
        !:RenderCommands = !.State ^ s_render_commands,
        !:RenderCommands = [ Params | !.RenderCommands ],
        !:State = !.State ^ s_render_commands := !.RenderCommands
    ).

interpret(Code, !State) :-
    map__init(Env0),
    Stack0 = [],
	interpret(Code, Env0, Stack0, _Env, _Stack, !State).

interpret([], Env, Stack, Env, Stack) --> [].
interpret([Token|Tokens], Env0, Stack0, Env, Stack) -->
	do_token_group(Token, Env0, Stack0, Env1, Stack1),
	interpret(Tokens, Env1, Stack1, Env, Stack).

:- pred do_token_group(token_group::in, env::in, stack::in,
		env::out, stack::out, state::in, state::out) is det.

do_token_group(single_token(Token), Env0, Stack0, Env, Stack) -->
	do_token(Token, Env0, Stack0, Env, Stack).
do_token_group(function(TokenList), Env0, Stack0, Env, Stack) -->
		% XXX this is only a win if a function gets invoked
		% multiple times.
	% { peephole(TokenList, OptTokenList) },
	{ (TokenList = OptTokenList) },
	{ Stack = push(closure(Env0, OptTokenList), Stack0) },
	{ Env = Env0 }.
do_token_group(array(TokenList), Env0, Stack0, Env, Stack) -->
	interpret(TokenList, Env0, empty_stack, _ResultEnv, ArrayStack),
	{ Stack = push(array(array(reverse(ArrayStack))), Stack0) },
	{ Env = Env0 }.

:- pred do_token(token::in, env::in, stack::in,
		env::out, stack::out, state::in, state::out) is det.
do_token(operator(Operator), Env0, Stack0, Env, Stack) -->
	{ Env = Env0 },
	do_op(Operator, Env, Stack0, Stack).
do_token(identifier(Id), Env0, Stack0, Env, Stack) -->
	(
		{ map__search(Env0, Id, Val) }
	->
		{ Stack = push(Val, Stack0) },
		{ Env = Env0 }
	;
		{ throw(program_error(string__append_list(
			["identifier `", Id, "' is unknown"]))) }
	).
do_token(binder(Id), Env0, Stack0, Env, Stack) -->
	{ pop(Stack0, Val, Stack1) ->
		Stack = Stack1,
		map__set(Id, Val, Env0, Env)
			% XXX what if id is already bound?
			% is it right to just overwrite
			% the old value?
			% XXX trd: I think so.  You can't rebind
			% operators but you can rebind other things.
	;
		empty_stack(Env0, Stack0, binder(Id))
	}.
do_token(boolean(Bool), Env0, Stack0, Env, Stack) -->
	{ Stack = push(boolean(Bool), Stack0) },
	{ Env = Env0 }.
do_token(number(integer(Int)), Env0, Stack0, Env, Stack) -->
	{ Stack = push(int(Int), Stack0) },
	{ Env = Env0 }.
do_token(number(real(Real)), Env0, Stack0, Env, Stack) -->
	{ Stack = push(real(Real), Stack0) },
	{ Env = Env0 }.
do_token(string(String), Env0, Stack0, Env, Stack) -->
	{ Stack = push(string(String), Stack0) },
	{ Env = Env0 }.
do_token(extra(Operator), Env0, Stack0, Env, Stack) -->
	{ Env = Env0 },
	do_extra(Operator, Env, Stack0, Stack).

%-----------------------------------------------------------------------------%

:- pred do_op(operator, env, stack, stack, state, state).
:- mode do_op(in, in, in, out, in, out) is det.


do_op(acos, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_acos(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(addi, Env, Stack0, Stack) -->
	( { Stack0 = [int(N2), int(N1) | Stack1] } ->
		{ Stack = push(int(op_addi(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(addf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N2), real(N1) | Stack1] } ->
		{ Stack = push(real(op_addf(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(apply, Env, Stack0, Stack) -->
	( { Stack0 = [closure(ClosureEnv, ClosureCode) | Stack1] } ->
		interpret(ClosureCode, ClosureEnv, Stack1, 
			_ResultEnv, Stack)
	;
		{ eval_error(Env, Stack0) }
	).
do_op(asin, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_asin(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(clampf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_clampf(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(cone, Env, Stack0, Stack) --> %Tier-2
	( { Stack0 = [closure(CEnv, CCode) | Stack1] } ->
		next_object_id(Id),
		{ Stack = push(object(
				basic_object(Id, cone(surface(CEnv, CCode)),
						[])),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(cos, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_cos(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(cube, Env, Stack0, Stack) --> %Tier-2
	( { Stack0 = [closure(CEnv, CCode) | Stack1] } ->
		next_object_id(Id),
		{ Stack = push(object(
				basic_object(Id, cube(surface(CEnv, CCode)),
						[])),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(cylinder, Env, Stack0, Stack) --> %Tier-2
	( { Stack0 = [closure(CEnv, CCode) | Stack1] } ->
		next_object_id(Id),
		{ Stack = push(object(
				basic_object(Id, cylinder(surface(CEnv, CCode)),
					[])),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(difference, Env, Stack0, Stack) --> %Tier-3
	( { Stack0 = [object(O2), object(O1) | Stack1] } ->
		{ Stack = push(object(difference(O1, O2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(divi, Env, Stack0, Stack) -->
	( { Stack0 = [int(N2), int(N1) | Stack1], N2 \= 0 } ->
		{ Stack = push(int(op_divi(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(divf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N2), real(N1) | Stack1] } ->
		{ Stack = push(real(op_divf(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(eqi, Env, Stack0, Stack) -->
	( { Stack0 = [int(N2), int(N1) | Stack1] } ->
		{ Stack = push(boolean(op_eqi(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(eqf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N2), real(N1) | Stack1] } ->
		{ Stack = push(boolean(op_eqf(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(floor, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(int(op_floor(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(frac, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_frac(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(get, Env, Stack0, Stack) -->
	( { Stack0 = [int(I), array(A) | Stack1], in_bounds(A, I) } ->
		{ lookup(A, I, Val) },
		{ Stack = push(Val, Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(getx, Env, Stack0, Stack) -->
	( { Stack0 = [point(point(X,_Y,_Z)) | Stack1] } ->
		{ Stack = push(real(X), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(gety, Env, Stack0, Stack) -->
	( { Stack0 = [point(point(_X,Y,_Z)) | Stack1] } ->
		{ Stack = push(real(Y), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(getz, Env, Stack0, Stack) -->
	( { Stack0 = [point(point(_X,_Y,Z)) | Stack1] } ->
		{ Stack = push(real(Z), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(if, Env, Stack0, Stack) -->
	( { Stack0 = [closure(CE2, CC2), closure(CE1, CC1), boolean(YesNo)
				| Stack1] } ->
		(
			{ YesNo = yes },
			interpret(CC1, CE1, Stack1, _ResultEnv, Stack)
		;
			{ YesNo = no },
			interpret(CC2, CE2, Stack1, _ResultEnv, Stack)
		)
	;
		{ eval_error(Env, Stack0) }
	).
do_op(intersect, Env, Stack0, Stack) --> %Tier-3
	( { Stack0 = [object(O2), object(O1) | Stack1] } ->
		{ Stack = push(object(intersect(O1, O2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(length, Env, Stack0, Stack) -->
	( { Stack0 = [array(A) | Stack1] } ->
		{ size(A, Size) },
		{ Stack = push(int(Size), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(lessi, Env, Stack0, Stack) -->
	( { Stack0 = [int(N2), int(N1) | Stack1] } ->
		{ Stack = push(boolean(op_lessi(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(lessf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N2), real(N1) | Stack1] } ->
		{ Stack = push(boolean(op_lessf(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(light, Env, Stack0, Stack) -->
	( { Stack0 = [point(Colour), point(Dir) | Stack1] } ->
		{ Stack = push(light(directional(Dir, Colour)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(modi, Env, Stack0, Stack) -->
	( { Stack0 = [int(N2), int(N1) | Stack1] } ->
		{ Stack = push(int(op_modi(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(muli, Env, Stack0, Stack) -->
	( { Stack0 = [int(N2), int(N1) | Stack1] } ->
		{ Stack = push(int(op_muli(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(mulf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N2), real(N1) | Stack1] } ->
		{ Stack = push(real(op_mulf(N1, N2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(negi, Env, Stack0, Stack) -->
	( { Stack0 = [int(N) | Stack1] } ->
		{ Stack = push(int(op_negi(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(negf, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_negf(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(plane, Env, Stack0, Stack) -->
	( { Stack0 = [closure(CEnv, CCode) | Stack1] } ->
		next_object_id(Id),
		{ Stack = push(object(
				basic_object(Id, plane(surface(CEnv, CCode)),
						[])),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(point, Env, Stack0, Stack) -->
	( { Stack0 = [real(Z), real(Y), real(X) | Stack1] } ->
		{ Stack = push(point(point(X,Y,Z)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(pointlight, Env, Stack0, Stack) --> %Tier-2
	( { Stack0 = [point(Colour), point(Pos) | Stack1] } ->
		{ Stack = push(light(pointlight(Pos, Colour)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(real, Env, Stack0, Stack) -->
	( { Stack0 = [int(N) | Stack1] } ->
		{ Stack = push(real(op_real(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(render, Env, Stack0, Stack, !State) :- 
	(
		Stack0 = [string(File), int(Ht), int(Wid), real(FOV),
			int(Depth), object(Obj), array(Lights), point(Amb)
			| Stack1]
	->
		Scene = create_scene(push_transformations(Obj)),
		Params = render_params(Amb, Lights, Scene, Depth,
			FOV, Wid, Ht, File),
        state_add_render_command(Params, !State),
		Stack = Stack1
	;
		eval_error(Env, Stack0)
	).
do_op(rotatex, Env, Stack0, Stack) -->
	( { Stack0 = [real(Theta), object(Obj0) | Stack1] } ->
		renameObject(Obj0, Obj),
		{ Stack = push(object(transform(Obj, rotatex(Theta))),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(rotatey, Env, Stack0, Stack) -->
	( { Stack0 = [real(Theta), object(Obj0) | Stack1] } ->
		renameObject(Obj0, Obj),
		{ Stack = push(object(transform(Obj, rotatey(Theta))),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(rotatez, Env, Stack0, Stack) -->
	( { Stack0 = [real(Theta), object(Obj0) | Stack1] } ->
		renameObject(Obj0, Obj),
		{ Stack = push(object(transform(Obj, rotatez(Theta))),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(scale, Env, Stack0, Stack) -->
	( { Stack0 = [real(Z), real(Y), real(X), object(Obj0) | Stack1] } ->
		renameObject(Obj0, Obj),
		{ Stack = push(object(transform(Obj, scale(X, Y, Z))),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(sin, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1] } ->
		{ Stack = push(real(op_sin(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(sphere, Env, Stack0, Stack) -->
	( { Stack0 = [closure(CEnv, CCode) | Stack1] } ->
		next_object_id(Id),
		{ Stack = push(object(
				basic_object(Id, sphere(surface(CEnv, CCode)),
						[])),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(spotlight, Env, Stack0, Stack) --> %Tier-3
	( { Stack0 = [ real(Exp), real(Cutoff), point(Colour), 
				point(At), point(Pos) | Stack1] } ->
		{ Stack = push(light(spotlight(Pos, At, Colour, Cutoff, Exp)), 
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(sqrt, Env, Stack0, Stack) -->
	( { Stack0 = [real(N) | Stack1], N >= 0.0 } ->
		{ Stack = push(real(op_sqrt(N)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(subi, Env, Stack0, Stack) -->
	{ Stack0 = [int(N2), int(N1) | Stack1] ->
		Stack = push(int(op_subi(N1, N2)), Stack1)
	;
		empty_stack(Env, Stack0, operator(subi))
	}.
do_op(subf, Env, Stack0, Stack) -->
	{ Stack0 = [real(N2), real(N1) | Stack1] ->
		Stack = push(real(op_subf(N1, N2)), Stack1)
	;
		empty_stack(Env, Stack0, operator(subf))
	}.
do_op(translate, Env, Stack0, Stack) -->
	( { Stack0 = [real(Z), real(Y), real(X), object(Obj0) | Stack1] } ->
		renameObject(Obj0, Obj),
		{ Stack = push(object(transform(Obj, translate(X, Y, Z))),
				Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(union, Env, Stack0, Stack) -->
	( { Stack0 = [object(O2), object(O1) | Stack1] } ->
		{ Stack = push(object(union(O1, O2)), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).
do_op(uscale, Env, Stack0, Stack) -->
	( { Stack0 = [real(S), object(Obj0) | Stack1] } ->
		renameObject(Obj0, Obj),
		{ Stack = push(object(transform(Obj, uscale(S))), Stack1) }
	;
		{ eval_error(Env, Stack0) }
	).

%-----------------------------------------------------------------------------%

	% Rename each of the basic objects in the structure.
:- pred renameObject(object::in, object::out,
    state::in, state::out) is det.

renameObject(basic_object(_, BasicObject,L), basic_object(Id,BasicObject,L)) -->
	next_object_id(Id).
renameObject(transform(Obj0, Trans), transform(Obj, Trans)) -->
	renameObject(Obj0, Obj).
renameObject(union(Left0, Right0), union(Left, Right)) -->
	renameObject(Left0, Left),
	renameObject(Right0, Right).
renameObject(intersect(Left0, Right0), intersect(Left, Right)) -->
	renameObject(Left0, Left),
	renameObject(Right0, Right).
renameObject(difference(Left0, Right0), difference(Left, Right)) -->
	renameObject(Left0, Left),
	renameObject(Right0, Right).

:- pred next_object_id(object_id::out, state::in, state::out) is det.

next_object_id(Id, !State) :- 
    Id = !.State ^ s_global_object_counter,
    !:State = !.State ^ s_global_object_counter := Id + 1.

%-----------------------------------------------------------------------------%

:- pred extra_operator_mode(extra_operator::in,
	extra_operator::out(extra_operator_inst)) is det.

:- pragma foreign_proc("C",
	extra_operator_mode(A::in, B::out(extra_operator_inst)),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	B = A;
").

%-----------------------------------------------------------------------------%

:- pred do_extra(extra_operator, env, stack, stack, state, state).
:- mode do_extra(in, in, in, out, in, out) is det.
do_extra(Extra0, Env, Stack0, Stack) -->
	{ extra_operator_mode(Extra0, Extra) },	
	do_extra2(Extra, Env, Stack0, Stack).

:- pragma inline(do_extra2/6).

:- pred do_extra2(extra_operator, env, stack, stack, state, state).
:- mode do_extra2(in(extra_operator_inst), in, in, out, in, out) is det.

do_extra2(mercury_closure(C), Env, Stack0, Stack) -->
	C(Env, Stack0, _, Stack).
do_extra2(dup, Env, Stack0, Stack) -->
	{ Stack0 = [Head | Tail] -> 
		Stack = [Head, Head | Tail] 
	;
		eval_error(Env, Stack0)
	}.
do_extra2(popn(N), Env, Stack0, Stack) -->
	{ popn(N, Stack0, Stack1) ->
		Stack = Stack1
	;
		eval_error(Env, Stack0)
	}.
do_extra2(constant_sphere(SurfaceProperties), _Env, Stack0, Stack) -->
	next_object_id(Id),
	{ Stack = push(object(
			basic_object(Id, sphere(constant(SurfaceProperties)),
					[])),
			Stack0) }.
do_extra2(constant_plane(SurfaceProperties), _Env, Stack0, Stack) -->
	next_object_id(Id),
	{ Stack = push(object(
			basic_object(Id, plane(constant(SurfaceProperties)),
					[])),
			Stack0) }.
do_extra2(constant_cone(SurfaceProperties), _Env, Stack0, Stack) -->
	next_object_id(Id),
	{ Stack = push(object(
			basic_object(Id, cone(constant(SurfaceProperties)),
					[])),
			Stack0) }.
do_extra2(constant_cube(SurfaceProperties), _Env, Stack0, Stack) -->
	next_object_id(Id),
	{ Stack = push(object(
			basic_object(Id, cube(constant(SurfaceProperties)),
					[])),
			Stack0) }.
do_extra2(constant_cylinder(SurfaceProperties), _Env, Stack0, Stack) -->
	next_object_id(Id),
	{ Stack = push(object(
			basic_object(Id, cylinder(constant(SurfaceProperties)),
					[])),
			Stack0) }.
do_extra2(constant_point(Point), _Env, Stack0, Stack) -->
	{ Stack = push(point(Point), Stack0) }.
do_extra2(constant_if(C1, C2), Env, Stack0, Stack) -->
	( 
		{ Stack0 = [boolean(YesNo) | Stack1] } 
	->
		(
			{ YesNo = yes },
			{ Stack = push(C1, Stack1) }
		;
			{ YesNo = no },
			{ Stack = push(C2, Stack1) }
		)
	;
		{ eval_error(Env, Stack0) }
	).

%-----------------------------------------------------------------------------%

:- func create_surface_properties(float, float, float,
		float, float, float) = surface_properties.

create_surface_properties(R, G, B, Diffuse, Specular, Phong)
	= surface_properties(point(R, G, B), Diffuse, Specular, Phong).

%-----------------------------------------------------------------------------%

args(acos, 1, yes(1)).
args(addi, 2, yes(1)).
args(addf, 2, yes(1)).
args(apply, 1, no).		% XXX pd handle this differently!
args(asin, 1, yes(1)).
args(clampf, 1, yes(1)).
args(cone, 1, yes(1)).
args(cos, 1, yes(1)).
args(cube, 1, yes(1)).
args(cylinder, 1, yes(1)).
args(difference, 2, yes(1)).
args(divi, 2, yes(1)).
args(divf, 2, yes(1)).
args(eqi, 2, yes(1)).
args(eqf, 2, yes(1)).
args(floor, 1, yes(1)).
args(frac, 1, yes(1)).
args(get, 2, yes(1)).
args(getx, 1, yes(1)).
args(gety, 1, yes(1)).
args(getz, 1, yes(1)).
args(if, 3, no).		% XXX pd handle this differently
args(intersect, 2, yes(1)).
args(length, 1, yes(1)).
args(lessi, 2, yes(1)).
args(lessf, 2, yes(1)).
args(light, 2, yes(1)).
args(modi, 2, yes(1)).
args(muli, 2, yes(1)).
args(mulf, 2, yes(1)).
args(negi, 1, yes(1)).
args(negf, 1, yes(1)).
args(plane, 1, yes(1)).
args(point, 3, yes(1)).
args(pointlight, 2, yes(1)).
args(real, 1, yes(1)).
args(render, 8, yes(0)).	% XXX pd handle this specially.
args(rotatex, 2, yes(1)).
args(rotatey, 2, yes(1)).
args(rotatez, 2, yes(1)).
args(scale, 4, yes(1)).
args(sin, 1, yes(1)).
args(sphere, 1, yes(1)).
args(spotlight, 5, yes(1)).
args(sqrt, 1, yes(1)).
args(subi, 2, yes(1)).
args(subf, 2, yes(1)).
args(translate, 4, yes(1)).
args(union, 2, yes(1)).
args(uscale, 2, yes(1)).

%-----------------------------------------------------------------------------%

push(X, L) = [X | L].

pop([X|Xs], X, Xs).

	% pop n values of the stack and throw them away.
:- pred popn(int::in, stack::in, stack::out) is semidet.
popn(N, Stack0, Stack) :-
	( N =< 0 ->
		Stack = Stack0
	;
		pop(Stack0, _, Stack1),
		popn(N - 1, Stack1, Stack)
	).


:- func empty_stack = stack.
empty_stack = [].

%-----------------------------------------------------------------------------%

eval_error(Env, Stack) :- 
	( Stack = [] ->
		throw(stack_env_exception(
			"empty stack during evaluation",
			Env, Stack))
	;
		throw(program_error(
			"type error during evalutation", Stack
			))
	).


:- pred type_error is erroneous.
type_error :- throw("type error").

:- pred empty_stack(env, stack, token).
:- mode empty_stack(in, in, in) is erroneous.
empty_stack(E, S, T) :- 
	throw(stack_env_token_exception("empty stack", E, S, T)).

:- pred stub(env, stack, stack).
:- mode stub(in, in, out) is erroneous.
stub(E, S, S) :- throw(stack_env_exception("not yet implemented", E, S)).

%-----------------------------------------------------------------------------%

