%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: shapes.m
% Main author: trd
%
% This file prepares the shape information that is required by each
% module.
%
% shapes__request_shape_number is called during code generation
% and gathers information about the shape if it isn't already within the
% table.
%
% Different shapes and shape numbers are generated for every type which
% is different. Since list(foo) and list(bar) have different types, they
% will have seperate shapes. Parameteric polymorphism, such as list(T),
% is treated seperately also, but a normal shape description is not given
% to the T, but it is stored as a type variable, so it can be treated as
% a list, up until the point where garbage collection needs to know what
% it is a list of, then the gc system can find out (at runtime) what it
% needs to know.
%
% When a closure , eg pred(....) is passed a special shape is created for it
% so its arguments can be determined at runtime.
%
% Abstract shapes have their type infomation stored in a special format,
% and they can be replaced by a normal shape at link time, when we know what
% the implementation of the abstract shape is. For this to work properly,
% we need a table mapping abstract types (without any arguments) that are
% exported, to the definition of that type's shape (yet to be done).
% Then at link time, all abstract types which are encountered can be
% replaced by the implementation of that shape.
%
% XXX Rely on 32bit architecture due to 4 bit tags - better to use a list.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module shapes.
:- interface.

:- import_module hlds_module.
:- import_module list, term, io.

:- type tagged_num	==	pair(shape_num, tag_type).
:- type tag_type	--->	const; simple; complicated.

:- type shape_list	==	list(pair(shape_num, tagged_num)).
:- type length_list	==	list(int).
:- type contents_list	==	list(int).

:- type shape_num	--->	num(int)
			;	builtin(int)
			;	succip
			;	hp
			;	maxfr
			;	curfr
			;	redoip
			;	succfr
			;	prevfr
			;	sp
			;	ticket
			;	unwanted.

:- pred shapes__init_shape_table(shape_table).
:- mode shapes__init_shape_table(out) is det.

	% Returns a corresponding shape number for a shape_id,
	% adding it to the shape_table if it is not already there.
	% Presently, we only handle ground shapes.
:- pred shapes__request_shape_number(shape_id, type_table, shape_table,
			shape_table, int).
:- mode shapes__request_shape_number(in, in, in, out, out) is det.
	
	% Create shape numbers for any types that are abstract exports.
	% These are also the types that need to have their typeinfos
	% unify pred address mapped to a shape number, so we can do
	% that at the same time.

:- pred shapes__do_abstract_exports(module_info, module_info).
:- mode shapes__do_abstract_exports(in, out) is det.

:- pred shapes__write_shape_num(shape_num, io__state, io__state).
:- mode shapes__write_shape_num(in, di, uo) is det.

:- implementation.

:- import_module hlds_data, llds, prog_data, type_util.
:- import_module int, assoc_list, map, std_util, require.

:- type bit_number --->	bit_zero; bit_one; bit_two; bit_three.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% PUBLIC PREDICATES:
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Initialization is done rather simply.
%
% We manually insert some standard builtin types. 
% We also insert types that are defined in a special way (eg, univ
% io__stream, and io__external_state). 
% Since these types have an abstract definition, we also need to module 
% qualify them. 
%
% (string, float, int, character etc don't even have a :- type ....
% anywhere).
%
% These and cases need to be treated specially at runtime.
%-----------------------------------------------------------------------------%
shapes__init_shape_table((S_Tab_Out - S_Num)) :-
	Const = quad(constant, constant, constant, constant),
	I = ground(shared, no),
	construct_type(unqualified("string") - 0, [], StrType),
	construct_type(unqualified("float") - 0, [], FloatType),
	construct_type(unqualified("int") - 0, [], IntType),
	construct_type(unqualified("character") - 0, [], CharType),
	construct_type(qualified("io", "io__stream") - 0, [], IoType),
	construct_type(qualified("std_util", "univ") - 0, [], UnivType),
	construct_type(qualified("io", "io__external_state") - 0, 
		[], IoExtType),

	Builtins = [(StrType - I) - 	(builtin(0) - Const),
		(FloatType - I) - 	(builtin(1) - Const),
		(IntType - I) - 	(builtin(2) - Const),
		(CharType - I) - 	(builtin(3) - Const),
		(IoType - I) - 		(builtin(4) - Const),
		(UnivType - I) - 	(builtin(5) - Const),
		(IoExtType - I) -	(builtin(6) - Const)
	],
	map__from_assoc_list(Builtins, S_Tab_Out),
	S_Num = 7.			% Next shape number to be assigned.

%-----------------------------------------------------------------------------%
% Creation of the shape table allows shapes to be uniquely numbered.
% Later, this information will be used to create the shape lists.
% We only deal well with ground shapes, partial insts and free insts
% may need some modification.
%-----------------------------------------------------------------------------%
shapes__request_shape_number(ShapeId0, Type_Table, S_Tab0 - Next_S_Num0,
				S_Tab - NextNum, S_Num) :-
	shapes__replace_context(ShapeId0, ShapeId),
	(
		map__contains(S_Tab0, ShapeId)
	->
		map__lookup(S_Tab0, ShapeId, (Shape_Num - _)),
		S_Tab = S_Tab0,
		NextNum = Next_S_Num0,
		( 
			Shape_Num = num(_) 
		-> 
			Shape_Num = num(S_Num) 
		;	
		  	Shape_Num = builtin(_) 
		->
			Shape_Num = builtin(S_Num) 
		;
			error("shapes: Unexpected shape_num type found")
		)
	;
		Next_S_Num1 is Next_S_Num0 + 1,
		S_Num = Next_S_Num0,
	% Avoid infinite recursion by inserting a 'dummy' shape
	% so that if the shape is self-referential, it doesn't
	% cause trouble.
		map__set(S_Tab0, ShapeId, num(Next_S_Num0) - quad(constant,
			constant, constant, constant), S_Tab1),
		shapes__create_shape(Type_Table, ShapeId, Shape,
			S_Tab1 - Next_S_Num1, S_Tab2 - NextNum),
		map__set(S_Tab2, ShapeId, num(Next_S_Num0) - Shape, S_Tab)
	).

shapes__do_abstract_exports(HLDS0, HLDS) :-
	module_info_types(HLDS0, Types),
	module_info_shape_info(HLDS0, Shape_Info),
	Shape_Info = shape_info(Shapes, Abs_Exports, _PredShapes),
	map__to_assoc_list(Abs_Exports, Export_List),
	shapes__add_shape_numbers(Export_List, Types, Shapes, Shapes2,
			 Export_List2),
	map__from_assoc_list(Export_List2, Abs_Exports2),

	module_info_name(HLDS0, ModuleName),
	shapes__create_special_preds(Export_List2, ModuleName, 
			SpecialPredShapes),

	Shape_Info_2 = shape_info(Shapes2, Abs_Exports2, SpecialPredShapes),
	module_info_set_shape_info(HLDS0, Shape_Info_2, HLDS).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% LOCAL PREDICATES:
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


	% Create a mapping from unify pred label name to shape number.
	% The code to generate label names will have to be kept in sync
	% with however they are actually created.

	% Label = special_proc(ModuleName, "__Unify__", TypeName, Arity,
	%	UniModeNum).
:- pred shapes__create_special_preds(assoc_list(type_id, maybe_shape_num), 
					string, map(label, shape_num)).
:- mode shapes__create_special_preds(in, in, out) is det.

shapes__create_special_preds([], _ModuleName, SpecialPredShapes) :-
	map__init(SpecialPredShapes).
shapes__create_special_preds([L | Ls], ModuleName, SpecialPredShapes) :-
	shapes__create_special_preds(Ls, ModuleName, SpecialPredShapes0),
	L = TypeId - MaybeShapeNum,
	TypeId = TypeSymName - Arity,
	(
		TypeSymName = unqualified(_TypeName),
		Label = local(special_proc(
			ModuleName, "__Unify__", TypeSymName, Arity, 1))
	;
			% Don't think this will even happen...
		TypeSymName = qualified(ModuleSpec, _TypeName),
		Label = local(special_proc(
			ModuleSpec, "__Unify__", TypeSymName, Arity, 1))
	),
	(
		MaybeShapeNum = yes(ShapeNum)
	->
		map__set(SpecialPredShapes0, Label, ShapeNum, 
			SpecialPredShapes)
	;
		error("shapes: unable to find shape number for special pred")
	).

%-----------------------------------------------------------------------------%

:- pred shapes__add_shape_numbers(assoc_list(type_id, maybe_shape_num),
				type_table, shape_table, shape_table,
				assoc_list(type_id, maybe_shape_num)).
:- mode shapes__add_shape_numbers(in, in, in, out, out) is det.

shapes__add_shape_numbers([], _, ShapeTab, ShapeTab, []).
shapes__add_shape_numbers([T - S | Ts] , Types, ShapeTab0, ShapeTab,
			[ N | Ns] ) :-
	shapes__add_shape_numbers(Ts, Types, ShapeTab0, ShapeTab1, Ns),
	(
		S = yes(_)
	->
		N = T - S,
		ShapeTab = ShapeTab1
	;	
		S = no(Type)
	->
		shapes__request_shape_number(Type - ground(shared, no), Types,
 			ShapeTab1, ShapeTab, S_Num),
		N = T - yes(num(S_Num))
	;
		error("shapes__add_shape_numbers: Unreachable case reached!")
	).

%-----------------------------------------------------------------------------%
% We want to 'remove' the context of the types that we lookup and deal with
% in the shape table.
%-----------------------------------------------------------------------------%
:- pred shapes__replace_context(shape_id, shape_id).
:- mode shapes__replace_context(in, out) is det.
shapes__replace_context(Type - Inst, NewType - Inst) :-
	shapes__replace_all_contexts([Type], NewTypes),
	(
		NewTypes = [ _Type | [] ]
	->
		NewTypes = [ NewType | _ ]
	;
		error("shapes__replace_context - empty list returned")
	).

%-----------------------------------------------------------------------------%
% Want to also remove all type argument contexts, recursively.
%-----------------------------------------------------------------------------%
:- pred shapes__replace_all_contexts(list(type), list(type)).
:- mode shapes__replace_all_contexts(in, out) is det.
shapes__replace_all_contexts([], []).
shapes__replace_all_contexts([Type | TRest], [ NewType | NewRest ]) :-
	(
		Type = term__functor(C, Ts, _)
	->
		shapes__replace_all_contexts(Ts, Ns),
		term__context_init(Init),
		NewType = term__functor(C, Ns, Init)
	;
		NewType = Type
	),
	shapes__replace_all_contexts(TRest, NewRest).

:- pred shapes__replace_all_contexts_in_ctor_args(list(constructor_arg),
		list(type)).
:- mode shapes__replace_all_contexts_in_ctor_args(in, out) is det.
shapes__replace_all_contexts_in_ctor_args([], []).
shapes__replace_all_contexts_in_ctor_args([_Name - Type | TRest],
		[NewType | NewRest]) :-
	(
		Type = term__functor(C, Ts, _)
	->
		shapes__replace_all_contexts(Ts, Ns),
		term__context_init(Init),
		NewType = term__functor(C, Ns, Init)
	;
		NewType = Type
	),
	shapes__replace_all_contexts_in_ctor_args(TRest, NewRest).
%-----------------------------------------------------------------------------%
% To create each shape, we want to group the types on bit tags, eg all
% those with tag 0 are represented by the first part of the quad, all
% those with tag 1 are represented by the second part etc...
%-----------------------------------------------------------------------------%
:- pred shapes__tag_match(bit_number, int).
:- mode shapes__tag_match(in, in) is semidet.

shapes__tag_match(bit_zero, 0).
shapes__tag_match(bit_one, 1).
shapes__tag_match(bit_two, 2).
shapes__tag_match(bit_three, 3).

%-----------------------------------------------------------------------------%
% Create a shape (the structural information of the shape).
%-----------------------------------------------------------------------------%
:- pred shapes__create_shape(type_table, shape_id, shape, shape_table,
				shape_table).
:- mode shapes__create_shape(in, in, out, in, out) is det.

shapes__create_shape(Type_Tab, Shape_Id, Shape, S_Tab0, S_Tab) :-
	Shape_Id = Type - Inst,
	(
		type_to_type_id(Type, Type_Id, TypeArgs)
	->
		shapes__create_shape_2(Type_Tab, Type, Inst, Type_Id, TypeArgs,
				Shape, S_Tab0, S_Tab)
	;
		(
			Type = term__variable(Var)
		->
			term__var_to_int(Var, VarInt),
			Shape = polymorphic(Type, VarInt),
			S_Tab = S_Tab0
		;
			error("shapes: unexpected term")
		)
	).



%-----------------------------------------------------------------------------%
% XXX Should we create shapes for the abstract shape arguments. I think so!
% XXX What happens when the abstract shape refers to a local shape as an
% XXX argument? At link time there is no entry for that shape, so KABOOM.
%-----------------------------------------------------------------------------%
:- pred shapes__create_shape_2(type_table, type, inst, type_id, list(type),
				shape, shape_table, shape_table).
:- mode shapes__create_shape_2(in, in, in, in, in, out, in, out) is det.

shapes__create_shape_2(Type_Tab, Type, Inst, Type_Id, TypeArgs, Shape,
				S_Tab0, S_Tab) :-
	(
		map__search(Type_Tab, Type_Id, Hlds_Type)
	->
		hlds_data__get_type_defn_tparams(Hlds_Type, TypeParams),
		hlds_data__get_type_defn_body(Hlds_Type, Body),
		(
			Body = du_type(Ctors0, TagVals, _)
		->
			term__term_list_to_var_list(TypeParams,
				TypeParamVars),
			map__from_corresponding_lists(TypeParamVars,
				TypeArgs, TypeSubst),
			shapes__apply_to_ctors(Ctors0, TypeSubst,
				Ctors),

			% check for a type with only one functor of arity one:
			% such a type will have a `no_tag' functor
			% (unless it is type_info/1)
			(	Ctors = [SingleCtor - [_Name - SingleArgType]],
				SingleCtor \= qualified(_, "type_info"),
				SingleCtor \= unqualified("type_info")
			->
				% the shape is just the shape of the argument
				% we just need to figure out its inst,
				% and then recursively call shapes__create_shape
				( 	
					Inst = bound(_, [SingleInst]),
					SingleInst = functor(_,
							[SingleArgInst0])
				->
					SingleArgInst = SingleArgInst0
				;	
					% must have been `free' or `ground',
					% etc., so inst of arg is the same
					SingleArgInst = Inst
				),
				shapes__request_shape_number(
					SingleArgType - SingleArgInst,
					Type_Tab,
					S_Tab0, S_Tab, ShapeNum),
				Shape = equivalent(num(ShapeNum))
			;
				Shape = quad(A,B,C,D),
				shapes__create_shapeA(Type_Id, Ctors, TagVals,
					bit_zero, A, Type_Tab, S_Tab0, S_Tab1),
				shapes__create_shapeA(Type_Id, Ctors, TagVals,
					bit_one, B, Type_Tab, S_Tab1, S_Tab2),
				shapes__create_shapeA(Type_Id, Ctors, TagVals,
					bit_two, C, Type_Tab, S_Tab2, S_Tab3),
				shapes__create_shapeA(Type_Id, Ctors, TagVals,
					bit_three, D, Type_Tab, S_Tab3, S_Tab)
			)
		;
			Body = abstract_type

				% An abstract type that is imported from
				% elsewhere. We find the constructors of
				% this type only at linktime, but we
				% store the types of the arguments now.
		->
			(
				type_to_type_id(Type, _, TypeArgs)
			->
				shapes__lookup_simple_info(TypeArgs,
					ShapeList, Type_Tab, S_Tab0, S_Tab),

				shapes__get_snums(ShapeList, SNums),
				Shape = abstract(Type, SNums)
			;
				Shape = abstract(Type, []),
				S_Tab = S_Tab0
			)
		;
			Body = eqv_type(ET)

			% The case where a type is equivalent to
			% another type - we just find the type it is
			% equivalent to and store it as a reference.
		->
			shapes__replace_context(ET - ground(shared, no),
				EqvType - G),
			shapes__request_shape_number(EqvType - G,
				Type_Tab, S_Tab0, S_Tab, EqvShapeNum),
			Shape = equivalent(num(EqvShapeNum))
		;
			error("shapes__create_shape_2: unknown type")
		)
	;
		type_is_higher_order(Type, _, _)
	->
		S_Tab = S_Tab0,
		Shape = closure(Type)
	;
		error("shapes__create_shape_2: not in type table")
	).

:- pred shapes__get_snums(list(pair(shape_num, shape_id)), list(shape_num)).
:- mode shapes__get_snums(in, out) is det.

shapes__get_snums([], []).
shapes__get_snums([N - _I | NIs], [N | Ns]) :-
	shapes__get_snums(NIs, Ns).

:- pred shapes__apply_to_ctors(list(constructor), tsubst, list(constructor)).
:- mode shapes__apply_to_ctors(in, in, out) is det.

shapes__apply_to_ctors([], _, []).
shapes__apply_to_ctors([Ctor0 | Ctors0], Subst, [Ctor | Ctors]) :-
	Ctor0 = SymName - Args0,
	shapes__apply_to_ctor_args(Args0, Subst, Args),
	Ctor = SymName - Args,
	shapes__apply_to_ctors(Ctors0, Subst, Ctors).

:- pred shapes__apply_to_ctor_args(list(constructor_arg), tsubst,
		list(constructor_arg)).
:- mode shapes__apply_to_ctor_args(in, in, out) is det.

shapes__apply_to_ctor_args([], _, []).
shapes__apply_to_ctor_args([Name - Type0 | Args0], Subst,
		[Name - Type | Args]) :-
	term__apply_substitution(Type0, Subst, Type),
	shapes__apply_to_ctor_args(Args0, Subst, Args).

%-----------------------------------------------------------------------------%
% We pass seperate the head from the rest as we are going to want to
% match it against many cases.
% We traverse the list until we come to a case that matches out Type_Id,
% and the tag bit we are interested in. If there are none, it is an unused
% tag and we call it constant.
%-----------------------------------------------------------------------------%
:- pred shapes__create_shapeA(type_id, list(constructor), cons_tag_values,
		bit_number, shape_tag, type_table, shape_table, shape_table).
:- mode shapes__create_shapeA(in, in, in, in, out, in, in, out) is det.

shapes__create_shapeA(_, [], _, _, constant, _, S_Tab, S_Tab).
shapes__create_shapeA(Type_Id, [ Ctor | Rest ] , TagVals, Bits, A,
			Type_Table, S_Tab0, S_Tab) :-
	Ctor = Symname - Args,
	shapes__make_cons_id(Symname, Args, C_Id),
	map__lookup(TagVals, C_Id, C_Tag),
	(
		C_Tag = string_constant(_)
	->
		A = constant,
		S_Tab = S_Tab0
	;
		C_Tag = float_constant(_)
	->
		A = constant,
		S_Tab = S_Tab0
	;
		C_Tag = int_constant(_)
	->
		A = constant,
		S_Tab = S_Tab0
	;
		C_Tag = code_addr_constant(_, _)
	->
		A = constant,
		S_Tab = S_Tab0
	;	
		C_Tag = pred_closure_tag(_, _)
	->
		A = constant,
		S_Tab = S_Tab0
	;	
		C_Tag = no_tag
	->
		% no_tag functors should be handled above, in
		% shapes__create_shape_2
		error("shapes__create_shapesA: unexpected `no_tag'")
	;	
		C_Tag = simple_tag(X),
		shapes__tag_match(Bits, X)
	->
		shapes__replace_all_contexts_in_ctor_args(Args, NewArgs),
		shapes__lookup_simple_info(NewArgs, Shapes_Ids,
					Type_Table, S_Tab0, S_Tab),
		A = simple(Shapes_Ids)
	;
		C_Tag = complicated_tag(X, _Y),
		shapes__tag_match(Bits, X)
	->
		shapes__lookup_complicated_info( [Ctor | Rest], TagVals,
			Bits, Ls, Type_Table, S_Tab0, S_Tab),
		A = complicated(Ls)
	;
		C_Tag = complicated_constant_tag(X, _Y),
		shapes__tag_match(Bits, X)
	->
		A = constant,
		S_Tab = S_Tab0
	;
		shapes__create_shapeA(Type_Id, Rest, TagVals, Bits, A,
					Type_Table, S_Tab0, S_Tab)
		% Where nothing matches up, go on down the list.
	).


%-----------------------------------------------------------------------------%
% Want to find the list of shape_ids that are arguments to the simple
% tagged type.
%-----------------------------------------------------------------------------%
:- pred shapes__lookup_simple_info(list(type), list(pair(shape_num, shape_id)),
		type_table, shape_table, shape_table).
:- mode shapes__lookup_simple_info(in, out, in, in, out) is det.

shapes__lookup_simple_info([], [], _, S_Tab, S_Tab).
shapes__lookup_simple_info([ Arg | Args], [ num(Num) - S | ShapeIds],
				Type_Table, S_Tab0, S_Tab) :-
	S = Arg - ground(shared, no),
	shapes__request_shape_number(S, Type_Table, S_Tab0, S_Tab1, Num),
	shapes__lookup_simple_info(Args, ShapeIds, Type_Table, S_Tab1, S_Tab).

%-----------------------------------------------------------------------------%
% Since complicated tags are shared by many types, we look up
% all the types that shape, and find all their arguments, and return
% it as a list of lists. Fortunately, this is just a case of calling
% shapes__lookup_simple_info multiple times.
% XXX This code contains a bug. We are adding empty shape_id lists to
% the complicated info list. This makes it seem like we are adding
% constants to the complicated tag.
%-----------------------------------------------------------------------------%
:- pred shapes__lookup_complicated_info(list(constructor), cons_tag_values,
		bit_number, list(list(pair(shape_num, shape_id))), type_table,
		shape_table, shape_table).
:- mode shapes__lookup_complicated_info(in, in, in, out, in, in, out) is det.

shapes__lookup_complicated_info([], _, _, [], _, S_Tab, S_Tab).
shapes__lookup_complicated_info([Ctor | Cs], Tagvals, Bits, ShapeIds,
				 Type_Table, S_Tab0, S_Tab) :-
	shapes__lookup_complicated_info(Cs, Tagvals, Bits, Ss,
		Type_Table, S_Tab0, S_Tab1),
	(
		shapes__get_complicated_shapeids(Ctor, Tagvals, Bits, S_Ids,
					Type_Table, S_Tab1, S_TabOut)
	->
		ShapeIds = [S_Ids | Ss],
		S_Tab = S_TabOut
	;
		ShapeIds = Ss,
		S_Tab = S_Tab1
	).

:- pred shapes__get_complicated_shapeids(constructor, cons_tag_values,
			bit_number, list(pair(shape_num, shape_id)),
			type_table, shape_table, shape_table).
:- mode shapes__get_complicated_shapeids(in, in, in, out, in, in, out)
					is semidet.
shapes__get_complicated_shapeids(Ctor, Tagvals, Bits, S_Ids,
					Type_Table, S_Tab0, S_Tab) :-
	Ctor = Symname - Args,
	shapes__make_cons_id(Symname, Args, C_Id),
	map__lookup(Tagvals, C_Id, C_Tag),
	C_Tag = complicated_tag(Primary, _Sec),
	shapes__tag_match(Bits, Primary),
	shapes__replace_all_contexts_in_ctor_args(Args, NewArgs),
	shapes__lookup_simple_info(NewArgs, S_Ids, Type_Table, S_Tab0, S_Tab).

%-----------------------------------------------------------------------------%
% Our tags are created here.
%-----------------------------------------------------------------------------%
:- pred shapes__make_const_tag(shape_num, tagged_num).
:- mode shapes__make_const_tag(in, out) is det.

:- pred shapes__make_simple_tag(shape_num, tagged_num).
:- mode shapes__make_simple_tag(in, out) is det.

:- pred shapes__make_complicated_tag(shape_num, tagged_num).
:- mode shapes__make_complicated_tag(in, out) is det.

shapes__make_const_tag(X, (X - const)).
shapes__make_simple_tag(X, (X - simple)).
shapes__make_complicated_tag(X, (X - complicated)).

%-----------------------------------------------------------------------------%
% An interface to make_cons_id.
%-----------------------------------------------------------------------------%
:- pred shapes__make_cons_id(sym_name, list(constructor_arg), cons_id).
:- mode shapes__make_cons_id(in, in, out) is det.
shapes__make_cons_id(Sym, Args, C_Id) :-
	make_cons_id(Sym, Args, unqualified("X") - 0, C_Id).

%-----------------------------------------------------------------------------%
%
%-----------------------------------------------------------------------------%
shapes__write_shape_num(num(Number)) -->
	io__write_string("num("),
	io__write_int(Number),
	io__write_string(")").
shapes__write_shape_num(builtin(Number)) --> 
	io__write_string("builtin("),
	io__write_int(Number),
	io__write_string(")").
shapes__write_shape_num(succip) --> io__write_string("succip").
shapes__write_shape_num(hp) --> io__write_string("hp").
shapes__write_shape_num(maxfr) --> io__write_string("maxfr").
shapes__write_shape_num(curfr) --> io__write_string("curfr").
shapes__write_shape_num(redoip) --> io__write_string("redoip").
shapes__write_shape_num(sp) --> io__write_string("sp").
shapes__write_shape_num(unwanted) --> io__write_string("unwanted").
shapes__write_shape_num(prevfr) --> io__write_string("prevfr").
shapes__write_shape_num(succfr) --> io__write_string("succfr").
shapes__write_shape_num(ticket) --> io__write_string("ticket").
