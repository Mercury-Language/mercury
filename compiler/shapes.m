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

:- import_module int, map, std_util, list, hlds, require,
		 prog_io, type_util, string, term.

:- type tagged_num	==	pair(shape_num,tag_type).
:- type tag_type	--->    const; simple; complicated.

:- type shape_list	==	list(pair(shape_num, tagged_num)).
:- type length_list	==	list(int).
:- type contents_list	==	list(int).

:- pred shapes__init_shape_table(shape_table).
:- mode shapes__init_shape_table(out) is det.

:- pred shapes__request_shape_number(shape_id, type_table, shape_table, 
			shape_table, shape_num).
:- mode shapes__request_shape_number(in, in, in, out, out) is det.

:- pred shapes__construct_shape_lists(shape_table, shape_list, 
		length_list, contents_list). 
:- mode shapes__construct_shape_lists(in, out, out, out) is det. 

:- pred shapes__do_abstract_exports(module_info, module_info).
:- mode shapes__do_abstract_exports(in, out) is det.

:- implementation.

:- type bit_number --->  bit_zero; bit_one; bit_two; bit_three.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% PUBLIC PREDICATES: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Initialization is done rather simply.
% XXX We manually insert some non-standard builtin types. This may cause
% problems if the types are redefined in some way... These and other
% low numbered cases need to be treated specially at runtime...
% Note : still have to deal with succip etc.
%-----------------------------------------------------------------------------%
shapes__init_shape_table((S_Tab_Out - S_Num)) :-
	map__init(S_Tab0),
	Const = quad(constant, constant, constant, constant),
	term__context_init(TermContext),
	(
		map__insert(S_Tab0, term__functor(term__atom("string"), [],
			TermContext) - ground, 0 - Const, S_Tab1),
		map__insert(S_Tab1, term__functor(term__atom("float"), [],
			TermContext) - ground, 1 - Const, S_Tab2),
		map__insert(S_Tab2, term__functor(term__atom("int"), [],
			TermContext) - ground, 2 - Const, S_Tab3),
		map__insert(S_Tab3, term__functor(term__atom("character"), [],
			TermContext) - ground, 3 - Const, S_Tab4) 
	-> 
		S_Num = 4,
		S_Tab_Out = S_Tab4
	;
		error("shapes: init_shape_table: initialization failure") 
	).

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
		map__lookup(S_Tab0, ShapeId, (S_Num - _)),
		S_Tab = S_Tab0,
		NextNum = Next_S_Num0
	;
		Next_S_Num1 is Next_S_Num0 + 1,
		S_Num is Next_S_Num0 + 1,
	% Avoid infinite recursion by inserting a 'dummy' shape
	% so that if the shape is self-referential, it doesn't
	% cause trouble.
		map__set(S_Tab0, ShapeId, Next_S_Num1 - quad(constant, 
			constant, constant, constant), S_Tab1),
		shapes__create_shape(Type_Table, ShapeId, Shape, 
			S_Tab1 - Next_S_Num1, S_Tab2 - NextNum),
		map__set(S_Tab2, ShapeId, (Next_S_Num1 - Shape), S_Tab) 
	).
%-----------------------------------------------------------------------------%
% To actually construct the flat lists that are nearly ready for output
% into a file. 
%-----------------------------------------------------------------------------%
shapes__construct_shape_lists(S_Tab, S_List, L_List, C_List) :-
	S_Tab = Shape_Tab - _,
	map__values(Shape_Tab, Temp_List),
	list__sort(Temp_List, TS_List),
	shapes__make_shape_tag_list(TS_List, Tag_List),
	shapes__construct_lists(Tag_List, S_Tab, S_List, L_List, _,
		C_List, _).

shapes__do_abstract_exports(HLDS0, HLDS) :-
	module_info_types(HLDS0, Types),
	module_info_shape_info(HLDS0, Shape_Info),
	Shape_Info = shape_info(Shapes, Abs_Exports),
	map__to_assoc_list(Abs_Exports, Export_List),
	shapes__add_shape_numbers(Export_List, Types, Shapes, Shapes2,
			 Export_List2),
	map__from_assoc_list(Export_List2, Abs_Exports2),
	Shape_Info_2 = shape_info(Shapes2, Abs_Exports2),
	module_info_set_shape_info(HLDS0, Shape_Info_2, HLDS).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% LOCAL PREDICATES: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred shapes__add_shape_numbers(assoc_list(type_id, maybe_shape_num),
				type_table, shape_table, shape_table, 
				assoc_list(type_id, maybe_shape_num)).
:- mode shapes__add_shape_numbers(in, in, in, out, out) is det.

shapes__add_shape_numbers([], _, S, S, []).
shapes__add_shape_numbers([T - S | Ts] , Types, S0, S2, [ N | Ns] ) :-
	shapes__add_shape_numbers(Ts, Types, S0, S1, Ns),
	(
		S = yes(_)
	->
		N = T - S,
		S2 = S1
	;	
		S = no(Type)
	->
		shapes__request_shape_number(Type - ground, Types,  
 			S1, S2, S_Num),
		N = T - yes(S_Num)
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
	Shape_Id = Type - _Inst,
	(
		type_to_type_id(Type, Type_Id, TypeArgs) 
	->
		shapes__create_shape_2(Type_Tab, Type, Type_Id, TypeArgs,
				Shape, S_Tab0, S_Tab)
	;
	%% XXX should really check if it is a type variable first.
		Shape = polymorphic(Type),
		S_Tab = S_Tab0
	).

:- pred shapes__create_shape_2(type_table, type, type_id, list(type), shape,
					shape_table, shape_table).
:- mode shapes__create_shape_2(in, in, in, in, out, in, out) is det.

shapes__create_shape_2(Type_Tab, Type, Type_Id, TypeArgs, Shape, 
				S_Tab0, S_Tab) :-
	(
		map__search(Type_Tab, Type_Id, Hlds_Type)
	->
		(
			Hlds_Type = hlds__type_defn(_TypeVarSet, TypeParams,
				du_type(Ctors0, TagVals, _), _, _) 
		->
			term__term_list_to_var_list(TypeParams, TypeParamVars),
			map__from_corresponding_lists(TypeParamVars, TypeArgs,
				TypeSubstitution),
			apply_to_ctors(Ctors0, TypeSubstitution, Ctors),
			
			Shape = quad(A,B,C,D),
			shapes__create_shapeA(Type_Id, Ctors, TagVals,
				bit_zero, A, Type_Tab, S_Tab0, S_Tab1),
			shapes__create_shapeA(Type_Id, Ctors, TagVals,
		 		bit_one, B, Type_Tab, S_Tab1, S_Tab2),
			shapes__create_shapeA(Type_Id, Ctors, TagVals,
		 		bit_two, C, Type_Tab, S_Tab2, S_Tab3),
			shapes__create_shapeA(Type_Id, Ctors, TagVals,
				bit_three, D, Type_Tab, S_Tab3, S_Tab) 
		;
			Hlds_Type = hlds__type_defn(_, _, abstract_type, _, _) 
		% An abstract type that is imported from elsewhere.
		% Later we find the real definition.
		->
			Shape = abstract(Type),
			S_Tab = S_Tab0
		;
			Hlds_Type = hlds__type_defn(_, _, eqv_type(ET), _, _)
		% The case where an abstract type is equivalent to another
 		% abstract type...
		->
			shapes__replace_context(ET - ground, EqvType - _),
			Shape = abstract(EqvType),
			S_Tab = S_Tab0
		;
			error("shapes__create_shape_2: unknown type")
		)
	;
		Type = term__functor(term__atom("pred"), _Vars, _Context) 
	->
		S_Tab = S_Tab0,
		Shape = closure(Type) 
	;
		error("shapes__create_shape_2: not in type table")
	).

:- pred apply_to_ctors(list(constructor), tsubst, list(constructor)).
:- mode apply_to_ctors(in, in, out) is det.

apply_to_ctors([], _, []).
apply_to_ctors([Ctor0 | Ctors0], Subst, [Ctor | Ctors]) :-
	Ctor0 = SymName - ArgTypes0,
	term__apply_substitution_to_list(ArgTypes0, Subst, ArgTypes),
	Ctor = SymName - ArgTypes,
	apply_to_ctors(Ctors0, Subst, Ctors).

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
		C_Tag = address_constant(_, _) 
	->
		A = constant,
		S_Tab = S_Tab0
	;	
		C_Tag = pred_closure_tag(_, _) 
	->
		A = constant,
		S_Tab = S_Tab0
	;	
		C_Tag = simple_tag(X),
		shapes__tag_match(Bits, X) 
	->
		shapes__replace_all_contexts(Args, NewArgs),
		shapes__lookup_simple_info(NewArgs, Shapes_Ids, 
					Type_Table, S_Tab0, S_Tab),
		A = simple(Shapes_Ids) 
	;
		C_Tag = complicated_tag(X, Y),
		shapes__tag_match(Bits, X) 
	->
		shapes__lookup_complicated_info( [Ctor | Rest], TagVals, 
			Bits, Ls, Type_Table, S_Tab0, S_Tab),
		A = complicated(Ls)
	;
		C_Tag = complicated_constant_tag(X, Y),
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
shapes__lookup_simple_info([ Arg | Args], [ S_Num - S | ShapeIds],
				Type_Table, S_Tab0, S_Tab) :-
	S = Arg - ground,
        shapes__request_shape_number(S, Type_Table, S_Tab0, S_Tab1, S_Num),
	shapes__lookup_simple_info(Args, ShapeIds, Type_Table, S_Tab1, S_Tab).

%-----------------------------------------------------------------------------%
% Since complicated tags are shared by many types, we look up
% all the types that shape, and find all their arguments, and return
% it as a list of lists. Fortunately, this is just a case of calling
% shapes__lookup_simple_info multiple times. 
%-----------------------------------------------------------------------------%
:- pred shapes__lookup_complicated_info(list(constructor), cons_tag_values,
		bit_number, list(list(pair(shape_num, shape_id))), type_table, 
		shape_table, shape_table).
:- mode shapes__lookup_complicated_info(in, in, in, out, in, in, out) is det.

shapes__lookup_complicated_info([], _, _, [], _, S_Tab, S_Tab).
shapes__lookup_complicated_info([Ctor | Cs], Tagvals, Bits, [S_Ids | Ss],
				 Type_Table, S_Tab0, S_Tab) :-
	shapes__get_complicated_shapeids(Ctor, Tagvals, Bits, S_Ids,
					Type_Table, S_Tab0, S_Tab1),
	shapes__lookup_complicated_info(Cs, Tagvals, Bits, Ss, 
					Type_Table, S_Tab1, S_Tab).

:- pred shapes__get_complicated_shapeids(constructor, cons_tag_values,
			bit_number, list(pair(shape_num, shape_id)), 
			type_table, shape_table, shape_table).
:- mode shapes__get_complicated_shapeids(in, in, in, out, in, in, out) is det.
shapes__get_complicated_shapeids(Ctor, Tagvals, Bits, S_Ids, 
					Type_Table, S_Tab0, S_Tab) :- 
	Ctor = Symname - Args,
	shapes__make_cons_id(Symname, Args, C_Id),
	map__lookup(Tagvals, C_Id, C_Tag),
	(
		C_Tag = complicated_tag(Primary, _Sec),
		shapes__tag_match(Bits, Primary)
	->
		shapes__replace_all_contexts(Args, NewArgs),
		shapes__lookup_simple_info(NewArgs, S_Ids,  
						Type_Table, S_Tab0, S_Tab) 
	;
		S_Ids = [],
		S_Tab = S_Tab0
	).

%-----------------------------------------------------------------------------%
% From a list of shape_ids, create the list of shape tags and numbers, 
% as we don't really want to be breaking them into quadruples each time.
%-----------------------------------------------------------------------------%
:- pred shapes__make_shape_tag_list(list(pair(shape_num, shape)),  
		list(pair(shape_num, shape_tag))).
:- mode shapes__make_shape_tag_list(in, out) is det.

shapes__make_shape_tag_list([], []).
shapes__make_shape_tag_list([Num - Shape | Ids], ST_list) :-
	shapes__make_shape_tag_list(Ids, Rest),
	(
		Shape = quad(A,B,C,D)
	->
		ST_list = [ Num - A | [ Num - B | 
				[ Num - C | [ Num - D | Rest ]]]]
	;
		error("shapes__make_shape_tag_list : not ready for abstracts, polymorphism, etc")
	).


%-----------------------------------------------------------------------------%
% Our tags are created here.
%-----------------------------------------------------------------------------%
:- pred shapes__make_const_tag(shape_num, tagged_num).
:- mode shapes__make_const_tag(in, out) is det.

:- pred shapes__make_simple_tag(shape_num, tagged_num).
:- mode shapes__make_simple_tag(in, out) is det.

:- pred shapes__make_complicated_tag(shape_num, tagged_num).
:- mode shapes__make_complicated_tag(in, out) is det.

shapes__make_const_tag(X,(X - const)).
shapes__make_simple_tag(X,(X - simple)).
shapes__make_complicated_tag(X,(X - complicated)).

%-----------------------------------------------------------------------------%
% To construct the lists, want the determinism analysis to find this to be
% deterministic, so we take the head of the list off seperately. 
%-----------------------------------------------------------------------------%
:- pred shapes__construct_lists(list(pair(shape_num, shape_tag)), shape_table,
		shape_list, length_list, int,  contents_list, int).
:- mode shapes__construct_lists(in, in, out, out, out, out, out) is det.

shapes__construct_lists([], _N_tab, [], [], 0, [], 0).
shapes__construct_lists([Num - Stag | Rest], N_tab, Ss, Ls, L_Num, Cs, C_Num) :-
	shapes__constr_lists_1(Stag, Num, Rest, N_tab, Ss, Ls, L_Num, 
		Cs, C_Num).

%-----------------------------------------------------------------------------%
% Choose the case and do the appropriate action.
%-----------------------------------------------------------------------------%
:- pred shapes__constr_lists_1(shape_tag, shape_num, 
		list(pair(shape_num, shape_tag)), shape_table, shape_list, 
		length_list, int, contents_list, int).
:- mode shapes__constr_lists_1(in, in, in, in, out, out, out, out, out) is det.

shapes__constr_lists_1(constant, Num, Rest, N_tab, [Num - Const_Tag|Ss],
		  Ls, L_Num, Cs, C_Num) :-
	shapes__construct_lists(Rest, N_tab, Ss, Ls, L_Num, Cs, C_Num),
	shapes__make_const_tag(0,Const_Tag).
shapes__constr_lists_1(simple(S_Ids), Num, I_Rest, N_tab,
		  [Num - Tagged_L_Num | S_Rest], [C_Len | L_Rest], L_Num_New,
		  C_New, C_Num_New) :-
	shapes__construct_lists(I_Rest, N_tab, S_Rest, L_Rest, L_Num,
		C_Rest, C_Num),
	shapes__constr_lists_3(S_Ids, N_tab, C_Rest, C_New, C_Len),
	C_Num_New is C_Num + C_Len,
	L_Num_New is L_Num + 1,
	shapes__make_simple_tag(L_Num,Tagged_L_Num).
shapes__constr_lists_1(complicated(S_Id_List), Num, I_Rest, N_tab, 
		[Num - Tagged_L_Num | S_Rest], L_New, L_Num_New,
		 C_New, C_Num_New) :-
	shapes__construct_lists(I_Rest, N_tab, S_Rest, L_Rest, L_Num,
		C_Rest, C_Num),
	list__reverse(S_Id_List, Rev_S_Id_List),
% Want to reverse, so they end up in an indexable format for easy C indexing.
	shapes__constr_lists_2(Rev_S_Id_List, N_tab, L_Rest, L_New, L_Num, 
		L_Num_New, C_Rest, C_New, C_Num, C_Num_New),
	shapes__make_complicated_tag(L_Num, Tagged_L_Num).

%-----------------------------------------------------------------------------%
% In complicated shape_tags we want to basically do a simple tag 
% case multiple times. 
%-----------------------------------------------------------------------------%
:- pred shapes__constr_lists_2(list(list(pair(shape_num, shape_id))), shape_table, 
 	       length_list, length_list, int, int, contents_list,
	       contents_list, int, int).
:- mode shapes__constr_lists_2(in, in, in, out, in, out, in, out,
		in, out) is det.
shapes__constr_lists_2([], _N_tab, Ls, Ls, L_Num, L_Num, Cs, Cs, C_Num, C_Num).
shapes__constr_lists_2([S_Ids | Rest], N_tab, Ls0, Ls2, L_Num0, L_Num2, Cs0, Cs2,
		C_Num0, C_Num2) :-
        shapes__constr_lists_2(Rest, N_tab, Ls0, Ls1, L_Num0, L_Num1, 
		Cs0, Cs1, C_Num0, C_Num1),
	shapes__constr_lists_3(S_Ids,  N_tab, Cs1, Cs2, C_Len),
        C_Num2 is C_Num1 + C_Len,
	L_Num2 is L_Num1 + 1,
	Ls2 = [C_Len | Ls1 ].

%-----------------------------------------------------------------------------%
% The simple case - put the shape numbers into the contents table.
%-----------------------------------------------------------------------------%
:- pred shapes__constr_lists_3(list(pair(shape_num, shape_id)), shape_table,
	       contents_list, contents_list, int).
:- mode shapes__constr_lists_3(in, in, in, out, out) is det.

% XXX Might not even need the shape table down here now!

shapes__constr_lists_3([], _S_tab, Cs_Old, Cs_Old, 0).
shapes__constr_lists_3([Shape_Number - _ | S_Ids], S_tab, Cs_Old,
		[Shape_Number | Cs], C_Len_New) :-
	shapes__constr_lists_3(S_Ids, S_tab, Cs_Old, Cs, C_Len),
	C_Len_New is C_Len + 1.

%-----------------------------------------------------------------------------%
% An interface to make_cons_id. 
%-----------------------------------------------------------------------------%
:- pred shapes__make_cons_id(sym_name, list(type), cons_id).
:- mode shapes__make_cons_id(in, in, out) is det.
shapes__make_cons_id(Sym, Typelist, C_Id) :- 
	make_cons_id(Sym, Typelist, unqualified("X") - 0, C_Id).

