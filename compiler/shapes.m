%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% File: shapes.nl
% Main author: trd 
%
% This module creates the shape table information in a C
% readable format.
%
% The data is intendend to be written into a machine-independent
% file of offsets, which will be demand loaded by the program when
% garbage collection is required.
%
% shapes__request_shape_number is called during code generation
% and gathers information about the shape if it isn't already within the
% table.
%
% Abstract types are presently 'ingored' (they are treated as constants),
% as are type variables.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module shapes.
:- interface.

:- import_module int, map, std_util, list, io, hlds, require,
		 prog_io, type_util, string, term.

:- type tagged_num	==	pair(shape_num,tag_type).
:- type tag_type	--->    const; simple; complicated.

:- type shape_list	==	list(tagged_num).
:- type length_list	==	list(int).
:- type contents_list	==	list(int).

:- pred shapes__init_shape_table(shape_table).
:- mode shapes__init_shape_table(out) is det.

:- pred shapes__request_shape_number(type_id, inst, type_table, 
		shape_table, shape_table, shape_num).
:- mode shapes__request_shape_number(in, in, in, in, out, out) is det.

:- pred shapes__construct_shape_lists(shape_table, shape_list, 
		length_list, contents_list). 
:- mode shapes__construct_shape_lists(in, out, out, out) is det. 

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
% problems if the types are redefined in some way...
%-----------------------------------------------------------------------------%
shapes__init_shape_table((S_Tab_Out - S_Num)) :-
	map__init(S_Tab0),
	Const = quad(constant, constant, constant, constant),
	(
		map__insert(S_Tab0, unqualified("string") - 0 - ground,
				0 - Const, S_Tab1),
		map__insert(S_Tab1, unqualified("float") - 0 - ground,
				1 - Const, S_Tab2),
		map__insert(S_Tab2, unqualified("int") - 0 - ground,
				2 - Const, S_Tab3) 
	-> 
		S_Num = 3,
		S_Tab_Out = S_Tab3
	;
		error("shapes: init_shape_table: initialization failure") 
	).

%-----------------------------------------------------------------------------%
% Creation of the shape table allows shapes to be uniquely numbered.
% Later, this information will be used to create the shape lists.
% We only deal well with ground shapes, partial insts and free insts
% may need some modification.
%-----------------------------------------------------------------------------%
shapes__request_shape_number(Type_Id, Inst, Type_Table, S_Tab0 - Next_S_Num0,
				S_Tab - NextNum, S_Num) :-
	ShapeId = Type_Id - Inst,
	(
		map__contains(S_Tab0, ShapeId)  
	-> 
		map__lookup(S_Tab0, ShapeId, (S_Num - _)),
		S_Tab = S_Tab0,
		NextNum = Next_S_Num0
	;
		Next_S_Num1 is Next_S_Num0 + 1,
		S_Num is Next_S_Num0 + 1,
	% Avoid infinite recursion by inserting a 'dummy' shape so that if
	% the shape is self-referential, it doesn't cause trouble.
		map__set(S_Tab0, ShapeId, Next_S_Num1 - quad(constant, 
			constant, constant, constant), S_Tab1),
		shapes__create_shape(Type_Table, Type_Id, Inst, Shape,
				S_Tab1 - Next_S_Num1, S_Tab2 - NextNum),
		map__set(S_Tab2, ShapeId, (Next_S_Num1 - Shape), S_Tab) 
	).

%-----------------------------------------------------------------------------%
% To actually construct the flat lists that are nearly ready for output
% into a file. 
%-----------------------------------------------------------------------------%

%% XXX shapes will be a problem - they are out of order because of map__keys.
shapes__construct_shape_lists(S_Tab, S_List, L_List, C_List) :-
	S_Tab = Shape_Tab - _,
	map__keys(Shape_Tab, Temp_List),
	shapes__make_shape_tag_list(Temp_List, S_Tab, Tag_List),
	shapes__construct_lists(Tag_List, S_Tab, S_List, L_List, _,
		C_List, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% LOCAL PREDICATE DECLARATIONS: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- pred shapes__tag_match(bit_number, int).
:- mode shapes__tag_match(in, in) is semidet.

:- pred shapes__create_shape(type_table, type_id, inst, shape, shape_table,
				shape_table). 
:- mode shapes__create_shape(in, in, in, out, in, out) is det.

:- pred shapes__create_shapeA(type_id, list(constructor), cons_tag_values,
		bit_number, shape_tag, type_table, shape_table, shape_table).
:- mode shapes__create_shapeA(in, in, in, in, out, in, in, out) is det.

:- pred shapes__lookup_simple_info(list(type), list(shape_id), type_table,
					shape_table, shape_table).
:- mode shapes__lookup_simple_info(in, out, in, in, out) is det.

:- pred shapes__lookup_complicated_info(list(constructor), cons_tag_values,
		bit_number, list(list(shape_id)), type_table, shape_table,
		shape_table).
:- mode shapes__lookup_complicated_info(in, in, in, out, in, in, out) is det.

:- pred shapes__make_shape_tag_list(list(shape_id), shape_table,
		list(shape_tag)).
:- mode shapes__make_shape_tag_list(in, in, out) is det.

:- pred shapes__construct_lists(list(shape_tag), shape_table,
		shape_list, length_list, int,  contents_list, int).
:- mode shapes__construct_lists(in, in, out, out, out, out, out) is det.

:- pred shapes__constr_lists_1(shape_tag, list(shape_tag), shape_table,
		shape_list, length_list, int, contents_list, int).
:- mode shapes__constr_lists_1(in, in, in, out, out, out, out, out) is det.

:- pred shapes__constr_lists_2(list(list(shape_id)), shape_table, 
 	       length_list, length_list, int, int, contents_list,
	       contents_list, int, int).
:- mode shapes__constr_lists_2(in, in, in, out, in, out, in, out,
		in, out) is det.

:- pred shapes__constr_lists_3(list(shape_id), shape_table,
	       contents_list, contents_list, int).
:- mode shapes__constr_lists_3(in, in, in, out, out) is det.

:- pred shapes__make_const_tag(shape_num, tagged_num).
:- mode shapes__make_const_tag(in, out) is det.

:- pred shapes__make_simple_tag(shape_num, tagged_num).
:- mode shapes__make_simple_tag(in, out) is det.

:- pred shapes__make_complicated_tag(shape_num, tagged_num).
:- mode shapes__make_complicated_tag(in, out) is det.

:- pred shapes__make_cons_id(sym_name, list(type), cons_id).
:- mode shapes__make_cons_id(in, in, out) is det.
shapes__make_cons_id(Sym, Typelist, C_Id) :- 
	make_cons_id(Sym, Typelist, unqualified("X") - 0, C_Id).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% LOCAL PREDICATE IMPLEMENTATIONS: 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% To create each shape, we want to group the types on bit tags, eg all
% those with tag 0 are represented by the first part of the quad, all
% those with tag 1 are represented by the second part etc...
%-----------------------------------------------------------------------------%
shapes__tag_match(bit_zero, 0).
shapes__tag_match(bit_one, 1).
shapes__tag_match(bit_two, 2).
shapes__tag_match(bit_three, 3).

%-----------------------------------------------------------------------------%
% Create a shape (the structural information of the shape).
%-----------------------------------------------------------------------------%
shapes__create_shape(Type_Tab, Type_Id, Inst, Shape, S_Tab0, S_Tab) :-
	( 
		Inst = free
	->
		Shape = quad(constant, constant, constant, constant),
		S_Tab = S_Tab0
% XXX Free shapes are not really expected here.
	;
		( 
			map__search(Type_Tab, Type_Id, Hlds_Type),
			Hlds_Type = hlds__type_defn(_, _, Type_Body, _, _),
			Type_Body = du_type(Ctors, TagVals, _)
		->
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
			Type_Body = abstract_type
		->
			Shape = quad(constant, constant, constant, constant),
			S_Tab = S_Tab0
		;
			error("shapes: create_shape: not d.u./abstract type or not in type table")
		) 
	).

%-----------------------------------------------------------------------------%
% We pass seperate the head from the rest as we are going to want to
% match it against many cases.
% We traverse the list until we come to a case that matches out Type_Id, 
% and the tag bit we are interested in. If there are none, it is an unused
% tag and we call it constant.
%-----------------------------------------------------------------------------%
shapes__create_shapeA(_, [], _, _, constant, _, S_Tab, S_Tab).
shapes__create_shapeA(Type_Id, [ Ctor | Rest ] , TagVals, Bits, A, Type_Table,
			S_Tab0, S_Tab) :-
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
		C_Tag = pred_constant(_, _) 
	->
		A = constant,
		S_Tab = S_Tab0
	;	
		C_Tag = simple_tag(X),
		shapes__tag_match(Bits, X) 
	->
		shapes__lookup_simple_info(Args, Shapes_Ids, Type_Table, 
					S_Tab0, S_Tab),
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
shapes__lookup_simple_info([], [], _, S_Tab, S_Tab).
shapes__lookup_simple_info([ A | Args], [ S | ShapeIds], Type_Table, S_Tab0,
				 S_Tab) :-

%%% XXX Want to insert the shape into the shape_table, if the shape
%%% is not already present. In other words, we want to request shape
%%% number. This means that shape_table will need to be threaded
%%% down here, as will type_table. 
	shapes__type_to_shapeid(A, S),
	S = Type_Id - _Inst,
	shapes__lookup_simple_info(Args, ShapeIds, Type_Table, S_Tab0, S_Tab1),
        shapes__request_shape_number(Type_Id, ground, Type_Table, S_Tab1,
				     S_Tab, _S_Num).

:- pred shapes__type_to_shapeid(type, shape_id). 
:- mode shapes__type_to_shapeid(in, out) is det. 
shapes__type_to_shapeid(A, S) :- 
	(
		type_to_type_id(A, T_Id, _A_List)
	->
		S = T_Id - ground 
	;
		A = term__variable(_Varnum)
	->
		S = (unqualified("__type_variable__") - -2) - ground 
	;
		error("shapes__arg_to_shapeid : type unknown")
	).


%-----------------------------------------------------------------------------%
% Since complicated tags are shared by many types, we look up
% all the types that shape, and find all their arguments, and return
% it as a list of lists. Fortunately, this is just a case of calling
% shapes__lookup_simple_info multiple times. 
%-----------------------------------------------------------------------------%
shapes__lookup_complicated_info([], _, _, [], _, S_Tab, S_Tab).
shapes__lookup_complicated_info([Ctor | Cs], Tagvals, Bits, [S_Ids | Ss],
				 Type_Table, S_Tab0, S_Tab) :-
	shapes__get_complicated_shapeids(Ctor, Tagvals, Bits, S_Ids,
					Type_Table, S_Tab0, S_Tab1),
	shapes__lookup_complicated_info(Cs, Tagvals, Bits, Ss, Type_Table,
					S_Tab1, S_Tab).

:- pred shapes__get_complicated_shapeids(constructor, cons_tag_values,
					bit_number, list(shape_id), 
					type_table, shape_table, shape_table).
:- mode shapes__get_complicated_shapeids(in, in, in, out, in, in, out) is det.
shapes__get_complicated_shapeids(Ctor, Tagvals, Bits, S_Ids, Type_Table, 
				  S_Tab0, S_Tab) :- 
	Ctor = Symname - Args,
	shapes__make_cons_id(Symname, Args, C_Id),
	map__lookup(Tagvals, C_Id, C_Tag),
	(
		C_Tag = complicated_tag(Primary, _Sec),
		shapes__tag_match(Bits, Primary)
	->
		shapes__lookup_simple_info(Args, S_Ids, Type_Table, S_Tab0,
						S_Tab) 
	;
		S_Ids = [],
		S_Tab = S_Tab0
	).

%-----------------------------------------------------------------------------%
% From a list of shape_ids, create the list of shape tags, as we don't 
% really want to be breaking them into quadruples each time.
%-----------------------------------------------------------------------------%
shapes__make_shape_tag_list([], _S_table, []).
shapes__make_shape_tag_list([Id | Ids], S_tab - X , ST_list) :-
	map__lookup(S_tab, Id, (_Num - Shape)),
	Shape = quad(A,B,C,D),
	ST_list = [A | [B | [C | [D | Rest ]]]],
	shapes__make_shape_tag_list( Ids, S_tab - X, Rest).

%-----------------------------------------------------------------------------%
% Our tags are created here.
%-----------------------------------------------------------------------------%
shapes__make_const_tag(X,(X - const)).
shapes__make_simple_tag(X,(X - simple)).
shapes__make_complicated_tag(X,(X - complicated)).

%-----------------------------------------------------------------------------%
% To construct the lists, want to take off the head to make it
% deterministic. In future, could put the recursion as a common
% part into the contruct_lists predicate, but be careful that
% this doesn't ruin the order. 
%-----------------------------------------------------------------------------%
shapes__construct_lists([], _N_tab, [], [], 0, [], 0).
shapes__construct_lists([X | Rest], N_tab, Ss, Ls, L_Num, Cs, C_Num) :-
	shapes__constr_lists_1(X, Rest, N_tab, Ss, Ls, L_Num, Cs, C_Num).

%-----------------------------------------------------------------------------%
% Choose the case and do the appropriate action.
%-----------------------------------------------------------------------------%
shapes__constr_lists_1(constant, Rest, N_tab, [Const_Tag|Ss], Ls, L_Num,
		Cs, C_Num) :-
	shapes__make_const_tag(0,Const_Tag),
	shapes__construct_lists(Rest, N_tab, Ss, Ls, L_Num, Cs, C_Num). 
shapes__constr_lists_1(simple(S_Ids), I_Rest, N_tab, [Tagged_L_Num | S_Rest],
		[C_Len | L_Rest], L_Num_New, C_New, C_Num_New) :-
	shapes__construct_lists(I_Rest, N_tab, S_Rest, L_Rest, L_Num,
		C_Rest, C_Num),
	shapes__constr_lists_3(S_Ids, N_tab, C_Rest, C_New, C_Len),
	C_Num_New is C_Num + C_Len,
	L_Num_New is L_Num + 1,
	shapes__make_simple_tag(L_Num,Tagged_L_Num).
shapes__constr_lists_1(complicated(S_Id_List), I_Rest, N_tab, [Tagged_L_Num | S_Rest],
	        L_New, L_Num_New, C_New, C_Num_New) :-
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
shapes__constr_lists_3([], _S_tab, Cs_Old, Cs_Old, 0).
shapes__constr_lists_3([S_Id | S_Ids], S_tab, Cs_Old,
		[Shape_Number | Cs], C_Len_New) :-
	shapes__constr_lists_3(S_Ids,  S_tab, Cs_Old, Cs, C_Len),
	C_Len_New is C_Len + 1,
	S_tab = Shape_Tab - _,
        map__lookup(Shape_Tab, S_Id, (Shape_Number - _Shape)).


%%:- pred test_data(shape_table).
%%:- mode test_data(out).
%%test_data(X) :-
