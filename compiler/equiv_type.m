%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types.

% main author: fjh

:- module equiv_type.
:- interface.
:- import_module bool, list, io, std_util.
:- import_module recompilation, prog_data.

%-----------------------------------------------------------------------------%

	% equiv_type__expand_eqv_types(ModuleName, Items0, Items,
	%	CircularTypes, EqvMap, MaybeRecompInfo0, MaybeRecompInfo).
	%
	% First it builds up a map from type_id to the equivalent type.
	% Then it traverses through the list of items, expanding all types. 
	% This has the effect of eliminating all the equivalence types
	% from the source code. Error messages are generated for any
	% circular equivalence types.
	%
	% For items not defined in the current module, record the
	% equivalence types expanded while processing each item
	% in the recompilation_info. This is needed for smart
	% recompilation.
:- pred equiv_type__expand_eqv_types(module_name, list(item_and_context),
		list(item_and_context), bool, eqv_map,
		maybe(recompilation_info), maybe(recompilation_info),
		io__state, io__state).
:- mode equiv_type__expand_eqv_types(in, in, out, out, out,
		in, out, di, uo) is det.

	% Replace equivalence types in a given type, returning
	% the type_ids of the equivalence types replaced.
:- pred equiv_type__replace_in_type(type, tvarset, eqv_map, type,
		tvarset).
:- mode equiv_type__replace_in_type(in, in, in, out, out) is det.

:- type eqv_map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module assoc_list, bool, require, std_util, map, set, term, varset.
:- import_module prog_data, prog_util, prog_out.

% XXX we shouldn't import the HLDS here.
:- import_module hlds_data, type_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% First we build up a mapping which records the equivalence type
	% definitions.  Then we go through the item list and replace
	% them.

equiv_type__expand_eqv_types(ModuleName, Items0, Items, CircularTypes, EqvMap,
		Info0, Info) -->
	{ map__init(EqvMap0) },
	{ equiv_type__build_eqv_map(Items0, EqvMap0, EqvMap) },
	{ equiv_type__replace_in_item_list(ModuleName, Items0, EqvMap,
		[], RevItems, [], RevCircularTypeList, Info0, Info) },
	{ list__reverse(RevItems, Items) },
	(
		{ RevCircularTypeList = [] }
	->
		{ CircularTypes = no }	
	;
		{ list__reverse(RevCircularTypeList, CircularTypeList) },
		equiv_type__report_circular_types(CircularTypeList),
		{ CircularTypes = yes },
		io__set_exit_status(1)
	).

:- type eqv_type_body ---> eqv_type_body(tvarset, list(type_param), type).
:- type eqv_map == map(type_id, eqv_type_body).

:- pred equiv_type__build_eqv_map(list(item_and_context), eqv_map, eqv_map).
:- mode equiv_type__build_eqv_map(in, in, out) is det.

equiv_type__build_eqv_map([], EqvMap, EqvMap).
equiv_type__build_eqv_map([Item - _Context | Items], EqvMap0, EqvMap) :-
	( Item = type_defn(VarSet, Name, Args, eqv_type(Body), _Cond) ->
		list__length(Args, Arity),
		map__set(EqvMap0, Name - Arity,
			eqv_type_body(VarSet, Args, Body), EqvMap1)
	;
		EqvMap1 = EqvMap0
	),
	equiv_type__build_eqv_map(Items, EqvMap1, EqvMap).

	% The following predicate equiv_type__replace_in_item_list
	% performs substititution of equivalence types on a list
	% of items.  Similarly the replace_in_<foo> predicates that
	% follow perform substitution of equivalence types on <foo>s.

:- pred equiv_type__replace_in_item_list(module_name, list(item_and_context),
	eqv_map, list(item_and_context), list(item_and_context),
	list(item_and_context), list(item_and_context),
	maybe(recompilation_info), maybe(recompilation_info)).
:- mode equiv_type__replace_in_item_list(in, in, in, in, out,
	in, out, in, out) is det.

equiv_type__replace_in_item_list(_, [], _, Items, Items,
		Circ, Circ, Info, Info).
equiv_type__replace_in_item_list(ModuleName, [ItemAndContext0 | Items0],
		EqvMap, ReplItems0, ReplItems, Circ0, Circ, Info0, Info) :-
	ItemAndContext0 = Item0 - Context,
	(
		equiv_type__replace_in_item(ModuleName, Item0, EqvMap, Item,
			ContainsCirc, Info0, Info1)
	->
		Info2 = Info1,
		ItemAndContext = Item - Context,
		( ContainsCirc = yes ->
			Circ1 = [ItemAndContext | Circ0]
		;
			Circ1 = Circ0
		)
	;
		ItemAndContext = ItemAndContext0,
		Circ1 = Circ0,
		Info2 = Info0
	),
	ReplItems1 = [ItemAndContext | ReplItems0],
	equiv_type__replace_in_item_list(ModuleName, Items0, EqvMap,
		ReplItems1, ReplItems, Circ1, Circ, Info2, Info).

:- pred equiv_type__replace_in_item(module_name, item, eqv_map, item, bool,
		maybe(recompilation_info), maybe(recompilation_info)).
:- mode equiv_type__replace_in_item(in, in, in, out, out, in, out) is semidet.

equiv_type__replace_in_item(ModuleName,
		type_defn(VarSet0, Name, TArgs, TypeDefn0, Cond),
		EqvMap, type_defn(VarSet, Name, TArgs, TypeDefn, Cond),
		ContainsCirc, Info0, Info) :-
	list__length(TArgs, Arity),
	equiv_type__maybe_record_used_equivalences(ModuleName, Name,
		Info0, UsedTypeIds0),
	equiv_type__replace_in_type_defn(Name - Arity, TypeDefn0,
		VarSet0, EqvMap, TypeDefn, VarSet, ContainsCirc,
		UsedTypeIds0, UsedTypeIds),
	equiv_type__finish_recording_used_equivalences(
		item_id(type_body, Name - Arity), UsedTypeIds, Info0, Info).

equiv_type__replace_in_item(ModuleName,
		pred_or_func(TypeVarSet0, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes0, Det, Cond,
			Purity, ClassContext0),
		EqvMap,
		pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes, Det, Cond,
			Purity, ClassContext),
		no, Info0, Info) :-
	list__length(TypesAndModes0, Arity),
	equiv_type__maybe_record_used_equivalences(ModuleName, PredName,
		Info0, UsedTypeIds0),
	equiv_type__replace_in_class_constraints(ClassContext0, TypeVarSet0, 
		EqvMap, ClassContext, TypeVarSet1, UsedTypeIds0, UsedTypeIds1),
	equiv_type__replace_in_tms(TypesAndModes0, TypeVarSet1, EqvMap, 
		TypesAndModes, TypeVarSet, UsedTypeIds1, UsedTypeIds),
	ItemType = pred_or_func_to_item_type(PredOrFunc),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	equiv_type__finish_recording_used_equivalences(
		item_id(ItemType, PredName - OrigArity),
		UsedTypeIds, Info0, Info).

equiv_type__replace_in_item(ModuleName,
			typeclass(Constraints0, ClassName, Vars, 
				ClassInterface0, VarSet0),
			EqvMap,
			typeclass(Constraints, ClassName, Vars, 
				ClassInterface, VarSet),
			no, Info0, Info) :-
	list__length(Vars, Arity),
	equiv_type__maybe_record_used_equivalences(ModuleName, ClassName,
		Info0, UsedTypeIds0),
	equiv_type__replace_in_class_constraint_list(Constraints0, VarSet0, 
		EqvMap, Constraints, VarSet, UsedTypeIds0, UsedTypeIds1),
	(
		ClassInterface0 = abstract,
		ClassInterface = abstract,
		UsedTypeIds = UsedTypeIds1
	;
		ClassInterface0 = concrete(Methods0),
		equiv_type__replace_in_class_interface(Methods0,
			EqvMap, Methods, UsedTypeIds1, UsedTypeIds),
		ClassInterface = concrete(Methods)
	),
	equiv_type__finish_recording_used_equivalences(
		item_id(typeclass, ClassName - Arity),
		UsedTypeIds, Info0, Info).

equiv_type__replace_in_item(ModuleName,
			instance(Constraints0, ClassName, Ts0, 
				InstanceBody, VarSet0, ModName),
			EqvMap,
			instance(Constraints, ClassName, Ts, 
				InstanceBody, VarSet, ModName),
			no, Info0, Info) :-
	( (Info0 = no ; ModName = ModuleName) ->
		UsedTypeIds0 = no
	;	
		UsedTypeIds0 = yes(ModuleName - set__init)
	),
	equiv_type__replace_in_class_constraint_list(Constraints0, VarSet0, 
		EqvMap, Constraints, VarSet1, UsedTypeIds0, UsedTypeIds1),
	equiv_type__replace_in_type_list(Ts0, VarSet1, EqvMap, Ts, VarSet, _,
		UsedTypeIds1, UsedTypeIds),
	list__length(Ts0, Arity),
	equiv_type__finish_recording_used_equivalences(
		item_id(typeclass, ClassName - Arity),
		UsedTypeIds, Info0, Info).

equiv_type__replace_in_item(ModuleName,
		pragma(type_spec(PredName, B, Arity, D, E,
			Subst0, VarSet0, TypeIds0)),
		EqvMap,
		pragma(type_spec(PredName, B, Arity, D, E,
			Subst, VarSet, TypeIds)),
		no, Info, Info) :-
	( (Info = no ; PredName = qualified(ModuleName, _)) ->
		UsedTypeIds0 = no
	;	
		UsedTypeIds0 = yes(ModuleName - TypeIds0)
	),
	equiv_type__replace_in_subst(Subst0, VarSet0, EqvMap, Subst, VarSet,
		UsedTypeIds0, UsedTypeIds),
	(
		UsedTypeIds = no,
		TypeIds = TypeIds0
	;
		UsedTypeIds = yes(_ - TypeIds)
	).

:- pred equiv_type__replace_in_type_defn(type_id, type_defn, tvarset,
		eqv_map, type_defn, tvarset, bool,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_defn(in, in, in, in, out, out, out,
		in, out) is semidet.

equiv_type__replace_in_type_defn(TypeId, eqv_type(TBody0),
		VarSet0, EqvMap, eqv_type(TBody),
		VarSet, ContainsCirc, Info0, Info) :-
	equiv_type__replace_in_type_2(TBody0, VarSet0, EqvMap, [TypeId],
		TBody, VarSet, ContainsCirc, Info0, Info).

equiv_type__replace_in_type_defn(_, du_type(TBody0,
		EqPred), VarSet0, EqvMap, du_type(TBody, EqPred),
		VarSet, no, Info0, Info) :-
	equiv_type__replace_in_du(TBody0, VarSet0, EqvMap, TBody,
		VarSet, Info0, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_class_constraints(class_constraints, 
		tvarset, eqv_map, class_constraints, tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_constraints(in, in, in, out, out,
		in, out) is det.

equiv_type__replace_in_class_constraints(Cs0, VarSet0, EqvMap, Cs, VarSet,
		Info0, Info) :-
	Cs0 = constraints(UnivCs0, ExistCs0),
	Cs = constraints(UnivCs, ExistCs),
	equiv_type__replace_in_class_constraint_list(UnivCs0, VarSet0, EqvMap,
		UnivCs, VarSet1, Info0, Info1),
	equiv_type__replace_in_class_constraint_list(ExistCs0, VarSet1, EqvMap,
		ExistCs, VarSet, Info1, Info).

:- pred equiv_type__replace_in_class_constraint_list(list(class_constraint), 
		tvarset, eqv_map, list(class_constraint), tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_constraint_list(in, in, in,
		out, out, in, out) is det.

equiv_type__replace_in_class_constraint_list([], VarSet, _, [], VarSet,
		Info, Info).
equiv_type__replace_in_class_constraint_list([C0|C0s], VarSet0, EqvMap, 
		[C|Cs], VarSet, Info0, Info) :-
	equiv_type__replace_in_class_constraint(C0, VarSet0, EqvMap, C,
		VarSet1, Info0, Info1),
	equiv_type__replace_in_class_constraint_list(C0s, VarSet1, EqvMap, Cs, 
		VarSet, Info1, Info).

:- pred equiv_type__replace_in_class_constraint(class_constraint, tvarset, 
	eqv_map, class_constraint, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_constraint(in, in, in,
	out, out, in, out) is det.

equiv_type__replace_in_class_constraint(Constraint0, VarSet0, EqvMap,
		Constraint, VarSet, Info0, Info) :-
	Constraint0 = constraint(ClassName, Ts0),
	equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap, Ts, VarSet, _,
		Info0, Info),
	Constraint = constraint(ClassName, Ts).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_class_interface(list(class_method), eqv_map,
	list(class_method), equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_interface(in, in, out, in, out) is det.

equiv_type__replace_in_class_interface(ClassInterface0, EqvMap,
		ClassInterface, Info0, Info) :-
	list__map_foldl(equiv_type__replace_in_class_method(EqvMap),
		ClassInterface0, ClassInterface, Info0, Info).

:- pred equiv_type__replace_in_class_method(eqv_map, class_method, 
		class_method, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_method(in, in, out, in, out) is det.

equiv_type__replace_in_class_method(EqvMap,
		pred_or_func(TypeVarSet0, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes0, Det, Cond, Purity,
			ClassContext0, Context),
		pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes, Det, Cond, Purity,
			ClassContext, Context),
		Info0, Info) :-
	equiv_type__replace_in_class_constraints(ClassContext0, TypeVarSet0, 
		EqvMap, ClassContext, TypeVarSet1, Info0, Info1),
	equiv_type__replace_in_tms(TypesAndModes0, TypeVarSet1, EqvMap, 
		TypesAndModes, TypeVarSet, Info1, Info).

equiv_type__replace_in_class_method(_,
		pred_or_func_mode(A,B,C,D,E,F,G),
		pred_or_func_mode(A,B,C,D,E,F,G),
		Info, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_subst(assoc_list(tvar, type), tvarset,
		eqv_map, assoc_list(tvar, type), tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_subst(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_subst([], VarSet, _EqvMap, [], VarSet,
		Info, Info).
equiv_type__replace_in_subst([Var - Type0 | Subst0], VarSet0, EqvMap,
		[Var - Type | Subst], VarSet, Info0, Info) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet1,
		Info0, Info1),
	equiv_type__replace_in_subst(Subst0, VarSet1, EqvMap, Subst, VarSet,
		Info1, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_uu(list(type), tvarset, eqv_map, list(type),
		tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_uu(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_uu(Ts0, VarSet0, EqvMap,
		Ts, VarSet, Info0, Info) :-
	equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap,
		Ts, VarSet, _, Info0, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_du(list(constructor), tvarset, eqv_map,
		list(constructor), tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_du(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_du([], VarSet, _EqvMap, [], VarSet,
		Info, Info).
equiv_type__replace_in_du([T0|Ts0], VarSet0, EqvMap, [T|Ts], VarSet,
		Info0, Info) :-
	equiv_type__replace_in_ctor(T0, VarSet0, EqvMap, T, VarSet1,
		Info0, Info1),
	equiv_type__replace_in_du(Ts0, VarSet1, EqvMap, Ts, VarSet,
		Info1, Info).

:- pred equiv_type__replace_in_ctor(constructor, tvarset, eqv_map, constructor,
		tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctor(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_ctor(ctor(ExistQVars, Constraints0, TName, Targs0),
		VarSet0, EqvMap,
		ctor(ExistQVars, Constraints, TName, Targs), VarSet,
		Info0, Info) :-
	equiv_type__replace_in_ctor_arg_list(Targs0, VarSet0, EqvMap,
		Targs, VarSet1, _, Info0, Info1),
	equiv_type__replace_in_class_constraint_list(Constraints0, VarSet1, 
		EqvMap, Constraints, VarSet, Info1, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_type_list(list(type), tvarset, eqv_map,
		list(type), tvarset, bool, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_list(in, in, in, out, out, out,
		in, out) is det.

equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap,
		Ts, VarSet, ContainsCirc, Info0, Info) :-
	equiv_type__replace_in_type_list_2(Ts0, VarSet0, EqvMap, [],
		Ts, VarSet, no, ContainsCirc, Info0, Info).

:- pred equiv_type__replace_in_type_list_2(list(type), tvarset, eqv_map,
		list(type_id), list(type), tvarset, bool, bool,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_list_2(in, in, in,
		in, out, out, in, out, in, out) is det.

equiv_type__replace_in_type_list_2([], VarSet, _EqvMap, _Seen, [], VarSet,
		ContainsCirc, ContainsCirc, Info, Info).
equiv_type__replace_in_type_list_2([T0 | Ts0], VarSet0, EqvMap, Seen,
		[T | Ts], VarSet, Circ0, Circ, Info0, Info) :-
	equiv_type__replace_in_type_2(T0, VarSet0, EqvMap, Seen,
		T, VarSet1, ContainsCirc, Info0, Info1),
	bool__or(Circ0, ContainsCirc, Circ1),
	equiv_type__replace_in_type_list_2(Ts0, VarSet1, EqvMap, Seen,
		Ts, VarSet, Circ1, Circ, Info1, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_ctor_arg_list(list(constructor_arg), tvarset,
		eqv_map, list(constructor_arg), tvarset, bool,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctor_arg_list(in, in, in, out, out, out,
		in, out) is det.

equiv_type__replace_in_ctor_arg_list(As0, VarSet0, EqvMap,
		As, VarSet, ContainsCirc, Info0, Info) :-
	equiv_type__replace_in_ctor_arg_list_2(As0, VarSet0, EqvMap, [],
		As, VarSet, no, ContainsCirc, Info0, Info).

:- pred equiv_type__replace_in_ctor_arg_list_2(list(constructor_arg), tvarset,
	eqv_map, list(type_id), list(constructor_arg), tvarset, bool, bool,
	equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctor_arg_list_2(in, in, in,
	in, out, out, in, out, in, out) is det.

equiv_type__replace_in_ctor_arg_list_2([], VarSet, _EqvMap, _Seen,
		[], VarSet, ContainsCirc, ContainsCirc,
		Info, Info).
equiv_type__replace_in_ctor_arg_list_2([N - T0 | As0], VarSet0, EqvMap, Seen,
		[N - T | As], VarSet, Circ0, Circ, Info0, Info) :-
	equiv_type__replace_in_type_2(T0, VarSet0, EqvMap, Seen,
		T, VarSet1, ContainsCirc, Info0, Info1),
	bool__or(Circ0, ContainsCirc, Circ1),
	equiv_type__replace_in_ctor_arg_list_2(As0, VarSet1, EqvMap, Seen,
		As, VarSet, Circ1, Circ, Info1, Info).

%-----------------------------------------------------------------------------%

equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet,
		no, _).

:- pred equiv_type__replace_in_type(type, tvarset, eqv_map, type,
		tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet,
		Info0, Info) :-
	equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap,
			[], Type, VarSet, _, Info0, Info).

	% Replace all equivalence types in a given type, detecting  
	% any circularities.
:- pred equiv_type__replace_in_type_2(type, tvarset, eqv_map,
	list(type_id), type, tvarset, bool, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_2(in, in, in, in, out, out, out,
	in, out) is det.

equiv_type__replace_in_type_2(term__variable(V), VarSet, _EqvMap,
		_Seen, term__variable(V), VarSet, no, Info, Info).
equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap, TypeIdsAlreadyExpanded,
		Type, VarSet, Circ, Info0, Info) :- 

	Type0 = term__functor(_, _, _),
	(
		type_to_type_id(Type0, EqvTypeId, TArgs0)
	->
		equiv_type__replace_in_type_list_2(TArgs0, VarSet0, EqvMap,
			TypeIdsAlreadyExpanded, TArgs1, VarSet1, no, Circ0,
			Info0, Info1),

		( list__member(EqvTypeId, TypeIdsAlreadyExpanded) ->
			Circ1 = yes
		;
			Circ1 = no
		),
		(	
			map__search(EqvMap, EqvTypeId,
				eqv_type_body(EqvVarSet, Args0, Body0)),
			%
			% Don't merge in the variable names from the
			% type declaration to avoid creating multiple
			% variables with the same name so that
			% `varset__create_name_var_map' can be used
			% on the resulting tvarset.
			% make_hlds.m uses `varset__create_name_var_map' to
			% match up type variables in `:- pragma type_spec'
			% declarations and explicit type qualifications
			% with the type variables in the predicate's
			% declaration.
			%
			varset__merge_without_names(VarSet1, EqvVarSet,
				[Body0 | Args0], VarSet2, [Body | Args]),
			Circ0 = no,
			Circ1 = no
		->
			map_maybe(equiv_type__record_used_type(EqvTypeId),
				Info1, Info2),
			term__term_list_to_var_list(Args, ArgVars),
			term__substitute_corresponding(ArgVars, TArgs1,
							Body, Type1),
			equiv_type__replace_in_type_2(Type1, VarSet2,
				EqvMap, [EqvTypeId | TypeIdsAlreadyExpanded],
				Type, VarSet, Circ, Info2, Info)
		;
			VarSet = VarSet1,
			Info = Info1,
			construct_type(EqvTypeId, TArgs1, Type),
			bool__or(Circ0, Circ1, Circ)
		)
	;
		VarSet = VarSet0,
		Type = Type0,
		Info = Info0,
		Circ = no
	).

:- pred equiv_type__record_used_type(type_id, pair(module_name, set(type_id)),
		pair(module_name, set(type_id))).
:- mode equiv_type__record_used_type(in, in, out) is det.

equiv_type__record_used_type(TypeId, UsedTypes0, UsedTypes) :-
	UsedTypes0 = ModuleName - Types0,
	( TypeId = qualified(ModuleName, _) - _ ->
		% We don't need to record local types.
		UsedTypes = UsedTypes0
	;
		UsedTypes = ModuleName - set__insert(Types0, TypeId)
	).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_tms(list(type_and_mode), tvarset, eqv_map,
	list(type_and_mode), tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_tms(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_tms([], VarSet, _EqvMap, [], VarSet,
		Info, Info).
equiv_type__replace_in_tms([TM0|TMs0], VarSet0, EqvMap, [TM|TMs], VarSet,
		Info0, Info) :-
	equiv_type__replace_in_tm(TM0, VarSet0, EqvMap, TM, VarSet1,
		Info0, Info1),
	equiv_type__replace_in_tms(TMs0, VarSet1, EqvMap, TMs, VarSet,
		Info1, Info).

:- pred equiv_type__replace_in_tm(type_and_mode, tvarset, eqv_map,
	type_and_mode, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_tm(in, in, in, out, out, in, out) is det.

equiv_type__replace_in_tm(type_only(Type0), VarSet0, EqvMap,
		type_only(Type), VarSet, Info0, Info) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet,
		Info0, Info).

equiv_type__replace_in_tm(type_and_mode(Type0, Mode), VarSet0, EqvMap,
		type_and_mode(Type, Mode), VarSet, Info0, Info) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet,
		Info0, Info).

%-----------------------------------------------------------------------------%

:- type equiv_type_info == maybe(pair(module_name, set(type_id))).

:- pred equiv_type__maybe_record_used_equivalences(module_name, sym_name,
		maybe(recompilation_info), equiv_type_info). 
:- mode equiv_type__maybe_record_used_equivalences(in, in, in, out) is det.

equiv_type__maybe_record_used_equivalences(_, _, no, no).
equiv_type__maybe_record_used_equivalences(ModuleName, SymName,
		yes(_), MaybeInfo) :-
	( SymName = qualified(ModuleName, _) ->
		MaybeInfo = no
	;
		MaybeInfo = yes(ModuleName - set__init)
	).

:- pred equiv_type__finish_recording_used_equivalences(item_id,
	equiv_type_info, maybe(recompilation_info), maybe(recompilation_info)).
:- mode equiv_type__finish_recording_used_equivalences(in, in, in, out) is det.

equiv_type__finish_recording_used_equivalences(_, no, no, no).
equiv_type__finish_recording_used_equivalences(_, no, yes(Info), yes(Info)).
equiv_type__finish_recording_used_equivalences(_, yes(_), no, _) :-
	error("equiv_type__finish_recording_used_equivalences").
equiv_type__finish_recording_used_equivalences(Item, yes(_ - UsedTypeIds),
		yes(Info0), yes(Info)) :-
	recompilation__record_used_equivalence_types(Item, UsedTypeIds,
		Info0, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__report_circular_types(list(item_and_context)::in,
		io__state::di, io__state::uo) is det.

equiv_type__report_circular_types([]) --> [].
equiv_type__report_circular_types([Circ | Circs]) -->
	(
		{ Circ = type_defn(_, SymName, Params,
				TypeDefn, _) - Context },
		{ TypeDefn = eqv_type(_) }
	->
		{ list__length(Params, Arity) },
		prog_out__write_context(Context),
		io__write_string("Error: circular equivalence type `"),
		prog_out__write_sym_name(SymName),
		io__write_string("'/"),
		io__write_int(Arity),
		io__write_string(".\n"),
		equiv_type__report_circular_types(Circs)
	;
		{ error("equiv_type__report_circular_types: invalid item") }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
