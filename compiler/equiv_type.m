%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types. It also expands away `with_type`
% and `with_inst` annotations on predicate and function type declarations.

% main author: fjh

:- module parse_tree__equiv_type.
:- interface.
:- import_module bool, list, io, std_util.
:- import_module recompilation, parse_tree__prog_data.

%-----------------------------------------------------------------------------%

	% equiv_type__expand_eqv_types(ModuleName, Items0, Items,
	%	CircularTypes, EqvMap, MaybeRecompInfo0, MaybeRecompInfo).
	%
	% First it builds up a map from type_ctor to the equivalent type.
	% Then it traverses through the list of items, expanding all types. 
	% This has the effect of eliminating all the equivalence types
	% from the source code.
	%
	% `with_type` and `with_inst` annotations on predicate and
	% function type declarations are also expaneded.
	%
	% Error messages are generated for any circular equivalence types
	% and invalid `with_type` and `with_inst` annotations.
	%
	% For items not defined in the current module, the items expanded
	% while processing each item are recorded in the recompilation_info,
	% for use by smart recompilation.
:- pred equiv_type__expand_eqv_types(module_name, list(item_and_context),
		list(item_and_context), bool, eqv_map,
		maybe(recompilation_info), maybe(recompilation_info),
		io__state, io__state).
:- mode equiv_type__expand_eqv_types(in, in, out, out, out,
		in, out, di, uo) is det.

	% Replace equivalence types in a given type, returning
	% the type_ctors of the equivalence types replaced.
:- pred equiv_type__replace_in_type(type, tvarset, eqv_map, type,
		tvarset).
:- mode equiv_type__replace_in_type(in, in, in, out, out) is det.

:- type eqv_map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module assoc_list, bool, require, std_util, map, set, term, varset.
:- import_module parse_tree__prog_data, parse_tree__prog_util.
:- import_module parse_tree__prog_out, parse_tree__inst.

% XXX we shouldn't import the HLDS here.
:- import_module hlds__error_util, check_hlds__mode_util.
:- import_module hlds__hlds_data, check_hlds__type_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% First we build up a mapping which records the equivalence type
	% definitions.  Then we go through the item list and replace
	% them.

equiv_type__expand_eqv_types(ModuleName, Items0, Items, Error, EqvMap,
		Info0, Info) -->
	{ map__init(EqvMap0) },
	{ map__init(EqvInstMap0) },
	{ equiv_type__build_eqv_map(Items0, EqvMap0, EqvMap,
		EqvInstMap0, EqvInstMap) },
	{ equiv_type__replace_in_item_list(ModuleName, Items0, EqvMap,
		EqvInstMap, [], RevItems, [], ErrorList, Info0, Info) },
	{ list__reverse(RevItems, Items) },
	(
		{ ErrorList = [] }
	->
		{ Error = no }	
	;
		list__foldl(equiv_type__report_error,
			list__reverse(ErrorList)),
		{ Error = yes },
		io__set_exit_status(1)
	).

:- type eqv_type_body ---> eqv_type_body(tvarset, list(type_param), type).
:- type eqv_map == map(type_ctor, eqv_type_body).

	% We need to expand equivalence insts in
	% `:- pred p `with_inst` i' declarations.
:- type eqv_inst_body ---> eqv_inst_body(inst_varset, list(inst_var), inst).
:- type eqv_inst_map == map(inst_id, eqv_inst_body).

:- type pred_or_func_decl_type
	--->	type_decl
	;	mode_decl
	.

:- type eqv_error == pair(eqv_error_type, prog_context).

:- type eqv_error_type
	--->	circular_equivalence(item)
	;	invalid_with_type(sym_name, pred_or_func)
	;	invalid_with_inst(pred_or_func_decl_type,
			sym_name, maybe(pred_or_func))
	;	non_matching_with_type_with_inst(sym_name, pred_or_func)
	.

:- pred equiv_type__build_eqv_map(list(item_and_context), eqv_map, eqv_map,
		eqv_inst_map, eqv_inst_map).
:- mode equiv_type__build_eqv_map(in, in, out, in, out) is det.

equiv_type__build_eqv_map([], EqvMap, EqvMap, EqvInstMap, EqvInstMap).
equiv_type__build_eqv_map([Item - _Context | Items], EqvMap0, EqvMap,
		EqvInstMap0, EqvInstMap) :-
	( Item = type_defn(VarSet, Name, Args, eqv_type(Body), _Cond) ->
		list__length(Args, Arity),
		map__set(EqvMap0, Name - Arity,
			eqv_type_body(VarSet, Args, Body), EqvMap1),
		EqvInstMap1 = EqvInstMap0
	; Item = inst_defn(VarSet, Name, Args, eqv_inst(Body), _) ->
		list__length(Args, Arity),
		map__set(EqvInstMap0, Name - Arity,
			eqv_inst_body(VarSet, Args, Body), EqvInstMap1),
		EqvMap1 = EqvMap0
	;
		EqvMap1 = EqvMap0,
		EqvInstMap1 = EqvInstMap0
	),
	equiv_type__build_eqv_map(Items, EqvMap1, EqvMap,
		EqvInstMap1, EqvInstMap).

	% The following predicate equiv_type__replace_in_item_list
	% performs substititution of equivalence types on a list
	% of items.  Similarly the replace_in_<foo> predicates that
	% follow perform substitution of equivalence types on <foo>s.

:- pred equiv_type__replace_in_item_list(module_name, list(item_and_context),
	eqv_map, eqv_inst_map, list(item_and_context), list(item_and_context),
	list(eqv_error), list(eqv_error),
	maybe(recompilation_info), maybe(recompilation_info)).
:- mode equiv_type__replace_in_item_list(in, in, in, in, in, out,
	in, out, in, out) is det.

equiv_type__replace_in_item_list(_, [], _, _, Items, Items,
		Errors, Errors, Info, Info).
equiv_type__replace_in_item_list(ModuleName, [ItemAndContext0 | Items0],
		EqvMap, EqvInstMap, ReplItems0, ReplItems,
		Errors0, Errors, Info0, Info) :-
	ItemAndContext0 = Item0 - Context,
	(
		equiv_type__replace_in_item(ModuleName, Item0, Context, EqvMap,
			EqvInstMap, Item, Errors1, Info0, Info1)
	->
		Info2 = Info1,
		ItemAndContext = Item - Context,

		% Discard the item if there were any errors.
		( Errors1 = [] ->
			ReplItems1 = [ItemAndContext | ReplItems0]
		;
			ReplItems1 = ReplItems0
		),

		Errors2 = Errors1 ++ Errors0
	;
		ItemAndContext = ItemAndContext0,
		Errors2 = Errors0,
		Info2 = Info0,
		ReplItems1 = [ItemAndContext | ReplItems0]
	),
	equiv_type__replace_in_item_list(ModuleName, Items0, EqvMap,
		EqvInstMap, ReplItems1, ReplItems, Errors2, Errors,
		Info2, Info).

:- pred equiv_type__replace_in_item(module_name, item, prog_context,
	eqv_map, eqv_inst_map, item, list(eqv_error), maybe(recompilation_info),
	maybe(recompilation_info)).
:- mode equiv_type__replace_in_item(in, in, in, in, in, out, out,
	in, out) is semidet.

equiv_type__replace_in_item(ModuleName,
		type_defn(VarSet0, Name, TArgs, TypeDefn0, Cond) @ Item,
		Context, EqvMap, _EqvInstMap, type_defn(VarSet, Name, TArgs,
		TypeDefn, Cond), Error, Info0, Info) :-
	list__length(TArgs, Arity),
	equiv_type__maybe_record_expanded_items(ModuleName, Name,
		Info0, UsedTypeCtors0),
	equiv_type__replace_in_type_defn(Name - Arity, TypeDefn0,
		VarSet0, EqvMap, TypeDefn, VarSet, ContainsCirc,
		UsedTypeCtors0, UsedTypeCtors),
	( ContainsCirc = yes ->
		Error = [circular_equivalence(Item) - Context]
	;
		Error = []
	),
	equiv_type__finish_recording_expanded_items(
		item_id(type_body, Name - Arity), UsedTypeCtors, Info0, Info).

equiv_type__replace_in_item(ModuleName,
		pred_or_func(TypeVarSet0, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes0, MaybeWithType0,
			MaybeWithInst0, Det0, Cond, Purity, ClassContext0),
		Context, EqvMap, EqvInstMap,
		pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes, MaybeWithType,
			MaybeWithInst, Det, Cond, Purity, ClassContext),
		Errors, Info0, Info) :-
	equiv_type__maybe_record_expanded_items(ModuleName, PredName,
		Info0, ExpandedItems0),

	equiv_type__replace_in_pred_type(PredName, PredOrFunc, Context, EqvMap,
		EqvInstMap, ClassContext0, ClassContext,
		TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
		MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
		Det0, Det, ExpandedItems0, ExpandedItems, Errors),

	ItemType = pred_or_func_to_item_type(PredOrFunc),
	list__length(TypesAndModes, Arity),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	equiv_type__finish_recording_expanded_items(
		item_id(ItemType, PredName - OrigArity),
		ExpandedItems, Info0, Info).

equiv_type__replace_in_item(ModuleName,
		pred_or_func_mode(InstVarSet, MaybePredOrFunc0, PredName,
			Modes0, WithInst0, Det0, Cond),
		Context, _EqvMap, EqvInstMap,
		pred_or_func_mode(InstVarSet, MaybePredOrFunc, PredName,
			Modes, WithInst, Det, Cond),
		Errors, Info0, Info) :-
	equiv_type__maybe_record_expanded_items(ModuleName, PredName,
		Info0, ExpandedItems0),

	equiv_type__replace_in_pred_mode(PredName, length(Modes0), Context,
		mode_decl, EqvInstMap, MaybePredOrFunc0, MaybePredOrFunc,
		ExtraModes, WithInst0, WithInst, Det0, Det,
		ExpandedItems0, ExpandedItems, Errors),
	( ExtraModes = [] ->
		Modes = Modes0
	;
		Modes = Modes0 ++ ExtraModes
	),

	( MaybePredOrFunc = yes(PredOrFunc) ->
		ItemType = pred_or_func_to_item_type(PredOrFunc),
		list__length(Modes, Arity),
		adjust_func_arity(PredOrFunc, OrigArity, Arity),
		equiv_type__finish_recording_expanded_items(
			item_id(ItemType, PredName - OrigArity),
			ExpandedItems, Info0, Info)
	;
		Info = Info0
	).

equiv_type__replace_in_item(ModuleName,
			typeclass(Constraints0, ClassName, Vars, 
				ClassInterface0, VarSet0),
			_Context, EqvMap, EqvInstMap,
			typeclass(Constraints, ClassName, Vars, 
				ClassInterface, VarSet),
			Errors, Info0, Info) :-
	list__length(Vars, Arity),
	equiv_type__maybe_record_expanded_items(ModuleName, ClassName,
		Info0, ExpandedItems0),
	equiv_type__replace_in_class_constraint_list(Constraints0, VarSet0, 
		EqvMap, Constraints, VarSet, ExpandedItems0, ExpandedItems1),
	(
		ClassInterface0 = abstract,
		ClassInterface = abstract,
		ExpandedItems = ExpandedItems1,
		Errors = []
	;
		ClassInterface0 = concrete(Methods0),
		equiv_type__replace_in_class_interface(Methods0,
			EqvMap, EqvInstMap, Methods, [], Errors,
			ExpandedItems1, ExpandedItems),
		ClassInterface = concrete(Methods)
	),
	equiv_type__finish_recording_expanded_items(
		item_id(typeclass, ClassName - Arity),
		ExpandedItems, Info0, Info).

equiv_type__replace_in_item(ModuleName,
			instance(Constraints0, ClassName, Ts0, 
				InstanceBody, VarSet0, ModName),
			_Context, EqvMap, _EqvInstMap,
			instance(Constraints, ClassName, Ts, 
				InstanceBody, VarSet, ModName),
			[], Info0, Info) :-
	( (Info0 = no ; ModName = ModuleName) ->
		UsedTypeCtors0 = no
	;	
		UsedTypeCtors0 = yes(ModuleName - set__init)
	),
	equiv_type__replace_in_class_constraint_list(Constraints0, VarSet0, 
		EqvMap, Constraints, VarSet1, UsedTypeCtors0, UsedTypeCtors1),
	equiv_type__replace_in_type_list(Ts0, VarSet1, EqvMap, Ts, VarSet, _,
		UsedTypeCtors1, UsedTypeCtors),
	list__length(Ts0, Arity),
	equiv_type__finish_recording_expanded_items(
		item_id(typeclass, ClassName - Arity),
		UsedTypeCtors, Info0, Info).

equiv_type__replace_in_item(ModuleName,
		pragma(type_spec(PredName, B, Arity, D, E,
			Subst0, VarSet0, ItemIds0)),
		_Context, EqvMap, _EqvInstMap,
		pragma(type_spec(PredName, B, Arity, D, E,
			Subst, VarSet, ItemIds)),
		[], Info, Info) :-
	( (Info = no ; PredName = qualified(ModuleName, _)) ->
		ExpandedItems0 = no
	;	
		ExpandedItems0 = yes(ModuleName - ItemIds0)
	),
	equiv_type__replace_in_subst(Subst0, VarSet0, EqvMap, Subst, VarSet,
		ExpandedItems0, ExpandedItems),
	(
		ExpandedItems = no,
		ItemIds = ItemIds0
	;
		ExpandedItems = yes(_ - ItemIds)
	).

:- pred equiv_type__replace_in_type_defn(type_ctor, type_defn, tvarset,
		eqv_map, type_defn, tvarset, bool,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_defn(in, in, in, in, out, out, out,
		in, out) is semidet.

equiv_type__replace_in_type_defn(TypeCtor, eqv_type(TBody0),
		VarSet0, EqvMap, eqv_type(TBody),
		VarSet, ContainsCirc, Info0, Info) :-
	equiv_type__replace_in_type_2(TBody0, VarSet0, EqvMap, [TypeCtor],
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

:- pred equiv_type__replace_in_class_interface(list(class_method),
		eqv_map, eqv_inst_map, list(class_method),
		list(eqv_error), list(eqv_error),
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_interface(in,
		in, in, out, in, out, in, out) is det.

equiv_type__replace_in_class_interface(ClassInterface0, EqvMap, EqvInstMap,
		ClassInterface, Errors0, Errors, Info0, Info) :-
	list__map_foldl2(
		equiv_type__replace_in_class_method(EqvMap, EqvInstMap),
		ClassInterface0, ClassInterface, Errors0, Errors, Info0, Info).

:- pred equiv_type__replace_in_class_method(eqv_map, eqv_inst_map,
	class_method, class_method, list(eqv_error), list(eqv_error),
	equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_method(in, in, in, out,
	in, out, in, out) is det.

equiv_type__replace_in_class_method(EqvMap, EqvInstMap,
		pred_or_func(TypeVarSet0, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes0, WithType0, WithInst0,
			Det0, Cond, Purity, ClassContext0, Context),
		pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
			PredName, TypesAndModes, WithType, WithInst,
			Det, Cond, Purity, ClassContext, Context),
		Errors0, Errors, Info0, Info) :-
	equiv_type__replace_in_pred_type(PredName, PredOrFunc, Context, EqvMap,
		EqvInstMap, ClassContext0, ClassContext,
		TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
		WithType0, WithType, WithInst0, WithInst, Det0, Det,
		Info0, Info, Errors1),
	Errors = Errors1 ++ Errors0.

equiv_type__replace_in_class_method(_, EqvInstMap,
		pred_or_func_mode(InstVarSet, MaybePredOrFunc0, PredName,
			Modes0, WithInst0, Det0, Cond, Context),
		pred_or_func_mode(InstVarSet, MaybePredOrFunc, PredName,
			Modes, WithInst, Det, Cond, Context),
		Errors0, Errors, Info0, Info) :-
	equiv_type__replace_in_pred_mode(PredName, length(Modes0), Context,
		mode_decl, EqvInstMap, MaybePredOrFunc0, MaybePredOrFunc,
		ExtraModes, WithInst0, WithInst, Det0, Det, Info0, Info,
		Errors1),
	( ExtraModes = [] ->
		Modes = Modes0
	;
		Modes = Modes0 ++ ExtraModes
	),
	Errors = Errors1 ++ Errors0.

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
		list(type_ctor), list(type), tvarset, bool, bool,
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
	eqv_map, list(type_ctor), list(constructor_arg), tvarset, bool, bool,
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
	list(type_ctor), type, tvarset, bool,
	equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_2(in, in, in, in, out, out, out,
	in, out) is det.

equiv_type__replace_in_type_2(term__variable(V), VarSet, _EqvMap,
		_Seen, term__variable(V), VarSet, no, Info, Info).
equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap, TypeCtorsAlreadyExpanded,
		Type, VarSet, Circ, Info0, Info) :- 

	Type0 = term__functor(_, _, _),
	(
		type_to_ctor_and_args(Type0, EqvTypeCtor, TArgs0)
	->
		equiv_type__replace_in_type_list_2(TArgs0, VarSet0, EqvMap,
			TypeCtorsAlreadyExpanded, TArgs1, VarSet1, no, Circ0,
			Info0, Info1),

		( list__member(EqvTypeCtor, TypeCtorsAlreadyExpanded) ->
			Circ1 = yes
		;
			Circ1 = no
		),
		(	
			map__search(EqvMap, EqvTypeCtor,
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
			equiv_type__record_expanded_item(
				item_id(type, EqvTypeCtor), Info1, Info2),
			term__term_list_to_var_list(Args, ArgVars),
			term__substitute_corresponding(ArgVars, TArgs1,
							Body, Type1),
			equiv_type__replace_in_type_2(Type1, VarSet2, EqvMap,
				[EqvTypeCtor | TypeCtorsAlreadyExpanded],
				Type, VarSet, Circ, Info2, Info)
		;
			VarSet = VarSet1,
			Info = Info1,
			construct_type(EqvTypeCtor, TArgs1, Type),
			bool__or(Circ0, Circ1, Circ)
		)
	;
		VarSet = VarSet0,
		Type = Type0,
		Info = Info0,
		Circ = no
	).

:- pred equiv_type__replace_in_inst(inst, eqv_inst_map, inst,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_inst(in, in, out, in, out) is det.

equiv_type__replace_in_inst(Inst0, EqvInstMap, Inst, Info0, Info) :-
	equiv_type__replace_in_inst(Inst0, EqvInstMap, set__init,
		Inst, Info0, Info).

:- pred equiv_type__replace_in_inst(inst, eqv_inst_map,
		set(inst_id), inst, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_inst(in, in, in, out, in, out) is det.

equiv_type__replace_in_inst(Inst0, EqvInstMap, ExpandedInstIds,
		Inst, Info0, Info) :-
	(
		Inst0 = defined_inst(user_inst(SymName, ArgInsts))
	->
		InstId = SymName - length(ArgInsts),
		(
			set__member(InstId, ExpandedInstIds)
		->
			Info = Info0,
			Inst = Inst0
		;
			map__search(EqvInstMap, InstId,
				eqv_inst_body(_, EqvInstParams, EqvInst))
		->
			inst_substitute_arg_list(EqvInst, EqvInstParams,
				ArgInsts, Inst1),
			equiv_type__record_expanded_item(item_id(inst, InstId),
				Info0, Info1),
			equiv_type__replace_in_inst(Inst1, EqvInstMap,
				set__insert(ExpandedInstIds, InstId), Inst,
				Info1, Info)
		;
			Info = Info0,
			Inst = Inst0
		)
	;
		Info = Info0,
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_pred_type(sym_name, pred_or_func, prog_context,
	eqv_map, eqv_inst_map, class_constraints, class_constraints,
	list(type_and_mode), list(type_and_mode), tvarset, tvarset,
	maybe(type), maybe(type), maybe(inst), maybe(inst), 
	maybe(determinism), maybe(determinism), 
	equiv_type_info, equiv_type_info, list(eqv_error)).
:- mode equiv_type__replace_in_pred_type(in, in, in, in, in, in, out, in, out,
	in, out, in, out, in, out, in, out, in, out, out) is det.

equiv_type__replace_in_pred_type(PredName, PredOrFunc, Context, EqvMap,
		EqvInstMap, ClassContext0, ClassContext,
		TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
		MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
		Det0, Det, Info0, Info, Errors) :-
	equiv_type__replace_in_class_constraints(ClassContext0, TypeVarSet0, 
		EqvMap, ClassContext, TypeVarSet1, Info0, Info1),
	equiv_type__replace_in_tms(TypesAndModes0, TypeVarSet1, EqvMap, 
		TypesAndModes1, TypeVarSet2, Info1, Info2),

	(
		MaybeWithType0 = yes(WithType0),
		equiv_type__replace_in_type(WithType0, TypeVarSet2,
			EqvMap, WithType, TypeVarSet,
			Info2, Info3),
		( type_is_higher_order(WithType, PredOrFunc, _, ExtraTypes0) ->
			ExtraTypes = ExtraTypes0,
			Errors0 = []
		;
			ExtraTypes = [],
			Errors0 = [invalid_with_type(PredName, PredOrFunc)
					- Context]
		)
	;	
		MaybeWithType0 = no,
		Info3 = Info2,
		ExtraTypes = [],
		TypeVarSet = TypeVarSet2,
		Errors0 = []
	),

	equiv_type__replace_in_pred_mode(PredName, length(TypesAndModes0),
		Context, type_decl, EqvInstMap, yes(PredOrFunc), _, ExtraModes,
		MaybeWithInst0, _, Det0, Det, Info3, Info4, ModeErrors),
	Errors1 = Errors0 ++ ModeErrors,

	( Errors1 \= [] ->
		Errors = Errors1,
		ExtraTypesAndModes = []
	; ExtraModes = [] ->
		Errors = Errors1,
		ExtraTypesAndModes = list__map((func(Type) = type_only(Type)),
					ExtraTypes)
	; length(ExtraTypes) `with_type` int = length(ExtraModes) ->
		Errors = Errors1,
		assoc_list__from_corresponding_lists(ExtraTypes,
				ExtraModes, ExtraTypesModes),
		ExtraTypesAndModes = list__map(
			(func(Type - Mode) = type_and_mode(Type, Mode)),
			ExtraTypesModes)
	;
		Errors = [non_matching_with_type_with_inst(PredName,
				PredOrFunc) - Context | Errors1],
		ExtraTypesAndModes = []
	),

	( Errors = [] ->
		MaybeWithType = no,
		MaybeWithInst = no
	;
		% Leave the `with_type` and `with_inst` fields so
		% that make_hlds knows to discard this declaration.
		MaybeWithType = MaybeWithType0,
		MaybeWithInst = MaybeWithInst0
	),

	( ExtraTypesAndModes = [] ->
		Info = Info4,
		TypesAndModes = TypesAndModes1
	;
		OrigItemId = item_id(pred_or_func_to_item_type(PredOrFunc),
				PredName - list__length(TypesAndModes0)),
		equiv_type__record_expanded_item(OrigItemId,
			Info4, Info),
		TypesAndModes = TypesAndModes1 ++ ExtraTypesAndModes
	).

:- pred equiv_type__replace_in_pred_mode(sym_name, arity, prog_context,
	pred_or_func_decl_type, eqv_inst_map, maybe(pred_or_func),
	maybe(pred_or_func), list(mode), maybe(inst),
	maybe(inst), maybe(determinism), maybe(determinism),
	equiv_type_info, equiv_type_info, list(eqv_error)).
:- mode equiv_type__replace_in_pred_mode(in, in, in, in, in, in, out, out,
	in, out, in, out, in, out, out) is det.

equiv_type__replace_in_pred_mode(PredName, OrigArity, Context, DeclType,
		EqvInstMap, MaybePredOrFunc0, MaybePredOrFunc, ExtraModes,
		MaybeWithInst0, MaybeWithInst, Det0, Det,
		Info0, Info, Errors) :-
	(
		MaybeWithInst0 = yes(WithInst0),
		equiv_type__replace_in_inst(WithInst0, EqvInstMap, WithInst,
			Info0, Info1),
		(
			WithInst = ground(_, higher_order(pred_inst_info(
					PredOrFunc, ExtraModes0, Det1))),
			( MaybePredOrFunc0 = no
			; MaybePredOrFunc0 = yes(PredOrFunc)
			)
		->
			Det = yes(Det1),
			MaybeWithInst = no,
			MaybePredOrFunc = yes(PredOrFunc),
			Errors = [],
			ExtraModes = ExtraModes0,
			(
				MaybePredOrFunc0 = no,
				RecordedPredOrFunc = predicate
			;
				MaybePredOrFunc0 = yes(RecordedPredOrFunc)
			),
			OrigItemId = item_id(
				pred_or_func_to_item_type(RecordedPredOrFunc),
				PredName - OrigArity),
			equiv_type__record_expanded_item(OrigItemId,
				Info1, Info)
		;
			ExtraModes = [],
			MaybePredOrFunc = MaybePredOrFunc0,
			% Leave the `with_inst` fields so that make_hlds
			% knows to discard this declaration.
			MaybeWithInst = MaybeWithInst0,
			Info = Info1,
			Det = Det0,
			Errors = [invalid_with_inst(DeclType, PredName,
					MaybePredOrFunc0) - Context]
		)
	;
		MaybeWithInst0 = no,
		MaybeWithInst = MaybeWithInst0,
		MaybePredOrFunc = MaybePredOrFunc0,
		Info = Info0,
		Errors = [],
		Det = Det0,
		ExtraModes = []
	).

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

:- type equiv_type_info == maybe(pair(module_name, set(item_id))).

:- pred equiv_type__maybe_record_expanded_items(module_name, sym_name,
		maybe(recompilation_info), equiv_type_info). 
:- mode equiv_type__maybe_record_expanded_items(in, in, in, out) is det.

equiv_type__maybe_record_expanded_items(_, _, no, no).
equiv_type__maybe_record_expanded_items(ModuleName, SymName,
		yes(_), MaybeInfo) :-
	( SymName = qualified(ModuleName, _) ->
		MaybeInfo = no
	;
		MaybeInfo = yes(ModuleName - set__init)
	).

:- pred equiv_type__record_expanded_item(item_id,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__record_expanded_item(in, in, out) is det.

equiv_type__record_expanded_item(Item, Info0, Info) :-
	map_maybe(equiv_type__record_expanded_item_2(Item), Info0, Info).

:- pred equiv_type__record_expanded_item_2(item_id,
		pair(module_name, set(item_id)),
		pair(module_name, set(item_id))).
:- mode equiv_type__record_expanded_item_2(in, in, out) is det.

equiv_type__record_expanded_item_2(ItemId, ModuleName - Items0,
		ModuleName - Items) :-
	ItemId = item_id(_, ItemName),
	( ItemName = qualified(ModuleName, _) - _ ->
		% We don't need to record local types.
		Items = Items0
	;
		Items = set__insert(Items0, ItemId)
	).

:- pred equiv_type__finish_recording_expanded_items(item_id,
	equiv_type_info, maybe(recompilation_info), maybe(recompilation_info)).
:- mode equiv_type__finish_recording_expanded_items(in, in, in, out) is det.

equiv_type__finish_recording_expanded_items(_, no, no, no).
equiv_type__finish_recording_expanded_items(_, no, yes(Info), yes(Info)).
equiv_type__finish_recording_expanded_items(_, yes(_), no, _) :-
	error("equiv_type__finish_recording_expanded_items").
equiv_type__finish_recording_expanded_items(Item, yes(_ - ExpandedItems),
		yes(Info0), yes(Info)) :-
	recompilation__record_expanded_items(Item, ExpandedItems, Info0, Info).

%-----------------------------------------------------------------------------%

:- pred equiv_type__report_error(eqv_error::in,
		io__state::di, io__state::uo) is det.

equiv_type__report_error(circular_equivalence(Item) - Context) -->
	(
		{ Item = type_defn(_, SymName, Params,
				TypeDefn, _) },
		{ TypeDefn = eqv_type(_) }
	->
		{ Pieces = append_punctuation([
			words("Error: circular equivalence type"),
			fixed(error_util__describe_sym_name_and_arity(
				SymName / length(Params)))
			], '.') },
		error_util__write_error_pieces(Context, 0, Pieces)
	;
		{ error("equiv_type__report_error: invalid item") }
	).
equiv_type__report_error(invalid_with_type(SymName, PredOrFunc) - Context) -->
	{ FirstLine = append_punctuation([words("In type declaration for"),
			words(error_util__pred_or_func_to_string(PredOrFunc)),
			fixed(error_util__describe_sym_name(SymName))
		], ':') },
	{ Rest = [nl, words("error: expected higher order"),
			words(error_util__pred_or_func_to_string(PredOrFunc)),	
			words("type after `with_type`.")] },
	error_util__write_error_pieces(Context, 0, FirstLine ++ Rest).
equiv_type__report_error(invalid_with_inst(DeclType,
		SymName, MaybePredOrFunc) - Context) -->
	{ DeclType = type_decl, DeclStr = "declaration"
	; DeclType = mode_decl, DeclStr = "mode declaration"
	},
	{
		MaybePredOrFunc = no, PredOrFuncStr = ""
	;
		MaybePredOrFunc = yes(PredOrFunc),
		PredOrFuncStr = error_util__pred_or_func_to_string(PredOrFunc)
	},
	{ FirstLine = append_punctuation([words("In"), words(DeclStr),
			words("for"),
			words(PredOrFuncStr),
			fixed(error_util__describe_sym_name(SymName))
		], ':') },
	{ Rest = [nl, words("error: expected higher order "),
		words(PredOrFuncStr),
		words("inst after `with_inst`.")] },
	error_util__write_error_pieces(Context, 0, FirstLine ++ Rest).
equiv_type__report_error(non_matching_with_type_with_inst(SymName,
		PredOrFunc) - Context) -->
	{ FirstLine = append_punctuation([words("In type declaration for"),
			words(error_util__pred_or_func_to_string(PredOrFunc)),
			fixed(error_util__describe_sym_name(SymName))
		], ':') },
	{ Rest = [nl,
		words("error: the `with_type` and `with_inst`"),
		words("annotations are incompatible.")] },
	error_util__write_error_pieces(Context, 0, FirstLine ++ Rest).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
