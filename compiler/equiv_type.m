%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types. It also expands away `with_type`
% and `with_inst` annotations on predicate and function type declarations.

% main author: fjh

:- module parse_tree__equiv_type.
:- interface.

:- import_module parse_tree__prog_data.
:- import_module recompilation.

:- import_module bool, list, map, io, std_util.

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

	% Replace equivalence types in a given type.
	% The bool output is `yes' if anything changed.
:- pred equiv_type__replace_in_type(eqv_map, type, type, bool,
		tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type(in, in, out, out, in, out, in, out) is det.

:- pred equiv_type__replace_in_type_list(eqv_map, list(type), list(type),
		bool, tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_list(in, in, out, out, in, out,
		in, out) is det.

:- pred equiv_type__replace_in_class_constraints(eqv_map, class_constraints, 
		class_constraints, tvarset, tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_constraints(in, in, out,
		in, out, in,  out) is det.

:- pred equiv_type__replace_in_class_constraint(eqv_map,
	class_constraint, class_constraint, tvarset, tvarset,
	equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_constraint(in, in, out,
	in, out, in, out) is det.

:- pred equiv_type__replace_in_ctors(eqv_map,
		list(constructor), list(constructor), tvarset, tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctors(in, in, out, in, out, in, out) is det.

:- type eqv_type_body ---> eqv_type_body(tvarset, list(type_param), type).
:- type eqv_map == map(type_ctor, eqv_type_body).

:- type equiv_type_info == maybe(expanded_item_set).
:- type expanded_item_set.

	% For smart recompilation we need to record which items were
	% expanded in each declaration.  Any items which depend on
	% that declaration also depend on the expanded items.
:- pred equiv_type__maybe_record_expanded_items(module_name, sym_name,
		maybe(recompilation_info), equiv_type_info). 
:- mode equiv_type__maybe_record_expanded_items(in, in, in, out) is det.

	% Record all the expanded items in the recompilation_info.
:- pred equiv_type__finish_recording_expanded_items(item_id,
	equiv_type_info, maybe(recompilation_info), maybe(recompilation_info)).
:- mode equiv_type__finish_recording_expanded_items(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% XXX we shouldn't import the HLDS here.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__inst.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module assoc_list, bool, require, std_util, map, set, term, varset.

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
equiv_type__build_eqv_map([Item - _Context | Items0], EqvMap0, EqvMap,
		EqvInstMap0, EqvInstMap) :-
	(
		Item = module_defn(_, abstract_imported)
	->
		skip_abstract_imported_items(Items0, Items),
		EqvMap1 = EqvMap0,
		EqvInstMap1 = EqvInstMap0
	;	
		Item = type_defn(VarSet, Name, Args,
				eqv_type(Body), _Cond)
	->
		Items = Items0,
		list__length(Args, Arity),
		map__set(EqvMap0, Name - Arity,
			eqv_type_body(VarSet, Args, Body), EqvMap1),
		EqvInstMap1 = EqvInstMap0
	;
		Item = inst_defn(VarSet, Name, Args, eqv_inst(Body), _)
	->
		Items = Items0,
		list__length(Args, Arity),
		map__set(EqvInstMap0, Name - Arity,
			eqv_inst_body(VarSet, Args, Body), EqvInstMap1),
		EqvMap1 = EqvMap0
	;
		Items = Items0,
		EqvMap1 = EqvMap0,
		EqvInstMap1 = EqvInstMap0
	),
	equiv_type__build_eqv_map(Items, EqvMap1, EqvMap,
		EqvInstMap1, EqvInstMap).

:- pred skip_abstract_imported_items(list(item_and_context),
		list(item_and_context)).
:- mode skip_abstract_imported_items(in, out) is det.

skip_abstract_imported_items([], []).
skip_abstract_imported_items([Item - _ | Items0], Items) :-
	(
		Item = module_defn(_, Defn),
		is_section_defn(Defn) = yes,
		Defn \= abstract_imported
	->
		Items = Items0
	;
		skip_abstract_imported_items(Items0, Items)
	).

:- func is_section_defn(module_defn) = bool.

is_section_defn(module(_)) = yes.
is_section_defn(end_module(_)) = yes.
is_section_defn(interface) = yes.
is_section_defn(implementation) = yes.
is_section_defn(private_interface) = yes.
is_section_defn(imported(_)) = yes.
is_section_defn(used(_)) = yes.
is_section_defn(abstract_imported) = yes.
is_section_defn(opt_imported) = yes.
is_section_defn(transitively_imported) = yes.
is_section_defn(external(_)) = no.
is_section_defn(export(_)) = no.
is_section_defn(import(_)) = no.
is_section_defn(use(_)) = no.
is_section_defn(include_module(_)) = no.
is_section_defn(version_numbers(_, _)) = no.

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
		Context, EqvMap, _EqvInstMap,
		type_defn(VarSet, Name, TArgs, TypeDefn, Cond),
		Error, Info0, Info) :-
	list__length(TArgs, Arity),
	equiv_type__maybe_record_expanded_items(ModuleName, Name,
		Info0, UsedTypeCtors0),
	equiv_type__replace_in_type_defn(EqvMap, Name - Arity, TypeDefn0,
		TypeDefn, ContainsCirc, VarSet0, VarSet,
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
	equiv_type__replace_in_class_constraint_list(EqvMap,
		Constraints0, Constraints, VarSet0, VarSet,
		ExpandedItems0, ExpandedItems1),
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
	equiv_type__replace_in_class_constraint_list(EqvMap,
		Constraints0, Constraints, VarSet0, VarSet1,
		UsedTypeCtors0, UsedTypeCtors1),
	equiv_type__replace_in_type_list(EqvMap, Ts0, Ts, _, _,
		VarSet1, VarSet, UsedTypeCtors1, UsedTypeCtors),
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
	equiv_type__replace_in_subst(EqvMap, Subst0, Subst, VarSet0, VarSet,
		ExpandedItems0, ExpandedItems),
	(
		ExpandedItems = no,
		ItemIds = ItemIds0
	;
		ExpandedItems = yes(_ - ItemIds)
	).

:- pred equiv_type__replace_in_type_defn(eqv_map, type_ctor,
		type_defn, type_defn, bool, tvarset, tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_defn(in, in, in, out, out, in, out,
		in, out) is semidet.

equiv_type__replace_in_type_defn(EqvMap, TypeCtor, eqv_type(TBody0),
		eqv_type(TBody), ContainsCirc, !VarSet, !Info) :-
	equiv_type__replace_in_type_2(EqvMap, [TypeCtor], TBody0, TBody,
		_, ContainsCirc, !VarSet, !Info).

equiv_type__replace_in_type_defn(EqvMap, _,
		du_type(TBody0, IsSolverType, EqPred),
		du_type(TBody, IsSolverType, EqPred), no, !VarSet, !Info) :-
	equiv_type__replace_in_ctors(EqvMap, TBody0, TBody, !VarSet, !Info).

%-----------------------------------------------------------------------------%

equiv_type__replace_in_class_constraints(EqvMap, Cs0, Cs, !VarSet, !Info) :-
	Cs0 = constraints(UnivCs0, ExistCs0),
	Cs = constraints(UnivCs, ExistCs),
	equiv_type__replace_in_class_constraint_list(EqvMap, UnivCs0, UnivCs,
		!VarSet, !Info),
	equiv_type__replace_in_class_constraint_list(EqvMap, ExistCs0, ExistCs,
		!VarSet, !Info).

:- pred equiv_type__replace_in_class_constraint_list(eqv_map,
	list(class_constraint), list(class_constraint),
	tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_class_constraint_list(in, in, out,
	in, out, in, out) is det.

equiv_type__replace_in_class_constraint_list(EqvMap, Cs0, Cs,
		!VarSet, !Info) :-
	list__map_foldl2(equiv_type__replace_in_class_constraint(EqvMap),
		Cs0, Cs, !VarSet, !Info).

equiv_type__replace_in_class_constraint(EqvMap, Constraint0, Constraint,
		!VarSet, !Info) :-
	Constraint0 = constraint(ClassName, Ts0),
	equiv_type__replace_in_type_list(EqvMap, Ts0, Ts,
		_, _, !VarSet, !Info),
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

:- pred equiv_type__replace_in_subst(eqv_map,
		assoc_list(tvar, type), assoc_list(tvar, type),
		tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_subst(in, in, out, in, out, in, out) is det.

equiv_type__replace_in_subst(_EqvMap, [], [], !VarSet, !Info).
equiv_type__replace_in_subst(EqvMap, [Var - Type0 | Subst0],
		[Var - Type | Subst], !VarSet, !Info) :-
	equiv_type__replace_in_type(EqvMap, Type0, Type, _, !VarSet, !Info),
	equiv_type__replace_in_subst(EqvMap, Subst0, Subst, !VarSet, !Info).

%-----------------------------------------------------------------------------%

equiv_type__replace_in_ctors(EqvMap, !Ctors, !VarSet, !Info) :-
	list__map_foldl2(equiv_type__replace_in_ctor(EqvMap),
		!Ctors, !VarSet, !Info).

:- pred equiv_type__replace_in_ctor(eqv_map, constructor, constructor,
		tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctor(in, in, out, in, out, in, out) is det.

equiv_type__replace_in_ctor(EqvMap,
		ctor(ExistQVars, Constraints0, TName, Targs0),
		ctor(ExistQVars, Constraints, TName, Targs), !VarSet, !Info) :-
	equiv_type__replace_in_ctor_arg_list(EqvMap, Targs0, Targs, _,
		!VarSet, !Info),
	equiv_type__replace_in_class_constraint_list(EqvMap,
		Constraints0, Constraints, !VarSet, !Info).

%-----------------------------------------------------------------------------%

equiv_type__replace_in_type_list(EqvMap, Ts0, Ts, Changed,
		!VarSet, !Info) :-
	equiv_type__replace_in_type_list_2(EqvMap, [], Ts0, Ts,
		Changed, no, _, !VarSet, !Info).

:- pred equiv_type__replace_in_type_list(eqv_map, list(type), list(type),
		bool, bool, tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_list(in, in, out, out, out, in, out,
		in, out) is det.

equiv_type__replace_in_type_list(EqvMap, Ts0, Ts, Changed, ContainsCirc,
		!VarSet, !Info) :-
	equiv_type__replace_in_type_list_2(EqvMap, [], Ts0, Ts,
		Changed, no, ContainsCirc, !VarSet, !Info).

:- pred equiv_type__replace_in_type_list_2(eqv_map, list(type_ctor),
		list(type), list(type), bool, bool, bool, tvarset, tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_list_2(in, in, in, out, out,
		in, out, in, out, in, out) is det.

equiv_type__replace_in_type_list_2(_EqvMap, _Seen, [], [], no,
		!ContainsCirc, !VarSet, !Info).
equiv_type__replace_in_type_list_2(EqvMap, Seen, List0 @ [T0 | Ts0], List,
		Changed, !Circ, !VarSet, !Info) :-
	equiv_type__replace_in_type_2(EqvMap, Seen, T0, T, Changed0,
		ContainsCirc, !VarSet, !Info),
	!:Circ = ContainsCirc `or` !.Circ,
	equiv_type__replace_in_type_list_2(EqvMap, Seen, Ts0, Ts,
		Changed1, !Circ, !VarSet, !Info),
	( ( Changed0 = yes ; Changed1 = yes ) ->
		Changed = yes,
		List = [T | Ts]
	;
		Changed = no,
		List = List0
	).

%-----------------------------------------------------------------------------%

:- pred equiv_type__replace_in_ctor_arg_list(eqv_map,
		list(constructor_arg), list(constructor_arg), bool,
		tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctor_arg_list(in, in, out, out,
		in, out, in, out) is det.

equiv_type__replace_in_ctor_arg_list(EqvMap, As0, As, ContainsCirc,
		!VarSet, !Info) :-
	equiv_type__replace_in_ctor_arg_list_2(EqvMap, [], As0, As, no,
		ContainsCirc, !VarSet, !Info).

:- pred equiv_type__replace_in_ctor_arg_list_2(eqv_map, list(type_ctor),
		list(constructor_arg), list(constructor_arg), bool, bool,
		tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_ctor_arg_list_2(in, in, in, out,
		in, out, in, out, in, out) is det.

equiv_type__replace_in_ctor_arg_list_2(_EqvMap, _Seen, [], [], !ContainsCirc,
		!VarSet, !Info).
equiv_type__replace_in_ctor_arg_list_2(EqvMap, Seen, [N - T0 | As0],
		[N - T | As], !Circ, !VarSet, !Info) :-
	equiv_type__replace_in_type_2(EqvMap, Seen, T0, T, _, ContainsCirc,
		!VarSet, !Info),
	!:Circ = !.Circ `or` ContainsCirc,
	equiv_type__replace_in_ctor_arg_list_2(EqvMap, Seen, As0, As,
		!Circ, !VarSet, !Info).

%-----------------------------------------------------------------------------%

equiv_type__replace_in_type(EqvMap, Type0, Type, Changed, !VarSet, !Info) :-
	equiv_type__replace_in_type_2(EqvMap, [], Type0, Type, Changed, _,
		!VarSet, !Info).

	% Replace all equivalence types in a given type, detecting  
	% any circularities.
:- pred equiv_type__replace_in_type_2(eqv_map, list(type_ctor), type, type,
	bool, bool, tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_type_2(in, in, in, out, out, out,
	in, out, in, out) is det.

equiv_type__replace_in_type_2(_EqvMap, _Seen,
		term__variable(V), term__variable(V), no, no, !VarSet, !Info).
equiv_type__replace_in_type_2(EqvMap, TypeCtorsAlreadyExpanded, Type0, Type,
		Changed, Circ, !VarSet, !Info) :- 
	Type0 = term__functor(_, _, _),
	(
		type_to_ctor_and_args(Type0, EqvTypeCtor, TArgs0)
	->
		equiv_type__replace_in_type_list_2(EqvMap,
			TypeCtorsAlreadyExpanded, TArgs0, TArgs1,
			ArgsChanged, no, Circ0, !VarSet, !Info),

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
			varset__merge_without_names(!.VarSet, EqvVarSet,
				[Body0 | Args0], !:VarSet, [Body | Args]),
			Circ0 = no,
			Circ1 = no
		->
			Changed = yes,
			equiv_type__record_expanded_item(
				item_id(type, EqvTypeCtor), !Info),
			term__term_list_to_var_list(Args, ArgVars),
			term__substitute_corresponding(ArgVars, TArgs1,
							Body, Type1),
			equiv_type__replace_in_type_2(EqvMap,
				[EqvTypeCtor | TypeCtorsAlreadyExpanded],
				Type1, Type, _, Circ, !VarSet, !Info)
		;
			ArgsChanged = yes
		->
			Changed = yes,
			construct_type(EqvTypeCtor, TArgs1, Type),
			bool__or(Circ0, Circ1, Circ)
		;
			Changed = no,
			Type = Type0,
			bool__or(Circ0, Circ1, Circ)
		)
	;
		Changed = no,
		Type = Type0,
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
		TypesAndModes0, TypesAndModes, !TypeVarSet,
		MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
		Det0, Det, !Info, Errors) :-
	equiv_type__replace_in_class_constraints(EqvMap,
		ClassContext0, ClassContext, !TypeVarSet, !Info),
	equiv_type__replace_in_tms(EqvMap, TypesAndModes0,
		TypesAndModes1, !TypeVarSet, !Info),

	(
		MaybeWithType0 = yes(WithType0),
		equiv_type__replace_in_type(EqvMap, WithType0, WithType,
			_, !TypeVarSet, !Info),
		(
			type_is_higher_order(WithType, _Purity, PredOrFunc,
				_EvalMethod, ExtraTypes0)
		->
			ExtraTypes = ExtraTypes0,
			Errors0 = []
		;
			ExtraTypes = [],
			Errors0 = [invalid_with_type(PredName, PredOrFunc)
					- Context]
		)
	;	
		MaybeWithType0 = no,
		ExtraTypes = [],
		Errors0 = []
	),

	equiv_type__replace_in_pred_mode(PredName, length(TypesAndModes0),
		Context, type_decl, EqvInstMap, yes(PredOrFunc), _, ExtraModes,
		MaybeWithInst0, _, Det0, Det, !Info, ModeErrors),
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
		TypesAndModes = TypesAndModes1
	;
		OrigItemId = item_id(pred_or_func_to_item_type(PredOrFunc),
				PredName - list__length(TypesAndModes0)),
		equiv_type__record_expanded_item(OrigItemId, !Info),
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

:- pred equiv_type__replace_in_tms(eqv_map,
		list(type_and_mode), list(type_and_mode), tvarset, tvarset,
		equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_tms(in, in, out, in, out, in, out) is det.

equiv_type__replace_in_tms(EqvMap, !TMs, !VarSet, !Info) :-
	list__map_foldl2(equiv_type__replace_in_tm(EqvMap),
		!TMs, !VarSet, !Info).

:- pred equiv_type__replace_in_tm(eqv_map, type_and_mode, type_and_mode,
	tvarset, tvarset, equiv_type_info, equiv_type_info).
:- mode equiv_type__replace_in_tm(in, in, out, in, out, in, out) is det.

equiv_type__replace_in_tm(EqvMap, type_only(Type0),
		type_only(Type), !VarSet, !Info) :-
	equiv_type__replace_in_type(EqvMap, Type0, Type, _, !VarSet, !Info).

equiv_type__replace_in_tm(EqvMap, type_and_mode(Type0, Mode),
		type_and_mode(Type, Mode), !VarSet, !Info) :-
	equiv_type__replace_in_type(EqvMap, Type0, Type, _, !VarSet, !Info).

%-----------------------------------------------------------------------------%

:- type expanded_item_set == pair(module_name, set(item_id)).

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
		{ Item = type_defn(_, SymName, Params, TypeDefn, _) },
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
