%-----------------------------------------------------------------------------%
% Copyright (C) 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% Main author: stayl.
%
% Expand all types in the module_info using all equivalence
% type definitions, even those local to (transitively) imported 
% modules.
%
% This is necessary to avoid problems with back-ends that
% don't support equivalence types properly (or at all).
%
%-----------------------------------------------------------------------------%
:- module transform_hlds__equiv_type_hlds.

:- interface.

:- import_module hlds__hlds_module.

:- pred replace_in_hlds(module_info::in, module_info::out) is det.

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module check_hlds__polymorphism.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module hlds__hlds_data.
:- import_module hlds__instmap.
:- import_module hlds__quantification.
:- import_module parse_tree__equiv_type.
:- import_module parse_tree__inst.
:- import_module parse_tree__prog_data.
:- import_module recompilation.

:- import_module bool, list, map, require, std_util, term, varset.

replace_in_hlds(!ModuleInfo) :-
	module_info_types(!.ModuleInfo, Types0),
	map__foldl(add_type_to_eqv_map, Types0, map__init, EqvMap),

	module_info_get_maybe_recompilation_info(!.ModuleInfo,
		MaybeRecompInfo0),
	module_info_name(!.ModuleInfo, ModuleName),
	map__map_foldl(replace_in_type_defn(ModuleName, EqvMap), Types0, Types,
		MaybeRecompInfo0, MaybeRecompInfo),
	module_info_set_types(Types, !ModuleInfo),
	module_info_set_maybe_recompilation_info(MaybeRecompInfo, !ModuleInfo),

	module_info_insts(!.ModuleInfo, Insts0),
	replace_in_inst_table(EqvMap, Insts0, Insts),
	module_info_set_insts(Insts, !ModuleInfo),

	module_info_predids(!.ModuleInfo, PredIds),
	list__foldl(replace_in_pred(EqvMap), PredIds, !ModuleInfo).

:- pred add_type_to_eqv_map(type_ctor::in, hlds_type_defn::in,
		eqv_map::in, eqv_map::out) is det.

add_type_to_eqv_map(TypeCtor, Defn, !EqvMap) :-
	hlds_data__get_type_defn_body(Defn, Body),
	( Body = eqv_type(EqvType) ->
		hlds_data__get_type_defn_tvarset(Defn, TVarSet),
		hlds_data__get_type_defn_tparams(Defn, Params),
		map__det_insert(!.EqvMap, TypeCtor,
			eqv_type_body(TVarSet, Params, EqvType), !:EqvMap)
	;
		true
	).

:- pred replace_in_type_defn(module_name::in, eqv_map::in, type_ctor::in,
	hlds_type_defn::in, hlds_type_defn::out,
	maybe(recompilation_info)::in, maybe(recompilation_info)::out) is det.

replace_in_type_defn(ModuleName, EqvMap, TypeCtor,
		Defn0, Defn, !MaybeRecompInfo) :-
	hlds_data__get_type_defn_tvarset(Defn0, TVarSet0),
	hlds_data__get_type_defn_body(Defn0, Body0),
	equiv_type__maybe_record_expanded_items(ModuleName, fst(TypeCtor),
		!.MaybeRecompInfo, EquivTypeInfo0),
	(
		Body0 = du_type(Ctors0, _, _, _, _, _, _),
		equiv_type__replace_in_ctors(EqvMap, Ctors0, Ctors,
			TVarSet0, TVarSet, EquivTypeInfo0, EquivTypeInfo),
		Body = Body0 ^ du_type_ctors := Ctors
	;
		Body0 = eqv_type(Type0),
		equiv_type__replace_in_type(EqvMap, Type0, Type,
			TVarSet0, TVarSet, EquivTypeInfo0, EquivTypeInfo),
		Body = eqv_type(Type)
	;
		Body0 = foreign_type(_, _),
		EquivTypeInfo = EquivTypeInfo0,
		Body = Body0,
		TVarSet = TVarSet0
	;
		Body0 = abstract_type(_),
		EquivTypeInfo = EquivTypeInfo0,
		Body = Body0,
		TVarSet = TVarSet0
	),
	equiv_type__finish_recording_expanded_items(
		item_id(type_body, TypeCtor), EquivTypeInfo,
		!MaybeRecompInfo),
	hlds_data__set_type_defn_body(Defn0, Body, Defn1),
	hlds_data__set_type_defn_tvarset(Defn1, TVarSet, Defn).

:- pred replace_in_inst_table(eqv_map::in,
		inst_table::in, inst_table::out) is det.

replace_in_inst_table(EqvMap, !InstTable) :-
	/*
	%
	% We currently have no syntax for typed user-defined insts,
	% so this is unnecessary.
	%
	inst_table_get_user_insts(!.InstTable, UserInsts0),
	map__map_values(
		(pred(_::in, Defn0::in, Defn::out) is det :-
			Body0 = Defn0 ^ inst_body,
			(
				Body0 = abstract_inst,
				Defn = Defn0
			;
				Body0 = eqv_inst(Inst0),
				% XXX We don't have a valid tvarset here.
				TVarSet0 = varset__init.
				replace_in_inst(EqvMap, Inst0, Inst,
					TVarSet0, _)
			)
		). UserInsts0, UserInsts),
	inst_table_set_user_insts(!.InstTable, UserInsts, !:InstTable),
	*/		

	inst_table_get_unify_insts(!.InstTable, UnifyInsts0),
	inst_table_get_merge_insts(!.InstTable, MergeInsts0),
	inst_table_get_ground_insts(!.InstTable, GroundInsts0),
	inst_table_get_any_insts(!.InstTable, AnyInsts0),
	inst_table_get_shared_insts(!.InstTable, SharedInsts0),
	inst_table_get_mostly_uniq_insts(!.InstTable, MostlyUniqInsts0),
	replace_in_inst_table(replace_in_maybe_inst_det(EqvMap),
		EqvMap, UnifyInsts0, UnifyInsts),
	replace_in_merge_inst_table(EqvMap, MergeInsts0, MergeInsts),
	replace_in_inst_table(replace_in_maybe_inst_det(EqvMap),
		EqvMap, GroundInsts0, GroundInsts),
	replace_in_inst_table(replace_in_maybe_inst_det(EqvMap),
		EqvMap, AnyInsts0, AnyInsts),
	replace_in_inst_table(replace_in_maybe_inst(EqvMap),
		EqvMap, SharedInsts0, SharedInsts),
	replace_in_inst_table(replace_in_maybe_inst(EqvMap),
		EqvMap, MostlyUniqInsts0, MostlyUniqInsts),
	inst_table_set_unify_insts(!.InstTable, UnifyInsts, !:InstTable),
	inst_table_set_merge_insts(!.InstTable, MergeInsts, !:InstTable),
	inst_table_set_ground_insts(!.InstTable, GroundInsts, !:InstTable),
	inst_table_set_any_insts(!.InstTable, AnyInsts, !:InstTable),
	inst_table_set_shared_insts(!.InstTable, SharedInsts, !:InstTable),
	inst_table_set_mostly_uniq_insts(!.InstTable,
		MostlyUniqInsts, !:InstTable).

:- pred replace_in_inst_table(pred(T, T)::(pred(in, out) is det), eqv_map::in,
		map(inst_name, T)::in, map(inst_name, T)::out) is det.

replace_in_inst_table(P, EqvMap, Map0, Map) :-
	map__to_assoc_list(Map0, AL0),
	list__map(
		(pred((Name0 - T0)::in, (Name - T)::out) is det :-
			% XXX We don't have a valid tvarset here.
			varset__init(TVarSet),
			replace_in_inst_name(EqvMap, Name0, Name, TVarSet, _),
			P(T0, T)
		), AL0, AL),
	map__from_assoc_list(AL, Map).

:- pred replace_in_merge_inst_table(eqv_map::in, merge_inst_table::in,
		merge_inst_table::out) is det.

replace_in_merge_inst_table(EqvMap, Map0, Map) :-
	map__to_assoc_list(Map0, AL0),
	list__map(
		(pred(((InstA0 - InstB0) - MaybeInst0)::in,
				((InstA - InstB) - MaybeInst)::out) is det :-
			some [!TVarSet] (
				% XXX We don't have a valid tvarset here.
				!:TVarSet = varset__init,
				replace_in_inst(EqvMap, InstA0, InstA,
					!TVarSet),
				replace_in_inst(EqvMap, InstB0, InstB,
					!.TVarSet, _),
				replace_in_maybe_inst(EqvMap, MaybeInst0,
					MaybeInst)
			)
		), AL0, AL),
	map__from_assoc_list(AL, Map).

:- pred replace_in_maybe_inst(eqv_map::in,
		maybe_inst::in, maybe_inst::out) is det.

replace_in_maybe_inst(_, unknown, unknown).
replace_in_maybe_inst(EqvMap, known(Inst0), known(Inst)) :-
	% XXX We don't have a valid tvarset here.
	varset__init(TVarSet),
	replace_in_inst(EqvMap, Inst0, Inst, TVarSet, _).
	
:- pred replace_in_maybe_inst_det(eqv_map::in,
		maybe_inst_det::in, maybe_inst_det::out) is det.

replace_in_maybe_inst_det(_, unknown, unknown).
replace_in_maybe_inst_det(EqvMap, known(Inst0, Det), known(Inst, Det)) :-
	% XXX We don't have a valid tvarset here.
	varset__init(TVarSet),
	replace_in_inst(EqvMap, Inst0, Inst, TVarSet, _).	

:- pred replace_in_pred(eqv_map::in, pred_id::in,
		module_info::in, module_info::out) is det.

replace_in_pred(EqvMap, PredId, !ModuleInfo) :-
    some [!PredInfo, !EquivTypeInfo] (
	module_info_name(!.ModuleInfo, ModuleName),
	module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
	module_info_get_maybe_recompilation_info(!.ModuleInfo,
		MaybeRecompInfo0),

	PredName = pred_info_name(!.PredInfo),
	equiv_type__maybe_record_expanded_items(ModuleName,
		qualified(ModuleName, PredName),
		MaybeRecompInfo0, !:EquivTypeInfo),

	pred_info_arg_types(!.PredInfo, ArgTVarSet0, ExistQVars, ArgTypes0),
	list__map_foldl2(equiv_type__replace_in_type(EqvMap),
		ArgTypes0, ArgTypes, ArgTVarSet0, ArgTVarSet1, !EquivTypeInfo),

	% The constraint_proofs aren't used after polymorphism,
	% so they don't need to be processed.
	pred_info_get_class_context(!.PredInfo, ClassContext0),
	equiv_type__replace_in_class_constraints(EqvMap, ClassContext0,
		ClassContext, ArgTVarSet1, ArgTVarSet, !EquivTypeInfo),
	pred_info_set_class_context(ClassContext, !PredInfo),
    	pred_info_set_arg_types(ArgTVarSet, ExistQVars, ArgTypes, !PredInfo),

	ItemId = item_id(pred_or_func_to_item_type(
			pred_info_is_pred_or_func(!.PredInfo)),
			qualified(pred_info_module(!.PredInfo), PredName) -
				pred_info_arity(!.PredInfo)),
	equiv_type__finish_recording_expanded_items(ItemId,
		!.EquivTypeInfo, MaybeRecompInfo0, MaybeRecompInfo),
	module_info_set_maybe_recompilation_info(MaybeRecompInfo,
		!ModuleInfo),

    	pred_info_procedures(!.PredInfo, Procs0),
	map__map_foldl(replace_in_proc(EqvMap), Procs0, Procs,
		{!.ModuleInfo, !.PredInfo}, {!:ModuleInfo, !:PredInfo}),
    	pred_info_set_procedures(Procs, !PredInfo),

    	module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
    ).

:- pred replace_in_proc(eqv_map::in, proc_id::in,
	proc_info::in, proc_info::out, {module_info, pred_info}::in,
	{module_info, pred_info}::out) is det.

replace_in_proc(EqvMap, _, !ProcInfo, {!.ModuleInfo, !.PredInfo},
		{!:ModuleInfo, !:PredInfo}) :-
    some [!TVarSet] (
	pred_info_typevarset(!.PredInfo, !:TVarSet),

	proc_info_argmodes(!.ProcInfo, ArgModes0),
	list__map_foldl(replace_in_mode(EqvMap),
		ArgModes0, ArgModes, !TVarSet),
	proc_info_set_argmodes(ArgModes, !ProcInfo),

	proc_info_maybe_declared_argmodes(!.ProcInfo, MaybeDeclModes0),
	(
		MaybeDeclModes0 = yes(DeclModes0),
		list__map_foldl(replace_in_mode(EqvMap),
			DeclModes0, DeclModes, !TVarSet),
		proc_info_set_maybe_declared_argmodes(yes(DeclModes),
			!ProcInfo)
	;
		MaybeDeclModes0 = no
	),

	proc_info_vartypes(!.ProcInfo, VarTypes0),
	map__map_foldl(
		(pred(_::in, VarType0::in, VarType::out,
				!.TVarSet::in, !:TVarSet::out) is det :-
			equiv_type__replace_in_type(EqvMap,
				VarType0, VarType, !TVarSet, no, _)	
		),
		VarTypes0, VarTypes, !TVarSet),
	proc_info_set_vartypes(VarTypes, !ProcInfo),

	proc_info_typeclass_info_varmap(!.ProcInfo, TCVarMap0),
	map__to_assoc_list(TCVarMap0, TCVarAL0),
	list__map_foldl(
		(pred((Constraint0 - Locn)::in, (Constraint - Locn)::out,
				!.TVarSet::in, !:TVarSet::out) is det :-
			equiv_type__replace_in_class_constraint(EqvMap,
				Constraint0, Constraint, !TVarSet, no, _)
		), TCVarAL0, TCVarAL, !TVarSet),
	map__from_assoc_list(TCVarAL, TCVarMap),
	proc_info_set_typeclass_info_varmap(TCVarMap, !ProcInfo),

	proc_info_goal(!.ProcInfo, Goal0),
	replace_in_goal(EqvMap, Goal0, Goal,
		replace_info(!.ModuleInfo, !.PredInfo,
				!.ProcInfo, !.TVarSet, no),
		replace_info(!:ModuleInfo, !:PredInfo,
				!:ProcInfo, !:TVarSet, Recompute)),
	proc_info_set_goal(Goal, !ProcInfo),

	( Recompute = yes ->
		requantify_proc(!ProcInfo),
		recompute_instmap_delta_proc(no, !ProcInfo, !ModuleInfo)
	;
		true
	),

	pred_info_set_typevarset(!.TVarSet, !PredInfo)
    ).

:- pred replace_in_mode(eqv_map::in, (mode)::in, (mode)::out,
		tvarset::in, tvarset::out) is det.

replace_in_mode(EqvMap, (InstA0 -> InstB0), (InstA -> InstB), !TVarSet) :-
	replace_in_inst(EqvMap, InstA0, InstA, !TVarSet),
	replace_in_inst(EqvMap, InstB0, InstB, !TVarSet).
replace_in_mode(EqvMap, user_defined_mode(Name, Insts0),
		user_defined_mode(Name, Insts), !TVarSet) :-
	list__map_foldl(replace_in_inst(EqvMap), Insts0, Insts, !TVarSet).

:- pred replace_in_inst(eqv_map::in, (inst)::in, (inst)::out,
		tvarset::in, tvarset::out) is det.

replace_in_inst(_, any(_) @ Inst, Inst, !TVarSet).
replace_in_inst(_, free @ Inst, Inst, !TVarSet).
replace_in_inst(EqvMap, free(Type0), free(Type), !TVarSet) :-
	equiv_type__replace_in_type(EqvMap, Type0, Type, !TVarSet, no, _).
replace_in_inst(EqvMap, bound(Uniq, BoundInsts0),
		bound(Uniq, BoundInsts), !TVarSet) :-
	list__map_foldl(
		(pred(functor(ConsId, Insts0)::in, functor(ConsId, Insts)::out,
				!.TVarSet::in, !:TVarSet::out) is det :-
			list__map_foldl(replace_in_inst(EqvMap),
				Insts0, Insts, !TVarSet)
		), BoundInsts0, BoundInsts, !TVarSet).
replace_in_inst(_, ground(_, none) @ Inst, Inst, !TVarSet).
replace_in_inst(EqvMap,
		ground(Uniq, higher_order(pred_inst_info(PorF, Modes0, Det))),
		ground(Uniq, higher_order(pred_inst_info(PorF, Modes, Det))),
		!TVarSet) :-
	list__map_foldl(replace_in_mode(EqvMap), Modes0, Modes, !TVarSet).
replace_in_inst(_, not_reached @ Inst, Inst, !TVarSet).
replace_in_inst(_, inst_var(_) @ Inst, Inst, !TVarSet).
replace_in_inst(EqvMap, constrained_inst_vars(Vars, Inst0),
		constrained_inst_vars(Vars, Inst), !TVarSet) :-
	replace_in_inst(EqvMap, Inst0, Inst, !TVarSet).
replace_in_inst(EqvMap, defined_inst(InstName0),
		defined_inst(InstName), !TVarSet) :-
	replace_in_inst_name(EqvMap, InstName0, InstName, !TVarSet).
replace_in_inst(EqvMap, abstract_inst(Name, Insts0),
		abstract_inst(Name, Insts), !TVarSet) :-
	list__map_foldl(replace_in_inst(EqvMap), Insts0, Insts, !TVarSet).

:- pred replace_in_inst_name(eqv_map::in, inst_name::in, inst_name::out,
		tvarset::in, tvarset::out) is det.

replace_in_inst_name(EqvMap, user_inst(Name, Insts0),
		user_inst(Name, Insts), !TVarSet) :-
	list__map_foldl(replace_in_inst(EqvMap), Insts0, Insts, !TVarSet).
replace_in_inst_name(EqvMap, merge_inst(Name, Inst0),
		merge_inst(Name, Inst), !TVarSet) :-
	replace_in_inst(EqvMap, Inst0, Inst, !TVarSet).
replace_in_inst_name(EqvMap, unify_inst(Live, InstA0, InstB0, Real),
		unify_inst(Live, InstA, InstB, Real), !TVarSet) :-
	replace_in_inst(EqvMap, InstA0, InstA, !TVarSet),
	replace_in_inst(EqvMap, InstB0, InstB, !TVarSet).
replace_in_inst_name(EqvMap, ground_inst(Name0, Live, Uniq, Real),
		ground_inst(Name, Live, Uniq, Real), !TVarSet) :-
	replace_in_inst_name(EqvMap, Name0, Name, !TVarSet).
replace_in_inst_name(EqvMap, any_inst(Name0, Live, Uniq, Real),
		any_inst(Name, Live, Uniq, Real), !TVarSet) :-
	replace_in_inst_name(EqvMap, Name0, Name, !TVarSet).
replace_in_inst_name(EqvMap, shared_inst(Name0),
		shared_inst(Name), !TVarSet) :-
	replace_in_inst_name(EqvMap, Name0, Name, !TVarSet).
replace_in_inst_name(EqvMap, mostly_uniq_inst(Name0),
		mostly_uniq_inst(Name), !TVarSet) :-
	replace_in_inst_name(EqvMap, Name0, Name, !TVarSet).
replace_in_inst_name(EqvMap, typed_ground(Uniq, Type0),
		typed_ground(Uniq, Type), !TVarSet) :-
	replace_in_type(EqvMap, Type0, Type, !TVarSet, no, _).
replace_in_inst_name(EqvMap, typed_inst(Type0, Name0),
		typed_inst(Type, Name), !TVarSet) :-
	replace_in_type(EqvMap, Type0, Type, !TVarSet, no, _),
	replace_in_inst_name(EqvMap, Name0, Name, !TVarSet).

:- type replace_info
	---> replace_info(
		module_info :: module_info,
		pred_info :: pred_info,
		proc_info :: proc_info,
		tvarset :: tvarset,
		recompute :: bool
	).

:- pred replace_in_goal(eqv_map::in, hlds_goal::in, hlds_goal::out,
		replace_info::in, replace_info::out) is det.

replace_in_goal(EqvMap, GoalExpr0 - GoalInfo0,
		GoalExpr - GoalInfo, !Info) :-
	replace_in_goal_expr(EqvMap, GoalExpr0, GoalExpr, !Info),

	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
	TVarSet0 = !.Info ^ tvarset,
	instmap_delta_map_foldl(
		(pred(_::in, Inst0::in, Inst::out,
				!.TVarSet::in, !:TVarSet::out) is det :-
			replace_in_inst(EqvMap, Inst0, Inst, !TVarSet)
		), InstMapDelta0, InstMapDelta, TVarSet0, TVarSet),
	!:Info = !.Info ^ tvarset := TVarSet,
	goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo).

:- pred replace_in_goal_expr(eqv_map::in,
		hlds_goal_expr::in, hlds_goal_expr::out,
		replace_info::in, replace_info::out) is det.

replace_in_goal_expr(EqvMap, conj(Goals0), conj(Goals), !Info) :-
	list__map_foldl(replace_in_goal(EqvMap), Goals0, Goals, !Info).
replace_in_goal_expr(EqvMap, par_conj(Goals0), par_conj(Goals), !Info) :-
	list__map_foldl(replace_in_goal(EqvMap), Goals0, Goals, !Info).
replace_in_goal_expr(EqvMap, disj(Goals0), disj(Goals), !Info) :-
	list__map_foldl(replace_in_goal(EqvMap), Goals0, Goals, !Info).
replace_in_goal_expr(EqvMap, switch(A, B, Cases0),
		switch(A, B, Cases), !Info) :-
	list__map_foldl(
		(pred(case(ConsId, Goal0)::in, case(ConsId, Goal)::out,
				!.Info::in, !:Info::out) is det :-
			replace_in_goal(EqvMap, Goal0, Goal, !Info)
		), Cases0, Cases, !Info).
replace_in_goal_expr(EqvMap, not(Goal0), not(Goal), !Info) :-
	replace_in_goal(EqvMap, Goal0, Goal, !Info).
replace_in_goal_expr(EqvMap, some(A, B, Goal0), some(A, B, Goal), !Info) :-
	replace_in_goal(EqvMap, Goal0, Goal, !Info).
replace_in_goal_expr(EqvMap, if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else), !Info) :-
	replace_in_goal(EqvMap, Cond0, Cond, !Info),
	replace_in_goal(EqvMap, Then0, Then, !Info),
	replace_in_goal(EqvMap, Else0, Else, !Info).
replace_in_goal_expr(_, call(_, _, _, _, _, _) @ Goal, Goal, !Info).
replace_in_goal_expr(EqvMap, foreign_proc(_, _, _, _, _, _, _) @ Goal0, Goal,
			!Info) :-
	TVarSet0 = !.Info ^ tvarset,
	list__map_foldl2(replace_in_type(EqvMap), Goal0 ^ foreign_types,
		Types, TVarSet0, TVarSet, no, _),
	!:Info = !.Info ^ tvarset := TVarSet,
	Goal = Goal0 ^ foreign_types := Types.	
replace_in_goal_expr(EqvMap, generic_call(A, B, Modes0, D),
		generic_call(A, B, Modes, D), !Info) :-
	TVarSet0 = !.Info ^ tvarset,
	list__map_foldl(replace_in_mode(EqvMap), Modes0, Modes,
		TVarSet0, TVarSet),
	!:Info = !.Info ^ tvarset := TVarSet.
replace_in_goal_expr(EqvMap, unify(Var, _, _, _, _) @ Goal0, Goal, !Info) :-
	module_info_types(!.Info ^ module_info, Types),
	proc_info_vartypes(!.Info ^ proc_info, VarTypes),
	map__lookup(VarTypes, Var, VarType),
	classify_type(!.Info ^ module_info, VarType) = TypeCat,
	(
		%
		% If this goal constructs a type_info for an equivalence
		% type, we need to expand that to make the type_info for
		% the expanded type.  It's simpler to just recreate the
		% type-info from scratch.
		%
		Goal0 ^ unify_kind = construct(_, ConsId, _, _, _, _, _),
		ConsId = type_info_cell_constructor(TypeCtor),
		TypeCat = type_info_type,
		map__search(Types, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, Body),
		Body = eqv_type(_),
		type_to_ctor_and_args(VarType, _TypeInfoCtor,
				[TypeInfoArgType])
	->
		pred_info_set_typevarset(!.Info ^ tvarset,
			!.Info ^ pred_info, PredInfo0),
		create_poly_info(!.Info ^ module_info,
			PredInfo0, !.Info ^ proc_info, PolyInfo0),
		polymorphism__make_type_info_var(TypeInfoArgType,
			term__context_init, TypeInfoVar,
			Goals0, PolyInfo0, PolyInfo),
		poly_info_extract(PolyInfo, PredInfo0, PredInfo,
			!.Info ^ proc_info, ProcInfo, ModuleInfo),
		pred_info_typevarset(PredInfo, TVarSet),
		!:Info = (((!.Info ^ pred_info := PredInfo)
				^ proc_info := ProcInfo)
				^ module_info := ModuleInfo)
				^ tvarset := TVarSet,

		goal_util__rename_vars_in_goals(Goals0, no,
			map__from_assoc_list([TypeInfoVar - Var]),
			Goals),
		( Goals = [Goal1 - _] ->
			Goal = Goal1
		;
			Goal = conj(Goals)
		),
		!:Info = !.Info ^ recompute := yes
	;
		%
		% Check for a type_ctor_info for an equivalence type.
		% We can just remove these because after the code above
		% to fix up type_infos for equivalence types they can't
		% be used.
		%
		Goal0 ^ unify_kind = construct(_, ConsId, _, _, _, _, _),
		ConsId = type_info_cell_constructor(TypeCtor),
		TypeCat = type_ctor_info_type,
		map__search(Types, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, Body),
		Body = eqv_type(_)
	->
		Goal = conj([]),
		!:Info = !.Info ^ recompute := yes
	;
		Goal0 ^ unify_mode = LMode0 - RMode0,
		TVarSet0 = !.Info ^ tvarset,
		replace_in_mode(EqvMap, LMode0, LMode, TVarSet0, TVarSet1),
		replace_in_mode(EqvMap, RMode0, RMode, TVarSet1, TVarSet),
		!:Info = !.Info ^ tvarset := TVarSet,
		replace_in_unification(EqvMap, Goal0 ^ unify_kind, Unification,
			!Info),
		Goal = (Goal0 ^ unify_mode := LMode - RMode)
				^ unify_kind := Unification
	).
replace_in_goal_expr(_, shorthand(_), _, !Info) :-
	error("replace_in_goal_expr: shorthand").

:- pred replace_in_unification(eqv_map::in, unification::in, unification::out,
		replace_info::in, replace_info::out) is det.

replace_in_unification(_, assign(_, _) @ Uni, Uni, !Info).
replace_in_unification(_, simple_test(_, _) @ Uni, Uni, !Info).
replace_in_unification(EqvMap, complicated_unify(UniMode0, B, C),
		complicated_unify(UniMode, B, C), !Info) :-
	replace_in_uni_mode(EqvMap, UniMode0, UniMode, !Info).
replace_in_unification(EqvMap, construct(_, _, _, _, _, _, _) @ Uni0, Uni,
		!Info) :-
	list__map_foldl(replace_in_uni_mode(EqvMap),
		Uni0 ^ construct_arg_modes, UniModes, !Info),
	Uni = Uni0 ^ construct_arg_modes := UniModes.
replace_in_unification(EqvMap, deconstruct(_, _, _, _, _, _) @ Uni0, Uni,
		!Info) :-
	list__map_foldl(replace_in_uni_mode(EqvMap),
		Uni0 ^ deconstruct_arg_modes, UniModes, !Info),
	Uni = Uni0 ^ deconstruct_arg_modes := UniModes.

:- pred replace_in_uni_mode(eqv_map::in, uni_mode::in, uni_mode::out,
		replace_info::in, replace_info::out) is det.

replace_in_uni_mode(EqvMap, ((InstA0 - InstB0) -> (InstC0 - InstD0)),
		((InstA - InstB) -> (InstC - InstD)), !Info) :-
	some [!TVarSet] (
		!:TVarSet = !.Info ^ tvarset,
		replace_in_inst(EqvMap, InstA0, InstA, !TVarSet),
		replace_in_inst(EqvMap, InstB0, InstB, !TVarSet),
		replace_in_inst(EqvMap, InstC0, InstC, !TVarSet),
		replace_in_inst(EqvMap, InstD0, InstD, !TVarSet),
		!:Info = !.Info ^ tvarset := !.TVarSet
	).
