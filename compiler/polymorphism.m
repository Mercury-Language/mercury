%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: polymorphism.m
% main author: fjh

% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphism
% using higher-order predicates, and also invokes `lambda__transform_lambda'
% to handle lambda expressions by creating new predicates for them.
%
%-----------------------------------------------------------------------------%
%
% Tranformation of polymorphic code:
%
% Every polymorphic predicate is transformed so that it takes one additional
% argument for every type variable in the predicate's type declaration.
% The argument gives information about the type, including higher-order
% predicate variables for each of the builtin polymorphic operations
% (currently unify/2, compare/3, index/2).
%
%-----------------------------------------------------------------------------%
%
% Representation of type information:
%
% IMPORTANT: ANY CHANGES TO THE DOCUMENTATION HERE MUST BE REFLECTED BY
% SIMILAR CHANGES TO THE #defines IN "runtime/type_info.h"
% AND VICE VERSA.
%
% Type information is represented using one or two cells. The cell which
% is always present is the base_type_info structure, laid out like this:
%
%	word 0		<arity of type constructor>
%			e.g. 0 for `int', 1 for `list(T)', 2 for `map(K, V)'.
%	word 1		<=/2 predicate for type>
%	word 2		<index/2 predicate for type>
%	word 3		<compare/3 predicate for type>
%	word 4		<base_type_layout for type>
%	word 5		<base_type_functors for type>
%	word 6		<string name of type>
%			e.g. "int" for `int', "list" for `list(T)',
%			"map" for `map(K,V)'
%
% The other cell is the new type_info structure, laid out like this:
%
%	word 0		<pointer to the base_type_info structure>
%	word 1+		<the type_infos for the type params, at least one>
%
%	(but see note below for how higher order types differ)
%
%-----------------------------------------------------------------------------%
%
% Optimization of common case for one-or-two cells:
%
% The type_info structure itself is redundant if the type has no type
% parameters (i.e. its arity is zero). Therefore if the arity is zero,
% we pass the address of the base_type_info structure directly, instead of
% wrapping it up in another cell. The runtime system will look at the first
% field of the cell it is passed. If this field is zero, the cell is a
% base_type_info structure for an arity zero type. If this field is not zero,
% the cell is a new type_info structure, with the first field being the
% pointer to the base_type_info structure.
%
%-----------------------------------------------------------------------------%
%
% Higher order types:
%
% There is a slight variation on this for higher-order types. Higher
% order type_infos always have a pointer to the pred/0 base_type_info,
% regardless of their true arity, so we store the real arity in the
% type-info as well.
%
%	word 0		<pointer to the base_type_info structure (pred/0)>
%	word 1		<arity of predicate>
%	word 2+		<the type_infos for the type params, at least one>
%
%-----------------------------------------------------------------------------%
%
% Sharing one-or-two-cell structures:
%
% For compilation models that can put code addresses in static ground terms,
% we can arrange to create one copy of the base_type_info structure statically,
% avoiding the need to create other copies at runtime. For compilation models
% that cannot put code addresses in static ground terms, we have several
% options:
%
% 	1. use a one or two cell representation, but allocate all cells 
% 	   at runtime.
%	2. use another representation, allocating all cells at
%	   runtime.
%	3. use a shared static base_type_info, but initialize its code
%	   addresses during startup (that is, during the module
%	   initialization code).
%
% Presently, shared-one-or-two cells are the default, with grades that
% cannot use static code addresses using option 3.  Support for older
% type_info representations has been dropped. 
%
%-----------------------------------------------------------------------------%
%
% Example of transformation:
%
% Take the following code as an example, ignoring the requirement for
% super-homogeneous form for clarity:
%
%	:- pred p(T1).
%	:- pred q(T2).
%	:- pred r(T3).
%
%	p(X) :- q([X]), r(0).
%
% We add an extra argument for each type variable:
%
%	:- pred p(type_info(T1), T1).
%	:- pred q(type_info(T2), T2).
%	:- pred r(type_info(T3), T3).
%
% We transform the body of p to this:
%
%	p(TypeInfoT1, X) :-
%		BaseTypeInfoT2 = base_type_info(
%			1,
%			'__Unify__'<list/1>,
%			'__Index__'<list/1>,
%			'__Compare__'<list/1>,
%			<base_type_layout for list/1>,
%			<base_type_functors for list/1>,
%			"list"),
%		TypeInfoT2 = type_info(
%			BaseTypeInfoT2,
%			TypeInfoT1),
%		q(TypeInfoT2, [X]),
%		TypeInfoT3 = base_type_info(
%			0,
%			builtin_unify_int,
%			builtin_index_int,
%			builtin_compare_int,
%			<base_type_layout for int/0>,
%			<base_type_functors for int/0>,
%			"int"),
%		r(TypeInfoT3, 0).
%
% Note that base_type_infos are actually generated as references to a
% single shared base_type_info.
%
%-----------------------------------------------------------------------------%

:- module polymorphism.
:- interface.
:- import_module hlds_module.

:- pred polymorphism__process_module(module_info, module_info).
:- mode polymorphism__process_module(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, llds, (lambda), globals.
:- import_module prog_data, type_util, mode_util, quantification, instmap.
:- import_module code_util, unify_proc, special_pred, prog_util, make_hlds.

:- import_module bool, int, string, list, set, map.
:- import_module term, varset, std_util, require.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.
	% We do two passes, the first to fix up the procedure bodies,
	% (and in fact everything except the pred_info argtypes),
	% the second to fix up the pred_info argtypes.
	% The reason we need two passes is that the first pass looks at
	% the argtypes of the called predicates, and so we need to make
	% sure we don't muck them up before we've finished the first pass.

polymorphism__process_module(ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds0),
	map__keys(Preds0, PredIds0),
	polymorphism__process_preds(PredIds0, ModuleInfo0, ModuleInfo1),
	module_info_preds(ModuleInfo1, Preds1),
	map__keys(Preds1, PredIds1),
	polymorphism__fixup_preds(PredIds1, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__process_preds(in, in, out) is det.

polymorphism__process_preds([], ModuleInfo, ModuleInfo).
polymorphism__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo1),
	polymorphism__process_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_pred(pred_id, module_info, module_info).
:- mode polymorphism__process_pred(in, in, out) is det.

polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	polymorphism__process_procs(PredId, ProcIds, ModuleInfo0, ModuleInfo).

:- pred polymorphism__process_procs(pred_id, list(proc_id),
					module_info, module_info).
:- mode polymorphism__process_procs(in, in, in, out) is det.

polymorphism__process_procs(_PredId, [], ModuleInfo, ModuleInfo).
polymorphism__process_procs(PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo0,
					ProcInfo, PredInfo1, ModuleInfo1),

	pred_info_procedures(PredInfo1, ProcTable1),
	map__det_update(ProcTable1, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	module_info_preds(ModuleInfo1, PredTable1),
	map__det_update(PredTable1, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),

	polymorphism__process_procs(PredId, ProcIds, ModuleInfo2, ModuleInfo).

%---------------------------------------------------------------------------%

:- pred polymorphism__fixup_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__fixup_preds(in, in, out) is det.

polymorphism__fixup_preds([], ModuleInfo, ModuleInfo).
polymorphism__fixup_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	%
	% Recompute the arg types by finding the headvars and the var->type
	% mapping (from the first procedure for the predicate) and
	% applying the type mapping to the extra headvars to get the new
	% arg types.  Note that we are careful to only apply the mapping
	% to the extra head vars, not to the originals, because otherwise
	% we would stuff up the arg types for unification predicates for
	% equivalence types.
	%
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	pred_info_procids(PredInfo0, ProcIds),
	( ProcIds = [ProcId | _] ->
		map__lookup(ProcTable0, ProcId, ProcInfo),
		proc_info_vartypes(ProcInfo, VarTypes),
		proc_info_headvars(ProcInfo, HeadVars),
		pred_info_arg_types(PredInfo0, TypeVarSet, ArgTypes0),
		list__length(ArgTypes0, NumOldArgs),
		list__length(HeadVars, NumNewArgs),
		NumExtraArgs is NumNewArgs - NumOldArgs,
		(
			list__split_list(NumExtraArgs, HeadVars, ExtraHeadVars,
					_OldHeadVars)
		->
			map__apply_to_list(ExtraHeadVars, VarTypes,
				ExtraArgTypes),
			list__append(ExtraArgTypes, ArgTypes0, ArgTypes)
		;
			error("polymorphism.m: list__split_list failed")
		),

		pred_info_set_arg_types(PredInfo0, TypeVarSet, ArgTypes,
			PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1)
	;
		ModuleInfo1 = ModuleInfo0
	),
	polymorphism__fixup_preds(PredIds, ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%

:- type poly_info --->
		poly_info(
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			tvarset,		% from the proc_info
			map(tvar, var),		% specifies the type_info var
						% for each of the pred's type
						% parameters
			module_info
		).

:- pred polymorphism__process_proc(proc_info, pred_info, module_info,
				proc_info, pred_info, module_info).
:- mode polymorphism__process_proc(in, in, in, out, out, out) is det.

polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo0,
				ProcInfo, PredInfo, ModuleInfo) :-
	% grab the appropriate fields from the pred_info and proc_info
	pred_info_arg_types(PredInfo0, ArgTypeVarSet, ArgTypes),
	pred_info_typevarset(PredInfo0, TypeVarSet0),
	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_variables(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	% insert extra head variables to hold the address of the
	% equality predicate for each polymorphic type in the predicate's
	% type declaration
	term__vars_list(ArgTypes, HeadTypeVars0),
	list__remove_dups(HeadTypeVars0, HeadTypeVars), % remove duplicates
	polymorphism__make_head_vars(HeadTypeVars, ArgTypeVarSet,
		VarSet0, VarTypes0, ExtraHeadVars, VarSet1, VarTypes1),
	list__append(ExtraHeadVars, HeadVars0, HeadVars),
	list__length(ExtraHeadVars, NumExtraVars),
	list__duplicate(NumExtraVars, user_defined_mode(
		qualified("mercury_builtin", "in"), []), ExtraModes),
	list__append(ExtraModes, ArgModes0, ArgModes),

	% process any polymorphic calls inside the goal
	map__from_corresponding_lists(HeadTypeVars, ExtraHeadVars,
				TypeInfoMap0),
	Info0 = poly_info(VarSet1, VarTypes1, TypeVarSet0,
				TypeInfoMap0, ModuleInfo0),
	polymorphism__process_goal(Goal0, Goal1, Info0, Info1),
	polymorphism__fixup_quantification(Goal1, Goal, Info1, Info),
	Info = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap, ModuleInfo),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo4),
	proc_info_set_argmodes(ProcInfo4, ArgModes, ProcInfo5),
	proc_info_set_typeinfo_varmap(ProcInfo5, TypeInfoMap, ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo).

:- pred polymorphism__process_goal(hlds_goal, hlds_goal,
					poly_info, poly_info).
:- mode polymorphism__process_goal(in, out, in, out) is det.

polymorphism__process_goal(Goal0 - GoalInfo0, Goal) -->
	polymorphism__process_goal_expr(Goal0, GoalInfo0, Goal).

:- pred polymorphism__process_goal_expr(hlds_goal_expr, hlds_goal_info,
					hlds_goal, poly_info, poly_info).
:- mode polymorphism__process_goal_expr(in, in, out, in, out) is det.

	% We don't need to add type-infos for higher-order calls,
	% since the type-infos are added when the closures are
	% constructed, not when they are called.  (Or at least I
	% think we don't... -fjh.)
polymorphism__process_goal_expr(higher_order_call(A, B, C, D, E),
		GoalInfo, higher_order_call(A, B, C, D, E) - GoalInfo)
		--> [].

polymorphism__process_goal_expr(call(PredId0, ProcId0, ArgVars0,
		Builtin, Context, Name0), GoalInfo, Goal) -->
	% Check for a call to a special predicate like compare/3
	% for which the type is known at compile-time.
	% Replace such calls with calls to the particular version
	% for that type.
	(
		{ Name0 = unqualified(PredName0) },
		{ list__length(ArgVars0, Arity) },
		{ special_pred_name_arity(SpecialPredId, PredName0,
						MangledPredName, Arity) },
		=(poly_info(_, VarTypes, _, _TypeInfoMap, ModuleInfo)),
		{ special_pred_get_type(MangledPredName, ArgVars0, MainVar) },
		{ map__lookup(VarTypes, MainVar, Type) },
		{ Type \= term__variable(_) },

		% don't try this for any special preds if they're not
		% implemented

		{ special_pred_list(SpecialPredIds) },
		{ list__member(SpecialPredId, SpecialPredIds) }
	->
		{ classify_type(Type, ModuleInfo, TypeCategory) },
		{ polymorphism__get_special_proc(TypeCategory, SpecialPredId,
			ModuleInfo, Name, PredId, ProcId) }
	;
		{ PredId = PredId0 },
		{ ProcId = ProcId0 },
		{ Name = Name0 }
	),

	polymorphism__process_call(PredId, ProcId, ArgVars0,
		ArgVars, ExtraVars, ExtraGoals),
	{ goal_info_get_nonlocals(GoalInfo, NonLocals0) },
	{ set__insert_list(NonLocals0, ExtraVars, NonLocals) },
	{ goal_info_set_nonlocals(GoalInfo, NonLocals, CallGoalInfo) },
	{ Call = call(PredId, ProcId, ArgVars, Builtin, Context, Name)
		- CallGoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

polymorphism__process_goal_expr(unify(XVar, Y, Mode, Unification, Context),
				GoalInfo, Goal) -->
	(
		{ Unification = complicated_unify(UniMode, CanFail) },
		{ Y = var(YVar) }
	->
		=(poly_info(_, VarTypes, _, TypeInfoMap, ModuleInfo)),
		{ map__lookup(VarTypes, XVar, Type) },
		( { Type = term__variable(TypeVar) } ->
			% Convert polymorphic unifications into calls to
			% `unify/2', the general unification predicate, passing
			% the appropriate Type_info
			% 	=(TypeInfoVar, X, Y)
			% where TypeInfoVar is the type_info variable
			% associated with the type of the variables that
			% are being unified.

			{ module_info_get_predicate_table(ModuleInfo,
				PredicateTable) },
			{ predicate_table_search_pred_m_n_a(PredicateTable,
				"mercury_builtin", "unify", 2, [CallPredId]) ->
				PredId = CallPredId
			;
				error("polymorphism.m: can't find `mercury_builtin:unify/2'")
			},
			% XXX Bug! - we should check that the mode is (in, in),
			%     and report an error (e.g. "unification of
			%     polymorphically typed variables in partially
			%     instantiated mode") if it isn't
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{ map__lookup(TypeInfoMap, TypeVar, TypeInfoVar) },
			{ SymName = unqualified("unify") },
			{ ArgVars = [TypeInfoVar, XVar, YVar] },
			{ code_util__builtin_state(ModuleInfo, PredId, ProcId,
				BuiltinState) },
			{ CallContext = call_unify_context(XVar, Y, Context) },
			{ Goal = call(PredId, ProcId, ArgVars, BuiltinState,
				yes(CallContext), SymName) - GoalInfo }

		; { type_is_higher_order(Type, _, _) } ->
			{ SymName = unqualified("builtin_unify_pred") },
			{ ArgVars = [XVar, YVar] },
			{ module_info_get_predicate_table(ModuleInfo,
				PredicateTable) },
			{
				predicate_table_search_pred_m_n_a(
				    PredicateTable,
				    "mercury_builtin", "builtin_unify_pred", 2,
				    [PredId0])
			->
				PredId = PredId0
			;
				error("can't locate mercury_builtin:builtin_unify_pred/2")
			},
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{ CallContext = call_unify_context(XVar, Y, Context) },
			{ Call = call(PredId, ProcId, ArgVars, not_builtin,
				yes(CallContext), SymName) },
			polymorphism__process_goal_expr(Call, GoalInfo, Goal)
			
		; { type_to_type_id(Type, TypeId, _) } ->

			% Convert other complicated unifications into
			% calls to specific unification predicates, and then
			% recursively call polymorphism__process_goal_expr
			% to insert extra arguments if necessary.

			{ module_info_get_special_pred_map(ModuleInfo,
				SpecialPredMap) },
			{ map__lookup(SpecialPredMap, unify - TypeId, PredId) },
			{ determinism_components(Det, CanFail, at_most_one) },
			{ unify_proc__lookup_mode_num(ModuleInfo, TypeId,
				UniMode, Det, ProcId) },
			{ SymName = unqualified("__Unify__") },
			{ ArgVars = [XVar, YVar] },
			{ CallContext = call_unify_context(XVar, Y, Context) },
			{ Call = call(PredId, ProcId, ArgVars, not_builtin,
				yes(CallContext), SymName) },
			polymorphism__process_goal_expr(Call, GoalInfo, Goal)
		;
			{ error("polymorphism: type_to_type_id failed") }
		)
	; { Y = lambda_goal(PredOrFunc, Vars, Modes, Det, LambdaGoal0) } ->
		% for lambda expressions, we must recursively traverse the
		% lambda goal and then convert the lambda expression
		% into a new predicate
		{ LambdaGoal0 = _ - GoalInfo0 },
		{ goal_info_get_nonlocals(GoalInfo0, OrigNonLocals) },
		polymorphism__process_goal(LambdaGoal0, LambdaGoal1),
		polymorphism__fixup_quantification(LambdaGoal1, LambdaGoal),
		polymorphism__process_lambda(PredOrFunc, Vars, Modes, Det,
				OrigNonLocals, LambdaGoal, Unification,
				Y1, Unification1),
		{ Goal = unify(XVar, Y1, Mode, Unification1, Context)
				- GoalInfo }
	;
		% ordinary unifications are left unchanged,
		{ Goal = unify(XVar, Y, Mode, Unification, Context) - GoalInfo }
	).

	% the rest of the clauses just process goals recursively

polymorphism__process_goal_expr(conj(Goals0), GoalInfo,
		conj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(disj(Goals0, SM), GoalInfo,
		disj(Goals, SM) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_expr(switch(Var, CanFail, Cases0, SM), GoalInfo,
				switch(Var, CanFail, Cases, SM) - GoalInfo) -->
	polymorphism__process_case_list(Cases0, Cases).
polymorphism__process_goal_expr(some(Vars, Goal0), GoalInfo,
			some(Vars, Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_expr(if_then_else(Vars, A0, B0, C0, SM), GoalInfo,
			if_then_else(Vars, A, B, C, SM) - GoalInfo) -->
	polymorphism__process_goal(A0, A),
	polymorphism__process_goal(B0, B),
	polymorphism__process_goal(C0, C).

polymorphism__process_goal_expr(pragma_c_code(IsRecursive, C_Code, PredId,
		ProcId, ArgVars0, ArgNames0, OrigArgTypes0, ExtraInfo),
		GoalInfo, Goal) -->
	polymorphism__process_call(PredId, ProcId, ArgVars0,
		ArgVars, ExtraVars, ExtraGoals),
	%
	% update the non-locals
	%
	{ goal_info_get_nonlocals(GoalInfo, NonLocals0) },
	{ set__insert_list(NonLocals0, ExtraVars, NonLocals) },
	{ goal_info_set_nonlocals(GoalInfo, NonLocals, CallGoalInfo) },

	%
	% insert the type_info vars into the arg-name map,
	% so that the c_code can refer to the type_info variable
	% for type T as `TypeInfo_for_T'.
	%
	=(poly_info(_, _, _, _, ModuleInfo)),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_arg_types(PredInfo, PredTypeVarSet, PredArgTypes) },
	{ term__vars_list(PredArgTypes, PredTypeVars0) },
	{ list__remove_dups(PredTypeVars0, PredTypeVars) },
	{ polymorphism__c_code_add_typeinfos(ExtraVars, PredTypeVars,
			PredTypeVarSet, ArgNames0, ArgNames) },

	%
	% insert type_info types for all the inserted type_info vars
	% into the arg-types list
	%
	{ MakeType = lambda([TypeVar::in, TypeInfoType::out] is det,
		construct_type(qualified("mercury_builtin", "type_info") - 1,
			[term__variable(TypeVar)], TypeInfoType)) },
	{ list__map(MakeType, PredTypeVars, TypeInfoTypes) },
	{ list__append(TypeInfoTypes, OrigArgTypes0, OrigArgTypes) },

	%
	% plug it all back together
	%
	{ Call = pragma_c_code(IsRecursive, C_Code, PredId, ProcId, ArgVars,
			ArgNames, OrigArgTypes, ExtraInfo) - CallGoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

:- pred polymorphism__c_code_add_typeinfos(list(var), list(tvar),
			tvarset, list(maybe(string)), list(maybe(string))).
:- mode polymorphism__c_code_add_typeinfos(in, in, in, in, out) is det.

polymorphism__c_code_add_typeinfos([], [], _, ArgNames, ArgNames).
polymorphism__c_code_add_typeinfos([_Var|Vars], [TVar|TVars], TypeVarSet,
		ArgNames0, ArgNames) :-
	polymorphism__c_code_add_typeinfos(Vars, TVars, TypeVarSet,
		ArgNames0, ArgNames1),
	( varset__search_name(TypeVarSet, TVar, TypeVarName) ->
		string__append("TypeInfo_for_", TypeVarName, C_VarName),
		ArgNames = [yes(C_VarName) | ArgNames1]
	;
		ArgNames = [no | ArgNames1]
	).
polymorphism__c_code_add_typeinfos([], [_|_], _, _, _) :-
	error("polymorphism__c_code_add_typeinfos: length mismatch").
polymorphism__c_code_add_typeinfos([_|_], [], _, _, _) :-
	error("polymorphism__c_code_add_typeinfos: length mismatch").

:- pred polymorphism__process_goal_list(list(hlds_goal), list(hlds_goal),
					poly_info, poly_info).
:- mode polymorphism__process_goal_list(in, out, in, out) is det.

polymorphism__process_goal_list([], []) --> [].
polymorphism__process_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	polymorphism__process_goal(Goal0, Goal),
	polymorphism__process_goal_list(Goals0, Goals).

:- pred polymorphism__process_case_list(list(case), list(case),
					poly_info, poly_info).
:- mode polymorphism__process_case_list(in, out, in, out) is det.

polymorphism__process_case_list([], []) --> [].
polymorphism__process_case_list([Case0 | Cases0], [Case | Cases]) -->
	{ Case0 = case(ConsId, Goal0) },
	polymorphism__process_goal(Goal0, Goal),
	{ Case = case(ConsId, Goal) },
	polymorphism__process_case_list(Cases0, Cases).

%-----------------------------------------------------------------------------%

:- pred polymorphism__process_call(pred_id, proc_id, list(var), list(var),
					list(var), list(hlds_goal),
					poly_info, poly_info).
:- mode polymorphism__process_call(in, in, in, out, out, out, in, out) is det.

polymorphism__process_call(PredId, _ProcId, ArgVars0, ArgVars,
				ExtraVars, ExtraGoals, Info0, Info) :-
	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet0,
				TypeInfoMap0, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredArgTypes0),
		% rename apart
		% (this merge might be a performance bottleneck?)
	varset__merge(TypeVarSet0, PredTypeVarSet, PredArgTypes0,
			TypeVarSet, PredArgTypes),
	term__vars_list(PredArgTypes, PredTypeVars0),
	( PredTypeVars0 = [] ->
		% optimize for common case of non-polymorphic call
		ArgVars = ArgVars0,
		ExtraGoals = [],
		ExtraVars = [],
		Info = Info0
	;
		list__remove_dups(PredTypeVars0, PredTypeVars),
		map__apply_to_list(ArgVars0, VarTypes0, ActualArgTypes),
		( type_list_subsumes(PredArgTypes, ActualArgTypes,
				TypeSubst1) ->
			TypeSubst = TypeSubst1
		;
		error("polymorphism__process_goal_expr: type unification failed")
		),
		term__var_list_to_term_list(PredTypeVars, PredTypes0),
		term__apply_rec_substitution_to_list(PredTypes0, TypeSubst,
			PredTypes),
		polymorphism__make_vars(PredTypes, ModuleInfo, TypeInfoMap0,
				VarSet0, VarTypes0,
				ExtraVars, TypeInfoMap, ExtraGoals, VarSet, 
				VarTypes),
		list__append(ExtraVars, ArgVars0, ArgVars),
		Info = poly_info(VarSet, VarTypes, TypeVarSet,
				TypeInfoMap, ModuleInfo)
	).

:- pred polymorphism__fixup_quantification(hlds_goal, hlds_goal,
		poly_info, poly_info).
:- mode polymorphism__fixup_quantification(in, out, in, out) is det.

%
% If the predicate we are processing is a polymorphic predicate, we
% may need to fix up the quantification (non-local variables)
%

polymorphism__fixup_quantification(Goal0, Goal, Info0, Info) :-
	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet, TypeVarMap,
			ModuleInfo),
	map__values(TypeVarMap, ExtraHeadVars),
	( ExtraHeadVars = [] ->
		Goal = Goal0,
		VarTypes = VarTypes0,
		VarSet = VarSet0
	;
		Goal0 = _ - GoalInfo0,
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__list_to_set(ExtraHeadVars, NewOutsideVars),
		set__union(NewOutsideVars, NonLocals, OutsideVars),
		implicitly_quantify_goal(Goal0, VarSet0, VarTypes0,
			OutsideVars, Goal, VarSet, VarTypes, _Warnings)
	),
	Info = poly_info(VarSet, VarTypes, TypeVarSet, TypeVarMap, ModuleInfo).

:- pred polymorphism__process_lambda(pred_or_func, list(var), list(mode),
		determinism, set(var), hlds_goal, unification,
		unify_rhs, unification, poly_info, poly_info).
:- mode polymorphism__process_lambda(in, in, in, in, in, in, in, out, out,
		in, out) is det.

polymorphism__process_lambda(PredOrFunc, Vars, Modes, Det, OrigNonLocals,
		LambdaGoal, Unification0, Functor, Unification,
		PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(VarSet, VarTypes, TVarSet, TVarMap, ModuleInfo0),
	lambda__transform_lambda(PredOrFunc, Vars, Modes, Det, OrigNonLocals,
		LambdaGoal, Unification0, VarSet, VarTypes, TVarSet, TVarMap,
		ModuleInfo0, Functor, Unification, ModuleInfo),
	PolyInfo = poly_info(VarSet, VarTypes, TVarSet, TVarMap, ModuleInfo).

%---------------------------------------------------------------------------%

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

:- pred polymorphism__make_vars(list(type), module_info, map(tvar, var),
	varset, map(var, type), list(var), map(tvar, var), list(hlds_goal),
	varset, map(var, type)).
:- mode polymorphism__make_vars(in, in, in, in, in, out, out, out, out, 
	out) is det.

polymorphism__make_vars([], _, TypeInfoMap, VarSet, VarTypes, [], TypeInfoMap,
		[], VarSet, VarTypes).
polymorphism__make_vars([Type | Types], ModuleInfo, TypeInfoMap0,
		VarSet0, VarTypes0, ExtraVars, TypeInfoMap, ExtraGoals, 
		VarSet, VarTypes) :-
	polymorphism__make_var(Type, ModuleInfo, TypeInfoMap0,
		VarSet0, VarTypes0, Var, TypeInfoMap1, ExtraGoals1, VarSet1,
		VarTypes1),
	polymorphism__make_vars(Types, ModuleInfo, TypeInfoMap1,
		VarSet1, VarTypes1, ExtraVars2, TypeInfoMap, ExtraGoals2, 
		VarSet, VarTypes),
	ExtraVars = [Var | ExtraVars2],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals).

:- pred polymorphism__make_var(type, module_info, map(tvar, var), 
	varset, map(var, type), var, map(tvar, var), list(hlds_goal),
	varset, map(var, type)).
:- mode polymorphism__make_var(in, in, in, in, in, out, out, out, out, out) 
	is det.

polymorphism__make_var(Type, ModuleInfo, TypeInfoMap0, VarSet0, VarTypes0, 
		Var, TypeInfoMap, ExtraGoals, VarSet, VarTypes) :-
	(
		type_is_higher_order(Type, _PredOrFunc, TypeArgs)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known higher-order value of the type
		% variable.
		% The transformation we perform is basically the same
		% as in the first-order case below, except that
		% we ignore the PredOrFunc and map all pred/func types to 
		% builtin pred/0 for the purposes of creating type_infos.
		% To allow univ_to_type to check the type_infos
		% correctly, the actual arity of the pred is added to 
		% the type_info of higher-order types.
		TypeId = unqualified("pred") - 0,
		polymorphism__construct_type_info(Type, TypeId, TypeArgs,
			yes, ModuleInfo, TypeInfoMap0, VarSet0, VarTypes0,
			Var, TypeInfoMap, ExtraGoals, VarSet, VarTypes)
	;
		type_to_type_id(Type, TypeId, TypeArgs)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known value of the type variable.
		% The transformation we perform is shown in the comment
		% at the top of the module.

		polymorphism__construct_type_info(Type, TypeId, TypeArgs,
			no, ModuleInfo, TypeInfoMap0, VarSet0, VarTypes0,
			Var, TypeInfoMap, ExtraGoals, VarSet, VarTypes)
	;
		Type = term__variable(TypeVar1),
		map__search(TypeInfoMap0, TypeVar1, TypeInfoVar)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a bound but unknown value of the type variable.
		% For example, in
		%
		%	:- pred p(T1).
		%	:- pred q(T2).
		%
		%	p(X) :- q(X).
		%
		% we know that `T2' is bound to `T1', and we translate it into
		%
		%	:- pred p(TypeInfo(T1), T1).
		%	:- pred q(TypeInfo(T2), T2).
		%
		%	p(TypeInfo, X) :- q(TypeInfo, X).

		Var = TypeInfoVar,
		ExtraGoals = [],
		VarSet = VarSet0,
		VarTypes = VarTypes0,
		TypeInfoMap = TypeInfoMap0
	;
		Type = term__variable(TypeVar1)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with an unbound type variable, for example
		%
		%	:- pred p.
		%	:- pred q(list(T)).
		%	p :- q([]).
		%
		% In this case T is unbound, so there cannot be any objects
		% of type T, and so q/1 cannot possibly use the unification
		% predicate for type T.  We pass the type-info for the
		% type `void'/0.
		%
		%	:- pred p.
		%	:- pred q(type_info(T), list(T)).
		%	p :- q(<void/0>, []).
		%
		% Passing `void'/0 should ensure that we get a runtime
		% error if the special predicates for this type are
		% ever used (void has its special predicates set to
		% `unused'/0).
		%
		% XXX what about io__read_anything/3?
		% e.g.
		%	foo --> io__read_anything(_).
		% ?

		% introduce a new variable, and
		% create a construction unification which initializes the
		% variable to zero
		TypeId = unqualified("void") - 0,
		polymorphism__construct_type_info(Type, TypeId, [],
			no, ModuleInfo, TypeInfoMap0, VarSet0, VarTypes0,
			Var, TypeInfoMap1, ExtraGoals, VarSet, VarTypes),
		map__det_insert(TypeInfoMap1, TypeVar1, Var, TypeInfoMap)
	;
		error("polymorphism__make_var: unknown type")
	).

:- pred polymorphism__construct_type_info(type, type_id, list(type),
	bool, module_info, map(tvar, var), varset, map(var, type),
	var, map(tvar, var), list(hlds_goal), varset, map(var, type)).
:- mode polymorphism__construct_type_info(in, in, in, in, in, in, in, in,
	out, out, out, out, out) is det.

polymorphism__construct_type_info(Type, TypeId, TypeArgs, IsHigherOrder, 
		ModuleInfo, TypeInfoMap0, VarSet0, VarTypes0,
		Var, TypeInfoMap, ExtraGoals, VarSet, VarTypes) :-

	% Create the typeinfo vars for the arguments
	polymorphism__make_vars(TypeArgs, ModuleInfo, TypeInfoMap0,
		VarSet0, VarTypes0, ArgTypeInfoVars, TypeInfoMap, 
		ArgTypeInfoGoals, VarSet1, VarTypes1),

	module_info_globals(ModuleInfo, Globals),
	globals__get_type_info_method(Globals, TypeInfoMethod),
	(
		TypeInfoMethod = shared_one_or_two_cell,

		polymorphism__init_const_base_type_info_var(Type,
			TypeId, ModuleInfo, VarSet1, VarTypes1, 
			BaseVar, BaseGoal, VarSet2, VarTypes2),
		polymorphism__maybe_init_second_cell(ArgTypeInfoVars,
			ArgTypeInfoGoals, Type, IsHigherOrder,
			BaseVar, VarSet2, VarTypes2, [BaseGoal],
			Var, VarSet, VarTypes, ExtraGoals)
	).

		% Create a unification for the two-cell type_info
		% variable for this type if the type arity is not zero:
		%	TypeInfoVar = type_info(BaseVar,
		%				ArgTypeInfoVars...).
		% For closures, we add the actual arity before the
		% arguments, because all closures have a BaseVar
		% of "pred/0".
		% 	TypeInfoVar = type_info(BaseVar, Arity,
		% 				ArgTypeInfoVars...).

:- pred polymorphism__maybe_init_second_cell(list(var), list(hlds_goal), type,
	bool, var, varset, map(var, type), list(hlds_goal),
	var, varset, map(var, type), list(hlds_goal)).
:- mode polymorphism__maybe_init_second_cell(in, in, in, in, in, in, in, in,
	out, out, out, out) is det.

polymorphism__maybe_init_second_cell(ArgTypeInfoVars, ArgTypeInfoGoals, Type,
		IsHigherOrder, BaseVar, VarSet0, VarTypes0, ExtraGoals0,
		Var, VarSet, VarTypes, ExtraGoals) :-
	( 
		ArgTypeInfoVars = [],
		IsHigherOrder = no
	->
		Var = BaseVar,
		VarSet = VarSet0,
		VarTypes = VarTypes0,
		ExtraGoals = ExtraGoals0
	;
		% Unfortunately, if we have higher order terms, we
		% can no longer just optimise them to be the actual
		% base_type_info
		(
			IsHigherOrder = yes
		->
			list__length(ArgTypeInfoVars, PredArity),
			polymorphism__make_count_var(PredArity, VarSet0,
				VarTypes0, ArityVar, ArityGoal, VarSet1,
				VarTypes1),
			TypeInfoArgVars = [BaseVar, ArityVar | ArgTypeInfoVars],
			TypeInfoArgGoals = [ArityGoal |  ArgTypeInfoGoals]
		;
			TypeInfoArgVars = [BaseVar | ArgTypeInfoVars],
			TypeInfoArgGoals = ArgTypeInfoGoals,
			VarTypes1 = VarTypes0,
			VarSet1 = VarSet0
		),
		polymorphism__init_type_info_var(Type,
			TypeInfoArgVars, "type_info",
			VarSet1, VarTypes1, Var, TypeInfoGoal,
			VarSet, VarTypes),
		list__append(TypeInfoArgGoals, [TypeInfoGoal], ExtraGoals1),
		list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
	).

	% Create a unification `CountVar = <NumTypeArgs>'

:- pred polymorphism__make_count_var(int, varset, map(var, type),
	var, hlds_goal, varset, map(var, type)).
:- mode polymorphism__make_count_var(in, in, in, out, out, out, out) is det.

polymorphism__make_count_var(NumTypeArgs, VarSet0, VarTypes0,
		CountVar, CountGoal, VarSet, VarTypes) :-
	varset__new_var(VarSet0, CountVar, VarSet1),
	varset__name_var(VarSet1, CountVar, "TypeArity", VarSet),
	term__context_init(Context),
	IntType = term__functor(term__atom("int"), [], Context),
	map__set(VarTypes0, CountVar, IntType, VarTypes),
	polymorphism__init_with_int_constant(CountVar, NumTypeArgs, CountGoal).

	% Create a construction unification `Var = <Num>'
	% where Var is a freshly introduced variable and Num is an
	% integer constant.

:- pred polymorphism__init_with_int_constant(var, int, hlds_goal).
:- mode polymorphism__init_with_int_constant(in, in, out) is det.

polymorphism__init_with_int_constant(CountVar, Num, CountUnifyGoal) :-

	CountConsId = int_const(Num),
	CountUnification = construct(CountVar, CountConsId, [], []),

	CountTerm = functor(CountConsId, []),
	CountInst = bound(unique, [functor(int_const(Num), [])]),
	CountUnifyMode = (free -> CountInst) - (CountInst -> CountInst),
	CountUnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	CountUnify = unify(CountVar, CountTerm, CountUnifyMode,
		CountUnification, CountUnifyContext),

	% create a goal_info for the unification

	goal_info_init(CountGoalInfo0),
	set__singleton_set(CountNonLocals, CountVar),
	goal_info_set_nonlocals(CountGoalInfo0, CountNonLocals,	
				CountGoalInfo1),
	instmap_delta_from_assoc_list([CountVar - CountInst], InstmapDelta),
	goal_info_set_instmap_delta(CountGoalInfo1, InstmapDelta,
			CountGoalInfo2),
	goal_info_set_determinism(CountGoalInfo2, det, CountGoalInfo),

	CountUnifyGoal = CountUnify - CountGoalInfo.

	% Create the unifications to initialize the special pred
	% variables for this type:
	%
	%	SpecialPred1 = __Unify__<type>,
	%	SpecialPred2 = __Index__<type>,
	%	SpecialPred3 = __Compare__<type>.

:- pred polymorphism__get_special_proc_list(
			type, module_info, varset, map(var, type),
			list(var), list(hlds_goal), varset, map(var, type)).
:- mode polymorphism__get_special_proc_list(in, in, in, in,
					out, out, out, out) is det.

polymorphism__get_special_proc_list(Type, ModuleInfo, VarSet0, VarTypes0,
		SpecialPredVars, SpecialPredGoals, VarSet, VarTypes) :-
	special_pred_list(SpecialPreds),
	polymorphism__get_special_proc_list_2(SpecialPreds,
		Type, ModuleInfo, VarSet0, VarTypes0,
		SpecialPredVars, SpecialPredGoals, VarSet, VarTypes).

:- pred polymorphism__get_special_proc_list_2(list(special_pred_id),
			type, module_info, varset, map(var, type),
			list(var), list(hlds_goal), varset, map(var, type)).
:- mode polymorphism__get_special_proc_list_2(in, in, in, in, in,
					out, out, out, out) is det.

polymorphism__get_special_proc_list_2([],
		_Type, _ModuleInfo, VarSet, VarTypes,
		[], [], VarSet, VarTypes).
polymorphism__get_special_proc_list_2([Id | Ids],
		Type, ModuleInfo, VarSet0, VarTypes0,
		[Var | Vars], [Goal | Goals], VarSet, VarTypes) :-

	% introduce a fresh variable of the appropriate higher-order pred type

	special_pred_info(Id, Type, PredName, TypeArgs, _Modes, _Det),
	varset__new_var(VarSet0, Var, VarSet1a),
	string__append("Var__", PredName, VarName),
	varset__name_var(VarSet1a, Var, VarName, VarSet1),
	term__context_init(Context),
	PredType = term__functor(term__atom("pred"), TypeArgs, Context),
	map__set(VarTypes0, Var, PredType, VarTypes1),

	% get the ConsId for the address of the appropriate pred
	% for the operation specified by Id applied to Type.

	classify_type(Type, ModuleInfo, TypeCategory),
	polymorphism__get_special_proc(TypeCategory, Id, ModuleInfo,
					PredName2, PredId, ProcId),
	ConsId = code_addr_const(PredId, ProcId),

	% create a construction unification which unifies the fresh
	% variable with the address constant obtained above

	Unification = construct(Var, ConsId, [], []),

	Term = functor(cons(PredName2, 0), []),

	Inst = bound(unique, [functor(cons(PredName2, 0), [])]),
	UnifyMode = (free -> Inst) - (Inst -> Inst),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(Var, Term, UnifyMode, Unification, UnifyContext),

	% create a goal_info for the unification

	goal_info_init(GoalInfo0),
	set__singleton_set(NonLocals, Var),
	goal_info_set_nonlocals(GoalInfo0, NonLocals,	GoalInfo1),
	instmap_delta_from_assoc_list([Var - Inst], InstMapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo2),
	goal_info_set_determinism(GoalInfo2, det, GoalInfo),
	Goal = Unify - GoalInfo,

	polymorphism__get_special_proc_list_2(Ids,
		Type, ModuleInfo, VarSet1, VarTypes1,
		Vars, Goals, VarSet, VarTypes).

:- pred polymorphism__get_special_proc(builtin_type, special_pred_id,
				module_info, sym_name, pred_id, proc_id).
:- mode polymorphism__get_special_proc(in, in, in, out, out, out) is det.

polymorphism__get_special_proc(TypeCategory, SpecialPredId, ModuleInfo,
			PredName, PredId, ProcId) :-
	( TypeCategory = user_type(Type) ->
		module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
		( type_to_type_id(Type, TypeId, _TypeArgs) ->
			map__lookup(SpecialPredMap, SpecialPredId - TypeId,
				PredId)
		;
			error(
		"polymorphism__get_special_proc: type_to_type_id failed")
		),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_module(PredInfo, Module),
		pred_info_name(PredInfo, Name),
		PredName = qualified(Module, Name)
	;
		polymorphism__get_category_name(TypeCategory, CategoryName),
		special_pred_name_arity(SpecialPredId, SpecialName, _, Arity),
		string__append_list(
			["builtin_", SpecialName, "_", CategoryName], Name),
		polymorphism__get_builtin_pred_id(Name, Arity, ModuleInfo,
			PredId),
		PredName = unqualified(Name)
	),
	special_pred_mode_num(SpecialPredId, ProcInt),
	proc_id_to_int(ProcId, ProcInt).

:- pred polymorphism__get_category_name(builtin_type, string).
:- mode polymorphism__get_category_name(in, out) is det.

polymorphism__get_category_name(int_type, "int").
polymorphism__get_category_name(char_type, "int").
polymorphism__get_category_name(enum_type, "int").
polymorphism__get_category_name(float_type, "float").
polymorphism__get_category_name(str_type, "string").
polymorphism__get_category_name(pred_type, "pred").
polymorphism__get_category_name(polymorphic_type, _) :-
	error("polymorphism__get_category_name: polymorphic type").
polymorphism__get_category_name(user_type(_), _) :-
	error("polymorphism__get_category_name: user_type").

	% find the builtin predicate with the specified name

:- pred polymorphism__get_builtin_pred_id(string, int, module_info, pred_id).
:- mode polymorphism__get_builtin_pred_id(in, in, in, out) is det.

polymorphism__get_builtin_pred_id(Name, Arity, ModuleInfo, PredId) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		predicate_table_search_pred_m_n_a(PredicateTable,
			"mercury_builtin", Name, Arity, [PredId1])
	->
		PredId = PredId1
	;
		error("polymorphism__get_pred_id: pred_id lookup failed")
	).

	% Create a unification for a type_info or base_type_info variable:
	%
	%	TypeInfoVar = type_info(CountVar,
	%				SpecialPredVars...,
	%				ArgTypeInfoVars...)
	%
	% or
	%
	%	BaseTypeInfoVar = base_type_type_info(CountVar,
	%				SpecialPredVars...)
	%
	% These unifications WILL lead to the creation of cells on the
	% heap at runtime.

:- pred polymorphism__init_type_info_var(type, list(var), string,
	varset, map(var, type), var, hlds_goal, varset, map(var, type)).
:- mode polymorphism__init_type_info_var(in, in, in, in, in, out, out, out, out)
	is det.

polymorphism__init_type_info_var(Type, ArgVars, Symbol, VarSet0, VarTypes0,
			TypeInfoVar, TypeInfoGoal, VarSet, VarTypes) :-

	ConsId = cons(qualified("mercury_builtin", Symbol), 1),
	TypeInfoTerm = functor(ConsId, ArgVars),

	% introduce a new variable
	polymorphism__new_type_info_var(Type, Symbol, VarSet0, VarTypes0,
		TypeInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	UniMode = (free - ground(shared, no) ->
		   ground(shared, no) - ground(shared, no)),
	list__length(ArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(TypeInfoVar, ConsId, ArgVars, UniModes),
	UnifyMode = (free -> ground(shared, no)) -
			(ground(shared, no) -> ground(shared, no)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(TypeInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	goal_info_init(GoalInfo0),
	set__list_to_set([TypeInfoVar | ArgVars], NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
	list__duplicate(NumArgVars, ground(shared, no), ArgInsts),
		% note that we could perhaps be more accurate than
		% `ground(shared)', but it shouldn't make any
		% difference.
	InstConsId = cons(qualified("mercury_builtin", Symbol), NumArgVars),
	instmap_delta_from_assoc_list(
		[TypeInfoVar - bound(unique, [functor(InstConsId, ArgInsts)])],
		InstMapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo2),
	goal_info_set_determinism(GoalInfo2, det, GoalInfo),

	TypeInfoGoal = Unify - GoalInfo.

	% Create a unification for a type_info or base_type_info variable:
	%
	%	BaseTypeInfoVar = base_type_type_info(CountVar,
	%				SpecialPredVars...)
	%
	% This unification will NOT lead to the creation of a cell on the
	% heap at runtime; it will cause BaseTypeInfoVar to refer to the
	% statically allocated base_type_info cell for the type, allocated
	% in the module that defines the type.

:- pred polymorphism__init_const_base_type_info_var(type, type_id,
	module_info, varset, map(var, type), var, hlds_goal,
	varset, map(var, type)).
:- mode polymorphism__init_const_base_type_info_var(in, in, in, in, in,
	out, out, out, out) is det.

polymorphism__init_const_base_type_info_var(Type, TypeId,
		ModuleInfo, VarSet0, VarTypes0, BaseTypeInfoVar,
		BaseTypeInfoGoal, VarSet, VarTypes) :-

	type_util__type_id_module(ModuleInfo, TypeId, ModuleName),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	TypeId = _ - Arity,
	ConsId = base_type_info_const(ModuleName, TypeName, Arity),
	TypeInfoTerm = functor(ConsId, []),

	% introduce a new variable
	polymorphism__new_type_info_var(Type, "base_type_info",
		VarSet0, VarTypes0, BaseTypeInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	Unification = construct(BaseTypeInfoVar, ConsId, [], []),
	UnifyMode = (free -> ground(shared, no)) -
			(ground(shared, no) -> ground(shared, no)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(BaseTypeInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	goal_info_init(GoalInfo0),
	set__list_to_set([BaseTypeInfoVar], NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
	instmap_delta_from_assoc_list([BaseTypeInfoVar - ground(shared, no)],
		InstmapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstmapDelta, GoalInfo2),
	goal_info_set_determinism(GoalInfo2, det, GoalInfo),

	BaseTypeInfoGoal = Unify - GoalInfo.

:- pred polymorphism__make_head_vars(list(tvar), tvarset,
				varset, map(var, type),
				list(var), varset, map(var, type)).
:- mode polymorphism__make_head_vars(in, in, in, in, out, out, out) is det.

polymorphism__make_head_vars([], _, VarSet, VarTypes, [], VarSet, VarTypes).
polymorphism__make_head_vars([TypeVar|TypeVars], TypeVarSet,
				VarSet0, VarTypes0,
				TypeInfoVars, VarSet, VarTypes) :-
	Type = term__variable(TypeVar),
	polymorphism__new_type_info_var(Type, "type_info", VarSet0, VarTypes0,
					Var, VarSet1, VarTypes1),
	( varset__search_name(TypeVarSet, TypeVar, TypeVarName) ->
		string__append("TypeInfo_for_", TypeVarName, VarName),
		varset__name_var(VarSet1, Var, VarName, VarSet2)
	;
		VarSet2 = VarSet1
	),
	TypeInfoVars = [Var | TypeInfoVars1],
	polymorphism__make_head_vars(TypeVars, TypeVarSet,
				VarSet2, VarTypes1,
				TypeInfoVars1, VarSet, VarTypes).

:- pred polymorphism__new_type_info_var(type, string, varset, map(var, type),
					var, varset, map(var, type)).
:- mode polymorphism__new_type_info_var(in, in, in, in, out, out, out) is det.

polymorphism__new_type_info_var(Type, Symbol, VarSet0, VarTypes0,
				Var, VarSet, VarTypes) :-
	% introduce new variable
	varset__new_var(VarSet0, Var, VarSet1),
	term__var_to_int(Var, VarNum),
	string__int_to_string(VarNum, VarNumStr),
	string__append("TypeInfo_", VarNumStr, Name),
	varset__name_var(VarSet1, Var, Name, VarSet),
	construct_type(qualified("mercury_builtin", Symbol) - 1,
					[Type], UnifyPredType),
	map__set(VarTypes0, Var, UnifyPredType, VarTypes).

:- pred polymorphism__get_module_info(module_info, poly_info, poly_info).
:- mode polymorphism__get_module_info(out, in, out) is det.

polymorphism__get_module_info(ModuleInfo, PolyInfo, PolyInfo) :-
	PolyInfo = poly_info(_, _, _, _, ModuleInfo).

:- pred polymorphism__set_module_info(module_info, poly_info, poly_info).
:- mode polymorphism__set_module_info(in, in, out) is det.

polymorphism__set_module_info(ModuleInfo, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, _),
	PolyInfo = poly_info(A, B, C, D, ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
