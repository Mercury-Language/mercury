%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: lambda.m
% main author: fjh

% This module is a pass over the HLDS to deal with lambda expressions.
%
% Lambda expressions are converted into separate predicates, so for
% example we translate
%
%	:- pred p(int::in) is det.
%	p(X) :-
%		V__1 = lambda([Y::out] is nondet, q(Y, X))),
%		solutions(V__1, List),
%		...
%	:- pred q(int::out, int::in) is nondet.
%
% into
%
%	p(X) :-
%		V__1 = '__LambdaGoal__1'(X)
%		solutions(V__1, List),
%		...
%
%	:- pred '__LambdaGoal__1'(int::in, int::out) is nondet.
%	'__LambdaGoal__1'(X, Y) :- q(Y, X).
%
%
% Note that the mode checker requires that a lambda expression
% not bind any of the non-local variables such as `X' in the above
% example.
%
% Similarly, a lambda expression may not bind any of the type_infos for
% those variables; that is, none of the non-local variables
% should be existentially typed (from the perspective of the lambda goal).
% Now that we run the polymorphism.m pass before mode checking, this is
% also checked by mode analysis.
%
% It might be OK to allow the parameters of the lambda goal to be
% existentially typed, but currently that is not supported.
% One difficulty is that it's hard to determine here which type variables
% should be existentially quantified.  The information is readily
% available during type inference, and really type inference should save
% that information in a field in the lambda_goal struct, but currently it
% doesn't; it saves the head_type_params field in the pred_info, which
% tells us which type variables where produced by the body, but for
% any given lambda goal we don't know whether the type variable was
% produced by something outside the lambda goal or by something inside
% the lambda goal (only in the latter case should it be existentially
% quantified).
% The other difficulty is that taking the address of a predicate with an
% existential type would require second-order polymorphism:  for a predicate
% declared as `:- some [T] pred p(int, T)', the expression `p' must have
% type `some [T] pred(int, T)', which is quite a different thing to saying
% that there is some type `T' for which `p' has type `pred(int, T)' --
% we don't know what `T' is until the predicate is called, and it might
% be different for each call.
% Currently we don't support second-order polymorphism, so we
% don't support existentially typed lambda expressions either.
% 

%-----------------------------------------------------------------------------%

:- module (transform_hlds__lambda).

:- interface. 

:- import_module hlds__hlds_module, hlds__hlds_pred.

:- pred lambda__process_module(module_info, module_info).
:- mode lambda__process_module(in, out) is det.

:- pred lambda__process_pred(pred_id, module_info, module_info).
:- mode lambda__process_pred(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__code_model. % XXX for some back-end dependent optimizations

% Parse tree modules
:- import_module parse_tree__prog_data, parse_tree__prog_util.
% HLDS modules
:- import_module hlds__hlds_goal, hlds__hlds_data, hlds__quantification.
:- import_module check_hlds__type_util, hlds__goal_util.
:- import_module check_hlds__mode_util, check_hlds__inst_match.
% Misc
:- import_module libs__globals, libs__options.

% Standard library modules
:- import_module list, map, set.
:- import_module term, varset, bool, string, std_util, require.

:- type lambda_info --->
		lambda_info(
			prog_varset,		% from the proc_info
			map(prog_var, type),	% from the proc_info
			class_constraints,	% from the pred_info
			tvarset,		% from the proc_info
			inst_varset,		% from the proc_info
			map(tvar, type_info_locn),	
						% from the proc_info 
						% (typeinfos)
			map(class_constraint, prog_var),
						% from the proc_info
						% (typeclass_infos)
			pred_markers,		% from the pred_info
			pred_or_func,
			string,			% pred/func name
			aditi_owner,
			module_info,
			bool	% true iff we need to recompute the nonlocals
		).

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

lambda__process_module(ModuleInfo0, ModuleInfo) :-
	module_info_predids(ModuleInfo0, PredIds),
	lambda__process_preds(PredIds, ModuleInfo0, ModuleInfo1),
	% Need update the dependency graph to include the lambda predicates. 
	module_info_clobber_dependency_info(ModuleInfo1, ModuleInfo).

:- pred lambda__process_preds(list(pred_id), module_info, module_info).
:- mode lambda__process_preds(in, in, out) is det.

lambda__process_preds([], ModuleInfo, ModuleInfo).
lambda__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	lambda__process_pred(PredId, ModuleInfo0, ModuleInfo1),
	lambda__process_preds(PredIds, ModuleInfo1, ModuleInfo).

lambda__process_pred(PredId, ModuleInfo0, ModuleInfo) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	lambda__process_procs(PredId, ProcIds, ModuleInfo0, ModuleInfo).

:- pred lambda__process_procs(pred_id, list(proc_id), module_info, module_info).
:- mode lambda__process_procs(in, in, in, out) is det.

lambda__process_procs(_PredId, [], ModuleInfo, ModuleInfo).
lambda__process_procs(PredId, [ProcId | ProcIds], ModuleInfo0, ModuleInfo) :-
	lambda__process_proc(PredId, ProcId, ModuleInfo0, ModuleInfo1),
	lambda__process_procs(PredId, ProcIds, ModuleInfo1, ModuleInfo).

:- pred lambda__process_proc(pred_id, proc_id, module_info, module_info).
:- mode lambda__process_proc(in, in, in, out) is det.

lambda__process_proc(PredId, ProcId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	lambda__process_proc_2(ProcInfo0, PredInfo0, ModuleInfo0,
					ProcInfo, PredInfo1, ModuleInfo1),

	pred_info_procedures(PredInfo1, ProcTable1),
	map__det_update(ProcTable1, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	module_info_preds(ModuleInfo1, PredTable1),
	map__det_update(PredTable1, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo).

:- pred lambda__process_proc_2(proc_info, pred_info, module_info,
				proc_info, pred_info, module_info).
:- mode lambda__process_proc_2(in, in, in, out, out, out) is det.

lambda__process_proc_2(ProcInfo0, PredInfo0, ModuleInfo0,
				ProcInfo, PredInfo, ModuleInfo) :-
	% grab the appropriate fields from the pred_info and proc_info
	pred_info_name(PredInfo0, PredName),
	pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
	pred_info_typevarset(PredInfo0, TypeVarSet0),
	pred_info_get_markers(PredInfo0, Markers),
	pred_info_get_class_context(PredInfo0, Constraints0),
	pred_info_get_aditi_owner(PredInfo0, Owner),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_typeinfo_varmap(ProcInfo0, TVarMap0),
	proc_info_typeclass_info_varmap(ProcInfo0, TCVarMap0),
	proc_info_inst_varset(ProcInfo0, InstVarSet0),
	MustRecomputeNonLocals0 = no,

	% process the goal
	Info0 = lambda_info(VarSet0, VarTypes0, Constraints0, TypeVarSet0,
		InstVarSet0, TVarMap0, TCVarMap0, Markers, PredOrFunc, 
		PredName, Owner, ModuleInfo0, MustRecomputeNonLocals0),
	lambda__process_goal(Goal0, Goal1, Info0, Info1),
	Info1 = lambda_info(VarSet1, VarTypes1, Constraints, TypeVarSet, 
		_, TVarMap, TCVarMap, _, _, _, _, ModuleInfo,
		MustRecomputeNonLocals),

	% check if we need to requantify
	( MustRecomputeNonLocals = yes ->
		implicitly_quantify_clause_body(HeadVars,
			Goal1, VarSet1, VarTypes1,
			Goal, VarSet, VarTypes, _Warnings)
	;
		Goal = Goal1,
		VarSet = VarSet1,
		VarTypes = VarTypes1
	),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_varset(ProcInfo1, VarSet, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo3),
	proc_info_set_typeinfo_varmap(ProcInfo3, TVarMap, ProcInfo4),
	proc_info_set_typeclass_info_varmap(ProcInfo4, TCVarMap, ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo1),
	pred_info_set_class_context(PredInfo1, Constraints, PredInfo).

:- pred lambda__process_goal(hlds_goal, hlds_goal,
					lambda_info, lambda_info).
:- mode lambda__process_goal(in, out, in, out) is det.

lambda__process_goal(Goal0 - GoalInfo0, Goal) -->
	lambda__process_goal_2(Goal0, GoalInfo0, Goal).

:- pred lambda__process_goal_2(hlds_goal_expr, hlds_goal_info,
					hlds_goal, lambda_info, lambda_info).
:- mode lambda__process_goal_2(in, in, out, in, out) is det.

lambda__process_goal_2(unify(XVar, Y, Mode, Unification, Context), GoalInfo,
			Unify - GoalInfo) -->
	( { Y = lambda_goal(PredOrFunc, EvalMethod, _, NonLocalVars, Vars, 
			Modes, Det, LambdaGoal0) } ->
		% first, process the lambda goal recursively, in case it
		% contains some nested lambda expressions.
		lambda__process_goal(LambdaGoal0, LambdaGoal1),

		% then, convert the lambda expression into a new predicate
		lambda__process_lambda(PredOrFunc, EvalMethod, Vars,
			Modes, Det, NonLocalVars, LambdaGoal1, 
			Unification, Y1, Unification1),
		{ Unify = unify(XVar, Y1, Mode, Unification1, Context) }
	;
		% ordinary unifications are left unchanged
		{ Unify = unify(XVar, Y, Mode, Unification, Context) }
	).

	% the rest of the clauses just process goals recursively

lambda__process_goal_2(conj(Goals0), GoalInfo, conj(Goals) - GoalInfo) -->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(par_conj(Goals0), GoalInfo,
		par_conj(Goals) - GoalInfo) -->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(disj(Goals0), GoalInfo, disj(Goals) - GoalInfo)
		-->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	lambda__process_goal(Goal0, Goal).
lambda__process_goal_2(switch(Var, CanFail, Cases0), GoalInfo, 
			switch(Var, CanFail, Cases) - GoalInfo) -->
	lambda__process_cases(Cases0, Cases).
lambda__process_goal_2(some(Vars, CanRemove, Goal0), GoalInfo,
			some(Vars, CanRemove, Goal) - GoalInfo) -->
	lambda__process_goal(Goal0, Goal).
lambda__process_goal_2(if_then_else(Vars, A0, B0, C0), GoalInfo,
			if_then_else(Vars, A, B, C) - GoalInfo) -->
	lambda__process_goal(A0, A),
	lambda__process_goal(B0, B),
	lambda__process_goal(C0, C).
lambda__process_goal_2(generic_call(A,B,C,D), GoalInfo,
			generic_call(A,B,C,D) - GoalInfo) -->
	[].
lambda__process_goal_2(call(A,B,C,D,E,F), GoalInfo,
			call(A,B,C,D,E,F) - GoalInfo) -->
	[].
lambda__process_goal_2(foreign_proc(A,B,C,D,E,F,G), GoalInfo,
			foreign_proc(A,B,C,D,E,F,G) - GoalInfo) -->
	[].
lambda__process_goal_2(shorthand(_), _, _) -->
	% these should have been expanded out by now
	{ error("lambda__process_goal_2: unexpected shorthand") }.

:- pred lambda__process_goal_list(list(hlds_goal), list(hlds_goal),
					lambda_info, lambda_info).
:- mode lambda__process_goal_list(in, out, in, out) is det.

lambda__process_goal_list([], []) --> [].
lambda__process_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	lambda__process_goal(Goal0, Goal),
	lambda__process_goal_list(Goals0, Goals).

:- pred lambda__process_cases(list(case), list(case),
					lambda_info, lambda_info).
:- mode lambda__process_cases(in, out, in, out) is det.

lambda__process_cases([], []) --> [].
lambda__process_cases([case(ConsId, Goal0) | Cases0],
		[case(ConsId, Goal) | Cases]) -->
	lambda__process_goal(Goal0, Goal),
	lambda__process_cases(Cases0, Cases).

:- pred lambda__process_lambda(pred_or_func, lambda_eval_method,
		list(prog_var), list(mode), determinism, list(prog_var),
		hlds_goal, unification, unify_rhs, unification,
		lambda_info, lambda_info).
:- mode lambda__process_lambda(in, in, in, in, in, in, in, in, out, out,
		in, out) is det.

lambda__process_lambda(PredOrFunc, EvalMethod, Vars, Modes, Detism,
		OrigNonLocals0, LambdaGoal, Unification0, Functor,
		Unification, LambdaInfo0, LambdaInfo) :-
	LambdaInfo0 = lambda_info(VarSet, VarTypes, _PredConstraints, TVarSet,
		InstVarSet, TVarMap, TCVarMap, Markers, POF, OrigPredName,
		Owner, ModuleInfo0, MustRecomputeNonLocals0),

		% Calculate the constraints which apply to this lambda
		% expression. 
		% Note currently we only allow lambda expressions
		% to have universally quantified constraints.
	map__keys(TCVarMap, AllConstraints),
	map__apply_to_list(Vars, VarTypes, LambdaVarTypes),
	list__map(type_util__vars, LambdaVarTypes, LambdaTypeVarsList),
	list__condense(LambdaTypeVarsList, LambdaTypeVars),
	list__filter(lambda__constraint_contains_vars(LambdaTypeVars), 
		AllConstraints, UnivConstraints),
	Constraints = constraints(UnivConstraints, []),

	% existentially typed lambda expressions are not yet supported
	% (see the documentation at top of this file)
	ExistQVars = [],
	LambdaGoal = _ - LambdaGoalInfo,
	goal_info_get_nonlocals(LambdaGoalInfo, LambdaGoalNonLocals),
	set__insert_list(LambdaGoalNonLocals, Vars, LambdaNonLocals),
	goal_util__extra_nonlocal_typeinfos(TVarMap, TCVarMap, VarTypes,
		ExistQVars, LambdaNonLocals, ExtraTypeInfos),
	OrigVars = OrigNonLocals0,

	(
		Unification0 = construct(Var0, _, _, UniModes0, _, _, _)
	->
		Var = Var0,
		UniModes1 = UniModes0
	;
		error("lambda__transform_lambda: weird unification")
	),

	set__delete_list(LambdaGoalNonLocals, Vars, NonLocals1),

	% We need all the typeinfos, including the ones that are not used,
	% for the layout structure describing the closure.
	NewTypeInfos = ExtraTypeInfos `set__difference` NonLocals1,
	NonLocals = NonLocals1 `set__union` NewTypeInfos,

	% If we added variables to the nonlocals of the lambda goal,
	% then we need to recompute the nonlocals for the procedure
	% that contains it.
	( \+ set__empty(NewTypeInfos) ->
		MustRecomputeNonLocals = yes
	;
		MustRecomputeNonLocals = MustRecomputeNonLocals0
	),

	set__to_sorted_list(NonLocals, ArgVars1),

	( 
		% Optimize a special case: replace
		%	`lambda([Y1, Y2, ...] is Detism,
		%		p(X1, X2, ..., Y1, Y2, ...))'
		% where `p' has determinism `Detism' with
		%	`p(X1, X2, ...)'
		%
		% This optimization is only valid if the modes of the Xi are
		% input, since only input arguments can be curried.
		% It's also only valid if all the inputs in the Yi precede the
		% outputs.  It's also not valid if any of the Xi are in the Yi.

		LambdaGoal = call(PredId0, ProcId0, CallVars, _, _, _) - _,
		module_info_pred_proc_info(ModuleInfo0, PredId0, ProcId0,
			Call_PredInfo, Call_ProcInfo),

		(
			EvalMethod = (aditi_top_down),
			pred_info_get_markers(Call_PredInfo, Call_Markers),
			check_marker(Call_Markers, (aditi_top_down))
		;
			EvalMethod = (aditi_bottom_up),
			pred_info_get_markers(Call_PredInfo, Call_Markers),
			check_marker(Call_Markers, aditi)
		;
			EvalMethod = normal
		),
		list__remove_suffix(CallVars, Vars, InitialVars),
	
		% check that none of the variables that we're trying to
		% use as curried arguments are lambda-bound variables
		\+ (
			list__member(InitialVar, InitialVars),
			list__member(InitialVar, Vars)
		),

			% Check that the code models are compatible.
			% Note that det is not compatible with semidet,
			% and semidet is not compatible with nondet,
			% since the calling conventions are different.
			% If we're using the LLDS back-end
			% (i.e. not --high-level-code),
			% det is compatible with nondet.
			% If we're using the MLDS back-end,
			% then predicates and functions have different
			% calling conventions.
		proc_info_interface_code_model(Call_ProcInfo, Call_CodeModel),
		determinism_to_code_model(Detism, CodeModel),
		module_info_globals(ModuleInfo0, Globals),
		globals__lookup_bool_option(Globals, highlevel_code,
			HighLevelCode),
		(
			HighLevelCode = no,
			( CodeModel = Call_CodeModel
			; CodeModel = model_non, Call_CodeModel = model_det
			)
		;
			HighLevelCode = yes,
			pred_info_get_is_pred_or_func(Call_PredInfo,
				Call_PredOrFunc),
			PredOrFunc = Call_PredOrFunc,
			CodeModel = Call_CodeModel
		),
			
			% check that the curried arguments are all input
		proc_info_argmodes(Call_ProcInfo, Call_ArgModes),
		list__length(InitialVars, NumInitialVars),
		list__take(NumInitialVars, Call_ArgModes, CurriedArgModes),
		\+ (	list__member(Mode, CurriedArgModes), 
			\+ mode_is_input(ModuleInfo0, Mode)
		)
	->
		ArgVars = InitialVars,
		PredId = PredId0,
		ProcId = ProcId0,
		mode_util__modes_to_uni_modes(CurriedArgModes, CurriedArgModes,
			ModuleInfo0, UniModes),
		%
		% we need to mark the procedure as having had its
		% address taken
		%
		proc_info_set_address_taken(Call_ProcInfo, address_is_taken,
			Call_NewProcInfo),
		module_info_set_pred_proc_info(ModuleInfo0, PredId, ProcId,
			Call_PredInfo, Call_NewProcInfo, ModuleInfo)
	;
		% Prepare to create a new predicate for the lambda
		% expression: work out the arguments, module name, predicate
		% name, arity, arg types, determinism,
		% context, status, etc. for the new predicate.

		ArgVars = put_typeinfo_vars_first(ArgVars1, VarTypes),
		list__append(ArgVars, Vars, AllArgVars),

		module_info_name(ModuleInfo0, ModuleName),
		module_info_next_lambda_count(ModuleInfo0, LambdaCount,
					ModuleInfo1),
		goal_info_get_context(LambdaGoalInfo, OrigContext),
		term__context_line(OrigContext, OrigLine),
		make_pred_name_with_context(ModuleName, "IntroducedFrom",
			PredOrFunc, OrigPredName, OrigLine,
			LambdaCount, PredName),
		goal_info_get_context(LambdaGoalInfo, LambdaContext),
		% The TVarSet is a superset of what it really ought be,
		% but that shouldn't matter.
		% Existentially typed lambda expressions are not
		% yet supported (see the documentation at top of this file)
		ExistQVars = [],
		lambda__uni_modes_to_modes(UniModes1, OrigArgModes),

		% We have to jump through hoops to work out the mode
		% of the lambda predicate. For introduced
		% type_info arguments, we use the mode "in".  For the original
		% non-local vars, we use the modes from `UniModes1'.
		% For the lambda var arguments at the end,
		% we use the mode in the lambda expression. 

		list__length(ArgVars, NumArgVars),
		in_mode(In),
		list__duplicate(NumArgVars, In, InModes),
		map__from_corresponding_lists(ArgVars, InModes,
			ArgModesMap),

		map__from_corresponding_lists(OrigVars, OrigArgModes,
			OrigArgModesMap),
		map__overlay(ArgModesMap, OrigArgModesMap, ArgModesMap1),
		map__apply_to_list(ArgVars, ArgModesMap1, ArgModes1),

		% Recompute the uni_modes. 
		mode_util__modes_to_uni_modes(ArgModes1, ArgModes1, 
			ModuleInfo1, UniModes),

		list__append(ArgModes1, Modes, AllArgModes),
		map__apply_to_list(AllArgVars, VarTypes, ArgTypes),

		( 
			% Pass through the aditi markers for 
			% aggregate query closures.
			% XXX we should differentiate between normal
			% top-down closures and aggregate query closures,
			% possibly by using a different type for aggregate
			% queries. Currently all nondet lambda expressions
			% within Aditi predicates are treated as aggregate
			% inputs.
			% EvalMethod = (aditi_bottom_up),
			determinism_components(Detism, _, at_most_many),
			check_marker(Markers, aditi)
		->
			markers_to_marker_list(Markers, MarkerList0),
			list__filter(
			    lambda([Marker::in] is semidet, 
				% Pass through only Aditi markers.
				% Don't pass through `context' markers, since
				% they are useless for non-recursive predicates
				% such as the created predicate.
				( Marker = aditi
				; Marker = dnf
				; Marker = psn
				; Marker = naive
				; Marker = supp_magic
				; Marker = aditi_memo
				; Marker = aditi_no_memo
				)),
				MarkerList0, MarkerList),
			marker_list_to_markers(MarkerList, LambdaMarkers)
		;
			EvalMethod = (aditi_bottom_up)
		->
			marker_list_to_markers([aditi], LambdaMarkers)
		;
			EvalMethod = (aditi_top_down)
		->
			marker_list_to_markers([(aditi_top_down)],
				LambdaMarkers)
		; 
			init_markers(LambdaMarkers)
		),

		% Now construct the proc_info and pred_info for the new
		% single-mode predicate, using the information computed above

		proc_info_create(VarSet, VarTypes, AllArgVars, AllArgModes,
			InstVarSet, Detism, LambdaGoal, LambdaContext,
			TVarMap, TCVarMap, address_is_taken, ProcInfo0),
		% If we previously already needed to recompute the nonlocals,
		% then we'd better to that recomputation for the procedure
		% that we just created.
		( MustRecomputeNonLocals0 = yes ->
			requantify_proc(ProcInfo0, ProcInfo)
		;
			ProcInfo = ProcInfo0
		),

		set__init(Assertions),

		pred_info_create(ModuleName, PredName, TVarSet, ExistQVars,
			ArgTypes, true, LambdaContext, local, LambdaMarkers,
			PredOrFunc, Constraints, Owner, Assertions, ProcInfo,
			ProcId, PredInfo),

		% save the new predicate in the predicate table

		module_info_get_predicate_table(ModuleInfo1, PredicateTable0),
		predicate_table_insert(PredicateTable0, PredInfo,
			PredId, PredicateTable),
		module_info_set_predicate_table(ModuleInfo1, PredicateTable,
			ModuleInfo)
	),
	ConsId = pred_const(PredId, ProcId, EvalMethod),
	Functor = functor(ConsId, no, ArgVars),

	RLExprnId = no,
	Unification = construct(Var, ConsId, ArgVars, UniModes,
		construct_dynamically, cell_is_unique, RLExprnId),
	LambdaInfo = lambda_info(VarSet, VarTypes, Constraints, TVarSet,
		InstVarSet, TVarMap, TCVarMap, Markers, POF, OrigPredName,
		Owner, ModuleInfo, MustRecomputeNonLocals).

:- pred lambda__constraint_contains_vars(list(tvar), class_constraint).
:- mode lambda__constraint_contains_vars(in, in) is semidet.

lambda__constraint_contains_vars(LambdaVars, ClassConstraint) :-
	ClassConstraint = constraint(_, ConstraintTypes),
	list__map(type_util__vars, ConstraintTypes, ConstraintVarsList),
	list__condense(ConstraintVarsList, ConstraintVars),
		% Probably not the most efficient way of doing it, but I
		% wouldn't think that it matters.
	set__list_to_set(LambdaVars, LambdaVarsSet),
	set__list_to_set(ConstraintVars, ConstraintVarsSet),
	set__subset(ConstraintVarsSet, LambdaVarsSet).

:- pred lambda__uni_modes_to_modes(list(uni_mode), list(mode)).
:- mode lambda__uni_modes_to_modes(in, out) is det.

	% This predicate works out the modes of the original non-local
	% variables of a lambda expression based on the list of uni_mode
	% in the unify_info for the lambda unification.

lambda__uni_modes_to_modes([], []).
lambda__uni_modes_to_modes([UniMode | UniModes], [Mode | Modes]) :-
	UniMode = ((_Initial0 - Initial1) -> (_Final0 - _Final1)),
	Mode = (Initial1 -> Initial1),
	lambda__uni_modes_to_modes(UniModes, Modes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
