%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
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
% When we run the polymorphism.m pass before mode checking, this will
% be checked by mode analysis.  XXX But currently it is not checked.
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

:- module (lambda).

:- interface. 

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, prog_data.
:- import_module list, map, set, term, varset.

:- pred lambda__process_pred(pred_id, module_info, module_info).
:- mode lambda__process_pred(in, in, out) is det.

:- pred lambda__transform_lambda(pred_or_func, string, list(var), list(mode), 
		determinism, list(var), set(var), hlds_goal, unification,
		varset, map(var, type), class_constraints, tvarset,
		map(tvar, type_info_locn), map(class_constraint, var),
		module_info, unify_rhs, unification, module_info).
:- mode lambda__transform_lambda(in, in, in, in, in, in, in, in, in, in, in,
		in, in, in, in, in, out, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module make_hlds, globals, options.
:- import_module goal_util, prog_util, mode_util, inst_match, llds, arg_info.

:- import_module bool, string, std_util, require.

:- type lambda_info --->
		lambda_info(
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			class_constraints,	% from the pred_info
			tvarset,		% from the proc_info
			map(tvar, type_info_locn),	
						% from the proc_info 
						% (typeinfos)
			map(class_constraint, var),
						% from the proc_info
						% (typeclass_infos)
			pred_or_func,
			string,			% pred/func name
			module_info
		).

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

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
	pred_info_get_class_context(PredInfo0, Constraints0),
	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_typeinfo_varmap(ProcInfo0, TVarMap0),
	proc_info_typeclass_info_varmap(ProcInfo0, TCVarMap0),

	% process the goal
	Info0 = lambda_info(VarSet0, VarTypes0, Constraints0, TypeVarSet0,
		TVarMap0, TCVarMap0, PredOrFunc, PredName, ModuleInfo0),
	lambda__process_goal(Goal0, Goal, Info0, Info),
	Info = lambda_info(VarSet, VarTypes, Constraints, TypeVarSet, 
		TVarMap, TCVarMap, _, _, ModuleInfo),

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
	( { Y = lambda_goal(PredOrFunc, NonLocalVars, Vars, 
			Modes, Det, LambdaGoal0) } ->
		% for lambda expressions, we must convert the lambda expression
		% into a new predicate
		lambda__process_lambda(PredOrFunc, Vars, Modes, Det, 
			NonLocalVars, LambdaGoal0, 
			Unification, Y1, Unification1),
		{ Unify = unify(XVar, Y1, Mode, Unification1, Context) }
	;
		% ordinary unifications are left unchanged
		{ Unify = unify(XVar, Y, Mode, Unification, Context) }
	).

	% the rest of the clauses just process goals recursively

lambda__process_goal_2(conj(Goals0), GoalInfo, conj(Goals) - GoalInfo) -->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(par_conj(Goals0, SM), GoalInfo,
		par_conj(Goals, SM) - GoalInfo) -->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(disj(Goals0, SM), GoalInfo, disj(Goals, SM) - GoalInfo)
		-->
	lambda__process_goal_list(Goals0, Goals).
lambda__process_goal_2(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	lambda__process_goal(Goal0, Goal).
lambda__process_goal_2(switch(Var, CanFail, Cases0, SM), GoalInfo, 
			switch(Var, CanFail, Cases, SM) - GoalInfo) -->
	lambda__process_cases(Cases0, Cases).
lambda__process_goal_2(some(Vars, Goal0), GoalInfo,
			some(Vars, Goal) - GoalInfo) -->
	lambda__process_goal(Goal0, Goal).
lambda__process_goal_2(if_then_else(Vars, A0, B0, C0, SM), GoalInfo,
			if_then_else(Vars, A, B, C, SM) - GoalInfo) -->
	lambda__process_goal(A0, A),
	lambda__process_goal(B0, B),
	lambda__process_goal(C0, C).
lambda__process_goal_2(higher_order_call(A,B,C,D,E,F), GoalInfo,
			higher_order_call(A,B,C,D,E,F) - GoalInfo) -->
	[].
lambda__process_goal_2(class_method_call(A,B,C,D,E,F), GoalInfo,
			class_method_call(A,B,C,D,E,F) - GoalInfo) -->
	[].
lambda__process_goal_2(call(A,B,C,D,E,F), GoalInfo,
			call(A,B,C,D,E,F) - GoalInfo) -->
	[].
lambda__process_goal_2(pragma_c_code(A,B,C,D,E,F,G), GoalInfo,
			pragma_c_code(A,B,C,D,E,F,G) - GoalInfo) -->
	[].

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

:- pred lambda__process_lambda(pred_or_func, list(var), list(mode), determinism,
		list(var), hlds_goal, unification, unify_rhs, unification,
		lambda_info, lambda_info).
:- mode lambda__process_lambda(in, in, in, in, in, in, in, out, out,
		in, out) is det.

lambda__process_lambda(PredOrFunc, Vars, Modes, Det, OrigNonLocals0, LambdaGoal,
		Unification0, Functor, Unification, LambdaInfo0, LambdaInfo) :-
	LambdaInfo0 = lambda_info(VarSet, VarTypes, Constraints, TVarSet,
			TVarMap, TCVarMap, POF, PredName, ModuleInfo0),
	% XXX existentially typed lambda expressions are not yet supported
	% (see the documentation at top of this file)
	ExistQVars = [],
	goal_util__extra_nonlocal_typeinfos(TVarMap, TCVarMap, VarTypes,
		ExistQVars, LambdaGoal, ExtraTypeInfos),
	lambda__transform_lambda(PredOrFunc, PredName, Vars, Modes, Det,
		OrigNonLocals0, ExtraTypeInfos, LambdaGoal, Unification0,
		VarSet, VarTypes, Constraints, TVarSet, TVarMap, TCVarMap,
		ModuleInfo0, Functor, Unification, ModuleInfo),
	LambdaInfo = lambda_info(VarSet, VarTypes, Constraints, TVarSet,
			TVarMap, TCVarMap, POF, PredName, ModuleInfo).

lambda__transform_lambda(PredOrFunc, OrigPredName, Vars, Modes, Detism,
		OrigVars, ExtraTypeInfos, LambdaGoal, Unification0,
		VarSet, VarTypes, Constraints, TVarSet, TVarMap, TCVarMap,
		ModuleInfo0, Functor, Unification, ModuleInfo) :-
	(
		Unification0 = construct(Var0, _, _, UniModes0)
	->
		Var = Var0,
		UniModes1 = UniModes0
	;
		error("polymorphism__transform_lambda: weird unification")
	),

	% Optimize a special case: replace
	%	`lambda([Y1, Y2, ...] is Detism, p(X1, X2, ..., Y1, Y2, ...))'
	% where `p' has determinism `Detism' with
	%	`p(X1, X2, ...)'
	%
	% This optimization is only valid if the modes of the Xi are
	% input, since only input arguments can be curried.
	% It's also only valid if all the inputs in the Yi precede the
	% outputs.  It's also not valid if any of the Xi are in the Yi.

	LambdaGoal = _ - LambdaGoalInfo,
	goal_info_get_nonlocals(LambdaGoalInfo, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals1),
	module_info_globals(ModuleInfo0, Globals),

	% If typeinfo_liveness is set, all type_infos for the
	% arguments should be included, not just the ones
	% that are used.
	globals__lookup_bool_option(Globals,
		typeinfo_liveness, TypeInfoLiveness),
	( TypeInfoLiveness = yes ->
		set__union(NonLocals1, ExtraTypeInfos, NonLocals)
	;
		NonLocals = NonLocals1
	),

	set__to_sorted_list(NonLocals, ArgVars1),
	( 
		LambdaGoal = call(PredId0, ProcId0, CallVars,
					_, _, PredName0) - _,
		module_info_pred_proc_info(ModuleInfo0, PredId0, ProcId0, _,
			Call_ProcInfo),

			% check that this procedure uses an args_method which 
			% is always directly higher-order callable.
		proc_info_args_method(Call_ProcInfo, Call_ArgsMethod),
		module_info_globals(ModuleInfo0, Globals),
		arg_info__args_method_is_ho_callable(Globals,
			Call_ArgsMethod, yes),

		list__remove_suffix(CallVars, Vars, InitialVars),
	
		% check that none of the variables that we're trying to
		% use as curried arguments are lambda-bound variables
		\+ (
			list__member(InitialVar, InitialVars),
			list__member(InitialVar, Vars)
		),

		proc_info_interface_code_model(Call_ProcInfo, Call_CodeModel),
		determinism_to_code_model(Detism, CodeModel),
			% Check that the code models are compatible.
			% Note that det is not compatible with semidet,
			% and semidet is not compatible with nondet,
			% since the arguments go in different registers.
			% But det is compatible with nondet.
		( CodeModel = Call_CodeModel
		; CodeModel = model_non, Call_CodeModel = model_det
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
		PredName = PredName0,
		ModuleInfo = ModuleInfo0,
		NumArgVars = NumInitialVars,
		mode_util__modes_to_uni_modes(CurriedArgModes, CurriedArgModes,
			ModuleInfo0, UniModes)
	;
		% Prepare to create a new predicate for the lambda
		% expression: work out the arguments, module name, predicate
		% name, arity, arg types, determinism,
		% context, status, etc. for the new predicate.

		ArgVars = ArgVars1,
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
		% XXX existentially typed lambda expressions are not
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

		% Choose an args_method which is always directly callable
		% from do_call_*_closure even if the inputs don't preceed
		% the outputs in the declaration. mercury_ho_call.c requires
		% that procedures which are directly higher-order-called use
		% the compact args_method.
		%
		% Previously we permuted the argument variables so that
		% inputs came before outputs, but that resulted in the
		% HLDS not being type or mode correct which caused problems
		% for some transformations and for rerunning mode analysis.
		arg_info__ho_call_args_method(Globals, ArgsMethod),

		% Now construct the proc_info and pred_info for the new
		% single-mode predicate, using the information computed above

		proc_info_create(VarSet, VarTypes, AllArgVars,
			AllArgModes, Detism, LambdaGoal, LambdaContext,
			TVarMap, TCVarMap, ArgsMethod, ProcInfo),

		init_markers(Markers),
		pred_info_create(ModuleName, PredName, TVarSet, ExistQVars,
			ArgTypes, true, LambdaContext, local, Markers,
			PredOrFunc, Constraints, ProcInfo, ProcId, PredInfo),

		% save the new predicate in the predicate table

		module_info_get_predicate_table(ModuleInfo1, PredicateTable0),
		predicate_table_insert(PredicateTable0, PredInfo,
			PredId, PredicateTable),
		module_info_set_predicate_table(ModuleInfo1, PredicateTable,
			ModuleInfo)
	),
	Functor = functor(cons(PredName, NumArgVars), ArgVars),
	ConsId = pred_const(PredId, ProcId),
	Unification = construct(Var, ConsId, ArgVars, UniModes).

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
