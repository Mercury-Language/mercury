%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modecheck_unify.m.
% Main author: fjh.
%
% This module contains the code to modecheck a unification.
%
% Check that the unification doesn't attempt to unify two free variables
% (or in general two free sub-terms) unless one of them is dead. (Also we
% ought to split unifications up if necessary to avoid complicated
% sub-unifications.)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modecheck_unify.
:- interface.

:- import_module hlds_goal, mode_info, modes.
:- import_module term.

	% Modecheck a unification
:- pred modecheck_unification( var, unify_rhs, unification, unify_context,
			hlds_goal_info, how_to_check_goal, hlds_goal_expr,
			mode_info, mode_info).
:- mode modecheck_unification(in, in, in, in, in, in, out,
			mode_info_di, mode_info_uo) is det.

	% Work out what kind of unification a var-var unification is.
:- pred categorize_unify_var_var(mode, mode, is_live, is_live, var, var,
			determinism, unify_context, map(var, type), mode_info,
			hlds_goal_expr, mode_info).
:- mode categorize_unify_var_var(in, in, in, in, in, in, in, in, in,
			mode_info_di, out, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module llds, prog_data, prog_util, type_util, module_qual, instmap.
:- import_module hlds_module, hlds_goal, hlds_pred, hlds_data, hlds_out.
:- import_module mode_debug, mode_util, mode_info, modes, mode_errors.
:- import_module inst_match, unify_proc, code_util, unique_modes.
:- import_module typecheck, modecheck_call.
:- import_module bool, list, std_util, int, map, set, require, varset.
:- import_module string, assoc_list.

%-----------------------------------------------------------------------------%

modecheck_unification(X, var(Y), _Unification0, UnifyContext, _GoalInfo, _,
			Unify, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, X, InstOfX),
	instmap__lookup_var(InstMap0, Y, InstOfY),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_is_live(ModeInfo0, Y, LiveY),
	(
		( LiveX = live, LiveY = live ->
			BothLive = live
		;
			BothLive = dead
		),
		abstractly_unify_inst(BothLive, InstOfX, InstOfY,
			real_unify, ModuleInfo0, UnifyInst, Det1, ModuleInfo1)
	->
		Inst = UnifyInst,
		Det = Det1,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		modecheck_set_var_inst(Y, Inst, ModeInfo2, ModeInfo3),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		mode_info_get_var_types(ModeInfo3, VarTypes),
		categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y,
			Det, UnifyContext, VarTypes, ModeInfo3, Unify, ModeInfo)
	;
		set__list_to_set([X, Y], WaitingVars),
		mode_info_error(WaitingVars, mode_error_unify_var_var(X, Y,
				InstOfX, InstOfY), ModeInfo0, ModeInfo1),
			% If we get an error, set the inst to not_reached
			% to suppress follow-on errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		modecheck_set_var_inst(Y, Inst, ModeInfo2, ModeInfo),
			% return any old garbage
		Unification = assign(X, Y),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Modes = ModeOfX - ModeOfY,
		Unify = unify(X, var(Y), Modes, Unification, UnifyContext)
	).

modecheck_unification(X0, functor(ConsId0, ArgVars0), Unification0,
			UnifyContext, GoalInfo0, HowToCheckGoal,
			Goal, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, X0, TypeOfX),
	module_info_get_predicate_table(ModuleInfo0, PredTable),
	list__length(ArgVars0, Arity),
	mode_info_get_predid(ModeInfo0, ThisPredId),
	(
		%
		% is the function symbol apply/N or ''/N,
		% representing a higher-order function call?
		%
		% (As an optimization, if HowToCheck = check_unique_modes,
		% then don't bother checking, since they will have already
		% been expanded.)
		%
		HowToCheckGoal \= check_unique_modes,
		ConsId0 = cons(unqualified(ApplyName), _),
		( ApplyName = "apply" ; ApplyName = "" ),
		Arity >= 2,
		ArgVars0 = [FuncVar | FuncArgVars]
	->
		%
		% Convert the higher-order function call (apply/N)
		% into a higher-order predicate call
		% (i.e., replace `X = apply(F, A, B, C)'
		% with `call(F, A, B, C, X)')
		% and then mode-check it.
		%
		modecheck_higher_order_func_call(FuncVar, FuncArgVars, X0,
			GoalInfo0, Goal, ModeInfo0, ModeInfo)
	;
		%
		% is the function symbol a user-defined function, rather
		% than a functor which represents a data constructor?
		%

		% As an optimization, if HowToCheck = check_unique_modes,
		% then don't bother checking, since they will have already
		% been expanded.
		HowToCheckGoal \= check_unique_modes,

		% Find the set of candidate predicates which have the
		% specified name and arity (and module, if module-qualified)
		ConsId0 = cons(PredName, _),
		module_info_pred_info(ModuleInfo0, ThisPredId, PredInfo),

		%
		% We don't do this for compiler-generated predicates;
		% they are assumed to have been generated with all
		% functions already expanded.
		% If we did this check for compiler-generated
		% predicates, it would cause the wrong behaviour
		% in the case where there is a user-defined function
		% whose type is exactly the same as the type of
		% a constructor.  (Normally that would cause
		% a type ambiguity error, but compiler-generated
		% predicates are not type-checked.)
		%

		\+ code_util__compiler_generated(PredInfo),
		(
			PredName = unqualified(UnqualPName),
			predicate_table_search_func_name_arity(PredTable,
				UnqualPName, Arity, PredIds),
  
			% Check if there any of the candidate functions,
			% have argument/return types which subsume the actual
			% argument/return types of this function call
  
			pred_info_typevarset(PredInfo, TVarSet),
			map__apply_to_list(ArgVars0, VarTypes0, ArgTypes0),
			list__append(ArgTypes0, [TypeOfX], ArgTypes),
			typecheck__find_matching_pred_id(PredIds, ModuleInfo0,
				TVarSet, ArgTypes, PredId, QualifiedFuncName)

		;
			PredName = qualified(FuncModule, UnqualName),
			predicate_table_search_func_m_n_a(PredTable,
				    FuncModule, UnqualName, Arity, [PredId]),
			QualifiedFuncName = PredName
		)
	->
		%
		% Convert function calls into predicate calls:
		% replace `X = f(A, B, C)'
		% with `f(A, B, C, X)'
		%
		invalid_proc_id(ProcId),
		list__append(ArgVars0, [X0], ArgVars),
		FuncCallUnifyContext = call_unify_context(X0,
			functor(ConsId0, ArgVars0), UnifyContext),
		FuncCall = call(PredId, ProcId, ArgVars, not_builtin,
			yes(FuncCallUnifyContext), QualifiedFuncName),
		%
		% now modecheck it
		%
		modecheck_goal_expr(FuncCall, GoalInfo0, Goal, ModeInfo0, ModeInfo)

	;

	%
	% We replace any unifications with higher-order pred constants
	% by lambda expressions.  For example, we replace
	%
	%       X = list__append(Y)     % Y::in, X::out
	%
	% with
	%
	%       X = lambda [A1::in, A2::out] (list__append(Y, A1, A2))
	%
	% We do this because it makes two things easier.
	% Firstly, we need to check that the lambda-goal doesn't
	% bind any non-local variables (e.g. `Y' in above example).
	% This would require a bit of moderately tricky special-case code
	% if we didn't expand them.
	% Secondly, the polymorphism pass (polymorphism.m) is a lot easier
	% if we don't have to handle higher-order pred consts.
	% If it turns out that the predicate was non-polymorphic,
	% lambda.m will (I hope) turn the lambda expression
	% back into a higher-order pred constant again.
	%

		% check if variable has a higher-order type
		type_is_higher_order(TypeOfX, PredOrFunc, PredArgTypes),
		ConsId0 = cons(PName, _),
		% but in case we are redoing mode analysis, make sure
		% we don't mess with the address constants for type_info
		% fields created by polymorphism.m
		Unification0 \= construct(_, code_addr_const(_, _), _, _),
		Unification0 \= deconstruct(_, code_addr_const(_, _), _, _, _)
	->
		%
		% Create the new lambda-quantified variables
		%
		mode_info_get_varset(ModeInfo0, VarSet0),
		make_fresh_vars(PredArgTypes, VarSet0, VarTypes0,
				LambdaVars, VarSet, VarTypes),
		list__append(ArgVars0, LambdaVars, Args),
		mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
		mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo2),

		%
		% Build up the hlds_goal_expr for the call that will form
		% the lambda goal
		%

		module_info_pred_info(ModuleInfo0, ThisPredId, ThisPredInfo),
		pred_info_typevarset(ThisPredInfo, TVarSet),
		map__apply_to_list(Args, VarTypes, ArgTypes),
		get_pred_id_and_proc_id(PName, PredOrFunc, TVarSet, ArgTypes,
			ModuleInfo0, PredId, ProcId),
		module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
					PredInfo, ProcInfo),

		% module-qualify the pred name (is this necessary?)
		pred_info_module(PredInfo, PredModule),
		unqualify_name(PName, UnqualPName),
		QualifiedPName = qualified(PredModule, UnqualPName),

		CallUnifyContext = call_unify_context(X0,
				functor(ConsId0, ArgVars0), UnifyContext),
		LambdaGoalExpr = call(PredId, ProcId, Args, not_builtin,
				yes(CallUnifyContext), QualifiedPName),

		%
		% construct a goal_info for the lambda goal, making sure
		% to set up the nonlocals field in the goal_info correctly
		%
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__insert_list(NonLocals, LambdaVars, OutsideVars),
		set__list_to_set(Args, InsideVars),
		set__intersect(OutsideVars, InsideVars, LambdaNonLocals),
		goal_info_init(LambdaGoalInfo0),
		goal_info_set_nonlocals(LambdaGoalInfo0, LambdaNonLocals,
				LambdaGoalInfo),
		LambdaGoal = LambdaGoalExpr - LambdaGoalInfo,

		%
		% work out the modes of the introduced lambda variables
		% and the determinism of the lambda goal
		%
		proc_info_argmodes(ProcInfo, ArgModes),
		( list__drop(Arity, ArgModes, LambdaModes0) ->
			LambdaModes = LambdaModes0
		;
			error("modecheck_unification: list__drop failed")
		),
		proc_info_declared_determinism(ProcInfo, MaybeDet),
		( MaybeDet = yes(Det) ->
			LambdaDet = Det
		;
			error("Sorry, not implemented: determinism inference for higher-order predicate terms")
		),

		%
		% construct the lambda expression, and then go ahead
		% and modecheck this unification in its new form
		%
		Functor0 = lambda_goal(PredOrFunc, LambdaVars, LambdaModes,
					LambdaDet, LambdaGoal),
		modecheck_unification( X0, Functor0, Unification0, UnifyContext,
				GoalInfo0, HowToCheckGoal, Goal,
				ModeInfo2, ModeInfo)
	;
		%
		% It's not a higher-order pred unification - just
		% call modecheck_unify_functor to do the ordinary thing.
		%
		mode_info_get_instmap(ModeInfo0, InstMap0),
		modecheck_unify_functor(X0, TypeOfX,
			ConsId0, ArgVars0, Unification0, ExtraGoals, Mode,
			ConsId, ArgVars, Unification, ModeInfo0, ModeInfo),
		%
		% Optimize away construction of unused terms by
		% replacing the unification with `true'.
		%
		(
			Unification = construct(ConstructTarget, _, _, _),
			mode_info_var_is_live(ModeInfo, ConstructTarget, dead)
		->
			Goal = conj([])
		;
			Functor = functor(ConsId, ArgVars),
			Unify = unify(X, Functor, Mode, Unification,
				UnifyContext),
			X = X0,
			%
			% modecheck_unification sometimes needs to introduce
			% new goals to handle complicated sub-unifications
			% in deconstructions.  But this should never happen
			% during unique mode analysis.
			% (If it did, the code would be wrong since it
			% wouldn't have the correct determinism annotations.)
			%
			(
				HowToCheckGoal = check_unique_modes,
				ExtraGoals \= [] - []
			->
				error("unique_modes.m: re-modecheck of unification encountered complicated sub-unifies")
			;
				true
			),

			handle_extra_goals(Unify, ExtraGoals, GoalInfo0,
						[X0|ArgVars0], [X|ArgVars],
						InstMap0, ModeInfo, Goal)
		)
	).

modecheck_unification(X, lambda_goal(PredOrFunc, Vars, Modes0, Det, Goal0),
			Unification0, UnifyContext, _GoalInfo, HowToCheckGoal,
			unify(X, RHS, Mode, Unification, UnifyContext),
			ModeInfo0, ModeInfo) :-
	%
	% First modecheck the lambda goal itself:
	%
	% initialize the initial insts of the lambda variables,
	% lock the non-local vars,
	% mark the non-clobbered lambda variables as live,
	% modecheck the goal,
	% check that the final insts are correct,
	% unmark the live vars,
	% unlock the non-local vars,
	% restore the original instmap.
	%
	% XXX or should we merge the original and the final instmaps???
	%
	% The reason that we need to merge the original and final instmaps
	% is as follows.  The lambda goal will not have bound any variables
	% (since they were locked), but it may have added some information
	% or lost some uniqueness.  We cannot use the final instmap,
	% because that may have too much information.  If we use the
	% initial instmap, variables will be considered as unique
	% even if they become shared or clobbered in the lambda goal!
	%

	mode_info_get_module_info(ModeInfo0, ModuleInfo0),

	( HowToCheckGoal = check_modes ->
		% This only needs to be done once.
		mode_info_get_types_of_vars(ModeInfo0, Vars, VarTypes),
		propagate_types_into_mode_list(VarTypes, ModuleInfo0,
			Modes0, Modes)
 	;
		Modes = Modes0
	),
 
	% initialize the initial insts of the lambda variables
	mode_list_get_initial_insts(Modes, ModuleInfo0, VarInitialInsts),
	assoc_list__from_corresponding_lists(Vars, VarInitialInsts, VarInstAL),
	instmap_delta_from_assoc_list(VarInstAL, VarInstMapDelta),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__apply_instmap_delta(InstMap0, VarInstMapDelta, InstMap1),
	mode_info_set_instmap(InstMap1, ModeInfo0, ModeInfo1),
 
	% mark the non-clobbered lambda variables as live
	get_arg_lives(Modes, ModuleInfo0, ArgLives),
	get_live_vars(Vars, ArgLives, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),
	mode_info_add_live_vars(LiveVars, ModeInfo1, ModeInfo2),
 
	% lock the non-locals
	% (a lambda goal is not allowed to bind any of the non-local
	% variables, since it could get called more than once)
	Goal0 = _ - GoalInfo0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals),
	mode_info_lock_vars(NonLocals, ModeInfo2, ModeInfo3),
 
	mode_checkpoint(enter, "lambda goal", ModeInfo3, ModeInfo4),
	% if we're being called from unique_modes.m, then we need to 
	% call unique_modes__check_goal rather than modecheck_goal.
	( HowToCheckGoal = check_unique_modes ->
		unique_modes__check_goal(Goal0, Goal, ModeInfo4, ModeInfo5)
	;
		modecheck_goal(Goal0, Goal, ModeInfo4, ModeInfo5)
	),
	mode_list_get_final_insts(Modes, ModuleInfo0, FinalInsts),
	modecheck_final_insts(Vars, FinalInsts, ModeInfo5, ModeInfo6),
	mode_checkpoint(exit, "lambda goal", ModeInfo6, ModeInfo7),
 
	mode_info_remove_live_vars(LiveVars, ModeInfo7, ModeInfo8),
	mode_info_unlock_vars(NonLocals, ModeInfo8, ModeInfo9),
	mode_info_set_instmap(InstMap0, ModeInfo9, ModeInfo10),
 
	%
	% Now modecheck the unification of X with the lambda-expression.
	%
 
	set__to_sorted_list(NonLocals, ArgVars),
	modecheck_unify_lambda(X, PredOrFunc, ArgVars, Modes,
			Det, Unification0, Mode, Unification,
			ModeInfo10, ModeInfo),
	RHS = lambda_goal(PredOrFunc, Vars, Modes, Det, Goal).

:- pred modecheck_unify_lambda(var, pred_or_func, list(var),
			list(mode), determinism, unification,
			pair(mode), unification, mode_info, mode_info).
:- mode modecheck_unify_lambda(in, in, in, in, in, in,
			out, out, mode_info_di, mode_info_uo) is det.

modecheck_unify_lambda(X, PredOrFunc, ArgVars, LambdaModes, LambdaDet,
		Unification0, Mode, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, X, InstOfX),
	InstOfY = ground(unique, yes(LambdaPredInfo)),
	LambdaPredInfo = pred_inst_info(PredOrFunc, LambdaModes, LambdaDet),
	(
		abstractly_unify_inst(dead, InstOfX, InstOfY, real_unify,
			ModuleInfo0, UnifyInst, _Det, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		% the lambda expression just maps its argument variables
		% from their current insts to the same inst
		instmap__lookup_vars(ArgVars, InstMap0, ArgInsts),
		inst_lists_to_mode_list(ArgInsts, ArgInsts, ArgModes),
		categorize_unify_var_lambda(ModeOfX, ArgModes,
				X, ArgVars, PredOrFunc,
				Unification0, ModeInfo1,
				Unification, ModeInfo2),
		modecheck_set_var_inst(X, Inst, ModeInfo2, ModeInfo)
	;
		set__list_to_set([X], WaitingVars),
		mode_info_error(WaitingVars,
			mode_error_unify_var_lambda(X, InstOfX, InstOfY),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
			% return any old garbage
		Unification = Unification0
	).

:- pred modecheck_unify_functor(var, (type), cons_id, list(var), unification,
			pair(list(hlds_goal)), pair(mode), cons_id, list(var),
			unification, mode_info, mode_info).
:- mode modecheck_unify_functor(in, in, in, in, in, out, out, out, out, out,
			mode_info_di, mode_info_uo) is det.

modecheck_unify_functor(X, TypeOfX, ConsId0, ArgVars0, Unification0,
			ExtraGoals, Mode, ConsId, ArgVars, Unification,
			ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	list__length(ArgVars0, Arity),
	(
		% module qualify cons_ids
		ConsId0 = cons(unqualified(Name), _),
		type_to_type_id(TypeOfX, TypeId, _),
		TypeId = qualified(TypeModule, _) - _
	->
		ConsId = cons(qualified(TypeModule, Name), Arity)
	;
		ConsId = ConsId0
	),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, X, InstOfX),
	instmap__lookup_vars(ArgVars0, InstMap0, InstArgs),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_list_is_live(ArgVars0, ModeInfo0, LiveArgs),
	InstOfY = bound(unique, [functor(ConsId, InstArgs)]),
	(
		% The occur check: X = f(X) is considered a mode error
		% unless X is ground.  (Actually it wouldn't be that
		% hard to generate code for it - it always fails! -
		% but it's most likely to be a programming error,
		% so it's better to report it.)

		list__member(X, ArgVars0),
		\+ inst_is_ground(ModuleInfo0, InstOfX)
	->
		set__list_to_set([X], WaitingVars),
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, ConsId, ArgVars0,
							InstOfX, InstArgs),
			ModeInfo0, ModeInfo1
		),
		Inst = not_reached,
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		( bind_args(Inst, ArgVars0, ModeInfo2, ModeInfo3) ->
			ModeInfo = ModeInfo3
		;
			error("bind_args failed")
		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals = [] - []
	;
		abstractly_unify_inst_functor(LiveX, InstOfX, ConsId,
			InstArgs, LiveArgs, real_unify, ModuleInfo0,
			UnifyInst, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		( get_mode_of_args(Inst, InstArgs, ModeArgs0) ->
			ModeArgs = ModeArgs0
		;
			error("get_mode_of_args failed")
		),
		(
			inst_expand(ModuleInfo1, InstOfX, InstOfX1),
			get_arg_insts(InstOfX1, ConsId, Arity, InstOfXArgs),
			get_mode_of_args(Inst, InstOfXArgs, ModeOfXArgs0)
		->
			ModeOfXArgs = ModeOfXArgs0
		;
			error("get_(inst/mode)_of_args failed")
		),
		mode_info_get_var_types(ModeInfo1, VarTypes),
		categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ModeArgs,
				X, ConsId, ArgVars0, VarTypes,
				Unification0, ModeInfo1,
				Unification1, ModeInfo2),
		split_complicated_subunifies(Unification1, ArgVars0,
					Unification, ArgVars, ExtraGoals,
					ModeInfo2, ModeInfo3),
		modecheck_set_var_inst(X, Inst, ModeInfo3, ModeInfo4),
		( bind_args(Inst, ArgVars, ModeInfo4, ModeInfo5) ->
			ModeInfo = ModeInfo5
		;
			error("bind_args failed")
		)
	;
		set__list_to_set([X | ArgVars0], WaitingVars), % conservative
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, ConsId, ArgVars0,
							InstOfX, InstArgs),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		( bind_args(Inst, ArgVars0, ModeInfo2, ModeInfo3) ->
			ModeInfo = ModeInfo3
		;
			error("bind_args failed")
		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals = [] - []
	).

%-----------------------------------------------------------------------------%

:- pred modecheck_higher_order_func_call(var, list(var), var, hlds_goal_info,
		hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_higher_order_func_call(in, in, in, in, out,
		mode_info_di, mode_info_uo) is det.

modecheck_higher_order_func_call(FuncVar, Args0, RetVar, GoalInfo0, Goal) -->
	mode_checkpoint(enter, "higher-order function call"),
	mode_info_set_call_context(higher_order_call(function)),

	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },

	{ list__append(Args0, [RetVar], Args1) },
	modecheck_higher_order_call(function, FuncVar, Args1,
			Types, Modes, Det, Args, ExtraGoals),

	=(ModeInfo),
	{ Call = higher_order_call(FuncVar, Args, Types, Modes, Det) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0,
				[FuncVar | Args1], [FuncVar | Args],
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order function call").

%-----------------------------------------------------------------------------%

	% The argument unifications in a construction or deconstruction
	% unification must be simple assignments, they cannot be
	% complicated unifications.  If they are, we split them out
	% into separate unifications by introducing fresh variables here.

:- pred split_complicated_subunifies(unification, list(var),
			unification, list(var), pair(list(hlds_goal)),
			mode_info, mode_info).
:- mode split_complicated_subunifies(in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

split_complicated_subunifies(Unification0, ArgVars0,
				Unification, ArgVars, ExtraGoals) -->
	(
		{ Unification0 = deconstruct(X, ConsId, ArgVars0, ArgModes0,
			Det) }
	->
		(
			split_complicated_subunifies_2(ArgVars0, ArgModes0,
				ArgVars1, ArgModes, ExtraGoals1)
		->
			{ ArgVars = ArgVars1 },
			{ Unification = deconstruct(X, ConsId, ArgVars,
							ArgModes, Det) },
			{ ExtraGoals = ExtraGoals1 }
		;
			{ error("split_complicated_subunifies_2 failed") }
		)
	;
		{ Unification = Unification0 },
		{ ArgVars = ArgVars0 },
		{ ExtraGoals = [] - [] }
	).

:- pred split_complicated_subunifies_2(list(var), list(uni_mode),
			list(var), list(uni_mode), pair(list(hlds_goal)),
			mode_info, mode_info).
:- mode split_complicated_subunifies_2(in, in, out, out, out,
		mode_info_di, mode_info_uo) is semidet.

split_complicated_subunifies_2([], [], [], [], [] - []) --> [].
split_complicated_subunifies_2([Var0 | Vars0], [UniMode0 | UniModes0],
			Vars, UniModes, ExtraGoals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	UniMode0 = (InitialInstX - InitialInstY -> FinalInstX - FinalInstY),
	ModeX = (InitialInstX -> FinalInstX),
	ModeY = (InitialInstY -> FinalInstY),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, Var0, VarType),
	(
		mode_to_arg_mode(ModuleInfo, ModeX, VarType, top_in),
		mode_to_arg_mode(ModuleInfo, ModeY, VarType, top_in)
	->
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, UniModes1, ExtraGoals0,
				ModeInfo0, ModeInfo1),
		ExtraGoals0 = BeforeGoals - AfterGoals0,

		% introduce a new variable `Var'
		mode_info_get_varset(ModeInfo1, VarSet1),
		mode_info_get_var_types(ModeInfo1, VarTypes1),
		varset__new_var(VarSet1, Var, VarSet),
		map__set(VarTypes1, Var, VarType, VarTypes),
		mode_info_set_varset(VarSet, ModeInfo1, ModeInfo2),
		mode_info_set_var_types(VarTypes, ModeInfo2, ModeInfo3),

		% change the main unification to use `Var' instead of Var0
		UniMode = (InitialInstX - free -> InitialInstX - InitialInstX),
		UniModes = [UniMode | UniModes1],
		Vars = [Var | Vars1],

		% create code to do a unification between `Var' and `Var0'
		ModeVar0 = (InitialInstY -> FinalInstY),
		ModeVar = (InitialInstX -> FinalInstX),

		mode_info_get_module_info(ModeInfo3, ModuleInfo0),
		(
			abstractly_unify_inst(dead, InitialInstX, InitialInstY,
				real_unify, ModuleInfo0, _, Det1, ModuleInfo1)
		->
			mode_info_set_module_info(ModeInfo3, ModuleInfo1,
				ModeInfo4),
			Det = Det1
		;
			ModeInfo4 = ModeInfo3,
			Det = semidet
			% XXX warning - it might be det in some cases.
			% should we report an error here?  should this
			% ever happen?
		),
		mode_info_get_mode_context(ModeInfo4, ModeContext),
		mode_context_to_unify_context(ModeContext, ModeInfo4,
						UnifyContext),
		categorize_unify_var_var(ModeVar0, ModeVar,
			live, dead, Var0, Var, Det, UnifyContext,
			VarTypes, ModeInfo4, AfterGoal, ModeInfo),

		% compute the goal_info nonlocal vars & instmap delta
		% for the newly created goal
		set__list_to_set([Var0, Var], NonLocals),
		( InitialInstY = FinalInstY ->
			InstMapAL0 = []
		;
			InstMapAL0 = [Var0 - FinalInstY]
		),
		InstMapAL = [Var - FinalInstX | InstMapAL0],
		instmap_delta_from_assoc_list(InstMapAL, InstMapDelta),
		goal_info_init(GoalInfo0),
		goal_info_set_nonlocals(GoalInfo0, NonLocals,
			GoalInfo1),
		goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo),

		% insert the unification between `Var' and `Var0' at
		% the start of the AfterGoals
		AfterGoals = [AfterGoal - GoalInfo | AfterGoals0],
		ExtraGoals = BeforeGoals - AfterGoals
	;
		Vars = [Var0 | Vars1],
		UniModes = [UniMode0 | UniModes1],
		split_complicated_subunifies_2(Vars0, UniModes0,
			Vars1, UniModes1, ExtraGoals, ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

% categorize_unify_var_var works out which category a unification
% between a variable and another variable expression is - whether it is
% an assignment, a simple test or a complicated unify.

categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y, Det,
		UnifyContext, VarTypes, ModeInfo0, Unify, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		mode_is_output(ModuleInfo0, ModeOfX)
	->
		Unification = assign(X, Y),
		ModeInfo = ModeInfo0
	;
		mode_is_output(ModuleInfo0, ModeOfY)
	->
		Unification = assign(Y, X),
		ModeInfo = ModeInfo0
	;
		mode_is_unused(ModuleInfo0, ModeOfX),
		mode_is_unused(ModuleInfo0, ModeOfY)
	->
		% For free-free unifications, we pretend for a moment that they
		% are an assignment to the dead variable - they will then
		% be optimized away.
		( LiveX = dead ->
			Unification = assign(X, Y)
		; LiveY = dead ->
			Unification = assign(Y, X)
		;
			error("categorize_unify_var_var: free-free unify!")
		),
		ModeInfo = ModeInfo0
	;
		map__lookup(VarTypes, X, Type),
		(
			type_is_atomic(Type, ModuleInfo0)
		->
			Unification = simple_test(X, Y),
			ModeInfo = ModeInfo0
		;
			mode_get_insts(ModuleInfo0, ModeOfX, IX, FX),
			mode_get_insts(ModuleInfo0, ModeOfY, IY, FY),
			determinism_components(Det, CanFail, _),
			UniMode = ((IX - IY) -> (FX - FY)),
			Unification = complicated_unify(UniMode, CanFail),
			(
				type_is_higher_order(Type, PredOrFunc, _)
			->
				% we do not want to report this as an error
				% if it occurs in a compiler-generated
				% predicate - instead, we delay the error
				% until runtime so that it only occurs if
				% the compiler-generated predicate gets called
				mode_info_get_predid(ModeInfo0, PredId),
				module_info_pred_info(ModuleInfo0, PredId,
						PredInfo),
				( code_util__compiler_generated(PredInfo) ->
					ModeInfo = ModeInfo0
				;
					set__init(WaitingVars),
					mode_info_error(WaitingVars,
			mode_error_unify_pred(X, error_at_var(Y), Type, PredOrFunc),
						ModeInfo0, ModeInfo)
				)
			;
				type_to_type_id(Type, TypeId, _)
			->
				unify_proc__request_unify(TypeId - UniMode, Det,
					ModuleInfo0, ModuleInfo),
				mode_info_set_module_info(ModeInfo0, ModuleInfo,
					ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		)
	),
	%
	% Optimize away unifications with dead variables
	% and simple tests that cannot fail
	% by replacing them with `true'.
	% (The optimization of simple tests is necessary
	% because otherwise determinism analysis assumes they can fail.
	% The optimization of assignments to dead variables may be
	% necessary to stop the code generator from getting confused.)
	%
	(
		Unification = assign(AssignTarget, _),
		mode_info_var_is_live(ModeInfo, AssignTarget, dead)
	->
		Unify = conj([])
	;
		Unification = simple_test(_, _),
		Det = det
	->
		Unify = conj([])
	;
		Unify = unify(X, var(Y), ModeOfX - ModeOfY, Unification,
				UnifyContext)
	).

% categorize_unify_var_lambda works out which category a unification
% between a variable and a lambda expression is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_lambda(mode, list(mode),
			var, list(var), pred_or_func,
			unification, mode_info, unification, mode_info).
:- mode categorize_unify_var_lambda(in, in, in, in, in,
			in, mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_lambda(ModeOfX, ArgModes0, X, ArgVars, PredOrFunc,
		Unification0, ModeInfo0, Unification, ModeInfo) :-
	% if we are re-doing mode analysis, preserve the existing cons_id
	( Unification0 = construct(_, ConsId0, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _) ->
		ConsId = ConsId1
	;
		% the real cons_id will be computed by polymorphism.m;
		% we just put in a dummy one for now
		list__length(ArgVars, Arity),
		ConsId = cons(unqualified("__LambdaGoal__"), Arity)
	),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_util__modes_to_uni_modes(ArgModes0, ArgModes0,
						ModuleInfo, ArgModes),
	(
		mode_is_output(ModuleInfo, ModeOfX)
	->
		Unification = construct(X, ConsId, ArgVars, ArgModes),
		ModeInfo = ModeInfo0
	;
		% If it's a deconstruction, it is a mode error
		set__init(WaitingVars),
		mode_info_get_var_types(ModeInfo0, VarTypes0),
		map__lookup(VarTypes0, X, Type),
		mode_info_error(WaitingVars,
				mode_error_unify_pred(X,
					error_at_lambda(ArgVars, ArgModes0),
					Type, PredOrFunc),
				ModeInfo0, ModeInfo),
		% return any old garbage
		Unification = Unification0
	).

% categorize_unify_var_functor works out which category a unification
% between a variable and a functor is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_functor(mode, list(mode), list(mode), var,
		cons_id, list(var), map(var, type), unification, mode_info,
		unification, mode_info).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, in, in,
			mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ArgModes0,
		X, NewConsId, ArgVars, VarTypes, Unification0, ModeInfo0,
		Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	map__lookup(VarTypes, X, TypeOfX),
	% if we are re-doing mode analysis, preserve the existing cons_id
	( Unification0 = construct(_, ConsId0, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _) ->
		ConsId = ConsId1
	;
		ConsId = NewConsId
	),
	mode_util__modes_to_uni_modes(ModeOfXArgs, ArgModes0,
						ModuleInfo, ArgModes),
	(
		mode_is_output(ModuleInfo, ModeOfX)
	->
		Unification = construct(X, ConsId, ArgVars, ArgModes),
		ModeInfo = ModeInfo0
	;
		% It's a deconstruction.
		(
			% If the variable was already known to be bound
			% to a single particular functor, then the
			% unification either always succeeds or always
			% fails.  In the latter case, the final inst will
			% be `not_reached' or `bound([])'.  So if both
			% the initial and final inst are `bound([_])',
			% then the unification must be deterministic.
			mode_get_insts(ModuleInfo, ModeOfX,
					InitialInst0, FinalInst0),
			inst_expand(ModuleInfo, InitialInst0, InitialInst),
			inst_expand(ModuleInfo, FinalInst0, FinalInst),
			InitialInst = bound(_, [_]),
			FinalInst = bound(_, [_])
		->
			CanFail = cannot_fail,
			ModeInfo = ModeInfo0
		;
			% If the type has only one constructor,
			% then the unification cannot fail
			type_constructors(TypeOfX, ModuleInfo, Constructors),
			Constructors = [_]
		->
			CanFail = cannot_fail,
			ModeInfo = ModeInfo0
		;
			% Otherwise, it can fail
			CanFail = can_fail,
			( type_is_higher_order(TypeOfX, PredOrFunc, _) ->
				set__init(WaitingVars),
				mode_info_error(WaitingVars,
			mode_error_unify_pred(X,
					error_at_functor(ConsId, ArgVars),
					TypeOfX, PredOrFunc),
					ModeInfo0, ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		),
		Unification = deconstruct(X, ConsId, ArgVars, ArgModes, CanFail)
	).

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(var), mode_info, mode_info).
:- mode bind_args(in, in, mode_info_di, mode_info_uo) is semidet.

bind_args(not_reached, _) -->
	{ instmap__init_unreachable(InstMap) },
	mode_info_set_instmap(InstMap).
bind_args(ground(Uniq, no), Args) -->
	ground_args(Uniq, Args).
bind_args(bound(_Uniq, List), Args) -->
	( { List = [] } ->
		% the code is unreachable
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ List = [functor(_, InstList)] },
		bind_args_2(Args, InstList)
	).

:- pred bind_args_2(list(var), list(inst), mode_info, mode_info).
:- mode bind_args_2(in, in, mode_info_di, mode_info_uo) is semidet.

bind_args_2([], []) --> [].
bind_args_2([Arg | Args], [Inst | Insts]) -->
	modecheck_set_var_inst(Arg, Inst),
	bind_args_2(Args, Insts).

:- pred ground_args(uniqueness, list(var), mode_info, mode_info).
:- mode ground_args(in, in, mode_info_di, mode_info_uo) is det.

ground_args(_Uniq, []) --> [].
ground_args(Uniq, [Arg | Args]) -->
	modecheck_set_var_inst(Arg, ground(Uniq, no)),
	ground_args(Uniq, Args).

%-----------------------------------------------------------------------------%

% get_mode_of_args(FinalInst, InitialArgInsts, ArgModes):
%       for a var-functor unification,
%       given the final inst of the var
%       and the initial insts of the functor arguments,
%       compute the modes of the functor arguments

:- pred get_mode_of_args(inst, list(inst), list(mode)).
:- mode get_mode_of_args(in, in, out) is semidet.

get_mode_of_args(not_reached, ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, not_reached, ArgModes).
get_mode_of_args(any(Uniq), ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, any(Uniq), ArgModes).
get_mode_of_args(ground(Uniq, no), ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, ground(Uniq, no), ArgModes).
get_mode_of_args(bound(_Uniq, List), ArgInstsA, ArgModes) :-
	( List = [] ->
		% the code is unreachable
		mode_set_args(ArgInstsA, not_reached, ArgModes)
	;
		List = [functor(_Name, ArgInstsB)],
		get_mode_of_args_2(ArgInstsA, ArgInstsB, ArgModes)
	).

:- pred get_mode_of_args_2(list(inst), list(inst), list(mode)).
:- mode get_mode_of_args_2(in, in, out) is semidet.

get_mode_of_args_2([], [], []).
get_mode_of_args_2([InstA | InstsA], [InstB | InstsB], [Mode | Modes]) :-
	Mode = (InstA -> InstB),
	get_mode_of_args_2(InstsA, InstsB, Modes).

:- pred mode_set_args(list(inst), inst, list(mode)).
:- mode mode_set_args(in, in, out) is det.

mode_set_args([], _, []).
mode_set_args([Inst | Insts], FinalInst, [Mode | Modes]) :-
	Mode = (Inst -> FinalInst),
	mode_set_args(Insts, FinalInst, Modes).

%-----------------------------------------------------------------------------%

:- pred make_fresh_vars(list(type), varset, map(var, type),
			list(var), varset, map(var, type)).
:- mode make_fresh_vars(in, in, in, out, out, out) is det.

make_fresh_vars([], VarSet, VarTypes, [], VarSet, VarTypes).
make_fresh_vars([Type|Types], VarSet0, VarTypes0,
		[Var|Vars], VarSet, VarTypes) :-
	varset__new_var(VarSet0, Var, VarSet1),
	map__det_insert(VarTypes0, Var, Type, VarTypes1),
	make_fresh_vars(Types, VarSet1, VarTypes1, Vars, VarSet, VarTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
