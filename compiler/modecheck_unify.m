%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
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

:- import_module hlds_goal, hlds_data, prog_data, mode_info, (inst), instmap.
:- import_module map, term, list.

	% Modecheck a unification
:- pred modecheck_unification( var, unify_rhs, unification, unify_context,
			hlds_goal_info, hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_unification(in, in, in, in, in, out,
			mode_info_di, mode_info_uo) is det.

	% Work out what kind of unification a var-var unification is.
:- pred categorize_unify_var_var(inst, inst, inst, inst, is_live, is_live,
			var, var, instmap, instmap,
			determinism, unify_context, map(var, type), mode_info,
			hlds_goal_expr, mode_info).
:- mode categorize_unify_var_var(in, in, in, in, in, in, in, in, in, in, in,
			in, in, mode_info_di, out, mode_info_uo) is det.

	% Given a unification goal, which must be something that can be
	% returned from categorize_unify_var_var (i.e.  a unification
	% of two vars, a unification of a var and a functor, or a
	% conjunction thereof), return a list of all the variables in the
	% goal.
:- pred unify_vars(hlds_goal_expr, list(var)).
:- mode unify_vars(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, prog_util, type_util, module_qual.
:- import_module hlds_module, hlds_goal, hlds_pred, hlds_out.
:- import_module mode_debug, mode_util, mode_info, modes, mode_errors.
:- import_module inst_match, inst_util, unify_proc, code_util, unique_modes.
:- import_module typecheck, modecheck_call, quantification.

:- import_module bool, std_util, int, set, require, varset.
:- import_module string, assoc_list.

%-----------------------------------------------------------------------------%

:- pred mode_info_make_alias(inst, inst_key, mode_info, mode_info).
:- mode mode_info_make_alias(in, out, mode_info_di, mode_info_uo) is det.

mode_info_make_alias(Inst, InstKey, ModeInfo0, ModeInfo) :-
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	inst_table_get_inst_key_table(InstTable0, IKT0),
	inst_key_table_add(IKT0, Inst, InstKey, IKT),
	inst_table_set_inst_key_table(InstTable0, IKT, InstTable),
	mode_info_set_inst_table(InstTable, ModeInfo0, ModeInfo).

:- pred mode_info_make_aliased_insts(list(var), list(inst),
		mode_info, mode_info).
:- mode mode_info_make_aliased_insts(in, out,
		mode_info_di, mode_info_uo) is det.

mode_info_make_aliased_insts([], [], ModeInfo, ModeInfo).
mode_info_make_aliased_insts([V | Vs], [I | Is], ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, IM0),
	instmap__lookup_var(IM0, V, I0),
	( I0 = alias(_) ->
		I = I0,
		ModeInfo0 = ModeInfo2
	;
		mode_info_make_alias(I0, InstKey, ModeInfo0, ModeInfo1),
		I = alias(InstKey),
		instmap__set(IM0, V, I, IM),
		mode_info_set_instmap(IM, ModeInfo1, ModeInfo2)
	),
	mode_info_make_aliased_insts(Vs, Is, ModeInfo2, ModeInfo).

modecheck_unification(X, var(Y), _Unification0, UnifyContext, _GoalInfo,
			Unify, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
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
		abstractly_unify_inst(BothLive, InstOfX, InstOfY, real_unify,
			InstTable0, ModuleInfo0, InstMap0, UnifyInst,
			Det1, InstTable1, ModuleInfo1, InstMap1)
	->
		Inst = UnifyInst,
		Det = Det1,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		mode_info_set_inst_table(InstTable1, ModeInfo1, ModeInfo2),

		mode_info_set_instmap(InstMap1, ModeInfo2, ModeInfo3),
		( Inst = alias(_) ->
			AliasedInst = Inst,
			ModeInfo3 = ModeInfo4
		;
			mode_info_make_alias(Inst, InstKey, ModeInfo3,
				ModeInfo4),
			AliasedInst = alias(InstKey)
		),
		% XXX We force the var rather than unify it with its
		%     previous inst.  This is right, isn't it.
		modecheck_force_set_var_inst(X, AliasedInst, ModeInfo4,
			ModeInfo5),
		modecheck_force_set_var_inst(Y, AliasedInst, ModeInfo5,
			ModeInfo6),
		mode_info_get_var_types(ModeInfo6, VarTypes),
		categorize_unify_var_var(InstOfX, Inst, InstOfY, Inst,
			LiveX, LiveY, X, Y, InstMap0, InstMap1,
			Det, UnifyContext, VarTypes, ModeInfo6, Unify, ModeInfo)
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
		modecheck_set_var_inst(Y, Inst, ModeInfo1, ModeInfo),

			% return any old garbage
		Unification = assign(X, Y),
		ModeOfX = (InstOfX - Inst),
		ModeOfY = (InstOfY - Inst),
		Modes = ModeOfX - ModeOfY,
		Unify = unify(X, var(Y), Modes, Unification, UnifyContext)
	).

modecheck_unification(X0, functor(ConsId0, ArgVars0), Unification0,
			UnifyContext, GoalInfo0, Goal, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, X0, TypeOfX),
	module_info_get_predicate_table(ModuleInfo0, PredTable),
	list__length(ArgVars0, Arity),
	mode_info_get_predid(ModeInfo0, ThisPredId),
	mode_info_get_how_to_check(ModeInfo0, HowToCheckGoal),
	(
		%
		% is the function symbol apply/N or ''/N,
		% representing a higher-order function call?
		%
		% (As an optimization, if HowToCheck = check_unique_modes,
		% then don't bother checking, since they will have already
		% been expanded.)
		%
		HowToCheckGoal \= check_unique_modes(_),
		ConsId0 = cons(unqualified(ApplyName), _),
		( ApplyName = "apply" ; ApplyName = "" ),
		Arity >= 1,
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
		HowToCheckGoal \= check_unique_modes(_),

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

		predicate_table_search_func_sym_arity(PredTable,
			PredName, Arity, PredIds),

		% Check if any of the candidate functions have
		% argument/return types which subsume the actual
		% argument/return types of this function call

		pred_info_typevarset(PredInfo, TVarSet),
		map__apply_to_list(ArgVars0, VarTypes0, ArgTypes0),
		list__append(ArgTypes0, [TypeOfX], ArgTypes),
		typecheck__find_matching_pred_id(PredIds, ModuleInfo0,
			TVarSet, ArgTypes, PredId, QualifiedFuncName)
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
		(
			% If we are redoing mode analysis, use the
			% pred_id and proc_id found before, to avoid aborting
			% in get_pred_id_and_proc_id if there are multiple
			% matching procedures.
			Unification0 = construct(_, 
				pred_const(PredId0, ProcId0), _, _)
		->
			PredId = PredId0,
			ProcId = ProcId0
		;
			get_pred_id_and_proc_id(PName, PredOrFunc, TVarSet, 
				ArgTypes, ModuleInfo0, PredId, ProcId)
		),
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
		mode_info_get_context(ModeInfo2, Context),
		goal_info_set_context(LambdaGoalInfo0, Context,
				LambdaGoalInfo1),
		goal_info_set_nonlocals(LambdaGoalInfo1, LambdaNonLocals,
				LambdaGoalInfo),
		LambdaGoal = LambdaGoalExpr - LambdaGoalInfo,

		%
		% work out the modes of the introduced lambda variables
		% and the determinism of the lambda goal
		%
		proc_info_argmodes(ProcInfo, argument_modes(ArgInstTable,
					ArgModes)),

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
		instmap_delta_init_reachable(IMDelta),
		Functor0 = lambda_goal(PredOrFunc, ArgVars0, LambdaVars, 
				argument_modes(ArgInstTable, LambdaModes),
				LambdaDet, IMDelta, LambdaGoal),
		modecheck_unification( X0, Functor0, Unification0, UnifyContext,
				GoalInfo0, Goal, ModeInfo2, ModeInfo)
	;
		%
		% It's not a higher-order pred unification - just
		% call modecheck_unify_functor to do the ordinary thing.
		%
		modecheck_unify_functor(X0, TypeOfX,
			ConsId0, ArgVars0, Unification0, UnifyContext,
			GoalInfo0, Goal, ModeInfo0, ModeInfo)
	).

modecheck_unification(X, 
		lambda_goal(PredOrFunc, ArgVars, Vars, Modes0, Det,
				_, LambdaGoal0),
		Unification0, UnifyContext, GoalInfo0, 
		Goal, ModeInfo0, ModeInfo) :-
	%
	% First modecheck the lambda goal itself:
	%
	% initialize the initial insts of the lambda variables,
	% check that the non-local vars are ground (XXX or any),
	% mark the non-local vars as shared,
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
	% However even this may not be enough.  If a unique non-local
	% variable is used in its unique inst (e.g. it's used in a ui
	% mode) and then shared within the lambda body, this is unsound.
	% This variable should be marked as shared at the _top_ of the
	% lambda goal.  As for implementing this, it probably means that
	% the lambda goal should be re-modechecked, or even modechecked
	% to a fixpoint. 
	%
	% For the moment, since doing all that properly seems too hard,
	% we just share all non-local variables at the top of the lambda goal.
	% This is safe, but perhaps too conservative.
	%

	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_how_to_check(ModeInfo0, HowToCheckGoal),

	( HowToCheckGoal = check_modes ->
		% This only needs to be done once.
		Modes0 = argument_modes(ArgInstTable0, ArgModes0),
		mode_info_get_types_of_vars(ModeInfo0, Vars, VarTypes),
		propagate_types_into_mode_list(VarTypes, ArgInstTable0,
			ModuleInfo0, ArgModes0, ArgModes1),
		Modes = argument_modes(ArgInstTable0, ArgModes1)
 	;
		Modes = Modes0
	),

	% initialize the initial insts of the lambda variables
	% XXX Slightly bogus if ArgModesX has any aliasing in it.

	Modes = argument_modes(ArgInstTable, ArgModesX),
	inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable1),
	list__map(apply_inst_key_sub_mode(Sub), ArgModesX, ArgModes),
	mode_list_get_initial_insts(ArgModes, ModuleInfo0, InitialInsts),
	assoc_list__from_corresponding_lists(Vars, InitialInsts,
		VarInitialInsts),
	instmap_delta_from_assoc_list(VarInitialInsts, IMDelta),
	instmap__apply_instmap_delta(InstMap0, IMDelta, InstMap1),
 
	mode_info_set_inst_table(InstTable1, ModeInfo0, ModeInfo1),
	mode_info_set_instmap(InstMap1, ModeInfo1, ModeInfo2),

	% mark the non-clobbered lambda variables as live
	instmap__init_reachable(ArgInstMap),	% YYY
	get_arg_lives(ArgModes, ArgInstMap, ArgInstTable, ModuleInfo0,
			ArgLives),
	get_live_vars(Vars, ArgLives, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),
	mode_info_add_live_vars(LiveVars, ModeInfo2, ModeInfo3),
 
	% lock the non-locals
	% (a lambda goal is not allowed to bind any of the non-local
	% variables, since it could get called more than once, or
	% from inside a negation)
	LambdaGoal0 = _ - LambdaGoalInfo0,
	goal_info_get_nonlocals(LambdaGoalInfo0, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	instmap__lookup_vars(NonLocalsList, InstMap1, NonLocalInsts),
	mode_info_get_module_info(ModeInfo3, ModuleInfo3),
	mode_info_get_inst_table(ModeInfo3, InstTable3),
	mode_info_get_instmap(ModeInfo3, InstMap3),
	(
		% XXX This test is too conservative.
		%
		%     We should allow non-local variables to be non-ground
		%     sometimes, possibly dependent on whether or not they
		%     are dead after this unification.  In addition, we
		%     should not "share" a unique non-local variable if
		%     these two conditions hold:
		%
		%	- It is dead after this unification.
		%	- It is not shared within the lambda body.
		%
		%     Unfortunately, we can't test the latter condition
		%     until after we've mode-checked the lambda body.
		%     (See the above comment on merging the initial and
		%     final instmaps.)

		% XXX This test is also not conservative enough!
		%
		%     We should not allow non-local vars to have inst `any';
		%     because that can lead to unsoundness.
		%     However, disallowing that idiom would break
		%     extras/trailed_update/samples/vqueens.m, and
		%     would make freeze/3 basically useless...
		%     so for now at least, let's not disallow it,
		%     even though it is unsafe.

		inst_list_is_ground_or_any(NonLocalInsts, InstMap3, InstTable3,
				ModuleInfo3)
	->
		make_shared_inst_list(NonLocalInsts, InstTable3, ModuleInfo3,
			InstMap3, SharedNonLocalInsts, InstTable4, ModuleInfo4,
			InstMap4),
		instmap__set_vars(InstMap4, NonLocalsList, SharedNonLocalInsts,
			InstMap5),
		mode_info_set_module_info(ModeInfo3, ModuleInfo4, ModeInfo5),
		mode_info_set_inst_table(InstTable4, ModeInfo5, ModeInfo6),
		mode_info_set_instmap(InstMap5, ModeInfo6, ModeInfo7),

		mode_info_lock_vars(lambda(PredOrFunc), NonLocals,
				ModeInfo7, ModeInfo8),

		mode_checkpoint(enter, "lambda goal", GoalInfo0,
			ModeInfo8, ModeInfo9),
		% if we're being called from unique_modes.m, then we need to 
		% call unique_modes__check_goal rather than modecheck_goal.
		(
			HowToCheckGoal = check_unique_modes(_)
		->
			unique_modes__check_goal(LambdaGoal0, LambdaGoal,
				ModeInfo9, ModeInfo10)
		;
			modecheck_goal(LambdaGoal0, LambdaGoal,
				ModeInfo9, ModeInfo10)
		),
		mode_list_get_final_insts(ArgModesX, ModuleInfo4, FinalInsts),
		modecheck_final_insts(Vars, FinalInsts, ModeInfo10, ModeInfo11),
		mode_checkpoint(exit, "lambda goal", GoalInfo0,
				ModeInfo11, ModeInfo12),

		mode_info_remove_live_vars(LiveVars, ModeInfo12, ModeInfo13),
		mode_info_unlock_vars(lambda(PredOrFunc), NonLocals,
			ModeInfo13, ModeInfo14),

		%
		% Ensure that the non-local vars are shared OUTSIDE the
		% lambda unification as well as inside.
		%

		instmap__set_vars(InstMap0, NonLocalsList, SharedNonLocalInsts,
			InstMap14),
		mode_info_set_instmap(InstMap14, ModeInfo14, ModeInfo15),

		%
		% Now modecheck the unification of X with the lambda-expression.
		%

		RHS0 = lambda_goal(PredOrFunc, ArgVars, Vars, Modes, Det,
				IMDelta, LambdaGoal),
		modecheck_unify_lambda(X, PredOrFunc, ArgVars, Modes,
				Det, RHS0, Unification0, Mode,
				RHS, Unification, ModeInfo15, ModeInfo)
	;
		ModeInfo3 = ModeInfo4,
		list__filter(lambda([Var :: in] is semidet,
			( instmap__lookup_var(InstMap1, Var, Inst),
			  \+ inst_is_ground(Inst, InstMap3, InstTable3,
					ModuleInfo3)
			)),
			NonLocalsList, NonGroundNonLocals),
		( NonGroundNonLocals = [BadVar | _] ->
			instmap__lookup_var(InstMap1, BadVar, BadInst),
			set__singleton_set(WaitingVars, BadVar),
			mode_info_error(WaitingVars,
				mode_error_non_local_lambda_var(BadVar,
						BadInst),
				ModeInfo4, ModeInfo)
		;
			error("modecheck_unification(lambda): very strange var")
		),
			% return any old garbage
		RHS = lambda_goal(PredOrFunc, ArgVars, Vars,
				Modes0, Det, IMDelta, LambdaGoal0),
		Mode = (free(unique) - free(unique)) -
			(free(unique) - free(unique)),
		Unification = Unification0
	),
	Goal = unify(X, RHS, Mode, Unification, UnifyContext).

:- pred modecheck_unify_lambda(var, pred_or_func, list(var),
		argument_modes, determinism, unify_rhs, unification,
		unify_mode, unify_rhs, unification, mode_info, mode_info).
:- mode modecheck_unify_lambda(in, in, in, in, in, in, in,
			out, out, out, mode_info_di, mode_info_uo) is det.

modecheck_unify_lambda(X, PredOrFunc, ArgVars, LambdaModes, 
		LambdaDet, RHS0, Unification0, Mode, RHS, Unification, 
		ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	instmap__lookup_var(InstMap0, X, InstOfX),
	InstOfY = ground(unique, yes(LambdaPredInfo)),
	LambdaPredInfo = pred_inst_info(PredOrFunc, LambdaModes, LambdaDet),
	(
		abstractly_unify_inst(dead, InstOfX, InstOfY, real_unify,
			InstTable0, ModuleInfo0, InstMap0, UnifyInst, _Det,
			InstTable1, ModuleInfo1, InstMap2)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		mode_info_set_inst_table(InstTable1, ModeInfo1, ModeInfo2),
		mode_info_set_instmap(InstMap2, ModeInfo2, ModeInfo3),
		ModeOfX = (InstOfX - Inst),
		ModeOfY = (InstOfY - Inst),
		Mode = ModeOfX - ModeOfY,
		% the lambda expression just maps its argument variables
		% from their current insts to the same inst
		instmap__lookup_vars(ArgVars, InstMap2, ArgInsts),
		assoc_list__from_corresponding_lists(ArgInsts, ArgInsts,
				ArgModes0),
		mode_util__inst_pairs_to_uni_modes(ArgModes0, ArgModes0,
				ArgModes),
		categorize_unify_var_lambda(InstOfX, Inst, ArgModes,
				X, ArgVars, InstMap0, InstMap2, PredOrFunc,
				LambdaModes, RHS0, Unification0, ModeInfo3,
				RHS, Unification, ModeInfo4),
		modecheck_set_var_inst(X, Inst, ModeInfo4, ModeInfo)
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
		ModeOfX = (InstOfX - Inst),
		ModeOfY = (InstOfY - Inst),
		Mode = ModeOfX - ModeOfY,
			% return any old garbage
		Unification = Unification0,
		RHS = RHS0
	).

:- pred modecheck_unify_functor(var, (type), cons_id, list(var), unification,
			unify_context, hlds_goal_info,
			hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_unify_functor(in, in, in, in, in, in, in,
			out, mode_info_di, mode_info_uo) is det.

modecheck_unify_functor(X, TypeOfX, ConsId0, ArgVars0, Unification0,
			UnifyContext, GoalInfo0, Goal, ModeInfo0, ModeInfo) :-
	mode_info_get_how_to_check(ModeInfo0, HowToCheckGoal),

	%
	% fully module qualify all cons_ids
	% (except for builtins such as ints and characters).
	%
	list__length(ArgVars0, Arity),
	(
		ConsId0 = cons(Name, _),
		type_to_type_id(TypeOfX, TypeId, _),
		TypeId = qualified(TypeModule, _) - _
	->
		unqualify_name(Name, UnqualName),
		ConsId = cons(qualified(TypeModule, UnqualName), Arity)
	;
		ConsId = ConsId0
	),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, X, InstOfX),
	mode_info_make_aliased_insts(ArgVars0, InstArgs, ModeInfo0, ModeInfo1),
	mode_info_get_module_info(ModeInfo1, ModuleInfo1),
	mode_info_get_inst_table(ModeInfo1, InstTable1),
	mode_info_get_instmap(ModeInfo1, InstMap1),
	mode_info_var_is_live(ModeInfo1, X, LiveX),
	mode_info_var_list_is_live(ArgVars0, ModeInfo1, LiveArgs),
	InstOfY = bound(unique, [functor(ConsId, InstArgs)]),
	(
		% The occur check: X = f(X) is considered a mode error
		% unless X is ground.  (Actually it wouldn't be that
		% hard to generate code for it - it always fails! -
		% but it's most likely to be a programming error,
		% so it's better to report it.)

		list__member(X, ArgVars0),
		\+ inst_is_ground(InstOfX, InstMap1, InstTable1, ModuleInfo1)
	->
		set__list_to_set([X], WaitingVars),
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, ConsId, ArgVars0,
							InstOfX, InstArgs),
			ModeInfo1, ModeInfo2
		),
		Inst = not_reached,
		Det = erroneous,
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		ModeOfX = (InstOfX - Inst),
		ModeOfY = (InstOfY - Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo2, ModeInfo4),
%		( bind_args(Inst, ArgVars0, ModeInfo4, ModeInfo5) ->
			ModeInfo = ModeInfo4,
%		;
%			error("bind_args failed")
%		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals = no_extra_goals
	;
		abstractly_unify_inst_functor(LiveX, InstOfX, ConsId,
			InstArgs, LiveArgs, real_unify, InstTable1,
			ModuleInfo1, InstMap1,
			UnifyInst, Det1, InstTable2, ModuleInfo2, InstMap2),
		\+ inst_contains_free_alias(UnifyInst, InstMap2,
			InstTable2, ModuleInfo2)

			% AAA when we allow users to create
			% free(alias) insts themselves we will need a
			% better scheduling algorithm.  For now, it's
			% ok to disallow free(alias) insts in
			% mode-checking because they are only created
			% in the LCO pass.
			% One algorithm would be to schedule all constructions
			% as early as possible and then, in the code generator,
			% cache references to free(alias) variables until they
			% are actually needed.
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo1, ModuleInfo2, ModeInfo2),
		mode_info_set_inst_table(InstTable2, ModeInfo2, ModeInfo3),
		mode_info_set_instmap(InstMap2, ModeInfo3, ModeInfo4),
		ModuleInfo4 = ModuleInfo2,
		InstTable4 = InstTable2,
		Det = Det1,
		ModeOfX = (InstOfX - Inst),
		ModeOfY = (InstOfY - Inst),
		Mode = ModeOfX - ModeOfY,
		instmap__lookup_vars(ArgVars0, InstMap2, FinalInstsY),
		assoc_list__from_corresponding_lists(InstArgs, FinalInstsY,
				ModeArgs),
		(
			inst_expand(InstMap1, InstTable4, ModuleInfo4,
				InstOfX, InstOfX2),
			get_arg_insts(InstOfX2, ConsId, Arity, InitialInstsX),
			inst_expand(InstMap2, InstTable4, ModuleInfo4,
				Inst, Inst2),
			get_arg_insts(Inst2, ConsId, Arity, FinalInstsX)
		->
			assoc_list__from_corresponding_lists(InitialInstsX,
				FinalInstsX, ModeOfXArgs)
		;
			error("get_arg_insts failed")
		),
		mode_info_get_var_types(ModeInfo4, VarTypes),
		categorize_unify_var_functor(InstOfX, Inst, ModeOfXArgs,
				ModeArgs, X, ConsId, ArgVars0, InstMap1,
				InstMap2, VarTypes, Det1,
				Unification0, ModeInfo4,
				Unification1, ModeInfo5),
		split_complicated_subunifies(InstMap1, Unification1, ArgVars0,
					Unification, ArgVars, ExtraGoals,
					ModeInfo5, ModeInfo6),
		% XXX We force the var rather than unify it with its
		%     previous inst.  This is right, isn't it.
		modecheck_force_set_var_inst(X, Inst, ModeInfo6, ModeInfo7),
%		( bind_args(Inst, ArgVars, ModeInfo7, ModeInfo8) ->
			ModeInfo = ModeInfo7
%		;
%			error("bind_args failed")
%		)
	;
		set__list_to_set([X | ArgVars0], WaitingVars), % conservative
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, ConsId, ArgVars0,
							InstOfX, InstArgs),
			ModeInfo1, ModeInfo2
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		Det = erroneous,
		ModeOfX = (InstOfX - Inst),
		ModeOfY = (InstOfY - Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo2, ModeInfo4),
%		( bind_args(Inst, ArgVars0, ModeInfo4, ModeInfo5) ->
			ModeInfo = ModeInfo4,
%		;
%			error("bind_args failed")
%		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals = no_extra_goals
	),

	%
	% Optimize away construction of unused terms by
	% replacing the unification with `true'.  Optimize
	% away unifications which always fail by replacing
	% them with `fail'.
	%
	(
		Unification = construct(ConstructTarget, _, _, _),
		mode_info_var_is_live(ModeInfo, ConstructTarget, dead)
	->
		Goal = conj([])
	;
		Det = failure
	->
		% This optimisation is safe because the only way that
		% we can analyse a unification as having no solutions
		% is that the unification always fails.
		%
		% Unifying two preds is not erroneous as far as the
		% mode checker is concerned, but a mode _error_.
		map__init(Empty),
		Goal = disj([], Empty)
	;
		Functor = functor(ConsId, ArgVars),
		Unify = unify(X, Functor, Mode, Unification,
			UnifyContext),
		X = X0,
		%
		% modecheck_unification sometimes needs to introduce
		% new goals to handle complicated sub-unifications
		% in deconstructions. The only time this can happen
		% during unique mode analysis is if the instmap is 
		% unreachable, since inst_is_bound succeeds for not_reached.
		% (If it did in other cases, the code would be wrong since it
		% wouldn't have the correct determinism annotations.)
		%
		(
			HowToCheckGoal = check_unique_modes(_),
			ExtraGoals \= no_extra_goals,
			instmap__is_reachable(InstMap1)
		->
			error("unique_modes.m: re-modecheck of unification encountered complicated sub-unifies")
		;
			true
		),
		handle_extra_goals(Unify, ExtraGoals, GoalInfo0,
					[X0|ArgVars0], [X|ArgVars],
					InstMap1, ModeInfo, Goal)
	).

%-----------------------------------------------------------------------------%

	% The argument unifications in a construction or deconstruction
	% unification must be simple assignments, they cannot be
	% complicated unifications.  If they are, we split them out
	% into separate unifications by introducing fresh variables here.

:- pred split_complicated_subunifies(instmap, unification, list(var),
			unification, list(var), extra_goals,
			mode_info, mode_info).
:- mode split_complicated_subunifies(in, in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

split_complicated_subunifies(InstMapBefore, Unification0, ArgVars0,
				Unification, ArgVars, ExtraGoals) -->
	(
		{ Unification0 = deconstruct(X, ConsId, ArgVars0, ArgModes0,
			Det) }
	->
		(
			split_complicated_subunifies_2(ArgVars0, ArgModes0,
				ArgVars1, ArgModes, ExtraGoals1, InstMapBefore)
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
		{ ExtraGoals = no_extra_goals }
	).

:- pred split_complicated_subunifies_2(list(var), list(uni_mode),
			list(var), list(uni_mode), extra_goals,
			instmap, mode_info, mode_info).
:- mode split_complicated_subunifies_2(in, in, out, out, out, in,
			mode_info_di, mode_info_uo) is semidet.

split_complicated_subunifies_2([], [], [], [], no_extra_goals, _) --> [].
split_complicated_subunifies_2([Var0 | Vars0], [UniMode0 | UniModes0],
			Vars, UniModes, ExtraGoals,
			InstMapBefore, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_inst_table(ModeInfo0, InstTable),
	UniMode0 = (InitialInstX - InitialInstY -> FinalInstX - FinalInstY),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, Var0, VarType),
	(
		% YYY Check this!
		inst_has_representation(InitialInstX, InstMapBefore,
				InstTable, VarType, ModuleInfo),
		inst_has_representation(InitialInstY, InstMapBefore,
				InstTable, VarType, ModuleInfo)
	->
		% introduce a new variable `Var'
		mode_info_get_varset(ModeInfo0, VarSet0),
		mode_info_get_var_types(ModeInfo0, VarTypes0),
		varset__new_var(VarSet0, Var, VarSet),
		map__set(VarTypes0, Var, VarType, VarTypes),
		mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
		mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo2),

		% change the main unification to use `Var' instead of Var0
		UniMode = (InitialInstX - free(unique) -> 
				InitialInstX - InitialInstX),

		% Compute the instmap that results after the main unification.
		% We just need to set the inst of `Var'.
		mode_info_get_instmap(ModeInfo2, OrigInstMap),
		instmap__set(OrigInstMap, Var, InitialInstX, InstMapAfterMain),
		mode_info_set_instmap(InstMapAfterMain, ModeInfo2, ModeInfo3),

		% create code to do a unification between `Var' and `Var0'
		% ModeVar0 = (InitialInstY -> FinalInstY),
		% ModeVar = (InitialInstX -> FinalInstX),

		mode_info_get_module_info(ModeInfo3, ModuleInfo0),
		mode_info_get_inst_table(ModeInfo3, InstTable0),
		mode_info_get_instmap(ModeInfo3, InstMap3),
		(
			abstractly_unify_inst(dead, InitialInstX, InitialInstY,
				real_unify, InstTable0, ModuleInfo0, InstMap3,
				_Inst, Det1, InstTable1, ModuleInfo1, InstMap4)
		->
			mode_info_set_module_info(ModeInfo3, ModuleInfo1,
				ModeInfo4),
			mode_info_set_inst_table(InstTable1, ModeInfo4,
					ModeInfo5),
			mode_info_set_instmap(InstMap4, ModeInfo5, ModeInfo6),
			Det = Det1
		;
			ModeInfo6 = ModeInfo3,
			Det = semidet
			% XXX warning - it might be det in some cases.
			% should we report an error here?  should this
			% ever happen?
		),
		mode_info_get_mode_context(ModeInfo6, ModeContext),
		mode_info_get_instmap(ModeInfo6, InstMap6),
		mode_context_to_unify_context(ModeContext, ModeInfo6,
						UnifyContext),
		categorize_unify_var_var(InitialInstY, FinalInstY,
			InitialInstX, FinalInstX, live, dead, Var0, Var,
			InstMap3, InstMap6, Det, UnifyContext,
			VarTypes, ModeInfo6, NewUnifyGoal, ModeInfo7),
		unify_vars(NewUnifyGoal, NewUnifyGoalVars),

		% compute the goal_info nonlocal vars & instmap delta
		% for the newly created goal
		% N.B. This may overestimate the set of non-locals,
		% but that shouldn't cause any problems.
		set__list_to_set(NewUnifyGoalVars, NonLocals),
		( InitialInstY = FinalInstY ->
			InstMapDeltaAL0 = []
		;
			InstMapDeltaAL0 = [Var0 - FinalInstY]
		),
		InstMapDeltaAL = [Var - FinalInstX | InstMapDeltaAL0],
		instmap_delta_from_assoc_list(InstMapDeltaAL, InstMapDelta),
		goal_info_init(GoalInfo0),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo),

		% insert the new unification at
		% the start of the extra goals
		ExtraGoals0 = extra_goals([], after_goals(InstMapAfterMain,
					[NewUnifyGoal - GoalInfo])),

		% recursive call to handle the remaining variables...
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, UniModes1, ExtraGoals1, InstMapBefore,
				ModeInfo7, ModeInfo),
		Vars = [Var | Vars1],
		UniModes = [UniMode | UniModes1],
		append_extra_goals(ExtraGoals0, ExtraGoals1, ExtraGoals)
	;
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, UniModes1, ExtraGoals, InstMapBefore,
				ModeInfo0, ModeInfo),
		Vars = [Var0 | Vars1],
		UniModes = [UniMode0 | UniModes1]
	).

%-----------------------------------------------------------------------------%

% categorize_unify_var_var works out which category a unification
% between a variable and another variable expression is - whether it is
% an assignment, a simple test or a complicated unify.

categorize_unify_var_var(IX, FX, IY, FY, LiveX, LiveY, X, Y,
		InstMapBefore, InstMapAfter, Det, UnifyContext,
		VarTypes, ModeInfo0, Unify, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	(
		inst_is_free(IX, InstMapBefore, InstTable0, ModuleInfo0),
		inst_is_bound(FX, InstMapAfter, InstTable0, ModuleInfo0)
	->
		Unification = assign(X, Y),
		ModeInfo = ModeInfo0
	;
		inst_is_free(IY, InstMapBefore, InstTable0, ModuleInfo0),
		inst_is_bound(FY, InstMapAfter, InstTable0, ModuleInfo0)
	->
		Unification = assign(Y, X),
		ModeInfo = ModeInfo0
	;
		inst_is_free(IX, InstMapBefore, InstTable0, ModuleInfo0),
		inst_is_free(FX, InstMapAfter, InstTable0, ModuleInfo0),
		inst_is_free(IY, InstMapBefore, InstTable0, ModuleInfo0),
		inst_is_free(FY, InstMapAfter, InstTable0, ModuleInfo0)
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
			determinism_components(Det, CanFail, _),
			UniMode0 = ((IX - IY) -> (FX - FY)),
			Unification = complicated_unify(UniMode0, CanFail),
			mode_info_get_instmap(ModeInfo0, InstMap0),
			(
				type_is_higher_order(Type, PredOrFunc, _)
			->
				% We do not want to report this as an error
				% if it occurs in a compiler-generated
				% predicate - instead, we delay the error
				% until runtime so that it only occurs if
				% the compiler-generated predicate gets called.
				% not_reached is considered bound, so the 
				% error message would be spurious if the 
				% instmap is unreachable.
				mode_info_get_predid(ModeInfo0, PredId),
				module_info_pred_info(ModuleInfo0, PredId,
						PredInfo),
				( 
				    ( code_util__compiler_generated(PredInfo) 
				    ; instmap__is_unreachable(InstMap0)
				    )
				->
				    ModeInfo = ModeInfo0
				;
				    set__init(WaitingVars),
				    mode_info_error(WaitingVars,
			mode_error_unify_pred(X, error_at_var(Y), Type, PredOrFunc),
						ModeInfo0, ModeInfo)
				)
			;
				% Don't request a unification if it's a
				% X = X unification.
				\+ ( IX = alias(Key), IY = alias(Key) ),
				type_to_type_id(Type, TypeId, _)
			->
				% YYY Optimise UniMode0 in the case that there
				%     are no shared inst_keys
				UniMode0 = UniMode,
				mode_info_get_context(ModeInfo0, Context),
				unify_proc__request_unify(TypeId - UniMode, Det,
					Context, InstTable0, ModuleInfo0, ModuleInfo),
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
	% Optimize away unifications which always fail by replacing
	% them with `fail'.
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
		Det = failure
	->
		% This optimisation is safe because the only way that
		% we can analyse a unification as having no solutions
		% is that the unification always fails.
		%
		% Unifying two preds is not erroneous as far as the
		% mode checker is concerned, but a mode _error_.
		map__init(Empty),
		Unify = disj([], Empty)
	;
		% A unification of X with X should be optimised
		% away.
		IX = alias(K),
		IY = alias(K)
	->
		Unify = conj([])
	;
		ModeOfX = IX - FX,
		ModeOfY = IY - FY,
		Unify = unify(X, var(Y), ModeOfX - ModeOfY, Unification,
				UnifyContext)
	).

% categorize_unify_var_lambda works out which category a unification
% between a variable and a lambda expression is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_lambda(inst, inst, list(uni_mode), var,
		list(var), instmap, instmap, pred_or_func, argument_modes,
		unify_rhs, unification, mode_info,
		unify_rhs, unification, mode_info).
:- mode categorize_unify_var_lambda(in, in, in, in, in, in, in, in, in, in,
			in, mode_info_di, out, out, mode_info_uo) is det.

categorize_unify_var_lambda(IX, FX, ArgModes, X, ArgVars, InstMapBefore,
		InstMapAfter, PredOrFunc, LambdaModes,
		RHS0, Unification0, ModeInfo0,
		RHS, Unification, ModeInfo) :-
	% if we are re-doing mode analysis, preserve the existing cons_id
	list__length(ArgVars, Arity),
	( Unification0 = construct(_, ConsId0, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _) ->
		ConsId = ConsId1
	;
		% the real cons_id will be computed by polymorphism.m;
		% we just put in a dummy one for now
		ConsId = cons(unqualified("__LambdaGoal__"), Arity)
	),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_inst_table(ModeInfo0, InstTable),
	(
		inst_is_free(IX, InstMapBefore, InstTable, ModuleInfo),
		inst_is_bound(FX, InstMapAfter, InstTable, ModuleInfo)
	->
		( 
			% If pred_consts are present, lambda expansion
			% has already been done. Rerunning mode analysis
			% should not produce a lambda_goal which cannot
			% be directly converted back into a higher-order
			% predicate constant.
			% If the instmap is not reachable, the call
			% may have been handled as an implied mode,
			% since not_reached is considered to be bound. 
			% In this case the lambda_goal may not be 
			% converted back to a predicate constant, but
			% that doesn't matter since the code will be
			% pruned away later by simplify.m.
			ConsId = pred_const(PredId, ProcId),
			instmap__is_reachable(InstMapAfter)
		->
			( 
				RHS0 = lambda_goal(_, _, _, _, _, _, Goal),
				Goal = call(PredId, ProcId, _, _, _, _) - _
			->
				module_info_pred_info(ModuleInfo,
					PredId, PredInfo),
				pred_info_module(PredInfo, PredModule),
				pred_info_name(PredInfo, PredName),
				RHS = functor(
					cons(qualified(PredModule, PredName),
						Arity),
					ArgVars)	
			;
				error("categorize_unify_var_lambda - \
					reintroduced lambda goal")
			)
		;
			RHS = RHS0
		),
		Unification = construct(X, ConsId, ArgVars, ArgModes),
		ModeInfo = ModeInfo0
	;
		instmap__is_reachable(InstMapAfter)
	->
		% If it's a deconstruction, it is a mode error.
		% The error message would be incorrect in unreachable
		% code, since not_reached is considered bound.
		set__init(WaitingVars),
		mode_info_get_var_types(ModeInfo0, VarTypes0),
		map__lookup(VarTypes0, X, Type),
		mode_info_error(WaitingVars,
				mode_error_unify_pred(X,
					error_at_lambda(ArgVars, LambdaModes),
					Type, PredOrFunc),
				ModeInfo0, ModeInfo),
		% return any old garbage
		Unification = Unification0,
		RHS = RHS0
	;
		ModeInfo = ModeInfo0,
		Unification = Unification0,
		RHS = RHS0
	).

% categorize_unify_var_functor works out which category a unification
% between a variable and a functor is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_functor(inst, inst, assoc_list(inst, inst),
		assoc_list(inst, inst), var, cons_id, list(var), instmap,
		instmap, map(var, type), determinism, unification,
		mode_info, unification, mode_info).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, in, in, in, in,
		in, in, mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_functor(IX, FX, ModeOfXArgs, ArgModes0, X, NewConsId,
		ArgVars, InstMapBefore, InstMapAfter, VarTypes, Det,
		Unification0, ModeInfo0, Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	map__lookup(VarTypes, X, TypeOfX),
	% if we are re-doing mode analysis, preserve the existing cons_id
	( Unification0 = construct(_, ConsId0, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _) ->
		ConsId = ConsId1
	;
		ConsId = NewConsId
	),
	mode_util__inst_pairs_to_uni_modes(ModeOfXArgs, ArgModes0, ArgModes),
	(
		inst_is_free(IX, InstMapBefore, InstTable0, ModuleInfo),
		inst_is_bound(FX, InstMapAfter, InstTable0, ModuleInfo)
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

			/*********************
			mode_get_insts(ModuleInfo, ModeOfX,
					InitialInst0, FinalInst0),
			inst_expand(InstTable0, ModuleInfo, InitialInst0,
				InitialInst),
			inst_expand(InstTable0, ModuleInfo, FinalInst0,
				FinalInst),
			InitialInst = bound(_, [_]),
			FinalInst = bound(_, [_])
			*********************/
			determinism_components(Det, cannot_fail, _)
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
			mode_info_get_instmap(ModeInfo0, InstMap0),
			( 
				type_is_higher_order(TypeOfX, PredOrFunc, _),
				instmap__is_reachable(InstMap0)
			->
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

	% YYY This is probably deprecated.  However would it be more
	%     efficient in inst_key usage to use this instead of
	%     assigning inst_keys first?  Probably.  Must check this.
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

	% Given a unification goal, which must be something that can be
	% returned from categorize_unify_var_var (i.e. a unification
	% of two vars, a unification of a var and a functor,
	% an empty disjunction (fail), or a conjunction thereof),
	% return a list of all the variables in the goal.
unify_vars(Unify, UnifyVars) :-
	( Unify = unify(LHS, RHS, _, _, _), RHS = var(RHS_Var) ->
		UnifyVars = [LHS, RHS_Var]
	; Unify = unify(LHS, RHS, _, _, _), RHS = functor(_, RHS_ArgVars) ->
		UnifyVars = [LHS | RHS_ArgVars]
	; Unify = conj(List) ->
		unify_conj_vars(List, UnifyVars)
	; Unify = disj([], _StoreMap) ->
		UnifyVars = []
	;
		error("modecheck_unify: unify_vars")
	).

:- pred unify_conj_vars(list(hlds_goal), list(var)).
:- mode unify_conj_vars(in, out) is det.

unify_conj_vars([], []).
unify_conj_vars([Goal | Goals], Vars) :-
	quantification__goal_vars(Goal, Vars1Set),
	set__to_sorted_list(Vars1Set, Vars1),
	unify_conj_vars(Goals, Vars2),
	list__append(Vars1, Vars2, Vars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
