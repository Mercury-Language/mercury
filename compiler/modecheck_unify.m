%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
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

:- module check_hlds__modecheck_unify.
:- interface.

:- import_module hlds__hlds_goal, parse_tree__prog_data, check_hlds__mode_info.

	% Modecheck a unification
:- pred modecheck_unification(prog_var, unify_rhs, unification, unify_context,
			hlds_goal_info, hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_unification(in, in, in, in, in, out,
			mode_info_di, mode_info_uo) is det.

	% Create a unification between the two given variables.
	% The goal's mode and determinism information is not filled in.
:- pred modecheck_unify__create_var_var_unification(prog_var, prog_var, type,
		mode_info, hlds_goal).
:- mode modecheck_unify__create_var_var_unification(in, in, in,
		mode_info_ui, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend__llds, parse_tree__prog_util.
:- import_module check_hlds__type_util, parse_tree__module_qual, hlds__instmap.
:- import_module hlds__hlds_module, hlds__hlds_goal, hlds__hlds_pred.
:- import_module hlds__hlds_data, hlds__hlds_out.
:- import_module check_hlds__mode_debug, check_hlds__mode_util.
:- import_module check_hlds__mode_info, check_hlds__modes.
:- import_module check_hlds__mode_errors.
:- import_module check_hlds__inst_match, check_hlds__inst_util.
:- import_module check_hlds__unify_proc, ll_backend__code_util.
:- import_module check_hlds__unique_modes.
:- import_module check_hlds__typecheck, check_hlds__modecheck_call.
:- import_module (parse_tree__inst), hlds__quantification, hlds__make_hlds.
:- import_module check_hlds__polymorphism.

:- import_module bool, list, map, std_util, int, set, require.
:- import_module string, assoc_list.
:- import_module term, varset.

%-----------------------------------------------------------------------------%

modecheck_unification(X, var(Y), Unification0, UnifyContext, _GoalInfo,
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
			Det, UnifyContext, VarTypes, Unification0, ModeInfo3,
			Unify, ModeInfo)
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

modecheck_unification(X0, functor(ConsId0, IsExistConstruction, ArgVars0),
			Unification0, UnifyContext, GoalInfo0, Goal,
			ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, X0, TypeOfX),

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
	% Normally this is done by polymorphism__process_unify_functor,
	% but if we're re-modechecking goals after lambda.m has been run
	% (e.g. for deforestation), then we may need to do it again here.
	% Note that any changes to this code here will probably need to be
	% duplicated there too.
	%
	(
		% check if variable has a higher-order type
		type_is_higher_order(TypeOfX, _, EvalMethod, PredArgTypes),
		ConsId0 = pred_const(PredId, ProcId, _)
	->
		%
		% convert the pred term to a lambda expression
		%
		mode_info_get_varset(ModeInfo0, VarSet0),
		mode_info_get_context(ModeInfo0, Context),
		convert_pred_to_lambda_goal(EvalMethod,
			X0, PredId, ProcId, ArgVars0, PredArgTypes,
			UnifyContext, GoalInfo0, Context,
			ModuleInfo0, VarSet0, VarTypes0,
			Functor0, VarSet, VarTypes),
		mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
		mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo2),
		%
		% modecheck this unification in its new form
		%
		modecheck_unification(X0, Functor0, Unification0, UnifyContext,
				GoalInfo0, Goal, ModeInfo2, ModeInfo)
	;
		%
		% It's not a higher-order pred unification - just
		% call modecheck_unify_functor to do the ordinary thing.
		%
		modecheck_unify_functor(X0, TypeOfX, ConsId0,
			IsExistConstruction, ArgVars0, Unification0,
			UnifyContext, GoalInfo0, Goal, ModeInfo0, ModeInfo)
	).

modecheck_unification(X, 
		lambda_goal(PredOrFunc, EvalMethod, _, ArgVars,
			Vars, Modes0, Det, Goal0),
		Unification0, UnifyContext, _GoalInfo, 
		unify(X, RHS, Mode, Unification, UnifyContext),
		ModeInfo0, ModeInfo) :-
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
	mode_info_get_how_to_check(ModeInfo0, HowToCheckGoal),

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
	% variables, since it could get called more than once, or
	% from inside a negation)
	Goal0 = _ - GoalInfo0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	instmap__lookup_vars(NonLocalsList, InstMap1, NonLocalInsts),
	mode_info_get_module_info(ModeInfo2, ModuleInfo2),
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

		inst_list_is_ground_or_any(NonLocalInsts, ModuleInfo2)
	->
		make_shared_inst_list(NonLocalInsts, ModuleInfo2,
			SharedNonLocalInsts, ModuleInfo3),
		instmap__set_vars(InstMap1, NonLocalsList, SharedNonLocalInsts,
			InstMap2),
		mode_info_set_module_info(ModeInfo2, ModuleInfo3, ModeInfo3),
		mode_info_set_instmap(InstMap2, ModeInfo3, ModeInfo4),

		mode_info_lock_vars(lambda(PredOrFunc), NonLocals,
				ModeInfo4, ModeInfo5),

		mode_checkpoint(enter, "lambda goal", ModeInfo5, ModeInfo6),
		% if we're being called from unique_modes.m, then we need to 
		% call unique_modes__check_goal rather than modecheck_goal.
		(
			HowToCheckGoal = check_unique_modes
		->
			unique_modes__check_goal(Goal0, Goal, ModeInfo6,
				ModeInfo7)
		;
			modecheck_goal(Goal0, Goal, ModeInfo6, ModeInfo7)
		),
		mode_list_get_final_insts(Modes, ModuleInfo0, FinalInsts),
		modecheck_final_insts(Vars, FinalInsts, ModeInfo7, ModeInfo8),
		mode_checkpoint(exit, "lambda goal", ModeInfo8, ModeInfo9),

		mode_info_remove_live_vars(LiveVars, ModeInfo9, ModeInfo10),
		mode_info_unlock_vars(lambda(PredOrFunc), NonLocals,
			ModeInfo10, ModeInfo11),

		%
		% Ensure that the non-local vars are shared OUTSIDE the
		% lambda unification as well as inside.
		%

		instmap__set_vars(InstMap0, NonLocalsList, SharedNonLocalInsts,
			InstMap11),
		mode_info_set_instmap(InstMap11, ModeInfo11, ModeInfo12),

		%
		% Now modecheck the unification of X with the lambda-expression.
		%

		RHS0 = lambda_goal(PredOrFunc, EvalMethod, modes_are_ok,
				ArgVars, Vars, Modes, Det, Goal),
		modecheck_unify_lambda(X, PredOrFunc, ArgVars, Modes,
				Det, RHS0, Unification0, Mode,
				RHS, Unification, ModeInfo12, ModeInfo)
	;
		list__filter(lambda([Var :: in] is semidet,
			( instmap__lookup_var(InstMap1, Var, Inst),
			  \+ inst_is_ground(ModuleInfo2, Inst)
			)),
			NonLocalsList, NonGroundNonLocals),
		( NonGroundNonLocals = [BadVar | _] ->
			instmap__lookup_var(InstMap1, BadVar, BadInst),
			set__singleton_set(WaitingVars, BadVar),
			mode_info_error(WaitingVars,
				mode_error_non_local_lambda_var(BadVar,
						BadInst),
				ModeInfo2, ModeInfo)
		;
			error("modecheck_unification(lambda): very strange var")
		),
			% return any old garbage
		RHS = lambda_goal(PredOrFunc, EvalMethod, modes_are_ok,
				ArgVars, Vars, Modes0, Det, Goal0),
		Mode = (free -> free) - (free -> free),
		Unification = Unification0
	).

:- pred modecheck_unify_lambda(prog_var, pred_or_func, list(prog_var),
		list(mode), determinism, unify_rhs, unification,
		pair(mode), unify_rhs, unification, mode_info, mode_info).
:- mode modecheck_unify_lambda(in, in, in, in, in, in, in,
		out, out, out, mode_info_di, mode_info_uo) is det.

modecheck_unify_lambda(X, PredOrFunc, ArgVars, LambdaModes, 
		LambdaDet, RHS0, Unification0, Mode, RHS, Unification, 
		ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, X, InstOfX),
	InstOfY = ground(unique, higher_order(LambdaPredInfo)),
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
				RHS0, Unification0, ModeInfo1,
				RHS, Unification, ModeInfo2),
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
		Unification = Unification0,
		RHS = RHS0
	).

:- pred modecheck_unify_functor(prog_var, (type), cons_id,
		is_existential_construction, list(prog_var),
		unification, unify_context, hlds_goal_info, hlds_goal_expr,
		mode_info, mode_info).
:- mode modecheck_unify_functor(in, in, in, in, in, in, in, in,
			out, mode_info_di, mode_info_uo) is det.

modecheck_unify_functor(X0, TypeOfX, ConsId0, IsExistConstruction, ArgVars0,
			Unification0, UnifyContext, GoalInfo0, Goal,
			ModeInfo0, FinalModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_how_to_check(ModeInfo0, HowToCheckGoal),

	%
	% Fully module qualify all cons_ids
	% (except for builtins such as ints and characters).
	%
	(
		ConsId0 = cons(Name0, OrigArity),
		type_to_ctor_and_args(TypeOfX, TypeCtor, _),
		TypeCtor = qualified(TypeModule, _) - _
	->
		unqualify_name(Name0, UnqualName),
		Name = qualified(TypeModule, UnqualName),
		ConsId = cons(Name, OrigArity),
		%
		% Fix up the cons_id arity for type(class)_info constructions.
		% The cons_id for type(class)_info constructions always has
		% arity 1, to match the arity in the declaration in
		% library/private_builtin.m,
		% but for the inst we need the arity of the cons_id
		% to match the number of arguments.
		%
		(
			mercury_private_builtin_module(TypeModule),
			( UnqualName = "typeclass_info"
			; UnqualName = "type_info"
			)
		->
			list__length(ArgVars0, InstArity),
			InstConsId = cons(Name, InstArity)
		;
			InstConsId = ConsId
		)
	;
		ConsId = ConsId0,
		InstConsId = ConsId
	),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, X0, InstOfX0),
	instmap__lookup_vars(ArgVars0, InstMap0, InstArgs),
	mode_info_var_list_is_live(ArgVars0, ModeInfo0, LiveArgs),
	InstOfY = bound(unique, [functor(InstConsId, InstArgs)]),
	(
		% If the unification was originally of the form
		% X = 'new f'(Y) it must be classified as a
		% construction. If it were classified as a
		% deconstruction, the argument unifications would
		% be ill-typed.
		IsExistConstruction = yes,
		\+ inst_is_free(ModuleInfo0, InstOfX0)
	->
		% To make sure the unification is classified as
		% a construction, if X is already bound, we must
		% add a unification with an extra variable:
		%	Z = 'new f'(Y),
		%	X = Z.
		
		InstOfX = free,
		LiveX = live,
		make_complicated_sub_unify(X0, X, ExtraGoals0,
			ModeInfo0, ModeInfo1)
	;
		InstOfX = InstOfX0,
		X = X0,
		mode_info_var_is_live(ModeInfo0, X, LiveX),
		ExtraGoals0 = no_extra_goals,
		ModeInfo1 = ModeInfo0
	),
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
			mode_error_unify_var_functor(X, InstConsId, ArgVars0,
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
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo2, ModeInfo3),
		( bind_args(Inst, ArgVars0, ModeInfo3, ModeInfo4) ->
			ModeInfo = ModeInfo4
		;
			error("bind_args failed")
		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals1 = no_extra_goals
	;
		abstractly_unify_inst_functor(LiveX, InstOfX, InstConsId,
			InstArgs, LiveArgs, real_unify, ModuleInfo0,
			UnifyInst, Det1, ModuleInfo1)
	->
		Inst = UnifyInst,
		Det = Det1,
		mode_info_set_module_info(ModeInfo1, ModuleInfo1, ModeInfo2),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		( get_mode_of_args(Inst, InstArgs, ModeArgs0) ->
			ModeArgs = ModeArgs0
		;
			error("get_mode_of_args failed")
		),
		(
			inst_expand_and_remove_constrained_inst_vars(
				ModuleInfo1, InstOfX, InstOfX1),
			list__length(ArgVars0, Arity),
			get_arg_insts(InstOfX1, InstConsId, Arity, InstOfXArgs),
			get_mode_of_args(Inst, InstOfXArgs, ModeOfXArgs0)
		->
			ModeOfXArgs = ModeOfXArgs0
		;
			error("get_(inst/mode)_of_args failed")
		),
		mode_info_get_var_types(ModeInfo2, VarTypes),
		categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ModeArgs,
				X, ConsId, ArgVars0, VarTypes, UnifyContext,
				Unification0, ModeInfo2,
				Unification1, ModeInfo3),
		split_complicated_subunifies(Unification1, ArgVars0,
				Unification, ArgVars, ExtraGoals1,
				ModeInfo3, ModeInfo4),
		modecheck_set_var_inst(X, Inst, ModeInfo4, ModeInfo5),
		( bind_args(Inst, ArgVars, ModeInfo5, ModeInfo6) ->
			ModeInfo = ModeInfo6
		;
			error("bind_args failed")
		)
	;
		set__list_to_set([X | ArgVars0], WaitingVars), % conservative
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, InstConsId, ArgVars0,
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
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo2, ModeInfo3),
		( bind_args(Inst, ArgVars0, ModeInfo3, ModeInfo4) ->
			ModeInfo = ModeInfo4
		;
			error("bind_args failed")
		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals1 = no_extra_goals
	),

	%
	% Optimize away construction of unused terms by
	% replacing the unification with `true'.  Optimize
	% away unifications which always fail by replacing
	% them with `fail'.
	%
	(
		Unification = construct(_, _, _, _, _, _, _),
		LiveX = dead
	->
		Goal = conj([]),
		FinalModeInfo = ModeInfo
	;
		Det = failure
	->
		% This optimisation is safe because the only way that
		% we can analyse a unification as having no solutions
		% is that the unification always fails.
		%,
		% Unifying two preds is not erroneous as far as the
		% mode checker is concerned, but a mode _error_.
		Goal = disj([]),
		FinalModeInfo = ModeInfo
	;
		Functor = functor(ConsId, IsExistConstruction, ArgVars),
		Unify = unify(X, Functor, Mode, Unification,
			UnifyContext),
		%
		% modecheck_unification sometimes needs to introduce
		% new goals to handle complicated sub-unifications
		% in deconstructions. The only time this can happen
		% during unique mode analysis is if the instmap is 
		% unreachable, since inst_is_bound succeeds for not_reached.
		% (If it did in other cases, the code would be wrong since it
		% wouldn't have the correct determinism annotations.)
		%
		append_extra_goals(ExtraGoals0, ExtraGoals1, ExtraGoals),
		(
			HowToCheckGoal = check_unique_modes,
			ExtraGoals \= no_extra_goals,
			instmap__is_reachable(InstMap0)
		->
			error("unique_modes.m: re-modecheck of unification encountered complicated sub-unifies")
		;
			true
		),
		handle_extra_goals(Unify, ExtraGoals, GoalInfo0,
			[X0|ArgVars0], [X|ArgVars],
			InstMap0, Goal, ModeInfo, FinalModeInfo)
	).

%-----------------------------------------------------------------------------%

	% The argument unifications in a construction or deconstruction
	% unification must be simple assignments, they cannot be
	% complicated unifications.  If they are, we split them out
	% into separate unifications by introducing fresh variables here.

:- pred split_complicated_subunifies(unification, list(prog_var),
			unification, list(prog_var), extra_goals,
			mode_info, mode_info).
:- mode split_complicated_subunifies(in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

split_complicated_subunifies(Unification0, ArgVars0,
				Unification, ArgVars, ExtraGoals) -->
	(
		{ Unification0 = deconstruct(X, ConsId, ArgVars0, ArgModes0,
			Det, CanCGC) }
	->
		(
			split_complicated_subunifies_2(ArgVars0, ArgModes0,
				ArgVars1, ExtraGoals1)
		->
			{ ExtraGoals = ExtraGoals1 },
			{ ArgVars = ArgVars1 },
			{ Unification = deconstruct(X, ConsId, ArgVars,
					ArgModes0, Det, CanCGC) }
		;
			{ error("split_complicated_subunifies_2 failed") }
		)
	;
		{ Unification = Unification0 },
		{ ArgVars = ArgVars0 },
		{ ExtraGoals = no_extra_goals }
	).

:- pred split_complicated_subunifies_2(list(prog_var), list(uni_mode),
		list(prog_var), extra_goals, mode_info, mode_info).
:- mode split_complicated_subunifies_2(in, in, out, out,
			mode_info_di, mode_info_uo) is semidet.

split_complicated_subunifies_2([], [], [], no_extra_goals) --> [].
split_complicated_subunifies_2([Var0 | Vars0], [UniMode0 | UniModes0],
			Vars, ExtraGoals, ModeInfo0, ModeInfo) :-
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
		make_complicated_sub_unify(Var0, Var, ExtraGoals0,
				ModeInfo0, ModeInfo1),

		% recursive call to handle the remaining variables...
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, ExtraGoals1, ModeInfo1, ModeInfo),
		Vars = [Var | Vars1],
		append_extra_goals(ExtraGoals0, ExtraGoals1, ExtraGoals)
	;
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, ExtraGoals, ModeInfo0, ModeInfo),
		Vars = [Var0 | Vars1]
	).

:- pred make_complicated_sub_unify(prog_var::in, prog_var::out,
		extra_goals::out, mode_info::mode_info_di,
		mode_info::mode_info_uo) is det.

make_complicated_sub_unify(Var0, Var, ExtraGoals0, ModeInfo0, ModeInfo) :-
	% introduce a new variable `Var'
	mode_info_get_varset(ModeInfo0, VarSet0),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	varset__new_var(VarSet0, Var, VarSet),
	map__lookup(VarTypes0, Var0, VarType),
	map__set(VarTypes0, Var, VarType, VarTypes),
	mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
	mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo),

	modecheck_unify__create_var_var_unification(Var0, Var,
		VarType, ModeInfo, ExtraGoal),

	% insert the new unification at
	% the start of the extra goals
	ExtraGoals0 = extra_goals([], [ExtraGoal]).

modecheck_unify__create_var_var_unification(Var0, Var, Type, ModeInfo,
		Goal - GoalInfo) :-
	mode_info_get_context(ModeInfo, Context),
	mode_info_get_mode_context(ModeInfo, ModeContext),
	mode_context_to_unify_context(ModeContext, ModeInfo, UnifyContext),
	UnifyContext = unify_context(MainContext, SubContexts),
	
	create_atomic_unification(Var0, var(Var), Context,
		MainContext, SubContexts, Goal0 - GoalInfo0),

	%
	% compute the goal_info nonlocal vars for the newly created goal
	% (excluding the type_info vars -- they are added below).
	% N.B. This may overestimate the set of non-locals,
	% but that shouldn't cause any problems.
	%
	set__list_to_set([Var0, Var], NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
	goal_info_set_context(GoalInfo1, Context, GoalInfo2),

	%
	% Look up the map(tvar, type_info_locn) in the proc_info,
	% since it is needed by polymorphism__unification_typeinfos
	%
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_predid(ModeInfo, PredId),
	mode_info_get_procid(ModeInfo, ProcId),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_PredInfo, ProcInfo),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoVarMap),

	%
	% Call polymorphism__unification_typeinfos to add the appropriate
	% type-info and type-class-info variables to the nonlocals
	% and to the unification.
	%
	(
		Goal0 = unify(X, Y, Mode, Unification0, FinalUnifyContext)
	->
		polymorphism__unification_typeinfos(Type, TypeInfoVarMap,
			Unification0, GoalInfo2, Unification, GoalInfo),
		Goal = unify(X, Y, Mode, Unification, FinalUnifyContext)
	;
		error("modecheck_unify__create_var_var_unification")
	).
				        
%-----------------------------------------------------------------------------%

	% Work out what kind of unification a var-var unification is.
:- pred categorize_unify_var_var(mode, mode, is_live, is_live, prog_var,
		prog_var, determinism, unify_context, map(prog_var, type),
		unification, mode_info, hlds_goal_expr, mode_info).
:- mode categorize_unify_var_var(in, in, in, in, in, in, in, in, in, in,
			mode_info_di, out, mode_info_uo) is det.

% categorize_unify_var_var works out which category a unification
% between a variable and another variable expression is - whether it is
% an assignment, a simple test or a complicated unify.

categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y, Det,
		UnifyContext, VarTypes, Unification0, ModeInfo0,
		Unify, ModeInfo) :-
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
		%
		% Check for unreachable unifications
		%
		( mode_get_insts(ModuleInfo0, ModeOfX, not_reached, _)
		; mode_get_insts(ModuleInfo0, ModeOfY, not_reached, _)
		)
	->
		%
		% For these, we can generate any old junk here --
		% we just need to avoid calling modecheck_complicated_unify,
		% since that might abort.
		%
		Unification = simple_test(X, Y),
		ModeInfo = ModeInfo0
	;
		map__lookup(VarTypes, X, Type),
		(
			type_is_atomic(Type, ModuleInfo0)
		->
			Unification = simple_test(X, Y),
			ModeInfo = ModeInfo0
		;
			modecheck_complicated_unify(X, Y,
				Type, ModeOfX, ModeOfY, Det, UnifyContext,
				Unification0, ModeInfo0,
				Unification, ModeInfo)
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
		Unify = disj([])
	;
		Unify = unify(X, var(Y), ModeOfX - ModeOfY, Unification,
				UnifyContext)
	).

%
% modecheck_complicated_unify does some extra checks that are needed
% for mode-checking complicated unifications.
%

:- pred modecheck_complicated_unify(prog_var, prog_var,
		type, mode, mode, determinism, unify_context,
		unification, mode_info, unification, mode_info).
:- mode modecheck_complicated_unify(in, in, in, in, in, in, in,
		in, mode_info_di, out, mode_info_uo) is det.

modecheck_complicated_unify(X, Y, Type, ModeOfX, ModeOfY, Det, UnifyContext,
		Unification0, ModeInfo0, Unification, ModeInfo) :-
	%
	% Build up the unification
	%
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_get_insts(ModuleInfo0, ModeOfX, InitialInstX, FinalInstX),
	mode_get_insts(ModuleInfo0, ModeOfY, InitialInstY, FinalInstY),
	UniMode = ((InitialInstX - InitialInstY) -> (FinalInstX - FinalInstY)),
	determinism_components(Det, CanFail, _),
	( Unification0 = complicated_unify(_, _, UnifyTypeInfoVars0) ->
		UnifyTypeInfoVars = UnifyTypeInfoVars0
	;
		error("modecheck_complicated_unify")
	),
	Unification = complicated_unify(UniMode, CanFail, UnifyTypeInfoVars),

	%
	% check that all the type_info or type_class_info variables used
	% by the polymorphic unification are ground.
	%
	( UnifyTypeInfoVars = [] ->
		% optimize common case
		ModeInfo3 = ModeInfo0
	;
		list__length(UnifyTypeInfoVars, NumTypeInfoVars),
		list__duplicate(NumTypeInfoVars, ground(shared, none),
			ExpectedInsts),
		mode_info_set_call_context(unify(UnifyContext),
			ModeInfo0, ModeInfo1),
		NeedExactMatch = no,
		InitialArgNum = 0,
		modecheck_var_has_inst_list(UnifyTypeInfoVars, ExpectedInsts,
			NeedExactMatch, InitialArgNum, _InstVarSub,
			ModeInfo1, ModeInfo2),
			% We can ignore _InstVarSub since type_info variables
			% should not have variable insts.
		mode_info_unset_call_context(ModeInfo2, ModeInfo3)
	),

	mode_info_get_module_info(ModeInfo3, ModuleInfo3),

	(
		mode_info_get_errors(ModeInfo3, Errors),
		Errors \= []
	->
		ModeInfo = ModeInfo3
	;
		%
		% Check that we're not trying to do a polymorphic unification
		% in a mode other than (in, in).
		% [Actually we also allow `any' insts, since the (in, in)
		% mode of unification for types which have `any' insts must
		% also be able to handle (in(any), in(any)) unifications.]
		%
		Type = term__variable(_),
		\+ inst_is_ground_or_any(ModuleInfo3, InitialInstX)
	->
		set__singleton_set(WaitingVars, X),
		mode_info_error(WaitingVars,
			mode_error_poly_unify(X, InitialInstX),
			ModeInfo3, ModeInfo)
	;
		Type = term__variable(_),
		\+ inst_is_ground_or_any(ModuleInfo3, InitialInstY)
	->
		set__singleton_set(WaitingVars, Y),
		mode_info_error(WaitingVars,
			mode_error_poly_unify(Y, InitialInstY),
			ModeInfo3, ModeInfo)
	;

		%
		% check that we're not trying to do a higher-order unification
		%
		type_is_higher_order(Type, PredOrFunc, _, _)
	->
		% We do not want to report this as an error
		% if it occurs in a compiler-generated
		% predicate - instead, we delay the error
		% until runtime so that it only occurs if
		% the compiler-generated predicate gets called.
		% not_reached is considered bound, so the 
		% error message would be spurious if the 
		% instmap is unreachable.
		mode_info_get_predid(ModeInfo3, PredId),
		module_info_pred_info(ModuleInfo3, PredId,
				PredInfo),
		mode_info_get_instmap(ModeInfo3, InstMap0),
		( 
			( code_util__compiler_generated(PredInfo) 
			; instmap__is_unreachable(InstMap0)
			)
		->
			ModeInfo = ModeInfo3
		;
			set__init(WaitingVars),
			mode_info_error(WaitingVars,
				mode_error_unify_pred(X, error_at_var(Y),
						Type, PredOrFunc),
				ModeInfo3, ModeInfo)
		)
	;
		%
		% Ensure that we will generate code for the unification
		% procedure that will be used to implement this complicated
		% unification.
		%
		type_to_ctor_and_args(Type, TypeCtor, _)
	->
		mode_info_get_context(ModeInfo3, Context),
		mode_info_get_instvarset(ModeInfo3, InstVarSet),
		unify_proc__request_unify(TypeCtor - UniMode, InstVarSet,
			Det, Context, ModuleInfo3, ModuleInfo),
		mode_info_set_module_info(ModeInfo3, ModuleInfo,
			ModeInfo)
	;
		ModeInfo = ModeInfo3
	).
		

% categorize_unify_var_lambda works out which category a unification
% between a variable and a lambda expression is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_lambda(mode, list(mode),
		prog_var, list(prog_var), pred_or_func, unify_rhs, unification, 
		mode_info, unify_rhs, unification, mode_info).
:- mode categorize_unify_var_lambda(in, in, in, in, in, in,
			in, mode_info_di, out, out, mode_info_uo) is det.

categorize_unify_var_lambda(ModeOfX, ArgModes0, X, ArgVars,
		PredOrFunc, RHS0, Unification0, ModeInfo0, RHS, 
		Unification, ModeInfo) :-
	% if we are re-doing mode analysis, preserve the existing cons_id
	list__length(ArgVars, Arity),
	( Unification0 = construct(_, ConsId0, _, _, _, _, AditiInfo0) ->
		AditiInfo = AditiInfo0,
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _, _) ->
		AditiInfo = no,
		ConsId = ConsId1
	;
		% the real cons_id will be computed by lambda.m;
		% we just put in a dummy one for now
		AditiInfo = no,
		ConsId = cons(unqualified("__LambdaGoal__"), Arity)
	),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_util__modes_to_uni_modes(ArgModes0, ArgModes0,
						ModuleInfo, ArgModes),
	mode_info_get_instmap(ModeInfo0, InstMap),
	(
		mode_is_output(ModuleInfo, ModeOfX)
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
			ConsId = pred_const(PredId, ProcId, EvalMethod),
			instmap__is_reachable(InstMap)
		->
			( 
				RHS0 = lambda_goal(_, EvalMethod, _,
					_, _, _, _, Goal),
				Goal = call(PredId, ProcId, _, _, _, _) - _
			->
				module_info_pred_info(ModuleInfo,
					PredId, PredInfo),
				pred_info_module(PredInfo, PredModule),
				pred_info_name(PredInfo, PredName),
				RHS = functor(
					cons(qualified(PredModule, PredName),
						Arity),
					no, ArgVars)	
			;
				error("categorize_unify_var_lambda - \
					reintroduced lambda goal")
			)
		;
			RHS = RHS0
		),
		Unification = construct(X, ConsId, ArgVars, ArgModes,
			construct_dynamically, cell_is_unique, AditiInfo),
		ModeInfo = ModeInfo0
	;
		instmap__is_reachable(InstMap)
	->
		% If it's a deconstruction, it is a mode error.
		% The error message would be incorrect in unreachable
		% code, since not_reached is considered bound.
		set__init(WaitingVars),
		mode_info_get_var_types(ModeInfo0, VarTypes0),
		map__lookup(VarTypes0, X, Type),
		mode_info_error(WaitingVars,
				mode_error_unify_pred(X,
					error_at_lambda(ArgVars, ArgModes0),
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

:- pred categorize_unify_var_functor(mode, list(mode), list(mode), prog_var,
		cons_id, list(prog_var), map(prog_var, type), unify_context,
		unification, mode_info, unification, mode_info).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, in, in, in,
			mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ArgModes0,
		X, NewConsId, ArgVars, VarTypes, UnifyContext,
		Unification0, ModeInfo0, Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	map__lookup(VarTypes, X, TypeOfX),
	% if we are re-doing mode analysis, preserve the existing cons_id
	( Unification0 = construct(_, ConsId0, _, _, _, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _, _) ->
		ConsId = ConsId1
	;
		ConsId = NewConsId
	),
	mode_util__modes_to_uni_modes(ModeOfXArgs, ArgModes0,
						ModuleInfo, ArgModes),
	(
		mode_is_output(ModuleInfo, ModeOfX)
	->
		% It's a construction.
		RLExprnId = no,
		Unification = construct(X, ConsId, ArgVars, ArgModes,
			construct_dynamically, cell_is_unique, RLExprnId),

		% For existentially quantified data types,
		% check that any type_info or type_class_info variables in the
		% construction are ground.
		check_type_info_args_are_ground(ArgVars, VarTypes,
			UnifyContext, ModeInfo0, ModeInfo)
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
			mode_info_get_instmap(ModeInfo0, InstMap0),
			( 
				type_is_higher_order(TypeOfX, PredOrFunc,
					_, _),
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
		Unification = deconstruct(X, ConsId, ArgVars,
				ArgModes, CanFail, no)
	).

	% Check that any type_info or type_class_info variables
	% in the argument list are ground.
:- pred check_type_info_args_are_ground(list(prog_var), map(prog_var, type),
		unify_context, mode_info, mode_info).
:- mode check_type_info_args_are_ground(in, in, in,
		mode_info_di, mode_info_uo) is det.

check_type_info_args_are_ground([], _VarTypes, _UnifyContext) --> [].
check_type_info_args_are_ground([ArgVar | ArgVars], VarTypes, UnifyContext)
		-->
	( 
		{ map__lookup(VarTypes, ArgVar, ArgType) },
		{ is_introduced_type_info_type(ArgType) }
	->
		mode_info_set_call_context(unify(UnifyContext)),
		{ NeedExactMatch = no },
		{ InitialArgNum = 0 },
		modecheck_var_has_inst_list([ArgVar], [ground(shared, none)],
			NeedExactMatch, InitialArgNum, _InstVarSub),
		check_type_info_args_are_ground(ArgVars, VarTypes,
			UnifyContext),
		mode_info_unset_call_context
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(prog_var), mode_info, mode_info).
:- mode bind_args(in, in, mode_info_di, mode_info_uo) is semidet.

bind_args(not_reached, _) -->
	{ instmap__init_unreachable(InstMap) },
	mode_info_set_instmap(InstMap).
bind_args(ground(Uniq, none), Args) -->
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
bind_args(constrained_inst_vars(_, Inst), Args) -->
	bind_args(Inst, Args).

:- pred bind_args_2(list(prog_var), list(inst), mode_info, mode_info).
:- mode bind_args_2(in, in, mode_info_di, mode_info_uo) is semidet.

bind_args_2([], []) --> [].
bind_args_2([Arg | Args], [Inst | Insts]) -->
	modecheck_set_var_inst(Arg, Inst),
	bind_args_2(Args, Insts).

:- pred ground_args(uniqueness, list(prog_var), mode_info, mode_info).
:- mode ground_args(in, in, mode_info_di, mode_info_uo) is det.

ground_args(_Uniq, []) --> [].
ground_args(Uniq, [Arg | Args]) -->
	modecheck_set_var_inst(Arg, ground(Uniq, none)),
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
get_mode_of_args(ground(Uniq, none), ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, ground(Uniq, none), ArgModes).
get_mode_of_args(bound(_Uniq, List), ArgInstsA, ArgModes) :-
	( List = [] ->
		% the code is unreachable
		mode_set_args(ArgInstsA, not_reached, ArgModes)
	;
		List = [functor(_Name, ArgInstsB)],
		get_mode_of_args_2(ArgInstsA, ArgInstsB, ArgModes)
	).
get_mode_of_args(constrained_inst_vars(_, Inst), ArgInsts, ArgModes) :-
	get_mode_of_args(Inst, ArgInsts, ArgModes).

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
%-----------------------------------------------------------------------------%
