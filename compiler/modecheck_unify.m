%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
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

:- module check_hlds.modecheck_unify.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module parse_tree.prog_data.

    % Modecheck a unification.
    %
:- pred modecheck_unification(prog_var::in, unify_rhs::in, unification::in,
    unify_context::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

    % Create a unification between the two given variables.
    % The goal's mode and determinism information are not filled in.
    %
:- pred create_var_var_unification(prog_var::in, prog_var::in,
    mer_type::in, mode_info::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_debug.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_info.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_goal.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.modes.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module check_hlds.unify_proc.
:- import_module check_hlds.unique_modes.
:- import_module hlds.const_struct.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

modecheck_unification(LHSVar, RHS, Unification0, UnifyContext, UnifyGoalInfo0,
        Goal, !ModeInfo) :-
    % If a unification occurs in a negated context with an inst "any" argument
    % then it has an explicit `impure' annotation.
    %
    % With lambdas, the lambda itself has a higher-order any inst if it
    % includes any inst "any" nonlocals. The value of the lambda expression
    % does not become fixed until all of the nonlocals become fixed.
    % Executing such a lambda may constrain nonlocal solver variables,
    % which in turn constrains the higher-order value itself. Effectively,
    % call/N constrains the predicate value to be "some predicate that is
    % true for the given arguments", and apply/N constrains the function
    % value to be "some function that returns the given value for the given
    % arguments".
    %
    % But we also allow a ground higher-order inst to be used with non-ground
    % locals, provided the type of the higher-order value is impure.
    %
    (
        RHS = rhs_var(RHSVar),
        modecheck_unification_var(LHSVar, RHSVar,
            Unification0, UnifyContext, UnifyGoalInfo0, Goal, !ModeInfo)
    ;
        RHS = rhs_functor(ConsId, IsExistConstr, RHSVars),
        modecheck_unification_functor(LHSVar, ConsId, IsExistConstr, RHSVars,
            Unification0, UnifyContext, UnifyGoalInfo0, Goal, !ModeInfo)
    ;
        RHS = rhs_lambda_goal(Purity, HOGroundness, _PredOrFunc,
            _LambdaEvalMethod, LambdaNonLocals, _LambdaQuantVars, _ArgModes,
            _Detism, _LambdaGoal),
        (
            Purity \= purity_impure,
            HOGroundness = ho_ground,
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            mode_info_get_instmap(!.ModeInfo, InstMap),
            AnyVars = list.filter(var_inst_contains_any(ModuleInfo, InstMap),
                LambdaNonLocals),
            AnyVars = [_ | _]
        ->
            set_of_var.init(WaitingVars),
            mode_info_error(WaitingVars,
                purity_error_lambda_should_be_any(AnyVars), !ModeInfo),
            Goal = conj(plain_conj, [])
        ;
            (
                goal_info_has_feature(UnifyGoalInfo0,
                    feature_lambda_undetermined_mode)
            ->
                modecheck_unification_rhs_undetermined_mode_lambda(LHSVar,
                    RHS, Unification0, UnifyContext, UnifyGoalInfo0, Goal,
                    !ModeInfo)
            ;
                modecheck_unification_rhs_lambda(LHSVar,
                    RHS, Unification0, UnifyContext, UnifyGoalInfo0, Goal,
                    !ModeInfo)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred modecheck_unification_var(prog_var::in, prog_var::in, unification::in,
    unify_context::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_unification_var(X, Y, Unification0, UnifyContext,
        UnifyGoalInfo0, UnifyGoalExpr, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, X, InstOfX0),
    instmap_lookup_var(InstMap0, Y, InstOfY0),
    % If X and Y are free and have a solver type and we are allowed to
    % insert initialisation calls at this point, then do so to allow
    % scheduling of the unification.
    (
        mode_info_solver_init_is_supported(!.ModeInfo),
        mode_info_may_init_solver_vars(!.ModeInfo),
        InstOfX0   = free,
        InstOfY0   = free,
        search_var_type(VarTypes, X, VarType),
        type_is_solver_type_with_auto_init(ModuleInfo0, VarType)
    ->
        construct_initialisation_call(X, VarType, any_inst,
            context_init, no, InitXGoal, !ModeInfo),
        MaybeInitX = yes(InitXGoal),
        instmap_set_var(X, any_inst, InstMap0, InstMap),
        InstOfX    = any_inst,
        InstOfY    = InstOfY0
    ;
        MaybeInitX = no,
        InstMap    = InstMap0,
        InstOfX    = InstOfX0,
        InstOfY    = InstOfY0
    ),
    mode_info_var_is_live(!.ModeInfo, X, LiveX),
    mode_info_var_is_live(!.ModeInfo, Y, LiveY),
    (
        ( LiveX = is_live, LiveY = is_live ->
            BothLive = is_live
        ;
            BothLive = is_dead
        ),
        abstractly_unify_inst(BothLive, InstOfX, InstOfY, real_unify,
            UnifyInst, Det1, ModuleInfo0, ModuleInfo1),
        % Don't allow free-free unifications if both variables are locked.
        % (Normally the checks for binding locked variables are done in
        % modecheck_set_var_inst, which is called below, but that won't catch
        % this case, because the inst of the variable will remain `free'.
        % XXX are there other cases like this one?)
        \+ (
            UnifyInst = free,
            mode_info_var_is_locked(!.ModeInfo, X, _XLockedReason),
            mode_info_var_is_locked(!.ModeInfo, Y, _YLockedReason),
            % a unification of the form `X = X' doesn't bind X,
            % and thus should be allowed even if X is locked
            X \= Y
        )
    ->
        Inst = UnifyInst,
        Det = Det1,
        mode_info_set_module_info(ModuleInfo1, !ModeInfo),
        modecheck_set_var_inst(X, Inst, yes(InstOfY), !ModeInfo),
        modecheck_set_var_inst(Y, Inst, yes(InstOfX), !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y,
            Det, UnifyContext, UnifyGoalInfo0, VarTypes, Unification0,
            UnifyGoalExpr0, !ModeInfo),
        (
            MaybeInitX = no,
            UnifyGoalExpr = UnifyGoalExpr0
        ;
            MaybeInitX = yes(InitGoal),
            compute_goal_instmap_delta(InstMap, UnifyGoalExpr0,
                UnifyGoalInfo0, UnifyGoalInfo, !ModeInfo),
            UnifySubGoal = hlds_goal(UnifyGoalExpr0, UnifyGoalInfo),
            UnifyGoalExpr = conj(plain_conj, [InitGoal, UnifySubGoal])
        )
    ;
        set_of_var.list_to_set([X, Y], WaitingVars),
        ModeError = mode_error_unify_var_var(X, Y, InstOfX, InstOfY),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        % If we get an error, set the inst to not_reached to suppress
        % follow-on errors. But don't call categorize_unification, because
        % that could cause an invalid call to `unify_proc.request_unify'
        Inst = not_reached,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        modecheck_set_var_inst(Y, Inst, no, !ModeInfo),
        % Return any old garbage.
        Unification = assign(X, Y),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Modes = ModeOfX - ModeOfY,
        UnifyGoalExpr = unify(X, rhs_var(Y), Modes, Unification, UnifyContext)
    ).

%-----------------------------------------------------------------------------%

:- pred modecheck_unification_functor(prog_var::in, cons_id::in,
    is_exist_constr::in, list(prog_var)::in, unification::in,
    unify_context::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_unification_functor(X, ConsId, IsExistConstruction, ArgVars0,
        Unification0, UnifyContext, GoalInfo0, GoalExpr, !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    lookup_var_type(VarTypes0, X, TypeOfX),

    (
        % We replace any unifications with higher-order pred constants
        % by lambda expressions. For example, we replace
        %
        %       X = list.append(Y)     % Y::in, X::out
        %
        % with
        %
        %       X = lambda [A1::in, A2::out] (list.append(Y, A1, A2))
        %
        % Normally this is done by polymorphism.process_unify_functor,
        % but if we are re-modechecking goals after lambda.m has been run
        % (e.g. for deforestation), then we may need to do it again here.
        % Note that any changes to this code here will probably need to be
        % duplicated there too.

        type_is_higher_order_details(TypeOfX, Purity, _, EvalMethod,
            PredArgTypes),
        ConsId = closure_cons(ShroudedPredProcId, _)
    ->
        % Convert the pred term to a lambda expression.
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        mode_info_get_varset(!.ModeInfo, VarSet0),
        mode_info_get_context(!.ModeInfo, Context),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        convert_pred_to_lambda_goal(Purity, EvalMethod, X, PredId, ProcId,
            ArgVars0, PredArgTypes, UnifyContext, GoalInfo0, Context,
            ModuleInfo0, Functor0, VarSet0, VarSet, VarTypes0, VarTypes),
        mode_info_set_varset(VarSet, !ModeInfo),
        mode_info_set_var_types(VarTypes, !ModeInfo),

        % Modecheck this unification in its new form.
        modecheck_unification(X, Functor0, Unification0, UnifyContext,
            GoalInfo0, GoalExpr, !ModeInfo)
    ;
        % Right hand sides that represent constant structures need to be
        % handled specially, because the term is inherently shared.
        cons_id_is_const_struct(ConsId, ConstNum)
    ->
        expect(unify(IsExistConstruction, is_not_exist_constr), $module, $pred,
            "const struct construction is existential"),
        expect(unify(ArgVars0, []), $module, $pred,
            "const struct construction has args"),
        modecheck_unify_const_struct(X, ConsId, ConstNum, UnifyContext,
            GoalExpr, !ModeInfo)
    ;
        % It is not a higher-order pred unification or a unification with a
        % constant structure, so just call modecheck_unify_functor to do
        % the ordinary thing.
        modecheck_unify_functor(X, TypeOfX, ConsId, IsExistConstruction,
            ArgVars0, Unification0, UnifyContext, GoalInfo0, GoalExpr,
            !ModeInfo)
    ).

:- pred modecheck_unification_rhs_lambda(prog_var::in,
    unify_rhs::in(rhs_lambda_goal), unification::in, unify_context::in,
    hlds_goal_info::in, hlds_goal_expr::out, mode_info::in, mode_info::out)
    is det.

modecheck_unification_rhs_lambda(X, LambdaGoal, Unification0, UnifyContext, _,
        unify(X, RHS, Mode, Unification, UnifyContext), !ModeInfo) :-
    LambdaGoal = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
        ArgVars, Vars, Modes0, Det, Goal0),

    % First modecheck the lambda goal itself:
    %
    % initialize the initial insts of the lambda variables;
    % check that the non-local vars are ground or any;
    % mark the non-local vars as shared;
    % if the higher-order inst is ground lock the non-local vars,
    % otherwise if it is `any' lock the non-local vars that themselves
    % do not match_initial any;
    % mark the non-clobbered lambda variables as live;
    % modecheck the goal;
    % check that the final insts are correct;
    % unmark the live vars;
    % unlock the locked vars;
    % restore the original instmap.
    %
    % XXX or should we merge the original and the final instmaps???
    %
    % The reason that we need to merge the original and final instmaps is
    % as follows. The lambda goal will not have bound any variables (since
    % they were locked), but it may have added some information or lost some
    % uniqueness. We cannot use the final instmap, because that may have
    % too much information. If we use the initial instmap, variables will be
    % considered as unique even if they become shared or clobbered in the
    % lambda goal!
    %
    % However even this may not be enough. If a unique non-local variable
    % is used in its unique inst (e.g. it's used in a ui mode) and then shared
    % within the lambda body, this is unsound. This variable should be marked
    % as shared at the _top_ of the lambda goal. As for implementing this,
    % it probably means that the lambda goal should be re-modechecked,
    % or even modechecked to a fixpoint.
    %
    % For the moment, since doing all that properly seems too hard, we just
    % share all non-local variables at the top of the lambda goal. This is
    % safe, but perhaps too conservative.

    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_how_to_check(!.ModeInfo, HowToCheckGoal),

    (
        HowToCheckGoal = check_modes,
        % This only needs to be done once.
        mode_info_get_types_of_vars(!.ModeInfo, Vars, VarTypes),
        propagate_types_into_mode_list(ModuleInfo0, VarTypes, Modes0, Modes)
    ;
        HowToCheckGoal = check_unique_modes,
        Modes = Modes0
    ),

    % Initialize the initial insts of the lambda variables.
    mode_list_get_initial_insts(ModuleInfo0, Modes, VarInitialInsts),
    assoc_list.from_corresponding_lists(Vars, VarInitialInsts, VarInstAL),
    VarInstMapDelta = instmap_delta_from_assoc_list(VarInstAL),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap.apply_instmap_delta(InstMap0, VarInstMapDelta, InstMap1),
    mode_info_set_instmap(InstMap1, !ModeInfo),

    % Mark the non-clobbered lambda variables as live.
    get_arg_lives(ModuleInfo0, Modes, ArgLives),
    get_live_vars(Vars, ArgLives, LiveVarsList),
    set_of_var.list_to_set(LiveVarsList, LiveVars),
    mode_info_add_live_vars(LiveVars, !ModeInfo),

    % Lock the non-locals. A ground lambda goal is not allowed to bind any
    % of the non-local variables, since it could get called more than once,
    % or from inside a negation. So in this case we lock all non-locals
    % (not counting the lambda quantified vars).
    %
    % If the lambda goal is inst `any', we don't lock the non-locals which
    % match_initial any, since it is safe to bind these any time that it
    % is safe to bind the lambda goal itself.
    Goal0 = hlds_goal(_, GoalInfo0),
    NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
    set_of_var.delete_list(Vars, NonLocals0, NonLocals1),
    (
        Groundness = ho_ground,
        NonLocals = NonLocals1
    ;
        Groundness = ho_any,
        mode_info_get_var_types(!.ModeInfo, NonLocalTypes),
        NonLocals = set_of_var.filter((pred(NonLocal::in) is semidet :-
                lookup_var_type(NonLocalTypes, NonLocal, NonLocalType),
                instmap_lookup_var(InstMap1, NonLocal, NonLocalInst),
                \+ inst_matches_initial(NonLocalInst, any(shared, none),
                    NonLocalType, ModuleInfo0)
            ), NonLocals1)
    ),
    set_of_var.to_sorted_list(NonLocals, NonLocalsList),
    instmap_lookup_vars(InstMap1, NonLocalsList, NonLocalInsts),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo2),
    (
        % XXX This test is too conservative.
        %
        % We should allow non-local variables to be non-ground sometimes,
        % possibly dependent on whether or not they are dead after this
        % unification. In addition, we should not "share" a unique non-local
        % variable if these two conditions hold:
        %
        % - It is dead after this unification.
        % - It is not shared within the lambda body.
        %
        % Unfortunately, we can't test the latter condition until after
        % we've mode-checked the lambda body. (See the above comment on
        % merging the initial and final instmaps.)

        (
            Groundness = ho_ground,
            Purity \= purity_impure
        ->
            inst_list_is_ground(NonLocalInsts, ModuleInfo2)
        ;
            inst_list_is_ground_or_any(NonLocalInsts, ModuleInfo2)
        )
    ->
        make_shared_inst_list(NonLocalInsts, SharedNonLocalInsts,
            ModuleInfo2, ModuleInfo3),
        instmap_set_vars_corresponding(NonLocalsList, SharedNonLocalInsts,
            InstMap1, InstMap2),
        mode_info_set_module_info(ModuleInfo3, !ModeInfo),
        mode_info_set_instmap(InstMap2, !ModeInfo),

        mode_info_lock_vars(var_lock_lambda(PredOrFunc), NonLocals, !ModeInfo),

        mode_checkpoint(enter, "lambda goal", !ModeInfo),
        % If we're being called from unique_modes.m, then we need to
        % call unique_modes_check_goal rather than modecheck_goal.
        (
            HowToCheckGoal = check_unique_modes,
            unique_modes_check_goal(Goal0, Goal1, !ModeInfo)
        ;
            HowToCheckGoal = check_modes,
            modecheck_goal(Goal0, Goal1, !ModeInfo)
        ),
        mode_list_get_final_insts(ModuleInfo0, Modes, FinalInsts),
        modecheck_lambda_final_insts(Vars, FinalInsts, Goal1, Goal, !ModeInfo),
        mode_checkpoint(exit, "lambda goal", !ModeInfo),

        mode_info_remove_live_vars(LiveVars, !ModeInfo),
        mode_info_unlock_vars(var_lock_lambda(PredOrFunc), NonLocals,
            !ModeInfo),

        % Ensure that the non-local vars are shared OUTSIDE the
        % lambda unification as well as inside.

        instmap_set_vars_corresponding(NonLocalsList, SharedNonLocalInsts,
            InstMap0, InstMap11),
        mode_info_set_instmap(InstMap11, !ModeInfo),

        % Now modecheck the unification of X with the lambda-expression.

        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            ArgVars, Vars, Modes, Det, Goal),
        modecheck_unify_lambda(X, PredOrFunc, ArgVars, Modes, Det,
            RHS0, RHS, Unification0, Unification, Mode, !ModeInfo)
    ;
        list.filter(
            (pred(Var :: in) is semidet :-
                instmap_lookup_var(InstMap1, Var, Inst),
                \+ inst_is_ground(ModuleInfo2, Inst)
            ), NonLocalsList, NonGroundNonLocals),
        (
            NonGroundNonLocals = [BadVar | _],
            instmap_lookup_var(InstMap1, BadVar, BadInst),
            WaitingVars = set_of_var.make_singleton(BadVar),
            ModeError = mode_error_non_local_lambda_var(BadVar, BadInst),
            mode_info_error(WaitingVars, ModeError, !ModeInfo)
        ;
            NonGroundNonLocals = [],
            unexpected($module, $pred, "very strange var")
        ),
        % Return any old garbage.
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            ArgVars, Vars, Modes0, Det, Goal0),
        Mode = (free -> free) - (free -> free),
        Unification = Unification0
    ).

:- pred modecheck_unify_lambda(prog_var::in, pred_or_func::in,
    list(prog_var)::in, list(mer_mode)::in, determinism::in,
    unify_rhs::in, unify_rhs::out, unification::in, unification::out,
    pair(mer_mode)::out, mode_info::in, mode_info::out) is det.

modecheck_unify_lambda(X, PredOrFunc, ArgVars, LambdaModes, LambdaDet,
        RHS0, RHS, Unification0, Unification, Mode, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, X, InstOfX),
    InstOfY = ground(unique, higher_order(LambdaPredInfo)),
    LambdaPredInfo = pred_inst_info(PredOrFunc, LambdaModes,
        arg_reg_types_unset, LambdaDet),
    (
        abstractly_unify_inst(is_dead, InstOfX, InstOfY, real_unify,
            UnifyInst, _Det, ModuleInfo0, ModuleInfo1)
    ->
        Inst = UnifyInst,
        mode_info_set_module_info(ModuleInfo1, !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
        % the lambda expression just maps its argument variables
        % from their current insts to the same inst
        instmap_lookup_vars(InstMap0, ArgVars, ArgInsts),
        inst_lists_to_mode_list(ArgInsts, ArgInsts, ArgModes),
        categorize_unify_var_lambda(ModeOfX, ArgModes, X, ArgVars, PredOrFunc,
            RHS0, RHS, Unification0, Unification, !ModeInfo),
        modecheck_set_var_inst(X, Inst, no, !ModeInfo)
    ;
        set_of_var.list_to_set([X], WaitingVars),
        ModeError = mode_error_unify_var_lambda(X, InstOfX, InstOfY),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        % If we get an error, set the inst to not_reached to avoid cascading
        % errors. But don't call categorize_unification, because that could
        % cause an invalid call to `unify_proc.request_unify'
        Inst = not_reached,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,

        % Return any old garbage.
        Unification = Unification0,
        RHS = RHS0
    ).

:- pred modecheck_unification_rhs_undetermined_mode_lambda(prog_var::in,
    unify_rhs::in(rhs_lambda_goal), unification::in, unify_context::in,
    hlds_goal_info::in, hlds_goal_expr::out, mode_info::in, mode_info::out)
    is det.

modecheck_unification_rhs_undetermined_mode_lambda(X, RHS0, Unification,
        UnifyContext, GoalInfo0, Goal, !ModeInfo) :-
    RHS0 = rhs_lambda_goal(_, _, _, _, _, _, _, _, Goal0),
    % Find out the predicate called in the lambda goal.
    ( predids_with_args_from_goal(Goal0, [{PredId, ArgVars}]) ->
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        match_modes_by_higher_order_insts(ModuleInfo, InstMap, VarTypes,
            ArgVars, PredInfo, MatchResult),
        (
            ( MatchResult = possible_modes([])
            ; MatchResult = ho_arg_not_ground
            ),
            WaitingVars = set_of_var.make_singleton(X),
            ModeError = mode_error_unify_var_multimode_pred(X, PredId),
            mode_info_error(WaitingVars, ModeError, !ModeInfo),
            % Return any old garbage.
            Goal = true_goal_expr
        ;
            MatchResult = possible_modes([ProcId]),
            fix_undetermined_mode_lambda_goal(ProcId, RHS0, RHS, ModuleInfo),
            goal_info_remove_feature(feature_lambda_undetermined_mode,
                GoalInfo0, GoalInfo),
            % Modecheck this unification in its new form.
            modecheck_unification_rhs_lambda(X, RHS, Unification, UnifyContext,
                GoalInfo, Goal, !ModeInfo)
        ;
            MatchResult = possible_modes([_, _ | _]),
            WaitingVars = set_of_var.make_singleton(X),
            ModeError =
                mode_error_unify_var_multimode_pred_undetermined(X, PredId),
            mode_info_error(WaitingVars, ModeError, !ModeInfo),
            % Return any old garbage.
            Goal = true_goal_expr
        )
    ;
        unexpected($module, $pred, "expecting single call")
    ).

%-----------------------------------------------------------------------------%

:- pred modecheck_unify_const_struct(prog_var::in, cons_id::in, int::in,
    unify_context::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_unify_const_struct(X, ConsId, ConstNum, UnifyContext,
        UnifyGoalExpr, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap),
    instmap_lookup_var(InstMap, X, InstOfX),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    module_info_get_const_struct_db(ModuleInfo0, ConstStructDb),
    lookup_const_struct_num(ConstStructDb, ConstNum, ConstStruct),
    ConstStruct = const_struct(_, _, _, InstOfY),
    ( inst_is_free(ModuleInfo0, InstOfX) ->
        Inst = InstOfY,
        modecheck_set_var_inst(X, Inst, yes(InstOfY), !ModeInfo),
        Unification = construct(X, ConsId, [], [], construct_statically,
            cell_is_shared, no_construct_sub_info),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Modes = ModeOfX - ModeOfY,
        UnifyGoalExpr = unify(X, rhs_functor(ConsId, is_not_exist_constr, []),
            Modes, Unification, UnifyContext)
    ;
%       abstractly_unify_inst(LiveX, InstOfX, InstOfY, real_unify,
%           UnifyInst, Det1, ModuleInfo0, ModuleInfo1)
%   ->
%       Inst = UnifyInst,
%       Det = Det1,
%       mode_info_set_module_info(ModuleInfo1, !ModeInfo),
%       modecheck_set_var_inst(Y, Inst, yes(InstOfX), !ModeInfo),
%       ModeOfX = (InstOfX -> Inst),
%       ModeOfY = (InstOfY -> Inst),
%       categorize_unify_var_const_struct(ModeOfX, ModeOfY, LiveX, X, ConsId,
%           Det, UnifyContext, UnifyGoalInfo0, VarTypes, Unification0,
%           UnifyGoalExpr0, !ModeInfo),
%   ;
        set_of_var.list_to_set([X], WaitingVars),
        ModeError = mode_error_unify_var_functor(X, ConsId, [], InstOfX, []),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        % If we get an error, set the inst to not_reached to suppress
        % follow-on errors. But don't call categorize_unification, because
        % that could cause an invalid call to `unify_proc.request_unify'
        Inst = not_reached,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        % Return any old garbage.
        Unification = construct(X, ConsId, [], [], construct_statically,
            cell_is_shared, no_construct_sub_info),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Modes = ModeOfX - ModeOfY,
        UnifyGoalExpr = unify(X, rhs_functor(ConsId, is_not_exist_constr, []),
            Modes, Unification, UnifyContext)
    ).

%-----------------------------------------------------------------------------%

:- pred modecheck_unify_functor(prog_var::in, mer_type::in, cons_id::in,
    is_exist_constr::in, list(prog_var)::in, unification::in,
    unify_context::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_unify_functor(X0, TypeOfX, ConsId0, IsExistConstruction, ArgVars0,
        Unification0, UnifyContext, GoalInfo0, GoalExpr, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    ensure_exist_constr_is_construction(IsExistConstruction, X0, X,
        InstOfX, LiveX, ExtraGoals0, !ModeInfo),
    add_solver_init_calls_if_needed(InstOfX, ArgVars0, ExtraGoals1, !ModeInfo),

    % The calls above may have changed the instmap.
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    mode_info_get_how_to_check(!.ModeInfo, HowToCheckGoal),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    instmap_lookup_vars(InstMap1, ArgVars0, InstArgs),
    mode_info_var_list_is_live(!.ModeInfo, ArgVars0, LiveArgs),
    qualify_cons_id(ArgVars0, ConsId0, ConsId, InstConsId),
    InstOfY = bound(unique, inst_test_no_results,
        [bound_functor(InstConsId, InstArgs)]),
    (
        % The occur check: is the unification of the form X = f(..., X, ...)?
        list.member(X, ArgVars0)
    ->
        ( inst_is_ground(ModuleInfo0, InstOfX) ->
            % If X is ground, then we don't consider X = f(..., X, ...)
            % to be a mode error, but it is a unification that can never
            % succeed, and thus it is very unlikely to be what the programmer
            % intended to write.
            %
            % Unfortunately, if the unintended occurrence of X on the
            % right hand side has more than one function symbol above it,
            % then we won't generate this warning, because the compiler
            % will transform X = f(g(X)) into superhomogeneous form as
            % X = f(Y), Y = g(X), and neither of those unifications
            % will pass the list.member(X, ArgVars0) test above.
            Warning = cannot_succeed_ground_occur_check(X, ConsId),
            mode_info_warning(Warning, !ModeInfo),
            modecheck_set_var_inst(X, not_reached, no, !ModeInfo),
            GoalExpr = disj([])
        ;
            % If X is not ground, then X = f(..., X, ...) is a mode error.
            handle_var_functor_mode_error(X, InstConsId, ArgVars0,
                InstOfX, InstArgs, [X], GoalExpr, !ModeInfo)
        )
    ;
        % XXX We forbid the construction of partially instantiated structures
        % involving solver types. We'd like to forbid all such constructions
        % here, but that causes trouble with the current implementation of
        % term.term_to_univ_special_case which does use partial instantiation
        % (in a rather horrible way). This is a hacky solution that gets us
        % most of what we want w.r.t. solver types.
        not (
            inst_is_free(ModuleInfo0, InstOfX),
            list.member(InstArg, InstArgs),
            inst_is_free(ModuleInfo0, InstArg),
            list.member(ArgVar, ArgVars0),
            lookup_var_type(VarTypes, ArgVar, ArgType),
            type_is_or_may_contain_solver_type(ModuleInfo0, ArgType)
        ),
        abstractly_unify_inst_functor(LiveX, InstOfX, InstConsId,
            InstArgs, LiveArgs, real_unify, TypeOfX,
            Inst, Det, ModuleInfo0, ModuleInfo1)
    ->
        mode_info_set_module_info(ModuleInfo1, !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
        get_mode_of_args(Inst, InstArgs, ModeArgs),
        inst_expand_and_remove_constrained_inst_vars(ModuleInfo1,
            InstOfX, InstOfX1),
        list.length(ArgVars0, Arity),
        get_arg_insts_det(InstOfX1, InstConsId, Arity, InstOfXArgs),
        get_mode_of_args(Inst, InstOfXArgs, ModeOfXArgs),
        categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ModeArgs,
            X, ConsId, ArgVars0, VarTypes, UnifyContext,
            Unification0, Unification1, !ModeInfo),
        split_complicated_subunifies(Unification1, Unification,
            ArgVars0, ArgVars, ExtraGoals2, !ModeInfo),
        modecheck_set_var_inst(X, Inst, yes(InstOfY), !ModeInfo),
        bind_args_if_needed(InstOfX, Inst, ArgVars, InstOfXArgs, !ModeInfo),

        % Construct the final goal expression.
        (
            Unification = construct(_, _, _, _, _, _, _),
            LiveX = is_dead
        ->
            % Optimize away construction of unused terms by replacing
            % the unification with `true'.
            GoalExpr = conj(plain_conj, [])
        ;
            Det = detism_failure
        ->
            % Optimize away unifications which always fail by replacing
            % them with `fail'.
            GoalExpr = disj([]),
            maybe_generate_cannot_succeed_warning(X, InstOfX, ConsId,
                !ModeInfo)
        ;
            Functor = rhs_functor(ConsId, IsExistConstruction, ArgVars),
            UnifyExpr = unify(X, Functor, Mode, Unification, UnifyContext),

            % Modecheck_unification sometimes needs to introduce new goals
            % to handle complicated sub-unifications in deconstructions.
            % The only time this can happen during unique mode analysis is if
            % the instmap is unreachable, since inst_is_bound succeeds for
            % not_reached. (If it did in other cases, the code would be wrong
            % since it wouldn't have the correct determinism annotations.)

            append_extra_goals(ExtraGoals0, ExtraGoals1, ExtraGoals01),
            append_extra_goals(ExtraGoals01, ExtraGoals2, ExtraGoals),
            (
                HowToCheckGoal = check_unique_modes,
                ExtraGoals = extra_goals(_, _),
                instmap_is_reachable(InstMap1)
            ->
                unexpected($module, $pred,
                    "re-modecheck of unification " ++
                    "encountered complicated sub-unifies")
            ;
                true
            ),
            handle_extra_goals(UnifyExpr, ExtraGoals, GoalInfo0,
                [X0 | ArgVars0], [X | ArgVars], InstMap0, GoalExpr, !ModeInfo)
        )
    ;
        % Including all ArgVars0 in the waiting_vars is a conservative
        % approximation.
        handle_var_functor_mode_error(X, InstConsId, ArgVars0,
            InstOfX, InstArgs, [X | ArgVars0], GoalExpr, !ModeInfo)
    ).

    % If a unification was originally of the form X0 = 'new f'(Ys),
    % then it must be classified as a construction. If it were classified
    % as a deconstruction, the argument unifications would be ill-typed.
    %
    % If X0 is already bound, then, to make sure the unification is classified
    % as a construction, we make a new variable X the target of the
    % construction, and unify this new left-hand-side variable with the old,
    % yielding code of the form X = 'new f'(Ys), X0 = X.
    %
:- pred ensure_exist_constr_is_construction(is_exist_constr::in,
    prog_var::in, prog_var::out, mer_inst::out, is_live::out, extra_goals::out,
    mode_info::in, mode_info::out) is det.

ensure_exist_constr_is_construction(IsExistConstruction, X0, X, InstOfX, LiveX,
        ExtraGoals, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, X0, InstOfX0),
    (
        IsExistConstruction = is_exist_constr,
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        \+ inst_is_free(ModuleInfo0, InstOfX0)
    ->
        make_complicated_sub_unify(X0, X, ExtraGoals, !ModeInfo),
        InstOfX = free,
        LiveX = is_live
    ;
        X = X0,
        InstOfX = InstOfX0,
        mode_info_var_is_live(!.ModeInfo, X, LiveX),
        ExtraGoals = no_extra_goals
    ).

:- pred add_solver_init_calls_if_needed(mer_inst::in, list(prog_var)::in,
    extra_goals::out, mode_info::in, mode_info::out) is det.

add_solver_init_calls_if_needed(InstOfX, ArgVars0, ExtraGoals, !ModeInfo) :-
    mode_info_get_how_to_check(!.ModeInfo, HowToCheckGoal),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    (
        % If we are allowed to insert solver type initialisation calls
        % and InstOfX is free and all ArgVars0 are either non-free or have
        % solver types, then we know that this is going to be a construction,
        % so we can insert the necessary initialisation calls.
        ArgVars0 = [_ | _],
        HowToCheckGoal = check_modes,
        inst_match.inst_is_free(ModuleInfo0, InstOfX),
        mode_info_may_init_solver_vars(!.ModeInfo),
        mode_info_solver_init_is_supported(!.ModeInfo),
        instmap_lookup_vars(InstMap0, ArgVars0, InstArgs0),
        all_arg_vars_are_non_free_or_solver_vars(ArgVars0, InstArgs0,
            VarTypes, ModuleInfo0, ArgVarsToInit)
    ->
        construct_initialisation_calls(ArgVarsToInit, InitGoals, !ModeInfo),
        (
            InitGoals = [],
            ExtraGoals = no_extra_goals
        ;
            InitGoals = [_ | _],
            ExtraGoals = extra_goals(InitGoals, [])
        )
    ;
        ExtraGoals = no_extra_goals
    ).

:- pred all_arg_vars_are_non_free_or_solver_vars(list(prog_var)::in,
    list(mer_inst)::in, vartypes::in, module_info::in, list(prog_var)::out)
    is semidet.

all_arg_vars_are_non_free_or_solver_vars([], [], _, _, []).
all_arg_vars_are_non_free_or_solver_vars([], [_ | _], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
all_arg_vars_are_non_free_or_solver_vars([_ | _], [], _, _, _) :-
    unexpected($module, $pred, "mismatched list lengths").
all_arg_vars_are_non_free_or_solver_vars([ArgVar | ArgVars], [Inst | Insts],
        VarTypes, ModuleInfo, ArgVarsToInit) :-
    ( inst_match.inst_is_free(ModuleInfo, Inst) ->
        lookup_var_type(VarTypes, ArgVar, ArgType),
        type_is_or_may_contain_solver_type(ModuleInfo, ArgType),
        all_arg_vars_are_non_free_or_solver_vars(ArgVars, Insts,
            VarTypes, ModuleInfo, ArgVarsToInitTail),
        ArgVarsToInit = [ArgVar | ArgVarsToInitTail]
    ;
        all_arg_vars_are_non_free_or_solver_vars(ArgVars, Insts,
            VarTypes, ModuleInfo, ArgVarsToInit)
    ).

:- pred handle_var_functor_mode_error(prog_var::in, cons_id::in,
    list(prog_var)::in, mer_inst::in, list(mer_inst)::in,
    list(prog_var)::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

handle_var_functor_mode_error(X, InstConsId, ArgVars0,
        InstOfX, InstArgs, WaitingVarsList, GoalExpr, !ModeInfo) :-
    set_of_var.list_to_set(WaitingVarsList, WaitingVars),
    ModeError = mode_error_unify_var_functor(X, InstConsId, ArgVars0,
        InstOfX, InstArgs),
    mode_info_error(WaitingVars, ModeError, !ModeInfo),
    % If we get an error, set the inst to not_reached to avoid cascading
    % errors. But don't call categorize_unification, because that could
    % cause an invalid call to `unify_proc.request_unify'.
    Inst = not_reached,
    modecheck_set_var_inst(X, Inst, no, !ModeInfo),
    NoArgInsts = list.duplicate(list.length(ArgVars0), no),
    bind_args(Inst, ArgVars0, NoArgInsts, !ModeInfo),
    % Return any old garbage.
    GoalExpr = disj([]).

    % Warn about unifications that always fail if the user has requested
    % such warnings, and they are reasonably certain not to be misleading.
    %
:- pred maybe_generate_cannot_succeed_warning(prog_var::in, mer_inst::in,
    cons_id::in, mode_info::in, mode_info::out) is det.

maybe_generate_cannot_succeed_warning(X, InstOfX, ConsId, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_unification_cannot_succeed,
        WarnCannotSucceed),
    (
        WarnCannotSucceed = yes,
        mode_info_get_in_dupl_for_switch(!.ModeInfo, InDuplForSwitch),
        (
            InDuplForSwitch = in_dupl_for_switch
            % Suppress the warning, since the unification may succeed
            % in another copy of this duplicated switch arm.
        ;
            InDuplForSwitch = not_in_dupl_for_switch,
            mode_info_get_pred_id(!.ModeInfo, PredId),
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_origin(PredInfo, Origin),
            ReportWarning =
                should_report_mode_warning_for_pred_origin(Origin),
            (
                ReportWarning = yes,
                Warning = cannot_succeed_var_functor(X, InstOfX, ConsId),
                mode_info_warning(Warning, !ModeInfo)
            ;
                ReportWarning = no
            )
        )
    ;
        WarnCannotSucceed = no
    ).

%-----------------------------------------------------------------------------%

    % The argument unifications in a construction or deconstruction
    % unification must be simple assignments, they cannot be
    % complicated unifications. If they are, we split them out
    % into separate unifications by introducing fresh variables here.
    %
:- pred split_complicated_subunifies(unification::in, unification::out,
    list(prog_var)::in, list(prog_var)::out, extra_goals::out,
    mode_info::in, mode_info::out) is det.

split_complicated_subunifies(Unification0, Unification, ArgVars0, ArgVars,
        ExtraGoals, !ModeInfo) :-
    (
        Unification0 = deconstruct(X, ConsId, ArgVars0, ArgModes0, Det, CanCGC)
    ->
        (
            split_complicated_subunifies_2(ArgVars0, ArgModes0,
                ArgVars1, ExtraGoals1, !ModeInfo)
        ->
            ExtraGoals = ExtraGoals1,
            ArgVars = ArgVars1,
            Unification = deconstruct(X, ConsId, ArgVars, ArgModes0, Det,
                CanCGC)
        ;
            unexpected($module, $pred, "split_complicated_subunifies_2 failed")
        )
    ;
        Unification = Unification0,
        ArgVars = ArgVars0,
        ExtraGoals = no_extra_goals
    ).

:- pred split_complicated_subunifies_2(list(prog_var)::in, list(uni_mode)::in,
    list(prog_var)::out, extra_goals::out, mode_info::in, mode_info::out)
    is semidet.

split_complicated_subunifies_2([], [], [], no_extra_goals, !ModeInfo).
split_complicated_subunifies_2([Var0 | Vars0], [UniMode0 | UniModes0],
        Vars, ExtraGoals, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    UniMode0 = (InitialInstX - InitialInstY -> FinalInstX - FinalInstY),
    ModeX = (InitialInstX -> FinalInstX),
    ModeY = (InitialInstY -> FinalInstY),
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    lookup_var_type(VarTypes0, Var0, VarType),
    (
        mode_to_arg_mode(ModuleInfo, ModeX, VarType, top_in),
        mode_to_arg_mode(ModuleInfo, ModeY, VarType, top_in)
    ->
        make_complicated_sub_unify(Var0, Var, ExtraGoals0, !ModeInfo),

        % Recursive call to handle the remaining variables...
        split_complicated_subunifies_2(Vars0, UniModes0,
            Vars1, ExtraGoals1, !ModeInfo),
        Vars = [Var | Vars1],
        append_extra_goals(ExtraGoals0, ExtraGoals1, ExtraGoals)
    ;
        split_complicated_subunifies_2(Vars0, UniModes0, Vars1,
            ExtraGoals, !ModeInfo),
        Vars = [Var0 | Vars1]
    ).

:- pred make_complicated_sub_unify(prog_var::in, prog_var::out,
    extra_goals::out, mode_info::in, mode_info::out) is det.

make_complicated_sub_unify(Var0, Var, ExtraGoals0, !ModeInfo) :-
    % introduce a new variable `Var'
    mode_info_get_varset(!.ModeInfo, VarSet0),
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    varset.new_var(Var, VarSet0, VarSet),
    lookup_var_type(VarTypes0, Var0, VarType),
    add_var_type(Var, VarType, VarTypes0, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo),

    create_var_var_unification(Var0, Var, VarType, !.ModeInfo, ExtraGoal),

    % Insert the new unification at the start of the extra goals.
    ExtraGoals0 = extra_goals([], [ExtraGoal]).

create_var_var_unification(Var0, Var, Type, ModeInfo, Goal) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    mode_context_to_unify_context(ModeInfo, ModeContext, UnifyContext),
    UnifyContext = unify_context(MainContext, SubContexts),

    create_pure_atomic_complicated_unification(Var0, rhs_var(Var), Context,
        MainContext, SubContexts, hlds_goal(GoalExpr0, GoalInfo0)),

    % Compute the goal_info nonlocal vars for the newly created goal
    % (excluding the type_info vars -- they are added below).
    % N.B. This may overestimate the set of non-locals,
    % but that shouldn't cause any problems.

    set_of_var.list_to_set([Var0, Var], NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
    goal_info_set_context(Context, GoalInfo1, GoalInfo2),

    % Look up the map(tvar, type_info_locn) in the proc_info,
    % since it is needed by polymorphism.unification_typeinfos.

    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_pred_id(ModeInfo, PredId),
    mode_info_get_proc_id(ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),

    % Call polymorphism.unification_typeinfos to add the appropriate
    % type-info and type-class-info variables to the nonlocals
    % and to the unification.

    ( GoalExpr0 = unify(X, Y, Mode, Unification0, FinalUnifyContext) ->
        unification_typeinfos_rtti_varmaps(Type, RttiVarMaps,
            Unification0, Unification, GoalInfo2, GoalInfo),
        GoalExpr = unify(X, Y, Mode, Unification, FinalUnifyContext)
    ;
        unexpected($module, $pred, "unexpected GoalExpr0")
    ).

%-----------------------------------------------------------------------------%

    % categorize_unify_var_var works out which category a unification
    % between a variable and another variable expression is - whether it is
    % an assignment, a simple test or a complicated unify.
    %
:- pred categorize_unify_var_var(mer_mode::in, mer_mode::in,
    is_live::in, is_live::in, prog_var::in,
    prog_var::in, determinism::in, unify_context::in, hlds_goal_info::in,
    vartypes::in, unification::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y, Det,
        UnifyContext, GoalInfo, VarTypes, Unification0, Unify, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    (
        mode_is_output(ModuleInfo0, ModeOfX)
    ->
        Unification = assign(X, Y)
    ;
        mode_is_output(ModuleInfo0, ModeOfY)
    ->
        Unification = assign(Y, X)
    ;
        mode_is_unused(ModuleInfo0, ModeOfX),
        mode_is_unused(ModuleInfo0, ModeOfY)
    ->
        % For free-free unifications, we pretend for a moment that they are
        % an assignment to the dead variable - they will then be optimized
        % away.
        (
            LiveX = is_dead,
            Unification = assign(X, Y)
        ;
            LiveX = is_live,
            (
                LiveY = is_dead,
                Unification = assign(Y, X)
            ;
                LiveY = is_live,
                unexpected($module, $pred, "free-free unify!")
            )
        )
    ;
        % Check for unreachable unifications.
        ( mode_get_insts(ModuleInfo0, ModeOfX, not_reached, _)
        ; mode_get_insts(ModuleInfo0, ModeOfY, not_reached, _)
        )
    ->
        % For these, we can generate any old junk here --
        % we just need to avoid calling modecheck_complicated_unify,
        % since that might abort.

        Unification = simple_test(X, Y)
    ;
        lookup_var_type(VarTypes, X, Type),
        (
            type_is_atomic(ModuleInfo0, Type),
            not type_has_user_defined_equality_pred(ModuleInfo0, Type, _)
        ->
            Unification = simple_test(X, Y)
        ;
            % Unification of c_pointers is a runtime error unless introduced by
            % the compiler.
            Type = c_pointer_type,
            goal_info_has_feature(GoalInfo, feature_pretest_equality_condition)
        ->
            Unification = simple_test(X, Y)
        ;
            modecheck_complicated_unify(X, Y, Type, ModeOfX, ModeOfY, Det,
                UnifyContext, Unification0, Unification, !ModeInfo)
        )
    ),

    % Optimize away unifications with dead variables and simple tests that
    % cannot fail by replacing them with `true'. (The optimization of simple
    % tests is necessary because otherwise determinism analysis assumes they
    % can fail. The optimization of assignments to dead variables may be
    % necessary to stop the code generator from getting confused.)
    %
    % Optimize away unifications which always fail by replacing them with
    % `fail'.
    (
        Unification = assign(AssignTarget, AssignSource),
        mode_info_var_is_live(!.ModeInfo, AssignTarget, is_dead)
    ->
        Unify = conj(plain_conj, []),
        record_optimize_away(GoalInfo, AssignTarget, AssignSource, !ModeInfo)
    ;
        Unification = simple_test(TestVar1, TestVar2),
        Det = detism_det
    ->
        Unify = conj(plain_conj, []),
        record_optimize_away(GoalInfo, TestVar1, TestVar2, !ModeInfo)
    ;
        Det = detism_failure
    ->
        % This optimisation is safe because the only way that we can analyse
        % a unification as having no solutions is that the unification
        % always fails.
        %
        % Unifying two preds is not erroneous as far as the
        % mode checker is concerned, but a mode _error_.
        Unify = disj([]),
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, warn_unification_cannot_succeed,
            WarnCannotSucceed),
        (
            WarnCannotSucceed = yes,
            mode_get_insts(ModuleInfo0, ModeOfX, InstOfX, _),
            mode_get_insts(ModuleInfo0, ModeOfY, InstOfY, _),
            mode_info_get_pred_id(!.ModeInfo, PredId),
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_origin(PredInfo, Origin),
            ReportWarning = should_report_mode_warning_for_pred_origin(Origin),
            (
                ReportWarning = yes,
                Warning = cannot_succeed_var_var(X, Y, InstOfX, InstOfY),
                mode_info_warning(Warning, !ModeInfo)
            ;
                ReportWarning = no
            )
        ;
            WarnCannotSucceed = no
        )
    ;
        Unify = unify(X, rhs_var(Y), ModeOfX - ModeOfY,
            Unification, UnifyContext)
    ).

    % If we optimize away a singleton variable in a unification in one branch
    % of e.g. a switch, it is possible that the same variable is a singleton
    % in another branch, but cannot be optimized away because it is bound in
    % a call (which cannot be optimized away). In such cases, we must make sure
    % that we call requantification to delete the variable from the nonlocals
    % set of the switch, because otherwise, the arms of the switch would
    % disagree on which nonlocals are bound.
    %
:- pred record_optimize_away(hlds_goal_info::in, prog_var::in, prog_var::in,
    mode_info::in, mode_info::out) is det.

record_optimize_away(GoalInfo, Var1, Var2, !ModeInfo) :-
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    (
        set_of_var.member(NonLocals, Var1),
        set_of_var.member(NonLocals, Var2)
    ->
        true
    ;
        mode_info_need_to_requantify(!ModeInfo)
    ).

    % Modecheck_complicated_unify does some extra checks that are needed
    % for mode-checking complicated unifications.
    %
:- pred modecheck_complicated_unify(prog_var::in, prog_var::in,
    mer_type::in, mer_mode::in, mer_mode::in, determinism::in,
    unify_context::in, unification::in, unification::out,
    mode_info::in, mode_info::out) is det.

modecheck_complicated_unify(X, Y, Type, ModeOfX, ModeOfY, Det, UnifyContext,
        Unification0, Unification, !ModeInfo) :-
    % Build up the unification.
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_get_insts(ModuleInfo0, ModeOfX, InitialInstX, FinalInstX),
    mode_get_insts(ModuleInfo0, ModeOfY, InitialInstY, FinalInstY),
    UniMode = ((InitialInstX - InitialInstY) -> (FinalInstX - FinalInstY)),
    determinism_components(Det, CanFail, _),
    ( Unification0 = complicated_unify(_, _, UnifyTypeInfoVars0) ->
        UnifyTypeInfoVars = UnifyTypeInfoVars0
    ;
        unexpected($module, $pred, "non-complicated unify")
    ),
    Unification = complicated_unify(UniMode, CanFail, UnifyTypeInfoVars),

    % Check that all the type_info or type_class_info variables used
    % by the polymorphic unification are ground.
    (
        % Optimize common case.
        UnifyTypeInfoVars = []
    ;
        UnifyTypeInfoVars = [_ | _],
        list.length(UnifyTypeInfoVars, NumTypeInfoVars),
        list.duplicate(NumTypeInfoVars, ground(shared, none), ExpectedInsts),
        mode_info_set_call_context(call_context_unify(UnifyContext),
            !ModeInfo),
        InitialArgNum = 0,
        modecheck_var_has_inst_list_no_exact_match(UnifyTypeInfoVars,
            ExpectedInsts, InitialArgNum, _InstVarSub, !ModeInfo),
            % we can ignore _InstVarSub since type_info variables
            % should not have variable insts.
        mode_info_unset_call_context(!ModeInfo)
    ),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo3),
    (
        mode_info_get_errors(!.ModeInfo, Errors),
        Errors = [_ | _]
    ->
        true
    ;
        % Check that we're not trying to do a polymorphic unification
        % in a mode other than (in, in).
        % [Actually we also allow `any' insts, since the (in, in)
        % mode of unification for types which have `any' insts must
        % also be able to handle (in(any), in(any)) unifications.]
        Type = type_variable(_, _),
        \+ inst_is_ground_or_any(ModuleInfo3, InitialInstX)
    ->
        WaitingVars = set_of_var.make_singleton(X),
        ModeError = mode_error_poly_unify(X, InitialInstX),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ;
        Type = type_variable(_, _),
        \+ inst_is_ground_or_any(ModuleInfo3, InitialInstY)
    ->
        WaitingVars = set_of_var.make_singleton(Y),
        ModeError = mode_error_poly_unify(Y, InitialInstY),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ;
        % Check that we're not trying to do a higher-order unification.
        type_is_higher_order_details(Type, _, PredOrFunc, _, _)
    ->
        % We do not want to report this as an error if it occurs in a
        % compiler-generated predicate - instead, we delay the error
        % until runtime so that it only occurs if the compiler-generated
        % predicate gets called. not_reached is considered bound, so the
        % error message would be spurious if the instmap is unreachable.
        mode_info_get_pred_id(!.ModeInfo, PredId),
        module_info_pred_info(ModuleInfo3, PredId, PredInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap0),
        (
            ( is_unify_or_compare_pred(PredInfo)
            ; instmap_is_unreachable(InstMap0)
            )
        ->
            true
        ;
            set_of_var.init(WaitingVars),
            ModeError =
                mode_error_unify_pred(X, error_at_var(Y), Type, PredOrFunc),
            mode_info_error(WaitingVars, ModeError, !ModeInfo)
        )
    ;
        % Ensure that we will generate code for the unification procedure
        % that will be used to implement this complicated unification.
        type_to_ctor(Type, TypeCtor)
    ->
        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_instvarset(!.ModeInfo, InstVarSet),
        UnifyProcId = unify_proc_id(TypeCtor, UniMode),
        unify_proc.request_unify(UnifyProcId, InstVarSet,
            Det, Context, ModuleInfo3, ModuleInfo),
        mode_info_set_module_info(ModuleInfo, !ModeInfo)
    ;
        true
    ).

    % Categorize_unify_var_lambda works out which category a unification
    % between a variable and a lambda expression is - whether it is a
    % construction unification or a deconstruction. It also works out
    % whether it will be deterministic or semideterministic.
    %
:- pred categorize_unify_var_lambda(mer_mode::in, list(mer_mode)::in,
    prog_var::in, list(prog_var)::in, pred_or_func::in,
    unify_rhs::in, unify_rhs::out, unification::in, unification::out,
    mode_info::in, mode_info::out) is det.

categorize_unify_var_lambda(ModeOfX, ArgModes0, X, ArgVars, PredOrFunc,
        RHS0, RHS, Unification0, Unification, !ModeInfo) :-
    % If we are re-doing mode analysis, preserve the existing cons_id.
    list.length(ArgVars, Arity),
    (
        Unification0 = construct(_, ConsId, _, _, _, _, SubInfo),
        (
            SubInfo = construct_sub_info(MaybeTakeAddr, _MaybeSize),
            expect(unify(MaybeTakeAddr, no), $module, $pred, "take_addr")
        ;
            SubInfo = no_construct_sub_info
        )
    ;
        Unification0 = deconstruct(_, ConsId, _, _, _, _),
        SubInfo = no_construct_sub_info
    ;
        ( Unification0 = assign(_, _)
        ; Unification0 = simple_test(_, _)
        ; Unification0 = complicated_unify(_, _, _)
        ),
        SubInfo = no_construct_sub_info,
        % The real cons_id will be computed by lambda.m;
        % we just put in a dummy one for now.
        TypeCtor = type_ctor(unqualified("int"), 0),
        ConsId = cons(unqualified("__LambdaGoal__"), Arity, TypeCtor)
    ),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    modes_to_uni_modes(ModuleInfo, ArgModes0, ArgModes0, ArgModes),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    ( mode_is_output(ModuleInfo, ModeOfX) ->
        (
            % If pred_consts are present, lambda expansion has already been
            % done. Rerunning mode analysis should not produce a lambda_goal
            % which cannot be directly converted back into a higher-order
            % predicate constant. If the instmap is not reachable, the call
            % may have been handled as an implied mode, since not_reached
            % is considered to be bound. In this case the lambda_goal may
            % not be converted back to a predicate constant, but that doesn't
            % matter since the code will be pruned away later by simplify.m.
            ConsId = closure_cons(ShroudedPredProcId, EvalMethod),
            instmap_is_reachable(InstMap)
        ->
            proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
            (
                RHS0 = rhs_lambda_goal(_, _, _, EvalMethod, _, _, _, _, Goal),
                Goal = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _)
            ->
                module_info_pred_info(ModuleInfo, PredId, PredInfo),
                PredModule = pred_info_module(PredInfo),
                PredName = pred_info_name(PredInfo),
                mode_info_get_var_types(!.ModeInfo, VarTypes),
                lookup_var_type(VarTypes, X, Type),
                ( Type = higher_order_type(_, MaybeReturnType, _, _) ->
                    (
                        MaybeReturnType = no,
                        RHSTypeCtor = type_ctor(unqualified("pred"), 0)
                    ;
                        MaybeReturnType = yes(_),
                        RHSTypeCtor = type_ctor(unqualified("func"), 0)
                    )
                ;
                    unexpected($module, $pred, "bad HO type")
                ),
                RHSConsId = cons(qualified(PredModule, PredName), Arity,
                    RHSTypeCtor),
                RHS = rhs_functor(RHSConsId, is_not_exist_constr, ArgVars)
            ;
                unexpected($module, $pred, "reintroduced lambda goal")
            )
        ;
            RHS = RHS0
        ),
        Unification = construct(X, ConsId, ArgVars, ArgModes,
            construct_dynamically, cell_is_unique, SubInfo)
    ; instmap_is_reachable(InstMap) ->
        % If it is a deconstruction, it is a mode error.
        % The error message would be incorrect in unreachable code,
        % since not_reached is considered bound.
        set_of_var.init(WaitingVars),
        mode_info_get_var_types(!.ModeInfo, VarTypes0),
        lookup_var_type(VarTypes0, X, Type),
        ModeError = mode_error_unify_pred(X,
            error_at_lambda(ArgVars, ArgModes0), Type, PredOrFunc),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        % Return any old garbage.
        Unification = Unification0,
        RHS = RHS0
    ;
        Unification = Unification0,
        RHS = RHS0
    ).

    % Categorize_unify_var_functor works out which category a unification
    % between a variable and a functor is - whether it is a construction
    % unification or a deconstruction. It also works out whether it will be
    % deterministic or semideterministic.
    %
:- pred categorize_unify_var_functor(mer_mode::in, list(mer_mode)::in,
    list(mer_mode)::in, prog_var::in, cons_id::in, list(prog_var)::in,
    vartypes::in, unify_context::in,
    unification::in, unification::out,
    mode_info::in, mode_info::out) is det.

categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ArgModes0,
        X, NewConsId, ArgVars, VarTypes, UnifyContext,
        Unification0, Unification, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    lookup_var_type(VarTypes, X, TypeOfX),
    % If we are re-doing mode analysis, preserve the existing cons_id.
    (
        Unification0 = construct(_, ConsIdPrime, _, _, _, _, SubInfo0),
        (
            SubInfo0 = construct_sub_info(MaybeTakeAddr, _MaybeSize0),
            expect(unify(MaybeTakeAddr, no), $module, $pred, "take_addr")
        ;
            SubInfo0 = no_construct_sub_info
        ),
        SubInfo = SubInfo0,
        ConsId = ConsIdPrime
    ;
        Unification0 = deconstruct(_, ConsIdPrime, _, _, _, _),
        SubInfo = no_construct_sub_info,
        ConsId = ConsIdPrime
    ;
        ( Unification0 = assign(_, _)
        ; Unification0 = simple_test(_, _)
        ; Unification0 = complicated_unify(_, _, _)
        ),
        SubInfo = no_construct_sub_info,
        ConsId = NewConsId
    ),
    modes_to_uni_modes(ModuleInfo, ModeOfXArgs, ArgModes0, ArgModes),
    ( mode_is_output(ModuleInfo, ModeOfX) ->
        % It is a construction.
        Unification = construct(X, ConsId, ArgVars, ArgModes,
            construct_dynamically, cell_is_unique, SubInfo),

        % For existentially quantified data types, check that any type_info
        % or type_class_info variables in the construction are ground.
        mode_info_set_call_context(call_context_unify(UnifyContext),
            !ModeInfo),
        check_type_info_args_are_ground(ArgVars, VarTypes, UnifyContext,
            !ModeInfo),
        mode_info_unset_call_context(!ModeInfo)
    ;
        % It is a deconstruction.
        (
            % If the variable was already known to be bound to a single
            % particular functor, then the unification either always succeeds
            % or always fails. In the latter case, the final inst will be
            % `not_reached' or `bound([])'. So if both the initial and final
            % inst are `bound([_])', then the unification must be
            % deterministic.
            mode_get_insts(ModuleInfo, ModeOfX, InitialInst0, FinalInst0),
            inst_expand(ModuleInfo, InitialInst0, InitialInst),
            inst_expand(ModuleInfo, FinalInst0, FinalInst),
            InitialInst = bound(_, _, [_]),
            FinalInst = bound(_, _, [_])
        ->
            CanFail = cannot_fail
        ;
            % If the type has only one constructor, then the unification
            % cannot fail.
            type_constructors(ModuleInfo, TypeOfX, Constructors),
            Constructors = [_]
        ->
            CanFail = cannot_fail
        ;
            % Otherwise, it can fail.
            CanFail = can_fail,
            mode_info_get_instmap(!.ModeInfo, InstMap0),
            (
                type_is_higher_order_details(TypeOfX, _, PredOrFunc, _, _),
                instmap_is_reachable(InstMap0)
            ->
                set_of_var.init(WaitingVars),
                ModeError = mode_error_unify_pred(X,
                    error_at_functor(ConsId, ArgVars), TypeOfX, PredOrFunc),
                mode_info_error(WaitingVars, ModeError, !ModeInfo)
            ;
                true
            )
        ),
        Unification = deconstruct(X, ConsId, ArgVars, ArgModes, CanFail,
            cannot_cgc)
    ).

    % Check that any type_info or type_class_info variables
    % in the argument list are ground.
    %
:- pred check_type_info_args_are_ground(list(prog_var)::in,
    vartypes::in, unify_context::in, mode_info::in, mode_info::out) is det.

check_type_info_args_are_ground([], _VarTypes, _UnifyContext, !ModeInfo).
check_type_info_args_are_ground([ArgVar | ArgVars], VarTypes, UnifyContext,
        !ModeInfo) :-
    (
        lookup_var_type(VarTypes, ArgVar, ArgType),
        is_introduced_type_info_type(ArgType)
    ->
        mode_info_set_call_arg_context(1, !ModeInfo),
        modecheck_introduced_type_info_var_has_inst_no_exact_match(ArgVar,
            ArgType, ground(shared, none), !ModeInfo),
        check_type_info_args_are_ground(ArgVars, VarTypes, UnifyContext,
            !ModeInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- type match_modes_result
    --->    possible_modes(list(proc_id))
    ;       ho_arg_not_ground.

:- type match_mode_result
    --->    ho_insts_match
    ;       ho_insts_do_not_match
    ;       ho_arg_not_ground.

:- pred match_modes_by_higher_order_insts(module_info::in, instmap::in,
    vartypes::in, prog_vars::in, pred_info::in, match_modes_result::out)
    is det.

match_modes_by_higher_order_insts(ModuleInfo, InstMap, VarTypes, ArgVars,
        CalleePredInfo, Result) :-
    CalleeProcIds = pred_info_procids(CalleePredInfo),
    match_modes_by_higher_order_insts_2(ModuleInfo, InstMap, VarTypes,
        ArgVars, CalleePredInfo, CalleeProcIds, [], Result).

:- pred match_modes_by_higher_order_insts_2(module_info::in, instmap::in,
    vartypes::in, prog_vars::in, pred_info::in, list(proc_id)::in,
    list(proc_id)::in, match_modes_result::out) is det.

match_modes_by_higher_order_insts_2(_, _, _, _, _,
        [], RevMatchedProcIds, Result) :-
    Result = possible_modes(list.reverse(RevMatchedProcIds)).
match_modes_by_higher_order_insts_2(ModuleInfo, InstMap, VarTypes,
        ArgVars, PredInfo, [ProcId | ProcIds], RevMatchedProcIds, Result) :-
    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    match_mode_by_higher_order_insts(ModuleInfo, InstMap, VarTypes, ArgVars,
        ArgModes, ProcResult),
    (
        ProcResult = ho_insts_match,
        match_modes_by_higher_order_insts_2(ModuleInfo, InstMap,
            VarTypes, ArgVars, PredInfo, ProcIds, [ProcId | RevMatchedProcIds],
            Result)
    ;
        ProcResult = ho_insts_do_not_match,
        match_modes_by_higher_order_insts_2(ModuleInfo, InstMap, VarTypes,
            ArgVars, PredInfo, ProcIds, RevMatchedProcIds, Result)
    ;
        ProcResult = ho_arg_not_ground,
        Result = ho_arg_not_ground
    ).

:- pred match_mode_by_higher_order_insts(module_info::in, instmap::in,
    vartypes::in, prog_vars::in, list(mer_mode)::in, match_mode_result::out)
    is det.

match_mode_by_higher_order_insts(_ModuleInfo, _InstMap, _VarTypes,
        [], _, ho_insts_match).
match_mode_by_higher_order_insts(ModuleInfo, InstMap, VarTypes,
        [Arg | Args], ArgModesList, Result) :-
    (
        ArgModesList = [ArgMode | ArgModes]
    ;
        ArgModesList = [],
        unexpected($module, $pred, "too many arguments")
    ),

    % For arguments with higher order initial insts, check if the variable in
    % that position has a matching inst. If the variable is free then we need
    % to delay the goal.
    Initial = mode_get_initial_inst(ModuleInfo, ArgMode),
    ( Initial = ground(_, higher_order(_)) ->
        instmap_lookup_var(InstMap, Arg, ArgInst),
        lookup_var_type(VarTypes, Arg, ArgType),
        ( inst_matches_initial(ArgInst, Initial, ArgType, ModuleInfo) ->
            match_mode_by_higher_order_insts(ModuleInfo, InstMap, VarTypes,
                Args, ArgModes, Result)
        ; not inst_is_ground(ModuleInfo, ArgInst) ->
            Result = ho_arg_not_ground
        ;
            Result = ho_insts_do_not_match
        )
    ;
        match_mode_by_higher_order_insts(ModuleInfo, InstMap, VarTypes, Args,
            ArgModes, Result)
    ).

%-----------------------------------------------------------------------------%

    % The call to bind_args below serves to update the insts of the
    % argument variables on the right hand side of the unification,
    % putting into them any information we can derive from the original
    % inst of the variable on the left hand side.
    %
    % Unfortunately, the update can be very expensive. For example,
    % for a ground list with N elements, there will be N variables
    % bound to the cons cells of the list. Since the average size of the
    % insts of these variables is proportional to N/2, the task
    % of recording all their insts is at least quadratic in N.
    % In practice, it can actually be worse, because of the way the code
    % called by bind_args works. It keeps track of sets of insts seen
    % so far, and checks new insts for membership of such sets.
    % If the initial elements of a list are repeated, then the membership
    % test can try to unify e.g. [a, a, a, a] with [], [a], [a, a]
    % and [a, a, a]. This means that each step of the quadratic algorithm
    % is itself quadratic, for an overall complexity of O(n^4).
    %
    % It is therefore crucial that we avoid calling bind_args if at all
    % possible.
    %
    % There are two cases in which we definitely know we can avoid
    % calling bind_args. First, if the variable on the left hand side, X,
    % is originally free, then it cannot change the already recorded insts
    % of the variables on the right hand side. Second, in from_ground_term
    % scopes, the variables on the right hand sides of construct
    % unifications are all local to the scope of the from_ground_term
    % scope. We can avoid updating their insts because no part of the
    % compiler will ever want to see their insts.
    %
    % We test for the first case first, because we expect it to be
    % much more common.
    %
:- pred bind_args_if_needed(mer_inst::in, mer_inst::in,
    list(prog_var)::in, list(mer_inst)::in,
    mode_info::in, mode_info::out) is det.

bind_args_if_needed(InstOfX, Inst, ArgVars, InstOfXArgs, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    ( inst_is_free(ModuleInfo, InstOfX) ->
        true
    ;
        mode_info_get_in_from_ground_term(!.ModeInfo, InFromGroundTerm),
        (
            InFromGroundTerm = in_from_ground_term_scope
        ;
            InFromGroundTerm = not_in_from_ground_term_scope,
            UnifyArgInsts = list.map(func(I) = yes(I), InstOfXArgs),
            bind_args(Inst, ArgVars, UnifyArgInsts, !ModeInfo)
        )
    ).

:- pred bind_args(mer_inst::in, list(prog_var)::in, list(maybe(mer_inst))::in,
    mode_info::in, mode_info::out) is det.

bind_args(Inst, Args, UnifyArgInsts, !ModeInfo) :-
    ( try_bind_args(Inst, Args, UnifyArgInsts, !ModeInfo) ->
        true
    ;
        unexpected($module, $pred, "try_bind_args failed")
    ).

:- pred try_bind_args(mer_inst::in, list(prog_var)::in,
    list(maybe(mer_inst))::in, mode_info::in, mode_info::out) is semidet.

try_bind_args(Inst, ArgVars, UnifyArgInsts, !ModeInfo) :-
    (
        Inst = not_reached,
        instmap.init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        Inst = ground(Uniq, none),
        ground_args(Uniq, ArgVars, UnifyArgInsts, !ModeInfo)
    ;
        Inst = bound(_Uniq, _InstResults, BoundInsts),
        (
            BoundInsts = [],
            % The code is unreachable.
            instmap.init_unreachable(InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo)
        ;
            BoundInsts = [bound_functor(_, ArgInsts)],
            try_bind_args_2(ArgVars, ArgInsts, UnifyArgInsts, !ModeInfo)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        try_bind_args(SubInst, ArgVars, UnifyArgInsts, !ModeInfo)
    ).

:- pred try_bind_args_2(list(prog_var)::in, list(mer_inst)::in,
    list(maybe(mer_inst))::in, mode_info::in, mode_info::out) is semidet.

try_bind_args_2([], [], [], !ModeInfo).
try_bind_args_2([Arg | Args], [Inst | Insts], [UnifyArgInst | UnifyArgInsts],
        !ModeInfo) :-
    modecheck_set_var_inst(Arg, Inst, UnifyArgInst, !ModeInfo),
    try_bind_args_2(Args, Insts, UnifyArgInsts, !ModeInfo).

:- pred ground_args(uniqueness::in, list(prog_var)::in,
    list(maybe(mer_inst))::in, mode_info::in, mode_info::out) is semidet.

ground_args(_Uniq, [], [], !ModeInfo).
ground_args(Uniq, [Arg | Args], [UnifyArgInst | UnifyArgInsts], !ModeInfo) :-
    modecheck_set_var_inst(Arg, ground(Uniq, none), UnifyArgInst, !ModeInfo),
    ground_args(Uniq, Args, UnifyArgInsts, !ModeInfo).

%-----------------------------------------------------------------------------%

    % get_mode_of_args(FinalInst, InitialArgInsts, ArgModes):
    %
    % For a var-functor unification, given the final inst of the var
    % and the initial insts of the functor arguments, compute the modes
    % of the functor arguments.
    %
:- pred get_mode_of_args(mer_inst::in, list(mer_inst)::in,
    list(mer_mode)::out) is det.

get_mode_of_args(Inst, ArgInsts, ArgModes) :-
    ( try_get_mode_of_args(Inst, ArgInsts, ArgModesPrime) ->
        ArgModes = ArgModesPrime
    ;
        unexpected($module, $pred, "try_get_mode_of_args failed")
    ).

:- pred try_get_mode_of_args(mer_inst::in, list(mer_inst)::in,
    list(mer_mode)::out) is semidet.

try_get_mode_of_args(Inst, ArgInsts, ArgModes) :-
    (
        Inst = not_reached,
        mode_set_args(ArgInsts, not_reached, ArgModes)
    ;
        Inst = any(Uniq, none),
        mode_set_args(ArgInsts, any(Uniq, none), ArgModes)
    ;
        Inst = ground(Uniq, none),
        mode_set_args(ArgInsts, ground(Uniq, none), ArgModes)
    ;
        Inst = bound(_Uniq, _InstResults, BoundInsts),
        (
            BoundInsts = [],
            % The code is unreachable.
            mode_set_args(ArgInsts, not_reached, ArgModes)
        ;
            BoundInsts = [bound_functor(_Name, FunctorArgInsts)],
            pair_up_insts(ArgInsts, FunctorArgInsts, ArgModes)
        )
    ;
        Inst = constrained_inst_vars(_, SubInst),
        try_get_mode_of_args(SubInst, ArgInsts, ArgModes)
    ).

:- pred pair_up_insts(list(mer_inst)::in, list(mer_inst)::in,
    list(mer_mode)::out) is det.

pair_up_insts([], [], []).
pair_up_insts([], [_ | _], _) :-
    unexpected($module, $pred, "mismatched list lengths").
pair_up_insts([_ | _], [], _) :-
    unexpected($module, $pred, "mismatched list lengths").
pair_up_insts([InstA | InstsA], [InstB | InstsB], [Mode | Modes]) :-
    Mode = (InstA -> InstB),
    pair_up_insts(InstsA, InstsB, Modes).

:- pred mode_set_args(list(mer_inst)::in, mer_inst::in, list(mer_mode)::out)
    is det.

mode_set_args([], _, []).
mode_set_args([Inst | Insts], FinalInst, [Mode | Modes]) :-
    Mode = (Inst -> FinalInst),
    mode_set_args(Insts, FinalInst, Modes).

%-----------------------------------------------------------------------------%

:- func init_instmap_may_have_subtype(mode_info) = bool.

init_instmap_may_have_subtype(ModeInfo) = MayHaveSubtype :-
    mode_info_get_initial_instmap(ModeInfo, InitialInstMap),
    instmap_to_assoc_list(InitialInstMap, InitVarsInsts),
    assoc_list.values(InitVarsInsts, InitInsts),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    MayRestrictList =
        list.map(inst_may_restrict_cons_ids(ModuleInfo), InitInsts),
    bool.or_list(MayRestrictList, MayHaveSubtype).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_unify.
%-----------------------------------------------------------------------------%
