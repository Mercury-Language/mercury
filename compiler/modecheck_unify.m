%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
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

:- import_module check_hlds__mode_info.
:- import_module hlds__hlds_goal.
:- import_module parse_tree__prog_data.

:- import_module io.

    % Modecheck a unification.
    %
:- pred modecheck_unification(prog_var::in, unify_rhs::in, unification::in,
    unify_context::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

    % Create a unification between the two given variables.
    % The goal's mode and determinism information are not filled in.
    %
:- pred modecheck_unify__create_var_var_unification(prog_var::in, prog_var::in,
    (type)::in, mode_info::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__inst_util.
:- import_module check_hlds__mode_debug.
:- import_module check_hlds__mode_errors.
:- import_module check_hlds__mode_info.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__modecheck_call.
:- import_module check_hlds__modes.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__type_util.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__unify_proc.
:- import_module check_hlds__unique_modes.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module hlds__make_hlds.
:- import_module hlds__quantification.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

modecheck_unification(X, var(Y), Unification0, UnifyContext,
        UnifyGoalInfo0, Unify, !ModeInfo, !IO) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap__lookup_var(InstMap0, X, InstOfX0),
    instmap__lookup_var(InstMap0, Y, InstOfY0),
    % If X and Y are free and have a solver type and we are allowed to
    % insert initialisation calls at this point, then do so to allow
    % scheduling of the unification.
    (
        mode_info_may_initialise_solver_vars(!.ModeInfo),
        InstOfX0   = free,
        InstOfY0   = free,
        VarType    = VarTypes^elem(X),
        type_util__type_is_solver_type(ModuleInfo0, VarType)
    ->
        modes__construct_initialisation_call(X, VarType, any_inst,
            context_init, no, InitXGoal, !ModeInfo),
        MaybeInitX = yes(InitXGoal),
        instmap__set(X, any_inst, InstMap0, InstMap),
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
        ( LiveX = live, LiveY = live ->
            BothLive = live
        ;
            BothLive = dead
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
            Det, UnifyContext, VarTypes, Unification0, Unify0, !ModeInfo),
        (
            MaybeInitX = no,
            Unify = Unify0
        ;
            MaybeInitX = yes(InitGoal - InitGoalInfo),
            modes__compute_goal_instmap_delta(InstMap, Unify0,
                UnifyGoalInfo0, UnifyGoalInfo, !ModeInfo),
            Unify = conj([InitGoal - InitGoalInfo, Unify0 - UnifyGoalInfo])
        )
    ;
        set__list_to_set([X, Y], WaitingVars),
        mode_info_error(WaitingVars,
            mode_error_unify_var_var(X, Y, InstOfX, InstOfY), !ModeInfo),
            % If we get an error, set the inst to not_reached to suppress
            % follow-on errors. But don't call categorize_unification, because
            % that could cause an invalid call to `unify_proc__request_unify'
        Inst = not_reached,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        modecheck_set_var_inst(Y, Inst, no, !ModeInfo),
            % Return any old garbage.
        Unification = assign(X, Y),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Modes = ModeOfX - ModeOfY,
        Unify = unify(X, var(Y), Modes, Unification, UnifyContext)
    ).

modecheck_unification(X0, functor(ConsId0, IsExistConstruction, ArgVars0),
        Unification0, UnifyContext, GoalInfo0, Goal, !ModeInfo, !IO) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
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
        type_is_higher_order(TypeOfX, Purity, _, EvalMethod,
            PredArgTypes),
        ConsId0 = pred_const(ShroudedPredProcId, _)
    ->
        % Convert the pred term to a lambda expression.
        mode_info_get_varset(!.ModeInfo, VarSet0),
        mode_info_get_context(!.ModeInfo, Context),
        proc(PredId, ProcId) =
            unshroud_pred_proc_id(ShroudedPredProcId),
        convert_pred_to_lambda_goal(Purity, EvalMethod,
            X0, PredId, ProcId, ArgVars0, PredArgTypes,
            UnifyContext, GoalInfo0, Context,
            ModuleInfo0, Functor0,
            VarSet0, VarSet, VarTypes0, VarTypes),
        mode_info_set_varset(VarSet, !ModeInfo),
        mode_info_set_var_types(VarTypes, !ModeInfo),

        % Modecheck this unification in its new form.
        modecheck_unification(X0, Functor0, Unification0, UnifyContext,
            GoalInfo0, Goal, !ModeInfo, !IO)
    ;
        % It's not a higher-order pred unification - just
        % call modecheck_unify_functor to do the ordinary thing.
        modecheck_unify_functor(X0, TypeOfX, ConsId0,
            IsExistConstruction, ArgVars0, Unification0,
            UnifyContext, GoalInfo0, Goal, !ModeInfo, !IO)
    ).

modecheck_unification(X, LambdaGoal, Unification0, UnifyContext, _GoalInfo,
        unify(X, RHS, Mode, Unification, UnifyContext), !ModeInfo, !IO) :-
    LambdaGoal = lambda_goal(Purity, PredOrFunc, EvalMethod, _,
        ArgVars, Vars, Modes0, Det, Goal0),

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
    % The reason that we need to merge the original and final instmaps is
    % as follows. The lambda goal will not have bound any variables (since
    % they were locked), but it may have added some information or lost some
    % uniqueness. We cannot use the final instmap, because that may have
    % too much information. If we use the initial instmap, variables will be
    % considered as unique even if they become shared or clobbered in the
    % lambda goal!
    %
    % However even this may not be enough.  If a unique non-local variable
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

    ( HowToCheckGoal = check_modes ->
        % This only needs to be done once.
        mode_info_get_types_of_vars(!.ModeInfo, Vars, VarTypes),
        propagate_types_into_mode_list(ModuleInfo0, VarTypes, Modes0, Modes)
    ;
        Modes = Modes0
    ),

    % Initialize the initial insts of the lambda variables.
    mode_list_get_initial_insts(ModuleInfo0, Modes, VarInitialInsts),
    assoc_list__from_corresponding_lists(Vars, VarInitialInsts, VarInstAL),
    instmap_delta_from_assoc_list(VarInstAL, VarInstMapDelta),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap__apply_instmap_delta(InstMap0, VarInstMapDelta, InstMap1),
    mode_info_set_instmap(InstMap1, !ModeInfo),

    % Mark the non-clobbered lambda variables as live.
    get_arg_lives(ModuleInfo0, Modes, ArgLives),
    get_live_vars(Vars, ArgLives, LiveVarsList),
    set__list_to_set(LiveVarsList, LiveVars),
    mode_info_add_live_vars(LiveVars, !ModeInfo),

    % Lock the non-locals. (A lambda goal is not allowed to bind any of the
    % non-local variables, since it could get called more than once, or
    % from inside a negation)
    Goal0 = _ - GoalInfo0,
    goal_info_get_nonlocals(GoalInfo0, NonLocals0),
    set__delete_list(NonLocals0, Vars, NonLocals),
    set__to_sorted_list(NonLocals, NonLocalsList),
    instmap__lookup_vars(NonLocalsList, InstMap1, NonLocalInsts),
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

        % XXX This test is also not conservative enough!
        %
        % We should not allow non-local vars to have inst `any'; because that
        % can lead to unsoundness. However, disallowing that idiom would break
        % extras/trailed_update/samples/vqueens.m, and would make freeze/3
        % basically useless... so for now at least, let's not disallow it,
        % even though it is unsafe.

        inst_list_is_ground_or_any(NonLocalInsts, ModuleInfo2)
    ->
        make_shared_inst_list(NonLocalInsts, SharedNonLocalInsts,
            ModuleInfo2, ModuleInfo3),
        instmap__set_vars(NonLocalsList, SharedNonLocalInsts,
            InstMap1, InstMap2),
        mode_info_set_module_info(ModuleInfo3, !ModeInfo),
        mode_info_set_instmap(InstMap2, !ModeInfo),

        mode_info_lock_vars(lambda(PredOrFunc), NonLocals, !ModeInfo),

        mode_checkpoint(enter, "lambda goal", !ModeInfo, !IO),
        % If we're being called from unique_modes.m, then we need to
        % call unique_modes__check_goal rather than modecheck_goal.
        (
            HowToCheckGoal = check_unique_modes
        ->
            unique_modes__check_goal(Goal0, Goal1, !ModeInfo, !IO)
        ;
            modecheck_goal(Goal0, Goal1, !ModeInfo, !IO)
        ),
        mode_list_get_final_insts(ModuleInfo0, Modes, FinalInsts),
        modecheck_lambda_final_insts(Vars, FinalInsts, Goal1, Goal, !ModeInfo),
        mode_checkpoint(exit, "lambda goal", !ModeInfo, !IO),

        mode_info_remove_live_vars(LiveVars, !ModeInfo),
        mode_info_unlock_vars(lambda(PredOrFunc), NonLocals, !ModeInfo),

        % Ensure that the non-local vars are shared OUTSIDE the
        % lambda unification as well as inside.

        instmap__set_vars(NonLocalsList, SharedNonLocalInsts,
            InstMap0, InstMap11),
        mode_info_set_instmap(InstMap11, !ModeInfo),

        % Now modecheck the unification of X with the lambda-expression.

        RHS0 = lambda_goal(Purity, PredOrFunc, EvalMethod,
            modes_are_ok, ArgVars, Vars, Modes, Det, Goal),
        modecheck_unify_lambda(X, PredOrFunc, ArgVars, Modes, Det,
            RHS0, RHS, Unification0, Unification, Mode, !ModeInfo)
    ;
        list__filter((pred(Var :: in) is semidet :-
                instmap__lookup_var(InstMap1, Var, Inst),
                \+ inst_is_ground(ModuleInfo2, Inst)
            ), NonLocalsList, NonGroundNonLocals),
        ( NonGroundNonLocals = [BadVar | _] ->
            instmap__lookup_var(InstMap1, BadVar, BadInst),
            set__singleton_set(WaitingVars, BadVar),
            mode_info_error(WaitingVars,
                mode_error_non_local_lambda_var(BadVar, BadInst), !ModeInfo)
        ;
            unexpected(this_file,
                "modecheck_unification(lambda): very strange var")
        ),
            % Return any old garbage.
        RHS = lambda_goal(Purity, PredOrFunc, EvalMethod, modes_are_ok,
            ArgVars, Vars, Modes0, Det, Goal0),
        Mode = (free -> free) - (free -> free),
        Unification = Unification0
    ).

:- pred modecheck_unify_lambda(prog_var::in, pred_or_func::in,
    list(prog_var)::in, list(mode)::in, determinism::in,
    unify_rhs::in, unify_rhs::out, unification::in, unification::out,
    pair(mode)::out, mode_info::in, mode_info::out) is det.

modecheck_unify_lambda(X, PredOrFunc, ArgVars, LambdaModes, LambdaDet,
        RHS0, RHS, Unification0, Unification, Mode, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap__lookup_var(InstMap0, X, InstOfX),
    InstOfY = ground(unique, higher_order(LambdaPredInfo)),
    LambdaPredInfo = pred_inst_info(PredOrFunc, LambdaModes, LambdaDet),
    (
        abstractly_unify_inst(dead, InstOfX, InstOfY, real_unify,
            UnifyInst, _Det, ModuleInfo0, ModuleInfo1)
    ->
        Inst = UnifyInst,
        mode_info_set_module_info(ModuleInfo1, !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
        % the lambda expression just maps its argument variables
        % from their current insts to the same inst
        instmap__lookup_vars(ArgVars, InstMap0, ArgInsts),
        inst_lists_to_mode_list(ArgInsts, ArgInsts, ArgModes),
        categorize_unify_var_lambda(ModeOfX, ArgModes, X, ArgVars, PredOrFunc,
            RHS0, RHS, Unification0, Unification, !ModeInfo),
        modecheck_set_var_inst(X, Inst, no, !ModeInfo)
    ;
        set__list_to_set([X], WaitingVars),
        mode_info_error(WaitingVars,
            mode_error_unify_var_lambda(X, InstOfX, InstOfY),
            !ModeInfo),
        % If we get an error, set the inst to not_reached to avoid cascading
        % errors. But don't call categorize_unification, because that could
        % cause an invalid call to `unify_proc__request_unify'
        Inst = not_reached,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
            % Return any old garbage.
        Unification = Unification0,
        RHS = RHS0
    ).

:- pred modecheck_unify_functor(prog_var::in, (type)::in, cons_id::in,
    is_existential_construction::in, list(prog_var)::in, unification::in,
    unify_context::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

modecheck_unify_functor(X0, TypeOfX, ConsId0, IsExistConstruction, ArgVars0,
        Unification0, UnifyContext, GoalInfo0, Goal, !ModeInfo, !IO) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    mode_info_get_how_to_check(!.ModeInfo, HowToCheckGoal),

    % Fully module qualify all cons_ids (except for builtins such as
    % ints and characters).

    qualify_cons_id(TypeOfX, ArgVars0, ConsId0, ConsId, InstConsId),

    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap__lookup_var(InstMap0, X0, InstOfX0),
    (
        % If the unification was originally of the form X = 'new f'(Y),
        % it must be classified as a construction. If it were classified as a
        % deconstruction, the argument unifications would be ill-typed.
        IsExistConstruction = yes,
        \+ inst_is_free(ModuleInfo0, InstOfX0)
    ->
        % To make sure the unification is classified as a construction,
        % if X is already bound, we must add a unification with an extra
        % variable:
        %   Z = 'new f'(Y),
        %   X = Z.

        InstOfX = free,
        LiveX = live,
        make_complicated_sub_unify(X0, X, ExtraGoals0, !ModeInfo)
    ;
        InstOfX = InstOfX0,
        X = X0,
        mode_info_var_is_live(!.ModeInfo, X, LiveX),
        ExtraGoals0 = no_extra_goals
    ),

        % This needs to come after make_complicated_sub_unify because
        % make_complicated_sub_unify may introduce new variables
        % whose types we need to look-up.
        %
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    (
        % If we are allowed to insert solver type initialisation calls and
        % InstOfX0 is free and all ArgVars0 are either non-free or have
        % solver types, then we know that this is going to be a construction,
        % so we can insert the necessary initialisation calls.
        ArgVars0 \= [],
        HowToCheckGoal \= check_unique_modes,
        inst_match__inst_is_free(ModuleInfo0, InstOfX),
        mode_info_may_initialise_solver_vars(!.ModeInfo),
        instmap__lookup_vars(ArgVars0, InstMap0, InstArgs0),
        all_arg_vars_are_non_free_or_solver_vars(ArgVars0, InstArgs0,
            VarTypes, ModuleInfo0, ArgVarsToInit)
    ->
        modes__construct_initialisation_calls(ArgVarsToInit, InitGoals,
            !ModeInfo),
        (
            InitGoals = [],
            ExtraGoals1 = no_extra_goals
        ;
            InitGoals = [_ | _],
            ExtraGoals1 = extra_goals(InitGoals, [])
        )
    ;
        ExtraGoals1 = no_extra_goals
    ),
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    instmap__lookup_vars(ArgVars0, InstMap1, InstArgs),
    mode_info_var_list_is_live(!.ModeInfo, ArgVars0, LiveArgs),
    InstOfY = bound(unique, [functor(InstConsId, InstArgs)]),
    (
        % The occur check: X = f(X) is considered a mode error unless X is
        % ground. (Actually it wouldn't be that hard to generate code for it
        % - it always fails! - but it's most likely to be a programming error,
        % so it's better to report it.)

        list__member(X, ArgVars0),
        \+ inst_is_ground(ModuleInfo0, InstOfX)
    ->
        set__list_to_set([X], WaitingVars),
        mode_info_error(WaitingVars,
            mode_error_unify_var_functor(X, InstConsId, ArgVars0,
                InstOfX, InstArgs),
            !ModeInfo),
        Inst = not_reached,
        Det = erroneous,
            % If we get an error, set the inst to not_reached to avoid
            % cascading errors. But don't call categorize_unification, because
            % that could cause an invalid call to `unify_proc__request_unify'.
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        NoArgInsts = list__duplicate(length(ArgVars0), no),
        bind_args(Inst, ArgVars0, NoArgInsts, !ModeInfo),
            % Return any old garbage.
        Unification = Unification0,
        ArgVars = ArgVars0,
        ExtraGoals2 = no_extra_goals
    ;
            % XXX We forbid the construction of partially instantiated
            % structures involving solver types. We'd like to forbid all
            % such constructions here, but that causes trouble with the current
            % implementation of term.term_to_univ_special_case which does
            % use partial instantiation (in a rather horrible way). This is
            % a hacky solution that gets us most of what we want w.r.t.
            % solver types.
        not (
            inst_is_free(ModuleInfo0, InstOfX),
            list__member(InstArg, InstArgs),
            inst_is_free(ModuleInfo0, InstArg),
            list__member(ArgVar, ArgVars0),
            ArgType = VarTypes ^ elem(ArgVar),
            type_is_solver_type(ModuleInfo0, ArgType)
        ),
        abstractly_unify_inst_functor(LiveX, InstOfX, InstConsId,
            InstArgs, LiveArgs, real_unify, TypeOfX,
            UnifyInst, Det1, ModuleInfo0, ModuleInfo1)
    ->
        Inst = UnifyInst,
        Det = Det1,
        mode_info_set_module_info(ModuleInfo1, !ModeInfo),
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
        ( get_mode_of_args(Inst, InstArgs, ModeArgs0) ->
            ModeArgs = ModeArgs0
        ;
            unexpected(this_file, "get_mode_of_args failed")
        ),
        (
            inst_expand_and_remove_constrained_inst_vars(ModuleInfo1,
                InstOfX, InstOfX1),
            list__length(ArgVars0, Arity),
            get_arg_insts(InstOfX1, InstConsId, Arity, InstOfXArgs0),
            get_mode_of_args(Inst, InstOfXArgs0, ModeOfXArgs0)
        ->
            ModeOfXArgs = ModeOfXArgs0,
            InstOfXArgs = InstOfXArgs0
        ;
            unexpected(this_file, "get_(inst/mode)_of_args failed")
        ),
        categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ModeArgs,
            X, ConsId, ArgVars0, VarTypes, UnifyContext,
            Unification0, Unification1, !ModeInfo),
        split_complicated_subunifies(Unification1, Unification,
            ArgVars0, ArgVars, ExtraGoals2, !ModeInfo),
        modecheck_set_var_inst(X, Inst, yes(InstOfY), !ModeInfo),
        UnifyArgInsts = list__map(func(I) = yes(I), InstOfXArgs),
        bind_args(Inst, ArgVars, UnifyArgInsts, !ModeInfo)
    ;
        set__list_to_set([X | ArgVars0], WaitingVars), % conservative
        mode_info_error(WaitingVars,
            mode_error_unify_var_functor(X, InstConsId, ArgVars0,
                InstOfX, InstArgs),
            !ModeInfo),
            % If we get an error, set the inst to not_reached to avoid
            % cascading errors. But don't call categorize_unification, because
            % that could cause an invalid call to `unify_proc__request_unify'.
        Inst = not_reached,
        Det = erroneous,
        ModeOfX = (InstOfX -> Inst),
        ModeOfY = (InstOfY -> Inst),
        Mode = ModeOfX - ModeOfY,
        modecheck_set_var_inst(X, Inst, no, !ModeInfo),
        NoArgInsts = list__duplicate(length(ArgVars0), no),
        bind_args(Inst, ArgVars0, NoArgInsts, !ModeInfo),
            % Return any old garbage.
        Unification = Unification0,
        ArgVars = ArgVars0,
        ExtraGoals2 = no_extra_goals
    ),

    %
    % Optimize away construction of unused terms by replacing the unification
    % with `true'. Optimize % away unifications which always fail by replacing
    % them with `fail'.
    %
    (
        Unification = construct(_, _, _, _, _, _, _),
        LiveX = dead
    ->
        Goal = conj([])
    ;
        Det = failure
    ->
        % This optimisation is safe because the only way that we can analyse
        % a unification as having no solutions is that the unification always
        % fails.
        %
        % Unifying two preds is not erroneous as far as the mode checker
        % is concerned, but a mode _error_.
        Goal = disj([]),
        InitMayHaveSubtype = init_instmap_may_have_subtype(!.ModeInfo),
        (
            InitMayHaveSubtype = yes
            % Suppress the warning, since the unification may succeed
            % in another mode in which the initial inst of X,
            % or of another head variable that is unified with it,
            % is not so constrained.
        ;
            InitMayHaveSubtype = no,
            Warning = cannot_succeed_var_functor(X, InstOfX, ConsId),
            mode_info_warning(Warning, !ModeInfo)
        )
    ;
        Functor = functor(ConsId, IsExistConstruction, ArgVars),
        Unify = unify(X, Functor, Mode, Unification, UnifyContext),
        %
        % Modecheck_unification sometimes needs to introduce new goals
        % to handle complicated sub-unifications in deconstructions.
        % The only time this can happen during unique mode analysis is if
        % the instmap is unreachable, since inst_is_bound succeeds for
        % not_reached. (If it did in other cases, the code would be wrong
        % since it wouldn't have the correct determinism annotations.)
        %
        append_extra_goals(ExtraGoals0, ExtraGoals1, ExtraGoals01),
        append_extra_goals(ExtraGoals01, ExtraGoals2, ExtraGoals),
        (
            HowToCheckGoal = check_unique_modes,
            ExtraGoals \= no_extra_goals,
            instmap__is_reachable(InstMap1)
        ->
            unexpected(this_file,
                "re-modecheck of unification " ++
                "encountered complicated sub-unifies")
        ;
            true
        ),
        handle_extra_goals(Unify, ExtraGoals, GoalInfo0,
            [X0 | ArgVars0], [X | ArgVars], InstMap0, Goal, !ModeInfo, !IO)
    ).

:- pred all_arg_vars_are_non_free_or_solver_vars(list(prog_var)::in,
    list(inst)::in, map(prog_var, type)::in, module_info::in,
    list(prog_var)::out) is semidet.

all_arg_vars_are_non_free_or_solver_vars([], [], _, _, []).

all_arg_vars_are_non_free_or_solver_vars([], [_|_], _, _, _) :-
    unexpected(this_file,
        "modecheck_unify.all_arg_vars_are_non_free_or_solver_vars: " ++
        "mismatch in list lengths").

all_arg_vars_are_non_free_or_solver_vars([_|_], [], _, _, _) :-
    unexpected(this_file,
        "modecheck_unify.all_arg_vars_are_non_free_or_solver_vars: " ++
        "mismatch in list lengths").

all_arg_vars_are_non_free_or_solver_vars([Arg | Args], [Inst | Insts],
        VarTypes, ModuleInfo, ArgsToInit) :-
    ( inst_match__inst_is_free(ModuleInfo, Inst) ->
        type_is_solver_type(ModuleInfo, VarTypes ^ elem(Arg)),
        all_arg_vars_are_non_free_or_solver_vars(Args, Insts,
            VarTypes, ModuleInfo, ArgsToInit1),
        ArgsToInit = [Arg | ArgsToInit1]
    ;
        all_arg_vars_are_non_free_or_solver_vars(Args, Insts,
            VarTypes, ModuleInfo, ArgsToInit)
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
            unexpected(this_file, "split_complicated_subunifies_2 failed")
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
    map__lookup(VarTypes0, Var0, VarType),
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
    varset__new_var(VarSet0, Var, VarSet),
    map__lookup(VarTypes0, Var0, VarType),
    map__set(VarTypes0, Var, VarType, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo),

    modecheck_unify__create_var_var_unification(Var0, Var,
        VarType, !.ModeInfo, ExtraGoal),

    % Insert the new unification at the start of the extra goals.
    ExtraGoals0 = extra_goals([], [ExtraGoal]).

modecheck_unify__create_var_var_unification(Var0, Var, Type, ModeInfo,
        Goal - GoalInfo) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    mode_context_to_unify_context(ModeInfo, ModeContext, UnifyContext),
    UnifyContext = unify_context(MainContext, SubContexts),

    create_atomic_unification(Var0, var(Var), Context,
        MainContext, SubContexts, Goal0 - GoalInfo0),

    %
    % Compute the goal_info nonlocal vars for the newly created goal
    % (excluding the type_info vars -- they are added below).
    % N.B. This may overestimate the set of non-locals,
    % but that shouldn't cause any problems.
    %
    set__list_to_set([Var0, Var], NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
    goal_info_set_context(Context, GoalInfo1, GoalInfo2),

    %
    % Look up the map(tvar, type_info_locn) in the proc_info,
    % since it is needed by polymorphism__unification_typeinfos.
    %
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_predid(ModeInfo, PredId),
    mode_info_get_procid(ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    proc_info_rtti_varmaps(ProcInfo, RttiVarMaps),

    %
    % Call polymorphism__unification_typeinfos to add the appropriate
    % type-info and type-class-info variables to the nonlocals
    % and to the unification.
    %
    (
        Goal0 = unify(X, Y, Mode, Unification0, FinalUnifyContext)
    ->
        polymorphism__unification_typeinfos(Type, RttiVarMaps,
            Unification0, Unification, GoalInfo2, GoalInfo),
        Goal = unify(X, Y, Mode, Unification, FinalUnifyContext)
    ;
        unexpected(this_file, "modecheck_unify__create_var_var_unification")
    ).

%-----------------------------------------------------------------------------%

    % categorize_unify_var_var works out which category a unification
    % between a variable and another variable expression is - whether it is
    % an assignment, a simple test or a complicated unify.
    %
:- pred categorize_unify_var_var((mode)::in, (mode)::in,
    is_live::in, is_live::in, prog_var::in,
    prog_var::in, determinism::in, unify_context::in,
    map(prog_var, type)::in, unification::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y, Det,
        UnifyContext, VarTypes, Unification0, Unify, !ModeInfo) :-
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
        ( LiveX = dead ->
            Unification = assign(X, Y)
        ; LiveY = dead ->
            Unification = assign(Y, X)
        ;
            unexpected(this_file, "categorize_unify_var_var: free-free unify!")
        )
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
        Unification = simple_test(X, Y)
    ;
        map__lookup(VarTypes, X, Type),
        (
            type_is_atomic(Type, ModuleInfo0),
            not type_has_user_defined_equality_pred(ModuleInfo0, Type, _)
        ->
            Unification = simple_test(X, Y)
        ;
            modecheck_complicated_unify(X, Y, Type, ModeOfX, ModeOfY, Det,
                UnifyContext, Unification0, Unification, !ModeInfo)
        )
    ),
    %
    % Optimize away unifications with dead variables and simple tests that
    % cannot fail by replacing them with `true'. (The optimization of simple
    % tests is necessary because otherwise determinism analysis assumes they
    % can fail. The optimization of assignments to dead variables may be
    % necessary to stop the code generator from getting confused.)
    %
    % Optimize away unifications which always fail by replacing them with
    % `fail'.
    %
    (
        Unification = assign(AssignTarget, _),
        mode_info_var_is_live(!.ModeInfo, AssignTarget, dead)
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
        % This optimisation is safe because the only way that we can analyse
        % a unification as having no solutions is that the unification
        % always fails.
        %
        % Unifying two preds is not erroneous as far as the
        % mode checker is concerned, but a mode _error_.
        Unify = disj([]),
        InitMayHaveSubtype = init_instmap_may_have_subtype(!.ModeInfo),
        (
            InitMayHaveSubtype = yes
            % Suppress the warning, since the unification may succeed
            % in another mode in which the initial inst of X or Y,
            % or of another head variable that is unified with one of them,
            % is not so constrained.
        ;
            InitMayHaveSubtype = no,
            mode_get_insts(ModuleInfo0, ModeOfX, InstOfX, _),
            mode_get_insts(ModuleInfo0, ModeOfY, InstOfY, _),
            Warning = cannot_succeed_var_var(X, Y, InstOfX, InstOfY),
            mode_info_warning(Warning, !ModeInfo)
        )
    ;
        Unify = unify(X, var(Y), ModeOfX - ModeOfY, Unification, UnifyContext)
    ).

    % Modecheck_complicated_unify does some extra checks that are needed
    % for mode-checking complicated unifications.
    %
:- pred modecheck_complicated_unify(prog_var::in, prog_var::in,
    (type)::in, (mode)::in, (mode)::in, determinism::in, unify_context::in,
    unification::in, unification::out,
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
        unexpected(this_file, "modecheck_complicated_unify")
    ),
    Unification = complicated_unify(UniMode, CanFail, UnifyTypeInfoVars),

    % Check that all the type_info or type_class_info variables used
    % by the polymorphic unification are ground.
    (
        % Optimize common case.
        UnifyTypeInfoVars = []
    ;
        UnifyTypeInfoVars = [_ | _],
        list__length(UnifyTypeInfoVars, NumTypeInfoVars),
        list__duplicate(NumTypeInfoVars, ground(shared, none), ExpectedInsts),
        mode_info_set_call_context(unify(UnifyContext), !ModeInfo),
        NeedExactMatch = no,
        InitialArgNum = 0,
        modecheck_var_has_inst_list(UnifyTypeInfoVars, ExpectedInsts,
            NeedExactMatch, InitialArgNum, _InstVarSub, !ModeInfo),
            % we can ignore _InstVarSub since type_info variables
            % should not have variable insts.
        mode_info_unset_call_context(!ModeInfo)
    ),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo3),
    (
        mode_info_get_errors(!.ModeInfo, Errors),
        Errors \= []
    ->
        true
    ;
        % Check that we're not trying to do a polymorphic unification
        % in a mode other than (in, in).
        % [Actually we also allow `any' insts, since the (in, in)
        % mode of unification for types which have `any' insts must
        % also be able to handle (in(any), in(any)) unifications.]
        Type = variable(_, _),
        \+ inst_is_ground_or_any(ModuleInfo3, InitialInstX)
    ->
        set__singleton_set(WaitingVars, X),
        mode_info_error(WaitingVars, mode_error_poly_unify(X, InitialInstX),
            !ModeInfo)
    ;
        Type = variable(_, _),
        \+ inst_is_ground_or_any(ModuleInfo3, InitialInstY)
    ->
        set__singleton_set(WaitingVars, Y),
        mode_info_error(WaitingVars, mode_error_poly_unify(Y, InitialInstY),
            !ModeInfo)
    ;

        % Check that we're not trying to do a higher-order unification.
        type_is_higher_order(Type, _, PredOrFunc, _, _)
    ->
        % We do not want to report this as an error if it occurs in a
        % compiler-generated predicate - instead, we delay the error
        % until runtime so that it only occurs if the compiler-generated
        % predicate gets called. not_reached is considered bound, so the
        % error message would be spurious if the instmap is unreachable.
        mode_info_get_predid(!.ModeInfo, PredId),
        module_info_pred_info(ModuleInfo3, PredId, PredInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap0),
        (
            ( is_unify_or_compare_pred(PredInfo)
            ; instmap__is_unreachable(InstMap0)
            )
        ->
            true
        ;
            set__init(WaitingVars),
            mode_info_error(WaitingVars,
                mode_error_unify_pred(X, error_at_var(Y), Type, PredOrFunc),
                !ModeInfo)
        )
    ;
        % Ensure that we will generate code for the unification procedure
        % that will be used to implement this complicated unification.
        type_to_ctor_and_args(Type, TypeCtor, _)
    ->
        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_instvarset(!.ModeInfo, InstVarSet),
        unify_proc__request_unify(TypeCtor - UniMode, InstVarSet,
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
:- pred categorize_unify_var_lambda((mode)::in, list(mode)::in,
    prog_var::in, list(prog_var)::in, pred_or_func::in,
    unify_rhs::in, unify_rhs::out, unification::in, unification::out,
    mode_info::in, mode_info::out) is det.

categorize_unify_var_lambda(ModeOfX, ArgModes0, X, ArgVars, PredOrFunc,
        RHS0, RHS, Unification0, Unification, !ModeInfo) :-
    % If we are re-doing mode analysis, preserve the existing cons_id.
    list__length(ArgVars, Arity),
    (
        Unification0 = construct(_, ConsId0, _, _, _, _, SubInfo0)
    ->
        (
            SubInfo0 = construct_sub_info(MaybeTakeAddr, _MaybeSize),
            require(unify(MaybeTakeAddr, no),
                "categorize_unify_var_lambda: take_addr")
        ;
            SubInfo0 = no_construct_sub_info
        ),
        SubInfo = SubInfo0,
        ConsId = ConsId0
    ;
        Unification0 = deconstruct(_, ConsId1, _, _, _, _)
    ->
        SubInfo = no_construct_sub_info,
        ConsId = ConsId1
    ;
        % The real cons_id will be computed by lambda.m;
        % we just put in a dummy one for now.
        SubInfo = no_construct_sub_info,
        ConsId = cons(unqualified("__LambdaGoal__"), Arity)
    ),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_util__modes_to_uni_modes(ModuleInfo, ArgModes0, ArgModes0, ArgModes),
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
            ConsId = pred_const(ShroudedPredProcId, EvalMethod),
            instmap__is_reachable(InstMap)
        ->
            proc(PredId, ProcId) =
                unshroud_pred_proc_id(ShroudedPredProcId),
            (
                RHS0 = lambda_goal(_, _, EvalMethod, _,
                    _, _, _, _, Goal),
                Goal = call(PredId, ProcId, _, _, _, _) - _
            ->
                module_info_pred_info(ModuleInfo,
                    PredId, PredInfo),
                PredModule = pred_info_module(PredInfo),
                PredName = pred_info_name(PredInfo),
                RHS = functor(cons(qualified(PredModule, PredName), Arity),
                    no, ArgVars)
            ;
                unexpected(this_file,
                    "categorize_unify_var_lambda - reintroduced lambda goal")
            )
        ;
            RHS = RHS0
        ),
        Unification = construct(X, ConsId, ArgVars, ArgModes,
            construct_dynamically, cell_is_unique, SubInfo)
    ; instmap__is_reachable(InstMap) ->
        % If it's a deconstruction, it is a mode error.
        % The error message would be incorrect in unreachable code,
        % since not_reached is considered bound.
        set__init(WaitingVars),
        mode_info_get_var_types(!.ModeInfo, VarTypes0),
        map__lookup(VarTypes0, X, Type),
        mode_info_error(WaitingVars,
            mode_error_unify_pred(X, error_at_lambda(ArgVars, ArgModes0),
                Type, PredOrFunc),
            !ModeInfo),
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

:- pred categorize_unify_var_functor((mode)::in, list(mode)::in,
    list(mode)::in, prog_var::in, cons_id::in, list(prog_var)::in,
    map(prog_var, type)::in, unify_context::in,
    unification::in, unification::out,
    mode_info::in, mode_info::out) is det.

categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ArgModes0,
        X, NewConsId, ArgVars, VarTypes, UnifyContext,
        Unification0, Unification, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    map__lookup(VarTypes, X, TypeOfX),
    % If we are re-doing mode analysis, preserve the existing cons_id.
    (
        Unification0 = construct(_, ConsId0, _, _, _, _, SubInfo0)
    ->
        (
            SubInfo0 = construct_sub_info(MaybeTakeAddr, _MaybeSize0),
            require(unify(MaybeTakeAddr, no),
                "categorize_unify_var_functor: take_addr")
        ;
            SubInfo0 = no_construct_sub_info
        ),
        SubInfo = SubInfo0,
        ConsId = ConsId0
    ;
        Unification0 = deconstruct(_, ConsId1, _, _, _, _)
    ->
        SubInfo = no_construct_sub_info,
        ConsId = ConsId1
    ;
        SubInfo = no_construct_sub_info,
        ConsId = NewConsId
    ),
    mode_util__modes_to_uni_modes(ModuleInfo, ModeOfXArgs,
        ArgModes0, ArgModes),
    ( mode_is_output(ModuleInfo, ModeOfX) ->
        % It's a construction.
        Unification = construct(X, ConsId, ArgVars, ArgModes,
            construct_dynamically, cell_is_unique, SubInfo),

        % For existentially quantified data types, check that any type_info
        % or type_class_info variables in the construction are ground.
        check_type_info_args_are_ground(ArgVars, VarTypes,
            UnifyContext, !ModeInfo)
    ;
        % It's a deconstruction.
        (
            % If the variable was already known to be bound to a single
            % particular functor, then the unification either always succeeds
            % or always fails. In the latter case, the final inst will be
            % `not_reached' or `bound([])'. So if both the initial and final
            % inst are `bound([_])', then the unification must be
            % deterministic.
            mode_get_insts(ModuleInfo, ModeOfX,
                InitialInst0, FinalInst0),
            inst_expand(ModuleInfo, InitialInst0, InitialInst),
            inst_expand(ModuleInfo, FinalInst0, FinalInst),
            InitialInst = bound(_, [_]),
            FinalInst = bound(_, [_])
        ->
            CanFail = cannot_fail
        ;
            % If the type has only one constructor, then the unification
            % cannot fail.
            type_constructors(TypeOfX, ModuleInfo, Constructors),
            Constructors = [_]
        ->
            CanFail = cannot_fail
        ;
            % Otherwise, it can fail.
            CanFail = can_fail,
            mode_info_get_instmap(!.ModeInfo, InstMap0),
            (
                type_is_higher_order(TypeOfX, _, PredOrFunc, _, _),
                instmap__is_reachable(InstMap0)
            ->
                set__init(WaitingVars),
                mode_info_error(WaitingVars,
                    mode_error_unify_pred(X, error_at_functor(ConsId, ArgVars),
                        TypeOfX, PredOrFunc),
                    !ModeInfo)
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
    map(prog_var, type)::in, unify_context::in,
    mode_info::in, mode_info::out) is det.

check_type_info_args_are_ground([], _VarTypes, _UnifyContext, !ModeInfo).
check_type_info_args_are_ground([ArgVar | ArgVars], VarTypes, UnifyContext,
        !ModeInfo) :-
    (
        map__lookup(VarTypes, ArgVar, ArgType),
        is_introduced_type_info_type(ArgType)
    ->
        mode_info_set_call_context(unify(UnifyContext), !ModeInfo),
        NeedExactMatch = no,
        InitialArgNum = 0,
        modecheck_var_has_inst_list([ArgVar], [ground(shared, none)],
            NeedExactMatch, InitialArgNum, _InstVarSub, !ModeInfo),
        check_type_info_args_are_ground(ArgVars, VarTypes, UnifyContext,
            !ModeInfo),
        mode_info_unset_call_context(!ModeInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred bind_args((inst)::in, list(prog_var)::in, list(maybe(inst))::in,
    mode_info::in, mode_info::out) is det.

bind_args(Inst, Args, UnifyArgInsts, !ModeInfo) :-
    ( try_bind_args(Inst, Args, UnifyArgInsts, !ModeInfo) ->
        true
    ;
        unexpected(this_file, "bind_args: try_bind_args failed")
    ).

:- pred try_bind_args((inst)::in, list(prog_var)::in, list(maybe(inst))::in,
    mode_info::in, mode_info::out) is semidet.

try_bind_args(not_reached, _, _, !ModeInfo) :-
    instmap__init_unreachable(InstMap),
    mode_info_set_instmap(InstMap, !ModeInfo).
try_bind_args(ground(Uniq, none), Args, UnifyArgInsts, !ModeInfo) :-
    ground_args(Uniq, Args, UnifyArgInsts, !ModeInfo).
try_bind_args(bound(_Uniq, List), Args, UnifyArgInsts, !ModeInfo) :-
    (
        List = [],
        % The code is unreachable.
        instmap__init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        List = [_ | _],
        List = [functor(_, InstList)],
        try_bind_args_2(Args, InstList, UnifyArgInsts, !ModeInfo)
    ).
try_bind_args(constrained_inst_vars(_, Inst), Args, UnifyArgInsts,
        !ModeInfo) :-
    try_bind_args(Inst, Args, UnifyArgInsts, !ModeInfo).

:- pred try_bind_args_2(list(prog_var)::in, list(inst)::in,
    list(maybe(inst))::in, mode_info::in, mode_info::out) is semidet.

try_bind_args_2([], [], [], !ModeInfo).
try_bind_args_2([Arg | Args], [Inst | Insts], [UnifyArgInst | UnifyArgInsts],
        !ModeInfo) :-
    modecheck_set_var_inst(Arg, Inst, UnifyArgInst, !ModeInfo),
    try_bind_args_2(Args, Insts, UnifyArgInsts, !ModeInfo).

:- pred ground_args(uniqueness::in, list(prog_var)::in, list(maybe(inst))::in,
    mode_info::in, mode_info::out) is semidet.

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
:- pred get_mode_of_args((inst)::in, list(inst)::in, list(mode)::out)
    is semidet.

get_mode_of_args(not_reached, ArgInsts, ArgModes) :-
    mode_set_args(ArgInsts, not_reached, ArgModes).
get_mode_of_args(any(Uniq), ArgInsts, ArgModes) :-
    mode_set_args(ArgInsts, any(Uniq), ArgModes).
get_mode_of_args(ground(Uniq, none), ArgInsts, ArgModes) :-
    mode_set_args(ArgInsts, ground(Uniq, none), ArgModes).
get_mode_of_args(bound(_Uniq, List), ArgInstsA, ArgModes) :-
    (
        List = [],
        % The code is unreachable.
        mode_set_args(ArgInstsA, not_reached, ArgModes)
    ;
        List = [_ | _],
        List = [functor(_Name, ArgInstsB)],
        get_mode_of_args_2(ArgInstsA, ArgInstsB, ArgModes)
    ).
get_mode_of_args(constrained_inst_vars(_, Inst), ArgInsts, ArgModes) :-
    get_mode_of_args(Inst, ArgInsts, ArgModes).

:- pred get_mode_of_args_2(list(inst)::in, list(inst)::in, list(mode)::out)
    is semidet.

get_mode_of_args_2([], [], []).
get_mode_of_args_2([InstA | InstsA], [InstB | InstsB], [Mode | Modes]) :-
    Mode = (InstA -> InstB),
    get_mode_of_args_2(InstsA, InstsB, Modes).

:- pred mode_set_args(list(inst)::in, (inst)::in, list(mode)::out) is det.

mode_set_args([], _, []).
mode_set_args([Inst | Insts], FinalInst, [Mode | Modes]) :-
    Mode = (Inst -> FinalInst),
    mode_set_args(Insts, FinalInst, Modes).

%-----------------------------------------------------------------------------%

:- func init_instmap_may_have_subtype(mode_info) = bool.

init_instmap_may_have_subtype(ModeInfo) = MayHaveSubtype :-
    mode_info_get_initial_instmap(ModeInfo, InitialInstMap),
    instmap__to_assoc_list(InitialInstMap, InitVarsInsts),
    assoc_list__values(InitVarsInsts, InitInsts),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    MayRestrictList =
        list__map(inst_may_restrict_cons_ids(ModuleInfo), InitInsts),
    bool__or_list(MayRestrictList, MayHaveSubtype).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "modecheck_unify.m".

%-----------------------------------------------------------------------------%
