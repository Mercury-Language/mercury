%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modecheck_conj.m.
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.modecheck_conj.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.

:- import_module list.

:- pred modecheck_conj_list(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.mode_debug.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_goal.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

modecheck_conj_list(ConjType, Goals0, Goals, !ModeInfo) :-
    mode_info_get_errors(!.ModeInfo, OldErrors),
    mode_info_set_errors([], !ModeInfo),

    mode_info_get_may_init_solver_vars(!.ModeInfo, OldMayInit),

    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
    delay_info_enter_conj(DelayInfo0, DelayInfo1),
    mode_info_set_delay_info(DelayInfo1, !ModeInfo),

    mode_info_get_live_vars(!.ModeInfo, LiveVars1),
    mode_info_add_goals_live_vars(ConjType, Goals0, !ModeInfo),

    % Try to schedule goals without inserting any solver initialisation calls
    % by setting the mode_info flag may_initialise_solver_vars to no.
    mode_info_set_may_init_solver_vars(may_not_init_solver_vars, !ModeInfo),

    modecheck_conj_list_flatten_and_schedule(ConjType, Goals0, Goals1,
        [], RevImpurityErrors0, !ModeInfo),

    mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
    delay_info_leave_conj(DelayInfo2, DelayedGoals0, DelayInfo3),
    mode_info_set_delay_info(DelayInfo3, !ModeInfo),

    % Otherwise try scheduling by inserting solver initialisation calls
    % where necessary (although only if `--solver-type-auto-init' is enabled).
    %
    modecheck_delayed_solver_goals(ConjType, Goals2,
        DelayedGoals0, DelayedGoals, RevImpurityErrors0, RevImpurityErrors,
        !ModeInfo),
    Goals = Goals1 ++ Goals2,

    mode_info_get_errors(!.ModeInfo, NewErrors),
    Errors = OldErrors ++ NewErrors,
    mode_info_set_errors(Errors, !ModeInfo),

    % We only report impurity errors if there were no other errors.
    (
        DelayedGoals = [],

        % Report all the impurity errors
        % (making sure we report the errors in the correct order).
        list.reverse(RevImpurityErrors, ImpurityErrors),
        mode_info_get_errors(!.ModeInfo, Errors5),
        Errors6 = Errors5 ++ ImpurityErrors,
        mode_info_set_errors(Errors6, !ModeInfo)
    ;
        DelayedGoals = [FirstDelayedGoal | MoreDelayedGoals],
        % The variables in the delayed goals should no longer be considered
        % live (the conjunction itself will delay, and its nonlocals will be
        % made live).
        mode_info_set_live_vars(LiveVars1, !ModeInfo),
        (
            MoreDelayedGoals = [],
            FirstDelayedGoal = delayed_goal(_DVars, Error, _DGoal),
            mode_info_add_error(Error, !ModeInfo)
        ;
            MoreDelayedGoals = [_ | _],
            get_all_waiting_vars(DelayedGoals, Vars),
            ModeError = mode_error_conj(DelayedGoals, conj_floundered),
            mode_info_error(Vars, ModeError, !ModeInfo)
        )
    ),
    % Restore the value of the may_initialise_solver_vars flag.
    mode_info_set_may_init_solver_vars(OldMayInit, !ModeInfo).

:- type impurity_errors == list(mode_error_info).

    % Flatten conjunctions as we go, as long as they are of the same type.
    % Call modecheck_conj_list_schedule to do the actual scheduling.
    %
    % NOTE: In some rare cases, when the compiler is compled in a in hlc grade
    % and the conjunction is particularly large, the mutual recursion between
    % modecheck_conj_list_flatten_and_schedule and modecheck_conj_list_schedule
    % can exhaust the C stack, causing a core dump. This happens e.g. when
    % compiling zm_enums.m in a compiler in which polymorphism's reuse of
    % inserted typeinfo and related variables has been disabled.
    %
:- pred modecheck_conj_list_flatten_and_schedule(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_conj_list_flatten_and_schedule(_ConjType, [], [],
        !ImpurityErrors, !ModeInfo).
modecheck_conj_list_flatten_and_schedule(ConjType, [Goal0 | Goals0], Goals,
        !ImpurityErrors, !ModeInfo) :-
    (
        Goal0 = hlds_goal(conj(plain_conj, ConjGoals), _),
        ConjType = plain_conj
    ->
        Goals1 = ConjGoals ++ Goals0,
        modecheck_conj_list_flatten_and_schedule(ConjType, Goals1, Goals,
            !ImpurityErrors, !ModeInfo)
    ;
        % We attempt to schedule the first goal in the conjunction.
        % If successful, we try to wake up pending goals (if any), and if not,
        % we delay the goal. Then we continue attempting to schedule
        % all the rest of the conjuncts.

        Purity = goal_get_purity(Goal0),
        (
            Purity = purity_impure,
            Impure = yes,
            check_for_impurity_error(Goal0, ScheduledSolverGoals,
                !ImpurityErrors, !ModeInfo)
        ;
            ( Purity = purity_pure
            ; Purity = purity_semipure
            ),
            Impure = no,
            ScheduledSolverGoals = []
        ),

        % Hang onto the original instmap, delay_info, and live_vars.
        mode_info_get_instmap(!.ModeInfo, InstMap0),
        mode_info_get_delay_info(!.ModeInfo, DelayInfo0),

        % Modecheck the goal, noting first that the non-locals
        % which occur in the goal might not be live anymore.
        NonLocalVars = goal_get_nonlocals(Goal0),
        mode_info_remove_live_vars(NonLocalVars, !ModeInfo),
        modecheck_goal(Goal0, Goal, !ModeInfo),

        % Now see whether the goal was successfully scheduled. If we didn't
        % manage to schedule the goal, then we restore the original instmap,
        % delay_info and livevars here, and delay the goal.
        mode_info_get_errors(!.ModeInfo, Errors),
        (
            Errors = [FirstErrorInfo | _],
            mode_info_set_errors([], !ModeInfo),
            mode_info_set_instmap(InstMap0, !ModeInfo),
            mode_info_add_live_vars(NonLocalVars, !ModeInfo),
            delay_info_delay_goal(FirstErrorInfo, Goal0,
                DelayInfo0, DelayInfo1),
            % Delaying an impure goal is an impurity error.
            (
                Impure = yes,
                FirstErrorInfo = mode_error_info(Vars, _, _, _),
                ImpureError = mode_error_conj(
                    [delayed_goal(Vars, FirstErrorInfo, Goal0)],
                    goal_itself_was_impure),
                mode_info_get_context(!.ModeInfo, Context),
                mode_info_get_mode_context(!.ModeInfo, ModeContext),
                ImpureErrorInfo = mode_error_info(Vars, ImpureError,
                    Context, ModeContext),
                !:ImpurityErrors = [ImpureErrorInfo | !.ImpurityErrors]
            ;
                Impure = no
            )
        ;
            Errors = [],
            mode_info_get_delay_info(!.ModeInfo, DelayInfo1)
        ),

        % Next, we attempt to wake up any pending goals, and then continue
        % scheduling the rest of the goal.
        delay_info_wakeup_goals(WokenGoals, DelayInfo1, DelayInfo),
        Goals1 = WokenGoals ++ Goals0,
        (
            WokenGoals = []
        ;
            WokenGoals = [_],
            mode_checkpoint(wakeup, "goal", !ModeInfo)
        ;
            WokenGoals = [_, _ | _],
            mode_checkpoint(wakeup, "goals", !ModeInfo)
        ),
        mode_info_set_delay_info(DelayInfo, !ModeInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        ( instmap_is_unreachable(InstMap) ->
            % We should not mode-analyse the remaining goals, since they are
            % unreachable. Instead we optimize them away, so that later passes
            % won't complain about them not having mode information.
            mode_info_remove_goals_live_vars(Goals1, !ModeInfo),
            Goals2  = []
        ;
            % The remaining goals may still need to be flattened.
            modecheck_conj_list_flatten_and_schedule(ConjType, Goals1, Goals2,
                !ImpurityErrors, !ModeInfo)
        ),
        (
            Errors = [_ | _],
            % We delayed this goal -- it will be stored in the delay_info.
            Goals = ScheduledSolverGoals ++ Goals2
        ;
            Errors = [],
            % We successfully scheduled this goal, so insert it
            % in the list of successfully scheduled goals.
            % We flatten out conjunctions if we can. They can arise
            % when Goal0 was a scope(from_ground_term, _) goal.
            ( Goal = hlds_goal(conj(ConjType, SubGoals), _) ->
                Goals = ScheduledSolverGoals ++ SubGoals ++ Goals2
            ;
                Goals = ScheduledSolverGoals ++ [Goal | Goals2]
            )
        )
    ).

    % We may still have some unscheduled goals. This may be because some
    % initialisation calls are needed to turn some solver type vars
    % from inst free to inst any. This predicate attempts to schedule
    % such goals.
    %
    % XXX Despite its name this predicate will in fact try to reschedule all
    % delayed goals, not just delayed solver goals.
    %
:- pred modecheck_delayed_solver_goals(conj_type::in, list(hlds_goal)::out,
    list(delayed_goal)::in, list(delayed_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_delayed_solver_goals(ConjType, Goals, !DelayedGoals,
        !ImpurityErrors, !ModeInfo) :-
    % Try to handle any unscheduled goals by inserting solver
    % initialisation calls, aiming for a deterministic schedule.
    modecheck_delayed_goals_try_det(ConjType, !DelayedGoals,
        Goals0, !ImpurityErrors, !ModeInfo),

    % Try to handle any unscheduled goals by inserting solver
    % initialisation calls, aiming for *any* workable schedule.
    modecheck_delayed_goals_eager(ConjType, !DelayedGoals,
        Goals1, !ImpurityErrors, !ModeInfo),
    Goals = Goals0 ++ Goals1.

    % We may still have some unscheduled goals.  This may be because some
    % initialisation calls are needed to turn some solver type vars
    % from inst free to inst any.  This pass attempts to identify a
    % minimal subset of such vars to initialise that will allow the
    % remaining goals to be scheduled in a deterministic fashion.
    %
    % This works as follows.  If a deterministic schedule exists for
    % the remaining goals, then each subgoal must also be deterministic.
    % Moreover, no call may employ an implied mode since these mean
    % introducing a semidet unification.  Therefore we only need to
    % consider det procs for calls, constructions for var/functor
    % unifications, and assignments for var/var unifications.
    %
    % If a consistent deterministic schedule exists then every
    % variable involved in the goals either
    % - has already been instantiated;
    % - will be instantiated by a single remaining subgoal;
    % - will not be instantiated by any remaining subgoal.
    % Variables in this last category that are solver type variables
    % should be initialised.  If all the variables that will remain
    % uninstantiated are in this last category then, after inserting
    % initialisation call, we should expect another attempt at
    % scheduling the remaining goals to succeed and produce a
    % deterministic result.
    %
    % XXX At some point we should extend this analysis to handle
    % disjunction, if-then-else goals, and negation.
    %
:- pred modecheck_delayed_goals_try_det(conj_type::in, list(delayed_goal)::in,
    list(delayed_goal)::out, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_delayed_goals_try_det(ConjType, DelayedGoals0, DelayedGoals, Goals,
        !ImpurityErrors, !ModeInfo) :-
    (
        % There are no unscheduled goals, so we don't need to do anything.

        DelayedGoals0 = [],
        DelayedGoals  = [],
        Goals         = []
    ;
        % There are some unscheduled goals. See if allowing extra
        % initialisation calls (for a single goal) makes a difference.

        DelayedGoals0 = [_ | _],
        (
            % Extract the HLDS goals from the delayed goals.
            Goals0 = list.map(hlds_goal_from_delayed_goal, DelayedGoals0),

            % Work out which vars are already instantiated
            % (i.e. have non-free insts).
            mode_info_get_instmap(!.ModeInfo, InstMap),
            instmap_to_assoc_list(InstMap, VarInsts),
            NonFreeVars0 = set_of_var.list_to_set(
                non_free_vars_in_assoc_list(VarInsts)),

            % Find the set of vars whose instantiation should lead to
            % a deterministic schedule.
            promise_equivalent_solutions [CandidateInitVars] (
                candidate_init_vars(!.ModeInfo, Goals0, NonFreeVars0,
                    CandidateInitVars)
            ),

            % And verify that all of these vars are solver type vars
            % (and can therefore be initialised.)
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            mode_info_get_var_types(!.ModeInfo, VarTypes),
            all [Var] (
                set_of_var.member(CandidateInitVars, Var)
            =>
                (
                    map.lookup(VarTypes, Var, VarType),
                    type_is_solver_type_with_auto_init(ModuleInfo, VarType)
                )
            ),
            mode_info_solver_init_is_supported(!.ModeInfo)
        ->
            % Construct the inferred initialisation goals
            % and try scheduling again.
            CandidateInitVarList =
                set_of_var.to_sorted_list(CandidateInitVars),
            construct_initialisation_calls(CandidateInitVarList,
                InitGoals, !ModeInfo),
            Goals1 = InitGoals ++ Goals0,

            mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
            delay_info_enter_conj(DelayInfo0, DelayInfo1),
            mode_info_set_delay_info(DelayInfo1, !ModeInfo),

            mode_info_add_goals_live_vars(ConjType, InitGoals, !ModeInfo),

            modecheck_conj_list_flatten_and_schedule(ConjType, Goals1, Goals,
                !ImpurityErrors, !ModeInfo),

            mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
            delay_info_leave_conj(DelayInfo2, DelayedGoals, DelayInfo3),
            mode_info_set_delay_info(DelayInfo3, !ModeInfo)
        ;
            % We couldn't identify a deterministic solution.
            DelayedGoals = DelayedGoals0,
            Goals        = []
        )
    ).

    % XXX will this catch synonyms for `free'?
    % N.B. This is perhaps the only time when `for' and `free'
    % can be juxtaposed grammatically :-)
    %
:- func non_free_vars_in_assoc_list(assoc_list(prog_var, mer_inst)) =
    list(prog_var).

non_free_vars_in_assoc_list([]) = [].
non_free_vars_in_assoc_list([Var - Inst | AssocList]) =
    (
        ( Inst = free
        ; Inst = free(_)
        )
    ->
        non_free_vars_in_assoc_list(AssocList)
    ;
        [Var | non_free_vars_in_assoc_list(AssocList)]
    ).

    % Find a set of vars that, if they were instantiated, might
    % lead to a deterministic scheduling of the given goals.
    %
    % This approximation is fairly crude: it only considers variables as
    % being free or non-free, rather than having detailed insts.
    %
    % XXX Does not completely handle negation, disjunction, if_then_else
    % goals, foreign_code, or var/lambda unifications.
    %
:- pred candidate_init_vars(mode_info::in, list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is cc_nondet.

candidate_init_vars(ModeInfo, Goals, NonFreeVars0, CandidateVars) :-
    CandidateVars0 = set_of_var.init,
    candidate_init_vars_2(ModeInfo, Goals, NonFreeVars0, NonFreeVars1,
        CandidateVars0, CandidateVars1),
    CandidateVars = set_of_var.difference(CandidateVars1, NonFreeVars1).

:- pred candidate_init_vars_2(mode_info::in, list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is nondet.

candidate_init_vars_2(ModeInfo, Goals, !NonFree, !CandidateVars) :-
    list.foldl2(candidate_init_vars_3(ModeInfo), Goals,
        !NonFree, !CandidateVars).

:- pred candidate_init_vars_3(mode_info::in, hlds_goal::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is nondet.

candidate_init_vars_3(ModeInfo, Goal, !NonFree, !CandidateVars) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        % A var/var unification.
        GoalExpr = unify(X, RHS, _, _, _),
        RHS  = rhs_var(Y),
        ( set_of_var.member(!.NonFree, X) ->
            not set_of_var.member(!.NonFree, Y),
            % It is an assignment from X to Y.
            set_of_var.insert(Y, !NonFree)
        ; set_of_var.member(!.NonFree, Y) ->
            % It is an assignment from Y to X.
            set_of_var.insert(X, !NonFree)
        ;
            % It is an assignment one way or the other.
            (
                set_of_var.insert(X, !NonFree),
                set_of_var.insert(Y, !CandidateVars)
            ;
                set_of_var.insert(Y, !NonFree),
                set_of_var.insert(X, !CandidateVars)
            )
        )
    ;
        % A var/functor unification, which can only be deterministic
        % if it is a construction.
        GoalExpr = unify(X, RHS, _, _, _),
        RHS  = rhs_functor(_, _, Args),

        % If this is a construction then X must be free.
        not set_of_var.member(!.NonFree, X),

        % But X becomes instantiated.
        set_of_var.insert(X, !NonFree),

        % And the Args are potential candidates for initialisation.
        set_of_var.insert_list(Args, !CandidateVars)
    ;
        % A var/lambda unification, which can only be deterministic if it is
        % a construction.
        GoalExpr = unify(X, RHS, _, _, _),
        RHS  = rhs_lambda_goal(_, _, _, _, _, _, _, _, _),

        % If this is a construction then X must be free.
        not set_of_var.member(!.NonFree, X),

        % But X becomes instantiated.
        set_of_var.insert(X, !NonFree)
    ;
        % Disjunctions are tricky, because we don't perform switch analysis
        % until after mode analysis. So here we assume that the disjunction
        % is a det switch and that we can ignore it for the purposes of
        % identifying candidate vars for initialisation.
        GoalExpr = disj(_Goals)
    ;
        % We ignore the condition of an if-then-else goal, other than to assume
        % that it binds its non-solver-type non-locals, but proceed on the
        % assumption that the then and else arms are det. This isn't very
        % accurate and may need refinement.

        GoalExpr = if_then_else(_LocalVars, CondGoal, ThenGoal, ElseGoal),

        CondGoal = hlds_goal(_CondGoalExpr, CondGoalInfo),
        NonLocals = goal_info_get_nonlocals(CondGoalInfo),
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        mode_info_get_var_types(ModeInfo, VarTypes),
        NonSolverNonLocals =
            set_of_var.filter(non_solver_var(ModuleInfo, VarTypes), NonLocals),
        set_of_var.union(NonSolverNonLocals, !NonFree),

        candidate_init_vars_3(ModeInfo, ThenGoal, !.NonFree, NonFreeThen,
            !CandidateVars),
        candidate_init_vars_3(ModeInfo, ElseGoal, !.NonFree, NonFreeElse,
            !CandidateVars),
        set_of_var.union(NonFreeThen, NonFreeElse, !:NonFree)
    ;
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        GoalExpr = scope(_, SubGoal),
        candidate_init_vars_3(ModeInfo, SubGoal, !NonFree, !CandidateVars)
    ;
        GoalExpr = conj(_ConjType, Goals),
        candidate_init_vars_2(ModeInfo, Goals, !NonFree, !CandidateVars)
    ;
        % We assume that generic calls are deterministic. The modes field of
        % higher_order calls is junk until *after* mode analysis, hence we
        % can't handle them here.
        GoalExpr = generic_call(Details, Args, ArgModes, _JunkArgRegs,
            _JunkDetism),
        Details \= higher_order(_, _, _, _),
        candidate_init_vars_call(ModeInfo, Args, ArgModes,
            !NonFree, !CandidateVars)
    ;
        % A call (at this point the ProcId is just a dummy value since it isn't
        % meaningful until the call is scheduled.)

        GoalExpr = plain_call(PredId, _, Args, _, _, _),

        % Find a deterministic proc for this call.
        mode_info_get_preds(ModeInfo, Preds),
        map.lookup(Preds, PredId, PredInfo),
        pred_info_get_procedures(PredInfo, ProcTable),
        map.values(ProcTable, ProcInfos),
        list.member(ProcInfo, ProcInfos),
        proc_info_get_declared_determinism(ProcInfo, yes(DeclaredDetism)),
        ( DeclaredDetism = detism_det ; DeclaredDetism = detism_cc_multi ),

        % Find the argument modes.
        proc_info_get_argmodes(ProcInfo, ArgModes),

        % Process the call args.
        candidate_init_vars_call(ModeInfo, Args, ArgModes,
            !NonFree, !CandidateVars)
    ).

    % This filter pred succeeds if the given variable does not have
    % a solver type.
    %
:- pred non_solver_var(module_info::in, vartypes::in, prog_var::in) is semidet.

non_solver_var(ModuleInfo, VarTypes, Var) :-
    VarType = VarTypes ^ det_elem(Var),
    not type_is_solver_type(ModuleInfo, VarType).

    % Update !NonFree and !CandidateVars given the args and modes for a call.
    %
:- pred candidate_init_vars_call(mode_info::in,
    list(prog_var)::in, list(mer_mode)::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is semidet.

candidate_init_vars_call(_ModeInfo, [], [], !NonFree, !CandidateVars).
candidate_init_vars_call(ModeInfo, [Arg | Args], [Mode | Modes],
        !NonFree, !CandidateVars) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_get_insts_semidet(ModuleInfo, Mode, InitialInst, FinalInst),
    (
        InitialInst \= free,
        InitialInst \= free(_)
    ->
        % This arg is an input that needs instantiation.
        set_of_var.insert(Arg, !CandidateVars)
    ;
        % Otherwise this arg could be an output...
        FinalInst \= free,
        FinalInst \= free(_)
    ->
        % And it is.
        ( set_of_var.contains(!.NonFree, Arg) ->
            % This arg appears in an implied mode.
            fail
        ;
            % This arg is instantiated on output.
            set_of_var.insert(Arg, !NonFree)
        )
    ;
        % This arg is unused.
        true
    ),
    candidate_init_vars_call(ModeInfo, Args, Modes, !NonFree, !CandidateVars).

    % We may still have some unscheduled goals. This may be because some
    % initialisation calls are needed to turn some solver type vars
    % from inst free to inst any. This pass tries to unblock the
    % remaining goals by conservatively inserting initialisation calls.
    % It is "eager" in the sense that as soon as it encounters a sub-goal
    % that may be unblocked this way it tries to do so.
    %
:- pred modecheck_delayed_goals_eager(conj_type::in, list(delayed_goal)::in,
    list(delayed_goal)::out, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_delayed_goals_eager(ConjType, DelayedGoals0, DelayedGoals, Goals,
        !ImpurityErrors, !ModeInfo) :-
    (
        % There are no unscheduled goals, so we don't need to do anything.
        DelayedGoals0 = [],
        DelayedGoals  = [],
        Goals         = []
    ;
        % There are some unscheduled goals. See if allowing extra
        % initialisation calls (for a single goal) makes a difference.
        DelayedGoals0 = [_ | _],

        Goals0 = list.map(hlds_goal_from_delayed_goal, DelayedGoals0),

        mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
        delay_info_enter_conj(DelayInfo0, DelayInfo1),
        mode_info_set_delay_info(DelayInfo1, !ModeInfo),

        mode_info_get_may_init_solver_vars(!.ModeInfo, OldMayInit),
        expect(unify(OldMayInit, may_not_init_solver_vars), $module, $pred,
            "may init solver vars"),
        mode_info_set_may_init_solver_vars(may_init_solver_vars, !ModeInfo),
        modecheck_conj_list_flatten_and_schedule(ConjType, Goals0, Goals1,
            !ImpurityErrors, !ModeInfo),
        mode_info_set_may_init_solver_vars(may_not_init_solver_vars,
            !ModeInfo),

        mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
        delay_info_leave_conj(DelayInfo2, DelayedGoals1, DelayInfo3),
        mode_info_set_delay_info(DelayInfo3, !ModeInfo),

        % See if we scheduled any goals.
        ( length(DelayedGoals1) < length(DelayedGoals0) ->
            % We scheduled some goals. Keep going until we either
            % flounder or succeed.
            modecheck_delayed_goals_eager(ConjType,
                DelayedGoals1, DelayedGoals, Goals2,
                !ImpurityErrors, !ModeInfo),
            Goals = Goals1 ++ Goals2
        ;
            DelayedGoals = DelayedGoals1,
            Goals = Goals1
        )
    ).

:- func hlds_goal_from_delayed_goal(delayed_goal) = hlds_goal.

hlds_goal_from_delayed_goal(delayed_goal(_WaitingVars, _ModeError, Goal)) =
    Goal.

    % Check whether there are any delayed goals (other than unifications)
    % at the point where we are about to schedule an impure goal. If so,
    % that is an error. Headvar unifications are allowed to be delayed
    % because in the case of output arguments, they cannot be scheduled until
    % the variable value is known. If headvar unifications couldn't be delayed
    % past impure goals, impure predicates wouldn't be able to have outputs!
    % (Note that we first try to schedule any delayed solver goals waiting
    % for initialisation.)
    %
:- pred check_for_impurity_error(hlds_goal::in, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

check_for_impurity_error(Goal, Goals, !ImpurityErrors, !ModeInfo) :-
    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
    delay_info_leave_conj(DelayInfo0, DelayedGoals0, DelayInfo1),
    mode_info_set_delay_info(DelayInfo1, !ModeInfo),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_pred_id(!.ModeInfo, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_headvar_list(ClausesInfo, HeadVars),
    filter_headvar_unification_goals(HeadVars, DelayedGoals0,
        HeadVarUnificationGoals, NonHeadVarUnificationGoals0),
    modecheck_delayed_solver_goals(plain_conj, Goals,
        NonHeadVarUnificationGoals0, NonHeadVarUnificationGoals,
        !ImpurityErrors, !ModeInfo),
    mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
    delay_info_enter_conj(DelayInfo2, DelayInfo3),
    redelay_goals(HeadVarUnificationGoals, DelayInfo3, DelayInfo),
    mode_info_set_delay_info(DelayInfo, !ModeInfo),
    (
        NonHeadVarUnificationGoals = []
    ;
        NonHeadVarUnificationGoals = [_ | _],
        get_all_waiting_vars(NonHeadVarUnificationGoals, Vars),
        ModeError = mode_error_conj(NonHeadVarUnificationGoals,
            goals_followed_by_impure_goal(Goal)),
        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_mode_context(!.ModeInfo, ModeContext),
        ImpurityError = mode_error_info(Vars, ModeError, Context, ModeContext),
        !:ImpurityErrors = [ImpurityError | !.ImpurityErrors]
    ).

:- pred filter_headvar_unification_goals(list(prog_var)::in,
    list(delayed_goal)::in, list(delayed_goal)::out, list(delayed_goal)::out)
    is det.

filter_headvar_unification_goals(HeadVars, DelayedGoals,
        HeadVarUnificationGoals, NonHeadVarUnificationGoals) :-
    list.filter(is_headvar_unification_goal(HeadVars), DelayedGoals,
        HeadVarUnificationGoals, NonHeadVarUnificationGoals).

:- pred is_headvar_unification_goal(list(prog_var)::in, delayed_goal::in)
    is semidet.

is_headvar_unification_goal(HeadVars, delayed_goal(_, _, Goal)) :-
    Goal ^ hlds_goal_expr = unify(Var, RHS, _, _, _),
    (
        list.member(Var, HeadVars)
    ;
        RHS = rhs_var(OtherVar),
        list.member(OtherVar, HeadVars)
    ).

    % Given an association list of Vars - Goals,
    % combine all the Vars together into a single set.
    %
:- pred get_all_waiting_vars(list(delayed_goal)::in, set_of_progvar::out)
    is det.

get_all_waiting_vars(DelayedGoals, Vars) :-
    get_all_waiting_vars_2(DelayedGoals, set_of_var.init, Vars).

:- pred get_all_waiting_vars_2(list(delayed_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

get_all_waiting_vars_2([], Vars, Vars).
get_all_waiting_vars_2([delayed_goal(Vars1, _, _) | Rest], Vars0, Vars) :-
    set_of_var.union(Vars0, Vars1, Vars2),
    get_all_waiting_vars_2(Rest, Vars2, Vars).

:- pred redelay_goals(list(delayed_goal)::in, delay_info::in, delay_info::out)
    is det.

redelay_goals([], !DelayInfo).
redelay_goals([DelayedGoal | DelayedGoals], !DelayInfo) :-
    DelayedGoal = delayed_goal(_WaitingVars, ModeErrorInfo, Goal),
    delay_info_delay_goal(ModeErrorInfo, Goal, !DelayInfo),
    redelay_goals(DelayedGoals, !DelayInfo).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_conj.
%-----------------------------------------------------------------------------%
