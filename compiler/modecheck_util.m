%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modecheck_util.m.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.modecheck_util.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type extra_goals
    --->    no_extra_goals
    ;       extra_goals(
                % Goals to insert before the main goal.
                extra_before_main   :: list(hlds_goal),

                % Goals to append after the main goal.
                extra_after_main    :: list(hlds_goal)
            ).

:- type after_goals
    --->    no_after_goals
    ;       after_goals(
                % Instmap at end of main goal.
                after_instmap       :: instmap,

                % Goals to append after the main goal.
                after_goals         :: list(hlds_goal)
            ).

    % Append_extra_goals inserts adds some goals to the
    % list of goals to insert before/after the main goal.
    %
:- pred append_extra_goals(extra_goals::in, extra_goals::in,
    extra_goals::out) is det.

    % Handle_extra_goals combines MainGoal and ExtraGoals into a single
    % hlds_goal_expr, rerunning mode analysis on the entire conjunction
    % if ExtraGoals is not empty.
    %
:- pred handle_extra_goals(hlds_goal_expr::in, extra_goals::in,
    hlds_goal_info::in, list(prog_var)::in, list(prog_var)::in,
    instmap::in, hlds_goal_expr::out, mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

    % Calculate the argument number offset that needs to be passed to
    % modecheck_var_list_is_live, modecheck_var_has_inst_list, and
    % modecheck_set_var_inst_list. This offset number is calculated so that
    % real arguments get positive argument numbers and type_info arguments
    % get argument numbers less than or equal to 0.
    %
:- pred compute_arg_offset(pred_info::in, int::out) is det.

    % Given a list of variables and a list of expected liveness, ensure
    % that the inst of each variable satisfies the corresponding expected
    % liveness. See below for the difference between the two variants.
    %
:- pred modecheck_var_list_is_live_exact_match(list(prog_var)::in,
    list(is_live)::in, int::in, mode_info::in, mode_info::out) is det.
:- pred modecheck_var_list_is_live_no_exact_match(list(prog_var)::in,
    list(is_live)::in, int::in, mode_info::in, mode_info::out) is det.

    % Given a list of variables and a list of initial insts, ensure that
    % the inst of each variable matches the corresponding initial inst.
    % The first variant requires an exact match (using inst_matches_final),
    % while the second we allow the var to be more instantiated than the inst
    % (using inst_matches_initial).
    %
:- pred modecheck_var_has_inst_list_exact_match(list(prog_var)::in,
    list(mer_inst)::in, int::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.
:- pred modecheck_var_has_inst_list_no_exact_match(list(prog_var)::in,
    list(mer_inst)::in, int::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

    % This is a special-cased, cut-down version of
    % modecheck_var_has_inst_list_no_exact_match for use specifically
    % on introduced type_info_type variables.
    %
:- pred modecheck_introduced_type_info_var_has_inst_no_exact_match(
    prog_var::in, mer_type::in, mer_inst::in,
    mode_info::in, mode_info::out) is det.

    % modecheck_set_var_inst(Var, Inst, MaybeUInst, !ModeInfo):
    %
    % Assign the given Inst to the given Var, after checking that it is
    % okay to do so. If the inst to be assigned is the result of an
    % abstract unification then the MaybeUInst argument should be the
    % initial inst of the _other_ side of the unification. This allows
    % more precise (i.e. less conservative) checking in the case that
    % Inst contains `any' components and Var is locked (i.e. is a
    % nonlocal variable in a negated context). Where the inst is not
    % the result of an abstract unification then MaybeUInst should be `no'.
    %
:- pred modecheck_set_var_inst(prog_var::in, mer_inst::in, maybe(mer_inst)::in,
    mode_info::in, mode_info::out) is det.

:- pred modecheck_set_var_inst_list(list(prog_var)::in, list(mer_inst)::in,
    list(mer_inst)::in, int::in, list(prog_var)::out, extra_goals::out,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_add_goals_live_vars(conj_type::in, list(hlds_goal)::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_remove_goals_live_vars(list(hlds_goal)::in,
    mode_info::in, mode_info::out) is det.

    % modecheck_functor_test(Var, ConsId, !ModeInfo):
    %
    % Update the instmap to reflect the fact that Var was bound to ConsId.
    % This is used for the functor tests in `switch' statements.
    %
:- pred modecheck_functor_test(prog_var::in, cons_id::in,
    mode_info::in, mode_info::out) is det.

    % modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo):
    %
    % Update the instmap to reflect the fact that Var was bound to either
    % MainConsId or one of the OtherConsIds.
    % This is used for the functor tests in `switch' statements.
    %
:- pred modecheck_functors_test(prog_var::in, cons_id::in, list(cons_id)::in,
    mode_info::in, mode_info::out) is det.

    % compute_goal_instmap_delta(InstMap0, GoalExpr, !GoalInfo, !ModeInfo):
    %
    % Work out the instmap_delta for a goal from the instmaps before and after
    % the goal. The instmap before the goal is given by InstMap0; the instmap
    % after the goal is given by !.ModeInfo.
    %
:- pred compute_goal_instmap_delta(instmap::in, hlds_goal_expr::in,
    hlds_goal_info::in, hlds_goal_info::out, mode_info::in, mode_info::out)
    is det.

%-----------------------------------------------------------------------------%

    % Construct a call to initialise a free solver type variable.
    %
:- pred construct_initialisation_call(prog_var::in, mer_type::in, mer_inst::in,
    prog_context::in, maybe(call_unify_context)::in,
    hlds_goal::out, mode_info::in, mode_info::out) is det.

    % Construct a list of initialisation calls.
    %
:- pred construct_initialisation_calls(list(prog_var)::in,
    list(hlds_goal)::out, mode_info::in, mode_info::out) is det.

:- pred prepend_initialisation_call(prog_var::in, mer_type::in, mer_inst::in,
    hlds_goal::in, hlds_goal::out, mode_info::in, mode_info::out) is det.

:- pred mode_context_to_unify_context(mode_info::in, mode_context::in,
    unify_context::out) is det.

    % Given a list of variables, and a list of livenesses,
    % select the live variables.
    %
:- pred get_live_vars(list(prog_var)::in, list(is_live)::in,
    list(prog_var)::out) is det.

    % Return a map of all the inst variables in the given modes, and the
    % sub-insts to which they are constrained.
    %
:- pred get_constrained_inst_vars(module_info::in, list(mer_mode)::in,
    head_inst_vars::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.modecheck_goal.
:- import_module check_hlds.modecheck_unify.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.
:- import_module unit.
:- import_module varset.

%-----------------------------------------------------------------------------%

append_extra_goals(no_extra_goals, ExtraGoals, ExtraGoals).
append_extra_goals(extra_goals(BeforeGoals, AfterGoals),
        no_extra_goals, extra_goals(BeforeGoals, AfterGoals)).
append_extra_goals(extra_goals(BeforeGoals0, AfterGoals0),
        extra_goals(BeforeGoals1, AfterGoals1),
        extra_goals(BeforeGoals, AfterGoals)) :-
    BeforeGoals = BeforeGoals0 ++ BeforeGoals1,
    AfterGoals = AfterGoals0 ++ AfterGoals1.

handle_extra_goals(MainGoal, no_extra_goals, _GoalInfo0, _Args0, _Args,
        _InstMap0, MainGoal, !ModeInfo).
handle_extra_goals(MainGoal, extra_goals(BeforeGoals0, AfterGoals0),
        GoalInfo0, Args0, Args, InstMap0, Goal, !ModeInfo) :-
    mode_info_get_errors(!.ModeInfo, Errors),
    (
        % There is no point adding extra goals if the code is unreachable
        % anyway.
        instmap_is_reachable(InstMap0),

        % If we recorded errors processing the goal, it will have to be
        % reprocessed anyway, so don't add the extra goals now.
        Errors = []
    ->
        % We need to be careful to update the delta-instmaps
        % correctly, using the appropriate instmaps:
        %
        %       % InstMapAtStart is here
        %    BeforeGoals,
        %       % we don't know the instmap here,
        %       % but as it happens we don't need it
        %    main goal,
        %       % InstMapAfterMain is here
        %    AfterGoals
        %       % InstMapAtEnd (from the ModeInfo) is here

        % Recompute the new set of non-local variables for the main goal.
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.list_to_set(Args0, OldArgVars),
        set_of_var.list_to_set(Args, NewArgVars),
        set_of_var.difference(NewArgVars, OldArgVars, IntroducedVars),
        set_of_var.union(NonLocals0, IntroducedVars, OutsideVars),
        set_of_var.intersect(OutsideVars, NewArgVars, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),

        % Combine the main goal and the extra goals into a conjunction.
        Goal0 = hlds_goal(MainGoal, GoalInfo),
        Context = goal_info_get_context(GoalInfo0),
        handle_extra_goals_contexts(BeforeGoals0, Context, BeforeGoals),
        handle_extra_goals_contexts(AfterGoals0, Context, AfterGoals),
        GoalList0 = BeforeGoals ++ [Goal0 | AfterGoals],

        mode_info_get_may_change_called_proc(!.ModeInfo, MayChangeCalledProc0),

        % Make sure we don't go into an infinite loop if
        % there is a bug in the code to add extra goals.
        mode_info_set_checking_extra_goals(yes, !ModeInfo),

        % We have already worked out which procedure should be called,
        % we don't need to do it again.
        mode_info_set_may_change_called_proc(may_not_change_called_proc,
            !ModeInfo),

        mode_info_set_instmap(InstMap0, !ModeInfo),

        % Recheck the goals to compute the instmap_deltas.
        %
        % This can fail even if the original check on the goal
        % succeeded in the case of a unification procedure which
        % binds a partially instantiated variable, because adding
        % the extra goals can make the partially instantiated
        % variables `live' after the main goal.
        % The other thing to beware of in this case is that delaying
        % must be disabled while processing the extra goals. If it
        % is not, the main unification will be delayed until after the
        % argument unifications, which turns them into assignments,
        % and we end up repeating the process forever.
        mode_info_add_goals_live_vars(plain_conj, GoalList0, !ModeInfo),
        modecheck_conj_list_no_delay(GoalList0, GoalList, !ModeInfo),
        Goal = conj(plain_conj, GoalList),
        mode_info_set_checking_extra_goals(no, !ModeInfo),
        mode_info_set_may_change_called_proc(MayChangeCalledProc0, !ModeInfo)
    ;
        Goal = MainGoal
    ).

    % Modecheck a conjunction without doing any reordering.
    % This is used by handle_extra_goals above.
    %
:- pred modecheck_conj_list_no_delay(list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out) is det.

modecheck_conj_list_no_delay([], [], !ModeInfo).
modecheck_conj_list_no_delay([Goal0 | Goals0], [Goal | Goals], !ModeInfo) :-
    NonLocals = goal_get_nonlocals(Goal0),
    mode_info_remove_live_vars(NonLocals, !ModeInfo),
    modecheck_goal(Goal0, Goal, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    ( instmap_is_unreachable(InstMap) ->
        % We should not mode-analyse the remaining goals, since they
        % are unreachable. Instead we optimize them away, so that
        % later passes won't complain about them not having mode information.
        mode_info_remove_goals_live_vars(Goals0, !ModeInfo),
        Goals  = []
    ;
        modecheck_conj_list_no_delay(Goals0, Goals, !ModeInfo)
    ).

:- pred handle_extra_goals_contexts(list(hlds_goal)::in, prog_context::in,
    list(hlds_goal)::out) is det.

handle_extra_goals_contexts([], _Context, []).
handle_extra_goals_contexts([Goal0 | Goals0], Context, [Goal | Goals]) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    handle_extra_goals_contexts(Goals0, Context, Goals).

%-----------------------------------------------------------------------------%

modecheck_functor_test(Var, ConsId, !ModeInfo) :-
    % Figure out the arity of this constructor, _including_ any type-infos
    % or typeclass-infos inserted for existential data types.
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    BoundInst = cons_id_to_bound_inst(ModuleInfo, Type, ConsId),

    % Record the fact that Var was bound to ConsId.
    Inst = bound(unique, inst_test_no_results, [BoundInst]),
    modecheck_set_var_inst(Var, Inst, no, !ModeInfo).

modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo) :-
    % Figure out the arity of this constructor, _including_ any type-infos
    % or typeclass-infos inserted for existential data types.
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    BoundInsts = list.map(cons_id_to_bound_inst(ModuleInfo, Type),
        [MainConsId | OtherConsIds]),

    % Record the fact that Var was bound to MainConsId or one of the
    % OtherConsIds.
    Inst = bound(unique, inst_test_no_results, BoundInsts),
    modecheck_set_var_inst(Var, Inst, no, !ModeInfo).

:- func cons_id_to_bound_inst(module_info, mer_type, cons_id) = bound_inst.

cons_id_to_bound_inst(ModuleInfo, Type, ConsId) = BoundInst :-
    ConsIdAdjustedArity = cons_id_adjusted_arity(ModuleInfo, Type, ConsId),
    list.duplicate(ConsIdAdjustedArity, free, ArgInsts),
    BoundInst = bound_functor(ConsId, ArgInsts).

compute_goal_instmap_delta(InstMap0, GoalExpr, !GoalInfo, !ModeInfo) :-
    ( GoalExpr = conj(_, []) ->
        % When modecheck_unify.m replaces a unification with a dead variable
        % with `true', make sure the instmap_delta of the goal is empty.
        % The code generator and mode_util.recompute_instmap_delta can be
        % confused by references to the dead variable in the instmap_delta,
        % resulting in calls to error/1.

        instmap_delta_init_reachable(DeltaInstMap),
        mode_info_set_instmap(InstMap0, !ModeInfo)
    ;
        NonLocals = goal_info_get_nonlocals(!.GoalInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap)
    ),
    goal_info_set_instmap_delta(DeltaInstMap, !GoalInfo).

%-----------------------------------------------------------------------------%

compute_arg_offset(PredInfo, ArgOffset) :-
    OrigArity = pred_info_orig_arity(PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    list.length(ArgTypes, CurrentArity),
    ArgOffset = OrigArity - CurrentArity.

%-----------------------------------------------------------------------------%

modecheck_var_list_is_live_exact_match([_ | _], [], _, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_list_is_live_exact_match([], [_ | _], _, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_list_is_live_exact_match([], [], _ArgNum, !ModeInfo).
modecheck_var_list_is_live_exact_match([Var | Vars], [IsLive | IsLives],
        ArgNum0, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_var_is_live_exact_match(Var, IsLive, !ModeInfo),
    modecheck_var_list_is_live_exact_match(Vars, IsLives, ArgNum, !ModeInfo).

modecheck_var_list_is_live_no_exact_match([_ | _], [], _, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_list_is_live_no_exact_match([], [_ | _], _, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_list_is_live_no_exact_match([], [], _ArgNum, !ModeInfo).
modecheck_var_list_is_live_no_exact_match([Var | Vars], [IsLive | IsLives],
        ArgNum0, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_var_is_live_no_exact_match(Var, IsLive, !ModeInfo),
    modecheck_var_list_is_live_no_exact_match(Vars, IsLives, ArgNum,
        !ModeInfo).

    % `live' means possibly used later on, and `dead' means definitely not used
    % later on. If you don't need an exact match, then the only time you get
    % an error is if you pass a variable which is live to a predicate
    % that expects the variable to be dead; the predicate may use destructive
    % update to clobber the variable, so we must be sure that it is dead
    % after the call.
    %

    % A version of modecheck_var_is_live specialized for NeedExactMatch = no.
    %
:- pred modecheck_var_is_live_no_exact_match(prog_var::in, is_live::in,
    mode_info::in, mode_info::out) is det.

modecheck_var_is_live_no_exact_match(VarId, ExpectedIsLive, !ModeInfo) :-
    mode_info_var_is_live(!.ModeInfo, VarId, VarIsLive),
    (
        ExpectedIsLive = is_dead,
        VarIsLive = is_live
    ->
        WaitingVars = set_of_var.make_singleton(VarId),
        ModeError = mode_error_var_is_live(VarId),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ;
        true
    ).

    % A version of modecheck_var_is_live specialized for NeedExactMatch = yes.
    %
:- pred modecheck_var_is_live_exact_match(prog_var::in, is_live::in,
    mode_info::in, mode_info::out) is det.

modecheck_var_is_live_exact_match(VarId, ExpectedIsLive, !ModeInfo) :-
    mode_info_var_is_live(!.ModeInfo, VarId, VarIsLive),
    ( VarIsLive = ExpectedIsLive ->
        true
    ;
        WaitingVars = set_of_var.make_singleton(VarId),
        ModeError = mode_error_var_is_live(VarId),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ).

%-----------------------------------------------------------------------------%

    % Given a list of variables and a list of initial insts, ensure that
    % the inst of each variable matches the corresponding initial inst.
    %
modecheck_var_has_inst_list_exact_match(Vars, Insts, ArgNum, Subst,
        !ModeInfo) :-
    modecheck_var_has_inst_list_exact_match_2(Vars, Insts, ArgNum,
        map.init, Subst, !ModeInfo),
    modecheck_head_inst_vars(Vars, Subst, !ModeInfo).

modecheck_var_has_inst_list_no_exact_match(Vars, Insts, ArgNum, Subst,
        !ModeInfo) :-
    modecheck_var_has_inst_list_no_exact_match_2(Vars, Insts, ArgNum,
        map.init, Subst, !ModeInfo),
    modecheck_head_inst_vars(Vars, Subst, !ModeInfo).

:- pred modecheck_var_has_inst_list_exact_match_2(list(prog_var)::in,
    list(mer_inst)::in, int::in, inst_var_sub::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

modecheck_var_has_inst_list_exact_match_2([_ | _], [], _, !Subst, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_has_inst_list_exact_match_2([], [_ | _], _, !Subst, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_has_inst_list_exact_match_2([], [], _ArgNum, !Subst, !ModeInfo).
modecheck_var_has_inst_list_exact_match_2([Var | Vars], [Inst | Insts],
        ArgNum0, !Subst, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_var_has_inst_exact_match(Var, Inst, !Subst, !ModeInfo),
    modecheck_var_has_inst_list_exact_match_2(Vars, Insts, ArgNum,
        !Subst, !ModeInfo).

:- pred modecheck_var_has_inst_list_no_exact_match_2(list(prog_var)::in,
    list(mer_inst)::in, int::in, inst_var_sub::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

modecheck_var_has_inst_list_no_exact_match_2([_ | _], [], _,
        !Subst, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_has_inst_list_no_exact_match_2([], [_ | _], _,
        !Subst, !ModeInfo) :-
    unexpected($module, $pred, "length mismatch").
modecheck_var_has_inst_list_no_exact_match_2([], [], _ArgNum,
        !Subst, !ModeInfo).
modecheck_var_has_inst_list_no_exact_match_2([Var | Vars], [Inst | Insts],
        ArgNum0, !Subst, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_var_has_inst_no_exact_match(Var, Inst, !Subst, !ModeInfo),
    modecheck_var_has_inst_list_no_exact_match_2(Vars, Insts, ArgNum,
        !Subst, !ModeInfo).

:- pred modecheck_var_has_inst_exact_match(prog_var::in, mer_inst::in,
    inst_var_sub::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

modecheck_var_has_inst_exact_match(Var, Inst0, !Subst, !ModeInfo) :-
    % Apply the substitution computed while matching earlier arguments.
    inst_apply_substitution(!.Subst, Inst0, Inst),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    instmap_lookup_var(InstMap, Var, VarInst),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    (
        inst_matches_initial_no_implied_modes_sub(VarInst, Inst, Type,
            ModuleInfo0, ModuleInfo, !Subst)
    ->
        mode_info_set_module_info(ModuleInfo, !ModeInfo)
    ;
        WaitingVars = set_of_var.make_singleton(Var),
        ModeError = mode_error_var_has_inst(Var, VarInst, Inst),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ).

:- pred modecheck_var_has_inst_no_exact_match(prog_var::in, mer_inst::in,
    inst_var_sub::in, inst_var_sub::out,
    mode_info::in, mode_info::out) is det.

modecheck_var_has_inst_no_exact_match(Var, Inst0, !Subst, !ModeInfo) :-
    % Apply the substitution computed while matching earlier arguments.
    inst_apply_substitution(!.Subst, Inst0, Inst),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    instmap_lookup_var(InstMap, Var, VarInst),
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    (
        inst_matches_initial_sub(VarInst, Inst, Type, ModuleInfo0, ModuleInfo,
            !Subst)
    ->
        mode_info_set_module_info(ModuleInfo, !ModeInfo)
    ;
        WaitingVars = set_of_var.make_singleton(Var),
        ModeError = mode_error_var_has_inst(Var, VarInst, Inst),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ).

modecheck_introduced_type_info_var_has_inst_no_exact_match(Var, Type, Inst,
        !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap),
    instmap_lookup_var(InstMap, Var, VarInst),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    (
        inst_matches_initial_sub(VarInst, Inst, Type, ModuleInfo0, ModuleInfo,
            map.init, _Subst)
    ->
        mode_info_set_module_info(ModuleInfo, !ModeInfo)
    ;
        WaitingVars = set_of_var.make_singleton(Var),
        ModeError = mode_error_var_has_inst(Var, VarInst, Inst),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred modecheck_head_inst_vars(list(prog_var)::in, inst_var_sub::in,
    mode_info::in, mode_info::out) is det.

modecheck_head_inst_vars(Vars, InstVarSub, !ModeInfo) :-
    mode_info_get_head_inst_vars(!.ModeInfo, HeadInstVars),
    ( map.foldl(modecheck_head_inst_var(HeadInstVars), InstVarSub, unit, _) ->
        true
    ;
        mode_info_get_instmap(!.ModeInfo, InstMap),
        instmap_lookup_vars(InstMap, Vars, VarInsts),
        WaitingVars = set_of_var.list_to_set(Vars),
        ModeError = mode_error_no_matching_mode(Vars, VarInsts),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ).

:- pred modecheck_head_inst_var(inst_var_sub::in, inst_var::in, mer_inst::in,
    unit::in, unit::out) is semidet.

modecheck_head_inst_var(HeadInstVars, InstVar, Subst, !Acc) :-
    ( map.search(HeadInstVars, InstVar, Inst) ->
        % Subst should not change the constraint.
        Subst = constrained_inst_vars(InstVars, Inst),
        set.member(InstVar, InstVars)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

modecheck_set_var_inst_list(Vars0, InitialInsts, FinalInsts, ArgOffset,
        Vars, Goals, !ModeInfo) :-
    (
        modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts,
            ArgOffset, Vars1, no_extra_goals, Goals1, !ModeInfo)
    ->
        Vars = Vars1,
        Goals = Goals1
    ;
        unexpected($module, $pred, "length mismatch")
    ).

:- pred modecheck_set_var_inst_list_2(list(prog_var)::in, list(mer_inst)::in,
    list(mer_inst)::in, int::in, list(prog_var)::out,
    extra_goals::in, extra_goals::out, mode_info::in, mode_info::out)
    is semidet.

modecheck_set_var_inst_list_2([], [], [], _, [], !ExtraGoals, !ModeInfo).
modecheck_set_var_inst_list_2([Var0 | Vars0], [InitialInst | InitialInsts],
        [FinalInst | FinalInsts], ArgNum0, [Var | Vars],
        !ExtraGoals, !ModeInfo) :-
    ArgNum = ArgNum0 + 1,
    mode_info_set_call_arg_context(ArgNum, !ModeInfo),
    modecheck_set_var_inst_call(Var0, InitialInst, FinalInst,
        Var, !ExtraGoals, !ModeInfo),
    modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts, ArgNum,
        Vars, !ExtraGoals, !ModeInfo).

:- pred modecheck_set_var_inst_call(prog_var::in, mer_inst::in, mer_inst::in,
    prog_var::out, extra_goals::in, extra_goals::out,
    mode_info::in, mode_info::out) is det.

modecheck_set_var_inst_call(Var0, InitialInst, FinalInst, Var, !ExtraGoals,
        !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    ( instmap_is_reachable(InstMap0) ->
        % The new inst must be computed by unifying the old inst
        % and the proc's final inst.
        instmap_lookup_var(InstMap0, Var0, VarInst0),
        handle_implied_mode(Var0, VarInst0, InitialInst, Var, !ExtraGoals,
            !ModeInfo),
        modecheck_set_var_inst(Var0, FinalInst, no, !ModeInfo),
        ( Var = Var0 ->
            true
        ;
            modecheck_set_var_inst(Var, FinalInst, no, !ModeInfo)
        )
    ;
        Var = Var0
    ).

modecheck_set_var_inst(Var0, NewInst, MaybeUInst, !ModeInfo) :-
    % Note that there are two versions of modecheck_set_var_inst,
    % one with arity 8 (suffixed with _call) and one with arity 5.
    % The former is used for predicate calls, where we may need
    % to introduce unifications to handle calls to implied modes.
    %
    mode_info_get_parallel_vars(!.ModeInfo, PVars0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    ( instmap_is_reachable(InstMap0) ->
        instmap_lookup_var(InstMap0, Var0, OldInst),
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        % The final new inst must be computed by unifying the old inst
        % and the tentative new inst. However, abstractly unifying
        % a large inst with itself can be VERY expensive; it can be worse
        % than quadratic. The OldInst = NewInst test here may increase
        % execution time slightly in normal cases, but should reduce it
        % greatly in the worst cases.
        (
            OldInst = NewInst
        ->
            ModuleInfo = ModuleInfo0,
            Inst = OldInst
        ;
            abstractly_unify_inst(is_dead, OldInst, NewInst,
                fake_unify, UnifyInst, _Det, ModuleInfo0, ModuleInfo1)
        ->
            ModuleInfo = ModuleInfo1,
            Inst = UnifyInst
        ;
            unexpected($module, $pred, "unify_inst failed")
        ),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        lookup_var_type(VarTypes, Var0, Type),
        (
            % If the top-level inst of the variable is not_reached,
            % then the instmap as a whole must be unreachable.
            inst_expand(ModuleInfo, Inst, not_reached)
        ->
            instmap.init_unreachable(InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo)
        ;
            % If we haven't added any information and
            % we haven't bound any part of the var, then
            % the only thing we can have done is lose uniqueness.
            inst_matches_initial(OldInst, Inst, Type, ModuleInfo)
        ->
            instmap_set_var(Var0, Inst, InstMap0, InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo)
        ;
            % We must have either added some information,
            % lost some uniqueness, or bound part of the var.
            % The call to inst_matches_binding will succeed
            % only if we haven't bound any part of the var.
            \+ inst_matches_binding(Inst, OldInst, Type, ModuleInfo),

            % We have bound part of the var. If the var was locked,
            % then we need to report an error ...
            mode_info_var_is_locked(!.ModeInfo, Var0, Reason0),
            \+ (
                % ... unless the goal is a unification and the var was unified
                % with something no more instantiated than itself. This allows
                % for the case of `any = free', for example. The call to
                % inst_matches_binding, above will fail for the var with
                % mode `any >> any' however, it should be allowed because
                % it has only been unified with a free variable.
                MaybeUInst = yes(UInst),
                inst_is_at_least_as_instantiated(Inst, UInst, Type,
                    ModuleInfo),
                inst_matches_binding_allow_any_any(Inst, OldInst, Type,
                    ModuleInfo)
            )
        ->
            WaitingVars = set_of_var.make_singleton(Var0),
            ModeError = mode_error_bind_var(Reason0, Var0, OldInst, Inst),
            mode_info_error(WaitingVars, ModeError, !ModeInfo)
        ;
            instmap_set_var(Var0, Inst, InstMap0, InstMap),
            mode_info_set_instmap(InstMap, !ModeInfo),
            mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
            delay_info_bind_var(Var0, DelayInfo0, DelayInfo),
            mode_info_set_delay_info(DelayInfo, !ModeInfo)
        )
    ;
        true
    ),
    (
        PVars0 = []
    ;
        PVars0 = [par_conj_mode_check(NonLocals, Bound0) | PVars1],
        ( set_of_var.member(NonLocals, Var0) ->
            set_of_var.insert(Var0, Bound0, Bound),
            PVars = [par_conj_mode_check(NonLocals, Bound) | PVars1]
        ;
            PVars = PVars0
        ),
        mode_info_set_parallel_vars(PVars, !ModeInfo)
    ).

    % If this was a call to an implied mode for that variable, then we need to
    % introduce a fresh variable.
    %
:- pred handle_implied_mode(prog_var::in, mer_inst::in, mer_inst::in,
    prog_var::out, extra_goals::in, extra_goals::out,
    mode_info::in, mode_info::out) is det.

handle_implied_mode(Var0, VarInst0, InitialInst0, Var, !ExtraGoals,
        !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    inst_expand(ModuleInfo0, InitialInst0, InitialInst),
    inst_expand(ModuleInfo0, VarInst0, VarInst1),

    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    lookup_var_type(VarTypes0, Var0, VarType),
    (
        % If the initial inst of the variable matches_final the initial inst
        % specified in the pred's mode declaration, then it's not a call
        % to an implied mode, it's an exact match with a genuine mode.
        inst_matches_initial_no_implied_modes_sub(VarInst1, InitialInst,
            VarType, ModuleInfo0, _ModuleInfo, map.init, _Sub)
    ->
        Var = Var0
    ;
        % This is the implied mode case. We do not yet handle implied modes
        % for partially instantiated vars, since that would require doing
        % a partially instantiated deep copy, and we don't know how to do
        % that yet.

        InitialInst = any(_, _),
        inst_is_free(ModuleInfo0, VarInst1)
    ->
        % This is the simple case of implied `any' modes, where the declared
        % mode was `any -> ...' and the argument passed was `free'.

        Var = Var0,

        % If the variable's type is not a solver type (in which case inst `any'
        % means the same as inst `ground') then this is an implied mode that we
        % don't yet know how to handle.
        %
        % If the variable's type is a solver type then we need to insert a call
        % to the solver type's initialisation predicate. (To avoid unnecessary
        % complications, we avoid doing this if there are any mode errors
        % recorded at this point.)

        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_mode_context(!.ModeInfo, ModeContext),
        mode_context_to_unify_context(!.ModeInfo, ModeContext, UnifyContext),
        CallUnifyContext = yes(call_unify_context(Var, rhs_var(Var),
            UnifyContext)),
        (
            mode_info_get_errors(!.ModeInfo, ModeErrors),
            ModeErrors = [],
            mode_info_may_init_solver_vars(!.ModeInfo),
            mode_info_solver_init_is_supported(!.ModeInfo),
            type_is_solver_type_with_auto_init(ModuleInfo0, VarType)
        ->
            % Create code to initialize the variable to inst `any',
            % by calling the solver type's initialisation predicate.
            insert_extra_initialisation_call(Var, VarType, InitialInst,
                Context, CallUnifyContext, !ExtraGoals, !ModeInfo)
        ;
            % If the type is a type variable, or isn't a solver type,
            % then give up.
            WaitingVars = set_of_var.make_singleton(Var0),
            ModeError = mode_error_implied_mode(Var0, VarInst0, InitialInst),
            mode_info_error(WaitingVars, ModeError, !ModeInfo)
        )
    ;
        inst_is_bound(ModuleInfo0, InitialInst)
    ->
        % This is the case we can't handle.
        Var = Var0,
        WaitingVars = set_of_var.make_singleton(Var0),
        ModeError = mode_error_implied_mode(Var0, VarInst0, InitialInst),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    ;
        % This is the simple case of implied modes,
        % where the declared mode was free -> ...

        % Introduce a new variable.
        mode_info_get_varset(!.ModeInfo, VarSet0),
        varset.new_var(Var, VarSet0, VarSet),
        add_var_type(Var, VarType, VarTypes0, VarTypes),
        mode_info_set_varset(VarSet, !ModeInfo),
        mode_info_set_var_types(VarTypes, !ModeInfo),

        % Construct the code to do the unification.
        create_var_var_unification(Var0, Var, VarType, !.ModeInfo, ExtraGoal),

        % Append the goals together in the appropriate order:
        % ExtraGoals0, then NewUnify.
        NewUnifyExtraGoal = extra_goals([], [ExtraGoal]),
        append_extra_goals(!.ExtraGoals, NewUnifyExtraGoal, !:ExtraGoals)
    ).

%-----------------------------------------------------------------------------%

mode_info_add_goals_live_vars(_ConjType, [], !ModeInfo).
mode_info_add_goals_live_vars(ConjType, [Goal | Goals], !ModeInfo) :-
    % We add the live vars for the goals in the goal list in reverse order,
    % because this ensures that in the common case (where there is no
    % delaying), when we come to remove the live vars for the first goal
    % they will have been added last and will thus be at the start of the list
    % of live vars sets, which makes them cheaper to remove.
    mode_info_add_goals_live_vars(ConjType, Goals, !ModeInfo),
    (
        % Recurse into conjunctions, in case there are any conjunctions
        % that have not been flattened.
        Goal = hlds_goal(conj(ConjType, ConjGoals), _)
    ->
        mode_info_add_goals_live_vars(ConjType, ConjGoals, !ModeInfo)
    ;
        NonLocals = goal_get_nonlocals(Goal),
        mode_info_add_live_vars(NonLocals, !ModeInfo)
    ).

mode_info_remove_goals_live_vars([], !ModeInfo).
mode_info_remove_goals_live_vars([Goal | Goals], !ModeInfo) :-
    (
        % Recurse into conjunctions, in case there are any conjunctions
        % that have not been flattened.
        Goal = hlds_goal(conj(plain_conj, ConjGoals), _)
    ->
        mode_info_remove_goals_live_vars(ConjGoals, !ModeInfo)
    ;
        NonLocals = goal_get_nonlocals(Goal),
        mode_info_remove_live_vars(NonLocals, !ModeInfo)
    ),
    mode_info_remove_goals_live_vars(Goals, !ModeInfo).

%-----------------------------------------------------------------------------%

:- pred insert_extra_initialisation_call(prog_var::in, mer_type::in,
    mer_inst::in, prog_context::in, maybe(call_unify_context)::in,
    extra_goals::in, extra_goals::out, mode_info::in, mode_info::out) is det.

insert_extra_initialisation_call(Var, VarType, Inst, Context, CallUnifyContext,
        !ExtraGoals, !ModeInfo) :-
    construct_initialisation_call(Var, VarType, Inst, Context,
        CallUnifyContext, InitVarGoal, !ModeInfo),
    NewExtraGoal = extra_goals([InitVarGoal], []),
    append_extra_goals(!.ExtraGoals, NewExtraGoal, !:ExtraGoals).

construct_initialisation_calls([], [], !ModeInfo).
construct_initialisation_calls([Var | Vars], [Goal | Goals], !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes),
    lookup_var_type(VarTypes, Var, VarType),
    InitialInst           = free,
    Context               = term.context_init,
    MaybeCallUnifyContext = no,
    construct_initialisation_call(Var, VarType, InitialInst, Context,
        MaybeCallUnifyContext, Goal, !ModeInfo),
    construct_initialisation_calls(Vars, Goals, !ModeInfo).

construct_initialisation_call(Var, VarType, Inst, Context,
        MaybeCallUnifyContext, InitVarGoal, !ModeInfo) :-
    (
        type_to_ctor(VarType, TypeCtor),
        PredName = special_pred_name(spec_pred_init, TypeCtor),
        (
            TypeCtor = type_ctor(qualified(ModuleName, _TypeName), _Arity)
        ;
            TypeCtor = type_ctor(unqualified(_TypeName), _Arity),
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            module_info_get_name(ModuleInfo, ModuleName)
        ),
        NonLocals = set_of_var.make_singleton(Var),
        InstMapDeltaAL = [Var - Inst],
        InstMapDelta = instmap_delta_from_assoc_list(InstMapDeltaAL),
        build_call(ModuleName, PredName, [Var], [VarType], NonLocals,
            InstMapDelta, Context, MaybeCallUnifyContext,
            hlds_goal(GoalExpr, GoalInfo), !ModeInfo)
    ->
        InitVarGoal = hlds_goal(GoalExpr, GoalInfo),
        % If Var was ignored, i.e. it occurred in only one atomic goal
        % and was not in that atomic goal's nonlocals set, then creating
        % the call to the initialisation predicate and adding it to the
        % procedure body requires the addition of Var to the original goal's
        % nonlocals set. This *should* be done by looking at all the places
        % in the compiler that decide to call construct_initialisation_call
        % directly or indirectly, and modifying that code to add Var to
        % the relevant nonlocals set, or possibly by avoiding the call
        % to construct_initialisation_call altogether (after all, if
        % a variable is ignored, it should not need initialization).
        %
        % However, getting a requantify pass to do it for us is less work.
        %
        % An example of code that needs this fix for the correctness of the
        % HLDS is tests/hard_coded/solver_construction_init_test.m.
        mode_info_set_need_to_requantify(need_to_requantify, !ModeInfo)
    ;
        unexpected($module, $pred, "condition failed")
    ).

:- pred build_call(module_name::in, string::in, list(prog_var)::in,
    list(mer_type)::in, set_of_progvar::in, instmap_delta::in,
    prog_context::in, maybe(call_unify_context)::in, hlds_goal::out,
    mode_info::in, mode_info::out) is semidet.

build_call(CalleeModuleName, CalleePredName, ArgVars, ArgTypes, NonLocals,
        InstMapDelta, Context, MaybeCallUnifyContext, Goal, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),

    % Get the relevant information for the procedure we are transforming
    % (i.e., the caller).
    mode_info_get_pred_id(!.ModeInfo, PredId),
    mode_info_get_proc_id(!.ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, PredInfo0,
        ProcInfo0),
    pred_info_get_typevarset(PredInfo0, TVarSet),
    pred_info_get_exist_quant_tvars(PredInfo0, ExistQTVars),
    pred_info_get_head_type_params(PredInfo0, HeadTypeParams),

    % Get the pred_info and proc_info for the procedure we are calling.
    SymName = qualified(CalleeModuleName, CalleePredName),
    get_pred_id_and_proc_id_by_types(is_fully_qualified, SymName, pf_predicate,
        TVarSet, ExistQTVars, ArgTypes, HeadTypeParams, ModuleInfo0,
        Context, CalleePredId, CalleeProcId),
    module_info_pred_proc_info(ModuleInfo0, CalleePredId, CalleeProcId,
        CalleePredInfo, CalleeProcInfo),

    % Create a poly_info for the caller. We have to set the varset and
    % vartypes from the mode_info, not the proc_info, because new vars may
    % have been introduced during mode analysis (e.g., when adding
    % unifications to handle implied modes).
    mode_info_get_varset(!.ModeInfo, VarSet0),
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    proc_info_set_varset(VarSet0, ProcInfo0, ProcInfo1),
    proc_info_set_vartypes(VarTypes0, ProcInfo1, ProcInfo2),
    polymorphism.create_poly_info(ModuleInfo0, PredInfo0, ProcInfo2,
        PolyInfo0),

    % Create a goal_info for the call.
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo1),
    goal_info_set_nonlocals(NonLocals, GoalInfo1, GoalInfo2),
    goal_info_set_instmap_delta(InstMapDelta, GoalInfo2, GoalInfo),

    % Do the transformation for this call goal.
    SymName = qualified(CalleeModuleName, CalleePredName),
    polymorphism_process_new_call(CalleePredInfo, CalleeProcInfo,
        CalleePredId, CalleeProcId, ArgVars, not_builtin,
        MaybeCallUnifyContext, SymName, GoalInfo, Goal, PolyInfo0, PolyInfo),

    % Update the information in the predicate table.
    polymorphism.poly_info_extract(PolyInfo, PredInfo0, PredInfo,
        ProcInfo2, ProcInfo, ModuleInfo1),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        ModuleInfo1, ModuleInfo),

    % Update the information in the mode_info.
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo),
    mode_info_set_module_info(ModuleInfo, !ModeInfo).

prepend_initialisation_call(Var, VarType, InitialInst, Goal0, Goal,
        !ModeInfo) :-
    Goal0   = hlds_goal(_GoalExpr0, GoalInfo0),
    Context = goal_info_get_context(GoalInfo0),
    CallUnifyContext = no,
    construct_initialisation_call(Var, VarType, InitialInst, Context,
        CallUnifyContext, InitVarGoal, !ModeInfo),
    goal_to_conj_list(Goal0, ConjList0),
    conj_list_to_goal([InitVarGoal | ConjList0], GoalInfo0, Goal).

%-----------------------------------------------------------------------------%

mode_context_to_unify_context(_ModeInfo, ModeContext, UnifyContext) :-
    (
        ModeContext = mode_context_unify(UnifyContext, _)
    ;
        ModeContext = mode_context_call(CallId, Arg),
        UnifyContext = unify_context(umc_call(CallId, Arg), [])
    ;
        ModeContext = mode_context_uninitialized,
        unexpected($module, $pred, "uninitialized context")
    ).

%-----------------------------------------------------------------------------%

get_live_vars([], [], []).
get_live_vars([_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
get_live_vars([], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
get_live_vars([Var | Vars], [IsLive | IsLives], LiveVars) :-
    (
        IsLive = is_live,
        LiveVars = [Var | LiveVars0]
    ;
        IsLive = is_dead,
        LiveVars = LiveVars0
    ),
    get_live_vars(Vars, IsLives, LiveVars0).

%-----------------------------------------------------------------------------%

:- type inst_expansions == set_tree234(inst_name).

get_constrained_inst_vars(ModuleInfo, Modes, Map) :-
    list.foldl2(get_constrained_insts_in_mode(ModuleInfo), Modes,
        map.init, Map, set_tree234.init, _Expansions).

:- pred get_constrained_insts_in_mode(module_info::in, mer_mode::in,
    head_inst_vars::in, head_inst_vars::out,
    inst_expansions::in, inst_expansions::out) is det.

get_constrained_insts_in_mode(ModuleInfo, Mode, !Map, !Expansions) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    get_constrained_insts_in_inst(ModuleInfo, InitialInst, !Map, !Expansions),
    get_constrained_insts_in_inst(ModuleInfo, FinalInst, !Map, !Expansions).

:- pred get_constrained_insts_in_inst(module_info::in, mer_inst::in,
    head_inst_vars::in, head_inst_vars::out,
    inst_expansions::in, inst_expansions::out) is det.

get_constrained_insts_in_inst(ModuleInfo, Inst, !Map, !Expansions) :-
    (
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        )
    ;
        Inst = bound(_, _, BoundInsts),
        list.foldl2(get_constrained_insts_in_bound_inst(ModuleInfo),
            BoundInsts, !Map, !Expansions)
    ;
        ( Inst = any(_, HOInstInfo)
        ; Inst = ground(_, HOInstInfo)
        ),
        (
            HOInstInfo = none
        ;
            HOInstInfo = higher_order(PredInstInfo),
            get_constrained_insts_in_ho_inst(ModuleInfo, PredInstInfo,
                !Map, !Expansions)
        )
    ;
        Inst = constrained_inst_vars(InstVars, _),
        inst_expand_and_remove_constrained_inst_vars(ModuleInfo,
            Inst, SubInst),
        set.fold(add_constrained_inst(SubInst), InstVars, !Map)
    ;
        Inst = defined_inst(InstName),
        ( insert_new(InstName, !Expansions) ->
            inst_lookup(ModuleInfo, InstName, ExpandedInst),
            get_constrained_insts_in_inst(ModuleInfo, ExpandedInst,
                !Map, !Expansions)
        ;
            true
        )
    ;
        Inst = inst_var(_),
        unexpected($module, $pred, "inst_var")
    ;
        Inst = abstract_inst(_, _),
        sorry($module, $pred, "abstract_inst")
    ).

:- pred get_constrained_insts_in_bound_inst(module_info::in, bound_inst::in,
    head_inst_vars::in, head_inst_vars::out,
    inst_expansions::in, inst_expansions::out) is det.

get_constrained_insts_in_bound_inst(ModuleInfo, BoundInst, !Map, !Expansions)
        :-
    BoundInst = bound_functor(_ConsId, Insts),
    list.foldl2(get_constrained_insts_in_inst(ModuleInfo), Insts,
        !Map, !Expansions).

:- pred get_constrained_insts_in_ho_inst(module_info::in, pred_inst_info::in,
    head_inst_vars::in, head_inst_vars::out,
    inst_expansions::in, inst_expansions::out) is det.

get_constrained_insts_in_ho_inst(ModuleInfo, PredInstInfo, !Map, !Expansions)
        :-
    PredInstInfo = pred_inst_info(_, Modes, _, _),
    list.foldl2(get_constrained_insts_in_mode(ModuleInfo), Modes,
        !Map, !Expansions).

:- pred add_constrained_inst(mer_inst::in, inst_var::in,
    head_inst_vars::in, head_inst_vars::out) is det.

add_constrained_inst(SubInst, InstVar, !Map) :-
    ( map.search(!.Map, InstVar, SubInst0) ->
        ( SubInst0 = SubInst ->
            true
        ;
            unexpected($module, $pred, "SubInst differs")
        )
    ;
        map.det_insert(InstVar, SubInst, !Map)
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_util.
%-----------------------------------------------------------------------------%
