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
:- import_module check_hlds.modecheck_goal.
:- import_module check_hlds.modecheck_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

modecheck_conj_list(ConjType, Goals0, Goals, !ModeInfo) :-
    mode_info_get_errors(!.ModeInfo, OldErrors),
    mode_info_set_errors([], !ModeInfo),

    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
    delay_info_enter_conj(DelayInfo0, DelayInfo1),
    mode_info_set_delay_info(DelayInfo1, !ModeInfo),

    mode_info_get_live_vars(!.ModeInfo, LiveVars1),
    mode_info_add_goals_live_vars(ConjType, Goals0, !ModeInfo),

    % Try to schedule the goals of the conjunction.
    modecheck_conj_list_flatten_and_schedule(ConjType, Goals0, Goals1,
        [], RevImpurityErrors0, !ModeInfo),

    mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
    delay_info_leave_conj(DelayInfo2, DelayedGoals0, DelayInfo3),
    mode_info_set_delay_info(DelayInfo3, !ModeInfo),

    % Try to schedule the goals that our earlier scheduling attempt delayed.
    modecheck_delayed_goals(ConjType, DelayedGoals0, DelayedGoals, Goals2,
        RevImpurityErrors0, RevImpurityErrors, !ModeInfo),
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
    ).

:- type impurity_errors == list(mode_error_info).

    % Schedule goals, and also flatten conjunctions of the same type as we go.
    %
:- pred modecheck_conj_list_flatten_and_schedule(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_conj_list_flatten_and_schedule(ConjType, Goals0, Goals,
        !ImpurityErrors, !ModeInfo) :-
    modecheck_conj_list_flatten_and_schedule_acc(ConjType, Goals0,
        cord.init, GoalsCord, !ImpurityErrors, !ModeInfo),
    Goals = cord.list(GoalsCord).

:- pred modecheck_conj_list_flatten_and_schedule_acc(conj_type::in,
    list(hlds_goal)::in, cord(hlds_goal)::in, cord(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_conj_list_flatten_and_schedule_acc(_ConjType, [], !Goals,
        !ImpurityErrors, !ModeInfo).
modecheck_conj_list_flatten_and_schedule_acc(ConjType, [Goal0 | Goals0],
        !Goals, !ImpurityErrors, !ModeInfo) :-
    ( if
        Goal0 = hlds_goal(conj(plain_conj, ConjGoals), _),
        ConjType = plain_conj
    then
        Goals1 = ConjGoals ++ Goals0,
        modecheck_conj_list_flatten_and_schedule_acc(ConjType, Goals1,
            !Goals, !ImpurityErrors, !ModeInfo)
    else
        % We attempt to schedule the first goal in the conjunction.
        % If successful, we try to wake up pending goals (if any), and if not,
        % we delay the goal. Then we continue attempting to schedule
        % all the rest of the conjuncts.

        Purity = goal_get_purity(Goal0),
        (
            Purity = purity_impure,
            Impure = yes,
            check_for_impurity_error(Goal0, ScheduledSolverGoals,
                !ImpurityErrors, !ModeInfo),
            !:Goals = !.Goals ++ cord.from_list(ScheduledSolverGoals)
        ;
            ( Purity = purity_pure
            ; Purity = purity_semipure
            ),
            Impure = no
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
            mode_info_get_delay_info(!.ModeInfo, DelayInfo1),
            % We successfully scheduled this goal, so insert it
            % in the list of successfully scheduled goals.
            % We flatten out conjunctions if we can. They can arise
            % when Goal0 was a scope(from_ground_term, _) goal.
            ( if Goal = hlds_goal(conj(ConjType, SubGoals), _) then
                !:Goals = !.Goals ++ from_list(SubGoals)
            else
                !:Goals = snoc(!.Goals, Goal)
            )
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
        ( if instmap_is_unreachable(InstMap) then
            % We should not mode-analyse the remaining goals, since they are
            % unreachable. Instead we optimize them away, so that later passes
            % won't complain about them not having mode information.
            mode_info_remove_goals_live_vars(Goals1, !ModeInfo)
        else
            % The remaining goals may still need to be flattened.
            modecheck_conj_list_flatten_and_schedule_acc(ConjType, Goals1,
                !Goals, !ImpurityErrors, !ModeInfo)
        )
    ).

%-----------------------------------------------------------------------------%

    % The attempt to schedule the goals of a conjunction by our caller
    % may have delayed some goals. Try to schedule those goals now.
    %
    % If we succeed in scheduling some delayed goal, this may wake up other
    % delayed goals, so recurse in order to try to schedule them.
    % Keep recursing until we can schedule no more delayed goal,
    % whether that happens because there are no more left, or not.
    %
:- pred modecheck_delayed_goals(conj_type::in, list(delayed_goal)::in,
    list(delayed_goal)::out, list(hlds_goal)::out,
    impurity_errors::in, impurity_errors::out,
    mode_info::in, mode_info::out) is det.

modecheck_delayed_goals(ConjType, DelayedGoals0, DelayedGoals, Goals,
        !ImpurityErrors, !ModeInfo) :-
    (
        % There are no unscheduled goals, so we don't need to do anything.
        DelayedGoals0 = [],
        DelayedGoals = [],
        Goals = []
    ;
        % There are some unscheduled goals.
        DelayedGoals0 = [_ | _],

        Goals0 = list.map(hlds_goal_from_delayed_goal, DelayedGoals0),

        mode_info_get_delay_info(!.ModeInfo, DelayInfo0),
        delay_info_enter_conj(DelayInfo0, DelayInfo1),
        mode_info_set_delay_info(DelayInfo1, !ModeInfo),

        modecheck_conj_list_flatten_and_schedule(ConjType, Goals0, Goals1,
            !ImpurityErrors, !ModeInfo),

        mode_info_get_delay_info(!.ModeInfo, DelayInfo2),
        delay_info_leave_conj(DelayInfo2, DelayedGoals1, DelayInfo3),
        mode_info_set_delay_info(DelayInfo3, !ModeInfo),

        % See if we scheduled any goals.
        ( if list.length(DelayedGoals1) < list.length(DelayedGoals0) then
            % We scheduled some goals. Keep going until we either
            % flounder or succeed.
            modecheck_delayed_goals(ConjType, DelayedGoals1, DelayedGoals,
                Goals2, !ImpurityErrors, !ModeInfo),
            Goals = Goals1 ++ Goals2
        else
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
    modecheck_delayed_goals(plain_conj,
        NonHeadVarUnificationGoals0, NonHeadVarUnificationGoals, Goals,
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
