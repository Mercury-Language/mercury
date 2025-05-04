%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mark_trace_goals.m.
%
% This module ensures that goals have the contains_trace_goals feature
% if and only if they actually contain trace goals.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.simplify.mark_trace_goals.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.

:- pred set_goal_contains_trace_features_in_goal(hlds_goal::in, hlds_goal::out,
    contains_trace_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module list.
:- import_module maybe.
:- import_module require.

%-----------------------------------------------------------------------------%

set_goal_contains_trace_features_in_goal(Goal0, Goal, ContainsTrace) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        ContainsTrace = contains_no_trace_goal
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        ContainsTrace0 = contains_no_trace_goal,
        set_goal_contains_trace_features_in_goals(SubGoals0, SubGoals,
            ContainsTrace0, ContainsTrace),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        ContainsTrace0 = contains_no_trace_goal,
        set_goal_contains_trace_features_in_goals(SubGoals0, SubGoals,
            ContainsTrace0, ContainsTrace),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        ContainsTrace0 = contains_no_trace_goal,
        set_goal_contains_trace_features_in_cases(Cases0, Cases,
            ContainsTrace0, ContainsTrace),
        GoalExpr = switch(SwitchVar, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        set_goal_contains_trace_features_in_goal(Cond0, Cond,
            CondContainsTrace),
        set_goal_contains_trace_features_in_goal(Then0, Then,
            ThenContainsTrace),
        set_goal_contains_trace_features_in_goal(Else0, Else,
            ElseContainsTrace),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        ContainsTrace = worst_contains_trace(CondContainsTrace,
            worst_contains_trace(ThenContainsTrace, ElseContainsTrace))
    ;
        GoalExpr0 = negation(SubGoal0),
        set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
            ContainsTrace),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = trace_goal(_, _, _, _, _),
            SubGoal = SubGoal0,
            ContainsTrace = contains_trace_goal
        ;
            Reason = from_ground_term(_, FGT),
            (
                ( FGT = from_ground_term_construct
                ; FGT = from_ground_term_deconstruct
                ),
                SubGoal = SubGoal0,
                ContainsTrace = contains_no_trace_goal
            ;
                ( FGT = from_ground_term_initial
                ; FGT = from_ground_term_other
                ),
                set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
                    ContainsTrace)
            )
        ;
            ( Reason = exist_quant(_, _)
            ; Reason = disable_warnings(_, _)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = loop_control(_, _, _)
            ),
            set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
                ContainsTrace)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            set_goal_contains_trace_features_in_goal(MainGoal0, MainGoal,
                MainContainsTrace),
            OrElseContainsTrace0 = contains_no_trace_goal,
            set_goal_contains_trace_features_in_goals(
                OrElseGoals0, OrElseGoals,
                OrElseContainsTrace0, OrElseContainsTrace),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand),
            ContainsTrace = worst_contains_trace(MainContainsTrace,
                OrElseContainsTrace)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            set_goal_contains_trace_features_in_goal(SubGoal0, SubGoal,
                ContainsTrace),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ),
    (
        ContainsTrace = contains_trace_goal,
        goal_info_add_feature(feature_contains_trace, GoalInfo0, GoalInfo)
    ;
        ContainsTrace = contains_no_trace_goal,
        goal_info_remove_feature(feature_contains_trace, GoalInfo0, GoalInfo)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred set_goal_contains_trace_features_in_goals(
    list(hlds_goal)::in, list(hlds_goal)::out,
    contains_trace_goal::in, contains_trace_goal::out) is det.

set_goal_contains_trace_features_in_goals([], [], !ContainsTrace).
set_goal_contains_trace_features_in_goals([Goal0 | Goals0], [Goal | Goals],
        !ContainsTrace) :-
    set_goal_contains_trace_features_in_goal(Goal0, Goal, GoalContainsTrace),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    set_goal_contains_trace_features_in_goals(Goals0, Goals, !ContainsTrace).

:- pred set_goal_contains_trace_features_in_cases(
    list(case)::in, list(case)::out,
    contains_trace_goal::in, contains_trace_goal::out) is det.

set_goal_contains_trace_features_in_cases([], [], !ContainsTrace).
set_goal_contains_trace_features_in_cases([Case0 | Cases0], [Case | Cases],
        !ContainsTrace) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_goal_contains_trace_features_in_goal(Goal0, Goal, GoalContainsTrace),
    Case = case(MainConsId, OtherConsIds, Goal),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    set_goal_contains_trace_features_in_cases(Cases0, Cases, !ContainsTrace).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.simplify.mark_trace_goals.
%-----------------------------------------------------------------------------%
