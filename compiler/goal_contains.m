%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015-2019, 2021-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_contains.m.
%
% This module provides predicates to answer the question
% "what subgoals does this goal contain?".
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_contains.
:- interface.

:- import_module hlds.hlds_goal.

:- import_module bool.

%---------------------------------------------------------------------------%

    % Test whether the goal contains a reconstruction
    % (a construction where the `construct_how' field is `cell_to_reuse(_)').
    %
:- pred goal_contains_reconstruction(hlds_goal::in, bool::out) is det.

%---------------------%

    % goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
    % i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
    % or indirect sub-goal.
    %
:- pred goal_contains_goal(hlds_goal::in, hlds_goal::out) is multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

goal_contains_reconstruction(Goal, ContainsReconstruction) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        goals_contain_reconstruction(Conjuncts, ContainsReconstruction)
    ;
        GoalExpr = disj(Disjuncts),
        goals_contain_reconstruction(Disjuncts, ContainsReconstruction)
    ;
        GoalExpr = switch(_, _, Cases),
        cases_contain_reconstruction(Cases, ContainsReconstruction)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( if
            ( goal_contains_reconstruction(Cond, yes)
            ; goal_contains_reconstruction(Then, yes)
            ; goal_contains_reconstruction(Else, yes)
            )
        then
            ContainsReconstruction = yes
        else
            ContainsReconstruction = no
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_contains_reconstruction(SubGoal, ContainsReconstruction)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % Construct scopes contain only construction unifications
            % that do no reuse. Deconstruct scopes do not contain
            % any constructions at all.
            ContainsReconstruction = no
        else
            goal_contains_reconstruction(SubGoal, ContainsReconstruction)
        )
    ;
        GoalExpr = unify(_, _, _, Unify, _),
        ( if
            Unify = construct(_, _, _, _, HowToConstruct, _, _),
            HowToConstruct = reuse_cell(_)
        then
            ContainsReconstruction = yes
        else
            ContainsReconstruction = no
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ContainsReconstruction = no
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(GoalA, GoalB),
            ( if
                goal_contains_reconstruction(GoalA, yes),
                goal_contains_reconstruction(GoalB, yes)
            then
                ContainsReconstruction = yes
            else
                ContainsReconstruction = no
            )
        ;
            Shorthand = atomic_goal(_AtomicGoalType, _OuterVars, _InnerVars,
                _OutputVars, MainGoal, OrElseGoals, _Inners),
            ( if
                goal_contains_reconstruction(MainGoal, yes),
                goals_contain_reconstruction(OrElseGoals, yes)
            then
                ContainsReconstruction = yes
            else
                ContainsReconstruction = no
            )
        ;
            Shorthand = try_goal(_MaybeTryIOStateVars, _ResultVar, SubGoal),
            goal_contains_reconstruction(SubGoal, ContainsReconstruction)
        )
    ).

:- pred goals_contain_reconstruction(list(hlds_goal)::in, bool::out) is det.

goals_contain_reconstruction([], no).
goals_contain_reconstruction([Goal | Goals], ContainsReconstruction) :-
    goal_contains_reconstruction(Goal, HeadContainsReconstruction),
    (
        HeadContainsReconstruction = yes,
        ContainsReconstruction = yes
    ;
        HeadContainsReconstruction = no,
        goals_contain_reconstruction(Goals, ContainsReconstruction)
    ).

:- pred cases_contain_reconstruction(list(case)::in, bool::out) is det.

cases_contain_reconstruction([], no).
cases_contain_reconstruction([Case | Cases], ContainsReconstruction) :-
    Case = case(_, _, CaseGoal),
    goal_contains_reconstruction(CaseGoal, HeadContainsReconstruction),
    (
        HeadContainsReconstruction = yes,
        ContainsReconstruction = yes
    ;
        HeadContainsReconstruction = no,
        cases_contain_reconstruction(Cases, ContainsReconstruction)
    ).

%---------------------------------------------------------------------------%

goal_contains_goal(Goal, ContainedGoal) :-
    (
        ContainedGoal = Goal
    ;
        Goal = hlds_goal(GoalExpr, _),
        direct_subgoal(GoalExpr, DirectSubGoal),
        goal_contains_goal(DirectSubGoal, ContainedGoal)
    ).

    % direct_subgoal(Goal, DirectSubGoal) is true iff
    % DirectSubGoal is a direct subgoal of Goal.
    %
:- pred direct_subgoal(hlds_goal_expr::in, hlds_goal::out) is nondet.

direct_subgoal(GoalExpr, DirectSubGoal) :-
    require_complete_switch [GoalExpr]
    (
        ( GoalExpr = conj(_, DirectSubGoals)
        ; GoalExpr = disj(DirectSubGoals)
        ),
        list.member(DirectSubGoal, DirectSubGoals)
    ;
        GoalExpr = switch(_, _, Cases),
        list.member(Case, Cases),
        Case = case(_, _, DirectSubGoal)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( DirectSubGoal = Cond
        ; DirectSubGoal = Then
        ; DirectSubGoal = Else
        )
    ;
        GoalExpr = negation(DirectSubGoal)
    ;
        GoalExpr = scope(_, DirectSubGoal)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        fail
    ;
        GoalExpr = shorthand(Shorthand),
        % NOTE We used to fail for shorthand goals. This was either a bug,
        % or a limitation that should have been documented.
        (
            Shorthand = bi_implication(SubGoalA, SubGoalB),
            ( DirectSubGoal = SubGoalA
            ; DirectSubGoal = SubGoalB
            )
        ;
            Shorthand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            ( DirectSubGoal = MainGoal
            ; list.member(DirectSubGoal, OrElseGoals)
            )
        ;
            Shorthand = try_goal(_, _, DirectSubGoal)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_contains.
%---------------------------------------------------------------------------%
