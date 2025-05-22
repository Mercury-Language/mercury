%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015-2019, 2021-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_vars.m.
%
% This module provides predicates for answering the question
% "what variables occur in this goal?
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_vars.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module list.

%---------------------------------------------------------------------------%

    % Return all the variables in the goal.
    % Unlike quantification.goal_vars, this predicate returns
    % even the explicitly quantified variables.
    %
    % Warning: the complexity of this predicate is proportional to the
    % size of the goal. Goals can be pretty big. Whatever you want to do,
    % if you have a way to do it *without* calling the predicate, you will
    % probably want to do it that way.
    %
:- pred vars_in_goal(hlds_goal::in, set_of_progvar::out) is det.

    % Do the same job as vars_in_goal, but for a list of goals.
    %
:- pred vars_in_goals(list(hlds_goal)::in, set_of_progvar::out) is det.

%---------------------%

    % Do the same job as the vars_in_goal predicate above, with one exception:
    % ignore the variables that occur in any unification goal whose features
    % include feature_state_var_copy.
    %
:- pred non_svar_copy_vars_in_goal(hlds_goal::in, set_of_progvar::out) is det.

%---------------------%

    % Return all the variables in a generic call.
    %
:- pred vars_in_generic_call(generic_call::in, list(prog_var)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.

:- import_module assoc_list.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%---------------------------------------------------------------------------%

vars_in_goal(Goal, !:Set) :-
    set_of_var.init(!:Set),
    vars_in_goal_acc(Goal, !Set).

vars_in_goals(Goals, !:Set) :-
    set_of_var.init(!:Set),
    vars_in_goals_acc(Goals, !Set).

:- pred vars_in_goal_acc(hlds_goal::in,
    set_of_progvar::in, set_of_progvar::out) is det.

vars_in_goal_acc(Goal, !Set) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(Var, RHS, _, Unif, _),
        set_of_var.insert(Var, !Set),
        (
            Unif = construct(_, _, _, _, CellToReuse, _, _),
            ( if CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) then
                set_of_var.insert(Var, !Set)
            else
                true
            )
        ;
            ( Unif = deconstruct(_, _, _, _, _, _)
            ; Unif = assign(_, _)
            ; Unif = simple_test(_, _)
            ; Unif = complicated_unify(_, _, _)
            )
        ),
        vars_in_rhs_acc(RHS, !Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _, _, _),
        vars_in_generic_call(GenericCall, GenericCallVars),
        set_of_var.insert_list(GenericCallVars, !Set),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        vars_in_goals_acc(Goals, !Set)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        set_of_var.insert(Var, !Set),
        vars_in_cases_acc(Cases, !Set)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = exist_quant(Vars, _),
            set_of_var.insert_list(Vars, !Set)
        ;
            Reason = promise_solutions(Vars, _),
            set_of_var.insert_list(Vars, !Set)
        ;
            Reason = from_ground_term(Var, _),
            set_of_var.insert(Var, !Set)
        ;
            ( Reason = require_complete_switch(Var)
            ; Reason = require_switch_arms_detism(Var, _)
            ),
            set_of_var.insert(Var, !Set)
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            set_of_var.insert(LCVar, !Set),
            set_of_var.insert(LCSVar, !Set)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            )
        ),
        vars_in_goal_acc(SubGoal, !Set)
    ;
        GoalExpr = negation(SubGoal),
        vars_in_goal_acc(SubGoal, !Set)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        set_of_var.insert_list(Vars, !Set),
        vars_in_goal_acc(Cond, !Set),
        vars_in_goal_acc(Then, !Set),
        vars_in_goal_acc(Else, !Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        set_of_var.insert_list(ArgVars, !Set),
        set_of_var.insert_list(ExtraVars, !Set)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = atomic_goal(_, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, _),
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            set_of_var.insert(OuterDI, !Set),
            set_of_var.insert(OuterUO, !Set),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert(InnerDI, !Set),
            set_of_var.insert(InnerUO, !Set),
            (
                MaybeOutputVars = no
            ;
                MaybeOutputVars = yes(OutputVars),
                set_of_var.insert_list(OutputVars, !Set)
            ),
            vars_in_goal_acc(MainGoal, !Set),
            vars_in_goals_acc(OrElseGoals, !Set)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            % The IO and Result variables would be in SubGoal.
            vars_in_goal_acc(SubGoal, !Set)
        ;
            Shorthand = bi_implication(LeftGoal, RightGoal),
            vars_in_goal_acc(LeftGoal, !Set),
            vars_in_goal_acc(RightGoal, !Set)
        )
    ).

:- pred vars_in_goals_acc(list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

vars_in_goals_acc([], !Set).
vars_in_goals_acc([Goal | Goals], !Set) :-
    vars_in_goal_acc(Goal, !Set),
    vars_in_goals_acc(Goals, !Set).

:- pred vars_in_cases_acc(list(case)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

vars_in_cases_acc([], !Set).
vars_in_cases_acc([case(_, _, Goal) | Cases], !Set) :-
    vars_in_goal_acc(Goal, !Set),
    vars_in_cases_acc(Cases, !Set).

%---------------------%

non_svar_copy_vars_in_goal(Goal, !:Set) :-
    set_of_var.init(!:Set),
    non_svar_copy_vars_in_goal_acc(Goal, !Set).

:- pred non_svar_copy_vars_in_goal_acc(hlds_goal::in,
    set_of_progvar::in, set_of_progvar::out) is det.

non_svar_copy_vars_in_goal_acc(Goal, !Set) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(Var, RHS, _, Unif, _),
        GoalFeatures = goal_info_get_features(GoalInfo),
        ( if set.contains(GoalFeatures, feature_state_var_copy) then
            % Ignore this goal.
            true
        else
            set_of_var.insert(Var, !Set),
            (
                Unif = construct(_, _, _, _, CellToReuse, _, _),
                ( if CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) then
                    set_of_var.insert(Var, !Set)
                else
                    true
                )
            ;
                ( Unif = deconstruct(_, _, _, _, _, _)
                ; Unif = assign(_, _)
                ; Unif = simple_test(_, _)
                ; Unif = complicated_unify(_, _, _)
                )
            ),
            vars_in_rhs_acc(RHS, !Set)
        )
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _, _, _),
        vars_in_generic_call(GenericCall, GenericCallVars),
        set_of_var.insert_list(GenericCallVars, !Set),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        non_svar_copy_vars_in_goals_acc(Goals, !Set)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        set_of_var.insert(Var, !Set),
        non_svar_copy_vars_in_cases_acc(Cases, !Set)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = exist_quant(Vars, _),
            set_of_var.insert_list(Vars, !Set)
        ;
            Reason = promise_solutions(Vars, _),
            set_of_var.insert_list(Vars, !Set)
        ;
            Reason = from_ground_term(Var, _),
            set_of_var.insert(Var, !Set)
        ;
            ( Reason = require_complete_switch(Var)
            ; Reason = require_switch_arms_detism(Var, _)
            ),
            set_of_var.insert(Var, !Set)
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            set_of_var.insert(LCVar, !Set),
            set_of_var.insert(LCSVar, !Set)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            )
        ),
        non_svar_copy_vars_in_goal_acc(SubGoal, !Set)
    ;
        GoalExpr = negation(SubGoal),
        non_svar_copy_vars_in_goal_acc(SubGoal, !Set)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        set_of_var.insert_list(Vars, !Set),
        non_svar_copy_vars_in_goal_acc(Cond, !Set),
        non_svar_copy_vars_in_goal_acc(Then, !Set),
        non_svar_copy_vars_in_goal_acc(Else, !Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        set_of_var.insert_list(ArgVars, !Set),
        set_of_var.insert_list(ExtraVars, !Set)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = atomic_goal(_, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, _),
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            set_of_var.insert(OuterDI, !Set),
            set_of_var.insert(OuterUO, !Set),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert(InnerDI, !Set),
            set_of_var.insert(InnerUO, !Set),
            (
                MaybeOutputVars = no
            ;
                MaybeOutputVars = yes(OutputVars),
                set_of_var.insert_list(OutputVars, !Set)
            ),
            non_svar_copy_vars_in_goal_acc(MainGoal, !Set),
            non_svar_copy_vars_in_goals_acc(OrElseGoals, !Set)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            % The IO and Result variables would be in SubGoal.
            non_svar_copy_vars_in_goal_acc(SubGoal, !Set)
        ;
            Shorthand = bi_implication(LeftGoal, RightGoal),
            non_svar_copy_vars_in_goal_acc(LeftGoal, !Set),
            non_svar_copy_vars_in_goal_acc(RightGoal, !Set)
        )
    ).

:- pred non_svar_copy_vars_in_goals_acc(list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

non_svar_copy_vars_in_goals_acc([], !Set).
non_svar_copy_vars_in_goals_acc([Goal | Goals], !Set) :-
    non_svar_copy_vars_in_goal_acc(Goal, !Set),
    non_svar_copy_vars_in_goals_acc(Goals, !Set).

:- pred non_svar_copy_vars_in_cases_acc(list(case)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

non_svar_copy_vars_in_cases_acc([], !Set).
non_svar_copy_vars_in_cases_acc([case(_, _, Goal) | Cases], !Set) :-
    non_svar_copy_vars_in_goal_acc(Goal, !Set),
    non_svar_copy_vars_in_cases_acc(Cases, !Set).

%---------------------%

:- pred vars_in_rhs_acc(unify_rhs::in,
    set_of_progvar::in, set_of_progvar::out) is det.

vars_in_rhs_acc(RHS, !Set) :-
    (
        RHS = rhs_var(X),
        set_of_var.insert(X, !Set)
    ;
        RHS = rhs_functor(_Functor, _, ArgVars),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        RHS = rhs_lambda_goal(_, _, _, NonLocals, ArgVarsModes, _, Goal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        set_of_var.insert_list(NonLocals, !Set),
        set_of_var.insert_list(ArgVars, !Set),
        vars_in_goal_acc(Goal, !Set)
    ).

vars_in_generic_call(higher_order(Var, _, _, _, _), [Var]).
vars_in_generic_call(class_method(Var, _, _, _), [Var]).
vars_in_generic_call(event_call(_), []).
vars_in_generic_call(cast(_), []).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_vars.
%---------------------------------------------------------------------------%
