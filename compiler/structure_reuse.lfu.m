%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.lfu.m.
% Main authors: nancy.
%
% Implementation of the process of annotating each program point within
% a procedure with local forward use information.
%
% At a program point (a goal), a variable is called in local forward use iff
% * it was already instantiated before the goal
% * and it is (syntactically) used in the goals following the current goal in
% forward execution.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.lfu.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.set_of_var.

%-----------------------------------------------------------------------------%

:- pred forward_use_information(module_info::in,
    proc_info::in, proc_info::out) is det.

    % add_vars_to_lfu(Vars, !ProcInfo).
    %
    % Add the vars to all the LFU sets in the body of the procedure.
    %
:- pred add_vars_to_lfu(set_of_progvar::in, proc_info::in, proc_info::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

forward_use_information(ModuleInfo, !ProcInfo) :-
    proc_info_get_var_table(ModuleInfo, !.ProcInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, Goal0),

    % Set of variables initially instantiated.
    proc_info_get_liveness_info(!.ProcInfo, InstantiatedVars0),
    % Set of variables initially "dead" = instantiated variables that
    % syntactically do not occur in the remainder of the goal.
    set_of_var.init(DeadVars0),

    forward_use_in_goal(VarTable, Goal0, Goal,
        remove_typeinfo_vars_from_set_of_var(VarTable, InstantiatedVars0),
        _InstantiatedVars, DeadVars0, _DeadVars),

    proc_info_set_goal(Goal, !ProcInfo).

:- pred forward_use_in_goal(var_table::in, hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_goal(VarTable, !Goal, !InstantiatedVars, !DeadVars) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    HasSubGoals = goal_expr_has_subgoals(GoalExpr0),
    (
        HasSubGoals = does_not_have_subgoals,
        InstantiatedVars0 = !.InstantiatedVars,
        compute_instantiated_and_dead_vars(VarTable, GoalInfo0,
            !InstantiatedVars, !DeadVars),
        set_of_var.difference(InstantiatedVars0, !.DeadVars, LFU),
        goal_info_set_lfu(LFU, GoalInfo0, GoalInfo),
        !:Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        HasSubGoals = has_subgoals,
        goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
        set_of_var.union(PreDeaths, !DeadVars),
        forward_use_in_composite_goal(VarTable, !Goal,
            !InstantiatedVars, !DeadVars)
    ).

:- pred compute_instantiated_and_dead_vars(var_table::in, hlds_goal_info::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

compute_instantiated_and_dead_vars(VarTable, Info, !Inst, !Dead) :-
    % Inst = Inst0 + birth-set
    % Dead = Dead0 + death-set
    goal_info_get_pre_births(Info, PreBirths),
    goal_info_get_post_births(Info, PostBirths),
    goal_info_get_post_deaths(Info, PostDeaths),
    goal_info_get_pre_deaths(Info, PreDeaths),
    !:Inst = set_of_var.union_list([
        remove_typeinfo_vars_from_set_of_var(VarTable, PreBirths),
        remove_typeinfo_vars_from_set_of_var(VarTable, PostBirths),
        !.Inst]),
    !:Dead = set_of_var.union_list([PreDeaths, PostDeaths, !.Dead]).

:- pred forward_use_in_composite_goal(var_table::in, hlds_goal::in,
    hlds_goal::out, set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_composite_goal(VarTable, !Goal, !InstantiatedVars,
        !DeadVars) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    InstantiadedBefore = !.InstantiatedVars,

    (
        GoalExpr0 = conj(ConjType, Goals0),
        forward_use_in_conj(VarTable, Goals0, Goals,
            !InstantiatedVars, !DeadVars),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        forward_use_in_cases(VarTable, Cases0, Cases,
            !InstantiatedVars, !DeadVars),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = disj(Disj0),
        forward_use_in_disj(VarTable, Disj0, Disj,
            !InstantiatedVars, !DeadVars),
        GoalExpr = disj(Disj)
    ;
        GoalExpr0 = negation(SubGoal0),
        forward_use_in_goal(VarTable, SubGoal0, SubGoal,
            !InstantiatedVars, !DeadVars),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        forward_use_in_goal(VarTable, SubGoal0, SubGoal,
            !InstantiatedVars, !DeadVars),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        Inst0 = !.InstantiatedVars,
        Dead0 = !.DeadVars,
        forward_use_in_goal(VarTable, Cond0, Cond,
            !InstantiatedVars, !DeadVars),
        forward_use_in_goal(VarTable, Then0, Then,
            !InstantiatedVars, !DeadVars),
        forward_use_in_goal(VarTable, Else0, Else, Inst0, Inst1, Dead0, Dead1),
        set_of_var.union(Inst1, !InstantiatedVars),
        set_of_var.union(Dead1, !DeadVars),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        unexpected($pred, "atomic goal")
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),
    set_of_var.difference(InstantiadedBefore, !.DeadVars, LFU),
    goal_info_set_lfu(LFU, GoalInfo0, GoalInfo),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred forward_use_in_conj(var_table::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_conj(VarTable, !Goals, !InstantiatedVars, !DeadVars) :-
    list.map_foldl2(forward_use_in_goal(VarTable), !Goals,
        !InstantiatedVars, !DeadVars).

:- pred forward_use_in_cases(var_table::in, list(case)::in, list(case)::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_cases(VarTable, !Cases, !InstantiatedVars, !DeadVars) :-
    Inst0 = !.InstantiatedVars,
    Dead0 = !.DeadVars,
    list.map_foldl2(forward_use_in_case(VarTable, Inst0, Dead0),
        !Cases, !InstantiatedVars, !DeadVars).

:- pred forward_use_in_case(var_table::in, set_of_progvar::in,
    set_of_progvar::in, case::in, case::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_case(VarTable, Inst0, Dead0, !Case,
        !InstantiatedVars, !DeadVars) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    forward_use_in_goal(VarTable, Goal0, Goal, Inst0, Inst, Dead0, Dead),
    !:Case = case(MainConsId, OtherConsIds, Goal),
    set_of_var.union(Inst, !InstantiatedVars),
    set_of_var.union(Dead, !DeadVars).

:- pred forward_use_in_disj(var_table::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_disj(VarTable, !Goals, !InstantiatedVars, !DeadVars):-
    Inst0 = !.InstantiatedVars,
    Dead0 = !.DeadVars,
    list.map_foldl2(forward_use_in_disj_goal(VarTable, Inst0, Dead0),
        !Goals, !InstantiatedVars, !DeadVars).

:- pred forward_use_in_disj_goal(var_table::in, set_of_progvar::in,
    set_of_progvar::in, hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

forward_use_in_disj_goal(VarTable, Inst0, Dead0, !Goal,
        !InstantiatedVars, !DeadVars) :-
    forward_use_in_goal(VarTable, !Goal, Inst0, Inst, Dead0, Dead),
    set_of_var.union(Inst, !InstantiatedVars),
    set_of_var.union(Dead, !DeadVars).

%-----------------------------------------------------------------------------%

add_vars_to_lfu(ForceInUse, !ProcInfo) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    add_vars_to_lfu_in_goal(ForceInUse, Goal0, Goal),
    proc_info_set_goal(Goal, !ProcInfo).

:- pred add_vars_to_lfu_in_goal(set_of_progvar::in,
    hlds_goal::in, hlds_goal::out) is det.

add_vars_to_lfu_in_goal(ForceInUse, Goal0, Goal) :-
    Goal0 = hlds_goal(Expr0, GoalInfo0),
    add_vars_to_lfu_in_goal_expr(ForceInUse, Expr0, Expr),
    LFU0 = goal_info_get_lfu(GoalInfo0),
    LFU = set_of_var.union(ForceInUse, LFU0),
    goal_info_set_lfu(LFU, GoalInfo0, GoalInfo1),
    goal_info_set_reuse(no_reuse_info, GoalInfo1, GoalInfo),
    Goal = hlds_goal(Expr, GoalInfo).

:- pred add_vars_to_lfu_in_goal_expr(set_of_progvar::in,
    hlds_goal_expr::in, hlds_goal_expr::out) is det.

add_vars_to_lfu_in_goal_expr(ForceInUse, Expr0, Expr) :-
    (
        Expr0 = conj(ConjType, Goals0),
        add_vars_to_lfu_in_goals(ForceInUse, Goals0, Goals),
        Expr = conj(ConjType, Goals)
    ;
        Expr0 = disj(Goals0),
        add_vars_to_lfu_in_goals(ForceInUse, Goals0, Goals),
        Expr = disj(Goals)
    ;
        Expr0 = switch(Var, Det, Cases0),
        add_vars_to_lfu_in_cases(ForceInUse, Cases0, Cases),
        Expr = switch(Var, Det, Cases)
    ;
        Expr0 = if_then_else(Vars, Cond0, Then0, Else0),
        add_vars_to_lfu_in_goal(ForceInUse, Cond0, Cond),
        add_vars_to_lfu_in_goal(ForceInUse, Then0, Then),
        add_vars_to_lfu_in_goal(ForceInUse, Else0, Else),
        Expr = if_then_else(Vars, Cond, Then, Else)
    ;
        Expr0 = negation(Goal0),
        add_vars_to_lfu_in_goal(ForceInUse, Goal0, Goal),
        Expr = negation(Goal)
    ;
        Expr0 = scope(Reason, Goal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        add_vars_to_lfu_in_goal(ForceInUse, Goal0, Goal),
        Expr = scope(Reason, Goal)
    ;
        ( Expr0 = generic_call(_, _, _, _, _)
        ; Expr0 = plain_call(_, _, _, _, _, _)
        ; Expr0 = unify(_, _, _, _, _)
        ; Expr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Expr = Expr0
    ;
        Expr0 = shorthand(Shorthand0),
        (
            Shorthand0 = atomic_goal(GoalType, Outer, Inner,
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            add_vars_to_lfu_in_goal(ForceInUse, MainGoal0, MainGoal),
            add_vars_to_lfu_in_goals(ForceInUse, OrElseGoals0, OrElseGoals),
            Shorthand = atomic_goal(GoalType, Outer, Inner,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners)
        ;
            Shorthand0 = bi_implication(LeftGoal0, RightGoal0),
            add_vars_to_lfu_in_goal(ForceInUse, LeftGoal0, LeftGoal),
            add_vars_to_lfu_in_goal(ForceInUse, RightGoal0, RightGoal),
            Shorthand = bi_implication(LeftGoal, RightGoal)
        ;
            Shorthand0 = try_goal(_, _, _),
            unexpected($pred, "try_goal")
        ),
        Expr = shorthand(Shorthand)
    ).

:- pred add_vars_to_lfu_in_goals(set_of_progvar::in,
    hlds_goals::in, hlds_goals::out) is det.

add_vars_to_lfu_in_goals(_, [], []).
add_vars_to_lfu_in_goals(ForceInUse, [Goal0 | Goals0], [Goal | Goals]) :-
    add_vars_to_lfu_in_goal(ForceInUse, Goal0, Goal),
    add_vars_to_lfu_in_goals(ForceInUse, Goals0, Goals).

:- pred add_vars_to_lfu_in_cases(set_of_progvar::in,
    list(case)::in, list(case)::out) is det.

add_vars_to_lfu_in_cases(_, [], []).
add_vars_to_lfu_in_cases(ForceInUse, [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    add_vars_to_lfu_in_goal(ForceInUse, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    add_vars_to_lfu_in_cases(ForceInUse, Cases0, Cases).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.lfu.
%-----------------------------------------------------------------------------%
