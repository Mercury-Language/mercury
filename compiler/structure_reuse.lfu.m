%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
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

:- import_module hlds.hlds_pred.

:- pred forward_use_information(proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module list.
:- import_module map.
:- import_module set.
:- import_module pair.

%-----------------------------------------------------------------------------%

forward_use_information(!ProcInfo) :-
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    proc_info_get_goal(!.ProcInfo, Goal0),

        % Set of variables initially instantiated.
    proc_info_get_liveness_info(!.ProcInfo, InstantiatedVars0),
        % Set of variables initially "dead" = instantiated variables that
        % syntactically do not occur in the remainder of the goal.
    set.init(DeadVars0),

    forward_use_in_goal(VarTypes, Goal0, Goal,
        remove_typeinfo_vars_from_set(VarTypes, InstantiatedVars0),
        _InstantiatedVars, DeadVars0, _DeadVars),

    proc_info_set_goal(Goal, !ProcInfo).

:- pred forward_use_in_goal(vartypes::in, hlds_goal::in, hlds_goal::out,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det.

forward_use_in_goal(VarTypes, !Goal, !InstantiatedVars, !DeadVars) :-
    (
        !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
        goal_is_atomic(GoalExpr0)
    ->
        InstantiatedVars0 = !.InstantiatedVars,
        compute_instantiated_and_dead_vars(VarTypes, GoalInfo0,
            !InstantiatedVars, !DeadVars),
        set.difference(InstantiatedVars0, !.DeadVars, LFU),
        goal_info_set_lfu(LFU, GoalInfo0, GoalInfo),
        !:Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        forward_use_in_composite_goal(VarTypes, !Goal,
            !InstantiatedVars, !DeadVars)
    ).

:- pred compute_instantiated_and_dead_vars(vartypes::in, hlds_goal_info::in,
    set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det.

compute_instantiated_and_dead_vars(VarTypes, Info, !Inst, !Dead) :-
    % Inst = Inst0 + birth-set
    % Dead = Dead0 + death-set
    goal_info_get_pre_births(Info, PreBirths),
    goal_info_get_post_births(Info, PostBirths),
    goal_info_get_post_deaths(Info, PostDeaths),
    goal_info_get_pre_deaths(Info, PreDeaths),
    !:Inst = set.union_list([
        remove_typeinfo_vars_from_set(VarTypes, PreBirths),
        remove_typeinfo_vars_from_set(VarTypes, PostBirths), !.Inst]),
    !:Dead = set.union_list([PreDeaths, PostDeaths, !.Dead]).

:- pred forward_use_in_composite_goal(vartypes::in, hlds_goal::in,
    hlds_goal::out, set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det.

forward_use_in_composite_goal(VarTypes, !Goal, !InstantiatedVars,
        !DeadVars) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    InstantiadedBefore = !.InstantiatedVars,

    (
        GoalExpr0 = conj(A,Goals0)
    ->
        forward_use_in_conj(VarTypes, Goals0, Goals,
            !InstantiatedVars, !DeadVars),
        GoalExpr = conj(A, Goals)
    ;
        GoalExpr0 = switch(A, B, Cases0)
    ->
        forward_use_in_cases(VarTypes, Cases0, Cases,
            !InstantiatedVars, !DeadVars),
        GoalExpr = switch(A, B, Cases)
    ;
        GoalExpr0 = disj(Disj0)
    ->
        forward_use_in_disj(VarTypes, Disj0, Disj,
            !InstantiatedVars, !DeadVars),
        GoalExpr = disj(Disj)
    ;
        GoalExpr0 = negation(Goal0)
    ->
        forward_use_in_goal(VarTypes, Goal0, Goal,
            !InstantiatedVars, !DeadVars),
        GoalExpr = negation(Goal)
    ;
        GoalExpr0 = scope(A, Goal0)
    ->
        forward_use_in_goal(VarTypes, Goal0, Goal,
            !InstantiatedVars, !DeadVars),
        GoalExpr = scope(A, Goal)
    ;
        GoalExpr0 = if_then_else(V, Cond0, Then0, Else0)
    ->
        Inst0 = !.InstantiatedVars,
        Dead0 = !.DeadVars,
        forward_use_in_goal(VarTypes, Cond0, Cond,
            !InstantiatedVars, !DeadVars),
        forward_use_in_goal(VarTypes, Then0, Then,
            !InstantiatedVars, !DeadVars),
        forward_use_in_goal(VarTypes, Else0, Else, Inst0, Inst1, Dead0, Dead1),
        set.union(Inst1, !InstantiatedVars),
        set.union(Dead1, !DeadVars),
        GoalExpr = if_then_else(V, Cond, Then, Else)
    ;
        unexpected(this_file,
            "Atomic goal in forward_use_in_composite_goal.")
    ),
    set.difference(InstantiadedBefore, !.DeadVars, LFU),
    goal_info_set_lfu(LFU, GoalInfo0, GoalInfo),
    !:Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred forward_use_in_conj(vartypes::in, list(hlds_goal)::in,
    list(hlds_goal)::out, set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det.

forward_use_in_conj(VarTypes, !Goals, !InstantiatedVars, !DeadVars) :-
    list.map_foldl2(forward_use_in_goal(VarTypes), !Goals,
        !InstantiatedVars, !DeadVars).

:- pred forward_use_in_cases(vartypes::in, list(case)::in, list(case)::out,
    set(prog_var)::in, set(prog_var)::out, set(prog_var)::in,
    set(prog_var)::out) is det.

forward_use_in_cases(VarTypes, !Cases, !InstantiatedVars, !DeadVars) :-
    Inst0 = !.InstantiatedVars,
    Dead0 = !.DeadVars,
    list.map_foldl2(forward_use_in_case(VarTypes, Inst0, Dead0),
        !Cases, !InstantiatedVars, !DeadVars).

:- pred forward_use_in_case(vartypes::in, set(prog_var)::in,
    set(prog_var)::in, case::in, case::out, set(prog_var)::in,
    set(prog_var)::out, set(prog_var)::in, set(prog_var)::out) is det.

forward_use_in_case(VarTypes, Inst0, Dead0, !Case,
        !InstantiatedVars, !DeadVars) :-
    !.Case = case(Cons, Goal0),
    forward_use_in_goal(VarTypes, Goal0, Goal, Inst0, Inst, Dead0, Dead),
    !:Case = case(Cons, Goal),
    set.union(Inst, !InstantiatedVars),
    set.union(Dead, !DeadVars).

:- pred forward_use_in_disj(vartypes::in, list(hlds_goal)::in,
    list(hlds_goal)::out, set(prog_var)::in, set(prog_var)::out,
    set(prog_var)::in, set(prog_var)::out) is det.

forward_use_in_disj(VarTypes, !Goals, !InstantiatedVars, !DeadVars):-
    Inst0 = !.InstantiatedVars,
    Dead0 = !.DeadVars,
    list.map_foldl2(forward_use_in_disj_goal(VarTypes, Inst0, Dead0),
        !Goals, !InstantiatedVars, !DeadVars).

:- pred forward_use_in_disj_goal(vartypes::in, set(prog_var)::in,
    set(prog_var)::in, hlds_goal::in, hlds_goal::out, set(prog_var)::in,
    set(prog_var)::out, set(prog_var)::in, set(prog_var)::out) is det.

forward_use_in_disj_goal(VarTypes, Inst0, Dead0, !Goal,
        !InstantiatedVars, !DeadVars) :-
    forward_use_in_goal(VarTypes, !Goal, Inst0, Inst, Dead0, Dead),
    set.union(Inst, !InstantiatedVars),
    set.union(Dead, !DeadVars).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.lfu.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.lfu.
%-----------------------------------------------------------------------------%
