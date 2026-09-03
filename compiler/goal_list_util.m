%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_list_util.m.
%
% Utility operations dealing with lists of goals.
% (Utility operations dealing with single goals are in goal_util.m.)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_list_util.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module list.

%---------------------------------------------------------------------------%

:- func get_hlds_goal_expr(hlds_goal) = hlds_goal_expr.
:- func get_hlds_goal_info(hlds_goal) = hlds_goal_info.

%---------------------------------------------------------------------------%

    % Convert a goal to a list of conjuncts.
    % If the goal is a conjunction, then return its conjuncts,
    % otherwise return the goal as a singleton list.
    %
:- pred goal_to_conj_list(hlds_goal::in, list(hlds_goal)::out) is det.

    % Convert a goal to a list of parallel conjuncts.
    % If the goal is a parallel conjunction, then return its conjuncts,
    % otherwise return the goal as a singleton list.
    %
:- pred goal_to_par_conj_list(hlds_goal::in, list(hlds_goal)::out) is det.

    % Convert a goal to a list of disjuncts.
    % If the goal is a disjunction, then return its disjuncts,
    % otherwise return the goal as a singleton list.
    %
:- pred goal_to_disj_list(hlds_goal::in, list(hlds_goal)::out) is det.

    % Convert a list of conjuncts to a goal.
    % If the list contains only one goal, then return that goal,
    % otherwise return the conjunction of the conjuncts,
    % with the specified goal_info.
    %
:- pred conj_list_to_goal(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out) is det.

    % Convert a list of parallel conjuncts to a goal.
    % If the list contains only one goal, then return that goal,
    % otherwise return the parallel conjunction of the conjuncts,
    % with the specified goal_info.
    %
:- pred par_conj_list_to_goal(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out) is det.

    % Convert a list of disjuncts to a goal.
    % If the list contains only one goal, then return that goal,
    % otherwise return the disjunction of the disjuncts,
    % with the specified goal_info.
    %
:- pred disj_list_to_goal(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out) is det.

    % Takes a goal and a list of goals, and conjoins them
    % (with a potentially blank goal_info).
    %
:- pred conjoin_goal_and_goal_list(hlds_goal::in, list(hlds_goal)::in,
    hlds_goal::out) is det.

    % Conjoin two goals (with a potentially blank goal_info).
    %
:- pred conjoin_goals(hlds_goal::in, hlds_goal::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % Create a conjunction of the specified type using the specified two goals.
    % This fills in the hlds_goal_info.
    %
:- pred create_conj(hlds_goal::in, hlds_goal::in, conj_type::in,
    hlds_goal::out) is det.

    % Create a conjunction of the specified type using the specified goals,
    % This fills in the hlds_goal_info.
    %
:- pred create_conj_from_list(list(hlds_goal)::in, conj_type::in,
    hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % Return the union of all the nonlocals of a list of goals.
    %
:- pred goal_list_nonlocals(list(hlds_goal)::in, set_of_progvar::out) is det.

    % Compute the instmap_delta resulting from applying
    % all the instmap_deltas of the given goals.
    %
:- pred goal_list_instmap_delta(list(hlds_goal)::in, instmap_delta::out)
    is det.

    % Compute the determinism of a list of goals.
    %
:- pred goal_list_determinism(list(hlds_goal)::in, determinism::out) is det.

    % Compute the purity of a list of goals.
:- pred goal_list_purity(list(hlds_goal)::in, purity::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_detism.

:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

get_hlds_goal_expr(Goal) = Goal ^ hg_expr.
get_hlds_goal_info(Goal) = Goal ^ hg_info.

%---------------------------------------------------------------------------%
%
% Miscellaneous utility procedures for dealing with HLDS goals.
%

goal_to_conj_list(Goal, ConjList) :-
    ( if Goal = hlds_goal(conj(plain_conj, List), _) then
        ConjList = List
    else
        ConjList = [Goal]
    ).

goal_to_par_conj_list(Goal, ConjList) :-
    ( if Goal = hlds_goal(conj(parallel_conj, List), _) then
        ConjList = List
    else
        ConjList = [Goal]
    ).

goal_to_disj_list(Goal, DisjList) :-
    ( if Goal = hlds_goal(disj(List), _) then
        DisjList = List
    else
        DisjList = [Goal]
    ).

conj_list_to_goal(ConjList, GoalInfo, Goal) :-
    ( if ConjList = [Goal0] then
        Goal = Goal0
    else
        Goal = hlds_goal(conj(plain_conj, ConjList), GoalInfo)
    ).

par_conj_list_to_goal(ConjList, GoalInfo, Goal) :-
    ( if ConjList = [Goal0] then
        Goal = Goal0
    else
        Goal = hlds_goal(conj(parallel_conj, ConjList), GoalInfo)
    ).

disj_list_to_goal(DisjList, GoalInfo, Goal) :-
    ( if DisjList = [Goal0] then
        Goal = Goal0
    else
        Goal = hlds_goal(disj(DisjList), GoalInfo)
    ).

conjoin_goal_and_goal_list(Goal0, Goals, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if GoalExpr0 = conj(plain_conj, GoalList0) then
        GoalList = GoalList0 ++ Goals,
        GoalExpr = conj(plain_conj, GoalList)
    else
        GoalExpr = conj(plain_conj, [Goal0 | Goals])
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo0).

conjoin_goals(Goal1, Goal2, Goal) :-
    ( if Goal2 = hlds_goal(conj(plain_conj, Goals2), _) then
        GoalList = Goals2
    else
        GoalList = [Goal2]
    ),
    conjoin_goal_and_goal_list(Goal1, GoalList, Goal).

%---------------------------------------------------------------------------%

create_conj(GoalA, GoalB, Type, ConjGoal) :-
    create_conj_from_list([GoalA, GoalB], Type, ConjGoal).

create_conj_from_list(Conjuncts, ConjType, ConjGoal) :-
    (
        Conjuncts = [HeadGoal | TailGoals],
        (
            TailGoals = [ _ | _ ],
            ConjGoalExpr = conj(ConjType, Conjuncts),
            goal_list_nonlocals(Conjuncts, NonLocals),
            goal_list_instmap_delta(Conjuncts, InstMapDelta),
            goal_list_determinism(Conjuncts, Detism),
            goal_list_purity(Conjuncts, Purity),
            HeadGoal = hlds_goal(_, HeadGoalInfo),
            Context = goal_info_get_context(HeadGoalInfo),
            goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
                ConjGoalInfo),
            ConjGoal = hlds_goal(ConjGoalExpr, ConjGoalInfo)
        ;
            TailGoals = [],
            ConjGoal = HeadGoal
        )
    ;
        Conjuncts = [],
        unexpected($pred, "empty conjunction")
    ).

%---------------------------------------------------------------------------%

goal_list_nonlocals(Goals, NonLocals) :-
    GoalNonLocals = list.map(goal_get_nonlocals, Goals),
    set_of_var.union_list(GoalNonLocals, NonLocals).

goal_list_instmap_delta(Goals, InstMapDelta) :-
    ApplyDelta =
        ( pred(Goal::in, Delta0::in, Delta::out) is det :-
            Goal = hlds_goal(_, GoalInfo),
            Delta1 = goal_info_get_instmap_delta(GoalInfo),
            instmap_delta_apply_instmap_delta(Delta0, Delta1, test_size, Delta)
        ),
    instmap_delta_init_reachable(InstMapDelta0),
    list.foldl(ApplyDelta, Goals, InstMapDelta0, InstMapDelta).

goal_list_determinism(Goals, Determinism) :-
    ComputeDeterminism =
        ( pred(Goal::in, Det0::in, Det::out) is det :-
            Goal = hlds_goal(_, GoalInfo),
            Det1 = goal_info_get_determinism(GoalInfo),
            det_conjunction_detism(Det0, Det1, Det)
        ),
    list.foldl(ComputeDeterminism, Goals, detism_det, Determinism).

goal_list_purity(Goals, GoalsPurity) :-
    ComputePurity =
        ( pred(Goal::in, Purity0::in, Purity::out) is det :-
            Goal = hlds_goal(_, GoalInfo),
            Purity1 = goal_info_get_purity(GoalInfo),
            worst_purity(Purity0, Purity1) = Purity
        ),
    list.foldl(ComputePurity, Goals, purity_pure, GoalsPurity).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_list_util.
%---------------------------------------------------------------------------%
