%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2011 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: goal_path.m.
% Main author: zs.
%
% This module looks after goal ids, which give every goal a unique id within
% its procedure definition, and goal paths, which map each goal id to its
% goal's position in the procedure definition.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.goal_path.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bimap.
:- import_module map.

:- type containing_goal
    --->    whole_body_goal
            % This goal is the entire body of its procedure.
    ;       containing_goal(goal_id, goal_path_step).
            % This goal is an contained immediately inside the larger goal
            % identified by the goal_id, from which you need to take the
            % given goal_path step to get to this goal.
            %
            % The goal_id of the containing goal is guaranteed to be always
            % less than the goal_id of this goal.

:- type containing_goal_map == map(goal_id, containing_goal).
:- type goal_forward_path_map == map(goal_id, forward_goal_path).
:- type goal_reverse_path_map == map(goal_id, reverse_goal_path).
:- type goal_reverse_path_bimap == bimap(goal_id, reverse_goal_path).

    % goal_id_inside(ContainingGoalMap, GoalIdA, GoalIdB):
    %
    % Succeeds if GoalIdB denotes a goal *inside* the goal denoted by GoalIdA.
    % (It considers a goal to be inside itself.)
    %
:- pred goal_id_inside(containing_goal_map::in,
    goal_id::in, goal_id::in) is semidet.

    % Convert a goal_id to a forward goal path.
    %
:- func goal_id_to_forward_path(containing_goal_map, goal_id) =
    forward_goal_path.

    % Convert a goal_id to a reverse goal path.
    %
:- func goal_id_to_reverse_path(containing_goal_map, goal_id) =
    reverse_goal_path.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a forwward goal path.
    %
:- func create_forward_goal_path_map(containing_goal_map) =
    goal_forward_path_map.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a reverse goal path.
    %
:- func create_reverse_goal_path_map(containing_goal_map) =
    goal_reverse_path_map.

    % Given a containing_goal_map, create a map that maps each goal_id in it
    % to a reverse goal path, and back.
    %
:- func create_reverse_goal_path_bimap(containing_goal_map) =
    goal_reverse_path_bimap.

%-----------------------------------------------------------------------------%

    % IMPORTANT: the type constraint_id in hlds_data.m makes use of goal ids
    % to identify goals that impose constraints between the typechecking pass
    % and the polymorphism pass. For this reason, goal ids should not be
    % recalculated anywhere between these two passes. See the XXX comment
    % near the declaration of constraint_id.
    %
:- pred fill_goal_id_slots_in_proc(module_info::in,
    containing_goal_map::out, proc_info::in, proc_info::out) is det.

:- pred fill_goal_id_slots_in_proc_body(module_info::in, vartypes::in,
    containing_goal_map::out, hlds_goal::in, hlds_goal::out) is det.

    % Fill in the goal_ids for goals in the clauses_info.
    % Clauses are given goal paths `disj(1)', ...,  `disj(N)'.
    %
:- pred fill_goal_id_slots_in_clauses(module_info::in,
    containing_goal_map::out, clauses_info::in, clauses_info::out) is det.

%-----------------------------------------------------------------------------%

    % Fill in the goal path slots in the given procedure.
    % This predicate is here ONLY to support the RBMM and GTGC modules,
    % which are hard to transition to make use of goal_ids instead;
    % all new code should instead use the predicates above that fill in
    % the goal_id slots instead.
    %
:- pred fill_goal_path_slots_in_proc(module_info::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module svbimap.
:- import_module svmap.

%-----------------------------------------------------------------------------%

goal_id_inside(ContainingGoalId, GoalIdA, GoalIdB) :-
    (
        GoalIdB = GoalIdA
    ;
        map.lookup(ContainingGoalId, GoalIdB, GoalContainingB),
        GoalContainingB = containing_goal(ParentGoalIdB, _),
        goal_id_inside(ContainingGoalId, GoalIdA, ParentGoalIdB)
    ).

goal_id_to_forward_path(ContainingGoalMap, GoalId) = GoalPath :-
    StepsCord = goal_id_to_steps(ContainingGoalMap, GoalId),
    Steps = cord.list(StepsCord),
    GoalPath = fgp(Steps).

goal_id_to_reverse_path(ContainingGoalMap, GoalId) = GoalPath :-
    StepsCord = goal_id_to_steps(ContainingGoalMap, GoalId),
    Steps = cord.list(StepsCord),
    list.reverse(Steps, RevSteps),
    GoalPath = rgp(RevSteps).

:- func goal_id_to_steps(containing_goal_map, goal_id) =
    cord(goal_path_step).

goal_id_to_steps(ContainingGoalMap, GoalId) = Steps :-
    map.lookup(ContainingGoalMap, GoalId, ContainingGoal),
    (
        ContainingGoal = whole_body_goal,
        Steps = cord.empty
    ;
        ContainingGoal = containing_goal(ParentGoalId, LastStep),
        EarlierSteps = goal_id_to_steps(ContainingGoalMap, ParentGoalId),
        Steps = cord.snoc(EarlierSteps, LastStep)
    ).

create_forward_goal_path_map(ContainingGoalMap) = ForwardGoalPathMap :-
    ReverseGoalPathMap = create_reverse_goal_path_map(ContainingGoalMap),
    map.map_values_only(rgp_to_fgp, ReverseGoalPathMap, ForwardGoalPathMap).

:- pred rgp_to_fgp(reverse_goal_path::in, forward_goal_path::out) is det.

rgp_to_fgp(rgp(RevSteps), fgp(Steps)) :-
    list.reverse(RevSteps, Steps).

create_reverse_goal_path_map(ContainingGoalMap) = ReverseGoalPathMap :-
    map.to_assoc_list(ContainingGoalMap, ContainingGoalList),
    create_reverse_goal_path_map_2(ContainingGoalList,
        map.init, ReverseGoalPathMap).

:- pred create_reverse_goal_path_map_2(
    assoc_list(goal_id, containing_goal)::in,
    map(goal_id, reverse_goal_path)::in, map(goal_id, reverse_goal_path)::out)
    is det.

create_reverse_goal_path_map_2([], !ReverseGoalPathMap).
create_reverse_goal_path_map_2([Head | Tail], !ReverseGoalPathMap) :-
    Head = GoalId - ContainingGoal,
    (
        ContainingGoal = whole_body_goal,
        GoalReversePath = rgp([])
    ;
        ContainingGoal = containing_goal(ContainingGoalId, Step),
        map.lookup(!.ReverseGoalPathMap, ContainingGoalId,
            ContainingGoalReversePath),
        ContainingGoalReversePath = rgp(ContainingGoalReverseSteps),
        GoalReverseSteps = [Step | ContainingGoalReverseSteps],
        GoalReversePath = rgp(GoalReverseSteps)
    ),
    svmap.det_insert(GoalId, GoalReversePath, !ReverseGoalPathMap),
    create_reverse_goal_path_map_2(Tail, !ReverseGoalPathMap).

create_reverse_goal_path_bimap(ContainingGoalMap) = ReverseGoalPathBiMap :-
    map.to_assoc_list(ContainingGoalMap, ContainingGoalList),
    create_reverse_goal_path_bimap_2(ContainingGoalList,
        bimap.init, ReverseGoalPathBiMap).

:- pred create_reverse_goal_path_bimap_2(
    assoc_list(goal_id, containing_goal)::in,
    bimap(goal_id, reverse_goal_path)::in,
    bimap(goal_id, reverse_goal_path)::out) is det.

create_reverse_goal_path_bimap_2([], !ReverseGoalPathBiMap).
create_reverse_goal_path_bimap_2([Head | Tail], !ReverseGoalPathBiMap) :-
    Head = GoalId - ContainingGoal,
    (
        ContainingGoal = whole_body_goal,
        GoalReversePath = rgp([])
    ;
        ContainingGoal = containing_goal(ContainingGoalId, Step),
        bimap.lookup(!.ReverseGoalPathBiMap, ContainingGoalId,
            ContainingGoalReversePath),
        ContainingGoalReversePath = rgp(ContainingGoalReverseSteps),
        GoalReverseSteps = [Step | ContainingGoalReverseSteps],
        GoalReversePath = rgp(GoalReverseSteps)
    ),
    svbimap.det_insert(GoalId, GoalReversePath, !ReverseGoalPathBiMap),
    create_reverse_goal_path_bimap_2(Tail, !ReverseGoalPathBiMap).

%-----------------------------------------------------------------------------%

:- type slot_info
    --->    slot_info(
                slot_info_module_info               :: module_info,
                slot_info_vartypes                  :: vartypes
            ).

fill_goal_id_slots_in_proc(ModuleInfo, ContainingGoalMap, !ProcInfo) :-
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    proc_info_get_goal(!.ProcInfo, Goal0),
    fill_goal_id_slots_in_proc_body(ModuleInfo, VarTypes,
        ContainingGoalMap, Goal0, Goal),
    proc_info_set_goal(Goal, !ProcInfo).

fill_goal_id_slots_in_proc_body(ModuleInfo, VarTypes, ContainingGoalMap,
        Goal0, Goal) :-
    SlotInfo = slot_info(ModuleInfo, VarTypes),
    fill_goal_id_slots(SlotInfo, whole_body_goal, 0, _,
        map.init, ContainingGoalMap, Goal0, Goal).

fill_goal_id_slots_in_clauses(ModuleInfo, ContainingGoalMap,
        ClausesInfo0, ClausesInfo) :-
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers),
    get_clause_list(ClausesRep0, Clauses0),
    clauses_info_get_vartypes(ClausesInfo0, VarTypes),
    SlotInfo = slot_info(ModuleInfo, VarTypes),
    list.map_foldl3(fill_slots_in_clause(SlotInfo),
        Clauses0, Clauses, 1, _, 1, _, map.init, ContainingGoalMap),
    set_clause_list(Clauses, ClausesRep),
    clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
        ClausesInfo0, ClausesInfo).

:- pred fill_slots_in_clause(slot_info::in, clause::in, clause::out,
    int::in, int::out, int::in, int::out,
    containing_goal_map::in, containing_goal_map::out) is det.

fill_slots_in_clause(SlotInfo, Clause0, Clause, !GoalNum, !ClauseNum,
        !ContainingGoalMap) :-
    Goal0 = Clause0 ^ clause_body,
    ContainingGoal = containing_goal(whole_body_goal_id,
        step_disj(!.ClauseNum)),
    !:ClauseNum = !.ClauseNum + 1,
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalMap,
        Goal0, Goal),
    Clause = Clause0 ^ clause_body := Goal.

fill_goal_path_slots_in_proc(ModuleInfo, !Proc) :-
    proc_info_get_goal(!.Proc, Goal0),
    proc_info_get_vartypes(!.Proc, VarTypes),
    SlotInfo = slot_info(ModuleInfo, VarTypes),
    fill_goal_path_slots([], SlotInfo, Goal0, Goal),
    proc_info_set_goal(Goal, !Proc).

%-----------------------------------------------------------------------------%

:- pred fill_goal_id_slots(slot_info::in, containing_goal::in,
    int::in, int::out, containing_goal_map::in, containing_goal_map::out,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalMap,
        Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    GoalId = goal_id(!.GoalNum),
    !:GoalNum = !.GoalNum + 1,
    goal_info_set_goal_id(GoalId, GoalInfo0, GoalInfo),
    svmap.det_insert(GoalId, ContainingGoal, !ContainingGoalMap),
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Kind, Context),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocals, QuantVars, LambdaModes, Detism, LambdaGoal0),
            fill_goal_id_slots(SlotInfo,
                containing_goal(GoalId, step_lambda),
                !GoalNum, !ContainingGoalMap, LambdaGoal0, LambdaGoal),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocals, QuantVars, LambdaModes, Detism, LambdaGoal),
            GoalExpr = unify(LHS, RHS,  Mode, Kind, Context)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        fill_conj_id_slots(SlotInfo, GoalId, 0, !GoalNum, !ContainingGoalMap,
            Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        fill_disj_id_slots(SlotInfo, GoalId, 0, !GoalNum, !ContainingGoalMap,
            Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        VarTypes = SlotInfo ^ slot_info_vartypes,
        ModuleInfo = SlotInfo ^ slot_info_module_info,
        map.lookup(VarTypes, Var, Type),
        ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
            MaybeNumFunctors = yes(NumFunctors)
        ;
            MaybeNumFunctors = no
        ),
        fill_switch_id_slots(SlotInfo, GoalId, 0, MaybeNumFunctors, !GoalNum,
            !ContainingGoalMap, Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_neg),
            !GoalNum, !ContainingGoalMap, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % We should consider not filling in the goal path slots inside
        % from_ground_term_construct scopes, since we do not use them
        % for anything.
        SubGoal0 = hlds_goal(_, InnerInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo),
        InnerDetism = goal_info_get_determinism(InnerInfo),
        ( InnerDetism = OuterDetism ->
            MaybeCut = scope_is_no_cut
        ;
            MaybeCut = scope_is_cut
        ),
        fill_goal_id_slots(SlotInfo,
            containing_goal(GoalId, step_scope(MaybeCut)),
            !GoalNum, !ContainingGoalMap, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(A, Cond0, Then0, Else0),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_cond),
            !GoalNum, !ContainingGoalMap, Cond0, Cond),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_then),
            !GoalNum, !ContainingGoalMap, Then0, Then),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_else),
            !GoalNum, !ContainingGoalMap, Else0, Else),
        GoalExpr = if_then_else(A, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            fill_goal_id_slots(SlotInfo,
                containing_goal(GoalId, step_atomic_main),
                !GoalNum, !ContainingGoalMap, MainGoal0, MainGoal),
            fill_orelse_id_slots(SlotInfo, GoalId, 0, !GoalNum,
                !ContainingGoalMap, OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_try),
                !GoalNum, !ContainingGoalMap, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected(this_file, "fill_goal_id_slots: bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred fill_conj_id_slots(slot_info::in, goal_id::in, int::in,
    int::in, int::out, containing_goal_map::in, containing_goal_map::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_id_slots(_, _, _, !GoalNum, !ContainingGoalMap, [], []).
fill_conj_id_slots(SlotInfo, GoalId, N0, !GoalNum, !ContainingGoalMap,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    ContainingGoal = containing_goal(GoalId, step_conj(N1)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalMap,
        Goal0, Goal),
    fill_conj_id_slots(SlotInfo, GoalId, N1, !GoalNum, !ContainingGoalMap,
        Goals0, Goals).

:- pred fill_disj_id_slots(slot_info::in, goal_id::in, int::in,
    int::in, int::out, containing_goal_map::in, containing_goal_map::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_id_slots(_, _, _, !GoalNum, !ContainingGoalMap, [], []).
fill_disj_id_slots(SlotInfo, GoalId, N0, !GoalNum, !ContainingGoalMap,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    ContainingGoal = containing_goal(GoalId, step_disj(N1)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalMap,
        Goal0, Goal),
    fill_disj_id_slots(SlotInfo, GoalId, N1, !GoalNum, !ContainingGoalMap,
        Goals0, Goals).

:- pred fill_switch_id_slots(slot_info::in, goal_id::in,
    int::in, maybe(int)::in, int::in, int::out,
    containing_goal_map::in, containing_goal_map::out,
    list(case)::in, list(case)::out) is det.

fill_switch_id_slots(_, _, _, _, !GoalNum, !ContainingGoalMap, [], []).
fill_switch_id_slots(SlotInfo, GoalId, N0, MaybeNumFunctors, 
        !GoalNum, !ContainingGoalMap, [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    N1 = N0 + 1,
    ContainingGoal =
        containing_goal(GoalId, step_switch(N1, MaybeNumFunctors)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalMap,
        Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    fill_switch_id_slots(SlotInfo, GoalId, N1, MaybeNumFunctors,
        !GoalNum, !ContainingGoalMap, Cases0, Cases).

:- pred fill_orelse_id_slots(slot_info::in, goal_id::in, int::in,
    int::in, int::out, containing_goal_map::in, containing_goal_map::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_orelse_id_slots(_, _, _, !GoalNum, !ContainingGoalMap, [], []).
fill_orelse_id_slots(SlotInfo, GoalId, N0, !GoalNum, !ContainingGoalMap,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    ContainingGoal = containing_goal(GoalId, step_atomic_orelse(N1)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalMap,
        Goal0, Goal),
    fill_orelse_id_slots(SlotInfo, GoalId, N1, !GoalNum, !ContainingGoalMap,
        Goals0, Goals).

%-----------------------------------------------------------------------------%

:- pred fill_goal_path_slots(list(goal_path_step)::in, slot_info::in,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_path_slots(RevSteps0, SlotInfo, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_reverse_goal_path(rgp(RevSteps0), GoalInfo0, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        fill_conj_path_slots(RevSteps0, 0, SlotInfo, Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        fill_disj_path_slots(RevSteps0, 0, SlotInfo, Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        VarTypes = SlotInfo ^ slot_info_vartypes,
        ModuleInfo = SlotInfo ^ slot_info_module_info,
        map.lookup(VarTypes, Var, Type),
        ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
            MaybeNumFunctors = yes(NumFunctors)
        ;
            MaybeNumFunctors = no
        ),
        fill_switch_path_slots(RevSteps0, 0, MaybeNumFunctors, SlotInfo,
            Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        fill_goal_path_slots([step_neg | RevSteps0], SlotInfo,
            SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % We should consider not filling in the goal path slots inside
        % from_ground_term_construct scopes, since we do not use them
        % for anything.
        SubGoal0 = hlds_goal(_, InnerInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo),
        InnerDetism = goal_info_get_determinism(InnerInfo),
        ( InnerDetism = OuterDetism ->
            MaybeCut = scope_is_no_cut
        ;
            MaybeCut = scope_is_cut
        ),
        fill_goal_path_slots([step_scope(MaybeCut) | RevSteps0], SlotInfo,
            SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        fill_goal_path_slots([step_ite_cond | RevSteps0], SlotInfo,
            Cond0, Cond),
        fill_goal_path_slots([step_ite_then | RevSteps0], SlotInfo,
            Then0, Then),
        fill_goal_path_slots([step_ite_else | RevSteps0], SlotInfo,
            Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Kind, Context),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocals, QuantVars, LambdaModes, Detism, LambdaGoal0),
            fill_goal_path_slots(RevSteps0, SlotInfo, LambdaGoal0, LambdaGoal),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocals, QuantVars, LambdaModes, Detism, LambdaGoal)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            RHS = RHS0
        ),
        GoalExpr = unify(LHS, RHS,  Mode, Kind, Context)
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            fill_goal_path_slots([step_atomic_main | RevSteps0], SlotInfo,
                MainGoal0, MainGoal),
            fill_orelse_path_slots(RevSteps0, 0, SlotInfo,
                OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            fill_goal_path_slots(RevSteps0, SlotInfo, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected(this_file, "fill_goal_path_slots: bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred fill_conj_path_slots(list(goal_path_step)::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_path_slots(_, _, _, [], []).
fill_conj_path_slots(RevSteps0, N0, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_path_slots([step_conj(N1) | RevSteps0], SlotInfo, 
        Goal0, Goal),
    fill_conj_path_slots(RevSteps0, N1, SlotInfo, Goals0, Goals).

:- pred fill_disj_path_slots(list(goal_path_step)::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_path_slots(_, _, _, [], []).
fill_disj_path_slots(RevSteps0, N0, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_path_slots([step_disj(N1) | RevSteps0], SlotInfo, 
        Goal0, Goal),
    fill_disj_path_slots(RevSteps0, N1, SlotInfo, Goals0, Goals).

:- pred fill_switch_path_slots(list(goal_path_step)::in,
    int::in, maybe(int)::in, slot_info::in,
    list(case)::in, list(case)::out) is det.

fill_switch_path_slots(_, _, _, _, [], []).
fill_switch_path_slots(RevSteps0, N0, MaybeNumFunctors, SlotInfo,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    N1 = N0 + 1,
    fill_goal_path_slots([step_switch(N1, MaybeNumFunctors) | RevSteps0],
        SlotInfo, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    fill_switch_path_slots(RevSteps0, N1, MaybeNumFunctors, SlotInfo,
        Cases0, Cases).

:- pred fill_orelse_path_slots(list(goal_path_step)::in, int::in,
    slot_info::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_orelse_path_slots(_, _, _, [], []).
fill_orelse_path_slots(RevSteps0, N0, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_path_slots([step_atomic_orelse(N1) | RevSteps0], SlotInfo,
        Goal0, Goal),
    fill_orelse_path_slots(RevSteps0, N1, SlotInfo, Goals0, Goals).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "goal_path.m".

%-----------------------------------------------------------------------------%
:- end_module goal_path.
%-----------------------------------------------------------------------------%
