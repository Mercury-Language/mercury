%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2012 University of Melbourne.
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
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

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
    % which are hard to transition to make use of goal_ids instead.
    % All new code should instead use the predicates above that fill in
    % the goal_id slots.
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
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.

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
        [], ContainingGoalList, Goal0, Goal),
    map.from_rev_sorted_assoc_list(ContainingGoalList, ContainingGoalMap).

fill_goal_id_slots_in_clauses(ModuleInfo, ContainingGoalMap,
        ClausesInfo0, ClausesInfo) :-
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers),
    get_clause_list(ClausesRep0, Clauses0),
    clauses_info_get_vartypes(ClausesInfo0, VarTypes),
    SlotInfo = slot_info(ModuleInfo, VarTypes),
    list.map_foldl3(fill_slots_in_clause(SlotInfo),
        Clauses0, Clauses, 1, _, 1, _, [], ContainingGoalList),
    map.from_rev_sorted_assoc_list(ContainingGoalList, ContainingGoalMap),
    set_clause_list(Clauses, ClausesRep),
    clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
        ClausesInfo0, ClausesInfo).

%-----------------------------------------------------------------------------%

% We accumulate information about containing goals in lists of this type.
% The list is reverse ordered. When we assign a goal its id, which will be
% the highest numbered goal id allocated thus far, we put at the front of
% the list.
%
% We convert this list to a map only when it is complete. This is faster
% than building up the map itself as we go along, as it avoids the requirement
% to repeatedly rebalance the 234 tree that implements the map. (The final
% conversion builds the 234 tree we want directly, without any need for
% rebalancing.)
:- type containing_goal_list == assoc_list(goal_id, containing_goal).

:- pred fill_slots_in_clause(slot_info::in, clause::in, clause::out,
    int::in, int::out, int::in, int::out,
    containing_goal_list::in, containing_goal_list::out) is det.

fill_slots_in_clause(SlotInfo, Clause0, Clause, !GoalNum, !ClauseNum,
        !ContainingGoalList) :-
    Goal0 = Clause0 ^ clause_body,
    ContainingGoal = containing_goal(whole_body_goal_id,
        step_disj(!.ClauseNum)),
    !:ClauseNum = !.ClauseNum + 1,
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalList,
        Goal0, Goal),
    Clause = Clause0 ^ clause_body := Goal.

:- pred fill_goal_id_slots(slot_info::in, containing_goal::in,
    int::in, int::out, containing_goal_list::in, containing_goal_list::out,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalList,
        Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    GoalId = goal_id(!.GoalNum),
    !:GoalNum = !.GoalNum + 1,
    goal_info_set_goal_id(GoalId, GoalInfo0, GoalInfo),
    !:ContainingGoalList = [GoalId - ContainingGoal | !.ContainingGoalList],
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
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
                !GoalNum, !ContainingGoalList, LambdaGoal0, LambdaGoal),
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
        fill_conj_id_slots(SlotInfo, GoalId, 0, !GoalNum, !ContainingGoalList,
            Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        fill_disj_id_slots(SlotInfo, GoalId, 0, !GoalNum, !ContainingGoalList,
            Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        VarTypes = SlotInfo ^ slot_info_vartypes,
        ModuleInfo = SlotInfo ^ slot_info_module_info,
        lookup_var_type(VarTypes, Var, Type),
        ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
            MaybeNumFunctors = known_num_functors_in_type(NumFunctors)
        ;
            MaybeNumFunctors = unknown_num_functors_in_type
        ),
        fill_switch_id_slots(SlotInfo, GoalId, 0, MaybeNumFunctors, !GoalNum,
            !ContainingGoalList, Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_neg),
            !GoalNum, !ContainingGoalList, SubGoal0, SubGoal),
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
            !GoalNum, !ContainingGoalList, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(A, Cond0, Then0, Else0),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_cond),
            !GoalNum, !ContainingGoalList, Cond0, Cond),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_then),
            !GoalNum, !ContainingGoalList, Then0, Then),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_else),
            !GoalNum, !ContainingGoalList, Else0, Else),
        GoalExpr = if_then_else(A, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            fill_goal_id_slots(SlotInfo,
                containing_goal(GoalId, step_atomic_main),
                !GoalNum, !ContainingGoalList, MainGoal0, MainGoal),
            fill_orelse_id_slots(SlotInfo, GoalId, 0, !GoalNum,
                !ContainingGoalList, OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_try),
                !GoalNum, !ContainingGoalList, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred fill_conj_id_slots(slot_info::in, goal_id::in, int::in,
    int::in, int::out, containing_goal_list::in, containing_goal_list::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_id_slots(_, _, _, !GoalNum, !ContainingGoalList, [], []).
fill_conj_id_slots(SlotInfo, GoalId, N0, !GoalNum, !ContainingGoalList,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    ContainingGoal = containing_goal(GoalId, step_conj(N1)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalList,
        Goal0, Goal),
    fill_conj_id_slots(SlotInfo, GoalId, N1, !GoalNum, !ContainingGoalList,
        Goals0, Goals).

:- pred fill_disj_id_slots(slot_info::in, goal_id::in, int::in,
    int::in, int::out, containing_goal_list::in, containing_goal_list::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_id_slots(_, _, _, !GoalNum, !ContainingGoalList, [], []).
fill_disj_id_slots(SlotInfo, GoalId, N0, !GoalNum, !ContainingGoalList,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    ContainingGoal = containing_goal(GoalId, step_disj(N1)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalList,
        Goal0, Goal),
    fill_disj_id_slots(SlotInfo, GoalId, N1, !GoalNum, !ContainingGoalList,
        Goals0, Goals).

:- pred fill_switch_id_slots(slot_info::in, goal_id::in,
    int::in, maybe_switch_num_functors::in, int::in, int::out,
    containing_goal_list::in, containing_goal_list::out,
    list(case)::in, list(case)::out) is det.

fill_switch_id_slots(_, _, _, _, !GoalNum, !ContainingGoalList, [], []).
fill_switch_id_slots(SlotInfo, GoalId, N0, MaybeNumFunctors,
        !GoalNum, !ContainingGoalList, [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    N1 = N0 + 1,
    ContainingGoal =
        containing_goal(GoalId, step_switch(N1, MaybeNumFunctors)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalList,
        Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    fill_switch_id_slots(SlotInfo, GoalId, N1, MaybeNumFunctors,
        !GoalNum, !ContainingGoalList, Cases0, Cases).

:- pred fill_orelse_id_slots(slot_info::in, goal_id::in, int::in,
    int::in, int::out, containing_goal_list::in, containing_goal_list::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_orelse_id_slots(_, _, _, !GoalNum, !ContainingGoalList, [], []).
fill_orelse_id_slots(SlotInfo, GoalId, N0, !GoalNum, !ContainingGoalList,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    ContainingGoal = containing_goal(GoalId, step_atomic_orelse(N1)),
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNum, !ContainingGoalList,
        Goal0, Goal),
    fill_orelse_id_slots(SlotInfo, GoalId, N1, !GoalNum, !ContainingGoalList,
        Goals0, Goals).

%-----------------------------------------------------------------------------%

fill_goal_path_slots_in_proc(ModuleInfo, !Proc) :-
    proc_info_get_goal(!.Proc, Goal0),
    proc_info_get_vartypes(!.Proc, VarTypes),
    SlotInfo = slot_info(ModuleInfo, VarTypes),
    fill_goal_path_slots(rgp_nil, SlotInfo, Goal0, Goal),
    proc_info_set_goal(Goal, !Proc).

:- pred fill_goal_path_slots(reverse_goal_path::in, slot_info::in,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_path_slots(RevPath0, SlotInfo, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_reverse_goal_path(RevPath0, GoalInfo0, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        fill_conj_path_slots(RevPath0, 0, SlotInfo, Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        fill_disj_path_slots(RevPath0, 0, SlotInfo, Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        VarTypes = SlotInfo ^ slot_info_vartypes,
        ModuleInfo = SlotInfo ^ slot_info_module_info,
        lookup_var_type(VarTypes, Var, Type),
        ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
            MaybeNumFunctors = known_num_functors_in_type(NumFunctors)
        ;
            MaybeNumFunctors = unknown_num_functors_in_type
        ),
        fill_switch_path_slots(RevPath0, 0, MaybeNumFunctors, SlotInfo,
            Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        RevPath = rgp_cons(RevPath0, step_neg),
        fill_goal_path_slots(RevPath, SlotInfo, SubGoal0, SubGoal),
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
        RevPath = rgp_cons(RevPath0, step_scope(MaybeCut)),
        fill_goal_path_slots(RevPath, SlotInfo, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        RevPathCond = rgp_cons(RevPath0, step_ite_cond),
        RevPathThen = rgp_cons(RevPath0, step_ite_then),
        RevPathElse = rgp_cons(RevPath0, step_ite_else),
        fill_goal_path_slots(RevPathCond, SlotInfo, Cond0, Cond),
        fill_goal_path_slots(RevPathThen, SlotInfo, Then0, Then),
        fill_goal_path_slots(RevPathElse, SlotInfo, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Kind, Context),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocals, QuantVars, LambdaModes, Detism, LambdaGoal0),
            fill_goal_path_slots(RevPath0, SlotInfo, LambdaGoal0, LambdaGoal),
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
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            RevPathMain = rgp_cons(RevPath0, step_atomic_main),
            fill_goal_path_slots(RevPathMain, SlotInfo, MainGoal0, MainGoal),
            fill_orelse_path_slots(RevPath0, 0, SlotInfo,
                OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            RevPath = rgp_cons(RevPath0, step_try),
            fill_goal_path_slots(RevPath, SlotInfo, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred fill_conj_path_slots(reverse_goal_path::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_path_slots(_, _, _, [], []).
fill_conj_path_slots(RevPath0, N0, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_path_slots(rgp_cons(RevPath0, step_conj(N1)), SlotInfo,
        Goal0, Goal),
    fill_conj_path_slots(RevPath0, N1, SlotInfo, Goals0, Goals).

:- pred fill_disj_path_slots(reverse_goal_path::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_path_slots(_, _, _, [], []).
fill_disj_path_slots(RevPath0, N0, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_path_slots(rgp_cons(RevPath0, step_disj(N1)), SlotInfo,
        Goal0, Goal),
    fill_disj_path_slots(RevPath0, N1, SlotInfo, Goals0, Goals).

:- pred fill_switch_path_slots(reverse_goal_path::in,
    int::in, maybe_switch_num_functors::in, slot_info::in,
    list(case)::in, list(case)::out) is det.

fill_switch_path_slots(_, _, _, _, [], []).
fill_switch_path_slots(RevPath0, N0, MaybeNumFunctors, SlotInfo,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    N1 = N0 + 1,
    fill_goal_path_slots(rgp_cons(RevPath0, step_switch(N1, MaybeNumFunctors)),
        SlotInfo, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    fill_switch_path_slots(RevPath0, N1, MaybeNumFunctors, SlotInfo,
        Cases0, Cases).

:- pred fill_orelse_path_slots(reverse_goal_path::in, int::in,
    slot_info::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_orelse_path_slots(_, _, _, [], []).
fill_orelse_path_slots(RevPath0, N0, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_path_slots(rgp_cons(RevPath0, step_atomic_orelse(N1)), SlotInfo,
        Goal0, Goal),
    fill_orelse_path_slots(RevPath0, N1, SlotInfo, Goals0, Goals).

%-----------------------------------------------------------------------------%
:- end_module hlds.goal_path.
%-----------------------------------------------------------------------------%
