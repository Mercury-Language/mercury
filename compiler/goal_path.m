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
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.var_table.

%-----------------------------------------------------------------------------%
%
% IMPORTANT: the type constraint_id in hlds_data.m makes use of goal ids
% to identify goals that impose constraints between the typechecking pass
% and the polymorphism pass. For this reason, goal ids should not be
% recalculated anywhere between these two passes. See the XXX comment
% near the declaration of constraint_id.
%

:- pred fill_goal_id_slots_in_proc(module_info::in,
    containing_goal_map::out, proc_info::in, proc_info::out) is det.

:- pred fill_goal_id_slots_in_proc_body(module_info::in, var_table::in,
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

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type slot_info
    --->    slot_info(
                slot_info_module_info       :: module_info,
                slot_info_var_table         :: var_table
            ).

fill_goal_id_slots_in_proc(ModuleInfo, ContainingGoalMap, !ProcInfo) :-
    proc_info_get_var_table(ModuleInfo, !.ProcInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, Goal0),
    fill_goal_id_slots_in_proc_body(ModuleInfo, VarTable, ContainingGoalMap,
        Goal0, Goal),
    proc_info_set_goal(Goal, !ProcInfo).

fill_goal_id_slots_in_proc_body(ModuleInfo, VarTypeSrc, ContainingGoalMap,
        Goal0, Goal) :-
    SlotInfo = slot_info(ModuleInfo, VarTypeSrc),
    fill_goal_id_slots(SlotInfo, whole_body_goal, counter.init(0), _,
        [], ContainingGoalList, Goal0, Goal),
    map.from_rev_sorted_assoc_list(ContainingGoalList, ContainingGoalMap).

fill_goal_id_slots_in_clauses(ModuleInfo, ContainingGoalMap,
        ClausesInfo0, ClausesInfo) :-
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers),
    get_clause_list_for_replacement(ClausesRep0, Clauses0),
    clauses_info_get_var_table(ClausesInfo0, VarTable),
    SlotInfo = slot_info(ModuleInfo, VarTable),
    % If there is exactly one clause, we could theoretically start the counter
    % at zero, assigning goal_id(0) to the whole clause, since it is also
    % the whole procedure body. However, all passes that care about the whole
    % procedure body work on procedures, not clauses, and are there unaffected
    % by what we do here. So we don't bother.
    list.map_foldl3(fill_slots_in_clause(SlotInfo),
        Clauses0, Clauses, 1, _, counter.init(1), _,
        [], ContainingGoalList),
    map.from_rev_sorted_assoc_list(ContainingGoalList, ContainingGoalMap),
    set_clause_list(Clauses, ClausesRep),
    clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
        ClausesInfo0, ClausesInfo).

%-----------------------------------------------------------------------------%

% We accumulate information about containing goals in lists of this type.
% The list is reverse ordered. When we assign a goal its id, which will be
% the highest numbered goal id allocated thus far, we put it at the front of
% the list.
%
% We convert this list to a map only when it is complete. This is faster
% than building up the map itself as we go along, as it avoids the requirement
% to repeatedly rebalance the 234 tree that implements the map. (The final
% conversion builds the 234 tree we want directly, without any need for
% rebalancing.)
:- type containing_goal_list == assoc_list(goal_id, containing_goal).

:- pred fill_slots_in_clause(slot_info::in, clause::in, clause::out,
    int::in, int::out, counter::in, counter::out,
    containing_goal_list::in, containing_goal_list::out) is det.

fill_slots_in_clause(SlotInfo, Clause0, Clause, CurClauseNum, NextClauseNum,
        !GoalNumCounter, !ContainingGoalList) :-
    Goal0 = Clause0 ^ clause_body,
    ContainingGoal = containing_goal(whole_body_goal_id,
        step_disj(CurClauseNum)),
    NextClauseNum = CurClauseNum + 1,
    fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNumCounter,
        !ContainingGoalList, Goal0, Goal),
    Clause = Clause0 ^ clause_body := Goal.

:- pred fill_goal_id_slots(slot_info::in, containing_goal::in,
    counter::in, counter::out,
    containing_goal_list::in, containing_goal_list::out,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_id_slots(SlotInfo, ContainingGoal, !GoalNumCounter,
        !ContainingGoalList, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    counter.allocate(GoalNum, !GoalNumCounter),
    GoalId = goal_id(GoalNum),
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
                NonLocals, ArgVarsModes, Detism, LambdaGoal0),
            fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_lambda),
                !GoalNumCounter, !ContainingGoalList, LambdaGoal0, LambdaGoal),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocals, ArgVarsModes, Detism, LambdaGoal),
            GoalExpr = unify(LHS, RHS,  Mode, Kind, Context)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        fill_conj_id_slots(SlotInfo, GoalId, 0,
            !GoalNumCounter, !ContainingGoalList, Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        fill_disj_id_slots(SlotInfo, GoalId, 0,
            !GoalNumCounter, !ContainingGoalList, Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        VarTable = SlotInfo ^ slot_info_var_table,
        ModuleInfo = SlotInfo ^ slot_info_module_info,
        lookup_var_type(VarTable, Var, Type),
        ( if switch_type_num_functors(ModuleInfo, Type, NumFunctors) then
            MaybeNumFunctors = known_num_functors_in_type(NumFunctors)
        else
            MaybeNumFunctors = unknown_num_functors_in_type
        ),
        fill_switch_id_slots(SlotInfo, GoalId, 0, MaybeNumFunctors,
            !GoalNumCounter, !ContainingGoalList, Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_neg),
            !GoalNumCounter, !ContainingGoalList, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % We should consider not filling in the goal path slots inside
        % from_ground_term_construct scopes, since we do not use them
        % for anything.
        SubGoal0 = hlds_goal(_, InnerInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo),
        InnerDetism = goal_info_get_determinism(InnerInfo),
        ( if InnerDetism = OuterDetism then
            MaybeCut = scope_is_no_cut
        else
            MaybeCut = scope_is_cut
        ),
        fill_goal_id_slots(SlotInfo,
            containing_goal(GoalId, step_scope(MaybeCut)),
            !GoalNumCounter, !ContainingGoalList, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_cond),
            !GoalNumCounter, !ContainingGoalList, Cond0, Cond),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_then),
            !GoalNumCounter, !ContainingGoalList, Then0, Then),
        fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_ite_else),
            !GoalNumCounter, !ContainingGoalList, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            fill_goal_id_slots(SlotInfo,
                containing_goal(GoalId, step_atomic_main),
                !GoalNumCounter, !ContainingGoalList, MainGoal0, MainGoal),
            fill_orelse_id_slots(SlotInfo, GoalId, 0, !GoalNumCounter,
                !ContainingGoalList, OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            fill_goal_id_slots(SlotInfo, containing_goal(GoalId, step_try),
                !GoalNumCounter, !ContainingGoalList, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred fill_conj_id_slots(slot_info::in, goal_id::in, int::in,
    counter::in, counter::out,
    containing_goal_list::in, containing_goal_list::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_id_slots(_, _, _, !GoalNumCounter, !ContainingGoalList, [], []).
fill_conj_id_slots(SlotInfo, GoalId, LastConjunctNum, !GoalNumCounter,
        !ContainingGoalList, [Goal0 | Goals0], [Goal | Goals]) :-
    CurConjunctNum = LastConjunctNum + 1,
    ContainingGoal = containing_goal(GoalId, step_conj(CurConjunctNum)),
    fill_goal_id_slots(SlotInfo, ContainingGoal,
        !GoalNumCounter, !ContainingGoalList, Goal0, Goal),
    fill_conj_id_slots(SlotInfo, GoalId, CurConjunctNum, !GoalNumCounter,
        !ContainingGoalList, Goals0, Goals).

:- pred fill_disj_id_slots(slot_info::in, goal_id::in, int::in,
    counter::in, counter::out,
    containing_goal_list::in, containing_goal_list::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_id_slots(_, _, _, !GoalNumCounter, !ContainingGoalList, [], []).
fill_disj_id_slots(SlotInfo, GoalId, LastDisjunctNum, !GoalNumCounter,
        !ContainingGoalList, [Goal0 | Goals0], [Goal | Goals]) :-
    CurDisjunctNum = LastDisjunctNum + 1,
    ContainingGoal = containing_goal(GoalId, step_disj(CurDisjunctNum)),
    fill_goal_id_slots(SlotInfo, ContainingGoal,
        !GoalNumCounter, !ContainingGoalList, Goal0, Goal),
    fill_disj_id_slots(SlotInfo, GoalId, CurDisjunctNum, !GoalNumCounter,
        !ContainingGoalList, Goals0, Goals).

:- pred fill_switch_id_slots(slot_info::in, goal_id::in,
    int::in, maybe_switch_num_functors::in, counter::in, counter::out,
    containing_goal_list::in, containing_goal_list::out,
    list(case)::in, list(case)::out) is det.

fill_switch_id_slots(_, _, _, _, !GoalNumCounter, !ContainingGoalList, [], []).
fill_switch_id_slots(SlotInfo, GoalId, LastArmNum, MaybeNumFunctors,
        !GoalNumCounter, !ContainingGoalList,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    CurArmNum = LastArmNum + 1,
    ContainingGoal =
        containing_goal(GoalId, step_switch(CurArmNum, MaybeNumFunctors)),
    fill_goal_id_slots(SlotInfo, ContainingGoal,
        !GoalNumCounter, !ContainingGoalList, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    fill_switch_id_slots(SlotInfo, GoalId, CurArmNum, MaybeNumFunctors,
        !GoalNumCounter, !ContainingGoalList, Cases0, Cases).

:- pred fill_orelse_id_slots(slot_info::in, goal_id::in, int::in,
    counter::in, counter::out,
    containing_goal_list::in, containing_goal_list::out,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_orelse_id_slots(_, _, _, !GoalNumCounter, !ContainingGoalList, [], []).
fill_orelse_id_slots(SlotInfo, GoalId, LastOrElseNum, !GoalNumCounter,
        !ContainingGoalList, [Goal0 | Goals0], [Goal | Goals]) :-
    CurOrElseNum = LastOrElseNum + 1,
    ContainingGoal = containing_goal(GoalId, step_atomic_orelse(CurOrElseNum)),
    fill_goal_id_slots(SlotInfo, ContainingGoal,
        !GoalNumCounter, !ContainingGoalList, Goal0, Goal),
    fill_orelse_id_slots(SlotInfo, GoalId, CurOrElseNum, !GoalNumCounter,
        !ContainingGoalList, Goals0, Goals).

%-----------------------------------------------------------------------------%

fill_goal_path_slots_in_proc(ModuleInfo, !ProcInfo) :-
    proc_info_get_var_table(ModuleInfo, !.ProcInfo, VarTable),
    SlotInfo = slot_info(ModuleInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, Goal0),
    fill_goal_path_slots(rgp_nil, SlotInfo, Goal0, Goal),
    proc_info_set_goal(Goal, !ProcInfo).

:- pred fill_goal_path_slots(reverse_goal_path::in, slot_info::in,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_path_slots(RevGoalPath, SlotInfo, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_reverse_goal_path(RevGoalPath, GoalInfo0, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        fill_conj_path_slots(RevGoalPath, 0, SlotInfo, Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        fill_disj_path_slots(RevGoalPath, 0, SlotInfo, Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        VarTable = SlotInfo ^ slot_info_var_table,
        ModuleInfo = SlotInfo ^ slot_info_module_info,
        lookup_var_type(VarTable, Var, Type),
        ( if switch_type_num_functors(ModuleInfo, Type, NumFunctors) then
            MaybeNumFunctors = known_num_functors_in_type(NumFunctors)
        else
            MaybeNumFunctors = unknown_num_functors_in_type
        ),
        fill_switch_path_slots(RevGoalPath, 0, MaybeNumFunctors, SlotInfo,
            Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        SubRevGoalPath = rgp_cons(RevGoalPath, step_neg),
        fill_goal_path_slots(SubRevGoalPath, SlotInfo, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % We should consider not filling in the goal path slots inside
        % from_ground_term_construct scopes, since we do not use them
        % for anything.
        SubGoal0 = hlds_goal(_, InnerInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo),
        InnerDetism = goal_info_get_determinism(InnerInfo),
        ( if InnerDetism = OuterDetism then
            MaybeCut = scope_is_no_cut
        else
            MaybeCut = scope_is_cut
        ),
        SubRevGoalPath = rgp_cons(RevGoalPath, step_scope(MaybeCut)),
        fill_goal_path_slots(SubRevGoalPath, SlotInfo, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        RevPathCond = rgp_cons(RevGoalPath, step_ite_cond),
        RevPathThen = rgp_cons(RevGoalPath, step_ite_then),
        RevPathElse = rgp_cons(RevGoalPath, step_ite_else),
        fill_goal_path_slots(RevPathCond, SlotInfo, Cond0, Cond),
        fill_goal_path_slots(RevPathThen, SlotInfo, Then0, Then),
        fill_goal_path_slots(RevPathElse, SlotInfo, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Kind, Context),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                LambdaNonLocals, ArgVarsModes, Detism, LambdaGoal0),
            % This assigns RevGoalPath to LambdaGoal as well as to Goal.
            % This is ok only because the only user of rev goal paths that are
            % directly stored (as opposed to computed from goal_ids) is invoked
            % only after lambda goals have been expanded out.
            fill_goal_path_slots(RevGoalPath, SlotInfo,
                LambdaGoal0, LambdaGoal),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                LambdaNonLocals, ArgVarsModes, Detism, LambdaGoal)
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
            RevPathMain = rgp_cons(RevGoalPath, step_atomic_main),
            fill_goal_path_slots(RevPathMain, SlotInfo, MainGoal0, MainGoal),
            fill_orelse_path_slots(RevGoalPath, 0, SlotInfo,
                OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            SubRevGoalPath = rgp_cons(RevGoalPath, step_try),
            fill_goal_path_slots(SubRevGoalPath, SlotInfo, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred fill_conj_path_slots(reverse_goal_path::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_path_slots(_, _, _, [], []).
fill_conj_path_slots(ParentRevGoalPath, LastConjunctNum, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    CurConjunctNum = LastConjunctNum + 1,
    RevGoalPath = rgp_cons(ParentRevGoalPath, step_conj(CurConjunctNum)),
    fill_goal_path_slots(RevGoalPath, SlotInfo, Goal0, Goal),
    fill_conj_path_slots(ParentRevGoalPath, CurConjunctNum, SlotInfo,
        Goals0, Goals).

:- pred fill_disj_path_slots(reverse_goal_path::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_path_slots(_, _, _, [], []).
fill_disj_path_slots(ParentRevGoalPath, LastDisjunctNum, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    CurDisjunctNum = LastDisjunctNum + 1,
    RevGoalPath = rgp_cons(ParentRevGoalPath, step_disj(CurDisjunctNum)),
    fill_goal_path_slots(RevGoalPath, SlotInfo, Goal0, Goal),
    fill_disj_path_slots(ParentRevGoalPath, CurDisjunctNum, SlotInfo,
        Goals0, Goals).

:- pred fill_switch_path_slots(reverse_goal_path::in,
    int::in, maybe_switch_num_functors::in, slot_info::in,
    list(case)::in, list(case)::out) is det.

fill_switch_path_slots(_, _, _, _, [], []).
fill_switch_path_slots(ParentRevGoalPath, LastArmNum, MaybeNumFunctors,
        SlotInfo, [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    CurArmNum = LastArmNum + 1,
    RevGoalPath = rgp_cons(ParentRevGoalPath,
        step_switch(CurArmNum, MaybeNumFunctors)),
    fill_goal_path_slots(RevGoalPath, SlotInfo, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    fill_switch_path_slots(ParentRevGoalPath, CurArmNum, MaybeNumFunctors,
        SlotInfo, Cases0, Cases).

:- pred fill_orelse_path_slots(reverse_goal_path::in, int::in,
    slot_info::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_orelse_path_slots(_, _, _, [], []).
fill_orelse_path_slots(ParentRevGoalPath, LastOrElseNum, SlotInfo,
        [Goal0 | Goals0], [Goal | Goals]) :-
    CurOrElseNum = LastOrElseNum + 1,
    RevGoalPath = rgp_cons(ParentRevGoalPath,
        step_atomic_orelse(CurOrElseNum)),
    fill_goal_path_slots(RevGoalPath, SlotInfo, Goal0, Goal),
    fill_orelse_path_slots(ParentRevGoalPath, CurOrElseNum, SlotInfo,
        Goals0, Goals).

%-----------------------------------------------------------------------------%
:- end_module hlds.goal_path.
%-----------------------------------------------------------------------------%
