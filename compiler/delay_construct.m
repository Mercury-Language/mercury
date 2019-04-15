%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: delay_construct.m.
% Author: zs.
%
% This module transforms sequences of goals in procedure bodies.  It looks for
% a unification that constructs a ground term followed by primitive goals, at
% least one of which can fail, and none of which take the variable
% representing the cell as their input. Such code sequences cause the cell to
% be constructed even if the following goal would fail, which is wasteful.
% This module therefore reorders the sequence, moving the construction
% unification past all the semidet primitives it can.
%
% The reason we don't move the construction past calls or composite goals is
% that this may require storing the input arguments of the construction on the
% stack, which may cause a slowdown bigger than the speedup available from not
% having to construct the cell on some execution paths.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.delay_construct.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred delay_construct_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

delay_construct_proc(ModuleInfo, proc(PredId, ProcId), !ProcInfo) :-
    trace [io(!IO)] (
        write_proc_progress_message("% Delaying construction unifications in ",
            PredId, ProcId, ModuleInfo, !IO)
    ),
    module_info_get_globals(ModuleInfo, Globals),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    body_should_use_typeinfo_liveness(PredInfo, Globals, BodyTypeinfoLiveness),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps),
    proc_info_get_initial_instmap(!.ProcInfo, ModuleInfo, InstMap0),
    DelayInfo = delay_construct_info(ModuleInfo, BodyTypeinfoLiveness,
        VarTypes, RttiVarMaps),
    proc_info_get_goal(!.ProcInfo, Goal0),
    delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal),
    proc_info_set_goal(Goal, !ProcInfo).

:- type delay_construct_info
    --->    delay_construct_info(
                dci_module_info             :: module_info,
                dci_body_typeinfo_liveness  :: bool,
                dci_vartypes                :: vartypes,
                dci_rtti_varmaps            :: rtti_varmaps
            ).

%-----------------------------------------------------------------------------%

:- pred delay_construct_in_goal(hlds_goal::in, instmap::in,
    delay_construct_info::in, hlds_goal::out) is det.

delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            Detism = goal_info_get_determinism(GoalInfo0),
            determinism_components(Detism, CanFail, MaxSoln),
            ( if
                % If the conjunction cannot fail, then its conjuncts cannot
                % fail either, so we have no hope of pushing a construction
                % past a failing goal.
                %
                % If the conjuntion contains goals that can succeed more than
                % once, which is possible if MaxSoln is at_most_many or
                % at_most_many_cc, then moving a construction to the right
                % may increase the number of times the construction is
                % executed. We are therefore careful to make sure
                % delay_construct_in_conj doesn't move constructions
                % across goals that succeed more than once. If the conjunction
                % cannot succeed, i.e. MaxSoln is at_most_zero, there is no
                % point in trying to speed it up.

                CanFail = can_fail,
                MaxSoln \= at_most_zero
            then
                delay_construct_in_conj(Goals0, InstMap0, DelayInfo, set.init,
                    [], Goals1)
            else
                Goals1 = Goals0
            )
        ;
            ConjType = parallel_conj,
            Goals1 = Goals0
        ),
        delay_construct_in_goals(Goals1, InstMap0, DelayInfo, Goals),
        Goal = hlds_goal(conj(ConjType, Goals), GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        delay_construct_in_goals(Goals0, InstMap0, DelayInfo, Goals),
        Goal = hlds_goal(disj(Goals), GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        delay_construct_in_goal(NegGoal0, InstMap0, DelayInfo, NegGoal),
        Goal = hlds_goal(negation(NegGoal), GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        delay_construct_in_cases(Cases0, InstMap0, DelayInfo, Cases),
        Goal = hlds_goal(switch(Var, CanFail, Cases), GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        Cond0 = hlds_goal(_, CondInfo0),
        CondInstMapDelta = goal_info_get_instmap_delta(CondInfo0),
        instmap.apply_instmap_delta(InstMap0, CondInstMapDelta, InstMapThen),
        delay_construct_in_goal(Cond0, InstMap0, DelayInfo, Cond),
        delay_construct_in_goal(Then0, InstMapThen, DelayInfo, Then),
        delay_construct_in_goal(Else0, InstMap0, DelayInfo, Else),
        Goal = hlds_goal(if_then_else(Vars, Cond, Then, Else), GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Goal = Goal0
        else
            delay_construct_in_goal(SubGoal0, InstMap0, DelayInfo, SubGoal),
            Goal = hlds_goal(scope(Reason, SubGoal), GoalInfo0)
        )
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

% We maintain a list of delayed construction unifications that construct
% ground terms, and the set of variables they define.
%
% When we find other construction unifications, we add them to the list.  It
% does not matter if they depend on other delayed construction unifications;
% when we put them back into the conjunction, we do so in the original order.
%
% There are several reasons why we may not be able to delay a construction
% unification past a conjunct. The conjunct may not be a primitive goal, or it
% may be impure; in either case, we must insert all the delayed construction
% unifications before it. The conjunct may also require the value of a
% variable defined by a construction unification. In such cases, we could drop
% before that goal only the construction unifications that define the
% variables needed by the conjunct, either directly or indirectly through the
% values required by some of those construction unifications. However,
% separating out this set of delayed constructions from the others would
% require somewhat complex code, and it is not clear that there would be any
% significant benefit. We therefore insert *all* the delayed constructions
% before a goal if the goal requires *any* of the variables bound by the
% constructions.
%
% The instmap we pass around is the one that we construct from the original
% conjunction order. At each point, it reflects the bindings made by the
% conjuncts so far *plus* the bindings made by the delayed goals.

:- pred delay_construct_in_conj(list(hlds_goal)::in, instmap::in,
    delay_construct_info::in, set(prog_var)::in, list(hlds_goal)::in,
    list(hlds_goal)::out) is det.

delay_construct_in_conj([], _, _, _, RevDelayedGoals, DelayedGoals) :-
    list.reverse(RevDelayedGoals, DelayedGoals).
delay_construct_in_conj([Goal0 | Goals0], InstMap0, DelayInfo,
        ConstructedVars0, RevDelayedGoals0, Goals) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1),
    ( if
        GoalExpr0 = unify(_, _, _, Unif, _),
        Unif = construct(Var, _, Args, _, _, _, _),
        Args = [_ | _], % We are constructing a cell, not a constant
        instmap_lookup_var(InstMap0, Var, Inst0),
        inst_is_free(DelayInfo ^ dci_module_info, Inst0),
        instmap_lookup_var(InstMap1, Var, Inst1),
        inst_is_ground(DelayInfo ^ dci_module_info, Inst1)
    then
        set.insert(Var, ConstructedVars0, ConstructedVars1),
        RevDelayedGoals1 = [Goal0 | RevDelayedGoals0],
        delay_construct_in_conj(Goals0, InstMap1, DelayInfo,
            ConstructedVars1, RevDelayedGoals1, Goals)
    else if
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        delay_construct_skippable(GoalExpr0, GoalInfo0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        maybe_complete_with_typeinfo_vars(NonLocals,
            DelayInfo ^ dci_body_typeinfo_liveness,
            DelayInfo ^ dci_vartypes,
            DelayInfo ^ dci_rtti_varmaps, CompletedNonLocals),
        set_of_var.intersect(CompletedNonLocals,
            set_to_bitset(ConstructedVars0), Intersection),
        set_of_var.is_empty(Intersection),
        goal_info_get_purity(GoalInfo0) = purity_pure
    then
        delay_construct_in_conj(Goals0, InstMap1, DelayInfo,
            ConstructedVars0, RevDelayedGoals0, Goals1),
        Goals = [Goal0 | Goals1]
    else
        list.reverse(RevDelayedGoals0, DelayedGoals),
        delay_construct_in_conj(Goals0, InstMap1, DelayInfo,
            set.init, [], Goals1),
        list.append(DelayedGoals, [Goal0 | Goals1], Goals)
    ).

:- pred delay_construct_skippable(hlds_goal_expr::in, hlds_goal_info::in)
    is semidet.

delay_construct_skippable(GoalExpr, GoalInfo) :-
    (
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = plain_call(_, _, _, inline_builtin, _, _)
    ),
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, _CanFail, MaxSoln),
    MaxSoln \= at_most_many.

%-----------------------------------------------------------------------------%

:- pred delay_construct_in_goals(list(hlds_goal)::in, instmap::in,
    delay_construct_info::in, list(hlds_goal)::out) is det.

delay_construct_in_goals([], _, _, []).
delay_construct_in_goals([Goal0 | Goals0], InstMap0, DelayInfo,
        [Goal | Goals]) :-
    delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal),
    delay_construct_in_goals(Goals0, InstMap0, DelayInfo, Goals).

:- pred delay_construct_in_cases(list(case)::in, instmap::in,
    delay_construct_info::in, list(case)::out) is det.

delay_construct_in_cases([], _, _, []).
delay_construct_in_cases([Case0 | Cases0], InstMap0, DelayInfo,
        [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    delay_construct_in_cases(Cases0, InstMap0, DelayInfo, Cases).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.delay_construct.
%-----------------------------------------------------------------------------%
