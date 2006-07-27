%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: goal_path.m.
% Main author: zs.

% This module looks after goal paths, which associate each goal with its
% position in a procedure definition,

%-----------------------------------------------------------------------------%

:- module check_hlds.goal_path.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module bool.

%-----------------------------------------------------------------------------%

    % IMPORTANT: the type constraint_id in hlds_data.m makes use of
    % goal_paths to identify constraints between the typechecking pass
    % and the polymorphism pass. For this reason, goal paths should not
    % be recalculated anywhere between these two passes. See the XXX
    % comment near the declaration of constraint_id.
    %
:- pred fill_goal_path_slots(module_info::in, proc_info::in, proc_info::out)
    is det.

    % Fill in the goal_paths for goals in the clauses_info of the predicate.
    % Clauses are given goal paths `disj(1)', ...,  `disj(N)'. If the bool
    % argument is true then the goal paths are stored in a form where any
    % prefix consisting of `disj(_)', `neg', `exist(_)' and `ite_else'
    % components is removed. This is used to optimise the constraint-based
    % mode analysis where the instantiatedness of a variable at such a goal
    % path is always equivalent to its instantiatedness at the parent goal
    % path.
    %
:- pred fill_goal_path_slots_in_clauses(module_info::in, bool::in,
    pred_info::in, pred_info::out) is det.

:- pred fill_goal_path_slots_in_goal(hlds_goal::in, vartypes::in,
    module_info::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type slot_info
    --->    slot_info(
                vartypes                    :: vartypes,
                module_info                 :: module_info,
                omit_mode_equiv_prefix      :: bool
            ).

fill_goal_path_slots(ModuleInfo, !Proc) :-
    proc_info_get_goal(!.Proc, Goal0),
    proc_info_get_vartypes(!.Proc, VarTypes),
    fill_goal_path_slots_in_goal(Goal0, VarTypes, ModuleInfo, Goal),
    proc_info_set_goal(Goal, !Proc).

fill_goal_path_slots_in_clauses(ModuleInfo, OmitModeEquivPrefix, !PredInfo) :-
    pred_info_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_clauses_only(ClausesInfo0, Clauses0),
    clauses_info_get_vartypes(ClausesInfo0, VarTypes),
    SlotInfo = slot_info(VarTypes, ModuleInfo, OmitModeEquivPrefix),
    list.map_foldl(fill_slots_in_clause(SlotInfo), Clauses0, Clauses, 1, _),
    clauses_info_set_clauses(Clauses, ClausesInfo0, ClausesInfo),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo).

:- pred fill_slots_in_clause(slot_info::in, clause::in, clause::out,
    int::in, int::out) is det.

fill_slots_in_clause(SlotInfo, Clause0, Clause, ClauseNum, ClauseNum + 1) :-
    Clause0 = clause(ProcIds, Goal0, Lang, Context),
    fill_goal_slots([disj(ClauseNum)], SlotInfo, Goal0, Goal),
    Clause = clause(ProcIds, Goal, Lang, Context).

fill_goal_path_slots_in_goal(Goal0, VarTypes, ModuleInfo, Goal) :-
    SlotInfo = slot_info(VarTypes, ModuleInfo, no),
    fill_goal_slots([], SlotInfo, Goal0, Goal).

:- pred fill_goal_slots(goal_path::in, slot_info::in,
    hlds_goal::in, hlds_goal::out) is det.

fill_goal_slots(Path0, SlotInfo, Expr0 - Info0, Expr - Info) :-
    OmitModeEquivPrefix = SlotInfo ^ omit_mode_equiv_prefix,
    (
        OmitModeEquivPrefix = yes,
        list.takewhile(mode_equiv_step, Path0, _, Path)
    ;
        OmitModeEquivPrefix = no,
        Path = Path0
    ),
    goal_info_set_goal_path(Path, Info0, Info),
    fill_expr_slots(Info, Path0, SlotInfo, Expr0, Expr).

:- pred mode_equiv_step(goal_path_step::in) is semidet.

mode_equiv_step(Step) :-
    ( Step = disj(_)
    ; Step = neg
    ; Step = scope(_)
    ; Step = ite_else
    ).

:- pred fill_expr_slots(hlds_goal_info::in, goal_path::in, slot_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out) is det.

fill_expr_slots(GoalInfo, Path0, SlotInfo, Goal0, Goal) :-
    (
        Goal0 = conj(ConjType, Goals0),
        fill_conj_slots(Path0, 0, SlotInfo, Goals0, Goals),
        Goal = conj(ConjType, Goals)
    ;
        Goal0 = disj(Goals0),
        fill_disj_slots(Path0, 0, SlotInfo, Goals0, Goals),
        Goal = disj(Goals)
    ;
        Goal0 = switch(Var, CanFail, Cases0),
        VarTypes = SlotInfo ^ vartypes,
        ModuleInfo = SlotInfo ^ module_info,
        map.lookup(VarTypes, Var, Type),
        ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
            NumCases = NumFunctors
        ;
            NumCases = -1
        ),
        fill_switch_slots(Path0, 0, NumCases, SlotInfo, Cases0, Cases),
        Goal = switch(Var, CanFail, Cases)
    ;
        Goal0 = negation(SubGoal0),
        fill_goal_slots([neg | Path0], SlotInfo, SubGoal0, SubGoal),
        Goal = negation(SubGoal)
    ;
        Goal0 = scope(Reason, SubGoal0),
        SubGoal0 = _ - InnerInfo,
        goal_info_get_determinism(GoalInfo, OuterDetism),
        goal_info_get_determinism(InnerInfo, InnerDetism),
        ( InnerDetism = OuterDetism ->
            MaybeCut = no_cut
        ;
            MaybeCut = cut
        ),
        fill_goal_slots([scope(MaybeCut) | Path0], SlotInfo,
            SubGoal0, SubGoal),
        Goal = scope(Reason, SubGoal)
    ;
        Goal0 = if_then_else(A, Cond0, Then0, Else0),
        fill_goal_slots([ite_cond | Path0], SlotInfo, Cond0, Cond),
        fill_goal_slots([ite_then | Path0], SlotInfo, Then0, Then),
        fill_goal_slots([ite_else | Path0], SlotInfo, Else0, Else),
        Goal = if_then_else(A, Cond, Then, Else)
    ;
        Goal0 = unify(LHS, RHS0, Mode, Kind, Context),
        ( RHS0 = lambda_goal(A, B, C, D, E, F, G, LambdaGoal0) ->
            fill_goal_slots(Path0, SlotInfo, LambdaGoal0, LambdaGoal),
            RHS = lambda_goal(A, B, C, D, E, F, G, LambdaGoal)
        ;
            RHS = RHS0
        ),
        Goal = unify(LHS, RHS,  Mode, Kind, Context)
    ;
        Goal0 = plain_call(_, _, _, _, _, _),
        Goal = Goal0
    ;
        Goal0 = generic_call(_, _, _, _),
        Goal = Goal0
    ;
        Goal0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = Goal0
    ;
        Goal0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "fill_expr_slots: unexpected shorthand")
    ).

:- pred fill_conj_slots(goal_path::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_conj_slots(_, _, _, [], []).
fill_conj_slots(Path0, N0, SlotInfo, [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_slots([conj(N1) | Path0], SlotInfo, Goal0, Goal),
    fill_conj_slots(Path0, N1, SlotInfo, Goals0, Goals).

:- pred fill_disj_slots(goal_path::in, int::in, slot_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

fill_disj_slots(_, _, _, [], []).
fill_disj_slots(Path0, N0, SlotInfo, [Goal0 | Goals0], [Goal | Goals]) :-
    N1 = N0 + 1,
    fill_goal_slots([disj(N1) | Path0], SlotInfo, Goal0, Goal),
    fill_disj_slots(Path0, N1, SlotInfo, Goals0, Goals).

:- pred fill_switch_slots(goal_path::in, int::in, int::in, slot_info::in,
    list(case)::in, list(case)::out) is det.

fill_switch_slots(_, _, _, _, [], []).
fill_switch_slots(Path0, N0, NumCases, SlotInfo,
        [case(A, Goal0) | Cases0], [case(A, Goal) | Cases]) :-
    N1 = N0 + 1,
    fill_goal_slots([switch(N1, NumCases) | Path0], SlotInfo, Goal0, Goal),
    fill_switch_slots(Path0, N1, NumCases, SlotInfo, Cases0, Cases).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "goal_path.m".

%-----------------------------------------------------------------------------%
:- end_module goal_path.
%-----------------------------------------------------------------------------%
