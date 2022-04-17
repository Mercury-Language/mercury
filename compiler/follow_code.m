%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: follow_code.m.
% Main author: conway.
% Extensive modifications by zs.
%
% The problem attacked by this module is that sometimes the code generator
% doesn't know where it should put the values of live variables at the end
% of a branched control structure. All branches must put each live variable
% into the same lval, so having each branch leave each live variable where it
% just happens to be is not an option. We currently just put all live variables
% into its own rN register or stack slot, but often is not where the variable
% happens to be at the end of any branch, nor is it where the variable is next
% needed.
%
% The idea used by this module to attack this problem is to try to ensure
% that the branched control structure is followed immediately either by a call
% or by the end of the procedure body, because both have clear rules about
% where every live variable must be. If a branched control structure is
% followed by builtin goals such as unifications, we push those goals into
% each branch.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.follow_code.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_proc(pred_proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

move_follow_code_in_proc(_PredProcId, !ProcInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset_vartypes(!.ProcInfo, Varset0, VarTypes0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    move_follow_code_in_goal(Goal0, Goal1, RttiVarMaps0, no, Changed),
    (
        Changed = yes,
        % We need to fix up the goal_info by recalculating the nonlocal
        % vars and the non-atomic instmap deltas.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        implicitly_quantify_clause_body_general(
            ordinary_nonlocals_no_lambda,
            HeadVars, _Warnings, Goal1, Goal2,
            Varset0, Varset, VarTypes0, VarTypes,
            RttiVarMaps0, RttiVarMaps),
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
        proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
        recompute_instmap_delta(do_not_recompute_atomic_instmap_deltas,
            VarTypes, InstVarSet, InstMap0, Goal2, Goal, !ModuleInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset_vartypes(Varset, VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo)
    ;
        Changed = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal(hlds_goal::in, hlds_goal::out,
    rtti_varmaps::in, bool::in, bool::out) is det.

move_follow_code_in_goal(Goal0, Goal, RttiVarMaps, !Changed) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            ConjPurity = goal_info_get_purity(GoalInfo),
            move_follow_code_in_conj(Goals0, ConjPurity, RttiVarMaps, Goals,
                !Changed)
        ;
            ConjType = parallel_conj,
            move_follow_code_in_independent_goals(Goals0, Goals, RttiVarMaps,
                !Changed)
        ),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Goals0),
        move_follow_code_in_independent_goals(Goals0, Goals, RttiVarMaps,
            !Changed),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        move_follow_code_in_goal(SubGoal0, SubGoal, RttiVarMaps, !Changed),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        move_follow_code_in_cases(Cases0, Cases, RttiVarMaps, !Changed),
        GoalExpr = switch(Var, Det, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        move_follow_code_in_goal(Cond0, Cond, RttiVarMaps, !Changed),
        move_follow_code_in_goal(Then0, Then, RttiVarMaps, !Changed),
        move_follow_code_in_goal(Else0, Else, RttiVarMaps, !Changed),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            SubGoal = SubGoal0
        else
            move_follow_code_in_goal(SubGoal0, SubGoal, RttiVarMaps, !Changed)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
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

    % move_follow_code_in_independent_goals is used both for disjunction and
    % parallel conjunction.
    %
:- pred move_follow_code_in_independent_goals(list(hlds_goal)::in,
    list(hlds_goal)::out, rtti_varmaps::in, bool::in, bool::out) is det.

move_follow_code_in_independent_goals([], [], _, !Changed).
move_follow_code_in_independent_goals([Goal0 | Goals0], [Goal | Goals],
        RttiVarMaps, !Changed) :-
    move_follow_code_in_goal(Goal0, Goal, RttiVarMaps, !Changed),
    move_follow_code_in_independent_goals(Goals0, Goals,
        RttiVarMaps, !Changed).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_cases(list(case)::in, list(case)::out,
    rtti_varmaps::in, bool::in, bool::out) is det.

move_follow_code_in_cases([], [], _, !Changed).
move_follow_code_in_cases([Case0 | Cases0], [Case | Cases], RttiVarMaps,
        !Changed) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    move_follow_code_in_goal(Goal0, Goal, RttiVarMaps, !Changed),
    Case = case(MainConsId, OtherConsIds, Goal),
    move_follow_code_in_cases(Cases0, Cases, RttiVarMaps, !Changed).

%-----------------------------------------------------------------------------%

    % Find the first branched structure, and split the conj into those goals
    % before and after it.
    %
:- pred move_follow_code_in_conj(list(hlds_goal)::in, purity::in,
    rtti_varmaps::in, list(hlds_goal)::out, bool::in, bool::out) is det.

move_follow_code_in_conj(Goals0, ConjPurity, RttiVarMaps, Goals, !Changed) :-
    move_follow_code_in_conj_2(Goals0, ConjPurity, RttiVarMaps, [], RevGoals,
        !Changed),
    list.reverse(RevGoals, Goals).

:- pred move_follow_code_in_conj_2(list(hlds_goal)::in, purity::in,
    rtti_varmaps::in, list(hlds_goal)::in, list(hlds_goal)::out,
    bool::in, bool::out) is det.

move_follow_code_in_conj_2([], _ConjPurity, _RttiVarMaps, !RevPrevGoals,
        !Changed).
move_follow_code_in_conj_2([Goal0 | Goals0], ConjPurity, RttiVarMaps,
        !RevPrevGoals, !Changed) :-
    ( if
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        goal_util.goal_is_branched(GoalExpr0),
        % A goal that has the mode_check_clauses marker on it will not
        % have its set of output variables updated by recompute_instmap_delta.
        % If we move new code into it, this lack of recomputation will
        % be a bug if that code binds any variables, which it almost certainly
        % will.
        not goal_info_has_feature(GoalInfo0, feature_mode_check_clauses_goal),

        move_follow_code_select(Goals0, RttiVarMaps, FollowGoals,
            RestGoalsPrime, ConjPurity, WorstPurity),
        FollowGoals = [_ | _],
        % Moving any goals that bind variables into a model_semi (or model_det)
        % disjunction gives that disjunction some outputs, which means that it
        % will become nondet.
        (
            (
                GoalExpr0 = disj(_),
                goal_info_get_code_model(GoalInfo0) \= model_non
            )
        =>
            no_bind_vars(FollowGoals)
        ),
        move_follow_code_move_goals(Goal0, FollowGoals, WorstPurity,
            Goal1Prime)
    then
        !:Changed = yes,
        Goal1 = Goal1Prime,
        RestGoals = RestGoalsPrime
    else
        Goal1 = Goal0,
        RestGoals = Goals0
    ),
    move_follow_code_in_goal(Goal1, Goal, RttiVarMaps, !Changed),
    !:RevPrevGoals = [Goal | !.RevPrevGoals],
    move_follow_code_in_conj_2(RestGoals, ConjPurity, RttiVarMaps,
        !RevPrevGoals, !Changed).

:- pred no_bind_vars(list(hlds_goal)::in) is semidet.

no_bind_vars([]).
no_bind_vars([Goal | Goals]) :-
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap_delta_changed_vars(InstMapDelta, ChangedVars),
    set_of_var.is_empty(ChangedVars),
    no_bind_vars(Goals).

%-----------------------------------------------------------------------------%

    % Split a list of goals into the prefix of builtins and the rest.
    %
:- pred move_follow_code_select(list(hlds_goal)::in, rtti_varmaps::in,
    list(hlds_goal)::out, list(hlds_goal)::out, purity::in, purity::out)
    is det.

move_follow_code_select([], _, [], [], !Purity).
move_follow_code_select([Goal | Goals], RttiVarMaps, FollowGoals, RestGoals,
        !Purity) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if
        move_follow_code_is_builtin(GoalExpr),

        % Don't attempt to move existentially typed deconstructions
        % into branched structures. Doing so would confuse the
        % rtti_varmaps structure, which expects type(class)_infos
        % for a given type variable (constraint) to be retrieved from
        % a single location.
        %
        % XXX A better solution might be to introduce exists_cast goals,
        % which would allow separate type variables for each branch and
        % avoid the above confusion.
        %
        not (
            GoalExpr = unify(_, _, _, Unification, _),
            Unification = deconstruct(_, _, Args, _, _, _),
            list.member(Arg, Args),
            rtti_varmaps_var_info(RttiVarMaps, Arg, RttiVarInfo),
            RttiVarInfo \= non_rtti_var
        )
    then
        GoalPurity = goal_info_get_purity(GoalInfo),
        !:Purity = worst_purity(!.Purity, GoalPurity),
        move_follow_code_select(Goals, RttiVarMaps, FollowGoals0, RestGoals,
            !Purity),
        FollowGoals = [Goal | FollowGoals0]
    else
        FollowGoals = [],
        RestGoals = [Goal | Goals]
    ).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals(hlds_goal::in, list(hlds_goal)::in,
    purity::in, hlds_goal::out) is semidet.

move_follow_code_move_goals(Goal0, FollowGoals, FollowPurity, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = switch(Var, Det, Cases0),
        move_follow_code_move_goals_cases(Cases0, FollowGoals, FollowPurity,
            Cases),
        GoalExpr = switch(Var, Det, Cases)
    ;
        GoalExpr0 = disj(Goals0),
        move_follow_code_move_goals_disj(Goals0, FollowGoals, FollowPurity,
            Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        follow_code_conjoin_goal_and_goal_list(Then0, FollowGoals,
            FollowPurity, Then),
        follow_code_conjoin_goal_and_goal_list(Else0, FollowGoals,
            FollowPurity, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ),
    OldPurity = goal_info_get_purity(GoalInfo0),
    NewPurity = worst_purity(OldPurity, FollowPurity),
    goal_info_set_purity(NewPurity, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_cases(list(case)::in, list(hlds_goal)::in,
    purity::in, list(case)::out) is semidet.

move_follow_code_move_goals_cases([], _FollowGoals, _FollowPurity, []).
move_follow_code_move_goals_cases([Case0 | Cases0], FollowGoals, FollowPurity,
        [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    follow_code_conjoin_goal_and_goal_list(Goal0, FollowGoals, FollowPurity,
        Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    move_follow_code_move_goals_cases(Cases0, FollowGoals, FollowPurity,
        Cases).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_disj(list(hlds_goal)::in,
    list(hlds_goal)::in, purity::in, list(hlds_goal)::out) is semidet.

move_follow_code_move_goals_disj([], _FollowGoals, _FollowPurity, []).
move_follow_code_move_goals_disj([Goal0 | Goals0], FollowGoals, FollowPurity,
        [Goal | Goals]) :-
    follow_code_conjoin_goal_and_goal_list(Goal0, FollowGoals, FollowPurity,
        Goal),
    move_follow_code_move_goals_disj(Goals0, FollowGoals, FollowPurity, Goals).

%-----------------------------------------------------------------------------%

    % Takes a goal and a list of goals, and conjoins them (with a potentially
    % blank goal_info), checking that the determinism of the goal is not
    % changed.
    %
:- pred follow_code_conjoin_goal_and_goal_list(hlds_goal::in,
    list(hlds_goal)::in, purity::in, hlds_goal::out) is semidet.

follow_code_conjoin_goal_and_goal_list(Goal0, FollowGoals, FollowPurity,
        Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Detism0 = goal_info_get_determinism(GoalInfo0),
    determinism_components(Detism0, _CanFail0, MaxSolns0),
    (
        MaxSolns0 = at_most_zero,
        Goal = Goal0
    ;
        ( MaxSolns0 = at_most_one
        ; MaxSolns0 = at_most_many
        ; MaxSolns0 = at_most_many_cc
        ),
        check_follow_code_detism(FollowGoals, Detism0),
        ( if GoalExpr0 = conj(plain_conj, Conjuncts0) then
            GoalExpr = conj(plain_conj, Conjuncts0 ++ FollowGoals)
        else
            GoalExpr = conj(plain_conj, [Goal0 | FollowGoals])
        ),
        OldPurity = goal_info_get_purity(GoalInfo0),
        NewPurity = worst_purity(OldPurity, FollowPurity),
        goal_info_set_purity(NewPurity, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

    % This check is necessary to make sure that follow_code doesn't change
    % the determinism of the goal.
    %
:- pred check_follow_code_detism(list(hlds_goal)::in, determinism::in)
    is semidet.

check_follow_code_detism([], _).
check_follow_code_detism([hlds_goal(_, GoalInfo) | Goals], Detism0) :-
    Detism1 = goal_info_get_determinism(GoalInfo),
    det_conjunction_detism(Detism0, Detism1, Detism0),
    check_follow_code_detism(Goals, Detism0).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_is_builtin(hlds_goal_expr::in) is semidet.

move_follow_code_is_builtin(GoalExpr) :-
    (
        GoalExpr = unify(_, _, _, Unification, _),
        Unification \= complicated_unify(_, _, _)
    ;
        GoalExpr = plain_call(_, _, _, inline_builtin, _, _)
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.follow_code.
%-----------------------------------------------------------------------------%
