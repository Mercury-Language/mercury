%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015-2019, 2021-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_transform.m.
%
% This module provides ways to transform goals.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_transform.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Flatten the conjuncts of a conjunction.
    % Flattens only one level.
    % XXX Why not all levels recursively, as flatten_disj does?
    %
:- pred flatten_conj(list(hlds_goal)::in, list(hlds_goal)::out) is det.

    % Flatten the disjuncts of a disjunction.
    % Flattens all levels recursively.
    %
:- pred flatten_disj(list(hlds_goal)::in, list(hlds_goal)::out) is det.

%---------------------------------------------------------------------------%

:- type attach_in_from_ground_term
    --->    attach_in_from_ground_term
    ;       do_not_attach_in_from_ground_term.

    % Attach the given goal features to the given goal and all its subgoals,
    % except possibly in from_ground_term scopes.
    %
:- pred attach_features_to_all_goals(list(goal_feature),
    attach_in_from_ground_term, hlds_goal, hlds_goal).
:- mode attach_features_to_all_goals(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_all_goals(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

%---------------------------------------------------------------------------%

:- func maybe_strip_equality_pretest(hlds_goal) = hlds_goal.

%---------------------------------------------------------------------------%

:- type maybe_transformed_goal
    --->    ok(hlds_goal)
    ;       error(string)
    ;       goal_not_found.

    % Locate the goal described by the goal path and use its first argument to
    % transform that goal before rebuilding the goal tree and returning.
    % If the goal is not found, the result is no. If the result of the higher
    % order argument is no, then the result is no.
    %
:- pred maybe_transform_goal_at_goal_path(
    pred(hlds_goal, maybe_error(hlds_goal))::in(pred(in, out) is det),
    forward_goal_path::in, hlds_goal::in, maybe_transformed_goal::out) is det.

    % As above, except that we also compute the instmap during the traversal so
    % that the transformation expressed by the higher order value can use the
    % instmap at that point within the goal tree.
    %
:- pred maybe_transform_goal_at_goal_path_with_instmap(
    pred(instmap, hlds_goal, maybe_error(hlds_goal))::
        in(pred(in, in, out) is det),
    forward_goal_path::in, instmap::in, hlds_goal::in,
    maybe_transformed_goal::out) is det.

    % Transform the given goal and all its children according to the higher
    % order argument. Children are transformed before their parents, therefore
    % the higher order argument will receive a goal with children that have
    % already been transformed.
    %
:- pred transform_all_goals(
    pred(hlds_goal, hlds_goal)::in(pred(in, out) is det),
    hlds_goal::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%
%
% NOTE: Neither switch_to_disjunction nor case_to_disjunct is currently used,
% but they could be useful in the future.
%

    % Convert a switch back into a disjunction. This used to be needed
    % by the Aditi backend for the magic set transformation.
    % This aborts if any of the constructors are existentially typed.
    %
:- pred switch_to_disjunction(prog_var::in, list(case)::in,
    instmap::in, list(hlds_goal)::out, var_table::in, var_table::out,
    module_info::in, module_info::out) is det.

    % Convert a case into a conjunction by adding a tag test
    % (deconstruction unification) to the case goal.
    % This aborts if the constructor is existentially typed.
    %
:- pred case_to_disjunct(prog_var::in, hlds_goal::in, instmap::in,
    cons_id::in, hlds_goal::out, var_table::in, var_table::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.inst_lookup.
:- import_module hlds.type_util.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

flatten_conj([], []).
flatten_conj([Goal | Goals0], FlatConj) :-
    flatten_conj(Goals0, FlatConjTail),
    ( if Goal = hlds_goal(conj(plain_conj, SubGoals), _) then
        FlatConj = SubGoals ++ FlatConjTail
    else
        FlatConj = [Goal | FlatConjTail]
    ).

flatten_disj(Disjuncts, FlatDisjuncts) :-
    list.foldr(flatten_disj_acc, Disjuncts, [], FlatDisjuncts).

:- pred flatten_disj_acc(hlds_goal::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

flatten_disj_acc(Disjunct, !FlatDisjuncts) :-
    ( if Disjunct = hlds_goal(disj(SubDisjs), _GoalInfo) then
        list.foldr(flatten_disj_acc, SubDisjs, !FlatDisjuncts)
    else
        !:FlatDisjuncts = [Disjunct | !.FlatDisjuncts]
    ).

%---------------------------------------------------------------------------%

attach_features_to_all_goals(Features, InFromGroundTerm, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    attach_features_to_goal_expr(Features, InFromGroundTerm,
        GoalExpr0, GoalExpr),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred attach_features_to_goals(list(goal_feature),
    attach_in_from_ground_term, list(hlds_goal), list(hlds_goal)).
:- mode attach_features_to_goals(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_goals(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

attach_features_to_goals(_Features, _InFromGroundTerm, [], []).
attach_features_to_goals(Features, InFromGroundTerm,
        [Goal0 | Goals0], [Goal | Goals]) :-
    attach_features_to_all_goals(Features, InFromGroundTerm, Goal0, Goal),
    attach_features_to_goals(Features, InFromGroundTerm, Goals0, Goals).

:- pred attach_features_to_cases(list(goal_feature),
    attach_in_from_ground_term, list(case), list(case)).
:- mode attach_features_to_cases(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_cases(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

attach_features_to_cases(_Features, _InFromGroundTerm, [], []).
attach_features_to_cases(Features, InFromGroundTerm,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    attach_features_to_all_goals(Features, InFromGroundTerm, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    attach_features_to_cases(Features, InFromGroundTerm, Cases0, Cases).

:- pred attach_features_to_goal_expr(list(goal_feature),
    attach_in_from_ground_term, hlds_goal_expr, hlds_goal_expr).
:- mode attach_features_to_goal_expr(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_goal_expr(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

attach_features_to_goal_expr(Features, InFromGroundTerm,
        GoalExpr0, GoalExpr) :-
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        attach_features_to_goals(Features, InFromGroundTerm, Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        attach_features_to_goals(Features, InFromGroundTerm, Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        attach_features_to_cases(Features, InFromGroundTerm, Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        attach_features_to_all_goals(Features, InFromGroundTerm, Cond0, Cond),
        attach_features_to_all_goals(Features, InFromGroundTerm, Then0, Then),
        attach_features_to_all_goals(Features, InFromGroundTerm, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        attach_features_to_all_goals(Features, InFromGroundTerm,
            SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, _) then
            (
                InFromGroundTerm = do_not_attach_in_from_ground_term,
                SubGoal = SubGoal0
            ;
                InFromGroundTerm = attach_in_from_ground_term,
                attach_features_to_all_goals(Features, InFromGroundTerm,
                    SubGoal0, SubGoal)
            )
        else
            attach_features_to_all_goals(Features, InFromGroundTerm,
                SubGoal0, SubGoal)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                MainGoal0, MainGoal),
            attach_features_to_goals(Features, InFromGroundTerm,
                OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(GoalA0, GoalB0),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                GoalA0, GoalA),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                GoalB0, GoalB),
            ShortHand = bi_implication(GoalA, GoalB)
        ),
        GoalExpr = shorthand(ShortHand)
    ).

%---------------------------------------------------------------------------%

maybe_strip_equality_pretest(Goal0) = Goal :-
    % The if_then_else constructed by unify_proc is sometimes wrapped up
    % in conjunctions.
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        Goals = list.map(maybe_strip_equality_pretest, Goals0),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(SubGoals0),
        SubGoals = list.map(maybe_strip_equality_pretest, SubGoals0),
        GoalExpr = disj(SubGoals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        Cases = list.map(maybe_strip_equality_pretest_case, Cases0),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        ( if goal_info_has_feature(GoalInfo0, feature_pretest_equality) then
            Goal = Else0
        else
            Cond = maybe_strip_equality_pretest(Cond0),
            Then = maybe_strip_equality_pretest(Then0),
            Else = maybe_strip_equality_pretest(Else0),
            GoalExpr = if_then_else(Vars, Cond, Then, Else),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = negation(SubGoal0),
        SubGoal = maybe_strip_equality_pretest(SubGoal0),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
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
            SubGoal = maybe_strip_equality_pretest(SubGoal0),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            MainGoal = maybe_strip_equality_pretest(MainGoal0),
            OrElseGoals = list.map(maybe_strip_equality_pretest, OrElseGoals0),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            SubGoal = maybe_strip_equality_pretest(SubGoal0),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- func maybe_strip_equality_pretest_case(case) = case.

maybe_strip_equality_pretest_case(Case0) = Case :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    Goal = maybe_strip_equality_pretest(Goal0),
    Case = case(MainConsId, OtherConsIds, Goal).

%---------------------------------------------------------------------------%

maybe_transform_goal_at_goal_path(TransformPred, TargetGoalPath,
        Goal0, MaybeGoal) :-
    (
        TargetGoalPath = fgp_nil,
        TransformPred(Goal0, MaybeGoal0),
        maybe_error_to_maybe_transformed_goal(MaybeGoal0, MaybeGoal)
    ;
        TargetGoalPath = fgp_cons(FirstStep, LaterPath),
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        (
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            % This search should never reach an atomic goal.
            MaybeGoal = goal_not_found
        ;
            GoalExpr0 = conj(ConjType, Conjs0),
            ( if
                FirstStep = step_conj(ConjNum),
                list.index1(Conjs0, ConjNum, Conj0)
            then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    Conj0, MaybeConj),
                (
                    MaybeConj = ok(Conj),
                    list.det_replace_nth(Conjs0, ConjNum, Conj, Conjs),
                    GoalExpr = conj(ConjType, Conjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeConj = error(_)
                    ; MaybeConj = goal_not_found
                    ),
                    MaybeGoal = MaybeConj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = disj(Disjs0),
            ( if
                FirstStep = step_disj(DisjNum),
                list.index1(Disjs0, DisjNum, Disj0)
            then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    Disj0, MaybeDisj),
                (
                    MaybeDisj = ok(Disj),
                    list.det_replace_nth(Disjs0, DisjNum, Disj, Disjs),
                    GoalExpr = disj(Disjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeDisj = error(_)
                    ; MaybeDisj = goal_not_found
                    ),
                    MaybeGoal = MaybeDisj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            ( if
                FirstStep = step_switch(CaseNum, _MaybeNumConstructors),
                list.index1(Cases0, CaseNum, Case0)
            then
                CaseGoal0 = Case0 ^ case_goal,
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    CaseGoal0, MaybeCaseGoal),
                (
                    MaybeCaseGoal = ok(CaseGoal),
                    Case = Case0 ^ case_goal := CaseGoal,
                    list.det_replace_nth(Cases0, CaseNum, Case, Cases),
                    GoalExpr = switch(Var, CanFail, Cases),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCaseGoal = error(_)
                    ; MaybeCaseGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeCaseGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = negation(SubGoal0),
            ( if FirstStep = step_neg then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = negation(SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            ( if FirstStep = step_scope(_MaybeCut) then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = scope(Reason, SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = if_then_else(ExistVars, Cond0, Then0, Else0),
            ( if FirstStep = step_ite_cond then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    Cond0, MaybeCond),
                (
                    MaybeCond = ok(Cond),
                    GoalExpr = if_then_else(ExistVars, Cond, Then0, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCond = error(_)
                    ; MaybeCond = goal_not_found
                    ),
                    MaybeGoal = MaybeCond
                )
            else if FirstStep = step_ite_then then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    Then0, MaybeThen),
                (
                    MaybeThen = ok(Then),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeThen = error(_)
                    ; MaybeThen = goal_not_found
                    ),
                    MaybeGoal = MaybeThen
                )
            else if FirstStep = step_ite_else then
                maybe_transform_goal_at_goal_path(TransformPred, LaterPath,
                    Else0, MaybeElse),
                (
                    MaybeElse = ok(Else),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then0, Else),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeElse = error(_)
                    ; MaybeElse = goal_not_found
                    ),
                    MaybeGoal = MaybeElse
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = shorthand(_),
            unexpected($pred, "shorthand")
        )
    ).

maybe_transform_goal_at_goal_path_with_instmap(TransformPred, TargetGoalPath,
        Instmap0, Goal0, MaybeGoal) :-
    (
        TargetGoalPath = fgp_nil,
        TransformPred(Instmap0, Goal0, MaybeGoal0),
        maybe_error_to_maybe_transformed_goal(MaybeGoal0, MaybeGoal)
    ;
        TargetGoalPath = fgp_cons(FirstStep, LaterPath),
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        (
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            % This search should never reach an atomic goal.
            MaybeGoal = goal_not_found
        ;
            GoalExpr0 = conj(ConjType, Conjs0),
            ( if
                FirstStep = step_conj(ConjNum),
                list.index1(Conjs0, ConjNum, Conj0)
            then
                list.take_upto(ConjNum - 1, Conjs0, HeadConjs),
                HeadInstdeltas = map(
                    (func(G) = goal_info_get_instmap_delta(G ^ hg_info)),
                    HeadConjs),
                list.foldl(apply_instmap_delta, HeadInstdeltas,
                    Instmap0, Instmap),
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap, Conj0, MaybeConj),
                (
                    MaybeConj = ok(Conj),
                    list.det_replace_nth(Conjs0, ConjNum, Conj, Conjs),
                    GoalExpr = conj(ConjType, Conjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeConj = error(_)
                    ; MaybeConj = goal_not_found
                    ),
                    MaybeGoal = MaybeConj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = disj(Disjs0),
            ( if
                FirstStep = step_disj(DisjNum),
                list.index1(Disjs0, DisjNum, Disj0)
            then
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap0, Disj0, MaybeDisj),
                (
                    MaybeDisj = ok(Disj),
                    list.det_replace_nth(Disjs0, DisjNum, Disj, Disjs),
                    GoalExpr = disj(Disjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeDisj = error(_)
                    ; MaybeDisj = goal_not_found
                    ),
                    MaybeGoal = MaybeDisj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            ( if
                FirstStep = step_switch(CaseNum, _MaybeNumConstructors),
                list.index1(Cases0, CaseNum, Case0)
            then
                CaseGoal0 = Case0 ^ case_goal,
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap0, CaseGoal0, MaybeCaseGoal),
                (
                    MaybeCaseGoal = ok(CaseGoal),
                    Case = Case0 ^ case_goal := CaseGoal,
                    list.det_replace_nth(Cases0, CaseNum, Case, Cases),
                    GoalExpr = switch(Var, CanFail, Cases),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCaseGoal = error(_)
                    ; MaybeCaseGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeCaseGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = negation(SubGoal0),
            ( if FirstStep = step_neg then
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap0, SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = negation(SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            ( if FirstStep = step_scope(_MaybeCut) then
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap0, SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = scope(Reason, SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = if_then_else(ExistVars, Cond0, Then0, Else0),
            ( if FirstStep = step_ite_cond then
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap0, Cond0, MaybeCond),
                (
                    MaybeCond = ok(Cond),
                    GoalExpr = if_then_else(ExistVars, Cond, Then0, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCond = error(_)
                    ; MaybeCond = goal_not_found
                    ),
                    MaybeGoal = MaybeCond
                )
            else if FirstStep = step_ite_then then
                apply_instmap_delta(
                    goal_info_get_instmap_delta(Cond0 ^ hg_info),
                    Instmap0, Instmap),
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap, Then0, MaybeThen),
                (
                    MaybeThen = ok(Then),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeThen = error(_)
                    ; MaybeThen = goal_not_found
                    ),
                    MaybeGoal = MaybeThen
                )
            else if FirstStep = step_ite_else then
                maybe_transform_goal_at_goal_path_with_instmap(TransformPred,
                    LaterPath, Instmap0, Else0, MaybeElse),
                (
                    MaybeElse = ok(Else),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then0, Else),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeElse = error(_)
                    ; MaybeElse = goal_not_found
                    ),
                    MaybeGoal = MaybeElse
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = shorthand(_),
            unexpected($pred, "shorthand")
        )
    ).

:- pred maybe_error_to_maybe_transformed_goal(maybe_error(hlds_goal)::in,
    maybe_transformed_goal::out) is det.

maybe_error_to_maybe_transformed_goal(ok(Goal), ok(Goal)).
maybe_error_to_maybe_transformed_goal(error(Error), error(Error)).

transform_all_goals(TransformPred, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Conjs0),
        list.map(transform_all_goals(TransformPred), Conjs0, Conjs),
        GoalExpr = conj(ConjType, Conjs)
    ;
        GoalExpr0 = disj(Disjs0),
        list.map(transform_all_goals(TransformPred), Disjs0, Disjs),
        GoalExpr = disj(Disjs)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(
            ( pred(Case0::in, Case::out) is det :-
                GoalI0 = Case0 ^ case_goal,
                transform_all_goals(TransformPred, GoalI0, GoalI),
                Case = Case0 ^ case_goal := GoalI
            ), Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        transform_all_goals(TransformPred, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        transform_all_goals(TransformPred, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(ExistVars, Cond0, Then0, Else0),
        transform_all_goals(TransformPred, Cond0, Cond),
        transform_all_goals(TransformPred, Then0, Then),
        transform_all_goals(TransformPred, Else0, Else),
        GoalExpr = if_then_else(ExistVars, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),
    Goal1 = hlds_goal(GoalExpr, GoalInfo0),
    TransformPred(Goal1, Goal).

%---------------------------------------------------------------------------%

switch_to_disjunction(_, [], _, [], !VarTable, !ModuleInfo).
switch_to_disjunction(Var, [Case | Cases], InstMap, Goals,
        !VarTable, !ModuleInfo) :-
    Case = case(MainConsId, OtherConsIds, CaseGoal),
    case_to_disjunct(Var, CaseGoal, InstMap, MainConsId, MainDisjunctGoal,
        !VarTable, !ModuleInfo),
    list.map_foldl2(case_to_disjunct(Var, CaseGoal, InstMap),
        OtherConsIds, OtherDisjunctGoals, !VarTable, !ModuleInfo),
    switch_to_disjunction(Var, Cases, InstMap, CasesGoals, !VarTable,
        !ModuleInfo),
    Goals = [MainDisjunctGoal | OtherDisjunctGoals] ++ CasesGoals.

case_to_disjunct(Var, CaseGoal, InstMap, ConsId, Disjunct,
        !VarTable, !ModuleInfo) :-
    ConsArity = cons_id_arity(ConsId),
    lookup_var_type(!.VarTable, Var, VarType),
    type_util.get_cons_id_arg_types(!.ModuleInfo, VarType, ConsId, ArgTypes),
    MakeArgEntry =
        ( pred(T::in, vte("", T, IsDummy)::out) is det :-
            IsDummy = is_type_a_dummy(!.ModuleInfo, T)
        ),
    list.map(MakeArgEntry, ArgTypes, ArgEntries),
    list.map_foldl(add_var_entry, ArgEntries, ArgVars, !VarTable),

    instmap_lookup_var(InstMap, Var, Inst0),
    ( if
        inst_expand(!.ModuleInfo, Inst0, Inst1),
        get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
    then
        ArgInsts = ArgInsts1
    else
        unexpected($pred, "get_arg_insts failed")
    ),
    InstToArgUnifyMode =
        ( pred(ArgInst::in, ArgUnfyiMode::out) is det :-
            ArgUnfyiMode = unify_modes_li_lf_ri_rf(ArgInst, ArgInst,
                free, ArgInst)
        ),
    list.map(InstToArgUnifyMode, ArgInsts, UniModes),
    UnifyMode = unify_modes_li_lf_ri_rf(Inst0, Inst0, Inst0, Inst0),
    UnifyContext = unify_context(umc_explicit, []),
    Unification = deconstruct(Var, ConsId, ArgVars, UniModes, can_fail,
        cannot_cgc),
    RHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars),
    ExtraGoalExpr = unify(Var, RHS, UnifyMode, Unification, UnifyContext),
    NonLocals = set_of_var.make_singleton(Var),
    instmap_delta_init_reachable(ExtraInstMapDelta0),
    instmap_delta_bind_var_to_functor(Var, VarType, ConsId, InstMap,
        ExtraInstMapDelta0, ExtraInstMapDelta, !ModuleInfo),
    goal_info_init(NonLocals, ExtraInstMapDelta,
        detism_semi, purity_pure, ExtraGoalInfo),

    % Conjoin the test and the rest of the case.
    goal_to_conj_list(CaseGoal, CaseGoalConj),
    GoalList = [hlds_goal(ExtraGoalExpr, ExtraGoalInfo) | CaseGoalConj],

    % Work out the nonlocals, instmap_delta and determinism
    % of the entire conjunction.
    CaseGoal = hlds_goal(_, CaseGoalInfo),
    CaseNonLocals0 = goal_info_get_nonlocals(CaseGoalInfo),
    set_of_var.insert(Var, CaseNonLocals0, CaseNonLocals),
    CaseInstMapDelta = goal_info_get_instmap_delta(CaseGoalInfo),
    instmap_delta_apply_instmap_delta(ExtraInstMapDelta, CaseInstMapDelta,
        test_size, InstMapDelta),
    CaseDetism0 = goal_info_get_determinism(CaseGoalInfo),
    det_conjunction_detism(detism_semi, CaseDetism0, Detism),
    CasePurity = goal_info_get_purity(CaseGoalInfo),
    goal_info_init(CaseNonLocals, InstMapDelta, Detism, CasePurity,
        CombinedGoalInfo),
    Disjunct = hlds_goal(conj(plain_conj, GoalList), CombinedGoalInfo).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_transform.
%---------------------------------------------------------------------------%
