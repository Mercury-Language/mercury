%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: find_bind_var.m.
% Main authors: fjh, zs.
%
% Both switch detection and cse elimination use this module
% to find deconstruction unifications for a variable in the arms
% of a disjunction.
%
%---------------------------------------------------------------------------%

:- module check_hlds.find_bind_var.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type found_deconstruct
    --->    did_find_deconstruct
    ;       did_not_find_deconstruct.

:- inst goal_expr_deconstruct for hlds_goal_expr/0
    --->    unify(ground, ground, ground, unification_deconstruct, ground).

:- type process_unify(Result, Info) ==
    pred(prog_var, hlds_goal_expr, hlds_goal_info, list(hlds_goal),
        Result, Result, Info, Info).
:- inst process_unify ==
    (pred(in, in(goal_expr_deconstruct), in, out,
        in, out, in, out) is det).

    % find_bind_var(Var, ProcessUnify, Goal0, Goal, !Result, !Info,
    %   FoundDeconstruct):
    %
    % Used by both switch_detection and cse_detection. Searches through
    % Goal0 looking for the first deconstruction unification with Var
    % or an alias of Var. If find_bind_var finds a deconstruction unification
    % of the variable, it calls ProcessUnify to handle it (which may replace
    % the unification with some other goals, which is why we return Goal),
    % and it stops searching. If it doesn't find such a deconstruction,
    % find_bind_var leaves !Result unchanged.
    %
:- pred find_bind_var(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    hlds_goal::in, hlds_goal::out, Result::in, Result::out,
    Info::in, Info::out, found_deconstruct::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.

:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module term_context.
:- import_module term_subst.

%---------------------------------------------------------------------------%

find_bind_var(Var, ProcessUnify, !Goal, !Result, !Info, FoundDeconstruct) :-
    map.init(Subst),
    find_bind_var_2(Var, ProcessUnify, !Goal, Subst, _,
        !Result, !Info, DeconstructSearch),
    (
        DeconstructSearch = before_deconstruct,
        FoundDeconstruct = did_not_find_deconstruct
    ;
        DeconstructSearch = found_deconstruct,
        FoundDeconstruct = did_find_deconstruct
    ;
        DeconstructSearch = given_up_search,
        FoundDeconstruct = did_not_find_deconstruct
    ).

:- type deconstruct_search
    --->    before_deconstruct
    ;       found_deconstruct
    ;       given_up_search.

:- pred find_bind_var_2(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    hlds_goal::in, hlds_goal::out,
    prog_substitution::in, prog_substitution::out, Result::in, Result::out,
    Info::in, Info::out, deconstruct_search::out) is det.

find_bind_var_2(Var, ProcessUnify, Goal0, Goal, !Subst,
        !Result, !Info, FoundDeconstruct) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = scope(Reason0, SubGoal0),
        ( if Reason0 = from_ground_term(_, from_ground_term_construct) then
            % There are no deconstruction unifications inside these scopes.
            Goal = Goal0,
            % Whether we want to keep looking at the code that follows them
            % is a more interesting question. Since we keep going after
            % construction unifications (whose behavior this scope resembles),
            % we keep going.
            FoundDeconstruct = before_deconstruct
        else
            find_bind_var_2(Var, ProcessUnify, SubGoal0, SubGoal,
                !Subst, !Result, !Info, FoundDeconstruct),
            ( if
                FoundDeconstruct = found_deconstruct,
                Reason0 = from_ground_term(_, from_ground_term_deconstruct)
            then
                % If we remove a goal from such a scope, what is left
                % may no longer satisfy the invariants we expect it to satisfy.
                Goal = SubGoal
            else
                Goal = hlds_goal(scope(Reason0, SubGoal), GoalInfo)
            )
        )
    ;
        GoalExpr0 = conj(ConjType, SubGoals0),
        (
            ConjType = plain_conj,
            (
                SubGoals0 = [],
                Goal = Goal0,
                FoundDeconstruct = before_deconstruct
            ;
                SubGoals0 = [_ | _],
                conj_find_bind_var(Var, ProcessUnify, SubGoals0, SubGoals,
                    !Subst, !Result, !Info, FoundDeconstruct),
                Goal = hlds_goal(conj(ConjType, SubGoals), GoalInfo)
            )
        ;
            ConjType = parallel_conj,
            Goal = Goal0,
            FoundDeconstruct = given_up_search
        )
    ;
        GoalExpr0 = unify(LHS, RHS, UnifyMode, UnifyInfo0, UnifyContext),
        ( if
            % Check whether the unification is a deconstruction unification
            % on either Var or on a variable aliased to Var.
            UnifyInfo0 = deconstruct(UnifyVar, _ConsId, _, _, _, _),
            term_subst.apply_rec_substitution_in_term(!.Subst,
                term.variable(Var, dummy_context),
                term.variable(SubstVar, dummy_context)),
            term_subst.apply_rec_substitution_in_term(!.Subst,
                term.variable(UnifyVar, dummy_context),
                term.variable(SubstUnifyVar, dummy_context)),
            SubstVar = SubstUnifyVar
        then
            % Rebuild GoalExpr0, but this time with the compiler knowing
            % that UnifyInfo0 is bound to deconstruct. Common structure
            % optimization should prevent any runtime overhead.
            GoalExpr1 = unify(LHS, RHS, UnifyMode, UnifyInfo0, UnifyContext),
            ProcessUnify(Var, GoalExpr1, GoalInfo, Goals, !Result, !Info),
            conj_list_to_goal(Goals, GoalInfo, Goal),
            FoundDeconstruct = found_deconstruct
        else
            Goal = Goal0,
            FoundDeconstruct = before_deconstruct,
            % Otherwise abstractly interpret the unification.
            % XXX interpret_unify is defined in det_util.m,
            % but it is used only here.
            % XXX The code that sets things up for interpretation
            % would be simpler if we had a version of library/term.m
            % that did NOT include a context in each term.
            ( if interpret_unify(LHS, RHS, !.Subst, NewSubst) then
                !:Subst = NewSubst
            else
                % The unification must fail - just ignore it.
                true
            )
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = disj(_)
        ; GoalExpr0 = switch(_, _, _)
        ; GoalExpr0 = negation(_)
        ; GoalExpr0 = if_then_else(_, _, _, _)
        ),
        Goal = Goal0,
        ( if goal_info_has_feature(GoalInfo, feature_from_head) then
            FoundDeconstruct = before_deconstruct
        else
            FoundDeconstruct = given_up_search
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(_, _, _, _, _, _, _),
            Goal = Goal0,
            FoundDeconstruct = given_up_search
        ;
            ShortHand0 = try_goal(_, _, _),
            Goal = Goal0,
            FoundDeconstruct = given_up_search
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- pred conj_find_bind_var(prog_var::in,
    process_unify(Result, Info)::in(process_unify),
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_substitution::in, prog_substitution::out, Result::in, Result::out,
    Info::in, Info::out, deconstruct_search::out) is det.

conj_find_bind_var(_Var, _, [], [],
        !Subst, !Result, !Info, before_deconstruct).
conj_find_bind_var(Var, ProcessUnify, [HeadGoal0 | TailGoals0], Goals,
        !Subst, !Result, !Info, FoundDeconstruct) :-
    find_bind_var_2(Var, ProcessUnify, HeadGoal0, HeadGoal, !Subst,
        !Result, !Info, FoundDeconstruct1),
    (
        FoundDeconstruct1 = before_deconstruct,
        conj_find_bind_var(Var, ProcessUnify, TailGoals0, TailGoals,
            !Subst, !Result, !Info, FoundDeconstruct),
        Goals = [HeadGoal | TailGoals]
    ;
        FoundDeconstruct1 = found_deconstruct,
        FoundDeconstruct = FoundDeconstruct1,
        HeadGoal = hlds_goal(HeadGoalExpr, _),
        ( if HeadGoalExpr = conj(_, []) then
            % HeadGoal is "true". Delete it now, so simplify doesn't have to.
            Goals = TailGoals0
        else
            Goals = [HeadGoal | TailGoals0]
        )
    ;
        FoundDeconstruct1 = given_up_search,
        FoundDeconstruct = FoundDeconstruct1,
        Goals = [HeadGoal | TailGoals0]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.find_bind_var.
%---------------------------------------------------------------------------%
