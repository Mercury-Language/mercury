%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: delete_copy_goals.m.
%
% This module contains pass that executes after the transformation of
% clause bodies to superhomogeneous form, which also expands out references
% to state varuables. This post-pass deletes unneeded copy unifications
% that the previous pass could leave in its output. Such unifications
% can hide singleton variable problems.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.delete_copy_goals.
:- interface.

:- import_module hlds.hlds_goal.

    % Suppose we have a goal such as this:
    %
    %   ( if p(..., !.S, !:S) then
    %       <then_part>
    %   else
    %       <else_part>
    %   )
    %
    % and that !:S is not referred to anywhere in the clause
    % (not in then_part, not in else_part, not in the following code).
    %
    % In this form, warn_singletons can generate a warning about !:S
    % being a singleton. However, the state variable transformation
    % transforms it to code like this:
    %
    %   ( if p(..., STATE_VARIABLE_S_5, STATE_VARIABLE_S_6) then
    %       <then_part>, STATE_VARIABLE_S_7 = STATE_VARIABLE_S_6
    %   else
    %       <else_part>, STATE_VARIABLE_S_7 = STATE_VARIABLE_S_5
    %   )
    %
    % and marks both assignments to STATE_VARIABLE_S_7 as not being subject
    % to singleton warnings.
    %
    % We don't actually *want* to generate warnings about the assignments
    % to STATE_VARIABLE_S_7, because the problem is not in those assignments
    % or in the if-then-else arms that contain them. Instead, it is
    % in the condition. However, with this version of the code, the occurrence
    % of STATE_VARIABLE_S_6 in the condition is *not* a singleton.
    %
    % To allow us to generate a warning about !:S in the condition,
    % we delete all copy unifications inserted by the state variable
    % transformation that assign to a variable that is not referred to
    % either in the code that follows the assignment, or in the head.
    % (The head contains the output arguments, which will live
    % beyond the lifetime of anything in the clause body.)
    %
    % To find which variables occur after a copy goal, we have
    % delete_unneeded_copy_goals do a backwards traversal of the clause body,
    % keeping track of all the variables it has seen.
    %
    % To find which variables occur in the head, we use the call to goal_vars
    % below. The variables in HeadUnificationsGoal contain not just the
    % head_vars of the clause, but also the state variable instances any
    % of them are unified with. (This includes the input arguments as well as
    % the output arguments, but the clause body won't contain any assignments
    % to either the input arguments or the state variable instances
    % they are unified with, so including them in SeenLater0 is harmless.
    % As it happens, we have to include them because we don't know
    % which arguments are input and which are output, a distinction
    % that in any case may be mode-dependent.)
    %
    % We cannot count on the definitions of those state var instances being
    % before any of the occurrences of the head vars they are unified with.
    % For example, if a fact contains an !S argument pair, the call to
    % svar_finish_body in our caller will put the unification of the state
    % var instances representing !.S and !:S *after* HeadUnificationsGoal.
    % If we initialized SeenLater0 to just the head_vars of the clause,
    % this unification would assign to a variable that is *not* in SeenLater0,
    % and would thus be eliminated, which would be a bug.
    %
:- pred delete_unneeded_copy_goals_in_clause(hlds_goal::in,
    hlds_goal::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_vars.
:- import_module hlds.hlds_markers.
:- import_module hlds.make_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

delete_unneeded_copy_goals_in_clause(HeadUnificationsGoal, Goal0, Goal) :-
    vars_in_goal(HeadUnificationsGoal, HeadUnificationsGoalVars),
    SeenLater0 = HeadUnificationsGoalVars,
    delete_unneeded_copy_goals(Goal0, Goal, SeenLater0, _SeenLater).

:- pred delete_unneeded_copy_goals(hlds_goal::in, hlds_goal::out,
    set_of_progvar::in, set_of_progvar::out) is det.

delete_unneeded_copy_goals(Goal0, Goal, SeenAfter, SeenBefore) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = unify(LHSVar, _, _, _, _),
        vars_in_goal(Goal0, GoalVars0),
        ( if
            goal_info_has_feature(GoalInfo, feature_state_var_copy),
            not set_of_var.member(SeenAfter, LHSVar)
        then
            Goal = hlds_goal(true_goal_expr, GoalInfo),
            SeenBefore = SeenAfter
        else
            set_of_var.union(GoalVars0, SeenAfter, SeenBefore),
            Goal = Goal0
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        vars_in_goal(Goal0, GoalVars0),
        set_of_var.union(GoalVars0, SeenAfter, SeenBefore),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjKind, Conjuncts0),
        % Processing Conjuncts0 without reversing it would lead to recursion
        % as deep as Conjuncts0 is long. Since Conjuncts0 can be very long,
        % we prefer to pay the price of reversing and unreversing the list
        % to achieve tail recursion.
        list.reverse(Conjuncts0, RevConjuncts0),
        delete_unneeded_copy_goals_rev_conj(RevConjuncts0, RevConjuncts,
            SeenAfter, SeenBefore),
        list.reverse(RevConjuncts, Conjuncts),
        GoalExpr = conj(ConjKind, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Disjuncts0),
        delete_unneeded_copy_goals_disj(Disjuncts0, Disjuncts,
            SeenAfter, SeenBefores),
        GoalExpr = disj(Disjuncts),
        set_of_var.union_list(SeenBefores, SeenBefore),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        % Switches should not exist at this point in the compilation process,
        % but it is simple enough to prepare here for the eventuality that
        % this may change in the future.
        delete_unneeded_copy_goals_switch(Cases0, Cases,
            SeenAfter, SeenBefores),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        set_of_var.union_list(SeenBefores, SeenBefore0),
        set_of_var.insert(SwitchVar, SeenBefore0, SeenBefore),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(ITEVars, Cond0, Then0, Else0),
        delete_unneeded_copy_goals(Else0, Else, SeenAfter, SeenBeforeElse),
        delete_unneeded_copy_goals(Then0, Then, SeenAfter, SeenAfterThen),
        delete_unneeded_copy_goals(Cond0, Cond, SeenAfterThen, SeenBeforeCond),
        GoalExpr = if_then_else(ITEVars, Cond, Then, Else),
        set_of_var.union(SeenBeforeCond, SeenBeforeElse, SeenBefore0),
        set_of_var.insert_list(ITEVars, SeenBefore0, SeenBefore),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        delete_unneeded_copy_goals(SubGoal0, SubGoal, SeenAfter, SeenBefore),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = from_ground_term(TermVar, _Kind),
            % There won't be any feature_state_var_copy goals inside SubGoal0.
            SubGoal = SubGoal0,
            % None of the variables in SubGoal can occur in the rest of the
            % procedure body, with the exception of TermVar.
            set_of_var.insert(TermVar, SeenAfter, SeenBefore)
        ;
            ( Reason = require_complete_switch(ScopeVar)
            ; Reason = require_switch_arms_detism(ScopeVar, _Detism)
            ),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert(ScopeVar, SeenBefore0, SeenBefore)
        ;
            Reason = loop_control(LCVar, LCSVar, _UseParentStack),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert_list([LCVar, LCSVar], SeenBefore0, SeenBefore)
        ;
            ( Reason = exist_quant(ScopeVars, _)
            ; Reason = promise_solutions(ScopeVars, _PromiseKind)
            ; Reason = trace_goal(_Comp, _Run, _MaybeIO, _Mutables, ScopeVars)
            ),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert_list(ScopeVars, SeenBefore0, SeenBefore)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(AtomicType,
                atomic_interface_vars(OuterInitVar, OuterFinalVar),
                atomic_interface_vars(InnerInitVar, InnerFinalVar),
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            expect(unify(OrElseInners, []), $pred, "OrElseInners != []"),
            Disjuncts0 = [MainGoal0 | OrElseGoals0],
            delete_unneeded_copy_goals_disj(Disjuncts0, Disjuncts,
                SeenAfter, SeenBefores),
            (
                Disjuncts = [],
                unexpected($pred, "Disjuncts = []")
            ;
                Disjuncts = [MainGoal | OrElseGoals]
            ),
            ShortHand = atomic_goal(AtomicType,
                atomic_interface_vars(OuterInitVar, OuterFinalVar),
                atomic_interface_vars(InnerInitVar, InnerFinalVar),
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
            set_of_var.union_list(SeenBefores, SeenBefore0),
            set_of_var.insert_list([OuterInitVar, OuterFinalVar,
                InnerInitVar, InnerFinalVar], SeenBefore0, SeenBefore1),
            (
                MaybeOutputVars = no,
                SeenBefore = SeenBefore1
            ;
                MaybeOutputVars = yes(OutputVars),
                set_of_var.insert_list(OutputVars, SeenBefore1, SeenBefore)
            )
        ;
            ShortHand0 = try_goal(MaybeIOStateVars, ResultVar, SubGoal0),
            delete_unneeded_copy_goals(SubGoal0, SubGoal,
                SeenAfter, SeenBefore0),
            set_of_var.insert(ResultVar, SeenBefore0, SeenBefore1),
            (
                MaybeIOStateVars = no,
                SeenBefore = SeenBefore1
            ;
                MaybeIOStateVars = yes(try_io_state_vars(InitVar, FinalVar)),
                set_of_var.insert(InitVar, SeenBefore1, SeenBefore2),
                set_of_var.insert(FinalVar, SeenBefore2, SeenBefore)
            ),
            ShortHand = try_goal(MaybeIOStateVars, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(LeftGoal0, RightGoal0),
            delete_unneeded_copy_goals(LeftGoal0, LeftGoal,
                SeenAfter, SeenBeforeLeft),
            delete_unneeded_copy_goals(RightGoal0, RightGoal,
                SeenAfter, SeenBeforeRight),
            set_of_var.union(SeenBeforeLeft, SeenBeforeRight, SeenBefore),
            ShortHand = bi_implication(LeftGoal, RightGoal)
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred delete_unneeded_copy_goals_rev_conj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out) is det.

delete_unneeded_copy_goals_rev_conj([], [], SeenAfter, SeenBefore) :-
    SeenBefore = SeenAfter.
delete_unneeded_copy_goals_rev_conj(
        [RevConjunct0 | RevConjuncts0], [RevConjunct | RevConjuncts],
        SeenAfter, SeenBefore) :-
    delete_unneeded_copy_goals(RevConjunct0, RevConjunct,
        SeenAfter, SeenBetween),
    delete_unneeded_copy_goals_rev_conj(RevConjuncts0, RevConjuncts,
        SeenBetween, SeenBefore).

:- pred delete_unneeded_copy_goals_disj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, list(set_of_progvar)::out) is det.

delete_unneeded_copy_goals_disj([], [], _, []).
delete_unneeded_copy_goals_disj(
        [Disjunct0 | Disjuncts0], [Disjunct | Disjuncts],
        SeenAfter, [SeenBefore | SeenBefores]) :-
    delete_unneeded_copy_goals(Disjunct0, Disjunct, SeenAfter, SeenBefore),
    delete_unneeded_copy_goals_disj(Disjuncts0, Disjuncts,
        SeenAfter, SeenBefores).

:- pred delete_unneeded_copy_goals_switch(list(case)::in, list(case)::out,
    set_of_progvar::in, list(set_of_progvar)::out) is det.

delete_unneeded_copy_goals_switch([], [], _, []).
delete_unneeded_copy_goals_switch([Case0 | Cases0], [Case | Cases],
        SeenAfter, [SeenBefore | SeenBefores]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    delete_unneeded_copy_goals(Goal0, Goal, SeenAfter, SeenBefore),
    Case = case(MainConsId, OtherConsIds, Goal),
    delete_unneeded_copy_goals_switch(Cases0, Cases, SeenAfter, SeenBefores).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.delete_copy_goals.
%---------------------------------------------------------------------------%
