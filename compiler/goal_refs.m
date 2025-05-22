%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015-2019, 2021-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_refs.m.
%
% This module provides predicates for answering the question
% "what predicates and functions are referenced by this goal?".
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_refs.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % Return a set of all the procedures referred to by the given goal or
    % goals. Differs from goal_calls not just in the fact that it returns
    % a set of pred_proc_ids all at once, but also in that it also returns
    % procedures which are not CALLED in the given goal or goals but which
    % are referred to in other ways (e.g. by having their address taken).
    %
:- func goals_proc_refs(list(hlds_goal)) = set(pred_proc_id).
:- func goal_proc_refs(hlds_goal) = set(pred_proc_id).

%---------------------%

    % Test whether the goal calls the given procedure.
    %
:- pred goal_calls(hlds_goal, pred_proc_id).
:- mode goal_calls(in, in) is semidet.
:- mode goal_calls(in, out) is nondet.

%---------------------%

    % Test whether the goal calls the given predicate.
    % This is useful before mode analysis when the proc_ids
    % have not been determined.
    %
:- pred goal_calls_pred_id(hlds_goal, pred_id).
:- mode goal_calls_pred_id(in, in) is semidet.
:- mode goal_calls_pred_id(in, out) is nondet.

%---------------------%

    % goal_calls_proc_in_set(Goal, PredProcIds):
    %
    % Returns the subset of PredProcIds that are called from somewhere inside
    % Goal via plain_call.
    %
:- func goal_calls_proc_in_set(hlds_goal, set(pred_proc_id))
    = set(pred_proc_id).

    % goal_list_calls_proc_in_list(Goal, PredProcIds):
    %
    % Returns the subset of PredProcIds that are called from somewhere inside
    % Goals via plain_call.
    %
:- func goal_list_calls_proc_in_set(list(hlds_goal), set(pred_proc_id))
    = set(pred_proc_id).

%---------------------------------------------------------------------------%

    % Returns all the pred_ids that are called from a list of goals.
    %
:- pred pred_ids_called_from_goals(list(hlds_goal)::in,
    list(pred_id)::out) is det.

    % Returns all the pred_ids that are called from a goal.
    %
:- pred pred_ids_called_from_goal(hlds_goal::in, list(pred_id)::out) is det.

    % Returns all the pred_id/arg_list pairs that are called from a goal.
    %
:- pred pred_ids_args_called_from_goal(hlds_goal::in,
    list({pred_id, list(prog_var)})::out) is det.

    % Returns all the pred_proc_ids that are called from a goal.
    %
:- pred pred_proc_ids_called_from_goal(hlds_goal::in,
    list(pred_proc_id)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_contains.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.

:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module solutions.

%---------------------------------------------------------------------------%

goals_proc_refs(Goals) = ReferredToProcs :-
    list.foldl(goal_proc_refs_acc, Goals, set.init, ReferredToProcs).

goal_proc_refs(Goal) = ReferredToProcs :-
    goal_proc_refs_acc(Goal, set.init, ReferredToProcs).

:- pred goal_proc_refs_acc(hlds_goal::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

goal_proc_refs_acc(Goal, !ReferredToProcs) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        list.foldl(goal_proc_refs_acc, Conjuncts, !ReferredToProcs)
    ;
        GoalExpr = disj(Disjuncts),
        list.foldl(goal_proc_refs_acc, Disjuncts, !ReferredToProcs)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl(case_proc_refs_acc, Cases, !ReferredToProcs)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_proc_refs_acc(Cond, !ReferredToProcs),
        goal_proc_refs_acc(Then, !ReferredToProcs),
        goal_proc_refs_acc(Else, !ReferredToProcs)
    ;
        GoalExpr = negation(SubGoal),
        goal_proc_refs_acc(SubGoal, !ReferredToProcs)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction and deconstruction
            % unifications respectively.
            true
        else
            goal_proc_refs_acc(SubGoal, !ReferredToProcs)
        )
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(SubGoalA, SubGoalB),
            goal_proc_refs_acc(SubGoalA, !ReferredToProcs),
            goal_proc_refs_acc(SubGoalB, !ReferredToProcs)
        ;
            Shorthand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            goal_proc_refs_acc(MainGoal, !ReferredToProcs),
            list.foldl(goal_proc_refs_acc, OrElseGoals, !ReferredToProcs)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            goal_proc_refs_acc(SubGoal, !ReferredToProcs)
        )
    ;
        ( GoalExpr = plain_call(PredId, ProcId, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, PredId, ProcId, _, _, _, _)
        ),
        PredProcId = proc(PredId, ProcId),
        set.insert(PredProcId, !ReferredToProcs)
    ;
        GoalExpr = unify(_LHS, RHS, _, Unification, _),
        (
            RHS = rhs_var(_)
        ;
            RHS = rhs_functor(RHSConsId, _IsExistConstruct, _ArgVars),
            cons_id_proc_refs_acc(RHSConsId, !ReferredToProcs)
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, SubGoal),
            goal_proc_refs_acc(SubGoal, !ReferredToProcs)
        ),
        (
            Unification = construct(_LHSVar, ConstructConsId, _RHSVars,
                _ArgModes, _How, _IsUnique, _SubInfo),
            cons_id_proc_refs_acc(ConstructConsId, !ReferredToProcs)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            ; Unification = complicated_unify(_, _, _)
            )
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ).

:- pred case_proc_refs_acc(case::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

case_proc_refs_acc(Case, !ReferredToProcs) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    goal_proc_refs_acc(Goal, !ReferredToProcs).

:- pred cons_id_proc_refs_acc(cons_id::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

cons_id_proc_refs_acc(ConsId, !ReferredToProcs) :-
    (
        ( ConsId = du_data_ctor(_)
        ; ConsId = tuple_cons(_)
        ; ConsId = some_int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ;
        ConsId = closure_cons(ShroudedPredProcId),
        PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
        set.insert(PredProcId, !ReferredToProcs)
    ).

%---------------------------------------------------------------------------%
%
% We could implement goal_calls as
%   goal_calls(Goal, proc(PredId, ProcId)) :-
%       goal_contains_subgoal(Goal, plain_call(PredId, ProcId, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

% XXX STM
% split this predicate into two:
% goal_calls_this_proc(Goal, PredProcId) = bool
% all_called_procs_in_goal(Goal) = cord(pred_proc_id)

goal_calls(Goal, PredProcId) :-
    Goal = hlds_goal(GoalExpr, _),
    require_complete_switch [GoalExpr]
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        goals_calls(Conjuncts, PredProcId)
    ;
        GoalExpr = disj(Disjuncts),
        goals_calls(Disjuncts, PredProcId)
    ;
        GoalExpr = switch(_, _, Cases),
        cases_calls(Cases, PredProcId)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( goal_calls(Cond, PredProcId)
        ; goal_calls(Then, PredProcId)
        ; goal_calls(Else, PredProcId)
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_calls(SubGoal, PredProcId)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction and deconstruction
            % unifications respectively.
            fail
        else
            goal_calls(SubGoal, PredProcId)
        )
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        PredProcId = proc(PredId, ProcId)
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

:- pred goals_calls(list(hlds_goal), pred_proc_id).
:- mode goals_calls(in, in) is semidet.
:- mode goals_calls(in, out) is nondet.

goals_calls([Goal | Goals], PredProcId) :-
    (
        goal_calls(Goal, PredProcId)
    ;
        goals_calls(Goals, PredProcId)
    ).

:- pred cases_calls(list(case), pred_proc_id).
:- mode cases_calls(in, in) is semidet.
:- mode cases_calls(in, out) is nondet.

cases_calls([case(_, _, Goal) | Cases], PredProcId) :-
    (
        goal_calls(Goal, PredProcId)
    ;
        cases_calls(Cases, PredProcId)
    ).

%---------------------------------------------------------------------------%
%
% We could implement goal_calls_pred_id as
%   goal_calls_pred_id(Goal, PredId) :-
%       goal_contains_subgoal(Goal, plain_call(PredId, _, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls_pred_id(Goal, PredId) :-
    Goal = hlds_goal(GoalExpr, _),
    require_complete_switch [GoalExpr]
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        goals_calls_pred_id(Conjuncts, PredId)
    ;
        GoalExpr = disj(Disjuncts),
        goals_calls_pred_id(Disjuncts, PredId)
    ;
        GoalExpr = switch(_, _, Cases),
        cases_calls_pred_id(Cases, PredId)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( goal_calls_pred_id(Cond, PredId)
        ; goal_calls_pred_id(Then, PredId)
        ; goal_calls_pred_id(Else, PredId)
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_calls_pred_id(SubGoal, PredId)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction and deconstruction
            % unifications respectively.
            fail
        else
            goal_calls_pred_id(SubGoal, PredId)
        )
    ;
        GoalExpr = plain_call(PredId, _, _, _, _, _)
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

:- pred goals_calls_pred_id(list(hlds_goal), pred_id).
:- mode goals_calls_pred_id(in, in) is semidet.
:- mode goals_calls_pred_id(in, out) is nondet.

goals_calls_pred_id([Goal | Goals], PredId) :-
    (
        goal_calls_pred_id(Goal, PredId)
    ;
        goals_calls_pred_id(Goals, PredId)
    ).

:- pred cases_calls_pred_id(list(case), pred_id).
:- mode cases_calls_pred_id(in, in) is semidet.
:- mode cases_calls_pred_id(in, out) is nondet.

cases_calls_pred_id([case(_, _, Goal) | Cases], PredId) :-
    (
        goal_calls_pred_id(Goal, PredId)
    ;
        cases_calls_pred_id(Cases, PredId)
    ).

%---------------------------------------------------------------------------%

goal_calls_proc_in_set(Goal, PredProcIds) = CalledPredProcIds :-
    goal_calls_proc_in_set_acc(Goal, PredProcIds,
        set.init, CalledPredProcIds).

goal_list_calls_proc_in_set(Goals, PredProcIds) = CalledPredProcIds :-
    goal_list_calls_proc_in_set_acc(Goals, PredProcIds,
        set.init, CalledPredProcIds).

:- pred goal_calls_proc_in_set_acc(hlds_goal::in, set(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( if set.member(proc(PredId, ProcId), PredProcIds) then
            set.insert(proc(PredId, ProcId), !CalledSet)
        else
            true
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = conj(_, SubGoals),
        goal_list_calls_proc_in_set_acc(SubGoals, PredProcIds, !CalledSet)
    ;
        GoalExpr = disj(SubGoals),
        goal_list_calls_proc_in_set_acc(SubGoals, PredProcIds, !CalledSet)
    ;
        GoalExpr = switch(_, _, Cases),
        case_list_calls_proc_in_list_acc(Cases, PredProcIds, !CalledSet)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_calls_proc_in_set_acc(Cond, PredProcIds, !CalledSet),
        goal_calls_proc_in_set_acc(Then, PredProcIds, !CalledSet),
        goal_calls_proc_in_set_acc(Else, PredProcIds, !CalledSet)
    ;
        GoalExpr = negation(SubGoal),
        goal_calls_proc_in_set_acc(SubGoal, PredProcIds, !CalledSet)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction unifications.
            true
        else
            goal_calls_proc_in_set_acc(SubGoal, PredProcIds, !CalledSet)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            goal_calls_proc_in_set_acc(MainGoal, PredProcIds, !CalledSet),
            goal_list_calls_proc_in_set_acc(OrElseGoals, PredProcIds,
                !CalledSet)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            goal_calls_proc_in_set_acc(SubGoal, PredProcIds, !CalledSet)
        ;
            ShortHand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- pred goal_list_calls_proc_in_set_acc(list(hlds_goal)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::in, set(pred_proc_id)::out)
    is det.

goal_list_calls_proc_in_set_acc([], _, !CalledSet).
goal_list_calls_proc_in_set_acc([Goal | Goals], PredProcIds, !CalledSet) :-
    goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet),
    goal_list_calls_proc_in_set_acc(Goals, PredProcIds, !CalledSet).

:- pred case_list_calls_proc_in_list_acc(list(case)::in, set(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

case_list_calls_proc_in_list_acc([], _, !CalledSet).
case_list_calls_proc_in_list_acc([Case | Cases], PredProcIds, !CalledSet) :-
    Case = case(_, _, Goal),
    goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet),
    case_list_calls_proc_in_list_acc(Cases, PredProcIds, !CalledSet).

%---------------------------------------------------------------------------%

pred_ids_called_from_goals(Goals, PredIds) :-
    (
        Goals = [],
        PredIds = []
    ;
        Goals = [HeadGoal | TailGoals],
        pred_ids_called_from_goal(HeadGoal, HeadPredIds),
        pred_ids_called_from_goals(TailGoals, TailPredIds),
        PredIds = HeadPredIds ++ TailPredIds
    ).

pred_ids_called_from_goal(Goal, PredIds) :-
    % Explicit lambda expression needed since goal_calls_pred_id
    % has multiple modes.
    P = ( pred(PredId::out) is nondet :-
            goal_contains_goal(Goal, SubGoal),
            SubGoal = hlds_goal(SubGoalExpr, _),
            SubGoalExpr = plain_call(PredId, _, _, _, _, _)
        ),
    solutions.solutions(P, PredIds).

pred_ids_args_called_from_goal(Goal, List) :-
    P = ( pred({PredId, Args}::out) is nondet :-
            goal_contains_goal(Goal, SubGoal),
            SubGoal = hlds_goal(SubGoalExpr, _),
            SubGoalExpr = plain_call(PredId, _, Args, _, _, _)
        ),
    solutions(P, List).

pred_proc_ids_called_from_goal(Goal, PredProcIds) :-
    P = ( pred(PredProcId::out) is nondet :-
            goal_contains_goal(Goal, SubGoal),
            SubGoal = hlds_goal(SubGoalExpr, _),
            SubGoalExpr = plain_call(PredId, ProcId, _, _, _, _),
            PredProcId = proc(PredId, ProcId)
        ),
    solutions.solutions(P, PredProcIds).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_refs.
%---------------------------------------------------------------------------%
