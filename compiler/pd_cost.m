%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pd_cost.m.
% Main author: stayl.
%
% goal_cost gives a very rough guess as to how much work a given goal
% will cause at runtime. This only counts the local cost not including
% the time taken by called predicates.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.pd_cost.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.

%---------------------------------------------------------------------------%

:- pred goal_cost(hlds_goal::in, int::out) is det.

:- func cost_of_reg_assign = int.
:- func cost_of_heap_assign = int.
:- func cost_of_simple_test = int.
:- func cost_of_heap_incr = int.
:- func cost_of_stack_flush = int.
:- func cost_of_builtin_call = int.
:- func cost_of_call = int.
:- func cost_of_higher_order_call = int.

:- func cost_of_eliminate_switch = int.
:- func cost_of_fold = int.
:- func cost_of_recursive_fold = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module list.
:- import_module require.

%---------------------------------------------------------------------------%

goal_cost(Goal, Cost) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    goal_expr_cost(GoalExpr, GoalInfo, Cost).

:- pred goal_expr_cost(hlds_goal_expr::in, hlds_goal_info::in, int::out)
    is det.

goal_expr_cost(GoalExpr, GoalInfo, Cost) :-
    (
        GoalExpr = conj(_, Goals),
        goal_costs(Goals, 0, Cost)
    ;
        GoalExpr = disj(Goals),
        goal_costs(Goals, 0, Cost0),
        Cost = Cost0 + cost_of_stack_flush
    ;
        GoalExpr = switch(_, _, Cases),
        cases_cost(Cases, cost_of_simple_test, Cost)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_cost(Cond, Cost1),
        goal_cost(Then, Cost2),
        goal_cost(Else, Cost3),
        Cost = Cost1 + Cost2 + Cost3
    ;
        GoalExpr = plain_call(_, _, Args, BuiltinState, _, _),
        (
            BuiltinState = inline_builtin,
            Cost = cost_of_builtin_call
        ;
            BuiltinState = not_builtin,
            list.length(Args, Arity),
            InputArgs = Arity // 2, % rough
            Cost = cost_of_stack_flush + cost_of_call
                + cost_of_reg_assign * InputArgs
        )
    ;
        GoalExpr = negation(Goal),
        goal_cost(Goal, Cost)
    ;
        GoalExpr = scope(Reason, Goal),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            Cost = cost_of_reg_assign
        else
            goal_cost(Goal, Cost)
        )
    ;
        GoalExpr = generic_call(_, Args, _, _, _),
        list.length(Args, Arity),
        Cost0 = cost_of_reg_assign * Arity // 2,
        Cost = Cost0 + cost_of_stack_flush + cost_of_higher_order_call
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        unify_cost(NonLocals, Unification, Cost)
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, Args, _, _, _),
        ( if get_may_call_mercury(Attributes) = proc_will_not_call_mercury then
            Cost1 = 0
        else
            Cost1 = cost_of_stack_flush
        ),
        % XXX This is *too* rough.
        list.length(Args, Arity),
        InputArgs = Arity // 2, % rough
        Cost = Cost1 + cost_of_call + cost_of_reg_assign * InputArgs
    ;
        GoalExpr = shorthand(_),
        % these should have been expanded out by now
        unexpected($pred, "shorthand")
    ).

:- pred unify_cost(set_of_progvar::in, unification::in, int::out) is det.

unify_cost(NonLocals, Unification, Cost) :-
    (
        Unification = assign(_, _),
        Cost = 0
    ;
        Unification = complicated_unify(_, _, _),
        Cost = cost_of_stack_flush
    ;
        Unification = simple_test(_, _),
        Cost = cost_of_simple_test
    ;
        Unification = construct(Var, _, Args, _, _, _, _),
        ( if set_of_var.member(NonLocals, Var) then
            list.length(Args, Arity),
            Cost = cost_of_heap_incr + Arity * cost_of_heap_assign
        else
            Cost = 0
        )
    ;
        Unification = deconstruct(_, _, Args, _, CanFail, _),
        (
            CanFail = can_fail,
            Cost0 = cost_of_simple_test
        ;
            CanFail = cannot_fail,
            Cost0 = 0
        ),
        list.filter(set_of_var.contains(NonLocals), Args, NonLocalArgs),
        list.length(NonLocalArgs, NumAssigns),
        Cost = Cost0 + cost_of_heap_incr + NumAssigns * cost_of_heap_assign
    ).

:- pred goal_costs(list(hlds_goal)::in, int::in, int::out) is det.

goal_costs([], Cost, Cost).
goal_costs([Goal | Goals], Cost0, Cost) :-
    goal_cost(Goal, Cost1),
    Cost2 = Cost0 + Cost1,
    goal_costs(Goals, Cost2, Cost).

:- pred cases_cost(list(case)::in, int::in, int::out) is det.

cases_cost([], Cost, Cost).
cases_cost([case(_, _, Goal) | Cases], Cost0, Cost) :-
    goal_cost(Goal, Cost1),
    Cost2 = Cost0 + Cost1,
    cases_cost(Cases, Cost2, Cost).

%---------------------------------------------------------------------------%

cost_of_reg_assign = 1.
cost_of_heap_assign = 2.
cost_of_simple_test = 3.
cost_of_heap_incr = 3.           % Depends on GC, want to penalise mem usage.
cost_of_stack_flush = 5.
cost_of_builtin_call = 3.        % very rough - int:'+' == float:'/' !.
cost_of_call = 3.
cost_of_higher_order_call = 8.

cost_of_eliminate_switch = 5.
cost_of_fold = 15.               % reward folding
cost_of_recursive_fold = 25.     % reward recursive folding more

%---------------------------------------------------------------------------%
:- end_module transform_hlds.pd_cost.
%---------------------------------------------------------------------------%
