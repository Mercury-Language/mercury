%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: pd_cost.m
% Main author: stayl

% goal_cost gives a very rough guess as to how much work a given goal
% will cause at runtime. This only counts the local cost not including
% the time taken by called predicates.

%-----------------------------------------------------------------------------%

:- module transform_hlds.pd_cost.
:- interface.

:- import_module hlds.hlds_goal.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module list.
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%

goal_cost(conj(_, Goals) - _, Cost) :-
    goal_costs(Goals, 0, Cost).

goal_cost(disj(Goals) - _, Cost) :-
    goal_costs(Goals, 0, Cost0),
    Cost = Cost0 + cost_of_stack_flush.

goal_cost(switch(_, _, Cases) - _, Cost) :-
    cases_cost(Cases, cost_of_simple_test, Cost).

goal_cost(if_then_else(_, Cond, Then, Else) - _, Cost) :-
    goal_cost(Cond, Cost1),
    goal_cost(Then, Cost2),
    goal_cost(Else, Cost3),
    Cost = Cost1 + Cost2 + Cost3.

goal_cost(call(_, _, Args, BuiltinState, _, _) - _, Cost) :-
    ( BuiltinState = inline_builtin ->
        Cost = cost_of_builtin_call
    ;
        list.length(Args, Arity),
        InputArgs = Arity // 2, % rough
        Cost = cost_of_stack_flush + cost_of_call
            + cost_of_reg_assign * InputArgs
    ).

goal_cost(not(Goal) - _, Cost) :-
    goal_cost(Goal, Cost).

goal_cost(scope(_, Goal) - _, Cost) :-
    goal_cost(Goal, Cost).

goal_cost(generic_call(_, Args, _, _) - _, Cost) :-
    list.length(Args, Arity),
    Cost0 = cost_of_reg_assign * Arity // 2,
    Cost = Cost0 + cost_of_stack_flush + cost_of_higher_order_call.

goal_cost(unify(_, _, _, Unification, _) - GoalInfo, Cost) :-
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    unify_cost(NonLocals, Unification, Cost).

goal_cost(foreign_proc(Attributes, _, _, Args, _, _) - _, Cost) :-
    ( may_call_mercury(Attributes) = will_not_call_mercury ->
        Cost1 = 0
    ;
        Cost1 = cost_of_stack_flush
    ),
    list.length(Args, Arity),
    InputArgs = Arity // 2, % rough
    Cost = Cost1 + cost_of_call + cost_of_reg_assign * InputArgs.

goal_cost(shorthand(_) - _, _) :-
    % these should have been expanded out by now
    unexpected(this_file, "goal_cost: unexpected shorthand").

:- pred unify_cost(set(prog_var)::in, unification::in, int::out) is det.

unify_cost(_, assign(_, _), 0).
unify_cost(_, complicated_unify(_, _, _), cost_of_stack_flush).
unify_cost(_, simple_test(_, _), cost_of_simple_test).
unify_cost(NonLocals, construct(Var, _, Args, _, _, _, _), Cost) :-
    ( set.member(Var, NonLocals) ->
        list.length(Args, Arity),
        Cost = cost_of_heap_incr + Arity * cost_of_heap_assign
    ;
        Cost = 0
    ).
unify_cost(NonLocals, deconstruct(_, _, Args, _, CanFail, _), Cost) :-
    (
        CanFail = can_fail,
        Cost0 = cost_of_simple_test
    ;
        CanFail = cannot_fail,
        Cost0 = 0
    ),
    list.filter((pred(X::in) is semidet :-
            set.member(X, NonLocals)
        ), Args, NonLocalArgs),
    list.length(NonLocalArgs, NumAssigns),
    Cost = Cost0 + cost_of_heap_incr + NumAssigns * cost_of_heap_assign.

:- pred goal_costs(list(hlds_goal)::in, int::in, int::out) is det.

goal_costs([], Cost, Cost).
goal_costs([Goal | Goals], Cost0, Cost) :-
    goal_cost(Goal, Cost1),
    Cost2 = Cost0 + Cost1,
    goal_costs(Goals, Cost2, Cost).

:- pred cases_cost(list(case)::in, int::in, int::out) is det.

cases_cost([], Cost, Cost).
cases_cost([case(_, Goal) | Cases], Cost0, Cost) :-
    goal_cost(Goal, Cost1),
    Cost2 = Cost0 + Cost1,
    cases_cost(Cases, Cost2, Cost).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "pd_cost.m".

%-----------------------------------------------------------------------------%
:- end_module pd_cost.
%-----------------------------------------------------------------------------%
