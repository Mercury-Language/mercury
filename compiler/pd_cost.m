%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: pd_cost.m
% Main author: stayl
%
% pd_cost__goal gives a very rough guess as to how much work a given goal
% will cause at runtime. This only counts the local cost not including
% the time taken by called predicates.
%
%-----------------------------------------------------------------------------%
:- module transform_hlds__pd_cost.

:- interface.

:- import_module hlds__hlds_goal.

:- pred pd_cost__goal(hlds_goal::in, int::out) is det.

:- pred pd_cost__reg_assign(int::out) is det.
:- pred pd_cost__heap_assign(int::out) is det.
:- pred pd_cost__simple_test(int::out) is det.
:- pred pd_cost__heap_incr(int::out) is det.
:- pred pd_cost__stack_flush(int::out) is det.
:- pred pd_cost__builtin_call(int::out) is det.
:- pred pd_cost__call(int::out) is det.
:- pred pd_cost__higher_order_call(int::out) is det.

:- pred pd_cost__eliminate_switch(int::out) is det.
:- pred pd_cost__fold(int::out) is det.
:- pred pd_cost__recursive_fold(int::out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds__hlds_data, parse_tree__prog_data.
:- import_module int, list, set, std_util, require.

%-----------------------------------------------------------------------------%

pd_cost__goal(conj(Goals) - _, Cost) :-
	pd_cost__goals(Goals, 0, Cost).

pd_cost__goal(par_conj(Goals) - _, Cost) :-
	pd_cost__goals(Goals, 0, Cost).

pd_cost__goal(disj(Goals) - _, Cost) :-
	pd_cost__goals(Goals, 0, Cost0),
	pd_cost__stack_flush(Cost1),
	Cost is Cost0 + Cost1.

pd_cost__goal(switch(_, _, Cases) - _, Cost) :-
	pd_cost__simple_test(Cost0),
	pd_cost__cases(Cases, Cost0, Cost).

pd_cost__goal(if_then_else(_, Cond, Then, Else) - _, Cost) :-
	pd_cost__goal(Cond, Cost1),
	pd_cost__goal(Then, Cost2),
	pd_cost__goal(Else, Cost3),
	Cost is Cost1 + Cost2 + Cost3.

pd_cost__goal(call(_, _, Args, BuiltinState, _, _) - _, Cost) :-
	( BuiltinState = inline_builtin ->
		pd_cost__builtin_call(Cost)
	;
		pd_cost__stack_flush(Cost1),
		list__length(Args, Arity),
		InputArgs is Arity // 2,	% rough
		pd_cost__reg_assign(AssignCost),
		pd_cost__call(Cost2),
		Cost is Cost1 + Cost2 + AssignCost * InputArgs
	).

pd_cost__goal(not(Goal) - _, Cost) :-
	pd_cost__goal(Goal, Cost).

pd_cost__goal(some(_, _, Goal) - _, Cost) :-
	pd_cost__goal(Goal, Cost).

pd_cost__goal(generic_call(_, Args, _, _) - _, Cost) :-
	list__length(Args, Arity),
	pd_cost__reg_assign(AssignCost),
	Cost0 = AssignCost * Arity // 2,
	pd_cost__stack_flush(Cost1),
	pd_cost__higher_order_call(Cost2),
	Cost is Cost0 + Cost1 + Cost2.

pd_cost__goal(unify(_, _, _, Unification, _) - GoalInfo, Cost) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	pd_cost__unify(NonLocals, Unification, Cost).

pd_cost__goal(foreign_proc(Attributes, _, _, Args, _, _, _) - _,
		Cost) :-
	( may_call_mercury(Attributes, will_not_call_mercury) ->
		Cost1 = 0
	;
		pd_cost__stack_flush(Cost1)
	),
	pd_cost__call(Cost2),
	list__length(Args, Arity),
	InputArgs is Arity // 2,	% rough
	pd_cost__reg_assign(AssignCost),
	Cost is Cost1 + Cost2 + AssignCost * InputArgs.

pd_cost__goal(shorthand(_) - _, _) :-
	% these should have been expanded out by now
	error("pd_cost__goal: unexpected shorthand").

:- pred pd_cost__unify(set(prog_var)::in, unification::in, int::out) is det.

pd_cost__unify(_, assign(_, _), 0).

pd_cost__unify(_, complicated_unify(_, _, _), Cost) :-
	pd_cost__stack_flush(Cost).

pd_cost__unify(_, simple_test(_, _), Cost) :-
	pd_cost__simple_test(Cost).

pd_cost__unify(NonLocals, construct(Var, _, Args, _, _, _, _), Cost) :-
	( set__member(Var, NonLocals) ->
		list__length(Args, Arity),
		pd_cost__heap_incr(Cost1),
		pd_cost__heap_assign(Cost2),
		Cost = Cost1 + Arity * Cost2
	;
		Cost = 0
	).

pd_cost__unify(NonLocals, deconstruct(_, _, Args, _, CanFail, _), Cost) :-
	( CanFail = can_fail ->
		pd_cost__simple_test(Cost0)
	;
		Cost0 = 0
	),
	list__filter(
		lambda([X::in] is semidet,
			set__member(X, NonLocals)
		), Args, NonLocalArgs),
	list__length(NonLocalArgs, NumAssigns),
	pd_cost__heap_incr(Cost1),
	pd_cost__heap_assign(Cost2),
	Cost = Cost0 + Cost1 + NumAssigns * Cost2.

:- pred pd_cost__goals(list(hlds_goal)::in, int::in, int::out) is det.

pd_cost__goals([], Cost, Cost).
pd_cost__goals([Goal | Goals], Cost0, Cost) :-
	pd_cost__goal(Goal, Cost1),
	Cost2 is Cost0 + Cost1,
	pd_cost__goals(Goals, Cost2, Cost).

:- pred pd_cost__cases(list(case)::in, int::in, int::out) is det.

pd_cost__cases([], Cost, Cost).
pd_cost__cases([case(_, Goal) | Cases], Cost0, Cost) :-
	pd_cost__goal(Goal, Cost1),
	Cost2 is Cost0 + Cost1,
	pd_cost__cases(Cases, Cost2, Cost).

%-----------------------------------------------------------------------------%

pd_cost__reg_assign(1).
pd_cost__heap_assign(2).
pd_cost__simple_test(3).
pd_cost__heap_incr(3).		% Depends on GC, want to penalise mem usage.
pd_cost__stack_flush(5).
pd_cost__builtin_call(3).	% very rough - int:'+' == float:'/' !.
pd_cost__call(3).
pd_cost__higher_order_call(8).

pd_cost__eliminate_switch(5).
pd_cost__fold(15).		% reward folding 
pd_cost__recursive_fold(25).	% reward recursive folding more

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
