%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Auxiliary code generator module. Unlike code_util, it imports code_info.
%
% Main authors: conway, zs.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ll_backend__code_aux.

:- interface.

:- import_module ll_backend__code_info, hlds__hlds_module, hlds__hlds_goal.
:- import_module parse_tree__prog_data.
:- import_module bool.

	% code_aux__contains_only_builtins(G) is true if G is a leaf procedure,
	% i.e. control does not leave G to call another procedure, even if
	% that procedure is a complicated unification.

:- pred code_aux__contains_only_builtins(hlds_goal).
:- mode code_aux__contains_only_builtins(in) is semidet.

	% Succeeds if the goal cannot loop forever.
:- pred code_aux__goal_cannot_loop(module_info, hlds_goal).
:- mode code_aux__goal_cannot_loop(in, in) is semidet.

	% Succeeds if the goal cannot loop forever or throw an exception.
:- pred code_aux__goal_cannot_loop_or_throw(hlds_goal).
:- mode code_aux__goal_cannot_loop_or_throw(in) is semidet.

	% Succeeds if the goal can loop forever.
:- pred code_aux__goal_can_loop(module_info, hlds_goal).
:- mode code_aux__goal_can_loop(in, in) is semidet.

	% Succeeds if the goal can loop forever or throw an exception.
:- pred code_aux__goal_can_loop_or_throw(hlds_goal).
:- mode code_aux__goal_can_loop_or_throw(in) is semidet.

	% code_aux__goal_is_flat(Goal) is true if Goal does not contain
	% any branched structures (ie if-then-else or disjunctions or
	% switches.)
:- pred code_aux__goal_is_flat(hlds_goal).
:- mode code_aux__goal_is_flat(in) is semidet.

	% code_aux__contains_simple_recursive_call(G, CI, Last) succeeds
	% if G is a conjunction of goals, exactly one of which is a recursive
	% call (CI says what the current procedure is), and there are no
	% other goals that cause control to leave this procedure. Last is
	% set dependening on whether the recursive call is last in the
	% conjunction or not.

	% XXX should avoid the dependency on code_info here
:- pred code_aux__contains_simple_recursive_call(hlds_goal, code_info, bool).
:- mode code_aux__contains_simple_recursive_call(in, in, out) is semidet.

:- pred code_aux__explain_stack_slots(stack_slots, prog_varset, string).
:- mode code_aux__explain_stack_slots(in, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_pred, ll_backend__llds, ll_backend__llds_out.
:- import_module varset, check_hlds__type_util, transform_hlds__term_util.
:- import_module string, set, std_util, assoc_list, require.
:- import_module list, map.

code_aux__contains_only_builtins(Goal - _GoalInfo) :-
	code_aux__contains_only_builtins_2(Goal).

:- pred code_aux__contains_only_builtins_2(hlds_goal_expr).
:- mode code_aux__contains_only_builtins_2(in) is semidet.

code_aux__contains_only_builtins_2(conj(Goals)) :-
	code_aux__contains_only_builtins_list(Goals).
code_aux__contains_only_builtins_2(disj(Goals, _)) :-
	code_aux__contains_only_builtins_list(Goals).
code_aux__contains_only_builtins_2(switch(_Var, _Category, Cases, _)) :-
	code_aux__contains_only_builtins_cases(Cases).
code_aux__contains_only_builtins_2(not(Goal)) :-
	code_aux__contains_only_builtins(Goal).
code_aux__contains_only_builtins_2(some(_Vars, _, Goal)) :-
	code_aux__contains_only_builtins(Goal).
code_aux__contains_only_builtins_2(if_then_else(_Vars, Cond, Then, Else, _)) :-
	code_aux__contains_only_builtins(Cond),
	code_aux__contains_only_builtins(Then),
	code_aux__contains_only_builtins(Else).
code_aux__contains_only_builtins_2(call(_, _, _, BuiltinState, _, _)) :-
	BuiltinState = inline_builtin.
code_aux__contains_only_builtins_2(unify(_, _, _, Uni, _)) :-
	(
		Uni = assign(_, _)
	;
		Uni = simple_test(_, _)
	;
		Uni = construct(_, _, _, _, _, _, _)
	;
		Uni = deconstruct(_, _, _, _, _, _)
	).
		% Complicated unifies are _non_builtin_

:- pred code_aux__contains_only_builtins_cases(list(case)).
:- mode code_aux__contains_only_builtins_cases(in) is semidet.

code_aux__contains_only_builtins_cases([]).
code_aux__contains_only_builtins_cases([case(_ConsId, Goal)|Cases]) :-
	code_aux__contains_only_builtins(Goal),
	code_aux__contains_only_builtins_cases(Cases).

:- pred code_aux__contains_only_builtins_list(list(hlds_goal)).
:- mode code_aux__contains_only_builtins_list(in) is semidet.

code_aux__contains_only_builtins_list([]).
code_aux__contains_only_builtins_list([Goal|Goals]) :-
	code_aux__contains_only_builtins(Goal),
	code_aux__contains_only_builtins_list(Goals).

%-----------------------------------------------------------------------------%

code_aux__goal_can_loop(ModuleInfo, Goal) :-
	\+ code_aux__goal_cannot_loop(ModuleInfo, Goal).

code_aux__goal_can_loop_or_throw(Goal) :-
	\+ code_aux__goal_cannot_loop_or_throw(Goal).

code_aux__goal_cannot_loop(ModuleInfo, Goal) :-
	code_aux__goal_cannot_loop_aux(yes(ModuleInfo), Goal).

code_aux__goal_cannot_loop_or_throw(Goal) :-
	code_aux__goal_cannot_loop_aux(no, Goal).

:- pred code_aux__goal_cannot_loop_aux(maybe(module_info), hlds_goal).
:- mode code_aux__goal_cannot_loop_aux(in, in) is semidet.

code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Goal) :-
	Goal = GoalExpr - _,
	code_aux__goal_cannot_loop_expr(MaybeModuleInfo, GoalExpr).

:- pred code_aux__goal_cannot_loop_expr(maybe(module_info), hlds_goal_expr).
:- mode code_aux__goal_cannot_loop_expr(in, in) is semidet.

code_aux__goal_cannot_loop_expr(MaybeModuleInfo, conj(Goals)) :-
	list__member(Goal, Goals) =>
		code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Goal).
code_aux__goal_cannot_loop_expr(MaybeModuleInfo, disj(Goals, _)) :-
	list__member(Goal, Goals) =>
		code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Goal).
code_aux__goal_cannot_loop_expr(MaybeModuleInfo,
		switch(_Var, _Category, Cases, _)) :-
	list__member(Case, Cases) =>
		(
		Case = case(_, Goal),
		code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Goal)
		).
code_aux__goal_cannot_loop_expr(MaybeModuleInfo, not(Goal)) :-
	code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Goal).
code_aux__goal_cannot_loop_expr(MaybeModuleInfo, some(_Vars, _, Goal)) :-
	code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Goal).
code_aux__goal_cannot_loop_expr(MaybeModuleInfo,
		if_then_else(_Vars, Cond, Then, Else, _)) :-
	code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Cond),
	code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Then),
	code_aux__goal_cannot_loop_aux(MaybeModuleInfo, Else).
code_aux__goal_cannot_loop_expr(MaybeModuleInfo,
		call(PredId, ProcId, _, _, _, _)) :-
	MaybeModuleInfo = yes(ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, MaybeTermInfo),
	MaybeTermInfo = yes(cannot_loop).
code_aux__goal_cannot_loop_expr(_, unify(_, _, _, Uni, _)) :-
	(
		Uni = assign(_, _)
	;
		Uni = simple_test(_, _)
	;
		Uni = construct(_, _, _, _, _, _, _)
	;
		Uni = deconstruct(_, _, _, _, _, _)
	).
		% Complicated unifies are _non_builtin_
	
%-----------------------------------------------------------------------------%

code_aux__goal_is_flat(Goal - _GoalInfo) :-
	code_aux__goal_is_flat_2(Goal).

:- pred code_aux__goal_is_flat_2(hlds_goal_expr).
:- mode code_aux__goal_is_flat_2(in) is semidet.

code_aux__goal_is_flat_2(conj(Goals)) :-
	code_aux__goal_is_flat_list(Goals).
code_aux__goal_is_flat_2(not(Goal)) :-
	code_aux__goal_is_flat(Goal).
code_aux__goal_is_flat_2(some(_Vars, _, Goal)) :-
	code_aux__goal_is_flat(Goal).
code_aux__goal_is_flat_2(generic_call(_, _, _, _)).
code_aux__goal_is_flat_2(call(_, _, _, _, _, _)).
code_aux__goal_is_flat_2(unify(_, _, _, _, _)).
code_aux__goal_is_flat_2(foreign_proc(_, _, _, _, _, _, _)).

%-----------------------------------------------------------------------------%

:- pred code_aux__goal_is_flat_list(list(hlds_goal)).
:- mode code_aux__goal_is_flat_list(in) is semidet.

code_aux__goal_is_flat_list([]).
code_aux__goal_is_flat_list([Goal|Goals]) :-
	code_aux__goal_is_flat(Goal),
	code_aux__goal_is_flat_list(Goals).

%-----------------------------------------------------------------------------%

code_aux__contains_simple_recursive_call(Goal - _, CodeInfo, Last) :-
	Goal = conj(Goals),
	code_aux__contains_simple_recursive_call_2(Goals, CodeInfo, Last).

:- pred code_aux__contains_simple_recursive_call_2(list(hlds_goal), code_info,
	bool).
:- mode code_aux__contains_simple_recursive_call_2(in, in, out) is semidet.

code_aux__contains_simple_recursive_call_2([Goal|Goals], CodeInfo, Last) :-
	Goal = GoalExpr - _,
	(
		code_aux__contains_only_builtins_2(GoalExpr)
	->
		code_aux__contains_simple_recursive_call_2(Goals, CodeInfo,
			Last)
	;
		code_aux__is_recursive_call(GoalExpr, CodeInfo),
		( Goals = [] ->
			Last = yes
		;
			code_aux__contains_only_builtins_list(Goals),
			Last = no
		)
	).

:- pred code_aux__is_recursive_call(hlds_goal_expr, code_info).
:- mode code_aux__is_recursive_call(in, in) is semidet.

code_aux__is_recursive_call(Goal, CodeInfo) :-
	Goal = call(CallPredId, CallProcId, _, BuiltinState, _, _),
	BuiltinState = not_builtin,
	code_info__get_pred_id(PredId, CodeInfo, _),
	PredId = CallPredId,
	code_info__get_proc_id(ProcId, CodeInfo, _),
	ProcId = CallProcId.

%-----------------------------------------------------------------------------%

code_aux__explain_stack_slots(StackSlots, VarSet, Explanation) :-
	map__to_assoc_list(StackSlots, StackSlotsList),
	code_aux__explain_stack_slots_2(StackSlotsList, VarSet, "",
		Explanation1),
	string__append("\nStack slot assignments (if any):\n", Explanation1,
		Explanation).

:- pred code_aux__explain_stack_slots_2(assoc_list(prog_var, lval), prog_varset,
		string, string).
:- mode code_aux__explain_stack_slots_2(in, in, in, out) is det.

code_aux__explain_stack_slots_2([], _, String, String).
code_aux__explain_stack_slots_2([Var - Lval | Rest], VarSet, String0, String) :-
	code_aux__explain_stack_slots_2(Rest, VarSet, String0, String1),
	( llds_out__lval_to_string(Lval, LvalString0) ->
		LvalString = LvalString0
	;
		LvalString = "some lval"
	),
	varset__lookup_name(VarSet, Var, VarName),
	string__append_list([VarName, "\t ->\t", LvalString, "\n", String1],
		String).

%---------------------------------------------------------------------------%
