%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% A module that provides functions that check whether goals fulfill
% particular criteria
%
% Main authors: conway, zs.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds__goal_form.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_goal.
:- import_module list.

	% Succeeds if the goal cannot loop forever.
:- pred goal_cannot_loop(module_info, hlds_goal).
:- mode goal_cannot_loop(in, in) is semidet.

	% Succeeds if the goal cannot loop forever or throw an exception.
:- pred goal_cannot_loop_or_throw(hlds_goal).
:- mode goal_cannot_loop_or_throw(in) is semidet.

	% Succeeds if the goal can loop forever.
:- pred goal_can_loop(module_info, hlds_goal).
:- mode goal_can_loop(in, in) is semidet.

	% Succeeds if the goal can loop forever or throw an exception.
:- pred goal_can_loop_or_throw(hlds_goal).
:- mode goal_can_loop_or_throw(in) is semidet.

	% contains_only_builtins(G) is true if G is a leaf procedure,
	% i.e. control does not leave G to call another procedure, even if
	% that procedure is a complicated unification.

:- pred contains_only_builtins(hlds_goal).
:- mode contains_only_builtins(in) is semidet.

:- pred contains_only_builtins_expr(hlds_goal_expr).
:- mode contains_only_builtins_expr(in) is semidet.

:- pred contains_only_builtins_list(list(hlds_goal)).
:- mode contains_only_builtins_list(in) is semidet.

	% goal_is_flat(Goal) is true if Goal does not contain
	% any branched structures (ie if-then-else or disjunctions or
	% switches.)
:- pred goal_is_flat(hlds_goal).
:- mode goal_is_flat(in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_pred.
:- import_module transform_hlds__term_util.
:- import_module std_util.

%-----------------------------------------------------------------------------%

goal_can_loop(ModuleInfo, Goal) :-
	\+ goal_cannot_loop(ModuleInfo, Goal).

goal_can_loop_or_throw(Goal) :-
	\+ goal_cannot_loop_or_throw(Goal).

goal_cannot_loop(ModuleInfo, Goal) :-
	goal_cannot_loop_aux(yes(ModuleInfo), Goal).

goal_cannot_loop_or_throw(Goal) :-
	goal_cannot_loop_aux(no, Goal).

:- pred goal_cannot_loop_aux(maybe(module_info), hlds_goal).
:- mode goal_cannot_loop_aux(in, in) is semidet.

goal_cannot_loop_aux(MaybeModuleInfo, Goal) :-
	Goal = GoalExpr - _,
	goal_cannot_loop_expr(MaybeModuleInfo, GoalExpr).

:- pred goal_cannot_loop_expr(maybe(module_info), hlds_goal_expr).
:- mode goal_cannot_loop_expr(in, in) is semidet.

goal_cannot_loop_expr(MaybeModuleInfo, conj(Goals)) :-
	list__member(Goal, Goals) =>
		goal_cannot_loop_aux(MaybeModuleInfo, Goal).
goal_cannot_loop_expr(MaybeModuleInfo, disj(Goals)) :-
	list__member(Goal, Goals) =>
		goal_cannot_loop_aux(MaybeModuleInfo, Goal).
goal_cannot_loop_expr(MaybeModuleInfo, switch(_Var, _Category, Cases)) :-
	list__member(Case, Cases) =>
		(
		Case = case(_, Goal),
		goal_cannot_loop_aux(MaybeModuleInfo, Goal)
		).
goal_cannot_loop_expr(MaybeModuleInfo, not(Goal)) :-
	goal_cannot_loop_aux(MaybeModuleInfo, Goal).
goal_cannot_loop_expr(MaybeModuleInfo, some(_Vars, _, Goal)) :-
	goal_cannot_loop_aux(MaybeModuleInfo, Goal).
goal_cannot_loop_expr(MaybeModuleInfo,
		if_then_else(_Vars, Cond, Then, Else)) :-
	goal_cannot_loop_aux(MaybeModuleInfo, Cond),
	goal_cannot_loop_aux(MaybeModuleInfo, Then),
	goal_cannot_loop_aux(MaybeModuleInfo, Else).
goal_cannot_loop_expr(MaybeModuleInfo,
		call(PredId, ProcId, _, _, _, _)) :-
	MaybeModuleInfo = yes(ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, MaybeTermInfo),
	MaybeTermInfo = yes(cannot_loop).
goal_cannot_loop_expr(_, unify(_, _, _, Uni, _)) :-
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

contains_only_builtins(Goal - _GoalInfo) :-
	contains_only_builtins_expr(Goal).

contains_only_builtins_expr(conj(Goals)) :-
	contains_only_builtins_list(Goals).
contains_only_builtins_expr(disj(Goals)) :-
	contains_only_builtins_list(Goals).
contains_only_builtins_expr(switch(_Var, _Category, Cases)) :-
	contains_only_builtins_cases(Cases).
contains_only_builtins_expr(not(Goal)) :-
	contains_only_builtins(Goal).
contains_only_builtins_expr(some(_Vars, _, Goal)) :-
	contains_only_builtins(Goal).
contains_only_builtins_expr(if_then_else(_Vars, Cond, Then, Else)) :-
	contains_only_builtins(Cond),
	contains_only_builtins(Then),
	contains_only_builtins(Else).
contains_only_builtins_expr(call(_, _, _, BuiltinState, _, _)) :-
	BuiltinState = inline_builtin.
contains_only_builtins_expr(unify(_, _, _, Uni, _)) :-
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

:- pred contains_only_builtins_cases(list(case)).
:- mode contains_only_builtins_cases(in) is semidet.

contains_only_builtins_cases([]).
contains_only_builtins_cases([case(_ConsId, Goal)|Cases]) :-
	contains_only_builtins(Goal),
	contains_only_builtins_cases(Cases).

contains_only_builtins_list([]).
contains_only_builtins_list([Goal|Goals]) :-
	contains_only_builtins(Goal),
	contains_only_builtins_list(Goals).

%-----------------------------------------------------------------------------%

goal_is_flat(Goal - _GoalInfo) :-
	goal_is_flat_expr(Goal).

:- pred goal_is_flat_expr(hlds_goal_expr).
:- mode goal_is_flat_expr(in) is semidet.

goal_is_flat_expr(conj(Goals)) :-
	goal_is_flat_list(Goals).
goal_is_flat_expr(not(Goal)) :-
	goal_is_flat(Goal).
goal_is_flat_expr(some(_Vars, _, Goal)) :-
	goal_is_flat(Goal).
goal_is_flat_expr(generic_call(_, _, _, _)).
goal_is_flat_expr(call(_, _, _, _, _, _)).
goal_is_flat_expr(unify(_, _, _, _, _)).
goal_is_flat_expr(foreign_proc(_, _, _, _, _, _, _)).

:- pred goal_is_flat_list(list(hlds_goal)).
:- mode goal_is_flat_list(in) is semidet.

goal_is_flat_list([]).
goal_is_flat_list([Goal|Goals]) :-
	goal_is_flat(Goal),
	goal_is_flat_list(Goals).

%-----------------------------------------------------------------------------%
