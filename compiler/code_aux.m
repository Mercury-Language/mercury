%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- module code_aux.

:- interface.

:- import_module list, code_info, hlds, llds.
:- import_module map, varset, string.

	% code_aux__contains_only_builtins(G) is true if G is a leaf procedure,
	% i.e. control does not leave G to call another procedure, even if
	% that procedure is a complicated unification.

:- pred code_aux__contains_only_builtins(hlds__goal).
:- mode code_aux__contains_only_builtins(in) is semidet.

	% code_aux__goal_is_flat(Goal) is true if Goal does not contain
	% any branched structures (ie if-then-else or disjunctions or
	% switches.)
:- pred code_aux__goal_is_flat(hlds__goal).
:- mode code_aux__goal_is_flat(in) is semidet.

	% code_aux__contains_simple_recursive_call(G, CI, Last) succeeds
	% if G is a conjunction of goals, exactly one of which is a recursive
	% call (CI says what the current procedure is), and there are no
	% other goals that cause control to leave this procedure. Last is
	% set dependening on whether the recursive call is last in the
	% conjunction or not.

:- pred code_aux__contains_simple_recursive_call(hlds__goal, code_info, bool).
:- mode code_aux__contains_simple_recursive_call(in, in, out) is semidet.

	% code_aux__pre_goal_update(GoalInfo, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes
	% specified by GoalInfo. The components that change are:
	%	- The set of live variables has the predeath set from
	%	  GoalInfo removed (by set difference)
	%	- The set of live variable has the prebirth set from GoalInfo
	%	  added (set union)
	%	- The set of live variables has the postdeath set from GoalInfo
	%	  removed (by set difference). These variables are removed so
	%	  that they do not get saved across calls and positioned where
	%	  branched computations join unnecessarily.
	%	- The variables that die before a goal (The predeath set)
	%	  are removed from the exprn_info structure.
	%	- The variables that die but may still become live on
	%	  backtracking (and therefore still need to be stored on
	%	  the stack) are added to the set of nondet-live variables.
:- pred code_aux__pre_goal_update(hlds__goal_info, code_info, code_info).
:- mode code_aux__pre_goal_update(in, in, out) is det.

	% code_aux__post_goal_update(GoalInfo, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes described
	% by GoalInfo. These are:
	%	- The variables that died during the goal are removed from the
	%	  exprn_info structure (from the post-death set).
	%	- Variables that became live at the end of the goal (the post-
	%	  birth set) are added to the exprn_info structure and to the
	%	  set of live variables.
	%	- Variables that were nondet-live only during the current goal
	%	  are removed from the set of nondet-live variables.
:- pred code_aux__post_goal_update(hlds__goal_info, code_info, code_info).
:- mode code_aux__post_goal_update(in, in, out) is det.

:- pred code_aux__explain_call_info(map(var, lval), varset, string).
:- mode code_aux__explain_call_info(in, in, out) is det.

:- pred code_aux__lookup_type_defn(type, hlds__type_defn, code_info, code_info).
% :- mode code_aux__lookup_type_defn(in, out, di, uo) is det.
:- mode code_aux__lookup_type_defn(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module set, term, type_util, std_util, assoc_list, require.

code_aux__contains_only_builtins(Goal - _GoalInfo) :-
	code_aux__contains_only_builtins_2(Goal).

:- pred code_aux__contains_only_builtins_2(hlds__goal_expr).
:- mode code_aux__contains_only_builtins_2(in) is semidet.

code_aux__contains_only_builtins_2(conj(Goals)) :-
	code_aux__contains_only_builtins_list(Goals).
code_aux__contains_only_builtins_2(disj(Goals)) :-
	code_aux__contains_only_builtins_list(Goals).
code_aux__contains_only_builtins_2(switch(_Var, _Category, Cases)) :-
	code_aux__contains_only_builtins_cases(Cases).
code_aux__contains_only_builtins_2(not(Goal)) :-
	code_aux__contains_only_builtins(Goal).
code_aux__contains_only_builtins_2(some(_Vars, Goal)) :-
	code_aux__contains_only_builtins(Goal).
code_aux__contains_only_builtins_2(if_then_else(_Vars, Cond, Then, Else)) :-
	code_aux__contains_only_builtins(Cond),
	code_aux__contains_only_builtins(Then),
	code_aux__contains_only_builtins(Else).
code_aux__contains_only_builtins_2(call(_, _, _, Builtin, _, _, _)) :-
	hlds__is_builtin_is_inline(Builtin).
code_aux__contains_only_builtins_2(unify(_, _, _, Uni, _)) :-
	(
		Uni = assign(_, _)
	;
		Uni = simple_test(_, _)
	;
		Uni = construct(_, _, _, _)
	;
		Uni = deconstruct(_, _, _, _, _)
	).
		% Complicated unifies are _non_builtin_

:- pred code_aux__contains_only_builtins_cases(list(case)).
:- mode code_aux__contains_only_builtins_cases(in) is semidet.

code_aux__contains_only_builtins_cases([]).
code_aux__contains_only_builtins_cases([case(_ConsId, Goal)|Cases]) :-
	code_aux__contains_only_builtins(Goal),
	code_aux__contains_only_builtins_cases(Cases).

:- pred code_aux__contains_only_builtins_list(list(hlds__goal)).
:- mode code_aux__contains_only_builtins_list(in) is semidet.

code_aux__contains_only_builtins_list([]).
code_aux__contains_only_builtins_list([Goal|Goals]) :-
	code_aux__contains_only_builtins(Goal),
	code_aux__contains_only_builtins_list(Goals).

%-----------------------------------------------------------------------------%

code_aux__goal_is_flat(Goal - _GoalInfo) :-
	code_aux__goal_is_flat_2(Goal).

:- pred code_aux__goal_is_flat_2(hlds__goal_expr).
:- mode code_aux__goal_is_flat_2(in) is semidet.

code_aux__goal_is_flat_2(conj(Goals)) :-
	code_aux__goal_is_flat_list(Goals).
code_aux__goal_is_flat_2(not(Goal)) :-
	code_aux__goal_is_flat(Goal).
code_aux__goal_is_flat_2(some(_Vars, Goal)) :-
	code_aux__goal_is_flat(Goal).
code_aux__goal_is_flat_2(call(_, _, _, _, _, _, _)).
code_aux__goal_is_flat_2(unify(_, _, _, _, _)).
code_aux__goal_is_flat_2(pragma_c_code(_, _, _, _, _)).

%-----------------------------------------------------------------------------%

:- pred code_aux__goal_is_flat_list(list(hlds__goal)).
:- mode code_aux__goal_is_flat_list(in) is semidet.

code_aux__goal_is_flat_list([]).
code_aux__goal_is_flat_list([Goal|Goals]) :-
	code_aux__goal_is_flat(Goal),
	code_aux__goal_is_flat_list(Goals).

%-----------------------------------------------------------------------------%

code_aux__contains_simple_recursive_call(Goal - _, CodeInfo, Last) :-
	Goal = conj(Goals),
	code_aux__contains_simple_recursive_call_2(Goals, CodeInfo, Last).

:- pred code_aux__contains_simple_recursive_call_2(list(hlds__goal), code_info,
	bool).
:- mode code_aux__contains_simple_recursive_call_2(in, in, out) is semidet.

code_aux__contains_simple_recursive_call_2([Goal|Goals], CodeInfo, Last) :-
	Goal = GoalExpr - _,
	(
		code_aux__contains_only_builtins_2(GoalExpr)
	->
		code_aux__contains_simple_recursive_call_2(Goals, CodeInfo, Last)
	;
		code_aux__is_recursive_call(GoalExpr, CodeInfo),
		( Goals = [] ->
			Last = yes
		;
			code_aux__contains_only_builtins_list(Goals),
			Last = no
		)
	).

:- pred code_aux__is_recursive_call(hlds__goal_expr, code_info).
:- mode code_aux__is_recursive_call(in, in) is semidet.

code_aux__is_recursive_call(Goal, CodeInfo) :-
	Goal = call(CallPredId, CallProcId, _, Builtin, _, _, _),
	\+ hlds__is_builtin_is_internal(Builtin),
	code_info__get_pred_id(PredId, CodeInfo, _),
	PredId = CallPredId,
	code_info__get_proc_id(ProcId, CodeInfo, _),
	ProcId = CallProcId.

%-----------------------------------------------------------------------------%

	% Update the code info structure to be consistent
	% immediately prior to generating a goal
code_aux__pre_goal_update(GoalInfo) -->
	{ goal_info_nondet_lives(GoalInfo, NondetLives) },
	code_info__set_nondet_lives(NondetLives),
	{ goal_info_pre_delta_liveness(GoalInfo, PreDelta) },
	code_info__update_liveness_info(PreDelta),
	code_info__update_deadness_info(PreDelta),
	{ PreDelta = _ - PreDeaths },
	code_info__make_vars_dead(PreDeaths),
	{ goal_info_post_delta_liveness(GoalInfo, PostDelta) },
	code_info__update_deadness_info(PostDelta).

	% Update the code info structure to be consistent
	% immediately after generating a goal
code_aux__post_goal_update(GoalInfo) -->
	{ goal_info_post_delta_liveness(GoalInfo, PostDelta) },
	code_info__update_liveness_info(PostDelta),
	code_info__update_deadness_info(PostDelta),
	{ PostDelta = PostBirths - PostDeaths },
	code_info__make_vars_dead(PostDeaths),
	code_info__make_vars_live(PostBirths),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	code_info__apply_instmap_delta(InstMapDelta).

%-----------------------------------------------------------------------------%

code_aux__explain_call_info(CallInfo, VarSet, Explanation) :-
	map__to_assoc_list(CallInfo, CallInfoList),
	code_aux__explain_call_info_2(CallInfoList, VarSet, "", Explanation1),
	string__append("\nStack slot assignments (if any):\n", Explanation1,
		Explanation).

:- pred code_aux__explain_call_info_2(assoc_list(var, lval), varset, string,
				string).
:- mode code_aux__explain_call_info_2(in, in, in, out) is det.

code_aux__explain_call_info_2([], _, String, String).
code_aux__explain_call_info_2([Var - Lval | Rest], VarSet, String0, String) :-
	code_aux__explain_call_info_2(Rest, VarSet, String0, String1),
	( llds__lval_to_string(Lval, LvalString0) ->
		LvalString = LvalString0
	;
		LvalString = "some lval"
	),
	( varset__lookup_name(VarSet, Var, VarName) ->
		VarString = VarName
	;
		term__var_to_int(Var, VarNum),
		string__int_to_string(VarNum, VarNumString),
		string__append("variable number ", VarNumString, VarString)
	),
	string__append_list([VarString, "\t ->\t", LvalString, "\n", String1],
		String).

%---------------------------------------------------------------------------%

code_aux__lookup_type_defn(Type, TypeDefn) -->
	code_info__get_module_info(ModuleInfo),
	{ type_to_type_id(Type, TypeIdPrime, _) ->
		TypeId = TypeIdPrime
	;
		error("unknown type in code_aux__lookup_type_defn")
	},
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) }.

%---------------------------------------------------------------------------%
