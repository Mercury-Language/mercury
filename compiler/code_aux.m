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

:- import_module code_info, hlds_goal, hlds_data, llds.
:- import_module list, map, varset.

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

	% code_aux__pre_goal_update(GoalInfo, Atomic, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes
	% specified by GoalInfo. The components that change are:
	%	- The set of live variables has the predeath set from
	%	  GoalInfo removed (by set difference)
	%	- The set of live variable has the prebirth set from GoalInfo
	%	  added (set union)
	%	- If (and only if) `Atomic' is yes, then
	%	  the set of live variables has the postdeath set from GoalInfo
	%	  removed (by set difference). These variables are removed so
	%	  that they do not get saved across calls and positioned where
	%	  branched computations join unnecessarily.
	%	- The variables that die before a goal (The predeath set)
	%	  are removed from the exprn_info structure.
	%	- If the goal establishes a resume_point, its variables are
	%	  pushed onto the resume point variable stack.
	%	- The advisory information about where variables will be
	%	  needed next is updated if GoalInfo has new information.
	% If any of the variables that have died wrt forward execution are
	% nevertheless needed at a resume point, we need to flush them to
	% their stack slots. The returned code does this.
:- pred code_aux__pre_goal_update(hlds__goal_info, bool, code_info, code_info).
:- mode code_aux__pre_goal_update(in, in, in, out) is det.

	% code_aux__post_goal_update(GoalInfo, OldCodeInfo, NewCodeInfo)
	% updates OldCodeInfo to produce NewCodeInfo with the changes described
	% by GoalInfo. These are:
	%	- The set of live variables has the postdeath set from GoalInfo
	%	  removed (by set difference).
	%	- The variables that died during the goal are removed from the
	%	  exprn_info structure (from the post-death set).
	%	- Variables that became live at the end of the goal (the post-
	%	  birth set) are added to the exprn_info structure and to the
	%	  set of live variables.
	%	- The instmap delta is applied to the current instmap.
	%	- If the goal established a resume_point, its variables are
	%	  popped off the resume point variable stack.
	% If any of the variables that have died wrt forward execution are
	% nevertheless needed at a resume point, we need to flush them to
	% their stack slots. The returned code does this.
:- pred code_aux__post_goal_update(hlds__goal_info, code_info, code_info).
:- mode code_aux__post_goal_update(in, in, out) is det.

:- pred code_aux__explain_stack_slots(stack_slots, varset, string).
:- mode code_aux__explain_stack_slots(in, in, out) is det.

:- pred code_aux__lookup_type_defn(type, hlds__type_defn, code_info, code_info).
% :- mode code_aux__lookup_type_defn(in, out, di, uo) is det.
:- mode code_aux__lookup_type_defn(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, llds_out, type_util.
:- import_module bool, string, set, term, std_util, assoc_list, require.

code_aux__contains_only_builtins(Goal - _GoalInfo) :-
	code_aux__contains_only_builtins_2(Goal).

:- pred code_aux__contains_only_builtins_2(hlds__goal_expr).
:- mode code_aux__contains_only_builtins_2(in) is semidet.

code_aux__contains_only_builtins_2(conj(Goals)) :-
	code_aux__contains_only_builtins_list(Goals).
code_aux__contains_only_builtins_2(disj(Goals, _)) :-
	code_aux__contains_only_builtins_list(Goals).
code_aux__contains_only_builtins_2(switch(_Var, _Category, Cases, _)) :-
	code_aux__contains_only_builtins_cases(Cases).
code_aux__contains_only_builtins_2(not(Goal)) :-
	code_aux__contains_only_builtins(Goal).
code_aux__contains_only_builtins_2(some(_Vars, Goal)) :-
	code_aux__contains_only_builtins(Goal).
code_aux__contains_only_builtins_2(if_then_else(_Vars, Cond, Then, Else, _)) :-
	code_aux__contains_only_builtins(Cond),
	code_aux__contains_only_builtins(Then),
	code_aux__contains_only_builtins(Else).
code_aux__contains_only_builtins_2(call(_, _, _, Builtin, _, _)) :-
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
code_aux__goal_is_flat_2(higher_order_call(_, _, _, _, _)).
code_aux__goal_is_flat_2(call(_, _, _, _, _, _)).
code_aux__goal_is_flat_2(unify(_, _, _, _, _)).
code_aux__goal_is_flat_2(pragma_c_code(_, _, _, _, _, _)).

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

:- pred code_aux__is_recursive_call(hlds__goal_expr, code_info).
:- mode code_aux__is_recursive_call(in, in) is semidet.

code_aux__is_recursive_call(Goal, CodeInfo) :-
	Goal = call(CallPredId, CallProcId, _, Builtin, _, _),
	\+ hlds__is_builtin_is_internal(Builtin),
	code_info__get_pred_id(PredId, CodeInfo, _),
	PredId = CallPredId,
	code_info__get_proc_id(ProcId, CodeInfo, _),
	ProcId = CallProcId.

%-----------------------------------------------------------------------------%

	% Update the code info structure to be consistent
	% immediately prior to generating a goal
code_aux__pre_goal_update(GoalInfo, Atomic) -->
	{ goal_info_get_resume_point(GoalInfo, ResumePoint) },
	(
		{ ResumePoint = no_resume_point }
	;
		{ ResumePoint = resume_point(_, _) },
		{ error("pre_goal_update with resume point") }
	),
	{ goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) },
	(
		{ MaybeFollowVars = yes(FollowVars) },
		code_info__set_follow_vars(FollowVars)
	;
		{ MaybeFollowVars = no }
	),
	{ goal_info_get_pre_births(GoalInfo, PreBirths) },
	{ goal_info_get_pre_deaths(GoalInfo, PreDeaths) },
	code_info__update_liveness_info(PreBirths),
	code_info__update_deadness_info(PreDeaths),
	code_info__make_vars_dead(PreDeaths),
	( { Atomic = yes } ->
		{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
		code_info__update_deadness_info(PostDeaths)
	;
		[]
	).

	% Update the code info structure to be consistent
	% immediately after generating a goal
code_aux__post_goal_update(GoalInfo) -->
	{ goal_info_get_post_births(GoalInfo, PostBirths) },
	{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
	code_info__update_liveness_info(PostBirths),
	code_info__update_deadness_info(PostDeaths),
	code_info__make_vars_dead(PostDeaths),
	code_info__make_vars_live(PostBirths),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	code_info__apply_instmap_delta(InstMapDelta).

%-----------------------------------------------------------------------------%

code_aux__explain_stack_slots(StackSlots, VarSet, Explanation) :-
	map__to_assoc_list(StackSlots, StackSlotsList),
	code_aux__explain_stack_slots_2(StackSlotsList, VarSet, "",
		Explanation1),
	string__append("\nStack slot assignments (if any):\n", Explanation1,
		Explanation).

:- pred code_aux__explain_stack_slots_2(assoc_list(var, lval), varset, string,
				string).
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
