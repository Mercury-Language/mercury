%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: context.m
% Main author: stayl
%
% Magic context transformation, used to transform queries for evaluation
% by the Aditi deductive database. This is an alternative to the supplementary
% magic set transformation implemented by magic.m - the two transformations
% are mutually exclusive.
%
% The transformation applies to predicates which are not mutually recursive
% and which are made up only of exit rules and linear recursive rules,
% e.g. ancestor.
%
% The special property of these predicates is that the magic predicate
% contains all the answers, so we if add some plumbing to pair the
% magic tuple with the input tuple from which it was derived, we get the
% set of answers to the query.
%
% We should do some preprocessing on the goal to attempt to put it
% into a form which is recognizable as linear. Also, some transformations
% (e.g. deforestation) can take linear rules and make them non-linear.
%
% See David Kemp's PhD thesis (available from http://www.cs.mu.oz.au/~kemp).
%-----------------------------------------------------------------------------%
:- module aditi_backend__context.

:- interface.

:- import_module aditi_backend__magic_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module list.

	% context__process_disjuncts(OrigPredProcId, Inputs, Outputs,
	% 	Disjuncts0, Disjuncts).
	%
	% OrigPredProcId is the pred_proc_id from the original program
	% before the preprocessing pass. Inputs and Outputs are the
	% original inputs and outputs of the procedure, not including
	% the input closures added in the preprocessing pass.
:- pred context__process_disjuncts(pred_proc_id::in, list(prog_var)::in,
		list(prog_var)::in, list(hlds_goal)::in, list(hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__instmap.
:- import_module mdbcomp__prim_data.

:- import_module assoc_list, bool, map, require, set, std_util, term, varset.

context__process_disjuncts(OldPredProcId, Inputs, Outputs,
		Disjuncts0, Disjuncts) -->
	list__map_foldl(
		context__categorize_rule(OldPredProcId, Inputs, Outputs),
		Disjuncts0, ContextRules),
	context__transform_rules(OldPredProcId, ContextRules,
		Inputs, Outputs, [], Disjuncts).

%-----------------------------------------------------------------------------%

:- pred context__categorize_rule(pred_proc_id::in, list(prog_var)::in,
		list(prog_var)::in, hlds_goal::in,
		pair(context_rule, hlds_goal_info)::out,
		magic_info::in, magic_info::out) is det.

context__categorize_rule(OldPredProcId, InputArgs, OutputArgs, Goal,
		ContextRule - GoalInfo) -->
	{ goal_to_conj_list(Goal, GoalList) },
	magic_info_get_module_info(ModuleInfo),
	{ Goal = _ - GoalInfo },
	{ goal_info_get_context(GoalInfo, Context) },
	magic_info_get_pred_map(PredMap),
	{ context__get_db_calls(ModuleInfo, PredMap, GoalList,
		[], [], CallList, AfterGoals) },
	{ context__categorize_call_list(Context, OldPredProcId, InputArgs,
		OutputArgs, CallList, AfterGoals, ContextRule) }.

%-----------------------------------------------------------------------------%

:- pred context__get_db_calls(module_info::in, magic_map::in,
	list(hlds_goal)::in, list(hlds_goal)::in, db_call_list::in,
	db_call_list::out, list(hlds_goal)::out) is det.

context__get_db_calls(_, _, [], RevGoals, Calls0, Calls, AfterGoals) :-
	list__reverse(Calls0, Calls),
	list__reverse(RevGoals, AfterGoals).
context__get_db_calls(ModuleInfo, MagicMap, [Goal | Goals],
		RevBeforeGoals, Calls0, Calls, AfterGoals) :-
	(
		magic_util__goal_is_aditi_call(ModuleInfo, MagicMap,
			Goal, Call, AfterCallGoals)
	->
		list__reverse(RevBeforeGoals, BeforeGoals),
		Calls1 = [BeforeGoals - Call | Calls0],
		list__reverse(AfterCallGoals, RevAfterCallGoals),
		context__get_db_calls(ModuleInfo, MagicMap, Goals,
			RevAfterCallGoals, Calls1, Calls, AfterGoals)
	;
		context__get_db_calls(ModuleInfo, MagicMap, Goals,
			[Goal | RevBeforeGoals], Calls0, Calls, AfterGoals)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type context_rule
	--->	non_linear(assoc_list(linearity_error, prog_context))
	;	exit(db_call_list, list(hlds_goal))
	;	left_linear(db_call, db_call_list, list(hlds_goal))
	;	right_linear(db_call_list, list(hlds_goal), db_call)
	;	multi_linear(db_call_list, list(hlds_goal), db_call)
	.

	% Non-recursive goals, recursive call.
:- type db_call_list == assoc_list(list(hlds_goal), db_call).

:- pred context__categorize_call_list(term__context::in, pred_proc_id::in,
		list(prog_var)::in, list(prog_var)::in, db_call_list::in,
		list(hlds_goal)::in, context_rule::out) is det.

context__categorize_call_list(Context, PredProcId, InputArgs, OutputArgs,
		Calls, AfterGoals, Result) :-
	(
		\+ (
			list__member(_ - Call, Calls),
			magic_util__db_call_pred_proc_id(Call, PredProcId)
		)
	->
		Result = exit(Calls, AfterGoals)
	;
		context__check_left_linear_rule(PredProcId, InputArgs,
			OutputArgs, Calls, AfterGoals, LeftResult),

		% If context__check_left_linear_rule came up with reasons
		% for the rule not being left-linear, it couldn't be
		% right- or multi-linear.
		( LeftResult = non_linear([]) ->
			context__check_right_or_multi_linear_rule(Context,
				PredProcId, InputArgs, OutputArgs, Calls,
				AfterGoals, Result)
		;
			Result = LeftResult
		)
	).

%-----------------------------------------------------------------------------%

:- pred context__check_left_linear_rule(pred_proc_id::in, list(prog_var)::in,
		list(prog_var)::in, db_call_list::in,
		list(hlds_goal)::in, context_rule::out) is det.

context__check_left_linear_rule(PredProcId, InputArgs,
		_OutputArgs, Calls, AfterGoals, LeftResult) :-
	(
		% Check whether the rule is left-linear.
		Calls = [[] - Call | OtherCalls],
		magic_util__db_call_pred_proc_id(Call, PredProcId),
		\+ (
			list__member(_ - OtherCall, OtherCalls),
			magic_util__db_call_pred_proc_id(OtherCall, PredProcId)
		)
	->
		magic_util__db_call_input_args(Call, CallInputs),
			% The inputs to the recursive call must be the
			% inputs to the procedure.
		( CallInputs = InputArgs ->
			Errors0 = []
		;
			magic_util__db_call_context(Call, Context),
			Errors0 = [inputs_to_recursive_call - Context]
		),

		% None of the inputs may occur in the other goals in the rule.
		set__list_to_set(InputArgs, InputSet),
		list__foldl(context__check_db_call_nonlocals(InputSet),
			OtherCalls, Errors0, Errors),

		( Errors = [] ->
			LeftResult = left_linear(Call, OtherCalls, AfterGoals)
		;
			LeftResult = non_linear(Errors)
		)
	;
		LeftResult = non_linear([])
	).

%-----------------------------------------------------------------------------%

:- pred context__check_right_or_multi_linear_rule(prog_context::in,
	pred_proc_id::in, list(prog_var)::in, list(prog_var)::in,
	db_call_list::in, list(hlds_goal)::in, context_rule::out) is det.

context__check_right_or_multi_linear_rule(RuleContext, PredProcId, InputArgs,
		OutputArgs, Calls, AfterGoals, Result) :-
	(
		AfterGoals = [],
		list__reverse(Calls, RevCalls),
		RevCalls = [NonRecGoals - Call | OtherRevCalls]
	->
		% The outputs of the last call must be the outputs of
		% the procedure.
		magic_util__db_call_output_args(Call, Outputs),
		( Outputs = OutputArgs ->
			Errors0 = []
		;
			magic_util__db_call_context(Call, Context),
			Errors0 = [outputs_of_recursive_call - Context]
		),

		list__reverse(OtherRevCalls, OtherCalls),
		(
			\+ (
				list__member(_ - OtherCall, OtherCalls),
				magic_util__db_call_pred_proc_id(OtherCall,
					PredProcId)
			)
		->
			% The rule is right linear.
			( Errors0 = [] ->
				Result = right_linear(OtherCalls,
					NonRecGoals, Call)
			;
				Result = non_linear(Errors0)
			)
		;
			context__check_multi_calls(PredProcId, InputArgs,
				OtherCalls, Errors0, Errors),

			( Errors = [] ->
				Result = multi_linear(OtherCalls,
					NonRecGoals, Call)
			;
				Result = non_linear(Errors)
			)
		)
	;
		Result = non_linear([end_goals_not_recursive - RuleContext])
	).

:- pred context__check_multi_calls(pred_proc_id::in, list(prog_var)::in,
	db_call_list::in, assoc_list(linearity_error, prog_context)::in,
	assoc_list(linearity_error, prog_context)::out) is det.

context__check_multi_calls(_, _, [], Errors, Errors).
context__check_multi_calls(PredProcId, InputArgs, [BeforeGoals - Call | Calls],
		Errors0, Errors) :-
	set__list_to_set(InputArgs, InputSet),
	( magic_util__db_call_pred_proc_id(Call, PredProcId) ->
		magic_util__db_call_input_args(Call, Inputs),
		( Inputs = InputArgs ->
			Errors1 = Errors0
		;
			magic_util__db_call_context(Call, Context),
			Errors1 = [inputs_to_recursive_call - Context
					| Errors0]
		)
	;
		magic_util__db_call_nonlocals(Call, NonLocals),
		magic_util__db_call_context(Call, Context),
		context__check_nonlocals(Context, InputSet, NonLocals,
			Errors0, Errors1)
	),
	list__foldl(context__check_goal_nonlocals(InputSet), BeforeGoals,
		Errors1, Errors2),
	context__check_multi_calls(PredProcId, InputArgs, Calls,
		Errors2, Errors).

%-----------------------------------------------------------------------------%

:- pred context__check_db_call_nonlocals(set(prog_var)::in,
		pair(list(hlds_goal), db_call)::in,
		assoc_list(linearity_error, prog_context)::in,
		assoc_list(linearity_error, prog_context)::out) is det.

context__check_db_call_nonlocals(Inputs, BeforeGoals - Call,
		Errors0, Errors) :-
	magic_util__db_call_nonlocals(Call, NonLocals),
	magic_util__db_call_context(Call, Context),
	context__check_nonlocals(Context, Inputs, NonLocals, Errors0, Errors1),
	list__foldl(context__check_goal_nonlocals(Inputs), BeforeGoals,
		Errors1, Errors).

:- pred context__check_goal_nonlocals(set(prog_var)::in, hlds_goal::in,
		assoc_list(linearity_error, prog_context)::in,
		assoc_list(linearity_error, prog_context)::out) is det.

context__check_goal_nonlocals(Inputs, _ - GoalInfo, Errors0, Errors) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	goal_info_get_context(GoalInfo, Context),
	context__check_nonlocals(Context, Inputs, NonLocals, Errors0, Errors).

:- pred context__check_nonlocals(term__context::in, set(prog_var)::in,
	set(prog_var)::in, assoc_list(linearity_error, prog_context)::in,
	assoc_list(linearity_error, prog_context)::out) is det.

context__check_nonlocals(Context, Inputs, NonLocals, Errors0, Errors) :-
	set__intersect(Inputs, NonLocals, Intersection),
	( set__empty(Intersection) ->
		Errors = Errors0
	;
		Errors = [inputs_occur_in_other_goals - Context | Errors0]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred context__transform_rules(pred_proc_id::in,
		assoc_list(context_rule, hlds_goal_info)::in,
		list(prog_var)::in, list(prog_var)::in, list(hlds_goal)::in,
		list(hlds_goal)::out, magic_info::in, magic_info::out) is det.

context__transform_rules(_, [], _, _, RevGoals, Goals) -->
	{ list__reverse(RevGoals, Goals) }.
context__transform_rules(OldPredProcId, [Rule | Rules], Inputs, Outputs,
		Disjuncts0, Disjuncts) -->
	context__transform_rule(OldPredProcId, Rule,
		Inputs, Outputs, Disjuncts0, Disjuncts1),
	context__transform_rules(OldPredProcId, Rules, Inputs, Outputs,
		Disjuncts1, Disjuncts).

:- pred context__transform_rule(pred_proc_id::in,
		pair(context_rule, hlds_goal_info)::in,
		list(prog_var)::in, list(prog_var)::in, list(hlds_goal)::in,
		list(hlds_goal)::out, magic_info::in, magic_info::out) is det.

context__transform_rule(PredProcId, exit(CallList0, AfterGoals0) - GoalInfo,
		Inputs, Outputs, Disjuncts0, [Disjunct | Disjuncts0]) -->
	context__create_magic_call(MagicCall, yes, Subn, _),
	{ context__rename_vars_in_call_list(CallList0,
		Subn, CallList) },
	{ goal_util__rename_vars_in_goals(AfterGoals0, no, Subn, AfterGoals) },
	magic_info_get_magic_vars(Vars),
	{ list__condense([Vars, Inputs, Outputs], NonLocals0) },
	{ set__list_to_set(NonLocals0, NonLocals) },
	{ list__reverse(CallList, RevCallList) },
	context__factor_goal_list(PredProcId, MagicCall, RevCallList,
		NonLocals, FactoredGoal),
	{ list__append(FactoredGoal, AfterGoals, GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Disjunct) }.

context__transform_rule(_, non_linear(Errors) - _, _, _,
		Disjuncts, Disjuncts) -->
	list__foldl(context__add_linearity_error, Errors).

context__transform_rule(PredProcId,
		left_linear(Call, CallList, AfterGoals) - GoalInfo,
		Inputs, Outputs, Disjuncts0, [Disjunct | Disjuncts0]) -->
	% For a left-linear rule, just factor the rule into
	% a form that rl_gen.m can handle.

	{ Call = db_call(_, _ - RecGoalInfo, _, Args0, _, _, _) },
	magic_info_get_magic_vars(MagicVars),
	{ list__append(MagicVars, Args0, Args) },
	magic_info_get_pred_info(PredInfo),
	{ PredModule = pred_info_module(PredInfo) },
	{ PredName = pred_info_name(PredInfo) },
	magic_info_get_curr_pred_proc_id(proc(PredId, ProcId)),
	{ CallGoal = call(PredId, ProcId, Args, not_builtin, no,
			qualified(PredModule, PredName)) - RecGoalInfo },

	magic_info_get_magic_vars(Vars),
	{ list__condense([Vars, Inputs, Outputs], NonLocals0) },
	{ set__list_to_set(NonLocals0, NonLocals) },
	{ list__reverse(CallList, RevCallList) },
	context__factor_goal_list(PredProcId, CallGoal, RevCallList,
		NonLocals, FactoredGoal),
	{ list__append(FactoredGoal, AfterGoals, GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Disjunct) }.

context__transform_rule(PredProcId, right_linear(CallList, Goals, Call) - _,
		Inputs, Outputs, Disjuncts, Disjuncts) -->
	context__create_magic_call(MagicCall, no, _, MagicInputArgs),

	{ magic_util__db_call_nonlocals(Call, CallNonLocals) },
	{ goal_list_nonlocals(Goals, GoalNonLocals) },
	magic_info_get_magic_vars(Vars),
	{ list__condense([Vars, Inputs, Outputs], NonLocals) },
	{ set__insert_list(CallNonLocals, NonLocals, NonLocals1) },
	{ set__insert_list(NonLocals1, MagicInputArgs, NonLocals2) },
	{ set__union(NonLocals2, GoalNonLocals, NonLocals3) },
	magic_util__restrict_nonlocals(NonLocals3, AllNonLocals),

	% Put the goal into a form that rl_gen.m can handle.
	context__factor_goal_list(PredProcId, MagicCall, CallList,
		AllNonLocals, FactoredGoal),

	% Add the rule to the context magic predicate.
	{ magic_util__db_call_input_args(Call, CallInputs) },
	{ list__append(MagicInputArgs, CallInputs, MagicArgs) },
	magic_info_get_curr_pred_proc_id(PredProcId1),
	{ list__append(FactoredGoal, Goals, AllGoals) },
	{ goal_list_instmap_delta(AllGoals, Delta) },
	{ goal_list_determinism(AllGoals, Det) },
	{ goal_info_init(AllNonLocals, Delta, Det, pure, MagicRuleInfo) },
	{ conj_list_to_goal(AllGoals, MagicRuleInfo, MagicGoal) },
	magic_util__add_to_magic_predicate(PredProcId1, MagicGoal, MagicArgs).

context__transform_rule(PredProcId,
		multi_linear(CallList0, Goals0, LastCall) - _GoalInfo,
		Inputs, Outputs, Disjuncts, Disjuncts) -->
	magic_info_get_magic_vars(Vars),
	{ magic_util__db_call_nonlocals(LastCall, CallNonLocals) },
	{ list__condense([Vars, Inputs, Outputs], NonLocals0) },
	{ set__insert_list(CallNonLocals, NonLocals0, NonLocals1) },
	magic_util__restrict_nonlocals(NonLocals1, NonLocals2),

	% No call to the magic predicate is necessary, because the
	% internal recursive calls produce the input.
	% Pull the first recursive call to the front of the list.
	% XXX this could introduce inefficiency because the
	% constraints on the call are not taken as well.
	{ context__get_first_recursive_call(PredProcId,
		CallList0, [], CallList, FirstRecCall0, AfterGoals) },
	{ list__append(Goals0, AfterGoals, Goals1) },

	% Update the called pred_proc_id.
	{ FirstRecCall0 = db_call(_, _ - RecGoalInfo, _, Args0, _, _, _) },
	magic_info_get_pred_info(PredInfo),
	{ PredModule = pred_info_module(PredInfo) },
	{ PredName = pred_info_name(PredInfo) },
	magic_info_get_curr_pred_proc_id(proc(PredId, ProcId)),
	magic_info_get_magic_vars(MagicVars),
	{ list__append(MagicVars, Args0, Args) },
	{ RecGoal = call(PredId, ProcId, Args, not_builtin, no,
			qualified(PredModule, PredName)) - RecGoalInfo },

	{ list__reverse(CallList, RevCallList) },
	context__factor_goal_list(PredProcId, RecGoal,
		RevCallList, NonLocals2, FactoredGoal),

	{ magic_util__db_call_input_args(FirstRecCall0, CallInputs) },
	{ magic_util__db_call_input_args(LastCall, NewInputs) },
	magic_info_get_magic_vars(InputRels),
	{ list__condense([InputRels, CallInputs, NewInputs], MagicArgs) },
	{ list__append(FactoredGoal, Goals1, AllGoals) },
	{ goal_list_instmap_delta(AllGoals, Delta) },
	{ goal_list_determinism(AllGoals, Det) },
	{ goal_info_init(NonLocals2, Delta, Det, pure, MagicRuleInfo) },
	{ conj_list_to_goal(AllGoals, MagicRuleInfo, MagicGoal) },
	magic_info_get_curr_pred_proc_id(PredProcId1),
	magic_util__add_to_magic_predicate(PredProcId1, MagicGoal, MagicArgs).

:- pred context__get_first_recursive_call(pred_proc_id::in, db_call_list::in,
		db_call_list::in, db_call_list::out, db_call::out,
		list(hlds_goal)::out) is det.

context__get_first_recursive_call(_, [], _, _, _, _) :-
	error("context__get_first_recursive_call: no recursive call").
context__get_first_recursive_call(PredProcId,
		[BeforeGoals1 - Call1 | CallList0],
		RevCallList0, CallList, RecCall, Goals) :-
	( magic_util__db_call_pred_proc_id(Call1, PredProcId) ->
		RecCall = Call1,
		( CallList0 = [BeforeGoals2 - Call2 | CallList1] ->
			list__append(BeforeGoals1, BeforeGoals2, BeforeGoals),
			CallList2 = [BeforeGoals - Call2 | CallList1],
			list__reverse(RevCallList0, CallList3),
			list__append(CallList3, CallList2, CallList),
			Goals = []
		;
			list__reverse(RevCallList0, CallList),
			Goals = BeforeGoals1
		)
	;
		context__get_first_recursive_call(PredProcId, CallList0,
			[BeforeGoals1 - Call1 | RevCallList0],
			CallList, RecCall, Goals)
	).

	%
:- pred context__factor_goal_list(pred_proc_id::in, hlds_goal::in,
		db_call_list::in, set(prog_var)::in, list(hlds_goal)::out,
		magic_info::in, magic_info::out) is det.

context__factor_goal_list(_, FirstCall, [], _, [FirstCall]) --> [].
context__factor_goal_list(PredProcId, FirstCall,
		[BeforeGoals - Call  | CallList], NonLocals, SetupCall) -->
	{ magic_util__db_call_nonlocals(Call, CallNonLocals) },
	{ goal_list_nonlocals(BeforeGoals, GoalsNonLocals) },
	{ set__union(NonLocals, CallNonLocals, NonLocals1) },
	{ set__union(NonLocals1, GoalsNonLocals, NonLocals2) },
	magic_util__restrict_nonlocals(NonLocals2, NonLocals3),
	context__factor_goal_list(PredProcId, FirstCall, CallList,
		NonLocals3, Goal1),
	{ list__append(Goal1, BeforeGoals, InputGoals) },
	( { magic_util__db_call_pred_proc_id(Call, PredProcId) } ->

		%
		% If the call is recursive, we must be processing
		% a multi-linear rule. In that case, convert the
		% inputs to outputs and create some test unifications.
		% Due to the restrictions on interior calls in multi-linear
		% rules, there can be no extra input to add to the magic
		% predicate.
		%

		{ Call = db_call(_, _ - GoalInfo0, _, Args, InputArgs, _, _) },
		magic_info_get_curr_pred_proc_id(proc(PredId, ProcId)),
		magic_info_get_magic_proc_info(MagicProcInfo),
		{ map__lookup(MagicProcInfo, proc(PredId, ProcId),
			ThisProcInfo) },
		{ ThisProcInfo = magic_proc_info(OldArgModes, _, _, _, _) },
		magic_info_get_module_info(ModuleInfo0),
		magic_info_get_proc_info(ProcInfo0),
		{ magic_util__create_input_test_unifications(ModuleInfo0,
			Args, InputArgs, OldArgModes, NewArgs, [], Tests,
			GoalInfo0, GoalInfo1, ProcInfo0, ProcInfo) },
		magic_info_set_proc_info(ProcInfo),
		{ goal_info_get_nonlocals(GoalInfo1, GoalNonLocals1) },
		magic_util__restrict_nonlocals(GoalNonLocals1, GoalNonLocals),
		{ goal_info_set_nonlocals(GoalInfo1,
			GoalNonLocals, GoalInfo) },
		magic_info_get_pred_info(PredInfo),
		{ PredModule = pred_info_module(PredInfo) },
		{ PredName = pred_info_name(PredInfo) },
		magic_info_get_magic_vars(MagicVars),
		{ list__append(MagicVars, NewArgs, AllArgs) },
		{ RecGoal = call(PredId, ProcId, AllArgs, not_builtin, no,
			qualified(PredModule, PredName)) - GoalInfo },
		{ SetupCall = [RecGoal | Tests] }
	;
		% The call is non-recursive, do the usual thing to set it up.
		magic_util__setup_call(InputGoals, Call, NonLocals, SetupCall)
	).

:- pred context__create_magic_call(hlds_goal::out, bool::in,
		map(prog_var, prog_var)::out, list(prog_var)::out,
		magic_info::in, magic_info::out) is det.

context__create_magic_call(MagicCall, RenameInputs, Subn, MagicInputArgs) -->
	magic_util__magic_call_info(MagicPredId, MagicProcId, PredName,
		InputRels, InputArgs, MagicOutputModes),

	magic_info_get_proc_info(ProcInfo0),
	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ map__apply_to_list(InputArgs, VarTypes0, InputTypes) },
	{ proc_info_create_vars_from_types(InputTypes, NewInputArgs,
		ProcInfo0, ProcInfo) },
	magic_info_set_proc_info(ProcInfo),

	( { RenameInputs = yes } ->
		{ map__from_corresponding_lists(InputArgs,
			NewInputArgs, Subn) },
		{ list__append(InputRels, InputArgs, MagicInputArgs) },
		{ list__append(MagicInputArgs, NewInputArgs, MagicArgs) },

		{ list__append(InputArgs, NewInputArgs, AllInputArgs) }
	;
		{ map__init(Subn) },
		{ list__append(InputRels, NewInputArgs, MagicInputArgs) },
		{ list__append(MagicInputArgs, InputArgs, MagicArgs) },

		{ list__append(NewInputArgs, InputArgs, AllInputArgs) }
	),

	{ set__list_to_set(MagicArgs, NonLocals) },
	{ list__append(MagicOutputModes, MagicOutputModes, AllOutputModes) },
	magic_info_get_module_info(ModuleInfo),
	{ instmap_delta_from_mode_list(AllInputArgs, AllOutputModes,
		ModuleInfo, InstMapDelta) },
	{ goal_info_init(NonLocals, InstMapDelta, nondet, pure, GoalInfo) },

	{ MagicCall = call(MagicPredId, MagicProcId, MagicArgs,
			not_builtin, no, PredName) - GoalInfo }.

%-----------------------------------------------------------------------------%

:- pred context__rename_vars_in_call_list(db_call_list::in,
		map(prog_var, prog_var)::in, db_call_list::out) is det.

context__rename_vars_in_call_list([], _, []).
context__rename_vars_in_call_list([Goals0 - Call0 | Calls0],
		Subn, [Goals - Call | Calls]) :-
	goal_util__rename_vars_in_goals(Goals0, no, Subn, Goals),
	magic_util__rename_vars_in_db_call(Call0, Subn, Call),
	context__rename_vars_in_call_list(Calls0, Subn, Calls).

%-----------------------------------------------------------------------------%

:- pred context__add_linearity_error(pair(linearity_error, prog_context)::in,
		magic_info::in, magic_info::out) is det.

context__add_linearity_error(Error - Context) -->
	magic_info_get_curr_pred_proc_id(PredProcId),
	magic_info_get_errors(Errors0),
	{ set__insert(Errors0, context_error(Error, PredProcId) - Context,
		Errors) },
	magic_info_set_errors(Errors).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
