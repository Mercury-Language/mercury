%---------------------------------------------------------------------------%
% Copyright (C) 1994-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ite_gen.m
%
% Main authors: conway, fjh, zs.
%
% The predicates of this module generate code for if-then-elses, and for
% negations (which are cut-down versions of if-then-elses, since not(G)
% is equivalent to (G -> fail ; true)).
%
%---------------------------------------------------------------------------%

:- module ll_backend__ite_gen.

:- interface.

:- import_module hlds__code_model.
:- import_module hlds__hlds_goal.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.

:- pred ite_gen__generate_ite(code_model::in, hlds_goal::in, hlds_goal::in,
	hlds_goal::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred ite_gen__generate_negation(code_model::in, hlds_goal::in,
	hlds_goal_info::in, code_tree::out, code_info::in, code_info::out)
	is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module hlds__goal_form.
:- import_module hlds__hlds_llds.
:- import_module hlds__instmap.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__tree.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__trace.
:- import_module parse_tree__prog_data.

:- import_module bool, string, set, term, list, map, std_util, require.

ite_gen__generate_ite(CodeModel, CondGoal0, ThenGoal, ElseGoal, IteGoalInfo,
		Code, !CI) :-
	CondGoal0 = CondExpr - CondInfo0,
	goal_info_get_code_model(CondInfo0, CondCodeModel),
	(
		CodeModel = model_non,
		CondCodeModel \= model_non
	->
		EffCodeModel = model_semi
	;
		EffCodeModel = CodeModel
	),

	goal_info_get_resume_point(CondInfo0, Resume),
	(
		Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime)
	->
		ResumeVars = ResumeVarsPrime,
		ResumeLocs = ResumeLocsPrime,
			% The pre_goal_update sanity check insists on
			% no_resume_point, to make sure that all resume
			% points have been handled by surrounding code.
		goal_info_set_resume_point(CondInfo0, no_resume_point,
			CondInfo),
		CondGoal = CondExpr - CondInfo
	;
		error("condition of an if-then-else has no resume point")
	),

		% Make sure that the variables whose values will be needed
		% on backtracking to the else part are materialized into
		% registers or stack slots. Their locations are recorded
		% in ResumeMap.
	code_info__produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

		% Maybe save the heap state current before the condition.
		% This is after code_info__produce_vars since code that
		% flushes the cache may allocate memory we must not "recover".
	code_info__get_globals(!.CI, Globals),
	(
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		goal_may_allocate_heap(CondGoal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	),
	code_info__maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),

		% Maybe save the current trail state before the condition
	globals__lookup_bool_option(Globals, use_trail, UseTrail),
	code_info__maybe_save_ticket(UseTrail, SaveTicketCode,
		MaybeTicketSlot, !CI),

	code_info__remember_position(!.CI, BranchStart),

	code_info__prepare_for_ite_hijack(EffCodeModel, HijackInfo,
		PrepareHijackCode, !CI),

	code_info__make_resume_point(ResumeVars, ResumeLocs, ResumeMap,
		ResumePoint, !CI),
	code_info__effect_resume_point(ResumePoint, EffCodeModel,
		EffectResumeCode, !CI),

		% Generate the condition
	trace__maybe_generate_internal_event_code(CondGoal, IteGoalInfo,
		CondTraceCode, !CI),
	code_gen__generate_goal(CondCodeModel, CondGoal, CondCode, !CI),

	code_info__ite_enter_then(HijackInfo, ThenNeckCode, ElseNeckCode, !CI),

		% Kill again any variables that have become zombies
	code_info__pickup_zombies(Zombies, !CI),
	code_info__make_vars_forward_dead(Zombies, !CI),

		% Discard hp and prune trail ticket if the condition succeeded
	( CondCodeModel = model_non ->
		% We cannot release the stack slots used for the heap pointer
		% and the trail ticket if the condition can be backtracked
		% into.  Nor can we prune the trail ticket that we allocated,
		% since the condition may have allocated other trail tickets
		% since then which have not yet been pruned.
		code_info__maybe_reset_ticket(
			MaybeTicketSlot, solve, ResetTicketCode)
	;
		code_info__maybe_release_hp(MaybeHpSlot, !CI),
		code_info__maybe_reset_prune_and_release_ticket(
			MaybeTicketSlot, commit, ResetTicketCode, !CI)
	),

	goal_info_get_store_map(IteGoalInfo, StoreMap),
	code_info__get_instmap(!.CI, EndCondInstMap),
	( instmap__is_unreachable(EndCondInstMap) ->
		% If the instmap indicates we cannot reach the then part,
		% do not attempt to generate it (may cause aborts).
		ThenTraceCode = empty,
		ThenCode = empty,
		map__init(EmptyStoreMap),
		code_info__generate_branch_end(EmptyStoreMap, no,
			MaybeEnd0, ThenSaveCode, !CI)
	;
			% Generate the then branch
		trace__maybe_generate_internal_event_code(ThenGoal,
			IteGoalInfo, ThenTraceCode, !CI),
		code_gen__generate_goal(CodeModel, ThenGoal, ThenCode, !CI),
		code_info__generate_branch_end(StoreMap, no,
			MaybeEnd0, ThenSaveCode, !CI)
	),

		% Generate the entry to the else branch
	code_info__reset_to_position(BranchStart, !CI),
	code_info__generate_resume_point(ResumePoint, ResumeCode, !CI),

		% Restore the heap pointer and solver state if necessary.
	code_info__maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode,
		!CI),
	code_info__maybe_reset_discard_and_release_ticket(
		MaybeTicketSlot, undo, RestoreTicketCode, !CI),

		% Generate the else branch
	trace__maybe_generate_internal_event_code(ElseGoal, IteGoalInfo,
		ElseTraceCode, !CI),
	code_gen__generate_goal(CodeModel, ElseGoal, ElseCode, !CI),
	code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
		ElseSaveCode, !CI),

	code_info__get_next_label(EndLabel, !CI),
	JumpToEndCode = node([
		goto(label(EndLabel))
			- "Jump to the end of if-then-else"
	]),
	EndLabelCode = node([
		label(EndLabel)
			- "end of if-then-else"
	]),
	make_pneg_context_wrappers(Globals, PNegCondCode, PNegThenCode,
		PNegElseCode),
	Code =
		tree(FlushCode,
		tree(SaveHpCode,
		tree(SaveTicketCode,
		tree(PrepareHijackCode,
		tree(EffectResumeCode,
		tree(CondTraceCode,
		tree(PNegCondCode,
		tree(CondCode,
		tree(ThenNeckCode,
		tree(ResetTicketCode,
		tree(ThenTraceCode,
		tree(PNegThenCode,
		tree(ThenCode,
		tree(ThenSaveCode,
		tree(JumpToEndCode,
		tree(ResumeCode,
		tree(ElseNeckCode,
		tree(RestoreHpCode,
		tree(RestoreTicketCode,
		tree(ElseTraceCode,
		tree(PNegElseCode,
		tree(ElseCode,
		tree(ElseSaveCode,
		     EndLabelCode))))))))))))))))))))))),
	code_info__after_all_branches(StoreMap, MaybeEnd, !CI).

%---------------------------------------------------------------------------%

ite_gen__generate_negation(CodeModel, Goal0, NotGoalInfo, Code, !CI) :-
	( CodeModel = model_non ->
		error("nondet negation")
	;
		true
	),

	Goal0 = GoalExpr - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, Resume),
	( Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime) ->
		ResumeVars = ResumeVarsPrime,
		ResumeLocs = ResumeLocsPrime,
		goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo),
		Goal = GoalExpr - GoalInfo
	;
		error("negated goal has no resume point")
	),

		% For a negated simple test, we can generate better code
		% than the general mechanism, because we don't have to
		% flush the cache.
	(
		CodeModel = model_semi,
		GoalExpr = unify(_, _, _, simple_test(L, R), _),
		code_info__failure_is_direct_branch(!.CI, CodeAddr),
		code_info__get_globals(!.CI, Globals),
		globals__lookup_bool_option(Globals, simple_neg, yes)
	->
			% Because we are generating the negated goal ourselves,
			% we need to apply the pre- and post-goal updates
			% that would normally be applied by
			% code_gen__generate_goal.

		code_info__enter_simple_neg(ResumeVars, GoalInfo, SimpleNeg,
			!CI),
		code_info__produce_variable(L, CodeL, ValL, !CI),
		code_info__produce_variable(R, CodeR, ValR, !CI),
		Type = code_info__variable_type(!.CI, L),
		( Type = term__functor(term__atom("string"), [], _) ->
			Op = str_eq
		; Type = term__functor(term__atom("float"), [], _) ->
			Op = float_eq
		;
			Op = eq
		),
		TestCode = node([
			if_val(binop(Op, ValL, ValR), CodeAddr) -
				"test inequality"
		]),
		code_info__leave_simple_neg(GoalInfo, SimpleNeg, !CI),
		Code = tree(tree(CodeL, CodeR), TestCode)
	;
		generate_negation_general(CodeModel, Goal, NotGoalInfo,
			ResumeVars, ResumeLocs, Code, !CI)
	).

	% The code of generate_negation_general is a cut-down version
	% of the code for if-then-elses.

:- pred generate_negation_general(code_model::in, hlds_goal::in,
	hlds_goal_info::in, set(prog_var)::in, resume_locs::in, code_tree::out,
	code_info::in, code_info::out) is det.

generate_negation_general(CodeModel, Goal, NotGoalInfo, ResumeVars, ResumeLocs,
		Code, !CI) :-

	code_info__produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

		% Maybe save the heap state current before the condition;
		% this ought to be after we make the failure continuation
		% because that causes the cache to get flushed
	code_info__get_globals(!.CI, Globals),
	(
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		goal_may_allocate_heap(Goal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	),
	code_info__maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),

	globals__lookup_bool_option(Globals, use_trail, UseTrail),
	code_info__maybe_save_ticket(UseTrail, SaveTicketCode,
		MaybeTicketSlot, !CI),

	code_info__prepare_for_ite_hijack(CodeModel, HijackInfo,
		PrepareHijackCode, !CI),

	code_info__make_resume_point(ResumeVars, ResumeLocs, ResumeMap,
		ResumePoint, !CI),
	code_info__effect_resume_point(ResumePoint, CodeModel,
		EffectResumeCode, !CI),

		% Generate the negated goal as a semi-deterministic goal;
		% it cannot be nondet, since mode correctness requires it
		% to have no output vars.
	trace__maybe_generate_internal_event_code(Goal, NotGoalInfo,
		EnterTraceCode, !CI),
	code_gen__generate_goal(model_semi, Goal, GoalCode, !CI),

	code_info__ite_enter_then(HijackInfo, ThenNeckCode, ElseNeckCode, !CI),

		% Kill again any variables that have become zombies
	code_info__pickup_zombies(Zombies, !CI),
	code_info__make_vars_forward_dead(Zombies, !CI),

	code_info__get_forward_live_vars(!.CI, LiveVars),

	( CodeModel = model_det ->
			% the then branch will never be reached
		PruneTicketCode = empty,
		FailTraceCode = empty,
		FailCode = empty
	;
		code_info__remember_position(!.CI, AfterNegatedGoal),
		% The call to reset_ticket(..., commit) here is necessary
		% in order to properly detect floundering.
		code_info__maybe_release_hp(MaybeHpSlot, !CI),
		code_info__maybe_reset_prune_and_release_ticket(
			MaybeTicketSlot, commit, PruneTicketCode, !CI),
		trace__maybe_generate_negated_event_code(Goal, NotGoalInfo,
			neg_failure, FailTraceCode, !CI),
		code_info__generate_failure(FailCode, !CI),
			% We want liveness after not(G) to be the same as
			% after G. Information about what variables are where
			% will be set by code_info__generate_resume_point.
		code_info__reset_to_position(AfterNegatedGoal, !CI)
	),

		% Generate the entry to the else branch
	code_info__generate_resume_point(ResumePoint, ResumeCode, !CI),

	code_info__set_forward_live_vars(LiveVars, !CI),

		% Restore the heap pointer and solver state if necessary.
	code_info__maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode,
		!CI),
	code_info__maybe_reset_discard_and_release_ticket(
		MaybeTicketSlot, undo, RestoreTicketCode, !CI),
	trace__maybe_generate_negated_event_code(Goal, NotGoalInfo,
		neg_success, SuccessTraceCode, !CI),

	make_pneg_context_wrappers(Globals, PNegCondCode, PNegThenCode,
		PNegElseCode),
	Code =
		tree(FlushCode,
		tree(PrepareHijackCode,
		tree(EffectResumeCode,
		tree(SaveHpCode,
		tree(SaveTicketCode,
		tree(EnterTraceCode,
		tree(PNegCondCode,
		tree(GoalCode,
		tree(ThenNeckCode,
		tree(PruneTicketCode,
		tree(FailTraceCode,
		tree(PNegThenCode,
		tree(FailCode,
		tree(ResumeCode,
		tree(ElseNeckCode,
		tree(RestoreTicketCode,
		tree(RestoreHpCode,
		tree(SuccessTraceCode,
		     PNegElseCode)))))))))))))))))).

%---------------------------------------------------------------------------%

	% If the code in the condition depends on a consumer
	% of a generator that is not complete by the time we finish
	% executing the condition, then failure out of the condition
	% does not necessarily mean that the condition has no solution;
	% it may mean simply that the condition's solution depends on
	% a generator solution that hasn't been produced yet and thus
	% hasn't been given to the consumer yet.
	%
	% Detecting such situations requires knowing whether tabled
	% subgoals (both generators and consumers) are started inside
	% possibly negated contexts or not, which is why we wrap the
	% condition inside MR_pneg_enter_{cond,then,exit}.

:- pred make_pneg_context_wrappers(globals::in, code_tree::out, code_tree::out,
	code_tree::out) is det.

make_pneg_context_wrappers(Globals, PNegCondCode, PNegThenCode, PNegElseCode)
		:-
	globals__lookup_bool_option(Globals, use_minimal_model_stack_copy,
		UseMinimalModelStackCopy),
	(
		UseMinimalModelStackCopy = yes,

		PNegCondComponents = [
			pragma_c_raw_code(
				wrap_transient("\t\tMR_pneg_enter_cond();\n"),
				live_lvals_info(set__init))
		],
		PNegThenComponents = [
			pragma_c_raw_code(
				wrap_transient("\t\tMR_pneg_enter_then();\n"),
				live_lvals_info(set__init))
		],
		PNegElseComponents = [
			pragma_c_raw_code(
				wrap_transient("\t\tMR_pneg_enter_else();\n"),
				live_lvals_info(set__init))
		],
		PNegCondCode = node([
			pragma_c([], PNegCondComponents, will_not_call_mercury,
				no, no, no, no, yes, yes) - ""
		]),
		PNegThenCode = node([
			pragma_c([], PNegThenComponents, will_not_call_mercury,
				no, no, no, no, yes, yes) - ""
		]),
		PNegElseCode = node([
			pragma_c([], PNegElseComponents, will_not_call_mercury,
				no, no, no, no, yes, yes) - ""
		])
	;
		UseMinimalModelStackCopy = no,
		PNegCondCode = empty,
		PNegThenCode = empty,
		PNegElseCode = empty
	).

:- func wrap_transient(string) = string.

wrap_transient(Code) =
	string__append_list([
		"\t\tMR_save_transient_registers();\n",
		Code,
		"\t\tMR_restore_transient_registers();\n"]).

%---------------------------------------------------------------------------%
