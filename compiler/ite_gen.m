%---------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
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

:- import_module hlds__hlds_goal, backend_libs__code_model, ll_backend__llds.
:- import_module ll_backend__code_info.

:- pred ite_gen__generate_ite(code_model::in, hlds_goal::in, hlds_goal::in,
	hlds_goal::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred ite_gen__generate_negation(code_model::in, hlds_goal::in,
	code_tree::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data.
:- import_module hlds__instmap, hlds__hlds_llds.
:- import_module ll_backend__code_gen, ll_backend__code_util.
:- import_module ll_backend__trace.
:- import_module backend_libs__builtin_ops.
:- import_module libs__options, libs__globals, libs__tree.

:- import_module bool, set, term, list, map, std_util, require.

ite_gen__generate_ite(CodeModel, CondGoal0, ThenGoal, ElseGoal, IteGoalInfo,
		Code) -->
	{ CondGoal0 = CondExpr - CondInfo0 },
	{ goal_info_get_code_model(CondInfo0, CondCodeModel) },
	{
		CodeModel = model_non,
		CondCodeModel \= model_non
	->
		EffCodeModel = model_semi
	;
		EffCodeModel = CodeModel
	},

	{ goal_info_get_resume_point(CondInfo0, Resume) },
	{
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
	},

		% Make sure that the variables whose values will be needed
		% on backtracking to the else part are materialized into
		% registers or stack slots. Their locations are recorded
		% in ResumeMap.
	code_info__produce_vars(ResumeVars, ResumeMap, FlushCode),

		% Maybe save the heap state current before the condition.
		% This is after code_info__produce_vars since code that
		% flushes the cache may allocate memory we must not "recover".
	code_info__get_globals(Globals),
	{ 
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(CondGoal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	},
	code_info__maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot),

		% Maybe save the current trail state before the condition
	{ globals__lookup_bool_option(Globals, use_trail, UseTrail) },
	code_info__maybe_save_ticket(UseTrail, SaveTicketCode,
		MaybeTicketSlot),

	code_info__remember_position(BranchStart),

	code_info__prepare_for_ite_hijack(EffCodeModel, HijackInfo,
		PrepareHijackCode),

	code_info__make_resume_point(ResumeVars, ResumeLocs, ResumeMap,
		ResumePoint),
	code_info__effect_resume_point(ResumePoint, EffCodeModel,
		EffectResumeCode),

		% Generate the condition
	trace__maybe_generate_internal_event_code(CondGoal, CondTraceCode),
	code_gen__generate_goal(CondCodeModel, CondGoal, CondCode),

	code_info__ite_enter_then(HijackInfo, ThenNeckCode, ElseNeckCode),

		% Kill again any variables that have become zombies
	code_info__pickup_zombies(Zombies),
	code_info__make_vars_forward_dead(Zombies),

		% Discard hp and prune trail ticket if the condition succeeded
	( { CondCodeModel = model_non } ->
		% We cannot release the stack slots used for the heap pointer
		% and the trail ticket if the condition can be backtracked
		% into.  Nor can we prune the trail ticket that we allocated,
		% since the condition may have allocated other trail tickets
		% since then which have not yet been pruned.
		code_info__maybe_reset_ticket(
			MaybeTicketSlot, solve, ResetTicketCode)
	;
		code_info__maybe_release_hp(MaybeHpSlot),
		code_info__maybe_reset_prune_and_release_ticket(
			MaybeTicketSlot, commit, ResetTicketCode)
	),

	{ goal_info_get_store_map(IteGoalInfo, StoreMap) },
	code_info__get_instmap(EndCondInstMap),
	( { instmap__is_unreachable(EndCondInstMap) } ->
		% If the instmap indicates we cannot reach the then part,
		% do not attempt to generate it (may cause aborts).
		{ ThenTraceCode = empty },
		{ ThenCode = empty },
		{ map__init(EmptyStoreMap) },
		code_info__generate_branch_end(EmptyStoreMap, no,
			MaybeEnd0, ThenSaveCode)
	;	
			% Generate the then branch
		trace__maybe_generate_internal_event_code(ThenGoal,
			ThenTraceCode),
		code_gen__generate_goal(CodeModel, ThenGoal, ThenCode),
		code_info__generate_branch_end(StoreMap, no,
			MaybeEnd0, ThenSaveCode)
	),

		% Generate the entry to the else branch
	code_info__reset_to_position(BranchStart),
	code_info__generate_resume_point(ResumePoint, ResumeCode),

		% Restore the heap pointer and solver state if necessary.
	code_info__maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode),
	code_info__maybe_reset_discard_and_release_ticket(
		MaybeTicketSlot, undo, RestoreTicketCode),

		% Generate the else branch
	trace__maybe_generate_internal_event_code(ElseGoal, ElseTraceCode),
	code_gen__generate_goal(CodeModel, ElseGoal, ElseCode),
	code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
		ElseSaveCode),

	code_info__get_next_label(EndLabel),
	{ JumpToEndCode = node([
		goto(label(EndLabel))
			- "Jump to the end of if-then-else"
	]) },
	{ EndLabelCode = node([
		label(EndLabel)
			- "end of if-then-else"
	]) },
	{ Code =
		tree(FlushCode,
		tree(SaveHpCode,
		tree(SaveTicketCode,
		tree(PrepareHijackCode,
		tree(EffectResumeCode,
		tree(CondTraceCode,
		tree(CondCode,
		tree(ThenNeckCode,
		tree(ResetTicketCode,
		tree(ThenTraceCode,
		tree(ThenCode,
		tree(ThenSaveCode,
		tree(JumpToEndCode,
		tree(ResumeCode,
		tree(ElseNeckCode,
		tree(RestoreHpCode,
		tree(RestoreTicketCode,
		tree(ElseTraceCode,
		tree(ElseCode,
		tree(ElseSaveCode,
		     EndLabelCode))))))))))))))))))))
	},
	code_info__after_all_branches(StoreMap, MaybeEnd).

%---------------------------------------------------------------------------%

ite_gen__generate_negation(CodeModel, Goal0, Code) -->
	{ CodeModel = model_non ->
		error("nondet negation")
	;
		true
	},

	{ Goal0 = GoalExpr - GoalInfo0 },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	{
		Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime)
	->
		ResumeVars = ResumeVarsPrime,
		ResumeLocs = ResumeLocsPrime,
		goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo),
		Goal = GoalExpr - GoalInfo
	;
		error("negated goal has no resume point")
	},

		% For a negated simple test, we can generate better code
		% than the general mechanism, because we don't have to
		% flush the cache.
	(
		{ CodeModel = model_semi },
		{ GoalExpr = unify(_, _, _, simple_test(L, R), _) },
		code_info__failure_is_direct_branch(CodeAddr),
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, simple_neg, yes) }
	->
			% Because we are generating the negated goal ourselves,
			% we need to apply the pre- and post-goal updates
			% that would normally be applied by
			% code_gen__generate_goal.

		code_info__enter_simple_neg(ResumeVars, GoalInfo, SimpleNeg),
		code_info__produce_variable(L, CodeL, ValL),
		code_info__produce_variable(R, CodeR, ValR),
		code_info__variable_type(L, Type),
		{ Type = term__functor(term__atom("string"), [], _) ->
			Op = str_eq
		; Type = term__functor(term__atom("float"), [], _) ->
			Op = float_eq
		;
			Op = eq
		},
		{ TestCode = node([
			if_val(binop(Op, ValL, ValR), CodeAddr) -
				"test inequality"
		]) },
		code_info__leave_simple_neg(GoalInfo, SimpleNeg),
		{ Code = tree(tree(CodeL, CodeR), TestCode) }
	;
		generate_negation_general(CodeModel, Goal,
			ResumeVars, ResumeLocs, Code)
	).

	% The code of generate_negation_general is a cut-down version
	% of the code for if-then-elses.

:- pred generate_negation_general(code_model::in, hlds_goal::in,
	set(prog_var)::in, resume_locs::in, code_tree::out,
	code_info::in, code_info::out) is det.

generate_negation_general(CodeModel, Goal, ResumeVars, ResumeLocs, Code) -->

	code_info__produce_vars(ResumeVars, ResumeMap, FlushCode),

		% Maybe save the heap state current before the condition;
		% this ought to be after we make the failure continuation
		% because that causes the cache to get flushed
	code_info__get_globals(Globals),
	{
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(Goal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	},
	code_info__maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot),

	{ globals__lookup_bool_option(Globals, use_trail, UseTrail) },
	code_info__maybe_save_ticket(UseTrail, SaveTicketCode,
		MaybeTicketSlot),

	code_info__prepare_for_ite_hijack(CodeModel, HijackInfo,
		PrepareHijackCode),

	code_info__make_resume_point(ResumeVars, ResumeLocs, ResumeMap,
		ResumePoint),
	code_info__effect_resume_point(ResumePoint, CodeModel,
		EffectResumeCode),

		% Generate the negated goal as a semi-deterministic goal;
		% it cannot be nondet, since mode correctness requires it
		% to have no output vars.
	trace__maybe_generate_internal_event_code(Goal, EnterTraceCode),
	code_gen__generate_goal(model_semi, Goal, GoalCode),

	code_info__ite_enter_then(HijackInfo, ThenNeckCode, ElseNeckCode),

		% Kill again any variables that have become zombies
	code_info__pickup_zombies(Zombies),
	code_info__make_vars_forward_dead(Zombies),

	code_info__get_forward_live_vars(LiveVars),

	( { CodeModel = model_det } ->
			% the then branch will never be reached
		{ PruneTicketCode = empty },
		{ FailTraceCode = empty },
		{ FailCode = empty }
	;
		code_info__remember_position(AfterNegatedGoal),
		% The call to reset_ticket(..., commit) here is necessary
		% in order to properly detect floundering.
		code_info__maybe_release_hp(MaybeHpSlot),
		code_info__maybe_reset_prune_and_release_ticket(
			MaybeTicketSlot, commit, PruneTicketCode),
		trace__maybe_generate_negated_event_code(Goal, neg_failure,
			FailTraceCode),
		code_info__generate_failure(FailCode),
			% We want liveness after not(G) to be the same as
			% after G. Information about what variables are where
			% will be set by code_info__generate_resume_point.
		code_info__reset_to_position(AfterNegatedGoal)
	),

		% Generate the entry to the else branch
	code_info__generate_resume_point(ResumePoint, ResumeCode),

	code_info__set_forward_live_vars(LiveVars),

		% Restore the heap pointer and solver state if necessary.
	code_info__maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode),
	code_info__maybe_reset_discard_and_release_ticket(
		MaybeTicketSlot, undo, RestoreTicketCode),
	trace__maybe_generate_negated_event_code(Goal, neg_success,
		SuccessTraceCode),

	{ Code =
		tree(FlushCode,
		tree(PrepareHijackCode,
		tree(EffectResumeCode,
		tree(SaveHpCode,
		tree(SaveTicketCode,
		tree(EnterTraceCode,
		tree(GoalCode,
		tree(ThenNeckCode,
		tree(PruneTicketCode,
		tree(FailTraceCode,
		tree(FailCode,
		tree(ResumeCode,
		tree(ElseNeckCode,
		tree(RestoreTicketCode,
		tree(RestoreHpCode,
		     SuccessTraceCode)))))))))))))))
	}.

%---------------------------------------------------------------------------%
