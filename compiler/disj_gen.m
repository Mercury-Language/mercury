%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: disj_gen.m:
%
% Main authors: conway, zs.
%
% The predicates of this module generate code for disjunctions.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ll_backend__disj_gen.

:- interface.

:- import_module hlds__hlds_goal, backend_libs__code_model, ll_backend__llds.
:- import_module ll_backend__code_info.
:- import_module list.

:- pred disj_gen__generate_disj(code_model::in, list(hlds_goal)::in,
	hlds_goal_info::in, code_tree::out, code_info::in, code_info::out)
	is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_data, hlds__hlds_llds.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__code_util, ll_backend__trace.
:- import_module libs__options, libs__globals, libs__tree.

:- import_module bool, set, libs__tree, map, std_util, term, require.

disj_gen__generate_disj(CodeModel, Goals, DisjGoalInfo, Code) -->
	(
		{ Goals = [] },
		( { CodeModel = model_semi } ->
			code_info__generate_failure(Code)
		;
			{ error("empty disjunction") }
		)
	;
		{ Goals = [Goal | _] },
		{ Goal = _ - GoalInfo },
		{ goal_info_get_resume_point(GoalInfo, Resume) },
		{ Resume = resume_point(ResumeVarsPrime, _) ->
			ResumeVars = ResumeVarsPrime
		;
			set__init(ResumeVars)
		},
		disj_gen__generate_real_disj(CodeModel, ResumeVars,
			Goals, DisjGoalInfo, Code)
	).

%---------------------------------------------------------------------------%

:- pred disj_gen__generate_real_disj(code_model::in, set(prog_var)::in,
	list(hlds_goal)::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is det.

disj_gen__generate_real_disj(CodeModel, ResumeVars, Goals, DisjGoalInfo, Code)
		-->

		% Make sure that the variables whose values will be needed
		% on backtracking to any disjunct are materialized into
		% registers or stack slots. Their locations are recorded
		% in ResumeMap.
	code_info__produce_vars(ResumeVars, ResumeMap, FlushCode),

		% If we are using a trail, save the current trail state
		% before the first disjunct.
		% XXX We should use a scheme such as the one we use for heap
		% recovery for semi and det disjunctions, and delay saving
		% the ticket until necessary.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, use_trail, UseTrail) },
	code_info__maybe_save_ticket(UseTrail, SaveTicketCode,
		MaybeTicketSlot),

		% If we are using a grade in which we can recover memory
		% by saving and restoring the heap pointer, set up for
		% doing so if necessary.
	( { CodeModel = model_non } ->
			% With nondet disjunctions, we must recover memory
			% across all disjuncts, even disjuncts that cannot
			% themselves allocate memory, since we can backtrack
			% to disjunct N after control leaves disjunct N-1.
		{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_nondet_failure, ReclaimHeap) },
		code_info__maybe_save_hp(ReclaimHeap, SaveHpCode,
			MaybeHpSlot)
	;
			% With other disjunctions, we can backtrack to
			% disjunct N only from disjunct N-1, so if disjunct
			% N-1 does not allocate memory, we need not recover
			% memory across it. Since it is possible (and common)
			% for no disjunct to allocate memory, we delay saving
			% the heap pointer and allocating a stack slot for
			% the saved hp as long as possible.
		{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, ReclaimHeap) },
		{ SaveHpCode = empty },
		{ MaybeHpSlot = no }
	),

		% Save the values of any stack slots we may hijack,
		% and if necessary, set the redofr slot of the top frame
		% to point to this frame.
	code_info__prepare_for_disj_hijack(CodeModel,
		HijackInfo, PrepareHijackCode),

	code_info__get_next_label(EndLabel),

	code_info__remember_position(BranchStart),
	disj_gen__generate_disjuncts(Goals, CodeModel, ResumeMap, no,
		HijackInfo, DisjGoalInfo, EndLabel,
		ReclaimHeap, MaybeHpSlot, MaybeTicketSlot,
		BranchStart, no, MaybeEnd, GoalsCode),

	{ goal_info_get_store_map(DisjGoalInfo, StoreMap) },
	code_info__after_all_branches(StoreMap, MaybeEnd),
	( { CodeModel = model_non } ->
		code_info__set_resume_point_to_unknown
	;
		[]
	),
	{ Code =
		tree(FlushCode,
		tree(SaveTicketCode,
		tree(SaveHpCode,
		tree(PrepareHijackCode,
		     GoalsCode))))
	}.

%---------------------------------------------------------------------------%

:- pred disj_gen__generate_disjuncts(list(hlds_goal)::in,
	code_model::in, resume_map::in, maybe(resume_point_info)::in,
	disj_hijack_info::in, hlds_goal_info::in, label::in,
	bool::in, maybe(lval)::in, maybe(lval)::in, position_info::in,
	maybe(branch_end_info)::in, maybe(branch_end_info)::out,
	code_tree::out, code_info::in, code_info::out) is det.

disj_gen__generate_disjuncts([], _, _, _, _, _, _, _, _, _, _, _, _, _) -->
	{ error("empty disjunction!") }.
disj_gen__generate_disjuncts([Goal0 | Goals], CodeModel, FullResumeMap,
		MaybeEntryResumePoint, HijackInfo, DisjGoalInfo, EndLabel,
		ReclaimHeap, MaybeHpSlot0, MaybeTicketSlot,
		BranchStart0, MaybeEnd0, MaybeEnd, Code) -->

	code_info__reset_to_position(BranchStart0),

		% If this is not the first disjunct, generate the
		% resume point by which arrive at this disjunct.
	( { MaybeEntryResumePoint = yes(EntryResumePoint) } ->
		code_info__generate_resume_point(EntryResumePoint,
			EntryResumePointCode)
	;
		{ EntryResumePointCode = empty }
	),

	{ Goal0 = GoalExpr0 - GoalInfo0 },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVars, ResumeLocs) }
	->
		% Emit code for a non-last disjunct, including setting things
		% up for the execution of the next disjunct.

		( { MaybeEntryResumePoint = yes(_) } ->
				% Reset the heap pointer to recover memory
				% allocated by the previous disjunct(s),
				% if necessary.
			code_info__maybe_restore_hp(MaybeHpSlot0,
				RestoreHpCode),

				% Reset the solver state if necessary.
			code_info__maybe_reset_ticket(MaybeTicketSlot, undo,
				RestoreTicketCode)
		;
			{ RestoreHpCode = empty },
			{ RestoreTicketCode = empty }
		),

			% The pre_goal_update sanity check insists on
			% no_resume_point, to make sure that all resume
			% points have been handled by surrounding code.
		{ goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo) },
		{ Goal = GoalExpr0 - GoalInfo },

			% Save hp if it needs to be saved and hasn't been
			% saved previously.
		(
			{ ReclaimHeap = yes },
			{ code_util__goal_may_allocate_heap(Goal) },
			{ MaybeHpSlot0 = no }
		->
			code_info__save_hp(SaveHpCode, HpSlot),
			{ MaybeHpSlot = yes(HpSlot) },
				% This method of updating BranchStart0 is
				% ugly. The best alternative would be to
				% arrange things so that a remember_position
				% here could get BranchStart, but doing so
				% is iffy because we have already created
				% the resumption point for entry into this
				% disjunction, which overwrites part of the
				% location-dependent state originally in
				% BranchStart0.
			{ code_info__save_hp_in_branch(BranchSaveHpCode,
				BranchHpSlot, BranchStart0, BranchStart) },
			{ tree__flatten(SaveHpCode, HpCodeList) },
			{ tree__flatten(BranchSaveHpCode, BranchHpCodeList) },
			{ require(unify(HpCodeList, BranchHpCodeList),
				"cannot use same code for saving hp") },
			{ require(unify(HpSlot, BranchHpSlot),
				"cannot allocate same slot for saved hp") }
		;
			{ SaveHpCode = empty },
			{ MaybeHpSlot = MaybeHpSlot0 },
			{ BranchStart = BranchStart0 }
		),

		code_info__make_resume_point(ResumeVars, ResumeLocs,
			FullResumeMap, NextResumePoint),
		code_info__effect_resume_point(NextResumePoint, CodeModel,
			ModContCode),

		trace__maybe_generate_internal_event_code(Goal, TraceCode),
		{ goal_info_get_code_model(GoalInfo, GoalCodeModel) },
		code_gen__generate_goal(GoalCodeModel, Goal, GoalCode),

		( { CodeModel = model_non } ->
			% We can backtrack to the next disjunct from outside,
			% so we make sure every variable in the resume set
			% is in its stack slot.
			code_info__flush_resume_vars_to_stack(ResumeVarsCode),

			% We hang onto any temporary slots holding saved
			% heap pointers and/or tickets, thus ensuring that
			% they will still be reserved after the disjunction.
			{ PruneTicketCode = empty }
		;
			{ ResumeVarsCode = empty },

			code_info__maybe_release_hp(MaybeHpSlot),
			% We're committing to this disjunct if it succeeds.
			code_info__maybe_reset_prune_and_release_ticket(
				MaybeTicketSlot, commit, PruneTicketCode),

			code_info__reset_resume_known(BranchStart)
		),

			% Forget the variables that are needed only at the
			% resumption point at the start of the next disjunct,
			% so that we don't generate exceptions when their
			% storage is clobbered by the movement of the live
			% variables to the places indicated in the store map.
		code_info__pop_resume_point,
		code_info__pickup_zombies(Zombies),
		code_info__make_vars_forward_dead(Zombies),

			% Put every variable whose value is needed after
			% the disjunction to the place indicated by StoreMap,
			% and accumulate information about the code_info state
			% at the ends of the branches so far.
		{ goal_info_get_store_map(DisjGoalInfo, StoreMap) },
		code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd1,
			SaveCode),

		{ BranchCode = node([
			goto(label(EndLabel)) -
				"skip to end of nondet disj"
		]) },

		disj_gen__generate_disjuncts(Goals, CodeModel, FullResumeMap,
			yes(NextResumePoint), HijackInfo, DisjGoalInfo,
			EndLabel, ReclaimHeap, MaybeHpSlot, MaybeTicketSlot,
			BranchStart, MaybeEnd1, MaybeEnd, RestCode),

		{ Code =
			tree(EntryResumePointCode, 
			tree(RestoreHpCode,
			tree(RestoreTicketCode,
			tree(SaveHpCode,
			tree(ModContCode, 
			tree(TraceCode,
			tree(GoalCode,
			tree(ResumeVarsCode,
			tree(PruneTicketCode,
			tree(SaveCode,
			tree(BranchCode,
			     RestCode)))))))))))
		}
	;
		% Emit code for the last disjunct

			% Restore the heap pointer and solver state
			% if necessary.
		code_info__maybe_restore_and_release_hp(MaybeHpSlot0,
			RestoreHpCode),
		code_info__maybe_reset_discard_and_release_ticket(
			MaybeTicketSlot, undo, RestoreTicketCode),

		code_info__undo_disj_hijack(HijackInfo, UndoCode),

		trace__maybe_generate_internal_event_code(Goal0, TraceCode),
		code_gen__generate_goal(CodeModel, Goal0, GoalCode),
		{ goal_info_get_store_map(DisjGoalInfo, StoreMap) },
		code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
			SaveCode),

		{ EndCode = node([
			label(EndLabel) - "End of nondet disj"
		]) },
		{ Code =
			tree(EntryResumePointCode,
			tree(TraceCode,
			tree(RestoreHpCode,
			tree(RestoreTicketCode,
			tree(UndoCode,
			tree(GoalCode,
			tree(SaveCode,
			     EndCode)))))))
		}
	).

%---------------------------------------------------------------------------%
