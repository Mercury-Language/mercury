%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
% The handling of model_det and model_semi disjunctions is almost identical.
% The handling of model_non disjunctions is also quite similar.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module disj_gen.

:- interface.

:- import_module hlds_goal, llds, code_info.

:- pred disj_gen__generate_det_disj(list(hlds_goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_det_disj(in, in, out, in, out) is det.

:- pred disj_gen__generate_semi_disj(list(hlds_goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_disj(in, in, out, in, out) is det.

:- pred disj_gen__generate_non_disj(list(hlds_goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj(in, in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, code_gen, code_util, options, globals.
:- import_module bool, set, tree, list, map, std_util, require.

%---------------------------------------------------------------------------%

disj_gen__generate_det_disj(Goals, StoreMap, Code) -->
	disj_gen__generate_pruned_disj(Goals, StoreMap, Code).

disj_gen__generate_semi_disj(Goals, StoreMap, Code) -->
	( { Goals = [] } ->
		code_info__generate_failure(Code)
	;
		disj_gen__generate_pruned_disj(Goals, StoreMap, Code)
	).

%---------------------------------------------------------------------------%

:- pred disj_gen__generate_pruned_disj(list(hlds_goal), store_map,
	code_tree, code_info, code_info).
:- mode disj_gen__generate_pruned_disj(in, in, out, in, out) is det.

disj_gen__generate_pruned_disj(Goals, StoreMap, Code) -->
		% If we are using constraints, save the current solver state
		% before the first disjunct.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, reclaim_heap_on_semidet_failure,
		ReclaimHeap) },
	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode,
		MaybeTicketSlot),

		% Rather than saving the heap pointer here,
		% we delay saving it until we get to the first
		% disjunct that might allocate some heap space.
	{ MaybeHpSlot = no },

		% Generate all the disjuncts
	code_info__get_next_label(EndLabel),
	disj_gen__generate_pruned_disjuncts(Goals, StoreMap, EndLabel,
		ReclaimHeap, MaybeHpSlot, MaybeTicketSlot, no, GoalsCode),

		% Remake the code_info using the store map for the
		% variable locations at the end of the disjunction.
	code_info__remake_with_store_map(StoreMap),

	{ Code = tree(SaveTicketCode, GoalsCode) }.

%---------------------------------------------------------------------------%

:- pred disj_gen__generate_pruned_disjuncts(list(hlds_goal), store_map,
	label, bool, maybe(lval), maybe(lval), bool, code_tree,
	code_info, code_info).
:- mode disj_gen__generate_pruned_disjuncts(in, in, in, in, in, in, in,
	out, in, out) is det.

	% To generate code for a det or semidet disjunction,
	% we generate a chain of goals if-then-else style
	% until we come to a goal without a resume point.
	% That goal is the last in the chain that we need to
	% generate code for. (This is figured out by the liveness pass.)
	%
	% For a semidet disj, this goal will be semidet,
	% and will be followed by no other goal.
	% For a det disj, this goal will be det,
	% and may be followed by other goals.
	%
	% XXX For efficiency, we ought not to restore anything in the
	% first disjunct.

disj_gen__generate_pruned_disjuncts([], _, _, _, _, _, _, _) -->
	{ error("Empty pruned disjunction!") }.
disj_gen__generate_pruned_disjuncts([Goal0 | Goals], StoreMap, EndLabel,
		ReclaimHeap, MaybeHpSlot0, MaybeTicketSlot, First, Code) -->
	{ Goal0 = GoalExpr0 - GoalInfo0 },
	{ goal_info_get_code_model(GoalInfo0, CodeModel) },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVars, ResumeLocs) }
	->
		% Emit code for a non-last disjunct, including setting things
		% up for the execution of the next disjunct.

		code_info__push_resume_point_vars(ResumeVars),
		code_info__make_known_failure_cont(ResumeVars, ResumeLocs, no,
			ModContCode),
			% The next line is to enable Goal to pass the
			% pre_goal_update sanity check
		{ goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo) },
		{ Goal = GoalExpr0 - GoalInfo },

		( { First = no } ->
				% Reset the heap pointer to recover memory
				% allocated by the previous disjunct(s),
				% if necessary
			code_info__maybe_restore_hp(MaybeHpSlot0,
				RestoreHPCode),

				% Reset the solver state if necessary
			code_info__maybe_restore_ticket(MaybeTicketSlot,
				RestoreTicketCode)
		;
			{ RestoreHPCode = empty },
			{ RestoreTicketCode = empty }
		),

			% Save hp if it needs to be saved and hasn't been
			% saved previously
		(
			{ ReclaimHeap = yes },
			{ code_util__goal_may_allocate_heap(Goal) },
			{ MaybeHpSlot0 = no }
		->
			code_info__save_hp(SaveHPCode, HpSlot),
			{ MaybeHpSlot = yes(HpSlot) }
		;
			{ SaveHPCode = empty },
			{ MaybeHpSlot = MaybeHpSlot0 }
		),

		code_info__grab_code_info(CodeInfo),

			% generate the disjunct as a semi-deterministic goal
		{ CodeModel = model_semi ->
			true
		;
			error("pruned disj non-last goal is not semidet")
		},
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),

		{ BranchCode = node([
			goto(label(EndLabel)) -
				"skip to end of pruned disj"
		]) },

		code_info__slap_code_info(CodeInfo),
		code_info__pop_resume_point_vars,
		code_info__restore_failure_cont(RestoreContCode),

		disj_gen__generate_pruned_disjuncts(Goals, StoreMap, EndLabel,
			ReclaimHeap, MaybeHpSlot, MaybeTicketSlot, no,
			RestCode),

		{ Code = tree(ModContCode, 
			 tree(RestoreHPCode,
			 tree(SaveHPCode,
			 tree(RestoreTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			 tree(BranchCode,
			 tree(RestoreContCode,
			      RestCode)))))))) }
	;
		% Emit code for the last disjunct

			% Restore the heap pointer if necessary
		code_info__maybe_restore_and_discard_hp(MaybeHpSlot0,
			RestoreHPCode),

			% Restore the solver state if necessary
		code_info__maybe_restore_and_discard_ticket(MaybeTicketSlot, 
			RestorePopTicketCode),

			% Generate the goal
		code_gen__generate_goal(CodeModel, Goal0, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),

		{ EndCode = node([
			label(EndLabel) - "End of pruned disj"
		]) },
		{ Code = tree(RestoreHPCode,
			 tree(RestorePopTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			      EndCode)))) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_non_disj(Goals, StoreMap, Code) -->

		% Sanity check
	{
		Goals = [],
		error("empty disjunction shouldn't be nondet")
	;
		Goals = [_],
		error("singleton disjunction")
	;
		Goals = [_, _ | _]
	},

		% If we are using constraints, save the current solver state
		% before the first disjunct.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode,
		MaybeTicketSlot),

		% With nondet disjunctions, we must recover memory across
		% all disjuncts, since we can backtract to disjunct N
		% even after control leaves disjunct N-1.
	{ globals__lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
		ReclaimHeap) },
	code_info__maybe_save_hp(ReclaimHeap, SaveHeapCode, MaybeHpSlot),

	code_info__get_next_label(EndLabel),
	disj_gen__generate_non_disjuncts(Goals, StoreMap, EndLabel,
		MaybeHpSlot, MaybeTicketSlot, no, GoalsCode),

		% since we don't know which disjunct we have come from
		% we must set the current failure continuation to unknown.

	code_info__unset_failure_cont(FlushResumeVarsCode),
	code_info__remake_with_store_map(StoreMap),
	{ Code = tree(SaveTicketCode,
		 tree(SaveHeapCode,
		 tree(GoalsCode,
		      FlushResumeVarsCode))) }.

%---------------------------------------------------------------------------%

	% XXX For efficiency, we ought not to restore anything in the
	% first disjunct.

:- pred disj_gen__generate_non_disjuncts(list(hlds_goal), store_map, label,
	maybe(lval), maybe(lval), bool, code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disjuncts(in, in, in, in, in, in,
	out, in, out) is det.

disj_gen__generate_non_disjuncts([], _, _, _, _, _, _) -->
	{ error("empty nondet disjunction!") }.
disj_gen__generate_non_disjuncts([Goal0 | Goals], StoreMap, EndLabel,
		MaybeHpSlot, MaybeTicketSlot, First, Code) -->

	{ Goal0 = GoalExpr0 - GoalInfo0 },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVars, ResumeLocs) }
	->
		% Emit code for a non-last disjunct, including setting things
		% up for the execution of the next disjunct.

		code_info__push_resume_point_vars(ResumeVars),
		code_info__make_known_failure_cont(ResumeVars, ResumeLocs, yes,
			ModContCode),
			% The next line is to enable Goal to pass the
			% pre_goal_update sanity check
		{ goal_info_set_resume_point(GoalInfo0, no_resume_point,
			GoalInfo) },
		{ Goal = GoalExpr0 - GoalInfo },

		( { First = no } ->
				% Reset the heap pointer to recover memory
				% allocated by the previous disjunct(s),
				% if necessary
			code_info__maybe_restore_hp(MaybeHpSlot,
				RestoreHPCode),

				% Reset the solver state if necessary
			code_info__maybe_restore_ticket(MaybeTicketSlot,
				RestoreTicketCode)
		;
			{ RestoreHPCode = empty },
			{ RestoreTicketCode = empty }
		),

		code_info__grab_code_info(CodeInfo),

		code_gen__generate_goal(model_non, Goal, GoalCode),
		code_info__generate_branch_end(model_non, StoreMap, SaveCode),

			% make sure every variable in the resume set is in its
			% stack slot
		code_info__flush_resume_vars_to_stack(FlushResumeVarsCode),

		{ BranchCode = node([
			goto(label(EndLabel)) -
				"skip to end of nondet disj"
		]) },

		code_info__slap_code_info(CodeInfo),
		code_info__pop_resume_point_vars,

			% make sure that the redoip of the top nondet frame
			% points to the right label, and set up the start of
			% the next disjunct
		code_info__restore_failure_cont(RestoreContCode),

		disj_gen__generate_non_disjuncts(Goals, StoreMap, EndLabel,
			MaybeHpSlot, MaybeTicketSlot, no, RestCode),

		{ Code = tree(ModContCode, 
			 tree(RestoreHPCode,
			 tree(RestoreTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			 tree(FlushResumeVarsCode,
			 tree(BranchCode,
			 tree(RestoreContCode,
			      RestCode)))))))) }
	;
		% Emit code for the last disjunct

		{ Goals = [] ->
			true
		;
			error("disj_gen__generate_non_disjuncts: last disjunct followed by others")
		},

			% Restore the heap pointer if necessary
		code_info__maybe_restore_and_discard_hp(MaybeHpSlot,
			RestoreHPCode),

			% Restore the solver state if necessary
		code_info__maybe_restore_and_discard_ticket(MaybeTicketSlot,
			RestorePopTicketCode),

		code_gen__generate_goal(model_non, Goal0, GoalCode),
		code_info__generate_branch_end(model_non, StoreMap, SaveCode),

		{ EndCode = node([
			label(EndLabel) - "End of nondet disj"
		]) },
		{ Code = tree(RestoreHPCode,
			 tree(RestorePopTicketCode,
			 tree(GoalCode,
			 tree(SaveCode,
			      EndCode)))) }
	).

%---------------------------------------------------------------------------%
