%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% File: ite_gen.m
%
% Main authors: conway, fjh, zs.
%
% The predicates of this module generate code for if-then-elses.
%
% The handling of model_det and model_semi if-then-elses is almost identical.
% The handling of model_non if-then-elses is also quite similar.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ite_gen.

:- interface.

:- import_module hlds_goal, llds, code_info.

:- pred ite_gen__generate_det_ite(hlds_goal, hlds_goal, hlds_goal,
	store_map, code_tree, code_info, code_info).
:- mode ite_gen__generate_det_ite(in, in, in, in, out, in, out) is det.

:- pred ite_gen__generate_semidet_ite(hlds_goal, hlds_goal, hlds_goal,
	store_map, code_tree, code_info, code_info).
:- mode ite_gen__generate_semidet_ite(in, in, in, in, out, in, out) is det.

:- pred ite_gen__generate_nondet_ite(hlds_goal, hlds_goal, hlds_goal,
	store_map, code_tree, code_info, code_info).
:- mode ite_gen__generate_nondet_ite(in, in, in, in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module code_gen, code_util, options, globals.
:- import_module bool, set, tree, list, map, std_util, require.

ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, StoreMap, Code) -->
	ite_gen__generate_basic_ite(CondGoal, ThenGoal, ElseGoal, StoreMap,
		model_det, Code).

ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, StoreMap, Code) -->
	ite_gen__generate_basic_ite(CondGoal, ThenGoal, ElseGoal, StoreMap,
		model_semi, Code).

%---------------------------------------------------------------------------%

:- pred ite_gen__generate_basic_ite(hlds_goal, hlds_goal, hlds_goal,
	store_map, code_model, code_tree, code_info, code_info).
:- mode ite_gen__generate_basic_ite(in, in, in, in, in, out, in, out) is det.

ite_gen__generate_basic_ite(CondGoal0, ThenGoal, ElseGoal, StoreMap, CodeModel,
		Code) -->

		% Set up for the possible failure of the condition
	{ CondGoal0 = CondExpr - CondInfo0 },
	{ goal_info_get_resume_point(CondInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime) }
	->
		{ ResumeVars = ResumeVarsPrime},
		{ ResumeLocs = ResumeLocsPrime}
	;
		{ error("condition of an if-then-else has no resume point") }
	),
	code_info__make_known_failure_cont(ResumeVars, ResumeLocs, no,
		ModContCode),
		% The next line is to enable Cond to pass the
		% pre_goal_update sanity check
	{ goal_info_set_resume_point(CondInfo0, no_resume_point, CondInfo) },
	{ CondGoal = CondExpr - CondInfo },

		% Maybe save the heap state current before the condition;
		% this ought to be after we make the failure continuation
		% because that causes the cache to get flushed
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
	code_info__maybe_save_hp(ReclaimHeap, SaveHPCode, MaybeHpSlot),

		% Maybe save the solver state current before the condition
	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode,
		MaybeTicketSlot),

	code_info__grab_code_info(CodeInfo),

		% Generate the condition as a semi-deterministic goal
	code_info__push_resume_point_vars(ResumeVars),
	code_gen__generate_goal(model_semi, CondGoal, CondCode),
	code_info__pop_resume_point_vars,

		% Kill again any variables that have become zombies
	code_info__pickup_zombies(Zombies),
	code_info__make_vars_forward_dead(Zombies),

	code_info__pop_failure_cont,

		% Discard hp and solver ticket if the condition succeeded
	code_info__maybe_discard_ticket(MaybeTicketSlot, DiscardTicketCode),
	code_info__maybe_discard_hp(MaybeHpSlot),

		% Generate the then branch
	code_gen__generate_goal(CodeModel, ThenGoal, ThenCode),
	code_info__generate_branch_end(CodeModel, StoreMap, ThenSaveCode),

		% Generate the entry to the else branch
	code_info__slap_code_info(CodeInfo),
	code_info__restore_failure_cont(RestoreContCode),
	code_info__maybe_restore_and_discard_ticket(MaybeTicketSlot,
		RestoreTicketCode),
	code_info__maybe_restore_and_discard_hp(MaybeHpSlot, RestoreHPCode),

		% Generate the else branch
	code_gen__generate_goal(CodeModel, ElseGoal, ElseCode),
	code_info__generate_branch_end(CodeModel, StoreMap, ElseSaveCode),

	code_info__get_next_label(EndLab),
	{ JumpToEndCode = node([goto(label(EndLab))
		- "Jump to the end of if-then-else"]) },
	{ EndLabelCode = node([label(EndLab) - "end of if-then-else"]) },
	{ Code = tree(ModContCode,
		 tree(SaveHPCode,
		 tree(SaveTicketCode,
		 tree(CondCode,
		 tree(DiscardTicketCode,
		 tree(ThenCode,
		 tree(ThenSaveCode,
		 tree(JumpToEndCode,
		 tree(RestoreContCode,
		 tree(RestoreHPCode,
		 tree(RestoreTicketCode,
		 tree(ElseCode,
		 tree(ElseSaveCode,
		      EndLabelCode)))))))))))))
	},
	code_info__remake_with_store_map(StoreMap).

%---------------------------------------------------------------------------%

ite_gen__generate_nondet_ite(CondGoal0, ThenGoal, ElseGoal, StoreMap, Code) -->

		% Set up for the possible failure of the condition
	{ CondGoal0 = CondExpr - CondInfo0 },
	{ goal_info_get_code_model(CondInfo0, CondCodeModel) },
	( { CondCodeModel = model_non } ->
		{ NondetCond = yes }
	;
		{ NondetCond = no }
	),
	{ goal_info_get_resume_point(CondInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime) }
	->
		{ ResumeVars = ResumeVarsPrime},
		{ ResumeLocs = ResumeLocsPrime}
	;
		{ error("condition of an if-then-else has no resume point") }
	),
	code_info__make_known_failure_cont(ResumeVars, ResumeLocs, NondetCond,
		ModContCode),
		% The next line is to enable Cond to pass the
		% pre_goal_update sanity check
	{ goal_info_set_resume_point(CondInfo0, no_resume_point, CondInfo) },
	{ CondGoal = CondExpr - CondInfo },

		% Prevent a nondet condition from hijacking the redoip slot
		% We could improve the efficiency of this
	( { NondetCond = yes } ->
		code_info__unset_failure_cont(FlushEnclosingResumeVarsCode),
		code_info__save_maxfr(MaxfrLval0, SaveMaxfrCode),
		{ MaybeMaxfrLval = yes(MaxfrLval0) }
	;
		{ FlushEnclosingResumeVarsCode = empty },
		{ SaveMaxfrCode = empty },
		{ MaybeMaxfrLval = no }
	),

		% Maybe save the heap state current before the condition;
		% this ought to be after we make the failure continuation
		% because that causes the cache to get flushed
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
	code_info__maybe_save_hp(ReclaimHeap, SaveHPCode, MaybeHpSlot),

		% Maybe save the current solver state before the condition
	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode,
		MaybeTicketSlot),

	code_info__grab_code_info(CodeInfo),

		% Generate the condition as either a semi-deterministic
		% or as a non-deterministic goal (the failure continuation
		% must be set up the same way)
	code_info__push_resume_point_vars(ResumeVars),
	code_gen__generate_goal(CondCodeModel, CondGoal, CondCode),
	code_info__pop_resume_point_vars,

	code_info__pop_failure_cont,
	( { MaybeMaxfrLval = yes(MaxfrLval) } ->
		code_info__do_soft_cut(MaxfrLval, SoftCutCode),
		code_info__unset_failure_cont(FlushCode)
			% XXX why call unset_failure_cont here?
			% We're going to call it from branch_end at the
			% end of the `then' anyway, so is this
			% one really necessary?
	;
		{ SoftCutCode = empty },
		{ FlushCode = empty }
	),

		% Kill again any variables that have become zombies
	code_info__pickup_zombies(Zombies),
	code_info__make_vars_forward_dead(Zombies),

		% Discard hp and maybe solver ticket if the condition succeeded
	code_info__maybe_discard_hp(MaybeHpSlot),
	( { NondetCond = yes } ->
			% We cannot discard the solver ticket if the 
			% condition can be backtracked into.
		% code_info__maybe_pop_stack(MaybeTicketSlot, DiscardTicketCode)
		{ DiscardTicketCode = empty }
	;
			% Discard the solver ticket if the condition succeeded
			% and we will not backtrack into the condition
		code_info__maybe_discard_ticket(MaybeTicketSlot,
			DiscardTicketCode)
	),

		% Generate the then branch
	code_gen__generate_goal(model_non, ThenGoal, ThenCode),
	code_info__generate_branch_end(model_non, StoreMap, ThenSaveCode),

		% Generate the entry to the else branch
	code_info__slap_code_info(CodeInfo),
	code_info__restore_failure_cont(RestoreContCode),
	code_info__maybe_restore_and_discard_hp(MaybeHpSlot, RestoreHPCode),
	code_info__maybe_restore_and_discard_ticket(MaybeTicketSlot,
		RestoreTicketCode),

		% Generate the else branch
	code_gen__generate_goal(model_non, ElseGoal, ElseCode),
	code_info__generate_branch_end(model_non, StoreMap, ElseSaveCode),

	code_info__get_next_label(EndLab),
	{ JumpToEndCode = node([goto(label(EndLab))
		- "Jump to the end of if-then-else"]) },
	{ EndLabelCode = node([label(EndLab) - "end of if-then-else"]) },
	{ Code = tree(ModContCode,
		 tree(FlushEnclosingResumeVarsCode,
		 tree(SaveMaxfrCode,
		 tree(SaveHPCode,
		 tree(SaveTicketCode,
		 tree(CondCode,
		 tree(SoftCutCode,
		 tree(FlushCode,
		 tree(DiscardTicketCode,
		 tree(ThenCode,
		 tree(ThenSaveCode,
		 tree(JumpToEndCode,
		 tree(RestoreContCode,
		 tree(RestoreHPCode,
		 tree(RestoreTicketCode,
		 tree(ElseCode,
		 tree(ElseSaveCode,
		      EndLabelCode)))))))))))))))))
	},
	code_info__remake_with_store_map(StoreMap).

%---------------------------------------------------------------------------%
