%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: disj_gen.m:
%
% Generate code for disjunctions.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module disj_gen.

:- interface.

:- import_module hlds_goal, llds, code_info.

:- pred disj_gen__generate_det_disj(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_det_disj(in, in, out, in, out) is det.

:- pred disj_gen__generate_semi_disj(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_disj(in, in, out, in, out) is det.

:- pred disj_gen__generate_non_disj(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj(in, in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, code_gen, code_util, options, globals.
:- import_module bool, set, tree, list, map, std_util, require.

%---------------------------------------------------------------------------%

disj_gen__generate_det_disj(Goals, StoreMap, Code) -->
		% If we are using constraints, save the current solver state
		% before the first disjunct.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			constraints, SaveTicket) },
	code_info__maybe_save_ticket(SaveTicket, SaveTicketCode),

		% Rather than saving the heap pointer here,
		% we delay saving it until we get to the first
		% disjunct that might allocate some heap space.
	{ SavedHP = no },		% we haven't yet saved the heap pointer
	{ MustRestoreHP = no },	% we won't need to restore yet

		% Generate all the cases
	code_info__get_next_label(EndLabel),
	disj_gen__generate_det_disj_2(Goals, StoreMap, EndLabel,
		SavedHP, MustRestoreHP, GoalsCode),
	{ Code = tree(SaveTicketCode, GoalsCode) }.

:- pred disj_gen__generate_det_disj_2(list(hlds__goal), store_map,
			label, bool, bool, code_tree, code_info, code_info).
:- mode disj_gen__generate_det_disj_2(in, in, in, in, in, out, in, out) is det.

	% To generate code for a det disjunction, we generate a
	% chain (if-then-else style) of goals until we come to
	% one that cannot fail. When we get to a goal that can't
	% fail, we just generate that goal.
disj_gen__generate_det_disj_2([], _, _, _, _, _) -->
	{ error("Empty det disj!") }.
disj_gen__generate_det_disj_2([Goal | Goals], StoreMap, EndLabel,
		SavedHP, MustRestoreHP, Code) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_determinism(GoalInfo, GoalDet) },
	{ goal_info_get_code_model(GoalInfo, GoalModel) },
	{ determinism_components(GoalDet, CanFail, _) },
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			constraints, RestoreTicket) },
	(
		{ CanFail = cannot_fail }
	->
		% If this disjunct can't fail, we ignore
		% the remaining disjuncts, and treat this
		% disjunct as the last disjunct.

			% Restore the heap pointer if necessary,
			% and pop the temp stack that we saved it on
			% if we saved it
		code_info__maybe_get_old_hp(MustRestoreHP, RestoreHPCode),
		code_info__maybe_pop_stack(SavedHP, UnSaveHPCode),

			% Restore the solver state if necessary
		code_info__maybe_restore_ticket_and_pop(RestoreTicket, 
			RestoreTicketCode),

			% Generate the goal
		code_gen__generate_forced_goal(GoalModel, Goal, StoreMap,
			GoalCode),

		{ EndCode = node([label(EndLabel) - "end of det disj"]) },
		{ Code = tree(RestoreHPCode,
			 tree(UnSaveHPCode,
			 tree(RestoreTicketCode,
			 tree(GoalCode,
			      EndCode)))) },
		code_info__remake_with_store_map(StoreMap)
	;
		code_info__get_live_variables(VarList),
		{ set__list_to_set(VarList, Vars) },
		code_info__make_known_failure_cont(Vars, no, ModContCode),

			% Reset the heap pointer to recover memory allocated
			% by the previous disjunct, if necessary
		code_info__maybe_get_old_hp(MustRestoreHP, RestoreHPCode),

			% If this disjunct might allocate heap space, then
			% we must restore the HP on entry to the next one.
		{ globals__lookup_bool_option(Globals,
				reclaim_heap_on_semidet_failure, ReclaimHeap) },
		{ ReclaimHeap = yes, code_util__goal_may_allocate_heap(Goal) ->
			MustRestoreHP_Next = yes
		;
			MustRestoreHP_Next = no
		},

			% If we are going to need to restore the HP,
			% and we haven't saved it already, then we must
			% save it now.
		( { MustRestoreHP_Next = yes, SavedHP = no } ->
			code_info__save_hp(SaveHPCode),
			{ SavedHP_Next = yes }
		;
			{ SaveHPCode = empty },
			{ SavedHP_Next = SavedHP }
		),

		code_info__maybe_restore_ticket(RestoreTicket,
			RestoreTicketCode),

		code_info__grab_code_info(CodeInfo),

			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_goal(GoalModel, Goal, StoreMap,
			GoalCode),
		{ BranchCode = node([goto(label(EndLabel)) -
						"skip to end of det disj"]) },
		{ ThisCode = tree(GoalCode, BranchCode) },

			% If there are more cases, then we need to restore
			% the machine state, and clear registers, since
			% we need to use the saved input vars.
		code_info__slap_code_info(CodeInfo),
		code_info__restore_failure_cont(RestoreContCode),
		(
			{ Goals \= [] }
		->
			disj_gen__generate_det_disj_2(Goals, StoreMap,
				EndLabel, SavedHP_Next, MustRestoreHP_Next,
				RestCode)
		;
			% a det disj should have at least one det disjunct
			{ error("disj_gen__generate_det_disj: huh?") }
		),
		{ Code = tree(ModContCode, 
			 tree(RestoreHPCode,
			 tree(SaveHPCode,
			 tree(RestoreTicketCode,
			 tree(ThisCode,
			 tree(RestoreContCode,
			      RestCode)))))) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_semi_disj(Goals, StoreMap, Code) -->
	( { Goals = [] } ->
		code_info__generate_failure(Code)
	;
		disj_gen__generate_semi_disj_2(Goals, StoreMap, Code)
	).

:- pred disj_gen__generate_semi_disj_2(list(hlds__goal), store_map,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_disj_2(in, in, out, in, out) is det.

disj_gen__generate_semi_disj_2(Goals, StoreMap, Code) -->
		% If we are using constraints, save the current solver state
		% before the first disjunct.
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			constraints, SaveTicket) },
	code_info__maybe_save_ticket(SaveTicket, SaveTicketCode),

		% Rather than saving the heap pointer here,
		% we delay saving it until we get to the first
		% disjunct that might allocate some heap space.
	{ SavedHP = no },	% we haven't yet saved the heap pointer
	{ MustRestoreHP = no },	% we won't need to restore yet

		% Generate all the cases
	code_info__get_next_label(EndLabel),
	disj_gen__generate_semi_cases(Goals, StoreMap, EndLabel,
		SavedHP, MustRestoreHP, GoalsCode),

		% Remake the code_info using the store map for the
		% variable locations at the end of the disjunction.
	code_info__remake_with_store_map(StoreMap),

	{ Code = tree(SaveTicketCode, GoalsCode) }.

:- pred disj_gen__generate_semi_cases(list(hlds__goal), store_map, label,
			bool, bool, code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_cases(in, in, in, in, in, out, in, out) is det.

disj_gen__generate_semi_cases([], _, _, _, _, _) -->
	{ error("disj_gen__generate_semi_cases") }.
disj_gen__generate_semi_cases([Goal | Goals], StoreMap, EndLabel,
		SavedHP, MustRestoreHP, GoalsCode) -->
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			constraints, RestoreTicket) },
	(
		{ Goals = [] }
	->
			% Restore the heap pointer if necessary,
			% and pop the temp stack that we saved it on
			% if we saved it
		code_info__maybe_get_old_hp(MustRestoreHP, RestoreHPCode),
		code_info__maybe_pop_stack(SavedHP, UnSaveHPCode),

			% Restore the solver state if necessary
		code_info__maybe_restore_ticket_and_pop(RestoreTicket, 
			RestoreTicketCode),

			% Generate the case as a semi-deterministic goal
		code_gen__generate_forced_goal(model_semi, Goal, StoreMap,
			ThisCode),

		{ EndCode = node([
			label(EndLabel) - "End of model_semi disj"
		]) },

		{ GoalsCode = tree(RestoreHPCode,
			      tree(UnSaveHPCode,
			      tree(RestoreTicketCode,
		              tree(ThisCode,
				   EndCode)))) }
	;
		code_info__get_live_variables(VarList),
		{ set__list_to_set(VarList, Vars) },
		code_info__make_known_failure_cont(Vars, no, ModContCode),

			% Reset the heap pointer to recover memory allocated
			% by the previous disjunct, if necessary
		code_info__maybe_get_old_hp(MustRestoreHP, RestoreHPCode),

			% If this disjunct might allocate heap space, then
			% we must restore the HP on entry to the next one.
		{ globals__lookup_bool_option(Globals,
				reclaim_heap_on_semidet_failure, ReclaimHeap) },
		{ ReclaimHeap = yes, code_util__goal_may_allocate_heap(Goal) ->
			MustRestoreHP_Next = yes
		;
			MustRestoreHP_Next = no
		},

			% If we are going to need to restore the HP,
			% and we haven't saved it already, then we must
			% save it now.
		( { MustRestoreHP_Next = yes, SavedHP = no } ->
			code_info__save_hp(SaveHPCode),
			{ SavedHP_Next = yes }
		;
			{ SaveHPCode = empty },
			{ SavedHP_Next = SavedHP }
		),

			% Reset the solver state if necessary
		code_info__maybe_restore_ticket(RestoreTicket,
			RestoreTicketCode),

		code_info__grab_code_info(CodeInfo),

			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_goal(model_semi, Goal, StoreMap,
			ThisCode),

			% If there are more cases, then we need to restore
			% the machine state, and clear registers, since
			% we need to use the saved input vars.
		code_info__slap_code_info(CodeInfo),
		code_info__restore_failure_cont(RestoreContCode),

			% generate the rest of the cases.
		disj_gen__generate_semi_cases(Goals, StoreMap, EndLabel,
			SavedHP_Next, MustRestoreHP_Next, GoalsCode0),
		{ SuccCode = node([
			goto(label(EndLabel)) - "Jump to end of model_semi disj"
		  ]) },
		{ GoalsCode = tree(ModContCode, 
			      tree(RestoreHPCode,
			      tree(SaveHPCode,
			      tree(RestoreTicketCode,
			      tree(ThisCode,
			      tree(SuccCode,
			      tree(RestoreContCode,
				   GoalsCode0))))))) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_non_disj(Goals1, StoreMap, Code) -->

		% Sanity check
	{ Goals1 = [] ->
		error("empty disjunction shouldn't be non-det")
	; Goals1 = [_]  ->
		error("singleton disjunction")
	;
		true
	},

	code_info__get_live_variables(VarList),
	{ set__list_to_set(VarList, Vars) },
	code_info__make_known_failure_cont(Vars, yes, HijackCode),

	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_nondet_failure, ReclaimHeap) },
	code_info__maybe_save_hp(ReclaimHeap, SaveHeapCode),
	{ globals__lookup_bool_option(Globals,
			constraints, SaveTicket) },
	code_info__maybe_save_ticket(SaveTicket, SaveTicketCode),
	code_info__get_next_label(EndLab),
	disj_gen__generate_non_disj_2(Goals1, StoreMap, EndLab, GoalsCode),
	{ Code = tree(HijackCode, 
		tree(SaveHeapCode, 
		tree(SaveTicketCode, GoalsCode))) },

		% since we don't know which disjunct we have come from
		% we must set the current failure continuation to unkown.
	code_info__unset_failure_cont.

:- pred disj_gen__generate_non_disj_2(list(hlds__goal), store_map, label,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj_2(in, in, in, out, in, out) is det.

disj_gen__generate_non_disj_2([], _StoreMap, _EndLab, _Code) -->
	{ error("disj_gen__generate_non_disj_2") }.
disj_gen__generate_non_disj_2([Goal | Goals], StoreMap, EndLab, DisjCode) -->
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_nondet_failure, ReclaimHeap) },
	{ globals__lookup_bool_option(Globals,
			constraints, RestoreTicket) },
	code_info__get_live_variables(Vars),
	code_gen__ensure_vars_are_saved(Vars, GoalCode0), 
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_forced_goal(model_non, Goal, StoreMap, GoalCode1),
	{ GoalCode = tree(GoalCode0, GoalCode1) },
	code_info__slap_code_info(CodeInfo),
	{ SuccCode =
		node([
			goto(label(EndLab)) - "Jump to end of disj"
		])
	},
	( { Goals = [] } ->
		{ error("disj_gen__generate_non_disj_2 #2") }
	; { Goals = [Goal2] } ->
			% Process the last disjunct
		code_info__remake_with_stack_slots,
		code_info__restore_failure_cont(RestoreAfterFailureCode),
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHeapCode),
		code_info__maybe_pop_stack(ReclaimHeap, PopCode),
			% restore and pop the solver ticket before 
			% the final arm of the disjunction
		code_info__maybe_restore_ticket_and_pop(RestoreTicket, 
			RestorePopCode),
		code_gen__generate_forced_goal(model_non, Goal2, StoreMap,
			Goal2Code),
		{ EndCode = node([
			label(EndLab) - "End of disj"
		]) },
		{ DisjCode = tree(tree(GoalCode, SuccCode),
				tree(RestoreAfterFailureCode,
				tree(RestoreHeapCode, 
				tree(PopCode,
				tree(RestorePopCode, 
				tree(Goal2Code, EndCode)))))) }
	;
		code_info__remake_with_stack_slots,
		code_info__modify_failure_cont(ModifyFailureContCode),
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHeapCode),
		code_info__maybe_restore_ticket(RestoreTicket, 
			RestoreTicketCode),
		disj_gen__generate_non_disj_2(Goals, StoreMap, EndLab,
			RestCode),
		{ DisjCode = tree(tree(GoalCode, SuccCode),
				tree(ModifyFailureContCode,
				tree(RestoreHeapCode, 
				tree(RestoreTicketCode, RestCode)))) }
	).

%---------------------------------------------------------------------------%

:- pred disj_gen__sort_cases(list(hlds__goal), list(hlds__goal)).
:- mode disj_gen__sort_cases(in, out) is det.

disj_gen__sort_cases(Goals0, Goals) :-
	disj_gen__sort_cases_2(Goals0, CanFail, CannotFail),
	list__append(CannotFail, CanFail, Goals).

:- pred disj_gen__sort_cases_2(list(hlds__goal), list(hlds__goal),
					list(hlds__goal)).
:- mode disj_gen__sort_cases_2(in, out, out) is det.

disj_gen__sort_cases_2([], [], []).
disj_gen__sort_cases_2([Goal0 - GoalInfo0 | Goals0], CanFail, CannotFail) :-
	disj_gen__sort_cases_2(Goals0, CanFail0, CannotFail0),
	goal_info_get_code_model(GoalInfo0, CodeModel),
	( CodeModel = model_det ->
		CannotFail = [Goal0 - GoalInfo0 | CannotFail0],
		CanFail = CanFail0
	;
		CannotFail = CannotFail0,
		CanFail = [Goal0 - GoalInfo0 | CanFail0]
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
