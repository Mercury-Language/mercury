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

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred disj_gen__generate_semi_disj(list(hlds__goal),
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_disj(in, out, in, out) is det.

:- pred disj_gen__generate_non_disj(list(hlds__goal),
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj(in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require, options, globals.

%---------------------------------------------------------------------------%

disj_gen__generate_semi_disj(Goals, Code) -->
	code_info__generate_nondet_saves(SaveVarsCode),
	code_info__get_globals(Globals),
	{ 
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		code_util__goal_list_may_allocate_heap(Goals)
	->
		RestoreHeap = yes
	;
		RestoreHeap = no
	},
	code_info__maybe_save_hp(RestoreHeap, HPSaveCode),
	code_info__get_next_label(EndLabel, no),
	disj_gen__generate_semi_cases(Goals, EndLabel, GoalsCode),
	code_info__remake_with_store_map,
	code_info__maybe_restore_hp(RestoreHeap, HPRestoreCode),
	{ Code = tree(tree(SaveVarsCode, HPSaveCode),
			tree(GoalsCode, HPRestoreCode)) }.

:- pred disj_gen__generate_semi_cases(list(hlds__goal), label,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_cases(in, in, out, in, out) is det.

disj_gen__generate_semi_cases([], _EndLabel, Code) -->
		% This only gets executed if the disjunction
		% was empty, corresponding to an explicit `fail' in the
		% source code.
	code_info__generate_failure(Code).
disj_gen__generate_semi_cases([Goal|Goals], EndLabel, GoalsCode) -->
	(
		{ Goals = [] }
	->
			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_semi_goal(Goal, ThisCode),
		{ GoalsCode = tree(ThisCode, node([
			label(EndLabel) - "End of semideterministic disj"
		])) }
	;
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLab, no),
		code_info__push_failure_cont(known(ElseLab)),
			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_semi_goal(Goal, ThisCode),
		code_info__pop_failure_cont,
		{ ElseLabel = node([
			goto(label(EndLabel), label(EndLabel)) -
				"skip to the end of the disj",
			label(ElseLab) - "next case"
		]) },
			% If there are more cases, the we need to restore
			% the machine state, and clear registers, since
			% we need to use the saved input vars.
		code_info__slap_code_info(CodeInfo),
		code_info__remake_with_call_info,
			% generate the rest of the cases.
		disj_gen__generate_semi_cases(Goals, EndLabel, GoalsCode0),
		{ GoalsCode = tree(ThisCode, tree(ElseLabel, GoalsCode0)) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_non_disj(Goals0, Code) -->
	{ disj_gen__sort_cases(Goals0, Goals1) },

		% Sanity check
	{ Goals1 = [] ->
		error("empty disjunction shouldn't be non-det")
	; Goals1 = [_]  ->
		error("singleton disjunction")
	;
		true
	},

	code_info__generate_nondet_saves(SaveVarsCode),

	disj_gen__hijack_failure_cont(ContLabel, HijackCode),

	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_nondet_failure, ReclaimHeap) },
	code_info__maybe_save_hp(ReclaimHeap, SaveHeapCode),

	code_info__get_next_label(EndLab, yes),
	disj_gen__generate_non_disj_2(Goals1, EndLab, ContLabel, GoalsCode),
	{ Code = tree(tree(SaveVarsCode, HijackCode),
			tree(SaveHeapCode, GoalsCode)) },

		% since we don't know which disjunct we have come from
		% we must set the current failure continuation to unkown.
	code_info__pop_failure_cont,
	code_info__push_failure_cont(unknown).

:- pred disj_gen__generate_non_disj_2(list(hlds__goal), label, label,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj_2(in, in, in, out, in, out) is det.

disj_gen__generate_non_disj_2([], _EndLab, _ContLabel, _Code) -->
	{ error("disj_gen__generate_non_disj_2") }.
disj_gen__generate_non_disj_2([Goal|Goals], EndLab, ContLab0, DisjCode) -->
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_nondet_failure, ReclaimHeap) },
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_forced_non_goal(Goal, GoalCode),
	code_info__slap_code_info(CodeInfo),
	{ SuccCode = node([
		goto(label(EndLab), label(EndLab)) -
			"Jump to end of disj",
		label(ContLab0) - "Start of next disjunct"
	]) },
	( { Goals = [] } ->
		{ error("disj_gen__generate_non_disj_2 #2") }
	; { Goals = [Goal2] } ->
		disj_gen__restore_failure_cont(RestoreAfterFailureCode),
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHeapCode),
		code_info__remake_with_call_info,
		code_info__maybe_pop_stack(ReclaimHeap, PopCode),
		code_gen__generate_forced_non_goal(Goal2, Goal2Code),
		{ EndCode = node([
			label(EndLab) - "End of disj"
		]) },
		{ DisjCode = tree(tree(GoalCode, SuccCode),
				tree(RestoreAfterFailureCode,
				tree(RestoreHeapCode, tree(PopCode,
				tree(Goal2Code, EndCode))))) }
	;
		disj_gen__modify_failure_cont(ContLab1, ModifyFailureContCode),
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHeapCode),
		code_info__remake_with_call_info,
		disj_gen__generate_non_disj_2(Goals, EndLab, ContLab1,
				RestCode),
		{ DisjCode = tree(tree(GoalCode, SuccCode),
				tree(ModifyFailureContCode,
				tree(RestoreHeapCode, RestCode))) }
	).

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

:- pred disj_gen__hijack_failure_cont(label, code_tree,
					code_info, code_info).
:- mode disj_gen__hijack_failure_cont(out, out, in, out) is det.

	% Set the failure continuation for the
	% maxfr to point to the start of the next
	% disjunct. If the failure continuation is
	% known(_) then this is equivalent to a
	% modframe.
disj_gen__hijack_failure_cont(ContLab, HijackCode) -->
	code_info__failure_cont(OrigCont),
	code_info__get_next_label(ContLab, yes),
	code_info__push_failure_cont(known(ContLab)),
	( { OrigCont = unknown } ->
			% efficiency of this code could be improved
		{ HijackCode = node([
			mkframe("hijack", 1, label(ContLab)) -
				"create a temporary frame",
			assign(curfr, lval(succfr(lval(maxfr)))) -
				"restore curfr (which was clobbered by mkframe)"
		]) }
	;
		{ HijackCode = node([
			assign(redoip(lval(maxfr)),
				const(address_const(label(ContLab)))) -
				"Set failure continuation"
		]) }
	).

:- pred disj_gen__modify_failure_cont(label, code_tree,
					code_info, code_info).
:- mode disj_gen__modify_failure_cont(out, out, in, out) is det.

disj_gen__modify_failure_cont(ContLab, ModifyCode) -->
	code_info__pop_failure_cont,
	code_info__failure_cont(OrigCont),
	( { OrigCont = unknown } ->
		{ RestoreCurfrCode = node([
			assign(curfr, lval(succfr(lval(maxfr)))) -
				"restore curfr from temporary frame"
		]) }
	;
		{ RestoreCurfrCode = empty }
	),
	code_info__get_next_label(ContLab, yes),
	code_info__push_failure_cont(known(ContLab)),
	{ ContCode = node([
		assign(redoip(lval(maxfr)),
			const(address_const(label(ContLab)))) -
			"Set failure continuation"
	]) },
	{ ModifyCode = tree(RestoreCurfrCode, ContCode) }.

:- pred disj_gen__restore_failure_cont(code_tree,
					code_info, code_info).
:- mode disj_gen__restore_failure_cont(out, in, out) is det.

disj_gen__restore_failure_cont(RestoreCode) -->
	code_info__pop_failure_cont,
	code_info__failure_cont(OrigCont),
	( { OrigCont = unknown } ->
		{ RestoreCode = node([
			assign(curfr, lval(succfr(lval(maxfr)))) -
				"restore curfr from temporary frame",
			assign(maxfr, lval(prevfr(lval(maxfr)))) -
				"pop the temporary frame"
		]) }
	;
		% this generates a modframe
		code_info__restore_failure_cont(RestoreCode)
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
