%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% File: disj_gen.nl:
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
	code_info__get_next_label(EndLabel),
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
		code_info__get_next_label(ElseLab),
		code_info__push_failure_cont(known(ElseLab)),
			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_semi_goal(Goal, ThisCode),
		code_info__pop_failure_cont,
		{ ElseLabel = node([
			goto(label(EndLabel)) - "skip to the end of the disj",
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
		% Sanity check
	{ Goals0 = [] ->
		error("empty disjunction shouldn't be non-det")
	; Goals0 = [_]  ->
		error("singleton disjunction")
	;
		true
	},
	{ disj_gen__sort_cases(Goals0, Goals1) },
	code_info__generate_nondet_saves(SaveVarsCode),
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals,
			reclaim_heap_on_nondet_failure, ReclaimHeap) },
	code_info__maybe_save_hp(ReclaimHeap, SaveHeapCode),
	code_info__get_next_label(EndLab),
	disj_gen__generate_non_disj_2(Goals1, EndLab, GoalsCode),
	{ Code = tree(SaveVarsCode, tree(SaveHeapCode, GoalsCode)) }.

:- pred disj_gen__generate_non_disj_2(list(hlds__goal), label, code_tree,
						code_info, code_info).
:- mode disj_gen__generate_non_disj_2(in, in, out, in, out) is det.

disj_gen__generate_non_disj_2([], _EndLab, _EndCode) -->
	{ error("disj_gen__generate_non_disj_2") }.
disj_gen__generate_non_disj_2([Goal|Goals], EndLab, DisjCode) -->
	(
		{ Goals = [_|_] }
	->
		code_info__get_next_label(ContLab0),
		code_info__push_failure_cont(known(ContLab0)),
		{ ContCode = node([
			modframe(label(ContLab0)) -
					"Set failure continuation"
		]) },
		code_info__grab_code_info(CodeInfo),
		code_gen__generate_forced_non_goal(Goal, GoalCode),
		{ SuccCode = node([
			goto(label(EndLab)) - "Jump to end of disj",
			label(ContLab0) - "Start of next disjunct"
		]) },
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals,
				reclaim_heap_on_nondet_failure, ReclaimHeap) },
		code_info__maybe_get_old_hp(ReclaimHeap, RestoreHeapCode),
		code_info__slap_code_info(CodeInfo),
		code_info__remake_with_call_info,
		code_info__pop_failure_cont,
		{ DisjCode = tree(tree(ContCode, GoalCode),
			tree(SuccCode, tree(RestoreHeapCode, RestCode))) },
		disj_gen__generate_non_disj_2(Goals, EndLab, RestCode)
	;
		code_info__pop_stack(PopCode),
		code_info__restore_failure_cont(ContCode),
		code_gen__generate_forced_non_goal(Goal, GoalCode),
		{ EndCode = node([
			label(EndLab) - "End of disj"
		]) },
		{ DisjCode = tree(tree(PopCode, ContCode),
				tree(GoalCode, EndCode)) }
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
	goal_info_determinism(GoalInfo0, Category),
	( Category = deterministic ->
		CannotFail = [Goal0 - GoalInfo0 | CannotFail0],
		CanFail = CanFail0
	;
		CannotFail = CannotFail0,
		CanFail = [Goal0 - GoalInfo0 | CanFail0]
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
