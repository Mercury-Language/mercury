%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
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
	{ Code = tree(HPSaveCode, tree(GoalsCode, HPRestoreCode)) }.

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
		code_info__push_failure_cont(yes(ElseLab)),
			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_semi_goal(Goal, ThisCode),
		code_info__pop_failure_cont,
		{ ElseLabel = node([
			goto(EndLabel) - "skip to the end of the disj",
			label(ElseLab) - "next case"
		]) },
			% If there are more cases, the we need to restore
			% the expression cache, etc.
		code_info__slap_code_info(CodeInfo),
			% generate the rest of the cases.
		disj_gen__generate_semi_cases(Goals, EndLabel, GoalsCode0),
		{ GoalsCode = tree(ThisCode, tree(ElseLabel, GoalsCode0)) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_non_disj(Goals, tree(SaveCode, GoalsCode)) -->
	{ Goals = [] ->
		error("empty disjunction shouldn't be non-det")
	; Goals = [_]  ->
		error("singleton disjunction")
	;
		true
	},
	code_info__generate_nondet_saves(SaveCode),
	code_info__get_next_label(EndLab),
	disj_gen__generate_non_disj_2(Goals, EndLab, GoalsCode).

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
		code_info__push_failure_cont(yes(ContLab0)),
		{ ContCode = node([
			modframe(yes(ContLab0)) -
					"Set failure continuation"
		]) },
		{ FailCode = node([
			goto(EndLab) - "Jump to end of disj",
			label(ContLab0) - "Start of next disjunct"
		]) },
		code_info__grab_code_info(CodeInfo),
		code_info__get_globals(Globals),
		{	globals__lookup_bool_option(Globals,
				reclaim_heap_on_nondet_failure, yes),
			code_util__goal_may_allocate_heap(Goal)
		->
			ReclaimHeap = yes
		;
			ReclaimHeap = no
		},
		code_info__maybe_save_hp(ReclaimHeap, SaveHeapCode),
		code_gen__generate_forced_non_goal(Goal, GoalCode),
		code_info__maybe_restore_hp(ReclaimHeap, RestoreHeapCode),
		code_info__slap_code_info(CodeInfo),
		code_info__pop_failure_cont,
		{ DisjCode = tree(tree(tree(ContCode,SaveHeapCode), GoalCode),
			tree(FailCode, tree(RestoreHeapCode, RestCode))) },
		disj_gen__generate_non_disj_2(Goals, EndLab, RestCode)
	;
		( code_info__failure_cont(ContLab1) ->
			{ Label = yes(ContLab1) }
		;
			{ Label = no }
		),
		{ ContCode = node([
			modframe(Label) - "Restore failure continuation"
		]) },
		{ FailCode = empty },
		code_info__grab_code_info(CodeInfo),
		code_gen__generate_forced_non_goal(Goal, GoalCode),
		{ RestCode = node([
			label(EndLab) - "End of disj"
		]) },
		{ DisjCode = tree(tree(ContCode, GoalCode),
			tree(FailCode, RestCode)) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
