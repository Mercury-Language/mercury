%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ite_gen.

:- interface.

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred ite_gen__generate_det_ite(hlds__goal, hlds__goal, hlds__goal,
					code_tree, code_info, code_info).
:- mode ite_gen__generate_det_ite(in, in, in, out, in, out) is det.

:- pred ite_gen__generate_semidet_ite(hlds__goal, hlds__goal, hlds__goal,
					code_tree, code_info, code_info).
:- mode ite_gen__generate_semidet_ite(in, in, in, out, in, out) is det.

:- pred ite_gen__generate_nondet_ite(hlds__goal, hlds__goal, hlds__goal,
					code_tree, code_info, code_info).
:- mode ite_gen__generate_nondet_ite(in, in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module set, tree, list, map, std_util, require, options, globals.

ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, Instr) -->
	code_info__get_globals(Options),
	{ 
		globals__lookup_bool_option(Options,
				reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(CondGoal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	},
	code_info__maybe_save_hp(ReclaimHeap, HPSaveCode),
	code_info__get_next_label(ElseLab, no),
	code_info__push_failure_cont(known(ElseLab)),
	code_info__generate_nondet_saves(SaveCode),
		% Grab the instmap
		% generate the semi-deterministic test goal
	code_info__get_instmap(InstMap),
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_semi_goal(CondGoal, TestCode),
	code_info__pop_failure_cont,
	code_info__maybe_pop_stack(ReclaimHeap, HPPopCode),
	code_gen__generate_forced_det_goal(ThenGoal, ThenGoalCode),
		% generate code that executes the then condition
		% and branches to the end of the if-then-else
	code_info__slap_code_info(CodeInfo),
	code_info__remake_with_call_info,
		% restore the instmap
	code_info__set_instmap(InstMap),
	code_info__maybe_restore_hp(ReclaimHeap, HPRestoreCode),
	code_gen__generate_forced_det_goal(ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab, no),
		% place the label marking the start of the then code,
		% then execute the then goal, and then mark the end
		% of the if-then-else
	{ ThenCode = tree(
		tree(HPPopCode, ThenGoalCode),
		node([ goto(label(EndLab), label(EndLab)) -
			"Jump to the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			node([label(ElseLab) - "else case"]),
			tree(HPRestoreCode, ElseGoalCode)
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(
		tree(tree(HPSaveCode, SaveCode), TestCode),
		tree(ThenCode, ElseCode)
	) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, Instr) -->
	code_info__get_globals(Options),
	{ 
		globals__lookup_bool_option(Options,
				reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(CondGoal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	},
	code_info__maybe_save_hp(ReclaimHeap, HPSaveCode),
	code_info__get_next_label(ElseLab, no),
	code_info__push_failure_cont(known(ElseLab)),
	code_info__generate_nondet_saves(SaveCode),
		% generate the semi-deterministic test goal
	code_gen__generate_semi_goal(CondGoal, CondCode),
	code_info__pop_failure_cont,
	code_info__get_instmap(InstMap),
	code_info__grab_code_info(CodeInfo),
	code_info__maybe_pop_stack(ReclaimHeap, HPPopCode),
	code_gen__generate_forced_semi_goal(ThenGoal, ThenGoalCode),
	code_info__slap_code_info(CodeInfo),
	code_info__remake_with_call_info,
		% restore the instmap
	code_info__set_instmap(InstMap),
	code_info__maybe_restore_hp(ReclaimHeap, HPRestoreCode),
	code_gen__generate_forced_semi_goal(ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab, no),
	{ TestCode = tree(
		tree(HPSaveCode, SaveCode),
		CondCode
	) },
	{ ThenCode = tree(
		tree(
			HPPopCode,
			ThenGoalCode
		),
		node([ goto(label(EndLab), label(EndLab)) -
			"Jump to the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			node([label(ElseLab) - "else case"]),
			tree(
				HPRestoreCode,
				ElseGoalCode
			)
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(TestCode, tree(ThenCode, ElseCode)) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

ite_gen__generate_nondet_ite(CondGoal, ThenGoal, ElseGoal, Instr) -->
	code_info__get_globals(Options),
	{ 
		globals__lookup_bool_option(Options,
				reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(CondGoal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	},
	code_info__maybe_save_hp(ReclaimHeap, HPSaveCode),
	code_info__get_next_label(ElseLab, no),
	code_info__push_failure_cont(known(ElseLab)),
	code_info__generate_nondet_saves(SaveCode),
	{ CondGoal = _ - GoalInfo },
	{ goal_info_determinism(GoalInfo, CondDeterminism) },
	{ CondDeterminism = nondeterministic ->
		ModRedoipCode = node([
			modframe(label(ElseLab)) - "Set failure continuation"
		])
	;
		ModRedoipCode = empty
	},
	code_gen__generate_non_goal(CondGoal, CondCode),
	code_info__pop_failure_cont,
	( { CondDeterminism = nondeterministic } ->
		% XXX bug
		code_info__restore_failure_cont(RestoreRedoipCode)
	;
		{ RestoreRedoipCode = empty }
	),
	code_info__get_instmap(InstMap),
	code_info__grab_code_info(CodeInfo),
	code_info__maybe_pop_stack(ReclaimHeap, HPPopCode),
	code_gen__generate_forced_non_goal(ThenGoal, ThenGoalCode),
	code_info__slap_code_info(CodeInfo),
	code_info__remake_with_call_info,
		% restore the instmap
	code_info__set_instmap(InstMap),
	code_info__maybe_restore_hp(ReclaimHeap, HPRestoreCode),
	code_gen__generate_forced_non_goal(ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab, no),
	{ TestCode = tree(
		tree(
			tree(HPSaveCode, SaveCode),
			ModRedoipCode
		),
		CondCode
	) },
	{ ThenCode = tree(
		tree(
			tree(
				RestoreRedoipCode,
				HPPopCode
			),
			ThenGoalCode
		),
		node([ goto(label(EndLab), label(EndLab)) -
			"Jump to the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			node([label(ElseLab) - "else case"]),
			tree(
				tree(
					RestoreRedoipCode,
					HPRestoreCode
				),
				ElseGoalCode
			)
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(TestCode, tree(ThenCode, ElseCode)) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%
