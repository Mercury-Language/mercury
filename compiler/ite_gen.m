%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% file: ite_gen.m
%
% main authors: conway, fjh.
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
	{ CondGoal = _Goal - CondGoalInfo },
	{ goal_info_cont_lives(CondGoalInfo, MaybeLives) },
	{
		MaybeLives = yes(Vars0)
	->
		Vars = Vars0
	;
		error("ite_gen__generate_det_ite: no cont_lives!")
	},
	code_info__make_known_failure_cont(Vars, no, ModContCode),
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
		% Grab the instmap
		% generate the semi-deterministic test goal
	code_info__get_instmap(InstMap),
	code_gen__generate_goal(model_semi, CondGoal, TestCode),
	code_info__grab_code_info(CodeInfo),
	code_info__pop_failure_cont,
	code_info__maybe_pop_stack(ReclaimHeap, HPPopCode),
	code_gen__generate_forced_goal(model_det, ThenGoal, ThenGoalCode),
		% generate code that executes the then condition
		% and branches to the end of the if-then-else
	code_info__slap_code_info(CodeInfo),
	code_info__restore_failure_cont(RestoreContCode),
		% restore the instmap
	code_info__set_instmap(InstMap),
	code_info__maybe_restore_hp(ReclaimHeap, HPRestoreCode),
	code_gen__generate_forced_goal(model_det, ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab),
		% place the label marking the start of the then code,
		% then execute the then goal, and then mark the end
		% of the if-then-else
	{ CondCode = tree(
		HPSaveCode,
		TestCode
	) },
	{ ThenCode = tree(
		tree(HPPopCode, ThenGoalCode),
		node([ goto(label(EndLab)) -
			"Jump to the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			tree(RestoreContCode, HPRestoreCode),
			ElseGoalCode
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(
		tree(ModContCode, CondCode),
		tree(ThenCode, ElseCode)
	) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, Instr) -->
	code_info__get_globals(Options),
	{ CondGoal = _Goal - CondGoalInfo },
	{ goal_info_cont_lives(CondGoalInfo, MaybeLives) },
	{
		MaybeLives = yes(Vars0)
	->
		Vars = Vars0
	;
		error("ite_gen__generate_det_ite: no cont_lives!")
	},
	code_info__make_known_failure_cont(Vars, no, ModContCode),
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
		% generate the semi-deterministic test goal
	code_info__get_instmap(InstMap),
	code_gen__generate_goal(model_semi, CondGoal, CondCode),
	code_info__grab_code_info(CodeInfo),
	code_info__maybe_pop_stack(ReclaimHeap, HPPopCode),
	code_info__pop_failure_cont,
	code_gen__generate_forced_goal(model_semi, ThenGoal, ThenGoalCode),
	code_info__slap_code_info(CodeInfo),
	code_info__restore_failure_cont(RestoreContCode),
		% restore the instmap
	code_info__set_instmap(InstMap),
	code_info__maybe_restore_hp(ReclaimHeap, HPRestoreCode),
	code_gen__generate_forced_goal(model_semi, ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab),
	{ TestCode = tree(
		tree(ModContCode, HPSaveCode),
		CondCode
	) },
	{ ThenCode = tree(
		tree(
			HPPopCode,
			ThenGoalCode
		),
		node([ goto(label(EndLab)) -
			"Jump to the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			tree(RestoreContCode, HPRestoreCode),
			ElseGoalCode
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
	{ CondGoal = _ - GoalInfo },
	{ goal_info_get_code_model(GoalInfo, CodeModel) },
	(
		{ CodeModel = model_non }
	->
		{ NondetCond = yes }
	;
		{ NondetCond = no }
	),
	{ CondGoal = _Goal - CondGoalInfo },
	{ goal_info_cont_lives(CondGoalInfo, MaybeLives) },
	{
		MaybeLives = yes(Vars0)
	->
		Vars = Vars0
	;
		error("ite_gen__generate_det_ite: no cont_lives!")
	},
	code_info__make_known_failure_cont(Vars, NondetCond, ModContCode),
	(
		{ NondetCond = yes }
	->
			% prevent the condition from hijacking the redoip slot
			% We could improve the efficiency of this
		code_info__unset_failure_cont,
		code_info__save_maxfr(MaxfrLval0, SaveMaxfrCode),
		{ MaybeMaxfrLval = yes(MaxfrLval0) }
	;
		{ MaybeMaxfrLval = no },
		{ SaveMaxfrCode = empty }
	),
	code_info__maybe_save_hp(ReclaimHeap, HPSaveCode),
	(
		{ NondetCond = yes }
	->
		% we need to save variables on the stack here since the
		% condition might do a redo() without saving them
		{ set__to_sorted_list(Vars, VarList) },
		code_gen__ensure_vars_are_saved(VarList, EnsureCode)
	;
		{ EnsureCode = empty }
	),
	code_info__get_instmap(InstMap),
	code_gen__generate_goal(model_non, CondGoal, CondCode0),
	{ CondCode = tree(EnsureCode, CondCode0) },
	code_info__grab_code_info(CodeInfo),
	code_info__maybe_pop_stack(ReclaimHeap, HPPopCode),
	code_info__pop_failure_cont,
	(
		{ MaybeMaxfrLval = yes(MaxfrLval) }
	->
		code_info__do_soft_cut(MaxfrLval, HackStackCode)
	;
		{ HackStackCode = empty }
	),
	code_gen__generate_forced_goal(model_non, ThenGoal, ThenGoalCode),
	code_info__slap_code_info(CodeInfo),
	code_info__restore_failure_cont(RestoreContCode),
		% restore the instmap
	code_info__set_instmap(InstMap),
	code_info__maybe_restore_hp(ReclaimHeap, HPRestoreCode),
	code_gen__generate_forced_goal(model_non, ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab),
	{ TestCode = tree(
		tree(
			tree(ModContCode, SaveMaxfrCode),
			HPSaveCode
		),
		CondCode
	) },
	{ ThenCode = tree(
		tree(
			tree(
				HackStackCode,
				HPPopCode
			),
			ThenGoalCode
		),
		node([ goto(label(EndLab)) -
			"Jump to the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			tree(
				RestoreContCode,
				HPRestoreCode
			),
			ElseGoalCode
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(TestCode, tree(ThenCode, ElseCode)) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%
