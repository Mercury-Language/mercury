%---------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Code generation - do middle recursion optimization
% Main authors: zs and conway.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ll_backend__middle_rec.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.

:- pred middle_rec__match_and_generate(hlds_goal, code_tree,
	code_info, code_info).
:- mode middle_rec__match_and_generate(in, out, in, out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module hlds__code_model.
:- import_module hlds__goal_form.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_module.
:- import_module libs__tree.
:- import_module ll_backend__code_aux.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__code_util.
:- import_module ll_backend__opt_util.
:- import_module ll_backend__unify_gen.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.

:- import_module bool, int, string, list, assoc_list, set, std_util.
:- import_module require.

%---------------------------------------------------------------------------%

middle_rec__match_and_generate(Goal, Instrs, !CI) :-
	Goal = GoalExpr - GoalInfo,
	(
		GoalExpr = switch(Var, cannot_fail, [Case1, Case2]),
		Case1 = case(ConsId1, Goal1),
		Case2 = case(ConsId2, Goal2),
		(
			contains_only_builtins(Goal1),
			code_aux__contains_simple_recursive_call(Goal2,
				!.CI, _)
		->
			middle_rec__generate_switch(Var, ConsId1, Goal1, Goal2,
				GoalInfo, Instrs, !CI)
		;
			contains_only_builtins(Goal2),
			code_aux__contains_simple_recursive_call(Goal1,
				!.CI, _)
		->
			middle_rec__generate_switch(Var, ConsId2, Goal2, Goal1,
				GoalInfo, Instrs, !CI)
		;
			fail
		)
	;
		GoalExpr = if_then_else(Vars, Cond, Then, Else),
		(
			contains_only_builtins(Cond),
			contains_only_builtins(Then),
			code_aux__contains_simple_recursive_call(Else, !.CI,
				no)
		->
			semidet_fail,
			middle_rec__generate_ite(Vars, Cond, Then, Else,
				in_else, GoalInfo, Instrs, !CI)
		;
			contains_only_builtins(Cond),
			code_aux__contains_simple_recursive_call(Then, !.CI,
				no),
			contains_only_builtins(Else)
		->
			semidet_fail,
			middle_rec__generate_ite(Vars, Cond, Then, Else,
				in_then, GoalInfo, Instrs, !CI)
		;
			fail
		)
	).

:- type ite_rec	--->	in_then ; in_else.

%---------------------------------------------------------------------------%

:- pred middle_rec__generate_ite(list(prog_var)::in, hlds_goal::in,
	hlds_goal::in, hlds_goal::in, ite_rec::in, hlds_goal_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

middle_rec__generate_ite(_Vars, _Cond, _Then, _Else, _Rec, _IteGoalInfo,
		Instrs, !CI) :-
	( semidet_fail ->
		Instrs = empty
	;
		error("middle_rec__generate_ite reached")
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__generate_switch(prog_var::in, cons_id::in, hlds_goal::in,
	hlds_goal::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is semidet.

middle_rec__generate_switch(Var, BaseConsId, Base, Recursive, SwitchGoalInfo,
		Instrs, !CI) :-
	code_info__get_stack_slots(!.CI, StackSlots),
	code_info__get_varset(!.CI, VarSet),
	code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment),
	code_info__get_module_info(!.CI, ModuleInfo),
	code_info__get_pred_id(!.CI, PredId),
	code_info__get_proc_id(!.CI, ProcId),
	code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		EntryLabel),

	code_info__pre_goal_update(SwitchGoalInfo, no, !CI),
	unify_gen__generate_tag_test(Var, BaseConsId, branch_on_success,
		BaseLabel, EntryTestCode, !CI),
	tree__flatten(EntryTestCode, EntryTestListList),
	list__condense(EntryTestListList, EntryTestList),

	goal_info_get_store_map(SwitchGoalInfo, StoreMap),
	code_info__remember_position(!.CI, BranchStart),
	code_gen__generate_goal(model_det, Base, BaseGoalCode, !CI),
	code_info__generate_branch_end(StoreMap, no, MaybeEnd1,
		BaseSaveCode, !CI),
	code_info__reset_to_position(BranchStart, !CI),
	code_gen__generate_goal(model_det, Recursive, RecGoalCode, !CI),
	code_info__generate_branch_end(StoreMap, MaybeEnd1, MaybeEnd,
		RecSaveCode, !CI),

	code_info__post_goal_update(SwitchGoalInfo, !CI),
	code_info__after_all_branches(StoreMap, MaybeEnd, !CI),

	ArgModes = code_info__get_arginfo(!.CI),
	HeadVars = code_info__get_headvars(!.CI),
	assoc_list__from_corresponding_lists(HeadVars, ArgModes, Args),
	code_info__setup_return(Args, LiveArgs, EpilogCode, !CI),

	BaseCode = tree(BaseGoalCode, tree(BaseSaveCode, EpilogCode)),
	RecCode = tree(RecGoalCode, tree(RecSaveCode, EpilogCode)),
	LiveValCode = [livevals(LiveArgs) - ""],

	tree__flatten(BaseCode, BaseListList),
	list__condense(BaseListList, BaseList),
	tree__flatten(RecCode, RecListList),
	list__condense(RecListList, RecList),

	% In the code we generate, the base instruction sequence is executed
	% in situations where this procedure has no stack frame. If this
	% sequence refers to stackvars, it will be to some other procedure's
	% variables, which is obviously incorrect.
	opt_util__block_refers_stackvars(BaseList, no),

	list__append(BaseList, RecList, AvoidList),
	middle_rec__find_unused_register(AvoidList, AuxReg),

	middle_rec__split_rec_code(RecList, BeforeList0, AfterList),
	middle_rec__add_counter_to_livevals(BeforeList0, AuxReg,
		BeforeList),

	code_info__get_next_label(Loop1Label, !CI),
	code_info__get_next_label(Loop2Label, !CI),
	code_info__get_total_stackslot_count(!.CI, FrameSize),

	middle_rec__generate_downloop_test(EntryTestList,
		Loop1Label, Loop1Test),

	( FrameSize = 0 ->
		MaybeIncrSp = [],
		MaybeDecrSp = [],
		InitAuxReg = [assign(AuxReg, const(int_const(0)))
				- "initialize counter register"],
		IncrAuxReg = [assign(AuxReg, binop((+),
				lval(AuxReg),
				const(int_const(1))))
				- "increment loop counter"],
		DecrAuxReg = [assign(AuxReg, binop((-),
				lval(AuxReg),
				const(int_const(1))))
				- "decrement loop counter"],
		TestAuxReg = [if_val(binop((>),
				lval(AuxReg), const(int_const(0))),
				label(Loop2Label))
				- "test on upward loop"]
	;
		PushMsg = code_gen__push_msg(ModuleInfo, PredId, ProcId),
		MaybeIncrSp = [incr_sp(FrameSize, PushMsg) - ""],
		MaybeDecrSp = [decr_sp(FrameSize) - ""],
		InitAuxReg =  [assign(AuxReg, lval(sp))
				- "initialize counter register"],
		IncrAuxReg = [],
		DecrAuxReg = [],
		TestAuxReg = [if_val(binop((>),
				lval(sp), lval(AuxReg)),
				label(Loop2Label))
				- "test on upward loop"]
	),

	% Even though the recursive call is followed by some goals
	% in the HLDS, these goals may generate no LLDS code, so
	% it is in fact possible for AfterList to be empty.
	% There is no point in testing BeforeList for empty,
	% since if it is, the code is an infinite loop anyway.

	(
		AfterList = [],
		list__condense([
			[
				label(EntryLabel) - "Procedure entry point",
				comment(SlotsComment) - ""
			],
			EntryTestList,
			[
				label(Loop1Label)
					- "start of the down loop"
			],
			BeforeList,
			Loop1Test,
			[
				label(BaseLabel)
					- "start of base case"
			],
			BaseList,
			LiveValCode,
			[
				goto(succip)
				- "exit from base case"
			]
		], InstrList)
	;
		AfterList = [_ | _],
		% The instruction list we are constructing has two copies
		% of BaseList. If this list of instructions defines any
		% labels, we must either not apply this version of the
		% optimization, or we must consistently substitute the
		% labels (which will be referred to only from within the
		% BaseList instructions themselves). We choose the former
		% course.
		middle_rec__find_labels(BaseList, BaseLabels),
		BaseLabels = [],
		list__condense([
			[
				label(EntryLabel) - "Procedure entry point",
				comment(SlotsComment) - ""
			],
			EntryTestList,
			InitAuxReg,
			[
				label(Loop1Label)
					- "start of the down loop"
			],
			MaybeIncrSp,
			IncrAuxReg,
			BeforeList,
			Loop1Test,
			BaseList,
			[
				label(Loop2Label) - ""
			],
			AfterList,
			MaybeDecrSp,
			DecrAuxReg,
			TestAuxReg,
			LiveValCode,
			[
				goto(succip)
					- "exit from recursive case",
				label(BaseLabel)
					- "start of base case"
			],
			BaseList,
			LiveValCode,
			[
				goto(succip)
				- "exit from base case"
			]
		], InstrList)
	),
	Instrs = node(InstrList).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred middle_rec__generate_downloop_test(list(instruction), label,
	list(instruction)).
:- mode middle_rec__generate_downloop_test(in, in, out) is det.

middle_rec__generate_downloop_test([], _, _) :-
	error("middle_rec__generate_downloop_test on empty list").
middle_rec__generate_downloop_test([Instr0 | Instrs0], Target, Instrs) :-
	( Instr0 = if_val(Test, _OldTarget) - _Comment ->
		( Instrs0 = [] ->
			true
		;
			error("middle_rec__generate_downloop_test: " ++
				"if_val followed by other instructions")
		),
		code_util__neg_rval(Test, NewTest),
		Instrs = [if_val(NewTest, label(Target))
			- "test on downward loop"]
	;
		middle_rec__generate_downloop_test(Instrs0, Target, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__split_rec_code(list(instruction),
	list(instruction), list(instruction)).
:- mode middle_rec__split_rec_code(in, out, out) is det.

middle_rec__split_rec_code([], _, _) :-
	error("did not find call in middle_rec__split_rec_code").
middle_rec__split_rec_code([Instr0 | Instrs1], Before, After) :-
	( Instr0 = call(_, _, _, _, _, _) - _ ->
		(
			opt_util__skip_comments(Instrs1, Instrs2),
			Instrs2 = [Instr2 | Instrs3],
			Instr2 = label(_) - _
		->
			Before = [],
			After = Instrs3
		;
			error("middle_rec__split_rec_code: " ++
				"call not followed by label")
		)
	;
		middle_rec__split_rec_code(Instrs1, Before1, After),
		Before = [Instr0 | Before1]
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__add_counter_to_livevals(list(instruction)::in, lval::in,
	list(instruction)::out) is det.

middle_rec__add_counter_to_livevals([], _Lval, []).
middle_rec__add_counter_to_livevals([I0|Is0], Lval, [I|Is]) :-
	(
		I0 = livevals(Lives0) - Comment
	->
		set__insert(Lives0, Lval, Lives),
		I = livevals(Lives) - Comment
	;
		I = I0
	),
	middle_rec__add_counter_to_livevals(Is0, Lval, Is).

%---------------------------------------------------------------------------%

:- pred middle_rec__find_unused_register(list(instruction)::in, lval::out)
	is det.

middle_rec__find_unused_register(Instrs, UnusedReg) :-
	set__init(Used0),
	middle_rec__find_used_registers(Instrs, Used0, Used1),
	set__to_sorted_list(Used1, UsedList),
	middle_rec__find_unused_register_2(UsedList, 1, UnusedReg).

:- pred middle_rec__find_unused_register_2(list(int)::in, int::in, lval::out)
	is det.

middle_rec__find_unused_register_2([], N, reg(r, N)).
middle_rec__find_unused_register_2([H | T], N, Reg) :-
	( N < H ->
		Reg = reg(r, N)
	;
		N1 = N + 1,
		middle_rec__find_unused_register_2(T, N1, Reg)
	).

:- pred middle_rec__find_used_registers(list(instruction)::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers([], !Used).
middle_rec__find_used_registers([Instr - _ | Instrs], !Used) :-
	middle_rec__find_used_registers_instr(Instr, !Used),
	middle_rec__find_used_registers(Instrs, !Used).

:- pred middle_rec__find_used_registers_instr(instr::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_instr(comment(_), !Used).
middle_rec__find_used_registers_instr(livevals(LvalSet), !Used) :-
	set__to_sorted_list(LvalSet, LvalList),
	middle_rec__find_used_registers_lvals(LvalList, !Used).
middle_rec__find_used_registers_instr(block(_, _, Instrs), !Used) :-
	middle_rec__find_used_registers(Instrs, !Used).
middle_rec__find_used_registers_instr(assign(Lval, Rval), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used),
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(call(_, _, _, _, _, _), !Used).
middle_rec__find_used_registers_instr(mkframe(_, _), !Used).
middle_rec__find_used_registers_instr(label(_), !Used).
middle_rec__find_used_registers_instr(goto(_), !Used).
middle_rec__find_used_registers_instr(computed_goto(Rval, _), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(c_code(_, _), !Used).
middle_rec__find_used_registers_instr(if_val(Rval, _), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(incr_hp(Lval, _, _, Rval, _), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used),
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(mark_hp(Lval), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used).
middle_rec__find_used_registers_instr(restore_hp(Rval), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(free_heap(Rval), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(store_ticket(Lval), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used).
middle_rec__find_used_registers_instr(reset_ticket(Rval, _Rsn), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(discard_ticket, !Used).
middle_rec__find_used_registers_instr(prune_ticket, !Used).
middle_rec__find_used_registers_instr(mark_ticket_stack(Lval), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used).
middle_rec__find_used_registers_instr(prune_tickets_to(Rval), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).
middle_rec__find_used_registers_instr(incr_sp(_, _), !Used).
middle_rec__find_used_registers_instr(decr_sp(_), !Used).
middle_rec__find_used_registers_instr(pragma_c(_, Components,
		_, _, _, _, _, _, _), !Used) :-
	middle_rec__find_used_registers_components(Components, !Used).
middle_rec__find_used_registers_instr(init_sync_term(Lval, _), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used).
middle_rec__find_used_registers_instr(fork(_, _, _), !Used).
middle_rec__find_used_registers_instr(join_and_terminate(Lval), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used).
middle_rec__find_used_registers_instr(join_and_continue(Lval,_), !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used).

:- pred middle_rec__find_used_registers_components(
	list(pragma_c_component)::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_components([], !Used).
middle_rec__find_used_registers_components([Comp | Comps], !Used) :-
	middle_rec__find_used_registers_component(Comp, !Used),
	middle_rec__find_used_registers_components(Comps, !Used).

:- pred middle_rec__find_used_registers_component(pragma_c_component::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_component(pragma_c_inputs(In), !Used) :-
	insert_pragma_c_input_registers(In, !Used).
middle_rec__find_used_registers_component(pragma_c_outputs(Out), !Used) :-
	insert_pragma_c_output_registers(Out, !Used).
middle_rec__find_used_registers_component(pragma_c_user_code(_, _), !Used).
middle_rec__find_used_registers_component(pragma_c_raw_code(_, _), !Used).
middle_rec__find_used_registers_component(pragma_c_fail_to(_), !Used).
middle_rec__find_used_registers_component(pragma_c_noop, !Used).

:- pred middle_rec__find_used_registers_lvals(list(lval)::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_lvals([], !Used).
middle_rec__find_used_registers_lvals([Lval | Lvals], !Used) :-
	middle_rec__find_used_registers_lval(Lval, !Used),
	middle_rec__find_used_registers_lvals(Lvals, !Used).

:- pred middle_rec__find_used_registers_lval(lval::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_lval(Lval, !Used) :-
	( Lval = reg(r, N) ->
		copy(N, N1),
		set__insert(!.Used, N1, !:Used)
	; Lval = field(_, Rval, FieldNum) ->
		middle_rec__find_used_registers_rval(Rval, !Used),
		middle_rec__find_used_registers_rval(FieldNum, !Used)
	; Lval = lvar(_) ->
		error("lvar found in middle_rec__find_used_registers_lval")
	;
		true
	).

:- pred middle_rec__find_used_registers_rval(rval::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_rval(Rval, !Used) :-
	(
		Rval = lval(Lval),
		middle_rec__find_used_registers_lval(Lval, !Used)
	;
		Rval = var(_),
		error("var found in middle_rec__find_used_registers_rval")
	;
		Rval = mkword(_, Rval1),
		middle_rec__find_used_registers_rval(Rval1, !Used)
	;
		Rval = const(_)
	;
		Rval = unop(_, Rval1),
		middle_rec__find_used_registers_rval(Rval1, !Used)
	;
		Rval = binop(_, Rval1, Rval2),
		middle_rec__find_used_registers_rval(Rval1, !Used),
		middle_rec__find_used_registers_rval(Rval2, !Used)
	;
		Rval = mem_addr(MemRef),
		middle_rec__find_used_registers_mem_ref(MemRef, !Used)
	).

:- pred middle_rec__find_used_registers_mem_ref(mem_ref::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_mem_ref(stackvar_ref(_), !Used).
middle_rec__find_used_registers_mem_ref(framevar_ref(_), !Used).
middle_rec__find_used_registers_mem_ref(heap_ref(Rval, _, _), !Used) :-
	middle_rec__find_used_registers_rval(Rval, !Used).

:- pred middle_rec__find_used_registers_maybe_rvals(list(maybe(rval))::in,
	set(int)::in, set(int)::out) is det.

middle_rec__find_used_registers_maybe_rvals([], !Used).
middle_rec__find_used_registers_maybe_rvals([MaybeRval | MaybeRvals], !Used) :-
	(
		MaybeRval = no
	;
		MaybeRval = yes(Rval),
		middle_rec__find_used_registers_rval(Rval, !Used)
	),
	middle_rec__find_used_registers_maybe_rvals(MaybeRvals, !Used).

:- pred insert_pragma_c_input_registers(list(pragma_c_input)::in,
	set(int)::in, set(int)::out) is det.

insert_pragma_c_input_registers([], !Used).
insert_pragma_c_input_registers([Input|Inputs], !Used) :-
	Input = pragma_c_input(_, _, _, Rval, _),
	middle_rec__find_used_registers_rval(Rval, !Used),
	insert_pragma_c_input_registers(Inputs, !Used).

:- pred insert_pragma_c_output_registers(list(pragma_c_output)::in,
	set(int)::in, set(int)::out) is det.

insert_pragma_c_output_registers([], !Used).
insert_pragma_c_output_registers([Output|Outputs], !Used) :-
	Output = pragma_c_output(Lval, _, _, _, _),
	middle_rec__find_used_registers_lval(Lval, !Used),
	insert_pragma_c_output_registers(Outputs, !Used).

%---------------------------------------------------------------------------%

	% Find all the labels defined in an instruction sequence.

:- pred middle_rec__find_labels(list(instruction)::in, list(label)::out)
	is det.

middle_rec__find_labels(Instrs, Label2) :-
	middle_rec__find_labels_2(Instrs, [], Label2).

:- pred middle_rec__find_labels_2(list(instruction)::in,
	list(label)::in, list(label)::out) is det.

middle_rec__find_labels_2([], !Labels).
middle_rec__find_labels_2([Instr - _ | Instrs], !Labels) :-
	( Instr = label(Label) ->
		!:Labels = [Label | !.Labels]
	; Instr = block(_, _, Block) ->
		middle_rec__find_labels_2(Block, !Labels)
	;
		true
	),
	middle_rec__find_labels_2(Instrs, !Labels).
