%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Code generation - do middle recursion optimization
% Main authors: zs and conway.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module middle_rec.

:- interface.
:- import_module hlds, llds, code_info.

:- pred middle_rec__match_det(hlds__goal, hlds__goal, code_info, code_info).
:- mode middle_rec__match_det(in, out, in, out) is semidet.

:- pred middle_rec__gen_det(hlds__goal, code_tree, code_info, code_info).
:- mode middle_rec__gen_det(in, out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module code_gen, unify_gen, set, bintree_set, int.
:- import_module code_util, code_aux, opt_util, std_util, tree, list, require.

%---------------------------------------------------------------------------%

middle_rec__match_det(Goal, Switch, CodeInfo, CodeInfo) :-
	Goal = GoalExpr - GoalInfo,
	GoalExpr = switch(Var, Category, [Case1, Case2]),
	Category = deterministic,	% we are in trouble if this fails
	Case1 = case(ConsId1, Goal1),
	Case2 = case(ConsId2, Goal2),
	(
		code_aux__contains_only_builtins(Goal1)
	->
		code_aux__contains_simple_recursive_call(Goal2, CodeInfo, _),
		Switch = switch(Var, deterministic, [
			case(ConsId1, Goal1),
			case(ConsId2, Goal2)
		]) - GoalInfo
	;
		code_aux__contains_only_builtins(Goal2),
		code_aux__contains_simple_recursive_call(Goal1, CodeInfo, _),
		Switch = switch(Var, deterministic, [
			case(ConsId2, Goal2),
			case(ConsId1, Goal1)
		]) - GoalInfo
	).

middle_rec__gen_det(Goal, Instrs) -->
	(
		{ Goal = switch(Var, deterministic, [Case1, Case2])
			- SwitchGoalInfo},
		{ Case1 = case(_NonrecConsId, Base) },
		{ Case2 = case(RecConsId, Recursive) }
	->
		code_info__get_call_info(CallInfo),
		code_info__get_varset(VarSet),
		{ code_aux__explain_call_info(CallInfo, VarSet,
			CallInfoComment) },
		code_info__get_module_info(ModuleInfo),
		code_info__get_pred_id(PredId),
		code_info__get_proc_id(ProcId),
		{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
			Entry) },

		code_aux__pre_goal_update(SwitchGoalInfo),

		code_info__get_next_label(BaseLabel),
		code_info__push_failure_cont(known(BaseLabel)),
		unify_gen__generate_tag_test(Var, RecConsId, EntryTestCode),

		code_info__grab_code_info(CodeInfo),
		code_gen__generate_forced_det_goal(Base, BaseCode),
		code_info__slap_code_info(CodeInfo),
		code_gen__generate_forced_det_goal(Recursive, RecCode),
		{ tree__flatten(RecCode, RecListList) },
		{ list__condense(RecListList, RecList) },
		{ middle_rec__split_rec_code(RecList, BeforeList, AfterList) },

		{ tree__flatten(EntryTestCode, EntryTestListList) },
		{ list__condense(EntryTestListList, EntryTestList) },
		{ tree__flatten(BaseCode, BaseListList) },
		{ list__condense(BaseListList, BaseList) },

		{ middle_rec__generate_downloop_test(EntryTestList,
			Loop1Label, Loop1Test) },

		code_info__get_next_label(Loop1Label),
		code_info__get_next_label(Loop2Label),
		{ list__append(BeforeList, AfterList, RecCodeList) },
		{ middle_rec__find_unused_register(RecCodeList, CountReg) },
		code_info__get_total_stackslot_count(StackSlots),

		( { StackSlots = 0 } ->
			{ MaybeIncrSp = [] },
			{ MaybeDecrSp = [] }
		;
			{ MaybeIncrSp = [incr_sp(StackSlots) - ""] },
			{ MaybeDecrSp = [decr_sp(StackSlots) - ""] }
		),

		{ list__condense([
			[
				label(Entry) - "Procedure entry point",
				comment(CallInfoComment) - ""
			],
			EntryTestList,
			[
				assign(CountReg, const(int_const(0)))
					- "initialize counter register",
				label(Loop1Label) - "start of the down loop"
			],
			MaybeIncrSp,
			[
				assign(CountReg, binop((+), lval(CountReg),
					const(int_const(1)))) -
					"increment loop counter"
			],
			BeforeList,
			Loop1Test,
			BaseList,
			[
				label(Loop2Label) - ""
			],
			AfterList,
			MaybeDecrSp,
			[
				assign(CountReg, binop((-), lval(CountReg),
					const(int_const(1))))
					- "decrement loop counter",
				if_val(binop((>), lval(CountReg),
					const(int_const(0))), label(Loop2Label))
					- "test on upward loop",
				goto(succip) - "exit from recursive case",
				label(BaseLabel) - "start of base case"
			],
			BaseList,
			[
				goto(succip) - "exit from base case"
			]
		], InstrList) },
		{ Instrs = node(InstrList) }
	;
		{ error("middle_rec__gen_det match failed") }
	).

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
			error("middle_rec__generate_downloop_test: if_val followed by other instructions")
		),
		code_util__neg_rval(Test, NewTest),
		Instrs = [if_val(NewTest, label(Target)) - "test on downward loop"]
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
	( Instr0 = call(_, _) - _ ->
		(
			opt_util__skip_comments(Instrs1, Instrs2),
			Instrs2 = [Instr2 | Instrs3],
			Instr2 = label(_) - _
		->
			Before = [],
			After = Instrs3
		;
			error("call not followed by label in middle_rec__split_rec_code")
		)
	;
		middle_rec__split_rec_code(Instrs1, Before1, After),
		Before = [Instr0 | Before1]
	).

%---------------------------------------------------------------------------%

:- pred middle_rec__find_unused_register(list(instruction), lval).
:- mode middle_rec__find_unused_register(in, out) is det.

middle_rec__find_unused_register(Instrs, UnusedReg) :-
	set__init(Used0),
	middle_rec__find_used_registers(Instrs, Used0, Used1),
	set__to_sorted_list(Used1, UsedList),
	middle_rec__find_unused_register_2(UsedList, 1, UnusedReg).

:- pred middle_rec__find_unused_register_2(list(int), int, lval).
:- mode middle_rec__find_unused_register_2(in, in, out) is det.

middle_rec__find_unused_register_2([], N, reg(r(N))).
middle_rec__find_unused_register_2([H | T], N, Reg) :-
	( N < H ->
		Reg = reg(r(N))
	;
		N1 is N + 1,
		middle_rec__find_unused_register_2(T, N1, Reg)
	).

:- pred middle_rec__find_used_registers(list(instruction), set(int), set(int)).
:- mode middle_rec__find_used_registers(in, di, uo) is det.

middle_rec__find_used_registers([], Used, Used).
middle_rec__find_used_registers([Instr - _ | Instrs], Used0, Used) :-
	middle_rec__find_used_registers_instr(Instr, Used0, Used1),
	middle_rec__find_used_registers(Instrs, Used1, Used).

:- pred middle_rec__find_used_registers_instr(instr, set(int), set(int)).
:- mode middle_rec__find_used_registers_instr(in, di, uo) is det.

middle_rec__find_used_registers_instr(comment(_), Used, Used).
middle_rec__find_used_registers_instr(livevals(_, LvalSet), Used0, Used) :-
	bintree_set__to_sorted_list(LvalSet, LvalList),
	middle_rec__find_used_registers_lvals(LvalList, Used0, Used).
middle_rec__find_used_registers_instr(block(_, Instrs), Used0, Used) :-
	middle_rec__find_used_registers(Instrs, Used0, Used).
middle_rec__find_used_registers_instr(assign(Lval, Rval), Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used1),
	middle_rec__find_used_registers_rval(Rval, Used1, Used).
middle_rec__find_used_registers_instr(call(_, _), Used, Used).
middle_rec__find_used_registers_instr(mkframe(_, _, _), Used, Used).
middle_rec__find_used_registers_instr(modframe(_), Used, Used).
middle_rec__find_used_registers_instr(label(_), Used, Used).
middle_rec__find_used_registers_instr(goto(_), Used, Used).
middle_rec__find_used_registers_instr(computed_goto(Rval, _), Used0, Used) :-
	middle_rec__find_used_registers_rval(Rval, Used0, Used).
middle_rec__find_used_registers_instr(c_code(_), Used, Used).
middle_rec__find_used_registers_instr(if_val(Rval, _), Used0, Used) :-
	middle_rec__find_used_registers_rval(Rval, Used0, Used).
middle_rec__find_used_registers_instr(incr_sp(_), Used, Used).
middle_rec__find_used_registers_instr(decr_sp(_), Used, Used).
middle_rec__find_used_registers_instr(incr_hp(_), Used, Used).

:- pred middle_rec__find_used_registers_lvals(list(lval), set(int), set(int)).
:- mode middle_rec__find_used_registers_lvals(in, di, uo) is det.

middle_rec__find_used_registers_lvals([], Used, Used).
middle_rec__find_used_registers_lvals([Lval | Lvals], Used0, Used) :-
	middle_rec__find_used_registers_lval(Lval, Used0, Used1),
	middle_rec__find_used_registers_lvals(Lvals, Used1, Used).

:- pred middle_rec__find_used_registers_lval(lval, set(int), set(int)).
:- mode middle_rec__find_used_registers_lval(in, di, uo) is det.

middle_rec__find_used_registers_lval(Lval, Used0, Used) :-
	( Lval = reg(r(N)) ->
		set__insert(Used0, N, Used)
	; Lval = field(_, BaseLval, _) ->
		middle_rec__find_used_registers_lval(BaseLval, Used0, Used)
	; Lval = lvar(_) ->
		error("lvar found in middle_rec__find_used_registers_lval")
	;
		Used = Used0
	).

:- pred middle_rec__find_used_registers_rval(rval, set(int), set(int)).
:- mode middle_rec__find_used_registers_rval(in, di, uo) is det.

middle_rec__find_used_registers_rval(Rval, Used0, Used) :-
	(
		Rval = lval(Lval),
		middle_rec__find_used_registers_lval(Lval, Used0, Used)
	;
		Rval = var(_),
		error("var found in middle_rec__find_used_registers_rval")
	;
		Rval = create(_, MaybeRvals, _),
		middle_rec__find_used_registers_maybe_rvals(MaybeRvals, Used0, Used)
	;
		Rval = mkword(_, Rval1),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used)
	;
		Rval = field(_, Rval1, _),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used)
	;
		Rval = const(_),
		Used = Used0
	;
		Rval = unop(_, Rval1),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used)
	;
		Rval = binop(_, Rval1, Rval2),
		middle_rec__find_used_registers_rval(Rval1, Used0, Used1),
		middle_rec__find_used_registers_rval(Rval2, Used1, Used)
	).

:- pred middle_rec__find_used_registers_maybe_rvals(list(maybe(rval)),
	set(int), set(int)).
:- mode middle_rec__find_used_registers_maybe_rvals(in, di, uo) is det.

middle_rec__find_used_registers_maybe_rvals([], Used, Used).
middle_rec__find_used_registers_maybe_rvals([MaybeRval | MaybeRvals], Used0, Used) :-
	(
		MaybeRval = no,
		Used1 = Used0
	;
		MaybeRval = yes(Rval),
		middle_rec__find_used_registers_rval(Rval, Used0, Used1)
	),
	middle_rec__find_used_registers_maybe_rvals(MaybeRvals, Used1, Used).
