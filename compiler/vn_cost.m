%-----------------------------------------------------------------------------%

% Vn_cost.nl - predicates to discover the costs of instruction sequences.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_cost.

:- interface.

:- import_module llds, list, int, io.

:- pred vn__block_cost(list(instruction), int, io__state, io__state).
:- mode vn__block_cost(in, out, di, uo) is det.

:- pred vn__lval_cost(lval, int).
:- mode vn__lval_cost(in, out) is det.

:- pred vn__rval_cost(rval, int).
:- mode vn__rval_cost(in, out) is det.

:- implementation.

:- import_module vn_debug, require, string, std_util.

vn__block_cost(Instr, Cost) -->
	vn__block_cost_2(Instr, 0, Cost).

:- pred vn__block_cost_2(list(instruction), int, int,
	io__state, io__state).
:- mode vn__block_cost_2(in, in, out, di, uo) is det.

vn__block_cost_2([], Cost, Cost) --> [].
vn__block_cost_2([Instr | Instrs], CostBefore, Cost) -->
	{ Instr = Uinstr - _ },
	{ vn__instr_cost(Uinstr, InstrCost) },
	{ Uinstr = if_val(_, _) ->
		% We can now count earlier instructions twice
		% to favor code sequences that move code after ifs.
		% Code that saves common subexpressions in the Rval
		% before the if is now handled specially.
		CostNow1 is CostBefore + InstrCost,
		CostNow is 2 * CostNow1
	;
		CostNow is CostBefore + InstrCost
	},
	vn__cost_detail_msg(Uinstr, InstrCost, CostNow),
	vn__block_cost_2(Instrs, CostNow, Cost).

:- pred vn__instr_cost(instr, int).
:- mode vn__instr_cost(in, out) is det.

vn__instr_cost(Uinstr, Cost) :-
	vn__costof_assign(AssignCost),
	(
		Uinstr = comment(_),
		Cost = 0
	;
		Uinstr = livevals(_),
		Cost = 0
	;
		Uinstr = block(_, _),
		error("block found in vn_block_cost")
	;
		Uinstr = assign(Lval, Rval),
		vn__lval_cost(Lval, LvalCost),
		vn__rval_cost(Rval, RvalCost),
		(
			% Is this an assignment that speeds up future accesses?
			% If yes, do not count a cost for the assignment.
			% Basically, assignments to registers are free
			% unless they merely shuffle registers around.
			LvalCost = 0,
			Rval = lval(_),
			RvalCost > 0
		->
			Cost = RvalCost
		;
			% Tagging a value has the same cost as the assignment
			% itself, so don't count this cost twice.
			Rval = mkword(_, _)
		->
			Cost is RvalCost + LvalCost
		;
			Cost1 is RvalCost + LvalCost,
			Cost is Cost1 + AssignCost
		)
	;
		Uinstr = call(_, _, _, _),
		Cost = 0
	;
		Uinstr = call_closure(_, _, _),
		Cost = 0
	;
		Uinstr = mkframe(_, _, _),
		Cost = 0
	;
		Uinstr = modframe(_),
		Cost = 0
	;
		Uinstr = label(_),
		Cost = 0
	;
		Uinstr = goto(_, _),
		Cost = 0
	;
		Uinstr = computed_goto(Rval, _),
		vn__rval_cost(Rval, RvalCost),
		Cost = RvalCost
	;
		Uinstr = c_code(_),
		error("c_code found in vn_block_cost")
	;
		Uinstr = if_val(Rval, _),
		vn__rval_cost(Rval, RvalCost),
		Cost = RvalCost
	;
		Uinstr = incr_hp(Lval, MaybeTag, Rval),
		vn__lval_cost(Lval, LvalCost),
		vn__rval_cost(Rval, RvalCost),
		Cost1 is RvalCost + LvalCost,
		Cost2 is 3 * AssignCost,
		Cost12 is Cost1 + Cost2,
		(
			MaybeTag = yes(_),
			Cost3 = 1
		;
			MaybeTag = no,
			Cost3 = 0
		),
		Cost is Cost12 + Cost3
	;
		Uinstr = mark_hp(Lval),
		vn__lval_cost(Lval, LvalCost),
		Cost = LvalCost
	;
		Uinstr = restore_hp(Rval),
		vn__rval_cost(Rval, RvalCost),
		Cost = RvalCost
	;
		Uinstr = incr_sp(_),
		Cost = 0
	;
		Uinstr = decr_sp(_),
		Cost = 0
	).

vn__lval_cost(Lval, Cost) :-
	vn__costof_stackref(StackrefCost),
	vn__costof_heapref(HeaprefCost),
	(
		Lval = reg(_),
		Cost = 0
	;
		Lval = stackvar(_),
		Cost = StackrefCost
	;
		Lval = framevar(_),
		Cost = StackrefCost
	;
		Lval = succip,
		Cost = 0
	;
		Lval = maxfr,
		Cost = 0
	;
		Lval = curfr,
		Cost = 0
	;
		Lval = redoip(Rval1),
		vn__rval_cost(Rval1, RvalCost1),
		Cost is RvalCost1 + StackrefCost
	;
		Lval = hp,
		Cost = 0
	;
		Lval = sp,
		Cost = 0
	;
		Lval = field(_, Rval1, Rval2),
		vn__rval_cost(Rval1, RvalCost1),
		vn__rval_cost(Rval2, RvalCost2),
		Cost1 is RvalCost1 + RvalCost2,
		Cost is Cost1 + HeaprefCost
	;
		Lval = lvar(_),
		error("lvar found in lval_cost")
	;
		Lval = temp(_),
		Cost = 0
	).

vn__rval_cost(Rval, Cost) :-
	vn__costof_ops(OpsCost),
	(
		Rval = lval(Lval),
		vn__lval_cost(Lval, LvalCost),
		Cost = LvalCost
	;
		Rval = var(_),
		error("var found in rval_cost")
	;
		Rval = create(_, _, _),
		Cost = 0
	;
		Rval = mkword(_, Rval1),
		vn__rval_cost(Rval1, RvalCost1),
		Cost is RvalCost1 + OpsCost
	;
		Rval = const(_),
		Cost = 0
	;
		Rval = unop(_, Rval1),
		vn__rval_cost(Rval1, RvalCost1),
		Cost is RvalCost1 + OpsCost
	;
		Rval = binop(_, Rval1, Rval2),
		vn__rval_cost(Rval1, RvalCost1),
		vn__rval_cost(Rval2, RvalCost2),
		Cost1 is RvalCost1 + RvalCost2,
		Cost is Cost1 + OpsCost
	).

:- pred vn__costof_assign(int).
:- mode vn__costof_assign(out) is det.

:- pred vn__costof_ops(int).
:- mode vn__costof_ops(out) is det.

:- pred vn__costof_stackref(int).
:- mode vn__costof_stackref(out) is det.

:- pred vn__costof_heapref(int).
:- mode vn__costof_heapref(out) is det.

vn__costof_assign(1).
vn__costof_ops(1).
vn__costof_stackref(2).
vn__costof_heapref(2).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
