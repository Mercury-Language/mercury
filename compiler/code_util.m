%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_util.

:- interface.

:- import_module list, string.
:- import_module hlds, llds.

:- pred code_util__make_entry_label(module_info, pred_id, proc_id, code_addr).
:- mode code_util__make_entry_label(in, in, in, out) is det.

:- pred code_util__make_local_entry_label(module_info, pred_id, proc_id, label).
:- mode code_util__make_local_entry_label(in, in, in, out) is det.

:- pred code_util__make_nonlocal_entry_label(module_info,
						pred_id, proc_id, code_addr).
:- mode code_util__make_nonlocal_entry_label(in, in, in, out) is det.

:- pred code_util__make_local_label(module_info, pred_id, proc_id, int, label).
:- mode code_util__make_local_label(in, in, in, in, out) is det.

:- pred code_util__make_uni_label(module_info, type_id, int, unilabel).
:- mode code_util__make_uni_label(in, in, in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, reg).
:- mode code_util__arg_loc_to_register(in, out) is det.

	% Determine whether a goal might allocate some heap space,
	% i.e. whether it contains any construction unifications
	% or predicate calls.

:- pred code_util__goal_may_allocate_heap(hlds__goal).
:- mode code_util__goal_may_allocate_heap(in) is semidet.

:- pred code_util__goal_list_may_allocate_heap(list(hlds__goal)).
:- mode code_util__goal_list_may_allocate_heap(in) is semidet.

	% Check whether an instruction can possibly branch away.

:- pred code_util__can_instr_branch_away(instr, bool).
:- mode code_util__can_instr_branch_away(in, out) is det.

	% Check whether an instruction can possibly fall through
	% to the next instruction without using its label.

:- pred code_util__can_instr_fall_through(instr, bool).
:- mode code_util__can_instr_fall_through(in, out) is det.

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

:- pred code_util__neg_rval(rval, rval).
:- mode code_util__neg_rval(in, out) is det.

:- pred atom_to_operator(string, operator).
:- mode atom_to_operator(in, out) is semidet.
:- mode atom_to_operator(out, in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module list, hlds, map, std_util.
:- import_module type_util.

%---------------------------------------------------------------------------%

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = entrylabel(ModuleName, PredName, Arity, ProcId).

code_util__make_nonlocal_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = nonlocal(ModuleName, PredName, Arity, ProcId).

code_util__make_local_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = label(ModuleName, PredName, Arity, ProcId, LabelNum).

code_util__make_entry_label(ModuleInfo, PredId, ProcId, PredAddress) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	(
		pred_info_is_imported(PredInfo)
	->
		code_util__make_nonlocal_entry_label(ModuleInfo,
						PredId, ProcId, PredAddress)
	;
		code_util__make_local_entry_label(ModuleInfo,
							PredId, ProcId, Label),
		PredAddress = local(Label)
	).

%-----------------------------------------------------------------------------%

code_util__make_uni_label(ModuleInfo, TypeId, UniModeNum, Label) :-
	type_util__type_id_module(ModuleInfo, TypeId, ModuleName),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	type_util__type_id_arity(ModuleInfo, TypeId, Arity),
	Label = unilabel(ModuleName, TypeName, Arity, UniModeNum).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, r(ArgLoc)).

%-----------------------------------------------------------------------------%

atom_to_operator("builtin_plus", (+)).
atom_to_operator("builtin_minus", (-)).
atom_to_operator("builtin_times", (*)).
atom_to_operator("builtin_div", (/)).
atom_to_operator("builtin_mod", (mod)).
atom_to_operator(">", (>)).
atom_to_operator("<", (<)).
atom_to_operator(">=", (>=)).
atom_to_operator("=<", (<=)).

%-----------------------------------------------------------------------------%

	% This code may _look_ nondeterministic, but it's really semidet,
	% and Mercury is smart enough to know this.

code_util__goal_may_allocate_heap(Goal - _GoalInfo) :-
	code_util__goal_may_allocate_heap_2(Goal).

:- pred code_util__goal_may_allocate_heap_2(hlds__goal_expr).
:- mode code_util__goal_may_allocate_heap_2(in) is semidet.

code_util__goal_may_allocate_heap_2(call(_, _, _, not_builtin, _, _)).
code_util__goal_may_allocate_heap_2(unify(_, _, _, construct(_,_,Args,_), _)) :-
	Args = [_|_].
code_util__goal_may_allocate_heap_2(some(_Vars, Goal)) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_may_allocate_heap_2(not(_Vars, Goal)) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_may_allocate_heap_2(conj(Goals)) :-
	code_util__goal_list_may_allocate_heap(Goals).
code_util__goal_may_allocate_heap_2(disj(Goals)) :-
	code_util__goal_list_may_allocate_heap(Goals).
code_util__goal_may_allocate_heap_2(switch(_Var, _Det, Cases)) :-
	code_util__cases_may_allocate_heap(Cases).
code_util__goal_may_allocate_heap_2(if_then_else(_Vars, A, B, C)) :-
	(
		code_util__goal_may_allocate_heap(A)
	;	
		code_util__goal_may_allocate_heap(B)
	;
		code_util__goal_may_allocate_heap(C)
	).

:- pred code_util__cases_may_allocate_heap(list(case)).
:- mode code_util__cases_may_allocate_heap(in) is semidet.

code_util__cases_may_allocate_heap([case(_, Goal) | _]) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__cases_may_allocate_heap([_ | Cases]) :-
	code_util__cases_may_allocate_heap(Cases).

code_util__goal_list_may_allocate_heap([Goal | _]) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_list_may_allocate_heap([_ | Goals]) :-
	code_util__goal_list_may_allocate_heap(Goals).

%-----------------------------------------------------------------------------%

code_util__can_instr_branch_away(comment(_), no).
code_util__can_instr_branch_away(assign(_, _), no).
code_util__can_instr_branch_away(call(_, _), yes).
code_util__can_instr_branch_away(entrycall(_, _), yes).
code_util__can_instr_branch_away(unicall(_, _), yes).
code_util__can_instr_branch_away(tailcall(_), yes).
code_util__can_instr_branch_away(proceed, yes).
code_util__can_instr_branch_away(succeed, yes).
code_util__can_instr_branch_away(fail, yes).
code_util__can_instr_branch_away(redo, yes).
code_util__can_instr_branch_away(mkframe(_, _, _), no).
code_util__can_instr_branch_away(modframe(_), no).
code_util__can_instr_branch_away(label(_), no).
code_util__can_instr_branch_away(unilabel(_), no).
code_util__can_instr_branch_away(goto(_), yes).
code_util__can_instr_branch_away(c_code(_), no).
code_util__can_instr_branch_away(if_val(_, _), yes).
code_util__can_instr_branch_away(incr_sp(_), no).
code_util__can_instr_branch_away(decr_sp(_), no).
code_util__can_instr_branch_away(incr_hp(_), no).

code_util__can_instr_fall_through(comment(_), yes).
code_util__can_instr_fall_through(assign(_, _), yes).
code_util__can_instr_fall_through(call(_, _), no).
code_util__can_instr_fall_through(entrycall(_, _), no).
code_util__can_instr_fall_through(unicall(_, _), no).
code_util__can_instr_fall_through(tailcall(_), no).
code_util__can_instr_fall_through(proceed, no).
code_util__can_instr_fall_through(succeed, no).
code_util__can_instr_fall_through(fail, no).
code_util__can_instr_fall_through(redo, no).
code_util__can_instr_fall_through(mkframe(_, _, _), yes).
code_util__can_instr_fall_through(modframe(_), yes).
code_util__can_instr_fall_through(label(_), yes).
code_util__can_instr_fall_through(unilabel(_), yes).
code_util__can_instr_fall_through(goto(_), no).
code_util__can_instr_fall_through(c_code(_), yes).
code_util__can_instr_fall_through(if_val(_, _), yes).
code_util__can_instr_fall_through(incr_sp(_), yes).
code_util__can_instr_fall_through(decr_sp(_), yes).
code_util__can_instr_fall_through(incr_hp(_), yes).

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

code_util__neg_rval(Rval, NegRval) :-
	( code_util__neg_rval_2(Rval, NegRval0) ->
		NegRval = NegRval0
	;
		NegRval = not(Rval)
	).

:- pred code_util__neg_rval_2(rval, rval).
:- mode code_util__neg_rval_2(in, out) is semidet.

code_util__neg_rval_2(not(Rval), Rval).
code_util__neg_rval_2(binop(Op, X, Y), binop(NegOp, X, Y)) :-
	code_util__neg_op(Op, NegOp).
code_util__neg_rval_2(false, true).
code_util__neg_rval_2(true, false).
	
:- pred code_util__neg_op(operator, operator).
:- mode code_util__neg_op(in, out) is semidet.

code_util__neg_op(eq, ne).
code_util__neg_op(ne, eq).
code_util__neg_op(<, >=).
code_util__neg_op(<=, >).
code_util__neg_op(>, <=).
code_util__neg_op(>=, <).

%-----------------------------------------------------------------------------%
