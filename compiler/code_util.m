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

:- pred code_util__make_local_label(module_info, pred_id, proc_id, int, label).
:- mode code_util__make_local_label(in, in, in, in, out) is det.

:- pred code_util__make_proc_label(module_info, pred_id, proc_id, proc_label).
:- mode code_util__make_proc_label(in, in, in, out) is det.

:- pred code_util__make_uni_label(module_info, type_id, int, proc_label).
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

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

:- pred code_util__neg_rval(rval, rval).
:- mode code_util__neg_rval(in, out) is det.

:- pred code_util__atom_to_binop(string, binary_op).
:- mode code_util__atom_to_binop(in, out) is semidet.
:- mode code_util__atom_to_binop(out, in) is semidet.

:- pred code_util__atom_to_unop(string, unary_op).
:- mode code_util__atom_to_unop(in, out) is semidet.
:- mode code_util__atom_to_unop(out, in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module list, hlds, map, std_util.
:- import_module type_util.

%---------------------------------------------------------------------------%

code_util__make_entry_label(ModuleInfo, PredId, ProcId, PredAddress) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	(
		pred_info_is_imported(PredInfo)
	->
		code_util__make_proc_label(ModuleInfo,
						PredId, ProcId, ProcLabel),
		PredAddress = imported(ProcLabel)
	;
		code_util__make_local_entry_label(ModuleInfo,
							PredId, ProcId, Label),
		PredAddress = label(Label)
	).

%---------------------------------------------------------------------------%

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	Label = exported(ProcLabel).

code_util__make_local_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	Label = local(ProcLabel, LabelNum).

%-----------------------------------------------------------------------------%

code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	ProcLabel = proc(ModuleName, PredName, Arity, ProcId).

code_util__make_uni_label(ModuleInfo, TypeId, UniModeNum, ProcLabel) :-
	type_util__type_id_module(ModuleInfo, TypeId, ModuleName),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	type_util__type_id_arity(ModuleInfo, TypeId, Arity),
	ProcLabel = unify_proc(ModuleName, TypeName, Arity, UniModeNum).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, r(ArgLoc)).

%-----------------------------------------------------------------------------%

code_util__atom_to_binop("builtin_plus", (+)).
code_util__atom_to_binop("builtin_minus", (-)).
code_util__atom_to_binop("builtin_times", (*)).
code_util__atom_to_binop("builtin_div", (/)).
code_util__atom_to_binop("builtin_mod", (mod)).
code_util__atom_to_binop("builtin_left_shift", (<<)).
code_util__atom_to_binop("builtin_right_shift", (>>)).
code_util__atom_to_binop("builtin_bit_and", (&)).
code_util__atom_to_binop("builtin_bit_or", (|)).
code_util__atom_to_binop("builtin_bit_xor", (^)).
code_util__atom_to_binop(">", (>)).
code_util__atom_to_binop("<", (<)).
code_util__atom_to_binop(">=", (>=)).
code_util__atom_to_binop("=<", (<=)).

code_util__atom_to_unop("builtin_bit_neg", bitwise_complement).

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

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

code_util__neg_rval(Rval, NegRval) :-
	( code_util__neg_rval_2(Rval, NegRval0) ->
		NegRval = NegRval0
	;
		NegRval = unop(not, Rval)
	).

:- pred code_util__neg_rval_2(rval, rval).
:- mode code_util__neg_rval_2(in, out) is semidet.

code_util__neg_rval_2(const(Const), const(NegConst)) :-
	( Const = true, NegConst = false
	; Const = false, NegConst = true ).
code_util__neg_rval_2(unop(not, Rval), Rval).
code_util__neg_rval_2(binop(Op, X, Y), binop(NegOp, X, Y)) :-
	code_util__neg_op(Op, NegOp).
	
:- pred code_util__neg_op(binary_op, binary_op).
:- mode code_util__neg_op(in, out) is semidet.

code_util__neg_op(eq, ne).
code_util__neg_op(ne, eq).
code_util__neg_op(<, >=).
code_util__neg_op(<=, >).
code_util__neg_op(>, <=).
code_util__neg_op(>=, <).
code_util__neg_op(str_eq, str_ne).
code_util__neg_op(str_ne, str_eq).
code_util__neg_op(str_lt, str_ge).
code_util__neg_op(str_le, str_gt).
code_util__neg_op(str_gt, str_le).
code_util__neg_op(str_ge, str_lt).

%-----------------------------------------------------------------------------%
