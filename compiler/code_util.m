%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% file: code_util.m.
%
% various utilities routines for code generation and recognition
% of builtins.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_util.

:- interface.

:- import_module list, string.
:- import_module hlds, llds.

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

:- pred code_util__negate_the_test(list(instruction), list(instruction)).
:- mode code_util__negate_the_test(in, out) is det.

:- pred code_util__compiler_generated(pred_info).
:- mode code_util__compiler_generated(in) is semidet.

:- pred code_util__predinfo_is_builtin(module_info, pred_info).
:- mode code_util__predinfo_is_builtin(in, in) is semidet.

:- pred code_util__is_builtin(module_info, pred_id, proc_id, is_builtin).
:- mode code_util__is_builtin(in, in, in, out) is det.

:- pred code_util__builtin_binop(string, int, binary_op).
:- mode code_util__builtin_binop(in, in, out) is semidet.
:- mode code_util__builtin_binop(out, out, in) is semidet.

:- pred code_util__builtin_unop(string, int, unary_op).
:- mode code_util__builtin_unop(in, in, out) is semidet.
:- mode code_util__builtin_unop(out, out, in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module type_util, special_pred, list, map, require, std_util.

%---------------------------------------------------------------------------%

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	(
		( pred_info_is_exported(PredInfo)
		; pred_info_is_pseudo_exported(PredInfo),
		  % only the (in, in) mode of a unification is exported
		  ProcId = 0
		)
	->
		Label = exported(ProcLabel)
	;
		Label = local(ProcLabel)
	).

code_util__make_local_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	Label = local(ProcLabel, LabelNum).

%-----------------------------------------------------------------------------%

code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	(
		string__prefix(PredName, "__"),
		\+ string__prefix(PredName, "__LambdaGoal__")
	->
		module_info_preds(ModuleInfo, Preds),
		map__lookup(Preds, PredId, PredInfo),
		pred_info_arg_types(PredInfo, _TypeVarSet, ArgTypes),
		(
			special_pred_get_type(PredName, ArgTypes, Type),
			type_to_type_id(Type, TypeId0, _)
		->
			TypeId = TypeId0
		;
			string__append_list(["code_util__make_proc_label:\n",
				"cannot make label for special pred `",
				PredName, "'"], ErrorMessage),
			error(ErrorMessage)
		),
		type_util__type_id_name(ModuleInfo, TypeId, TypeName),
		type_util__type_id_arity(ModuleInfo, TypeId, Arity),
		ProcLabel = special_proc(ModuleName, PredName, TypeName,
				Arity, ProcId)
	;
		predicate_arity(ModuleInfo, PredId, Arity),
		ProcLabel = proc(ModuleName, PredName, Arity, ProcId)
	).

code_util__make_uni_label(ModuleInfo, TypeId, UniModeNum, ProcLabel) :-
	type_util__type_id_module(ModuleInfo, TypeId, ModuleName),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	type_util__type_id_arity(ModuleInfo, TypeId, Arity),
	ProcLabel = special_proc(ModuleName, "__Unify__", TypeName, Arity,
				UniModeNum).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, r(ArgLoc)).

%-----------------------------------------------------------------------------%

code_util__predinfo_is_builtin(_ModuleInfo, PredInfo) :-
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity),
	code_util__builtin(PredName, Arity).

code_util__is_builtin(ModuleInfo, PredId0, _PredMode0, IsBuiltin) :-
	predicate_name(ModuleInfo, PredId0, PredName),
	predicate_arity(ModuleInfo, PredId0, Arity),
	(
		(
			code_util__builtin_binop(PredName, Arity, _)
		;
			code_util__builtin_unop(PredName, Arity, _)
		)
	->
		hlds__is_builtin_make_builtin(yes, yes, IsBuiltin)
	;
		PredName = "call"
	->
		hlds__is_builtin_make_builtin(yes, no, IsBuiltin)
	;
		hlds__is_builtin_make_builtin(no, no, IsBuiltin)
	).

	% XXX module qualifiers

:- pred code_util__builtin(string, int).
:- mode code_util__builtin(in, in) is semidet.

code_util__builtin(PredName, Arity) :-
	( code_util__builtin_binop(PredName, Arity, _)
	; code_util__builtin_unop(PredName, Arity, _)
	; PredName = "call"
	).

code_util__builtin_binop("builtin_plus", 3, (+)).
code_util__builtin_binop("builtin_minus", 3, (-)).
code_util__builtin_binop("builtin_times", 3, (*)).
code_util__builtin_binop("builtin_div", 3, (/)).
code_util__builtin_binop("builtin_mod", 3, (mod)).
code_util__builtin_binop("builtin_left_shift", 3, (<<)).
code_util__builtin_binop("builtin_right_shift", 3, (>>)).
code_util__builtin_binop("builtin_bit_and", 3, (&)).
code_util__builtin_binop("builtin_bit_or", 3, ('|')).
			% Need single quotes around '|' for Sicstus Prolog
code_util__builtin_binop("builtin_bit_xor", 3, (^)).
code_util__builtin_binop(">", 2, (>)).
code_util__builtin_binop("<", 2, (<)).
code_util__builtin_binop(">=", 2, (>=)).
code_util__builtin_binop("=<", 2, (<=)).
code_util__builtin_binop("builtin_float_plus", 3, float_plus).
code_util__builtin_binop("builtin_float_minus", 3, float_minus).
code_util__builtin_binop("builtin_float_times", 3, float_times).
code_util__builtin_binop("builtin_float_divide", 3, float_divide).
code_util__builtin_binop("builtin_float_gt", 2, float_gt).
code_util__builtin_binop("builtin_float_lt", 2, float_lt).
code_util__builtin_binop("builtin_float_ge", 2, float_ge).
code_util__builtin_binop("builtin_float_le", 2, float_le).

code_util__builtin_unop("builtin_bit_neg", 2, bitwise_complement).

%-----------------------------------------------------------------------------%

	% code_util__compiler_generated(PredInfo) should succeed iff
	% the PredInfo is for a compiler generated predicate.

code_util__compiler_generated(PredInfo) :-
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	( PredName = "__Unify__", PredArity = 2
	; PredName = "__Compare__", PredArity = 3
	; PredName = "__Index__", PredArity = 2
	; PredName = "__Term_To_Type__", PredArity = 2
	; PredName = "__Type_To_Term__", PredArity = 2
	).

%-----------------------------------------------------------------------------%

	% This code may _look_ nondeterministic, but it's really semidet,
	% and Mercury is smart enough to know this.

code_util__goal_may_allocate_heap(Goal - _GoalInfo) :-
	code_util__goal_may_allocate_heap_2(Goal).

:- pred code_util__goal_may_allocate_heap_2(hlds__goal_expr).
:- mode code_util__goal_may_allocate_heap_2(in) is semidet.

code_util__goal_may_allocate_heap_2(call(_, _, _, Builtin, _, _, _)) :-
	\+ hlds__is_builtin_is_inline(Builtin).
code_util__goal_may_allocate_heap_2(unify(_, _, _, construct(_,_,Args,_), _)) :-
	Args = [_|_].
code_util__goal_may_allocate_heap_2(some(_Vars, Goal)) :-
	code_util__goal_may_allocate_heap(Goal).
code_util__goal_may_allocate_heap_2(not(Goal)) :-
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
	(
		Const = true, NegConst = false
	;
		Const = false, NegConst = true
	).
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
code_util__neg_op(float_eq, float_ne).
code_util__neg_op(float_ne, float_eq).
code_util__neg_op(float_lt, float_ge).
code_util__neg_op(float_le, float_gt).
code_util__neg_op(float_gt, float_le).
code_util__neg_op(float_ge, float_lt).

code_util__negate_the_test([], _) :-
	error("code_util__negate_the_test on empty list").
code_util__negate_the_test([Instr0 | Instrs0], Instrs) :-
	( Instr0 = if_val(Test, Target) - Comment ->
		code_util__neg_rval(Test, NewTest),
		Instrs = [if_val(NewTest, Target) - Comment]
	;
		code_util__negate_the_test(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%
