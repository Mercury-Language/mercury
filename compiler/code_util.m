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

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_data, llds.
:- import_module list.

	% Create a code address which holds the address of the specified
	% procedure.
	% The fourth argument should be `no' if the the caller wants the
	% returned address to be valid from everywhere in the program.
	% If being valid from within the current procedure is enough,
	% this argument should be `yes' wrapped around the value of the
	% --procs-per-c-function option and the current procedure id.
	% Using an address that is only valid from within the current
	% procedure may make jumps more efficient.

:- pred code_util__make_entry_label(module_info, pred_id, proc_id, 
	maybe(pair(int, pred_proc_id)), code_addr).
:- mode code_util__make_entry_label(in, in, in, in, out) is det.

	% Create a label which holds the address of the specified procedure,
	% which must be defined in the current module (procedures that are
	% imported from other modules have representations only as code_addrs,
	% not as labels, since their address is not known at C compilation
	% time).
	% The fourth argument has the same meaning as for
	% code_util__make_entry_label.

:- pred code_util__make_local_entry_label(module_info, pred_id, proc_id,
	maybe(pair(int, pred_proc_id)), label).
:- mode code_util__make_local_entry_label(in, in, in, in, out) is det.

	% Create a label internal to a Mercury procedure.
:- pred code_util__make_internal_label(module_info, pred_id, proc_id, int,
	label).
:- mode code_util__make_internal_label(in, in, in, in, out) is det.

:- pred code_util__make_proc_label(module_info, pred_id, proc_id, proc_label).
:- mode code_util__make_proc_label(in, in, in, out) is det.

:- pred code_util__make_uni_label(module_info, type_id, int, proc_label).
:- mode code_util__make_uni_label(in, in, in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, reg).
:- mode code_util__arg_loc_to_register(in, out) is det.

	% Determine whether a goal might allocate some heap space,
	% i.e. whether it contains any construction unifications
	% or predicate calls.  BEWARE that this predicate is only
	% an approximation, used to decide whether or not to try to
	% reclaim the heap space; currently it fails even for some
	% goals which do allocate heap space, such as construction
	% of boxed constants.

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

	% Given a module name, a predicate name, a proc_id and a list of
	% variables as the arguments, find out if that procedure of that
	% predicate is a builtin. If yes, the last two arguments
	% return two things:
	%
	% - an rval to execute as a test if the builtin is semidet; and
	%
	% - an rval to assign to a variable if the builtin calls for this.
	%
	% At least one of these will be present.
	%
	% Each rval returned is guaranteed to be either a unop or a binop,
	% applied to arguments that are either variables (from the argument
	% list) or constants.

:- pred code_util__translate_builtin(string, string, proc_id, list(var),
	maybe(rval), maybe(pair(var, rval))).
:- mode code_util__translate_builtin(in, in, in, in, out, out) is semidet.

	% Find out how a function symbol (constructor) is represented
	% in the given type.

:- pred code_util__cons_id_to_tag(cons_id, type, module_info, cons_tag).
:- mode code_util__cons_id_to_tag(in, in, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module prog_data, type_util, special_pred.
:- import_module bool, char, int, string, map, varset, require, std_util.

%---------------------------------------------------------------------------%

code_util__make_entry_label(ModuleInfo, PredId, ProcId, Immed, PredAddress) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	(
		(
			pred_info_is_imported(PredInfo)
		;
			pred_info_is_pseudo_imported(PredInfo),
			% only the (in, in) mode of unification is imported
			ProcId = 0
		)
	->
		code_util__make_proc_label(ModuleInfo, PredId, ProcId,
			ProcLabel),
		PredAddress = imported(ProcLabel)
	;
		code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
			Immed, Label),
		PredAddress = label(Label)
	).

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Immed, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	(
		(
			pred_info_is_exported(PredInfo)
		;
			pred_info_is_pseudo_exported(PredInfo),
			% only the (in, in) mode of a unification is exported
			ProcId = 0
		)
	->
		(
			Immed = no,
			Label = exported(ProcLabel)
		;
			Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)),
			choose_local_label_type(ProcsPerFunc, CurPredId,
				CurProcId, PredId, ProcId, ProcLabel, Label)
		)
	;
		(
			% If we want to define the label or use it to put it
			% into a data structure, a label that is usable only
			% within the current C module won't do.
			Immed = no,
			Label = local(ProcLabel)
		;
			Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)),
			choose_local_label_type(ProcsPerFunc, CurPredId,
				CurProcId, PredId, ProcId, ProcLabel, Label)
		)
	).


:- pred choose_local_label_type(int, pred_id, proc_id,
		pred_id, proc_id, proc_label, label).
:- mode choose_local_label_type(in, in, in, in, in, in, out) is det.

choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
		PredId, ProcId, ProcLabel, Label) :-
	(
		% If we want to branch to the label now,
		% we prefer a form that are usable only within
		% the current C module, since it is likely
		% to be faster.
		(
			ProcsPerFunc = 0
		;
			PredId = CurPredId,
			ProcId = CurProcId
		)
	->
		Label = c_local(ProcLabel)
	;
		Label = local(ProcLabel)
	).

%-----------------------------------------------------------------------------%

code_util__make_internal_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	Label = local(ProcLabel, LabelNum).

code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	(
		code_util__compiler_generated(PredInfo)
	->
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
		TypeId = TypeName - Arity,
		ProcLabel = special_proc(ModuleName, PredName, TypeName,
				Arity, ProcId)
	;
		predicate_arity(ModuleInfo, PredId, Arity),
		ProcLabel = proc(ModuleName, PredName, Arity, ProcId)
	).

code_util__make_uni_label(ModuleInfo, TypeId, UniModeNum, ProcLabel) :-
	module_info_name(ModuleInfo, ModuleName),
	TypeId = TypeName - Arity,
	ProcLabel = special_proc(ModuleName, "__Unify__", TypeName, Arity,
				UniModeNum).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, r(ArgLoc)).

%-----------------------------------------------------------------------------%

code_util__predinfo_is_builtin(_ModuleInfo, PredInfo) :-
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
%	code_util__translate_builtin(ModuleName, PredName, _, _, _, _).
	pred_info_arity(PredInfo, Arity),
	( code_util__builtin(ModuleName, PredName, 0, Arity)
	; code_util__builtin(ModuleName, PredName, 10000, Arity)
	).

code_util__is_builtin(ModuleInfo, PredId0, ProcId, IsBuiltin) :-
	predicate_module(ModuleInfo, PredId0, ModuleName),
	predicate_name(ModuleInfo, PredId0, PredName),
	predicate_arity(ModuleInfo, PredId0, Arity),
	(
		code_util__builtin(ModuleName, PredName, ProcId, Arity)
	->
		hlds__is_builtin_make_builtin(yes, yes, IsBuiltin)
	;
		hlds__is_builtin_make_builtin(no, no, IsBuiltin)
	).

:- pred code_util__builtin(string, string, proc_id, int).
:- mode code_util__builtin(in, in, in, in) is semidet.

code_util__builtin(ModuleName, PredName, ProcId, Arity) :-
	Arity =< 3,
	varset__init(VarSet),
	varset__new_vars(VarSet, Arity, Args, _),
	code_util__translate_builtin(ModuleName, PredName, ProcId, Args, _, _).

code_util__translate_builtin("mercury_builtin", "builtin_int_gt", 0, [X, Y],
	yes(binop((>), var(X), var(Y))), no).
code_util__translate_builtin("mercury_builtin", "builtin_int_lt", 0, [X, Y],
	yes(binop((<), var(X), var(Y))), no).

code_util__translate_builtin("int", "builtin_plus", 10000, [X, Y, Z],
	no, yes(Z - binop((+), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_plus", 10001, [X, Y, Z],
	no, yes(X - binop((-), var(Z), var(Y)))).
code_util__translate_builtin("int", "builtin_plus", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(Z), var(X)))).
code_util__translate_builtin("int", "+", 10000, [X, Y, Z],
	no, yes(Z - binop((+), var(X), var(Y)))).
code_util__translate_builtin("int", "+", 10001, [X, Y, Z],
	no, yes(X - binop((-), var(Z), var(Y)))).
code_util__translate_builtin("int", "+", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(Z), var(X)))).
code_util__translate_builtin("int", "builtin_minus", 10000, [X, Y, Z],
	no, yes(Z - binop((-), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_minus", 10001, [X, Y, Z],
	no, yes(X - binop((+), var(Y), var(Z)))).
code_util__translate_builtin("int", "builtin_minus", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(X), var(Z)))).
code_util__translate_builtin("int", "-", 10000, [X, Y, Z],
	no, yes(Z - binop((-), var(X), var(Y)))).
code_util__translate_builtin("int", "-", 10001, [X, Y, Z],
	no, yes(X - binop((+), var(Y), var(Z)))).
code_util__translate_builtin("int", "-", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(X), var(Z)))).
code_util__translate_builtin("int", "builtin_times", 10000, [X, Y, Z],
	no, yes(Z - binop((*), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_times", 10001, [X, Y, Z],
	no, yes(X - binop((/), var(Z), var(Y)))).
code_util__translate_builtin("int", "builtin_times", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(Z), var(X)))).
code_util__translate_builtin("int", "*", 10000, [X, Y, Z],
	no, yes(Z - binop((*), var(X), var(Y)))).
code_util__translate_builtin("int", "*", 10001, [X, Y, Z],
	no, yes(X - binop((/), var(Z), var(Y)))).
code_util__translate_builtin("int", "*", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(Z), var(X)))).
code_util__translate_builtin("int", "builtin_div", 10000, [X, Y, Z],
	no, yes(Z - binop((/), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_div", 10001, [X, Y, Z],
	no, yes(X - binop((*), var(Y), var(Z)))).
code_util__translate_builtin("int", "builtin_div", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(X), var(Z)))).
code_util__translate_builtin("int", "//", 10000, [X, Y, Z],
	no, yes(Z - binop((/), var(X), var(Y)))).
code_util__translate_builtin("int", "//", 10001, [X, Y, Z],
	no, yes(X - binop((*), var(Y), var(Z)))).
code_util__translate_builtin("int", "//", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(X), var(Z)))).
code_util__translate_builtin("int", "builtin_mod", 10000, [X, Y, Z],
	no, yes(Z - binop((mod), var(X), var(Y)))).
code_util__translate_builtin("int", "mod", 10000, [X, Y, Z],
	no, yes(Z - binop((mod), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_left_shift", 10000, [X, Y, Z],
	no, yes(Z - binop((<<), var(X), var(Y)))).
code_util__translate_builtin("int", "<<", 10000, [X, Y, Z],
	no, yes(Z - binop((<<), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_right_shift", 10000, [X, Y, Z],
	no, yes(Z - binop((>>), var(X), var(Y)))).
code_util__translate_builtin("int", ">>", 10000, [X, Y, Z],
	no, yes(Z - binop((>>), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_bit_and", 10000, [X, Y, Z],
	no, yes(Z - binop((&), var(X), var(Y)))).
code_util__translate_builtin("int", "/\\", 10000, [X, Y, Z],
	no, yes(Z - binop((&), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_bit_or", 10000, [X, Y, Z],
	no, yes(Z - binop(('|'), var(X), var(Y)))).
code_util__translate_builtin("int", "\\/", 10000, [X, Y, Z],
	no, yes(Z - binop(('|'), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_bit_xor", 10000, [X, Y, Z],
	no, yes(Z - binop((^), var(X), var(Y)))).
code_util__translate_builtin("int", "^", 10000, [X, Y, Z],
	no, yes(Z - binop((^), var(X), var(Y)))).
code_util__translate_builtin("int", "builtin_unary_plus", 10000, [X, Y],
	no, yes(Y - var(X))).
code_util__translate_builtin("int", "+", 10000, [X, Y],
	no, yes(Y - var(X))).
code_util__translate_builtin("int", "builtin_unary_minus", 10000, [X, Y],
	no, yes(Y - binop((-), const(int_const(0)), var(X)))).
code_util__translate_builtin("int", "-", 10000, [X, Y],
	no, yes(Y - binop((-), const(int_const(0)), var(X)))).
code_util__translate_builtin("int", "builtin_bit_neg", 10000, [X, Y],
	no, yes(Y - unop(bitwise_complement, var(X)))).
code_util__translate_builtin("int", "\\", 10000, [X, Y],
	no, yes(Y - unop(bitwise_complement, var(X)))).
code_util__translate_builtin("int", ">", 0, [X, Y],
	yes(binop((>), var(X), var(Y))), no).
code_util__translate_builtin("int", "<", 0, [X, Y],
	yes(binop((<), var(X), var(Y))), no).
code_util__translate_builtin("int", ">=", 0, [X, Y],
	yes(binop((>=), var(X), var(Y))), no).
code_util__translate_builtin("int", "=<", 0, [X, Y],
	yes(binop((<=), var(X), var(Y))), no).

code_util__translate_builtin("float", "builtin_float_plus", 10000, [X, Y, Z],
	no, yes(Z - binop(float_plus, var(X), var(Y)))).
code_util__translate_builtin("float", "builtin_float_plus", 10001, [X, Y, Z],
	no, yes(X - binop(float_minus, var(Z), var(Y)))).
code_util__translate_builtin("float", "builtin_float_plus", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(Z), var(X)))).
code_util__translate_builtin("float", "+", 10000, [X, Y, Z],
	no, yes(Z - binop(float_plus, var(X), var(Y)))).
code_util__translate_builtin("float", "+", 10001, [X, Y, Z],
	no, yes(X - binop(float_minus, var(Z), var(Y)))).
code_util__translate_builtin("float", "+", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(Z), var(X)))).
code_util__translate_builtin("float", "builtin_float_minus", 10000, [X, Y, Z],
	no, yes(Z - binop(float_minus, var(X), var(Y)))).
code_util__translate_builtin("float", "builtin_float_minus", 10001, [X, Y, Z],
	no, yes(X - binop(float_plus, var(Y), var(Z)))).
code_util__translate_builtin("float", "builtin_float_minus", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(X), var(Z)))).
code_util__translate_builtin("float", "-", 10000, [X, Y, Z],
	no, yes(Z - binop(float_minus, var(X), var(Y)))).
code_util__translate_builtin("float", "-", 10001, [X, Y, Z],
	no, yes(X - binop(float_plus, var(Y), var(Z)))).
code_util__translate_builtin("float", "-", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(X), var(Z)))).
code_util__translate_builtin("float", "builtin_float_times", 10000, [X, Y, Z],
	no, yes(Z - binop(float_times, var(X), var(Y)))).
code_util__translate_builtin("float", "builtin_float_times", 10001, [X, Y, Z],
	no, yes(X - binop(float_divide, var(Z), var(Y)))).
code_util__translate_builtin("float", "builtin_float_times", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(Z), var(X)))).
code_util__translate_builtin("float", "*", 10000, [X, Y, Z],
	no, yes(Z - binop(float_times, var(X), var(Y)))).
code_util__translate_builtin("float", "*", 10001, [X, Y, Z],
	no, yes(X - binop(float_divide, var(Z), var(Y)))).
code_util__translate_builtin("float", "*", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(Z), var(X)))).
code_util__translate_builtin("float", "builtin_float_divide", 10000, [X, Y, Z],
	no, yes(Z - binop(float_divide, var(X), var(Y)))).
code_util__translate_builtin("float", "builtin_float_divide", 10001, [X, Y, Z],
	no, yes(X - binop(float_times, var(Y), var(Z)))).
code_util__translate_builtin("float", "builtin_float_divide", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(X), var(Z)))).
code_util__translate_builtin("float", "/", 10000, [X, Y, Z],
	no, yes(Z - binop(float_divide, var(X), var(Y)))).
code_util__translate_builtin("float", "/", 10001, [X, Y, Z],
	no, yes(X - binop(float_times, var(Y), var(Z)))).
code_util__translate_builtin("float", "/", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(X), var(Z)))).
code_util__translate_builtin("float", "+", 10000, [X, Y],
	no, yes(Y - var(X))).
code_util__translate_builtin("float", "-", 10000, [X, Y],
	no, yes(Y - binop(float_minus, const(float_const(0.0)), var(X)))).
code_util__translate_builtin("float", "builtin_float_gt", 0, [X, Y],
	yes(binop(float_gt, var(X), var(Y))), no).
code_util__translate_builtin("float", ">", 0, [X, Y],
	yes(binop(float_gt, var(X), var(Y))), no).
code_util__translate_builtin("float", "builtin_float_lt", 0, [X, Y],
	yes(binop(float_lt, var(X), var(Y))), no).
code_util__translate_builtin("float", "<", 0, [X, Y],
	yes(binop(float_lt, var(X), var(Y))), no).
code_util__translate_builtin("float", "builtin_float_ge", 0, [X, Y],
	yes(binop(float_ge, var(X), var(Y))), no).
code_util__translate_builtin("float", ">=", 0, [X, Y],
	yes(binop(float_ge, var(X), var(Y))), no).
code_util__translate_builtin("float", "builtin_float_le", 0, [X, Y],
	yes(binop(float_le, var(X), var(Y))), no).
code_util__translate_builtin("float", "=<", 0, [X, Y],
	yes(binop(float_le, var(X), var(Y))), no).

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

code_util__goal_may_allocate_heap_2(higher_order_call(_, _, _, _, _, _)).
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
code_util__goal_may_allocate_heap_2(disj(Goals, _)) :-
	code_util__goal_list_may_allocate_heap(Goals).
code_util__goal_may_allocate_heap_2(switch(_Var, _Det, Cases, _)) :-
	code_util__cases_may_allocate_heap(Cases).
code_util__goal_may_allocate_heap_2(if_then_else(_Vars, A, B, C, _)) :-
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

code_util__cons_id_to_tag(int_const(X), _, _, int_constant(X)).
code_util__cons_id_to_tag(float_const(X), _, _, float_constant(X)).
code_util__cons_id_to_tag(string_const(X), _, _, string_constant(X)).
code_util__cons_id_to_tag(code_addr_const(P,M), _, _, code_addr_constant(P,M)).
code_util__cons_id_to_tag(pred_const(P,M), _, _, pred_closure_tag(P,M)).
code_util__cons_id_to_tag(base_type_info_const(M,T,A), _, _,
		base_type_info_constant(M,T,A)).
code_util__cons_id_to_tag(cons(qualified(_, _), _), _, _, _) :-
	% should have been transformed into a function call or pred_const.
	error("code_util__cons_id_to_tag - qualified cons_id").
code_util__cons_id_to_tag(cons(unqualified(Name), Arity),
			Type, ModuleInfo, Tag) :-
	(
			% handle the `character' type specially
		Type = term__functor(term__atom("character"), [], _),
	 	string__char_to_string(Char, Name)
	->
		char__to_int(Char, CharCode),
		Tag = int_constant(CharCode)
	;
			% handle higher-order types specially
		type_is_higher_order(Type, PredOrFunc, PredArgTypes)
	->
		list__length(PredArgTypes, PredArity),
		module_info_get_predicate_table(ModuleInfo, PredicateTable),
		TotalArity is Arity + PredArity,
		(
			predicate_table_search_pf_name_arity(
				PredicateTable, PredOrFunc, Name, TotalArity,
				PredIds)
		->
			( PredIds = [PredId] ->
				predicate_table_get_preds(PredicateTable,
					Preds),
				map__lookup(Preds, PredId, PredInfo),
				pred_info_procedures(PredInfo, Procs),
				map__keys(Procs, ProcIds),
				( ProcIds = [ProcId] ->
					Tag = pred_closure_tag(PredId, ProcId)
				;
					error("sorry, not implemented: taking address of predicate or function with multiple modes")
				)
			;
					% cons_id ought to include the module
					% prefix, so that we could use
					% predicate_table__search_pf_m_n_a to 
					% prevent this from happening
				error("code_util__cons_id_to_tag: ambiguous pred or func")
			)
		;
			% the type-checker should ensure that this never happens
			error("code_util__cons_id_to_tag: invalid pred or func")
		)
	;
			% Use the type to determine the type_id
		( type_to_type_id(Type, TypeId0, _) ->
			TypeId = TypeId0
		;
			% the type-checker should ensure that this never happens
			error("code_util__cons_id_to_tag: invalid type")
		),

			% Given the type_id, lookup up the constructor tag
			% table for that type
		module_info_types(ModuleInfo, TypeTable),
		map__lookup(TypeTable, TypeId, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		(
			TypeBody = du_type(_, ConsTable0, _)
		->
			ConsTable = ConsTable0
		;
			% this should never happen
			error(
			"code_util__cons_id_to_tag: type is not d.u. type?"
			)
		),
			% Finally look up the cons_id in the table
		map__lookup(ConsTable, cons(unqualified(Name), Arity), Tag)
	).

%-----------------------------------------------------------------------------%
