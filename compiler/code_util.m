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

:- pred code_util__make_uni_label(module_info, type_id, proc_id, proc_label).
:- mode code_util__make_uni_label(in, in, in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, lval).
:- mode code_util__arg_loc_to_register(in, out) is det.

	% Determine whether a goal might allocate some heap space,
	% i.e. whether it contains any construction unifications
	% or predicate calls.  BEWARE that this predicate is only
	% an approximation, used to decide whether or not to try to
	% reclaim the heap space; currently it fails even for some
	% goals which do allocate heap space, such as construction
	% of boxed constants.

:- pred code_util__goal_may_allocate_heap(hlds_goal).
:- mode code_util__goal_may_allocate_heap(in) is semidet.

:- pred code_util__goal_list_may_allocate_heap(list(hlds_goal)).
:- mode code_util__goal_list_may_allocate_heap(in) is semidet.

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

:- pred code_util__neg_rval(rval, rval).
:- mode code_util__neg_rval(in, out) is det.

:- pred code_util__negate_the_test(list(instruction), list(instruction)).
:- mode code_util__negate_the_test(in, out) is det.

:- pred code_util__compiler_generated(pred_info).
:- mode code_util__compiler_generated(in) is semidet.

:- pred code_util__predinfo_is_builtin(pred_info).
:- mode code_util__predinfo_is_builtin(in) is semidet.

:- pred code_util__builtin_state(module_info, pred_id, proc_id, builtin_state).
:- mode code_util__builtin_state(in, in, in, out) is det.

	% Given a module name, a predicate name, a proc_id and a list of
	% variables as the arguments, find out if that procedure of that
	% predicate is an inline builtin. If yes, the last two arguments
	% return two things:
	%
	% - an rval to execute as a test if the builtin is semidet; and
	%
	% - an rval to assign to a variable if the builtin calls for this.
	%
	% At least one of these will be present.
	%
	% Each test rval returned is guaranteed to be either a unop or a binop,
	% applied to arguments that are either variables (from the argument
	% list) or constants.
	%
	% Each to be assigned rval is guaranteed to be either in a form
	% acceptable for a test rval, or in the form of a variable.

:- pred code_util__translate_builtin(string, string, proc_id, list(var),
	maybe(rval), maybe(pair(var, rval))).
:- mode code_util__translate_builtin(in, in, in, in, out, out) is semidet.

	% Find out how a function symbol (constructor) is represented
	% in the given type.

:- pred code_util__cons_id_to_tag(cons_id, type, module_info, cons_tag).
:- mode code_util__cons_id_to_tag(in, in, in, out) is det.

	% Succeed if the given goal cannot encounter a context
	% that causes any variable to be flushed to its stack slot.
	% If such a goal needs a resume point, and that resume point cannot
	% be backtracked to once control leaves the goal, then the only entry
	% point we need for the resume point is the one with the resume
	% variables in their original locations.

:- pred code_util__cannot_stack_flush(hlds_goal).
:- mode code_util__cannot_stack_flush(in) is semidet.

	% Succeed if the given goal cannot fail before encountering a context
	% that forces all variables to be flushed to their stack slots.
	% If such a goal needs a resume point, the only entry point we need
	% is the stack entry point.

:- pred code_util__cannot_fail_before_stack_flush(hlds_goal).
:- mode code_util__cannot_fail_before_stack_flush(in) is semidet.

	% code_util__count_recursive_calls(Goal, PredId, ProcId, Min, Max)
	% Given that we are in predicate PredId and procedure ProcId,
	% return the minimum and maximum number of recursive calls that
	% an execution of Goal may encounter.

:- pred code_util__count_recursive_calls(hlds_goal, pred_id, proc_id,
	int, int).
:- mode code_util__count_recursive_calls(in, in, in, out, out) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module prog_data, type_util, special_pred.
:- import_module bool, char, int, string, map, term, varset, require, std_util.

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
			hlds_pred__in_in_unification_proc_id(ProcId)
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
			hlds_pred__in_in_unification_proc_id(ProcId)
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
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	module_info_name(ModuleInfo, ThisModule),
	(
		code_util__compiler_generated(PredInfo)
	->
		pred_info_arg_types(PredInfo, _TypeVarSet, ArgTypes),
		(
			special_pred_get_type(PredName, ArgTypes, Type),
			type_to_type_id(Type, TypeId, _),
			% All type_ids here should be module qualified,
			% since builtin types are handled separately in
			% polymorphism.m.
			TypeId = qualified(TypeModule, TypeName) - Arity
		->
			(
				ThisModule \= TypeModule,
				PredName = "__Unify__",
				\+ hlds_pred__in_in_unification_proc_id(ProcId)
			->
				DefiningModule = ThisModule
			;
				DefiningModule = TypeModule
			),
			ProcLabel = special_proc(DefiningModule, PredName,
				TypeModule, TypeName, Arity, ProcId)
		;
			string__append_list(["code_util__make_proc_label:\n",
				"cannot make label for special pred `",
				PredName, "'"], ErrorMessage),
			error(ErrorMessage)
		)
	;
		(
			% Work out which module supplies the code for
			% the predicate.
			ThisModule \= PredModule,
			\+ pred_info_is_imported(PredInfo)
		->
			% This predicate is a specialized version of 
			% a pred from a `.opt' file.
			DefiningModule = ThisModule
		;	
			DefiningModule = PredModule
		),
		pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
		pred_info_arity(PredInfo, Arity),
		ProcLabel = proc(DefiningModule, PredOrFunc,
			PredModule, PredName, Arity, ProcId)
	).

code_util__make_uni_label(ModuleInfo, TypeId, UniModeNum, ProcLabel) :-
	module_info_name(ModuleInfo, ModuleName),
	( TypeId = qualified(TypeModule, TypeName) - Arity ->
		( hlds_pred__in_in_unification_proc_id(UniModeNum) ->
			Module = TypeModule
		;
			Module = ModuleName
		),
		ProcLabel = special_proc(Module, "__Unify__", TypeModule,
			TypeName, Arity, UniModeNum)
	;
		error("code_util__make_uni_label: unqualified type_id")
	).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, reg(r, ArgLoc)).

%-----------------------------------------------------------------------------%

code_util__predinfo_is_builtin(PredInfo) :-
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
%	code_util__translate_builtin(ModuleName, PredName, _, _, _, _).
	pred_info_arity(PredInfo, Arity),
	( code_util__inline_builtin(ModuleName, PredName, 0, Arity)
	; code_util__inline_builtin(ModuleName, PredName, 10000, Arity)
	).

code_util__builtin_state(ModuleInfo, PredId0, ProcId, BuiltinState) :-
	predicate_module(ModuleInfo, PredId0, ModuleName),
	predicate_name(ModuleInfo, PredId0, PredName),
	predicate_arity(ModuleInfo, PredId0, Arity),
	proc_id_to_int(ProcId, ProcInt),
	( code_util__inline_builtin(ModuleName, PredName, ProcInt, Arity) ->
		BuiltinState = inline_builtin
	;
		BuiltinState = not_builtin
	).

:- pred code_util__inline_builtin(string, string, int, int).
:- mode code_util__inline_builtin(in, in, in, in) is semidet.

code_util__inline_builtin(ModuleName, PredName, ProcId, Arity) :-
	Arity =< 3,
	varset__init(VarSet),
	varset__new_vars(VarSet, Arity, Args, _),
	code_util__translate_builtin_2(ModuleName, PredName, ProcId, Args, _, _).

code_util__translate_builtin(Module, PredName, ProcId, Args, BinOp, AsgOp) :-
	proc_id_to_int(ProcId, ProcInt),
	code_util__translate_builtin_2(Module, PredName, ProcInt, Args,
		BinOp, AsgOp).

:- pred code_util__translate_builtin_2(string, string, int, list(var),
	maybe(rval), maybe(pair(var, rval))).
:- mode code_util__translate_builtin_2(in, in, in, in, out, out) is semidet.

code_util__translate_builtin_2("mercury_builtin", "builtin_int_gt", 0, [X, Y],
	yes(binop((>), var(X), var(Y))), no).
code_util__translate_builtin_2("mercury_builtin", "builtin_int_lt", 0, [X, Y],
	yes(binop((<), var(X), var(Y))), no).

code_util__translate_builtin_2("int", "builtin_plus", 10000, [X, Y, Z],
	no, yes(Z - binop((+), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_plus", 10001, [X, Y, Z],
	no, yes(X - binop((-), var(Z), var(Y)))).
code_util__translate_builtin_2("int", "builtin_plus", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(Z), var(X)))).
code_util__translate_builtin_2("int", "+", 10000, [X, Y, Z],
	no, yes(Z - binop((+), var(X), var(Y)))).
code_util__translate_builtin_2("int", "+", 10001, [X, Y, Z],
	no, yes(X - binop((-), var(Z), var(Y)))).
code_util__translate_builtin_2("int", "+", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(Z), var(X)))).
code_util__translate_builtin_2("int", "builtin_minus", 10000, [X, Y, Z],
	no, yes(Z - binop((-), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_minus", 10001, [X, Y, Z],
	no, yes(X - binop((+), var(Y), var(Z)))).
code_util__translate_builtin_2("int", "builtin_minus", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(X), var(Z)))).
code_util__translate_builtin_2("int", "-", 10000, [X, Y, Z],
	no, yes(Z - binop((-), var(X), var(Y)))).
code_util__translate_builtin_2("int", "-", 10001, [X, Y, Z],
	no, yes(X - binop((+), var(Y), var(Z)))).
code_util__translate_builtin_2("int", "-", 10002, [X, Y, Z],
	no, yes(Y - binop((-), var(X), var(Z)))).
code_util__translate_builtin_2("int", "builtin_times", 10000, [X, Y, Z],
	no, yes(Z - binop((*), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_times", 10001, [X, Y, Z],
	no, yes(X - binop((/), var(Z), var(Y)))).
code_util__translate_builtin_2("int", "builtin_times", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(Z), var(X)))).
code_util__translate_builtin_2("int", "*", 10000, [X, Y, Z],
	no, yes(Z - binop((*), var(X), var(Y)))).
code_util__translate_builtin_2("int", "*", 10001, [X, Y, Z],
	no, yes(X - binop((/), var(Z), var(Y)))).
code_util__translate_builtin_2("int", "*", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(Z), var(X)))).
code_util__translate_builtin_2("int", "builtin_div", 10000, [X, Y, Z],
	no, yes(Z - binop((/), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_div", 10001, [X, Y, Z],
	no, yes(X - binop((*), var(Y), var(Z)))).
code_util__translate_builtin_2("int", "builtin_div", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(X), var(Z)))).
code_util__translate_builtin_2("int", "//", 10000, [X, Y, Z],
	no, yes(Z - binop((/), var(X), var(Y)))).
code_util__translate_builtin_2("int", "//", 10001, [X, Y, Z],
	no, yes(X - binop((*), var(Y), var(Z)))).
code_util__translate_builtin_2("int", "//", 10002, [X, Y, Z],
	no, yes(Y - binop((/), var(X), var(Z)))).
code_util__translate_builtin_2("int", "builtin_mod", 10000, [X, Y, Z],
	no, yes(Z - binop((mod), var(X), var(Y)))).
code_util__translate_builtin_2("int", "rem", 10000, [X, Y, Z],
	no, yes(Z - binop((mod), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_left_shift", 10000, [X, Y, Z],
	no, yes(Z - binop((<<), var(X), var(Y)))).
code_util__translate_builtin_2("int", "<<", 10000, [X, Y, Z],
	no, yes(Z - binop((<<), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_right_shift", 10000, [X, Y, Z],
	no, yes(Z - binop((>>), var(X), var(Y)))).
code_util__translate_builtin_2("int", ">>", 10000, [X, Y, Z],
	no, yes(Z - binop((>>), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_bit_and", 10000, [X, Y, Z],
	no, yes(Z - binop((&), var(X), var(Y)))).
code_util__translate_builtin_2("int", "/\\", 10000, [X, Y, Z],
	no, yes(Z - binop((&), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_bit_or", 10000, [X, Y, Z],
	no, yes(Z - binop(('|'), var(X), var(Y)))).
code_util__translate_builtin_2("int", "\\/", 10000, [X, Y, Z],
	no, yes(Z - binop(('|'), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_bit_xor", 10000, [X, Y, Z],
	no, yes(Z - binop((^), var(X), var(Y)))).
code_util__translate_builtin_2("int", "^", 10000, [X, Y, Z],
	no, yes(Z - binop((^), var(X), var(Y)))).
code_util__translate_builtin_2("int", "builtin_unary_plus", 10000, [X, Y],
	no, yes(Y - var(X))).
code_util__translate_builtin_2("int", "+", 10000, [X, Y],
	no, yes(Y - var(X))).
code_util__translate_builtin_2("int", "builtin_unary_minus", 10000, [X, Y],
	no, yes(Y - binop((-), const(int_const(0)), var(X)))).
code_util__translate_builtin_2("int", "-", 10000, [X, Y],
	no, yes(Y - binop((-), const(int_const(0)), var(X)))).
code_util__translate_builtin_2("int", "builtin_bit_neg", 10000, [X, Y],
	no, yes(Y - unop(bitwise_complement, var(X)))).
code_util__translate_builtin_2("int", "\\", 10000, [X, Y],
	no, yes(Y - unop(bitwise_complement, var(X)))).
code_util__translate_builtin_2("int", ">", 0, [X, Y],
	yes(binop((>), var(X), var(Y))), no).
code_util__translate_builtin_2("int", "<", 0, [X, Y],
	yes(binop((<), var(X), var(Y))), no).
code_util__translate_builtin_2("int", ">=", 0, [X, Y],
	yes(binop((>=), var(X), var(Y))), no).
code_util__translate_builtin_2("int", "=<", 0, [X, Y],
	yes(binop((<=), var(X), var(Y))), no).

code_util__translate_builtin_2("float", "builtin_float_plus", 10000, [X, Y, Z],
	no, yes(Z - binop(float_plus, var(X), var(Y)))).
code_util__translate_builtin_2("float", "builtin_float_plus", 10001, [X, Y, Z],
	no, yes(X - binop(float_minus, var(Z), var(Y)))).
code_util__translate_builtin_2("float", "builtin_float_plus", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(Z), var(X)))).
code_util__translate_builtin_2("float", "+", 10000, [X, Y, Z],
	no, yes(Z - binop(float_plus, var(X), var(Y)))).
code_util__translate_builtin_2("float", "+", 10001, [X, Y, Z],
	no, yes(X - binop(float_minus, var(Z), var(Y)))).
code_util__translate_builtin_2("float", "+", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(Z), var(X)))).
code_util__translate_builtin_2("float", "builtin_float_minus", 10000, [X, Y, Z],
	no, yes(Z - binop(float_minus, var(X), var(Y)))).
code_util__translate_builtin_2("float", "builtin_float_minus", 10001, [X, Y, Z],
	no, yes(X - binop(float_plus, var(Y), var(Z)))).
code_util__translate_builtin_2("float", "builtin_float_minus", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(X), var(Z)))).
code_util__translate_builtin_2("float", "-", 10000, [X, Y, Z],
	no, yes(Z - binop(float_minus, var(X), var(Y)))).
code_util__translate_builtin_2("float", "-", 10001, [X, Y, Z],
	no, yes(X - binop(float_plus, var(Y), var(Z)))).
code_util__translate_builtin_2("float", "-", 10002, [X, Y, Z],
	no, yes(Y - binop(float_minus, var(X), var(Z)))).
code_util__translate_builtin_2("float", "builtin_float_times", 10000, [X, Y, Z],
	no, yes(Z - binop(float_times, var(X), var(Y)))).
code_util__translate_builtin_2("float", "builtin_float_times", 10001, [X, Y, Z],
	no, yes(X - binop(float_divide, var(Z), var(Y)))).
code_util__translate_builtin_2("float", "builtin_float_times", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(Z), var(X)))).
code_util__translate_builtin_2("float", "*", 10000, [X, Y, Z],
	no, yes(Z - binop(float_times, var(X), var(Y)))).
code_util__translate_builtin_2("float", "*", 10001, [X, Y, Z],
	no, yes(X - binop(float_divide, var(Z), var(Y)))).
code_util__translate_builtin_2("float", "*", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(Z), var(X)))).
code_util__translate_builtin_2("float", "builtin_float_divide", 10000, [X, Y, Z],
	no, yes(Z - binop(float_divide, var(X), var(Y)))).
code_util__translate_builtin_2("float", "builtin_float_divide", 10001, [X, Y, Z],
	no, yes(X - binop(float_times, var(Y), var(Z)))).
code_util__translate_builtin_2("float", "builtin_float_divide", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(X), var(Z)))).
code_util__translate_builtin_2("float", "/", 10000, [X, Y, Z],
	no, yes(Z - binop(float_divide, var(X), var(Y)))).
code_util__translate_builtin_2("float", "/", 10001, [X, Y, Z],
	no, yes(X - binop(float_times, var(Y), var(Z)))).
code_util__translate_builtin_2("float", "/", 10002, [X, Y, Z],
	no, yes(Y - binop(float_divide, var(X), var(Z)))).
code_util__translate_builtin_2("float", "+", 10000, [X, Y],
	no, yes(Y - var(X))).
code_util__translate_builtin_2("float", "-", 10000, [X, Y],
	no, yes(Y - binop(float_minus, const(float_const(0.0)), var(X)))).
code_util__translate_builtin_2("float", "builtin_float_gt", 0, [X, Y],
	yes(binop(float_gt, var(X), var(Y))), no).
code_util__translate_builtin_2("float", ">", 0, [X, Y],
	yes(binop(float_gt, var(X), var(Y))), no).
code_util__translate_builtin_2("float", "builtin_float_lt", 0, [X, Y],
	yes(binop(float_lt, var(X), var(Y))), no).
code_util__translate_builtin_2("float", "<", 0, [X, Y],
	yes(binop(float_lt, var(X), var(Y))), no).
code_util__translate_builtin_2("float", "builtin_float_ge", 0, [X, Y],
	yes(binop(float_ge, var(X), var(Y))), no).
code_util__translate_builtin_2("float", ">=", 0, [X, Y],
	yes(binop(float_ge, var(X), var(Y))), no).
code_util__translate_builtin_2("float", "builtin_float_le", 0, [X, Y],
	yes(binop(float_le, var(X), var(Y))), no).
code_util__translate_builtin_2("float", "=<", 0, [X, Y],
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
	).

%-----------------------------------------------------------------------------%

	% This code may _look_ nondeterministic, but it's really semidet,
	% and Mercury is smart enough to know this.

code_util__goal_may_allocate_heap(Goal - _GoalInfo) :-
	code_util__goal_may_allocate_heap_2(Goal).

:- pred code_util__goal_may_allocate_heap_2(hlds_goal_expr).
:- mode code_util__goal_may_allocate_heap_2(in) is semidet.

code_util__goal_may_allocate_heap_2(higher_order_call(_, _, _, _, _)).
code_util__goal_may_allocate_heap_2(call(_, _, _, Builtin, _, _)) :-
	Builtin \= inline_builtin.
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
code_util__cons_id_to_tag(cons(Name, Arity), Type, ModuleInfo, Tag) :-
	(
			% handle the `character' type specially
		Type = term__functor(term__atom("character"), [], _),
		Name = unqualified(ConsName),
	 	string__char_to_string(Char, ConsName)
	->
		char__to_int(Char, CharCode),
		Tag = int_constant(CharCode)
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
		map__lookup(ConsTable, cons(Name, Arity), Tag)
	).

%-----------------------------------------------------------------------------%

code_util__cannot_stack_flush(GoalExpr - _) :-
	code_util__cannot_stack_flush_2(GoalExpr).

:- pred code_util__cannot_stack_flush_2(hlds_goal_expr).
:- mode code_util__cannot_stack_flush_2(in) is semidet.

code_util__cannot_stack_flush_2(unify(_, _, _, Unify, _)) :-
	Unify \= complicated_unify(_, _).
code_util__cannot_stack_flush_2(call(_, _, _, BuiltinState, _, _)) :-
	BuiltinState = inline_builtin.
code_util__cannot_stack_flush_2(conj(Goals)) :-
	code_util__cannot_stack_flush_goals(Goals).
code_util__cannot_stack_flush_2(switch(_, _, Cases, _)) :-
	code_util__cannot_stack_flush_cases(Cases).

:- pred code_util__cannot_stack_flush_goals(list(hlds_goal)).
:- mode code_util__cannot_stack_flush_goals(in) is semidet.

code_util__cannot_stack_flush_goals([]).
code_util__cannot_stack_flush_goals([Goal | Goals]) :-
	code_util__cannot_stack_flush(Goal),
	code_util__cannot_stack_flush_goals(Goals).

:- pred code_util__cannot_stack_flush_cases(list(case)).
:- mode code_util__cannot_stack_flush_cases(in) is semidet.

code_util__cannot_stack_flush_cases([]).
code_util__cannot_stack_flush_cases([case(_, Goal) | Cases]) :-
	code_util__cannot_stack_flush(Goal),
	code_util__cannot_stack_flush_cases(Cases).

%-----------------------------------------------------------------------------%

code_util__cannot_fail_before_stack_flush(GoalExpr - GoalInfo) :-
	goal_info_get_determinism(GoalInfo, Detism),
	determinism_components(Detism, CanFail, _),
	( CanFail = cannot_fail ->
		true
	;
		code_util__cannot_fail_before_stack_flush_2(GoalExpr)
	).

:- pred code_util__cannot_fail_before_stack_flush_2(hlds_goal_expr).
:- mode code_util__cannot_fail_before_stack_flush_2(in) is semidet.

code_util__cannot_fail_before_stack_flush_2(conj(Goals)) :-
	code_util__cannot_fail_before_stack_flush_conj(Goals).

:- pred code_util__cannot_fail_before_stack_flush_conj(list(hlds_goal)).
:- mode code_util__cannot_fail_before_stack_flush_conj(in) is semidet.

code_util__cannot_fail_before_stack_flush_conj([]).
code_util__cannot_fail_before_stack_flush_conj([Goal | Goals]) :-
	Goal = GoalExpr - GoalInfo,
	(
		(
			GoalExpr = call(_, _, _, BuiltinState, _, _),
			BuiltinState \= inline_builtin
		;
			GoalExpr = higher_order_call(_, _, _, _, _)
		)
	->
		true
	;
		goal_info_get_determinism(GoalInfo, Detism),
		determinism_components(Detism, cannot_fail, _)
	->
		code_util__cannot_fail_before_stack_flush_conj(Goals)
	;
		fail
	).

%-----------------------------------------------------------------------------%

code_util__count_recursive_calls(Goal - _, PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_2(Goal, PredId, ProcId, Min, Max).

:- pred code_util__count_recursive_calls_2(hlds_goal_expr, pred_id, proc_id,
	int, int).
:- mode code_util__count_recursive_calls_2(in, in, in, out, out) is det.

code_util__count_recursive_calls_2(not(Goal), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls(Goal, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(some(_, Goal), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls(Goal, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(unify(_, _, _, _, _), _, _, 0, 0).
code_util__count_recursive_calls_2(higher_order_call(_,_, _, _, _), _, _, 0, 0).
code_util__count_recursive_calls_2(pragma_c_code(_,_,_,_, _, _, _, _), _, _,
		0, 0).
code_util__count_recursive_calls_2(call(CallPredId, CallProcId, _, _, _, _),
		PredId, ProcId, Count, Count) :-
	(
		PredId = CallPredId,
		ProcId = CallProcId
	->
		Count = 1
	;
		Count = 0
	).
code_util__count_recursive_calls_2(conj(Goals), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_conj(Goals, PredId, ProcId, 0, 0,
		Min, Max).
code_util__count_recursive_calls_2(disj(Goals, _), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_disj(Goals, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(switch(_, _, Cases, _), PredId, ProcId,
		Min, Max) :-
	code_util__count_recursive_calls_cases(Cases, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(if_then_else(_, Cond, Then, Else, _),
		PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls(Cond, PredId, ProcId, CMin, CMax),
	code_util__count_recursive_calls(Then, PredId, ProcId, TMin, TMax),
	code_util__count_recursive_calls(Else, PredId, ProcId, EMin, EMax),
	CTMin is CMin + TMin,
	CTMax is CMax + TMax,
	int__min(CTMin, EMin, Min),
	int__max(CTMax, EMax, Max).

:- pred code_util__count_recursive_calls_conj(list(hlds_goal),
	pred_id, proc_id, int, int, int, int).
:- mode code_util__count_recursive_calls_conj(in, in, in, in, in, out, out)
	is det.

code_util__count_recursive_calls_conj([], _, _, Min, Max, Min, Max).
code_util__count_recursive_calls_conj([Goal | Goals], PredId, ProcId,
		Min0, Max0, Min, Max) :-
	code_util__count_recursive_calls(Goal, PredId, ProcId, Min1, Max1),
	Min2 is Min0 + Min1,
	Max2 is Max0 + Max1,
	code_util__count_recursive_calls_conj(Goals, PredId, ProcId,
		Min2, Max2, Min, Max).

:- pred code_util__count_recursive_calls_disj(list(hlds_goal),
	pred_id, proc_id, int, int).
:- mode code_util__count_recursive_calls_disj(in, in, in, out, out) is det.

code_util__count_recursive_calls_disj([], _, _, 0, 0).
code_util__count_recursive_calls_disj([Goal | Goals], PredId, ProcId,
		Min, Max) :-
	( Goals = [] ->
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min, Max)
	;
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min0, Max0),
		code_util__count_recursive_calls_disj(Goals, PredId, ProcId,
			Min1, Max1),
		int__min(Min0, Min1, Min),
		int__max(Max0, Max1, Max)
	).

:- pred code_util__count_recursive_calls_cases(list(case),
	pred_id, proc_id, int, int).
:- mode code_util__count_recursive_calls_cases(in, in, in, out, out) is det.

code_util__count_recursive_calls_cases([], _, _, _, _) :-
	error("empty cases in code_util__count_recursive_calls_cases").
code_util__count_recursive_calls_cases([case(_, Goal) | Cases], PredId, ProcId,
		Min, Max) :-
	( Cases = [] ->
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min, Max)
	;
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min0, Max0),
		code_util__count_recursive_calls_cases(Cases, PredId, ProcId,
			Min1, Max1),
		int__min(Min0, Min1, Min),
		int__max(Max0, Max1, Max)
	).
