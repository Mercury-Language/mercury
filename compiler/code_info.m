%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% file: code_info.nl
%
% main author: conway.
%
% This file defines the code_info type and various operations on it.
%
% The code_info structure is a 'state' used by the code generator.
%
% The following assumptions are made about the state of the high-level
% data structure:
%	o  Variables can be stored in any number of distinct places.
%	o  Registers may contain a value corresponding to more than
%		one variable.
%	o  Procedures are in superhomogeneous form. This means that
%		construction unifications and builtins are not nested.
%	o  Evaluation of arguments in construction and deconstruction
%		unifications is lazy. This means that arguments in a
%		`don't care' mode are ignored, and that assignments
%		are cached.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_info.

:- interface.

:- import_module hlds, llds.
:- import_module code_util, tree, set, std_util, globals.

:- type code_info.

:- type code_tree	==	tree(list(instruction)).

		% Create a new code_info structure.
:- pred code_info__init(varset, liveness_info, call_info, bool, globals,
				pred_id, proc_id, proc_info, category,
					follow_vars, module_info, code_info).
:- mode code_info__init(in, in, in, in, in,
					in, in, in, in, in, in, out) is det.

		% Generate the next local label in sequence.
:- pred code_info__get_next_label(label, code_info, code_info).
:- mode code_info__get_next_label(out, in, out) is det.

		% Get the current local label.
:- pred code_info__get_current_label(label, code_info, code_info).
:- mode code_info__get_current_label(out, in, out) is det.

		% Get the variables for the current procedure.
:- pred code_info__get_varset(varset, code_info, code_info).
:- mode code_info__get_varset(out, in, out) is det.

		% Set the variables for the current procedure.
:- pred code_info__set_varset(varset, code_info, code_info).
:- mode code_info__set_varset(in, in, out) is det.

		% Get the hlds mapping from variables to
		% stack slots
:- pred code_info__get_call_info(call_info, code_info, code_info).
:- mode code_info__get_call_info(out, in, out) is det.

		% Set the hlds mapping from variables to
		% stack slots
:- pred code_info__set_call_info(call_info, code_info, code_info).
:- mode code_info__set_call_info(in, in, out) is det.

:- pred code_info__get_pred_id(pred_id, code_info, code_info).
:- mode code_info__get_pred_id(out, in, out) is det.

:- pred code_info__set_pred_id(pred_id, code_info, code_info).
:- mode code_info__set_pred_id(in, in, out) is det.

:- pred code_info__get_proc_id(proc_id, code_info, code_info).
:- mode code_info__get_proc_id(out, in, out) is det.

:- pred code_info__set_proc_id(proc_id, code_info, code_info).
:- mode code_info__set_proc_id(in, in, out) is det.

:- pred code_info__set_module_info(module_info, code_info, code_info).
:- mode code_info__set_module_info(in, in, out) is det.

		% Get an unused register, and mark it as
		% `reserved'
:- pred code_info__get_free_register(reg, code_info, code_info).
:- mode code_info__get_free_register(out, in, out) is det.

		% Clear all reserved registers.
:- pred code_info__clear_reserved_registers(code_info, code_info).
:- mode code_info__clear_reserved_registers(in, out) is det.

		% Generate all cached expressions
:- pred code_info__flush_expression_cache(code_tree,
						code_info, code_info).
:- mode code_info__flush_expression_cache(out, in, out) is det.

		% Flush a single cached expression
:- pred code_info__flush_variable(var, code_tree, code_info, code_info).
:- mode code_info__flush_variable(in, out, in, out) is det.

		% Enter the expression for a variable into the cache
:- pred code_info__cache_expression(var, rval, code_info, code_info).
:- mode code_info__cache_expression(in, in, in, out) is det.

		% Enter an expression and variable into the cache
		% with a specific target
:- pred code_info__cache_expression_with_target(var, rval, lval,
							code_info, code_info).
:- mode code_info__cache_expression_with_target(in, in, in, in, out) is det.

		% Generate code to either setup the input arguments for a call
		% (i.e. in the caller), or to setup the output arguments in the
		% predicate epilog (i.e. in the callee).

:- type call_direction ---> caller ; callee.

:- pred code_info__setup_call(assoc_list(var, arg_info), list(var),
			call_direction, code_tree, code_info, code_info).
:- mode code_info__setup_call(in, in, in, out, in, out) is det.

		% Generate code to swap a live value out of the given
		% register, and place the value for the given variable
		% into that register.
:- pred code_info__shuffle_register(var, list(var), reg, code_tree,
						code_info, code_info).
:- mode code_info__shuffle_register(in, in, in, out, in, out) is det.

		% Mark a register as reserved
:- pred code_info__reserve_register(reg, code_info, code_info).
:- mode code_info__reserve_register(in, in, out) is det.

		% Generate code to store the value of
		% a given variable on the stack
:- pred code_info__save_variable_on_stack(var, code_tree,
							code_info, code_info).
:- mode code_info__save_variable_on_stack(in, out, in, out) is det.

		% Save a variable. If there is a known register for
		% this variable, save it there. Else, if there is a
		% stack slot for it, save it there. Else error.
:- pred code_info__save_variable_somewhere_definite(var, code_tree,
							code_info, code_info).
:- mode code_info__save_variable_somewhere_definite(in, out, in, out) is det.

		% Save a variable. Use a known reg if one exists, else use
		% J. Random register.
:- pred code_info__save_variable_somewhere(var, code_tree,
							code_info, code_info).
:- mode code_info__save_variable_somewhere(in, out, in, out) is det.

		% Succeed if the given variable is live at the
		% end of this goal.
:- pred code_info__variable_is_live(var, code_info, code_info).
:- mode code_info__variable_is_live(in, in, out) is semidet.

		% Get the call argument info for a given mode
		% of a given predicate
:- pred code_info__get_pred_proc_arginfo(pred_id, proc_id, list(arg_info),
							code_info, code_info).
:- mode code_info__get_pred_proc_arginfo(in, in, out, in, out) is det.

		% Get the call argument information for the
		% current procedure
:- pred code_info__get_arginfo(list(arg_info), code_info, code_info).
:- mode code_info__get_arginfo(out, in, out) is det.

:- pred code_info__get_headvars(list(var), code_info, code_info).
:- mode code_info__get_headvars(out, in, out) is det.

:- pred code_info__generate_expression(rval, lval, code_tree,
							code_info, code_info).
:- mode code_info__generate_expression(in, in, out, in, out) is det.

:- pred code_info__get_variable_register(var, lval, code_info, code_info).
:- mode code_info__get_variable_register(in, out, in, out) is det.

:- pred code_info__variable_register(var, lval, code_info, code_info).
:- mode code_info__variable_register(in, out, in, out) is semidet.

:- pred code_info__variable_has_register(var, lval, code_info, code_info).
:- mode code_info__variable_has_register(in, in, in, out) is semidet.

:- pred code_info__cons_id_to_tag(var, cons_id, cons_tag,
							code_info, code_info).
:- mode code_info__cons_id_to_tag(in, in, out, in, out) is det.

:- pred code_info__get_stackslot_count(int, code_info, code_info).
:- mode code_info__get_stackslot_count(out, in, out) is det.

:- pred code_info__reset_variable_target(var, lval, code_info, code_info).
:- mode code_info__reset_variable_target(in, in, in, out) is semidet.

:- pred code_info__clear_all_registers(code_info, code_info).
:- mode code_info__clear_all_registers(in, out) is det.

:- pred code_info__set_variable_register(var, reg, code_info, code_info).
:- mode code_info__set_variable_register(in, in, in, out) is det.

:- pred code_info__get_live_variables(list(var), code_info, code_info).
:- mode code_info__get_live_variables(out, in, out) is det.

:- pred code_info__generate_forced_saves(code_tree, code_info, code_info).
:- mode code_info__generate_forced_saves(out, in, out) is det.

:- pred code_info__generate_eager_flush(code_tree, code_info, code_info).
:- mode code_info__generate_eager_flush(out, in, out) is det.

:- pred code_info__grab_code_info(code_info, code_info, code_info).
:- mode code_info__grab_code_info(out, in, out) is det.

:- pred code_info__slap_code_info(code_info, code_info, code_info).
:- mode code_info__slap_code_info(in, in, out) is det.

:- pred code_info__register_variables(reg, set(var), code_info, code_info).
:- mode code_info__register_variables(in, out, in, out) is semidet.

:- pred code_info__get_proc_info(proc_info, code_info, code_info).
:- mode code_info__get_proc_info(out, in, out) is det.

:- pred code_info__get_succip_used(bool, code_info, code_info).
:- mode code_info__get_succip_used(out, in, out) is det.

:- pred code_info__set_succip_used(bool, code_info, code_info).
:- mode code_info__set_succip_used(in, in, out) is det.

:- pred code_info__remake_with_store_map(code_info, code_info).
:- mode code_info__remake_with_store_map(in, out) is det.

:- pred code_info__remake_with_call_info(code_info, code_info).
:- mode code_info__remake_with_call_info(in, out) is det.

:- pred code_info__update_liveness_info(delta_liveness, code_info, code_info).
:- mode code_info__update_liveness_info(in, in, out) is det.

:- pred code_info__update_deadness_info(delta_liveness, code_info, code_info).
:- mode code_info__update_deadness_info(in, in, out) is det.

:- pred code_info__reduce_variables_and_registers(code_info, code_info).
:- mode code_info__reduce_variables_and_registers(in, out) is det.

:- pred code_info__add_lvalue_to_variable(lval, var, code_info, code_info).
:- mode code_info__add_lvalue_to_variable(in, in, in, out) is det.

:- pred code_info__get_module_info(module_info, code_info, code_info).
:- mode code_info__get_module_info(out, in, out) is det.

:- pred code_info__push_failure_cont(label, code_info, code_info).
:- mode code_info__push_failure_cont(in, in, out) is det.

:- pred code_info__pop_failure_cont(label, code_info, code_info).
:- mode code_info__pop_failure_cont(out, in, out) is semidet.

:- pred code_info__pop_failure_cont_det(label, code_info, code_info).
:- mode code_info__pop_failure_cont_det(out, in, out) is det.

:- pred code_info__set_failure_cont(label, code_info, code_info).
:- mode code_info__set_failure_cont(in, in, out) is det.

:- pred code_info__get_failure_cont(label, code_info, code_info).
:- mode code_info__get_failure_cont(out, in, out) is det.

:- pred code_info__failure_cont(label, code_info, code_info).
:- mode code_info__failure_cont(out, in, out) is semidet.

:- pred code_info__unset_failure_cont(code_info, code_info).
:- mode code_info__unset_failure_cont(in, out) is det.

:- pred code_info__stack_variable(int, lval, code_info, code_info).
:- mode code_info__stack_variable(in, out, in, out) is det.

:- pred code_info__generate_nondet_saves(code_tree, code_info, code_info).
:- mode code_info__generate_nondet_saves(out, in, out) is det.

:- pred code_info__generate_failure(code_tree, code_info, code_info).
:- mode code_info__generate_failure(out, in, out) is det.

:- pred code_info__generate_test_and_fail(rval, code_tree,
							code_info, code_info).
:- mode code_info__generate_test_and_fail(in, out, in, out) is det.

:- pred code_info__generate_cond_branch(rval, label, label, code_tree,
							code_info, code_info).
:- mode code_info__generate_cond_branch(in, in, in, out, in, out) is det.

:- pred code_info__generate_icond_branch(rval, label, label, code_tree,
							code_info, code_info).
:- mode code_info__generate_icond_branch(in, in, in, out, in, out) is det.

:- pred code_info__generate_pre_commit(code_tree, label, code_info, code_info).
:- mode code_info__generate_pre_commit(out, out, in, out) is det.

:- pred code_info__generate_commit(label, code_tree, code_info, code_info).
:- mode code_info__generate_commit(in, out, in, out) is det.

:- pred code_info__get_continuation(maybe(label), code_info, code_info).
:- mode code_info__get_continuation(out, in, out) is det.

:- pred code_info__set_continuation(maybe(label), code_info, code_info).
:- mode code_info__set_continuation(in, in, out) is det.

:- pred code_info__save_hp(code_tree, code_info, code_info).
:- mode code_info__save_hp(out, in, out) is det.

:- pred code_info__restore_hp(code_tree, code_info, code_info).
:- mode code_info__restore_hp(out, in, out) is det.

:- pred code_info__pop_stack(code_tree, code_info, code_info).
:- mode code_info__pop_stack(out, in, out) is det.

:- pred code_info__get_globals(globals, code_info, code_info).
:- mode code_info__get_globals(out, in, out) is det.

:- pred code_info__set_globals(globals, code_info, code_info).
:- mode code_info__set_globals(in, in, out) is det.

:- pred code_info__push_store_map(map(var, lval), code_info, code_info).
:- mode code_info__push_store_map(in, in, out) is det.

:- pred code_info__pop_store_map(code_info, code_info).
:- mode code_info__pop_store_map(in, out) is det.

:- pred code_info__current_store_map(map(var, lval), code_info, code_info).
:- mode code_info__current_store_map(out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module string, require, list, map, bimap, tree, int.
:- import_module varset, term, stack.
:- import_module type_util.

:- type code_info	--->
		code_info(
			int,		% The number of stack slots allocated.
			int,		% Counter for the local labels used
					% by this procedure.
			varset,		% The variables in this procedure.
			call_info,	% The storage allocations for the
					% live variables at the end of the
					% current switch.
			pred_id,	% The label of the current predicate.
			proc_id,	% The label of the current procedure.
			register_info,	% A map storing the information about
					% what is stored in each register.
			variable_info,	% A map storing the information about
					% the status of each variable.
			proc_info,	% The proc_info for the this procedure.
			bool,		% do we need to store succip?
			fall_through,	% The fallthrough label for failure
			module_info,	% The module_info structure - you just
					% never know when you might need it.
			liveness_info,	% Variables that are live
					% after this goal
			stack(map(var, lval)),
					% Store Map - where to put things
			pair(category),	% The category of the current procedure
					% and the current context (respectively)
			maybe(label),	% The first failure continuation for
					% nondet code
			int,		% The number of extra stackslots
					% that have been pushed during the
					% procedure
			globals		% code generation options
	).

:- type register_info	==	map(reg, register_stat).

:- type register_stat	--->	unused
			;	reserved
			;	vars(set(var)).

:- type target_register	--->	none
			;	target(lval).

:- type variable_info	==	map(var, variable_stat).

:- type variable_stat	--->	unused
			;	evaluated(set(lval))
			;	cached(rval, target_register).

:- type fall_through	==	stack(label).

%---------------------------------------------------------------------------%

code_info__init(Varset, Liveness, CallInfo, SaveSuccip, Globals,
					PredId, ProcId, ProcInfo, Category,
						_FollowVars, ModuleInfo, C) :-
	code_info__init_register_info(PredId, ProcId,
					ModuleInfo, RegisterInfo),
	code_info__init_variable_info(PredId, ProcId,
					ModuleInfo, VariableInfo),
	stack__init(Continue),
	stack__init(StoreMapStack0),
	map__init(StoreMap),
	stack__push(StoreMapStack0, StoreMap, StoreMapStack),
	code_info__max_slot(CallInfo, SlotCount),
	C = code_info(
		SlotCount,
		0,
		Varset,
		CallInfo, 
		PredId,
		ProcId,
		RegisterInfo,
		VariableInfo,
		ProcInfo,
		SaveSuccip,
		Continue,
		ModuleInfo,
		Liveness,
		StoreMapStack,
		Category - Category,
		no,
		0,
		Globals
	).

%---------------------------------------------------------------------------%

:- pred code_info__max_slot(call_info, int).
:- mode code_info__max_slot(in, out) is det.

code_info__max_slot(CallInfo, SlotCount) :-
	map__to_assoc_list(CallInfo, CallList),
	list__length(CallList, SlotCount).

%---------------------------------------------------------------------------%

:- pred code_info__init_register_info(pred_id, proc_id, module_info,
								register_info).
:- mode code_info__init_register_info(in, in, in, out) is det.

code_info__init_register_info(PredId, ProcId, ModuleInfo, RegisterInfo) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	map__init(RegisterInfo0),
	code_info__init_reg_info_2(ArgInfos, 1, HeadVars,
						RegisterInfo0, RegisterInfo).

:- pred code_info__init_reg_info_2(list(arg_info), int, list(var),
						register_info, register_info).
:- mode code_info__init_reg_info_2(in, in, in, in, out) is det.

code_info__init_reg_info_2([], _N, _HeadVars, RegisterInfo, RegisterInfo).
code_info__init_reg_info_2([arg_info(ArgLoc, Mode)|ArgVars], N0, HeadVars,
						RegisterInfo0, RegisterInfo) :-
	code_util__arg_loc_to_register(ArgLoc, Reg),
	list__index1_det(HeadVars, N0, Var),
	(
		Mode = top_in
	->
		set__singleton_set(Vars, Var),
		map__set(RegisterInfo0, Reg, vars(Vars), RegisterInfo1)
	;
		RegisterInfo1 = RegisterInfo0
	),
	N1 is N0 + 1,
	code_info__init_reg_info_2(ArgVars, N1, HeadVars,
						RegisterInfo1, RegisterInfo).

%---------------------------------------------------------------------------%

:- pred code_info__init_variable_info(pred_id, proc_id, module_info,
								variable_info).
:- mode code_info__init_variable_info(in, in, in, out) is det.

code_info__init_variable_info(PredId, ProcId, ModuleInfo, VariableInfo) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	map__init(VariableInfo0),
	code_info__init_var_info_2(ArgInfos, 1, HeadVars,
						VariableInfo0, VariableInfo).

:- pred code_info__init_var_info_2(list(arg_info), int, list(var),
						variable_info, variable_info).
:- mode code_info__init_var_info_2(in, in, in, in, out) is det.

code_info__init_var_info_2([], _N, _HeadVars, VariableInfo, VariableInfo).
code_info__init_var_info_2([arg_info(ArgLoc, Mode)|ArgVars], N0, HeadVars,
						VariableInfo0, VariableInfo) :-
	(
		Mode = top_in
	->
		code_util__arg_loc_to_register(ArgLoc, Reg),
		list__index1_det(HeadVars, N0, Var),
		Lval = reg(Reg),
		set__singleton_set(Lvals, Lval),
		map__set(VariableInfo0, Var, evaluated(Lvals), VariableInfo1)
	;
		VariableInfo1 = VariableInfo0
	),
	N1 is N0 + 1,
	code_info__init_var_info_2(ArgVars, N1, HeadVars,
						VariableInfo1, VariableInfo).

%---------------------------------------------------------------------------%

	% The lowest numbered free register is returned.

code_info__get_free_register(Reg) -->
	code_info__get_registers(Registers0),
	{ map__keys(Registers0, RegList) },
	{ code_info__get_free_register_2(1, RegList, RegNum) },
	{ Reg = r(RegNum) },
	{ map__set(Registers0, Reg, reserved, Registers) },
	code_info__set_registers(Registers).

:- pred code_info__get_free_register_2(int, list(reg), int).
:- mode code_info__get_free_register_2(in, in, out) is det.

code_info__get_free_register_2(N, [], N).
code_info__get_free_register_2(N, [R|Rs], RegNum) :-
	(
		R = r(RN),
		N < RN
	->
		RegNum = N
	;
		R = r(NR0)
	->
		NR is NR0 + 1,
		code_info__get_free_register_2(NR, Rs, RegNum)
	;
		code_info__get_free_register_2(N, Rs, RegNum)
	).

%---------------------------------------------------------------------------%

:- pred code_info__get_next_free_register(reg, reg, code_info, code_info).
:- mode code_info__get_next_free_register(in, out, in, out) is det.

code_info__get_next_free_register(Reg, NewReg) -->
	code_info__get_registers(Registers0),
	{ map__keys(Registers0, RegList0) },
	(
		{ Reg = r(N0) }
	->
		{ N1 is N0 + 1 }
	;
		{ error("Next free register!") }
	),
	{ code_info__filter_register_list(N1, RegList0, RegList) },
	{ code_info__get_free_register_2(N1, RegList, RegNum) },
	{ NewReg = r(RegNum) },
	{ map__set(Registers0, Reg, reserved, Registers) },
	code_info__set_registers(Registers).

:- pred code_info__filter_register_list(int, list(reg), list(reg)).
:- mode code_info__filter_register_list(in, in, out) is det.

code_info__filter_register_list(_N0, [], []).
code_info__filter_register_list(N0, [Reg|Regs0], Regs) :-
	(
		Reg = r(N1),
		N1 < N0
	->
		code_info__filter_register_list(N0, Regs0, Regs)
	;
		Regs = [Reg|Regs0]
	).

%---------------------------------------------------------------------------%

code_info__clear_reserved_registers -->
	code_info__get_registers(Registers0),
	{ map__to_assoc_list(Registers0, RegList0) },
	{ code_info__clear_reserved_registers_2(RegList0, RegList) },
	{ map__from_sorted_assoc_list(RegList, Registers) },
	code_info__set_registers(Registers).

:- pred code_info__clear_reserved_registers_2(assoc_list(reg, register_stat),
						assoc_list(reg, register_stat)).
:- mode code_info__clear_reserved_registers_2(in, out) is det.

code_info__clear_reserved_registers_2([], []).
code_info__clear_reserved_registers_2([R - S|Rs0], Rs) :-
	code_info__clear_reserved_registers_2(Rs0, Rs1),
	(
		S = reserved
	->
		Rs = Rs1
	;
		Rs = [R - S|Rs1]
	).

%---------------------------------------------------------------------------%

code_info__flush_expression_cache(Code) -->
	code_info__get_variables(Variables0),
	{ map__keys(Variables0, VarList) },
	code_info__flush_exp_cache_2(VarList, Code).

:- pred code_info__flush_exp_cache_2(list(var), code_tree,
						code_info, code_info).
:- mode code_info__flush_exp_cache_2(in, out, in, out) is det.

code_info__flush_exp_cache_2([], empty) --> [].
code_info__flush_exp_cache_2([Var|Vars], Code) -->
	code_info__flush_exp_cache_2(Vars, Code0),
	code_info__flush_variable(Var, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

code_info__flush_variable(Var, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = cached(Exprn, Target) }
	->
		code_info__target_to_lvalue(Target, Exprn, Var, TargetReg),
			% Generate code to evaluate the expression.
		code_info__generate_expression(Exprn, TargetReg, Code),
			% Mark the register as taken, so that
			% it doesn't get allocated
			% NOTE:
			%	1. The mode analysis ensures that
			%	the variable does not depend on
			%	itself (occurs check!)
			%	2. Code reordering guarentees that
			%	there is no other value that _requires_
			%	this register.
		(
			{ TargetReg = reg(Reg) }
		->
			code_info__add_variable_to_register(Var, Reg)
		;
			{ true }
		),
		code_info__add_lvalue_to_variable(TargetReg, Var)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

	% unused - do nothing.
code_info__generate_expression(unused, _, empty) --> [].
code_info__generate_expression(lval(Lval), TargetReg, Code) -->
	(
		{ Lval = TargetReg }
	->
		{ Code = empty }
	;
		{ Code = node([
			assign(TargetReg, lval(Lval)) - "Copy lvalue"
		]) }
	).
code_info__generate_expression(var(Var), TargetReg, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = evaluated(Lvals0) }
	->
		(
			{ code_info__select_lvalue(Lvals0, TargetReg, Lval0) },
			{ Lval0 \= TargetReg }
		->
			code_info__make_assignment_comment(Var,
							TargetReg, Comment),
			{ Code = node([
				assign(TargetReg, lval(Lval0)) - Comment
			]) }
		;
			{ Code = empty }
		)
	;
		{ VarStat = cached(Exprn, target(TargetReg1)) }
	->
		(
			{ TargetReg \= TargetReg1 }
		->
			code_info__generate_expression(Exprn,
							TargetReg1, Code0),
				% Mark the target register.
			code_info__add_lvalue_to_variable(TargetReg1, Var),
				% Assemble the code
			code_info__make_assignment_comment(Var,
							TargetReg, Comment),
			{ Code1 = node([
				assign(TargetReg, lval(TargetReg1)) - Comment
			]) }
		;
			{ Code0 = empty },
			{ Code1 = empty }
		),
		{ Code = tree(Code0, Code1) }
	;
		{ VarStat = cached(Exprn, none) }
	->
		code_info__generate_expression(Exprn, TargetReg, Code0),
			% Mark the target register.
		code_info__add_lvalue_to_variable(TargetReg, Var),
			% Assemble the code
		{ Code = Code0 }
	;
		% { VarStat = unused }
		{ Code = empty }
	).
code_info__generate_expression(binop(Op, L0, R0), TargetReg, Code) -->
	code_info__generate_expression_vars(L0, L, Code0),
	code_info__generate_expression_vars(R0, R, Code1),
	{ ThisCode = node([
		assign(TargetReg, binop(Op, L, R)) -
				"Evaluate binary expression"
	]) },
	{ Code2 = tree(Code1, ThisCode) },
	{ Code = tree(Code0, Code2) }.
code_info__generate_expression(create(Tag, Args), TargetReg, Code) -->
	{ list__length(Args, Arity) }, % includes possible tag word
	(
		{ Arity > 0 }
	->
		{ CodeA = node([
			assign(TargetReg, lval(hp)) - "Get the heap memory",
			incr_hp(Arity) - "Allocate heap space",
			assign(TargetReg, mkword(Tag, lval(TargetReg))) -
							"Tag the pointer"
		]) }
	;
		{ CodeA = node([
			assign(TargetReg, mkword(Tag, iconst(0))) -
					"Assign a constant"
		]) }
	),
	code_info__generate_cons_args(TargetReg, Tag, 0, Args, CodeB),
	{ Code = tree(CodeA, CodeB) }.
code_info__generate_expression(mkword(Tag, Rval0), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = node([
		assign(TargetReg, mkword(Tag, Rval)) - "Tag a word"
	]) },
	{ Code = tree(Code0, Code1) }.
code_info__generate_expression(mkbody(Rval0), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = node([
		assign(TargetReg, mkbody(Rval)) - "Tag a word"
	]) },
	{ Code = tree(Code0, Code1) }.
code_info__generate_expression(body(Rval0), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = node([
		assign(TargetReg, body(Rval)) - "Tag a word"
	]) },
	{ Code = tree(Code0, Code1) }.
code_info__generate_expression(iconst(Int), TargetReg, Code) -->
	{ Code = node([
		assign(TargetReg, iconst(Int)) - "Make an integer const"
	]) }.
code_info__generate_expression(sconst(Str), TargetReg, Code) -->
	{ Code = node([
		assign(TargetReg, sconst(Str)) - "Make a string const"
	]) }.
code_info__generate_expression(field(Tag, Rval0, Field), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = node([
		assign(TargetReg, field(Tag, Rval, Field)) -
						"extract a field of a term"
	]) },
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_cons_args(lval, int, int, list(rval),
				code_tree, code_info, code_info).
:- mode code_info__generate_cons_args(in, in, in, in, out, in, out) is det.

:- code_info__generate_cons_args(_, _, _, X, _, _, _) when X. % indexing

code_info__generate_cons_args(_Reg, _Tag, _Field0, [], empty) --> [].
code_info__generate_cons_args(Reg, Tag, Field0, [Arg|Args], Code) -->
	code_info__generate_expression(Arg, field(Tag, Reg, Field0), Code0),
	{ Field1 is Field0 + 1 },
	code_info__generate_cons_args(Reg, Tag, Field1, Args, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_expression_vars(rval, rval, code_tree,
							code_info, code_info).
:- mode code_info__generate_expression_vars(in, out, out, in, out) is det.

code_info__generate_expression_vars(unused, unused, empty) --> [].
code_info__generate_expression_vars(lval(Lval), lval(Lval), empty) --> [].
code_info__generate_expression_vars(var(Var), Result, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = evaluated(Lvals0) },
		{ set__to_sorted_list(Lvals0, [Lval0|_]) }
	->
		{ Result = lval(Lval0) },
		{ Code = empty }
	;
		{ VarStat = cached(Exprn, Target) }
	->
		code_info__target_to_lvalue(Target, Exprn, Var, TargetReg),
		code_info__generate_expression(Exprn, TargetReg, Code),
		(
			{ TargetReg = reg(Reg) }
		->
			code_info__add_variable_to_register(Var, Reg)
		;
			{ true }
		),
		code_info__add_lvalue_to_variable(TargetReg, Var),
		{ Result = lval(TargetReg) }
	;
		% { VarStat = unused },
		{ Result = unused },
		{ Code = empty }
	).
code_info__generate_expression_vars(binop(Op, L0, R0),
						binop(Op, L, R), Code) -->
	code_info__generate_expression_vars(L0, L, Code0),
	code_info__generate_expression_vars(R0, R, Code1),
	{ Code = tree(Code0, Code1) }.
code_info__generate_expression_vars(create(Tag, Rvals0), create(Tag, Rvals),
								Code) -->
	code_info__generate_expression_vars_2(Rvals0, Rvals, Code).
code_info__generate_expression_vars(mkword(Tag, Rval0), mkword(Tag, Rval),
								Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(mktag(Rval0), mktag(Rval), Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(mkbody(Rval0), mkbody(Rval), Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(body(Rval0), body(Rval), Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(iconst(Int), iconst(Int), empty) --> [].
code_info__generate_expression_vars(sconst(Str), sconst(Str), empty) --> [].
code_info__generate_expression_vars(field(Tag, Rval0, Field),
					field(Tag, Rval, Field), Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).

%---------------------------------------------------------------------------%

:- pred code_info__generate_expression_vars_2(list(rval), list(rval), 
				code_tree, code_info, code_info).
:- mode code_info__generate_expression_vars_2(in, out, out, in, out) is det.

code_info__generate_expression_vars_2([], [], empty) --> [].
code_info__generate_expression_vars_2([R0|Rs0], [R|Rs], Code) -->
	code_info__generate_expression_vars(R0, R, Code0),
	code_info__generate_expression_vars_2(Rs0, Rs, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred code_info__target_to_lvalue(target_register, rval, var, lval,
							code_info, code_info).
:- mode code_info__target_to_lvalue(in, in, in, out, in, out) is det.

code_info__target_to_lvalue(target(Lvalue), _Exprn, Var, Lvalue) -->
	(
		code_info__lval_is_free_reg(Lvalue)
	->
		(
			{ Lvalue = reg(R) }
		->
			code_info__add_variable_to_register(Var, R)
		;
			{ true }
		)
	;
		{ error("Target is a live register!") }
	).
code_info__target_to_lvalue(none, Exprn, Var, Lvalue) -->
	(
		code_info__evaluated_expression(Exprn, Lval0)
	->
		{ Lvalue = Lval0 }
	;
		code_info__current_store_map(StoreMap),
		{ map__search(StoreMap, Var, Lval1) },
		code_info__lval_is_free_reg(Lval1)
	->
		{ Lvalue = Lval1 },
		(
			{ Lval1 = reg(R) }
		->
			code_info__add_variable_to_register(Var, R)
		;
			{ true }
		)
	;
		code_info__get_free_register(Reg),
		code_info__add_variable_to_register(Var, Reg),
		{ Lvalue = reg(Reg) }
	).

:- pred code_info__lval_is_free_reg(lval, code_info, code_info).
:- mode code_info__lval_is_free_reg(in, in, out) is semidet.

code_info__lval_is_free_reg(Lval) -->
	(
		{ Lval = reg(Reg) }
	->
		code_info__get_registers(Registers),
		(
			{ map__search(Registers, Reg, RegStat) }
		->
			(
				{ RegStat = vars(_) }
			->
				{ fail }
			;
				{ RegStat = reserved }
			->
				{ fail }
			)
		;
			{ true }
		)
	;
		{ true }
	).

:- pred code_info__evaluated_expression(rval, lval, code_info, code_info).
:- mode code_info__evaluated_expression(in, out, in, out) is semidet.


code_info__evaluated_expression(lval(Lval), Lval) --> [].
code_info__evaluated_expression(var(Var), Lval) -->
	code_info__get_variables(Variables),
	{ map__lookup(Variables, Var, VarStat) },
	(
		{ VarStat = evaluated(Lvals0) },
		{ set__to_sorted_list(Lvals0, [Lval0|_]) }
	->
		{ Lval = Lval0 }
	;
		{ VarStat = cached(Exprn,_) }
	->
		code_info__evaluated_expression(Exprn, Lval),
		code_info__add_lvalue_to_variable(Lval, Var)
	;
		{ fail }
	).

%---------------------------------------------------------------------------%

:- pred code_info__select_lvalue(set(lval), lval, lval).
:- mode code_info__select_lvalue(in, in, out) is det.

code_info__select_lvalue(Lvals, Target, Lval) :-
	(
		set__member(Target, Lvals)
	->
		Lval = Target
	;
		set__to_sorted_list(Lvals, [Lval0|_])
	->
		Lval = Lval0
	;
		error("No lval to select")
	).

%---------------------------------------------------------------------------%

code_info__cache_expression(Var, Exprn) -->
	(
			% If we are simply forwarding a new
			% name to an existing value, then
			% don't cache the expression.
		code_info__evaluated_expression(Exprn, Lval)
	->
		code_info__add_lvalue_to_variable(Lval, Var),
		(
			{ Lval = reg(Reg) }
		->
			code_info__add_variable_to_register(Var, Reg)
		;
			{ true }
		)
	;
		code_info__get_variables(Variables0),
		{ map__set(Variables0, Var, cached(Exprn, none), Variables) },
		code_info__set_variables(Variables)
	).

%---------------------------------------------------------------------------%

code_info__cache_expression_with_target(Var, Exprn, TargetReg) -->
	code_info__get_variables(Variables0),
	{ map__set(Variables0, Var, cached(Exprn, target(TargetReg)),
								Variables) },
	code_info__set_variables(Variables).

%---------------------------------------------------------------------------%

code_info__setup_call([], _Args, _CallDirection, empty) --> [].
code_info__setup_call([Var - arg_info(ArgLoc, Mode)|Vars], Args, CallDirection,
		Code) -->
	(
		( { Mode = top_in, CallDirection = caller }
		; { Mode = top_out, CallDirection = callee }
		)
	->
		{ code_util__arg_loc_to_register(ArgLoc, Reg) },
		(
			code_info__variable_register(Var, Lval0),
			{ Lval0 = reg(Reg0) },
			{ Reg \= Reg0 }
		->
			code_info__shuffle_register(Var, Args, Reg, CodeA)
		;
			code_info__variable_register(Var, Lval1),
			{ Lval1 = reg(_) }
		->
			{ CodeA = empty }
		;
			code_info__shuffle_register(Var, Args, Reg, CodeA)
		)
	;
		{ CodeA = empty }
	),
	(
		{ Args = [_|Args1] }
	->
		{ Args2 = Args1 }
	;
		{ error("Vanishing arguments!") }
	),
	code_info__setup_call(Vars, Args2, CallDirection, CodeB),
	{ Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

code_info__shuffle_register(Var, Args, Reg, Code) -->
	code_info__get_registers(Registers),
	(
		{ map__search(Registers, Reg, RegContents) }
	->
			% Generate code to swap the contents of
			% the register out of the way
		code_info__shuffle_registers_2(Reg, Args, RegContents, CodeA)
	;
		{ CodeA = empty }
	),
		% generate the code to place the variable in
		% the desired register
	code_info__generate_expression(var(Var), reg(Reg), CodeB),
	code_info__add_lvalue_to_variable(reg(Reg), Var),
	code_info__add_variable_to_register(Var, Reg),
	{ Code = tree(CodeA, CodeB) }.

:- pred code_info__shuffle_registers_2(reg, list(var), register_stat,
					code_tree, code_info, code_info).
:- mode code_info__shuffle_registers_2(in, in, in, out, in, out) is det.

code_info__shuffle_registers_2(Reg, Args, Contents, Code) -->
	(
		{ Contents = unused }
	->
		{ Code = empty }
	;
		{ Contents = reserved }
	->
		{ error("Cannot shuffle a reserved register.") }
	;
		{ Contents = vars(Vars) },
		code_info__variables_are_live(Vars)
	->
			% get a spare register
		code_info__get_next_free_register(Reg, NewReg),
			% Update the register info -
			% remove the entry for the old register,
			% and set the entry for the new register.
		code_info__add_variables_to_register(Vars, NewReg),
			% Update the variable info -
			% Set the location of the variable to the
			% new register.
		code_info__remap_variables(Vars, reg(Reg), reg(NewReg)),
			% Generate the code fragment.
		{ Code = node([
			assign(reg(NewReg), lval(reg(Reg))) -
				"Swap variable to a new register"
		]) }
	;
		{ Contents = vars(Vars1) },
		{ set__to_sorted_list(Vars1, Vars2) },
		{ code_info__variables_are_args(Vars2, Args) }
	->
			% get a spare register
		code_info__get_free_register(NewReg),
			% Update the register info -
			% remove the entry for the old register,
			% and set the entry for the new register.
		code_info__add_variables_to_register(Vars1, NewReg),
			% Update the variable info -
			% Set the location of the variable to the
			% new register.
		code_info__remap_variables(Vars1, reg(Reg), reg(NewReg)),
			% Generate the code fragment.
		{ Code = node([
			assign(reg(NewReg), lval(reg(Reg))) -
				"Swap variable to a new register"
		]) }
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

:- pred code_info__variables_are_args(list(var), list(var)).
:- mode code_info__variables_are_args(in, in) is semidet.

code_info__variables_are_args([], _Args) :- fail.
code_info__variables_are_args([Var|Vars], Args) :-
	(
		list__member(Var, Args)
	->
		true
	;
		code_info__variables_are_args(Vars, Args)
	).

%---------------------------------------------------------------------------%

code_info__save_variable_on_stack(Var, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = unused }
	->
		{ Code = empty }
	;
		{ VarStat = evaluated(_) }
	->
		code_info__save_variable_in_slot(Var, Code)
	;
		{ VarStat = cached(_Exprn, none) }
	->
		code_info__save_variable_in_slot(Var, Code)
	;
		{ VarStat = cached(Exprn, target(TargetReg)) }
	->
		code_info__generate_expression(Exprn, TargetReg, Code0),
		code_info__add_lvalue_to_variable(TargetReg, Var),
		code_info__save_variable_in_slot(Var, Code1),
		{ Code = tree(Code0, Code1) }
	;
		{ error("This should never happen") }
	).

%---------------------------------------------------------------------------%

code_info__reserve_register(Reg) -->
	code_info__get_registers(Registers0),
	(
		{ map__search(Registers0, Reg, reserved) }
	->
		{ error("Cannot reserve a live register.") }
	;
		{ map__set(Registers0, Reg, reserved, Registers) },
		code_info__set_registers(Registers)
	).

%---------------------------------------------------------------------------%

code_info__get_next_label(Label) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_count(N0),
	{ N is N0 + 1 },
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_label(ModuleInfo, PredId, ProcId, N, Label) },
	code_info__set_label_count(N).

%---------------------------------------------------------------------------%

code_info__get_current_label(Label) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_count(N),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_label(ModuleInfo, PredId, ProcId, N, Label) }.

%---------------------------------------------------------------------------%

code_info__get_variable_register(Var, Lval) -->
	code_info__get_variables(Variables),
	(
		{ map__search(Variables, Var, VarStat) },
		{ VarStat = evaluated(Lvals0) },
		{ set__to_sorted_list(Lvals0, [Lval0|_]) }
	->
		{ Lval = Lval0 }
	;
		{ error("Cannot find lvalue of variable") }
	).

%---------------------------------------------------------------------------%

code_info__variable_register(Var, Lval) -->
	code_info__get_variables(Variables),
	{ map__search(Variables, Var, VarStat) },
	{ VarStat = evaluated(Lvals) },
	{ set__to_sorted_list(Lvals, [Lval|_]) }.

%---------------------------------------------------------------------------%

code_info__variable_has_register(Var, Lval) -->
	code_info__get_variables(Variables),
	{ map__search(Variables, Var, VarStat) },
	{ VarStat = evaluated(Lvals) },
	{ set__member(Lval, Lvals) }.

%---------------------------------------------------------------------------%

code_info__register_variables(Reg, Vars) -->
	code_info__get_registers(Registers),
	{ map__search(Registers, Reg, RegStat) },
	{ RegStat = vars(Vars) }.

%---------------------------------------------------------------------------%

code_info__set_variable_register(Var, Reg) -->
	code_info__add_lvalue_to_variable(reg(Reg), Var),
	code_info__add_variable_to_register(Var, Reg).

%---------------------------------------------------------------------------%

code_info__reset_variable_target(Var, Lval) -->
	code_info__get_variables(Variables0),
	{ map__search(Variables0, Var, VarStat) },
	{ VarStat = cached(Exprn, _) },
	{ map__set(Variables0, Var,
				cached(Exprn, target(Lval)), Variables) },
	code_info__set_variables(Variables).

%---------------------------------------------------------------------------%

:- pred code_info__save_variable_in_slot(var, code_tree, code_info, code_info).
:- mode code_info__save_variable_in_slot(in, out, in, out) is det.

code_info__save_variable_in_slot(Var, Code) -->
	code_info__get_variable_slot(Var, Slot),
	code_info__generate_expression(var(Var), Slot, Code),
	code_info__add_lvalue_to_variable(Slot, Var).

%---------------------------------------------------------------------------%

:- pred code_info__get_variable_slot(var, lval, code_info, code_info).
:- mode code_info__get_variable_slot(in, out, in, out) is det.

code_info__get_variable_slot(Var, Slot) -->
	code_info__get_call_info(CallInfo),
	{ map__lookup(CallInfo, Var, Slot) }.

%---------------------------------------------------------------------------%

code_info__clear_all_registers -->
	{ map__init(Registers) },
	code_info__set_registers(Registers),
	code_info__get_variables(Variables0),
	{ map__init(Variables) },
	code_info__set_variables(Variables),
	{ map__to_assoc_list(Variables0, Variables1) },
	code_info__reconstruct_variables(Variables1).

:- pred code_info__reconstruct_variables(assoc_list(var, variable_stat),
							code_info, code_info).
:- mode code_info__reconstruct_variables(in, in, out) is det.

code_info__reconstruct_variables([]) --> [].
code_info__reconstruct_variables([Var - VarStat|VarStats]) -->
	(
		{ VarStat = evaluated(Lvals0) }
	->
		{ set__to_sorted_list(Lvals0, LvalList) },
		code_info__reenter_lvals(Var, LvalList)
	;
		{ true }
	),
	code_info__reconstruct_variables(VarStats).

:- pred code_info__reenter_lvals(var, list(lval), code_info, code_info).
:- mode code_info__reenter_lvals(in, in, in, out) is det.

code_info__reenter_lvals(_Var, []) --> [].
code_info__reenter_lvals(Var, [L|Ls]) -->
	(
		{ L = stackvar(_) }
	->
		code_info__add_lvalue_to_variable(L, Var)
	;
		{ L = framevar(_) }
	->
		code_info__add_lvalue_to_variable(L, Var)
	;
		{ true }
	),
	code_info__reenter_lvals(Var, Ls).

%---------------------------------------------------------------------------%

	% Given a constructor id, and a variable (so that we can work out the
	% type of the constructor), determine correct tag (representation)
	% of that constructor.

:- code_info__cons_id_to_tag(_, X, _, _, _) when X. % NU-Prolog indexing.

code_info__cons_id_to_tag(_Var, int_const(X), int_constant(X)) --> [].
code_info__cons_id_to_tag(_Var, float_const(X), float_constant(X)) --> [].
code_info__cons_id_to_tag(_Var, string_const(X), string_constant(X)) --> [].
code_info__cons_id_to_tag(Var, cons(Name, Arity), Tag) -->
		%
		% Use the variable to determine the type_id
		%
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) },
	{ type_to_type_id(Type, TypeId0, _) ->
		TypeId = TypeId0
	;
		error("cons_info__cons_id_to_tag: invalid type")
	},

		%
		% Given the type_id, lookup up the constructor tag
		% table for that type
		%
	code_info__get_module_info(ModuleInfo),
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) },
	{ TypeDefn = hlds__type_defn(_, _, du_type(_, ConsTable0, _), _, _) ->
		ConsTable = ConsTable0
	;
		error("code_info__cons_id_to_tag: type is not d.u. type?")
	},
		% Finally look up the cons_id in the table
	{ map__lookup(ConsTable, cons(Name, Arity), Tag) }.

%---------------------------------------------------------------------------%

code_info__variable_is_live(Var) -->
	code_info__get_liveness_info(Liveness),
	{ set__member(Var, Liveness) }.

:- pred code_info__variables_are_live(set(var), code_info, code_info).
:- mode code_info__variables_are_live(in, in, out) is semidet.

code_info__variables_are_live(Vars) -->
	{ set__to_sorted_list(Vars, VarList) },
	code_info__variables_are_live_2(VarList).

:- pred code_info__variables_are_live_2(list(var), code_info, code_info).
:- mode code_info__variables_are_live_2(in, in, out) is semidet.

code_info__variables_are_live_2([]) --> { fail }.
code_info__variables_are_live_2([V|Vs]) -->
	(
		code_info__variable_is_live(V)
	->
		{ true }
	;
		code_info__variables_are_live_2(Vs)
	).

%---------------------------------------------------------------------------%

code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_arg_info(ProcInfo, ArgInfo) }.

%---------------------------------------------------------------------------%

:- pred code_info__get_variable(var, variable_stat, code_info, code_info).
:- mode code_info__get_variable(in, out, in, out) is det.

code_info__get_variable(Var, VarLoc) -->
	code_info__get_variables(Variables),
	(
		{ map__search(Variables, Var, VarStat) }
	->
		{ VarLoc = VarStat }
	;
		{ error("Variable not in register or on stack.") }
	).

%---------------------------------------------------------------------------%

code_info__get_arginfo(ArgInfo) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo).

%---------------------------------------------------------------------------%

code_info__get_live_variables(Variables) -->
	code_info__get_variables(Variables0),
	{ map__keys(Variables0, Variables1) },
	code_info__get_live_variables_2(Variables1, Variables).

:- pred code_info__get_live_variables_2(list(var), list(var), code_info,
								code_info).
:- mode code_info__get_live_variables_2(in, out, in, out) is det.

code_info__get_live_variables_2([], []) --> [].
code_info__get_live_variables_2([Var|Vars0], Vars) -->
	code_info__get_live_variables_2(Vars0, Vars1),
	(
		code_info__variable_is_live(Var)
	->
		{ Vars = [Var|Vars1] }
	;
		{ Vars = Vars1 }
	).

%---------------------------------------------------------------------------%

code_info__generate_forced_saves(Code) --> 
	code_info__get_variables(Variables),
	{ map__keys(Variables, Vars) },
	code_info__generate_forced_saves_2(Vars, Code).

:- pred code_info__generate_forced_saves_2(list(var), code_tree,
						code_info, code_info).
:- mode code_info__generate_forced_saves_2(in, out, in, out) is det.

code_info__generate_forced_saves_2([], empty) --> [].
code_info__generate_forced_saves_2([Var|Vars], Code) -->
	(
		code_info__variable_is_live(Var),
		code_info__get_variables(Variables),
		{ map__contains(Variables, Var) }
	->
		code_info__current_store_map(Store),
		(
			{ map__search(Store, Var, Lval) },
			{ Lval = reg(Reg) }	
		->
			% if the target location is not free
			% then we better swap the live value to
			% a spare register
			(
				code_info__lval_is_free_reg(Lval)
			->
				{ CodeA = empty }
			;
				% unless of course, it is already there...
				code_info__variable_has_register(Var, Lval)
			->
				{ CodeA = empty }
			;
				code_info__swap_out_reg(Reg, CodeA)
			),
			code_info__generate_expression(var(Var), Lval, CodeB),
			code_info__add_lvalue_to_variable(Lval, Var),
			(
				{ Lval = reg(R) }
			->
				code_info__add_variable_to_register(Var, R)
			;
				{ true }
			),
			{ Code0 = tree(CodeA, CodeB) }
		;
			% If it wasn't in the store-map, then
			% check the call info to see if it has
			% a stack location assigned to it.
			code_info__get_call_info(CallInfo),
			(
				{ map__search(CallInfo, Var, StackThing) }
			->
				code_info__generate_expression(var(Var),
							StackThing, Code0),
				code_info__add_lvalue_to_variable(StackThing,
									Var)
			;
				% if it wasn't there, then flush the
				% variable, and let it land where-ever
				code_info__flush_variable(Var, Code0)
			)
		),
		code_info__generate_forced_saves_2(Vars, RestCode),
		{ Code = tree(Code0, RestCode) }
	;
			% This case should only occur in the presence
			% of `erroneous' or `failure' procedures, i.e.
			% procedures which never succeed.
		code_info__variable_is_live(Var)
	->
		code_info__current_store_map(StoreMap),
		(
			{ map__search(StoreMap, Var, Lval) }
		->
			code_info__add_lvalue_to_variable(Lval, Var),
			(
				{ Lval = reg(R) }
			->
				code_info__add_variable_to_register(Var, R)
			;
				{ true }
			)
		;
			% If it wasn't in the followvars, then
			% check the call info to see if it has
			% a stack location assigned to it.
			code_info__get_call_info(CallInfo),
			(
				{ map__search(CallInfo, Var, StackThing) }
			->
				code_info__add_lvalue_to_variable(StackThing,
									Var)
			;
				{ error("Panic!") }
			)
		),
		code_info__generate_forced_saves_2(Vars, Code)
	;
		code_info__generate_forced_saves_2(Vars, Code)
	).

%---------------------------------------------------------------------------%

code_info__generate_nondet_saves(Code) -->
	code_info__get_call_info(CallInfo),
	{ map__to_assoc_list(CallInfo, CallList) },
	code_info__generate_nondet_saves_2(CallList, Code),
	code_info__remake_with_call_info.

:- pred code_info__generate_nondet_saves_2(assoc_list(var, lval), code_tree,
							code_info, code_info).
:- mode code_info__generate_nondet_saves_2(in, out, in, out) is det.

code_info__generate_nondet_saves_2([], empty) --> [].
code_info__generate_nondet_saves_2([Var - StackThing|VarSlots], Code) --> 
	(
		code_info__variable_is_live(Var),
		code_info__get_variables(Variables),
		{ map__contains(Variables, Var) }
	->
		code_info__generate_expression(var(Var), StackThing, Code0),
		code_info__add_lvalue_to_variable(StackThing, Var),
		code_info__generate_nondet_saves_2(VarSlots, RestCode),
		{ Code = tree(Code0, RestCode) }
	;
			% This case should only occur in the presence
			% of `erroneous' or `failure' procedures, i.e.
			% procedures which never succeed.
		code_info__variable_is_live(Var)
	->
		code_info__add_lvalue_to_variable(StackThing, Var),
		code_info__generate_nondet_saves_2(VarSlots, Code)
	;
		code_info__generate_nondet_saves_2(VarSlots, Code)
	).

%---------------------------------------------------------------------------%

:- pred code_info__swap_out_reg(reg, code_tree, code_info, code_info).
:- mode code_info__swap_out_reg(in, out, in, out) is det.

code_info__swap_out_reg(Reg, Code) -->
	code_info__get_registers(Registers),
	(
		{ map__search(Registers, Reg, RegContents) }
	->
			% Generate code to swap the contents of
			% the register out of the way
		code_info__shuffle_registers_2(Reg, [], RegContents, Code)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

code_info__get_headvars(HeadVars) -->
	code_info__get_module_info(ModuleInfo),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_headvars(ProcInfo, HeadVars) }.

%---------------------------------------------------------------------------%

code_info__remake_with_store_map -->
	code_info__current_store_map(StoreMap),
	{ map__to_assoc_list(StoreMap, StoreList) },
	{ map__init(Variables0) },
	code_info__set_variables(Variables0),
	code_info__remake_with_store_map_2(StoreList),
	code_info__get_variables(Variables1),
	{ map__to_assoc_list(Variables1, VarList) },
	{ map__init(Registers) },
	code_info__set_registers(Registers),
	code_info__reconstruct_registers(VarList).

%---------------------------------------------------------------------------%

:- pred code_info__remake_with_store_map_2(assoc_list(var, lval),
						code_info, code_info).
:- mode code_info__remake_with_store_map_2(in, in, out) is det.

code_info__remake_with_store_map_2([]) --> [].
code_info__remake_with_store_map_2([V - ST|VSs]) -->
	(
		code_info__variable_is_live(V)
	->
		code_info__current_store_map(StoreMap),
		(
			{ map__search(StoreMap, V, L0) }
		->
			code_info__add_lvalue_to_variable(L0, V),
			(
				{ L0 = reg(R0) }
			->
				code_info__add_variable_to_register(V, R0)
			;
				{ true }
			)
		;
			code_info__add_lvalue_to_variable(ST, V)
		)
	;
		{ true }
	),
	code_info__remake_with_store_map_2(VSs).

%---------------------------------------------------------------------------%

code_info__remake_with_call_info -->
	code_info__get_call_info(CallInfo),
	{ map__to_assoc_list(CallInfo, CallList) },
	{ map__init(Variables0) },
	code_info__set_variables(Variables0),
	code_info__remake_with_call_info_2(CallList),
	code_info__get_variables(Variables1),
	{ map__to_assoc_list(Variables1, VarList) },
	{ map__init(Registers) },
	code_info__set_registers(Registers),
	code_info__reconstruct_registers(VarList).

%---------------------------------------------------------------------------%

:- pred code_info__remake_with_call_info_2(assoc_list(var, lval),
						code_info, code_info).
:- mode code_info__remake_with_call_info_2(in, in, out) is det.

code_info__remake_with_call_info_2([]) --> [].
code_info__remake_with_call_info_2([V - ST|VSs]) -->
	(
		code_info__variable_is_live(V)
	->
		code_info__add_lvalue_to_variable(ST, V)
	;
		{ true }
	),
	code_info__remake_with_call_info_2(VSs).

%---------------------------------------------------------------------------%

code_info__update_liveness_info(Births - _Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__union(Liveness0, Births, Liveness) },
	code_info__set_liveness_info(Liveness).

code_info__update_deadness_info(_Births - Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__difference(Liveness0, Deaths, Liveness) },
	code_info__set_liveness_info(Liveness).

%---------------------------------------------------------------------------%

code_info__reduce_variables_and_registers -->
	code_info__get_live_variables(Vars),
	{ map__init(Variables0) },
	code_info__reduce_variables_and_registers_2(Vars, Variables0).

:- pred code_info__reduce_variables_and_registers_2(list(var),
				variable_info, code_info, code_info).
:- mode code_info__reduce_variables_and_registers_2(in, in, in, out)
								is det.

code_info__reduce_variables_and_registers_2([], Variables) -->
	code_info__set_variables(Variables),
	{ map__to_assoc_list(Variables, VarList) },
	{ map__init(Registers) },
	code_info__set_registers(Registers),
	code_info__reconstruct_registers(VarList).
code_info__reduce_variables_and_registers_2([Var|Vars], Variables0) -->
	(
		code_info__get_variables(VariablesA),
		{ map__search(VariablesA, Var, VarStat) }
	->
		{ code_info__update_variables(Var, VarStat,
						Variables0, Variables1) },
		code_info__variable_dependencies(Var, VarStat,
						Variables1, Variables)
	;
		{ error("Live variable not found!") }
	),
	code_info__reduce_variables_and_registers_2(Vars, Variables).

%---------------------------------------------------------------------------%

:- pred code_info__variable_dependencies(var, variable_stat,
					variable_info, variable_info, 
							code_info, code_info).
:- mode code_info__variable_dependencies(in, in, in, out, in, out) is det.

code_info__variable_dependencies(_Var, unused, V, V) --> [].
code_info__variable_dependencies(_Var, evaluated(_Lvals), V, V) --> [].
code_info__variable_dependencies(Var, cached(Exprn, _), V0, V) -->
	code_info__expression_dependencies(Var, Exprn, V0, V).

%---------------------------------------------------------------------------%

:- pred code_info__expressions_dependencies(var, list(rval),
				variable_info, variable_info,
						code_info, code_info).
:- mode code_info__expressions_dependencies(in, in, in, out, in, out) is det.

:- code_info__expressions_dependencies(_, X, _, _, _, _) when X. % Indexing

code_info__expressions_dependencies(_Var, [], V, V) --> [].
code_info__expressions_dependencies(Var, [R|Rs], V0, V) -->
	code_info__expression_dependencies(Var, R, V0, V1),
	code_info__expressions_dependencies(Var, Rs, V1, V).

:- pred code_info__expression_dependencies(var, rval,
				variable_info, variable_info,
						code_info, code_info).
:- mode code_info__expression_dependencies(in, in, in, out, in, out) is det.

:- code_info__expression_dependencies(_, X, _, _, _, _) when X. % Indexing

code_info__expression_dependencies(_Var, lval(_Lval), V, V) --> [].
code_info__expression_dependencies(_Var, var(Var0), V0, V) -->
	(
		code_info__get_variables(VariablesA),
		{ map__search(VariablesA, Var0, VarStat) }
	->
		{ code_info__update_variables(Var0, VarStat, V0, V1) },
		code_info__variable_dependencies(Var0, VarStat, V1, V)
	;
		{ error("Live variable not found!") }
	).
code_info__expression_dependencies(Var, binop(_, E0, E1), V0, V) -->
	code_info__expression_dependencies(Var, E0, V0, V1),
	code_info__expression_dependencies(Var, E1, V1, V).
code_info__expression_dependencies(Var, field(_, Rval,_), V0, V) -->
	code_info__expression_dependencies(Var, Rval, V0, V).
code_info__expression_dependencies(Var, mkword(_Tag, Rval), V0, V) -->
	code_info__expression_dependencies(Var, Rval, V0, V).
code_info__expression_dependencies(Var, mkbody(Rval), V0, V) -->
	code_info__expression_dependencies(Var, Rval, V0, V).
code_info__expression_dependencies(Var, body(Rval), V0, V) -->
	code_info__expression_dependencies(Var, Rval, V0, V).
code_info__expression_dependencies(_Var, mktag(_), V, V) --> [].
code_info__expression_dependencies(Var, create(_Tag, Rvals), V0, V) -->
	code_info__expressions_dependencies(Var, Rvals, V0, V).
code_info__expression_dependencies(_Var, iconst(_), V, V) --> [].
code_info__expression_dependencies(_Var, sconst(_), V, V) --> [].

%---------------------------------------------------------------------------%

:- pred code_info__update_variables(var, variable_stat, variable_info,
						variable_info).
:- mode code_info__update_variables(in, in, in, out) is det.

code_info__update_variables(Var, VarStat, Variables0, Variables) :-
	(
		map__insert(Variables0, Var, VarStat, Variables1)
	->
		Variables = Variables1
	;
		Variables = Variables0
	).

:- pred code_info__update_variables_2(list(var), lval,
					variable_info, variable_info).
:- mode code_info__update_variables_2(in, in, in, out) is det.

code_info__update_variables_2([], _L, V, V).
code_info__update_variables_2([Var|Vars], Lval, V0, V) :-
	set__singleton_set(Lvals, Lval),
	code_info__update_variables(Var, evaluated(Lvals), V0, V1),
	code_info__update_variables_2(Vars, Lval, V1, V).

%---------------------------------------------------------------------------%

:- pred code_info__reconstruct_registers(assoc_list(var, variable_stat),
						code_info, code_info).
:- mode code_info__reconstruct_registers(in, in, out) is det.

code_info__reconstruct_registers([]) --> [].
code_info__reconstruct_registers([Var - VarStat|VarStats]) -->
	(
		{ VarStat = evaluated(Lvals) }
	->
		{ set__to_sorted_list(Lvals, LvalList) },
		code_info__reenter_registers(Var, LvalList)
	;
		{ true }
	),
	code_info__reconstruct_registers(VarStats).

:- pred code_info__reenter_registers(var, list(lval), code_info, code_info).
:- mode code_info__reenter_registers(in, in, in, out) is det.

:- code_info__reenter_registers(_, X, _, _) when X. % Indexing

code_info__reenter_registers(_Var, []) --> [].
code_info__reenter_registers(Var, [L|Ls]) -->
	(
		{ L = reg(R) }
	->
		code_info__add_variable_to_register(Var, R)
	;
		{ L = field(_, reg(R1), _) }
	->
		code_info__add_variable_to_register(Var, R1)
	;
		{ true }
	),
	code_info__reenter_registers(Var, Ls).

%---------------------------------------------------------------------------%

:- pred code_info__add_variable_to_register(var, reg, code_info, code_info).
:- mode code_info__add_variable_to_register(in, in, in, out) is det.

code_info__add_variable_to_register(Var, Reg) -->
	code_info__get_registers(Registers0),
	(
		{ map__search(Registers0, Reg, vars(Vars0)) }
	->
		{ set__insert(Vars0, Var, Vars) }
	;
		{ set__singleton_set(Vars, Var) }
	),
	{ map__set(Registers0, Reg, vars(Vars), Registers) },
	code_info__set_registers(Registers).

:- pred code_info__add_variables_to_register(set(var), reg,
						code_info, code_info).
:- mode code_info__add_variables_to_register(in, in, in, out) is det.

code_info__add_variables_to_register(Vars0, Reg) -->
	{ set__to_sorted_list(Vars0, Vars) },
	code_info__add_variables_to_register_2(Vars, Reg).

:- pred code_info__add_variables_to_register_2(list(var), reg,
						code_info, code_info).
:- mode code_info__add_variables_to_register_2(in, in, in, out) is det.

code_info__add_variables_to_register_2([], _Reg) --> [].
code_info__add_variables_to_register_2([Var|Vars], Reg) -->
	code_info__add_variable_to_register(Var, Reg),
	code_info__add_variables_to_register_2(Vars, Reg).

%---------------------------------------------------------------------------%

code_info__add_lvalue_to_variable(Lval, Var) -->
	code_info__get_variables(Variables0),
	(
		{ map__search(Variables0, Var, evaluated(Lvals0)) }
	->
		{ set__insert(Lvals0, Lval, Lvals) }
	;
		{ set__singleton_set(Lvals, Lval) }
	),
	{ map__set(Variables0, Var, evaluated(Lvals), Variables) },
	code_info__set_variables(Variables).

:- pred code_info__add_lvalues_to_variable(set(lval), var,
						code_info, code_info).
:- mode code_info__add_lvalues_to_variable(in, in, in, out) is det.

code_info__add_lvalues_to_variable(Lvals0, Var) -->
	{ set__to_sorted_list(Lvals0, Lvals) },
	code_info__add_lvalues_to_variable_2(Lvals, Var).

:- pred code_info__add_lvalues_to_variable_2(list(lval), var,
						code_info, code_info).
:- mode code_info__add_lvalues_to_variable_2(in, in, in, out) is det.

code_info__add_lvalues_to_variable_2([], _Var) --> [].
code_info__add_lvalues_to_variable_2([Lval|Lvals], Var) -->
	code_info__add_lvalue_to_variable(Lval, Var),
	code_info__add_lvalues_to_variable_2(Lvals, Var).

%---------------------------------------------------------------------------%

:- pred code_info__remap_variable(var, lval, lval, code_info, code_info).
:- mode code_info__remap_variable(in, in, in, in, out) is det.

code_info__remap_variable(Var, Lval0, Lval) -->
	code_info__get_variables(Variables0),
	(
		{ map__search(Variables0, Var, evaluated(Lvals0)) }
	->
		{ set__init(None) },
		{ map__set(Variables0, Var, evaluated(None), Variables1) },
		code_info__set_variables(Variables1),
		{ set__to_sorted_list(Lvals0, Lvals1) },
		code_info__reenter_lvalues(Var, Lval0, Lval, Lvals1)
	;
		{ true }
	).

:- pred code_info__reenter_lvalues(var, lval, lval, list(lval),
							code_info, code_info).
:- mode code_info__reenter_lvalues(in, in, in, in, in, out) is det.

:- code_info__reenter_lvalues(_,_,_,L,_,_) when L.

code_info__reenter_lvalues(_Var, _Lval0, _Lval, []) --> [].
code_info__reenter_lvalues(Var, Lval0, Lval, [L|Ls]) -->
	(
		{ L = stackvar(_) }
	->
		code_info__add_lvalue_to_variable(L, Var)
	;
		{ L = framevar(_) }
	->
		code_info__add_lvalue_to_variable(L, Var)
	;
		{ L = field(Tag, Lval1, Field) },
		{ \+ Lval1 = Lval0 }
	->
		code_info__add_lvalue_to_variable(L, Var)
	;
		{ L = field(Tag, Lval0, Field) }
	->
		code_info__add_lvalue_to_variable(field(Tag, Lval, Field), Var)
	;
		{ L = Lval0 }
	->
		code_info__add_lvalue_to_variable(Lval, Var)
	;
		{ true }
	),
	code_info__reenter_lvalues(Var, Lval0, Lval, Ls).


%---------------------------------------------------------------------------%

:- pred code_info__remap_variables(set(var), lval, lval, code_info, code_info).
:- mode code_info__remap_variables(in, in, in, in, out) is det.

code_info__remap_variables(Vars0, Lval0, Lval) -->
	{ set__to_sorted_list(Vars0, Vars) },
	code_info__remap_variables_2(Vars, Lval0, Lval).

:- pred code_info__remap_variables_2(list(var), lval, lval,
							code_info, code_info).
:- mode code_info__remap_variables_2(in, in, in, in, out) is det.

code_info__remap_variables_2([], _Lval0, _Lval) --> [].
code_info__remap_variables_2([Var|Vars], Lval0, Lval) -->
	code_info__remap_variable(Var, Lval0, Lval),
	code_info__remap_variables_2(Vars, Lval0, Lval).

%---------------------------------------------------------------------------%

code_info__save_hp(Code) -->
	code_info__push_rval(lval(hp), Code).

code_info__restore_hp(Code) -->
	code_info__pop_lval(hp, Code).

code_info__pop_stack(Code) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	{ Code = node([
		decr_sp(1) - "Decrement stack pointer"
	]) }.


%---------------------------------------------------------------------------%

code_info__stack_variable(Num0, Lval) -->
	code_info__get_proc_category(Cat),
	code_info__get_push_count(Count),
	{ Num is Num0 + Count },
	(
		{ Cat = nondeterministic }
	->
		{ Lval = framevar(Num) }
	;
		{ Lval = stackvar(Num) }
	).

%---------------------------------------------------------------------------%

:- pred code_info__push_rval(rval, code_tree, code_info, code_info).
:- mode code_info__push_rval(in, out, in, out) is det.

code_info__push_rval(Rval, Code) -->
	code_info__get_globals(Globals),
	(
		{ globals__get_gc_method(Globals, accurate) }
	->
		{ error("`push' is incompatible with accurate GC") }
	;
		[]
	),
	code_info__get_proc_category(Cat),
	(
		{ Cat = nondeterministic }
	->
		{ error("Cannot push onto nondet stack") }
	;
		code_info__get_push_count(Count0),
		{ Count is Count0 + 1 },
		code_info__set_push_count(Count),
		{ Code = node([
			incr_sp(1) - "Increment stack pointer",
			assign(stackvar(1), Rval) -
					"Store value"
		]) }
	).

%---------------------------------------------------------------------------%

:- pred code_info__pop_lval(lval, code_tree, code_info, code_info).
:- mode code_info__pop_lval(in, out, in, out) is det.

code_info__pop_lval(Lval, Code) -->
	code_info__get_proc_category(Cat),
	(
		{ Cat = nondeterministic }
	->
		{ error("Cannot pop off nondet stack") }
	;
		code_info__get_push_count(Count0),
		{ Count is Count0 - 1 },
		code_info__set_push_count(Count),
		{ Code = node([
			assign(Lval, lval(stackvar(1))) -
					"Store value",
			decr_sp(1) - "Decrement stack pointer"
		]) }
	).

%---------------------------------------------------------------------------%

code_info__generate_failure(Code) -->
	code_info__get_context_category(Category),
	(
		{ Category = nondeterministic }
	->
		{ Code = node([ redo - "" ]) }
	;
		code_info__failure_cont(Cont)
	->
		{ Code = node([ goto(Cont) -
					"Branch to failure continuation" ]) }
	;
		{ error("code_info__generate_failure: missing failure continuation") }
	).

%---------------------------------------------------------------------------%

code_info__generate_test_and_fail(Rval, Code) -->
	code_info__get_context_category(Category),
	(
		{ Category = nondeterministic }
	->
		code_info__get_next_label(Success),
		{ Code = node([
			if_val(Rval, Success) - "",
			redo - "",
			label(Success) - ""
		]) }
	;
		code_info__failure_cont(Cont)
	->
		{ Code = node([ if_not_val(Rval, Cont) - "" ]) }
	;
		{ error("code_info__generate_test_and_fail: missing failure continuation") }
	).

%---------------------------------------------------------------------------%

code_info__generate_cond_branch(Rval, YesLab, NoLab, Code) -->
	{ Code = node([
		if_val(Rval, YesLab) - "",
		goto(NoLab) - ""
	]) }.

%---------------------------------------------------------------------------%

code_info__generate_icond_branch(Rval, YesLab, NoLab, Code) -->
	{ Code = node([
		if_not_val(Rval, NoLab) - "",
		goto(YesLab) - ""
	]) }.

%---------------------------------------------------------------------------%

code_info__generate_pre_commit(PreCommit, FailLabel) -->
	code_info__get_next_label(FailLabel),
	code_info__push_rval(lval(maxfr), SaveMaxfr),
	code_info__push_rval(lval(curredoip), SaveRedoip),
	{ SetRedoIp = node([
		modframe(yes(FailLabel)) - "hijack the failure continuation"
	]) },
	{ PreCommit = tree(SaveMaxfr, tree(SaveRedoip, SetRedoIp)) }.

code_info__generate_commit(FailLabel, Commit) -->
	code_info__get_next_label(SuccLabel),
	{ GotoSuccCode = node([
		goto(SuccLabel) - "jump to success continuation",
		label(FailLabel) - "failure continuation"
	]) },
	{ SuccLabelCode = node([
		label(SuccLabel) - "success continuation"
	]) },
	code_info__pop_lval(curredoip, RestoreRedoIp),
	code_info__pop_lval(maxfr, RestoreMaxfr),
	code_info__generate_failure(Fail),
	{ RestoreRegs = tree(RestoreRedoIp, RestoreMaxfr) },
	{ FailCode = tree(RestoreRegs, Fail) },
	{ SuccessCode = RestoreRegs },
	{ Commit = tree(GotoSuccCode, tree(FailCode,
		tree(SuccLabelCode, SuccessCode))) }.

%---------------------------------------------------------------------------%

code_info__push_failure_cont(Cont) -->
	code_info__get_fall_through(Fall0),
	{ stack__push(Fall0, Cont, Fall) },
	code_info__set_fall_through(Fall).

code_info__pop_failure_cont_det(Cont) -->
	( code_info__pop_failure_cont(Cont0) ->
		{ Cont = Cont0 }
	;
		{ error("code_info__pop_failure_cont failed") }
	).

code_info__pop_failure_cont(Cont) -->
	code_info__get_fall_through(Fall0),
	{ stack__pop(Fall0, Cont, Fall) },
	code_info__set_fall_through(Fall).

code_info__set_failure_cont(Cont) -->
	code_info__get_fall_through(Fall0),
	{ stack__push(Fall0, Cont, Fall) },
	code_info__set_fall_through(Fall).

code_info__get_failure_cont(Cont) -->
	code_info__get_fall_through(Fall),
	(
		{ stack__top(Fall, Cont0) }
	->
		{ Cont = Cont0 }
	;
		{ error("No failure continuation") }
	).

code_info__failure_cont(Cont) -->
	code_info__get_fall_through(Fall),
	{ stack__top(Fall, Cont) }.

code_info__unset_failure_cont -->
	code_info__get_fall_through(Fall0),
	(
		{ stack__pop(Fall0, _Cont, Fall) }
	->
		code_info__set_fall_through(Fall)
	;
		{ error("No failure continuation") }
	).

%---------------------------------------------------------------------------%

code_info__push_store_map(Map) -->
	code_info__get_store_map(Maps0),
	{ stack__push(Maps0, Map, Maps) },
	code_info__set_store_map(Maps).

code_info__pop_store_map -->
	code_info__get_store_map(Maps0),
	{ stack__pop_det(Maps0, _, Maps) },
	code_info__set_store_map(Maps).

code_info__current_store_map(Map) -->
	code_info__get_store_map(Maps0),
	(
		{ stack__top(Maps0, Map0) }
	->
		{ Map = Map0 }
	;
		{ error("No store map on stack") }
	).

%---------------------------------------------------------------------------%

:- pred code_info__set_stackslot_count(int, code_info, code_info).
:- mode code_info__set_stackslot_count(in, in, out) is det.

:- pred code_info__get_label_count(int, code_info, code_info).
:- mode code_info__get_label_count(out, in, out) is det.

:- pred code_info__set_label_count(int, code_info, code_info).
:- mode code_info__set_label_count(in, in, out) is det.

:- pred code_info__get_registers(register_info, code_info, code_info).
:- mode code_info__get_registers(out, in, out) is det.

:- pred code_info__set_registers(register_info, code_info, code_info).
:- mode code_info__set_registers(in, in, out) is det.

:- pred code_info__get_variables(variable_info, code_info, code_info).
:- mode code_info__get_variables(out, in, out) is det.

:- pred code_info__set_variables(variable_info, code_info, code_info).
:- mode code_info__set_variables(in, in, out) is det.

		% Get the fall though point for failure
:- pred code_info__get_fall_through(stack(label), code_info, code_info).
:- mode code_info__get_fall_through(out, in, out) is det.

		% Set the fall though point for failure
:- pred code_info__set_fall_through(stack(label), code_info, code_info).
:- mode code_info__set_fall_through(in, in, out) is det.

:- pred code_info__get_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__get_liveness_info(out, in, out) is det.

:- pred code_info__set_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__set_liveness_info(in, in, out) is det.

:- pred code_info__get_proc_category(category, code_info, code_info).
:- mode code_info__get_proc_category(out, in, out) is det.

:- pred code_info__get_context_category(category, code_info, code_info).
:- mode code_info__get_context_category(out, in, out) is det.

:- pred code_info__set_proc_category(category, code_info, code_info).
:- mode code_info__set_proc_category(in, in, out) is det.

:- pred code_info__set_context_category(category, code_info, code_info).
:- mode code_info__set_context_category(in, in, out) is det.

:- pred code_info__get_push_count(int, code_info, code_info).
:- mode code_info__get_push_count(out, in, out) is det.

:- pred code_info__set_push_count(int, code_info, code_info).
:- mode code_info__set_push_count(in, in, out) is det.

:- pred code_info__get_store_map(stack(map(var, lval)), code_info, code_info).
:- mode code_info__get_store_map(out, in, out) is det.

:- pred code_info__set_store_map(stack(map(var, lval)), code_info, code_info).
:- mode code_info__set_store_map(in, in, out) is det.

code_info__get_stackslot_count(A, CI, CI) :-
	CI = code_info(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_stackslot_count(A, CI0, CI) :-
	CI0 = code_info(_, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_label_count(B, CI, CI) :-
	CI = code_info(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_label_count(B, CI0, CI) :-
	CI0 = code_info(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_varset(C, CI, CI) :-
	CI = code_info(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_varset(C, CI0, CI) :-
	CI0 = code_info(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_call_info(D, CI, CI) :-
	CI = code_info(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_call_info(D, CI0, CI) :-
	CI0 = code_info(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_pred_id(E, CI, CI) :-
	CI = code_info(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_pred_id(E, CI0, CI) :-
	CI0 = code_info(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_proc_id(F, CI, CI) :-
	CI = code_info(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_proc_id(F, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_registers(G, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_registers(G, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_variables(H, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, _, _).

code_info__set_variables(H, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, _, I, J, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_proc_info(I, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, _, _).

code_info__get_succip_used(J, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, _, _).

code_info__set_succip_used(J, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_fall_through(K, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, _, _).

code_info__set_fall_through(K, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_module_info(L, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, _, _).

code_info__set_module_info(L, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_liveness_info(M, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _, _, _).

code_info__set_liveness_info(M, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_store_map(N, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, _, _).

code_info__set_store_map(N, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, P, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_proc_category(OP, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, OP - _, _, _,
		_).

code_info__get_context_category(OC, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _ - OC, _, _,
		_).

code_info__set_context_category(OC, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, OP - _, P,
		Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, OP - OC, P,
		Q, R).

code_info__get_continuation(P, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, _, _).

code_info__set_continuation(P, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _, Q, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_push_count(Q, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Q, _).

code_info__set_push_count(Q, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, _, R),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

code_info__get_globals(R, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, R).

code_info__set_globals(R, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, _),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R).

%---------------------------------------------------------------------------%

code_info__grab_code_info(C, C, C).

code_info__slap_code_info(C0, C1, C) :-
	code_info__get_label_count(L, C1, _),
	code_info__set_label_count(L, C0, C2),
	code_info__get_succip_used(S, C1, _),
	code_info__set_succip_used(S, C2, C3),
	code_info__get_store_map(F, C1, _),
	code_info__set_store_map(F, C3, C4),
	code_info__get_fall_through(J, C1, _),
	code_info__set_fall_through(J, C4, C).

%---------------------------------------------------------------------------%

:- pred code_info__make_assignment_comment(var, lval, string,
							code_info, code_info).
:- mode code_info__make_assignment_comment(in, in, out, in, out) is det.

code_info__make_assignment_comment(Var, _Lval, Comment) -->
	code_info__get_varset(Varset),
	(
		{ varset__lookup_name(Varset, Var, Name) }
	->
		{ string__append("Assigning from ", Name, Comment) }
	;
		{ term__var_to_int(Var, Int) },
		{ string__int_to_string(Int, IntString) },
		{ string__append("Assigning from variable number ", IntString,
			Comment) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
