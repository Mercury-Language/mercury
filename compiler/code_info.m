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
:- import_module code_util, tree, set, std_util, globals, unify_proc.

:- type code_info.

:- type code_tree	==	tree(list(instruction)).

		% Create a new code_info structure.
:- pred code_info__init(varset, liveness_info, call_info, bool, globals,
			pred_id, proc_id, proc_info, category,
			instmap, follow_vars, module_info, code_info).
:- mode code_info__init(in, in, in, in, in, in, in, in, in, in, in, in, out)
			is det.

		% Generate the next local label in sequence.
:- pred code_info__get_next_label(label, code_info, code_info).
:- mode code_info__get_next_label(out, in, out) is det.

		% Generate the next local label number in sequence.
:- pred code_info__get_next_label_number(int, code_info, code_info).
:- mode code_info__get_next_label_number(out, in, out) is det.

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

		% Produce a value for a variable, flushing
		% it if necessary
:- pred code_info__produce_variable(var, code_tree, rval,
					code_info, code_info).
:- mode code_info__produce_variable(in, out, out, in, out) is det.

		% Flush a single cached expression
:- pred code_info__flush_variable(var, code_tree, code_info, code_info).
:- mode code_info__flush_variable(in, out, in, out) is det.

		% Flush the expression cache for the generation
		% of eager code.
:- pred code_info__generate_eager_flush(code_tree, code_info, code_info).
:- mode code_info__generate_eager_flush(out, in, out) is det.

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

:- pred code_info__generate_expression(var, rval, lval, code_tree,
							code_info, code_info).
:- mode code_info__generate_expression(in, in, in, out, in, out) is det.

:- pred code_info__get_variable_register(var, lval, code_info, code_info).
:- mode code_info__get_variable_register(in, out, in, out) is det.

:- pred code_info__variable_register(var, lval, code_info, code_info).
:- mode code_info__variable_register(in, out, in, out) is semidet.

:- pred code_info__variable_has_register(var, lval, code_info, code_info).
:- mode code_info__variable_has_register(in, in, in, out) is semidet.

:- pred code_info__cons_id_to_tag(var, cons_id, cons_tag,
							code_info, code_info).
:- mode code_info__cons_id_to_tag(in, in, out, in, out) is det.

:- pred code_info__get_total_stackslot_count(int, code_info, code_info).
:- mode code_info__get_total_stackslot_count(out, in, out) is det.

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

:- pred code_info__add_variable_to_register(var, reg, code_info, code_info).
:- mode code_info__add_variable_to_register(in, in, in, out) is det.

:- pred code_info__get_module_info(module_info, code_info, code_info).
:- mode code_info__get_module_info(out, in, out) is det.

:- type failure_cont
	--->	known(label)	% on failure we jump to `Label'
	;	do_fail		% on failure we do a `fail()'
	;	unknown.	% on failure we do a `redo()'

	% push a new failure continuation onto the stack

:- pred code_info__push_failure_cont(failure_cont, code_info, code_info).
:- mode code_info__push_failure_cont(in, in, out) is det.

	% pop the failure continuation stack

:- pred code_info__pop_failure_cont(code_info, code_info).
:- mode code_info__pop_failure_cont(in, out) is det.

	% lookup the value on the top of the failure continuation stack

:- pred code_info__failure_cont(failure_cont, code_info, code_info).
:- mode code_info__failure_cont(out, in, out) is det.

	% generate some code to restore the current redoip, by looking
	% at the top of the failure continuation stack.

:- pred code_info__restore_failure_cont(code_tree, code_info, code_info).
:- mode code_info__restore_failure_cont(out, in, out) is det.

:- pred code_info__stack_variable(int, lval, code_info, code_info).
:- mode code_info__stack_variable(in, out, in, out) is det.

:- pred code_info__generate_nondet_saves(code_tree, code_info, code_info).
:- mode code_info__generate_nondet_saves(out, in, out) is det.

:- pred code_info__generate_failure(code_tree, code_info, code_info).
:- mode code_info__generate_failure(out, in, out) is det.

:- pred code_info__generate_test_and_fail(rval, code_tree,
							code_info, code_info).
:- mode code_info__generate_test_and_fail(in, out, in, out) is det.

:- pred code_info__generate_pre_commit(code_tree, label, code_info, code_info).
:- mode code_info__generate_pre_commit(out, out, in, out) is det.

:- pred code_info__generate_commit(label, code_tree, code_info, code_info).
:- mode code_info__generate_commit(in, out, in, out) is det.

:- pred code_info__save_hp(code_tree, code_info, code_info).
:- mode code_info__save_hp(out, in, out) is det.

:- pred code_info__restore_hp(code_tree, code_info, code_info).
:- mode code_info__restore_hp(out, in, out) is det.

:- pred code_info__get_old_hp(code_tree, code_info, code_info).
:- mode code_info__get_old_hp(out, in, out) is det.

:- pred code_info__pop_stack(code_tree, code_info, code_info).
:- mode code_info__pop_stack(out, in, out) is det.

:- pred code_info__maybe_save_hp(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_save_hp(in, out, in, out) is det.

:- pred code_info__maybe_restore_hp(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_restore_hp(in, out, in, out) is det.

:- pred code_info__maybe_get_old_hp(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_get_old_hp(in, out, in, out) is det.

:- pred code_info__maybe_pop_stack(bool, code_tree, code_info, code_info).
:- mode code_info__maybe_pop_stack(in, out, in, out) is det.

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

:- pred code_info__variable_type(var, type, code_info, code_info).
:- mode code_info__variable_type(in, out, in, out) is det.

:- pred code_info__get_requests(unify_requests, code_info, code_info).
:- mode code_info__get_requests(out, in, out) is det.

:- pred code_info__generate_stack_livevals(bintree_set(lval),
						code_info, code_info).
:- mode code_info__generate_stack_livevals(out, in, out) is det.

:- pred code_info__generate_stack_livelvals(list(liveinfo),
						code_info, code_info).
:- mode code_info__generate_stack_livelvals(out, in, out) is det.

:- pred code_info__variable_to_string(var, string, code_info, code_info).
:- mode code_info__variable_to_string(in, out, in, out) is det.

:- pred code_info__apply_instmap_delta(instmap_delta, code_info, code_info).
:- mode code_info__apply_instmap_delta(in, in, out) is det.

:- pred code_info__get_instmap(instmap, code_info, code_info).
:- mode code_info__get_instmap(out, in, out) is det.

:- pred code_info__set_instmap(instmap, code_info, code_info).
:- mode code_info__set_instmap(in, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module string, require, char, list, map, bimap, tree, int.
:- import_module bintree_set, varset, term, stack, prog_io.
:- import_module type_util, mode_util, options, shapes.

:- pred code_info__get_label_count(int, code_info, code_info).
:- mode code_info__get_label_count(out, in, out) is det.

:- pred code_info__set_label_count(int, code_info, code_info).
:- mode code_info__set_label_count(in, in, out) is det.

:- type code_info	--->
		code_info(
			int,		% The number of stack slots allocated.
					% for storing variables.
					% (Some extra stack slots are used
					% for saving and restoring registers.)
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
			fall_through,	% The failure continuation stack
			module_info,	% The module_info structure - you just
					% never know when you might need it.
			liveness_info,	% Variables that are live
					% after this goal
			stack(map(var, lval)),
					% Store Map - where to put things
			category,	% The category of the current procedure
			instmap,	% insts of variables
			pair(int),	% The current and maximum (respectively)
					% number of extra temporary stackslots
					% that have been pushed during the
					% procedure
			globals,	% code generation options
			stack(lval)	% the locations in use on the stack
	).

:- type register_info	==	map(reg, register_stat).

:- type register_stat	--->	reserved
			;	vars(set(var)).
			% unused registers simply don't occur in the
			% register_info map.

:- type target_register	--->	none
			;	target(lval).

:- type variable_info	==	map(var, variable_stat).

:- type variable_stat	--->	evaluated(set(lval))
			;	cached(rval, target_register).
			% unused variables simply don't occur in the
			% variable_info map.

:- type fall_through	==	stack(failure_cont).

%---------------------------------------------------------------------------%

code_info__init(Varset, Liveness, CallInfo, SaveSuccip, Globals,
					PredId, ProcId, ProcInfo, Category,
					Requests, _FollowVars, ModuleInfo, C) :-
	code_info__init_register_info(PredId, ProcId,
					ModuleInfo, RegisterInfo),
	code_info__init_variable_info(PredId, ProcId,
					ModuleInfo, VariableInfo),
	stack__init(Continue),
	stack__init(StoreMapStack0),
	stack__init(PushedVals0),
	map__init(StoreMap),
	stack__push(StoreMapStack0, StoreMap, StoreMapStack),
	code_info__max_slot(CallInfo, SlotCount0),
	(
		Category = nondeterministic
	->
		SlotCount is SlotCount0 + 1
	;
		SlotCount = SlotCount0
	),
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
		Category,
		Requests,
		0 - 0,
		Globals,
		PushedVals0
	).

%---------------------------------------------------------------------------%

:- pred code_info__max_slot(call_info, int).
:- mode code_info__max_slot(in, out) is det.

code_info__max_slot(CallInfo, SlotCount) :-
	map__values(CallInfo, CallList),
	code_info__max_slot_2(CallList, 0, SlotCount).

:- pred code_info__max_slot_2(list(lval), int, int).
:- mode code_info__max_slot_2(in, in, out) is det.

code_info__max_slot_2([], Max, Max).
code_info__max_slot_2([L|Ls], Max0, Max) :-
	(
		L = stackvar(N)
	->
		int__max(N, Max0, Max1)
	;
		L = framevar(N)
	->
		int__max(N, Max0, Max1)
	;
		Max1 = Max0
	),
	code_info__max_slot_2(Ls, Max1, Max).

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

code_info__generate_eager_flush(Code) -->
		% There's probably a more efficient way to do
		% this if we know we are flushing between every goal
	code_info__get_variables(Variables0),
	{ map__keys(Variables0, VarList) },
	code_info__generate_eager_flush_2(VarList, Code).

:- pred code_info__generate_eager_flush_2(list(var), code_tree,
						code_info, code_info).
:- mode code_info__generate_eager_flush_2(in, out, in, out) is det.

code_info__generate_eager_flush_2([], empty) --> [].
code_info__generate_eager_flush_2([Var|Vars], Code) -->
	code_info__generate_eager_flush_2(Vars, Code0),
	code_info__get_variables(Variables),
	(
		{ map__search(Variables, Var, _) }
	->
		code_info__flush_variable(Var, Code1)
	;
		{ Code1 = empty }
	),
	{ Code = tree(Code0, Code1) }.


%---------------------------------------------------------------------------%

code_info__produce_variable(Var, Code, Rval) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = cached(Expr, _Target) },
		(
			code_info__expr_is_constant(Expr)
		->
			{ Code0 = empty },
			{ Rval0 = Expr }
		;
			{ Expr = var(ExprVar) }
		->
			code_info__produce_variable(ExprVar, Code0, Rval0)
		;
			{ fail }
		)
	->
		{ Code = Code0 },
		{ Rval = Rval0 }
	;
		code_info__flush_variable(Var, Code),
		code_info__get_variable_register(Var, Lval),
		{ Rval = lval(Lval) }
	).

%-----------------------------------------------------------------------------%

		% Flush a single cached expression

code_info__flush_variable(Var, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = cached(Exprn, Target) }
	->
		code_info__target_to_lvalue(Target, Exprn, Var, TargetReg),
			% Generate code to evaluate the expression.
		code_info__generate_expression(Var, Exprn, TargetReg, Code),
			% Mark the register as taken, so that
			% it doesn't get allocated
		(
			{ TargetReg = reg(Reg) },
			{ Code \= empty }
		->
			{ set__singleton_set(Vars, Var) },
			code_info__set_register_contents(Reg, vars(Vars))
		;
			{ true }
		),
		code_info__add_lvalue_to_variable(TargetReg, Var)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

	% Succeed if the expression is a constant, i.e. something
	% that can be represented just by an rval which doesn't require
	% any code to be generated.  Note that variables whose value is
	% cached can be constants.  Note also that create() expressions whose
	% arguments are constants are themselves constants - unless the
	% --static-ground-terms option was disabled.
	% Note also that addresses of imported predicates are not constant
	% if we are using GNU C non-local gotos.

:- pred code_info__expr_is_constant(rval, code_info, code_info).
:- mode code_info__expr_is_constant(in, in, out) is semidet.

code_info__expr_is_constant(const(Const)) -->
	( { Const = pred_const(CodeAddress) } ->
		( { CodeAddress = succip } ->
			{ fail }
		; { CodeAddress = label(_) } ->
			{ true }
		;
			code_info__get_globals(Globals),
			{ globals__lookup_bool_option(Globals,
				gcc_non_local_gotos, no) }
		)
	;
		{ true }
	).

code_info__expr_is_constant(unop(_Op, Expr)) -->
	code_info__expr_is_constant(Expr).

code_info__expr_is_constant(binop(_Op, Expr1, Expr2)) -->
	code_info__expr_is_constant(Expr1),
	code_info__expr_is_constant(Expr2).

code_info__expr_is_constant(mkword(_Tag, Expr)) -->
	code_info__expr_is_constant(Expr).

code_info__expr_is_constant(create(_Tag, Args, _Label)) -->
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, static_ground_terms, yes) },
	code_info__args_are_constant(Args).

code_info__expr_is_constant(var(Var)) -->
	code_info__get_variables(Variables),
	{ map__search(Variables, Var, cached(Expr, _)) },
	code_info__expr_is_constant(Expr).

:- pred code_info__args_are_constant(list(maybe(rval)), 
					code_info, code_info).
:- mode code_info__args_are_constant(in, in, out) is semidet.

code_info__args_are_constant([]) --> [].
code_info__args_are_constant([yes(Arg) | Args]) -->
	code_info__expr_is_constant(Arg),
	code_info__args_are_constant(Args).

%---------------------------------------------------------------------------%

% Before generating code for an expression, we make sure that if
% the target location is a used stackslot, we swap the value stored
% there out of the way first.

code_info__generate_expression(Var, Exprn, TargetReg, Code) -->
	(
		{ % some [N]
			(
				TargetReg = stackvar(N)
			;
				TargetReg = framevar(N)
			)
		},
		code_info__get_variables(Variables),
		{ map__to_assoc_list(Variables, VarList) },
		{ code_info__slot_occupied(VarList, Var, TargetReg) }
	->
		code_info__get_free_register(Reg),
		code_info__relocate_slot(VarList, Reg, TargetReg, NewVarList),
		{ map__from_assoc_list(NewVarList, Variables1) },
		code_info__set_variables(Variables1),
		{ SaveCode = node([assign(reg(Reg), lval(TargetReg))
				- "Move value in slot out of the way"]) }
	;
		{ SaveCode = empty }
	),
	code_info__generate_expression_2(Exprn, TargetReg, ECode),
	{ Code = tree(SaveCode, ECode) }.

:- pred code_info__generate_expression_2(rval, lval, code_tree,
							code_info, code_info).
:- mode code_info__generate_expression_2(in, in, out, in, out) is det.

code_info__generate_expression_2(lval(Lval), TargetReg, Code) -->
	(
		{ Lval = TargetReg }
	->
		{ Code = empty }
	;
		{ Lval = field(Tag, StructRval, FieldNum0) }
	->
		code_info__generate_expression_vars(FieldNum0, FieldNum,
			CodeA),
		code_info__generate_expression_vars(StructRval, Rval, CodeB),
		{ CodeC = node([
			assign(TargetReg, lval(field(Tag, Rval, FieldNum)))
				- "Copy field"
		]) },
		{ Code = tree(CodeA, tree(CodeB, CodeC)) }
	;
		{ Code = node([
			assign(TargetReg, lval(Lval)) - "Copy lvalue"
		]) }
	).
code_info__generate_expression_2(var(Var), TargetReg, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = evaluated(Lvals0) }
	->
		{ code_info__select_lvalue(Lvals0, TargetReg, Lval0) },
		(
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
			code_info__generate_expression(Var, Exprn,
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
		code_info__generate_expression(Var, Exprn, TargetReg, Code0),
			% Mark the target register.
		code_info__add_lvalue_to_variable(TargetReg, Var),
			% Assemble the code
		{ Code = Code0 }
	;
		{ error("invalid variable status") }
	).
code_info__generate_expression_2(create(Tag, Args, Label), TargetReg, Code) -->
	{ list__length(Args, Arity) }, % includes possible tag word
	(
		{ Arity = 0 }
	->
		{ Code = node([
			assign(TargetReg,
				mkword(	Tag, const(int_const(0)))) -
					"Assign a constant"
		]) }
	;
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, static_ground_terms,
			yes) },
		code_info__args_are_constant(Args)
	->
		code_info__generate_expression_cons_vars(Args, Args1, CodeA),
		{ CodeB = node([
			assign(TargetReg, create(Tag, Args1, Label))
				- "Assign a constant ground term"
		]) },
		{ Code = tree(CodeA, CodeB) }
	;
		{ CodeA = node([
			incr_hp(TargetReg, yes(Tag), const(int_const(Arity))) -
				"Allocate heap"
		]) },
		code_info__generate_cons_args(TargetReg, Tag, 0, Args, CodeB),
		{ Code = tree(CodeA, CodeB) }
	).
code_info__generate_expression_2(mkword(Tag, Rval0), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = node([
		assign(TargetReg, mkword(Tag, Rval)) - "Tag a word"
	]) },
	{ Code = tree(Code0, Code1) }.
code_info__generate_expression_2(unop(Unop, Rval0), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = node([
		assign(TargetReg, unop(Unop, Rval)) - "Apply unary operator"
	]) },
	{ Code = tree(Code0, Code1) }.
code_info__generate_expression_2(const(Const), TargetReg, Code) -->
	{ Code = node([
		assign(TargetReg, const(Const)) - "Make a constant"
	]) }.
code_info__generate_expression_2(binop(Op, L0, R0), TargetReg, Code) -->
	code_info__generate_expression_vars(L0, L, Code0),
	code_info__generate_expression_vars(R0, R, Code1),
	{ ThisCode = node([
		assign(TargetReg, binop(Op, L, R)) -
				"Evaluate binary expression"
	]) },
	{ Code2 = tree(Code1, ThisCode) },
	{ Code = tree(Code0, Code2) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_cons_args(lval, int, int, list(maybe(rval)),
				code_tree, code_info, code_info).
:- mode code_info__generate_cons_args(in, in, in, in, out, in, out) is det.

:- code_info__generate_cons_args(_, _, _, X, _, _, _) when X. % indexing

	% `Reg' is the register which contains the pointer to the
	% term whose arguments we are generating.
	% `Tag' is the tag on that pointer. 
	% `Field0' is the field number of this field.

code_info__generate_cons_args(_Reg, _Tag, _Field0, [], empty) --> [].
code_info__generate_cons_args(Reg, Tag, Field0, [Arg|Args], Code) -->
	(
		{ Arg = yes(Rval) }
	->
		% since the target is a field, we won't need to swap
		% a stackslot out of the way, so we call ..._2
		% directly.
		code_info__generate_expression_2(Rval,
			field(Tag, lval(Reg), const(int_const(Field0))), Code0)
	;
		{ Code0 = empty }
	),
	{ Field1 is Field0 + 1 },
	code_info__generate_cons_args(Reg, Tag, Field1, Args, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_expression_vars(rval, rval, code_tree,
							code_info, code_info).
:- mode code_info__generate_expression_vars(in, out, out, in, out) is det.

code_info__generate_expression_vars(lval(Lval), lval(Lval), empty) --> [].
code_info__generate_expression_vars(var(Var), Rval, Code) -->
	code_info__produce_variable(Var, CodeA, Rval0),
	code_info__generate_expression_vars(Rval0, Rval, CodeB),
	{ Code = tree(CodeA, CodeB) }.
code_info__generate_expression_vars(create(Tag, MaybeRvals0, Label),
				create(Tag, MaybeRvals, Label), Code) -->
	code_info__generate_expression_cons_vars(MaybeRvals0, MaybeRvals, Code).
code_info__generate_expression_vars(mkword(Tag, Rval0), mkword(Tag, Rval),
								Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(unop(Unop, Rval0), unop(Unop, Rval), Code)
						-->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(const(Const), const(Const), empty) --> [].
code_info__generate_expression_vars(binop(Op, L0, R0),
						binop(Op, L, R), Code) -->
	code_info__generate_expression_vars(L0, L, Code0),
	code_info__generate_expression_vars(R0, R, Code1),
	{ Code = tree(Code0, Code1) }.

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

:- pred code_info__generate_expression_cons_vars(list(maybe(rval)),
			list(maybe(rval)), code_tree, code_info, code_info).
:- mode code_info__generate_expression_cons_vars(in, out, out, in, out) is det.

code_info__generate_expression_cons_vars([], [], empty) --> [].
code_info__generate_expression_cons_vars([R0|Rs0], [R|Rs], Code) -->
	(
		{ R0 = yes(Rval0) }
	->
		code_info__generate_expression_vars(Rval0, Rval, Code0),
		{ R = yes(Rval) }
	;
		{ R = R0 },
		{ Code0 = empty }
	),
	code_info__generate_expression_cons_vars(Rs0, Rs, Code1),
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
		{ \+ map__contains(Registers, Reg) }
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
			code_info__variable_register(Var, Lval1),
			{ Lval1 = reg(Reg) }
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
	code_info__generate_expression(Var, var(Var), reg(Reg), CodeB),
	code_info__add_lvalue_to_variable(reg(Reg), Var),
	code_info__add_variable_to_register(Var, Reg),
	{ Code = tree(CodeA, CodeB) }.

:- pred code_info__shuffle_registers_2(reg, list(var), register_stat,
					code_tree, code_info, code_info).
:- mode code_info__shuffle_registers_2(in, in, in, out, in, out) is det.

code_info__shuffle_registers_2(Reg, Args, Contents, Code) -->
	(
		{ Contents = reserved }
	->
		{ error("Cannot shuffle a reserved register.") }
	;
		{ Contents = vars(Vars) },
		code_info__must_be_swapped(Vars, Args, reg(Reg))
	->
			% We could make a smarter choice here,
			% but value numbering should make the
			% code good enough.
		code_info__get_next_free_register(Reg, NewReg),
			% Update the register info -
			% remove the entry for the old register,
			% and set the entry for the new register.
		code_info__set_register_contents(NewReg, Contents),
		code_info__unset_register_contents(Reg),
			% Update the variable info -
			% Set the location of the variable to the
			% new register.
		code_info__remap_variables(Vars, reg(Reg), reg(NewReg)),
			% Generate the code fragment.
		{ Code = node([
			assign(reg(NewReg), lval(reg(Reg))) -
				"Swap live variable to a new register"
		]) }
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

code_info__save_variable_on_stack(Var, Code) -->
	code_info__get_variable(Var, VarStat),
	(
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
		code_info__generate_expression(Var, Exprn, TargetReg, Code0),
		code_info__add_lvalue_to_variable(TargetReg, Var),
		code_info__save_variable_in_slot(Var, Code1),
		{ Code = tree(Code0, Code1) }
	;
		{ error("This should never happen") }
	).

%---------------------------------------------------------------------------%

% code_info__slot_occupied succeeds if the given lval (a stackvar/framevar)
% is used to store a variable other than the given one.

:- pred code_info__slot_occupied(assoc_list(var, variable_stat), var, lval).
:- mode code_info__slot_occupied(in, in, in) is semidet.

code_info__slot_occupied([], _Var, _Slot) :- fail.
code_info__slot_occupied([V - Stat|Rest], Var, Slot) :-
	(
		% some [Lval] (
			Stat = evaluated(Set),
			V \= Var,
			set__member(Lval, Set),
			code_info__lval_contains(Lval, Slot)
		% )
	->
		true
	;
		code_info__slot_occupied(Rest, Var, Slot)
	).

%---------------------------------------------------------------------------%

:- pred code_info__lval_contains(lval, lval).
:- mode code_info__lval_contains(in ,in) is semidet.

code_info__lval_contains(reg(R), reg(R)).
code_info__lval_contains(stackvar(N), stackvar(N)).
code_info__lval_contains(framevar(N), framevar(N)).
code_info__lval_contains(field(_, Rval0, Rval1), Lval) :-
	(
		code_info__rval_contains(Rval0, Lval)
	;
		code_info__rval_contains(Rval1, Lval)
	).

:- pred code_info__rval_contains(rval, lval).
:- mode code_info__rval_contains(in ,in) is semidet.

code_info__rval_contains(lval(Lval0), Lval1) :-
	code_info__lval_contains(Lval0, Lval1).
code_info__rval_contains(mkword(_, Rval), Lval) :-
	code_info__rval_contains(Rval, Lval).
code_info__rval_contains(unop(_, Rval), Lval) :-
	code_info__rval_contains(Rval, Lval).
code_info__rval_contains(binop(_, Rval0, Rval1), Lval) :-
	(
		code_info__rval_contains(Rval0, Lval)
	;
		code_info__rval_contains(Rval1, Lval)
	).

%---------------------------------------------------------------------------%

:- pred code_info__get_matching_lval(list(lval), lval, lval).
:- mode code_info__get_matching_lval(in, in, out) is semidet.

code_info__get_matching_lval([L|Ls], S, R) :-
	(
		code_info__lval_contains(L, S)
	->
		R = L,
		(
			code_info__get_matching_lval(Ls, S, _)
		->
			error("code_info__get_matching_lval: sanity check failed")
		;
			true
		)
	;
		code_info__get_matching_lval(Ls, S, R)
	).

%---------------------------------------------------------------------------%

:- pred code_info__relocate_slot(assoc_list(var, variable_stat), reg, lval,
					assoc_list(var, variable_stat),
						code_info, code_info).
:- mode code_info__relocate_slot(in, in, in, out, in, out) is det.

% XXX this code does not handle the case that there are multiple matching
% lvals.

code_info__relocate_slot([], _Reg, _Slot, []) --> [].
code_info__relocate_slot([V - S0|Rest0], Reg, Slot, [V - S|Rest]) -->
	(
		{ S0 = evaluated(Set0) },
		{ set__to_sorted_list(Set0, List0) },
		{ code_info__get_matching_lval(List0, Slot, Lval0) }
	->
		{ set__singleton_set(SSet, Lval0) },
		{ set__difference(Set0, SSet, Set1) },
		{ code_info__replace_lval(Lval0, Slot, reg(Reg), Lval) },
		{ set__insert(Set1, Lval, Set2) },
		{ S = evaluated(Set2) },
		code_info__add_variable_to_register(V, Reg)
	;
		{ S = S0 }
	),
	code_info__relocate_slot(Rest0, Reg, Slot, Rest).

%---------------------------------------------------------------------------%

:- pred code_info__replace_lval(lval, lval, lval, lval).
:- mode code_info__replace_lval(in, in, in, out) is det.

code_info__replace_lval(Lval0, Match, Repl, Result) :-
	(
		Lval0 = Match
	->
		Result = Repl
	;
		Lval0 = field(T, Rval0, N)
	->
		code_info__replace_lval_in_rval(Rval0, Match, Repl, Rval),
		Result = field(T, Rval, N)
	;
		error("code_info__replace_lval: sanity check failed")
	).

:- pred code_info__replace_lval_in_rval(rval, lval, lval, rval).
:- mode code_info__replace_lval_in_rval(in, in, in, out) is det.

code_info__replace_lval_in_rval(Rval0, Match, Repl, Result) :-
	(
		Rval0 = lval(Lval0)
	->
		code_info__replace_lval(Lval0, Match, Repl, Lval),
		Result = lval(Lval)
	;
		error("code_info__replace_lval_in_rval: sanity check failed")
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
	code_info__get_next_label_number(N),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_label(ModuleInfo, PredId, ProcId, N, Label) }.

code_info__get_next_label_number(N) -->
	code_info__get_label_count(N0),
	{ N is N0 + 1 },
	code_info__set_label_count(N).

%---------------------------------------------------------------------------%

code_info__get_variable_register(Var, Lval) -->
	(
		code_info__variable_register(Var, Lval0)
	->
		{ Lval = Lval0 }
	;
		{ error("Cannot find lvalue of variable") }
	).

:- pred first_register(list(lval), lval).
:- mode first_register(in, out) is semidet.

first_register([L|Ls], R) :-
	(
		L = reg(_)
	->
		R = L
	;
		first_register(Ls, R)
	).

%---------------------------------------------------------------------------%

	% This first off tries to find a register containing `Var'.
	% If there's no register, it will look for any other memory
	% location containing the variable.

code_info__variable_register(Var, Lval) -->
	code_info__get_variables(Variables),
	{ map__search(Variables, Var, VarStat) },
	{ VarStat = evaluated(Lvals0) },
	{ set__to_sorted_list(Lvals0, LvalList) },
	(
		{ first_register(LvalList, Lval0) }
	->
		{ Lval = Lval0 }
	;
		{ LvalList = [Lval0|_] },
		{ Lval = Lval0 }
	).

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
	code_info__generate_expression(Var, var(Var), Slot, Code),
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
code_info__cons_id_to_tag(_Var, pred_const(P,M), pred_constant(P,M)) --> [].
code_info__cons_id_to_tag(Var, cons(Name, Arity), Tag) -->
		%
		% Lookup the type of the variable
		%
	code_info__variable_type(Var, Type),
	(
		% handle the `character' type specially
		{ Type = term__functor(term__atom("character"), [], _) },
	 	{ string__char_to_string(Char, Name) }
	->
		{ char_to_int(Char, CharCode) },
		{ Tag = int_constant(CharCode) }
	;
		% handle higher-order pred types specially
		{ Type = term__functor(term__atom("pred"), PredArgTypes, _) }
	->
		{ list__length(PredArgTypes, PredArity) },
		code_info__get_module_info(ModuleInfo),
		{ module_info_get_predicate_table(ModuleInfo, PredicateTable) },
		{ TotalArity is Arity + PredArity },
		{
		    predicate_table_search_name_arity(PredicateTable,
			Name, TotalArity, PredIds)
		->
		    (
			PredIds = [PredId]
		    ->
			predicate_table_get_preds(PredicateTable, Preds),
			map__lookup(Preds, PredId, PredInfo),
			pred_info_procedures(PredInfo, Procs),
			map__keys(Procs, ProcIds),
			(
			    ProcIds = [ProcId]
			->
			    Tag = pred_constant(PredId, ProcId)
			;
			    error("sorry, not implemented: taking address of predicate with multiple modes")
			)
		    ;
			% cons_id ought to include the module prefix, so
			% that we could use predicate_table__search_m_n_a to 
			% prevent this from happening
			error("code_info__cons_id_to_tag: ambiguous pred")
		    )
		;
		    % the type-checker should ensure that this never happens
		    error("code_info__cons_id_to_tag: invalid pred")
		}
	;
			%
			% Use the type to determine the type_id
			%
		{ type_to_type_id(Type, TypeId0, _) ->
			TypeId = TypeId0
		;
			% the type-checker should ensure that this never happens
			error("code_info__cons_id_to_tag: invalid type")
		},

			%
			% Given the type_id, lookup up the constructor tag
			% table for that type
			%
		code_info__get_module_info(ModuleInfo),
		{ module_info_types(ModuleInfo, TypeTable) },
		{ map__lookup(TypeTable, TypeId, TypeDefn) },
		{
			TypeDefn = hlds__type_defn(_, _,
				du_type(_, ConsTable0, _), _, _)
		->
			ConsTable = ConsTable0
		;
			% this should never happen
			error(
			"code_info__cons_id_to_tag: type is not d.u. type?"
			)
		},
			% Finally look up the cons_id in the table
		{ map__lookup(ConsTable, cons(Name, Arity), Tag) }
	).

%---------------------------------------------------------------------------%

code_info__variable_is_live(Var) -->
	code_info__get_liveness_info(Liveness),
	{ set__member(Var, Liveness) }.

:- pred code_info__must_be_swapped(set(var), list(var), lval, code_info, code_info).
:- mode code_info__must_be_swapped(in, in, in, in, out) is semidet.

code_info__must_be_swapped(Vars, Args, Lval) -->
	code_info__get_liveness_info(Liveness),
	code_info__get_variables(Variables),
	{ 
		code_info__var_must_be_swapped(Vars, Args, Liveness,
			Variables, Lval)
		;
		% We also need to swap out variables if they might
		% be an argument that we haven't positioned yet.
			set__member(Var, Vars),
			list__member(Var, Args)
	}.

:- pred code_info__var_must_be_swapped(set(var), list(var), liveness_info,
					variable_info, lval).
:- mode code_info__var_must_be_swapped(in, in, in, in, in) is semidet.

	% The contents of a location must be swapped if any
	% of the variables in it must be swapped.
	% (NB this is actually overly conservative - we could
	% avoid swapping unless *all* variables needed to be
	% swapped, since we know that the variables must all be
	% aliased since they share the same storage location.)
	%
	% A variable must be swapped iff it is live and
	% it is not stored anywhere else.  A variable is live
	% if it is in the livevars or it is one of the remaining
	% arguments for the current call.

code_info__var_must_be_swapped(Vars, Args, Liveness, Variables, ThisLval) :-
	set__member(V, Vars),
	( set__member(V, Liveness) ; list__member(V, Args) ),
	map__search(Variables, V, evaluated(Lvals)),
	\+ (set__member(Lval, Lvals), Lval \= ThisLval).

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
		code_info__variable_to_string(Var, VarString),
		{ string__append_list(["`", VarString,
			"' not in register or on stack."], Message) },
		{ error(Message) }
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
			code_info__generate_expression(Var, var(Var), Lval, CodeB),
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
				code_info__generate_expression(Var, var(Var),
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
	code_info__generate_nondet_saves_2(CallList, Code).

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
		code_info__generate_expression(Var, var(Var), StackThing, Code0),
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

% code_info__remake_with_store_map rebuilds the register info and
% variable info data structures. It operates under the assumption
% that if a variable is live, it is stored somewhere. What is more,
% it assumes that if a variable is live, then there is a copy of it
% stored on the stack in the location given by the store map. This
% means that it only makes sense to call this predicate in situations
% such as the start of disjuncts where it only makes sense to have
% variables on the stack. (Note that disj_gen contains a piece of
% magic to use the register versions of variables if possible in the
% first disjunct).

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

% code_info__remake_with_call_info rebuilds the register info and the
% variable info structures. It operates under the assumption that if
% a variable is live, then it should be located in the storage indicated
% by the call info information. This is for use at the end of the branches
% in branched structures, where we need to ensure that variables are
% stored in consistient places.

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

% code_info__reduce_variables_and_registers is called at the end of
% each goal to clear out anything in the register info or the variable
% info structures which is not needed any longer. A variable is not
% needed if it is not live, and there are no cached expressions which
% depend upon it.

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
		code_info__variable_dependencies(VarStat,
						Variables1, Variables)
	;
		{ error("Live variable not found!") }
	),
	code_info__reduce_variables_and_registers_2(Vars, Variables).

%---------------------------------------------------------------------------%

:- pred code_info__variable_dependencies(variable_stat,
						variable_info, variable_info, 
						code_info, code_info).
:- mode code_info__variable_dependencies(in, in, out, in, out) is det.

code_info__variable_dependencies(evaluated(_Lvals), V, V) --> [].
code_info__variable_dependencies(cached(Exprn, _), V0, V) -->
	code_info__expression_dependencies(Exprn, V0, V).

%---------------------------------------------------------------------------%

:- pred code_info__expressions_dependencies(list(rval),
						variable_info, variable_info,
						code_info, code_info).
:- mode code_info__expressions_dependencies(in, in, out, in, out) is det.

code_info__expressions_dependencies([], V, V) --> [].
code_info__expressions_dependencies([R|Rs], V0, V) -->
	code_info__expression_dependencies(R, V0, V1),
	code_info__expressions_dependencies(Rs, V1, V).

:- pred code_info__expression_dependencies(rval,
				variable_info, variable_info,
						code_info, code_info).
:- mode code_info__expression_dependencies(in, in, out, in, out) is det.

code_info__expression_dependencies(lval(_Lval), V, V) --> [].
code_info__expression_dependencies(var(Var0), V0, V) -->
	(
		code_info__get_variables(VariablesA),
		{ map__search(VariablesA, Var0, VarStat) }
	->
		{ code_info__update_variables(Var0, VarStat, V0, V1) },
		code_info__variable_dependencies(VarStat, V1, V)
	;
		{ error("Live variable not found!") }
	).
code_info__expression_dependencies(create(_Tag, MaybeRvals, _Label), V0, V) -->
	code_info__expression_cons_dependencies(MaybeRvals, V0, V).
code_info__expression_dependencies(mkword(_Tag, Rval), V0, V) -->
	code_info__expression_dependencies(Rval, V0, V).
code_info__expression_dependencies(unop(_Op, Rval), V0, V) -->
	code_info__expression_dependencies(Rval, V0, V).
code_info__expression_dependencies(const(_), V, V) --> [].
code_info__expression_dependencies(binop(_, E0, E1), V0, V) -->
	code_info__expression_dependencies(E0, V0, V1),
	code_info__expression_dependencies(E1, V1, V).

%---------------------------------------------------------------------------%

:- pred code_info__expression_cons_dependencies(list(maybe(rval)),
						variable_info, variable_info,
						code_info, code_info).
:- mode code_info__expression_cons_dependencies(in, in, out, in, out) is det.

code_info__expression_cons_dependencies([], V, V) --> [].
code_info__expression_cons_dependencies([R|Rs], V0, V) -->
	(
		{ R = yes(Rval) }
	->
		code_info__expression_dependencies(Rval, V0, V1)
	;
		{ V1 = V0 }
	),
	code_info__expression_cons_dependencies(Rs, V1, V).

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
		% XXX should this be transitive?
		{ L = field(_, lval(reg(R1)), _) }
	->
		code_info__add_variable_to_register(Var, R1)
	;
		{ true }
	),
	code_info__reenter_registers(Var, Ls).

%---------------------------------------------------------------------------%

:- pred code_info__set_register_contents(reg, register_stat,
						code_info, code_info).
:- mode code_info__set_register_contents(in, in, in, out) is det.

code_info__set_register_contents(Reg, Contents) -->
	code_info__get_registers(Registers0),
	{ map__set(Registers0, Reg, Contents, Registers) },
	code_info__set_registers(Registers).

:- pred code_info__unset_register_contents(reg, code_info, code_info).
:- mode code_info__unset_register_contents(in, in, out) is det.

code_info__unset_register_contents(Reg) -->
	code_info__get_registers(Registers0),
	{ map__delete(Registers0, Reg, Registers) },
	code_info__set_registers(Registers).

%---------------------------------------------------------------------------%

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
		{ L = field(Tag, lval(Lval1), _) },
		{ Lval1 \= Lval0 }
	->
		code_info__add_lvalue_to_variable(L, Var)
	;
		{ L = field(Tag, lval(Lval0), Field) }
	->
		code_info__add_lvalue_to_variable(
			field(Tag, lval(Lval), Field), Var)
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
%---------------------------------------------------------------------------%

code_info__maybe_save_hp(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__save_hp(Code)
	;
		{ Code = empty }
	).

code_info__maybe_restore_hp(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__restore_hp(Code)
	;
		{ Code = empty }
	).

code_info__maybe_get_old_hp(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__get_old_hp(Code)
	;
		{ Code = empty }
	).

code_info__maybe_pop_stack(Maybe, Code) -->
	( { Maybe = yes } ->
		code_info__pop_stack(Code)
	;
		{ Code = empty }
	).

%---------------------------------------------------------------------------%

code_info__save_hp(Code) -->
	code_info__push_temp(HpSlot),
	{ Code = node([ mark_hp(HpSlot) - "save heap pointer" ]) }.

code_info__restore_hp(Code) -->
	code_info__pop_temp(Lval),
	{ Code = node([ restore_hp(lval(Lval)) - "restore heap pointer" ]) }.

code_info__get_old_hp(Code) -->
	code_info__get_stack_top(Lval),
	{ Code = node([ restore_hp(lval(Lval)) - "reset heap pointer" ]) }.

%---------------------------------------------------------------------------%

code_info__stack_variable(Num, Lval) -->
	code_info__get_proc_category(Cat),
	(
		{ Cat = nondeterministic }
	->
		{ Num1 is Num - 1 },		% framevars start at zero
		{ Lval = framevar(Num1) }
	;
		{ Lval = stackvar(Num) }	% stackvars start at one
	).

%---------------------------------------------------------------------------%

	% The det stack frame is organized as follows.
	%
	%		... unused ...
	%	sp --->	<first unused slot>
	%		<space for local var 1>
	%		... local vars ...
	%		<space for local var n>
	%		<space for temporary reg save 1>
	%		... temporary reg saves ...
	%		<space for temporary reg save n>
	%		<space for succip>
	%
	% The stack pointer points to the first free location at the
	% top of the stack.
	%
	% `code_info__num_stackslots' counts the number of slots reserved 
	% for saving local variables.
	%
	% `code_info__max_push_count' counts the number of slots reserved
	% for saving and restoring registers (hp, redoip, etc.)
	%
	% `code_info__succip_used' determines whether we need a slot to
	% hold the succip.
	%
	% The variable part of the nondet stack is organized in the same way
	% as the det stack (but the nondet stack also contains several other
	% fixed fields.)

%---------------------------------------------------------------------------%

	% Returns the total stackslot count, but not including space for
	% succip.
code_info__get_total_stackslot_count(NumSlots) -->
	code_info__get_stackslot_count(SlotsForVars),
	code_info__get_max_push_count(SlotsForTemps),
	{ NumSlots is SlotsForVars + SlotsForTemps }.

%---------------------------------------------------------------------------%

	% `push_temp' doesn't actually increment the stack pointer, it just
	% increments the push count.  The space will be allocated in the
	% procedure prologue.

:- pred code_info__push_temp(lval, code_info, code_info).
:- mode code_info__push_temp(out, in, out) is det.

code_info__push_temp(StackVar) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 + 1 },
	code_info__set_push_count(Count),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count + NumSlots },
	code_info__stack_variable(Slot, StackVar),
	code_info__get_pushed_values(VStack0),
	{ stack__push(VStack0, StackVar, VStack) },
	code_info__set_pushed_values(VStack).

%---------------------------------------------------------------------------%

	% `pop_stack' and `pop_temp' don't actually decrement the stack
	% pointer, they just decrement the push count.  The space will
	% be deallocated in the procedure epilogue.

code_info__pop_stack(empty) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	code_info__get_pushed_values(VStack0),
	{ stack__pop_det(VStack0, _, VStack) },
	code_info__set_pushed_values(VStack).

:- pred code_info__pop_temp(lval, code_info, code_info).
:- mode code_info__pop_temp(out, in, out) is det.

code_info__pop_temp(StackVar) -->
	code_info__get_push_count(Count0),
	{ Count is Count0 - 1 },
	code_info__set_push_count(Count),
	code_info__get_pushed_values(VStack0),
	{ stack__pop_det(VStack0, _, VStack) },
	code_info__set_pushed_values(VStack),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count0 + NumSlots },
	code_info__stack_variable(Slot, StackVar).

:- pred code_info__get_stack_top(lval, code_info, code_info).
:- mode code_info__get_stack_top(out, in, out) is det.

code_info__get_stack_top(StackVar) -->
	code_info__get_push_count(Count),
	code_info__get_stackslot_count(NumSlots),
	{ Slot is Count + NumSlots },
	code_info__stack_variable(Slot, StackVar).

%---------------------------------------------------------------------------%

code_info__generate_failure(Code) -->
	code_info__failure_cont(Cont),
	{ code_info__failure_cont_address(Cont, FailureAddress) },
	{ Code = node([ goto(FailureAddress) - "fail" ]) }.

code_info__generate_test_and_fail(Rval, Code) -->
	code_info__failure_cont(Cont),
	{ code_info__failure_cont_address(Cont, FailureAddress) },
	{ code_util__neg_rval(Rval, NegRval) },
	{ Code = node([ if_val(NegRval, FailureAddress) -
				"test for failure" ]) }.

:- pred code_info__failure_cont_address(failure_cont, code_addr).
:- mode code_info__failure_cont_address(in, out) is det.

code_info__failure_cont_address(known(Label), label(Label)).
code_info__failure_cont_address(do_fail, do_fail).
code_info__failure_cont_address(unknown, do_redo).

%---------------------------------------------------------------------------%

	% `pre_commit' and `commit' should be generated as a pair
	% surrounding a non-det goal.
	% If the goal succeeds, the `commit' will cut any choice points
	% generated in the goal.

code_info__generate_pre_commit(PreCommit, FailLabel) -->
	code_info__get_next_label(FailLabel),
	code_info__push_temp(MaxfrSlot),
	code_info__push_temp(RedoipSlot),
	{ SaveCode = node([
		assign(MaxfrSlot, lval(maxfr)) - "Save nondet stack pointer",
		assign(RedoipSlot, lval(curredoip)) - "Save current redoip"
	]) },
	code_info__push_failure_cont(known(FailLabel)),
	{ SetRedoIp = node([
		modframe(label(FailLabel)) - "hijack the failure continuation"
	]) },
	{ PreCommit = tree(SaveCode, SetRedoIp) }.

code_info__generate_commit(FailLabel, Commit) -->
	code_info__get_next_label(SuccLabel),
	{ GotoSuccCode = node([
		goto(label(SuccLabel)) - "jump to success continuation",
		label(FailLabel) - "failure continuation"
	]) },
	{ SuccLabelCode = node([
		label(SuccLabel) - "success continuation"
	]) },
	code_info__pop_failure_cont,
	code_info__pop_temp(RedoIpSlot),
	code_info__pop_temp(MaxfrSlot),
	{ RestoreRedoIp = node([
		assign(curredoip, lval(RedoIpSlot)) - "Restore current redoip"
	]) },
	{ RestoreMaxfr = node([
		assign(maxfr, lval(MaxfrSlot)) - "Restore nondet stack pointer"
	]) },
	code_info__generate_failure(Fail),
	{ RestoreRegs = tree(RestoreRedoIp, RestoreMaxfr) },
	{ FailCode = tree(RestoreRedoIp, Fail) },
	{ SuccessCode = RestoreRegs },
	{ Commit = tree(GotoSuccCode, tree(FailCode,
		tree(SuccLabelCode, SuccessCode))) }.

%---------------------------------------------------------------------------%

code_info__push_failure_cont(Cont) -->
	code_info__get_fall_through(Fall0),
	{ stack__push(Fall0, Cont, Fall) },
	code_info__set_fall_through(Fall).

code_info__pop_failure_cont -->
	( 
		code_info__get_fall_through(Fall0),
		{ stack__pop(Fall0, _, Fall) },
		code_info__set_fall_through(Fall)
	->
		[]
	;
		{ error("code_info__pop_failure_cont: empty stack") }
	).

code_info__failure_cont(Cont) -->
	code_info__get_fall_through(Fall),
	(
		{ stack__top(Fall, Cont0) }
	->
		{ Cont = Cont0 }
	;
		{ error("code_info__failure_cont: no failure continuation") }
	).

code_info__restore_failure_cont(ContCode) -->
	code_info__failure_cont(FailCont),
	( { FailCont = known(ContLab) } ->
		{ Label = label(ContLab) }
	; { FailCont = do_fail } ->
		{ Label = do_fail }
	;
		{ error("cannot restore unknown failure continuation") }
	),
	{ ContCode = node([
		modframe(Label) - "Restore failure continuation"
	]) }.

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

code_info__generate_stack_livevals(LiveVals) -->
	code_info__get_live_variables(LiveVars),
	{ bintree_set__init(LiveVals0) },
	code_info__generate_stack_livevals_2(LiveVars, LiveVals0, LiveVals1),
	code_info__get_pushed_values(Pushed),
	{ code_info__generate_stack_livevals_3(Pushed, LiveVals1, LiveVals) }.

:- pred code_info__generate_stack_livevals_2(list(var), bintree_set(lval),
						bintree_set(lval),
						code_info, code_info).
:- mode code_info__generate_stack_livevals_2(in, in, out, in, out) is det.

code_info__generate_stack_livevals_2([], Vals, Vals) --> [].
code_info__generate_stack_livevals_2([V|Vs], Vals0, Vals) -->
	(
		code_info__get_variables(Variables),
		{ map__search(Variables, V, evaluated(ValsSet)) }
	->
			% When 'set' becomes 'bintree_set' (RSN)
			% this code should be modified to make better
			% use of the accumulator.
		{ set__to_sorted_list(ValsSet, ValsList) },
		{ code_info__filter_out_registersB(ValsList, Vals1) },
		{ bintree_set__union(Vals0, Vals1, Vals2) }
	;
		{ Vals2 = Vals0 }
	),
	code_info__generate_stack_livevals_2(Vs, Vals2, Vals).

:- pred code_info__generate_stack_livevals_3(stack(lval), bintree_set(lval),
							bintree_set(lval)).
:- mode code_info__generate_stack_livevals_3(in, in, out) is det.

code_info__generate_stack_livevals_3(Stack0, Vals0, Vals) :-
	(
		stack__pop(Stack0, Top, Stack1)
	->
		bintree_set__insert(Vals0, Top, Vals1),
		code_info__generate_stack_livevals_3(Stack1, Vals1, Vals)
	;
		Vals = Vals0
	).

%---------------------------------------------------------------------------%

:- pred code_info__filter_out_registers(list(pair(lval, var)), 
						bintree_set(pair(lval, var))).
:- mode code_info__filter_out_registers(in, out) is det.

code_info__filter_out_registers([], Vals) :-
	bintree_set__init(Vals).
code_info__filter_out_registers([Val - Var|Vs], Vals) :-
	code_info__filter_out_registers(Vs, Vals0),
	(
		Val = reg(_) % should include transitives....
	->
		Vals = Vals0
	;
		bintree_set__insert(Vals0, Val - Var, Vals)
	).

:- pred code_info__filter_out_registersB(list(lval), 
						bintree_set(lval)).
:- mode code_info__filter_out_registersB(in, out) is det.

code_info__filter_out_registersB([], Vals) :-
	bintree_set__init(Vals).
code_info__filter_out_registersB([Val|Vs], Vals) :-
	code_info__filter_out_registersB(Vs, Vals0),
	(
		Val = reg(_) % should include transitives....
	->
		Vals = Vals0
	;
		bintree_set__insert(Vals0, Val, Vals)
	).

%---------------------------------------------------------------------------%

code_info__get_requests(Requests) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_get_unify_requests(ModuleInfo, Requests) }.

%---------------------------------------------------------------------------%

code_info__apply_instmap_delta(Delta) -->
	code_info__get_instmap(InstMap0),
	{ apply_instmap_delta(InstMap0, Delta, InstMap) },
	code_info__set_instmap(InstMap).

%---------------------------------------------------------------------------%

	% XXX this pred will need to be rewritten to lookup variable shapes
code_info__generate_stack_livelvals(LiveVals) -->
	code_info__get_live_variables(LiveVars),
        { bintree_set__init(LiveVals0) },
        code_info__generate_stack_livelvals_2(LiveVars, LiveVals0, LiveVals1),
	{ bintree_set__to_sorted_list(LiveVals1, LiveVals2) },
	code_info__livevals_to_livelvals(LiveVals2, LiveVals3),
        code_info__get_pushed_values(Pushed),
        { code_info__generate_stack_livelvals_3(Pushed, LiveVals3, LiveVals) }.

:- pred code_info__generate_stack_livelvals_2(list(var), 
					bintree_set(pair(lval, var)),
					bintree_set(pair(lval, var)),
						code_info, code_info).
:- mode code_info__generate_stack_livelvals_2(in, in, out, in, out) is det.

code_info__generate_stack_livelvals_2([], Vals, Vals) --> [].
code_info__generate_stack_livelvals_2([V|Vs], Vals0, Vals) -->
	(
		code_info__get_variables(Variables),
		{ map__search(Variables, V, evaluated(ValsSet)) }
	->
			% When 'set' becomes 'bintree_set' (RSN)
			% this code should be modified to make better
			% use of the accumulator.
		{ set__to_sorted_list(ValsSet, ValsList) },
		{ list__length(ValsList, ValsLength) },
		{ list__duplicate(ValsLength, V, VarList) },
		{ assoc_list__from_corresponding_lists(ValsList, VarList,
							 VList) },
		{ code_info__filter_out_registers(VList, Vals1) },
		{ bintree_set__union(Vals0, Vals1, Vals2) }
	;
		{ Vals2 = Vals0 }
	),
	code_info__generate_stack_livelvals_2(Vs, Vals2, Vals).

:- pred code_info__generate_stack_livelvals_3(stack(lval), list(liveinfo),
						list(liveinfo)).
:- mode code_info__generate_stack_livelvals_3(in, in, out) is det.

code_info__generate_stack_livelvals_3(Stack0, LiveInfo0, LiveInfo) :-
	(
		stack__pop(Stack0, Top, Stack1)
	->
		LiveInfo = [live_lvalue(Top, -1) | Lives],
		code_info__generate_stack_livelvals_3(Stack1, LiveInfo0, Lives)
	;
		LiveInfo = LiveInfo0
	).





:- pred code_info__livevals_to_livelvals(list(pair(lval, var)), list(liveinfo),
					 code_info, code_info).
:- mode code_info__livevals_to_livelvals(in, out, in, out) is det.

code_info__livevals_to_livelvals([], [], C, C).
code_info__livevals_to_livelvals([L - V|Ls], [live_lvalue(L, S_Num )|Lives]) --> 
	code_info__get_module_info(Module),
	{ module_info_shapes(Module, S_Tab0) } ,
	{ module_info_types(Module, Type_Table) } ,
	code_info__variable_type(V, Type),
	{ shapes__request_shape_number(Type - ground, Type_Table, 
					S_Tab0, S_Tab1, S_Num) } ,
	{ module_info_set_shapes(Module, S_Tab1, Module1) },
	code_info__set_module_info(Module1),
	code_info__livevals_to_livelvals(Ls, Lives).

%---------------------------------------------------------------------------%

code_info__variable_type(Var, Type) -->
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) }.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

:- pred code_info__get_stackslot_count(int, code_info, code_info).
:- mode code_info__get_stackslot_count(out, in, out) is det.

:- pred code_info__get_registers(register_info, code_info, code_info).
:- mode code_info__get_registers(out, in, out) is det.

:- pred code_info__set_registers(register_info, code_info, code_info).
:- mode code_info__set_registers(in, in, out) is det.

:- pred code_info__get_variables(variable_info, code_info, code_info).
:- mode code_info__get_variables(out, in, out) is det.

:- pred code_info__set_variables(variable_info, code_info, code_info).
:- mode code_info__set_variables(in, in, out) is det.

		% Get the fall though point for failure
:- pred code_info__get_fall_through(fall_through, code_info, code_info).
:- mode code_info__get_fall_through(out, in, out) is det.

		% Set the fall though point for failure
:- pred code_info__set_fall_through(fall_through, code_info, code_info).
:- mode code_info__set_fall_through(in, in, out) is det.

:- pred code_info__get_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__get_liveness_info(out, in, out) is det.

:- pred code_info__set_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__set_liveness_info(in, in, out) is det.

:- pred code_info__get_proc_category(category, code_info, code_info).
:- mode code_info__get_proc_category(out, in, out) is det.

:- pred code_info__get_push_count(int, code_info, code_info).
:- mode code_info__get_push_count(out, in, out) is det.

:- pred code_info__set_push_count(int, code_info, code_info).
:- mode code_info__set_push_count(in, in, out) is det.

:- pred code_info__get_max_push_count(int, code_info, code_info).
:- mode code_info__get_max_push_count(out, in, out) is det.

:- pred code_info__set_max_push_count(int, code_info, code_info).
:- mode code_info__set_max_push_count(in, in, out) is det.

:- pred code_info__get_store_map(stack(map(var, lval)), code_info, code_info).
:- mode code_info__get_store_map(out, in, out) is det.

:- pred code_info__set_store_map(stack(map(var, lval)), code_info, code_info).
:- mode code_info__set_store_map(in, in, out) is det.

:- pred code_info__get_pushed_values(stack(lval), code_info, code_info).
:- mode code_info__get_pushed_values(out, in, out) is det.

:- pred code_info__set_pushed_values(stack(lval), code_info, code_info).
:- mode code_info__set_pushed_values(in, in, out) is det.

code_info__get_stackslot_count(A, CI, CI) :-
	CI = code_info(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

% code_info__set_stackslot_count(A, CI0, CI) :-
% 	CI0 =code_info(_, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
% 	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_label_count(B, CI, CI) :-
	CI = code_info(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_label_count(B, CI0, CI) :-
	CI0 = code_info(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_varset(C, CI, CI) :-
	CI = code_info(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_varset(C, CI0, CI) :-
	CI0 = code_info(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_call_info(D, CI, CI) :-
	CI = code_info(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_call_info(D, CI0, CI) :-
	CI0 = code_info(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_pred_id(E, CI, CI) :-
	CI = code_info(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_pred_id(E, CI0, CI) :-
	CI0 = code_info(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_proc_id(F, CI, CI) :-
	CI = code_info(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_proc_id(F, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_registers(G, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_registers(G, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_variables(H, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_variables(H, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, _, I, J, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_proc_info(I, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, _, _, _).

code_info__get_succip_used(J, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, _, _, _).

code_info__set_succip_used(J, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_fall_through(K, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, _, _, _).

code_info__set_fall_through(K, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_module_info(L, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, _, _, _).

code_info__set_module_info(L, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_liveness_info(M, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _, _, _, _).

code_info__set_liveness_info(M, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_store_map(N, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, _, _, _).

code_info__set_store_map(N, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, P, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_proc_category(O, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _, _, _, _).

code_info__get_instmap(P, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, _, _, _).

code_info__set_instmap(P, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _, Q, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_push_count(Q, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		Q - _, _, _).

code_info__get_max_push_count(QMax, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_ - QMax, _, _).

code_info__set_push_count(Q, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		_ - QMax0, R, S),
	int__max(QMax0, Q, QMax),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q - QMax, R, S).

code_info__set_max_push_count(QMax, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q - _, R, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q - QMax, R, S).

code_info__get_globals(R, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, R, _).

code_info__set_globals(R, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, _, S),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

code_info__get_pushed_values(S, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, S).

code_info__set_pushed_values(S, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, _),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S).

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
	code_info__set_fall_through(J, C4, C5),
	code_info__get_max_push_count(PC, C1, _),
	code_info__set_max_push_count(PC, C5, C).

%---------------------------------------------------------------------------%

:- pred code_info__make_assignment_comment(var, lval, string,
							code_info, code_info).
:- mode code_info__make_assignment_comment(in, in, out, in, out) is det.

code_info__make_assignment_comment(Var, _Lval, Comment) -->
	code_info__variable_to_string(Var, Name),
	{ string__append("Assigning from ", Name, Comment) }.

code_info__variable_to_string(Var, VarName) -->
	code_info__get_varset(Varset),
	(
		{ varset__lookup_name(Varset, Var, Name) }
	->
		{ VarName = Name }
	;
		{ term__var_to_int(Var, Int) },
		{ string__int_to_string(Int, IntString) },
		{ string__append("variable number ", IntString, VarName) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
