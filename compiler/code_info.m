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
%	o  Variables are never stored in more than one place.
%		The exception to this is when variables are stored in
%		stack slots across calls, but this case is handled
%		separately by making the mapping between stackslots and
%		variables bijective.
%	o  Procedures are in superhomogeneous form. This means that
%		construction unifications and builtins are not nested.
%	o  Evaluation of arguments in construction and deconstruction
%		unifications is lazy. This means that arguments in a
%		`don't care' mode are ignored.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_info.

:- interface.

:- import_module hlds, llds.
:- import_module code_util, tree, set.

:- type code_info.

:- type code_tree	==	tree(list(instruction)).

		% Create a new code_info structure.
:- pred code_info__init(int, varset, liveness_info, call_info, pred_id,
					proc_id, module_info, code_info).
:- mode code_info__init(in, in, in, in, in, in, in, out) is det.

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

:- pred code_info__get_call_info(call_info, code_info, code_info).
:- mode code_info__get_call_info(out, in, out) is det.

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

:- pred code_info__get_module_info(module_info, code_info, code_info).
:- mode code_info__get_module_info(out, in, out) is det.

:- pred code_info__set_module_info(module_info, code_info, code_info).
:- mode code_info__set_module_info(in, in, out) is det.

:- pred code_info__get_free_register(reg, code_info, code_info).
:- mode code_info__get_free_register(out, in, out) is det.

:- pred code_info__clear_reserved_registers(code_info, code_info).
:- mode code_info__clear_reserved_registers(in, out) is det.

:- pred code_info__flush_expression_cashe(list(instruction),
						code_info, code_info).
:- mode code_info__flush_expression_cashe(out, in, out) is det.

:- pred code_info__flush_variable(var, list(instruction), code_info, code_info).
:- mode code_info__flush_variable(in, out, in, out) is det.

:- pred code_info__cashe_expression(var, rval, code_info, code_info).
:- mode code_info__cashe_expression(in, in, in, out) is det.

:- pred code_info__cashe_expression_with_target(var, rval, lval,
							code_info, code_info).
:- mode code_info__cashe_expression_with_target(in, in, in, in, out) is det.

:- pred code_info__shuffle_register(reg, list(instruction),
						code_info, code_info).
:- mode code_info__shuffle_register(in, out, in, out) is det.

:- pred code_info__get_fall_through(label, code_info, code_info).
:- mode code_info__get_fall_through(out, in, out) is det.

:- pred code_info__set_fall_through(label, code_info, code_info).
:- mode code_info__set_fall_through(in, in, out) is det.

:- pred code_info__generate_fall_through(list(instruction), code_info,
								code_info).
:- mode code_info__generate_fall_through(out, in, out) is det.

:- pred code_info__reserve_register(reg, code_info, code_info).
:- mode code_info__reserve_register(in, in, out) is det.

:- pred code_info__save_variable_on_stack(var, list(instruction),
							code_info, code_info).
:- mode code_info__save_variable_on_stack(in, out, in, out) is det.

:- pred code_info__variable_is_live(var, code_info, code_info).
:- mode code_info__variable_is_live(in, in, out) is semidet.

:- pred code_info__get_pred_proc_arginfo(pred_id, proc_id, list(arg_info),
							code_info, code_info).
:- mode code_info__get_pred_proc_arginfo(in, in, out, in, out) is det.

:- pred code_info__get_arginfo(list(arg_info), code_info, code_info).
:- mode code_info__get_arginfo(out, in, out) is det.

:- pred code_info__get_headvars(list(var), code_info, code_info).
:- mode code_info__get_headvars(out, in, out) is det.

:- pred code_info__generate_expression(rval, lval, list(instruction),
							code_info, code_info).
:- mode code_info__generate_expression(in, in, out, in, out) is det.

:- pred code_info__get_variable_register(var, lval, code_info, code_info).
:- mode code_info__get_variable_register(in, out, in, out) is det.

:- pred code_info__variable_register(var, lval, code_info, code_info).
:- mode code_info__variable_register(in, out, in, out) is semidet.

:- pred code_info__cons_id_to_abstag(cons_id, abstag, code_info, code_info).
:- mode code_info__cons_id_to_abstag(in, out, in, out) is det.

:- pred code_info__get_stackslot_count(int, code_info, code_info).
:- mode code_info__get_stackslot_count(out, in, out) is det.

:- pred code_info__reset_variable_target(var, lval, code_info, code_info).
:- mode code_info__reset_variable_target(in, in, in, out) is semidet.

:- pred code_info__clear_all_variables_and_registers(code_info, code_info).
:- mode code_info__clear_all_variables_and_registers(in, out) is det.

:- pred code_info__set_variable_register(var, reg, code_info, code_info).
:- mode code_info__set_variable_register(in, in, in, out) is det.

:- pred code_info__get_live_variables(list(var), code_info, code_info).
:- mode code_info__get_live_variables(out, in, out) is det.

:- pred code_info__generate_forced_saves(assoc_list(var, int), code_tree,
						code_info, code_info).
:- mode code_info__generate_forced_saves(in, out, in, out) is det.

:- pred code_info__grab_code_info(code_info, code_info, code_info).
:- mode code_info__grab_code_info(out, in, out) is det.

:- pred code_info__slap_code_info(code_info, code_info, code_info).
:- mode code_info__slap_code_info(in, in, out) is det.

:- pred code_info__register_variable(reg, var, code_info, code_info).
:- mode code_info__register_variable(in, out, in, out) is semidet.

:- pred code_info__get_succip_used(bool, code_info, code_info).
:- mode code_info__get_succip_used(out, in, out) is det.

:- pred code_info__set_succip_used(bool, code_info, code_info).
:- mode code_info__set_succip_used(in, in, out) is det.

:- pred code_info__remake_code_info(code_info, code_info).
:- mode code_info__remake_code_info(in, out) is det.

:- pred code_info__update_liveness_info(delta_liveness, code_info, code_info).
:- mode code_info__update_liveness_info(in, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module require, list, map, bimap, tree, int, std_util.
:- import_module string, varset.

:- type code_info	--->
		code_info(
			int,		% Counter for the number of stack slots
					% allocated so far. Used to mangage
					% the stack frame.
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
			stackslot_info,	% A bijective map between variables and
					% stack locations.
			bool,		% do we need to store succip?
			fall_through,	% The fallthrough label for semi-
					% deterministic code.
			module_info,	% The module_info structure - you just
					% never know when you might need it.
			liveness_info
	).

:- type register_info	==	map(reg, register_stat).

:- type register_stat	--->	unused
			;	reserved
			;	result
			;	var(var)
			;	iconst(int).

:- type target_register	--->	none
			;	target(lval).

:- type variable_info	==	map(var, variable_stat).

:- type variable_stat	--->	unused
			;	evaluated(lval)
			;	cashed(rval, target_register).

:- type stackslot_info	==	bimap(stackslot_num, var).

:- type stackslot_num	==	int.

:- type result_register	--->	unallocated
			;	allocated(reg).

:- type fall_through	--->	none
			;	label(label).

code_info__init(SlotCount, Varset, Liveness, CallInfo,
					PredId, ProcId, ModuleInfo, C) :-
	code_info__init_register_info(PredId, ProcId,
					ModuleInfo, RegisterInfo),
	code_info__init_variable_info(PredId, ProcId,
					ModuleInfo, VariableInfo),
	code_info__init_stackslot_info(PredId, ProcId,
					ModuleInfo, StackslotInfo),
	SuccIPUsed = yes, % succip is always saved.
	FallThough = none,
	C = code_info(
		SlotCount,
		0,
		Varset,
		CallInfo, 
		PredId,
		ProcId,
		RegisterInfo,
		VariableInfo,
		StackslotInfo,
		SuccIPUsed,
		FallThough,
		ModuleInfo,
		Liveness
	).

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
	list__nth_member_lookup(HeadVars, N0, Var),
	(
		Mode = top_in
	->
		map__set(RegisterInfo0, Reg, var(Var), RegisterInfo1)
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
		list__nth_member_lookup(HeadVars, N0, Var),
		LVAL = reg(Reg),
		map__set(VariableInfo0, Var, evaluated(LVAL), VariableInfo1)
	;
		VariableInfo1 = VariableInfo0
	),
	N1 is N0 + 1,
	code_info__init_var_info_2(ArgVars, N1, HeadVars,
						VariableInfo1, VariableInfo).

%---------------------------------------------------------------------------%

:- pred code_info__init_stackslot_info(pred_id, proc_id, module_info,
								stackslot_info).
:- mode code_info__init_stackslot_info(in, in, in, out) is det.

code_info__init_stackslot_info(_PredId, _ProcId, _ModuleInfo, StackslotInfo) :-
	bimap__init(StackslotInfo).

%---------------------------------------------------------------------------%

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

code_info__flush_expression_cashe(Code) -->
	code_info__get_variables(Variables0),
	{ map__keys(Variables0, VarList) },
	code_info__flush_exp_cashe_2(VarList, Code).

:- pred code_info__flush_exp_cashe_2(list(var), list(instruction),
						code_info, code_info).
:- mode code_info__flush_exp_cashe_2(in, out, in, out) is det.

code_info__flush_exp_cashe_2([], []) --> [].
code_info__flush_exp_cashe_2([Var|Vars], Code) -->
	code_info__flush_exp_cashe_2(Vars, Code0),
	code_info__flush_variable(Var, Code1),
	{ list__append(Code1, Code0, Code) }.

%---------------------------------------------------------------------------%

code_info__flush_variable(Var, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = cashed(Exprn, Target) }
	->
		code_info__target_to_lvalue(Target, Exprn, TargetReg),
			% Generate code to evaluate the expression.
		code_info__generate_expression(Exprn, TargetReg, Code),
			% Mark the register as taken, so that
			% it doesn't get allocated
			% NOTE:
			%	1. The mode analysis ensures that
			%	the variable does not depend on
			%	itself (occurs check!)
			%	2. Code reordering guarentees that
			%	the target register is available.
			%	(I think. comment fjh?)
		(
			{ TargetReg = reg(Reg) }
		->
			code_info__get_registers(Registers0),
			{ map__set(Registers0, Reg, var(Var), Registers) },
			code_info__set_registers(Registers)
		;
			{ true }
		),
		code_info__get_variables(Variables0),
		{ map__set(Variables0, Var, evaluated(TargetReg), Variables) },
		code_info__set_variables(Variables)
	;
		{ Code = [] }
	).

%---------------------------------------------------------------------------%

	% unused - do nothing.
code_info__generate_expression(unused, _, []) --> [].
code_info__generate_expression(lval(Lval), TargetReg, Code) -->
	{ Code = [assign(TargetReg, lval(Lval)) - "Copy lvalue"] }.
code_info__generate_expression(var(Var), TargetReg, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = evaluated(Lval0) },
		{ not TargetReg = Lval0 }
	->
		code_info__make_assignment_comment(Var, TargetReg, Comment),
		{ Code = [ assign(TargetReg, lval(Lval0)) - Comment] }
	;
		{ VarStat = cashed(Exprn, target(TargetReg1)) }
	->
		code_info__generate_expression(Exprn, TargetReg1, Code0),
			% Mark the target register.
		code_info__get_variables(Variables0),
		{ map__set(Variables0, Var, evaluated(TargetReg1), Variables) },
		code_info__set_variables(Variables),
			% Assemble the code
		(
			{ not TargetReg = TargetReg1 }
		->
			code_info__make_assignment_comment(Var,
							TargetReg, Comment),
			{ Code1 = [ assign(TargetReg, lval(TargetReg1)) -
				Comment] }
		;
			{ Code1 = [] }
		),
		{ list__append(Code0, Code1, Code) }
	;
		{ VarStat = cashed(Exprn, none) }
	->
		code_info__generate_expression(Exprn, TargetReg, Code0),
			% Mark the target register.
		code_info__get_variables(Variables0),
		{ map__set(Variables0, Var, evaluated(TargetReg), Variables) },
		code_info__set_variables(Variables),
			% Assemble the code
		{ Code = Code0 }
	;
		% { VarStat = unused } or
		% { Lval0 = Lval }
		{ Code = [] }
	).
code_info__generate_expression(binop(Op, L0, R0), TargetReg, Code) -->
	code_info__generate_expression_vars(L0, L, Code0),
	code_info__generate_expression_vars(R0, R, Code1),
	{ ThisCode = [ assign(TargetReg, binop(Op, L, R)) -
				"Evaluate binary expression"] },
	{ list__append(Code1, ThisCode, Code2) },
	{ list__append(Code0, Code2, Code) }.
code_info__generate_expression(create(Tag, Args), TargetReg, Code) -->
	{ list__length(Args, Arity) },
	(
		{ Tag = simple(TagNum0) }
	->
		{ TagNum = TagNum0 },
		{ HInc = Arity },
		{ Field = 0 }
	;
		{ Tag = unsimple(TagNum) },
		{ HInc = 1 },
		{ Field = 1 }
	),
	(
		{ Arity > 0 }
	->
		{ CodeA = [
			assign(TargetReg, lval(hp)) - "Get the heap memory",
			incr_hp(HInc) - "Allocate heap space",
			assign(TargetReg, mkword(TagNum, lval(TargetReg))) -
							"Tag the pointer"
		] }
	;
		{ CodeA = [
			assign(TargetReg, iconst(TagNum)) -
					"Assign an enumeration"
		] }
	),
	code_info__generate_cons_args(TargetReg, TagNum, Field, Args, CodeB),
	{ list__append(CodeA, CodeB, Code) }.
code_info__generate_expression(mkword(Tag, Rval0), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = [assign(TargetReg, mkword(Tag, Rval)) - "Tag a word"] },
	{ list__append(Code0, Code1, Code) }.
code_info__generate_expression(iconst(Int), TargetReg, Code) -->
	{ Code = [assign(TargetReg, iconst(Int)) - "Make a integer const"] }.
code_info__generate_expression(sconst(Str), TargetReg, Code) -->
	{ Code = [assign(TargetReg, sconst(Str)) - "Make a string const"] }.
code_info__generate_expression(field(Tag, Rval0, Field), TargetReg, Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code0),
	{ Code1 = [assign(TargetReg, field(Tag, Rval, Field)) -
						"extract a field of a term"] },
	{ list__append(Code0, Code1, Code) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_cons_args(lval, int, int, list(rval),
				list(instruction), code_info, code_info).
:- mode code_info__generate_cons_args(in, in, in, in, out, in, out) is det.

code_info__generate_cons_args(_Reg, _Tag, _Field0, [], []) --> [].
code_info__generate_cons_args(Reg, Tag, Field0, [Arg|Args], Code) -->
	code_info__generate_expression(Arg, field(Tag, Reg, Field0), Code0),
	{ Field1 is Field0 + 1 },
	code_info__generate_cons_args(Reg, Tag, Field1, Args, Code1),
	{ list__append(Code0, Code1, Code) }.

%---------------------------------------------------------------------------%

:- pred code_info__generate_expression_vars(rval, rval, list(instruction),
							code_info, code_info).
:- mode code_info__generate_expression_vars(in, out, out, in, out) is det.

code_info__generate_expression_vars(unused, unused, []) --> [].
code_info__generate_expression_vars(lval(Lval), lval(Lval), []) --> [].
code_info__generate_expression_vars(var(Var), Result, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = evaluated(Lval0) }
	->
		{ Result = lval(Lval0) },
		{ Code = [] }
	;
		{ VarStat = cashed(Exprn, Target) }
	->
		code_info__target_to_lvalue(Target, Exprn, TargetReg),
		code_info__generate_expression(Exprn, TargetReg, Code),
		{ Result = lval(TargetReg) }
	;
		% { VarStat = unused },
		{ Result = unused },
		{ Code = [] }
	).
code_info__generate_expression_vars(binop(Op, L0, R0),
						binop(Op, L, R), Code) -->
	code_info__generate_expression_vars(L0, L, Code0),
	code_info__generate_expression_vars(R0, R, Code1),
	{ list__append(Code0, Code1, Code) }.
code_info__generate_expression_vars(create(Tag, Rvals0), create(Tag, Rvals),
								Code) -->
	code_info__generate_expression_vars_2(Rvals0, Rvals, Code).
code_info__generate_expression_vars(mkword(Tag, Rval0), mkword(Tag, Rval),
								Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).
code_info__generate_expression_vars(iconst(Int), iconst(Int), []) --> [].
code_info__generate_expression_vars(sconst(Str), sconst(Str), []) --> [].
code_info__generate_expression_vars(field(Tag, Rval0, Field),
					field(Tag, Rval, Field), Code) -->
	code_info__generate_expression_vars(Rval0, Rval, Code).

%---------------------------------------------------------------------------%

:- pred code_info__generate_expression_vars_2(list(rval), list(rval), 
				list(instruction), code_info, code_info).
:- mode code_info__generate_expression_vars_2(in, out, out, in, out) is det.

code_info__generate_expression_vars_2([], [], []) --> [].
code_info__generate_expression_vars_2([R0|Rs0], [R|Rs], Code) -->
	code_info__generate_expression_vars(R0, R, Code0),
	code_info__generate_expression_vars_2(Rs0, Rs, Code1),
	{ list__append(Code0, Code1, Code) }.

%---------------------------------------------------------------------------%

:- pred code_info__target_to_lvalue(target_register, rval, lval, code_info,
								code_info).
:- mode code_info__target_to_lvalue(in, in, out, in, out) is det.

code_info__target_to_lvalue(target(Lvalue), _Exprn, Lvalue) --> [].
code_info__target_to_lvalue(none, Exprn, Lvalue) -->
	(
		code_info__evaluated_expression(Exprn, Lval0)
	->
		{ Lvalue = Lval0 }
	;
		code_info__get_free_register(Reg),
		{ Lvalue = reg(Reg) }
	).

:- pred code_info__evaluated_expression(rval, lval, code_info, code_info).
:- mode code_info__evaluated_expression(in, out, in, out) is semidet.


code_info__evaluated_expression(lval(Lval), Lval) --> [].
code_info__evaluated_expression(var(Var), Lval) -->
	code_info__get_variables(Variables),
	{ map__lookup(Variables, Var, VarStat) },
	(
		{ VarStat = evaluated(Lval0) }
	->
		{ Lval = Lval0 }
	;
		{ VarStat = cashed(Exprn,_) }
	->
		code_info__evaluated_expression(Exprn, Lval)
	;
		{ fail }
	).

%---------------------------------------------------------------------------%

code_info__cashe_expression(Var, Exprn) -->
	code_info__get_variables(Variables0),
	{ map__set(Variables0, Var, cashed(Exprn, none), Variables) },
	code_info__set_variables(Variables).

%---------------------------------------------------------------------------%

code_info__cashe_expression_with_target(Var, Exprn, TargetReg) -->
	code_info__get_variables(Variables0),
	{ map__set(Variables0, Var, cashed(Exprn, target(TargetReg)),
								Variables) },
	code_info__set_variables(Variables).

%---------------------------------------------------------------------------%

code_info__shuffle_register(Reg, Code) -->
	code_info__get_registers(Registers),
	(
		{ map__search(Registers, Reg, RegContents) }
	->
		code_info__shuffle_registers_2(Reg, RegContents, Code)
	;
		{ Code = [] }
	).

:- pred code_info__shuffle_registers_2(reg, register_stat, list(instruction),
						code_info, code_info).
:- mode code_info__shuffle_registers_2(in, in, out, in, out) is det.

code_info__shuffle_registers_2(Reg, Contents, Code) -->
	(
		{ Contents = unused }
	->
		{ Code = [] }
	;
		{ Contents = reserved }
	->
		{ error("Cannot shuffle a reserved register.") }
	;
		{ Contents = result }
	->
		{ error("Cannot shuffle the result register.") }
	;
		{ Contents = var(Var) }
	->
			% get a spare register
		code_info__get_free_register(NewReg),
			% Update the register info -
			% remove the entry for the old register,
			% and set the entry for the new register.
		code_info__get_registers(Registers0),
		{ map__delete(Registers0, Reg, Registers1) },
		{ map__set(Registers1, NewReg, var(Var), Registers) },
		code_info__set_registers(Registers),
			% Update the variable info -
			% Set the location of the variable to the
			% new register.
		code_info__get_variables(Variables0),
		{ map__set(Variables0, Var, evaluated(reg(NewReg)),
								Variables) },
		code_info__set_variables(Variables),
			% Generate the code fragment.
		{ Code = [assign(reg(NewReg), lval(reg(Reg))) -
				"Swap variable to a new register"] }
	;
		{ error("This can never happen") }
	).

%---------------------------------------------------------------------------%

code_info__save_variable_on_stack(Var, Code) -->
	code_info__get_variable(Var, VarStat),
	(
		{ VarStat = unused }
	->
		{ Code = [] }
	;
		{ VarStat = evaluated(Lval) }
	->
		code_info__get_stackslots(Stackslots0),
		(
			{ bimap__search(Stackslots0, _Slot0, Var) }
		->
			{ Code = [] }
		;
			code_info__get_call_info(CallInfo),
			{ map__lookup(CallInfo, Var, Slot) },
			{ bimap__set(Stackslots0, Slot, Var, StackSlots1) },
			code_info__set_stackslots(StackSlots1),
			{ Code = [assign(stackvar(Slot), lval(Lval)) -
						"Copy Lval to det stack"] }
		)
	;
		{ VarStat = cashed(Exprn, Target) }
	->
		code_info__target_to_lvalue(Target, Exprn, TargetReg1),
		code_info__generate_expression(Exprn, TargetReg1, Code0),
		code_info__get_variables(Variables1),
		{ map__set(Variables1, Var, evaluated(TargetReg1),
							Variables2) },
		code_info__set_variables(Variables2),
		code_info__get_stackslots(Stackslots2),
		(
			{ bimap__search(Stackslots2, _Slot1, Var) }
		->
			{ Code1 = [] }
		;
			code_info__get_call_info(CallInfo),
			{ map__lookup(CallInfo, Var, Slot2) },
			{ bimap__set(Stackslots2, Slot2, Var, StackSlots3) },
			code_info__set_stackslots(StackSlots3),
			{ Code1 = [assign(stackvar(Slot2), lval(TargetReg1)) -
						"Copy Lval to det stack"] }
		),
		{ list__append(Code0, Code1, Code) }
	;
		{ error("This should never happen") }
	).

%---------------------------------------------------------------------------%

code_info__reserve_register(Reg) -->
	code_info__get_registers(Registers0),
	(
		{ map__insert(Registers0, Reg, reserved, Registers) }
	->
		code_info__set_registers(Registers)
	;
		{ error("Cannot reserve a live register.") }
	).

%---------------------------------------------------------------------------%

code_info__get_next_label(Label) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_count(N0),
	{ N is N0 + 1 },
	{ code_util__make_local_label(PredId, ProcId, N, Label) },
	code_info__set_label_count(N).

%---------------------------------------------------------------------------%

code_info__get_current_label(Label) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_label_count(N),
	{ code_util__make_local_label(PredId, ProcId, N, Label) }.

%---------------------------------------------------------------------------%

:- pred code_info__get_free_stackslot(stackslot_num, code_info, code_info).
:- mode code_info__get_free_stackslot(out, in, out) is det.

code_info__get_free_stackslot(Slot) -->
	code_info__get_stackslots(StackSlots),
	{ bimap__ordinates(StackSlots, SlotKeys) },
	{ code_info__get_free_stackslot_2(0, SlotKeys, Slot) }.

:- pred code_info__get_free_stackslot_2(int, list(int), int).
:- mode code_info__get_free_stackslot_2(in, in, out) is det.

code_info__get_free_stackslot_2(N, [], N).
code_info__get_free_stackslot_2(N0, [S|Ss], Slot) :-
	(
		N0 < S
	->
		Slot = N0
	;
		N is S + 1,
		code_info__get_free_stackslot_2(N, Ss, Slot)
	).

%---------------------------------------------------------------------------%

code_info__get_variable_register(Var, Lval) -->
	code_info__get_variables(Variables),
	(
		{ map__search(Variables, Var, VarStat) },
		{ VarStat = evaluated(Lval0) }
	->
		{ Lval = Lval0 }
	;
		code_info__get_stackslots(StackSlots),
		{ bimap__search(StackSlots, Slot, Var) }
	->
		{ Lval = stackvar(Slot) }
	;
		{ error("Cannot find lvalue of variable") }
	).

%---------------------------------------------------------------------------%

code_info__variable_register(Var, Lval) -->
	code_info__get_variables(Variables),
	{ map__search(Variables, Var, VarStat) },
	{ VarStat = evaluated(Lval) }.

%---------------------------------------------------------------------------%

code_info__register_variable(Reg, Var) -->
	code_info__get_registers(Registers),
	{ map__search(Registers, Reg, RegStat) },
	{ RegStat = var(Var) }.

%---------------------------------------------------------------------------%

code_info__set_variable_register(Var, Reg) -->
	code_info__get_variables(Variables0),
	{ map__set(Variables0, Var, evaluated(reg(Reg)), Variables) },
	code_info__set_variables(Variables),
	code_info__get_registers(Registers0),
	{ map__set(Registers0, Reg, var(Var), Registers) },
	code_info__set_registers(Registers).

%---------------------------------------------------------------------------%

code_info__reset_variable_target(Var, Lval) -->
	code_info__get_variables(Variables0),
	(
		{ map__search(Variables0, Var, VarStat) },
		{ VarStat = cashed(Exprn, _) }
	->
		{ map__set(Variables0, Var, cashed(Exprn, target(Lval)),
								Variables) },
		code_info__set_variables(Variables)
	).


%---------------------------------------------------------------------------%

code_info__clear_all_variables_and_registers -->
	{ map__init(Registers) },
	code_info__set_registers(Registers),
	{ map__init(Variables) },
	code_info__set_variables(Variables).

%---------------------------------------------------------------------------%

code_info__cons_id_to_abstag(_ConsId, AbsTag) -->
	{ AbsTag = simple(3) }. % XXX stub

%---------------------------------------------------------------------------%

code_info__variable_is_live(Var) -->
	code_info__get_liveness_info(Liveness),
	{ set__member(Var, Liveness) }.

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
		code_info__get_stackslots(StackSlots),
		(
			{ bimap__search(StackSlots, Slot0, Var) }
		->
			{ VarLoc = evaluated(stackvar(Slot0)) },
			{ map__set(Variables, Var, VarLoc, Variables1) },
			code_info__set_variables(Variables1)
		;
			{ error("Variable not in register or on stack.") }
		)
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
	code_info__get_live_variables_2(Variables1, [], Variables).

:- pred code_info__get_live_variables_2(list(var), list(var), list(var),
						code_info, code_info).
:- mode code_info__get_live_variables_2(in, in, out, in, out) is det.

code_info__get_live_variables_2([], Vars, Vars) --> [].
code_info__get_live_variables_2([Var|Vars0], Vars1, Vars) -->
	(
		code_info__variable_is_live(Var)
	->
		{ Vars2 = [Var|Vars1] }
	;
		{ Vars2 = Vars1 }
	),
	code_info__get_live_variables_2(Vars0, Vars2, Vars).

%---------------------------------------------------------------------------%

code_info__generate_forced_saves([], empty) --> [].
code_info__generate_forced_saves([Var - Slot|VarSlots], Code) --> 
	code_info__get_variables(Variables),
	code_info__get_stackslots(StackSlots),
	(
		{ bimap__search(StackSlots, _, Var) }
	->
			% If a variable is on the stack but not
			% in the vars, then it has been generated,
			% so we do nothing.
		code_info__generate_forced_saves(VarSlots, Code)
	;
		{ map__search(Variables, Var, _) }
	->
			% The variable isn't on the stack,
			% so we check to see if it exists in a
			% register, or in the cashe
		code_info__generate_expression(var(Var), stackvar(Slot), Code0),
		{ CodeA = node(Code0) },
		code_info__generate_forced_saves(VarSlots, CodeB),
		{ Code = tree(CodeA, CodeB) }
	;
			% The variable is not live yet.
		code_info__generate_forced_saves(VarSlots, Code)
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

code_info__remake_code_info -->
	code_info__get_call_info(CallInfo0),
	{ map__init(Variables0) },
	{ map__to_assoc_list(CallInfo0, CallInfo) },
	{ code_info__remake_code_info_2(CallInfo, Variables0, Variables) },
	code_info__set_variables(Variables),
	{ map__init(Registers) },
	code_info__set_registers(Registers).

%---------------------------------------------------------------------------%

:- pred code_info__remake_code_info_2(assoc_list(var, int),
						variable_info, variable_info).
:- mode code_info__remake_code_info_2(in, in, out) is det.

code_info__remake_code_info_2([], Variables, Variables).
code_info__remake_code_info_2([V - S|VSs], Variables0, Variables) :-
	map__insert(Variables0,V, evaluated(stackvar(S)), Variables1),
	code_info__remake_code_info_2(VSs, Variables1, Variables).

%---------------------------------------------------------------------------%

code_info__update_liveness_info(Births - Deaths) -->
	code_info__get_liveness_info(Liveness0),
	{ set__difference(Liveness0, Deaths, Liveness1) },
	{ set__union(Liveness1, Births, Liveness) },
	code_info__set_liveness_info(Liveness).

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

:- pred code_info__get_stackslots(stackslot_info, code_info, code_info).
:- mode code_info__get_stackslots(out, in, out) is det.

:- pred code_info__set_stackslots(stackslot_info, code_info, code_info).
:- mode code_info__set_stackslots(in, in, out) is det.

:- pred code_info__get_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__get_liveness_info(out, in, out) is det.

:- pred code_info__set_liveness_info(liveness_info, code_info, code_info).
:- mode code_info__set_liveness_info(in, in, out) is det.

code_info__get_stackslot_count(A, CI, CI) :-
	CI = code_info(A, _, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_stackslot_count(A, CI0, CI) :-
	CI0 = code_info(_, B, C, D, E, F, G, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_label_count(B, CI, CI) :-
	CI = code_info(_, B, _, _, _, _, _, _, _, _, _, _, _).

code_info__set_label_count(B, CI0, CI) :-
	CI0 = code_info(A, _, C, D, E, F, G, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_varset(C, CI, CI) :-
	CI = code_info(_, _, C, _, _, _, _, _, _, _, _, _, _).

code_info__set_varset(C, CI0, CI) :-
	CI0 = code_info(A, B, _, D, E, F, G, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_call_info(D, CI, CI) :-
	CI = code_info(_, _, _, D, _, _, _, _, _, _, _, _, _).

code_info__set_call_info(D, CI0, CI) :-
	CI0 = code_info(A, B, C, _, E, F, G, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_pred_id(E, CI, CI) :-
	CI = code_info(_, _, _, _, E, _, _, _, _, _, _, _, _).

code_info__set_pred_id(E, CI0, CI) :-
	CI0 = code_info(A, B, C, D, _, F, G, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_proc_id(F, CI, CI) :-
	CI = code_info(_, _, _, _, _, F, _, _, _, _, _, _, _).

code_info__set_proc_id(F, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, _, G, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_registers(G, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, G, _, _, _, _, _, _).

code_info__set_registers(G, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, _, H, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_variables(H, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, H, _, _, _, _, _).

code_info__set_variables(H, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, _, I, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_stackslots(I, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, I, _, _, _, _).

code_info__set_stackslots(I, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, _, J, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_succip_used(J, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, J, _, _, _).

code_info__set_succip_used(J, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, _, K, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_fall_through(K, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, label(K), _, _).

code_info__set_fall_through(K, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, _, L, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, label(K), L, M).

code_info__get_module_info(L, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, L, _).

code_info__set_module_info(L, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, _, M),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

code_info__get_liveness_info(M, CI, CI) :-
	CI = code_info(_, _, _, _, _, _, _, _, _, _, _, _, M).

code_info__set_liveness_info(M, CI0, CI) :-
	CI0 = code_info(A, B, C, D, E, F, G, H, I, J, K, L, _),
	CI = code_info(A, B, C, D, E, F, G, H, I, J, K, L, M).

%---------------------------------------------------------------------------%

code_info__grab_code_info(C, C, C).

code_info__slap_code_info(C0, C1, C) :-
	code_info__get_label_count(L, C1, _),
	code_info__set_label_count(L, C0, C).

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
		{ Comment = "Assigning from an anonymous variable" }
	).

%---------------------------------------------------------------------------%

