%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module call_gen.

:- interface.

:- import_module hlds, llds, code_info, code_util.

:- pred call_gen__generate_det_call(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_det_call(in, in, in, out, in, out) is det.

:- pred call_gen__generate_semidet_call(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_call(in, in, in, out, in, out) is det.

:- pred call_gen__generate_det_builtin(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_det_builtin(in, in, in, out, in, out) is det.

:- pred call_gen__generate_semidet_builtin(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_builtin(in, in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require.

call_gen__generate_det_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	code_info__flush_expression_cashe(Code0),
	{ CodeA = node(Code0) },
	code_info__clear_reserved_registers,
	call_gen__setup_call(Args, CodeB),
	call_gen__save_variables(CodeC),
	code_info__get_next_label(ReturnLabel),
	{ code_util__make_entry_label(PredId, ModeId, Label) },
	{ CodeD = node([
		call(Label, ReturnLabel) - "branch to procedure",
		label(ReturnLabel) - "Continutation label"
	]) },
	{ Code = tree(CodeA, tree(CodeB, tree(CodeC, CodeD))) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	code_info__flush_expression_cashe(Code0),
	{ CodeA = node(Code0) },
	code_info__clear_reserved_registers,
	call_gen__setup_call(Args, CodeB),
	call_gen__save_variables(CodeC),
	code_info__get_next_label(ReturnLabel),
	code_info__get_fall_through(FallThrough),
	{ code_util__make_entry_label(PredId, ModeId, Label) },
	{ CodeD = node([
		call(Label, ReturnLabel) - "branch to procedure",
		label(ReturnLabel) - "Continutation label",
		if_not_val(lval(reg(r(1))), FallThrough) - "Test result"
	]) },
	{ Code = tree(CodeA, tree(CodeB, tree(CodeC, CodeD))) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

:- pred call_gen__setup_call(assoc_list(var, arg_info), code_tree,
							code_info, code_info).
:- mode call_gen__setup_call(in, out, in, out) is det.

call_gen__setup_call([], empty) --> [].
call_gen__setup_call([Var - arg_info(ArgLoc, Mode)|Vars], Code) -->
	(
		{ Mode = top_in }
	->
		{ code_util__arg_loc_to_register(ArgLoc, Reg) },
		(
			code_info__variable_register(Var, Lval),
			{ not Lval = reg(Reg) }
		->
			code_info__shuffle_register(Reg, Code0),
			code_info__generate_expression(var(Var), reg(Reg),
									Code1),
			code_info__reserve_register(Reg),
			{ CodeA = tree(node(Code0), node(Code1)) }
		;
			{ CodeA = empty }
		)
	;
		{ CodeA = empty }
	),
	call_gen__setup_call(Vars, CodeB),
	{ Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

:- pred call_gen__save_variables(code_tree, code_info, code_info).
:- mode call_gen__save_variables(out, in, out) is det.

call_gen__save_variables(Code) -->
	code_info__get_live_variables(Variables),
	call_gen__save_variables_2(Variables, Code).

:- pred call_gen__save_variables_2(list(var), code_tree, code_info, code_info).
:- mode call_gen__save_variables_2(in, out, in, out) is det.

call_gen__save_variables_2([], empty) --> [].
call_gen__save_variables_2([Var|Vars], Code) -->
	code_info__save_variable_on_stack(Var, Code0),
	{ CodeA = node(Code0) },
        call_gen__save_variables_2(Vars, CodeB),
        { Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

:- pred call_gen__rebuild_registers(assoc_list(var, arg_info),
							code_info, code_info).
:- mode call_gen__rebuild_registers(in, in, out).

call_gen__rebuild_registers(Args) -->
	code_info__clear_all_variables_and_registers,
	call_gen__rebuild_registers_2(Args).

:- pred call_gen__rebuild_registers_2(assoc_list(var, arg_info),
							code_info, code_info).
:- mode call_gen__rebuild_registers_2(in, in, out).

call_gen__rebuild_registers_2([]) --> [].
call_gen__rebuild_registers_2([Var - arg_info(ArgLoc, Mode)|Args]) -->
	(
		{ Mode = top_out }
	->
		{ code_util__arg_loc_to_register(ArgLoc, Register) },
		code_info__set_variable_register(Var, Register)
	;
		{ true }
	),
	call_gen__rebuild_registers_2(Args).

%---------------------------------------------------------------------------%

call_gen__generate_det_builtin(PredId, _ProcId, Args, empty) -->
	{ predicate_name(PredId, OpStr) },
	(
		{ atom_to_operator(OpStr, Op) },
		{ Args = [ X, Y, Var ] }
	->
		code_info__cashe_expression(Var, binop(Op, var(X), var(Y)))
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_builtin(PredId, _ProcId, Args, Code) -->
	{ predicate_name(PredId, OpStr) },
	(
		{ atom_to_operator(OpStr, Op) },
		{ Args = [ X, Y ] }
	->
		code_info__flush_variable(X, CodeX),
		code_info__get_variable_register(X, XLval),
		code_info__flush_variable(Y, CodeY),
		code_info__get_variable_register(Y, YLval),
		code_info__get_fall_through(FallThrough),
		{ CodeT = node([
			if_not_val(binop(Op, lval(XLval), lval(YLval)),
								FallThrough) -
				"Perform test and fall though on failure"
		]) },
		{ Code = tree(tree(node(CodeX),node(CodeY)), CodeT) }
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
