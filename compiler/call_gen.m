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

:- pred call_gen__generate_nondet_call(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_nondet_call(in, in, in, out, in, out) is det.

:- pred call_gen__generate_det_builtin(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_det_builtin(in, in, in, out, in, out) is det.

:- pred call_gen__generate_semidet_builtin(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_builtin(in, in, in, out, in, out) is det.

:- pred call_gen__generate_nondet_builtin(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_nondet_builtin(in, in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require.

	% To generate a call to a deterministic predicate, first
	% we get the arginfo for the callee.
	% We then save any live variables onto the stack, clear any
	% "reserved" registers (which get allocated as temporaries
	% during expression generation), and then setup the registers
	% for the procedure call. We then branch to the procedure
	% and rebuild the register information to reflect the state
	% when the callee returns.
call_gen__generate_det_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	call_gen__save_variables(CodeA),
	code_info__clear_reserved_registers,
	call_gen__setup_call(Args, Arguments, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Label) },
	(
		call_gen__is_imported(PredId)
	->
		{ CodeC = node([
			entrycall(Label, ReturnLabel) -
					"branch to non-local procedure",
			label(ReturnLabel) - "Continutation label"
		]) }
	;
		{ CodeC = node([
			call(Label, ReturnLabel) - "branch to procedure",
			label(ReturnLabel) - "Continutation label"
		]) }
	),
	{ Code = tree(CodeA, tree(CodeB, CodeC)) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

	% To generate a call to a semideterministic predicate, first
	% we get the arginfo for the callee.
	% We then save any live variables onto the stack, clear any
	% "reserved" registers (which get allocated as temporaries
	% during expression generation), and then setup the registers
	% for the procedure call. We then branch to the procedure
	% and rebuild the register information to reflect the state
	% when the callee returns.
	% On return we test the value in register r1 to see if the
	% callee succeeded or failed. In the event of failure
	% we branch to the fall-through for this procedure.
call_gen__generate_semidet_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	call_gen__save_variables(CodeA),
	code_info__clear_reserved_registers,
	call_gen__setup_call(Args, Arguments, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Label) },
	(
		call_gen__is_imported(PredId)
	->
		{ CodeC = node([
			entrycall(Label, ReturnLabel) - "branch to procedure",
			label(ReturnLabel) - "Continutation label"
		]) }
	;
		{ CodeC = node([
			call(Label, ReturnLabel) - "branch to procedure",
			label(ReturnLabel) - "Continutation label"
		]) }
	),
	code_info__get_next_label(ContLab),
	code_info__generate_failure(FailCode),
	{ CodeD = tree(node([
			if_val(lval(reg(r(1))), ContLab) - "Test for success"
		]), tree(FailCode, node([ label(ContLab) - "" ]))) },

	{ Code = tree(CodeA, tree(CodeB, tree(CodeC, CodeD))) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	call_gen__save_variables(CodeA),
	code_info__clear_reserved_registers,
	call_gen__setup_call(Args, Arguments, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Label) },
	(
		call_gen__is_imported(PredId)
	->
		{ CodeC = node([
			entrycall(Label, ReturnLabel) -
					"branch to non-local procedure",
			label(ReturnLabel) - "Continutation label"
		]) }
	;
		{ CodeC = node([
			call(Label, ReturnLabel) - "branch to procedure",
			label(ReturnLabel) - "Continutation label"
		]) }
	),
	{ Code = tree(CodeA, tree(CodeB, CodeC)) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

:- pred call_gen__setup_call(assoc_list(var, arg_info), list(var), code_tree,
							code_info, code_info).
:- mode call_gen__setup_call(in, in, out, in, out) is det.

call_gen__setup_call([], _Args, empty) --> [].
call_gen__setup_call([Var - arg_info(ArgLoc, Mode)|Vars], Args, Code) -->
	(
		{ Mode = top_in }
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
	call_gen__setup_call(Vars, Args2, CodeB),
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
	code_info__save_variable_on_stack(Var, CodeA),
        call_gen__save_variables_2(Vars, CodeB),
        { Code = tree(CodeA, CodeB) }.

%---------------------------------------------------------------------------%

:- pred call_gen__rebuild_registers(assoc_list(var, arg_info),
							code_info, code_info).
:- mode call_gen__rebuild_registers(in, in, out) is det.

call_gen__rebuild_registers(Args) -->
	code_info__clear_all_registers,
	call_gen__rebuild_registers_2(Args).

:- pred call_gen__rebuild_registers_2(assoc_list(var, arg_info),
							code_info, code_info).
:- mode call_gen__rebuild_registers_2(in, in, out) is det.

call_gen__rebuild_registers_2([]) --> [].
call_gen__rebuild_registers_2([Var - arg_info(ArgLoc, Mode)|Args]) -->
	(
		{ Mode = top_out }
	->
		{ code_util__arg_loc_to_register(ArgLoc, Register) },
		code_info__add_lvalue_to_variable(reg(Register), Var)
	;
		{ true }
	),
	call_gen__rebuild_registers_2(Args).

%---------------------------------------------------------------------------%

call_gen__generate_det_builtin(PredId, _ProcId, Args, empty) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_name(ModuleInfo, PredId, OpStr) },
	(
		{ atom_to_operator(OpStr, Op) },
		{ Args = [ X, Y, Var ] }
	->
		code_info__cache_expression(Var, binop(Op, var(X), var(Y)))
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_builtin(PredId, _ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_name(ModuleInfo, PredId, OpStr) },
	(
		{ atom_to_operator(OpStr, Op) },
		{ Args = [ X, Y ] }
	->
		code_info__flush_variable(X, CodeX),
		code_info__get_variable_register(X, XLval),
		code_info__flush_variable(Y, CodeY),
		code_info__get_variable_register(Y, YLval),
		code_info__get_failure_cont(FallThrough),
		{ CodeT = node([
			if_not_val(binop(Op, lval(XLval), lval(YLval)),
								FallThrough) -
				"Perform test and fall though on failure"
		]) },
		{ Code = tree(tree(CodeX,CodeY), CodeT) }
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_builtin(_PredId, _ProcId, _Args, _Code) -->
	{ error("Unknown builtin predicate") }.

%---------------------------------------------------------------------------%

:- pred call_gen__is_imported(pred_id, code_info, code_info).
:- mode call_gen__is_imported(in, in, out) is semidet.

call_gen__is_imported(PredId) -->
	code_info__get_module_info(Module),
	{ module_info_preds(Module, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_is_imported(PredInfo) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
