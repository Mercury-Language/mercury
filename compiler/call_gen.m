%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% file: call_gen.nl
%
% main author: conway.
%
% This module provides predicates for generating procedure calls.
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

:- pred call_gen__generate_complicated_unify(var, var, uni_mode, category,
					code_tree, code_info, code_info).
:- mode call_gen__generate_complicated_unify(in, in, in, in, out, in, out)
	is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require, bintree_set.
:- import_module prog_io, arg_info, type_util, mode_util, unify_proc.

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
	code_info__setup_call(Args, Arguments, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(InputArguments, CodeC0),
	{ call_gen__output_args(ArgInfo, OutputArguments) },
	call_gen__generate_return_livevals(OutputArguments, OutLiveVals),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Address) },
	{ CodeC1 = node([
		call(Address, label(ReturnLabel), OutLiveVals) - % XXX
					"branch to det procedure",
		label(ReturnLabel) - "Continuation label"
	]) },
	{ Code = tree(CodeA, tree(CodeB, tree(CodeC0, CodeC1))) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

	% Check whether the call was really deterministic or whether
	% it's a nondet call for which we are pruning all solutions except the
	% first.

call_gen__generate_semidet_call(PredId, ProcId, Arguments, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_interface_determinism(ProcInfo, Determinism) },
	( { Determinism = semideterministic } ->
		call_gen__generate_semidet_call_2(PredId, ProcId, Arguments,
			Code)
	;
		code_info__generate_pre_commit(PreCommit, FailLabel),
		call_gen__generate_nondet_call(PredId, ProcId, Arguments,
			CallCode),
		code_info__generate_commit(FailLabel, Commit),
		{ Code = tree(PreCommit, tree(CallCode, Commit)) }
	).

:- pred call_gen__generate_semidet_call_2(pred_id, proc_id, list(var),
					code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_call_2(in, in, in, out, in, out) is det.

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
	% we branch to the appropriate continuation as generated by
	% code_info__generate_failure.

call_gen__generate_semidet_call_2(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	call_gen__save_variables(CodeA),
	code_info__clear_reserved_registers,
	code_info__setup_call(Args, Arguments, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(InputArguments, CodeC0),
	{ call_gen__output_args(ArgInfo, OutputArguments) },
	call_gen__generate_return_livevals(OutputArguments, OutLiveVals),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Address) },
	{ CodeC1 = node([
		call(Address, label(ReturnLabel), OutLiveVals) - % XXX
			"branch to semidet procedure",
		label(ReturnLabel) - "Continuation label"
	]) },
	code_info__get_next_label(ContLab),
	code_info__generate_failure(FailCode),
	{ CodeD = tree(node([
		if_val(lval(reg(r(1))), label(ContLab)) -
			"Test for success"
		]), tree(FailCode, node([ label(ContLab) - "" ]))) },

	{ Code = tree(CodeA, tree(CodeB, tree(tree(CodeC0, CodeC1), CodeD))) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	call_gen__save_variables(CodeA),
	code_info__clear_reserved_registers,
	code_info__setup_call(Args, Arguments, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(InputArguments, CodeC0),
	{ call_gen__output_args(ArgInfo, OutputArguments) },
	call_gen__generate_return_livevals(OutputArguments, OutLiveVals),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Address) },
	{ CodeC1 = node([
		call(Address, label(ReturnLabel), OutLiveVals) - % XXX
			"branch to nondet procedure",
		label(ReturnLabel) - "Continuation label"
	]) },
	{ Code = tree(CodeA, tree(CodeB, tree(CodeC0, CodeC1))) },
	call_gen__rebuild_registers(Args),
		% the nondet procedure may have created choice points,
		% so we must set the current failure continuation to `unknown'
		% which means `on failure, just do a redo()'.
	code_info__pop_failure_cont,
	code_info__push_failure_cont(unknown).

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
		code_info__add_lvalue_to_variable(reg(Register), Var),
		code_info__add_variable_to_register(Var, Register)
	;
		{ true }
	),
	call_gen__rebuild_registers_2(Args).

%---------------------------------------------------------------------------%

call_gen__generate_det_builtin(PredId, _ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ code_util__builtin_binop(PredName, 3, BinOp) },
		{ Args = [ X, Y, Var ] }
	->
		code_info__cache_expression(Var, binop(BinOp, var(X), var(Y))),
		{ Code = empty }
	;
		{ code_util__builtin_unop(PredName, 2, UnOp) },
		{ Args = [ X, Var ] }
	->
		code_info__cache_expression(Var, unop(UnOp, var(X))),
		{ Code = empty }
	;
		{ PredName = "call" }
	->
		(
			{ Args = [PredTerm] }
		->
			call_gen__generate_higher_call(deterministic,
								PredTerm, Code)
		;
			{ error("call_gen__generate_det_builtin: call/N, N > 1, unimplemented") }
		)
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_builtin(PredId, _ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ code_util__builtin_binop(PredName, 2, BinOp) },
		{ Args = [ X, Y ] }
	->
		code_info__produce_variable(X, CodeX, XRval),
		code_info__produce_variable(Y, CodeY, YRval),
		code_info__generate_test_and_fail(
			binop(BinOp, XRval, YRval), TestCode),
		{ Code = tree(tree(CodeX,CodeY), TestCode) }
	;
		{ code_util__builtin_unop(PredName, 1, UnOp) },
		{ Args = [ X ] }
	->
		code_info__produce_variable(X, CodeX, XRval),
		code_info__generate_test_and_fail(
			unop(UnOp, XRval), TestCode),
		{ Code = tree(CodeX, TestCode) }
	;
		{ PredName = "call" }
	->
		(
			{ Args = [PredTerm] }
		->
			call_gen__generate_higher_call(semideterministic,
								PredTerm, Code)
		;
			{ error("call_gen__generate_semi_builtin: call/N, N > 1, unimplemented") }
		)
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_builtin(PredId, _ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ PredName = "call" }
	->
		(
			{ Args = [PredTerm] }
		->
			call_gen__generate_higher_call(nondeterministic,
								PredTerm, Code)
		;
			{ error("call_gen__generate_non_builtin: call/N, N > 1, unimplemented") }
		)
	;
		{ error("Unknown nondet builtin predicate") }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

call_gen__generate_complicated_unify(Var1, Var2, UniMode, Det, Code) -->
	{ arg_info__unify_arg_info(Det, ArgInfo) },
	{ Arguments = [Var1, Var2] },
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	call_gen__save_variables(CodeA),
	code_info__clear_reserved_registers,
	code_info__setup_call(Args, Arguments, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	code_info__variable_type(Var1, VarType),
	( { type_to_type_id(VarType, VarTypeId, _) } ->
		% We handle (in, in) unifications specially - they
		% are always mode zero, and the procedure is global
		% rather than local
		(
			{ UniMode = (XInitial - YInitial -> _Final) },
			{ inst_is_ground(ModuleInfo, XInitial) },
			{ inst_is_ground(ModuleInfo, YInitial) }
		->
			{ ModeNum = 0 }
		;
			code_info__get_requests(Requests),
			{ unify_proc__lookup_num(Requests, VarTypeId, UniMode,
				ModeNum) }
		),
		{ call_gen__input_args(ArgInfo, InputArguments) },
		call_gen__generate_call_livevals(InputArguments, CodeC0),
		{ call_gen__output_args(ArgInfo, OutputArguments) },
		call_gen__generate_return_livevals(OutputArguments, OutLiveVals),
		{ code_util__make_uni_label(ModuleInfo, VarTypeId, ModeNum,
			UniLabel) },
		{ ModeNum = 0 ->
			Address = imported(UniLabel)
		;
			Address = label(local(UniLabel))
		},
		{ CodeC1 = node([
			call(Address, label(ReturnLabel), OutLiveVals) - % XXX
				"branch to out-of-line unification procedure",
			label(ReturnLabel) - "Continuation label"
		]) }
	;
		% type_to_type_id failed - the type must be a type variable
		% { error("sorry, polymorphic unifications not implemented") }
		% XXX a temporary hack
		{ CodeC0 = empty },
		{ CodeC1 = node([
			c_code(
	"fatal_error(\"Sorry, polymorphic unifications not implemented\");") -
				"Temporary hack"
			
		]) }
	),
	(
		{ Det = semideterministic }
	->
		code_info__get_next_label(ContLab),
		code_info__generate_failure(FailCode),
		{ CodeD = tree(node([
			if_val(lval(reg(r(1))), label(ContLab)) -
				"Test for success"
			]), tree(FailCode, node([ label(ContLab) - "" ]))) }
	;
		{ CodeD = empty }
	),

	{ Code = tree(CodeA, tree(CodeB, tree(tree(CodeC0, CodeC1), CodeD))) },
	call_gen__rebuild_registers(Args).

%---------------------------------------------------------------------------%

:- pred call_gen__input_args(list(arg_info), list(arg_loc)).
:- mode call_gen__input_args(in, out) is det.

call_gen__input_args([], []).
call_gen__input_args([arg_info(Loc, Mode)|Args], Vs) :-
	(
		Mode = top_in
	->
		Vs = [Loc|Vs0]
	;
		Vs = Vs0
	),
	call_gen__input_args(Args, Vs0).

%---------------------------------------------------------------------------%

:- pred call_gen__output_args(list(arg_info), list(arg_loc)).
:- mode call_gen__output_args(in, out) is det.

call_gen__output_args([], []).
call_gen__output_args([arg_info(Loc, Mode)|Args], Vs) :-
	(
		Mode = top_out
	->
		Vs = [Loc|Vs0]
	;
		Vs = Vs0
	),
	call_gen__output_args(Args, Vs0).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_call_livevals(list(arg_loc), code_tree,
						code_info, code_info).
:- mode call_gen__generate_call_livevals(in, out, in, out) is det.

call_gen__generate_call_livevals(InputArgs, Code) -->
	code_info__generate_stack_livevals(LiveVals0),
	{ call_gen__insert_arg_livevals(InputArgs, LiveVals0, LiveVals) },
	{ Code = node([
		livevals(yes, LiveVals) - ""
	]) }.

%---------------------------------------------------------------------------%

:- pred call_gen__insert_arg_livevals(list(arg_loc),
					bintree_set(lval), bintree_set(lval)).
:- mode call_gen__insert_arg_livevals(in, in, out) is det.

call_gen__insert_arg_livevals([], LiveVals, LiveVals).
call_gen__insert_arg_livevals([L|As], LiveVals0, LiveVals) :-
	code_util__arg_loc_to_register(L, R),
	bintree_set__insert(LiveVals0, reg(R), LiveVals1),
	call_gen__insert_arg_livevals(As, LiveVals1, LiveVals).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_return_livevals(list(arg_loc), list(liveinfo),
						code_info, code_info).
:- mode call_gen__generate_return_livevals(in, out, in, out) is det.

call_gen__generate_return_livevals(OutputArgs, LiveVals) -->
	code_info__generate_stack_livelvals(LiveVals0),
	{ call_gen__insert_arg_livelvals(OutputArgs, LiveVals0, LiveVals) }.

%---------------------------------------------------------------------------%

:- pred call_gen__insert_arg_livelvals(list(arg_loc),
					list(liveinfo), list(liveinfo)).
:- mode call_gen__insert_arg_livelvals(in, in, out) is det.

call_gen__insert_arg_livelvals([], LiveVals, LiveVals).
call_gen__insert_arg_livelvals([L|As], LiveVals0, LiveVals) :-
	code_util__arg_loc_to_register(L, R),
	LiveVal = live_lvalue(reg(R), -1), % XXX get shape number
	call_gen__insert_arg_livelvals(As, [LiveVal|LiveVals0], LiveVals).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_higher_call(category, var, code_tree,
						code_info, code_info).
:- mode call_gen__generate_higher_call(in, in, out, in, out) is det.

call_gen__generate_higher_call(Det, Var, Code) -->
	call_gen__save_variables(SaveCode),
	code_info__clear_reserved_registers,
	code_info__produce_variable(Var, VarCode, RVal),
	(
		{ RVal = lval(reg(r(1))) }
	->
		{ CopyCode = empty }
	;
		{ CopyCode = node([
			assign(reg(r(1)), RVal) - "Copy pred-term"
		])}
	),
	code_info__get_next_label(ReturnLabel),
	(
		{ Det = deterministic },
		{ CallCode = node([
			call_closure(no, label(ReturnLabel)) - "setup and call higher order pred",
			label(ReturnLabel) - "Continuation label"
		]) }
	;
		{ Det = semideterministic },
		{ CallCode = node([
			call_closure(yes, label(ReturnLabel)) - "setup and call higher order pred",
			label(ReturnLabel) - "Continuation label"
		]) }
	;
		{ Det = nondeterministic },
		{ CallCode = node([
			call_closure(no, label(ReturnLabel)) - "setup and call higher order pred",
			label(ReturnLabel) - "Continuation label"
		]) }
	),
	(
		{ Det = deterministic },
		{ ReturnCode = empty }
	;
		{ Det = semideterministic },
		code_info__generate_failure(FailCode),
		code_info__get_next_label(ContLab),
		{ ReturnCode = tree(node([
			if_val(lval(reg(r(1))), label(ContLab)) -
				"Test for success"
			]), tree(FailCode, node([ label(ContLab) - "" ]))) }
	;
		{ Det = nondeterministic },
		{ ReturnCode = empty }
		% XXX we don't handle commits across nondet calls
	),
	{ Code = tree(tree(SaveCode, VarCode),
		tree(CopyCode, tree(CallCode, ReturnCode))) },
	call_gen__rebuild_registers([]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
