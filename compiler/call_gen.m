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

:- import_module tree, list, map, std_util, require, bintree_set, int.
:- import_module prog_io, arg_info, type_util, mode_util, unify_proc.
:- import_module set, shapes.

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
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel, yes),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_args(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments, OutLiveVals),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Address) },
	code_info__get_pred_id(CallerPredId),
	code_info__get_proc_id(CallerProcId),
	{ code_util__make_entry_label(ModuleInfo, CallerPredId, CallerProcId, 
		CallerAddress) },
	{ CodeC1 = node([
		call(Address, label(ReturnLabel), CallerAddress, OutLiveVals) -
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
		call_gen__generate_nondet_call(PredId, ProcId, Arguments, Code)
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
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel, yes),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_args(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments, OutLiveVals),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Address) },
        code_info__get_pred_id(CallerPredId),
        code_info__get_proc_id(CallerProcId),
        { code_util__make_entry_label(ModuleInfo, CallerPredId, CallerProcId,
                CallerAddress) },
        { CodeC1 = node([
                call(Address, label(ReturnLabel), CallerAddress, OutLiveVals) -
			"branch to semidet procedure",
		label(ReturnLabel) - "Continuation label"
	]) },
	code_info__get_next_label(ContLab, no),
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
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel, yes),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_args(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments, OutLiveVals),
	{ code_util__make_entry_label(ModuleInfo, PredId, ModeId, Address) },
        code_info__get_pred_id(CallerPredId),
        code_info__get_proc_id(CallerProcId),
        { code_util__make_entry_label(ModuleInfo, CallerPredId, CallerProcId,
                CallerAddress) },
        { CodeC1 = node([
                call(Address, label(ReturnLabel), CallerAddress, OutLiveVals) -
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

:- pred call_gen__save_variables(set(var), code_tree,
						code_info, code_info).
:- mode call_gen__save_variables(in, out, in, out) is det.

call_gen__save_variables(Args, Code) -->
	code_info__get_live_variables(Variables0),
	{ set__list_to_set(Variables0, Vars0) },
	{ set__difference(Vars0, Args, Vars) },
	{ set__to_sorted_list(Vars, Variables) },
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
		code_info__set_var_location(Var, reg(Register))
	;
		{ true }
	),
	call_gen__rebuild_registers_2(Args).

%---------------------------------------------------------------------------%

call_gen__generate_det_builtin(PredId, ProcId, Args, Code) -->
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
			{ Args = [PredTerm|OtherArgs] },
			{ module_info_pred_proc_info(ModuleInfo,
				PredId, ProcId, _PredInfo, ProcInfo) },
			{ proc_info_arg_info(ProcInfo, ArgInfo0) },
			{ ArgInfo0 = [arg_info(_, top_in)|ArgInfo1] }
		->
			{ assoc_list__from_corresponding_lists(ArgInfo1,
						OtherArgs, Immediates) },
			{ call_gen__partition_args(Immediates, In, Out) },
			call_gen__generate_higher_call(deterministic,
				PredTerm, In, Out, Code)
		;
			{ error("call_gen__generate_non_builtin: invalid call") }
		)
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_builtin(PredId, ProcId, Args, Code) -->
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
			{ Args = [PredTerm|OtherArgs] },
			{ module_info_pred_proc_info(ModuleInfo,
				PredId, ProcId, _PredInfo, ProcInfo) },
			{ proc_info_arg_info(ProcInfo, ArgInfo0) },
			{ ArgInfo0 = [arg_info(_, top_in)|ArgInfo1] }
		->
			{ assoc_list__from_corresponding_lists(ArgInfo1,
						OtherArgs, Immediates) },
			{ call_gen__partition_args(Immediates, In, Out) },
			call_gen__generate_higher_call(semideterministic,
				PredTerm, In, Out, Code)
		;
			{ error("call_gen__generate_non_builtin: invalid call") }
		)
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_builtin(PredId, ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ PredName = "call" }
	->
		(
			{ Args = [PredTerm|OtherArgs] },
			{ module_info_pred_proc_info(ModuleInfo,
				PredId, ProcId, _PredInfo, ProcInfo) },
			{ proc_info_arg_info(ProcInfo, ArgInfo0) },
			{ ArgInfo0 = [arg_info(_, top_in)|ArgInfo1] }
		->
			{ assoc_list__from_corresponding_lists(ArgInfo1,
						OtherArgs, Immediates) },
			{ call_gen__partition_args(Immediates, In, Out) },
			call_gen__generate_higher_call(nondeterministic,
				PredTerm, In, Out, Code)
		;
			{ error("call_gen__generate_non_builtin: invalid call") }
		)
	;
		{ error("Unknown nondet builtin predicate") }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred call_gen__partition_args(assoc_list(arg_info, var),
						list(var), list(var)).
:- mode call_gen__partition_args(in, out, out) is det.

call_gen__partition_args([], [], []).
call_gen__partition_args([arg_info(_Loc,Mode) - V|Rest], Ins, Outs) :-
	(
		Mode = top_in
	->
		call_gen__partition_args(Rest, Ins0, Outs),
		Ins = [V|Ins0]
	;
		Mode = top_out
	->
		call_gen__partition_args_2(Rest, Outs0),
		Ins = [],
		Outs = [V|Outs0]
	;
		error("call_gen__partition_args: top_unused")
	).
		
:- pred call_gen__partition_args_2(assoc_list(arg_info, var), list(var)).
:- mode call_gen__partition_args_2(in, out) is det.

call_gen__partition_args_2([], []).
call_gen__partition_args_2([arg_info(_Loc,Mode) - V|Rest], Outs) :-
	(
		Mode = top_in
	->
		error("call_gen__partition_args_2: input argument occurs after an output argument")
	;
		call_gen__partition_args_2(Rest, Outs0),
		Outs = [V|Outs0]
	).

%---------------------------------------------------------------------------%

call_gen__generate_complicated_unify(Var1, Var2, UniMode, Det, Code) -->
	{ arg_info__unify_arg_info(Det, ArgInfo) },
	{ Arguments = [Var1, Var2] },
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel, yes),
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
		call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
		{ call_gen__output_args(Args, OutputArguments) },
		call_gen__generate_return_livevals(OutArgs, OutputArguments, 
						OutLiveVals),
		{ code_util__make_uni_label(ModuleInfo, VarTypeId, ModeNum,
			UniLabel) },
		{ ModeNum = 0 ->
			Address = imported(UniLabel)
		;
			Address = label(local(UniLabel))
		},
		code_info__get_pred_id(CallerPredId),
		code_info__get_proc_id(CallerProcId),
		{ code_util__make_entry_label(ModuleInfo, CallerPredId, 
			CallerProcId, CallerAddress) },
		{ CodeC1 = node([
			call(Address, label(ReturnLabel), CallerAddress,
				 OutLiveVals) -
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
	"fatal_error(""Sorry, polymorphic unifications not implemented"");") -
				"Temporary hack"
			
		]) }
	),
	(
		{ Det = semideterministic }
	->
		code_info__get_next_label(ContLab, no),
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

:- pred call_gen__select_out_args(assoc_list(var, arg_info), set(var)).
:- mode call_gen__select_out_args(in, out) is det.

call_gen__select_out_args([], Out) :-
	set__init(Out).
call_gen__select_out_args([V - arg_info(_Loc, Mode)|Rest], Out) :-
	call_gen__select_out_args(Rest, Out0),
	(
		Mode = top_out
	->
		set__insert(Out0, V, Out)
	;
		Out = Out0
	).

%---------------------------------------------------------------------------%

:- pred call_gen__input_args(list(arg_info), list(arg_loc)).
:- mode call_gen__input_args(in, out) is det.

call_gen__input_args([], []).
call_gen__input_args([arg_info(Loc, Mode) | Args], Vs) :-
	(
		Mode = top_in
	->
		Vs = [Loc |Vs0]
	;
		Vs = Vs0
	),
	call_gen__input_args(Args, Vs0).

%---------------------------------------------------------------------------%

:- pred call_gen__output_args(list(pair(var, arg_info)), 
				list(pair(var, arg_loc))).
:- mode call_gen__output_args(in, out) is det.

call_gen__output_args([], []).
call_gen__output_args([Var - arg_info(Loc, Mode)|Args], Vs) :-
	(
		Mode = top_out
	->
		Vs = [Var - Loc|Vs0]
	;
		Vs = Vs0
	),
	call_gen__output_args(Args, Vs0).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_call_livevals(set(var), list(arg_loc), code_tree,
							code_info, code_info).
:- mode call_gen__generate_call_livevals(in, in, out, in, out) is det.

call_gen__generate_call_livevals(OutArgs, InputArgs, Code) -->
	code_info__generate_stack_livevals(OutArgs, LiveVals0),
	{ call_gen__insert_arg_livevals(InputArgs, LiveVals0, LiveVals) },
	{ Code = node([
		livevals(LiveVals) - ""
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

:- pred call_gen__generate_return_livevals(set(var), list(pair(var, arg_loc)),
					list(liveinfo), code_info, code_info).
:- mode call_gen__generate_return_livevals(in, in, out, in, out) is det.

call_gen__generate_return_livevals(OutArgs, OutputArgs, LiveVals, Code0, Code) :-
	code_info__generate_stack_livelvals(OutArgs, LiveVals0, Code0, Code1),
	code_info__get_module_info(Module, Code1, Code2),
	module_info_shapes(Module, S_Tab0),
	call_gen__insert_arg_livelvals(OutputArgs, Module,
					LiveVals0, LiveVals, Code2, Code3,
					S_Tab0, S_Tab),
	module_info_set_shapes(Module, S_Tab, Module1),
	code_info__set_module_info(Module1, Code3, Code).

% Maybe a varlist to type_id list would be a better way to do this...

%---------------------------------------------------------------------------%

:- pred call_gen__insert_arg_livelvals(list(pair(var, arg_loc)),
					module_info, list(liveinfo),
					list(liveinfo), code_info, code_info,
					shape_table, shape_table).
:- mode call_gen__insert_arg_livelvals(in, in, in, out, in, out,
					in, out) is det.

call_gen__insert_arg_livelvals([], _, LiveVals, LiveVals, C, C, S, S).
call_gen__insert_arg_livelvals([Var - L|As], Module_Info, LiveVals0, LiveVals,
				 	Code0, Code, S_Tab0, S_Tab) :-
	code_util__arg_loc_to_register(L, R),
	code_info__variable_type(Var, Type, Code0, Code1),
	module_info_types(Module_Info, Type_Table),
	shapes__request_shape_number(Type - ground, Type_Table,
			S_Tab0, S_Tab1, S_Number),
	LiveVal = live_lvalue(reg(R), S_Number),
	call_gen__insert_arg_livelvals(As, Module_Info, 
			[LiveVal|LiveVals0], LiveVals, Code1,
			 Code, S_Tab1, S_Tab).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_higher_call(category, var, list(var), list(var), code_tree,
						code_info, code_info).
:- mode call_gen__generate_higher_call(in, in, in, in, out, in, out) is det.

call_gen__generate_higher_call(PredDet, Var, InVars, OutVars, Code) -->
	code_info__set_succip_used(yes),
	{ set__list_to_set(OutVars, OutArgs) },
	call_gen__save_variables(OutArgs, SaveCode),
		% place the immediate input arguments in registers
		% starting at r4.
	call_gen__generate_immediate_args(InVars, InVars, 4, ImmediateCode),
	code_info__generate_stack_livevals(OutArgs, LiveVals0),
	{ bintree_set__insert(LiveVals0, reg(r(1)), LiveVals) },
	call_gen__generate_return_livevals(OutArgs, [], OutLiveVals),
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
	{ list__length(InVars, NInVars) },
	{ list__length(OutVars, NOutVars) },
	{ SetupCode = tree(CopyCode, node([
			assign(reg(r(2)), const(int_const(NInVars))) -
				"Assign number of immediate input arguments",
			assign(reg(r(3)), const(int_const(NOutVars))) -
				"Assign number of output arguments"
		])
	) },
	code_info__get_next_label(ReturnLabel, yes),
	(
		{ PredDet = deterministic },
		{ CallCode = node([
			livevals(LiveVals) - "",
			call_closure(no, label(ReturnLabel), OutLiveVals) -
				"setup and call det higher order pred",
			label(ReturnLabel) - "Continuation label"
		]) }
	;
		{ PredDet = semideterministic },
		{ TryCallCode = node([
			livevals(LiveVals) - "",
			call_closure(yes, label(ReturnLabel), OutLiveVals) -
				"setup and call semidet higher order pred",
			label(ReturnLabel) - "Continuation label"
		]) },
		code_info__generate_failure(FailCode),
		code_info__get_next_label(ContLab, no),
		{ CheckReturnCode = tree(node([
			if_val(lval(reg(r(1))), label(ContLab)) -
				"Test for success"
			]), tree(FailCode, node([ label(ContLab) - "" ]))) },
		{ CallCode = tree(TryCallCode, CheckReturnCode) }
	;
		{ PredDet = nondeterministic },
		{ CallCode = node([
			livevals(LiveVals) - "",
			call_closure(no, label(ReturnLabel),
						OutLiveVals)
				- "setup and call nondet higher order pred",
			label(ReturnLabel) - "Continuation label"
		]) }
	),
	{ Code = tree(tree(SaveCode, tree(ImmediateCode, VarCode)),
		tree(SetupCode, CallCode)) },
	(
		{ PredDet = semideterministic }
	->
		{ FirstArg = 2 }
	;
		{ FirstArg = 1 }
	),
	{ call_gen__outvars_to_outargs(OutVars, FirstArg, OutArguments) },
	call_gen__rebuild_registers(OutArguments).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_immediate_args(list(var), list(var), int, code_tree,
							code_info, code_info).
:- mode call_gen__generate_immediate_args(in, in, in, out, in, out) is det.

call_gen__generate_immediate_args([], _Args, _N, empty) --> [].
call_gen__generate_immediate_args([V|Vs], Args, N0, Code) -->
	code_info__place_var(V, reg(r(N0)), Code0),
	{ N1 is N0 + 1 },
	call_gen__generate_immediate_args(Vs, Args, N1, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred call_gen__outvars_to_outargs(list(var), int, assoc_list(var,arg_info)).
:- mode call_gen__outvars_to_outargs(in, in, out) is det.

call_gen__outvars_to_outargs([], _N, []).
call_gen__outvars_to_outargs([V|Vs], N0, [V - Arg|ArgInfos]) :-
	Arg = arg_info(N0, top_out),
	N1 is N0 + 1,
	call_gen__outvars_to_outargs(Vs, N1, ArgInfos).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
