%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: call_gen.m
%
% main author: conway.
%
% This module provides predicates for generating procedure calls,
% including calls to higher-order pred variables.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module call_gen.

:- interface.

:- import_module hlds_pred, hlds_goal, llds, code_info, code_util.

:- pred call_gen__generate_higher_order_call(code_model, var, list(var),
				list(type), list(mode), determinism,
				code_tree, code_info, code_info).
:- mode call_gen__generate_higher_order_call(in, in, in, in, in, in, out,
				in, out) is det.

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

:- pred call_gen__generate_complicated_unify(var, var, uni_mode, can_fail,
					code_tree, code_info, code_info).
:- mode call_gen__generate_complicated_unify(in, in, in, in, out, in, out)
	is det.

:- pred call_gen__input_arg_locs(list(pair(var, arg_info)), 
				list(pair(var, arg_loc))).
:- mode call_gen__input_arg_locs(in, out) is det.

:- pred call_gen__output_arg_locs(list(pair(var, arg_info)), 
				list(pair(var, arg_loc))).
:- mode call_gen__output_arg_locs(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_data, prog_data, globals.
:- import_module arg_info, type_util, mode_util, shapes, unify_proc.
:- import_module bool, int, list, assoc_list, tree, set, map.
:- import_module std_util, require.

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
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_arg_locs(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments,
		OutLiveVals),
	code_info__make_entry_label(ModuleInfo, PredId, ModeId, Address),
	{ CodeC1 = node([
		call(Address, label(ReturnLabel), OutLiveVals, det)
			- "branch to det procedure",
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
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
	( { CodeModel = model_semi } ->
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
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_arg_locs(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments,	
		OutLiveVals),
	code_info__make_entry_label(ModuleInfo, PredId, ModeId, Address),
        { CodeC1 = node([
                call(Address, label(ReturnLabel), OutLiveVals, semidet)
			- "branch to semidet procedure",
		label(ReturnLabel) - "Continuation label"
	]) },
	call_gen__rebuild_registers(Args),
	code_info__generate_failure(FailCode),
	code_info__get_next_label(ContLab),
	{ CodeD = tree(node([
		if_val(lval(reg(r(1))), label(ContLab)) -
			"Test for success"
		]), tree(FailCode, node([ label(ContLab) - "" ]))) },

	{ Code = tree(CodeA, tree(CodeB, tree(tree(CodeC0, CodeC1), CodeD))) }.

%---------------------------------------------------------------------------%

call_gen__generate_nondet_call(PredId, ModeId, Arguments, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_arg_locs(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments,
		OutLiveVals),
	code_info__make_entry_label(ModuleInfo, PredId, ModeId, Address),
	code_info__failure_cont(failure_cont(IsKnown, _, FailureMap)),
	(
		{ IsKnown = known(_) },
		{ FailureMap = [_ - do_fail | _] }
	->
		{ TailCallable = nondet(yes) }
	;
		{ TailCallable = nondet(no) }
	),
        { CodeC1 = node([
                call(Address, label(ReturnLabel), OutLiveVals, TailCallable)
			- "branch to nondet procedure",
		label(ReturnLabel) - "Continuation label"
	]) },
	{ Code = tree(CodeA, tree(CodeB, tree(CodeC0, CodeC1))) },
	call_gen__rebuild_registers(Args).

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
call_gen__save_variables_2([Var | Vars], Code) -->
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
call_gen__rebuild_registers_2([Var - arg_info(ArgLoc, Mode) | Args]) -->
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

call_gen__generate_det_builtin(PredId, _ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ code_util__builtin_binop(ModuleName, PredName, 3, BinOp) },
		{ Args = [ X, Y, Var ] }
	->
		code_info__cache_expression(Var, binop(BinOp, var(X), var(Y))),
		{ Code = empty }
	;
		{ code_util__builtin_unop(ModuleName, PredName, 2, UnOp) },
		{ Args = [ X, Var ] }
	->
		code_info__cache_expression(Var, unop(UnOp, var(X))),
		{ Code = empty }
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_builtin(PredId, _ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ code_util__builtin_binop(ModuleName, PredName, 2, BinOp) },
		{ Args = [ X, Y ] }
	->
		code_info__produce_variable(X, CodeX, XRval),
		code_info__produce_variable(Y, CodeY, YRval),
		code_info__generate_test_and_fail(
			binop(BinOp, XRval, YRval), TestCode),
		{ Code = tree(tree(CodeX,CodeY), TestCode) }
	;
		{ code_util__builtin_unop(ModuleName, PredName, 1, UnOp) },
		{ Args = [ X ] }
	->
		code_info__produce_variable(X, CodeX, XRval),
		code_info__generate_test_and_fail(
			unop(UnOp, XRval), TestCode),
		{ Code = tree(CodeX, TestCode) }
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_builtin(_PredId, _ProcId, _Args, _Code) -->
	% there aren't any nondet builtins
	{ error("Unknown nondet builtin predicate") }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred call_gen__partition_args(assoc_list(arg_info, var),
						list(var), list(var)).
:- mode call_gen__partition_args(in, out, out) is det.

call_gen__partition_args([], [], []).
call_gen__partition_args([arg_info(_Loc,Mode) - V | Rest], Ins, Outs) :-
	(
		Mode = top_in
	->
		call_gen__partition_args(Rest, Ins0, Outs),
		Ins = [V | Ins0]
	;
		call_gen__partition_args(Rest, Ins, Outs0),
		Outs = [V | Outs0]
	).

%---------------------------------------------------------------------------%

call_gen__generate_complicated_unify(Var1, Var2, UniMode, CanFail, Code) -->
	{ determinism_components(Det, CanFail, at_most_one) },
	{ determinism_to_code_model(Det, CodeModel) },
	code_info__get_globals(Globals),
	{ globals__get_args_method(Globals, ArgsMethod) },
	{ arg_info__unify_arg_info(ArgsMethod, CodeModel, ArgInfo) },
	{ Arguments = [Var1, Var2] },
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	code_info__variable_type(Var1, VarType),
	( { type_to_type_id(VarType, VarTypeId, _) } ->
		{ unify_proc__lookup_mode_num(ModuleInfo, VarTypeId, UniMode,
				Det, ModeNum) },
		{ call_gen__input_args(ArgInfo, InputArguments) },
		call_gen__generate_call_livevals(OutArgs, InputArguments,
			CodeC0),
		{ call_gen__output_arg_locs(Args, OutputArguments) },
		call_gen__generate_return_livevals(OutArgs, OutputArguments, 
						OutLiveVals),
		{ code_util__make_uni_label(ModuleInfo, VarTypeId, ModeNum,
			UniLabel) },
		{ Address = imported(UniLabel) },
	/************
		% Currently we just conservatively assume the address
		% of a unification predicate is imported.  For
		% non-standard modes, we could do better, if
		% procs_per_c_function is zero (meaning infinity),
		% or if it is a recursive call.
		% But the code below doesn't work if procs_per_c_function
		% is non-zero and it's not a recursive call.
		{ ModeNum = 0 ->
			Address = imported(UniLabel)
		;
			Address = label(local(UniLabel))
		},
	**************/
		(
			{ CanFail = can_fail }
		->
			{ CallModel = semidet }
		;
			{ CallModel = det }
		),
		{ CodeC1 = node([
			call(Address, label(ReturnLabel),
				OutLiveVals, CallModel)
				- "branch to out-of-line unification procedure",
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
		{ CanFail = can_fail }
	->
		code_info__get_next_label(ContLab),
		call_gen__rebuild_registers(Args),
		code_info__generate_failure(FailCode),
		{ CodeD = tree(node([
			if_val(lval(reg(r(1))), label(ContLab)) -
				"Test for success"
			]), tree(FailCode, node([ label(ContLab) - "" ]))) }
	;
		call_gen__rebuild_registers(Args),
		{ CodeD = empty }
	),

	{ Code = tree(CodeA, tree(CodeB, tree(tree(CodeC0, CodeC1), CodeD))) }.

%---------------------------------------------------------------------------%

:- pred call_gen__select_out_args(assoc_list(var, arg_info), set(var)).
:- mode call_gen__select_out_args(in, out) is det.

call_gen__select_out_args([], Out) :-
	set__init(Out).
call_gen__select_out_args([V - arg_info(_Loc, Mode) | Rest], Out) :-
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

call_gen__input_arg_locs([], []).
call_gen__input_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
	(
		Mode = top_in
	->
		Vs = [Var - Loc | Vs0]
	;
		Vs = Vs0
	),
	call_gen__input_arg_locs(Args, Vs0).

call_gen__output_arg_locs([], []).
call_gen__output_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
	(
		Mode = top_out
	->
		Vs = [Var - Loc | Vs0]
	;
		Vs = Vs0
	),
	call_gen__output_arg_locs(Args, Vs0).

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
					set(lval), set(lval)).
:- mode call_gen__insert_arg_livevals(in, in, out) is det.

call_gen__insert_arg_livevals([], LiveVals, LiveVals).
call_gen__insert_arg_livevals([L | As], LiveVals0, LiveVals) :-
	code_util__arg_loc_to_register(L, R),
	set__insert(LiveVals0, reg(R), LiveVals1),
	call_gen__insert_arg_livevals(As, LiveVals1, LiveVals).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_return_livevals(set(var), list(pair(var, arg_loc)),
					list(liveinfo), code_info, code_info).
:- mode call_gen__generate_return_livevals(in, in, out, in, out) is det.

call_gen__generate_return_livevals(OutArgs, OutputArgs, LiveVals, Code0, Code)
		:-
	code_info__generate_stack_livelvals(OutArgs, LiveVals0, Code0, Code1),
	code_info__get_module_info(Module, Code1, Code2),
	code_info__get_shapes(S_Tab0, Code2, Code3),
	call_gen__insert_arg_livelvals(OutputArgs, Module,
		LiveVals0, LiveVals, Code3, Code4, S_Tab0, S_Tab),
	code_info__set_shapes(S_Tab, Code4, Code).

% Maybe a varlist to type_id list would be a better way to do this...

%---------------------------------------------------------------------------%

:- pred call_gen__insert_arg_livelvals(list(pair(var, arg_loc)),
					module_info, list(liveinfo),
					list(liveinfo), code_info, code_info,
					shape_table, shape_table).
:- mode call_gen__insert_arg_livelvals(in, in, in, out, in, out,
					in, out) is det.

call_gen__insert_arg_livelvals([], _, LiveVals, LiveVals, C, C, S, S).
call_gen__insert_arg_livelvals([Var - L | As], Module_Info, LiveVals0, LiveVals,
				 	Code0, Code, S_Tab0, S_Tab) :-
	code_util__arg_loc_to_register(L, R),
	code_info__variable_type(Var, Type, Code0, Code1),
	module_info_types(Module_Info, Type_Table),
		% XXX what does this do?  Is `ground(shared, no)' right???
	shapes__request_shape_number(Type - ground(shared, no), Type_Table,
			S_Tab0, S_Tab1, S_Number),
	LiveVal = live_lvalue(reg(R), num(S_Number)),
	call_gen__insert_arg_livelvals(As, Module_Info, 
			[LiveVal | LiveVals0], LiveVals, Code1,
			 Code, S_Tab1, S_Tab).

%---------------------------------------------------------------------------%

	%
	% for a higher-order call,
	% we split the arguments into inputs and outputs, put the inputs
	% in the locations expected by do_call_<detism>_closure in
	% runtime/call.mod, generate the call to do_call_<detism>_closure,
	% and pick up the outputs from the locations that we know
	% runtime/call.mod leaves them in.
	%
	% lambda.m transforms the generated lambda predicates to
	% make sure that all inputs come before all outputs, so that
	% runtime/call.mod doesn't have trouble figuring out which registers
	% the arguments go in.
	%

call_gen__generate_higher_order_call(_OuterCodeModel, PredVar, Args, Types,
		Modes, Det, Code) -->
	{ determinism_to_code_model(Det, InnerCodeModel) },
	code_info__get_globals(Globals),
	code_info__get_module_info(ModuleInfo),
	{ globals__get_args_method(Globals, ArgsMethod) },
	{ make_arg_infos(ArgsMethod, Types, Modes, InnerCodeModel, ModuleInfo,
		ArgInfo) },
	{ assoc_list__from_corresponding_lists(ArgInfo, Args, ArgsAndArgInfo) },
	{ call_gen__partition_args(ArgsAndArgInfo, InVars, OutVars) },
	call_gen__generate_higher_call(InnerCodeModel, PredVar, InVars, OutVars,
		Code).

:- pred call_gen__generate_higher_call(code_model, var, list(var), list(var),
				code_tree, code_info, code_info).
:- mode call_gen__generate_higher_call(in, in, in, in, out, in, out) is det.

call_gen__generate_higher_call(CodeModel, PredVar, InVars, OutVars, Code) -->
	code_info__set_succip_used(yes),
	{ set__list_to_set(OutVars, OutArgs) },
	call_gen__save_variables(OutArgs, SaveCode),
		% place the immediate input arguments in registers
		% starting at r4.
	call_gen__generate_immediate_args(InVars, 4, InLocs, ImmediateCode),
	code_info__generate_stack_livevals(OutArgs, LiveVals0),
	{ set__insert_list(LiveVals0,
		[reg(r(1)), reg(r(2)), reg(r(3)) | InLocs], LiveVals) },
	(
		{ CodeModel = model_semi }
	->
		{ FirstArg = 2 }
	;
		{ FirstArg = 1 }
	),
	{ call_gen__outvars_to_outargs(OutVars, FirstArg, OutArguments) },
	{ call_gen__output_arg_locs(OutArguments, OutLocs) },
	call_gen__generate_return_livevals(OutArgs, OutLocs, OutLiveVals),
	code_info__produce_variable(PredVar, PredVarCode, PredRVal),
	(
		{ PredRVal = lval(reg(r(1))) }
	->
		{ CopyCode = empty }
	;
		{ CopyCode = node([
			assign(reg(r(1)), PredRVal) - "Copy pred-term"
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
	code_info__get_next_label(ReturnLabel),
	(
		{ CodeModel = model_det },
		{ CallModel = det },
		{ RuntimeAddr = do_det_closure }
	;
		{ CodeModel = model_semi },
		{ CallModel = semidet },
		{ RuntimeAddr = do_semidet_closure }
	;
		{ CodeModel = model_non },
		code_info__failure_cont(failure_cont(IsKnown, _, FailureMap)),
		(
			{ IsKnown = known(_) },
			{ FailureMap = [_ - do_fail | _] }
		->
			{ TailCallable = yes }
		;
			{ TailCallable = no }
		),
		{ CallModel = nondet(TailCallable) },
		{ RuntimeAddr = do_nondet_closure }
	),
	{ TryCallCode = node([
		livevals(LiveVals) - "",
		call(RuntimeAddr, label(ReturnLabel), OutLiveVals, CallModel)
			- "setup and call higher order pred",
		label(ReturnLabel) - "Continuation label"
	]) },
	call_gen__rebuild_registers(OutArguments),
	(
		{ CodeModel = model_semi }
	->
		code_info__generate_failure(FailCode),
		code_info__get_next_label(ContLab),
		{ CheckReturnCode = tree(node([
			if_val(lval(reg(r(1))), label(ContLab)) -
				"Test for success"
			]), tree(FailCode, node([ label(ContLab) - "" ]))) },
		{ CallCode = tree(TryCallCode, CheckReturnCode) }
	;
		{ CallCode = TryCallCode }
	),
	{ Code = tree(tree(SaveCode, tree(ImmediateCode, PredVarCode)),
		tree(SetupCode, CallCode)) }.

%---------------------------------------------------------------------------%

:- pred call_gen__generate_immediate_args(list(var), int, list(lval), code_tree,
							code_info, code_info).
:- mode call_gen__generate_immediate_args(in, in, out, out, in, out) is det.

call_gen__generate_immediate_args([], _N, [], empty) --> [].
call_gen__generate_immediate_args([V | Vs], N0, [Lval | Lvals], Code) -->
	{ Lval = reg(r(N0)) },
	code_info__place_var(V, Lval, Code0),
	{ N1 is N0 + 1 },
	call_gen__generate_immediate_args(Vs, N1, Lvals, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%

:- pred call_gen__outvars_to_outargs(list(var), int, assoc_list(var,arg_info)).
:- mode call_gen__outvars_to_outargs(in, in, out) is det.

call_gen__outvars_to_outargs([], _N, []).
call_gen__outvars_to_outargs([V | Vs], N0, [V - Arg | ArgInfos]) :-
	Arg = arg_info(N0, top_out),
	N1 is N0 + 1,
	call_gen__outvars_to_outargs(Vs, N1, ArgInfos).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
