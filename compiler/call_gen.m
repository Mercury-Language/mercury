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

:- import_module hlds_pred, llds, code_info.
:- import_module term.

:- pred call_gen__generate_higher_order_call(code_model, var, list(var),
			list(type), list(mode), determinism, hlds_goal_info,
			code_tree, code_info, code_info).
:- mode call_gen__generate_higher_order_call(in, in, in, in, in, in, in, out,
				in, out) is det.

:- pred call_gen__generate_det_call(pred_id, proc_id, list(var), 
			hlds_goal_info, code_tree, code_info, code_info).
:- mode call_gen__generate_det_call(in, in, in, in, out, in, out) is det.

:- pred call_gen__generate_semidet_call(pred_id, proc_id, list(var),
			hlds_goal_info, code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_call(in, in, in, in, out, in, out) is det.

:- pred call_gen__generate_nondet_call(pred_id, proc_id, list(var),
			hlds_goal_info, code_tree, code_info, code_info).
:- mode call_gen__generate_nondet_call(in, in, in, in, out, in, out) is det.

:- pred call_gen__generate_det_builtin(pred_id, proc_id, list(var),
			code_tree, code_info, code_info).
:- mode call_gen__generate_det_builtin(in, in, in, out, in, out) is det.

:- pred call_gen__generate_semidet_builtin(pred_id, proc_id, list(var),
			code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_builtin(in, in, in, out, in, out) is det.

:- pred call_gen__generate_nondet_builtin(pred_id, proc_id, list(var),
			code_tree, code_info, code_info).
:- mode call_gen__generate_nondet_builtin(in, in, in, out, in, out) is
					erroneous.

/* DEAD CODE
:- pred call_gen__generate_complicated_unify(var, var, uni_mode, can_fail,
					code_tree, code_info, code_info).
:- mode call_gen__generate_complicated_unify(in, in, in, in, out, in, out)
	is det.
*/

:- pred call_gen__partition_args(assoc_list(var, arg_info),
						list(var), list(var)).
:- mode call_gen__partition_args(in, out, out) is det.

:- pred call_gen__input_arg_locs(list(pair(var, arg_info)), 
				list(pair(var, arg_loc))).
:- mode call_gen__input_arg_locs(in, out) is det.

:- pred call_gen__output_arg_locs(list(pair(var, arg_info)), 
				list(pair(var, arg_loc))).
:- mode call_gen__output_arg_locs(in, out) is det.

:- pred call_gen__save_variables(set(var), code_tree,
						code_info, code_info).
:- mode call_gen__save_variables(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_data, prog_data, code_util, globals.
:- import_module arg_info, type_util, mode_util, unify_proc, instmap.
:- import_module hlds_goal.
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
call_gen__generate_det_call(PredId, ModeId, Arguments, GoalInfo, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, CodeA),
	code_info__setup_call(Args, caller, CodeB),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),

	code_info__get_instmap(InstMap),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap, InstMapDelta,
		AfterCallInstMap) },

	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
	{ call_gen__output_arg_locs(Args, OutputArguments) },
	call_gen__generate_return_livevals(OutArgs, OutputArguments,
		AfterCallInstMap, OutLiveVals),
	code_info__make_entry_label(ModuleInfo, PredId, ModeId, yes, Address),
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

call_gen__generate_semidet_call(PredId, ProcId, Arguments, GoalInfo, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
	( { CodeModel = model_semi } ->
		call_gen__generate_semidet_call_2(PredId, ProcId, Arguments,
			GoalInfo, Code)
	;
		call_gen__generate_nondet_call(PredId, ProcId, Arguments, 
			GoalInfo, Code)
	).

:- pred call_gen__generate_semidet_call_2(pred_id, proc_id, list(var),
			hlds_goal_info, code_tree, code_info, code_info).
:- mode call_gen__generate_semidet_call_2(in, in, in, in, out, in, out) is det.

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

call_gen__generate_semidet_call_2(PredId, ModeId, Arguments, GoalInfo, Code) -->
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

	code_info__get_instmap(InstMap),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap, InstMapDelta,
		AfterCallInstMap) },

	call_gen__generate_return_livevals(OutArgs, OutputArguments, 
		AfterCallInstMap, OutLiveVals),
	code_info__make_entry_label(ModuleInfo, PredId, ModeId, yes, Address),
	{ CodeC1 = node([
		call(Address, label(ReturnLabel), OutLiveVals, semidet)
			- "branch to semidet procedure",
		label(ReturnLabel)
			- "Continuation label"
	]) },
	call_gen__rebuild_registers(Args),
	code_info__generate_failure(FailCode),
	code_info__get_next_label(ContLab),
	{ CodeD = tree(node([
		if_val(lval(reg(r, 1)), label(ContLab)) -
			"Test for success"
		]), tree(FailCode, node([ label(ContLab) - "" ]))) },

	{ Code = tree(CodeA, tree(CodeB, tree(tree(CodeC0, CodeC1), CodeD))) }.

%---------------------------------------------------------------------------%

call_gen__generate_nondet_call(PredId, ModeId, Arguments, GoalInfo, Code) -->
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
	{ call_gen__select_out_args(Args, OutArgs) },
	call_gen__save_variables(OutArgs, SaveCode),
	code_info__may_use_nondet_tailcall(TailCall),
	code_info__unset_failure_cont(FlushCode),
	code_info__setup_call(Args, caller, SetupCode),
	code_info__get_next_label(ReturnLabel),
	code_info__get_module_info(ModuleInfo),
	{ call_gen__input_args(ArgInfo, InputArguments) },
	call_gen__generate_call_livevals(OutArgs, InputArguments, LiveCode),
	{ call_gen__output_arg_locs(Args, OutputArguments) },

	code_info__get_instmap(InstMap),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap, InstMapDelta,
		AfterCallInstMap) },

	call_gen__generate_return_livevals(OutArgs, OutputArguments,
		AfterCallInstMap, OutLiveVals),
	code_info__make_entry_label(ModuleInfo, PredId, ModeId, yes, Address),
	{ CallModel = nondet(TailCall) },
	{ CallCode = node([
		call(Address, label(ReturnLabel), OutLiveVals, CallModel)
			- "branch to nondet procedure",
		label(ReturnLabel)
			- "Continuation label"
	]) },
	call_gen__rebuild_registers(Args),
	{ Code =
		tree(SaveCode,
		tree(FlushCode,
		tree(SetupCode,
		tree(LiveCode,
		     CallCode))))
	}.

%---------------------------------------------------------------------------%

call_gen__save_variables(Args, Code) -->
	code_info__get_known_variables(Variables0),
	{ set__list_to_set(Variables0, Vars0) },
	{ set__difference(Vars0, Args, Vars1) },
	code_info__get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) },
	( 
		{ GC_Method = accurate }
	->
		code_info__get_proc_info(ProcInfo),
		{ proc_info_get_typeinfo_vars_setwise(ProcInfo, Vars1, 
			TypeInfoVars) },
		{ set__union(Vars1, TypeInfoVars, Vars) }
	;
		{ Vars = Vars1 }
	),
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
		code_info__set_var_location(Var, Register)
	;
		{ true }
	),
	call_gen__rebuild_registers_2(Args).

%---------------------------------------------------------------------------%

call_gen__generate_det_builtin(PredId, ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ code_util__translate_builtin(ModuleName, PredName, ProcId,
			Args, no, yes(Var - Rval)) }
	->
		code_info__cache_expression(Var, Rval),
		{ Code = empty }
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_semidet_builtin(PredId, ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	(
		{ code_util__translate_builtin(ModuleName, PredName, ProcId,
			Args, yes(Rval0), Assign) }
	->
		( { Rval0 = binop(BinOp, X0, Y0) } ->
			call_gen__generate_builtin_arg(X0, X, CodeX),
			call_gen__generate_builtin_arg(Y0, Y, CodeY),
			{ Rval = binop(BinOp, X, Y) },
			{ ArgCode = tree(CodeX, CodeY) }
		; { Rval0 = unop(UnOp, X0) } ->
			call_gen__generate_builtin_arg(X0, X, ArgCode),
			{ Rval = unop(UnOp, X) }
		;
			{ error("Unknown builtin predicate") }
		),
		code_info__fail_if_rval_is_false(Rval, TestCode),
		( { Assign = yes(Var - AssignRval) } ->
			code_info__cache_expression(Var, AssignRval)
		;
			[]
		),
		{ Code = tree(ArgCode, TestCode) }
	;
		{ error("Unknown builtin predicate") }
	).

%---------------------------------------------------------------------------%

call_gen__generate_nondet_builtin(_PredId, _ProcId, _Args, _Code) -->
	% there aren't any nondet builtins
	{ error("Unknown nondet builtin predicate") }.

%---------------------------------------------------------------------------%

:- pred call_gen__generate_builtin_arg(rval, rval, code_tree,
	code_info, code_info).
:- mode call_gen__generate_builtin_arg(in, out, out, in, out) is det.

call_gen__generate_builtin_arg(Rval0, Rval, Code) -->
	( { Rval0 = var(Var) } ->
		code_info__produce_variable(Var, Code, Rval)
	;
		{ Rval = Rval0 },
		{ Code = empty }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

call_gen__partition_args([], [], []).
call_gen__partition_args([V - arg_info(_Loc,Mode) | Rest], Ins, Outs) :-
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

/* DEAD CODE
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
				GoalInfo, OutLiveVals),
		{ code_util__make_uni_label(ModuleInfo, VarTypeId, ModeNum,
			UniLabel) },
		{ Address = imported(UniLabel) },
	/\************
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
	**************\/
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
		% `type_to_type_id' failed - the type must be a type variable,
		% i.e. it is a polymorphic unification.
		% However, these sorts of unifications should have been changed
		% into calls to unify/2 by polymorphism.m, so if we encounter
		% any here, it's an internal error.
		{ error("unexpected polymorphic unification") }
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
*/

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
	set__insert(LiveVals0, R, LiveVals1),
	call_gen__insert_arg_livevals(As, LiveVals1, LiveVals).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_return_livevals(set(var), list(pair(var, arg_loc)),
		instmap, list(liveinfo), code_info, code_info).
:- mode call_gen__generate_return_livevals(in, in, in, out, in, out) is det.

call_gen__generate_return_livevals(OutArgs, OutputArgs, AfterCallInstMap, 
		LiveVals) -->
	code_info__generate_stack_livelvals(OutArgs, AfterCallInstMap, 
		LiveVals0),
	code_info__get_globals(Globals),
	{ globals__get_gc_method(Globals, GC_Method) },
	call_gen__insert_arg_livelvals(OutputArgs, GC_Method, AfterCallInstMap,
		LiveVals0, LiveVals).
	

% Maybe a varlist to type_id list would be a better way to do this...

%---------------------------------------------------------------------------%

:- pred call_gen__insert_arg_livelvals(list(pair(var, arg_loc)), gc_method, 
	instmap, list(liveinfo), list(liveinfo), code_info, code_info).
:- mode call_gen__insert_arg_livelvals(in, in, in, in, out, in, out) is det.

call_gen__insert_arg_livelvals([], _, _, LiveVals, LiveVals) --> [].
call_gen__insert_arg_livelvals([Var - L | As], GC_Method, AfterCallInstMap, 
		LiveVals0, LiveVals) -->
	(
		{ GC_Method = accurate }
	->
		{ instmap__lookup_var(AfterCallInstMap, Var, Inst) },

		code_info__variable_type(Var, Type),
		{ type_util__vars(Type, TypeVars) },
		code_info__find_type_infos(TypeVars, TypeParams),
		{ LiveVal = live_lvalue(R, var(Type, Inst), TypeParams) }
	;
		{ LiveVal = live_lvalue(R, unwanted, []) }
	),
	{ code_util__arg_loc_to_register(L, R) },
	call_gen__insert_arg_livelvals(As, GC_Method, AfterCallInstMap, 
		[LiveVal | LiveVals0], LiveVals).

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
		Modes, Det, GoalInfo, Code) -->
	{ determinism_to_code_model(Det, InnerCodeModel) },
	code_info__get_globals(Globals),
	code_info__get_module_info(ModuleInfo),
	{ globals__get_args_method(Globals, ArgsMethod) },
	{ make_arg_infos(ArgsMethod, Types, Modes, InnerCodeModel, ModuleInfo,
		ArgInfo) },
	{ assoc_list__from_corresponding_lists(Args, ArgInfo, ArgsAndArgInfo) },
	{ call_gen__partition_args(ArgsAndArgInfo, InVars, OutVars) },
	call_gen__generate_higher_call(InnerCodeModel, PredVar, InVars, OutVars,
		GoalInfo, Code).

:- pred call_gen__generate_higher_call(code_model, var, list(var), list(var),
			hlds_goal_info, code_tree, code_info, code_info).
:- mode call_gen__generate_higher_call(in, in, in, in, in, out, in, out) is det.

call_gen__generate_higher_call(CodeModel, PredVar, InVars, OutVars, GoalInfo,
		Code) -->
	code_info__succip_is_used,
	{ set__list_to_set(OutVars, OutArgs) },
	call_gen__save_variables(OutArgs, SaveCode),
	(
		{ CodeModel = model_det },
		{ CallModel = det },
		{ RuntimeAddr = do_det_closure },
		{ FlushCode = empty }
	;
		{ CodeModel = model_semi },
		{ CallModel = semidet },
		{ RuntimeAddr = do_semidet_closure },
		{ FlushCode = empty }
	;
		{ CodeModel = model_non },
		code_info__may_use_nondet_tailcall(TailCall),
		{ CallModel = nondet(TailCall) },
		{ RuntimeAddr = do_nondet_closure },
		code_info__unset_failure_cont(FlushCode)
	),
		% place the immediate input arguments in registers
		% starting at r4.
	call_gen__generate_immediate_args(InVars, 4, InLocs, ImmediateCode),
	code_info__generate_stack_livevals(OutArgs, LiveVals0),
	{ set__insert_list(LiveVals0,
		[reg(r, 1), reg(r, 2), reg(r, 3) | InLocs], LiveVals) },
	(
		{ CodeModel = model_semi }
	->
		{ FirstArg = 2 }
	;
		{ FirstArg = 1 }
	),
	{ call_gen__outvars_to_outargs(OutVars, FirstArg, OutArguments) },
	{ call_gen__output_arg_locs(OutArguments, OutLocs) },

	code_info__get_instmap(InstMap),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap, InstMapDelta,
		AfterCallInstMap) },

	call_gen__generate_return_livevals(OutArgs, OutLocs, AfterCallInstMap, 
		OutLiveVals),
	code_info__produce_variable(PredVar, PredVarCode, PredRVal),
	(
		{ PredRVal = lval(reg(r, 1)) }
	->
		{ CopyCode = empty }
	;
		{ CopyCode = node([
			assign(reg(r, 1), PredRVal) - "Copy pred-term"
		])}
	),
	{ list__length(InVars, NInVars) },
	{ list__length(OutVars, NOutVars) },
	{ SetupCode = tree(CopyCode, node([
			assign(reg(r, 2), const(int_const(NInVars))) -
				"Assign number of immediate input arguments",
			assign(reg(r, 3), const(int_const(NOutVars))) -
				"Assign number of output arguments"
		])
	) },
	code_info__get_next_label(ReturnLabel),
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
		{ TestSuccessCode = node([
			if_val(lval(reg(r, 1)), label(ContLab)) -
				"Test for success"
		]) },
		{ ContLabelCode = node([label(ContLab) - ""]) },
		{ CallCode =
			tree(TryCallCode,
			tree(TestSuccessCode,
			tree(FailCode,
			     ContLabelCode))) }
	;
		{ CallCode = TryCallCode }
	),
	{ Code =
		tree(SaveCode,
		tree(FlushCode,
		tree(ImmediateCode,
		tree(PredVarCode,
		tree(SetupCode,
		     CallCode)))))
	}.

%---------------------------------------------------------------------------%

:- pred call_gen__generate_immediate_args(list(var), int, list(lval), code_tree,
							code_info, code_info).
:- mode call_gen__generate_immediate_args(in, in, out, out, in, out) is det.

call_gen__generate_immediate_args([], _N, [], empty) --> [].
call_gen__generate_immediate_args([V | Vs], N0, [Lval | Lvals], Code) -->
	{ Lval = reg(r, N0) },
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
