%---------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: call_gen.m
%
% Authors: conway, zs.
%
% This module provides predicates for generating procedure calls,
% including calls to higher-order pred variables.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ll_backend__call_gen.

:- interface.

:- import_module parse_tree__prog_data, hlds__hlds_pred, hlds__hlds_goal.
:- import_module backend_libs__code_model, ll_backend__llds.
:- import_module ll_backend__code_info.
:- import_module list, assoc_list.

:- pred call_gen__generate_call(code_model::in, pred_id::in, proc_id::in,
	list(prog_var)::in, hlds_goal_info::in, code_tree::out,
	code_info::in, code_info::out) is det.

:- pred call_gen__generate_generic_call(code_model::in, generic_call::in,
	list(prog_var)::in, list(mode)::in, determinism::in,
	hlds_goal_info::in, code_tree::out, code_info::in, code_info::out)
	is det.

:- pred call_gen__generate_builtin(code_model::in, pred_id::in, proc_id::in,
	list(prog_var)::in, code_tree::out, code_info::in, code_info::out)
	is det.

:- pred call_gen__maybe_remove_aditi_state_args(generic_call::in,
	list(prog_var)::in, list(type)::in, list(mode)::in,
	list(prog_var)::out, list(type)::out, list(mode)::out) is det.

	% call_gen__generic_call_info(CodeModel, GenericCall,
	% 	CodeAddr, FirstImmediateInputReg).
:- pred call_gen__generic_call_info(code_model::in, generic_call::in,
	code_addr::out, assoc_list(prog_var, arg_info)::out, int::out) is det.

:- pred call_gen__input_arg_locs(assoc_list(prog_var, arg_info)::in,
	assoc_list(prog_var, arg_loc)::out) is det.

:- pred call_gen__output_arg_locs(assoc_list(prog_var, arg_info)::in,
	assoc_list(prog_var, arg_loc)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_module, hlds__hlds_data, hlds__hlds_llds.
:- import_module check_hlds__polymorphism, check_hlds__type_util.
:- import_module check_hlds__mode_util, check_hlds__unify_proc, hlds__instmap.
:- import_module ll_backend__arg_info, ll_backend__code_util.
:- import_module ll_backend__trace.
:- import_module aditi_backend__rl.
:- import_module backend_libs__builtin_ops.
:- import_module libs__globals, libs__options, libs__tree. 

:- import_module bool, int, string, map, set, std_util, require, varset.

%---------------------------------------------------------------------------%

call_gen__generate_call(CodeModel, PredId, ProcId, ArgVars, GoalInfo, Code) -->

		% Find out which arguments are input and which are output.
	code_info__get_pred_proc_arginfo(PredId, ProcId, ArgInfo),
	{ assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgsInfos) },

		% Save the necessary vars on the stack and move the input args
		% to their registers.
	code_info__setup_call(GoalInfo, ArgsInfos, LiveVals, SetupCode),
	call_gen__kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs),

		% Figure out what the call model is.
	call_gen__prepare_for_call(CodeModel, CallModel, TraceCode),

		% Make the call.
	code_info__get_module_info(ModuleInfo),
	code_info__make_entry_label(ModuleInfo, PredId, ProcId, yes, Address),
	code_info__get_next_label(ReturnLabel),
	{ call_gen__call_comment(CodeModel, CallComment) },
	{ goal_info_get_context(GoalInfo, Context) },
	{ goal_info_get_goal_path(GoalInfo, GoalPath) },
	{ CallCode = node([
		livevals(LiveVals)
			- "",
		call(Address, label(ReturnLabel), ReturnLiveLvalues, Context,
			GoalPath, CallModel)
			- CallComment,
		label(ReturnLabel)
			- "continuation label"
	]) },

		% Figure out what variables will be live at the return point,
		% and where, for use in the accurate garbage collector, and
		% in the debugger.
	code_info__get_instmap(InstMap),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap, InstMapDelta, ReturnInstMap) },

		% Update the code generator state to reflect the situation
		% after the call.
	call_gen__handle_return(ArgsInfos, GoalInfo, NonLiveOutputs,
		ReturnInstMap, ReturnLiveLvalues),

		% If the call can fail, generate code to check for and
		% handle the failure.
	call_gen__handle_failure(CodeModel, GoalInfo, FailHandlingCode),

	{ Code =
		tree(SetupCode,
		tree(TraceCode,
		tree(CallCode,
		     FailHandlingCode)))
	}.

%---------------------------------------------------------------------------%

	%
	% For a generic_call,
	% we split the arguments into inputs and outputs, put the inputs
	% in the locations expected by mercury__do_call_closure in
	% runtime/mercury_ho_call.c, generate the call to that code,
	% and pick up the outputs from the locations that we know
	% the runtime system leaves them in.
	%

call_gen__generate_generic_call(_OuterCodeModel, GenericCall, Args0,
		Modes0, Det, GoalInfo, Code) -->
	list__map_foldl(code_info__variable_type, Args0, Types0),
	{ call_gen__maybe_remove_aditi_state_args(GenericCall,
		Args0, Types0, Modes0, Args, Types, Modes) },

	{ determinism_to_code_model(Det, CodeModel) },
	{ call_gen__generic_call_info(CodeModel, GenericCall,
		CodeAddr, SpecifierArgInfos, FirstImmInput) },
	{ CodeModel = model_semi ->
		FirstOutput = 2
	;
		FirstOutput = 1
	},

	code_info__get_module_info(ModuleInfo),
	{ arg_info__compute_in_and_out_vars(ModuleInfo, Args, Modes, Types,
		InVars, OutVars) },
	{ call_gen__give_vars_consecutive_arg_infos(InVars, FirstImmInput,
		top_in, InVarArgInfos) },
	{ call_gen__give_vars_consecutive_arg_infos(OutVars, FirstOutput,
		top_out, OutArgsInfos) },
	{ list__append(SpecifierArgInfos, InVarArgInfos, InArgInfos) },
	{ list__append(InArgInfos, OutArgsInfos, ArgInfos) },

		% Save the necessary vars on the stack and move the input args
		% defined by variables to their registers.
	code_info__setup_call(GoalInfo, ArgInfos, LiveVals0, SetupCode),
	call_gen__kill_dead_input_vars(ArgInfos, GoalInfo, NonLiveOutputs),

		% Move the input args not defined by variables to their
		% registers. Setting up these arguments last results in
		% slightly more efficient code, since we can use their
		% registers when placing the variables.
	call_gen__generic_call_nonvar_setup(GenericCall, InVars, OutVars,
		NonVarCode),

	{ call_gen__extra_livevals(FirstImmInput, ExtraLiveVals) },
	{ set__insert_list(LiveVals0, ExtraLiveVals, LiveVals) },

	call_gen__prepare_for_call(CodeModel, CallModel, TraceCode),

		% Make the call.
	code_info__get_next_label(ReturnLabel),
	{ goal_info_get_context(GoalInfo, Context) },
	{ goal_info_get_goal_path(GoalInfo, GoalPath) },
	{ CallCode = node([
		livevals(LiveVals)
			- "",
		call(CodeAddr, label(ReturnLabel), ReturnLiveLvalues,
			Context, GoalPath, CallModel)
			- "Setup and call",
		label(ReturnLabel)
			- "Continuation label"
	]) },

		% Figure out what variables will be live at the return point,
		% and where, for use in the accurate garbage collector, and
		% in the debugger.
	code_info__get_instmap(InstMap),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap, InstMapDelta, ReturnInstMap) },

		% Update the code generator state to reflect the situation
		% after the call.
	call_gen__handle_return(OutArgsInfos, GoalInfo, NonLiveOutputs,
		ReturnInstMap, ReturnLiveLvalues),

		% If the call can fail, generate code to check for and
		% handle the failure.
	call_gen__handle_failure(CodeModel, GoalInfo, FailHandlingCode),

	{ Code =
		tree(SetupCode,
		tree(NonVarCode,
		tree(TraceCode,
		tree(CallCode,
		     FailHandlingCode))))
	}.

call_gen__maybe_remove_aditi_state_args(GenericCall, Args0, Types0, Modes0,
		Args, Types, Modes) :-
	( GenericCall = aditi_builtin(aditi_tuple_insert_delete(_, _), _) ->
		% Remove the `aditi__state' argument and its type-info from
		% the tuple to insert or delete. This must be done after
		% mode analysis (so that removal of the `aditi__state' does
		% not stuff up the argument numbers in error messages).
		% Here is as good a place as any.
		get_state_args_det(Types0, TupleTypes, _, _),
		call_gen__remove_tuple_state_arg(TupleTypes, Args0, Args),
		call_gen__remove_tuple_state_arg(TupleTypes, Types0, Types),
		call_gen__remove_tuple_state_arg(TupleTypes, Modes0, Modes)
	;
		Args = Args0,
		Types = Types0,
		Modes = Modes0
	).

:- pred call_gen__remove_tuple_state_arg(list(type)::in, list(T)::in,
	list(T)::out) is det.

call_gen__remove_tuple_state_arg(TupleTypes, Args0, Args) :-
	get_state_args_det(Args0, OtherArgs0, State0Arg, StateArg),
	assoc_list__from_corresponding_lists(TupleTypes, OtherArgs0,
		TypesAndArgs0),
	list__filter(
		(pred((Type - _)::in) is semidet :-
			\+ type_is_aditi_state(Type),
			\+ (
				polymorphism__type_info_type(Type, TheType),
				type_is_aditi_state(TheType)
			)
		), TypesAndArgs0, TypesAndArgs),
	assoc_list__values(TypesAndArgs, OtherArgs),
	list__append(OtherArgs, [State0Arg, StateArg], Args).

%---------------------------------------------------------------------------%

	% The registers before the first input argument are all live.
:- pred call_gen__extra_livevals(int::in, list(lval)::out) is det.

call_gen__extra_livevals(FirstInput, ExtraLiveVals) :-
	call_gen__extra_livevals(1, FirstInput, ExtraLiveVals).

:- pred call_gen__extra_livevals(int::in, int::in, list(lval)::out) is det.

call_gen__extra_livevals(Reg, FirstInput, ExtraLiveVals) :-
	( Reg < FirstInput ->
		ExtraLiveVals = [reg(r, Reg) | ExtraLiveVals1],
		NextReg is Reg + 1,
		call_gen__extra_livevals(NextReg, FirstInput, ExtraLiveVals1)
	;
		ExtraLiveVals = []
	).

call_gen__generic_call_info(_, higher_order(PredVar, _, _),
		do_call_closure, [PredVar - arg_info(1, top_in)], 4).
call_gen__generic_call_info(_, class_method(TCVar, _, _, _),
		do_call_class_method, [TCVar - arg_info(1, top_in)], 5).
call_gen__generic_call_info(CodeModel, aditi_builtin(aditi_call(_,_,_,_),_),
		CodeAddr, [], 5) :-
	( CodeModel = model_det, CodeAddr = do_det_aditi_call
	; CodeModel = model_semi, CodeAddr = do_semidet_aditi_call
	; CodeModel = model_non, CodeAddr = do_nondet_aditi_call
	).
call_gen__generic_call_info(CodeModel,
		aditi_builtin(aditi_tuple_insert_delete(InsertDelete, _), _),
		CodeAddr, [], 5) :-
	( InsertDelete = insert, CodeAddr = do_aditi_insert
	; InsertDelete = delete, CodeAddr = do_aditi_delete
	),
	require(unify(CodeModel, model_det),
		"aditi_insert/delete not model_det").
call_gen__generic_call_info(CodeModel,
		aditi_builtin(
			aditi_insert_delete_modify(InsertDelMod, _, _), _),
		CodeAddr, [], FirstReg) :-
	call_gen__aditi_insert_delete_modify_info(InsertDelMod,
		CodeAddr, FirstReg),
	require(unify(CodeModel, model_det),
		"aditi_insert_delete_modify not model_det").

:- pred call_gen__aditi_insert_delete_modify_info(
	aditi_insert_delete_modify::in, code_addr::out, int::out) is det.

call_gen__aditi_insert_delete_modify_info(bulk_insert,
		do_aditi_bulk_insert, 3).
call_gen__aditi_insert_delete_modify_info(delete(filter), _, _) :-
	error("Sorry, not yet implemented: aditi_delete(filter)").
call_gen__aditi_insert_delete_modify_info(delete(bulk),
		do_aditi_bulk_delete, 3).
call_gen__aditi_insert_delete_modify_info(modify(filter), _, _) :-
	error("Sorry, not yet implemented: aditi_modify(filter)").
call_gen__aditi_insert_delete_modify_info(modify(bulk),
		do_aditi_bulk_modify, 3).

	% Some of the values that generic call passes to the dispatch routine
	% to specify what code is being indirectly called come from HLDS
	% variables, while the others come from constants. The ones that come
	% from variables (the closure for a higher order call, the
	% typeclass_info for a method call) are set up together with the
	% arguments being passed the indirectly called code, since with eager
	% code generation this ensures that each target register is reserved
	% for the variable destined for it. This is set up by
	% call_gen__generic_call_info. call_gen__generic_call_nonvar_setup
	% generates code to pass to the dispatch routine the parts of the
	% indirectly called code's identifier that come from constants.

:- pred call_gen__generic_call_nonvar_setup(generic_call::in,
	list(prog_var)::in, list(prog_var)::in, code_tree::out,
	code_info::in, code_info::out) is det.

call_gen__generic_call_nonvar_setup(higher_order(_, _, _),
		InVars, OutVars, Code) -->
	code_info__clobber_regs([reg(r, 2), reg(r, 3)]),
	{ list__length(InVars, NInVars) },
	{ list__length(OutVars, NOutVars) },
	{ Code = node([
		assign(reg(r, 2), const(int_const(NInVars))) -
			"Assign number of immediate input arguments",
		assign(reg(r, 3), const(int_const(NOutVars))) -
			"Assign number of output arguments"
	]) }.
call_gen__generic_call_nonvar_setup(class_method(_, Method, _, _),
		InVars, OutVars, Code) -->
	code_info__clobber_regs([reg(r, 2), reg(r, 3), reg(r, 4)]),
	{ list__length(InVars, NInVars) },
	{ list__length(OutVars, NOutVars) },
	{ Code = node([
		assign(reg(r, 2), const(int_const(Method))) -
			"Index of class method in typeclass info",
		assign(reg(r, 3), const(int_const(NInVars))) -
			"Assign number of immediate input arguments",
		assign(reg(r, 4), const(int_const(NOutVars))) -
			"Assign number of output arguments"
	]) }.
call_gen__generic_call_nonvar_setup(aditi_builtin(Builtin, _),
		InVars, OutVars, Code) -->
	call_gen__aditi_builtin_setup(Builtin, InVars, OutVars, Code).

:- pred call_gen__aditi_builtin_setup(aditi_builtin::in,
	list(prog_var)::in, list(prog_var)::in, code_tree::out,
	code_info::in, code_info::out) is det.

call_gen__aditi_builtin_setup(
		aditi_call(PredProcId, NumInputs, InputTypes, NumOutputs),
		_, _, SetupCode) -->
	code_info__get_module_info(ModuleInfo),
	{ rl__get_entry_proc_name(ModuleInfo, PredProcId, ProcName) },
	{ rl__proc_name_to_string(ProcName, ProcStr) },
	{ rl__schema_to_string(ModuleInfo, InputTypes, InputSchema) },
	code_info__clobber_regs([reg(r, 1), reg(r, 2), reg(r, 3), reg(r, 4)]),
	{ SetupCode = node([
		assign(reg(r, 1), const(string_const(ProcStr))) -
			"Assign name of procedure to call",
		assign(reg(r, 2), const(int_const(NumInputs))) -
			"Assign number of input arguments",
		assign(reg(r, 3), const(string_const(InputSchema))) -
			"Assign schema of input arguments",
		assign(reg(r, 4), const(int_const(NumOutputs))) -
			"Assign number of output arguments"
	]) }.

call_gen__aditi_builtin_setup(
		aditi_tuple_insert_delete(InsertOrDelete, PredId),
		InputArgs, _, SetupCode) -->
	code_info__clobber_regs([reg(r, 1), reg(r, 2), reg(r, 3), reg(r, 4)]),
	call_gen__setup_base_relation_name(PredId, NameCode),

	code_info__get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_arity(PredInfo, PredArity) },
	% The `aditi__state' was removed.
	{ TupleArity = PredArity - 1 },
	{ ArityCode = node([
			assign(reg(r, 2), const(int_const(TupleArity))) -
				"Assign arity of relation to insert into"
		]) },

	(
		{ InsertOrDelete = insert },
		{ ProcCode = empty }
	;
		{ InsertOrDelete = delete },

		%
		% For now tuple deletions need to be done as bulk
		% deletions. The API function to delete a single
		% tuple only works if the relation being
		% deleted from has an index.
		%

		call_gen__setup_update_proc_name(rl__get_delete_proc_name,
			PredId, reg(r, 3), ProcNameCode),

		%
		% Work out the schema of the input relation of the
		% deletion procedure
		%
		{ list__reverse(InputArgs, RevInputArgs) },
		{
			RevInputArgs = [_DiState | RevTupleArgs],
			list__reverse(RevTupleArgs, TupleArgs0),
			list__length(TupleArgs0, TupleArityTimes2),

			% Remove the type-infos for the tuple arguments.
			list__drop(TupleArityTimes2 // 2,
				TupleArgs0, TupleArgs1)
		->
			TupleArgs = TupleArgs1
		;
			error(
	"call_gen__aditi_builtin_setup: error in schema for aditi_delete")
		},
		list__map_foldl(code_info__variable_type,
			TupleArgs, TupleTypes),
		{ rl__schema_to_string(ModuleInfo, TupleTypes, InputSchema) },
		{ ProcSchemaCode =
			node([
				assign(reg(r, 4),
					const(string_const(InputSchema))) -
				"Assign schema of tuple to insert/delete"
			]) },

		{ ProcCode = tree(ProcNameCode, ProcSchemaCode) }
	),
	{ SetupCode = tree(NameCode, tree(ArityCode, ProcCode)) }.

call_gen__aditi_builtin_setup(
		aditi_insert_delete_modify(InsertDelMod, PredId, _),
		_, _, SetupCode) -->
	call_gen__aditi_insert_delete_modify_setup(InsertDelMod,
		PredId, SetupCode).

:- pred call_gen__aditi_insert_delete_modify_setup(
	aditi_insert_delete_modify::in, pred_id::in, code_tree::out,
	code_info::in, code_info::out) is det.

call_gen__aditi_insert_delete_modify_setup(bulk_insert, PredId, SetupCode) -->
	code_info__clobber_regs([reg(r, 1), reg(r, 2)]),
	call_gen__setup_base_relation_name(PredId, RelNameCode),
	call_gen__setup_update_proc_name(rl__get_insert_proc_name,
		PredId, reg(r, 2), ProcNameCode),
	{ SetupCode = tree(RelNameCode, ProcNameCode) }.
call_gen__aditi_insert_delete_modify_setup(delete(_), PredId, SetupCode) -->
	code_info__clobber_regs([reg(r, 1), reg(r, 2)]),
	call_gen__setup_base_relation_name(PredId, RelNameCode),
	call_gen__setup_update_proc_name(rl__get_delete_proc_name,
		PredId, reg(r, 2), ProcNameCode),
	{ SetupCode = tree(RelNameCode, ProcNameCode) }.
call_gen__aditi_insert_delete_modify_setup(modify(_), PredId, SetupCode) -->
	code_info__clobber_regs([reg(r, 1), reg(r, 2)]),
	call_gen__setup_base_relation_name(PredId, RelNameCode),
	call_gen__setup_update_proc_name(rl__get_modify_proc_name,
		PredId, reg(r, 2), ProcNameCode),
	{ SetupCode = tree(RelNameCode, ProcNameCode) }.

:- pred call_gen__setup_base_relation_name(pred_id::in,
	code_tree::out, code_info::in, code_info::out) is det.

call_gen__setup_base_relation_name(PredId, SetupCode) -->
	code_info__get_module_info(ModuleInfo),
	{ rl__permanent_relation_name(ModuleInfo, PredId, ProcStr) },
	{ SetupCode = node([
		assign(reg(r, 1), const(string_const(ProcStr))) -
			"Assign name of base relation"
	]) }.

:- pred call_gen__setup_update_proc_name(
	pred(module_info, pred_id, rl_proc_name),
	pred_id, lval, code_tree, code_info, code_info).
:- mode call_gen__setup_update_proc_name(pred(in, in, out) is det,
	in, in, out, in, out) is det.

call_gen__setup_update_proc_name(NamePred, PredId, Lval, ProcNameCode) -->
	code_info__get_module_info(ModuleInfo),
	{ NamePred(ModuleInfo, PredId, ProcName) },
	{ rl__proc_name_to_string(ProcName, ProcNameStr) },
	{ ProcNameCode =
		node([
			assign(Lval,
				const(string_const(ProcNameStr))) -
				"Assign name of update RL procedure"
		])
	}.

%---------------------------------------------------------------------------%

:- pred call_gen__prepare_for_call(code_model::in, call_model::out,
	code_tree::out, code_info::in, code_info::out) is det.

call_gen__prepare_for_call(CodeModel, CallModel, TraceCode) -->
	code_info__succip_is_used,
	(
		{ CodeModel = model_det },
		{ CallModel = det }
	;
		{ CodeModel = model_semi },
		{ CallModel = semidet }
	;
		{ CodeModel = model_non },
		code_info__may_use_nondet_tailcall(TailCallStatus),
		{ CallModel = nondet(TailCallStatus) },
		code_info__set_resume_point_and_frame_to_unknown
	),
	trace__prepare_for_call(TraceCode).

:- pred call_gen__handle_failure(code_model::in, hlds_goal_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

call_gen__handle_failure(CodeModel, GoalInfo, FailHandlingCode) -->
	( { CodeModel = model_semi } ->
		{ goal_info_get_determinism(GoalInfo, Detism) },
		( { Detism = failure } ->
			code_info__generate_failure(FailHandlingCode)
		;
			code_info__get_next_label(ContLab),
			{ FailTestCode = node([
				if_val(lval(reg(r, 1)), label(ContLab))
					- "test for success"
			]) },
			code_info__generate_failure(FailCode),
			{ ContLabelCode = node([
				label(ContLab)
					- ""
			]) },
			{ FailHandlingCode =
				tree(FailTestCode,
				tree(FailCode,
				     ContLabelCode))
			}
		)
	;
		{ FailHandlingCode = empty }
	).

:- pred call_gen__call_comment(code_model::in, string::out) is det.

call_gen__call_comment(model_det,  "branch to det procedure").
call_gen__call_comment(model_semi, "branch to semidet procedure").
call_gen__call_comment(model_non,  "branch to nondet procedure").

%---------------------------------------------------------------------------%

	% After we have placed all the input variables in their registers,
	% we will want to clear all the registers so we can start updating
	% the code generator state to reflect their contents after the call.
	% (In the case of higher order calls, we may place some constant
	% input arguments in registers before clearing them.) The register
	% clearing code complains if it is asked to dispose of the last copy
	% of a still live variable, so before we clear the registers, we must
	% make forward-dead all the variables that are in this goal's
	% post-death set. However, a variable may be in this set even if it
	% is not live before the call, if it is bound by the call. (This can
	% happen when the caller ignores some of the output arguments of the
	% called procedure.) We handle such variables not by making them
	% forward-dead but by simply never making them forward-live in the
	% first place.

	% ArgsInfos should list all the output arguments of the call.
	% It may contain the input arguments as well; kill_dead_input_vars
	% and handle_return ignore them.

:- pred call_gen__kill_dead_input_vars(assoc_list(prog_var, arg_info)::in,
	hlds_goal_info::in, set(prog_var)::out,
	code_info::in, code_info::out) is det.

call_gen__kill_dead_input_vars(ArgsInfos, GoalInfo, NonLiveOutputs) -->
	code_info__get_forward_live_vars(Liveness),
	{ call_gen__find_nonlive_outputs(ArgsInfos, Liveness,
		set__init, NonLiveOutputs) },
	{ goal_info_get_post_deaths(GoalInfo, PostDeaths) },
	{ set__difference(PostDeaths, NonLiveOutputs, ImmediatePostDeaths) },
	code_info__make_vars_forward_dead(ImmediatePostDeaths).

:- pred call_gen__handle_return(assoc_list(prog_var, arg_info)::in,
	hlds_goal_info::in, set(prog_var)::in, instmap::in,
	list(liveinfo)::out, code_info::in, code_info::out) is det.

call_gen__handle_return(ArgsInfos, GoalInfo, _NonLiveOutputs, ReturnInstMap,
		ReturnLiveLvalues) -->
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap_delta_is_reachable(InstMapDelta) ->
		OkToDeleteAny = no
	;
		OkToDeleteAny = yes
	},
	code_info__clear_all_registers(OkToDeleteAny),
	code_info__get_forward_live_vars(Liveness),
	call_gen__rebuild_registers(ArgsInfos, Liveness, OutputArgLocs),
	code_info__generate_return_live_lvalues(OutputArgLocs, ReturnInstMap,
		OkToDeleteAny, ReturnLiveLvalues).

:- pred call_gen__find_nonlive_outputs(assoc_list(prog_var, arg_info)::in,
	set(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

call_gen__find_nonlive_outputs([], _, NonLiveOutputs, NonLiveOutputs).
call_gen__find_nonlive_outputs([Var - arg_info(_ArgLoc, Mode) | Args],
		Liveness, NonLiveOutputs0, NonLiveOutputs) :-
	( Mode = top_out ->
		( set__member(Var, Liveness) ->
			NonLiveOutputs1 = NonLiveOutputs0
		;
			set__insert(NonLiveOutputs0, Var, NonLiveOutputs1)
		)
	;
		NonLiveOutputs1 = NonLiveOutputs0
	),
	call_gen__find_nonlive_outputs(Args, Liveness,
		NonLiveOutputs1, NonLiveOutputs).

:- pred call_gen__rebuild_registers(assoc_list(prog_var, arg_info)::in,
	set(prog_var)::in, assoc_list(prog_var, arg_loc)::out,
	code_info::in, code_info::out) is det.

call_gen__rebuild_registers([], _, []) --> [].
call_gen__rebuild_registers([Var - arg_info(ArgLoc, Mode) | Args], Liveness,
		OutputArgLocs) -->
	call_gen__rebuild_registers(Args, Liveness, OutputArgLocs1),
	(
		{ Mode = top_out },
		{ set__member(Var, Liveness) }
	->
		{ code_util__arg_loc_to_register(ArgLoc, Register) },
		code_info__set_var_location(Var, Register),
		{ OutputArgLocs = [Var - ArgLoc | OutputArgLocs1] }
	;
		{ OutputArgLocs = OutputArgLocs1 }
	).

%---------------------------------------------------------------------------%

call_gen__generate_builtin(CodeModel, PredId, ProcId, Args, Code) -->
	code_info__get_module_info(ModuleInfo),
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{
		builtin_ops__translate_builtin(ModuleName, PredName,
			ProcId, Args, SimpleCode0)
	->
		SimpleCode = SimpleCode0
	;
		length(Args, Arity),
		format("Unknown builtin predicate: %s/%d",
			[s(PredName), i(Arity)], Msg),
		error(Msg)
	},
	(
		{ CodeModel = model_det },
		(
			{ SimpleCode = assign(Var, AssignExpr) }
		->
			( code_info__variable_is_forward_live(Var) ->
				{ Rval = convert_simple_expr(AssignExpr) },
				code_info__assign_expr_to_var(Var, Rval, Code)
			;
				{ Code = empty }
			)
		;
			{ error("Malformed det builtin predicate") }
		)
	;
		{ CodeModel = model_semi },
		(
			{ SimpleCode = test(TestExpr) }
		->
			call_gen__generate_simple_test(TestExpr, Rval,
				ArgCode),
			code_info__fail_if_rval_is_false(Rval, TestCode),
			{ Code = tree(ArgCode, TestCode) }
		;
			{ error("Malformed semi builtin predicate") }
		)
	;
		{ CodeModel = model_non },
		{ error("Nondet builtin predicate") }
	).

:- func convert_simple_expr(simple_expr(prog_var)) = rval.

convert_simple_expr(leaf(Var)) = var(Var).
convert_simple_expr(int_const(Int)) = const(int_const(Int)).
convert_simple_expr(float_const(Float)) = const(float_const(Float)).
convert_simple_expr(unary(UnOp, Expr)) =
	unop(UnOp, convert_simple_expr(Expr)).
convert_simple_expr(binary(BinOp, Expr1, Expr2)) =
	binop(BinOp, convert_simple_expr(Expr1), convert_simple_expr(Expr2)).

:- pred call_gen__generate_simple_test(
	simple_expr(prog_var)::in(simple_test_expr), rval::out,
	code_tree::out, code_info::in, code_info::out) is det.

call_gen__generate_simple_test(TestExpr, Rval, ArgCode) -->
	(
		{ TestExpr = binary(BinOp, X0, Y0) },
		{ X1 = convert_simple_expr(X0) },
		{ Y1 = convert_simple_expr(Y0) },
		call_gen__generate_builtin_arg(X1, X, CodeX),
		call_gen__generate_builtin_arg(Y1, Y, CodeY),
		{ Rval = binop(BinOp, X, Y) },
		{ ArgCode = tree(CodeX, CodeY) }
	;
		{ TestExpr = unary(UnOp, X0) },
		{ X1 = convert_simple_expr(X0) },
		call_gen__generate_builtin_arg(X1, X, ArgCode),
		{ Rval = unop(UnOp, X) }
	).

:- pred call_gen__generate_builtin_arg(rval::in, rval::out, code_tree::out,
	code_info::in, code_info::out) is det.

call_gen__generate_builtin_arg(Rval0, Rval, Code) -->
	( { Rval0 = var(Var) } ->
		code_info__produce_variable(Var, Code, Rval)
	;
		{ Rval = Rval0 },
		{ Code = empty }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

call_gen__input_arg_locs([], []).
call_gen__input_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
	call_gen__input_arg_locs(Args, Vs0),
	( Mode = top_in ->
		Vs = [Var - Loc | Vs0]
	;
		Vs = Vs0
	).

call_gen__output_arg_locs([], []).
call_gen__output_arg_locs([Var - arg_info(Loc, Mode) | Args], Vs) :-
	call_gen__output_arg_locs(Args, Vs0),
	( Mode = top_out ->
		Vs = [Var - Loc | Vs0]
	;
		Vs = Vs0
	).

%---------------------------------------------------------------------------%

:- pred call_gen__generate_call_vn_livevals(list(arg_loc)::in,
	set(prog_var)::in, code_tree::out,
	code_info::in, code_info::out) is det.

call_gen__generate_call_vn_livevals(InputArgLocs, OutputArgs, Code) -->
	code_info__generate_call_vn_livevals(InputArgLocs, OutputArgs,
		LiveVals),
	{ Code = node([
		livevals(LiveVals) - ""
	]) }.

%---------------------------------------------------------------------------%

:- pred call_gen__give_vars_consecutive_arg_infos(list(prog_var)::in, int::in,
	arg_mode::in, assoc_list(prog_var, arg_info)::out) is det.

call_gen__give_vars_consecutive_arg_infos([], _N, _M, []).
call_gen__give_vars_consecutive_arg_infos([Var | Vars], N0, ArgMode,
		[Var - ArgInfo | ArgInfos]) :-
	ArgInfo = arg_info(N0, ArgMode),
	N1 = N0 + 1,
	call_gen__give_vars_consecutive_arg_infos(Vars, N1, ArgMode, ArgInfos).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
