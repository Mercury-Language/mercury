%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2004 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: aditi_builtin_ops.m
% Main author: stayl
%
% Transform Aditi builtin generic_call goals into calls to
% the predicates in extras/aditi/aditi_private_builtin.m.
%
%-----------------------------------------------------------------------------%
:- module aditi_backend__aditi_builtin_ops.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module io.

	% Transform Aditi builtin generic_call goals into calls to
	% the predicates in extras/aditi/aditi_private_builtin.m.
:- pred transform_aditi_builtins(module_info, module_info,
		io__state, io__state).
:- mode transform_aditi_builtins(in, out, di, uo) is det.

	% Change the goal for the given procedure to call the
	% corresponding Aditi procedure.
:- pred create_aditi_call_proc(pred_proc_id, module_info, module_info).
:- mode create_aditi_call_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module aditi_backend__rl.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module std_util.
:- import_module term.

transform_aditi_builtins(ModuleInfo0, ModuleInfo) -->
	passes_aux__process_all_nonimported_nonaditi_procs(
		update_module_io(transform_aditi_builtins_in_proc), _,
		ModuleInfo0, ModuleInfo).

:- pred transform_aditi_builtins_in_proc(pred_id, proc_id,
		proc_info, proc_info, module_info, module_info,
		io__state, io__state).
:- mode transform_aditi_builtins_in_proc(in, in, in, out,
		in, out, di, uo) is det.

transform_aditi_builtins_in_proc(PredId, _ProcId, !ProcInfo,
		!ModuleInfo, IO, IO) :-
	proc_info_goal(!.ProcInfo, Goal0),
	construct_aditi_transform_info(!.ModuleInfo, PredId, !.ProcInfo,
		Info0),
	transform_aditi_builtins_in_goal(Goal0, Goal, Info0, Info),
	deconstruct_aditi_transform_info(Info, PredId,
		!:ModuleInfo, !:ProcInfo, Changed),
	( Changed = yes ->
		proc_info_set_goal(Goal, !ProcInfo),
		requantify_proc(!ProcInfo),
		recompute_instmap_delta_proc(yes, !ProcInfo, !ModuleInfo)
	;
		true
	).

:- pred transform_aditi_builtins_in_goal(hlds_goal, hlds_goal,
		aditi_transform_info, aditi_transform_info).
:- mode transform_aditi_builtins_in_goal(in, out, in, out) is det.

transform_aditi_builtins_in_goal(Goal0 - GoalInfo, Goal - GoalInfo) -->
	transform_aditi_builtins_in_goal_expr(Goal0, GoalInfo, Goal).

:- pred transform_aditi_builtins_in_goal_expr(hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, aditi_transform_info, aditi_transform_info).
:- mode transform_aditi_builtins_in_goal_expr(in, in, out, in, out) is det.

transform_aditi_builtins_in_goal_expr(Goal0, GoalInfo, Goal) -->
	{ Goal0 = unify(_, _, _, Unification, _) },
	(
		{ Unification = construct(Var, ConsId, Args, _, _, _, _) },
		{ ConsId = pred_const(ShroudedPredProcId, aditi_bottom_up) },
		{ proc(PredId, ProcId) =
			unshroud_pred_proc_id(ShroudedPredProcId) }
	->
		^ changed := yes,
		transform_aditi_bottom_up_closure(Var, PredId, ProcId,
			Args, GoalInfo, Goal)
	;
		{ Goal = Goal0 }
	).
transform_aditi_builtins_in_goal_expr(Goal0, GoalInfo, Goal) -->
	{ Goal0 = generic_call(Call, Args, Modes, Det) },
	(
		{ Call = higher_order(_, _, _, _) },
		{ Goal = Goal0 }
	;
		{ Call = class_method(_, _, _, _) },
		{ Goal = Goal0 }
	;
		{ Call = unsafe_cast },
		{ Goal = Goal0 }
	;
		{ Call = aditi_builtin(Builtin0, _) },
		^ changed := yes,
		transform_aditi_builtin(Builtin0, Args, Modes, Det,
			GoalInfo, Goal)
	).
transform_aditi_builtins_in_goal_expr(conj(Goals0), _, conj(Goals)) -->
	list__map_foldl(transform_aditi_builtins_in_goal, Goals0, Goals).
transform_aditi_builtins_in_goal_expr(disj(Goals0), _, disj(Goals)) -->
	list__map_foldl(transform_aditi_builtins_in_goal, Goals0, Goals).
transform_aditi_builtins_in_goal_expr(par_conj(Goals0), _, par_conj(Goals)) -->
	list__map_foldl(transform_aditi_builtins_in_goal, Goals0, Goals).
transform_aditi_builtins_in_goal_expr(
		if_then_else(Vars, Cond0, Then0, Else0),
		_, if_then_else(Vars, Cond, Then, Else)) -->
	transform_aditi_builtins_in_goal(Cond0, Cond),
	transform_aditi_builtins_in_goal(Then0, Then),
	transform_aditi_builtins_in_goal(Else0, Else).
transform_aditi_builtins_in_goal_expr(switch(Var, CanFail, Cases0), _,
		switch(Var, CanFail, Cases)) -->
	list__map_foldl(
		(pred(Case0::in, Case::out, in, out) is det -->
			{ Case0 = case(ConsId, Goal0) },
			transform_aditi_builtins_in_goal(Goal0, Goal),
			{ Case = case(ConsId, Goal) }
		), Cases0, Cases).
transform_aditi_builtins_in_goal_expr(not(Goal0), _, not(Goal)) -->
	transform_aditi_builtins_in_goal(Goal0, Goal).
transform_aditi_builtins_in_goal_expr(some(A, B, Goal0), _,
		some(A, B, Goal)) -->
	transform_aditi_builtins_in_goal(Goal0, Goal).
transform_aditi_builtins_in_goal_expr(Goal, _, Goal) -->
	{ Goal = foreign_proc(_, _, _, _, _, _) }.
transform_aditi_builtins_in_goal_expr(Goal, _, Goal) -->
	{ Goal = call(_, _, _, _, _, _) }.
transform_aditi_builtins_in_goal_expr(shorthand(_), _, _) -->
	{ error("transform_aditi_builtins_in_goal_expr: shorthand") }.

:- pred transform_aditi_bottom_up_closure(prog_var, pred_id, proc_id,
		list(prog_var), hlds_goal_info, hlds_goal_expr,
		aditi_transform_info, aditi_transform_info).
:- mode transform_aditi_bottom_up_closure(in, in, in, in, in,
		out, in, out) is det.

transform_aditi_bottom_up_closure(Var, PredId, ProcId, Args,
		_GoalInfo, Goal) -->
	%
	% Transform a closure
	%
	% Var = (aditi_bottom_up pred(DB::aditi_mui, X::out, ...) is nondet :-
	%		p(NonLocal1, NonLocal2, ..., DB, X, ...)
	%	)
	%
	% into
	%
	% ProcName = "<RL proc name for p>",
	% InputSchema = "<RL input schema for p>",
	% InputTuple = <tuple of InputArgs>,
	% TypeInfo = <type-info for InputTuple>,
	% NewVar = (pred(DB::aditi_ui, Relation::out) is det :-
	%	aditi_private_builtin__do_call_returning_relation(
	%		TypeInfo, ProcName, InputSchema, InputTuple, DB,
	%		Relation)
	%	),
	% unsafe_cast(NewVar, Var).
	%
	% The code to transform bulk updates below will cast the closure
	% back to its real type (pred(c_pointer::out) is det) before
	% passing it to `aditi_private_builtin__do_bulk_*'.
	%

	create_bulk_update_closure_var(NewVar),
	ProcInfo =^ proc_info,
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__apply_to_list(Args, VarTypes, InputTupleTypes) },

	ModuleInfo0 =^ module_info,
	{ lookup_builtin_pred_proc_id(ModuleInfo0,
		aditi_private_builtin_module, "do_call_returning_relation",
		predicate, 6, only_mode, BuiltinPredId, BuiltinProcId) },

	%
	% Build the input arguments describing the procedure to call.
	%
	{ rl__get_c_interface_rl_proc_name(ModuleInfo0,
			proc(PredId, ProcId), RLProcName) },
	{ rl__proc_name_to_string(RLProcName, RLProcNameStr) },
	{ rl__schema_to_string(ModuleInfo0, InputTupleTypes, InputSchema) },
	{ ConstArgs = [string(RLProcNameStr), string(InputSchema)] },
	generate_const_args(ConstArgs, ConstArgVars, ConstArgGoals),

	%
	% Build the vectors of input arguments and type-infos.
	%
	handle_input_tuple(Args, InputTupleVar, InputTupleTypeInfo,
		TupleGoals),

	{ BuiltinArgs = [InputTupleTypeInfo | ConstArgVars] ++
				[InputTupleVar] },
	{ list__length(BuiltinArgs, NumBuiltinArgs) },

	{ Functor = cons(qualified(aditi_private_builtin_module,
			"do_call_returning_relation"), NumBuiltinArgs) },
	{ Rhs = functor(Functor, no, BuiltinArgs) },

	{ CastInputInst = ground(shared,
			higher_order(pred_inst_info(predicate,
				[aditi_ui_mode, out_mode], det))) },
	{ UniMode = ((free_inst - CastInputInst) ->
			(CastInputInst - CastInputInst)) },
	{ list__duplicate(NumBuiltinArgs, UniMode, UniModes) },
	{ ShroudedPredProcId = shroud_pred_proc_id(
		proc(BuiltinPredId, BuiltinProcId)) },
	{ BuiltinConsId = pred_const(ShroudedPredProcId, normal) },
	{ Unification = construct(NewVar, BuiltinConsId, BuiltinArgs,
			UniModes, construct_dynamically,
			cell_is_unique, no) },
	{ set__list_to_set([NewVar | BuiltinArgs], NonLocals) },
	{ instmap_delta_from_assoc_list([NewVar - ground_inst],
		InstMapDelta) },
	{ goal_info_init(NonLocals, InstMapDelta, det, pure, GoalInfo) },
	{ UnifyMode = out_mode - in_mode },
	{ UnifyContext = unify_context(explicit, []) },
	{ UnifyGoal = unify(NewVar, Rhs, UnifyMode,
			Unification, UnifyContext) - GoalInfo },

	%
	% Change the import_status to exported, so dead_proc_elim.m
	% will not attempt to remove the predicate after the reference
	% to it is transformed away here.
	%
	ModuleInfo1 =^ module_info,
	{ module_info_pred_proc_info(ModuleInfo1, PredId, ProcId,
		CalleePredInfo0, CalleeProcInfo) },
	{ pred_info_import_status(CalleePredInfo0, CalleeStatus) },
	(
		{ status_defined_in_this_module(CalleeStatus, yes) },
		\+ { hlds_pred__pred_info_is_base_relation(CalleePredInfo0) }
	->
		{ pred_info_set_import_status(exported,
			CalleePredInfo0, CalleePredInfo) },
		{ module_info_set_pred_info(PredId, CalleePredInfo,
			ModuleInfo1, ModuleInfo) },
		^ module_info := ModuleInfo
	;
		{ CalleePredInfo = CalleePredInfo0 }
	),

	%
	% Cast the closure to the type and inst of the original closure,
	% so that the HLDS is still type and mode correct.
	%
	{ CalleePredOrFunc = pred_info_is_pred_or_func(CalleePredInfo) },
	{ proc_info_argmodes(CalleeProcInfo, CalleeArgModes) },
	{
		list__drop(list__length(Args), CalleeArgModes,
			NonCurriedArgModes0)
	->
		NonCurriedArgModes = NonCurriedArgModes0
	;
		error("transform_aditi_bottom_up_closure: list__drop failed")
	},
	{ proc_info_interface_determinism(CalleeProcInfo, CalleeDetism) },
	{ CastOutputInst = ground(shared,
			higher_order(pred_inst_info(CalleePredOrFunc,
				NonCurriedArgModes, CalleeDetism))) },
	{ CastModes = [(CastInputInst -> CastInputInst),
			(free_inst -> CastOutputInst)] },
	{ CastGoal = generic_call(unsafe_cast, [NewVar, Var],
			CastModes, det) - GoalInfo },

	{ Goals = list__condense([ConstArgGoals, TupleGoals,
				[UnifyGoal, CastGoal]]) },
	{ Goal = conj(Goals) }.

:- pred transform_aditi_builtin(aditi_builtin, list(prog_var),
		list(mode), determinism, hlds_goal_info, hlds_goal_expr,
		aditi_transform_info, aditi_transform_info).
:- mode transform_aditi_builtin(in, in, in, in, in, out, in, out) is det.

transform_aditi_builtin(Builtin, Args, Modes, Det, GoalInfo, Goal) -->
	ModuleInfo =^ module_info,
	{ aditi_builtin_info(ModuleInfo, Builtin,
		BuiltinProcName, BuiltinProcArity, ConstArgs) },
	{ lookup_builtin_pred_proc_id(ModuleInfo, aditi_private_builtin_module,
		BuiltinProcName, predicate, BuiltinProcArity, only_mode,
		PredId, ProcId) },
	generate_const_args(ConstArgs, ConstArgVars, ConstArgGoals),
	transform_aditi_builtin_2(Builtin, Args, Modes, Det,
		GoalInfo, proc(PredId, ProcId),
		qualified(aditi_private_builtin_module, BuiltinProcName),
		ConstArgVars, CallGoals),
	{ list__append(ConstArgGoals, CallGoals, Goals) },
	{ conj_list_to_goal(Goals, GoalInfo, Goal - _) }.


:- pred transform_aditi_builtin_2(aditi_builtin, list(prog_var),
		list(mode), determinism, hlds_goal_info, pred_proc_id,
		sym_name, list(prog_var), list(hlds_goal),
		aditi_transform_info, aditi_transform_info).
:- mode transform_aditi_builtin_2(in, in, in, in, in, in, in, in,
		out, in, out) is det.

transform_aditi_builtin_2(aditi_tuple_update(_, _),
		Args0, _Modes0, _Det, _GoalInfo, BuiltinPredProcId,
		BuiltinSymName, ConstArgs, Goals) -->

	{ get_state_args_det(Args0, TupleArgs0, State0Arg, StateArg) },
	ProcInfo =^ proc_info,
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__apply_to_list(TupleArgs0, VarTypes, TupleTypes0) },

	% Remove the `aditi__state' from the list of arguments.
	{ type_util__remove_aditi_state(TupleTypes0, TupleArgs0, TupleArgs) },

	%
	% Produce code to create the vectors of type-infos and arguments
	% for the tuple.
	%
	handle_input_tuple(TupleArgs, InputTuple,
		InputTupleTypeInfo, TupleGoals),

	% Produce the call
	% aditi_private_builtin__do_insert_tuple(InputTupleTypeInfo,
	%	RelationName, InputTuple, State0, State)
	%
	% or
	%
	% aditi_private_builtin__do_insert_tuple(InputTupleTypeInfo,
	%	RelationName, DeleteProc, DeleteProcInputSchema,
	%	InputTuple, State0, State)
	%
	{ list__append([InputTupleTypeInfo | ConstArgs],
		[InputTuple, State0Arg, StateArg], CallArgs) },
	{ BuiltinPredProcId = proc(BuiltinPredId, BuiltinProcId) },

	{ set__list_to_set(CallArgs, NonLocals) },
	{ instmap_delta_from_assoc_list([StateArg - ground_inst],
		InstMapDelta) },
	{ goal_info_init(NonLocals, InstMapDelta, det, pure, CallGoalInfo) },
	{ CallGoal = call(BuiltinPredId, BuiltinProcId, CallArgs,
			not_builtin, no, BuiltinSymName) - CallGoalInfo },

	{ Goals = list__append(TupleGoals, [CallGoal]) }.

transform_aditi_builtin_2(
		aditi_bulk_update(Op, PredId, _),
		Args, _Modes, _Det, _GoalInfo, BuiltinPredProcId,
		BuiltinSymName, ConstArgs, Goals) -->

	{ Args = [Closure0, State0Arg0, StateArg0] ->
		Closure = Closure0,
		State0Arg = State0Arg0,
		StateArg = StateArg0
	;
		error(
	"transform_aditi_builtin_2: wrong number of arguments in bulk update")
	},

	create_bulk_update_closure_var(CastClosure),

	%
	% Cast the closure to the type expected by the predicate
	% to perform the update in aditi_private_builtin.m
	%
	ModuleInfo =^ module_info,
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ PredOrFunc = pred_info_is_pred_or_func(PredInfo) },
	{ list__length(ArgTypes, PredArity) },
	{
		Op = bulk_delete,
		ClosurePredOrFunc = PredOrFunc,
		ClosureArity = PredArity
	;
		Op = bulk_insert,
		ClosurePredOrFunc = PredOrFunc,
		ClosureArity = PredArity
	;
		Op = bulk_modify,
		ClosurePredOrFunc = predicate,
		ClosureArity = PredArity * 2
	},
	{ list__duplicate(ClosureArity, out_mode, ClosureModes) },

	{ CastInputInst = ground(shared,
		higher_order(pred_inst_info(ClosurePredOrFunc,
			ClosureModes, nondet))) },
	{ CastOutputInst = ground(shared,
		higher_order(pred_inst_info(predicate,
			[aditi_ui_mode, out_mode], det))) },
	{ CastModes = [(CastInputInst -> CastInputInst),
			(free_inst -> CastOutputInst)] },

	{ set__list_to_set([Closure, CastClosure], NonLocals) },
	{ instmap_delta_from_assoc_list([CastClosure - CastOutputInst],
		InstMapDelta) },
	{ goal_info_init(NonLocals, InstMapDelta, det, pure, GoalInfo) },
	{ CastGoal = generic_call(unsafe_cast, [Closure, CastClosure],
			CastModes, det) - GoalInfo },

	% Produce the call
	% aditi_private_builtin__do_bulk_*(RelationName, UpdateProc,
	%	Closure, State0, State)
	%

	{ list__append(ConstArgs, [CastClosure, State0Arg, StateArg],
		CallArgs) },

	{ BuiltinPredProcId = proc(BuiltinPredId, BuiltinProcId) },
	{ CallGoal = call(BuiltinPredId, BuiltinProcId, CallArgs, not_builtin,
				no, BuiltinSymName) - GoalInfo },

	{ Goals = [CastGoal, CallGoal] }.

create_aditi_call_proc(PredProcId, ModuleInfo0, ModuleInfo) :-
	module_info_pred_proc_info(ModuleInfo0, PredProcId,
		PredInfo0, ProcInfo0),
	rl__get_c_interface_rl_proc_name(ModuleInfo0, PredProcId, ProcName),
	rl__proc_name_to_string(ProcName, ProcNameStr),
	Changed = no,
	Info0 = aditi_transform_info(ModuleInfo0,
			PredInfo0, ProcInfo0, Changed),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_argmodes(ProcInfo0, ArgModes),
	proc_info_interface_determinism(ProcInfo0, Det),
	create_aditi_call_goal(ProcNameStr, HeadVars, ArgModes, Det, Goal,
		Info0, Info),
	Info = aditi_transform_info(ModuleInfo1, PredInfo, ProcInfo1, _),
	proc_info_set_goal(Goal, ProcInfo1, ProcInfo2),
	requantify_proc(ProcInfo2, ProcInfo3),
	recompute_instmap_delta_proc(yes, ProcInfo3, ProcInfo,
		ModuleInfo1, ModuleInfo2),
	module_info_set_pred_proc_info(PredProcId, PredInfo, ProcInfo,
		ModuleInfo2, ModuleInfo).

	%
	% Produce the call
	% aditi_private_builtin__do_*_call(InputTupleTypeInfo,
	%	OutputTupleTypeInfo, ProcName, NumInputs, InputSchema,
	%	NumOutputs, InputTuple, OutputTuple).
	%
:- pred create_aditi_call_goal(string, list(prog_var), list(mode), determinism,
	hlds_goal, aditi_transform_info, aditi_transform_info).
:- mode create_aditi_call_goal(in, in, in, in, out, in, out) is det.

create_aditi_call_goal(ProcName, HeadVars0, ArgModes0, Det, Goal) -->
	ProcInfo0 =^ proc_info,
	ModuleInfo0 =^ module_info,

	{ Det = det, Proc = "do_det_call"
	; Det = semidet, Proc = "do_semidet_call"
	; Det = nondet, Proc = "do_nondet_call"
	; Det = multidet, Proc = "do_multi_call"
	; Det = cc_nondet, Proc = "do_cc_nondet_call"
	; Det = cc_multidet, Proc = "do_cc_multi_call"
	; Det = failure, Proc = "do_failure_call"
	; Det = erroneous, Proc = "do_erroneous_call"
	},
	{ lookup_builtin_pred_proc_id(ModuleInfo0,
		aditi_private_builtin_module, Proc, predicate, 5, only_mode,
		BuiltinPredId, BuiltinProcId) },
	{ BuiltinSymName = qualified(aditi_private_builtin_module, Proc) },

	{ proc_info_vartypes(ProcInfo0, VarTypes) },
	{ map__apply_to_list(HeadVars0, VarTypes, ArgTypes0) },

	%
	% Find the aditi__state argument.
	%
	{ TypesVarsAL = assoc_list__from_corresponding_lists(ArgTypes0,
				HeadVars0) },
	{ list__filter_map(
		(pred(Type - Var::in, Var::out) is semidet :-
			type_is_aditi_state(Type)
		), TypesVarsAL, AditiStateVars) },
	{ AditiStateVars = [FirstStateVar | _] ->
		AditiStateVar = FirstStateVar
	;
		% post_typecheck.m ensures that all Aditi predicates
		% have an aditi__state argument.
		error("create_aditi_call_goal: no aditi__state")
	},

	%
	% Dont pass `aditi__state' arguments to Aditi -- they do not appear
	% as attributes in Aditi relations.
	%
	{ type_util__remove_aditi_state(ArgTypes0, ArgModes0, ArgModes) },
	{ type_util__remove_aditi_state(ArgTypes0, HeadVars0, HeadVars) },

	%
	% Generate arguments to describe the procedure to call.
	%
	{ partition_args(ModuleInfo0, ArgModes, HeadVars,
		InputArgs, OutputArgs) },
	{ map__apply_to_list(InputArgs, VarTypes, InputTypes) },
	{ rl__schema_to_string(ModuleInfo0, InputTypes, InputSchema) },
	{ ConstArgs = [string(ProcName), string(InputSchema)] },
	generate_const_args(ConstArgs, ConstArgVars, ConstArgGoals),

	%
	% Build a tuple containing the input arguments.
	%
	handle_input_tuple(InputArgs, InputTupleVar, InputTupleTypeInfo,
		InputTupleGoals),

	%
	% Build a goal to deconstruct the vector of output arguments.
	%
	make_tuple_var(OutputArgs, OutputTupleVar,
		OutputTupleTypeInfo, OutputTypeInfoGoals),
	{ deconstruct_tuple(OutputTupleVar, OutputArgs, OutputGoal) },

	%
	% Build the call.
	%
	{ CallArgs =
		[InputTupleTypeInfo, OutputTupleTypeInfo, AditiStateVar
			| ConstArgVars] ++
		[InputTupleVar, OutputTupleVar] },
	{ set__list_to_set(CallArgs, NonLocals) },
	{ determinism_components(Det, _, at_most_zero) ->
		instmap_delta_init_unreachable(InstMapDelta)
	;
		instmap_delta_from_assoc_list([OutputTupleVar - ground_inst],
			InstMapDelta)
	},
	{ goal_info_init(NonLocals, InstMapDelta, Det, pure, GoalInfo) },
	{ CallGoal = call(BuiltinPredId, BuiltinProcId, CallArgs,
		not_builtin, no, BuiltinSymName) - GoalInfo },

	{ Goals = list__condense(
			[ConstArgGoals, InputTupleGoals, OutputTypeInfoGoals,
			[CallGoal, OutputGoal]]) },
	{ Goal = conj(Goals) - GoalInfo }.

:- pred make_tuple_var(list(prog_var), prog_var, prog_var, list(hlds_goal),
		aditi_transform_info, aditi_transform_info).
:- mode make_tuple_var(in, out, out, out, in, out) is det.

make_tuple_var(Args, TupleVar, TupleTypeInfo, TupleTypeInfoGoal) -->
	ProcInfo0 =^ proc_info,
	{ proc_info_vartypes(ProcInfo0, VarTypes) },
	{ map__apply_to_list(Args, VarTypes, ArgTypes) },
	{ construct_type(unqualified("{}") - list__length(Args),
		ArgTypes, TupleType) },
	{ proc_info_create_var_from_type(TupleType, no, TupleVar,
		ProcInfo0, ProcInfo) },
	^ proc_info := ProcInfo,
	make_type_info_for_var(TupleVar, TupleTypeInfo, TupleTypeInfoGoal).

:- pred make_type_info_for_var(prog_var, prog_var,
		list(hlds_goal), aditi_transform_info, aditi_transform_info).
:- mode make_type_info_for_var(in, out, out, in, out) is det.

make_type_info_for_var(Var, TypeInfo, Goals) -->
	PredInfo0 =^ pred_info,
	ProcInfo0 =^ proc_info,
	ModuleInfo0 =^ module_info,
	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ map__lookup(VarTypes0, Var, Type) },
	{ create_poly_info(ModuleInfo0, PredInfo0, ProcInfo0, PolyInfo0) },
	{ term__context_init(Context) },
	{ polymorphism__make_type_info_vars([Type], Context, TypeInfos, Goals,
		PolyInfo0, PolyInfo) },
	{
		TypeInfos = [TypeInfo0]
	->
		TypeInfo = TypeInfo0
	;
		error("make_type_infos_for_var")
	},
	{ poly_info_extract(PolyInfo, PredInfo0, PredInfo,
		ProcInfo0, ProcInfo, ModuleInfo) },
	^ pred_info := PredInfo,
	^ proc_info := ProcInfo,
	^ module_info := ModuleInfo.

:- pred handle_input_tuple(list(prog_var), prog_var, prog_var,
		list(hlds_goal), aditi_transform_info, aditi_transform_info).
:- mode handle_input_tuple(in, out, out, out, in, out) is det.

handle_input_tuple(Args, ArgTupleVar, TypeInfoVar, Goals) -->
	make_tuple_var(Args, ArgTupleVar, TypeInfoVar, TypeInfoGoals),
	{ construct_tuple(ArgTupleVar, Args, ArgTupleGoal) },
	{ Goals = [ArgTupleGoal | TypeInfoGoals] }.

	% An argument passed to an Aditi builtin to describe the
	% predicate being called or updated.
:- type const_arg
	--->	int(int)
	;	string(string)
	.

:- pred generate_const_args(list(const_arg), list(prog_var), list(hlds_goal),
		aditi_transform_info, aditi_transform_info).
:- mode generate_const_args(in, out, out, in, out) is det.

generate_const_args([], [], []) --> [].
generate_const_args([ConstArg | ConstArgs], [ConstArgVar | ConstArgVars],
		[ConstGoal | ConstGoals]) -->
	ProcInfo0 =^ proc_info,
	{
		ConstArg = int(Int),
		make_int_const_construction(Int, no, ConstGoal, ConstArgVar,
			ProcInfo0, ProcInfo)
	;
		ConstArg = string(String),
		make_string_const_construction(String, no, ConstGoal,
			ConstArgVar, ProcInfo0, ProcInfo)
	},
	^ proc_info := ProcInfo,
	generate_const_args(ConstArgs, ConstArgVars, ConstGoals).

	% Create a variable for the query closure passed to a bulk update.
	% The closure returns a reference to the answer relation for the
	% query, rather than returning the answer tuples non-deterministically.
:- pred create_bulk_update_closure_var(prog_var,
		aditi_transform_info, aditi_transform_info).
:- mode create_bulk_update_closure_var(out, in, out) is det.

create_bulk_update_closure_var(NewVar, Info0, Info) :-
	construct_type(qualified(aditi_private_builtin_module, "relation") - 0,
		[], RelationType),
	construct_higher_order_pred_type(pure, normal,
		[aditi_state_type, RelationType], PredType),
	proc_info_create_var_from_type(PredType, no, NewVar,
		Info0 ^ proc_info, ProcInfo),
	Info = Info0 ^ proc_info := ProcInfo.

	% Work out which predicate from aditi_private_builtin.m
	% should be called to perform an Aditi builtin, and work
	% out what arguments should be passed to describe the
	% predicate being called or updated.
:- pred aditi_builtin_info(module_info, aditi_builtin,
		string, arity, list(const_arg)).
:- mode aditi_builtin_info(in, in, out, out, out) is det.

aditi_builtin_info(ModuleInfo, aditi_tuple_update(Op, PredId),
		BuiltinProcName, BuiltinProcArity, ConstArgs) :-
	rl__permanent_relation_name(ModuleInfo, PredId, RelName),
	(
		Op = insert,
		BuiltinProcName = "do_insert_tuple",
		BuiltinProcArity = 4,
		UpdateProcArgs = []
	;
		Op = delete,
		BuiltinProcName = "do_delete_tuple",
		BuiltinProcArity = 6,
		rl__get_delete_proc_name(ModuleInfo, PredId, DeleteProcName),
		rl__proc_name_to_string(DeleteProcName, DeleteProcStr),

		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_arg_types(PredInfo, ArgTypes0),
		type_util__remove_aditi_state(ArgTypes0, ArgTypes0, ArgTypes),
		rl__schema_to_string(ModuleInfo, ArgTypes, InputSchema),

		UpdateProcArgs = [string(DeleteProcStr), string(InputSchema)]
	),
	ConstArgs = [string(RelName) | UpdateProcArgs].
aditi_builtin_info(ModuleInfo, aditi_bulk_update(Op, PredId, _),
		BuiltinProcName, BuiltinProcArity, ConstArgs) :-

	rl__permanent_relation_name(ModuleInfo, PredId, RelName),
	(
		Op = bulk_delete,
		BuiltinProcName = "do_bulk_delete",
		rl__get_delete_proc_name(ModuleInfo, PredId, UpdateProcName)
	;
		Op = bulk_insert,
		BuiltinProcName = "do_bulk_insert",
		rl__get_insert_proc_name(ModuleInfo, PredId, UpdateProcName)
	;
		Op = bulk_modify,
		BuiltinProcName = "do_bulk_modify",
		rl__get_modify_proc_name(ModuleInfo, PredId, UpdateProcName)
	),
	BuiltinProcArity = 5,
	rl__proc_name_to_string(UpdateProcName, UpdateProcStr),
	ConstArgs = [string(RelName), string(UpdateProcStr)].

:- pred construct_aditi_transform_info(module_info, pred_id, proc_info,
		aditi_transform_info).
:- mode construct_aditi_transform_info(in, in, in, out) is det.

construct_aditi_transform_info(ModuleInfo, PredId, ProcInfo, Info) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	Changed = no,
	Info = aditi_transform_info(ModuleInfo, PredInfo, ProcInfo, Changed).

:- pred deconstruct_aditi_transform_info(aditi_transform_info, pred_id,
		module_info, proc_info, bool).
:- mode deconstruct_aditi_transform_info(in, in, out, out, out) is det.

deconstruct_aditi_transform_info(Info, PredId,
		ModuleInfo, ProcInfo, Changed) :-
	ModuleInfo0 = Info ^ module_info,
	ProcInfo = Info ^ proc_info,
	PredInfo = Info ^ pred_info,
	Changed = Info ^ changed,
	module_info_set_pred_info(PredId, PredInfo, ModuleInfo0, ModuleInfo).

:- type aditi_transform_info
	---> aditi_transform_info(
		module_info :: module_info,
		pred_info :: pred_info,
		proc_info :: proc_info,
		changed :: bool
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
