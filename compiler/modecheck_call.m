%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modecheck_call.m.
% Main author: fjh.
%
% This file contains the code to modecheck a call.
%
% Check that there is a mode declaration for the predicate which matches
% the current instantiation of the arguments.  (Also handle calls to
% implied modes.)  If the called predicate is one for which we must infer
% the modes, then a new mode for the called predicate whose initial insts
% are the result of normalising the current inst of the arguments.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modecheck_call.
:- interface.

:- import_module hlds_goal, mode_info.
:- import_module term.

:- pred modecheck_call_pred(pred_id, list(var), proc_id, list(var),
				extra_goals, mode_info, mode_info).
:- mode modecheck_call_pred(in, in, out, out, out,
				mode_info_di, mode_info_uo) is det.

:- pred modecheck_higher_order_call(pred_or_func, var, list(var),
				list(type), argument_modes, determinism,
				list(var), extra_goals, mode_info, mode_info).
:- mode modecheck_higher_order_call(in, in, in, out, out, out, out, out,
				mode_info_di, mode_info_uo) is det.

:- pred modecheck_higher_order_pred_call(var, list(var), pred_or_func,
		hlds_goal_info, hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_higher_order_pred_call(in, in, in, in, out,
		mode_info_di, mode_info_uo) is det.

:- pred modecheck_higher_order_func_call(var, list(var), var, hlds_goal_info,
		hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_higher_order_func_call(in, in, in, in, out,
		mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_data, hlds_pred, hlds_data, hlds_module, instmap, (inst).
:- import_module mode_info, mode_debug, modes, mode_util, mode_errors.
:- import_module clause_to_proc, inst_match, make_hlds, inst_util.
:- import_module map, list, bool, std_util, set, io, int, require.

modecheck_higher_order_pred_call(PredVar, Args0, PredOrFunc, GoalInfo0, Goal)
		-->
	mode_checkpoint(enter, "higher-order call"),
	mode_info_set_call_context(higher_order_call(PredOrFunc)),
	=(ModeInfo0),

	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	modecheck_higher_order_call(PredOrFunc, PredVar, Args0,
			Types, Modes, Det, Args, ExtraGoals),

	=(ModeInfo),
	{ Call = higher_order_call(PredVar, Args, Types, Modes, Det,
			PredOrFunc) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0,
			[PredVar | Args0], [PredVar | Args],
			InstMap0, ModeInfo, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order predicate call").

modecheck_higher_order_func_call(FuncVar, Args0, RetVar, GoalInfo0, Goal) -->
	mode_checkpoint(enter, "higher-order function call"),
	mode_info_set_call_context(higher_order_call(function)),

	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },

	{ list__append(Args0, [RetVar], Args1) },
	modecheck_higher_order_call(function, FuncVar, Args1,
			Types, Modes, Det, Args, ExtraGoals),

	=(ModeInfo),
	{ Call = higher_order_call(FuncVar, Args, Types, Modes, Det,
				function) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0,
				[FuncVar | Args1], [FuncVar | Args],
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order function call").

modecheck_higher_order_call(PredOrFunc, PredVar, Args0, Types, Modes, Det, Args,
		ExtraGoals, ModeInfo0, ModeInfo) :-

	mode_info_get_types_of_vars(ModeInfo0, Args0, Types),

	%
	% First, check that `PredVar' has a higher-order pred inst
	% (of the appropriate arity)
	%
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap__lookup_var(InstMap0, PredVar, PredVarInst0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	inst_expand(InstTable0, ModuleInfo0, PredVarInst0, PredVarInst),
	list__length(Args0, Arity),
	(
		PredVarInst = ground(_Uniq, yes(PredInstInfo)),
		PredInstInfo = pred_inst_info(_PredOrFunc, Modes0, Det0),
		Modes0 = argument_modes(ArgInstTable, ArgModes0),
		list__length(ArgModes0, Arity)
	->
		Det = Det0,
		Modes = Modes0,

		%
		% Check that `Args0' have livenesses which match the
		% expected livenesses.
		%
		get_arg_lives(ArgModes0, ArgInstTable, ModuleInfo0,
			ExpectedArgLives),
		modecheck_var_list_is_live(Args0, ExpectedArgLives, 1,
			ModeInfo0, ModeInfo1),

		inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable1),
		list__map(apply_inst_key_sub_mode(Sub), ArgModes0, ArgModes),
		mode_info_set_inst_table(InstTable1, ModeInfo1, ModeInfo2),

		%
		% Check that `Args0' have insts which match the expected
		% initial insts, set their new final insts (introducing
		% extra unifications for implied modes, if necessary),
		% then check that the final insts of the vars match the
		% declared final insts.
		%
		mode_list_get_initial_insts(ArgModes, ModuleInfo0,
			InitialInsts),
		modecheck_var_has_inst_list(Args0, InitialInsts, 1,
					ModeInfo2, ModeInfo3),
		mode_list_get_final_insts(ArgModes, ModuleInfo0, FinalInsts),
		modecheck_set_var_inst_list(Args0, InitialInsts, FinalInsts,
			Args, ExtraGoals, ModeInfo3, ModeInfo4),
		modecheck_var_has_final_inst_list(Args0, FinalInsts, 1,
			ModeInfo4, ModeInfo5),
		( determinism_components(Det, _, at_most_zero) ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo5, ModeInfo)
		;
			ModeInfo = ModeInfo5
		)
	;
		% the error occurred in argument 1, i.e. the pred term
		mode_info_set_call_arg_context(1, ModeInfo0, ModeInfo1),
		set__singleton_set(WaitingVars, PredVar),
		mode_info_error(WaitingVars, mode_error_higher_order_pred_var(
				PredOrFunc, PredVar, PredVarInst, Arity),
				ModeInfo1, ModeInfo),
		inst_table_init(ArgInstTable),
		Modes = argument_modes(ArgInstTable, []),
		Det = erroneous,
		Args = Args0,
		ExtraGoals = no_extra_goals
	).

%-----------------------------------------------------------------------------%

modecheck_call_pred(PredId, ArgVars0, TheProcId, ArgVars, ExtraGoals,
		ModeInfo0, ModeInfo) :-

		% Get the list of different possible modes for the called
		% predicate
	mode_info_get_preds(ModeInfo0, Preds),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	map__lookup(Preds, PredId, PredInfo0),
	maybe_add_default_mode(PredInfo0, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__keys(Procs, ProcIds),
	pred_info_get_marker_list(PredInfo, Markers),

		% In order to give better diagnostics, we handle the
		% cases where there are zero or one modes for the called
		% predicate specially.
	(
		ProcIds = [],
		\+ list__member(request(infer_modes), Markers)
	->
		set__init(WaitingVars),
		mode_info_error(WaitingVars, mode_error_no_mode_decl,
			ModeInfo0, ModeInfo),
		invalid_proc_id(TheProcId),
		ArgVars = ArgVars0,
		ExtraGoals = no_extra_goals
	;
		ProcIds = [ProcId],
		\+ list__member(request(infer_modes), Markers)
	->
		TheProcId = ProcId,
		map__lookup(Procs, ProcId, ProcInfo),
		proc_info_argmodes(ProcInfo, ProcArgModes),
		proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),

		%
		% Check that `ArgsVars0' have livenesses which match the
		% expected livenesses.
		%
		modecheck_var_list_is_live(ArgVars0, ProcArgLives0, 0,
					ModeInfo0, ModeInfo1),

		%
		% Check that `ArgsVars0' have insts which match the expected
		% initial insts, set their new final insts (introducing
		% extra unifications for implied modes, if necessary),
		% then check that the final insts of the vars match the
		% declared final insts.
		%
		ProcArgModes = argument_modes(ArgInstTable, ArgModes0),

		mode_info_get_inst_table(ModeInfo1, InstTable0),
		inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable1),
		list__map(apply_inst_key_sub_mode(Sub), ArgModes0, ArgModes),
		mode_info_set_inst_table(InstTable1, ModeInfo1, ModeInfo2),

		mode_list_get_initial_insts(ArgModes, ModuleInfo,
					InitialInsts),
		modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
					ModeInfo2, ModeInfo3),
		mode_list_get_final_insts(ArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
			ArgVars, ExtraGoals, ModeInfo3, ModeInfo4),
		modecheck_var_has_final_inst_list(ArgVars0, FinalInsts, 1,
			ModeInfo4, ModeInfo5),
		proc_info_never_succeeds(ProcInfo, NeverSucceeds),
		( NeverSucceeds = yes ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo5, ModeInfo)
		;
			ModeInfo = ModeInfo5
		)
	;
			% set the current error list to empty (and
			% save the old one in `OldErrors').  This is so the
			% test for `Errors = []' in call_pred_2 will work.
		mode_info_get_errors(ModeInfo0, OldErrors),
		mode_info_set_errors([], ModeInfo0, ModeInfo1),

		set__init(WaitingVars),
		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars0,
			WaitingVars, TheProcId, ArgVars, ExtraGoals,
			ModeInfo1, ModeInfo2),

			% restore the error list, appending any new error(s)
		mode_info_get_errors(ModeInfo2, NewErrors),
		list__append(OldErrors, NewErrors, Errors),
		mode_info_set_errors(Errors, ModeInfo2, ModeInfo)
	).

:- pred modecheck_call_pred_2(list(proc_id), pred_id, proc_table, list(var),
			set(var), proc_id, list(var), extra_goals,
			mode_info, mode_info).
:- mode modecheck_call_pred_2(in, in, in, in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

modecheck_call_pred_2([], PredId, _Procs, ArgVars, WaitingVars,
		TheProcId, ArgVars, no_extra_goals, ModeInfo0, ModeInfo) :-
	%
	% There were no matching modes.
	% If we're inferring modes for this called predicate, then
	% just insert a new mode declaration which will match.
	% Otherwise, report an error.
	%
	mode_info_get_preds(ModeInfo0, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_get_marker_list(PredInfo, Markers),
	( list__member(request(infer_modes), Markers) ->
		insert_new_mode(PredId, ArgVars, TheProcId,
			ModeInfo0, ModeInfo1),
		% we don't yet know the final insts for the newly created mode
		% of the called predicate, so we set the instmap to unreachable,
		% indicating that we have no information about the modes at this
		% point in the computation.
		instmap__init_unreachable(Instmap),
		mode_info_set_instmap(Instmap, ModeInfo1, ModeInfo)
	;
		invalid_proc_id(TheProcId),	% dummy value
		mode_info_get_instmap(ModeInfo0, InstMap),
		instmap__lookup_vars(ArgVars, InstMap, ArgInsts),
		mode_info_set_call_arg_context(0, ModeInfo0, ModeInfo1),
		mode_info_error(WaitingVars,
			mode_error_no_matching_mode(ArgVars, ArgInsts),
			ModeInfo1, ModeInfo)
	).

modecheck_call_pred_2([ProcId | ProcIds], PredId, Procs, ArgVars0, WaitingVars,
			TheProcId, ArgVars, ExtraGoals, ModeInfo0, ModeInfo) :-

		% find the initial insts and the final livenesses
		% of the arguments for this mode of the called pred
	map__lookup(Procs, ProcId, ProcInfo),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),

	proc_info_argmodes(ProcInfo, argument_modes(ArgInstTable, ArgModes0)),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable1),
	list__map(apply_inst_key_sub_mode(Sub), ArgModes0, ArgModes),
	mode_info_set_inst_table(InstTable1, ModeInfo0, ModeInfo1),

	mode_list_get_initial_insts(ArgModes, ModuleInfo, InitialInsts),
	mode_list_get_final_insts(ArgModes, ModuleInfo, FinalInsts),

		% check whether the livenesses of the args matches their
		% expected liveness
	modecheck_var_list_is_live(ArgVars0, ProcArgLives0, 0,
				ModeInfo1, ModeInfo2),

		% check whether the insts of the args matches their expected
		% initial insts
	modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
				ModeInfo2, ModeInfo3),
	modecheck_var_has_final_inst_list(ArgVars0, FinalInsts, 1,
				ModeInfo3, ModeInfo4),

	mode_info_get_errors(ModeInfo4, Errors),
	(
			% if error(s) occured, keep trying with the other modes
			% for the called pred
		Errors = [FirstError | _]
	->
		FirstError = mode_error_info(WaitingVars2, _, _, _),
		set__union(WaitingVars, WaitingVars2, WaitingVars3),
		mode_info_set_errors([], ModeInfo4, ModeInfo5),
		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars0,
				WaitingVars3, TheProcId, ArgVars, ExtraGoals,
				ModeInfo5, ModeInfo)
	;
			% if there are no errors, then set their insts to the
			% final insts specified in the mode for the called pred
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
				ArgVars, ExtraGoals, ModeInfo4, ModeInfo5),
		TheProcId = ProcId,
		proc_info_never_succeeds(ProcInfo, NeverSucceeds),
		( NeverSucceeds = yes ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo5, ModeInfo)
		;
			ModeInfo = ModeInfo5
		)
	).

%-----------------------------------------------------------------------------%

:- pred modecheck_var_has_final_inst_list(list(var), list(inst), int,
		mode_info, mode_info).
:- mode modecheck_var_has_final_inst_list(in, in, in,
		mode_info_di, mode_info_uo) is det.

modecheck_var_has_final_inst_list([_|_], [], _) -->
        { error("modecheck_var_has_final_inst_list: length mismatch") }.
modecheck_var_has_final_inst_list([], [_|_], _) -->
        { error("modecheck_var_has_final_inst_list: length mismatch") }.
modecheck_var_has_final_inst_list([], [], _ArgNum) --> [].
modecheck_var_has_final_inst_list([Var|Vars], [Inst|Insts], ArgNum0) -->
        { ArgNum is ArgNum0 + 1 },
        mode_info_set_call_arg_context(ArgNum),
        modecheck_var_has_final_inst(Var, Inst),
        modecheck_var_has_final_inst_list(Vars, Insts, ArgNum).

:- pred modecheck_var_has_final_inst(var, inst, mode_info, mode_info).
:- mode modecheck_var_has_final_inst(in, in, mode_info_di, mode_info_uo) is det.

modecheck_var_has_final_inst(_VarId, _Inst, ModeInfo0, ModeInfo) :-
	% XXX To be filled in.
	ModeInfo0 = ModeInfo.

%-----------------------------------------------------------------------------%

:- pred insert_new_mode(pred_id, list(var), proc_id, mode_info, mode_info).
:- mode insert_new_mode(in, in, out, mode_info_di, mode_info_uo) is det.

	% Insert a new inferred mode for a predicate.
	% The initial insts are determined by using a normalised
	% version of the call pattern (i.e. the insts of the arg vars).
	% The final insts are initially just assumed to be all `not_reached'.
	% The determinism for this mode will be inferred.

insert_new_mode(PredId, ArgVars, ProcId, ModeInfo0, ModeInfo) :-
	% figure out the values of all the variables we need to
	% create a new mode for this predicate
	get_var_insts_and_lives(ArgVars, ModeInfo0, InitialInsts, ArgLives),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_context(PredInfo0, Context),
	list__length(ArgVars, Arity),
	list__duplicate(Arity, not_reached, FinalInsts),
	inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes),
	MaybeDeterminism = no,

	% create the new mode
	inst_table_init(ArgInstTable),
	add_new_proc(PredInfo0, Arity, argument_modes(ArgInstTable, Modes), no,
		yes(ArgLives), MaybeDeterminism, Context, PredInfo1, ProcId),

	% copy the clauses for the predicate to this procedure,
	% and then store the new proc_info and pred_info
	% back in the module_info.

	pred_info_procedures(PredInfo1, Procs1),
	map__lookup(Procs1, ProcId, ProcInfo1),
	pred_info_clauses_info(PredInfo1, ClausesInfo),

	copy_clauses_to_proc(ProcId, ClausesInfo, ProcInfo1, ProcInfo),

	map__det_update(Procs1, ProcId, ProcInfo, Procs),
	pred_info_set_procedures(PredInfo1, Procs, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo),
	mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),

	% Since we've created a new inferred mode for this predicate,
	% things have changed, so we will need to do at least one more
	% pass of the fixpoint analysis.
	mode_info_set_changed_flag(yes, ModeInfo1, ModeInfo).

:- pred get_var_insts_and_lives(list(var), mode_info,
				list(inst), list(is_live)).
:- mode get_var_insts_and_lives(in, mode_info_ui, out, out) is det.

get_var_insts_and_lives([], _, [], []).
get_var_insts_and_lives([Var | Vars], ModeInfo,
		[Inst | Insts], [IsLive | IsLives]) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_instmap(ModeInfo, InstMap),
	mode_info_get_inst_table(ModeInfo, InstTable),
	instmap__lookup_var(InstMap, Var, Inst0),
	normalise_inst(Inst0, InstTable, ModuleInfo, Inst),

	mode_info_var_is_live(ModeInfo, Var, IsLive0),

	( IsLive0 = live ->
		IsLive = live
	;
		% To reduce the potentially exponential explosion in the
		% number of modes, we only set IsLive to `dead' - meaning
		% that the procedure requires its argument to be dead, so
		% that it can do destructive update - if there really is
		% a good chance of being able to do destructive update.
		(
			inst_is_ground(Inst, InstTable, ModuleInfo),
			inst_is_unique(Inst, InstTable, ModuleInfo)
		->
			IsLive = dead
		;
			IsLive = live
		)
	),

	get_var_insts_and_lives(Vars, ModeInfo, Insts, IsLives).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
