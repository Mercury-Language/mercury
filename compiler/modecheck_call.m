%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
				pair(list(hlds_goal)), mode_info, mode_info).
:- mode modecheck_call_pred(in, in, out, out, out,
				mode_info_di, mode_info_uo) is det.

:- pred modecheck_higher_order_call(pred_or_func, var, list(var),
				list(type), list(mode), determinism, list(var),
				pair(list(hlds_goal)), mode_info, mode_info).
:- mode modecheck_higher_order_call(in, in, in, out, out, out, out, out,
				mode_info_di, mode_info_uo) is det.

:- pred modecheck_higher_order_pred_call(var, list(var), hlds_goal_info,
		hlds_goal_expr, mode_info, mode_info).
:- mode modecheck_higher_order_pred_call(in, in, in, out,
		mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_data, hlds_pred, hlds_data, hlds_module, instmap.
:- import_module mode_info, mode_debug, modes, mode_util, mode_errors.
:- import_module clause_to_proc, inst_match, make_hlds.
:- import_module map, list, bool, std_util, set.

modecheck_higher_order_pred_call(PredVar, Args0, GoalInfo0, Goal) -->
	mode_checkpoint(enter, "higher-order predicate call"),
	mode_info_set_call_context(higher_order_call(predicate)),
	=(ModeInfo0),

	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	modecheck_higher_order_call(predicate, PredVar, Args0,
			Types, Modes, Det, Args, ExtraGoals),

	=(ModeInfo),
	{ Call = higher_order_call(PredVar, Args, Types, Modes, Det) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0,
			[PredVar | Args0], [PredVar | Args],
			InstMap0, ModeInfo, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order predicate call").

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
	inst_expand(ModuleInfo0, PredVarInst0, PredVarInst),
	list__length(Args0, Arity),
	(
		PredVarInst = ground(_Uniq, yes(PredInstInfo)),
		PredInstInfo = pred_inst_info(PredOrFunc, Modes0, Det0),
		list__length(Modes0, Arity)
	->
		Det = Det0,
		Modes = Modes0,

		%
		% Check that `Args0' have livenesses which match the
		% expected livenesses.
		%
		get_arg_lives(Modes, ModuleInfo0, ExpectedArgLives),
		modecheck_var_list_is_live(Args0, ExpectedArgLives, 1,
			ModeInfo0, ModeInfo1),

		%
		% Check that `Args0' have insts which match the expected
		% initial insts, and set their new final insts (introducing
		% extra unifications for implied modes, if necessary).
		%
		mode_list_get_initial_insts(Modes, ModuleInfo0, InitialInsts),
		modecheck_var_has_inst_list(Args0, InitialInsts, 1,
					ModeInfo1, ModeInfo2),
		mode_list_get_final_insts(Modes, ModuleInfo0, FinalInsts),
		modecheck_set_var_inst_list(Args0, InitialInsts, FinalInsts,
			Args, ExtraGoals, ModeInfo2, ModeInfo3),
		( determinism_components(Det, _, at_most_zero) ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo3, ModeInfo)
		;
			ModeInfo = ModeInfo3
		)
	;
		% the error occurred in argument 1, i.e. the pred term
		mode_info_set_call_arg_context(1, ModeInfo0, ModeInfo1),
		set__singleton_set(WaitingVars, PredVar),
		mode_info_error(WaitingVars, mode_error_higher_order_pred_var(
				PredOrFunc, PredVar, PredVarInst, Arity),
				ModeInfo1, ModeInfo),
		Modes = [],
		Det = erroneous,
		Args = Args0,
		ExtraGoals = [] - []
	).

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
		ExtraGoals = [] - []
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
		% initial insts, and set their new final insts (introducing
		% extra unifications for implied modes, if necessary).
		%
		mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
					InitialInsts),
		modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
					ModeInfo1, ModeInfo2),
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
			ArgVars, ExtraGoals, ModeInfo2, ModeInfo3),
		proc_info_never_succeeds(ProcInfo, NeverSucceeds),
		( NeverSucceeds = yes ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo3, ModeInfo)
		;
			ModeInfo = ModeInfo3
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
			set(var), proc_id, list(var), pair(list(hlds_goal)),
			mode_info, mode_info).
:- mode modecheck_call_pred_2(in, in, in, in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

modecheck_call_pred_2([], PredId, _Procs, ArgVars, WaitingVars,
			TheProcId, ArgVars, [] - [], ModeInfo0, ModeInfo) :-
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
	proc_info_argmodes(ProcInfo, ProcArgModes),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),

		% check whether the livenesses of the args matches their
		% expected liveness
	modecheck_var_list_is_live(ArgVars0, ProcArgLives0, 0,
				ModeInfo0, ModeInfo1),

		% check whether the insts of the args matches their expected
		% initial insts
	modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
				ModeInfo1, ModeInfo2),

	mode_info_get_errors(ModeInfo2, Errors),
	(
			% if error(s) occured, keep trying with the other modes
			% for the called pred
		Errors = [FirstError | _]
	->
		FirstError = mode_error_info(WaitingVars2, _, _, _),
		set__union(WaitingVars, WaitingVars2, WaitingVars3),
		mode_info_set_errors([], ModeInfo2, ModeInfo3),
		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars0,
				WaitingVars3, TheProcId, ArgVars, ExtraGoals,
				ModeInfo3, ModeInfo)
	;
			% if there are no errors, then set their insts to the
			% final insts specified in the mode for the called pred
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
				ArgVars, ExtraGoals, ModeInfo2, ModeInfo3),
		TheProcId = ProcId,
		proc_info_never_succeeds(ProcInfo, NeverSucceeds),
		( NeverSucceeds = yes ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo3, ModeInfo)
		;
			ModeInfo = ModeInfo3
		)
	).

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
	add_new_proc(PredInfo0, Arity, Modes, no, yes(ArgLives),
		MaybeDeterminism, Context, PredInfo1, ProcId),

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
	instmap__lookup_var(InstMap, Var, Inst0),
	normalise_inst(Inst0, ModuleInfo, Inst),

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
			inst_is_ground(ModuleInfo, Inst),
			inst_is_unique(ModuleInfo, Inst)
		->
			IsLive = dead
		;
			IsLive = live
		)
	),

	get_var_insts_and_lives(Vars, ModeInfo, Insts, IsLives).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
