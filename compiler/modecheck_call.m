%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001 The University of Melbourne.
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

:- import_module hlds_goal, hlds_pred, hlds_module.
:- import_module prog_data, modes, mode_info.
:- import_module list, std_util.

:- pred modecheck_call_pred(pred_id, proc_id, list(prog_var),
		maybe(determinism), proc_id, list(prog_var),
		extra_goals, mode_info, mode_info).
:- mode modecheck_call_pred(in, in, in, in, out, out, out,
		mode_info_di, mode_info_uo) is det.

:- pred modecheck_higher_order_call(pred_or_func, prog_var, list(prog_var),
		list(mode), determinism, list(prog_var),
		extra_goals, mode_info, mode_info).
:- mode modecheck_higher_order_call(in, in, in, out, out, out, out,
		mode_info_di, mode_info_uo) is det.

:- pred modecheck_aditi_builtin(aditi_builtin, simple_call_id,
		list(prog_var), list(mode), determinism,
		list(prog_var), extra_goals, mode_info, mode_info).
:- mode modecheck_aditi_builtin(in, in, in, in, out, out, out,
		mode_info_di, mode_info_uo) is det.

	%
	% Given two modes of a predicate, figure out whether
	% they are indistinguishable; that is, whether any valid call to
	% one mode would also be a valid call to the other.
	% (If so, it is a mode error.)
	% Note that mode declarations which only have different final insts
	% do not count as distinguishable.
	%
:- pred modes_are_indistinguishable(proc_id, proc_id, pred_info, module_info).
:- mode modes_are_indistinguishable(in, in, in, in) is semidet.

	%
	% Given two modes of a predicate, figure out whether
	% they are identical, except that one is cc_nondet/cc_multi
	% and the other is nondet/multi.
	% This is used by determinism analysis to substitute
	% a multi mode for a cc_multi one if the call occurs in a
	% non-cc context.
	%
:- pred modes_are_identical_bar_cc(proc_id, proc_id, pred_info, module_info).
:- mode modes_are_identical_bar_cc(in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module hlds_data, instmap, prog_data, (inst), inst_util, type_util.
:- import_module mode_info, mode_debug, modes, mode_util, mode_errors.
:- import_module clause_to_proc, inst_match.
:- import_module det_report, unify_proc.
:- import_module int, map, bool, set, require, term, varset.

modecheck_higher_order_call(PredOrFunc, PredVar, Args0, Modes, Det,
		Args, ExtraGoals, ModeInfo0, ModeInfo) :-
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
		PredVarInst = ground(_Uniq, GroundInstInfo),
		(
			GroundInstInfo = higher_order(PredInstInfo)
		;
			% If PredVar has no higher-order inst
			% information, but is a function type, then
			% assume the default function mode.
			GroundInstInfo = none,
			mode_info_get_var_types(ModeInfo0, VarTypes),
			map__lookup(VarTypes, PredVar, Type),
			type_is_higher_order(Type, function, _, ArgTypes),
			PredInstInfo = pred_inst_info_standard_func_mode(
					list__length(ArgTypes))
		),
		PredInstInfo = pred_inst_info(PredOrFunc, Modes0, Det0),
		list__length(Modes0, Arity)
	->
		Det = Det0,
		Modes = Modes0,
		ArgOffset = 1,
		modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args,
			ModeInfo0, ModeInfo1),

		( determinism_components(Det, _, at_most_zero) ->
			instmap__init_unreachable(Instmap),
			mode_info_set_instmap(Instmap, ModeInfo1, ModeInfo)
		;
			ModeInfo = ModeInfo1
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
		ExtraGoals = no_extra_goals
	).

modecheck_aditi_builtin(AditiBuiltin, CallId,
		Args0, Modes, Det, Args, ExtraGoals) -->
	{ aditi_builtin_determinism(AditiBuiltin, Det) },

	% `aditi_insert' and `aditi_delete' goals have type_info
	% arguments for each of the arguments of the tuple to
	% insert added to the start of the argument list by polymorphism.m.
	( { AditiBuiltin = aditi_tuple_insert_delete(_, _) } ->
		{ CallId = _ - _/Arity },
		{ ArgOffset = -Arity }
	;
		{ ArgOffset = 0 }
	),

	% The argument modes are set by post_typecheck.m, so all
	% that needs to be done here is to check that they match.
	modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args).

:- pred aditi_builtin_determinism(aditi_builtin, determinism).
:- mode aditi_builtin_determinism(in, out) is det.

aditi_builtin_determinism(aditi_call(_, _, _, _), _) :-
	error(
	"modecheck_call__aditi_builtin_determinism: unexpected Aditi call"). 
aditi_builtin_determinism(aditi_tuple_insert_delete(_, _), det).
aditi_builtin_determinism(aditi_insert_delete_modify(_, _, _), det).

:- pred modecheck_arg_list(int, list(mode), extra_goals,
		list(prog_var), list(prog_var), mode_info, mode_info).
:- mode modecheck_arg_list(in, in, out, in, out,
		mode_info_di, mode_info_uo) is det.

modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args,
		ModeInfo0, ModeInfo) :-

	%
	% Check that `Args0' have livenesses which match the
	% expected livenesses.
	%
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	get_arg_lives(Modes, ModuleInfo0, ExpectedArgLives),
	NeedExactMatch = no,
	modecheck_var_list_is_live(Args0, ExpectedArgLives, NeedExactMatch,
		ArgOffset, ModeInfo0, ModeInfo1),

	%
	% Check that `Args0' have insts which match the expected
	% initial insts, and set their new final insts (introducing
	% extra unifications for implied modes, if necessary).
	%
	mode_list_get_initial_insts(Modes, ModuleInfo0, InitialInsts),
	modecheck_var_has_inst_list(Args0, InitialInsts, NeedExactMatch,
		ArgOffset, InstVarSub, ModeInfo1, ModeInfo2),
	mode_list_get_final_insts(Modes, ModuleInfo0, FinalInsts0),
	inst_list_apply_substitution(FinalInsts0, InstVarSub, FinalInsts),
	modecheck_set_var_inst_list(Args0, InitialInsts, FinalInsts,
		ArgOffset, Args, ExtraGoals, ModeInfo2, ModeInfo).

modecheck_call_pred(PredId, ProcId0, ArgVars0, DeterminismKnown,
		TheProcId, ArgVars, ExtraGoals, ModeInfo0, ModeInfo) :-

	mode_info_get_may_change_called_proc(ModeInfo0, MayChangeCalledProc),

	mode_info_get_preds(ModeInfo0, Preds),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),

	( MayChangeCalledProc = may_not_change_called_proc ->
		( invalid_proc_id(ProcId0) ->
			error("modecheck_call_pred: invalid proc_id")
		;
			ProcIds = [ProcId0]
		)
	;
			% Get the list of different possible
			% modes for the called predicate
		pred_info_all_procids(PredInfo, ProcIds)
	),

	compute_arg_offset(PredInfo, ArgOffset),
	pred_info_get_markers(PredInfo, Markers),

		% In order to give better diagnostics, we handle the
		% cases where there are zero or one modes for the called
		% predicate specially.
	(
		ProcIds = [],
		\+ check_marker(Markers, infer_modes)
	->
		set__init(WaitingVars),
		mode_info_error(WaitingVars, mode_error_no_mode_decl,
			ModeInfo0, ModeInfo),
		invalid_proc_id(TheProcId),
		ArgVars = ArgVars0,
		ExtraGoals = no_extra_goals
	;
		ProcIds = [ProcId],
		( \+ check_marker(Markers, infer_modes)
		; MayChangeCalledProc = may_not_change_called_proc
		)
	->
		TheProcId = ProcId,
		map__lookup(Procs, ProcId, ProcInfo),

		%
		% Check that `ArgsVars0' have livenesses which match the
		% expected livenesses.
		%
		proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),
		NeedExactMatch = no,
		modecheck_var_list_is_live(ArgVars0, ProcArgLives0,
			NeedExactMatch, ArgOffset, ModeInfo0, ModeInfo1),

		%
		% Check that `ArgsVars0' have insts which match the expected
		% initial insts, and set their new final insts (introducing
		% extra unifications for implied modes, if necessary).
		%
		proc_info_argmodes(ProcInfo, ProcArgModes0),
		proc_info_inst_varset(ProcInfo, ProcInstVarSet),
		mode_info_get_instvarset(ModeInfo1, InstVarSet),
		rename_apart_inst_vars(InstVarSet, ProcInstVarSet,
			ProcArgModes0, ProcArgModes),
		mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
				InitialInsts),
		modecheck_var_has_inst_list(ArgVars0, InitialInsts,
				NeedExactMatch, ArgOffset, InstVarSub,
				ModeInfo1, ModeInfo2),

		modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0,
			ArgOffset, InstVarSub, ArgVars, ExtraGoals, ModeInfo2,
			ModeInfo)
	;
			% set the current error list to empty (and
			% save the old one in `OldErrors').  This is so the
			% test for `Errors = []' in find_matching_modes
			% will work.
		mode_info_get_errors(ModeInfo0, OldErrors),
		mode_info_set_errors([], ModeInfo0, ModeInfo1),

		set__init(WaitingVars0),
		modecheck_find_matching_modes(ProcIds, PredId, Procs, ArgVars0,
			[], RevMatchingProcIds, WaitingVars0, WaitingVars1,
			ModeInfo1, ModeInfo2),

		(	RevMatchingProcIds = [],
			no_matching_modes(PredId, ArgVars0,
				DeterminismKnown, WaitingVars1,
				TheProcId, ModeInfo2, ModeInfo4),
			ArgVars = ArgVars0,
			ExtraGoals = no_extra_goals
		;
			RevMatchingProcIds = [_|_],
			list__reverse(RevMatchingProcIds, MatchingProcIds),
			choose_best_match(MatchingProcIds, PredId, Procs,
				ArgVars0, TheProcId, InstVarSub, ProcArgModes,
				ModeInfo2),
			map__lookup(Procs, TheProcId, ProcInfo),
			CalleeModeErrors = ProcInfo ^ mode_errors,
			( CalleeModeErrors = [_|_] ->
				% mode error in callee for this mode
				ArgVars = ArgVars0,
				WaitingVars = set__list_to_set(ArgVars),
				ExtraGoals = no_extra_goals,
				mode_info_get_instmap(ModeInfo2, InstMap),
				instmap__lookup_vars(ArgVars, InstMap,
					ArgInsts),
				mode_info_set_call_arg_context(0, ModeInfo2,
					ModeInfo3),
				mode_info_error(WaitingVars,
					mode_error_in_callee(ArgVars, ArgInsts,
						PredId, TheProcId,
						CalleeModeErrors),
					ModeInfo3, ModeInfo4)
			;
				modecheck_end_of_call(ProcInfo, ProcArgModes,
					ArgVars0, ArgOffset, InstVarSub,
					ArgVars, ExtraGoals,
					ModeInfo2, ModeInfo4)
			)
		),

			% restore the error list, appending any new error(s)
		mode_info_get_errors(ModeInfo4, NewErrors),
		list__append(OldErrors, NewErrors, Errors),
		mode_info_set_errors(Errors, ModeInfo4, ModeInfo)
	).

%--------------------------------------------------------------------------%

:- pred no_matching_modes(pred_id, list(prog_var), maybe(determinism),
		set(prog_var), proc_id, mode_info, mode_info).
:- mode no_matching_modes(in, in, in, in, out, mode_info_di, mode_info_uo)
	is det.

no_matching_modes(PredId, ArgVars, DeterminismKnown, WaitingVars, TheProcId,
		ModeInfo0, ModeInfo) :-
	%
	% There were no matching modes.
	% If we're inferring modes for this called predicate, then
	% just insert a new mode declaration which will match.
	% Otherwise, report an error.
	%
	mode_info_get_preds(ModeInfo0, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	( check_marker(Markers, infer_modes) ->
		insert_new_mode(PredId, ArgVars, DeterminismKnown, TheProcId,
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

:- type proc_mode ---> proc_mode(proc_id, inst_var_sub, list(mode)).

:- pred modecheck_find_matching_modes(list(proc_id), pred_id, proc_table,
		list(prog_var), list(proc_mode), list(proc_mode), set(prog_var),
		set(prog_var), mode_info, mode_info).
:- mode modecheck_find_matching_modes(in, in, in, in,
			in, out, in, out, mode_info_di, mode_info_uo) is det.

modecheck_find_matching_modes([], _PredId, _Procs, _ArgVars,
			MatchingProcIds, MatchingProcIds,
			WaitingVars, WaitingVars, ModeInfo, ModeInfo).

modecheck_find_matching_modes([ProcId | ProcIds], PredId, Procs, ArgVars0,
			MatchingProcIds0, MatchingProcIds,
			WaitingVars0, WaitingVars, ModeInfo0, ModeInfo) :-

		% find the initial insts and the final livenesses
		% of the arguments for this mode of the called pred
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_argmodes(ProcInfo, ProcArgModes0),
	proc_info_inst_varset(ProcInfo, ProcInstVarSet),
	mode_info_get_instvarset(ModeInfo0, InstVarSet),
	rename_apart_inst_vars(InstVarSet, ProcInstVarSet, ProcArgModes0,
		ProcArgModes),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),

		% check whether the livenesses of the args matches their
		% expected liveness
	NeedLivenessExactMatch = no,
	modecheck_var_list_is_live(ArgVars0, ProcArgLives0,
			NeedLivenessExactMatch, 0, ModeInfo0, ModeInfo1),

		% If we're doing mode inference for the called
		% procedure, and the called procedure has been inferred as
		% an invalid mode, then don't use it unless it is an exact
		% match.
		%
		% XXX Do we really want mode inference to use implied modes?
		% Would it be better to always require an exact match when
		% doing mode inference, to ensure that we add new inferred
		% modes rather than using implied modes?
	(
		proc_info_is_valid_mode(ProcInfo)
	->
		NeedExactMatch = no
	;
		NeedExactMatch = yes
	),

		% Check whether the insts of the args matches their expected
		% initial insts. 
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	modecheck_var_has_inst_list(ArgVars0, InitialInsts, NeedExactMatch, 0,
			InstVarSub, ModeInfo1, ModeInfo2),

		% If we got an error, reset the error list
		% and save the list of vars to wait on.
		% Otherwise, insert the proc_id in the list of matching
		% proc_ids.
	mode_info_get_errors(ModeInfo2, Errors),
	(
		Errors = [FirstError | _]
	->
		MatchingProcIds1 = MatchingProcIds0,
		mode_info_set_errors([], ModeInfo2, ModeInfo3),
		FirstError = mode_error_info(ErrorWaitingVars, _, _, _),
		set__union(WaitingVars0, ErrorWaitingVars, WaitingVars1)
	;
		MatchingProcIds1 = [proc_mode(ProcId, InstVarSub, ProcArgModes)
					| MatchingProcIds0],
		ModeInfo3 = ModeInfo2,
		WaitingVars1 = WaitingVars0
	),

		% keep trying with the other modes for the called pred
	modecheck_find_matching_modes(ProcIds, PredId, Procs, ArgVars0,
			MatchingProcIds1, MatchingProcIds,
			WaitingVars1, WaitingVars, ModeInfo3, ModeInfo).

:- pred modecheck_end_of_call(proc_info, list(mode), list(prog_var), int,
	inst_var_sub, list(prog_var), extra_goals, mode_info, mode_info).
:- mode modecheck_end_of_call(in, in, in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0, ArgOffset, InstVarSub,
			ArgVars, ExtraGoals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts0),
	inst_list_apply_substitution(InitialInsts0, InstVarSub, InitialInsts),
	mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts0),
	inst_list_apply_substitution(FinalInsts0, InstVarSub, FinalInsts),
	modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
			ArgOffset, ArgVars, ExtraGoals, ModeInfo0, ModeInfo1),
	proc_info_never_succeeds(ProcInfo, NeverSucceeds),
	( NeverSucceeds = yes ->
		instmap__init_unreachable(Instmap),
		mode_info_set_instmap(Instmap, ModeInfo1, ModeInfo)
	;
		ModeInfo = ModeInfo1
	).

:- pred insert_new_mode(pred_id, list(prog_var), maybe(determinism), proc_id,
			mode_info, mode_info).
:- mode insert_new_mode(in, in, in, out, mode_info_di, mode_info_uo) is det.

	% Insert a new inferred mode for a predicate.
	% The initial insts are determined by using a normalised
	% version of the call pattern (i.e. the insts of the arg vars).
	% The final insts are initially just assumed to be all `not_reached'.
	% The determinism for this mode will be inferred.

insert_new_mode(PredId, ArgVars, MaybeDet, ProcId, ModeInfo0, ModeInfo) :-
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
	mode_info_get_instvarset(ModeInfo0, InstVarSet),

	%
	% call unify_proc__request_proc, which will
	% create the new procedure, set its "can-process" flag to `no',
	% and insert it into the queue of requested procedures.
	%
	unify_proc__request_proc(PredId, Modes, InstVarSet, yes(ArgLives),
		MaybeDet, Context, ModuleInfo0, ProcId, ModuleInfo),

	mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),

	% Since we've created a new inferred mode for this predicate,
	% things have changed, so we will need to do at least one more
	% pass of the fixpoint analysis.
	mode_info_set_changed_flag(yes, ModeInfo1, ModeInfo).

:- pred get_var_insts_and_lives(list(prog_var), mode_info,
				list(inst), list(is_live)).
:- mode get_var_insts_and_lives(in, mode_info_ui, out, out) is det.

get_var_insts_and_lives([], _, [], []).
get_var_insts_and_lives([Var | Vars], ModeInfo,
		[Inst | Insts], [IsLive | IsLives]) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_instmap(ModeInfo, InstMap),
	mode_info_get_var_types(ModeInfo, VarTypes),
	instmap__lookup_var(InstMap, Var, Inst0),
	map__lookup(VarTypes, Var, Type),
	normalise_inst(Inst0, Type, ModuleInfo, Inst),

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
			inst_is_mostly_unique(ModuleInfo, Inst)
		->
			IsLive = dead
		;
			IsLive = live
		)
	),

	get_var_insts_and_lives(Vars, ModeInfo, Insts, IsLives).

%-----------------------------------------------------------------------------%

	%
	% Given two modes of a predicate, figure out whether
	% they are indistinguishable; that is, whether any valid call to
	% one mode would also be a valid call to the other.
	% (If so, it is a mode error.)
	% Note that mode declarations which only have different final insts
	% do not count as distinguishable.
	%
	% The code for this is similar to the code for
	% modes_are_indentical/4 and compare_proc/5 below.
	%
modes_are_indistinguishable(ProcId, OtherProcId, PredInfo, ModuleInfo) :-
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	map__lookup(Procs, OtherProcId, OtherProcInfo),

	%
	% Compare the initial insts of the arguments
	%
	proc_info_argmodes(ProcInfo, ProcArgModes),
	proc_info_argmodes(OtherProcInfo, OtherProcArgModes),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	mode_list_get_initial_insts(OtherProcArgModes, ModuleInfo,
							OtherInitialInsts),
	pred_info_arg_types(PredInfo, ArgTypes),
	compare_inst_list(InitialInsts, OtherInitialInsts, no,
		ArgTypes, CompareInsts, ModuleInfo),
	CompareInsts = same,

	%
	% Compare the expected livenesses of the arguments
	%
	get_arg_lives(ProcArgModes, ModuleInfo, ProcArgLives),
	get_arg_lives(OtherProcArgModes, ModuleInfo, OtherProcArgLives),
	compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
	CompareLives = same,

	%
	% Compare the determinisms --
	%	If both are cc_, or if both are not cc_,
	%	then they are indistinguishable.
	%
	proc_info_interface_determinism(ProcInfo, Detism),
	proc_info_interface_determinism(OtherProcInfo, OtherDetism),
	determinism_components(Detism, _CanFail, Solns),
	determinism_components(OtherDetism, _OtherCanFail, OtherSolns),
	( Solns = at_most_many_cc, OtherSolns = at_most_many_cc
	; Solns \= at_most_many_cc, OtherSolns \= at_most_many_cc
	).

%-----------------------------------------------------------------------------%

	%
	% Given two modes of a predicate, figure out whether
	% they are identical, except that one is cc_nondet/cc_multi
	% and the other is nondet/multi.
	%
	% The code for this is similar to the code for compare_proc/5 below
	% and modes_are_indistinguishable/4 above.
	%
modes_are_identical_bar_cc(ProcId, OtherProcId, PredInfo, ModuleInfo) :-
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	map__lookup(Procs, OtherProcId, OtherProcInfo),

	%
	% Compare the initial insts of the arguments
	%
	proc_info_argmodes(ProcInfo, ProcArgModes),
	proc_info_argmodes(OtherProcInfo, OtherProcArgModes),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	mode_list_get_initial_insts(OtherProcArgModes, ModuleInfo,
							OtherInitialInsts),
	pred_info_arg_types(PredInfo, ArgTypes),
	compare_inst_list(InitialInsts, OtherInitialInsts, no,
		ArgTypes, CompareInitialInsts, ModuleInfo),
	CompareInitialInsts = same,

	%
	% Compare the final insts of the arguments
	%
	mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
	mode_list_get_final_insts(OtherProcArgModes, ModuleInfo,
							OtherFinalInsts),
	compare_inst_list(FinalInsts, OtherFinalInsts, no,
		ArgTypes, CompareFinalInsts, ModuleInfo),
	CompareFinalInsts = same,

	%
	% Compare the expected livenesses of the arguments
	%
	get_arg_lives(ProcArgModes, ModuleInfo, ProcArgLives),
	get_arg_lives(OtherProcArgModes, ModuleInfo, OtherProcArgLives),
	compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
	CompareLives = same,

	%
	% Compare the determinisms, ignoring the cc part.
	%
	proc_info_interface_determinism(ProcInfo, Detism),
	proc_info_interface_determinism(OtherProcInfo, OtherDetism),
	determinism_components(Detism, CanFail, Solns),
	determinism_components(OtherDetism, OtherCanFail, OtherSolns),
	CanFail = OtherCanFail,
	( Solns = OtherSolns
	; Solns = at_most_many_cc, OtherSolns = at_most_many
	; Solns = at_most_many, OtherSolns = at_most_many_cc
	).

%-----------------------------------------------------------------------------%

/*
The algorithm for choose_best_match is supposed to be equivalent
to the following specification:

	1.  Remove any modes that are strictly less instantiated or
	    less informative on input than other valid modes; eg,
	    prefer an (in, in, out) mode over an (out, in, out) mode,
	    but not necessarily over an (out, out, in) mode,
	    and prefer a (ground -> ...) mode over a (any -> ...) mode,
	    and prefer a (bound(f) -> ...) mode over a (ground -> ...) mode,
	    and prefer a (... -> dead) mode over a (... -> not dead) mode.

	    Also prefer a (any -> ...) mode over a (free -> ...) mode,
	    unless the actual argument is free, in which case prefer
	    the (free -> ...) mode.

 	2.  If neither is prefered over the other by step 1, then
	    prioritize them by determinism, according to the standard
	    partial order (best first):

 				erroneous
 			       /       \
  			    det		failure
 		          /    \       /
  		      multi	semidet
		         \      /
 			  nondet

 	3.  If there are still multiple possibilities, take them in 
 	    declaration order.
*/

:- type match
	--->	better
	;	worse
	;	same
	;	incomparable.

:- pred choose_best_match(list(proc_mode), pred_id,
		proc_table, list(prog_var), proc_id, inst_var_sub, list(mode),
		mode_info).
:- mode choose_best_match(in, in, in, in, out, out, out, mode_info_ui) is det.

choose_best_match([], _, _, _, _, _, _, _) :-
	error("choose_best_match: no best match").
choose_best_match([proc_mode(ProcId, InstVarSub, ArgModes) | ProcIds], PredId,
		Procs, ArgVars, TheProcId, TheInstVarSub, TheArgModes,
		ModeInfo) :-
	%
	% This ProcId is best iff there is no other proc_id which is better.
	%
	(
		\+ (
			list__member(proc_mode(OtherProcId, _, _), ProcIds),
			compare_proc(OtherProcId, ProcId, ArgVars, better,
					Procs, ModeInfo)
		)
	->
		TheProcId = ProcId,
		TheInstVarSub = InstVarSub,
		TheArgModes = ArgModes
	;
		choose_best_match(ProcIds, PredId, Procs, ArgVars, TheProcId,
			TheInstVarSub, TheArgModes, ModeInfo)
	).

	%
	% Given two modes of a predicate, figure out whether
	% one of them is a better match than the other,
	% for calls which could match either mode.
	%
	% The code for this is similar to the code for
	% modes_are_indistinguishable/4 and
	% modes_are_identical_bar_cc/4 above.
	%
:- pred compare_proc(proc_id, proc_id, list(prog_var), match, proc_table,
		mode_info).
:- mode compare_proc(in, in, in, out, in, mode_info_ui) is det.

compare_proc(ProcId, OtherProcId, ArgVars, Compare, Procs, ModeInfo) :-
	map__lookup(Procs, ProcId, ProcInfo),
	map__lookup(Procs, OtherProcId, OtherProcInfo),
	%
	% Compare the initial insts of the arguments
	%
	proc_info_argmodes(ProcInfo, ProcArgModes),
	proc_info_argmodes(OtherProcInfo, OtherProcArgModes),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_var_types(ModeInfo, VarTypes),
	list__map(map__lookup(VarTypes), ArgVars, ArgTypes),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	mode_list_get_initial_insts(OtherProcArgModes, ModuleInfo,
							OtherInitialInsts),
	get_var_insts_and_lives(ArgVars, ModeInfo, ArgInitialInsts, _ArgLives),
	compare_inst_list(InitialInsts, OtherInitialInsts, yes(ArgInitialInsts),
		ArgTypes, CompareInsts, ModuleInfo),
	%
	% Compare the expected livenesses of the arguments
	%
	get_arg_lives(ProcArgModes, ModuleInfo, ProcArgLives),
	get_arg_lives(OtherProcArgModes, ModuleInfo, OtherProcArgLives),
	compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
	%
	% Compare the determinisms
	%
	proc_info_interface_determinism(ProcInfo, Detism),
	proc_info_interface_determinism(OtherProcInfo, OtherDetism),
	compare_determinisms(Detism, OtherDetism, CompareDet0),
	( CompareDet0 = tighter, CompareDet = better
	; CompareDet0 = looser, CompareDet = worse
	; CompareDet0 = sameas, CompareDet = same
	),
	%
	% Combine the results, with the insts & lives comparisons
	% taking priority over the determinism comparison.
	%
	combine_results(CompareInsts, CompareLives, Compare0),
	prioritized_combine_results(Compare0, CompareDet, Compare).

:- pred compare_inst_list(list(inst), list(inst), maybe(list(inst)),
		list(type), match, module_info).
:- mode compare_inst_list(in, in, in, in, out, in) is det.

compare_inst_list(InstsA, InstsB, ArgInsts, Types, Result, ModuleInfo) :-
	(
		compare_inst_list_2(InstsA, InstsB, ArgInsts, Types, Result0,
			ModuleInfo)
	->
		Result = Result0
	;
		error("compare_inst_list: length mis-match")
	).

:- pred compare_inst_list_2(list(inst), list(inst), maybe(list(inst)),
		list(type), match, module_info).
:- mode compare_inst_list_2(in, in, in, in, out, in) is semidet.

compare_inst_list_2([], [], _, [], same, _).
compare_inst_list_2([InstA | InstsA], [InstB | InstsB],
		no, [Type | Types], Result, ModuleInfo) :-
	compare_inst(InstA, InstB, no, Type, Result0, ModuleInfo),
	compare_inst_list_2(InstsA, InstsB, no, Types, Result1, ModuleInfo),
	combine_results(Result0, Result1, Result).
compare_inst_list_2([InstA | InstsA], [InstB | InstsB],
		yes([ArgInst|ArgInsts]), [Type | Types], Result, ModuleInfo) :-
	compare_inst(InstA, InstB, yes(ArgInst), Type, Result0, ModuleInfo),
	compare_inst_list_2(InstsA, InstsB, yes(ArgInsts), Types, Result1,
		ModuleInfo),
	combine_results(Result0, Result1, Result).

:- pred compare_liveness_list(list(is_live), list(is_live), match).
:- mode compare_liveness_list(in, in, out) is det.

compare_liveness_list([], [], same).
compare_liveness_list([_|_], [], _) :-
	error("compare_liveness_list: length mis-match").
compare_liveness_list([], [_|_], _) :-
	error("compare_liveness_list: length mis-match").
compare_liveness_list([LiveA | LiveAs], [LiveB | LiveBs], Result) :-
	compare_liveness(LiveA, LiveB, Result0),
	compare_liveness_list(LiveAs, LiveBs, Result1),
	combine_results(Result0, Result1, Result).

	%
	% compare_liveness -- prefer dead to live
	%	(if either is a valid match, then the actual argument
	%	must be dead, so prefer the mode which can take advantage
	%	of that).
	%
:- pred compare_liveness(is_live, is_live, match).
:- mode compare_liveness(in, in, out) is det.

compare_liveness(dead, dead, same).
compare_liveness(dead, live, better).
compare_liveness(live, dead, worse).
compare_liveness(live, live, same).

	%
	% combine two results, giving priority to the first one
	%
:- pred prioritized_combine_results(match, match, match).
:- mode prioritized_combine_results(in, in, out) is det.

prioritized_combine_results(better, _, better).
prioritized_combine_results(worse, _, worse).
prioritized_combine_results(same, Result, Result).
prioritized_combine_results(incomparable, _, incomparable).

	%
	% combine two results, giving them equal priority
	%
:- pred combine_results(match, match, match).
:- mode combine_results(in, in, out) is det.

combine_results(better, better, better).
combine_results(better, same, better).
combine_results(better, worse, incomparable).
combine_results(better, incomparable, incomparable).
combine_results(worse, worse, worse).
combine_results(worse, same, worse).
combine_results(worse, better, incomparable).
combine_results(worse, incomparable, incomparable).
combine_results(same, Result, Result).
combine_results(incomparable, _, incomparable).

	%
	% Compare two initial insts, to figure out which would be a better
	% match.
	%
	% More information is better:
	% 	prefer bound(f) to ground
	% 	prefer unique to mostly_unique or ground, and
	%	prefer mostly_unique to ground
	%		(unique > mostly_unique > shared > mostly_dead > dead)
	% More bound is better:
	%		(if both can match, the one which is more bound
	%		is better, because it may be an exact match, whereas
	%		the other one would be an implied mode)
	%	prefer ground to free	(i.e. prefer in to out)
	% 	prefer ground to any	(e.g. prefer in to in(any))
	% 	prefer any to free	(e.g. prefer any->ground to out)

:- pred compare_inst(inst, inst, maybe(inst), type, match, module_info).
:- mode compare_inst(in, in, in, in, out, in) is det.

compare_inst(InstA, InstB, MaybeArgInst, Type, Result, ModuleInfo) :-
	% inst_matches_initial(A,B) succeeds iff
	%	A specifies at least as much information
	%	and at least as much binding as B --
	%	with the exception that `any' matches_initial `free'
	% 	and perhaps vice versa.
	( inst_matches_initial(InstA, InstB, Type, ModuleInfo) ->
		A_mi_B = yes
	;
		A_mi_B = no
	),
	( inst_matches_initial(InstB, InstA, Type, ModuleInfo) ->
		B_mi_A = yes
	;
		B_mi_A = no
	),
	( A_mi_B = yes, B_mi_A = no,  Result = better
	; A_mi_B = no,  B_mi_A = yes, Result = worse
	; A_mi_B = no,  B_mi_A = no,  Result = incomparable
	; A_mi_B = yes, B_mi_A = yes,
		%
		% We need to further disambiguate the cases involving
		% `any' and `free', since `any' matches_initial `free'
		% and vice versa.  For these cases, we want to take
		% the actual inst of the argument into account:
		% if the argument is `free', we should prefer `free',
		% but otherwise, we should prefer `any'.
		%
		(
			MaybeArgInst = no,
			Result0 = same
		;
			MaybeArgInst = yes(ArgInst),
			(
				inst_matches_final(ArgInst, InstA, Type,
					ModuleInfo)
			->
				Arg_mf_A = yes
			;
				Arg_mf_A = no
			),
			(
				inst_matches_final(ArgInst, InstB, Type,
					ModuleInfo)
			->
				Arg_mf_B = yes
			;
				Arg_mf_B = no
			),
			( Arg_mf_A = yes, Arg_mf_B = no,  Result0 = better
			; Arg_mf_A = no,  Arg_mf_B = yes, Result0 = worse
			; Arg_mf_A = yes, Arg_mf_B = yes, Result0 = same
			; Arg_mf_A = no,  Arg_mf_B = no,  Result0 = same
			)
		),
		( Result0 = same ->
			%
			% if the actual arg inst is not available,
			% or comparing with the arg inst doesn't help,
			% then compare the two proc insts
			%
			( inst_matches_final(InstA, InstB, Type, ModuleInfo) ->
				A_mf_B = yes
			;
				A_mf_B = no
			),
			( inst_matches_final(InstB, InstA, Type, ModuleInfo) ->
				B_mf_A = yes
			;
				B_mf_A = no
			),
			( A_mf_B = yes, B_mf_A = no,  Result = better
			; A_mf_B = no,  B_mf_A = yes, Result = worse
			; A_mf_B = no,  B_mf_A = no,  Result = incomparable
			; A_mf_B = yes, B_mf_A = yes, Result = same
			)
		;
			Result = Result0
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
