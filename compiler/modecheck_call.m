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
				list(type), list(mode), determinism, list(var),
				extra_goals, mode_info, mode_info).
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

	%
	% Given two modes of a predicate, figure out whether
	% they are indistinguishable; that is, whether any valid call to
	% one mode would also be a valid call to the other.
	% (If so, it is a mode error.)
	%
:- pred modes_are_indistinguishable(proc_id, proc_id, pred_info, module_info).
:- mode modes_are_indistinguishable(in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_data, hlds_pred, hlds_data, hlds_module, instmap, (inst).
:- import_module mode_info, mode_debug, modes, mode_util, mode_errors.
:- import_module clause_to_proc, inst_match, make_hlds.
:- import_module det_report.
:- import_module map, list, bool, std_util, set, require.

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
		ExtraGoals = no_extra_goals
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
		ExtraGoals = no_extra_goals
	;
		ProcIds = [ProcId],
		\+ list__member(request(infer_modes), Markers)
	->
		TheProcId = ProcId,
		map__lookup(Procs, ProcId, ProcInfo),
		%
		% Check that `ArgsVars0' have livenesses which match the
		% expected livenesses.
		%
		proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),
		modecheck_var_list_is_live(ArgVars0, ProcArgLives0, 0,
					ModeInfo0, ModeInfo1),

		%
		% Check that `ArgsVars0' have insts which match the expected
		% initial insts, and set their new final insts (introducing
		% extra unifications for implied modes, if necessary).
		%
		proc_info_argmodes(ProcInfo, ProcArgModes),
		mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
					InitialInsts),
		modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
					ModeInfo1, ModeInfo2),

		modecheck_end_of_call(ProcInfo, ArgVars0, ArgVars,
					ExtraGoals, ModeInfo2, ModeInfo)
	;
			% set the current error list to empty (and
			% save the old one in `OldErrors').  This is so the
			% test for `Errors = []' in find_matching_modes
			% will work.
		mode_info_get_errors(ModeInfo0, OldErrors),
		mode_info_set_errors([], ModeInfo0, ModeInfo1),

		set__init(WaitingVars0),
		modecheck_find_matching_modes(ProcIds, PredId, Procs, ArgVars0,
			[], RevMatchingProcIds, WaitingVars0, WaitingVars,
			ModeInfo1, ModeInfo2),

		(	RevMatchingProcIds = [],
			no_matching_modes(PredId, ArgVars0, WaitingVars,
				TheProcId, ModeInfo2, ModeInfo3),
			ArgVars = ArgVars0,
			ExtraGoals = no_extra_goals
		;
			RevMatchingProcIds = [_|_],
			list__reverse(RevMatchingProcIds, MatchingProcIds),
			choose_best_match(MatchingProcIds, PredId, Procs,
				TheProcId, ModeInfo2),
			map__lookup(Procs, TheProcId, ProcInfo),
			modecheck_end_of_call(ProcInfo, ArgVars0, ArgVars,
				ExtraGoals, ModeInfo2, ModeInfo3)
		),

			% restore the error list, appending any new error(s)
		mode_info_get_errors(ModeInfo3, NewErrors),
		list__append(OldErrors, NewErrors, Errors),
		mode_info_set_errors(Errors, ModeInfo3, ModeInfo)
	).

:- pred no_matching_modes(pred_id, list(var), set(var), proc_id, 	
				mode_info, mode_info).
:- mode no_matching_modes(in, in, in, out, mode_info_di, mode_info_uo) is det.

no_matching_modes(PredId, ArgVars, WaitingVars, TheProcId,
		ModeInfo0, ModeInfo) :-
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

:- pred modecheck_find_matching_modes(
			list(proc_id), pred_id, proc_table, list(var),
			list(proc_id), list(proc_id), set(var), set(var),
			mode_info, mode_info).
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
	proc_info_argmodes(ProcInfo, ProcArgModes),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),

		% check whether the livenesses of the args matches their
		% expected liveness
	modecheck_var_list_is_live(ArgVars0, ProcArgLives0, 0,
				ModeInfo0, ModeInfo1),

		% check whether the insts of the args matches their expected
		% initial insts
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
				ModeInfo1, ModeInfo2),

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
		MatchingProcIds1 = [ProcId | MatchingProcIds0],
		ModeInfo3 = ModeInfo2,
		WaitingVars1 = WaitingVars0
	),

		% keep trying with the other modes for the called pred
	modecheck_find_matching_modes(ProcIds, PredId, Procs, ArgVars0,
			MatchingProcIds1, MatchingProcIds,
			WaitingVars1, WaitingVars, ModeInfo3, ModeInfo).

:- pred modecheck_end_of_call(proc_info, list(var), list(var), extra_goals,
				mode_info, mode_info).
:- mode modecheck_end_of_call(in, in, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_end_of_call(ProcInfo, ArgVars0, ArgVars, ExtraGoals,
			ModeInfo0, ModeInfo) :-
	proc_info_argmodes(ProcInfo, ProcArgModes),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
	modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
			ArgVars, ExtraGoals, ModeInfo0, ModeInfo1),
	proc_info_never_succeeds(ProcInfo, NeverSucceeds),
	( NeverSucceeds = yes ->
		instmap__init_unreachable(Instmap),
		mode_info_set_instmap(Instmap, ModeInfo1, ModeInfo)
	;
		ModeInfo = ModeInfo1
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

	%
	% Given two modes of a predicate, figure out whether
	% they are indistinguishable; that is, whether any valid call to
	% one mode would also be a valid call to the other.
	% (If so, it is a mode error.)
	%
	% The code for this is similar to the code for compare_procs/5 below.
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
	compare_inst_list(InitialInsts, OtherInitialInsts, CompareInsts,
		ModuleInfo),
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

/*
The algorithm for choose_best_match is supposed to be equivalent
to the following specification:

	1.  Remove any modes that are strictly less instantiated or
	    less informative on input than other valid modes; eg,
	    prefer an (in, in, out) mode over an (out, in, out) mode,
	    but not necessarily over an (out, out, in) mode,
	    and prefer a (free -> ...) mode over a (any -> ...) mode,
	    and prefer a (bound(f) -> ...) mode over a (ground -> ...) mode,
	    and prefer a (... -> dead) mode over a (... -> not dead) mode.

	    This is a partial order.

 	2.  Prioritize them by determinism, according to the standard
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

:- pred choose_best_match(list(proc_id), pred_id, proc_table, proc_id,
				mode_info).
:- mode choose_best_match(in, in, in, out,
				mode_info_ui) is det.

choose_best_match([], _, _, _, _) :-
	error("choose_best_match: no best match").
choose_best_match([ProcId | ProcIds], PredId, Procs, TheProcId,
			ModeInfo) :-
	%
	% This ProcId is best iff there is no other proc_id which is better.
	%
	(
		\+ (
			list__member(OtherProcId, ProcIds),
			compare_proc(OtherProcId, ProcId, better,
					Procs, ModeInfo)
		)
	->
		TheProcId = ProcId
	;
		choose_best_match(ProcIds, PredId, Procs, TheProcId, ModeInfo)
	).

	%
	% Given two modes of a predicate, figure out whether
	% one of them is a better match than the other,
	% for calls which could match either mode.
	%
	% The code for this is similar to the code for
	% modes_are_indistiguisable/4 above.
	%
:- pred compare_proc(proc_id, proc_id, match, proc_table, mode_info).
:- mode compare_proc(in, in, out, in, mode_info_ui) is det.

compare_proc(ProcId, OtherProcId, Compare, Procs, ModeInfo) :-
	map__lookup(Procs, ProcId, ProcInfo),
	map__lookup(Procs, OtherProcId, OtherProcInfo),
	%
	% Compare the initial insts of the arguments
	%
	proc_info_argmodes(ProcInfo, ProcArgModes),
	proc_info_argmodes(OtherProcInfo, OtherProcArgModes),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	mode_list_get_initial_insts(OtherProcArgModes, ModuleInfo,
							OtherInitialInsts),
	compare_inst_list(InitialInsts, OtherInitialInsts, CompareInsts,
		ModuleInfo),
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

:- pred compare_inst_list(list(inst), list(inst), match, module_info).
:- mode compare_inst_list(in, in, out, in) is det.

compare_inst_list([], [], same, _).
compare_inst_list([_|_], [], _, _) :-
	error("compare_inst_list: length mis-match").
compare_inst_list([], [_|_], _, _) :-
	error("compare_inst_list: length mis-match").
compare_inst_list([InstA | InstsA], [InstB | InstsB], Result, ModuleInfo) :-
	compare_inst(InstA, InstB, Result0, ModuleInfo),
	compare_inst_list(InstsA, InstsB, Result1, ModuleInfo),
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

:- pred compare_inst(inst, inst, match, module_info).
:- mode compare_inst(in, in, out, in) is det.

compare_inst(InstA, InstB, Result, ModuleInfo) :-
	% inst_matches_initial(A,B) succeeds iff
	%	A specifies at least as much information
	%	and at least as much binding as B --
	%	with the exception that `any' matches_initial `free'
	% 	and perhaps vice versa.
	( inst_matches_initial(InstA, InstB, ModuleInfo) ->
		A_mi_B = yes
	;
		A_mi_B = no
	),
	( inst_matches_initial(InstB, InstA, ModuleInfo) ->
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
		% and vice versa, but we want to prefer `any'.
		% We use matches_final, because `free' may match_final `any',
		% but `any' does not match_final `free'.
		%
		( inst_matches_final(InstA, InstB, ModuleInfo) ->
			A_mf_B = yes
		;
			A_mf_B = no
		),
		( inst_matches_final(InstB, InstA, ModuleInfo) ->
			B_mf_A = yes
		;
			B_mf_A = no
		),
		( A_mf_B = yes, B_mf_A = no,  Result = worse
		; A_mf_B = no,  B_mf_A = yes, Result = better
		; A_mf_B = yes, B_mf_A = yes, Result = same
		; A_mf_B = no,  B_mf_A = no,  Result = incomparable
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
