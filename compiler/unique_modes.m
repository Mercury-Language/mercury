%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: unique_modes.m
% main author: fjh

% This module checks that variables with a unique mode (as opposed to
% a mostly-unique mode) really are unique, and not nondet live - i.e.,
% that they cannot be referenced on backtracking.

% Basically we just traverse each goal, keeping track of which variables
% are nondet live.  At each procedure call, we check that any arguments
% whose initial insts are required to be unique are not nondet live.
% If they are, we report an error message.

% XXX what if it would have matched ok with a different mode of the
% called predicate (e.g. if a predicate is overloaded with both
% `ui' and `in' modes)?

% XXX we currently make the conservative assumption that
% any non-local variable in a disjunction or nondet call
% is nondet-live - and stays nondet-live.

%-----------------------------------------------------------------------------%

:- module unique_modes.
:- interface. 
:- import_module hlds_module, hlds_pred, hlds_goal, mode_info, io.

	% check every predicate in a module
:- pred unique_modes__check_module(module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_module(in, out, di, uo) is det.

	% just check a single procedure
:- pred unique_modes__check_proc(proc_id, pred_id, module_info,
				module_info, io__state, io__state).
:- mode unique_modes__check_proc(in, in, in, out, di, uo) is det.

	% just check a single goal
:- pred unique_modes__check_goal(hlds_goal, hlds_goal, mode_info, mode_info).
:- mode unique_modes__check_goal(in, out, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, mode_debug, modecheck_unify, instmap.
:- import_module mode_util, prog_out, hlds_out, mercury_to_mercury, passes_aux.
:- import_module modes, inst_match, prog_data, mode_errors, llds, unify_proc.
:- import_module bool, int, list, map, set, std_util, require, term, varset.
:- import_module assoc_list.

%-----------------------------------------------------------------------------%

	% This section just traverses the module structure.

unique_modes__check_module(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	unique_modes__check_preds(PredIds, ModuleInfo0, ModuleInfo1),
	modecheck_unify_procs(check_unique_modes, ModuleInfo1, ModuleInfo).

:- pred unique_modes__check_preds(list(pred_id), module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_preds(in, in, out, di, uo) is det.

unique_modes__check_preds([], ModuleInfo, ModuleInfo) --> [].
unique_modes__check_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds \= [] } ->
		write_pred_progress_message("% Unique-mode-checking ",
			PredId, ModuleInfo0)
	;
		[]
	),
	unique_modes__check_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	unique_modes__check_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred unique_modes__check_procs(list(proc_id), pred_id,
					module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_procs(in, in, in, out, di, uo) is det.

unique_modes__check_procs([], _PredId, ModuleInfo, ModuleInfo) --> [].
unique_modes__check_procs([ProcId | ProcIds], PredId, ModuleInfo0,
		ModuleInfo) -->
	unique_modes__check_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1),
	unique_modes__check_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

unique_modes__check_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) -->
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
		_PredInfo0, ProcInfo0) },
	( { proc_info_can_process(ProcInfo0, no) } ->
		{ ModuleInfo = ModuleInfo0 }
	;
		unique_modes__check_proc_2(ProcInfo0, PredId, ProcId,
			ModuleInfo0, ProcInfo, ModuleInfo1),

		{ module_info_preds(ModuleInfo1, PredTable1) },
		{ map__lookup(PredTable1, PredId, PredInfo1) },
		{ pred_info_procedures(PredInfo1, ProcTable1) },
		{ map__set(ProcTable1, ProcId, ProcInfo, ProcTable) },
		{ pred_info_set_procedures(PredInfo1, ProcTable, PredInfo) },
		{ map__set(PredTable1, PredId, PredInfo, PredTable) },
		{ module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% just check a single procedure
:- pred unique_modes__check_proc_2(proc_info, pred_id, proc_id, module_info,
				proc_info, module_info, io__state, io__state).
:- mode unique_modes__check_proc_2(in, in, in, in, out, out, di, uo) is det.

unique_modes__check_proc_2(ProcInfo0, PredId, ProcId, ModuleInfo0,
			ProcInfo, ModuleInfo,
			IOState0, IOState) :-
	%
	% Extract the useful fields in the proc_info.
	%
	proc_info_headvars(ProcInfo0, Args),
	proc_info_argmodes(ProcInfo0, ArgModes),
	proc_info_arglives(ProcInfo0, ModuleInfo0, ArgLives),
	proc_info_goal(ProcInfo0, Goal0),

	%
	% Figure out the right context to use.
	% We use the context of the first clause, unless
	% there weren't any clauses at all, in which case
	% we use the context of the mode declaration.
	%
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	ClausesInfo = clauses_info(_, _, _, _, ClauseList),
	( ClauseList = [FirstClause | _] ->
		FirstClause = clause(_, _, Context)
	;
		proc_info_context(ProcInfo0, Context)
	),

	%
	% Construct the initial instmap
	%
	mode_list_get_initial_insts(ArgModes, ModuleInfo0, ArgInitialInsts),
	assoc_list__from_corresponding_lists(Args, ArgInitialInsts, InstAL),
	instmap__from_assoc_list(InstAL, InstMap0),

	%
	% Construct the initial set of live variables:
	% initially, only the non-clobbered head variables are live
	%
	get_live_vars(Args, ArgLives, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),

	%
	% At last we're ready to construct the initial mode_info
	%
	mode_info_init(IOState0, ModuleInfo0, PredId, ProcId, Context,
			LiveVars, InstMap0, ModeInfo0),
	%
	% Modecheck the goal
	%
	unique_modes__check_goal(Goal0, Goal, ModeInfo0, ModeInfo1),

	%
	% Check that the final insts of the head vars is OK
	%
	mode_list_get_final_insts(ArgModes, ModuleInfo0, ArgFinalInsts),
	modecheck_final_insts(Args, ArgFinalInsts, ModeInfo1, ModeInfo2),

	%
	% If we encountered any errors then report them
	%
	report_mode_errors(ModeInfo2, ModeInfo),

	%
	% Get the info we need from the mode_info and stuff it back
	% in the proc_info
	%
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_io_state(ModeInfo, IOState1),
	mode_info_get_errors(ModeInfo, Errors),
	( Errors = [] ->
		IOState = IOState1
	;
		io__set_exit_status(1, IOState1, IOState)
	),
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_variables(ProcInfo1, VarSet, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo).

	% XXX we currently make the conservative assumption that
	% any non-local variable in a disjunction or nondet call
	% is nondet-live - and stays nondet-live.

unique_modes__check_goal(Goal0, Goal, ModeInfo0, ModeInfo) :-
	%
	% store the current context in the mode_info
	%
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_context(GoalInfo0, Context),
	term__context_init(EmptyContext),
	( Context = EmptyContext ->
		ModeInfo1 = ModeInfo0
	;
		mode_info_set_context(Context, ModeInfo0, ModeInfo1)
	),

	%
	% Grab the original instmap
	%
	mode_info_get_instmap(ModeInfo1, InstMap0),

	% 
	% Grab the original bag of nondet-live vars
	%
	mode_info_get_nondet_live_vars(ModeInfo1, NondetLiveVars0),

	% 
	% If the goal is not nondet, then nothing is nondet-live,
	% so reset the bag of nondet-live vars to be empty.
	%
	goal_info_get_code_model(GoalInfo0, CodeModel),
	( CodeModel = model_non ->
		ModeInfo2 = ModeInfo1
	;
		mode_info_set_nondet_live_vars([], ModeInfo1, ModeInfo2)
	),

	%
	% Modecheck the goal
	%
	unique_modes__check_goal_2(GoalExpr0, GoalInfo0, GoalExpr,
		ModeInfo2, ModeInfo3),

	% 
	% Restore the original bag of nondet-live vars
	%
	mode_info_set_nondet_live_vars(NondetLiveVars0, ModeInfo3, ModeInfo),

	%
	% Grab the final instmap, compute the change in insts
	% over this goal, and save that instmap_delta in the goal_info.
	%
	mode_info_get_instmap(ModeInfo, InstMap),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo),

	Goal = GoalExpr - GoalInfo.

	% Make all nondet-live variables whose current inst
	% is `unique' become `mostly_unique'.
	%
:- pred make_all_nondet_live_vars_mostly_uniq(mode_info, mode_info).
:- mode make_all_nondet_live_vars_mostly_uniq(mode_info_di, mode_info_uo)
	is det.

make_all_nondet_live_vars_mostly_uniq(ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, FullInstMap0),
	( instmap__is_reachable(FullInstMap0) ->
		instmap__vars_list(FullInstMap0, AllVars),
		select_nondet_live_vars(AllVars, ModeInfo0, NondetLiveVars),
		make_var_list_mostly_uniq(NondetLiveVars, ModeInfo0, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

:- pred select_live_vars(list(var), mode_info, list(var)).
:- mode select_live_vars(in, mode_info_ui, out) is det.

select_live_vars([], _, []).
select_live_vars([Var|Vars], ModeInfo, LiveVars) :-
	( mode_info_var_is_live(ModeInfo, Var, live) ->
		LiveVars = [Var | LiveVars1],
		select_live_vars(Vars, ModeInfo, LiveVars1)
	;
		select_live_vars(Vars, ModeInfo, LiveVars)
	).

:- pred select_nondet_live_vars(list(var), mode_info, list(var)).
:- mode select_nondet_live_vars(in, mode_info_ui, out) is det.

select_nondet_live_vars([], _, []).
select_nondet_live_vars([Var|Vars], ModeInfo, NondetLiveVars) :-
	( mode_info_var_is_nondet_live(ModeInfo, Var, live) ->
		NondetLiveVars = [Var | NondetLiveVars1],
		select_nondet_live_vars(Vars, ModeInfo, NondetLiveVars1)
	;
		select_nondet_live_vars(Vars, ModeInfo, NondetLiveVars)
	).

	% Given a list of variables, a delta instmap, and a mode_info,
	% select all the variables whose inst changed in the delta instmap
	% (other than changes which just add information,
	% e.g. `ground -> bound(42)'.)
	%
:- pred select_changed_inst_vars(list(var), instmap_delta, mode_info,
				list(var)).
:- mode select_changed_inst_vars(in, in, mode_info_ui, out) is det.

select_changed_inst_vars([], _DeltaInstMap, _ModeInfo, []).
select_changed_inst_vars([Var | Vars], DeltaInstMap, ModeInfo, ChangedVars) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_instmap(ModeInfo, InstMap0),
	instmap__lookup_var(InstMap0, Var, Inst0),
	(
		instmap_delta_is_reachable(DeltaInstMap),
		instmap_delta_lookup_var(DeltaInstMap, Var, Inst),
		\+ inst_matches_final(Inst, Inst0, ModuleInfo)
	->
		ChangedVars = [Var | ChangedVars1],
		select_changed_inst_vars(Vars, DeltaInstMap, ModeInfo,
			ChangedVars1)
	;
		select_changed_inst_vars(Vars, DeltaInstMap, ModeInfo,
			ChangedVars)
	).

:- pred make_var_list_mostly_uniq(list(var), mode_info, mode_info).
:- mode make_var_list_mostly_uniq(in, mode_info_di, mode_info_uo) is det.

make_var_list_mostly_uniq([], ModeInfo, ModeInfo).
make_var_list_mostly_uniq([Var | Vars], ModeInfo0, ModeInfo) :-
	make_var_mostly_uniq(Var, ModeInfo0, ModeInfo1),
	make_var_list_mostly_uniq(Vars, ModeInfo1, ModeInfo).

:- pred make_var_mostly_uniq(var, mode_info, mode_info).
:- mode make_var_mostly_uniq(in, mode_info_di, mode_info_uo) is det.

make_var_mostly_uniq(Var, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		%
		% only variables which are `unique' need to be changed
		%
		instmap__is_reachable(InstMap0),
		instmap__vars_list(InstMap0, Vars),
		list__member(Var, Vars),
		instmap__lookup_var(InstMap0, Var, Inst0),
		inst_expand(ModuleInfo0, Inst0, Inst1),
		( Inst1 = ground(unique, _)
		; Inst1 = bound(unique, _)
		; Inst1 = any(unique)
		)
	->
		make_mostly_uniq_inst(Inst0, ModuleInfo0, Inst, ModuleInfo),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
		instmap__set(InstMap0, Var, Inst, InstMap),
		mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

:- pred unique_modes__check_goal_2(hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, mode_info, mode_info).
:- mode unique_modes__check_goal_2(in, in, out, mode_info_di, mode_info_uo)
		is det.

unique_modes__check_goal_2(conj(List0), _GoalInfo0, conj(List)) -->
	mode_checkpoint(enter, "conj"),
	mode_info_add_goals_live_vars(List0),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] }
	;
		unique_modes__check_conj(List0, List)
	),
	mode_checkpoint(exit, "conj").

unique_modes__check_goal_2(disj(List0, SM), GoalInfo0, disj(List, SM)) -->
	mode_checkpoint(enter, "disj"),
	( { List0 = [] } ->
		{ List = [] },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		{ goal_info_get_code_model(GoalInfo0, CodeModel) },
		% does this disjunction create a choice point?
		( { CodeModel = model_non } ->
			%
			% Mark all the variables which are nondet-live at the
			% start of the disjunction and whose inst is `unique'
			% as instead being only `mostly_unique'.
			%
			mode_info_add_live_vars(NonLocals),
			make_all_nondet_live_vars_mostly_uniq,
			mode_info_remove_live_vars(NonLocals)
		;
			[]
		),

		%
		% Now just modecheck each disjunct in turn, and then
		% merge the resulting instmaps.
		%
		unique_modes__check_disj(List0, List, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "disj").

unique_modes__check_goal_2(if_then_else(Vs, A0, B0, C0, SM), GoalInfo0, Goal)
		-->
	mode_checkpoint(enter, "if-then-else"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	{ unique_modes__goal_get_nonlocals(A0, A_Vars) },
	{ unique_modes__goal_get_nonlocals(B0, B_Vars) },
	{ unique_modes__goal_get_nonlocals(C0, C_Vars) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(NonLocals),

	%
	% At this point, we should set the inst of any `unique'
	% variables which occur in the condition and which
	% are live to `mostly_unique'.  However, if a variable's
	% inst was unchanged over the condition (i.e. it remains
	% `unique' on exit from the condition), then it is
	% safe to leave it as `unique' on entry to the condition.
	% The only case we need to set it to `mostly_unique' is
	% if the condition would clobber it.
	%
	mode_info_add_live_vars(C_Vars),
	=(ModeInfo),
	{ set__to_sorted_list(A_Vars, A_Vars_List) },
	{ select_live_vars(A_Vars_List, ModeInfo, A_Live_Vars) },
	{ A0 = _ - A0_GoalInfo },
	{ goal_info_get_instmap_delta(A0_GoalInfo, A0_DeltaInstMap) },
	{ select_changed_inst_vars(A_Live_Vars, A0_DeltaInstMap, ModeInfo,
				ChangedVars) },
	make_var_list_mostly_uniq(ChangedVars),
	mode_info_remove_live_vars(C_Vars),

	mode_info_add_live_vars(B_Vars),
	unique_modes__check_goal(A0, A),
	mode_info_remove_live_vars(B_Vars),
	mode_info_unlock_vars(NonLocals),
	% mode_info_dcg_get_instmap(InstMapA),
	unique_modes__check_goal(B0, B),
	mode_info_dcg_get_instmap(InstMapB),
	mode_info_set_instmap(InstMap0),
	unique_modes__check_goal(C0, C),
	mode_info_dcg_get_instmap(InstMapC),
	mode_info_set_instmap(InstMap0),
	instmap__merge(NonLocals, [InstMapB, InstMapC], if_then_else),
	{ Goal = if_then_else(Vs, A, B, C, SM) },
	mode_checkpoint(exit, "if-then-else").

unique_modes__check_goal_2(not(A0), GoalInfo0, not(A)) -->
	mode_checkpoint(enter, "not"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	mode_info_dcg_get_instmap(InstMap0),
	{ set__to_sorted_list(NonLocals, NonLocalsList) },
	=(ModeInfo),
	{ select_live_vars(NonLocalsList, ModeInfo, LiveNonLocals) },
	make_var_list_mostly_uniq(LiveNonLocals),
	mode_info_lock_vars(NonLocals),
	unique_modes__check_goal(A0, A),
	mode_info_unlock_vars(NonLocals),
	mode_info_set_instmap(InstMap0),
	mode_checkpoint(exit, "not").

unique_modes__check_goal_2(some(Vs, G0), _, some(Vs, G)) -->
	mode_checkpoint(enter, "some"),
	unique_modes__check_goal(G0, G),
	mode_checkpoint(exit, "some").

unique_modes__check_goal_2(higher_order_call(PredVar, Args, Types, Modes, Det),
		_GoalInfo0, Goal) -->
	mode_checkpoint(enter, "higher-order call"),
	mode_info_set_call_context(higher_order_call(predicate)),
	{ determinism_components(Det, _, at_most_zero) ->
		NeverSucceeds = yes
	;
		NeverSucceeds = no
	},
	{ determinism_to_code_model(Det, CodeModel) },
	unique_modes__check_call_modes(Args, Modes, CodeModel, NeverSucceeds),
	{ Goal = higher_order_call(PredVar, Args, Types, Modes, Det) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order call").

unique_modes__check_goal_2(call(PredId, ProcId, Args, Builtin, CallContext,
		PredName), _GoalInfo0, Goal) -->
	mode_checkpoint(enter, "call"),
	mode_info_set_call_context(call(PredId)),
	unique_modes__check_call(PredId, ProcId, Args),
	{ Goal = call(PredId, ProcId, Args, Builtin, CallContext, PredName) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

unique_modes__check_goal_2(unify(A0, B0, _, UnifyInfo0, UnifyContext),
		GoalInfo0, Goal) -->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),

	modecheck_unification(A0, B0, UnifyInfo0, UnifyContext, GoalInfo0,
		check_unique_modes, Goal),

	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

unique_modes__check_goal_2(switch(Var, CanFail, Cases0, SM), GoalInfo0,
		switch(Var, CanFail, Cases, SM)) -->
	mode_checkpoint(enter, "switch"),
	( { Cases0 = [] } ->
		{ Cases = [] },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		unique_modes__check_case_list(Cases0, Var, Cases, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "switch").

	% to modecheck a pragma_c_code, we just modecheck the proc for 
	% which it is the goal.
unique_modes__check_goal_2(pragma_c_code(IsRecursive, C_Code, PredId, ProcId,
		Args, ArgNameMap, OrigArgTypes, ExtraPragmaInfo),
		_GoalInfo, Goal) -->
	mode_checkpoint(enter, "pragma_c_code"),
	mode_info_set_call_context(call(PredId)),
	unique_modes__check_call(PredId, ProcId, Args),
	{ Goal = pragma_c_code(IsRecursive, C_Code, PredId, ProcId, Args,
			ArgNameMap, OrigArgTypes, ExtraPragmaInfo) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "pragma_c_code").

:- pred unique_modes__check_call(pred_id, proc_id, list(var), 
			mode_info, mode_info).
:- mode unique_modes__check_call(in, in, in, mode_info_di, mode_info_uo) is det.

unique_modes__check_call(PredId, ProcId, ArgVars, 
		ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_argmodes(ProcInfo, ProcArgModes0),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	proc_info_never_succeeds(ProcInfo, NeverSucceeds),
	unique_modes__check_call_modes(ArgVars, ProcArgModes0, CodeModel,
				NeverSucceeds, ModeInfo0, ModeInfo).

	% to check a call, we just look up the required initial insts
	% for the arguments of the call, and then check for each
	% argument if the variable is nondet-live and the required initial
	% inst was unique.

:- pred unique_modes__check_call_modes(list(var), list(mode), code_model, bool,
			mode_info, mode_info).
:- mode unique_modes__check_call_modes(in, in, in, in,
			mode_info_di, mode_info_uo) is det.

unique_modes__check_call_modes(ArgVars, ProcArgModes, CodeModel, NeverSucceeds,
			ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
				InitialInsts),
	modecheck_var_has_inst_list(ArgVars, InitialInsts, 0,
				ModeInfo0, ModeInfo1),
	mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
	modecheck_set_var_inst_list(ArgVars, InitialInsts, FinalInsts,
		NewArgVars, ExtraGoals, ModeInfo1, ModeInfo2),
	( NewArgVars = ArgVars, ExtraGoals = [] - [] ->
		true
	;	
		% this shouldn't happen, since modes.m should do
		% all the handling of implied modes
		error("unique_modes.m: call to implied mode?")
	),
	( NeverSucceeds = yes ->
		instmap__init_unreachable(InstMap),
		mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo)
	;
		%
		% Check whether we are at a call to a nondet predicate.
		% If so, mark all the currently nondet-live variables
		% whose inst is `unique' as instead being only `mostly_unique'.
		%
		( CodeModel = model_non ->
			make_all_nondet_live_vars_mostly_uniq(ModeInfo2,
				ModeInfo)
		;
			ModeInfo = ModeInfo2
		)
	).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_conj(list(hlds_goal), list(hlds_goal),
		mode_info, mode_info).
:- mode unique_modes__check_conj(in, out, mode_info_di, mode_info_uo) is det.

	% Just process each conjunct in turn.
	% Note that we don't do any reordering of conjuncts here.

unique_modes__check_conj([], []) --> [].
unique_modes__check_conj([Goal0 | Goals0], [Goal | Goals]) -->
	{ unique_modes__goal_get_nonlocals(Goal0, NonLocals) },
	mode_info_remove_live_vars(NonLocals),
	unique_modes__check_goal(Goal0, Goal),
	unique_modes__check_conj(Goals0, Goals).

%-----------------------------------------------------------------------------%

	% Process each of the disjunctions in turn, making sure to restore
	% the original instmap before processing the next one.
	% Collect up a list of the resulting instmaps.

:- pred unique_modes__check_disj(list(hlds_goal), list(hlds_goal),
		list(instmap), mode_info, mode_info).
:- mode unique_modes__check_disj(in, out, out, mode_info_di, mode_info_uo)
		is det.

unique_modes__check_disj([], [], []) --> [].
unique_modes__check_disj([Goal0 | Goals0], [Goal | Goals],
		[InstMap | InstMaps]) -->
	mode_info_dcg_get_instmap(InstMap0),
	unique_modes__check_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	unique_modes__check_disj(Goals0, Goals, InstMaps).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_case_list(list(case), var, list(case),
		list(instmap), mode_info, mode_info).
:- mode unique_modes__check_case_list(in, in, out, out,
		mode_info_di, mode_info_uo) is det.

unique_modes__check_case_list([], _Var, [], []) --> [].
unique_modes__check_case_list([Case0 | Cases0], Var,
			[Case | Cases], [InstMap | InstMaps]) -->
	{ Case0 = case(ConsId, Goal0) },
	{ Case = case(ConsId, Goal) },
	mode_info_dcg_get_instmap(InstMap0),

		% record the fact that Var was bound to ConsId in the
		% instmap before processing this case
	{ cons_id_arity(ConsId, Arity) },
	{ list__duplicate(Arity, free, ArgInsts) },
	modecheck_set_var_inst(Var,
		bound(unique, [functor(ConsId, ArgInsts)])),

	unique_modes__check_goal(Goal0, Goal1),
	mode_info_dcg_get_instmap(InstMap),
	{ fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal) },
	mode_info_set_instmap(InstMap0),
	unique_modes__check_case_list(Cases0, Var, Cases, InstMaps).

%-----------------------------------------------------------------------------%

:- pred unique_modes__goal_get_nonlocals(hlds_goal, set(var)).
:- mode unique_modes__goal_get_nonlocals(in, out) is det.

unique_modes__goal_get_nonlocals(_Goal - GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

%-----------------------------------------------------------------------------%
