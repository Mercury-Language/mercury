%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mode_info.m.
% Main author: fjh.

% This file defines the mode_info data structure, which is used to hold
% the information we need during mode analysis.

%-----------------------------------------------------------------------------%

:- module check_hlds__mode_info.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_goal.
:- import_module hlds__hlds_data, hlds__instmap.
:- import_module parse_tree__prog_data, check_hlds__mode_errors.
:- import_module check_hlds__delay_info, (parse_tree__inst).
:- import_module map, list, set, bool, assoc_list, std_util, io.

:- interface.

	% The mode_info data structure and access predicates.

	% XXX `side' is not used
:- type mode_context
	--->	call(	
			call_id,
			int		% argument number (offset so that
					% the real arguments start at number 1
					% whereas the type_info arguments
					% have numbers <= 0).
		)
	;	unify(
			unify_context,	% original source of the unification
			side		% LHS or RHS
		)
/**** Not yet used
	;	unify_arg(
			unify_context,
			side,
			cons_id,
			int
		)
****/
	;	uninitialized.

:- type side ---> left ; right.

:- type call_context
	--->	unify(unify_context)
	;	call(call_id).

:- type var_lock_reason
	--->	negation
	;	if_then_else
	;	lambda(pred_or_func)
	;	par_conj
	.

	% Specify how to process goals - using either
	% modes.m or unique_modes.m.
:- type how_to_check_goal
	--->    check_modes
	;       check_unique_modes
	.
	
	% Is mode analysis allowed to change which procedure of a predicate
	% is called. It may not change the called procedure after deforestation
	% has performed a generalisation step, since that could result
	% in selecting a less efficient mode, or one which doesn't even
	% have the same termination behaviour.
	% Also, when rechecking a goal after adding extra goals, it is
	% not necessary to choose again which procedure is to be called.
:- type may_change_called_proc
	--->	may_change_called_proc
	;	may_not_change_called_proc.

:- type locked_vars == assoc_list(var_lock_reason, set(prog_var)).

:- type mode_info.

:- pred mode_info_init(io__state, module_info, pred_id, proc_id, prog_context,
		set(prog_var), instmap, how_to_check_goal,
		may_change_called_proc, mode_info).
:- mode mode_info_init(di, in, in, in, in, in, in, in, in,
		mode_info_uo) is det.

:- pred mode_info_get_io_state(mode_info, io__state).
:- mode mode_info_get_io_state(mode_info_get_io_state, uo) is det.

:- pred mode_info_set_io_state(mode_info, io__state, mode_info).
:- mode mode_info_set_io_state(mode_info_set_io_state, di, mode_info_uo) is det.

:- pred mode_info_get_module_info(mode_info, module_info).
:- mode mode_info_get_module_info(mode_info_ui, out) is det.

:- pred mode_info_set_module_info(mode_info, module_info, mode_info).
:- mode mode_info_set_module_info(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_preds(mode_info, pred_table).
:- mode mode_info_get_preds(mode_info_ui, out) is det.

:- pred mode_info_get_modes(mode_info, mode_table).
:- mode mode_info_get_modes(mode_info_ui, out) is det.

:- pred mode_info_get_insts(mode_info, inst_table).
:- mode mode_info_get_insts(mode_info_ui, out) is det.

:- pred mode_info_get_predid(mode_info, pred_id).
:- mode mode_info_get_predid(mode_info_ui, out) is det.

:- pred mode_info_set_predid(mode_info, pred_id, mode_info).
:- mode mode_info_set_predid(mode_info_ui, in, mode_info_uo) is det.

:- pred mode_info_get_procid(mode_info, proc_id).
:- mode mode_info_get_procid(mode_info_ui, out) is det.

:- pred mode_info_set_procid(mode_info, proc_id, mode_info).
:- mode mode_info_set_procid(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_context(mode_info, prog_context).
:- mode mode_info_get_context(mode_info_ui, out) is det.

:- pred mode_info_set_context(prog_context, mode_info, mode_info).
:- mode mode_info_set_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_mode_context(mode_info, mode_context).
:- mode mode_info_get_mode_context(mode_info_ui, out) is det.

:- pred mode_info_set_mode_context(mode_context, mode_info, mode_info).
:- mode mode_info_set_mode_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_call_context(call_context, mode_info, mode_info).
:- mode mode_info_set_call_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_call_arg_context(int, mode_info, mode_info).
:- mode mode_info_set_call_arg_context(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_unset_call_context(mode_info, mode_info).
:- mode mode_info_unset_call_context(mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_instmap(mode_info, instmap).
:- mode mode_info_get_instmap(mode_info_ui, out) is det.

:- pred mode_info_dcg_get_instmap(instmap, mode_info, mode_info).
:- mode mode_info_dcg_get_instmap(out, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_instmap(instmap, mode_info, mode_info).
:- mode mode_info_set_instmap(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_locked_vars(mode_info, locked_vars).
:- mode mode_info_get_locked_vars(mode_info_ui, out) is det.

:- pred mode_info_set_locked_vars(mode_info, locked_vars, mode_info).
:- mode mode_info_set_locked_vars(mode_info_di, in, mode_info_uo) is det.

:- pred mode_info_get_errors(mode_info, list(mode_error_info)).
:- mode mode_info_get_errors(mode_info_ui, out) is det.

:- pred mode_info_get_num_errors(mode_info, int).
:- mode mode_info_get_num_errors(mode_info_ui, out) is det.

:- pred mode_info_set_errors(list(mode_error_info), mode_info, mode_info).
:- mode mode_info_set_errors(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_live_vars(set(prog_var), mode_info, mode_info).
:- mode mode_info_add_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_remove_live_vars(set(prog_var), mode_info, mode_info).
:- mode mode_info_remove_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_var_list_is_live(list(prog_var), mode_info, list(is_live)).
:- mode mode_info_var_list_is_live(in, mode_info_ui, out) is det.

:- pred mode_info_var_is_live(mode_info, prog_var, is_live).
:- mode mode_info_var_is_live(mode_info_ui, in, out) is det.

:- pred mode_info_var_is_nondet_live(mode_info, prog_var, is_live).
:- mode mode_info_var_is_nondet_live(mode_info_ui, in, out) is det.

:- pred mode_info_get_liveness(mode_info, set(prog_var)).
:- mode mode_info_get_liveness(mode_info_ui, out) is det.

:- pred mode_info_get_liveness_2(list(set(prog_var)), set(prog_var),
		set(prog_var)).
:- mode mode_info_get_liveness_2(in, in, out) is det.

:- pred mode_info_get_varset(mode_info, prog_varset).
:- mode mode_info_get_varset(mode_info_ui, out) is det.

:- pred mode_info_set_varset(prog_varset, mode_info, mode_info).
:- mode mode_info_set_varset(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_instvarset(mode_info, inst_varset).
:- mode mode_info_get_instvarset(mode_info_ui, out) is det.

:- pred mode_info_get_var_types(mode_info, map(prog_var, type)).
:- mode mode_info_get_var_types(mode_info_ui, out) is det.

:- pred mode_info_set_var_types(map(prog_var, type), mode_info, mode_info).
:- mode mode_info_set_var_types(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_types_of_vars(mode_info, list(prog_var), list(type)).
:- mode mode_info_get_types_of_vars(mode_info_ui, in, out) is det.

:- pred mode_info_lock_vars(var_lock_reason, set(prog_var),
		mode_info, mode_info).
:- mode mode_info_lock_vars(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_unlock_vars(var_lock_reason, set(prog_var),
		mode_info, mode_info).
:- mode mode_info_unlock_vars(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_var_is_locked(mode_info, prog_var, var_lock_reason).
:- mode mode_info_var_is_locked(mode_info_ui, in, out) is semidet.

:- pred mode_info_var_is_locked_2(locked_vars, prog_var, var_lock_reason).
:- mode mode_info_var_is_locked_2(in, in, out) is semidet.

:- pred mode_info_get_delay_info(mode_info, delay_info).
:- mode mode_info_get_delay_info(mode_info_no_io, out) is det.

:- pred mode_info_set_delay_info(delay_info, mode_info, mode_info).
:- mode mode_info_set_delay_info(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_live_vars(mode_info, list(set(prog_var))).
:- mode mode_info_get_live_vars(mode_info_ui, out) is det.

:- pred mode_info_set_live_vars(list(set(prog_var)), mode_info, mode_info).
:- mode mode_info_set_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_nondet_live_vars(mode_info, list(set(prog_var))).
:- mode mode_info_get_nondet_live_vars(mode_info_no_io, out) is det.

:- pred mode_info_set_nondet_live_vars(list(set(prog_var)),
		mode_info, mode_info).
:- mode mode_info_set_nondet_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_last_checkpoint_insts(mode_info,
		assoc_list(prog_var, inst)).
:- mode mode_info_get_last_checkpoint_insts(mode_info_no_io, out) is det.

:- pred mode_info_set_last_checkpoint_insts(assoc_list(prog_var, inst),
	mode_info, mode_info).
:- mode mode_info_set_last_checkpoint_insts(in, mode_info_di, mode_info_uo)
	is det.

:- pred mode_info_get_parallel_vars(list(pair(set(prog_var))), mode_info,
		mode_info).
:- mode mode_info_get_parallel_vars(out, mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_parallel_vars(list(pair(set(prog_var))), mode_info,
		mode_info).
:- mode mode_info_set_parallel_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_changed_flag(mode_info, bool).
:- mode mode_info_get_changed_flag(mode_info_no_io, out) is det.

:- pred mode_info_set_changed_flag(bool, mode_info, mode_info).
:- mode mode_info_set_changed_flag(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_how_to_check(mode_info, how_to_check_goal).
:- mode mode_info_get_how_to_check(mode_info_ui, out) is det.

:- pred mode_info_set_how_to_check(how_to_check_goal, mode_info, mode_info).
:- mode mode_info_set_how_to_check(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_get_may_change_called_proc(mode_info,
		may_change_called_proc).
:- mode mode_info_get_may_change_called_proc(mode_info_ui, out) is det.

:- pred mode_info_set_may_change_called_proc(may_change_called_proc,
		mode_info, mode_info).
:- mode mode_info_set_may_change_called_proc(in,
		mode_info_di, mode_info_uo) is det.

:- pred mode_info_set_checking_extra_goals(bool, mode_info, mode_info).
:- mode mode_info_set_checking_extra_goals(in,
		mode_info_di, mode_info_uo) is det.

	% Find the simple_call_id to use in error messages
	% for the given pred_id.
:- pred mode_info_get_call_id(mode_info, pred_id, simple_call_id).
:- mode mode_info_get_call_id(mode_info_ui, in, out) is det.

/*
:- inst uniq_mode_info	=	bound_unique(
					mode_info(
						unique,
						ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).
*/
:- inst uniq_mode_info	=	ground.

:- mode mode_info_uo :: free -> uniq_mode_info.
:- mode mode_info_ui :: uniq_mode_info -> uniq_mode_info.
:- mode mode_info_di :: uniq_mode_info -> dead.

	% Some fiddly modes used when we want to extract
	% the io_state from a mode_info struct and then put it back again.

/*
:- inst mode_info_no_io	=	bound_unique(
					mode_info(
						dead, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).
*/
:- inst mode_info_no_io	=	ground.

:- mode mode_info_get_io_state	:: uniq_mode_info -> mode_info_no_io.
:- mode mode_info_no_io		:: mode_info_no_io -> mode_info_no_io.
:- mode mode_info_set_io_state	:: mode_info_no_io -> dead.

%-----------------------------------------------------------------------------%

        % record a mode error (and associated context info) in the mode_info.

:- pred mode_info_error(set(prog_var), mode_error, mode_info, mode_info).
:- mode mode_info_error(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_error(mode_error_info, mode_info, mode_info).
:- mode mode_info_add_error(in, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__delay_info, check_hlds__mode_errors.
:- import_module check_hlds__mode_util.
:- import_module term, varset.
:- import_module require, std_util, queue.

:- type mode_info 
	--->	mode_info(
			io_state :: io__state,
			module_info :: module_info,
			predid :: pred_id,	% The pred we are checking
			procid :: proc_id,	% The mode which we are checking
			varset :: prog_varset,	
					% The variables in the current proc
			var_types :: vartypes,
					% The types of the variables
			context :: prog_context,
					% The line number of the subgoal we
					% are currently checking
			mode_context :: mode_context,
					% A description of where in the
					% goal the error occurred
			instmap :: instmap,
					% The current instantiatedness
					% of the variables
			locked_vars :: locked_vars,
					% The "locked" variables,
					% i.e. variables which cannot be
					% further instantiated inside a
					% negated context
			delay_info :: delay_info,
					% info about delayed goals
			errors :: list(mode_error_info),
					% The mode errors found

			live_vars :: list(set(prog_var)),
					% The live variables,
	% i.e. those variables which may be referenced again on forward
	% execution or after shallow backtracking.  (By shallow
	% backtracking, I mean semidet backtracking in a negation,
	% if-then-else, or semidet disjunction within the current
	% predicate.)

			nondet_live_vars :: list(set(prog_var)),
					% The nondet-live variables,
	% i.e. those variables which may be referenced again after deep
	% backtracking TO THE CURRENT EXECUTION POINT.  These are the
	% variables which need to be made mostly_unique rather than
	% unique when we get to a nondet disjunction or a nondet call.
	% We do not include variables which may be referenced again
	% after backtracking to a point EARLIER THAN the current
	% execution point, since those variables will *already* have
	% been marked as mostly_unique rather than unique.)

			instvarset :: inst_varset,

			last_checkpoint_insts :: assoc_list(prog_var, inst),
	% This field is used by the checkpoint code when debug_modes is on.
	% It has the instmap that was current at the last mode checkpoint,
	% so that checkpoints do not print out the insts of variables
	% whose insts have not changed since the last checkpoint.
	% This field will always contain an empty list if debug_modes is off,
	% since its information is not needed then.

			parallel_vars ::
				list(pair(set(prog_var), set(prog_var))),
	% A stack of pairs of sets of variables used to mode-check
	% parallel conjunctions. The first set is the nonlocals of
	% the parallel conjunction. The second set is a subset of the
	% first, and is the set of variables that have been [further]
	% bound inside the current parallel conjunct - the stack is for
	% the correct handling of nested parallel conjunctions.

			changed_flag :: bool,
					% Changed flag
					% If `yes', then we may need
					% to repeat mode inference.

			how_to_check :: how_to_check_goal,

			may_change_called_proc :: may_change_called_proc,
					% Is mode analysis allowed
					% to change which procedure
					% is called?

			checking_extra_goals :: bool
					% Are we rechecking a goal after
					% introducing unifications for
					% complicated sub-unifications
					% or an implied mode?
					% If so, redoing the mode check
					% should not introduce more
					% extra unifications.
		).

	% The normal inst of a mode_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

%-----------------------------------------------------------------------------%

	% Initialize the mode_info

mode_info_init(IOState, ModuleInfo, PredId, ProcId, Context,
		LiveVars, InstMapping0, HowToCheck, MayChangeProc, ModeInfo) :-
	mode_context_init(ModeContext),
	LockedVars = [],
	delay_info__init(DelayInfo),
	ErrorList = [],
		% look up the varset and var types
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_inst_varset(ProcInfo, InstVarSet),

	LiveVarsList = [LiveVars],
	NondetLiveVarsList = [LiveVars],

	Changed = no,
	CheckingExtraGoals = no,

	ModeInfo = mode_info(
		IOState, ModuleInfo, PredId, ProcId, VarSet, VarTypes,
		Context, ModeContext, InstMapping0, LockedVars, DelayInfo,
		ErrorList, LiveVarsList, NondetLiveVarsList, InstVarSet, [], [],
		Changed, HowToCheck, MayChangeProc, CheckingExtraGoals
	).

%-----------------------------------------------------------------------------%

	% Lots of very boring access predicates.

mode_info_get_io_state(ModeInfo, IOState) :-
	% XXX
	unsafe_promise_unique(ModeInfo^io_state, IOState).

mode_info_set_io_state(ModeInfo, IOState0, ModeInfo^io_state := IOState) :-
	% XXX
	unsafe_promise_unique(IOState0, IOState).

%-----------------------------------------------------------------------------%

mode_info_get_preds(ModeInfo, Preds) :-
	module_info_preds(ModeInfo^module_info, Preds).

mode_info_get_modes(ModeInfo, Modes) :-
	module_info_modes(ModeInfo^module_info, Modes).

mode_info_get_insts(ModeInfo, Insts) :-
	module_info_insts(ModeInfo^module_info, Insts).

mode_info_get_module_info(MI, MI^module_info).
mode_info_get_predid(MI, MI^predid).
mode_info_get_procid(MI, MI^procid).
mode_info_get_varset(MI, MI^varset).
mode_info_get_var_types(MI, MI^var_types).
mode_info_get_context(MI, MI^context).
mode_info_get_mode_context(MI, MI^mode_context).
mode_info_get_instmap(MI, MI^instmap).
mode_info_get_locked_vars(MI, MI^locked_vars).
mode_info_get_errors(MI, MI^errors).
mode_info_get_delay_info(MI, MI^delay_info).
mode_info_get_live_vars(MI, MI^live_vars).
mode_info_get_nondet_live_vars(MI, MI^nondet_live_vars).
mode_info_get_last_checkpoint_insts(MI, MI^last_checkpoint_insts).
mode_info_get_parallel_vars(PVars) --> PVars =^ parallel_vars.
mode_info_get_changed_flag(MI, MI^changed_flag).
mode_info_get_how_to_check(MI, MI^how_to_check).
mode_info_get_may_change_called_proc(MI, MI^may_change_called_proc).

mode_info_set_module_info(MI, ModuleInfo, MI^module_info := ModuleInfo).
mode_info_set_predid(MI, PredId, MI^predid := PredId).
mode_info_set_procid(MI, ProcId, MI^procid := ProcId).
mode_info_set_varset(VarSet) --> ^varset := VarSet.
mode_info_set_var_types(VTypes) --> ^var_types := VTypes.
mode_info_set_context(Context) --> ^context := Context.
mode_info_set_mode_context(ModeContext) --> ^mode_context := ModeContext.
mode_info_set_locked_vars(MI, LockedVars, MI^locked_vars := LockedVars).
mode_info_set_errors(Errors) --> ^errors := Errors.
mode_info_set_delay_info(DelayInfo) --> ^delay_info := DelayInfo.
mode_info_set_live_vars(LiveVarsList) --> ^live_vars := LiveVarsList.
mode_info_set_nondet_live_vars(NondetLiveVars) -->
	^nondet_live_vars := NondetLiveVars.
mode_info_set_last_checkpoint_insts(LastCheckpointInsts) -->
	^last_checkpoint_insts := LastCheckpointInsts.
mode_info_set_parallel_vars(PVars) --> ^parallel_vars := PVars.
mode_info_set_changed_flag(Changed) --> ^changed_flag := Changed.
mode_info_set_how_to_check(How) --> ^how_to_check := How.
mode_info_set_may_change_called_proc(MayChange) -->
	^may_change_called_proc := MayChange.

%-----------------------------------------------------------------------------%

mode_info_set_call_context(unify(UnifyContext)) -->
	mode_info_set_mode_context(unify(UnifyContext, left)).
mode_info_set_call_context(call(CallId)) -->
	mode_info_set_mode_context(call(CallId, 0)).

mode_info_set_call_arg_context(ArgNum, ModeInfo0, ModeInfo) :-
	mode_info_get_mode_context(ModeInfo0, ModeContext0),
	( ModeContext0 = call(CallId, _) ->
		mode_info_set_mode_context(call(CallId, ArgNum),
			ModeInfo0, ModeInfo)
	; ModeContext0 = unify(_UnifyContext, _Side) ->
		% This only happens when checking that the typeinfo variables
		% for polymorphic complicated unifications are ground.
		% For that case, we don't care about the ArgNum.
		ModeInfo = ModeInfo0
	;
		error("mode_info_set_call_arg_context")
	).

mode_info_unset_call_context -->
	mode_info_set_mode_context(uninitialized).

%-----------------------------------------------------------------------------%

	% mode_info_dcg_get_instmap/3 is the same as mode_info_get_instmap/2
	% except that it's easier to use inside a DCG.

mode_info_dcg_get_instmap(InstMap, ModeInfo, ModeInfo) :-
	mode_info_get_instmap(ModeInfo, InstMap).

%-----------------------------------------------------------------------------%

mode_info_set_instmap(InstMap) -->
	InstMap0 =^ instmap,
	^instmap := InstMap,
	(
		{ instmap__is_unreachable(InstMap) },
		{ instmap__is_reachable(InstMap0) }
	->
		DelayInfo0 =^ delay_info,
		{ delay_info__bind_all_vars(DelayInfo0, DelayInfo) },
		^delay_info := DelayInfo
	;
		[]
	).

%-----------------------------------------------------------------------------%

mode_info_get_num_errors(ModeInfo, NumErrors) :-
	list__length(ModeInfo^errors, NumErrors).

%-----------------------------------------------------------------------------%

	% We keep track of the live variables and the nondet-live variables
	% a bag, represented as a list of sets of vars.
	% This allows us to easily add and remove sets of variables.
	% It's probably not maximally efficient.

	% Add a set of vars to the bag of live vars and
	% the bag of nondet-live vars.

mode_info_add_live_vars(NewLiveVars) -->
	LiveVars0 =^ live_vars,
	NondetLiveVars0 =^ nondet_live_vars,
	^live_vars := [NewLiveVars | LiveVars0],
	^nondet_live_vars := [NewLiveVars | NondetLiveVars0].

	% Remove a set of vars from the bag of live vars and
	% the bag of nondet-live vars.

mode_info_remove_live_vars(OldLiveVars) -->
	LiveVars0 =^ live_vars,
	NondetLiveVars0 =^ nondet_live_vars,
	{
		list__delete_first(LiveVars0, OldLiveVars, LiveVars1),
		list__delete_first(NondetLiveVars0, OldLiveVars,
			NondetLiveVars1)
	->
		LiveVars = LiveVars1,
		NondetLiveVars = NondetLiveVars1
	;
		error("mode_info_remove_live_vars: failed")
	},
	^live_vars := LiveVars,
	^nondet_live_vars := NondetLiveVars,
		% when a variable becomes dead, we may be able to wake
		% up a goal which is waiting on that variable
	{ set__to_sorted_list(OldLiveVars, VarList) },
	DelayInfo0 =^ delay_info,
	{ delay_info__bind_var_list(VarList, DelayInfo0, DelayInfo) },
	^delay_info := DelayInfo.

	% Check whether a list of variables are live or not

mode_info_var_list_is_live([], _, []).
mode_info_var_list_is_live([Var | Vars], ModeInfo, [Live | Lives]) :-
	mode_info_var_is_live(ModeInfo, Var, Live),
	mode_info_var_list_is_live(Vars, ModeInfo, Lives).

	% Check whether a variable is live or not

mode_info_var_is_live(ModeInfo, Var, Result) :-
	(
		% some [LiveVars] 
		list__member(LiveVars, ModeInfo^live_vars),
		set__member(Var, LiveVars)
	->
		Result = live
	;
		Result = dead
	).

	% Check whether a variable is nondet_live or not.

mode_info_var_is_nondet_live(ModeInfo, Var, Result) :-
	(
		% some [LiveVars] 
		list__member(LiveVars, ModeInfo^nondet_live_vars),
		set__member(Var, LiveVars)
	->
		Result = live
	;
		Result = dead
	).

mode_info_get_liveness(ModeInfo, LiveVars) :-
	set__init(LiveVars0),
	mode_info_get_liveness_2(ModeInfo^live_vars, LiveVars0, LiveVars).

mode_info_get_liveness_2([], LiveVars, LiveVars).
mode_info_get_liveness_2([LiveVarsSet | LiveVarsList], LiveVars0, LiveVars) :-
	set__union(LiveVars0, LiveVarsSet, LiveVars1),
	mode_info_get_liveness_2(LiveVarsList, LiveVars1, LiveVars).

%-----------------------------------------------------------------------------%

mode_info_get_instvarset(ModeInfo, ModeInfo^instvarset).

mode_info_get_types_of_vars(ModeInfo, Vars, TypesOfVars) :-
	mode_info_get_var_types(ModeInfo, VarTypes),
	map__apply_to_list(Vars, VarTypes, TypesOfVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The locked variables are stored as a stack 
	% of sets of variables.  A variable is locked if it is
	% a member of any of the sets.  To lock a set of vars, we just
	% push them on the stack, and to unlock a set of vars, we just
	% pop them off the stack.  The stack is implemented as a list.

mode_info_lock_vars(Reason, Vars, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars),
	mode_info_set_locked_vars(ModeInfo0, [Reason - Vars | LockedVars],
			ModeInfo).

mode_info_unlock_vars(Reason, Vars, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars0),
	(
		LockedVars0 = [Reason - TheseVars | LockedVars1],
		set__equal(TheseVars, Vars)
	->
		LockedVars = LockedVars1
	;
		error("mode_info_unlock_vars: some kind of nesting error")
	),
	mode_info_set_locked_vars(ModeInfo0, LockedVars, ModeInfo).

mode_info_var_is_locked(ModeInfo, Var, Reason) :-
	mode_info_get_locked_vars(ModeInfo, LockedVarsList),
	mode_info_var_is_locked_2(LockedVarsList, Var, Reason).

mode_info_var_is_locked_2([ThisReason - Set | Sets], Var, Reason) :-
	(
		set__member(Var, Set)
	->
		Reason = ThisReason
	;
		mode_info_var_is_locked_2(Sets, Var, Reason)
	).

mode_info_set_checking_extra_goals(Checking) -->
	( yes =^ checking_extra_goals, { Checking = yes } ->
		% This should never happen - once the extra goals are
		% introduced, rechecking the goal should not introduce
		% more extra goals.
		{ error(
		"mode analysis: rechecking extra goals adds more extra goals") }
	;
		^checking_extra_goals := Checking
	).

%-----------------------------------------------------------------------------%

mode_info_get_call_id(ModeInfo, PredId, CallId) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_call_id(PredInfo, CallId).

%-----------------------------------------------------------------------------%

mode_info_error(Vars, ModeError, ModeInfo0, ModeInfo) :-
        mode_info_get_context(ModeInfo0, Context),
        mode_info_get_mode_context(ModeInfo0, ModeContext),
        ModeErrorInfo = mode_error_info(Vars, ModeError, Context, ModeContext),
        mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo).

mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo) :-
        mode_info_get_errors(ModeInfo0, Errors0),
        list__append(Errors0, [ModeErrorInfo], Errors),
        mode_info_set_errors(Errors, ModeInfo0, ModeInfo).

%-----------------------------------------------------------------------------%
