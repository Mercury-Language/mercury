%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2004 The University of Melbourne.
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

:- import_module check_hlds__delay_info.
:- import_module check_hlds__mode_errors.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__prog_data.

:- import_module map, list, set, bool, assoc_list, std_util.

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
	;	par_conj.

	% Specify how to process goals - using either
	% modes.m or unique_modes.m.
:- type how_to_check_goal
	--->    check_modes
	;       check_unique_modes.

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

:- pred mode_info_init(module_info::in, pred_id::in,
	proc_id::in, prog_context::in, set(prog_var)::in, instmap::in,
	how_to_check_goal::in, may_change_called_proc::in,
	mode_info::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_get_module_info(mode_info::in, module_info::out) is det.
:- pred mode_info_get_preds(mode_info::in, pred_table::out) is det.
:- pred mode_info_get_modes(mode_info::in, mode_table::out) is det.
:- pred mode_info_get_insts(mode_info::in, inst_table::out) is det.
:- pred mode_info_get_predid(mode_info::in, pred_id::out) is det.
:- pred mode_info_get_procid(mode_info::in, proc_id::out) is det.
:- pred mode_info_get_context(mode_info::in, prog_context::out) is det.
:- pred mode_info_get_mode_context(mode_info::in, mode_context::out) is det.
:- pred mode_info_get_instmap(mode_info::in, instmap::out) is det.
:- pred mode_info_get_locked_vars(mode_info::in, locked_vars::out) is det.
:- pred mode_info_get_errors(mode_info::in, list(mode_error_info)::out) is det.
:- pred mode_info_get_num_errors(mode_info::in, int::out) is det.
:- pred mode_info_get_liveness(mode_info::in, set(prog_var)::out) is det.
:- pred mode_info_get_varset(mode_info::in, prog_varset::out) is det.
:- pred mode_info_get_instvarset(mode_info::in, inst_varset::out) is det.
:- pred mode_info_get_var_types(mode_info::in,
	map(prog_var, type)::out) is det.
:- pred mode_info_get_delay_info(mode_info::in, delay_info::out) is det.
:- pred mode_info_get_live_vars(mode_info::in,
	list(set(prog_var))::out) is det.
:- pred mode_info_get_nondet_live_vars(mode_info::in,
	list(set(prog_var))::out) is det.
:- pred mode_info_get_last_checkpoint_insts(mode_info::in,
	assoc_list(prog_var, inst)::out) is det.
:- pred mode_info_get_parallel_vars(mode_info::in,
	list(pair(set(prog_var)))::out) is det.
:- pred mode_info_get_changed_flag(mode_info::in, bool::out) is det.
:- pred mode_info_get_how_to_check(mode_info::in,
	how_to_check_goal::out) is det.
:- pred mode_info_get_may_change_called_proc(mode_info::in,
	may_change_called_proc::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_set_module_info(module_info::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_predid(pred_id::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_procid(proc_id::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_context(prog_context::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_mode_context(mode_context::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_call_context(call_context::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_call_arg_context(int::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_unset_call_context(
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_instmap(instmap::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_locked_vars(locked_vars::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_errors(list(mode_error_info)::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_add_live_vars(set(prog_var)::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_remove_live_vars(set(prog_var)::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_varset(prog_varset::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_var_types(map(prog_var, type)::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_delay_info(delay_info::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_live_vars(list(set(prog_var))::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_nondet_live_vars(list(set(prog_var))::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_last_checkpoint_insts(assoc_list(prog_var, inst)::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_parallel_vars(list(pair(set(prog_var)))::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_changed_flag(bool::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_how_to_check(how_to_check_goal::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_may_change_called_proc(may_change_called_proc::in,
	mode_info::in, mode_info::out) is det.
:- pred mode_info_set_checking_extra_goals(bool::in,
	mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_get_types_of_vars(mode_info::in,
	list(prog_var)::in, list(type)::out) is det.

:- pred mode_info_lock_vars(var_lock_reason::in, set(prog_var)::in,
	mode_info::in, mode_info::out) is det.

:- pred mode_info_unlock_vars(var_lock_reason::in, set(prog_var)::in,
	mode_info::in, mode_info::out) is det.

:- pred mode_info_var_is_locked(mode_info::in, prog_var::in,
	var_lock_reason::out) is semidet.

	% Find the simple_call_id to use in error messages
	% for the given pred_id.
:- pred mode_info_get_call_id(mode_info::in, pred_id::in,
	simple_call_id::out) is det.

:- pred mode_info_var_list_is_live(mode_info::in, list(prog_var)::in,
	list(is_live)::out) is det.

:- pred mode_info_var_is_live(mode_info::in, prog_var::in,
	is_live::out) is det.

:- pred mode_info_var_is_nondet_live(mode_info::in, prog_var::in,
	is_live::out) is det.

%-----------------------------------------------------------------------------%

        % record a mode error (and associated context info) in the mode_info.

:- pred mode_info_error(set(prog_var)::in, mode_error::in,
	mode_info::in, mode_info::out) is det.

:- pred mode_info_add_error(mode_error_info::in,
	mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

	% The mode_info contains a flag indicating whether initialisation
	% calls, converting a solver variable from `free' to `any', may be
	% inserted during mode analysis.

:- pred mode_info_may_initialise_solver_vars(mode_info::in)
		is semidet.

:- pred mode_info_set_may_initialise_solver_vars(bool::in,
		mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__delay_info.
:- import_module check_hlds__mode_errors.
:- import_module check_hlds__mode_util.

:- import_module string, term, varset.
:- import_module require, std_util, queue.

:- type mode_info --->
	mode_info(
		module_info	:: module_info,
		predid		:: pred_id,
				% The pred we are checking
		procid		:: proc_id,
				% The mode which we are checking
		varset		:: prog_varset,
				% The variables in the current proc
		var_types	:: vartypes,
				% The types of the variables
		context		:: prog_context,
				% The line number of the subgoal we
				% are currently checking
		mode_context	:: mode_context,
				% A description of where in the
				% goal the error occurred
		instmap		:: instmap,
				% The current instantiatedness
				% of the variables
		locked_vars	:: locked_vars,
				% The "locked" variables,
				% i.e. variables which cannot be
				% further instantiated inside a
				% negated context
		delay_info	:: delay_info,
				% info about delayed goals
		errors		:: list(mode_error_info),
				% The mode errors found

		live_vars	:: list(set(prog_var)),
				% The live variables, i.e. those variables
				% which may be referenced again on forward
				% execution or after shallow backtracking.
				% (By shallow backtracking, I mean semidet
				% backtracking in a negation, if-then-else,
				% or semidet disjunction within the current
				% predicate.)

		nondet_live_vars :: list(set(prog_var)),
				% The nondet-live variables,
				% i.e. those variables which may be
				% referenced again after deep backtracking
				% TO THE CURRENT EXECUTION POINT.
				% These are the variables which need to be
				% made mostly_unique rather than unique
				% when we get to a nondet disjunction
				% or a nondet call.  We do not include
				% variables which may be referenced again
				% after backtracking to a point EARLIER THAN
				% the current execution point, since those
				% variables will *already* have been marked
				% as mostly_unique rather than unique.)

		instvarset	:: inst_varset,

		last_checkpoint_insts :: assoc_list(prog_var, inst),
				% This field is used by the checkpoint
				% code when debug_modes is on.  It has the
				% instmap that was current at the last
				% mode checkpoint, so that checkpoints
				% do not print out the insts of variables
				% whose insts have not changed since the
				% last checkpoint.  This field will always
				% contain an empty list if debug_modes
				% is off, since its information is not
				% needed then.

		parallel_vars	:: list(pair(set(prog_var), set(prog_var))),
				% A stack of pairs of sets of variables used
				% to mode-check parallel conjunctions. The
				% first set is the nonlocals of the
				% parallel conjunction. The second set is
				% a subset of the first, and is the set of
				% variables that have been [further] bound
				% inside the current parallel conjunct -
				% the stack is for the correct handling
				% of nested parallel conjunctions.

		changed_flag	:: bool,
				% Changed flag
				% If `yes', then we may need
				% to repeat mode inference.

		how_to_check	:: how_to_check_goal,

		may_change_called_proc :: may_change_called_proc,
				% Is mode analysis allowed
				% to change which procedure
				% is called?

		checking_extra_goals :: bool,
				% Are we rechecking a goal after
				% introducing unifications for
				% complicated sub-unifications
				% or an implied mode?
				% If so, redoing the mode check
				% should not introduce more
				% extra unifications.

		may_initialise_solver_vars :: bool
				% `yes' if calls to the initialisation
				% predicates for solver vars can be
				% inserted during mode analysis in order
				% to make goals schedulable.
	).

%-----------------------------------------------------------------------------%

	% Initialize the mode_info

mode_info_init(ModuleInfo, PredId, ProcId, Context, LiveVars, InstMapping0,
		HowToCheck, MayChangeProc, ModeInfo) :-
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
	MayInitSolverVars = no,

	ModeInfo = mode_info(ModuleInfo, PredId, ProcId, VarSet, VarTypes,
		Context, ModeContext, InstMapping0, LockedVars, DelayInfo,
		ErrorList, LiveVarsList, NondetLiveVarsList, InstVarSet,
		[], [], Changed, HowToCheck, MayChangeProc,
		CheckingExtraGoals, MayInitSolverVars
	).

%-----------------------------------------------------------------------------%

mode_info_get_module_info(MI, MI ^ module_info).
mode_info_get_predid(MI, MI ^ predid).
mode_info_get_procid(MI, MI ^ procid).
mode_info_get_varset(MI, MI ^ varset).
mode_info_get_var_types(MI, MI ^ var_types).
mode_info_get_context(MI, MI ^ context).
mode_info_get_mode_context(MI, MI ^ mode_context).
mode_info_get_instmap(MI, MI ^ instmap).
mode_info_get_locked_vars(MI, MI ^ locked_vars).
mode_info_get_errors(MI, MI ^ errors).
mode_info_get_delay_info(MI, MI ^ delay_info).
mode_info_get_live_vars(MI, MI ^ live_vars).
mode_info_get_nondet_live_vars(MI, MI ^ nondet_live_vars).
mode_info_get_last_checkpoint_insts(MI, MI ^ last_checkpoint_insts).
mode_info_get_parallel_vars(MI, MI ^  parallel_vars).
mode_info_get_changed_flag(MI, MI ^ changed_flag).
mode_info_get_how_to_check(MI, MI ^ how_to_check).
mode_info_get_may_change_called_proc(MI, MI ^ may_change_called_proc).

mode_info_set_module_info(ModuleInfo, MI, MI ^ module_info := ModuleInfo).
mode_info_set_predid(PredId, MI, MI ^ predid := PredId).
mode_info_set_procid(ProcId, MI, MI ^ procid := ProcId).
mode_info_set_varset(VarSet, MI, MI ^ varset := VarSet).
mode_info_set_var_types(VTypes, MI, MI ^ var_types := VTypes).
mode_info_set_context(Context, MI, MI ^ context := Context).
mode_info_set_mode_context(ModeContext, MI, MI ^ mode_context := ModeContext).
mode_info_set_locked_vars(LockedVars, MI, MI ^ locked_vars := LockedVars).
mode_info_set_errors(Errors, MI, MI ^ errors := Errors).
mode_info_set_delay_info(DelayInfo, MI, MI ^ delay_info := DelayInfo).
mode_info_set_live_vars(LiveVarsList, MI, MI ^ live_vars := LiveVarsList).
mode_info_set_nondet_live_vars(NondetLiveVars, MI,
	MI ^ nondet_live_vars := NondetLiveVars).
mode_info_set_last_checkpoint_insts(LastCheckpointInsts, MI,
	MI ^ last_checkpoint_insts := LastCheckpointInsts).
mode_info_set_parallel_vars(PVars, MI, MI ^ parallel_vars := PVars).
mode_info_set_changed_flag(Changed, MI, MI ^ changed_flag := Changed).
mode_info_set_how_to_check(How, MI, MI ^ how_to_check := How).
mode_info_set_may_change_called_proc(MayChange, MI,
	MI ^ may_change_called_proc := MayChange).

%-----------------------------------------------------------------------------%

mode_info_get_preds(ModeInfo, Preds) :-
	module_info_preds(ModeInfo ^ module_info, Preds).

mode_info_get_modes(ModeInfo, Modes) :-
	module_info_modes(ModeInfo ^ module_info, Modes).

mode_info_get_insts(ModeInfo, Insts) :-
	module_info_insts(ModeInfo ^ module_info, Insts).

mode_info_set_call_context(unify(UnifyContext), !MI) :-
	mode_info_set_mode_context(unify(UnifyContext, left), !MI).
mode_info_set_call_context(call(CallId), !MI) :-
	mode_info_set_mode_context(call(CallId, 0), !MI).

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

mode_info_unset_call_context(!MI) :-
	mode_info_set_mode_context(uninitialized, !MI).

%-----------------------------------------------------------------------------%

mode_info_set_instmap(InstMap, !MI) :-
	InstMap0 = !.MI ^ instmap,
	!:MI = !.MI ^ instmap := InstMap,
	(
		instmap__is_unreachable(InstMap),
		instmap__is_reachable(InstMap0)
	->
		DelayInfo0 = !.MI ^ delay_info,
		delay_info__bind_all_vars(DelayInfo0, DelayInfo),
		!:MI = !.MI ^ delay_info := DelayInfo
	;
		true
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

mode_info_add_live_vars(NewLiveVars, !MI) :-
	LiveVars0 = !.MI ^ live_vars,
	NondetLiveVars0 = !.MI ^ nondet_live_vars,
	!:MI = !.MI ^ live_vars := [NewLiveVars | LiveVars0],
	!:MI = !.MI ^ nondet_live_vars := [NewLiveVars | NondetLiveVars0].

	% Remove a set of vars from the bag of live vars and
	% the bag of nondet-live vars.

mode_info_remove_live_vars(OldLiveVars, !MI) :-
	LiveVars0 = !.MI ^ live_vars,
	NondetLiveVars0 = !.MI ^ nondet_live_vars,
	(
		list__delete_first(LiveVars0, OldLiveVars, LiveVars1),
		list__delete_first(NondetLiveVars0, OldLiveVars,
			NondetLiveVars1)
	->
		LiveVars = LiveVars1,
		NondetLiveVars = NondetLiveVars1
	;
		error("mode_info_remove_live_vars: failed")
	),
	!:MI = !.MI ^ live_vars := LiveVars,
	!:MI = !.MI ^ nondet_live_vars := NondetLiveVars,
		% when a variable becomes dead, we may be able to wake
		% up a goal which is waiting on that variable
	set__to_sorted_list(OldLiveVars, VarList),
	DelayInfo0 = !.MI ^ delay_info,
	delay_info__bind_var_list(VarList, DelayInfo0, DelayInfo),
	!:MI = !.MI ^ delay_info := DelayInfo.

	% Check whether a list of variables are live or not

mode_info_var_list_is_live(_, [], []).
mode_info_var_list_is_live(ModeInfo, [Var | Vars], [Live | Lives]) :-
	mode_info_var_is_live(ModeInfo, Var, Live),
	mode_info_var_list_is_live(ModeInfo, Vars, Lives).

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
	mode_info_get_liveness_2(ModeInfo ^ live_vars, LiveVars0, LiveVars).

:- pred mode_info_get_liveness_2(list(set(prog_var))::in, set(prog_var)::in,
	set(prog_var)::out) is det.

mode_info_get_liveness_2([], LiveVars, LiveVars).
mode_info_get_liveness_2([LiveVarsSet | LiveVarsList], LiveVars0, LiveVars) :-
	set__union(LiveVars0, LiveVarsSet, LiveVars1),
	mode_info_get_liveness_2(LiveVarsList, LiveVars1, LiveVars).

%-----------------------------------------------------------------------------%

mode_info_get_instvarset(ModeInfo, ModeInfo ^ instvarset).

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

mode_info_lock_vars(Reason, Vars, !ModeInfo) :-
	mode_info_get_locked_vars(!.ModeInfo, LockedVars),
	mode_info_set_locked_vars([Reason - Vars | LockedVars], !ModeInfo).

mode_info_unlock_vars(Reason, Vars, !ModeInfo) :-
	mode_info_get_locked_vars(!.ModeInfo, LockedVars0),
	(
		LockedVars0 = [Reason - TheseVars | LockedVars1],
		set__equal(TheseVars, Vars)
	->
		LockedVars = LockedVars1
	;
		error("mode_info_unlock_vars: some kind of nesting error")
	),
	mode_info_set_locked_vars(LockedVars, !ModeInfo).

mode_info_var_is_locked(ModeInfo, Var, Reason) :-
	mode_info_get_locked_vars(ModeInfo, LockedVarsList),
	mode_info_var_is_locked_2(LockedVarsList, Var, Reason).

:- pred mode_info_var_is_locked_2(locked_vars::in, prog_var::in,
	var_lock_reason::out) is semidet.

mode_info_var_is_locked_2([ThisReason - Set | Sets], Var, Reason) :-
	( set__member(Var, Set) ->
		Reason = ThisReason
	;
		mode_info_var_is_locked_2(Sets, Var, Reason)
	).

mode_info_set_checking_extra_goals(Checking, !MI) :-
	(
		yes = !.MI ^ checking_extra_goals,
		Checking = yes
	->
		% This should never happen - once the extra goals are
		% introduced, rechecking the goal should not introduce
		% more extra goals.
		error("mode analysis: rechecking extra goals " ++
			"adds more extra goals")
	;
		!:MI = !.MI ^ checking_extra_goals := Checking
	).

%-----------------------------------------------------------------------------%

mode_info_get_call_id(ModeInfo, PredId, CallId) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_call_id(PredInfo, CallId).

%-----------------------------------------------------------------------------%

mode_info_error(Vars, ModeError, !ModeInfo) :-
        mode_info_get_context(!.ModeInfo, Context),
        mode_info_get_mode_context(!.ModeInfo, ModeContext),
        ModeErrorInfo = mode_error_info(Vars, ModeError, Context, ModeContext),
        mode_info_add_error(ModeErrorInfo, !ModeInfo).

mode_info_add_error(ModeErrorInfo, !ModeInfo) :-
        mode_info_get_errors(!.ModeInfo, Errors0),
        list__append(Errors0, [ModeErrorInfo], Errors),
        mode_info_set_errors(Errors, !ModeInfo).

%-----------------------------------------------------------------------------%

mode_info_may_initialise_solver_vars(ModeInfo) :-
	ModeInfo ^ may_initialise_solver_vars = yes.

mode_info_set_may_initialise_solver_vars(MayInit, !ModeInfo) :-
	!:ModeInfo = !.ModeInfo ^ may_initialise_solver_vars := MayInit.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
