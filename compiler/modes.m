%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modes.m.
% Main author: fjh.
%
% This file contains a mode-checker.
% Basically what this module does is to traverse the HLDS,
% mode-checking each procedure.  For each procedure,
% it will reorder the procedure body if necessary, check that the
% procedure body is mode-correct, annotate each sub_goal with its mode.
% This pass also determines whether or not unifications can fail.
% It also converts unifications with higher-order predicate terms into
% unifications with lambda expressions.

% The input to this pass must be type-correct and in superhomogeneous form.

% This pass does not check that `unique' modes are not used in contexts
% which might require backtracking - that is done by unique_modes.m.
% Changes here may also require changes to unique_modes.m.

% XXX we need to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is 
%     dead (causes problems handling e.g. `same_length').
% XXX break unifications into "micro-unifications"
% XXX would even the above fixes be enough?

/*************************************
To mode-check a clause:
	1.  Initialize the insts of the head variables.
	2.  Mode-check the goal.
	3.  Check that the final insts of the head variables
	    matches that specified in the mode declaration.

To mode-check a goal:
If goal is
	(a) a disjunction
		Mode-check the sub-goals;
		check that the final insts of all the non-local
		variables are the same for all the sub-goals.
	(b) a conjunction
		Attempt to schedule each sub-goal.  If a sub-goal can
		be scheduled, then schedule it, otherwise delay it.
		Continue with the remaining sub-goals until there are
		no goals left.  Every time a variable gets bound,
		see whether we should wake up a delayed goal,
		and if so, wake it up next time we get back to
		the conjunction.  If there are still delayed goals
		hanging around at the end of the conjunction, 
		report a mode error.
	(c) a negation
		Mode-check the sub-goal.
		Check that the sub-goal does not further instantiate
		any non-local variables.  (Actually, rather than
		doing this check after we mode-check the subgoal,
		we instead "lock" the non-local variables, and
		disallow binding of locked variables.)
	(d) a unification
		Check that the unification doesn't attempt to unify
		two free variables (or in general two free sub-terms)
		unless one of them is dead. (Also split unifications
		up if necessary to avoid complicated sub-unifications.)
	(e) a predicate call
		Check that there is a mode declaration for the
		predicate which matches the current instantiation of
		the arguments.  (Also handle calls to implied modes.)
	(f) an if-then-else
		Attempt to schedule the condition.  If successful,
		then check that it doesn't further instantiate any
		non-local variables, mode-check the `then' part
		and the `else' part, and then check that the final
		insts match.  (Perhaps also think about expanding
		if-then-elses so that they can be run backwards,
		if the condition can't be scheduled?)

To attempt to schedule a goal, first mode-check the goal.  If mode-checking
succeeds, then scheduling succeeds.  If mode-checking would report
an error due to the binding of a non-local variable, then scheduling
fails.  If mode-checking would report an error due to the binding of
a local variable, then report the error [this idea not yet implemented].

Note that the notion of liveness used here is different to that
used in liveness.m and the code generator.  Here, we consider
a variable live if its value will be used later on in the computation.

******************************************/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modes.
:- interface.
:- import_module hlds, io.

:- pred modecheck(module_info, module_info, io__state, io__state).
:- mode modecheck(in, out, di, uo) is det.

:- pred modecheck_pred_mode(pred_id, pred_info, module_info, module_info,
	int, io__state, io__state).
% :- mode modecheck_pred_mode(in, in, di, uo, out, di, uo) is det.
:- mode modecheck_pred_mode(in, in, in, out, out, di, uo) is det.

:- pred modecheck_proc(proc_id, pred_id, module_info, module_info, int,
			io__state, io__state).
:- mode modecheck_proc(in, in, in, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by unique_modes.m.

:- import_module mode_info, mode_errors.

	% If there were any errors recorded in the mode_info,
	% report them to the user now.
	%
:- pred modecheck_report_errors(mode_info, mode_info).
:- mode modecheck_report_errors(mode_info_di, mode_info_uo) is det.

	% Modecheck a unification.

	% This argument specifies how to recursively process lambda goals -
	% using either modes.m or unique_modes.m.
:- type how_to_check_goal
	--->	check_modes
	;	check_unique_modes.

:- pred modecheck_unification( var, unify_rhs, unification, unify_context,
			hlds__goal_info, how_to_check_goal,
			var, unify_rhs, pair(list(hlds__goal)), pair(mode),
			unification, mode_info, mode_info).
:- mode modecheck_unification(in, in, in, in, in, in, out, out, out, out, out,
			mode_info_di, mode_info_uo) is det.

	% handle_extra_goals combines MainGoal and ExtraGoals into a single
	% hlds__goal_expr.
	%
:- pred handle_extra_goals(hlds__goal_expr, pair(list(hlds__goal)),
		hlds__goal_info, list(var), list(var),
		mode_info, mode_info, hlds__goal_expr).
:- mode handle_extra_goals(in, in, in, in, in, mode_info_ui, mode_info_ui, out)
	is det.

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.
	%
:- pred compute_instmap_delta(instmap, instmap, set(var), instmap_delta).
:- mode compute_instmap_delta(in, in, in, out) is det.

	% Print a debugging message which includes the port, message string,
	% and the current instmap (but only if `--debug-modes' was enabled).
	%
:- pred mode_checkpoint(port, string, mode_info, mode_info).
:- mode mode_checkpoint(in, in, mode_info_di, mode_info_uo) is det.

:- type port
	--->	enter
	;	exit
	;	wakeup.

	% instmap_merge(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables, 
	%	checking that it is the same for every branch.
	%
:- pred instmap_merge(set(var), list(instmap), merge_context,
		mode_info, mode_info).
:- mode instmap_merge(in, in, in, mode_info_di, mode_info_uo) is det.

 	% given the right-hand-side of a unification, return a list of
	% the potentially non-local variables of that unification.
	%
:- pred unify_rhs_vars(unify_rhs, list(var)).
:- mode unify_rhs_vars(in, out) is det.

	% Given the head vars and the final insts of a predicate,
	% work out which of those variables may be used again
	% by the caller of that predicate (i.e. which variables
	% did not have final inst `clobbered').
	%
:- pred get_live_vars(list(var), list(inst), module_info, list(var)).
:- mode get_live_vars(in, in, in, out) is det.

	% Given a list of variables and a list of initial insts, ensure
	% that the inst of each variable matches the corresponding initial
	% inst.
	%
:- pred modecheck_var_has_inst_list(list(var), list(inst), int, mode_info,
					mode_info).
:- mode modecheck_var_has_inst_list(in, in, in, mode_info_di, mode_info_uo)
	is det.

:- pred modecheck_set_var_inst(var, inst, mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, mode_info_di, mode_info_uo) is det.

:- pred modecheck_set_var_inst_list(list(var), list(inst), list(inst),
					list(var), pair(list(hlds__goal)),
					mode_info, mode_info).
:- mode modecheck_set_var_inst_list(in, in, in, out, out,
					mode_info_di, mode_info_uo) is det.

	% check that the final insts of the head vars matches their
	% expected insts
	%
:- pred modecheck_final_insts(list(var), list(inst), mode_info, mode_info).
:- mode modecheck_final_insts(in, in, mode_info_di, mode_info_uo) is det.

	% mode_info_never_succeeds(ModeInfo, PredId, ProcId, Result):
	% return Result = yes if the called predicate is known to never succeed.
	%
:- pred mode_info_never_succeeds(mode_info, pred_id, proc_id, bool).
:- mode mode_info_never_succeeds(mode_info_ui, in, in, out) is det.

:- pred mode_info_add_goals_live_vars(list(hlds__goal), mode_info, mode_info).
:- mode mode_info_add_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_remove_goals_live_vars(list(hlds__goal), mode_info,
					mode_info).
:- mode mode_info_remove_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module undef_modes, mode_info, delay_info, mode_errors, inst_match.
:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module assoc_list.
:- import_module type_util, mode_util, code_util, prog_io, unify_proc.
:- import_module globals, options, mercury_to_mercury, hlds_out, int, set.
:- import_module passes_aux.
:- import_module unique_modes, (lambda).

%-----------------------------------------------------------------------------%

modecheck(Module0, Module) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose,
		"% Checking for undefined insts and modes...\n"),
	check_undefined_modes(Module0, Module1, FoundUndefError),
	maybe_report_stats(Statistics),
	( { FoundUndefError = yes } ->
		{ module_info_incr_errors(Module1, Module) }
	;
		maybe_write_string(Verbose, "% Mode-checking clauses...\n"),
		check_pred_modes(Module1, Module),
		maybe_report_stats(Statistics)
	),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%
	
	% Mode-check the code for all the predicates in a module.

:- pred check_pred_modes(module_info, module_info, io__state, io__state).
:- mode check_pred_modes(in, out, di, uo) is det.

check_pred_modes(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	modecheck_pred_modes_2(PredIds, ModuleInfo0, ModuleInfo1),
	modecheck_unify_procs(ModuleInfo1, ModuleInfo).

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_pred_modes_2(list(pred_id), module_info, 
			module_info, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, out, di, uo) is det.

modecheck_pred_modes_2([], ModuleInfo, ModuleInfo) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	( { pred_info_is_imported(PredInfo0) } ->
		{ ModuleInfo3 = ModuleInfo0 }
	; { pred_info_is_pseudo_imported(PredInfo0) } ->
		{ ModuleInfo3 = ModuleInfo0 }
	;
		write_progress_message("% Mode-checking predicate ",
			PredId, ModuleInfo0),
		modecheck_pred_mode(PredId, PredInfo0, ModuleInfo0,
			ModuleInfo1, Errs),
		{ Errs = 0 ->
			ModuleInfo3 = ModuleInfo1
		;
			module_info_num_errors(ModuleInfo1, NumErrors0),
			NumErrors is NumErrors0 + Errs,
			module_info_set_num_errors(ModuleInfo1, NumErrors,
				ModuleInfo2),
			module_info_remove_predid(ModuleInfo2, PredId,
				ModuleInfo3)
		}
	),
	modecheck_pred_modes_2(PredIds, ModuleInfo3, ModuleInfo).

%-----------------------------------------------------------------------------%

modecheck_pred_mode(PredId, PredInfo0, ModuleInfo0, ModuleInfo, NumErrors) -->
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ map__keys(Procs0, ProcIds) },
	( { ProcIds = [] } ->
		report_warning_no_modes(PredId, PredInfo0, ModuleInfo0),
		{ ModuleInfo = ModuleInfo0 },
		{ NumErrors = 0 }
	;
		modecheck_procs(ProcIds, PredId, ModuleInfo0, 0,
					ModuleInfo, NumErrors)
	).

	% Iterate over the list of modes for a predicate.

:- pred modecheck_procs(list(proc_id), pred_id, module_info, int,
				module_info, int, io__state, io__state).
:- mode modecheck_procs(in, in, in, in, out, out, di, uo) is det.

modecheck_procs([], _PredId,  ModuleInfo, Errs, ModuleInfo, Errs) --> [].
modecheck_procs([ProcId|ProcIds], PredId, ModuleInfo0, Errs0,
					ModuleInfo, Errs) -->
		% mode-check that mode of the predicate
	modecheck_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1, NumErrors),
	{ Errs1 is Errs0 + NumErrors },
		% recursively process the remaining modes
	modecheck_procs(ProcIds, PredId, ModuleInfo1, Errs1, ModuleInfo, Errs).

%-----------------------------------------------------------------------------%

	% Mode-check the code for predicate in a given mode.

modecheck_proc(ProcId, PredId, ModuleInfo0, ModuleInfo, NumErrors) -->
		% get the proc_info from the module_info
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
					_PredInfo0, ProcInfo0) },
		% modecheck it
	modecheck_proc_2(ProcId, PredId, ModuleInfo0, ProcInfo0,
				ModuleInfo1, ProcInfo, NumErrors),
		% save the proc_info back in the module_info
	{ module_info_preds(ModuleInfo1, Preds1) },
	{ map__lookup(Preds1, PredId, PredInfo1) },
	{ pred_info_procedures(PredInfo1, Procs1) },
	{ map__set(Procs1, ProcId, ProcInfo, Procs) },
	{ pred_info_set_procedures(PredInfo1, Procs, PredInfo) },
	{ map__set(Preds1, PredId, PredInfo, Preds) },
	{ module_info_set_preds(ModuleInfo1, Preds, ModuleInfo) }.

:- pred modecheck_proc_2(proc_id, pred_id, module_info, proc_info,
			module_info, proc_info, int, io__state, io__state).
:- mode modecheck_proc_2(in, in, in, in, out, out, out, di, uo) is det.

modecheck_proc_2(ProcId, PredId, ModuleInfo0, ProcInfo0,
				ModuleInfo, ProcInfo, NumErrors,
				IOState0, IOState) :-
		% extract the useful fields in the proc_info
	proc_info_goal(ProcInfo0, Body0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	proc_info_headvars(ProcInfo0, HeadVars),

		% We use the context of the first clause, unless
		% there weren't any clauses at all, in which case
		% we use the context of the mode declaration.
	module_info_preds(ModuleInfo0, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	ClausesInfo = clauses_info(_, _, _, ClauseList),
	( ClauseList = [FirstClause | _] ->
		FirstClause = clause(_, _, Context)
	;
		proc_info_context(ProcInfo0, Context)
	),
/**************
		% extract the predicate's type from the pred_info
		% and propagate the type information into the modes
	pred_info_arg_types(PredInfo, _TypeVars, ArgTypes),
	propagate_type_info_mode_list(ArgTypes, ModuleInfo0, ArgModes0,
			ArgModes),
**************/
	ArgModes = ArgModes0,
		% modecheck the clause - first set the initial instantiation
		% of the head arguments, mode-check the body, and
		% then check that the final instantiation matches that in
		% the mode declaration
	mode_list_get_initial_insts(ArgModes, ModuleInfo0, ArgInitialInsts),
	map__from_corresponding_lists(HeadVars, ArgInitialInsts, InstMapping0),
	InstMap0 = reachable(InstMapping0),
		% initially, only the non-clobbered head variables are live
	mode_list_get_final_insts(ArgModes, ModuleInfo0, ArgFinalInsts),
	get_live_vars(HeadVars, ArgFinalInsts, ModuleInfo0, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),
	mode_info_init(IOState0, ModuleInfo0, PredId, ProcId, Context, LiveVars,
			InstMap0, ModeInfo0),
	modecheck_goal(Body0, Body, ModeInfo0, ModeInfo1),
	modecheck_final_insts(HeadVars, ArgFinalInsts, ModeInfo1, ModeInfo2),
	modecheck_report_errors(ModeInfo2, ModeInfo),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_num_errors(ModeInfo, NumErrors),
	mode_info_get_io_state(ModeInfo, IOState),
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	proc_info_set_goal(ProcInfo0, Body, ProcInfo1),
	proc_info_set_variables(ProcInfo1, VarSet, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo).

	% Given the head vars and the final insts of a predicate,
	% work out which of those variables may be used again
	% by the caller of that predicate (i.e. which variables
	% did not have final inst `clobbered').

get_live_vars([], [], _, []).
get_live_vars([Var|Vars], [FinalInst|FinalInsts], ModuleInfo, LiveVars) :-
	( inst_is_clobbered(ModuleInfo, FinalInst) ->
		LiveVars = LiveVars0
	;
		LiveVars = [Var | LiveVars0]
	),
	get_live_vars(Vars, FinalInsts, ModuleInfo, LiveVars0).

get_live_vars([_|_], [], _, _) :- error("get_live_vars: length mismatch").
get_live_vars([], [_|_], _, _) :- error("get_live_vars: length mismatch").

	% check that the final insts of the head vars matches their
	% expected insts
	%
modecheck_final_insts(HeadVars, ArgFinalInsts, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_instmap(ModeInfo0, InstMap),
	check_final_insts(HeadVars, ArgFinalInsts, 1, InstMap,
		ModuleInfo, ModeInfo0, ModeInfo).

:- pred check_final_insts(list(var), list(inst), int, instmap, module_info,
				mode_info, mode_info).
:- mode check_final_insts(in, in, in, in, in, mode_info_di, mode_info_uo)
	is det.

check_final_insts([], [_|_], _, _, _) -->
	{ error("check_final_insts: length mismatch") }.
check_final_insts([_|_], [], _, _, _) -->
	{ error("check_final_insts: length mismatch") }.
check_final_insts([], [], _, _, _) --> [].
check_final_insts([Var | Vars], [Inst | Insts], ArgNum, InstMap, ModuleInfo)
		-->
	{ instmap_lookup_var(InstMap, Var, VarInst) },
	( { inst_matches_final(VarInst, Inst, ModuleInfo) } ->
		[]
	;
		% XXX this might need to be reconsidered now we have
		% unique modes
		( { inst_matches_initial(VarInst, Inst, ModuleInfo) } ->
			{ Reason = too_instantiated }
		; { inst_matches_initial(Inst, VarInst, ModuleInfo) } ->
			{ Reason = not_instantiated_enough }
		;
			% I don't think this can happen.  But just in case...
			{ Reason = wrongly_instantiated }
		),
		{ set__init(WaitingVars) },
		mode_info_error(WaitingVars, mode_error_final_inst(ArgNum,
			Var, VarInst, Inst, Reason))
	),
	{ ArgNum1 is ArgNum + 1 },
	check_final_insts(Vars, Insts, ArgNum1, InstMap, ModuleInfo).

%-----------------------------------------------------------------------------%

% Modecheck a goal by abstractly interpreteting it, as explained
% at the top of this file.

% Note: any changes here may need to be duplicated in unique_modes.m.

% Input-output: InstMap - Stored in the ModeInfo, which is passed as an
%			  argument pair
%		DelayInfo - Stored in the ModeInfo
%		Goal	- Passed as an argument pair
% Input only:   Symbol tables	(constant)
%			- Stored in the ModuleInfo which is in the ModeInfo
%		Context Info	(changing as we go along the clause)
%			- Stored in the ModeInfo
% Output only:	Error Message(s)
%			- Output directly to stdout.

:- pred modecheck_goal(hlds__goal, hlds__goal, mode_info, mode_info).
:- mode modecheck_goal(in, out, mode_info_di, mode_info_uo) is det.

modecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, ModeInfo0, ModeInfo) :-
		%
		% store the current context in the mode_info
		%
	goal_info_context(GoalInfo0, Context),
	term_context_init(EmptyContext),
	( Context = EmptyContext ->
		ModeInfo1 = ModeInfo0
	;
		mode_info_set_context(Context, ModeInfo0, ModeInfo1)
	),
		%
		% modecheck the goal, and then store the changes in
		% instantiation of the non-local vars in the delta_instmap
		% in the goal's goal_info.
		%
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	mode_info_get_vars_instmap(ModeInfo1, NonLocals, InstMap0),

	modecheck_goal_2(Goal0, GoalInfo0, Goal, ModeInfo1, ModeInfo),

	mode_info_get_vars_instmap(ModeInfo, NonLocals, InstMap),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).

:- pred modecheck_goal_2(hlds__goal_expr, hlds__goal_info, hlds__goal_expr,
			mode_info, mode_info).
:- mode modecheck_goal_2(in, in, out, mode_info_di, mode_info_uo) is det.

modecheck_goal_2(conj(List0), _GoalInfo0, conj(List)) -->
	mode_checkpoint(enter, "conj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] }
	;
		modecheck_conj_list(List0, List)
	),
	mode_checkpoint(exit, "conj").

modecheck_goal_2(disj(List0), GoalInfo0, disj(List)) -->
	mode_checkpoint(enter, "disj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] },
		mode_info_set_instmap(unreachable)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		modecheck_disj_list(List0, List, InstMapList),
		instmap_merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "disj").

modecheck_goal_2(if_then_else(Vs, A0, B0, C0), GoalInfo0, Goal) -->
	mode_checkpoint(enter, "if-then-else"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	{ goal_get_nonlocals(B0, B_Vars) },
	{ goal_get_nonlocals(C0, C_Vars) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(NonLocals),
	mode_info_add_live_vars(B_Vars),
	mode_info_add_live_vars(C_Vars),
	modecheck_goal(A0, A),
	mode_info_remove_live_vars(C_Vars),
	mode_info_remove_live_vars(B_Vars),
	mode_info_unlock_vars(NonLocals),
	mode_info_dcg_get_instmap(InstMapA),
	modecheck_goal(B0, B),
	mode_info_dcg_get_instmap(InstMapB),
	mode_info_set_instmap(InstMap0),
	modecheck_goal(C0, C),
	mode_info_dcg_get_instmap(InstMapC),
	mode_info_set_instmap(InstMap0),
	instmap_merge(NonLocals, [InstMapB, InstMapC], if_then_else),
	( { InstMapA = unreachable } ->
		% if the condition can never succeed, we delete the
		% unreachable `then' part by replacing
		%	if some [Vs] A then B else C
		% with
		%	(not some [Vs] A), C
		{ A = _A_Goal - A_GoalInfo },
		{ goal_info_get_nonlocals(A_GoalInfo, A_Vars) },
		{ set__union(NonLocals, C_Vars, OutsideVars) },
		{ set__delete_list(OutsideVars, Vs, OutsideVars1) },
		{ set__intersect(OutsideVars1, A_Vars, SomeA_NonLocals) },
		{ goal_info_init(EmptyGoalInfo) },
		{ goal_info_set_nonlocals(EmptyGoalInfo, SomeA_NonLocals,
			SomeA_GoalInfo) },
		{ map__init(EmptyInstmapDelta) },
		{ goal_info_set_instmap_delta(SomeA_GoalInfo,
			reachable(EmptyInstmapDelta), NotSomeA_GoalInfo) },
		
		{ Goal = conj([not(some(Vs, A) - SomeA_GoalInfo) -
				NotSomeA_GoalInfo, C]) }
	;
		{ Goal = if_then_else(Vs, A, B, C) }
	),
	mode_checkpoint(exit, "if-then-else").

modecheck_goal_2(not(A0), GoalInfo0, not(A)) -->
	mode_checkpoint(enter, "not"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(NonLocals),
	modecheck_goal(A0, A),
	mode_info_unlock_vars(NonLocals),
	mode_info_set_instmap(InstMap0),
	mode_checkpoint(exit, "not").

modecheck_goal_2(some(Vs, G0), _, some(Vs, G)) -->
	mode_checkpoint(enter, "some"),
	modecheck_goal(G0, G),
	mode_checkpoint(exit, "some").

modecheck_goal_2(call(PredId, _, Args0, _, Context, PredName, Follow),
		GoalInfo0, Goal) -->
	mode_checkpoint(enter, "call"),
	{ list__length(Args0, Arity) },
	mode_info_set_call_context(call(PredName/Arity)),
	=(ModeInfo0),
	
	modecheck_call_pred(PredId, Args0, Mode, Args, ExtraGoals),
	=(ModeInfo),
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ code_util__is_builtin(ModuleInfo, PredId, Mode, Builtin) },
	{ Call = call(PredId, Mode, Args, Builtin, Context, PredName, Follow) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
				ModeInfo0, ModeInfo, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

modecheck_goal_2(unify(A0, B0, _, UnifyInfo0, UnifyContext), GoalInfo0, Goal)
		-->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),
	=(ModeInfo0),
	modecheck_unification(A0, B0, UnifyInfo0, UnifyContext, GoalInfo0,
		check_modes, A, B, ExtraGoals, Mode, UnifyInfo),
	=(ModeInfo),
	% optimize away unifications with dead variables
	(
		{ UnifyInfo = assign(AssignTarget, _),
		  mode_info_var_is_live(ModeInfo, AssignTarget, dead)
		; UnifyInfo = construct(ConstructTarget, _, _, _),
		  mode_info_var_is_live(ModeInfo, ConstructTarget, dead)
		}
	->
		% replace the unification with `true'
		{ Goal = conj([]) }
	;
		{ Unify = unify(A, B, Mode, UnifyInfo, UnifyContext) },
		{ unify_rhs_vars(B0, B0Vars) },
		{ unify_rhs_vars(B, BVars) },
		{ handle_extra_goals(Unify, ExtraGoals, GoalInfo0,
					[A0|B0Vars], [A|BVars],
					ModeInfo0, ModeInfo, Goal) }
	),
	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

modecheck_goal_2(switch(Var, CanFail, Cases0), GoalInfo0,
		switch(Var, CanFail, Cases)) -->
	mode_checkpoint(enter, "switch"),
	( { Cases0 = [] } ->
		{ Cases = [] },
		mode_info_set_instmap(unreachable)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		modecheck_case_list(Cases0, Var, Cases, InstMapList),
		instmap_merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "switch").

	% to modecheck a pragma_c_code, we just modecheck the proc for 
	% which it is the goal.
modecheck_goal_2(pragma_c_code(C_Code, PredId, ProcId, Args0, ArgNameMap), 
		GoalInfo, Goal) -->
	mode_checkpoint(enter, "pragma_c_code"),
	=(ModeInfo0),
	{ mode_info_get_preds(ModeInfo0, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, Arity) },

	=(ModeInfo1),
	mode_info_set_call_context(call(unqualified(PredName)/Arity)),
	
	=(ModeInfo2),
	modecheck_call_pred(PredId, Args0, _Mode, Args, ExtraGoals),
	{ Pragma = pragma_c_code(C_Code, PredId, ProcId, Args0, ArgNameMap) },
	{ handle_extra_goals(Pragma, ExtraGoals, GoalInfo, Args0, Args,
				ModeInfo1, ModeInfo2, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "pragma_c_code").


:- pred map_delete_list(list(K), map(K, V), map(K, V)).
:- mode map_delete_list(in, in, out) is det.

map_delete_list([], Map, Map).
map_delete_list([Key|Keys], Map0, Map) :-
	map__delete(Map0, Key, Map1),
	map_delete_list(Keys, Map1, Map).

 	% given the right-hand-side of a unification, return a list of
	% the potentially non-local variables of that unification.

unify_rhs_vars(var(Var), [Var]).
unify_rhs_vars(functor(_Functor, Vars), Vars).
unify_rhs_vars(lambda_goal(LambdaVars, _Modes, _Det, _Goal - GoalInfo), Vars) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals0),
	set__delete_list(NonLocals0, LambdaVars, NonLocals),
	set__to_sorted_list(NonLocals, Vars).

	% handle_extra_goals combines MainGoal and ExtraGoals into a single
	% hlds__goal_expr.

handle_extra_goals(MainGoal, ExtraGoals, GoalInfo0, Args0, Args,
		ModeInfo0, ModeInfo, Goal) :-
	% did we introduced any extra variables (and code)?
	( ExtraGoals = [] - [] ->
		Goal = MainGoal	% no
	;
		% recompute the new set of non-local variables for the main goal
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__list_to_set(Args0, OldArgVars),
		set__list_to_set(Args, NewArgVars),
		set__difference(NewArgVars, OldArgVars, IntroducedVars),
		set__union(NonLocals0, IntroducedVars, OutsideVars),
		set__intersect(NewArgVars, OutsideVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		% compute the instmap delta for the main goal
		mode_info_get_vars_instmap(ModeInfo0, NewArgVars, InstMap0),
		mode_info_get_vars_instmap(ModeInfo, NewArgVars, InstMap),
		compute_instmap_delta(InstMap0, InstMap, NonLocals,
			DeltaInstMap),
		goal_info_set_instmap_delta(GoalInfo1, DeltaInstMap, GoalInfo),

		% combine the main goal and the extra goals into a conjunction
		Goal0 = MainGoal - GoalInfo,
		ExtraGoals = BeforeGoals0 - AfterGoals0,
		goal_info_context(GoalInfo0, Context),
		handle_extra_goals_contexts(BeforeGoals0, Context, BeforeGoals),
		handle_extra_goals_contexts(AfterGoals0, Context, AfterGoals),
		list__append(BeforeGoals, [Goal0 | AfterGoals], GoalList),
		Goal = conj(GoalList)
	).

:- pred handle_extra_goals_contexts(list(hlds__goal), term_context,
	list(hlds__goal)).
:- mode handle_extra_goals_contexts(in, in, out) is det.

handle_extra_goals_contexts([], _Context, []).
handle_extra_goals_contexts([Goal0 | Goals0], Context, [Goal | Goals]) :-
	Goal0 = Expr - GoalInfo0,
	Goal  = Expr - GoalInfo,
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	handle_extra_goals_contexts(Goals0, Context, Goals).

	% Return Result = yes if the called predicate is known to never succeed.
	%
mode_info_never_succeeds(ModeInfo, PredId, ProcId, Result) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_declared_determinism(ProcInfo, DeclaredDeterminism),
	(
		DeclaredDeterminism = no,
		Result = no
	;
		DeclaredDeterminism = yes(Determinism),
		determinism_components(Determinism, _, HowMany),
		( HowMany = at_most_zero ->
			Result = yes
		;
			Result = no
		)
	).

:- pred goal_get_nonlocals(hlds__goal, set(var)).
:- mode goal_get_nonlocals(in, out) is det.

goal_get_nonlocals(_Goal - GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

%-----------------------------------------------------------------------------%

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.

compute_instmap_delta(unreachable, _, _, unreachable).
compute_instmap_delta(reachable(_), unreachable, _, unreachable).
compute_instmap_delta(reachable(InstMapA), reachable(InstMapB), NonLocals,
			reachable(DeltaInstMap)) :-
	set__to_sorted_list(NonLocals, NonLocalsList),
	compute_instmap_delta_2(NonLocalsList, InstMapA, InstMapB, AssocList),
	map__from_sorted_assoc_list(AssocList, DeltaInstMap).

:- pred compute_instmap_delta_2(list(var), instmapping, instmapping,
					assoc_list(var, inst)).
:- mode compute_instmap_delta_2(in, in, in, out) is det.

compute_instmap_delta_2([], _, _, []).
compute_instmap_delta_2([Var | Vars], InstMapA, InstMapB, AssocList) :-
	instmapping_lookup_var(InstMapA, Var, InstA),
	instmapping_lookup_var(InstMapB, Var, InstB),
	( InstA = InstB ->
		AssocList1 = AssocList
	;
		AssocList = [ Var - InstB | AssocList1 ]
	),
	compute_instmap_delta_2(Vars, InstMapA, InstMapB, AssocList1).

:- pred instmap_lookup_arg_list(list(var), instmap, list(inst)).
:- mode instmap_lookup_arg_list(in, in, out) is det.

instmap_lookup_arg_list([], _InstMap, []).
instmap_lookup_arg_list([Arg|Args], InstMap, [Inst|Insts]) :-
	instmap_lookup_var(InstMap, Arg, Inst),
	instmap_lookup_arg_list(Args, InstMap, Insts).

%-----------------------------------------------------------------------------%

:- pred modecheck_conj_list(list(hlds__goal), list(hlds__goal),
				mode_info, mode_info).
:- mode modecheck_conj_list(in, out, mode_info_di, mode_info_uo) is det.

modecheck_conj_list(Goals0, Goals) -->
	=(ModeInfo0),
	{ mode_info_get_errors(ModeInfo0, OldErrors) },
	mode_info_set_errors([]),

	{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },
	{ delay_info__enter_conj(DelayInfo0, DelayInfo1) },
	mode_info_set_delay_info(DelayInfo1),
	mode_info_add_goals_live_vars(Goals0),

	modecheck_conj_list_2(Goals0, Goals),

	=(ModeInfo3),
	{ mode_info_get_errors(ModeInfo3, NewErrors) },
	{ list__append(OldErrors, NewErrors, Errors) },
	mode_info_set_errors(Errors),

	{ mode_info_get_delay_info(ModeInfo3, DelayInfo4) },
	{ delay_info__leave_conj(DelayInfo4, DelayedGoals, DelayInfo5) },
	mode_info_set_delay_info(DelayInfo5),

	( { DelayedGoals = [] } ->
		[]
	; { DelayedGoals = [delayed_goal(_DVars, Error, _DGoal)] } ->
		mode_info_add_error(Error)
	;
		{ get_all_waiting_vars(DelayedGoals, Vars) },
		mode_info_error(Vars, mode_error_conj(DelayedGoals))
	).

mode_info_add_goals_live_vars([]) --> [].
mode_info_add_goals_live_vars([Goal | Goals]) -->
	{ goal_get_nonlocals(Goal, Vars) },
	mode_info_add_live_vars(Vars),
	mode_info_add_goals_live_vars(Goals).

mode_info_remove_goals_live_vars([]) --> [].
mode_info_remove_goals_live_vars([Goal | Goals]) -->
	{ goal_get_nonlocals(Goal, Vars) },
	mode_info_remove_live_vars(Vars),
	mode_info_remove_goals_live_vars(Goals).

:- pred modecheck_conj_list_2(list(hlds__goal), list(hlds__goal),
				mode_info, mode_info).
:- mode modecheck_conj_list_2(in, out, mode_info_di, mode_info_uo) is det.

	% Schedule a conjunction.
	% If it's empty, then there is nothing to do.
	% For non-empty conjunctions, we attempt to schedule the first
	% goal in the conjunction.  If successful, we wakeup a newly
	% pending goal (if any), and if not, we delay the goal.  Then we
	% continue attempting to schedule all the rest of the goals.

modecheck_conj_list_2([], []) --> [].
modecheck_conj_list_2([Goal0 | Goals0], Goals) -->

		% Hang onto the original instmap & delay_info
	mode_info_dcg_get_instmap(InstMap0),
	=(ModeInfo0),
	{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },

		% Modecheck the goal, noting first that the non-locals
		% which occur in the goal might not be live anymore.
	{ goal_get_nonlocals(Goal0, NonLocalVars) },
	mode_info_remove_live_vars(NonLocalVars),
	modecheck_goal(Goal0, Goal),

		% Now see whether the goal was successfully scheduled.
		% If we didn't manage to schedule the goal, then we
		% restore the original instmap, delay_info & livevars here,
		% and delay the goal.
	=(ModeInfo1),
	{ mode_info_get_errors(ModeInfo1, Errors) },
	( { Errors = [ FirstError | _] } ->
		mode_info_set_errors([]),
		mode_info_set_instmap(InstMap0),
		mode_info_add_live_vars(NonLocalVars),
		{ delay_info__delay_goal(DelayInfo0, FirstError, Goal0,
					DelayInfo1) }
	;
		{ mode_info_get_delay_info(ModeInfo1, DelayInfo1) }
	),

		% Next, we attempt to wake up any pending goals,
		% and then continue scheduling the rest of the goal.
	{ delay_info__wakeup_goals(DelayInfo1, WokenGoals, DelayInfo) },
	{ list__append(WokenGoals, Goals0, Goals1) },
	( { WokenGoals = [] } ->
		[]
	; { WokenGoals = [_] } ->
		mode_checkpoint(wakeup, "goal")
	;
		mode_checkpoint(wakeup, "goals")
	),
	mode_info_set_delay_info(DelayInfo),
	mode_info_dcg_get_instmap(InstMap),
	( { InstMap = unreachable } ->
		mode_info_remove_goals_live_vars(Goals1),
		{ Goals2  = [] }
	;
		modecheck_conj_list_2(Goals1, Goals2)
	),
	( { Errors = [] } ->
		{ Goals = [Goal | Goals2] }
	;
		{ Goals = Goals2 }
	).

:- pred dcg_set_state(T, T, T).
:- mode dcg_set_state(in, in, out) is det.

dcg_set_state(Val, _OldVal, Val).

	% Given an association list of Vars - Goals,
	% combine all the Vars together into a single set.

:- pred get_all_waiting_vars(list(delayed_goal), set(var)).
:- mode get_all_waiting_vars(in, out) is det.

get_all_waiting_vars(DelayedGoals, Vars) :-
	set__init(Vars0),
	get_all_waiting_vars_2(DelayedGoals, Vars0, Vars).

:- pred get_all_waiting_vars_2(list(delayed_goal), set(var), set(var)).
:- mode get_all_waiting_vars_2(in, in, out) is det.

get_all_waiting_vars_2([], Vars, Vars).
get_all_waiting_vars_2([delayed_goal(Vars1, _, _) | Rest], Vars0, Vars) :-
	set__union(Vars0, Vars1, Vars2),
	get_all_waiting_vars_2(Rest, Vars2, Vars).

%-----------------------------------------------------------------------------%

:- pred modecheck_disj_list(list(hlds__goal), list(hlds__goal), list(instmap),
				mode_info, mode_info).
:- mode modecheck_disj_list(in, out, out, mode_info_di, mode_info_uo) is det.

modecheck_disj_list([], [], []) --> [].
modecheck_disj_list([Goal0 | Goals0], [Goal | Goals], [InstMap | InstMaps]) -->
	mode_info_dcg_get_instmap(InstMap0),
	modecheck_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	modecheck_disj_list(Goals0, Goals, InstMaps).

:- pred modecheck_case_list(list(case), var, list(case), list(instmap),
				mode_info, mode_info).
:- mode modecheck_case_list(in, in, out, out, mode_info_di, mode_info_uo)
	is det.

modecheck_case_list([], _Var, [], []) --> [].
modecheck_case_list([Case0 | Cases0], Var,
			[Case | Cases], [InstMap | InstMaps]) -->
	{ Case0 = case(ConsId, Goal0) },
	{ Case = case(ConsId, Goal) },
	mode_info_dcg_get_instmap(InstMap0),

		% record the fact that Var was bound to ConsId in the
		% instmap before processing this case
	( { cons_id_to_const(ConsId, Const, Arity) } ->
		{ list__duplicate(Arity, free, ArgInsts) },
		modecheck_set_var_inst(Var,
			bound(unique, [functor(Const, ArgInsts)]))
	;
		% cons_id_to_const will fail for pred_consts and
		% address_consts; we don't worry about them,
		% since you can't have a switch on a higher-order
		% pred term anyway.
		[]
	),

	modecheck_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	modecheck_case_list(Cases0, Var, Cases, InstMaps).

	% instmap_merge(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables, 
	%	checking that it is the same for every branch.

instmap_merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	get_reachable_instmaps(InstMapList, InstMappingList),
	( InstMappingList = [] ->
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0
	; InstMap0 = reachable(InstMapping0) ->
		set__to_sorted_list(NonLocals, NonLocalsList),
		instmap_merge_2(NonLocalsList, InstMapList, ModuleInfo0,
			InstMapping0, ModuleInfo, InstMapping, ErrorList),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
		( ErrorList = [FirstError | _] ->
			FirstError = Var - _,
			set__singleton_set(WaitingVars, Var),
			mode_info_error(WaitingVars,
				mode_error_disj(MergeContext, ErrorList),
				ModeInfo1, ModeInfo2
			)
		;
			ModeInfo2 = ModeInfo1
		),
		InstMap = reachable(InstMapping)
	;
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0
	),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo).

:- pred get_reachable_instmaps(list(instmap), list(map(var,inst))).
:- mode get_reachable_instmaps(in, out) is det.

get_reachable_instmaps([], []).
get_reachable_instmaps([InstMap | InstMaps], Reachables) :-
	( InstMap = reachable(InstMapping) ->
		Reachables = [InstMapping | Reachables1],
		get_reachable_instmaps(InstMaps, Reachables1)
	;
		get_reachable_instmaps(InstMaps, Reachables)
	).

%-----------------------------------------------------------------------------%

	% instmap_merge_2(Vars, InstMaps, ModuleInfo, ErrorList):
	%	Let `ErrorList' be the list of variables in `Vars' for
	%	there are two instmaps in `InstMaps' for which the inst
	%	the variable is incompatible.

:- pred instmap_merge_2(list(var), list(instmap), module_info, map(var, inst),
			module_info, map(var, inst), merge_errors).
:- mode instmap_merge_2(in, in, in, in, out, out, out) is det.

instmap_merge_2([], _, ModuleInfo, InstMap, ModuleInfo, InstMap, []).
instmap_merge_2([Var|Vars], InstMapList, ModuleInfo0, InstMap0,
			ModuleInfo, InstMap, ErrorList) :-
	instmap_merge_2(Vars, InstMapList, ModuleInfo0, InstMap0,
			ModuleInfo1, InstMap1, ErrorList1),
	instmap_merge_var(InstMapList, Var, ModuleInfo1,
			Insts, Inst, ModuleInfo, Error),
	( Error = yes ->
		ErrorList = [Var - Insts | ErrorList1],
		map__set(InstMap1, Var, not_reached, InstMap)
	;
		ErrorList = ErrorList1,
		map__set(InstMap1, Var, Inst, InstMap)
	).

	% instmap_merge_var(InstMaps, Var, ModuleInfo, Insts, Error):
	%	Let `Insts' be the list of the inst of `Var' in the
	%	corresponding `InstMaps'.  Let `Error' be yes iff
	%	there are two instmaps for which the inst of `Var'
	%	is incompatible.

:- pred instmap_merge_var(list(instmap), var, module_info,
				list(inst), inst, module_info, bool).
:- mode instmap_merge_var(in, in, in, out, out, out, out) is det.

instmap_merge_var([], _, ModuleInfo, [], not_reached, ModuleInfo, no).
instmap_merge_var([InstMap | InstMaps], Var, ModuleInfo0,
		InstList, Inst, ModuleInfo, Error) :-
	instmap_merge_var(InstMaps, Var, ModuleInfo0,
		InstList0, Inst0, ModuleInfo1, Error0),
	instmap_lookup_var(InstMap, Var, VarInst),
	InstList = [VarInst | InstList0],
	( inst_merge(Inst0, VarInst, ModuleInfo1, Inst1, ModuleInfo2) ->
		Inst = Inst1,
		ModuleInfo = ModuleInfo2,
		Error = Error0
	;
		Error = yes,
		ModuleInfo = ModuleInfo1,
		Inst = not_reached
	).

%-----------------------------------------------------------------------------%

:- pred modecheck_call_pred(pred_id, list(var), proc_id, list(var),
				pair(list(hlds__goal)), mode_info, mode_info).
:- mode modecheck_call_pred(in, in, out, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_call_pred(PredId, ArgVars0, TheProcId, ArgVars, ExtraGoals,
		ModeInfo0, ModeInfo) :-

		% Get the list of different possible modes for the called
		% predicate
	mode_info_get_preds(ModeInfo0, Preds),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__keys(Procs, ProcIds),

		% In order to give better diagnostics, we handle the
		% cases where there are zero or one modes for the called
		% predicate specially.
	(
		ProcIds = []
	->
		set__init(WaitingVars),
		mode_info_error(WaitingVars, mode_error_no_mode_decl,
			ModeInfo0, ModeInfo),
		TheProcId = 0,
		ArgVars = ArgVars0,
		ExtraGoals = [] - []
	;
		ProcIds = [ProcId]
	->
		TheProcId = ProcId,
		map__lookup(Procs, ProcId, ProcInfo),
		proc_info_argmodes(ProcInfo, ProcArgModes0),
/*********************
		% propagate type info into modes
		mode_info_get_types_of_vars(ModeInfo0, ArgVars0, ArgTypes),
		propagate_type_info_mode_list(ArgTypes, ModuleInfo,
			ProcArgModes0, ProcArgModes),
*********************/
		ProcArgModes = ProcArgModes0,
		mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
					InitialInsts),
		modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
					ModeInfo0, ModeInfo1),
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
			ArgVars, ExtraGoals, ModeInfo1, ModeInfo2),
		mode_info_never_succeeds(ModeInfo2, PredId, ProcId, Result),
		( Result = yes ->
			mode_info_set_instmap(unreachable, ModeInfo2, ModeInfo)
		;
			ModeInfo = ModeInfo2
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
			set(var), proc_id, list(var), pair(list(hlds__goal)),
			mode_info, mode_info).
:- mode modecheck_call_pred_2(in, in, in, in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

modecheck_call_pred_2([], _PredId, _Procs, ArgVars, WaitingVars,
			0, ArgVars, [] - [], ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	get_var_insts(ArgVars, InstMap, ArgInsts),
	mode_info_error(WaitingVars,
		mode_error_no_matching_mode(ArgVars, ArgInsts),
		ModeInfo0, ModeInfo).
	
modecheck_call_pred_2([ProcId | ProcIds], PredId, Procs, ArgVars0, WaitingVars,
			TheProcId, ArgVars, ExtraGoals, ModeInfo0, ModeInfo) :-

		% find the initial insts for this mode of the called pred
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_argmodes(ProcInfo, ProcArgModes0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
/**************
		% propagate the type information into the modes
	mode_info_get_types_of_vars(ModeInfo0, ArgVars0, ArgTypes),
	propagate_type_info_mode_list(ArgTypes, ModuleInfo,
		ProcArgModes0, ProcArgModes),
**************/
	ProcArgModes = ProcArgModes0,
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),

		% check whether the insts of the args matches their expected
		% initial insts
	modecheck_var_has_inst_list(ArgVars0, InitialInsts, 0,
				ModeInfo0, ModeInfo1),
	mode_info_get_errors(ModeInfo1, Errors),
	(
			% if error(s) occured, keep trying with the other modes
			% for the called pred
		Errors = [FirstError | _]
	->
		FirstError = mode_error_info(WaitingVars2, _, _, _),
		set__union(WaitingVars, WaitingVars2, WaitingVars3),
		mode_info_set_errors([], ModeInfo1, ModeInfo2),
		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars0,
				WaitingVars3, TheProcId, ArgVars, ExtraGoals,
				ModeInfo2, ModeInfo)
	;
			% if there are no errors, then set their insts to the
			% final insts specified in the mode for the called pred
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
				ArgVars, ExtraGoals, ModeInfo1, ModeInfo2),
		TheProcId = ProcId,
		mode_info_never_succeeds(ModeInfo2, PredId, ProcId, Result),
		( Result = yes ->
			mode_info_set_instmap(unreachable, ModeInfo2, ModeInfo)
		;
			ModeInfo = ModeInfo2
		)
	).

:- pred get_var_insts(list(var), instmap, list(inst)).
:- mode get_var_insts(in, in, out) is det.

get_var_insts([], _, []).
get_var_insts([Var | Vars], InstMap, [Inst | Insts]) :-
	instmap_lookup_var(InstMap, Var, Inst),
	get_var_insts(Vars, InstMap, Insts).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of initial insts, ensure
	% that the inst of each variable matches the corresponding initial
	% inst.

modecheck_var_has_inst_list([_|_], [], _) -->
	{ error("modecheck_var_has_inst_list: length mismatch") }.
modecheck_var_has_inst_list([], [_|_], _) -->
	{ error("modecheck_var_has_inst_list: length mismatch") }.
modecheck_var_has_inst_list([], [], _ArgNum) --> [].
modecheck_var_has_inst_list([Var|Vars], [Inst|Insts], ArgNum0) -->
	{ ArgNum is ArgNum0 + 1 },
	mode_info_set_call_arg_context(ArgNum),
	modecheck_var_has_inst(Var, Inst),
	modecheck_var_has_inst_list(Vars, Insts, ArgNum).

:- pred modecheck_var_has_inst(var, inst, mode_info, mode_info).
:- mode modecheck_var_has_inst(in, in, mode_info_di, mode_info_uo) is det.

modecheck_var_has_inst(VarId, Inst, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	instmap_lookup_var(InstMap, VarId, VarInst),

	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	( inst_matches_initial(VarInst, Inst, ModuleInfo) ->
		ModeInfo = ModeInfo0
	;
		set__singleton_set(WaitingVars, VarId),
		mode_info_error(WaitingVars,
			mode_error_var_has_inst(VarId, VarInst, Inst),
			ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

modecheck_set_var_inst_list(Vars0, InitialInsts, FinalInsts, Vars, Goals) -->
	(
		modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts,
			Vars1, Goals1)
	->
		{ Vars = Vars1, Goals = Goals1 }
	;
		{ error("modecheck_set_var_inst_list: length mismatch") }
	).

:- pred modecheck_set_var_inst_list_2(list(var), list(inst), list(inst),
					list(var), pair(list(hlds__goal)),
					mode_info, mode_info).
:- mode modecheck_set_var_inst_list_2(in, in, in, out, out,
					mode_info_di, mode_info_uo) is semidet.

modecheck_set_var_inst_list_2([], [], [], [], [] - []) --> [].
modecheck_set_var_inst_list_2([Var0 | Vars0], [InitialInst | InitialInsts],
			[FinalInst | FinalInsts], [Var | Vars], Goals) -->
	modecheck_set_var_inst(Var0, InitialInst, FinalInst,
				Var, BeforeGoals0 - AfterGoals0),
	modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts,
				Vars, BeforeGoals1 - AfterGoals1),
	{ list__append(BeforeGoals0, BeforeGoals1, BeforeGoals) },
	{ list__append(AfterGoals0, AfterGoals1, AfterGoals) },
	{ Goals = BeforeGoals - AfterGoals }.

% XXX this might need to be revisited to handle unique modes

:- pred modecheck_set_var_inst(var, inst, inst, var, pair(list(hlds__goal)),
				mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var0, InitialInst, FinalInst, Var, Goals,
			ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(_) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap_lookup_var(InstMap0, Var0, VarInst0),
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		(
			abstractly_unify_inst(dead, VarInst0, FinalInst,
				fake_unify, ModuleInfo0,
				UnifyInst, Det1, ModuleInfo1)
		->
			ModuleInfo = ModuleInfo1,
			VarInst = UnifyInst,
			Det = Det1
		;
			error("modecheck_set_var_inst: unify_inst failed")
		),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
		handle_implied_mode(Var0,
			VarInst0, VarInst, InitialInst, FinalInst, Det,
		 	Var, Goals, ModeInfo1, ModeInfo2),
		modecheck_set_var_inst(Var0, FinalInst, ModeInfo2, ModeInfo3),
		modecheck_set_var_inst(Var, FinalInst, ModeInfo3, ModeInfo)
	;
		Var = Var0,
		Goals = [] - [],
		ModeInfo = ModeInfo0
	).

	% Note that there are two versions of modecheck_set_var_inst,
	% one with arity 7 and one with arity 4.
	% The former is used for predicate calls, where we may need
	% to introduce unifications to handle calls to implied modes.

% XXX this might need to be revisited to handle unique modes

modecheck_set_var_inst(Var0, FinalInst, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(InstMapping0) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap_lookup_var(InstMap0, Var0, Inst0),
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		(
			abstractly_unify_inst(dead, Inst0, FinalInst,
				fake_unify, ModuleInfo0,
				UnifyInst, _Det, ModuleInfo1)
		->
			ModuleInfo = ModuleInfo1,
			Inst = UnifyInst
		;
			error("modecheck_set_var_inst: unify_inst failed")
		),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
		(
			% If we haven't added any information and
			% we haven't bound any part of the var, then
			% the only thing we can have done is lose uniqueness.
			inst_matches_initial(Inst0, Inst, ModuleInfo)
		->
			map__set(InstMapping0, Var0, Inst, InstMapping),
			InstMap = reachable(InstMapping),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo)
		;
			% We must have either added some information,
			% lost some uniqueness, or bound part of the var.
			% The call to inst_matches_binding will succeed
			% only if we haven't bound any part of the var.
			inst_matches_binding(Inst, Inst0, ModuleInfo)
		->
			% We've just added some information
			% or lost some uniqueness.
			map__set(InstMapping0, Var0, Inst, InstMapping),
			InstMap = reachable(InstMapping),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo2),
			mode_info_get_delay_info(ModeInfo2, DelayInfo0),
			delay_info__bind_var(DelayInfo0, Var0, DelayInfo),
			mode_info_set_delay_info(DelayInfo, ModeInfo2, ModeInfo)
		;
			% We've bound part of the var.  If the var was locked,
			% then we need to report an error.
			mode_info_var_is_locked(ModeInfo1, Var0)
		->
			set__singleton_set(WaitingVars, Var0),
			mode_info_error(WaitingVars,
					mode_error_bind_var(Var0, Inst0, Inst),
					ModeInfo1, ModeInfo
			)
		;
			map__set(InstMapping0, Var0, Inst, InstMapping),
			InstMap = reachable(InstMapping),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo2),
			mode_info_get_delay_info(ModeInfo2, DelayInfo0),
			delay_info__bind_var(DelayInfo0, Var0, DelayInfo),
			mode_info_set_delay_info(DelayInfo, ModeInfo2, ModeInfo)
		)
	;
		ModeInfo = ModeInfo0
	).


% If this was a call to an implied mode for that variable, then we need to
% introduce a fresh variable.

:- pred handle_implied_mode(var, inst, inst, inst, inst, determinism,
				var, pair(list(hlds__goal)),
				mode_info, mode_info).
:- mode handle_implied_mode(in, in, in, in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

handle_implied_mode(Var0, VarInst0, VarInst, InitialInst, FinalInst, Det,
		Var, Goals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		% If the initial inst of the variable matches
		% the initial inst specified in the pred's mode declaration,
		% then it's not a call to an implied mode, it's an exact
		% match with a genuine mode.
		inst_matches_binding(VarInst0, InitialInst, ModuleInfo0)
	->
		Var = Var0,
		Goals = [] - [],
		ModeInfo = ModeInfo0
	;
		% This is the implied mode case.
		% We do not yet handle implied modes for partially
		% instantiated vars, since that would require
		% doing a deep copy, and we don't know how to do that yet.
		( inst_is_bound(ModuleInfo0, InitialInst) ->
			% This is the case we can't handle
			Var = Var0,
			Goals = [] - [],
			set__singleton_set(WaitingVars, Var0),
			mode_info_error(WaitingVars,
				mode_error_implied_mode(Var0, VarInst0,
				InitialInst),
				ModeInfo0, ModeInfo
			)
		;
			% This is the simple case of implied modes,
			% where the declared mode was free -> ...

			% Introduce a new variable
			mode_info_get_varset(ModeInfo0, VarSet0),
			mode_info_get_var_types(ModeInfo0, VarTypes0),
			varset__new_var(VarSet0, Var, VarSet),
			map__lookup(VarTypes0, Var0, VarType),
			map__set(VarTypes0, Var, VarType, VarTypes),
			mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
			mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo2),

			% Construct the code to do the unification
			ModeVar0 = (VarInst0 -> VarInst),
			ModeVar = (FinalInst -> VarInst),
			categorize_unify_var_var(ModeVar0, ModeVar,
				live, dead, Var0, Var, Det,
				VarTypes, ModeInfo2,
				Unification, ModeInfo),
			mode_info_get_mode_context(ModeInfo, ModeContext),
			mode_context_to_unify_context(ModeContext,
				UnifyContext),
			AfterGoal = unify(Var0, var(Var),
				ModeVar0 - ModeVar, Unification, UnifyContext),

			% compute the goal_info nonlocal vars & instmap delta
			set__list_to_set([Var0, Var], NonLocals),
			map__init(InstMapDelta0),
			( VarInst = VarInst0 ->
				InstMapDelta1 = InstMapDelta0
			;
				map__set(InstMapDelta0, Var0, VarInst,
					InstMapDelta1)
			),
			map__set(InstMapDelta1, Var, VarInst, InstMapDelta),
			goal_info_init(GoalInfo0),
			goal_info_set_nonlocals(GoalInfo0, NonLocals,
				GoalInfo1),
			goal_info_set_instmap_delta(GoalInfo1,
				reachable(InstMapDelta), GoalInfo),
			Goals = [] - [AfterGoal - GoalInfo]
		)
	).

:- pred mode_context_to_unify_context(mode_context, unify_context).
:- mode mode_context_to_unify_context(in, out) is det.

mode_context_to_unify_context(unify(UnifyContext, _), UnifyContext).
mode_context_to_unify_context(call(PredId, Arg),
		unify_context(call(PredId, Arg), [])).
mode_context_to_unify_context(uninitialized, _) :-
	error("mode_context_to_unify_context: uninitialized context").

%-----------------------------------------------------------------------------%

	% This code is used to trace the actions of the mode checker.

mode_checkpoint(Port, Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
        globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
		IOState0, IOState1),
	( DoCheckPoint = yes ->
		mode_checkpoint_2(Port, Msg, ModeInfo0, IOState1, IOState)
	;
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred bool(bool::in) is det.
bool(_).

:- pred mode_checkpoint_2(port, string, mode_info, io__state, io__state).
:- mode mode_checkpoint_2(in, in, mode_info_ui, di, uo) is det.

mode_checkpoint_2(Port, Msg, ModeInfo) -->
	{ mode_info_get_errors(ModeInfo, Errors) },
	{ bool(Detail) },	% explicit type qualification needed to
				% resolve type ambiguity
	( { Port = enter } ->
		io__write_string("Enter "),
		{ Detail = yes }
	; { Port = wakeup } ->
		io__write_string("Wake  "),
		{ Detail = no }
	; { Errors = [] } ->
		io__write_string("Exit "),
		{ Detail = yes }
	;
		io__write_string("Delay  "),
		{ Detail = no }
	),
	io__write_string(Msg),
	( { Detail = yes } ->
		io__write_string(":\n"),
		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),
		{ mode_info_get_instmap(ModeInfo, InstMap) },
		( { InstMap = reachable(InstMapping) } ->
			{ map__to_assoc_list(InstMapping, AssocList) },
			{ mode_info_get_varset(ModeInfo, VarSet) },
			{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
			write_var_insts(AssocList, VarSet, InstVarSet)
		;
			io__write_string("\tUnreachable\n")
		)
	;
		[]
	),
	io__write_string("\n").

:- pred write_var_insts(assoc_list(var, inst), varset, varset,
			io__state, io__state).
:- mode write_var_insts(in, in, in, di, uo) is det.

write_var_insts([], _, _) --> [].
write_var_insts([Var - Inst | VarInsts], VarSet, InstVarSet) -->
	io__write_string("\t"),
	mercury_output_var(Var, VarSet),
	io__write_string(" :: "),
	mercury_output_inst(Inst, InstVarSet),
	( { VarInsts = [] } ->
		[]
	;
		io__write_string("\n"),
		write_var_insts(VarInsts, VarSet, InstVarSet)
	).

%-----------------------------------------------------------------------------%

	% Mode check a unification.

modecheck_unification(X, var(Y), _Unification0, _UnifyContext, _GoalInfo, _,
			X, var(Y), ExtraGoals, Modes, Unification,
			ModeInfo0, ModeInfo) :-
	ExtraGoals = [] - [],
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstOfX),
	instmap_lookup_var(InstMap0, Y, InstOfY),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_is_live(ModeInfo0, Y, LiveY),
	(
		( LiveX = live, LiveY = live ->
			BothLive = live
		;
			BothLive = dead
		),
		abstractly_unify_inst(BothLive, InstOfX, InstOfY,
			real_unify, ModuleInfo0, UnifyInst, Det1, ModuleInfo1)
	->
		Inst = UnifyInst,
		Det = Det1,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		modecheck_set_var_inst(Y, Inst, ModeInfo2, ModeInfo3),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Modes = ModeOfX - ModeOfY,
		mode_info_get_var_types(ModeInfo3, VarTypes),
		categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y,
			Det, VarTypes, ModeInfo3, Unification, ModeInfo)
	;
		set__list_to_set([X, Y], WaitingVars),
		mode_info_error(WaitingVars, mode_error_unify_var_var(X, Y,
				InstOfX, InstOfY), ModeInfo0, ModeInfo1),
			% If we get an error, set the inst to not_reached
			% to suppress follow-on errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		modecheck_set_var_inst(Y, Inst, ModeInfo2, ModeInfo),
			% return any old garbage
		Unification = assign(X, Y),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Modes = ModeOfX - ModeOfY
	).

modecheck_unification(X0, functor(Name, ArgVars0), Unification0,
			UnifyContext, GoalInfo0, HowToCheckGoal,
			X, Functor, ExtraGoals, Mode, Unification,
			ModeInfo0, ModeInfo) :-
	%
	% We replace any unifications with higher-order pred constants
	% by lambda expressions.  For example, we replace
	%
	%	X = list__append(Y)	% Y::in, X::out
	%
	% with
	%
	%	X = lambda [A1::in, A2::out] (list__append(Y, A1, A2))
	% 
	% We do this because it makes two things easier.
	% Firstly, we need to check that the lambda-goal doesn't
	% bind any non-local variables (e.g. `Y' in above example).
	% This would require a bit of moderately tricky special-case code
	% if we didn't expand them.
	% Secondly, the polymorphism pass (polymorphism.m) is a lot easier
	% if we don't have to handle higher-order pred consts.
	% If it turns out that the predicate was non-polymorphic,
	% lambda.m will (I hope) turn the lambda expression
	% back into a higher-order pred constant again.
	%
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, X0, TypeOfX),
	(
		% check if variable has a higher-order pred type
		TypeOfX = term_functor(term_atom("pred"), PredArgTypes, _),
		Name = term_atom(PName),
		% but in case we are redoing mode analysis, make sure
		% we don't mess with the address constants for type_info
		% fields created by polymorphism.m
		Unification0 \= construct(_, address_const(_, _), _, _),
		Unification0 \= deconstruct(_, address_const(_, _), _, _, _)
	->
		%
		% Create the new lambda-quantified variables
		%
		mode_info_get_varset(ModeInfo0, VarSet0),
		make_fresh_vars(PredArgTypes, VarSet0, VarTypes0,
				LambdaVars, VarSet, VarTypes),
		list__append(ArgVars0, LambdaVars, Args),
		mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
		mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo2),

		%
		% Build up the hlds__goal_expr for the call that will form
		% the lambda goal
		%
		list__length(ArgVars0, Arity),
		get_pred_id_and_proc_id(PName, Arity, PredArgTypes,
				 ModuleInfo0, PredId, ProcId),
		PredName = unqualified(PName),
		hlds__is_builtin_make_builtin(no, no, Builtin),
		map__init(Follow),
		CallUnifyContext = call_unify_context(X0,
					functor(Name, ArgVars0), UnifyContext),
		LambdaGoalExpr = call(PredId, ProcId, Args, Builtin,
				yes(CallUnifyContext), PredName, Follow),

		%
		% construct a goal_info for the lambda goal, making sure 
		% to set up the nonlocals field in the goal_info correctly
		%
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__insert_list(NonLocals, LambdaVars, OutsideVars),
		set__list_to_set(Args, InsideVars),
		set__intersect(OutsideVars, InsideVars, LambdaNonLocals),
		goal_info_init(LambdaGoalInfo0),
		goal_info_set_nonlocals(LambdaGoalInfo0, LambdaNonLocals,
				LambdaGoalInfo),
		LambdaGoal = LambdaGoalExpr - LambdaGoalInfo,

		%
		% work out the modes of the introduced lambda variables
		% and the determinism of the lambda goal
		%
		module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
					_PredInfo, ProcInfo),
		proc_info_argmodes(ProcInfo, ArgModes),
		( list__drop(Arity, ArgModes, LambdaModes0) ->
			LambdaModes = LambdaModes0
		;
			error("modecheck_unification: list__drop failed")
		),
		proc_info_declared_determinism(ProcInfo, MaybeDet),
		( MaybeDet = yes(Det) ->
			LambdaDet = Det
		;
			error("Sorry, not implemented: determinism inference for higher-order predicate terms")
		),

		%
		% construct the lambda expression, and then go ahead
		% and modecheck this unification in its new form
		%
		Functor0 = lambda_goal(LambdaVars, LambdaModes, LambdaDet,
					LambdaGoal),
		modecheck_unification( X0, Functor0, Unification0, UnifyContext,
				GoalInfo0, HowToCheckGoal,
				X, Functor, ExtraGoals, Mode, Unification,
				ModeInfo2, ModeInfo)
	;
		%
		% It's not a higher-order pred unification - just
		% call modecheck_unify_functor to do the ordinary thing.
		%
		modecheck_unify_functor(X0, Name, ArgVars0, Unification0,
				ExtraGoals, Mode, ArgVars, Unification,
				ModeInfo0, ModeInfo),
		X = X0,
		Functor = functor(Name, ArgVars)
	).

modecheck_unification(X, lambda_goal(Vars, Modes, Det, Goal0),
			Unification0, _UnifyContext, _GoalInfo, HowToCheckGoal,
			X, RHS, ExtraGoals, Mode, Unification,
			ModeInfo0, ModeInfo) :-
	ExtraGoals = [] - [],

	%
	% First modecheck the lambda goal itself:
	%
	% initialize the initial insts of the lambda variables,
	% lock the non-local vars,
	% mark the non-clobbered lambda variables as live,
	% modecheck the goal,
	% check that the final insts are correct,
	% unmark the live vars,
	% unlock the non-local vars,
	% restore the original instmap.
	%
	% XXX or should we merge the original and the final instmaps???
	%
	% The reason that we need to merge the original and final instmaps
	% is as follows.  The lambda goal will not have bound any variables
	% (since they were locked), but it may have added some information
	% or lost some uniqueness.  We cannot use the final instmap,
	% because that may have too much information.  If we use the
	% initial instmap, variables will be considered as unique
	% even if they become shared or clobbered in the lambda goal!
	%

	% initialize the initial insts of the lambda variables
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_list_get_initial_insts(Modes, ModuleInfo0, VarInitialInsts),
	map__from_corresponding_lists(Vars, VarInitialInsts, VarInstMapping),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(InstMapping0) ->
		map__overlay(InstMapping0, VarInstMapping, InstMapping1),
		InstMap1 = reachable(InstMapping1)
	;
		InstMap1 = unreachable
	),
	mode_info_set_instmap(InstMap1, ModeInfo0, ModeInfo1),

	% mark the non-clobbered lambda variables as live
	mode_list_get_final_insts(Modes, ModuleInfo0, FinalInsts),
	get_live_vars(Vars, FinalInsts, ModuleInfo0, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),
	mode_info_add_live_vars(LiveVars, ModeInfo1, ModeInfo2),

	% lock the non-locals
	% (a lambda goal is not allowed to bind any of the non-local
	% variables, since it could get called more than once)
	Goal0 = _ - GoalInfo0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__delete_list(NonLocals0, Vars, NonLocals),
	mode_info_lock_vars(NonLocals, ModeInfo2, ModeInfo3),

	mode_checkpoint(enter, "lambda goal", ModeInfo3, ModeInfo4),
	% if we're being called from unique_modes.m, then we need to call
	% unique_modes__check_goal rather than modecheck_goal.
	( HowToCheckGoal = check_unique_modes ->
		unique_modes__check_goal(Goal0, Goal, ModeInfo4, ModeInfo5)
	;
		modecheck_goal(Goal0, Goal, ModeInfo4, ModeInfo5)
	),
	modecheck_final_insts(Vars, FinalInsts, ModeInfo5, ModeInfo6),
	mode_checkpoint(exit, "lambda goal", ModeInfo6, ModeInfo7),

	mode_info_remove_live_vars(LiveVars, ModeInfo7, ModeInfo8),
	mode_info_unlock_vars(NonLocals, ModeInfo8, ModeInfo9),
	mode_info_set_instmap(InstMap0, ModeInfo9, ModeInfo10),

	%
	% Now modecheck the unification of X with the lambda-expression.
	%

	set__to_sorted_list(NonLocals, ArgVars),
	modecheck_unify_lambda(X, ArgVars, Modes, Det, Unification0,
				Mode, Unification1,
				ModeInfo10, ModeInfo11),

	%
	% if we're being called from unique_modes.m, then we need to
	% transform away lambda expressions
	%
	( HowToCheckGoal = check_unique_modes ->
		modes__transform_lambda(Vars, Modes, Det, Goal, Unification1,
			RHS, Unification, ModeInfo11, ModeInfo)
	;
		RHS = lambda_goal(Vars, Modes, Det, Goal), 
		Unification = Unification1,
		ModeInfo = ModeInfo11
	).

:- pred modes__transform_lambda(list(var), list(mode), determinism,
		hlds__goal, unification, unify_rhs, unification,
		mode_info, mode_info).
:- mode modes__transform_lambda(in, in, in, in, in, out, out,
		mode_info_di, mode_info_uo) is det.

modes__transform_lambda(Vars, Modes, Det, LambdaGoal,
		Unification0, Functor, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_varset(ModeInfo0, VarSet),
	mode_info_get_predid(ModeInfo0, PredId),
	mode_info_get_procid(ModeInfo0, ProcId),
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo, ProcInfo),
	pred_info_typevarset(PredInfo, TVarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	lambda__transform_lambda(Vars, Modes, Det, LambdaGoal,
		Unification0, VarSet, VarTypes, TVarSet, ModuleInfo0,
		Functor, Unification, ModuleInfo),
	mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo).

:- pred modecheck_unify_lambda(var, list(var),
			list(mode), determinism, unification,
			pair(mode), unification, mode_info, mode_info).
:- mode modecheck_unify_lambda(in, in, in, in, in,
			out, out, mode_info_di, mode_info_uo) is det.

modecheck_unify_lambda(X, ArgVars, LambdaModes, LambdaDet, Unification0,
		Mode, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstOfX),
	InstOfY = ground(unique, yes(pred_inst_info(LambdaModes, LambdaDet))),
	(
		abstractly_unify_inst(dead, InstOfX, InstOfY, real_unify,
			ModuleInfo0, UnifyInst, _Det, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		instmap_lookup_arg_list(ArgVars, InstMap0, ArgInsts),
		mode_list_from_inst_list(ArgInsts, ArgModes),
		categorize_unify_var_lambda(ModeOfX, ArgModes, X, ArgVars,
				Unification0, ModeInfo1,
				Unification, ModeInfo2),
		modecheck_set_var_inst(X, Inst, ModeInfo2, ModeInfo)
	;
		set__list_to_set([X], WaitingVars),
		mode_info_error(WaitingVars,
			mode_error_unify_var_lambda(X, InstOfX, InstOfY),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
			% return any old garbage
		Unification = Unification0
	).

:- pred modecheck_unify_functor(var, const, list(var), unification,
			pair(list(hlds__goal)), pair(mode), list(var),
			unification,
			mode_info, mode_info).
:- mode modecheck_unify_functor(in, in, in, in, out, out, out, out,
			mode_info_di, mode_info_uo) is det.

modecheck_unify_functor(X, Name, ArgVars0, Unification0,
			ExtraGoals, Mode, ArgVars, Unification,
			ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstOfX),
	instmap_lookup_arg_list(ArgVars0, InstMap0, InstArgs),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_list_is_live(ArgVars0, ModeInfo0, LiveArgs),
	InstOfY = bound(unique, [functor(Name, InstArgs)]),
	(
		% The occur check: X = f(X) is considered a mode error
		% unless X is ground.  (Actually it wouldn't be that
		% hard to generate code for it - it always fails! -
		% but it's most likely to be a programming error,
		% so it's better to report it.)

		list__member(X, ArgVars0),
		\+ inst_is_ground(ModuleInfo0, InstOfX)
	->
		set__list_to_set([X], WaitingVars),
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, Name, ArgVars0,
							InstOfX, InstArgs),
			ModeInfo0, ModeInfo1
		),
		Inst = not_reached,
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		( bind_args(Inst, ArgVars0, ModeInfo2, ModeInfo3) ->
			ModeInfo = ModeInfo3
		;
			error("bind_args failed")
		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals = [] - []
	;
		abstractly_unify_inst_functor(LiveX, InstOfX, Name,
			InstArgs, LiveArgs, real_unify, ModuleInfo0,
			UnifyInst, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		( get_mode_of_args(Inst, InstArgs, ModeArgs0) ->
			ModeArgs = ModeArgs0
		;
			error("get_mode_of_args failed")
		),
		(
			list__length(ArgVars0, Arity),
			inst_expand(ModuleInfo1, InstOfX, InstOfX1),
			get_arg_insts(InstOfX1, Name, Arity, InstOfXArgs),
			get_mode_of_args(Inst, InstOfXArgs, ModeOfXArgs0)
		->
			ModeOfXArgs = ModeOfXArgs0
		;
			error("get_(inst/mode)_of_args failed")
		),
		mode_info_get_var_types(ModeInfo1, VarTypes),
		categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ModeArgs,
				X, Name, ArgVars0, VarTypes,
				Unification0, ModeInfo1,
				Unification1, ModeInfo2),
		split_complicated_subunifies(Unification1, ArgVars0,
					Unification, ArgVars, ExtraGoals,
					ModeInfo2, ModeInfo3),
		modecheck_set_var_inst(X, Inst, ModeInfo3, ModeInfo4),
		( bind_args(Inst, ArgVars, ModeInfo4, ModeInfo5) ->
			ModeInfo = ModeInfo5
		;
			error("bind_args failed")
		)
	;
		set__list_to_set([X | ArgVars0], WaitingVars), % conservative
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, Name, ArgVars0,
							InstOfX, InstArgs),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
			% But don't call categorize_unification, because
			% that could cause an invalid call to
			% `unify_proc__request_unify'
		Inst = not_reached,
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
		( bind_args(Inst, ArgVars0, ModeInfo2, ModeInfo3) ->
			ModeInfo = ModeInfo3
		;
			error("bind_args failed")
		),
			% return any old garbage
		Unification = Unification0,
		ArgVars = ArgVars0,
		ExtraGoals = [] - []
	).

%-----------------------------------------------------------------------------%

	% The argument unifications in a construction or deconstruction
	% unification must be simple assignments, they cannot be
	% complicated unifications.  If they are, we split them out
	% into separate unifications by introducing fresh variables here.

:- pred split_complicated_subunifies(unification, list(var),
			unification, list(var), pair(list(hlds__goal)),
			mode_info, mode_info).
:- mode split_complicated_subunifies(in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

split_complicated_subunifies(Unification0, ArgVars0,
				Unification, ArgVars, ExtraGoals) -->
	(
		{ Unification0 = deconstruct(X, ConsId, ArgVars0, ArgModes0,
			Det) }
	->
		(
			split_complicated_subunifies_2(ArgVars0, ArgModes0,
				ArgVars1, ArgModes, ExtraGoals1)
		->
			{ ArgVars = ArgVars1 },
			{ Unification = deconstruct(X, ConsId, ArgVars,
							ArgModes, Det) },
			{ ExtraGoals = ExtraGoals1 }
		;
			{ error("split_complicated_subunifies_2 failed") }
		)
	;
		{ Unification = Unification0 },
		{ ArgVars = ArgVars0 },
		{ ExtraGoals = [] - [] }
	).

:- pred split_complicated_subunifies_2(list(var), list(uni_mode),
			list(var), list(uni_mode), pair(list(hlds__goal)),
			mode_info, mode_info).
:- mode split_complicated_subunifies_2(in, in, out, out, out,
		mode_info_di, mode_info_uo) is semidet.

split_complicated_subunifies_2([], [], [], [], [] - []) --> [].
split_complicated_subunifies_2([Var0 | Vars0], [UniMode0 | UniModes0], 
			Vars, UniModes, ExtraGoals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	UniMode0 = (InitialInstX - InitialInstY -> FinalInstX - FinalInstY),
	(
		mode_is_input(ModuleInfo, (InitialInstX -> FinalInstX)),
		mode_is_input(ModuleInfo, (InitialInstY -> FinalInstY))
	->
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, UniModes1, ExtraGoals0,
				ModeInfo0, ModeInfo1),
		ExtraGoals0 = BeforeGoals - AfterGoals0,

		% introduce a new variable `Var'
		mode_info_get_varset(ModeInfo1, VarSet0),
		mode_info_get_var_types(ModeInfo1, VarTypes0),
		varset__new_var(VarSet0, Var, VarSet),
		map__lookup(VarTypes0, Var0, VarType),
		map__set(VarTypes0, Var, VarType, VarTypes),
		mode_info_set_varset(VarSet, ModeInfo1, ModeInfo2),
		mode_info_set_var_types(VarTypes, ModeInfo2, ModeInfo3),

		% change the main unification to use `Var' instead of Var0
		UniMode = (InitialInstX - free -> InitialInstX - InitialInstX),
		UniModes = [UniMode | UniModes1],
		Vars = [Var | Vars1],

		% create code to do a unification between `Var' and `Var0'
		ModeVar0 = (InitialInstY -> FinalInstY),
		ModeVar = (InitialInstX -> FinalInstX),

		mode_info_get_module_info(ModeInfo3, ModuleInfo0),
		(
			abstractly_unify_inst(dead, InitialInstX, InitialInstY,
				real_unify, ModuleInfo0, _, Det1, ModuleInfo1)
		->
			mode_info_set_module_info(ModeInfo3, ModuleInfo1,
				ModeInfo4),
			Det = Det1
		;
			ModeInfo4 = ModeInfo3,
			Det = semidet
			% XXX warning - it might be det in some cases.
			% should we report an error here?  should this
			% ever happen?
		),
		categorize_unify_var_var(ModeVar0, ModeVar,
			live, dead, Var0, Var, Det,
			VarTypes, ModeInfo4, Unification, ModeInfo),
		mode_info_get_mode_context(ModeInfo, ModeContext),
		mode_context_to_unify_context(ModeContext, UnifyContext),
		AfterGoal = unify(Var0, var(Var),
				ModeVar0 - ModeVar, Unification, UnifyContext),

		% compute the goal_info nonlocal vars & instmap delta
		% for the newly created goal
		set__list_to_set([Var0, Var], NonLocals),
		map__init(InstMapDelta0),
		( InitialInstY = FinalInstY ->
			InstMapDelta1 = InstMapDelta0
		;
			map__set(InstMapDelta0, Var0, FinalInstY,
				InstMapDelta1)
		),
		map__set(InstMapDelta1, Var, FinalInstX, InstMapDelta),
		goal_info_init(GoalInfo0),
		goal_info_set_nonlocals(GoalInfo0, NonLocals,
			GoalInfo1),
		goal_info_set_instmap_delta(GoalInfo1,
			reachable(InstMapDelta), GoalInfo),

		% insert the unification between `Var' and `Var0' at
		% the start of the AfterGoals
		AfterGoals = [AfterGoal - GoalInfo | AfterGoals0],
		ExtraGoals = BeforeGoals - AfterGoals
	;
		Vars = [Var0 | Vars1],
		UniModes = [UniMode0 | UniModes1],
		split_complicated_subunifies_2(Vars0, UniModes0, 
			Vars1, UniModes1, ExtraGoals, ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(var), mode_info, mode_info).
:- mode bind_args(in, in, mode_info_di, mode_info_uo) is semidet.

bind_args(not_reached, _) -->
	mode_info_set_instmap(unreachable).
bind_args(ground(Uniq, no), Args) -->
	ground_args(Uniq, Args).
bind_args(bound(_Uniq, List), Args) -->
	( { List = [] } ->
		% the code is unreachable
		mode_info_set_instmap(unreachable)
	;
		{ List = [functor(_, InstList)] },
		bind_args_2(Args, InstList)
	).

:- pred bind_args_2(list(var), list(inst), mode_info, mode_info).
:- mode bind_args_2(in, in, mode_info_di, mode_info_uo) is semidet.

bind_args_2([], []) --> [].
bind_args_2([Arg | Args], [Inst | Insts]) -->
	modecheck_set_var_inst(Arg, Inst),
	bind_args_2(Args, Insts).

:- pred ground_args(uniqueness, list(var), mode_info, mode_info).
:- mode ground_args(in, in, mode_info_di, mode_info_uo) is det.

ground_args(_Uniq, []) --> [].
ground_args(Uniq, [Arg | Args]) -->
	modecheck_set_var_inst(Arg, ground(Uniq, no)),
	ground_args(Uniq, Args).

%-----------------------------------------------------------------------------%

:- pred get_arg_insts(inst, const, arity, list(inst)).
:- mode get_arg_insts(in, in, in, out) is semidet.

get_arg_insts(not_reached, _Name, Arity, ArgInsts) :-
	list__duplicate(Arity, not_reached, ArgInsts).
get_arg_insts(ground(Uniq, _PredInst), _Name, Arity, ArgInsts) :-
	list__duplicate(Arity, ground(Uniq, no), ArgInsts).
get_arg_insts(bound(_Uniq, List), Name, Arity, ArgInsts) :-
	( get_arg_insts_2(List, Name, ArgInsts0) ->
		ArgInsts = ArgInsts0
	;
		% the code is unreachable
		list__duplicate(Arity, not_reached, ArgInsts)
	).
get_arg_insts(free, _Name, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(free(_Type), _Name, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).

:- pred get_arg_insts_2(list(bound_inst), const, list(inst)).
:- mode get_arg_insts_2(in, in, out) is semidet.

get_arg_insts_2([BoundInst | BoundInsts], Name, ArgInsts) :-
	( BoundInst = functor(Name, ArgInsts0) ->
		ArgInsts = ArgInsts0
	;
		get_arg_insts_2(BoundInsts, Name, ArgInsts)
	).

% get_mode_of_args(FinalInst, InitialArgInsts, ArgModes):
%	for a var-functor unification,
% 	given the final inst of the var
%	and the initial insts of the functor arguments,
%	compute the modes of the functor arguments

:- pred get_mode_of_args(inst, list(inst), list(mode)).
:- mode get_mode_of_args(in, in, out) is semidet.

get_mode_of_args(not_reached, ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, not_reached, ArgModes).
get_mode_of_args(ground(Uniq, no), ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, ground(Uniq, no), ArgModes).
get_mode_of_args(bound(_Uniq, List), ArgInstsA, ArgModes) :-
	( List = [] ->
		% the code is unreachable
		mode_set_args(ArgInstsA, not_reached, ArgModes)
	;
		List = [functor(_Name, ArgInstsB)],
		get_mode_of_args_2(ArgInstsA, ArgInstsB, ArgModes)
	).

:- pred get_mode_of_args_2(list(inst), list(inst), list(mode)).
:- mode get_mode_of_args_2(in, in, out) is semidet.

get_mode_of_args_2([], [], []).
get_mode_of_args_2([InstA | InstsA], [InstB | InstsB], [Mode | Modes]) :-
	Mode = (InstA -> InstB),
	get_mode_of_args_2(InstsA, InstsB, Modes).

:- pred mode_set_args(list(inst), inst, list(mode)).
:- mode mode_set_args(in, in, out) is det.

mode_set_args([], _, []).
mode_set_args([Inst | Insts], FinalInst, [Mode | Modes]) :-
	Mode = (Inst -> FinalInst),
	mode_set_args(Insts, FinalInst, Modes).

%-----------------------------------------------------------------------------%

:- pred categorize_unify_var_var(mode, mode, is_live, is_live, var, var,
			determinism, map(var, type), mode_info,
			unification, mode_info).
:- mode categorize_unify_var_var(in, in, in, in, in, in, in, in, mode_info_di,
				out, mode_info_uo) is det.

categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y, Det, VarTypes,
		ModeInfo0, Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		mode_is_output(ModuleInfo0, ModeOfX)
	->
		Unification = assign(X, Y),
		ModeInfo = ModeInfo0
	;
		mode_is_output(ModuleInfo0, ModeOfY)
	->
		Unification = assign(Y, X),
		ModeInfo = ModeInfo0
	;
		mode_is_unused(ModuleInfo0, ModeOfX),
		mode_is_unused(ModuleInfo0, ModeOfY)
	->
		% For free-free unifications, we pretend for a moment that they
		% are an assignment to the dead variable - they will then
		% be optimized away.
		( LiveX = dead ->
			Unification = assign(X, Y)
		; LiveY = dead ->
			Unification = assign(Y, X)
		;
			error("categorize_unify_var_var: free-free unify!")
		),
		ModeInfo = ModeInfo0
	;
		map__lookup(VarTypes, X, Type),
		(
			type_is_atomic(Type, ModuleInfo0)
		->
			Unification = simple_test(X, Y),
			ModeInfo = ModeInfo0
		;
			mode_get_insts(ModuleInfo0, ModeOfX, IX, FX),
			mode_get_insts(ModuleInfo0, ModeOfY, IY, FY),
			map__init(Follow),
			determinism_components(Det, CanFail, _),
			UniMode = ((IX - IY) -> (FX - FY)),
			Unification = complicated_unify(UniMode, CanFail,
				Follow),
			(
				Type = term_functor(term_atom("pred"), _, _)
			->
				% we do not want to report this as an error
				% if it occurs in a compiler-generated
				% predicate - instead, we delay the error
				% until runtime so that it only occurs if
				% the compiler-generated predicate gets called
				mode_info_get_predid(ModeInfo0, PredId),
				module_info_pred_info(ModuleInfo0, PredId,
						PredInfo),
				( code_util__compiler_generated(PredInfo) ->
					ModeInfo = ModeInfo0
				;
					set__init(WaitingVars),
					mode_info_error(WaitingVars,
						mode_error_unify_pred,
						ModeInfo0, ModeInfo)
				)
			;
				type_to_type_id(Type, TypeId, _)
			->
				unify_proc__request_unify(TypeId - UniMode, 
					ModuleInfo0, ModuleInfo),
				mode_info_set_module_info(ModeInfo0, ModuleInfo,
					ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		)
	).

% categorize_unify_var_lambda works out which category a unification
% between a variable and a lambda expression is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_lambda(mode, list(mode), var, list(var),
			unification, mode_info, unification, mode_info).
:- mode categorize_unify_var_lambda(in, in, in, in,
			in, mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_lambda(ModeOfX, ArgModes0, X, ArgVars,
		Unification0, ModeInfo0, Unification, ModeInfo) :-
	% if we are re-doing mode analysis, preserve the existing cons_id
	( Unification0 = construct(_, ConsId0, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _) ->
		ConsId = ConsId1
	;
		% the real cons_id will be computed by polymorphism.m;
		% we just put in a dummy one for now
		list__length(ArgVars, Arity),
		ConsId = cons("__LambdaGoal__", Arity)
	),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_util__modes_to_uni_modes(ArgModes0, ArgModes0,
						ModuleInfo, ArgModes),
	(
		mode_is_output(ModuleInfo, ModeOfX)
	->
		Unification = construct(X, ConsId, ArgVars, ArgModes),
		ModeInfo = ModeInfo0
	; 
		% If it's a deconstruction, it is a mode error
		set__init(WaitingVars),
		mode_info_error(WaitingVars, mode_error_unify_pred,
				ModeInfo0, ModeInfo),
		% return any old garbage
		Unification = Unification0
	).

% categorize_unify_var_functor works out which category a unification
% between a variable and a functor is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_functor(mode, list(mode), list(mode), var, const,
			list(var), map(var, type), unification, mode_info,
			unification, mode_info).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, in, in,
			mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_functor(ModeOfX, ModeOfXArgs, ArgModes0,
		X, Name, ArgVars, VarTypes, Unification0, ModeInfo0,
		Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	list__length(ArgVars, Arity),
	map__lookup(VarTypes, X, TypeOfX),
	% if we are re-doing mode analysis, preserve the existing cons_id
	( Unification0 = construct(_, ConsId0, _, _) ->
		ConsId = ConsId0
	; Unification0 = deconstruct(_, ConsId1, _, _, _) ->
		ConsId = ConsId1
	;
		make_functor_cons_id(Name, Arity, ConsId)
	),
	mode_util__modes_to_uni_modes(ModeOfXArgs, ArgModes0,
						ModuleInfo, ArgModes),
	(
		mode_is_output(ModuleInfo, ModeOfX)
	->
		Unification = construct(X, ConsId, ArgVars, ArgModes),
		ModeInfo = ModeInfo0
	; 
		% It's a deconstruction.
		(
			% If the variable was already known to be bound
			% to a single particular functor, then the
			% unification either always succeeds or always
			% fails.  In the latter case, the final inst will
			% be `not_reached' or `bound([])'.  So if both
			% the initial and final inst are `bound([_])',
			% then the unification must be deterministic.
			mode_get_insts(ModuleInfo, ModeOfX,
					InitialInst0, FinalInst0),
			inst_expand(ModuleInfo, InitialInst0, InitialInst),
			inst_expand(ModuleInfo, FinalInst0, FinalInst),
			InitialInst = bound(_, [_]),
			FinalInst = bound(_, [_])
		->
			CanFail = cannot_fail,
			ModeInfo = ModeInfo0
		;
			% If the type has only one constructor,
			% then the unification cannot fail
			type_constructors(TypeOfX, ModuleInfo, Constructors),
			Constructors = [_]
		->
			CanFail = cannot_fail,
			ModeInfo = ModeInfo0
		;
			% Otherwise, it can fail
			CanFail = can_fail,
			( TypeOfX = term_functor(term_atom("pred"), _, _) ->
				set__init(WaitingVars),
				mode_info_error(WaitingVars,
					mode_error_unify_pred,
					ModeInfo0, ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		),
		Unification = deconstruct(X, ConsId, ArgVars, ArgModes, CanFail)
	).

:- pred get_pred_id_and_proc_id(string, arity, list(type), module_info,
				pred_id, proc_id).
:- mode get_pred_id_and_proc_id(in, in, in, in, out, out) is det.

get_pred_id_and_proc_id(Name, Arity, PredArgTypes, ModuleInfo,
			PredId, ProcId) :-
	list__length(PredArgTypes, PredArity),
	TotalArity is Arity + PredArity,
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
	    predicate_table_search_name_arity(PredicateTable,
		Name, TotalArity, PredIds)
	->
	    (
		PredIds = [PredId0]
	    ->
		PredId = PredId0,
		predicate_table_get_preds(PredicateTable, Preds),
		map__lookup(Preds, PredId, PredInfo),
		pred_info_procedures(PredInfo, Procs),
		map__keys(Procs, ProcIds),
		(
		    ProcIds = [ProcId0]
		->
		    ProcId = ProcId0
		;
		    string__append_list([
			    "sorry, not implemented: ",
			    "taking address of predicate\n(`",
			    Name,
			    "') with multiple modes.\n",
			    "(use an explicit lambda expression instead)"],
			    Message),
		    error(Message)
		)
	    ;
		% cons_id ought to include the module prefix, so
		% that we could use predicate_table__search_m_n_a to 
		% prevent this from happening
		string__append("get_pred_id_and_proc_id: ambiguous pred: ",
				Name, Msg),
		error(Msg)
	    )
	;
	    % the type-checker should ensure that this never happens
	    string__append("get_pred_id_and_proc_id: invalid pred: ",
				Name, Msg),
	    error(Msg)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred make_fresh_vars(list(type), varset, map(var, type),
			list(var), varset, map(var, type)).
:- mode make_fresh_vars(in, in, in, out, out, out) is det.

make_fresh_vars([], VarSet, VarTypes, [], VarSet, VarTypes).
make_fresh_vars([Type|Types], VarSet0, VarTypes0,
		[Var|Vars], VarSet, VarTypes) :-
	varset__new_var(VarSet0, Var, VarSet1),
	map__det_insert(VarTypes0, Var, Type, VarTypes1),
	make_fresh_vars(Types, VarSet1, VarTypes1, Vars, VarSet, VarTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% XXX - At the moment we don't check for circular modes or insts.
	% (If they aren't used, the compiler will probably not
	% detect the error; if they are, it will probably go into
	% an infinite loop).

:- pred check_circular_modes(module_info, module_info, io__state, io__state).
:- mode check_circular_modes(in, out, di, uo) is det.

check_circular_modes(Module0, Module) -->
	{ Module = Module0 }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% record a mode error (and associated context info) in the mode_info.

:- pred mode_info_error(set(var), mode_error, mode_info, mode_info).
:- mode mode_info_error(in, in, mode_info_di, mode_info_uo) is det.

mode_info_error(Vars, ModeError, ModeInfo0, ModeInfo) :-
	mode_info_get_context(ModeInfo0, Context),
	mode_info_get_mode_context(ModeInfo0, ModeContext),
	ModeErrorInfo = mode_error_info(Vars, ModeError, Context, ModeContext),
	mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo).

:- pred mode_info_add_error(mode_error_info, mode_info, mode_info).
:- mode mode_info_add_error(in, mode_info_di, mode_info_uo) is det.

mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo) :-
	mode_info_get_errors(ModeInfo0, Errors0),
	list__append(Errors0, [ModeErrorInfo], Errors),
	mode_info_set_errors(Errors, ModeInfo0, ModeInfo).

%-----------------------------------------------------------------------------%

	% If there were any errors recorded in the mode_info,
	% report them to the user now.

modecheck_report_errors(ModeInfo0, ModeInfo) :-
	mode_info_get_errors(ModeInfo0, Errors),
	( Errors = [FirstError | _] ->
		FirstError = mode_error_info(_, ModeError,
						Context, ModeContext),
		mode_info_set_context(Context, ModeInfo0, ModeInfo1),
		mode_info_set_mode_context(ModeContext, ModeInfo1, ModeInfo2),
		mode_info_get_io_state(ModeInfo2, IOState0),
		report_mode_error(ModeError, ModeInfo2,
				IOState0, IOState),
		mode_info_set_io_state(ModeInfo2, IOState, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
