%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modes.m.
% Main author: fjh.
%
% This module contains the top level of the code for mode checking and mode
% inference.  It uses code in the subsidiary modules mode_info, delay_info,
% inst_match, mode_errors, and mode_util.
%
% Basically what this module does is to traverse the HLDS, performing
% mode-checking or mode inference on each predicate.  For each procedure, it
% will reorder the procedure body if necessary, annotate each sub_goal with
% its mode, and check that the procedure body is mode-correct,
% This pass also determines whether or not unifications can fail.  It
% also converts unifications with higher-order predicate terms into
% unifications with lambda expressions.
%
% The input to this pass must be type-correct and in superhomogeneous form.
%
% This pass does not check that `unique' modes are not used in contexts
% which might require backtracking - that is done by unique_modes.m.
% N.B. Changes here may also require changes to unique_modes.m!

% IMPLEMENTATION DOCUMENTATION
% How does it all work?  Well, mode checking/inference is basically a
% process of abstract interpretation.  To perform this abstract
% interpretation on a procedure body, we need to know the initial insts of
% the arguments; then we can abstractly interpretet the goal to compute the
% final insts.  For mode checking, we then just compare the inferred final
% insts with the declared final insts, and that's about all there is to it.
%
% For mode inference, it's a little bit trickier.  When we see a call to a
% predicate for which the modes weren't declared, we first check whether the
% call matches any of the modes we've already inferred.  If not, we create a
% new mode for the predicate, with the initial insts set to a "normalised"
% version of the insts of the call arguments.  For a first approximation, we
% set the final insts to `not_reached'.  What this means is that we don't
% yet have any information about what the final insts will be.  We then keep
% iterating mode inference passes until we reach a fixpoint.

/*************************************
To mode-analyse a procedure:
	1.  Initialize the insts of the head variables.
	2.  Mode-analyse the goal.
	3.  a.  If we're doing mode-checking:
	        Check that the final insts of the head variables
	        matches that specified in the mode declaration
	    b.  If we're doing mode-inference:
		Normalise the final insts of the head variables,
	        record the newly inferred normalised final insts
		in the proc_info, and check whether they changed
		(so that we know when we've reached the fixpoint).

To mode-analyse a goal:
If goal is
	(a) a disjunction
		Mode-analyse the sub-goals;
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
		doing this check after we mode-analyse the subgoal,
		we instead "lock" the non-local variables, and
		disallow binding of locked variables.)
	(d) a unification
		Check that the unification doesn't attempt to unify
		two free variables (or in general two free sub-terms)
		unless one of them is dead. (Also we ought to split
		unifications up if necessary to avoid complicated
		sub-unifications.)
	(e) a predicate call
		Check that there is a mode declaration for the
		predicate which matches the current instantiation of
		the arguments.  (Also handle calls to implied modes.)
		If the called predicate is one for which we must infer
		the modes, then a new mode for the called predicate
		whose initial insts are the result of normalising
		the current inst of the arguments.
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
an error due to the binding of a local variable, then scheduling
fails.  (If mode-checking would report an error due to the binding of
a *local* variable, we could report the error right away --
but this idea has not yet been implemented.)

Note that the notion of liveness used here is different to that
used in liveness.m and the code generator.  Here, we consider
a variable live if its value will be used later on in the computation.

******************************************/

% XXX we need to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is 
%     dead (causes problems handling e.g. `same_length').
% XXX break unifications into "micro-unifications"
% XXX would even the above fixes be enough?

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modes.

:- interface.

:- import_module hlds_module, hlds_pred.
:- import_module bool, io.

:- pred modecheck(module_info, module_info, io__state, io__state).
:- mode modecheck(in, out, di, uo) is det.

	% Mode-check the code for single predicate.

:- pred modecheck_pred_mode(pred_id, pred_info, module_info, module_info,
			int, io__state, io__state).
:- mode modecheck_pred_mode(in, in, in, out, out, di, uo) is det.

	% Mode-check the code for predicate in a given mode.

:- pred modecheck_proc(proc_id, pred_id, module_info, module_info, int,
			io__state, io__state).
:- mode modecheck_proc(in, in, in, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by unique_modes.m.

:- import_module mode_info, mode_errors, clause_to_proc.

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
			hlds__goal_info, how_to_check_goal, hlds__goal_expr,
			mode_info, mode_info).
:- mode modecheck_unification(in, in, in, in, in, in, out,
			mode_info_di, mode_info_uo) is det.

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

:- import_module make_hlds, hlds_goal, hlds_data, unique_modes.
:- import_module mode_info, delay_info, mode_errors, inst_match.
:- import_module type_util, mode_util, code_util, prog_data, unify_proc.
:- import_module globals, options, mercury_to_mercury, hlds_out, int, set.
:- import_module passes_aux.
:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module assoc_list.

%-----------------------------------------------------------------------------%

modecheck(Module0, Module) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),

	maybe_write_string(Verbose, "% Mode-checking clauses...\n"),
	check_pred_modes(Module0, Module),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%
	
	% Mode-check the code for all the predicates in a module.

:- pred check_pred_modes(module_info, module_info, io__state, io__state).
:- mode check_pred_modes(in, out, di, uo) is det.

check_pred_modes(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	{ MaxIterations = 30 }, % XXX FIXME should be command-line option
	modecheck_to_fixpoint(PredIds, MaxIterations, ModuleInfo0, ModuleInfo1),
	write_mode_inference_messages(PredIds, ModuleInfo1),
	modecheck_unify_procs(check_modes, ModuleInfo1, ModuleInfo).

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_to_fixpoint(list(pred_id), int, module_info, 
			module_info, io__state, io__state).
:- mode modecheck_to_fixpoint(in, in, in, out, di, uo) is det.

modecheck_to_fixpoint(PredIds, MaxIterations, ModuleInfo0, ModuleInfo) -->
	{ copy_module_clauses_to_procs(PredIds, ModuleInfo0, ModuleInfo1) },
	modecheck_pred_modes_2(PredIds, ModuleInfo1, ModuleInfo2, no, Changed,
				0, NumErrors),
	% stop if we have reached a fixpoint or found any errors
	( { Changed = no ; NumErrors > 0 } ->
		{ ModuleInfo = ModuleInfo2 }
	;
		% stop if we exceed the iteration limit
		( { MaxIterations =< 1 } ->
			report_max_iterations_exceeded,
			{ ModuleInfo = ModuleInfo2 }
		;
			globals__io_lookup_bool_option(debug_modes, DebugModes),
			( { DebugModes = yes } ->
				write_mode_inference_messages(PredIds,
						ModuleInfo2)
			;
				[]
			),
			{ MaxIterations1 is MaxIterations - 1 },
			modecheck_to_fixpoint(PredIds, MaxIterations1,
				ModuleInfo2, ModuleInfo)
		)
	).

:- pred report_max_iterations_exceeded(io__state, io__state).
:- mode report_max_iterations_exceeded(di, uo) is det.

report_max_iterations_exceeded -->
	io__set_exit_status(1),
	io__write_string("Mode analysis iteration limit exceeded.\n").
	% XXX FIXME add verbose_errors message

:- pred modecheck_pred_modes_2(list(pred_id), module_info, module_info,
			bool, bool, int, int, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, out, in, out, in, out, di, uo) is det.

modecheck_pred_modes_2([], ModuleInfo, ModuleInfo, Changed, Changed,
		NumErrors, NumErrors) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, ModuleInfo,
		Changed0, Changed, NumErrors0, NumErrors) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	( { pred_info_is_imported(PredInfo0) } ->
		{ ModuleInfo3 = ModuleInfo0 },
		{ Changed1 = Changed0 },
		{ NumErrors1 = NumErrors0 }
	; { pred_info_is_pseudo_imported(PredInfo0) } ->
		{ ModuleInfo3 = ModuleInfo0 },
		{ Changed1 = Changed0 },
		{ NumErrors1 = NumErrors0 }
	;
		{ pred_info_get_marker_list(PredInfo0, Markers) },
		( { list__member(request(infer_modes), Markers) } ->
			write_pred_progress_message("% Mode-analysing ",
				PredId, ModuleInfo0)
		;
			write_pred_progress_message("% Mode-checking ",
				PredId, ModuleInfo0)
		),
		modecheck_pred_mode_2(PredId, PredInfo0, ModuleInfo0,
			ModuleInfo1, Changed0, Changed1, ErrsInThisPred),
		{ ErrsInThisPred = 0 ->
			ModuleInfo3 = ModuleInfo1
		;
			module_info_num_errors(ModuleInfo1, ModNumErrors0),
			ModNumErrors1 is ModNumErrors0 + ErrsInThisPred,
			module_info_set_num_errors(ModuleInfo1, ModNumErrors1,
				ModuleInfo2),
			module_info_remove_predid(ModuleInfo2, PredId,
				ModuleInfo3)
		},
		{ NumErrors1 is NumErrors0 + ErrsInThisPred }
	),
	modecheck_pred_modes_2(PredIds, ModuleInfo3, ModuleInfo,
		Changed1, Changed, NumErrors1, NumErrors).

%-----------------------------------------------------------------------------%

	% Mode-check the code for single predicate.

modecheck_pred_mode(PredId, PredInfo0, ModuleInfo0, ModuleInfo, NumErrors) -->
	modecheck_pred_mode_2(PredId, PredInfo0, ModuleInfo0, ModuleInfo,
		no, _Changed, NumErrors).

:- pred modecheck_pred_mode_2(pred_id, pred_info, module_info, module_info,
			bool, bool, int, io__state, io__state).
:- mode modecheck_pred_mode_2(in, in, in, out, in, out, out, di, uo) is det.

modecheck_pred_mode_2(PredId, PredInfo0, ModuleInfo0, ModuleInfo,
		Changed0, Changed, NumErrors) -->
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ map__keys(Procs0, ProcIds) },
	( { ProcIds = [] } ->
		maybe_report_error_no_modes(PredId, PredInfo0, ModuleInfo0),
		{ ModuleInfo = ModuleInfo0 },
		{ NumErrors = 0 },
		{ Changed = Changed0 }
	;
		modecheck_procs(ProcIds, PredId, ModuleInfo0, Changed0, 0,
				ModuleInfo, Changed, NumErrors)
	).

	% Iterate over the list of modes for a predicate.

:- pred modecheck_procs(list(proc_id), pred_id, module_info, bool, int,
				module_info, bool, int, io__state, io__state).
:- mode modecheck_procs(in, in, in, in, in, out, out, out, di, uo) is det.

modecheck_procs([], _PredId,  ModuleInfo, Changed, Errs,
				ModuleInfo, Changed, Errs) --> [].
modecheck_procs([ProcId|ProcIds], PredId, ModuleInfo0, Changed0, Errs0,
					ModuleInfo, Changed, Errs) -->
	% mode-check that mode of the predicate
	modecheck_proc_2(ProcId, PredId, ModuleInfo0, Changed0,
			ModuleInfo1, Changed1, NumErrors),
	{ Errs1 is Errs0 + NumErrors },
		% recursively process the remaining modes
	modecheck_procs(ProcIds, PredId, ModuleInfo1, Changed1, Errs1,
			ModuleInfo, Changed, Errs).

%-----------------------------------------------------------------------------%

	% Mode-check the code for predicate in a given mode.

modecheck_proc(ProcId, PredId, ModuleInfo0, ModuleInfo, NumErrors) -->
	modecheck_proc_2(ProcId, PredId, ModuleInfo0, no,
			ModuleInfo, _Changed, NumErrors).

:- pred modecheck_proc_2(proc_id, pred_id, module_info, bool,
			module_info, bool, int,
			io__state, io__state).
:- mode modecheck_proc_2(in, in, in, in, out, out, out, di, uo) is det.

modecheck_proc_2(ProcId, PredId, ModuleInfo0, Changed0,
				ModuleInfo, Changed, NumErrors) -->
		% get the proc_info from the module_info
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
					_PredInfo0, ProcInfo0) },
	( { proc_info_can_process(ProcInfo0, no) } ->
		{ ModuleInfo = ModuleInfo0 },
		{ Changed = Changed0 },
		{ NumErrors = 0 }
	;
			% modecheck it
		modecheck_proc_3(ProcId, PredId,
				ModuleInfo0, ProcInfo0, Changed0,
				ModuleInfo1, ProcInfo, Changed, NumErrors),
			% save the proc_info back in the module_info
		{ module_info_preds(ModuleInfo1, Preds1) },
		{ map__lookup(Preds1, PredId, PredInfo1) },
		{ pred_info_procedures(PredInfo1, Procs1) },
		{ map__set(Procs1, ProcId, ProcInfo, Procs) },
		{ pred_info_set_procedures(PredInfo1, Procs, PredInfo) },
		{ map__set(Preds1, PredId, PredInfo, Preds) },
		{ module_info_set_preds(ModuleInfo1, Preds, ModuleInfo) }
	).

:- pred modecheck_proc_3(proc_id, pred_id, module_info, proc_info, bool,
			module_info, proc_info, bool, int,
			io__state, io__state).
:- mode modecheck_proc_3(in, in, in, in, in, out, out, out, out, di, uo)
	is det.

modecheck_proc_3(ProcId, PredId, ModuleInfo0, ProcInfo0, Changed0,
				ModuleInfo, ProcInfo, Changed, NumErrors,
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
			ArgModes1),
**************/
	ArgModes1 = ArgModes0,
		% modecheck the clause - first set the initial instantiation
		% of the head arguments, mode-check the body, and
		% then check that the final instantiation matches that in
		% the mode declaration
	mode_list_get_initial_insts(ArgModes1, ModuleInfo0, ArgInitialInsts),
	map__from_corresponding_lists(HeadVars, ArgInitialInsts, InstMapping0),
	InstMap0 = reachable(InstMapping0),
		% initially, only the non-clobbered head variables are live
	mode_list_get_final_insts(ArgModes1, ModuleInfo0, ArgFinalInsts0),
	get_live_vars(HeadVars, ArgFinalInsts0, ModuleInfo0, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),
	mode_info_init(IOState0, ModuleInfo0, PredId, ProcId, Context, LiveVars,
			InstMap0, ModeInfo0),
	modecheck_goal(Body0, Body, ModeInfo0, ModeInfo1),
	modecheck_final_insts_2(HeadVars, ArgFinalInsts0, ModeInfo1, Changed0,
				ArgFinalInsts, ModeInfo2, Changed),
	inst_lists_to_mode_list(ArgInitialInsts, ArgFinalInsts, ArgModes),
	modecheck_report_errors(ModeInfo2, ModeInfo),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_num_errors(ModeInfo, NumErrors),
	mode_info_get_io_state(ModeInfo, IOState),
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	proc_info_set_goal(ProcInfo0, Body, ProcInfo1),
	proc_info_set_variables(ProcInfo1, VarSet, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo3),
	proc_info_set_argmodes(ProcInfo3, ArgModes, ProcInfo).

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

modecheck_final_insts(HeadVars, ArgFinalInsts, ModeInfo0, ModeInfo) :-
	modecheck_final_insts_2(HeadVars, ArgFinalInsts, ModeInfo0, no,
				_NewFinalInsts, ModeInfo, _Changed).

:- pred modecheck_final_insts_2(list(var), list(inst), mode_info, bool,
					list(inst), mode_info, bool).
:- mode modecheck_final_insts_2(in, in, mode_info_di, in,
					out, mode_info_uo, out) is det.

	% check that the final insts of the head vars matches their
	% expected insts
	%
modecheck_final_insts_2(HeadVars, ArgFinalInsts0, ModeInfo0, Changed0,
			ArgFinalInsts, ModeInfo, Changed) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_instmap(ModeInfo0, InstMap),

	mode_info_get_predid(ModeInfo0, PredId),
	mode_info_get_preds(ModeInfo0, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_get_marker_list(PredInfo, Markers),
	( list__member(request(infer_modes), Markers) ->
		InferModes = yes
	;
		InferModes = no
	),
	check_final_insts(HeadVars, ArgFinalInsts0, ArgFinalInsts1, InferModes,
		1, InstMap, ModuleInfo, no, Changed1, ModeInfo0, ModeInfo),
	( InferModes = yes, Changed1 = yes ->
		normalise_insts(ArgFinalInsts1, ModuleInfo, ArgFinalInsts)
	;
		ArgFinalInsts = ArgFinalInsts0
	),
	bool__or(Changed0, Changed1, Changed).

:- pred check_final_insts(list(var), list(inst), list(inst), bool, int, instmap,
				module_info, bool, bool, mode_info, mode_info).
:- mode check_final_insts(in, in, out, in, in, in, in, in, out, mode_info_di,
				mode_info_uo) is det.

check_final_insts([], [_|_], _, _, _, _, _, _, _) -->
	{ error("check_final_insts: length mismatch") }.
check_final_insts([_|_], [], _, _, _, _, _, _, _) -->
	{ error("check_final_insts: length mismatch") }.
check_final_insts([], [], [], _, _, _, _, Changed, Changed) --> [].
check_final_insts([Var | Vars], [Inst | Insts], [VarInst | VarInsts],
		InferModes, ArgNum, InstMap, ModuleInfo, Changed0, Changed) -->
	{ instmap_lookup_var(InstMap, Var, VarInst) },
	( { inst_matches_final(VarInst, Inst, ModuleInfo) } ->
		{ Changed1 = Changed0 }
	;
		{ Changed1 = yes },
		( { InferModes = yes } ->
			% if we're inferring the mode, then don't report
			% an error, just set changed to yes to make sure
			% that we will do another fixpoint pass
			[]
		;
			% XXX this might need to be reconsidered now we have
			% unique modes
			( { inst_matches_initial(VarInst, Inst, ModuleInfo) } ->
				{ Reason = too_instantiated }
			; { inst_matches_initial(Inst, VarInst, ModuleInfo) } ->
				{ Reason = not_instantiated_enough }
			;
				% I don't think this can happen. 
				% But just in case...
				{ Reason = wrongly_instantiated }
			),
			{ set__init(WaitingVars) },
			mode_info_error(WaitingVars,
				mode_error_final_inst(ArgNum,
				Var, VarInst, Inst, Reason))
		)
	),
	{ ArgNum1 is ArgNum + 1 },
	check_final_insts(Vars, Insts, VarInsts, InferModes, ArgNum1, InstMap,
		ModuleInfo, Changed1, Changed).

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
	term__context_init(EmptyContext),
	( Context = EmptyContext ->
		ModeInfo1 = ModeInfo0
	;
		mode_info_set_context(Context, ModeInfo0, ModeInfo1)
	),
		%
		% modecheck the goal, and then store the changes in
		% instantiation of the vars in the delta_instmap
		% in the goal's goal_info.
		%
	mode_info_get_instmap(ModeInfo1, InstMap0),

	modecheck_goal_2(Goal0, GoalInfo0, Goal, ModeInfo1, ModeInfo),

	mode_info_get_instmap(ModeInfo, InstMap),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
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

modecheck_goal_2(disj(List0, FV), GoalInfo0, disj(List, FV)) -->
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

modecheck_goal_2(if_then_else(Vs, A0, B0, C0, FV), GoalInfo0, Goal) -->
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
		{ Goal = if_then_else(Vs, A, B, C, FV) }
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

modecheck_goal_2(call(PredId0, _, Args0, _, Context, PredName, Follow),
		GoalInfo0, Goal) -->
	% do the last step of type-checking
	=(ModeInfo0),
	{ resolve_pred_overloading(PredId0, Args0, PredName, ModeInfo0,
		PredId) },

	mode_checkpoint(enter, "call"),
	mode_info_set_call_context(call(PredId)),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },

	modecheck_call_pred(PredId, Args0, Mode, Args, ExtraGoals),

	=(ModeInfo),
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ code_util__is_builtin(ModuleInfo, PredId, Mode, Builtin) },
	{ Call = call(PredId, Mode, Args, Builtin, Context, PredName, Follow) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

modecheck_goal_2(higher_order_call(PredVar, Args0, _, _, _, _),
		GoalInfo0, Goal) -->
	modecheck_higher_order_pred_call(PredVar, Args0, GoalInfo0, Goal).

modecheck_goal_2(unify(A0, B0, _, UnifyInfo0, UnifyContext), GoalInfo0, Goal)
		-->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),
	modecheck_unification(A0, B0, UnifyInfo0, UnifyContext, GoalInfo0,
		check_modes, Goal),
	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

modecheck_goal_2(switch(Var, CanFail, Cases0, FV), GoalInfo0,
		switch(Var, CanFail, Cases, FV)) -->
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
modecheck_goal_2(pragma_c_code(C_Code, PredId, _ProcId0, Args0, ArgNameMap), 
		GoalInfo, Goal) -->
	mode_checkpoint(enter, "pragma_c_code"),
	mode_info_set_call_context(call(PredId)),

	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	modecheck_call_pred(PredId, Args0, ProcId, Args, ExtraGoals),

	=(ModeInfo),
	{ Pragma = pragma_c_code(C_Code, PredId, ProcId, Args0, ArgNameMap) },
	{ handle_extra_goals(Pragma, ExtraGoals, GoalInfo, Args0, Args,
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "pragma_c_code").

:- pred modecheck_higher_order_pred_call(var, list(var), hlds__goal_info,
		hlds__goal_expr, mode_info, mode_info).
:- mode modecheck_higher_order_pred_call(in, in, in, out,
		mode_info_di, mode_info_uo) is det.

modecheck_higher_order_pred_call(PredVar, Args0, GoalInfo0, Goal) -->
	mode_checkpoint(enter, "higher-order predicate call"),
	mode_info_set_call_context(higher_order_call(predicate)),
	=(ModeInfo0),

	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	modecheck_higher_order_call(predicate, PredVar, Args0,
			Types, Modes, Det, Args, ExtraGoals),

	=(ModeInfo),
	{ map__init(Follow) },
	{ Call = higher_order_call(PredVar, Args, Types, Modes, Det, Follow) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
				InstMap0, ModeInfo, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order predicate call").

:- pred modecheck_higher_order_func_call(var, list(var), var, hlds__goal_info,
		hlds__goal_expr, mode_info, mode_info).
:- mode modecheck_higher_order_func_call(in, in, in, in, out,
		mode_info_di, mode_info_uo) is det.

modecheck_higher_order_func_call(FuncVar, Args0, RetVar, GoalInfo0, Goal) -->
	mode_checkpoint(enter, "higher-order function call"),
	mode_info_set_call_context(higher_order_call(function)),

	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },

	{ list__append(Args0, [RetVar], Args1) },
	modecheck_higher_order_call(function, FuncVar, Args1,
			Types, Modes, Det, Args, ExtraGoals),

	=(ModeInfo),
	{ map__init(Follow) },
	{ Call = higher_order_call(FuncVar, Args, Types, Modes, Det, Follow) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args1, Args,
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "higher-order function call").

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
unify_rhs_vars(lambda_goal(_PredOrFunc, LambdaVars, _Modes, _Det,
			_Goal - GoalInfo), Vars) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals0),
	set__delete_list(NonLocals0, LambdaVars, NonLocals),
	set__to_sorted_list(NonLocals, Vars).

	% handle_extra_goals combines MainGoal and ExtraGoals into a single
	% hlds__goal_expr.
	%
:- pred handle_extra_goals(hlds__goal_expr, pair(list(hlds__goal)),
		hlds__goal_info, list(var), list(var),
		instmap, mode_info, hlds__goal_expr).
:- mode handle_extra_goals(in, in, in, in, in, in, mode_info_ui, out)
	is det.

handle_extra_goals(MainGoal, ExtraGoals, GoalInfo0, Args0, Args,
		InstMap0, ModeInfo, Goal) :-
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
		set__intersect(OutsideVars, NewArgVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		% compute the instmap delta for the main goal
		mode_info_get_instmap(ModeInfo, InstMap),
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

:- pred handle_extra_goals_contexts(list(hlds__goal), term__context,
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
	( { cons_id_to_const(ConsId, _Const, Arity) } ->
		{ list__duplicate(Arity, free, ArgInsts) },
		modecheck_set_var_inst(Var,
			bound(unique, [functor(ConsId, ArgInsts)]))
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

:- pred modecheck_higher_order_call(pred_or_func, var, list(var),
				list(type), list(mode), determinism, list(var),
				pair(list(hlds__goal)), mode_info, mode_info).
:- mode modecheck_higher_order_call(in, in, in, out, out, out, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_higher_order_call(PredOrFunc, PredVar, Args0, Types, Modes, Det, Args,
		ExtraGoals, ModeInfo0, ModeInfo) :-

	mode_info_get_types_of_vars(ModeInfo0, Args0, Types),

	%
	% First, check that `PredVar' has a higher-order pred inst
	% (of the appropriate arity)
	%
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, PredVar, PredVarInst0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	inst_expand(ModuleInfo0, PredVarInst0, PredVarInst),
	list__length(Args0, Arity),
	(
		PredVarInst = ground(_Uniq, yes(PredInstInfo)),
		PredInstInfo = pred_inst_info(PredOrFunc, Modes0, Det0),
		list__length(Modes0, Arity)
	->
		Modes = Modes0,
		Det = Det0,
		%
		% Next, check that `Args0' have insts which match the expected
		% initial insts, and set their new final insts (introducing
		% extra unifications for implied modes, if necessary).
		%
	/*********************
		% propagate type info into modes
		propagate_type_info_mode_list(Types, ModuleInfo0, Modes1,
			Modes),
	*********************/
		mode_list_get_initial_insts(Modes, ModuleInfo0, InitialInsts),
		modecheck_var_has_inst_list(Args0, InitialInsts, 1,
					ModeInfo0, ModeInfo1),
		mode_list_get_final_insts(Modes, ModuleInfo0, FinalInsts),
		modecheck_set_var_inst_list(Args0, InitialInsts, FinalInsts,
			Args, ExtraGoals, ModeInfo1, ModeInfo2),
		( determinism_components(Det, _, at_most_zero) ->
			mode_info_set_instmap(unreachable, ModeInfo2, ModeInfo)
		;
			ModeInfo = ModeInfo2
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
		TheProcId = 0,
		ArgVars = ArgVars0,
		ExtraGoals = [] - []
	;
		ProcIds = [ProcId],
		\+ list__member(request(infer_modes), Markers)
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
		% We need to either set the final insts of the arg variables
		% to not_reached, or just set the whole instmap to unreachable.
		% We do the latter.
		mode_info_set_instmap(unreachable, ModeInfo1, ModeInfo)
	;
		TheProcId = 0, % dummy value
		mode_info_get_instmap(ModeInfo0, InstMap),
		get_var_insts(ArgVars, InstMap, ArgInsts),
		mode_info_set_call_arg_context(0, ModeInfo0, ModeInfo1),
		mode_info_error(WaitingVars,
			mode_error_no_matching_mode(ArgVars, ArgInsts),
			ModeInfo1, ModeInfo)
	).
	
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
	mode_info_get_instmap(ModeInfo0, InstMap),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	get_var_insts(ArgVars, InstMap, VarInsts),
	normalise_insts(VarInsts, ModuleInfo0, InitialInsts),
	mode_info_get_preds(ModeInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_context(PredInfo0, Context),
	list__length(ArgVars, Arity),
	list__duplicate(Arity, not_reached, FinalInsts),
	inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes),
	MaybeDeterminism = no,

	% create the new mode
	add_new_proc(PredInfo0, Arity, Modes, MaybeDeterminism, Context,
		PredInfo1, ProcId),

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
	mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo).

:- pred normalise_insts(list(inst), module_info, list(inst)).
:- mode normalise_insts(in, in, out) is det.

normalise_insts([], _, []).
normalise_insts([Inst0|Insts0], ModuleInfo, [Inst|Insts]) :-
	normalise_inst(Inst0, ModuleInfo, Inst),
	normalise_insts(Insts0, ModuleInfo, Insts).

:- pred normalise_inst(inst, module_info, inst).
:- mode normalise_inst(in, in, out) is det.

	% This is a bit of a hack.
	% The aim is to avoid non-termination due to the creation
	% of ever-expanding insts.
	% XXX should also normalise partially instantiated insts.

normalise_inst(Inst0, ModuleInfo, NormalisedInst) :-
	inst_expand(ModuleInfo, Inst0, Inst),
	( Inst = bound(_, _) ->
		(
			inst_is_ground(ModuleInfo, Inst),
			inst_is_unique(ModuleInfo, Inst)
		->
			NormalisedInst = ground(unique, no)
		;
			inst_is_ground(ModuleInfo, Inst),
			inst_is_mostly_unique(ModuleInfo, Inst)
		->
			NormalisedInst = ground(mostly_unique, no)
		;
			inst_is_ground(ModuleInfo, Inst),
			\+ inst_is_clobbered(ModuleInfo, Inst)
		->
			NormalisedInst = ground(shared, no)
		;
			% XXX need to limit the potential size of insts
			% here in order to avoid infinite loops in
			% mode inference
			NormalisedInst = Inst
		)
	;
		NormalisedInst = Inst
	).

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
		( inst_matches_binding(VarInst0, InitialInst, ModuleInfo0)
		; inst_expand(ModuleInfo0, VarInst0, any(_)),
		  inst_expand(ModuleInfo0, InitialInst, any(_))
		  % XXX this doesn't handle `any's that are nested inside
		  % the inst -- we really ought to define a predicate
		  % like inst_matches_binding but which allows anys to match
		)
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
			mode_info_get_mode_context(ModeInfo2, ModeContext),
			mode_context_to_unify_context(ModeContext, ModeInfo2,
				UnifyContext),
			categorize_unify_var_var(ModeVar0, ModeVar,
				live, dead, Var0, Var, Det, UnifyContext,
				VarTypes, ModeInfo2,
				AfterGoal, ModeInfo),

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

:- pred mode_context_to_unify_context(mode_context, mode_info, unify_context).
:- mode mode_context_to_unify_context(in, mode_info_ui, out) is det.

mode_context_to_unify_context(unify(UnifyContext, _), _, UnifyContext).
mode_context_to_unify_context(call(PredId, Arg), ModeInfo,
		unify_context(call(PredCallId, Arg), [])) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),
	PredCallId = qualified(Module, Name) / Arity.
mode_context_to_unify_context(higher_order_call(_PredOrFunc, _Arg), _ModeInfo,
		unify_context(explicit, [])).
		% XXX could do better; it's not really explicit
mode_context_to_unify_context(uninitialized, _, _) :-
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

modecheck_unification(X, var(Y), _Unification0, UnifyContext, _GoalInfo, _,
			Unify, ModeInfo0, ModeInfo) :-
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
		mode_info_get_var_types(ModeInfo3, VarTypes),
		categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y,
			Det, UnifyContext, VarTypes, ModeInfo3, Unify, ModeInfo)
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
		Modes = ModeOfX - ModeOfY,
		Unify = unify(X, var(Y), Modes, Unification, UnifyContext)
	).

modecheck_unification(X0, functor(Name, ArgVars0), Unification0,
			UnifyContext, GoalInfo0, HowToCheckGoal,
			Goal, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, X0, TypeOfX),
	module_info_get_predicate_table(ModuleInfo0, PredTable),
	list__length(ArgVars0, Arity),
	mode_info_get_predid(ModeInfo0, ThisPredId),
	(
		%
		% is the function symbol apply/N,
		% representing a higher-order function call?
		%
		% (As an optimization, if HowToCheck = check_unique_modes,
		% then don't bother checking, since they will have already
		% been expanded.)
		%
		HowToCheckGoal \= check_unique_modes,
		Name = term__atom("apply"),
		Arity >= 2,
		ArgVars0 = [FuncVar | FuncArgVars]
	->
		%
		% Convert the higher-order function call (apply/N)
		% into a higher-order predicate call
		% (i.e., replace `X = apply(F, A, B, C)'
		% with `call(F, A, B, C, X)')
		% and then mode-check it.
		%
		modecheck_higher_order_func_call(FuncVar, FuncArgVars, X0,
			GoalInfo0, Goal, ModeInfo0, ModeInfo)
	;
		%
		% is the function symbol a user-defined function, rather
		% than a functor which represents a data constructor?
		%

		% As an optimization, if HowToCheck = check_unique_modes,
		% then don't bother checking, since they will have already
		% been expanded.
		HowToCheckGoal \= check_unique_modes,

		% Find the set of candidate predicates which have the
		% specified name and arity
		% (XXX and module, if module-qualified)
		Name = term__atom(PredName),
		predicate_table_search_func_name_arity(PredTable, PredName,
			Arity, PredIds),

		% Check if there any of the candidate predicates are functions,
		% and have argument/return types which subsume the actual
		% argument/return types of this function call
		module_info_pred_info(ModuleInfo0, ThisPredId, PredInfo),
		pred_info_typevarset(PredInfo, TVarSet),
		map__apply_to_list(ArgVars0, VarTypes0, ArgTypes0),
		list__append(ArgTypes0, [TypeOfX], ArgTypes),
		find_matching_pred_id(PredIds, ModuleInfo0, TVarSet,
			ArgTypes, PredId)
	->
		%
		% Convert function calls into predicate calls:
		% replace `X = f(A, B, C)'
		% with `f(A, B, C, X)'
		%
		ProcId = 0,
		list__append(ArgVars0, [X0], ArgVars),
		hlds__is_builtin_make_builtin(no, no, Builtin),
		FuncCallUnifyContext = call_unify_context(X0,
					functor(Name, ArgVars0), UnifyContext),
		FuncName = unqualified(PredName),
		map__init(Follow),
		FuncCall = call(PredId, ProcId, ArgVars, Builtin,
				yes(FuncCallUnifyContext), FuncName, Follow),
		%
		% now modecheck it
		%
		modecheck_goal_2(FuncCall, GoalInfo0, Goal, ModeInfo0, ModeInfo)

	;

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

		% check if variable has a higher-order pred type
		type_is_higher_order(TypeOfX, PredOrFunc, PredArgTypes),
		Name = term__atom(PName),
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
		get_pred_id_and_proc_id(PName, Arity, PredOrFunc, PredArgTypes,
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
		Functor0 = lambda_goal(PredOrFunc, LambdaVars, LambdaModes,
					LambdaDet, LambdaGoal),
		modecheck_unification( X0, Functor0, Unification0, UnifyContext,
				GoalInfo0, HowToCheckGoal, Goal,
				ModeInfo2, ModeInfo)
	;
		%
		% It's not a higher-order pred unification - just
		% call modecheck_unify_functor to do the ordinary thing.
		%
		mode_info_get_instmap(ModeInfo0, InstMap0),
		modecheck_unify_functor(X0, Name, ArgVars0, Unification0,
				ExtraGoals, Mode, ArgVars, Unification,
				ModeInfo0, ModeInfo),
		%
		% Optimize away construction of unused terms by
		% replace the unification with `true'.
		%
		(
			Unification = construct(ConstructTarget, _, _, _),
			mode_info_var_is_live(ModeInfo, ConstructTarget, dead)
		->
			Goal = conj([])
		;
			Functor = functor(Name, ArgVars),
			Unify = unify(X, Functor, Mode, Unification,
				UnifyContext),
			X = X0,
			%
			% modecheck_unification sometimes needs to introduce
			% new goals to handle complicated sub-unifications
			% in deconstructions.  But this should never happen
			% during unique mode analysis.
			% (If it did, the code would be wrong since it
			% wouldn't have the correct determinism annotations.)
			%
			(
				HowToCheckGoal = check_unique_modes,
				ExtraGoals \= [] - []
			->
				error("unique_modes.m: re-modecheck of unification encountered complicated sub-unifies")
			;
				true
			),

			handle_extra_goals(Unify, ExtraGoals, GoalInfo0,
						[X0|ArgVars0], [X|ArgVars],
						InstMap0, ModeInfo, Goal)
		)
	).

modecheck_unification(X, lambda_goal(PredOrFunc, Vars, Modes, Det, Goal0),
			Unification0, UnifyContext, _GoalInfo, HowToCheckGoal,
			unify(X, RHS, Mode, Unification, UnifyContext),
			ModeInfo0, ModeInfo) :-
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
	modecheck_unify_lambda(X, PredOrFunc, ArgVars, Modes, Det, Unification0,
				Mode, Unification,
				ModeInfo10, ModeInfo),
	RHS = lambda_goal(PredOrFunc, Vars, Modes, Det, Goal).

:- pred modecheck_unify_lambda(var, pred_or_func, list(var),
			list(mode), determinism, unification,
			pair(mode), unification, mode_info, mode_info).
:- mode modecheck_unify_lambda(in, in, in, in, in, in,
			out, out, mode_info_di, mode_info_uo) is det.

modecheck_unify_lambda(X, PredOrFunc, ArgVars, LambdaModes, LambdaDet,
		Unification0, Mode, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstOfX),
	InstOfY = ground(unique, yes(LambdaPredInfo)),
	LambdaPredInfo = pred_inst_info(PredOrFunc, LambdaModes, LambdaDet),
	(
		abstractly_unify_inst(dead, InstOfX, InstOfY, real_unify,
			ModuleInfo0, UnifyInst, _Det, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1),
		ModeOfX = (InstOfX -> Inst),
		ModeOfY = (InstOfY -> Inst),
		Mode = ModeOfX - ModeOfY,
		% the lambda expression just maps its argument variables
		% from their current insts to the same inst
		instmap_lookup_arg_list(ArgVars, InstMap0, ArgInsts),
		inst_lists_to_mode_list(ArgInsts, ArgInsts, ArgModes),
		categorize_unify_var_lambda(ModeOfX, ArgModes,
				X, ArgVars, PredOrFunc,
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

:- pred resolve_pred_overloading(pred_id, list(var), sym_name, mode_info,
				pred_id).
:- mode resolve_pred_overloading(in, in, in, mode_info_ui, out) is det.
	%
	% In the case of a call to an overloaded predicate, typecheck.m
	% does not figure out the correct pred_id.  We must do that here.
	%
resolve_pred_overloading(PredId0, Args0, PredName, ModeInfo0, PredId) :-
	( invalid_pred_id(PredId0) ->
		%
		% Find the set of candidate pred_ids for predicates which
		% have the specified name and arity
		%
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		module_info_get_predicate_table(ModuleInfo0, PredTable),
		(
			predicate_table_search_pred_sym(PredTable, PredName,
				PredIds0)
		->
			PredIds = PredIds0
		;
			PredIds = []
		),

		%
		% Check if there any of the candidate pred_ids 
		% have argument/return types which subsume the actual
		% argument/return types of this function call
		%
		mode_info_get_predid(ModeInfo0, ThisPredId),
		module_info_pred_info(ModuleInfo0, ThisPredId, PredInfo),
		pred_info_typevarset(PredInfo, TVarSet),
		mode_info_get_var_types(ModeInfo0, VarTypes0),
		map__apply_to_list(Args0, VarTypes0, ArgTypes),
		(
			find_matching_pred_id(PredIds, ModuleInfo0,
				TVarSet, ArgTypes, PredId1)
		->
			PredId = PredId1
		;
			% if there is no matching predicate for this call,
			% then this predicate must have a type error which
			% should have been caught by typecheck.m
			error("modes.m: type error in pred call: no matching pred")
		)
	;
		PredId = PredId0
	).

:- pred find_matching_pred_id(list(pred_id), module_info, tvarset, list(type),
				pred_id).
:- mode find_matching_pred_id(in, in, in, in, out) is semidet.

find_matching_pred_id([PredId | PredIds], ModuleInfo, TVarSet, ArgTypes,
		ThePredId) :-
	(
		%
		% lookup the argument types of the candidate predicate
		% (or the argument types + return type of the candidate
		% function)
		%
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_arg_types(PredInfo, PredTVarSet, PredArgTypes0),

		%
		% rename them apart from the actual argument types
		%
		varset__merge_subst(TVarSet, PredTVarSet, _TVarSet1, Subst),
		term__apply_substitution_to_list(PredArgTypes0, Subst,
					PredArgTypes),

		%
		% check that the types of the candidate predicate/function
		% subsume the actual argument types
		%
		type_list_subsumes(PredArgTypes, ArgTypes, _TypeSubst)
	->
		%
		% we've found a matching predicate
		% was there was more than one matching predicate/function?
		%
		(
			find_matching_pred_id(PredIds,
				ModuleInfo, TVarSet, ArgTypes, _OtherPredId)
		->
			% XXX this should report an error properly, not
			% via error/1
			error("Type error in predicate call: unresolvable predicate overloading.  You need to use an explicit module qualifier.  Compile with -V to find out where.")
		;
			ThePredId = PredId
		)
	;
		find_matching_pred_id(PredIds, ModuleInfo,
				TVarSet, ArgTypes, ThePredId)
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
	list__length(ArgVars0, Arity),
	make_functor_cons_id(Name, Arity, ConsId),
	InstOfY = bound(unique, [functor(ConsId, InstArgs)]),
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
			inst_expand(ModuleInfo1, InstOfX, InstOfX1),
			get_arg_insts(InstOfX1, ConsId, Arity, InstOfXArgs),
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
		mode_info_get_mode_context(ModeInfo4, ModeContext),
		mode_context_to_unify_context(ModeContext, ModeInfo4,
						UnifyContext),
		categorize_unify_var_var(ModeVar0, ModeVar,
			live, dead, Var0, Var, Det, UnifyContext,
			VarTypes, ModeInfo4, AfterGoal, ModeInfo),

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

:- pred get_arg_insts(inst, cons_id, arity, list(inst)).
:- mode get_arg_insts(in, in, in, out) is semidet.

get_arg_insts(not_reached, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, not_reached, ArgInsts).
get_arg_insts(ground(Uniq, _PredInst), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, ground(Uniq, no), ArgInsts).
get_arg_insts(bound(_Uniq, List), ConsId, Arity, ArgInsts) :-
	( get_arg_insts_2(List, ConsId, ArgInsts0) ->
		ArgInsts = ArgInsts0
	;
		% the code is unreachable
		list__duplicate(Arity, not_reached, ArgInsts)
	).
get_arg_insts(free, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(free(_Type), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(any(Uniq), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, any(Uniq), ArgInsts).

:- pred get_arg_insts_2(list(bound_inst), cons_id, list(inst)).
:- mode get_arg_insts_2(in, in, out) is semidet.

get_arg_insts_2([BoundInst | BoundInsts], ConsId, ArgInsts) :-
	(
		BoundInst = functor(ConsId, ArgInsts0)
	->
		ArgInsts = ArgInsts0
	;
		get_arg_insts_2(BoundInsts, ConsId, ArgInsts)
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
get_mode_of_args(any(Uniq), ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, any(Uniq), ArgModes).
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
			determinism, unify_context, map(var, type), mode_info,
			hlds__goal_expr, mode_info).
:- mode categorize_unify_var_var(in, in, in, in, in, in, in, in, in,
			mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_var(ModeOfX, ModeOfY, LiveX, LiveY, X, Y, Det,
		UnifyContext, VarTypes, ModeInfo0, Unify, ModeInfo) :-
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
				type_is_higher_order(Type, PredOrFunc, _)
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
			mode_error_unify_pred(X, error_at_var(Y), Type, PredOrFunc),
						ModeInfo0, ModeInfo)
				)
			;
				type_to_type_id(Type, TypeId, _)
			->
				unify_proc__request_unify(TypeId - UniMode, Det,
					ModuleInfo0, ModuleInfo),
				mode_info_set_module_info(ModeInfo0, ModuleInfo,
					ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		)
	),
	%
	% Optimize away unifications with dead variables
	% and simple tests that cannot fail
	% by replacing them with `true'.
	% (The optimization of simple tests is necessary
	% because otherwise determinism analysis assumes they can fail.
	% The optimization of assignments to dead variables may be
	% necessary to stop the code generator from getting confused.)
	%
	(
		Unification = assign(AssignTarget, _),
		mode_info_var_is_live(ModeInfo, AssignTarget, dead)
	->
		Unify = conj([])
	;
		Unification = simple_test(_, _),
		Det = det
	->
		Unify = conj([])
	;
		Unify = unify(X, var(Y), ModeOfX - ModeOfY, Unification,
				UnifyContext)
	).

% categorize_unify_var_lambda works out which category a unification
% between a variable and a lambda expression is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_lambda(mode, list(mode),
			var, list(var), pred_or_func,
			unification, mode_info, unification, mode_info).
:- mode categorize_unify_var_lambda(in, in, in, in, in,
			in, mode_info_di, out, mode_info_uo) is det.

categorize_unify_var_lambda(ModeOfX, ArgModes0, X, ArgVars, PredOrFunc,
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
		mode_info_get_var_types(ModeInfo0, VarTypes0),
		map__lookup(VarTypes0, X, Type),
		mode_info_error(WaitingVars,
				mode_error_unify_pred(X,
					error_at_lambda(ArgVars, ArgModes0),
					Type, PredOrFunc),
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
			( type_is_higher_order(TypeOfX, PredOrFunc, _) ->
				set__init(WaitingVars),
				mode_info_error(WaitingVars,
			mode_error_unify_pred(X,
					error_at_functor(Name, ArgVars),
					TypeOfX, PredOrFunc),
					ModeInfo0, ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		),
		Unification = deconstruct(X, ConsId, ArgVars, ArgModes, CanFail)
	).

:- pred get_pred_id_and_proc_id(string, arity, pred_or_func, list(type),
				module_info, pred_id, proc_id).
:- mode get_pred_id_and_proc_id(in, in, in, in, in, out, out) is det.

get_pred_id_and_proc_id(Name, Arity, PredOrFunc, PredArgTypes, ModuleInfo,
			PredId, ProcId) :-
	list__length(PredArgTypes, PredArity),
	TotalArity is Arity + PredArity,
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
	    predicate_table_search_pf_name_arity(PredicateTable,
		PredOrFunc, Name, TotalArity, PredIds)
	->
	    (
		PredIds = [PredId0]
	    ->
		PredId = PredId0,
		predicate_table_get_preds(PredicateTable, Preds),
		map__lookup(Preds, PredId, PredInfo),
		pred_info_procedures(PredInfo, Procs),
		map__keys(Procs, ProcIds),
		( ProcIds = [ProcId0] ->
		    ProcId = ProcId0
		; ProcIds = [] ->
		    hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		    string__int_to_string(TotalArity, TotalArityString),
		    string__append_list([
			    "cannot take address of ", PredOrFuncStr,
			    "\n`", Name, "/", TotalArityString,
			    "' with no modes.\n",
			    "(Sorry, confused by earlier errors ",
			    	"-- bailing out.)"],
			    Message),
		    error(Message)
		;
		    hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		    string__int_to_string(TotalArity, TotalArityString),
		    string__append_list([
			    "sorry, not implemented: ",
			    "taking address of ", PredOrFuncStr,
			    "\n`", Name, "/", TotalArityString,
			    "' with multiple modes.\n",
			    "(use an explicit lambda expression instead)"],
			    Message),
		    error(Message)
		)
	    ;
	        % Ambiguous pred or func.
		% cons_id ought to include the module prefix, so
		% that we could use predicate_table__search_m_n_a to 
		% prevent this from happening
	        hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
	        string__int_to_string(TotalArity, TotalArityString),
		string__append_list(
			["get_pred_id_and_proc_id: ",
			"ambiguous ", PredOrFuncStr,
		        "\n`", Name, "/", TotalArityString, "'"],
			Msg),
		error(Msg)
	    )
	;
	    % Undefined/invalid pred or func.
	    % the type-checker should ensure that this never happens
	    hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
	    string__int_to_string(TotalArity, TotalArityString),
	    string__append_list(
		["get_pred_id_and_proc_id: ",
		"undefined/invalid ", PredOrFuncStr,
		"\n`", Name, "/", TotalArityString, "'"],
		Msg),
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
