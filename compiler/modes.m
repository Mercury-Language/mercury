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
		unless one of them is dead.  Split unifications
		up if necessary to avoid complicated sub-unifications.
		We also figure out at this point whether or not each
		unification can fail.
	(e) a predicate call
		Check that there is a mode declaration for the
		predicate which matches the current instantiation of
		the arguments.  (Also handle calls to implied modes.)
		If the called predicate is one for which we must infer
		the modes, then create a new mode for the called predicate
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

% XXX we ought to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is 
%     dead (causes problems handling e.g. `list__same_length').

% XXX we ought to break unifications into "micro-unifications", because
%     some code can't be scheduled without splitting up unifications.
%     For example, `p(X) :- X = f(A, B), B is A + 1.', where
%     p is declared as `:- mode p(bound(f(ground,free))->ground).'.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modes.

:- interface.

:- import_module hlds_module, hlds_pred, instmap.
:- import_module bool, io.

	% modecheck(HLDS0, HLDS, UnsafeToContinue):
	% Perform mode inference and checking for a whole module.
	% UnsafeToContinue = yes means that mode inference
	% was halted prematurely, due to an error, and that
	% we should therefore not perform determinism-checking, because we
	% might get internal errors.

:- pred modecheck(module_info, module_info, bool, io__state, io__state).
:- mode modecheck(in, out, out, di, uo) is det.

	% Mode-check the code for single predicate.

:- pred modecheck_pred_mode(pred_id, pred_info, module_info, module_info,
			int, io__state, io__state).
:- mode modecheck_pred_mode(in, in, in, out, out, di, uo) is det.

	% Mode-check the code for predicate in a given mode.

:- pred modecheck_proc(proc_id, pred_id, module_info, module_info, int,
			io__state, io__state).
:- mode modecheck_proc(in, in, in, out, out, di, uo) is det.

:- pred modecheck_proc_info(proc_id, pred_id, module_info, proc_info,
		module_info, proc_info, int, io__state, io__state).
:- mode modecheck_proc_info(in, in, in, in, out, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by unique_modes.m.

:- import_module mode_info.

	% Modecheck a unification.

	% This argument specifies how to recursively process lambda goals -
	% using either modes.m or unique_modes.m.
:- type how_to_check_goal
	--->	check_modes
	;	check_unique_modes.

 	% given the right-hand-side of a unification, return a list of
	% the potentially non-local variables of that unification.
	%
:- pred unify_rhs_vars(unify_rhs, list(var)).
:- mode unify_rhs_vars(in, out) is det.

	% Given a list of variables, and a list of livenesses,
	% select the live variables.
	%
:- pred get_live_vars(list(var), list(is_live), list(var)).
:- mode get_live_vars(in, in, out) is det.

	% Given a list of variables and a list of expected liveness, ensure
	% that the inst of each variable satisfies the corresponding expected
	% liveness.
	%
:- pred modecheck_var_list_is_live(list(var), list(is_live), int, mode_info,
					mode_info).
:- mode modecheck_var_list_is_live(in, in, in, mode_info_di, mode_info_uo)
	is det.

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
					list(var), pair(list(hlds_goal)),
					mode_info, mode_info).
:- mode modecheck_set_var_inst_list(in, in, in, out, out,
					mode_info_di, mode_info_uo) is det.

	% check that the final insts of the head vars of a lambda
	% goal matches their expected insts
	%
:- pred modecheck_final_insts(list(var), list(inst), mode_info, mode_info).
:- mode modecheck_final_insts(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_goals_live_vars(list(hlds_goal), mode_info, mode_info).
:- mode mode_info_add_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_remove_goals_live_vars(list(hlds_goal), mode_info,
					mode_info).
:- mode mode_info_remove_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by modecheck_unify.m.

:- pred modecheck_goal(hlds_goal, hlds_goal, mode_info, mode_info).
:- mode modecheck_goal(in, out, mode_info_di, mode_info_uo) is det.

	% Mode-check a single goal-expression.

:- pred modecheck_goal_expr(hlds_goal_expr, hlds_goal_info, hlds_goal_expr,
			mode_info, mode_info).
:- mode modecheck_goal_expr(in, in, out, mode_info_di, mode_info_uo) is det.

	% handle_extra_goals combines MainGoal and ExtraGoals into a single
	% hlds_goal_expr.
	%
:- pred handle_extra_goals(hlds_goal_expr, pair(list(hlds_goal)),
		hlds_goal_info, list(var), list(var),
		instmap, mode_info, hlds_goal_expr).
:- mode handle_extra_goals(in, in, in, in, in, in, mode_info_ui, out)
	is det.

:- pred mode_context_to_unify_context(mode_context, mode_info, unify_context).
:- mode mode_context_to_unify_context(in, mode_info_ui, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module make_hlds, hlds_goal, hlds_data, unique_modes, mode_debug.
:- import_module mode_info, delay_info, mode_errors, inst_match, instmap.
:- import_module type_util, mode_util, code_util, prog_data, unify_proc.
:- import_module globals, options, mercury_to_mercury, hlds_out, int, set.
:- import_module passes_aux, typecheck, module_qual, clause_to_proc.
:- import_module modecheck_unify, modecheck_call.
:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module assoc_list.

%-----------------------------------------------------------------------------%

modecheck(Module0, Module, UnsafeToContinue) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),

	maybe_write_string(Verbose, "% Mode-checking clauses...\n"),
	check_pred_modes(Module0, Module, UnsafeToContinue),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%
	
	% Mode-check the code for all the predicates in a module.

:- pred check_pred_modes(module_info, module_info, bool, io__state, io__state).
:- mode check_pred_modes(in, out, out, di, uo) is det.

check_pred_modes(ModuleInfo0, ModuleInfo, UnsafeToContinue) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	{ MaxIterations = 30 }, % XXX FIXME should be command-line option
	modecheck_to_fixpoint(PredIds, MaxIterations, ModuleInfo0,
					ModuleInfo1, UnsafeToContinue),
	write_mode_inference_messages(PredIds, ModuleInfo1),
	modecheck_unify_procs(check_modes, ModuleInfo1, ModuleInfo).

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_to_fixpoint(list(pred_id), int, module_info, 
			module_info, bool, io__state, io__state).
:- mode modecheck_to_fixpoint(in, in, in, out, out, di, uo) is det.

modecheck_to_fixpoint(PredIds, MaxIterations, ModuleInfo0,
		ModuleInfo, UnsafeToContinue) -->
	{ copy_module_clauses_to_procs(PredIds, ModuleInfo0, ModuleInfo1) },
	modecheck_pred_modes_2(PredIds, ModuleInfo1, ModuleInfo2, no, Changed,
				0, NumErrors),
	% stop if we have reached a fixpoint or found any errors
	( { Changed = no ; NumErrors > 0 } ->
		{ ModuleInfo = ModuleInfo2 },
		{ UnsafeToContinue = Changed }
	;
		% stop if we exceed the iteration limit
		( { MaxIterations =< 1 } ->
			report_max_iterations_exceeded,
			{ ModuleInfo = ModuleInfo2 },
			{ UnsafeToContinue = yes }
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
				ModuleInfo2, ModuleInfo, UnsafeToContinue)
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
:- mode modecheck_pred_modes_2(in, in, out,
			in, out, in, out, di, uo) is det.

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
	modecheck_procs(ProcIds, PredId, ModuleInfo1, Changed1,
			Errs1, ModuleInfo, Changed, Errs).

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

modecheck_proc_info(ProcId, PredId, ModuleInfo0, ProcInfo0,
		ModuleInfo, ProcInfo, NumErrors) -->
	modecheck_proc_3(ProcId, PredId, ModuleInfo0, ProcInfo0, no,
		ModuleInfo, ProcInfo, _Changed, NumErrors).

:- pred modecheck_proc_3(proc_id, pred_id, module_info, proc_info, bool,
			module_info, proc_info, bool, int,
			io__state, io__state).
:- mode modecheck_proc_3(in, in, in, in, in, out, out, out, out, di, uo)
	is det.

modecheck_proc_3(ProcId, PredId, ModuleInfo0, ProcInfo0, Changed0,
				ModuleInfo, ProcInfo, Changed, NumErrors,
				IOState0, IOState) :-
		% extract the useful fields in the proc_info
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	proc_info_arglives(ProcInfo0, ModuleInfo0, ArgLives0),
	proc_info_goal(ProcInfo0, Body0),

		% We use the context of the first clause, unless
		% there weren't any clauses at all, in which case
		% we use the context of the mode declaration.
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	ClausesInfo = clauses_info(_, _, _, _, ClauseList),
	( ClauseList = [FirstClause | _] ->
		FirstClause = clause(_, _, Context)
	;
		proc_info_context(ProcInfo0, Context)
	),

		% modecheck the clause - first set the initial instantiation
		% of the head arguments, mode-check the body, and
		% then check that the final instantiation matches that in
		% the mode declaration
	mode_list_get_initial_insts(ArgModes0, ModuleInfo0, ArgInitialInsts),
	assoc_list__from_corresponding_lists(HeadVars, ArgInitialInsts, InstAL),
	instmap__from_assoc_list(InstAL, InstMap0),
		% initially, only the non-clobbered head variables are live
	mode_list_get_final_insts(ArgModes0, ModuleInfo0, ArgFinalInsts0),
	get_live_vars(HeadVars, ArgLives0, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),
	mode_info_init(IOState0, ModuleInfo0, PredId, ProcId,
			Context, LiveVars, InstMap0, ModeInfo0),
	mode_info_set_changed_flag(Changed0, ModeInfo0, ModeInfo1),
	modecheck_goal(Body0, Body, ModeInfo1, ModeInfo2),
	pred_info_get_marker_list(PredInfo, Markers),
	( list__member(request(infer_modes), Markers) ->
		InferModes = yes
	;
		InferModes = no
	),
	modecheck_final_insts_2(HeadVars, ArgFinalInsts0, ModeInfo2,
			InferModes, ArgFinalInsts, ModeInfo3),
	inst_lists_to_mode_list(ArgInitialInsts, ArgFinalInsts, ArgModes),
	report_mode_errors(ModeInfo3, ModeInfo),
	mode_info_get_changed_flag(ModeInfo, Changed),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_num_errors(ModeInfo, NumErrors),
	mode_info_get_io_state(ModeInfo, IOState),
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	proc_info_set_goal(ProcInfo0, Body, ProcInfo1),
	proc_info_set_variables(ProcInfo1, VarSet, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo3),
	proc_info_set_argmodes(ProcInfo3, ArgModes, ProcInfo).

	% modecheck_final_insts for a lambda expression
modecheck_final_insts(HeadVars, ArgFinalInsts, ModeInfo0, ModeInfo) :-
		% for lambda expressions, modes must always be
		% declared, we never infer them.
	InferModes = no,
	modecheck_final_insts_2(HeadVars, ArgFinalInsts, ModeInfo0,
			InferModes, _NewFinalInsts, ModeInfo).

:- pred modecheck_final_insts_2(list(var), list(inst), mode_info, bool,
					list(inst), mode_info).
:- mode modecheck_final_insts_2(in, in, mode_info_di, in,
					out, mode_info_uo) is det.

	% check that the final insts of the head vars matches their
	% expected insts
	%
modecheck_final_insts_2(HeadVars, FinalInsts0, ModeInfo0, InferModes,
			FinalInsts, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_instmap(ModeInfo0, InstMap),
	instmap__lookup_vars(HeadVars, InstMap, VarFinalInsts1),

	( InferModes = yes ->
		normalise_insts(VarFinalInsts1, ModuleInfo, VarFinalInsts2),
		%
		% make sure we set the final insts of any variables which
		% we assumed were dead to `clobbered'.
		%
		mode_info_get_preds(ModeInfo0, Preds),
		mode_info_get_predid(ModeInfo0, PredId),
		map__lookup(Preds, PredId, PredInfo),
		pred_info_procedures(PredInfo, Procs),
		mode_info_get_procid(ModeInfo0, ProcId),
		map__lookup(Procs, ProcId, ProcInfo),
		proc_info_arglives(ProcInfo, ModuleInfo, ArgLives),
		maybe_clobber_insts(VarFinalInsts2, ArgLives, FinalInsts),
		check_final_insts(HeadVars, FinalInsts0, FinalInsts,
			InferModes, 1, ModuleInfo, no, Changed1,
			ModeInfo0, ModeInfo1),
		mode_info_get_changed_flag(ModeInfo1, Changed0),
		bool__or(Changed0, Changed1, Changed),
		mode_info_set_changed_flag(Changed, ModeInfo1, ModeInfo)
	;
		check_final_insts(HeadVars, FinalInsts0, VarFinalInsts1,
			InferModes, 1, ModuleInfo, no, _Changed1,
			ModeInfo0, ModeInfo),
		FinalInsts = FinalInsts0
	).

:- pred maybe_clobber_insts(list(inst), list(is_live), list(inst)).
:- mode maybe_clobber_insts(in, in, out) is det.

maybe_clobber_insts([], [_|_], _) :-
	error("maybe_clobber_insts: length mismatch").
maybe_clobber_insts([_|_], [], _) :-
	error("maybe_clobber_insts: length mismatch").
maybe_clobber_insts([], [], []).
maybe_clobber_insts([Inst0 | Insts0], [IsLive | IsLives], [Inst | Insts]) :-
	( IsLive = dead ->
		Inst = ground(clobbered, no)
	;
		Inst = Inst0
	),
	maybe_clobber_insts(Insts0, IsLives, Insts).

:- pred check_final_insts(list(var), list(inst), list(inst), bool, int,
				module_info, bool, bool, mode_info, mode_info).
:- mode check_final_insts(in, in, in, in, in, in, in, out, mode_info_di,
				mode_info_uo) is det.

check_final_insts(Vars, Insts, VarInsts, InferModes, ArgNum, ModuleInfo,
		Changed0, Changed) -->
	( { Vars = [], Insts = [], VarInsts = [] } ->
		{ Changed = Changed0 }
	; { Vars = [Var|Vars1], Insts = [Inst|Insts1],
	    VarInsts = [VarInst|VarInsts1] } ->
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
				% XXX this might need to be reconsidered now
				% we have unique modes
				( { inst_matches_initial(VarInst, Inst,
				    ModuleInfo) } ->
					{ Reason = too_instantiated }
				; { inst_matches_initial(Inst, VarInst,
				    ModuleInfo) } ->
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
		check_final_insts(Vars1, Insts1, VarInsts1, InferModes, ArgNum1,
			ModuleInfo, Changed1, Changed)
	;
		{ error("check_final_insts: length mismatch") }
	).

%-----------------------------------------------------------------------------%
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

modecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, ModeInfo0, ModeInfo) :-
		%
		% store the current context in the mode_info
		%
	goal_info_get_context(GoalInfo0, Context),
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

	modecheck_goal_expr(Goal0, GoalInfo0, Goal, ModeInfo1, ModeInfo),

	mode_info_get_instmap(ModeInfo, InstMap),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).

modecheck_goal_expr(conj(List0), _GoalInfo0, conj(List)) -->
	mode_checkpoint(enter, "conj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] }
	;
		modecheck_conj_list(List0, List)
	),
	mode_checkpoint(exit, "conj").

modecheck_goal_expr(disj(List0, SM), GoalInfo0, disj(List, SM)) -->
	mode_checkpoint(enter, "disj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		modecheck_disj_list(List0, List, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "disj").

modecheck_goal_expr(if_then_else(Vs, A0, B0, C0, SM), GoalInfo0, Goal) -->
	mode_checkpoint(enter, "if-then-else"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	{ goal_get_nonlocals(B0, B_Vars) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(NonLocals),
	mode_info_add_live_vars(B_Vars),
	modecheck_goal(A0, A),
	mode_info_remove_live_vars(B_Vars),
	mode_info_unlock_vars(NonLocals),
	modecheck_goal(B0, B),
	mode_info_dcg_get_instmap(InstMapB),
	mode_info_set_instmap(InstMap0),
	modecheck_goal(C0, C),
	mode_info_dcg_get_instmap(InstMapC),
	mode_info_set_instmap(InstMap0),
	instmap__merge(NonLocals, [InstMapB, InstMapC], if_then_else),
	{ Goal = if_then_else(Vs, A, B, C, SM) },
	mode_checkpoint(exit, "if-then-else").

modecheck_goal_expr(not(A0), GoalInfo0, not(A)) -->
	mode_checkpoint(enter, "not"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(NonLocals),
	modecheck_goal(A0, A),
	mode_info_unlock_vars(NonLocals),
	mode_info_set_instmap(InstMap0),
	mode_checkpoint(exit, "not").

modecheck_goal_expr(some(Vs, G0), _, some(Vs, G)) -->
	mode_checkpoint(enter, "some"),
	modecheck_goal(G0, G),
	mode_checkpoint(exit, "some").

modecheck_goal_expr(call(PredId0, _, Args0, _, Context, PredName0),
                GoalInfo0, Goal) -->
        % do the last step of type-checking
        =(ModeInfo0),
        { resolve_pred_overloading(PredId0, Args0, PredName0, PredName,
                ModeInfo0, PredId) },

	mode_checkpoint(enter, "call"),
	mode_info_set_call_context(call(PredId)),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },

	modecheck_call_pred(PredId, Args0, Mode, Args, ExtraGoals),

	=(ModeInfo),
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ code_util__builtin_state(ModuleInfo, PredId, Mode, Builtin) },
	{ Call = call(PredId, Mode, Args, Builtin, Context, PredName) },
	{ handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

modecheck_goal_expr(higher_order_call(PredVar, Args0, _, _, _),
		GoalInfo0, Goal) -->
	modecheck_higher_order_pred_call(PredVar, Args0, GoalInfo0, Goal).

modecheck_goal_expr(unify(A0, B0, _, UnifyInfo0, UnifyContext), GoalInfo0, Goal)
		-->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),
	modecheck_unification(A0, B0, UnifyInfo0, UnifyContext, GoalInfo0,
		check_modes, Goal),
	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

modecheck_goal_expr(switch(Var, CanFail, Cases0, SM), GoalInfo0,
		switch(Var, CanFail, Cases, SM)) -->
	mode_checkpoint(enter, "switch"),
	( { Cases0 = [] } ->
		{ Cases = [] },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		modecheck_case_list(Cases0, Var, Cases, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "switch").

	% to modecheck a pragma_c_code, we just modecheck the proc for 
	% which it is the goal.
modecheck_goal_expr(pragma_c_code(IsRecursive, C_Code, PredId, _ProcId0, Args0,
		ArgNameMap, OrigArgTypes, ExtraPragmaInfo), GoalInfo, Goal) -->
	mode_checkpoint(enter, "pragma_c_code"),
	mode_info_set_call_context(call(PredId)),

	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	modecheck_call_pred(PredId, Args0, ProcId, Args, ExtraGoals),

	=(ModeInfo),
	{ Pragma = pragma_c_code(IsRecursive, C_Code, PredId, ProcId, Args0,
			ArgNameMap, OrigArgTypes, ExtraPragmaInfo) },
	{ handle_extra_goals(Pragma, ExtraGoals, GoalInfo, Args0, Args,
				InstMap0, ModeInfo, Goal) },

	mode_info_unset_call_context,
	mode_checkpoint(exit, "pragma_c_code").

 	% given the right-hand-side of a unification, return a list of
	% the potentially non-local variables of that unification.

unify_rhs_vars(var(Var), [Var]).
unify_rhs_vars(functor(_Functor, Vars), Vars).
unify_rhs_vars(lambda_goal(_PredOrFunc, LambdaVars, _Modes, _Det,
			_Goal - GoalInfo), Vars) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals0),
	set__delete_list(NonLocals0, LambdaVars, NonLocals),
	set__to_sorted_list(NonLocals, Vars).

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
		goal_info_get_context(GoalInfo0, Context),
		handle_extra_goals_contexts(BeforeGoals0, Context, BeforeGoals),
		handle_extra_goals_contexts(AfterGoals0, Context, AfterGoals),
		list__append(BeforeGoals, [Goal0 | AfterGoals], GoalList),
		Goal = conj(GoalList)
	).

:- pred handle_extra_goals_contexts(list(hlds_goal), term__context,
	list(hlds_goal)).
:- mode handle_extra_goals_contexts(in, in, out) is det.

handle_extra_goals_contexts([], _Context, []).
handle_extra_goals_contexts([Goal0 | Goals0], Context, [Goal | Goals]) :-
	Goal0 = Expr - GoalInfo0,
	Goal  = Expr - GoalInfo,
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	handle_extra_goals_contexts(Goals0, Context, Goals).

:- pred goal_get_nonlocals(hlds_goal, set(var)).
:- mode goal_get_nonlocals(in, out) is det.

goal_get_nonlocals(_Goal - GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

%-----------------------------------------------------------------------------%

:- pred modecheck_conj_list(list(hlds_goal), list(hlds_goal),
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

:- pred modecheck_conj_list_2(list(hlds_goal), list(hlds_goal),
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
	( { instmap__is_unreachable(InstMap) } ->
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

:- pred modecheck_disj_list(list(hlds_goal), list(hlds_goal), list(instmap),
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
	{ cons_id_arity(ConsId, Arity) },
	{ list__duplicate(Arity, free, ArgInsts) },
	modecheck_set_var_inst(Var,
		bound(unique, [functor(ConsId, ArgInsts)])),

	modecheck_goal(Goal0, Goal1),
	mode_info_dcg_get_instmap(InstMap),
	% Don't lose the information added by the functor test above.
	{ fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal) },
	mode_info_set_instmap(InstMap0),
	modecheck_case_list(Cases0, Var, Cases, InstMaps).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of expected livenesses,
	% ensure the liveness of each variable satisfies the corresponding
	% expected liveness.

modecheck_var_list_is_live([_|_], [], _) -->
	{ error("modecheck_var_list_is_live: length mismatch") }.
modecheck_var_list_is_live([], [_|_], _) -->
	{ error("modecheck_var_list_is_live: length mismatch") }.
modecheck_var_list_is_live([], [], _ArgNum) --> [].
modecheck_var_list_is_live([Var|Vars], [IsLive|IsLives], ArgNum0) -->
	{ ArgNum is ArgNum0 + 1 },
	mode_info_set_call_arg_context(ArgNum),
	modecheck_var_is_live(Var, IsLive),
	modecheck_var_list_is_live(Vars, IsLives, ArgNum).

:- pred modecheck_var_is_live(var, is_live, mode_info, mode_info).
:- mode modecheck_var_is_live(in, in, mode_info_di, mode_info_uo) is det.

	% `live' means possibly used later on, and
	% `dead' means definitely not used later on.
	% The only time you get an error is if you pass a variable
	% which is live to a predicate that expects the variable to
	% be dead; the predicate may use destructive update to clobber
	% the variable, so we must be sure that it is dead after the call.

modecheck_var_is_live(VarId, ExpectedIsLive, ModeInfo0, ModeInfo) :-
	mode_info_var_is_live(ModeInfo0, VarId, VarIsLive),
	( ExpectedIsLive = dead, VarIsLive = live ->
		set__singleton_set(WaitingVars, VarId),
		mode_info_error(WaitingVars, mode_error_var_is_live(VarId),
			ModeInfo0, ModeInfo)
	;
		ModeInfo = ModeInfo0
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
	instmap__lookup_var(InstMap, VarId, VarInst),

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
					list(var), pair(list(hlds_goal)),
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

:- pred modecheck_set_var_inst(var, inst, inst, var, pair(list(hlds_goal)),
				mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var0, InitialInst, FinalInst, Var, Goals,
			ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( instmap__is_reachable(InstMap0) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap__lookup_var(InstMap0, Var0, VarInst0),
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
	( instmap__is_reachable(InstMap0) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap__lookup_var(InstMap0, Var0, Inst0),
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
			% if the top-level inst of the variable is not_reached,
			% then the instmap as a whole must be unreachable
			inst_expand(ModuleInfo, Inst, not_reached)
		->
			instmap__init_unreachable(InstMap),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo)
		;
			% If we haven't added any information and
			% we haven't bound any part of the var, then
			% the only thing we can have done is lose uniqueness.
			inst_matches_initial(Inst0, Inst, ModuleInfo)
		->
			instmap__set(InstMap0, Var0, Inst, InstMap),
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
			instmap__set(InstMap0, Var0, Inst, InstMap),
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
			instmap__set(InstMap0, Var0, Inst, InstMap),
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
				var, pair(list(hlds_goal)),
				mode_info, mode_info).
:- mode handle_implied_mode(in, in, in, in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

handle_implied_mode(Var0, VarInst0, VarInst, InitialInst, FinalInst, Det,
		Var, Goals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		% If the initial inst of the variable matches_final
		% the initial inst specified in the pred's mode declaration,
		% then it's not a call to an implied mode, it's an exact
		% match with a genuine mode.
		inst_matches_final(VarInst0, InitialInst, ModuleInfo0)
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
			( VarInst = VarInst0 ->
				InstMapDeltaAL0 = []
			;
				InstMapDeltaAL0 = [Var0 - VarInst]
			),
			
			InstMapDeltaAL = [Var - VarInst | InstMapDeltaAL0],
			goal_info_init(GoalInfo0),
			goal_info_set_nonlocals(GoalInfo0, NonLocals,
				GoalInfo1),
			instmap_delta_from_assoc_list(InstMapDeltaAL,
				InstMapDelta),
			goal_info_set_instmap_delta(GoalInfo1, InstMapDelta,
				GoalInfo),
			Goals = [] - [AfterGoal - GoalInfo]
		)
	).

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

:- pred resolve_pred_overloading(pred_id, list(var), sym_name, sym_name,
                                mode_info, pred_id).
:- mode resolve_pred_overloading(in, in, in, out, mode_info_ui, out) is det.
        %
        % In the case of a call to an overloaded predicate, typecheck.m
        % does not figure out the correct pred_id.  We must do that here.
        %
resolve_pred_overloading(PredId0, Args0, PredName0, PredName,
                        ModeInfo0, PredId) :-
        ( invalid_pred_id(PredId0) ->
                %
                % Find the set of candidate pred_ids for predicates which
                % have the specified name and arity
                %
                mode_info_get_module_info(ModeInfo0, ModuleInfo0),
                mode_info_get_predid(ModeInfo0, ThisPredId),
                module_info_pred_info(ModuleInfo0, ThisPredId, PredInfo),
                pred_info_typevarset(PredInfo, TVarSet),
                mode_info_get_var_types(ModeInfo0, VarTypes0),
                typecheck__resolve_pred_overloading(ModuleInfo0, Args0,
                        VarTypes0, TVarSet, PredName0, PredName, PredId)
        ;
                PredId = PredId0,
                PredName = PredName0
        ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given a list of variables, and a list of livenesses,
	% select the live variables.

get_live_vars([_|_], [], _) :- error("get_live_vars: length mismatch").
get_live_vars([], [_|_], _) :- error("get_live_vars: length mismatch").
get_live_vars([], [], []).
get_live_vars([Var|Vars], [IsLive|IsLives], LiveVars) :-
	( IsLive = live ->
		LiveVars = [Var | LiveVars0]
	;
		LiveVars = LiveVars0
	),
	get_live_vars(Vars, IsLives, LiveVars0).

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
