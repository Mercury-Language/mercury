%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
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

:- module check_hlds__modes.

:- interface.

:- import_module parse_tree__prog_data, hlds__hlds_goal, hlds__hlds_module.
:- import_module hlds__hlds_pred, (parse_tree__inst), hlds__instmap.
:- import_module bool, list, io.

	% modecheck(HLDS0, HLDS, UnsafeToContinue):
	% Perform mode inference and checking for a whole module.
	% UnsafeToContinue = yes means that mode inference
	% was halted prematurely, due to an error, and that
	% we should therefore not perform determinism-checking, because we
	% might get internal errors.

:- pred modecheck(module_info, module_info, bool, io__state, io__state).
:- mode modecheck(in, out, out, di, uo) is det.

	% Mode-check or unique-mode-check the code for all the predicates
	% in a module.
:- pred check_pred_modes(how_to_check_goal, may_change_called_proc,
		module_info, module_info, bool, io__state, io__state).
:- mode check_pred_modes(in, in, in, out, out, di, uo) is det.

	% Mode-check or unique-mode-check the code for single predicate.

:- pred modecheck_pred_mode(pred_id, pred_info, how_to_check_goal,
		may_change_called_proc, module_info, module_info,
		int, io__state, io__state).
:- mode modecheck_pred_mode(in, in, in, in, in, out, out, di, uo) is det.

	% Mode-check the code for predicate in a given mode.
	% Returns the number of errs found and a bool `Changed'
	% which is true iff another pass of fixpoint analysis
	% may be needed.

:- pred modecheck_proc(proc_id, pred_id, module_info, module_info, int, bool,
			io__state, io__state).
:- mode modecheck_proc(in, in, in, out, out, out, di, uo) is det.

	% Mode-check or unique-mode-check the code for predicate in a
	% given mode.
	% Returns the number of errs found and a bool `Changed'
	% which is true iff another pass of fixpoint analysis
	% may be needed.

:- pred modecheck_proc(proc_id, pred_id, how_to_check_goal,
		may_change_called_proc, module_info, module_info, int, bool,
		io__state, io__state).
:- mode modecheck_proc(in, in, in, in, in, out, out, out, di, uo) is det.

	% Mode-check the code for predicate in a given mode.

:- pred modecheck_proc_info(proc_id, pred_id, module_info, proc_info,
		module_info, proc_info, int, io__state, io__state).
:- mode modecheck_proc_info(in, in, in, in, out, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by unique_modes.m.

:- import_module check_hlds__mode_info, hlds__hlds_data.

	% Modecheck a unification.

	% Given a list of variables, and a list of livenesses,
	% select the live variables.
	%
:- pred get_live_vars(list(prog_var), list(is_live), list(prog_var)).
:- mode get_live_vars(in, in, out) is det.

	%
	% calculate the argument number offset that needs to be passed to
	% modecheck_var_list_is_live, modecheck_var_has_inst_list, and
	% modecheck_set_var_inst_list.  This offset number is calculated
	% so that real arguments get positive argument numbers and
	% type_info arguments get argument numbers less than or equal to 0.
	%
:- pred compute_arg_offset(pred_info, int).
:- mode compute_arg_offset(in, out) is det.

	% Given a list of variables and a list of expected liveness, ensure
	% that the inst of each variable satisfies the corresponding expected
	% liveness.  If the bool argument is `yes', then require an exact
	% match.
	%
:- pred modecheck_var_list_is_live(list(prog_var), list(is_live), bool, int,
		mode_info, mode_info).
:- mode modecheck_var_list_is_live(in, in, in, in, mode_info_di, mode_info_uo)
	is det.

	% Given a list of variables and a list of initial insts, ensure
	% that the inst of each variable matches the corresponding initial
	% inst.  If the bool argument is `yes', then we require an exact
	% match (using inst_matches_final), otherwise we allow the var
	% to be more instantiated than the inst (using inst_matches_initial).
	%
:- pred modecheck_var_has_inst_list(list(prog_var), list(inst), bool, int,
		inst_var_sub, mode_info, mode_info).
:- mode modecheck_var_has_inst_list(in, in, in, in, out, mode_info_di, mode_info_uo)
	is det.

:- pred modecheck_set_var_inst(prog_var, inst, mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, mode_info_di, mode_info_uo) is det.

:- pred modecheck_set_var_inst_list(list(prog_var), list(inst), list(inst),
		int, list(prog_var), extra_goals, mode_info, mode_info).
:- mode modecheck_set_var_inst_list(in, in, in, in, out, out,
					mode_info_di, mode_info_uo) is det.

	% check that the final insts of the head vars of a lambda
	% goal matches their expected insts
	%
:- pred modecheck_final_insts(list(prog_var), list(inst), mode_info, mode_info).
:- mode modecheck_final_insts(in, in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_add_goals_live_vars(list(hlds_goal), mode_info, mode_info).
:- mode mode_info_add_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

:- pred mode_info_remove_goals_live_vars(list(hlds_goal), mode_info,
					mode_info).
:- mode mode_info_remove_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

	% modecheck_functor_test(ConsId, Var):
	%	update the instmap to reflect the fact that
	%	Var was bound to ConsId. 
	% This is used for the functor tests in `switch' statements.
	%
:- pred modecheck_functor_test(prog_var, cons_id, mode_info, mode_info).
:- mode modecheck_functor_test(in, in, mode_info_di, mode_info_uo) is det.

	% compute_goal_instmap_delta(InstMap0, Goal,
	%	GoalInfo0, GoalInfo, ModeInfo0, ModeInfo).
	%
	% Work out the instmap_delta for a goal from
	% the instmaps before and after the goal.
:- pred compute_goal_instmap_delta(instmap, hlds_goal_expr,
		hlds_goal_info, hlds_goal_info, mode_info, mode_info).
:- mode compute_goal_instmap_delta(in, in, in, out,
		mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%

% The following predicates are used by modecheck_unify.m.

:- pred modecheck_goal(hlds_goal, hlds_goal, mode_info, mode_info).
:- mode modecheck_goal(in, out, mode_info_di, mode_info_uo) is det.

	% Mode-check a single goal-expression.

:- pred modecheck_goal_expr(hlds_goal_expr, hlds_goal_info, hlds_goal_expr,
			mode_info, mode_info).
:- mode modecheck_goal_expr(in, in, out, mode_info_di, mode_info_uo) is det.


:- type extra_goals
	--->	no_extra_goals
	;	extra_goals(
			list(hlds_goal),	% goals to insert before
						% the main goal
			list(hlds_goal)		% goals to append after
						% the main goal
				
		).
:- type after_goals
	--->	no_after_goals
	;	after_goals(
			instmap,		% instmap at end of main goal
			list(hlds_goal)		% goals to append after
						% the main goal
		).

	% append_extra_goals inserts adds some goals to the
	% list of goals to insert before/after the main goal.
	%
:- pred append_extra_goals(extra_goals, extra_goals, extra_goals).
:- mode append_extra_goals(in, in, out) is det.

	% handle_extra_goals combines MainGoal and ExtraGoals into a single
	% hlds_goal_expr, rerunning mode analysis on the entire
	% conjunction if ExtraGoals is not empty.
	%
:- pred handle_extra_goals(hlds_goal_expr, extra_goals,
		hlds_goal_info, list(prog_var), list(prog_var),
		instmap, hlds_goal_expr, mode_info, mode_info).
:- mode handle_extra_goals(in, in, in, in, in, in, out,
		mode_info_di, mode_info_uo) is det.

:- pred mode_context_to_unify_context(mode_context, mode_info, unify_context).
:- mode mode_context_to_unify_context(in, mode_info_ui, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__make_hlds, hlds__hlds_data, check_hlds__unique_modes.
:- import_module check_hlds__mode_debug.
:- import_module check_hlds__mode_info, check_hlds__delay_info.
:- import_module check_hlds__mode_errors, check_hlds__inst_match.
:- import_module hlds__instmap.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module ll_backend__code_util, check_hlds__unify_proc.
:- import_module hlds__special_pred.
:- import_module libs__globals, libs__options, parse_tree__mercury_to_mercury.
:- import_module hlds__hlds_out, int, set.
:- import_module hlds__passes_aux, check_hlds__typecheck.
:- import_module parse_tree__module_qual, check_hlds__clause_to_proc.
:- import_module check_hlds__modecheck_unify, check_hlds__modecheck_call.
:- import_module check_hlds__inst_util, check_hlds__purity.
:- import_module parse_tree__prog_out, term, varset.

:- import_module list, map, string, require, std_util.
:- import_module assoc_list.

%-----------------------------------------------------------------------------%

modecheck(Module0, Module, UnsafeToContinue) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),

	maybe_write_string(Verbose, "% Mode-checking clauses...\n"),
	check_pred_modes(check_modes, may_change_called_proc,
		Module0, Module, UnsafeToContinue),
	maybe_report_stats(Statistics).

%-----------------------------------------------------------------------------%
	
	% Mode-check the code for all the predicates in a module.

check_pred_modes(WhatToCheck, MayChangeCalledProc,
		ModuleInfo0, ModuleInfo, UnsafeToContinue) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	globals__io_lookup_int_option(mode_inference_iteration_limit,
		MaxIterations),
	modecheck_to_fixpoint(PredIds, MaxIterations, WhatToCheck,
		MayChangeCalledProc, ModuleInfo0, ModuleInfo1,
		UnsafeToContinue),
	( { WhatToCheck = check_unique_modes },
		write_mode_inference_messages(PredIds, yes, ModuleInfo1),
		check_eval_methods(ModuleInfo1, ModuleInfo2)
	; { WhatToCheck = check_modes },
		( { UnsafeToContinue = yes } ->
			write_mode_inference_messages(PredIds, no, ModuleInfo1)
		;
			[]
		),
		{ ModuleInfo2 = ModuleInfo1 }
	),
	{ ModuleInfo = ModuleInfo2 }.

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_to_fixpoint(list(pred_id), int, how_to_check_goal,
		may_change_called_proc, module_info, module_info,
		bool, io__state, io__state).
:- mode modecheck_to_fixpoint(in, in, in, in, in, out, out, di, uo) is det.

modecheck_to_fixpoint(PredIds, MaxIterations, WhatToCheck, MayChangeCalledProc,
		ModuleInfo0, ModuleInfo, UnsafeToContinue) -->
	{ ModuleInfo1 = ModuleInfo0 },

	% save the old procedure bodies so that we can restore them for the
	% next pass
	{ module_info_preds(ModuleInfo0, OldPredTable0) },

	% analyze everything which has the "can-process" flag set to `yes'
	modecheck_pred_modes_2(PredIds, WhatToCheck, MayChangeCalledProc,
		ModuleInfo1, ModuleInfo2, no, Changed1, 0, NumErrors),

	% analyze the procedures whose "can-process" flag was no;
	% those procedures were inserted into the unify requests queue.
	modecheck_queued_procs(WhatToCheck, OldPredTable0,
		ModuleInfo2, OldPredTable, ModuleInfo3, Changed2),
	io__get_exit_status(ExitStatus),

	{ bool__or(Changed1, Changed2, Changed) },

	% stop if we have reached a fixpoint or found any errors
	( { Changed = no ; NumErrors > 0 ; ExitStatus \= 0 } ->
		{ ModuleInfo = ModuleInfo3 },
		{ UnsafeToContinue = Changed }
	;
		% stop if we exceed the iteration limit
		( { MaxIterations =< 1 } ->
			report_max_iterations_exceeded,
			{ ModuleInfo = ModuleInfo3 },
			{ UnsafeToContinue = yes }
		;
			globals__io_lookup_bool_option(debug_modes, DebugModes),
			( { DebugModes = yes } ->
				write_mode_inference_messages(PredIds, no,
						ModuleInfo3)
			;
				[]
			),

			%
			% Mode analysis may have modified the procedure
			% bodies, since it does some optimizations such
			% as deleting unreachable code.  But since we didn't
			% reach a fixpoint yet, the mode information is not
			% correct yet, and so those optimizations will have
			% been done based on incomplete information, and so
			% they may therefore produce incorrect results.
			% Thus we need to restore the old procedure bodies.
			%

			( { WhatToCheck = check_modes } ->
				% restore the proc_info goals from the
				% clauses in the pred_info
				{ copy_module_clauses_to_procs(PredIds,
					ModuleInfo3, ModuleInfo4) }
			;
				% restore the proc_info goals from the
				% proc_infos in the old module_info
				{ copy_pred_bodies(OldPredTable, PredIds,
					ModuleInfo3, ModuleInfo4) }
			),

			{ MaxIterations1 is MaxIterations - 1 },
			modecheck_to_fixpoint(PredIds, MaxIterations1,
				WhatToCheck, MayChangeCalledProc,
				ModuleInfo4, ModuleInfo, UnsafeToContinue)
		)
	).

:- pred report_max_iterations_exceeded(io__state, io__state).
:- mode report_max_iterations_exceeded(di, uo) is det.

report_max_iterations_exceeded -->
	io__set_exit_status(1),
	io__write_strings([
	   "Mode analysis iteration limit exceeded.\n",
	   "You may need to declare the modes explicitly, or use the\n",
	   "`--mode-inference-iteration-limit' option to increase the limit.\n"
	]),
	globals__io_lookup_int_option(mode_inference_iteration_limit,
		MaxIterations),
	io__format("(The current limit is %d iterations.)\n",
		[i(MaxIterations)]).

% copy_pred_bodies(OldPredTable, ProcId, ModuleInfo0, ModuleInfo):
%	copy the procedure bodies for all procedures of the specified
%	PredIds from OldPredTable into ModuleInfo0, giving ModuleInfo.
:- pred copy_pred_bodies(pred_table, list(pred_id), module_info, module_info).
:- mode copy_pred_bodies(in, in, in, out) is det.
copy_pred_bodies(OldPredTable, PredIds, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	list__foldl(copy_pred_body(OldPredTable), PredIds,
		PredTable0, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo).

% copy_pred_body(OldPredTable, ProcId, PredTable0, PredTable):
%	copy the procedure bodies for all procedures of the specified
%	PredId from OldPredTable into PredTable0, giving PredTable.
:- pred copy_pred_body(pred_table, pred_id, pred_table, pred_table).
:- mode copy_pred_body(in, in, in, out) is det.
copy_pred_body(OldPredTable, PredId, PredTable0, PredTable) :-
	map__lookup(PredTable0, PredId, PredInfo0),
	(
		% don't copy type class methods, because their
		% proc_infos are generated already mode-correct,
		% and because copying from the clauses_info doesn't
		% work for them.
		pred_info_get_markers(PredInfo0, Markers),
		check_marker(Markers, class_method)
	->
		PredTable = PredTable0
	;
		pred_info_procedures(PredInfo0, ProcTable0),
		map__lookup(OldPredTable, PredId, OldPredInfo),
		pred_info_procedures(OldPredInfo, OldProcTable),
		map__keys(OldProcTable, OldProcIds),
		list__foldl(copy_proc_body(OldProcTable), OldProcIds,
			ProcTable0, ProcTable),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__set(PredTable0, PredId, PredInfo, PredTable)
	).

% copy_proc_body(OldProcTable, ProcId, ProcTable0, ProcTable):
%	copy the body of the specified ProcId from OldProcTable
%	into ProcTable0, giving ProcTable.
:- pred copy_proc_body(proc_table, proc_id, proc_table, proc_table).
:- mode copy_proc_body(in, in, in, out) is det.
copy_proc_body(OldProcTable, ProcId, ProcTable0, ProcTable) :-
	map__lookup(OldProcTable, ProcId, OldProcInfo),
	proc_info_goal(OldProcInfo, OldProcBody),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_set_goal(ProcInfo0, OldProcBody, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable).

:- pred modecheck_pred_modes_2(list(pred_id), how_to_check_goal,
		may_change_called_proc, module_info, module_info,
		bool, bool, int, int, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, in, in, out, in, out, in, out, di, uo)
			is det.

modecheck_pred_modes_2([], _, _, ModuleInfo, ModuleInfo, Changed, Changed,
		NumErrors, NumErrors) --> [].
modecheck_pred_modes_2([PredId | PredIds], WhatToCheck, MayChangeCalledProc,
		ModuleInfo0, ModuleInfo, Changed0, Changed,
		NumErrors0, NumErrors) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	(
		(
			%
			% don't modecheck imported predicates
			%
			( { pred_info_is_imported(PredInfo0) }
			; { pred_info_is_pseudo_imported(PredInfo0) }
			)
		;
			%
			% don't modecheck class methods, because they
			% are generated already mode-correct and with
			% correct instmap deltas.
			%
			{ pred_info_get_markers(PredInfo0, PredMarkers) },
			{ check_marker(PredMarkers, class_method) }
		)
	->
		{ ModuleInfo3 = ModuleInfo0 },
		{ Changed1 = Changed0 },
		{ NumErrors1 = NumErrors0 }
	;
		write_modes_progress_message(PredId, PredInfo0, ModuleInfo0,
			WhatToCheck),
		modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck,
			MayChangeCalledProc, ModuleInfo0, ModuleInfo1,
			Changed0, Changed1, ErrsInThisPred),
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
	modecheck_pred_modes_2(PredIds, WhatToCheck, MayChangeCalledProc,
		ModuleInfo3, ModuleInfo, Changed1, Changed,
		NumErrors1, NumErrors).

:- pred write_modes_progress_message(pred_id, pred_info, module_info,
			how_to_check_goal, io__state, io__state).
:- mode write_modes_progress_message(in, in, in, in, di, uo) is det.
	
write_modes_progress_message(PredId, PredInfo, ModuleInfo, WhatToCheck) -->
	{ pred_info_get_markers(PredInfo, Markers) },
	( { check_marker(Markers, infer_modes) } ->
		(	{ WhatToCheck = check_modes },
			write_pred_progress_message("% Mode-analysing ",
				PredId, ModuleInfo)
		;	{ WhatToCheck = check_unique_modes },
			write_pred_progress_message("% Unique-mode-analysing ",
				PredId, ModuleInfo)
		)
	;
		(	{ WhatToCheck = check_modes },
			write_pred_progress_message("% Mode-checking ",
				PredId, ModuleInfo)
		;	{ WhatToCheck = check_unique_modes },
			write_pred_progress_message("% Unique-mode-checking ",
				PredId, ModuleInfo)
		)
	).

%-----------------------------------------------------------------------------%

	% Mode-check the code for single predicate.

modecheck_pred_mode(PredId, PredInfo0, WhatToCheck, MayChangeCalledProc,
		ModuleInfo0, ModuleInfo, NumErrors) -->
	modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck,
		MayChangeCalledProc, ModuleInfo0, ModuleInfo,
		no, _Changed, NumErrors).

:- pred modecheck_pred_mode_2(pred_id, pred_info, how_to_check_goal,
		may_change_called_proc, module_info, module_info,
		bool, bool, int, io__state, io__state).
:- mode modecheck_pred_mode_2(in, in, in, in, in,
		out, in, out, out, di, uo) is det.

modecheck_pred_mode_2(PredId, PredInfo0, WhatToCheck, MayChangeCalledProc,
		ModuleInfo0, ModuleInfo, Changed0, Changed, NumErrors) -->
	% Note that we use pred_info_procids rather than
	% pred_info_all_procids here, which means that we
	% don't process modes that have already been inferred
	% as invalid.
	{ pred_info_procids(PredInfo0, ProcIds) },
	( { WhatToCheck = check_modes } ->
		(
			{ ProcIds = [] }
		->
			maybe_report_error_no_modes(PredId, PredInfo0,
					ModuleInfo0)
		;
			[]
		)
	;
		[]
	),
	modecheck_procs(ProcIds, PredId, WhatToCheck, MayChangeCalledProc,
				ModuleInfo0, Changed0, 0,
				ModuleInfo, Changed, NumErrors).

	% Iterate over the list of modes for a predicate.

:- pred modecheck_procs(list(proc_id), pred_id, how_to_check_goal,
		may_change_called_proc, module_info, bool, int,
		module_info, bool, int, io__state, io__state).
:- mode modecheck_procs(in, in, in, in, in, in, in,
		out, out, out, di, uo) is det.

modecheck_procs([], _PredId, _, _, ModuleInfo, Changed, Errs,
				ModuleInfo, Changed, Errs) --> [].
modecheck_procs([ProcId|ProcIds], PredId, WhatToCheck, MayChangeCalledProc,
				ModuleInfo0, Changed0, Errs0,
				ModuleInfo, Changed, Errs) -->
	% mode-check that mode of the predicate
	modecheck_proc_2(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
				ModuleInfo0, Changed0,
				ModuleInfo1, Changed1, NumErrors),
	{ Errs1 is Errs0 + NumErrors },
		% recursively process the remaining modes
	modecheck_procs(ProcIds, PredId, WhatToCheck, MayChangeCalledProc,
				ModuleInfo1, Changed1, Errs1,
				ModuleInfo, Changed, Errs).

%-----------------------------------------------------------------------------%

	% Mode-check the code for predicate in a given mode.

modecheck_proc(ProcId, PredId, ModuleInfo0, ModuleInfo, NumErrors, Changed) -->
	modecheck_proc(ProcId, PredId, check_modes, may_change_called_proc,
		ModuleInfo0, ModuleInfo, NumErrors, Changed).

modecheck_proc(ProcId, PredId, WhatToCheck, MayChangeCalledProc, ModuleInfo0,
			ModuleInfo, NumErrors, Changed) -->
	modecheck_proc_2(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
			ModuleInfo0, no, ModuleInfo, Changed, NumErrors).

:- pred modecheck_proc_2(proc_id, pred_id, how_to_check_goal,
		may_change_called_proc, module_info, bool,
		module_info, bool, int, io__state, io__state).
:- mode modecheck_proc_2(in, in, in, in, in, in, out, out, out, di, uo) is det.

modecheck_proc_2(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
		ModuleInfo0, Changed0, ModuleInfo, Changed, NumErrors) -->
		% get the proc_info from the module_info
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
					_PredInfo0, ProcInfo0) },
	( { proc_info_can_process(ProcInfo0, no) } ->
		{ ModuleInfo = ModuleInfo0 },
		{ Changed = Changed0 },
		{ NumErrors = 0 }
	;
			% modecheck it
		modecheck_proc_3(ProcId, PredId, WhatToCheck,
			MayChangeCalledProc, ModuleInfo0, ProcInfo0, Changed0,
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
	{ WhatToCheck = check_modes },
	modecheck_proc_3(ProcId, PredId, WhatToCheck, may_change_called_proc,
			ModuleInfo0, ProcInfo0, no,
			ModuleInfo, ProcInfo, _Changed, NumErrors).

:- pred modecheck_proc_3(proc_id, pred_id, how_to_check_goal,
		may_change_called_proc, module_info, proc_info, bool,
		module_info, proc_info, bool, int, io__state, io__state).
:- mode modecheck_proc_3(in, in, in, in, in, in, in,
		out, out, out, out, di, uo) is det.

modecheck_proc_3(ProcId, PredId, WhatToCheck, MayChangeCalledProc,
			ModuleInfo0, ProcInfo0, Changed0,
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
	clauses_info_clauses(ClausesInfo, ClauseList),
	( ClauseList = [FirstClause | _] ->
		FirstClause = clause(_, _, _, Context)
	;
		proc_info_context(ProcInfo0, Context)
	),

	%
	% modecheck the clause - first set the initial instantiation
	% of the head arguments, mode-check the body, and
	% then check that the final instantiation matches that in
	% the mode declaration
	%

		% construct the initial instmap
	mode_list_get_initial_insts(ArgModes0, ModuleInfo0, ArgInitialInsts),
	assoc_list__from_corresponding_lists(HeadVars, ArgInitialInsts, InstAL),
	instmap__from_assoc_list(InstAL, InstMap0),

		% construct the initial set of live vars:
		% initially, only the non-clobbered head variables are live
	get_live_vars(HeadVars, ArgLives0, LiveVarsList),
	set__list_to_set(LiveVarsList, LiveVars),

		% initialize the mode info
	mode_info_init(IOState0, ModuleInfo0, PredId, ProcId, Context,
		LiveVars, InstMap0, WhatToCheck,
		MayChangeCalledProc, ModeInfo0),
	mode_info_set_changed_flag(Changed0, ModeInfo0, ModeInfo1),

		% modecheck the procedure body
	( WhatToCheck = check_unique_modes ->
		unique_modes__check_goal(Body0, Body, ModeInfo1, ModeInfo2)
	;
		modecheck_goal(Body0, Body, ModeInfo1, ModeInfo2)
	),

		% check that final insts match those specified in the
		% mode declaration
	mode_list_get_final_insts(ArgModes0, ModuleInfo0, ArgFinalInsts0),
	pred_info_get_markers(PredInfo, Markers),
	( check_marker(Markers, infer_modes) ->
		InferModes = yes
	;
		InferModes = no
	),
	modecheck_final_insts_2(HeadVars, ArgFinalInsts0, ModeInfo2,
			InferModes, ArgFinalInsts, ModeInfo3),

	( InferModes = yes ->
		% For inferred predicates, we don't report the
		% error(s) here; instead we just save them in the
		% proc_info, thus marking that procedure as invalid.
		ModeInfo = ModeInfo3,
		% This is sometimes handy for debugging:
		% report_mode_errors(ModeInfo3, ModeInfo),
		mode_info_get_errors(ModeInfo, ModeErrors),
		ProcInfo1 = ProcInfo0 ^ mode_errors := ModeErrors,
		NumErrors = 0
	;
		% report any errors we found
		report_mode_errors(ModeInfo3, ModeInfo),
		mode_info_get_num_errors(ModeInfo, NumErrors),
		ProcInfo1 = ProcInfo0
	),
	% save away the results
	inst_lists_to_mode_list(ArgInitialInsts, ArgFinalInsts, ArgModes),
	mode_info_get_changed_flag(ModeInfo, Changed),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_io_state(ModeInfo, IOState),
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	proc_info_set_goal(ProcInfo1, Body, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo4),
	proc_info_set_argmodes(ProcInfo4, ArgModes, ProcInfo).

	% modecheck_final_insts for a lambda expression
modecheck_final_insts(HeadVars, ArgFinalInsts, ModeInfo0, ModeInfo) :-
		% for lambda expressions, modes must always be
		% declared, we never infer them.
	InferModes = no,
	modecheck_final_insts_2(HeadVars, ArgFinalInsts, ModeInfo0,
			InferModes, _NewFinalInsts, ModeInfo).

:- pred modecheck_final_insts_2(list(prog_var), list(inst), mode_info, bool,
					list(inst), mode_info).
:- mode modecheck_final_insts_2(in, in, mode_info_di, in,
					out, mode_info_uo) is det.

	% check that the final insts of the head vars matches their
	% expected insts
	%
modecheck_final_insts_2(HeadVars, FinalInsts0, ModeInfo0, InferModes,
			FinalInsts, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_errors(ModeInfo0, Errors),
	% If there were any mode errors, use an unreachable instmap.
	% This ensures that we don't get unwanted flow-on errors.
	% This is not strictly necessary, since we only report the
	% first mode error anyway, and the resulting FinalInsts
	% will not be used; but it improves the readability of the
	% rejected modes.
	( Errors \= [] ->
		% If there were any mode errors, something must have
		% changed, since if the procedure had mode errors
		% in a previous pass then it wouldn't have been
		% processed at all in this pass.
		Changed0 = yes,
		instmap__init_unreachable(InstMap)
	;
		Changed0 = no,
		mode_info_get_instmap(ModeInfo0, InstMap)
	),
	mode_info_get_var_types(ModeInfo0, VarTypes),
	instmap__lookup_vars(HeadVars, InstMap, VarFinalInsts1),
	map__apply_to_list(HeadVars, VarTypes, ArgTypes),

	( InferModes = yes ->
		normalise_insts(VarFinalInsts1, ArgTypes, ModuleInfo,
			VarFinalInsts2),
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
		mode_info_get_changed_flag(ModeInfo1, Changed2),
		bool__or_list([Changed0, Changed1, Changed2], Changed),
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
		Inst = ground(clobbered, none)
	;
		Inst = Inst0
	),
	maybe_clobber_insts(Insts0, IsLives, Insts).

:- pred check_final_insts(list(prog_var), list(inst), list(inst), bool, int,
				module_info, bool, bool, mode_info, mode_info).
:- mode check_final_insts(in, in, in, in, in, in, in, out, mode_info_di,
				mode_info_uo) is det.

check_final_insts(Vars, Insts, VarInsts, InferModes, ArgNum, ModuleInfo,
		Changed0, Changed) -->
	( { Vars = [], Insts = [], VarInsts = [] } ->
		{ Changed = Changed0 }
	; { Vars = [Var|Vars1], Insts = [Inst|Insts1],
	    VarInsts = [VarInst|VarInsts1] } ->
		=(ModeInfo),
		{ mode_info_get_var_types(ModeInfo, VarTypes) },
		{ map__lookup(VarTypes, Var, Type) },
		( { inst_matches_final(VarInst, Inst, Type, ModuleInfo) } ->
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
					    Type, ModuleInfo) } ->
					{ Reason = too_instantiated }
				; { inst_matches_initial(Inst, VarInst,
					    Type, ModuleInfo) } ->
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

	modecheck_goal_expr(Goal0, GoalInfo0, Goal, ModeInfo1, ModeInfo2),

	compute_goal_instmap_delta(InstMap0, Goal, GoalInfo0, GoalInfo,
		ModeInfo2, ModeInfo).

compute_goal_instmap_delta(InstMap0, Goal,
		GoalInfo0, GoalInfo, ModeInfo0, ModeInfo) :-
	( Goal = conj([]) ->
		%
		% When modecheck_unify.m replaces a unification with a
		% dead variable with `true', make sure the instmap_delta
		% of the goal is empty. The code generator and
		% mode_util__recompute_instmap_delta can be confused
		% by references to the dead variable in the instmap_delta,
		% resulting in calls to error/1.
		%
		instmap_delta_init_reachable(DeltaInstMap),
		mode_info_set_instmap(InstMap0, ModeInfo0, ModeInfo)
	;
		ModeInfo = ModeInfo0,
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		mode_info_get_instmap(ModeInfo, InstMap),
		compute_instmap_delta(InstMap0, InstMap,
			NonLocals, DeltaInstMap)
	),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).

modecheck_goal_expr(conj(List0), GoalInfo0, Goal) -->
	mode_checkpoint(enter, "conj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ Goal = conj([]) }
	;
		modecheck_conj_list(List0, List),
		{ conj_list_to_goal(List, GoalInfo0, Goal - _GoalInfo) }
	),
	mode_checkpoint(exit, "conj").

	% To modecheck a parallel conjunction, we modecheck each
	% conjunct independently (just like for disjunctions).
	% To make sure that we don't try to bind a variable more than
	% once (by binding it in more than one conjunct), we maintain a
	% datastructure that keeps track of three things:
	%	the set of variables that are nonlocal to the conjuncts
	%	(which may be a superset of the nonlocals of the par_conj
	%	as a whole);
	%	the set of nonlocal variables that have been bound in the
	%	current conjunct; and
	%	the set of variables that were bound in previous conjuncts.
	% When binding a variable, we check that it wasn't in the set of
	% variables bound in other conjuncts, and we add it to the set of
	% variables bound in this conjunct.
	% At the end of the conjunct, we add the set of variables bound in
	% this conjunct to the set of variables bound in previous conjuncts
	% and set the set of variables bound in the current conjunct to
	% empty.
	% A stack of these structures is maintained to handle nested parallel
	% conjunctions properly.
modecheck_goal_expr(par_conj(List0), GoalInfo0, par_conj(List)) -->
	mode_checkpoint(enter, "par_conj"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	modecheck_par_conj_list(List0, List, NonLocals, InstMapNonlocalList),
	instmap__unify(NonLocals, InstMapNonlocalList),
	mode_checkpoint(exit, "par_conj").

modecheck_goal_expr(disj(List0), GoalInfo0, Goal) -->
	mode_checkpoint(enter, "disj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ Goal = disj(List0) },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		modecheck_disj_list(List0, List, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj),
		{ disj_list_to_goal(List, GoalInfo0, Goal - _GoalInfo) }
	),
	mode_checkpoint(exit, "disj").

modecheck_goal_expr(if_then_else(Vs, A0, B0, C0), GoalInfo0, Goal) -->
	mode_checkpoint(enter, "if-then-else"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	{ goal_get_nonlocals(B0, B_Vars) },
	mode_info_dcg_get_instmap(InstMap0),
	%
	% We need to lock the non-local variables, to ensure
	% that the condition of the if-then-else does not bind them.
	%
	mode_info_lock_vars(if_then_else, NonLocals),
	mode_info_add_live_vars(B_Vars),
	modecheck_goal(A0, A),
	mode_info_dcg_get_instmap(InstMapA),
	mode_info_remove_live_vars(B_Vars),
	mode_info_unlock_vars(if_then_else, NonLocals),
	( { instmap__is_reachable(InstMapA) } ->
		modecheck_goal(B0, B),
		mode_info_dcg_get_instmap(InstMapB)
	;
		% We should not mode-analyse the goal, since it is unreachable.
		% Instead we optimize the goal away, so that later passes
		% won't complain about it not having mode information.
		{ true_goal(B) },
		{ InstMapB = InstMapA }
	),
	mode_info_set_instmap(InstMap0),
	modecheck_goal(C0, C),
	mode_info_dcg_get_instmap(InstMapC),
	mode_info_set_instmap(InstMap0),
	instmap__merge(NonLocals, [InstMapB, InstMapC], if_then_else),
	{ Goal = if_then_else(Vs, A, B, C) },
	mode_checkpoint(exit, "if-then-else").

modecheck_goal_expr(not(A0), GoalInfo0, not(A)) -->
	mode_checkpoint(enter, "not"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	mode_info_dcg_get_instmap(InstMap0),
	%
	% when analyzing a negated goal, nothing is forward-live
	% (live on forward executution after that goal), because
	% if the goal succeeds then execution will immediately backtrack.
	% So we need to set the live variables set to empty here.
	% This allows those variables to be backtrackably
	% destructively updated.  (If you try to do non-backtrackable
	% destructive update on such a variable, it will be caught
	% later on by unique_modes.m.)
	%
	=(ModeInfo),
	{ mode_info_get_live_vars(ModeInfo, LiveVars0) },
	mode_info_set_live_vars([]),
	%
	% We need to lock the non-local variables, to ensure
	% that the negation does not bind them.
	%
	mode_info_lock_vars(negation, NonLocals),
	modecheck_goal(A0, A),
	mode_info_set_live_vars(LiveVars0),
	mode_info_unlock_vars(negation, NonLocals),
	mode_info_set_instmap(InstMap0),
	mode_checkpoint(exit, "not").

modecheck_goal_expr(some(Vs, CanRemove, G0), _, some(Vs, CanRemove, G)) -->
	mode_checkpoint(enter, "some"),
	modecheck_goal(G0, G),
	mode_checkpoint(exit, "some").

modecheck_goal_expr(call(PredId, ProcId0, Args0, _, Context, PredName),
		GoalInfo0, Goal) -->
	{ prog_out__sym_name_to_string(PredName, PredNameString) },
	{ string__append("call ", PredNameString, CallString) },
	mode_checkpoint(enter, CallString),

	=(ModeInfo0),
	{ mode_info_get_call_id(ModeInfo0, PredId, CallId) },
	mode_info_set_call_context(call(call(CallId))),

	=(ModeInfo1),
	{ mode_info_get_instmap(ModeInfo1, InstMap0) },
	{ DeterminismKnown = no },
	modecheck_call_pred(PredId, ProcId0, Args0, DeterminismKnown,
				Mode, Args, ExtraGoals),

	=(ModeInfo),
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ code_util__builtin_state(ModuleInfo, PredId, Mode, Builtin) },
	{ Call = call(PredId, Mode, Args, Builtin, Context, PredName) },
	handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
				InstMap0, Goal),

	mode_info_unset_call_context,
	mode_checkpoint(exit, CallString).

modecheck_goal_expr(generic_call(GenericCall, Args0, Modes0, _),
		GoalInfo0, Goal) -->
	mode_checkpoint(enter, "generic_call"),
	mode_info_dcg_get_instmap(InstMap0),

	{ hlds_goal__generic_call_id(GenericCall, CallId) },
	mode_info_set_call_context(call(CallId)),
	(
		{ GenericCall = higher_order(PredVar, PredOrFunc, _) },
		modecheck_higher_order_call(PredOrFunc, PredVar,
			Args0, Modes, Det, Args, ExtraGoals),
		{ AllArgs0 = [PredVar | Args0] },
		{ AllArgs = [PredVar | Args] }
	;
		% Class method calls are added by polymorphism.m.
		% XXX We should probably fill this in so that
		% rerunning mode analysis works on code with typeclasses.
		{ GenericCall = class_method(_, _, _, _) },
		{ error("modecheck_goal_expr: class_method_call") }
	;
		{ GenericCall = aditi_builtin(AditiBuiltin, UpdatedCallId) },
		modecheck_aditi_builtin(AditiBuiltin, UpdatedCallId,
			Args0, Modes0, Det, Args, ExtraGoals),
		{ Modes = Modes0 },
		{ AllArgs0 = Args0 },
		{ AllArgs = Args }
	),

	{ Goal1 = generic_call(GenericCall, Args, Modes, Det) },
	handle_extra_goals(Goal1, ExtraGoals, GoalInfo0, AllArgs0, AllArgs,
		InstMap0, Goal),
		
	mode_info_unset_call_context,
	mode_checkpoint(exit, "generic_call").

modecheck_goal_expr(unify(A0, B0, _, UnifyInfo0, UnifyContext), GoalInfo0, Goal)
		-->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),
	modecheck_unification(A0, B0, UnifyInfo0, UnifyContext, GoalInfo0,
		Goal),
	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

modecheck_goal_expr(switch(Var, CanFail, Cases0), GoalInfo0,
		switch(Var, CanFail, Cases)) -->
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
modecheck_goal_expr(foreign_proc(Attributes, PredId, ProcId0,
		Args0, ArgNameMap, OrigArgTypes, PragmaCode),
		GoalInfo, Goal) -->
	mode_checkpoint(enter, "pragma_foreign_code"),

	=(ModeInfo0),
	{ mode_info_get_call_id(ModeInfo0, PredId, CallId) },

	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	{ DeterminismKnown = no },
	mode_info_set_call_context(call(call(CallId))),
	modecheck_call_pred(PredId, ProcId0, Args0, DeterminismKnown,
				ProcId, Args, ExtraGoals),

	{ Pragma = foreign_proc(Attributes, PredId, ProcId,
			Args0, ArgNameMap, OrigArgTypes, PragmaCode) },
	handle_extra_goals(Pragma, ExtraGoals, GoalInfo, Args0, Args,
			InstMap0, Goal),

	mode_info_unset_call_context,
	mode_checkpoint(exit, "pragma_foreign_code").

modecheck_goal_expr(shorthand(_), _, _) -->
	% these should have been expanded out by now
	{ error("modecheck_goal_expr: unexpected shorthand") }.

append_extra_goals(no_extra_goals, ExtraGoals, ExtraGoals).
append_extra_goals(extra_goals(BeforeGoals, AfterGoals),
		no_extra_goals, extra_goals(BeforeGoals, AfterGoals)).
append_extra_goals(extra_goals(BeforeGoals0, AfterGoals0),
			extra_goals(BeforeGoals1, AfterGoals1),
			extra_goals(BeforeGoals, AfterGoals)) :-
	list__append(BeforeGoals0, BeforeGoals1, BeforeGoals),
	list__append(AfterGoals0, AfterGoals1, AfterGoals).

handle_extra_goals(MainGoal, no_extra_goals, _GoalInfo0, _Args0, _Args,
		_InstMap0, MainGoal, ModeInfo, ModeInfo).
handle_extra_goals(MainGoal, extra_goals(BeforeGoals0, AfterGoals0),
		GoalInfo0, Args0, Args, InstMap0, Goal, ModeInfo0, ModeInfo) :-
	mode_info_get_errors(ModeInfo0, Errors),
	(
		% There's no point adding extra goals if the code is
		% unreachable anyway.
		instmap__is_reachable(InstMap0),

		% If we recorded errors processing the goal, it will
		% have to be reprocessed anyway, so don't add the extra
		% goals now.
		Errors = []
	->
		%
		% We need to be careful to update the delta-instmaps
		% correctly, using the appropriate instmaps:
		%
		%		% InstMapAtStart is here
		%	 BeforeGoals,
		%		% we don't know the instmap here,
		%		% but as it happens we don't need it
		%	 main goal,
		%		% InstMapAfterMain is here
		%	 AfterGoals
		%		% InstMapAtEnd (from the ModeInfo) is here
		%

		% recompute the new set of non-local variables for the main goal
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__list_to_set(Args0, OldArgVars),
		set__list_to_set(Args, NewArgVars),
		set__difference(NewArgVars, OldArgVars, IntroducedVars),
		set__union(NonLocals0, IntroducedVars, OutsideVars),
		set__intersect(OutsideVars, NewArgVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),

		% combine the main goal and the extra goals into a conjunction
		Goal0 = MainGoal - GoalInfo,
		goal_info_get_context(GoalInfo0, Context),
		handle_extra_goals_contexts(BeforeGoals0, Context, BeforeGoals),
		handle_extra_goals_contexts(AfterGoals0, Context, AfterGoals),
		list__append(BeforeGoals, [Goal0 | AfterGoals], GoalList0),

		mode_info_get_may_change_called_proc(ModeInfo0,
			MayChangeCalledProc0),

		% Make sure we don't go into an infinite loop if
		% there is a bug in the code to add extra goals.
		mode_info_set_checking_extra_goals(yes, ModeInfo0, ModeInfo1),

		% We've already worked out which procedure should be called,
		% we don't need to do it again.
		mode_info_set_may_change_called_proc(
			may_not_change_called_proc, ModeInfo1, ModeInfo2),

		mode_info_set_instmap(InstMap0, ModeInfo2, ModeInfo3),

		% Recheck the goals to compute the instmap_deltas.
		%
		% This can fail even if the original check on the goal
		% succeeded in the case of a unification procedure which
		% binds a partially instantiated variable, because adding
		% the extra goals can make the partially instantiated
		% variables `live' after the main goal.
		% The other thing to beware of in this case is that delaying
		% must be disabled while processing the extra goals. If it
		% is not, the main unification will be delayed until after the
		% argument unifications, which turns them into assignments,
		% and we end up repeating the process forever.
		mode_info_add_goals_live_vars(GoalList0, ModeInfo3, ModeInfo4),
		modecheck_conj_list_no_delay(GoalList0, GoalList,
			ModeInfo4, ModeInfo5),
		Goal = conj(GoalList),
		mode_info_set_checking_extra_goals(no, ModeInfo5, ModeInfo6),
		mode_info_set_may_change_called_proc(MayChangeCalledProc0,
			ModeInfo6, ModeInfo)
	;
		Goal = MainGoal,
		ModeInfo = ModeInfo0
	).

:- pred handle_extra_goals_contexts(list(hlds_goal), prog_context,
	list(hlds_goal)).
:- mode handle_extra_goals_contexts(in, in, out) is det.

handle_extra_goals_contexts([], _Context, []).
handle_extra_goals_contexts([Goal0 | Goals0], Context, [Goal | Goals]) :-
	Goal0 = Expr - GoalInfo0,
	Goal  = Expr - GoalInfo,
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	handle_extra_goals_contexts(Goals0, Context, Goals).

%-----------------------------------------------------------------------------%

	% Modecheck a conjunction without doing any reordering.
	% This is used by handle_extra_goals above.
:- pred modecheck_conj_list_no_delay(list(hlds_goal), list(hlds_goal),
				mode_info, mode_info).
:- mode modecheck_conj_list_no_delay(in, out,
				mode_info_di, mode_info_uo) is det.

modecheck_conj_list_no_delay([], []) --> [].
modecheck_conj_list_no_delay([Goal0 | Goals0], [Goal | Goals]) -->
	{ goal_get_nonlocals(Goal0, NonLocals) },
	mode_info_remove_live_vars(NonLocals),
	modecheck_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	( { instmap__is_unreachable(InstMap) } ->
		% We should not mode-analyse the remaining goals, since they
		% are unreachable.  Instead we optimize them away, so that
		% later passes won't complain about them not having mode
		% information.
		mode_info_remove_goals_live_vars(Goals0),
		{ Goals  = [] }
	;
		modecheck_conj_list_no_delay(Goals0, Goals)
	).

%-----------------------------------------------------------------------------%

:- pred modecheck_conj_list(list(hlds_goal), list(hlds_goal),
				mode_info, mode_info).
:- mode modecheck_conj_list(in, out, mode_info_di, mode_info_uo) is det.

modecheck_conj_list(Goals0, Goals) -->
	=(ModeInfo0),
	{ mode_info_get_errors(ModeInfo0, OldErrors) },
	mode_info_set_errors([]),

	=(ModeInfo1),
	{ mode_info_get_live_vars(ModeInfo1, LiveVars1) },
	{ mode_info_get_delay_info(ModeInfo1, DelayInfo1) },
	{ delay_info__enter_conj(DelayInfo1, DelayInfo2) },
	mode_info_set_delay_info(DelayInfo2),
	mode_info_add_goals_live_vars(Goals0),

	modecheck_conj_list_2(Goals0, [], Goals, RevImpurityErrors),

	=(ModeInfo3),
	{ mode_info_get_errors(ModeInfo3, NewErrors) },
	{ list__append(OldErrors, NewErrors, Errors) },
	mode_info_set_errors(Errors),

	=(ModeInfo4),
	{ mode_info_get_delay_info(ModeInfo4, DelayInfo4) },
	{ delay_info__leave_conj(DelayInfo4, DelayedGoals, DelayInfo5) },
	mode_info_set_delay_info(DelayInfo5),

	( { DelayedGoals \= [] } ->
		% the variables in the delayed goals should not longer
		% be considered live (the conjunction itself will
		% delay, and its nonlocals will be made live)
		mode_info_set_live_vars(LiveVars1)
	;
		[]
	),
	% we only report impurity errors if there were no other errors
	( { DelayedGoals = [] } ->
		%
		% report all the impurity errors
		% (making sure we report the errors in the correct order)
		%
		{ list__reverse(RevImpurityErrors, ImpurityErrors) },
		=(ModeInfo5),
		{ mode_info_get_errors(ModeInfo5, Errors5) },
		{ list__append(Errors5, ImpurityErrors, Errors6) },
		mode_info_set_errors(Errors6)
	; { DelayedGoals = [delayed_goal(_DVars, Error, _DGoal)] } ->
		mode_info_add_error(Error)
	;
		{ get_all_waiting_vars(DelayedGoals, Vars) },
		mode_info_error(Vars,
			mode_error_conj(DelayedGoals, conj_floundered))
	).

mode_info_add_goals_live_vars([]) --> [].
mode_info_add_goals_live_vars([Goal | Goals]) -->
	% We add the live vars for the goals in the goal list
	% in reverse order, because this ensures that in the
	% common case (where there is no delaying), when we come
	% to remove the live vars for the first goal
	% they will have been added last and will thus be
	% at the start of the list of live vars sets, which
	% makes them cheaper to remove.
	mode_info_add_goals_live_vars(Goals),
	{ goal_get_nonlocals(Goal, NonLocals) },
	mode_info_add_live_vars(NonLocals).

mode_info_remove_goals_live_vars([]) --> [].
mode_info_remove_goals_live_vars([Goal | Goals]) -->
	{ goal_get_nonlocals(Goal, NonLocals) },
	mode_info_remove_live_vars(NonLocals),
	mode_info_remove_goals_live_vars(Goals).

:- type impurity_errors == list(mode_error_info).

:- pred modecheck_conj_list_2(list(hlds_goal), impurity_errors,
			list(hlds_goal), impurity_errors,
			mode_info, mode_info).
:- mode modecheck_conj_list_2(in, in, out, out, mode_info_di, mode_info_uo)
	is det.

	% Schedule a conjunction.
	% If it's empty, then there is nothing to do.
	% For non-empty conjunctions, we attempt to schedule the first
	% goal in the conjunction.  If successful, we wakeup a newly
	% pending goal (if any), and if not, we delay the goal.  Then we
	% continue attempting to schedule all the rest of the goals.

modecheck_conj_list_2([], ImpurityErrors, [], ImpurityErrors) --> [].
modecheck_conj_list_2([Goal0 | Goals0], ImpurityErrors0,
		Goals, ImpurityErrors) -->

	{ Goal0 = _GoalExpr - GoalInfo0 },
	( { goal_info_is_impure(GoalInfo0) } ->
		{ Impure = yes },
		check_for_impurity_error(Goal0, ImpurityErrors0,
					 ImpurityErrors1)
	;
		{ Impure = no },
		{ ImpurityErrors1 = ImpurityErrors0 }
	),

		% Hang onto the original instmap, delay_info, and live_vars
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
		% restore the original instmap, delay_info and livevars
		% here, and delay the goal.
	=(ModeInfo1),
	{ mode_info_get_errors(ModeInfo1, Errors) },
	(   { Errors = [ FirstErrorInfo | _] } ->
		mode_info_set_errors([]),
		mode_info_set_instmap(InstMap0),
		mode_info_add_live_vars(NonLocalVars),
		{ delay_info__delay_goal(DelayInfo0, FirstErrorInfo,
					 Goal0, DelayInfo1) },
		%  delaying an impure goal is an impurity error
		( { Impure = yes } ->
			{ FirstErrorInfo = mode_error_info(Vars, _, _, _) },
			{ ImpureError = mode_error_conj(
				[delayed_goal(Vars, FirstErrorInfo, Goal0)],
				goal_itself_was_impure) },
			=(ModeInfo2),
			{ mode_info_get_context(ModeInfo2, Context) },
			{ mode_info_get_mode_context(ModeInfo2, ModeContext) },
			{ ImpureErrorInfo = mode_error_info( Vars, ImpureError,
						Context, ModeContext) },
			{ ImpurityErrors2 = [ImpureErrorInfo |
						ImpurityErrors1] }
		;   
			{ ImpurityErrors2 = ImpurityErrors1 }
		)
	;   
		{ mode_info_get_delay_info(ModeInfo1, DelayInfo1) },
		{ ImpurityErrors2 = ImpurityErrors1 }
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
		% We should not mode-analyse the remaining goals, since they
		% are unreachable.  Instead we optimize them away, so that
		% later passes won't complain about them not having mode
		% information.
		mode_info_remove_goals_live_vars(Goals1),
		{ Goals2  = [] },
		{ ImpurityErrors = ImpurityErrors2 }
	;
		modecheck_conj_list_2(Goals1, ImpurityErrors2,
				      Goals2, ImpurityErrors)
	),

	( { Errors = [] } ->
		% we successfully scheduled this goal, so insert
		% it in the list of successfully scheduled goals
		{ Goals = [Goal | Goals2] }
	;
		% we delayed this goal -- it will be stored in the delay_info
		{ Goals = Goals2 }
	).

%  check whether there are any delayed goals (other than headvar unifications)
%  at the point where we are about to schedule an impure goal.  If so, that is
%  an error.  Headvar unifications are allowed to be delayed because in the
%  case of output arguments, they cannot be scheduled until the variable value
%  is known.  If headvar unifications couldn't be delayed past impure goals,
%  impure predicates wouldn't be able to have outputs!
:- pred check_for_impurity_error(hlds_goal, impurity_errors, impurity_errors,
		mode_info, mode_info).
:- mode check_for_impurity_error(in, in, out, mode_info_di, mode_info_uo)
	is det.
check_for_impurity_error(Goal, ImpurityErrors0, ImpurityErrors) -->
	=(ModeInfo0),
	{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },
	{ delay_info__leave_conj(DelayInfo0, DelayedGoals,
				 DelayInfo1) },
	{ delay_info__enter_conj(DelayInfo1, DelayInfo) },
	{ mode_info_get_module_info(ModeInfo0, ModuleInfo) },
	{ mode_info_get_predid(ModeInfo0, PredId) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_clauses_info(PredInfo, ClausesInfo) },
	{ clauses_info_headvars(ClausesInfo, HeadVars) },
	(   { no_non_headvar_unification_goals(DelayedGoals, HeadVars) } ->
		{ ImpurityErrors = ImpurityErrors0 }
	;
		mode_info_set_delay_info(DelayInfo),
		{ get_all_waiting_vars(DelayedGoals, Vars) },
		{ ModeError = mode_error_conj(DelayedGoals,
					goals_followed_by_impure_goal(Goal)) },
		=(ModeInfo1),
		{ mode_info_get_context(ModeInfo1, Context) },
		{ mode_info_get_mode_context(ModeInfo1, ModeContext) },
		{ ImpurityError = mode_error_info(Vars, ModeError,
					Context, ModeContext) },
		{ ImpurityErrors = [ImpurityError | ImpurityErrors0] }
	).

	
:- pred no_non_headvar_unification_goals(list(delayed_goal), list(prog_var)).
:- mode no_non_headvar_unification_goals(in, in) is semidet.

no_non_headvar_unification_goals([], _).
no_non_headvar_unification_goals([delayed_goal(_,_,Goal-_)|Goals], HeadVars) :-
	Goal = unify(Var,Rhs,_,_,_),
	(   list__member(Var, HeadVars)
	;   Rhs = var(OtherVar),
	    list__member(OtherVar, HeadVars)
	),
	no_non_headvar_unification_goals(Goals, HeadVars).

:- pred dcg_set_state(T, T, T).
:- mode dcg_set_state(in, in, out) is det.

dcg_set_state(Val, _OldVal, Val).

	% Given an association list of Vars - Goals,
	% combine all the Vars together into a single set.

:- pred get_all_waiting_vars(list(delayed_goal), set(prog_var)).
:- mode get_all_waiting_vars(in, out) is det.

get_all_waiting_vars(DelayedGoals, Vars) :-
	set__init(Vars0),
	get_all_waiting_vars_2(DelayedGoals, Vars0, Vars).

:- pred get_all_waiting_vars_2(list(delayed_goal), set(prog_var),
		set(prog_var)).
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
modecheck_disj_list([Goal0 | Goals0], Goals, [InstMap | InstMaps]) -->
	mode_info_dcg_get_instmap(InstMap0),
	modecheck_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	modecheck_disj_list(Goals0, Goals1, InstMaps),
	%
	% If Goal is a nested disjunction,
	% then merge it with the outer disjunction.
	% If Goal is `fail', this will delete it.
	%
	{ goal_to_disj_list(Goal, DisjList) },
	{ list__append(DisjList, Goals1, Goals) }.

:- pred modecheck_case_list(list(case), prog_var, list(case), list(instmap),
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
	modecheck_functor_test(Var, ConsId),

	% modecheck this case (if it is reachable)
	mode_info_dcg_get_instmap(InstMap1),
	( { instmap__is_reachable(InstMap1) } ->
		modecheck_goal(Goal0, Goal1),
		mode_info_dcg_get_instmap(InstMap)
	;
		% We should not mode-analyse the goal, since it is unreachable.
		% Instead we optimize the goal away, so that later passes
		% won't complain about it not having mode information.
		{ true_goal(Goal1) },
		{ InstMap = InstMap1 }
	),

	% Don't lose the information added by the functor test above.
	{ fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal) },

	mode_info_set_instmap(InstMap0),
	modecheck_case_list(Cases0, Var, Cases, InstMaps).

	% modecheck_functor_test(ConsId, Var):
	%	update the instmap to reflect the fact that
	%	Var was bound to ConsId. 
	% This is used for the functor tests in `switch' statements.
	%
modecheck_functor_test(Var, ConsId) -->
		% figure out the arity of this constructor,
		% _including_ any type-infos or typeclass-infos
		% inserted for existential data types.
	=(ModeInfo0),
	{ mode_info_get_module_info(ModeInfo0, ModuleInfo) },
	{ mode_info_get_var_types(ModeInfo0, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) },
	{ AdjustedArity = cons_id_adjusted_arity(ModuleInfo, Type, ConsId) },

		% record the fact that Var was bound to ConsId in the instmap
	{ list__duplicate(AdjustedArity, free, ArgInsts) },
	modecheck_set_var_inst(Var,
		bound(unique, [functor(ConsId, ArgInsts)])).

%-----------------------------------------------------------------------------%

:- pred modecheck_par_conj_list(list(hlds_goal), list(hlds_goal),
		set(prog_var), list(pair(instmap, set(prog_var))),
		mode_info, mode_info).
:- mode modecheck_par_conj_list(in, out, in, out,
		mode_info_di, mode_info_uo) is det.

modecheck_par_conj_list([], [], _NonLocals, []) --> [].
modecheck_par_conj_list([Goal0 | Goals0], [Goal|Goals], NonLocals, 
		[InstMap - GoalNonLocals | InstMaps]) -->
	mode_info_dcg_get_instmap(InstMap0),
	{ Goal0 = _ - GoalInfo },
	{ goal_info_get_nonlocals(GoalInfo, GoalNonLocals) },
	mode_info_get_parallel_vars(PVars0),
	{ set__init(Bound0) },
	mode_info_set_parallel_vars([NonLocals - Bound0|PVars0]),

	modecheck_goal(Goal0, Goal),
	mode_info_get_parallel_vars(PVars1),
	(
		{ PVars1 = [_ - Bound1|PVars2] },
		(
			{ PVars2 = [OuterNonLocals - OuterBound0|PVars3] },
			{ set__intersect(OuterNonLocals, Bound1, Bound) },
			{ set__union(OuterBound0, Bound, OuterBound) },
			{ PVars = [OuterNonLocals - OuterBound|PVars3] },
			mode_info_set_parallel_vars(PVars)
		;
			{ PVars2 = [] },
			mode_info_set_parallel_vars(PVars2)
		)
	;
		{ PVars1 = [] },
		{ error("lost parallel vars") }
	),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	mode_info_lock_vars(par_conj, Bound1),
	modecheck_par_conj_list(Goals0, Goals, NonLocals, InstMaps),
	mode_info_unlock_vars(par_conj, Bound1).

%-----------------------------------------------------------------------------%

	%
	% calculate the argument number offset that needs to be passed to
	% modecheck_var_list_is_live, modecheck_var_has_inst_list, and
	% modecheck_set_var_inst_list.  This offset number is calculated
	% so that real arguments get positive argument numbers and
	% type_info arguments get argument numbers less than or equal to 0.
	%
compute_arg_offset(PredInfo, ArgOffset) :-
	pred_info_arity(PredInfo, OrigArity),
	pred_info_arg_types(PredInfo, ArgTypes),
	list__length(ArgTypes, CurrentArity),
	ArgOffset = OrigArity - CurrentArity.

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of expected livenesses,
	% ensure the liveness of each variable satisfies the corresponding
	% expected liveness.

modecheck_var_list_is_live([_|_], [], _, _) -->
	{ error("modecheck_var_list_is_live: length mismatch") }.
modecheck_var_list_is_live([], [_|_], _, _) -->
	{ error("modecheck_var_list_is_live: length mismatch") }.
modecheck_var_list_is_live([], [], _NeedExactMatch, _ArgNum) --> [].
modecheck_var_list_is_live([Var|Vars], [IsLive|IsLives], NeedExactMatch,
		ArgNum0) -->
	{ ArgNum is ArgNum0 + 1 },
	mode_info_set_call_arg_context(ArgNum),
	modecheck_var_is_live(Var, IsLive, NeedExactMatch),
	modecheck_var_list_is_live(Vars, IsLives, NeedExactMatch, ArgNum).

:- pred modecheck_var_is_live(prog_var, is_live, bool, mode_info, mode_info).
:- mode modecheck_var_is_live(in, in, in, mode_info_di, mode_info_uo) is det.

	% `live' means possibly used later on, and
	% `dead' means definitely not used later on.
	% If you don't need an exact match, then
	% the only time you get an error is if you pass a variable
	% which is live to a predicate that expects the variable to
	% be dead; the predicate may use destructive update to clobber
	% the variable, so we must be sure that it is dead after the call.

modecheck_var_is_live(VarId, ExpectedIsLive, NeedExactMatch,
		ModeInfo0, ModeInfo) :-
	mode_info_var_is_live(ModeInfo0, VarId, VarIsLive),
	( 
		( ExpectedIsLive = dead, VarIsLive = live
		; NeedExactMatch = yes, VarIsLive \= ExpectedIsLive
		)
	->
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

modecheck_var_has_inst_list(Vars, Insts, NeedEaxctMatch, ArgNum, Subst) -->
	{ map__init(Subst0) },
	modecheck_var_has_inst_list_2(Vars, Insts, NeedEaxctMatch, ArgNum,
		Subst0, Subst).

:- pred modecheck_var_has_inst_list_2(list(prog_var), list(inst), bool, int,
		inst_var_sub, inst_var_sub, mode_info, mode_info).
:- mode modecheck_var_has_inst_list_2(in, in, in, in, in, out,
		mode_info_di, mode_info_uo) is det.

modecheck_var_has_inst_list_2([_|_], [], _, _, _, _) -->
	{ error("modecheck_var_has_inst_list: length mismatch") }.
modecheck_var_has_inst_list_2([], [_|_], _, _, _, _) -->
	{ error("modecheck_var_has_inst_list: length mismatch") }.
modecheck_var_has_inst_list_2([], [], _Exact, _ArgNum, Subst, Subst) --> [].
modecheck_var_has_inst_list_2([Var|Vars], [Inst|Insts],
		NeedExactMatch, ArgNum0, Subst0, Subst) -->
	{ ArgNum is ArgNum0 + 1 },
	mode_info_set_call_arg_context(ArgNum),
	modecheck_var_has_inst(Var, Inst, NeedExactMatch, Subst0, Subst1),
	modecheck_var_has_inst_list_2(Vars, Insts,
		NeedExactMatch, ArgNum, Subst1, Subst).

:- pred modecheck_var_has_inst(prog_var, inst, bool,
		inst_var_sub, inst_var_sub, mode_info, mode_info).
:- mode modecheck_var_has_inst(in, in, in,
		in, out, mode_info_di, mode_info_uo) is det.

modecheck_var_has_inst(VarId, Inst, NeedExactMatch, Subst0, Subst,
		ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	instmap__lookup_var(InstMap, VarId, VarInst),
	mode_info_get_var_types(ModeInfo0, VarTypes),
	map__lookup(VarTypes, VarId, Type),

	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		(
			NeedExactMatch = no,
			inst_matches_initial(VarInst, Inst, Type, ModuleInfo0,
				ModuleInfo, Subst0, Subst1)
		;
			NeedExactMatch = yes,
			inst_matches_final(VarInst, Inst, Type, ModuleInfo0),
			ModuleInfo = ModuleInfo0,
			Subst1 = Subst0
			% WARNING:
			% The code above (Subst1 = Subst0) assumes that there
			% are no inst variables in the mode of the callee.
			% Currently this will always true, since
			% `NeedExactMatch' will be `yes' only if we are
			% doing mode inference on the callee, and mode
			% inference currently will not infer polymorphic modes.
			% But that assumption might not always hold in future.
			% An alternative would be to call inst_matches_initial
			% here too, just to calculate the inst substitution.
			% But that would be less efficient, so (at least
			% for now) we don't do it.
		)
	->
		Subst = Subst1,
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo)
	;
		Subst = Subst0,
		set__singleton_set(WaitingVars, VarId),
		mode_info_error(WaitingVars,
			mode_error_var_has_inst(VarId, VarInst, Inst),
			ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

modecheck_set_var_inst_list(Vars0, InitialInsts, FinalInsts, ArgOffset,
		Vars, Goals) -->
	(
		modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts,
			no_extra_goals, ArgOffset, Vars1, Goals1)
	->
		{ Vars = Vars1, Goals = Goals1 }
	;
		{ error("modecheck_set_var_inst_list: length mismatch") }
	).

:- pred modecheck_set_var_inst_list_2(list(prog_var), list(inst), list(inst),
		extra_goals, int, list(prog_var), extra_goals,
		mode_info, mode_info).
:- mode modecheck_set_var_inst_list_2(in, in, in, in, in, out, out,
					mode_info_di, mode_info_uo) is semidet.

modecheck_set_var_inst_list_2([], [], [], ExtraGoals, _, [], ExtraGoals) -->
	[].
modecheck_set_var_inst_list_2([Var0 | Vars0], [InitialInst | InitialInsts],
			[FinalInst | FinalInsts], ExtraGoals0, ArgNum0,
			[Var | Vars], ExtraGoals) -->
	{ ArgNum is ArgNum0 + 1 },
	mode_info_set_call_arg_context(ArgNum),
	modecheck_set_var_inst(Var0, InitialInst, FinalInst,
				Var, ExtraGoals0, ExtraGoals1),
	modecheck_set_var_inst_list_2(Vars0, InitialInsts, FinalInsts,
 				ExtraGoals1, ArgNum, Vars, ExtraGoals).

:- pred modecheck_set_var_inst(prog_var, inst, inst, prog_var, extra_goals,
		extra_goals, mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, in, out, in, out,
				mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var0, InitialInst, FinalInst, Var,
		ExtraGoals0, ExtraGoals, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( instmap__is_reachable(InstMap0) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap__lookup_var(InstMap0, Var0, VarInst0),
		handle_implied_mode(Var0, VarInst0, InitialInst,
		 	Var, ExtraGoals0, ExtraGoals, ModeInfo0, ModeInfo1),
		modecheck_set_var_inst(Var0, FinalInst, ModeInfo1, ModeInfo2),
		( Var = Var0 ->
			ModeInfo = ModeInfo2
		;
			modecheck_set_var_inst(Var, FinalInst,
				ModeInfo2, ModeInfo)
		)
	;
		Var = Var0,
		ExtraGoals = ExtraGoals0,
		ModeInfo = ModeInfo0
	).

	% Note that there are two versions of modecheck_set_var_inst,
	% one with arity 7 and one with arity 4.
	% The former is used for predicate calls, where we may need
	% to introduce unifications to handle calls to implied modes.

modecheck_set_var_inst(Var0, FinalInst, ModeInfo00, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_parallel_vars(PVars0, ModeInfo00, ModeInfo0),
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
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo3)
		;
			% If we haven't added any information and
			% we haven't bound any part of the var, then
			% the only thing we can have done is lose uniqueness.
			mode_info_get_var_types(ModeInfo1, VarTypes),
			map__lookup(VarTypes, Var0, Type),
			inst_matches_initial(Inst0, Inst, Type, ModuleInfo)
		->
			instmap__set(InstMap0, Var0, Inst, InstMap),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo3)
		;
			% We must have either added some information,
			% lost some uniqueness, or bound part of the var.
			% The call to inst_matches_binding will succeed
			% only if we haven't bound any part of the var.
			mode_info_get_var_types(ModeInfo1, VarTypes),
			map__lookup(VarTypes, Var0, Type),
			inst_matches_binding(Inst, Inst0, Type, ModuleInfo)
		->
			% We've just added some information
			% or lost some uniqueness.
			instmap__set(InstMap0, Var0, Inst, InstMap),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo2),
			mode_info_get_delay_info(ModeInfo2, DelayInfo0),
			delay_info__bind_var(DelayInfo0, Var0, DelayInfo),
			mode_info_set_delay_info(DelayInfo,
				ModeInfo2, ModeInfo3)
		;
			% We've bound part of the var.  If the var was locked,
			% then we need to report an error.
			mode_info_var_is_locked(ModeInfo1, Var0, Reason0)
		->
			set__singleton_set(WaitingVars, Var0),
			mode_info_error(WaitingVars,
				mode_error_bind_var(Reason0, Var0, Inst0, Inst),
				ModeInfo1, ModeInfo3
			)
		;
			instmap__set(InstMap0, Var0, Inst, InstMap),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo2),
			mode_info_get_delay_info(ModeInfo2, DelayInfo0),
			delay_info__bind_var(DelayInfo0, Var0, DelayInfo),
			mode_info_set_delay_info(DelayInfo,
						ModeInfo2, ModeInfo3)
		)
	;
		ModeInfo3 = ModeInfo0
	),
	(
		PVars0 = [],
		ModeInfo = ModeInfo3
	;
		PVars0 = [NonLocals - Bound0|PVars1],
		( set__member(Var0, NonLocals) ->
			set__insert(Bound0, Var0, Bound),
			PVars = [NonLocals - Bound|PVars1]
		;
			PVars = PVars0
		),
		mode_info_set_parallel_vars(PVars, ModeInfo3, ModeInfo)
	).


% If this was a call to an implied mode for that variable, then we need to
% introduce a fresh variable.

:- pred handle_implied_mode(prog_var, inst, inst, prog_var,
		extra_goals, extra_goals, mode_info, mode_info).
:- mode handle_implied_mode(in, in, in, out, in, out,
		mode_info_di, mode_info_uo) is det.

handle_implied_mode(Var0, VarInst0, InitialInst0, Var,
		ExtraGoals0, ExtraGoals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	inst_expand(ModuleInfo0, InitialInst0, InitialInst),
	inst_expand(ModuleInfo0, VarInst0, VarInst1),

	mode_info_get_var_types(ModeInfo0, VarTypes0),
	map__lookup(VarTypes0, Var0, VarType),
	(
		% If the initial inst of the variable matches_final
		% the initial inst specified in the pred's mode declaration,
		% then it's not a call to an implied mode, it's an exact
		% match with a genuine mode.
		inst_matches_final(VarInst1, InitialInst, VarType, ModuleInfo0)
	->
		Var = Var0,
		ExtraGoals = ExtraGoals0,
		ModeInfo = ModeInfo0
	;
		% This is the implied mode case.
		% We do not yet handle implied modes for partially
		% instantiated vars, since that would require
		% doing a partially instantiated deep copy, and we
		% don't know how to do that yet.
		(
			InitialInst = any(_),
			inst_is_free(ModuleInfo0, VarInst1)
		->
			% This is the simple case of implied `any' modes,
			% where the declared mode was `any -> ...'
			% and the argument passed was `free'
			
			Var = Var0,

			% Create code to initialize the variable to
			% inst `any', by calling <mod>:<type>_init_any/1,
			% where <mod>:<type> is the type of the variable.
			% XXX We ought to use a more elegant method
			% XXX than hard-coding the name `<foo>_init_any'.

			mode_info_get_context(ModeInfo0, Context),
			mode_info_get_mode_context(ModeInfo0, ModeContext),
			mode_context_to_unify_context(ModeContext, ModeInfo0,
				UnifyContext),
			CallUnifyContext = yes(call_unify_context(
						Var, var(Var), UnifyContext)),
			( 
				type_to_ctor_and_args(VarType, TypeCtor, _TypeArgs),
				TypeCtor = qualified(TypeModule, TypeName) -
						_TypeArity,
				string__append(TypeName, "_init_any", PredName),
				modes__build_call(TypeModule, PredName, [Var],
					Context, CallUnifyContext, ModuleInfo0,
					BeforeGoal - GoalInfo0)
			->
				set__singleton_set(NonLocals, Var),
				goal_info_set_nonlocals(GoalInfo0,
					NonLocals, GoalInfo1),
				InstmapDeltaAL = [Var - InitialInst],
				instmap_delta_from_assoc_list(InstmapDeltaAL,
					InstmapDelta),
				goal_info_set_instmap_delta(GoalInfo1,
					InstmapDelta, GoalInfo),
				NewExtraGoal = extra_goals(
					[BeforeGoal - GoalInfo], []),
				append_extra_goals(ExtraGoals0, NewExtraGoal,
					ExtraGoals),
				ModeInfo0 = ModeInfo
			;
				% If the type is a type variable,
				% or there isn't any <mod>:<type>_init_any/1
				% predicate, then give up.
				ExtraGoals = ExtraGoals0,
				set__singleton_set(WaitingVars, Var0),
				mode_info_error(WaitingVars,
					mode_error_implied_mode(Var0, VarInst0,
					InitialInst),
					ModeInfo0, ModeInfo
				)
			)
		;
			inst_is_bound(ModuleInfo0, InitialInst)
		->
			% This is the case we can't handle
			Var = Var0,
			ExtraGoals = ExtraGoals0,
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
			varset__new_var(VarSet0, Var, VarSet),
			map__set(VarTypes0, Var, VarType, VarTypes),
			mode_info_set_varset(VarSet, ModeInfo0, ModeInfo1),
			mode_info_set_var_types(VarTypes, ModeInfo1, ModeInfo),

			% Construct the code to do the unification
			modecheck_unify__create_var_var_unification(Var0, Var,
				VarType, ModeInfo, ExtraGoal),

			% append the goals together in the appropriate order:
			% ExtraGoals0, then NewUnify
			NewUnifyExtraGoal = extra_goals([], [ExtraGoal]),
			append_extra_goals(ExtraGoals0, NewUnifyExtraGoal,
				ExtraGoals)
		)
	).

:- pred modes__build_call(module_name, string, list(prog_var),
		prog_context, maybe(call_unify_context), module_info,
		hlds_goal).
:- mode modes__build_call(in, in, in, in, in, in, out) is semidet.

modes__build_call(Module, Name, ArgVars, Context, CallUnifyContext, ModuleInfo,
		Goal) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	list__length(ArgVars, Arity),
	predicate_table_search_pred_m_n_a(PredicateTable, Module, Name, Arity,
		[PredId]),
	hlds_pred__proc_id_to_int(ModeId, 0), % first mode
	Call = call(PredId, ModeId, ArgVars, not_builtin, CallUnifyContext,
		qualified(Module, Name)),
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	Goal = Call - GoalInfo.

%-----------------------------------------------------------------------------%

mode_context_to_unify_context(unify(UnifyContext, _), _, UnifyContext).
mode_context_to_unify_context(call(CallId, Arg), _ModeInfo,
		unify_context(call(CallId, Arg), [])).
mode_context_to_unify_context(uninitialized, _, _) :-
	error("mode_context_to_unify_context: uninitialized context").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% check that the evaluation method is OK for the given mode(s).
% we also check the mode of main/2 here.

:- pred check_eval_methods(module_info, module_info, io__state, io__state).
:- mode check_eval_methods(in, out, di, uo) is det.

check_eval_methods(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	pred_check_eval_methods(PredIds, ModuleInfo0, ModuleInfo).

:- pred pred_check_eval_methods(list(pred_id), module_info, module_info,
		io__state, io__state).
:- mode pred_check_eval_methods(in, in, out, di, uo) is det.

pred_check_eval_methods([], M, M) --> [].
pred_check_eval_methods([PredId|Rest], ModuleInfo0, ModuleInfo) --> 
	{ module_info_preds(ModuleInfo0, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procids(PredInfo, ProcIds) },
	proc_check_eval_methods(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	pred_check_eval_methods(Rest, ModuleInfo1, ModuleInfo).	

:- pred proc_check_eval_methods(list(proc_id), pred_id, module_info, 
		module_info, io__state, io__state).
:- mode proc_check_eval_methods(in, in, in, out, di, uo) is det.

proc_check_eval_methods([], _, M, M) --> [].
proc_check_eval_methods([ProcId|Rest], PredId, ModuleInfo0, ModuleInfo) --> 
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, 
		PredInfo, ProcInfo) },
	{ proc_info_eval_method(ProcInfo, EvalMethod) },
	{ proc_info_argmodes(ProcInfo, Modes) },
	( 
		{ eval_method_requires_ground_args(EvalMethod) = yes },
		\+ { only_fully_in_out_modes(Modes, ModuleInfo0) } 
	->
		report_eval_method_requires_ground_args(ProcInfo,
			ModuleInfo0, ModuleInfo1)
	;
		{ ModuleInfo1 = ModuleInfo0 }	
	),	
	( 
		{ eval_method_destroys_uniqueness(EvalMethod) = yes },
		\+ { only_nonunique_modes(Modes, ModuleInfo1) } 
	->
		report_eval_method_destroys_uniqueness(ProcInfo,
			ModuleInfo1, ModuleInfo2)
	;
		{ ModuleInfo2 = ModuleInfo1 }	
	),
	(
		{ pred_info_name(PredInfo, "main") },
		{ pred_info_arity(PredInfo, 2) },
		{ pred_info_is_exported(PredInfo) },
		{ \+ check_mode_of_main(Modes, ModuleInfo2) }
	->
		report_wrong_mode_for_main(ProcInfo,
			ModuleInfo2, ModuleInfo3)
	;
		{ ModuleInfo3 = ModuleInfo2 }
	),
	proc_check_eval_methods(Rest, PredId, ModuleInfo3, ModuleInfo).

:- pred only_fully_in_out_modes(list(mode), module_info).
:- mode only_fully_in_out_modes(in, in) is semidet.

only_fully_in_out_modes([], _).
only_fully_in_out_modes([Mode|Rest], ModuleInfo) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	(
		inst_is_ground(ModuleInfo, InitialInst)
	;
		inst_is_free(ModuleInfo, InitialInst),
		(
			inst_is_free(ModuleInfo, FinalInst)
		;
			inst_is_ground(ModuleInfo, FinalInst)
		)
	),
	only_fully_in_out_modes(Rest, ModuleInfo).

:- pred only_nonunique_modes(list(mode), module_info).
:- mode only_nonunique_modes(in, in) is semidet.

only_nonunique_modes([], _).
only_nonunique_modes([Mode|Rest], ModuleInfo) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_not_partly_unique(ModuleInfo, InitialInst),
	inst_is_not_partly_unique(ModuleInfo, FinalInst),
	only_nonunique_modes(Rest, ModuleInfo).

:- pred check_mode_of_main(list(mode), module_info).
:- mode check_mode_of_main(in, in) is semidet.

check_mode_of_main([Di, Uo], ModuleInfo) :-
	mode_get_insts(ModuleInfo, Di, DiInitialInst, DiFinalInst),
	mode_get_insts(ModuleInfo, Uo, UoInitialInst, UoFinalInst),
	%
	% Note that we hard-code these tests,
	% rather than using `inst_is_free', `inst_is_unique', etc.,
	% since for main/2 we're looking for an exact match
	% (modulo inst synonyms) with what the language reference
	% manual specifies, rather than looking for a particular
	% abstract property.
	%
	inst_expand(ModuleInfo, DiInitialInst, ground(unique, none)),
	inst_expand(ModuleInfo, DiFinalInst, ground(clobbered, none)),
	inst_expand(ModuleInfo, UoInitialInst, Free),
	( Free = free ; Free = free(_Type) ),
	inst_expand(ModuleInfo, UoFinalInst, ground(unique, none)).

:- pred report_eval_method_requires_ground_args(proc_info,
		module_info, module_info, io__state, io__state).
:- mode report_eval_method_requires_ground_args(in, in, out, di, uo) is det.

report_eval_method_requires_ground_args(ProcInfo, ModuleInfo0, ModuleInfo) -->
	{ proc_info_eval_method(ProcInfo, EvalMethod) },
	{ proc_info_context(ProcInfo, Context) },
	{ eval_method_to_string(EvalMethod, EvalMethodS) },
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	prog_out__write_context(Context),
	io__write_string("Sorry, not implemented: `pragma "),
	io__write_string(EvalMethodS),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string(
	    "  declaration not allowed for procedure with\n"),
	prog_out__write_context(Context),
	io__write_string(
	    "  partially instantiated modes.\n"), 
	( { VerboseErrors = yes } ->
		io__write_string(
"	Tabling of predicates/functions with partially instantiated modes
	is not currently implemented.\n")
	;
		[]
	),
	{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }.

:- pred report_eval_method_destroys_uniqueness(proc_info,
		module_info, module_info, io__state, io__state).
:- mode report_eval_method_destroys_uniqueness(in, in, out, di, uo) is det.

report_eval_method_destroys_uniqueness(ProcInfo, ModuleInfo0, ModuleInfo) -->
	{ proc_info_eval_method(ProcInfo, EvalMethod) },
	{ proc_info_context(ProcInfo, Context) },
	{ eval_method_to_string(EvalMethod, EvalMethodS) },
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	prog_out__write_context(Context),
	io__write_string("Error: `pragma "),
	io__write_string(EvalMethodS),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string(
	    "  declaration not allowed for procedure with\n"),
	prog_out__write_context(Context),
	io__write_string("  unique modes.\n"), 
	( { VerboseErrors = yes } ->
		io__write_string(
"	Tabling of predicates/functions with unique modes is not allowed
	as this would lead to a copying of the unique arguments which 
	would result in them no longer being unique.\n")
	;
		[]
	),
	{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }.

:- pred report_wrong_mode_for_main(proc_info,
		module_info, module_info, io__state, io__state).
:- mode report_wrong_mode_for_main(in, in, out, di, uo) is det.

report_wrong_mode_for_main(ProcInfo, ModuleInfo0, ModuleInfo) -->
	{ proc_info_context(ProcInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Error: main/2 must have mode `(di, uo)'.\n"),
	{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }.

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
