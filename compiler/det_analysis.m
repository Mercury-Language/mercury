%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_analysis.m - the determinism analysis pass.

% Main authors: conway, fjh, zs.

% This pass has three components:
%
%	o Segregate the procedures into those that have determinism
%		declarations, and those that don't
%
%	o A step of performing a local analysis pass on each procedure
%		without a determinism declaration is iterated until
%		a fixpoint is reached
%
%	o A checking step is performed on all the procedures that have
%		determinism declarations to ensure that they are at
%		least as deterministic as their declaration. This uses
%		a form of the local analysis pass.
%
% If we are to avoid global analysis for predicates with
% declarations, then it must be an error, not just a warning,
% if the determinism checking step detects that the determinism
% annotation was wrong.  If we were to issue just a warning, then
% we would have to override the determinism annotation, and that
% would force us to re-check the inferred determinism for all
% calling predicates.
%
% Alternately, we could leave it as a warning, but then we would
% have to _make_ the predicate deterministic (or semideterministic)
% by inserting run-time checking code which calls error/1 if the
% predicate really isn't deterministic (semideterministic).

% Determinism has three components:
%
%	whether a goal can fail
%	whether a goal has more than one possible solution
%	whether a goal occurs in a context where only the first solution
%		is required
%
% The first two components are synthesized attributes: they are inferred
% bottom-up.  The last component is an inherited attribute: it is
% propagated top-down.

%-----------------------------------------------------------------------------%

:- module det_analysis.

:- interface.

:- import_module hlds, io.

	% perform full determinism analysis and checking, including determinism
	% inference for local predicates with no determinism declaration
:- pred determinism_pass(module_info, module_info, io__state, io__state).
:- mode determinism_pass(in, out, di, uo) is det.

	% check the determinism of a single procedure
	% (only works if the determinism of the procedures it calls
	% has already been inferred).
:- pred determinism_check_proc(proc_id, pred_id, module_info, module_info,
				io__state, io__state).
:- mode determinism_check_proc(in, in, in, out, di, uo) is det.

:- pred det_conjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_conjunction_maxsoln(in, in, out) is det.

:- pred det_conjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_conjunction_canfail(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, list, map, set, std_util, require.

:- import_module det_report, prog_io, mode_util, globals, options, passes_aux.

%-----------------------------------------------------------------------------%

determinism_pass(ModuleInfo0, ModuleInfo) -->
	{ determinism_declarations(ModuleInfo0, DeclaredProcs,
		UndeclaredProcs) },
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Doing determinism analysis pass(es).."),
	maybe_flush_output(Verbose),
		% Note that `global_analysis_pass' can actually be several
		% passes.  It prints out a `.' for each pass.
	global_analysis_pass(ModuleInfo0, UndeclaredProcs, ModuleInfo1),
	maybe_write_string(Verbose, " done.\n"),
	maybe_write_string(Verbose, "% Doing determinism checking pass...\n"),
	maybe_flush_output(Verbose),
	global_final_pass(ModuleInfo1, DeclaredProcs, ModuleInfo),
	maybe_write_string(Verbose, "% done.\n").

determinism_check_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) -->
	global_final_pass(ModuleInfo0, [proc(PredId, ProcId)], ModuleInfo).

%-----------------------------------------------------------------------------%

	% determinism_declarations takes a module_info as input and
	% returns two lists of procedure ids, the first being those
	% with determinism declarations, and the second being those without.

:- pred determinism_declarations(module_info, pred_proc_list, pred_proc_list).
:- mode determinism_declarations(in, out, out) is det.

determinism_declarations(ModuleInfo, DeclaredProcs, UndeclaredProcs) :-
	get_all_pred_procs(ModuleInfo, PredProcs),
	segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs).

	% get_all_pred_procs takes a module_info and returns a list
	% of all the procedures ids for that module.

:- pred get_all_pred_procs(module_info, pred_proc_list).
:- mode get_all_pred_procs(in, out) is det.

get_all_pred_procs(ModuleInfo, PredProcs) :-
	module_info_predids(ModuleInfo, PredIds),
	module_info_preds(ModuleInfo, Preds),
	get_all_pred_procs_2(Preds, PredIds, [], PredProcs).

:- pred get_all_pred_procs_2(pred_table, list(pred_id),
				pred_proc_list, pred_proc_list).
:- mode get_all_pred_procs_2(in, in, in, out) is det.

get_all_pred_procs_2(_Preds, [], PredProcs, PredProcs).
get_all_pred_procs_2(Preds, [PredId|PredIds], PredProcs0, PredProcs) :-
	map__lookup(Preds, PredId, Pred),
	pred_info_non_imported_procids(Pred, ProcIds),
	fold_pred_modes(PredId, ProcIds, PredProcs0, PredProcs1),
	get_all_pred_procs_2(Preds, PredIds, PredProcs1, PredProcs).

:- pred fold_pred_modes(pred_id, list(proc_id), pred_proc_list, pred_proc_list).
:- mode fold_pred_modes(in, in, in, out) is det.

fold_pred_modes(_PredId, [], PredProcs, PredProcs).
fold_pred_modes(PredId, [ProcId|ProcIds], PredProcs0, PredProcs) :-
	fold_pred_modes(PredId, ProcIds, [proc(PredId, ProcId) | PredProcs0],
		PredProcs).

	% segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs)
	% splits the list of procedures PredProcs into DeclaredProcs and
	% UndeclaredProcs.

:- pred segregate_procs(module_info, pred_proc_list, pred_proc_list,
	pred_proc_list).
:- mode segregate_procs(in, in, out, out) is det.

segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs) :-
	segregate_procs_2(ModuleInfo, PredProcs, [], DeclaredProcs,
					[], UndeclaredProcs).

:- pred segregate_procs_2(module_info, pred_proc_list, pred_proc_list,
			pred_proc_list, pred_proc_list, pred_proc_list).
:- mode segregate_procs_2(in, in, in, out, in, out) is det.

segregate_procs_2(_ModuleInfo, [], DeclaredProcs, DeclaredProcs,
				UndeclaredProcs, UndeclaredProcs).
segregate_procs_2(ModuleInfo, [proc(PredId, PredMode) | PredProcs],
		DeclaredProcs0, DeclaredProcs,
		UndeclaredProcs0, UndeclaredProcs) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, Pred),
	pred_info_procedures(Pred, Procs),
	map__lookup(Procs, PredMode, Proc),
	proc_info_declared_determinism(Proc, MaybeDetism),
	(
		MaybeDetism = no,
		UndeclaredProcs1 = [proc(PredId, PredMode) | UndeclaredProcs0],
		DeclaredProcs1 = DeclaredProcs0
	;
		MaybeDetism = yes(_),
		DeclaredProcs1 = [proc(PredId, PredMode) | DeclaredProcs0],
		UndeclaredProcs1 = UndeclaredProcs0
	),
	segregate_procs_2(ModuleInfo, PredProcs, DeclaredProcs1, DeclaredProcs,
		UndeclaredProcs1, UndeclaredProcs).

%-----------------------------------------------------------------------------%

:- pred global_analysis_pass(module_info, pred_proc_list, module_info,
	io__state, io__state).
:- mode global_analysis_pass(in, in, out, di, uo) is det.

	% Iterate until a fixpoint is reached. This can be expensive
	% if a module has many predicates with undeclared determinisms.
	% If this ever becomes a problem, we should switch to doing
	% iterations only on strongly connected components of the
	% dependency graph.

global_analysis_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "."),
	maybe_flush_output(Verbose),
	global_analysis_single_pass(ProcList, ModuleInfo0, ModuleInfo1,
		unchanged, Changed),
	( { Changed = changed } ->
		global_analysis_pass(ModuleInfo1, ProcList, ModuleInfo)
	;
		{ ModuleInfo = ModuleInfo1 }
	).

:- pred global_analysis_single_pass(pred_proc_list,
	module_info, module_info, maybe_changed, maybe_changed,
	io__state, io__state).
:- mode global_analysis_single_pass(in, in, out, in, out, di, uo)
	is det.

global_analysis_single_pass([], ModuleInfo, ModuleInfo,
	Changed, Changed) --> [].
global_analysis_single_pass([proc(PredId, PredMode) | PredProcs],
		ModuleInfo0, ModuleInfo, Changed0, Changed) -->
	{ det_infer_proc(PredId, PredMode, ModuleInfo0, ModuleInfo1,
		Changed0, Changed1, Msgs) },
	( { Msgs \= [] } ->
		det_report_msgs(Msgs, ModuleInfo1),
		globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
		( { HaltAtWarn = yes } ->
			{ module_info_incr_errors(ModuleInfo1, ModuleInfo2) }
		;
			{ ModuleInfo2 = ModuleInfo1 }
		)
	;
		{ ModuleInfo2 = ModuleInfo1 }
	),
	global_analysis_single_pass(PredProcs,
		ModuleInfo2, ModuleInfo, Changed1, Changed).

:- pred global_final_pass(module_info, pred_proc_list,
	module_info, io__state, io__state).
:- mode global_final_pass(in, in, out, di, uo) is det.

global_final_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	global_analysis_single_pass(ProcList, ModuleInfo0, ModuleInfo1,
		unchanged, _),
	global_checking_pass(ProcList, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Infer the determinism of a procedure.

:- pred det_infer_proc(pred_id, proc_id, module_info, module_info,
	maybe_changed, maybe_changed, list(det_msg)).
:- mode det_infer_proc(in, in, in, out, in, out, out) is det.

det_infer_proc(PredId, PredMode, ModuleInfo0, ModuleInfo, Changed0, Changed,
		Msgs) :-

		% Get the proc_info structure for this procedure
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, Pred0),
	pred_info_procedures(Pred0, Procs0),
	map__lookup(Procs0, PredMode, Proc0),

		% Remember the old inferred determinism of this procedure
	proc_info_inferred_determinism(Proc0, Detism0),

		% Work out whether the procedure occurs in a single-solution
		% context or not.  Currently we only assume so if
		% the predicate has an explicit determinism declaration
		% that says so.
	(
		proc_info_declared_determinism(Proc0, yes(DeclaredDetism)),
		determinism_components(DeclaredDetism, _, at_most_many_cc)
	->
		SolnContext = first_soln
	;	
		SolnContext = all_solns
	),

		% Infer the determinism of the goal
	proc_info_goal(Proc0, Goal0),
	proc_info_get_initial_instmap(Proc0, ModuleInfo0, InstMap0),
	MiscInfo = misc_info(ModuleInfo0, PredId, PredMode),
	det_infer_goal(Goal0, InstMap0, SolnContext, MiscInfo,
			Goal, Detism1, Msgs),

		% Take the worst of the old and new detisms.
		% This is needed to prevent loops on p :- not(p)
		% at least if the initial assumed detism is det.
	determinism_components(Detism0, CanFail0, MaxSoln0),
	determinism_components(Detism1, CanFail1, MaxSoln1),
	det_switch_canfail(CanFail0, CanFail1, CanFail),
	det_switch_maxsoln(MaxSoln0, MaxSoln1, MaxSoln),
	determinism_components(Detism, CanFail, MaxSoln),

		% Check whether the determinism of this procedure changed
	(
		Detism = Detism0
	->
		Changed = Changed0
	;
		Changed = changed
	),

		% Save the newly inferred information
	proc_info_set_goal(Proc0, Goal, Proc1),
	proc_info_set_inferred_determinism(Proc1, Detism, Proc),
	map__set(Procs0, PredMode, Proc, Procs),
	pred_info_set_procedures(Pred0, Procs, Pred),
	map__set(Preds0, PredId, Pred, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo).

%-----------------------------------------------------------------------------%

	% given a goal and an initial instmap, compute the final instmap that
	% results from the initial instmap after execution of the goal.

:- pred update_instmap(hlds__goal, instmap, instmap).
:- mode update_instmap(in, in, out) is det.

update_instmap(_Goal0 - GoalInfo0, InstMap0, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
	apply_instmap_delta(InstMap0, DeltaInstMap, InstMap).

%-----------------------------------------------------------------------------%

	% Infers the determinism of `Goal0' and returns this in `Detism'.
	% It annotates the goal and all its subgoals with their determinism
	% and returns the annotated goal in `Goal'.

:- pred det_infer_goal(hlds__goal, instmap, soln_context, misc_info,
	hlds__goal, determinism, list(det_msg)).
:- mode det_infer_goal(in, in, in, in, out, out, out) is det.

det_infer_goal(Goal0 - GoalInfo0, InstMap0, SolnContext0, MiscInfo,
		Goal - GoalInfo, Detism, Msgs) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),

	% If a goal has no output variables, then the goal is in
	% single-solution context

	( no_output_vars(NonLocalVars, InstMap0, DeltaInstMap, MiscInfo) ->
		OutputVars = no,
		SolnContext = first_soln
	;
		OutputVars = yes,
		SolnContext = SolnContext0
	),

	det_infer_goal_2(Goal0, GoalInfo0, InstMap0, SolnContext, MiscInfo,
		NonLocalVars, DeltaInstMap, Goal1, InternalDetism, Msgs1),

	determinism_components(InternalDetism, InternalCanFail, InternalSolns),
	(
		% If a goal with multiple solutions has no output variables,
		% then it really it has only one solution
		% (we will need to do pruning)

		( InternalSolns = at_most_many
		; InternalSolns = at_most_many_cc
		),
		OutputVars = no
	->
		determinism_components(Detism, InternalCanFail, at_most_one)
	;
		% If a goal with multiple solutions occurs in a single-solution
		% context, then we will need to do pruning

		InternalSolns = at_most_many,
		SolnContext = first_soln
	->
		determinism_components(Detism, InternalCanFail, at_most_many_cc)
	;
		Detism = InternalDetism
	),

	goal_info_set_determinism(GoalInfo0, Detism, GoalInfo),

	% See how we should introduce the commit operator, if one is needed.

	(
		Goal1 = disj(Disjuncts),
		Disjuncts \= [],
		determinism_components(Detism, _, ExternalSolns),
		ExternalSolns \= at_most_many
	->
		% a disjunction with no more than one solution
		% needs to get converted into an if-then-else or
		% something similar
		det_fixup_disj(Disjuncts, InternalDetism, OutputVars,
			GoalInfo, InstMap0, MiscInfo, Goal, Msgs1, Msgs)
	;
		Detism \= InternalDetism,
		Goal1 \= some(_, _)
	->
		% a commit needed - we must introduce an explicit `some'
		% so that the code generator knows to insert the appropriate
		% code for pruning
		goal_info_set_determinism(GoalInfo0, InternalDetism, InnerInfo),
		Goal = some([], Goal1 - InnerInfo),
		Msgs = Msgs1
	;
		% either no commit needed, or a `some' already present
		Goal = Goal1,
		Msgs = Msgs1
	).

%-----------------------------------------------------------------------------%

	% Disjunctions that cannot succeed more than once when viewed from the
	% outside generally need some fixing up, and/or some warnings to be
	% issued.
	%
	% For locally multidet disjunctions without output vars, which are det
	% from the outside, generate a warning, and then as an optimization
	% replace the whole disjunction with a disjunct that cannot fail.
	% 
	% For locally det disjunctions with or without output var(s),
	% (i.e. disjunctions in which the end of every disjunct except
	% one is unreachable) generate a warning, and then as an
	% optimization find the first disjunct which cannot fail and
	% replace the whole disjunction with that disjunct.
	%
	% For locally nondet disjunctions without output vars, which are
	% semidet from outside, we replace them with an if-then-else.
	% (Is there any good reason for this?  Why not just leave them
	% as semidet disjunctions?)
	%
	% For locally semidet disjunctions with or without output var(s),
	% (i.e. disjunctions in which the end of every disjunct except
	% one is unreachable) 
	% leave
	% the disjunction as semidet; the code generator must handle such
	% semidet disjunctions by emitting code equivalent to an if-then-else
	% chain. If the disjunction has output vars, generate a warning.
	% 
	% For locally semidet disjunctions without output var(s), we should
	% pick whichever of approaches 3 and 4 generates smaller code.
	%
	% For disjunctions that cannot succeed, generate a warning.
	%
	% The second argument is the *internal* determinism of the disjunction.

:- pred det_fixup_disj(list(hlds__goal), determinism, bool,
		hlds__goal_info, instmap, misc_info, hlds__goal_expr,
		list(det_msg), list(det_msg)).
:- mode det_fixup_disj(in, in, in, in, in, in, out, in, out) is det.

det_fixup_disj(Disjuncts, cc_multidet, OutputVars, GoalInfo, _, _,
		Goal, Msgs0, Msgs) :-
	( OutputVars = no ->
		Msgs = [multidet_disj(GoalInfo, Disjuncts) | Msgs0]
	;
		Msgs = Msgs0
	),
	% Same considerations apply to GoalInfos as for det disjunctions
	% below.
	det_pick_no_fail_disjunct(Disjuncts, Goal - _).
det_fixup_disj(Disjuncts, multidet, OutputVars, GoalInfo, _, _,
		Goal, Msgs0, Msgs) :-
	( OutputVars = no ->
		Msgs = [multidet_disj(GoalInfo, Disjuncts) | Msgs0]
	;
		Msgs = Msgs0
	),
	% Same considerations apply to GoalInfos as for det disjunctions
	% below.
	det_pick_no_fail_disjunct(Disjuncts, Goal - _).
det_fixup_disj(Disjuncts, cc_nondet, OutputVars, GoalInfo, _InstMap, _MiscInfo,
		Goal, Msgs0, Msgs) :-
	Msgs = Msgs0,
	( OutputVars = no ->
		det_disj_to_ite(Disjuncts, GoalInfo, IfThenElse),
		IfThenElse = Goal - _
	;
		% This is the case of a nondet disjunction in a
		% single-solution context.  We can't replace it with
		% an if-then-else, because the disjuncts may bind output
		% variables.  We just leave it as a model_semi disjunction;
		% the code generator will generate similar code to what
		% it would for an if-then-else.
		Goal = disj(Disjuncts)
	).
det_fixup_disj(Disjuncts, nondet, OutputVars, GoalInfo, _InstMap, _MiscInfo,
		Goal, Msgs0, Msgs) :-
	Msgs = Msgs0,
	( OutputVars = no ->
		det_disj_to_ite(Disjuncts, GoalInfo, IfThenElse),
		IfThenElse = Goal - _
	;
		% This is the case of a nondet disjunction in a
		% single-solution context.  We can't replace it with
		% an if-then-else, because the disjuncts may bind output
		% variables.  We just leave it as a model_semi disjunction;
		% the code generator will generate similar code to what
		% it would for an if-then-else.
		Goal = disj(Disjuncts)
	).
det_fixup_disj(Disjuncts, det, _OutputVars, GoalInfo, _, _, Goal,
		Msgs0, Msgs) :-
	% We are discarding the GoalInfo of the picked goal; we will
	% replace it with the GoalInfo inferred for the disjunction
	% as a whole. Although the disjunction may be det while the
	% picked disjunct is erroneous, this is OK, since erronoues
	% and det use the same code model. The replacement of the
	% GoalInfo is necessary to prevent spurious determinism
	% errors outside the disjunction.
	Msgs = [det_disj(GoalInfo, Disjuncts) | Msgs0],
	det_pick_no_fail_disjunct(Disjuncts, Goal - _).
det_fixup_disj(Disjuncts, semidet, OutputVars, GoalInfo, _, _, Goal,
		Msgs0, Msgs) :-
	( OutputVars = yes ->
		Msgs = [semidet_disj(GoalInfo, Disjuncts) | Msgs0]
	;
		Msgs = Msgs0
	),
	Goal = disj(Disjuncts).
det_fixup_disj(Disjuncts, erroneous, _OutputVars, GoalInfo, _, _, Goal,
		Msgs0, Msgs) :-
	% Same considerations apply to GoalInfos as for det disjunctions
	% above.
	Msgs = [zero_soln_disj(GoalInfo, Disjuncts) | Msgs0],
	det_pick_no_fail_disjunct(Disjuncts, Goal - _).
det_fixup_disj(Disjuncts, failure, _OutputVars, GoalInfo, _, _, Goal,
		Msgs0, Msgs) :-
	Msgs = [zero_soln_disj(GoalInfo, Disjuncts) | Msgs0],
	Goal = disj(Disjuncts).

:- pred det_pick_no_fail_disjunct(list(hlds__goal), hlds__goal).
:- mode det_pick_no_fail_disjunct(in, out) is det.

det_pick_no_fail_disjunct([], _) :-
	error("cannot find a disjunct that cannot fail").
det_pick_no_fail_disjunct([Goal0 | Goals0], Goal) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_determinism(GoalInfo0, Detism),
	determinism_components(Detism, CanFail, _),
	( CanFail = cannot_fail ->
		Goal = Goal0
	;
		det_pick_no_fail_disjunct(Goals0, Goal)
	).

:- pred det_disj_to_ite(list(hlds__goal), hlds__goal_info, hlds__goal).
% :- mode det_disj_to_ite(di, in, uo) is det.
:- mode det_disj_to_ite(in, in, out) is det.

	% det_disj_to_ite is used to transform disjunctions that occur
	% in prunable contexts into if-then-elses.
	% For example, it would transform
	%
	%	( Disjunct1
	%	; Disjunct2
	%	; Disjunct3
	%	)
	% into
	%	( Disjunct1 ->
	%		true
	%	; Disjunct2 ->
	%		true
	%	;
	%		Disjunct3
	%	).

det_disj_to_ite([], GoalInfo, disj([]) - GoalInfo).
det_disj_to_ite([Disjunct | Disjuncts], GoalInfo, Goal) :-
	( Disjuncts = [] ->
		Goal = Disjunct
	;
		Cond = Disjunct,
		Cond = _CondGoal - CondGoalInfo,

		goal_info_init(InitGoalInfo0),
		map__init(InstMap1),
		goal_info_set_instmap_delta(InitGoalInfo0,
				reachable(InstMap1), InitGoalInfo),
		Then = conj([]) - InitGoalInfo,

		det_disj_to_ite(Disjuncts, GoalInfo, Rest),
		Rest = _RestGoal - RestGoalInfo,

		goal_info_get_nonlocals(CondGoalInfo, CondNonLocals),
		goal_info_get_nonlocals(RestGoalInfo, RestNonLocals),
		set__union(CondNonLocals, RestNonLocals, NonLocals),
		goal_info_set_nonlocals(GoalInfo, NonLocals, NewGoalInfo0),

		goal_info_get_instmap_delta(GoalInfo, InstMapDelta0),
		(
			InstMapDelta0 = reachable(InstMap0),
			map__select(InstMap0, NonLocals, InstMap),
			InstMapDelta = reachable(InstMap)
		;
			InstMapDelta0 = unreachable,
			InstMapDelta = InstMapDelta0
		),
		goal_info_set_instmap_delta(NewGoalInfo0,
					InstMapDelta, NewGoalInfo),

		Goal = if_then_else([], Cond, Then, Rest) - NewGoalInfo
	).

%-----------------------------------------------------------------------------%

:- pred det_infer_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
		soln_context, misc_info, set(var), instmap_delta,
		hlds__goal_expr, determinism, list(det_msg)).
:- mode det_infer_goal_2(in, in, in, in, in, in, in, out, out, out) is det.

	% The determinism of a conjunction is the worst case of the elements
	% of that conjuction.

det_infer_goal_2(conj(Goals0), GoalInfo0, InstMap0, SolnContext, MiscInfo, _, _,
		Goal, Detism, Msgs) :-
	( Goals0 = [SingleGoal0] ->
		% a singleton conjunction is equivalent to the goal itself
		det_infer_goal(SingleGoal0, InstMap0, SolnContext, MiscInfo,
				SingleGoal, Detism, Msgs),
		Goal = conj([SingleGoal])
	;
		det_infer_conj(Goals0, InstMap0, SolnContext, MiscInfo,
			Goals1, Detism, Msgs),
		% Conjunctions that cannot produce solutions may nevertheless
		% contain nondet and multidet goals. If this happens, the part
		% of the conjunction up to and including the always-failing
		% goal are put inside a some to appease the code generator.
		( determinism_components(Detism, CanFail, at_most_zero) ->
			det_fixup_nosoln_conj(Goals1, Goals, no, NeedCut),
			( NeedCut = yes ->
				determinism_components(InnerDetism,
					CanFail, at_most_many),
				goal_info_set_determinism(GoalInfo0,
					InnerDetism, InnerInfo),
				InnerGoal = conj(Goals) - InnerInfo,
				Goal = some([], InnerGoal)
			;
				Goal = conj(Goals)
			)
		;
			Goal = conj(Goals1)
		)
	).

det_infer_goal_2(disj(Goals0), _, InstMap0, SolnContext, MiscInfo, _, _,
		Goal, Detism, Msgs) :-
	( Goals0 = [SingleGoal0] ->
		% a singleton disjunction is equivalent to the goal itself
		det_infer_goal(SingleGoal0, InstMap0, SolnContext, MiscInfo,
			Goal - _, Detism, Msgs)
	;
		det_infer_disj(Goals0, InstMap0, SolnContext, MiscInfo,
			can_fail, at_most_zero, Goals, Detism, Msgs),
		Goal = disj(Goals)
	).

	% The determinism of a switch is the worst of the determinism of each
	% of the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the SwitchCanFail field.

det_infer_goal_2(switch(Var, SwitchCanFail, Cases0), _, InstMap0, SolnContext,
		MiscInfo, _, _,
		switch(Var, SwitchCanFail, Cases), Detism, Msgs) :-
	det_infer_switch(Cases0, InstMap0, SolnContext, MiscInfo,
		cannot_fail, at_most_zero, Cases, CasesDetism, Msgs),
	determinism_components(CasesDetism, CasesCanFail, CasesSolns),
	det_conjunction_canfail(SwitchCanFail, CasesCanFail, CanFail),
	determinism_components(Detism, CanFail, CasesSolns).

	% For calls, just look up the determinism entry associated with
	% the called predicate.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.

det_infer_goal_2(call(PredId, ModeId, A, B, C, N, F), GoalInfo, _, SolnContext,
		MiscInfo, _, _,
		call(PredId, ModeId, A, B, C, N, F), Detism, Msgs) :-
	det_lookup_detism(MiscInfo, PredId, ModeId, Detism),
	determinism_components(Detism, _, NumSolns),
	( NumSolns = at_most_many_cc, SolnContext \= first_soln ->
		Msgs = [cc_pred_in_wrong_context(GoalInfo, Detism,
				PredId, ModeId)]
	;
		Msgs = []
	).

	% unifications are either deterministic or semideterministic.
	% (see det_infer_unify).
det_infer_goal_2(unify(LT, RT0, M, U, C), GoalInfo, InstMap0, _SolnContext,
		MiscInfo, _, _, unify(LT, RT, M, U, C), UnifyDet, Msgs) :-
	( RT0 = lambda_goal(Vars, Modes, LambdaDeclaredDet, Goal0) ->
		(
			determinism_components(LambdaDeclaredDet, _,
				at_most_many_cc)
		->
			LambdaSolnContext = first_soln
		;	
			LambdaSolnContext = all_solns
		),
		det_infer_goal(Goal0, InstMap0, LambdaSolnContext, MiscInfo,
				Goal, LambdaInferredDet, Msgs1),
		det_check_lambda(LambdaDeclaredDet, LambdaInferredDet,
				Goal, GoalInfo, MiscInfo, Msgs2),
		list__append(Msgs1, Msgs2, Msgs),
		RT = lambda_goal(Vars, Modes, LambdaDeclaredDet, Goal)
	;
		RT = RT0,
		Msgs = []
	),
	det_infer_unify(U, MiscInfo, UnifyDet).

det_infer_goal_2(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo0, InstMap0,
		SolnContext, MiscInfo, NonLocalVars, DeltaInstMap,
		Goal, Detism, Msgs) :-

	% We process the goal right-to-left, doing the `then' before
	% the condition of the if-then-else, so that we can propagate
	% the SolnContext correctly.

	%
	% First process the `then' part
	%
	update_instmap(Cond0, InstMap0, InstMap1),
	det_infer_goal(Then0, InstMap1, SolnContext, MiscInfo,
			Then, ThenDetism, ThenMsgs),
	determinism_components(ThenDetism, ThenCanFail, ThenSolns),

	%
	% Next, work out the right soln_context to use for the condition.
	% The condition is in a first_soln context if and only if the goal
	% as a whole was in a first_soln context and the `then' part
	% cannot fail.
	%
	(
		ThenCanFail = cannot_fail,
		SolnContext = first_soln
	->
		CondSolnContext = first_soln
	;
		CondSolnContext = all_solns
	),

	%
	% Now process the condition.
	%
	det_infer_goal(Cond0, InstMap0, CondSolnContext, MiscInfo,
			Cond, CondDetism, CondMsgs),
	determinism_components(CondDetism, CondCanFail, CondSolns),

	%
	% Check for some special cases
	%
	( CondCanFail = cannot_fail ->
		%
		% Optimize away the `else' part.
		% (We should actually convert this to a _sequential_
		% conjunction, because if-then-else has an ordering
		% constraint, whereas conjunction doesn't; however,
		% currently reordering is only done in mode analysis,
		% not in the code generator, so we don't have a
		% sequential conjunction construct.)
		%
		goal_to_conj_list(Cond, CondList),
		goal_to_conj_list(Then0, ThenList),
		list__append(CondList, ThenList, List),
		det_infer_goal_2(conj(List), GoalInfo0, InstMap0, SolnContext,
				MiscInfo, NonLocalVars, DeltaInstMap,
				Goal, Detism, Msgs1),
		Msgs = [ite_cond_cannot_fail(GoalInfo0)|Msgs1]
/***********
% The following optimization is not semantically valid if Cond can raise
% an exception. Since this part of the compiler doesn't (yet) know about
% the possibilities of exceptions, we forego the optimization.
% (The safe part of this optimization is now done by modes.m anyway -
% it replaces `(A -> B ; C)' with `not(A), B'.)
%	; CondSolns = at_most_zero ->
%		% Optimize away the condition and the `then' part.
%		Else0 = ElseGoal0 - _,
%		det_infer_goal_2(ElseGoal0, InstMap0, MiscInfo,
%			NonLocalVars, DeltaInstMap, Goal, Detism)
************/
	;
		%
		% Process the `else' part
		%
		det_infer_goal(Else0, InstMap0, SolnContext, MiscInfo,
				Else, ElseDetism, ElseMsgs),
		determinism_components(ElseDetism, ElseCanFail, ElseSolns),

		%
		% Finally combine the results from the three parts
		%
		Goal = if_then_else(Vars, Cond, Then, Else),
		det_conjunction_maxsoln(CondSolns, ThenSolns, AllThenSolns),
		det_switch_maxsoln(AllThenSolns, ElseSolns, Solns),
		det_switch_canfail(ThenCanFail, ElseCanFail, CanFail),
		determinism_components(Detism, CanFail, Solns),
		list__append(ThenMsgs, ElseMsgs, AfterMsgs),
		list__append(CondMsgs, AfterMsgs, Msgs)
	).

	% Negations are almost always semideterministic.  It is an error for
	% a negation to further instantiate any non-local variable. Such
	% errors will be reported by the mode analysis.
	%
	% Question: should we warn about the negation of goals that either
	% cannot succeed or cannot fail?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(not(Goal0), _, InstMap0, _SolnContext, MiscInfo, _, _, Goal,
		Det, Msgs) :-
	det_infer_goal(Goal0, InstMap0, first_soln, MiscInfo,
			Goal1, NegDet, Msgs),
	det_negation_det(NegDet, MaybeDet),
	(
		MaybeDet = no,
		error("inappropriate determinism inside a negation")
	;
		MaybeDet = yes(Det),
		(
		% replace `not true' with `fail'
			Goal1 = conj([]) - _GoalInfo
		->
			Goal = disj([])
		;
		% replace `not fail' with `true'
			Goal1 = disj([]) - _GoalInfo2
		->
			Goal = conj([])
		;
			Goal = not(Goal1)
		)
	).
	% The following optimizations are generic versions of the ones above,
	% but they are semantically valid only if we know that the goal
	% concerned cannot raise exceptions.
	%	determinism_components(NegDet, NegCanFail, NegSolns),
	%	( NegCanFail = cannot_fail, NegDet \= erroneous ->
	%		Goal = disj([])
	%	; NegSolns = at_most_zero ->
	%		Goal = conj([])
	%	;
	%		Goal = not(Goal1)
	%	).

	% Existential quantification may require a cut to throw away solutions,
	% but we cannot rely on explicit quantification to detect this.
	% Therefore cuts are handled in det_infer_goal.

det_infer_goal_2(some(Vars0, Goal0), _, InstMap0, SolnContext, MiscInfo, _, _,
			Goal, Det, Msgs) :-
	det_infer_goal(Goal0, InstMap0, SolnContext, MiscInfo,
			Goal1, Det, Msgs),
	% make sure that nested `some's are replaced by a single `some'
	( Goal1 = some(Vars1, Goal2) - _GoalInfo ->
		list__append(Vars0, Vars1, Vars),
		Goal = some(Vars, Goal2)
	;
		Goal = some(Vars0, Goal1)
	).

	% pragma_c_code must be deterministic.
det_infer_goal_2(pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap), 
		GoalInfo, _, SolnContext, MiscInfo, _, _,
		pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap),
		Detism, Msgs) :-
	det_lookup_detism(MiscInfo, PredId, ProcId, Detism),
	determinism_components(Detism, _, NumSolns),
	( NumSolns = at_most_many_cc, SolnContext \= first_soln ->
		Msgs = [cc_pred_in_wrong_context(GoalInfo, Detism,
				PredId, ProcId)]
	;
		Msgs = []
	).

%-----------------------------------------------------------------------------%

:- pred det_infer_conj(list(hlds__goal), instmap, soln_context, misc_info,
		list(hlds__goal), determinism, list(det_msg)).
:- mode det_infer_conj(in, in, in, in, out, out, out) is det.

det_infer_conj([], _InstMap0, _SolnContext, _MiscInfo, [], det, []).
det_infer_conj([Goal0 | Goals0], InstMap0, SolnContext, MiscInfo, 
		[Goal | Goals], Detism, Msgs) :-

	% We should look to see when we get to a not_reached point
	% and optimize away the remaining elements of the conjunction.
	% But that optimization is done in the code generation anyway.

	% We infer the determinisms right-to-left, so that we can propagate
	% the SolnContext properly.

	%
	% First, process the second and subsequent conjuncts
	%
	update_instmap(Goal0, InstMap0, InstMap1),
	det_infer_conj(Goals0, InstMap1, SolnContext, MiscInfo,
			Goals, DetismB, MsgsB),
	determinism_components(DetismB, CanFailB, MaxSolnsB),

	%
	% Next, work out whether the first conjunct is in
	% a first_soln context or not
	%
	( 
		CanFailB = cannot_fail,
		SolnContext = first_soln
	->
		SolnContextA = first_soln
	;
		SolnContextA = all_solns
	),
	%
	% Process the first conjunct
	%
	det_infer_goal(Goal0, InstMap0, SolnContextA, MiscInfo,
			Goal, DetismA, MsgsA),
	determinism_components(DetismA, CanFailA, MaxSolnsA),

	%
	% Finally combine the results computed above
	%
	det_conjunction_canfail(CanFailA, CanFailB, CanFail),
	det_conjunction_maxsoln(MaxSolnsA, MaxSolnsB, MaxSolns),
	determinism_components(Detism, CanFail, MaxSolns),
	list__append(MsgsA, MsgsB, Msgs).

:- pred det_fixup_nosoln_conj(list(hlds__goal), list(hlds__goal), bool, bool).
:- mode det_fixup_nosoln_conj(in, out, in, out) is det.

det_fixup_nosoln_conj([], _, _, _) :-
	error("conjunction without solutions has no failing goal").
det_fixup_nosoln_conj([Goal0 | Goals0], Goals, NeedCut0, NeedCut) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_determinism(GoalInfo0, Detism0),
	determinism_components(Detism0, _, MaxSolns0),
	( MaxSolns0 = at_most_zero ->
		Goals = [Goal0],
		NeedCut = NeedCut0
	;
		( MaxSolns0 = at_most_many ->
			NeedCut1 = yes
		;
			NeedCut1 = NeedCut0
		),
		det_fixup_nosoln_conj(Goals0, Goals1, NeedCut1, NeedCut),
		Goals = [Goal0 | Goals1]
	).

:- pred det_infer_disj(list(hlds__goal), instmap, soln_context, misc_info,
	can_fail, soln_count, list(hlds__goal), determinism, list(det_msg)).
:- mode det_infer_disj(in, in, in, in, in, in, out, out, out) is det.

det_infer_disj([], _InstMap0, _SolnContext, _MiscInfo, CanFail, MaxSolns,
		[], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_disj([Goal0 | Goals0], InstMap0, SolnContext, MiscInfo, CanFail0,
		MaxSolns0, [Goal | Goals1], Detism, Msgs) :-
	det_infer_goal(Goal0, InstMap0, SolnContext, MiscInfo,
			Goal, Detism1, Msgs1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_disjunction_canfail(CanFail0, CanFail1, CanFail2),
	det_disjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_disj(Goals0, InstMap0, SolnContext, MiscInfo, CanFail2,
		MaxSolns2, Goals1, Detism, Msgs2),
	list__append(Msgs1, Msgs2, Msgs3),
	( MaxSolns1 = at_most_zero ->
		Goal0 = _ - GoalInfo0,
		Msgs = [zero_soln_disjunct(GoalInfo0) | Msgs3]
	;
		Msgs = Msgs3
	).

:- pred det_infer_switch(list(case), instmap, soln_context, misc_info,
	can_fail, soln_count, list(case), determinism, list(det_msg)).
:- mode det_infer_switch(in, in, in, in, in, in, out, out, out) is det.

det_infer_switch([], _InstMap0, _SolnContext, _MiscInfo, CanFail, MaxSolns,
		[], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_switch([Case0 | Cases0], InstMap0, SolnContext, MiscInfo, CanFail0,
		MaxSolns0, [Case | Cases], Detism, Msgs) :-
	% Technically, we should update the instmap to reflect the
	% knowledge that the var is bound to this particular
	% constructor, but we wouldn't use that information here anyway,
	% so we don't bother.
	Case0 = case(ConsId, Goal0),
	det_infer_goal(Goal0, InstMap0, SolnContext, MiscInfo,
			Goal, Detism1, Msgs1),
	Case = case(ConsId, Goal),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_switch_canfail(CanFail0, CanFail1, CanFail2),
	det_switch_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_switch(Cases0, InstMap0, SolnContext, MiscInfo, CanFail2,
		MaxSolns2, Cases, Detism, Msgs2),
	list__append(Msgs1, Msgs2, Msgs).

:- pred det_infer_unify(unification, misc_info, determinism).
:- mode det_infer_unify(in, in, out) is det.

	% Deconstruction unifications are deterministic if the type
	% only has one constructor, or if the variable is known to be
	% already bound to the appropriate functor.
	% 
	% This is handled (modulo bugs) by modes.m, which sets
	% the determinism field in the deconstruct(...) to semidet for
	% those deconstruction unifications which might fail.
	% But switch_detection.m may set it back to det again, if it moves
	% the functor test into a switch instead.

det_infer_unify(deconstruct(_, _, _, _, CanFail), _MiscInfo, Detism) :-
	determinism_components(Detism, CanFail, at_most_one).
det_infer_unify(assign(_, _), _MiscInfo, det).
det_infer_unify(construct(_, _, _, _), _MiscInfo, det).
det_infer_unify(simple_test(_, _), _MiscInfo, semidet).
det_infer_unify(complicated_unify(_, CanFail, _), _MiscInfo, Detism) :-
	determinism_components(Detism, CanFail, at_most_one).

%-----------------------------------------------------------------------------%

det_conjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_one,     at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many,    at_most_zero).

det_conjunction_maxsoln(at_most_one,     at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_conjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_conjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_conjunction_maxsoln(at_most_many,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_conjunction_canfail(can_fail,    can_fail,    can_fail).
det_conjunction_canfail(can_fail,    cannot_fail, can_fail).
det_conjunction_canfail(cannot_fail, can_fail,    can_fail).
det_conjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

:- pred det_disjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_disjunction_maxsoln(in, in, out) is det.

det_disjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_disjunction_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_disjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_disjunction_maxsoln(at_most_one,     at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_disjunction_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

:- pred det_disjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_disjunction_canfail(in, in, out) is det.

det_disjunction_canfail(can_fail,    can_fail,    can_fail).
det_disjunction_canfail(can_fail,    cannot_fail, cannot_fail).
det_disjunction_canfail(cannot_fail, can_fail,    cannot_fail).
det_disjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

:- pred det_switch_maxsoln(soln_count, soln_count, soln_count).
:- mode det_switch_maxsoln(in, in, out) is det.

det_switch_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_switch_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_switch_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_switch_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_switch_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_switch_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_switch_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_switch_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_switch_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_switch_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many,    at_most_many,    at_most_many).

:- pred det_switch_canfail(can_fail, can_fail, can_fail).
:- mode det_switch_canfail(in, in, out) is det.

det_switch_canfail(can_fail,    can_fail,    can_fail).
det_switch_canfail(can_fail,    cannot_fail, can_fail).
det_switch_canfail(cannot_fail, can_fail,    can_fail).
det_switch_canfail(cannot_fail, cannot_fail, cannot_fail).

:- pred det_negation_det(determinism, maybe(determinism)).
:- mode det_negation_det(in, out) is det.

det_negation_det(det,		yes(failure)).
det_negation_det(semidet,	yes(semidet)).
det_negation_det(multidet,	no).
det_negation_det(nondet,	no).
det_negation_det(cc_multidet,	no).
det_negation_det(cc_nondet,	no).
det_negation_det(erroneous,	yes(erroneous)).
det_negation_det(failure,	yes(det)).

%-----------------------------------------------------------------------------%
