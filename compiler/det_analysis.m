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

%-----------------------------------------------------------------------------%

:- module det_analysis.
:- interface.
:- import_module hlds.

:- pred determinism_pass(module_info, module_info, io__state, io__state).
:- mode determinism_pass(in, out, di, uo) is det.

:- pred det_conjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_conjunction_maxsoln(in, in, out) is det.

:- pred det_conjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_conjunction_canfail(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, prog_io, prog_out, hlds_out, std_util.
:- import_module globals, options, io, mercury_to_mercury, varset, int, term.
:- import_module type_util, mode_util, quantification, inst_match, require.

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
	global_checking_pass(ModuleInfo1, DeclaredProcs, ModuleInfo),
	maybe_write_string(Verbose, "% done.\n").

%-----------------------------------------------------------------------------%

:- type predproclist	==	list(pair(pred_id, proc_id)).

:- type misc_info	--->	misc_info(
				% generally useful info:
					module_info,
				% the id of the procedure
				% we are currently processing:
					pred_id,	
					proc_id
				).

	% determinism_declarations takes a module_info as input and
	% returns two lists of procedure ids, the first being those
	% with determinism declarations, and the second being those without.

:- pred determinism_declarations(module_info, predproclist, predproclist).
:- mode determinism_declarations(in, out, out) is det.

determinism_declarations(ModuleInfo, DeclaredProcs, UndeclaredProcs) :-
	get_all_pred_procs(ModuleInfo, PredProcs),
	segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs).

	% get_all_pred_procs takes a module_info and returns a list
	% of all the procedures ids for that module.

:- pred get_all_pred_procs(module_info, predproclist).
:- mode get_all_pred_procs(in, out) is det.

get_all_pred_procs(ModuleInfo, PredProcs) :-
	module_info_predids(ModuleInfo, PredIds),
	module_info_preds(ModuleInfo, Preds),
	get_all_pred_procs_2(Preds, PredIds, [], PredProcs).

:- pred get_all_pred_procs_2(pred_table, list(pred_id),
				predproclist, predproclist).
:- mode get_all_pred_procs_2(in, in, in, out) is det.

get_all_pred_procs_2(_Preds, [], PredProcs, PredProcs).
get_all_pred_procs_2(Preds, [PredId|PredIds], PredProcs0, PredProcs) :-
	map__lookup(Preds, PredId, Pred),
	pred_info_non_imported_procids(Pred, ProcIds),
	fold_pred_modes(PredId, ProcIds, PredProcs0, PredProcs1),
	get_all_pred_procs_2(Preds, PredIds, PredProcs1, PredProcs).

:- pred fold_pred_modes(pred_id, list(proc_id), predproclist, predproclist).
:- mode fold_pred_modes(in, in, in, out) is det.

fold_pred_modes(_PredId, [], PredProcs, PredProcs).
fold_pred_modes(PredId, [ProcId|ProcIds], PredProcs0, PredProcs) :-
	fold_pred_modes(PredId, ProcIds, [PredId - ProcId|PredProcs0],
		PredProcs).

	% segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs)
	% splits the list of procedures PredProcs into DeclaredProcs and
	% UndeclaredProcs.

:- pred segregate_procs(module_info, predproclist, predproclist, predproclist).
:- mode segregate_procs(in, in, out, out) is det.

segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs) :-
	segregate_procs_2(ModuleInfo, PredProcs, [], DeclaredProcs,
					[], UndeclaredProcs).

:- pred segregate_procs_2(module_info, predproclist, predproclist,
			predproclist, predproclist, predproclist).
:- mode segregate_procs_2(in, in, in, out, in, out) is det.

segregate_procs_2(_ModuleInfo, [], DeclaredProcs, DeclaredProcs,
				UndeclaredProcs, UndeclaredProcs).
segregate_procs_2(ModuleInfo, [PredId - PredMode|PredProcs],
			DeclaredProcs0, DeclaredProcs,
				UndeclaredProcs0, UndeclaredProcs) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, Pred),
	pred_info_procedures(Pred, Procs),
	map__lookup(Procs, PredMode, Proc),
	proc_info_declared_determinism(Proc, MaybeDetism),
	(
		MaybeDetism = no,
		UndeclaredProcs1 = [PredId - PredMode|UndeclaredProcs0],
		DeclaredProcs1 = DeclaredProcs0
	;
		MaybeDetism = yes(_),
		DeclaredProcs1 = [PredId - PredMode|DeclaredProcs0],
		UndeclaredProcs1 = UndeclaredProcs0
	),
	segregate_procs_2(ModuleInfo, PredProcs, DeclaredProcs1, DeclaredProcs,
				UndeclaredProcs1, UndeclaredProcs).

%-----------------------------------------------------------------------------%

:- pred global_analysis_pass(module_info, predproclist, module_info,
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

:- type maybe_changed	--->	changed ; unchanged.

:- type msg	--->	multidet_disj(hlds__goal_info, list(hlds__goal))
		;	det_disj(hlds__goal_info, list(hlds__goal))
		;	semidet_disj(hlds__goal_info, list(hlds__goal))
		;	zero_soln_disj(hlds__goal_info, list(hlds__goal))
		;	zero_soln_disjunct(hlds__goal_info).

:- pred global_analysis_single_pass(predproclist,
	module_info, module_info, maybe_changed, maybe_changed,
	io__state, io__state).
:- mode global_analysis_single_pass(in, in, out, in, out, di, uo)
	is det.

global_analysis_single_pass([], ModuleInfo, ModuleInfo,
	Changed, Changed) --> [].
global_analysis_single_pass([PredId - PredMode | PredProcs],
		ModuleInfo0, ModuleInfo, Changed0, Changed) -->
	{ det_infer_proc(PredId, PredMode, ModuleInfo0, ModuleInfo1,
		Changed0, Changed1, Msgs) },
	( { Msgs \= [] } ->
		det_report_msgs(Msgs),
		( globals__io_lookup_bool_option(halt_at_warn, yes) ->
			{ module_info_incr_errors(ModuleInfo1, ModuleInfo2) }
		;
			{ ModuleInfo2 = ModuleInfo1 }
		)
	;
		{ ModuleInfo2 = ModuleInfo1 }
	),
	global_analysis_single_pass(PredProcs,
		ModuleInfo2, ModuleInfo, Changed1, Changed).

%-----------------------------------------------------------------------------%

	% Infer the determinism of a procedure.

:- pred det_infer_proc(pred_id, proc_id, module_info, module_info,
	maybe_changed, maybe_changed, list(msg)).
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

		% Infer the determinism of the goal
	proc_info_goal(Proc0, Goal0),
	proc_info_get_initial_instmap(Proc0, ModuleInfo0, InstMap0),
	MiscInfo = misc_info(ModuleInfo0, PredId, PredMode),
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Detism1,
		Msgs),

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

	% Infers the determinism of `Goal0' and returns this in `Detism'.
	% It annotates the goal and all its subgoals with their determinism
	% and returns the annotated goal in `Goal'.

:- pred det_infer_goal(hlds__goal, instmap, misc_info,
	hlds__goal, instmap, determinism, list(msg)).
:- mode det_infer_goal(in, in, in, out, out, out, out) is det.

det_infer_goal(Goal0 - GoalInfo0, InstMap0, MiscInfo,
		Goal - GoalInfo, InstMap, Detism, Msgs) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
	apply_instmap_delta(InstMap0, DeltaInstMap, InstMap),
	det_infer_goal_2(Goal0, GoalInfo0, InstMap0, MiscInfo, NonLocalVars,
		DeltaInstMap, Goal1, InternalDetism, Msgs1),

	% If a goal with possibly multiple solutions doesn't have any
	% output variables, then we make it succeed at most once.

	( no_output_vars(NonLocalVars, InstMap0, DeltaInstMap, MiscInfo) ->
		OutputVars = no
	;
		OutputVars = yes
	),

	determinism_components(InternalDetism, InternalCanFail, InternalSolns),
	(
		InternalSolns = at_most_many,
		OutputVars = no
	->
		determinism_components(Detism, InternalCanFail, at_most_one)
	;
		Detism = InternalDetism
	),

	% See how we should introduce the commit operator, if one is needed.

	(
		Goal1 = disj(Disjuncts),
		Disjuncts \= [],
		determinism_components(Detism, _, ExternalSolns),
		\+ ExternalSolns = at_most_many
	->
		goal_info_set_determinism(GoalInfo0, Detism, GoalInfo),
		det_fixup_disj(Disjuncts, InternalDetism, OutputVars, GoalInfo,
			InstMap0, MiscInfo, Goal, Msgs1, Msgs)
	;
		Detism \= InternalDetism,
		Goal1 \= some(_, _)
	->
		goal_info_set_determinism(GoalInfo0, InternalDetism, InnerInfo),
		goal_info_set_determinism(GoalInfo0, Detism, GoalInfo),
		Goal = some([], Goal1 - InnerInfo),
		Msgs = Msgs1
	;
		goal_info_set_determinism(GoalInfo0, Detism, GoalInfo),
		Goal = Goal1,
		Msgs = Msgs1
	).

%-----------------------------------------------------------------------------%

	% Disjunctions that cannot succeed more than once when viwed from the
	% outside generally need some fixing up, and/or some warnings to be
	% issued.
	%
	% For locally multidet disjunctions without output vars, which are det
	% from the outside, generate a warning, then replace the whole
	% disjunction with a disjunct that cannot fail.
	% 
	% For locally det disjunctions with or without output var(s), generate
	% a warning, and find the first disjunct which cannot fail and replace
	% the whole disjunction with that disjunct.
	% 
	% For locally nondet disjunctions without output vars, which are
	% semidet from outside, replace them with an if-then-else.
	% 
	% For locally semidet disjunctions with or without output var(s), leave
	% the disjunction as semidet; the code generator must handle such
	% semidet disjunctions by emitting code equivalent to an if-then-else
	% chain. If the disjunction has output vars, generate a warning.
	% 
	% For locally semidet disjunctions without output var(s), we should
	% pick whichever of approaches 3 and 4 generates smaller code.
	%
	% For disjunctions that cannot succeed, generate a warning.
	%
	% The second argument is the *internal* determinism of the dijunction.

:- pred det_fixup_disj(list(hlds__goal), determinism, bool, hlds__goal_info,
	instmap, misc_info, hlds__goal_expr, list(msg), list(msg)).
:- mode det_fixup_disj(in, in, in, in, in, in, out, in, out) is det.

det_fixup_disj(Disjuncts, multidet, _OutputVars, GoalInfo, _, _, Goal,
		Msgs0, Msgs) :-
	% Same considerations apply to GoalInfos as for det disjunctions
	% above.
	Msgs = [multidet_disj(GoalInfo, Disjuncts) | Msgs0],
	det_pick_no_fail_disjunct(Disjuncts, Goal - _).
det_fixup_disj(Disjuncts, nondet, _, GoalInfo, InstMap, MiscInfo, Goal,
		Msgs0, Msgs) :-
	Msgs = Msgs0,
	det_disj_to_ite(Disjuncts, GoalInfo, Goal1),
	det_infer_goal(Goal1, InstMap, MiscInfo, Goal - _, _, NewDetism, _),
	( NewDetism = semidet ->
		true
	;
		error("transformation of pruned disj to ite changes its determinism")
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
	( Disjuncts = [Goal1 - _ | _] ->
		% Here we also discard the GoalInfo, but a disjunction can
		% be failure only if all the disjuncts are failure.
		Goal = Goal1
	;
		error("empty list of disjuncts in det_fixup_disj")
	).

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

:- pred det_infer_goal_2(hlds__goal_expr, hlds__goal_info, instmap, misc_info,
	set(var), instmap_delta, hlds__goal_expr, determinism,
	list(msg)).
:- mode det_infer_goal_2(in, in, in, in, in, in, out, out, out) is det.

	% The determinism of a conjunction is the worst case of the elements
	% of that conjuction.

det_infer_goal_2(conj(Goals0), GoalInfo0, InstMap0, MiscInfo, _, _,
		Goal, Detism, Msgs) :-
	( Goals0 = [SingleGoal0] ->
		% a singleton conjunction is equivalent to the goal itself
		det_infer_goal(SingleGoal0, InstMap0, MiscInfo,
				SingleGoal, _InstMap, Detism, Msgs),
		Goal = conj([SingleGoal])
	;
		det_infer_conj(Goals0, InstMap0, MiscInfo,
			cannot_fail, at_most_one, Goals1, Detism, Msgs),
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

det_infer_goal_2(disj(Goals0), _, InstMap0, MiscInfo, _, _,
		Goal, Detism, Msgs) :-
	( Goals0 = [SingleGoal0] ->
		% a singleton disjunction is equivalent to the goal itself
		det_infer_goal(SingleGoal0, InstMap0, MiscInfo,
			Goal - _, _InstMap, Detism, Msgs)
	;
		det_infer_disj(Goals0, InstMap0, MiscInfo,
			can_fail, at_most_zero, Goals, Detism, Msgs),
		Goal = disj(Goals)
	).

	% The determinism of a switch is the worst of the determinism of each
	% of the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the SwitchCanFail field.

det_infer_goal_2(switch(Var, SwitchCanFail, Cases0), _, InstMap0, MiscInfo,
		_, _, switch(Var, SwitchCanFail, Cases), Detism, Msgs) :-
	det_infer_switch(Cases0, InstMap0, MiscInfo, cannot_fail, at_most_zero,
		Cases, CasesDetism, Msgs),
	determinism_components(CasesDetism, CasesCanFail, CasesSolns),
	det_conjunction_canfail(SwitchCanFail, CasesCanFail, CanFail),
	determinism_components(Detism, CanFail, CasesSolns).

	% Look up the determinism entry associated with the call.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.

det_infer_goal_2(call(PredId, ModeId, A, B, C, N, F), _, _, MiscInfo, _, _,
		call(PredId, ModeId, A, B, C, N, F), Detism, []) :-
	det_lookup_detism(MiscInfo, PredId, ModeId, Detism).

	% unifications are either deterministic or semideterministic.
	% (see det_infer_unify).
det_infer_goal_2(unify(LT, RT, M, U, C), _, _, MiscInfo, _, _,
		unify(LT, RT, M, U, C), Detism, []) :-
	det_infer_unify(U, MiscInfo, Detism).

	% Question: should we warn about if-then-elses with deterministic
	% and erroneous conditions?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo0, InstMap0,
		MiscInfo, NonLocalVars, DeltaInstMap, Goal, Detism, Msgs) :-
	det_infer_goal(Cond0, InstMap0, MiscInfo, Cond, InstMap1, CondDetism,
		CondMsgs),
	determinism_components(CondDetism, CondCanFail, CondSolns),
	( CondCanFail = cannot_fail ->
		% Optimize away the `else' part.
		% (We should actually convert this to a _sequential_
		% conjunction, because if-then-else has an ordering
		% constraint, whereas conjunction doesn't; however,
		% currently reordering is only done in mode analysis,
		% not in the code generator, so we don't have a
		% sequential conjunction construct.)
		goal_to_conj_list(Cond, CondList),
		goal_to_conj_list(Then0, ThenList),
		list__append(CondList, ThenList, List),
		det_infer_goal_2(conj(List), GoalInfo0, InstMap0, MiscInfo,
			NonLocalVars, DeltaInstMap, Goal, Detism, Msgs)
/***********
% The following optimization is not semantically valid if Cond can raise
% an exception. Since this part of the compiler doesn't (yet) know about
% the possibilities of exceptions, we forego the optimization.
%	; CondSolns = at_most_zero ->
%		% Optimize away the condition and the `then' part.
%		Else0 = ElseGoal0 - _,
%		det_infer_goal_2(ElseGoal0, InstMap0, MiscInfo,
%			NonLocalVars, DeltaInstMap, Goal, Detism)
************/
	;
		det_infer_goal(Then0, InstMap1, MiscInfo, Then, _, ThenDetism,
			ThenMsgs),
		det_infer_goal(Else0, InstMap0, MiscInfo, Else, _, ElseDetism,
			ElseMsgs),
		list__append(ThenMsgs, ElseMsgs, AfterMsgs),
		list__append(CondMsgs, AfterMsgs, Msgs),
		Goal = if_then_else(Vars, Cond, Then, Else),
		determinism_components(ThenDetism, ThenCanFail, ThenSolns),
		determinism_components(ElseDetism, ElseCanFail, ElseSolns),
		det_conjunction_maxsoln(CondSolns, ThenSolns, AllThenSolns),
		det_switch_maxsoln(AllThenSolns, ElseSolns, Solns),
		det_switch_canfail(ThenCanFail, ElseCanFail, CanFail),
		determinism_components(Detism, CanFail, Solns)
	).

	% Negations are almost always semideterministic.  It is an error for
	% a negation to further instantiate any non-local variable. Such
	% errors will be reported by the mode analysis.
	%
	% Question: should we warn about the negation of goals that either
	% cannot succeed or cannot fail?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(not(Goal0), _, InstMap0, MiscInfo, _, _, Goal, Det, Msgs) :-
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal1, _InstMap, NegDet,
		Msgs),
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

det_infer_goal_2(some(Vars, Goal0), _, InstMap0, MiscInfo, _, _,
			some(Vars, Goal), Det, Msgs) :-
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Det, Msgs).

	% pragma_c_code must be deterministic.
det_infer_goal_2(pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap), 
		_, _, _, _, _, 
		pragma_c_code(C_Code, PredId, ProcId, Args, ArgNameMap),
		det, []).

%-----------------------------------------------------------------------------%

:- pred det_infer_conj(list(hlds__goal), instmap, misc_info,
	can_fail, soln_count, list(hlds__goal), determinism, list(msg)).
:- mode det_infer_conj(in, in, in, in, in, out, out, out) is det.

det_infer_conj([], _InstMap0, _MiscInfo, CanFail, MaxSolns, [], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_conj([Goal0 | Goals0], InstMap0, MiscInfo, CanFail0, MaxSolns0,
		[Goal | Goals], Detism, Msgs) :-
	% We should look to see when we get to a not_reached point
	% and optimize away the remaining elements of the conjunction.
	% But that optimization is done in the code generation anyway.

	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, InstMap1, Detism1,
		Msgs1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_conjunction_canfail(CanFail0, CanFail1, CanFail2),
	det_conjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_conj(Goals0, InstMap1, MiscInfo, CanFail2, MaxSolns2,
		Goals, Detism, Msgs2),
	list__append(Msgs1, Msgs2, Msgs).

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

:- pred det_infer_disj(list(hlds__goal), instmap, misc_info,
	can_fail, soln_count, list(hlds__goal), determinism, list(msg)).
:- mode det_infer_disj(in, in, in, in, in, out, out, out) is det.

det_infer_disj([], _InstMap0, _MiscInfo, CanFail, MaxSolns, [], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_disj([Goal0 | Goals0], InstMap0, MiscInfo, CanFail0, MaxSolns0,
		[Goal | Goals1], Detism, Msgs) :-
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Detism1,
		Msgs1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_disjunction_canfail(CanFail0, CanFail1, CanFail2),
	det_disjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_disj(Goals0, InstMap0, MiscInfo, CanFail2, MaxSolns2,
		Goals1, Detism, Msgs2),
	list__append(Msgs1, Msgs2, Msgs3),
	( MaxSolns1 = at_most_zero ->
		Goal0 = _ - GoalInfo0,
		Msgs = [zero_soln_disjunct(GoalInfo0) | Msgs3]
	;
		Msgs = Msgs3
	).

:- pred det_infer_switch(list(case), instmap, misc_info,
	can_fail, soln_count, list(case), determinism, list(msg)).
:- mode det_infer_switch(in, in, in, in, in, out, out, out) is det.

det_infer_switch([], _InstMap0, _MiscInfo, CanFail, MaxSolns, [], Detism, []) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_switch([Case0 | Cases0], InstMap0, MiscInfo, CanFail0, MaxSolns0,
		[Case | Cases], Detism, Msgs) :-
	% Technically, we should update the instmap to reflect the
	% knowledge that the var is bound to this particular
	% constructor, but we wouldn't use that information here anyway,
	% so we don't bother.
	Case0 = case(ConsId, Goal0),
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Detism1,
		Msgs1),
	Case = case(ConsId, Goal),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_switch_canfail(CanFail0, CanFail1, CanFail2),
	det_switch_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_switch(Cases0, InstMap0, MiscInfo, CanFail2, MaxSolns2,
		Cases, Detism, Msgs2),
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

det_conjunction_maxsoln(at_most_zero, at_most_zero, at_most_zero).
det_conjunction_maxsoln(at_most_zero, at_most_one,  at_most_zero).
det_conjunction_maxsoln(at_most_zero, at_most_many, at_most_zero).
det_conjunction_maxsoln(at_most_one,  at_most_zero, at_most_zero).
det_conjunction_maxsoln(at_most_one,  at_most_one,  at_most_one).
det_conjunction_maxsoln(at_most_one,  at_most_many, at_most_many).
det_conjunction_maxsoln(at_most_many, at_most_zero, at_most_zero).
det_conjunction_maxsoln(at_most_many, at_most_one,  at_most_many).
det_conjunction_maxsoln(at_most_many, at_most_many, at_most_many).

det_conjunction_canfail(can_fail,    can_fail,    can_fail).
det_conjunction_canfail(can_fail,    cannot_fail, can_fail).
det_conjunction_canfail(cannot_fail, can_fail,    can_fail).
det_conjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

:- pred det_disjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_disjunction_maxsoln(in, in, out) is det.

det_disjunction_maxsoln(at_most_zero, at_most_zero, at_most_zero).
det_disjunction_maxsoln(at_most_zero, at_most_one,  at_most_one).
det_disjunction_maxsoln(at_most_zero, at_most_many, at_most_many).
det_disjunction_maxsoln(at_most_one,  at_most_zero, at_most_one).
det_disjunction_maxsoln(at_most_one,  at_most_one,  at_most_many).
det_disjunction_maxsoln(at_most_one,  at_most_many, at_most_many).
det_disjunction_maxsoln(at_most_many, at_most_zero, at_most_many).
det_disjunction_maxsoln(at_most_many, at_most_one,  at_most_many).
det_disjunction_maxsoln(at_most_many, at_most_many, at_most_many).

:- pred det_disjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_disjunction_canfail(in, in, out) is det.

det_disjunction_canfail(can_fail,    can_fail,    can_fail).
det_disjunction_canfail(can_fail,    cannot_fail, cannot_fail).
det_disjunction_canfail(cannot_fail, can_fail,    cannot_fail).
det_disjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

:- pred det_switch_maxsoln(soln_count, soln_count, soln_count).
:- mode det_switch_maxsoln(in, in, out) is det.

det_switch_maxsoln(at_most_zero, at_most_zero, at_most_zero).
det_switch_maxsoln(at_most_zero, at_most_one,  at_most_one).
det_switch_maxsoln(at_most_zero, at_most_many, at_most_many).
det_switch_maxsoln(at_most_one,  at_most_zero, at_most_one).
det_switch_maxsoln(at_most_one,  at_most_one,  at_most_one).
det_switch_maxsoln(at_most_one,  at_most_many, at_most_many).
det_switch_maxsoln(at_most_many, at_most_zero, at_most_many).
det_switch_maxsoln(at_most_many, at_most_one,  at_most_many).
det_switch_maxsoln(at_most_many, at_most_many, at_most_many).

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
det_negation_det(erroneous,	yes(erroneous)).
det_negation_det(failure,	yes(det)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred global_checking_pass(module_info, predproclist, module_info,
	io__state, io__state).
:- mode global_checking_pass(in, in, out, di, uo) is det.

global_checking_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	global_analysis_single_pass(ProcList, ModuleInfo0, ModuleInfo1,
		unchanged, _),
	global_checking_pass_2(ProcList, ModuleInfo1, ModuleInfo).

:- pred global_checking_pass_2(predproclist, module_info, module_info,
	io__state, io__state).
:- mode global_checking_pass_2(in, in, out, di, uo) is det.

global_checking_pass_2([], ModuleInfo, ModuleInfo) --> [].
global_checking_pass_2([PredId - ModeId | Rest], ModuleInfo0, ModuleInfo) -->
	{
		module_info_preds(ModuleInfo0, PredTable),
		map__lookup(PredTable, PredId, PredInfo),
		pred_info_procedures(PredInfo, ProcTable),
		map__lookup(ProcTable, ModeId, ProcInfo),
		proc_info_declared_determinism(ProcInfo, MaybeDetism),
		proc_info_inferred_determinism(ProcInfo, InferredDetism)
	},
	(
		{ MaybeDetism = no },
		{ ModuleInfo1 = ModuleInfo0 }
	;
		{ MaybeDetism = yes(DeclaredDetism) },
		{ compare_determinisms(DeclaredDetism, InferredDetism, Cmp) },
		(
			{ Cmp = sameas },
			{ ModuleInfo1 = ModuleInfo0 }
		;
			{ Cmp = looser },
			globals__io_lookup_bool_option(
				warn_det_decls_too_lax,
				ShouldIssueWarning),
			( { ShouldIssueWarning = yes } ->
				{ Message = "  Warning: determinism declaration could be tighter.\n" },
				report_determinism_problem(PredId,
					ModeId, ModuleInfo0, Message,
					DeclaredDetism, InferredDetism)
			;
				[]
			),
			{ ModuleInfo1 = ModuleInfo0 }
		;
			{ Cmp = tighter },
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo1) },
			{ Message = "  Error: determinism declaration not satisfied.\n" },
			report_determinism_problem(PredId,
				ModeId, ModuleInfo1, Message,
				DeclaredDetism, InferredDetism),
			{ proc_info_goal(ProcInfo, Goal) },
			{ MiscInfo = misc_info(ModuleInfo1, PredId, ModeId) },
			det_diagnose_goal(Goal, DeclaredDetism, [], MiscInfo, _)
			% XXX with the right verbosity options, we want to
			% call report_determinism_problem only if diagnose
			% returns false, i.e. it didn't print a message.
		)
	),
	% check that `main/2' cannot fail
	( 
		{ pred_info_name(PredInfo, "main") },
		{ pred_info_arity(PredInfo, 2) },
		{ pred_info_is_exported(PredInfo) },
		{
		  determinism_components(InferredDetism, can_fail, _)
		;
		  MaybeDetism = yes(DeclaredDeterminism),
		  determinism_components(DeclaredDeterminism, can_fail, _)
		}
	->
		{ proc_info_context(ProcInfo, Context) },
		prog_out__write_context(Context),
			% The error message is actually a lie -
			% main/2 can also be `erroneous'.  But mentioning
			% that would probably just confuse people.
		io__write_string(
			"Error: main/2 must be `det' or `multidet'.\n"),
		{ module_info_incr_errors(ModuleInfo1,
			ModuleInfo2) }
	;
		{ ModuleInfo2 = ModuleInfo1 }
	),
	global_checking_pass_2(Rest, ModuleInfo2, ModuleInfo).

:- pred report_determinism_problem(pred_id, proc_id, module_info, string,
	determinism, determinism, io__state, io__state).
:- mode report_determinism_problem(in, in, in, in, in, in, di, uo) is det.

report_determinism_problem(PredId, ModeId, ModuleInfo, Message,
		DeclaredDetism, InferredDetism) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ModeId, ProcInfo) },
	{ proc_info_context(ProcInfo, Context) },
	{ proc_info_argmodes(ProcInfo, ArgModes0) },

	( globals__io_lookup_bool_option( halt_at_warn, yes) ->
		 io__set_exit_status(1)
	;
		[]
	),

	% We need to strip off the extra type_info arguments inserted at the
	% front by polymorphism.m - we only want the last `PredArity' of them.
	%
	{ list__length(ArgModes0, NumArgModes) },
	{ NumToDrop is NumArgModes - Arity },
	( { list__drop(NumToDrop, ArgModes0, ArgModes1) } ->
		{ ArgModes = ArgModes1 }
	;	
		{ error("report_determinism_problem: list__drop failed") }
	),

	prog_out__write_context(Context),
	io__write_string("In `"),
	det_report_pred_name_mode(PredName, ArgModes),
	io__write_string("':\n"),

	prog_out__write_context(Context),
	io__write_string(Message),
	prog_out__write_context(Context),
	io__write_string("  Declared `"),
	hlds_out__write_determinism(DeclaredDetism),
	io__write_string("', inferred `"),
	hlds_out__write_determinism(InferredDetism),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- type det_comparison	--->	tighter ; sameas ; looser.

:- pred compare_determinisms(determinism, determinism, det_comparison).
:- mode compare_determinisms(in, in, out) is det.

compare_determinisms(DeclaredDetism, InferredDetism, CmpDetism) :-
	determinism_components(DeclaredDetism, DeclaredCanFail, DeclaredSolns),
	determinism_components(InferredDetism, InferredCanFail, InferredSolns),
	compare_canfails(DeclaredCanFail, InferredCanFail, CmpCanFail),
	compare_solncounts(DeclaredSolns, InferredSolns, CmpSolns),

	% We can get e.g. tighter canfail and looser solncount
	% e.g. for a predicate declared multidet and inferred semidet.
	% Therefore the ordering of the following two tests is important:
	% we want errors to take precedence over warnings.

	( ( CmpCanFail = tighter ; CmpSolns = tighter ) ->
		CmpDetism = tighter
	; ( CmpCanFail = looser ; CmpSolns = looser ) ->
		CmpDetism = looser
	;
		CmpDetism = sameas
	).

:- pred compare_canfails(can_fail, can_fail, det_comparison).
:- mode compare_canfails(in, in, out) is det.

compare_canfails(cannot_fail, cannot_fail, sameas).
compare_canfails(cannot_fail, can_fail,    tighter).
compare_canfails(can_fail,    cannot_fail, looser).
compare_canfails(can_fail,    can_fail,    sameas).

:- pred compare_solncounts(soln_count, soln_count, det_comparison).
:- mode compare_solncounts(in, in, out) is det.

compare_solncounts(at_most_zero, at_most_zero, sameas).
compare_solncounts(at_most_zero, at_most_one,  tighter).
compare_solncounts(at_most_zero, at_most_many, tighter).
compare_solncounts(at_most_one,  at_most_zero, looser).
compare_solncounts(at_most_one,  at_most_one,  sameas).
compare_solncounts(at_most_one,  at_most_many, tighter).
compare_solncounts(at_most_many, at_most_zero, looser).
compare_solncounts(at_most_many, at_most_one,  looser).
compare_solncounts(at_most_many, at_most_many, sameas).

%-----------------------------------------------------------------------------%

:- type switch_context --->	switch_context(var, cons_id).

:- pred det_diagnose_write_switch_context(term__context, list(switch_context),
	misc_info, io__state, io__state).
:- mode det_diagnose_write_switch_context(in, in, in, di, uo) is det.

det_diagnose_write_switch_context(_Context, [], _MiscInco) --> [].
det_diagnose_write_switch_context(Context, [SwitchContext | SwitchContexts],
		MiscInfo) -->
	prog_out__write_context(Context),
	{ det_misc_get_proc_info(MiscInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	{ SwitchContext = switch_context(Var, ConsId) },
	io__write_string("  Inside the case "),
	hlds_out__write_cons_id(ConsId),
	io__write_string(" of the switch on "),
	mercury_output_var(Var, Varset),
	io__write_string(":\n"),
	det_diagnose_write_switch_context(Context, SwitchContexts, MiscInfo).

%-----------------------------------------------------------------------------%

	% The given goal should have determinism Desired, but doesn't.
	% Find out what is wrong and print a report of the cause.

:- pred det_diagnose_goal(hlds__goal, determinism, list(switch_context),
	misc_info, bool, io__state, io__state).
:- mode det_diagnose_goal(in, in, in, in, out, di, uo) is det.

det_diagnose_goal(Goal - GoalInfo, Desired, SwitchContext, MiscInfo,
		Diagnosed) -->
	{ goal_info_get_determinism(GoalInfo, Actual) },
	( { compare_determinisms(Desired, Actual, tighter) } ->
		det_diagnose_goal_2(Goal, GoalInfo, Desired, Actual,
			SwitchContext, MiscInfo, Diagnosed)
	;
		{ Diagnosed = no }
	).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_2(hlds__goal_expr, hlds__goal_info,
	determinism, determinism, list(switch_context), misc_info, bool,
	io__state, io__state).
:- mode det_diagnose_goal_2(in, in, in, in, in, in, out, di, uo) is det.

det_diagnose_goal_2(conj(Goals), _GoalInfo, Desired, _Actual, Context, MiscInfo,
		Diagnosed) -->
	det_diagnose_conj(Goals, Desired, Context, MiscInfo, Diagnosed).

det_diagnose_goal_2(disj(Goals), GoalInfo, Desired, _Actual, SwitchContext,
		MiscInfo, Diagnosed) -->
	det_diagnose_disj(Goals, Desired, SwitchContext, MiscInfo, 0, Clauses,
		Diagnosed1),
	{ determinism_components(Desired, _, DesSolns) },
	(
		{ DesSolns \= at_most_many },
		{ Clauses > 1 }
	->
		{ goal_info_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  Disjunction has multiple clauses with solutions.\n"),
		{ Diagnosed = yes }
	;
		{ Diagnosed = Diagnosed1 }
	).

	% The determinism of a switch is the worst of the determinism of each of
	% the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the CanFail field.

det_diagnose_goal_2(switch(Var, SwitchCanFail, Cases), GoalInfo,
		Desired, _Actual, SwitchContext, MiscInfo, Diagnosed) -->
	(
		{ SwitchCanFail = can_fail },
		{ determinism_components(Desired, cannot_fail, _) }
	->
		{ goal_info_context(GoalInfo, Context) },
		det_diagnose_write_switch_context(Context, SwitchContext,
			MiscInfo),
		prog_out__write_context(Context),
		{ det_misc_get_proc_info(MiscInfo, ProcInfo) },
		{ proc_info_variables(ProcInfo, Varset) },
		{ MiscInfo = misc_info(ModuleInfo, _, _) },
		(
			{ det_lookup_var_type(ModuleInfo, ProcInfo, Var,
				TypeDefn) },
			{ TypeDefn = hlds__type_defn(_, _, TypeBody, _, _) },
			{ TypeBody = du_type(_, ConsTable, _) }
		->
			{ map__keys(ConsTable, ConsIds) },
			{ det_diagnose_missing_consids(ConsIds, Cases,
				Missing) },
			io__write_string("  The switch on "),
			mercury_output_var(Var, Varset),
			io__write_string(" does not cover "),
			det_output_consid_list(Missing, yes),
			io__write_string(".\n")
		;
			io__write_string("  The switch on "),
			mercury_output_var(Var, Varset),
			io__write_string(" can fail.\n")
		),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext, MiscInfo,
		Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

det_diagnose_goal_2(call(PredId, ModeId, _, _, CallContext, _, _), GoalInfo,
		Desired, Actual, _, MiscInfo, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	{ compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail) },
	( { CmpCanFail = tighter } ->
		det_report_call_context(Context, CallContext, MiscInfo,
			PredId, ModeId),
		io__write_string("can fail.\n"),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	{ compare_solncounts(DesiredSolns, ActualSolns, CmpSolns) },
	( { CmpSolns = tighter } ->
		det_report_call_context(Context, CallContext, MiscInfo,
			PredId, ModeId),
		io__write_string("can succeed"),
		( { DesiredSolns = at_most_one } ->
			io__write_string(" more than once\n.")
		;
			io__write_string(".\n")
		),
		{ Diagnosed2 = yes }
	;
		{ Diagnosed2 = no }
	),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) },
	(
		{ Diagnosed = yes }
	;
		{ Diagnosed = no },
		det_report_call_context(Context, CallContext, MiscInfo,
			PredId, ModeId),
		io__write_string("has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(unify(LT, RT, _, _, UnifyContext), GoalInfo,
		Desired, Actual, _, MiscInfo, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, _DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, _ActualSolns) },
	det_report_unify_context(Context, UnifyContext, MiscInfo, LT, RT),
	(
		{ DesiredCanFail = cannot_fail },
		{ ActualCanFail = can_fail }
	->
		io__write_string("can fail.\n")
	;
		io__write_string("has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(if_then_else(_Vars, Cond, Then, Else), _GoalInfo,
		Desired, _Actual, SwitchContext, MiscInfo, Diagnosed) -->
	{
		determinism_components(Desired, _DesiredCanFail, DesiredSolns),
		Cond = _CondGoal - CondInfo,
		goal_info_get_determinism(CondInfo, CondDetism),
		determinism_components(CondDetism, _CondCanFail, CondSolns)
	},
	(
		{ CondSolns = at_most_many },
		{ DesiredSolns \= at_most_many }
	->
		{ determinism_components(DesiredCond, can_fail, DesiredSolns) },
		det_diagnose_goal(Cond, DesiredCond, SwitchContext, MiscInfo,
			Diagnosed1)
	;
		{ Diagnosed1 = no }
	),
	det_diagnose_goal(Then, Desired, SwitchContext, MiscInfo, Diagnosed2),
	det_diagnose_goal(Else, Desired, SwitchContext, MiscInfo, Diagnosed3),
	{ bool__or(Diagnosed2, Diagnosed3, Diagnosed23) },
	{ bool__or(Diagnosed1, Diagnosed23, Diagnosed) }.

det_diagnose_goal_2(not(_), GoalInfo, _, _, _, _, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  It should be impossible to get a determinism error\n"),
	prog_out__write_context(Context),
	io__write_string("  with a negated goal that stays a negation.\n").

det_diagnose_goal_2(some(_Vars, Goal), _, Desired, Actual,
		SwitchContext, MiscInfo, Diagnosed) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_determinism(GoalInfo, Internal) },
	{ Actual = Internal ->
		InternalDesired = Desired
	;
		determinism_components(Desired, CanFail, _),
		determinism_components(InternalDesired, CanFail, at_most_many)
	},
	det_diagnose_goal(Goal, InternalDesired, SwitchContext, MiscInfo,
		Diagnosed).

det_diagnose_goal_2(pragma_c_code(_, _, _, _, _), GoalInfo, Desired, 
		_, _, _, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  Determinism declaration not satisfied. Desired \n"),
	prog_out__write_context(Context),
	io__write_string("  determinism is "),
	hlds_out__write_determinism(Desired),
	io__write_string(".\n"),
	prog_out__write_context(Context),
	io__write_string("  pragma(c_code, ...) declarations only allowed\n"),
	prog_out__write_context(Context),
	io__write_string("  for deterministic modes.\n").

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(term__context, maybe(call_unify_context),
	misc_info, pred_id, proc_id, io__state, io__state).
:- mode det_report_call_context(in, in, in, in, in, di, uo) is det.

det_report_call_context(Context, CallUnifyContext, MiscInfo, PredId, ModeId) -->
	(
		{ CallUnifyContext = yes(call_unify_context(LT, RT, UC)) },
		det_report_unify_context(Context, UC, MiscInfo, LT, RT)
	;
		{ CallUnifyContext = no },
		{ MiscInfo = misc_info(ModuleInfo, _, _) },
		{ module_info_preds(ModuleInfo, PredTable) },
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ map__lookup(PredTable, PredId, PredInfo) },
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ map__lookup(ProcTable, ModeId, ProcInfo) },
		{ proc_info_argmodes(ProcInfo, ArgModes) },
		prog_out__write_context(Context),
		io__write_string("  Call to `"),
		det_report_pred_name_mode(PredName, ArgModes),
		io__write_string("' ")
	).

:- pred det_report_unify_context(term__context, unify_context,
	misc_info, var, unify_rhs, io__state, io__state).
:- mode det_report_unify_context(in, in, in, in, in, di, uo) is det.

det_report_unify_context(Context, UnifyContext, MiscInfo, LT, RT) -->
	hlds_out__write_unify_context(UnifyContext, Context),
	prog_out__write_context(Context),
	{ det_misc_get_proc_info(MiscInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	{ MiscInfo = misc_info(ModuleInfo, _, _) },
	(
		{ varset__lookup_name(Varset, LT, _) }
	->
		(
			{ RT = var(RV) },
			\+ { varset__lookup_name(Varset, RV, _) }
		->
			io__write_string("  unification with `"),
			mercury_output_var(LT, Varset)
		;
			io__write_string("  unification of `"),
			mercury_output_var(LT, Varset),
			io__write_string("' and `"),
			hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, 3)
		)
	;
		io__write_string("  unification with `"),
		hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, 3)
	),
	io__write_string("' ").

:- pred det_report_pred_name_mode(string, list((mode)), io__state, io__state).
:- mode det_report_pred_name_mode(in, in, di, uo) is det.

det_report_pred_name_mode(PredName, ArgModes) -->
	io__write_string(PredName),
	( { ArgModes \= [] } ->
		{ varset__init(InstVarSet) },	% XXX inst var names
		io__write_string("("),
		mercury_output_mode_list(ArgModes, InstVarSet),
		io__write_string(")")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_conj(list(hlds__goal), determinism,
	list(switch_context), misc_info, bool, io__state, io__state).
:- mode det_diagnose_conj(in, in, in, in, out, di, uo) is det.

det_diagnose_conj([], _Desired, _SwitchContext, _MiscInfo, no) --> [].
det_diagnose_conj([Goal | Goals], Desired, SwitchContext, MiscInfo,
		Diagnosed) -->
	det_diagnose_goal(Goal, Desired, SwitchContext, MiscInfo, Diagnosed1),
	det_diagnose_conj(Goals, Desired, SwitchContext, MiscInfo, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_disj(list(hlds__goal), determinism,
	list(switch_context), misc_info, int, int, bool, io__state, io__state).
:- mode det_diagnose_disj(in, in, in, in, in, out, out, di, uo) is det.

det_diagnose_disj([], _Desired, _SwitchContext, _MiscInfo,
		Clauses, Clauses, no) --> [].
det_diagnose_disj([Goal | Goals], Desired, SwitchContext, MiscInfo,
		Clauses0, Clauses, Diagnosed) -->
	{ determinism_components(Desired, _, DesiredSolns) },
	{ determinism_components(ClauseDesired, can_fail, DesiredSolns) },
	det_diagnose_goal(Goal, ClauseDesired, SwitchContext, MiscInfo,
		Diagnosed1),
	{ Clauses1 is Clauses0 + 1 },
	det_diagnose_disj(Goals, Desired, SwitchContext, MiscInfo,
		Clauses1, Clauses, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_switch(var, list(case), determinism,
	list(switch_context), misc_info, bool, io__state, io__state).
:- mode det_diagnose_switch(in, in, in, in, in, out, di, uo) is det.

det_diagnose_switch(_Var, [], _Desired, _SwitchContext, _MiscInfo, no) --> [].
det_diagnose_switch(Var, [case(ConsId, Goal) | Cases], Desired,
		SwitchContext0, MiscInfo, Diagnosed) -->
	{ SwitchContext1 = [switch_context(Var, ConsId) | SwitchContext0] },
	det_diagnose_goal(Goal, Desired, SwitchContext1, MiscInfo, Diagnosed1),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext0, MiscInfo,
		Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

%-----------------------------------------------------------------------------%

	% det_lookup_detism(MiscInfo, PredId, ModeId, Category):
	% 	Given the MiscInfo, and the PredId & ModeId of a procedure,
	% 	look up the determinism of that procedure and return it
	% 	in Category.

:- pred det_lookup_detism(misc_info, pred_id, proc_id, determinism).
:- mode det_lookup_detism(in, in, in, out) is det.

det_lookup_detism(MiscInfo, PredId, ModeId, Detism) :-
	MiscInfo = misc_info(ModuleInfo, _, _),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo),
	proc_info_interface_determinism(ProcInfo, Detism).

:- pred det_misc_get_proc_info(misc_info, proc_info).
:- mode det_misc_get_proc_info(in, out) is det.

det_misc_get_proc_info(MiscInfo, ProcInfo) :-
	MiscInfo = misc_info(ModuleInfo, PredId, ModeId),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo).

:- pred det_lookup_var_type(module_info, proc_info, var, hlds__type_defn).
:- mode det_lookup_var_type(in, in, in, out) is semidet.

det_lookup_var_type(ModuleInfo, ProcInfo, Var, TypeDefn) :-
	proc_info_vartypes(ProcInfo, VarTypes),
	map__lookup(VarTypes, Var, Type),
	(
		type_to_type_id(Type, TypeId, _)
	->
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeId, TypeDefn)
	;
		error("cannot lookup the type of a variable")
	).

:- pred no_output_vars(set(var), instmap, instmap_delta, misc_info).
:- mode no_output_vars(in, in, in, in) is semidet.

no_output_vars(_, _, unreachable, _).
no_output_vars(Vars, InstMap0, reachable(InstMapDelta), MiscInfo) :-
	set__to_sorted_list(Vars, VarList),
	MiscInfo = misc_info(ModuleInfo, _, _),
	no_output_vars_2(VarList, InstMap0, InstMapDelta, ModuleInfo).

:- pred no_output_vars_2(list(var), instmap, instmapping, module_info).
:- mode no_output_vars_2(in, in, in, in) is semidet.

no_output_vars_2([], _, _, _).
no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, ModuleInfo) :-
	( map__search(InstMapDelta, Var, Inst) ->
		% The instmap delta contains the variable, but the variable may
		% still not be output, if the change is just an increase in
		% information rather than an increase in instantiatedness.
		% We use `inst_matches_final' to check that the new inst
		% has only added information, not bound anything.
		instmap_lookup_var(InstMap0, Var, Inst0),
		inst_matches_final(Inst, Inst0, ModuleInfo)
	;
		true
	),
	no_output_vars_2(Vars, InstMap0, InstMapDelta, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_missing_consids(list(cons_id), list(case), list(cons_id)).
:- mode det_diagnose_missing_consids(in, in, out) is det.

det_diagnose_missing_consids([], _, []).
det_diagnose_missing_consids([ConsId | ConsIds], Cases, Missing) :-
	det_diagnose_missing_consids(ConsIds, Cases, Missing0),
	(
		list__member(Case, Cases),
		Case = case(ConsId, _)
	->
		Missing = Missing0
	;
		Missing = [ConsId | Missing0]
	).

:- pred det_output_consid_list(list(cons_id), bool, io__state, io__state).
:- mode det_output_consid_list(in, in, di, uo) is det.

det_output_consid_list([], _) --> [].
det_output_consid_list([ConsId | ConsIds], First) -->
	( { First = yes } ->
		[]
	; { ConsIds = [] } ->
		io__write_string(" and ")
	;
		io__write_string(", ")
	),
	hlds_out__write_cons_id(ConsId),
	det_output_consid_list(ConsIds, no).

%-----------------------------------------------------------------------------%

:- pred det_report_msgs(list(msg), io__state, io__state).
:- mode det_report_msgs(in, di, uo) is det.

det_report_msgs([]) --> [].
det_report_msgs([Msg | Msgs]) -->
	det_report_msg(Msg),
	det_report_msgs(Msgs).

:- pred det_report_msg(msg, io__state, io__state).
:- mode det_report_msg(in, di, uo) is det.

det_report_msg(multidet_disj(GoalInfo, Disjuncts0)) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("The disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("has no outputs, but can succeed more than once.\n").
det_report_msg(det_disj(GoalInfo, Disjuncts0)) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("The disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("will succeed exactly once.\n").
det_report_msg(semidet_disj(GoalInfo, Disjuncts0)) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("The disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("is semidet, yet it has an output.\n").
det_report_msg(zero_soln_disj(GoalInfo, Disjuncts0)) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("The disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("cannot succeed.\n").
det_report_msg(zero_soln_disjunct(GoalInfo)) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("This disjunct will never have any solutions.\n").

	% Insertion sort is good enough.

:- pred det_report_sort_context_lines(list(hlds__goal), list(hlds__goal)).
:- mode det_report_sort_context_lines(in, out) is det.

det_report_sort_context_lines([], []).
det_report_sort_context_lines([Goal0 | Goals0], Goals) :-
	det_report_sort_context_lines(Goals0, Goals1),
	det_report_insert_context_line(Goals1, Goal0, Goals).

:- pred det_report_insert_context_line(list(hlds__goal), hlds__goal,
	list(hlds__goal)).
:- mode det_report_insert_context_line(in, in, out) is det.

det_report_insert_context_line([], Goal, [Goal]).
det_report_insert_context_line([Goal0 | Goals0], Goal, Goals) :-
	Goal0 = _ - GoalInfo0,
	goal_info_context(GoalInfo0, Context0),
	term__context_line(Context0, Line0),
	Goal = _ - GoalInfo,
	goal_info_context(GoalInfo, Context),
	term__context_line(Context, Line),
	( Line < Line0 ->
		Goals = [Goal, Goal0 | Goals0]
	;
		det_report_insert_context_line(Goals0, Goal, Goals1),
		Goals = [Goal0 | Goals1]
	).

:- pred det_report_context_lines(list(hlds__goal), bool, io__state, io__state).
:- mode det_report_context_lines(in, in, di, uo) is det.

det_report_context_lines([], _) --> [].
det_report_context_lines([_ - GoalInfo | Goals], First) -->
	{ goal_info_context(GoalInfo, Context) },
	{ term__context_line(Context, Line) },
	( { First = yes } ->
		[]
	; { Goals = [] } ->
		io__write_string(" and ")
	;
		io__write_string(", ")
	),
	io__write_int(Line),
	det_report_context_lines(Goals, no).

%-----------------------------------------------------------------------------%
