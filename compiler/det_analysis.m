%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_analysis.m - the determinism analysis pass.

% Main authors: conway, fjh, zs.

% This pass has three components:
%	o Segregate the procedures into those that have determinism
%		declarations, and those that don't
%	o A step of performing a local analysis pass on each procedure
%		without a determinism declaration is iterated until
%		a fixpoint is reached
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

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, prog_io, prog_out, hlds_out, std_util.
:- import_module globals, options, io, mercury_to_mercury, varset.
:- import_module mode_util, inst_match.

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
	( pred_info_is_imported(Pred) ->
		PredProcs1 = PredProcs0
	;
		pred_info_procids(Pred, ProcIds),
		fold_pred_modes(PredId, ProcIds, PredProcs0, PredProcs1)
	),
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

	% Iterate until a fixpoint is reached

global_analysis_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "."),
	maybe_flush_output(Verbose),
	{ global_analysis_single_pass(ModuleInfo0, ProcList, unchanged,
			ModuleInfo1, Changed) },
	( { Changed = changed } ->
		global_analysis_pass(ModuleInfo1, ProcList, ModuleInfo)
	;
		{ ModuleInfo = ModuleInfo1 }
	).

:- type maybe_changed ---> changed ; unchanged.

:- pred global_analysis_single_pass(module_info, predproclist, maybe_changed,
				module_info, maybe_changed).
:- mode global_analysis_single_pass(in, in, in, out, out) is det.

:- global_analysis_single_pass(_, A, _, _, _) when A.	% NU-Prolog indexing.

global_analysis_single_pass(ModuleInfo, [], Changed, ModuleInfo, Changed).
global_analysis_single_pass(ModuleInfo0, [PredId - PredMode|PredProcs], State0,
		ModuleInfo, State) :-
	det_infer_proc(ModuleInfo0, PredId, PredMode, State0,
			ModuleInfo1, State1),
	global_analysis_single_pass(ModuleInfo1, PredProcs, State1,
			ModuleInfo, State).

%-----------------------------------------------------------------------------%

	% Infer the determinism of a procedure.

:- pred det_infer_proc(module_info, pred_id, proc_id, maybe_changed,
				module_info, maybe_changed).
:- mode det_infer_proc(in, in, in, in, out, out) is det.

det_infer_proc(ModuleInfo0, PredId, PredMode, State0, ModuleInfo, State) :-
		% Get the proc_info structure for this procedure
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, Pred0),
	pred_info_procedures(Pred0, Procs0),
	map__lookup(Procs0, PredMode, Proc0),

		% Remember the old inferred determinism of this procedure
		% XXX zs: make sure this is initailzed right
	proc_info_inferred_determinism(Proc0, Detism0),

		% Infer the determinism of the goal
	proc_info_goal(Proc0, Goal0),
	proc_info_get_initial_instmap(Proc0, ModuleInfo0, InstMap0),
	MiscInfo = misc_info(ModuleInfo0, PredId, PredMode),
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Detism),

		% Check whether the determinism of this procedure changed
	(
		Detism = Detism0
	->
		State = State0
	;
		State = changed
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
			hlds__goal, instmap, determinism).
:- mode det_infer_goal(in, in, in, out, out, out) is det.

det_infer_goal(Goal0 - GoalInfo0, InstMap0, MiscInfo,
		Goal - GoalInfo, InstMap, Detism) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
	apply_instmap_delta(InstMap0, DeltaInstMap, InstMap),
	det_infer_goal_2(Goal0, InstMap0, MiscInfo, NonLocalVars, DeltaInstMap,
		Goal, InternalDetism),

	% If a goal with possibly multiple solutions doesn't have any
	% output variables, then we make it succeed at most once.
	% By setting the InternalDetism different from the external Detism
	% we tell the code generator to generate a commit after the goal.
	determinism_components(InternalDetism, CanFail, InternalSolns),
	(
		InternalSolns = at_most_many,
		no_output_vars(NonLocalVars, InstMap0, DeltaInstMap, MiscInfo)
	->
		determinism_components(Detism, CanFail, at_most_one)
	;
		Detism = InternalDetism
	),

	% XXX if Detism = failure, we could replace the Goal with fail.
	% We don't do this yet because we might optimize away calls to error.

	goal_info_set_internal_determinism(GoalInfo0, InternalDetism,
		GoalInfo1),
	goal_info_set_determinism(GoalInfo1, Detism, GoalInfo).

:- pred det_infer_goal_2(hlds__goal_expr, instmap, misc_info, set(var),
				instmap_delta, hlds__goal_expr, determinism).
:- mode det_infer_goal_2(in, in, in, in, in, out, out) is det.

	% the determinism of a conjunction is the worst case of the elements
	% of that conjuction.
det_infer_goal_2(conj(Goals0), InstMap0, MiscInfo, _, _, conj(Goals), Detism) :-
	( Goals0 = [SingleGoal0] ->
		% a singleton conjunction is equivalent to the goal itself
		det_infer_goal(SingleGoal0, InstMap0, MiscInfo,
				SingleGoal, _InstMap, Detism),
		Goals = [SingleGoal]
	;
		det_infer_conj(Goals0, InstMap0, MiscInfo,
			cannot_fail, at_most_one, Goals, Detism)
	).

det_infer_goal_2(disj(Goals0), InstMap0, MiscInfo, _, _, disj(Goals), Detism) :-
	( Goals0 = [SingleGoal0] ->
		% a singleton disjunction is equivalent to the goal itself
		det_infer_goal(SingleGoal0, InstMap0, MiscInfo,
				SingleGoal, _InstMap, Detism),
		Goals = [SingleGoal]
	;
		% an empty disjunction is equivalent to `fail',
		% but det_infer_disj will discover this fact.
		det_infer_disj(Goals0, InstMap0, MiscInfo,
			can_fail, at_most_zero, Goals, Detism)
	).

	% the determinism of a switch is the worst of the determinism of each of
	% the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the LocalDet field.

det_infer_goal_2(switch(Var, SwitchCanFail, Cases0), InstMap0, MiscInfo, _, _,
		switch(Var, SwitchCanFail, Cases), Detism) :-
	det_infer_switch(Cases0, InstMap0, MiscInfo, cannot_fail, at_most_zero,
		Cases, CasesDetism),
	determinism_components(CasesDetism, CasesCanFail, CasesSolns),
	det_conjunction_canfail(SwitchCanFail, CasesCanFail, CanFail),
	determinism_components(Detism, CanFail, CasesSolns).

	% look up the determinism entry associated with the call.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.
	%
	% Note that it _might_ be a good idea to record a list
	% of dependencies, so that we avoid recomputing the determinism
	% of clauses when none of the predicates they call changed
	% determinism.  But let's wait until this part of the
	% compilation becomes a bottleneck before worrying about
	% this.

det_infer_goal_2(call(PredId, ModeId, Args, BuiltIn, Name, Follow),
						_, MiscInfo, _, _,
		call(PredId, ModeId, Args, BuiltIn, Name, Follow), Detism) :-
	det_lookup_detism(MiscInfo, PredId, ModeId, Detism).

	% unifications are either deterministic or semideterministic.
	% (see det_infer_unify).
det_infer_goal_2(unify(LT, RT, M, U, C), _, MiscInfo, _, _,
		unify(LT, RT, M, U, C), D) :-
	det_infer_unify(U, MiscInfo, D).

	% Question: should we warn about if-then-elses with deterministic
	% and erroneous conditions?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(if_then_else(Vars, Cond0, Then0, Else0), InstMap0, MiscInfo,
		NonLocalVars, DeltaInstMap,
		Goal, Detism) :-
	det_infer_goal(Cond0, InstMap0, MiscInfo, Cond, InstMap1, CondDetism),
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
		det_infer_goal_2(conj(List), InstMap0, MiscInfo,
			NonLocalVars, DeltaInstMap, Goal, Detism)
	; CondSolns = at_most_zero ->
		% Optimize away the condition and the `then' part.
		% XXX We could give a warning if the condition
		% contains a (possibly indirect) call to error.
		Else0 = ElseGoal0 - _,
		det_infer_goal_2(ElseGoal0, InstMap0, MiscInfo,
			NonLocalVars, DeltaInstMap, Goal, Detism)
	;
		det_infer_goal(Then0, InstMap1, MiscInfo, Then, _, ThenDetism),
		det_infer_goal(Else0, InstMap0, MiscInfo, Else, _, ElseDetism),
		Goal = if_then_else(Vars, Cond, Then, Else),
		determinism_components(ThenDetism, ThenCanFail, ThenSolns),
		determinism_components(ElseDetism, ElseCanFail, ElseSolns),
		det_conjunction_maxsoln(CondSolns, ThenSolns, AllThenSolns),
		det_switch_maxsoln(AllThenSolns, ElseSolns, Solns),
		det_switch_canfail(ThenCanFail, ElseCanFail, CanFail),
		determinism_components(Detism, CanFail, Solns)
	).

	% Negations are always semideterministic.  It is an error for
	% a negation to further instantiate any non-local variable. Such
	% errors will be reported by the mode analysis.
	%
	% Question: should we warn about and/or optimize the negation of a
	% deterministic goal (which will always fail) here?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(not(Goal0), InstMap0, MiscInfo, _, _,
		not(Goal), Det) :-
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, _Det1),
	Det = semidet.

	% explicit quantification isn't important, since we've already
	% stored the _information about variable scope in the goal_info.

:- pred det_infer_conj(list(hlds__goal), instmap, misc_info,
	can_fail, soln_count, list(hlds__goal), determinism).
:- mode det_infer_conj(in, in, in, in, in, out, out) is det.

det_infer_conj([], _InstMap0, _MiscInfo, CanFail, MaxSolns, [], Detism) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_conj([Goal0 | Goals0], InstMap0, MiscInfo, CanFail0, MaxSolns0,
		[Goal | Goals], Detism) :-
	% XXX We should look to see when we get to a not_reached point
	% and optimize away the remaining elements of the conjunction.
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, InstMap1, Detism1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_conjunction_canfail(CanFail0, CanFail1, CanFail2),
	det_conjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_conj(Goals0, InstMap1, MiscInfo, CanFail2, MaxSolns2,
		Goals, Detism).

:- pred det_infer_disj(list(hlds__goal), instmap, misc_info,
	can_fail, soln_count, list(hlds__goal), determinism).
:- mode det_infer_disj(in, in, in, in, in, out, out) is det.

det_infer_disj([], _InstMap0, _MiscInfo, CanFail, MaxSolns, [], Detism) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_disj([Goal0 | Goals0], InstMap0, MiscInfo, CanFail0, MaxSolns0,
		[Goal | Goals], Detism) :-
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Detism1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_disjunction_canfail(CanFail0, CanFail1, CanFail2),
	det_disjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_disj(Goals0, InstMap0, MiscInfo, CanFail2, MaxSolns2,
		Goals, Detism).

:- pred det_infer_switch(list(case), instmap, misc_info,
	can_fail, soln_count, list(case), determinism).
:- mode det_infer_switch(in, in, in, in, in, out, out) is det.

det_infer_switch([], _InstMap0, _MiscInfo, CanFail, MaxSolns, [], Detism) :-
	determinism_components(Detism, CanFail, MaxSolns).
det_infer_switch([Case0 | Cases0], InstMap0, MiscInfo, CanFail0, MaxSolns0,
		[Case | Cases], Detism) :-
	% Technically, we should update the instmap to reflect the
	% knowledge that the var is bound to this particular
	% constructor, but we wouldn't use that information here anyway,
	% so we don't bother.
	Case0 = case(ConsId, Goal0),
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Detism1),
	Case = case(ConsId, Goal),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_switch_canfail(CanFail0, CanFail1, CanFail2),
	det_switch_maxsoln(MaxSolns0, MaxSolns1, MaxSolns2),
	det_infer_switch(Cases0, InstMap0, MiscInfo, CanFail2, MaxSolns2,
		Cases, Detism).

:- pred det_infer_unify(unification, misc_info, determinism).
:- mode det_infer_unify(in, in, out) is det.

det_infer_unify(assign(_, _), _MiscInfo, det).

det_infer_unify(construct(_, _, _, _), _MiscInfo, det).

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

det_infer_unify(simple_test(_, _), _MiscInfo, semidet).

det_infer_unify(complicated_unify(_, CanFail, _), _MiscInfo, Detism) :-
	determinism_components(Detism, CanFail, at_most_one).

det_infer_goal_2(some(Vars, Goal0), InstMap0, MiscInfo, _, _,
			some(Vars, Goal), Det) :-
	det_infer_goal(Goal0, InstMap0, MiscInfo, Goal, _InstMap, Det).

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

% :- pred det_lookup_var_typedefn(module_info, proc_info, var, hlds__type_defn).
% :- mode det_lookup_var_typedefn(in, in, in, out) is det.
% 
% det_lookup_var_typedefn(ModuleInfo, ProcInfo, Var, TypeDefn) :-
%	proc_info_vartypes(ProcInfo, VarTypes),
%	map__lookup(VarTypes, Var, Type),
%	type_to_type_id(Type, TypeId, _),
%	module_info_types(ModuleInfo, TypeTable),
%	map__lookup(TypeTable, TypeId, TypeDefn).

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

:- pred det_conjunction_maxsoln(soln_count, soln_count, soln_count).
:- mode det_conjunction_maxsoln(in, in, out) is det.

det_conjunction_maxsoln(at_most_zero, at_most_zero, at_most_zero).
det_conjunction_maxsoln(at_most_zero, at_most_one,  at_most_zero).
det_conjunction_maxsoln(at_most_zero, at_most_many, at_most_zero).
det_conjunction_maxsoln(at_most_one,  at_most_zero, at_most_zero).
det_conjunction_maxsoln(at_most_one,  at_most_one,  at_most_one).
det_conjunction_maxsoln(at_most_one,  at_most_many, at_most_many).
det_conjunction_maxsoln(at_most_many, at_most_zero, at_most_zero).
det_conjunction_maxsoln(at_most_many, at_most_one,  at_most_many).
det_conjunction_maxsoln(at_most_many, at_most_many, at_most_many).

:- pred det_conjunction_canfail(can_fail, can_fail, can_fail).
:- mode det_conjunction_canfail(in, in, out) is det.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred global_checking_pass(module_info, predproclist, module_info,
				io__state, io__state).
:- mode global_checking_pass(in, in, out, di, uo) is det.

global_checking_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	{ global_analysis_single_pass(ModuleInfo0, ProcList, unchanged,
		ModuleInfo1, _) },
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
			{ Message = "  Error: determinism declaration not satisfied.\n" },
			report_determinism_problem(PredId,
				ModeId, ModuleInfo0, Message,
				DeclaredDetism, InferredDetism),
			{ module_info_incr_errors(ModuleInfo0,
				ModuleInfo1) }
			% XXX The error messages should say _why_
			% a determinism declaration is wrong.
		)
	),
	global_checking_pass_2(Rest, ModuleInfo1, ModuleInfo).

:- pred report_determinism_problem(pred_id, proc_id, module_info, string,
	determinism, determinism, io__state, io__state).
:- mode report_determinism_problem(in, in, in, in, in, in, di, uo) is det.

report_determinism_problem(PredId, ModeId, ModuleInfo, Message,
		DeclaredDetism, InferredDetism) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ModeId, ProcInfo) },
	{ proc_info_context(ProcInfo, Context) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },

	prog_out__write_context(Context),
	io__write_string("In `"),
	io__write_string(PredName),
	( { ArgModes \= [] } ->
		{ varset__init(InstVarSet) },	% XXX inst var names
		io__write_string("("),
		mercury_output_mode_list(ArgModes, InstVarSet),
		io__write_string(")")
	;
		[]
	),
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
