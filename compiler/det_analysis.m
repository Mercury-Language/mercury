%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% det_analysis.nl - the determinism analysis pass.

% Main authors: conway, fjh.

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

% XXX we should record errors by calling module_info_incr_errors

%-----------------------------------------------------------------------------%

:- module det_analysis.
:- interface.
:- import_module hlds.

:- pred determinism_pass(module_info, module_info, io__state, io__state).
:- mode determinism_pass(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, prog_io, prog_out, hlds_out, std_util.
:- import_module globals, options, io.

%-----------------------------------------------------------------------------%

determinism_pass(ModuleInfo0, ModuleInfo) -->
	{ determinism_declarations(ModuleInfo0, DeclaredProcs,
		UndeclaredProcs) },
	globals__lookup_option(verbose, bool(Verbose)),
	maybe_write_string(Verbose, "% Doing determinism analysis pass(es) "),
	maybe_flush_output(Verbose),
	global_analysis_pass(ModuleInfo0, UndeclaredProcs, ModuleInfo1),
	maybe_write_string(Verbose, " done\n"),
	maybe_write_string(Verbose, "% Doing determinism checking pass...\n"),
	maybe_flush_output(Verbose),
	global_checking_pass(ModuleInfo1, DeclaredProcs, ModuleInfo),
	maybe_write_string(Verbose, "% done\n").

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
	proc_info_declared_determinism(Proc, Category),
	(
		Category = unspecified
	->
		UndeclaredProcs1 = [PredId - PredMode|UndeclaredProcs0],
		DeclaredProcs1 = DeclaredProcs0
	;
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
	globals__lookup_option(verbose, bool(Verbose)),
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
	proc_info_inferred_determinism(Proc0, Detism0),

		% Infer the determinism of the goal
	proc_info_goal(Proc0, Goal0),
	MiscInfo = misc_info(ModuleInfo0, PredId, PredMode),
	det_infer_goal(Goal0, MiscInfo, Goal, Detism),

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

	% XXX The error messages should say _why_ a determinism declaration
	% is wrong.

:- pred global_checking_pass(module_info, predproclist, module_info,
				io__state, io__state).
:- mode global_checking_pass(in, in, out, di, uo) is det.

global_checking_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	{ global_analysis_single_pass(ModuleInfo0, ProcList, unchanged,
		ModuleInfo, _) },
	global_checking_pass_2(ProcList, ModuleInfo).

:- pred global_checking_pass_2(predproclist, module_info, io__state, io__state).
:- mode global_checking_pass_2(in, in, di, uo) is det.

global_checking_pass_2([], _ModuleInfo) --> [].
global_checking_pass_2([PredId - ModeId | Rest], ModuleInfo) -->
	{
	  module_info_preds(ModuleInfo, PredTable),
	  map__lookup(PredTable, PredId, PredInfo),
	  pred_info_procedures(PredInfo, ProcTable),
	  map__lookup(ProcTable, ModeId, ProcInfo),
	  proc_info_declared_determinism(ProcInfo, Detism),
	  determinism_to_category(Detism, DeclaredCategory),
	  proc_info_inferred_determinism(ProcInfo, InferredCategory)
	},
	( { DeclaredCategory = InferredCategory } ->
		[]
	;
		{ max_category(DeclaredCategory, InferredCategory, Category) },
		( { Category = DeclaredCategory } ->
			globals__lookup_option(warn_det_decls_too_lax,
				bool(ShouldIssueWarning)),
			( { ShouldIssueWarning = yes } ->
				report_determinism_warning(PredId, ModeId,
					InferredCategory, DeclaredCategory,
					ModuleInfo)
			;
				[]
			)
		;
			report_determinism_error(PredId, ModeId,
				InferredCategory, DeclaredCategory, ModuleInfo)
		)
	),
	global_checking_pass_2(Rest, ModuleInfo).

:- pred report_determinism_error(pred_id, proc_id, category, category,
				module_info, io__state, io__state).
:- mode report_determinism_error(in, in, in, in, in, di, uo) is det.

report_determinism_error(PredId, ModeId, Category, DeclaredCategory,
		ModuleInfo) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ModeId, ProcInfo) },
	{ proc_info_context(ProcInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Error: determinism declaration not satisfied.\n"),
	prog_out__write_context(Context),
	io__write_string("  Declared `"),
	hlds_out__write_category(DeclaredCategory),
	io__write_string("', inferred `"),
	hlds_out__write_category(Category),
	io__write_string("'.\n").

:- pred report_determinism_warning(pred_id, proc_id, category, category,
				module_info, io__state, io__state).
:- mode report_determinism_warning(in, in, in, in, in, di, uo) is det.

report_determinism_warning(PredId, ModeId, Category, DeclaredCategory,
		ModuleInfo) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ModeId, ProcInfo) },
	{ proc_info_context(ProcInfo, Context) },
	prog_out__write_context(Context),
	io__write_string(
		"Warning: determinism declaration could be stricter.\n"
	),
	prog_out__write_context(Context),
	io__write_string("  Declared `"),
	hlds_out__write_category(DeclaredCategory),
	io__write_string("', inferred `"),
	hlds_out__write_category(Category),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

	% det_infer_goal(Goal0, MiscInfo, Goal, Category)
	% Infers the determinism of `Goal0' and returns this in `Category'.
	% It annotates the goal and all it's subgoals with their determinism
	% and returns the annotated goal in `Goal'.

:- pred det_infer_goal(hlds__goal, misc_info, hlds__goal, category).
:- mode det_infer_goal(in, in, out, out) is det.

det_infer_goal(Goal0 - GoalInfo0, MiscInfo, Goal - GoalInfo, Category) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
	det_infer_goal_2(Goal0, MiscInfo, NonLocalVars, DeltaInstMap,
		Goal, Category0),

	% If a non-deterministic goal doesn't have any output variables,
	% then we can make it semi-deterministic (which will tell the
	% code generator to generate a commit after the goal).
	(
		Category0 = nondeterministic,
		no_output_vars(NonLocalVars, DeltaInstMap, MiscInfo)
	->
		Category1 = semideterministic
	;
		Category1 = Category0
	),

	% We need to take into account the `local' determinism of the goal,
	% which will get computed during mode analysis and/or switch detection.
	goal_info_get_local_determinism(GoalInfo0, LocalDeterminism),
	max_category(Category1, LocalDeterminism, Category),

	goal_info_set_determinism(GoalInfo0, Category, GoalInfo).

:- pred det_infer_goal_2(hlds__goal_expr, misc_info, set(var), instmap_delta,
				hlds__goal_expr, category).
:- mode det_infer_goal_2(in, in, in, in, out, out) is det.

	% the category of a conjunction is the worst case of the elements
	% of that conjuction.
det_infer_goal_2(conj(Goals0), MiscInfo, _, _, conj(Goals), D) :-
	det_infer_conj(Goals0, MiscInfo, Goals, D).

det_infer_goal_2(disj(Goals0), MiscInfo, _, _, disj(Goals), D) :-
	( Goals0 = [] ->
		% an empty disjunction is equivalent to `fail' and
		% is hence semi-deterministic
		Goals = [],
		D = semideterministic
	; Goals0 = [SingleGoal0] ->
		% a singleton disjunction is just equivalent to
		% the goal itself
		det_infer_goal(SingleGoal0, MiscInfo, SingleGoal, D),
		Goals = [SingleGoal]
	;
		D = nondeterministic,
		det_infer_disj(Goals0, MiscInfo, Goals)
	).

	% the category of a switch is the worst of the category of each of
	% the cases and (if only a subset of the constructors are handled)
	% semideterministic.

det_infer_goal_2(switch(Var, Cases0), MiscInfo, _, _, switch(Var, Cases), D) :-
	det_infer_switch(Cases0, MiscInfo, Cases, Cons, D1),
	test_to_see_that_all_constructors_are_tested(Cons, MiscInfo, D2),
	max_category(D1, D2, D).

	% look up the category entry associated with the call.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.
	%
	% Note that it _might_ be a good idea to record a list
	% of dependencies, so that we avoid recomputing the determinism
	% of clauses when none of the predicates they call changed
	% determinism.  But let's wait until this part of the
	% compilation becomes a bottleneck before worrying about
	% this.

det_infer_goal_2(call(PredId, ModeId, Args, BuiltIn, Name), MiscInfo, _, _,
		call(PredId, ModeId, Args, BuiltIn, Name), Category) :-
	detism_lookup(MiscInfo, PredId, ModeId, Category).
		% XXX need to handle calls to inferred modes!

	% unifications are either deterministic or semideterministic.
	% (see det_infer_unify).
det_infer_goal_2(unify(LT, RT, M, U, C), MiscInfo, _, _,
		unify(LT, RT, M, U, C), D) :-
	det_infer_unify(U, MiscInfo, D).

det_infer_goal_2(if_then_else(Vars, Cond0, Then0, Else0), MiscInfo,
		_NonLocals, _DeltaInstMap,
		if_then_else(Vars, Cond, Then, Else), D) :-
	det_infer_goal(Cond0, MiscInfo, Cond, DCond),
	det_infer_goal(Then0, MiscInfo, Then, DThen),
	det_infer_goal(Else0, MiscInfo, Else, DElse),
	( DCond = deterministic ->
		D = DThen
	; DCond = semideterministic ->
		max_category(DThen, DElse, D)
	;
		D = nondeterministic
	).

	% Negations are always semideterministic.  It is an error for
	% a negation to further instantiate any non-local variable. Such
	% errors will be reported by the mode analysis.
	%
	% Question: should we warn about and/or optimize the negation of a
	% deterministic goal (which will always fail) here?
	% Answer: yes, probably, but it's not a high priority.

det_infer_goal_2(not(Vars, Goal0), MiscInfo, _, _, not(Vars, Goal), D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, _D1),
	D = semideterministic.

	% explicit quantification isn't important, since we've already
	% stored the _information about variable scope in the goal_info.

det_infer_goal_2(some(Vars, Goal0), MiscInfo, _, _, some(Vars, Goal), D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, D).

:- pred no_output_vars(set(var), instmap_delta, misc_info).
:- mode no_output_vars(in, in, in) is semidet.

no_output_vars(_, unreachable, _).
no_output_vars(Vars, reachable(InstMapDelta), _MiscInfo) :-
	set__to_sorted_list(Vars, VarList),
	no_output_vars_2(VarList, InstMapDelta).

:- pred no_output_vars_2(list(var), instmapping).
:- mode no_output_vars_2(in, in) is semidet.

no_output_vars_2([], _).
no_output_vars_2([Var | Vars], InstMapDelta) :-
		% XXX this is a safe approximation, but it is not
		% precise enough.  The instmap delta may contain the
		% variable, but the variable may not be output, if
		% the change is just an increase in information rather
		% than an increase in instantiatedness.
		% This means that we will consider some goals as nondet
		% when they are really semidet.
	\+ map__contains(InstMapDelta, Var),
	no_output_vars_2(Vars, InstMapDelta).

:- pred det_infer_conj(list(hlds__goal), misc_info, list(hlds__goal), category).
:- mode det_infer_conj(in, in, out, out) is det.

det_infer_conj(Goals0, MiscInfo, Goals, D) :-
	det_infer_conj_2(Goals0, MiscInfo, deterministic, Goals, D).

:- pred det_infer_conj_2(list(hlds__goal), misc_info,
			category, list(hlds__goal), category).
:- mode det_infer_conj_2(in, in, in, out, out) is det.

det_infer_conj_2([], _MiscInfo, D, [], D).
det_infer_conj_2([Goal0|Goals0], MiscInfo, D0, [Goal|Goals], D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, D1),
	max_category(D0, D1, D2),
	det_infer_conj_2(Goals0, MiscInfo, D2, Goals, D).

:- pred det_infer_disj(list(hlds__goal), misc_info, list(hlds__goal)).
:- mode det_infer_disj(in, in, out) is det.

det_infer_disj([], _MiscInfo, []).
det_infer_disj([Goal0|Goals0], MiscInfo, [Goal|Goals]) :-
	det_infer_goal(Goal0, MiscInfo, Goal, _D),
	det_infer_disj(Goals0, MiscInfo, Goals).

:- pred det_infer_unify(unification, misc_info, category).
:- mode det_infer_unify(in, in, out) is det.

det_infer_unify(assign(_, _), _MiscInfo, deterministic).

det_infer_unify(construct(_, _, _, _), _MiscInfo, deterministic).

	% Deconstruction unifications are deterministic if the type
	% only has one constructor, or if the variable is known to be
	% already bound to the appropriate functor.
	% 
	% This will (XXX eventually!) be handled by modes.nl setting
	% the determinism field in the deconstruct(...) to semidet for
	% those deconstruction unifications which might fail.
	% But switch_detection.nl may set it back to det again, if it moves
	% the functor test into a switch instead.

det_infer_unify(deconstruct(_, _, _, _, Det), _MiscInfo, Det).

det_infer_unify(simple_test(_, _), _MiscInfo, semideterministic).

det_infer_unify(complicated_unify(_, _, _, Det), _MiscInfo, Det).

:- pred det_infer_switch(list(case), misc_info, list(case), 
			list(cons_id), category).
:- mode det_infer_switch(in, in, out, out, out) is det.

det_infer_switch(Cases0, MiscInfo, Cases, Cons, D1) :-
	det_infer_switch_2(Cases0, MiscInfo, Cases, [], Cons, 
		deterministic, D1).

	% Note that the use of an accumulator here is very Prolog-ish;
	% it doesn't actually buy us anything for Mercury.

:- pred det_infer_switch_2(list(case), misc_info, list(case), list(cons_id),
			list(cons_id), category, category).
:- mode det_infer_switch_2(in, in, out, in, out, in, out) is det.

det_infer_switch_2([], _MiscInfo, [], Cons, Cons, D, D).
det_infer_switch_2([Case0|Cases0], MiscInfo, [Case|Cases], Cons0, Cons,
		D0, D) :-
		% XXX we should update the instmap to reflect the
		% knowledge that the var is bound to this particular
		% constructor
	Case0 = case(ConsId, Goal0),
	det_infer_goal(Goal0, MiscInfo, Goal, D1),
	max_category(D0, D1, D2),
	Case = case(ConsId, Goal),
	det_infer_switch_2(Cases0, MiscInfo, Cases, [ConsId|Cons0], Cons,
		D2, D).

%-----------------------------------------------------------------------------%

:- pred max_category(category, category, category).
:- mode max_category(in, in, out) is det.

:- max_category(X, Y, _) when X and Y.	% NU-Prolog indexing.

max_category(deterministic, deterministic, deterministic).
max_category(deterministic, semideterministic, semideterministic).
max_category(deterministic, nondeterministic, nondeterministic).

max_category(semideterministic, deterministic, semideterministic).
max_category(semideterministic, semideterministic, semideterministic).
max_category(semideterministic, nondeterministic, nondeterministic).

max_category(nondeterministic, deterministic, nondeterministic).
max_category(nondeterministic, semideterministic, nondeterministic).
max_category(nondeterministic, nondeterministic, nondeterministic).

%-----------------------------------------------------------------------------%

	% detism_lookup(MiscInfo, PredId, ModeId, Category):
	% 	Given the MiscInfo, and the PredId & ModeId of a procedure,
	% 	look up the determinism of that procedure and return it
	% 	in Category.

:- pred detism_lookup(misc_info, pred_id, proc_id, category).
:- mode detism_lookup(in, in, in, out) is det.

detism_lookup(MiscInfo, PredId, ModeId, Category) :-
	MiscInfo = misc_info(ModuleInfo, _, _),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo),
	proc_info_inferred_determinism(ProcInfo, Category).

%-----------------------------------------------------------------------------%

:- pred test_to_see_that_all_constructors_are_tested(list(cons_id),
		misc_info, category).
:- mode test_to_see_that_all_constructors_are_tested(in, in, out) is det.
	
test_to_see_that_all_constructors_are_tested(_, _, deterministic).
		% Actually, we may handle this via the local_det field
		% in the goal_info, so this stub may be unnecessary.
	
%-----------------------------------------------------------------------------%
