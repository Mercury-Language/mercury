%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% det_analysis.nl - the determinism analysis pass.

% Main author: conway.
% (Hacked somewhat by fjh ;-)

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

% As originally written, this module had a major design flaw.
% The interface
%
% 	:- pred determinism_pass(module_info::in, module_info::out).
%
% did not cater for the following:
%
%	o The determinism pass needs to output error and/or warning
%	  messages
%	o The determinism pass needs access to the command-line options
%	  (eg. in order to determine the verbosity of its messages)
%	o The determinism pass should also record whether or not
%	  any errors occurred (although perhaps this should be recorded as
%	  part of the module_info).

% Also, if we are to avoid global analysis for predicates with
% declarations, then it must be an _error_, not just a warning,
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
:- mode determinism_pass(input, output, di, uo).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, prog_io, prog_out, std_util.

%-----------------------------------------------------------------------------%

determinism_pass(ModuleInfo0, ModuleInfo) -->
	{ determinism_declarations(ModuleInfo0, DeclaredProcs,
		UndeclaredProcs) },
	{ global_analysis_pass(ModuleInfo0, UndeclaredProcs, ModuleInfo1) },
	global_checking_pass(ModuleInfo1, DeclaredProcs, ModuleInfo).

%-----------------------------------------------------------------------------%

:- type predproclist	==	list(pair(pred_id, proc_id)).

:- type miscinfo	--->	miscinfo(
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
:- mode determinism_declarations(input, output, output) is det.

determinism_declarations(ModuleInfo, DeclaredProcs, UndeclaredProcs) :-
	get_all_pred_procs(ModuleInfo, PredProcs),
	segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs).

	% get_all_pred_procs takes a module_info and returns a list
	% of all the procedures ids for that module.
	
:- pred get_all_pred_procs(module_info, predproclist).
:- mode get_all_pred_procs(input, output) is det.

get_all_pred_procs(ModuleInfo, PredProcs) :-
	moduleinfo_predids(ModuleInfo, PredIds),
	moduleinfo_preds(ModuleInfo, Preds),
	get_all_pred_procs_2(Preds, PredIds, [], PredProcs).

:- pred get_all_pred_procs_2(pred_table, list(pred_id),
				predproclist, predproclist).
:- mode get_all_pred_procs_2(input, input, input, output).

get_all_pred_procs_2(_Preds, [], PredProcs, PredProcs).
get_all_pred_procs_2(Preds, [PredId|PredIds], PredProcs0, PredProcs) :-
	map__lookup(Preds, PredId, Pred),
	predinfo_procids(Pred, ProcIds),
	fold_pred_modes(PredId, ProcIds, PredProcs0, PredProcs1),
	get_all_pred_procs_2(Preds, PredIds, PredProcs1, PredProcs).

:- pred fold_pred_modes(pred_id, list(proc_id), predproclist, predproclist).
:- mode fold_pred_modes(input, input, input, output).

fold_pred_modes(_PredId, [], PredProcs, PredProcs).
fold_pred_modes(PredId, [ProcId|ProcIds], PredProcs0, PredProcs) :-
	fold_pred_modes(PredId, ProcIds, [PredId - ProcId|PredProcs0],
		PredProcs).

	% segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs)
	% splits the list of procedures PredProcs into DeclaredProcs and
	% UndeclaredProcs.

:- pred segregate_procs(module_info, predproclist, predproclist, predproclist).
:- mode segregate_procs(input, input, output, output).

segregate_procs(ModuleInfo, PredProcs, DeclaredProcs, UndeclaredProcs) :-
	segregate_procs_2(ModuleInfo, PredProcs, [], DeclaredProcs,
					[], UndeclaredProcs).

:- pred segregate_procs_2(module_info, predproclist, predproclist,
			predproclist, predproclist, predproclist).
:- mode segregate_procs_2(input, input, input, output, input, output).

segregate_procs_2(_ModuleInfo, [], DeclaredProcs, DeclaredProcs,
				UndeclaredProcs, UndeclaredProcs).
segregate_procs_2(ModuleInfo, [PredId - PredMode|PredProcs],
			DeclaredProcs0, DeclaredProcs,
				UndeclaredProcs0, UndeclaredProcs) :-
	moduleinfo_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, Pred),
	predinfo_procedures(Pred, Procs),
	map__lookup(Procs, PredMode, Proc),
	procinfo_declared_determinism(Proc, Category),
	(if
		Category = unspecified
	then
		UndeclaredProcs1 = [PredId - PredMode|UndeclaredProcs0],
		DeclaredProcs1 = DeclaredProcs0
	else
		DeclaredProcs1 = [PredId - PredMode|DeclaredProcs0],
		UndeclaredProcs1 = UndeclaredProcs0
	),
	segregate_procs_2(ModuleInfo, PredProcs, DeclaredProcs1, DeclaredProcs,
				UndeclaredProcs1, UndeclaredProcs).

%-----------------------------------------------------------------------------%

:- pred global_analysis_pass(module_info, predproclist, module_info).
:- mode global_analysis_pass(input, input, output).

	% Iterate until a fixpoint is reached

global_analysis_pass(ModuleInfo0, ProcList, ModuleInfo) :-
	global_analysis_single_pass(ModuleInfo0, ProcList, unchanged,
			ModuleInfo1, Changed),
	( Changed = changed ->
		global_analysis_pass(ModuleInfo1, ProcList, ModuleInfo)
	;
		ModuleInfo = ModuleInfo1
	).

:- type maybe_changed ---> changed ; unchanged.

:- pred global_analysis_single_pass(module_info, predproclist, maybe_changed,
				module_info, maybe_changed).
:- mode global_analysis_single_pass(input, input, input, output, output).

:- global_analysis_single_pass(_, A, _, _) when A.	% NU-Prolog indexing.

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
:- mode det_infer_proc(input, input, input, input, output, output).

det_infer_proc(ModuleInfo0, PredId, PredMode, State0, ModuleInfo, State) :-
		% Get the procinfo structure for this procedure
	moduleinfo_preds(ModuleInfo, Preds0),
	map__lookup(Preds0, PredId, Pred0),
	predinfo_procedures(Pred0, Procs0),
	map__lookup(Procs0, PredMode, Proc0),

		% Remember the old inferred determinism of this procedure
	procinfo_inferred_determinism(Proc0, Detism0),

		% Infer the determinism of the goal
	procinfo_goal(Proc0, Goal0),
	MiscInfo = miscinfo(ModuleInfo0, PredId, PredMode),
	make_inst_map(ModuleInfo0, Proc0, InstMap0),
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap, Detism),

		% Check whether the determinism of this procedure changed
	(
		Detism = Detism0
	->
		State = State0
	;
		State = changed
	),

		% Save the newly inferred information
	procinfo_set_goal(Proc0, Goal, Proc1),
	procinfo_set_inferred_determinism(Proc1, Detism, Proc),
	map__set(Procs0, PredMode, Proc, Procs),
	predinfo_set_procedures(Pred0, Procs, Pred),
	map__set(Preds0, PredId, Pred, Preds),
	moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo).

:- pred make_inst_map(module_info::in, proc_info::in, map(var, inst)::out)
	is det.

make_inst_map(ModuleInfo, ProcInfo, InstMap) :-
	procinfo_argmodes(ProcInfo, ArgModes),
	mode_list_get_initial_insts(ArgModes, ModuleInfo, InitialInsts),
	procinfo_headvars(ProcInfo, HeadVars),
	map__from_corresponding_lists(HeadVars, InitialInsts, InstMap).

%-----------------------------------------------------------------------------%

:- pred global_checking_pass(module_info, predproclist, module_info,
				io__state, io__state).
:- mode global_checking_pass(input, input, output, di, uo).

global_checking_pass(ModuleInfo0, ProcList, ModuleInfo) -->
	{ global_analysis_single_pass(ModuleInfo0, ProcList, unchanged,
		ModuleInfo, _) },
	global_checking_pass_2(ProcList, ModuleInfo).

:- pred global_checking_pass_2(predproclist, module_info, io__state, io__state).
:- mode global_checking_pass_2(input, input, di, uo).

global_checking_pass_2([], _ModuleInfo) --> [].
global_checking_pass_2([PredId - ModeId | Rest], ModuleInfo) -->
	{
	  moduleinfo_preds(ModuleInfo, PredTable),
	  map__lookup(PredTable, PredId, PredInfo),
	  predinfo_procedures(PredInfo, ProcTable),
	  map__lookup(ProcTable, ModeId, ProcInfo),
	  procinfo_declared_determinism(ProcInfo, Detism),
	  determinism_to_category(Detism, DeclaredCategory),
	  procinfo_inferred_determinism(ProcInfo, InferredCategory)
	},
	( { DeclaredCategory = InferredCategory } ->
		[]
	;
		{ max_category(DeclaredCategory, InferredCategory, Category) },
		( { Category = DeclaredCategory } ->
			report_determinism_warning(PredId, ModeId, ModuleInfo)
		;
			report_determinism_error(PredId, ModeId, ModuleInfo)
		)
	),
	global_checking_pass_2(Rest, ModuleInfo).

:- pred report_determinism_error(pred_id, proc_id, module_info,
				io__state, io__state).
:- mode report_determinism_error(input, input, input, di, uo).

report_determinism_error(PredId, ModeId, ModuleInfo) -->
	{ moduleinfo_preds(ModuleInfo, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ predinfo_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ModeId, ProcInfo) },
	{ procinfo_context(ProcInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("error: determinism declaration not satisfied").

:- pred report_determinism_warning(pred_id, proc_id, module_info,
				io__state, io__state).
:- mode report_determinism_warning(input, input, input, di, uo).

report_determinism_warning(PredId, ModeId, ModuleInfo) -->
	{ moduleinfo_preds(ModuleInfo, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ predinfo_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ModeId, ProcInfo) },
	{ procinfo_context(ProcInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("warning: determinism declaration could be stricter").

%-----------------------------------------------------------------------------%

	% det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap, Category)
	% Infers the determinism of `Goal0' and returns this in `Category'.
	% It annotates the goal and all it's subgoals with their determinism
	% and returns the annotated goal in `Goal'.
	% `InstMap0' should specify the instantiatedness of the variables
	% immediately before this goal gets executed; the returned
	% value of `InstMap' will specify the instantiatedness of the
	% variables after the goal has been executed.

:- pred det_infer_goal(hlds__goal, miscinfo, hlds__goal, instmap, instmap,
			category).
:- mode det_infer_goal(input, output, input, output, input, output).

det_infer_goal(Goal0 - GoalInfo0, MiscInfo, Goal - GoalInfo, InstMap0,
		InstMap, Category) :-
	det_infer_goal_2(Goal0, MiscInfo, Goal, InstMap0, Category),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_inferred_determinism(GoalInfo0, Category, GoalInfo).

:- pred det_infer_goal_2(hlds__goal_expr, miscinfo, hlds__goal_expr,
			instmap, category).
:- mode det_infer_goal_2(input, input, output, input, output).

	% the category of a conjunction is the worst case of the elements
	% of that conjuction.
det_infer_goal_2(conj(Goals0), MiscInfo, conj(Goals), InstMap0, D) :-
	det_infer_conj(Goals0, MiscInfo, InstMap0, Goals, D).

det_infer_goal_2(disj(Goals0), MiscInfo, Goal, InstMap0, D) :-
	( Goals0 = [] ->
		% an empty disjunction is equivalent to `fail' and
		% is hence semi-deterministic
		Goals = Goals0,
		D = semideterministic
	; Goals0 = [SingleGoal0] ->
		% a singleton disjunction is just equivalent to
		% the goal itself
		det_infer_goal(SingleGoal0, MiscInfo, SingleGoal,
				InstMap0, _, D),
		Goal = disj([SingleGoal])
	;
		% a non-trivial disjunction is always nondeterministic.
		% however, we do need to recursively decend the subgoals
		% to annotate them.
		det_infer_disj(Goals0, MiscInfo, InstMap0, Goals),
		D = nondeterministic
	).

	% the category of a switch is the worst of the category of each of
	% the cases, and (if only a subset of the constructors are handled)
	% semideterministic
det_infer_goal_2(switch(Var, Cases0, Follow), MiscInfo,
		switch(Var, Cases, Follow), InstMap0, D) :-
	det_infer_switch(Cases0, MiscInfo, Cases, Cons, InstMap0, D1),
	test_to_see_that_all_constructors_are_tested(Cons, MiscInfo, D2),
	max_category(D1, D2, D).

:- pred test_to_see_that_all_constructors_are_tested(list(cons_id),
		miscinfo, category).
:- mode test_to_see_that_all_constructors_are_tested(input, input, output).
	
test_to_see_that_all_constructors_are_tested(_, _, semideterministic).
	% XXX stub only!
	
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

det_infer_goal_2(call(PredId, ModeId, Args, BuiltIn), MiscInfo,
		call(PredId, ModeId, Args, BuiltIn), _InstMap0, Category) :-
	detism_lookup(MiscInfo, PredId, ModeId, Category).

	% unifications are either deterministic or semideterministic.
	% (see det_infer_unify).
det_infer_goal_2(unify(LT, RT, M, U, C), MiscInfo,
		unify(LT, RT, M, U, C), _InstMap0, D) :-
	det_infer_unify(U, MiscInfo, D).

	% if_then_elses have a category determined by the worst of the
	% condition and the two alternitive cases (then, else). The
	% condition is evaluated the same as a "some" construct, and if
	% the goal is nondeterministic and it further instansiates non-
	% local variables, the a commit must be performed. XXX at the 
	% moment, this is not dealt with - if_then_else/4 should be
	% extended to contain an annotation saying whether or not to
	% generate code for a commit.
det_infer_goal_2(if_then_else(Vars, Cond0, Then0, Else0), MiscInfo,
		if_then_else(Vars, Cond, Then, Else), InstMap0, D) :-
	det_infer_goal(Cond0, MiscInfo, Cond, InstMap0, InstMap1, DCond),
	det_infer_goal(Then0, MiscInfo, Then, InstMap1, _InstMap2, DThen),
	det_infer_goal(Else0, MiscInfo, Else, InstMap1, _InstMap3, DElse),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap, MiscInfo)
	then
		max_category(DThen, DElse, D)
	else
		max_category(DCond, DThen, DIf),
		max_category(DIf, DElse, D)
	).

	% nots are always semideterministic. it is an error for
	% a not to further instansiate any non-local variable. Such
	% errors should be reported by the mode analysis
	% XXX should they be reported here?
	% XXX should we warn about and/or optimize the negation of a
	%     deterministic goal (which will always fail) here?
det_infer_goal_2(not(Vars, Goal0), MiscInfo, not(Vars, Goal), InstMap0, D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1, _D1),
	D = semideterministic.
det_infer_goal_2(some(Vars, Goal0), MiscInfo, some(Vars, Goal), InstMap0, D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap1, D1),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	% `some' can transform a nondeterministic goal into a semideterministic
	% one, if all of the output variables are quantified
	(
		D1 = nondeterministic,
		no_output_vars(Vars, ModeMap, MiscInfo)
	->
		D = semideterministic
	;
		D = D1
	).

	% XXX need to think about `all'.
	% For the moment just make the safe assumption
	% that they're all non-deterministic.

det_infer_goal_2(all(Vars, Goal0), MiscInfo, all(Vars, Goal), InstMap0, D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1, _D1),
	D = nondeterministic.

:- pred no_output_vars(list(var), map(var, mode), miscinfo).
:- mode no_output_vars(input, input, input) is semidet.

no_output_vars(_, _, _) :-
	fail.	% XXX stub only!

:- pred det_infer_conj(list(hlds__goal), miscinfo, instmap,
			list(hlds__goal), category).
:- mode det_infer_conj(input, input, input, output, output).

det_infer_conj(Goals0, MiscInfo, InstMap0, Goals, D) :-
	det_infer_conj_2(Goals0, MiscInfo, InstMap0, deterministic, Goals, D).

:- pred det_infer_conj_2(list(hlds__goal), miscinfo,
			instmap, category, list(hlds__goal), category).
:- mode det_infer_conj_2(input, input, input, input, output, output).

det_infer_conj_2([], _MiscInfo, _InstMap0, D, [], D).
det_infer_conj_2([Goal0|Goals0], MiscInfo, InstMap0, D0, [Goal|Goals], D) :-
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap1, D1),
	max_category(D0, D1, D2),
	det_infer_conj_2(Goals0, MiscInfo, InstMap1, D2, Goals, D).

:- pred det_infer_disj(list(hlds__goal), miscinfo, instmap, list(hlds__goal)).
:- mode det_infer_disj(input, input, input, output).

det_infer_disj([], _MiscInfo, _InstMap0, []).
det_infer_disj([Goal0|Goals0], MiscInfo, InstMap0, [Goal|Goals]) :-
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap, _D),
	det_infer_disj(Goals0, MiscInfo, InstMap, Goals).

:- pred det_infer_unify(unification, miscinfo, category).
:- mode det_infer_unify(input, input, output).

det_infer_unify(assign(_, _), _MiscInfo, deterministic).

det_infer_unify(construct(_, _, _, _), _MiscInfo, deterministic).

	% XXX - This is deterministic if the type only has one constructor.
det_infer_unify(deconstruct(_, _, _, _), _MiscInfo, semideterministic).

det_infer_unify(simple_test(_, _), _MiscInfo, semideterministic).

	% XXX - Some of these could be deterministic.
det_infer_unify(complicated_unify(_, _, _), _MiscInfo, semideterministic).

:- pred det_infer_switch(list(case), miscinfo, list(case), 
			list(cons_id), instmap, category).
:- mode det_infer_switch(input, input, output, output, input, output).

det_infer_switch(Cases0, MiscInfo, Cases, Cons, InstMap0, D1) :-
	det_infer_switch_2(Cases0, MiscInfo, Cases, [], Cons, InstMap0,
		deterministic, D1).

	% Note that the use of an accumulator here is very Prolog-ish;
	% it doesn't actually buy us anything for Mercury.

:- pred det_infer_switch_2(list(case), miscinfo, list(case), list(cons_id),
			list(cons_id), instmap, category, category).
:- mode det_infer_switch_2(input, input, output, input,
			output, input, input, output).

det_infer_switch_2([], _MiscInfo, [], Cons, Cons, _InstMap0, D, D).
det_infer_switch_2([Case0|Cases0], MiscInfo, [Case|Cases], Cons0, Cons,
		InstMap0, D0, D) :-
	Case0 = case(ConsId, Vars, Goal0),
	det_infer_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1, D1),
	max_category(D0, D1, D2),
	Case = case(ConsId, Vars, Goal),
	det_infer_switch_2(Cases0, MiscInfo, Cases, [ConsId|Cons0], Cons,
		InstMap0, D2, D).

%-----------------------------------------------------------------------------%

:- pred max_category(category, category, category).
:- mode max_category(input, input, output).

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

:- type modemap == map(var, mode).

:- pred make_var_modes(instmap, instmap, modemap).
:- mode make_var_modes(input, input, output).

make_var_modes(InstMap0, InstMap1, ModeMap) :-
	map__keys(InstMap1, Keys),
	make_var_modes_2(InstMap0, InstMap1, Keys, ModeMap).

:- pred make_var_modes_2(instmap, instmap, list(var), modemap).
:- mode make_var_modes_2(input, input, input, output).

:- make_var_modes_2(_, _, X, _) when X.		% NU-Prolog indexing.

make_var_modes_2(_InstMap0, _InstMap1, [], ModeMap) :-
	map__init(ModeMap).
make_var_modes_2(InstMapA, InstMapB, [Key|Keys], ModeMap) :-
	make_var_modes_2(InstMapA, InstMapB, Keys, ModeMap0),
	( map__search(InstMapA, Key, InstA0) ->
		InstA = InstA0
	;
		InstA = free
	),
	( map__search(InstMapB, Key, InstB0) ->
		InstB = InstB0
	;
		InstB = free
	),
	map__set(ModeMap0, Key, (InstA -> InstB), ModeMap).

%-----------------------------------------------------------------------------%

	% detism_lookup(MiscInfo, PredId, ModeId, Category):
	% 	Given the MiscInfo, and the PredId & ModeId of a procedure,
	% 	look up the determinism of that procedure and return it
	% 	in Category.

:- pred detism_lookup(miscinfo, pred_id, proc_id, category).
:- mode detism_lookup(input, input, input, output) is det.

detism_lookup(MiscInfo, PredId, ModeId, Category) :-
	MiscInfo = miscinfo(ModuleInfo, _, _),
	moduleinfo_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	predinfo_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo),
	procinfo_inferred_determinism(ProcInfo, Category).

%-----------------------------------------------------------------------------%


/********************* ALL THIS IS COMMENTED OUT

	% local_check_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap, D0, D):

:- pred local_check_goal(hlds__goal, miscinfo, hlds__goal, instmap, instmap,
			category, category).
:- mode local_check_goal(input, input, output, input, output, input, output).

local_check_goal(Goal0 - GoalInfo0, MiscInfo, Goal - GoalInfo,
		InstMap0, InstMap, Category, D) :-
	local_check_goal(Goal0, MiscInfo, InstMap0, Category, Goal, D),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D, GoalInfo).

:- pred local_check_goal(hlds__goal, miscinfo, hlds__goal, instmap, instmap,
			category, category).
:- mode local_check_goal(input, input, output, input, output, input, output).

	% the category of a conjunction is the worst case of the elements
	% of that conjuction.
	% We don't need to report any errors at this point because a conj
	% of itself does not introduce any nondeterminism, so any conflict
	% will have been incurred in a member of the conjuction and will 
	% have been reported there.

local_check_goal(conj(Goals0) - GoalInfo0, MiscInfo, conj(Goals) - GoalInfo,
		InstMap0, InstMap, Category, D) :-
	local_check_conj(Goals0, MiscInfo, InstMap0, Category,
				deterministic, Goals, D),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D, GoalInfo).

	% A disjunction is always nondeterministic, however, we do need to
	% recursively decend the subgoals to annotate them.
	% (Actually, disjunctions in the source code can be deterministic, 
	% but if so then they will have been converted into switches
	% before we get to this stage of the compilation.)
	% Since a disjunction is always nondeterministic, if the declared
	% category is anything other, then there is an error, irrespective
	% of the determinacy of the members of the disjunction.
	% since a disjunction is maximally nondeterministic, we don't need
	% to check the subgoals, but we still need to annotate them; so
	% we can use the det_infer_disj predicate.
		% XXX
local_check_goal(disj(Goals0) - GoalInfo0, MiscInfo, disj(Goals) - GoalInfo,
		InstMap0, InstMap, Category, D0, D) :-
	det_infer_disj(Goals0, MiscInfo, InstMap0, Goals),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, nondeterministic, GoalInfo),
	max_category(D0, nondeterministic, D),
	(if
		Category = nondeterministic
	then
		true
	else
		% XXX
		%	We have a design problem here!
		%	The interface to this predicate doesn't
		%	allow it do to any I/O.  Also, the warning
		%	would need to specify where in the source code
		%	the problem occurred.
		% 	
		%%% warn("A procedure containing a disjunction must be"),
		%%% warn("declared as nondet."),
		true
	).

	% the category of a switch is the worst of the category of each of
	% the cases, and (if only a subset of the constructors are handled)
	% semideterministic
	% if the switch is semideterministic (ie there are constructors not
	% handled by the switch) then the bodies of the switch must be at
	% most semideterministic. If the switch covers all the constructors
	% then the cases must all be deterministic for the switch to be
	% deterministic, and since my brain is currently semifunctional,
	% working out how to do all this properly will have to wait.
local_check_goal(switch(Var, Cases0, Follow) - GoalInfo0, MiscInfo,
		switch(Var, Cases, Follow) - GoalInfo, InstMap0,
		InstMap, Category, D0, D) :-
	local_check_switch(Cases0, MiscInfo, Cases,
			[], Cons, InstMap0, Category,
					deterministic, D1),
	test_to_see_that_all_constructors_are_tested(Cons, MiscInfo, D2),
	max_category(D1, D2, D3),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D2, GoalInfo),
	max_category(D0, D3, D).
	
	% look up the category entry associated with the call.
	% This is the point at which annotations start changing
	% when we iterate to fixpoint for global determinism analysis.
	% the called function must be at most the same category as this
	% procedure - if it is more nondeterministic than this procedure
	% then that is an error.
local_check_goal(call(PredId, ModeId, Args, BuiltIn) - GoalInfo, MiscInfo,
		call(PredId, ModeId, Args, BuiltIn) - GoalInfo, InstMap0,
		InstMap, Category, D, D) :-
	goalinfo_instmap(GoalInfo, InstMap),
	detism_lookup(MiscInfo, PredId, ModeId, Category0),
	goalinfo_set_category(GoalInfo0, Category0, GoalInfo),
	max_category(D0, Category0, D),
	(if
		max_category(Category, D, Category)
	then
		true
	else
		% XXX design problem
		%%% warn("predicate call is more nondeterministic than the"),
		%%% warn("declared determinism for this mode of this predicate")
		true
	).

	% unifications are either deterministic or semideterministic.
	% (see local_check_unify).
	% like calls, if the unification is more nondeterministic than
	% the declared determinism of this procedure, then raise an error.
local_check_goal(unify(LT, RT, M, U, C) - GoalInfo0, MiscInfo,
		unify(LT, RT, M, U, C) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_unify(U, MiscInfo, D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D1, GoalInfo),
	max_category(D0, D1, D),
	(if
		max_category(Category, D, Category)
	then
		true
	else
		% XXX
		% warn("unification is more nondeterministic than the"),
		% warn("declared determinism for this mode of this predicate."),
		true
	).

	% if_then_elses have a category determined by the worst of the
	% condition and the two alternitive cases (then, else). The
	% condition is evaluated the same as a "some" construct, and if
	% the goal is nondeterministic and it further instansiates non-
	% local variables, the a commit must be performed. XXX at the 
	% moment, this is not dealt with - if_then_else/4 should be
	% extended to contain an annotation saying whether or not to
	% generate code for a commit.
local_check_goal(if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo0, MiscInfo,
		if_then_else(Vars, Cond, Then, Else) - GoalInfo,
		InstMap0, InstMap, Category, D0, D) :-
	local_check_goal(Cond0, MiscInfo, Cond, InstMap0, InstMap1, Category,
			D1),
	local_check_goal(Then0, MiscInfo, Then, InstMap1, _InstMap2, Category,
			D2),
	local_check_goal(Else0, MiscInfo, Else, InstMap1, _InstMap3, Category,
			D3),
	goalinfo_instmap(GoalInfo0, InstMap),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap, MiscInfo)
	then
		max_category(D2,D3,D4)
	else
		D4 = nondeterministic
	),
	goalinfo_set_category(GoalInfo0, D4, GoalInfo),
	max_category(D0, D4, D).

	% nots are always semideterministic. it is an error for
	% a not to further instansiate any non-local variable. Such
	% errors should be reported by the mode analysis
	% XXX should they be reported here?
local_check_goal(not(Vars, Goal0) - GoalInfo0, MiscInfo,
		not(Vars, Goal) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1, Category,
			deterministic, _D1),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, semideterministic, GoalInfo),
	max_category(D0, semideterministic, D).
local_check_goal(some(Vars, Goal0) - GoalInfo0, MiscInfo,
		some(Vars, Goal) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0, InstMap1, Category,
			deterministic, D1),
	make_var_modes(InstMap0, InstMap1, ModeMap),
	(if
		no_output_vars(Vars, ModeMap, MiscInfo)
	then
		(if
			some [Inference] (
				D1 = deterministic
			)
		then
			D2 = D1
		else
			D2 = semideterministic
		)
	else
		D2 = D1
	),
	goalinfo_instmap(GoalInfo0, InstMap),
	goalinfo_set_category(GoalInfo0, D2, GoalInfo),
	max_category(D0, D2, D).
local_check_goal(all(Vars, Goal0) - GoalInfo0, MiscInfo,
		all(Vars, Goal) - GoalInfo, InstMap0,
					InstMap, Category, D0, D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0, _InstMap1,
				Category, deterministic, _D1),
		% XXX is instmap right here???
	InstMap = InstMap0,
	goalinfo_set_category(GoalInfo0, semideterministic, GoalInfo),
	max_category(D0, semideterministic, D).

:- pred local_check_conj(list(hlds__goal), miscinfo, instmap, category,
			category, list(hlds__goal), category).
:- mode local_check_conj(input, input, input, input, input, output, output).

local_check_conj([], MiscInfo, InstMap0, Category, D, [], D).
local_check_conj([Goal0|Goals0], MiscInfo, InstMap0,
					Category, D0, [Goal|Goals], D) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0,
					InstMap1, Category, D0, D1),
	local_check_conj(Goals0, MiscInfo, InstMap1, Category, D1, Goals, D).

:- pred local_check_disj(list(hlds__goal), miscinfo, instmap, 
			category, list(hlds__goal)).
:- mode local_check_disj(input, input, input, input, output).

local_check_disj([], MiscInfo, InstMap0, Category, []).
local_check_disj([Goal0|Goals0], MiscInfo, InstMap0, Category, [Goal|Goals]) :-
	local_check_goal(Goal0, MiscInfo, Goal, InstMap0,
			InstMap, Category, deterministic, _D),
	local_check_disj(Goals0, MiscInfo, InstMap, Category, Goals).

ALL OF THE ABOVE IS COMMENTED OUT *************************************/

%-----------------------------------------------------------------------------%
