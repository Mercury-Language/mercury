%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: modes.nl.
% Main author: fjh.
%
% This file contains a mode-checker.
% Still very incomplete.

% XXX BUG! abstractly_unify_inst will go into an infinite loop
% 	   when unifying two recursively defined insts.
%	   We need to introduce compiler-generated recursive
%	   insts in this case.

% XXX we need to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is 
%     dead.

% XXX break unifications into "micro-unifications"

% XXX handle code which always fails or always loops.
%	eg. currently the following code
%		p(a, Y) :- fail.
%		p(b, 1).
%	gives a mode error, because Y is not output by the first clause.
%	The same problem occurs with calls to error/1.

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
		handing around at the end of the conjunction, 
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
		two free variables (or in general two free sub-terms).
	(e) a predicate call
		Check that there is a mode declaration for the
		predicate which matches the current instantiation of
		the arguments.
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

******************************************/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modes.
:- interface.
:- import_module hlds, io.

:- pred modecheck(module_info, module_info, io__state, io__state).
:- mode modecheck(in, out, di, uo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module type_util, mode_util, prog_io.
:- import_module globals, options, mercury_to_mercury, hlds_out.
:- import_module stack.

%-----------------------------------------------------------------------------%

modecheck(Module0, Module) -->
	lookup_option(statistics, bool(Statistics)),
	lookup_option(verbose, bool(Verbose)),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose,
		"% Checking for undefined insts and modes...\n"),
	check_undefined_modes(Module0, Module1),
	maybe_report_stats(Statistics),

	maybe_write_string(Verbose, "% Mode-checking clauses...\n"),
	check_pred_modes(Module1, Module),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%
	
	% Mode-check the code for all the predicates in a module.

:- pred check_pred_modes(module_info, module_info, io__state, io__state).
:- mode check_pred_modes(in, out, di, uo).

check_pred_modes(Module0, Module) -->
	{ module_info_predids(Module0, PredIds) },
	modecheck_pred_modes_2(PredIds, Module0, Module).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_pred_modes_2(list(pred_id), module_info, 
			module_info, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, out, di, uo).

modecheck_pred_modes_2([], ModuleInfo, ModuleInfo) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__search(Preds0, PredId, PredInfo0) },
	{ pred_info_clauses_info(PredInfo0, ClausesInfo0) },
	{ ClausesInfo0 = clauses_info(_, _, _, Clauses0) },
	( { Clauses0 = [] } ->
		{ ModuleInfo3 = ModuleInfo0 }
	;
		lookup_option(very_verbose, bool(VeryVerbose)),
		( { VeryVerbose = yes } ->
			io__write_string("% Mode-checking predicate "),
			hlds_out__write_pred_id(PredId),
			io__write_string("\n")
		;
			[]
		),
		{ copy_clauses_to_procs(PredInfo0, PredInfo1) },
		{ map__set(Preds0, PredId, PredInfo1, Preds1) },
		{ module_info_set_preds(ModuleInfo0, Preds1, ModuleInfo1) },
		modecheck_procs(PredId, ModuleInfo1, PredInfo1, PredInfo, Errs),
		{ map__set(Preds1, PredId, PredInfo, Preds) },
		{ module_info_set_preds(ModuleInfo1, Preds, ModuleInfo2) },
		{ module_info_num_errors(ModuleInfo2, NumErrors0) },
		{ NumErrors is NumErrors0 + Errs },
		{ module_info_set_num_errors(ModuleInfo2, NumErrors,
						ModuleInfo3) }
	),
	modecheck_pred_modes_2(PredIds, ModuleInfo3, ModuleInfo).

%-----------------------------------------------------------------------------%

	% In the hlds, we initially record the clauses for a predicate
	% in the clauses_info data structure which is part of the
	% pred_info data structure.  But once the clauses have been
	% type-checked, we want to have a separate copy of each clause
	% for each different mode of the predicate, since we may
	% end up reordering the clauses differently in different modes.
	% Here we copy the clauses from the clause_info data structure
	% into the proc_info data structure.  Each clause is marked
	% with a list of the modes for which it applies, so that
	% there can be different code to implement different modes
	% of a predicate (e.g. sort).  For each mode of the predicate,
	% we select the clauses for that mode, disjoin them together,
	% and save this in the proc_info.

:- pred copy_clauses_to_procs(pred_info, pred_info).
:- mode copy_clauses_to_procs(in, out).

copy_clauses_to_procs(PredInfo0, PredInfo) :-
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	pred_info_procedures(PredInfo0, Procs0),
	map__keys(Procs0, ProcIds),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id)::in, clauses_info::in,
				proc_table::in, proc_table::out).

copy_clauses_to_procs_2([], _, Procs, Procs).
copy_clauses_to_procs_2([ProcId | ProcIds], ClausesInfo, Procs0, Procs) :-
	ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, Clauses),
	select_matching_clauses(Clauses, ProcId, MatchingClauses),
	get_clause_goals(MatchingClauses, GoalList),
	(GoalList = [SingleGoal] ->
		Goal = SingleGoal
	;
		% XXX we should initialize the goal_info context
		% XXX we should avoid creating nested disjunctions
		goal_info_init(GoalInfo0),
		set__list_to_set(HeadVars, NonLocalVars),
		goal_info_set_nonlocals(GoalInfo0, NonLocalVars, GoalInfo),
		Goal = disj(GoalList) - GoalInfo
	),
	map__lookup(Procs0, ProcId, Proc0),
	Proc0 = procedure(DeclaredDet, _, _, _, ArgModes, _, Context, CallInfo,
			InferredDet, ArgInfo),
	Proc = procedure(DeclaredDet, VarSet, VarTypes, HeadVars, ArgModes,
			Goal, Context, CallInfo, InferredDet, ArgInfo),
	map__set(Procs0, ProcId, Proc, Procs1),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs1, Procs).

:- pred select_matching_clauses(list(clause), proc_id, list(clause)).
:- mode select_matching_clauses(in, in, out).

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
	Clause = clause(ProcIds, _, _),
	( member(ProcId, ProcIds) ->
		MatchingClauses = [Clause | MatchingClauses1]
	;
		MatchingClauses = MatchingClauses1
	),
	select_matching_clauses(Clauses, ProcId, MatchingClauses1).

:- pred get_clause_goals(list(clause)::in, list(hlds__goal)::out) is det.

get_clause_goals([], []).
get_clause_goals([Clause | Clauses], [Goal | Goals]) :-
	Clause = clause(_, Goal, _),
	get_clause_goals(Clauses, Goals).

%-----------------------------------------------------------------------------%

:- pred modecheck_procs(pred_id, module_info, pred_info, pred_info, int,
			io__state, io__state).
:- mode modecheck_procs(in, in, in, out, out, di, uo).

modecheck_procs(PredId, ModuleInfo, PredInfo0, PredInfo, NumErrors) -->
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ map__keys(Procs0, ProcIds) },
	modecheck_procs_2(ProcIds, PredId, ModuleInfo, Procs0, 0,
				Procs, NumErrors),
	{ pred_info_set_procedures(PredInfo0, Procs, PredInfo) }.

	% Iterate over the list of modes for a predicate.

:- pred modecheck_procs_2(list(proc_id), pred_id, module_info,
		proc_table, int, proc_table, int, io__state, io__state).
:- mode modecheck_procs_2(in, in, in, in, in, out, out, di, uo).

modecheck_procs_2([], _PredId, _ModuleInfo, Procs, Errs, Procs, Errs) --> [].
modecheck_procs_2([ProcId|ProcIds], PredId, ModuleInfo, Procs0, Errs0,
					Procs, Errs) -->
		% lookup the proc_info
	{ map__lookup(Procs0, ProcId, ProcInfo0) },
		% mode-check that mode of the predicate
	modecheck_proc(ProcId, PredId, ModuleInfo, ProcInfo0,
			ProcInfo, NumErrors),
	{ Errs1 is Errs0 + NumErrors },
		% save the proc_info
	{ map__set(Procs0, ProcId, ProcInfo, Procs1) },
		% recursively process the remaining modes
	modecheck_procs_2(ProcIds, PredId, ModuleInfo, Procs1, Errs1,
				Procs, Errs).

%-----------------------------------------------------------------------------%

	% Mode-check the code for predicate in a given mode.

:- pred modecheck_proc(proc_id, pred_id, module_info, proc_info,
				proc_info, int, io__state, io__state).
:- mode modecheck_proc(in, in, in, in, out, out, di, uo).

modecheck_proc(ProcId, PredId, ModuleInfo, ProcInfo0, ProcInfo, NumErrors,
			IOState0, IOState) :-
		% extract the useful fields in the proc_info
	proc_info_goal(ProcInfo0, Body0),
	proc_info_argmodes(ProcInfo0, ArgModes),
	proc_info_context(ProcInfo0, Context),
	proc_info_headvars(ProcInfo0, HeadVars),
		% modecheck the clause - first set the initial instantiation
		% of the head arguments, mode-check the body, and
		% then check that the final instantiation matches that in
		% the mode declaration
	mode_list_get_initial_insts(ArgModes, ModuleInfo, ArgInitialInsts),
	map__from_corresponding_lists(HeadVars, ArgInitialInsts, InstMapping0),
	InstMap0 = reachable(InstMapping0),
		% initially, only the head variables are live
	set__list_to_set(HeadVars, LiveVars),
	mode_info_init(IOState0, ModuleInfo, PredId, ProcId, Context, LiveVars,
			InstMap0, ModeInfo0),
	modecheck_goal(Body0, Body, ModeInfo0, ModeInfo1),
	modecheck_final_insts(HeadVars, ArgModes, ModeInfo1, ModeInfo2),
	modecheck_report_errors(ModeInfo2, ModeInfo),
	mode_info_get_num_errors(ModeInfo, NumErrors),
	mode_info_get_io_state(ModeInfo, IOState),
	proc_info_set_goal(ProcInfo0, Body, ProcInfo).

:- pred modecheck_final_insts(list(var), list(mode), mode_info, mode_info).
:- mode modecheck_final_insts(in, in, in, out).

modecheck_final_insts(_, _, ModeInfo, ModeInfo).	% XXX Stub only!!!

modecheck_final_insts(HeadVars, ArgModes, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_list_get_final_insts(ArgModes, ModuleInfo, ArgFinalInsts),
	mode_info_get_instmap(ModeInfo0, InstMap),
	check_final_insts(HeadVars, ArgFinalInsts, 1, InstMap,
		ModuleInfo, ModeInfo0, ModeInfo).

:- pred check_final_insts(list(var), list(inst), int, instmap, module_info,
				mode_info, mode_info).
:- mode check_final_insts(in, in, in, in, in, mode_info_di, mode_info_uo)
	is det.

check_final_insts([], [], _, _, _) --> [].
check_final_insts([Var | Vars], [Inst | Insts], ArgNum, InstMap, ModuleInfo)
		-->
	{ instmap_lookup_var(InstMap, Var, VarInst) },
	( { inst_matches_final(VarInst, Inst, ModuleInfo) } ->
		[]
	;
		mode_info_error([],
			mode_error_final_inst(ArgNum, Var, VarInst, Inst)
		)
	),
	{ ArgNum1 is ArgNum + 1 },
	check_final_insts(Vars, Insts, ArgNum1, InstMap, ModuleInfo).

%-----------------------------------------------------------------------------%

% Modecheck a goal by abstractly interpreteting it, as explained
% at the top of this file.

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
	%%% goal_info_get_context(GoalInfo0, Context),
	%%% mode_info_set_context(ModeInfo0, Context, ModeInfo1)
		%
		% modecheck the goal, and then store the changes in
		% instantiation of the non-local vars in the goal's goal_info.
		%
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	mode_info_get_vars_instmap(ModeInfo0, NonLocals, InstMap0),
	modecheck_goal_2(Goal0, NonLocals, Goal, ModeInfo0, ModeInfo),
	mode_info_get_vars_instmap(ModeInfo, NonLocals, InstMap),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).

:- pred modecheck_goal_2(hlds__goal_expr, set(var), hlds__goal_expr,
			mode_info, mode_info).
:- mode modecheck_goal_2(in, in, out, mode_info_di, mode_info_uo) is det.

modecheck_goal_2(conj(List0), _NonLocals, conj(List)) -->
	mode_checkpoint(enter, "conj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] }
	;
		modecheck_conj_list(List0, List)
	),
	mode_checkpoint(exit, "conj").

modecheck_goal_2(disj(List0), NonLocals, disj(List)) -->
	mode_checkpoint(enter, "disj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] },
		mode_info_set_instmap(unreachable)
	;
		modecheck_disj_list(List0, List, InstMapList),
		instmap_merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "disj").

modecheck_goal_2(if_then_else(Vs, A0, B0, C0), NonLocals,
		if_then_else(Vs, A, B, C)) -->
	mode_checkpoint(enter, "if-then-else"),
	{ goal_get_nonlocals(B0, B_Vars) },
	{ goal_get_nonlocals(C0, C_Vars) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(NonLocals),
	mode_info_add_live_vars(B_Vars),
	mode_info_add_live_vars(C_Vars),
	modecheck_goal(A0, A),
	mode_info_remove_live_vars(B_Vars),
	mode_info_remove_live_vars(C_Vars),
	mode_info_unlock_vars(NonLocals),
	modecheck_goal(B0, B),
	mode_info_dcg_get_instmap(InstMapB),
	mode_info_set_instmap(InstMap0),
	modecheck_goal(C0, C),
	mode_info_dcg_get_instmap(InstMapC),
	mode_info_set_instmap(InstMap0),
	instmap_merge(NonLocals, [InstMapB, InstMapC], if_then_else),
	mode_checkpoint(exit, "if-then-else").

modecheck_goal_2(not(Vs, A0), NonLocals, not(Vs, A)) -->
	mode_checkpoint(enter, "not"),
	mode_info_lock_vars(NonLocals),
	modecheck_goal(A0, A),
	mode_info_unlock_vars(NonLocals),
	mode_checkpoint(exit, "not").

modecheck_goal_2(some(Vs, G0), _, some(Vs, G)) -->
	mode_checkpoint(enter, "some"),
	modecheck_goal(G0, G),
	mode_checkpoint(exit, "some").

modecheck_goal_2(all(Vs, G0), NonLocals, all(Vs, G)) -->
	mode_checkpoint(enter, "all"),
	mode_info_lock_vars(NonLocals),
	modecheck_goal(G0, G),
	mode_info_unlock_vars(NonLocals),
	mode_checkpoint(exit, "all").

modecheck_goal_2(call(PredId, _, Args, Builtin), _,
		 call(PredId, Mode, Args, Builtin)) -->
	mode_checkpoint(enter, "call"),
	mode_info_set_call_context(call(PredId)),
	modecheck_call_pred(PredId, Args, Mode),
	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

modecheck_goal_2(unify(A, B, _, _, UnifyContext), _,
		 unify(A, B, Mode, UnifyInfo, UnifyContext)) -->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),
	modecheck_unification(A, B, Mode, UnifyInfo),
	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

:- pred goal_get_nonlocals(hlds__goal, set(var)).
:- mode goal_get_nonlocals(in, out).

goal_get_nonlocals(_Goal - GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

%-----------------------------------------------------------------------------%

:- pred compute_instmap_delta(instmap, instmap, set(var), instmap_delta).
:- mode compute_instmap_delta(in, in, in, out) is det.

compute_instmap_delta(InstMapA, InstMapB, NonLocals, DeltaInstMap) :-
	set__to_sorted_list(NonLocals, NonLocalsList),
	compute_instmap_delta_2(NonLocalsList, InstMapA, InstMapB, AssocList),
	map__from_sorted_assoc_list(AssocList, DeltaInstMap).

:- pred compute_instmap_delta_2(list(var), instmap, instmap,
					assoc_list(var, inst)).
:- mode compute_instmap_delta_2(in, in, in, out) is det.

compute_instmap_delta_2([], _, _, []).
compute_instmap_delta_2([Var | Vars], InstMapA, InstMapB, AssocList) :-
	instmap_lookup_var(InstMapA, Var, InstA),
	instmap_lookup_var(InstMapB, Var, InstB),
	( InstA = InstB ->
		AssocList1 = AssocList
	;
		AssocList = [ Var - InstB | AssocList1 ]
	),
	compute_instmap_delta_2(Vars, InstMapA, InstMapB, AssocList1).

:- pred instmap_lookup_var(instmap, var, inst).
:- mode instmap_lookup_var(in, in, out) is det.

instmap_lookup_var(unreachable, _Var, not_reached).
instmap_lookup_var(reachable(InstMap), Var, Inst) :-
	( map__search(InstMap, Var, VarInst) ->
		Inst = VarInst
	;
		Inst = free
	).

:- pred instmap_lookup_arg_list(list(term), instmap, list(inst)).
:- mode instmap_lookup_arg_list(in, in, out).

instmap_lookup_arg_list([], _InstMap, []).
instmap_lookup_arg_list([Arg|Args], InstMap, [Inst|Insts]) :-
	Arg = term__variable(Var),
	instmap_lookup_var(InstMap, Var, Inst),
	instmap_lookup_arg_list(Args, InstMap, Insts).

%-----------------------------------------------------------------------------%

:- pred modecheck_conj_list(list(hlds__goal), list(hlds__goal),
				mode_info, mode_info).
:- mode modecheck_conj_list(in, in, mode_info_di, mode_info_uo) is det.

modecheck_conj_list(Goals0, Goals) -->
	=(ModeInfo0),
	{ mode_info_get_errors(ModeInfo0, OldErrors) },
	mode_info_set_errors([]),

	{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },
	{ delay_info_enter_conj(DelayInfo0, DelayInfo1) },
	mode_info_set_delay_info(DelayInfo1),
	mode_info_add_goals_live_vars(Goals0),

	modecheck_conj_list_2(Goals0, Goals),

	=(ModeInfo3),
	{ mode_info_get_errors(ModeInfo3, NewErrors) },
	{ append(OldErrors, NewErrors, Errors) },
	mode_info_set_errors(Errors),

	{ mode_info_get_delay_info(ModeInfo3, DelayInfo4) },
	{ delay_info_leave_conj(DelayInfo4, DelayedGoals, DelayInfo5) },
	mode_info_set_delay_info(DelayInfo5),

	( { DelayedGoals = [] } ->
		[]
	; { DelayedGoals = [delayed_goal(_DVars, Error, _DGoal)] } ->
		mode_info_add_error(Error)
	;
		{ get_all_waiting_vars(DelayedGoals, Vars0) },
		{ sort(Vars0, Vars) },	% eliminate duplicates
		mode_info_error(Vars, mode_error_conj(DelayedGoals))
	).

:- pred mode_info_add_goals_live_vars(list(hlds__goal), mode_info, mode_info).
:- mode mode_info_add_goals_live_vars(in, mode_info_di, mode_info_uo).

mode_info_add_goals_live_vars([]) --> [].
mode_info_add_goals_live_vars([Goal | Goals]) -->
	{ goal_get_nonlocals(Goal, Vars) },
	mode_info_add_live_vars(Vars),
	mode_info_add_goals_live_vars(Goals).

:- pred modecheck_conj_list_2(list(hlds__goal), list(hlds__goal),
				mode_info, mode_info).
:- mode modecheck_conj_list_2(in, in, mode_info_di, mode_info_uo) is det.

modecheck_conj_list_2([], []) --> [].
modecheck_conj_list_2([Goal0 | Goals0], Goals) -->
	=(ModeInfo0),
	{ goal_get_nonlocals(Goal0, NonLocalVars) },
	mode_info_remove_live_vars(NonLocalVars),
	modecheck_goal(Goal0, Goal),
	=(ModeInfo1),
	{ mode_info_get_errors(ModeInfo1, Errors) },
	( { Errors = [] } ->
		{ Goals = [Goal | Goals1] },
		{ mode_info_get_delay_info(ModeInfo1, DelayInfo0) },
		(
			{ delay_info_wakeup_goal(DelayInfo0, WokenGoal,
				DelayInfo) }
		->
			mode_checkpoint(wakeup, "goal"),
			mode_info_set_delay_info(DelayInfo),
			modecheck_conj_list_2([WokenGoal | Goals0], Goals1)
		;
			modecheck_conj_list_2(Goals0, Goals1)
		)
	;
			% If we didn't manage to schedule the goal, then we
			% restore the original mode_info here.
		dcg_set_state(ModeInfo0),

		{ Errors = [ FirstError | _] },
		{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },
		{ delay_info_delay_goal(DelayInfo0, FirstError, Goal0,
					DelayInfo) },
		mode_info_set_delay_info(DelayInfo),
		modecheck_conj_list_2(Goals0, Goals)
	).

:- pred dcg_set_state(T, T, T).
:- mode dcg_set_state(in, in, out).

dcg_set_state(Val, _OldVal, Val).

	% Given an association list of Vars - Goals,
	% combine all the Vars together into a single list.

:- pred get_all_waiting_vars(list(delayed_goal), list(var)).
:- mode get_all_waiting_vars(in, out).

get_all_waiting_vars([], []).
get_all_waiting_vars([delayed_goal(Vars, _, _) | Rest], List) :-
	append(Vars, List0, List),
	get_all_waiting_vars(Rest, List0).

	% Schedule a conjunction.
	% If it's empty, then there is nothing to do.
	% For non-empty conjunctions, we attempt to schedule the first
	% goal in the conjunction.  If successful, we wakeup a newly
	% pending goal (if any), and if not, we delay the goal.  Then we
	% continue attempting to schedule all the rest of the goals.

%-----------------------------------------------------------------------------%

	% XXX we don't handle disjunctions or if-then-else yet

:- pred modecheck_disj_list(list(hlds__goal), list(hlds__goal), list(instmap),
				mode_info, mode_info).
:- mode modecheck_disj_list(in, out, out, mode_info_di, mode_info_uo).

modecheck_disj_list([], [], []) --> [].
modecheck_disj_list([Goal0 | Goals0], [Goal | Goals], [InstMap | InstMaps]) -->
	mode_info_dcg_get_instmap(InstMap0),
	modecheck_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	modecheck_disj_list(Goals0, Goals, InstMaps).

	% instmap_merge(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables, 
	%	checking that it is the same for every branch.

:- type merge_context
	---> disj
	;    if_then_else.

:- pred instmap_merge(set(var), list(instmap), merge_context,
		mode_info, mode_info).
:- mode instmap_merge(in, in, in, mode_info_di, mode_info_uo).

instmap_merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	get_reachable_instmaps(InstMapList, InstMappingList),
	( InstMappingList = [] ->
		InstMap = unreachable
	; InstMap0 = reachable(InstMapping0) ->
		set__to_sorted_list(NonLocals, NonLocalsList),
		instmap_merge_2(NonLocalsList, InstMapList, ModuleInfo,
					InstMapping0, InstMapping, ErrorList),
		( ErrorList = [] ->
			ModeInfo2 = ModeInfo0
		;
			ErrorList = [Var - _|_], 
			mode_info_error([Var],
				mode_error_disj(MergeContext, ErrorList),
				ModeInfo0, ModeInfo2
			)
		),
		InstMap = reachable(InstMapping)
	;
		InstMap = unreachable
	),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo).

:- pred get_reachable_instmaps(list(instmap), list(map(var,inst))).
:- mode get_reachable_instmaps(in, out) is det.

:- get_reachable_instmaps([], _) when ever.
:- get_reachable_instmaps([X|_], _) when X.

get_reachable_instmaps([], _).
get_reachable_instmaps([reachable(InstMapping) | InstMaps], Reachables) :-
	Reachables = [InstMapping | Reachables1],
	get_reachable_instmaps(InstMaps, Reachables1).
get_reachable_instmaps([unreachable | InstMaps], Reachables) :-
	get_reachable_instmaps(InstMaps, Reachables).

%-----------------------------------------------------------------------------%

	% instmap_merge_2(Vars, InstMaps, ModuleInfo, ErrorList):
	%	Let `ErrorList' be the list of variables in `Vars' for
	%	there are two instmaps in `InstMaps' for which the inst
	%	the variable is incompatible.

:- type merge_errors == assoc_list(var, list(inst)).

:- pred instmap_merge_2(list(var), list(instmap), module_info,
			map(var, inst), map(var, inst), merge_errors).
:- mode instmap_merge_2(in, in, in, in, out, out) is det.

instmap_merge_2([], _, _, InstMap, InstMap, []).
instmap_merge_2([Var|Vars], InstMapList, ModuleInfo, InstMap0,
			InstMap, ErrorList) :-
	instmap_merge_2(Vars, InstMapList, ModuleInfo, InstMap0,
			InstMap1, ErrorList1),
	instmap_merge_var(InstMapList, Var, ModuleInfo, Insts, Inst, Error),
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
				list(inst), inst, bool).
:- mode instmap_merge_var(in, in, in, out, out, out) is det.

instmap_merge_var([], _, _, [], not_reached, no).
instmap_merge_var([InstMap | InstMaps], Var, ModuleInfo, InstList, Inst, Error)
		:-
	instmap_merge_var(InstMaps, Var, ModuleInfo, InstList0, Inst0, Error0),
	instmap_lookup_var(InstMap, Var, VarInst),
	InstList = [VarInst | InstList0],
	( inst_merge(Inst0, VarInst, ModuleInfo, Inst1) ->
		Inst = Inst1,
		Error = Error0
	;
		Error = yes,
		Inst = not_reached
	).

%-----------------------------------------------------------------------------%

:- pred modecheck_call_pred(pred_id, list(term), proc_id, mode_info, mode_info).
:- mode modecheck_call_pred(in, in, in, mode_info_di, mode_info_uo) is det.

modecheck_call_pred(PredId, Args, TheProcId, ModeInfo0, ModeInfo) :-
	term_list_to_var_list(Args, ArgVars),

		% Get the list of different possible modes for the called
		% predicate
	mode_info_get_preds(ModeInfo0, Preds),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__keys(Procs, ProcIds),

		% In order to give better diagnostics, we handle the
		% case where there is only one mode for the called predicate
		% specially.
	(
		ProcIds = [ProcId]
	->
		TheProcId = ProcId,
		map__lookup(Procs, ProcId, ProcInfo),
		proc_info_argmodes(ProcInfo, ProcArgModes),
		mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
					InitialInsts),
		modecheck_var_has_inst_list(ArgVars, InitialInsts,
					ModeInfo0, ModeInfo1),
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars, FinalInsts,
					ModeInfo1, ModeInfo)
	;
			% set the current error list to empty (and
			% save the old one in `OldErrors').  This is so the
			% test for `Errors = []' in call_pred_2 will work.
		mode_info_get_errors(ModeInfo0, OldErrors),
		mode_info_set_errors([], ModeInfo0, ModeInfo1),

		modecheck_call_pred_2(ProcIds, Procs, ArgVars,
			[], TheProcId, ModeInfo1, ModeInfo2),

			% restore the error list, appending any new error(s)
		mode_info_get_errors(ModeInfo2, NewErrors),
		append(OldErrors, NewErrors, Errors),
		mode_info_set_errors(Errors, ModeInfo2, ModeInfo)
	).

:- pred modecheck_call_pred_2(list(proc_id), proc_table, list(var), list(var),
				proc_id, mode_info, mode_info).
:- mode modecheck_call_pred_2(in, in, in, in, out, mode_info_di, mode_info_uo)
	is det.

modecheck_call_pred_2([], _Procs, ArgVars, WaitingVars, 0, ModeInfo0,
		ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	get_var_insts(ArgVars, InstMap, ArgInsts),
	mode_info_error(WaitingVars,
		mode_error_no_matching_mode(ArgVars, ArgInsts),
		ModeInfo0, ModeInfo).
	
modecheck_call_pred_2([ProcId | ProcIds], Procs, ArgVars, WaitingVars,
			TheProcId, ModeInfo0, ModeInfo) :-

		% find the initial insts for this mode of the called pred
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_argmodes(ProcInfo, ProcArgModes),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),

		% check whether the insts of the args matches their expected
		% initial insts
	modecheck_var_has_inst_list(ArgVars, InitialInsts,
				ModeInfo0, ModeInfo1),
	mode_info_get_errors(ModeInfo1, Errors),
	(
		Errors = [] 
	->
			% if so, then set their insts to the final insts
			% specified in the mode for the called pred
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars, FinalInsts, ModeInfo1,
			ModeInfo),
		TheProcId = ProcId
	;
			% otherwise, keep trying with the other modes
			% for the called pred
		Errors = [mode_error_info(WaitingVars2, _, _, _) | _],
		append(WaitingVars2, WaitingVars, WaitingVars3),

		modecheck_call_pred_2(ProcIds, Procs, ArgVars, WaitingVars3,
				TheProcId, ModeInfo0, ModeInfo)
	).

:- pred get_var_insts(list(var), instmap, list(inst)).
:- mode get_var_insts(in, in, out).

get_var_insts([], _, []).
get_var_insts([Var | Vars], InstMap, [Inst | Insts]) :-
	instmap_lookup_var(InstMap, Var, Inst),
	get_var_insts(Vars, InstMap, Insts).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of initial insts, ensure
	% that the inst of each variable matches the corresponding initial
	% inst.

:- pred modecheck_var_has_inst_list(list(var), list(inst), mode_info,
					mode_info).
:- mode modecheck_var_has_inst_list(in, in, mode_info_di, mode_info_uo) is det.

modecheck_var_has_inst_list([], []) --> [].
modecheck_var_has_inst_list([Var|Vars], [Inst|Insts]) -->
	modecheck_var_has_inst(Var, Inst),
	modecheck_var_has_inst_list(Vars, Insts).

:- pred modecheck_var_has_inst(var, inst, mode_info, mode_info).
:- mode modecheck_var_has_inst(in, in, mode_info_di, mode_info_uo) is det.

modecheck_var_has_inst(VarId, Inst, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	instmap_lookup_var(InstMap, VarId, VarInst),

	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	( inst_matches_initial(VarInst, Inst, ModuleInfo) ->
		ModeInfo = ModeInfo0
	;
		mode_info_error([VarId],
			mode_error_var_has_inst(VarId, VarInst, Inst),
			ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

	% inst_matches_initial(InstA, InstB, ModuleInfo) is true iff
	% `InstA' is at least as instantiated as `InstB'.
	% (`not_reached' is considered to be more instantiated
	% than `ground'.)

:- pred inst_matches_initial(inst, inst, module_info).
:- mode inst_matches_initial(in, in, in) is semidet.

inst_matches_initial(InstA, InstB, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_initial_2(InstA, InstB, ModuleInfo, Expansions).

:- type expansions == set(pair(inst)).

:- pred inst_matches_initial_2(inst, inst, module_info, expansions).
:- mode inst_matches_initial_2(in, in, in, in) is semidet.

inst_matches_initial_2(InstA, InstB, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_initial_3(InstA2, InstB2, ModuleInfo, Expansions2)
	).

:- pred inst_matches_initial_3(inst, inst, module_info, expansions).
:- mode inst_matches_initial_3(in, in, in, in) is semidet.

:- inst_matches_initial_3(InstA, InstB, _, _) when InstA and InstB. % Indexing.

	% inst_matches_initial_3(InstA, InstB, ModuleInfo, Expansions)
	% is true iff `InstA' is at least as instantiated as `InstB'.
	% To avoid infinite regress, we assume that
	% inst_matches_initial is true for any pairs of insts which
	% occur in Expansions.

inst_matches_initial_3(free, free, _, _).
inst_matches_initial_3(bound(_List), free, _, _).
inst_matches_initial_3(bound(ListA), bound(ListB), ModuleInfo, Expansions) :-
	bound_inst_list_matches_initial(ListA, ListB, ModuleInfo, Expansions).
inst_matches_initial_3(bound(List), ground, ModuleInfo, _) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_matches_initial_3(bound(List), abstract_inst(_, _), ModuleInfo, _) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_matches_initial_3(ground, Inst, _, _) :- Inst \= not_reached.
inst_matches_initial_3(abstract_inst(_Name, _Args), free, _, _).
inst_matches_initial_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
				ModuleInfo, Expansions) :-
	inst_list_matches_initial(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_initial_3(not_reached, _, _, _).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_initial(list(bound_inst), list(bound_inst),
					module_info, expansions).
:- mode bound_inst_list_matches_initial(in, in, in, in) is semidet.

bound_inst_list_matches_initial([], _, _, _).
bound_inst_list_matches_initial([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	X = functor(NameX, ArgsX),
	Y = functor(NameY, ArgsY),
	length(ArgsX, ArityX),
	length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
		inst_list_matches_initial(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_initial(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, X, Y),
			% NameY/ArityY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of NameY/ArityY, and hence 
			% automatically matches_initial Y.  We just need to
			% check that [X|Xs] matches_initial Ys.
		bound_inst_list_matches_initial([X|Xs], Ys, ModuleInfo,
					Expansions)
	).

:- pred inst_list_matches_initial(list(inst), list(inst), module_info,
				expansions).
:- mode inst_list_matches_initial(in, in, in, in) is semidet.

inst_list_matches_initial([], [], _, _).
inst_list_matches_initial([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	inst_matches_initial_2(X, Y, ModuleInfo, Expansions),
	inst_list_matches_initial(Xs, Ys, ModuleInfo, Expansions).

%-----------------------------------------------------------------------------%

:- pred inst_expand(module_info, inst, inst).
:- mode inst_expand(in, in, out) is det.

inst_expand(ModuleInfo, Inst0, Inst) :-
	( Inst0 = user_defined_inst(Name, Args) ->
		inst_lookup(ModuleInfo, Name, Args, Inst1),
		inst_expand(ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

:- pred modecheck_set_term_inst_list(list(term), list(inst),
					mode_info, mode_info).
:- mode modecheck_set_term_inst_list(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_term_inst_list([], []) --> [].
modecheck_set_term_inst_list([Arg | Args], [Inst | Insts]) -->
	{ Arg = term__variable(Var) },
	modecheck_set_var_inst(Var, Inst),
	modecheck_set_term_inst_list(Args, Insts).

:- pred modecheck_set_var_inst_list(list(var), list(inst),
					mode_info, mode_info).
:- mode modecheck_set_var_inst_list(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst_list([], []) --> [].
modecheck_set_var_inst_list([Var | Vars], [Inst | Insts]) -->
	modecheck_set_var_inst(Var, Inst),
	modecheck_set_var_inst_list(Vars, Insts).

:- pred modecheck_set_var_inst(var, inst, mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var, Inst, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(InstMapping0) ->
		mode_info_get_module_info(ModeInfo0, ModuleInfo),
		instmap_lookup_var(InstMap0, Var, Inst0),
		( inst_matches_initial(Inst0, Inst, ModuleInfo) ->
			ModeInfo = ModeInfo0
		; mode_info_var_is_locked(ModeInfo0, Var) ->
			mode_info_error([Var],
					mode_error_bind_var(Var, Inst0, Inst),
					ModeInfo0, ModeInfo
			)
		;
			map__set(InstMapping0, Var, Inst, InstMapping),
			InstMap = reachable(InstMapping),
			mode_info_set_instmap(InstMap, ModeInfo0, ModeInfo1),
			mode_info_get_delay_info(ModeInfo1, DelayInfo0),
			delay_info_bind_var(DelayInfo0, Var, DelayInfo),
			mode_info_set_delay_info(DelayInfo, ModeInfo1, ModeInfo)
		)
	;
		ModeInfo = ModeInfo0
	).

%-----------------------------------------------------------------------------%

	% inst_merge(InstA, InstB, InstC):
	%	Combine the insts found in different arms of a
	%	disjunction (or if-then-else).

:- pred inst_merge(inst, inst, module_info, inst).
:- mode inst_merge(in, in, in, out) is semidet.

inst_merge(InstA, InstB, ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_merge_2(InstA, InstB, ModuleInfo, Expansions, Inst).

:- pred inst_merge_2(inst, inst, module_info, expansions, inst).
:- mode inst_merge_2(in, in, in, in, out) is semidet.

inst_merge_2(InstA, InstB, ModuleInfo, Expansions, Inst) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		error("not implemented: inst_merge of recursive insts")
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		( InstB2 = not_reached ->
			Inst = InstA2
		;
			inst_merge_3(InstA2, InstB2, ModuleInfo, Expansions2,
				Inst)
		)
	).

:- pred inst_merge_3(inst, inst, module_info, expansions, inst).
:- mode inst_merge_3(in, in, in, in, out) is semidet.

:- inst_merge_3(A, B, _, _, _) when A and B.

inst_merge_3(free, free, _, _, free).
inst_merge_3(bound(ListA), bound(ListB), ModuleInfo, Expansions, bound(List)) :-
	bound_inst_list_merge(ListA, ListB, ModuleInfo, Expansions, List).
inst_merge_3(bound(ListA), ground, ModuleInfo, _Expansions, ground) :-
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_merge_3(ground, bound(ListB), ModuleInfo, _Expansions, ground) :-
	bound_inst_list_is_ground(ListB, ModuleInfo).
inst_merge_3(ground, ground, _, _, ground).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions, abstract_inst(Name, Args)) :-
	inst_list_merge(ArgsA, ArgsB, ModuleInfo, Expansions, Args).
inst_merge_3(not_reached, Inst, _, _, Inst).

:- pred inst_list_merge(list(inst), list(inst), module_info, expansions,
			list(inst)).
:- mode inst_list_merge(in, in, in, in, out) is semidet.

inst_list_merge([], [], _ModuleInfo, _, []).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo, Expansions,
		[Arg | Args]) :-
	inst_merge_2(ArgA, ArgB, ModuleInfo, Expansions, Arg),
	inst_list_merge(ArgsA, ArgsB, ModuleInfo, Expansions, Args).

	% bound_inst_list_merge(Xs, Ys, ModuleInfo, Exps, Zs):
	% The two input lists Xs and Ys must already be sorted.
	% Here we perform a sorted merge operation,
	% so that the functors of the output list Zs are the union
	% of the functors of the input lists Xs and Ys.

:- pred bound_inst_list_merge(list(bound_inst), list(bound_inst),
				module_info, expansions, list(bound_inst)).
:- mode bound_inst_list_merge(in, in, in, in, out) is semidet.

bound_inst_list_merge(Xs, Ys, ModuleInfo, Expansions, Zs) :-
	( Xs = [] ->
		Zs = Ys
	; Ys = [] ->
		Zs = Xs
	;
		Xs = [X | Xs1],
		Ys = [Y | Ys1],
		X = functor(NameX, ArgsX),
		Y = functor(NameY, ArgsY),
		length(ArgsX, ArityX),
		length(ArgsY, ArityY),
		( NameX = NameY, ArityX = ArityY ->
			inst_list_merge(ArgsX, ArgsY, ModuleInfo, Expansions,
					Args),
			Z = functor(NameX, Args),
			Zs = [Z | Zs1],
			bound_inst_list_merge(Xs1, Ys1, ModuleInfo, Expansions,
				Zs1)
		; compare(<, X, Y) ->
			Zs = [X | Zs1],
			bound_inst_list_merge(Xs1, Ys, ModuleInfo,
						Expansions, Zs1)
		;
			Zs = [Y | Zs1],
			bound_inst_list_merge(Xs, Ys1, ModuleInfo,
						Expansions, Zs1)
		)
	).

%-----------------------------------------------------------------------------%

	% inst_matches_final(InstA, InstB):
	%	Succeed iff InstA is compatible with InstB,
	%	i.e. iff InstA will satisfy the final inst
	%	requirement InstB.
	%	The difference between inst_matches_initial and
	%	inst_matches_final is that inst_matches_initial requires
	%	only something which is at least as instantiated,
	%	whereas this predicate wants something which is an
	%	exact match (or not reachable).
	%	Note that this predicate is not symmetric,
	%	because of the existence of `not_reached' insts:
	%	not_reached matches_final with anything,
	%	but not everything matches_final with not_reached -
	%	in fact only not_reached matches_final with not_reached.

	% It would be a good idea to fold inst_matches_initial and
	% inst_matches_final into a single predicate inst_matches(When, ...)
	% where When is either `initial' or `final'.

:- pred inst_matches_final(inst, inst, module_info).
:- mode inst_matches_final(in, in, in) is semidet.

inst_matches_final(InstA, InstB, ModuleInfo) :-
	set__init(Expansions),
	inst_matches_final_2(InstA, InstB, ModuleInfo, Expansions).

:- pred inst_matches_final_2(inst, inst, module_info, expansions).
:- mode inst_matches_final_2(in, in, in, in) is semidet.

inst_matches_final_2(InstA, InstB, ModuleInfo, Expansions) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		true
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		inst_matches_final_3(InstA2, InstB2, ModuleInfo,
			Expansions2)
	).

:- pred inst_matches_final_3(inst, inst, module_info, expansions).
:- mode inst_matches_final_3(in, in, in, in) is semidet.

:- inst_matches_final_3(A, B, _, _) when A and B.

inst_matches_final_3(free, free, _, _).
inst_matches_final_3(bound(ListA), bound(ListB), ModuleInfo, Expansions) :-
	bound_inst_list_matches_final(ListA, ListB, ModuleInfo, Expansions).
inst_matches_final_3(bound(ListA), ground, ModuleInfo, _Expansions) :-
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_matches_final_3(ground, bound(ListB), ModuleInfo, _Expansions) :-
	bound_inst_list_is_ground(ListB, ModuleInfo),
		% XXX Bug! Should fail if there are not_reached insts in
		% ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	error("not implemented: `ground' matches_final `bound(...)'").
inst_matches_final_3(ground, ground, _, _).
inst_matches_final_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo, Expansions) :-
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo, Expansions).
inst_matches_final_3(not_reached, _, _, _).

:- pred inst_list_matches_final(list(inst), list(inst), module_info,
				expansions).
:- mode inst_list_matches_final(in, in, in, in) is semidet.

inst_list_matches_final([], [], _ModuleInfo, _).
inst_list_matches_final([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo,
			Expansions) :-
	inst_matches_final_2(ArgA, ArgB, ModuleInfo, Expansions),
	inst_list_matches_final(ArgsA, ArgsB, ModuleInfo, Expansions).

	% Here we check that the functors in the first list are a
	% subset of the functors in the second list. 
	% (If a bound(...) inst only specifies the insts for some of
	% the constructors of its type, then it implicitly means that
	% all other constructors must have all their arguments
	% `not_reached'.)
	% The code here makes use of the fact that the bound_inst lists
	% are sorted.

:- pred bound_inst_list_matches_final(list(bound_inst), list(bound_inst),
					module_info, expansions).
:- mode bound_inst_list_matches_final(in, in, in, in) is semidet.

bound_inst_list_matches_final([], _, _, _).
bound_inst_list_matches_final([X|Xs], [Y|Ys], ModuleInfo, Expansions) :-
	X = functor(NameX, ArgsX),
	Y = functor(NameY, ArgsY),
	length(ArgsX, ArityX),
	length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
		inst_list_matches_final(ArgsX, ArgsY, ModuleInfo, Expansions),
		bound_inst_list_matches_final(Xs, Ys, ModuleInfo, Expansions)
	;
		compare(>, X, Y),
			% NameY/ArityY does not occur in [X|Xs].
			% Hence [X|Xs] implicitly specifies `not_reached'
			% for the args of NameY/ArityY, and hence 
			% automatically matches_final Y.  We just need to
			% check that [X|Xs] matches_final Ys.
		bound_inst_list_matches_final([X|Xs], Ys, ModuleInfo,
					Expansions)
	).

%-----------------------------------------------------------------------------%

	% This code is used to trace the actions of the mode checker.

:- type port
	--->	enter
	;	exit
	;	wakeup.

:- pred mode_checkpoint(port, string, mode_info, mode_info).
:- mode mode_checkpoint(in, in, mode_info_di, mode_info_uo).

mode_checkpoint(Port, Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
        lookup_option(debug, bool(DoCheckPoint), IOState0, IOState1),
	( DoCheckPoint = yes ->
		mode_checkpoint_2(Port, Msg, ModeInfo0, IOState1, IOState)
	;
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred mode_checkpoint_2(port, string, mode_info, io__state, io__state).
:- mode mode_checkpoint_2(in, in, mode_info_ui, di, uo).

mode_checkpoint_2(Port, Msg, ModeInfo) -->
	{ mode_info_get_errors(ModeInfo, Errors) },
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
		lookup_option(statistics, bool(Statistics)),
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
:- mode write_var_insts(in, in, in, di, uo).

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

:- pred modecheck_unification(term, term, pair(mode, mode), unification,
				mode_info, mode_info).
:- mode modecheck_unification(in, in, out, out, mode_info_di, mode_info_uo).

modecheck_unification(term__variable(X), term__variable(Y), Modes, Unification,
			ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	instmap_lookup_var(InstMap0, Y, InstY),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_is_live(ModeInfo0, Y, LiveY),
	(
		( LiveX = live, LiveY = live ->
			abstractly_unify_inst(live, InstX, InstY, ModuleInfo,
				UnifyInst)
		;
			abstractly_unify_inst(dead, InstX, InstY, ModuleInfo,
				UnifyInst)
		)
	->
		Inst = UnifyInst,
		ModeInfo1 = ModeInfo0
	;
		mode_info_error( [X, Y],
				mode_error_unify_var_var(X, Y, InstX, InstY),
					ModeInfo0, ModeInfo1),
			% If we get an error, set the inst to not_reached
			% to suppress follow-on errors
		Inst = not_reached
	),
	modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
	modecheck_set_var_inst(Y, Inst, ModeInfo2, ModeInfo),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Modes = ModeX - ModeY,
	mode_info_get_var_types(ModeInfo, VarTypes),
	categorize_unify_var_var(ModeX, ModeY, X, Y, VarTypes, ModuleInfo,
		Unification).

modecheck_unification(term__variable(X), term__functor(Name, Args, _),
			Mode, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	instmap_lookup_arg_list(Args, InstMap0, InstArgs),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	term_list_to_var_list(Args, ArgVars),
	mode_info_var_list_is_live(ArgVars, ModeInfo0, LiveArgs),
	InstY = bound([functor(Name, InstArgs)]),
	(
		abstractly_unify_inst_functor(LiveX, InstX, Name,
			InstArgs, LiveArgs, ModuleInfo, UnifyInst)
	->
		Inst = UnifyInst,
		ModeInfo1 = ModeInfo0
	;
		term_list_to_var_list(Args, ArgVars),
		mode_info_error(
			[X | ArgVars],
			mode_error_unify_var_functor(X, Name, Args,
							InstX, InstArgs),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
		Inst = not_reached
	),
	modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
	bind_args(Inst, Args, ModeInfo2, ModeInfo),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Mode = ModeX - ModeY,
	get_mode_of_args(Inst, InstArgs, ModeArgs),
	categorize_unify_var_functor(ModeX, ModeArgs, X, Name, Args,
			ModuleInfo, Unification).

modecheck_unification(term__functor(F, As, _), term__variable(Y),
		Modes, Unification, ModeInfo0, ModeInfo) :-
	modecheck_unification(term__variable(Y), term__functor(F, As, _),
		Modes, Unification, ModeInfo0, ModeInfo).
	
modecheck_unification(term__functor(_, _, _), term__functor(_, _, _),
		_, _, _, _) :-
	error("modecheck internal error: unification of term with term\n").

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(term), mode_info, mode_info).
:- mode bind_args(in, in, mode_info_di, mode_info_uo) is det.

		% This first clause shouldn't be necessary, but it is
		% until the code above marked "Loses information" get fixed.
bind_args(not_reached, _) -->
	mode_info_set_instmap(unreachable).
bind_args(ground, Args) -->
	ground_args(Args).
bind_args(bound(List), Args) -->
	( { List = [] } ->
		% the code is unreachable
		mode_info_set_instmap(unreachable)
	;
		{ List = [functor(_, InstList)] },
		bind_args_2(Args, InstList)
	).

:- pred bind_args_2(list(term), list(inst), mode_info, mode_info).
:- mode bind_args_2(in, in, mode_info_di, mode_info_uo).

bind_args_2([], []) --> [].
bind_args_2([Arg | Args], [Inst | Insts]) -->
	{ Arg = term__variable(Var) },
	modecheck_set_var_inst(Var, Inst),
	bind_args_2(Args, Insts).

:- pred ground_args(list(term), mode_info, mode_info).
:- mode ground_args(in, mode_info_di, mode_info_uo).

ground_args([]) --> [].
ground_args([Arg | Args]) -->
	{ Arg = term__variable(Var) },
	modecheck_set_var_inst(Var, ground),
	ground_args(Args).

%-----------------------------------------------------------------------------%

:- pred get_mode_of_args(inst, list(inst), list(mode)).
:- mode get_mode_of_args(in, in, out) is det.

get_mode_of_args(not_reached, ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, not_reached, ArgModes).
get_mode_of_args(ground, ArgInsts, ArgModes) :-
	mode_set_args(ArgInsts, ground, ArgModes).
get_mode_of_args(bound(List), ArgInstsA, ArgModes) :-
	( List = [] ->
		% the code is unreachable
		mode_set_args(ArgInstsA, not_reached, ArgModes)
	;
		List = [functor(_Name, ArgInstsB)],
		get_mode_of_args_2(ArgInstsA, ArgInstsB, ArgModes)
	).

:- pred get_mode_of_args_2(list(inst), list(inst), list(mode)).
:- mode get_mode_of_args_2(in, in, out).

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

	% Mode checking is like abstract interpretation.
	% This is the abstract unification operation which
	% unifies two instantiatednesses.  If the unification
	% would be illegal, then abstract unification fails.
	% If the unification would fail, then the abstract unification
	% will succeed, and the resulting instantiatedness will be
	% `not_reached'.

	% Abstractly unify two insts.

:- pred abstractly_unify_inst(is_live, inst, inst, module_info, inst).
:- mode abstractly_unify_inst(in, in, in, in, out) is semidet.

abstractly_unify_inst(Live, InstA, InstB, ModuleInfo, Inst) :-
	set__init(Expansions),
	abstractly_unify_inst_2(Live, InstA, InstB, ModuleInfo, Expansions,
				Inst).

:- pred abstractly_unify_inst_2(is_live, inst, inst, module_info, expansions,
				inst).
:- mode abstractly_unify_inst_2(in, in, in, in, in, out) is semidet.

abstractly_unify_inst_2(Live, InstA, InstB, ModuleInfo, Expansions, Inst) :-
	ThisExpansion = InstA - InstB,
	( set__member(ThisExpansion, Expansions) ->
		error(
		    "not implemented: abstractly_unify_inst of recursive insts"
		)
	;
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, ThisExpansion, Expansions2),
		( InstB2 = not_reached ->
			Inst = InstA2
		;
			abstractly_unify_inst_3(Live, InstA2, InstB2,
				ModuleInfo, Expansions2, Inst)
		)
	).

	% Abstractly unify two expanded insts.
	% The is_live parameter is `live' iff *both* insts are live.
	% XXX handle `not_reached' insts.

:- pred abstractly_unify_inst_3(is_live, inst, inst, module_info, expansions,
				inst).
:- mode abstractly_unify_inst_3(in, in, in, in, in, out) is semidet.

:- abstractly_unify_inst_3(A, B, C, _, _, _) when A and B and C.

abstractly_unify_inst_3(live, free,	free,		_, _, _) :- fail.
abstractly_unify_inst_3(live, free,	bound(List),	M, _, bound(List)) :-
	bound_inst_list_is_ground(List, M).
abstractly_unify_inst_3(live, free,	ground,		_, _, ground).
abstractly_unify_inst_3(live, free,	abstract_inst(_,_), _, _, _) :- fail.
	
abstractly_unify_inst_3(live, bound(List), free,	M, _, bound(List)) :-
	bound_inst_list_is_ground(List, M).
abstractly_unify_inst_3(live, bound(ListX),bound(ListY), M, Exs, bound(List)) :-
	abstractly_unify_bound_inst_list(live, ListX, ListY, M, Exs, List).
abstractly_unify_inst_3(live, bound(_),	ground,		_, _, ground).
abstractly_unify_inst_3(live, bound(List), abstract_inst(_,_), ModuleInfo, _,
							   ground) :-
	bound_inst_list_is_ground(List, ModuleInfo).

abstractly_unify_inst_3(live, ground,	_,		_, _, ground).

abstractly_unify_inst_3(live, abstract_inst(_,_), free,	_, _, _) :- fail.
abstractly_unify_inst_3(live, abstract_inst(_,_), bound(List), ModuleInfo, _,
				ground) :-
	bound_inst_list_is_ground(List, ModuleInfo).
abstractly_unify_inst_3(live, abstract_inst(_,_), ground, _, _, ground).
abstractly_unify_inst_3(live, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo, Expansions, 
			abstract_inst(Name, Args)) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, live, ModuleInfo, Expansions,
		Args).

abstractly_unify_inst_3(dead, free, Inst, _, _, Inst).
	
abstractly_unify_inst_3(dead, bound(List), free, _, _, bound(List)).

abstractly_unify_inst_3(dead, bound(ListX), bound(ListY), M, Exs, bound(List))
		:-
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M, Exs, List).
abstractly_unify_inst_3(dead, bound(_), ground, _, _, ground).
abstractly_unify_inst_3(dead, bound(List), abstract_inst(N,As), ModuleInfo, _,
							   Result) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = ground
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As)
	;
		fail
	).

abstractly_unify_inst_3(dead, ground,		_,		_, _, ground).

abstractly_unify_inst_3(dead, abstract_inst(N,As), abstract_inst(N,As), _, _, _)
						:- fail.
abstractly_unify_inst_3(dead, abstract_inst(N,As), bound(List), ModuleInfo, _,
							Result) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = ground
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As)
	;
		fail
	).
abstractly_unify_inst_3(dead, abstract_inst(_,_), ground,	_, _, ground).
abstractly_unify_inst_3(dead, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo, Expansions,
			abstract_inst(Name, Args)) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, dead, ModuleInfo, Expansions,
			Args).

%-----------------------------------------------------------------------------%

	% Abstractly unify two inst lists.

:- pred abstractly_unify_inst_list(list(inst), list(inst), is_live, module_info,
					expansions, list(inst)).
:- mode abstractly_unify_inst_list(in, in, in, in, in, out).

abstractly_unify_inst_list([], [], _, _, _, []).
abstractly_unify_inst_list([X|Xs], [Y|Ys], Live, ModuleInfo, Expansions,
				[Z|Zs]) :-
	abstractly_unify_inst_2(Live, X, Y, ModuleInfo, Expansions, Z),
	abstractly_unify_inst_list(Xs, Ys, Live, ModuleInfo, Expansions, Zs).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.

:- pred abstractly_unify_inst_functor(is_live, inst, const, list(inst),
					list(is_live), module_info, inst).
:- mode abstractly_unify_inst_functor(in, in, in, in, in, in, out) is semidet.

abstractly_unify_inst_functor(Live, InstA, Name, ArgInsts, ArgLives,
		ModuleInfo, Inst) :-
	inst_expand(ModuleInfo, InstA, InstA2),
	abstractly_unify_inst_functor_2(Live, InstA2, Name, ArgInsts, ArgLives,
			ModuleInfo, Inst).

:- pred abstractly_unify_inst_functor_2(is_live, inst, const, list(inst),
					list(is_live), module_info, inst).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, in, in, out) is semidet.

abstractly_unify_inst_functor_2(live, free, Name, Args, ArgLives, ModuleInfo,
			bound([functor(Name, Args)])) :-
	inst_list_is_ground_or_dead(Args, ArgLives, ModuleInfo).
abstractly_unify_inst_functor_2(live, bound(ListX), Name, Args, ArgLives, M,
		bound(List)) :-
	abstractly_unify_bound_inst_list_lives(ListX, Name, Args, ArgLives,
						M, List).
		% XXX loses information:
abstractly_unify_inst_functor_2(live, ground, _Name, _Args, _, _, ground).
abstractly_unify_inst_functor_2(live, abstract_inst(_,_), _, _, _, _, _) :-
	fail.

abstractly_unify_inst_functor_2(dead, free, Name, Args, _ArgLives, _ModuleInfo,
			bound([functor(Name, Args)])).
abstractly_unify_inst_functor_2(dead, bound(ListX), Name, Args, _ArgLives, M,
		bound(List)) :-
	ListY = [functor(Name, Args)],
	set__init(Expansions),
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M, Expansions,
		List). 
		% XXX loses information:
abstractly_unify_inst_functor_2(dead, ground, _Name, _Args, _, _, ground).
abstractly_unify_inst_functor_2(dead, abstract_inst(_,_), _, _, _, _, _) :-
	fail.

	% Given a list of insts, and a corresponding list of livenesses,
	% return true iff for every element in the list of insts, either
	% the elemement is ground or the corresponding element in the liveness
	% list is dead.

:- pred inst_list_is_ground_or_dead(list(inst), list(is_live), module_info).
:- mode inst_list_is_ground_or_dead(in, in, in) is semidet.

inst_list_is_ground_or_dead([], [], _).
inst_list_is_ground_or_dead([Inst | Insts], [Live | Lives], ModuleInfo) :-
	( Live = live ->
		inst_is_ground(ModuleInfo, Inst)
	;
		true
	),
	inst_list_is_ground_or_dead(Insts, Lives, ModuleInfo).

%-----------------------------------------------------------------------------%

	% This code performs abstract unification of two bound(...) insts.
	% like a sorted merge operation.  If two elements have the
	% The lists of bound_inst are guaranteed to be sorted.
	% Abstract unification of two bound(...) insts proceeds
	% like a sorted merge operation.  If two elements have the
	% same functor name, they are inserted in the output list
	% iff their argument inst list can be abstractly unified.

:- pred abstractly_unify_bound_inst_list(is_live, list(bound_inst),
		list(bound_inst), module_info, expansions, list(bound_inst)).
:- mode abstractly_unify_bound_inst_list(in, in, in, in, in, out).

:- abstractly_unify_bound_inst_list(_, Xs, Ys, _, _, _) when Xs and Ys. % Index

abstractly_unify_bound_inst_list(_, [], _, _ModuleInfo, _Expansions, []).
abstractly_unify_bound_inst_list(_, [_|_], [], _ModuleInfo,  _Expansions,[]).
abstractly_unify_bound_inst_list(Live, [X|Xs], [Y|Ys], ModuleInfo, Expansions,
		L) :-
	X = functor(NameX, ArgsX),
	length(ArgsX, ArityX),
	Y = functor(NameY, ArgsY),
	length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
	    ( abstractly_unify_inst_list(ArgsX, ArgsY, Live, ModuleInfo,
			Expansions, Args)
	    ->
		L = [functor(NameX, Args) | L1],
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
						ModuleInfo, Expansions, L1)
	    ;
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
						ModuleInfo, Expansions, L)
	    )
	;
	    ( compare(<, X, Y) ->
		abstractly_unify_bound_inst_list(Live, Xs, [Y|Ys],
						ModuleInfo, Expansions, L)
	    ;
		abstractly_unify_bound_inst_list(Live, [X|Xs], Ys,
						ModuleInfo, Expansions, L)
	    )
	).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst), const,
		list(inst), list(is_live), module_info, list(bound_inst)).
:- mode abstractly_unify_bound_inst_list_lives(in, in, in, in, in, out).

abstractly_unify_bound_inst_list_lives([], _, _, _, _ModuleInfo, []).
abstractly_unify_bound_inst_list_lives([X|Xs], NameY, ArgsY, LivesY,
		ModuleInfo, L) :-
	X = functor(NameX, ArgsX),
	length(ArgsX, ArityX),
	length(ArgsY, ArityY),
	( 
		NameX = NameY,
		ArityX = ArityY
	->
		abstractly_unify_inst_list_lives(ArgsX, ArgsY, LivesY,
			ModuleInfo, Args),
		L = [functor(NameX, Args)]
	;
		abstractly_unify_bound_inst_list_lives(Xs, NameY, ArgsY,
					LivesY, ModuleInfo, L)
	).

:- pred abstractly_unify_inst_list_lives(list(inst), list(inst), list(is_live),
					module_info, list(inst)).
:- mode abstractly_unify_inst_list_lives(in, in, in, in, out) is semidet.

abstractly_unify_inst_list_lives([], [], [], _, []).
abstractly_unify_inst_list_lives([X|Xs], [Y|Ys], [Live|Lives], ModuleInfo,
		[Z|Zs]) :-
	abstractly_unify_inst(Live, X, Y, ModuleInfo, Z),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, ModuleInfo, Zs).

%-----------------------------------------------------------------------------%

:- pred categorize_unify_var_var(mode, mode, var, var, map(var, type), 
				module_info, unification).
:- mode categorize_unify_var_var(in, in, in, in, in, in, out).

categorize_unify_var_var(ModeX, ModeY, X, Y, VarTypes, ModuleInfo,
		Unification) :-
	( mode_is_output(ModuleInfo, ModeX) ->
		Unification = assign(X, Y)
	; mode_is_output(ModuleInfo, ModeY) ->
		Unification = assign(Y, X)
	;
		map__lookup(VarTypes, X, Type),
		type_is_atomic(Type, ModuleInfo)
	->
		Unification = simple_test(X, Y)
	;
		Unification = complicated_unify(ModeX - ModeY,
				term__variable(X), term__variable(Y))
	).

:- pred categorize_unify_var_functor(mode, list(mode), var, const,
				list(term), module_info, unification).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, out).

categorize_unify_var_functor(ModeX, ArgModes, X, Name, Args, ModuleInfo,
		Unification) :-
	length(Args, Arity),
	make_functor_cons_id(Name, Arity, ConsId),
	term_list_to_var_list(Args, ArgVars),
	( mode_is_output(ModuleInfo, ModeX) ->
		Unification = construct(X, ConsId, ArgVars, ArgModes)
	; 
		Unification = deconstruct(X, ConsId, ArgVars, ArgModes)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% XXX - At the moment we don't check for circular modes or insts.
	% (If they aren't used, the compiler will probably not
	% detect the error; if they are, it will probably go into
	% an infinite loop).

:- pred check_circular_modes(module_info, module_info, io__state, io__state).
:- mode check_circular_modes(in, out, di, uo).

check_circular_modes(Module0, Module) -->
	{ Module = Module0 }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check for any possible undefined insts/modes.
	% Should we add a definition for undefined insts/modes?

:- pred check_undefined_modes(module_info, module_info, io__state, io__state).
:- mode check_undefined_modes(in, out, di, uo).
check_undefined_modes(Module, Module) -->
	{ module_info_insts(Module, InstDefns) },
	{ map__keys(InstDefns, InstIds) },
	find_undef_inst_bodies(InstIds, InstDefns),
	{ module_info_modes(Module, ModeDefns) },
	{ map__keys(ModeDefns, ModeIds) },
	find_undef_mode_bodies(ModeIds, ModeDefns, InstDefns),
	{ module_info_preds(Module, Preds) },
	{ module_info_predids(Module, PredIds) },
	find_undef_pred_modes(PredIds, Preds, ModeDefns, InstDefns).

	% Find any undefined insts/modes used in predicate mode declarations.

:- pred find_undef_pred_modes(list(pred_id), pred_table, mode_table,
				inst_table, io__state, io__state).
:- mode find_undef_pred_modes(in, in, in, in, di, uo).

find_undef_pred_modes([], _Preds, _ModeDefns, _InstDefns) --> [].
find_undef_pred_modes([PredId | PredIds], Preds, ModeDefns, InstDefns) -->
	{ map__search(Preds, PredId, PredDefn) },
	{ pred_info_procedures(PredDefn, Procs) },
	{ map__keys(Procs, ProcIds) },
	find_undef_proc_modes(ProcIds, PredId, Procs, ModeDefns, InstDefns),
	find_undef_pred_modes(PredIds, Preds, ModeDefns, InstDefns).

:- pred find_undef_proc_modes(list(proc_id), pred_id, proc_table, mode_table,
				inst_table, io__state, io__state).
:- mode find_undef_proc_modes(in, in, in, in, in, di, uo).

find_undef_proc_modes([], _PredId, _Procs, _ModeDefns, _InstDefns) --> [].
find_undef_proc_modes([ProcId | ProcIds], PredId, Procs, ModeDefns,
		InstDefns) -->
	{ map__search(Procs, ProcId, ProcDefn) },
	{ proc_info_argmodes(ProcDefn, ArgModes) },
	{ proc_info_context(ProcDefn, Context) },
	find_undef_mode_list(ArgModes, pred(PredId) - Context, ModeDefns, 
		InstDefns),
	find_undef_proc_modes(ProcIds, PredId, Procs, ModeDefns, InstDefns).

%-----------------------------------------------------------------------------%

	% Find any undefined insts/modes used in the bodies of other mode
	% declarations.

:- pred find_undef_mode_bodies(list(mode_id), mode_table, inst_table,
				io__state, io__state).
:- mode find_undef_mode_bodies(in, in, in, di, uo).

find_undef_mode_bodies([], _, _) --> [].
find_undef_mode_bodies([ModeId | ModeIds], ModeDefns, InstDefns) -->
	{ map__search(ModeDefns, ModeId, HLDS_ModeDefn) },
		% XXX abstract hlds__mode_defn/5
	{ HLDS_ModeDefn = hlds__mode_defn(_, _, Mode, _, Context) },
	find_undef_mode_body(Mode, mode(ModeId) - Context, ModeDefns,
			InstDefns),
	find_undef_mode_bodies(ModeIds, ModeDefns, InstDefns).

	% Find any undefined insts/modes used in the given mode definition.

:- pred find_undef_mode_body(hlds__mode_body, mode_error_context,
				mode_table, inst_table, io__state, io__state).
:- mode find_undef_mode_body(in, in, in, in, di, uo).

find_undef_mode_body(eqv_mode(Mode), ErrorContext, ModeDefns, InstDefns) -->
	find_undef_mode(Mode, ErrorContext, ModeDefns, InstDefns).

	% Find any undefined modes in a list of modes.

:- pred find_undef_mode_list(list(mode), mode_error_context,
				mode_table, inst_table, io__state, io__state).
:- mode find_undef_mode_list(in, in, in, in, di, uo).

find_undef_mode_list([], _, _, _) --> [].
find_undef_mode_list([Mode|Modes], ErrorContext, ModeDefns, InstDefns) -->
	find_undef_mode(Mode, ErrorContext, ModeDefns, InstDefns),
	find_undef_mode_list(Modes, ErrorContext, ModeDefns, InstDefns).

	% Find any undefined modes/insts used in a mode.
	% The mode itself may be undefined, and also
	% any inst arguments may also be undefined.
	% (eg. the mode `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_mode(mode, mode_error_context, mode_table, inst_table,
				io__state, io__state).
:- mode find_undef_mode(in, in, in, in, di, uo).

find_undef_mode((InstA -> InstB), ErrorContext, _ModeDefns, InstDefns) -->
	find_undef_inst(InstA, ErrorContext, InstDefns),
	find_undef_inst(InstB, ErrorContext, InstDefns).
find_undef_mode(user_defined_mode(Name, Args), ErrorContext, ModeDefns,
		InstDefns) -->
		  %%% no builtin modes as yet
	{ length(Args, Arity) },
	{ ModeId = Name - Arity },
	(
		{ map__contains(ModeDefns, ModeId) }
	->
		[]
	;
		report_undef_mode(ModeId, ErrorContext)
	),
	find_undef_inst_list(Args, ErrorContext, InstDefns).

%-----------------------------------------------------------------------------%

	% Find any undefined insts used in the bodies of other inst
	% declarations.

:- pred find_undef_inst_bodies(list(inst_id), inst_table, io__state, io__state).
:- mode find_undef_inst_bodies(in, in, di, uo).

find_undef_inst_bodies([], _) --> [].
find_undef_inst_bodies([InstId | InstIds], InstDefns) -->
	{ map__search(InstDefns, InstId, HLDS_InstDefn) },
		% XXX abstract hlds__inst_defn/5
	{ HLDS_InstDefn = hlds__inst_defn(_, _, Inst, _, Context) },
	find_undef_inst_body(Inst, inst(InstId) - Context, InstDefns),
	find_undef_inst_bodies(InstIds, InstDefns).

	% Find any undefined insts used in the given inst definition.

:- pred find_undef_inst_body(hlds__inst_body, mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_inst_body(in, in, in, di, uo).

find_undef_inst_body(eqv_inst(Inst), ErrorContext, InstDefns) -->
	find_undef_inst(Inst, ErrorContext, InstDefns).
find_undef_inst_body(abstract_inst, _, _) --> [].

	% Find any undefined insts in a list of insts.

:- pred find_undef_inst_list(list(inst), mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_inst_list(in, in, in, di, uo).

find_undef_inst_list([], _ErrorContext, _InstDefns) --> [].
find_undef_inst_list([Inst|Insts], ErrorContext, InstDefns) -->
	find_undef_inst(Inst, ErrorContext, InstDefns),
	find_undef_inst_list(Insts, ErrorContext, InstDefns).

	% Find any undefined insts used in an inst.
	% The inst itself may be undefined, and also
	% any inst arguments may also be undefined.
	% (eg. the inst `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_inst(inst, mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_inst(in, in, in, di, uo).

find_undef_inst(free, _, _) --> [].
find_undef_inst(ground, _, _) --> [].
find_undef_inst(inst_var(_), _, _) --> [].
find_undef_inst(bound(BoundInsts), ErrorContext, InstDefns) -->
	find_undef_bound_insts(BoundInsts, ErrorContext, InstDefns).
find_undef_inst(user_defined_inst(Name, Args), ErrorContext, InstDefns) -->
	{ length(Args, Arity) },
	{ InstId = Name - Arity },
	(
		{ map__contains(InstDefns, InstId) }
	->
		[]
	;
		report_undef_inst(InstId, ErrorContext)
	),
	find_undef_inst_list(Args, ErrorContext, InstDefns).
find_undef_inst(abstract_inst(Name, Args), ErrorContext, InstDefns) -->
	find_undef_inst(user_defined_inst(Name, Args), ErrorContext, InstDefns).

:- pred find_undef_bound_insts(list(bound_inst), mode_error_context, inst_table,
				io__state, io__state).
:- mode find_undef_bound_insts(in, in, in, di, uo).

find_undef_bound_insts([], _, _) --> [].
find_undef_bound_insts([functor(_Name, Args) | BoundInsts], ErrorContext,
		InstDefns) -->
	find_undef_inst_list(Args, ErrorContext, InstDefns),
	find_undef_bound_insts(BoundInsts, ErrorContext, InstDefns).

%-----------------------------------------------------------------------------%

:- type mode_error_context == pair(mode_error_context_2, term__context).
:- type mode_error_context_2	--->	inst(inst_id)
				;	mode(mode_id)
				;	pred(pred_id).

	% Output an error message about an undefined mode
	% in the specified context.

:- pred report_undef_mode(mode_id, mode_error_context, io__state, io__state).
:- mode report_undef_mode(in, in, di, uo).
report_undef_mode(ModeId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In "),
	write_mode_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined mode "),
	write_mode_id(ModeId),
	io__write_string(".\n").

	% Output an error message about an undefined inst
	% in the specified context.

:- pred report_undef_inst(inst_id, mode_error_context, io__state, io__state).
:- mode report_undef_inst(in, in, di, uo).
report_undef_inst(InstId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In "),
	write_mode_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined inst "),
	write_inst_id(InstId),
	io__write_string(".\n").

	% Output a description of the context where an undefined mode was
	% used.

:- pred write_mode_error_context(mode_error_context_2, io__state, io__state).
:- mode write_mode_error_context(in, di, uo).

write_mode_error_context(pred(PredId)) -->
	io__write_string("mode declaration for predicate "),
	hlds_out__write_pred_id(PredId).
write_mode_error_context(mode(ModeId)) -->
	io__write_string("definition of mode "),
	write_mode_id(ModeId).
write_mode_error_context(inst(InstId)) -->
	io__write_string("definition of inst "),
	write_inst_id(InstId).

%-----------------------------------------------------------------------------%

	% Predicates to output inst_ids and mode_ids.
	% XXX inst_ids and mode_ids should include the module.

:- pred write_mode_id(mode_id, io__state, io__state).
:- mode write_mode_id(in, di, uo).

write_mode_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).

	% XXX inst_ids should include the module.

:- pred write_inst_id(inst_id, io__state, io__state).
:- mode write_inst_id(in, di, uo).

write_inst_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The mode_info data structure and access predicates.

	% XXX
:- type mode_context
	--->	call(	
			pred_id,	% pred name
			int		% argument number
		)
	;	unify(
			unify_context,	% original source of the unification
			side		% LHS or RHS
		)
	;	unify_arg(
			unify_context,
			side,
			cons_id,
			int
		)
	;	uninitialized.

:- type side ---> left ; right.

:- type call_context
	--->	unify(unify_context)
	;	call(pred_id).

:- type instmap
	--->	reachable(map(var, inst))
	;	unreachable.

:- type mode_info 
	--->	mode_info(
			io__state,
			module_info,
			pred_id,	% The pred we are checking
			proc_id,	% The mode which we are checking
			term__context,	% The line number of the subgoal we
					% are currently checking
			mode_context,	% A description of where in the
					% goal the error occurred
			instmap,	% The current instantiatedness
					% of the variables
			list(set(var)),	% The "locked" variables,
					% i.e. variables which cannot be
					% further instantiated inside a
					% negated context
			delay_info,	% info about delayed goals
			list(mode_error_info),
					% The mode errors found
			list(set(var))	% The live variables
		).

	% The normal inst of a mode_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

:- inst uniq_mode_info	=	bound_unique(
					mode_info(
						ground_unique, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).

:- mode mode_info_uo :: free -> uniq_mode_info.
:- mode mode_info_ui :: uniq_mode_info -> uniq_mode_info.
:- mode mode_info_di :: uniq_mode_info -> dead.

	% Some fiddly modes used when we want to extract
	% the io_state from a mode_info struct and then put it back again.

:- inst mode_info_no_io	=	bound_unique(
					mode_info(
						dead, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground
					)
				).

:- mode mode_info_get_io_state	:: uniq_mode_info -> mode_info_no_io.
:- mode mode_info_no_io		:: mode_info_no_io -> mode_info_no_io.
:- mode mode_info_set_io_state	:: mode_info_no_io -> dead.

%-----------------------------------------------------------------------------%

	% Initialize the mode_info

:- pred mode_info_init(io__state, module_info, pred_id, proc_id,
			term__context, set(var), instmap, mode_info).
:- mode mode_info_init(di, in, in, in, in, in, in, mode_info_uo) is det.

mode_info_init(IOState, ModuleInfo, PredId, ProcId, Context, LiveVars,
		InstMapping0, ModeInfo) :-
	mode_context_init(ModeContext),
	LockedVars = [],
	delay_info_init(DelayInfo),
	ErrorList = [],
	ModeInfo = mode_info(
		IOState, ModuleInfo, PredId, ProcId, Context, ModeContext,
		InstMapping0, LockedVars, DelayInfo, ErrorList, [LiveVars]
	).

%-----------------------------------------------------------------------------%

	% Lots of very boring access predicates.

:- pred mode_info_get_io_state(mode_info, io__state).
:- mode mode_info_get_io_state(mode_info_get_io_state, uo) is det.

mode_info_get_io_state(mode_info(IOState,_,_,_,_,_,_,_,_,_,_), IOState).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_io_state(mode_info, io__state, mode_info).
:- mode mode_info_set_io_state(mode_info_set_io_state, ui, mode_info_uo) is det.

mode_info_set_io_state( mode_info(_,B,C,D,E,F,G,H,I,J,K), IOState,
			mode_info(IOState,B,C,D,E,F,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_module_info(mode_info, module_info).
:- mode mode_info_get_module_info(in, out) is det.

mode_info_get_module_info(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_),
				ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_preds(mode_info, pred_table).
:- mode mode_info_get_preds(in, out) is det.

mode_info_get_preds(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_), Preds) :-
	module_info_preds(ModuleInfo, Preds).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_modes(mode_info, mode_table).
:- mode mode_info_get_modes(in, out) is det.

mode_info_get_modes(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_), Modes) :-
	module_info_modes(ModuleInfo, Modes).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_insts(mode_info, inst_table).
:- mode mode_info_get_insts(in, out) is det.

mode_info_get_insts(mode_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_), Insts) :-
	module_info_insts(ModuleInfo, Insts).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_predid(mode_info, pred_id).
:- mode mode_info_get_predid(in, out) is det.

mode_info_get_predid(mode_info(_,_,PredId,_,_,_,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_procid(mode_info, proc_id).
:- mode mode_info_get_procid(in, out) is det.

mode_info_get_procid(mode_info(_,_,_,ProcId,_,_,_,_,_,_,_), ProcId).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_context(mode_info, term__context).
:- mode mode_info_get_context(in, out).

mode_info_get_context(mode_info(_,_,_,_,Context,_,_,_,_,_,_), Context).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_context(term__context, mode_info, mode_info).
:- mode mode_info_set_context(in, mode_info_di, mode_info_uo) is det.

mode_info_set_context(Context, mode_info(A,B,C,D,_,F,G,H,I,J,K),
				mode_info(A,B,C,D,Context,F,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_mode_context(mode_info, mode_context).
:- mode mode_info_get_mode_context(in, out) is det.

mode_info_get_mode_context(mode_info(_,_,_,_,_,ModeContext,_,_,_,_,_),
				ModeContext).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_mode_context(mode_context, mode_info, mode_info).
:- mode mode_info_set_mode_context(in, mode_info_di, mode_info_uo) is det.

mode_info_set_mode_context(ModeContext, mode_info(A,B,C,D,E,_,G,H,I,J,K),
				mode_info(A,B,C,D,E,ModeContext,G,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_call_context(call_context, mode_info, mode_info).
:- mode mode_info_set_call_context(in, in, out) is det.

mode_info_set_call_context(unify(UnifyContext)) -->
	mode_info_set_mode_context(unify(UnifyContext, left)).
mode_info_set_call_context(call(PredId)) -->
	mode_info_set_mode_context(call(PredId, 0)).

:- pred mode_info_unset_call_context(mode_info, mode_info).
:- mode mode_info_unset_call_context(in, out) is det.

mode_info_unset_call_context -->
	mode_info_set_mode_context(uninitialized).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_instmap(mode_info, instmap).
:- mode mode_info_get_instmap(in, out) is det.

mode_info_get_instmap(mode_info(_,_,_,_,_,_,InstMap,_,_,_,_), InstMap).

	% mode_info_dcg_get_instmap/3 is the same as mode_info_get_instmap/2
	% except that it's easier to use inside a DCG.

:- pred mode_info_dcg_get_instmap(instmap, mode_info, mode_info).
:- mode mode_info_dcg_get_instmap(out, mode_info_di, mode_info_uo) is det.

mode_info_dcg_get_instmap(InstMap, ModeInfo, ModeInfo) :-
	mode_info_get_instmap(ModeInfo, InstMap).

	% mode_info_get_vars_instmap/3 is the same as mode_info_get_instmap/2
	% except that the map it returns might only contain the specified
	% variables if that would be more efficient; currently it's not,
	% so the two are just the same, but if we were to change the
	% data structures...

:- pred mode_info_get_vars_instmap(mode_info, set(var), instmap).
:- mode mode_info_get_vars_instmap(in, in, out) is det.

mode_info_get_vars_instmap(ModeInfo, _Vars, InstMap) :-
	mode_info_get_instmap(ModeInfo, InstMap).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_instmap(instmap, mode_info, mode_info).
:- mode mode_info_set_instmap(in, mode_info_di, mode_info_uo) is det.

mode_info_set_instmap( InstMap, mode_info(A,B,C,D,E,F,_,H,I,J,K),
			mode_info(A,B,C,D,E,F,InstMap,H,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_locked_vars(mode_info, list(set(var))).
:- mode mode_info_get_locked_vars(mode_info_ui, out) is det.

mode_info_get_locked_vars(mode_info(_,_,_,_,_,_,_,LockedVars,_,_,_),
		LockedVars).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_locked_vars(mode_info, list(set(var)), mode_info).
:- mode mode_info_set_locked_vars(mode_info_di, in, mode_info_uo) is det.

mode_info_set_locked_vars( mode_info(A,B,C,D,E,F,G,_,I,J,K), LockedVars,
			mode_info(A,B,C,D,E,F,G,LockedVars,I,J,K)).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_errors(mode_info, list(mode_error_info)).
:- mode mode_info_get_errors(mode_info_ui, out) is det.

mode_info_get_errors(mode_info(_,_,_,_,_,_,_,_,_,Errors,_), Errors).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_num_errors(mode_info, int).
:- mode mode_info_get_num_errors(mode_info_ui, out) is det.

mode_info_get_num_errors(mode_info(_,_,_,_,_,_,_,_,_,Errors,_), NumErrors) :-
	length(Errors, NumErrors).

%-----------------------------------------------------------------------------%

:- pred mode_info_set_errors(list(mode_error_info), mode_info, mode_info).
:- mode mode_info_set_errors(in, mode_info_di, mode_info_uo) is det.

mode_info_set_errors( Errors, mode_info(A,B,C,D,E,F,G,H,I,_,K), 
			mode_info(A,B,C,D,E,F,G,H,I,Errors,K)).

%-----------------------------------------------------------------------------%

	% We keep track of the live variables as a bag, represented
	% as a list of sets of vars.
	% This allows us to easily add and remove sets of variables.
	% It's probably not maximally efficient.

	% Add a set of vars to the bag of live vars.

:- pred mode_info_add_live_vars(set(var), mode_info, mode_info).
:- mode mode_info_add_live_vars(in, mode_info_di, mode_info_uo).

mode_info_add_live_vars(NewLiveVars,
			mode_info(A,B,C,D,E,F,G,H,I,J,LiveVars0),
			mode_info(A,B,C,D,E,F,G,H,I,J,LiveVars)) :-
	LiveVars = [NewLiveVars | LiveVars0].

	% Remove a set of vars from the bag of live vars.

:- pred mode_info_remove_live_vars(set(var), mode_info, mode_info).
:- mode mode_info_remove_live_vars(in, mode_info_di, mode_info_uo) is det.

mode_info_remove_live_vars(OldLiveVars, ModeInfo0, ModeInfo) :-
	ModeInfo0 = mode_info(A,B,C,D,E,F,G,H,I,J,LiveVars0),
	ModeInfo1 = mode_info(A,B,C,D,E,F,G,H,I,J,LiveVars),
	( delete_first(LiveVars0, OldLiveVars, LiveVars) ->
		true
	;
		error("mode_info_remove_live_vars: delete_first failed")
	),
		% when a variable becomes dead, we may be able to wake
		% up a goal which is waiting on that variable
	set__to_sorted_list(OldLiveVars, VarList),
	mode_info_get_delay_info(ModeInfo1, DelayInfo0),
	delay_info_bind_var_list(VarList, DelayInfo0, DelayInfo),
	mode_info_set_delay_info(DelayInfo, ModeInfo1, ModeInfo).

:- pred delay_info_bind_var_list(list(var), delay_info, delay_info).
:- mode delay_info_bind_var_list(in, in, out).

delay_info_bind_var_list([], DelayInfo, DelayInfo).
delay_info_bind_var_list([Var|Vars], DelayInfo0, DelayInfo) :-
	delay_info_bind_var(DelayInfo0, Var, DelayInfo1),
	delay_info_bind_var_list(Vars, DelayInfo1, DelayInfo).

	% Check whether a list of variables are live or not

:- pred mode_info_var_list_is_live(list(var), mode_info, list(is_live)).
:- mode mode_info_var_list_is_live(in, mode_info_ui, out) is det.

mode_info_var_list_is_live([], _, []).
mode_info_var_list_is_live([Var | Vars], ModeInfo, [Live | Lives]) :-
	mode_info_var_is_live(ModeInfo, Var, Live),
	mode_info_var_list_is_live(Vars, ModeInfo, Lives).

	% Check whether a variable is live or not

:- pred mode_info_var_is_live(mode_info, var, is_live).
:- mode mode_info_var_is_live(mode_info_ui, in, out) is det.

mode_info_var_is_live(mode_info(_,_,_,_,_,_,_,_,_,_,LiveVarsList), Var,
		Result) :-
	(
		% some [LiveVars] 
		member(LiveVars, LiveVarsList),
		set__member(Var, LiveVars)
	->
		Result = live
	;
		Result = dead
	).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_varset(mode_info, varset).
:- mode mode_info_get_varset(mode_info_ui, out) is det.

	% we don't bother to store the varset directly in the mode_info,
	% since we only need it to report errors, and we can afford
	% to waste a little bit of time when reporting errors.

mode_info_get_varset(ModeInfo, VarSet) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_predid(ModeInfo, PredId),
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	mode_info_get_procid(ModeInfo, ProcId),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_variables(ProcInfo, VarSet).

:- pred mode_info_get_instvarset(mode_info, varset).
:- mode mode_info_get_instvarset(mode_info_ui, out) is det.

	% Since we don't yet handle polymorphic modes, the inst varset
	% is always empty.

mode_info_get_instvarset(_ModeInfo, InstVarSet) :-
	varset__init(InstVarSet).

:- pred mode_info_get_var_types(mode_info, map(var,type)).
:- mode mode_info_get_var_types(mode_info_ui, out) is det.

	% We don't bother to store the var types directly in the mode_info.
	% Instead we look them up every time we need them.
	% This is probably inefficient!

mode_info_get_var_types(ModeInfo, VarTypes) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_predid(ModeInfo, PredId),
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	mode_info_get_procid(ModeInfo, ProcId),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_vartypes(ProcInfo, VarTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The locked variables are stored as a stack 
	% of sets of variables.  A variable is locked if it is
	% a member of any of the sets.  To lock a set of vars, we just
	% push them on the stack, and to unlock a set of vars, we just
	% pop them off the stack.  The stack is implemented as a list.

:- pred mode_info_lock_vars(set(var), mode_info, mode_info).
:- mode mode_info_lock_vars(in, mode_info_di, mode_info_uo) is det.

mode_info_lock_vars(Vars, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, LockedVars),
	mode_info_set_locked_vars(ModeInfo0, [Vars | LockedVars], ModeInfo).

:- pred mode_info_unlock_vars(set(var), mode_info, mode_info).
:- mode mode_info_unlock_vars(in, mode_info_di, mode_info_uo) is det.

mode_info_unlock_vars(_, ModeInfo0, ModeInfo) :-
	mode_info_get_locked_vars(ModeInfo0, [_ | LockedVars]),
	mode_info_set_locked_vars(ModeInfo0, LockedVars, ModeInfo).

:- pred mode_info_var_is_locked(mode_info, var).
:- mode mode_info_var_is_locked(mode_info_ui, in) is semidet.

mode_info_var_is_locked(ModeInfo, Var) :-
	mode_info_get_locked_vars(ModeInfo, LockedVarsList),
	mode_info_var_is_locked_2(LockedVarsList, Var).

:- pred mode_info_var_is_locked_2(list(set(var)), var).
:- mode mode_info_var_is_locked_2(in, in) is semidet.

mode_info_var_is_locked_2([Set | Sets], Var) :-
	(
		set__member(Var, Set)
	->
		true
	;
		mode_info_var_is_locked_2(Sets, Var)
	).

:- pred mode_info_get_delay_info(mode_info, delay_info).
:- mode mode_info_get_delay_info(mode_info_no_io, out) is det.

mode_info_get_delay_info(mode_info(_,_,_,_,_,_,_,_,DelayInfo,_,_), DelayInfo).

:- pred mode_info_set_delay_info(delay_info, mode_info, mode_info).
:- mode mode_info_set_delay_info(in, mode_info_di, mode_info_uo) is det.

mode_info_set_delay_info(DelayInfo, mode_info(A,B,C,D,E,F,G,H,_,J,K),
			mode_info(A,B,C,D,E,F,G,H,DelayInfo,J,K)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Reordering of conjunctions is done
	% by simulating coroutining at compile time.
	% This is handled by the following data structure.

:- type delay_info
	--->	delay_info(
			depth_num,	% CurrentDepth:
					% the current conjunction depth,
					% i.e. the number of nested conjunctions
					% which are currently active
			stack(map(seq_num, delayed_goal)),
					% DelayedGoalStack:
					% for each nested conjunction,
					% we store a collection of delayed goals
					% associated with that conjunction,
					% indexed by sequence number
			waiting_goals_table,
					% WaitingGoalsTable:
					% for each variable, we keep track of
					% all the goals which are waiting on
					% that variable
			pending_goals_table,
					% PendingGoalsTable:
					% when a variable gets bound, we
					% mark all the goals which are waiting
					% on that variable as ready to be
					% reawakened at the next opportunity
			stack(seq_num)
					% SeqNumsStack:
					% For each nested conjunction, the
					% next available sequence number.
		).

:- type delayed_goal
	--->	delayed_goal(
			list(var),	% The vars it's waiting on
			mode_error_info,% The reason it can't be scheduled
			hlds__goal	% The goal itself
		).

:- type waiting_goals_table == map(var, waiting_goals).
	% Used to store the collection of goals waiting on a variable.
:- type waiting_goals == map(goal_num, list(var)).
	% For each goal, we store all the variables that it is waiting on.

:- type pending_goals_table == map(depth_num, list(seq_num)).
	
:- type goal_num == pair(depth_num, seq_num).
:- type depth_num == int.
:- type seq_num == int.

%-----------------------------------------------------------------------------%

	% Initialize the delay info structure in preparation for
	% mode analysis of a goal.

:- pred delay_info_init(delay_info).
:- mode delay_info_init(out) is det.

delay_info_init(DelayInfo) :-
	CurrentDepth = 0,
	stack__init(DelayedGoalStack),
	map__init(WaitingGoalsTable),
	map__init(PendingGoals),
	stack__init(NextSeqNums),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums).

%-----------------------------------------------------------------------------%

:- pred delay_info_enter_conj(delay_info, delay_info).
:- mode delay_info_enter_conj(in, out) is det.

delay_info_enter_conj(DelayInfo0, DelayInfo) :-
	DelayInfo0 = delay_info(CurrentDepth0, DelayedGoalStack0,
				WaitingGoalsTable, PendingGoals, NextSeqNums0),
	map__init(DelayedGoals),
	stack__push(DelayedGoalStack0, DelayedGoals, DelayedGoalStack),
	stack__push(NextSeqNums0, 0, NextSeqNums),
	CurrentDepth is CurrentDepth0 + 1,
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums).

%-----------------------------------------------------------------------------%

:- pred delay_info_leave_conj(delay_info, list(delayed_goal), delay_info).
:- mode delay_info_leave_conj(in, out, out) is det.

delay_info_leave_conj(DelayInfo0, DelayedGoalsList, DelayInfo) :-
	DelayInfo0 = delay_info(CurrentDepth0, DelayedGoalStack0,
				WaitingGoalsTable, PendingGoals, NextSeqNums0),
	stack__pop(DelayedGoalStack0, DelayedGoals, DelayedGoalStack),
	stack__pop(NextSeqNums0, _, NextSeqNums),
	CurrentDepth is CurrentDepth0 - 1,
	map__values(DelayedGoals, DelayedGoalsList),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums).

%-----------------------------------------------------------------------------%

:- pred delay_info_delay_goal(delay_info, mode_error_info,
				hlds__goal, delay_info).
:- mode delay_info_delay_goal(in, in, in, out) is det.

delay_info_delay_goal(DelayInfo0, Error, Goal, DelayInfo) :-
	Error = mode_error_info(Vars, _, _, _),
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack0,
				WaitingGoalsTable0, PendingGoals, NextSeqNums0),

		% Get the next sequence number
	stack__pop(NextSeqNums0, SeqNum, NextSeqNums1),
	NextSeq is SeqNum + 1,
	stack__push(NextSeqNums1, NextSeq, NextSeqNums),

		% Store the goal in the delayed goal stack
	stack__pop(DelayedGoalStack0, DelayedGoals0, DelayedGoalStack1),
	map__set(DelayedGoals0, SeqNum, delayed_goal(Vars, Error, Goal),
			DelayedGoals),
	stack__push(DelayedGoalStack1, DelayedGoals, DelayedGoalStack),

		% Store indexes to the goal in the waiting goals table
	GoalNum = CurrentDepth - SeqNum,
	add_waiting_vars(Vars, GoalNum, Vars, WaitingGoalsTable0,
				WaitingGoalsTable),
	
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums).

:- pred add_waiting_vars(list(var), goal_num, list(var), waiting_goals_table,
				waiting_goals_table).
:- mode add_waiting_vars(in, in, in, in, out).

add_waiting_vars([], _, _, WaitingGoalsTable, WaitingGoalsTable).
add_waiting_vars([Var | Vars], Goal, AllVars, WaitingGoalsTable0,
			WaitingGoalsTable) :-
	(
		map__search(WaitingGoalsTable0, Var, WaitingGoals0)
	->
		WaitingGoals1 = WaitingGoals0
	;
		map__init(WaitingGoals1)
	),
	map__set(WaitingGoals1, Goal, AllVars, WaitingGoals),
	map__set(WaitingGoalsTable0, Var, WaitingGoals, WaitingGoalsTable1),
	add_waiting_vars(Vars, Goal, AllVars, WaitingGoalsTable1,
		WaitingGoalsTable).

%-----------------------------------------------------------------------------%

	% Whenever we bind a variable, we also check to see whether
	% we need to wake up some goals.  If so, we remove those
	% goals from the waiting goals table and add them to the pending
	% goals table.  They will be woken up next time we get back
	% to their conjunction.

:- pred delay_info_bind_var(delay_info, var, delay_info).
:- mode delay_info_bind_var(in, in, out) is det.

delay_info_bind_var(DelayInfo0, Var, DelayInfo) :-
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable0, PendingGoals0, NextSeqNums),
	(
		map__search(WaitingGoalsTable0, Var, GoalsWaitingOnVar)
	->
		map__keys(GoalsWaitingOnVar, Keys),
		add_pending_goals(Keys, Var, GoalsWaitingOnVar,
				PendingGoals0, PendingGoals,
				WaitingGoalsTable0, WaitingGoalsTable1),
		map__delete(WaitingGoalsTable1, Var, WaitingGoalsTable),
		DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums)
	;
		DelayInfo = DelayInfo0
	).

	% Add a collection of goals, identified by depth_num and seq_num
	% (depth of nested conjunction and sequence number within conjunction),
	% to the collection of pending goals.
	
:- pred add_pending_goals(list(goal_num), var, map(goal_num, list(var)),
			pending_goals_table, pending_goals_table,
			waiting_goals_table, waiting_goals_table).
:- mode add_pending_goals(in, in, in, in, out, in, out) is det.

add_pending_goals([], _Var, _WaitingVarsTable,
			PendingGoals, PendingGoals,
			WaitingGoals, WaitingGoals).
add_pending_goals([Depth - SeqNum | Rest], Var, WaitingVarsTable,
			PendingGoals0, PendingGoals,
			WaitingGoals0, WaitingGoals) :-

		% remove any other indexes to the goal from the waiting
		% goals table
	GoalNum = Depth - SeqNum,
	map__lookup(WaitingVarsTable, GoalNum, WaitingVars),
	delete_waiting_vars(WaitingVars, Var, GoalNum, WaitingGoals0,
			WaitingGoals1),

		% add the goal to the pending goals table
	( map__search(PendingGoals0, Depth, PendingSeqNums0) ->
		% XXX should use a queue
		append(PendingSeqNums0, [SeqNum], PendingSeqNums)
	;
		PendingSeqNums = [SeqNum]
	),
	map__set(PendingGoals0, Depth, PendingSeqNums, PendingGoals1),

		% do the same for the rest of the pending goals
	add_pending_goals(Rest, Var, WaitingVarsTable,
		PendingGoals1, PendingGoals,
		WaitingGoals1, WaitingGoals).

	% Since we're about to move this goal from the waiting goal table
	% to the pending table, we need to delete all the other indexes to it
	% in the waiting goal table, so that we don't attempt to wake
	% it up twice.

:- pred delete_waiting_vars(list(var), var, goal_num,
				waiting_goals_table, waiting_goals_table).
:- mode delete_waiting_vars(in, in, in, in, out) is det.

delete_waiting_vars([], _, _, WaitingGoalTables, WaitingGoalTables).
delete_waiting_vars([Var | Vars], ThisVar, GoalNum, WaitingGoalsTable0,
				WaitingGoalsTable) :-
	( Var = ThisVar ->
		WaitingGoalsTable1 = WaitingGoalsTable0
	;
		map__lookup(WaitingGoalsTable0, Var, WaitingGoals0),
		map__delete(WaitingGoals0, GoalNum, WaitingGoals),
		map__set(WaitingGoalsTable0, Var, WaitingGoals,
			WaitingGoalsTable1)
	),
	delete_waiting_vars(Vars, ThisVar, GoalNum, WaitingGoalsTable1,
				WaitingGoalsTable).

%-----------------------------------------------------------------------------%

	% mode_info_wakeup_goal(DelayInfo0, Goal, DelayInfo) is true iff
	% DelayInfo0 specifies that there is at least one goal which is
	% pending, Goal is the pending goal which should be reawakened first,
	% and DelayInfo is the new delay_info, updated to reflect the fact
	% that Goal has been woken up and is hence no longer pending.

:- pred delay_info_wakeup_goal(delay_info, hlds__goal, delay_info).
:- mode delay_info_wakeup_goal(in, out, out) is semidet.

delay_info_wakeup_goal(DelayInfo0, Goal, DelayInfo) :-
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack0, WaitingGoals,
				PendingGoalsTable0, NextSeqNums),

		% is there a goal in the current conjunction which is pending?
	map__search(PendingGoalsTable0, CurrentDepth, PendingGoals0),

		% if so, remove it from the pending goals table,
		% remove it from the delayed goals stack, and return it
	PendingGoals0 = [SeqNum | PendingGoals],
	map__set(PendingGoalsTable0, CurrentDepth, PendingGoals,
			PendingGoalsTable),
	stack__pop(DelayedGoalStack0, DelayedGoals0, DelayedGoalStack1),
	map__lookup(DelayedGoals0, SeqNum, DelayedGoal),
	DelayedGoal = delayed_goal(_Vars, _ErrorReason, Goal),
	map__delete(DelayedGoals0, SeqNum, DelayedGoals),
	stack__push(DelayedGoalStack1, DelayedGoals, DelayedGoalStack),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack, WaitingGoals,
				PendingGoalsTable, NextSeqNums).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type mode_error_info
	---> mode_error_info(
		list(var),	% the variables which caused the error
				% (we will attempt to reschedule the goal
				% if the one of these variables becomes
				% more instantiated)
		mode_error,	% the nature of the error
		term__context,	% where the error occurred
		mode_context	% where the error occurred
	).

%-----------------------------------------------------------------------------%

	% record a mode error (and associated context _info) in the mode_info.

:- pred mode_info_error(list(var), mode_error, mode_info, mode_info).
:- mode mode_info_error(in, in, mode_info_di, mode_info_uo).

mode_info_error(Vars, ModeError, ModeInfo0, ModeInfo) :-
	mode_info_get_context(ModeInfo0, Context),
	mode_info_get_mode_context(ModeInfo0, ModeContext),
	ModeErrorInfo = mode_error_info(Vars, ModeError, Context, ModeContext),
	mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo).

:- pred mode_info_add_error(mode_error_info, mode_info, mode_info).
:- mode mode_info_add_error(in, mode_info_di, mode_info_uo).

mode_info_add_error(ModeErrorInfo, ModeInfo0, ModeInfo) :-
	mode_info_get_errors(ModeInfo0, Errors0),
	append(Errors0, [ModeErrorInfo], Errors),
	mode_info_set_errors(Errors, ModeInfo0, ModeInfo).

%-----------------------------------------------------------------------------%

	% If there were any errors recorded in the mode_info,
	% report them to the user now.

:- pred modecheck_report_errors(mode_info, mode_info).
:- mode modecheck_report_errors(mode_info_di, mode_info_uo).

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

:- type mode_error
	--->	mode_error_disj(merge_context, merge_errors)
			% different arms of a disjunction result in
			% different insts for some non-local variables
	;	mode_error_var_has_inst(var, inst, inst)
			% call to a predicate with an insufficiently
			% instantiated variable (for preds with one mode)
	;	mode_error_no_matching_mode(list(var), list(inst))
			% call to a predicate with an insufficiently
			% instantiated variable (for preds with >1 mode)
	;	mode_error_bind_var(var, inst, inst)
			% attempt to bind a non-local variable inside
			% a negated context
	;	mode_error_unify_var_var(var, var, inst, inst)
			% attempt to unify two free variables
	;	mode_error_unify_var_functor(var, const, list(term),
							inst, list(inst))
			% attempt to unify a free var with a functor containing
			% free arguments
	;	mode_error_conj(list(delayed_goal))
			% a conjunction contains one or more unscheduleable
			% goals
	;	mode_error_final_inst(int, var, inst, inst).
			% one of the head variables did not have the
			% expected final inst on exit from the proc

%-----------------------------------------------------------------------------%

	% print an error message describing a mode error:
	% just dispatch on the diffferent sorts of mode errors

:- pred report_mode_error(mode_error, mode_info, io__state, io__state).
:- mode report_mode_error(in, mode_info_no_io, di, uo).

report_mode_error(mode_error_disj(MergeContext, ErrorList), ModeInfo) -->
	report_mode_error_disj(ModeInfo, MergeContext, ErrorList).
report_mode_error(mode_error_var_has_inst(Var, InstA, InstB), ModeInfo) -->
	report_mode_error_var_has_inst(ModeInfo, Var, InstA, InstB).
report_mode_error(mode_error_bind_var(Var, InstA, InstB), ModeInfo) -->
	report_mode_error_bind_var(ModeInfo, Var, InstA, InstB).
report_mode_error(mode_error_unify_var_var(VarA, VarB, InstA, InstB),
		ModeInfo) -->
	report_mode_error_unify_var_var(ModeInfo, VarA, VarB, InstA, InstB).
report_mode_error(mode_error_unify_var_functor(Var, Name, Args, Inst,
			ArgInsts), ModeInfo) -->
	report_mode_error_unify_var_functor(ModeInfo, Var, Name, Args, Inst,
			ArgInsts).
report_mode_error(mode_error_conj(Errors), ModeInfo) -->
	report_mode_error_conj(ModeInfo, Errors).
report_mode_error(mode_error_no_matching_mode(Vars, Insts), ModeInfo) -->
	report_mode_error_no_matching_mode(ModeInfo, Vars, Insts).
report_mode_error(mode_error_final_inst(ArgNum, Var, VarInst, Inst), ModeInfo)
		-->
	report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_conj(mode_info, list(delayed_goal),
				io__state, io__state).
:- mode report_mode_error_conj(mode_info_no_io, in, di, uo).

report_mode_error_conj(ModeInfo, Errors) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in conjunction. The next "),
	{ length(Errors, NumErrors) },
	io__write_int(NumErrors),
	io__write_string(" error messages\n"),
	prog_out__write_context(Context),
	io__write_string("  indicate possible causes of this error.\n"),
	report_mode_error_conj_2(Errors, VarSet, Context, ModeInfo).

:- pred report_mode_error_conj_2(list(delayed_goal), varset, term__context,
				mode_info, io__state, io__state).
:- mode report_mode_error_conj_2(in, in, in, mode_info_no_io, di, uo).

report_mode_error_conj_2([], _, _, _) --> [].
report_mode_error_conj_2([delayed_goal(Vars, Error, Goal) | Rest],
			VarSet, Context, ModeInfo) -->
	prog_out__write_context(Context),
	io__write_string("Floundered goal, waiting on { "),
	mercury_output_vars(Vars, VarSet),
	io__write_string(" } :\n"),
	lookup_option(verbose_errors, bool(VerboseErrors)),
	( { VerboseErrors = yes } ->
		io__write_string("\t\t"),
		mercury_output_hlds_goal(Goal, VarSet, 2),
		io__write_string(".\n")
	;
		[]
	),
	{ Error = mode_error_info(_, ModeError, Context, ModeContext) },
	{ mode_info_set_context(Context, ModeInfo, ModeInfo1) },
	{ mode_info_set_mode_context(ModeContext, ModeInfo1, ModeInfo2) },
	report_mode_error(ModeError, ModeInfo2),
	report_mode_error_conj_2(Rest, VarSet, Context, ModeInfo).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_disj(mode_info, merge_context, merge_errors,
				io__state, io__state).
:- mode report_mode_error_disj(mode_info_no_io, in, in, di, uo).

report_mode_error_disj(ModeInfo, MergeContext, ErrorList) -->
	{ mode_info_get_context(ModeInfo, Context) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode mismatch in "),
	write_merge_context(MergeContext),
	io__write_string(".\n"),
	write_merge_error_list(ErrorList, ModeInfo).

:- pred write_merge_error_list(merge_errors, mode_info, io__state, io__state).
:- mode write_merge_error_list(in, mode_info_no_io, di, uo).

write_merge_error_list([], _) --> [].
write_merge_error_list([Var - Insts | Errors], ModeInfo) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(Var, VarSet),
	io__write_string("' :: "),
	mercury_output_inst_list(Insts, InstVarSet),
	io__write_string(".\n"),
	write_merge_error_list(Errors, ModeInfo).

:- pred write_merge_context(merge_context, io__state, io__state).
:- mode write_merge_context(in, di, uo).

write_merge_context(disj) -->
	io__write_string("disjunction").
write_merge_context(if_then_else) -->
	io__write_string("if-then-else").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_bind_var(mode_info, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_bind_var(in, in, in, in, di, uo).

report_mode_error_bind_var(ModeInfo, Var, VarInst, Inst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string(
		"  mode error: attempt to bind variable inside a negation.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(Var, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	mercury_output_inst(Inst, InstVarSet),
	io__write_string("'.\n"),
	lookup_option(verbose_errors, bool(VerboseErrors)),
	( { VerboseErrors = yes } ->
		io__write_string("\tA negation is only allowed to bind variables which are local to the\n"),
		io__write_string("\tnegation, i.e. those which are implicitly existentially quantified\n"),
		io__write_string("\tinside the scope of the negation.\n"),
		io__write_string("\tNote that the condition of an if-then-else is implicitly\n"),
		io__write_string("\tnegated in the \"else\" part, so the condition can only bind\n"),
		io__write_string("\tvariables in the \"then\" part.\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_no_matching_mode(mode_info, list(var), list(inst),
					io__state, io__state).
:- mode report_mode_error_no_matching_mode(in, in, in, di, uo) is det.

report_mode_error_no_matching_mode(ModeInfo, Vars, Insts) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: arguments `"),
	mercury_output_vars(Vars, VarSet),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("have insts `"),
	mercury_output_inst_list(Insts, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("which does not match any of the modes for `"),
	{ mode_info_get_mode_context(ModeInfo, call(PredId, _)) },
	hlds_out__write_pred_id(PredId),
	io__write_string("'.\n").

:- pred report_mode_error_var_has_inst(mode_info, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_var_has_inst(in, in, in, in, di, uo) is det.

report_mode_error_var_has_inst(ModeInfo, Var, VarInst, Inst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	mercury_output_inst(Inst, InstVarSet),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_var(mode_info, var, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_unify_var_var(in, in, in, in, in, di, uo) is det.

report_mode_error_unify_var_var(ModeInfo, X, Y, InstX, InstY) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet),
	io__write_string("' and `"),
	mercury_output_var(Y, VarSet),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  variable `"),
	mercury_output_var(Y, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(InstY, InstVarSet),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_functor(mode_info, var, const, list(term),
					inst, list(inst), io__state, io__state).
:- mode report_mode_error_unify_var_functor(in, in, in, in, in, in, di, uo)
	is det.

report_mode_error_unify_var_functor(ModeInfo, X, Name, Args, InstX, ArgInsts)
		-->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	{ Term = term__functor(Name, Args, Context) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet),
	io__write_string("' and `"),
	io__write_term(VarSet, Term),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  term `"),
	io__write_term(VarSet, Term),
	( { Args \= [] } ->
		io__write_string("'\n"),
		prog_out__write_context(Context),
		io__write_string("  has instantiatedness `"),
		io__write_constant(Name),
		io__write_string("("),
		mercury_output_inst_list(ArgInsts, InstVarSet),
		io__write_string(")")
	;
		io__write_string("' has instantiatedness `"),
		io__write_constant(Name)
	),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred mode_info_write_context(mode_info, io__state, io__state).
:- mode mode_info_write_context(mode_info_no_io, di, uo).

mode_info_write_context(ModeInfo) -->
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_predid(ModeInfo, PredId) },
	{ mode_info_get_procid(ModeInfo, ProcId) },
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ predicate_name(PredId, PredName) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },

	prog_out__write_context(Context),
	io__write_string("In clause for `"),
	io__write_string(PredName),
	( { ArgModes \= [] } ->
		io__write_string("("),
		mercury_output_mode_list(ArgModes, InstVarSet),
		io__write_string(")")
	;
		[]
	),
	io__write_string("':\n"),
	{ mode_info_get_mode_context(ModeInfo, ModeContext) },
	write_mode_context(ModeContext, Context).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_final_inst(mode_info, int, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_final_inst(in, in, in, in, in, di, uo) is det.

report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst) -->
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: argument "),
	io__write_int(ArgNum),
	( { inst_matches_initial(VarInst, Inst, ModuleInfo) } ->
		io__write_string(" became too instantiated")
	; { inst_matches_initial(Inst, VarInst, ModuleInfo) } ->
		io__write_string(" did not get sufficiently instantiated")
	;
		% I don't think this can happen.  But just in case...
		io__write_string(" had the wrong instantiatedness")
	),
	io__write_string(".\n"),

	prog_out__write_context(Context),
	io__write_string("  Final instantiatedness of `"),
	mercury_output_var(Var, VarSet),
	io__write_string("' was `"),
	mercury_output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),

	prog_out__write_context(Context),
	io__write_string("  expected final instantiatedness was `"),
	mercury_output_inst(Inst, InstVarSet),
	io__write_string("'.\n").


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mode_context_init(mode_context).
:- mode mode_context_init(in) is det.

mode_context_init(uninitialized).

%-----------------------------------------------------------------------------%

	% XXX some parts of the mode context never get set up

:- pred write_mode_context(mode_context, term__context, io__state, io__state).
:- mode write_mode_context(in, in, di, uo).

write_mode_context(uninitialized, _Context) -->
	[].

write_mode_context(call(PredId, _ArgNum), Context) -->
	prog_out__write_context(Context),
	io__write_string("  in call to predicate `"),
	hlds_out__write_pred_id(PredId),
	io__write_string("':\n").

write_mode_context(unify(UnifyContext, _Side), Context) -->
	hlds_out__write_unify_context(UnifyContext, Context).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
