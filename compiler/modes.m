%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: modes.nl.
% Main author: fjh.
%
% This file contains a mode-checker.
% Still very incomplete.

% XXX we need to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is 
%     dead.

% XXX break unifications into "micro-unifications"

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
:- mode modecheck(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module undef_modes, mode_info, delay_info, mode_errors.
:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module type_util, mode_util, prog_io.
:- import_module globals, options, mercury_to_mercury, hlds_out, int, set.

%-----------------------------------------------------------------------------%

modecheck(Module0, Module) -->
	globals__lookup_option(statistics, bool(Statistics)),
	globals__lookup_option(verbose, bool(Verbose)),
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
:- mode check_pred_modes(in, out, di, uo) is det.

check_pred_modes(Module0, Module) -->
	{ module_info_predids(Module0, PredIds) },
	modecheck_pred_modes_2(PredIds, Module0, Module).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_pred_modes_2(list(pred_id), module_info, 
			module_info, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, out, di, uo) is det.

modecheck_pred_modes_2([], ModuleInfo, ModuleInfo) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	{ pred_info_clauses_info(PredInfo0, ClausesInfo0) },
	{ ClausesInfo0 = clauses_info(_, _, _, Clauses0) },
	( { Clauses0 = [] } ->
		{ ModuleInfo3 = ModuleInfo0 }
	;
		globals__lookup_option(very_verbose, bool(VeryVerbose)),
		( { VeryVerbose = yes } ->
			io__write_string("% Mode-checking predicate "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
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
:- mode copy_clauses_to_procs(in, out) is det.

copy_clauses_to_procs(PredInfo0, PredInfo) :-
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	pred_info_procedures(PredInfo0, Procs0),
	map__keys(Procs0, ProcIds),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id)::in, clauses_info::in,
				proc_table::in, proc_table::out) is det.

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
	proc_info_set_body(Proc0, VarSet, VarTypes, HeadVars, Goal, Proc),
	map__set(Procs0, ProcId, Proc, Procs1),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs1, Procs).

:- pred select_matching_clauses(list(clause), proc_id, list(clause)).
:- mode select_matching_clauses(in, in, out) is det.

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
	Clause = clause(ProcIds, _, _),
	( list__member(ProcId, ProcIds) ->
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
:- mode modecheck_procs(in, in, in, out, out, di, uo) is det.

modecheck_procs(PredId, ModuleInfo, PredInfo0, PredInfo, NumErrors) -->
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ map__keys(Procs0, ProcIds) },
	modecheck_procs_2(ProcIds, PredId, ModuleInfo, Procs0, 0,
				Procs, NumErrors),
	{ pred_info_set_procedures(PredInfo0, Procs, PredInfo) }.

	% Iterate over the list of modes for a predicate.

:- pred modecheck_procs_2(list(proc_id), pred_id, module_info,
		proc_table, int, proc_table, int, io__state, io__state).
:- mode modecheck_procs_2(in, in, in, in, in, out, out, di, uo) is det.

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
:- mode modecheck_proc(in, in, in, in, out, out, di, uo) is det.

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
	proc_info_set_liveness_info(ProcInfo0, LiveVars, ProcInfo1),
	mode_info_init(IOState0, ModuleInfo, PredId, ProcId, Context, LiveVars,
			InstMap0, ModeInfo0),
	modecheck_goal(Body0, Body, ModeInfo0, ModeInfo1),
	modecheck_final_insts(HeadVars, ArgModes, ModeInfo1, ModeInfo2),
	modecheck_report_errors(ModeInfo2, ModeInfo),
	mode_info_get_num_errors(ModeInfo, NumErrors),
	mode_info_get_io_state(ModeInfo, IOState),
	proc_info_set_goal(ProcInfo1, Body, ProcInfo).

:- pred modecheck_final_insts(list(var), list(mode), mode_info, mode_info).
:- mode modecheck_final_insts(in, in, in, out) is det.

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
		% instantiation of the non-local vars and the changes
		% in liveness in the goal's goal_info.
		%
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	mode_info_get_vars_instmap(ModeInfo0, NonLocals, InstMap0),
%	mode_info_get_liveness(ModeInfo0, Liveness0),
	modecheck_goal_2(Goal0, NonLocals, Goal, ModeInfo0, ModeInfo),
		%
		% save the changes in instantiation of the non-local vars
		%
	mode_info_get_vars_instmap(ModeInfo, NonLocals, InstMap),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).
		%
		% save the changes in liveness
		%
%	mode_info_get_liveness(ModeInfo, Liveness),
%	compute_liveness_delta(Liveness0, Liveness, DeltaLiveness),
%	goal_info_set_delta_liveness(GoalInfo1, DeltaLiveness, GoalInfo).

:- pred compute_liveness_delta(set(var), set(var), delta_liveness).
:- mode compute_liveness_delta(in, in, out) is det.

compute_liveness_delta(Liveness0, Liveness, Births - Deaths) :-
	set__difference(Liveness0, Liveness, Deaths),
	set__difference(Liveness, Liveness0, Births).

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

modecheck_goal_2(call(PredId, _, Args, Builtin, PredName), _,
		 call(PredId, Mode, Args, Builtin, PredName)) -->
	mode_checkpoint(enter, "call"),
	{ list__length(Args, Arity) },
	mode_info_set_call_context(call(PredName/Arity)),
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

	% Return Result = yes if the called predicate never succeeds.

:- pred mode_info_never_succeeds(mode_info, pred_id, proc_id, bool).
:- mode mode_info_never_succeeds(mode_info_ui, in, in, out) is det.

mode_info_never_succeeds(ModeInfo, PredId, ProcId, Result) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_declared_determinism(ProcInfo, DeclaredDeterminism),
	determinism_never_succeeds(DeclaredDeterminism, Result).

:- pred determinism_never_succeeds(determinism, bool).
:- mode determinism_never_succeeds(in, out) is det.

determinism_never_succeeds(erroneous, yes).
determinism_never_succeeds(failure, yes).
determinism_never_succeeds(det, no).
determinism_never_succeeds(semidet, no).
determinism_never_succeeds(nondet, no).
determinism_never_succeeds(unspecified, no).

:- pred goal_get_nonlocals(hlds__goal, set(var)).
:- mode goal_get_nonlocals(in, out) is det.

goal_get_nonlocals(_Goal - GoalInfo, NonLocals) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals).

%-----------------------------------------------------------------------------%

:- pred compute_instmap_delta(instmap, instmap, set(var), instmap_delta).
:- mode compute_instmap_delta(in, in, in, out) is det.

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

:- pred mode_info_add_goals_live_vars(list(hlds__goal), mode_info, mode_info).
:- mode mode_info_add_goals_live_vars(in, mode_info_di, mode_info_uo) is det.

mode_info_add_goals_live_vars([]) --> [].
mode_info_add_goals_live_vars([Goal | Goals]) -->
	{ goal_get_nonlocals(Goal, Vars) },
	mode_info_add_live_vars(Vars),
	mode_info_add_goals_live_vars(Goals).

:- pred modecheck_conj_list_2(list(hlds__goal), list(hlds__goal),
				mode_info, mode_info).
:- mode modecheck_conj_list_2(in, out, mode_info_di, mode_info_uo) is det.

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
	( { delay_info__wakeup_goal(DelayInfo1, WokenGoal, DelayInfo2) } ->
		mode_checkpoint(wakeup, "goal"),
		{ DelayInfo = DelayInfo2 },
		{ Goals1 = [WokenGoal | Goals0] }
	;
		{ DelayInfo = DelayInfo1 },
		{ Goals1 = Goals0 }
	),
	mode_info_set_delay_info(DelayInfo),
	( { Errors = [] } ->
		{ Goals = [Goal | Goals2] }
	;
		{ Goals = Goals2 }
	),
	modecheck_conj_list_2(Goals1, Goals2).

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

	% Schedule a conjunction.
	% If it's empty, then there is nothing to do.
	% For non-empty conjunctions, we attempt to schedule the first
	% goal in the conjunction.  If successful, we wakeup a newly
	% pending goal (if any), and if not, we delay the goal.  Then we
	% continue attempting to schedule all the rest of the goals.

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

	% instmap_merge(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables, 
	%	checking that it is the same for every branch.

:- pred instmap_merge(set(var), list(instmap), merge_context,
		mode_info, mode_info).
:- mode instmap_merge(in, in, in, mode_info_di, mode_info_uo) is det.

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
		( ErrorList = [] ->
			ModeInfo2 = ModeInfo1
		;
			ErrorList = [Var - _|_], 
			set__singleton_set(WaitingVars, Var),
			mode_info_error(WaitingVars,
				mode_error_disj(MergeContext, ErrorList),
				ModeInfo1, ModeInfo2
			)
		),
		InstMap = reachable(InstMapping)
	;
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0
	),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo).

:- pred get_reachable_instmaps(list(instmap), list(map(var,inst))).
:- mode get_reachable_instmaps(in, out) is det.

:- get_reachable_instmaps([], _) when ever.
:- get_reachable_instmaps([X|_], _) when X.

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

:- pred modecheck_call_pred(pred_id, list(term), proc_id, mode_info, mode_info).
:- mode modecheck_call_pred(in, in, out, mode_info_di, mode_info_uo) is det.

modecheck_call_pred(PredId, Args, TheProcId, ModeInfo0, ModeInfo) :-
	term__term_list_to_var_list(Args, ArgVars),

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
					ModeInfo1, ModeInfo2),
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
		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars,
			WaitingVars, TheProcId, ModeInfo1, ModeInfo2),

			% restore the error list, appending any new error(s)
		mode_info_get_errors(ModeInfo2, NewErrors),
		list__append(OldErrors, NewErrors, Errors),
		mode_info_set_errors(Errors, ModeInfo2, ModeInfo)
	).

:- pred modecheck_call_pred_2(list(proc_id), pred_id, proc_table, list(var),
				set(var), proc_id, mode_info, mode_info).
:- mode modecheck_call_pred_2(in, in, in, in, in, out,
				mode_info_di, mode_info_uo) is det.

modecheck_call_pred_2([], _PredId, _Procs, ArgVars, WaitingVars, 0, ModeInfo0,
		ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	get_var_insts(ArgVars, InstMap, ArgInsts),
	mode_info_error(WaitingVars,
		mode_error_no_matching_mode(ArgVars, ArgInsts),
		ModeInfo0, ModeInfo).
	
modecheck_call_pred_2([ProcId | ProcIds], PredId, Procs, ArgVars, WaitingVars,
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
			ModeInfo2),
		TheProcId = ProcId,
		mode_info_never_succeeds(ModeInfo2, PredId, ProcId, Result),
		( Result = yes ->
			mode_info_set_instmap(unreachable, ModeInfo2, ModeInfo)
		;
			ModeInfo = ModeInfo2
		)
	;
			% otherwise, keep trying with the other modes
			% for the called pred
		Errors = [mode_error_info(WaitingVars2, _, _, _) | _],
		set__union(WaitingVars, WaitingVars2, WaitingVars3),

		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars,
				WaitingVars3, TheProcId, ModeInfo0, ModeInfo)
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
		set__singleton_set(WaitingVars, VarId),
		mode_info_error(WaitingVars,
			mode_error_var_has_inst(VarId, VarInst, Inst),
			ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

	% inst_matches_initial(InstA, InstB, ModuleInfo) is true iff
	% `InstA' specifies at least as much information as InstA,
	% and in those parts where they specify the same information,
	% `InstA' is at least as instantiated as `InstB'.
	% Thus, inst_matches_initial(not_reached, ground, _) succeeds),
	% since not_reached contains more information than ground - but
	% not vice versa.  Similarly, inst_matches_initial(bound(a),
	% bound(a;b), _) should succeed, but not vice versa.

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

	% To avoid infinite regress, we assume that
	% inst_matches_initial is true for any pairs of insts which
	% occur in Expansions.

inst_matches_initial_3(free, free, _, _).
inst_matches_initial_3(bound(_List), free, _, _).
inst_matches_initial_3(bound(ListA), bound(ListB), ModuleInfo, Expansions) :-
	bound_inst_list_matches_initial(ListA, ListB, ModuleInfo, Expansions).
inst_matches_initial_3(bound(List), ground, ModuleInfo, _) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_matches_initial_3(bound(List), abstract_inst(_,_), ModuleInfo, _) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_matches_initial_3(ground, free, _, _).
inst_matches_initial_3(ground, bound(_List), _, _ModuleInfo) :-
	true.	% XXX BUG! should fail if 
		% and List does not include all the constructors for the type.
		% or if List contains some not_reached insts.
inst_matches_initial_3(ground, ground, _, _).
inst_matches_initial_3(ground, abstract_inst(_,_), _, _) :-
		% I don't know what this should do.
	error("inst_matches_initial(ground, abstract_inst) == ??").
inst_matches_initial_3(abstract_inst(_,_), free, _, _).
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
	list__length(ArgsX, ArityX),
	list__length(ArgsY, ArityY),
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
	( Inst0 = defined_inst(InstName) ->
		inst_lookup(ModuleInfo, InstName, Inst1),
		inst_expand(ModuleInfo, Inst1, Inst)
	;
		Inst = Inst0
	).

%-----------------------------------------------------------------------------%

/*************** not used
:- pred modecheck_set_term_inst_list(list(term), list(inst),
					mode_info, mode_info).
:- mode modecheck_set_term_inst_list(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_term_inst_list([], []) --> [].
modecheck_set_term_inst_list([Arg | Args], [Inst | Insts]) -->
	{ Arg = term__variable(Var) },
	modecheck_set_var_inst(Var, Inst),
	modecheck_set_term_inst_list(Args, Insts).
***************/

:- pred modecheck_set_var_inst_list(list(var), list(inst),
					mode_info, mode_info).
:- mode modecheck_set_var_inst_list(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst_list([], []) --> [].
modecheck_set_var_inst_list([Var | Vars], [Inst | Insts]) -->
	modecheck_set_var_inst(Var, Inst),
	modecheck_set_var_inst_list(Vars, Insts).

:- pred modecheck_set_var_inst(var, inst, mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var, FinalInst, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(InstMapping0) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap_lookup_var(InstMap0, Var, Inst0),
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		(
			abstractly_unify_inst(dead, Inst0, FinalInst,
				ModuleInfo0, UnifyInst, ModuleInfo1)
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
			% we haven't done anything.

			inst_matches_initial(Inst0, Inst, ModuleInfo)
		->
			ModeInfo = ModeInfo1
		;
			% We must have either added some information,
			% or bound part of the var.  The call to
			% inst_matches_final will fail iff we have
			% bound part of a var.  If the var was locked,
			% then we need to report an error.

			\+ inst_matches_final(Inst, Inst0, ModuleInfo),
			mode_info_var_is_locked(ModeInfo0, Var)
		->
			set__singleton_set(WaitingVars, Var),
			mode_info_error(WaitingVars,
					mode_error_bind_var(Var, Inst0, Inst),
					ModeInfo1, ModeInfo
			)
		;
			map__set(InstMapping0, Var, Inst, InstMapping),
			InstMap = reachable(InstMapping),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo2),
			mode_info_get_delay_info(ModeInfo2, DelayInfo0),
			delay_info__bind_var(DelayInfo0, Var, DelayInfo),
			mode_info_set_delay_info(DelayInfo, ModeInfo2, ModeInfo)
		)
	;
		ModeInfo = ModeInfo0
	).

%-----------------------------------------------------------------------------%

	% inst_merge(InstA, InstB, InstC):
	%	Combine the insts found in different arms of a
	%	disjunction (or if-then-else).
	%	The information in InstC is the minimum of the
	%	information in InstA and InstB.  Where InstA and
	%	InstB specify a binding (free or bound), it must be
	%	the same in both.

:- pred inst_merge(inst, inst, module_info, inst, module_info).
:- mode inst_merge(in, in, in, out, out) is semidet.

inst_merge(InstA, InstB, ModuleInfo0, Inst, ModuleInfo) :-
		% check whether this pair of insts is already in
		% the merge_insts table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_merge_insts(InstTable0, MergeInstTable0),
	ThisInstPair = InstA - InstB,
	( map__search(MergeInstTable0, ThisInstPair, Result) ->
		ModuleInfo = ModuleInfo0,
		( Result = known(MergedInst) ->
			Inst = MergedInst
		;
			Inst = defined_inst(merge_inst(InstA, InstB))
		)
	;
			% insert ThisInstPair into the table with value
			%`unknown' 
		map__insert(MergeInstTable0, ThisInstPair, unknown,
			MergeInstTable1),
		inst_table_set_merge_insts(InstTable0, MergeInstTable1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

			% merge the insts
		inst_merge_2(InstA, InstB, ModuleInfo1, Inst, ModuleInfo2),

			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_merge_insts(InstTable2, MergeInstTable2),
		map__set(MergeInstTable2, ThisInstPair, known(Inst),
			MergeInstTable3),
		inst_table_set_merge_insts(InstTable2, MergeInstTable3,
			InstTable3),
		module_info_set_insts(ModuleInfo2, InstTable3, ModuleInfo)
	).

:- pred inst_merge_2(inst, inst, module_info, inst, module_info).
:- mode inst_merge_2(in, in, in, out, out) is semidet.
			
inst_merge_2(InstA, InstB, ModuleInfo0, Inst, ModuleInfo) :-
	inst_expand(ModuleInfo0, InstA, InstA2),
	inst_expand(ModuleInfo0, InstB, InstB2),
	( InstB2 = not_reached ->
		Inst = InstA2,
		ModuleInfo = ModuleInfo0
	;
		inst_merge_3(InstA2, InstB2, ModuleInfo0, Inst, ModuleInfo)
	).

:- pred inst_merge_3(inst, inst, module_info, inst, module_info).
:- mode inst_merge_3(in, in, in, out, out) is semidet.

:- inst_merge_3(A, B, _, _, _) when A and B.

inst_merge_3(free, free, M, free, M).
inst_merge_3(bound(ListA), bound(ListB), ModuleInfo0, bound(List), ModuleInfo)
		:-
	bound_inst_list_merge(ListA, ListB, ModuleInfo0, List, ModuleInfo).
inst_merge_3(bound(ListA), ground, ModuleInfo, ground, ModuleInfo) :-
	bound_inst_list_is_ground(ListA, ModuleInfo).
inst_merge_3(ground, bound(ListB), ModuleInfo, ground, ModuleInfo) :-
	bound_inst_list_is_ground(ListB, ModuleInfo).
inst_merge_3(ground, ground, M, ground, M).
inst_merge_3(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		ModuleInfo0, abstract_inst(Name, Args), ModuleInfo) :-
	inst_list_merge(ArgsA, ArgsB, ModuleInfo0, Args, ModuleInfo).
inst_merge_3(not_reached, Inst, M, Inst, M).

:- pred inst_list_merge(list(inst), list(inst), module_info, list(inst),
			module_info).
:- mode inst_list_merge(in, in, in, out, out) is semidet.

inst_list_merge([], [], ModuleInfo, [], ModuleInfo).
inst_list_merge([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo0,
		[Arg | Args], ModuleInfo) :-
	inst_merge_2(ArgA, ArgB, ModuleInfo0, Arg, ModuleInfo1),
	inst_list_merge(ArgsA, ArgsB, ModuleInfo1, Args, ModuleInfo).

	% bound_inst_list_merge(Xs, Ys, ModuleInfo0, Zs, ModuleInfo):
	% The two input lists Xs and Ys must already be sorted.
	% Here we perform a sorted merge operation,
	% so that the functors of the output list Zs are the union
	% of the functors of the input lists Xs and Ys.

:- pred bound_inst_list_merge(list(bound_inst), list(bound_inst),
				module_info, list(bound_inst), module_info).
:- mode bound_inst_list_merge(in, in, in, out, out) is semidet.

bound_inst_list_merge(Xs, Ys, ModuleInfo0, Zs, ModuleInfo) :-
	( Xs = [] ->
		Zs = Ys,
		ModuleInfo = ModuleInfo0
	; Ys = [] ->
		Zs = Xs,
		ModuleInfo = ModuleInfo0
	;
		Xs = [X | Xs1],
		Ys = [Y | Ys1],
		X = functor(NameX, ArgsX),
		Y = functor(NameY, ArgsY),
		list__length(ArgsX, ArityX),
		list__length(ArgsY, ArityY),
		( NameX = NameY, ArityX = ArityY ->
			inst_list_merge(ArgsX, ArgsY, ModuleInfo0,
					Args, ModuleInfo1),
			Z = functor(NameX, Args),
			Zs = [Z | Zs1],
			bound_inst_list_merge(Xs1, Ys1, ModuleInfo1,
				Zs1, ModuleInfo)
		; compare(<, X, Y) ->
			Zs = [X | Zs1],
			bound_inst_list_merge(Xs1, Ys, ModuleInfo0,
						Zs1, ModuleInfo)
		;
			Zs = [Y | Zs1],
			bound_inst_list_merge(Xs, Ys1, ModuleInfo0,
						Zs1, ModuleInfo)
		)
	).

%-----------------------------------------------------------------------------%

	% inst_matches_final(InstA, InstB):
	%	Succeed iff InstA is compatible with InstB,
	%	i.e. iff InstA will satisfy the final inst
	%	requirement InstB.  This is true if the
	%	information specified by InstA is at least as
	%	great as that specified by InstB, and where the information
	%	is the same and both insts specify a binding, the binding
	%	must be identical.
	%
	%	The difference between inst_matches_initial and
	%	inst_matches_final is that inst_matches_initial requires
	%	only something which is at least as instantiated,
	%	whereas this predicate wants something which is an
	%	exact match (or not reachable).
	%
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
	bound_inst_list_is_ground(ListB, ModuleInfo).
		% XXX BUG! Should fail if there are not_reached
		% insts in ListB, or if ListB does not contain a complete list
		% of all the constructors for the type in question.
	%%% error("not implemented: `ground' matches_final `bound(...)'").
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
	list__length(ArgsX, ArityX),
	list__length(ArgsY, ArityY),
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
:- mode mode_checkpoint(in, in, mode_info_di, mode_info_uo) is det.

mode_checkpoint(Port, Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
        globals__lookup_option(debug_modes, bool(DoCheckPoint),
		IOState0, IOState1),
	( DoCheckPoint = yes ->
		mode_checkpoint_2(Port, Msg, ModeInfo0, IOState1, IOState)
	;
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred mode_checkpoint_2(port, string, mode_info, io__state, io__state).
:- mode mode_checkpoint_2(in, in, mode_info_ui, di, uo) is det.

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
		globals__lookup_option(statistics, bool(Statistics)),
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

:- pred modecheck_unification(term, term, pair(mode, mode), unification,
				mode_info, mode_info).
:- mode modecheck_unification(in, in, out, out, mode_info_di, mode_info_uo)
	is det.

modecheck_unification(term__variable(X), term__variable(Y), Modes, Unification,
			ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	instmap_lookup_var(InstMap0, Y, InstY),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_is_live(ModeInfo0, Y, LiveY),
	(
		( LiveX = live, LiveY = live ->
			abstractly_unify_inst(live, InstX, InstY, ModuleInfo0,
				UnifyInst, ModuleInfo1)
		;
			abstractly_unify_inst(dead, InstX, InstY, ModuleInfo0,
				UnifyInst, ModuleInfo1)
		)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1)
	;
		set__list_to_set([X, Y], WaitingVars),
		mode_info_error(WaitingVars, 
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
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	categorize_unify_var_var(ModeX, ModeY, X, Y, VarTypes, ModuleInfo,
		Unification).

modecheck_unification(term__variable(X), term__functor(Name, Args, _),
			Mode, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	term__term_list_to_var_list(Args, ArgVars),
	instmap_lookup_arg_list(ArgVars, InstMap0, InstArgs),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_list_is_live(ArgVars, ModeInfo0, LiveArgs),
	InstY = bound([functor(Name, InstArgs)]),
	(
		% the occur check: X = f(X) will always fail
		list__member(X, ArgVars)
	->
		ModeInfo1 = ModeInfo0,
		Inst = not_reached
	;
		abstractly_unify_inst_functor(LiveX, InstX, Name,
			InstArgs, LiveArgs, ModuleInfo0, UnifyInst, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1)
	;
		set__list_to_set([X | ArgVars], WaitingVars), % conservative
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, Name, Args,
							InstX, InstArgs),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
		Inst = not_reached
	),
	modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
	bind_args(Inst, ArgVars, ModeInfo2, ModeInfo),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Mode = ModeX - ModeY,
	get_mode_of_args(Inst, InstArgs, ModeArgs),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	categorize_unify_var_functor(ModeX, ModeArgs, X, Name, ArgVars,
			ModuleInfo, Unification).

modecheck_unification(term__functor(F, As, Context), term__variable(Y),
		Modes, Unification, ModeInfo0, ModeInfo) :-
	modecheck_unification(term__variable(Y), term__functor(F, As, Context),
		Modes, Unification, ModeInfo0, ModeInfo).
	
modecheck_unification(term__functor(_, _, _), term__functor(_, _, _),
		_, _, _, _) :-
	error("modecheck internal error: unification of term with term\n").

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(var), mode_info, mode_info).
:- mode bind_args(in, in, mode_info_di, mode_info_uo) is det.

		% This first clause shouldn't be necessary, but it is
		% until the code below marked "Loses information" get fixed.
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

:- pred bind_args_2(list(var), list(inst), mode_info, mode_info).
:- mode bind_args_2(in, in, mode_info_di, mode_info_uo) is det.

bind_args_2([], []) --> [].
bind_args_2([Arg | Args], [Inst | Insts]) -->
	modecheck_set_var_inst(Arg, Inst),
	bind_args_2(Args, Insts).

:- pred ground_args(list(var), mode_info, mode_info).
:- mode ground_args(in, mode_info_di, mode_info_uo) is det.

ground_args([]) --> [].
ground_args([Arg | Args]) -->
	modecheck_set_var_inst(Arg, ground),
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
:- mode get_mode_of_args_2(in, in, out) is det.

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

:- pred abstractly_unify_inst(is_live, inst, inst, module_info,
				inst, module_info).
:- mode abstractly_unify_inst(in, in, in, in, out, out) is semidet.

abstractly_unify_inst(Live, InstA, InstB, ModuleInfo0, Inst, ModuleInfo) :-
		% check whether this pair of insts is already in
		% the unify_insts table
	ThisInstPair = unify_inst_pair(Live, InstA, InstB),
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_unify_insts(InstTable0, UnifyInsts0),
	( map__search(UnifyInsts0, ThisInstPair, Result) ->
		( Result = known(UnifyInst) ->
			Inst = UnifyInst
		;
			Inst = defined_inst(unify_inst(Live, InstA, InstB))
		),
		ModuleInfo = ModuleInfo0
	;
			% insert ThisInstPair into the table with value
			% `unknown' 
		map__set(UnifyInsts0, ThisInstPair, unknown, UnifyInsts1),
		inst_table_set_unify_insts(InstTable0, UnifyInsts1, InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),
			% unify the insts
		inst_expand(ModuleInfo0, InstA, InstA2),
		inst_expand(ModuleInfo0, InstB, InstB2),
		( InstB2 = not_reached ->
			Inst = InstA2,
			ModuleInfo2 = ModuleInfo1
		;
			abstractly_unify_inst_3(Live, InstA2, InstB2,
				ModuleInfo1, Inst, ModuleInfo2)
		),
			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_unify_insts(InstTable2, UnifyInsts2),
		map__set(UnifyInsts2, ThisInstPair, known(Inst), UnifyInsts),
		inst_table_set_unify_insts(InstTable2, UnifyInsts, InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	).

	% Abstractly unify two expanded insts.
	% The is_live parameter is `live' iff *both* insts are live.

:- pred abstractly_unify_inst_3(is_live, inst, inst, module_info,
				inst, module_info).
:- mode abstractly_unify_inst_3(in, in, in, in, out, out) is semidet.

:- abstractly_unify_inst_3(A, B, C, _, _, _) when A and B and C.

abstractly_unify_inst_3(live, not_reached, _,		M, not_reached, M).

abstractly_unify_inst_3(live, free,	free,		_, _, _) :- fail.
abstractly_unify_inst_3(live, free,	bound(List),	M, bound(List), M) :-
	bound_inst_list_is_ground(List, M).
abstractly_unify_inst_3(live, free,	ground,		M, ground, M).
abstractly_unify_inst_3(live, free,	abstract_inst(_,_), _, _, _) :- fail.
	
abstractly_unify_inst_3(live, bound(List), free,	M, bound(List), M) :-
	bound_inst_list_is_ground(List, M).
abstractly_unify_inst_3(live, bound(ListX),bound(ListY), M0, bound(List), M) :-
	abstractly_unify_bound_inst_list(live, ListX, ListY, M0, List, M).
abstractly_unify_inst_3(live, bound(BoundInsts0),	ground,	M0,
		bound(BoundInsts), M) :-
	make_ground_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
abstractly_unify_inst_3(live, bound(List), abstract_inst(_,_), ModuleInfo,
							ground, ModuleInfo) :-
	bound_inst_list_is_ground(List, ModuleInfo).

abstractly_unify_inst_3(live, ground,	Inst0,		M0, Inst, M) :-
	make_ground_inst(Inst0, M0, Inst, M).

abstractly_unify_inst_3(live, abstract_inst(_,_), free,	_, _, _) :- fail.
abstractly_unify_inst_3(live, abstract_inst(_,_), bound(List), ModuleInfo,
				ground, ModuleInfo) :-
	bound_inst_list_is_ground(List, ModuleInfo).
abstractly_unify_inst_3(live, abstract_inst(_,_), ground, M, ground, M).
abstractly_unify_inst_3(live, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo0,
			abstract_inst(Name, Args), ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, live, ModuleInfo0,
		Args, ModuleInfo).

abstractly_unify_inst_3(dead, not_reached, _, M, not_reached, M).

abstractly_unify_inst_3(dead, free, Inst, M, Inst, M).
	
abstractly_unify_inst_3(dead, bound(List), free, M, bound(List), M).

abstractly_unify_inst_3(dead, bound(ListX), bound(ListY), M0, bound(List), M)
		:-
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M0, List, M).
abstractly_unify_inst_3(dead, bound(BoundInsts0), ground, M0,
			bound(BoundInsts), M) :-
	make_ground_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
abstractly_unify_inst_3(dead, bound(List), abstract_inst(N,As), ModuleInfo,
					Result, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(List)
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As)
	;
		fail
	).

abstractly_unify_inst_3(dead, ground,		Inst0,	M0, Inst, M) :-
	make_ground_inst(Inst0, M0, Inst, M).

abstractly_unify_inst_3(dead, abstract_inst(N,As), bound(List), ModuleInfo, 
							Result, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(List)
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As)
	;
		fail
	).
abstractly_unify_inst_3(dead, abstract_inst(_,_), ground,	M, ground, M).
abstractly_unify_inst_3(dead, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo0,
			abstract_inst(Name, Args), ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, dead, ModuleInfo0,
			Args, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Abstractly unify two inst lists.

:- pred abstractly_unify_inst_list(list(inst), list(inst), is_live, module_info,
					list(inst), module_info).
:- mode abstractly_unify_inst_list(in, in, in, in, out, out) is semidet.

abstractly_unify_inst_list([], [], _, M, [], M).
abstractly_unify_inst_list([X|Xs], [Y|Ys], Live, ModuleInfo0,
				[Z|Zs], ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, ModuleInfo0, Z, ModuleInfo1),
	abstractly_unify_inst_list(Xs, Ys, Live, ModuleInfo1, Zs, ModuleInfo).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.

:- pred abstractly_unify_inst_functor(is_live, inst, const, list(inst),
				list(is_live), module_info, inst, module_info).
:- mode abstractly_unify_inst_functor(in, in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_inst_functor(Live, InstA, Name, ArgInsts, ArgLives,
		ModuleInfo0, Inst, ModuleInfo) :-
	inst_expand(ModuleInfo0, InstA, InstA2),
	abstractly_unify_inst_functor_2(Live, InstA2, Name, ArgInsts, ArgLives,
			ModuleInfo0, Inst, ModuleInfo).

:- pred abstractly_unify_inst_functor_2(is_live, inst, const, list(inst),
				list(is_live), module_info, inst, module_info).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_inst_functor_2(live, not_reached, _, _, _, M, not_reached, M).
abstractly_unify_inst_functor_2(live, free, Name, Args, ArgLives, ModuleInfo,
			bound([functor(Name, Args)]), ModuleInfo) :-
	inst_list_is_ground_or_dead(Args, ArgLives, ModuleInfo).
abstractly_unify_inst_functor_2(live, bound(ListX), Name, Args, ArgLives, M0,
		bound(List), M) :-
	abstractly_unify_bound_inst_list_lives(ListX, Name, Args, ArgLives,
						M0, List, M).
abstractly_unify_inst_functor_2(live, ground, Name, ArgInsts, _ArgLives, M0,
		Inst, M) :-
	make_ground_inst_list(ArgInsts, M0, GroundArgInsts, M), 
	Inst = bound([functor(Name, GroundArgInsts)]).
abstractly_unify_inst_functor_2(live, abstract_inst(_,_), _, _, _, _, _, _) :-
	fail.

abstractly_unify_inst_functor_2(dead, not_reached, _, _, _, M, not_reached, M).
abstractly_unify_inst_functor_2(dead, free, Name, Args, _ArgLives, M,
			bound([functor(Name, Args)]), M).
abstractly_unify_inst_functor_2(dead, bound(ListX), Name, Args, _ArgLives, M0,
		bound(List), M) :-
	ListY = [functor(Name, Args)],
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M0, List, M). 
abstractly_unify_inst_functor_2(dead, ground, Name, ArgInsts, _ArgLives, M0,
		Inst, M) :-
	make_ground_inst_list(ArgInsts, M0, GroundArgInsts, M),
	Inst = bound([functor(Name, GroundArgInsts)]).
abstractly_unify_inst_functor_2(dead, abstract_inst(_,_), _, _, _, _, _, _) :-
	fail.

:- pred make_ground_inst_list(list(inst), module_info, list(inst), module_info).
:- mode make_ground_inst_list(in, in, out, out) is det.

make_ground_inst_list([], ModuleInfo, [], ModuleInfo).
make_ground_inst_list([Inst0 | Insts0], ModuleInfo0,
		[Inst | Insts], ModuleInfo) :-
	make_ground_inst(Inst0, ModuleInfo0, Inst, ModuleInfo1),
	make_ground_inst_list(Insts0, ModuleInfo1, Insts, ModuleInfo).

:- pred make_ground_inst(inst, module_info, inst, module_info).
:- mode make_ground_inst(in, in, out, out) is det.

make_ground_inst(not_reached, M, not_reached, M).
make_ground_inst(free, M, ground, M).
make_ground_inst(bound(BoundInsts0), M0, bound(BoundInsts), M) :-
	make_ground_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
make_ground_inst(ground, M, ground, M).
make_ground_inst(inst_var(_), _, _, _) :-
	error("free inst var").
make_ground_inst(abstract_inst(_,_), M, ground, M).
make_ground_inst(defined_inst(InstName), ModuleInfo0, Inst, ModuleInfo) :-
		% check whether the inst name is already in the
		% ground_inst table
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_ground_insts(InstTable0, GroundInsts0),
	( map__search(GroundInsts0, InstName, Result) ->
		( Result = known(GroundInst) ->
			Inst = GroundInst
		;
			Inst = defined_inst(ground_inst(InstName))
		),
		ModuleInfo = ModuleInfo0
	;
		% insert the inst name in the ground_inst table, with
		% value `unknown' for the moment
		map__set(GroundInsts0, InstName, unknown, GroundInsts1),
		inst_table_set_ground_insts(InstTable0, GroundInsts1,
			InstTable1),
		module_info_set_insts(ModuleInfo0, InstTable1, ModuleInfo1),

		% expand the inst name, and invoke ourself recursively on
		% it's expansion
		inst_lookup(ModuleInfo1, InstName, Inst0),
		inst_expand(ModuleInfo1, Inst0, Inst1),
		make_ground_inst(Inst1, ModuleInfo1, Inst, ModuleInfo2),

		% now that we have determined the resulting Inst, store
		% the appropriate value `known(Inst)' in the ground_inst
		% table
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_ground_insts(InstTable2, GroundInsts2),
		map__set(GroundInsts2, InstName, known(Inst), GroundInsts),
		inst_table_set_ground_insts(InstTable2, GroundInsts,
			InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	).

:- pred make_ground_bound_inst_list(list(bound_inst), module_info,
					list(bound_inst), module_info).
:- mode make_ground_bound_inst_list(in, in, out, out) is det.

make_ground_bound_inst_list([], ModuleInfo, [], ModuleInfo).
make_ground_bound_inst_list([Bound0 | Bounds0], ModuleInfo0,
			[Bound | Bounds], ModuleInfo) :-
	Bound0 = functor(Name, ArgInsts0),
	make_ground_inst_list(ArgInsts0, ModuleInfo0, ArgInsts, ModuleInfo1),
	Bound = functor(Name, ArgInsts),
	make_ground_bound_inst_list(Bounds0, ModuleInfo1, Bounds, ModuleInfo).

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
		list(bound_inst), module_info, list(bound_inst), module_info).
:- mode abstractly_unify_bound_inst_list(in, in, in, in, out, out) is semidet.

:- abstractly_unify_bound_inst_list(_, Xs, Ys, _, _, _) when Xs and Ys. % Index

abstractly_unify_bound_inst_list(_, [], _, ModuleInfo, [], ModuleInfo).
abstractly_unify_bound_inst_list(_, [_|_], [], ModuleInfo, [], ModuleInfo).
abstractly_unify_bound_inst_list(Live, [X|Xs], [Y|Ys], ModuleInfo0,
		L, ModuleInfo) :-
	X = functor(NameX, ArgsX),
	list__length(ArgsX, ArityX),
	Y = functor(NameY, ArgsY),
	list__length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
	    ( abstractly_unify_inst_list(ArgsX, ArgsY, Live, ModuleInfo0,
			Args, ModuleInfo1)
	    ->
		L = [functor(NameX, Args) | L1],
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
						ModuleInfo1, L1, ModuleInfo)
	    ;
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
						ModuleInfo0, L, ModuleInfo)
	    )
	;
	    ( compare(<, X, Y) ->
		abstractly_unify_bound_inst_list(Live, Xs, [Y|Ys],
						ModuleInfo0, L, ModuleInfo)
	    ;
		abstractly_unify_bound_inst_list(Live, [X|Xs], Ys,
						ModuleInfo0, L, ModuleInfo)
	    )
	).

:- pred abstractly_unify_bound_inst_list_lives(list(bound_inst), const,
	list(inst), list(is_live), module_info, list(bound_inst), module_info).
:- mode abstractly_unify_bound_inst_list_lives(in, in, in, in, in, out, out)
	is semidet.

abstractly_unify_bound_inst_list_lives([], _, _, _, ModuleInfo, [], ModuleInfo).
abstractly_unify_bound_inst_list_lives([X|Xs], NameY, ArgsY, LivesY,
		ModuleInfo0, L, ModuleInfo) :-
	X = functor(NameX, ArgsX),
	list__length(ArgsX, ArityX),
	list__length(ArgsY, ArityY),
	( 
		NameX = NameY,
		ArityX = ArityY
	->
		abstractly_unify_inst_list_lives(ArgsX, ArgsY, LivesY,
			ModuleInfo0, Args, ModuleInfo),
		L = [functor(NameX, Args)]
	;
		abstractly_unify_bound_inst_list_lives(Xs, NameY, ArgsY,
					LivesY, ModuleInfo0, L, ModuleInfo)
	).

:- pred abstractly_unify_inst_list_lives(list(inst), list(inst), list(is_live),
					module_info, list(inst), module_info).
:- mode abstractly_unify_inst_list_lives(in, in, in, in, out, out) is semidet.

abstractly_unify_inst_list_lives([], [], [], ModuleInfo, [], ModuleInfo).
abstractly_unify_inst_list_lives([X|Xs], [Y|Ys], [Live|Lives], ModuleInfo0,
		[Z|Zs], ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, ModuleInfo0, Z, ModuleInfo1),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, ModuleInfo1, Zs,
		ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred categorize_unify_var_var(mode, mode, var, var, map(var, type), 
				module_info, unification).
:- mode categorize_unify_var_var(in, in, in, in, in, in, out) is det.

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
		ModeX = (IX -> FX),
		ModeY = (IY -> FY),
		Unification = complicated_unify((IX - IY) -> (FX - FY),
				term__variable(X), term__variable(Y))
	).

:- pred categorize_unify_var_functor(mode, list(mode), var, const,
				list(var), module_info, unification).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, out) is det.

categorize_unify_var_functor(ModeX, ArgModes0, X, Name, ArgVars, ModuleInfo,
		Unification) :-
	list__length(ArgVars, Arity),
	make_functor_cons_id(Name, Arity, ConsId),
	mode_util__modes_to_uni_modes(ModeX, ArgModes0,
						ModuleInfo, ArgModes),
	(
		mode_is_output(ModuleInfo, ModeX)
	->
		Unification = construct(X, ConsId, ArgVars, ArgModes)
	; 
		% XXXXX
		% module_info_get_insts(ModuleInfo, ModeX, InitialInst0, _Final),
		% inst_expand(ModuleInfo, InitialInst0, InitialInst),
		% ( InitialInst = bound([SingleFunctor]) ->
		
		Unification = deconstruct(X, ConsId, ArgVars, ArgModes)
	).

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

	% record a mode error (and associated context _info) in the mode_info.

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

:- pred modecheck_report_errors(mode_info, mode_info).
:- mode modecheck_report_errors(mode_info_di, mode_info_uo) is det.

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
