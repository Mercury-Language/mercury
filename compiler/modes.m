%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: modes.nl.
% Main author: fjh.
%
% This file contains a mode-checker.
% Still very incomplete.

% XXX unifying `free' with `free' should be allowed if one of the variables
% is dead.

% XXX unifying `free' with `f(free)' introduces aliasing, and should be
% disallowed, unless either the variable is dead or all the free argument(s)
% are dead.

% XXX break unifications into "micro-unifications"

% XXX should handle reordering of conjunctions.

% XXX we should check that the final insts are correct.

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
		Attempt to schedule each sub-goal.
		If a sub-goal can be scheduled, then schedule it,
		and continue with the remaining sub-goals until
		there is only one left, which gets mode-checked.
		If no sub-goals can be scheduled, report a mode error.
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
a local variable, then report the error.

******************************************/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module modes.
:- interface.
:- import_module hlds, io, prog_io.

:- pred modecheck(module_info, module_info, io__state, io__state).
:- mode modecheck(in, out, di, uo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module mode_util.
:- import_module globals, options, mercury_to_mercury, hlds_out.

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
	{ moduleinfo_predids(Module0, PredIds) },
	modecheck_pred_modes_2(PredIds, Module0, Module).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_pred_modes_2(list(pred_id), module_info, 
			module_info, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, out, di, uo).

modecheck_pred_modes_2([], ModuleInfo, ModuleInfo) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ moduleinfo_preds(ModuleInfo0, Preds0) },
	{ map__search(Preds0, PredId, PredInfo0) },
	{ predinfo_clauses_info(PredInfo0, ClausesInfo0) },
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
		{ moduleinfo_set_preds(ModuleInfo0, Preds1, ModuleInfo1) },
		modecheck_procs(PredId, ModuleInfo1, PredInfo1, PredInfo, Errs),
		{ map__set(Preds1, PredId, PredInfo, Preds) },
		{ moduleinfo_set_preds(ModuleInfo1, Preds, ModuleInfo2) },
		{ moduleinfo_num_errors(ModuleInfo2, NumErrors0) },
		{ NumErrors is NumErrors0 + Errs },
		{ moduleinfo_set_num_errors(ModuleInfo2, NumErrors,
						ModuleInfo3) }
	),
	modecheck_pred_modes_2(PredIds, ModuleInfo3, ModuleInfo).

%-----------------------------------------------------------------------------%

	% In the hlds, we initially record the clauses for a predicate
	% in the clauses_info data structure which is part of the
	% predinfo data structure.  But once the clauses have been
	% type-checked, we want to have a separate copy of each clause
	% fo
	% each clauses record a list of the modes for which it applies.
	% At this point in the compilation, we must make 

:- pred copy_clauses_to_procs(pred_info, pred_info).
:- mode copy_clauses_to_procs(in, out).

copy_clauses_to_procs(PredInfo0, PredInfo) :-
	predinfo_clauses_info(PredInfo0, ClausesInfo),
	predinfo_procedures(PredInfo0, Procs0),
	map__keys(Procs0, ProcIds),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
	predinfo_set_procedures(PredInfo0, Procs, PredInfo).

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
		goalinfo_init(GoalInfo),
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
	{ predinfo_procedures(PredInfo0, Procs0) },
	{ map__keys(Procs0, ProcIds) },
	modecheck_procs_2(ProcIds, PredId, ModuleInfo, Procs0, 0,
				Procs, NumErrors),
	{ predinfo_set_procedures(PredInfo0, Procs, PredInfo) }.

	% Iterate over the list of modes for a predicate.

:- pred modecheck_procs_2(list(proc_id), pred_id, module_info,
		proc_table, int, proc_table, int, io__state, io__state).
:- mode modecheck_procs_2(in, in, in, in, in, out, out, di, uo).

modecheck_procs_2([], _PredId, _ModuleInfo, Procs, Errs, Procs, Errs) --> [].
modecheck_procs_2([ProcId|ProcIds], PredId, ModuleInfo, Procs0, Errs0,
					Procs, Errs) -->
		% lookup the procinfo
	{ map__lookup(Procs0, ProcId, ProcInfo0) },
		% mode-check that mode of the predicate
	modecheck_proc(ProcId, PredId, ModuleInfo, ProcInfo0,
			ProcInfo, NumErrors),
	{ Errs1 is Errs0 + NumErrors },
		% save the procinfo
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
		% extract the useful fields in the procinfo
	procinfo_goal(ProcInfo0, Body0),
	procinfo_argmodes(ProcInfo0, ArgModes),
	procinfo_context(ProcInfo0, Context),
	procinfo_headvars(ProcInfo0, HeadVars),
		% modecheck the clause - first set the initial instantiation
		% of the head arguments, mode-check the body, and
		% then check that the final instantiation matches that in
		% the mode declaration
	mode_list_get_initial_insts(ArgModes, ModuleInfo, ArgInitialInsts),
	map__from_corresponding_lists(HeadVars, ArgInitialInsts, InstMapping0),
	modeinfo_init(IOState0, ModuleInfo, PredId, ProcId, Context,
			InstMapping0, ModeInfo0),
	modecheck_goal(Body0, Body, ModeInfo0, ModeInfo1),
	modecheck_final_insts(HeadVars, ArgModes, ModeInfo1, ModeInfo),
	modeinfo_get_num_errors(ModeInfo, NumErrors),
	modeinfo_get_io_state(ModeInfo, IOState),
	procinfo_set_goal(ProcInfo0, Body, ProcInfo).

:- pred modecheck_final_insts(list(var), list(mode), modeinfo, modeinfo).
:- mode modecheck_final_insts(in, in, in, out).

modecheck_final_insts(_, _, ModeInfo, ModeInfo).	% XXX Stub only!!!

/****
modecheck_final_insts(HeadVars, ArgModes, ModeInfo1, ModeInfo) :-
	modeinfo_found_error(ModeInfo, Error),
	( Error = no ->
		mode_list_get_final_insts(ArgModes, ModuleInfo, ArgFinalInsts),
		check_final_insts(
*/

%-----------------------------------------------------------------------------%

% Input-output: InstMap - Stored in the ModeInfo, which is passed as an
%			  argument pair
%		Goal	- Passed as an argument pair
% Input only:   Symbol tables	(constant)
%			- Stored in the ModuleInfo which is in the ModeInfo
%		Context Info	(changing as we go along the clause)
%			- Stored in the ModeInfo
% Output only:	Error Message(s)
%			- Output directly to stdout.

:- pred modecheck_goal(hlds__goal, hlds__goal, modeinfo, modeinfo).
:- mode modecheck_goal(in, out, modeinfo_di, modeinfo_uo) is det.

modecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, ModeInfo0, ModeInfo) :-
		%
		% store the current context in the modeinfo
		%
	%%% goalinfo_get_context(GoalInfo0, Context),
	%%% modeinfo_set_context(ModeInfo0, Context, ModeInfo1)
		%
		% modecheck the goal, and then store the changes in
		% instantiation of the non-local vars in the goal's goalinfo.
		%
	goalinfo_get_nonlocals(GoalInfo0, NonLocals),
	modeinfo_get_vars_instmap(ModeInfo0, NonLocals, InstMap0),
	modecheck_goal_2(Goal0, NonLocals, Goal, ModeInfo0, ModeInfo),
	modeinfo_get_vars_instmap(ModeInfo, NonLocals, InstMap),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goalinfo_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).

:- pred modecheck_goal_2(hlds__goal_expr, set(var), hlds__goal_expr,
			modeinfo, modeinfo).
:- mode modecheck_goal_2(in, in, out, modeinfo_di, modeinfo_uo) is det.

modecheck_goal_2(conj(List0), _, conj(List1)) -->
	mode_checkpoint("conj"),
	modecheck_conj_list(List0, List1).

modecheck_goal_2(disj(List0), NonLocals, disj(List)) -->
	mode_checkpoint("disj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] }
	;
		modecheck_disj_list(List0, List, InstMapList),
		instmap_merge(NonLocals, InstMapList, disj)
	).

modecheck_goal_2(if_then_else(Vs, A0, B0, C0), NonLocals,
		if_then_else(Vs, A, B, C)) -->
	mode_checkpoint("if-then-else"),
	modeinfo_dcg_get_instmap(InstMap0),
	modeinfo_lock_vars(NonLocals),
	modecheck_goal(A0, A),
	modeinfo_unlock_vars(NonLocals),
	modecheck_goal(B0, B),
	modeinfo_dcg_get_instmap(InstMapB),
	modeinfo_set_instmap(InstMap0),
	modecheck_goal(C0, C),
	modeinfo_dcg_get_instmap(InstMapC),
	instmap_merge(NonLocals, [InstMapB, InstMapC], if_then_else).

modecheck_goal_2(not(Vs, A0), NonLocals, not(Vs, A)) -->
	mode_checkpoint("not"),
	modeinfo_lock_vars(NonLocals),
	modecheck_goal(A0, A),
	modeinfo_unlock_vars(NonLocals).

modecheck_goal_2(some(Vs, G0), _, some(Vs, G)) -->
	mode_checkpoint("some"),
	modecheck_goal(G0, G).

modecheck_goal_2(all(Vs, G0), NonLocals, all(Vs, G)) -->
	mode_checkpoint("all"),
	modeinfo_lock_vars(NonLocals),
	modecheck_goal(G0, G),
	modeinfo_unlock_vars(NonLocals).

modecheck_goal_2(call(PredId, _, Args, Builtin), _,
		 call(PredId, Mode, Args, Builtin)) -->
	mode_checkpoint("call"),
	modeinfo_set_call_context(call(PredId)),
	modecheck_call_pred(PredId, Args, Mode).

modecheck_goal_2(unify(A, B, _, _, UnifyContext), _,
		 unify(A, B, Mode, UnifyInfo, UnifyContext)) -->
	mode_checkpoint("unify"),
	modeinfo_set_call_context(unify(UnifyContext)),
	modecheck_unification(A, B, Mode, UnifyInfo).

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
		% XXX should use inst_is_compat/3
	( InstA = InstB ->
		AssocList1 = AssocList
	;
		AssocList = [ Var - InstB | AssocList1 ]
	),
	compute_instmap_delta_2(Vars, InstMapA, InstMapB, AssocList1).

:- pred instmap_lookup_var(instmap, var, inst).
:- mode instmap_lookup_var(in, in, out) is det.

instmap_lookup_var(InstMap, Var, Inst) :-
	( map__search(InstMap, Var, VarInst) ->
		Inst = VarInst
	;
		Inst = free
	).

:- pred instmap_lookup_arg_list(list(term), instmap, list(inst)).
:- mode instmap_lookup_arg_list(in, in, out).

instmap_lookup_arg_list([], _InstMap, []).
instmap_lookup_arg_list([Arg|Args], InstMap, [Inst|Insts]) :-
	Arg = term_variable(Var),
	instmap_lookup_var(InstMap, Var, Inst),
	instmap_lookup_arg_list(Args, InstMap, Insts).

%-----------------------------------------------------------------------------%

	% XXX we don't reorder conjunctions yet

:- pred modecheck_conj_list(list(hlds__goal), list(hlds__goal),
				modeinfo, modeinfo).
:- mode modecheck_conj_list(in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_conj_list([], []) --> [].
modecheck_conj_list([Goal0 | Goals0], [Goal | Goals]) -->
	modecheck_goal(Goal0, Goal),
	modecheck_conj_list(Goals0, Goals).

%-----------------------------------------------------------------------------%

	% XXX we don't handle disjunctions or if-then-else yet

:- pred modecheck_disj_list(list(hlds__goal), list(hlds__goal), list(instmap),
				modeinfo, modeinfo).
:- mode modecheck_disj_list(in, out, out, modeinfo_di, modeinfo_uo).

modecheck_disj_list([], [], []) --> [].
modecheck_disj_list([Goal0 | Goals0], [Goal | Goals], [InstMap | InstMaps]) -->
	modeinfo_dcg_get_instmap(InstMap0),
	modecheck_goal(Goal0, Goal),
	modeinfo_dcg_get_instmap(InstMap),
	modeinfo_set_instmap(InstMap0),
	modecheck_disj_list(Goals0, Goals, InstMaps).

	% instmap_merge_2(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, checking that
	%	the resulting instantiatedness of all the nonlocal variables
	%	is the same for every branch.

:- type merge_context
	---> disj
	;    if_then_else.

:- pred instmap_merge(set(var), list(instmap), merge_context,
		modeinfo, modeinfo).
:- mode instmap_merge(in, in, in, modeinfo_di, modeinfo_uo).

instmap_merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
	modeinfo_get_moduleinfo(ModeInfo0, ModuleInfo),
	map__init(InstMap0),
	set__to_sorted_list(NonLocals, NonLocalsList),
	instmap_merge_2(NonLocalsList, InstMapList, ModuleInfo, InstMap0,
				InstMap, ErrorList),
	( ErrorList = [] ->
		ModeInfo2 = ModeInfo0
	;
		modeinfo_get_io_state(ModeInfo0, IOState0),
		report_mode_error_disj(ModeInfo0, MergeContext, ErrorList,
				IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_incr_errors(ModeInfo1, ModeInfo2)
	),
	modeinfo_set_instmap(InstMap, ModeInfo2, ModeInfo).

	% instmap_merge_2(Vars, InstMaps, ModuleInfo, ErrorList):
	%	Let `ErrorList' be the list of variables in `Vars' for
	%	there are two instmaps in `InstMaps' for which the inst
	%	the variable is different.

:- type merge_errors == assoc_list(var, list(inst)).

:- pred instmap_merge_2(list(var), list(instmap), module_info, instmap,
			instmap, merge_errors).
:- mode instmap_merge_2(in, in, in, in, out, out) is det.

instmap_merge_2([], _, _, InstMap, InstMap, []).
instmap_merge_2([Var|Vars], InstMapList, ModuleInfo, InstMap0,
			InstMap, ErrorList) :-
	instmap_merge_2(Vars, InstMapList, ModuleInfo, InstMap0,
			InstMap1, ErrorList1),
	instmap_merge_var(InstMapList, Var, ModuleInfo, Insts, Error),
	( Error = yes ->
		ErrorList = [Var - Insts | ErrorList1],
		map__set(InstMap1, Var, ground, InstMap)
	;
		ErrorList = ErrorList1,
		Insts = [Inst | _],
		map__set(InstMap1, Var, Inst, InstMap)
	).

	% instmap_merge_var(InstMaps, Var, ModuleInfo, Insts, Error):
	%	Let `Insts' be the list of the inst of `Var' in the
	%	corresponding `InstMaps'.  Let `Error' be yes iff
	%	there are two instmaps for which the inst of `Var'
	%	is different.

:- pred instmap_merge_var(list(instmap), var, module_info, list(inst), bool).
:- mode instmap_merge_var(in, in, in, out, out) is det.

instmap_merge_var([], _, _, [], no).
instmap_merge_var([InstMap | InstMaps], Var, ModuleInfo, Insts, Error) :-
	instmap_lookup_var(InstMap, Var, Inst),
	instmap_merge_var_2(InstMaps, Inst, Var, ModuleInfo, Insts, Error).

:- pred instmap_merge_var_2(list(instmap), inst, var, module_info,
				list(inst), bool).
:- mode instmap_merge_var_2(in, in, in, in, out, out) is det.

instmap_merge_var_2([], Inst, _Var, _ModuleInfo, [Inst], no).
instmap_merge_var_2([InstMapB | InstMaps], InstA, Var, ModuleInfo,
			Insts, Error) :-
	instmap_lookup_var(InstMapB, Var, InstB),
	( inst_is_compat(InstA, InstB, ModuleInfo) ->
		Error = no
	;
		Error = Error1
	),
	Insts = [InstA | Insts1],
	instmap_merge_var_2(InstMaps, InstB, Var, ModuleInfo, Insts1, Error1).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_disj(modeinfo, merge_context, merge_errors,
				io__state, io__state).
:- mode report_mode_error_disj(modeinfo_no_io, in, in, di, uo).

report_mode_error_disj(ModeInfo, MergeContext, ErrorList) -->
	{ modeinfo_get_context(ModeInfo, Context) },
	modeinfo_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode mismatch in "),
	write_merge_context(MergeContext),
	io__write_string(".\n"),
	write_merge_error_list(ErrorList, ModeInfo).

:- pred write_merge_error_list(merge_errors, modeinfo, io__state, io__state).
:- mode write_merge_error_list(in, modeinfo_no_io, di, uo).

write_merge_error_list([], _) --> [].
write_merge_error_list([Var - Insts | Errors], ModeInfo) -->
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },
	prog_out__write_context(Context),
	io__write_string("  `"),
	io__write_variable(Var, VarSet),
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

:- pred modecheck_call_pred(pred_id, list(term), proc_id, modeinfo, modeinfo).
:- mode modecheck_call_pred(in, in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_call_pred(PredId, Args, ProcId, ModeInfo0, ModeInfo) :-
		% look up the called predicate's arg modes
	modeinfo_get_preds(ModeInfo0, Preds),
	modeinfo_get_moduleinfo(ModeInfo0, ModuleInfo),
	map__lookup(Preds, PredId, PredInfo),
	predinfo_procedures(PredInfo, Procs),
		% XXX We should handle multiple modes per predicate!
		% At the moment we just assume that all calls are to
		% the first listed mode for each predicate.
	ProcId = 0,
	map__lookup(Procs, ProcId, ProcInfo),
	procinfo_argmodes(ProcInfo, ProcArgModes),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),
	term_list_to_var_list(Args, ArgVars),
	modecheck_var_has_inst_list(ArgVars, InitialInsts,
				ModeInfo0, ModeInfo1),
	mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
	modecheck_set_var_inst_list(ArgVars, FinalInsts, ModeInfo1, ModeInfo).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of insts, ensure
	% that each variable has the corresponding inst.

:- pred modecheck_var_has_inst_list(list(var), list(inst), modeinfo,
					modeinfo).
:- mode modecheck_var_has_inst_list(in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_var_has_inst_list([], []) --> [].
modecheck_var_has_inst_list([Var|Vars], [Inst|Insts]) -->
	modecheck_var_has_inst(Var, Inst),
	modecheck_var_has_inst_list(Vars, Insts).

:- pred modecheck_var_has_inst(var, inst, modeinfo, modeinfo).
:- mode modecheck_var_has_inst(in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_var_has_inst(VarId, Inst, ModeInfo0, ModeInfo) :-
	modeinfo_get_instmap(ModeInfo0, InstMap),
	instmap_lookup_var(InstMap, VarId, VarInst),

	modeinfo_get_moduleinfo(ModeInfo0, ModuleInfo),
	( inst_gteq(VarInst, Inst, ModuleInfo) ->
		ModeInfo = ModeInfo0
	;
		modeinfo_get_io_state(ModeInfo0, IOState0),
		report_mode_error_var_has_inst(ModeInfo0, VarId, VarInst, Inst,
				IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_incr_errors(ModeInfo1, ModeInfo)
	).

	% inst_gteq(InstA, InstB, ModuleInfo) is true iff
	% `InstA' is at least as instantiated as `InstB'.

:- pred inst_gteq(inst, inst, module_info).
:- mode inst_gteq(in, in, in) is semidet.

inst_gteq(InstA, InstB, ModuleInfo) :-
	inst_expand(ModuleInfo, InstA, InstA2),
	inst_expand(ModuleInfo, InstB, InstB2),
	inst_gteq_2(InstA2, InstB2, ModuleInfo).

:- pred inst_gteq_2(inst, inst, module_info).
:- mode inst_gteq_2(in, in, in) is semidet.

:- inst_gteq_2(InstA, InstB, _) when InstA and InstB.	% Indexing.

	% inst_gteq_2(InstA, InstB, ModuleInfo) is true iff
	% `InstA' is at least as instantiated as `InstB'.

inst_gteq_2(free, free, _).
inst_gteq_2(bound(_List), free, _).
inst_gteq_2(bound(ListA), bound(ListB), ModuleInfo) :-
	bound_inst_list_gteq(ListA, ListB, ModuleInfo).
inst_gteq_2(bound(List), ground, ModuleInfo) :-
	bound_inst_list_is_ground(List, ModuleInfo).
inst_gteq_2(ground, _, _).
inst_gteq_2(abstract_inst(_Name, _Args), free, _).

:- pred bound_inst_list_gteq(list(bound_inst), list(bound_inst), module_info).
:- mode bound_inst_list_gteq(in, in, in) is semidet.

bound_inst_list_gteq([], _, _).
bound_inst_list_gteq([_|_], [], _) :-
	error("modecheck internal error: bound(...) missing case").
bound_inst_list_gteq([X|Xs], [Y|Ys], ModuleInfo) :-
	X = functor(NameX, ArgsX),
	Y = functor(NameY, ArgsY),
	length(ArgsX, ArityX),
	length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
		inst_list_gteq(ArgsX, ArgsY, ModuleInfo)
	;
		bound_inst_list_gteq([X|Xs], Ys, ModuleInfo)
	).

:- pred inst_list_gteq(list(inst), list(inst), module_info).
:- mode inst_list_gteq(in, in, in) is semidet.

inst_list_gteq([], [], _).
inst_list_gteq([X|Xs], [Y|Ys], ModuleInfo) :-
	inst_gteq(X, Y, ModuleInfo),
	inst_list_gteq(Xs, Ys, ModuleInfo).

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

:- pred report_mode_error_var_has_inst(modeinfo, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_var_has_inst(in, in, in, in, di, uo) is det.

report_mode_error_var_has_inst(ModeInfo, Var, VarInst, Inst) -->
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },
	modeinfo_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	io__write_variable(Var, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	mercury_output_inst(Inst, InstVarSet),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_var(modeinfo, var, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_unify_var_var(in, in, in, in, in, di, uo) is det.

report_mode_error_unify_var_var(ModeInfo, X, Y, InstX, InstY) -->
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },
	modeinfo_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	io__write_variable(X, VarSet),
	io__write_string("' and `"),
	io__write_variable(Y, VarSet),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	io__write_variable(X, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  variable `"),
	io__write_variable(Y, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(InstY, InstVarSet),
	io__write_string("'.\n").

:- pred report_mode_error_unify_var_functor(modeinfo, var, const, list(term),
					inst, list(inst), io__state, io__state).
:- mode report_mode_error_unify_var_functor(in, in, in, in, in, in, di, uo)
	is det.

report_mode_error_unify_var_functor(ModeInfo, X, Name, Args, InstX, ArgInsts)
		-->
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },
	{ term__context_init(0, Context) },
	{ Term = term_functor(Name, Args, Context) },
	modeinfo_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	io__write_variable(X, VarSet),
	io__write_string("' and `"),
	io__write_term(VarSet, Term),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	io__write_variable(X, VarSet),
	io__write_string("' has instantiatedness `"),
	mercury_output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  term `"),
	io__write_term(VarSet, Term),
	io__write_string("' has instantiatedness `"),
	io__write_constant(Name),
	( { Args \= [] } ->
		io__write_string("("),
		mercury_output_inst_list(ArgInsts, InstVarSet),
		io__write_string(")")
	;
		[]
	),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred modecheck_set_term_inst_list(list(term), list(inst),
					modeinfo, modeinfo).
:- mode modecheck_set_term_inst_list(in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_set_term_inst_list([], []) --> [].
modecheck_set_term_inst_list([Arg | Args], [Inst | Insts]) -->
	{ Arg = term_variable(Var) },
	modecheck_set_var_inst(Var, Inst),
	modecheck_set_term_inst_list(Args, Insts).

:- pred modecheck_set_var_inst_list(list(var), list(inst),
					modeinfo, modeinfo).
:- mode modecheck_set_var_inst_list(in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_set_var_inst_list([], []) --> [].
modecheck_set_var_inst_list([Var | Vars], [Inst | Insts]) -->
	modecheck_set_var_inst(Var, Inst),
	modecheck_set_var_inst_list(Vars, Insts).

:- pred modecheck_set_var_inst(var, inst, modeinfo, modeinfo).
:- mode modecheck_set_var_inst(in, in, modeinfo_di, modeinfo_uo) is det.

modecheck_set_var_inst(Var, Inst, ModeInfo0, ModeInfo) :-
	modeinfo_get_instmap(ModeInfo0, InstMap0),
	modeinfo_get_moduleinfo(ModeInfo0, ModuleInfo),
	instmap_lookup_var(InstMap0, Var, Inst0),
	( inst_is_compat(Inst0, Inst, ModuleInfo) ->
		ModeInfo = ModeInfo0
	; modeinfo_var_is_locked(ModeInfo0, Var) ->
		modeinfo_get_io_state(ModeInfo0, IOState0),
		report_mode_error_bind_var(ModeInfo0, Var, Inst0, Inst,
				IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_incr_errors(ModeInfo1, ModeInfo)
	;
		% XXX bind var
		map__set(InstMap0, Var, Inst, InstMap),
		modeinfo_set_instmap(InstMap, ModeInfo0, ModeInfo)
	).

:- pred inst_is_compat(inst, inst, module_info).
:- mode inst_is_compat(in, in, in) is semidet.

inst_is_compat(InstA, InstB, ModuleInfo) :-
	inst_expand(ModuleInfo, InstA, InstA2),
	inst_expand(ModuleInfo, InstB, InstB2),
	inst_is_compat_2(InstA2, InstB2, ModuleInfo).

:- pred inst_is_compat_2(inst, inst, module_info).
:- mode inst_is_compat_2(in, in, in) is semidet.

inst_is_compat_2(free, free, _).
inst_is_compat_2(bound(ListA), bound(ListB), ModuleInfo) :-
	bound_inst_list_is_compat(ListA, ListB, ModuleInfo).
inst_is_compat_2(ground, ground, _).
inst_is_compat_2(abstract_inst(NameA, ArgsA), abstract_inst(NameB, ArgsB),
		ModuleInfo) :-
	NameA = NameB,
	inst_is_compat_list(ArgsA, ArgsB, ModuleInfo).


:- pred inst_is_compat_list(list(inst), list(inst), module_info).
:- mode inst_is_compat_list(in, in, in) is semidet.

inst_is_compat_list([], [], _ModuleInfo).
inst_is_compat_list([ArgA | ArgsA], [ArgB | ArgsB], ModuleInfo) :-
	inst_is_compat(ArgA, ArgB, ModuleInfo),
	inst_is_compat_list(ArgsA, ArgsB, ModuleInfo).

:- pred bound_inst_list_is_compat(list(bound_inst), list(bound_inst),
			module_info).
:- mode bound_inst_list_is_compat(in, in, in) is semidet.

bound_inst_list_is_compat([], [], _).
bound_inst_list_is_compat([X|Xs], [Y|Ys], ModuleInfo) :-
	bound_inst_is_compat(X, Y, ModuleInfo),
	bound_inst_list_is_compat(Xs, Ys, ModuleInfo).

:- pred bound_inst_is_compat(bound_inst, bound_inst, module_info).
:- mode bound_inst_is_compat(in, in, in) is semidet.

bound_inst_is_compat(functor(Name, ArgsA), functor(Name, ArgsB), ModuleInfo) :-
	inst_is_compat_list(ArgsA, ArgsB, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred modeinfo_write_context(modeinfo, io__state, io__state).
:- mode modeinfo_write_context(modeinfo_no_io, di, uo).

modeinfo_write_context(ModeInfo) -->
	{ modeinfo_get_moduleinfo(ModeInfo, ModuleInfo) },
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_predid(ModeInfo, PredId) },
	{ modeinfo_get_procid(ModeInfo, ProcId) },
	{ moduleinfo_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ predinfo_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ procinfo_argmodes(ProcInfo, ArgModes) },
	{ predicate_name(PredId, PredName) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },

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
	{ modeinfo_get_mode_context(ModeInfo, ModeContext) },
	write_mode_context(ModeContext, Context).

:- pred write_mode_context(mode_context, term__context, io__state, io__state).
:- mode write_mode_context(in, in, di, uo).

write_mode_context(call(PredId, _ArgNum), Context) -->
	prog_out__write_context(Context),
	io__write_string("  in call to predicate `"),
	hlds_out__write_pred_id(PredId),
	io__write_string("':\n").

write_mode_context(unify(UnifyContext, _Side), Context) -->
	hlds_out__write_unify_context(UnifyContext, Context).

:- pred report_mode_error_bind_var(modeinfo, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_bind_var(in, in, in, in, di, uo).

report_mode_error_bind_var(ModeInfo, Var, VarInst, Inst) -->
	{ modeinfo_get_context(ModeInfo, Context) },
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },
	modeinfo_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string(
		"  mode error: attempt to bind variable inside a negation.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	io__write_variable(Var, VarSet),
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

	% used for debugging

:- pred mode_checkpoint(string, modeinfo, modeinfo).
:- mode mode_checkpoint(in, modeinfo_di, modeinfo_uo).

mode_checkpoint(Msg, ModeInfo0, ModeInfo) :-
	modeinfo_get_io_state(ModeInfo0, IOState0),
        lookup_option(debug, bool(DoCheckPoint), IOState0, IOState1),
	( DoCheckPoint = yes ->
		mode_checkpoint_2(Msg, ModeInfo0, IOState1, IOState)
	;
		IOState = IOState1
	),
	modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred mode_checkpoint_2(string, modeinfo, io__state, io__state).
:- mode mode_checkpoint_2(in, modeinfo_ui, di, uo).

mode_checkpoint_2(Msg, ModeInfo) -->
	io__write_string("At "),
	io__write_string(Msg),
	io__write_string(":\n"),
	lookup_option(statistics, bool(Statistics)),
	maybe_report_stats(Statistics),
	{ modeinfo_get_instmap(ModeInfo, InstMap) },
	{ map__to_assoc_list(InstMap, AssocList) },
	{ modeinfo_get_varset(ModeInfo, VarSet) },
	{ modeinfo_get_instvarset(ModeInfo, InstVarSet) },
	write_var_insts(AssocList, VarSet, InstVarSet),
	io__write_string("\n").

:- pred write_var_insts(assoc_list(var, inst), varset, varset,
			io__state, io__state).
:- mode write_var_insts(in, in, in, di, uo).

write_var_insts([], _, _) --> [].
write_var_insts([Var - Inst | VarInsts], VarSet, InstVarSet) -->
	io__write_string("\t"),
	io__write_variable(Var, VarSet),
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
				modeinfo, modeinfo).
:- mode modecheck_unification(in, in, out, out, modeinfo_di, modeinfo_uo).

modecheck_unification(term_variable(X), term_variable(Y), Modes, Unification,
			ModeInfo0, ModeInfo) :-
	modeinfo_get_moduleinfo(ModeInfo0, ModuleInfo),
	modeinfo_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	instmap_lookup_var(InstMap0, Y, InstY),
	( abstractly_unify_inst(InstX, InstY, ModuleInfo, Inst) ->
		modeinfo_get_instmap(ModeInfo0, InstMap0),
		% XXX bind var
		map__set(InstMap0, X, Inst, InstMap1),
		map__set(InstMap1, Y, Inst, InstMap),
		modeinfo_set_instmap(InstMap, ModeInfo0, ModeInfo)
	;
		modeinfo_get_io_state(ModeInfo0, IOState0),
		report_mode_error_unify_var_var(ModeInfo0, X, Y, InstX, InstY,
					IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_incr_errors(ModeInfo1, ModeInfo2),
			% If we get an error, set the inst to ground
			% to suppress follow-on errors
		Inst = ground,
		modeinfo_get_instmap(ModeInfo0, InstMap0),
		map__set(InstMap0, X, Inst, InstMap1),
		map__set(InstMap1, Y, Inst, InstMap),
		modeinfo_set_instmap(InstMap, ModeInfo2, ModeInfo)
	),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Modes = ModeX - ModeY,
	categorize_unify_var_var(ModeX, ModeY, X, Y, ModuleInfo, Unification).

modecheck_unification(term_variable(X), term_functor(Name, Args, _),
			Mode, Unification, ModeInfo0, ModeInfo) :-
	modeinfo_get_moduleinfo(ModeInfo0, ModuleInfo),
	modeinfo_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	instmap_lookup_arg_list(Args, InstMap0, InstArgs),
	InstY = bound([functor(Name, InstArgs)]),
	(
		% could just use abstractly_unify_inst(InstX, InstY, ...)
		% but this is a little bit faster
		abstractly_unify_inst_functor(InstX, Name, InstArgs, ModuleInfo,
			Inst)
	->
		modeinfo_get_instmap(ModeInfo0, InstMap0),
		map__set(InstMap0, X, Inst, InstMap1),
		bind_args(Inst, Args, InstMap1, InstMap),
		modeinfo_set_instmap(InstMap, ModeInfo0, ModeInfo)
	;
		modeinfo_get_io_state(ModeInfo0, IOState0),
		report_mode_error_unify_var_functor(ModeInfo0, X, Name, Args,
					InstX, InstArgs, IOState0, IOState),
		modeinfo_set_io_state(ModeInfo0, IOState, ModeInfo1),
		modeinfo_incr_errors(ModeInfo1, ModeInfo),
			% If we get an error, set the inst to ground
			% to suppress follow-on errors
		Inst = ground
	),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Mode = ModeX - ModeY,
	get_mode_of_args(Inst, InstArgs, ModeArgs),
	categorize_unify_var_functor(ModeX, ModeArgs, X, Name, Args,
			ModuleInfo, Unification).

modecheck_unification(term_functor(F, As, _), term_variable(Y),
		Modes, Unification, ModeInfo0, ModeInfo) :-
	modecheck_unification(term_variable(Y), term_functor(F, As, _),
		Modes, Unification, ModeInfo0, ModeInfo).
	
modecheck_unification(term_functor(_, _, _), term_functor(_, _, _),
		_, _, _, _) :-
	error("modecheck internal error: unification of term with term\n").

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(term), instmap, instmap).
:- mode bind_args(in, in, in, out) is det.

bind_args(ground, Args, InstMap0, InstMap) :-
	ground_args(Args, InstMap0, InstMap).
bind_args(bound(List), Args, InstMap0, InstMap) :-
	( List = [] ->
		% the code is unreachable - in an attempt to avoid spurious
		% mode errors, we ground the arguments
		ground_args(Args, InstMap0, InstMap)
	;
		List = [functor(_, InstList)],
		bind_args_2(Args, InstList, InstMap0, InstMap)
	).

:- pred bind_args_2(list(term), list(inst), instmap, instmap).
:- mode bind_args_2(in, in, in, out).

bind_args_2([], [], InstMap, InstMap).
bind_args_2([Arg | Args], [Inst | Insts], InstMap0, InstMap) :-
	Arg = term_variable(Var),
	map__set(InstMap0, Var, Inst, InstMap1),
	bind_args_2(Args, Insts, InstMap1, InstMap).

:- pred ground_args(list(term), instmap, instmap).
:- mode ground_args(in, in, out).

ground_args([], InstMap, InstMap).
ground_args([Arg | Args], InstMap0, InstMap) :-
	Arg = term_variable(Var),
	map__set(InstMap0, Var, ground, InstMap1),
	ground_args(Args, InstMap1, InstMap).

%-----------------------------------------------------------------------------%

:- pred get_mode_of_args(inst, list(inst), list(mode)).
:- mode get_mode_of_args(in, in, out) is det.

get_mode_of_args(ground, ArgInsts, ArgModes) :-
	mode_ground_args(ArgInsts, ArgModes).
get_mode_of_args(bound(List), ArgInstsA, ArgModes) :-
	( List = [] ->
		% the code is unreachable, so in an attempt to
		% avoid spurious mode errors we assume that the
		% args are ground
		mode_ground_args(ArgInstsA, ArgModes)
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

:- pred mode_ground_args(list(inst), list(mode)).
:- mode mode_ground_args(in, out).

mode_ground_args([], []).
mode_ground_args([Inst | Insts], [Mode | Modes]) :-
	Mode = (Inst -> ground),
	mode_ground_args(Insts, Modes).

%-----------------------------------------------------------------------------%

	% Mode checking is like abstract interpretation.
	% This is the abstract unification operation which
	% unifies two instantiatednesses.  If the unification
	% would be illegal, then abstract unification fails.
	% If the unification would fail, then the abstract unification
	% will succeed, and the resulting instantiatedness will be
	% something like bound([]), which effectively means "this program
	% point will never be reached".

:- pred abstractly_unify_inst_list(list(inst), list(inst), module_info,
					list(inst)).
:- mode abstractly_unify_inst_list(in, in, in, out).

abstractly_unify_inst_list([], [], _, []).
abstractly_unify_inst_list([X|Xs], [Y|Ys], ModuleInfo, [Z|Zs]) :-
	abstractly_unify_inst(X, Y, ModuleInfo, Z),
	abstractly_unify_inst_list(Xs, Ys, ModuleInfo, Zs).

:- pred abstractly_unify_inst(inst, inst, module_info, inst).
:- mode abstractly_unify_inst(in, in, in, out) is semidet.

abstractly_unify_inst(InstA, InstB, ModuleInfo, Inst) :-
	inst_expand(ModuleInfo, InstA, InstA2),
	inst_expand(ModuleInfo, InstB, InstB2),
	abstractly_unify_inst_2(InstA2, InstB2, ModuleInfo, Inst).

:- pred abstractly_unify_inst_2(inst, inst, module_info, inst).
:- mode abstractly_unify_inst_2(in, in, in, out) is semidet.

abstractly_unify_inst_2(free,		free,		_, _) :- fail.
abstractly_unify_inst_2(free,		bound(List),	_, bound(List)). % XXX
abstractly_unify_inst_2(free,		ground,		_, ground).
abstractly_unify_inst_2(free,		abstract_inst(_,_), _, _) :- fail.
	
abstractly_unify_inst_2(bound(List),	free,		_, bound(List)). % XXX
abstractly_unify_inst_2(bound(ListX),	bound(ListY),	M, bound(List)) :-
	abstractly_unify_bound_inst_list(ListX, ListY, M, List).
abstractly_unify_inst_2(bound(_),	ground,		_, ground).
abstractly_unify_inst_2(bound(List),	abstract_inst(_,_), ModuleInfo,
							   ground) :-
	bound_inst_list_is_ground(List, ModuleInfo).

abstractly_unify_inst_2(ground,		_,		_, ground).

abstractly_unify_inst_2(abstract_inst(_,_), free,	_, _) :- fail.
abstractly_unify_inst_2(abstract_inst(_,_), bound(List), ModuleInfo, ground) :-
	bound_inst_list_is_ground(List, ModuleInfo).
abstractly_unify_inst_2(abstract_inst(_,_), ground,	_, ground).
abstractly_unify_inst_2(abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo, 
			abstract_inst(Name, Args)) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, ModuleInfo, Args).

%-----------------------------------------------------------------------------%

	% This is the abstract unification operation which
	% unifies a variable (or rather, it's instantiatedness)
	% with a functor.  We could just set the instantiatedness
	% of the functor to be `bound([functor(Name, Args)])', and then
	% call abstractly_unify_inst, but the following specialized code
	% is slightly more efficient.

:- pred abstractly_unify_inst_functor(inst, const, list(inst),
					module_info, inst).
:- mode abstractly_unify_inst_functor(in, in, in, in, out) is semidet.

abstractly_unify_inst_functor(InstA, Name, ArgInsts, ModuleInfo, Inst) :-
	inst_expand(ModuleInfo, InstA, InstA2),
	abstractly_unify_inst_functor_2(InstA2, Name, ArgInsts, ModuleInfo,
		Inst).

:- pred abstractly_unify_inst_functor_2(inst, const, list(inst),
					module_info, inst).
:- mode abstractly_unify_inst_functor_2(in, in, in, in, out) is semidet.

abstractly_unify_inst_functor_2(free, Name, Args, _,
			bound([functor(Name, Args)])).
abstractly_unify_inst_functor_2(bound(ListX), Name, Args, M, bound(List)) :-
	ListY = [functor(Name, Args)],
	abstractly_unify_bound_inst_list(ListX, ListY, M, List).
abstractly_unify_inst_functor_2(ground, _Name, _Args, _, ground).
abstractly_unify_inst_functor_2(abstract_inst(_,_), _Name, _Args, _, _) :- fail.

%-----------------------------------------------------------------------------%

	% This code performs abstract unification of two bound(...) insts.
	% like a sorted merge operation.  If two elements have the
	% The lists of bound_inst are guaranteed to be sorted.
	% Abstract unification of two bound(...) insts proceeds
	% like a sorted merge operation.  If two elements have the
	% same functor name, they are inserted in the output list
	% iff their argument inst list can be abstractly unified.

:- pred abstractly_unify_bound_inst_list(list(bound_inst), list(bound_inst),
		module_info, list(bound_inst)).
:- mode abstractly_unify_bound_inst_list(in, in, in, out).

:- abstractly_unify_bound_inst_list(Xs, Ys, _, _) when Xs and Ys. % Index

abstractly_unify_bound_inst_list([], _, _ModuleInfo, []).
abstractly_unify_bound_inst_list([_|_], [], _ModuleInfo, []).
abstractly_unify_bound_inst_list([X|Xs], [Y|Ys], ModuleInfo, L) :-
	X = functor(NameX, ArgsX),
	length(ArgsX, ArityX),
	Y = functor(NameY, ArgsY),
	length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
	    ( abstractly_unify_inst_list(ArgsX, ArgsY, ModuleInfo, Args) ->
		L = [functor(NameX, Args) | L1],
		abstractly_unify_bound_inst_list(Xs, Ys, ModuleInfo, L1)
	    ;
		abstractly_unify_bound_inst_list(Xs, Ys, ModuleInfo, L)
	    )
	;
	    ( compare(<, X, Y) ->
		abstractly_unify_bound_inst_list(Xs, [Y|Ys], ModuleInfo, L)
	    ;
		abstractly_unify_bound_inst_list([X|Xs], Ys, ModuleInfo, L)
	    )
	).

%-----------------------------------------------------------------------------%

:- pred categorize_unify_var_var(mode, mode, var, var, module_info,
				unification).
:- mode categorize_unify_var_var(in, in, in, in, in, out).

categorize_unify_var_var(ModeX, ModeY, X, Y, ModuleInfo, Unification) :-
	( mode_is_output(ModuleInfo, ModeX) ->
		Unification = assign(X, Y)
	; mode_is_output(ModuleInfo, ModeY) ->
		Unification = assign(Y, X)
	;
		% XXX we should distinguish `simple_test's from
		% `complicated_unify's!!!
		% Currently we just assume that they're all `simple_test's.
		Unification = simple_test(X, Y)
/******
	;
		Unification = complicated_unify(ModeX - ModeY,
				term_variable(X), term_variable(Y))
*******/
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
	{ moduleinfo_insts(Module, InstDefns) },
	{ map__keys(InstDefns, InstIds) },
	find_undef_inst_bodies(InstIds, InstDefns),
	{ moduleinfo_modes(Module, ModeDefns) },
	{ map__keys(ModeDefns, ModeIds) },
	find_undef_mode_bodies(ModeIds, ModeDefns, InstDefns),
	{ moduleinfo_preds(Module, Preds) },
	{ moduleinfo_predids(Module, PredIds) },
	find_undef_pred_modes(PredIds, Preds, ModeDefns, InstDefns).

	% Find any undefined insts/modes used in predicate mode declarations.

:- pred find_undef_pred_modes(list(pred_id), pred_table, mode_table,
				inst_table, io__state, io__state).
:- mode find_undef_pred_modes(in, in, in, in, di, uo).

find_undef_pred_modes([], _Preds, _ModeDefns, _InstDefns) --> [].
find_undef_pred_modes([PredId | PredIds], Preds, ModeDefns, InstDefns) -->
	{ map__search(Preds, PredId, PredDefn) },
	{ predinfo_procedures(PredDefn, Procs) },
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
	{ procinfo_argmodes(ProcDefn, ArgModes) },
	{ procinfo_context(ProcDefn, Context) },
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

	% The modeinfo data structure and access predicates.

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

:- type instmap == map(var, inst).

:- type modeinfo 
	--->	modeinfo(
			io__state,
			module_info,
			pred_id,	% The pred we are checking
			proc_id,	% The mode which we are checking
			term__context,	% The line number of the subgoal we
					% are currently checking
			mode_context,	% A description of where in the
					% goal the error occurred
			map(var, inst),	% The current instantiatedness
					% of the variables
			list(set(var)),	% The "locked" variables,
					% i.e. variables which cannot be
					% further instantiated inside a
					% negated context
			delay_info,	% info about delayed goals
			int		% The number of mode errors found
		).

	% Reordering of conjunctions is done
	% by simulating coroutining at compile time.
	% This is handled by the following data structure.

:- type delay_info
	--->	delay_info(
			depth_num,	% CurrentDepth:
					% the current conjunction depth,
					% i.e. the number of nested conjunctions
					% which are currently active
			list(map(seq_num, delayed_goal)),
					% DelayedGoalStack:
					% for each nested conjunction,
					% we store a collection of delayed goals
					% associated with that conjunction,
					% indexed by sequence number
			map(var, assoc_list(depth_num, list(seq_num))),
					% WaitingGoals:
					% for each variable, we keep track of
					% all the goals which are waiting on
					% that variable
			map(depth_num, list(seq_num)),
					% PendingGoals:
					% when a variable gets bound, we
					% mark all the goals which are waiting
					% on that variable as ready to be
					% reawakened at the next opportunity
			map(depth_num, seq_num)
					% NextSeqNums:
					% For each nested conjunction, the
					% next available sequence number.
		).

:- type depth_num == int.
:- type seq_num == int.
:- type delayed_goal == hlds__goal.	% XXX problem
% :- type delayed_goal ---> delayed_goal(hlds__goal, ...).

	% The normal inst of a modeinfo struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

:- inst uniq_modeinfo	=	bound_unique(
					modeinfo(
						ground_unique, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground
					)
				).

:- mode modeinfo_uo :: free -> uniq_modeinfo.
:- mode modeinfo_ui :: uniq_modeinfo -> uniq_modeinfo.
:- mode modeinfo_di :: uniq_modeinfo -> dead.

	% Some fiddly modes used when we want to extract
	% the io_state from a modeinfo struct and then put it back again.

:- inst modeinfo_no_io	=	bound_unique(
					modeinfo(
						dead, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground
					)
				).

:- mode modeinfo_get_io_state	:: uniq_modeinfo -> modeinfo_no_io.
:- mode modeinfo_no_io		:: modeinfo_no_io -> modeinfo_no_io.
:- mode modeinfo_set_io_state	:: modeinfo_no_io -> dead.

%-----------------------------------------------------------------------------%

	% Initialize the modeinfo

:- pred modeinfo_init(io__state, module_info, pred_id, proc_id,
			term__context, instmap, modeinfo).
:- mode modeinfo_init(di, in, in, in, in, in, modeinfo_uo) is det.

modeinfo_init(IOState, ModuleInfo, PredId, ProcId, Context, InstMapping0,
		ModeInfo) :-
	mode_context_init(ModeContext),
	LockedVars = [],
	delay_info_init(DelayInfo),
	ModeInfo = modeinfo(
		IOState, ModuleInfo, PredId, ProcId, Context, ModeContext,
		InstMapping0, LockedVars, DelayInfo, 0
	).

%-----------------------------------------------------------------------------%

:- pred mode_context_init(mode_context).
:- mode mode_context_init(in) is det.

mode_context_init(uninitialized).

%-----------------------------------------------------------------------------%

	% Lots of very boring access predicates.

:- pred modeinfo_get_io_state(modeinfo, io__state).
:- mode modeinfo_get_io_state(modeinfo_get_io_state, uo) is det.

modeinfo_get_io_state(modeinfo(IOState,_,_,_,_,_,_,_,_,_), IOState).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_io_state(modeinfo, io__state, modeinfo).
:- mode modeinfo_set_io_state(modeinfo_set_io_state, ui, modeinfo_uo) is det.

modeinfo_set_io_state( modeinfo(_,B,C,D,E,F,G,H,I,J), IOState,
			modeinfo(IOState,B,C,D,E,F,G,H,I,J)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_moduleinfo(modeinfo, module_info).
:- mode modeinfo_get_moduleinfo(in, out) is det.

modeinfo_get_moduleinfo(modeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_), ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_preds(modeinfo, pred_table).
:- mode modeinfo_get_preds(in, out) is det.

modeinfo_get_preds(modeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_), Preds) :-
	moduleinfo_preds(ModuleInfo, Preds).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_modes(modeinfo, mode_table).
:- mode modeinfo_get_modes(in, out) is det.

modeinfo_get_modes(modeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_), Modes) :-
	moduleinfo_modes(ModuleInfo, Modes).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_insts(modeinfo, inst_table).
:- mode modeinfo_get_insts(in, out) is det.

modeinfo_get_insts(modeinfo(_,ModuleInfo,_,_,_,_,_,_,_,_), Insts) :-
	moduleinfo_insts(ModuleInfo, Insts).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_predid(modeinfo, pred_id).
:- mode modeinfo_get_predid(in, out) is det.

modeinfo_get_predid(modeinfo(_,_,PredId,_,_,_,_,_,_,_), PredId).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_procid(modeinfo, proc_id).
:- mode modeinfo_get_procid(in, out) is det.

modeinfo_get_procid(modeinfo(_,_,_,ProcId,_,_,_,_,_,_), ProcId).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_context(modeinfo, term__context).
:- mode modeinfo_get_context(in, out).

modeinfo_get_context(modeinfo(_,_,_,_,Context,_,_,_,_,_), Context).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_mode_context(modeinfo, mode_context).
:- mode modeinfo_get_mode_context(in, out) is det.

modeinfo_get_mode_context(modeinfo(_,_,_,_,_,ModeContext,_,_,_,_),
				ModeContext).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_mode_context(mode_context, modeinfo, modeinfo).
:- mode modeinfo_set_mode_context(in, modeinfo_di, modeinfo_uo) is det.

modeinfo_set_mode_context(ModeContext, modeinfo(A,B,C,D,E,_,G,H,I,J),
				modeinfo(A,B,C,D,E,ModeContext,G,H,I,J)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_call_context(call_context, modeinfo, modeinfo).
:- mode modeinfo_set_call_context(in, in, out) is det.

modeinfo_set_call_context(unify(UnifyContext)) -->
	modeinfo_set_mode_context(unify(UnifyContext, left)).
modeinfo_set_call_context(call(PredId)) -->
	modeinfo_set_mode_context(call(PredId, 0)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_instmap(modeinfo, instmap).
:- mode modeinfo_get_instmap(in, out) is det.

modeinfo_get_instmap(modeinfo(_,_,_,_,_,_,InstMap,_,_,_), InstMap).

	% modeinfo_dcg_get_instmap/3 is the same as modeinfo_get_instmap/2
	% except that it's easier to use inside a DCG.

:- pred modeinfo_dcg_get_instmap(instmap, modeinfo, modeinfo).
:- mode modeinfo_dcg_get_instmap(out, modeinfo_di, modeinfo_uo) is det.

modeinfo_dcg_get_instmap(InstMap, ModeInfo, ModeInfo) :-
	modeinfo_get_instmap(ModeInfo, InstMap).

	% modeinfo_get_vars_instmap/3 is the same as modeinfo_get_instmap/2
	% except that the map it returns might only contain the specified
	% variables if that would be more efficient; currently it's not,
	% so the two are just the same, but if we were to change the
	% data structures...

:- pred modeinfo_get_vars_instmap(modeinfo, set(var), instmap).
:- mode modeinfo_get_vars_instmap(in, in, out) is det.

modeinfo_get_vars_instmap(ModeInfo, _Vars, InstMap) :-
	modeinfo_get_instmap(ModeInfo, InstMap).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_instmap(instmap, modeinfo, modeinfo).
:- mode modeinfo_set_instmap(in, modeinfo_di, modeinfo_uo) is det.

modeinfo_set_instmap( InstMap, modeinfo(A,B,C,D,E,F,_,H,I,J),
			modeinfo(A,B,C,D,E,F,InstMap,H,I,J)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_locked_vars(modeinfo, list(set(var))).
:- mode modeinfo_get_locked_vars(modeinfo_ui, out) is det.

modeinfo_get_locked_vars(modeinfo(_,_,_,_,_,_,_,LockedVars,_,_), LockedVars).

%-----------------------------------------------------------------------------%

:- pred modeinfo_set_locked_vars(modeinfo, list(set(var)), modeinfo).
:- mode modeinfo_set_locked_vars(modeinfo_di, in, modeinfo_uo) is det.

modeinfo_set_locked_vars( modeinfo(A,B,C,D,E,F,G,_,I,J), LockedVars,
			modeinfo(A,B,C,D,E,F,G,LockedVars,I,J)).

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_num_errors(modeinfo, int).
:- mode modeinfo_get_num_errors(modeinfo_ui, out) is det.

modeinfo_get_num_errors(modeinfo(_,_,_,_,_,_,_,_,_,NumErrors), NumErrors).

%-----------------------------------------------------------------------------%

:- pred modeinfo_incr_errors(modeinfo, modeinfo).
:- mode modeinfo_incr_errors(modeinfo_di, modeinfo_uo) is det.

modeinfo_incr_errors( modeinfo(A,B,C,D,E,F,G,H,I,NumErrors0),
			modeinfo(A,B,C,D,E,F,G,H,I,NumErrors)) :-
	NumErrors is NumErrors0 + 1.

%-----------------------------------------------------------------------------%

:- pred modeinfo_get_varset(modeinfo, varset).
:- mode modeinfo_get_varset(modeinfo_ui, out) is det.

	% perhaps we should store it directly in the modeinfo?

modeinfo_get_varset(ModeInfo, VarSet) :-
	modeinfo_get_moduleinfo(ModeInfo, ModuleInfo),
	modeinfo_get_predid(ModeInfo, PredId),
	moduleinfo_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	predinfo_procedures(PredInfo, Procs),
	modeinfo_get_procid(ModeInfo, ProcId),
	map__lookup(Procs, ProcId, ProcInfo),
	procinfo_variables(ProcInfo, VarSet).

:- pred modeinfo_get_instvarset(modeinfo, varset).
:- mode modeinfo_get_instvarset(modeinfo_ui, out) is det.

	% Since we don't yet handle polymorphic modes, the inst varset
	% is always empty.

modeinfo_get_instvarset(_ModeInfo, InstVarSet) :-
	varset__init(InstVarSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The locked variables are stored as a stack 
	% of sets of variables.  A variable is locked if it is
	% a member of any of the sets.  To lock a set of vars, we just
	% push them on the stack, and to unlock a set of vars, we just
	% pop them off the stack.  The stack is implemented as a list.

:- pred modeinfo_lock_vars(set(var), modeinfo, modeinfo).
:- mode modeinfo_lock_vars(in, modeinfo_di, modeinfo_uo) is det.

modeinfo_lock_vars(Vars, ModeInfo0, ModeInfo) :-
	modeinfo_get_locked_vars(ModeInfo0, LockedVars),
	modeinfo_set_locked_vars(ModeInfo0, [Vars | LockedVars], ModeInfo).

:- pred modeinfo_unlock_vars(set(var), modeinfo, modeinfo).
:- mode modeinfo_unlock_vars(in, modeinfo_di, modeinfo_uo) is det.

modeinfo_unlock_vars(_, ModeInfo0, ModeInfo) :-
	modeinfo_get_locked_vars(ModeInfo0, [_ | LockedVars]),
	modeinfo_set_locked_vars(ModeInfo0, LockedVars, ModeInfo).

:- pred modeinfo_var_is_locked(modeinfo, var).
:- mode modeinfo_var_is_locked(modeinfo_ui, in) is semidet.

modeinfo_var_is_locked(ModeInfo, Var) :-
	modeinfo_get_locked_vars(ModeInfo, LockedVarsList),
	modeinfo_var_is_locked_2(LockedVarsList, Var).

:- pred modeinfo_var_is_locked_2(list(set(var)), var).
:- mode modeinfo_var_is_locked_2(in, in) is semidet.

modeinfo_var_is_locked_2([Set | Sets], Var) :-
	(
		set__member(Var, Set)
	->
		true
	;
		modeinfo_var_is_locked_2(Sets, Var)
	).

:- pred modeinfo_get_delay_info(modeinfo, delay_info).
:- mode modeinfo_get_delay_info(in, out) is det.

modeinfo_get_delay_info(modeinfo(_,_,_,_,_,_,_,_,DelayInfo,_), DelayInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% modeinfo_wakeup_goal(DelayInfo0, Goal, DelayInfo) is true iff
	% DelayInfo0 specifies that there is at least one goal which is
	% pending, Goal is the pending goal which should be reawakened first,
	% and DelayInfo is the new delay_info, updated to reflect the fact
	% that Goal has been woken up and is hence no longer pending.

:- pred delay_info_wakeup_goal(delay_info, hlds__goal, delay_info).
:- mode delay_info_wakeup_goal(in, out, out) is semidet.

delay_info_wakeup_goal(DelayInfo0, Goal, DelayInfo) :-
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack0, WaitingGoals,
				PendingGoalsTable0, NextSeqNums),
	map__search(PendingGoalsTable0, CurrentDepth, PendingGoals0),
	PendingGoals0 = [SeqNum | PendingGoals],
	map__set(PendingGoalsTable0, CurrentDepth, PendingGoals,
			PendingGoalsTable),
	DelayedGoalStack0 = [DelayedGoals0|StackRest],
	map__lookup(DelayedGoals0, SeqNum, Goal),
	map__delete(DelayedGoals0, SeqNum, DelayedGoals),
	DelayedGoalStack = [DelayedGoals | StackRest],
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack, WaitingGoals,
				PendingGoalsTable, NextSeqNums).

:- pred delay_info_bind_var(delay_info, var, delay_info).
:- mode delay_info_bind_var(in, in, out) is det.

delay_info_bind_var(DelayInfo0, Var, DelayInfo) :-
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable0, PendingGoals0, NextSeqNums),
	map__search(WaitingGoalsTable0, Var, GoalsWaitingOnVar),
	add_pending_goals(GoalsWaitingOnVar, PendingGoals0, PendingGoals),
	map__delete(WaitingGoalsTable0, Var, WaitingGoalsTable),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums).
	
:- pred add_pending_goals(assoc_list(depth_num, list(seq_num)),
			map(depth_num, list(seq_num)),
			map(depth_num, list(seq_num))).
:- mode add_pending_goals(in, in, out) is det.

add_pending_goals([], PendingGoals, PendingGoals).
add_pending_goals([Depth - SeqNums | Rest], PendingGoals0, PendingGoals) :-
	( map__search(PendingGoals0, Depth, PendingSeqNums0) ->
		append(PendingSeqNums0, SeqNums, PendingSeqNums)
	;
		PendingSeqNums = SeqNums
	),
	map__set(PendingGoals0, Depth, PendingSeqNums, PendingGoals1),
	add_pending_goals(Rest, PendingGoals1, PendingGoals).

:- pred delay_info_init(delay_info).
:- mode delay_info_init(out) is det.

delay_info_init(DelayInfo) :-
	CurrentDepth = 0,
	DelayedGoalStack = [],
	map__init(WaitingGoalsTable),
	map__init(PendingGoals),
	map__init(NextSeqNums),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
