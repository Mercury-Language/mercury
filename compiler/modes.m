%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: modes.nl.
% Main author: fjh.
%
% This file contains a mode-checker.
% Still somewhat incomplete.

% XXX we need to allow unification of free with free even when both
%     *variables* are live, if one of the particular *sub-nodes* is 
%     dead (causes problems handling e.g. `same_length').
% XXX break unifications into "micro-unifications"
% XXX would even the above fixes be enough?

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
		two free variables (or in general two free sub-terms)
		unless one of them is dead. (Also split unifications
		up if necessary to avoid complicated sub-unifications.)
	(e) a predicate call
		Check that there is a mode declaration for the
		predicate which matches the current instantiation of
		the arguments.  (Also handle calls to implied modes.)
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

:- import_module undef_modes, mode_info, delay_info, mode_errors, inst_match.
:- import_module list, map, varset, term, prog_out, string, require, std_util.
:- import_module type_util, mode_util, code_util, prog_io, unify_proc.
:- import_module globals, options, mercury_to_mercury, hlds_out, int, set.

%-----------------------------------------------------------------------------%

modecheck(Module0, Module) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
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

check_pred_modes(ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	modecheck_pred_modes_2(PredIds, ModuleInfo0, ModuleInfo1),
	modecheck_unify_procs(ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred modecheck_pred_modes_2(list(pred_id), module_info, 
			module_info, io__state, io__state).
:- mode modecheck_pred_modes_2(in, in, out, di, uo) is det.

modecheck_pred_modes_2([], ModuleInfo, ModuleInfo) --> [].
modecheck_pred_modes_2([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	( { pred_info_is_imported(PredInfo0) } ->
		{ ModuleInfo3 = ModuleInfo0 }
	;
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
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

:- pred copy_clauses_to_proc(pred_info, proc_id, pred_info).
:- mode copy_clauses_to_proc(in, in, out) is det.

copy_clauses_to_proc(PredInfo0, ProcId, PredInfo) :-
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	pred_info_procedures(PredInfo0, Procs0),
	copy_clauses_to_procs_2([ProcId], ClausesInfo, Procs0, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

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
	map__lookup(Procs0, ProcId, Proc0),
	ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, Clauses),
	select_matching_clauses(Clauses, ProcId, MatchingClauses),
	get_clause_goals(MatchingClauses, GoalList),
	( GoalList = [SingleGoal] ->
		Goal = SingleGoal
	;
		% Construct a goal_info for the disjunction.
		% We use the context of the first clause, unless
		% there weren't any clauses at all, in which case
		% we use the context of the mode declaration.
		% The non-local vars are just the head variables.
		goal_info_init(GoalInfo0),
		( GoalList = [FirstGoal | _] ->
			FirstGoal = _ - FirstGoalInfo,
			goal_info_context(FirstGoalInfo, Context)
		;
			proc_info_context(Proc0, Context)
		),
		goal_info_set_context(GoalInfo0, Context, GoalInfo1),
		set__list_to_set(HeadVars, NonLocalVars),
		goal_info_set_nonlocals(GoalInfo1, NonLocalVars, GoalInfo),
		Goal = disj(GoalList) - GoalInfo
	),
	proc_info_set_body(Proc0, VarSet, VarTypes, HeadVars, Goal, Proc),
	map__set(Procs0, ProcId, Proc, Procs1),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs1, Procs).

:- pred select_matching_clauses(list(clause), proc_id, list(clause)).
:- mode select_matching_clauses(in, in, out) is det.

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
	Clause = clause(ProcIds, _, _),
	( ProcIds = [] ->
		MatchingClauses = [Clause | MatchingClauses1]
	; list__member(ProcId, ProcIds) ->
		MatchingClauses = [Clause | MatchingClauses1]
	;
		MatchingClauses = MatchingClauses1
	),
	select_matching_clauses(Clauses, ProcId, MatchingClauses1).

:- pred get_clause_goals(list(clause)::in, list(hlds__goal)::out) is det.

get_clause_goals([], []).
get_clause_goals([Clause | Clauses], Goals) :-
	Clause = clause(_, Goal, _),
	goal_to_disj_list(Goal, GoalList),
	list__append(GoalList, Goals1, Goals),
	get_clause_goals(Clauses, Goals1).

%-----------------------------------------------------------------------------%

:- pred modecheck_procs(pred_id, module_info, pred_info, pred_info, int,
			io__state, io__state).
:- mode modecheck_procs(in, in, in, out, out, di, uo) is det.

modecheck_procs(PredId, ModuleInfo, PredInfo0, PredInfo, NumErrors) -->
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ map__keys(Procs0, ProcIds) },
	( { ProcIds = [] } ->
		report_warning_no_modes(PredId, PredInfo0, ModuleInfo),
		{ PredInfo = PredInfo0 },
		{ NumErrors = 0 }
	;
		modecheck_procs_2(ProcIds, PredId, ModuleInfo, Procs0, 0,
					Procs, NumErrors),
		{ pred_info_set_procedures(PredInfo0, Procs, PredInfo) }
	).

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
	proc_info_argmodes(ProcInfo0, ArgModes0),
	proc_info_headvars(ProcInfo0, HeadVars),

		% We use the context of the first clause, unless
		% there weren't any clauses at all, in which case
		% we use the context of the mode declaration.
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	ClausesInfo = clauses_info(_, _, _, ClauseList),
	( ClauseList = [FirstClause | _] ->
		FirstClause = clause(_, _, Context)
	;
		proc_info_context(ProcInfo0, Context)
	),
/**************
		% extract the predicate's type from the pred_info
		% and propagate the type information into the modes
	pred_info_arg_types(PredInfo, _TypeVars, ArgTypes),
	propagate_type_info_mode_list(ArgTypes, ModuleInfo, ArgModes0,
			ArgModes),
**************/
	ArgModes = ArgModes0,
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
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	proc_info_set_goal(ProcInfo1, Body, ProcInfo2),
	proc_info_set_variables(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo).

:- pred modecheck_final_insts(list(var), list(mode), mode_info, mode_info).
:- mode modecheck_final_insts(in, in, mode_info_di, mode_info_uo) is det.

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

check_final_insts([], [_|_], _, _, _) -->
	{ error("check_final_insts: length mismatch") }.
check_final_insts([_|_], [], _, _, _) -->
	{ error("check_final_insts: length mismatch") }.
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
	modecheck_goal_2(Goal0, NonLocals, Goal, ModeInfo0, ModeInfo),
		%
		% save the changes in instantiation of the non-local vars
		%
	mode_info_get_vars_instmap(ModeInfo, NonLocals, InstMap),
	compute_instmap_delta(InstMap0, InstMap, NonLocals, DeltaInstMap),
	goal_info_set_instmap_delta(GoalInfo0, DeltaInstMap, GoalInfo).

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

modecheck_goal_2(not(A0), NonLocals, not(A)) -->
	mode_checkpoint(enter, "not"),
	mode_info_lock_vars(NonLocals),
	modecheck_goal(A0, A),
	mode_info_unlock_vars(NonLocals),
	mode_checkpoint(exit, "not").

modecheck_goal_2(some(Vs, G0), _, some(Vs, G)) -->
	mode_checkpoint(enter, "some"),
	modecheck_goal(G0, G),
	mode_checkpoint(exit, "some").

modecheck_goal_2(call(PredId, _, Args0, _, PredName, Follow), NonLocals, Goal)
		-->
	{ list__length(Args0, Arity) },
	mode_info_set_call_context(call(PredName/Arity)),
	=(ModeInfo0),
	modecheck_call_pred(PredId, Args0, Mode, Args, ExtraGoals),
	=(ModeInfo),
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ code_util__is_builtin(ModuleInfo, PredId, Mode, Builtin) },
	{ Call = call(PredId, Mode, Args, Builtin, PredName, Follow) },
	{ handle_extra_goals(Call, ExtraGoals, NonLocals, Args0, Args,
				ModeInfo0, ModeInfo, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

modecheck_goal_2(unify(A0, B0, _, _, UnifyContext), NonLocals, Goal) -->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),
	=(ModeInfo0),
	modecheck_unification(A0, B0, A, B, ExtraGoals, Mode, UnifyInfo),
	=(ModeInfo),
	{ Unify = unify(A, B, Mode, UnifyInfo, UnifyContext) },
	{ handle_extra_goals(Unify, ExtraGoals, NonLocals, [A0, B0], [A, B],
				ModeInfo0, ModeInfo, Goal) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

modecheck_goal_2(switch(_, _, _), _, _) -->
	{ error("modecheck_goal_2: unexpected switch") }.

	% handle_extra_goals combines MainGoal and ExtraGoals into a single
	% hlds__goal_expr.

:- pred handle_extra_goals(hlds__goal_expr, pair(list(hlds__goal)), set(var),
			list(term), list(term), mode_info, mode_info,
			hlds__goal_expr).
:- mode handle_extra_goals(in, in, in, in, in, mode_info_ui, mode_info_ui, out)
	is det.

handle_extra_goals(MainGoal, ExtraGoals, NonLocals0, Args0, Args,
		ModeInfo0, ModeInfo, Goal) :-
	% did we introduced any extra variables (and code)?
	( ExtraGoals = [] - [] ->
		Goal = MainGoal	% no
	;
		% recompute the new set of non-local variables for the main goal
		term__vars_list(Args0, OldArgList),
		term__vars_list(Args, NewArgList),
		set__list_to_set(OldArgList, OldArgVars),
		set__list_to_set(NewArgList, NewArgVars),
		set__difference(NewArgVars, OldArgVars, IntroducedVars),
		set__union(NonLocals0, IntroducedVars, OutsideVars),
		set__intersect(NewArgVars, OutsideVars, NonLocals),
		goal_info_init(GoalInfo0),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		% compute the instmap delta for the main goal
		mode_info_get_vars_instmap(ModeInfo0, NewArgVars, InstMap0),
		mode_info_get_vars_instmap(ModeInfo, NewArgVars, InstMap),
		compute_instmap_delta(InstMap0, InstMap, NonLocals,
			DeltaInstMap),
		goal_info_set_instmap_delta(GoalInfo1, DeltaInstMap, GoalInfo),

		% combine the main goal and the extra goals into a conjunction
		Goal0 = MainGoal - GoalInfo ,
		ExtraGoals = BeforeGoals - AfterGoals ,
		list__append(BeforeGoals, [Goal0 | AfterGoals], GoalList),
		Goal = conj(GoalList)
	).

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
	mode_info_dcg_get_instmap(InstMap),
	( { InstMap = unreachable } ->
		{ Goals2  = [] }
	;
		modecheck_conj_list_2(Goals1, Goals2)
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
		( ErrorList = [FirstError | _] ->
			FirstError = Var - _,
			set__singleton_set(WaitingVars, Var),
			mode_info_error(WaitingVars,
				mode_error_disj(MergeContext, ErrorList),
				ModeInfo1, ModeInfo2
			)
		;
			ModeInfo2 = ModeInfo1
		),
		InstMap = reachable(InstMapping)
	;
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0
	),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo).

:- pred get_reachable_instmaps(list(instmap), list(map(var,inst))).
:- mode get_reachable_instmaps(in, out) is det.

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

:- pred modecheck_call_pred(pred_id, list(term), proc_id, list(term),
				pair(list(hlds__goal)), mode_info, mode_info).
:- mode modecheck_call_pred(in, in, out, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_call_pred(PredId, Args0, TheProcId, Args, ExtraGoals,
		ModeInfo0, ModeInfo) :-
	term__term_list_to_var_list(Args0, ArgVars0),

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
		proc_info_argmodes(ProcInfo, ProcArgModes0),
/*********************
		% propagate type info into modes
		mode_info_get_types_of_vars(ModeInfo0, ArgVars0, ArgTypes),
		propagate_type_info_mode_list(ArgTypes, ModuleInfo,
			ProcArgModes0, ProcArgModes),
*********************/
		ProcArgModes = ProcArgModes0,
		mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
					InitialInsts),
		modecheck_var_has_inst_list(ArgVars0, InitialInsts,
					ModeInfo0, ModeInfo1),
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
			ArgVars, ExtraGoals, ModeInfo1, ModeInfo2),
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
		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars0,
			WaitingVars, TheProcId, ArgVars, ExtraGoals,
			ModeInfo1, ModeInfo2),

			% restore the error list, appending any new error(s)
		mode_info_get_errors(ModeInfo2, NewErrors),
		list__append(OldErrors, NewErrors, Errors),
		mode_info_set_errors(Errors, ModeInfo2, ModeInfo)
	),
	term__var_list_to_term_list(ArgVars, Args).

:- pred modecheck_call_pred_2(list(proc_id), pred_id, proc_table, list(var),
			set(var), proc_id, list(var), pair(list(hlds__goal)),
			mode_info, mode_info).
:- mode modecheck_call_pred_2(in, in, in, in, in, out, out, out,
			mode_info_di, mode_info_uo) is det.

modecheck_call_pred_2([], _PredId, _Procs, ArgVars, WaitingVars,
			0, ArgVars, [] - [], ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap),
	get_var_insts(ArgVars, InstMap, ArgInsts),
	mode_info_error(WaitingVars,
		mode_error_no_matching_mode(ArgVars, ArgInsts),
		ModeInfo0, ModeInfo).
	
modecheck_call_pred_2([ProcId | ProcIds], PredId, Procs, ArgVars0, WaitingVars,
			TheProcId, ArgVars, ExtraGoals, ModeInfo0, ModeInfo) :-

		% find the initial insts for this mode of the called pred
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_argmodes(ProcInfo, ProcArgModes0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
/**************
		% propagate the type information into the modes
	mode_info_get_types_of_vars(ModeInfo0, ArgVars0, ArgTypes),
	propagate_type_info_mode_list(ArgTypes, ModuleInfo,
		ProcArgModes0, ProcArgModes),
**************/
	ProcArgModes = ProcArgModes0,
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo, InitialInsts),

		% check whether the insts of the args matches their expected
		% initial insts
	modecheck_var_has_inst_list(ArgVars0, InitialInsts,
				ModeInfo0, ModeInfo1),
	mode_info_get_errors(ModeInfo1, Errors),
	(
			% if error(s) occured, keep trying with the other modes
			% for the called pred
		Errors = [FirstError | _]
	->
		FirstError = mode_error_info(WaitingVars2, _, _, _),
		set__union(WaitingVars, WaitingVars2, WaitingVars3),

		modecheck_call_pred_2(ProcIds, PredId, Procs, ArgVars0,
				WaitingVars3, TheProcId, ArgVars, ExtraGoals,
				ModeInfo0, ModeInfo)
	;
			% if there are no errors, then set their insts to the
			% final insts specified in the mode for the called pred
		mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts),
		modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
				ArgVars, ExtraGoals, ModeInfo1, ModeInfo2),
		TheProcId = ProcId,
		mode_info_never_succeeds(ModeInfo2, PredId, ProcId, Result),
		( Result = yes ->
			mode_info_set_instmap(unreachable, ModeInfo2, ModeInfo)
		;
			ModeInfo = ModeInfo2
		)
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

modecheck_var_has_inst_list([_|_], []) -->
	{ error("modecheck_var_has_inst_list: length mismatch") }.
modecheck_var_has_inst_list([], [_|_]) -->
	{ error("modecheck_var_has_inst_list: length mismatch") }.
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

:- pred modecheck_set_var_inst_list(list(var), list(inst), list(inst),
					list(var), pair(list(hlds__goal)),
					mode_info, mode_info).
:- mode modecheck_set_var_inst_list(in, in, in, out, out,
					mode_info_di, mode_info_uo) is det.

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
					list(var), pair(list(hlds__goal)),
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

:- pred modecheck_set_var_inst(var, inst, inst, var, pair(list(hlds__goal)),
				mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var0, InitialInst, FinalInst, Var, Goals,
			ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(_) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap_lookup_var(InstMap0, Var0, VarInst0),
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		(
			abstractly_unify_inst(dead, VarInst0, FinalInst,
				ModuleInfo0, UnifyInst, Det1, ModuleInfo1)
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

:- pred modecheck_set_var_inst(var, inst, mode_info, mode_info).
:- mode modecheck_set_var_inst(in, in, mode_info_di, mode_info_uo) is det.

modecheck_set_var_inst(Var0, FinalInst, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	( InstMap0 = reachable(InstMapping0) ->
		% The new inst must be computed by unifying the
		% old inst and the proc's final inst
		instmap_lookup_var(InstMap0, Var0, Inst0),
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		(
			abstractly_unify_inst(dead, Inst0, FinalInst,
				ModuleInfo0, UnifyInst, _Det, ModuleInfo1)
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
			% bound part of a var.
			inst_matches_final(Inst, Inst0, ModuleInfo)
		->
			% We've just added some information
			map__set(InstMapping0, Var0, Inst, InstMapping),
			InstMap = reachable(InstMapping),
			mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo2),
			mode_info_get_delay_info(ModeInfo2, DelayInfo0),
			delay_info__bind_var(DelayInfo0, Var0, DelayInfo),
			mode_info_set_delay_info(DelayInfo, ModeInfo2, ModeInfo)
		;
			% We've bound part of the var.  If the var was locked,
			% then we need to report an error.
			mode_info_var_is_locked(ModeInfo0, Var0)
		->
			set__singleton_set(WaitingVars, Var0),
			mode_info_error(WaitingVars,
					mode_error_bind_var(Var0, Inst0, Inst),
					ModeInfo1, ModeInfo
			)
		;
			map__set(InstMapping0, Var0, Inst, InstMapping),
			InstMap = reachable(InstMapping),
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
				var, pair(list(hlds__goal)),
				mode_info, mode_info).
:- mode handle_implied_mode(in, in, in, in, in, in, out, out,
				mode_info_di, mode_info_uo) is det.

handle_implied_mode(Var0, VarInst0, VarInst, InitialInst, FinalInst, Det,
		Var, Goals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		% If the initial inst of the variable matches
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
			categorize_unify_var_var(ModeVar0, ModeVar,
				live, dead, Var0, Var, Det,
				VarTypes, ModeInfo2,
				Unification, ModeInfo),
			mode_info_get_mode_context(ModeInfo, ModeContext),
			mode_context_to_unify_context(ModeContext,
				UnifyContext),
			AfterGoal = unify(term__variable(Var0),
					term__variable(Var),
					ModeVar0 - ModeVar,
					Unification,
					UnifyContext
			),

			% compute the goal_info nonlocal vars & instmap delta
			set__list_to_set([Var0, Var], NonLocals),
			map__init(InstMapDelta0),
			( VarInst = VarInst0 ->
				InstMapDelta1 = InstMapDelta0
			;
				map__set(InstMapDelta0, Var0, VarInst,
					InstMapDelta1)
			),
			map__set(InstMapDelta1, Var, VarInst, InstMapDelta),
			goal_info_init(GoalInfo0),
			goal_info_set_nonlocals(GoalInfo0, NonLocals,
				GoalInfo1),
			goal_info_set_instmap_delta(GoalInfo1,
				reachable(InstMapDelta), GoalInfo),
			Goals = [] - [AfterGoal - GoalInfo]
		)
	).

:- pred mode_context_to_unify_context(mode_context, unify_context).
:- mode mode_context_to_unify_context(in, out) is det.

mode_context_to_unify_context(unify(UnifyContext, _), UnifyContext).
mode_context_to_unify_context(call(PredId, Arg),
		unify_context(call(PredId, Arg), [])).
mode_context_to_unify_context(uninitialized, _) :-
	error("mode_context_to_unify_context: uninitialized context").

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
/*********
		% would this test improve efficiency??
	( InstA = InstB ->
		Inst = InstA,
		ModuleInfo = ModuleInfo0
	;
*********/
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
	inst_merge(ArgA, ArgB, ModuleInfo0, Arg, ModuleInfo1),
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

	% This code is used to trace the actions of the mode checker.

:- type port
	--->	enter
	;	exit
	;	wakeup.

:- pred mode_checkpoint(port, string, mode_info, mode_info).
:- mode mode_checkpoint(in, in, mode_info_di, mode_info_uo) is det.

mode_checkpoint(Port, Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
        globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
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
		globals__io_lookup_bool_option(statistics, Statistics),
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

:- pred modecheck_unification(term, term, term, term, pair(list(hlds__goal)),
			pair(mode, mode), unification, mode_info, mode_info).
:- mode modecheck_unification(in, in, out, out,
			out, out, out, mode_info_di, mode_info_uo) is det.

modecheck_unification(term__variable(X), term__variable(Y),
			term__variable(X), term__variable(Y),
			ExtraGoals, Modes, Unification, ModeInfo0, ModeInfo) :-
	ExtraGoals = [] - [],
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	instmap_lookup_var(InstMap0, Y, InstY),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_is_live(ModeInfo0, Y, LiveY),
	(
		( LiveX = live, LiveY = live ->
			abstractly_unify_inst(live, InstX, InstY, ModuleInfo0,
				UnifyInst, Det1, ModuleInfo1)
		;
			abstractly_unify_inst(dead, InstX, InstY, ModuleInfo0,
				UnifyInst, Det1, ModuleInfo1)
		)
	->
		Inst = UnifyInst,
		Det = Det1,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1)
	;
		set__list_to_set([X, Y], WaitingVars),
		mode_info_error(WaitingVars, 
				mode_error_unify_var_var(X, Y, InstX, InstY),
					ModeInfo0, ModeInfo1),
			% If we get an error, set the inst to not_reached
			% to suppress follow-on errors
		Inst = not_reached,
		Det = det
	),
	modecheck_set_var_inst(X, Inst, ModeInfo1, ModeInfo2),
	modecheck_set_var_inst(Y, Inst, ModeInfo2, ModeInfo3),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Modes = ModeX - ModeY,
	mode_info_get_var_types(ModeInfo3, VarTypes),
	categorize_unify_var_var(ModeX, ModeY, LiveX, LiveY, X, Y,
			Det, VarTypes, ModeInfo3, Unification, ModeInfo).

modecheck_unification(term__variable(X), term__functor(Name, Args0, Context),
			term__variable(X), term__functor(Name, Args, Context),
			ExtraGoals, Mode, Unification, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	instmap_lookup_var(InstMap0, X, InstX),
	term__term_list_to_var_list(Args0, ArgVars0),
	instmap_lookup_arg_list(ArgVars0, InstMap0, InstArgs),
	mode_info_var_is_live(ModeInfo0, X, LiveX),
	mode_info_var_list_is_live(ArgVars0, ModeInfo0, LiveArgs),
	InstY = bound([functor(Name, InstArgs)]),
	(
		% The occur check: X = f(X) is considered a mode error
		% unless X is ground.  (Actually it wouldn't be that
		% hard to generate code for it - it always fails! -
		% but it's most likely to be a programming error,
		% so it's better to report it.)

		list__member(X, ArgVars0),
		\+ inst_is_ground(ModuleInfo0, InstX)
	->
		set__list_to_set([X], WaitingVars),
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, Name, Args0,
							InstX, InstArgs),
			ModeInfo0, ModeInfo1
		),
		Inst = not_reached
	;
		abstractly_unify_inst_functor(LiveX, InstX, Name,
			InstArgs, LiveArgs, ModuleInfo0, UnifyInst, ModuleInfo1)
	->
		Inst = UnifyInst,
		mode_info_set_module_info(ModeInfo0, ModuleInfo1, ModeInfo1)
	;
		set__list_to_set([X | ArgVars0], WaitingVars), % conservative
		mode_info_error(WaitingVars,
			mode_error_unify_var_functor(X, Name, Args0,
							InstX, InstArgs),
			ModeInfo0, ModeInfo1
		),
			% If we get an error, set the inst to not_reached
			% to avoid cascading errors
		Inst = not_reached
	),
	ModeX = (InstX -> Inst),
	ModeY = (InstY -> Inst),
	Mode = ModeX - ModeY,
	( get_mode_of_args(Inst, InstArgs, ModeArgs0) ->
		ModeArgs = ModeArgs0
	;
		error("get_mode_of_args failed")
	),
	mode_info_get_var_types(ModeInfo1, VarTypes),
	categorize_unify_var_functor(ModeX, ModeArgs, X, Name, ArgVars0,
			VarTypes, ModeInfo1, Unification0, ModeInfo2),
	split_complicated_subunifies(Unification0, Args0, ArgVars0,
			Unification, Args, ArgVars,
			ExtraGoals, ModeInfo2, ModeInfo3),
	modecheck_set_var_inst(X, Inst, ModeInfo3, ModeInfo4),
	( bind_args(Inst, ArgVars, ModeInfo4, ModeInfo5) ->
		ModeInfo = ModeInfo5
	;
		error("bind_args failed")
	).

modecheck_unification(term__functor(F, As, Context), term__variable(Y),
		Var, Functor, ExtraGoals, Modes, Unification,
		ModeInfo0, ModeInfo) :-
	modecheck_unification(term__variable(Y), term__functor(F, As, Context),
		Var, Functor, ExtraGoals, Modes, Unification,
		ModeInfo0, ModeInfo).
	
modecheck_unification(term__functor(_, _, _), term__functor(_, _, _),
		_, _, _, _, _, _, _) :-
	error("modecheck internal error: unification of term with term\n").

%-----------------------------------------------------------------------------%

:- pred split_complicated_subunifies(unification, list(term), list(var),
			unification, list(term), list(var),
			pair(list(hlds__goal)), mode_info, mode_info).
:- mode split_complicated_subunifies(in, in, in, out, out, out, out,
		mode_info_di, mode_info_uo) is det.

split_complicated_subunifies(Unification0, Args0, ArgVars0,
				Unification, Args, ArgVars, ExtraGoals) -->
	(
		{ Unification0 = deconstruct(X, ConsId, ArgVars0, ArgModes0,
			Det) }
	->
		(
			split_complicated_subunifies_2(ArgVars0, ArgModes0,
				ArgVars1, ArgModes, ExtraGoals1)
		->
			{ ArgVars = ArgVars1 },
			{ Unification = deconstruct(X, ConsId, ArgVars,
							ArgModes, Det) },
			{ term__var_list_to_term_list(ArgVars, Args) },
			{ ExtraGoals = ExtraGoals1 }
		;
			{ error("split_complicated_subunifies_2 failed") }
		)
	;
		{ Unification = Unification0 },
		{ Args = Args0 },
		{ ArgVars = ArgVars0 },
		{ ExtraGoals = [] - [] }
	).

:- pred split_complicated_subunifies_2(list(var), list(uni_mode),
			list(var), list(uni_mode), pair(list(hlds__goal)),
			mode_info, mode_info).
:- mode split_complicated_subunifies_2(in, in, out, out, out,
		mode_info_di, mode_info_uo) is semidet.

split_complicated_subunifies_2([], [], [], [], [] - []) --> [].
split_complicated_subunifies_2([Var0 | Vars0], [UniMode0 | UniModes0], 
			Vars, UniModes, ExtraGoals, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	UniMode0 = (InitialInstX - InitialInstY -> FinalInstX - FinalInstY),
	(
		mode_is_input(ModuleInfo, InitialInstX -> FinalInstX),
		mode_is_input(ModuleInfo, InitialInstY -> FinalInstY)
	->
		split_complicated_subunifies_2(Vars0, UniModes0,
				Vars1, UniModes1, ExtraGoals0,
				ModeInfo0, ModeInfo1),
		ExtraGoals0 = BeforeGoals - AfterGoals0,

		% introduce a new variable `Var'
		mode_info_get_varset(ModeInfo1, VarSet0),
		mode_info_get_var_types(ModeInfo1, VarTypes0),
		varset__new_var(VarSet0, Var, VarSet),
		map__lookup(VarTypes0, Var0, VarType),
		map__set(VarTypes0, Var, VarType, VarTypes),
		mode_info_set_varset(VarSet, ModeInfo1, ModeInfo2),
		mode_info_set_var_types(VarTypes, ModeInfo2, ModeInfo3),

		% change the main unification to use `Var' instead of Var0
		UniMode = (InitialInstX - free -> InitialInstX - InitialInstX),
		UniModes = [UniMode | UniModes1],
		Vars = [Var | Vars1],

		% create code to do a unification between `Var' and `Var0'
		ModeVar0 = (InitialInstY -> FinalInstY),
		ModeVar = (InitialInstX -> FinalInstX),

		/* XXX temporary hack alert XXX */
		Det = semidet,	% warning - it might be det in some cases
/********
		(
			abstractly_unify_inst(dead, InitialInstX, InitialInstY,
				ModuleInfo0, _, Det1, ModuleInfo1)
		->
			ModuleInfo = ModuleInfo1,
			Det = Det1
		;
			error("abstractly_unify_inst failed")
		),
*********/
		categorize_unify_var_var(ModeVar0, ModeVar,
			live, dead, Var0, Var, Det,
			VarTypes, ModeInfo3, Unification, ModeInfo),
		mode_info_get_mode_context(ModeInfo, ModeContext),
		mode_context_to_unify_context(ModeContext,
			UnifyContext),
		AfterGoal = unify(term__variable(Var0),
				term__variable(Var),
				ModeVar0 - ModeVar,
				Unification,
				UnifyContext
		),

		% compute the goal_info nonlocal vars & instmap delta
		% for the newly created goal
		set__list_to_set([Var0, Var], NonLocals),
		map__init(InstMapDelta0),
		( InitialInstY = FinalInstY ->
			InstMapDelta1 = InstMapDelta0
		;
			map__set(InstMapDelta0, Var0, FinalInstY,
				InstMapDelta1)
		),
		map__set(InstMapDelta1, Var, FinalInstX, InstMapDelta),
		goal_info_init(GoalInfo0),
		goal_info_set_nonlocals(GoalInfo0, NonLocals,
			GoalInfo1),
		goal_info_set_instmap_delta(GoalInfo1,
			reachable(InstMapDelta), GoalInfo),

		% insert the unification between `Var' and `Var0' at
		% the start of the AfterGoals
		AfterGoals = [AfterGoal - GoalInfo | AfterGoals0],
		ExtraGoals = BeforeGoals - AfterGoals
	;
		Vars = [Var0 | Vars1],
		UniModes = [UniMode0 | UniModes1],
		split_complicated_subunifies_2(Vars0, UniModes0, 
			Vars1, UniModes1, ExtraGoals, ModeInfo0, ModeInfo)
	).

%-----------------------------------------------------------------------------%

:- pred bind_args(inst, list(var), mode_info, mode_info).
:- mode bind_args(in, in, mode_info_di, mode_info_uo) is semidet.

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
:- mode bind_args_2(in, in, mode_info_di, mode_info_uo) is semidet.

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
:- mode get_mode_of_args(in, in, out) is semidet.

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
:- mode get_mode_of_args_2(in, in, out) is semidet.

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
				inst, determinism, module_info).
:- mode abstractly_unify_inst(in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst(Live, InstA, InstB, ModuleInfo0, Inst, Det, ModuleInfo) :-
		% check whether this pair of insts is already in
		% the unify_insts table
	ThisInstPair = unify_inst_pair(Live, InstA, InstB),
	module_info_insts(ModuleInfo0, InstTable0),
	inst_table_get_unify_insts(InstTable0, UnifyInsts0),
	( map__search(UnifyInsts0, ThisInstPair, Result) ->
		( Result = known(UnifyInst, UnifyDet) ->
			Inst = UnifyInst,
			Det = UnifyDet
		;
			Inst = defined_inst(unify_inst(Live, InstA, InstB)),
				% It's ok to assume that the unification is
				% deterministic here, because the only time that
				% this will happen is when we get to the
				% recursive case for a recursively defined inst.
				% If the unification as a whole is semidet then
				% it must be semidet somewhere else too.
			Det = det
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
		abstractly_unify_inst_2(Live, InstA2, InstB2, ModuleInfo1,
			Inst, Det, ModuleInfo2),
			% now update the value associated with ThisInstPair
		module_info_insts(ModuleInfo2, InstTable2),
		inst_table_get_unify_insts(InstTable2, UnifyInsts2),
		map__set(UnifyInsts2, ThisInstPair, known(Inst, Det),
			UnifyInsts),
		inst_table_set_unify_insts(InstTable2, UnifyInsts, InstTable),
		module_info_set_insts(ModuleInfo2, InstTable, ModuleInfo)
	).

:- pred abstractly_unify_inst_2(is_live, inst, inst, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_2(in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_2(IsLive, InstA, InstB, ModuleInfo0, Inst, Det,
		ModuleInfo) :-
	( InstB = not_reached ->
		Inst = InstA,
		Det = det,
		ModuleInfo = ModuleInfo0
/****************
		% does this test improve efficiency??
	; InstA = InstB ->
		( IsLive = dead ->
			true
		;
			inst_is_ground(ModuleInfo0, InstA)
		),
		Inst = InstA,
		ModuleInfo = ModuleInfo0
****************/
	;
		abstractly_unify_inst_3(IsLive, InstA, InstB, ModuleInfo0,
			Inst, Det, ModuleInfo)
	).

	% Abstractly unify two expanded insts.
	% The is_live parameter is `live' iff *both* insts are live.

:- pred abstractly_unify_inst_3(is_live, inst, inst, module_info,
				inst, determinism, module_info).
:- mode abstractly_unify_inst_3(in, in, in, in, out, out, out) is semidet.

:- abstractly_unify_inst_3(A, B, C, _, _, _, _) when A and B and C.

abstractly_unify_inst_3(live, not_reached, _,		M, not_reached, det, M).

abstractly_unify_inst_3(live, free,	free,		_, _, _, _) :- fail.
abstractly_unify_inst_3(live, free,	bound(List),	M, bound(List), det,
		M) :-
	bound_inst_list_is_ground(List, M).
abstractly_unify_inst_3(live, free,	ground,		M, ground, det, M).
abstractly_unify_inst_3(live, free,	abstract_inst(_,_), _, _, _, _) :- fail.
	
abstractly_unify_inst_3(live, bound(List), free,	M, bound(List), det,
		M) :-
	bound_inst_list_is_ground(List, M).
abstractly_unify_inst_3(live, bound(ListX), bound(ListY), M0, bound(List), Det,
		M) :-
	abstractly_unify_bound_inst_list(live, ListX, ListY, M0, List, Det, M).
abstractly_unify_inst_3(live, bound(BoundInsts0),	ground,	M0,
		bound(BoundInsts), semidet, M) :-
	make_ground_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
abstractly_unify_inst_3(live, bound(List), abstract_inst(_,_), ModuleInfo,
						ground, semidet, ModuleInfo) :-
	bound_inst_list_is_ground(List, ModuleInfo).

abstractly_unify_inst_3(live, ground,	Inst0,		M0, Inst, Det, M) :-
	( inst_is_free(M0, Inst0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, M0, Inst, M).

abstractly_unify_inst_3(live, abstract_inst(_,_), free,	_, _, _, _) :- fail.
abstractly_unify_inst_3(live, abstract_inst(_,_), bound(List), ModuleInfo,
				ground, semidet, ModuleInfo) :-
	bound_inst_list_is_ground(List, ModuleInfo).
abstractly_unify_inst_3(live, abstract_inst(_,_), ground, M, ground, semidet,
				M).
abstractly_unify_inst_3(live, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo0,
			abstract_inst(Name, Args), Det, ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, live, ModuleInfo0,
		Args, Det, ModuleInfo).

abstractly_unify_inst_3(dead, not_reached, _, M, not_reached, det, M).

abstractly_unify_inst_3(dead, free, Inst, M, Inst, det, M).
	
abstractly_unify_inst_3(dead, bound(List), free, M, bound(List), det, M).

abstractly_unify_inst_3(dead, bound(ListX), bound(ListY), M0, bound(List), Det,
		M) :-
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M0, List, Det, M).
abstractly_unify_inst_3(dead, bound(BoundInsts0), ground, M0,
			bound(BoundInsts), semidet, M) :-
	make_ground_bound_inst_list(BoundInsts0, M0, BoundInsts, M).
abstractly_unify_inst_3(dead, bound(List), abstract_inst(N,As), ModuleInfo,
					Result, Det, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(List),
		Det = semidet
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).

abstractly_unify_inst_3(dead, ground,		Inst0,	M0, Inst, Det, M) :-
	( inst_is_free(M0, Inst0) ->
		Det = det
	;
		Det = semidet
	),
	make_ground_inst(Inst0, M0, Inst, M).

abstractly_unify_inst_3(dead, abstract_inst(N,As), bound(List), ModuleInfo, 
						Result, Det, ModuleInfo) :-
	( bound_inst_list_is_ground(List, ModuleInfo) ->
		Result = bound(List),
		Det = semidet
	; bound_inst_list_is_free(List, ModuleInfo) ->
		Result = abstract_inst(N,As),
		Det = det
	;
		fail
	).
abstractly_unify_inst_3(dead, abstract_inst(_,_), ground, ModuleInfo,
		ground, semidet, ModuleInfo).
abstractly_unify_inst_3(dead, abstract_inst(Name, ArgsA),
			abstract_inst(Name, ArgsB), ModuleInfo0,
			abstract_inst(Name, Args), Det, ModuleInfo) :-
	abstractly_unify_inst_list(ArgsA, ArgsB, dead, ModuleInfo0,
			Args, Det, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Abstractly unify two inst lists.

:- pred abstractly_unify_inst_list(list(inst), list(inst), is_live, module_info,
					list(inst), determinism, module_info).
:- mode abstractly_unify_inst_list(in, in, in, in, out, out, out) is semidet.

abstractly_unify_inst_list([], [], _, M, [], det, M).
abstractly_unify_inst_list([X|Xs], [Y|Ys], Live, ModuleInfo0,
				[Z|Zs], Det, ModuleInfo) :-
	abstractly_unify_inst(Live, X, Y, ModuleInfo0, Z, Det1, ModuleInfo1),
	abstractly_unify_inst_list(Xs, Ys, Live, ModuleInfo1, Zs, Det2,
		ModuleInfo),
	( Det1 = semidet ->
		Det = semidet
	;
		Det = Det2
	).

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
	abstractly_unify_bound_inst_list(dead, ListX, ListY, M0, List, _, M). 
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

% abstractly unify and inst with `ground' and calculate the new inst
% and the determinism of the unification.

:- pred make_ground_inst(inst, module_info, inst, module_info).
:- mode make_ground_inst(in, in, out, out) is det.

make_ground_inst(not_reached, M, not_reached, M).
make_ground_inst(free, M, ground, M).
make_ground_inst(free(T), M, defined_inst(typed_ground(T)), M).
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
		list(bound_inst), module_info,
		list(bound_inst), determinism, module_info).
:- mode abstractly_unify_bound_inst_list(in, in, in, in,
		out, out, out) is semidet.

:- abstractly_unify_bound_inst_list(_, Xs, Ys, _, _, _, _)
	when Xs and Ys. % Index

abstractly_unify_bound_inst_list(_, [], [], ModuleInfo, [], det, ModuleInfo).
abstractly_unify_bound_inst_list(_, [], [_|_], M, [], semidet, M).
abstractly_unify_bound_inst_list(_, [_|_], [], M, [], semidet, M).
abstractly_unify_bound_inst_list(Live, [X|Xs], [Y|Ys], ModuleInfo0,
		L, Det, ModuleInfo) :-
	X = functor(NameX, ArgsX),
	list__length(ArgsX, ArityX),
	Y = functor(NameY, ArgsY),
	list__length(ArgsY, ArityY),
	( NameX = NameY, ArityX = ArityY ->
	    ( abstractly_unify_inst_list(ArgsX, ArgsY, Live, ModuleInfo0,
			Args, Det1, ModuleInfo1)
	    ->
		L = [functor(NameX, Args) | L1],
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
					ModuleInfo1, L1, Det2, ModuleInfo),
		( Det1 = semidet ->
		    Det = semidet
		;
		    Det = Det2
		)
	    ;
		abstractly_unify_bound_inst_list(Live, Xs, Ys,
						ModuleInfo0, L, Det, ModuleInfo)
	    )
	;
	    Det = semidet,
	    ( compare(<, X, Y) ->
		abstractly_unify_bound_inst_list(Live, Xs, [Y|Ys],
						ModuleInfo0, L, _, ModuleInfo)
	    ;
		abstractly_unify_bound_inst_list(Live, [X|Xs], Ys,
						ModuleInfo0, L, _, ModuleInfo)
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
	abstractly_unify_inst(Live, X, Y, ModuleInfo0, Z, _Det, ModuleInfo1),
	abstractly_unify_inst_list_lives(Xs, Ys, Lives, ModuleInfo1, Zs,
		ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred categorize_unify_var_var(mode, mode, is_live, is_live, var, var,
			determinism, map(var, type), mode_info,
			unification, mode_info).
:- mode categorize_unify_var_var(in, in, in, in, in, in, in, in, mode_info_di,
				out, mode_info_uo) is det.

categorize_unify_var_var(ModeX, ModeY, LiveX, LiveY, X, Y, Det, VarTypes,
		ModeInfo0, Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		mode_is_output(ModuleInfo0, ModeX)
	->
		Unification = assign(X, Y),
		ModeInfo = ModeInfo0
	;
		mode_is_output(ModuleInfo0, ModeY)
	->
		Unification = assign(Y, X),
		ModeInfo = ModeInfo0
	;
		mode_is_unused(ModuleInfo0, ModeX),
		mode_is_unused(ModuleInfo0, ModeY)
	->
		% For free-free unifications, we pretend that they
		% are an assignment to the dead variable.
		% (It might be a better idea to have a separate category
		% for these)
		( LiveX = dead ->
			Unification = assign(X, Y)
		; LiveY = dead ->
			Unification = assign(Y, X)
		;
			error("categorize_unify_var_var: free-free unify!")
		),
		ModeInfo = ModeInfo0
	;
		map__lookup(VarTypes, X, Type),
		(
			type_is_atomic(Type, ModuleInfo0)
		->
			Unification = simple_test(X, Y),
			ModeInfo = ModeInfo0
		;
			mode_get_insts(ModuleInfo0, ModeX, IX, FX),
			mode_get_insts(ModuleInfo0, ModeY, IY, FY),
			map__init(Follow),
			determinism_to_category(Det, Determinism),
			UniMode = ((IX - IY) -> (FX - FY)),
			Unification = complicated_unify(UniMode, Determinism,
				Follow),
			(
				Type = term__functor(term__atom("pred"), _, _)
			->
				set__init(WaitingVars),
				mode_info_error(WaitingVars,
					mode_error_unify_pred,
					ModeInfo0, ModeInfo)
			;
				type_to_type_id(Type, TypeId, _)
			->
				module_info_get_unify_requests(ModuleInfo0,
					UnifyRequests0),
				unify_proc__request_unify(TypeId - UniMode, 
					UnifyRequests0, UnifyRequests),
				module_info_set_unify_requests(ModuleInfo0,
					UnifyRequests, ModuleInfo),
				mode_info_set_module_info(ModeInfo0, ModuleInfo,
					ModeInfo)
			;
				ModeInfo = ModeInfo0
			)
		)
	).

% categorize_unify_var_functor works out which category a unification
% between a variable and a functor is - whether it is a construction
% unification or a deconstruction.  It also works out whether it will
% be deterministic or semideterministic.

:- pred categorize_unify_var_functor(mode, list(mode), var, const,
			list(var), map(var, type), mode_info,
			unification, mode_info).
:- mode categorize_unify_var_functor(in, in, in, in, in, in, mode_info_di,
			out, mode_info_uo) is det.

categorize_unify_var_functor(ModeX, ArgModes0, X, Name, ArgVars, VarTypes,
		ModeInfo0, Unification, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	list__length(ArgVars, Arity),
	make_functor_cons_id(Name, Arity, ConsId),
	mode_util__modes_to_uni_modes(ModeX, ArgModes0,
						ModuleInfo, ArgModes),
	map__lookup(VarTypes, X, TypeX),
	( TypeX = term__functor(term__atom("pred"), _, _) ->
		set__init(WaitingVars),
		mode_info_error(WaitingVars, mode_error_unify_pred,
			ModeInfo0, ModeInfo)
	;
		ModeInfo = ModeInfo0
	),
	(
		mode_is_output(ModuleInfo, ModeX)
	->
		Unification = construct(X, ConsId, ArgVars, ArgModes)
	; 
		% It's a deconstruction.
		(
			% If the variable was already known to be bound
			% to a single particular functor, then the
			% unification either always succeeds or always
			% fails.  In the latter case, the final inst will
			% be `not_reached' or `bound([])'.  So if both
			% the initial and final inst are `bound([_])',
			% then the unification must be deterministic.
			mode_get_insts(ModuleInfo, ModeX,
					InitialInst0, FinalInst0),
			inst_expand(ModuleInfo, InitialInst0, InitialInst),
			inst_expand(ModuleInfo, FinalInst0, FinalInst),
			InitialInst = bound([_]),
			FinalInst = bound([_])
		->
			Det = deterministic
		;
			% If the type has only one constructor, then the
			% unification must be deterministic
			type_constructors(TypeX, ModuleInfo, Constructors),
			Constructors = [_]
		->
			Det = deterministic
		;
			% Otherwise, it's semidet
			Det = semideterministic
		),
		Unification = deconstruct(X, ConsId, ArgVars, ArgModes, Det)
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
