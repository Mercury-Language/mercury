%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module check_hlds__clause_to_proc.

:- interface.

:- import_module hlds__hlds_pred, hlds__hlds_module.
:- import_module list, std_util.

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

:- pred copy_module_clauses_to_procs(list(pred_id), module_info, module_info).
:- mode copy_module_clauses_to_procs(in, in, out) is det.

:- pred copy_clauses_to_procs(pred_info, pred_info).
:- mode copy_clauses_to_procs(in, out) is det.

:- pred copy_clauses_to_proc(proc_id, clauses_info, proc_info, proc_info).
:- mode copy_clauses_to_proc(in, in, in, out) is det.

	% Before copying the clauses to the procs, we need to add
	% a default mode of `:- mode foo(in, in, ..., in) = out is det.'
	% for functions that don't have an explicit mode declaration.

:- pred maybe_add_default_func_modes(list(pred_id), pred_table, pred_table).
:- mode maybe_add_default_func_modes(in, in, out) is det.

:- pred maybe_add_default_func_mode(pred_info, pred_info, maybe(proc_id)).
:- mode maybe_add_default_func_mode(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_data, parse_tree__prog_data.
:- import_module check_hlds__mode_util, hlds__make_hlds, check_hlds__purity.
:- import_module libs__globals.
:- import_module bool, int, set, map, varset.

maybe_add_default_func_modes([], Preds, Preds).
maybe_add_default_func_modes([PredId | PredIds], Preds0, Preds) :-
	map__lookup(Preds0, PredId, PredInfo0),
	maybe_add_default_func_mode(PredInfo0, PredInfo, _),
	map__det_update(Preds0, PredId, PredInfo, Preds1),
	maybe_add_default_func_modes(PredIds, Preds1, Preds).

maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeProcId) :-
	pred_info_procedures(PredInfo0, Procs0),
	pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
	( 
		%
		% Is this a function with no modes?
		%
		PredOrFunc = function,
		map__is_empty(Procs0)
	->
		%
		% If so, add a default mode of 
		%
		%	:- mode foo(in, in, ..., in) = out is det.
		%
		% for this function.  (N.B. functions which can
		% fail must be explicitly declared as semidet.)
		%
		pred_info_arity(PredInfo0, PredArity),
		FuncArity is PredArity - 1,
		in_mode(InMode),
		out_mode(OutMode),
		list__duplicate(FuncArity, InMode, FuncArgModes),
		FuncRetMode = OutMode,
		list__append(FuncArgModes, [FuncRetMode], PredArgModes),
		Determinism = det,
		pred_info_context(PredInfo0, Context),
		MaybePredArgLives = no,
		varset__init(InstVarSet),
			% No inst_vars in default func mode.
		add_new_proc(PredInfo0, InstVarSet, PredArity, PredArgModes, 
			yes(PredArgModes), MaybePredArgLives, yes(Determinism),
			Context, address_is_not_taken, PredInfo, ProcId),
		MaybeProcId = yes(ProcId)
	;
		PredInfo = PredInfo0,
		MaybeProcId = no
	).

copy_module_clauses_to_procs(PredIds, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds0),
	copy_module_clauses_to_procs_2(PredIds, Preds0, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo).

:- pred copy_module_clauses_to_procs_2(list(pred_id), pred_table, pred_table).
:- mode copy_module_clauses_to_procs_2(in, in, out) is det.

copy_module_clauses_to_procs_2([], Preds, Preds).
copy_module_clauses_to_procs_2([PredId | PredIds], Preds0, Preds) :-
	map__lookup(Preds0, PredId, PredInfo0),
	(
		% don't process typeclass methods, because their proc_infos
		% are generated already mode-correct
		pred_info_get_markers(PredInfo0, PredMarkers),
		check_marker(PredMarkers, class_method)
	->
		Preds1 = Preds0
	;
		copy_clauses_to_procs(PredInfo0, PredInfo),
		map__det_update(Preds0, PredId, PredInfo, Preds1)
	),
	copy_module_clauses_to_procs_2(PredIds, Preds1, Preds).


copy_clauses_to_procs(PredInfo0, PredInfo) :-
	pred_info_procedures(PredInfo0, Procs0),
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	pred_info_all_non_imported_procids(PredInfo0, ProcIds),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id), clauses_info,
	proc_table, proc_table).
:- mode copy_clauses_to_procs_2(in, in, in, out) is det.

copy_clauses_to_procs_2([], _, Procs, Procs).
copy_clauses_to_procs_2([ProcId | ProcIds], ClausesInfo, Procs0, Procs) :-
	map__lookup(Procs0, ProcId, Proc0),
	copy_clauses_to_proc(ProcId, ClausesInfo, Proc0, Proc),
	map__det_update(Procs0, ProcId, Proc, Procs1),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs1, Procs).

copy_clauses_to_proc(ProcId, ClausesInfo, Proc0, Proc) :-
	ClausesInfo = clauses_info(VarSet, _, _, VarTypes, HeadVars, Clauses,
		TI_VarMap, TCI_VarMap, _),
	select_matching_clauses(Clauses, ProcId, MatchingClauses),
	get_clause_goals(MatchingClauses, GoalList),
	( GoalList = [SingleGoal] ->
		Goal = SingleGoal
	;
		%
		% Convert the list of clauses into a disjunction,
		% and construct a goal_info for the disjunction.
		%

		%
		% We use the context of the first clause, unless
		% there weren't any clauses at all, in which case
		% we use the context of the mode declaration.
		%
		goal_info_init(GoalInfo0),
		( GoalList = [FirstGoal | _] ->
			FirstGoal = _ - FirstGoalInfo,
			goal_info_get_context(FirstGoalInfo, Context)
		;
			proc_info_context(Proc0, Context)
		),
		goal_info_set_context(GoalInfo0, Context, GoalInfo1),

		%
		% The non-local vars are just the head variables.
		%
		set__list_to_set(HeadVars, NonLocalVars),
		goal_info_set_nonlocals(GoalInfo1, NonLocalVars, GoalInfo2),

		%
		% The disjunction is impure/semipure if any of the disjuncts
		% is impure/semipure.
		%
		(
			list__member(_SubGoal - SubGoalInfo, GoalList),
			\+ goal_info_is_pure(SubGoalInfo)
		->
			list__map(get_purity, GoalList, PurityList),
			list__foldl(worst_purity, PurityList, (pure), Purity),
			add_goal_info_purity_feature(GoalInfo2, Purity,
				GoalInfo)
		;
			GoalInfo2 = GoalInfo
		),

		Goal = disj(GoalList) - GoalInfo
	),
	proc_info_set_body(Proc0, VarSet, VarTypes, HeadVars, Goal,
		TI_VarMap, TCI_VarMap, Proc).

:- pred get_purity(hlds_goal, purity).
:- mode get_purity(in, out) is det.

get_purity(_Goal - GoalInfo, Purity) :-
	infer_goal_info_purity(GoalInfo, Purity).

:- pred select_matching_clauses(list(clause), proc_id, list(clause)).
:- mode select_matching_clauses(in, in, out) is det.

select_matching_clauses([], _, []).
select_matching_clauses([Clause | Clauses], ProcId, MatchingClauses) :-
	Clause = clause(ProcIds, _, _, _),
	% an empty list here means that the clause applies to all procs
	( ProcIds = [] ->
		MatchingClauses = [Clause | MatchingClauses1]
	; list__member(ProcId, ProcIds) ->
		MatchingClauses = [Clause | MatchingClauses1]
	;
		MatchingClauses = MatchingClauses1
	),
	select_matching_clauses(Clauses, ProcId, MatchingClauses1).

:- pred get_clause_goals(list(clause)::in, list(hlds_goal)::out) is det.

get_clause_goals([], []).
get_clause_goals([Clause | Clauses], Goals) :-
	Clause = clause(_, Goal, _, _),
	goal_to_disj_list(Goal, GoalList),
	list__append(GoalList, Goals1, Goals),
	get_clause_goals(Clauses, Goals1).

