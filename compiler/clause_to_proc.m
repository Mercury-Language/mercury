%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module clause_to_proc.

:- interface.

:- import_module hlds.

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

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list, set, map, std_util.

copy_clauses_to_procs(PredInfo0, PredInfo) :-
	pred_info_clauses_info(PredInfo0, ClausesInfo),
	pred_info_procedures(PredInfo0, Procs0),
	map__keys(Procs0, ProcIds),
	copy_clauses_to_procs_2(ProcIds, ClausesInfo, Procs0, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

:- pred copy_clauses_to_procs_2(list(proc_id), clauses_info,
	proc_table, proc_table).
:- mode copy_clauses_to_procs_2(in, in, di, uo) is det.

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
