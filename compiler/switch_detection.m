%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Switch Detection - replace disjunctions with deterministic switch
% statements, where we can determine that the disjunction is actually
% just a switch.
%
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module switch_detection.

:- interface. 

:- import_module hlds.

:- pred detect_switches(module_info, module_info).
:- mode detect_switches(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_switches_in_goal'
	% for each procedure body.

detect_switches(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	detect_switches_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_switches_in_preds(list(pred_id), module_info, module_info).
:- mode detect_switches_in_preds(in, in, out) is det.

detect_switches_in_preds([], ModuleInfo, ModuleInfo).
detect_switches_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	detect_switches_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_switches_in_procs(list(proc_id), pred_id, module_info,
					module_info).
:- mode detect_switches_in_procs(in, in, in, out) is det.

detect_switches_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_switches_in_procs([ProcId | ProcIds], PredId, ModuleInfo0,
					ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% and the initial instmap, and pass these to
		% `detect_switches_in_goal'.
	proc_info_goal(ProcInfo0, Goal0),
	map__init(InstMap0),
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo, Goal),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

	% This version doesn't return the resulting instmap

:- pred detect_switches_in_goal(hlds__goal, instmap, module_info, hlds__goal).
:- mode detect_switches_in_goal(in, in, in, out).

detect_switches_in_goal(Goal0 - GoalInfo, InstMap0, ModuleInfo,
			Goal - GoalInfo) :-
	detect_switches_in_goal_2(Goal0, InstMap0, ModuleInfo, Goal).
	
	% This version does return the resulting instmap

:- pred detect_switches_in_goal(hlds__goal, instmap, module_info,
				hlds__goal, instmap).
:- mode detect_switches_in_goal(in, in, in, out, out).

detect_switches_in_goal(Goal0 - GoalInfo, InstMap0, ModuleInfo,
		Goal - GoalInfo, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0, ModuleInfo,
			Goal),
	apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

:- pred detect_switches_in_goal_2(hlds__goal_expr, goal_info, instmap,
		module_info, hlds__goal_expr, instmap).
:- mode detect_switches_in_goal_2(in, in, in, in, out, out).

detect_switches_in_goal_2(conj(Goals0), GoalInfo, InstMap0, ModuleInfo, conj(Goals), InstMap) :-
	detect_switches_in_goal_list(Goals0, Goals).

detect_switches_in_goal_2(disj(Goals0), GoalInfo0, Goals, GoalInfo) :-
	goal_info_nonlocals(GoalInfo0, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	detect_switches_in_disj(NonLocalsList, Goals0, InstMap, InstMapDelta,
			ModuleInfo, Goal).

detect_switches_in_goal_2(not(Vars, Goal0), GoalInfo, not(Vars, Goal),
		GoalInfo) :-
	detect_switches_in_goal(Goal0, Goal).

detect_switches_in_goal(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo0,
			if_then_else(Vars, Cond, Then, Else), GoalInfo) :-
	detect_switches_in_goal(Cond0, Cond),
	detect_switches_in_goal(Then0, Then),
	detect_switches_in_goal(Else0, Else).

detect_switches_in_goal_2(some(Vars, Goal0), GoalInfo, some(Vars, Goal),
		GoalInfo) :-
	detect_switches_in_goal(Goal0, Goal).
detect_switches_in_goal_2(all(Vars, Goal0), GoalInfo, all(Vars, Goal),
		GoalInfo) :-
	detect_switches_in_goal(Goal0, Goal).

detect_switches_in_goal_2(call(A,B,C,D), GoalInfo, call(A,B,C,D), GoalInfo).
detect_switches_in_goal_2(unify(A,B,C,D,E), GoalInfo, unify(A,B,C,D,E),
		GoalInfo).

%-----------------------------------------------------------------------------%

	% This is the interesting bit - we've found a disjunction, and
	% we've got a list of the non-local variables of that
	% disjunction.  Now for each non-local variable, we check
	% whether there is a partition of the disjuncts such that each
	% group of disjunctions can only succeed if the variable is
	% bound to a different functor.  We check this by examining the
	% instantiatedness of the variable after the disjunction.

:- pred detect_switches_in_disj(list(var), list(hlds__goal), instmap,
				instmap_delta, module_info, hlds__goal).
:- mode detect_switches_in_disj(in, in, in, in, out).

detect_switches_in_disj([], Goals, _, _, _, disj(Goals)).
detect_switches_in_disj([Var | Vars], Goals0, InstMap, InstMapDelta, ModuleInfo,
		Goal) :-
	(
		map__search(InstMap, Var, VarInst0),
		inst_is_bound(ModuleInfo, VarInst0),
		( map__search(InstMapDelta, Var, VarInst1) ->
			VarInst = VarInst1
		;
			VarInst = VarInst0
		),
		inst_is_bound_to_functors(ModuleInfo, VarInst, Functors),
		partition_disj(Goals0, Functors, InstMapDelta, ModuleInfo,
			Partitions)
	->
		detect_switches_in_cases(Partitions, InstMap, ModuleInfo,
			Cases),
		map__init(FollowVars),
		Goal = switch(Var, Cases, FollowVars)
	;
		detect_switches_in_disj(Vars, Goals0, InstMap, InstMapDelta,
			ModuleInfo, Goal)
	).

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_cases(list(case), instmap, module_info, list(case)).
:- mode detect_switches_in_cases(in, in, in, out).

detect_switches_in_cases([], _InstMap, _ModuleInfo, []).
detect_switches_in_cases([Case0 | Cases0], InstMap, ModuleInfo,
		[Case | Cases]) :-
	detect_switches_in_goal(Case0, InstMap, ModuleInfo, Case),
	detect_switches_in_cases(Cases0, InstMap, ModuleInfo, Cases).

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_goal_list(list(hlds__goal), instmap, module_info,
				list(hlds__goal)).
:- mode detect_switches_in_goal_list(in, in, in, out).

detect_switches_in_goal_list([], _InstMap, _ModuleInfo, []).
detect_switches_in_goal_list([Goal0 | Goals0], InstMap0, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo, Goal, InstMap1),
	detect_switches_in_goal_list(Goals0, InstMap1, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
