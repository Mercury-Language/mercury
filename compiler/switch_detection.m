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
:- import_module list, map, set, std_util.
:- import_module mode_util, term.

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
	( pred_info_is_imported(PredInfo) ->
		ModuleInfo1 = ModuleInfo0
	;
		pred_info_procids(PredInfo, ProcIds),
		detect_switches_in_procs(ProcIds, PredId, ModuleInfo0,
			ModuleInfo1)
	),
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
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_switches_in_goal'.
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo0, Goal),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

	% This version doesn't return the resulting instmap

:- pred detect_switches_in_goal(hlds__goal, instmap, module_info,
				hlds__goal).
:- mode detect_switches_in_goal(in, in, in, out) is det.

detect_switches_in_goal(Goal0 - GoalInfo, InstMap0, ModuleInfo,
			Goal - GoalInfo) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0, InstMapDelta,
			ModuleInfo, Goal).

	% This version is the same as the above except that it
	% does return the resulting instmap, which is computed
	% by applying the instmap delta specified in the goal's goalinfo.

:- pred detect_switches_in_goal(hlds__goal, instmap, module_info,
				hlds__goal, instmap).
:- mode detect_switches_in_goal(in, in, in, out, out) is det.

detect_switches_in_goal(Goal0 - GoalInfo, InstMap0, ModuleInfo,
		Goal - GoalInfo, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0, InstMapDelta,
		ModuleInfo, Goal),
	apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_switches_in_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
		instmap, module_info, hlds__goal_expr).
:- mode detect_switches_in_goal_2(in, in, in, in, in, out) is det.

detect_switches_in_goal_2(conj(Goals0), _GoalInfo, InstMap0, _InstMapDelta,
		ModuleInfo, conj(Goals)) :-
	detect_switches_in_conj(Goals0, InstMap0, ModuleInfo, Goals).

detect_switches_in_goal_2(disj(Goals0), GoalInfo, InstMap0, InstMapDelta,
		ModuleInfo, Goal) :-
	( Goals0 = [] ->
		Goal = disj([])
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo, 
			InstMap0, InstMapDelta, ModuleInfo, Goal)
	).

detect_switches_in_goal_2(not(Vars, Goal0), _GoalInfo, InstMap0, _InstMapDelta,
		ModuleInfo, not(Vars, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo, Goal).

detect_switches_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), _GoalInfo,
			InstMap0, _InstMapDelta, ModuleInfo,
			if_then_else(Vars, Cond, Then, Else)) :-
	detect_switches_in_goal(Cond0, InstMap0, ModuleInfo, Cond, InstMap1),
	detect_switches_in_goal(Then0, InstMap1, ModuleInfo, Then),
	detect_switches_in_goal(Else0, InstMap0, ModuleInfo, Else).

detect_switches_in_goal_2(some(Vars, Goal0), _GoalInfo, InstMap0, _InstMapDelta,
		ModuleInfo, some(Vars, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo, Goal).

detect_switches_in_goal_2(call(A,B,C,D,E), _, _, _, _, call(A,B,C,D,E)).

detect_switches_in_goal_2(unify(A,B,C,D,E), _, _, _, _, unify(A,B,C,D,E)).

%-----------------------------------------------------------------------------%

	% This is the interesting bit - we've found a non-empty
	% disjunction, and we've got a list of the non-local variables
	% of that disjunction.  Now for each non-local variable, we
	% check whether there is a partition of the disjuncts such that
	% each group of disjunctions can only succeed if the variable
	% is bound to a different functor.  We check this by examining
	% the instantiatedness of the variable after the disjunction.

:- pred detect_switches_in_disj(list(var), list(hlds__goal), hlds__goal_info,
		instmap, instmap, module_info, hlds__goal_expr).
:- mode detect_switches_in_disj(in, in, in, in, in, in, out) is det.

detect_switches_in_disj([], Goals0, _, InstMap, _, ModuleInfo, disj(Goals)) :-
	detect_switches_in_disj_2(Goals0, InstMap, ModuleInfo, Goals).
	
detect_switches_in_disj([Var | Vars], Goals0, GoalInfo, InstMap, InstMapDelta,
		ModuleInfo, Goal) :-
	(
		instmap_lookup_var(InstMap, Var, VarInst0),
		inst_is_bound(ModuleInfo, VarInst0),
		partition_disj(Goals0, Var, GoalInfo, Cases0)
	->
		% XXX it might be a good idea to convert switches
		% with only one case into something simpler
		detect_switches_in_cases(Cases0, InstMap, ModuleInfo,
			Cases),
		map__init(FollowVars),
		Goal = switch(Var, Cases, FollowVars)
	;
		detect_switches_in_disj(Vars, Goals0, GoalInfo,
			InstMap, InstMapDelta, ModuleInfo, Goal)
	).

%-----------------------------------------------------------------------------%

:- type cases == map(cons_id, list(hlds__goal)).

:- pred partition_disj(list(hlds__goal), var, hlds__goal_info, list(case)).
:- mode partition_disj(in, in, in, out) is semidet.

partition_disj(Goals0, Var, GoalInfo, CaseList) :-
	map__init(Cases0),
	partition_disj_2(Goals0, Var, Cases0, Cases),
	map__to_assoc_list(Cases, CasesAssocList),
	CasesAssocList \= [_],
	fix_case_list(CasesAssocList, GoalInfo, CaseList).
	
:- pred partition_disj_2(list(hlds__goal), var, cases, cases).
:- mode partition_disj_2(in, in, in, out) is semidet.

partition_disj_2([], _Var, Cases, Cases).
partition_disj_2([Goal | Goals], Var, Cases0, Cases) :-
	goal_to_conj_list(Goal, ConjList),
	find_unify_var_functor(ConjList, Var, Functor),	% may fail
	( map__search(Cases0, Functor, DisjList0) ->
		DisjList1 = [Goal | DisjList0]
	;
		DisjList1 = [Goal]
	),
	map__set(Cases0, Functor, DisjList1, Cases1),
	partition_disj_2(Goals, Var, Cases1, Cases).

:- pred find_unify_var_functor(list(hlds__goal), var, cons_id).
:- mode find_unify_var_functor(in, in, out) is semidet.

find_unify_var_functor([Goal - _GoalInfo | Goals], Var, Functor) :-
	( Goal = unify(_, _, _, UnifyInfo, _) ->
		( UnifyInfo = deconstruct(Var, Functor0, _, _, _) ->
			Functor = Functor0
		;
			find_unify_var_functor(Goals, Var, Functor)
		)
	;
		fail
	).

:- pred fix_case_list(assoc_list(cons_id, list(hlds__goal)), hlds__goal_info,
			list(case)).
:- mode fix_case_list(in, in, out) is det.

fix_case_list([], _, []).
fix_case_list([Functor - DisjList | Cases0], GoalInfo,
		[case(Functor, Goal) | Cases]) :-
	disj_list_to_goal(DisjList, GoalInfo, Goal),
	fix_case_list(Cases0, GoalInfo, Cases).

:- pred disj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode disj_list_to_goal(in, in, out) is det.

disj_list_to_goal(DisjList, GoalInfo, Goal) :-
	( DisjList = [SingleGoal] ->
		Goal = SingleGoal
	;
		Goal = disj(DisjList) - GoalInfo
	).

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_disj_2(list(hlds__goal), instmap, module_info,
				list(hlds__goal)).
:- mode detect_switches_in_disj_2(in, in, in, out) is det.

detect_switches_in_disj_2([], _InstMap, _ModuleInfo, []).
detect_switches_in_disj_2([Goal0 | Goals0], InstMap0, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo, Goal),
	detect_switches_in_disj_2(Goals0, InstMap0, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_cases(list(case), instmap, module_info,
				list(case)).
:- mode detect_switches_in_cases(in, in, in, out) is det.

detect_switches_in_cases([], _InstMap, _ModuleInfo, []).
detect_switches_in_cases([Case0 | Cases0], InstMap, ModuleInfo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_switches_in_goal(Goal0, InstMap, ModuleInfo, Goal),
	Case = case(Functor, Goal),
	detect_switches_in_cases(Cases0, InstMap, ModuleInfo, Cases).

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_conj(list(hlds__goal), instmap, module_info,
				list(hlds__goal)).
:- mode detect_switches_in_conj(in, in, in, out) is det.

detect_switches_in_conj([], _InstMap, _ModuleInfo, []).
detect_switches_in_conj([Goal0 | Goals0], InstMap0, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap0, ModuleInfo, Goal, InstMap1),
	detect_switches_in_conj(Goals0, InstMap1, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
