
:- module liveness.

:- interface.

:- import_module hlds, llds.

:- pred detect_liveness(module_info, module_info).
:- mode detect_liveness(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util.
:- import_module mode_util, term.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_liveness_in_goal'
	% for each procedure body.

detect_liveness(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	detect_liveness_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_liveness_in_preds(list(pred_id), module_info, module_info).
:- mode detect_liveness_in_preds(in, in, out) is det.

detect_liveness_in_preds([], ModuleInfo, ModuleInfo).
detect_liveness_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	( pred_info_is_imported(PredInfo) ->
		ModuleInfo1 = ModuleInfo0
	;
		pred_info_procids(PredInfo, ProcIds),
		detect_liveness_in_procs(ProcIds, PredId, ModuleInfo0,
			ModuleInfo1)
	),
	detect_liveness_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_liveness_in_procs(list(proc_id), pred_id, module_info,
					module_info).
:- mode detect_liveness_in_procs(in, in, in, out) is det.

detect_liveness_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_liveness_in_procs([ProcId | ProcIds], PredId, ModuleInfo0,
					ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_liveness_in_goal'.
	proc_info_goal(ProcInfo0, Goal0),

	detect_initial_liveness(ProcInfo0, ModuleInfo0, Liveness0),
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo0, Goal1),

	detect_initial_deadness(ProcInfo0, ModuleInfo0, Deadness0),
	detect_deadness_in_goal(Goal1, Deadness0, ModuleInfo0, Goal),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_liveness_info(ProcInfo1, Liveness0, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	detect_liveness_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				liveness_info, hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out, out) is det.

detect_liveness_in_goal(Goal0 - GoalInfo0, Liveness0, ModuleInfo,
					Liveness, Goal - GoalInfo) :-
	goal_info_delta_liveness(GoalInfo0, Delta0),
	Delta0 = _Births0 - Deaths,
	(
		detect_liveness_is_atomic(Goal0)
	->
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__difference(NonLocals, Liveness0, Births),
		set__union(Liveness0, Births, Liveness)
	;
		Liveness = Liveness0,
		set__init(Births)
	),
	Delta = Births - Deaths,
	goal_info_set_delta_liveness(GoalInfo0, Delta, GoalInfo),
	detect_liveness_in_goal_2(Goal0, Liveness, ModuleInfo, Goal).

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out) is det.

detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Goal) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, _, Goal).

	% Here we process each of the different sorts of goals.

:- pred detect_liveness_in_goal_2(hlds__goal_expr, liveness_info,
					module_info, hlds__goal_expr).
:- mode detect_liveness_in_goal_2(in, in, in, out) is det.

detect_liveness_in_goal_2(conj(Goals0), Liveness, ModuleInfo,
		conj(Goals)) :-
	detect_liveness_in_conj(Goals0, Liveness, ModuleInfo, Goals).

detect_liveness_in_goal_2(disj(Goals0), Liveness, ModuleInfo,
		disj(Goals)) :-
	detect_liveness_in_disj(Goals0, Liveness, ModuleInfo, Goals).

detect_liveness_in_goal_2(not(Vars, Goal0), Liveness, ModuleInfo,
		not(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Liveness, Goal).

detect_liveness_in_goal_2(switch(Var, Cases0, FollowVars), Liveness,
		ModuleInfo, switch(Var, Cases, FollowVars)) :-
	detect_liveness_in_cases(Cases0, Liveness, ModuleInfo, Cases).

detect_liveness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Liveness,
		ModuleInfo, if_then_else(Vars, Cond, Then, Else)) :-
	detect_liveness_in_goal(Cond0, Liveness, ModuleInfo, Liveness1, Cond),
	detect_liveness_in_goal(Then0, Liveness1, ModuleInfo, Then),
	detect_liveness_in_goal(Else0, Liveness1, ModuleInfo, Else).

detect_liveness_in_goal_2(some(Vars, Goal0), Liveness, ModuleInfo,
		some(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Goal).

detect_liveness_in_goal_2(all(Vars, Goal0), Liveness, ModuleInfo,
							all(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Goal).

detect_liveness_in_goal_2(call(A,B,C,D), _, _, call(A,B,C,D)).

detect_liveness_in_goal_2(unify(A,B,C,D,E), _, _, unify(A,B,C,D,E)).

:- pred detect_liveness_in_conj(list(hlds__goal), set(var), module_info,
							list(hlds__goal)).
:- mode detect_liveness_in_conj(in, in, in, out) is det.

detect_liveness_in_conj([], _Liveness, _ModuleInfo, []).
detect_liveness_in_conj([Goal0|Goals0], Liveness, ModuleInfo, [Goal|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Liveness1, Goal),
	detect_liveness_in_conj(Goals0, Liveness1, ModuleInfo, Goals).

:- pred detect_liveness_in_disj(list(hlds__goal), set(var), module_info,
							list(hlds__goal)).
:- mode detect_liveness_in_disj(in, in, in, out) is det.

detect_liveness_in_disj([], _Liveness, _ModuleInfo, []).
detect_liveness_in_disj([Goal0|Goals0], Liveness, ModuleInfo, [Goal|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Goal),
	detect_liveness_in_disj(Goals0, Liveness, ModuleInfo, Goals).

:- pred detect_liveness_in_cases(list(case), set(var), module_info, list(case)).
:- mode detect_liveness_in_cases(in, in, in, out) is det.

detect_liveness_in_cases([], _Liveness, _ModuleInfo, []).
detect_liveness_in_cases([case(Cons, Goal0)|Goals0], Liveness, ModuleInfo,
						[case(Cons, Goal)|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Goal),
	detect_liveness_in_cases(Goals0, Liveness, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_goal(hlds__goal, liveness_info, module_info,
				liveness_info, hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, out, out) is det.

detect_deadness_in_goal(Goal0 - GoalInfo0, Deadness0, ModuleInfo,
					Deadness, Goal - GoalInfo) :-
	goal_info_delta_liveness(GoalInfo0, Delta0),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	Delta0 = Births - _Deaths0,
	(
		detect_liveness_is_atomic(Goal0)
	->
		set__difference(NonLocals, Deadness0, Deaths),
		set__union(Deadness0, Deaths, Deadness),
		Goal = Goal0
	;
		set__union(Deadness0, NonLocals, Deadness),
		set__init(Deaths),
		detect_deadness_in_goal_2(Goal0, Deadness0, ModuleInfo, Goal)
	),
	Delta = Births - Deaths,
	goal_info_set_delta_liveness(GoalInfo0, Delta, GoalInfo).

:- pred detect_deadness_in_goal(hlds__goal, liveness_info, module_info,
								hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, out) is det.

detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, _, Goal).

	% Here we process each of the different sorts of goals.

:- pred detect_deadness_in_goal_2(hlds__goal_expr, liveness_info,
					module_info, hlds__goal_expr).
:- mode detect_deadness_in_goal_2(in, in, in, out) is det.

detect_deadness_in_goal_2(conj(Goals0), Deadness, ModuleInfo, conj(Goals)) :-
	detect_deadness_in_conj(Goals0, Deadness, ModuleInfo, Goals, _).

detect_deadness_in_goal_2(disj(Goals0), Deadness, ModuleInfo,
		disj(Goals)) :-
	detect_deadness_in_disj(Goals0, Deadness, ModuleInfo, Goals).

detect_deadness_in_goal_2(not(Vars, Goal0), Deadness, ModuleInfo,
		not(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Deadness, Goal).

detect_deadness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Deadness,
		ModuleInfo, if_then_else(Vars, Cond, Then, Else)) :-
	detect_deadness_in_goal(Then0, Deadness, ModuleInfo, Deadness1, Then1),
	detect_deadness_in_goal(Else0, Deadness, ModuleInfo, Deadness2, Else1),
	set__union(Deadness1, Deadness2, Deadness3),
	detect_deadness_in_goal(Cond0, Deadness3, ModuleInfo, Cond),
	set__intersect(Deadness1, Deadness2, Residue0),
	set__difference(Deadness3, Residue0, Residue),
	stuff_residue_into_goal(Then1, Residue, Then),
	stuff_residue_into_goal(Else1, Residue, Else).

detect_deadness_in_goal_2(switch(Var, Cases0, FollowVars), Deadness,
		ModuleInfo, switch(Var, Cases, FollowVars)) :-
	set__init(Union0),
	detect_deadness_in_cases(Cases0, Deadness, ModuleInfo, Union0,
						Union, Intersection, Cases1),
	set__difference(Union, Intersection, Residue),
	stuff_residue_into_cases(Cases1, Residue, Cases).
	
detect_deadness_in_goal_2(some(Vars, Goal0), Deadness, ModuleInfo,
		some(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal).

detect_deadness_in_goal_2(all(Vars, Goal0), Deadness, ModuleInfo,
						all(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal).

detect_deadness_in_goal_2(call(A,B,C,D), _, _, call(A,B,C,D)).

detect_deadness_in_goal_2(unify(A,B,C,D,E), _, _, unify(A,B,C,D,E)).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_conj(list(hlds__goal), set(var), module_info,
						list(hlds__goal), set(var)).
:- mode detect_deadness_in_conj(in, in, in, out, out) is det.

detect_deadness_in_conj([], Deadness, _ModuleInfo, [], Deadness).
detect_deadness_in_conj([Goal0|Goals0], Deadness0, ModuleInfo,
						[Goal|Goals], Deadness) :-
	detect_deadness_in_conj(Goals0, Deadness0, ModuleInfo,
						Goals, Deadness1),
	detect_deadness_in_goal(Goal0, Deadness1, ModuleInfo, Deadness, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_disj(list(hlds__goal), set(var), module_info,
							list(hlds__goal)).
:- mode detect_deadness_in_disj(in, in, in, out) is det.

detect_deadness_in_disj([], _Deadness, _ModuleInfo, []).
detect_deadness_in_disj([Goal0|Goals0], Deadness, ModuleInfo, [Goal|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal),
	detect_deadness_in_disj(Goals0, Deadness, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_cases(list(case), set(var), module_info, set(var),
						set(var), set(var), list(case)).
:- mode detect_deadness_in_cases(in, in, in, in, out, out, out) is det.

detect_deadness_in_cases([], _Deadness, _ModuleInfo, Union, Union, Union, []).
detect_deadness_in_cases([case(Cons, Goal0)|Goals0], Deadness0, ModuleInfo,
		Union0, Union, Intersection, [case(Cons, Goal)|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, Deadness1, Goal),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_cases(Goals0, Deadness0, ModuleInfo, Union1, Union,
							Intersection0, Goals),
	set__intersect(Intersection0, Deadness1, Intersection).

%-----------------------------------------------------------------------------%

:- pred detect_initial_liveness(proc_info, module_info, set(var)).
:- mode detect_initial_liveness(in, in, out) is det.

detect_initial_liveness(ProcInfo, ModuleInfo, Liveness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Args),
	assoc_list__from_corresponding_lists(Vars, Args, VarArgs),
	set__init(Liveness0),
	detect_initial_liveness_2(VarArgs, ModuleInfo, Liveness0, Liveness).

:- pred detect_initial_liveness_2(assoc_list(var,mode), module_info,
							set(var), set(var)).
:- mode detect_initial_liveness_2(in, in, in, out) is det.

detect_initial_liveness_2([], _ModuleInfo, Liveness, Liveness).
detect_initial_liveness_2([V - M|VAs], ModuleInfo,
						Liveness0, Liveness) :-
	(
		mode_is_input(ModuleInfo, M)
	->
		set__insert(Liveness0, V, Liveness1)
	;
		Liveness1 = Liveness0
	),
	detect_initial_liveness_2(VAs, ModuleInfo, Liveness1, Liveness).

%-----------------------------------------------------------------------------%

:- pred detect_initial_deadness(proc_info, module_info, set(var)).
:- mode detect_initial_deadness(in, in, out) is det.

detect_initial_deadness(ProcInfo, ModuleInfo, Deadness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Args),
	assoc_list__from_corresponding_lists(Vars, Args, VarArgs),
	set__init(Deadness0),
	detect_initial_deadness_2(VarArgs, ModuleInfo, Deadness0, Deadness).

:- pred detect_initial_deadness_2(assoc_list(var,mode), module_info,
							set(var), set(var)).
:- mode detect_initial_deadness_2(in, in, in, out) is det.

detect_initial_deadness_2([], _ModuleInfo, Deadness, Deadness).
detect_initial_deadness_2([V - M|VAs], ModuleInfo, Deadness0, Deadness) :-
	(
		mode_is_output(ModuleInfo, M)
	->
		set__insert(Deadness0, V, Deadness1)
	;
		Deadness1 = Deadness0
	),
	detect_initial_deadness_2(VAs, ModuleInfo, Deadness1, Deadness).

%-----------------------------------------------------------------------------%

:- pred stuff_residue_into_goal(hlds__goal, liveness_info, hlds__goal).
:- mode stuff_residue_into_goal(in, in, out) is det.

stuff_residue_into_goal(Goal0 - GoalInfo0, Residue, Goal - GoalInfo) :-
	stuff_residue_into_goal_2(Goal0, GoalInfo0, Residue, Goal, GoalInfo).

:- pred stuff_residue_into_goal_2(hlds__goal_expr, hlds__goal_info,
			liveness_info, hlds__goal_expr, hlds__goal_info).
:- mode stuff_residue_into_goal_2(in, in, in, out, out) is det.

stuff_residue_into_goal_2(conj(Goals0), GoalInfo, Residue,
						conj(Goals), GoalInfo) :-
		stuff_residue_into_conj(Goals0, Residue, Goals).
stuff_residue_into_goal_2(disj(Goals0), GoalInfo, Residue,
						disj(Goals), GoalInfo) :-
		stuff_residue_into_disj(Goals0, Residue, Goals).
stuff_residue_into_goal_2(switch(Var, Cases0, FollowVars), GoalInfo, Residue,
				switch(Var, Cases, FollowVars), GoalInfo) :-
		stuff_residue_into_cases(Cases0, Residue, Cases).
stuff_residue_into_goal_2(call(A, B, C, D), GoalInfo0, Residue,
						call(A, B, C, D), GoalInfo) :-
	goal_info_delta_liveness(GoalInfo0, Delta0),
	Delta0 = Births - Deaths0,
	set__union(Deaths0, Residue, Deaths),
	Delta = Births - Deaths,
	goal_info_set_delta_liveness(GoalInfo0, Delta, GoalInfo).
stuff_residue_into_goal_2(unify(A, B, C, D, E), GoalInfo0, Residue,
					unify(A, B, C, D, E), GoalInfo) :-
	goal_info_delta_liveness(GoalInfo0, Delta0),
	Delta0 = Births - Deaths0,
	set__union(Deaths0, Residue, Deaths),
	Delta = Births - Deaths,
	goal_info_set_delta_liveness(GoalInfo0, Delta, GoalInfo).
stuff_residue_into_goal_2(if_then_else(A, B, C0, D0), GoalInfo, Residue,
					if_then_else(A, B, C, D), GoalInfo) :-
	stuff_residue_into_goal(C0, Residue, C),
	stuff_residue_into_goal(D0, Residue, D).
stuff_residue_into_goal_2(not(A, B0), GoalInfo, Residue, not(A, B), GoalInfo) :-
	stuff_residue_into_goal(B0, Residue, B).
stuff_residue_into_goal_2(all(A, B0), GoalInfo, Residue, all(A, B), GoalInfo) :-
	stuff_residue_into_goal(B0, Residue, B).
stuff_residue_into_goal_2(some(A, B0), GoalInfo, Residue,
						some(A, B), GoalInfo) :-
	stuff_residue_into_goal(B0, Residue, B).

%-----------------------------------------------------------------------------%

:- pred stuff_residue_into_conj(list(hlds__goal), liveness_info,
							list(hlds__goal)).
:- mode stuff_residue_into_conj(in, in, out) is det.

stuff_residue_into_conj([], _Residue, []).
stuff_residue_into_conj([G0|Gs0], Residue0, [G|Gs]) :-
	(
		Gs0 = [_|_]
	->
		G = G0,
		reduce_residue(G, Residue0, Residue1),
		stuff_residue_into_conj(Gs0, Residue1, Gs)
	;
		stuff_residue_into_goal(G0, Residue0, G),
		Gs = Gs0
	).

%-----------------------------------------------------------------------------%

:- pred stuff_residue_into_disj(list(hlds__goal), liveness_info,
							list(hlds__goal)).
:- mode stuff_residue_into_disj(in, in, out) is det.

stuff_residue_into_disj([], _Residue, []).
stuff_residue_into_disj([G0|Gs0], Residue, [G|Gs]) :-
	stuff_residue_into_disj(Gs0, Residue, Gs),
	stuff_residue_into_goal(G0, Residue, G).

%-----------------------------------------------------------------------------%

:- pred stuff_residue_into_cases(list(case), set(var), list(case)).
:- mode stuff_residue_into_cases(in, in, out) is det.

stuff_residue_into_cases([], _Residue, []).
stuff_residue_into_cases([case(Cons, Goal0)|Goals0], Residue,
						[case(Cons,Goal)|Goals]) :-
	stuff_residue_into_goal(Goal0, Residue, Goal),
	stuff_residue_into_cases(Goals0, Residue, Goals).

%-----------------------------------------------------------------------------%

:- pred reduce_residue(hlds__goal, liveness_info, liveness_info).
:- mode reduce_residue(in, in, out) is det.

reduce_residue(Goal - GoalInfo, Residue0, Residue) :-
	goal_info_delta_liveness(GoalInfo, _Births - Deaths),
	set__difference(Residue0, Deaths, Residue1),
	(
		detect_liveness_is_atomic(Goal)
	->
		Residue = Residue1
	;
		reduce_residue_2(Goal, Residue1, Residue)
	).

:- pred reduce_residue_2(hlds__goal_expr, liveness_info, liveness_info).
:- mode reduce_residue_2(in, in, out) is det.

reduce_residue_2(conj(Goals), Residue0, Residue) :-
	reduce_residue_conj(Goals, Residue0, Residue).
reduce_residue_2(disj(Goals), Residue0, Residue) :-
	reduce_residue_disj(Goals, Residue0, Residue).
reduce_residue_2(switch(_Var, Cases, _FollowVars), Residue0, Residue) :-
	reduce_residue_cases(Cases, Residue0, Residue).
reduce_residue_2(not(_Var, Goal), Residue0, Residue) :-
	reduce_residue(Goal, Residue0, Residue).
reduce_residue_2(some(_Var, Goal), Residue0, Residue) :-
	reduce_residue(Goal, Residue0, Residue).
reduce_residue_2(all(_Var, Goal), Residue0, Residue) :-
	reduce_residue(Goal, Residue0, Residue).
reduce_residue_2(if_then_else(_Var, A, B, C), Residue0, Residue) :-
	reduce_residue(A, Residue0, Residue1),
	reduce_residue(B, Residue1, Residue2),
	reduce_residue(C, Residue2, Residue).
reduce_residue_2(call(_, _, _, _), Residue, Residue).
reduce_residue_2(unify(_, _, _, _, _), Residue, Residue).

:- pred reduce_residue_conj(list(hlds__goal), liveness_info, liveness_info).
:- mode reduce_residue_conj(in, in, out) is det.

reduce_residue_conj([], Residue, Residue).
reduce_residue_conj([G|Gs], Residue0, Residue) :-
	reduce_residue(G, Residue0, Residue1),
	reduce_residue_conj(Gs, Residue1, Residue).

:- pred reduce_residue_disj(list(hlds__goal), liveness_info, liveness_info).
:- mode reduce_residue_disj(in, in, out) is det.

reduce_residue_disj([], Residue, Residue).
reduce_residue_disj([G|Gs], Residue0, Residue) :-
	reduce_residue(G, Residue0, Residue1),
	reduce_residue_disj(Gs, Residue1, Residue).

:- pred reduce_residue_cases(list(case), liveness_info, liveness_info).
:- mode reduce_residue_cases(in, in, out) is det.

reduce_residue_cases([], Residue, Residue).
reduce_residue_cases([case(_Var, G)|Cs], Residue0, Residue) :-
	reduce_residue(G, Residue0, Residue1),
	reduce_residue_cases(Cs, Residue1, Residue).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_is_atomic(hlds__goal_expr).
:- mode detect_liveness_is_atomic(in) is semidet.

detect_liveness_is_atomic(call(_,_,_,_)).
detect_liveness_is_atomic(unify(_,_,_,_,_)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
