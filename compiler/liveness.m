%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module liveness.
% Main author: conway.

	% This module traverses the goal for each procedure, and adds
	% liveness annotations to the goal_info for each sub-goal.

	% Note - the concept of `liveness' here is different to that
	% used in the mode analysis.  The mode analysis is concerned
	% with the liveness of what is *pointed* to by a variable, for
	% the purpose of avoiding aliasing and for structure re-use
	% optimization, whereas here we are concerned with the liveness
	% of the variable itself, for the purposes of minimizing stack
	% slot usage and for register re-use.

	% XXX - we will need a different algorithm for determining liveness
	% for non-deterministic code.  (That may also cause some
	% difficulties for structure re-use, too!)

%-----------------------------------------------------------------------------%

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
%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				liveness_info, hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out, out) is det.

detect_liveness_in_goal(Goal0 - GoalInfo0, Liveness0, ModuleInfo,
					Liveness, Goal - GoalInfo) :-
	goal_info_pre_delta_liveness(GoalInfo0, Delta0),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	Delta0 = _Births0 - Deaths,
	(
		detect_liveness_is_atomic(Goal0)
	->
		set__difference(NonLocals, Liveness0, Births),
		set__union(Liveness0, Births, Liveness),
		Goal = Goal0
	;
		set__union(Liveness0, NonLocals, Liveness),
		set__init(Births),
		detect_liveness_in_goal_2(Goal0, Liveness0, ModuleInfo, Goal)
	),
	Delta = Births - Deaths,
	goal_info_set_pre_delta_liveness(GoalInfo0, Delta, GoalInfo).

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out) is det.

detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Goal) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, _, Goal).

	% Here we process each of the different sorts of goals.

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal_2(hlds__goal_expr, liveness_info,
					module_info, hlds__goal_expr).
:- mode detect_liveness_in_goal_2(in, in, in, out) is det.

detect_liveness_in_goal_2(conj(Goals0), Liveness, ModuleInfo,
		conj(Goals)) :-
	detect_liveness_in_conj(Goals0, Liveness, ModuleInfo, Goals).

detect_liveness_in_goal_2(disj(Goals0), Liveness, ModuleInfo,
		disj(Goals)) :-
	set__init(Union),
	detect_liveness_in_disj(Goals0, Liveness, ModuleInfo, Union, _, Goals).

detect_liveness_in_goal_2(not(Vars, Goal0), Liveness, ModuleInfo,
		not(Vars, Goal)) :- % XXX is this quite correct?
			% where q:- not(p(X)), r(X). and p does not
			% bind X.
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Goal).

detect_liveness_in_goal_2(switch(Var, Cases0), Liveness,
				ModuleInfo, switch(Var, Cases)) :-
	set__init(Union),
	detect_liveness_in_cases(Cases0, Liveness, ModuleInfo, Union, _, Cases).

detect_liveness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Liveness0,
		ModuleInfo, if_then_else(Vars, Cond, Then, Else)) :-
	detect_liveness_in_goal(Cond0, Liveness0, ModuleInfo, Liveness1, Cond),
	detect_liveness_in_goal(Then0, Liveness1, ModuleInfo, Liveness2, Then1),
	detect_liveness_in_goal(Else0, Liveness0, ModuleInfo, Liveness3, Else1),
	set__difference(Liveness3, Liveness2, Residue0),
	stuff_liveness_residue_into_goal(Then1, Residue0, Then),
	set__difference(Liveness2, Liveness3, Residue1),
	stuff_liveness_residue_into_goal(Else1, Residue1, Else).

detect_liveness_in_goal_2(some(Vars, Goal0), Liveness, ModuleInfo,
		some(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Goal).

detect_liveness_in_goal_2(call(A,B,C,D,E,F), _, _, call(A,B,C,D,E,F)).

detect_liveness_in_goal_2(unify(A,B,C,D,E), _, _, unify(A,B,C,D,E)).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_conj(list(hlds__goal), set(var), module_info,
							list(hlds__goal)).
:- mode detect_liveness_in_conj(in, in, in, out) is det.

detect_liveness_in_conj([], _Liveness, _ModuleInfo, []).
detect_liveness_in_conj([Goal0|Goals0], Liveness, ModuleInfo, [Goal|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Liveness1, Goal),
	detect_liveness_in_conj(Goals0, Liveness1, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_disj(list(hlds__goal), set(var), module_info,
					set(var), set(var), list(hlds__goal)).
:- mode detect_liveness_in_disj(in, in, in, in, out, out) is det.

detect_liveness_in_disj([], _Liveness, _ModuleInfo, Union, Union, []).
detect_liveness_in_disj([Goal0|Goals0], Liveness, ModuleInfo,
						Union0, Union, [Goal|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Liveness1, Goal1),
	set__union(Union0, Liveness1, Union1),
	detect_liveness_in_disj(Goals0, Liveness, ModuleInfo,
							Union1, Union, Goals),
	set__difference(Liveness1, Union, Residue),
	stuff_liveness_residue_into_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_cases(list(case), set(var), module_info,
					set(var), set(var), list(case)).
:- mode detect_liveness_in_cases(in, in, in, in, out, out) is det.

detect_liveness_in_cases([], _Liveness, _ModuleInfo, Union, Union, []).
detect_liveness_in_cases([case(Cons, Goal0)|Goals0], Liveness, ModuleInfo,
				Union0, Union, [case(Cons, Goal)|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness, ModuleInfo, Liveness1, Goal1),
	set__union(Union0, Liveness1, Union1),
	detect_liveness_in_cases(Goals0, Liveness, ModuleInfo,
							Union1, Union, Goals),
	set__difference(Liveness1, Union, Residue),
	stuff_liveness_residue_into_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_goal(hlds__goal, liveness_info, module_info,
				liveness_info, hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, out, out) is det.

detect_deadness_in_goal(Goal0 - GoalInfo0, Deadness0, ModuleInfo,
					Deadness, Goal - GoalInfo) :-
	goal_info_post_delta_liveness(GoalInfo0, Delta0),
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
	goal_info_set_post_delta_liveness(GoalInfo0, Delta, GoalInfo).

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

detect_deadness_in_goal_2(disj(Goals0), Deadness, ModuleInfo, disj(Goals)) :-
	set__init(Union0),
	detect_deadness_in_disj(Goals0, Deadness, ModuleInfo, Union0,
								_, Goals).

detect_deadness_in_goal_2(not(Vars, Goal0), Deadness, ModuleInfo,
		not(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal).

detect_deadness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Deadness0,
		ModuleInfo, if_then_else(Vars, Cond, Then, Else)) :-
	detect_deadness_in_goal(Then0, Deadness0, ModuleInfo, Deadness1, Then1),
	detect_deadness_in_goal(Else0, Deadness0, ModuleInfo, Deadness2, Else1),
	set__union(Deadness1, Deadness2, Deadness3),
	detect_deadness_in_goal(Cond0, Deadness3, ModuleInfo, Cond),
	set__difference(Deadness2, Deadness1, Residue0),
	stuff_deadness_residue_into_goal(Then1, Residue0, Then),
	set__difference(Deadness1, Deadness2, Residue1),
	stuff_deadness_residue_into_goal(Else1, Residue1, Else).

detect_deadness_in_goal_2(switch(Var, Cases0), Deadness,
				ModuleInfo, switch(Var, Cases)) :-
	set__init(Union0),
	detect_deadness_in_cases(Cases0, Deadness, ModuleInfo, Union0,
								_, Cases).
	
detect_deadness_in_goal_2(some(Vars, Goal0), Deadness, ModuleInfo,
		some(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal).

detect_deadness_in_goal_2(call(A,B,C,D,E,F), _, _, call(A,B,C,D,E,F)).

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
					set(var), set(var), list(hlds__goal)).
:- mode detect_deadness_in_disj(in, in, in, in, out, out) is det.

detect_deadness_in_disj([], _Deadness, _ModuleInfo, Union, Union, []).
detect_deadness_in_disj([Goal0|Goals0], Deadness, ModuleInfo,
						Union0, Union, [Goal|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_disj(Goals0, Deadness, ModuleInfo,
							Union1, Union, Goals),
	set__difference(Deadness1, Union, Residue),
	stuff_deadness_residue_into_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_cases(list(case), set(var), module_info, set(var),
						set(var), list(case)).
:- mode detect_deadness_in_cases(in, in, in, in, out, out) is det.

detect_deadness_in_cases([], _Deadness, _ModuleInfo, Union, Union, []).
detect_deadness_in_cases([case(Cons, Goal0)|Goals0], Deadness0, ModuleInfo,
				Union0, Union, [case(Cons, Goal)|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_cases(Goals0, Deadness0, ModuleInfo,
							Union1, Union, Goals),
	set__difference(Deadness1, Union, Residue),
	stuff_deadness_residue_into_goal(Goal1, Residue, Goal).

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
%-----------------------------------------------------------------------------%

:- pred stuff_liveness_residue_into_goal(hlds__goal, liveness_info, hlds__goal).
:- mode stuff_liveness_residue_into_goal(in, in, out) is det.

stuff_liveness_residue_into_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_post_delta_liveness(GoalInfo0, Births0 - Deaths),
	set__union(Births0, Residue, Births),
	goal_info_set_post_delta_liveness(GoalInfo0, Births - Deaths, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred stuff_deadness_residue_into_goal(hlds__goal, liveness_info, hlds__goal).
:- mode stuff_deadness_residue_into_goal(in, in, out) is det.

stuff_deadness_residue_into_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_pre_delta_liveness(GoalInfo0, Births - Deaths0),
	set__union(Deaths0, Residue, Deaths),
	goal_info_set_pre_delta_liveness(GoalInfo0, Births - Deaths, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_liveness_is_atomic(hlds__goal_expr).
:- mode detect_liveness_is_atomic(in) is semidet.

detect_liveness_is_atomic(conj([])).
detect_liveness_is_atomic(disj([])).
detect_liveness_is_atomic(call(_,_,_,_,_,_)).
detect_liveness_is_atomic(unify(_,_,_,_,_)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
