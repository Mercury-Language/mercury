%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- module liveness.

:- interface.

:- import_module hlds, llds.

:- pred detect_liveness(module_info, module_info).
:- mode detect_liveness(in, out) is det.

:- pred detect_liveness_proc(proc_info, module_info, proc_info).
% :- mode detect_liveness_proc(di, in, uo) is det.
:- mode detect_liveness_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util.
:- import_module mode_util, term, quantification.

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
	pred_info_non_imported_procids(PredInfo, ProcIds),
	detect_liveness_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	detect_liveness_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_liveness_in_procs(list(proc_id), pred_id,
	module_info, module_info).
:- mode detect_liveness_in_procs(in, in, in, out) is det.

detect_liveness_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_liveness_in_procs([ProcId | ProcIds], PredId, ModuleInfo0, ModuleInfo) :-
	detect_liveness_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1),
	detect_liveness_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

:- pred detect_liveness_in_proc(proc_id, pred_id, module_info, module_info).
:- mode detect_liveness_in_proc(in, in, in, out) is det.

detect_liveness_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	detect_liveness_proc(ProcInfo0, ModuleInfo0, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo).

	% To process each ProcInfo, we get the goal,
	% initialize the instmap based on the modes of the head vars,
	% and pass these to `detect_liveness_in_goal'.

detect_liveness_proc(ProcInfo0, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),

	detect_initial_liveness(ProcInfo0, ModuleInfo, Liveness0),
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Goal1),

	detect_initial_deadness(ProcInfo0, ModuleInfo, Deadness0),
	detect_deadness_in_goal(Goal1, Deadness0, ModuleInfo, Goal2),

	set__init(Extras0),
	add_nondet_lives_to_goal(Goal2, Liveness0, Extras0, Goal, _, _),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_liveness_info(ProcInfo1, Liveness0, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				liveness_info, hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out, out) is det.

detect_liveness_in_goal(Goal0 - GoalInfo0, Liveness0, ModuleInfo,
					Liveness, Goal - GoalInfo) :-
	goal_info_pre_delta_liveness(GoalInfo0, PreDelta0),
	goal_info_post_delta_liveness(GoalInfo0, PostDelta0),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	PreDelta0 = _PreBirths0 - PreDeaths,
	PostDelta0 = _PostBirths0 - PostDeaths,
		% work out which variables get born in this goal
	set__difference(NonLocals, Liveness0, NewVarsSet),
	set__to_sorted_list(NewVarsSet, NewVarsList),
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	set__init(Births0),
	find_binding_occurrences(NewVarsList, ModuleInfo, InstMapDelta,
		Births0, Births),
	set__union(Liveness0, Births, Liveness),
	(
		goal_is_atomic(Goal0)
	->
		PreBirths = Births,
		Goal = Goal0,
		PostDelta = PostDelta0
	;
		set__init(PreBirths),
		detect_liveness_in_goal_2(Goal0, Liveness0,
						ModuleInfo, Liveness1, Goal),
		set__difference(Births, Liveness1, PostBirths),
		PostDelta = PostBirths - PostDeaths
	),
	PreDelta = PreBirths - PreDeaths,
	goal_info_set_pre_delta_liveness(GoalInfo0, PreDelta, GoalInfo1),
	goal_info_set_post_delta_liveness(GoalInfo1, PostDelta, GoalInfo).

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out) is det.

detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Goal) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, _, Goal).

	% Here we process each of the different sorts of goals.

%-----------------------------------------------------------------------------%

	% Given a list of variables and an instmap delta, determine
	% which of those variables become bound (according to the instmap
	% delta) and insert them into the accumulated set of bound vars.

:- pred find_binding_occurrences(list(var), module_info, instmap_delta,
				set(var), set(var)).
:- mode find_binding_occurrences(in, in, in, in, out) is det.

find_binding_occurrences([], _, _, BoundVars, BoundVars).
find_binding_occurrences([Var | Vars], ModuleInfo, InstMapDelta, BoundVars0,
		BoundVars) :-
	instmap_lookup_var(InstMapDelta, Var, Inst),
	( inst_is_bound(ModuleInfo, Inst) ->
		set__insert(BoundVars0, Var, BoundVars1)
	;
		BoundVars1 = BoundVars0
	),
	find_binding_occurrences(Vars, ModuleInfo, InstMapDelta, BoundVars1,
		BoundVars).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_goal_2(hlds__goal_expr, liveness_info,
				module_info, liveness_info, hlds__goal_expr).
:- mode detect_liveness_in_goal_2(in, in, in, out, out) is det.

detect_liveness_in_goal_2(conj(Goals0), Liveness0, ModuleInfo,
		Liveness, conj(Goals)) :-
	detect_liveness_in_conj(Goals0, Liveness0, ModuleInfo, Liveness, Goals).

detect_liveness_in_goal_2(disj(Goals0), Liveness0, ModuleInfo,
		Liveness, disj(Goals)) :-
	set__init(Union0),
	detect_liveness_in_disj(Goals0, Liveness0, ModuleInfo,
							Union0, Union, Goals),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(not(Goal0), Liveness0, ModuleInfo,
		Liveness, not(Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Liveness, Goal).

detect_liveness_in_goal_2(switch(Var, Det, Cases0), Liveness0,
			ModuleInfo, Liveness, switch(Var, Det, Cases)) :-
	set__init(Union0),
	detect_liveness_in_cases(Cases0, Liveness0, ModuleInfo,
							Union0, Union, Cases),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Liveness0,
		M, Liveness, if_then_else(Vars, Cond, Then, Else)) :-
	detect_liveness_in_goal(Cond0, Liveness0, M, LivenessCond, Cond),
	detect_liveness_in_goal(Then0, LivenessCond, M, LivenessThen, Then1),
	detect_liveness_in_goal(Else0, Liveness0, M, LivenessElse, Else1),

	set__difference(LivenessThen, LivenessCond, ProducedInThen),
	set__difference(LivenessElse, Liveness0, ProducedInElse),

	set__difference(ProducedInElse, ProducedInThen, ResidueThen),
	set__difference(ProducedInThen, ProducedInElse, ResidueElse),

	stuff_liveness_residue_into_goal(Then1, ResidueThen, Then),
	stuff_liveness_residue_into_goal(Else1, ResidueElse, Else),

	set__union(LivenessThen, LivenessElse, Liveness).

detect_liveness_in_goal_2(some(Vars, Goal0), Liveness0, ModuleInfo,
		Liveness, some(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Liveness, Goal).

detect_liveness_in_goal_2(call(A,B,C,D,E,F,G), L, _, L, call(A,B,C,D,E,F,G)).

detect_liveness_in_goal_2(unify(A,B,C,D,E), L, _, L, unify(A,B,C,D,E)).

%-----------------------------------------------------------------------------%

:- pred detect_liveness_in_conj(list(hlds__goal), set(var), module_info,
						set(var), list(hlds__goal)).
:- mode detect_liveness_in_conj(in, in, in, out, out) is det.

detect_liveness_in_conj([], Liveness, _ModuleInfo, Liveness, []).
detect_liveness_in_conj([Goal0|Goals0], Liveness0,
					ModuleInfo, Liveness, [Goal|Goals]) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Liveness1, Goal),
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, unreachable)
	->
		Goals = Goals0,
		Liveness = Liveness1
	;
		detect_liveness_in_conj(Goals0, Liveness1,
						ModuleInfo, Liveness, Goals)
	).

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
	set__difference(Union, Liveness1, Residue),
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
	set__difference(Union, Liveness1, Residue),
	stuff_liveness_residue_into_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_goal(hlds__goal, liveness_info, module_info,
				liveness_info, hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, out, out) is det.

detect_deadness_in_goal(Goal0 - GoalInfo0, Deadness0, ModuleInfo,
					Deadness, Goal - GoalInfo) :-
	goal_info_post_delta_liveness(GoalInfo0, PostDelta0),
	goal_info_pre_delta_liveness(GoalInfo0, PreDelta0),
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	PostDelta0 = PostBirths - _PostDeaths0,
	PreDelta0 = PreBirths - _PreDeaths0,
	(
		goal_is_atomic(Goal0)
	->
		set__difference(NonLocals, Deadness0, PostDeaths),
		set__union(Deadness0, PostDeaths, Deadness),
		Goal = Goal0,
		PreDelta = PreDelta0
	;
		set__union(Deadness0, NonLocals, Deadness),
		set__init(PostDeaths),
		detect_deadness_in_goal_2(Goal0, Deadness0,
						ModuleInfo, Deadness1, Goal),
		set__difference(Deadness, Deadness1, PreDeaths),
		PreDelta = PreBirths - PreDeaths
	),
	PostDelta = PostBirths - PostDeaths,
	goal_info_set_post_delta_liveness(GoalInfo0, PostDelta, GoalInfo1),
	goal_info_set_pre_delta_liveness(GoalInfo1, PreDelta, GoalInfo).

:- pred detect_deadness_in_goal(hlds__goal, liveness_info, module_info,
						hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, out) is det.

detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, Goal) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, _, Goal).

	% Here we process each of the different sorts of goals.

:- pred detect_deadness_in_goal_2(hlds__goal_expr, liveness_info,
				module_info, liveness_info, hlds__goal_expr).
:- mode detect_deadness_in_goal_2(in, in, in, out, out) is det.

detect_deadness_in_goal_2(conj(Goals0), Deadness0, ModuleInfo,
						Deadness, conj(Goals)) :-
	detect_deadness_in_conj(Goals0, Deadness0,
						ModuleInfo, Goals, Deadness).

detect_deadness_in_goal_2(disj(Goals0), Deadness0,
					ModuleInfo, Deadness, disj(Goals)) :-
	set__init(Union0),
	detect_deadness_in_disj(Goals0, Deadness0, ModuleInfo, Union0,
								Union, Goals),
	set__union(Deadness0, Union, Deadness).

detect_deadness_in_goal_2(not(Goal0), Deadness0, ModuleInfo,
		Deadness, not(Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, Deadness, Goal).

detect_deadness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Deadness0,
		ModuleInfo, Deadness, if_then_else(Vars, Cond, Then, Else)) :-
	detect_deadness_in_goal(Then0, Deadness0, ModuleInfo,
					DeadnessThen, Then1),
	detect_deadness_in_goal(Else0, Deadness0, ModuleInfo,
					DeadnessElse, Else1),
	set__union(DeadnessThen, DeadnessElse, DeadnessThenElse),
	detect_deadness_in_goal(Cond0, DeadnessThenElse, ModuleInfo,
					Deadness, Cond),
	set__difference(DeadnessElse, DeadnessThen, ResidueThen),
	stuff_deadness_residue_into_goal(Then1, ResidueThen, Then),
	set__difference(DeadnessThen, DeadnessElse, ResidueElse),
	stuff_deadness_residue_into_goal(Else1, ResidueElse, Else).

detect_deadness_in_goal_2(switch(Var, Det, Cases0), Deadness0,
			ModuleInfo, Deadness, switch(Var, Det, Cases)) :-
	set__init(Union0),
	detect_deadness_in_cases(Cases0, Deadness0, ModuleInfo, Union0,
								Union, Cases),
	set__union(Deadness0, Union, Deadness).

detect_deadness_in_goal_2(some(Vars, Goal0), Deadness0, ModuleInfo,
		Deadness, some(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, Deadness, Goal).

detect_deadness_in_goal_2(call(A,B,C,D,E,F,G), Dn, _, Dn, call(A,B,C,D,E,F,G)).

detect_deadness_in_goal_2(unify(A,B,C,D,E), Dn, _, Dn, unify(A,B,C,D,E)).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_conj(list(hlds__goal), set(var), module_info,
						list(hlds__goal), set(var)).
:- mode detect_deadness_in_conj(in, in, in, out, out) is det.

detect_deadness_in_conj([], Deadness, _ModuleInfo, [], Deadness).
detect_deadness_in_conj([Goal0|Goals0], Deadness0, ModuleInfo,
						[Goal|Goals], Deadness) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, unreachable)
	->
		Goals = Goals0,
		detect_deadness_in_goal(Goal0, Deadness0,
						ModuleInfo, Deadness, Goal)
	;
		detect_deadness_in_conj(Goals0, Deadness0,
						ModuleInfo, Goals, Deadness1),
		detect_deadness_in_goal(Goal0, Deadness1,
						ModuleInfo, Deadness, Goal)
	).

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
	set__difference(Union, Deadness1, Residue),
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
	set__difference(Union, Deadness1, Residue),
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

:- pred stuff_deadness_residue_into_goal(hlds__goal, liveness_info, hlds__goal).
:- mode stuff_deadness_residue_into_goal(in, in, out) is det.

stuff_deadness_residue_into_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_pre_delta_liveness(GoalInfo0, Births - Deaths0),
	set__union(Deaths0, Residue, Deaths),
	goal_info_set_pre_delta_liveness(GoalInfo0, Births - Deaths, GoalInfo).

%-----------------------------------------------------------------------------%

	% traverses a goal, annotating it with information about which vars
	% are live on backtracking.  `Liveness' is the variables that are
	% are live for the purposes of forward execution, i.e. they have
	% been bound and may be referenced again (during forward execution).
	% `Extras' is the variables that may be referenced on backtracking.

:- pred add_nondet_lives_to_goal(hlds__goal, set(var),
				set(var), hlds__goal, set(var), set(var)).
:- mode add_nondet_lives_to_goal(in, in, in, out, out, out) is det.

add_nondet_lives_to_goal(Goal0 - GoalInfo0, Liveness0,
				Extras0, Goal - GoalInfo, Liveness, Extras) :-
	goal_info_pre_delta_liveness(GoalInfo0, PreDelta0),
	goal_info_post_delta_liveness(GoalInfo0, PostDelta0),

	PreDelta0 = PreBirths0 - PreDeaths0,
	set__difference(PreDeaths0, Extras0, PreDeaths),
	PreDelta = PreBirths0 - PreDeaths,

	PostDelta0 = PostBirths0 - PostDeaths0,

	set__difference(Liveness0,  PreDeaths0, Liveness1),

	goal_info_get_code_model(GoalInfo0, GoalModel),
	(
		GoalModel = model_non,
		( Goal0 = disj(_) ; Goal0 = call(_,_,_,_,_,_,_) )
	->
		set__union(Extras0, Liveness1, Extras1)
	;
		Extras1 = Extras0
	),

	set__union(Liveness1, PreBirths0, Liveness2),

	add_nondet_lives_to_goal_2(Goal0, Liveness2, Extras0, Extras1,
						Goal, Liveness3, Extras),

	set__difference(PostDeaths0, Extras1, PostDeaths),
	PostDelta = PostBirths0 - PostDeaths,

	set__difference(Liveness3, PostDeaths0, Liveness4),
	set__union(Liveness4, PostBirths0, Liveness),

        goal_info_set_pre_delta_liveness(GoalInfo0, PreDelta, GoalInfo1),
	goal_info_set_post_delta_liveness(GoalInfo1, PostDelta, GoalInfo).

:- pred add_nondet_lives_to_goal_2(hlds__goal_expr, set(var), set(var),set(var),
					hlds__goal_expr, set(var), set(var)).
:- mode add_nondet_lives_to_goal_2(in, in, in, in, out, out, out) is det.

add_nondet_lives_to_goal_2(conj(Goals0), Liveness0, _, Extras0,
				conj(Goals), Liveness, Extras) :-
	add_nondet_lives_to_conj(Goals0, Liveness0, Extras0,
					Goals, Liveness, Extras).

add_nondet_lives_to_goal_2(disj(Goals0), Liveness0, OutsideExtras, Extras0,
				disj(Goals), Liveness, Extras) :-
	add_nondet_lives_to_disj(Goals0, Liveness0, OutsideExtras, Extras0,
					Goals, Liveness, Extras).

add_nondet_lives_to_goal_2(switch(Var, CF, Goals0), Liveness0, _, Extras0,
				switch(Var, CF, Goals), Liveness, Extras) :-
	set__init(Empty),
	add_nondet_lives_to_switch(Goals0, Liveness0, Extras0,
					Empty, Goals, Liveness, Extras).

add_nondet_lives_to_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Liveness0,
		_, Extras0, if_then_else(Vars, Cond, Then, Else),
							Liveness, Extras) :-
	add_nondet_lives_to_goal(Cond0, Liveness0, Extras0,
					Cond, Liveness1, Extras1),
	add_nondet_lives_to_goal(Then0, Liveness1, Extras1,
					Then, _Liveness2, Extras2),
	add_nondet_lives_to_goal(Else0, Liveness0, Extras0,
					Else1, Liveness, Extras3),
	set__union(Extras2, Extras3, Extras),

		% Anything that became nondet-live in the condition
		% or the "then" can safely be killed off in the "else"
		% so long as it isn't live!
	set__difference(Extras2, Extras0, CTExtras0),
	set__difference(CTExtras0, Liveness0, CTExtras1),
	set__difference(CTExtras1, Liveness, CTExtras),

	stuff_liveness_residue_into_goal(Else1, CTExtras, Else2),
	stuff_deadness_residue_into_goal(Else2, CTExtras, Else).


	% Nondet lives cannot escape from a some if they
	% are quantified to that quantifier.
	% XXX until variables are properly renamed apart,
	% variables with overlapping scopes will cause problems.
add_nondet_lives_to_goal_2(some(Vars, Goal0), Liveness0, _, Extras0,
				some(Vars, Goal), Liveness, Extras) :-
	add_nondet_lives_to_goal(Goal0, Liveness0, Extras0,
					Goal1, Liveness, Extras1),
	set__list_to_set(Vars, VarsSet),
	set__difference(Extras1, VarsSet, Extras),
	set__intersect(Extras1, VarsSet, QExtras1),
	set__difference(QExtras1, Liveness, QExtras),
	add_deadness_to_goal(Goal1, QExtras, Goal).

	% Nondet lives cannot escape from a negation
add_nondet_lives_to_goal_2(not(Goal0), Liveness0, _, Extras0,
				not(Goal), Liveness, Extras0) :-
	add_nondet_lives_to_goal(Goal0, Liveness0, Extras0,
					Goal1, Liveness, Extras1),
		% since nondeterminism cannot escape a negation
		% ie, you can't backtrack *into* a negation,
		% any nondet-lives generated in the negation
		% should be killed off.
	set__difference(Extras1, Extras0, NExtras1),
	set__difference(NExtras1, Liveness, NExtras),
	add_deadness_to_goal(Goal1, NExtras, Goal).

add_nondet_lives_to_goal_2(call(A,B,C,D,E,F,G), Liveness, _, Extras,
				call(A,B,C,D,E,F,G), Liveness, Extras).

add_nondet_lives_to_goal_2(unify(A,B,C,D,E), Liveness, _, Extras,
				unify(A,B,C,D,E), Liveness, Extras).

:- pred add_nondet_lives_to_conj(list(hlds__goal), set(var), set(var),
				list(hlds__goal), set(var), set(var)).
:- mode add_nondet_lives_to_conj(in, in, in, out, out, out) is det.

add_nondet_lives_to_conj([], Liveness, Extras, [], Liveness, Extras).
add_nondet_lives_to_conj([G0|Gs0], Liveness0, Extras0,
				[G|Gs], Liveness, Extras) :-
	add_nondet_lives_to_goal(G0, Liveness0, Extras0, G, Liveness1, Extras1),
	(
		G0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, unreachable)
	->
		Gs = Gs0,
		Liveness = Liveness1,
		Extras = Extras1
	;
		add_nondet_lives_to_conj(Gs0, Liveness1, Extras1,
							Gs, Liveness, Extras)
	).

:- pred add_nondet_lives_to_disj(list(hlds__goal), set(var), set(var), set(var),
				list(hlds__goal), set(var), set(var)).
:- mode add_nondet_lives_to_disj(in, in, in, in, out, out, out) is det.

add_nondet_lives_to_disj([], Liveness, _, Extras, [], Liveness, Extras).
add_nondet_lives_to_disj([G0], Liveness0, OutsideExtras, Extras0,
					[G], Liveness, Extras) :-
	add_nondet_lives_to_goal(G0, Liveness0, OutsideExtras,
						G, Liveness, Extras1),
	set__union(Extras0, Extras1, Extras).
add_nondet_lives_to_disj([G0|Gs0], Liveness0, OutsideExtras, Extras0,
					[G|Gs], Liveness, Extras) :-
	Gs0 = [_|_],
	add_nondet_lives_to_disj(Gs0, Liveness0, OutsideExtras, Extras0,
						Gs, _Liveness1, Extras1),
	add_nondet_lives_to_goal(G0, Liveness0, Extras0, G, Liveness, Extras2),
	set__union(Extras1, Extras2, Extras).

:- pred add_nondet_lives_to_switch(list(case), set(var), set(var), set(var),
				list(case), set(var), set(var)).
:- mode add_nondet_lives_to_switch(in, in, in, in, out, out, out) is det.

add_nondet_lives_to_switch([], Liveness, _Extras0,
				Extras, [], Liveness, Extras).
add_nondet_lives_to_switch([case(ConsId, G0)|Gs0], Liveness0, Extras0,
			Extras1, [case(ConsId, G)|Gs], Liveness, Extras) :-

	add_nondet_lives_to_goal(G0, Liveness0, Extras0, G1, Liveness, Extras2),
	set__union(Extras1, Extras2, Extras3),

	add_nondet_lives_to_switch(Gs0, Liveness0, Extras0,
					Extras3, Gs, _Liveness1, Extras),

		% Anything that became nondet-live in another case
		% but didn't in this one had better be killed off.
	set__difference(Extras, Extras2, CTExtras0),
	goal_vars(G1, GoalVars),
	set__difference(CTExtras0, GoalVars, CTExtras),

	stuff_liveness_residue_into_goal(G1, CTExtras, G2),
	stuff_deadness_residue_into_goal(G2, CTExtras, G).

:- pred add_deadness_to_goal(hlds__goal, set(var), hlds__goal).
:- mode add_deadness_to_goal(in, in, out) is det.

add_deadness_to_goal(Goal - GoalInfo0, Vars, Goal - GoalInfo) :-
	goal_info_nondet_lives(GoalInfo0, NondetLives0),
	goal_info_post_delta_liveness(GoalInfo0, PostDelta0),
	PostDelta0 = PostBirths - PostDeaths0,
	set__union(NondetLives0, Vars, NondetLives),
	set__union(PostDeaths0, Vars, PostDeaths),
	PostDelta = PostBirths - PostDeaths,
	goal_info_set_nondet_lives(GoalInfo0, NondetLives, GoalInfo1),
	goal_info_set_post_delta_liveness(GoalInfo1, PostDelta, GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
