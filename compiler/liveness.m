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

:- import_module hlds_module, hlds_pred.

:- pred detect_liveness_proc(proc_info, module_info, proc_info).
% :- mode detect_liveness_proc(di, in, uo) is det.
:- mode detect_liveness_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, llds, mode_util, term, quantification, instmap.
:- import_module list, map, set, std_util, assoc_list, globals, require.

%-----------------------------------------------------------------------------%

	% To process each ProcInfo, we get the goal,
	% initialize the instmap based on the modes of the head vars,
	% and pass these to `detect_liveness_in_goal'.

detect_liveness_proc(ProcInfo0, ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),

	detect_initial_liveness(ProcInfo0, ModuleInfo, Liveness0),
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Goal1),

	detect_initial_deadness(ProcInfo0, ModuleInfo, Deadness0),
	detect_deadness_in_goal(Goal1, Deadness0, ModuleInfo, ProcInfo0, Goal2),

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
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
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
		set__init(PostBirths),
		Goal = Goal0
	;
		set__init(PreBirths),
		detect_liveness_in_goal_2(Goal0, Liveness0, ModuleInfo,
						Liveness1, Goal),
		set__difference(Births, Liveness1, PostBirths)
	),
	goal_info_set_pre_births(GoalInfo0, PreBirths, GoalInfo1),
	goal_info_set_post_births(GoalInfo1, PostBirths, GoalInfo).

:- pred detect_liveness_in_goal(hlds__goal, liveness_info, module_info,
				hlds__goal).
:- mode detect_liveness_in_goal(in, in, in, out) is det.

detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Goal) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, _, Goal).

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
	instmap_delta_lookup_var(InstMapDelta, Var, Inst),
	( inst_is_bound(ModuleInfo, Inst) ->
		set__insert(BoundVars0, Var, BoundVars1)
	;
		BoundVars1 = BoundVars0
	),
	find_binding_occurrences(Vars, ModuleInfo, InstMapDelta, BoundVars1,
		BoundVars).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.

:- pred detect_liveness_in_goal_2(hlds__goal_expr, liveness_info,
				module_info, liveness_info, hlds__goal_expr).
:- mode detect_liveness_in_goal_2(in, in, in, out, out) is det.

detect_liveness_in_goal_2(conj(Goals0), Liveness0, ModuleInfo,
		Liveness, conj(Goals)) :-
	detect_liveness_in_conj(Goals0, Liveness0, ModuleInfo, Liveness, Goals).

detect_liveness_in_goal_2(disj(Goals0, FV), Liveness0, ModuleInfo,
		Liveness, disj(Goals, FV)) :-
	set__init(Union0),
	detect_liveness_in_disj(Goals0, Liveness0, ModuleInfo,
					Union0, Union, Goals),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(not(Goal0), Liveness0, ModuleInfo,
		Liveness, not(Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Liveness, Goal).

detect_liveness_in_goal_2(switch(Var, Det, Cases0, FV), Liveness0,
		ModuleInfo, Liveness, switch(Var, Det, Cases, FV)) :-
	set__init(Union0),
	detect_liveness_in_cases(Cases0, Liveness0, ModuleInfo,
							Union0, Union, Cases),
	set__union(Liveness0, Union, Liveness).

detect_liveness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, FV),
		Liveness0, M, Liveness,
		if_then_else(Vars, Cond, Then, Else, FV)) :-
	detect_liveness_in_goal(Cond0, Liveness0, M, LivenessCond, Cond),
	detect_liveness_in_goal(Then0, LivenessCond, M, LivenessThen, Then1),
	detect_liveness_in_goal(Else0, Liveness0, M, LivenessElse, Else1),

	set__difference(LivenessThen, LivenessCond, ProducedInThen),
	set__difference(LivenessElse, Liveness0, ProducedInElse),

	set__difference(ProducedInElse, ProducedInThen, ResidueThen),
	set__difference(ProducedInThen, ProducedInElse, ResidueElse),

	stuff_liveness_residue_after_goal(Then1, ResidueThen, Then),
	stuff_liveness_residue_after_goal(Else1, ResidueElse, Else),

	set__union(LivenessThen, LivenessElse, Liveness).

detect_liveness_in_goal_2(some(Vars, Goal0), Liveness0, ModuleInfo,
		Liveness, some(Vars, Goal)) :-
	detect_liveness_in_goal(Goal0, Liveness0, ModuleInfo, Liveness, Goal).

detect_liveness_in_goal_2(higher_order_call(A,B,C,D,E), L, _, L,
			higher_order_call(A,B,C,D,E)).

detect_liveness_in_goal_2(call(A,B,C,D,E,F), L, _, L, call(A,B,C,D,E,F)).

detect_liveness_in_goal_2(unify(A,B,C,D,E), L, _, L, unify(A,B,C,D,E)).

detect_liveness_in_goal_2(pragma_c_code(A,B,C,D,E,F), L, _, L, 
		pragma_c_code(A,B,C,D,E,F)).

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
		goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
		instmap_delta_is_unreachable(InstmapDelta)
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
	stuff_liveness_residue_after_goal(Goal1, Residue, Goal).

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
	stuff_liveness_residue_after_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_goal(hlds__goal, liveness_info, module_info,
				proc_info, liveness_info, hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, in, out, out) is det.

detect_deadness_in_goal(Goal0 - GoalInfo0, Deadness0, ModuleInfo, ProcInfo,
					Deadness, Goal - GoalInfo) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	module_info_globals(ModuleInfo, Globals),
	globals__get_gc_method(Globals, GC_Method),
	(
		goal_is_atomic(Goal0)
	->
		(
			GC_Method = accurate
		->
			proc_info_get_used_typeinfos_setwise(ProcInfo, 
				NonLocals, TypeInfoVars),
			set__union(NonLocals, TypeInfoVars, NonLocals1)
		;
			NonLocals1 = NonLocals
		),
		set__init(PreDeaths),
		set__difference(NonLocals1, Deadness0, PostDeaths),
		set__union(Deadness0, PostDeaths, Deadness),
		Goal = Goal0
	;
		set__union(Deadness0, NonLocals, Deadness_Nonlocals),
		(
			GC_Method = accurate
		->
			proc_info_get_used_typeinfos_setwise(ProcInfo,
				NonLocals, TypeInfoVars),
			set__union(Deadness_Nonlocals, TypeInfoVars, 
				Deadness)
		;
			Deadness = Deadness_Nonlocals
		),
		set__init(PostDeaths),
		detect_deadness_in_goal_2(Goal0, GoalInfo0, Deadness0,
				ModuleInfo, ProcInfo, Deadness1, Goal),
		set__difference(Deadness, Deadness1, PreDeaths)
	),
	goal_info_set_post_deaths(GoalInfo0, PostDeaths, GoalInfo1),
	goal_info_set_pre_deaths(GoalInfo1, PreDeaths, GoalInfo).

:- pred detect_deadness_in_goal(hlds__goal, liveness_info,
				module_info, proc_info, hlds__goal).
:- mode detect_deadness_in_goal(in, in, in, in, out) is det.

detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, ProcInfo, Goal) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, ProcInfo, _, Goal).

	% Here we process each of the different sorts of goals.

:- pred detect_deadness_in_goal_2(hlds__goal_expr, hlds__goal_info,
	liveness_info, module_info, proc_info, liveness_info, hlds__goal_expr).
:- mode detect_deadness_in_goal_2(in, in, in, in, in, out, out) is det.

detect_deadness_in_goal_2(conj(Goals0), _, Deadness0, ModuleInfo, ProcInfo,
			Deadness, conj(Goals)) :-
	detect_deadness_in_conj(Goals0, Deadness0, ModuleInfo, ProcInfo,
		Goals, Deadness).

detect_deadness_in_goal_2(disj(Goals0, FV), GoalInfo, Deadness0, ModuleInfo,
		ProcInfo, Deadness, disj(Goals, FV)) :-
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		set__init(Union0),
		detect_deadness_in_nondet_disj(Goals0, Deadness0, ModuleInfo,
			ProcInfo, Union0, Union, Goals),
		set__union(Deadness0, Union, Deadness)
	;
		detect_deadness_in_pruned_disj(Goals0, GoalInfo,
			Deadness0, ModuleInfo,
			ProcInfo, Goals, Deadness)
	).

detect_deadness_in_goal_2(not(Goal0), _, Deadness0, ModuleInfo, ProcInfo,
		Deadness, not(Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, ProcInfo, 
		Deadness, Goal).

detect_deadness_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, FV), _,
		Deadness0, ModuleInfo, ProcInfo, Deadness, 
		if_then_else(Vars, Cond, Then, Else, FV)) :-
	detect_deadness_in_goal(Then0, Deadness0, ModuleInfo, ProcInfo,
		DeadnessThen, Then1),
	detect_deadness_in_goal(Else0, Deadness0, ModuleInfo, ProcInfo,
		DeadnessElse, Else1),
	set__union(DeadnessThen, DeadnessElse, DeadnessThenElse),
	detect_deadness_in_goal(Cond0, DeadnessThenElse, ModuleInfo, ProcInfo,
		Deadness, Cond),
	set__difference(DeadnessElse, DeadnessThen, ResidueThen),
	stuff_deadness_residue_before_goal(Then1, ResidueThen, Then),
	set__difference(DeadnessThen, DeadnessElse, ResidueElse),
	stuff_deadness_residue_before_goal(Else1, ResidueElse, Else).

detect_deadness_in_goal_2(switch(Var, Det, Cases0, FV), _, Deadness0,
			ModuleInfo, ProcInfo, Deadness,
			switch(Var, Det, Cases, FV)) :-
	set__init(Union0),
	detect_deadness_in_cases(Var, Cases0, Deadness0, ModuleInfo, ProcInfo, 
		Union0, Union, Cases),
	set__union(Deadness0, Union, Deadness).

detect_deadness_in_goal_2(some(Vars, Goal0), _, Deadness0, ModuleInfo,
		ProcInfo, Deadness, some(Vars, Goal)) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, ProcInfo,
		Deadness, Goal).

detect_deadness_in_goal_2(higher_order_call(A,B,C,D,E), _, Dn, _, _, Dn,
			higher_order_call(A,B,C,D,E)).

detect_deadness_in_goal_2(call(A,B,C,D,E,F), _, Dn, _, _, Dn, 
			call(A,B,C,D,E,F)).

detect_deadness_in_goal_2(unify(A,B,C,D,E), _, Dn, _, _, Dn, unify(A,B,C,D,E)).

detect_deadness_in_goal_2(pragma_c_code(A,B,C,D,E,F), _, Dn, _, _, Dn, 
		pragma_c_code(A,B,C,D,E,F)).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_conj(list(hlds__goal), set(var), module_info,
				proc_info, list(hlds__goal), set(var)).
:- mode detect_deadness_in_conj(in, in, in, in, out, out) is det.

detect_deadness_in_conj([], Deadness, _ModuleInfo, _ProcInfo, [], Deadness).
detect_deadness_in_conj([Goal0|Goals0], Deadness0, ModuleInfo, ProcInfo,
						[Goal|Goals], Deadness) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
		instmap_delta_is_unreachable(InstmapDelta)
	->
		Goals = Goals0,
		detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, ProcInfo,
				Deadness, Goal)
	;
		detect_deadness_in_conj(Goals0, Deadness0, ModuleInfo, ProcInfo,
				Goals, Deadness1),
		detect_deadness_in_goal(Goal0, Deadness1, ModuleInfo, ProcInfo,
				Deadness, Goal)
	).

%-----------------------------------------------------------------------------%

	% for a pruned disj, for simplicity we handle things pretty
	% conservatively: we assume that none of the non-local variables
	% die until the very end of the disjunction.

:- pred detect_deadness_in_pruned_disj(list(hlds__goal), hlds__goal_info,
		set(var), module_info, proc_info, list(hlds__goal), set(var)).
:- mode detect_deadness_in_pruned_disj(in, in, in, in, in, out, out) is det.

detect_deadness_in_pruned_disj(Goals0, GoalInfo, Deadness0, ModuleInfo,
			ProcInfo, Goals, Deadness) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__difference(NonLocals, Deadness0, PostDeaths),
	set__union(Deadness0, PostDeaths, Deadness),
	detect_deadness_in_pruned_disj_2(Goals0, Deadness, PostDeaths,
		ModuleInfo, ProcInfo, Goals).

:- pred detect_deadness_in_pruned_disj_2(list(hlds__goal), set(var), set(var),
		module_info, proc_info, list(hlds__goal)).
:- mode detect_deadness_in_pruned_disj_2(in, in, in, in, in, out) is det.

detect_deadness_in_pruned_disj_2([], _Deadness0, _PostDeaths,
				_ModuleInfo, _PInfo, []).
detect_deadness_in_pruned_disj_2([Goal0|Goals0], Deadness0, PostDeaths,
				ModuleInfo, ProcInfo, [Goal|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, ProcInfo, 
			_Deadness1, Goal1),
	stuff_deadness_residue_after_goal(Goal1, PostDeaths, Goal),
	detect_deadness_in_pruned_disj_2(Goals0, Deadness0, PostDeaths,
			ModuleInfo, ProcInfo, Goals).

%-----------------------------------------------------------------------------%
:- pred detect_deadness_in_nondet_disj(list(hlds__goal), set(var), module_info, 
			proc_info, set(var), set(var), list(hlds__goal)).
:- mode detect_deadness_in_nondet_disj(in, in, in, in, in, out, out) is det.

detect_deadness_in_nondet_disj([], _Deadness, _ModuleInfo, _PInfo, Union,
				Union, []).
detect_deadness_in_nondet_disj([Goal0|Goals0], Deadness, ModuleInfo, ProcInfo,
						Union0, Union, [Goal|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness, ModuleInfo, ProcInfo, 
			Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_nondet_disj(Goals0, Deadness, ModuleInfo, ProcInfo,
							Union1, Union, Goals),
	set__difference(Union, Deadness1, Residue),
	stuff_deadness_residue_before_goal(Goal1, Residue, Goal).

%-----------------------------------------------------------------------------%

:- pred detect_deadness_in_cases(var, list(case), set(var), module_info, 
					proc_info, set(var), set(var), 
					list(case)).
:- mode detect_deadness_in_cases(in, in, in, in, in, in, out, out) is det.

detect_deadness_in_cases(_Var, [], _Deadness, _ModuleInfo, _ProcInfo, 
			Union, Union, []).
detect_deadness_in_cases(SwitchVar, [case(Cons, Goal0)|Goals0], Deadness0,
			ModuleInfo, ProcInfo, Union0, Union, 
			[case(Cons, Goal)|Goals]) :-
	detect_deadness_in_goal(Goal0, Deadness0, ModuleInfo, ProcInfo,
			Deadness1, Goal1),
	set__union(Union0, Deadness1, Union1),
	detect_deadness_in_cases(SwitchVar, Goals0, Deadness0, ModuleInfo,
					ProcInfo, Union1, Union2, Goals),
		% If the switch variable does not become dead in a case
		% it must be put in the pre-death set of that case.
	set__insert(Union2, SwitchVar, Union),
	set__difference(Union, Deadness1, Residue),
	stuff_deadness_residue_before_goal(Goal1, Residue, Goal).

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
	detect_initial_deadness_2(VarArgs, ModuleInfo, Deadness0, Deadness1),
		% If doing accurate garbage collection, the corresponding
		% typeinfos need to be added to these.
	module_info_globals(ModuleInfo, Globals),
	globals__get_gc_method(Globals, GC_Method),
	(
		GC_Method = accurate
	->
		proc_info_get_used_typeinfos_setwise(ProcInfo, Deadness1, 
			TypeInfoVars),
		set__union(Deadness1, TypeInfoVars, Deadness)
	;
		Deadness = Deadness1
	).

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

:- pred stuff_liveness_residue_after_goal(hlds__goal, liveness_info,
					hlds__goal).
:- mode stuff_liveness_residue_after_goal(in, in, out) is det.

stuff_liveness_residue_after_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo) :-
	goal_info_post_births(GoalInfo0, PostBirths0),
	set__union(PostBirths0, Residue, PostBirths),
	goal_info_set_post_births(GoalInfo0, PostBirths, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred stuff_deadness_residue_before_goal(hlds__goal, liveness_info,
						hlds__goal).
:- mode stuff_deadness_residue_before_goal(in, in, out) is det.

stuff_deadness_residue_before_goal(Goal - GoalInfo0, Residue, Goal - GoalInfo)
		:-
	goal_info_pre_deaths(GoalInfo0, PreDeaths0),
	set__union(PreDeaths0, Residue, PreDeaths),
	goal_info_set_pre_deaths(GoalInfo0, PreDeaths, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred stuff_deadness_residue_after_goal(hlds__goal, liveness_info,
						hlds__goal).
:- mode stuff_deadness_residue_after_goal(in, in, out) is det.

stuff_deadness_residue_after_goal(Goal0 - GoalInfo0, Residue, Goal - GoalInfo)
		:-
	( goal_is_atomic(Goal0) ->
		% XXX the following code is a bit dodgy...
		% For atomic goals, we can't just put the deadness residue
		% in the post-delta-liveness, because the code generator
		% would then try to apply it prematurely in the pre_goal_update.
		% Instead, we create a singleton conjunction, so that we
		% can use the post-delta-liveness of the (non-atomic)
		% conjunction.  This technique is probably a bit fragile,
		% since other parts of the compiler aren't very keen on
		% singleton disjunctions.
		% XXX The unconditional overwriting of the three other fields
		% is also worrying.
		Goal = conj([Goal0 - GoalInfo0]),
		set__init(PreBirths),
		set__init(PostBirths),
		set__init(PreDeaths),
		goal_info_set_pre_births(GoalInfo0, PreBirths, GoalInfo1),
		goal_info_set_post_births(GoalInfo1, PostBirths, GoalInfo2),
		goal_info_set_pre_deaths(GoalInfo2, PreDeaths, GoalInfo3),
		goal_info_set_post_deaths(GoalInfo3, Residue, GoalInfo)
	;
		Goal = Goal0,
		goal_info_post_deaths(GoalInfo0, PostDeaths0),
		set__union(PostDeaths0, Residue, PostDeaths),
		goal_info_set_post_deaths(GoalInfo0, PostDeaths, GoalInfo)
	).

%-----------------------------------------------------------------------------%

	% traverses a goal, annotating it with information about which vars
	% are live on backtracking.  `Liveness' is the variables that are
	% are live for the purposes of forward execution, i.e. they have
	% been bound and may be referenced again (during forward execution).
	% `Extras' is the variables that may be referenced on backtracking.
	% Each goal is annotated with the variables that will be live on
	% backtracking.

:- pred add_nondet_lives_to_goal(hlds__goal, set(var),
				set(var), hlds__goal, set(var), set(var)).
:- mode add_nondet_lives_to_goal(in, in, in, out, out, out) is det.

add_nondet_lives_to_goal(Goal0 - GoalInfo0, Liveness0,
				Extras0, Goal - GoalInfo, Liveness, Extras) :-
	goal_info_pre_births(GoalInfo0, PreBirths0),
	goal_info_post_births(GoalInfo0, PostBirths0),
	goal_info_pre_deaths(GoalInfo0, PreDeaths0),
	goal_info_post_deaths(GoalInfo0, PostDeaths0),

	set__difference(Liveness0, PreDeaths0, Liveness1),
	set__union(Liveness1, PreBirths0, Liveness2),

	goal_info_get_code_model(GoalInfo0, GoalModel),
	(
		GoalModel = model_non,
		Goal0 = disj(_, _)
	->
		% It is an invariant that for compound goals, the
		% prebirths set should be empty
		require(set__empty(PreBirths0), "Nonempty prebirth set in disj"),
		% If the goal is a nondet disj then all the variables
		% that are live at the start of the disj will be
		% needed at later disjuncts (a conservative approximation)
		set__union(Extras0, Liveness2, Extras1)
	;
		GoalModel = model_non,
		(
			Goal0 = call(_,_,_,_,_,_)
		;
			Goal0 = higher_order_call(_,_,_,_,_)
		)
	->
		% If the goal is a nondet call then all the variables
		% that are live across the call become nondet live.
		set__difference(Liveness2, PostDeaths0, LivenessAcross),
		set__union(Extras0, LivenessAcross, Extras1)
	;
		Extras1 = Extras0
	),

	add_nondet_lives_to_goal_2(Goal0, Liveness2, Extras1,
				GoalModel, Goal, Liveness3, Extras),

	set__difference(Liveness3, PostDeaths0, Liveness4),
	set__union(Liveness4, PostBirths0, Liveness),

        goal_info_set_nondet_lives(GoalInfo0, Extras1, GoalInfo).

:- pred add_nondet_lives_to_goal_2(hlds__goal_expr, set(var), set(var),
			code_model, hlds__goal_expr, set(var), set(var)).
:- mode add_nondet_lives_to_goal_2(in, in, in, in, out, out, out) is det.

add_nondet_lives_to_goal_2(conj(Goals0), Liveness0, Extras0, _,
				conj(Goals), Liveness, Extras) :-
	add_nondet_lives_to_conj(Goals0, Liveness0, Extras0,
					Goals, Liveness, Extras).

add_nondet_lives_to_goal_2(disj(Goals0, FV), Liveness0, Extras0, _,
				disj(Goals, FV), Liveness, Extras) :-
	ExtrasAcc = Extras0,
	add_nondet_lives_to_disj(Goals0, Liveness0, Extras0,
					Goals, Liveness, ExtrasAcc, Extras).

add_nondet_lives_to_goal_2(switch(Var, CF, Goals0, FV), Liveness0, Extras0, _,
				switch(Var, CF, Goals, FV), Liveness, Extras) :-
	ExtrasAcc = Extras0,
	add_nondet_lives_to_switch(Goals0, Liveness0, Extras0,
					Goals, Liveness, ExtrasAcc, Extras).

add_nondet_lives_to_goal_2(if_then_else(Vars, Cond0, Then0, Else0, FV),
		Liveness0, Extras0, _, if_then_else(Vars, Cond, Then, Else, FV),
							Liveness, Extras) :-
	add_nondet_lives_to_goal(Cond0, Liveness0, Extras0,
					Cond, Liveness1, Extras1),
	add_nondet_lives_to_goal(Then0, Liveness1, Extras1,
					Then1, LivenessThen, Extras2),
	add_nondet_lives_to_goal(Else0, Liveness1, Extras0,
					Else1, Liveness, Extras3),
	set__union(Extras2, Extras3, Extras),

		% things that become nondet live in the Else
		% but not the Then and which are not live at
		% the end of the Then have to become automagically
		% live at the end of the Then.
	set__difference(Extras, Extras2, ElseOnlyExtras0),
	set__difference(ElseOnlyExtras0, LivenessThen, ElseOnlyExtras),
	stuff_liveness_residue_after_goal(Then1, ElseOnlyExtras, Then),

		% things that become nondet live in the Then
		% but not the Else and which are not live at the
		% end of the Else have to become automagically
		% live at the end of the Else.
	set__difference(Extras, Extras3, ThenOnlyExtras0),
	set__difference(ThenOnlyExtras0, Liveness, ThenOnlyExtras),
	stuff_liveness_residue_after_goal(Else1, ThenOnlyExtras, Else).

	% Nondet lives cannot escape from a commit
	% so we have to work out if this quantifier is a commit or not.
add_nondet_lives_to_goal_2(some(Vars, Goal0), Liveness0, Extras0, OuterModel,
				some(Vars, Goal), Liveness, Extras) :-
	add_nondet_lives_to_goal(Goal0, Liveness0, Extras0,
					Goal, Liveness, Extras1),
	Goal0 = _ - GoalInfo,
	goal_info_get_code_model(GoalInfo, InnerModel),
	(
		% is this a commit?
		OuterModel \= model_non,
		InnerModel = model_non
	->
		% if it is, then we revert to the original
		% set of nondet live variables
		Extras = Extras0
	;
		Extras = Extras1
	).

	% Nondet lives cannot escape from a negation
add_nondet_lives_to_goal_2(not(Goal0), Liveness0, Extras0, _,
				not(Goal), Liveness, Extras0) :-
	add_nondet_lives_to_goal(Goal0, Liveness0, Extras0,
					Goal, Liveness, _).

add_nondet_lives_to_goal_2(higher_order_call(A,B,C,D,E), Liveness, Extras, _,
			higher_order_call(A,B,C,D,E), Liveness, Extras).

add_nondet_lives_to_goal_2(call(A,B,C,D,E,F), Liveness, Extras, _,
				call(A,B,C,D,E,F), Liveness, Extras).

add_nondet_lives_to_goal_2(unify(A,B,C,D,E), Liveness, Extras, _,
				unify(A,B,C,D,E), Liveness, Extras).

add_nondet_lives_to_goal_2(pragma_c_code(A,B,C,D,E,F), Liveness, Extras, _,
				pragma_c_code(A,B,C,D,E,F), Liveness, Extras).


:- pred add_nondet_lives_to_conj(list(hlds__goal), set(var), set(var),
				list(hlds__goal), set(var), set(var)).
:- mode add_nondet_lives_to_conj(in, in, in, out, out, out) is det.

add_nondet_lives_to_conj([], Liveness, Extras, [], Liveness, Extras).
add_nondet_lives_to_conj([G0|Gs0], Liveness0, Extras0,
				[G|Gs], Liveness, Extras) :-
	add_nondet_lives_to_goal(G0, Liveness0, Extras0, G, Liveness1, Extras1),
	(
		G0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
		instmap_delta_is_unreachable(InstmapDelta)
	->
		Gs = Gs0,
		Liveness = Liveness1,
		Extras = Extras1
	;
		add_nondet_lives_to_conj(Gs0, Liveness1, Extras1,
							Gs, Liveness, Extras)
	).

:- pred add_nondet_lives_to_disj(list(hlds__goal), set(var), set(var),
				list(hlds__goal), set(var), set(var), set(var)).
:- mode add_nondet_lives_to_disj(in, in, in, out, out, in, out) is det.

	% We have to add post-births of variables that become nondet
	% live in some disjuncts but are neither live nor nondet live at
	% the end of others.

add_nondet_lives_to_disj([], Liveness, _Extras0, [], Liveness, Extras, Extras).
	% For the last disjunct, we optimize what variables are nondet-
	% live by observing that in the last disjunct we no longer need
	% to save the inputs onto the stack for later disjuncts (since
	% there aren't any).
add_nondet_lives_to_disj([G0], Liveness0, Extras0,
					[G], Liveness, ExtrasAcc, Extras) :-
	add_nondet_lives_to_goal(G0, Liveness0, Extras0,
					G1, Liveness, Extras1),
	set__union(ExtrasAcc, Extras1, Extras),
	set__difference(ExtrasAcc, Extras1, OtherGoalExtras0),
	set__difference(OtherGoalExtras0, Liveness, OtherGoalExtras),
	stuff_liveness_residue_after_goal(G1, OtherGoalExtras, G).
add_nondet_lives_to_disj([G0|Gs0], Liveness0, Extras0,
					[G|Gs], Liveness, ExtrasAcc0, Extras) :-
		% make this clause mutually exclusive with the previous one
	Gs0 = [_|_],
	add_nondet_lives_to_goal(G0, Liveness0, Extras0, G1, Liveness, Extras2),
	set__union(ExtrasAcc0, Extras2, ExtrasAcc),
	add_nondet_lives_to_disj(Gs0, Liveness0, Extras0,
					Gs, _Liveness1, ExtrasAcc, Extras),
	set__difference(Extras, Extras2, OtherGoalExtras0),
	set__difference(OtherGoalExtras0, Liveness, OtherGoalExtras),
	stuff_liveness_residue_after_goal(G1, OtherGoalExtras, G).

:- pred add_nondet_lives_to_switch(list(case), set(var), set(var), 
				list(case), set(var),set(var), set(var)).
:- mode add_nondet_lives_to_switch(in, in, in, out, out, in, out) is det.

add_nondet_lives_to_switch([], Liveness, _Extras0,
				[], Liveness, Extras, Extras).
add_nondet_lives_to_switch([case(ConsId, G0)|Gs0], Liveness0, Extras0,
			[case(ConsId, G)|Gs], Liveness, ExtrasAcc0, Extras) :-

	add_nondet_lives_to_goal(G0, Liveness0, Extras0, G1, Liveness, Extras2),
	set__union(ExtrasAcc0, Extras2, ExtrasAcc),

	add_nondet_lives_to_switch(Gs0, Liveness0, Extras0,
					Gs, _Liveness1, ExtrasAcc, Extras),
	set__difference(Extras, Extras2, OtherGoalExtras0),
	set__difference(OtherGoalExtras0, Liveness, OtherGoalExtras),
	stuff_liveness_residue_after_goal(G1, OtherGoalExtras, G).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
