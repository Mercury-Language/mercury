%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Switch detection - replace disjunctions with (semi)deterministic switch
% statements, where we can determine that the disjunction is actually
% just a switch.
%
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module switch_detection.

:- interface.

:- import_module hlds.

:- pred detect_switches(module_info, module_info, io__state, io__state).
:- mode detect_switches(in, out, di, uo) is det.

:- pred detect_switches_in_pred(pred_id, pred_info, module_info, module_info,
	io__state, io__state).
:- mode detect_switches_in_pred(in, in, di, uo, di, uo) is det.

	% utility pred used by cse_detection.m
:- pred interpret_unify(var, unify_rhs, substitution, substitution).
:- mode interpret_unify(in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util, require.
:- import_module modes, mode_util, type_util, term.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_switches_in_goal'
	% for each procedure body.

detect_switches(ModuleInfo0, ModuleInfo1) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	detect_switches_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_switches_in_preds(list(pred_id), module_info, module_info,
	io__state, io__state).
:- mode detect_switches_in_preds(in, in, out, di, uo) is det.

detect_switches_in_preds([], ModuleInfo, ModuleInfo) --> [].
detect_switches_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	detect_switches_in_pred(PredId, PredInfo, ModuleInfo0, ModuleInfo1),
	detect_switches_in_preds(PredIds, ModuleInfo1, ModuleInfo).

detect_switches_in_pred(PredId, PredInfo0, ModuleInfo0, ModuleInfo,
		IOstate, IOstate) :-
	pred_info_non_imported_procids(PredInfo0, ProcIds),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo).

:- pred detect_switches_in_procs(list(proc_id), pred_id,
	module_info, module_info).
:- mode detect_switches_in_procs(in, in, di, uo) is det.

detect_switches_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_switches_in_procs([ProcId | ProcIds], PredId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% To process each ProcInfo, we get the goal,
		% initialize the instmap based on the modes of the head vars,
		% and pass these to `detect_switches_in_goal'.
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo0, Goal),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	detect_switches_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Given a goal, and the instmap on entry to that goal,
	% replace disjunctions with switches whereever possible.

:- pred detect_switches_in_goal(hlds__goal, instmap, map(var, type),
	module_info, hlds__goal).
:- mode detect_switches_in_goal(in, in, in, in, out) is det.

detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal) :-
	detect_switches_in_goal_1(Goal0, InstMap0, VarTypes, ModuleInfo,
		Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_switches_in_goal_1(hlds__goal, instmap, map(var, type),
	module_info, hlds__goal, instmap).
:- mode detect_switches_in_goal_1(in, in, in, in, out, out) is det.

detect_switches_in_goal_1(Goal0 - GoalInfo, InstMap0, VarTypes, ModuleInfo,
		Goal - GoalInfo, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0, InstMapDelta,
		VarTypes, ModuleInfo, Goal),
	apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_switches_in_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
		instmap, map(var, type), module_info, hlds__goal_expr).
:- mode detect_switches_in_goal_2(in, in, in, in, in, in, out) is det.

detect_switches_in_goal_2(conj(Goals0), _GoalInfo, InstMap0, _InstMapDelta,
		VarTypes, ModuleInfo, conj(Goals)) :-
	detect_switches_in_conj(Goals0, InstMap0, VarTypes, ModuleInfo, Goals).

detect_switches_in_goal_2(disj(Goals0), GoalInfo, InstMap0, InstMapDelta,
		VarTypes, ModuleInfo, Goal) :-
	( Goals0 = [] ->
		Goal = disj([])
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo,
			InstMap0, InstMapDelta, VarTypes, ModuleInfo, [], Goal)
	).

detect_switches_in_goal_2(not(Goal0), _GoalInfo, InstMap0, _InstMapDelta,
		VarTypes, ModuleInfo, not(Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), _GoalInfo,
		InstMap0, _InstMapDelta, VarTypes, ModuleInfo,
		if_then_else(Vars, Cond, Then, Else)) :-
	detect_switches_in_goal_1(Cond0, InstMap0, VarTypes, ModuleInfo, Cond,
		InstMap1),
	detect_switches_in_goal(Then0, InstMap1, VarTypes, ModuleInfo, Then),
	detect_switches_in_goal(Else0, InstMap0, VarTypes, ModuleInfo, Else).

detect_switches_in_goal_2(some(Vars, Goal0), _GoalInfo, InstMap0, _InstMapDelta,
		VarTypes, ModuleInfo, some(Vars, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(call(A,B,C,D,E,F,G), _, _, _, _, _,
		call(A,B,C,D,E,F,G)).

detect_switches_in_goal_2(unify(A,B,C,D,E), _, _, _, _, _, unify(A,B,C,D,E)).

detect_switches_in_goal_2(switch(Var, CanFail, Cases0), _, InstMap, _,
		VarTypes, ModuleInfo, switch(Var, CanFail, Cases)) :-
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases).

%-----------------------------------------------------------------------------%

	% This is the interesting bit - we've found a non-empty
	% disjunction, and we've got a list of the non-local variables
	% of that disjunction. Now for each non-local variable, we
	% check whether there is a partition of the disjuncts such that
	% each group of disjunctions can only succeed if the variable
	% is bound to a different functor.

:- type cases == map(cons_id, list(hlds__goal)).
:- type again ---> again(var, list(hlds__goal),
	assoc_list(cons_id, list(hlds__goal))).

:- pred detect_switches_in_disj(list(var), list(hlds__goal), hlds__goal_info,
	instmap, instmap, map(var, type), module_info, list(again),
	hlds__goal_expr).
:- mode detect_switches_in_disj(in, in, in, in, in, in, in, in, out) is det.

detect_switches_in_disj([Var | Vars], Goals0, GoalInfo, InstMap, InstMapDelta,
		VarTypes, ModuleInfo, Again0, Goal) :-
	% can we do at least a partial switch on this variable?
	(
		instmap_lookup_var(InstMap, Var, VarInst0),
		inst_is_bound(ModuleInfo, VarInst0),
		partition_disj(Goals0, Var, Left, CasesList)
	->
		% are there any disjuncts that are not part of the switch?
		(
			Left = []
		->
			cases_to_switch(CasesList, Var, VarTypes, GoalInfo,
				InstMap, ModuleInfo, Goal)
		;
			detect_switches_in_disj(Vars, Goals0, GoalInfo,
				InstMap, InstMapDelta, VarTypes, ModuleInfo,
				[again(Var, Left, CasesList) | Again0], Goal)
		)
	;
		detect_switches_in_disj(Vars, Goals0, GoalInfo, InstMap,
			InstMapDelta, VarTypes, ModuleInfo, Again0, Goal)
	).
detect_switches_in_disj([], Goals0, GoalInfo, InstMap, _, VarTypes, ModuleInfo,
		AgainList0, disj(Goals)) :-
	(
		AgainList0 = [],
		detect_sub_switches_in_disj(Goals0, InstMap, VarTypes,
			ModuleInfo, Goals)
	;
		AgainList0 = [Again | AgainList1],
		select_best_switch(AgainList1, Again, BestAgain),
		BestAgain = again(Var, Left0, CasesList),
		cases_to_switch(CasesList, Var, VarTypes, GoalInfo, InstMap,
			ModuleInfo, SwitchGoal),
		list__reverse(Left0, Left),
		Goals = [SwitchGoal - GoalInfo | Left]
	).

:- pred select_best_switch(list(again), again, again).
:- mode select_best_switch(in, in, out) is det.

select_best_switch([], BestAgain, BestAgain).
select_best_switch([Again | AgainList], BestAgain0, BestAgain) :-
	(
		Again = again(_, _, CasesList),
		BestAgain0 = again(_, _, BestCasesList),
		list__length(CasesList, Length),
		list__length(BestCasesList, BestLength),
		Length < BestLength
	->
		BestAgain1 = BestAgain0
	;
		BestAgain1 = Again
	),
	select_best_switch(AgainList, BestAgain1, BestAgain).

:- pred detect_sub_switches_in_disj(list(hlds__goal), instmap, map(var, type),
	module_info, list(hlds__goal)).
:- mode detect_sub_switches_in_disj(in, in, in, in, out) is det.

detect_sub_switches_in_disj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_sub_switches_in_disj([Goal0 | Goals0], InstMap, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	detect_sub_switches_in_disj(Goals0, InstMap, VarTypes, ModuleInfo,
		Goals).

:- pred detect_switches_in_cases(list(case), instmap, map(var, type),
	module_info, list(case)).
:- mode detect_switches_in_cases(in, in, in, in, out) is det.

detect_switches_in_cases([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_cases([Case0 | Cases0], InstMap, VarTypes, ModuleInfo,
		[Case | Cases]) :-
	Case0 = case(Functor, Goal0),
	detect_switches_in_goal(Goal0, InstMap, VarTypes, ModuleInfo, Goal),
	Case = case(Functor, Goal),
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases).

:- pred detect_switches_in_conj(list(hlds__goal), instmap, map(var, type),
	module_info, list(hlds__goal)).
:- mode detect_switches_in_conj(in, in, in, in, out) is det.

detect_switches_in_conj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_conj([Goal0 | Goals0], InstMap0, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal_1(Goal0, InstMap0, VarTypes, ModuleInfo, Goal,
		InstMap1),
	detect_switches_in_conj(Goals0, InstMap1, VarTypes, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

	% partition_disj(Goals, Var, GoalInfo, VarTypes, ModuleInfo,
	%	Left, Cases):
	% Attempts to partition the disjunction `Goals' into a switch on `Var'.
	% If at least partially successful, returns the resulting `Cases', with
	% any disjunction goals not fitting into the switch in Left.

	% Given the list of goals in a disjunction, and an input variable
	% to switch on, we attempt to partition the goals into a switch.
	% For each constructor id, we record the list of disjuncts
	% which unify the variable with that constructor.
	% We partition the goals by abstractly interpreting the unifications
	% at the start of each disjunction, to build up a substitution.

:- pred partition_disj(list(hlds__goal), var, 
	list(hlds__goal), assoc_list(cons_id, list(hlds__goal))).
:- mode partition_disj(in, in, out, out) is semidet.

partition_disj(Goals0, Var, Left, CasesAssocList) :-
	map__init(Cases0),
	partition_disj_trial(Goals0, Var, [], Left, Cases0, Cases),
	map__to_assoc_list(Cases, CasesAssocList),
	CasesAssocList = [_,_|_].	% there must be more than one case

:- pred partition_disj_trial(list(hlds__goal), var,
	list(hlds__goal), list(hlds__goal), cases, cases).
:- mode partition_disj_trial(in, in, di, uo, di, uo) is det.

partition_disj_trial([], _Var, Left, Left, Cases, Cases).
partition_disj_trial([Goal0 | Goals], Var, Left0, Left, Cases0, Cases) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var_for_switch(ConjList0, Substitution, Var,
			ConjList, _NewSubstitution, MaybeFunctor),
	(
		MaybeFunctor = yes(Functor),
		Left1 = Left0,
		conj_list_to_goal(ConjList, GoalInfo, Goal),
		( map__search(Cases0, Functor, DisjList0) ->
			DisjList1 = [Goal | DisjList0]
		;
			DisjList1 = [Goal]
		),
		map__set(Cases0, Functor, DisjList1, Cases1)
	;
		MaybeFunctor = no,
		Left1 = [Goal0 | Left0],
		Cases1 = Cases0
	),
	partition_disj_trial(Goals, Var, Left1, Left, Cases1, Cases).

	% find_bind_var_for_switch(Goals0, Subst0, Var, Goals, Subst,
	%	MaybeFunctor):
	%	Searches through Goals0 looking for a deconstruction
	%	unification with `Var'. If found, sets `MaybeFunctor'
	% 	to `yes(Functor)', where Functor is the
	%	functor which `Var' gets unified, and
	%	sets `Goals' to be `Goals0' with that deconstruction
	%	unification made deterministic. If not found, sets
	%	`MaybeFunctor' to `no', and computes `Subst' as the
	%	resulting substitution from interpreting through the goal.

:- pred find_bind_var_for_switch(list(hlds__goal), substitution, var,
	list(hlds__goal), substitution, maybe(cons_id)).
:- mode find_bind_var_for_switch(in, in, in, out, out, out) is det.

find_bind_var_for_switch([], Substitution, _Var, [], Substitution, no).
find_bind_var_for_switch([Goal0 - GoalInfo | Goals0], Substitution0, Var,
		[Goal - GoalInfo | Goals], Substitution, MaybeFunctor) :-
	( Goal0 = conj(SubGoals0) ->
		find_bind_var_for_switch(SubGoals0, Substitution0, Var,
			SubGoals, Substitution1, MaybeFunctor1),
		Goal = conj(SubGoals),
		( MaybeFunctor1 = yes(_) ->
			Goals = Goals0,
			Substitution = Substitution1,
			MaybeFunctor = MaybeFunctor1
		;
			find_bind_var_for_switch(Goals0, Substitution0, Var,
				Goals, Substitution, MaybeFunctor)
		)
	; Goal0 = unify(A, B, C, UnifyInfo0, E) ->
			 % otherwise abstractly interpret the unification
		( interpret_unify(A, B, Substitution0, Substitution1) ->
			Substitution2 = Substitution1
		;
			% the unification must fail - just ignore it
			Substitution2 = Substitution0
		),
			% check whether the var was bound
		term__apply_rec_substitution(term__variable(Var), Substitution2,
			Term),
		(
			Term = term__functor(_Name, _Args, _Context),
			UnifyInfo0 = deconstruct(Var1, Functor, F, G, _)
		->
			MaybeFunctor = yes(Functor),
				% The deconstruction unification now becomes
				% deterministic, since the test will get
				% carried out in the switch.
			UnifyInfo = deconstruct(Var1, Functor, F, G,
				cannot_fail),
			Goal = unify(A, B, C, UnifyInfo, E),
			Goals = Goals0,
			Substitution = Substitution2
		;
			Goal = Goal0,
			find_bind_var_for_switch(Goals0, Substitution2, Var,
				Goals, Substitution, MaybeFunctor)
		)
	;
		Goal = Goal0,
		Goals = Goals0,
		Substitution = Substitution0,
		MaybeFunctor = no
	).

interpret_unify(X, var(Y), Subst0, Subst) :-
	term__unify(term__variable(X), term__variable(Y), Subst0, Subst).
interpret_unify(X, functor(Functor, ArgVars), Subst0, Subst) :-
	term__context_init(Context),
	term__var_list_to_term_list(ArgVars, ArgTerms),
	term__unify(term__variable(X),
		term__functor(Functor, ArgTerms, Context),
		Subst0, Subst).
interpret_unify(_X, lambda_goal(_LambdaVars, _Goal), Subst0, Subst) :-
		% For ease of implementation we just ignore unifications with
		% lambda terms.  This is a safe approximation, it just
		% prevents us from optimizing them as well as we would like.
	Subst = Subst0.

:- pred cases_to_switch(assoc_list(cons_id, list(hlds__goal)), var,
	map(var, type), hlds__goal_info, instmap, module_info, hlds__goal_expr).
:- mode cases_to_switch(di, in, in, in, in, in, uo) is det.

cases_to_switch(CasesList, Var, VarTypes, GoalInfo, InstMap, ModuleInfo,
		Goal) :-
	map__lookup(VarTypes, Var, Type),
	( switch_covers_all_cases(CasesList, Type, ModuleInfo) ->
		CanFail = cannot_fail
	;
		CanFail = can_fail
	),
	fix_case_list(CasesList, GoalInfo, Cases0),
	detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo, Cases),
	Goal = switch(Var, CanFail, Cases).

	% check whether a switch handles all the possible
	% constants/functors for the type

:- pred switch_covers_all_cases(assoc_list(cons_id, list(hlds__goal)),
	type, module_info).
:- mode switch_covers_all_cases(in, in, in) is semidet.

switch_covers_all_cases(CasesList, Type, _ModuleInfo) :-
	Type = term__functor(term__atom("character"), [], _),
	list__length(CasesList, 127).	% XXX should be 256
		% NU-Prolog only allows chars '\001' .. '\0177'.
		% Currently we assume the same set.

switch_covers_all_cases(CasesList, Type, ModuleInfo) :-
	type_to_type_id(Type, TypeId, _),
	module_info_types(ModuleInfo, TypeTable),
	map__search(TypeTable, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(_, _, du_type(_, ConsTable, _), _, _),
	map__keys(ConsTable, Constructors),
	list__same_length(CasesList, Constructors).

	% convert the assoc_list(cons_id, list(hlds__goal) back into
	% a plain list(case).

:- pred fix_case_list(assoc_list(cons_id, list(hlds__goal)), hlds__goal_info,
	list(case)).
:- mode fix_case_list(in, in, out) is det.

fix_case_list([], _, []).
fix_case_list([Functor - DisjList | Cases0], GoalInfo,
		[case(Functor, Goal) | Cases]) :-
	disj_list_to_goal(DisjList, GoalInfo, Goal),
	fix_case_list(Cases0, GoalInfo, Cases).

%-----------------------------------------------------------------------------%
