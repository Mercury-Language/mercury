%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Switch Detection - replace disjunctions with (semi)deterministic switch
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
:- import_module list, map, set, std_util, require.
:- import_module mode_util, type_util, term.

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
	% replace disjunctions with switches where-ever possible.

:- pred detect_switches_in_goal(hlds__goal, instmap, map(var, type),
			module_info, hlds__goal).
:- mode detect_switches_in_goal(in, in, in, in, out) is det.

detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo,
			Goal, _InstMap).

	% This version is the same as the above except that it returns
	% the resulting instmap on exit from the goal, which is
	% computed by applying the instmap delta specified in the
	% goal's goalinfo.

:- pred detect_switches_in_goal(hlds__goal, instmap, map(var, type),
			module_info, hlds__goal, instmap).
:- mode detect_switches_in_goal(in, in, in, in, out, out) is det.

detect_switches_in_goal(Goal0 - GoalInfo, InstMap0, VarTypes, ModuleInfo,
		Goal - GoalInfo, InstMap) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	detect_switches_in_goal_2(Goal0, GoalInfo, InstMap0, InstMapDelta,
		VarTypes, ModuleInfo, Goal),
	apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

	% Here we process each of the different sorts of goals.

:- pred detect_switches_in_goal_2(hlds__goal_expr, hlds__goal_info, instmap,
		instmap, map(var, type), module_info,
		hlds__goal_expr).
:- mode detect_switches_in_goal_2(in, in, in, in, in, in, out) is det.

detect_switches_in_goal_2(conj(Goals0), _GoalInfo, InstMap0, _InstMapDelta,
		VarTypes, ModuleInfo, conj(Goals)) :-
	detect_switches_in_conj(Goals0, InstMap0, VarTypes, ModuleInfo,
		Goals).

detect_switches_in_goal_2(disj(Goals0), GoalInfo, InstMap0, InstMapDelta,
		VarTypes, ModuleInfo, Goal) :-
	( Goals0 = [] ->
		Goal = disj([])
	;
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		set__to_sorted_list(NonLocals, NonLocalsList),
		detect_switches_in_disj(NonLocalsList, Goals0, GoalInfo, 
			InstMap0, InstMapDelta, VarTypes, ModuleInfo, Goal)
	).

detect_switches_in_goal_2(not(Vars, Goal0), _GoalInfo, InstMap0, _InstMapDelta,
		VarTypes, ModuleInfo, not(Vars, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), _GoalInfo,
			InstMap0, _InstMapDelta, VarTypes, ModuleInfo,
			if_then_else(Vars, Cond, Then, Else)) :-
	detect_switches_in_goal(Cond0, InstMap0, VarTypes, ModuleInfo, Cond,
		InstMap1),
	detect_switches_in_goal(Then0, InstMap1, VarTypes, ModuleInfo, Then),
	detect_switches_in_goal(Else0, InstMap0, VarTypes, ModuleInfo, Else).

detect_switches_in_goal_2(some(Vars, Goal0), _GoalInfo, InstMap0, _InstMapDelta,
		VarTypes, ModuleInfo, some(Vars, Goal)) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal).

detect_switches_in_goal_2(call(A,B,C,D,E,F), _, _, _, _, _, call(A,B,C,D,E,F)).

detect_switches_in_goal_2(unify(A,B,C,D,E), _, _, _, _, _, unify(A,B,C,D,E)).

detect_switches_in_goal_2(switch(_,_,_), _, _, _, _, _, _) :-
	error("detect_switches run twice?").

%-----------------------------------------------------------------------------%

	% This is the interesting bit - we've found a non-empty
	% disjunction, and we've got a list of the non-local variables
	% of that disjunction.  Now for each non-local variable, we
	% check whether there is a partition of the disjuncts such that
	% each group of disjunctions can only succeed if the variable
	% is bound to a different functor.

:- pred detect_switches_in_disj(list(var), list(hlds__goal), hlds__goal_info,
		instmap, instmap, map(var, type), module_info,
		hlds__goal_expr).
:- mode detect_switches_in_disj(in, in, in, in, in, in, in, out) is det.

detect_switches_in_disj([], Goals0, _, InstMap, _, VarTypes, ModuleInfo,
		disj(Goals)) :-
	detect_switches_in_disj_2(Goals0, InstMap, VarTypes, ModuleInfo,
		Goals).
	
detect_switches_in_disj([Var | Vars], Goals0, GoalInfo0, InstMap, InstMapDelta,
		VarTypes, ModuleInfo, Goal) :-
	(
		instmap_lookup_var(InstMap, Var, VarInst0),
		inst_is_bound(ModuleInfo, VarInst0),
		partition_disj(Goals0, Var, GoalInfo0, VarTypes, ModuleInfo,
			Cases0, Det)
	->
		% It might be a good idea to convert switches
		% with only one case into something simpler
		detect_switches_in_cases(Cases0, InstMap, VarTypes, ModuleInfo,
			Cases),
		Goal = switch(Var, Det, Cases)
	;
		detect_switches_in_disj(Vars, Goals0, GoalInfo0,
			InstMap, InstMapDelta, VarTypes, ModuleInfo, Goal)
	).

%-----------------------------------------------------------------------------%

	% partition_disj(Goals, Var, GoalInfo, VarTypes, ModuleInfo,
	%			Cases, Det):
	%	Attempts to partition the disjunction `Goals' into a switch
	%	on `Var'.  If successful, returns the resulting `Cases',
	%	and returns the local determinism of the switch in `Det'.

	% Given the list of goals in a disjunction, and an input variable
	% to switch on, we attempt to partition the goals into a switch.
	% For each constructor id, we record the list of disjuncts
	% which unify the variable with that constructor.
	% We partition the goals by abstractly interpreting the unifications
	% at the start of each disjunction, to build up a substitution.
	% We by finding unifications with that variable

:- type cases == map(cons_id, list(hlds__goal)).

:- pred partition_disj(list(hlds__goal), var, hlds__goal_info, map(var, type),
		module_info, list(case), category).
:- mode partition_disj(in, in, in, in, in, out, out) is semidet.

partition_disj(Goals0, Var, GoalInfo, VarTypes, ModuleInfo, CaseList, Det) :-
	map__init(Cases0),
	partition_disj_2(Goals0, Var, Cases0, Cases),
	map__to_assoc_list(Cases, CasesAssocList),
	CasesAssocList \= [_],	% there must be more than one case
	map__lookup(VarTypes, Var, Type),
	( 
		switch_covers_all_cases(CasesAssocList, Type,  ModuleInfo)
	->
		Det = deterministic
	;
		Det = semideterministic
	),
	fix_case_list(CasesAssocList, GoalInfo, CaseList).

:- pred partition_disj_2(list(hlds__goal), var, cases, cases).
:- mode partition_disj_2(in, in, in, out) is semidet.

partition_disj_2([], _Var, Cases, Cases).
partition_disj_2([Goal0 | Goals], Var, Cases0, Cases) :-
	goal_to_conj_list(Goal0, ConjList0),
	Goal0 = _ - GoalInfo,
	map__init(Substitution),
	find_bind_var(ConjList0, Var, Substitution,
			Functor, ConjList), % may fail
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	( map__search(Cases0, Functor, DisjList0) ->
		DisjList1 = [Goal | DisjList0]
	;
		DisjList1 = [Goal]
	),
	map__set(Cases0, Functor, DisjList1, Cases1),
	partition_disj_2(Goals, Var, Cases1, Cases).


	% find_unify_var_functor(Goals0, Var, ConsId, Goals):
	%	Searches through Goals0 looking for a deconstruction
	%	unification with `Var'.  If successful, returns the
	%	functor which `Var' gets unified with as `ConsId',
	%	sets `Goals' to be `Goals0' with that deconstruction
	%	unification made deterministic.

:- pred find_bind_var(list(hlds__goal), var, substitution, cons_id, 
                       list(hlds__goal)).
:- mode find_bind_var(in, in, in, out, out) is semidet.

find_bind_var([Goal0 - GoalInfo | Goals0], Var, Substitution0,
		Functor, [Goal - GoalInfo | Goals]) :-
		% fail if the next goal is not a unification
	( Goal0 = unify(A, B, C, UnifyInfo0, E) ->
			 % otherwise abstractly interpret the unification
		term__unify(A, B, Substitution0, Substitution),
			% check whether the var was bound
		term__apply_rec_substitution(term__variable(Var), Substitution,
			Term),
		(
			Term = term__functor(_Name, _Args, _Context),
			UnifyInfo0 = deconstruct(Var1, Functor0, F, G, _Det)
		->
			Functor = Functor0,
				% The deconstruction unification now becomes
				% deterministic, since the test will get
				% carried out in the switch.
			UnifyInfo = deconstruct(Var1, Functor0, F, G,
					deterministic),
			Goal = unify(A, B, C, UnifyInfo, E),
			Goals = Goals0
		;
			Goal = Goal0,
			find_bind_var(Goals0, Var, Substitution, Functor, Goals)
		)
	;
		fail
	).

	% check whether a switch handles all the possible
	% constants/functors for the type

:- pred switch_covers_all_cases(list(_Cases), type, module_info).
:- mode switch_covers_all_cases(in, in, in) is semidet.

switch_covers_all_cases(CasesList, Type, _ModuleInfo) :-
	Type = term__functor(term__atom("character"), [], _),
	list__length(CasesList, 127).	% XXX should be 255
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

:- pred detect_switches_in_disj_2(list(hlds__goal), instmap, map(var, type),
				module_info, list(hlds__goal)).
:- mode detect_switches_in_disj_2(in, in, in, in, out) is det.

detect_switches_in_disj_2([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_disj_2([Goal0 | Goals0], InstMap0, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal),
	detect_switches_in_disj_2(Goals0, InstMap0, VarTypes, ModuleInfo,
		Goals).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- pred detect_switches_in_conj(list(hlds__goal), instmap, map(var, type),
				module_info, list(hlds__goal)).
:- mode detect_switches_in_conj(in, in, in, in, out) is det.

detect_switches_in_conj([], _InstMap, _VarTypes, _ModuleInfo, []).
detect_switches_in_conj([Goal0 | Goals0], InstMap0, VarTypes, ModuleInfo,
		[Goal | Goals]) :-
	detect_switches_in_goal(Goal0, InstMap0, VarTypes, ModuleInfo, Goal,
		InstMap1),
	detect_switches_in_conj(Goals0, InstMap1, VarTypes, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
