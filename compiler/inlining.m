%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module inlining.
% Main author: conway.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds, llds.

:- pred inlining(module_info, module_info).
:- mode inlining(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util.
:- import_module mode_util, term, require, quantification.
:- import_module arg_info, varset, code_aux, prog_io.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `inlining__do_inlining'
	% for each procedure body.

inlining(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	inlining_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred inlining_in_preds(list(pred_id), module_info, module_info).
:- mode inlining_in_preds(in, in, out) is det.

inlining_in_preds([], ModuleInfo, ModuleInfo).
inlining_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	( pred_info_is_imported(PredInfo) ->
		ModuleInfo1 = ModuleInfo0
	;
		pred_info_procids(PredInfo, ProcIds),
		inlining_in_procs(ProcIds, PredId, ModuleInfo0,
			ModuleInfo1)
	),
	inlining_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred inlining_in_procs(list(proc_id), pred_id, module_info,
					module_info).
:- mode inlining_in_procs(in, in, in, out) is det.

inlining_in_procs([], _PredId, ModuleInfo, ModuleInfo).
inlining_in_procs([ProcId | ProcIds], PredId, ModuleInfo0,
					ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_headvars(ProcInfo0, HeadVars),

	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo0,
		Goal1, Varset, VarTypes),
	implicitly_quantify_clause_body(HeadVars, Goal1, Goal),

	proc_info_set_variables(ProcInfo0, Varset, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	inlining_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal(hlds__goal, varset, map(var, type),
			module_info, hlds__goal, varset, map(var, type)).
:- mode inlining__inlining_in_goal(in, in, in, in, out, out, out) is det.

inlining__inlining_in_goal(Goal0 - GoalInfo, Varset0, VarTypes0, ModuleInfo,
		Goal - GoalInfo, Varset, VarTypes) :-
	inlining__inlining_in_goal_2(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_goal_2(hlds__goal_expr, varset, map(var, type),
		module_info, hlds__goal_expr, varset, map(var, type)).
:- mode inlining__inlining_in_goal_2(in, in, in, in, out, out, out) is det.

inlining__inlining_in_goal_2(conj(Goals0), Varset0, VarTypes0, ModuleInfo,
		conj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_conj(Goals0, Varset0, VarTypes0, ModuleInfo,
		Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(disj(Goals0), Varset0, VarTypes0, ModuleInfo,
		disj(Goals), Varset, VarTypes) :-
	inlining__inlining_in_disj(Goals0, Varset0, VarTypes0, ModuleInfo,
		Goals, Varset, VarTypes).

inlining__inlining_in_goal_2(switch(Var, Det, Cases0), Varset0, VarTypes0,
		ModuleInfo, switch(Var, Det, Cases), Varset, VarTypes) :-
	inlining__inlining_in_cases(Cases0, Varset0, VarTypes0, ModuleInfo,
		Cases, Varset, VarTypes).

inlining__inlining_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), Varset0,
		VarTypes0, ModuleInfo, if_then_else(Vars, Cond, Then, Else),
		Varset, VarTypes) :-
	inlining__inlining_in_goal(Cond0, Varset0, VarTypes0, ModuleInfo,
		Cond, Varset1, VarTypes1),
	inlining__inlining_in_goal(Then0, Varset1, VarTypes1, ModuleInfo,
		Then, Varset2, VarTypes2),
	inlining__inlining_in_goal(Else0, Varset2, VarTypes2, ModuleInfo,
		Else, Varset, VarTypes).

inlining__inlining_in_goal_2(not(Vars, Goal0), Varset0, VarTypes0, ModuleInfo,
		not(Vars, Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(some(Vars, Goal0), Varset0, VarTypes0, ModuleInfo,
		some(Vars, Goal), Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset, VarTypes).

inlining__inlining_in_goal_2(
		call(PredId, ProcId, Args, Builtin, SymName, Follow),
		Varset0, VarTypes0, ModuleInfo, Goal, Varset, VarTypes) :-
	(
		Builtin = not_builtin,
        	module_info_preds(ModuleInfo, Preds),
        	map__lookup(Preds, PredId, PredInfo),
		\+ pred_info_is_imported(PredInfo),
        	pred_info_procedures(PredInfo, Procs),
        	map__lookup(Procs, ProcId, ProcInfo),
        	proc_info_goal(ProcInfo, CalledGoal),
			% this heuristic could be improved
		code_aux__contains_only_builtins(CalledGoal),
		code_aux__goal_is_flat(CalledGoal)
	->
		proc_info_headvars(ProcInfo, HeadVars),
        	proc_info_variables(ProcInfo, PVarset),
		proc_info_vartypes(ProcInfo, CVarTypes),
		varset__vars(PVarset, Vars0),
		term__vars_list(Args, ArgVars),
		map__init(Subn0),
		assoc_list__from_corresponding_lists(ArgVars, HeadVars, ArgSub),
		inlining__init_subn(ArgSub, Subn0, Subn1),
		inlining__create_variables(Vars0, Varset0, VarTypes0,
			Subn1, CVarTypes, Varset, VarTypes, Subn),
		inlining__name_apart(CalledGoal, Subn, NewGoal),
		goal_to_conj_list(NewGoal, GoalList),
		(
			GoalList = [SingleGoal - _]
		->
			Goal = SingleGoal
		;
			Goal = conj(GoalList)
		)
	;
		Goal = call(PredId, ProcId, Args, Builtin, SymName, Follow),
		Varset = Varset0,
		VarTypes = VarTypes0
	).

inlining__inlining_in_goal_2(unify(A,B,C,D,E), Varset, VarTypes, _,
					unify(A,B,C,D,E), Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_disj(list(hlds__goal), varset, map(var, type),
			module_info, list(hlds__goal), varset, map(var, type)).
:- mode inlining__inlining_in_disj(in, in, in, in, out, out, out) is det.

inlining__inlining_in_disj([], Varset, VarTypes, _ModuleInfo,
				[], Varset, VarTypes).
inlining__inlining_in_disj([Goal0|Goals0], Varset0, VarTypes0, ModuleInfo,
		[Goal|Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset1, VarTypes1),
	inlining__inlining_in_disj(Goals0, Varset1, VarTypes1, ModuleInfo,
		Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_cases(list(case), varset, map(var, type), module_info,
		list(case), varset, map(var, type)).
:- mode inlining__inlining_in_cases(in, in, in, in, out, out, out) is det.

inlining__inlining_in_cases([], Varset, VarTypes, _ModuleInfo,
		[], Varset, VarTypes).
inlining__inlining_in_cases([case(Cons, Goal0)|Goals0], Varset0, VarTypes0,
		ModuleInfo, [case(Cons, Goal)|Goals], Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal, Varset1, VarTypes1),
	inlining__inlining_in_cases(Goals0, Varset1, VarTypes1, ModuleInfo,
		Goals, Varset, VarTypes).

%-----------------------------------------------------------------------------%

:- pred inlining__inlining_in_conj(list(hlds__goal), varset, map(var, type),
		module_info, list(hlds__goal), varset, map(var, type)).
:- mode inlining__inlining_in_conj(in, in, in, in, out, out, out) is det.

	% Since a single goal may become a conjunction,
	% we flatten the conjunction as we go.

inlining__inlining_in_conj([], Varset, VarTypes, _ModuleInfo,
		[], Varset, VarTypes).
inlining__inlining_in_conj([Goal0 | Goals0], Varset0, VarTypes0, ModuleInfo,
		Goals, Varset, VarTypes) :-
	inlining__inlining_in_goal(Goal0, Varset0, VarTypes0, ModuleInfo,
		Goal1, Varset1, VarTypes1),
	goal_to_conj_list(Goal1, Goal1List),
	inlining__inlining_in_conj(Goals0, Varset1, VarTypes1, ModuleInfo,
		Goals1, Varset, VarTypes),
	list__append(Goal1List, Goals1, Goals).

%-----------------------------------------------------------------------------%

:- pred inlining__create_variables(list(var), varset,
		map(var, type), map(var, var), map(var, type),
				varset, map(var, type), map(var, var)).
:- mode inlining__create_variables(in, in, in, in, in, out, out, out) is det.

inlining__create_variables([], Varset, VarTypes, Subn, _CVars,
		Varset, VarTypes, Subn).
inlining__create_variables([V|Vs], Varset0, VarTypes0, Subn0, CVars,
		Varset, VarTypes, Subn) :-
	(
		map__contains(Subn0, V)
	->
		NV = V,
		Varset1 = Varset0,
		Subn1 = Subn0,
		VarTypes1 = VarTypes0
	;
		varset__new_var(Varset0, NV, Varset1),
		map__set(Subn0, V, NV, Subn1),
		map__lookup(CVars, V, VT),
		map__set(VarTypes0, NV, VT, VarTypes1)
	),
	inlining__create_variables(Vs, Varset1, VarTypes1, Subn1, CVars,
		Varset, VarTypes, Subn).

%-----------------------------------------------------------------------------%

:- pred inlining__init_subn(assoc_list(var, var), map(var, var), map(var, var)).
:- mode inlining__init_subn(in, in, out) is det.

inlining__init_subn([], Subn, Subn).
inlining__init_subn([A-H|Vs], Subn0, Subn) :-
	map__set(Subn0, H, A, Subn1),
	inlining__init_subn(Vs, Subn1, Subn).

%-----------------------------------------------------------------------------%

:- pred inlining__rename_headvars(list(var), map(var, var), list(var)).
:- mode inlining__rename_headvars(in, in, out) is det.

inlining__rename_headvars([], _Subn, []).
inlining__rename_headvars([V|Vs], Subn, [N|Ns]) :-
	map__lookup(Subn, V, N),
	inlining__rename_headvars(Vs, Subn, Ns).

%-----------------------------------------------------------------------------%

:- pred inlining__name_apart(hlds__goal, map(var, var), hlds__goal).
:- mode inlining__name_apart(in, in, out) is det.

inlining__name_apart(Goal0 - GoalInfo0, Subn, Goal - GoalInfo) :-
	inlining__name_apart_2(Goal0, Subn, Goal),
	inlining__name_apart_goalinfo(GoalInfo0, Subn, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred inlining__name_apart_2(hlds__goal_expr, map(var, var), hlds__goal_expr).
:- mode inlining__name_apart_2(in, in, out) is det.

inlining__name_apart_2(conj(Goals0), Subn, conj(Goals)) :-
	inlining__name_apart_list(Goals0, Subn, Goals).

inlining__name_apart_2(disj(Goals0), Subn, disj(Goals)) :-
	inlining__name_apart_list(Goals0, Subn, Goals).

inlining__name_apart_2(switch(Var0, Det, Cases0), Subn,
						switch(Var, Det, Cases)) :-
	map__lookup(Subn, Var0, Var),
	inlining__name_apart_cases(Cases0, Subn, Cases).

inlining__name_apart_2(if_then_else(Vars0, Cond0, Then0, Else0), Subn,
				if_then_else(Vars, Cond, Then, Else)) :-
	inlining__rename_headvars(Vars0, Subn, Vars),
	inlining__name_apart(Cond0, Subn, Cond),
	inlining__name_apart(Then0, Subn, Then),
	inlining__name_apart(Else0, Subn, Else).

inlining__name_apart_2(not(Vars0, Goal0), Subn, not(Vars, Goal)) :-
	inlining__rename_headvars(Vars0, Subn, Vars),
	inlining__name_apart(Goal0, Subn, Goal).

inlining__name_apart_2(some(Vars0, Goal0), Subn, some(Vars, Goal)) :-
	inlining__rename_headvars(Vars0, Subn, Vars),
	inlining__name_apart(Goal0, Subn, Goal).

inlining__name_apart_2(
		call(PredId, ProcId, Args0, Builtin, SymName, Follow0), Subn,
		call(PredId, ProcId, Args, Builtin, SymName, Follow)) :-
	inlining__rename_args(Args0, Subn, Args),
	inlining__rename_follow_vars(Follow0, Subn, Follow).

inlining__name_apart_2(unify(TermL0,TermR0,Mode,Unify0,Context), Subn,
		unify(TermL,TermR,Mode,Unify,Context)) :-
	inlining__rename_term(TermL0, Subn, TermL),
	inlining__rename_term(TermR0, Subn, TermR),
	inlining__rename_unify(Unify0, Subn, Unify).

%-----------------------------------------------------------------------------%

:- pred inlining__name_apart_list(list(hlds__goal), map(var, var),
							list(hlds__goal)).
:- mode inlining__name_apart_list(in, in, out) is det.

inlining__name_apart_list([], _Subn, []).
inlining__name_apart_list([G0|Gs0], Subn, [G|Gs]) :-
	inlining__name_apart(G0, Subn, G),
	inlining__name_apart_list(Gs0, Subn, Gs).

%-----------------------------------------------------------------------------%

:- pred inlining__name_apart_cases(list(case), map(var, var), list(case)).
:- mode inlining__name_apart_cases(in, in, out) is det.

inlining__name_apart_cases([], _Subn, []).
inlining__name_apart_cases([case(Cons, G0)|Gs0], Subn, [case(Cons, G)|Gs]) :-
	inlining__name_apart(G0, Subn, G),
	inlining__name_apart_cases(Gs0, Subn, Gs).

%-----------------------------------------------------------------------------%

:- pred inlining__rename_args(list(term), map(var, var), list(term)).
:- mode inlining__rename_args(in, in, out) is det.

inlining__rename_args([], _Subn, []).
inlining__rename_args([T0|Ts0], Subn, [T|Ts]) :-
	inlining__rename_term(T0, Subn, T),
	inlining__rename_args(Ts0, Subn, Ts).

:- pred inlining__rename_term(term, map(var, var), term).
:- mode inlining__rename_term(in, in, out) is det.

inlining__rename_term(term__variable(V), Subn, term__variable(N)) :-
	map__lookup(Subn, V, N).
inlining__rename_term(term__functor(Cons, Terms0, Cont), Subn,
				term__functor(Cons, Terms, Cont)) :-
	inlining__rename_args(Terms0, Subn, Terms).

%-----------------------------------------------------------------------------%

:- pred inlining__rename_unify(unification, map(var, var), unification).
:- mode inlining__rename_unify(in, in, out) is det.

inlining__rename_unify(construct(Var0, ConsId, Vars0, Modes), Subn,
			construct(Var, ConsId, Vars, Modes)) :-
	map__lookup(Subn, Var0, Var),
	inlining__rename_headvars(Vars0, Subn, Vars).
inlining__rename_unify(deconstruct(Var0, ConsId, Vars0, Modes, Cat), Subn,
			deconstruct(Var, ConsId, Vars, Modes, Cat)) :-
	map__lookup(Subn, Var0, Var),
	inlining__rename_headvars(Vars0, Subn, Vars).
inlining__rename_unify(assign(L0, R0), Subn, assign(L, R)) :-
	map__lookup(Subn, L0, L),
	map__lookup(Subn, R0, R).
inlining__rename_unify(simple_test(L0, R0), Subn, simple_test(L, R)) :-
	map__lookup(Subn, L0, L),
	map__lookup(Subn, R0, R).
inlining__rename_unify(complicated_unify(Modes, Cat, Follow0), Subn,
			complicated_unify(Modes, Cat, Follow)) :-
	inlining__rename_follow_vars(Follow0, Subn, Follow).

%-----------------------------------------------------------------------------%

:- pred inlining__rename_follow_vars(map(var, T), map(var, var), map(var, T)).
:- mode inlining__rename_follow_vars(in, in, out) is det.

inlining__rename_follow_vars(Follow0, Subn, Follow) :-
	map__to_assoc_list(Follow0, FollowList0),
	inlining__rename_follow_vars_2(FollowList0, Subn, FollowList),
	map__from_assoc_list(FollowList, Follow).

:- pred inlining__rename_follow_vars_2(assoc_list(var, T),
				map(var, var), assoc_list(var, T)).
:- mode inlining__rename_follow_vars_2(in, in, out) is det.

inlining__rename_follow_vars_2([], _Subn, []).
inlining__rename_follow_vars_2([V - L | Vs], Subn, [N - L | Ns]) :-
	map__lookup(Subn, V, N),
	inlining__rename_follow_vars_2(Vs, Subn, Ns).

%-----------------------------------------------------------------------------%

:- pred inlining__name_apart_goalinfo(hlds__goal_info,
					map(var, var), hlds__goal_info).
:- mode inlining__name_apart_goalinfo(in, in, out) is det.

inlining__name_apart_goalinfo(GoalInfo0, Subn, GoalInfo) :-
	goal_info_pre_delta_liveness(GoalInfo0, PreBirths0 - PreDeaths0),
	inlining__name_apart_set(PreBirths0, Subn, PreBirths),
	inlining__name_apart_set(PreDeaths0, Subn, PreDeaths),
	goal_info_set_pre_delta_liveness(GoalInfo0, PreBirths - PreDeaths,
						GoalInfo1),

	goal_info_post_delta_liveness(GoalInfo1, PostBirths0 - PostDeaths0),
	inlining__name_apart_set(PostBirths0, Subn, PostBirths),
	inlining__name_apart_set(PostDeaths0, Subn, PostDeaths),
	goal_info_set_pre_delta_liveness(GoalInfo1, PostBirths - PostDeaths,
						GoalInfo2),

	goal_info_get_nonlocals(GoalInfo2, NonLocals0),
	inlining__name_apart_set(NonLocals0, Subn, NonLocals),
	goal_info_set_nonlocals(GoalInfo2, NonLocals, GoalInfo3),

	goal_info_get_instmap_delta(GoalInfo3, MaybeInstMap0),
	(
		MaybeInstMap0 = reachable(InstMap0)
	->
		inlining__rename_follow_vars(InstMap0, Subn, InstMap),
		MaybeInstMap = reachable(InstMap)
	;
		MaybeInstMap = MaybeInstMap0
	),
	goal_info_set_instmap_delta(GoalInfo3, MaybeInstMap, GoalInfo4),

	goal_info_store_map(GoalInfo4, MaybeStoreMap0),
	(
		MaybeStoreMap0 = yes(StoreMap0)
	->
		inlining__rename_follow_vars(StoreMap0, Subn, StoreMap),
		MaybeStoreMap = yes(StoreMap)
	;
		MaybeStoreMap = MaybeStoreMap0
	),
	goal_info_set_store_map(GoalInfo4, MaybeStoreMap, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred inlining__name_apart_set(set(var), map(var, var), set(var)).
:- mode inlining__name_apart_set(in, in, out) is det.

inlining__name_apart_set(Vars0, Subn, Vars) :-
	set__to_sorted_list(Vars0, VarsList0),
	inlining__rename_headvars(VarsList0, Subn, VarsList),
	set__list_to_set(VarsList, Vars).

%-----------------------------------------------------------------------------%

:- pred inlining__argument_unifications(list(mode), list(var), list(var),
		module_info, list(hlds__goal), list(hlds__goal)).
:- mode inlining__argument_unifications(in, in, in, in, out, out) is semidet.

inlining__argument_unifications([], [], [], _, [], []).
inlining__argument_unifications([Mode|ArgInfos],
		[V|Vs], [H|Hs], ModuleInfo, Before, After) :-
	inlining__argument_unifications(ArgInfos, Vs, Hs, ModuleInfo,
		Before0, After0),
	(
		mode_is_input(ModuleInfo, Mode)
	->
		Context = unify_context(explicit, []),
			% XXX unify context is wrong
		Unify = unify(term__variable(H), term__variable(V),
				((free -> ground) - (ground -> ground)),
				assign(H, V), Context),
			% XXX bug! the mode is wrong
		goal_info_init(GoalInfo0),
		map__init(Inst0),
		map__insert(Inst0, H, ground, Inst),
		goal_info_set_instmap_delta(GoalInfo0,
					reachable(Inst), GoalInfo),
		Before = [ Unify - GoalInfo | Before0 ],
		After = After0
	;
		mode_is_output(ModuleInfo, Mode)
	->
		Context = unify_context(explicit, []),
		Unify = unify(term__variable(V), term__variable(H),
				((free -> ground) - (ground -> ground)),
				assign(V, H), Context),
		goal_info_init(GoalInfo0),
		map__init(Inst0),
		map__insert(Inst0, V, ground, Inst),
		goal_info_set_instmap_delta(GoalInfo0,
					reachable(Inst), GoalInfo),
		Before = Before0,
		After = [ Unify - GoalInfo | After0 ]
	;
		Before = Before0,
		After = After0
	).

%-----------------------------------------------------------------------------%
