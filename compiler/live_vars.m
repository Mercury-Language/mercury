%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module live_vars.
% Main author: conway.

% Computes the `live_vars', i.e. which variables are either live across a
% call or live at the start of a disjunction, allocates a stack slot for
% each of these variables, and stores this information in the call_info
% structure in the proc_info.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds, llds.

:- pred detect_live_vars(module_info, module_info).
:- mode detect_live_vars(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util.
:- import_module mode_util, int, term, require.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `detect_live_vars_in_goal'
	% for each procedure body.

detect_live_vars(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	detect_live_vars_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred detect_live_vars_in_preds(list(pred_id), module_info, module_info).
:- mode detect_live_vars_in_preds(in, in, out) is det.

detect_live_vars_in_preds([], ModuleInfo, ModuleInfo).
detect_live_vars_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	( pred_info_is_imported(PredInfo) ->
		ModuleInfo1 = ModuleInfo0
	;
		pred_info_procids(PredInfo, ProcIds),
		detect_live_vars_in_procs(ProcIds, PredId, ModuleInfo0,
			ModuleInfo1)
	),
	detect_live_vars_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred detect_live_vars_in_procs(list(proc_id), pred_id, module_info,
					module_info).
:- mode detect_live_vars_in_procs(in, in, in, out) is det.

detect_live_vars_in_procs([], _PredId, ModuleInfo, ModuleInfo).
detect_live_vars_in_procs([ProcId | ProcIds], PredId,
						ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_inferred_determinism(ProcInfo0, Category),

	detect_initial_live_vars(ProcInfo0, ModuleInfo0, Liveness0),
	set__init(LiveVars0),
	detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
					ModuleInfo0, _Liveness, LiveVars),

	set__to_sorted_list(LiveVars, LiveVarList),
	map__init(CallInfo0),
	allocate_live_vars(LiveVarList, 1, Category, CallInfo0, CallInfo),
	proc_info_set_call_info(ProcInfo0, CallInfo, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	detect_live_vars_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_goal(hlds__goal, liveness_info, set(var),
				module_info, liveness_info, set(var)).
:- mode detect_live_vars_in_goal(in, in, in, in, out, out) is det.

detect_live_vars_in_goal(Goal0 - GoalInfo, Liveness0, LiveVars0, ModuleInfo,
					Liveness, LiveVars) :-
	goal_info_pre_delta_liveness(GoalInfo, PreDelta),
	PreDelta = PreBirths - PreDeaths,
	goal_info_pre_delta_liveness(GoalInfo, PostDelta),
	PostDelta = PostBirths - PostDeaths,
	set__difference(Liveness0,  PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),
	set__difference(Liveness2, PostDeaths, Liveness3),

	detect_live_vars_in_goal_2(Goal0, Liveness3, LiveVars0,
					ModuleInfo, Liveness4, LiveVars),

        set__union(Liveness4, PostBirths, Liveness).

%-----------------------------------------------------------------------------%
	% Here we process each of the different sorts of goals.
	% `Liveness' is the set of live variables, i.e. vars which
	% have been referenced and will be referenced again.
	% `LiveVars' is the accumulated set of variables which need a
	% stack slot, i.e. they are live across calls or at the start of
	% a disjunction.

:- pred detect_live_vars_in_goal_2(hlds__goal_expr, liveness_info, set(var),
				module_info, liveness_info, set(var)).
:- mode detect_live_vars_in_goal_2(in, in, in, in, out, out) is det.

detect_live_vars_in_goal_2(conj(Goals0), Liveness0, LiveVars0, ModuleInfo,
							Liveness, LiveVars) :-
	detect_live_vars_in_conj(Goals0, Liveness0, LiveVars0, ModuleInfo,
							Liveness, LiveVars).

detect_live_vars_in_goal_2(disj(Goals0), Liveness0, LiveVars0,
					ModuleInfo, Liveness, LiveVars) :-
	set__union(Liveness0, LiveVars0, LiveVars1),
	detect_live_vars_in_disj(Goals0, Liveness0, LiveVars1,
					ModuleInfo, Liveness, LiveVars).

detect_live_vars_in_goal_2(not(_Vars, Goal0), Liveness0, LiveVars0, ModuleInfo,
					Liveness, LiveVars) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
					ModuleInfo, Liveness, LiveVars).

detect_live_vars_in_goal_2(switch(_Var, Cases0), Liveness0, LiveVars0,
			ModuleInfo, Liveness, LiveVars) :-
	detect_live_vars_in_cases(Cases0, Liveness0, LiveVars0,
				ModuleInfo, Liveness, LiveVars).

detect_live_vars_in_goal_2(if_then_else(_Vars, Cond0, Then0, Else0),
			Liveness0, LiveVars0, ModuleInfo, Liveness, LiveVars) :-
	detect_live_vars_in_goal(Cond0, Liveness0, LiveVars0,
					ModuleInfo, Liveness1, LiveVars1),
	detect_live_vars_in_goal(Then0, Liveness1, LiveVars1,
					ModuleInfo, _Liveness2, LiveVars2),
	detect_live_vars_in_goal(Else0, Liveness0, LiveVars2,
					ModuleInfo, Liveness, LiveVars).

detect_live_vars_in_goal_2(some(_Vars, Goal0), Liveness0, LiveVars0,
			ModuleInfo, Liveness, LiveVars) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
					ModuleInfo, Liveness, LiveVars).

detect_live_vars_in_goal_2(
		call(PredId, ProcId, ArgTerms, Builtin, _SymName, _Follow),
			Liveness0, LiveVars0, ModuleInfo, Liveness, LiveVars) :-
	(
		Builtin = is_builtin
	->
		LiveVars = LiveVars0
	;
		term__vars_list(ArgTerms, ArgVars),
		find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars),
		set__difference(Liveness0, OutVars, NewLiveVars),
		set__union(NewLiveVars, LiveVars0, LiveVars)
	),
	Liveness = Liveness0.

detect_live_vars_in_goal_2(unify(_,_,_,Unification,_), Liveness0, LiveVars0,
				_ModuleInfo, Liveness, LiveVars) :-
	(
		Unification = complicated_unify(_, _, _, _)
	->
		error("Panic - complicated unification")
	;
		Liveness = Liveness0,
		LiveVars = LiveVars0
	).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_conj(list(hlds__goal), liveness_info, set(var),
					module_info, liveness_info, set(var)).
:- mode detect_live_vars_in_conj(in, in, in, in, out, out) is det.

detect_live_vars_in_conj([], Liveness, LiveVars, _M, Liveness, LiveVars).
detect_live_vars_in_conj([Goal0|Goals0], Liveness0, LiveVars0,
			ModuleInfo, Liveness, LiveVars) :-
	(
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, unreachable)
	->
		detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
					ModuleInfo, Liveness, LiveVars)
	;
		detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
					ModuleInfo, Liveness1, LiveVars1),
		detect_live_vars_in_conj(Goals0, Liveness1, LiveVars1,
					ModuleInfo, Liveness, LiveVars)
	).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_disj(list(hlds__goal), liveness_info, set(var),
					module_info, liveness_info,
						set(var)).
:- mode detect_live_vars_in_disj(in, in, in, in, out, out) is det.

detect_live_vars_in_disj([], Liveness, LiveVars,
					_ModuleInfo, Liveness, LiveVars).
detect_live_vars_in_disj([Goal0|Goals0], Liveness0, LiveVars0,
					ModuleInfo, Liveness, LiveVars) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
				ModuleInfo, Liveness1, LiveVars1),
	detect_live_vars_in_disj(Goals0, Liveness0, LiveVars0,
				ModuleInfo, Liveness2, LiveVars2),
	set__union(Liveness1, Liveness2, Liveness),
	set__union(LiveVars1, LiveVars2, LiveVars).

%-----------------------------------------------------------------------------%

:- pred detect_live_vars_in_cases(list(case), liveness_info, set(var),
					module_info, liveness_info, set(var)).
:- mode detect_live_vars_in_cases(in, in, in, in, out, out) is det.

detect_live_vars_in_cases([], Liveness, LiveVars,
				_ModuleInfo, Liveness, LiveVars).
detect_live_vars_in_cases([case(_Cons, Goal0)|Goals0], Liveness0, LiveVars0,
					ModuleInfo, Liveness, LiveVars) :-
	detect_live_vars_in_goal(Goal0, Liveness0, LiveVars0,
			ModuleInfo, Liveness1, LiveVars1),
	detect_live_vars_in_cases(Goals0, Liveness0, LiveVars0,
			ModuleInfo, Liveness2, LiveVars2),
	set__union(Liveness1, Liveness2, Liveness),
	set__union(LiveVars1, LiveVars2, LiveVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred detect_initial_live_vars(proc_info, module_info, set(var)).
:- mode detect_initial_live_vars(in, in, out) is det.

detect_initial_live_vars(ProcInfo, ModuleInfo, Liveness) :-
	proc_info_headvars(ProcInfo, Vars),
	proc_info_argmodes(ProcInfo, Args),
	assoc_list__from_corresponding_lists(Vars, Args, VarArgs),
	set__init(Liveness0),
	detect_initial_live_vars_2(VarArgs, ModuleInfo, Liveness0, Liveness).

:- pred detect_initial_live_vars_2(assoc_list(var,mode), module_info,
							set(var), set(var)).
:- mode detect_initial_live_vars_2(in, in, in, out) is det.

detect_initial_live_vars_2([], _ModuleInfo, Liveness, Liveness).
detect_initial_live_vars_2([V - M|VAs], ModuleInfo,
						Liveness0, Liveness) :-
	(
		mode_is_input(ModuleInfo, M)
	->
		set__insert(Liveness0, V, Liveness1)
	;
		Liveness1 = Liveness0
	),
	detect_initial_live_vars_2(VAs, ModuleInfo, Liveness1, Liveness).

%-----------------------------------------------------------------------------%

:- pred find_output_vars(pred_id, proc_id, list(var), module_info, set(var)).
:- mode find_output_vars(in, in, in, in, out) is det.

find_output_vars(PredId, ProcId, ArgVars, ModuleInfo, OutVars) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgPairs),
	set__init(OutVars0),
	find_output_vars_2(ArgPairs, OutVars0, OutVars).

:- pred find_output_vars_2(assoc_list(var, arg_info), set(var), set(var)).
:- mode find_output_vars_2(in, in, out) is det.

find_output_vars_2([], OutVars, OutVars).
find_output_vars_2([Var - arg_info(_, Mode)|Rest], OutVars0, OutVars) :-
	(
		Mode = top_out
	->
		set__insert(OutVars0, Var, OutVars1)
	;
		OutVars1 = OutVars0
	),
	find_output_vars_2(Rest, OutVars1, OutVars).

%-----------------------------------------------------------------------------%

:- pred allocate_live_vars(list(var), int, category,
					map(var, lval), map(var, lval)).
:- mode allocate_live_vars(in, in, in, in, out) is det.

allocate_live_vars([], _N, _Category, CallInfo, CallInfo).
allocate_live_vars([Var|Vars], N0, Category, CallInfo0, CallInfo) :-
	(
		Category = nondeterministic
	->
		Lval = framevar(N0)
	;
		Lval = stackvar(N0)
	),
	map__set(CallInfo0, Var, Lval, CallInfo1),
	N1 is N0 + 1,
	allocate_live_vars(Vars, N1, Category, CallInfo1, CallInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
