%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Original author: conway.
% Extensive modification by zs.

% Allocates the storage location for each live variable at the end of
% each branched structure, so that the code generator will generate code
% which puts the variable in the same place in each branch.

% This module requires arg_infos and livenesses to have already been computed,
% and stack slots allocated.

% If the appropriate option is set, the code calls the follow_vars module
% to help guide its decisions.

% See compiler/notes/allocation.html for a description of the framework that
% this pass operates within.

%-----------------------------------------------------------------------------%

:- module ll_backend__store_alloc.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- type store_map_run_type
	--->	final_allocation
	;	for_stack_opt.

:- pred allocate_store_maps(store_map_run_type::in, pred_id::in,
	module_info::in, proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module hlds__instmap.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__trace_params.
:- import_module ll_backend__code_util.
:- import_module ll_backend__follow_vars.
:- import_module ll_backend__liveness.
:- import_module ll_backend__llds.
:- import_module ll_backend__trace.
:- import_module parse_tree__prog_data.

:- import_module bool, int, require.
:- import_module list, map, set, std_util, assoc_list.

%-----------------------------------------------------------------------------%

allocate_store_maps(RunType, PredId, ModuleInfo, !ProcInfo) :-
	module_info_globals(ModuleInfo, Globals),
	( RunType = final_allocation ->
		proc_info_goal(!.ProcInfo, Goal0),

		find_final_follow_vars(!.ProcInfo,
			FollowVarsMap0, NextNonReserved0),
		proc_info_vartypes(!.ProcInfo, VarTypes),
		find_follow_vars_in_goal(Goal0, Goal1, VarTypes, ModuleInfo,
			FollowVarsMap0, FollowVarsMap,
			NextNonReserved0, NextNonReserved),
		Goal1 = GoalExpr1 - GoalInfo1,
		FollowVars = abs_follow_vars(FollowVarsMap, NextNonReserved),
		goal_info_set_follow_vars(GoalInfo1, yes(FollowVars),
			GoalInfo2),
		Goal2 = GoalExpr1 - GoalInfo2
	;
		proc_info_goal(!.ProcInfo, Goal2)
	),
	initial_liveness(!.ProcInfo, PredId, ModuleInfo, Liveness0),
	globals__get_trace_level(Globals, TraceLevel),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	( eff_trace_level_is_none(PredInfo, !.ProcInfo, TraceLevel) = no ->
		trace__fail_vars(ModuleInfo, !.ProcInfo, ResumeVars0)
	;
		set__init(ResumeVars0)
	),
	build_input_arg_list(!.ProcInfo, InputArgLvals),
	LastLocns0 = initial_last_locns(InputArgLvals),
	proc_info_stack_slots(!.ProcInfo, StackSlots),
	StoreAllocInfo = store_alloc_info(ModuleInfo, StackSlots),
	store_alloc_in_goal(Goal2, Goal, Liveness0, _, LastLocns0, _,
		ResumeVars0, StoreAllocInfo),
	proc_info_set_goal(Goal, !ProcInfo).

:- func initial_last_locns(assoc_list(prog_var, lval)) = last_locns.

initial_last_locns([]) = map__init.
initial_last_locns([Var - Lval | VarLvals]) =
	map__det_insert(initial_last_locns(VarLvals), Var,
		set__make_singleton_set(Lval)).

%-----------------------------------------------------------------------------%

:- type store_alloc_info
	--->	store_alloc_info(
			module_info		:: module_info,
			stack_slots		:: stack_slots
						% maps each var to its stack
						% slot (if it has one)
		).

:- type where_stored	== set(lval).	% These lvals may contain var() rvals.

:- type last_locns	== map(prog_var, where_stored).

:- pred store_alloc_in_goal(hlds_goal::in, hlds_goal::out,
	liveness_info::in, liveness_info::out, last_locns::in, last_locns::out,
	set(prog_var)::in, store_alloc_info::in) is det.

store_alloc_in_goal(Goal0 - GoalInfo0, Goal - GoalInfo, Liveness0, Liveness,
		!LastLocns, ResumeVars0, StoreAllocInfo) :-
	% note: we must be careful to apply deaths before births
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
	goal_info_get_pre_births(GoalInfo0, PreBirths),
	goal_info_get_post_deaths(GoalInfo0, PostDeaths),
	goal_info_get_post_births(GoalInfo0, PostBirths),

	set__difference(Liveness0,  PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),
	store_alloc_in_goal_2(Goal0, Goal, Liveness2, Liveness3,
		!LastLocns, ResumeVars0, PostDeaths, StoreAllocInfo),
	set__difference(Liveness3, PostDeaths, Liveness4),
	% If any variables magically become live in the PostBirths,
	% then they have to mundanely become live in a parallel goal,
	% so we don't need to allocate anything for them here.
	set__union(Liveness4, PostBirths, Liveness),
	( goal_util__goal_is_branched(Goal) ->
		% Any variables that become magically live at the
		% end of the goal should not be included in the store map.
		% That is why we use Liveness4 instead of Liveness here.
		set__union(Liveness4, ResumeVars0, MappedSet),
		set__to_sorted_list(MappedSet, MappedVars),
		( goal_info_maybe_get_store_map(GoalInfo0, StoreMapPrime) ->
			AdvisoryStoreMap = StoreMapPrime
		;
			AdvisoryStoreMap = map__init
		),
		store_alloc_allocate_storage(MappedVars, StoreAllocInfo,
			AdvisoryStoreMap, StoreMap),
		goal_info_set_store_map(GoalInfo0, StoreMap, GoalInfo)
	;
		GoalInfo = GoalInfo0
	).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.

:- pred store_alloc_in_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
	liveness_info::in, liveness_info::out,
	last_locns::in, last_locns::out, set(prog_var)::in, set(prog_var)::in,
	store_alloc_info::in) is det.

store_alloc_in_goal_2(conj(Goals0), conj(Goals), !Liveness, !LastLocns,
		ResumeVars0, _, StoreAllocInfo) :-
	store_alloc_in_conj(Goals0, Goals, !Liveness, !LastLocns,
		ResumeVars0, StoreAllocInfo).

store_alloc_in_goal_2(par_conj(Goals0), par_conj(Goals),
		!Liveness, !LastLocns, ResumeVars0, _, StoreAllocInfo) :-
	store_alloc_in_par_conj(Goals0, Goals, !Liveness, !LastLocns,
		ResumeVars0, StoreAllocInfo).

store_alloc_in_goal_2(disj(Goals0), disj(Goals), !Liveness, !LastLocns,
		ResumeVars0, _, StoreAllocInfo) :-
	store_alloc_in_disj(Goals0, Goals, !Liveness,
		!.LastLocns, LastLocnsList, ResumeVars0, StoreAllocInfo),
	merge_last_locations(LastLocnsList, !:LastLocns).

store_alloc_in_goal_2(not(Goal0), not(Goal), !Liveness, !LastLocns,
		_ResumeVars0, _, StoreAllocInfo) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, ResumeNot),
	goal_info_resume_vars_and_loc(ResumeNot, ResumeNotVars, _),
	store_alloc_in_goal(Goal0, Goal, !Liveness, !.LastLocns, _,
		ResumeNotVars, StoreAllocInfo).

store_alloc_in_goal_2(switch(Var, Det, Cases0), switch(Var, Det, Cases),
		!Liveness, !LastLocns, ResumeVars0, _, StoreAllocInfo) :-
	store_alloc_in_cases(Cases0, Cases, !Liveness,
		!.LastLocns, LastLocnsList, ResumeVars0, StoreAllocInfo),
	merge_last_locations(LastLocnsList, !:LastLocns).

store_alloc_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else),
		Liveness0, Liveness, LastLocns0, LastLocns,
		ResumeVars0, _, StoreAllocInfo) :-
	Cond0 = _ - CondGoalInfo0,
	goal_info_get_resume_point(CondGoalInfo0, ResumeCond),
	goal_info_resume_vars_and_loc(ResumeCond, ResumeCondVars, _),
	store_alloc_in_goal(Cond0, Cond, Liveness0, Liveness1,
		LastLocns0, LastLocnsCond, ResumeCondVars, StoreAllocInfo),
	store_alloc_in_goal(Then0, Then, Liveness1, Liveness,
		LastLocnsCond, LastLocnsThen, ResumeVars0, StoreAllocInfo),
	store_alloc_in_goal(Else0, Else, Liveness0, _Liveness2,
		LastLocns0, LastLocnsElse, ResumeVars0, StoreAllocInfo),
	merge_last_locations([LastLocnsThen, LastLocnsElse], LastLocns).

store_alloc_in_goal_2(some(Vars, CanRemove, Goal0),
		some(Vars, CanRemove, Goal), !Liveness, !LastLocns,
		ResumeVars0, _, StoreAllocInfo) :-
	store_alloc_in_goal(Goal0, Goal, !Liveness, !LastLocns,
		ResumeVars0, StoreAllocInfo).

store_alloc_in_goal_2(Goal @ generic_call(_, _, _, _), Goal,
		!Liveness, !LastLocns, _, _, _).

store_alloc_in_goal_2(Goal @ call(_, _, _, _, _, _), Goal,
		!Liveness, !LastLocns, _, _, _).

store_alloc_in_goal_2(Goal @ unify(_, _, _, _, _), Goal,
		!Liveness, !LastLocns, _, _, _).

store_alloc_in_goal_2(Goal @ foreign_proc(_, _, _, _, _, _), Goal,
		!Liveness, !LastLocns, _, _, _).

store_alloc_in_goal_2(shorthand(_), _, _, _, _, _, _, _, _) :-
	% these should have been expanded out by now
	error("store_alloc_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
	liveness_info::in, liveness_info::out, last_locns::in, last_locns::out,
	set(prog_var)::in, store_alloc_info::in) is det.

store_alloc_in_conj([], [], !Liveness, !LastLocns, _, _).
store_alloc_in_conj([Goal0 | Goals0], [Goal | Goals], !Liveness, !LastLocns,
		ResumeVars0, StoreAllocInfo) :-
	(
			% XXX should be threading the instmap
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		store_alloc_in_goal(Goal0, Goal, !Liveness, !LastLocns,
			ResumeVars0, StoreAllocInfo),
		Goals = Goals0
	;
		store_alloc_in_goal(Goal0, Goal, !Liveness, !LastLocns,
			ResumeVars0, StoreAllocInfo),
		store_alloc_in_conj(Goals0, Goals, !Liveness, !LastLocns,
			ResumeVars0, StoreAllocInfo)
	).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
	liveness_info::in, liveness_info::out, last_locns::in, last_locns::out,
	set(prog_var)::in, store_alloc_info::in) is det.

store_alloc_in_par_conj([], [], !Liveness, !LastLocns, _, _).
store_alloc_in_par_conj([Goal0 | Goals0], [Goal | Goals], Liveness0, Liveness,
		!LastLocns, ResumeVars0, StoreAllocInfo) :-
	% XXX ignoring _Liveness1 looks fishy
	store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness,
		!LastLocns, ResumeVars0, StoreAllocInfo),
	store_alloc_in_par_conj(Goals0, Goals, Liveness0, _Liveness1,
		!LastLocns, ResumeVars0, StoreAllocInfo).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
	liveness_info::in, liveness_info::out,
	last_locns::in, list(last_locns)::out,
	set(prog_var)::in, store_alloc_info::in) is det.

store_alloc_in_disj([], [], !Liveness, _, [], _, _).
store_alloc_in_disj([Goal0 | Goals0], [Goal | Goals], Liveness0, Liveness,
		LastLocns0, [LastLocnsGoal | LastLocnsDisj],
		ResumeVars0, StoreAllocInfo) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, ResumeGoal),
	(
		ResumeGoal = no_resume_point,
		ResumeGoalVars = ResumeVars0
	;
		ResumeGoal = resume_point(ResumeGoalVars, _)
	),
	store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness,
		LastLocns0, LastLocnsGoal, ResumeGoalVars, StoreAllocInfo),
	store_alloc_in_disj(Goals0, Goals, Liveness0, _Liveness1,
		LastLocns0, LastLocnsDisj, ResumeVars0, StoreAllocInfo).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_cases(list(case)::in, list(case)::out,
	liveness_info::in, liveness_info::out,
	last_locns::in, list(last_locns)::out,
	set(prog_var)::in, store_alloc_info::in) is det.

store_alloc_in_cases([], [], !Liveness, _, [], _, _). 
store_alloc_in_cases([case(Cons, Goal0) | Goals0], [case(Cons, Goal) | Goals],
		Liveness0, Liveness,
		LastLocns0, [LastLocnsGoal | LastLocnsCases],
		ResumeVars0, StoreAllocInfo) :-
	store_alloc_in_goal(Goal0, Goal, Liveness0, Liveness,
		LastLocns0, LastLocnsGoal, ResumeVars0, StoreAllocInfo),
	store_alloc_in_cases(Goals0, Goals, Liveness0, _Liveness1,
		LastLocns0, LastLocnsCases, ResumeVars0, StoreAllocInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred merge_last_locations(list(last_locns)::in, last_locns::out) is det.

merge_last_locations(LastLocnsList, LastLocns) :-
	( LastLocnsList = [LastLocnsPrime | _] ->
		LastLocns = LastLocnsPrime
	;
		LastLocns = map__init
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given a follow_map which
	%
	% 1	may contain entries for non-live variables,
	%
	% 2	may contain no entry for a live variable,
	%
	% 3	which may map two live variables to one lval, and/or
	%
	% 4	map an lval to the artificial location reg(r(-1)),
	%
	% generate a store map that maps every live variable to its own
	% real location.

:- pred store_alloc_allocate_storage(list(prog_var)::in, store_alloc_info::in,
	abs_store_map::in, abs_store_map::out) is det.

store_alloc_allocate_storage(LiveVars, StoreAllocInfo, FollowVars, StoreMap) :-

	% This addresses point 1
	map__keys(FollowVars, FollowKeys),
	store_alloc_remove_nonlive(FollowKeys, LiveVars, FollowVars,
		StoreMap0),

	% This addresses points 3 and 4
	map__keys(StoreMap0, StoreVars),
	set__init(SeenLvals0),
	store_alloc_handle_conflicts_and_nonreal(StoreVars, 1, N,
		SeenLvals0, SeenLvals, StoreMap0, StoreMap1),

	% This addresses point 2
	store_alloc_allocate_extras(LiveVars, N, SeenLvals, StoreAllocInfo,
		StoreMap1, StoreMap).

:- pred store_alloc_remove_nonlive(list(prog_var)::in, list(prog_var)::in,
	abs_store_map::in, abs_store_map::out) is det.

store_alloc_remove_nonlive([], _LiveVars, !StoreMap).
store_alloc_remove_nonlive([Var | Vars], LiveVars, !StoreMap) :-
	( list__member(Var, LiveVars) ->
		true
	;
		map__delete(!.StoreMap, Var, !:StoreMap)
	),
	store_alloc_remove_nonlive(Vars, LiveVars, !StoreMap).

:- pred store_alloc_handle_conflicts_and_nonreal(list(prog_var)::in,
	int::in, int::out, set(abs_locn)::in, set(abs_locn)::out,
	abs_store_map::in, abs_store_map::out) is det.

store_alloc_handle_conflicts_and_nonreal([], !N, !SeenLocns, !StoreMap).
store_alloc_handle_conflicts_and_nonreal([Var | Vars], !N, !SeenLocns,
		!StoreMap) :-
	map__lookup(!.StoreMap, Var, Locn),
	(
		( Locn = any_reg
		; set__member(Locn, !.SeenLocns)
		)
	->
		next_free_reg(!.SeenLocns, !N),
		FinalLocn = abs_reg(!.N),
		map__det_update(!.StoreMap, Var, FinalLocn, !:StoreMap)
	;
		FinalLocn = Locn
	),
	set__insert(!.SeenLocns, FinalLocn, !:SeenLocns),
	store_alloc_handle_conflicts_and_nonreal(Vars, !N, !SeenLocns,
		!StoreMap).

:- pred store_alloc_allocate_extras(list(prog_var)::in, int::in,
	set(abs_locn)::in, store_alloc_info::in,
	abs_store_map::in, abs_store_map::out) is det.

store_alloc_allocate_extras([], _, _, _, !StoreMap).
store_alloc_allocate_extras([Var | Vars], !.N, !.SeenLocns, StoreAllocInfo,
		!StoreMap) :-
	( map__contains(!.StoreMap, Var) ->
		% We have already allocated a slot for this variable.
		true
	;
		% We have not yet allocated a slot for this variable,
		% which means it is not in the follow vars (if any).
		StoreAllocInfo = store_alloc_info(_, StackSlots),
		(
			map__search(StackSlots, Var, StackSlot),
			StackSlotLocn = stack_slot_to_abs_locn(StackSlot),
			\+ set__member(StackSlotLocn, !.SeenLocns)
			% Follow_vars was run, so the only
			% reason why a var would not be in the
			% follow_vars set is if it was supposed to
			% be in its stack slot.
		->
			Locn = StackSlotLocn
		;
			next_free_reg(!.SeenLocns, !N),
			Locn = abs_reg(!.N)
		),
		map__det_insert(!.StoreMap, Var, Locn, !:StoreMap),
		set__insert(!.SeenLocns, Locn, !:SeenLocns)
	),
	store_alloc_allocate_extras(Vars, !.N, !.SeenLocns, StoreAllocInfo,
		!StoreMap).

%-----------------------------------------------------------------------------%

:- pred next_free_reg(set(abs_locn)::in, int::in, int::out) is det.

next_free_reg(Values, N0, N) :-
	( set__member(abs_reg(N0), Values) ->
		N1 = N0 + 1,
		next_free_reg(Values, N1, N)
	;
		N = N0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
