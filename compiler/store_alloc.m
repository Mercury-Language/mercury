%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
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

:- import_module hlds__hlds_module, hlds__hlds_pred.

:- type store_map_run_type
	--->	final_allocation
	;	for_stack_opt.

:- pred allocate_store_maps(store_map_run_type::in, proc_info::in, pred_id::in,
	module_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_goal, hlds__hlds_llds.
:- import_module hlds__goal_util, hlds__instmap.
:- import_module check_hlds__mode_util.
:- import_module ll_backend__llds, ll_backend__trace, ll_backend__arg_info.
:- import_module ll_backend__follow_vars, ll_backend__liveness.
:- import_module libs__options, libs__globals, libs__trace_params.

:- import_module bool, int, require.
:- import_module list, map, set, std_util, assoc_list.

%-----------------------------------------------------------------------------%

allocate_store_maps(RunType, ProcInfo0, PredId, ModuleInfo, ProcInfo) :-
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, follow_vars, ApplyFollowVars),
	(
		RunType = final_allocation,
		ApplyFollowVars = yes
	->
		proc_info_goal(ProcInfo0, Goal0),

		find_final_follow_vars(ProcInfo0,
			FollowVarsMap0, NextNonReserved0),
		proc_info_vartypes(ProcInfo0, VarTypes),
		find_follow_vars_in_goal(Goal0, VarTypes, ModuleInfo,
			FollowVarsMap0, NextNonReserved0,
			Goal1, FollowVarsMap, NextNonReserved),
		Goal1 = GoalExpr1 - GoalInfo1,
		FollowVars = follow_vars(FollowVarsMap, NextNonReserved),
		goal_info_set_follow_vars(GoalInfo1, yes(FollowVars),
			GoalInfo2),
		Goal2 = GoalExpr1 - GoalInfo2
	;
		proc_info_goal(ProcInfo0, Goal2)
	),
	initial_liveness(ProcInfo0, PredId, ModuleInfo, Liveness0),
	globals__get_trace_level(Globals, TraceLevel),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	( eff_trace_level_is_none(PredInfo, ProcInfo0, TraceLevel) = no ->
		trace__fail_vars(ModuleInfo, ProcInfo0, ResumeVars0)
	;
		set__init(ResumeVars0)
	),
	arg_info__build_input_arg_list(ProcInfo0, InputArgLvals),
	LastLocns0 = initial_last_locns(InputArgLvals),
	globals__lookup_int_option(Globals, num_real_r_regs, NumRealRRegs),
	proc_info_stack_slots(ProcInfo0, StackSlots),
	StoreAllocInfo = store_alloc_info(ModuleInfo, ApplyFollowVars,
		NumRealRRegs, StackSlots),
	store_alloc_in_goal(Goal2, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goal, _, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo).

:- func initial_last_locns(assoc_list(prog_var, lval)) = last_locns.

initial_last_locns([]) = map__init.
initial_last_locns([Var - Lval | VarLvals]) =
	map__det_insert(initial_last_locns(VarLvals), Var,
		set__make_singleton_set(Lval)).

%-----------------------------------------------------------------------------%

:- type store_alloc_info
	--->	store_alloc_info(
			module_info		:: module_info,
			done_follow_vars	:: bool,
						% was follow_vars run?
			num_real_r_regs		:: int,
						% the number of real r regs
			stack_slots		:: stack_slots
						% maps each var to its stack
						% slot (if it has one)
		).

:- type where_stored	== set(lval).	% These lvals may contain var() rvals.

:- type last_locns	== map(prog_var, where_stored).

:- pred store_alloc_in_goal(hlds_goal::in, liveness_info::in, set(prog_var)::in,
	last_locns::in, store_alloc_info::in, hlds_goal::out,
	liveness_info::out, last_locns::out) is det.

store_alloc_in_goal(Goal0 - GoalInfo0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goal - GoalInfo, Liveness, LastLocns) :-
	% note: we must be careful to apply deaths before births
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
	goal_info_get_pre_births(GoalInfo0, PreBirths),
	goal_info_get_post_deaths(GoalInfo0, PostDeaths),
	goal_info_get_post_births(GoalInfo0, PostBirths),

	set__difference(Liveness0,  PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),
	store_alloc_in_goal_2(Goal0, Liveness2, ResumeVars0, LastLocns0,
		PostDeaths, StoreAllocInfo, Goal, Liveness3, LastLocns),
	set__difference(Liveness3, PostDeaths, Liveness4),
	% If any variables magically become live in the PostBirths,
	% then they have to mundanely become live in a parallel goal,
	% so we don't need to allocate anything for them here.
	%
	% Any variables that become magically live at the end of the goal
	% should not be included in the store map.
	set__union(Liveness4, PostBirths, Liveness),
	( goal_util__goal_is_branched(Goal) ->
		set__union(Liveness4, ResumeVars0, MappedSet),
		set__to_sorted_list(MappedSet, MappedVars),
		( goal_info_maybe_get_store_map(GoalInfo0, StoreMapPrime) ->
			AdvisoryStoreMap = StoreMapPrime
		;
			AdvisoryStoreMap = map__init
		),
		store_alloc_allocate_storage(MappedVars, AdvisoryStoreMap,
			StoreAllocInfo, StoreMap),
		goal_info_set_store_map(GoalInfo0, StoreMap, GoalInfo)
	;
		GoalInfo = GoalInfo0
	).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.

:- pred store_alloc_in_goal_2(hlds_goal_expr::in, liveness_info::in,
	set(prog_var)::in, last_locns::in, set(prog_var)::in,
	store_alloc_info::in, hlds_goal_expr::out, liveness_info::out,
	last_locns::out) is det.

store_alloc_in_goal_2(conj(Goals0), Liveness0, ResumeVars0, LastLocns0,
		_, StoreAllocInfo, conj(Goals), Liveness, LastLocns) :-
	store_alloc_in_conj(Goals0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goals, Liveness, LastLocns).

store_alloc_in_goal_2(par_conj(Goals0), Liveness0, ResumeVars0, LastLocns0,
		_, StoreAllocInfo, par_conj(Goals), Liveness, LastLocns) :-
	store_alloc_in_par_conj(Goals0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goals, Liveness, LastLocns).

store_alloc_in_goal_2(disj(Goals0), Liveness0, ResumeVars0, LastLocns0,
		_, StoreAllocInfo, disj(Goals), Liveness, LastLocns) :-
	store_alloc_in_disj(Goals0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goals, Liveness, LastLocnsList),
	merge_last_locations(LastLocnsList, LastLocns).

store_alloc_in_goal_2(not(Goal0), Liveness0, _ResumeVars0, LastLocns0,
		_, StoreAllocInfo, not(Goal), Liveness, LastLocns0) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, ResumeNot),
	goal_info_resume_vars_and_loc(ResumeNot, ResumeNotVars, _),
	store_alloc_in_goal(Goal0, Liveness0, ResumeNotVars, LastLocns0,
		StoreAllocInfo, Goal, Liveness, _).

store_alloc_in_goal_2(switch(Var, Det, Cases0), Liveness0, ResumeVars0,
		LastLocns0, _, StoreAllocInfo,
		switch(Var, Det, Cases), Liveness, LastLocns) :-
	store_alloc_in_cases(Cases0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Cases, Liveness, LastLocnsList),
	merge_last_locations(LastLocnsList, LastLocns).

store_alloc_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
		Liveness0, ResumeVars0, LastLocns0, _, StoreAllocInfo,
		if_then_else(Vars, Cond, Then, Else), Liveness, LastLocns) :-
	Cond0 = _ - CondGoalInfo0,
	goal_info_get_resume_point(CondGoalInfo0, ResumeCond),
	goal_info_resume_vars_and_loc(ResumeCond, ResumeCondVars, _),
	store_alloc_in_goal(Cond0, Liveness0, ResumeCondVars, LastLocns0,
		StoreAllocInfo, Cond, Liveness1, LastLocnsCond),
	store_alloc_in_goal(Then0, Liveness1, ResumeVars0, LastLocnsCond,
		StoreAllocInfo, Then, Liveness, LastLocnsThen),
	store_alloc_in_goal(Else0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Else, _Liveness2, LastLocnsElse),
	merge_last_locations([LastLocnsThen, LastLocnsElse], LastLocns).

store_alloc_in_goal_2(some(Vars, CanRemove, Goal0), Liveness0, ResumeVars0,
		LastLocns0, _, StoreAllocInfo,
		some(Vars, CanRemove, Goal), Liveness, LastLocns) :-
	store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goal, Liveness, LastLocns).

store_alloc_in_goal_2(generic_call(A, B, C, D), Liveness, _, LastLocns,
		_, _, generic_call(A, B, C, D), Liveness, LastLocns).

store_alloc_in_goal_2(call(A, B, C, D, E, F), Liveness, _, LastLocns,
		_, _, call(A, B, C, D, E, F), Liveness, LastLocns).

store_alloc_in_goal_2(unify(A,B,C,D,E), Liveness, _, LastLocns,
		_, _, unify(A,B,C,D,E), Liveness, LastLocns).

store_alloc_in_goal_2(foreign_proc(A, B, C, D, E, F, G), Liveness, _,
		LastLocns, _, _, foreign_proc(A, B, C, D, E, F, G),
		Liveness, LastLocns).

store_alloc_in_goal_2(shorthand(_), _, _, _, _, _, _, _, _) :-
	% these should have been expanded out by now
	error("store_alloc_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_conj(list(hlds_goal)::in, liveness_info::in,
	set(prog_var)::in, last_locns::in, store_alloc_info::in,
	list(hlds_goal)::out, liveness_info::out, last_locns::out) is det.

store_alloc_in_conj([], Liveness, _, LastLocns, _, [], Liveness, LastLocns).
store_alloc_in_conj([Goal0 | Goals0], Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, [Goal | Goals], Liveness, LastLocns) :-
	(
			% XXX should be threading the instmap
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, LastLocns0,
			StoreAllocInfo, Goal, Liveness, LastLocns),
		Goals = Goals0
	;
		store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, LastLocns0,
			StoreAllocInfo, Goal, Liveness1, LastLocns1),
		store_alloc_in_conj(Goals0, Liveness1, ResumeVars0, LastLocns1,
			StoreAllocInfo, Goals, Liveness, LastLocns)
	).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_par_conj(list(hlds_goal)::in, liveness_info::in,
	set(prog_var)::in, last_locns::in, store_alloc_info::in,
	list(hlds_goal)::out, liveness_info::out, last_locns::out) is det.

store_alloc_in_par_conj([], Liveness, _, LastLocns, _,
		[], Liveness, LastLocns).
store_alloc_in_par_conj([Goal0 | Goals0], Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, [Goal | Goals], Liveness, LastLocns) :-
	% XXX ignoring _Liveness1 looks fishy
	store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goal, Liveness, LastLocns1),
	store_alloc_in_par_conj(Goals0, Liveness0, ResumeVars0, LastLocns1,
		StoreAllocInfo, Goals, _Liveness1, LastLocns).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_disj(list(hlds_goal)::in, liveness_info::in,
	set(prog_var)::in, last_locns::in, store_alloc_info::in,
	list(hlds_goal)::out, liveness_info::out, list(last_locns)::out)
	is det.

store_alloc_in_disj([], Liveness, _, _, _, [], Liveness, []).
store_alloc_in_disj([Goal0 | Goals0], Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, [Goal | Goals], Liveness,
		[LastLocnsGoal | LastLocnsDisj]) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, ResumeGoal),
	(
		ResumeGoal = no_resume_point,
		ResumeGoalVars = ResumeVars0
	;
		ResumeGoal = resume_point(ResumeGoalVars, _)
	),
	store_alloc_in_goal(Goal0, Liveness0, ResumeGoalVars, LastLocns0,
		StoreAllocInfo, Goal, Liveness, LastLocnsGoal),
	store_alloc_in_disj(Goals0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goals, _Liveness1, LastLocnsDisj).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_cases(list(case)::in, liveness_info::in,
	set(prog_var)::in, last_locns::in, store_alloc_info::in,
	list(case)::out, liveness_info::out, list(last_locns)::out) is det.

store_alloc_in_cases([], Liveness, _, _, _, [], Liveness, []).
store_alloc_in_cases([case(Cons, Goal0) | Goals0], Liveness0, ResumeVars0,
		LastLocns0, StoreAllocInfo, [case(Cons, Goal) | Goals],
		Liveness, [LastLocnsGoal | LastLocnsCases]) :-
	store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goal, Liveness, LastLocnsGoal),
	store_alloc_in_cases(Goals0, Liveness0, ResumeVars0, LastLocns0,
		StoreAllocInfo, Goals, _Liveness1, LastLocnsCases).

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

:- pred store_alloc_allocate_storage(list(prog_var)::in, store_map::in,
	store_alloc_info::in, store_map::out) is det.

store_alloc_allocate_storage(LiveVars, FollowVars, StoreAllocInfo, StoreMap) :-

	% This addresses point 1
	map__keys(FollowVars, FollowKeys),
	store_alloc_remove_nonlive(FollowKeys, LiveVars, FollowVars, StoreMap0),

	% This addresses points 3 and 4
	map__keys(StoreMap0, StoreVars),
	set__init(SeenLvals0),
	store_alloc_handle_conflicts_and_nonreal(StoreVars, 1, N,
		SeenLvals0, SeenLvals, StoreMap0, StoreMap1),

	% This addresses point 2
	store_alloc_allocate_extras(LiveVars, N, SeenLvals, StoreAllocInfo,
		StoreMap1, StoreMap).

:- pred store_alloc_remove_nonlive(list(prog_var), list(prog_var),
		store_map, store_map).
:- mode store_alloc_remove_nonlive(in, in, in, out) is det.

store_alloc_remove_nonlive([], _LiveVars, StoreMap, StoreMap).
store_alloc_remove_nonlive([Var | Vars], LiveVars, StoreMap0, StoreMap) :-
	( list__member(Var, LiveVars) ->
		StoreMap1 = StoreMap0
	;
		map__delete(StoreMap0, Var, StoreMap1)
	),
	store_alloc_remove_nonlive(Vars, LiveVars, StoreMap1, StoreMap).

:- pred store_alloc_handle_conflicts_and_nonreal(list(prog_var),
	int, int, set(lval), set(lval), store_map, store_map).
:- mode store_alloc_handle_conflicts_and_nonreal(in, in, out, in, out, in, out)
	is det.

store_alloc_handle_conflicts_and_nonreal([], N, N, SeenLvals, SeenLvals,
		StoreMap, StoreMap).
store_alloc_handle_conflicts_and_nonreal([Var | Vars], N0, N,
		SeenLvals0, SeenLvals, StoreMap0, StoreMap) :-
	map__lookup(StoreMap0, Var, Lval),
	(
		( artificial_lval(Lval)
		; set__member(Lval, SeenLvals0)
		)
	->
		next_free_reg(N0, SeenLvals0, N1),
		FinalLval = reg(r, N1),
		map__det_update(StoreMap0, Var, FinalLval, StoreMap1)
	;
		N1 = N0,
		FinalLval = Lval,
		StoreMap1 = StoreMap0
	),
	set__insert(SeenLvals0, FinalLval, SeenLvals1),
	store_alloc_handle_conflicts_and_nonreal(Vars, N1, N,
		SeenLvals1, SeenLvals, StoreMap1, StoreMap).

:- pred store_alloc_allocate_extras(list(prog_var), int, set(lval),
		store_alloc_info, store_map, store_map).
:- mode store_alloc_allocate_extras(in, in, in, in, in, out) is det.

store_alloc_allocate_extras([], _, _, _, StoreMap, StoreMap).
store_alloc_allocate_extras([Var | Vars], N0, SeenLvals0, StoreAllocInfo,
		StoreMap0, StoreMap) :-
	(
		map__contains(StoreMap0, Var)
	->
		% We have already allocated a slot for this variable.
		N1 = N0,
		StoreMap1 = StoreMap0,
		SeenLvals1 = SeenLvals0
	;
		% We have not yet allocated a slot for this variable,
		% which means it is not in the follow vars (if any).
		StoreAllocInfo = store_alloc_info(_, FollowVars, NumRealRRegs,
			StackSlots),
		(
			map__search(StackSlots, Var, StackSlot),
			\+ set__member(StackSlot, SeenLvals0),
			(
				FollowVars = yes
				% If follow_vars was run, then the only
				% reason why a var would not be in the
				% follow_vars set is if it was supposed to
				% be in its stack slot.
			;
				FollowVars = no,
				% If follow_vars was not run, then we
				% prefer to put the variable in a register,
				% provided it is a real register.
				next_free_reg(N0, SeenLvals0, TentativeReg),
				TentativeReg =< NumRealRRegs
			)
		->
			Locn = StackSlot,
			N1 = N0
		;
			next_free_reg(N0, SeenLvals0, N1),
			Locn = reg(r, N1)
		),
		map__det_insert(StoreMap0, Var, Locn, StoreMap1),
		set__insert(SeenLvals0, Locn, SeenLvals1)
	),
	store_alloc_allocate_extras(Vars, N1, SeenLvals1, StoreAllocInfo,
		StoreMap1, StoreMap).

%-----------------------------------------------------------------------------%

	% The follow_vars pass maps some variables r(-1) as a hint to the
	% code generator to put them in any free register. Since store maps
	% require real locations, we can't use such hints directly.

	% For robustness, we check for N < 1 instead of N = -1.

:- pred artificial_lval(lval).
:- mode artificial_lval(in) is semidet.

artificial_lval(reg(_Type, Num)) :-
	Num < 1.

%-----------------------------------------------------------------------------%

:- pred next_free_reg(int, set(lval), int).
:- mode next_free_reg(in, in, out) is det.

next_free_reg(N0, Values, N) :-
	( set__member(reg(r, N0), Values) ->
		N1 is N0 + 1,
		next_free_reg(N1, Values, N)
	;
		N = N0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
