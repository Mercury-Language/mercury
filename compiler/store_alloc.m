%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

% See notes/ALLOCATION for a description of the framework that this pass
% operates within.

%-----------------------------------------------------------------------------%

:- module store_alloc.

:- interface.

:- import_module hlds_module, hlds_pred.

:- pred store_alloc_in_proc(proc_info, module_info, proc_info).
:- mode store_alloc_in_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module follow_vars, liveness, hlds_goal, llds.
:- import_module options, globals, goal_util, mode_util, instmap.
:- import_module list, map, set, std_util, assoc_list.
:- import_module bool, int, require, term.

%-----------------------------------------------------------------------------%

store_alloc_in_proc(ProcInfo0, ModuleInfo, ProcInfo) :-
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, follow_vars, ApplyFollowVars),
	( ApplyFollowVars = yes ->
		globals__get_args_method(Globals, ArgsMethod),
		proc_info_goal(ProcInfo0, Goal0),

		find_final_follow_vars(ProcInfo0, FollowVars0),
		find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo,
			FollowVars0, Goal1, FollowVars),
		Goal1 = GoalExpr1 - GoalInfo1,
		goal_info_set_follow_vars(GoalInfo1, yes(FollowVars),
			GoalInfo2),
		Goal2 = GoalExpr1 - GoalInfo2
	;
		proc_info_goal(ProcInfo0, Goal2)
	),
	initial_liveness(ProcInfo0, ModuleInfo, Liveness0),
	set__init(ResumeVars0),
	store_alloc_in_goal(Goal2, Liveness0, ResumeVars0, ModuleInfo, Goal, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_goal(hlds_goal, liveness_info, set(var), module_info,
	hlds_goal, liveness_info).
:- mode store_alloc_in_goal(in, in, in, in, out, out) is det.

store_alloc_in_goal(Goal0 - GoalInfo0, Liveness0, ResumeVars0, ModuleInfo,
		Goal - GoalInfo0, Liveness) :-
	goal_info_get_pre_births(GoalInfo0, PreBirths),
	goal_info_get_pre_deaths(GoalInfo0, PreDeaths),
	goal_info_get_post_births(GoalInfo0, PostBirths),
	goal_info_get_post_deaths(GoalInfo0, PostDeaths),

	set__difference(Liveness0,  PreDeaths, Liveness1),
	set__union(Liveness1, PreBirths, Liveness2),
	store_alloc_in_goal_2(Goal0, Liveness2, ResumeVars0, ModuleInfo,
		Goal1, Liveness3),
	set__difference(Liveness3, PostDeaths, Liveness4),
	% If any variables magically become live in the PostBirths,
	% then they have to mundanely become live in a parallel goal,
	% so we don't need to allocate anything for them here.
	%
	% Any variables that become magically live at the end of the goal
	% should not be included in the store map.
	set__union(Liveness4, PostBirths, Liveness),
	(
		Goal1 = switch(Var, CanFail, Cases, FollowVars)
	->
		set__union(Liveness4, ResumeVars0, MappedSet),
		set__to_sorted_list(MappedSet, MappedVars),
		store_alloc_allocate_storage(MappedVars, FollowVars, StoreMap),
		Goal = switch(Var, CanFail, Cases, StoreMap)
	;
		Goal1 = if_then_else(Vars, Cond, Then, Else, FollowVars)
	->
		set__union(Liveness4, ResumeVars0, MappedSet),
		set__to_sorted_list(MappedSet, MappedVars),
		store_alloc_allocate_storage(MappedVars, FollowVars, StoreMap),
		Goal = if_then_else(Vars, Cond, Then, Else, StoreMap)
	;
		Goal1 = disj(Disjuncts, FollowVars)
	->
		set__union(Liveness4, ResumeVars0, MappedSet),
		set__to_sorted_list(MappedSet, MappedVars),
		store_alloc_allocate_storage(MappedVars, FollowVars, StoreMap),
		Goal = disj(Disjuncts, StoreMap)
	;
		Goal = Goal1
	).

%-----------------------------------------------------------------------------%

	% Here we process each of the different sorts of goals.

:- pred store_alloc_in_goal_2(hlds_goal_expr, liveness_info,
	set(var), module_info, hlds_goal_expr, liveness_info).
:- mode store_alloc_in_goal_2(in, in, in, in, out, out) is det.

store_alloc_in_goal_2(conj(Goals0), Liveness0, ResumeVars0, ModuleInfo,
		conj(Goals), Liveness) :-
	store_alloc_in_conj(Goals0, Liveness0, ResumeVars0, ModuleInfo,
		Goals, Liveness).

store_alloc_in_goal_2(disj(Goals0, FV), Liveness0, ResumeVars0, ModuleInfo,
		disj(Goals, FV), Liveness) :-
	store_alloc_in_disj(Goals0, Liveness0, ResumeVars0, ModuleInfo,
		Goals, Liveness).

store_alloc_in_goal_2(not(Goal0), Liveness0, _ResumeVars0, ModuleInfo,
		not(Goal), Liveness) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, ResumeNot),
	goal_info_resume_vars_and_loc(ResumeNot, ResumeNotVars, _),
	store_alloc_in_goal(Goal0, Liveness0, ResumeNotVars, ModuleInfo,
		Goal, Liveness).

store_alloc_in_goal_2(switch(Var, Det, Cases0, FV), Liveness0, ResumeVars0,
		ModuleInfo, switch(Var, Det, Cases, FV), Liveness) :-
	store_alloc_in_cases(Cases0, Liveness0, ResumeVars0, ModuleInfo,
		Cases, Liveness).

store_alloc_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, FV),
		Liveness0, ResumeVars0, ModuleInfo,
		if_then_else(Vars, Cond, Then, Else, FV), Liveness) :-
	Cond0 = _ - CondGoalInfo0,
	goal_info_get_resume_point(CondGoalInfo0, ResumeCond),
	goal_info_resume_vars_and_loc(ResumeCond, ResumeCondVars, _),
	store_alloc_in_goal(Cond0, Liveness0, ResumeCondVars, ModuleInfo,
		Cond, Liveness1),
	store_alloc_in_goal(Then0, Liveness1, ResumeVars0, ModuleInfo,
		Then, Liveness),
	store_alloc_in_goal(Else0, Liveness0, ResumeVars0, ModuleInfo,
		Else, _Liveness2).

store_alloc_in_goal_2(some(Vars, Goal0), Liveness0, ResumeVars0, ModuleInfo,
		some(Vars, Goal), Liveness) :-
	store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, ModuleInfo,
		Goal, Liveness).

store_alloc_in_goal_2(higher_order_call(A, B, C, D, E), Liveness, _, _,
		higher_order_call(A, B, C, D, E), Liveness).

store_alloc_in_goal_2(call(A, B, C, D, E, F), Liveness, _, _,
		call(A, B, C, D, E, F), Liveness).

store_alloc_in_goal_2(unify(A,B,C,D,E), Liveness, _, _,
		unify(A,B,C,D,E), Liveness).

store_alloc_in_goal_2(pragma_c_code(A, B, C, D, E, F, G, H), Liveness, _, _,
		pragma_c_code(A, B, C, D, E, F, G, H), Liveness).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_conj(list(hlds_goal), liveness_info, set(var),
		module_info, list(hlds_goal), liveness_info).
:- mode store_alloc_in_conj(in, in, in, in, out, out) is det.

store_alloc_in_conj([], Liveness, _R, _M, [], Liveness).
store_alloc_in_conj([Goal0 | Goals0], Liveness0, ResumeVars0, ModuleInfo,
		[Goal | Goals], Liveness) :-
	(
			% XXX should be threading the instmap
		Goal0 = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap_delta_is_unreachable(InstMapDelta)
	->
		store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, ModuleInfo,
			Goal, Liveness),
		Goals = Goals0
	;
		store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, ModuleInfo,
			Goal, Liveness1),
		store_alloc_in_conj(Goals0, Liveness1, ResumeVars0, ModuleInfo,
			Goals, Liveness)
	).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_disj(list(hlds_goal), liveness_info, set(var),
	module_info, list(hlds_goal), liveness_info).
:- mode store_alloc_in_disj(in, in, in, in, out, out) is det.

store_alloc_in_disj([], Liveness, _ResumeVars0, _ModuleInfo, [], Liveness).
store_alloc_in_disj([Goal0 | Goals0], Liveness0, ResumeVars0, ModuleInfo,
		[Goal | Goals], Liveness) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_resume_point(GoalInfo0, ResumeGoal),
	(
		ResumeGoal = no_resume_point,
		ResumeGoalVars = ResumeVars0
	;
		ResumeGoal = resume_point(ResumeGoalVars, _)
	),
	store_alloc_in_goal(Goal0, Liveness0, ResumeGoalVars, ModuleInfo,
		Goal, Liveness),
	store_alloc_in_disj(Goals0, Liveness0, ResumeVars0, ModuleInfo,
		Goals, _Liveness1).

%-----------------------------------------------------------------------------%

:- pred store_alloc_in_cases(list(case), liveness_info, set(var),
	module_info, list(case), liveness_info).
:- mode store_alloc_in_cases(in, in, in, in, out, out) is det.

store_alloc_in_cases([], Liveness, _ResumeVars0, _ModuleInfo, [], Liveness).
store_alloc_in_cases([case(Cons, Goal0) | Goals0], Liveness0, ResumeVars0,
		ModuleInfo, [case(Cons, Goal) | Goals], Liveness) :-
	store_alloc_in_goal(Goal0, Liveness0, ResumeVars0, ModuleInfo,
		Goal, Liveness),
	store_alloc_in_cases(Goals0, Liveness0, ResumeVars0, ModuleInfo,
		Goals, _Liveness1).

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

:- pred store_alloc_allocate_storage(list(var), follow_vars, store_map).
:- mode store_alloc_allocate_storage(in, in, out) is det.

store_alloc_allocate_storage(LiveVars, FollowVars, StoreMap) :-

	% This addresses point 1
	map__keys(FollowVars, FollowKeys),
	store_alloc_remove_nonlive(FollowKeys, LiveVars, FollowVars, StoreMap0),

	% This addresses points 3 and 4
	map__keys(StoreMap0, StoreVars),
	set__init(SeenLvals0),
	store_alloc_handle_conflicts_and_nonreal(StoreVars, 1, N,
		SeenLvals0, SeenLvals, StoreMap0, StoreMap1),

	% This addresses point 2
	store_alloc_allocate_extras(LiveVars, N, SeenLvals,
		StoreMap1, StoreMap).

:- pred store_alloc_remove_nonlive(list(var), list(var), store_map, store_map).
:- mode store_alloc_remove_nonlive(in, in, in, out) is det.

store_alloc_remove_nonlive([], _LiveVars, StoreMap, StoreMap).
store_alloc_remove_nonlive([Var | Vars], LiveVars, StoreMap0, StoreMap) :-
	( list__member(Var, LiveVars) ->
		StoreMap1 = StoreMap0
	;
		map__delete(StoreMap0, Var, StoreMap1)
	),
	store_alloc_remove_nonlive(Vars, LiveVars, StoreMap1, StoreMap).

:- pred store_alloc_handle_conflicts_and_nonreal(list(var),
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

:- pred store_alloc_allocate_extras(list(var), int, set(lval),
	store_map, store_map).
:- mode store_alloc_allocate_extras(in, in, in, in, out) is det.

store_alloc_allocate_extras([], _N, _SeenLvals, StoreMap, StoreMap).
store_alloc_allocate_extras([Var | Vars], N0, SeenLvals0,
		StoreMap0, StoreMap) :-
	( map__contains(StoreMap0, Var) ->
		N1 = N0,
		StoreMap1 = StoreMap0,
		SeenLvals1 = SeenLvals0
	;
		next_free_reg(N0, SeenLvals0, N1),
		map__det_insert(StoreMap0, Var, reg(r, N1), StoreMap1),
		set__insert(SeenLvals0, reg(r, N1), SeenLvals1)
	),
	store_alloc_allocate_extras(Vars, N1, SeenLvals1, StoreMap1, StoreMap).

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
