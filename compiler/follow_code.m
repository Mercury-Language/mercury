%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
% Extensive modifications by zs.

%-----------------------------------------------------------------------------%

:- module follow_code.

:- interface.

:- import_module hlds, llds.

:- pred move_follow_code(module_info, pair(bool), module_info).
:- mode move_follow_code(in, in, out) is det.

:- pred move_follow_code_in_proc(proc_info, proc_info,
	pair(bool), module_info, module_info).
% :- mode move_follow_code_in_proc(di, uo, in, di, uo) is det.
:- mode move_follow_code_in_proc(in, out, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module det_analysis, mode_util, quantification.
:- import_module list, map, set, std_util.
:- import_module term, require.

%-----------------------------------------------------------------------------%

move_follow_code(ModuleInfo0, Flags, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	move_follow_code_in_preds(PredIds, Flags, ModuleInfo0, ModuleInfo1).

:- pred move_follow_code_in_preds(list(pred_id), pair(bool),
	module_info, module_info).
:- mode move_follow_code_in_preds(in, in, in, out) is det.

move_follow_code_in_preds([], _Flags, ModuleInfo, ModuleInfo).
move_follow_code_in_preds([PredId | PredIds], Flags,
		ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	move_follow_code_in_procs(ProcIds, PredId, Flags,
		ModuleInfo0, ModuleInfo1),
	move_follow_code_in_preds(PredIds, Flags, ModuleInfo1, ModuleInfo).

:- pred move_follow_code_in_procs(list(proc_id), pred_id, pair(bool),
	module_info, module_info).
:- mode move_follow_code_in_procs(in, in, in, in, out) is det.

move_follow_code_in_procs([], _PredId, _Flags, ModuleInfo, ModuleInfo).
move_follow_code_in_procs([ProcId | ProcIds], PredId,
		Flags, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	move_follow_code_in_proc(ProcInfo0, ProcInfo, Flags,
		ModuleInfo0, ModuleInfo1),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),
	move_follow_code_in_procs(ProcIds, PredId, Flags,
		ModuleInfo2, ModuleInfo).

move_follow_code_in_proc(ProcInfo0, ProcInfo, Flags,
		ModuleInfo0, ModuleInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	(
		move_follow_code_in_goal(Goal0, Goal1, Flags, no, Res),
			% did the goal change?
		Res = yes
	->
			% we need to fix up the goal_info by recalculating
			% the nonlocal vars and the non-atomic instmap deltas.
		proc_info_headvars(ProcInfo0, HeadVars),
		implicitly_quantify_clause_body(HeadVars, Goal1, Goal2),
		recompute_instmap_delta(Goal2, Goal, ModuleInfo0, ModuleInfo)
	;
		Goal = Goal0,
		ModuleInfo = ModuleInfo0
	),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal(hlds__goal, hlds__goal, pair(bool),
	bool, bool).
:- mode move_follow_code_in_goal(in, out, in, in, out) is det.

move_follow_code_in_goal(Goal0 - GoalInfo, Goal - GoalInfo, Flags, R0, R) :-
	move_follow_code_in_goal_2(Goal0, Goal, Flags, R0, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal_2(hlds__goal_expr, hlds__goal_expr,
	pair(bool), bool, bool).
:- mode move_follow_code_in_goal_2(in, out, in, in, out) is det.

move_follow_code_in_goal_2(conj(Goals0), conj(Goals), Flags, R0, R) :-
	move_follow_code_in_conj(Goals0, Goals, Flags, R0, R).

move_follow_code_in_goal_2(disj(Goals0), disj(Goals), Flags, R0, R) :-
	move_follow_code_in_disj(Goals0, Goals, Flags, R0, R).

move_follow_code_in_goal_2(not(Goal0), not(Goal), Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R).

move_follow_code_in_goal_2(switch(Var, Det, Cases0),
		switch(Var, Det, Cases), Flags, R0, R) :-
	move_follow_code_in_cases(Cases0, Cases, Flags, R0, R).

move_follow_code_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else), Flags, R0, R) :-
	move_follow_code_in_goal(Cond0, Cond, Flags, R0, R1),
	move_follow_code_in_goal(Then0, Then, Flags, R1, R2),
	move_follow_code_in_goal(Else0, Else, Flags, R2, R).

move_follow_code_in_goal_2(some(Vars, Goal0), some(Vars, Goal), Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R).

move_follow_code_in_goal_2(call(A,B,C,D,E,F,G), call(A,B,C,D,E,F,G), _, R, R).

move_follow_code_in_goal_2(unify(A,B,C,D,E), unify(A,B,C,D,E), _, R, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_disj(list(hlds__goal), list(hlds__goal),
	pair(bool), bool, bool).
:- mode move_follow_code_in_disj(in, out, in, in, out) is det.

move_follow_code_in_disj([], [], _, R, R).
move_follow_code_in_disj([Goal0|Goals0], [Goal|Goals], Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R1),
	move_follow_code_in_disj(Goals0, Goals, Flags, R1, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_cases(list(case), list(case), pair(bool),
	bool, bool).
:- mode move_follow_code_in_cases(in, out, in, in, out) is det.

move_follow_code_in_cases([], [], _, R, R).
move_follow_code_in_cases([case(Cons, Goal0)|Goals0], [case(Cons, Goal)|Goals],
		Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R1),
	move_follow_code_in_cases(Goals0, Goals, Flags, R1, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_conj(list(hlds__goal), list(hlds__goal),
	pair(bool), bool, bool).
:- mode move_follow_code_in_conj(in, out, in, in, out) is det.

	% Find the first branched structure, and split the
	% conj into those goals before and after it.

move_follow_code_in_conj(Goals0, Goals, Flags, R0, R) :-
	move_follow_code_in_conj_2(Goals0, [], RevGoals, Flags, R0, R),
	list__reverse(RevGoals, Goals).

:- pred move_follow_code_in_conj_2(list(hlds__goal), list(hlds__goal),
	list(hlds__goal), pair(bool), bool, bool).
:- mode move_follow_code_in_conj_2(in, in, out, in, in, out) is det.

move_follow_code_in_conj_2([], RevPrevGoals, RevPrevGoals, _, R, R).
move_follow_code_in_conj_2([Goal0 | Goals0], RevPrevGoals0, RevPrevGoals,
		Flags, R0, R) :-
	Flags = PushFollowCode - PushPrevCode,
	(
		PushFollowCode = yes,
		move_follow_code_is_branched(Goal0),
		move_follow_code_select(Goals0, FollowGoals, RestGoalsPrime),
		FollowGoals \= [],
		move_follow_code_move_goals(Goal0, FollowGoals, Goal1Prime)
	->
		R1 = yes,
		Goal1 = Goal1Prime,
		RestGoals = RestGoalsPrime
	;
		R1 = R0,
		Goal1 = Goal0,
		RestGoals = Goals0
	),
	(
		PushPrevCode = yes
	->
		move_prev_code_forbidden_vars(RestGoals, ForbiddenVars),
		move_prev_code(Goal1, Goal2, ForbiddenVars,
			RevPrevGoals0, RevPrevGoals1, R1, R2)
	;
		RevPrevGoals1 = RevPrevGoals0,
		Goal2 = Goal1,
		R2 = R1
	),
	move_follow_code_in_goal(Goal2, Goal, Flags, R2, R3),
	RevPrevGoals2 = [Goal | RevPrevGoals1],
	move_follow_code_in_conj_2(RestGoals, RevPrevGoals2, RevPrevGoals,
		Flags, R3, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_select(list(hlds__goal), list(hlds__goal),
	list(hlds__goal)).
:- mode move_follow_code_select(in, out, out) is det.

move_follow_code_select([], [], []).
move_follow_code_select([Goal|Goals], FollowGoals, RestGoals) :-
	(
		move_follow_code_is_builtin(Goal)
	->
		move_follow_code_select(Goals, FollowGoals0, RestGoals),
		FollowGoals = [Goal|FollowGoals0]
	;
		move_follow_code_is_call(Goal)
	->
		FollowGoals = [Goal],
		RestGoals = Goals
	;
		FollowGoals = [],
		RestGoals = [Goal|Goals]
	).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals(hlds__goal, list(hlds__goal), hlds__goal).
:- mode move_follow_code_move_goals(in, in, out) is semidet.

move_follow_code_move_goals(Goal0 - GoalInfo, FollowGoals, Goal - GoalInfo) :-
	(
		Goal0 = switch(Var, Det, Cases0),
		move_follow_code_move_goals_cases(Cases0, FollowGoals, Cases),
		Goal = switch(Var, Det, Cases)
	;
		Goal0 = disj(Goals0),
		move_follow_code_move_goals_disj(Goals0, FollowGoals, Goals),
		Goal = disj(Goals)
	;
		Goal0 = if_then_else(Vars, Cond, Then0, Else0),
		conjoin_goal_and_goal_list(Then0, FollowGoals, Then),
		conjoin_goal_and_goal_list(Else0, FollowGoals, Else),
		Goal = if_then_else(Vars, Cond, Then, Else)
	).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_cases(list(case), list(hlds__goal),
	list(case)).
:- mode move_follow_code_move_goals_cases(in, in, out) is semidet.

move_follow_code_move_goals_cases([], _FollowGoals, []).
move_follow_code_move_goals_cases([Case0|Cases0], FollowGoals, [Case|Cases]) :-
	Case0 = case(Cons, Goal0),
	conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal),
	Case = case(Cons, Goal),
	move_follow_code_move_goals_cases(Cases0, FollowGoals, Cases).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_disj(list(hlds__goal), list(hlds__goal),
	list(hlds__goal)).
:- mode move_follow_code_move_goals_disj(in, in, out) is semidet.

move_follow_code_move_goals_disj([], _FollowGoals, []).
move_follow_code_move_goals_disj([Goal0|Goals0], FollowGoals, [Goal|Goals]) :-
	conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal),
	move_follow_code_move_goals_disj(Goals0, FollowGoals, Goals).

%-----------------------------------------------------------------------------%

	% Takes a goal and a list of goals, and conjoins them
	% (with a potentially blank goal_info).

:- pred conjoin_goal_and_goal_list(hlds__goal, list(hlds__goal),
	hlds__goal).
:- mode conjoin_goal_and_goal_list(in, in, out) is semidet.

conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_determinism(GoalInfo0, Detism0),
	determinism_components(Detism0, CanFail0, MaxSolns0),
	check_follow_code_detism(FollowGoals, CanFail0, MaxSolns0),
	( GoalExpr0 = conj(GoalList0) ->
		list__append(GoalList0, FollowGoals, GoalList),
		GoalExpr = conj(GoalList)
	;
		GoalExpr = conj([Goal0 | FollowGoals])
	),
	Goal = GoalExpr - GoalInfo0.

	% This check is necessary to make sure that follow_code
	% doesn't change the determinism of the goal.

:- pred check_follow_code_detism(list(hlds__goal), can_fail, soln_count).
:- mode check_follow_code_detism(in, in, in) is semidet.

check_follow_code_detism([], _, _).
check_follow_code_detism([_ - GoalInfo | Goals], CanFail0, MaxSolns0) :-
	goal_info_get_determinism(GoalInfo, Detism1),
	determinism_components(Detism1, CanFail1, MaxSolns1),
	det_conjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns0),
	det_conjunction_canfail(CanFail0, CanFail1, CanFail0),
	check_follow_code_detism(Goals, CanFail0, MaxSolns0).

%-----------------------------------------------------------------------------%

	% See if the given goal has multiple paths through it.

:- pred move_follow_code_is_branched(hlds__goal).
:- mode move_follow_code_is_branched(in) is semidet.

move_follow_code_is_branched(switch(_,_,_) - _GoalInfo).
move_follow_code_is_branched(if_then_else(_,_,_,_) - _GoalInfo).
move_follow_code_is_branched(disj(_) - _GoalInfo).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_is_builtin(hlds__goal).
:- mode move_follow_code_is_builtin(in) is semidet.

move_follow_code_is_builtin(unify(_,_,_,Unification,_) - _GoalInfo) :-
	Unification \= complicated_unify(_, _, _).
move_follow_code_is_builtin(call(_,_,_,Builtin, _, _, _) - _GoalInfo) :-
	is_builtin__is_inline(Builtin).

:- pred move_follow_code_is_call(hlds__goal).
:- mode move_follow_code_is_call(in) is semidet.

move_follow_code_is_call(call(_,_,_,Builtin, _, _, _) - _GoalInfo) :-
	\+ is_builtin__is_inline(Builtin).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred move_prev_code(hlds__goal, hlds__goal, set(var),
	list(hlds__goal), list(hlds__goal), bool, bool).
% :- mode move_prev_code(di, uo, in, di, uo, in, out) is det.
:- mode move_prev_code(in, out, in, in, out, in, out) is det.

move_prev_code(Goal, Goal, _, RevPrevGoals, RevPrevGoals, R, R).

% move_prev_code(Goal0, Goal, ForbiddenVars0, RevPrevGoals0, RevPrevGoals,
% 		R0, R) :-
% 	(
% 		move_prev_code_breakup_branched(Goal0, Cond0, First0, Rest0)
% 	->
% 		move_prev_code_new_forbidden_vars(Cond0, First0,
% 			ForbiddenVars0, ForbiddenVars1),
% 		(
% 			move_prev_code_vars_difference(Cond0, First0, Rest0,
% 				ForbiddenVars0, LocalVars),
% 			move_prev_code_can_pull_producer(LocalVars, Producers,
% 				RevPrevGoals0, RevPrevGoals1)
% 		->
% 			move_prev_code(Rest0, Rest, ForbiddenVars1,
% 				RevPrevGoals1, RevPrevGoals, R0, R),
% 			move_prev_code_replace_first(Goal0,
% 				Producers, Rest, Goal)
% 		;
% 			move_prev_code(Rest0, Rest, ForbiddenVars1,
% 				RevPrevGoals0, RevPrevGoals, R0, R)
% 		)
% 	;
% 		R = R0,
% 		Goal = Goal0,
% 		RevPrevGoals = [Goal0 | RevPrevGoals0]
% 	).
% 
% :- pred move_prev_code_breakup_branched(hlds__goal, hlds__goal, hlds__goal,
% 	hlds__goal).
% :- mode move_prev_code_breakup_branched(in, out, out, out) is semidet.
 
:- pred move_prev_code_forbidden_vars(list(hlds__goal), set(var)).
:- mode move_prev_code_forbidden_vars(in, out) is det.

move_prev_code_forbidden_vars([], Empty) :-
	set__init(Empty).
move_prev_code_forbidden_vars([_Goal - GoalInfo | Goals], Varset) :-
	move_prev_code_forbidden_vars(Goals, Varset0),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__union(Varset0, NonLocals, Varset).

% 
% :- pred move_prev_code_new_forbidden_vars(hlds__goal, hlds__goal,
% 	set(var), set(var)).
% :- mode move_prev_code_new_forbidden_vars(in, in, in, out) is det.
% 
% :- pred move_prev_code_vars_difference(hlds__goal, hlds__goal, hlds__goal,
% 	set(var), set(var)).
% :- mode move_prev_code_vars_difference(in, in, in, in, out) is det.
% 
% move_prev_code_vars_difference(Cond, First, Rest, ForbiddenVars, LocalVars) :-
% 	Cond  = _ - CondInfo,
% 	First = _ - FirstInfo,
% 	Rest  = _ - RestInfo,
% 	goal_info_get_nonlocals(CondInfo,  CondVars),
% 	goal_info_get_nonlocals(FirstInfo, FirstVars),
% 	goal_info_get_nonlocals(RestInfo,  RestVars),
% 	set__union(CondVars, FirstVars, ThisBranchVars),
% 	set__difference(ThisBranchVars, RestVars, LocalVars0),
% 	set__difference(LocalVars0, ForbiddenVars, LocalVars).
% 
% :- pred move_prev_code_can_pull_producer(set(var), list(hlds__goal),
% 	list(hlds__goal), list(hlds__goal)).
% :- mode move_prev_code_can_pull_producer(in, in, di, uo) is det.
% 
% :- pred move_prev_code_replace_first(hlds__goal,
% 	list(hlds__goal), list(hlds__goal), hlds__goal).
% :- mode move_prev_code_replace_first(di, in, in, uo) is det.
