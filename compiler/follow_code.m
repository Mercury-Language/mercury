%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
% Extensive modifications by zs.

%-----------------------------------------------------------------------------%

:- module follow_code.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal.
:- import_module list.

:- pred move_follow_code_in_proc(proc_info, proc_info,
	module_info, module_info).
% :- mode move_follow_code_in_proc(di, uo, di, uo) is det.
:- mode move_follow_code_in_proc(in, out, in, out) is det.

	% Split a list of goals into the prefix of builtins and the rest.
:- pred move_follow_code_select(list(hlds_goal), list(hlds_goal),
	list(hlds_goal)).
:- mode move_follow_code_select(in, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, goal_util, mode_util.
:- import_module globals, options, det_analysis, quantification.
:- import_module bool, map, set, term, std_util, require.

%-----------------------------------------------------------------------------%

move_follow_code_in_proc(ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo) :-
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, follow_code, FollowCode),
	globals__lookup_bool_option(Globals, prev_code, PrevCode),
	Flags = FollowCode - PrevCode,
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	(
		move_follow_code_in_goal(Goal0, Goal1, Flags, no, Res),
			% did the goal change?
		Res = yes
	->
			% we need to fix up the goal_info by recalculating
			% the nonlocal vars and the non-atomic instmap deltas.
		proc_info_headvars(ProcInfo0, HeadVars),
		implicitly_quantify_clause_body(HeadVars, Goal1,
			Varset0, VarTypes0, Goal2, Varset, VarTypes, _Warnings),
		proc_info_get_initial_instmap(ProcInfo0,
			ModuleInfo0, InstMap0),
		proc_info_inst_table(ProcInfo0, InstTable0),
		proc_info_headvars(ProcInfo0, ArgVars),
		proc_info_arglives(ProcInfo0, ModuleInfo0, ArgLives),
		recompute_instmap_delta(ArgVars, ArgLives, VarTypes,
			Goal2, Goal, InstMap0, InstTable0, InstTable,
			_, ModuleInfo0, ModuleInfo),
		proc_info_set_inst_table(ProcInfo0, InstTable, ProcInfo1),
		proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
		proc_info_set_varset(ProcInfo2, Varset, ProcInfo3),
		proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo)
	;
		ModuleInfo = ModuleInfo0,
		ProcInfo = ProcInfo0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal(hlds_goal, hlds_goal, pair(bool),
	bool, bool).
:- mode move_follow_code_in_goal(in, out, in, in, out) is det.

move_follow_code_in_goal(Goal0 - GoalInfo, Goal - GoalInfo, Flags, R0, R) :-
	move_follow_code_in_goal_2(Goal0, Goal, Flags, R0, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal_2(hlds_goal_expr, hlds_goal_expr,
	pair(bool), bool, bool).
:- mode move_follow_code_in_goal_2(in, out, in, in, out) is det.

move_follow_code_in_goal_2(conj(Goals0), conj(Goals), Flags, R0, R) :-
	move_follow_code_in_conj(Goals0, Goals, Flags, R0, R).

move_follow_code_in_goal_2(par_conj(Goals0, SM), par_conj(Goals, SM),
		Flags, R0, R) :-
		% move_follow_code_in_disj treats its list of goals as
		% independent goals, so we can use it to process the
		% independent parallel conjuncts.
	move_follow_code_in_disj(Goals0, Goals, Flags, R0, R).

move_follow_code_in_goal_2(disj(Goals0, SM), disj(Goals, SM), Flags, R0, R) :-
	move_follow_code_in_disj(Goals0, Goals, Flags, R0, R).

move_follow_code_in_goal_2(not(Goal0), not(Goal), Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R).

move_follow_code_in_goal_2(switch(Var, Det, Cases0, SM),
		switch(Var, Det, Cases, SM), Flags, R0, R) :-
	move_follow_code_in_cases(Cases0, Cases, Flags, R0, R).

move_follow_code_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		if_then_else(Vars, Cond, Then, Else, SM), Flags, R0, R) :-
	move_follow_code_in_goal(Cond0, Cond, Flags, R0, R1),
	move_follow_code_in_goal(Then0, Then, Flags, R1, R2),
	move_follow_code_in_goal(Else0, Else, Flags, R2, R).

move_follow_code_in_goal_2(some(Vars, Goal0), some(Vars, Goal), Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R).

move_follow_code_in_goal_2(higher_order_call(A,B,C,D,E,F),
			higher_order_call(A,B,C,D,E,F), _, R, R).

move_follow_code_in_goal_2(class_method_call(A,B,C,D,E,F),
			class_method_call(A,B,C,D,E,F), _, R, R).

move_follow_code_in_goal_2(call(A,B,C,D,E,F), call(A,B,C,D,E,F), _, R, R).

move_follow_code_in_goal_2(unify(A,B,C,D,E), unify(A,B,C,D,E), _, R, R).

move_follow_code_in_goal_2(pragma_c_code(A,B,C,D,E,F,G), 
			pragma_c_code(A,B,C,D,E,F,G), _, R, R).

%-----------------------------------------------------------------------------%

	% move_follow_code_in_disj is used both for disjunction and
	% parallel conjunction.

:- pred move_follow_code_in_disj(list(hlds_goal), list(hlds_goal),
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
move_follow_code_in_cases([case(Cons, IMDelta, Goal0)|Goals0],
		[case(Cons, IMDelta, Goal)|Goals], Flags, R0, R) :-
	move_follow_code_in_goal(Goal0, Goal, Flags, R0, R1),
	move_follow_code_in_cases(Goals0, Goals, Flags, R1, R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_conj(list(hlds_goal), list(hlds_goal),
	pair(bool), bool, bool).
:- mode move_follow_code_in_conj(in, out, in, in, out) is det.

	% Find the first branched structure, and split the
	% conj into those goals before and after it.

move_follow_code_in_conj(Goals0, Goals, Flags, R0, R) :-
	move_follow_code_in_conj_2(Goals0, [], RevGoals, Flags, R0, R),
	list__reverse(RevGoals, Goals).

:- pred move_follow_code_in_conj_2(list(hlds_goal), list(hlds_goal),
	list(hlds_goal), pair(bool), bool, bool).
:- mode move_follow_code_in_conj_2(in, in, out, in, in, out) is det.

move_follow_code_in_conj_2([], RevPrevGoals, RevPrevGoals, _, R, R).
move_follow_code_in_conj_2([Goal0 | Goals0], RevPrevGoals0, RevPrevGoals,
		Flags, R0, R) :-
	Flags = PushFollowCode - PushPrevCode,
	(
		PushFollowCode = yes,
		Goal0 = GoalExpr0 - _,
		goal_util__goal_is_branched(GoalExpr0),
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

move_follow_code_select([], [], []).
move_follow_code_select([Goal|Goals], FollowGoals, RestGoals) :-
	(
		move_follow_code_is_builtin(Goal)
	->
		move_follow_code_select(Goals, FollowGoals0, RestGoals),
		FollowGoals = [Goal|FollowGoals0]
	;
		FollowGoals = [],
		RestGoals = [Goal|Goals]
	).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals(hlds_goal, list(hlds_goal), hlds_goal).
:- mode move_follow_code_move_goals(in, in, out) is semidet.

move_follow_code_move_goals(Goal0 - GoalInfo, FollowGoals, Goal - GoalInfo) :-
	(
		Goal0 = switch(Var, Det, Cases0, SM),
		move_follow_code_move_goals_cases(Cases0, FollowGoals, Cases),
		Goal = switch(Var, Det, Cases, SM)
	;
		Goal0 = disj(Goals0, SM),
		move_follow_code_move_goals_disj(Goals0, FollowGoals, Goals),
		Goal = disj(Goals, SM)
	;
		Goal0 = if_then_else(Vars, Cond, Then0, Else0, SM),
		follow_code__conjoin_goal_and_goal_list(Then0,
			FollowGoals, Then),
		follow_code__conjoin_goal_and_goal_list(Else0,
			FollowGoals, Else),
		Goal = if_then_else(Vars, Cond, Then, Else, SM)
	).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_cases(list(case), list(hlds_goal),
	list(case)).
:- mode move_follow_code_move_goals_cases(in, in, out) is semidet.

move_follow_code_move_goals_cases([], _FollowGoals, []).
move_follow_code_move_goals_cases([Case0|Cases0], FollowGoals, [Case|Cases]) :-
	Case0 = case(Cons, IMDelta, Goal0),
	follow_code__conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal),
	Case = case(Cons, IMDelta, Goal),
	move_follow_code_move_goals_cases(Cases0, FollowGoals, Cases).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_disj(list(hlds_goal), list(hlds_goal),
	list(hlds_goal)).
:- mode move_follow_code_move_goals_disj(in, in, out) is semidet.

move_follow_code_move_goals_disj([], _FollowGoals, []).
move_follow_code_move_goals_disj([Goal0|Goals0], FollowGoals, [Goal|Goals]) :-
	follow_code__conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal),
	move_follow_code_move_goals_disj(Goals0, FollowGoals, Goals).

%-----------------------------------------------------------------------------%

	% Takes a goal and a list of goals, and conjoins them
	% (with a potentially blank goal_info), checking that the
	% determinism of the goal is not changed.

:- pred follow_code__conjoin_goal_and_goal_list(hlds_goal, list(hlds_goal),
	hlds_goal).
:- mode follow_code__conjoin_goal_and_goal_list(in, in, out) is semidet.

follow_code__conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_determinism(GoalInfo0, Detism0),
	determinism_components(Detism0, _CanFail0, MaxSolns0),
	( MaxSolns0 = at_most_zero ->	
		Goal = Goal0
	;
		check_follow_code_detism(FollowGoals, Detism0),
		( GoalExpr0 = conj(GoalList0) ->
			list__append(GoalList0, FollowGoals, GoalList),
			GoalExpr = conj(GoalList)
		;
			GoalExpr = conj([Goal0 | FollowGoals])
		),
		Goal = GoalExpr - GoalInfo0
	).

	% This check is necessary to make sure that follow_code
	% doesn't change the determinism of the goal.

:- pred check_follow_code_detism(list(hlds_goal), determinism).
:- mode check_follow_code_detism(in, in) is semidet.

check_follow_code_detism([], _).
check_follow_code_detism([_ - GoalInfo | Goals], Detism0) :-
	goal_info_get_determinism(GoalInfo, Detism1),
	det_conjunction_detism(Detism0, Detism1, Detism0),
	check_follow_code_detism(Goals, Detism0).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_is_builtin(hlds_goal).
:- mode move_follow_code_is_builtin(in) is semidet.

move_follow_code_is_builtin(unify(_, _, _, Unification, _) - _GoalInfo) :-
	Unification \= complicated_unify(_, _).
move_follow_code_is_builtin(call(_, _, _, Builtin, _, _) - _GoalInfo) :-
	Builtin = inline_builtin.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred move_prev_code(hlds_goal, hlds_goal, set(var),
	list(hlds_goal), list(hlds_goal), bool, bool).
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
% :- pred move_prev_code_breakup_branched(hlds_goal, hlds_goal, hlds_goal,
% 	hlds_goal).
% :- mode move_prev_code_breakup_branched(in, out, out, out) is semidet.

:- pred move_prev_code_forbidden_vars(list(hlds_goal), set(var)).
:- mode move_prev_code_forbidden_vars(in, out) is det.

move_prev_code_forbidden_vars([], Empty) :-
	set__init(Empty).
move_prev_code_forbidden_vars([_Goal - GoalInfo | Goals], Varset) :-
	move_prev_code_forbidden_vars(Goals, Varset0),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__union(Varset0, NonLocals, Varset).

% 
% :- pred move_prev_code_new_forbidden_vars(hlds_goal, hlds_goal,
% 	set(var), set(var)).
% :- mode move_prev_code_new_forbidden_vars(in, in, in, out) is det.
% 
% :- pred move_prev_code_vars_difference(hlds_goal, hlds_goal, hlds_goal,
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
% :- pred move_prev_code_can_pull_producer(set(var), list(hlds_goal),
% 	list(hlds_goal), list(hlds_goal)).
% :- mode move_prev_code_can_pull_producer(in, in, di, uo) is det.
% 
% :- pred move_prev_code_replace_first(hlds_goal,
% 	list(hlds_goal), list(hlds_goal), hlds_goal).
% :- mode move_prev_code_replace_first(di, in, in, uo) is det.
