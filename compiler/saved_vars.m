%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: zs.

% This module traverses the goal for each procedure, looking for and
% exploiting opportunities to reduce the number of variables that have
% to be saved across calls.
%
% At the moment the only opportunity we look for is the assignment of
% constants to variables. These assignments should be delayed until
% the value of the variable is needed. If the variable has several uses,
% we generate a copy for each use, renaming the using goal to refer to the
% variable by its new name.
%
% We thread the SlotInfo structure through the module; this allows us
% to define new variables.

%-----------------------------------------------------------------------------%

:- module ll_backend__saved_vars.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module io.

:- pred saved_vars_proc(pred_id::in, proc_id::in,
	proc_info::in, proc_info::out, module_info::in, module_info::out,
	io__state::di, io__state::uo) is det.

:- pred saved_vars_proc_no_io(proc_info::in, proc_info::out,
	module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_out, hlds__goal_util.
:- import_module hlds__quantification, hlds__passes_aux.
:- import_module check_hlds__mode_util, parse_tree__prog_data, term, varset.
:- import_module bool, list, set, map, std_util, require.

%-----------------------------------------------------------------------------%

saved_vars_proc(PredId, ProcId, ProcInfo0, ProcInfo,
		ModuleInfo0, ModuleInfo) -->
	write_proc_progress_message("% Minimizing saved vars in ",
		PredId, ProcId, ModuleInfo0),
	{ saved_vars_proc_no_io(ProcInfo0, ProcInfo,
		ModuleInfo0, ModuleInfo) }.

saved_vars_proc_no_io(ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	init_slot_info(Varset0, VarTypes0, SlotInfo0),

	saved_vars_in_goal(Goal0, SlotInfo0, Goal1, SlotInfo),

	final_slot_info(Varset1, VarTypes1, SlotInfo),
	proc_info_headvars(ProcInfo0, HeadVars),

	% hlds_out__write_goal(Goal1, ModuleInfo, Varset1, 0, "\n"),

	% recompute the nonlocals for each goal
	implicitly_quantify_clause_body(HeadVars, Goal1, Varset1,
		VarTypes1, Goal2, Varset, VarTypes, _Warnings),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_inst_varset(ProcInfo0, InstVarSet),
	recompute_instmap_delta(no, Goal2, Goal, VarTypes, InstVarSet, InstMap0,
		ModuleInfo0, ModuleInfo),

	% hlds_out__write_goal(Goal, ModuleInfo, Varset, 0, "\n"),

	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo).

%-----------------------------------------------------------------------------%

:- pred saved_vars_in_goal(hlds_goal, slot_info, hlds_goal, slot_info).
:- mode saved_vars_in_goal(in, in, out, out) is det.

saved_vars_in_goal(GoalExpr0 - GoalInfo0, SlotInfo0, Goal, SlotInfo) :-
	(
		GoalExpr0 = conj(Goals0),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		saved_vars_in_conj(Goals0, NonLocals, SlotInfo0,
			Goals, SlotInfo),
		conj_list_to_goal(Goals, GoalInfo0, Goal)
	;
		GoalExpr0 = par_conj(Goals0),
			% saved_vars_in_disj treats its goal list as
			% an independent list of goals, so we can use
			% it to process the list of parallel conjuncts too.
		saved_vars_in_disj(Goals0, SlotInfo0, Goals, SlotInfo),
		Goal = par_conj(Goals) - GoalInfo0
	;
		GoalExpr0 = disj(Goals0),
		saved_vars_in_disj(Goals0, SlotInfo0, Goals, SlotInfo),
		Goal = disj(Goals) - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		saved_vars_in_goal(NegGoal0, SlotInfo0, NegGoal, SlotInfo),
		Goal = not(NegGoal) - GoalInfo0
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		saved_vars_in_switch(Cases0, SlotInfo0, Cases, SlotInfo),
		Goal = switch(Var, CanFail, Cases) - GoalInfo0
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
		saved_vars_in_goal(Cond0, SlotInfo0, Cond, SlotInfo1),
		saved_vars_in_goal(Then0, SlotInfo1, Then, SlotInfo2),
		saved_vars_in_goal(Else0, SlotInfo2, Else, SlotInfo),
		Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo0
	;
		GoalExpr0 = some(Var, CanRemove, SubGoal0),
		saved_vars_in_goal(SubGoal0, SlotInfo0, SubGoal, SlotInfo),
		Goal = some(Var, CanRemove, SubGoal) - GoalInfo0
	;
		GoalExpr0 = generic_call(_, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		SlotInfo = SlotInfo0
	;
		GoalExpr0 = call(_, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		SlotInfo = SlotInfo0
	;
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		SlotInfo = SlotInfo0
	;
		GoalExpr0 = foreign_proc(_, _, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		SlotInfo = SlotInfo0
	;
		GoalExpr0 = shorthand(_),
		% these should have been expanded out by now
		error("saved_vars_in_goal: unexpected shorthand")
	).

%-----------------------------------------------------------------------------%

% If we find a unification that assigns a constant to a variable,
% attempt to push it into the following code.
%
% We cannot push such a unification if the following goal needs the value
% of the variable. We also avoid attempting to push the unification into
% any following similar unifications, since the order of such unifications
% does not matter, and we would like to avoid "pushing contests" between
% several such unifications about which one should be closest to a following
% goal that uses all the variables they define.

:- pred saved_vars_in_conj(list(hlds_goal), set(prog_var), slot_info,
	list(hlds_goal), slot_info).
:- mode saved_vars_in_conj(in, in, in, out, out) is det.

saved_vars_in_conj([], _, SlotInfo, [], SlotInfo).
saved_vars_in_conj([Goal0 | Goals0], NonLocals, SlotInfo0,
		Goals, SlotInfo) :-
	(
		Goal0 = unify(_, _, _, Unif, _) - _,
		Unif = construct(Var, _, [], _, _, _, _),
		skip_constant_constructs(Goals0, Constants, Others),
		Others = [First | _Rest],
		can_push(Var, First)
	->
		set__is_member(Var, NonLocals, IsNonLocal),
		saved_vars_delay_goal(Others, Goal0, Var, IsNonLocal,
			SlotInfo0, Goals1, SlotInfo1),
		list__append(Constants, Goals1, Goals2),
		saved_vars_in_conj(Goals2, NonLocals, SlotInfo1,
			Goals, SlotInfo)
	;
		saved_vars_in_goal(Goal0, SlotInfo0, Goal1, SlotInfo1),
		saved_vars_in_conj(Goals0, NonLocals, SlotInfo1,
			Goals1, SlotInfo),
		Goals = [Goal1 | Goals1]
	).

% Divide a list of goals into an initial subsequence of goals that
% construct constants, and all other goals.

:- pred skip_constant_constructs(list(hlds_goal), list(hlds_goal),
	list(hlds_goal)).
:- mode skip_constant_constructs(in, out, out) is det.

skip_constant_constructs([], [], []).
skip_constant_constructs([Goal0 | Goals0], Constants, Others) :-
	(
		Goal0 = unify(_, _, _, Unif, _) - _,
		Unif = construct(_, _, [], _, _, _, _)
	->
		skip_constant_constructs(Goals0, Constants1, Others),
		Constants = [Goal0 | Constants1]
	;
		Constants = [],
		Others = [Goal0 | Goals0]
	).

% Decide whether the value of the given variable is needed immediately
% in the goal, or whether the unification that constructs a value for
% the variable can be usefully pushed into the goal.
%
% The logic of this predicate must match the logic of saved_vars_delay_goal.

:- pred can_push(prog_var, hlds_goal).
:- mode can_push(in, in) is semidet.

can_push(Var, First) :-
	First = FirstExpr - FirstInfo,
	goal_info_get_nonlocals(FirstInfo, FirstNonLocals),
	( set__member(Var, FirstNonLocals) ->
		(
			FirstExpr = conj(_)
		;
			FirstExpr = some(_, _, _)
		;
			FirstExpr = not(_)
		;
			FirstExpr = disj(_)
		;
			FirstExpr = switch(SwitchVar, _, _),
			Var \= SwitchVar
		;
			FirstExpr = if_then_else(_, _, _, _)
		)
	;
		true
	).

% The main inputs of this predicate are a list of goals in a conjunction,
% and a goal Construct that assigns a constant to a variable Var.
%
% When we find an atomic goal in the conjunction that refers to Var,
% we create a new variable NewVar, rename both this goal and Construct
% to refer to NewVar instead of Var, and insert the new version of Construct
% before the new version of the goal.
%
% When we find a non-atomic goal in the conjunction that refers to Var,
% we push Construct into each of its components.
%
% If Var is exported from the conjunction, we include Construct at the end
% of the conjunction to give it its value.

:- pred saved_vars_delay_goal(list(hlds_goal), hlds_goal, prog_var, bool,
	slot_info, list(hlds_goal), slot_info).
:- mode saved_vars_delay_goal(in, in, in, in, in, out, out) is det.

saved_vars_delay_goal([], Construct, _Var, IsNonLocal, SlotInfo,
		Goals, SlotInfo) :-
	(
		IsNonLocal = yes,
		Goals = [Construct]
	;
		IsNonLocal = no,
		Goals = []
	).
saved_vars_delay_goal([Goal0 | Goals0], Construct, Var, IsNonLocal, SlotInfo0,
		Goals, SlotInfo) :-
	Goal0 = Goal0Expr - Goal0Info,
	goal_info_get_nonlocals(Goal0Info, Goal0NonLocals),
	( set__member(Var, Goal0NonLocals) ->
		(
			Goal0Expr = unify(_, _, _, _, _),
			rename_var(SlotInfo0, Var, _NewVar, Subst, SlotInfo1),
			goal_util__rename_vars_in_goal(Construct, Subst,
				NewConstruct),
			goal_util__rename_vars_in_goal(Goal0, Subst, Goal1),
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo1, Goals1, SlotInfo),
			Goals = [NewConstruct, Goal1 | Goals1]
		;
			Goal0Expr = call(_, _, _, _, _, _),
			rename_var(SlotInfo0, Var, _NewVar, Subst, SlotInfo1),
			goal_util__rename_vars_in_goal(Construct, Subst,
				NewConstruct),
			goal_util__rename_vars_in_goal(Goal0, Subst, Goal1),
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo1, Goals1, SlotInfo),
			Goals = [NewConstruct, Goal1 | Goals1]
		;
			Goal0Expr = generic_call(_, _, _, _),
			rename_var(SlotInfo0, Var, _NewVar, Subst, SlotInfo1),
			goal_util__rename_vars_in_goal(Construct, Subst,
				NewConstruct),
			goal_util__rename_vars_in_goal(Goal0, Subst, Goal1),
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo1, Goals1, SlotInfo),
			Goals = [NewConstruct, Goal1 | Goals1]
		;
			Goal0Expr = foreign_proc(_, _, _, _, _, _, _),
			rename_var(SlotInfo0, Var, _NewVar, Subst, SlotInfo1),
			goal_util__rename_vars_in_goal(Construct, Subst,
				NewConstruct),
			goal_util__rename_vars_in_goal(Goal0, Subst, Goal1),
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo1, Goals1, SlotInfo),
			Goals = [NewConstruct, Goal1 | Goals1]
		;
			Goal0Expr = conj(Conj),
			list__append(Conj, Goals0, Goals1),
			saved_vars_delay_goal(Goals1, Construct, Var,
				IsNonLocal, SlotInfo0, Goals, SlotInfo)
		;
			Goal0Expr = par_conj(_ParConj),
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo0, Goals1, SlotInfo),
			Goals = [Goal0|Goals1]
		;
			Goal0Expr = some(SomeVars, CanRemove, SomeGoal0),
			rename_var(SlotInfo0, Var, NewVar, Subst, SlotInfo1),
			goal_util__rename_vars_in_goal(Construct, Subst,
				NewConstruct),
			goal_util__rename_vars_in_goal(SomeGoal0, Subst,
				SomeGoal1),
			push_into_goal(SomeGoal1, NewConstruct, NewVar,
				SlotInfo1, SomeGoal, SlotInfo2),
			Goal1 = some(SomeVars, CanRemove, SomeGoal)
					- Goal0Info,
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo2, Goals1, SlotInfo),
			Goals = [Goal1 | Goals1]
		;
			Goal0Expr = not(NegGoal0),
			rename_var(SlotInfo0, Var, NewVar, Subst, SlotInfo1),
			goal_util__rename_vars_in_goal(Construct, Subst,
				NewConstruct),
			goal_util__rename_vars_in_goal(NegGoal0, Subst,
				NegGoal1),
			push_into_goal(NegGoal1, NewConstruct, NewVar,
				SlotInfo1, NegGoal, SlotInfo2),
			Goal1 = not(NegGoal) - Goal0Info,
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo2, Goals1, SlotInfo),
			Goals = [Goal1 | Goals1]
		;
			Goal0Expr = disj(Disjuncts0),
			push_into_goals_rename(Disjuncts0, Construct, Var,
				SlotInfo0, Disjuncts, SlotInfo1),
			Goal1 = disj(Disjuncts) - Goal0Info,
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo1, Goals1, SlotInfo),
			Goals = [Goal1 | Goals1]
		;
			Goal0Expr = switch(SwitchVar, CF, Cases0),
			( SwitchVar = Var ->
				saved_vars_delay_goal(Goals0, Construct, Var,
					IsNonLocal, SlotInfo0,
					Goals1, SlotInfo),
				Goals = [Construct, Goal0 | Goals1]
			;
				push_into_cases_rename(Cases0, Construct, Var,
					SlotInfo0, Cases, SlotInfo1),
				Goal1 = switch(SwitchVar, CF, Cases)
					- Goal0Info,
				saved_vars_delay_goal(Goals0, Construct, Var,
					IsNonLocal, SlotInfo1,
					Goals1, SlotInfo),
				Goals = [Goal1 | Goals1]
			)
		;
			Goal0Expr = if_then_else(V, Cond0, Then0, Else0),
			push_into_goal_rename(Cond0, Construct, Var,
				SlotInfo0, Cond, SlotInfo1),
			push_into_goal_rename(Then0, Construct, Var,
				SlotInfo1, Then, SlotInfo2),
			push_into_goal_rename(Else0, Construct, Var,
				SlotInfo2, Else, SlotInfo3),
			Goal1 = if_then_else(V, Cond, Then, Else)
				- Goal0Info,
			saved_vars_delay_goal(Goals0, Construct, Var,
				IsNonLocal, SlotInfo3, Goals1, SlotInfo),
			Goals = [Goal1 | Goals1]
		;
			Goal0Expr = shorthand(_),
			% these should have been expanded out by now
			error("saved_vars_delay_goal: unexpected shorthand")
		)
	;
		saved_vars_delay_goal(Goals0, Construct, Var, IsNonLocal,
			SlotInfo0, Goals1, SlotInfo),
		Goals = [Goal0 | Goals1]
	).

% Push a non-renamed version of the given construction into the given goal.
% Also traverse the goal looking for further opprtunities.

:- pred push_into_goal(hlds_goal, hlds_goal, prog_var, slot_info,
	hlds_goal, slot_info).
:- mode push_into_goal(in, in, in, in, out, out) is det.

push_into_goal(Goal0, Construct, Var, SlotInfo0, Goal, SlotInfo) :-
	saved_vars_in_goal(Goal0, SlotInfo0, Goal1, SlotInfo1),
	Goal1 = _ - Goal1Info,
	goal_to_conj_list(Goal1, Conj1),
	saved_vars_delay_goal(Conj1, Construct, Var, no, SlotInfo1,
		Conj, SlotInfo),
	conj_list_to_goal(Conj, Goal1Info, Goal).

% Push a renamed version of the given construction into the given goal.
% If the goal does not refer to the variable bound by the construction,
% then this would have no effect, so we merely traverse the goal looking
% for other opportunities.

:- pred push_into_goal_rename(hlds_goal, hlds_goal, prog_var, slot_info,
	hlds_goal, slot_info).
:- mode push_into_goal_rename(in, in, in, in, out, out) is det.

push_into_goal_rename(Goal0, Construct, Var, SlotInfo0, Goal, SlotInfo) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	( set__member(Var, NonLocals) ->
		rename_var(SlotInfo0, Var, NewVar, Subst, SlotInfo1),
		goal_util__rename_vars_in_goal(Construct, Subst, NewConstruct),
		goal_util__rename_vars_in_goal(Goal0, Subst, Goal1),
		push_into_goal(Goal1, NewConstruct, NewVar, SlotInfo1,
			Goal, SlotInfo)
	;
		saved_vars_in_goal(Goal0, SlotInfo0, Goal, SlotInfo)
	).

% Push renamed versions of the given construction into each of several goals.

:- pred push_into_goals_rename(list(hlds_goal), hlds_goal, prog_var, slot_info,
	list(hlds_goal), slot_info).
:- mode push_into_goals_rename(in, in, in, in, out, out) is det.

push_into_goals_rename([], _Construct, _Var, SlotInfo, [], SlotInfo).
push_into_goals_rename([Goal0 | Goals0], Construct, Var, SlotInfo0,
		[Goal | Goals], SlotInfo) :-
	push_into_goal_rename(Goal0, Construct, Var, SlotInfo0,
		Goal, SlotInfo1),
	push_into_goals_rename(Goals0, Construct, Var, SlotInfo1,
		Goals, SlotInfo).

% Push renamed versions of the given construction into each of several cases.

:- pred push_into_cases_rename(list(case), hlds_goal, prog_var, slot_info,
	list(case), slot_info).
:- mode push_into_cases_rename(in, in, in, in, out, out) is det.

push_into_cases_rename([], _Construct, _Var, SlotInfo, [], SlotInfo).
push_into_cases_rename([case(ConsId, Goal0) | Cases0], Construct, Var,
		SlotInfo0, [case(ConsId, Goal) | Cases], SlotInfo) :-
	push_into_goal_rename(Goal0, Construct, Var, SlotInfo0,
		Goal, SlotInfo1),
	push_into_cases_rename(Cases0, Construct, Var, SlotInfo1,
		Cases, SlotInfo).

%-----------------------------------------------------------------------------%

	% saved_vars_in_disj does a saved_vars_in_goal on an list of independent
	% goals, and is used to process disjunctions and parallel conjunctions.

:- pred saved_vars_in_disj(list(hlds_goal), slot_info,
	list(hlds_goal), slot_info).
:- mode saved_vars_in_disj(in, in, out, out) is det.

saved_vars_in_disj([], SlotInfo, [], SlotInfo).
saved_vars_in_disj([Goal0 | Goals0], SlotInfo0, [Goal | Goals], SlotInfo) :-
	saved_vars_in_goal(Goal0, SlotInfo0, Goal, SlotInfo1),
	saved_vars_in_disj(Goals0, SlotInfo1, Goals, SlotInfo).

:- pred saved_vars_in_switch(list(case), slot_info,
				     list(case), slot_info).
:- mode saved_vars_in_switch(in, in, out, out) is det.

saved_vars_in_switch([], SlotInfo, [], SlotInfo).
saved_vars_in_switch([case(Cons, Goal0) | Cases0], SlotInfo0,
		[case(Cons, Goal) | Cases], SlotInfo) :-
	saved_vars_in_goal(Goal0, SlotInfo0, Goal, SlotInfo1),
	saved_vars_in_switch(Cases0, SlotInfo1, Cases, SlotInfo).

%-----------------------------------------------------------------------------%

:- type slot_info	--->	slot_info(
					prog_varset,
					map(prog_var, type)
				).

:- pred init_slot_info(prog_varset, map(prog_var, type), slot_info).
:- mode init_slot_info(in, in, out) is det.

init_slot_info(Varset, VarTypes, slot_info(Varset, VarTypes)).

:- pred final_slot_info(prog_varset, map(prog_var, type), slot_info).
:- mode final_slot_info(out, out, in) is det.

final_slot_info(Varset, VarTypes, slot_info(Varset, VarTypes)).

:- pred rename_var(slot_info, prog_var, prog_var, map(prog_var, prog_var),
		slot_info).
:- mode rename_var(in, in, out, out, out) is det.

rename_var(SlotInfo0, Var, NewVar, Substitution, SlotInfo) :-
	SlotInfo0 = slot_info(Varset0, VarTypes0),
	varset__new_var(Varset0, NewVar, Varset),
	map__from_assoc_list([Var - NewVar], Substitution),
	map__lookup(VarTypes0, Var, Type),
	map__det_insert(VarTypes0, NewVar, Type, VarTypes),
	SlotInfo = slot_info(Varset, VarTypes).

%-----------------------------------------------------------------------------%
