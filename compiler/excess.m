%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: zs.

% This module traverses the goal for each procedure, and removes any
% assignments that effectively just rename variables. This optimization
% allows middle recursion optimization to be simplified, and it reduces
% the pressure on the stack slot allocator.

%-----------------------------------------------------------------------------%

:- module excess.

:- interface.

:- import_module hlds, llds.

:- pred excess_assignments(module_info, module_info).
:- mode excess_assignments(in, out) is det.

:- pred excess_assignments_proc(proc_info, module_info, proc_info).
% :- mode excess_assignments_proc(di, in, uo) is det.
:- mode excess_assignments_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module goal_util.
:- import_module varset, list, map, set, std_util.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `excess_assignments_in_goal'
	% for each procedure body.

excess_assignments(ModuleInfo0, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	excess_assignments_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred excess_assignments_in_preds(list(pred_id), module_info, module_info).
:- mode excess_assignments_in_preds(in, in, out) is det.

excess_assignments_in_preds([], ModuleInfo, ModuleInfo).
excess_assignments_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, ProcIds),
	excess_assignments_in_procs(ProcIds, PredId, ModuleInfo0, ModuleInfo1),
	excess_assignments_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred excess_assignments_in_procs(list(proc_id), pred_id,
	module_info, module_info).
:- mode excess_assignments_in_procs(in, in, in, out) is det.

excess_assignments_in_procs([], _PredId, ModuleInfo, ModuleInfo).
excess_assignments_in_procs([ProcId | ProcIds], PredId,
		ModuleInfo0, ModuleInfo) :-
	excess_assignments_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo1),
	excess_assignments_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

:- pred excess_assignments_in_proc(proc_id, pred_id, module_info, module_info).
:- mode excess_assignments_in_proc(in, in, in, out) is det.

excess_assignments_in_proc(ProcId, PredId, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	excess_assignments_proc(ProcInfo0, ModuleInfo0, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo).

excess_assignments_proc(ProcInfo0, _ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	excess_assignments_in_goal(Goal0, ElimVars, Goal),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),

	% XXX We probably ought to remove these vars from the type map as well.
	proc_info_variables(ProcInfo1, Varset0),
	varset__delete_vars(Varset0, ElimVars, Varset),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred excess_assignments_in_goal(hlds__goal, list(var), hlds__goal).
:- mode excess_assignments_in_goal(in, out, out) is det.

excess_assignments_in_goal(GoalExpr0 - GoalInfo0, ElimVars, Goal) :-
	(
		GoalExpr0 = conj(Goals0),
		goal_info_get_nonlocals(GoalInfo0, NonLocal),
		excess_assignments_in_conj(Goals0, [], NonLocal,
			ElimVars, Goals),
		( Goals = [OneGoal] ->
			Goal = OneGoal
		;
			Goal = conj(Goals) - GoalInfo0
		)
	;
		GoalExpr0 = disj(Goals0),
		excess_assignments_in_disj(Goals0, ElimVars, Goals),
		Goal = disj(Goals) - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		excess_assignments_in_goal(NegGoal0, ElimVars, NegGoal),
		Goal = not(NegGoal) - GoalInfo0
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		excess_assignments_in_switch(Cases0, ElimVars, Cases),
		Goal = switch(Var, CanFail, Cases) - GoalInfo0
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
		excess_assignments_in_goal(Cond0, ElimVars1, Cond),
		excess_assignments_in_goal(Then0, ElimVars2, Then),
		excess_assignments_in_goal(Else0, ElimVars3, Else),
		list__append(ElimVars2, ElimVars3, ElimVars23),
		list__append(ElimVars1, ElimVars23, ElimVars),
		Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo0
	;
		GoalExpr0 = some(Var, SubGoal0),
		excess_assignments_in_goal(SubGoal0, ElimVars, SubGoal),
		Goal = some(Var, SubGoal) - GoalInfo0
	;
		GoalExpr0 = call(_, _, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = []
	;
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = []
	;
		GoalExpr0 = pragma_c_code(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = []
	).

%-----------------------------------------------------------------------------%

	% We apply each subsitutions as soon as we find the need for it.
	% This us to handle code which has V_4 = V_5, V_5 = V_6. If at most
	% one of these variables is nonlocal, we can eliminate both assignments.
	% If (say) V_4 and V_6 are nonlocal, then after the V_5 => V_4
	% substitution has been made, the second assignment V_4 = V_6
	% is left alone.

:- pred excess_assignments_in_conj(list(hlds__goal), list(hlds__goal),
	set(var), list(var), list(hlds__goal)).
:- mode excess_assignments_in_conj(in, in, in, out, out) is det.

excess_assignments_in_conj([], RevGoals, _, [], Goals) :-
	list__reverse(RevGoals, Goals).
excess_assignments_in_conj([Goal0 | Goals0], RevGoals0, NonLocals,
		ElimVars, Goals) :-
	(
		Goal0 = unify(_, _, _, Unif, _) - _,
		Unif = assign(Lvar, Rvar),
		set__is_member(Lvar, NonLocals, Lnl),
		set__is_member(Rvar, NonLocals, Rnl),
		(
			Lnl = yes, Rnl = no,
			From = Rvar, To = Lvar
		;
			Lnl = no, Rnl = yes,
			From = Lvar, To = Rvar
		;
			Lnl = no, Rnl = no,
			From = Lvar, To = Rvar
		)
	->
		map__init(Subn0),
		map__set(Subn0, From, To, Subn),
		goal_util__rename_vars_in_goals(Goals0, Subn, Goals1),
		goal_util__rename_vars_in_goals(RevGoals0, Subn, RevGoals1),
		excess_assignments_in_conj(Goals1, RevGoals1, NonLocals,
			ElimVars1, Goals),
		ElimVars = [From | ElimVars1]
	;
		Goals1 = Goals0,
		excess_assignments_in_goal(Goal0, ElimVars1, Goal1),
		RevGoals1 = [Goal1 | RevGoals0],
		excess_assignments_in_conj(Goals1, RevGoals1, NonLocals,
			ElimVars2, Goals),
		list__append(ElimVars1, ElimVars2, ElimVars)
	).

%-----------------------------------------------------------------------------%

:- pred excess_assignments_in_disj(list(hlds__goal), list(var),
	list(hlds__goal)).
:- mode excess_assignments_in_disj(in, out, out) is det.

excess_assignments_in_disj([], [], []).
excess_assignments_in_disj([Goal0 | Goals0], ElimVars, [Goal | Goals]) :-
	excess_assignments_in_goal(Goal0, ElimVars1, Goal),
	excess_assignments_in_disj(Goals0, ElimVars2, Goals),
	list__append(ElimVars1, ElimVars2, ElimVars).

:- pred excess_assignments_in_switch(list(case), list(var), list(case)).
:- mode excess_assignments_in_switch(in, out, out) is det.

excess_assignments_in_switch([], [], []).
excess_assignments_in_switch([case(Cons, Goal0) | Cases0], ElimVars,
		[case(Cons, Goal) | Cases]) :-
	excess_assignments_in_goal(Goal0, ElimVars1, Goal),
	excess_assignments_in_switch(Cases0, ElimVars2, Cases),
	list__append(ElimVars1, ElimVars2, ElimVars).

%-----------------------------------------------------------------------------%
