%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: zs.

% This module traverses the goal for each procedure, looking
% for conjunctions containing assignment unifications to or from
% a variable that is local to the conjunction.  Such unifications
% effectively just introduce a new local name for a variable.
% This module optimizes away such unifications by replacing all
% occurrences of the local name with the other name.
%
% This HLDS-to-HLDS optimization is applied after the front end has
% completed all of its semantic checks (i.e. after determinism analysis),
% but before code generation.
% 
% It allows middle recursion optimization to be simplified, 
% and it reduces the pressure on the stack slot allocator.

%-----------------------------------------------------------------------------%

:- module excess.

:- interface.

:- import_module hlds, llds.

	% optimize away excess assignments for a whol module
:- pred excess_assignments(module_info, module_info).
:- mode excess_assignments(in, out) is det.

	% optimize away excess assignments for a single procedure
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
	map__init(Subn0),
	excess_assignments_in_goal(Goal0, Subn0, Goal1, Subn),
	( map__is_empty(Subn) ->
		Goal = Goal1
	;
		goal_util__rename_vars_in_goal(Goal1, Subn, Goal)
	),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),

	map__keys(Subn, ElimVars),

	% XXX We probably ought to remove these vars from the type map as well.
	proc_info_variables(ProcInfo1, Varset0),
	varset__delete_vars(Varset0, ElimVars, Varset),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% We want to replace code sequences of the form
%
%	(  <Foo>,
%	   LocalVar = OtherVar,
%	   <Bar>
%	)
%
% with
%	( <Foo> [LocalVar/OtherVar],
%	  <Bar> [LocalVar/OtherVar],
%	)
%
% where <Foo> and <Bar> are sequences of conjuncts,
% LocalVar is a variable that is local to the conjuncts,
% and the notation `<Foo> [X/Y]' means <Foo> with all
% occurrences of `X' replaced with `Y'.

:- pred excess_assignments_in_goal(hlds__goal, map(var,var),
				   hlds__goal, map(var,var)).
:- mode excess_assignments_in_goal(in, in, out, out) is det.

excess_assignments_in_goal(GoalExpr0 - GoalInfo0, Subn0, Goal, Subn) :-
	(
		GoalExpr0 = conj(Goals0),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		excess_assignments_in_conj(Goals0, [], Subn0, NonLocals,
					Goals, Subn),
		conj_list_to_goal(Goals, GoalInfo0, Goal)
	;
		GoalExpr0 = disj(Goals0),
		excess_assignments_in_disj(Goals0, Subn0, Goals, Subn),
		Goal = disj(Goals) - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		excess_assignments_in_goal(NegGoal0, Subn0,
						NegGoal, Subn),
		Goal = not(NegGoal) - GoalInfo0
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		excess_assignments_in_switch(Cases0, Subn0,
						Cases, Subn),
		Goal = switch(Var, CanFail, Cases) - GoalInfo0
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
		excess_assignments_in_goal(Cond0, Subn0, Cond, Subn1),
		excess_assignments_in_goal(Then0, Subn1, Then, Subn2),
		excess_assignments_in_goal(Else0, Subn2, Else, Subn),
		Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo0
	;
		GoalExpr0 = some(Var, SubGoal0),
		excess_assignments_in_goal(SubGoal0, Subn0,
					   SubGoal, Subn),
		Goal = some(Var, SubGoal) - GoalInfo0
	;
		GoalExpr0 = call(_, _, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		Subn = Subn0
	;
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		Subn = Subn0
	;
		GoalExpr0 = pragma_c_code(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		Subn = Subn0
	).

%-----------------------------------------------------------------------------%

	% We apply each substitution as soon as we find the need for it.
	% This us to handle code which has V_4 = V_5, V_5 = V_6. If at most
	% one of these variables is nonlocal, we can eliminate both assignments.
	% If (say) V_4 and V_6 are nonlocal, then after the V_5 => V_4
	% substitution has been made, the second assignment V_4 = V_6
	% is left alone.

:- pred excess_assignments_in_conj(list(hlds__goal), list(hlds__goal),
	map(var,var), set(var), list(hlds__goal), map(var,var)).
:- mode excess_assignments_in_conj(in, in, in, in, out, out) is det.

excess_assignments_in_conj([], RevGoals, Subn, _, Goals, Subn) :-
	list__reverse(RevGoals, Goals).
excess_assignments_in_conj([Goal0 | Goals0], RevGoals0, Subn0, NonLocals,
		Goals, Subn) :-
	(
		Goal0 = unify(_, _, _, Unif, _) - _,
		Unif = assign(LeftVar0, RightVar0),
		goal_util__rename_var(LeftVar0, Subn0, LeftVar),
		goal_util__rename_var(RightVar0, Subn0, RightVar),
		( \+ set__member(LeftVar, NonLocals) ->
			LocalVar = LeftVar, ReplacementVar = RightVar
		; \+ set__member(RightVar, NonLocals) ->
			LocalVar = RightVar, ReplacementVar = LeftVar
		;
			fail
		)
	->
		map__det_insert(Subn0, LocalVar, ReplacementVar, Subn1),
		RevGoals1 = RevGoals0
	;
		excess_assignments_in_goal(Goal0, Subn0, Goal1, Subn1),
		RevGoals1 = [Goal1 | RevGoals0]
	),
	excess_assignments_in_conj(Goals0, RevGoals1, Subn1,
		NonLocals, Goals, Subn).

%-----------------------------------------------------------------------------%

:- pred excess_assignments_in_disj(list(hlds__goal), map(var,var),
	list(hlds__goal), map(var,var)).
:- mode excess_assignments_in_disj(in, in, out, out) is det.

excess_assignments_in_disj([], Subn, [], Subn).
excess_assignments_in_disj([Goal0 | Goals0], Subn0, [Goal | Goals], Subn) :-
	excess_assignments_in_goal(Goal0, Subn0, Goal, Subn1),
	excess_assignments_in_disj(Goals0, Subn1, Goals, Subn).

:- pred excess_assignments_in_switch(list(case), map(var,var),
				     list(case), map(var,var)).
:- mode excess_assignments_in_switch(in, in, out, out) is det.

excess_assignments_in_switch([], Subn, [], Subn).
excess_assignments_in_switch([case(Cons, Goal0) | Cases0], Subn0,
		[case(Cons, Goal) | Cases], Subn) :-
	excess_assignments_in_goal(Goal0, Subn0, Goal, Subn1),
	excess_assignments_in_switch(Cases0, Subn1, Cases, Subn).

%-----------------------------------------------------------------------------%
