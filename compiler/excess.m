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
% Note that the output of this pass is not in super-homogeneous form,
% since if two equivalent variables are passed in two argument positions,
% they will not be distinct after this pass. This is not a problem, since
% this pass occurs after the passes that rely on super-homogeneous form.
%
% This HLDS-to-HLDS optimization is applied after the front end has
% completed all of its semantic checks (i.e. after determinism analysis),
% but before code generation.
% 
% It allows optimizations such as middle recursion to be simplified, 
% and it reduces the pressure on the stack slot allocator.

%-----------------------------------------------------------------------------%

:- module excess.

:- interface.

:- import_module hlds_module, hlds_pred.

	% optimize away excess assignments for a single procedure
:- pred excess_assignments_proc(proc_info, module_info, proc_info).
% :- mode excess_assignments_proc(di, in, uo) is det.
:- mode excess_assignments_proc(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, goal_util.
:- import_module varset, list, bool, map, set, std_util, term.

%-----------------------------------------------------------------------------%

excess_assignments_proc(ProcInfo0, _ModuleInfo, ProcInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	excess_assignments_in_goal(Goal0, [], Goal, ElimVars),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),

	% XXX We probably ought to remove these vars from the type map as well.
	proc_info_variables(ProcInfo1, Varset0),
	varset__delete_vars(Varset0, ElimVars, Varset),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo).

%-----------------------------------------------------------------------------%

% We want to replace code sequences of the form
%
%	(
%		<Foo>,
%		LocalVar = OtherVar,
%		<Bar>
%	)
%
% with
%
%	(
%		<Foo> [LocalVar/OtherVar],
%		<Bar> [LocalVar/OtherVar],
%	)
%
% where <Foo> and <Bar> are sequences of conjuncts,
% LocalVar is a variable that is local to the conjuncts,
% and the notation `<Foo> [X/Y]' means <Foo> with all
% occurrences of `X' replaced with `Y'.

:- pred excess_assignments_in_goal(hlds_goal, list(var),
				   hlds_goal, list(var)).
:- mode excess_assignments_in_goal(in, in, out, out) is det.

excess_assignments_in_goal(GoalExpr0 - GoalInfo0, ElimVars0, Goal, ElimVars) :-
	(
		GoalExpr0 = conj(Goals0),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		excess_assignments_in_conj(Goals0, [], ElimVars0, NonLocals,
					Goals, ElimVars),
		conj_list_to_goal(Goals, GoalInfo0, Goal)
	;
		GoalExpr0 = disj(Goals0, SM),
		excess_assignments_in_disj(Goals0, ElimVars0, Goals, ElimVars),
		Goal = disj(Goals, SM) - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		excess_assignments_in_goal(NegGoal0, ElimVars0,
						NegGoal, ElimVars),
		Goal = not(NegGoal) - GoalInfo0
	;
		GoalExpr0 = switch(Var, CanFail, Cases0, SM),
		excess_assignments_in_switch(Cases0, ElimVars0,
						Cases, ElimVars),
		Goal = switch(Var, CanFail, Cases, SM) - GoalInfo0
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0, SM),
		excess_assignments_in_goal(Cond0, ElimVars0, Cond, ElimVars1),
		excess_assignments_in_goal(Then0, ElimVars1, Then, ElimVars2),
		excess_assignments_in_goal(Else0, ElimVars2, Else, ElimVars),
		Goal = if_then_else(Vars, Cond, Then, Else, SM) - GoalInfo0
	;
		GoalExpr0 = some(Var, SubGoal0),
		excess_assignments_in_goal(SubGoal0, ElimVars0,
					   SubGoal, ElimVars),
		Goal = some(Var, SubGoal) - GoalInfo0
	;
		GoalExpr0 = higher_order_call(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = ElimVars0
	;
		GoalExpr0 = call(_, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = ElimVars0
	;
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = ElimVars0
	;
		GoalExpr0 = pragma_c_code(_, _, _, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		ElimVars = ElimVars0
	),
	!.

%-----------------------------------------------------------------------------%

	% We apply each substitution as soon as we find the need for it.
	% This is to handle code which has V_4 = V_5, V_5 = V_6. If at most
	% one of these variables is nonlocal, we can eliminate both assignments.
	% If (say) V_4 and V_6 are nonlocal, then after the V_5 => V_4
	% substitution has been made, the second assignment V_4 = V_6
	% is left alone.

:- pred excess_assignments_in_conj(list(hlds_goal), list(hlds_goal),
	list(var), set(var), list(hlds_goal), list(var)).
:- mode excess_assignments_in_conj(in, in, in, in, out, out) is det.

excess_assignments_in_conj([], RevGoals, ElimVars, _, Goals, ElimVars) :-
	list__reverse(RevGoals, Goals).
excess_assignments_in_conj([Goal0 | Goals0], RevGoals0, ElimVars0, NonLocals,
		Goals, ElimVars) :-
	(
		Goal0 = unify(_, _, _, Unif, _) - _,
		Unif = assign(LeftVar, RightVar),
		( \+ set__member(LeftVar, NonLocals) ->
			LocalVar = LeftVar, ReplacementVar = RightVar
		; \+ set__member(RightVar, NonLocals) ->
			LocalVar = RightVar, ReplacementVar = LeftVar
		;
			fail
		)
	->
		map__init(Subn0),
		map__det_insert(Subn0, LocalVar, ReplacementVar, Subn),
		goal_util__rename_vars_in_goals(Goals0, no, Subn, Goals1),
		goal_util__rename_vars_in_goals(RevGoals0, no, Subn, RevGoals1),
		ElimVars1 = [LocalVar | ElimVars0]
	;
		Goals1 = Goals0,
		excess_assignments_in_goal(Goal0, ElimVars0, Goal1, ElimVars1),
		RevGoals1 = [Goal1 | RevGoals0]
	),
	excess_assignments_in_conj(Goals1, RevGoals1, ElimVars1,
		NonLocals, Goals, ElimVars).

%-----------------------------------------------------------------------------%

:- pred excess_assignments_in_disj(list(hlds_goal), list(var),
	list(hlds_goal), list(var)).
:- mode excess_assignments_in_disj(in, in, out, out) is det.

excess_assignments_in_disj([], ElimVars, [], ElimVars).
excess_assignments_in_disj([Goal0 | Goals0], ElimVars0,
			   [Goal | Goals], ElimVars) :-
	excess_assignments_in_goal(Goal0, ElimVars0, Goal, ElimVars1),
	excess_assignments_in_disj(Goals0, ElimVars1, Goals, ElimVars).

:- pred excess_assignments_in_switch(list(case), list(var),
				     list(case), list(var)).
:- mode excess_assignments_in_switch(in, in, out, out) is det.

excess_assignments_in_switch([], ElimVars, [], ElimVars).
excess_assignments_in_switch([case(Cons, Goal0) | Cases0], ElimVars0,
		[case(Cons, Goal) | Cases], ElimVars) :-
	excess_assignments_in_goal(Goal0, ElimVars0, Goal, ElimVars1),
	excess_assignments_in_switch(Cases0, ElimVars1, Cases, ElimVars).

%-----------------------------------------------------------------------------%
