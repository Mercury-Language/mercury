%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: delay_construct.m
%
% Author: zs.
%
% This module transforms sequences of goals in procedure bodies.
% It looks for a unification that constructs a ground term followed by
% primitive goals, at least one of which can fail, and none of which take
% the variable representing the cell as their input. Such code sequences
% cause the cell to be constructed even if the following goal would fail,
% which is wasteful. This module therefore reorders the sequence, moving the
% construction unification past all the semidet primitives it can.
%
% The reason we don't move the construction past calls or composite goals
% is that this may require storing the input arguments of the construction on
% the stack, which may cause a slowdown bigger than the speedup available from
% not having to construct the cell on some execution paths.

%-----------------------------------------------------------------------------%

:- module transform_hlds__delay_construct.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module io.

:- pred delay_construct_proc(pred_id::in, proc_id::in, module_info::in,
	proc_info::in, proc_info::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, hlds__hlds_data, hlds__hlds_goal.
:- import_module hlds__passes_aux, libs__globals.
:- import_module hlds__instmap, check_hlds__inst_match.
:- import_module bool, list, set, std_util, require.

%-----------------------------------------------------------------------------%

delay_construct_proc(PredId, ProcId, ModuleInfo, ProcInfo0, ProcInfo) -->
	write_proc_progress_message("% Delaying construction unifications in ",
		PredId, ProcId, ModuleInfo),
	globals__io_get_globals(Globals),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ delay_construct_proc_no_io(ProcInfo0, PredInfo, ModuleInfo, Globals,
		ProcInfo) }.

:- pred delay_construct_proc_no_io(proc_info::in, pred_info::in,
	module_info::in, globals::in, proc_info::out) is det.

delay_construct_proc_no_io(ProcInfo0, PredInfo, ModuleInfo, Globals, ProcInfo)
		:-
	body_should_use_typeinfo_liveness(PredInfo, Globals,
		BodyTypeinfoLiveness),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_typeinfo_varmap(ProcInfo0, TypeInfoVarMap),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo, InstMap0),
	DelayInfo = delay_construct_info(ModuleInfo, BodyTypeinfoLiveness,
		VarTypes, TypeInfoVarMap),
	proc_info_goal(ProcInfo0, Goal0),
	delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo).

:- type delay_construct_info
	--->	delay_construct_info(
			module_info		:: module_info,
			body_typeinfo_liveness	:: bool,
			vartypes		:: vartypes,
			type_info_varmap	:: type_info_varmap
		).

%-----------------------------------------------------------------------------%

:- pred delay_construct_in_goal(hlds_goal::in, instmap::in,
	delay_construct_info::in, hlds_goal::out) is det.

delay_construct_in_goal(GoalExpr0 - GoalInfo0, InstMap0, DelayInfo, Goal) :-
	(
		GoalExpr0 = conj(Goals0),
		goal_info_get_determinism(GoalInfo0, Detism),
		determinism_components(Detism, CanFail, MaxSoln),
		(
			% If the conjunction cannot fail, then its conjuncts
			% cannot fail either, so we have no hope of pushing a
			% construction past a failing goal.
			%
			% If the conjuntion contains goals that can succeed
			% more than once, which is possible if MaxSoln is
			% at_most_many or at_most_many_cc, then moving a
			% construction to the right may increase the number of
			% times the construction is executed. We are therefore
			% careful to make sure delay_construct_in_conj doesn't
			% move constructions across goals that succeed more
			% than once.
			%
			% If the conjunction cannot succeed, i.e. MaxSoln is
			% at_most_zero, there is no point in trying to speed it
			% up.

			CanFail = can_fail,
			MaxSoln \= at_most_zero
		->
			delay_construct_in_conj(Goals0, InstMap0, DelayInfo,
				set__init, [], Goals1)
		;
			Goals1 = Goals0
		),
		delay_construct_in_goals(Goals1, InstMap0, DelayInfo, Goals),
		Goal = conj(Goals) - GoalInfo0
	;
		GoalExpr0 = par_conj(Goals0),
		delay_construct_in_goals(Goals0, InstMap0, DelayInfo, Goals),
		Goal = par_conj(Goals) - GoalInfo0
	;
		GoalExpr0 = disj(Goals0),
		delay_construct_in_goals(Goals0, InstMap0, DelayInfo, Goals),
		Goal = disj(Goals) - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		delay_construct_in_goal(NegGoal0, InstMap0, DelayInfo, NegGoal),
		Goal = not(NegGoal) - GoalInfo0
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		delay_construct_in_cases(Cases0, InstMap0, DelayInfo, Cases),
		Goal = switch(Var, CanFail, Cases) - GoalInfo0
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
		Cond0 = _ - CondInfo0,
		goal_info_get_instmap_delta(CondInfo0, CondInstMapDelta),
		instmap__apply_instmap_delta(InstMap0, CondInstMapDelta,
			InstMapThen),
		delay_construct_in_goal(Cond0, InstMap0, DelayInfo, Cond),
		delay_construct_in_goal(Then0, InstMapThen, DelayInfo, Then),
		delay_construct_in_goal(Else0, InstMap0, DelayInfo, Else),
		Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo0
	;
		GoalExpr0 = some(Var, CanRemove, SubGoal0),
		delay_construct_in_goal(SubGoal0, InstMap0, DelayInfo, SubGoal),
		Goal = some(Var, CanRemove, SubGoal) - GoalInfo0
	;
		GoalExpr0 = generic_call(_, _, _, _),
		Goal = GoalExpr0 - GoalInfo0
	;
		GoalExpr0 = call(_, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0
	;
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0
	;
		GoalExpr0 = foreign_proc(_, _, _, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0
	;
		GoalExpr0 = shorthand(_),
		% these should have been expanded out by now
		error("delay_construct_in_goal: unexpected shorthand")
	).

%-----------------------------------------------------------------------------%

% We maintain a list of delayed construction unifications that construct ground
% terms, and the set of variables they define.
%
% When we find other construction unifications, we add them to the list.
% It does not matter if they depend on other delayed construction unifications;
% when we put them back into the conjunction, we do so in the original order.
%
% There are several reasons why we may not be able to delay a construction
% unification past a conjunct. The conjunct may not be a primitive goal,
% or it may be impure; in either case, we must insert all the delayed
% construction unifications before it. The conjunct may also require the value
% of a variable defined by a construction unification. In such cases, we could
% drop before that goal only the construction unifications that define the
% variables needed by the conjunct, either directly or indirectly through 
% the values required by some of those construction unifications. However,
% separating out this set of delayed constructions from the others would
% require somewhat complex code, and it is not clear that there would be any
% significant benefit. We therefore insert *all* the delayed constructions
% before a goal if the goal requires *any* of the variables bound by the
% constructions.
%
% The instmap we pass around is the one that we construct from the original
% conjunction order. At each point, it reflects the bindings made by the
% conjuncts so far *plus* the bindings made by the delayed goals.

:- pred delay_construct_in_conj(list(hlds_goal)::in, instmap::in,
	delay_construct_info::in, set(prog_var)::in, list(hlds_goal)::in,
	list(hlds_goal)::out) is det.

delay_construct_in_conj([], _, _, _, RevDelayedGoals, DelayedGoals) :-
	list__reverse(RevDelayedGoals, DelayedGoals).
delay_construct_in_conj([Goal0 | Goals0], InstMap0, DelayInfo,
		ConstructedVars0, RevDelayedGoals0, Goals) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1),
	(
		GoalExpr0 = unify(_, _, _, Unif, _),
		Unif = construct(Var, _, Args, _, _, _, _),
		Args = [_ | _],	% We are constructing a cell, not a constant
		instmap__lookup_var(InstMap0, Var, Inst0),
		inst_is_free(DelayInfo ^ module_info, Inst0),
		instmap__lookup_var(InstMap1, Var, Inst1),
		inst_is_ground(DelayInfo ^ module_info, Inst1)
	->
		set__insert(ConstructedVars0, Var, ConstructedVars1),
		RevDelayedGoals1 = [Goal0 | RevDelayedGoals0],
		delay_construct_in_conj(Goals0, InstMap1, DelayInfo,
			ConstructedVars1, RevDelayedGoals1, Goals)
	;
		Goal0 = GoalExpr0 - GoalInfo0,
		delay_construct_skippable(GoalExpr0, GoalInfo0),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		proc_info_maybe_complete_with_typeinfo_vars(NonLocals,
			DelayInfo ^ body_typeinfo_liveness,
			DelayInfo ^ vartypes,
			DelayInfo ^ type_info_varmap, CompletedNonLocals),
		set__intersect(CompletedNonLocals, ConstructedVars0,
			Intersection),
		set__empty(Intersection),
		\+ goal_info_has_feature(GoalInfo0, impure),
		\+ goal_info_has_feature(GoalInfo0, semipure)
	->
		delay_construct_in_conj(Goals0, InstMap1, DelayInfo,
			ConstructedVars0, RevDelayedGoals0, Goals1),
		Goals = [Goal0 | Goals1]
	;
		list__reverse(RevDelayedGoals0, DelayedGoals),
		delay_construct_in_conj(Goals0, InstMap1, DelayInfo,
			set__init, [], Goals1),
		list__append(DelayedGoals, [Goal0 | Goals1], Goals)
	).

:- pred delay_construct_skippable(hlds_goal_expr::in, hlds_goal_info::in)
	is semidet.

delay_construct_skippable(GoalExpr, GoalInfo) :-
	(
		GoalExpr = unify(_, _, _, _, _)
	;
		GoalExpr = call(_, _, _, inline_builtin, _, _)
	),
	goal_info_get_determinism(GoalInfo, Detism),
	determinism_components(Detism, _CanFail, MaxSoln),
	MaxSoln \= at_most_many.

%-----------------------------------------------------------------------------%

:- pred delay_construct_in_goals(list(hlds_goal)::in, instmap::in,
	delay_construct_info::in, list(hlds_goal)::out) is det.

delay_construct_in_goals([], _, _, []).
delay_construct_in_goals([Goal0 | Goals0], InstMap0, DelayInfo,
		[Goal | Goals]) :-
	delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal),
	delay_construct_in_goals(Goals0, InstMap0, DelayInfo, Goals).

:- pred delay_construct_in_cases(list(case)::in, instmap::in,
	delay_construct_info::in, list(case)::out) is det.

delay_construct_in_cases([], _, _, []).
delay_construct_in_cases([case(Cons, Goal0) | Cases0], InstMap0, DelayInfo,
		[case(Cons, Goal) | Cases]) :-
	delay_construct_in_goal(Goal0, InstMap0, DelayInfo, Goal),
	delay_construct_in_cases(Cases0, InstMap0, DelayInfo, Cases).

%-----------------------------------------------------------------------------%
