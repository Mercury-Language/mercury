%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: delay_info.m.
% Main author: fjh.

% This module implements part of the mode analysis algorithm.
% In the mode analysis, reordering of conjunctions is done
% by simulating coroutining at compile time.  This module
% defines an abstract data type `delay_info' which records the
% information necessary for suspending and waking up goals.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module delay_info.

:- interface.

:- import_module hlds_goal, mode_errors.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type delay_info.

%-----------------------------------------------------------------------------%

	% Sanity-check the delay_info structure.
	%
:- pred delay_info__check_invariant(delay_info).
:- mode delay_info__check_invariant(in) is det.

	% Initialize the delay_info structure.
	%
:- pred delay_info__init(delay_info).
:- mode delay_info__init(out) is det.

	% Tell the delay_info structure that we've entered a new conjunction.
	%
:- pred delay_info__enter_conj(delay_info, delay_info).
:- mode delay_info__enter_conj(in, out) is det.

	% Tell the delay_info structure that we've left a conjunction.
	% This predicate returns a list of the delayed goals from that
	% conjunction, i.e. goals which could not be scheduled.
	%
:- pred delay_info__leave_conj(delay_info, list(delayed_goal), delay_info).
:- mode delay_info__leave_conj(in, out, out) is det.

	% Insert a new delayed goal into the delay_info structure.
	%
:- pred delay_info__delay_goal(delay_info, mode_error_info,
				hlds_goal, delay_info).
:- mode delay_info__delay_goal(in, in, in, out) is det.

	% Mark a list of variables as having been bound.
	% This may allow a previously delayed goal to change status
	% from "delayed" to "pending".
	% (This predicate just calls delay_info__bind_var in a loop.)
	%
:- pred delay_info__bind_var_list(list(var), delay_info, delay_info).
:- mode delay_info__bind_var_list(in, in, out) is det.

	% Mark a variable as having been bound.
	% This may allow a previously delayed goal to change status
	% from "delayed" to "pending".
	%
:- pred delay_info__bind_var(delay_info, var, delay_info).
:- mode delay_info__bind_var(in, in, out) is det.

	% Mark all variables as having been bound.
	% This will allow all previously delayed goals to change status
	% from "delayed" to "pending".
	%
:- pred delay_info__bind_all_vars(delay_info, delay_info).
:- mode delay_info__bind_all_vars(in, out) is det.

	% Check if there are any "pending" goals, and if so,
	% remove them from the delay_info and return them.
	%
:- pred delay_info__wakeup_goals(delay_info, list(hlds_goal), delay_info).
:- mode delay_info__wakeup_goals(in, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, stack, set, map, require, std_util.
:- import_module mode_errors.	% for the mode_error_info and delay_info
				% types.

	% The delay_info structure is a tangled web of substructures
	% all of which are pointing at each other - debugging it
	% reminds me of debugging C code - it's a nightmare!
	% (Of course, it could just be that my brain has decided to
	% switch off in protest - after all, it is 5:25am. ;-)

:- type delay_info
	--->	delay_info(
			depth_num,	% CurrentDepth:
					% the current conjunction depth,
					% i.e. the number of nested conjunctions
					% which are currently active
			stack(map(seq_num, delayed_goal)),
					% DelayedGoalStack:
					% for each nested conjunction,
					% we store a collection of delayed goals
					% associated with that conjunction,
					% indexed by sequence number
			waiting_goals_table,
					% WaitingGoalsTable:
					% for each variable, we keep track of
					% all the goals which are waiting on
					% that variable
			pending_goals_table,
					% PendingGoalsTable:
					% when a variable gets bound, we
					% mark all the goals which are waiting
					% on that variable as ready to be
					% reawakened at the next opportunity
			stack(seq_num)
					% SeqNumsStack:
					% For each nested conjunction, the
					% next available sequence number.
		).

:- type waiting_goals_table == map(var, waiting_goals).
	% Used to store the collection of goals waiting on a variable.
:- type waiting_goals == map(goal_num, list(var)).
	% For each goal, we store all the variables that it is waiting on.

:- type pending_goals_table == map(depth_num, list(seq_num)).

:- type goal_num == pair(depth_num, seq_num).
:- type depth_num == int.		/* Eeek! Pointers! */
:- type seq_num == int.

%-----------------------------------------------------------------------------%

	% Check that the invariants for the delay_info structure
	% hold, and if not, call error/1.

delay_info__check_invariant(_).
/***
	% for debugging purposes
delay_info__check_invariant(DelayInfo) :-
	delay_info__check_invariant_x(DelayInfo).
***/

:- pred delay_info__check_invariant_x(delay_info).
:- mode delay_info__check_invariant_x(in) is det.

delay_info__check_invariant_x(DelayInfo) :-
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, _PendingGoals, NextSeqNums),
	(
		stack__depth(DelayedGoalStack, CurrentDepth),
		stack__depth(NextSeqNums, CurrentDepth),
		map__keys(WaitingGoalsTable, WaitingVars),
		waiting_goals_check_invariant(WaitingVars, WaitingGoalsTable)
	->
		true
	;
		error("delay_info: invariant violated")
	).


	% For every variable which goals are waiting on, check the
	% consistency of all the goals waiting on that var.

:- pred waiting_goals_check_invariant(list(var), waiting_goals_table).
:- mode waiting_goals_check_invariant(in, in) is semidet.

waiting_goals_check_invariant([], _).
waiting_goals_check_invariant([V|Vs], WaitingGoalsTable) :-
	map__lookup(WaitingGoalsTable, V, WaitingGoals),
	map__keys(WaitingGoals, GoalNums),
	waiting_goal_check_invariant(GoalNums, WaitingGoals, WaitingGoalsTable),
	waiting_goals_check_invariant(Vs, WaitingGoalsTable).

	% Check the consistency of a list of goal_nums in the
	% waiting_goals_table.

:- pred waiting_goal_check_invariant(list(goal_num), waiting_goals,
					waiting_goals_table).
:- mode waiting_goal_check_invariant(in, in, in) is semidet.

waiting_goal_check_invariant([], _, _).
waiting_goal_check_invariant([GoalNum | GoalNums], WaitingGoals,
		WaitingGoalsTable) :-
	map__lookup(WaitingGoals, GoalNum, Vars),
	set__list_to_set(Vars, VarsSet),
	waiting_goal_vars_check_invariant(Vars, GoalNum, VarsSet,
		WaitingGoalsTable),
	waiting_goal_check_invariant(GoalNums, WaitingGoals, WaitingGoalsTable).

	% For every variable which a goal is waiting on, there should
	% be an entry in the waiting_goals_table for that goal,
	% and the set of vars which it is waiting on in that entry
	% should be the same as in all its other entries.

:- pred waiting_goal_vars_check_invariant(list(var), goal_num, set(var),
					waiting_goals_table).
:- mode waiting_goal_vars_check_invariant(in, in, in, in) is semidet.

waiting_goal_vars_check_invariant([], _, _, _).
waiting_goal_vars_check_invariant([V|Vs], GoalNum, Vars, WaitingGoalsTable) :-
	map__search(WaitingGoalsTable, V, WaitingGoals),
	map__search(WaitingGoals, GoalNum, VarsList),
	set__list_to_set(VarsList, VarsSet),
	set__equal(Vars, VarsSet),
	waiting_goal_vars_check_invariant(Vs, GoalNum, Vars, WaitingGoalsTable).

%-----------------------------------------------------------------------------%

	% Initialize the delay info structure in preparation for
	% mode analysis of a goal.

delay_info__init(DelayInfo) :-
	CurrentDepth = 0,
	stack__init(DelayedGoalStack),
	map__init(WaitingGoalsTable),
	map__init(PendingGoals),
	stack__init(NextSeqNums),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums),
	delay_info__check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%

delay_info__enter_conj(DelayInfo0, DelayInfo) :-
	delay_info__check_invariant(DelayInfo0),
	DelayInfo0 = delay_info(CurrentDepth0, DelayedGoalStack0,
				WaitingGoalsTable, PendingGoals, NextSeqNums0),
	map__init(DelayedGoals),
	stack__push(DelayedGoalStack0, DelayedGoals, DelayedGoalStack),
	stack__push(NextSeqNums0, 0, NextSeqNums),
	CurrentDepth is CurrentDepth0 + 1,
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums),
	delay_info__check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%

delay_info__leave_conj(DelayInfo0, DelayedGoalsList, DelayInfo) :-
	delay_info__check_invariant(DelayInfo0),
	DelayInfo0 = delay_info(CurrentDepth0, DelayedGoalStack0,
				WaitingGoalsTable0, PendingGoals, NextSeqNums0),
	stack__pop_det(DelayedGoalStack0, DelayedGoals, DelayedGoalStack),
	map__keys(DelayedGoals, SeqNums),
	remove_delayed_goals(SeqNums, DelayedGoals, CurrentDepth0,
				WaitingGoalsTable0, WaitingGoalsTable),
	stack__pop_det(NextSeqNums0, _, NextSeqNums),
	CurrentDepth is CurrentDepth0 - 1,
	map__values(DelayedGoals, DelayedGoalsList),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums),
	delay_info__check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%

	% When a conjunction flounders, we need to remove the delayed sub-goals
	% from the waiting goals table before we delay the conjunction as a
	% whole.

:- pred remove_delayed_goals(list(seq_num), map(seq_num, delayed_goal),
			depth_num, waiting_goals_table, waiting_goals_table).
:- mode remove_delayed_goals(in, in, in, in, out) is det.

remove_delayed_goals([], _, _, WaitingGoalsTable, WaitingGoalsTable).
remove_delayed_goals([SeqNum | SeqNums], DelayedGoalsTable, Depth,
			WaitingGoalsTable0, WaitingGoalsTable) :-
	map__lookup(DelayedGoalsTable, SeqNum, DelayedGoal),
	DelayedGoal = delayed_goal(Vars, _Error, _Goal),
	GoalNum = Depth - SeqNum,
	set__to_sorted_list(Vars, VarList),
	delete_waiting_vars(VarList, GoalNum,
		WaitingGoalsTable0, WaitingGoalsTable1),
	remove_delayed_goals(SeqNums, DelayedGoalsTable, Depth,
		WaitingGoalsTable1, WaitingGoalsTable).

%-----------------------------------------------------------------------------%

	% We are going to delay a goal.
	% Update the delay info structure to record the delayed goal.

delay_info__delay_goal(DelayInfo0, Error, Goal, DelayInfo) :-
	delay_info__check_invariant(DelayInfo0),
	Error = mode_error_info(Vars, _, _, _),
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack0,
				WaitingGoalsTable0, PendingGoals, NextSeqNums0),

		% Get the next sequence number
	stack__pop_det(NextSeqNums0, SeqNum, NextSeqNums1),
	NextSeq is SeqNum + 1,
	stack__push(NextSeqNums1, NextSeq, NextSeqNums),

		% Store the goal in the delayed goal stack
	stack__pop_det(DelayedGoalStack0, DelayedGoals0, DelayedGoalStack1),
	map__set(DelayedGoals0, SeqNum, delayed_goal(Vars, Error, Goal),
			DelayedGoals),
	stack__push(DelayedGoalStack1, DelayedGoals, DelayedGoalStack),

		% Store indexes to the goal in the waiting goals table
	GoalNum = CurrentDepth - SeqNum,
	set__to_sorted_list(Vars, VarList),
	add_waiting_vars(VarList, GoalNum, VarList, WaitingGoalsTable0,
				WaitingGoalsTable),

	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums),
	delay_info__check_invariant(DelayInfo).


	% add_waiting_vars(Vars, Goal, AllVars, WGT0, WGT):
	% update the waiting goals table by adding indexes
	% from each of the variables in Vars to Goal.
	% AllVars must be the list of all the variables which the goal is
	% waiting on.

:- pred add_waiting_vars(list(var), goal_num, list(var), waiting_goals_table,
				waiting_goals_table).
:- mode add_waiting_vars(in, in, in, in, out) is det.

add_waiting_vars([], _, _, WaitingGoalsTable, WaitingGoalsTable).
add_waiting_vars([Var | Vars], Goal, AllVars, WaitingGoalsTable0,
			WaitingGoalsTable) :-
	(
		map__search(WaitingGoalsTable0, Var, WaitingGoals0)
	->
		WaitingGoals1 = WaitingGoals0
	;
		map__init(WaitingGoals1)
	),
	map__set(WaitingGoals1, Goal, AllVars, WaitingGoals),
	map__set(WaitingGoalsTable0, Var, WaitingGoals, WaitingGoalsTable1),
	add_waiting_vars(Vars, Goal, AllVars, WaitingGoalsTable1,
		WaitingGoalsTable).

%-----------------------------------------------------------------------------%

	% Whenever we hit a goal which cannot succeed, we need to wake
	% up all the delayed goals, so that we don't get mode errors in
	% unreachable code.  We remove all the goals from the waiting
	% goals table and add them to the pending goals table.  They
	% will be woken up next time we get back to their conjunction.

delay_info__bind_all_vars(DelayInfo0, DelayInfo) :-
	DelayInfo0 = delay_info(_, _, WaitingGoalsTable0, _, _),
	map__keys(WaitingGoalsTable0, WaitingVars),
	delay_info__bind_var_list(WaitingVars, DelayInfo0, DelayInfo).

delay_info__bind_var_list([], DelayInfo, DelayInfo).
delay_info__bind_var_list([Var|Vars], DelayInfo0, DelayInfo) :-
	delay_info__bind_var(DelayInfo0, Var, DelayInfo1),
	delay_info__bind_var_list(Vars, DelayInfo1, DelayInfo).

	% Whenever we bind a variable, we also check to see whether
	% we need to wake up some goals.  If so, we remove those
	% goals from the waiting goals table and add them to the pending
	% goals table.  They will be woken up next time we get back
	% to their conjunction.

delay_info__bind_var(DelayInfo0, Var, DelayInfo) :-
	delay_info__check_invariant(DelayInfo0),
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable0, PendingGoals0, NextSeqNums),
	(
		map__search(WaitingGoalsTable0, Var, GoalsWaitingOnVar)
	->
		map__keys(GoalsWaitingOnVar, NewlyPendingGoals),
		add_pending_goals(NewlyPendingGoals, GoalsWaitingOnVar,
				PendingGoals0, PendingGoals,
				WaitingGoalsTable0, WaitingGoalsTable),
		DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
				WaitingGoalsTable, PendingGoals, NextSeqNums),
		delay_info__check_invariant(DelayInfo)
	;
		DelayInfo = DelayInfo0
	).

	% Add a collection of goals, identified by depth_num and seq_num
	% (depth of nested conjunction and sequence number within conjunction),
	% to the collection of pending goals.

:- pred add_pending_goals(list(goal_num), map(goal_num, list(var)),
			pending_goals_table, pending_goals_table,
			waiting_goals_table, waiting_goals_table).
:- mode add_pending_goals(in, in, in, out, in, out) is det.

add_pending_goals([], _WaitingVarsTable,
			PendingGoals, PendingGoals,
			WaitingGoals, WaitingGoals).
add_pending_goals([Depth - SeqNum | Rest], WaitingVarsTable,
			PendingGoals0, PendingGoals,
			WaitingGoals0, WaitingGoals) :-

		% remove any other indexes to the goal from the waiting
		% goals table
	GoalNum = Depth - SeqNum,
	map__lookup(WaitingVarsTable, GoalNum, WaitingVars),
	delete_waiting_vars(WaitingVars, GoalNum, WaitingGoals0, WaitingGoals1),

		% add the goal to the pending goals table
	( map__search(PendingGoals0, Depth, PendingSeqNums0) ->
		% XXX should use a queue
		list__append(PendingSeqNums0, [SeqNum], PendingSeqNums)
	;
		PendingSeqNums = [SeqNum]
	),
	map__set(PendingGoals0, Depth, PendingSeqNums, PendingGoals1),

		% do the same for the rest of the pending goals
	add_pending_goals(Rest, WaitingVarsTable,
		PendingGoals1, PendingGoals,
		WaitingGoals1, WaitingGoals).

%-----------------------------------------------------------------------------%

	% Remove all references to a goal from the waiting goals table.

:- pred delete_waiting_vars(list(var), goal_num,
				waiting_goals_table, waiting_goals_table).
:- mode delete_waiting_vars(in, in, in, out) is det.

delete_waiting_vars([], _, WaitingGoalTables, WaitingGoalTables).
delete_waiting_vars([Var | Vars], GoalNum, WaitingGoalsTable0,
				WaitingGoalsTable) :-
	map__lookup(WaitingGoalsTable0, Var, WaitingGoals0),
	map__delete(WaitingGoals0, GoalNum, WaitingGoals),
	( map__is_empty(WaitingGoals) ->
		map__delete(WaitingGoalsTable0, Var, WaitingGoalsTable1)
	;
		map__set(WaitingGoalsTable0, Var, WaitingGoals,
			WaitingGoalsTable1)
	),
	delete_waiting_vars(Vars, GoalNum, WaitingGoalsTable1,
				WaitingGoalsTable).

%-----------------------------------------------------------------------------%

	% delay_info__wakeup_goals(DelayInfo0, Goals, DelayInfo):
	% Goals is the list of pending goal in the order that they should
	% be woken up, and DelayInfo is the new delay_info, updated to
	% reflect the fact that the Goals have been woken up and is
	% hence are longer pending.

delay_info__wakeup_goals(DelayInfo0, Goals, DelayInfo) :-
	( delay_info__wakeup_goal(DelayInfo0, Goal, DelayInfo1) ->
		Goals = [Goal | Goals1],
		delay_info__wakeup_goals(DelayInfo1, Goals1, DelayInfo)
	;
		Goals = [],
		DelayInfo = DelayInfo0
	).

	% Check if there are any "pending" goals, and if so,
	% select one to wake up, remove it from the delay_info,
	% and return it.  If there are no pending goals, this
	% predicate will fail.
	%
:- pred delay_info__wakeup_goal(delay_info, hlds_goal, delay_info).
:- mode delay_info__wakeup_goal(in, out, out) is semidet.

	% delay_info__wakeup_goal(DelayInfo0, Goal, DelayInfo) is true iff
	% DelayInfo0 specifies that there is at least one goal which is
	% pending, Goal is the pending goal which should be reawakened first,
	% and DelayInfo is the new delay_info, updated to reflect the fact
	% that Goal has been woken up and is hence no longer pending.

delay_info__wakeup_goal(DelayInfo0, Goal, DelayInfo) :-
	delay_info__check_invariant(DelayInfo0),
	DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack0, WaitingGoals,
				PendingGoalsTable0, NextSeqNums),

		% is there a goal in the current conjunction which is pending?
	map__search(PendingGoalsTable0, CurrentDepth, PendingGoals0),

		% if so, remove it from the pending goals table,
		% remove it from the delayed goals stack, and return it
	PendingGoals0 = [SeqNum | PendingGoals],
	map__set(PendingGoalsTable0, CurrentDepth, PendingGoals,
			PendingGoalsTable),
	stack__pop_det(DelayedGoalStack0, DelayedGoals0, DelayedGoalStack1),
	map__lookup(DelayedGoals0, SeqNum, DelayedGoal),
	DelayedGoal = delayed_goal(_Vars, _ErrorReason, Goal),
	map__delete(DelayedGoals0, SeqNum, DelayedGoals),
	stack__push(DelayedGoalStack1, DelayedGoals, DelayedGoalStack),
	DelayInfo = delay_info(CurrentDepth, DelayedGoalStack, WaitingGoals,
				PendingGoalsTable, NextSeqNums),
	delay_info__check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
