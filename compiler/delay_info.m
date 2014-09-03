%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2003-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: delay_info.m.
% Main author: fjh.
%
% This module implements part of the mode analysis algorithm.  In the mode
% analysis, reordering of conjunctions is done by simulating coroutining at
% compile time.  This module defines an abstract data type `delay_info' that
% records the information necessary for suspending and waking up goals.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.delay_info.
:- interface.

:- import_module check_hlds.mode_errors.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type delay_info.

%-----------------------------------------------------------------------------%

    % Sanity-check the delay_info structure.
    %
:- pred delay_info_check_invariant(delay_info::in) is det.

    % Initialize the delay_info structure.
    %
:- pred delay_info_init(delay_info::out) is det.

    % Tell the delay_info structure that we've entered a new conjunction.
    %
:- pred delay_info_enter_conj(delay_info::in, delay_info::out) is det.

    % Tell the delay_info structure that we've left a conjunction.
    % This predicate returns a list of the delayed goals from that
    % conjunction, i.e. goals which could not be scheduled.
    %
:- pred delay_info_leave_conj(delay_info::in, list(delayed_goal)::out,
    delay_info::out) is det.

    % Insert a new delayed goal into the delay_info structure.
    %
:- pred delay_info_delay_goal(mode_error_info::in, hlds_goal::in,
    delay_info::in, delay_info::out) is det.

    % Mark a list of variables as having been bound.
    % This may allow a previously delayed goal to change status
    % from "delayed" to "pending".
    % (This predicate just calls delay_info_bind_var in a loop.)
    %
:- pred delay_info_bind_var_list(list(prog_var)::in,
    delay_info::in, delay_info::out) is det.

    % Mark a variable as having been bound.
    % This may allow a previously delayed goal to change status
    % from "delayed" to "pending".
    %
:- pred delay_info_bind_var(prog_var::in, delay_info::in, delay_info::out)
    is det.

    % Mark all variables as having been bound.
    % This will allow all previously delayed goals to change status
    % from "delayed" to "pending".
    %
:- pred delay_info_bind_all_vars(delay_info::in, delay_info::out) is det.

    % delay_info_wakeup_goals(Goals, !DelayInfo):
    %
    % Check if there are any "pending" goals, and if so,
    % remove them from the delay_info and return them.
    %
    % Goals is the list of pending goal in the order that they should be
    % woken up, and !:DelayInfo is the new delay_info, updated to reflect
    % the fact that the Goals have been woken up and is hence are longer
    % pending.
    %
:- pred delay_info_wakeup_goals(list(hlds_goal)::out,
    delay_info::in, delay_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_errors.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module stack.

%-----------------------------------------------------------------------------%

    % The delay_info structure is a tangled web of substructures
    % all of which are pointing at each other - debugging it
    % reminds me of debugging C code - it's a nightmare!
    % (Of course, it could just be that my brain has decided to
    % switch off in protest - after all, it is 5:25am. ;-)

:- type delay_info
    --->    delay_info(
                % CurrentDepth: the current conjunction depth,
                % i.e. the number of nested conjunctions which are
                % currently active.
                delay_depth     :: depth_num,

                % DelayedGoalStack: for each nested conjunction,
                % we store a collection of delayed goals associated with
                % that conjunction, indexed by sequence number.
                delay_goals     :: stack(map(seq_num, delayed_goal)),

                % WaitingGoalsTable: for each variable, we keep track of
                % all the goals which are waiting on that variable.
                delay_waiting   :: waiting_goals_table,

                % PendingGoalsTable: when a variable gets bound, we mark
                % all the goals which are waiting on that variable as ready
                % to be reawakened at the next opportunity.
                delay_pending   :: pending_goals_table,

                % SeqNumsStack: For each nested conjunction, the next
                % available sequence number.
                delay_seqs      :: stack(seq_num)
            ).

    % Used to store the collection of goals waiting on a variable.
:- type waiting_goals_table == map(prog_var, waiting_goals).

    % For each goal, we store all the variables that it is waiting on.
:- type waiting_goals == map(delay_goal_num, list(prog_var)).

:- type pending_goals_table == map(depth_num, list(seq_num)).

:- type delay_goal_num
    --->    delay_goal_num(
                depth_num,
                seq_num
            ).

:- type depth_num == int.       % Eeek! Pointers!
:- type seq_num == int.

%-----------------------------------------------------------------------------%

delay_info_check_invariant(_).
    % for debugging purposes
%%% delay_info_check_invariant(DelayInfo) :-
%%%     delay_info_check_invariant_x(DelayInfo).

:- pred delay_info_check_invariant_x(delay_info::in) is det.

delay_info_check_invariant_x(DelayInfo) :-
    DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, _PendingGoals, NextSeqNums),
    (
        stack.depth(DelayedGoalStack, CurrentDepth),
        stack.depth(NextSeqNums, CurrentDepth),
        map.keys(WaitingGoalsTable, WaitingVars),
        waiting_goals_check_invariant(WaitingVars, WaitingGoalsTable)
    ->
        true
    ;
        unexpected($module, $pred, "invariant violated")
    ).

    % For every variable which goals are waiting on, check the consistency
    % of all the goals waiting on that var.
    %
:- pred waiting_goals_check_invariant(list(prog_var)::in,
    waiting_goals_table::in) is semidet.

waiting_goals_check_invariant([], _).
waiting_goals_check_invariant([Var | Vars], WaitingGoalsTable) :-
    map.lookup(WaitingGoalsTable, Var, WaitingGoals),
    map.keys(WaitingGoals, GoalNums),
    waiting_goal_check_invariant(GoalNums, WaitingGoals, WaitingGoalsTable),
    waiting_goals_check_invariant(Vars, WaitingGoalsTable).

    % Check the consistency of a list of goal_nums in the waiting_goals_table.
    %
:- pred waiting_goal_check_invariant(list(delay_goal_num)::in,
    waiting_goals::in, waiting_goals_table::in) is semidet.

waiting_goal_check_invariant([], _, _).
waiting_goal_check_invariant([GoalNum | GoalNums], WaitingGoals,
        WaitingGoalsTable) :-
    map.lookup(WaitingGoals, GoalNum, Vars),
    set.list_to_set(Vars, VarsSet),
    waiting_goal_vars_check_invariant(Vars, GoalNum, VarsSet,
        WaitingGoalsTable),
    waiting_goal_check_invariant(GoalNums, WaitingGoals, WaitingGoalsTable).

    % For every variable which a goal is waiting on, there should be an entry
    % in the waiting_goals_table for that goal, and the set of vars which it is
    % waiting on in that entry should be the same as in all its other entries.
    %
:- pred waiting_goal_vars_check_invariant(list(prog_var)::in,
    delay_goal_num::in, set(prog_var)::in, waiting_goals_table::in) is semidet.

waiting_goal_vars_check_invariant([], _, _, _).
waiting_goal_vars_check_invariant([Var | Vars], GoalNum, GivenVars,
        WaitingGoalsTable) :-
    map.search(WaitingGoalsTable, Var, WaitingGoals),
    map.search(WaitingGoals, GoalNum, VarsList),
    set.list_to_set(VarsList, VarsSet),
    set.equal(GivenVars, VarsSet),
    waiting_goal_vars_check_invariant(Vars, GoalNum, GivenVars,
        WaitingGoalsTable).

%-----------------------------------------------------------------------------%

    % Initialize the delay info structure in preparation for mode analysis of
    % a goal.
    %
delay_info_init(DelayInfo) :-
    CurrentDepth = 0,
    stack.init(DelayedGoalStack),
    map.init(WaitingGoalsTable),
    map.init(PendingGoals),
    stack.init(NextSeqNums),
    DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%

delay_info_enter_conj(DelayInfo0, DelayInfo) :-
    delay_info_check_invariant(DelayInfo0),
    DelayInfo0 = delay_info(CurrentDepth0, DelayedGoalStack0,
        WaitingGoalsTable, PendingGoals, NextSeqNums0),
    map.init(DelayedGoals),
    stack.push(DelayedGoals, DelayedGoalStack0, DelayedGoalStack),
    stack.push(0, NextSeqNums0, NextSeqNums),
    CurrentDepth = CurrentDepth0 + 1,
    DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%

delay_info_leave_conj(DelayInfo0, DelayedGoalsList, DelayInfo) :-
    delay_info_check_invariant(DelayInfo0),
    DelayInfo0 = delay_info(CurrentDepth0, DelayedGoalStack0,
        WaitingGoalsTable0, PendingGoals, NextSeqNums0),
    stack.det_pop(DelayedGoals, DelayedGoalStack0, DelayedGoalStack),
    map.keys(DelayedGoals, SeqNums),
    remove_delayed_goals(SeqNums, DelayedGoals, CurrentDepth0,
        WaitingGoalsTable0, WaitingGoalsTable),
    stack.det_pop(_, NextSeqNums0, NextSeqNums),
    CurrentDepth = CurrentDepth0 - 1,
    map.values(DelayedGoals, DelayedGoalsList),
    DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo).

%-----------------------------------------------------------------------------%

    % When a conjunction flounders, we need to remove the delayed sub-goals
    % from the waiting goals table before we delay the conjunction as a whole.
    %
:- pred remove_delayed_goals(list(seq_num)::in, map(seq_num, delayed_goal)::in,
    depth_num::in, waiting_goals_table::in, waiting_goals_table::out) is det.

remove_delayed_goals([], _, _, !WaitingGoalsTable).
remove_delayed_goals([SeqNum | SeqNums], DelayedGoalsTable, Depth,
        !WaitingGoalsTable) :-
    map.lookup(DelayedGoalsTable, SeqNum, DelayedGoal),
    DelayedGoal = delayed_goal(Vars, _Error, _Goal),
    GoalNum = delay_goal_num(Depth, SeqNum),
    set_of_var.to_sorted_list(Vars, VarList),
    delete_waiting_vars(VarList, GoalNum, !WaitingGoalsTable),
    remove_delayed_goals(SeqNums, DelayedGoalsTable, Depth,
        !WaitingGoalsTable).

%-----------------------------------------------------------------------------%

    % We are going to delay a goal.
    % Update the delay info structure to record the delayed goal.
    %
delay_info_delay_goal(Error, Goal, DelayInfo0, DelayInfo) :-
    delay_info_check_invariant(DelayInfo0),
    Error = mode_error_info(Vars, _, _, _),
    DelayInfo0 = delay_info(CurrentDepth, DelayedGoalStack0,
        WaitingGoalsTable0, PendingGoals, NextSeqNums0),

    % Get the next sequence number
    stack.det_pop(SeqNum, NextSeqNums0, NextSeqNums1),
    NextSeq = SeqNum + 1,
    stack.push(NextSeq, NextSeqNums1, NextSeqNums),

    % Store the goal in the delayed goal stack
    stack.det_pop(DelayedGoals0, DelayedGoalStack0, DelayedGoalStack1),
    map.set(SeqNum, delayed_goal(Vars, Error, Goal),
        DelayedGoals0, DelayedGoals),
    stack.push(DelayedGoals, DelayedGoalStack1, DelayedGoalStack),

    % Store indexes to the goal in the waiting goals table
    GoalNum = delay_goal_num(CurrentDepth, SeqNum),
    set_of_var.to_sorted_list(Vars, VarList),
    add_waiting_vars(VarList, GoalNum, VarList,
        WaitingGoalsTable0, WaitingGoalsTable),

    DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo).

    % add_waiting_vars(Vars, Goal, AllVars, WGT0, WGT):
    %
    % Update the waiting goals table by adding indexes from each of the
    % variables in Vars to Goal. AllVars must be the list of all the variables
    % which the goal is waiting on.
    %
:- pred add_waiting_vars(list(prog_var)::in, delay_goal_num::in,
    list(prog_var)::in, waiting_goals_table::in, waiting_goals_table::out)
    is det.

add_waiting_vars([], _, _, !WaitingGoalsTable).
add_waiting_vars([Var | Vars], Goal, AllVars, !WaitingGoalsTable) :-
    ( map.search(!.WaitingGoalsTable, Var, WaitingGoals0) ->
        WaitingGoals1 = WaitingGoals0
    ;
        map.init(WaitingGoals1)
    ),
    map.set(Goal, AllVars, WaitingGoals1, WaitingGoals),
    map.set(Var, WaitingGoals, !WaitingGoalsTable),
    add_waiting_vars(Vars, Goal, AllVars, !WaitingGoalsTable).

%-----------------------------------------------------------------------------%

    % Whenever we hit a goal which cannot succeed, we need to wake up all
    % the delayed goals, so that we don't get mode errors in unreachable code.
    % We remove all the goals from the waiting goals table and add them
    % to the pending goals table. They will be woken up next time we get back
    % to their conjunction.
    %
delay_info_bind_all_vars(!DelayInfo) :-
    map.keys(!.DelayInfo ^ delay_waiting, WaitingVars),
    delay_info_bind_var_list(WaitingVars, !DelayInfo).

delay_info_bind_var_list([], !DelayInfo).
delay_info_bind_var_list([Var|Vars], !DelayInfo) :-
    delay_info_bind_var(Var, !DelayInfo),
    delay_info_bind_var_list(Vars, !DelayInfo).

    % Whenever we bind a variable, we also check to see whether we need to wake
    % up some goals. If so, we remove those goals from the waiting goals table
    % and add them to the pending goals table. They will be woken up next time
    % we get back to their conjunction.
    %
delay_info_bind_var(Var, !DelayInfo) :-
    delay_info_check_invariant(!.DelayInfo),
    !.DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable0, PendingGoals0, NextSeqNums),
    ( map.search(WaitingGoalsTable0, Var, GoalsWaitingOnVar) ->
        map.keys(GoalsWaitingOnVar, NewlyPendingGoals),
        add_pending_goals(NewlyPendingGoals, GoalsWaitingOnVar,
            PendingGoals0, PendingGoals,
            WaitingGoalsTable0, WaitingGoalsTable),
        !:DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
            WaitingGoalsTable, PendingGoals, NextSeqNums),
        delay_info_check_invariant(!.DelayInfo)
    ;
        true
    ).

    % Add a collection of goals, identified by depth_num and seq_num
    % (depth of nested conjunction and sequence number within conjunction),
    % to the collection of pending goals.
    %
:- pred add_pending_goals(list(delay_goal_num)::in,
    map(delay_goal_num, list(prog_var))::in,
    pending_goals_table::in, pending_goals_table::out,
    waiting_goals_table::in, waiting_goals_table::out) is det.

add_pending_goals([], _WaitingVarsTable, !PendingGoals, !WaitingGoals).
add_pending_goals([delay_goal_num(Depth, SeqNum) | Rest], WaitingVarsTable,
        !PendingGoals, !WaitingGoals) :-
    % Remove any other indexes to the goal from the waiting goals table.
    GoalNum = delay_goal_num(Depth, SeqNum),
    map.lookup(WaitingVarsTable, GoalNum, WaitingVars),
    delete_waiting_vars(WaitingVars, GoalNum, !WaitingGoals),

    % Add the goal to the pending goals table.
    ( map.search(!.PendingGoals, Depth, PendingSeqNums0) ->
        % XXX Should use a queue.
        list.append(PendingSeqNums0, [SeqNum], PendingSeqNums)
    ;
        PendingSeqNums = [SeqNum]
    ),
    map.set(Depth, PendingSeqNums, !PendingGoals),

    % Do the same for the rest of the pending goals.
    add_pending_goals(Rest, WaitingVarsTable, !PendingGoals, !WaitingGoals).

%-----------------------------------------------------------------------------%

    % Remove all references to a goal from the waiting goals table.
    %
:- pred delete_waiting_vars(list(prog_var)::in, delay_goal_num::in,
    waiting_goals_table::in, waiting_goals_table::out) is det.

delete_waiting_vars([], _, !WaitingGoalTables).
delete_waiting_vars([Var | Vars], GoalNum, !WaitingGoalsTable) :-
    map.lookup(!.WaitingGoalsTable, Var, WaitingGoals0),
    map.delete(GoalNum, WaitingGoals0, WaitingGoals),
    ( map.is_empty(WaitingGoals) ->
        map.delete(Var, !WaitingGoalsTable)
    ;
        map.set(Var, WaitingGoals, !WaitingGoalsTable)
    ),
    delete_waiting_vars(Vars, GoalNum, !WaitingGoalsTable).

%-----------------------------------------------------------------------------%

delay_info_wakeup_goals(Goals, !DelayInfo) :-
    ( delay_info_wakeup_goal(Goal, !DelayInfo) ->
        Goals = [Goal | Goals1],
        delay_info_wakeup_goals(Goals1, !DelayInfo)
    ;
        Goals = []
    ).

    % Check if there are any "pending" goals, and if so, select one to wake up,
    % remove it from the delay_info, and return it. If there are no pending
    % goals, this predicate will fail.
    %
    % delay_info_wakeup_goal(Goal, !DelayInfo) is true iff
    % !.DelayInfo specifies that there is at least one goal which is pending,
    % Goal is the pending goal which should be reawakened first, and
    % !:DelayInfo is the new delay_info, updated to reflect the fact that
    % Goal has been woken up and is hence no longer pending.
    %
:- pred delay_info_wakeup_goal(hlds_goal::out,
    delay_info::in, delay_info::out) is semidet.

delay_info_wakeup_goal(Goal, !DelayInfo) :-
    delay_info_check_invariant(!.DelayInfo),
    !.DelayInfo = delay_info(CurrentDepth, DelayedGoalStack0, WaitingGoals,
        PendingGoalsTable0, NextSeqNums),

    % Is there a goal in the current conjunction which is pending?
    map.search(PendingGoalsTable0, CurrentDepth, PendingGoals0),

    % If so, remove it from the pending goals table, remove it from the
    % delayed goals stack, and return it.
    PendingGoals0 = [SeqNum | PendingGoals],
    map.set(CurrentDepth, PendingGoals,
        PendingGoalsTable0, PendingGoalsTable),
    stack.det_pop(DelayedGoals0, DelayedGoalStack0, DelayedGoalStack1),
    map.lookup(DelayedGoals0, SeqNum, DelayedGoal),
    DelayedGoal = delayed_goal(_Vars, _ErrorReason, Goal),
    map.delete(SeqNum, DelayedGoals0, DelayedGoals),
    stack.push(DelayedGoals, DelayedGoalStack1, DelayedGoalStack),
    !:DelayInfo = delay_info(CurrentDepth, DelayedGoalStack, WaitingGoals,
        PendingGoalsTable, NextSeqNums),
    delay_info_check_invariant(!.DelayInfo).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.delay_info.
%-----------------------------------------------------------------------------%
