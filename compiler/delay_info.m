%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2003-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2017, 2019, 2021, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: delay_info.m.
% Main author: fjh.
%
% This module implements part of the mode analysis algorithm. In the mode
% analysis, reordering of conjunctions is done by simulating coroutining at
% compile time. This module defines an abstract data type `delay_info' that
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

    % Mark all variables as having been bound.
    % This will allow all previously delayed goals to change status
    % from "delayed" to "pending". They will be woken up the next time
    % we get back to their conjunction.
    %
    % The reason why we need this is that when we hit a goal which cannot
    % succeed, we need to wake up all the delayed goals, so that we don't get
    % mode errors in the following unreachable code.
    %
:- pred delay_info_bind_all_vars(delay_info::in, delay_info::out) is det.

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

    % delay_info_wakeup_goals(Goals, !DelayInfo):
    %
    % If there are any "pending" goals at the current depth, remove them
    % from the delay_info, and return them. Otherwise, return the empty list.
    %
    % Goals is the list of those pending goals in the order that they should
    % be woken up, and !:DelayInfo is the new delay_info, updated to reflect
    % the fact that Goals have been woken up, and hence are longer "pending".
    %
:- pred delay_info_wakeup_goals(list(hlds_goal)::out,
    delay_info::in, delay_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.set_of_var.

:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module stack.

%-----------------------------------------------------------------------------%

:- type depth_num == int.       % Eeek! Pointers!
:- type seq_num == int.

:- type delay_goal_num
    --->    delay_goal_num(
                depth_num,
                seq_num
            ).

    % A delay_goal_stack has one entry for each conjunction depth,
    % the current depth being at the top. The entry for the conjunction
    % at each depth maps the seq_num of each conjunct to the delayed_goal
    % construct of that goal.
:- type delay_goal_stack == stack(map(seq_num, delayed_goal)).

    % Map each variable to the collection of goals waiting on that variable.
:- type waiting_goals_table == map(prog_var, waiting_goals).

    % Map each goal to all the variables that it is waiting on.
    % This SHOULD be all the variables whose entries in the waiting_goals_table
    % include this goal.
:- type waiting_goals == map(delay_goal_num, list(prog_var)).

    % We want to preserve the order of the goals pending at each depth.
    %
    % Using a cord is actually slightly slower in typical usage than
    % an ordered list in which each new seq_num is added at the end,
    % since the list is usually very short. However, using a cord has
    % *much* better worst case behavior.
:- type pending_goals_table == map(depth_num, cord(seq_num)).

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
                delay_depth         :: depth_num,

                % DelayedGoalStack: for each nested conjunction,
                % we store a collection of delayed goals associated with
                % that conjunction, indexed by sequence number.
                delay_goal_stack    :: delay_goal_stack,

                % WaitingGoalsTable: for each variable, we keep track of
                % all the goals which are waiting on that variable.
                delay_waiting       :: waiting_goals_table,

                % PendingGoalsTable: when a variable gets bound, we mark
                % all the goals which are waiting on that variable as ready
                % to be reawakened at the next opportunity.
                delay_pending       :: pending_goals_table,

                % SeqNumsStack: For each nested conjunction, the next
                % available sequence number.
                delay_seq_stack     :: stack(seq_num)
            ).

%-----------------------------------------------------------------------------%

    % Sanity-check the delay_info structure.
    %
    % We return a "new" delay_info that is just the old one, purely
    % to avoid having the call to this predicate eliminated by simplification
    % before inlining has had a chance to run.
    %
:- pred delay_info_check_invariant(delay_info::in, delay_info::out) is det.
:- pragma inline(pred(delay_info_check_invariant/2)).

delay_info_check_invariant(!DelayInfo) :-
    trace [compiletime(flag("check_delay_info_invariant"))] (
        !.DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
            WaitingGoalsTable, _PendingGoals, NextSeqNums),
        ( if
            % Note: the two calls to stack.depth are det, but
            % the unifications of their outputs with CurrentDepth may fail.
            stack.depth(DelayedGoalStack, CurrentDepth),
            stack.depth(NextSeqNums, CurrentDepth),
            % We want to compute WaitingVars only if the depth checks succeed.
            map.keys(WaitingGoalsTable, WaitingVars),
            check_invariant_waiting_goals(WaitingGoalsTable, WaitingVars)
        then
            true
        else
            % NOTE: The next time we get a busted invariant, the first step
            % should to finding the problem should be to replace the code
            % of the condition above with det code that returns a list
            % of specific violations, and abort if the list is non-empty.
            % That would allow this code to report more than the one bit
            % worth of information represented by the absence vs presence
            % of this abort.
            unexpected($pred, "invariant violated")
        )
    ).

    % For every variable which goals are waiting on, check the consistency
    % of all the goals waiting on that var.
    %
:- pred check_invariant_waiting_goals(waiting_goals_table::in,
    list(prog_var)::in) is semidet.

check_invariant_waiting_goals(_WaitingGoalsTable, []).
check_invariant_waiting_goals(WaitingGoalsTable, [Var | Vars]) :-
    map.lookup(WaitingGoalsTable, Var, WaitingGoals),
    map.keys(WaitingGoals, GoalNums),
    check_invariant_waiting_goal(WaitingGoalsTable, WaitingGoals, GoalNums),
    check_invariant_waiting_goals(WaitingGoalsTable, Vars).

    % Check the consistency of a list of goal_nums in the waiting_goals_table.
    %
:- pred check_invariant_waiting_goal(waiting_goals_table::in,
    waiting_goals::in, list(delay_goal_num)::in) is semidet.

check_invariant_waiting_goal(_, _, []).
check_invariant_waiting_goal(WaitingGoalsTable, WaitingGoals,
        [GoalNum | GoalNums]) :-
    map.lookup(WaitingGoals, GoalNum, Vars),
    set.list_to_set(Vars, VarsSet),
    check_invariant_waiting_goal_vars(WaitingGoalsTable, GoalNum,
        VarsSet, Vars),
    check_invariant_waiting_goal(WaitingGoalsTable, WaitingGoals, GoalNums).

    % For every variable which a goal is waiting on, there should be an entry
    % in the waiting_goals_table for that goal, and the set of vars which it is
    % waiting on in that entry should be the same as in all its other entries.
    %
:- pred check_invariant_waiting_goal_vars(waiting_goals_table::in,
    delay_goal_num::in, set(prog_var)::in, list(prog_var)::in) is semidet.

check_invariant_waiting_goal_vars(_, _, _, []).
check_invariant_waiting_goal_vars(WaitingGoalsTable, GoalNum, GivenVars,
        [Var | Vars]) :-
    map.search(WaitingGoalsTable, Var, WaitingGoals),
    map.search(WaitingGoals, GoalNum, VarsList),
    set.list_to_set(VarsList, VarsSet),
    set.equal(GivenVars, VarsSet),
    check_invariant_waiting_goal_vars(WaitingGoalsTable, GoalNum, GivenVars,
        Vars).

%-----------------------------------------------------------------------------%

delay_info_init(DelayInfo) :-
    CurrentDepth = 0,
    stack.init(DelayedGoalStack),
    map.init(WaitingGoalsTable),
    map.init(PendingGoals),
    stack.init(NextSeqNums),
    DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo, _).

%-----------------------------------------------------------------------------%

delay_info_enter_conj(DelayInfo0, DelayInfo) :-
    delay_info_check_invariant(DelayInfo0, DelayInfo1),
    DelayInfo1 = delay_info(CurrentDepth1, DelayedGoalStack1,
        WaitingGoalsTable, PendingGoals, NextSeqNums1),
    map.init(DelayedGoals),
    stack.push(DelayedGoals, DelayedGoalStack1, DelayedGoalStack),
    stack.push(0, NextSeqNums1, NextSeqNums),
    CurrentDepth = CurrentDepth1 + 1,
    DelayInfo2 = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo2, DelayInfo).

%-----------------------------------------------------------------------------%

delay_info_leave_conj(DelayInfo0, DelayedGoalsList, DelayInfo) :-
    delay_info_check_invariant(DelayInfo0, DelayInfo1),
    DelayInfo1 = delay_info(CurrentDepth1, DelayedGoalStack1,
        WaitingGoalsTable1, PendingGoals, NextSeqNums1),
    stack.det_pop(DelayedGoals, DelayedGoalStack1, DelayedGoalStack),
    map.keys(DelayedGoals, SeqNums),
    remove_delayed_goals(SeqNums, DelayedGoals, CurrentDepth1,
        WaitingGoalsTable1, WaitingGoalsTable),
    stack.det_pop(_, NextSeqNums1, NextSeqNums),
    CurrentDepth = CurrentDepth1 - 1,
    map.values(DelayedGoals, DelayedGoalsList),
    DelayInfo2 = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo2, DelayInfo).

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

delay_info_delay_goal(Error, Goal, DelayInfo0, DelayInfo) :-
    delay_info_check_invariant(DelayInfo0, DelayInfo1),
    Error = mode_error_info(Vars, _, _, _),
    DelayInfo1 = delay_info(CurrentDepth, DelayedGoalStack1,
        WaitingGoalsTable1, PendingGoals, NextSeqNums1),

    % Get the next sequence number.
    stack.det_pop(SeqNum, NextSeqNums1, NextSeqNums2),
    NextSeq = SeqNum + 1,
    stack.push(NextSeq, NextSeqNums2, NextSeqNums),

    % Store the goal in the delayed goal stack.
    stack.det_pop(DelayedGoals1, DelayedGoalStack1, DelayedGoalStack2),
    map.set(SeqNum, delayed_goal(Vars, Error, Goal),
        DelayedGoals1, DelayedGoals),
    stack.push(DelayedGoals, DelayedGoalStack2, DelayedGoalStack),

    % Store indexes to the goal in the waiting goals table.
    GoalNum = delay_goal_num(CurrentDepth, SeqNum),
    set_of_var.to_sorted_list(Vars, VarList),
    add_waiting_vars(VarList, GoalNum, VarList,
        WaitingGoalsTable1, WaitingGoalsTable),

    DelayInfo2 = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable, PendingGoals, NextSeqNums),
    delay_info_check_invariant(DelayInfo2, DelayInfo).

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
    ( if map.search(!.WaitingGoalsTable, Var, WaitingGoals0) then
        WaitingGoals1 = WaitingGoals0
    else
        map.init(WaitingGoals1)
    ),
    map.set(Goal, AllVars, WaitingGoals1, WaitingGoals),
    map.set(Var, WaitingGoals, !WaitingGoalsTable),
    add_waiting_vars(Vars, Goal, AllVars, !WaitingGoalsTable).

%-----------------------------------------------------------------------------%

delay_info_bind_all_vars(!DelayInfo) :-
    map.keys(!.DelayInfo ^ delay_waiting, WaitingVars),
    delay_info_bind_var_list(WaitingVars, !DelayInfo).

delay_info_bind_var_list([], !DelayInfo).
delay_info_bind_var_list([Var|Vars], !DelayInfo) :-
    delay_info_bind_var(Var, !DelayInfo),
    delay_info_bind_var_list(Vars, !DelayInfo).

delay_info_bind_var(Var, !DelayInfo) :-
    delay_info_check_invariant(!DelayInfo),
    !.DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
        WaitingGoalsTable0, PendingGoals0, NextSeqNums),
    ( if map.search(WaitingGoalsTable0, Var, GoalsWaitingOnVar) then
        map.keys(GoalsWaitingOnVar, NewlyPendingGoals),
        add_pending_goals(NewlyPendingGoals, GoalsWaitingOnVar,
            PendingGoals0, PendingGoals,
            WaitingGoalsTable0, WaitingGoalsTable),
        !:DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
            WaitingGoalsTable, PendingGoals, NextSeqNums),
        delay_info_check_invariant(!DelayInfo)
    else
        true
    ).

    % Add a collection of goals, identified by depth_num and seq_num
    % (depth of nested conjunction and sequence number within conjunction)
    % to the collection of pending goals.
    %
:- pred add_pending_goals(list(delay_goal_num)::in,
    map(delay_goal_num, list(prog_var))::in,
    pending_goals_table::in, pending_goals_table::out,
    waiting_goals_table::in, waiting_goals_table::out) is det.

add_pending_goals([], _WaitingVarsTable, !PendingGoals, !WaitingGoals).
add_pending_goals([DelayGoalNum | DelayGoalNums], WaitingVarsTable,
        !PendingGoals, !WaitingGoals) :-
    DelayGoalNum = delay_goal_num(Depth, SeqNum),
    % Remove any other indexes to the goal from the waiting goals table.
    GoalNum = delay_goal_num(Depth, SeqNum),
    map.lookup(WaitingVarsTable, GoalNum, WaitingVars),
    delete_waiting_vars(WaitingVars, GoalNum, !WaitingGoals),

    % Add the goal to the pending goals table.
    ( if map.search(!.PendingGoals, Depth, PendingSeqNums0) then
        PendingSeqNums = cord.snoc(PendingSeqNums0, SeqNum),
        map.det_update(Depth, PendingSeqNums, !PendingGoals)
    else
        PendingSeqNums = cord.singleton(SeqNum),
        map.det_insert(Depth, PendingSeqNums, !PendingGoals)
    ),

    % Do the same for the rest of the pending goals.
    add_pending_goals(DelayGoalNums, WaitingVarsTable,
        !PendingGoals, !WaitingGoals).

%-----------------------------------------------------------------------------%

    % Remove all references to a goal from the waiting goals table.
    %
:- pred delete_waiting_vars(list(prog_var)::in, delay_goal_num::in,
    waiting_goals_table::in, waiting_goals_table::out) is det.

delete_waiting_vars([], _, !WaitingGoalTables).
delete_waiting_vars([Var | Vars], GoalNum, !WaitingGoalsTable) :-
    map.lookup(!.WaitingGoalsTable, Var, WaitingGoals0),
    map.delete(GoalNum, WaitingGoals0, WaitingGoals),
    ( if map.is_empty(WaitingGoals) then
        map.delete(Var, !WaitingGoalsTable)
    else
        map.det_update(Var, WaitingGoals, !WaitingGoalsTable)
    ),
    delete_waiting_vars(Vars, GoalNum, !WaitingGoalsTable).

%-----------------------------------------------------------------------------%

delay_info_wakeup_goals(Goals, !DelayInfo) :-
    % We used to process the pending goals individually, and gather them
    % together in a loop. We now process all the pending goals at the current
    % depth together, but this yields no real speedup.
    %
    % During an instrumented bootcheck on 2014 jan 3, almost 91% of the calls
    % returned zero goals, a bit less than 9% returned one goal, and less than
    % 1% of all calls returned two or more goals.

    delay_info_check_invariant(!DelayInfo),
    !.DelayInfo = delay_info(CurrentDepth, DelayedGoalStack0, WaitingGoals,
        PendingGoalsTable0, NextSeqNums),

    % Are there pending goals in the current conjunction?
    ( if map.search(PendingGoalsTable0, CurrentDepth, PendingGoals0) then
        % If so, remove them from the pending goals table, and
        % from the delayed goals stack, and return them.
        SeqNums = cord.list(PendingGoals0),
        (
            SeqNums = [HeadSeqNum | TailSeqNums],
            map.det_update(CurrentDepth, cord.init,
                PendingGoalsTable0, PendingGoalsTable),

            stack.det_pop(DelayedGoals0, DelayedGoalStack0, DelayedGoalStack1),
            lookup_delayed_goal(HeadSeqNum, HeadGoal,
                DelayedGoals0, DelayedGoals1),
            lookup_delayed_goals(TailSeqNums, TailGoals,
                DelayedGoals1, DelayedGoals),
            stack.push(DelayedGoals, DelayedGoalStack1, DelayedGoalStack),
            Goals = [HeadGoal | TailGoals],

            !:DelayInfo = delay_info(CurrentDepth, DelayedGoalStack,
                WaitingGoals, PendingGoalsTable, NextSeqNums),
            delay_info_check_invariant(!DelayInfo)
        ;
            SeqNums = [],
            Goals = []
        )
    else
        Goals = []
    ).

:- pred lookup_delayed_goals(list(seq_num)::in, list(hlds_goal)::out,
    map(seq_num, delayed_goal)::in, map(seq_num, delayed_goal)::out) is det.

lookup_delayed_goals([], [], !DelayedGoalMap).
lookup_delayed_goals([SeqNum | SeqNums], [Goal | Goals], !DelayedGoalMap) :-
    lookup_delayed_goal(SeqNum, Goal, !DelayedGoalMap),
    lookup_delayed_goals(SeqNums, Goals, !DelayedGoalMap).

:- pred lookup_delayed_goal(seq_num::in, hlds_goal::out,
    map(seq_num, delayed_goal)::in, map(seq_num, delayed_goal)::out) is det.

lookup_delayed_goal(SeqNum, Goal, !DelayedGoalMap) :-
    map.det_remove(SeqNum, DelayedGoal, !DelayedGoalMap),
    DelayedGoal = delayed_goal(_Vars, _ErrorReason, Goal).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.delay_info.
%-----------------------------------------------------------------------------%
