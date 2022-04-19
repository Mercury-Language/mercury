%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2003, 2005-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: goal_store.m.
% Main author: petdr.
%
% Define a type goal_store(Key) which allows a hlds_goal to be stored in a
% dictionary like structure. However, there some operations on this dictionary
% that are specific to hlds_goals.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.goal_store.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type stored_goal
    --->    stored_goal(hlds_goal, instmap).

:- type goal_store(T).

:- pred goal_store_init(goal_store(T)::out) is det.
:- func goal_store_init = goal_store(T).

:- pred goal_store_det_insert(T::in, stored_goal::in,
    goal_store(T)::in, goal_store(T)::out) is det.

:- pred goal_store_lookup(goal_store(T)::in, T::in, stored_goal::out) is det.

:- pred goal_store_member(goal_store(T)::in, T::out, stored_goal::out)
    is nondet.

:- pred goal_store_all_ancestors(module_info::in, var_table::in,
    goal_store(T)::in, bool::in, T::in, set(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.

:- import_module list.
:- import_module map.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- type goal_store(T) == map.map(T, stored_goal).

%-----------------------------------------------------------------------------%

goal_store_init(GS) :-
    map.init(GS).

goal_store_init = GS :-
    goal_store_init(GS).

goal_store_det_insert(Id, Goal, !GS) :-
    map.det_insert(Id, Goal, !GS).

goal_store_lookup(GS, Id, Goal) :-
    map.lookup(GS, Id, Goal).

goal_store_member(GoalStore, Key, Goal) :-
    map.member(GoalStore, Key, Goal).

goal_store_all_ancestors(ModuleInfo, VarTable, GoalStore, FullyStrict, StartId,
        AncestorIds) :-
    ancestors_loop(ModuleInfo, VarTable, GoalStore, FullyStrict, set.init,
        [StartId], set.init, AncestorIds).

:- pred ancestors_loop(module_info::in, var_table::in, goal_store(T)::in,
    bool::in, set(T)::in, list(T)::in, set(T)::in, set(T)::out) is det.

ancestors_loop(_, _, _, _, _, [], !AncestorIds).
ancestors_loop(ModuleInfo, VarTable, GoalStore, FullyStrict, VisitedIds0,
        [Id | Ids], !AncestorIds) :-
    ( if set.member(Id, VisitedIds0) then
        ancestors_loop(ModuleInfo, VarTable, GoalStore, FullyStrict,
            VisitedIds0, Ids, !AncestorIds)
    else
        direct_ancestors(ModuleInfo, VarTable, GoalStore, FullyStrict,
            Id, IdAncestors),
        set.insert(Id, VisitedIds0, VisitedIds),
        ToProcessIds = set.to_sorted_list(IdAncestors) ++ Ids,
        set.union(IdAncestors, !AncestorIds),
        ancestors_loop(ModuleInfo, VarTable, GoalStore, FullyStrict,
            VisitedIds, ToProcessIds, !AncestorIds)
    ).

:- pred direct_ancestors(module_info::in, var_table::in, goal_store(T)::in,
    bool::in, T::in, set(T)::out) is det.

direct_ancestors(ModuleInfo, VarTable, GoalStore, FullyStrict, StartId,
        Ancestors) :-
    solutions.solutions_set(
        direct_ancestor(ModuleInfo, VarTable, GoalStore, FullyStrict, StartId),
        Ancestors).

:- pred direct_ancestor(module_info::in, var_table::in, goal_store(T)::in,
    bool::in, T::in, T::out) is nondet.

direct_ancestor(ModuleInfo, VarTable, GoalStore, FullyStrict,
        StartId, EarlierId) :-
    goal_store_lookup(GoalStore, StartId,
        stored_goal(LaterGoal, LaterInstMap)),
    goal_store_member(GoalStore, EarlierId,
        stored_goal(EarlierGoal, EarlierInstMap)),
    compare((<), EarlierId, StartId),
    not can_reorder_goals_old(ModuleInfo, VarTable, FullyStrict,
        EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.goal_store.
%-----------------------------------------------------------------------------%
