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
:- import_module parse_tree.vartypes.

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

:- pred goal_store_all_ancestors(goal_store(T)::in, T::in, vartypes::in,
    module_info::in, bool::in, set(T)::out) is det.

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

goal_store_all_ancestors(GoalStore, StartId, VarTypes, ModuleInfo, FullyStrict,
        AncestorIds) :-
    AncestorIds = ancestors_2(GoalStore, [StartId], set.init,
        VarTypes, ModuleInfo, FullyStrict).

:- func ancestors_2(goal_store(T), list(T), set(T), vartypes, module_info,
    bool) = set(T).

ancestors_2(_GoalStore, [], _VisitedIds, _VarTypes, _ModuleInfo, _FullyStrict)
        = set.init.
ancestors_2(GoalStore, [Id|Ids], VisitedIds, VarTypes, ModuleInfo, FullyStrict)
        =  AncestorIds :-
    ( if set.member(Id, VisitedIds) then
        AncestorIds = ancestors_2(GoalStore, Ids, VisitedIds, VarTypes,
            ModuleInfo, FullyStrict)
    else
        Ancestors = direct_ancestors(GoalStore, Id, VarTypes, ModuleInfo,
            FullyStrict),
        AncestorIds = set.list_to_set(Ancestors) `union`
            ancestors_2(GoalStore, Ancestors `append` Ids,
                set.insert(VisitedIds, Id), VarTypes, ModuleInfo, FullyStrict)
    ).

:- func direct_ancestors(goal_store(T), T, vartypes, module_info, bool)
    = list(T).

direct_ancestors(GoalStore, StartId, VarTypes, ModuleInfo, FullyStrict)
        = Ancestors :-
    solutions.solutions(
        direct_ancestor(GoalStore, StartId, VarTypes, ModuleInfo, FullyStrict),
        Ancestors).

:- pred direct_ancestor(goal_store(T)::in, T::in, vartypes::in,
    module_info::in, bool::in, T::out) is nondet.

direct_ancestor(GoalStore, StartId, VarTypes, ModuleInfo, FullyStrict,
        EarlierId) :-
    goal_store_lookup(GoalStore, StartId,
        stored_goal(LaterGoal, LaterInstMap)),
    goal_store_member(GoalStore, EarlierId,
        stored_goal(EarlierGoal, EarlierInstMap)),
    compare((<), EarlierId, StartId),
    not can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
        EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.goal_store.
%-----------------------------------------------------------------------------%
