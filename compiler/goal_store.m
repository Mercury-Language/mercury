%-----------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module:	goal_store
% Main authors: petdr
%
% Define a type goal_store(Key) which allows a hlds_goal to be stored in a
% dictionary like structure.  However there some operations on this
% dictionary which are specific to hlds_goals.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module goal_store.

:- interface.

:- import_module hlds_goal, hlds_module, instmap.
:- import_module bool, set, std_util.

%-----------------------------------------------------------------------------%

:- type goal == pair(hlds_goal, instmap).
:- type goal_store(T).

:- pred goal_store__init(goal_store(T)::out) is det.
:- func goal_store__init = goal_store(T).

:- pred goal_store__det_insert(goal_store(T)::in, T::in, goal::in,
		goal_store(T)::out) is det.

:- pred goal_store__lookup(goal_store(T)::in, T::in, goal::out) is det.

:- pred goal_store__member(goal_store(T)::in, T::out, goal::out) is nondet.

:- pred goal_store__all_ancestors(goal_store(T)::in, T::in, module_info::in,
		bool::in, set(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module goal_util.
:- import_module int, list, map, require.

:- type goal_store(T) == map__map(T, goal).

%-----------------------------------------------------------------------------%

goal_store__init(GS) :-
	map__init(GS).

goal_store__init = GS :-
	goal_store__init(GS).

goal_store__det_insert(GS0, Id, Goal, GS) :-
	map__det_insert(GS0, Id, Goal, GS).

goal_store__lookup(GS, Id, Goal) :-
	map__lookup(GS, Id, Goal).

goal_store__member(GoalStore, Key, Goal) :-
	map__member(GoalStore, Key, Goal).

goal_store__all_ancestors(GoalStore, StartId, ModuleInfo, FullyStrict,
		AncestorIds) :-
	AncestorIds = ancestors_2(GoalStore, [StartId], set__init,
			ModuleInfo, FullyStrict).

:- func ancestors_2(goal_store(T), list(T), set(T), module_info, bool) = set(T).

ancestors_2(_GoalStore, [], _VisitedIds, _ModuleInfo, _FullyStrict) = set__init.
ancestors_2(GoalStore, [Id|Ids], VisitedIds, ModuleInfo, FullyStrict)
	=  AncestorIds :-
		(
			set__member(Id, VisitedIds)
		->
			AncestorIds = ancestors_2(GoalStore, Ids, VisitedIds,
					ModuleInfo, FullyStrict)
		;
			Ancestors = direct_ancestors(GoalStore, Id, ModuleInfo,
					FullyStrict),
			AncestorIds = set__list_to_set(Ancestors) `union`
					ancestors_2(
						GoalStore,
						Ancestors `append` Ids,
						set__insert(VisitedIds, Id),
						ModuleInfo, FullyStrict)

		).

:- func direct_ancestors(goal_store(T), T, module_info, bool) = list(T).

direct_ancestors(GoalStore, StartId, ModuleInfo, FullyStrict)
	= Ancestors :-
		solutions(direct_ancestor(GoalStore, StartId, ModuleInfo,
				FullyStrict), Ancestors).

:- pred direct_ancestor(goal_store(T)::in, T::in, module_info::in,
		bool::in, T::out) is nondet.

direct_ancestor(GoalStore, StartId, ModuleInfo, FullyStrict, EarlierId) :-
	goal_store__lookup(GoalStore, StartId, LaterGoal - LaterInstMap),
	goal_store__member(GoalStore, EarlierId, EarlierGoal - EarlierInstMap),
	compare((<), EarlierId, StartId),
	not goal_util__can_reorder_goals(ModuleInfo, FullyStrict,
			EarlierInstMap, EarlierGoal,
			LaterInstMap, LaterGoal).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
