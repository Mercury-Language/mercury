%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: instmap.m
% Main author: bromage.
%
% This module contains code which implements the instmap and instmap_delta
% ADTs.
%
% An instmap stores information on what instantiation states a set of
% variables have.  An instmap_delta stores information on how these
% instantiation states change across a goal.
%
%-----------------------------------------------------------------------------%

:- module instmap.

:- interface.

:- import_module hlds_module, prog_data, mode_info, (inst), mode_errors.
:- import_module hlds_data.

:- import_module map, bool, set, list, assoc_list, std_util.

:- type instmap.
:- type instmap_delta.

	% Initialize an empty instmap.
	%
:- pred instmap__init_reachable(instmap).
:- mode instmap__init_reachable(out) is det.

	% Initialize an empty unreachable instmap.
	%
:- pred instmap__init_unreachable(instmap).
:- mode instmap__init_unreachable(out) is det.

	% Initialize an empty reachable instmap_delta.
	%
:- pred instmap_delta_init_reachable(instmap_delta).
:- mode instmap_delta_init_reachable(out) is det.

	% Initialize an empty unreachable instmap_delta.
	%
:- pred instmap_delta_init_unreachable(instmap_delta).
:- mode instmap_delta_init_unreachable(out) is det.

	% For any instmap InstMap, exactly one of
	% instmap__is_reachable(InstMap) and
	% instmap__is_unreachable(InstMap) holds.

	% Is the instmap reachable?
	%
:- pred instmap__is_reachable(instmap).
:- mode instmap__is_reachable(in) is semidet.

	% Is the instmap unreachable?
	%
:- pred instmap__is_unreachable(instmap).
:- mode instmap__is_unreachable(in) is semidet.

	% For any instmap InstMapDelta, exactly one of
	% instmap_delta_is_reachable(InstMapDelta) and
	% instmap_delta_is_unreachable(InstMapDelta) holds.

	% Is the instmap_delta reachable?
	%
:- pred instmap_delta_is_reachable(instmap_delta).
:- mode instmap_delta_is_reachable(in) is semidet.

	% Is the instmap_delta unreachable?
	%
:- pred instmap_delta_is_unreachable(instmap_delta).
:- mode instmap_delta_is_unreachable(in) is semidet.

	% Construct an instmap from an assoc_list.
	% NOTE: The assoc_list in argument 1 must not contain alias/1.
:- pred instmap__from_assoc_list(assoc_list(prog_var, inst), instmap).
:- mode instmap__from_assoc_list(in, out) is det.

	% Construct an instmap_delta from an assoc_list.
	% NOTE: The assoc_list in argument 1 must not contain alias/1.
:- pred instmap_delta_from_assoc_list(assoc_list(prog_var, inst),
		instmap_delta).
:- mode instmap_delta_from_assoc_list(in, out) is det.

	% Construct an instmap from a list of modes.
	% NOTE: The assoc_list in argument 2 must not contain alias/1.
% :- pred instmap_delta_from_mode_list(list(prog_var), list(mode),
% 		module_info, instmap_delta).
% :- mode instmap_delta_from_mode_list(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Return the set of variables in an instmap.
	%
:- pred instmap__vars(instmap, set(prog_var)).
:- mode instmap__vars(in, out) is det.

	% Return the list of variables in an instmap.
	%
:- pred instmap__vars_list(instmap, list(prog_var)).
:- mode instmap__vars_list(in, out) is det.

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.
	%
:- pred instmap__lookup_var(instmap, prog_var, inst).
:- mode instmap__lookup_var(in, in, out) is det.

	% Given an instmap and a list of variables, return a list
	% containing the insts of those variable.
	%
:- pred instmap__lookup_vars(list(prog_var), instmap, list(inst)).
:- mode instmap__lookup_vars(in, in, out) is det.

	% Set an entry in an instmap.
	%
:- pred instmap__set(instmap, prog_var, inst, instmap).
:- mode instmap__set(in, in, in, out) is det.

	% Set multiple entries in an instmap.
	%
:- pred instmap__set_vars(instmap, list(prog_var), list(inst), instmap).
:- mode instmap__set_vars(in, in, in, out) is det.

:- pred instmap__add_alias(instmap, inst_key, inst_key, instmap).
:- mode instmap__add_alias(in, in, in, out) is det.

	% Update the given instmap to include the initial insts of the
	% lambda variables.
:- pred instmap__pre_lambda_update(module_info, list(prog_var), argument_modes,
	instmap_delta, inst_table, inst_table, instmap, instmap).
:- mode instmap__pre_lambda_update(in, in, in, out, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

	% Given two instmaps, compute an instmap delta which records
	% the change in the instantiation state of those variables.
	%
:- pred compute_instmap_delta(instmap, instmap, instmap_delta).
:- mode compute_instmap_delta(in, in, out) is det.

	% Given an instmap and an instmap_delta, apply the instmap_delta
	% to the instmap to produce a new instmap.
	%
:- pred instmap__apply_instmap_delta(instmap, instmap_delta, instmap).
:- mode instmap__apply_instmap_delta(in, in, out) is det.

	% Given an instmap_delta and an instmap_delta, apply the
	% second instmap_delta to the first to produce a new
	% instmap_delta.
	%
:- pred instmap_delta_apply_instmap_delta(instmap_delta, instmap_delta,
		instmap_delta).
:- mode instmap_delta_apply_instmap_delta(in, in, out) is det.

	% instmap_merge(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables,
	%	checking that it is the same for every branch.
	%
:- pred instmap__merge(set(prog_var), list(prog_var), list(instmap),
		instmap, instmap, module_info, inst_table,
		module_info, inst_table, merge_errors).
:- mode instmap__merge(in, in, in, in, out, in, in, out, out, out) is det.

	% instmap_merge(NonLocalVars, LiveVars, InstMaps, InitialInstMap,
	%		FinalInstMap, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables,
	%	checking that it is the same for every branch.
	%
:- pred instmap__merge(set(prog_var), list(instmap), merge_context,
		mode_info, mode_info).
:- mode instmap__merge(in, in, in, mode_info_di, mode_info_uo) is det.

	% instmap__restrict takes an instmap and a set of vars and
	% returns an instmap with its domain restricted to those
	% vars.
	%
:- pred instmap__restrict(instmap, set(prog_var), instmap).
:- mode instmap__restrict(in, in, out) is det.

:- pred instmap_delta_delete_vars(instmap_delta, list(prog_var),
		instmap_delta).
:- mode instmap_delta_delete_vars(in, in, out) is det.

	% `instmap__no_output_vars(Instmap, InstmapDelta, Vars, InstTable,
	%  ModuleInfo)'
	% is true if none of the vars in the set Vars could have become more
	% instantiated when InstmapDelta is applied to Instmap.
:- pred instmap__no_output_vars(instmap, instmap_delta, set(prog_var),
		inst_table, module_info).
:- mode instmap__no_output_vars(in, in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%

	% Bind a variable in an instmap to a functor at the beginning
	% of a case in a switch.
	% (note: cons_id_to_const must succeed given the cons_id).

:- pred instmap__bind_var_to_functor(prog_var, cons_id, instmap, instmap,
		inst_table, inst_table, module_info, module_info).
:- mode instmap__bind_var_to_functor(in, in, in, out, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

	% instmap__unify(NonLocalVars, InstMapNonlocalvarsPairs):
	%	Unify the `InstMaps' in the list of pairs resulting
	%	from different branches of a parallel conjunction and
	%	update the instantiatedness of all the nonlocal variables.
	%	The variable locking that is done when modechecking
	%	the individual conjuncts ensures that variables have
	%	at most one producer.
	%
:- pred instmap__unify(set(prog_var), instmap,
		list(pair(instmap, set(prog_var))), mode_info, mode_info).
:- mode instmap__unify(in, in, in, mode_info_di, mode_info_uo) is det.

	% instmap__unify(NonLocals, InstMapBefore, InstMapNonlocalvarsPairs,
	%	InstTable0, InstTable, ModuleInfo0, ModuleInfo, InstMap,
	%	MergeErrors)
	% Unify the instmap_deltas of different branches of a parallel
	% conjunction.
:- pred instmap__unify(list(prog_var), instmap,
		list(pair(instmap, set(prog_var))), inst_table, inst_table,
		module_info, module_info, instmap, merge_errors).
:- mode instmap__unify(in, in, in, in, out, in, out, out, out) is det.

%-----------------------------------------------------------------------------%

	% `instmap_delta_apply_sub(InstmapDelta0, Must, Sub, InstmapDelta)'
	% the variable substitution Sub to InstmapDelta0 to get the new
	% instmap_delta InstmapDelta.  If there is a variable in
	% InstmapDelta0 which does not appear in Sub, it is ignored if
	% Must is set to no, otherwise it is an error.
	%
:- pred instmap_delta_apply_sub(instmap_delta, bool, map(prog_var, prog_var),
		instmap_delta).
:- mode instmap_delta_apply_sub(in, in, in, out) is det.

:- pred instmap__apply_sub(instmap, bool, map(prog_var, prog_var), instmap).
:- mode instmap__apply_sub(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred instmap__to_assoc_list(instmap, assoc_list(prog_var, inst)).
:- mode instmap__to_assoc_list(in, out) is det.

:- pred instmap_delta_to_assoc_list(instmap_delta, assoc_list(prog_var, inst)).
:- mode instmap_delta_to_assoc_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- pred instmap__apply_alias_sub(instmap, inst, inst).
:- mode instmap__apply_alias_sub(in, in, out) is det.

:- pred instmap__inst_key_table_lookup(instmap, inst_key_table, inst_key,
		inst).
:- mode instmap__inst_key_table_lookup(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred instmap__inst_keys_are_equivalent(inst_key, instmap, inst_key, instmap).
:- mode instmap__inst_keys_are_equivalent(in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%

:- pred instmap__get_inst_key_sub(instmap, inst_key_sub).
:- mode instmap__get_inst_key_sub(in, out) is det.

:- pred instmap_delta_get_inst_key_sub(instmap_delta, inst_key_sub).
:- mode instmap_delta_get_inst_key_sub(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_util, inst_match, prog_data, goal_util.
:- import_module hlds_data, inst_util, term.

:- import_module std_util, require, multi_map, set_bbbtree, string.

:- type instmap_delta	==	instmap.

:- type instmap	--->
		reachable(instmapping, inst_key_sub)
	;	unreachable.

:- type instmapping	==	map(prog_var, inst).

%-----------------------------------------------------------------------------%

	% Initialize an empty instmap and instmap_delta.

instmap__init_reachable(reachable(InstMapping, AliasMap)) :-
	map__init(InstMapping),
	map__init(AliasMap).

instmap__init_unreachable(unreachable).

instmap_delta_init_reachable(reachable(InstMapping, AliasMap)) :-
	map__init(InstMapping),
	map__init(AliasMap).

instmap_delta_init_unreachable(unreachable).

%-----------------------------------------------------------------------------%

instmap__is_reachable(reachable(_, _)).

instmap__is_unreachable(unreachable).

instmap_delta_is_reachable(reachable(_, _)).

instmap_delta_is_unreachable(unreachable).

%-----------------------------------------------------------------------------%

instmap__from_assoc_list(AL, InstMap) :-
	( list__member(_ - not_reached, AL) ->
		InstMap = unreachable
	;
		map__from_assoc_list(AL, Instmapping),
		map__init(AliasMap),
		InstMap = reachable(Instmapping, AliasMap)
	).

instmap_delta_from_assoc_list(AL, InstMapDelta) :-
	instmap__from_assoc_list(AL, InstMapDelta).

%-----------------------------------------------------------------------------%

% instmap_delta_from_mode_list(Vars, Modes, ModuleInfo, InstMapDelta) :-
% 	instmap_delta_init_reachable(InstMapDelta0),
% 	instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo,
% 		InstMapDelta0, InstMapDelta).
% 	
% :- pred instmap_delta_from_mode_list_2(list(prog_var), list(mode),
% 		module_info, instmap_delta, instmap_delta).
% :- mode instmap_delta_from_mode_list_2(in, in, in, in, out) is det.
% 
% instmap_delta_from_mode_list_2([], [], _, InstMapDelta, InstMapDelta).
% instmap_delta_from_mode_list_2([], [_|_], _, _, _) :-
% 	error("instmap_delta_from_mode_list_2").
% instmap_delta_from_mode_list_2([_|_], [], _, _, _) :-
% 	error("instmap_delta_from_mode_list_2").
% instmap_delta_from_mode_list_2([Var | Vars], [Mode | Modes], ModuleInfo,
% 		InstMapDelta0, InstMapDelta) :-
% 	mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
% 	( Inst1 = Inst2 ->
% 		instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo,
% 			InstMapDelta0, InstMapDelta)
% 	;
% 		instmap_delta_set(InstMapDelta0, Var, Inst2, InstMapDelta1),
% 		instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo,
% 			InstMapDelta1, InstMapDelta)
% 	).

%-----------------------------------------------------------------------------%

instmap__vars(Instmap, Vars) :-
	instmap__vars_list(Instmap, VarsList),
	set__list_to_set(VarsList, Vars).

instmap__vars_list(unreachable, []).
instmap__vars_list(reachable(InstMapping, _), VarsList) :-
	map__keys(InstMapping, VarsList).

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.

instmap__lookup_var(unreachable, _Var, not_reached).
instmap__lookup_var(reachable(InstMap, Sub), Var, Inst) :-
	instmapping_lookup_var(InstMap, Var, Inst0),
	inst_apply_sub(Sub, Inst0, Inst).

:- pred instmapping_lookup_var(instmapping, prog_var, inst).
:- mode instmapping_lookup_var(in, in, out) is det.

instmapping_lookup_var(InstMap, Var, Inst) :-
	( map__search(InstMap, Var, VarInst) ->
		Inst = VarInst
	;
		Inst = free(unique)
	).

instmap__lookup_vars([], _InstMap, []).
instmap__lookup_vars([Arg|Args], InstMap, [Inst|Insts]) :-
	instmap__lookup_var(InstMap, Arg, Inst),
	instmap__lookup_vars(Args, InstMap, Insts).

instmap__set_vars(InstMap, [], [], InstMap).
instmap__set_vars(InstMap0, [V | Vs], [I | Is], InstMap) :-
	instmap__set(InstMap0, V, I, InstMap1),
	instmap__set_vars(InstMap1, Vs, Is, InstMap).
instmap__set_vars(_, [_ | _], [], _) :-
	error("instmap__set_vars").
instmap__set_vars(_, [], [_ | _], _) :-
	error("instmap__set_vars").

:- pred find_latest_inst_key(inst_key_sub, inst_key, inst_key).
:- mode find_latest_inst_key(in, in, out) is det.

find_latest_inst_key(Sub, IK0, IK) :-
	( map__search(Sub, IK0, IK1) ->
		find_latest_inst_key(Sub, IK1, IK)
	;
		IK = IK0
	).

instmap__add_alias(unreachable, _, _, unreachable).
instmap__add_alias(reachable(Fwd, Alias0), From0, To0, reachable(Fwd, Alias)) :-
		% XXX Do we need to path compress the alias map here?
	find_latest_inst_key(Alias0, From0, From),
	find_latest_inst_key(Alias0, To0, To),
	map__det_insert(Alias0, From, To, Alias).

instmap__set(unreachable, _Var, _Inst, unreachable).
instmap__set(reachable(InstMapping0, AliasMap), Var, Inst, Instmap) :-
	(
		Inst = not_reached
	->
		Instmap = unreachable
	;
		map__set(InstMapping0, Var, Inst, InstMapping),
		Instmap = reachable(InstMapping, AliasMap)
	).

%-----------------------------------------------------------------------------%

instmap__pre_lambda_update(ModuleInfo, Vars, Modes, InstMapDelta,
		InstTable0, InstTable, InstMap0, InstMap) :-
	% XXX Slightly bogus if Modes has any aliasing.
	Modes = argument_modes(_, ArgModes),
	InstTable = InstTable0,
	mode_list_get_initial_insts(ArgModes, ModuleInfo, Insts),
	assoc_list__from_corresponding_lists(Vars, Insts, VarInsts),
	instmap_delta_from_assoc_list(VarInsts, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

%-----------------------------------------------------------------------------%

	% Given an instmap and an instmap_delta, overlay the
	% entries in the instmap_delta on top of those in the
	% instmap to produce a new instmap.

instmap__apply_instmap_delta(unreachable, _, unreachable).
instmap__apply_instmap_delta(reachable(_, _), unreachable, unreachable).
instmap__apply_instmap_delta(reachable(InstMapping0, AliasMap0),
		reachable(InstMappingDelta, AliasMapDelta),
		reachable(InstMapping, AliasMap)) :-
	map__overlay(InstMapping0, InstMappingDelta, InstMapping),
	map__overlay(AliasMap0, AliasMapDelta, AliasMap).

	% Given two instmap_deltas, overlay the entries in the second
	% instmap_delta on top of those in the first to produce a new
	% instmap_delta.

instmap_delta_apply_instmap_delta(unreachable, _, unreachable).
instmap_delta_apply_instmap_delta(reachable(_, _), unreachable, unreachable).
instmap_delta_apply_instmap_delta(reachable(InstMappingDelta0, AliasMap0),
		reachable(InstMappingDelta1, AliasMap1),
		reachable(InstMappingDelta, AliasMap)) :-
	map__overlay(InstMappingDelta0, InstMappingDelta1, InstMappingDelta),
	map__overlay(AliasMap0, AliasMap1, AliasMap).

%-----------------------------------------------------------------------------%

instmap__restrict(unreachable, _, unreachable).
instmap__restrict(reachable(InstMapping0, AliasMap), Vars,
		reachable(InstMapping, AliasMap)) :-
	map__select(InstMapping0, Vars, InstMapping).

instmap_delta_delete_vars(unreachable, _, unreachable).
instmap_delta_delete_vars(reachable(InstMapping0, AliasMap), Vars,
		reachable(InstMapping, AliasMap)) :-
	instmap_delta_delete_vars_2(Vars, InstMapping0, InstMapping).

:- pred instmap_delta_delete_vars_2(list(prog_var), map(prog_var, inst),
		map(prog_var, inst)).
:- mode instmap_delta_delete_vars_2(in, in, out) is det.

instmap_delta_delete_vars_2([], IM, IM).
instmap_delta_delete_vars_2([V | Vs], IM0, IM) :-
	map__delete(IM0, V, IM1),
	instmap_delta_delete_vars_2(Vs, IM1, IM).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% instmap__merge(NonLocalVars, InstMaps, MergeContext):
	%	Merge the `InstMaps' resulting from different branches
	%	of a disjunction or if-then-else, and update the
	%	instantiatedness of all the nonlocal variables,
	%	checking that it is the same for every branch.
	% Algorithm:
	%	1. Remove unreachable instmaps.
	%	2. Take the first two instmaps and remove singleton inst_keys
	%	   from each.
	%	3. Expand any inst_key substitutions that occur in one instmap
	%	   but not the other (done by `instmap__merge_subs').
	%	4. Call `inst_merge' for each non-local variable.
	%	5. Work out which inst_keys need to become shared and make
	%	   them shared (uses information collected by `inst_merge').
	%	6. Recursively merge the resulting instmap with the instmap
	%	   from the next branch (go back to step 2).

instmap__merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMapBefore),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_liveness(ModeInfo0, Liveness),
	set__to_sorted_list(Liveness, LiveList),
	instmap__merge(NonLocals, LiveList, InstMapList, InstMapBefore,
		InstMapAfter, ModuleInfo0, InstTable0, ModuleInfo, InstTable,
		ErrorList),
	( ErrorList = [FirstError | _] ->
		FirstError = Var - _,
		set__singleton_set(WaitingVars, Var),
		mode_info_error(WaitingVars,
			mode_error_disj(MergeContext, ErrorList),
			ModeInfo0, ModeInfo1
		)
	;
		ModeInfo0 = ModeInfo1
	),
	mode_info_set_module_info(ModeInfo1, ModuleInfo, ModeInfo2),
	mode_info_set_instmap(InstMapAfter, ModeInfo2, ModeInfo3),
	mode_info_set_inst_table(InstTable, ModeInfo3, ModeInfo).

instmap__merge(NonLocals, Liveness, InstMapList0, InstMap0,
		InstMap, ModuleInfo0, InstTable0, ModuleInfo, InstTable,
		ErrorList) :-
	get_reachable_instmaps(InstMapList0, InstMapList1),
	(
		InstMapList1 = []
	->
		InstMap = unreachable,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		ErrorList = []
	;
		InstMap0 = reachable(_, _),
		InstMapList1 = [InstMap1]
	->
		InstMap = InstMap1,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		ErrorList = []
	;
		InstMap0 = reachable(_, _)
	->
		instmap__vars(InstMap0, Vars0),
		set__union(NonLocals, Vars0, AllNonLocals),
		set__to_sorted_list(AllNonLocals, AllNonLocalsList),
		instmap__merge_2(AllNonLocalsList, Liveness, InstMapList1,
			InstTable0, ModuleInfo0, InstMap0, InstTable,
			ModuleInfo, InstMap, ErrorList)
	;
		InstMap = unreachable,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		ErrorList = []
	).

:- pred get_reachable_instmaps(list(instmap), list(instmap)).
:- mode get_reachable_instmaps(in, out) is det.

get_reachable_instmaps -->
	list__filter(instmap__is_reachable).

%-----------------------------------------------------------------------------%

:- interface.

% Export this stuff for use in inst_util.m.
  
:- type uniq_count
	--->	known(int)
	;	many.

:- type uniq_counts(T) == map(T, uniq_count).

:- type inst_key_counts == uniq_counts(inst_key).

:- pred inc_uniq_count(T, uniq_counts(T), uniq_counts(T)).
:- mode inc_uniq_count(in, in, out) is det.

:- pred dec_uniq_count(T, uniq_counts(T), uniq_counts(T)).
:- mode dec_uniq_count(in, in, out) is det.

:- pred has_count_zero(uniq_counts(T), T).
:- mode has_count_zero(in, in) is semidet.

:- pred has_count_one(uniq_counts(T), T).
:- mode has_count_one(in, in) is semidet.

:- pred has_count_many(uniq_counts(T), T).
:- mode has_count_many(in, in) is semidet.

:- pred set_count_many(T, uniq_counts(T), uniq_counts(T)).
:- mode set_count_many(in, in, out) is det.

:- pred uniq_count_max(uniq_count, uniq_count, uniq_count).
:- mode uniq_count_max(in, in, out) is det.

:- pred uniq_counts_max_merge(uniq_counts(T), uniq_counts(T), uniq_counts(T)).
:- mode uniq_counts_max_merge(in, in, out) is det.

:- implementation.

:- import_module int.

inc_uniq_count(Item, Map0, Map) :-
	( map__search(Map0, Item, C0) ->
		(
			C0 = known(N),
			map__det_update(Map0, Item, known(N + 1), Map)
		;
			C0 = many,
			Map = Map0
		)
	;
		map__det_insert(Map0, Item, known(1), Map)
	).

dec_uniq_count(Item, Map0, Map) :-
	( map__search(Map0, Item, C0) ->
		(
			C0 = known(N0),
			int__max(N0 - 1, 0, N),
			map__det_update(Map0, Item, known(N), Map)
		;
			C0 = many,
			Map = Map0
		)
	;
		Map = Map0
	).

has_count_zero(Map, Item) :-
	map__search(Map, Item, Count) => Count = known(0).

has_count_one(Map, Item) :-
	map__search(Map, Item, known(1)).

has_count_many(Map, Item) :-
	map__search(Map, Item, Count),
	( Count = known(N), N > 1
	; Count = many
	).

set_count_many(Item, Map0, Map) :-
	map__set(Map0, Item, many, Map).

uniq_count_max(many, _, many).
uniq_count_max(known(_), many, many).
uniq_count_max(known(A), known(B), known(C)) :-
	int__max(A, B, C).

uniq_counts_max_merge(MapA, MapB, Map) :-
	map__foldl(lambda([Item::in, CountA::in, M0::in, M::out] is det,
		( map__search(M0, Item, CountB) ->
			uniq_count_max(CountA, CountB, Count),
			( Count = CountB ->
				M = M0
			;
				map__det_update(M0, Item, Count, M)
			)
		;
			map__det_insert(M0, Item, CountA, M)
		)), MapA, MapB, Map).

%-----------------------------------------------------------------------------%

	% instmap__count_inst_keys(Vars, InstMaps, InstTable, SeenKeys,
	%		DuplicateKeys, InstKeys):
	%	Return a set of all inst_keys which appear more than
	%	once in the instmaps.

:- pred instmap__count_inst_keys_in_instmaps(list(prog_var), list(instmap),
		module_info, inst_table, inst_key_counts).
:- mode instmap__count_inst_keys_in_instmaps(in, in, in, in, out) is det.

instmap__count_inst_keys_in_instmaps(Vars, InstMaps, ModuleInfo, InstTable,
		IKCounts) :-
	map__init(IKCounts0),
	list__foldl(instmap__count_inst_keys_2(Vars, ModuleInfo, InstTable),
		InstMaps, IKCounts0, IKCounts).

:- pred instmap__count_inst_keys(list(prog_var), module_info, inst_table,
	instmap, inst_key_counts).
:- mode instmap__count_inst_keys(in, in, in, in, out) is det.

instmap__count_inst_keys(Vars, ModuleInfo, InstTable, InstMap, IKCounts) :-
	map__init(IKCounts0),
	instmap__count_inst_keys_2(Vars, ModuleInfo, InstTable, InstMap,
		IKCounts0, IKCounts).

:- pred instmap__count_inst_keys_2(list(prog_var), module_info,
	inst_table, instmap, inst_key_counts, inst_key_counts).
:- mode instmap__count_inst_keys_2(in, in, in, in, in, out) is det.

instmap__count_inst_keys_2([], _InstTable, _ModuleInfo, _InstMap) --> [].
instmap__count_inst_keys_2([V | Vs], ModuleInfo, InstTable, InstMap) -->
	{ instmap__lookup_var(InstMap, V, Inst) },
	{ set__init(SeenTwice) },
	instmap__count_inst_keys_in_inst(no, InstMap, InstTable, ModuleInfo,
		SeenTwice, Inst),
	instmap__count_inst_keys_2(Vs, ModuleInfo, InstTable, InstMap).

:- pred instmap__count_inst_keys_in_inst(bool, instmap, inst_table, module_info,
	set(inst_name), inst, inst_key_counts, inst_key_counts).
:- mode instmap__count_inst_keys_in_inst(in, in, in, in, in, in, in, out)
	is det.

instmap__count_inst_keys_in_inst(SetCountMany, InstMap, InstTable, ModuleInfo,
		SeenTwice, Inst) -->
	inst_fold(InstMap, InstTable, ModuleInfo,
	    count_inst_keys_before(SetCountMany), 
	    count_inst_keys_after(InstMap, InstTable, ModuleInfo, SeenTwice),
	    uniq_counts_max_merge, Inst).

:- pred count_inst_keys_before(bool::in, (inst)::in, set(inst_name)::in,
	inst_key_counts::in, inst_key_counts::out) is semidet.

count_inst_keys_before(SetCountMany, alias(Key), _) -->
	(
		{ SetCountMany = yes },
		set_count_many(Key)
	;
		{ SetCountMany = no },
		inc_uniq_count(Key)
	).

:- pred count_inst_keys_after(instmap::in, inst_table::in, module_info::in,
	set(inst_name)::in, (inst)::in, set(inst_name)::in,
	inst_key_counts::in, inst_key_counts::out) is semidet.

count_inst_keys_after(InstMap, InstTable, ModuleInfo, SeenTwice0,
		defined_inst(InstName), SeenOnce) -->
	{ set__member(InstName, SeenOnce) },
	{ \+ set__member(InstName, SeenTwice0) },
	{ set__insert(SeenTwice0, InstName, SeenTwice) },

		% We need to count the inst_keys in a recursive inst twice
		% because the inst may be unfolded an arbitrary number of
		% times.
	instmap__count_inst_keys_in_inst(yes, InstMap, InstTable, ModuleInfo,
		SeenTwice, defined_inst(InstName)).

%-----------------------------------------------------------------------------%

	% instmap__merge_2(Vars, Liveness, InstMaps, ModuleInfo, ErrorList):
	%       Let `ErrorList' be the list of variables in `Vars' for which
	%       there are two instmaps in `InstMaps' for which the inst
	%       the variable is incompatible.

:- pred instmap__merge_2(list(prog_var), list(prog_var), list(instmap),
		inst_table, module_info, instmap, inst_table, module_info,
		instmap, merge_errors).
:- mode instmap__merge_2(in, in, in, in, in, in, out, out, out, out) is det.

instmap__merge_2(_, _, [], InstTable, ModuleInfo, InstMap, InstTable,
		ModuleInfo, InstMap, []).
instmap__merge_2(Vars, Liveness, [InstMapA | InstMaps], InstTable0, ModuleInfo0,
		InstMap0, InstTable, ModuleInfo, InstMap, ErrorList) :-
	instmap__merge_3(Vars, Liveness, InstMapA, InstMaps, InstTable0,
		ModuleInfo0, InstMap0, InstTable, ModuleInfo, InstMap1,
		ErrorList),
	instmap__remove_singleton_inst_keys(Vars, ModuleInfo, InstTable,
		InstMap1, InstMap).

:- pred instmap__merge_3(list(prog_var), list(prog_var), instmap,
		list(instmap), inst_table, module_info, instmap, inst_table,
		module_info, instmap, merge_errors).
:- mode instmap__merge_3(in, in, in, in, in, in, in, out, out, out, out)
		is det.

instmap__merge_3(_, _, InstMap, [], InstTable, ModuleInfo, _InstMap0,
		InstTable, ModuleInfo, InstMap, []).
instmap__merge_3(Vars, Liveness, InstMapA0, [InstMapB0 | InstMaps], InstTable0,
		ModuleInfo0, InstMap00, InstTable, ModuleInfo, InstMap,
		ErrorList) :-
	instmap__remove_singleton_inst_keys(Vars, ModuleInfo0, InstTable0,
		InstMapA0, InstMapA1),
	instmap__remove_singleton_inst_keys(Vars, ModuleInfo0, InstTable0,
		InstMapB0, InstMapB1),

	instmap__merge_subs(InstMapA1, InstMapB1, InstMap00, InstTable0,
		ModuleInfo0, InstMapA, InstMapB, InstMap0, InstTable1),

	instmap__count_inst_keys(Liveness, ModuleInfo0, InstTable1, InstMapA,
		LiveCountsA),
	instmap__count_inst_keys(Liveness, ModuleInfo0, InstTable1, InstMapB,
		LiveCountsB),
	instmap__count_inst_keys(Vars, ModuleInfo0, InstTable1, InstMapA,
		TotalCountsA),
	instmap__count_inst_keys(Vars, ModuleInfo0, InstTable1, InstMapB,
		TotalCountsB),
	RefCounts0 = ref_counts(LiveCountsA, LiveCountsB, TotalCountsA,
		TotalCountsB),

	map__init(MergeSubs0),
	MergeInfo0 = merge_info(MergeSubs0, RefCounts0),
	instmap__merge_4(Vars, InstMapA, InstMapB, Liveness, InstTable1,
		ModuleInfo0, InstMap0, MergeInfo0, InstTable2,
		ModuleInfo1, InstMapAB0, MergeInfo, ErrorList0),
	MergeInfo = merge_info(MergeSubs, _RefCounts),

	% Work out which inst keys need to be made shared.
	solutions(lambda([I::out] is nondet, (
		map__member(MergeSubs, IKA0 - IKB0, IK),
		map__member(MergeSubs, IKA1 - IKB1, _),
		\+ (
			instmap__inst_keys_are_equivalent(IKA0, InstMapAB0,
				IKA1, InstMapAB0)
		<=>
			instmap__inst_keys_are_equivalent(IKB0, InstMapAB0,
				IKB1, InstMapAB0)
		),
		I = alias(IK) )), Insts),
	make_shared_inst_list(Insts, InstTable2, ModuleInfo1, InstMapAB0,
		_, InstTable3, ModuleInfo2, InstMapAB),

	instmap__merge_3(Vars, Liveness, InstMapAB, InstMaps, InstTable3,
		ModuleInfo2, InstMap0, InstTable, ModuleInfo, InstMap,
		ErrorList1),
	list__append(ErrorList0, ErrorList1, ErrorList).

:- pred instmap__merge_4(list(prog_var), instmap, instmap, list(prog_var),
	inst_table, module_info, instmap, merge_info, inst_table, module_info,
	instmap, merge_info, merge_errors).
:- mode instmap__merge_4(in, in, in, in, in, in, in, in, out, out, out,
	out, out) is det.

instmap__merge_4([], _, _, _, InstTable, ModuleInfo, InstMap, MergeInfo,
		InstTable, ModuleInfo, InstMap, MergeInfo, []).
instmap__merge_4([Var | Vars], InstMapA, InstMapB, Liveness, InstTable0,
		ModuleInfo0, InstMap0, MergeInfo0, InstTable, ModuleInfo,
		InstMap, MergeInfo, Errors) :-
	instmap__merge_4(Vars, InstMapA, InstMapB, Liveness, InstTable0,
		ModuleInfo0, InstMap0, MergeInfo0, InstTable1,
		ModuleInfo1, InstMap1, MergeInfo1,  Errors1),
	instmap__lookup_var(InstMapA, Var, InstA),
	instmap__lookup_var(InstMapB, Var, InstB),
	IsLive = ( list__member(Var, Liveness) -> live ; dead ),
	(
		inst_merge(InstA, InstB, IsLive, InstMap1, InstTable1,
			ModuleInfo1, MergeInfo1, Inst, InstMap2, InstTable2,
			ModuleInfo2, MergeInfo2)
	->
		instmap__set(InstMap2, Var, Inst, InstMap),
		Errors = Errors1,
		ModuleInfo = ModuleInfo2,
		InstTable = InstTable2,
		MergeInfo = MergeInfo2
	;
		instmap__set(InstMap1, Var, not_reached, InstMap),
		Errors = [Var - [InstA, InstB] | Errors1],
		ModuleInfo = ModuleInfo1,
		InstTable = InstTable1,
		MergeInfo = MergeInfo1
	).

:- pred instmap__merge_subs(instmap, instmap, instmap, inst_table, module_info,
		instmap, instmap, instmap, inst_table).
:- mode instmap__merge_subs(in, in, in, in, in, out, out, out, out) is det.

instmap__merge_subs(InstMapA0, InstMapB0, InstMap00, InstTable0, ModuleInfo,
		InstMapA, InstMapB, InstMap0, InstTable) :-
	(
		InstMapA0 = reachable(InstMappingA0, SubA0),
		InstMapB0 = reachable(InstMappingB0, SubB0)
	->
		solutions(lambda([K::out] is nondet,
			(
				map__member(SubA0, K, V),
				\+ map__search(SubB0, K, V)
			;
				map__member(SubB0, K, V),
				\+ map__search(SubA0, K, V)
			)), KeysList),
		set_bbbtree__sorted_list_to_set(KeysList, Keys),
		instmap__expand_subs(Keys, ModuleInfo, SubA0, InstMappingA0,
			InstMappingA, InstTable0, InstTable1),
		instmap__expand_subs(Keys, ModuleInfo, SubB0, InstMappingB0,
			InstMappingB, InstTable1, InstTable),
		map__delete_list(SubA0, KeysList, Sub),
		InstMapA = reachable(InstMappingA, Sub),
		InstMapB = reachable(InstMappingB, Sub),
		( InstMap00 = reachable(InstMapping0, _) ->
			InstMap0 = reachable(InstMapping0, Sub)
		;
			error("instmap__merge_subs: initial instmap unreachable")
		)
	;
		error("instmap__merge_subs: unreachable instmap(s)")
	).

:- pred instmap__expand_subs(inst_key_set, module_info, inst_key_sub,
		instmapping, instmapping, inst_table, inst_table).
:- mode instmap__expand_subs(in, in, in, in, out, in, out) is det.

instmap__expand_subs(Keys, ModuleInfo, Sub, InstMapping0, InstMapping,
		InstTable0, InstTable) :-
	map__to_assoc_list(InstMapping0, AL0),
	map__init(SeenIKs),
	instmap__expand_subs_2(Keys, ModuleInfo, Sub, SeenIKs, AL0, AL,
		InstTable0, InstTable),
	map__from_assoc_list(AL, InstMapping).

:- pred instmap__expand_subs_2(inst_key_set, module_info, inst_key_sub,
		inst_key_sub, assoc_list(prog_var, inst),
		assoc_list(prog_var, inst), inst_table, inst_table).
:- mode instmap__expand_subs_2(in, in, in, in, in, out, in, out) is det.

instmap__expand_subs_2(_, _, _, _, [], [], InstTable, InstTable).
instmap__expand_subs_2(Keys, ModuleInfo, Sub, SeenIKs0,
		[Var - Inst0 | VarInsts0], [Var - Inst | VarInsts],
		InstTable0, InstTable) :-
	instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		Inst0, Inst, InstTable0, InstTable1),
	instmap__expand_subs_2(Keys, ModuleInfo, Sub, SeenIKs,
		VarInsts0, VarInsts, InstTable1, InstTable).

:- pred instmap__expand_inst_sub(inst_key_set, module_info,
	inst_key_sub, inst_key_sub, inst_key_sub, inst, inst,
	inst_table, inst_table).
:- mode instmap__expand_inst_sub(in, in, in, in, out, in, out, in, out) is det.

instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		alias(IK0), Inst, InstTable0, InstTable) :-
	( map__search(SeenIKs0, IK0, IK1) ->
		% We have seen IK0 before and replaced it with IK1.
		Inst = alias(IK1),
		SeenIKs = SeenIKs0,
		InstTable = InstTable0
	; map__search(Sub, IK0, IK1) ->
		% IK0 has a substitution so recursively expand it.
		instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0,
			SeenIKs1, alias(IK1), Inst1, InstTable0, InstTable),
		(
			Inst1 = alias(IK1),
			\+ set_bbbtree__member(IK0, Keys)
		->
			Inst = alias(IK0),
			map__det_insert(SeenIKs1, IK0, IK0, SeenIKs)
		;
			Inst = Inst1,
			( Inst = alias(IK2) ->
				map__det_insert(SeenIKs1, IK0, IK2, SeenIKs)
			;
				error("instmap__expand_inst_sub")
			)
		)
	;
		inst_table_get_inst_key_table(InstTable0, IKT0),
		inst_key_table_lookup(IKT0, IK0, Inst0),
		instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0,
			SeenIKs1, Inst0, Inst1, InstTable0, InstTable1),
		( Inst0 = Inst1 ->
			Inst = alias(IK0),
			InstTable = InstTable1,
			map__det_insert(SeenIKs1, IK0, IK0, SeenIKs)
		;
			% Inst has changed so we need to create a new inst_key.
			inst_table_get_inst_key_table(InstTable1, IKT1),
			inst_key_table_add(IKT1, Inst1, IK1, IKT),
			inst_table_set_inst_key_table(InstTable1, IKT,
				InstTable),
			map__det_insert(SeenIKs1, IK0, IK1, SeenIKs),
			Inst = alias(IK1)
		)
	).
instmap__expand_inst_sub(_, _, _, SeenIKs, SeenIKs, any(U), any(U),
		InstTable, InstTable).
instmap__expand_inst_sub(_, _, _, SeenIKs, SeenIKs, free(A), free(A),
		InstTable, InstTable).
instmap__expand_inst_sub(_, _, _, SeenIKs, SeenIKs, free(A, T),
		free(A, T), InstTable, InstTable).
instmap__expand_inst_sub(_, _, _, SeenIKs, SeenIKs, ground(U, P),
		ground(U, P), InstTable, InstTable).
instmap__expand_inst_sub(_, _, _, SeenIKs, SeenIKs, not_reached,
		not_reached, InstTable, InstTable).
instmap__expand_inst_sub(_, _, _, _, _, inst_var(_), _, _, _) :-
	error("instmap__expand_inst_sub: inst_var(_)").
instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		bound(U, BoundInsts0), bound(U, BoundInsts),
		InstTable0, InstTable) :-
	instmap__expand_bound_insts_sub(Keys, ModuleInfo, Sub, SeenIKs0,
		SeenIKs, BoundInsts0, BoundInsts, InstTable0, InstTable).
instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		abstract_inst(N, Insts0), abstract_inst(N, Insts),
		InstTable0, InstTable) :-
	instmap__expand_inst_list_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		Insts0, Insts, InstTable0, InstTable).
instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		defined_inst(InstName), Inst, InstTable0, InstTable) :-
	inst_table_get_substitution_insts(InstTable0, SubInsts0),
	SubInst = substitution_inst(InstName, Keys, Sub),
	SubInstName = substitution_inst(InstName, Keys, Sub),
	(
		map__search(SubInsts0, SubInst, Result)
	->
		( Result = known(Inst0) ->
			Inst2 = Inst0
		;
			Inst2 = defined_inst(SubInstName)
		),
		SeenIKs = SeenIKs0,
		InstTable = InstTable0
	;
		% Insert the inst_name in the substitution_inst_table with
		% value `unknown' for the moment.
		map__det_insert(SubInsts0, SubInst, unknown, SubInsts1),
		inst_table_set_substitution_insts(InstTable0, SubInsts1,
			InstTable1),

		% Recursively expand the inst.
		inst_lookup(InstTable1, ModuleInfo, InstName, Inst0),
		inst_expand_defined_inst(InstTable1, ModuleInfo, Inst0, Inst1),
		instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0,
			SeenIKs, Inst1, Inst2, InstTable1, InstTable2),

		% Update the substitution_inst_table with the known value.
		inst_table_get_substitution_insts(InstTable2, SubInsts2),
		map__det_update(SubInsts2, SubInst, known(Inst2), SubInsts),
		inst_table_set_substitution_insts(InstTable2, SubInsts,
			InstTable)
	),
		% Avoid expanding recursive insts.
	map__init(InstMapping),
	(
			% InstMapping is not used by inst_contains_instname.
		inst_contains_instname(Inst2, reachable(InstMapping, Sub),
			InstTable, ModuleInfo, InstName)
	->
		Inst = defined_inst(InstName)
	;
			% InstMapping is not used by inst_contains_instname.
		inst_contains_instname(Inst2, reachable(InstMapping, Sub),
			InstTable, ModuleInfo, SubInstName)
	->
		Inst = defined_inst(SubInstName)
	;
		Inst = Inst2
	).

:- pred instmap__expand_bound_insts_sub(inst_key_set, module_info,
	inst_key_sub, inst_key_sub, inst_key_sub, list(bound_inst),
	list(bound_inst), inst_table, inst_table).
:- mode instmap__expand_bound_insts_sub(in, in, in, in, out, in, out, in, out)
	is det.

instmap__expand_bound_insts_sub(_, _, _, SeenIKs, SeenIKs, [], [],
		InstTable, InstTable).
instmap__expand_bound_insts_sub(Keys, ModuleInfo, Sub, SeenIKs0, SeenIKs,
		[functor(ConsId, Insts0) | BoundInsts0],
		[functor(ConsId, Insts) | BoundInsts], InstTable0, InstTable) :-
	instmap__expand_inst_list_sub(Keys, ModuleInfo, Sub,
		SeenIKs0, SeenIKs1, Insts0, Insts, InstTable0, InstTable1),
	instmap__expand_bound_insts_sub(Keys, ModuleInfo, Sub,
		SeenIKs1, SeenIKs, BoundInsts0, BoundInsts,
		InstTable1, InstTable).

:- pred instmap__expand_inst_list_sub(inst_key_set, module_info,
	inst_key_sub, inst_key_sub, inst_key_sub, list(inst), list(inst),
	inst_table, inst_table).
:- mode instmap__expand_inst_list_sub(in, in, in, in, out, in, out, in, out)
	is det.

instmap__expand_inst_list_sub(_, _, _, SeenIKs, SeenIKs, [], [],
		InstTable, InstTable).
instmap__expand_inst_list_sub(Keys, ModuleInfo, Sub,
		SeenIKs0, SeenIKs, [Inst0 | Insts0], [Inst | Insts],
		InstTable0, InstTable) :-
	instmap__expand_inst_sub(Keys, ModuleInfo, Sub, SeenIKs0,
		SeenIKs1, Inst0, Inst, InstTable0, InstTable1),
	instmap__expand_inst_list_sub(Keys, ModuleInfo, Sub,
		SeenIKs1, SeenIKs, Insts0, Insts, InstTable1, InstTable).

%-----------------------------------------------------------------------------%

:- pred instmap__remove_singleton_inst_keys(list(prog_var), module_info,
	inst_table, instmap, instmap).
:- mode instmap__remove_singleton_inst_keys(in, in, in, in, out) is det.

instmap__remove_singleton_inst_keys(Vars, ModuleInfo, InstTable, InstMap0,
		InstMap) :-
	instmap__count_inst_keys(Vars, ModuleInfo, InstTable, InstMap0,
		IKCounts),
	list__foldl(instmap__remove_singleton_inst_keys_2(IKCounts, ModuleInfo,
		InstTable), Vars, InstMap0, InstMap).

:- pred instmap__remove_singleton_inst_keys_2(inst_key_counts,
	module_info, inst_table, prog_var, instmap, instmap).
:- mode instmap__remove_singleton_inst_keys_2(in, in, in, in, in, out) is det.

instmap__remove_singleton_inst_keys_2(IKCounts, ModuleInfo, InstTable, Var,
		InstMap0, InstMap) :-
	instmap__lookup_var(InstMap0, Var, Inst0),
	instmap__remove_singleton_inst_key_from_inst(IKCounts, ModuleInfo,
		InstTable, InstMap0, Inst0, Inst),
	instmap__set(InstMap0, Var, Inst, InstMap).

:- pred instmap__remove_singleton_inst_key_from_inst(inst_key_counts,
	module_info, inst_table, instmap, inst, inst).
:- mode instmap__remove_singleton_inst_key_from_inst(in, in, in, in, in, out)
	is det.

instmap__remove_singleton_inst_key_from_inst(IKCounts, ModuleInfo, InstTable,
		InstMap, alias(IK), Inst) :-
	( has_count_one(IKCounts, IK) ->
		inst_table_get_inst_key_table(InstTable, IKT),
		instmap__inst_key_table_lookup(InstMap, IKT, IK, Inst1),
		instmap__remove_singleton_inst_key_from_inst(IKCounts,
			ModuleInfo, InstTable, InstMap, Inst1, Inst)
	;
		Inst = alias(IK)
	).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _, any(U), any(U)).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _, free(A), free(A)).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _,
		free(A, T), free(A, T)).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _,
		ground(U, P), ground(U, P)).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _,
		not_reached, not_reached).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _,
		defined_inst(N), defined_inst(N)).
instmap__remove_singleton_inst_key_from_inst(_, _, _, _, inst_var(_), _) :-
	error("instmap__remove_singleton_inst_key_from_inst: inst_var").
instmap__remove_singleton_inst_key_from_inst(IKCounts, ModuleInfo, InstTable,
		InstMap, bound(U, BoundInsts0), bound(U, BoundInsts)) :-
	list__map(lambda([BI0::in, BI::out] is det, (
		BI0 = functor(C, Insts0),
		list__map(instmap__remove_singleton_inst_key_from_inst(IKCounts,
				ModuleInfo, InstTable, InstMap),
			Insts0, Insts),
		BI = functor(C, Insts)
		)), BoundInsts0, BoundInsts).
instmap__remove_singleton_inst_key_from_inst(IKCounts, ModuleInfo, InstTable,
		InstMap, abstract_inst(N, Insts0), abstract_inst(N, Insts)) :-
	list__map(instmap__remove_singleton_inst_key_from_inst(IKCounts,
			ModuleInfo, InstTable, InstMap),
		Insts0, Insts).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__unify(NonLocals, InstMapBefore, InstMapList, ModeInfo0, ModeInfo) :-
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	set__to_sorted_list(NonLocals, NonLocalsList),
	instmap__unify(NonLocalsList, InstMapBefore, InstMapList,
		InstTable0, InstTable, ModuleInfo0, ModuleInfo, InstMap,
		ErrorList),
	( ErrorList = [FirstError | _] ->
		FirstError = Var - _,
		set__singleton_set(WaitingVars, Var),
		mode_info_error(WaitingVars, mode_error_par_conj(ErrorList),
			ModeInfo0, ModeInfo1)
	;
		ModeInfo0 = ModeInfo1
	),
	mode_info_set_module_info(ModeInfo1, ModuleInfo, ModeInfo2),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo3),
	mode_info_set_inst_table(InstTable, ModeInfo3, ModeInfo).

instmap__unify(_NonLocals, InstMapBefore, InstMapList,
		InstTable0, InstTable, ModuleInfo0, ModuleInfo,
		InstMap, ErrorList) :-
	(
			% If any of the instmaps is unreachable, then
			% the final instmap is unreachable.
		list__member(unreachable - _, InstMapList)
	->
		instmap__init_unreachable(InstMap),
		InstTable = InstTable0,
		ModuleInfo = ModuleInfo0,
		ErrorList = []
	;
			% If there is only one instmap, then we just
			% stick it in the mode_info.
		InstMapList = [SingleInstMap - _]
	->
		InstMap = SingleInstMap,
		InstTable = InstTable0,
		ModuleInfo = ModuleInfo0,
		ErrorList = []
	;
		instmap__is_reachable(InstMapBefore)
	->
			% having got the first instmapping, to use as
			% an accumulator, all instmap__unify_2 which
			% unifies each of the nonlocals from each instmap
			% with the corresponding inst in the accumulator.
		error("instmap__unify_2: NYI")
		% instmap__unify_2(NonLocals, InstMapBefore,
		% 	InstMapList, InstTable0, ModuleInfo0, InstMapping0,
		% 	InstTable, ModuleInfo, InstMap, ErrorList)
	;
		InstMap = InstMapBefore,
		InstTable = InstTable0,
		ModuleInfo = ModuleInfo0,
		ErrorList = []
	).

% 	% instmap__unify_2(Vars, InitialInstMap, InstMaps, ModuleInfo,
% 	%		ErrorList):
% 	%       Let `ErrorList' be the list of variables in `Vars' for
% 	%       which there are two instmaps in `InstMaps' for which the insts
% 	%       of the variable is incompatible.
% :- pred instmap__unify_2(list(prog_var), instmap,
%		list(pair(instmap, set(prog_var))),
% 		inst_table, module_info, instmap,
% 		inst_table, module_info, instmap,
% 		merge_errors).
% :- mode instmap__unify_2(in, in, in, in, in, in, out, out, out, out)
% 		is det.
% 
% instmap__unify_2([], _, _, InstTable, ModuleInfo, InstMap, InstTable,
% 		ModuleInfo, InstMap, []).
% instmap__unify_2([Var|Vars], InitialInstMap, InstMapList, InstTable0,
% 		ModuleInfo0, InstMap0, InstTable, ModuleInfo, InstMap,
% 		ErrorList) :-
% 	instmap__unify_2(Vars, InitialInstMap, InstMapList, InstTable0,
% 		ModuleInfo0, InstMap0, InstTable1, ModuleInfo1, InstMap1,
% 		ErrorList1),
% 	instmap__lookup_var(InitialInstMap, Var, InitialVarInst),
% 	instmap__unify_var(InstMapList, Var, [], Insts, InitialVarInst, Inst,
% 		InstTable1, InstTable, ModuleInfo1, ModuleInfo, InstMap1,
% 		InstMap, no, Error),
% 	( Error = yes ->
% 		ErrorList = [Var - Insts | ErrorList1]
% 	;
% 		ErrorList = ErrorList1
% 	),
% 	instmap__set(InstMap1, Var, Inst, InstMap).
% 
% 	% instmap__unify_var(InstMaps, Var, InitialInstMap, ModuleInfo,
% 	%		Insts, Error):
% 	%	Let `Insts' be the list of the inst of `Var' in
% 	%	each of the corresponding `InstMaps'.  Let `Error' be yes
% 	%	iff there are two instmaps for which the inst of `Var'
% 	%	is incompatible.
% 
% :- pred instmap__unify_var(list(pair(instmap, set(prog_var))), prog_var,
% 		list(inst), list(inst), inst, inst, inst_table, inst_table,
% 		module_info, module_info, bool, bool).
% :- mode instmap__unify_var(in, in, in, out, in, out, in, out, in, out,
% 		in, out) is det.
% 
% instmap__unify_var([], _, Insts, Insts, Inst, Inst, InstTable, InstTable,
% 		ModuleInfo, ModuleInfo, Sub, Sub, Error, Error).
% instmap__unify_var([InstMap - Nonlocals| Rest], Var, InstList0, InstList,
% 		Inst0, Inst, InstTable0, InstTable, ModuleInfo0, ModuleInfo, 
% 		Error0, Error) :-
% 	(
% 		set__member(Var, Nonlocals)
% 	->
% 		instmap__lookup_var(InstMap, Var, VarInst),
% 		(
% 			% We can ignore the determinism of the unification:
% 			% if it isn't det, then there will be a mode error
% 			% or a determinism error in one of the parallel
% 			% conjuncts.
% 
% 			abstractly_unify_inst(live, Inst0, VarInst, fake_unify,
% 				InstTable0, ModuleInfo0, _InstMap, Inst1, _Det,
% 				InstTable1, ModuleInfo1, _InstMap)
% 		->
% 			Inst2 = Inst1,
% 			ModuleInfo2 = ModuleInfo1,
% 			Error1 = Error0,
% 			InstTable2 = InstTable1
% 		;
% 			Error1 = yes,
% 			ModuleInfo2 = ModuleInfo0,
% 			Inst2 = not_reached,
% 			InstTable2 = InstTable0
% 		)
% 	;
% 		VarInst = free(unique),
% 		Inst2 = Inst0,
% 		Error1 = Error0,
% 		ModuleInfo2 = ModuleInfo0,
% 		InstTable2 = InstTable0
% 	),
% 	instmap__unify_var(Rest, Var, [VarInst | InstList0], InstList,
% 		Inst2, Inst, InstTable2, InstTable, ModuleInfo2, ModuleInfo,
% 		Error1, Error).

%-----------------------------------------------------------------------------%

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.

compute_instmap_delta(unreachable, _, unreachable).
compute_instmap_delta(reachable(_, _), unreachable, unreachable).
compute_instmap_delta(reachable(InstMapA, AliasMapA),
		reachable(InstMapB, AliasMapB),
		reachable(DeltaInstMap, DeltaAliasMap)) :-
	compute_map_delta(InstMapA, InstMapB, DeltaInstMap),
	compute_map_delta(AliasMapA, AliasMapB, DeltaAliasMap).

:- pred compute_map_delta(map(K, V), map(K, V), map(K, V)).
:- mode compute_map_delta(in, in, out) is det.

compute_map_delta(MapA, MapB, DeltaMap) :-
	map__to_assoc_list(MapB, ALB),
	map__init(DeltaMap0),
	compute_map_delta_2(ALB, MapA, DeltaMap0, DeltaMap).

:- pred compute_map_delta_2(assoc_list(K, V), map(K, V),
					map(K, V), map(K, V)).
:- mode compute_map_delta_2(in, in, in, out) is det.

compute_map_delta_2([], _, Delta, Delta).
compute_map_delta_2([K - V | KVs], MapA, Delta0, Delta) :-
	( map__search(MapA, K, V) ->
		Delta1 = Delta0
	;
		map__det_insert(Delta0, K, V, Delta1)
	),
	compute_map_delta_2(KVs, MapA, Delta1, Delta).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__no_output_vars(_, unreachable, _, _, _).
instmap__no_output_vars(InstMap0, InstMapDelta, Vars, InstTable, ModuleInfo) :-
	InstMapDelta = reachable(_, _),
	set__to_sorted_list(Vars, VarList),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	instmap__no_output_vars_2(VarList, InstMap0, InstMap,
		InstTable, ModuleInfo).

:- pred instmap__no_output_vars_2(list(prog_var), instmap, instmap,
		inst_table, module_info).
:- mode instmap__no_output_vars_2(in, in, in, in, in) is semidet.

instmap__no_output_vars_2([], _, _, _, _).
instmap__no_output_vars_2([Var | Vars], InstMap0, InstMap,
		InstTable, ModuleInfo) :-
	% We use `inst_matches_binding' to check that the new inst
	% has only added information or lost uniqueness,
	% not bound anything.
	% If the instmap delta contains the variable, the variable may
	% still not be output, if the change is just an increase in
	% information rather than an increase in instantiatedness.
	% If the instmap delta doesn't contain the variable, it may still
	% have been (partially) output, if its inst is (or contains) `any'.
	instmap__lookup_var(InstMap0, Var, Inst0),
	instmap__lookup_var(InstMap, Var, Inst),
	inst_matches_binding(Inst0, InstMap0, Inst, InstMap, InstTable,
		ModuleInfo),
	instmap__no_output_vars_2(Vars, InstMap0, InstMap, InstTable,
		ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__bind_var_to_functor(Var, ConsId, InstMap0, InstMap,
		InstTable0, InstTable, ModuleInfo0, ModuleInfo) :-
	instmap__lookup_var(InstMap0, Var, Inst0),
	bind_inst_to_functor(Inst0, ConsId, Inst, InstTable0, ModuleInfo0,
			InstMap0, InstTable, ModuleInfo, InstMap1),
	instmap__set(InstMap1, Var, Inst, InstMap).

:- pred bind_inst_to_functor((inst), cons_id, (inst), inst_table, module_info,
		instmap, inst_table, module_info, instmap).
:- mode bind_inst_to_functor(in, in, out, in, in, in, out, out, out) is det.

bind_inst_to_functor(Inst0, ConsId, Inst, InstTable0, ModuleInfo0,
		InstMap0, InstTable, ModuleInfo, InstMap) :-
	( ConsId = cons(_, Arity) ->
		list__duplicate(Arity, dead, ArgLives),
		list__duplicate(Arity, free(unique), ArgInsts)
	;
		ArgLives = [],
		ArgInsts = []
	),
	(
		abstractly_unify_inst_functor(dead, Inst0, ConsId,
			ArgInsts, ArgLives, real_unify, InstTable0, ModuleInfo0,
			InstMap0, Inst1, _, InstTable1, ModuleInfo1, InstMap1)
	->
		ModuleInfo = ModuleInfo1,
		InstTable = InstTable1,
		Inst = Inst1,
		InstMap = InstMap1
	;
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		Inst = not_reached,
		InstMap = InstMap0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_delta_apply_sub(unreachable, _Must, _Sub, unreachable).
instmap_delta_apply_sub(reachable(OldInstMapping, Alias), Must, Sub,
		reachable(InstMapping, Alias)) :-
	map__to_assoc_list(OldInstMapping, InstMappingAL),
	map__init(InstMapping0),
	instmap_delta_apply_sub_2(InstMappingAL, Must, Sub,
		InstMapping0, InstMapping).

instmap__apply_sub(InstMap0, Must, Sub, InstMap) :-
	instmap_delta_apply_sub(InstMap0, Must, Sub, InstMap).

:- pred instmap_delta_apply_sub_2(assoc_list(prog_var, inst), bool,
		map(prog_var, prog_var), instmapping, instmapping).
:- mode instmap_delta_apply_sub_2(in, in, in, in, out) is det.

instmap_delta_apply_sub_2([], _Must, _Sub, IM, IM).
instmap_delta_apply_sub_2([V - I | AL], Must, Sub, IM0, IM) :-
	(
		map__search(Sub, V, N0)
	->
		N = N0
	;
		( Must = no,
			N = V
		; Must = yes,
			term__var_to_int(V, VInt),
			string__format(
			"instmap_delta_apply_sub_2: no substitute for var %i", 
				[i(VInt)], Msg),
			error(Msg)
		)
	),
	% XXX temporary hack alert XXX
	% this should be a call to to map__det_insert,
	% rather than a call to map__set.  However, if we
	% do that, then the compiler breaks, due to a problem
	% with excess.m not preserving super-homogenous form.
	map__set(IM0, N, I, IM1),
	instmap_delta_apply_sub_2(AL, Must, Sub, IM1, IM).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__to_assoc_list(unreachable, []).
instmap__to_assoc_list(reachable(InstMapping, Alias), AL) :-
	map__to_assoc_list(InstMapping, AL0),
	list__map(lambda([VarInst0 :: in, VarInst :: out] is det,
		(
			VarInst0 = Var - Inst0,
			inst_apply_sub(Alias, Inst0, Inst),
			VarInst  = Var - Inst
		)), AL0, AL).

instmap_delta_to_assoc_list(unreachable, []).
instmap_delta_to_assoc_list(reachable(InstMapping, _Alias), AL) :-
	map__to_assoc_list(InstMapping, AL).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__apply_alias_sub(unreachable, Inst, Inst).
instmap__apply_alias_sub(reachable(_, Alias), Inst0, Inst) :-
	inst_apply_sub(Alias, Inst0, Inst).

instmap__inst_key_table_lookup(unreachable, IKT, Key, Inst) :-
	inst_key_table_lookup(IKT, Key, Inst).
instmap__inst_key_table_lookup(reachable(_, Alias), IKT, Key0, Inst) :-
	find_latest_inst_key(Alias, Key0, Key),
	inst_key_table_lookup(IKT, Key, Inst0),
	inst_apply_sub(Alias, Inst0, Inst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__inst_keys_are_equivalent(KeyA, InstMapA, KeyB, InstMapB) :-
	(
		KeyA = KeyB
	;
		InstMapA = reachable(_, AliasMapA),
		InstMapB = reachable(_, AliasMapB),
		find_latest_inst_key(AliasMapA, KeyA, Key),
		find_latest_inst_key(AliasMapB, KeyB, Key)
	).

%-----------------------------------------------------------------------------%

instmap__get_inst_key_sub(unreachable, Sub) :-
	map__init(Sub).
instmap__get_inst_key_sub(reachable(_, Sub), Sub).

instmap_delta_get_inst_key_sub(IMD, Sub) :-
	instmap__get_inst_key_sub(IMD, Sub).
