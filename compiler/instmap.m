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
	%       Merge the `InstMaps' resulting from different branches
	%       of a disjunction or if-then-else, and update the
	%       instantiatedness of all the nonlocal variables,
	%       checking that it is the same for every branch.
	%
:- pred instmap__merge(set(prog_var), list(instmap),
		instmap, instmap, module_info, inst_table,
		module_info, inst_table, merge_errors).
:- mode instmap__merge(in, in, in, out, in, in, out, out, out) is det.

	% instmap_merge(NonLocalVars, InstMaps, MergeContext):
	%       Merge the `InstMaps' resulting from different branches
	%       of a disjunction or if-then-else, and update the
	%       instantiatedness of all the nonlocal variables,
	%       checking that it is the same for every branch.
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
	%       Unify the `InstMaps' in the list of pairs resulting
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

instmap__from_assoc_list(AL, reachable(Instmapping, AliasMap)) :-
	map__from_assoc_list(AL, Instmapping),
	map__init(AliasMap).

instmap_delta_from_assoc_list(AL, reachable(Instmapping, AliasMap)) :-
	map__from_assoc_list(AL, Instmapping),
	map__init(AliasMap).

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
	%       Merge the `InstMaps' resulting from different branches
	%       of a disjunction or if-then-else, and update the
	%       instantiatedness of all the nonlocal variables,
	%       checking that it is the same for every branch.

instmap__merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMapBefore),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	instmap__merge(NonLocals, InstMapList, InstMapBefore,
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

instmap__merge(NonLocals, InstMapList, InstMap0,
		InstMap, ModuleInfo0, InstTable0, ModuleInfo, InstTable,
		ErrorList) :-
	get_reachable_instmaps(InstMapList, InstMappingList),
	(
		InstMappingList = []
	->
		InstMap = unreachable,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		ErrorList = []
	;
		InstMap0 = reachable(_, _),
		InstMappingList = [InstMapping - AliasMap]
	->
		instmap__restrict(reachable(InstMapping, AliasMap), NonLocals,
				InstMap),
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		ErrorList = []
	;
		InstMap0 = reachable(InstMapping0, _AliasMap0)
	->
		set__to_sorted_list(NonLocals, NonLocalsList),
		instmap__merge_2(NonLocalsList, InstMapList, InstTable0,
			ModuleInfo0, InstMapping0, InstTable, ModuleInfo,
			InstMapping, ErrorList),
		map__init(AliasMap),
		InstMap = reachable(InstMapping, AliasMap)
	;
		InstMap = unreachable,
		ModuleInfo = ModuleInfo0,
		InstTable = InstTable0,
		ErrorList = []
	).

:- pred get_reachable_instmaps(list(instmap),
		list(pair(map(prog_var, inst), inst_key_sub))).
:- mode get_reachable_instmaps(in, out) is det.

get_reachable_instmaps([], []).
get_reachable_instmaps([InstMap | InstMaps], Reachables) :-
	( InstMap = reachable(InstMapping, AliasMap) ->
		Reachables = [InstMapping - AliasMap | Reachables1],
		get_reachable_instmaps(InstMaps, Reachables1)
	;
		get_reachable_instmaps(InstMaps, Reachables)
	).

%-----------------------------------------------------------------------------%

	% instmap__get_relevant_inst_keys(Vars, InstMaps, InstTable, SeenKeys,
	%		DuplicateKeys, InstKeys):
	%	Return a set of all inst_keys which appear more than
	%	once in the instmaps.

:- pred instmap__get_relevant_inst_keys(list(prog_var), list(instmap),
		module_info, inst_table, set_bbbtree(inst_key)).
:- mode instmap__get_relevant_inst_keys(in, in, in, in, out) is det.

instmap__get_relevant_inst_keys(Vars, InstMaps, ModuleInfo, InstTable,
		RelevantIKs) :-
	set_bbbtree__init(Seen0),
	set_bbbtree__init(Duplicate0),
	list__foldl2(instmap__get_relevant_inst_keys_2(Vars, ModuleInfo,
				InstTable),
		InstMaps, Seen0, _Seen, Duplicate0, Duplicate),
	RelevantIKs = Duplicate.

:- pred instmap__get_relevant_inst_keys_2(list(prog_var), module_info,
		inst_table, instmap, set_bbbtree(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key),
		set_bbbtree(inst_key)).
:- mode instmap__get_relevant_inst_keys_2(in, in, in, in,
		in, out, in, out) is det.

instmap__get_relevant_inst_keys_2([], _InstTable, _ModuleInfo, _InstMap,
		Seen, Seen, Duplicate, Duplicate).
instmap__get_relevant_inst_keys_2([V | Vs], ModuleInfo, InstTable, InstMap,
		Seen0, Seen, Duplicate0, Duplicate) :-
	instmap__lookup_var(InstMap, V, Inst),
	set_bbbtree__init(Recursive),
	instmap__get_relevant_inst_keys_in_inst(Inst, Recursive, ModuleInfo,
		InstTable, Seen0, Seen1, Duplicate0, Duplicate1),
	instmap__get_relevant_inst_keys_2(Vs, ModuleInfo, InstTable, InstMap,
		Seen1, Seen, Duplicate1, Duplicate).

:- pred instmap__get_relevant_inst_keys_in_inst(inst, set_bbbtree(inst_name),
		module_info, inst_table, set_bbbtree(inst_key),
		set_bbbtree(inst_key), set_bbbtree(inst_key),
		set_bbbtree(inst_key)).
:- mode instmap__get_relevant_inst_keys_in_inst(in, in, in, in,
		in, out, in, out) is det.

instmap__get_relevant_inst_keys_in_inst(any(_), _, _, _, S, S, D, D).
instmap__get_relevant_inst_keys_in_inst(alias(Key), Recursive, ModuleInfo,
		InstTable, S0, S, D0, D) :-
	inst_table_get_inst_key_table(InstTable, IKT),
	inst_key_table_lookup(IKT, Key, Inst),
	( set_bbbtree__member(Key, S0) ->
		set_bbbtree__insert(D0, Key, D1),
		S1 = S0
	;
		set_bbbtree__insert(S0, Key, S1),
		D1 = D0
	),
	instmap__get_relevant_inst_keys_in_inst(Inst, Recursive, ModuleInfo,
		InstTable, S1, S, D1, D).
instmap__get_relevant_inst_keys_in_inst(free(_), _, _, _, S, S, D, D).
instmap__get_relevant_inst_keys_in_inst(free(_, _), _, _, _, S, S, D, D).
instmap__get_relevant_inst_keys_in_inst(bound(_, BoundInsts), Rec, ModuleInfo,
		InstTable, S0, S, D0, D) :-
	list__foldl2(lambda([BoundInst :: in, AS0 :: in, AS :: out,
				AD0 :: in, AD :: out] is det,
			( BoundInst = functor(_, Insts),
			  list__foldl2(lambda([Inst :: in, BS0 :: in, BS :: out,
						BD0 :: in, BD :: out] is det,
				instmap__get_relevant_inst_keys_in_inst(Inst,
					Rec, ModuleInfo, InstTable,
					BS0, BS, BD0, BD)),
				Insts, AS0, AS, AD0, AD)
			)
		), BoundInsts, S0, S, D0, D).
instmap__get_relevant_inst_keys_in_inst(ground(_, _), _, _, _, S, S, D, D).
instmap__get_relevant_inst_keys_in_inst(not_reached, _, _, _, _, _, _, _) :-
	error("instmap__get_relevant_inst_keys_in_inst: not_reached").
instmap__get_relevant_inst_keys_in_inst(inst_var(_), _, _, _, _, _, _, _) :-
	error("instmap__get_relevant_inst_keys_in_inst: inst_var").
instmap__get_relevant_inst_keys_in_inst(defined_inst(InstName), Recursive0,
		ModuleInfo, InstTable, S0, S, D0, D) :-
	% This is tricky, because an inst_key is "relevant" if it
	% appears only once in an inst which is recursive.  (If we
	% were to unfold the inst, it would appear multiple times.)
	( set_bbbtree__member(InstName, Recursive0) ->
		set_bbbtree__union(S0, D0, D),
		S = S0
	;
		set_bbbtree__insert(Recursive0, InstName, Recursive),
		set_bbbtree__init(NewS0),
		inst_lookup(InstTable, ModuleInfo, InstName, Inst),
		instmap__get_relevant_inst_keys_in_inst(Inst, Recursive,
			ModuleInfo, InstTable, NewS0, NewS, D0, D1),
		set_bbbtree__intersect(NewS, S0, NewD),
		set_bbbtree__union(NewD, D1, D),
		set_bbbtree__union(NewS, S0, S)
	).
instmap__get_relevant_inst_keys_in_inst(abstract_inst(_, Insts), Rec,
		ModuleInfo, InstTable, S0, S, D0, D) :-
	list__foldl2(lambda([Inst :: in, AS0 :: in, AS :: out,
				AD0 :: in, AD :: out] is det,
			instmap__get_relevant_inst_keys_in_inst(Inst,
				Rec, ModuleInfo, InstTable, AS0, AS, AD0, AD)),
		Insts, S0, S, D0, D).

%-----------------------------------------------------------------------------%

	% instmap__merge_2(Vars, InstMaps, ModuleInfo, ErrorList):
	%       Let `ErrorList' be the list of variables in `Vars' for
	%       there are two instmaps in `InstMaps' for which the inst
	%       the variable is incompatible.

:- pred instmap__merge_2(list(prog_var), list(instmap), inst_table, module_info,
			map(prog_var, inst), inst_table, module_info,
			map(prog_var, inst), merge_errors).
:- mode instmap__merge_2(in, in, in, in, in, out, out, out, out) is det.

instmap__merge_2([], _, InstTable, ModuleInfo, InstMap, InstTable, ModuleInfo,
			InstMap, []).
instmap__merge_2([Var|Vars], InstMapList, InstTable0, ModuleInfo0, InstMap0,
			InstTable, ModuleInfo, InstMap, ErrorList) :-
	instmap__merge_2(Vars, InstMapList, InstTable0, ModuleInfo0, InstMap0,
			InstTable1, ModuleInfo1, InstMap1, ErrorList1),
	instmap__merge_var(InstMapList, Var, InstTable1, ModuleInfo1,
			Insts, Inst, InstTable, ModuleInfo, Error),
	( Error = yes ->
		ErrorList = [Var - Insts | ErrorList1],
		map__set(InstMap1, Var, not_reached, InstMap)
	;
		ErrorList = ErrorList1,
		map__set(InstMap1, Var, Inst, InstMap)
	).

	% instmap_merge_var(InstMaps, Var, ModuleInfo, Insts, Error):
	%       Let `Insts' be the list of the inst of `Var' in the
	%       corresponding `InstMaps'.  Let `Error' be yes iff
	%       there are two instmaps for which the inst of `Var'
	%       is incompatible.

:- pred instmap__merge_var(list(instmap), prog_var, inst_table, module_info,
			list(inst), inst, inst_table, module_info, bool).
:- mode instmap__merge_var(in, in, in, in, out, out, out, out, out) is det.

instmap__merge_var([], _, InstTable, ModuleInfo, [],
		not_reached, InstTable, ModuleInfo, no).
instmap__merge_var([InstMap | InstMaps], Var, InstTable0, ModuleInfo0,
		InstList, Inst, InstTable, ModuleInfo, Error) :-
	instmap__merge_var(InstMaps, Var, InstTable0, ModuleInfo0,
		InstList0, Inst0, InstTable1, ModuleInfo1, Error0),
	instmap__lookup_var(InstMap, Var, VarInst0),
	inst_table_get_inst_key_table(InstTable1, IKT1),
	inst_expand_fully(IKT1, InstMap, VarInst0, VarInst),
	InstList = [VarInst | InstList0],
	(
		% YYY Not sure about the returned InstMap here.
		inst_merge(Inst0, VarInst, InstMap, InstTable1, ModuleInfo1,
			Inst1, _, InstTable2, ModuleInfo2)
	->
		Inst = Inst1,
		ModuleInfo = ModuleInfo2,
		Error = Error0,
		InstTable = InstTable2
	;
		Error = yes,
		ModuleInfo = ModuleInfo1,
		Inst = not_reached,
		InstTable = InstTable1
	).

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
% 	%       Let `Insts' be the list of the inst of `Var' in
% 	%       each of the corresponding `InstMaps'.  Let `Error' be yes
% 	%	iff there are two instmaps for which the inst of `Var'
% 	%       is incompatible.
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

:- pred merge_instmapping_delta(instmap, set(prog_var), instmapping,
		instmapping, instmapping, inst_table, inst_table,
		module_info, module_info).
:- mode merge_instmapping_delta(in, in, in, in, out, in, out, in, out) is det.

merge_instmapping_delta(InstMap, NonLocals, InstMappingA,
		InstMappingB, InstMapping, InstTable0, InstTable) -->
	{ map__keys(InstMappingA, VarsInA) },
	{ map__keys(InstMappingB, VarsInB) },
	{ set__sorted_list_to_set(VarsInA, SetofVarsInA) },
	{ set__insert_list(SetofVarsInA, VarsInB, SetofVars0) },
	{ set__intersect(SetofVars0, NonLocals, SetofVars) },
	{ map__init(InstMapping0) },
	{ set__to_sorted_list(SetofVars, ListofVars) },
	merge_instmapping_delta_2(ListofVars, InstMap, InstMappingA,
		InstMappingB, InstMapping0, InstMapping, InstTable0, InstTable).

:- pred merge_instmapping_delta_2(list(prog_var), instmap, instmapping,
		instmapping, instmapping, instmapping, inst_table, inst_table,
		module_info, module_info).
:- mode merge_instmapping_delta_2(in, in, in, in, in, out, in, out, in, out)
		is det.

merge_instmapping_delta_2([], _, _, _, InstMapping, InstMapping,
			InstTable, InstTable, ModInfo, ModInfo).
merge_instmapping_delta_2([Var | Vars], InstMap, InstMappingA, InstMappingB,
			InstMapping0, InstMapping, InstTable0, InstTable,
			ModuleInfo0, ModuleInfo) :-
	( map__search(InstMappingA, Var, InstInA) ->
		InstA = InstInA
	;
		instmap__lookup_var(InstMap, Var, InstA)
	),
	( map__search(InstMappingB, Var, InstInB) ->
		InstB = InstInB
	;
		instmap__lookup_var(InstMap, Var, InstB)
	),
	(
		% YYY Not sure about the returned InstMap here.
		inst_merge(InstA, InstB, InstMap, InstTable0, ModuleInfo0,
			Inst, _, InstTable1, ModuleInfo1)
	->
		ModuleInfo2 = ModuleInfo1,
		InstTable2 = InstTable1,
		map__det_insert(InstMapping0, Var, Inst, InstMapping1)
	;
		term__var_to_int(Var, VarInt),
		string__format(
			"merge_instmapping_delta_2: error merging var %i",
			[i(VarInt)], Msg),
		error(Msg)
	),
	merge_instmapping_delta_2(Vars, InstMap, InstMappingA, InstMappingB,
		InstMapping1, InstMapping, InstTable2, InstTable,
		ModuleInfo2, ModuleInfo).

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
			error("instmap_delta_apply_sub_2: no substitute")
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

instmap__get_inst_key_sub(unreachable, Sub) :-
	map__init(Sub).
instmap__get_inst_key_sub(reachable(_, Sub), Sub).

instmap_delta_get_inst_key_sub(IMD, Sub) :-
	instmap__get_inst_key_sub(IMD, Sub).
