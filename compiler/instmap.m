%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1997 The University of Melbourne.
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
:- import_module hlds_module, prog_data, mode_info, (inst).
:- import_module set, term.

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

:- pred instmap__from_assoc_list(assoc_list(var, inst), instmap).
:- mode instmap__from_assoc_list(in, out) is det.

:- pred instmap_delta_from_assoc_list(assoc_list(var, inst), instmap_delta).
:- mode instmap_delta_from_assoc_list(in, out) is det.

:- pred instmap_delta_from_mode_list(list(var), list(mode),
		module_info, instmap_delta).
:- mode instmap_delta_from_mode_list(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Return the set of variables in an instmap.
	%
:- pred instmap__vars(instmap, set(var)).
:- mode instmap__vars(in, out) is det.

	% Return the list of variables in an instmap.
	%
:- pred instmap__vars_list(instmap, list(var)).
:- mode instmap__vars_list(in, out) is det.

	% Return the set of variables whose instantiations have
	% changed (or our knowledge about them has changed) across
	% an instmap_delta.
	%
:- pred instmap_delta_changed_vars(instmap_delta, set(var)).
:- mode instmap_delta_changed_vars(in, out) is det.

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.
	%
:- pred instmap__lookup_var(instmap, var, inst).
:- mode instmap__lookup_var(in, in, out) is det.

	% Given an instmap_delta and a variable, determine the new inst
	% of that variable (if any).
	%
:- pred instmap_delta_search_var(instmap_delta, var, inst).
:- mode instmap_delta_search_var(in, in, out) is semidet.

	% Given an instmap and a list of variables, return a list
	% containing the insts of those variable.
	%
:- pred instmap__lookup_vars(list(var), instmap, list(inst)).
:- mode instmap__lookup_vars(in, in, out) is det.

	% Given an instmap and an inst_key, find those vars which
	% depend on that inst_key.
:- pred instmap__lookup_dependent_vars(instmap, inst_key, list(var)).
:- mode instmap__lookup_dependent_vars(in, in, out) is det.

	% Set an entry in an instmap.
	%
:- pred instmap__set(instmap, var, inst, instmap).
:- mode instmap__set(in, in, in, out) is det.

:- pred instmap_delta_set(instmap_delta, var, inst, instmap_delta).
:- mode instmap_delta_set(in, in, in, out) is det.

	% Update the given instmap to include the initial insts of the
	% lambda variables.
:- pred instmap__pre_lambda_update(module_info, list(var), argument_modes,
	instmap_delta, inst_table, inst_table, instmap, instmap).
:- mode instmap__pre_lambda_update(in, in, in, out, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.
	%
:- pred compute_instmap_delta(instmap, instmap, set(var), instmap_delta).
:- mode compute_instmap_delta(in, in, in, out) is det.

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
:- pred instmap__merge(set(var), list(instmap), merge_context,
		mode_info, mode_info).
:- mode instmap__merge(in, in, in, mode_info_di, mode_info_uo) is det.

	% instmap__restrict takes an instmap and a set of vars and
	% returns an instmap with its domain restricted to those
	% vars.
	%
:- pred instmap__restrict(instmap, set(var), instmap).
:- mode instmap__restrict(in, in, out) is det.

	% instmap_delta_restrict takes an instmap and a set of vars
	% and returns an instmap_delta with its domain restricted to
	% those vars.
	%
:- pred instmap_delta_restrict(instmap_delta, set(var), instmap_delta).
:- mode instmap_delta_restrict(in, in, out) is det.

	% instmap_delta_delete_vars takes an instmap_delta and a list of
	% vars and returns an instmap_delta with those vars removed from
	% its domain.
	%
:- pred instmap_delta_delete_vars(instmap_delta, list(var), instmap_delta).
:- mode instmap_delta_delete_vars(in, in, out) is det.

	% `instmap__no_output_vars(Instmap, InstmapDelta, Vars, InstTable,
	%  ModuleInfo)'
	% is true if none of the vars in the set Vars could have become more
	% instantiated when InstmapDelta is applied to Instmap.
:- pred instmap__no_output_vars(instmap, instmap_delta, set(var),
		inst_table, module_info).
:- mode instmap__no_output_vars(in, in, in, in, in) is semidet.

	% merge_instmap_delta(InitialInstMap, NonLocals,
	%	InstMapDeltaA, InstMapDeltaB, ModuleInfo0, ModuleInfo)
	% Merge the instmap_deltas of different branches of an ite, disj
	% or switch.
:- pred merge_instmap_delta(instmap, set(var), instmap_delta, instmap_delta,
		instmap_delta, inst_table, inst_table,
		module_info, module_info).
:- mode merge_instmap_delta(in, in, in, in, out, in, out, in, out) is det.

	% merge_instmap_deltas(Vars, InstMapDeltas, MergedInstMapDelta,
	%	InstTable0, InstTable1, ModuleInfo0, ModuleInfo)
	% takes a list of instmap deltas from the branches of an if-then-else,
	% switch, or disj and merges them. This is used in situations
	% where the bindings are known to be compatible.
:- pred merge_instmap_deltas(instmap, set(var), list(instmap_delta),
		instmap_delta, inst_table, inst_table,
		module_info, module_info).
:- mode merge_instmap_deltas(in, in, in, out, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

	% `instmap_delta_apply_sub(InstmapDelta0, Must, Sub, InstmapDelta)'
	% the variable substitution Sub to InstmapDelta0 to get the new
	% instmap_delta InstmapDelta.  If there is a variable in
	% InstmapDelta0 which does not appear in Sub, it is ignored if
	% Must is set to no, otherwise it is an error.
	%
:- pred instmap_delta_apply_sub(instmap_delta, bool, map(var, var),
		instmap_delta).
:- mode instmap_delta_apply_sub(in, in, in, out) is det.

:- pred instmap__apply_sub(instmap, bool, map(var, var), instmap).
:- mode instmap__apply_sub(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred instmap__to_assoc_list(instmap, assoc_list(var, inst)).
:- mode instmap__to_assoc_list(in, out) is det.

:- pred instmap_delta_to_assoc_list(instmap_delta, assoc_list(var, inst)).
:- mode instmap_delta_to_assoc_list(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_util, inst_match, prog_data, mode_errors, goal_util.
:- import_module mercury_to_mercury, hlds_data, hlds_module, inst_util.
:- import_module std_util, bool, map, set, assoc_list, require, multi_map.
:- import_module int, list, varset.

:- type instmap_delta	==	instmap.

:- type instmap	--->
		reachable(instmapping, bwd_mapping)
	;	unreachable.

:- type instmapping	==	map(var, inst).
:- type bwd_mapping	==	multi_map(inst_key, var).

%-----------------------------------------------------------------------------%

	% Initialize an empty instmap and instmap_delta.

instmap__init_reachable(reachable(InstMapping, BwdMap)) :-
	map__init(InstMapping),
	multi_map__init(BwdMap).

instmap__init_unreachable(unreachable).

instmap_delta_init_reachable(reachable(InstMapping, BwdMap)) :-
	map__init(InstMapping),
	multi_map__init(BwdMap).

instmap_delta_init_unreachable(unreachable).

%-----------------------------------------------------------------------------%

instmap__is_reachable(reachable(_, _)).

instmap__is_unreachable(unreachable).

instmap_delta_is_reachable(reachable(_, _)).

instmap_delta_is_unreachable(unreachable).

%-----------------------------------------------------------------------------%

:- pred bwd_mapping_from_instmapping(instmapping :: in,
		bwd_mapping :: in, bwd_mapping :: out) is det.

bwd_mapping_from_instmapping(InstMapping, Bwd0, Bwd) :-
	map__to_assoc_list(InstMapping, AssocList),
	bwd_mapping_from_assoc_list(AssocList, Bwd0, Bwd).

:- pred bwd_mapping_from_assoc_list(assoc_list(var, inst) :: in,
		bwd_mapping :: in, bwd_mapping :: out) is det.

bwd_mapping_from_assoc_list([], Bwd, Bwd).
bwd_mapping_from_assoc_list([Var - Inst | AL], Bwd0, Bwd) :-
	inst_keys_in_inst(Inst, [], Keys),
	add_backward_dependencies(Keys, Var, Bwd0, Bwd1),
	bwd_mapping_from_assoc_list(AL, Bwd1, Bwd).

instmap__from_assoc_list(AL, reachable(Instmapping, BwdMap)) :-
	map__from_assoc_list(AL, Instmapping),
	multi_map__init(BwdMap0),
	bwd_mapping_from_assoc_list(AL, BwdMap0, BwdMap).

instmap_delta_from_assoc_list(AL, reachable(Instmapping, BwdMap)) :-
	map__from_assoc_list(AL, Instmapping),
	multi_map__init(BwdMap0),
	bwd_mapping_from_assoc_list(AL, BwdMap0, BwdMap).

%-----------------------------------------------------------------------------%

instmap_delta_from_mode_list(Vars, Modes, ModuleInfo, InstMapDelta) :-
	instmap_delta_init_reachable(InstMapDelta0),
	instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo,
		InstMapDelta0, InstMapDelta).
	
:- pred instmap_delta_from_mode_list_2(list(var), list(mode),
		module_info, instmap_delta, instmap_delta).
:- mode instmap_delta_from_mode_list_2(in, in, in, in, out) is det.

instmap_delta_from_mode_list_2([], [], _, InstMapDelta, InstMapDelta).
instmap_delta_from_mode_list_2([], [_|_], _, _, _) :-
	error("instmap_delta_from_mode_list_2").
instmap_delta_from_mode_list_2([_|_], [], _, _, _) :-
	error("instmap_delta_from_mode_list_2").
instmap_delta_from_mode_list_2([Var | Vars], [Mode | Modes], ModuleInfo,
		InstMapDelta0, InstMapDelta) :-
	mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
	( Inst1 = Inst2 ->
		instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo,
			InstMapDelta0, InstMapDelta)
	;
		instmap_delta_set(InstMapDelta0, Var, Inst2, InstMapDelta1),
		instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo,
			InstMapDelta1, InstMapDelta)
	).

%-----------------------------------------------------------------------------%

instmap__vars(Instmap, Vars) :-
	instmap__vars_list(Instmap, VarsList),
	set__list_to_set(VarsList, Vars).

instmap__vars_list(unreachable, []).
instmap__vars_list(reachable(InstMapping, _), VarsList) :-
	map__keys(InstMapping, VarsList).

instmap_delta_changed_vars(unreachable, EmptySet) :-
	set__init(EmptySet).
instmap_delta_changed_vars(reachable(InstMapping, _), ChangedVars) :-
	map__keys(InstMapping, ChangedVarsList),
	set__sorted_list_to_set(ChangedVarsList, ChangedVars).

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.

instmap__lookup_var(unreachable, _Var, not_reached).
instmap__lookup_var(reachable(InstMap, _), Var, Inst) :-
	instmapping_lookup_var(InstMap, Var, Inst).

:- pred instmapping_lookup_var(instmapping, var, inst).
:- mode instmapping_lookup_var(in, in, out) is det.

instmapping_lookup_var(InstMap, Var, Inst) :-
	( map__search(InstMap, Var, VarInst) ->
		Inst = VarInst
	;
		Inst = free
	).

instmap_delta_search_var(unreachable, _, not_reached).
instmap_delta_search_var(reachable(InstmapDelta, _), Var, Inst) :-
	map__search(InstmapDelta, Var, Inst).

instmap__lookup_vars([], _InstMap, []).
instmap__lookup_vars([Arg|Args], InstMap, [Inst|Insts]) :-
	instmap__lookup_var(InstMap, Arg, Inst),
	instmap__lookup_vars(Args, InstMap, Insts).

instmap__lookup_dependent_vars(unreachable, _InstKey, []).
instmap__lookup_dependent_vars(reachable(_FwdMap, BwdMap), InstKey, Vars) :-
	( map__search(BwdMap, InstKey, Vars0) ->
		Vars = Vars0
	;
		Vars = []
	).

instmap__set(unreachable, _Var, _Inst, unreachable).
instmap__set(reachable(InstMapping0, BwdMap0), Var, Inst, Instmap) :-
	(
		Inst = not_reached
	->
		Instmap = unreachable
	;
		inst_keys_in_inst(Inst, [], Keys),
		add_backward_dependencies(Keys, Var, BwdMap0, BwdMap),
		map__set(InstMapping0, Var, Inst, InstMapping),
		Instmap = reachable(InstMapping, BwdMap)
	).

instmap_delta_set(unreachable, _Var, _Inst, unreachable).
instmap_delta_set(reachable(InstMapping0, BwdMap0), Var, Inst, Instmap) :-
	(
		Inst = not_reached
	->
		Instmap = unreachable
	;
        	inst_keys_in_inst(Inst, [], Keys),
        	add_backward_dependencies(Keys, Var, BwdMap0, BwdMap),
		map__set(InstMapping0, Var, Inst, InstMapping),
		Instmap = reachable(InstMapping, BwdMap)
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
instmap__apply_instmap_delta(reachable(InstMapping0, BwdMap0),
		reachable(InstMappingDelta, BwdMapDelta),
		reachable(InstMapping, BwdMap)) :-
	map__overlay(InstMapping0, InstMappingDelta, InstMapping),
	map__overlay(BwdMap0, BwdMapDelta, BwdMap).

	% Given two instmap_deltas, overlay the entries in the second
	% instmap_delta on top of those in the first to produce a new
	% instmap_delta.

instmap_delta_apply_instmap_delta(unreachable, _, unreachable).
instmap_delta_apply_instmap_delta(reachable(_, _), unreachable, unreachable).
instmap_delta_apply_instmap_delta(reachable(InstMappingDelta0, BwdMap0),
		reachable(InstMappingDelta1, BwdMap1),
		reachable(InstMappingDelta, BwdMap)) :-
	map__overlay(InstMappingDelta0, InstMappingDelta1, InstMappingDelta),
	map__overlay(BwdMap0, BwdMap1, BwdMap).

%-----------------------------------------------------------------------------%

instmap__restrict(unreachable, _, unreachable).
instmap__restrict(reachable(InstMapping0, _), Vars,
		reachable(InstMapping, BwdMap)) :-
	map__select(InstMapping0, Vars, InstMapping),
	map__init(BwdMap0),
	bwd_mapping_from_instmapping(InstMapping, BwdMap0, BwdMap).

instmap_delta_restrict(unreachable, _, unreachable).
instmap_delta_restrict(reachable(InstMapping0, _), Vars,
		reachable(InstMapping, BwdMap)) :-
	map__select(InstMapping0, Vars, InstMapping),
	map__init(BwdMap0),
	bwd_mapping_from_instmapping(InstMapping, BwdMap0, BwdMap).

instmap_delta_delete_vars(unreachable, _, unreachable).
instmap_delta_delete_vars(reachable(InstMapping0, _), Vars,
		reachable(InstMapping, BwdMap)) :-
	map__delete_list(InstMapping0, Vars, InstMapping),
	map__init(BwdMap0),
	bwd_mapping_from_instmapping(InstMapping, BwdMap0, BwdMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% instmap__merge(NonLocalVars, InstMaps, MergeContext):
	%       Merge the `InstMaps' resulting from different branches
	%       of a disjunction or if-then-else, and update the
	%       instantiatedness of all the nonlocal variables,
	%       checking that it is the same for every branch.

instmap__merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	mode_info_get_inst_table(ModeInfo0, InstTable0),
	get_reachable_instmaps(InstMapList, InstMappingList),
	(
		InstMappingList = []
	->
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0,
		InstTable = InstTable0
	;
%		InstMap0 = reachable(_, _),
%		InstMappingList = [InstMapping]
%	->
%		map__init(BwdMap0),
%		bwd_mapping_from_instmapping(InstMapping, BwdMap0, BwdMap),
%		InstMap = reachable(InstMapping, BwdMap),
%		ModeInfo2 = ModeInfo0,
%		InstTable = InstTable0
%	;
		InstMap0 = reachable(InstMapping0, _BwdMap0)
	->
		set__to_sorted_list(NonLocals, NonLocalsList),
		instmap__merge_2(NonLocalsList, InstMapList, InstTable0,
			ModuleInfo0, InstMapping0, InstTable, ModuleInfo,
			InstMapping, ErrorList),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
		( ErrorList = [FirstError | _] ->
			FirstError = Var - _,
			set__singleton_set(WaitingVars, Var),
			mode_info_error(WaitingVars,
				mode_error_disj(MergeContext, ErrorList),
				ModeInfo1, ModeInfo2
			)
		;
			ModeInfo2 = ModeInfo1
		),
		map__init(BwdMap0),
		bwd_mapping_from_instmapping(InstMapping, BwdMap0, BwdMap),
		InstMap = reachable(InstMapping, BwdMap)
	;
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0,
		InstTable = InstTable0
	),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo3),
	mode_info_set_inst_table(InstTable, ModeInfo3, ModeInfo).

:- pred get_reachable_instmaps(list(instmap), list(map(var,inst))).
:- mode get_reachable_instmaps(in, out) is det.

get_reachable_instmaps([], []).
get_reachable_instmaps([InstMap | InstMaps], Reachables) :-
	( InstMap = reachable(InstMapping, _BwdMap) ->
		Reachables = [InstMapping | Reachables1],
		get_reachable_instmaps(InstMaps, Reachables1)
	;
		get_reachable_instmaps(InstMaps, Reachables)
	).

%-----------------------------------------------------------------------------%

	% instmap__merge_2(Vars, InstMaps, ModuleInfo, ErrorList):
	%       Let `ErrorList' be the list of variables in `Vars' for
	%       there are two instmaps in `InstMaps' for which the inst
	%       the variable is incompatible.

:- pred instmap__merge_2(list(var), list(instmap), inst_table, module_info,
			map(var, inst), inst_table, module_info,
			map(var, inst), merge_errors).
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

:- pred instmap__merge_var(list(instmap), var, inst_table, module_info,
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
	inst_expand_fully(IKT1, VarInst0, VarInst),
	InstList = [VarInst | InstList0],
	( inst_merge(Inst0, VarInst, InstTable1, ModuleInfo1,
			Inst1, InstTable2, ModuleInfo2) ->
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

merge_instmap_deltas(InstMap, NonLocals, InstMapDeltaList, MergedDelta,
		InstTable0, InstTable, ModuleInfo0, ModuleInfo) :-
	(
		InstMapDeltaList = [],
		error("merge_instmap_deltas: empty instmap_delta list.")
	;
		InstMapDeltaList = [Delta|Deltas],
		merge_instmap_deltas(InstMap, NonLocals, Delta, Deltas,
			MergedDelta, InstTable0, InstTable,
			ModuleInfo0, ModuleInfo)
	).

:- pred merge_instmap_deltas(instmap, set(var), instmap_delta,
		list(instmap_delta), instmap_delta, inst_table,
		inst_table, module_info, module_info).
:- mode merge_instmap_deltas(in, in, in, in, out, in, out, in, out) is det.

merge_instmap_deltas(_InstMap, _NonLocals, MergedDelta, [], MergedDelta,
		InstTable, InstTable, ModuleInfo, ModuleInfo).
merge_instmap_deltas(InstMap, NonLocals, MergedDelta0, [Delta|Deltas],
		MergedDelta, InstTable0, InstTable, ModuleInfo0, ModuleInfo) :-
	merge_instmap_delta(InstMap, NonLocals, MergedDelta0, Delta,
		MergedDelta1, InstTable0, InstTable1, ModuleInfo0, ModuleInfo1),
	merge_instmap_deltas(InstMap, NonLocals, MergedDelta1, Deltas,
		MergedDelta, InstTable1, InstTable, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.

compute_instmap_delta(unreachable, _, _, unreachable).
compute_instmap_delta(reachable(_, _), unreachable, _, unreachable).
compute_instmap_delta(reachable(InstMapA, _BwdA), reachable(InstMapB, _BwdB),
		NonLocals, reachable(DeltaInstMap, BwdMap)) :-
	set__to_sorted_list(NonLocals, NonLocalsList),
	compute_instmap_delta_2(NonLocalsList, InstMapA, InstMapB, AssocList),
	map__from_sorted_assoc_list(AssocList, DeltaInstMap),
	map__init(BwdMap0),
	bwd_mapping_from_assoc_list(AssocList, BwdMap0, BwdMap).

:- pred compute_instmap_delta_2(list(var), instmapping, instmapping,
					assoc_list(var, inst)).
:- mode compute_instmap_delta_2(in, in, in, out) is det.

compute_instmap_delta_2([], _, _, []).
compute_instmap_delta_2([Var | Vars], InstMapA, InstMapB, AssocList) :-
	instmapping_lookup_var(InstMapA, Var, InstA),
	instmapping_lookup_var(InstMapB, Var, InstB),
	( InstA = InstB ->
		AssocList1 = AssocList
	;
		AssocList = [ Var - InstB | AssocList1 ]
	),
	compute_instmap_delta_2(Vars, InstMapA, InstMapB, AssocList1).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__no_output_vars(_, unreachable, _, _, _).
instmap__no_output_vars(InstMap0, reachable(InstMapDelta, _BwdMap), Vars,
		InstTable, ModuleInfo) :-
	set__to_sorted_list(Vars, VarList),
	instmap__no_output_vars_2(VarList, InstMap0, InstMapDelta,
		InstTable, ModuleInfo).

:- pred instmap__no_output_vars_2(list(var), instmap, instmapping,
		inst_table, module_info).
:- mode instmap__no_output_vars_2(in, in, in, in, in) is semidet.

instmap__no_output_vars_2([], _, _, _, _).
instmap__no_output_vars_2([Var | Vars], InstMap0, InstMapDelta,
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
	( map__search(InstMapDelta, Var, Inst1) ->
		Inst = Inst1
	;
		Inst = Inst0
	),
	inst_matches_binding(Inst, Inst0, InstTable, ModuleInfo),
	instmap__no_output_vars_2(Vars, InstMap0, InstMapDelta, InstTable,
		ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given two instmap deltas, merge them to produce a new instmap_delta.

merge_instmap_delta(_, _, unreachable, InstMap, InstMap,
			InstTable, InstTable) --> [].
merge_instmap_delta(_, _, reachable(InstMapping, BwdMap), unreachable,
			reachable(InstMapping, BwdMap),
			InstTable, InstTable) --> [].
merge_instmap_delta(InstMap, NonLocals, reachable(InstMappingA, _BwdA),
		reachable(InstMappingB, _BwdB), reachable(InstMapping, Bwd),
		InstTable0, InstTable) -->
	merge_instmapping_delta(InstMap, NonLocals, InstMappingA, InstMappingB,
		InstMapping, InstTable0, InstTable),
	{ map__init(Bwd0) },
	{ bwd_mapping_from_instmapping(InstMapping, Bwd0, Bwd) }.

:- pred merge_instmapping_delta(instmap, set(var), instmapping, instmapping,
		instmapping, inst_table, inst_table, module_info, module_info).
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

:- pred merge_instmapping_delta_2(list(var), instmap, instmapping, instmapping,
		instmapping, instmapping, inst_table, inst_table,
		module_info, module_info).
:- mode merge_instmapping_delta_2(in, in, in, in,
		in, out, in, out, in, out) is det.

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
		inst_merge(InstA, InstB, InstTable0, ModuleInfo0,
			Inst, InstTable1, ModuleInfo1)
	->
		ModuleInfo2 = ModuleInfo1,
		InstTable2 = InstTable1,
		map__det_insert(InstMapping0, Var, Inst, InstMapping1)
	;
		error("merge_instmapping_delta_2: unexpected mode error")
	),
	merge_instmapping_delta_2(Vars, InstMap, InstMappingA, InstMappingB,
		InstMapping1, InstMapping, InstTable2, InstTable,
		ModuleInfo2, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_delta_apply_sub(unreachable, _Must, _Sub, unreachable).
instmap_delta_apply_sub(reachable(OldInstMapping, OldBwd), Must, Sub,
		reachable(InstMapping, Bwd)) :-
	map__to_assoc_list(OldInstMapping, InstMappingAL),
	map__init(InstMapping0),
	instmap_delta_apply_sub_2(InstMappingAL, Must, Sub,
		InstMapping0, InstMapping),
	multi_map__to_assoc_list(OldBwd, BwdAL),
	multi_map__init(Bwd0),
	instmap_delta_apply_sub_bwd(BwdAL, Must, Sub, Bwd0, Bwd).

instmap__apply_sub(InstMap0, Must, Sub, InstMap) :-
	instmap_delta_apply_sub(InstMap0, Must, Sub, InstMap).

:- pred instmap_delta_apply_sub_2(assoc_list(var, inst), bool, map(var, var),
		instmapping, instmapping).
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

:- pred instmap_delta_apply_sub_bwd(assoc_list(inst_key, list(var)), bool,
		map(var, var), bwd_mapping, bwd_mapping).
:- mode instmap_delta_apply_sub_bwd(in, in, in, in, out) is det.

instmap_delta_apply_sub_bwd([], _Must, _Sub, IM, IM).
instmap_delta_apply_sub_bwd([K - Vs0 | AL0], Must, Sub, IM0, IM) :-
	goal_util__rename_var_list(Vs0, Must, Sub, Vs),
	map__det_insert(IM0, K, Vs, IM1),
	instmap_delta_apply_sub_bwd(AL0, Must, Sub, IM1, IM).

:- pred add_backward_dependencies(list(inst_key), var,
		bwd_mapping, bwd_mapping).
:- mode add_backward_dependencies(in, in, in, out) is det.

add_backward_dependencies([], _V, BwdMap, BwdMap).
add_backward_dependencies([K | Ks], V, BwdMap0, BwdMap) :-
	multi_map__set(BwdMap0, K, V, BwdMap1),
	add_backward_dependencies(Ks, V, BwdMap1, BwdMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap__to_assoc_list(unreachable, []).
instmap__to_assoc_list(reachable(InstMapping, _Bwd), AL) :-
	map__to_assoc_list(InstMapping, AL).

instmap_delta_to_assoc_list(unreachable, []).
instmap_delta_to_assoc_list(reachable(InstMapping, _Bwd), AL) :-
	map__to_assoc_list(InstMapping, AL).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
