%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001 The University of Melbourne.
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
:- import_module hlds_data, hlds_pred.

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

:- pred instmap__from_assoc_list(assoc_list(prog_var, inst), instmap).
:- mode instmap__from_assoc_list(in, out) is det.

:- pred instmap_delta_from_assoc_list(assoc_list(prog_var, inst),
		instmap_delta).
:- mode instmap_delta_from_assoc_list(in, out) is det.

:- pred instmap_delta_from_mode_list(list(prog_var), list(mode),
		module_info, instmap_delta).
:- mode instmap_delta_from_mode_list(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Return the set of variables in an instmap.
	%
:- pred instmap__vars(instmap, set(prog_var)).
:- mode instmap__vars(in, out) is det.

	% Return the list of variables in an instmap.
	%
:- pred instmap__vars_list(instmap, list(prog_var)).
:- mode instmap__vars_list(in, out) is det.

	% Return the set of variables whose instantiations have
	% changed (or our knowledge about them has changed) across
	% an instmap_delta.
	%
	% This predicate shouldn't be used if you want your code to
	% compile on the alias branch, use instmap_changed_vars instead.
	%
:- pred instmap_delta_changed_vars(instmap_delta, set(prog_var)).
:- mode instmap_delta_changed_vars(in, out) is det.

	%
	% instmap_changed_vars(IMA, IMB, MI, CV)
	%
	% Given an earlier instmap, IMA, and a later instmap, IMB,
	% determine what variables, CV, have had their instantiatedness
	% information changed.
	%
	% This predicate is meant to be equivalent to
	% instmap_delta_changed_vars, where the instmap_delta is simply
	% the one to take IMA to IMB.  However this predicate should
	% transform more easily to the alias branch.
	%
:- pred instmap_changed_vars(instmap::in, instmap::in, vartypes::in,
		module_info::in, set(prog_var)::out) is det.

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.
	%
:- pred instmap__lookup_var(instmap, prog_var, inst).
:- mode instmap__lookup_var(in, in, out) is det.

	% Given an instmap_delta and a variable, determine the new inst
	% of that variable (if any).
	%
:- pred instmap_delta_search_var(instmap_delta, prog_var, inst).
:- mode instmap_delta_search_var(in, in, out) is semidet.

	% Given an instmap and a list of variables, return a list
	% containing the insts of those variable.
	%
:- pred instmap__lookup_vars(list(prog_var), instmap, list(inst)).
:- mode instmap__lookup_vars(in, in, out) is det.

	% Insert an entry into an instmap_delta.  Note that you
	% cannot call instmap_delta_insert for a variable already
	% present.
	%
:- pred instmap_delta_insert(instmap_delta, prog_var, inst, instmap_delta).
:- mode instmap_delta_insert(in, in, in, out) is det.

	% Set an entry in an instmap.
	%
:- pred instmap__set(instmap, prog_var, inst, instmap).
:- mode instmap__set(in, in, in, out) is det.

	% Set multiple entries in an instmap.
	%
:- pred instmap__set_vars(instmap, list(prog_var), list(inst), instmap).
:- mode instmap__set_vars(in, in, in, out) is det.

:- pred instmap_delta_set(instmap_delta, prog_var, inst, instmap_delta).
:- mode instmap_delta_set(in, in, in, out) is det.

	% Bind a variable in an instmap to a functor at the beginning
	% of a case in a switch. Aborts on compiler generated cons_ids.
:- pred instmap_delta_bind_var_to_functor(prog_var, type, cons_id, instmap,
		instmap_delta, instmap_delta, module_info, module_info).
:- mode instmap_delta_bind_var_to_functor(in, in, in, in, in, out, in, out)
		is det.

:- pred instmap__bind_var_to_functor(prog_var, type, cons_id,
		instmap, instmap, module_info, module_info).
:- mode instmap__bind_var_to_functor(in, in, in, in, out, in, out) is det.

	% Update the given instmap to include the initial insts of the
	% lambda variables.
:- pred instmap__pre_lambda_update(module_info, list(prog_var), list(mode),
		instmap, instmap).
:- mode instmap__pre_lambda_update(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.
	%
:- pred compute_instmap_delta(instmap, instmap, set(prog_var), instmap_delta).
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
:- pred instmap__merge(set(prog_var), list(instmap), merge_context,
		mode_info, mode_info).
:- mode instmap__merge(in, in, in, mode_info_di, mode_info_uo) is det.

	% instmap__unify(NonLocalVars, InstMapNonlocalvarPairss):
	%       Unify the `InstMaps' in the list of pairs resulting
	%	from different branches of a parallel conjunction and
	%	update the instantiatedness of all the nonlocal variables.
	%	The variable locking that is done when modechecking
	%	the individual conjuncts ensures that variables have
	%	at most one producer.
	%
:- pred instmap__unify(set(prog_var), list(pair(instmap, set(prog_var))),
		mode_info, mode_info).
:- mode instmap__unify(in, in, mode_info_di, mode_info_uo) is det.

	% instmap__restrict takes an instmap and a set of vars and
	% returns an instmap with its domain restricted to those
	% vars.
	%
:- pred instmap__restrict(instmap, set(prog_var), instmap).
:- mode instmap__restrict(in, in, out) is det.

	% instmap_delta_restrict takes an instmap and a set of vars
	% and returns an instmap_delta with its domain restricted to
	% those vars.
	%
:- pred instmap_delta_restrict(instmap_delta, set(prog_var), instmap_delta).
:- mode instmap_delta_restrict(in, in, out) is det.

	% instmap_delta_delete_vars takes an instmap_delta and a list of
	% vars and returns an instmap_delta with those vars removed from
	% its domain.
	%
:- pred instmap_delta_delete_vars(instmap_delta, list(prog_var), instmap_delta).
:- mode instmap_delta_delete_vars(in, in, out) is det.

	% `instmap__no_output_vars(Instmap, InstmapDelta, Vars, ModuleInfo)'
	% is true if none of the vars in the set Vars could have become more
	% instantiated when InstmapDelta is applied to Instmap.
:- pred instmap__no_output_vars(instmap, instmap_delta, set(prog_var),
		vartypes, module_info).
:- mode instmap__no_output_vars(in, in, in, in, in) is semidet.

	% merge_instmap_delta(InitialInstMap, NonLocals,
	%	InstMapDeltaA, InstMapDeltaB, ModuleInfo0, ModuleInfo)
	% Merge the instmap_deltas of different branches of an if-then-else,
	% disj or switch.
:- pred merge_instmap_delta(instmap, set(prog_var), vartypes, instmap_delta,
		instmap_delta, instmap_delta, module_info, module_info).
:- mode merge_instmap_delta(in, in, in, in, in, out, in, out) is det.

	% merge_instmap_deltas(Vars, InstMapDeltas,
	%	MergedInstMapDelta, ModuleInfo0, ModuleInfo)
	% takes a list of instmap deltas from the branches of an if-then-else,
	% switch, or disj and merges them. This is used in situations
	% where the bindings are known to be compatible.
:- pred merge_instmap_deltas(instmap, set(prog_var), vartypes,
		list(instmap_delta), instmap_delta, module_info, module_info).
:- mode merge_instmap_deltas(in, in, in, in, out, in, out) is det.

	% unify_instmap_delta(InitialInstMap, NonLocals,
	%	InstMapDeltaA, InstMapDeltaB, ModuleInfo0, ModuleInfo)
	% Unify the instmap_deltas of different branches of a parallel
	% conjunction.
:- pred unify_instmap_delta(instmap, set(prog_var), instmap_delta,
		instmap_delta, instmap_delta, module_info, module_info).
:- mode unify_instmap_delta(in, in, in, in, out, in, out) is det.

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
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_util, inst_match, prog_data, goal_util, type_util.
:- import_module hlds_data, inst_util.

:- import_module std_util, require, string, term.

:- type instmap_delta	==	instmap.

:- type instmap	--->	reachable(instmapping)
		;	unreachable.

:- type instmapping	==	map(prog_var, inst).

%-----------------------------------------------------------------------------%

	% Initialize an empty instmap and instmap_delta.

instmap__init_reachable(reachable(InstMapping)) :-
	map__init(InstMapping).

instmap__init_unreachable(unreachable).

instmap_delta_init_reachable(reachable(InstMapping)) :-
	map__init(InstMapping).

instmap_delta_init_unreachable(unreachable).

%-----------------------------------------------------------------------------%

instmap__is_reachable(reachable(_)).

instmap__is_unreachable(unreachable).

instmap_delta_is_reachable(reachable(_)).

instmap_delta_is_unreachable(unreachable).

%-----------------------------------------------------------------------------%

instmap__from_assoc_list(AL, reachable(Instmapping)) :-
	map__from_assoc_list(AL, Instmapping).

instmap_delta_from_assoc_list(AL, reachable(Instmapping)) :-
	map__from_assoc_list(AL, Instmapping).

%-----------------------------------------------------------------------------%

instmap_delta_from_mode_list(Var, Modes, ModuleInfo, InstMapDelta) :-
	instmap_delta_init_reachable(InstMapDelta0),
	instmap_delta_from_mode_list_2(Var, Modes, ModuleInfo,
		InstMapDelta0, InstMapDelta).
	
:- pred instmap_delta_from_mode_list_2(list(prog_var), list(mode),
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
instmap__vars_list(reachable(InstMapping), VarsList) :-
	map__keys(InstMapping, VarsList).

instmap_delta_changed_vars(unreachable, EmptySet) :-
	set__init(EmptySet).
instmap_delta_changed_vars(reachable(InstMapping), ChangedVars) :-
	map__keys(InstMapping, ChangedVarsList),
	set__sorted_list_to_set(ChangedVarsList, ChangedVars).

%-----------------------------------------------------------------------------%

instmap_changed_vars(InstMapA, InstMapB, VarTypes, ModuleInfo, ChangedVars) :-
	instmap__vars_list(InstMapB, VarsB),
	changed_vars_2(VarsB, InstMapA, InstMapB, VarTypes, ModuleInfo,
		ChangedVars).

:- pred changed_vars_2(prog_vars::in, instmap::in, instmap::in, vartypes::in,
		module_info::in, set(prog_var)::out) is det.

changed_vars_2([], _InstMapA, _InstMapB, _Types, _ModuleInfo, ChangedVars) :-
	set__init(ChangedVars).
changed_vars_2([VarB|VarBs], InstMapA, InstMapB, VarTypes, ModuleInfo,
		ChangedVars) :-
	changed_vars_2(VarBs, InstMapA, InstMapB, VarTypes, ModuleInfo,
		ChangedVars0),

	instmap__lookup_var(InstMapA, VarB, InitialInst),
	instmap__lookup_var(InstMapB, VarB, FinalInst),
	map__lookup(VarTypes, VarB, Type),

	(
		inst_matches_final(InitialInst, FinalInst, Type, ModuleInfo)
	->
		ChangedVars = ChangedVars0
	;
		set__insert(ChangedVars0, VarB, ChangedVars)
	).

%-----------------------------------------------------------------------------%

	% Given an instmap and a variable, determine the inst of
	% that variable.

instmap__lookup_var(unreachable, _Var, not_reached).
instmap__lookup_var(reachable(InstMap), Var, Inst) :-
	instmapping_lookup_var(InstMap, Var, Inst).

:- pred instmapping_lookup_var(instmapping, prog_var, inst).
:- mode instmapping_lookup_var(in, in, out) is det.

instmapping_lookup_var(InstMap, Var, Inst) :-
	( map__search(InstMap, Var, VarInst) ->
		Inst = VarInst
	;
		Inst = free
	).

instmap_delta_search_var(unreachable, _, not_reached).
instmap_delta_search_var(reachable(InstMap), Var, Inst) :-
	map__search(InstMap, Var, Inst).

instmap__lookup_vars([], _InstMap, []).
instmap__lookup_vars([Arg|Args], InstMap, [Inst|Insts]) :-
	instmap__lookup_var(InstMap, Arg, Inst),
	instmap__lookup_vars(Args, InstMap, Insts).

instmap__set(unreachable, _Var, _Inst, unreachable).
instmap__set(reachable(InstMapping0), Var, Inst,
		reachable(InstMapping)) :-
	map__set(InstMapping0, Var, Inst, InstMapping).

instmap__set_vars(InstMap, [], [], InstMap).
instmap__set_vars(InstMap0, [V | Vs], [I | Is], InstMap) :-
	instmap__set(InstMap0, V, I, InstMap1),
	instmap__set_vars(InstMap1, Vs, Is, InstMap).
instmap__set_vars(_, [_ | _], [], _) :-
	error("instmap__set_vars").
instmap__set_vars(_, [], [_ | _], _) :-
	error("instmap__set_vars").

instmap_delta_set(unreachable, _Var, _Inst, unreachable).
instmap_delta_set(reachable(InstMapping0), Var, Inst, Instmap) :-
	( Inst = not_reached ->
		Instmap = unreachable
	;
		map__set(InstMapping0, Var, Inst, InstMapping),
		Instmap = reachable(InstMapping)
	).

instmap_delta_insert(unreachable, _Var, _Inst, unreachable).
instmap_delta_insert(reachable(InstMapping0), Var, Inst, Instmap) :-
	( Inst = not_reached ->
		Instmap = unreachable
	;
		map__det_insert(InstMapping0, Var, Inst, InstMapping),
		Instmap = reachable(InstMapping)
	).

%-----------------------------------------------------------------------------%

instmap_delta_bind_var_to_functor(Var, Type, ConsId, InstMap,
		InstmapDelta0, InstmapDelta, ModuleInfo0, ModuleInfo) :-
	( 
		InstmapDelta0 = unreachable,
		InstmapDelta = unreachable,
		ModuleInfo = ModuleInfo0
	;
		InstmapDelta0 = reachable(InstmappingDelta0),

		%
		% Get the initial inst from the InstMap
		%
		instmap__lookup_var(InstMap, Var, OldInst),

		%
		% Compute the new inst by taking the old inst,
		% applying the instmap delta to it,
		% and then unifying with bound(ConsId, ...)
		%
		( map__search(InstmappingDelta0, Var, NewInst0) ->
			NewInst1 = NewInst0
		;
			NewInst1 = OldInst
		),
		bind_inst_to_functor(Type, ConsId, NewInst1, NewInst,
			ModuleInfo0, ModuleInfo),

		%
		% add `Var :: OldInst -> NewInst' to the instmap delta
		%
		( NewInst \= OldInst ->
			instmap_delta_set(InstmapDelta0, Var, NewInst,
				InstmapDelta)
		;
			InstmapDelta = InstmapDelta0
		)
	).

instmap__bind_var_to_functor(Var, Type, ConsId, InstMap0, InstMap,
		ModuleInfo0, ModuleInfo) :-
	instmap__lookup_var(InstMap0, Var, Inst0),
	bind_inst_to_functor(Type, ConsId, Inst0, Inst,
		ModuleInfo0, ModuleInfo),
	instmap__set(InstMap0, Var, Inst, InstMap).

:- pred bind_inst_to_functor(type, cons_id, (inst), (inst),
		module_info, module_info). 
:- mode bind_inst_to_functor(in, in, in, out, in, out) is det.

bind_inst_to_functor(Type, ConsId, Inst0, Inst, ModuleInfo0, ModuleInfo) :-
	Arity = cons_id_adjusted_arity(ModuleInfo0, Type, ConsId),
	list__duplicate(Arity, dead, ArgLives),
	list__duplicate(Arity, free, ArgInsts),
	(
		abstractly_unify_inst_functor(dead, Inst0, ConsId, ArgInsts, 
			ArgLives, real_unify, ModuleInfo0, Inst1, _Det,
			ModuleInfo1)
	->
		ModuleInfo = ModuleInfo1,
		Inst = Inst1
	;
		error("bind_inst_to_functor: mode error")
	).

%-----------------------------------------------------------------------------%

instmap__pre_lambda_update(ModuleInfo, Vars, Modes, InstMap0, InstMap) :-
	mode_list_get_initial_insts(Modes, ModuleInfo, Insts),
	assoc_list__from_corresponding_lists(Vars, Insts, VarInsts),
	instmap_delta_from_assoc_list(VarInsts, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

%-----------------------------------------------------------------------------%

	% Given an instmap and an instmap_delta, overlay the
	% entries in the instmap_delta on top of those in the
	% instmap to produce a new instmap.

instmap__apply_instmap_delta(unreachable, _, unreachable).
instmap__apply_instmap_delta(reachable(_), unreachable, unreachable).
instmap__apply_instmap_delta(reachable(InstMapping0),
		reachable(InstMappingDelta), reachable(InstMapping)) :-
	map__overlay(InstMapping0, InstMappingDelta, InstMapping).

	% Given two instmap_deltas, overlay the entries in the second
	% instmap_delta on top of those in the first to produce a new
	% instmap_delta.

instmap_delta_apply_instmap_delta(unreachable, _, unreachable).
instmap_delta_apply_instmap_delta(reachable(_), unreachable, unreachable).
instmap_delta_apply_instmap_delta(reachable(InstMappingDelta0),
		reachable(InstMappingDelta1), reachable(InstMappingDelta)) :-
	map__overlay(InstMappingDelta0, InstMappingDelta1, InstMappingDelta).

%-----------------------------------------------------------------------------%

instmap__restrict(unreachable, _, unreachable).
instmap__restrict(reachable(InstMapping0), Vars, reachable(InstMapping)) :-
	map__select(InstMapping0, Vars, InstMapping).

instmap_delta_restrict(unreachable, _, unreachable).
instmap_delta_restrict(reachable(InstMapping0), Vars, reachable(InstMapping)) :-
	map__select(InstMapping0, Vars, InstMapping).

instmap_delta_delete_vars(unreachable, _, unreachable).
instmap_delta_delete_vars(reachable(InstMapping0), Vars,
		reachable(InstMapping)) :-
	map__delete_list(InstMapping0, Vars, InstMapping).

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
	get_reachable_instmaps(InstMapList, InstMappingList),
	( InstMappingList = [] ->
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0
	; InstMap0 = reachable(InstMapping0) ->
		set__to_sorted_list(NonLocals, NonLocalsList),
		mode_info_get_var_types(ModeInfo0, VarTypes),
		instmap__merge_2(NonLocalsList, InstMapList, VarTypes,	
			ModuleInfo0, InstMapping0, ModuleInfo, InstMapping,
			ErrorList),
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
		InstMap = reachable(InstMapping)
	;
		InstMap = unreachable,
		ModeInfo2 = ModeInfo0
	),
	mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo).

:- pred get_reachable_instmaps(list(instmap), list(map(prog_var, inst))).
:- mode get_reachable_instmaps(in, out) is det.

get_reachable_instmaps([], []).
get_reachable_instmaps([InstMap | InstMaps], Reachables) :-
	( InstMap = reachable(InstMapping) ->
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

:- pred instmap__merge_2(list(prog_var), list(instmap), vartypes, module_info,
		map(prog_var, inst), module_info, map(prog_var, inst),
		merge_errors).
:- mode instmap__merge_2(in, in, in, in, in, out, out, out) is det.

instmap__merge_2([], _, _, ModuleInfo, InstMap, ModuleInfo, InstMap, []).
instmap__merge_2([Var|Vars], InstMapList, VarTypes, ModuleInfo0, InstMap0,
			ModuleInfo, InstMap, ErrorList) :-
	instmap__merge_2(Vars, InstMapList, VarTypes, ModuleInfo0, InstMap0,
			ModuleInfo1, InstMap1, ErrorList1),
	instmap__merge_var(InstMapList, Var, VarTypes ^ det_elem(Var),
		ModuleInfo1, Insts, Inst, ModuleInfo, Error),
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

:- pred instmap__merge_var(list(instmap), prog_var, (type), module_info,
				list(inst), inst, module_info, bool).
:- mode instmap__merge_var(in, in, in, in, out, out, out, out) is det.

instmap__merge_var([], _, _, ModuleInfo, [], not_reached, ModuleInfo, no).
instmap__merge_var([InstMap | InstMaps], Var, Type, ModuleInfo0,
		InstList, Inst, ModuleInfo, Error) :-
	instmap__merge_var(InstMaps, Var, Type, ModuleInfo0,
		InstList0, Inst0, ModuleInfo1, Error0),
	instmap__lookup_var(InstMap, Var, VarInst),
	InstList = [VarInst | InstList0],
	(
		inst_merge(Inst0, VarInst, yes(Type), ModuleInfo1, Inst1,
			ModuleInfo2)
	->
		Inst = Inst1,
		ModuleInfo = ModuleInfo2,
		Error = Error0
	;
		Error = yes,
		ModuleInfo = ModuleInfo1,
		Inst = not_reached
	).

%-----------------------------------------------------------------------------%

merge_instmap_deltas(InstMap, NonLocals, VarTypes, InstMapDeltaList,
		MergedDelta, ModuleInfo0, ModuleInfo) :-
	(
		InstMapDeltaList = [],
		error("merge_instmap_deltas: empty instmap_delta list.")
	;
		InstMapDeltaList = [Delta | Deltas],
		merge_instmap_deltas(InstMap, NonLocals, VarTypes, Delta,
			Deltas, MergedDelta, ModuleInfo0, ModuleInfo)
	).

:- pred merge_instmap_deltas(instmap, set(prog_var), vartypes, instmap_delta,
		list(instmap_delta), instmap_delta, module_info, module_info).
:- mode merge_instmap_deltas(in, in, in, in, in, out, in, out) is det.

merge_instmap_deltas(_InstMap, _NonLocals, _VarTypes, MergedDelta, [],
		MergedDelta, ModuleInfo, ModuleInfo).
merge_instmap_deltas(InstMap, NonLocals, VarTypes, MergedDelta0, [Delta|Deltas],
		MergedDelta, ModuleInfo0, ModuleInfo) :-
	merge_instmap_delta(InstMap, NonLocals, VarTypes, MergedDelta0, Delta,
		MergedDelta1, ModuleInfo0, ModuleInfo1),
	merge_instmap_deltas(InstMap, NonLocals, VarTypes, MergedDelta1, Deltas,
		MergedDelta, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

instmap__unify(NonLocals, InstMapList, ModeInfo0, ModeInfo) :-
	(
			% If any of the instmaps is unreachable, then
			% the final instmap is unreachable.
		list__member(unreachable - _, InstMapList)
	->
		mode_info_set_instmap(unreachable, ModeInfo0, ModeInfo)
	;
			% If there is only one instmap, then we just
			% stick it in the mode_info.
		InstMapList = [InstMap - _]
	->
		mode_info_set_instmap(InstMap, ModeInfo0, ModeInfo)
	;
		InstMapList = [InstMap0 - _|InstMapList1],
		InstMap0 = reachable(InstMapping0)
	->
			% having got the first instmapping, to use as
			% an accumulator, all instmap__unify_2 which
			% unifies each of the nonlocals from each instmap
			% with the corresponding inst in the accumulator.
		mode_info_get_module_info(ModeInfo0, ModuleInfo0),
		set__to_sorted_list(NonLocals, NonLocalsList),
		instmap__unify_2(NonLocalsList, InstMap0, InstMapList1,
			ModuleInfo0, InstMapping0, ModuleInfo,
			InstMapping, ErrorList),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
			
			% If there were any errors, then add the error
			% to the list of possible errors in the mode_info.
		( ErrorList = [FirstError | _] ->
			FirstError = Var - _,
			set__singleton_set(WaitingVars, Var),
			mode_info_error(WaitingVars,
				mode_error_par_conj(ErrorList),
				ModeInfo1, ModeInfo2)
		;
			ModeInfo2 = ModeInfo1
		),
		mode_info_set_instmap(reachable(InstMapping),
			ModeInfo2, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

%-----------------------------------------------------------------------------%

	% instmap__unify_2(Vars, InitialInstMap, InstMaps, ModuleInfo,
	%		ErrorList):
	%       Let `ErrorList' be the list of variables in `Vars' for
	%       which there are two instmaps in `InstMaps' for which the insts
	%       of the variable is incompatible.
:- pred instmap__unify_2(list(prog_var), instmap, list(pair(instmap,
		set(prog_var))), module_info, map(prog_var, inst), module_info,
		map(prog_var, inst), merge_errors).
:- mode instmap__unify_2(in, in, in, in, in, out, out, out) is det.

instmap__unify_2([], _, _, ModuleInfo, InstMap, ModuleInfo, InstMap, []).
instmap__unify_2([Var|Vars], InitialInstMap, InstMapList, ModuleInfo0, InstMap0,
			ModuleInfo, InstMap, ErrorList) :-
	instmap__unify_2(Vars, InitialInstMap, InstMapList, ModuleInfo0,
			InstMap0, ModuleInfo1, InstMap1, ErrorList1),
	instmap__lookup_var(InitialInstMap, Var, InitialVarInst),
	instmap__unify_var(InstMapList, Var, [], Insts, InitialVarInst, Inst,
		ModuleInfo1, ModuleInfo, no, Error),
	( Error = yes ->
		ErrorList = [Var - Insts | ErrorList1]
	;
		ErrorList = ErrorList1
	),
	map__set(InstMap1, Var, Inst, InstMap).

	% instmap__unify_var(InstMaps, Var, InitialInstMap, ModuleInfo,
	%		Insts, Error):
	%       Let `Insts' be the list of the inst of `Var' in
	%       each of the corresponding `InstMaps'.  Let `Error' be yes
	%	iff there are two instmaps for which the inst of `Var'
	%       is incompatible.

:- pred instmap__unify_var(list(pair(instmap, set(prog_var))), prog_var,
		list(inst), list(inst), inst, inst, module_info, module_info,
		bool, bool).
:- mode instmap__unify_var(in, in, in, out, in, out, in, out, in, out) is det.

instmap__unify_var([], _, Insts, Insts, Inst, Inst, ModuleInfo, ModuleInfo,
		Error, Error).
instmap__unify_var([InstMap - Nonlocals| Rest], Var, InstList0, InstList,
		Inst0, Inst, ModuleInfo0, ModuleInfo, Error0, Error) :-
	(
		set__member(Var, Nonlocals)
	->
		instmap__lookup_var(InstMap, Var, VarInst),
		(
			% We can ignore the determinism of the unification:
			% if it isn't det, then there will be a mode error
			% or a determinism error in one of the parallel
			% conjuncts.

			abstractly_unify_inst(live, Inst0, VarInst, fake_unify,
				ModuleInfo0, Inst1, _Det, ModuleInfo1)
		->
			Inst2 = Inst1,
			ModuleInfo2 = ModuleInfo1,
			Error1 = Error0
		;
			Error1 = yes,
			ModuleInfo2 = ModuleInfo0,
			Inst2 = not_reached
		)
	;
		VarInst = free,
		Inst2 = Inst0,
		Error1 = Error0,
		ModuleInfo2 = ModuleInfo0
	),
	instmap__unify_var(Rest, Var, [VarInst | InstList0], InstList,
		Inst2, Inst, ModuleInfo2, ModuleInfo, Error1, Error).

%-----------------------------------------------------------------------------%

	% Given two instmaps and a set of variables, compute an instmap delta
	% which records the change in the instantiation state of those
	% variables.

compute_instmap_delta(unreachable, _, _, unreachable).
compute_instmap_delta(reachable(_), unreachable, _, unreachable).
compute_instmap_delta(reachable(InstMapA), reachable(InstMapB), NonLocals,
			reachable(DeltaInstMap)) :-
	set__to_sorted_list(NonLocals, NonLocalsList),
	compute_instmap_delta_2(NonLocalsList, InstMapA, InstMapB, AssocList),
	map__from_sorted_assoc_list(AssocList, DeltaInstMap).

:- pred compute_instmap_delta_2(list(prog_var), instmapping, instmapping,
					assoc_list(prog_var, inst)).
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
instmap__no_output_vars(InstMap0, reachable(InstMapDelta), Vars, VT, M) :-
	set__to_sorted_list(Vars, VarList),
	instmap__no_output_vars_2(VarList, InstMap0, InstMapDelta, VT, M).

:- pred instmap__no_output_vars_2(list(prog_var), instmap, instmapping,
		vartypes, module_info).
:- mode instmap__no_output_vars_2(in, in, in, in, in) is semidet.

instmap__no_output_vars_2([], _, _, _, _).
instmap__no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, VarTypes,
		ModuleInfo) :-
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
	map__lookup(VarTypes, Var, Type),
	inst_matches_binding(Inst, Inst0, Type, ModuleInfo),
	instmap__no_output_vars_2(Vars, InstMap0, InstMapDelta, VarTypes,
		ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given two instmap deltas, merge them to produce a new instmap_delta.

merge_instmap_delta(_, _, _, unreachable, InstMapDelta, InstMapDelta) --> [].
merge_instmap_delta(_, _, _, reachable(InstMapping), unreachable,
				reachable(InstMapping)) --> [].
merge_instmap_delta(InstMap, NonLocals, VarTypes, reachable(InstMappingA),
		reachable(InstMappingB), reachable(InstMapping)) -->
	merge_instmapping_delta(InstMap, NonLocals, VarTypes, InstMappingA,
		InstMappingB, InstMapping).

:- pred merge_instmapping_delta(instmap, set(prog_var), vartypes, instmapping,
		instmapping, instmapping, module_info, module_info).
:- mode merge_instmapping_delta(in, in, in, in, in, out, in, out) is det.

merge_instmapping_delta(InstMap, NonLocals, VarTypes, InstMappingA,
		InstMappingB, InstMapping) -->
	{ map__keys(InstMappingA, VarsInA) },
	{ map__keys(InstMappingB, VarsInB) },
	{ set__sorted_list_to_set(VarsInA, SetofVarsInA) },
	{ set__insert_list(SetofVarsInA, VarsInB, SetofVars0) },
	{ set__intersect(SetofVars0, NonLocals, SetofVars) },
	{ map__init(InstMapping0) },
	{ set__to_sorted_list(SetofVars, ListofVars) },
	merge_instmapping_delta_2(ListofVars, InstMap, VarTypes, InstMappingA,
		InstMappingB, InstMapping0, InstMapping).

:- pred merge_instmapping_delta_2(list(prog_var), instmap, vartypes,
		instmapping, instmapping, instmapping, instmapping,
		module_info, module_info).
:- mode merge_instmapping_delta_2(in, in, in, in, in, in, out, in, out) is det.

merge_instmapping_delta_2([], _, _, _, _, InstMapping, InstMapping,
		ModInfo, ModInfo).
merge_instmapping_delta_2([Var | Vars], InstMap, VarTypes, InstMappingA,
		InstMappingB, InstMapping0, InstMapping,
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
		inst_merge(InstA, InstB, yes(VarTypes ^ det_elem(Var)),
			ModuleInfo0, Inst, ModuleInfoPrime)
	->
		ModuleInfo1 = ModuleInfoPrime,
		map__det_insert(InstMapping0, Var, Inst, InstMapping1)
	;
		term__var_to_int(Var, VarInt),
		string__format(
			"merge_instmapping_delta_2: error merging var %i",
			[i(VarInt)], Msg),
		error(Msg)
	),
	merge_instmapping_delta_2(Vars, InstMap, VarTypes, InstMappingA,
		InstMappingB, InstMapping1, InstMapping,
		ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given two instmap deltas, unify them to produce a new instmap_delta.

unify_instmap_delta(_, _, unreachable, InstMapDelta, InstMapDelta) --> [].
unify_instmap_delta(_, _, reachable(InstMapping), unreachable,
				reachable(InstMapping)) --> [].
unify_instmap_delta(InstMap, NonLocals, reachable(InstMappingA),
		reachable(InstMappingB), reachable(InstMapping)) -->
	unify_instmapping_delta(InstMap, NonLocals, InstMappingA,
		InstMappingB, InstMapping).

:- pred unify_instmapping_delta(instmap, set(prog_var), instmapping,
		instmapping, instmapping, module_info, module_info).
:- mode unify_instmapping_delta(in, in, in, in, out, in, out) is det.

unify_instmapping_delta(InstMap, NonLocals, InstMappingA,
		InstMappingB, InstMapping) -->
	{ map__keys(InstMappingA, VarsInA) },
	{ map__keys(InstMappingB, VarsInB) },
	{ set__sorted_list_to_set(VarsInA, SetofVarsInA) },
	{ set__insert_list(SetofVarsInA, VarsInB, SetofVars0) },
	{ set__intersect(SetofVars0, NonLocals, SetofVars) },
	{ map__init(InstMapping0) },
	{ set__to_sorted_list(SetofVars, ListofVars) },
	unify_instmapping_delta_2(ListofVars, InstMap, InstMappingA,
		InstMappingB, InstMapping0, InstMapping).

:- pred unify_instmapping_delta_2(list(prog_var), instmap, instmapping,
		instmapping, instmapping, instmapping,
		module_info, module_info).
:- mode unify_instmapping_delta_2(in, in, in, in, in, out, in, out) is det.

unify_instmapping_delta_2([], _, _, _, InstMapping, InstMapping,
		ModInfo, ModInfo).
unify_instmapping_delta_2([Var | Vars], InstMap, InstMappingA, InstMappingB,
			InstMapping0, InstMapping, ModuleInfo0, ModuleInfo) :-
	( map__search(InstMappingA, Var, InstA) ->
		( map__search(InstMappingB, Var, InstB) ->
			(
				% We can ignore the determinism of the
				% unification: if it isn't det, then there
				% will be a mode error or a determinism error
				% in one of the parallel conjuncts.

				abstractly_unify_inst(live, InstA, InstB,
					fake_unify, ModuleInfo0, Inst, _Det,
					ModuleInfoPrime)
			->
				ModuleInfo1 = ModuleInfoPrime,
				map__det_insert(InstMapping0, Var, Inst,
					InstMapping1)
			;
				error(
				"unify_instmapping_delta_2: unexpected error")
			)
		;
			ModuleInfo1 = ModuleInfo0,
			map__det_insert(InstMapping0, Var, InstA, InstMapping1)
		)
	;
		( map__search(InstMappingB, Var, InstB) ->
			ModuleInfo1 = ModuleInfo0,
			map__det_insert(InstMapping0, Var, InstB, InstMapping1)
		;
			ModuleInfo1 = ModuleInfo0,
			InstMapping1 = InstMapping0
		)
	),
	unify_instmapping_delta_2(Vars, InstMap, InstMappingA, InstMappingB,
		InstMapping1, InstMapping, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_delta_apply_sub(unreachable, _Must, _Sub, unreachable).
instmap_delta_apply_sub(reachable(OldInstMapping), Must, Sub,
		reachable(InstMapping)) :-
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
instmap__to_assoc_list(reachable(InstMapping), AL) :-
	map__to_assoc_list(InstMapping, AL).

instmap_delta_to_assoc_list(unreachable, []).
instmap_delta_to_assoc_list(reachable(InstMapping), AL) :-
	map__to_assoc_list(InstMapping, AL).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
