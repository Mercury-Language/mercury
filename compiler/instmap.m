%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: instmap.m
% Main author: bromage.
%
% This module contains code which implements the instmap data type.
%
% An instmap is a 
%
%-----------------------------------------------------------------------------%

:- module instmap.

:- interface.
:- import_module hlds_module, prog_data, mode_info.

:- type instmap_delta   ==      instmap.

:- type instmap         --->    reachable(instmapping)
                        ;       unreachable.

:- type instmapping     ==      map(var, inst).

        % Initialize an empty instmap.
        %
:- pred instmap_init(instmap).
:- mode instmap_init(out) is det.

        % Given an instmap and an instmap_delta, apply the instmap_delta
        % to the instmap to produce a new instmap.
        %
:- pred apply_instmap_delta(instmap, instmap_delta, instmap).
:- mode apply_instmap_delta(in, in, out) is det.

        % Given an instmap and a variable, determine the inst of
        % that variable.
        %
:- pred instmap_lookup_var(instmap, var, inst).
:- mode instmap_lookup_var(in, in, out) is det.

:- pred instmap_lookup_vars(list(var), instmap, list(inst)).
:- mode instmap_lookup_vars(in, in, out) is det.

:- pred instmapping_lookup_var(instmapping, var, inst).
:- mode instmapping_lookup_var(in, in, out) is det.

        % Given two instmaps and a set of variables, compute an instmap delta
        % which records the change in the instantiation state of those
        % variables.
        %
:- pred compute_instmap_delta(instmap, instmap, set(var), instmap_delta).
:- mode compute_instmap_delta(in, in, in, out) is det.

        % instmap_merge(NonLocalVars, InstMaps, MergeContext):
        %       Merge the `InstMaps' resulting from different branches
        %       of a disjunction or if-then-else, and update the
        %       instantiatedness of all the nonlocal variables,
        %       checking that it is the same for every branch.
        %
:- pred instmap_merge(set(var), list(instmap), merge_context,
                mode_info, mode_info).
:- mode instmap_merge(in, in, in, mode_info_di, mode_info_uo) is det.

:- pred instmap_restrict(instmap, set(var), instmap).
:- mode instmap_restrict(in, in, out) is det.

:- pred normalise_insts(list(inst), module_info, list(inst)).
:- mode normalise_insts(in, in, out) is det.

:- pred normalise_inst(inst, module_info, inst).
:- mode normalise_inst(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_util, inst_match, prog_data, mode_errors.
:- import_module std_util, bool, map, set, assoc_list.

%-----------------------------------------------------------------------------%

        % Initialize an empty instmap.

instmap_init(reachable(InstMapping)) :-
        map__init(InstMapping).

%-----------------------------------------------------------------------------%

        % Given an instmap and a variable, determine the inst of
        % that variable.

instmap_lookup_var(unreachable, _Var, not_reached).
instmap_lookup_var(reachable(InstMap), Var, Inst) :-
        instmapping_lookup_var(InstMap, Var, Inst).

instmapping_lookup_var(InstMap, Var, Inst) :-
        ( map__search(InstMap, Var, VarInst) ->
                Inst = VarInst
        ;
                Inst = free
        ).

instmap_lookup_vars([], _InstMap, []).
instmap_lookup_vars([Arg|Args], InstMap, [Inst|Insts]) :-
        instmap_lookup_var(InstMap, Arg, Inst),
        instmap_lookup_vars(Args, InstMap, Insts).

%-----------------------------------------------------------------------------%

        % Given two instmaps, overlay the entries in the second map
        % on top of those in the first map to produce a new map.

apply_instmap_delta(unreachable, _, unreachable).
apply_instmap_delta(reachable(_), unreachable, unreachable).
apply_instmap_delta(reachable(InstMapping0), reachable(InstMappingDelta),
                        reachable(InstMapping)) :-
        map__overlay(InstMapping0, InstMappingDelta, InstMapping).

%-----------------------------------------------------------------------------%

instmap_restrict(unreachable, _, unreachable).
instmap_restrict(reachable(InstMapping0), Vars, reachable(InstMapping)) :-
        map_restrict(InstMapping0, Vars, InstMapping).

:- pred map_restrict(map(K,V), set(K), map(K,V)).
:- mode map_restrict(in, in, out) is det.

map_restrict(Map0, Domain0, Map) :-
        map__keys(Map0, MapKeys),
        set__sorted_list_to_set(MapKeys, MapKeysSet),
        set__intersect(Domain0, MapKeysSet, Domain),
        set__to_sorted_list(Domain, Keys),
        map__apply_to_list(Keys, Map0, Values),
        assoc_list__from_corresponding_lists(Keys, Values, AssocList),
        map__from_sorted_assoc_list(AssocList, Map).

%-----------------------------------------------------------------------------%

normalise_insts([], _, []).
normalise_insts([Inst0|Insts0], ModuleInfo, [Inst|Insts]) :-
        normalise_inst(Inst0, ModuleInfo, Inst),
        normalise_insts(Insts0, ModuleInfo, Insts).

        % This is a bit of a hack.
        % The aim is to avoid non-termination due to the creation
        % of ever-expanding insts.
        % XXX should also normalise partially instantiated insts.

normalise_inst(Inst0, ModuleInfo, NormalisedInst) :-
        inst_expand(ModuleInfo, Inst0, Inst),
        ( Inst = bound(_, _) ->
                (
                        inst_is_ground(ModuleInfo, Inst),
                        inst_is_unique(ModuleInfo, Inst)
                ->
                        NormalisedInst = ground(unique, no)
                ;
                        inst_is_ground(ModuleInfo, Inst),
                        inst_is_mostly_unique(ModuleInfo, Inst)
                ->
                        NormalisedInst = ground(mostly_unique, no)
                ;
                        inst_is_ground(ModuleInfo, Inst),
                        \+ inst_is_clobbered(ModuleInfo, Inst)
                ->
                        NormalisedInst = ground(shared, no)
                ;
                        % XXX need to limit the potential size of insts
                        % here in order to avoid infinite loops in
                        % mode inference
                        NormalisedInst = Inst
                )
        ;
                NormalisedInst = Inst
        ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

        % instmap_merge(NonLocalVars, InstMaps, MergeContext):
        %       Merge the `InstMaps' resulting from different branches
        %       of a disjunction or if-then-else, and update the
        %       instantiatedness of all the nonlocal variables,
        %       checking that it is the same for every branch.

instmap_merge(NonLocals, InstMapList, MergeContext, ModeInfo0, ModeInfo) :-
        mode_info_get_instmap(ModeInfo0, InstMap0),
        mode_info_get_module_info(ModeInfo0, ModuleInfo0),
        get_reachable_instmaps(InstMapList, InstMappingList),
        ( InstMappingList = [] ->
                InstMap = unreachable,
                ModeInfo2 = ModeInfo0
        ; InstMap0 = reachable(InstMapping0) ->
                set__to_sorted_list(NonLocals, NonLocalsList),
                instmap_merge_2(NonLocalsList, InstMapList, ModuleInfo0,
                        InstMapping0, ModuleInfo, InstMapping, ErrorList),
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

:- pred get_reachable_instmaps(list(instmap), list(map(var,inst))).
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

        % instmap_merge_2(Vars, InstMaps, ModuleInfo, ErrorList):
        %       Let `ErrorList' be the list of variables in `Vars' for
        %       there are two instmaps in `InstMaps' for which the inst
        %       the variable is incompatible.

:- pred instmap_merge_2(list(var), list(instmap), module_info, map(var, inst),
                        module_info, map(var, inst), merge_errors).
:- mode instmap_merge_2(in, in, in, in, out, out, out) is det.

instmap_merge_2([], _, ModuleInfo, InstMap, ModuleInfo, InstMap, []).
instmap_merge_2([Var|Vars], InstMapList, ModuleInfo0, InstMap0,
                        ModuleInfo, InstMap, ErrorList) :-
        instmap_merge_2(Vars, InstMapList, ModuleInfo0, InstMap0,
                        ModuleInfo1, InstMap1, ErrorList1),
        instmap_merge_var(InstMapList, Var, ModuleInfo1,
                        Insts, Inst, ModuleInfo, Error),
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

:- pred instmap_merge_var(list(instmap), var, module_info,
                                list(inst), inst, module_info, bool).
:- mode instmap_merge_var(in, in, in, out, out, out, out) is det.

instmap_merge_var([], _, ModuleInfo, [], not_reached, ModuleInfo, no).
instmap_merge_var([InstMap | InstMaps], Var, ModuleInfo0,
                InstList, Inst, ModuleInfo, Error) :-
        instmap_merge_var(InstMaps, Var, ModuleInfo0,
                InstList0, Inst0, ModuleInfo1, Error0),
        instmap_lookup_var(InstMap, Var, VarInst),
        InstList = [VarInst | InstList0],
        ( inst_merge(Inst0, VarInst, ModuleInfo1, Inst1, ModuleInfo2) ->
                Inst = Inst1,
                ModuleInfo = ModuleInfo2,
                Error = Error0
        ;
                Error = yes,
                ModuleInfo = ModuleInfo1,
                Inst = not_reached
        ).

%-----------------------------------------------------------------------------%
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
