%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
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

:- module hlds__instmap.

:- interface.

:- import_module check_hlds__mode_errors.
:- import_module check_hlds__mode_info.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

:- type instmap.
:- type instmap_delta.

    % Initialize an empty instmap.
    %
:- pred instmap__init_reachable(instmap::out) is det.

    % Initialize an empty unreachable instmap.
    %
:- pred instmap__init_unreachable(instmap::out) is det.

    % Initialize an empty reachable instmap_delta.
    %
:- pred instmap_delta_init_reachable(instmap_delta::out) is det.

    % Initialize an empty unreachable instmap_delta.
    %
:- pred instmap_delta_init_unreachable(instmap_delta::out) is det.

    % For any instmap InstMap, exactly one of
    % instmap__is_reachable(InstMap) and
    % instmap__is_unreachable(InstMap) holds.

    % Is the instmap reachable?
    %
:- pred instmap__is_reachable(instmap::in) is semidet.

    % Is the instmap unreachable?
    %
:- pred instmap__is_unreachable(instmap::in) is semidet.

    % For any instmap InstMapDelta, exactly one of
    % instmap_delta_is_reachable(InstMapDelta) and
    % instmap_delta_is_unreachable(InstMapDelta) holds.

    % Is the instmap_delta reachable?
    %
:- pred instmap_delta_is_reachable(instmap_delta::in) is semidet.

    % Is the instmap_delta unreachable?
    %
:- pred instmap_delta_is_unreachable(instmap_delta::in) is semidet.

:- pred instmap__from_assoc_list(assoc_list(prog_var, inst)::in, instmap::out)
    is det.

:- pred instmap_delta_from_assoc_list(assoc_list(prog_var, inst)::in,
    instmap_delta::out) is det.

:- pred instmap_delta_from_mode_list(list(prog_var)::in, list(mode)::in,
    module_info::in, instmap_delta::out) is det.

%-----------------------------------------------------------------------------%

    % Return the set of variables in an instmap.
    %
:- pred instmap__vars(instmap::in, set(prog_var)::out) is det.

    % Return the list of variables in an instmap.
    %
:- pred instmap__vars_list(instmap::in, list(prog_var)::out) is det.

    % Return the set of variables whose instantiations have
    % changed (or our knowledge about them has changed) across
    % an instmap_delta.
    %
    % This predicate shouldn't be used if you want your code to
    % compile on the alias branch, use instmap_changed_vars instead.
    %
:- pred instmap_delta_changed_vars(instmap_delta::in, set(prog_var)::out)
    is det.

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

    % Given an instmap and a variable, determine the inst of that variable.
    %
:- pred instmap__lookup_var(instmap::in, prog_var::in, (inst)::out) is det.

    % Given an instmap_delta and a variable, determine the new inst
    % of that variable (if any).
    %
:- pred instmap_delta_search_var(instmap_delta::in, prog_var::in, (inst)::out)
    is semidet.

    % Given an instmap and a list of variables, return a list
    % containing the insts of those variable.
    %
:- pred instmap__lookup_vars(list(prog_var)::in, instmap::in, list(inst)::out)
    is det.

    % Insert an entry into an instmap_delta.  Note that you
    % cannot call instmap_delta_insert for a variable already
    % present.
    %
:- pred instmap_delta_insert(instmap_delta::in, prog_var::in, (inst)::in,
    instmap_delta::out) is det.

    % Set an entry in an instmap.
    %
:- pred instmap__set(instmap::in, prog_var::in, (inst)::in, instmap::out)
    is det.

    % Set multiple entries in an instmap.
    %
:- pred instmap__set_vars(instmap::in, list(prog_var)::in, list(inst)::in,
    instmap::out) is det.

:- pred instmap_delta_set(instmap_delta::in, prog_var::in, (inst)::in,
    instmap_delta::out) is det.

    % Bind a variable in an instmap to a functor at the beginning
    % of a case in a switch. Aborts on compiler generated cons_ids.
    %
:- pred instmap_delta_bind_var_to_functor(prog_var::in, (type)::in,
    cons_id::in, instmap::in, instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

:- pred instmap__bind_var_to_functor(prog_var::in, (type)::in, cons_id::in,
    instmap::in, instmap::out, module_info::in, module_info::out) is det.

    % Update the given instmap to include the initial insts of the
    % lambda variables.
    %
:- pred instmap__pre_lambda_update(module_info::in, list(prog_var)::in,
    list(mode)::in, instmap::in, instmap::out) is det.

%-----------------------------------------------------------------------------%

:- type overlay_how
    --->    large_base
    ;       large_overlay
    ;       test_size.

    % Given two instmaps and a set of variables, compute an instmap delta
    % which records the change in the instantiation state of those variables.
    %
:- pred compute_instmap_delta(instmap::in, instmap::in, set(prog_var)::in,
    instmap_delta::out) is det.

    % Given an instmap and an instmap_delta, overlay the entries in the
    % instmap_delta on top of those in the instmap to produce a new instmap.
    %
:- pred instmap__apply_instmap_delta(instmap::in, instmap_delta::in,
    instmap::out) is det.

    % Given two instmap_deltas, overlay the entries in the second instmap_delta
    % on top of those in the first to produce a new instmap_delta.
    %
:- pred instmap_delta_apply_instmap_delta(instmap_delta::in, instmap_delta::in,
    overlay_how::in, instmap_delta::out) is det.

    % instmap_merge(NonLocalVars, InstMaps, MergeContext):
    %
    % Merge the `InstMaps' resulting from different branches of a disjunction
    % or if-then-else, and update the instantiatedness of all the nonlocal
    % variables, checking that it is the same for every branch.
    %
:- pred instmap__merge(set(prog_var)::in, list(instmap)::in, merge_context::in,
    mode_info::in, mode_info::out) is det.

    % instmap__unify(NonLocalVars, InstMapNonlocalvarPairss):
    %
    % Unify the `InstMaps' in the list of pairs resulting from different
    % branches of a parallel conjunction and update the instantiatedness
    % of all the nonlocal variables. The variable locking that is done
    % when modechecking the individual conjuncts ensures that variables
    % have at most one producer.
    %
:- pred instmap__unify(set(prog_var)::in, list(pair(instmap,
    set(prog_var)))::in, mode_info::in, mode_info::out) is det.

    % instmap__restrict takes an instmap and a set of vars and returns
    % an instmap with its domain restricted to those vars.
    %
:- pred instmap__restrict(instmap::in, set(prog_var)::in, instmap::out)
    is det.

    % instmap_delta_restrict takes an instmap and a set of vars and returns
    % an instmap_delta with its domain restricted to those vars.
    %
:- pred instmap_delta_restrict(instmap_delta::in, set(prog_var)::in,
    instmap_delta::out) is det.

    % instmap_delta_delete_vars takes an instmap_delta and a list of vars
    % and returns an instmap_delta with those vars removed from its domain.
    %
:- pred instmap_delta_delete_vars(instmap_delta::in, list(prog_var)::in,
    instmap_delta::out) is det.

    % `instmap__no_output_vars(Instmap, InstmapDelta, Vars, ModuleInfo)'
    % is true if none of the vars in the set Vars could have become more
    % instantiated when InstmapDelta is applied to Instmap.
    %
:- pred instmap__no_output_vars(instmap::in, instmap_delta::in,
    set(prog_var)::in, vartypes::in, module_info::in) is semidet.

    % merge_instmap_delta(InitialInstMap, NonLocals,
    %   InstMapDeltaA, InstMapDeltaB, !ModuleInfo):
    %
    % Merge the instmap_deltas of different branches of an if-then-else,
    % disj or switch.
    %
:- pred merge_instmap_delta(instmap::in, set(prog_var)::in, vartypes::in,
    instmap_delta::in, instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

    % merge_instmap_deltas(Vars, InstMapDeltas,
    %   MergedInstMapDelta, ModuleInfo):
    %
    % Takes a list of instmap deltas from the branches of an if-then-else,
    % switch, or disj and merges them. This is used in situations
    % where the bindings are known to be compatible.
    %
:- pred merge_instmap_deltas(instmap::in, set(prog_var)::in, vartypes::in,
    list(instmap_delta)::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

    % unify_instmap_delta(InitialInstMap, NonLocals,
    %   InstMapDeltaA, InstMapDeltaB, !ModuleInfo)
    %
    % Unify the instmap_deltas of different branches of a parallel
    % conjunction.
    %
:- pred unify_instmap_delta(instmap::in, set(prog_var)::in, instmap_delta::in,
    instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

    % `instmap_delta_apply_sub(InstmapDelta0, Must, Sub, InstmapDelta)'
    % the variable substitution Sub to InstmapDelta0 to get the new
    % instmap_delta InstmapDelta.  If there is a variable in
    % InstmapDelta0 which does not appear in Sub, it is ignored if
    % Must is set to no, otherwise it is an error.
    %
:- pred instmap_delta_apply_sub(instmap_delta::in, bool::in,
    map(prog_var, prog_var)::in, instmap_delta::out) is det.

:- pred instmap__apply_sub(instmap::in, bool::in, map(prog_var, prog_var)::in,
    instmap::out) is det.

%-----------------------------------------------------------------------------%

:- pred instmap__to_assoc_list(instmap::in, assoc_list(prog_var, inst)::out)
    is det.

:- pred instmap_delta_to_assoc_list(instmap_delta::in,
    assoc_list(prog_var, inst)::out) is det.

    % Apply the specified procedure to all insts in an instmap_delta.
    %
:- pred instmap_delta_map_foldl(
    pred(prog_var, inst, inst, T, T)::in(pred(in, in, out, in, out) is det),
    instmap_delta::in, instmap_delta::out, T::in, T::out) is det.

:- pred var_is_ground_in_instmap(module_info::in, instmap::in, prog_var::in)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__inst_util.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module parse_tree__prog_data.

:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module term.

:- type instmap_delta   ==  instmap.

:- type instmap
    --->    reachable(instmapping)
    ;       unreachable.

:- type instmapping ==  map(prog_var, inst).

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

instmap_delta_map_foldl(_, unreachable, unreachable, !T).
instmap_delta_map_foldl(P, reachable(Instmapping0), reachable(Instmapping),
        !T) :-
    map__map_foldl(P, Instmapping0, Instmapping, !T).

%-----------------------------------------------------------------------------%

instmap_delta_from_mode_list(Var, Modes, ModuleInfo, InstMapDelta) :-
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_from_mode_list_2(Var, Modes, ModuleInfo,
        InstMapDelta0, InstMapDelta).

:- pred instmap_delta_from_mode_list_2(list(prog_var)::in, list(mode)::in,
    module_info::in, instmap_delta::in, instmap_delta::out) is det.

instmap_delta_from_mode_list_2([], [], _, !InstMapDelta).
instmap_delta_from_mode_list_2([], [_ | _], _, !InstMapDelta) :-
    error("instmap_delta_from_mode_list_2").
instmap_delta_from_mode_list_2([_ | _], [], _, !InstMapDelta) :-
    error("instmap_delta_from_mode_list_2").
instmap_delta_from_mode_list_2([Var | Vars], [Mode | Modes], ModuleInfo,
        !InstMapDelta) :-
    mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
    ( Inst1 = Inst2 ->
        instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo, !InstMapDelta)
    ;
        instmap_delta_set(!.InstMapDelta, Var, Inst2, !:InstMapDelta),
        instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo, !InstMapDelta)
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
changed_vars_2([VarB | VarBs], InstMapA, InstMapB, VarTypes, ModuleInfo,
        ChangedVars) :-
    changed_vars_2(VarBs, InstMapA, InstMapB, VarTypes, ModuleInfo,
        ChangedVars0),

    instmap__lookup_var(InstMapA, VarB, InitialInst),
    instmap__lookup_var(InstMapB, VarB, FinalInst),
    map__lookup(VarTypes, VarB, Type),

    ( inst_matches_final(InitialInst, FinalInst, Type, ModuleInfo) ->
        ChangedVars = ChangedVars0
    ;
        set__insert(ChangedVars0, VarB, ChangedVars)
    ).

%-----------------------------------------------------------------------------%

instmap__lookup_var(unreachable, _Var, not_reached).
instmap__lookup_var(reachable(InstMap), Var, Inst) :-
    instmapping_lookup_var(InstMap, Var, Inst).

:- pred instmapping_lookup_var(instmapping::in, prog_var::in, (inst)::out)
    is det.

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
instmap__set(reachable(InstMapping0), Var, Inst, reachable(InstMapping)) :-
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
        InstmapDelta0, InstmapDelta, !ModuleInfo) :-
    (
        InstmapDelta0 = unreachable,
        InstmapDelta = unreachable
    ;
        InstmapDelta0 = reachable(InstmappingDelta0),

        % Get the initial inst from the InstMap
        instmap__lookup_var(InstMap, Var, OldInst),

        % Compute the new inst by taking the old inst, applying the instmap
        % delta to it, and then unifying with bound(ConsId, ...).
        ( map__search(InstmappingDelta0, Var, NewInst0) ->
            NewInst1 = NewInst0
        ;
            NewInst1 = OldInst
        ),
        bind_inst_to_functor(Type, ConsId, NewInst1, NewInst,
            !ModuleInfo),

        % add `Var :: OldInst -> NewInst' to the instmap delta.
        ( NewInst \= OldInst ->
            instmap_delta_set(InstmapDelta0, Var, NewInst, InstmapDelta)
        ;
            InstmapDelta = InstmapDelta0
        )
    ).

instmap__bind_var_to_functor(Var, Type, ConsId, InstMap0, InstMap,
        !ModuleInfo) :-
    instmap__lookup_var(InstMap0, Var, Inst0),
    bind_inst_to_functor(Type, ConsId, Inst0, Inst, !ModuleInfo),
    instmap__set(InstMap0, Var, Inst, InstMap).

:- pred bind_inst_to_functor((type)::in, cons_id::in, (inst)::in, (inst)::out,
    module_info::in, module_info::out) is det.

bind_inst_to_functor(Type, ConsId, !Inst, !ModuleInfo) :-
    Arity = cons_id_adjusted_arity(!.ModuleInfo, Type, ConsId),
    list__duplicate(Arity, dead, ArgLives),
    list__duplicate(Arity, free, ArgInsts),
    (
        abstractly_unify_inst_functor(dead, !.Inst, ConsId, ArgInsts,
            ArgLives, real_unify, Type, !:Inst, _Det, !ModuleInfo)
    ->
        true
    ;
        error("bind_inst_to_functor: mode error")
    ).

%-----------------------------------------------------------------------------%

instmap__pre_lambda_update(ModuleInfo, Vars, Modes, InstMap0, InstMap) :-
    mode_list_get_initial_insts(ModuleInfo, Modes, Insts),
    assoc_list__from_corresponding_lists(Vars, Insts, VarInsts),
    instmap_delta_from_assoc_list(VarInsts, InstMapDelta),
    instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

%-----------------------------------------------------------------------------%

instmap__apply_instmap_delta(unreachable, _, unreachable).
instmap__apply_instmap_delta(reachable(_), unreachable, unreachable).
instmap__apply_instmap_delta(reachable(InstMapping0),
        reachable(InstMappingDelta), reachable(InstMapping)) :-
    map__overlay(InstMapping0, InstMappingDelta, InstMapping).

instmap_delta_apply_instmap_delta(InstMap1, InstMap2, How, InstMap) :-
    (
        InstMap1 = unreachable,
        InstMap = unreachable
    ;
        InstMap1 = reachable(_),
        InstMap2 = unreachable,
        InstMap = unreachable
    ;
        InstMap1 = reachable(InstMappingDelta1),
        InstMap2 = reachable(InstMappingDelta2),
        (
            How = large_base,
            map__overlay(InstMappingDelta1, InstMappingDelta2,
                InstMappingDelta)
        ;
            How = large_overlay,
            map__overlay_large_map(InstMappingDelta1,
                InstMappingDelta2, InstMappingDelta)
        ;
            How = test_size,
            (
                map__count(InstMappingDelta1, Count1),
                map__count(InstMappingDelta2, Count2),
                Count1 >= Count2
            ->
                map__overlay(InstMappingDelta1,
                    InstMappingDelta2, InstMappingDelta)
            ;
                map__overlay_large_map(InstMappingDelta1,
                    InstMappingDelta2, InstMappingDelta)
            )
        ),
        InstMap = reachable(InstMappingDelta)
    ).

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
    % Merge the `InstMaps' resulting from different branches of a disjunction
    % or if-then-else, and update the instantiatedness of all the nonlocal
    % variables, checking that it is the same for every branch.
    %
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
            InstMapping0, InstMapping, ModuleInfo0, ModuleInfo, ErrorList),
        mode_info_set_module_info(ModuleInfo, ModeInfo0, ModeInfo1),
        ( ErrorList = [FirstError | _] ->
            FirstError = Var - _,
            set__singleton_set(WaitingVars, Var),
            mode_info_error(WaitingVars,
                mode_error_disj(MergeContext, ErrorList), ModeInfo1, ModeInfo2)
        ;
            ModeInfo2 = ModeInfo1
        ),
        InstMap = reachable(InstMapping)
    ;
        InstMap = unreachable,
        ModeInfo2 = ModeInfo0
    ),
    mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo).

:- pred get_reachable_instmaps(list(instmap)::in,
    list(map(prog_var, inst))::out) is det.

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
    % Let `ErrorList' be the list of variables in `Vars' for there are two
    % instmaps in `InstMaps' for which the inst the variable is incompatible.
    %
:- pred instmap__merge_2(list(prog_var)::in, list(instmap)::in, vartypes::in,
    instmapping::in, instmapping::out, module_info::in, module_info::out,
    merge_errors::out) is det.

instmap__merge_2([], _, _, !InstMap, !ModuleInfo, []).
instmap__merge_2([Var | Vars], InstMapList, VarTypes, !InstMap, !ModuleInfo,
        !:ErrorList) :-
    instmap__merge_2(Vars, InstMapList, VarTypes, !InstMap, !ModuleInfo,
        !:ErrorList),
    instmap__merge_var(InstMapList, Var, VarTypes ^ det_elem(Var),
        Insts, Inst, !ModuleInfo, Error),
    (
        Error = yes,
        !:ErrorList = [Var - Insts | !.ErrorList],
        svmap__set(Var, not_reached, !InstMap)
    ;
        Error = no,
        svmap__set(Var, Inst, !InstMap)
    ).

    % instmap_merge_var(InstMaps, Var, ModuleInfo, Insts, Error):
    %
    % Let `Insts' be the list of the inst of `Var' in the corresponding
    % `InstMaps'.  Let `Error' be yes iff there are two instmaps
    % for which the inst of `Var' is incompatible.
    %
:- pred instmap__merge_var(list(instmap)::in, prog_var::in, (type)::in,
    list(inst)::out, (inst)::out, module_info::in, module_info::out,
    bool::out) is det.

instmap__merge_var([], _, _, [], not_reached, !ModuleInfo, no).
instmap__merge_var([InstMap | InstMaps], Var, Type, InstList, Inst,
        !ModuleInfo, Error) :-
    instmap__merge_var(InstMaps, Var, Type, InstList0, Inst0, !ModuleInfo,
        Error0),
    instmap__lookup_var(InstMap, Var, VarInst),
    InstList = [VarInst | InstList0],
    ( inst_merge(Inst0, VarInst, yes(Type), Inst1, !ModuleInfo) ->
        Inst = Inst1,
        Error = Error0
    ;
        Error = yes,
        Inst = not_reached
    ).

%-----------------------------------------------------------------------------%

merge_instmap_deltas(InstMap, NonLocals, VarTypes, InstMapDeltaList,
        MergedDelta, !ModuleInfo) :-
    (
        InstMapDeltaList = [],
        error("merge_instmap_deltas: empty instmap_delta list.")
    ;
        InstMapDeltaList = [Delta | Deltas],
        merge_instmap_deltas(InstMap, NonLocals, VarTypes, Delta,
            Deltas, MergedDelta, !ModuleInfo)
    ).

:- pred merge_instmap_deltas(instmap::in, set(prog_var)::in, vartypes::in,
    instmap_delta::in, list(instmap_delta)::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

merge_instmap_deltas(_InstMap, _NonLocals, _VarTypes, MergedDelta, [],
        MergedDelta, !ModuleInfo).
merge_instmap_deltas(InstMap, NonLocals, VarTypes, MergedDelta0, [Delta|Deltas],
        MergedDelta, !ModuleInfo) :-
    merge_instmap_delta(InstMap, NonLocals, VarTypes, MergedDelta0, Delta,
        MergedDelta1, !ModuleInfo),
    merge_instmap_deltas(InstMap, NonLocals, VarTypes, MergedDelta1, Deltas,
        MergedDelta, !ModuleInfo).

%-----------------------------------------------------------------------------%

instmap__unify(NonLocals, InstMapList, !ModeInfo) :-
    (
        % If any of the instmaps is unreachable, then the final instmap
        % is unreachable.
        list__member(unreachable - _, InstMapList)
    ->
        mode_info_set_instmap(unreachable, !ModeInfo)
    ;
        % If there is only one instmap, then we just stick it in the mode_info.
        InstMapList = [InstMap - _]
    ->
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        InstMapList = [InstMap0 - _|InstMapList1],
        InstMap0 = reachable(InstMapping0)
    ->
        % Having got the first instmapping, to use as an accumulator,
        % all instmap__unify_2 which unifies each of the nonlocals from
        % each instmap with the corresponding inst in the accumulator.
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        set__to_sorted_list(NonLocals, NonLocalsList),
        instmap__unify_2(NonLocalsList, InstMap0, InstMapList1,
            ModuleInfo0, InstMapping0, ModuleInfo, InstMapping, ErrorList),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),

        % If there were any errors, then add the error to the list
        % of possible errors in the mode_info.
        (
            ErrorList = [FirstError | _],
            FirstError = Var - _,
            set__singleton_set(WaitingVars, Var),
            mode_info_error(WaitingVars,
                mode_error_par_conj(ErrorList), !ModeInfo)
        ;
            ErrorList = []
        ),
        mode_info_set_instmap(reachable(InstMapping), !ModeInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % instmap__unify_2(Vars, InitialInstMap, InstMaps, ModuleInfo, ErrorList):
    %
    % Let `ErrorList' be the list of variables in `Vars' for which there are
    % two instmaps in `InstMaps' for which the insts of the variable is
    % incompatible.
    %
:- pred instmap__unify_2(list(prog_var)::in, instmap::in,
    list(pair(instmap, set(prog_var)))::in, module_info::in,
    map(prog_var, inst)::in, module_info::out,
    map(prog_var, inst)::out, merge_errors::out) is det.

instmap__unify_2([], _, _, ModuleInfo, InstMap, ModuleInfo, InstMap, []).
instmap__unify_2([Var|Vars], InitialInstMap, InstMapList, ModuleInfo0, InstMap0,
        ModuleInfo, InstMap, ErrorList) :-
    instmap__unify_2(Vars, InitialInstMap, InstMapList, ModuleInfo0,
        InstMap0, ModuleInfo1, InstMap1, ErrorList1),
    instmap__lookup_var(InitialInstMap, Var, InitialVarInst),
    instmap__unify_var(InstMapList, Var, [], Insts, InitialVarInst, Inst,
        ModuleInfo1, ModuleInfo, no, Error),
    (
        Error = yes,
        ErrorList = [Var - Insts | ErrorList1]
    ;
        Error = no,
        ErrorList = ErrorList1
    ),
    map__set(InstMap1, Var, Inst, InstMap).

    % instmap__unify_var(InstMaps, Var, InitialInstMap, ModuleInfo,
    %   Insts, Error):
    %
    % Let `Insts' be the list of the inst of `Var' in each of the
    % corresponding `InstMaps'. Let `Error' be yes iff there are two
    % instmaps for which the inst of `Var' is incompatible.
    %
:- pred instmap__unify_var(list(pair(instmap, set(prog_var)))::in,
    prog_var::in, list(inst)::in, list(inst)::out, (inst)::in, (inst)::out,
    module_info::in, module_info::out, bool::in, bool::out) is det.

instmap__unify_var([], _, !Insts, !Inst, !ModuleInfo, !Error).
instmap__unify_var([InstMap - Nonlocals| Rest], Var, !InstList, !Inst,
        !ModuleInfo, !Error) :-
    ( set__member(Var, Nonlocals) ->
        instmap__lookup_var(InstMap, Var, VarInst),
        (
            % We can ignore the determinism of the unification:
            % if it isn't det, then there will be a mode error
            % or a determinism error in one of the parallel conjuncts.
            abstractly_unify_inst(live, !.Inst, VarInst,
                fake_unify, !:Inst, _Det, !ModuleInfo)
        ->
            true
        ;
            !:Error = yes,
            !:Inst = not_reached
        )
    ;
        VarInst = free
    ),
    !:InstList = [VarInst | !.InstList],
    instmap__unify_var(Rest, Var, !InstList, !Inst, !ModuleInfo, !Error).

%-----------------------------------------------------------------------------%

compute_instmap_delta(unreachable, _, _, unreachable).
compute_instmap_delta(reachable(_), unreachable, _, unreachable).
compute_instmap_delta(reachable(InstMapA), reachable(InstMapB), NonLocals,
        reachable(DeltaInstMap)) :-
    set__to_sorted_list(NonLocals, NonLocalsList),
    compute_instmap_delta_2(NonLocalsList, InstMapA, InstMapB, AssocList),
    map__from_sorted_assoc_list(AssocList, DeltaInstMap).

:- pred compute_instmap_delta_2(list(prog_var)::in, instmapping::in,
    instmapping::in, assoc_list(prog_var, inst)::out) is det.

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

:- pred instmap__no_output_vars_2(list(prog_var)::in, instmap::in,
    instmapping::in, vartypes::in, module_info::in) is semidet.

instmap__no_output_vars_2([], _, _, _, _).
instmap__no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, VarTypes,
        ModuleInfo) :-
    % We use `inst_matches_binding' to check that the new inst has only
    % added information or lost uniqueness, not bound anything.
    % If the instmap delta contains the variable, the variable may still
    % not be output, if the change is just an increase in information
    % rather than an increase in instantiatedness. If the instmap delta
    % doesn't contain the variable, it may still have been (partially) output,
    % if its inst is (or contains) `any'.
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

merge_instmap_delta(_, _, _, unreachable, InstMapDelta, InstMapDelta,
        !ModuleInfo).
merge_instmap_delta(_, _, _, reachable(InstMapping), unreachable,
        reachable(InstMapping), !ModuleInfo).
merge_instmap_delta(InstMap, NonLocals, VarTypes, reachable(InstMappingA),
        reachable(InstMappingB), reachable(InstMapping), !ModuleInfo) :-
    merge_instmapping_delta(InstMap, NonLocals, VarTypes,
        InstMappingA, InstMappingB, InstMapping, !ModuleInfo).

:- pred merge_instmapping_delta(instmap::in, set(prog_var)::in, vartypes::in,
    instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

merge_instmapping_delta(InstMap, NonLocals, VarTypes,
        InstMappingA, InstMappingB, InstMapping, !ModuleInfo) :-
    map__keys(InstMappingA, VarsInA),
    map__keys(InstMappingB, VarsInB),
    set__sorted_list_to_set(VarsInA, SetofVarsInA),
    set__insert_list(SetofVarsInA, VarsInB, SetofVars0),
    set__intersect(SetofVars0, NonLocals, SetofVars),
    set__to_sorted_list(SetofVars, ListofVars),
    merge_instmapping_delta_2(ListofVars, InstMap, VarTypes,
        InstMappingA, InstMappingB, map__init, InstMapping, !ModuleInfo).

:- pred merge_instmapping_delta_2(list(prog_var)::in, instmap::in,
    vartypes::in, instmapping::in, instmapping::in,
    instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

merge_instmapping_delta_2([], _, _, _, _, !InstMapping, !ModuleInfo).
merge_instmapping_delta_2([Var | Vars], InstMap, VarTypes,
        InstMappingA, InstMappingB, !InstMapping, !ModuleInfo) :-
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
        inst_merge(InstA, InstB, yes(VarTypes ^ det_elem(Var)), Inst1,
            !ModuleInfo)
    ->
        % XXX Given instmap__lookup_var(InstMap, Var, OldInst),
        % we should probably set Inst not directly from Inst1, but
        % from a conjunction of OldInst and Inst1. If OldInst says that
        % Var is bound to f, and Inst1 says that it is bound to g,
        % Inst should be `unreachable', not bound(g). If OldInst says
        % that Var is bound to f or g, and Inst1 says that it is bound
        % to g or h, Inst should say that it is bound(g).
        %
        % If there is an invariant to the effect that such situations
        % are not supposed to arise, then it is being broken, due to
        % the XXX in recompute_instmap_delta_unify in mode_util.m.
        %
        % At present, I believe that the cases we mishandle here can
        % arise only after inlining, as in puzzle_detism_bug.m in
        % tests/hard_coded. -zs

        Inst = Inst1,
        svmap__det_insert(Var, Inst, !InstMapping)
    ;
        term__var_to_int(Var, VarInt),
        string__format("merge_instmapping_delta_2: error merging var %i",
            [i(VarInt)], Msg),
        error(Msg)
    ),
    merge_instmapping_delta_2(Vars, InstMap, VarTypes,
        InstMappingA, InstMappingB, !InstMapping, !ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given two instmap deltas, unify them to produce a new instmap_delta.

unify_instmap_delta(_, _, unreachable, InstMapDelta, InstMapDelta, !ModuleInfo).
unify_instmap_delta(_, _, reachable(InstMapping), unreachable,
        reachable(InstMapping), !ModuleInfo).
unify_instmap_delta(InstMap, NonLocals, reachable(InstMappingA),
        reachable(InstMappingB), reachable(InstMapping), !ModuleInfo) :-
    unify_instmapping_delta(InstMap, NonLocals, InstMappingA, InstMappingB,
        InstMapping, !ModuleInfo).

:- pred unify_instmapping_delta(instmap::in, set(prog_var)::in,
    instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

unify_instmapping_delta(InstMap, NonLocals, InstMappingA, InstMappingB,
        InstMapping, !ModuleInfo) :-
    map__keys(InstMappingA, VarsInA),
    map__keys(InstMappingB, VarsInB),
    set__sorted_list_to_set(VarsInA, SetofVarsInA),
    set__insert_list(SetofVarsInA, VarsInB, SetofVars0),
    set__intersect(SetofVars0, NonLocals, SetofVars),
    set__to_sorted_list(SetofVars, ListofVars),
    unify_instmapping_delta_2(ListofVars, InstMap, InstMappingA, InstMappingB,
        map__init, InstMapping, !ModuleInfo).

:- pred unify_instmapping_delta_2(list(prog_var)::in, instmap::in,
    instmapping::in, instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

unify_instmapping_delta_2([], _, _, _, !InstMapping, !ModuleInfo).
unify_instmapping_delta_2([Var | Vars], InstMap, InstMappingA, InstMappingB,
        !InstMapping, !ModuleInfo) :-
    ( map__search(InstMappingA, Var, InstA) ->
        ( map__search(InstMappingB, Var, InstB) ->
            (
                % We can ignore the determinism of the unification: if it
                % isn't det, then there will be a mode error or a determinism
                % error in one of the parallel conjuncts.

                abstractly_unify_inst(live, InstA, InstB,
                    fake_unify, Inst, _Det, !ModuleInfo)
            ->
                svmap__det_insert(Var, Inst, !InstMapping)
            ;
                error("unify_instmapping_delta_2: unexpected error")
            )
        ;
            svmap__det_insert(Var, InstA, !InstMapping)
        )
    ;
        ( map__search(InstMappingB, Var, InstB) ->
            svmap__det_insert(Var, InstB, !InstMapping)
        ;
            true
        )
    ),
    unify_instmapping_delta_2(Vars, InstMap, InstMappingA, InstMappingB,
        !InstMapping, !ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_delta_apply_sub(unreachable, _Must, _Sub, unreachable).
instmap_delta_apply_sub(reachable(OldInstMapping), Must, Sub,
        reachable(InstMapping)) :-
    map__to_assoc_list(OldInstMapping, InstMappingAL),
    instmap_delta_apply_sub_2(InstMappingAL, Must, Sub,
        map__init, InstMapping).

instmap__apply_sub(InstMap0, Must, Sub, InstMap) :-
    instmap_delta_apply_sub(InstMap0, Must, Sub, InstMap).

:- pred instmap_delta_apply_sub_2(assoc_list(prog_var, inst)::in, bool::in,
    map(prog_var, prog_var)::in, instmapping::in, instmapping::out) is det.

instmap_delta_apply_sub_2([], _Must, _Sub, IM, IM).
instmap_delta_apply_sub_2([V - I | AL], Must, Sub, IM0, IM) :-
    ( map__search(Sub, V, N0) ->
        N = N0
    ;
        (
            Must = no,
            N = V
        ;
            Must = yes,
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

instmap__to_assoc_list(unreachable, []).
instmap__to_assoc_list(reachable(InstMapping), AL) :-
    map__to_assoc_list(InstMapping, AL).

instmap_delta_to_assoc_list(unreachable, []).
instmap_delta_to_assoc_list(reachable(InstMapping), AL) :-
    map__to_assoc_list(InstMapping, AL).

%-----------------------------------------------------------------------------%

var_is_ground_in_instmap(ModuleInfo, InstMap, Var) :-
    instmap__lookup_var(InstMap, Var, Inst),
    inst_is_ground(ModuleInfo, Inst).

%-----------------------------------------------------------------------------%
