%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: instmap.m.
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

:- module hlds.instmap.
:- interface.

:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_info.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.

:- type instmap.
:- type instmap_delta.

    % Initialize an empty instmap.
    %
:- pred init_reachable(instmap::out) is det.

    % Initialize an empty unreachable instmap.
    %
:- pred init_unreachable(instmap::out) is det.

    % Initialize an empty reachable instmap_delta.
    %
:- pred instmap_delta_init_reachable(instmap_delta::out) is det.

    % Initialize an empty unreachable instmap_delta.
    %
:- pred instmap_delta_init_unreachable(instmap_delta::out) is det.

    % For any instmap InstMap, exactly one of
    % is_reachable(InstMap) and
    % is_unreachable(InstMap) holds.

    % Is the instmap reachable?
    %
:- pred is_reachable(instmap::in) is semidet.

    % Is the instmap unreachable?
    %
:- pred is_unreachable(instmap::in) is semidet.

    % For any instmap InstMapDelta, exactly one of
    % instmap_delta_is_reachable(InstMapDelta) and
    % instmap_delta_is_unreachable(InstMapDelta) holds.

    % Is the instmap_delta reachable?
    %
:- pred instmap_delta_is_reachable(instmap_delta::in) is semidet.

    % Is the instmap_delta unreachable?
    %
:- pred instmap_delta_is_unreachable(instmap_delta::in) is semidet.

:- pred from_assoc_list(assoc_list(prog_var, mer_inst)::in,
    instmap::out) is det.

:- pred instmap_delta_from_assoc_list(assoc_list(prog_var, mer_inst)::in,
    instmap_delta::out) is det.

:- pred instmap_delta_from_mode_list(list(prog_var)::in, list(mer_mode)::in,
    module_info::in, instmap_delta::out) is det.

%-----------------------------------------------------------------------------%

    % Return the set of variables in an instmap.
    %
:- pred vars(instmap::in, set(prog_var)::out) is det.

    % Return the list of variables in an instmap.
    %
:- pred vars_list(instmap::in, list(prog_var)::out) is det.

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
:- pred lookup_var(instmap::in, prog_var::in, mer_inst::out) is det.

    % Given an instmap_delta and a variable, determine the new inst
    % of that variable (if any).
    %
:- pred instmap_delta_search_var(instmap_delta::in, prog_var::in,
    mer_inst::out) is semidet.

    % Given an instmap and a list of variables, return a list
    % containing the insts of those variable.
    %
:- pred lookup_vars(list(prog_var)::in, instmap::in,
    list(mer_inst)::out) is det.

    % Insert an entry into an instmap_delta. Note that you cannot call
    % instmap_delta_insert for a variable already present.
    %
:- pred instmap_delta_insert(prog_var::in, mer_inst::in,
    instmap_delta::in, instmap_delta::out) is det.

    % Set an entry in an instmap.
    %
:- pred set(prog_var::in, mer_inst::in, instmap::in, instmap::out)
    is det.

    % Set multiple entries in an instmap.
    %
:- pred set_vars(list(prog_var)::in, list(mer_inst)::in,
    instmap::in, instmap::out) is det.

:- pred instmap_delta_set(prog_var::in, mer_inst::in,
    instmap_delta::in, instmap_delta::out) is det.

    % Bind a variable in an instmap to a functor at the beginning
    % of a case in a switch. Aborts on compiler generated cons_ids.
    %
:- pred instmap_delta_bind_var_to_functor(prog_var::in, mer_type::in,
    cons_id::in, instmap::in, instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

:- pred bind_var_to_functor(prog_var::in, mer_type::in, cons_id::in,
    instmap::in, instmap::out, module_info::in, module_info::out) is det.

    % Update the given instmap to include the initial insts of the
    % lambda variables.
    %
:- pred pre_lambda_update(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, instmap::in, instmap::out) is det.

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
:- pred apply_instmap_delta(instmap::in, instmap_delta::in,
    instmap::out) is det.

    % Given two instmap_deltas, overlay the entries in the second instmap_delta
    % on top of those in the first to produce a new instmap_delta.
    %
:- pred instmap_delta_apply_instmap_delta(instmap_delta::in, instmap_delta::in,
    overlay_how::in, instmap_delta::out) is det.

    % instmap_merge(NonLocalVars, InstMaps, MergeContext, !ModeInfo):
    %
    % Merge the `InstMaps' resulting from different branches of a disjunction
    % or if-then-else, and update the instantiatedness of all the nonlocal
    % variables, checking that it is the same for every branch.
    %
:- pred instmap_merge(set(prog_var)::in, list(instmap)::in, merge_context::in,
    mode_info::in, mode_info::out) is det.

    % instmap_unify(NonLocalVars, InstMapNonlocalvarPairs, !ModeInfo):
    %
    % Unify the `InstMaps' in the list of pairs resulting from different
    % branches of a parallel conjunction and update the instantiatedness
    % of all the nonlocal variables. The variable locking that is done
    % when modechecking the individual conjuncts ensures that variables
    % have at most one producer.
    %
:- pred instmap_unify(set(prog_var)::in,
    assoc_list(instmap, set(prog_var))::in,
    mode_info::in, mode_info::out) is det.

    % instmap_restrict takes an instmap and a set of vars and returns
    % an instmap with its domain restricted to those vars.
    %
:- pred instmap_restrict(set(prog_var)::in, instmap::in, instmap::out) is det.

    % instmap_delta_restrict takes an instmap and a set of vars and returns
    % an instmap_delta with its domain restricted to those vars.
    %
:- pred instmap_delta_restrict(set(prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

    % instmap_delta_delete_vars takes an instmap_delta and a list of vars
    % and returns an instmap_delta with those vars removed from its domain.
    %
:- pred instmap_delta_delete_vars(list(prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

    % `no_output_vars(Instmap, InstmapDelta, Vars, ModuleInfo)'
    % is true if none of the vars in the set Vars could have become more
    % instantiated when InstmapDelta is applied to Instmap.
    %
:- pred no_output_vars(instmap::in, instmap_delta::in,
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

    % merge_instmap_deltas(Vars, InstMapDeltas, MergedInstMapDelta,
    %   !ModuleInfo):
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

    % `instmap_delta_apply_sub(Must, Sub, InstmapDelta0, InstmapDelta)'
    % the variable substitution Sub to InstmapDelta0 to get the new
    % instmap_delta InstmapDelta.  If there is a variable in
    % InstmapDelta0 which does not appear in Sub, it is ignored if
    % Must is set to no, otherwise it is an error.
    %
:- pred instmap_delta_apply_sub(bool::in, map(prog_var, prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

:- pred apply_sub(bool::in, map(prog_var, prog_var)::in,
    instmap::in, instmap::out) is det.

%-----------------------------------------------------------------------------%

:- pred to_assoc_list(instmap::in,
    assoc_list(prog_var, mer_inst)::out) is det.

:- pred instmap_delta_to_assoc_list(instmap_delta::in,
    assoc_list(prog_var, mer_inst)::out) is det.

    % Apply the specified procedure to all insts in an instmap_delta.
    %
:- pred instmap_delta_map_foldl(
    pred(prog_var, mer_inst, mer_inst, T, T)::in(pred(in, in, out, in, out)
        is det),
    instmap_delta::in, instmap_delta::out, T::in, T::out) is det.

:- pred var_is_ground_in_instmap(module_info::in, instmap::in, prog_var::in)
    is semidet.

    % var_is_bound_in_instmap_delta(ModuleInfo, InstMap,
    %   InstMapDelta, Var)
    %
    % Succeed if Var is a variable bound between InstMap and
    % InstMap+InstMapDelta.
    %
:- pred var_is_bound_in_instmap_delta(module_info::in, instmap::in,
    instmap_delta::in, prog_var::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type instmap_delta   ==  instmap.

:- type instmap
    --->    reachable(instmapping)
    ;       unreachable.

:- type instmapping ==  map(prog_var, mer_inst).

%-----------------------------------------------------------------------------%

    % Initialize an empty instmap and instmap_delta.

init_reachable(reachable(InstMapping)) :-
    map.init(InstMapping).

init_unreachable(unreachable).

instmap_delta_init_reachable(reachable(InstMapping)) :-
    map.init(InstMapping).

instmap_delta_init_unreachable(unreachable).

%-----------------------------------------------------------------------------%

is_reachable(reachable(_)).

is_unreachable(unreachable).

instmap_delta_is_reachable(reachable(_)).

instmap_delta_is_unreachable(unreachable).

%-----------------------------------------------------------------------------%

from_assoc_list(AL, reachable(Instmapping)) :-
    map.from_assoc_list(AL, Instmapping).

instmap_delta_from_assoc_list(AL, reachable(Instmapping)) :-
    map.from_assoc_list(AL, Instmapping).

instmap_delta_map_foldl(_, unreachable, unreachable, !T).
instmap_delta_map_foldl(P, reachable(Instmapping0), reachable(Instmapping),
        !T) :-
    map.map_foldl(P, Instmapping0, Instmapping, !T).

%-----------------------------------------------------------------------------%

instmap_delta_from_mode_list(Var, Modes, ModuleInfo, InstMapDelta) :-
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_from_mode_list_2(Var, Modes, ModuleInfo,
        InstMapDelta0, InstMapDelta).

:- pred instmap_delta_from_mode_list_2(list(prog_var)::in, list(mer_mode)::in,
    module_info::in, instmap_delta::in, instmap_delta::out) is det.

instmap_delta_from_mode_list_2([], [], _, !InstMapDelta).
instmap_delta_from_mode_list_2([], [_ | _], _, !InstMapDelta) :-
    unexpected(this_file, "instmap_delta_from_mode_list_2").
instmap_delta_from_mode_list_2([_ | _], [], _, !InstMapDelta) :-
    unexpected(this_file, "instmap_delta_from_mode_list_2").
instmap_delta_from_mode_list_2([Var | Vars], [Mode | Modes], ModuleInfo,
        !InstMapDelta) :-
    mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
    ( Inst1 = Inst2 ->
        instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo, !InstMapDelta)
    ;
        instmap_delta_set(Var, Inst2, !InstMapDelta),
        instmap_delta_from_mode_list_2(Vars, Modes, ModuleInfo, !InstMapDelta)
    ).

%-----------------------------------------------------------------------------%

vars(Instmap, Vars) :-
    vars_list(Instmap, VarsList),
    set.list_to_set(VarsList, Vars).

vars_list(unreachable, []).
vars_list(reachable(InstMapping), VarsList) :-
    map.keys(InstMapping, VarsList).

instmap_delta_changed_vars(unreachable, EmptySet) :-
    set.init(EmptySet).
instmap_delta_changed_vars(reachable(InstMapping), ChangedVars) :-
    map.keys(InstMapping, ChangedVarsList),
    set.sorted_list_to_set(ChangedVarsList, ChangedVars).

%-----------------------------------------------------------------------------%

instmap_changed_vars(InstMapA, InstMapB, VarTypes, ModuleInfo, ChangedVars) :-
    vars_list(InstMapB, VarsB),
    changed_vars_2(VarsB, InstMapA, InstMapB, VarTypes, ModuleInfo,
        ChangedVars).

:- pred changed_vars_2(prog_vars::in, instmap::in, instmap::in, vartypes::in,
    module_info::in, set(prog_var)::out) is det.

changed_vars_2([], _InstMapA, _InstMapB, _Types, _ModuleInfo, ChangedVars) :-
    set.init(ChangedVars).
changed_vars_2([VarB | VarBs], InstMapA, InstMapB, VarTypes, ModuleInfo,
        ChangedVars) :-
    changed_vars_2(VarBs, InstMapA, InstMapB, VarTypes, ModuleInfo,
        ChangedVars0),

    lookup_var(InstMapA, VarB, InitialInst),
    lookup_var(InstMapB, VarB, FinalInst),
    map.lookup(VarTypes, VarB, Type),

    ( inst_matches_final(InitialInst, FinalInst, Type, ModuleInfo) ->
        ChangedVars = ChangedVars0
    ;
        set.insert(ChangedVars0, VarB, ChangedVars)
    ).

%-----------------------------------------------------------------------------%

lookup_var(unreachable, _Var, not_reached).
lookup_var(reachable(InstMap), Var, Inst) :-
    instmapping_lookup_var(InstMap, Var, Inst).

:- pred instmapping_lookup_var(instmapping::in, prog_var::in, mer_inst::out)
    is det.

instmapping_lookup_var(InstMap, Var, Inst) :-
    ( map.search(InstMap, Var, VarInst) ->
        Inst = VarInst
    ;
        Inst = free
    ).

instmap_delta_search_var(unreachable, _, not_reached).
instmap_delta_search_var(reachable(InstMap), Var, Inst) :-
    map.search(InstMap, Var, Inst).

lookup_vars([], _InstMap, []).
lookup_vars([Arg|Args], InstMap, [Inst|Insts]) :-
    lookup_var(InstMap, Arg, Inst),
    lookup_vars(Args, InstMap, Insts).

set(_Var, _Inst, unreachable, unreachable).
set(Var, Inst, reachable(InstMapping0), reachable(InstMapping)) :-
    map.set(InstMapping0, Var, Inst, InstMapping).

set_vars([], [], !InstMap).
set_vars([V | Vs], [I | Is], !InstMap) :-
    set(V, I, !InstMap),
    set_vars(Vs, Is, !InstMap).
set_vars([_ | _], [], !InstMap) :-
    unexpected(this_file, "set_vars: length mismatch (1)").
set_vars([], [_ | _], !InstMap) :-
    unexpected(this_file, "set_vars: length mismatch (2)").

instmap_delta_set(_Var, _Inst, unreachable, unreachable).
instmap_delta_set(Var, Inst, reachable(InstMapping0), Instmap) :-
    ( Inst = not_reached ->
        Instmap = unreachable
    ;
        map.set(InstMapping0, Var, Inst, InstMapping),
        Instmap = reachable(InstMapping)
    ).

instmap_delta_insert(_Var, _Inst, unreachable, unreachable).
instmap_delta_insert(Var, Inst, reachable(InstMapping0), Instmap) :-
    ( Inst = not_reached ->
        Instmap = unreachable
    ;
        map.det_insert(InstMapping0, Var, Inst, InstMapping),
        Instmap = reachable(InstMapping)
    ).

%-----------------------------------------------------------------------------%

instmap_delta_bind_var_to_functor(Var, Type, ConsId, InstMap, !InstmapDelta,
        !ModuleInfo) :-
    (
        !.InstmapDelta = unreachable
    ;
        !.InstmapDelta = reachable(InstmappingDelta0),

        % Get the initial inst from the InstMap
        lookup_var(InstMap, Var, OldInst),

        % Compute the new inst by taking the old inst, applying the instmap
        % delta to it, and then unifying with bound(ConsId, ...).
        ( map.search(InstmappingDelta0, Var, NewInst0) ->
            NewInst1 = NewInst0
        ;
            NewInst1 = OldInst
        ),
        bind_inst_to_functor(Type, ConsId, NewInst1, NewInst, !ModuleInfo),

        % Add `Var :: OldInst -> NewInst' to the instmap delta.
        ( NewInst \= OldInst ->
            instmap_delta_set(Var, NewInst, !InstmapDelta)
        ;
            true
        )
    ).

bind_var_to_functor(Var, Type, ConsId, !InstMap, !ModuleInfo) :-
    lookup_var(!.InstMap, Var, Inst0),
    bind_inst_to_functor(Type, ConsId, Inst0, Inst, !ModuleInfo),
    set(Var, Inst, !InstMap).

:- pred bind_inst_to_functor(mer_type::in, cons_id::in,
    mer_inst::in, mer_inst::out, module_info::in, module_info::out) is det.

bind_inst_to_functor(Type, ConsId, !Inst, !ModuleInfo) :-
    Arity = cons_id_adjusted_arity(!.ModuleInfo, Type, ConsId),
    list.duplicate(Arity, dead, ArgLives),
    list.duplicate(Arity, free, ArgInsts),
    (
        abstractly_unify_inst_functor(dead, !.Inst, ConsId, ArgInsts,
            ArgLives, real_unify, Type, !:Inst, _Det, !ModuleInfo)
    ->
        true
    ;
        unexpected(this_file, "bind_inst_to_functor: mode error")
    ).

%-----------------------------------------------------------------------------%

pre_lambda_update(ModuleInfo, Vars, Modes, InstMap0, InstMap) :-
    mode_list_get_initial_insts(ModuleInfo, Modes, Insts),
    assoc_list.from_corresponding_lists(Vars, Insts, VarInsts),
    instmap_delta_from_assoc_list(VarInsts, InstMapDelta),
    apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

%-----------------------------------------------------------------------------%

apply_instmap_delta(unreachable, _, unreachable).
apply_instmap_delta(reachable(_), unreachable, unreachable).
apply_instmap_delta(reachable(InstMapping0),
        reachable(InstMappingDelta), reachable(InstMapping)) :-
    map.overlay(InstMapping0, InstMappingDelta, InstMapping).

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
            map.overlay(InstMappingDelta1, InstMappingDelta2,
                InstMappingDelta)
        ;
            How = large_overlay,
            map.overlay_large_map(InstMappingDelta1,
                InstMappingDelta2, InstMappingDelta)
        ;
            How = test_size,
            (
                map.count(InstMappingDelta1, Count1),
                map.count(InstMappingDelta2, Count2),
                Count1 >= Count2
            ->
                map.overlay(InstMappingDelta1,
                    InstMappingDelta2, InstMappingDelta)
            ;
                map.overlay_large_map(InstMappingDelta1,
                    InstMappingDelta2, InstMappingDelta)
            )
        ),
        InstMap = reachable(InstMappingDelta)
    ).

%-----------------------------------------------------------------------------%

instmap_restrict(_, unreachable, unreachable).
instmap_restrict(Vars, reachable(InstMapping0), reachable(InstMapping)) :-
    map.select(InstMapping0, Vars, InstMapping).

instmap_delta_restrict(_, unreachable, unreachable).
instmap_delta_restrict(Vars,
        reachable(InstMapping0), reachable(InstMapping)) :-
    map.select(InstMapping0, Vars, InstMapping).

instmap_delta_delete_vars(_, unreachable, unreachable).
instmap_delta_delete_vars(Vars,
        reachable(InstMapping0), reachable(InstMapping)) :-
    map.delete_list(InstMapping0, Vars, InstMapping).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_merge(NonLocals, InstMapList, MergeContext, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    get_reachable_instmaps(InstMapList, InstMappingList),
    (
        % We can reach the code after the branched control structure only if
        % (a) we can reach its start, and (b) some branch can reach the end.
        InstMap0 = reachable(InstMapping0),
        InstMappingList = [_ | _]
    ->
        set.to_sorted_list(NonLocals, NonLocalsList),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        merge_2(NonLocalsList, InstMapList, VarTypes,
            InstMapping0, InstMapping, ModuleInfo0, ModuleInfo, ErrorList),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),
        (
            ErrorList = [FirstError | _],
            FirstError = Var - _,
            set.singleton_set(WaitingVars, Var),
            mode_info_error(WaitingVars,
                mode_error_disj(MergeContext, ErrorList), !ModeInfo)
        ;
            ErrorList = []
        ),
        InstMap = reachable(InstMapping)
    ;
        InstMap = unreachable
    ),
    mode_info_set_instmap(InstMap, !ModeInfo).

:- pred get_reachable_instmaps(list(instmap)::in, list(instmapping)::out)
    is det.

get_reachable_instmaps([], []).
get_reachable_instmaps([InstMap | InstMaps], Reachables) :-
    (
        InstMap = reachable(InstMapping),
        get_reachable_instmaps(InstMaps, ReachablesTail),
        Reachables = [InstMapping | ReachablesTail]
    ;
        InstMap = unreachable,
        get_reachable_instmaps(InstMaps, Reachables)
    ).

%-----------------------------------------------------------------------------%

    % merge_2(Vars, InstMapList, VarTypes, !InstMapping, !ModuleInfo,
    %   Errors):
    %
    % Given Vars, a list of variables, and InstMapList, a list of instmaps
    % giving the insts of those variables (and possibly others) at the ends of
    % a branched control structure such as a disjunction or if-then-else,
    % update !InstMapping, which initially gives the insts of variables at the
    % start of the branched control structure, to reflect their insts at its
    % end.
    %
    % For variables mentioned in Vars, merge their insts and put the merged
    % inst into !:InstMapping. For variables not in Vars, leave their insts in
    % !.InstMapping alone.
    %
    % If some variables in Vars have incompatible insts in two or more instmaps
    % in InstMapList, return them in `Errors'.
    %
:- pred merge_2(list(prog_var)::in, list(instmap)::in, vartypes::in,
    instmapping::in, instmapping::out, module_info::in, module_info::out,
    merge_errors::out) is det.

merge_2([], _, _, !InstMap, !ModuleInfo, []).
merge_2([Var | Vars], InstMapList, VarTypes, !InstMapping,
        !ModuleInfo, !:ErrorList) :-
    merge_2(Vars, InstMapList, VarTypes, !InstMapping, !ModuleInfo,
        !:ErrorList),
    map.lookup(VarTypes, Var, VarType),
    list.map(lookup_var_in_instmap(Var), InstMapList, InstList),
    merge_var(InstList, Var, VarType, !ModuleInfo, MaybeInst),
    (
        MaybeInst = no,
        !:ErrorList = [Var - InstList | !.ErrorList],
        svmap.set(Var, not_reached, !InstMapping)
    ;
        MaybeInst = yes(Inst),
        svmap.set(Var, Inst, !InstMapping)
    ).

:- pred lookup_var_in_instmap(prog_var::in, instmap::in, mer_inst::out) is det.

lookup_var_in_instmap(Var, InstMap, Inst) :-
    lookup_var(InstMap, Var, Inst).

    % merge_var(Insts, Var, Type, Inst, !ModuleInfo, !Error):
    %
    % Given a list of insts of the given variable that reflect the inst of that
    % variable at the ends of a branched control structure such as a
    % disjunction or if-then-else, return the final inst of that variable
    % after the branched control structure as a whole.
    %
    % Set !:Error to yes if two insts of the variable are incompatible.
    %
    % We used to use a straightforward algorithm that, given a list of N insts,
    % merged the tail N-1 insts, and merged the result with the head inst.
    % While this is simple and efficient for small N, it has very bad
    % performance for large N. The reason is that its complexity can be N^2,
    % since in many cases each arm of the branched control structure binds
    % Var to a different function symbol, and this means that the inst of Var
    % evolves like this:
    %
    %   bound(f)
    %   bound(f; g)
    %   bound(f; g; h)
    %   bound(f; g; h; i)
    %
    % Our current algorithm uses a number of passes, each of which divides the
    % number of insts by four by merging groups of four adjacent insts.
    % The overall complexity is thus closer to N log N than N^2.
    %
:- pred merge_var(list(mer_inst)::in, prog_var::in, mer_type::in,
    module_info::in, module_info::out, maybe(mer_inst)::out) is det.

merge_var(Insts, Var, Type, !ModuleInfo, MaybeMergedInst) :-
    % Construct yes(Type) here once per merge_var pass to avoid merge_var_2 
    % constructing the yes(Type) cell N times per pass.
    merge_var_2(Insts, Var, yes(Type), [], MergedInsts, !ModuleInfo,
        no, Error),
    (
        Error = yes,
        MaybeMergedInst = no
    ;
        Error = no,
        (
            MergedInsts = [],
            MaybeMergedInst = yes(not_reached)
        ;
            MergedInsts = [MergedInst],
            MaybeMergedInst = yes(MergedInst)
        ;
            MergedInsts = [_, _ | _],
            merge_var(MergedInsts, Var, Type, !ModuleInfo, MaybeMergedInst)
        )
    ).

:- pred merge_var_2(list(mer_inst)::in, prog_var::in, maybe(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out, module_info::in, module_info::out,
    bool::in, bool::out) is det.

merge_var_2(Insts, Var, YesType, !MergedInsts, !ModuleInfo, !Error) :-
    (
        Insts = []
    ;
        Insts = [Inst1],
        !:MergedInsts = [Inst1 | !.MergedInsts]
    ;
        Insts = [Inst1, Inst2],
        (
            inst_merge(Inst1, Inst2, YesType, Inst12, !ModuleInfo)
        ->
            !:MergedInsts = [Inst12 | !.MergedInsts]
        ;
            !:Error = yes
        )
    ;
        Insts = [Inst1, Inst2, Inst3],
        (
            inst_merge(Inst1, Inst2, YesType, Inst12, !ModuleInfo),
            inst_merge(Inst12, Inst3, YesType, Inst123, !ModuleInfo)
        ->
            !:MergedInsts = [Inst123 | !.MergedInsts]
        ;
            !:Error = yes
        )
    ;
        Insts = [Inst1, Inst2, Inst3, Inst4 | MoreInsts],
        (
            inst_merge(Inst1, Inst2, YesType, Inst12, !ModuleInfo),
            inst_merge(Inst3, Inst4, YesType, Inst34, !ModuleInfo),
            inst_merge(Inst12, Inst34, YesType, Inst1234, !ModuleInfo)
        ->
            !:MergedInsts = [Inst1234 | !.MergedInsts],
            merge_var_2(MoreInsts, Var, YesType, !MergedInsts, !ModuleInfo,
                !Error)
        ;
            !:Error = yes
        )
    ).

%-----------------------------------------------------------------------------%

    % We use the same technique for merge_instmap_deltas as for merge_var,
    % and for the same reason.
merge_instmap_deltas(InstMap, NonLocals, VarTypes, Deltas,
        MergedDelta, !ModuleInfo) :-
    merge_instmap_deltas_2(InstMap, NonLocals, VarTypes, Deltas,
        [], MergedDeltas, !ModuleInfo),
    (
        MergedDeltas = [],
        unexpected(this_file,
            "merge_instmap_deltas: empty instmap_delta list.")
    ;
        MergedDeltas = [MergedDelta]
    ;
        MergedDeltas = [_, _ | _],
        merge_instmap_deltas(InstMap, NonLocals, VarTypes, MergedDeltas,
            MergedDelta, !ModuleInfo)
    ).

:- pred merge_instmap_deltas_2(instmap::in, set(prog_var)::in, vartypes::in,
    list(instmap_delta)::in, list(instmap_delta)::in, list(instmap_delta)::out,
    module_info::in, module_info::out) is det.

merge_instmap_deltas_2(InstMap, NonLocals, VarTypes, Deltas,
        !MergedDeltas, !ModuleInfo) :-
    (
        Deltas = []
    ;
        Deltas = [Delta1],
        !:MergedDeltas = [Delta1 | !.MergedDeltas]
    ;
        Deltas = [Delta1, Delta2],
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta1, Delta2,
            Delta12, !ModuleInfo),
        !:MergedDeltas = [Delta12 | !.MergedDeltas]
    ;
        Deltas = [Delta1, Delta2, Delta3],
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta1, Delta2,
            Delta12, !ModuleInfo),
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta12, Delta3,
            Delta123, !ModuleInfo),
        !:MergedDeltas = [Delta123 | !.MergedDeltas]
    ;
        Deltas = [Delta1, Delta2, Delta3, Delta4 | MoreDeltas],
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta1, Delta2,
            Delta12, !ModuleInfo),
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta3, Delta4,
            Delta34, !ModuleInfo),
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta12, Delta34,
            Delta1234, !ModuleInfo),
        !:MergedDeltas = [Delta1234 | !.MergedDeltas],
        merge_instmap_deltas_2(InstMap, NonLocals, VarTypes, MoreDeltas,
            !MergedDeltas, !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

instmap_unify(NonLocals, InstMapList, !ModeInfo) :-
    (
        % If any of the instmaps is unreachable, then the final instmap
        % is unreachable.
        list.member(unreachable - _, InstMapList)
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
        % all unify_2 which unifies each of the nonlocals from
        % each instmap with the corresponding inst in the accumulator.
        mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
        set.to_sorted_list(NonLocals, NonLocalsList),
        unify_2(NonLocalsList, InstMap0, InstMapList1,
            ModuleInfo0, ModuleInfo, InstMapping0, InstMapping, ErrorList),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),

        % If there were any errors, then add the error to the list
        % of possible errors in the mode_info.
        (
            ErrorList = [FirstError | _],
            FirstError = Var - _,
            set.singleton_set(WaitingVars, Var),
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

    % unify_2(Vars, InitialInstMap, InstMaps, !ModuleInfo,
    %   !Instmap, ErrorList):
    %
    % Let `ErrorList' be the list of variables in `Vars' for which there are
    % two instmaps in `InstMaps' for which the insts of the variable is
    % incompatible.
    %
:- pred unify_2(list(prog_var)::in, instmap::in,
    list(pair(instmap, set(prog_var)))::in, module_info::in, module_info::out,
    map(prog_var, mer_inst)::in, map(prog_var, mer_inst)::out,
    merge_errors::out) is det.

unify_2([], _, _, !ModuleInfo, !InstMap, []).
unify_2([Var|Vars], InitialInstMap, InstMapList,
        !ModuleInfo, !InstMap, ErrorList) :-
    unify_2(Vars, InitialInstMap, InstMapList, !ModuleInfo, !InstMap,
        ErrorListTail),
    lookup_var(InitialInstMap, Var, InitialVarInst),
    unify_var(InstMapList, Var, [], Insts, InitialVarInst, Inst,
        !ModuleInfo, no, Error),
    (
        Error = yes,
        ErrorList = [Var - Insts | ErrorListTail]
    ;
        Error = no,
        ErrorList = ErrorListTail
    ),
    map.set(!.InstMap, Var, Inst, !:InstMap).

    % unify_var(InstMaps, Var, InitialInstMap, ModuleInfo,
    %   Insts, Error):
    %
    % Let `Insts' be the list of the inst of `Var' in each of the
    % corresponding `InstMaps'. Let `Error' be yes iff there are two
    % instmaps for which the inst of `Var' is incompatible.
    %
:- pred unify_var(list(pair(instmap, set(prog_var)))::in,
    prog_var::in, list(mer_inst)::in, list(mer_inst)::out,
    mer_inst::in, mer_inst::out, module_info::in, module_info::out,
    bool::in, bool::out) is det.

unify_var([], _, !Insts, !Inst, !ModuleInfo, !Error).
unify_var([InstMap - Nonlocals| Rest], Var, !InstList, !Inst,
        !ModuleInfo, !Error) :-
    ( set.member(Var, Nonlocals) ->
        lookup_var(InstMap, Var, VarInst),
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
    unify_var(Rest, Var, !InstList, !Inst, !ModuleInfo, !Error).

%-----------------------------------------------------------------------------%

compute_instmap_delta(unreachable, _, _, unreachable).
compute_instmap_delta(reachable(_), unreachable, _, unreachable).
compute_instmap_delta(reachable(InstMapA), reachable(InstMapB), NonLocals,
        reachable(DeltaInstMap)) :-
    set.to_sorted_list(NonLocals, NonLocalsList),
    compute_instmap_delta_2(NonLocalsList, InstMapA, InstMapB, AssocList),
    map.from_sorted_assoc_list(AssocList, DeltaInstMap).

:- pred compute_instmap_delta_2(list(prog_var)::in, instmapping::in,
    instmapping::in, assoc_list(prog_var, mer_inst)::out) is det.

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

no_output_vars(_, unreachable, _, _, _).
no_output_vars(InstMap0, reachable(InstMapDelta), Vars, VT, M) :-
    set.to_sorted_list(Vars, VarList),
    no_output_vars_2(VarList, InstMap0, InstMapDelta, VT, M).

:- pred no_output_vars_2(list(prog_var)::in, instmap::in,
    instmapping::in, vartypes::in, module_info::in) is semidet.

no_output_vars_2([], _, _, _, _).
no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, VarTypes,
        ModuleInfo) :-
    % We use `inst_matches_binding' to check that the new inst has only
    % added information or lost uniqueness, not bound anything.
    % If the instmap delta contains the variable, the variable may still
    % not be output, if the change is just an increase in information
    % rather than an increase in instantiatedness. If the instmap delta
    % doesn't contain the variable, it may still have been (partially) output,
    % if its inst is (or contains) `any'.
    lookup_var(InstMap0, Var, Inst0),
    ( map.search(InstMapDelta, Var, Inst1) ->
        Inst = Inst1
    ;
        Inst = Inst0
    ),
    map.lookup(VarTypes, Var, Type),
    inst_matches_binding(Inst, Inst0, Type, ModuleInfo),
    no_output_vars_2(Vars, InstMap0, InstMapDelta, VarTypes, ModuleInfo).

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
    map.keys(InstMappingA, VarsInA),
    map.keys(InstMappingB, VarsInB),
    set.sorted_list_to_set(VarsInA, SetofVarsInA),
    set.insert_list(SetofVarsInA, VarsInB, SetofVars0),
    set.intersect(SetofVars0, NonLocals, SetofVars),
    set.to_sorted_list(SetofVars, ListofVars),
    merge_instmapping_delta_2(ListofVars, InstMap, VarTypes,
        InstMappingA, InstMappingB, map.init, InstMapping, !ModuleInfo).

:- pred merge_instmapping_delta_2(list(prog_var)::in, instmap::in,
    vartypes::in, instmapping::in, instmapping::in,
    instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

merge_instmapping_delta_2([], _, _, _, _, !InstMapping, !ModuleInfo).
merge_instmapping_delta_2([Var | Vars], InstMap, VarTypes,
        InstMappingA, InstMappingB, !InstMapping, !ModuleInfo) :-
    ( map.search(InstMappingA, Var, InstInA) ->
        InstA = InstInA
    ;
        lookup_var(InstMap, Var, InstA)
    ),
    ( map.search(InstMappingB, Var, InstInB) ->
        InstB = InstInB
    ;
        lookup_var(InstMap, Var, InstB)
    ),
    (
        inst_merge(InstA, InstB, yes(VarTypes ^ det_elem(Var)), Inst1,
            !ModuleInfo)
    ->
        % XXX Given lookup_var(InstMap, Var, OldInst),
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
        svmap.det_insert(Var, Inst, !InstMapping)
    ;
        term.var_to_int(Var, VarInt),
        string.format("merge_instmapping_delta_2: error merging var %i",
            [i(VarInt)], Msg),
        unexpected(this_file, Msg)
    ),
    merge_instmapping_delta_2(Vars, InstMap, VarTypes,
        InstMappingA, InstMappingB, !InstMapping, !ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given two instmap deltas, unify them to produce a new instmap_delta.
    %
unify_instmap_delta(_, _, unreachable, InstMapDelta, InstMapDelta,
        !ModuleInfo).
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
    map.keys(InstMappingA, VarsInA),
    map.keys(InstMappingB, VarsInB),
    set.sorted_list_to_set(VarsInA, SetofVarsInA),
    set.insert_list(SetofVarsInA, VarsInB, SetofVars0),
    set.intersect(SetofVars0, NonLocals, SetofVars),
    set.to_sorted_list(SetofVars, ListofVars),
    unify_instmapping_delta_2(ListofVars, InstMap, InstMappingA, InstMappingB,
        map.init, InstMapping, !ModuleInfo).

:- pred unify_instmapping_delta_2(list(prog_var)::in, instmap::in,
    instmapping::in, instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

unify_instmapping_delta_2([], _, _, _, !InstMapping, !ModuleInfo).
unify_instmapping_delta_2([Var | Vars], InstMap, InstMappingA, InstMappingB,
        !InstMapping, !ModuleInfo) :-
    ( map.search(InstMappingA, Var, InstA) ->
        ( map.search(InstMappingB, Var, InstB) ->
            (
                % We can ignore the determinism of the unification: if it
                % isn't det, then there will be a mode error or a determinism
                % error in one of the parallel conjuncts.

                abstractly_unify_inst(live, InstA, InstB,
                    fake_unify, Inst, _Det, !ModuleInfo)
            ->
                svmap.det_insert(Var, Inst, !InstMapping)
            ;
                unexpected(this_file,
                    "unify_instmapping_delta_2: unexpected error")
            )
        ;
            svmap.det_insert(Var, InstA, !InstMapping)
        )
    ;
        ( map.search(InstMappingB, Var, InstB) ->
            svmap.det_insert(Var, InstB, !InstMapping)
        ;
            true
        )
    ),
    unify_instmapping_delta_2(Vars, InstMap, InstMappingA, InstMappingB,
        !InstMapping, !ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

instmap_delta_apply_sub(_Must, _Sub, unreachable, unreachable).
instmap_delta_apply_sub(Must, Sub,
        reachable(OldInstMapping), reachable(InstMapping)) :-
    map.to_assoc_list(OldInstMapping, InstMappingAL),
    instmap_delta_apply_sub_2(InstMappingAL, Must, Sub,
        map.init, InstMapping).

apply_sub(Must, Sub, InstMap0, InstMap) :-
    instmap_delta_apply_sub(Must, Sub, InstMap0, InstMap).

:- pred instmap_delta_apply_sub_2(assoc_list(prog_var, mer_inst)::in, bool::in,
    map(prog_var, prog_var)::in, instmapping::in, instmapping::out) is det.

instmap_delta_apply_sub_2([], _Must, _Sub, IM, IM).
instmap_delta_apply_sub_2([V - I | AL], Must, Sub, IM0, IM) :-
    ( map.search(Sub, V, N0) ->
        N = N0
    ;
        (
            Must = no,
            N = V
        ;
            Must = yes,
            unexpected(this_file, "instmap_delta_apply_sub_2: no substitute")
        )
    ),
    % XXX temporary hack alert XXX
    % This should be a call to to map.det_insert, rather than a call
    % to map.set. However, if we do that, then the compiler breaks,
    % due to a problem with excess.m not preserving super-homogenous form.
    map.set(IM0, N, I, IM1),
    instmap_delta_apply_sub_2(AL, Must, Sub, IM1, IM).

%-----------------------------------------------------------------------------%

to_assoc_list(unreachable, []).
to_assoc_list(reachable(InstMapping), AL) :-
    map.to_assoc_list(InstMapping, AL).

instmap_delta_to_assoc_list(unreachable, []).
instmap_delta_to_assoc_list(reachable(InstMapping), AL) :-
    map.to_assoc_list(InstMapping, AL).

%-----------------------------------------------------------------------------%

var_is_ground_in_instmap(ModuleInfo, InstMap, Var) :-
    lookup_var(InstMap, Var, Inst),
    inst_is_ground(ModuleInfo, Inst).

var_is_bound_in_instmap_delta(ModuleInfo, InstMap, InstMapDelta, Var) :-
    instmap.lookup_var(InstMap, Var, OldVarInst),
    inst_is_free(ModuleInfo, OldVarInst),
    instmap_delta_search_var(InstMapDelta, Var, VarInst),
    inst_is_bound(ModuleInfo, VarInst).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "instmap.m".

%-----------------------------------------------------------------------------%
