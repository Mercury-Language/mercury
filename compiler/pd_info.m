%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2001, 2003-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pd_info.m.
% Main author: stayl.
%
% Types for deforestation.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.pd_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.pd_term.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%---------------------------------------------------------------------------%

:- type pd_info
    --->    pd_info(
                pdi_module_info         :: module_info,
                pdi_maybe_unfold_info   :: maybe(unfold_info),
                pdi_goal_version_index  :: goal_version_index,
                pdi_versions            :: version_index,
                pdi_proc_arg_info       :: pd_arg_info,
                pdi_counter             :: counter,
                pdi_global_term_info    :: global_term_info,
                pdi_parent_versions     :: set(pred_proc_id),
                pdi_depth               :: int,
                pdi_created_versions    :: set(pred_proc_id),
                pdi_useless_versions    :: useless_versions
            ).

    % Map from list of called preds in the conjunctions
    % to the specialised versions.
    %
:- type goal_version_index == map(list(pred_proc_id), list(pred_proc_id)).

:- type useless_versions == set(pair(pred_proc_id)).

    % Map from version id to the info about the version.
    %
:- type version_index == map(pred_proc_id, version_info).

:- pred pd_info_init(module_info::in, pd_arg_info::in, pd_info::out) is det.

:- pred pd_info_init_unfold_info(pred_proc_id::in, pred_info::in,
    proc_info::in, pd_info::in, pd_info::out) is det.

:- pred pd_info_get_module_info(pd_info::in, module_info::out) is det.
:- pred pd_info_get_unfold_info(pd_info::in, unfold_info::out) is det.
:- pred pd_info_get_goal_version_index(pd_info::in, goal_version_index::out)
    is det.
:- pred pd_info_get_versions(pd_info::in, version_index::out) is det.
:- pred pd_info_get_proc_arg_info(pd_info::in, pd_arg_info::out) is det.
:- pred pd_info_get_counter(pd_info::in, counter::out) is det.
:- pred pd_info_get_global_term_info(pd_info::in, global_term_info::out)
    is det.
:- pred pd_info_get_parent_versions(pd_info::in, set(pred_proc_id)::out)
    is det.
:- pred pd_info_get_depth(pd_info::in, int::out) is det.
:- pred pd_info_get_created_versions(pd_info::in, set(pred_proc_id)::out)
    is det.
:- pred pd_info_get_useless_versions(pd_info::in, useless_versions::out)
    is det.

:- pred pd_info_set_module_info(module_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_unfold_info(unfold_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_unset_unfold_info(
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_goal_version_index(goal_version_index::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_versions(version_index::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_proc_arg_info(pd_arg_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_counter(counter::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_global_term_info(global_term_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_parent_versions(set(pred_proc_id)::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_depth(int::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_created_versions(set(pred_proc_id)::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_useless_versions(useless_versions::in,
    pd_info::in, pd_info::out) is det.

:- pred pd_info_update_goal(hlds_goal::in, pd_info::in, pd_info::out) is det.

:- pred pd_info_bind_var_to_functors(prog_var::in,
    cons_id::in, list(cons_id)::in, pd_info::in, pd_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.modecheck_util.
:- import_module hlds.pred_name.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.pd_debug.
:- import_module transform_hlds.pd_util.

:- import_module int.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

pd_info_init(ModuleInfo, ProcArgInfos, PDInfo) :-
    map.init(GoalVersionIndex),
    map.init(Versions),
    set.init(ParentVersions),
    pd_term.global_term_info_init(GlobalInfo),
    set.init(CreatedVersions),
    set.init(UselessVersions),
    PDInfo = pd_info(ModuleInfo, no, GoalVersionIndex, Versions,
        ProcArgInfos, counter.init(0), GlobalInfo, ParentVersions,
        0, CreatedVersions, UselessVersions).

pd_info_init_unfold_info(PredProcId, PredInfo, ProcInfo, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    get_constrained_inst_vars(ModuleInfo, ArgModes, HeadInstVars),
    proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap),
    CostDelta = 0,
    pd_term.local_term_info_init(LocalTermInfo),
    Parents = set.make_singleton_set(PredProcId),
    UnfoldInfo = unfold_info(ProcInfo, HeadInstVars, InstMap, CostDelta,
        LocalTermInfo, PredInfo, Parents, PredProcId, 0, no, no),
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).

pd_info_get_module_info(PDInfo, PDInfo ^ pdi_module_info).
pd_info_get_unfold_info(PDInfo, UnfoldInfo) :-
    MaybeUnfoldInfo = PDInfo ^ pdi_maybe_unfold_info,
    (
        MaybeUnfoldInfo = yes(UnfoldInfo)
    ;
        MaybeUnfoldInfo = no,
        unexpected($pred, "unfold_info not set")
    ).
pd_info_get_goal_version_index(PDInfo, PDInfo ^ pdi_goal_version_index).
pd_info_get_versions(PDInfo, PDInfo ^ pdi_versions).
pd_info_get_proc_arg_info(PDInfo, PDInfo ^ pdi_proc_arg_info).
pd_info_get_counter(PDInfo, PDInfo ^ pdi_counter).
pd_info_get_global_term_info(PDInfo, PDInfo ^ pdi_global_term_info).
pd_info_get_parent_versions(PDInfo, PDInfo ^ pdi_parent_versions).
pd_info_get_depth(PDInfo, PDInfo ^ pdi_depth).
pd_info_get_created_versions(PDInfo, PDInfo ^ pdi_created_versions).
pd_info_get_useless_versions(PDInfo, PDInfo ^ pdi_useless_versions).

pd_info_set_module_info(ModuleInfo, !PDInfo) :-
    !PDInfo ^ pdi_module_info := ModuleInfo.
pd_info_set_unfold_info(UnfoldInfo, !PDInfo) :-
    !PDInfo ^ pdi_maybe_unfold_info := yes(UnfoldInfo).
pd_info_unset_unfold_info(!PDInfo) :-
    !PDInfo ^ pdi_maybe_unfold_info := no.
pd_info_set_goal_version_index(Index, !PDInfo) :-
    !PDInfo ^ pdi_goal_version_index := Index.
pd_info_set_versions(Versions, !PDInfo) :-
    !PDInfo ^ pdi_versions := Versions.
pd_info_set_proc_arg_info(ProcArgInfo, !PDInfo) :-
    !PDInfo ^ pdi_proc_arg_info := ProcArgInfo.
pd_info_set_counter(Counter, !PDInfo) :-
    !PDInfo ^ pdi_counter := Counter.
pd_info_set_global_term_info(TermInfo, !PDInfo) :-
    !PDInfo ^ pdi_global_term_info := TermInfo.
pd_info_set_parent_versions(Parents, !PDInfo) :-
    !PDInfo ^ pdi_parent_versions := Parents.
pd_info_set_depth(Depth, !PDInfo) :-
    !PDInfo ^ pdi_depth := Depth.
pd_info_set_created_versions(Versions, !PDInfo) :-
    !PDInfo ^ pdi_created_versions := Versions.
pd_info_set_useless_versions(Versions, !PDInfo) :-
    !PDInfo ^ pdi_useless_versions := Versions.

pd_info_update_goal(hlds_goal(_, GoalInfo), !PDInfo) :-
    pd_info_get_instmap(!.PDInfo, InstMap0),
    Delta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(Delta, InstMap0, InstMap),
    pd_info_set_instmap(InstMap, !PDInfo).

pd_info_bind_var_to_functors(Var, MainConsId, OtherConsIds, !PDInfo) :-
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    lookup_var_type(VarTable, Var, Type),
    bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap0, InstMap, ModuleInfo0, ModuleInfo),
    pd_info_set_instmap(InstMap, !PDInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%---------------------------------------------------------------------------%

:- interface.

    % unfold_info contains information used while searching a procedure
    % body for unfolding and deforestation opportunities.
:- type unfold_info
    --->    unfold_info(
                ufi_proc_info       :: proc_info,
                ufi_head_inst_vars  :: map(inst_var, mer_inst),
                ufi_instmap         :: instmap,

                % Improvement in cost measured while processing this procedure.
                ufi_cost_delta      :: int,

                % Information used to prevent infinite unfolding within the
                % current procedure..
                ufi_local_term_info :: local_term_info,

                ufi_pred_info       :: pred_info,
                ufi_parents         :: set(pred_proc_id),

                % Current pred_proc_id.
                ufi_pred_proc_id    :: pred_proc_id,

                % Increase in size measured while processing this procedure.
                ufi_size_delta      :: int,

                % Has anything changed?
                ufi_changed         :: bool,

                % Does determinism analysis need to be rerun.
                ufi_rerun_det       :: bool
            ).

    % pd_arg_info records which procedures have arguments for which
    % it might be worthwhile to attempt deforestation if there
    % is extra information about them, and the branches of the single
    % branched goal in the top level conjunction which produce that extra
    % information.
:- type pd_arg_info == map(pred_proc_id, pd_proc_arg_info).

:- type pd_proc_arg_info    ==  pd_branch_info(int).

:- type pd_branch_info(T)
    --->    pd_branch_info(
                branch_info_map(T),

                % variables for which we want extra left context
                set(T),

                % outputs for which we have no extra information
                set(T)
            ).

    % Vars for which there is extra information at the end
    % of some branches, and the branches which add the extra
    % information (numbered from 1).
:- type branch_info_map(T)  ==  map(T, set(int)).

:- pred pd_info_get_proc_info(pd_info::in, proc_info::out) is det.
:- pred pd_info_get_head_inst_vars(pd_info::in, map(inst_var, mer_inst)::out)
    is det.
:- pred pd_info_get_instmap(pd_info::in, instmap::out) is det.
:- pred pd_info_get_cost_delta(pd_info::in, int::out) is det.
:- pred pd_info_get_local_term_info(pd_info::in, local_term_info::out) is det.
:- pred pd_info_get_pred_info(pd_info::in, pred_info::out) is det.
:- pred pd_info_get_parents(pd_info::in, set(pred_proc_id)::out) is det.
:- pred pd_info_get_pred_proc_id(pd_info::in, pred_proc_id::out) is det.
:- pred pd_info_get_size_delta(pd_info::in, int::out) is det.
:- pred pd_info_get_changed(pd_info::in, bool::out) is det.
:- pred pd_info_get_rerun_det(pd_info::in, bool::out) is det.

:- pred pd_info_set_proc_info(proc_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_instmap(instmap::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_cost_delta(int::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_local_term_info(local_term_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_pred_info(pred_info::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_parents(set(pred_proc_id)::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_pred_proc_id(pred_proc_id::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_size_delta(int::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_changed(bool::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_set_rerun_det(bool::in,
    pd_info::in, pd_info::out) is det.

:- pred pd_info_incr_cost_delta(int::in,
    pd_info::in, pd_info::out) is det.
:- pred pd_info_incr_size_delta(int::in,
    pd_info::in, pd_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

pd_info_get_proc_info(PDInfo, UnfoldInfo ^ ufi_proc_info) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_head_inst_vars(PDInfo, UnfoldInfo ^ ufi_head_inst_vars) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_instmap(PDInfo, UnfoldInfo ^ ufi_instmap) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_cost_delta(PDInfo, UnfoldInfo ^ ufi_cost_delta) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_local_term_info(PDInfo, UnfoldInfo ^ ufi_local_term_info) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_pred_info(PDInfo, UnfoldInfo ^ ufi_pred_info) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_parents(PDInfo, UnfoldInfo ^ ufi_parents) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_pred_proc_id(PDInfo, UnfoldInfo ^ ufi_pred_proc_id) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_size_delta(PDInfo, UnfoldInfo ^ ufi_size_delta) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_changed(PDInfo, UnfoldInfo ^ ufi_changed) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).
pd_info_get_rerun_det(PDInfo, UnfoldInfo ^ ufi_rerun_det) :-
    pd_info_get_unfold_info(PDInfo, UnfoldInfo).

pd_info_set_proc_info(ProcInfo, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_proc_info := ProcInfo,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_instmap(InstMap, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_instmap := InstMap,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_cost_delta(CostDelta, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_cost_delta := CostDelta,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_local_term_info(TermInfo, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_local_term_info := TermInfo,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_pred_info(PredInfo, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_pred_info := PredInfo,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_parents(Parents, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_parents := Parents,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_pred_proc_id(PredProcId, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_pred_proc_id := PredProcId,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_size_delta(SizeDelta, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_size_delta := SizeDelta,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_changed(Changed, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_changed := Changed,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).
pd_info_set_rerun_det(Rerun, !PDInfo) :-
    pd_info_get_unfold_info(!.PDInfo, UnfoldInfo0),
    UnfoldInfo = UnfoldInfo0 ^ ufi_rerun_det := Rerun,
    pd_info_set_unfold_info(UnfoldInfo, !PDInfo).

pd_info_incr_cost_delta(Delta1, !PDInfo) :-
    pd_info_get_cost_delta(!.PDInfo, Delta0),
    Delta = Delta0 + Delta1,
    pd_info_set_cost_delta(Delta, !PDInfo).

pd_info_incr_size_delta(Delta1, !PDInfo) :-
    pd_info_get_size_delta(!.PDInfo, Delta0),
    Delta = Delta0 + Delta1,
    pd_info_set_size_delta(Delta, !PDInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % Find the deforestation procedure which most closely matches
    % the given goal.
    %
:- pred pd_info_search_version(pd_info::in, hlds_goal::in, maybe_version::out)
    is det.

    % Create a new predicate for the input goal, returning a goal
    % which calls the new predicate.
    %
:- pred pd_info_define_new_pred(hlds_goal::in,
    pred_proc_id::out, hlds_goal::out, pd_info::in, pd_info::out) is det.

    % Add a version to the table.
    %
:- pred pd_info_register_version(pred_proc_id::in, version_info::in,
    pd_info::in, pd_info::out) is det.

    % Remove a version and make sure it is never recreated.
    %
:- pred pd_info_invalidate_version(pred_proc_id::in,
    pd_info::in, pd_info::out) is det.

    % Remove a version, but allow it to be recreated if it
    % is used elsewhere.
    %
:- pred pd_info_remove_version(pred_proc_id::in,
    pd_info::in, pd_info::out) is det.

    % The result of looking up a specialised version of a pred.
:- type maybe_version
    --->    no_version
    ;       version(
                mv_is_exact         :: version_is_exact,
                mv_ppid             :: pred_proc_id,
                mv_version          :: version_info,

                % renaming of the version info
                mv_renaming         :: map(prog_var, prog_var),

                % var types substitution
                mv_tsubst           :: tsubst
            ).

:- type version_is_exact
    --->    exact
    ;       more_general.

:- type version_info
    --->    version_info(
                % goal before unfolding.
                version_orig_goal   :: hlds_goal,

                % calls being deforested.
                version_deforest_calls  :: list(pred_proc_id),

                % arguments.
                version_arg_vars    :: list(prog_var),

                % argument types.
                version_arg_types   :: list(mer_type),

                % initial insts of the nonlocals.
                version_init_insts  :: instmap,

                % cost of the original goal.
                version_orig_cost   :: int,

                % improvement in cost.
                version_cost_improv :: int,

                % parent versions.
                version_parents     :: set(pred_proc_id),

                % the version which was generalised to produce this version.
                version_source      :: maybe(pred_proc_id)
            ).

%---------------------------------------------------------------------------%

:- implementation.

pd_info_search_version(PDInfo, Goal, MaybeVersion) :-
    trace [io(!IO)] (
        pd_debug_output_goal(PDInfo, "Searching for version:\n", Goal, !IO)
    ),
    pd_util.goal_get_calls(Goal, CalledPreds),
    pd_info_get_versions(PDInfo, Versions),
    pd_info_get_goal_version_index(PDInfo, GoalVersionIndex),
    pd_info_get_module_info(PDInfo, ModuleInfo),
    pd_info_get_proc_info(PDInfo, ProcInfo),
    pd_info_get_instmap(PDInfo, InstMap),
    proc_info_get_var_table(ProcInfo, VarTable),
    ( if
        map.search(GoalVersionIndex, CalledPreds, VersionIds),
        pd_info_get_matching_version(ModuleInfo, Goal, InstMap,
            VarTable, VersionIds, Versions, MaybeVersion0)
    then
        MaybeVersion = MaybeVersion0
    else
        MaybeVersion = no_version
    ),
    trace [io(!IO)] (
        pd_debug_search_version_result(PDInfo, MaybeVersion, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred pd_info_get_matching_version(module_info::in, hlds_goal::in,
    instmap::in, var_table::in, list(pred_proc_id)::in,
    version_index::in, maybe_version::out) is semidet.

pd_info_get_matching_version(_, _, _, _, [], _, no_version).
pd_info_get_matching_version(ModuleInfo, ThisGoal, ThisInstMap, VarTable,
        [VersionId | VersionIds], Versions, MaybeVersion) :-
    map.lookup(Versions, VersionId, Version),
    Version = version_info(OldGoal, _, OldArgs, OldArgTypes,
        OldInstMap, _, _, _, _),
    ( if
        pd_info_goal_is_more_general(ModuleInfo, OldGoal, OldInstMap, OldArgs,
            OldArgTypes, ThisGoal, ThisInstMap, VarTable, VersionId, Version,
            MaybeVersion1)
    then
        (
            MaybeVersion1 = no_version,
            pd_info_get_matching_version(ModuleInfo, ThisGoal, ThisInstMap,
                VarTable, VersionIds, Versions, MaybeVersion)
        ;
            MaybeVersion1 = version(exact, _, _, _, _),
            MaybeVersion = MaybeVersion1
        ;
            MaybeVersion1 = version(more_general, PredProcId,
                MoreGeneralVersion, Renaming, TypeSubn),
            pd_info_get_matching_version(ModuleInfo, ThisGoal,
                ThisInstMap, VarTable, VersionIds,
                Versions, MaybeVersion2),
            pd_info_pick_version(ModuleInfo, PredProcId, Renaming,
                TypeSubn, MoreGeneralVersion, MaybeVersion2, MaybeVersion)
        )
    else
        pd_info_get_matching_version(ModuleInfo, ThisGoal, ThisInstMap,
            VarTable, VersionIds, Versions, MaybeVersion)
    ).

%---------------------------------------------------------------------------%

    % Choose between two versions.
    %
:- pred pd_info_pick_version(module_info::in, pred_proc_id::in,
    map(prog_var, prog_var)::in, tsubst::in, version_info::in,
    maybe_version::in, maybe_version::out) is det.

pd_info_pick_version(_, PredProcId, Renaming, TSubn, VersionInfo, no_version,
    version(more_general, PredProcId, VersionInfo, Renaming, TSubn)).
pd_info_pick_version(_, _, _, _, _,
        version(exact, PredProcId, Version2, Renaming2, TSubn2),
        version(exact, PredProcId, Version2, Renaming2, TSubn2)).
pd_info_pick_version(_ModuleInfo, PredProcId1, Renaming1, TSubn1, Version1,
        version(more_general, PredProcId2, Version2, Renaming2, TSubn2),
        MaybeVersion) :-
    Version1 = version_info(_, _, _, _, _, _, CostDelta1, _, _),
    Version2 = version_info(_, _, _, _, _, _, CostDelta2, _, _),
    % Select the version with the biggest decrease in cost.
    ( if CostDelta1 > CostDelta2 then
        MaybeVersion = version(more_general, PredProcId1,
            Version1, Renaming1, TSubn1)
    else
        MaybeVersion = version(more_general, PredProcId2,
            Version2, Renaming2, TSubn2)
    ).

%---------------------------------------------------------------------------%

    % The aim of this is to check whether the first goal can be used
    % instead of the second if specialisation on the second goal does
    % not produce any more improvement.
    %
    % An old version is more general than a new one if:
    % - the goals have the same "shape" (see pd_util.goals_match).
    % - each variable in the old goal maps to exactly one
    %   variable in the new (multiple vars in the new goal can
    %   map to one var in the old).
    % - each nonlocal in the new goal maps to a non-local in the
    %   old (i.e. the old version produces all the variables
    %   that the new one does).
    % - for each pair of corresponding insts in the above mapping,
    %   the old inst must be at least as general as the
    %   new one, i.e inst_matches_initial(FirstInst, SecondInst) (?)
    %
:- pred pd_info_goal_is_more_general(module_info::in, hlds_goal::in,
    instmap::in, list(prog_var)::in, list(mer_type)::in, hlds_goal::in,
    instmap::in, var_table::in, pred_proc_id::in,
    version_info::in, maybe_version::out) is semidet.

pd_info_goal_is_more_general(ModuleInfo, OldGoal, OldInstMap, OldArgs,
        OldArgTypes, NewGoal, NewInstMap, NewVarTable, PredProcId,
        Version, MaybeVersion) :-
    pd_util.goals_match(ModuleInfo, OldGoal, OldArgs, OldArgTypes,
        NewGoal, NewVarTable, OldNewRenaming, TypeRenaming),
    OldGoal = hlds_goal(_, OldGoalInfo),
    OldNonLocals0 = goal_info_get_nonlocals(OldGoalInfo),
    set_of_var.to_sorted_list(OldNonLocals0, OldNonLocalsList),
    pd_info_check_insts(ModuleInfo, OldNonLocalsList, OldNewRenaming,
        OldInstMap, NewInstMap, NewVarTable, exact, Exact),

    MaybeVersion = version(Exact, PredProcId, Version,
        OldNewRenaming, TypeRenaming).

%---------------------------------------------------------------------------%

    % Check that all the insts in the old version are at least as
    % general as the insts in the new version.
    %
:- pred pd_info_check_insts(module_info::in, list(prog_var)::in,
    map(prog_var, prog_var)::in, instmap::in, instmap::in, var_table::in,
    version_is_exact::in, version_is_exact::out) is semidet.

pd_info_check_insts(_, [], _, _, _, _, !ExactSoFar).
pd_info_check_insts(ModuleInfo, [OldVar | Vars], VarRenaming, OldInstMap,
        NewInstMap, VarTable, !ExactSoFar) :-
    instmap_lookup_var(OldInstMap, OldVar, OldVarInst),
    map.lookup(VarRenaming, OldVar, NewVar),
    instmap_lookup_var(NewInstMap, NewVar, NewVarInst),
    lookup_var_type(VarTable, NewVar, Type),
    inst_matches_initial(ModuleInfo, Type, NewVarInst, OldVarInst),
    (
        !.ExactSoFar = exact,
        % Does inst_matches_initial(Inst1, Inst2, M) and
        % inst_matches_initial(Inst2, Inst1, M) imply that Inst1
        % and Inst2 are interchangable?
        ( if
            inst_matches_initial(ModuleInfo, Type, OldVarInst, NewVarInst)
        then
            !:ExactSoFar = exact
        else
            !:ExactSoFar = more_general
        )
    ;
        !.ExactSoFar = more_general
    ),
    pd_info_check_insts(ModuleInfo, Vars, VarRenaming, OldInstMap,
        NewInstMap, VarTable, !ExactSoFar).

%---------------------------------------------------------------------------%

pd_info_define_new_pred(Goal, PredProcId, CallGoal, !PDInfo) :-
    pd_info_get_instmap(!.PDInfo, InstMap),
    Goal = hlds_goal(_, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(NonLocals, Args),
    pd_info_get_counter(!.PDInfo, Counter0),
    counter.allocate(SeqNum, Counter0, Counter),
    pd_info_set_counter(Counter, !PDInfo),
    pd_info_get_pred_info(!.PDInfo, PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Context = goal_info_get_context(GoalInfo),
    LineNum = term_context.context_line(Context),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    Origin = origin_compiler(made_for_deforestation(LineNum, SeqNum)),
    Transform = tn_deforestation(pf_predicate, lnc(LineNum, SeqNum)),
    make_transformed_pred_sym_name(PredModule, PredName, Transform,
        NewPredSymName),

    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_class_context(PredInfo, ClassContext),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    proc_info_get_var_name_remap(ProcInfo, VarNameRemap),
    % XXX handle the extra typeinfo arguments for
    % --typeinfo-liveness properly.
    hlds_pred.define_new_pred(NewPredSymName, Origin, TVarSet, InstVarSet,
        VarTable, RttiVarMaps, ClassContext, InstMap, VarNameRemap, Markers,
        address_is_not_taken, HasParallelConj, PredProcId, Args, _ExtraArgs,
        Goal, CallGoal, ModuleInfo0, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%---------------------------------------------------------------------------%

pd_info_register_version(PredProcId, Version, !PDInfo) :-
    trace [io(!IO)] (
        pd_debug_register_version(!.PDInfo, PredProcId, Version, !IO)
    ),
    pd_info_get_goal_version_index(!.PDInfo, GoalVersionIndex0),
    Goal = Version ^ version_orig_goal,
    pd_util.goal_get_calls(Goal, Calls),
    ( if map.search(GoalVersionIndex0, Calls, VersionList0) then
        VersionList = [PredProcId | VersionList0],
        map.det_update(Calls, VersionList, GoalVersionIndex0, GoalVersionIndex)
    else
        VersionList = [PredProcId],
        map.det_insert(Calls, VersionList, GoalVersionIndex0, GoalVersionIndex)
    ),
    pd_info_set_goal_version_index(GoalVersionIndex, !PDInfo),
    pd_info_get_versions(!.PDInfo, Versions0),
    map.det_insert(PredProcId, Version, Versions0, Versions),
    pd_info_set_versions(Versions, !PDInfo),
    pd_info_get_created_versions(!.PDInfo, CreatedVersions0),
    set.insert(PredProcId, CreatedVersions0, CreatedVersions),
    pd_info_set_created_versions(CreatedVersions, !PDInfo).

%---------------------------------------------------------------------------%

pd_info_invalidate_version(PredProcId, !PDInfo) :-
    pd_info_get_versions(!.PDInfo, Versions0),
    map.lookup(Versions0, PredProcId, Version),
    Goal = Version ^ version_orig_goal,
    pd_util.goal_get_calls(Goal, Calls),
    ( if
        Calls = [FirstCall | _],
        list.last(Calls, LastCall)
    then
        % Make sure we never create another version to deforest
        % this pair of calls.
        pd_info_get_useless_versions(!.PDInfo, Useless0),
        set.insert(FirstCall - LastCall, Useless0, Useless),
        pd_info_set_useless_versions(Useless, !PDInfo)
    else
        true
    ),
    pd_info_remove_version(PredProcId, !PDInfo).

pd_info_remove_version(PredProcId, !PDInfo) :-
    pd_info_get_versions(!.PDInfo, Versions0),
    map.lookup(Versions0, PredProcId, Version),
    Goal = Version ^ version_orig_goal,
    pd_util.goal_get_calls(Goal, Calls),
    map.delete(PredProcId, Versions0, Versions),
    pd_info_set_versions(Versions, !PDInfo),

    pd_info_get_goal_version_index(!.PDInfo, GoalIndex0),
    ( if map.search(GoalIndex0, Calls, GoalVersions0) then
        list.delete_all(GoalVersions0, PredProcId, GoalVersions),
        map.det_update(Calls, GoalVersions, GoalIndex0, GoalIndex),
        pd_info_set_goal_version_index(GoalIndex, !PDInfo)
    else
        true
    ),

    pd_info_get_created_versions(!.PDInfo, CreatedVersions0),
    set.delete(PredProcId, CreatedVersions0, CreatedVersions),
    pd_info_set_created_versions(CreatedVersions, !PDInfo),

    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    PredProcId = proc(PredId, _),
    module_info_remove_predicate(PredId, ModuleInfo0, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.pd_info.
%---------------------------------------------------------------------------%
