%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod_mark_exported.m.
% Main author: stayl (the original intermod.m).
%
% This module contains predicates to adjust the import status of local
% predicates which are exported for intermodule optimization. We do this
% when intermodule analysis is enabled, but the current compiler invocation
% is not being asked to create the module's .opt file.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod_mark_exported.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.intermod_info.

%---------------------------------------------------------------------------%

    % Find out which predicates would be opt-exported, and mark them
    % accordingly. (See the comment on do_maybe_opt_export_entities
    % for why we do this.)
    %
:- pred maybe_opt_export_entities(module_info::in, module_info::out) is det.

    % Change the status of the entities (predicates, types, insts, modes,
    % classes and instances) listed as opt-exported in the given intermod_info
    % to opt-exported. This affects how the rest of the compiler treats
    % these entities. For example, the entry labels at the starts of
    % the C code fragments we generate for an opt-exported local predicate
    % needs to be exported from the .c file, and opt-exported procedures
    % should not be touched by dead proc elimination.
    %
    % The reason why we have a separate pass for this, instead of changing
    % the status of an item to reflect the fact that it is opt-exported
    % at the same time as we decide to opt-export it, is that the decision
    % to opt-export e.g. a procedure takes place inside invocations of
    % mmc --make-opt-int, but we also need the same status updates
    % in invocations of mmc that generate target language code.
    %
:- pred maybe_opt_export_listed_entities(intermod_info::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.intermod_decide.
:- import_module transform_hlds.intermod_status.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

maybe_opt_export_entities(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    trace [io(!IO)] (
        get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
        maybe_write_string(ProgressStream, VeryVerbose,
            "% Adjusting import status of predicates in the `.opt' file...",
            !IO)
    ),
    decide_what_to_opt_export(!.ModuleInfo, IntermodInfo),
    maybe_opt_export_listed_entities(IntermodInfo, !ModuleInfo),
    trace [io(!IO)] (
        get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
        maybe_write_string(ProgressStream, VeryVerbose, " done\n", !IO)
    ).

maybe_opt_export_listed_entities(IntermodInfo, !ModuleInfo) :-
    % XXX This would be clearer as well as faster if we gathered up
    % the pred_ids of all the predicates that we found we need to opt_export
    % while processing type, typeclass and instance definitions,
    % and then opt_exported them all at once.
    intermod_info_get_pred_decls(IntermodInfo, PredDeclsSet),
    set.to_sorted_list(PredDeclsSet, PredDecls),
    opt_export_preds(PredDecls, !ModuleInfo),
    maybe_opt_export_types(!ModuleInfo),
    maybe_opt_export_classes(!ModuleInfo),
    maybe_opt_export_instances(!ModuleInfo).

%---------------------%

:- pred maybe_opt_export_types(module_info::in, module_info::out) is det.

maybe_opt_export_types(!ModuleInfo) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    map_foldl_over_type_ctor_defns(maybe_opt_export_type_defn,
        TypeTable0, TypeTable, !ModuleInfo),
    module_info_set_type_table(TypeTable, !ModuleInfo).

:- pred maybe_opt_export_type_defn(type_ctor::in,
    hlds_type_defn::in, hlds_type_defn::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_type_defn(TypeCtor, TypeDefn0, TypeDefn, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ( if should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn0) then
        hlds_data.set_type_defn_status(type_status(status_exported),
            TypeDefn0, TypeDefn),
        adjust_status_of_special_preds(TypeCtor, !ModuleInfo)
    else
        TypeDefn = TypeDefn0
    ).

:- pred adjust_status_of_special_preds((type_ctor)::in,
    module_info::in, module_info::out) is det.

adjust_status_of_special_preds(TypeCtor, ModuleInfo0, ModuleInfo) :-
    special_pred_list(SpecialPredList),
    module_info_get_special_pred_maps(ModuleInfo0, SpecPredMaps),
    list.filter_map(
        ( pred(SpecPredId::in, PredId::out) is semidet :-
            search_special_pred_maps(SpecPredMaps, SpecPredId, TypeCtor,
                PredId)
        ), SpecialPredList, PredIds),
    opt_export_preds(PredIds, ModuleInfo0, ModuleInfo).

%---------------------%

:- pred maybe_opt_export_classes(module_info::in, module_info::out) is det.

maybe_opt_export_classes(!ModuleInfo) :-
    module_info_get_class_table(!.ModuleInfo, Classes0),
    map.to_assoc_list(Classes0, ClassAL0),
    list.map_foldl(maybe_opt_export_class_defn, ClassAL0, ClassAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(ClassAL, Classes),
    module_info_set_class_table(Classes, !ModuleInfo).

:- pred maybe_opt_export_class_defn(pair(class_id, hlds_class_defn)::in,
    pair(class_id, hlds_class_defn)::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_class_defn(ClassId - ClassDefn0, ClassId - ClassDefn,
        !ModuleInfo) :-
    ToWrite = typeclass_status_to_write(ClassDefn0 ^ classdefn_status),
    (
        ToWrite = yes,
        ClassDefn = ClassDefn0 ^ classdefn_status :=
            typeclass_status(status_exported),
        method_infos_to_pred_ids(ClassDefn ^ classdefn_method_infos, PredIds),
        opt_export_preds(PredIds, !ModuleInfo)
    ;
        ToWrite = no,
        ClassDefn = ClassDefn0
    ).

:- pred method_infos_to_pred_ids(list(method_info)::in, list(pred_id)::out)
    is det.

method_infos_to_pred_ids(MethodInfos, PredIds) :-
    GetMethodPredId =
        ( pred(MI::in, PredId::out) is det :-
            MI ^ method_cur_proc = proc(PredId, _ProcId)
        ),
    list.map(GetMethodPredId, MethodInfos, PredIds0),
    list.remove_adjacent_dups(PredIds0, PredIds).

%---------------------%

:- pred maybe_opt_export_instances(module_info::in, module_info::out) is det.

maybe_opt_export_instances(!ModuleInfo) :-
    module_info_get_instance_table(!.ModuleInfo, Instances0),
    map.to_assoc_list(Instances0, InstanceAL0),
    list.map_foldl(maybe_opt_export_class_instances, InstanceAL0, InstanceAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(InstanceAL, Instances),
    module_info_set_instance_table(Instances, !ModuleInfo).

:- pred maybe_opt_export_class_instances(
    pair(class_id, list(hlds_instance_defn))::in,
    pair(class_id, list(hlds_instance_defn))::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_class_instances(ClassId - InstanceList0,
        ClassId - InstanceList, !ModuleInfo) :-
    list.map_foldl(maybe_opt_export_instance_defn, InstanceList0, InstanceList,
        !ModuleInfo).

:- pred maybe_opt_export_instance_defn(hlds_instance_defn::in,
    hlds_instance_defn::out, module_info::in, module_info::out) is det.

maybe_opt_export_instance_defn(Instance0, Instance, !ModuleInfo) :-
    Instance0 = hlds_instance_defn(InstanceModule, InstanceStatus0,
        TVarSet, OriginalTypes, Types,
        Constraints, MaybeSubsumedContext, ConstraintProofs,
        Body, MaybeMethodInfos, Context),
    ToWrite = instance_status_to_write(InstanceStatus0),
    (
        ToWrite = yes,
        InstanceStatus = instance_status(status_exported),
        Instance = hlds_instance_defn(InstanceModule, InstanceStatus,
            TVarSet, OriginalTypes, Types,
            Constraints, MaybeSubsumedContext, ConstraintProofs,
            Body, MaybeMethodInfos, Context),
        (
            MaybeMethodInfos = yes(MethodInfos),
            method_infos_to_pred_ids(MethodInfos, PredIds),
            opt_export_preds(PredIds, !ModuleInfo)
        ;
            % This can happen if an instance has multiple declarations,
            % one of which is abstract.
            MaybeMethodInfos = no
        )
    ;
        ToWrite = no,
        Instance = Instance0
    ).

%---------------------%

:- pred opt_export_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

opt_export_preds(PredIds, !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    opt_export_preds_in_pred_id_table(PredIds, PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred opt_export_preds_in_pred_id_table(list(pred_id)::in,
    pred_id_table::in, pred_id_table::out) is det.

opt_export_preds_in_pred_id_table([], !PredIdTable).
opt_export_preds_in_pred_id_table([PredId | PredIds], !PredIdTable) :-
    map.lookup(!.PredIdTable, PredId, PredInfo0),
    pred_info_get_status(PredInfo0, PredStatus0),
    ToWrite = pred_status_to_write(PredStatus0),
    (
        ToWrite = yes,
        ( if
            pred_info_get_origin(PredInfo0, Origin),
            Origin = origin_compiler(made_for_uci(spec_pred_unify, _))
        then
            PredStatus = pred_status(status_pseudo_exported)
        else if
            PredStatus0 = pred_status(status_external(_))
        then
            PredStatus = pred_status(status_external(status_opt_exported))
        else
            PredStatus = pred_status(status_opt_exported)
        ),
        pred_info_set_status(PredStatus, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredIdTable)
    ;
        ToWrite = no
    ),
    opt_export_preds_in_pred_id_table(PredIds, !PredIdTable).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod_mark_exported.
%---------------------------------------------------------------------------%
