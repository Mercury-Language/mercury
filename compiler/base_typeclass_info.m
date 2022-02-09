%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2000, 2003-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: base_typeclass_info.m.
% Author: dgj.
%
% This module generates the RTTI data for the global variables (or constants)
% that hold the base_typeclass_info structures of the typeclass instances
% defined by the current module.
%
% See notes/type_class_transformation.html for a description of the various
% ways to represent type information, including a description of the
% base_typeclass_info structures.
%
% XXX The function of this file will soon be taken over by type_class_info.m.
%
%---------------------------------------------------------------------------%

:- module backend_libs.base_typeclass_info.
:- interface.

:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module list.

%---------------------------------------------------------------------------%

    % Generate all the base_typeclass_infos defined by the module.
    %
:- pred generate_base_typeclass_info_rtti(module_info::in,
    list(rtti_data)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.type_class_info.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

generate_base_typeclass_info_rtti(ModuleInfo, RttiDatas) :-
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.to_assoc_list(InstanceTable, AllInstances),
    gen_infos_for_classes(ModuleInfo, AllInstances, [], RttiDatas).

:- pred gen_infos_for_classes(module_info::in,
    assoc_list(class_id, list(hlds_instance_defn))::in,
    list(rtti_data)::in, list(rtti_data)::out) is det.

gen_infos_for_classes(_ModuleInfo, [], !RttiDatas).
gen_infos_for_classes(ModuleInfo, [Class | Classes], !RttiDatas) :-
    Class = ClassId - InstanceDefns,
    gen_infos_for_instances(ModuleInfo, ClassId, InstanceDefns, !RttiDatas),
    gen_infos_for_classes(ModuleInfo, Classes, !RttiDatas).

:- pred gen_infos_for_instances(module_info::in, class_id::in,
    list(hlds_instance_defn)::in,
    list(rtti_data)::in, list(rtti_data)::out) is det.

gen_infos_for_instances(_, _, [], !RttiDatas).
gen_infos_for_instances(ModuleInfo, ClassId,
        [InstanceDefn | InstanceDefns], !RttiDatas) :-
    % We could make this procedure tail recursive just by doing this call
    % at the end of the clause, but keeping the declarations in instance
    % order seems worthwhile on aesthetic grounds.
    gen_infos_for_instances(ModuleInfo, ClassId, InstanceDefns, !RttiDatas),

    InstanceDefn = hlds_instance_defn(InstanceModule,
        InstanceTypes, _OriginalTypes, ImportStatus, _TermContext,
        _InstanceConstraints, Body, _MaybePredProcIds,
        _Varset, _SuperClassProofs),
    ( if
        Body = instance_body_concrete(_),
        % Only make the base_typeclass_info if the instance declaration
        % originally came from _this_ module.
        instance_status_defined_in_this_module(ImportStatus) = yes
    then
        make_instance_string(InstanceTypes, InstanceString),
        gen_body(ModuleInfo, ClassId, InstanceDefn, BaseTypeClassInfo),
        TCName = generate_class_name(ClassId),
        RttiData = rtti_data_base_typeclass_info(TCName, InstanceModule,
            InstanceString, BaseTypeClassInfo),
        !:RttiDatas = [RttiData | !.RttiDatas]
    else
        % The instance decl is from another module, or is abstract,
        % so we don't bother including it.
        true
    ).

%----------------------------------------------------------------------------%

:- pred gen_body(module_info::in, class_id::in, hlds_instance_defn::in,
    base_typeclass_info::out) is det.

gen_body(ModuleInfo, ClassId, InstanceDefn, BaseTypeClassInfo) :-
    num_extra_instance_args(InstanceDefn, NumExtra),

    Constraints = InstanceDefn ^ instdefn_constraints,
    list.length(Constraints, NumConstraints),

    MaybeInstancePredProcIds = InstanceDefn ^ instdefn_maybe_method_ppids,
    (
        MaybeInstancePredProcIds = no,
        unexpected($pred,
            "pred_proc_ids not filled in by check_typeclass.m")
    ;
        MaybeInstancePredProcIds = yes(InstancePredProcIds)
    ),
    construct_proc_labels(ModuleInfo, InstancePredProcIds, ProcLabels),
    gen_superclass_count(ClassId, ModuleInfo, SuperClassCount, ClassArity),
    list.length(ProcLabels, NumMethods),
    BaseTypeClassInfo = base_typeclass_info(NumExtra, NumConstraints,
        SuperClassCount, ClassArity, NumMethods, ProcLabels).

:- pred construct_proc_labels(module_info::in, list(pred_proc_id)::in,
    list(rtti_proc_label)::out) is det.

construct_proc_labels(_, [], []).
construct_proc_labels(ModuleInfo, [proc(PredId, ProcId) | PredProcIds],
        [ProcLabel | ProcLabels]) :-
    ProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    construct_proc_labels(ModuleInfo, PredProcIds, ProcLabels).

%----------------------------------------------------------------------------%

:- pred gen_superclass_count(class_id::in, module_info::in,
    int::out, int::out) is det.

gen_superclass_count(ClassId, ModuleInfo, NumSuperClasses, ClassArity) :-
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, ClassId, ClassDefn),
    list.length(ClassDefn ^ classdefn_supers, NumSuperClasses),
    list.length(ClassDefn ^ classdefn_vars, ClassArity).

%----------------------------------------------------------------------------%
:- end_module backend_libs.base_typeclass_info.
%----------------------------------------------------------------------------%
