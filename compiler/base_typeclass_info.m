%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2000, 2003-2006 The University of Melbourne.
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
:- import_module hlds.hlds_module.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_base_typeclass_info_rtti(module_info::in,
    list(rtti_data)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.type_class_info.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

generate_base_typeclass_info_rtti(ModuleInfo, RttiDataList) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.to_assoc_list(InstanceTable, AllInstances),
    gen_infos_for_classes(AllInstances, ModuleName, ModuleInfo,
        [], RttiDataList).

:- pred gen_infos_for_classes(
    assoc_list(class_id, list(hlds_instance_defn))::in, module_name::in,
    module_info::in, list(rtti_data)::in, list(rtti_data)::out) is det.

gen_infos_for_classes([], _ModuleName, _ModuleInfo, !RttiDataList).
gen_infos_for_classes([C|Cs], ModuleName, ModuleInfo, !RttiDataList) :-
    gen_infos_for_instance_list(C, ModuleName, ModuleInfo, !RttiDataList),
    gen_infos_for_classes(Cs, ModuleName, ModuleInfo, !RttiDataList).

    % XXX make it use an accumulator
:- pred gen_infos_for_instance_list(
    pair(class_id, list(hlds_instance_defn))::in, module_name::in,
    module_info::in, list(rtti_data)::in, list(rtti_data)::out) is det.

gen_infos_for_instance_list(_ - [], _, _, !RttiDataList).
gen_infos_for_instance_list(ClassId - [InstanceDefn | Is], ModuleName,
        ModuleInfo, !RttiDataList) :-
    gen_infos_for_instance_list(ClassId - Is, ModuleName, ModuleInfo,
        !RttiDataList),
    InstanceDefn = hlds_instance_defn(InstanceModule, ImportStatus,
        _TermContext, InstanceConstraints, InstanceTypes, Body,
        PredProcIds, _Varset, _SuperClassProofs),
    (
        Body = concrete(_),
        % Only make the base_typeclass_info if the instance
        % declaration originally came from _this_ module.
        status_defined_in_this_module(ImportStatus) = yes
    ->
        make_instance_string(InstanceTypes, InstanceString),
        gen_body(PredProcIds, InstanceTypes, InstanceConstraints,
            ModuleInfo, ClassId, BaseTypeClassInfo),
        TCName = generate_class_name(ClassId),
        RttiData = base_typeclass_info(TCName, InstanceModule,
            InstanceString, BaseTypeClassInfo),
        !:RttiDataList = [RttiData | !.RttiDataList]
    ;
        % The instance decl is from another module,
        % or is abstract, so we don't bother including it.
        true
    ).

%----------------------------------------------------------------------------%

:- pred gen_body(maybe(list(hlds_class_proc))::in, list(mer_type)::in,
    list(prog_constraint)::in, module_info::in, class_id::in,
    base_typeclass_info::out) is det.

gen_body(no, _, _, _, _, _) :-
    unexpected(this_file, "pred_proc_ids should have " ++
        "been filled in by check_typeclass.m").
gen_body(yes(PredProcIds0), Types, Constraints, ModuleInfo, ClassId,
        BaseTypeClassInfo) :-
    prog_type.vars_list(Types, TypeVars),
    get_unconstrained_tvars(TypeVars, Constraints, Unconstrained),
    list.length(Constraints, NumConstraints),
    list.length(Unconstrained, NumUnconstrained),
    NumExtra = NumConstraints + NumUnconstrained,
    ExtractPredProcId = (pred(HldsPredProc::in, PredProc::out) is det :-
        (
            HldsPredProc = hlds_class_proc(PredId, ProcId),
            PredProc = proc(PredId, ProcId)
        )),
    list.map(ExtractPredProcId, PredProcIds0, PredProcIds),
    construct_proc_labels(PredProcIds, ModuleInfo, ProcLabels),
    gen_superclass_count(ClassId, ModuleInfo, SuperClassCount, ClassArity),
    list.length(ProcLabels, NumMethods),
    BaseTypeClassInfo = base_typeclass_info(NumExtra, NumConstraints,
        SuperClassCount, ClassArity, NumMethods, ProcLabels).

:- pred construct_proc_labels(list(pred_proc_id)::in,
    module_info::in, list(rtti_proc_label)::out) is det.

construct_proc_labels([], _, []).
construct_proc_labels([proc(PredId, ProcId) | Procs], ModuleInfo,
        [ProcLabel | ProcLabels]) :-
    ProcLabel = rtti.make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    construct_proc_labels(Procs, ModuleInfo, ProcLabels).

%----------------------------------------------------------------------------%

:- pred gen_superclass_count(class_id::in, module_info::in,
    int::out, int::out) is det.

gen_superclass_count(ClassId, ModuleInfo, NumSuperClasses, ClassArity) :-
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, ClassId, ClassDefn),
    list.length(ClassDefn ^ class_supers, NumSuperClasses),
    list.length(ClassDefn ^ class_vars, ClassArity).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "base_typeclass_info.m".

%----------------------------------------------------------------------------%
