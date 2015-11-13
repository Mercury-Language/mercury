%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module provides the capability to print out the list of the entities
% defined in the given module.
%
% When splitting up a module, it is sometimes hard to ensure that every part
% of the original module ends up in exactly one of the successor modules.
% Once programmers have access to a list of the main kinds of entities
% (types, insts, modes, functions and predicates) defined in each module,
% they can use standard tools to check this. For example, a command such as
% "comm -12 m1.defns m2.defns" will list the entities that are defined in
% both m1 and m2.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_defns.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

:- pred write_hlds_defns(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- type name_arity
    --->    name_arity(string, arity).

%-----------------------------------------------------------------------------%

write_hlds_defns(Stream, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),

    module_info_get_type_table(ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorDefns),
    gather_local_type_names(ModuleName, TypeCtorDefns,
        set.init, TypeNameArities),

    module_info_get_inst_table(ModuleInfo, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    map.keys(UserInstTable, UserInstIds),
    gather_local_inst_names(ModuleName, UserInstIds,
        set.init, InstNameArities),

    module_info_get_mode_table(ModuleInfo, ModeTable),
    mode_table_get_mode_defns(ModeTable, ModeDefnMap),
    map.keys(ModeDefnMap, ModeIds),
    gather_local_mode_names(ModuleName, ModeIds,
        set.init, ModeNameArities),

    module_info_get_preds(ModuleInfo, Preds),
    map.to_sorted_assoc_list(Preds, PredDefns),
    gather_local_pred_names(ModuleName, PredDefns,
        set.init, FuncNameArities, set.init, PredNameArities),

    module_info_get_class_table(ModuleInfo, ClassTable),
    map.keys(ClassTable, ClassIds),
    gather_local_typeclass_names(ModuleName, ClassIds,
        set.init, ClassNameArities),

    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.to_sorted_assoc_list(InstanceTable, InstanceDefns),
    gather_local_instance_names(ModuleName, InstanceDefns,
        set.init, InstanceDescs),

    % We print the output in this order to ensure that the resulting file
    % is sorted.
    output_prefixed_name_arities(Stream, "func ",
        set.to_sorted_list(FuncNameArities), !IO),
    output_prefixed_name_arities(Stream, "inst ",
        set.to_sorted_list(InstNameArities), !IO),
    output_prefixed_strings(Stream, "instance ",
        set.to_sorted_list(InstanceDescs), !IO),
    output_prefixed_name_arities(Stream, "mode ",
        set.to_sorted_list(ModeNameArities), !IO),
    output_prefixed_name_arities(Stream, "pred ",
        set.to_sorted_list(PredNameArities), !IO),
    output_prefixed_name_arities(Stream, "type ",
        set.to_sorted_list(TypeNameArities), !IO),
    output_prefixed_name_arities(Stream, "typeclass ",
        set.to_sorted_list(ClassNameArities), !IO).

%-----------------------------------------------------------------------------%

:- pred gather_local_type_names(module_name::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_type_names(_, [], !TypeNameArities).
gather_local_type_names(ModuleName, [TypeCtor - _Defn | TypeCtorDefns],
        !TypeNameArities) :-
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    (
        TypeCtorSymName = unqualified(_),
        unexpected($module, $pred, "unqualified type_ctor name")
    ;
        TypeCtorSymName = qualified(TypeCtorModuleName, TypeCtorName),
        ( if TypeCtorModuleName = ModuleName then
            set.insert(name_arity(TypeCtorName, TypeCtorArity),
                !TypeNameArities)
        else
            true
        )
    ),
    gather_local_type_names(ModuleName, TypeCtorDefns, !TypeNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_inst_names(module_name::in, list(inst_id)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_inst_names(_, [], !InstNameArities).
gather_local_inst_names(ModuleName, [InstId | InstIds], !InstNameArities) :-
    InstId = inst_id(InstSymName, InstArity),
    (
        InstSymName = unqualified(_),
        unexpected($module, $pred, "unqualified inst_id name")
    ;
        InstSymName = qualified(InstModuleName, InstName),
        ( if InstModuleName = ModuleName then
            set.insert(name_arity(InstName, InstArity), !InstNameArities)
        else
            true
        )
    ),
    gather_local_inst_names(ModuleName, InstIds, !InstNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_mode_names(module_name::in, list(mode_id)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_mode_names(_, [], !ModeNameArities).
gather_local_mode_names(ModuleName, [ModeId | ModeIds],
        !ModeNameArities) :-
    ModeId = mode_id(ModeSymName, ModeArity),
    (
        ModeSymName = unqualified(_),
        unexpected($module, $pred, "unqualified mode_id name")
    ;
        ModeSymName = qualified(ModeModuleName, ModeName),
        ( if ModeModuleName = ModuleName then
            set.insert(name_arity(ModeName, ModeArity), !ModeNameArities)
        else
            true
        )
    ),
    gather_local_mode_names(ModuleName, ModeIds, !ModeNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_pred_names(module_name::in,
    assoc_list(pred_id, pred_info)::in,
    set(name_arity)::in, set(name_arity)::out,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_pred_names(_ModuleName, [], !FuncNameArities, !PredNameArities).
gather_local_pred_names(ModuleName, [PredDefn | PredDefns],
        !FuncNameArities, !PredNameArities) :-
    PredDefn = _PredId - PredInfo,
    pred_info_get_module_name(PredInfo, PredModuleName),
    pred_info_get_origin(PredInfo, Origin),
    ( if
        PredModuleName = ModuleName,
        Origin = origin_user(_)
    then
        pred_info_get_name(PredInfo, PredName),
        PredArity = pred_info_orig_arity(PredInfo),
        NameArity = name_arity(PredName, PredArity),
        PorF = pred_info_is_pred_or_func(PredInfo),
        (
            PorF = pf_function,
            set.insert(NameArity, !FuncNameArities)
        ;
            PorF = pf_predicate,
            set.insert(NameArity, !PredNameArities)
        )
    else
        true
    ),
    gather_local_pred_names(ModuleName, PredDefns,
        !FuncNameArities, !PredNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_typeclass_names(module_name::in, list(class_id)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_typeclass_names(_, [], !ClassNameArities).
gather_local_typeclass_names(ModuleName, [ClassId | ClassIds],
        !ClassNameArities) :-
    ClassId = class_id(ClassSymName, ClassArity),
    (
        ClassSymName = unqualified(_),
        unexpected($module, $pred, "unqualified class_id name")
    ;
        ClassSymName = qualified(ClassModuleName, ClassName),
        ( if ClassModuleName = ModuleName then
            set.insert(name_arity(ClassName, ClassArity), !ClassNameArities)
        else
            true
        )
    ),
    gather_local_typeclass_names(ModuleName, ClassIds, !ClassNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_instance_names(module_name::in,
    assoc_list(class_id, list(hlds_instance_defn))::in,
    set(string)::in, set(string)::out) is det.

gather_local_instance_names(_, [], !InstanceDescs).
gather_local_instance_names(ModuleName, [InstancePair | InstancePairs],
        !InstanceDescs) :-
    InstancePair = ClassId - InstanceDefns,
    gather_local_instance_names_for_class(ModuleName, ClassId, InstanceDefns,
        !InstanceDescs),
    gather_local_instance_names(ModuleName, InstancePairs, !InstanceDescs).

:- pred gather_local_instance_names_for_class(module_name::in, class_id::in,
    list(hlds_instance_defn)::in, set(string)::in, set(string)::out) is det.

gather_local_instance_names_for_class(_, _, [], !InstanceDescs).
gather_local_instance_names_for_class(ModuleName, ClassId,
        [InstanceDefn | InstanceDefns], !InstanceDescs) :-
    InstanceDefn = hlds_instance_defn(InstanceModuleName, _Types, OrigTypes,
        _Status, _Context, _Constraints, _Body, _Interface, _TVarSet, _Proofs),
    ( if InstanceModuleName = ModuleName then
        ClassId = class_id(ClassSymName, ClassArity),
        ClassName = unqualify_name(ClassSymName),

        list.map(instance_type_to_desc, OrigTypes, OrigTypeDescs),
        TypeVectorDesc = string.join_list(", ", OrigTypeDescs),

        string.format("%s/%d <%s>",
            [s(ClassName), i(ClassArity), s(TypeVectorDesc)], InstanceDesc),
        set.insert(InstanceDesc, !InstanceDescs)
    else
        true
    ),
    gather_local_instance_names_for_class(ModuleName, ClassId,
        InstanceDefns, !InstanceDescs).

:- pred instance_type_to_desc(mer_type::in, string::out) is det.

instance_type_to_desc(Type, TypeDesc) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    TypeName = unqualify_name(TypeSymName),
    TypeDesc = TypeName ++ "/" ++ string.int_to_string(TypeArity).

%-----------------------------------------------------------------------------%

:- pred output_prefixed_name_arities(io.text_output_stream::in,
    string::in, list(name_arity)::in, io::di, io::uo) is det.

output_prefixed_name_arities(_Stream, _Prefix, [], !IO).
output_prefixed_name_arities(Stream, Prefix, [NameArity | NameArities], !IO) :-
    NameArity = name_arity(Name, Arity),
    io.write_string(Stream, Prefix, !IO),
    io.write_string(Stream, Name, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, Arity, !IO),
    io.nl(Stream, !IO),
    output_prefixed_name_arities(Stream, Prefix, NameArities, !IO).

:- pred output_prefixed_strings(io.text_output_stream::in,
    string::in, list(string)::in, io::di, io::uo) is det.

output_prefixed_strings(_Stream, _Prefix, [], !IO).
output_prefixed_strings(Stream, Prefix, [Str | Strs], !IO) :-
    io.write_string(Stream, Prefix, !IO),
    io.write_string(Stream, Str, !IO),
    io.nl(Stream, !IO),
    output_prefixed_strings(Stream, Prefix, Strs, !IO).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_defns.
%-----------------------------------------------------------------------------%
