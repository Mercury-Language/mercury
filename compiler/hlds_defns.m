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
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.

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

    % We print the output in this order to ensure that the resulting file
    % is sorted.
    output_prefixed_name_arities(Stream, "func ",
        set.to_sorted_list(FuncNameArities), !IO),
    output_prefixed_name_arities(Stream, "inst ",
        set.to_sorted_list(InstNameArities), !IO),
    output_prefixed_name_arities(Stream, "mode ",
        set.to_sorted_list(ModeNameArities), !IO),
    output_prefixed_name_arities(Stream, "pred ",
        set.to_sorted_list(PredNameArities), !IO),
    output_prefixed_name_arities(Stream, "type ",
        set.to_sorted_list(TypeNameArities), !IO).

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
        unexpected($module, $pred, "unqualified insts_id name")
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
        unexpected($module, $pred, "unqualified modes_id name")
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

:- pred output_prefixed_name_arities(io.text_output_stream::in,
    string::in, list(name_arity)::in, io::di, io::uo) is det.

output_prefixed_name_arities(_Stream, _Prefix, [], !IO).
output_prefixed_name_arities(Stream, Prefix, [StrArity | StrArities], !IO) :-
    StrArity = name_arity(Str, Arity),
    io.write_string(Stream, Prefix, !IO),
    io.write_string(Stream, Str, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, Arity, !IO),
    io.nl(Stream, !IO),
    output_prefixed_name_arities(Stream, Prefix, StrArities, !IO).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_defns.
%-----------------------------------------------------------------------------%
