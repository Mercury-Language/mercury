%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rtti_out.m.
% Main author: zs.
%
% This module contains code to output the RTTI data structures defined in
% rtti.m as C code.
%
% This module is part of the LLDS back-end. The decl_set data type that it
% uses, which is defined in llds_out.m, represents a set of LLDS declarations,
% and thus depends on the LLDS. Also the code to output code_addrs depends on
% the LLDS.
%
% The MLDS back-end does not use this module; instead it converts the RTTI
% data structures to MLDS (and then to C or Java, etc.).
%
%-----------------------------------------------------------------------------%

:- module ll_backend.rtti_out.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_util.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Output a C declaration for the rtti_datas.
    %
:- pred output_rtti_data_decl_list(llds_out_info::in, list(rtti_data)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output a C declaration for the rtti_data.
    %
:- pred output_rtti_data_decl(llds_out_info::in, rtti_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output a C definition for the rtti_data.
    %
:- pred output_rtti_data_defn(llds_out_info::in, rtti_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output C code (e.g. a call to the MR_INIT_TYPE_CTOR_INFO() macro)
    % to initialize the rtti_data if necessary.
    %
:- pred init_rtti_data_if_nec(rtti_data::in,
    io::di, io::uo) is det.

    % Output C code (e.g. a call to MR_register_type_ctor_info()) to register
    % the rtti_data in the type tables, if it represents a data structure
    % that should be so registered.
    %
:- pred register_rtti_data_if_nec(rtti_data::in, io::di, io::uo)
    is det.

    % Output a C expression holding the address of the C name of the specified
    % rtti_data, preceded by the string in the first argument (that string will
    % usually be a C cast).
    %
:- pred output_cast_addr_of_rtti_data(string::in, rtti_data::in,
    io::di, io::uo) is det.

    % Output a C expression holding the address of the C name of
    % the specified rtti_data.
    %
:- pred output_addr_of_rtti_data(rtti_data::in, io::di, io::uo) is det.

    % Output the C name of the rtti_data specified by the given rtti_id.
    %
:- pred output_rtti_id(rtti_id::in, io::di, io::uo) is det.

    % Output the C storage class, C type, and C name of the rtti_data
    % specified by the given rtti_id for use in a declaration or
    % definition. The bool should be `yes' iff it is for a definition.
    %
:- pred output_rtti_id_storage_type_name(llds_out_info::in,
    rtti_id::in, bool::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output the C storage class, C type, and C name of the rtti_data
    % specified by the given rtti_id for use in a declaration or
    % definition. The bool should be `yes' iff it is for a definition.
    %
:- pred output_rtti_id_storage_type_name_no_decl(llds_out_info::in,
    rtti_id::in, bool::in, io::di, io::uo) is det.

:- func tabling_struct_data_addr_string(proc_label, proc_tabling_struct_id)
    = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.type_ctor_info.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
:- import_module ll_backend.code_util.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module ll_backend.llds_out.llds_out_file.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.

:- import_module assoc_list.
:- import_module counter.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int8.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint8.
:- import_module univ.

%-----------------------------------------------------------------------------%

output_rtti_data_decl_list(Info, RttiDatas, !DeclSet, !IO) :-
    classify_rtti_datas_to_decl(RttiDatas, multi_map.init, GroupMap),
    multi_map.to_assoc_list(GroupMap, GroupList),
    list.foldl2(output_rtti_data_decl_group(Info), GroupList, !DeclSet, !IO).

:- type data_group
    --->    data_group(
                data_c_type     :: string,
                data_is_array   :: is_array,
                data_linkage    :: linkage
            ).

:- pred classify_rtti_datas_to_decl(list(rtti_data)::in,
    multi_map(data_group, rtti_id)::in,
    multi_map(data_group, rtti_id)::out) is det.

classify_rtti_datas_to_decl([], !GroupMap).
classify_rtti_datas_to_decl([RttiData | RttiDatas], !GroupMap) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(_)) then
        % These just get represented as integers, so we don't need to declare
        % them. Also rtti_data_to_id/3 does not handle this case.
        true
    else
        rtti_data_to_id(RttiData, RttiId),
        rtti_id_c_type(RttiId, CType, IsArray),
        rtti_id_linkage(RttiId, Linkage),
        Group = data_group(CType, IsArray, Linkage),
        multi_map.set(Group, RttiId, !GroupMap)
    ),
    classify_rtti_datas_to_decl(RttiDatas, !GroupMap).

:- pred output_rtti_data_decl_group(llds_out_info::in,
    pair(data_group, list(rtti_id))::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_rtti_data_decl_group(Info, Group - RttiIds, !DeclSet, !IO) :-
    % ChunkSize should be as large as possible to reduce the size of the
    % file being generated, but small enough not to overload the fixed
    % limits of our target C compilers.
    ChunkSize = 10,
    % The process of creating the multi_map reverses the order of rtti_ids,
    % we now undo this reversal.
    list.chunk(list.reverse(RttiIds), ChunkSize, RttiIdChunks),
    list.foldl2(output_rtti_data_decl_chunk(Info, Group), RttiIdChunks,
        !DeclSet, !IO).

:- pred output_rtti_data_decl_chunk(llds_out_info::in, data_group::in,
    list(rtti_id)::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rtti_data_decl_chunk(Info, Group, RttiIds, !DeclSet, !IO) :-
    (
        % Pick a representative RttiId. All the operations we perform on it
        % below would have the same result regardless of which one we picked.
        RttiIds = [RttiId | _]
    ;
        RttiIds = [],
        unexpected($pred, "empty list")
    ),
    Group = data_group(CType, IsArray, Linkage),

    io.nl(!IO),
    output_rtti_type_decl(RttiId, !DeclSet, !IO),
    Globals = Info ^ lout_globals,
    LinkageStr = c_data_linkage_string(Linkage, no),
    InclCodeAddr = rtti_id_would_include_code_addr(RttiId),

    io.write_string(LinkageStr, !IO),
    io.write_string(c_data_const_string(Globals, InclCodeAddr), !IO),
    c_util.output_quoted_string_cur_stream(CType, !IO),
    io.nl(!IO),

    output_rtti_data_decl_chunk_entries(IsArray, RttiIds, !DeclSet, !IO).

:- pred output_rtti_data_decl_chunk_entries(is_array::in, list(rtti_id)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rtti_data_decl_chunk_entries(_IsArray, [], !DeclSet, !IO) :-
    unexpected($pred, "empty list").
output_rtti_data_decl_chunk_entries(IsArray, [RttiId | RttiIds],
        !DeclSet, !IO) :-
    decl_set_insert(decl_rtti_id(RttiId), !DeclSet),
    io.write_string("\t", !IO),
    output_rtti_id(RttiId, !IO),
    (
        IsArray = is_array,
        io.write_string("[]", !IO)
    ;
        IsArray = not_array
    ),
    (
        RttiIds = [_ | _],
        io.write_string(",\n", !IO),
        output_rtti_data_decl_chunk_entries(IsArray, RttiIds, !DeclSet, !IO)
    ;
        RttiIds = [],
        io.write_string(";\n", !IO)
    ).

%-----------------------------------------------------------------------------%

output_rtti_data_decl(Info, RttiData, !DeclSet, !IO) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(_)) then
        % These just get represented as integers, so we don't need to declare
        % them. Also rtti_data_to_id/3 does not handle this case.
        true
    else
        rtti_data_to_id(RttiData, RttiId),
        output_generic_rtti_data_decl(Info, RttiId, !DeclSet, !IO)
    ).

%-----------------------------------------------------------------------------%

output_rtti_data_defn(Info, RttiDefn, !DeclSet, !IO) :-
    (
        RttiDefn = rtti_data_type_info(TypeInfo),
        output_type_info_defn(Info, TypeInfo, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_pseudo_type_info(PseudoTypeInfo),
        output_pseudo_type_info_defn(Info, PseudoTypeInfo, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_type_ctor_info(TypeCtorData),
        output_type_ctor_data_defn(Info, TypeCtorData, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_base_typeclass_info(TCName, InstanceModuleName,
            InstanceString, BaseTypeClassInfo),
        output_base_typeclass_info_defn(Info, TCName, InstanceModuleName,
            InstanceString, BaseTypeClassInfo, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_type_class_decl(TCDecl),
        output_type_class_decl_defn(Info, TCDecl, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_type_class_instance(InstanceDecl),
        output_type_class_instance_defn(Info, InstanceDecl, !DeclSet, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred output_base_typeclass_info_defn(llds_out_info::in, tc_name::in,
    module_name::in, string::in, base_typeclass_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_base_typeclass_info_defn(Info, TCName, InstanceModuleName,
        InstanceString, BaseTypeClassInfo, !DeclSet, !IO) :-
    BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5, Methods),
    CodeAddrs = list.map(make_code_addr, Methods),
    list.foldl2(output_record_code_addr_decls(Info), CodeAddrs, !DeclSet, !IO),
    io.write_string("\n", !IO),
    RttiId = tc_rtti_id(TCName,
        type_class_base_typeclass_info(InstanceModuleName, InstanceString)),
    output_rtti_id_storage_type_name(Info, RttiId, yes, !DeclSet, !IO),
    % XXX It would be nice to avoid generating redundant declarations
    % of base_typeclass_infos, but currently we don't.
    io.write_string(" = {\n\t(MR_Code *) ", !IO),
    io.write_list([N1, N2, N3, N4, N5], ",\n\t(MR_Code *) ", io.write_int,
        !IO),
    io.write_string(",\n\t", !IO),
    io.write_list(CodeAddrs, ",\n\t", output_static_code_addr, !IO),
    io.write_string("\n};\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_type_class_decl_defn(llds_out_info::in, tc_decl::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_class_decl_defn(Info, TCDecl, !DeclSet, !IO) :-
    TCDecl = tc_decl(TCId, Version, Supers),
    TCId = tc_id(TCName, TVarNames, MethodIds),
    TCName = tc_name(ModuleSymName, ClassName, Arity),

    TCIdVarNamesRttiName = type_class_id_var_names,
    TCIdVarNamesRttiId = tc_rtti_id(TCName, TCIdVarNamesRttiName),
    TCIdMethodIdsRttiName = type_class_id_method_ids,
    TCIdMethodIdsRttiId = tc_rtti_id(TCName, TCIdMethodIdsRttiName),
    TCIdRttiName = type_class_id,
    TCIdRttiId = tc_rtti_id(TCName, TCIdRttiName),
    TCDeclSupersRttiName = type_class_decl_supers,
    TCDeclSupersRttiId = tc_rtti_id(TCName, TCDeclSupersRttiName),
    TCDeclRttiName = type_class_decl,
    TCDeclRttiId = tc_rtti_id(TCName, TCDeclRttiName),
    (
        TVarNames = []
    ;
        TVarNames = [_ | _],
        output_generic_rtti_data_defn_start(Info, TCIdVarNamesRttiId,
            !DeclSet, !IO),
        io.write_string(" = {\n", !IO),
        list.foldl(output_type_class_id_tvar_name, TVarNames, !IO),
        io.write_string("};\n", !IO)
    ),
    (
        MethodIds = []
    ;
        MethodIds = [_ | _],
        output_generic_rtti_data_defn_start(Info, TCIdMethodIdsRttiId,
            !DeclSet, !IO),
        io.write_string(" = {\n", !IO),
        list.foldl(output_type_class_id_method_id, MethodIds, !IO),
        io.write_string("};\n", !IO)
    ),
    list.length(TVarNames, NumTVarNames),
    list.length(MethodIds, NumMethodIds),
    output_generic_rtti_data_defn_start(Info, TCIdRttiId, !DeclSet, !IO),
    io.write_string(" = {\n\t""", !IO),
    c_util.output_quoted_string_cur_stream(sym_name_to_string(ModuleSymName),
        !IO),
    io.write_string(""",\n\t""", !IO),
    c_util.output_quoted_string_cur_stream(ClassName, !IO),
    io.write_string(""",\n\t", !IO),
    io.write_int(Arity, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(NumTVarNames, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(NumMethodIds, !IO),
    io.write_string(",\n\t", !IO),
    (
        TVarNames = [],
        io.write_string("NULL", !IO)
    ;
        TVarNames = [_ | _],
        output_rtti_id(TCIdVarNamesRttiId, !IO)
    ),
    io.write_string(",\n\t", !IO),
    (
        MethodIds = [],
        io.write_string("NULL", !IO)
    ;
        MethodIds = [_ | _],
        output_rtti_id(TCIdMethodIdsRttiId, !IO)
    ),
    io.write_string("\n};\n", !IO),
    (
        Supers = []
    ;
        Supers = [_ | _],
        list.map_foldl3(output_type_class_constraint(Info,
            make_tc_decl_super_id(TCName)), Supers, SuperIds,
            counter.init(1), _, !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, TCDeclSupersRttiId,
            !DeclSet, !IO),
        io.write_string(" = {\n", !IO),
        output_cast_addr_of_rtti_ids("(MR_TypeClassConstraint) ",
            SuperIds, !IO),
        io.write_string("};\n", !IO)
    ),
    list.length(Supers, NumSupers),
    output_generic_rtti_data_defn_start(Info, TCDeclRttiId, !DeclSet, !IO),
    io.write_string(" = {\n\t&", !IO),
    output_rtti_id(TCIdRttiId, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(Version, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(NumSupers, !IO),
    io.write_string(",\n\t", !IO),
    (
        Supers = [],
        io.write_string("NULL", !IO)
    ;
        Supers = [_ | _],
        output_rtti_id(TCDeclSupersRttiId, !IO)
    ),
    io.write_string("\n};\n", !IO).

:- pred output_type_class_id_tvar_name(string::in, io::di, io::uo) is det.

output_type_class_id_tvar_name(TVarName, !IO) :-
    io.write_string("\t""", !IO),
    c_util.output_quoted_string_cur_stream(TVarName, !IO),
    io.write_string(""",\n", !IO).

:- pred output_type_class_id_method_id(tc_method_id::in,
    io::di, io::uo) is det.

output_type_class_id_method_id(MethodId, !IO) :-
    MethodId = tc_method_id(MethodName, MethodArity, PredOrFunc),
    io.write_string("\t{ """, !IO),
    c_util.output_quoted_string_cur_stream(MethodName, !IO),
    io.write_string(""", ", !IO),
    io.write_int(MethodArity, !IO),
    io.write_string(", ", !IO),
    output_pred_or_func(PredOrFunc, !IO),
    io.write_string(" },\n", !IO).

:- pred make_tc_decl_super_id(tc_name::in, int::in, int::in, rtti_id::out)
    is det.

make_tc_decl_super_id(TCName, Ordinal, NumTypes, RttiId) :-
    RttiId = tc_rtti_id(TCName, type_class_decl_super(Ordinal, NumTypes)).

%-----------------------------------------------------------------------------%

:- pred output_type_class_instance_defn(llds_out_info::in, tc_instance::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_class_instance_defn(Info, Instance, !DeclSet, !IO) :-
    Instance = tc_instance(TCName, TCTypes, NumTypeVars, Constraints,
        _MethodProcLabels),
    list.foldl2(output_maybe_pseudo_type_info_defn(Info), TCTypes,
        !DeclSet, !IO),
    TCTypeRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, TCTypes),
    TCInstanceTypesRttiId = tc_rtti_id(TCName,
        type_class_instance_tc_type_vector(TCTypes)),
    output_generic_rtti_data_defn_start(Info, TCInstanceTypesRttiId,
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", TCTypeRttiDatas,
        !IO),
    io.write_string("};\n", !IO),
    TCInstanceConstraintsRttiId = tc_rtti_id(TCName,
        type_class_instance_constraints(TCTypes)),
    (
        Constraints = []
    ;
        Constraints = [_ | _],
        list.map_foldl3(output_type_class_constraint(Info,
            make_tc_instance_constraint_id(TCName, TCTypes)),
            Constraints, ConstraintIds, counter.init(1), _, !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, TCInstanceConstraintsRttiId,
            !DeclSet, !IO),
        io.write_string(" = {\n", !IO),
        output_cast_addr_of_rtti_ids("(MR_TypeClassConstraint) ",
            ConstraintIds, !IO),
        io.write_string("};\n", !IO)
    ),
%   TCInstanceMethodsRttiId = tc_rtti_id(
%       type_class_instance_methods(TCName, TCTypes)),
%   (
%       MethodProcLabels = []
%   ;
%       MethodProcLabels = [_ | _],
%       MethodCodeAddrs = list.map(make_code_addr, MethodProcLabels),
%       list.foldl2(output_code_addr_decls, MethodCodeAddrs,
%           !DeclSet, !IO),
%       output_generic_rtti_data_defn_start(TCInstanceMethodsRttiId,
%           !DeclSet, !IO),
%       io.write_string(" = {\n", !IO),
%       list.foldl(output_code_addr_in_list, MethodCodeAddrs, !IO),
%       io.write_string("};\n", !IO)
%   ),
    TCDeclRttiId = tc_rtti_id(TCName, type_class_decl),
    output_record_rtti_id_decls(Info, TCDeclRttiId, "", "", 0, _,
        !DeclSet, !IO),
    TCInstanceRttiId = tc_rtti_id(TCName, type_class_instance(TCTypes)),
    output_generic_rtti_data_defn_start(Info, TCInstanceRttiId, !DeclSet, !IO),
    io.write_string(" = {\n\t&", !IO),
    output_rtti_id(TCDeclRttiId, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(NumTypeVars, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(list.length(Constraints), !IO),
    io.write_string(",\n\t", !IO),
    output_rtti_id(TCInstanceTypesRttiId, !IO),
    io.write_string(",\n\t", !IO),
    (
        Constraints = [],
        io.write_string("NULL", !IO)
    ;
        Constraints = [_ | _],
        output_rtti_id(TCInstanceConstraintsRttiId, !IO)
    ),
%   io.write_string(",\n\t", !IO),
%   (
%       MethodProcLabels = [],
%       io.write_string("NULL", !IO)
%   ;
%       MethodProcLabels = [_ | _],
%       io.write_string("&", !IO),
%       output_rtti_id(TCInstanceMethodsRttiId, !IO)
%   ),
    io.write_string("\n};\n", !IO).

:- pred make_tc_instance_constraint_id(tc_name::in, list(tc_type)::in,
    int::in, int::in, rtti_id::out) is det.

make_tc_instance_constraint_id(TCName, TCTypes, Ordinal, NumTypes, RttiId) :-
    RttiId = tc_rtti_id(TCName,
        type_class_instance_constraint(TCTypes, Ordinal, NumTypes)).

:- pred output_code_addr_in_list(code_addr::in,
    io::di, io::uo) is det.
:- pragma consider_used(output_code_addr_in_list/3).

output_code_addr_in_list(CodeAddr, !IO) :-
    io.write_string("\t", !IO),
    output_static_code_addr(CodeAddr, !IO),
    io.write_string(",\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_type_class_constraint(llds_out_info::in,
    pred(int, int, rtti_id)::in(pred(in, in, out) is det),
    tc_constraint::in, rtti_id::out, counter::in, counter::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_class_constraint(Info, MakeRttiId, Constraint, TCDeclSuperRttiId,
        !Counter, !DeclSet, !IO) :-
    Constraint = tc_constraint(TCName, Types),
    list.length(Types, NumTypes),
    counter.allocate(TCNum, !Counter),
    MakeRttiId(TCNum, NumTypes, TCDeclSuperRttiId),
    TCDeclRttiId = tc_rtti_id(TCName, type_class_decl),
    output_generic_rtti_data_decl(Info, TCDeclRttiId, !DeclSet, !IO),
    list.foldl2(output_maybe_pseudo_type_info_defn(Info), Types,
        !DeclSet, !IO),
    TypeRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Types),
    output_generic_rtti_data_defn_start(Info, TCDeclSuperRttiId,
        !DeclSet, !IO),
    io.write_string(" = {\n\t&", !IO),
    output_rtti_id(TCDeclRttiId, !IO),
    io.write_string(",\n\t{\n", !IO),
    output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", TypeRttiDatas, !IO),
    io.write_string("\t}\n};\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_maybe_pseudo_type_info_or_self_defn(llds_out_info::in,
    rtti_maybe_pseudo_type_info_or_self::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_maybe_pseudo_type_info_or_self_defn(Info, MaybePseudoTypeInfo,
        !DeclSet, !IO) :-
    (
        MaybePseudoTypeInfo = plain(TypeInfo),
        output_type_info_defn(Info, TypeInfo, !DeclSet, !IO)
    ;
        MaybePseudoTypeInfo = pseudo(PseudoTypeInfo),
        output_pseudo_type_info_defn(Info, PseudoTypeInfo, !DeclSet, !IO)
    ;
        MaybePseudoTypeInfo = self
    ).

:- pred output_maybe_pseudo_type_info_defn(llds_out_info::in,
    rtti_maybe_pseudo_type_info::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_maybe_pseudo_type_info_defn(Info, MaybePseudoTypeInfo, !DeclSet, !IO) :-
    (
        MaybePseudoTypeInfo = plain(TypeInfo),
        output_type_info_defn(Info, TypeInfo, !DeclSet, !IO)
    ;
        MaybePseudoTypeInfo = pseudo(PseudoTypeInfo),
        output_pseudo_type_info_defn(Info, PseudoTypeInfo, !DeclSet, !IO)
    ).

:- pred output_type_info_defn(llds_out_info::in, rtti_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_info_defn(Info, TypeInfo, !DeclSet, !IO) :-
    ( if
        rtti_data_to_id(rtti_data_type_info(TypeInfo), RttiId),
        decl_set_is_member(decl_rtti_id(RttiId), !.DeclSet)
    then
        true
    else
        do_output_type_info_defn(Info, TypeInfo, !DeclSet, !IO)
    ).

:- pred do_output_type_info_defn(llds_out_info::in, rtti_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

do_output_type_info_defn(Info, TypeInfo, !DeclSet, !IO) :-
    (
        TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO)
    ;
        TypeInfo = plain_type_info(RttiTypeCtor, Args),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info,
            ctor_rtti_id(RttiTypeCtor, type_ctor_type_info(TypeInfo)),
            !DeclSet, !IO),
        io.write_string(" = {\n\t&", !IO),
        output_ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info, !IO),
        io.write_string(",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_TypeInfo) ", ArgRttiDatas, !IO),
        io.write_string("}};\n", !IO)
    ;
        TypeInfo = var_arity_type_info(RttiVarArityId, Args),
        RttiTypeCtor = var_arity_id_to_rtti_type_ctor(RttiVarArityId),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info,
            ctor_rtti_id(RttiTypeCtor, type_ctor_type_info(TypeInfo)),
            !DeclSet, !IO),
        io.write_string(" = {\n\t&", !IO),
        output_ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info, !IO),
        io.write_string(",\n\t", !IO),
        list.length(Args, Arity),
        io.write_int(Arity, !IO),
        io.write_string(",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_TypeInfo) ", ArgRttiDatas, !IO),
        io.write_string("}};\n", !IO)
    ).

:- pred output_pseudo_type_info_defn(llds_out_info::in,
    rtti_pseudo_type_info::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_pseudo_type_info_defn(Info, PseudoTypeInfo, !DeclSet, !IO) :-
    ( if
        PseudoTypeInfo = type_var(_)
    then
        true
    else if
        rtti_data_to_id(rtti_data_pseudo_type_info(PseudoTypeInfo), RttiId),
        decl_set_is_member(decl_rtti_id(RttiId), !.DeclSet)
    then
        true
    else
        do_output_pseudo_type_info_defn(Info, PseudoTypeInfo, !DeclSet, !IO)
    ).

:- pred do_output_pseudo_type_info_defn(llds_out_info::in,
    rtti_pseudo_type_info::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

do_output_pseudo_type_info_defn(Info, PseudoTypeInfo, !DeclSet, !IO) :-
    (
        PseudoTypeInfo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO)
    ;
        PseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, Args),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info,
            ctor_rtti_id(RttiTypeCtor,
                type_ctor_pseudo_type_info(PseudoTypeInfo)),
            !DeclSet, !IO),
        io.write_string(" = {\n\t&", !IO),
        output_ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info, !IO),
        io.write_string(",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas,
            !IO),
        io.write_string("}};\n", !IO)
    ;
        PseudoTypeInfo = var_arity_pseudo_type_info(RttiVarArityId, Args),
        RttiTypeCtor = var_arity_id_to_rtti_type_ctor(RttiVarArityId),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info,
            ctor_rtti_id(RttiTypeCtor,
                type_ctor_pseudo_type_info(PseudoTypeInfo)),
            !DeclSet, !IO),
        io.write_string(" = {\n\t&", !IO),
        output_ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info, !IO),
        io.write_string(",\n\t", !IO),
        list.length(Args, Arity),
        io.write_int(Arity, !IO),
        io.write_string(",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas,
            !IO),
        io.write_string("}};\n", !IO)
    ;
        PseudoTypeInfo = type_var(_)
    ).

:- pred output_type_ctor_arg_defns_and_decls(llds_out_info::in,
    list(rtti_data)::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_ctor_arg_defns_and_decls(Info, ArgRttiDatas, !DeclSet, !IO) :-
    % We must output the definitions of the rtti_datas of the argument
    % typeinfos and/or pseudo-typeinfos, because they may contain other
    % typeinfos and/or pseudo-typeinfos nested within them. However,
    % zero arity typeinfos and pseudo-typeinfos have empty definitions,
    % yet the type_ctor_info they refer to still must be declared.
    % This is why both calls below are needed.
    list.foldl2(output_rtti_data_defn(Info), ArgRttiDatas, !DeclSet, !IO),
    output_record_rtti_datas_decls(Info, ArgRttiDatas, "", "", 0, _,
        !DeclSet, !IO).

%-----------------------------------------------------------------------------%

:- pred output_type_ctor_data_defn(llds_out_info::in, type_ctor_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_ctor_data_defn(Info, TypeCtorData, !DeclSet, !IO) :-
    RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
    TypeCtorData = type_ctor_data(Version, Module, TypeName, TypeArity,
        UnifyUniv, CompareUniv, Flags, TypeCtorDetails),
    output_type_ctor_details_defn(Info, RttiTypeCtor, TypeCtorDetails,
        MaybeFunctorsName, MaybeLayoutName, HaveFunctorNumberMap,
        !DeclSet, !IO),
    det_univ_to_type(UnifyUniv, UnifyProcLabel),
    UnifyCodeAddr = make_code_addr(UnifyProcLabel),
    det_univ_to_type(CompareUniv, CompareProcLabel),
    CompareCodeAddr = make_code_addr(CompareProcLabel),
    CodeAddrs = [UnifyCodeAddr, CompareCodeAddr],
    list.foldl2(output_record_code_addr_decls(Info), CodeAddrs, !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info), !DeclSet, !IO),
    io.write_string(" = {\n\t", !IO),
    % MR_type_ctor_arity -- XXX MAKE_FIELD_UNSIGNED
    io.write_int(uint16.to_int(TypeArity), !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_version
    io.write_uint8(Version, !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_num_ptags
    MaybeNumPtags = type_ctor_details_num_ptags(TypeCtorDetails),
    (
        MaybeNumPtags = yes(NumPtags),
        NumPtagsEncoding = int8.det_from_int(NumPtags)
    ;
        MaybeNumPtags = no,
        NumPtagsEncoding = -1i8
    ),
    io.write_int8(NumPtagsEncoding, !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_rep_CAST_ME
    rtti.type_ctor_rep_to_string(TypeCtorData, _TargetPrefixes, CtorRepStr),
    io.write_string(CtorRepStr, !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_unify_pred
    output_static_code_addr(UnifyCodeAddr, !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_compare_pred
    output_static_code_addr(CompareCodeAddr, !IO),
    io.write_string(",\n\t""", !IO),
    % MR_type_ctor_module_name
    c_util.output_quoted_string_cur_stream(sym_name_to_string(Module), !IO),
    io.write_string(""",\n\t""", !IO),
    % MR_type_ctor_name
    c_util.output_quoted_string_cur_stream(TypeName, !IO),
    io.write_string(""",\n\t", !IO),
    % MR_type_ctor_functors
    (
        MaybeFunctorsName = yes(FunctorsName),
        FunctorsRttiId = ctor_rtti_id(RttiTypeCtor, FunctorsName),
        io.write_string("{ ", !IO),
        output_cast_addr_of_rtti_id("(void *) ", FunctorsRttiId, !IO),
        io.write_string(" }", !IO)
    ;
        MaybeFunctorsName = no,
        io.write_string("{ 0 }", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_layout
    (
        MaybeLayoutName = yes(LayoutName),
        LayoutRttiId = ctor_rtti_id(RttiTypeCtor, LayoutName),
        io.write_string("{ ", !IO),
        output_cast_addr_of_rtti_id("(void *) ", LayoutRttiId, !IO),
        io.write_string(" }", !IO)
    ;
        MaybeLayoutName = no,
        io.write_string("{ 0 }", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_num_functors -- XXX MAKE_FIELD_UNSIGNED
    MaybeNumFunctors = type_ctor_details_num_functors(TypeCtorDetails),
    (
        MaybeNumFunctors = yes(NumFunctors),
        NumFunctorsEncoding = int32.det_from_int(NumFunctors)
    ;
        MaybeNumFunctors = no,
        NumFunctorsEncoding = -1i32
    ),
    io.write_int32(NumFunctorsEncoding, !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_flags
    io.write_uint16(encode_type_ctor_flags(Flags), !IO),
    io.write_string(",\n\t", !IO),
    % MR_type_ctor_functor_number_map
    (
        HaveFunctorNumberMap = yes,
        FunctorNumberMapRttiId =
            ctor_rtti_id(RttiTypeCtor, type_ctor_functor_number_map),
        output_rtti_id(FunctorNumberMapRttiId, !IO)
    ;
        HaveFunctorNumberMap = no,
        io.write_string("NULL", !IO)
    ),
% This code is commented out while the corresponding fields of the
% MR_TypeCtorInfo_Struct type are commented out.
%
%   io.write_string(",\n\t"),
%   (
%       { MaybeHashCons = yes(HashConsDataAddr) },
%       io.write_string("&"),
%       output_ctor_rtti_id(RttiTypeCtor, HashConsDataAddr)
%   ;
%       { MaybeHashCons = no },
%       io.write_string("NULL")
%   ),
%   io.write_string(",\n\t"),
%   output_maybe_static_code_addr(Prettyprinter),
    io.write_string("\n};\n", !IO).

:- pred output_type_ctor_details_defn(llds_out_info::in,
    rtti_type_ctor::in, type_ctor_details::in,
    maybe(ctor_rtti_name)::out, maybe(ctor_rtti_name)::out, bool::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_ctor_details_defn(Info, RttiTypeCtor, TypeCtorDetails,
        MaybeFunctorsName, MaybeLayoutName, HaveFunctorNumberMap,
        !DeclSet, !IO) :-
    (
        TypeCtorDetails = tcd_enum(_, _IsDummy, EnumFunctors,
            EnumByRep, EnumByName, FunctorNumberMap),
        list.foldl2(output_enum_functor_defn(Info, RttiTypeCtor), EnumFunctors,
            !DeclSet, !IO),
        output_enum_value_ordered_table(Info, RttiTypeCtor, EnumByRep,
            !DeclSet, !IO),
        output_enum_name_ordered_table(Info, RttiTypeCtor, EnumByName,
            !DeclSet, !IO),
        output_functor_number_map(Info, RttiTypeCtor, FunctorNumberMap,
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_enum_value_ordered_table),
        MaybeFunctorsName = yes(type_ctor_enum_name_ordered_table),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_foreign_enum(Lang, _, ForeignEnumFunctors,
            ForeignEnumByOrdinal, ForeignEnumByName, FunctorNumberMap),
        expect(unify(Lang, lang_c), $pred,
            "language other than C for foreign enumeration"),
        list.foldl2(output_foreign_enum_functor_defn(Info, RttiTypeCtor),
            ForeignEnumFunctors, !DeclSet, !IO),
        output_foreign_enum_ordinal_ordered_table(Info, RttiTypeCtor,
            ForeignEnumByOrdinal, !DeclSet, !IO),
        output_foreign_enum_name_ordered_table(Info, RttiTypeCtor,
            ForeignEnumByName, !DeclSet, !IO),
        output_functor_number_map(Info, RttiTypeCtor, FunctorNumberMap,
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_foreign_enum_ordinal_ordered_table),
        MaybeFunctorsName = yes(type_ctor_foreign_enum_name_ordered_table),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_du(_, DuFunctors, DuByRep,
            DuByName, FunctorNumberMap),
        list.foldl2(output_du_functor_defn(Info, RttiTypeCtor), DuFunctors,
            !DeclSet, !IO),
        output_du_ptag_ordered_table(Info, RttiTypeCtor, DuByRep,
            !DeclSet, !IO),
        output_du_name_ordered_table(Info, RttiTypeCtor, DuByName,
            !DeclSet, !IO),
        output_functor_number_map(Info, RttiTypeCtor, FunctorNumberMap,
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_du_ptag_ordered_table),
        MaybeFunctorsName = yes(type_ctor_du_name_ordered_table),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_notag(_, NotagFunctor),
        output_notag_functor_defn(Info, RttiTypeCtor, NotagFunctor,
            !DeclSet, !IO),
        output_functor_number_map(Info, RttiTypeCtor, [0u32], !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_notag_functor_desc),
        MaybeFunctorsName = yes(type_ctor_notag_functor_desc),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_eqv(EqvType),
        output_maybe_pseudo_type_info_defn(Info, EqvType, !DeclSet, !IO),
        TypeData = maybe_pseudo_type_info_to_rtti_data(EqvType),
        output_record_rtti_data_decls(Info, TypeData, "", "", 0, _,
            !DeclSet, !IO),
        (
            EqvType = plain(TypeInfo),
            LayoutName = type_ctor_type_info(TypeInfo)
        ;
            EqvType = pseudo(PseudoTypeInfo),
            LayoutName = type_ctor_pseudo_type_info(PseudoTypeInfo)
        ),
        MaybeLayoutName = yes(LayoutName),
        MaybeFunctorsName = no,
        HaveFunctorNumberMap = no
    ;
        ( TypeCtorDetails = tcd_builtin(_)
        ; TypeCtorDetails = tcd_impl_artifact(_)
        ; TypeCtorDetails = tcd_foreign(_)
        ),
        MaybeLayoutName = no,
        MaybeFunctorsName = no,
        HaveFunctorNumberMap = no
    ).

%-----------------------------------------------------------------------------%

:- pred output_enum_functor_defn(llds_out_info::in, rtti_type_ctor::in,
    enum_functor::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_enum_functor_defn(Info, RttiTypeCtor, EnumFunctor, !DeclSet, !IO) :-
    EnumFunctor = enum_functor(FunctorName, Ordinal),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_enum_functor_desc(Ordinal)),
        !DeclSet, !IO),
    io.write_string(" = {\n\t""", !IO),
    % MR_enum_functor_name
    c_util.output_quoted_string_cur_stream(FunctorName, !IO),
    io.write_string(""",\n\t", !IO),
    % MR_enum_functor_ordinal -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(int32.cast_from_uint32(Ordinal), !IO),
    io.write_string("\n};\n", !IO).

:- pred output_foreign_enum_functor_defn(llds_out_info::in, rtti_type_ctor::in,
    foreign_enum_functor::in, decl_set::in, decl_set::out, io::di, io::uo)
    is det.

output_foreign_enum_functor_defn(Info, RttiTypeCtor, ForeignEnumFunctor,
        !DeclSet, !IO) :-
    ForeignEnumFunctor = foreign_enum_functor(FunctorName, FunctorOrdinal,
        FunctorValue),
    RttiId = ctor_rtti_id(RttiTypeCtor,
        type_ctor_foreign_enum_functor_desc(FunctorOrdinal)),
    output_generic_rtti_data_defn_start(Info, RttiId, !DeclSet, !IO),
    io.write_string(" = {\n\t""", !IO),
    % MR_foreign_enum_functor_name
    c_util.output_quoted_string_cur_stream(FunctorName, !IO),
    io.write_string(""",\n\t", !IO),
    % MR_foreign_enum_functor_ordinal -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(int32.cast_from_uint32(FunctorOrdinal), !IO),
    io.write_string(",\n\t", !IO),
    % MR_foreign_enum_functor_value
    io.write_string(FunctorValue, !IO),
    io.write_string("\n};\n", !IO).

:- pred output_notag_functor_defn(llds_out_info::in, rtti_type_ctor::in,
    notag_functor::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_notag_functor_defn(Info, RttiTypeCtor, NotagFunctor, !DeclSet, !IO) :-
    NotagFunctor = notag_functor(FunctorName, ArgType, MaybeArgName,
        FunctorSubtypeInfo),
    output_maybe_pseudo_type_info_defn(Info, ArgType, !DeclSet, !IO),
    ArgTypeData = maybe_pseudo_type_info_to_rtti_data(ArgType),
    output_record_rtti_data_decls(Info, ArgTypeData, "", "", 0, _,
        !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_notag_functor_desc),
        !DeclSet, !IO),
    io.write_string(" = {\n\t""", !IO),
    % MR_notag_functor_name
    c_util.output_quoted_string_cur_stream(FunctorName, !IO),
    io.write_string(""",\n\t", !IO),
    % MR_notag_functor_arg_type
    (
        ArgType = plain(ArgTypeInfo),
        output_cast_addr_of_rtti_data("(MR_PseudoTypeInfo) ",
            rtti_data_type_info(ArgTypeInfo), !IO)
    ;
        ArgType = pseudo(ArgPseudoTypeInfo),
        % We need to cast the argument to MR_PseudoTypeInfo in case
        % it turns out to be a small integer, not a pointer.
        output_cast_addr_of_rtti_data("(MR_PseudoTypeInfo) ",
            rtti_data_pseudo_type_info(ArgPseudoTypeInfo), !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_notag_functor_arg_name
    (
        MaybeArgName = yes(ArgName),
        io.write_string("""", !IO),
        io.write_string(ArgName, !IO),
        io.write_string("""", !IO)
    ;
        MaybeArgName = no,
        io.write_string("NULL", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_notag_functor_subtype
    output_functor_subtype_info(FunctorSubtypeInfo, !IO),
    io.write_string("\n};\n", !IO).

:- pred output_du_functor_defn(llds_out_info::in, rtti_type_ctor::in,
    du_functor::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_functor_defn(Info, RttiTypeCtor, DuFunctor, !DeclSet, !IO) :-
    DuFunctor = du_functor(FunctorName, OrigArity, Ordinal, Rep,
        ArgInfos, MaybeExistInfo, FunctorSubtypeInfo),
    ArgTypes = list.map(du_arg_info_type, ArgInfos),
    MaybeArgNames = list.map(du_arg_info_name, ArgInfos),
    HaveArgNames = (if list.member(yes(_), MaybeArgNames) then yes else no),
    (
        ArgInfos = [_ | _],
        output_du_arg_types(Info, RttiTypeCtor, Ordinal, ArgTypes,
            !DeclSet, !IO)
    ;
        ArgInfos = []
    ),
    (
        HaveArgNames = yes,
        output_du_arg_names(Info, RttiTypeCtor, Ordinal, MaybeArgNames,
            !DeclSet, !IO)
    ;
        HaveArgNames = no
    ),
    output_du_arg_locns(Info, RttiTypeCtor, Ordinal, ArgInfos,
        HaveArgLocns, !DeclSet, !IO),
    (
        MaybeExistInfo = yes(ExistInfo),
        output_exist_info(Info, RttiTypeCtor, Ordinal, ExistInfo,
            !DeclSet, !IO)
    ;
        MaybeExistInfo = no
    ),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_functor_desc(Ordinal)),
        !DeclSet, !IO),
    io.write_string(" = {\n\t""", !IO),
    % MR_du_functor_name
    c_util.output_quoted_string_cur_stream(FunctorName, !IO),
    io.write_string(""",\n\t", !IO),
    % MR_du_functor_orig_arity -- XXX MAKE_FIELD_UNSIGNED
    io.write_int16(int16.cast_from_uint16(OrigArity), !IO),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_arg_type_contains_var
    ContainsVarBitVector = compute_contains_var_bit_vector(ArgTypes),
    io.write_uint16(ContainsVarBitVector, !IO),
    io.write_string(",\n\t", !IO),
    (
        Rep = du_ll_rep(Ptag, SectagAndLocn)
    ;
        Rep = du_hl_rep(_),
        unexpected($pred, "du_hl_rep")
    ),
    Ptag = ptag(PtagUint8),
    (
        SectagAndLocn = sectag_locn_none,
        Locn = "MR_SECTAG_NONE",
        StagEncoding = -1i32,
        NumSectagBits = 0u8
    ;
        SectagAndLocn = sectag_locn_none_direct_arg,
        Locn = "MR_SECTAG_NONE_DIRECT_ARG",
        StagEncoding = -1i32,
        NumSectagBits = 0u8
    ;
        SectagAndLocn = sectag_locn_local_rest_of_word(StagUint),
        Locn = "MR_SECTAG_LOCAL_REST_OF_WORD",
        StagEncoding = int32.det_from_int(uint.cast_to_int(StagUint)),
        NumSectagBits = 0u8
    ;
        SectagAndLocn = sectag_locn_local_bits(StagUint, NumSectagBits, _Mask),
        Locn = "MR_SECTAG_LOCAL_BITS",
        StagEncoding = int32.det_from_int(uint.cast_to_int(StagUint))
    ;
        SectagAndLocn = sectag_locn_remote_word(StagUint),
        Locn = "MR_SECTAG_REMOTE_FULL_WORD",
        StagEncoding = int32.det_from_int(uint.cast_to_int(StagUint)),
        NumSectagBits = 0u8
    ;
        SectagAndLocn = sectag_locn_remote_bits(StagUint, NumSectagBits,
            _Mask),
        Locn = "MR_SECTAG_REMOTE_BITS",
        StagEncoding = int32.det_from_int(uint.cast_to_int(StagUint))
    ),
    % MR_du_functor_sectag_locn
    io.write_string(Locn, !IO),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_primary
    io.write_uint8(PtagUint8, !IO),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_secondary -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(StagEncoding, !IO),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_ordinal -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(int32.cast_from_uint32(Ordinal), !IO),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_arg_types
    io.write_string("(MR_PseudoTypeInfo *) ", !IO), % cast away const
    (
        ArgInfos = [_ | _],
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_field_types(Ordinal), !IO)
    ;
        ArgInfos = [],
        io.write_string("NULL", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_arg_names
    (
        HaveArgNames = yes,
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_field_names(Ordinal), !IO)
    ;
        HaveArgNames = no,
        io.write_string("NULL", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_arg_locns
    (
        HaveArgLocns = yes,
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_field_locns(Ordinal), !IO)
    ;
        HaveArgLocns = no,
        io.write_string("NULL", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_exist_info
    (
        MaybeExistInfo = yes(_),
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_exist_info(Ordinal), !IO)
    ;
        MaybeExistInfo = no,
        io.write_string("NULL", !IO)
    ),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_subtype
    output_functor_subtype_info(FunctorSubtypeInfo, !IO),
    io.write_string(",\n\t", !IO),
    % MR_du_functor_num_sectag_bits
    io.write_uint8(NumSectagBits, !IO),
    io.write_string("\n};\n", !IO).

:- pred output_functor_subtype_info(functor_subtype_info::in, io::di, io::uo)
    is det.

output_functor_subtype_info(FunctorSubtypeInfo, !IO) :-
    (
        FunctorSubtypeInfo = functor_subtype_none,
        io.write_string("MR_FUNCTOR_SUBTYPE_NONE", !IO)
    ;
        FunctorSubtypeInfo = functor_subtype_exists,
        io.write_string("MR_FUNCTOR_SUBTYPE_EXISTS", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred output_exist_locns_array(llds_out_info::in, rtti_type_ctor::in,
    uint32::in, list(exist_typeinfo_locn)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_exist_locns_array(Info, RttiTypeCtor, Ordinal, Locns, !DeclSet, !IO) :-
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_exist_locns(Ordinal)),
        !DeclSet, !IO),
    (
        % ANSI/ISO C doesn't allow empty arrays, so
        % place a dummy value in the array if necessary.
        Locns = [],
        io.write_string("= { {0, 0} };\n", !IO)
    ;
        Locns = [_ | _],
        io.write_string(" = {\n", !IO),
        output_exist_locns(Locns, !IO),
        io.write_string("};\n", !IO)
    ).

:- pred make_exist_tc_constr_id(rtti_type_ctor::in, uint32::in,
    int::in, int::in, rtti_id::out) is det.

make_exist_tc_constr_id(RttiTypeCtor, Ordinal, TCNum, Arity, RttiId) :-
    RttiName = type_ctor_exist_tc_constr(Ordinal, TCNum, Arity),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName).

:- pred output_exist_constraints_data(llds_out_info::in, rtti_type_ctor::in,
    uint32::in, list(tc_constraint)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_exist_constraints_data(Info, RttiTypeCtor, Ordinal, Constraints,
        !DeclSet, !IO) :-
    list.map_foldl3(output_type_class_constraint(Info,
        make_exist_tc_constr_id(RttiTypeCtor, Ordinal)), Constraints,
        ConstraintIds, counter.init(1), _, !DeclSet, !IO),
    RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_exist_tc_constrs(Ordinal)),
    output_generic_rtti_data_defn_start(Info, RttiId, !DeclSet, !IO),
    io.write_string(" = {\n\t", !IO),
    output_cast_addr_of_rtti_ids("(MR_TypeClassConstraint) ", ConstraintIds,
        !IO),
    io.write_string("\n};\n", !IO).

:- pred output_exist_info(llds_out_info::in, rtti_type_ctor::in, uint32::in,
    exist_info::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_exist_info(Info, RttiTypeCtor, Ordinal, ExistInfo, !DeclSet, !IO) :-
    ExistInfo = exist_info(Plain, InTci, Constraints, Locns),
    output_exist_locns_array(Info, RttiTypeCtor, Ordinal, Locns,
        !DeclSet, !IO),
    (
        Constraints = [_ | _],
        output_exist_constraints_data(Info, RttiTypeCtor, Ordinal, Constraints,
            !DeclSet, !IO)
    ;
        Constraints = []
    ),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_exist_info(Ordinal)),
        !DeclSet, !IO),
    io.write_string(" = {\n\t", !IO),
    % MR_exist_typeinfos_plain -- XXX MAKE_FIELD_UNSIGNED
    io.write_int16(int16.cast_from_uint16(Plain), !IO),
    io.write_string(",\n\t", !IO),
    % MR_exist_typeinfos_in_tci -- XXX MAKE_FIELD_UNSIGNED
    io.write_int16(int16.cast_from_uint16(InTci), !IO),
    io.write_string(",\n\t", !IO),
    % MR_exist_tcis
    list.length(Constraints, Tci),
    io.write_int16(int16.det_from_int(Tci), !IO),
    io.write_string(",\n\t", !IO),
    % MR_exist_typeinfo_locns
    output_ctor_rtti_id(RttiTypeCtor, type_ctor_exist_locns(Ordinal), !IO),
    io.write_string(",\n\t", !IO),
    % MR_exist_constraints
    (
        Constraints = [_ | _],
        output_ctor_rtti_id(RttiTypeCtor, type_ctor_exist_tc_constrs(Ordinal),
            !IO)
    ;
        Constraints = [],
        io.write_string("NULL", !IO)
    ),
    io.write_string("\n};\n", !IO).

:- pred output_du_arg_types(llds_out_info::in, rtti_type_ctor::in, uint32::in,
    list(rtti_maybe_pseudo_type_info_or_self)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_arg_types(Info, RttiTypeCtor, Ordinal, ArgTypes, !DeclSet, !IO) :-
    list.foldl2(output_maybe_pseudo_type_info_or_self_defn(Info), ArgTypes,
        !DeclSet, !IO),
    ArgTypeDatas = list.map(maybe_pseudo_type_info_or_self_to_rtti_data,
        ArgTypes),
    output_record_rtti_datas_decls(Info, ArgTypeDatas, "", "", 0, _,
        !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_field_types(Ordinal)),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    expect(list.is_not_empty(ArgTypes), $pred, "empty list"),
    output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgTypeDatas, !IO),
    io.write_string("};\n", !IO).

:- pred output_du_arg_names(llds_out_info::in, rtti_type_ctor::in, uint32::in,
    list(maybe(string))::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_du_arg_names(Info, RttiTypeCtor, Ordinal, MaybeNames, !DeclSet, !IO) :-
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_field_names(Ordinal)),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    expect(list.is_not_empty(MaybeNames), $pred, "empty list"),
    output_maybe_quoted_strings(MaybeNames, !IO),
    io.write_string("};\n", !IO).

:- pred output_du_arg_locns(llds_out_info::in, rtti_type_ctor::in, uint32::in,
    list(du_arg_info)::in, bool::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_du_arg_locns(Info, RttiTypeCtor, Ordinal, ArgInfos, HaveArgLocns,
        !DeclSet, !IO) :-
    ( if
        some [ArgInfo] (
            list.member(ArgInfo, ArgInfos),
            ArgInfo = du_arg_info(_, _, Width),
            Width \= apw_full(_, _)
        )
    then
        output_generic_rtti_data_defn_start(Info,
            ctor_rtti_id(RttiTypeCtor, type_ctor_field_locns(Ordinal)),
            !DeclSet, !IO),
        io.write_string(" = {\n", !IO),
        output_du_arg_locns_loop(ArgInfos, !IO),
        io.write_string("};\n", !IO),
        HaveArgLocns = yes
    else
        HaveArgLocns = no
    ).

:- pred output_du_arg_locns_loop(list(du_arg_info)::in, io::di, io::uo) is det.

output_du_arg_locns_loop([], !IO).
output_du_arg_locns_loop([ArgInfo | ArgInfos], !IO) :-
    ArgWidth = ArgInfo ^ du_arg_pos_width,
    % The meanings of the various special values of MR_arg_bits
    % are documented next to the definition of the MR_DuArgLocn type
    % in mercury_type_info.h.
    (
        ArgWidth = apw_full(arg_only_offset(ArgOnlyOffset), _CellOffset),
        % NumBits = 0 means the argument takes a full word.
        Shift = 0,
        NumBits = 0
    ;
        ArgWidth = apw_double(arg_only_offset(ArgOnlyOffset), _CellOffset,
            DoubleWordKind),
        % NumBits = -1, -2 and -3 are all special cases, meaning
        % double words containing floats, int64s and uint64s respectively.
        Shift = 0,
        (
            DoubleWordKind = dw_float,
            NumBits = -1
        ;
            DoubleWordKind = dw_int64,
            NumBits = -2
        ;
            DoubleWordKind = dw_uint64,
            NumBits = -3
        )
    ;
        (
            ArgWidth = apw_partial_first(arg_only_offset(ArgOnlyOffset), _,
                arg_shift(Shift), arg_num_bits(NumBits0), _Mask, Fill)
        ;
            ArgWidth = apw_partial_shifted(arg_only_offset(ArgOnlyOffset), _,
                arg_shift(Shift), arg_num_bits(NumBits0), _Mask, Fill)
        ),
        (
            ( Fill = fill_enum
            ; Fill = fill_char21
            ),
            NumBits = NumBits0
        ;
            Fill = fill_int8,
            % NumBits = -4 is a special case meaning "int8".
            NumBits = -4
        ;
            Fill = fill_uint8,
            % NumBits = -5 is a special case meaning "uint8".
            NumBits = -5
        ;
            Fill = fill_int16,
            % NumBits = -6 is a special case meaning "int16".
            NumBits = -6
        ;
            Fill = fill_uint16,
            % NumBits = -7 is a special case meaning "uint16".
            NumBits = -7
        ;
            Fill = fill_int32,
            % NumBits = -8 is a special case meaning "int32".
            NumBits = -8
        ;
            Fill = fill_uint32,
            % NumBits = -9 is a special case meaning "uint32".
            NumBits = -9
        )
    ;
        (
            ArgWidth = apw_none_shifted(arg_only_offset(ArgOnlyOffset), _)
        ;
            ArgWidth = apw_none_nowhere,
            ArgOnlyOffset = -1
        ),
        % NumBits = -10 is a special case meaning "dummy argument".
        Shift = 0,
        NumBits = -10
    ),
    % MR_arg_offset, MR_arg_shift, MR_arg_bits
    io.format("\t{ %d, %d, %d },\n",
        [i(ArgOnlyOffset), i(Shift), i(NumBits)], !IO),
    output_du_arg_locns_loop(ArgInfos, !IO).

%-----------------------------------------------------------------------------%

:- pred output_enum_value_ordered_table(llds_out_info::in, rtti_type_ctor::in,
    map(uint32, enum_functor)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_enum_value_ordered_table(Info, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(enum_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_enum_value_ordered_table),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames, !IO),
    io.write_string("};\n", !IO).

:- pred output_enum_name_ordered_table(llds_out_info::in, rtti_type_ctor::in,
    map(string, enum_functor)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_enum_name_ordered_table(Info, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(enum_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_enum_name_ordered_table),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames, !IO),
    io.write_string("};\n", !IO).

:- pred output_foreign_enum_ordinal_ordered_table(llds_out_info::in,
    rtti_type_ctor::in, map(uint32, foreign_enum_functor)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_foreign_enum_ordinal_ordered_table(Info, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(foreign_enum_functor_rtti_name, Functors),
    RttiId = ctor_rtti_id(RttiTypeCtor,
        type_ctor_foreign_enum_ordinal_ordered_table),
    output_generic_rtti_data_defn_start(Info, RttiId, !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames, !IO),
    io.write_string("};\n", !IO).

:- pred output_foreign_enum_name_ordered_table(llds_out_info::in,
    rtti_type_ctor::in, map(string, foreign_enum_functor)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_foreign_enum_name_ordered_table(Info, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(foreign_enum_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_foreign_enum_name_ordered_table),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames, !IO),
    io.write_string("};\n", !IO).

:- pred output_du_name_ordered_table(llds_out_info::in, rtti_type_ctor::in,
    map(string, map(uint16, du_functor))::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_du_name_ordered_table(Info, RttiTypeCtor, NameArityMap,
        !DeclSet, !IO) :-
    map.values(NameArityMap, ArityMaps),
    list.map(map.values, ArityMaps, FunctorLists),
    list.condense(FunctorLists, Functors),
    FunctorRttiNames = list.map(du_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_name_ordered_table),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames, !IO),
    io.write_string("};\n", !IO).

:- pred output_du_stag_ordered_table(llds_out_info::in, rtti_type_ctor::in,
    pair(ptag, sectag_table)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_du_stag_ordered_table(Info, RttiTypeCtor, Ptag - SectagTable,
        !DeclSet, !IO) :-
    SectagTable = sectag_table(_SectagLocn, _NumSectagBits, _NumSharers,
        SectagMap),
    map.values(SectagMap, SectagFunctors),
    FunctorNames = list.map(du_functor_rtti_name, SectagFunctors),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_stag_ordered_table(Ptag)),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorNames, !IO),
    io.write_string("\n};\n", !IO).

:- pred output_du_ptag_ordered_table(llds_out_info::in, rtti_type_ctor::in,
    map(ptag, sectag_table)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_du_ptag_ordered_table(Info, RttiTypeCtor, PtagMap, !DeclSet, !IO) :-
    map.to_assoc_list(PtagMap, PtagList),
    list.foldl2(output_du_stag_ordered_table(Info, RttiTypeCtor), PtagList,
        !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_ptag_ordered_table),
        !DeclSet, !IO),
    io.write_string(" = {\n", !IO),
    ( if PtagList = [ptag(0u8) - _ | _] then
        FirstPtag = ptag(0u8)
    else
        unexpected($pred, "bad ptag list")
    ),
    output_du_ptag_ordered_table_body(RttiTypeCtor, PtagList, FirstPtag, !IO),
    io.write_string("\n};\n", !IO).

:- pred output_du_ptag_ordered_table_body(rtti_type_ctor::in,
    assoc_list(ptag, sectag_table)::in, ptag::in, io::di, io::uo) is det.

output_du_ptag_ordered_table_body(_RttiTypeCtor, [], _CurPtag, !IO).
output_du_ptag_ordered_table_body(RttiTypeCtor,
        [Ptag - SectagTable | PtagTail], CurPtag, !IO) :-
    expect(unify(Ptag, CurPtag), $pred, "ptag mismatch"),
    SectagTable = sectag_table(SectagLocn, NumSectagBits, NumSharers,
        _SectagMap),
    io.write_string("\t{ ", !IO),
    % MR_sectag_sharers
    io.write_uint32(NumSharers, !IO),
    io.write_string(", ", !IO),
    % MR_sectag_locn
    rtti.sectag_locn_to_string(SectagLocn, _TargetPrefixes, LocnStr),
    io.write_string(LocnStr, !IO),
    io.write_string(",\n\t", !IO),
    % MR_sectag_alternatives
    output_ctor_rtti_id(RttiTypeCtor, type_ctor_du_stag_ordered_table(Ptag),
        !IO),
    io.write_string(",\n\t", !IO),
    % MR_sectag_numbits
    io.write_int8(NumSectagBits, !IO),
    (
        PtagTail = [],
        io.write_string(" }\n", !IO)
    ;
        PtagTail = [_ | _],
        io.write_string(" },\n", !IO),
        CurPtag = ptag(CurPtagUint8),
        NextPtag = ptag(CurPtagUint8 + 1u8),
        output_du_ptag_ordered_table_body(RttiTypeCtor, PtagTail,
            NextPtag, !IO)
    ).

%-----------------------------------------------------------------------------%

:- func make_code_addr(rtti_proc_label) = code_addr.

make_code_addr(ProcLabel) =
    make_entry_label_from_rtti(ProcLabel, no).

%-----------------------------------------------------------------------------%

:- pred output_functor_number_map(llds_out_info::in, rtti_type_ctor::in,
    list(uint32)::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_functor_number_map(Info, RttiTypeCtor, FunctorNumberMap,
        !DeclSet, !IO) :-
   output_generic_rtti_data_defn_start(Info,
        ctor_rtti_id(RttiTypeCtor, type_ctor_functor_number_map),
        !DeclSet, !IO),
    io.write_string(" = {\n\t", !IO),
    io.write_list(FunctorNumberMap, ",\n\t", output_functor_number_map_value,
        !IO),
    io.write_string("\n};\n\t", !IO).

:- pred output_functor_number_map_value(uint32::in, io::di, io::uo) is det.

output_functor_number_map_value(NumUint32, !IO) :-
    % XXX MAKE_FIELD_UNSIGNED
    Num = uint32.cast_to_int(NumUint32),
    io.write_int(Num, !IO).

%-----------------------------------------------------------------------------%

:- pred output_generic_rtti_data_decl(llds_out_info::in, rtti_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_generic_rtti_data_decl(Info, RttiId, !DeclSet, !IO) :-
    output_rtti_id_storage_type_name(Info, RttiId, no, !DeclSet, !IO),
    io.write_string(";\n", !IO),
    decl_set_insert(decl_rtti_id(RttiId), !DeclSet).

:- pred output_generic_rtti_data_defn_start(llds_out_info::in, rtti_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_generic_rtti_data_defn_start(Info, RttiId, !DeclSet, !IO) :-
    io.write_string("\n", !IO),
    output_rtti_id_storage_type_name(Info, RttiId, yes, !DeclSet, !IO),
    decl_set_insert(decl_rtti_id(RttiId), !DeclSet).

%-----------------------------------------------------------------------------%

init_rtti_data_if_nec(Data, !IO) :-
    (
        Data = rtti_data_type_ctor_info(TypeCtorData),
        RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
        io.write_string("\tMR_INIT_TYPE_CTOR_INFO(\n\t\t", !IO),
        output_ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info, !IO),
        io.write_string(",\n\t\t", !IO),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity),
        ModuleNameString = sym_name_mangle(ModuleName),
        string.append(ModuleNameString, "__", UnderscoresModule),
        ( if string.append(UnderscoresModule, _, TypeName) then
            true
        else
            io.write_string(UnderscoresModule, !IO)
        ),
        MangledTypeName = name_mangle(TypeName),
        io.write_string(MangledTypeName, !IO),
        io.write_string("_", !IO),
        io.write_uint16(Arity, !IO),
        io.write_string("_0);\n", !IO)
    ;
        Data = rtti_data_base_typeclass_info(TCName, _ModuleName, ClassArity,
            base_typeclass_info(_N1, _N2, _N3, _N4, _N5, Methods)),
        io.write_string("#ifndef MR_STATIC_CODE_ADDRESSES\n", !IO),
        % The field number for the first method is 5, since the methods are
        % stored after N1 .. N5, and fields are numbered from 0.
        FirstFieldNum = 5,
        CodeAddrs = list.map(make_code_addr, Methods),
        output_init_method_pointers(FirstFieldNum, CodeAddrs,
            TCName, ClassArity, !IO),
        io.write_string("#endif /* MR_STATIC_CODE_ADDRESSES */\n", !IO)
    ;
        Data = rtti_data_type_class_instance(_),
        io.write_string("#ifndef MR_STATIC_CODE_ADDRESSES\n", !IO),
        io.write_string("#error ""type_class_instance " ++
            "not yet supported without static code addresses""\n", !IO),
        io.write_string("#endif /* MR_STATIC_CODE_ADDRESSES */\n", !IO)
    ;
        ( Data = rtti_data_type_info(_)
        ; Data = rtti_data_pseudo_type_info(_)
        ; Data = rtti_data_type_class_decl(_)
        )
    ).

register_rtti_data_if_nec(Data, !IO) :-
    (
        Data = rtti_data_type_ctor_info(TypeCtorData),
        RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        io.write_string("\t{\n\t", !IO),
        io.write_string("\tMR_register_type_ctor_info(\n\t\t&", !IO),
        output_rtti_id(RttiId, !IO),
        io.write_string(");\n\t}\n", !IO)
    ;
        Data = rtti_data_type_class_decl(TCDecl),
        TCDecl = tc_decl(TCId, _, _),
        TCId = tc_id(TCName, _, _),
        RttiId = tc_rtti_id(TCName, type_class_decl),
        io.write_string("\t{\n\t", !IO),
        io.write_string("\tMR_register_type_class_decl(\n\t\t&", !IO),
        output_rtti_id(RttiId, !IO),
        io.write_string(");\n\t}\n", !IO)
    ;
        Data = rtti_data_type_class_instance(TCInstance),
        TCInstance = tc_instance(TCName, TCTypes, _, _, _),
        RttiId = tc_rtti_id(TCName, type_class_instance(TCTypes)),
        io.write_string("\t{\n\t", !IO),
        io.write_string("\tMR_register_type_class_instance(\n\t\t&", !IO),
        output_rtti_id(RttiId, !IO),
        io.write_string(");\n\t}\n", !IO)
    ;
        ( Data = rtti_data_type_info(_)
        ; Data = rtti_data_pseudo_type_info(_)
        ; Data = rtti_data_base_typeclass_info(_, _, _, _)
        )
    ).

:- pred output_init_method_pointers(int::in, list(code_addr)::in, tc_name::in,
    string::in, io::di, io::uo) is det.

output_init_method_pointers(_, [], _, _, !IO).
output_init_method_pointers(FieldNum, [Arg | Args], TCName, InstanceStr,
        !IO) :-
    io.write_string("\t\t", !IO),
    io.write_string("MR_field(MR_mktag(0), ", !IO),
    output_base_typeclass_info_name(TCName, InstanceStr, !IO),
    io.format(", %d) =\n\t\t\t", [i(FieldNum)], !IO),
    output_code_addr(Arg, !IO),
    io.write_string(";\n", !IO),
    output_init_method_pointers(FieldNum + 1, Args, TCName, InstanceStr, !IO).

%-----------------------------------------------------------------------------%

:- pred output_record_rtti_datas_decls(llds_out_info::in, list(rtti_data)::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_rtti_datas_decls(_, [], _, _, !N, !DeclSet, !IO).
output_record_rtti_datas_decls(Info, [RttiData | RttiDatas],
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    output_record_rtti_data_decls(Info, RttiData,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO),
    output_record_rtti_datas_decls(Info, RttiDatas,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

:- pred output_record_rtti_data_decls(llds_out_info::in, rtti_data::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_rtti_data_decls(Info, RttiData, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(_)) then
        % These just get represented as integers, so we don't need to declare
        % them. Also rtti_data_to_id/3 does not handle this case.
        true
    else
        rtti_data_to_id(RttiData, RttiId),
        output_record_rtti_id_decls(Info, RttiId, FirstIndent, LaterIndent,
            !N, !DeclSet, !IO)
    ).

:- pred output_record_rtti_id_decls(llds_out_info::in, rtti_id::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_rtti_id_decls(Info, RttiId, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    output_record_data_id_decls_format(Info, rtti_data_id(RttiId),
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

:- pred output_cast_addr_of_rtti_ids(string::in, list(rtti_id)::in,
    io::di, io::uo) is det.

output_cast_addr_of_rtti_ids(_, [], !IO) :-
    io.write_string(
        "\t/* Dummy entry, since ISO C forbids zero-sized arrays */\n", !IO),
    io.write_string("\t0\n", !IO).
output_cast_addr_of_rtti_ids(Cast, [TCRttiName | TCRttiNames], !IO) :-
    io.write_string("\t", !IO),
    io.write_list([TCRttiName | TCRttiNames], ",\n\t",
        output_cast_addr_of_rtti_id(Cast), !IO),
    io.write_string("\n", !IO).

:- pred output_addr_of_ctor_rtti_names(rtti_type_ctor::in,
    list(ctor_rtti_name)::in, io::di, io::uo) is det.

output_addr_of_ctor_rtti_names(_, [], !IO).
output_addr_of_ctor_rtti_names(RttiTypeCtor, [RttiName | RttiNames], !IO) :-
    io.write_string("\t", !IO),
    io.write_list([RttiName | RttiNames], ",\n\t",
        output_addr_of_ctor_rtti_id(RttiTypeCtor), !IO),
    io.write_string("\n", !IO).

:- pred output_cast_addr_of_rtti_datas(string::in, list(rtti_data)::in,
    io::di, io::uo) is det.

output_cast_addr_of_rtti_datas(_, [], !IO) :-
    io.write_string(
        "\t/* Dummy entry, since ISO C forbids zero-sized arrays */\n", !IO),
    io.write_string("\t0\n", !IO).
output_cast_addr_of_rtti_datas(Cast, [RttiData | RttiDatas], !IO) :-
    io.write_string("\t", !IO),
    io.write_list([RttiData | RttiDatas], ",\n\t",
        output_cast_addr_of_rtti_data(Cast), !IO),
    io.write_string("\n", !IO).

output_cast_addr_of_rtti_data(Cast, RttiData, !IO) :-
    io.write_string(Cast, !IO),
    output_addr_of_rtti_data(RttiData, !IO).

output_addr_of_rtti_data(RttiData, !IO) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(VarNum)) then
        % rtti_data_to_id/3 does not handle this case
        io.write_int(VarNum, !IO)
    else
        rtti_data_to_id(RttiData, RttiId),
        output_addr_of_rtti_id(RttiId, !IO)
    ).

:- pred output_cast_addr_of_rtti_id(string::in, rtti_id::in,
    io::di, io::uo) is det.

output_cast_addr_of_rtti_id(Cast, RttiId, !IO) :-
    io.write_string(Cast, !IO),
    output_addr_of_rtti_id(RttiId, !IO).

:- pred output_addr_of_rtti_id(rtti_id::in, io::di, io::uo) is det.

output_addr_of_rtti_id(RttiId, !IO) :-
    % All RttiIds are references to memory, with one exception: type variables.
    ( if
        RttiId = ctor_rtti_id(_, type_ctor_pseudo_type_info(type_var(VarNum)))
    then
        io.write_int(VarNum, !IO)
    else
        % If the RttiName is not an array, then we need to use `&'
        % to take its address.
        IsArray = rtti_id_has_array_type(RttiId),
        (
            IsArray = is_array,
            output_rtti_id(RttiId, !IO)
        ;
            IsArray = not_array,
            io.write_string("&", !IO),
            output_rtti_id(RttiId, !IO)
        )
    ).

:- pred output_addr_of_ctor_rtti_id(rtti_type_ctor::in, ctor_rtti_name::in,
    io::di, io::uo) is det.

output_addr_of_ctor_rtti_id(RttiTypeCtor, RttiName, !IO) :-
    output_addr_of_rtti_id(ctor_rtti_id(RttiTypeCtor, RttiName), !IO).

output_rtti_id(RttiId, !IO) :-
    io.write_string(mercury_data_prefix, !IO),
    rtti.id_to_c_identifier(RttiId, Str),
    io.write_string(Str, !IO).

:- pred output_ctor_rtti_id(rtti_type_ctor::in, ctor_rtti_name::in,
    io::di, io::uo) is det.

output_ctor_rtti_id(RttiTypeCtor, RttiName, !IO) :-
    output_rtti_id(ctor_rtti_id(RttiTypeCtor, RttiName), !IO).

%-----------------------------------------------------------------------------%

:- pred output_maybe_quoted_string(maybe(string)::in, io::di, io::uo) is det.

output_maybe_quoted_string(MaybeName, !IO) :-
    (
        MaybeName = yes(Name),
        io.write_string("""", !IO),
        c_util.output_quoted_string_cur_stream(Name, !IO),
        io.write_string("""", !IO)
    ;
        MaybeName = no,
        io.write_string("NULL", !IO)
    ).

:- pred output_maybe_quoted_strings(list(maybe(string))::in,
    io::di, io::uo) is det.

output_maybe_quoted_strings(MaybeNames, !IO) :-
    io.write_string("\t", !IO),
    io.write_list(MaybeNames, ",\n\t", output_maybe_quoted_string, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_exist_locn(exist_typeinfo_locn::in, io::di, io::uo) is det.

output_exist_locn(Locn, !IO) :-
    (
        Locn = plain_typeinfo(SlotInCell),
        io.write_string("{ ", !IO),
        % MR_exist_arg_num -- XXX MAKE_FIELD_UNSIGNED
        io.write_int16(int16.cast_from_uint16(SlotInCell), !IO),
        % MR_exist_offset_in_tci
        io.write_string(", -1 }", !IO)
    ;
        Locn = typeinfo_in_tci(SlotInCell, SlotInTci),
        io.write_string("{ ", !IO),
        % MR_exist_arg_num -- XXX MAKE_FIELD_UNSIGNED
        io.write_int16(int16.cast_from_uint16(SlotInCell), !IO),
        io.write_string(", ", !IO),
        % MR_exist_offset_in_tci -- XXX MAKE_FIELD_UNSIGNED
        io.write_int16(int16.cast_from_uint16(SlotInTci), !IO),
        io.write_string(" }", !IO)
    ).

:- pred output_exist_locns(list(exist_typeinfo_locn)::in,
    io::di, io::uo) is det.

output_exist_locns(Locns, !IO) :-
    io.write_string("\t", !IO),
    io.write_list(Locns, ",\n\t", output_exist_locn, !IO),
    io.write_string("\n", !IO).

:- pred output_maybe_static_code_addr(maybe(code_addr)::in,
    io::di, io::uo) is det.
:- pragma consider_used(output_maybe_static_code_addr/3).

output_maybe_static_code_addr(yes(CodeAddr), !IO) :-
    output_static_code_addr(CodeAddr, !IO).
output_maybe_static_code_addr(no, !IO) :-
    io.write_string("NULL", !IO).

:- pred output_static_code_addr(code_addr::in, io::di, io::uo) is det.

output_static_code_addr(CodeAddr, !IO) :-
    io.write_string("MR_MAYBE_STATIC_CODE(", !IO),
    output_code_addr(CodeAddr, !IO),
    io.write_string(")", !IO).

%-----------------------------------------------------------------------------%

:- pred rtti_id_linkage(rtti_id::in, linkage::out) is det.

rtti_id_linkage(RttiId, Linkage) :-
    IsArray = rtti_id_has_array_type(RttiId),
    (
        IsArray = is_array,
        % ANSI/ISO C doesn't allow forward declarations of static data
        % with incomplete types (in this case array types without an explicit
        % array size), so make the declarations extern.
        Linkage = extern
    ;
        IsArray = not_array,
        Exported = rtti_id_is_exported(RttiId),
        ( Exported = yes, Linkage = extern
        ; Exported = no, Linkage = static
        )
    ).

%-----------------------------------------------------------------------------%

output_rtti_id_storage_type_name(Info, RttiId, BeingDefined, !DeclSet, !IO) :-
    output_rtti_type_decl(RttiId, !DeclSet, !IO),
    rtti_id_linkage(RttiId, Linkage),
    LinkageStr = c_data_linkage_string(Linkage, BeingDefined),
    io.write_string(LinkageStr, !IO),

    Globals = Info ^ lout_globals,
    InclCodeAddr = rtti_id_would_include_code_addr(RttiId),
    io.write_string(c_data_const_string(Globals, InclCodeAddr), !IO),

    rtti_id_c_type(RttiId, CType, IsArray),
    c_util.output_quoted_string_cur_stream(CType, !IO),
    io.write_string(" ", !IO),
    output_rtti_id(RttiId, !IO),
    (
        IsArray = is_array,
        io.write_string("[]", !IO)
    ;
        IsArray = not_array
    ).

    % Each type_info and pseudo_type_info may have a different C type,
    % depending on what kind of type_info or pseudo_type_info it is,
    % and also on its arity. We need to declare that C type here.
    %
:- pred output_rtti_type_decl(rtti_id::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_rtti_type_decl(RttiId, !DeclSet, !IO) :-
    ( if
        RttiId = ctor_rtti_id(_, RttiName),
        rtti_type_ctor_template_arity(RttiName, Arity),
        Arity > max_always_declared_arity_type_ctor
    then
        DeclId = decl_type_info_like_struct(Arity),
        ( if decl_set_is_member(DeclId, !.DeclSet) then
            true
        else
            Template =
"#ifndef MR_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY_%d_GUARD
#define MR_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY_%d_GUARD
MR_DECLARE_ALL_TYPE_INFO_LIKE_STRUCTS_FOR_ARITY(%d);
#endif
",
            io.format(Template, [i(Arity), i(Arity), i(Arity)], !IO),
            decl_set_insert(DeclId, !DeclSet)
        )
    else if
        RttiId = tc_rtti_id(_, TCRttiName),
        rtti_type_class_constraint_template_arity(TCRttiName, Arity),
        Arity > max_always_declared_arity_type_class_constraint
    then
        DeclId = decl_typeclass_constraint_struct(Arity),
        ( if decl_set_is_member(DeclId, !.DeclSet) then
            true
        else
            Template =
"#ifndef MR_TYPECLASS_CONSTRAINT_STRUCT_%d_GUARD
#define MR_TYPECLASS_CONSTRAINT_STRUCT_%d_GUARD
MR_DEFINE_TYPECLASS_CONSTRAINT_STRUCT(MR_TypeClassConstraint_%d, %d);
#endif
",
            io.format(Template, [i(Arity), i(Arity), i(Arity), i(Arity)],
                !IO),
            decl_set_insert(DeclId, !DeclSet)
        )
    else
        true
    ).

:- pred rtti_type_ctor_template_arity(ctor_rtti_name::in, int::out) is semidet.

rtti_type_ctor_template_arity(RttiName, NumArgTypes) :-
    (
        RttiName = type_ctor_type_info(TypeInfo),
        require_complete_switch [TypeInfo]
        (
            ( TypeInfo = plain_type_info(_, ArgTypes)
            ; TypeInfo = var_arity_type_info(_, ArgTypes)
            ),
            list.length(ArgTypes, NumArgTypes)
        ;
            TypeInfo = plain_arity_zero_type_info(_),
            NumArgTypes = 0
        )
    ;
        RttiName = type_ctor_pseudo_type_info(PseudoTypeInfo),
        require_complete_switch [PseudoTypeInfo]
        (
            ( PseudoTypeInfo = plain_pseudo_type_info(_, ArgTypes)
            ; PseudoTypeInfo = var_arity_pseudo_type_info(_, ArgTypes)
            ),
            list.length(ArgTypes, NumArgTypes)
        ;
            PseudoTypeInfo = plain_arity_zero_pseudo_type_info(_),
            NumArgTypes = 0
        ;
            PseudoTypeInfo = type_var(_),
            fail
        )
    ).

:- func max_always_declared_arity_type_ctor = int.

max_always_declared_arity_type_ctor = 20.

:- pred rtti_type_class_constraint_template_arity(tc_rtti_name::in, int::out)
    is semidet.

rtti_type_class_constraint_template_arity(TCRttiName, Arity) :-
    ( TCRttiName = type_class_decl_super(_, Arity)
    ; TCRttiName = type_class_instance_constraint(_, _, Arity)
    ).

:- func max_always_declared_arity_type_class_constraint = int.

max_always_declared_arity_type_class_constraint = 10.

%-----------------------------------------------------------------------------%

output_rtti_id_storage_type_name_no_decl(Info, RttiId, BeingDefined, !IO) :-
    decl_set_init(DeclSet0),
    output_rtti_id_storage_type_name(Info, RttiId, BeingDefined, DeclSet0, _,
        !IO).

%-----------------------------------------------------------------------------%

tabling_struct_data_addr_string(ProcLabel, Id) =
    mercury_var_prefix ++ "_proc" ++ tabling_info_id_str(Id) ++ "__" ++
        proc_label_to_c_string(ProcLabel, no).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.rtti_out.
%-----------------------------------------------------------------------------%
