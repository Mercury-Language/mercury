%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
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
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Output a C declaration for the rtti_datas.
    %
:- pred output_rtti_data_decl_list(llds_out_info::in,
    io.text_output_stream::in, list(rtti_data)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output a C declaration for the rtti_data.
    %
:- pred output_rtti_data_decl(llds_out_info::in,
    io.text_output_stream::in, rtti_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output a C definition for the rtti_data.
    %
:- pred output_rtti_data_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output C code (e.g. a call to the MR_INIT_TYPE_CTOR_INFO() macro)
    % to initialize the rtti_data if necessary.
    %
:- pred init_rtti_data_if_nec(io.text_output_stream::in, rtti_data::in,
    io::di, io::uo) is det.

    % Output C code (e.g. a call to MR_register_type_ctor_info()) to register
    % the rtti_data in the type tables, if it represents a data structure
    % that should be so registered.
    %
:- pred register_rtti_data_if_nec(io.text_output_stream::in, rtti_data::in,
    io::di, io::uo) is det.

    % Output a C expression holding the address of the C name of the specified
    % rtti_data, preceded by the string in the first argument (that string will
    % usually be a C cast).
    %
:- pred output_cast_addr_of_rtti_data(string::in, rtti_data::in,
    io.text_output_stream::in, io::di, io::uo) is det.

    % Output a C expression holding the address of the C name of
    % the specified rtti_data.
    %
:- pred output_addr_of_rtti_data(io.text_output_stream::in,
    rtti_data::in, io::di, io::uo) is det.

    % Output the C name of the rtti_data specified by the given rtti_id.
    %
:- pred output_rtti_id(io.text_output_stream::in, rtti_id::in,
    io::di, io::uo) is det.

    % Output the C storage class, C type, and C name of the rtti_data
    % specified by the given rtti_id for use in a declaration or
    % definition. The bool should be `yes' iff it is for a definition.
    %
:- pred output_rtti_id_storage_type_name(llds_out_info::in,
    io.text_output_stream::in, rtti_id::in, bool::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Output the C storage class, C type, and C name of the rtti_data
    % specified by the given rtti_id for use in a declaration or
    % definition. The bool should be `yes' iff it is for a definition.
    %
:- pred output_rtti_id_storage_type_name_no_decl(llds_out_info::in,
    io.text_output_stream::in, rtti_id::in, bool::in, io::di, io::uo) is det.

:- func tabling_struct_data_addr_string(proc_label, proc_tabling_struct_id)
    = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.type_ctor_info.
:- import_module hlds.
:- import_module hlds.hlds_rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.code_util.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module ll_backend.llds_out.llds_out_file.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
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

output_rtti_data_decl_list(Info, Stream, RttiDatas, !DeclSet, !IO) :-
    classify_rtti_datas_to_decl(RttiDatas, multi_map.init, GroupMap),
    multi_map.to_assoc_list(GroupMap, GroupList),
    list.foldl2(output_rtti_data_decl_group(Info, Stream), GroupList,
        !DeclSet, !IO).

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
    io.text_output_stream::in, pair(data_group, list(rtti_id))::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rtti_data_decl_group(Info, Stream, Group - RttiIds, !DeclSet, !IO) :-
    % ChunkSize should be as large as possible to reduce the size of the
    % file being generated, but small enough not to overload the fixed
    % limits of our target C compilers.
    ChunkSize = 10,
    % The process of creating the multi_map reverses the order of rtti_ids,
    % we now undo this reversal.
    list.chunk(list.reverse(RttiIds), ChunkSize, RttiIdChunks),
    list.foldl2(output_rtti_data_decl_chunk(Info, Stream, Group), RttiIdChunks,
        !DeclSet, !IO).

:- pred output_rtti_data_decl_chunk(llds_out_info::in,
    io.text_output_stream::in, data_group::in, list(rtti_id)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rtti_data_decl_chunk(Info, Stream, Group, RttiIds, !DeclSet, !IO) :-
    (
        % Pick a representative RttiId. All the operations we perform on it
        % below would have the same result regardless of which one we picked.
        RttiIds = [RttiId | _]
    ;
        RttiIds = [],
        unexpected($pred, "empty list")
    ),
    Group = data_group(CType, IsArray, Linkage),

    io.nl(Stream, !IO),
    output_rtti_type_decl(Stream, RttiId, !DeclSet, !IO),
    Globals = Info ^ lout_globals,
    LinkageStr = c_data_linkage_string(Linkage, no),
    InclCodeAddr = rtti_id_would_include_code_addr(RttiId),

    io.write_string(Stream, LinkageStr, !IO),
    io.write_string(Stream, c_data_const_string(Globals, InclCodeAddr), !IO),
    c_util.output_quoted_string(Stream, CType, !IO),
    io.nl(Stream, !IO),

    output_rtti_data_decl_chunk_entries(Stream, IsArray, RttiIds,
        !DeclSet, !IO).

:- pred output_rtti_data_decl_chunk_entries(io.text_output_stream::in,
    is_array::in, list(rtti_id)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rtti_data_decl_chunk_entries(_, _IsArray, [], !DeclSet, !IO) :-
    unexpected($pred, "empty list").
output_rtti_data_decl_chunk_entries(Stream, IsArray, [RttiId | RttiIds],
        !DeclSet, !IO) :-
    decl_set_insert(decl_rtti_id(RttiId), !DeclSet),
    io.write_string(Stream, "\t", !IO),
    output_rtti_id(Stream, RttiId, !IO),
    (
        IsArray = is_array,
        io.write_string(Stream, "[]", !IO)
    ;
        IsArray = not_array
    ),
    (
        RttiIds = [_ | _],
        io.write_string(Stream, ",\n", !IO),
        output_rtti_data_decl_chunk_entries(Stream, IsArray, RttiIds,
            !DeclSet, !IO)
    ;
        RttiIds = [],
        io.write_string(Stream, ";\n", !IO)
    ).

%-----------------------------------------------------------------------------%

output_rtti_data_decl(Info, Stream, RttiData, !DeclSet, !IO) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(_)) then
        % These just get represented as integers, so we don't need to declare
        % them. Also rtti_data_to_id/3 does not handle this case.
        true
    else
        rtti_data_to_id(RttiData, RttiId),
        output_generic_rtti_data_decl(Info, Stream, RttiId, !DeclSet, !IO)
    ).

%-----------------------------------------------------------------------------%

output_rtti_data_defn(Info, Stream, RttiDefn, !DeclSet, !IO) :-
    (
        RttiDefn = rtti_data_type_info(TypeInfo),
        output_type_info_defn(Info, Stream, TypeInfo, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_pseudo_type_info(PseudoTypeInfo),
        output_pseudo_type_info_defn(Info, Stream, PseudoTypeInfo,
            !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_type_ctor_info(TypeCtorData),
        output_type_ctor_data_defn(Info, Stream, TypeCtorData, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_base_typeclass_info(TCName, InstanceModuleName,
            InstanceString, BaseTypeClassInfo),
        output_base_typeclass_info_defn(Info, Stream, TCName,
            InstanceModuleName, InstanceString, BaseTypeClassInfo,
            !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_type_class_decl(TCDecl),
        output_type_class_decl_defn(Info, Stream, TCDecl, !DeclSet, !IO)
    ;
        RttiDefn = rtti_data_type_class_instance(InstanceDecl),
        output_type_class_instance_defn(Info, Stream, InstanceDecl,
            !DeclSet, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred output_base_typeclass_info_defn(llds_out_info::in,
    io.text_output_stream::in, tc_name::in, module_name::in, string::in,
    base_typeclass_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_base_typeclass_info_defn(Info, Stream, TCName, InstanceModuleName,
        InstanceString, BaseTypeClassInfo, !DeclSet, !IO) :-
    BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5, Methods),
    CodeAddrs = list.map(make_code_addr, Methods),
    list.foldl2(output_record_code_addr_decls(Info, Stream), CodeAddrs,
        !DeclSet, !IO),
    io.write_string(Stream, "\n", !IO),
    RttiId = tc_rtti_id(TCName,
        type_class_base_typeclass_info(InstanceModuleName, InstanceString)),
    output_rtti_id_storage_type_name(Info, Stream, RttiId, yes, !DeclSet, !IO),
    % XXX It would be nice to avoid generating redundant declarations
    % of base_typeclass_infos, but currently we don't.
    io.write_string(Stream, " = {\n\t(MR_Code *) ", !IO),
    add_list(add_int, ",\n\t(MR_Code *) ", [N1, N2, N3, N4, N5], Stream, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    write_out_list(output_static_code_addr, ",\n\t", CodeAddrs, Stream, !IO),
    io.write_string(Stream, "\n};\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_type_class_decl_defn(llds_out_info::in,
    io.text_output_stream::in, tc_decl::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_class_decl_defn(Info, Stream, TCDecl, !DeclSet, !IO) :-
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
        output_generic_rtti_data_defn_start(Info, Stream, TCIdVarNamesRttiId,
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n", !IO),
        list.foldl(output_type_class_id_tvar_name(Stream), TVarNames, !IO),
        io.write_string(Stream, "};\n", !IO)
    ),
    (
        MethodIds = []
    ;
        MethodIds = [_ | _],
        output_generic_rtti_data_defn_start(Info, Stream, TCIdMethodIdsRttiId,
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n", !IO),
        list.foldl(output_type_class_id_method_id(Stream), MethodIds, !IO),
        io.write_string(Stream, "};\n", !IO)
    ),
    list.length(TVarNames, NumTVarNames),
    list.length(MethodIds, NumMethodIds),
    output_generic_rtti_data_defn_start(Info, Stream, TCIdRttiId,
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t""", !IO),
    c_util.output_quoted_string(Stream, sym_name_to_string(ModuleSymName),
        !IO),
    io.write_string(Stream, """,\n\t""", !IO),
    c_util.output_quoted_string(Stream, ClassName, !IO),
    io.write_string(Stream, """,\n\t", !IO),
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    io.write_int(Stream, NumTVarNames, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    io.write_int(Stream, NumMethodIds, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    (
        TVarNames = [],
        io.write_string(Stream, "NULL", !IO)
    ;
        TVarNames = [_ | _],
        output_rtti_id(Stream, TCIdVarNamesRttiId, !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    (
        MethodIds = [],
        io.write_string(Stream, "NULL", !IO)
    ;
        MethodIds = [_ | _],
        output_rtti_id(Stream, TCIdMethodIdsRttiId, !IO)
    ),
    io.write_string(Stream, "\n};\n", !IO),
    (
        Supers = []
    ;
        Supers = [_ | _],
        list.map_foldl3(
            output_type_class_constraint(Info, Stream,
                make_tc_decl_super_id(TCName)),
            Supers, SuperIds, counter.init(1), _, !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, Stream, TCDeclSupersRttiId,
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n", !IO),
        output_cast_addr_of_rtti_ids("(MR_TypeClassConstraint) ", SuperIds,
            Stream, !IO),
        io.write_string(Stream, "};\n", !IO)
    ),
    list.length(Supers, NumSupers),
    output_generic_rtti_data_defn_start(Info, Stream, TCDeclRttiId,
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t&", !IO),
    output_rtti_id(Stream, TCIdRttiId, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    io.write_int(Stream, Version, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    io.write_int(Stream, NumSupers, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    (
        Supers = [],
        io.write_string(Stream, "NULL", !IO)
    ;
        Supers = [_ | _],
        output_rtti_id(Stream, TCDeclSupersRttiId, !IO)
    ),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_type_class_id_tvar_name(io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

output_type_class_id_tvar_name(Stream, TVarName, !IO) :-
    io.write_string(Stream, "\t""", !IO),
    c_util.output_quoted_string(Stream, TVarName, !IO),
    io.write_string(Stream, """,\n", !IO).

:- pred output_type_class_id_method_id(io.text_output_stream::in,
    tc_method_id::in, io::di, io::uo) is det.

output_type_class_id_method_id(Stream, MethodId, !IO) :-
    MethodId = tc_method_id(MethodName, MethodArity, PredOrFunc),
    io.write_string(Stream, "\t{ """, !IO),
    c_util.output_quoted_string(Stream, MethodName, !IO),
    io.write_string(Stream, """, ", !IO),
    io.write_int(Stream, MethodArity, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_string(Stream, mr_pred_or_func_to_string(PredOrFunc), !IO),
    io.write_string(Stream, " },\n", !IO).

:- pred make_tc_decl_super_id(tc_name::in, int::in, int::in, rtti_id::out)
    is det.

make_tc_decl_super_id(TCName, Ordinal, NumTypes, RttiId) :-
    RttiId = tc_rtti_id(TCName, type_class_decl_super(Ordinal, NumTypes)).

%-----------------------------------------------------------------------------%

:- pred output_type_class_instance_defn(llds_out_info::in,
    io.text_output_stream::in, tc_instance::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_class_instance_defn(Info, Stream, Instance, !DeclSet, !IO) :-
    Instance = tc_instance(TCName, TCTypes, NumTypeVars, Constraints,
        _MethodProcLabels),
    list.foldl2(output_maybe_pseudo_type_info_defn(Info, Stream), TCTypes,
        !DeclSet, !IO),
    TCTypeRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, TCTypes),
    TCInstanceTypesRttiId = tc_rtti_id(TCName,
        type_class_instance_tc_type_vector(TCTypes)),
    output_generic_rtti_data_defn_start(Info, Stream, TCInstanceTypesRttiId,
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", TCTypeRttiDatas,
        Stream, !IO),
    io.write_string(Stream, "};\n", !IO),
    TCInstanceConstraintsRttiId = tc_rtti_id(TCName,
        type_class_instance_constraints(TCTypes)),
    (
        Constraints = []
    ;
        Constraints = [_ | _],
        list.map_foldl3(
            output_type_class_constraint(Info, Stream,
                make_tc_instance_constraint_id(TCName, TCTypes)),
            Constraints, ConstraintIds, counter.init(1), _, !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, Stream,
            TCInstanceConstraintsRttiId, !DeclSet, !IO),
        io.write_string(Stream, " = {\n", !IO),
        output_cast_addr_of_rtti_ids("(MR_TypeClassConstraint) ",
            ConstraintIds, Stream, !IO),
        io.write_string(Stream, "};\n", !IO)
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
    output_record_rtti_id_decls(Info, Stream, TCDeclRttiId, "", "", 0, _,
        !DeclSet, !IO),
    TCInstanceRttiId = tc_rtti_id(TCName, type_class_instance(TCTypes)),
    output_generic_rtti_data_defn_start(Info, Stream, TCInstanceRttiId,
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t&", !IO),
    output_rtti_id(Stream, TCDeclRttiId, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    io.write_int(Stream, NumTypeVars, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    io.write_int(Stream, list.length(Constraints), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    output_rtti_id(Stream, TCInstanceTypesRttiId, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    (
        Constraints = [],
        io.write_string(Stream, "NULL", !IO)
    ;
        Constraints = [_ | _],
        output_rtti_id(Stream, TCInstanceConstraintsRttiId, !IO)
    ),
%   io.write_string(Stream, ",\n\t", !IO),
%   (
%       MethodProcLabels = [],
%       io.write_string(Stream, "NULL", !IO)
%   ;
%       MethodProcLabels = [_ | _],
%       io.write_string(Stream, "&", !IO),
%       output_rtti_id(Stream, TCInstanceMethodsRttiId, !IO)
%   ),
    io.write_string(Stream, "\n};\n", !IO).

:- pred make_tc_instance_constraint_id(tc_name::in, list(tc_type)::in,
    int::in, int::in, rtti_id::out) is det.

make_tc_instance_constraint_id(TCName, TCTypes, Ordinal, NumTypes, RttiId) :-
    RttiId = tc_rtti_id(TCName,
        type_class_instance_constraint(TCTypes, Ordinal, NumTypes)).

:- pred output_code_addr_in_list(io.text_output_stream::in, code_addr::in,
    io::di, io::uo) is det.
:- pragma consider_used(pred(output_code_addr_in_list/4)).

output_code_addr_in_list(Stream, CodeAddr, !IO) :-
    io.write_string(Stream, "\t", !IO),
    output_static_code_addr(CodeAddr, Stream, !IO),
    io.write_string(Stream, ",\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_type_class_constraint(llds_out_info::in,
    io.text_output_stream::in,
    pred(int, int, rtti_id)::in(pred(in, in, out) is det),
    tc_constraint::in, rtti_id::out, counter::in, counter::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_class_constraint(Info, Stream, MakeRttiId, Constraint,
        TCDeclSuperRttiId, !Counter, !DeclSet, !IO) :-
    Constraint = tc_constraint(TCName, Types),
    list.length(Types, NumTypes),
    counter.allocate(TCNum, !Counter),
    MakeRttiId(TCNum, NumTypes, TCDeclSuperRttiId),
    TCDeclRttiId = tc_rtti_id(TCName, type_class_decl),
    output_generic_rtti_data_decl(Info, Stream, TCDeclRttiId, !DeclSet, !IO),
    list.foldl2(output_maybe_pseudo_type_info_defn(Info, Stream), Types,
        !DeclSet, !IO),
    TypeRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Types),
    output_generic_rtti_data_defn_start(Info, Stream, TCDeclSuperRttiId,
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t&", !IO),
    output_rtti_id(Stream, TCDeclRttiId, !IO),
    io.write_string(Stream, ",\n\t{\n", !IO),
    output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", TypeRttiDatas,
        Stream, !IO),
    io.write_string(Stream, "\t}\n};\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_maybe_pseudo_type_info_or_self_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_maybe_pseudo_type_info_or_self::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_maybe_pseudo_type_info_or_self_defn(Info, Stream, MaybePseudoTypeInfo,
        !DeclSet, !IO) :-
    (
        MaybePseudoTypeInfo = plain(TypeInfo),
        output_type_info_defn(Info, Stream, TypeInfo, !DeclSet, !IO)
    ;
        MaybePseudoTypeInfo = pseudo(PseudoTypeInfo),
        output_pseudo_type_info_defn(Info, Stream, PseudoTypeInfo,
            !DeclSet, !IO)
    ;
        MaybePseudoTypeInfo = self
    ).

:- pred output_maybe_pseudo_type_info_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_maybe_pseudo_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_maybe_pseudo_type_info_defn(Info, Stream, MaybePseudoTypeInfo,
        !DeclSet, !IO) :-
    (
        MaybePseudoTypeInfo = plain(TypeInfo),
        output_type_info_defn(Info, Stream, TypeInfo, !DeclSet, !IO)
    ;
        MaybePseudoTypeInfo = pseudo(PseudoTypeInfo),
        output_pseudo_type_info_defn(Info, Stream, PseudoTypeInfo,
            !DeclSet, !IO)
    ).

:- pred output_type_info_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_info_defn(Info, Stream, TypeInfo, !DeclSet, !IO) :-
    ( if
        rtti_data_to_id(rtti_data_type_info(TypeInfo), RttiId),
        decl_set_is_member(decl_rtti_id(RttiId), !.DeclSet)
    then
        true
    else
        do_output_type_info_defn(Info, Stream, TypeInfo, !DeclSet, !IO)
    ).

:- pred do_output_type_info_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

do_output_type_info_defn(Info, Stream, TypeInfo, !DeclSet, !IO) :-
    (
        TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, Stream, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO)
    ;
        TypeInfo = plain_type_info(RttiTypeCtor, Args),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, Stream, TypeCtorRttiId,
            "", "", 0, _, !DeclSet, !IO),
        ArgRttiDatas = list.map(type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, Stream, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, Stream,
            ctor_rtti_id(RttiTypeCtor, type_ctor_type_info(TypeInfo)),
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n\t&", !IO),
        output_ctor_rtti_id(Stream, RttiTypeCtor,
            type_ctor_type_ctor_info, !IO),
        io.write_string(Stream, ",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_TypeInfo) ", ArgRttiDatas,
            Stream, !IO),
        io.write_string(Stream, "}};\n", !IO)
    ;
        TypeInfo = var_arity_type_info(RttiVarArityId, Args),
        RttiTypeCtor = var_arity_id_to_rtti_type_ctor(RttiVarArityId),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, Stream, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, Stream, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, Stream,
            ctor_rtti_id(RttiTypeCtor, type_ctor_type_info(TypeInfo)),
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n\t&", !IO),
        output_ctor_rtti_id(Stream, RttiTypeCtor,
            type_ctor_type_ctor_info, !IO),
        io.write_string(Stream, ",\n\t", !IO),
        list.length(Args, Arity),
        io.write_int(Stream, Arity, !IO),
        io.write_string(Stream, ",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_TypeInfo) ", ArgRttiDatas,
            Stream, !IO),
        io.write_string(Stream, "}};\n", !IO)
    ).

:- pred output_pseudo_type_info_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_pseudo_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_pseudo_type_info_defn(Info, Stream, PseudoTypeInfo, !DeclSet, !IO) :-
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
        do_output_pseudo_type_info_defn(Info, Stream, PseudoTypeInfo,
            !DeclSet, !IO)
    ).

:- pred do_output_pseudo_type_info_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_pseudo_type_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

do_output_pseudo_type_info_defn(Info, Stream, PseudoTypeInfo, !DeclSet, !IO) :-
    (
        PseudoTypeInfo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, Stream, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO)
    ;
        PseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, Args),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, Stream, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, Stream, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, Stream,
            ctor_rtti_id(RttiTypeCtor,
                type_ctor_pseudo_type_info(PseudoTypeInfo)),
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n\t&", !IO),
        output_ctor_rtti_id(Stream, RttiTypeCtor,
            type_ctor_type_ctor_info, !IO),
        io.write_string(Stream, ",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas,
            Stream, !IO),
        io.write_string(Stream, "}};\n", !IO)
    ;
        PseudoTypeInfo = var_arity_pseudo_type_info(RttiVarArityId, Args),
        RttiTypeCtor = var_arity_id_to_rtti_type_ctor(RttiVarArityId),
        TypeCtorRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        output_record_rtti_id_decls(Info, Stream, TypeCtorRttiId, "", "", 0, _,
            !DeclSet, !IO),
        ArgRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Args),
        output_type_ctor_arg_defns_and_decls(Info, Stream, ArgRttiDatas,
            !DeclSet, !IO),
        output_generic_rtti_data_defn_start(Info, Stream,
            ctor_rtti_id(RttiTypeCtor,
                type_ctor_pseudo_type_info(PseudoTypeInfo)),
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n\t&", !IO),
        output_ctor_rtti_id(Stream, RttiTypeCtor,
            type_ctor_type_ctor_info, !IO),
        io.write_string(Stream, ",\n\t", !IO),
        list.length(Args, Arity),
        io.write_int(Stream, Arity, !IO),
        io.write_string(Stream, ",\n{", !IO),
        output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgRttiDatas,
            Stream, !IO),
        io.write_string(Stream, "}};\n", !IO)
    ;
        PseudoTypeInfo = type_var(_)
    ).

:- pred output_type_ctor_arg_defns_and_decls(llds_out_info::in,
    io.text_output_stream::in, list(rtti_data)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_ctor_arg_defns_and_decls(Info, Stream, ArgRttiDatas,
        !DeclSet, !IO) :-
    % We must output the definitions of the rtti_datas of the argument
    % typeinfos and/or pseudo-typeinfos, because they may contain other
    % typeinfos and/or pseudo-typeinfos nested within them. However,
    % zero arity typeinfos and pseudo-typeinfos have empty definitions,
    % yet the type_ctor_info they refer to still must be declared.
    % This is why both calls below are needed.
    list.foldl2(output_rtti_data_defn(Info, Stream), ArgRttiDatas,
        !DeclSet, !IO),
    output_record_rtti_datas_decls(Info, Stream, ArgRttiDatas, "", "", 0, _,
        !DeclSet, !IO).

%-----------------------------------------------------------------------------%

:- pred output_type_ctor_data_defn(llds_out_info::in,
    io.text_output_stream::in, type_ctor_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_ctor_data_defn(Info, Stream, TypeCtorData, !DeclSet, !IO) :-
    RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
    TypeCtorData = type_ctor_data(Version, Module, TypeName, TypeArity,
        UnifyUniv, CompareUniv, Flags, TypeCtorDetails),
    output_type_ctor_details_defn(Info, Stream, RttiTypeCtor, TypeCtorDetails,
        MaybeFunctorsName, MaybeLayoutName, HaveFunctorNumberMap,
        !DeclSet, !IO),
    det_univ_to_type(UnifyUniv, UnifyProcLabel),
    UnifyCodeAddr = make_code_addr(UnifyProcLabel),
    det_univ_to_type(CompareUniv, CompareProcLabel),
    CompareCodeAddr = make_code_addr(CompareProcLabel),
    CodeAddrs = [UnifyCodeAddr, CompareCodeAddr],
    list.foldl2(output_record_code_addr_decls(Info, Stream), CodeAddrs,
        !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info), !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t", !IO),
    % MR_type_ctor_arity -- XXX MAKE_FIELD_UNSIGNED
    io.write_int(Stream, uint16.to_int(TypeArity), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_version
    io.write_uint8(Stream, Version, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_num_ptags
    MaybeNumPtags = type_ctor_details_num_ptags(TypeCtorDetails),
    (
        MaybeNumPtags = yes(NumPtags),
        NumPtagsEncoding = int8.det_from_int(NumPtags)
    ;
        MaybeNumPtags = no,
        NumPtagsEncoding = -1i8
    ),
    io.write_int8(Stream, NumPtagsEncoding, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_rep_CAST_ME
    rtti.type_ctor_rep_to_string(TypeCtorData, _TargetPrefixes, CtorRepStr),
    io.write_string(Stream, CtorRepStr, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_unify_pred
    output_static_code_addr(UnifyCodeAddr, Stream, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_compare_pred
    output_static_code_addr(CompareCodeAddr, Stream, !IO),
    io.write_string(Stream, ",\n\t""", !IO),
    % MR_type_ctor_module_name
    c_util.output_quoted_string(Stream, sym_name_to_string(Module), !IO),
    io.write_string(Stream, """,\n\t""", !IO),
    % MR_type_ctor_name
    c_util.output_quoted_string(Stream, TypeName, !IO),
    io.write_string(Stream, """,\n\t", !IO),
    % MR_type_ctor_functors
    (
        MaybeFunctorsName = yes(FunctorsName),
        FunctorsRttiId = ctor_rtti_id(RttiTypeCtor, FunctorsName),
        io.write_string(Stream, "{ ", !IO),
        output_cast_addr_of_rtti_id("(void *) ", FunctorsRttiId, Stream, !IO),
        io.write_string(Stream, " }", !IO)
    ;
        MaybeFunctorsName = no,
        io.write_string(Stream, "{ 0 }", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_layout
    (
        MaybeLayoutName = yes(LayoutName),
        LayoutRttiId = ctor_rtti_id(RttiTypeCtor, LayoutName),
        io.write_string(Stream, "{ ", !IO),
        output_cast_addr_of_rtti_id("(void *) ", LayoutRttiId, Stream, !IO),
        io.write_string(Stream, " }", !IO)
    ;
        MaybeLayoutName = no,
        io.write_string(Stream, "{ 0 }", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_num_functors -- XXX MAKE_FIELD_UNSIGNED
    MaybeNumFunctors = type_ctor_details_num_functors(TypeCtorDetails),
    (
        MaybeNumFunctors = yes(NumFunctors),
        NumFunctorsEncoding = int32.det_from_int(NumFunctors)
    ;
        MaybeNumFunctors = no,
        NumFunctorsEncoding = -1i32
    ),
    io.write_int32(Stream, NumFunctorsEncoding, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_flags
    io.write_uint16(Stream, encode_type_ctor_flags(Flags), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_type_ctor_functor_number_map
    (
        HaveFunctorNumberMap = yes,
        FunctorNumberMapRttiId =
            ctor_rtti_id(RttiTypeCtor, type_ctor_functor_number_map),
        output_rtti_id(Stream, FunctorNumberMapRttiId, !IO)
    ;
        HaveFunctorNumberMap = no,
        io.write_string(Stream, "NULL", !IO)
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
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_type_ctor_details_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in, type_ctor_details::in,
    maybe(ctor_rtti_name)::out, maybe(ctor_rtti_name)::out, bool::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_type_ctor_details_defn(Info, Stream, RttiTypeCtor, TypeCtorDetails,
        MaybeFunctorsName, MaybeLayoutName, HaveFunctorNumberMap,
        !DeclSet, !IO) :-
    (
        TypeCtorDetails = tcd_enum(_, _IsDummy, EnumFunctors,
            EnumByOrd, EnumByName, FunctorNumberMap, _MaybeBaseTypeCtor),
        % MaybeBaseTypeCtor is not required for low-level data.
        list.foldl2(output_enum_functor_defn(Info, Stream, RttiTypeCtor),
            EnumFunctors, !DeclSet, !IO),
        output_enum_ordinal_ordered_table(Info, Stream, RttiTypeCtor,
            EnumByOrd, !DeclSet, !IO),
        output_enum_name_ordered_table(Info, Stream, RttiTypeCtor, EnumByName,
            !DeclSet, !IO),
        output_functor_number_map(Info, Stream, RttiTypeCtor, FunctorNumberMap,
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_enum_ordinal_ordered_table),
        MaybeFunctorsName = yes(type_ctor_enum_name_ordered_table),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_foreign_enum(Lang, _, ForeignEnumFunctors,
            ForeignEnumByOrdinal, ForeignEnumByName, FunctorNumberMap),
        expect(unify(Lang, lang_c), $pred,
            "language other than C for foreign enumeration"),
        list.foldl2(
            output_foreign_enum_functor_defn(Info, Stream, RttiTypeCtor),
            ForeignEnumFunctors, !DeclSet, !IO),
        output_foreign_enum_ordinal_ordered_table(Info, Stream, RttiTypeCtor,
            ForeignEnumByOrdinal, !DeclSet, !IO),
        output_foreign_enum_name_ordered_table(Info, Stream, RttiTypeCtor,
            ForeignEnumByName, !DeclSet, !IO),
        output_functor_number_map(Info, Stream, RttiTypeCtor, FunctorNumberMap,
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_foreign_enum_ordinal_ordered_table),
        MaybeFunctorsName = yes(type_ctor_foreign_enum_name_ordered_table),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_du(_, DuFunctors, DuByRep, DuByName,
            FunctorNumberMap, _MaybeBaseTypeCtor),
        % MaybeBaseTypeCtor is not required for low-level data.
        list.foldl2(output_du_functor_defn(Info, Stream, RttiTypeCtor),
            DuFunctors, !DeclSet, !IO),
        output_du_ptag_ordered_table(Info, Stream, RttiTypeCtor, DuByRep,
            !DeclSet, !IO),
        output_du_name_ordered_table(Info, Stream, RttiTypeCtor, DuByName,
            !DeclSet, !IO),
        output_functor_number_map(Info, Stream, RttiTypeCtor, FunctorNumberMap,
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_du_ptag_ordered_table),
        MaybeFunctorsName = yes(type_ctor_du_name_ordered_table),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_notag(_, NotagFunctor, _MaybeBaseTypeCtor),
        % MaybeBaseTypeCtor is not required for low-level data.
        output_notag_functor_defn(Info, Stream, RttiTypeCtor, NotagFunctor,
            !DeclSet, !IO),
        output_functor_number_map(Info, Stream, RttiTypeCtor, [0u32],
            !DeclSet, !IO),
        MaybeLayoutName = yes(type_ctor_notag_functor_desc),
        MaybeFunctorsName = yes(type_ctor_notag_functor_desc),
        HaveFunctorNumberMap = yes
    ;
        TypeCtorDetails = tcd_eqv(EqvType),
        output_maybe_pseudo_type_info_defn(Info, Stream, EqvType,
            !DeclSet, !IO),
        TypeData = maybe_pseudo_type_info_to_rtti_data(EqvType),
        output_record_rtti_data_decls(Info, Stream, TypeData, "", "", 0, _,
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

:- pred output_enum_functor_defn(llds_out_info::in, io.text_output_stream::in,
    rtti_type_ctor::in, enum_functor::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_enum_functor_defn(Info, Stream, RttiTypeCtor, EnumFunctor,
        !DeclSet, !IO) :-
    EnumFunctor = enum_functor(FunctorName, Ordinal, enum_value(Value)),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_enum_functor_desc(Ordinal)),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t""", !IO),
    % MR_enum_functor_name
    c_util.output_quoted_string(Stream, FunctorName, !IO),
    io.write_string(Stream, """,\n\t", !IO),
    % MR_enum_functor_value -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(Stream, int32.cast_from_uint32(Value), !IO),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_foreign_enum_functor_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in, foreign_enum_functor::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_foreign_enum_functor_defn(Info, Stream, RttiTypeCtor,
        ForeignEnumFunctor, !DeclSet, !IO) :-
    ForeignEnumFunctor = foreign_enum_functor(FunctorName, FunctorOrdinal,
        FunctorValue),
    RttiId = ctor_rtti_id(RttiTypeCtor,
        type_ctor_foreign_enum_functor_desc(FunctorOrdinal)),
    output_generic_rtti_data_defn_start(Info, Stream, RttiId, !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t""", !IO),
    % MR_foreign_enum_functor_name
    c_util.output_quoted_string(Stream, FunctorName, !IO),
    io.write_string(Stream, """,\n\t", !IO),
    % MR_foreign_enum_functor_ordinal -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(Stream, int32.cast_from_uint32(FunctorOrdinal), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_foreign_enum_functor_value
    io.write_string(Stream, FunctorValue, !IO),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_notag_functor_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in, notag_functor::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_notag_functor_defn(Info, Stream, RttiTypeCtor, NotagFunctor,
        !DeclSet, !IO) :-
    NotagFunctor = notag_functor(FunctorName, ArgType, MaybeArgName,
        FunctorSubtypeInfo),
    output_maybe_pseudo_type_info_defn(Info, Stream, ArgType, !DeclSet, !IO),
    ArgTypeData = maybe_pseudo_type_info_to_rtti_data(ArgType),
    output_record_rtti_data_decls(Info, Stream, ArgTypeData, "", "", 0, _,
        !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_notag_functor_desc),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t""", !IO),
    % MR_notag_functor_name
    c_util.output_quoted_string(Stream, FunctorName, !IO),
    io.write_string(Stream, """,\n\t", !IO),
    % MR_notag_functor_arg_type
    (
        ArgType = plain(ArgTypeInfo),
        output_cast_addr_of_rtti_data("(MR_PseudoTypeInfo) ",
            rtti_data_type_info(ArgTypeInfo), Stream, !IO)
    ;
        ArgType = pseudo(ArgPseudoTypeInfo),
        % We need to cast the argument to MR_PseudoTypeInfo in case
        % it turns out to be a small integer, not a pointer.
        output_cast_addr_of_rtti_data("(MR_PseudoTypeInfo) ",
            rtti_data_pseudo_type_info(ArgPseudoTypeInfo), Stream, !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_notag_functor_arg_name
    (
        MaybeArgName = yes(ArgName),
        io.write_string(Stream, """", !IO),
        io.write_string(Stream, ArgName, !IO),
        io.write_string(Stream, """", !IO)
    ;
        MaybeArgName = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_notag_functor_subtype
    functor_subtype_info_to_string(FunctorSubtypeInfo, _,
        FunctorSubtypeInfoStr),
    io.write_string(Stream, FunctorSubtypeInfoStr, !IO),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_du_functor_defn(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in, du_functor::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_functor_defn(Info, Stream, RttiTypeCtor, DuFunctor, !DeclSet, !IO) :-
    DuFunctor = du_functor(FunctorName, OrigArity, Ordinal, Rep,
        ArgInfos, MaybeExistInfo, FunctorSubtypeInfo),
    ArgTypes = list.map(du_arg_info_type, ArgInfos),
    MaybeArgNames = list.map(du_arg_info_name, ArgInfos),
    HaveArgNames = (if list.member(yes(_), MaybeArgNames) then yes else no),
    (
        ArgInfos = [_ | _],
        output_du_arg_types(Info, Stream, RttiTypeCtor, Ordinal, ArgTypes,
            !DeclSet, !IO)
    ;
        ArgInfos = []
    ),
    (
        HaveArgNames = yes,
        output_du_arg_names(Info, Stream, RttiTypeCtor, Ordinal, MaybeArgNames,
            !DeclSet, !IO)
    ;
        HaveArgNames = no
    ),
    output_du_arg_locns(Info, Stream, RttiTypeCtor, Ordinal, ArgInfos,
        HaveArgLocns, !DeclSet, !IO),
    (
        MaybeExistInfo = yes(ExistInfo),
        output_exist_info(Info, Stream, RttiTypeCtor, Ordinal, ExistInfo,
            !DeclSet, !IO)
    ;
        MaybeExistInfo = no
    ),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_functor_desc(Ordinal)),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t""", !IO),
    % MR_du_functor_name
    c_util.output_quoted_string(Stream, FunctorName, !IO),
    io.write_string(Stream, """,\n\t", !IO),
    % MR_du_functor_orig_arity -- XXX MAKE_FIELD_UNSIGNED
    io.write_int16(Stream, int16.cast_from_uint16(OrigArity), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_arg_type_contains_var
    ContainsVarBitVector = compute_contains_var_bit_vector(ArgTypes),
    io.write_uint16(Stream, ContainsVarBitVector, !IO),
    io.write_string(Stream, ",\n\t", !IO),
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
    io.write_string(Stream, Locn, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_primary
    io.write_uint8(Stream, PtagUint8, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_secondary -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(Stream, StagEncoding, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_ordinal -- XXX MAKE_FIELD_UNSIGNED
    io.write_int32(Stream, int32.cast_from_uint32(Ordinal), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_arg_types
    io.write_string(Stream, "(MR_PseudoTypeInfo *) ", !IO), % cast away const
    (
        ArgInfos = [_ | _],
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_field_types(Ordinal), Stream, !IO)
    ;
        ArgInfos = [],
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_arg_names
    (
        HaveArgNames = yes,
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_field_names(Ordinal), Stream, !IO)
    ;
        HaveArgNames = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_arg_locns
    (
        HaveArgLocns = yes,
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_field_locns(Ordinal), Stream, !IO)
    ;
        HaveArgLocns = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_exist_info
    (
        MaybeExistInfo = yes(_),
        output_addr_of_ctor_rtti_id(RttiTypeCtor,
            type_ctor_exist_info(Ordinal), Stream, !IO)
    ;
        MaybeExistInfo = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_subtype
    functor_subtype_info_to_string(FunctorSubtypeInfo, _,
        FunctorSubtypeInfoStr),
    io.write_string(Stream, FunctorSubtypeInfoStr, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_functor_num_sectag_bits
    io.write_uint8(Stream, NumSectagBits, !IO),
    io.write_string(Stream, "\n};\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_exist_locns_array(llds_out_info::in, io.text_output_stream::in,
    rtti_type_ctor::in, uint32::in, list(exist_typeinfo_locn)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_exist_locns_array(Info, Stream, RttiTypeCtor, Ordinal, Locns,
        !DeclSet, !IO) :-
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_exist_locns(Ordinal)),
        !DeclSet, !IO),
    (
        % ANSI/ISO C doesn't allow empty arrays, so
        % place a dummy value in the array if necessary.
        Locns = [],
        io.write_string(Stream, "= { {0, 0} };\n", !IO)
    ;
        Locns = [_ | _],
        io.write_string(Stream, " = {\n", !IO),
        output_exist_locns(Locns, Stream, !IO),
        io.write_string(Stream, "};\n", !IO)
    ).

:- pred make_exist_tc_constr_id(rtti_type_ctor::in, uint32::in,
    int::in, int::in, rtti_id::out) is det.

make_exist_tc_constr_id(RttiTypeCtor, Ordinal, TCNum, Arity, RttiId) :-
    RttiName = type_ctor_exist_tc_constr(Ordinal, TCNum, Arity),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName).

:- pred output_exist_constraints_data(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in, uint32::in,
    list(tc_constraint)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_exist_constraints_data(Info, Stream, RttiTypeCtor, Ordinal, Constraints,
        !DeclSet, !IO) :-
    list.map_foldl3(output_type_class_constraint(Info, Stream,
        make_exist_tc_constr_id(RttiTypeCtor, Ordinal)),
        Constraints, ConstraintIds, counter.init(1), _, !DeclSet, !IO),
    RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_exist_tc_constrs(Ordinal)),
    output_generic_rtti_data_defn_start(Info, Stream, RttiId, !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t", !IO),
    output_cast_addr_of_rtti_ids("(MR_TypeClassConstraint) ",
        ConstraintIds, Stream, !IO),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_exist_info(llds_out_info::in, io.text_output_stream::in,
    rtti_type_ctor::in, uint32::in, exist_info::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_exist_info(Info, Stream, RttiTypeCtor, Ordinal, ExistInfo,
        !DeclSet, !IO) :-
    ExistInfo = exist_info(Plain, InTci, Constraints, Locns),
    output_exist_locns_array(Info, Stream, RttiTypeCtor, Ordinal, Locns,
        !DeclSet, !IO),
    (
        Constraints = [_ | _],
        output_exist_constraints_data(Info, Stream, RttiTypeCtor,
            Ordinal, Constraints, !DeclSet, !IO)
    ;
        Constraints = []
    ),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_exist_info(Ordinal)),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t", !IO),
    % MR_exist_typeinfos_plain -- XXX MAKE_FIELD_UNSIGNED
    io.write_int16(Stream, int16.cast_from_uint16(Plain), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_exist_typeinfos_in_tci -- XXX MAKE_FIELD_UNSIGNED
    io.write_int16(Stream, int16.cast_from_uint16(InTci), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_exist_tcis
    list.length(Constraints, Tci),
    io.write_int16(Stream, int16.det_from_int(Tci), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_exist_typeinfo_locns
    output_ctor_rtti_id(Stream, RttiTypeCtor,
        type_ctor_exist_locns(Ordinal), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_exist_constraints
    (
        Constraints = [_ | _],
        output_ctor_rtti_id(Stream, RttiTypeCtor,
            type_ctor_exist_tc_constrs(Ordinal), !IO)
    ;
        Constraints = [],
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_du_arg_types(llds_out_info::in, io.text_output_stream::in,
    rtti_type_ctor::in, uint32::in,
    list(rtti_maybe_pseudo_type_info_or_self)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_arg_types(Info, Stream, RttiTypeCtor, Ordinal, ArgTypes,
        !DeclSet, !IO) :-
    list.foldl2(output_maybe_pseudo_type_info_or_self_defn(Info, Stream),
        ArgTypes, !DeclSet, !IO),
    ArgTypeDatas = list.map(maybe_pseudo_type_info_or_self_to_rtti_data,
        ArgTypes),
    output_record_rtti_datas_decls(Info, Stream, ArgTypeDatas, "", "", 0, _,
        !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_field_types(Ordinal)),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    expect(list.is_not_empty(ArgTypes), $pred, "empty list"),
    output_cast_addr_of_rtti_datas("(MR_PseudoTypeInfo) ", ArgTypeDatas,
        Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_du_arg_names(llds_out_info::in, io.text_output_stream::in,
    rtti_type_ctor::in, uint32::in, list(maybe(string))::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_arg_names(Info, Stream, RttiTypeCtor, Ordinal, MaybeNames,
        !DeclSet, !IO) :-
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_field_names(Ordinal)),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    expect(list.is_not_empty(MaybeNames), $pred, "empty list"),
    output_maybe_quoted_strings(MaybeNames, Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_du_arg_locns(llds_out_info::in, io.text_output_stream::in,
    rtti_type_ctor::in, uint32::in, list(du_arg_info)::in, bool::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_arg_locns(Info, Stream, RttiTypeCtor, Ordinal, ArgInfos, HaveArgLocns,
        !DeclSet, !IO) :-
    ( if
        some [ArgInfo] (
            list.member(ArgInfo, ArgInfos),
            ArgInfo = du_arg_info(_, _, Width),
            Width \= apw_full(_, _)
        )
    then
        output_generic_rtti_data_defn_start(Info, Stream,
            ctor_rtti_id(RttiTypeCtor, type_ctor_field_locns(Ordinal)),
            !DeclSet, !IO),
        io.write_string(Stream, " = {\n", !IO),
        output_du_arg_locns_loop(Stream, ArgInfos, !IO),
        io.write_string(Stream, "};\n", !IO),
        HaveArgLocns = yes
    else
        HaveArgLocns = no
    ).

:- pred output_du_arg_locns_loop(io.text_output_stream::in,
    list(du_arg_info)::in, io::di, io::uo) is det.

output_du_arg_locns_loop(_, [], !IO).
output_du_arg_locns_loop(Stream, [ArgInfo | ArgInfos], !IO) :-
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
    io.format(Stream, "\t{ %d, %d, %d },\n",
        [i(ArgOnlyOffset), i(Shift), i(NumBits)], !IO),
    output_du_arg_locns_loop(Stream, ArgInfos, !IO).

%-----------------------------------------------------------------------------%

:- pred output_enum_ordinal_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    map(uint32, enum_functor)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_enum_ordinal_ordered_table(Info, Stream, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(enum_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_enum_ordinal_ordered_table),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames,
        Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_enum_name_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    map(string, enum_functor)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_enum_name_ordered_table(Info, Stream, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(enum_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_enum_name_ordered_table),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames, Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_foreign_enum_ordinal_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    map(uint32, foreign_enum_functor)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_foreign_enum_ordinal_ordered_table(Info, Stream, RttiTypeCtor,
        FunctorMap, !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(foreign_enum_functor_rtti_name, Functors),
    RttiId = ctor_rtti_id(RttiTypeCtor,
        type_ctor_foreign_enum_ordinal_ordered_table),
    output_generic_rtti_data_defn_start(Info, Stream, RttiId, !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames,
        Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_foreign_enum_name_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    map(string, foreign_enum_functor)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_foreign_enum_name_ordered_table(Info, Stream, RttiTypeCtor, FunctorMap,
        !DeclSet, !IO) :-
    Functors = map.values(FunctorMap),
    FunctorRttiNames = list.map(foreign_enum_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_foreign_enum_name_ordered_table),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames,
        Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_du_name_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    map(string, map(uint16, du_functor))::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_name_ordered_table(Info, Stream, RttiTypeCtor, NameArityMap,
        !DeclSet, !IO) :-
    map.values(NameArityMap, ArityMaps),
    list.map(map.values, ArityMaps, FunctorLists),
    list.condense(FunctorLists, Functors),
    FunctorRttiNames = list.map(du_functor_rtti_name, Functors),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_name_ordered_table),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorRttiNames,
        Stream, !IO),
    io.write_string(Stream, "};\n", !IO).

:- pred output_du_stag_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    pair(ptag, sectag_table)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_stag_ordered_table(Info, Stream, RttiTypeCtor, Ptag - SectagTable,
        !DeclSet, !IO) :-
    SectagTable = sectag_table(_SectagLocn, _NumSectagBits, _NumSharers,
        SectagMap),
    map.values(SectagMap, SectagFunctors),
    FunctorNames = list.map(du_functor_rtti_name, SectagFunctors),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_stag_ordered_table(Ptag)),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    output_addr_of_ctor_rtti_names(RttiTypeCtor, FunctorNames, Stream, !IO),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_du_ptag_ordered_table(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in, map(ptag, sectag_table)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_du_ptag_ordered_table(Info, Stream, RttiTypeCtor, PtagMap,
        !DeclSet, !IO) :-
    map.to_assoc_list(PtagMap, PtagList),
    list.foldl2(output_du_stag_ordered_table(Info, Stream, RttiTypeCtor),
        PtagList, !DeclSet, !IO),
    output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_du_ptag_ordered_table),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n", !IO),
    (
        PtagList = [FirstPtag - _ | _],
        FirstPtag = ptag(LeastPtag)
    ;
        PtagList = [],
        unexpected($pred, "bad ptag list")
    ),
    output_du_ptag_ordered_table_body(Stream, RttiTypeCtor, PtagList,
        LeastPtag, !IO),
    io.write_string(Stream, "\n};\n", !IO).

:- pred output_du_ptag_ordered_table_body(io.text_output_stream::in,
    rtti_type_ctor::in, assoc_list(ptag, sectag_table)::in, uint8::in,
    io::di, io::uo) is det.

output_du_ptag_ordered_table_body(_, _, [], _, !IO).
output_du_ptag_ordered_table_body(Stream, RttiTypeCtor,
        [Ptag - SectagTable | PtagTail], LeastPtag, !IO) :-
    Ptag = ptag(PtagUint8),
    % ptags for a subtype may start higher than zero, and may skip values.
    expect(LeastPtag =< PtagUint8, $pred, "ptag mismatch"),
    SectagTable = sectag_table(SectagLocn, NumSectagBits, NumSharers,
        _SectagMap),
    compute_du_ptag_layout_flags(SectagTable, Flags),
    io.write_string(Stream, "\t{ ", !IO),
    % MR_sectag_sharers
    io.write_uint32(Stream, NumSharers, !IO),
    io.write_string(Stream, ", ", !IO),
    % MR_sectag_locn
    rtti.sectag_locn_to_string(SectagLocn, _TargetPrefixes, LocnStr),
    io.write_string(Stream, LocnStr, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_sectag_alternatives
    output_ctor_rtti_id(Stream, RttiTypeCtor,
        type_ctor_du_stag_ordered_table(Ptag), !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_sectag_numbits
    io.write_int8(Stream, NumSectagBits, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_ptag
    io.write_uint8(Stream, PtagUint8, !IO),
    io.write_string(Stream, ",\n\t", !IO),
    % MR_du_ptag_flags
    io.write_uint8(Stream, encode_du_ptag_layout_flags(Flags), !IO),
    (
        PtagTail = [],
        io.write_string(Stream, " }\n", !IO)
    ;
        PtagTail = [_ | _],
        io.write_string(Stream, " },\n", !IO),
        NextLeastPtag = PtagUint8 + 1u8,
        output_du_ptag_ordered_table_body(Stream, RttiTypeCtor, PtagTail,
            NextLeastPtag, !IO)
    ).

%-----------------------------------------------------------------------------%

:- func make_code_addr(rtti_proc_label) = code_addr.

make_code_addr(ProcLabel) =
    make_entry_label_from_rtti(ProcLabel, for_from_everywhere).

%-----------------------------------------------------------------------------%

:- pred output_functor_number_map(llds_out_info::in,
    io.text_output_stream::in, rtti_type_ctor::in,
    list(uint32)::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_functor_number_map(Info, Stream, RttiTypeCtor, FunctorNumberMap,
        !DeclSet, !IO) :-
   output_generic_rtti_data_defn_start(Info, Stream,
        ctor_rtti_id(RttiTypeCtor, type_ctor_functor_number_map),
        !DeclSet, !IO),
    io.write_string(Stream, " = {\n\t", !IO),
    write_out_list(output_functor_number_map_value, ",\n\t",
        FunctorNumberMap, Stream, !IO),
    io.write_string(Stream, "\n};\n\t", !IO).

:- pred output_functor_number_map_value(uint32::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_functor_number_map_value(NumUint32, Stream, !IO) :-
    % XXX MAKE_FIELD_UNSIGNED
    Num = uint32.cast_to_int(NumUint32),
    io.write_int(Stream, Num, !IO).

%-----------------------------------------------------------------------------%

:- pred output_generic_rtti_data_decl(llds_out_info::in,
    io.text_output_stream::in, rtti_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_generic_rtti_data_decl(Info, Stream, RttiId, !DeclSet, !IO) :-
    output_rtti_id_storage_type_name(Info, Stream, RttiId, no, !DeclSet, !IO),
    io.write_string(Stream, ";\n", !IO),
    decl_set_insert(decl_rtti_id(RttiId), !DeclSet).

:- pred output_generic_rtti_data_defn_start(llds_out_info::in,
    io.text_output_stream::in, rtti_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_generic_rtti_data_defn_start(Info, Stream, RttiId, !DeclSet, !IO) :-
    io.write_string(Stream, "\n", !IO),
    output_rtti_id_storage_type_name(Info, Stream, RttiId, yes, !DeclSet, !IO),
    decl_set_insert(decl_rtti_id(RttiId), !DeclSet).

%-----------------------------------------------------------------------------%

init_rtti_data_if_nec(Stream, Data, !IO) :-
    (
        Data = rtti_data_type_ctor_info(TypeCtorData),
        RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
        io.write_string(Stream, "\tMR_INIT_TYPE_CTOR_INFO(\n\t\t", !IO),
        output_ctor_rtti_id(Stream, RttiTypeCtor,
            type_ctor_type_ctor_info, !IO),
        io.write_string(Stream, ",\n\t\t", !IO),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity),
        ModuleNameString = sym_name_mangle(ModuleName),
        string.append(ModuleNameString, "__", UnderscoresModule),
        ( if string.append(UnderscoresModule, _, TypeName) then
            true
        else
            io.write_string(Stream, UnderscoresModule, !IO)
        ),
        MangledTypeName = name_mangle(TypeName),
        io.write_string(Stream, MangledTypeName, !IO),
        io.write_string(Stream, "_", !IO),
        io.write_uint16(Stream, Arity, !IO),
        io.write_string(Stream, "_0);\n", !IO)
    ;
        Data = rtti_data_base_typeclass_info(TCName, _ModuleName, ClassArity,
            base_typeclass_info(_N1, _N2, _N3, _N4, _N5, Methods)),
        io.write_string(Stream, "#ifndef MR_STATIC_CODE_ADDRESSES\n", !IO),
        % The field number for the first method is 5, since the methods are
        % stored after N1 .. N5, and fields are numbered from 0.
        FirstFieldNum = 5,
        CodeAddrs = list.map(make_code_addr, Methods),
        output_init_method_pointers(Stream, FirstFieldNum, CodeAddrs,
            TCName, ClassArity, !IO),
        io.write_string(Stream, "#endif /* MR_STATIC_CODE_ADDRESSES */\n", !IO)
    ;
        Data = rtti_data_type_class_instance(_),
        io.write_string(Stream, "#ifndef MR_STATIC_CODE_ADDRESSES\n", !IO),
        io.write_string(Stream, "#error ""type_class_instance " ++
            "not yet supported without static code addresses""\n", !IO),
        io.write_string(Stream, "#endif /* MR_STATIC_CODE_ADDRESSES */\n", !IO)
    ;
        ( Data = rtti_data_type_info(_)
        ; Data = rtti_data_pseudo_type_info(_)
        ; Data = rtti_data_type_class_decl(_)
        )
    ).

register_rtti_data_if_nec(Stream, Data, !IO) :-
    (
        Data = rtti_data_type_ctor_info(TypeCtorData),
        RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        io.write_string(Stream, "\t{\n\t", !IO),
        io.write_string(Stream, "\tMR_register_type_ctor_info(\n\t\t&", !IO),
        output_rtti_id(Stream, RttiId, !IO),
        io.write_string(Stream, ");\n\t}\n", !IO)
    ;
        Data = rtti_data_type_class_decl(TCDecl),
        TCDecl = tc_decl(TCId, _, _),
        TCId = tc_id(TCName, _, _),
        RttiId = tc_rtti_id(TCName, type_class_decl),
        io.write_string(Stream, "\t{\n\t", !IO),
        io.write_string(Stream, "\tMR_register_type_class_decl(\n\t\t&", !IO),
        output_rtti_id(Stream, RttiId, !IO),
        io.write_string(Stream, ");\n\t}\n", !IO)
    ;
        Data = rtti_data_type_class_instance(TCInstance),
        TCInstance = tc_instance(TCName, TCTypes, _, _, _),
        RttiId = tc_rtti_id(TCName, type_class_instance(TCTypes)),
        io.write_string(Stream, "\t{\n\t", !IO),
        io.write_string(Stream, "\tMR_register_type_class_instance(\n\t\t&",
            !IO),
        output_rtti_id(Stream, RttiId, !IO),
        io.write_string(Stream, ");\n\t}\n", !IO)
    ;
        ( Data = rtti_data_type_info(_)
        ; Data = rtti_data_pseudo_type_info(_)
        ; Data = rtti_data_base_typeclass_info(_, _, _, _)
        )
    ).

:- pred output_init_method_pointers(io.text_output_stream::in, int::in,
    list(code_addr)::in, tc_name::in, string::in, io::di, io::uo) is det.

output_init_method_pointers(_, _, [], _, _, !IO).
output_init_method_pointers(Stream, FieldNum, [Arg | Args], TCName,
        InstanceStr, !IO) :-
    PrefixNameStr =
        make_base_typeclass_info_name_with_data_prefix(TCName, InstanceStr),
    io.format(Stream, "\t\tMR_field(MR_mktag(0), %s , %d) =\n\t\t\t",
        [s(PrefixNameStr), i(FieldNum)], !IO),
    output_code_addr(Stream, Arg, !IO),
    io.write_string(Stream, ";\n", !IO),
    output_init_method_pointers(Stream, FieldNum + 1, Args, TCName,
        InstanceStr, !IO).

%-----------------------------------------------------------------------------%

:- pred output_record_rtti_datas_decls(llds_out_info::in,
    io.text_output_stream::in, list(rtti_data)::in, string::in, string::in,
    int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rtti_datas_decls(_, _, [], _, _, !N, !DeclSet, !IO).
output_record_rtti_datas_decls(Info, Stream, [RttiData | RttiDatas],
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    output_record_rtti_data_decls(Info, Stream, RttiData,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO),
    output_record_rtti_datas_decls(Info, Stream, RttiDatas,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

:- pred output_record_rtti_data_decls(llds_out_info::in,
    io.text_output_stream::in, rtti_data::in, string::in, string::in,
    int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rtti_data_decls(Info, Stream, RttiData, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(_)) then
        % These just get represented as integers, so we don't need to declare
        % them. Also rtti_data_to_id/3 does not handle this case.
        true
    else
        rtti_data_to_id(RttiData, RttiId),
        output_record_rtti_id_decls(Info, Stream, RttiId,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

:- pred output_record_rtti_id_decls(llds_out_info::in,
    io.text_output_stream::in, rtti_id::in, string::in, string::in,
    int::in, int::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rtti_id_decls(Info, Stream, RttiId, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    output_record_data_id_decls_format(Info, Stream, rtti_data_id(RttiId),
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

:- pred output_cast_addr_of_rtti_ids(string::in, list(rtti_id)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_cast_addr_of_rtti_ids(_, [], Stream, !IO) :-
    io.write_string(Stream,
        "\t/* Dummy entry, since ISO C forbids zero-sized arrays */\n", !IO),
    io.write_string(Stream, "\t0\n", !IO).
output_cast_addr_of_rtti_ids(Cast, [TCRttiName | TCRttiNames], Stream, !IO) :-
    io.write_string(Stream, "\t", !IO),
    write_out_list(output_cast_addr_of_rtti_id(Cast),
        ",\n\t", [TCRttiName | TCRttiNames], Stream, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred output_addr_of_ctor_rtti_names(rtti_type_ctor::in,
    list(ctor_rtti_name)::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_addr_of_ctor_rtti_names(_, [], _, !IO).
output_addr_of_ctor_rtti_names(RttiTypeCtor, [RttiName | RttiNames],
        Stream, !IO) :-
    io.write_string(Stream, "\t", !IO),
    write_out_list(output_addr_of_ctor_rtti_id(RttiTypeCtor),
        ",\n\t", [RttiName | RttiNames], Stream, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred output_cast_addr_of_rtti_datas(string::in, list(rtti_data)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_cast_addr_of_rtti_datas(_, [], Stream, !IO) :-
    io.write_string(Stream,
        "\t/* Dummy entry, since ISO C forbids zero-sized arrays */\n", !IO),
    io.write_string(Stream, "\t0\n", !IO).
output_cast_addr_of_rtti_datas(Cast, [RttiData | RttiDatas], Stream, !IO) :-
    io.write_string(Stream, "\t", !IO),
    write_out_list(output_cast_addr_of_rtti_data(Cast),
        ",\n\t", [RttiData | RttiDatas], Stream, !IO),
    io.write_string(Stream, "\n", !IO).

output_cast_addr_of_rtti_data(Cast, RttiData, Stream, !IO) :-
    io.write_string(Stream, Cast, !IO),
    output_addr_of_rtti_data(Stream, RttiData, !IO).

output_addr_of_rtti_data(Stream, RttiData, !IO) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(VarNum)) then
        % rtti_data_to_id/3 does not handle this case
        io.write_int(Stream, VarNum, !IO)
    else
        rtti_data_to_id(RttiData, RttiId),
        output_addr_of_rtti_id(Stream, RttiId, !IO)
    ).

:- pred output_cast_addr_of_rtti_id(string::in, rtti_id::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_cast_addr_of_rtti_id(Cast, RttiId, Stream, !IO) :-
    io.write_string(Stream, Cast, !IO),
    output_addr_of_rtti_id(Stream, RttiId, !IO).

:- pred output_addr_of_rtti_id(io.text_output_stream::in, rtti_id::in,
    io::di, io::uo) is det.

output_addr_of_rtti_id(Stream, RttiId, !IO) :-
    % All RttiIds are references to memory, with one exception: type variables.
    ( if
        RttiId = ctor_rtti_id(_, type_ctor_pseudo_type_info(type_var(VarNum)))
    then
        io.write_int(Stream, VarNum, !IO)
    else
        % If the RttiName is not an array, then we need to use `&'
        % to take its address.
        IsArray = rtti_id_has_array_type(RttiId),
        (
            IsArray = is_array,
            output_rtti_id(Stream, RttiId, !IO)
        ;
            IsArray = not_array,
            io.write_string(Stream, "&", !IO),
            output_rtti_id(Stream, RttiId, !IO)
        )
    ).

:- pred output_addr_of_ctor_rtti_id(rtti_type_ctor::in, ctor_rtti_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_addr_of_ctor_rtti_id(RttiTypeCtor, RttiName, Stream, !IO) :-
    output_addr_of_rtti_id(Stream, ctor_rtti_id(RttiTypeCtor, RttiName), !IO).

output_rtti_id(Stream, RttiId, !IO) :-
    rtti.id_to_c_identifier(RttiId, Str),
    io.write_string(Stream, mercury_data_prefix, !IO),
    io.write_string(Stream, Str, !IO).

:- pred output_ctor_rtti_id(io.text_output_stream::in,
    rtti_type_ctor::in, ctor_rtti_name::in, io::di, io::uo) is det.

output_ctor_rtti_id(Stream, RttiTypeCtor, RttiName, !IO) :-
    output_rtti_id(Stream, ctor_rtti_id(RttiTypeCtor, RttiName), !IO).

%-----------------------------------------------------------------------------%

:- pred output_maybe_quoted_string(maybe(string)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_maybe_quoted_string(MaybeName, Stream, !IO) :-
    (
        MaybeName = yes(Name),
        io.write_string(Stream, """", !IO),
        c_util.output_quoted_string(Stream, Name, !IO),
        io.write_string(Stream, """", !IO)
    ;
        MaybeName = no,
        io.write_string(Stream, "NULL", !IO)
    ).

:- pred output_maybe_quoted_strings(list(maybe(string))::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_maybe_quoted_strings(MaybeNames, Stream, !IO) :-
    io.write_string(Stream, "\t", !IO),
    write_out_list(output_maybe_quoted_string, ",\n\t", MaybeNames,
        Stream, !IO),
    io.write_string(Stream, "\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_exist_locn(exist_typeinfo_locn::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_exist_locn(Locn, Stream, !IO) :-
    (
        Locn = plain_typeinfo(SlotInCell),
        io.write_string(Stream, "{ ", !IO),
        % MR_exist_arg_num -- XXX MAKE_FIELD_UNSIGNED
        io.write_int16(Stream, int16.cast_from_uint16(SlotInCell), !IO),
        % MR_exist_offset_in_tci
        io.write_string(Stream, ", -1 }", !IO)
    ;
        Locn = typeinfo_in_tci(SlotInCell, SlotInTci),
        io.write_string(Stream, "{ ", !IO),
        % MR_exist_arg_num -- XXX MAKE_FIELD_UNSIGNED
        io.write_int16(Stream, int16.cast_from_uint16(SlotInCell), !IO),
        io.write_string(Stream, ", ", !IO),
        % MR_exist_offset_in_tci -- XXX MAKE_FIELD_UNSIGNED
        io.write_int16(Stream, int16.cast_from_uint16(SlotInTci), !IO),
        io.write_string(Stream, " }", !IO)
    ).

:- pred output_exist_locns(list(exist_typeinfo_locn)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_exist_locns(Locns, Stream, !IO) :-
    io.write_string(Stream, "\t", !IO),
    write_out_list(output_exist_locn, ",\n\t", Locns, Stream, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred output_maybe_static_code_addr(maybe(code_addr)::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pragma consider_used(pred(output_maybe_static_code_addr/4)).

output_maybe_static_code_addr(yes(CodeAddr), Stream, !IO) :-
    output_static_code_addr(CodeAddr, Stream, !IO).
output_maybe_static_code_addr(no, Stream, !IO) :-
    io.write_string(Stream, "NULL", !IO).

:- pred output_static_code_addr(code_addr::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_static_code_addr(CodeAddr, Stream, !IO) :-
    io.write_string(Stream, "MR_MAYBE_STATIC_CODE(", !IO),
    output_code_addr(Stream, CodeAddr, !IO),
    io.write_string(Stream, ")", !IO).

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

output_rtti_id_storage_type_name(Info, Stream, RttiId, BeingDefined,
        !DeclSet, !IO) :-
    output_rtti_type_decl(Stream, RttiId, !DeclSet, !IO),
    rtti_id_linkage(RttiId, Linkage),
    LinkageStr = c_data_linkage_string(Linkage, BeingDefined),
    io.write_string(Stream, LinkageStr, !IO),

    Globals = Info ^ lout_globals,
    InclCodeAddr = rtti_id_would_include_code_addr(RttiId),
    io.write_string(Stream, c_data_const_string(Globals, InclCodeAddr), !IO),

    rtti_id_c_type(RttiId, CType, IsArray),
    c_util.output_quoted_string(Stream, CType, !IO),
    io.write_string(Stream, " ", !IO),
    output_rtti_id(Stream, RttiId, !IO),
    (
        IsArray = is_array,
        io.write_string(Stream, "[]", !IO)
    ;
        IsArray = not_array
    ).

    % Each type_info and pseudo_type_info may have a different C type,
    % depending on what kind of type_info or pseudo_type_info it is,
    % and also on its arity. We need to declare that C type here.
    %
:- pred output_rtti_type_decl(io.text_output_stream::in, rtti_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_rtti_type_decl(Stream, RttiId, !DeclSet, !IO) :-
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
            io.format(Stream, Template, [i(Arity), i(Arity), i(Arity)], !IO),
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
            io.format(Stream, Template,
                [i(Arity), i(Arity), i(Arity), i(Arity)], !IO),
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

output_rtti_id_storage_type_name_no_decl(Info, Stream, RttiId, BeingDefined,
        !IO) :-
    decl_set_init(DeclSet0),
    output_rtti_id_storage_type_name(Info, Stream, RttiId, BeingDefined,
        DeclSet0, _, !IO).

%-----------------------------------------------------------------------------%

tabling_struct_data_addr_string(ProcLabel, Id) =
    mercury_var_prefix ++ "_proc" ++ tabling_info_id_str(Id) ++ "__" ++
        proc_label_to_c_string(do_not_add_label_prefix, ProcLabel).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.rtti_out.
%-----------------------------------------------------------------------------%
