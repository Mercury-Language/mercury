%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS global data in C#.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_global.
:- interface.

:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_global_var_decls_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

:- pred output_init_global_var_method_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

:- pred output_global_var_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, output_aux::in,
    mlds_global_var_defn::in, io::di, io::uo) is det.

:- pred output_scalar_common_data_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, ml_scalar_cell_map::in,
    io::di, io::uo) is det.

:- pred output_vector_common_data_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, ml_vector_cell_map::in,
    io::di, io::uo) is det.

:- pred output_rtti_assignments_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds_to_cs_class.
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module ml_backend.rtti_to_mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_global_var_decls_for_csharp(_, _, _, [], !IO).
output_global_var_decls_for_csharp(Info, Stream, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, Flags,
        Type, _Initializer, _GCStmt),
    output_n_indents(Stream, Indent, !IO),
    % We can't honour _Constness here as the variable is assigned separately.
    Flags = mlds_global_var_decl_flags(Access, _Constness),
    NonConstFlags = mlds_global_var_decl_flags(Access, modifiable),
    output_global_var_decl_flags_for_csharp(Stream, NonConstFlags, !IO),
    output_global_var_decl_for_csharp(Info, Stream, GlobalVarName, Type, !IO),
    io.write_string(Stream, ";\n", !IO),
    output_global_var_decls_for_csharp(Info, Stream, Indent,
        GlobalVarDefns, !IO).

:- pred output_global_var_decl_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, mlds_global_var_name::in, mlds_type::in,
    io::di, io::uo) is det.

output_global_var_decl_for_csharp(Info, Stream, GlobalVarName, Type, !IO) :-
    output_type_for_csharp(Info, Type, Stream, !IO),
    io.write_char(Stream, ' ', !IO),
    output_global_var_name_for_csharp(Stream, GlobalVarName, !IO).

%---------------------------------------------------------------------------%

output_init_global_var_method_for_csharp(Info, Stream, Indent,
        GlobalVarDefns, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "private static void MR_init_data() {\n", !IO),
    output_init_global_var_statements_for_csharp(Info, Stream, Indent + 1,
        GlobalVarDefns, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred output_init_global_var_statements_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

output_init_global_var_statements_for_csharp(_, _, _, [], !IO).
output_init_global_var_statements_for_csharp(Info, Stream, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, _Flags,
        Type, Initializer, _GCStmt),
    output_n_indents(Stream, Indent, !IO),
    output_global_var_name_for_csharp(Stream, GlobalVarName, !IO),
    output_initializer_for_csharp(Info, Stream, oa_none, Indent + 1,
        Type, Initializer, ";", !IO),
    output_init_global_var_statements_for_csharp(Info, Stream, Indent,
        GlobalVarDefns, !IO).

%---------------------------------------------------------------------------%

output_global_var_defn_for_csharp(Info, Stream, Indent, OutputAux,
        GlobalVarDefn, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, Flags,
        Type, Initializer, _),
    output_global_var_decl_flags_for_csharp(Stream, Flags, !IO),
    output_global_var_decl_for_csharp(Info, Stream, GlobalVarName, Type, !IO),
    output_initializer_for_csharp(Info, Stream, OutputAux, Indent + 1,
        Type, Initializer, ";", !IO).

%---------------------------------------------------------------------------%

output_scalar_common_data_for_csharp(Info, Stream, Indent,
        ScalarCellGroupMap, !IO) :-
    % Elements of scalar data arrays may reference elements in higher-numbered
    % arrays, or elements of the same array, so we must initialise them
    % separately in a static initialisation block, and we must ensure that
    % elements which are referenced by other elements are initialised first.
    map.foldl3(output_scalar_defns_for_csharp(Info, Stream, Indent),
        ScalarCellGroupMap, digraph.init, Graph, map.init, Map, !IO),

    ( if digraph.tsort(Graph, SortedScalars0) then
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream,
            "private static void MR_init_scalar_common_data() {\n", !IO),
        list.reverse(SortedScalars0, SortedScalars),
        list.foldl(
            output_scalar_init_for_csharp(Info, Stream, Indent + 1, Map),
            SortedScalars, !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "}\n", !IO)
    else
        unexpected($pred, "digraph.tsort failed")
    ).

:- pred output_scalar_defns_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_scalar_common_type_num::in, ml_scalar_cell_group::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out, io::di, io::uo) is det.

output_scalar_defns_for_csharp(Info, Stream, Indent, TypeNum, CellGroup,
        !Graph, !Map, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, _InitArraySize, _Counter, _Members,
        RowInitsCord),
    ArrayType = mlds_array_type(Type),
    RowInits = cord.list(RowInitsCord),

    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "private static readonly ", !IO),
    output_type_for_csharp(Info, Type, Stream, !IO),
    io.format(Stream, "[] MR_scalar_common_%d = ", [i(TypeRawNum)], !IO),
    output_initializer_alloc_only_for_csharp(Info, Stream,
        init_array(RowInits), yes(ArrayType), ";", !IO),

    MLDS_ModuleName = Info ^ csoi_module_name,
    list.foldl3(add_scalar_inits(MLDS_ModuleName, Type, TypeNum),
        RowInits, 0, _, !Graph, !Map).

:- pred output_scalar_init_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in, mlds_scalar_common::in,
    io::di, io::uo) is det.

output_scalar_init_for_csharp(Info, Stream, Indent, Map, Scalar, !IO) :-
    map.lookup(Map, Scalar, Initializer),
    Scalar = ml_scalar_common(_, Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "MR_scalar_common_%d[%d] =\n",
        [i(TypeRawNum), i(RowNum)], !IO),
    output_initializer_body_for_csharp(Info, Stream, at_start_of_line,
        Indent + 1, Initializer, yes(Type), ";", !IO).

%---------------------------------------------------------------------------%

output_vector_common_data_for_csharp(Info, Stream, Indent,
        VectorCellGroupMap, !IO) :-
    map.foldl(output_vector_cell_decl_for_csharp(Info, Stream, Indent),
        VectorCellGroupMap, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream,
        "private static void MR_init_vector_common_data() {\n", !IO),
    map.foldl(output_vector_cell_init_for_csharp(Info, Stream, Indent + 1),
        VectorCellGroupMap, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred output_vector_cell_decl_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_decl_for_csharp(Info, Stream, Indent, TypeNum,
        CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, ClassDefn, _FieldIds, _NextRow,
        _RowInits),
    output_class_defn_for_csharp(Info, Stream, Indent, ClassDefn, !IO),

    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "private static /* readonly */ ", !IO),
    output_type_for_csharp(Info, Type, Stream, !IO),
    io.format(Stream, "[] MR_vector_common_%d;\n", [i(TypeRawNum)], !IO).

:- pred output_vector_cell_init_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_init_for_csharp(Info, Stream, Indent, TypeNum,
        CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, _ClassDefn, _FieldIds, _NextRow,
        RowInits),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "MR_vector_common_%d = new ", [i(TypeRawNum)], !IO),
    output_type_for_csharp(Info, Type, Stream, !IO),
    io.write_string(Stream, "[]\n", !IO),
    output_n_indents(Stream, Indent + 1, !IO),
    io.write_string(Stream, "{\n", !IO),
    output_nonempty_initializer_body_list_for_csharp(Info, Stream, Indent + 2,
        cord.list(RowInits), "", !IO),
    output_n_indents(Stream, Indent + 1, !IO),
    io.write_string(Stream, "};\n", !IO).

%---------------------------------------------------------------------------%

output_rtti_assignments_for_csharp(Info, Stream, Indent, Defns, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "static void MR_init_rtti() {\n", !IO),
    OrderedDefns = order_mlds_rtti_defns(Defns),
    list.foldl(
        output_rtti_defns_assignments_for_csharp(Info, Stream, Indent + 1),
        OrderedDefns, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred output_rtti_defns_assignments_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

output_rtti_defns_assignments_for_csharp(Info, Stream, Indent, Defns, !IO) :-
    % Separate cliques.
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "//\n", !IO),
    list.foldl(output_rtti_defn_assignments_for_csharp(Info, Stream, Indent),
        Defns, !IO).

:- pred output_rtti_defn_assignments_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_global_var_defn::in,
    io::di, io::uo) is det.

output_rtti_defn_assignments_for_csharp(Info, Stream, Indent,
        GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, _Flags,
        _Type, Initializer, _),
    (
        Initializer = no_initializer
    ;
        Initializer = init_obj(_),
        % Not encountered in practice.
        unexpected($pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        IsArray = type_is_array_for_csharp(StructType),
        (
            IsArray = not_array,
            output_n_indents(Stream, Indent, !IO),
            output_global_var_name_for_csharp(Stream, GlobalVarName, !IO),
            io.write_string(Stream, ".init(\n", !IO),
            output_nonempty_initializer_body_list_for_csharp(Info, Stream,
                Indent + 1, FieldInits, "", !IO),
            output_n_indents(Stream, Indent, !IO),
            io.write_string(Stream, ");\n", !IO)
        ;
            IsArray = is_array,
            % Not encountered in practice.
            unexpected($pred, "is_array")
        )
    ;
        Initializer = init_array(ElementInits),
        list.foldl2(
            output_rtti_array_assignments_for_csharp(Info, Stream, Indent,
                GlobalVarName),
            ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    mlds_global_var_name::in, mlds_initializer::in,
    int::in, int::out, io::di, io::uo) is det.

output_rtti_array_assignments_for_csharp(Info, Stream, Indent, GlobalVarName,
        ElementInit, Index, Index + 1, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    output_global_var_name_for_csharp(Stream, GlobalVarName, !IO),
    io.write_string(Stream, "[", !IO),
    io.write_int(Stream, Index, !IO),
    io.write_string(Stream, "] =\n", !IO),
    output_initializer_body_for_csharp(Info, Stream, at_start_of_line,
        Indent + 1, ElementInit, no, ";", !IO).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred output_global_var_decl_flags_for_csharp(io.text_output_stream::in,
    mlds_global_var_decl_flags::in, io::di, io::uo) is det.

output_global_var_decl_flags_for_csharp(Stream, Flags, !IO) :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    (
        Access = gvar_acc_whole_program,
        io.write_string(Stream, "public ", !IO)
    ;
        Access = gvar_acc_module_only,
        io.write_string(Stream, "private ", !IO)
    ),
    % PerInstance = one_copy,
    io.write_string(Stream, "static ", !IO),
    (
        Constness = const,
        io.write_string(Stream, "readonly ", !IO)
    ;
        Constness = modifiable
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_global.
%---------------------------------------------------------------------------%
