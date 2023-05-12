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

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.indent.
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
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, Flags0,
        Type, _Initializer, _GCStmt),
    IndentStr = indent2_string(Indent),
    % We can't honour _Constness here as the variable is assigned separately.
    Flags0 = mlds_global_var_decl_flags(Access, _Constness),
    Flags  = mlds_global_var_decl_flags(Access, modifiable),
    FlagsStr = global_var_decl_flags_to_string_for_csharp(Flags),
    TypeStr = type_to_string_for_csharp(Info, Type),
    GlobalVarNameStr = global_var_name_to_ll_string_for_csharp(GlobalVarName),
    io.format(Stream, "%s%s %s %s;\n",
        [s(IndentStr), s(FlagsStr), s(TypeStr), s(GlobalVarNameStr)], !IO),
    output_global_var_decls_for_csharp(Info, Stream, Indent,
        GlobalVarDefns, !IO).

%---------------------------------------------------------------------------%

output_init_global_var_method_for_csharp(Info, Stream, Indent,
        GlobalVarDefns, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sprivate static void MR_init_data() {\n",
        [s(IndentStr)], !IO),
    output_init_global_var_statements_for_csharp(Info, Stream, Indent + 1,
        GlobalVarDefns, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_init_global_var_statements_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

output_init_global_var_statements_for_csharp(_, _, _, [], !IO).
output_init_global_var_statements_for_csharp(Info, Stream, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, _Flags,
        Type, Initializer, _GCStmt),
    IndentStr = indent2_string(Indent),
    GlobalVarNameStr = global_var_name_to_ll_string_for_csharp(GlobalVarName),
    io.format(Stream, "%s%s", [s(IndentStr), s(GlobalVarNameStr)], !IO),
    output_initializer_for_csharp(Info, Stream, oa_none, Indent + 1,
        Type, Initializer, ";", !IO),
    output_init_global_var_statements_for_csharp(Info, Stream, Indent,
        GlobalVarDefns, !IO).

%---------------------------------------------------------------------------%

output_global_var_defn_for_csharp(Info, Stream, Indent, OutputAux,
        GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, Flags,
        Type, Initializer, _),
    IndentStr = indent2_string(Indent),
    FlagsStr = global_var_decl_flags_to_string_for_csharp(Flags),
    TypeStr = type_to_string_for_csharp(Info, Type),
    GlobalVarNameStr = global_var_name_to_ll_string_for_csharp(GlobalVarName),
    io.format(Stream, "%s%s %s %s",
        [s(IndentStr), s(FlagsStr), s(TypeStr), s(GlobalVarNameStr)], !IO),
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

    ( if digraph.return_vertices_in_from_to_order(Graph, FromToScalars) then
        list.reverse(FromToScalars, ToFromScalars),
        IndentStr = indent2_string(Indent),
        io.format(Stream,
            "%sprivate static void MR_init_scalar_common_data() {\n",
            [s(IndentStr)], !IO),
        list.foldl(
            output_scalar_init_for_csharp(Info, Stream, Indent + 1, Map),
            ToFromScalars, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
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

    IndentStr = indent2_string(Indent),
    TypeStr = type_to_string_for_csharp(Info, Type),
    io.format(Stream, "%sprivate static readonly %s[] MR_scalar_common_%d = ",
        [s(IndentStr), s(TypeStr), i(TypeRawNum)], !IO),
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
    IndentStr = indent2_string(Indent),
    map.lookup(Map, Scalar, Initializer),
    Scalar = mlds_scalar_common(_, Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    io.format(Stream, "%sMR_scalar_common_%d[%d] =\n",
        [s(IndentStr), i(TypeRawNum), i(RowNum)], !IO),
    output_initializer_body_for_csharp(Info, Stream, at_start_of_line,
        Indent + 1, Initializer, yes(Type), ";", !IO).

%---------------------------------------------------------------------------%

output_vector_common_data_for_csharp(Info, Stream, Indent,
        VectorCellGroupMap, !IO) :-
    map.foldl(output_vector_cell_decl_for_csharp(Info, Stream, Indent),
        VectorCellGroupMap, !IO),
    IndentStr = indent2_string(Indent),
    io.format(Stream,
        "%sprivate static void MR_init_vector_common_data() {\n",
        [s(IndentStr)], !IO),
    map.foldl(output_vector_cell_init_for_csharp(Info, Stream, Indent + 1),
        VectorCellGroupMap, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

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

    IndentStr = indent2_string(Indent),
    TypeStr = type_to_string_for_csharp(Info, Type),
    io.format(Stream,
        "%sprivate static /* readonly */ %s[] MR_vector_common_%d;\n",
        [s(IndentStr), s(TypeStr), i(TypeRawNum)], !IO).

:- pred output_vector_cell_init_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_init_for_csharp(Info, Stream, Indent, TypeNum,
        CellGroup, !IO) :-
    Indent1 = Indent + 1,
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent1),
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, _ClassDefn, _FieldIds, _NextRow,
        RowInits),
    TypeStr = type_to_string_for_csharp(Info, Type),
    io.format(Stream, "%sMR_vector_common_%d = new %s[]\n",
        [s(IndentStr), i(TypeRawNum), s(TypeStr)], !IO),
    io.format(Stream, "%s{\n", [s(Indent1Str)], !IO),
    output_nonempty_initializer_body_list_for_csharp(Info, Stream, Indent + 2,
        cord.list(RowInits), "", !IO),
    io.format(Stream, "%s};\n", [s(Indent1Str)], !IO).

%---------------------------------------------------------------------------%

output_rtti_assignments_for_csharp(Info, Stream, Indent, Defns, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sstatic void MR_init_rtti() {\n", [s(IndentStr)], !IO),
    OrderedDefns = order_mlds_rtti_defns(Defns),
    list.foldl(
        output_rtti_defns_assignments_for_csharp(Info, Stream, Indent + 1),
        OrderedDefns, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_rtti_defns_assignments_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

output_rtti_defns_assignments_for_csharp(Info, Stream, Indent, Defns, !IO) :-
    % Separate cliques.
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s//\n", [s(IndentStr)], !IO),
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
        type_to_string_and_dims_for_csharp(Info, StructType,
            _BaseTypeName, ArrayDims),
        (
            ArrayDims = [],
            IndentStr = indent2_string(Indent),
            GlobalVarNameStr =
                global_var_name_to_ll_string_for_csharp(GlobalVarName),
            io.format(Stream, "%s%s.init(\n",
                [s(IndentStr), s(GlobalVarNameStr)], !IO),
            output_nonempty_initializer_body_list_for_csharp(Info, Stream,
                Indent + 1, FieldInits, "", !IO),
            io.format(Stream, "%s);\n", [s(IndentStr)], !IO)
        ;
            ArrayDims = [_ | _],
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
    IndentStr = indent2_string(Indent),
    GlobalVarNameStr = global_var_name_to_ll_string_for_csharp(GlobalVarName),
    io.format(Stream, "%s%s[%d] =\n",
        [s(IndentStr), s(GlobalVarNameStr), i(Index)], !IO),
    output_initializer_body_for_csharp(Info, Stream, at_start_of_line,
        Indent + 1, ElementInit, no, ";", !IO).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- func global_var_decl_flags_to_string_for_csharp(mlds_global_var_decl_flags)
    = string.

global_var_decl_flags_to_string_for_csharp(Flags) = FlagsStr :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    (
        Access = gvar_acc_whole_program,
        (
            Constness = const,
            FlagsStr = "public static readonly"
        ;
            Constness = modifiable,
            FlagsStr = "public static"
        )
    ;
        Access = gvar_acc_module_only,
        (
            Constness = const,
            FlagsStr = "private static readonly"
        ;
            Constness = modifiable,
            FlagsStr = "private static"
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_global.
%---------------------------------------------------------------------------%
