%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS global data in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_global.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_global_var_decls_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

:- pred output_global_var_defn_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, output_aux::in,
    mlds_global_var_defn::in, io::di, io::uo) is det.

:- pred output_global_var_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

:- pred output_scalar_common_data_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_scalar_cell_map::in, io::di, io::uo) is det.

:- pred output_vector_common_data_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_vector_cell_map::in, io::di, io::uo) is det.

:- pred output_rtti_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds_to_java_class.
:- import_module ml_backend.mlds_to_java_data.
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_type.
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

output_global_var_decls_for_java(_, _, _, [], !IO).
output_global_var_decls_for_java(Info, Stream, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(Name, _Context, Flags,
        Type, _Initializer, _GCStmt),
    write_indent2(Stream, Indent, !IO),
    output_global_var_decl_flags_for_java(Stream, Flags, !IO),
    output_global_var_decl_for_java(Info, Stream, Name, Type, !IO),
    io.write_string(Stream, ";\n", !IO),
    output_global_var_decls_for_java(Info, Stream, Indent,
        GlobalVarDefns, !IO).

:- pred output_global_var_decl_for_java(java_out_info::in,
    io.text_output_stream::in, mlds_global_var_name::in, mlds_type::in,
    io::di, io::uo) is det.

output_global_var_decl_for_java(Info, Stream, GlobalVarName, Type, !IO) :-
    TypeStr = type_to_string_for_java(Info, Type),
    GlobalVarNameStr = global_var_name_to_string_for_java(GlobalVarName),
    io.format(Stream, "%s %s", [s(TypeStr), s(GlobalVarNameStr)], !IO).

%---------------------------------------------------------------------------%

output_global_var_defn_for_java(Info, Stream, Indent, OutputAux,
        GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, Context, Flags, Type,
        Initializer, _),
    indent_line_after_context(Stream, Info ^ joi_line_numbers, marker_comment,
        Context, Indent, !IO),
    output_global_var_decl_flags_for_java(Stream, Flags, !IO),
    output_global_var_decl_for_java(Info, Stream, GlobalVarName, Type, !IO),
    output_initializer_for_java(Info, Stream, OutputAux, Indent + 1,
        Type, Initializer, ";", !IO).

%---------------------------------------------------------------------------%

output_global_var_assignments_for_java(Info, Stream, Indent,
        GlobalVarDefns, !IO) :-
    % Putting all assignments to global variables into a single method
    % may run into maximum method size limits. To avoid this, we split up
    % the assignments into a bunch of methods, each of a limited size.
    list.chunk(GlobalVarDefns, 1000, DefnChunks),
    list.foldl2(output_init_global_var_method_for_java(Info, Stream, Indent),
        DefnChunks, 0, NumChunks, !IO),

    % Call the individual methods.
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sstatic {\n", [s(IndentStr)], !IO),
    int.fold_up(
        output_call_init_global_var_method_for_java(Stream, Indent + 1),
        0, NumChunks - 1, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_init_global_var_method_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    int::in, int::out, io::di, io::uo) is det.

output_init_global_var_method_for_java(Info, Stream, Indent, Defns,
        Chunk, Chunk + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sprivate static void MR_init_data_%d() {\n",
        [s(IndentStr), i(Chunk)], !IO),
    output_init_global_var_statements_for_java(Info, Stream, Indent + 1,
        Defns, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_init_global_var_statements_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

output_init_global_var_statements_for_java(_, _, _, [], !IO).
output_init_global_var_statements_for_java(Info, Stream, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, _Flags,
        Type, Initializer, _GCStmt),
    IndentStr = indent2_string(Indent),
    GlobalVarNameStr = global_var_name_to_string_for_java(GlobalVarName),
    io.format(Stream, "%s%s", [s(IndentStr), s(GlobalVarNameStr)], !IO),
    output_initializer_for_java(Info, Stream, oa_none, Indent + 1,
        Type, Initializer, ";", !IO),
    output_init_global_var_statements_for_java(Info, Stream, Indent,
        GlobalVarDefns, !IO).

:- pred output_call_init_global_var_method_for_java(io.text_output_stream::in,
    indent::in, int::in, io::di, io::uo) is det.

output_call_init_global_var_method_for_java(Stream, Indent, I, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sMR_init_data_%d();\n", [s(IndentStr), i(I)], !IO).

%---------------------------------------------------------------------------%
%
% Code to output common data.
%

output_scalar_common_data_for_java(Info, Stream, Indent,
        ScalarCellGroupMap, !IO) :-
    % Elements of scalar data arrays may reference elements in higher-numbered
    % arrays, or elements of the same array, so we must initialise them
    % separately in a static initialisation block, and we must ensure that
    % elements which are referenced by other elements are initialised first.
    map.foldl3(output_scalar_defns_for_java(Info, Stream, Indent),
        ScalarCellGroupMap, digraph.init, Graph, map.init, Map, !IO),

    ( if digraph.tsort(Graph, SortedScalars0) then
        % Divide into small methods to avoid running into the maximum method
        % size limit.
        list.reverse(SortedScalars0, SortedScalars),
        list.chunk(SortedScalars, 1000, ScalarChunks),
        list.foldl2(
            output_scalar_init_method_for_java(Info, Stream, Indent, Map),
            ScalarChunks, 0, NumChunks, !IO),

        % Call the individual methods.
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%sstatic {\n", [s(IndentStr)], !IO),
        int.fold_up(
            output_call_scalar_init_method_for_java(Stream, Indent + 1),
            0, NumChunks - 1, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
    else
        unexpected($pred, "digraph.tsort failed")
    ).

:- pred output_scalar_defns_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_scalar_common_type_num::in, ml_scalar_cell_group::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out, io::di, io::uo) is det.

output_scalar_defns_for_java(Info, Stream, Indent, TypeNum, CellGroup,
        !Graph, !Map, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, _InitArraySize, _Counter, _Members,
        RowInitsCord),
    ArrayType = mlds_array_type(Type),
    RowInits = cord.list(RowInitsCord),

    IndentStr = indent2_string(Indent),
    TypeStr = type_to_string_for_java(Info, Type),
    io.format(Stream, "%sprivate static final %s[] MR_scalar_common_%d = ",
        [s(IndentStr), s(TypeStr), i(TypeRawNum)], !IO),
    output_initializer_alloc_only_for_java(Info, Stream, init_array(RowInits),
        yes(ArrayType), ";", !IO),

    MLDS_ModuleName = Info ^ joi_module_name,
    list.foldl3(add_scalar_inits(MLDS_ModuleName, Type, TypeNum),
        RowInits, 0, _, !Graph, !Map).

:- pred output_scalar_init_method_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in,
    list(mlds_scalar_common)::in, int::in, int::out, io::di, io::uo) is det.

output_scalar_init_method_for_java(Info, Stream, Indent, Map, Scalars,
        ChunkNum, ChunkNum + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sprivate static void MR_init_scalars_%d() {\n",
        [s(IndentStr), i(ChunkNum)], !IO),
    list.foldl(output_scalar_init_for_java(Info, Stream, Indent + 1, Map),
        Scalars, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_scalar_init_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in, mlds_scalar_common::in,
    io::di, io::uo) is det.

output_scalar_init_for_java(Info, Stream, Indent, Map, Scalar, !IO) :-
    IndentStr = indent2_string(Indent),
    map.lookup(Map, Scalar, Initializer),
    Scalar = mlds_scalar_common(_, Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    io.format(Stream, "%sMR_scalar_common_%d[%d] =\n",
        [s(IndentStr), i(TypeRawNum), i(RowNum)], !IO),
    output_initializer_body_for_java(Info, Stream, at_start_of_line,
        Indent + 1, Initializer, yes(Type), ";", !IO).

:- pred output_call_scalar_init_method_for_java(io.text_output_stream::in,
    indent::in, int::in, io::di, io::uo) is det.

output_call_scalar_init_method_for_java(Stream, Indent, ChunkNum, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sMR_init_scalars_%d();\n",
        [s(IndentStr), i(ChunkNum)], !IO).

%---------------------------------------------------------------------------%

output_vector_common_data_for_java(Info, Stream, Indent,
        VectorCellGroupMap, !IO) :-
    map.foldl(output_vector_cell_group_for_java(Info, Stream, Indent),
        VectorCellGroupMap, !IO).

:- pred output_vector_cell_group_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_group_for_java(Info, Stream, Indent, TypeNum,
        CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, ClassDefn, _FieldIds, _NextRow,
        RowInits),
    output_class_defn_for_java(Info, Stream, Indent, ClassDefn, !IO),

    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1),
    TypeStr = type_to_string_for_java(Info, Type),
    io.format(Stream, "%sprivate static final %s MR_vector_common_%d[] =\n",
        [s(IndentStr), s(TypeStr), i(TypeRawNum)], !IO),
    io.format(Stream, "%s{\n", [s(Indent1Str)], !IO),
    output_nonempty_initializer_body_list_for_java(Info, Stream, Indent + 2,
        cord.list(RowInits), "", !IO),
    io.format(Stream, "%s};\n", [s(Indent1Str)], !IO).

%---------------------------------------------------------------------------%
%
% Code to output RTTI data assignments.
%

output_rtti_assignments_for_java(Info, Stream, Indent, GlobalVarDefns, !IO) :-
    (
        GlobalVarDefns = []
    ;
        GlobalVarDefns = [_ | _],
        OrderedDefns = order_mlds_rtti_defns(GlobalVarDefns),
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%sstatic {\n", [s(IndentStr)], !IO),
        list.foldl(
            output_rtti_defns_assignments_for_java(Info, Stream, Indent + 1),
            OrderedDefns, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
    ).

:- pred output_rtti_defns_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

output_rtti_defns_assignments_for_java(Info, Stream, Indent,
        GlobalVarDefns, !IO) :-
    % Separate cliques.
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s//\n", [s(IndentStr)], !IO),
    list.foldl(output_rtti_defn_assignments_for_java(Info, Stream, Indent),
        GlobalVarDefns, !IO).

:- pred output_rtti_defn_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    mlds_global_var_defn::in, io::di, io::uo) is det.

output_rtti_defn_assignments_for_java(Info, Stream, Indent,
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
        type_to_string_and_dims_for_java(Info, StructType,
            _BaseType, ArrayDims),
        (
            ArrayDims = [],
            IndentStr = indent2_string(Indent),
            GlobalVarNameStr =
                global_var_name_to_string_for_java(GlobalVarName),
            io.format(Stream, "%s%s.init(\n",
                [s(IndentStr), s(GlobalVarNameStr)], !IO),
            output_nonempty_initializer_body_list_for_java(Info, Stream,
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
            output_rtti_array_assignments_for_java(Info, Stream, Indent,
                GlobalVarName),
            ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_global_var_name::in,
    mlds_initializer::in, int::in, int::out, io::di, io::uo) is det.

output_rtti_array_assignments_for_java(Info, Stream, Indent, GlobalVarName,
        ElementInit, Index, Index + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    GlobalVarNameStr = global_var_name_to_string_for_java(GlobalVarName),
    io.format(Stream, "%s%s[%d] =\n",
        [s(IndentStr), s(GlobalVarNameStr), i(Index)],  !IO),
    output_initializer_body_for_java(Info, Stream, at_start_of_line,
        Indent + 1, ElementInit, no, ";", !IO).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred output_global_var_decl_flags_for_java(io.text_output_stream::in,
    mlds_global_var_decl_flags::in, io::di, io::uo) is det.

output_global_var_decl_flags_for_java(Stream, Flags, !IO) :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    (
        Access = gvar_acc_whole_program,
        io.write_string(Stream, "public ", !IO)
    ;
        Access = gvar_acc_module_only,
        io.write_string(Stream, "private ", !IO)
    ),
    io.write_string(Stream, "static ", !IO),
    (
        Constness = const,
        io.write_string(Stream, "final ", !IO)
    ;
        Constness = modifiable
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_global.
%---------------------------------------------------------------------------%
