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
:- import_module uint.

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
    output_initializer_for_java(Info, Stream, OutputAux, Indent + 1u,
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
    io.format(Stream, "\n%sstatic {\n", [s(IndentStr)], !IO),
    int.fold_up(
        output_call_init_global_var_method_for_java(Stream, Indent + 1u),
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
    output_init_global_var_statements_for_java(Info, Stream, Indent + 1u,
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
    output_initializer_for_java(Info, Stream, oa_none, Indent + 1u,
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
        ScalarCellGroupMap, map.init, InitMap, digraph.init, DepGraph, !IO),

    ( if digraph.return_vertices_in_to_from_order(DepGraph, ToFromScalars) then
        % Divide into small methods to avoid running into the maximum method
        % size limit.
        list.chunk(ToFromScalars, 1000, ScalarChunks),
        list.foldl2(
            output_scalar_init_method_for_java(Info, Stream, Indent, InitMap),
            ScalarChunks, 0, NumChunks, !IO),

        % Call the individual methods.
        io.nl(Stream, !IO),
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%sstatic {\n", [s(IndentStr)], !IO),
        int.fold_up(
            output_call_scalar_init_method_for_java(Stream, Indent + 1u),
            0, NumChunks - 1, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
    else
        unexpected($pred, "digraph.tsort failed")
    ).

:- pred output_scalar_defns_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    ml_scalar_common_type_num::in, ml_scalar_cell_group::in,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    io::di, io::uo) is det.

output_scalar_defns_for_java(Info, Stream, Indent, TypeNum, CellGroup,
        !InitMap, !DepGraph, !IO) :-
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
    record_scalar_inits_build_dep_graph(MLDS_ModuleName, Type, TypeNum,
        RowInits, 0, _, !InitMap, !DepGraph).

:- pred output_scalar_init_method_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in,
    list(mlds_scalar_common)::in, int::in, int::out, io::di, io::uo) is det.

output_scalar_init_method_for_java(Info, Stream, Indent, InitMap, Scalars,
        ChunkNum, ChunkNum + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "\n%sprivate static void MR_init_scalars_%d() {\n",
        [s(IndentStr), i(ChunkNum)], !IO),
    list.foldl(output_scalar_init_for_java(Info, Stream, Indent + 1u, InitMap),
        Scalars, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_scalar_init_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in, mlds_scalar_common::in,
    io::di, io::uo) is det.

output_scalar_init_for_java(Info, Stream, Indent, InitMap, Scalar, !IO) :-
    IndentStr = indent2_string(Indent),
    map.lookup(InitMap, Scalar, Initializer),
    Scalar = mlds_scalar_common(_, Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    io.format(Stream, "%sMR_scalar_common_%d[%d] =\n",
        [s(IndentStr), i(TypeRawNum), i(RowNum)], !IO),
    output_initializer_body_for_java(Info, Stream, at_start_of_line,
        Indent + 1u, Initializer, yes(Type), ";", !IO).

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
    CellGroup = ml_vector_cell_group(Type, StructDefn, _FieldIds, _NextRow,
        RowInits),
    output_struct_defn_for_java(Info, Stream, Indent, StructDefn, !IO),

    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1u),
    TypeStr = type_to_string_for_java(Info, Type),
    io.format(Stream, "%sprivate static final %s MR_vector_common_%d[] =\n",
        [s(IndentStr), s(TypeStr), i(TypeRawNum)], !IO),
    io.format(Stream, "%s{\n", [s(Indent1Str)], !IO),
    output_nonempty_initializer_body_list_for_java(Info, Stream, Indent + 2u,
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
        OrderedDefnSccs = order_mlds_rtti_defns_into_sccs(GlobalVarDefns),
        output_rtti_defn_chunk_assignments_for_java(Info, Stream, Indent,
            1, OrderedDefnSccs, [], RevChunkCalls, !IO),
        list.reverse(RevChunkCalls, ChunkCalls),
        IndentStr = indent2_string(Indent),
        io.format(Stream, "\n%sstatic {\n", [s(IndentStr)], !IO),
        list.foldl(io.write_string(Stream), ChunkCalls, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
    ).

:- pred output_rtti_defn_chunk_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, int::in,
    list(list(mlds_global_var_defn))::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

output_rtti_defn_chunk_assignments_for_java(Info, Stream, Indent, ChunkNum,
        Sccs, !RevChunkCalls, !IO) :-
    IndentStr = indent2_string(Indent),
    gather_global_var_sccs_for_chunk(Sccs, 0, NumDefns, 0u, ChunkSizeEstimate,
        [], RevChunkSccs, LeftOverSccs),
    list.reverse(RevChunkSccs, ChunkSccs),
    (
        ChunkSccs = []
    ;
        ChunkSccs = [_ | _],
        list.length(ChunkSccs, NumSccs),
        io.format(Stream,
            "\n%s// chunk #%d, #sccs = %d, #defns = %d size estimate %u\n",
            [s(IndentStr), i(ChunkNum), i(NumSccs), i(NumDefns),
            u(ChunkSizeEstimate)], !IO),
        string.format("rtti_init_%d", [i(ChunkNum)], ChunkMethodName),
        string.format("%s  %s();\n", [s(IndentStr), s(ChunkMethodName)],
            ChunkCall),
        !:RevChunkCalls = [ChunkCall | !.RevChunkCalls],
        io.format(Stream, "%sprivate static void %s() {\n",
            [s(IndentStr), s(ChunkMethodName)], !IO),
        list.foldl(
            output_rtti_defns_assignments_for_java(Info, Stream, Indent + 1u),
            ChunkSccs, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO),

        output_rtti_defn_chunk_assignments_for_java(Info, Stream, Indent,
            ChunkNum + 1, LeftOverSccs, !RevChunkCalls, !IO)
    ).

:- pred gather_global_var_sccs_for_chunk(list(list(mlds_global_var_defn))::in,
    int::in, int::out, uint::in, uint::out,
    list(list(mlds_global_var_defn))::in,
    list(list(mlds_global_var_defn))::out,
    list(list(mlds_global_var_defn))::out) is det.

gather_global_var_sccs_for_chunk([], !NumDefns, !ChunkSizeSoFar,
        !RevChunkDefns, []).
gather_global_var_sccs_for_chunk([Scc | Sccs], !NumDefns,
        ChunkSizeSoFar0, ChunkSizeSoFar, !RevChunkSccDefns,
        LeftOverSccDefns) :-
    list.foldl(acc_estimate_size_of_global_var_defn, Scc,
        0u, SccSizeEstimate),
    ChunkSizeSoFar1 = ChunkSizeSoFar0 + SccSizeEstimate,
    % This arbitrary figure, plucked out of the air, is intended to limit
    % the each chunk to a size that fits into 64 Kb of JVM bytecode.
    % It seems to work.
    ( if ChunkSizeSoFar1 < 5000u then
        !:NumDefns = !.NumDefns + list.length(Scc),
        !:RevChunkSccDefns = [Scc | !.RevChunkSccDefns],
        gather_global_var_sccs_for_chunk(Sccs, !NumDefns,
            ChunkSizeSoFar1, ChunkSizeSoFar,
            !RevChunkSccDefns, LeftOverSccDefns)
    else
        (
            !.RevChunkSccDefns = [],
            % We get here if Scc exceeds the size limit all by itself.
            % We *could* try to make it come in under the limit by breaking
            % its first definition off from the rest, but most SCCs contain
            % one global var definition anyway.
            !:NumDefns = list.length(Scc),
            ChunkSizeSoFar = SccSizeEstimate,
            !:RevChunkSccDefns = [Scc],
            LeftOverSccDefns = Sccs
        ;
            !.RevChunkSccDefns = [_ | _],
            ChunkSizeSoFar = ChunkSizeSoFar0,
            LeftOverSccDefns = [Scc | Sccs]
        )
    ).

:- pred acc_estimate_size_of_global_var_defn(mlds_global_var_defn::in,
    uint::in, uint::out) is det.

acc_estimate_size_of_global_var_defn(Defn, !TotalSizeEstimate) :-
    estimate_size_of_global_var_defn(Defn, SizeEstimate),
    !:TotalSizeEstimate = !.TotalSizeEstimate + SizeEstimate.

:- pred estimate_size_of_global_var_defn(mlds_global_var_defn::in,
    uint::out) is det.

estimate_size_of_global_var_defn(Defn, SizeEstimate) :-
    Defn = mlds_global_var_defn(_, _, _, _, Initializer, _),
    % The absolute values of the size estimates do not matter.
    % All that matters is the *relationship* to the chunk limit size
    % in gather_global_var_sccs_for_chunk.
    %
    % Our metric tries to predict the size of the JVM bytecode needed
    % for the global var definition. It does not have to accurate, as long as
    % its errors fall on the conservative side, and/or as long as its
    % underestimates are counterbalanced by the conservative limit in
    % gather_global_var_sccs_for_chunk.
    (
        Initializer = no_initializer,
        SizeEstimate = 0u
    ;
        Initializer = init_obj(_),
        SizeEstimate = 5u
    ;
        Initializer = init_struct(_StructType, FieldInits),
        list.length(FieldInits, NumFieldInits),
        SizeEstimate = 5u + 5u * uint.cast_from_int(NumFieldInits)
    ;
        Initializer = init_array(ElementInits),
        list.length(ElementInits, NumElementInits),
        SizeEstimate = 5u + 5u * uint.cast_from_int(NumElementInits)
    ).

:- pred output_rtti_defns_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

output_rtti_defns_assignments_for_java(Info, Stream, Indent, SccDefns,
        !IO) :-
    IndentStr = indent2_string(Indent),
    (
        SccDefns = [],
        unexpected($pred, "SccDefns = []")
    ;
        SccDefns = [HeadSccDefn | TailSccDefns],
        output_rtti_defn_assignments_for_java(Info, Stream, Indent,
            HeadSccDefn, !IO),
        (
            TailSccDefns = []
        ;
            TailSccDefns = [_ | _],
            list.length(SccDefns, NumSccDefns),
            ( if NumSccDefns > 1 then
                io.format(Stream, "%s// scc size %d\n",
                    [s(IndentStr), i(NumSccDefns)], !IO)
            else
                true
            ),
            list.foldl(
                output_rtti_defn_assignments_for_java(Info, Stream, Indent),
                TailSccDefns, !IO)
        )
    ).

:- pred output_rtti_defn_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in,
    mlds_global_var_defn::in, io::di, io::uo) is det.

output_rtti_defn_assignments_for_java(Info, Stream, Indent,
        GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, _Flags,
        _Type, Initializer, _),
    GlobalVarNameStr = global_var_name_to_string_for_java(GlobalVarName),
    (
        Initializer = no_initializer,
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%s// no initializer for %s\n",
            [s(IndentStr), s(GlobalVarNameStr)], !IO)
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
            io.format(Stream, "%s%s.init(\n",
                [s(IndentStr), s(GlobalVarNameStr)], !IO),
            output_nonempty_initializer_body_list_for_java(Info, Stream,
                Indent + 1u, FieldInits, "", !IO),
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
                GlobalVarNameStr),
            ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, string::in,
    mlds_initializer::in, int::in, int::out, io::di, io::uo) is det.

output_rtti_array_assignments_for_java(Info, Stream, Indent, GlobalVarNameStr,
        ElementInit, Index, Index + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s%s[%d] =\n",
        [s(IndentStr), s(GlobalVarNameStr), i(Index)],  !IO),
    output_initializer_body_for_java(Info, Stream, at_start_of_line,
        Indent + 1u, ElementInit, no, ";", !IO).

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
