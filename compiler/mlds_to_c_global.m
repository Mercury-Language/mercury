%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output the declarations and definitions of global data structures.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_global.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred mlds_output_scalar_cell_group_decls(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, string::in,
    assoc_list(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

:- pred mlds_output_scalar_cell_group_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, string::in,
    assoc_list(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_vector_cell_group_decls(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in, string::in,
    assoc_list(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

:- pred mlds_output_vector_cell_group_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, string::in,
    assoc_list(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_alloc_site_decls(io.text_output_stream::in, indent::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

:- pred mlds_output_alloc_site_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_global_var_decls(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

:- type maybe_blank_line_between_defns
    --->    no_blank_line_between_defns
    ;       blank_line_between_defns.

:- pred mlds_output_global_var_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, maybe_blank_line_between_defns::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_c_class.
:- import_module ml_backend.mlds_to_c_data.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_stmt.
:- import_module ml_backend.mlds_to_c_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mlds_output_scalar_cell_group_decls(_, _, _, _, [], !IO).
mlds_output_scalar_cell_group_decls(Opts, Stream, Indent,
        MangledModuleName, [CellGroup | CellGroups], !IO) :-
    mlds_output_scalar_cell_group_decl(Opts, Stream, Indent,
        MangledModuleName, CellGroup, !IO),
    mlds_output_scalar_cell_group_decls(Opts, Stream, Indent,
        MangledModuleName, CellGroups, !IO).

:- pred mlds_output_scalar_cell_group_decl(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, string::in,
    pair(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_scalar_cell_group_decl(Opts, Stream, Indent, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, InitArraySize,
        _Counter, _Members, Rows),
    ( if Type = mlds_mostly_generic_array_type(ElemTypes) then
        mlds_output_scalar_cell_group_struct_defn(Opts, Stream, Indent,
            MangledModuleName, TypeRawNum, ElemTypes, !IO)
    else
        true
    ),
    NumRows = cord.length(Rows),
    TypeNameStr = scalar_cell_group_type_and_name_to_string_for_c(Opts,
        MangledModuleName, TypeRawNum, Type, InitArraySize, NumRows),
    io.format(Stream, "\nstatic /* final */ const %s;\n",
        [s(TypeNameStr)], !IO).

:- func scalar_cell_group_type_and_name_to_string_for_c(mlds_to_c_opts,
    string, int, mlds_type, initializer_array_size, int) = string.

scalar_cell_group_type_and_name_to_string_for_c(Opts, MangledModuleName,
        TypeRawNum, Type, InitArraySize, NumRows) = Str :-
    ( if Type = mlds_mostly_generic_array_type(_) then
        string.format("struct %s_scalar_cell_group_%d %s_scalar_common_%d[%d]",
            [s(MangledModuleName), i(TypeRawNum),
            s(MangledModuleName), i(TypeRawNum), i(NumRows)], Str)
    else
        type_to_prefix_suffix_for_c(Opts, Type, InitArraySize,
            TypePrefix, TypeSuffix),
        string.format("%s %s_scalar_common_%d[%d]%s",
            [s(TypePrefix), s(MangledModuleName), i(TypeRawNum),
            i(NumRows), s(TypeSuffix)], Str)
    ).

%---------------------%

mlds_output_scalar_cell_group_defns(_, _, _, _, [], !IO).
mlds_output_scalar_cell_group_defns(Opts, Stream, Indent,
        MangledModuleName, [CellGroup | CellGroups], !IO) :-
    mlds_output_scalar_cell_group_defn(Opts, Stream, Indent,
        MangledModuleName, CellGroup, !IO),
    mlds_output_scalar_cell_group_defns(Opts, Stream, Indent,
        MangledModuleName, CellGroups, !IO).

:- pred mlds_output_scalar_cell_group_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, string::in,
    pair(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_scalar_cell_group_defn(Opts, Stream, Indent, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, InitArraySize,
        _Counter, _Members, RowCords),
    Rows = cord.list(RowCords),
    list.length(Rows, NumRows),
    IndentStr = indent2_string(Indent),
    TypeNameStr = scalar_cell_group_type_and_name_to_string_for_c(Opts,
        MangledModuleName, TypeRawNum, Type, InitArraySize, NumRows),
    io.format(Stream, "\n%sstatic /* final */ const %s = {\n",
        [s(IndentStr), s(TypeNameStr)], !IO),
    list.foldl2(mlds_output_cell(Opts, Stream, Indent + 1), Rows, 0, _, !IO),
    io.format(Stream, "%s};\n", [s(IndentStr)], !IO).

:- pred mlds_output_scalar_cell_group_struct_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, int::in, string::in, int::in,
    list(mlds_type)::in, io::di, io::uo) is det.

mlds_output_scalar_cell_group_struct_defn(Opts, Stream, Indent,
        MangledModuleName, TypeRawNum, ElemTypes, !IO) :-
    output_pragma_pack_push(Stream, !IO),
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sstruct %s_scalar_cell_group_%d {\n",
        [s(IndentStr), s(MangledModuleName), i(TypeRawNum)], !IO),
    list.foldl2(
        mlds_output_scalar_cell_group_struct_field(Opts, Stream, Indent + 1),
        ElemTypes, 1, _, !IO),
    io.format(Stream, "%s};\n", [s(IndentStr)], !IO),
    output_pragma_pack_pop(Stream, !IO).

:- pred mlds_output_scalar_cell_group_struct_field(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_type::in, int::in, int::out,
    io::di, io::uo) is det.

mlds_output_scalar_cell_group_struct_field(Opts, Stream, Indent, FieldType,
        Num, Num + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    ( if
        % Ensure double-word float, int64 and uint64 structure members
        % are word-aligned, not double-aligned.
        (
            FieldType = mlds_builtin_type_float,
            TypeName = "MR_Float_Aligned"
        ;
            FieldType = mlds_builtin_type_int(IntType),
            (
                IntType = int_type_int64,
                TypeName = "MR_Int64Aligned"
            ;
                IntType = int_type_uint64,
                TypeName = "MR_Uint64Aligned"
            )
        )
    then
        io.format(Stream, "%s%s f%d;\n",
            [s(IndentStr), s(TypeName), i(Num)], !IO)
    else
        type_to_prefix_suffix_for_c_no_size(Opts, FieldType,
            TypePrefix, TypeSuffix),
        expect(unify(TypeSuffix, ""), $pred, "TypeSuffix is not empty"),
        io.format(Stream, "%s%s f%d;\n",
            [s(IndentStr), s(TypePrefix), i(Num)], !IO)
    ).

%---------------------------------------------------------------------------%

mlds_output_vector_cell_group_decls(_, _, _, _, _, [], !IO).
mlds_output_vector_cell_group_decls(Opts, Stream, Indent,
        ModuleName, MangledModuleName, [CellGroup | CellGroups], !IO) :-
    mlds_output_vector_cell_group_decl(Opts, Stream, Indent,
        ModuleName, MangledModuleName, CellGroup, !IO),
    mlds_output_vector_cell_group_decls(Opts, Stream, Indent,
        ModuleName, MangledModuleName, CellGroups, !IO).

:- pred mlds_output_vector_cell_group_decl(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in, string::in,
    pair(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_vector_cell_group_decl(Opts, Stream, Indent,
        ModuleName, MangledModuleName, TypeNum - CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, StructDefn, _FieldNames,
        _NextRow, Rows),
    NumRows = cord.length(Rows),
    IndentStr = indent2_string(Indent),
    type_to_prefix_suffix_for_c(Opts, Type, no_size, TypePrefix, TypeSuffix),

    mlds_output_struct_defn(Opts, Stream, Indent, ModuleName, StructDefn, !IO),
    io.format(Stream,
        "\n%sstatic /* final */ const %s %s_vector_common_%d[%d]%s;\n",
        [s(IndentStr), s(TypePrefix),
        s(MangledModuleName), i(TypeRawNum), i(NumRows), s(TypeSuffix)], !IO).

%---------------------%

mlds_output_vector_cell_group_defns(_, _, _, _, [], !IO).
mlds_output_vector_cell_group_defns(Opts, Stream, Indent,
        MangledModuleName, [CellGroup | CellGroups], !IO) :-
    mlds_output_vector_cell_group_defn(Opts, Stream, Indent,
        MangledModuleName, CellGroup, !IO),
    mlds_output_vector_cell_group_defns(Opts, Stream, Indent,
        MangledModuleName, CellGroups, !IO).

:- pred mlds_output_vector_cell_group_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, string::in,
    pair(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_vector_cell_group_defn(Opts, Stream, Indent, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, _ClassDefn, _FieldNames,
        _NextRow, RowCords),
    Rows = cord.list(RowCords),
    list.length(Rows, NumRows),
    IndentStr = indent2_string(Indent),
    type_to_prefix_suffix_for_c(Opts, Type, no_size, TypePrefix, TypeSuffix),

    io.format(Stream,
        "\n%sstatic /* final */ const %s %s_vector_common_%d[%d]%s = {\n",
        [s(IndentStr), s(TypePrefix),
        s(MangledModuleName), i(TypeRawNum), i(NumRows), s(TypeSuffix)], !IO),
    list.foldl2(mlds_output_cell(Opts, Stream, Indent + 1), Rows, 0, _, !IO),
    io.format(Stream, "%s};\n", [s(IndentStr)], !IO).

:- pred mlds_output_cell(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_initializer::in, int::in, int::out,
    io::di, io::uo) is det.

mlds_output_cell(Opts, Stream, Indent, Initializer, !RowNum, !IO) :-
    IndentStr = indent2_string(Indent),
    ( if Initializer = init_struct(_, [_]) then
        EndChar = ' '
    else
        EndChar = '\n'
    ),
    ThisRowNum = !.RowNum,
    !:RowNum = !.RowNum + 1,
    io.format(Stream, "%s/* row %3d */%c",
        [s(IndentStr), i(ThisRowNum), c(EndChar)], !IO),
    mlds_output_initializer_body(Opts, Stream, Indent, Initializer, !IO),
    io.write_string(Stream, ",\n", !IO).

%---------------------------------------------------------------------------%

mlds_output_alloc_site_decls(Stream, Indent, AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        list.length(AllocSites, NumAllocSites),
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%sstatic MR_AllocSiteInfo MR_alloc_sites[%d];\n",
            [s(IndentStr), i(NumAllocSites)], !IO)
    ).

%---------------------%

mlds_output_alloc_site_defns(Opts, Stream, Indent, MLDS_ModuleName,
        AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        list.length(AllocSites, NumAllocSites),
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%sstatic MR_AllocSiteInfo MR_alloc_sites[%d] = {\n",
            [s(IndentStr), i(NumAllocSites)], !IO),
        list.foldl(
            mlds_output_alloc_site_defn(Opts, Stream, Indent + 1,
                MLDS_ModuleName),
            AllocSites, !IO),
        io.format(Stream, "%s};\n", [s(IndentStr)], !IO)
    ).

:- pred mlds_output_alloc_site_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    pair(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

mlds_output_alloc_site_defn(_Opts, Stream, Indent, MLDS_ModuleName,
        _AllocId - AllocData, !IO) :-
    AllocData = ml_alloc_site_data(FuncName, Context, Type, Size),
    QualFuncName = qual_function_name(MLDS_ModuleName, FuncName),
    QualFuncNameStr = qual_function_name_to_string_for_c(QualFuncName),
    TypeStr = quote_string_c(Type),
    Context = context(FileName, LineNumber),
    FileNameStr = quote_string_c(FileName),
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s{ %s, %s, %d, %s, %d },\n",
        [s(IndentStr), s(QualFuncNameStr), s(FileNameStr), i(LineNumber),
        s(TypeStr), i(Size)], !IO).

%---------------------------------------------------------------------------%

mlds_output_global_var_decls(_, _, _, _, [], !IO).
mlds_output_global_var_decls(Opts, Stream, Indent, ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    io.nl(Stream, !IO),
    mlds_output_global_var_decl(Opts, Stream, Indent,
        ModuleName, GlobalVarDefn, !IO),
    mlds_output_global_var_decls(Opts, Stream, Indent, ModuleName,
        GlobalVarDefns, !IO).

:- pred mlds_output_global_var_decl(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    mlds_global_var_defn::in, io::di, io::uo) is det.

mlds_output_global_var_decl(Opts, Stream, Indent, MLDS_ModuleName,
        GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, Context, Flags,
        Type, Initializer, _GCStmt),
    IndentStr = indent2_string(Indent),
    FlagsPrefix = global_var_decl_flags_to_prefix(forward_decl, Flags),
    TypeNameStr = global_var_decl_to_type_name_string(Opts, MLDS_ModuleName,
        GlobalVarName, Type, get_initializer_array_size(Initializer)),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s%s%s;\n",
        [s(IndentStr), s(FlagsPrefix), s(TypeNameStr)], !IO).

:- func global_var_decl_to_type_name_string(mlds_to_c_opts, mlds_module_name,
    mlds_global_var_name, mlds_type, initializer_array_size) = string.

global_var_decl_to_type_name_string(Opts, MLDS_ModuleName, GlobalVarName,
        Type, InitializerSize) = DeclStr :-
    type_to_prefix_suffix_for_c(Opts, Type, InitializerSize,
        TypePrefix, TypeSuffix),
    QualGlobalVarNameStr = maybe_qual_global_var_name_to_string_for_c(
        MLDS_ModuleName, GlobalVarName),
    string.format("%s %s%s",
        [s(TypePrefix), s(QualGlobalVarNameStr), s(TypeSuffix)], DeclStr).

%---------------------%

mlds_output_global_var_defns(_, _, _, _, _, [], !IO).
mlds_output_global_var_defns(Opts, Stream, Indent, BlankLine, ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    mlds_output_global_var_defn(Opts, Stream, Indent, BlankLine, ModuleName,
        GlobalVarDefn, !IO),
    mlds_output_global_var_defns(Opts, Stream, Indent, BlankLine, ModuleName,
        GlobalVarDefns, !IO).

:- pred mlds_output_global_var_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, maybe_blank_line_between_defns::in,
    mlds_module_name::in, mlds_global_var_defn::in, io::di, io::uo) is det.

mlds_output_global_var_defn(Opts, Stream, Indent, BlankLine,
        MLDS_ModuleName, GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, Context, Flags,
        Type, Initializer, GCStmt),
    Flags = mlds_global_var_decl_flags(Access, _Constness),
    ShouldModuleQual = should_module_qualify_global_var_name(GlobalVarName),
    ( if
        Access = gvar_acc_whole_program,
        ShouldModuleQual = no,
        % Some rtti variables are supposed to be exported without being module
        % qualified.
        GlobalVarName \= gvn_rtti_var(_)
    then
        unexpected($pred,
            "whole-program visible global var is not module qualified")
    else
        true
    ),
    (
        BlankLine = blank_line_between_defns,
        io.nl(Stream, !IO)
    ;
        BlankLine = no_blank_line_between_defns
    ),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    IndentStr = indent2_string(Indent),
    FlagsPrefix = global_var_decl_flags_to_prefix(definition, Flags),
    TypeNameStr = global_var_decl_to_type_name_string(Opts, MLDS_ModuleName,
        GlobalVarName, Type, get_initializer_array_size(Initializer)),
    io.format(Stream, "%s%s%s",
        [s(IndentStr), s(FlagsPrefix), s(TypeNameStr)], !IO),
    mlds_output_initializer(Opts, Stream, Type, Initializer, !IO),
    io.write_string(Stream, ";\n", !IO),
    mlds_output_gc_statement(Opts, Stream, Indent, GCStmt, "", !IO).

%---------------------------------------------------------------------------%

:- func global_var_decl_flags_to_prefix(decl_or_defn,
    mlds_global_var_decl_flags) = string.

global_var_decl_flags_to_prefix(DeclOrDefn, Flags) = Prefix :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    AccessPrefix = global_var_extern_or_static_prefix(DeclOrDefn, Access),
    ConstnessPrefix = constness_prefix(Constness),
    Prefix = AccessPrefix ++ ConstnessPrefix.

    % mlds_output_global_var_extern_or_static does for global variables
    % what mlds_output_extern_or_static does for other entities.
    %
:- func global_var_extern_or_static_prefix(decl_or_defn, global_var_access)
    = string.

global_var_extern_or_static_prefix(DeclOrDefn, Access) = Prefix :-
    (
        Access = gvar_acc_module_only,
        Prefix = "static "
    ;
        Access = gvar_acc_whole_program,
        (
            DeclOrDefn = forward_decl,
            Prefix = "extern "
        ;
            DeclOrDefn = definition,
            % Print no storage class.
            Prefix = ""
        )
    ).

:- func constness_prefix(constness) = string.

constness_prefix(const) = "const ".
constness_prefix(modifiable) = "".

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_global.
%---------------------------------------------------------------------------%
