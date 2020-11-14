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

:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred mlds_output_scalar_cell_group_decls(mlds_to_c_opts::in, indent::in,
    string::in,
    assoc_list(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

:- pred mlds_output_scalar_cell_group_defns(mlds_to_c_opts::in, indent::in,
    string::in,
    assoc_list(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_vector_cell_group_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, string::in,
    assoc_list(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

:- pred mlds_output_vector_cell_group_defns(mlds_to_c_opts::in, indent::in,
    string::in,
    assoc_list(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_alloc_site_decls(indent::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

:- pred mlds_output_alloc_site_defns(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, assoc_list(mlds_alloc_id, ml_alloc_site_data)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_global_var_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

:- pred mlds_output_global_var_defns(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_c_data.
:- import_module ml_backend.mlds_to_c_class.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_stmt.
:- import_module ml_backend.mlds_to_c_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mlds_output_scalar_cell_group_decls(_Opts, _Indent, _MangledModuleName,
        [], !IO).
mlds_output_scalar_cell_group_decls(Opts, Indent, MangledModuleName,
        [CellGroup | CellGroups], !IO) :-
    mlds_output_scalar_cell_group_decl(Opts, Indent, MangledModuleName,
        CellGroup, !IO),
    mlds_output_scalar_cell_group_decls(Opts, Indent, MangledModuleName,
        CellGroups, !IO).

:- pred mlds_output_scalar_cell_group_decl(mlds_to_c_opts::in, indent::in,
    string::in, pair(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_scalar_cell_group_decl(Opts, Indent, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, InitArraySize,
        _Counter, _Members, Rows),

    ( if Type = mlds_mostly_generic_array_type(ElemTypes) then
        mlds_output_scalar_cell_group_struct_defn(Opts, Indent,
            MangledModuleName, TypeRawNum, ElemTypes, !IO)
    else
        true
    ),

    output_n_indents(Indent, !IO),
    io.write_string("\nstatic /* final */ const ", !IO),
    NumRows = cord.length(Rows),
    mlds_output_scalar_cell_group_type_and_name(Opts, MangledModuleName,
        TypeRawNum, Type, InitArraySize, NumRows, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_scalar_cell_group_type_and_name(mlds_to_c_opts::in,
    string::in, int::in, mlds_type::in, initializer_array_size::in, int::in,
    io::di, io::uo) is det.

mlds_output_scalar_cell_group_type_and_name(Opts, MangledModuleName,
        TypeRawNum, Type, InitArraySize, NumRows, !IO) :-
    ( if Type = mlds_mostly_generic_array_type(_) then
        io.format("struct %s_scalar_cell_group_%d",
            [s(MangledModuleName), i(TypeRawNum)], !IO)
    else
        mlds_output_type_prefix(Opts, Type, !IO)
    ),
    io.format(" %s_scalar_common_%d[%d]",
        [s(MangledModuleName), i(TypeRawNum), i(NumRows)], !IO),
    ( if Type = mlds_mostly_generic_array_type(_) then
        true
    else
        mlds_output_type_suffix(Opts, Type, InitArraySize, !IO)
    ).

%---------------------%

mlds_output_scalar_cell_group_defns(_Opts, _Indent, _MangledModuleName,
        [], !IO).
mlds_output_scalar_cell_group_defns(Opts, Indent, MangledModuleName,
        [CellGroup | CellGroups], !IO) :-
    mlds_output_scalar_cell_group_defn(Opts, Indent, MangledModuleName,
        CellGroup, !IO),
    mlds_output_scalar_cell_group_defns(Opts, Indent, MangledModuleName,
        CellGroups, !IO).

:- pred mlds_output_scalar_cell_group_defn(mlds_to_c_opts::in, indent::in,
    string::in, pair(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_scalar_cell_group_defn(Opts, Indent, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, InitArraySize,
        _Counter, _Members, RowCords),
    Rows = cord.list(RowCords),
    list.length(Rows, NumRows),
    output_n_indents(Indent, !IO),
    io.write_string("\nstatic /* final */ const ", !IO),
    mlds_output_scalar_cell_group_type_and_name(Opts, MangledModuleName,
        TypeRawNum, Type, InitArraySize, NumRows, !IO),
    io.write_string(" = {\n", !IO),
    list.foldl2(mlds_output_cell(Opts, Indent + 1), Rows, 0, _, !IO),
    io.write_string("};\n", !IO).

:- pred mlds_output_scalar_cell_group_struct_defn(mlds_to_c_opts::in, int::in,
    string::in, int::in, list(mlds_type)::in, io::di, io::uo) is det.

mlds_output_scalar_cell_group_struct_defn(Opts, Indent, MangledModuleName,
        TypeRawNum, ElemTypes, !IO) :-
    output_pragma_pack_push_cur_stream(!IO),
    io.format("struct %s_scalar_cell_group_%d {\n",
        [s(MangledModuleName), i(TypeRawNum)], !IO),
    list.foldl2(mlds_output_scalar_cell_group_struct_field(Opts, Indent + 1),
        ElemTypes, 1, _, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("};\n", !IO),
    output_pragma_pack_pop_cur_stream(!IO).

:- pred mlds_output_scalar_cell_group_struct_field(mlds_to_c_opts::in,
    indent::in, mlds_type::in, int::in, int::out, io::di, io::uo) is det.

mlds_output_scalar_cell_group_struct_field(Opts, Indent, FieldType,
        Num, Num + 1, !IO) :-
    output_n_indents(Indent, !IO),
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
        io.write_string(TypeName, !IO)
    else
        mlds_output_type_prefix(Opts, FieldType, !IO)
    ),
    io.format(" f%d;\n", [i(Num)], !IO).

%---------------------------------------------------------------------------%

mlds_output_vector_cell_group_decls(_Opts, _Indent,
        _ModuleName, _MangledModuleName, [], !IO).
mlds_output_vector_cell_group_decls(Opts, Indent,
        ModuleName, MangledModuleName, [CellGroup | CellGroups], !IO) :-
    mlds_output_vector_cell_group_decl(Opts, Indent,
        ModuleName, MangledModuleName, CellGroup, !IO),
    mlds_output_vector_cell_group_decls(Opts, Indent,
        ModuleName, MangledModuleName, CellGroups, !IO).

:- pred mlds_output_vector_cell_group_decl(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, string::in,
    pair(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_vector_cell_group_decl(Opts, Indent, ModuleName, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, ClassDefn, _FieldNames,
        _NextRow, Rows),
    mlds_output_class_defn(Opts, Indent, ModuleName, ClassDefn, !IO),

    output_n_indents(Indent, !IO),
    io.write_string("\nstatic /* final */ const ", !IO),
    mlds_output_type_prefix(Opts, Type, !IO),
    NumRows = cord.length(Rows),
    io.format(" %s_vector_common_%d[%d]",
        [s(MangledModuleName), i(TypeRawNum), i(NumRows)], !IO),
    mlds_output_type_suffix(Opts, Type, no_size, !IO),
    io.write_string(";\n", !IO).

%---------------------%

mlds_output_vector_cell_group_defns(_Opts, _Indent, _MangledModuleName,
        [], !IO).
mlds_output_vector_cell_group_defns(Opts, Indent, MangledModuleName,
        [CellGroup | CellGroups], !IO) :-
    mlds_output_vector_cell_group_defn(Opts, Indent, MangledModuleName,
        CellGroup, !IO),
    mlds_output_vector_cell_group_defns(Opts, Indent, MangledModuleName,
        CellGroups, !IO).

:- pred mlds_output_vector_cell_group_defn(mlds_to_c_opts::in, indent::in,
    string::in, pair(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

mlds_output_vector_cell_group_defn(Opts, Indent, MangledModuleName,
        TypeNum - CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, _ClassDefn, _FieldNames,
        _NextRow, RowCords),
    Rows = cord.list(RowCords),
    list.length(Rows, NumRows),
    output_n_indents(Indent, !IO),
    io.write_string("\nstatic /* final */ const ", !IO),
    mlds_output_type_prefix(Opts, Type, !IO),
    io.format(" %s_vector_common_%d[%d]",
        [s(MangledModuleName), i(TypeRawNum), i(NumRows)], !IO),
    mlds_output_type_suffix(Opts, Type, no_size, !IO),
    io.write_string(" = {\n", !IO),
    list.foldl2(mlds_output_cell(Opts, Indent + 1), Rows, 0, _, !IO),
    io.write_string("};\n", !IO).

:- pred mlds_output_cell(mlds_to_c_opts::in, indent::in, mlds_initializer::in,
    int::in, int::out, io::di, io::uo) is det.

mlds_output_cell(Opts, Indent, Initializer, !RowNum, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("/* row ", !IO),
    io.write_int(!.RowNum, !IO),
    io.write_string(" */", !IO),
    ( if Initializer = init_struct(_, [_]) then
        io.write_char(' ', !IO)
    else
        io.nl(!IO)
    ),
    !:RowNum = !.RowNum + 1,
    mlds_output_initializer_body(Opts, Indent, Initializer, !IO),
    io.write_string(",\n", !IO).

%---------------------------------------------------------------------------%

mlds_output_alloc_site_decls(Indent, AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        list.length(AllocSites, NumAllocSites),
        output_n_indents(Indent, !IO),
        io.format("static MR_AllocSiteInfo MR_alloc_sites[%d];\n",
            [i(NumAllocSites)], !IO)
    ).

%---------------------%

mlds_output_alloc_site_defns(Opts, Indent, MLDS_ModuleName, AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        output_n_indents(Indent, !IO),
        list.length(AllocSites, NumAllocSites),
        io.format("static MR_AllocSiteInfo MR_alloc_sites[%d] = {\n",
            [i(NumAllocSites)], !IO),
        list.foldl(
            mlds_output_alloc_site_defn(Opts, Indent + 1, MLDS_ModuleName),
            AllocSites, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("};\n", !IO)
    ).

:- pred mlds_output_alloc_site_defn(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, pair(mlds_alloc_id, ml_alloc_site_data)::in,
    io::di, io::uo) is det.

mlds_output_alloc_site_defn(_Opts, Indent, MLDS_ModuleName,
        _AllocId - AllocData, !IO) :-
    AllocData = ml_alloc_site_data(FuncName, Context, Type, Size),
    QualFuncName = qual_function_name(MLDS_ModuleName, FuncName),
    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    output_n_indents(Indent, !IO),
    io.write_string("{ ", !IO),
    mlds_output_fully_qualified_function_name(QualFuncName, !IO),
    io.write_string(", """, !IO),
    c_util.output_quoted_string_cur_stream(FileName, !IO),
    io.write_string(""", ", !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(", """, !IO),
    c_util.output_quoted_string_cur_stream(Type, !IO),
    io.write_string(""", ", !IO),
    io.write_int(Size, !IO),
    io.write_string("},\n", !IO).

%---------------------------------------------------------------------------%

mlds_output_global_var_decls(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_global_var_decls(Opts, Indent, ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    io.nl(!IO),
    mlds_output_global_var_decl_opts(Opts, Indent, ModuleName, GlobalVarDefn,
        !IO),
    mlds_output_global_var_decls(Opts, Indent, ModuleName, GlobalVarDefns,
        !IO).

:- pred mlds_output_global_var_decl_opts(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_global_var_defn::in, io::di, io::uo) is det.

mlds_output_global_var_decl_opts(Opts, Indent, MLDS_ModuleName,
        GlobalVarDefn, !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, Context, Flags,
        Type, Initializer, _GCStmt),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_global_var_decl_flags(Flags, forward_decl, !IO),
    mlds_output_global_var_decl(Opts, MLDS_ModuleName, GlobalVarName, Type,
        get_initializer_array_size(Initializer), !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_global_var_decl(mlds_to_c_opts::in,
    mlds_module_name::in, mlds_global_var_name::in, mlds_type::in,
    initializer_array_size::in, io::di, io::uo) is det.

mlds_output_global_var_decl(Opts, MLDS_ModuleName, GlobalVarName, Type,
        InitializerSize, !IO) :-
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName,
        GlobalVarName, !IO),
    mlds_output_type_suffix(Opts, Type, InitializerSize, !IO).

:- pred mlds_output_global_var_decl_flags(mlds_global_var_decl_flags::in,
    decl_or_defn::in, io::di, io::uo) is det.

mlds_output_global_var_decl_flags(Flags, DeclOrDefn, !IO) :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    % Everything that one may want to know about Flags is available
    % in the output of the next two calls, so printing comments is not useful.
    mlds_output_global_var_extern_or_static(Access, DeclOrDefn, !IO),
    mlds_output_constness(Constness, !IO).

    % mlds_output_global_var_extern_or_static does for global variables
    % what mlds_output_extern_or_static does for other entities.
    %
:- pred mlds_output_global_var_extern_or_static(global_var_access::in,
    decl_or_defn::in, io::di, io::uo) is det.

mlds_output_global_var_extern_or_static(Access, DeclOrDefn, !IO) :-
    (
        Access = gvar_acc_module_only,
        io.write_string("static ", !IO)
    ;
        Access = gvar_acc_whole_program,
        (
            DeclOrDefn = forward_decl,
            io.write_string("extern ", !IO)
        ;
            DeclOrDefn = definition
            % Print no storage class.
        )
    ).

:- pred mlds_output_constness(constness::in, io::di, io::uo) is det.

mlds_output_constness(const, !IO) :-
    io.write_string("const ", !IO).
mlds_output_constness(modifiable, !IO).

%---------------------%

mlds_output_global_var_defns(_Opts, _Indent, _Separate, _ModuleName, [], !IO).
mlds_output_global_var_defns(Opts, Indent, Separate, ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    mlds_output_global_var_defn(Opts, Indent, Separate, ModuleName,
        GlobalVarDefn, !IO),
    mlds_output_global_var_defns(Opts, Indent, Separate, ModuleName,
        GlobalVarDefns, !IO).

:- pred mlds_output_global_var_defn(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_module_name::in, mlds_global_var_defn::in, io::di, io::uo) is det.

mlds_output_global_var_defn(Opts, Indent, Separate,
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
        Separate = yes,
        io.nl(!IO)
    ;
        Separate = no
    ),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_global_var_decl_flags(Flags, definition, !IO),
    mlds_output_global_var_decl(Opts, MLDS_ModuleName, GlobalVarName, Type,
        get_initializer_array_size(Initializer), !IO),
    mlds_output_initializer(Opts, Type, Initializer, !IO),
    io.write_string(";\n", !IO),
    mlds_output_gc_statement(Opts, Indent, GCStmt, "", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_global.
%---------------------------------------------------------------------------%
