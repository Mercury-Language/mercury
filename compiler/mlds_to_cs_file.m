%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mlds_to_cs_file.m.
% Main authors: wangp.
%
% Convert MLDS to C# code.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_file.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.maybe_succeeded.
:- import_module ml_backend.mlds.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_csharp_mlds(module_info::in, mlds::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_cs_class.
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_export.
:- import_module ml_backend.mlds_to_cs_func.
:- import_module ml_backend.mlds_to_cs_global.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

output_csharp_mlds(ModuleInfo, MLDS, Succeeded, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".cs")), ModuleName, SourceFileName, !IO),
    Indent = 0,
    output_to_file_stream(Globals, ModuleName, SourceFileName,
        output_csharp_src_file(ModuleInfo, Indent, MLDS), Succeeded, !IO).

:- pred output_csharp_src_file(module_info::in, indent::in, mlds::in,
    io.text_output_stream::in, list(string)::out, io::di, io::uo) is det.

output_csharp_src_file(ModuleInfo, Indent, MLDS, Stream, Errors, !IO) :-
    % Run further transformations on the MLDS.
    MLDS = mlds(ModuleName, Imports, GlobalData,
        TypeDefns, TableStructDefns, ProcDefns,
        InitPreds, FinalPreds, AllForeignCode, ExportedEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, _AllocIdMap,
        RttiDefns, CellDefns, ClosureWrapperFuncDefns),

    % Find all methods which would have their addresses taken to be used as a
    % function pointer.
    some [!CodeAddrsInConsts] (
        !:CodeAddrsInConsts = init_code_addrs_in_consts,
        method_ptrs_in_global_var_defns(RttiDefns, !CodeAddrsInConsts),
        method_ptrs_in_global_var_defns(CellDefns, !CodeAddrsInConsts),
        method_ptrs_in_global_var_defns(TableStructDefns, !CodeAddrsInConsts),
        method_ptrs_in_function_defns(ClosureWrapperFuncDefns,
            !CodeAddrsInConsts),
        method_ptrs_in_function_defns(ProcDefns, !CodeAddrsInConsts),

        map.values(ScalarCellGroupMap, ScalarCellGroups),
        ScalarCellRows = list.map(func(G) = G ^ mscg_rows, ScalarCellGroups),
        list.foldl(method_ptrs_in_scalars, ScalarCellRows, !CodeAddrsInConsts),
        !.CodeAddrsInConsts = code_addrs_in_consts(_, _, RevSeqNumCodeAddrs),

        make_code_addr_map_for_csharp(RevSeqNumCodeAddrs, map.init, CodeAddrs)
    ),

    % Get the foreign code for C#.
    % XXX We should not ignore _Imports.
    ForeignCode = mlds_get_csharp_foreign_code(AllForeignCode),
    ForeignCode = mlds_foreign_code(ForeignDeclCodes, ForeignBodyCodes,
        _Imports, ExportDefns),

    % Output transformed MLDS as C# source.
    module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
    Info = init_csharp_out_info(ModuleInfo, SourceFileName, CodeAddrs),
    output_src_start_for_csharp(Info, Stream, Indent, ModuleName, Imports,
        ForeignDeclCodes, ProcDefns, ForeignDeclErrors, !IO),
    list.map_foldl(output_csharp_body_code(Info, Stream, Indent),
        ForeignBodyCodes, ForeignCodeResults, !IO),
    list.filter_map(maybe_is_error, ForeignCodeResults, ForeignCodeErrors),

    output_pragma_warning_disable(Stream, !IO),

    io.write_string(Stream, "\n// RttiDefns\n", !IO),
    list.foldl(
        output_global_var_defn_for_csharp(Info, Stream, Indent + 1,
            oa_alloc_only),
        RttiDefns, !IO),
    output_rtti_assignments_for_csharp(Info, Stream, Indent + 1,
        RttiDefns, !IO),

    io.write_string(Stream, "\n// Cell and tabling definitions\n", !IO),
    output_global_var_decls_for_csharp(Info, Stream, Indent + 1,
        CellDefns, !IO),
    output_global_var_decls_for_csharp(Info, Stream, Indent + 1,
        TableStructDefns, !IO),
    output_init_global_var_method_for_csharp(Info, Stream, Indent + 1,
        CellDefns ++ TableStructDefns, !IO),

    % Scalar common data must appear after the previous data definitions,
    % and the vector common data after that.
    io.write_string(Stream, "\n// Scalar common data\n", !IO),
    output_scalar_common_data_for_csharp(Info, Stream, Indent + 1,
        ScalarCellGroupMap, !IO),

    io.write_string(Stream, "\n// Vector common data\n", !IO),
    output_vector_common_data_for_csharp(Info, Stream, Indent + 1,
        VectorCellGroupMap, !IO),

    io.write_string(Stream, "\n// Method pointers\n", !IO),
    output_method_ptr_constants(Info, Stream, Indent + 1, CodeAddrs, !IO),

    io.write_string(Stream, "\n// Function definitions\n", !IO),
    list.sort(ClosureWrapperFuncDefns ++ ProcDefns, SortedFuncDefns),
    list.foldl(
        output_function_defn_for_csharp(Info, Stream, Indent + 1, oa_none),
        SortedFuncDefns, !IO),

    io.write_string(Stream, "\n// Class definitions\n", !IO),
    list.sort(TypeDefns, SortedClassDefns),
    list.foldl(output_class_defn_for_csharp(Info, Stream, Indent + 1),
        SortedClassDefns, !IO),

    io.write_string(Stream, "\n// ExportDefns\n", !IO),
    output_exports_for_csharp(Info, Stream, Indent + 1, ExportDefns, !IO),

    io.write_string(Stream, "\n// ExportedEnums\n", !IO),
    output_exported_enums_for_csharp(Info, Stream, Indent + 1,
        ExportedEnums, !IO),

    io.write_string(Stream, "\n// EnvVarNames\n", !IO),
    set.init(EnvVarNamesSet0),
    list.foldl(accumulate_env_var_names, ProcDefns,
        EnvVarNamesSet0, EnvVarNamesSet1),
    list.foldl(accumulate_env_var_names, ClosureWrapperFuncDefns,
        EnvVarNamesSet1, EnvVarNamesSet),
    set.foldl(output_env_var_definition_for_csharp(Stream, Indent + 1),
        EnvVarNamesSet, !IO),

    StaticCtorCalls = [
        "MR_init_rtti",
        "MR_init_data",
        "MR_init_scalar_common_data",
        "MR_init_vector_common_data"
        | InitPreds
    ],
    output_static_constructor(Stream, ModuleName, Indent + 1, StaticCtorCalls,
        FinalPreds, !IO),

    output_src_end_for_csharp(Stream, Indent, ModuleName, !IO),

    Errors = ForeignDeclErrors ++ ForeignCodeErrors.

:- pred make_code_addr_map_for_csharp(assoc_list(int, mlds_code_addr)::in,
    map(mlds_code_addr, string)::in, map(mlds_code_addr, string)::out) is det.

make_code_addr_map_for_csharp([], !CodeAddrMap).
make_code_addr_map_for_csharp([SeqNum - CodeAddr | SeqNumsCodeAddrs],
        !CodeAddrMap) :-
    Name = "MR_method_ptr_" ++ string.from_int(SeqNum),
    map.det_insert(CodeAddr, Name, !CodeAddrMap),
    make_code_addr_map_for_csharp(SeqNumsCodeAddrs, !CodeAddrMap).

%---------------------------------------------------------------------------%
%
% Code for working with `foreign_code'.
%

:- pred output_csharp_decl(csharp_out_info::in, io.text_output_stream::in,
    indent::in, foreign_decl_code::in, maybe_error::out,
    io::di, io::uo) is det.

output_csharp_decl(Info, Stream, Indent, DeclCode, Res, !IO) :-
    DeclCode = foreign_decl_code(Lang, _IsLocal, LiteralOrInclude, Context),
    (
        Lang = lang_csharp,
        output_csharp_foreign_literal_or_include(Info, Stream, Indent,
            LiteralOrInclude, Context, Res, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ),
        sorry($pred, "foreign decl other than C#")
    ).

:- pred output_csharp_body_code(csharp_out_info::in, io.text_output_stream::in,
    indent::in, foreign_body_code::in, maybe_error::out,
    io::di, io::uo) is det.

output_csharp_body_code(Info, Stream, Indent, ForeignBodyCode, Res, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    % Only output C# code.
    (
        Lang = lang_csharp,
        output_csharp_foreign_literal_or_include(Info, Stream, Indent,
            LiteralOrInclude, Context, Res, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ),
        sorry($pred, "foreign code other than C#")
    ).

:- pred output_csharp_foreign_literal_or_include(csharp_out_info::in,
    io.text_output_stream::in, indent::in, foreign_literal_or_include::in,
    prog_context::in, maybe_error::out, io::di, io::uo) is det.

output_csharp_foreign_literal_or_include(Info, Stream, Indent,
        LiteralOrInclude, Context, Res, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        indent_line_after_context(Stream, Info ^ csoi_foreign_line_numbers,
            Context, Indent, !IO),
        io.write_string(Stream, Code, !IO),
        Res = ok
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        SourceFileName = Info ^ csoi_source_filename,
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        cs_output_context(Stream, Info ^ csoi_foreign_line_numbers,
            context(IncludePath, 1), !IO),
        write_include_file_contents(Stream, IncludePath, Res, !IO)
    ),
    io.nl(Stream, !IO),
    cs_output_default_context(Stream, Info ^ csoi_foreign_line_numbers, !IO).

:- func mlds_get_csharp_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_csharp_foreign_code(AllForeignCode) = ForeignCode :-
    ( if map.search(AllForeignCode, lang_csharp, ForeignCode0) then
        ForeignCode = ForeignCode0
    else
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%---------------------------------------------------------------------------%

:- pred output_method_ptr_constants(csharp_out_info::in,
    io.text_output_stream::in, indent::in, map(mlds_code_addr, string)::in,
    io::di, io::uo) is det.

output_method_ptr_constants(Info, Stream, Indent, CodeAddrs, !IO) :-
    map.foldl(output_method_ptr_constant(Info, Stream, Indent),
        CodeAddrs, !IO).

:- pred output_method_ptr_constant(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_code_addr::in, string::in,
    io::di, io::uo) is det.

output_method_ptr_constant(Info, Stream, Indent, CodeAddr, Name, !IO) :-
    CodeAddr = mlds_code_addr(_QualFuncLabel, Signature),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    TypeString = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "private static readonly %s %s = ",
        [s(TypeString), s(Name)], !IO),
    IsCall = no,
    mlds_output_code_addr_for_csharp(Info, Stream, CodeAddr, IsCall, !IO),
    io.write_string(Stream, ";\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output globals for environment variables.
%

:- pred output_env_var_definition_for_csharp(io.text_output_stream::in,
    indent::in, string::in, io::di, io::uo) is det.

output_env_var_definition_for_csharp(Stream, Indent, EnvVarName, !IO) :-
    % We use int because the generated code compares against zero, and changing
    % that is more trouble than it is worth, as it affects the C backends.
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "private static int mercury_envvar_%s =\n",
        [s(EnvVarName)], !IO),
    output_n_indents(Stream, Indent + 1, !IO),
    io.format(Stream,
        "System.Environment.GetEnvironmentVariable(\"%s\") == null ? 0 : 1;\n",
        [s(EnvVarName)], !IO).

%---------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mercury_module_name::in,
    list(mlds_import)::in, list(foreign_decl_code)::in,
    list(mlds_function_defn)::in, list(string)::out, io::di, io::uo) is det.

output_src_start_for_csharp(Info, Stream, Indent, MercuryModuleName, _Imports,
        ForeignDecls, Defns, Errors, !IO) :-
    output_auto_gen_comment(Stream, Info ^ csoi_source_filename, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "/* :- module ", !IO),
    prog_out.write_sym_name(Stream, MercuryModuleName, !IO),
    io.write_string(Stream, ". */\n", !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "namespace mercury {\n\n", !IO),

    list.map_foldl(output_csharp_decl(Info, Stream, Indent),
        ForeignDecls, ForeignDeclResults, !IO),
    list.filter_map(maybe_is_error, ForeignDeclResults, Errors),

    mangle_sym_name_for_csharp(MercuryModuleName, module_qual, "__",
        ClassName),
    io.format(Stream, "public static class %s {\n", [s(ClassName)], !IO),

    % Check whether this module contains a `main' predicate, and if it does,
    % then generate a `main' method that calls the `main' predicate.
    ( if func_defns_contain_main(Defns) then
        write_main_driver_for_csharp(Stream, Indent + 1, ClassName, !IO)
    else
        true
    ).

    % C# only allows a single static constructor so we just call the real
    % methods that we generated earlier.
    %
:- pred output_static_constructor(io.text_output_stream::in,
    mercury_module_name::in, indent::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

output_static_constructor(Stream, MercuryModuleName, Indent,
        StaticConstructors, FinalPreds, !IO) :-
    mangle_sym_name_for_csharp(MercuryModuleName, module_qual, "__",
        ClassName),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "static %s() {\n", [s(ClassName)], !IO),
    WriteCall =
        ( pred(MethodName::in, !.IO::di, !:IO::uo) is det :-
            output_n_indents(Stream, Indent + 1, !IO),
            io.format(Stream, "%s();\n", [s(MethodName)], !IO)
        ),
    list.foldl(WriteCall, StaticConstructors, !IO),
    WriteFinal =
        ( pred(FinalPred::in, !.IO::di, !:IO::uo) is det :-
            output_n_indents(Stream, Indent + 1, !IO),
            list.foldl(io.write_string(Stream), [
                "System.AppDomain.CurrentDomain.ProcessExit += ",
                "(sender, ev) => ", FinalPred, "();\n"
            ], !IO)
        ),
    list.foldl(WriteFinal, FinalPreds, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred write_main_driver_for_csharp(io.text_output_stream::in, indent::in,
    string::in, io::di, io::uo) is det.

write_main_driver_for_csharp(Stream, Indent, ClassName, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "public static void Main(string[] args)\n", !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "{\n", !IO),
    Body = [
        "try {",
        "   library.ML_std_library_init();",
        "   " ++ ClassName ++ ".main_2_p_0();",
        "} catch (runtime.Exception e) {",
        "   exception.ML_report_uncaught_exception(",
        "       (univ.Univ_0) e.exception);",
        "   if (System.Environment.GetEnvironmentVariable(",
        "           ""MERCURY_SUPPRESS_STACK_TRACE"") == null) {",
        "       System.Console.Error.WriteLine(e.StackTrace);",
        "   }",
        "   if (System.Environment.ExitCode == 0) {",
        "       System.Environment.ExitCode = 1;",
        "   }",
        "}"
    ],
    list.foldl(write_indented_line(Stream, Indent + 1), Body, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred output_src_end_for_csharp(io.text_output_stream::in, indent::in,
    mercury_module_name::in, io::di, io::uo) is det.

output_src_end_for_csharp(Stream, Indent, ModuleName, !IO) :-
    io.write_string(Stream, "}\n\n", !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n", !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "// :- end_module ", !IO),
    prog_out.write_sym_name(Stream, ModuleName, !IO),
    io.write_string(Stream, ".\n", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_file.
%---------------------------------------------------------------------------%
