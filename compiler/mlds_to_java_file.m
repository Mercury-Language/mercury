%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mlds_to_java_file.m.
% Main authors: juliensf, mjwybrow, fjh, wangp.
%
% Convert MLDS to Java code.
%
% DONE:
%   det and semidet predicates
%   multiple output arguments
%   boxing and unboxing
%   conjunctions
%   disjunctions
%   if-then-else's
%   enumerations
%   discriminated unions
%   higher order functions
%   multidet and nondet predicates
%   test tests/benchmarks/*.m
%   generate optimized tailcalls
%   RTTI generation
%   handle foreign code written in Java
%   Support for Java in mmc --make
%   Support for nested modules
%
% TODO:
% - Support nested modules
%   (The problem with current code generation scheme for nested modules
%   is that Java does not allow the name of a class to be the same
%   as the name of its enclosing package. That should work now, but
%   javac doesn't like the filenames we give for submodules.)
%
% - Generate names of classes etc. correctly.
%
% - General code cleanup
%
% - handle static ground terms(?)
%
% - support foreign_import_module for Java
%
% - handle foreign code written in C
%
% NOTES:
% To avoid namespace conflicts all Java names must be fully qualified,
% e.g. The classname `String' must be qualified as `java.lang.String'
% to avoid conflicting with `mercury.String'.
%
% There is currently some code threaded through the output predicates (usually
% a variable called `ExitMethods') which keeps track of, and removes
% unreachable code. Ideally this would be done as an MLDS->MLDS transformation,
% preferably in a separate module. Unfortunately this is not possible
% due to the fact that the back-end generates `break' statements for cases
% in switches as they are output, meaning that we can't remove them in
% a pass over the MLDS.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_file.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_java_mlds(module_info::in, mlds::in, bool::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_rename_classes.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module ml_backend.mlds_to_java_class.
:- import_module ml_backend.mlds_to_java_export.
:- import_module ml_backend.mlds_to_java_func.
:- import_module ml_backend.mlds_to_java_global.
:- import_module ml_backend.mlds_to_java_util.
:- import_module ml_backend.mlds_to_java_wrap.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_java_mlds(ModuleInfo, MLDS, Succeeded, !IO) :-
    % Note that the Java file name that we use for modules in the
    % Mercury standard library do not include a "mercury." prefix;
    % that is why we don't call mercury_module_name_to_mlds here.
    module_info_get_globals(ModuleInfo, Globals),
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(Globals, do_create_dirs, ".java",
        ModuleName, JavaSourceFile, !IO),
    Indent = 0,
    output_to_file(Globals, JavaSourceFile,
        output_java_src_file(ModuleInfo, Indent, MLDS), Succeeded, !IO).

:- pred output_java_src_file(module_info::in, indent::in, mlds::in,
    list(string)::out, io::di, io::uo) is det.

output_java_src_file(ModuleInfo, Indent, MLDS, Errors, !IO) :-
    % Run further transformations on the MLDS.
    MLDS = mlds(ModuleName, Imports, GlobalData,
        TypeDefns0, TableStructDefns0, ProcDefns0,
        InitPreds, FinalPreds, AllForeignCode, ExportedEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, _AllocIdMap,
        RttiDefns0, CellDefns0, ClosureWrapperFuncDefns0),

    % Do NOT enforce the outermost "mercury" qualifier here. This module name
    % is compared with other module names in the MLDS, to avoid unnecessary
    % module qualification.
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),

    % Find and build list of all methods which would have their addresses
    % taken to be used as a function pointer.
    some [!CodeAddrsInConsts] (
        !:CodeAddrsInConsts = init_code_addrs_in_consts,
        method_ptrs_in_class_defns(TypeDefns0, !CodeAddrsInConsts),
        method_ptrs_in_global_var_defns(RttiDefns0, !CodeAddrsInConsts),
        method_ptrs_in_global_var_defns(CellDefns0, !CodeAddrsInConsts),
        method_ptrs_in_global_var_defns(TableStructDefns0, !CodeAddrsInConsts),
        method_ptrs_in_function_defns(ClosureWrapperFuncDefns0,
            !CodeAddrsInConsts),
        method_ptrs_in_function_defns(ProcDefns0, !CodeAddrsInConsts),

        map.values(ScalarCellGroupMap, ScalarCellGroups),
        ScalarCellRows = list.map(func(G) = G ^ mscg_rows, ScalarCellGroups),
        list.foldl(method_ptrs_in_scalars, ScalarCellRows, !CodeAddrsInConsts),
        !.CodeAddrsInConsts = code_addrs_in_consts(_, _, RevSeqNumsCodeAddrs)
    ),

    assoc_list.values(RevSeqNumsCodeAddrs, RevCodeAddrs),

    make_code_addr_map_for_java(RevCodeAddrs, multi_map.init, CodeAddrsMap),
    map.to_assoc_list(CodeAddrsMap, CodeAddrsAssocList),

    % Create wrappers in MLDS for all pointer addressed methods.
    list.map_foldl(generate_addr_wrapper_class(MLDS_ModuleName),
        CodeAddrsAssocList, WrapperClassDefns0, map.init, AddrOfMap),

    % Rename classes with excessively long names.
    % XXX MLDS_DEFN We know most defns in Defns1 are *not* classes.
    list.map_foldl(maybe_shorten_long_class_name,
        TypeDefns0, TypeDefns1, map.init, RenamingMap1),
    list.map_foldl(maybe_shorten_long_class_name,
        WrapperClassDefns0, WrapperClassDefns1, RenamingMap1, RenamingMap),
    ( if map.is_empty(RenamingMap) then
        TypeDefns = TypeDefns0,
        WrapperClassDefns = WrapperClassDefns0,
        RttiDefns = RttiDefns0,
        CellDefns = CellDefns0,
        TableStructDefns = TableStructDefns0,
        ClosureWrapperFuncDefns = ClosureWrapperFuncDefns0,
        ProcDefns = ProcDefns0
    else
        Renaming = class_name_renaming(MLDS_ModuleName, RenamingMap),
        list.map(rename_class_names_in_class_defn(Renaming),
            TypeDefns1, TypeDefns),
        list.map(rename_class_names_in_class_defn(Renaming),
            WrapperClassDefns1, WrapperClassDefns),
        list.map(rename_class_names_in_global_var_defn(Renaming),
            RttiDefns0, RttiDefns),
        list.map(rename_class_names_in_global_var_defn(Renaming),
            CellDefns0, CellDefns),
        list.map(rename_class_names_in_global_var_defn(Renaming),
            TableStructDefns0, TableStructDefns),
        list.map(rename_class_names_in_function_defn(Renaming),
            ClosureWrapperFuncDefns0, ClosureWrapperFuncDefns),
        list.map(rename_class_names_in_function_defn(Renaming),
            ProcDefns0, ProcDefns)
    ),

    % Get the foreign code for Java
    % XXX We should not ignore _RevImports.
    ForeignCode = mlds_get_java_foreign_code(AllForeignCode),
    ForeignCode = mlds_foreign_code(ForeignDeclCodes, ForeignBodyCodes,
        _Imports, ExportDefns),

    % Output transformed MLDS as Java source.
    %
    % The order is important here, because Java requires static constants
    % be defined before they can be used in static initializers.
    % We start with the Java foreign code declarations, since for
    % library/private_builtin.m they contain static constants
    % that will get used in the RTTI definitions.
    module_info_get_globals(ModuleInfo, Globals),
    module_source_filename(Globals, ModuleName, SourceFileName, !IO),
    Info = init_java_out_info(ModuleInfo, SourceFileName, AddrOfMap),
    output_src_start_for_java(Info, Indent, ModuleName, Imports,
        ForeignDeclCodes, ProcDefns, ForeignDeclErrors, !IO),
    list.map_foldl(output_java_body_code(Info, Indent),
        ForeignBodyCodes, ForeignCodeResults, !IO),
    list.filter_map(maybe_is_error, ForeignCodeResults, ForeignCodeErrors),

    (
        RttiDefns = []
    ;
        RttiDefns = [_ | _],
        io.write_string("\n// RttiDefns\n", !IO),
        list.foldl(
            output_global_var_defn_for_java(Info, Indent + 1, oa_alloc_only),
            RttiDefns, !IO),
        output_rtti_assignments_for_java(Info, Indent + 1, RttiDefns, !IO)
    ),

    ( if
        CellDefns = [],
        TableStructDefns = []
    then
        true
    else
        io.write_string("\n// Cell and tabling definitions\n", !IO),
        output_global_var_decls_for_java(Info, Indent + 1, CellDefns, !IO),
        output_global_var_decls_for_java(Info, Indent + 1, TableStructDefns,
            !IO),
        output_global_var_assignments_for_java(Info, Indent + 1,
            CellDefns ++ TableStructDefns, !IO)
    ),

    % Scalar common data must appear after the previous data definitions, and
    % the vector common data after that.
    ( if map.is_empty(ScalarCellGroupMap) then
        true
    else
        io.write_string("\n// Scalar common data\n", !IO),
        output_scalar_common_data_for_java(Info, Indent + 1,
            ScalarCellGroupMap, !IO)
    ),

    ( if map.is_empty(VectorCellGroupMap) then
        true
    else
        io.write_string("\n// Vector common data\n", !IO),
        output_vector_common_data_for_java(Info, Indent + 1,
            VectorCellGroupMap, !IO)
    ),

    list.sort(ClosureWrapperFuncDefns ++ ProcDefns, SortedFuncDefns),
    (
        SortedFuncDefns = []
    ;
        SortedFuncDefns = [_ | _],
        io.write_string("\n// Function definitions\n", !IO),
        list.foldl(output_function_defn_for_java(Info, Indent + 1, oa_none),
            SortedFuncDefns, !IO)
    ),

    list.sort(WrapperClassDefns ++ TypeDefns, SortedClassDefns),
    (
        SortedClassDefns = []
    ;
        SortedClassDefns = [_ | _],
        io.write_string("\n// Class definitions\n", !IO),
        list.foldl(output_class_defn_for_java(Info, Indent + 1),
            SortedClassDefns, !IO)
    ),

    (
        ExportDefns = []
    ;
        ExportDefns = [_ | _],
        io.write_string("\n// ExportDefns\n", !IO),
        output_exports_for_java(Info, Indent + 1, ExportDefns, !IO)
    ),

    (
        ExportedEnums = []
    ;
        ExportedEnums = [_ | _],
        io.write_string("\n// ExportedEnums\n", !IO),
        output_exported_enums_for_java(Info, Indent + 1, ExportedEnums, !IO)
    ),

    (
        InitPreds = []
    ;
        InitPreds = [_ | _],
        io.write_string("\n// InitPreds\n", !IO),
        output_inits_for_java(Indent + 1, InitPreds, !IO)
    ),

    (
        FinalPreds = []
    ;
        FinalPreds = [_  | _],
        io.write_string("\n// FinalPreds\n", !IO),
        output_finals_for_java(Indent + 1, FinalPreds, !IO)
    ),

    set.init(EnvVarNamesSet0),
    list.foldl(accumulate_env_var_names, ProcDefns,
        EnvVarNamesSet0, EnvVarNamesSet1),
    list.foldl(accumulate_env_var_names, ClosureWrapperFuncDefns,
        EnvVarNamesSet1, EnvVarNamesSet),
    ( if set.is_empty(EnvVarNamesSet) then
        true
    else
        io.write_string("\n// EnvVarNames\n", !IO),
        set.foldl(output_env_var_definition_for_java(Indent + 1),
            EnvVarNamesSet, !IO)
    ),

    output_src_end_for_java(Info, Indent, ModuleName, !IO),
    % XXX Need to handle non-Java foreign code at this point.

    Errors = ForeignDeclErrors ++ ForeignCodeErrors.

:- pred make_code_addr_map_for_java(list(mlds_code_addr)::in,
    multi_map(arity, mlds_code_addr)::in,
    multi_map(arity, mlds_code_addr)::out) is det.

make_code_addr_map_for_java([], !Map).
make_code_addr_map_for_java([CodeAddr | CodeAddrs], !Map) :-
    CodeAddr = mlds_code_addr(_QualFuncLabel, OrigFuncSignature),
    OrigFuncSignature = mlds_func_signature(OrigArgTypes, _OrigRetTypes),
    list.length(OrigArgTypes, Arity),
    multi_map.set(Arity, CodeAddr, !Map),
    make_code_addr_map_for_java(CodeAddrs, !Map).

%---------------------------------------------------------------------------%
%
% Code to output imports.
%

:- pred output_imports(java_out_info::in, mlds_imports::in,
    io::di, io::uo) is det.

output_imports(Info, Imports, !IO) :-
    AutoComments = Info ^ joi_auto_comments,
    (
        AutoComments = yes,
        list.foldl(output_import, Imports, !IO)
    ;
        AutoComments = no
    ).

:- pred output_import(mlds_import::in, io::di, io::uo) is det.

output_import(Import, !IO) :-
    Import = mercury_import(ImportType, ImportName),
    (
        ImportType = user_visible_interface,
        unexpected($pred,
            "import_type `user_visible_interface' in Java backend")
    ;
        ImportType = compiler_visible_interface
    ),
    SymName = mlds_module_name_to_sym_name(ImportName),
    mangle_sym_name_for_java(SymName, module_qual, "__", ClassFile),
    % There are issues related to using import statements and Java's naming
    % conventions. To avoid these problems, we output dependencies as comments
    % only. This is ok, since we always use fully qualified names anyway.
    io.write_strings(["// import ", ClassFile, ";\n"], !IO).

%---------------------------------------------------------------------------%
%
% Code for working with Java `foreign_code'.
%

:- pred output_java_decl(java_out_info::in, indent::in, foreign_decl_code::in,
    maybe_error::out, io::di, io::uo) is det.

output_java_decl(Info, Indent, DeclCode, Res, !IO) :-
    DeclCode = foreign_decl_code(Lang, _IsLocal, LiteralOrInclude, Context),
    (
        Lang = lang_java,
        output_java_foreign_literal_or_include(Info, Indent,
            LiteralOrInclude, Context, Res, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign decl other than Java")
    ).

:- pred output_java_body_code(java_out_info::in, indent::in,
    foreign_body_code::in, maybe_error::out, io::di, io::uo) is det.

output_java_body_code(Info, Indent, ForeignBodyCode, Res, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    % Only output Java code.
    (
        Lang = lang_java,
        output_java_foreign_literal_or_include(Info, Indent, LiteralOrInclude,
            Context, Res, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign code other than Java")
    ).

:- pred output_java_foreign_literal_or_include(java_out_info::in,
    indent::in, foreign_literal_or_include::in, prog_context::in,
    maybe_error::out, io::di, io::uo) is det.

output_java_foreign_literal_or_include(Info, Indent, LiteralOrInclude,
        Context, Res, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        write_string_with_context_block(Info, Indent, Code, Context, !IO),
        Res = ok
    ;
        LiteralOrInclude = floi_include_file(IncludeFile),
        SourceFileName = Info ^ joi_source_filename,
        make_include_file_path(SourceFileName, IncludeFile, IncludePath),
        output_context_for_java(Info ^ joi_foreign_line_numbers,
            marker_begin_block, context(IncludePath, 1), !IO),
        write_include_file_contents_cur_stream(IncludePath, Res, !IO),
        io.nl(!IO),
        % We don't have the true end context readily available.
        output_context_for_java(Info ^ joi_foreign_line_numbers,
            marker_end_block, Context, !IO)
    ).

    % Get the foreign code for Java.
    %
:- func mlds_get_java_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_java_foreign_code(AllForeignCode) = ForeignCode :-
    ( if map.search(AllForeignCode, lang_java, ForeignCode0) then
        ForeignCode = ForeignCode0
    else
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%---------------------------------------------------------------------------%
%
% Code to output calls to module initialisers.
%

:- pred output_inits_for_java(int::in, list(string)::in,
    io::di, io::uo) is det.

output_inits_for_java(Indent, InitPreds, !IO) :-
    (
        InitPreds = []
    ;
        InitPreds = [_ | _],
        % We call the initialisation predicates from a static initialisation
        % block.
        output_n_indents(Indent, !IO),
        io.write_string("static {\n", !IO),
        list.foldl(output_init_for_java_2(Indent + 1), InitPreds, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_init_for_java_2(int::in, string::in, io::di, io::uo) is det.

output_init_for_java_2(Indent, InitPred, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string(InitPred, !IO),
    io.write_string("();\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output module finalisers.
%

:- pred output_finals_for_java(indent::in, list(string)::in,
    io::di, io::uo) is det.

output_finals_for_java(Indent, FinalPreds, !IO) :-
    (
        FinalPreds = []
    ;
        FinalPreds = [_ | _],
        output_n_indents(Indent, !IO),
        io.write_string("static {\n", !IO),
        output_n_indents(Indent + 1, !IO),
        io.write_string("jmercury.runtime.JavaInternal.register_finaliser(\n",
            !IO),
        output_n_indents(Indent + 2, !IO),
        io.write_string("new java.lang.Runnable() {\n", !IO),
        output_n_indents(Indent + 3, !IO),
        io.write_string("public void run() {\n", !IO),
        list.foldl(output_final_pred_call(Indent + 4), FinalPreds, !IO),
        output_n_indents(Indent + 3, !IO),
        io.write_string("}\n", !IO),
        output_n_indents(Indent + 2, !IO),
        io.write_string("}\n", !IO),
        output_n_indents(Indent + 1, !IO),
        io.write_string(");\n", !IO),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_final_pred_call(indent::in, string::in, io::di, io::uo) is det.

output_final_pred_call(Indent, FinalPred, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string(FinalPred, !IO),
    io.write_string("();\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output globals for environment variables.
%

:- pred output_env_var_definition_for_java(indent::in, string::in,
    io::di, io::uo) is det.

output_env_var_definition_for_java(Indent, EnvVarName, !IO) :-
    % We use int because the generated code compares against zero, and changing
    % that is more trouble than it's worth as it affects the C backends.
    output_n_indents(Indent, !IO),
    io.write_string("private static int mercury_envvar_", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string(" =\n", !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("java.lang.System.getenv(\"", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string("\") == null ? 0 : 1;\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start_for_java(java_out_info::in, indent::in,
    mercury_module_name::in, mlds_imports::in, list(foreign_decl_code)::in,
    list(mlds_function_defn)::in, list(string)::out, io::di, io::uo) is det.

output_src_start_for_java(Info, Indent, MercuryModuleName, Imports,
        ForeignDecls, FuncDefns, Errors, !IO) :-
    output_auto_gen_comment(Info ^ joi_source_filename, !IO),
    AutoComments = Info ^ joi_auto_comments,
    (
        AutoComments = yes,
        output_n_indents(Indent, !IO),
        io.write_string("/* :- module ", !IO),
        prog_out.write_sym_name(MercuryModuleName, !IO),
        io.write_string(". */\n\n", !IO)
    ;
        AutoComments = no
    ),
    output_n_indents(Indent, !IO),
    io.write_string("package jmercury;\n", !IO),

    output_imports(Info, Imports, !IO),
    list.map_foldl(output_java_decl(Info, Indent),
        ForeignDecls, ForeignDeclResults, !IO),
    list.filter_map(maybe_is_error, ForeignDeclResults, Errors),

    io.write_string("public class ", !IO),
    mangle_sym_name_for_java(MercuryModuleName, module_qual, "__", ClassName),
    io.write_string(ClassName, !IO),
    io.write_string(" {\n", !IO),

    output_debug_class_init(Info, MercuryModuleName, "start", !IO),

    % Check if this module contains a `main' predicate and if it does insert
    % a `main' method in the resulting Java class that calls the `main'
    % predicate.
    ( if func_defns_contain_main(FuncDefns) then
        write_main_driver_for_java(Indent + 1, ClassName, !IO)
    else
        true
    ).

:- pred write_main_driver_for_java(indent::in, string::in,
    io::di, io::uo) is det.

write_main_driver_for_java(Indent, ClassName, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("public static void main", !IO),
    io.write_string("(java.lang.String[] args)\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),

    % Save the progname and command line arguments in the class variables
    % of `jmercury.runtime.JavaInternal', as well as setting the default
    % exit status.
    Body = [
        "jmercury.runtime.JavaInternal.progname = """ ++ ClassName ++ """;",
        "jmercury.runtime.JavaInternal.args = args;",
        "jmercury.runtime.JavaInternal.exit_status = 0;",
        "library.ML_std_library_init();",
        "benchmarking.ML_initialise();",
        "Runnable run_main = new Runnable() {",
        "    public void run() {",
        "        " ++ ClassName ++ ".main_2_p_0();",
        "    }",
        "};",
        "jmercury.runtime.JavaInternal.runMain(run_main);",
        "io.flush_output_3_p_0(io.stdout_stream_0_f_0());",
        "io.flush_output_3_p_0(io.stderr_stream_0_f_0());",
        "java.lang.System.exit(jmercury.runtime.JavaInternal.exit_status);"
    ],
    list.foldl(write_indented_line(Indent + 1), Body, !IO),

    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_src_end_for_java(java_out_info::in, indent::in,
        mercury_module_name::in, io::di, io::uo) is det.

output_src_end_for_java(Info, Indent, ModuleName, !IO) :-
    output_debug_class_init(Info, ModuleName, "end", !IO),
    io.write_string("}\n", !IO),
    AutoComments = Info ^ joi_auto_comments,
    (
        AutoComments = yes,
        output_n_indents(Indent, !IO),
        io.write_string("// :- end_module ", !IO),
        prog_out.write_sym_name(ModuleName, !IO),
        io.write_string(".\n", !IO)
    ;
        AutoComments = no
    ).

:- pred output_debug_class_init(java_out_info::in, mercury_module_name::in,
    string::in, io::di, io::uo) is det.

output_debug_class_init(Info, ModuleName, State, !IO) :-
    DebugClassInit = get_debug_class_init(Info),
    (
        DebugClassInit = yes,
        list.foldl(io.write_string, [
        "  static {\n",
        "    if (System.getenv(""MERCURY_DEBUG_CLASS_INIT"") != null) {\n",
        "      System.out.println(""[", sym_name_mangle(ModuleName),
        " ", State, " init]"");\n",
        "    }\n",
        "  }\n"
        ], !IO)
    ;
        DebugClassInit = no
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_file.
%---------------------------------------------------------------------------%
