%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mlds_to_cs.m.
% Main authors: wangp.
%
% Convert MLDS to C# code.
%
% About multiple outputs: C# supports pass-by-reference so the first thought is
% to just let the MLDS code generator use `out' parameters to handle multiple
% outputs. But to reference a method, we have to assign it to a delegate with a
% matching signature, which needs to be declared.  Although we can parameterise
% the types, we cannot parameterise the argument modes (in/out), so we would
% need 2^n delegates to cover all methods of arity n.
%
% Instead, we generate code as if C# supported multiple return values.
% The first return value is returned as usual.  The second and later return
% values are assigned to `out' parameters, which we always place after any
% input arguments.  That way we don't need to declare delegates with all
% possible permutations of in/out parameters.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_csharp_mlds(module_info::in, mlds::in, bool::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

    % XXX needed for c_util.output_quoted_string,
    %     c_util.output_quoted_multi_string, and
    %     c_util.make_float_literal.
:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module hlds.hlds_pred.           % for pred_proc_id.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_type_gen.   % for ml_gen_type_name
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module ml_backend.rtti_to_mlds.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.    % for mercury_std_library_name.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_csharp_mlds(ModuleInfo, MLDS, Succeeded, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(Globals, do_create_dirs, ".cs",
        ModuleName, SourceFile, !IO),
    Indent = 0,
    output_to_file(Globals, SourceFile,
        output_csharp_src_file(ModuleInfo, Indent, MLDS), Succeeded, !IO).

%---------------------------------------------------------------------------%
%
% Utility predicates for various purposes.
%

    % Succeeds iff a given string matches the unqualified interface name
    % of a interface in Mercury's C# runtime system.
    %
:- pred interface_is_special_for_csharp(string::in) is semidet.

interface_is_special_for_csharp("MercuryType").

%---------------------------------------------------------------------------%
%
% Code to generate the `.cs' file.
%

:- pred output_csharp_src_file(module_info::in, indent::in, mlds::in,
    io::di, io::uo) is det.

output_csharp_src_file(ModuleInfo, Indent, MLDS, !IO) :-
    % Run further transformations on the MLDS.
    MLDS = mlds(ModuleName, AllForeignCode, Imports, GlobalData,
        TypeDefns, TableStructDefns, ProcDefns,
        InitPreds, FinalPreds, ExportedEnums),
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

    % Get the foreign code for C#
    % XXX We should not ignore _Imports.
    ForeignCode = mlds_get_csharp_foreign_code(AllForeignCode),
    ForeignCode = mlds_foreign_code(ForeignDeclCodes, ForeignBodyCodes,
        _Imports, ExportDefns),

    % Output transformed MLDS as C# source.
    module_info_get_globals(ModuleInfo, Globals),
    module_source_filename(Globals, ModuleName, SourceFileName, !IO),
    Info = init_csharp_out_info(ModuleInfo, SourceFileName, CodeAddrs),
    output_src_start_for_csharp(Info, Indent, ModuleName, Imports,
        ForeignDeclCodes, ProcDefns, !IO),
    io.write_list(ForeignBodyCodes, "\n",
        output_csharp_body_code(Info, Indent), !IO),

    output_pragma_warning_disable(!IO),

    io.write_string("\n// RttiDefns\n", !IO),
    list.foldl(
        output_global_var_defn_for_csharp(Info, Indent + 1, oa_alloc_only),
        RttiDefns, !IO),
    output_rtti_assignments_for_csharp(Info, Indent + 1, RttiDefns, !IO),

    io.write_string("\n// Cell and tabling definitions\n", !IO),
    output_global_var_decls_for_csharp(Info, Indent + 1, CellDefns, !IO),
    output_global_var_decls_for_csharp(Info, Indent + 1, TableStructDefns, !IO),
    output_init_global_var_method_for_csharp(Info, Indent + 1,
        CellDefns ++ TableStructDefns, !IO),

    % Scalar common data must appear after the previous data definitions,
    % and the vector common data after that.
    io.write_string("\n// Scalar common data\n", !IO),
    output_scalar_common_data_for_csharp(Info, Indent + 1,
        ScalarCellGroupMap, !IO),

    io.write_string("\n// Vector common data\n", !IO),
    output_vector_common_data_for_csharp(Info, Indent + 1,
        VectorCellGroupMap, !IO),

    io.write_string("\n// Method pointers\n", !IO),
    output_method_ptr_constants(Info, Indent + 1, CodeAddrs, !IO),

    io.write_string("\n// Function definitions\n", !IO),
    list.sort(ClosureWrapperFuncDefns ++ ProcDefns, SortedFuncDefns),
    list.foldl(output_function_defn_for_csharp(Info, Indent + 1, oa_none),
        SortedFuncDefns, !IO),

    io.write_string("\n// Class definitions\n", !IO),
    list.sort(TypeDefns, SortedClassDefns),
    list.foldl(output_class_defn_for_csharp(Info, Indent + 1),
        SortedClassDefns, !IO),

    io.write_string("\n// ExportDefns\n", !IO),
    output_exports_for_csharp(Info, Indent + 1, ExportDefns, !IO),

    io.write_string("\n// ExportedEnums\n", !IO),
    output_exported_enums_for_csharp(Info, Indent + 1, ExportedEnums, !IO),

    io.write_string("\n// EnvVarNames\n", !IO),
    set.init(EnvVarNamesSet0),
    list.foldl(accumulate_env_var_names, ProcDefns,
        EnvVarNamesSet0, EnvVarNamesSet1),
    list.foldl(accumulate_env_var_names, ClosureWrapperFuncDefns,
        EnvVarNamesSet1, EnvVarNamesSet),
    set.foldl(output_env_var_definition_for_csharp(Indent + 1),
        EnvVarNamesSet, !IO),

    StaticCtorCalls = [
        "MR_init_rtti",
        "MR_init_data",
        "MR_init_scalar_common_data",
        "MR_init_vector_common_data"
        | InitPreds
    ],
    output_static_constructor(ModuleName, Indent + 1, StaticCtorCalls,
        FinalPreds, !IO),

    output_src_end_for_csharp(Indent, ModuleName, !IO).

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

:- pred output_csharp_decl(csharp_out_info::in, indent::in,
    foreign_decl_code::in, io::di, io::uo) is det.

output_csharp_decl(Info, Indent, DeclCode, !IO) :-
    DeclCode = foreign_decl_code(Lang, _IsLocal, LiteralOrInclude, Context),
    (
        Lang = lang_csharp,
        output_csharp_foreign_literal_or_include(Info, Indent,
            LiteralOrInclude, Context, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign decl other than C#")
    ).

:- pred output_csharp_body_code(csharp_out_info::in, indent::in,
    foreign_body_code::in, io::di, io.state::uo) is det.

output_csharp_body_code(Info, Indent, ForeignBodyCode, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    % Only output C# code.
    (
        Lang = lang_csharp,
        output_csharp_foreign_literal_or_include(Info, Indent,
            LiteralOrInclude, Context, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign code other than C#")
    ).

:- pred output_csharp_foreign_literal_or_include(csharp_out_info::in,
    indent::in, foreign_literal_or_include::in, prog_context::in,
    io::di, io::uo) is det.

output_csharp_foreign_literal_or_include(Info, Indent, LiteralOrInclude,
        Context, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        indent_line_after_context(Info ^ csoi_foreign_line_numbers, Context,
            Indent, !IO),
        io.write_string(Code, !IO)
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        SourceFileName = Info ^ csoi_source_filename,
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        cs_output_context(Info ^ csoi_foreign_line_numbers,
            context(IncludePath, 1), !IO),
        write_include_file_contents_cur_stream(IncludePath, !IO)
    ),
    io.nl(!IO),
    cs_output_default_context(Info ^ csoi_foreign_line_numbers, !IO).

:- func mlds_get_csharp_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_csharp_foreign_code(AllForeignCode) = ForeignCode :-
    ( if map.search(AllForeignCode, lang_csharp, ForeignCode0) then
        ForeignCode = ForeignCode0
    else
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%---------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export'.
%

    % Exports are converted into forwarding methods that are given the
    % specified name.  These simply call the exported procedure.
    %
    % NOTE: the forwarding methods must be declared public as they might
    % be referred to within foreign_procs that are inlined across module
    % boundaries.
    %
:- pred output_exports_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_pragma_export)::in, io::di, io::uo) is det.

output_exports_for_csharp(Info, Indent, Exports, !IO) :-
    list.foldl(output_export_for_csharp(Info, Indent), Exports, !IO).

:- pred output_export_for_csharp(csharp_out_info::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

output_export_for_csharp(Info, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, MLDS_Name, MLDS_Signature,
        _UnivQTVars, _),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    expect(unify(Lang, lang_csharp), $pred,
        "foreign_export for language other than C#."),

    output_n_indents(Indent, !IO),
    io.write_string("public static ", !IO),
    % XXX C# has generics
    % output_generic_tvars(UnivQTVars, !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),

    (
        ReturnTypes = [],
        io.write_string("void ", !IO)
    ;
        ReturnTypes = [RetType],
        output_type_for_csharp(Info, RetType, !IO),
        io.write_string(" ", !IO)
    ;
        ReturnTypes = [_, _ | _],
        unexpected($pred, "multiple return values in export method")
    ),
    io.write_string(ExportName, !IO),
    output_params_for_csharp(Info, Indent + 1, Parameters, !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    output_n_indents(Indent + 1, !IO),
    list.filter(is_out_argument, Parameters, OutArgs, InArgs),
    (
        ReturnTypes = [],
        (
            OutArgs = [],
            RestOutArgs = []
        ;
            OutArgs = [FirstOutArg | RestOutArgs],
            FirstOutArg = mlds_argument(FirstOutArgName, _, _),
            output_local_var_name_for_csharp(FirstOutArgName, !IO),
            io.write_string(" = ", !IO)
        )
    ;
        ReturnTypes = [RetTypeB | _],
        % The cast is required when the exported method uses generics but the
        % underlying method does not use generics (i.e. returns Object).
        io.write_string("return (", !IO),
        output_type_for_csharp(Info, RetTypeB, !IO),
        io.write_string(") ", !IO),
        RestOutArgs = OutArgs
    ),
    write_export_call_for_csharp(MLDS_Name, InArgs ++ RestOutArgs, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred is_out_argument(mlds_argument::in) is semidet.

is_out_argument(mlds_argument(_, Type, _)) :-
    Type = mlds_ptr_type(_).

:- pred write_export_call_for_csharp(qual_function_name::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

write_export_call_for_csharp(QualFuncName, Parameters, !IO) :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    output_qual_name_prefix_cs(ModuleName, module_qual, !IO),
    output_function_name_for_csharp(FuncName, !IO),
    io.write_char('(', !IO),
    io.write_list(Parameters, ", ", write_argument_name_for_csharp, !IO),
    io.write_string(");\n", !IO).

:- pred write_argument_name_for_csharp(mlds_argument::in,
    io::di, io::uo) is det.

write_argument_name_for_csharp(Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _),
    ( if Type = mlds_ptr_type(_) then
        io.write_string("out ", !IO)
    else
        true
    ),
    output_local_var_name_for_csharp(Name, !IO).

%---------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export_enum'.
%

:- pred output_exported_enums_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

output_exported_enums_for_csharp(Info, Indent, ExportedEnums, !IO) :-
    list.foldl(output_exported_enum_for_csharp(Info, Indent),
        ExportedEnums, !IO).

:- pred output_exported_enum_for_csharp(csharp_out_info::in, indent::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

output_exported_enum_for_csharp(Info, Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants0),
    (
        Lang = lang_csharp,
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum),
        % We reverse the list so the constants are printed out in order.
        list.reverse(ExportedConstants0, ExportedConstants),
        list.foldl(
            output_exported_enum_constant_for_csharp(Info, Indent, MLDS_Type),
            ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_erlang
        )
    ).

:- pred output_exported_enum_constant_for_csharp(csharp_out_info::in,
    indent::in, mlds_type::in, mlds_exported_enum_constant::in,
    io::di, io::uo) is det.

output_exported_enum_constant_for_csharp(Info, Indent, MLDS_Type,
        ExportedConstant, !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    output_n_indents(Indent, !IO),
    io.write_string("public static readonly ", !IO),
    output_type_for_csharp(Info, MLDS_Type, !IO),
    io.write_string(" ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" = ", !IO),
    output_initializer_body_for_csharp(Info, Initializer, no, !IO),
    io.write_string(";\n", !IO).

%---------------------------------------------------------------------------%

:- pred output_method_ptr_constants(csharp_out_info::in, indent::in,
    map(mlds_code_addr, string)::in, io::di, io::uo) is det.

output_method_ptr_constants(Info, Indent, CodeAddrs, !IO) :-
    map.foldl(output_method_ptr_constant(Info, Indent), CodeAddrs, !IO).

:- pred output_method_ptr_constant(csharp_out_info::in, indent::in,
    mlds_code_addr::in, string::in, io::di, io::uo) is det.

output_method_ptr_constant(Info, Indent, CodeAddr, Name, !IO) :-
    CodeAddr = mlds_code_addr(_QualFuncLabel, Signature),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    TypeString = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
    output_n_indents(Indent, !IO),
    io.format("private static readonly %s %s = ",
        [s(TypeString), s(Name)], !IO),
    IsCall = no,
    mlds_output_code_addr_for_csharp(Info, CodeAddr, IsCall, !IO),
    io.write_string(";\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output globals for environment variables.
%

:- pred output_env_var_definition_for_csharp(indent::in, string::in,
    io::di, io::uo) is det.

output_env_var_definition_for_csharp(Indent, EnvVarName, !IO) :-
    % We use int because the generated code compares against zero, and changing
    % that is more trouble than it's worth as it affects the C backends.
    output_n_indents(Indent, !IO),
    io.write_string("private static int mercury_envvar_", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string(" =\n", !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("System.Environment.GetEnvironmentVariable(\"", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string("\") == null ? 0 : 1;\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start_for_csharp(csharp_out_info::in, indent::in,
    mercury_module_name::in, mlds_imports::in, list(foreign_decl_code)::in,
    list(mlds_function_defn)::in, io::di, io::uo) is det.

output_src_start_for_csharp(Info, Indent, MercuryModuleName, _Imports,
        ForeignDecls, Defns, !IO) :-
    output_auto_gen_comment(Info ^ csoi_source_filename, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("/* :- module ", !IO),
    prog_out.write_sym_name(MercuryModuleName, !IO),
    io.write_string(". */\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("namespace mercury {\n\n", !IO),

    io.write_list(ForeignDecls, "\n", output_csharp_decl(Info, Indent), !IO),
    io.write_string("public static class ", !IO),
    mangle_sym_name_for_csharp(MercuryModuleName, module_qual, "__",
        ClassName),
    io.write_string(ClassName, !IO),
    io.write_string(" {\n", !IO),

    % Check if this module contains a `main' predicate and if it does insert
    % a `main' method in the resulting source file that calls the `main'
    % predicate.
    ( if func_defns_contain_main(Defns) then
        write_main_driver_for_csharp(Indent + 1, ClassName, !IO)
    else
        true
    ).

    % C# only allows a single static constructor so we just call the real
    % methods that we generated earlier.
    %
:- pred output_static_constructor(mercury_module_name::in, indent::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

output_static_constructor(MercuryModuleName, Indent, StaticConstructors,
        FinalPreds, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("static ", !IO),
    mangle_sym_name_for_csharp(MercuryModuleName, module_qual, "__",
        ClassName),
    io.write_string(ClassName, !IO),
    io.write_string("() {\n", !IO),
    WriteCall = (pred(MethodName::in, !.IO::di, !:IO::uo) is det :-
        output_n_indents(Indent + 1, !IO),
        io.write_string(MethodName, !IO),
        io.write_string("();\n", !IO)
    ),
    list.foldl(WriteCall, StaticConstructors, !IO),
    WriteFinal = (pred(FinalPred::in, !.IO::di, !:IO::uo) is det :-
        output_n_indents(Indent + 1, !IO),
        list.foldl(io.write_string, [
            "System.AppDomain.CurrentDomain.ProcessExit += ",
            "(sender, ev) => ", FinalPred, "();\n"
        ], !IO)
    ),
    list.foldl(WriteFinal, FinalPreds, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred write_main_driver_for_csharp(indent::in, string::in,
    io::di, io::uo) is det.

write_main_driver_for_csharp(Indent, ClassName, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("public static void Main", !IO),
    io.write_string("(string[] args)\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    Body = [
        "try {",
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
    list.foldl(write_indented_line(Indent + 1), Body, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_src_end_for_csharp(indent::in, mercury_module_name::in,
    io::di, io::uo) is det.

output_src_end_for_csharp(Indent, ModuleName, !IO) :-
    io.write_string("}\n\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("// :- end_module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output declarations and definitions.
%

:- pred output_global_var_defn_for_csharp(csharp_out_info::in, indent::in,
    output_aux::in, mlds_global_var_defn::in, io::di, io::uo) is det.

output_global_var_defn_for_csharp(Info, Indent, OutputAux, GlobalVarDefn,
        !IO) :-
    output_n_indents(Indent, !IO),
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, Flags,
        Type, Initializer, _),
    output_global_var_decl_flags_for_csharp(Info, Flags, !IO),
    output_global_var_decl_for_csharp(Info, GlobalVarName, Type, !IO),
    output_initializer_for_csharp(Info, OutputAux, Type, Initializer, !IO),
    io.write_string(";\n", !IO).

:- pred output_local_var_defn_for_csharp(csharp_out_info::in, indent::in,
    output_aux::in, mlds_local_var_defn::in, io::di, io::uo) is det.

output_local_var_defn_for_csharp(Info, Indent, OutputAux, LocalVarDefn,
        !IO) :-
    output_n_indents(Indent, !IO),
    LocalVarDefn = mlds_local_var_defn(LocalVarName, _Context,
        Type, Initializer, _),
    output_local_var_decl_for_csharp(Info, LocalVarName, Type, !IO),
    output_initializer_for_csharp(Info, OutputAux, Type, Initializer, !IO),
    io.write_string(";\n", !IO).

:- pred output_field_var_defn_for_csharp(csharp_out_info::in, indent::in,
    output_aux::in, mlds_field_var_defn::in, io::di, io::uo) is det.

output_field_var_defn_for_csharp(Info, Indent, OutputAux, FieldVarDefn,
        !IO) :-
    output_n_indents(Indent, !IO),
    FieldVarDefn = mlds_field_var_defn(FieldVarName, _Context, Flags,
        Type, Initializer, _),
    output_field_var_decl_flags_for_csharp(Flags, !IO),
    output_field_var_decl_for_csharp(Info, FieldVarName, Type, !IO),
    output_initializer_for_csharp(Info, OutputAux, Type, Initializer, !IO),
    io.write_string(";\n", !IO).

:- pred output_function_defn_for_csharp(csharp_out_info::in, indent::in,
    output_aux::in, mlds_function_defn::in, io::di, io::uo) is det.

output_function_defn_for_csharp(Info, Indent, OutputAux, FunctionDefn, !IO) :-
    output_n_indents(Indent, !IO),
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody, _Attributes, _EnvVarNames,
        _MaybeRequireTailrecInfo),
    (
        MaybeBody = body_external,
        % This is just a function declaration, with no body.
        % C# doesn't support separate declarations and definitions,
        % so just output the declaration as a comment.
        % (Note that the actual definition of an external procedure
        % must be given in `pragma foreign_code' in the same module.)
        PreStr = "/* external:\n",
        PostStr = "*/\n"
    ;
        MaybeBody = body_defined_here(_),
        PreStr = "",
        PostStr = ""
    ),
    io.write_string(PreStr, !IO),
    output_function_decl_flags_for_csharp(Info, Flags, !IO),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        maybe_output_pred_proc_id_comment(Info ^ csoi_auto_comments,
            PredProcId, !IO)
    ),
    output_func_for_csharp(Info, Indent, FuncName, OutputAux, Context,
        Params, MaybeBody, !IO),
    io.write_string(PostStr, !IO).

%---------------------------------------------------------------------------%
%
% Code to output classes.
%

:- pred output_class_defn_for_csharp(csharp_out_info::in, indent::in,
    mlds_class_defn::in, io::di, io::uo) is det.

output_class_defn_for_csharp(!.Info, Indent, ClassDefn, !IO) :-
    output_n_indents(Indent, !IO),
    ClassDefn = mlds_class_defn(TypeName, _Context, Flags, Kind,
        _Imports, BaseClasses, Implements, TypeParams,
        MemberFields, MemberClasses, MemberMethods, Ctors),
    expect(unify(MemberMethods, []), $pred,
        "MemberMethods != []"),
    (
        (
            % `static' keyword not allowed on enumerations.
            Kind = mlds_enum
        ;
            % `static' not wanted on classes generated for Mercury types.
            Kind = mlds_class
        ),
        io.write_string("[System.Serializable]\n", !IO),
        output_n_indents(Indent, !IO)
    ;
        ( Kind = mlds_struct
        ; Kind = mlds_interface
        )
    ),
    output_class_decl_flags_for_csharp(!.Info, Flags, Kind, !IO),

    !Info ^ csoi_univ_tvars := TypeParams,

    output_class_kind_for_csharp(Kind, !IO),
    TypeName = mlds_type_name(ClassName, Arity),
    output_unqual_class_name_for_csharp(ClassName, Arity, !IO),
    OutputGenerics = !.Info ^ csoi_output_generics,
    (
        OutputGenerics = do_output_generics,
        output_generic_tvars(TypeParams, !IO)
    ;
        OutputGenerics = do_not_output_generics
    ),
    io.nl(!IO),

    output_supers_list(!.Info, Indent + 1, BaseClasses, Implements, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    (
        ( Kind = mlds_class
        ; Kind = mlds_interface
        ; Kind = mlds_struct
        ),
        list.foldl(
            output_field_var_defn_for_csharp(!.Info, Indent + 1, oa_none),
            MemberFields, !IO),
        list.foldl(
            output_class_defn_for_csharp(!.Info, Indent + 1),
            MemberClasses, !IO)
    ;
        Kind = mlds_enum,
        list.filter(field_var_defn_is_enum_const,
            MemberFields, EnumConstMemberFields),
        % XXX Why +2?
        output_enum_constants_for_csharp(!.Info, Indent + 2, TypeName,
            EnumConstMemberFields, !IO)
    ),
    io.nl(!IO),
    list.foldl(
        output_function_defn_for_csharp(!.Info, Indent + 1,
            oa_cname(TypeName)),
        Ctors, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n\n", !IO).

:- pred output_class_kind_for_csharp(mlds_class_kind::in,
    io::di, io::uo) is det.

output_class_kind_for_csharp(Kind, !IO) :-
    (
        Kind = mlds_interface,
        io.write_string("interface ", !IO)
    ;
        Kind = mlds_class,
        io.write_string("class ", !IO)
    ;
        Kind = mlds_struct,
        io.write_string("struct ", !IO)
    ;
        Kind = mlds_enum,
        io.write_string("enum ", !IO)
    ).

    % Output superclass that this class extends and interfaces implemented.
    % C# does not support multiple inheritance, so more than one superclass is
    % an error.
    %
:- pred output_supers_list(csharp_out_info::in, indent::in,
    list(mlds_class_id)::in, list(mlds_interface_id)::in,
    io::di, io::uo) is det.

output_supers_list(Info, Indent, BaseClasses, Interfaces, !IO) :-
    list.map(interface_to_string, Interfaces, Strings0),
    (
        BaseClasses = [],
        Strings = Strings0
    ;
        BaseClasses = [BaseClass],
        type_to_string_for_csharp(Info, BaseClass, BaseClassString,
            _ArrayDims),
        Strings = [BaseClassString | Strings0]
    ;
        BaseClasses = [_, _ | _],
        unexpected($pred, "multiple inheritance not supported")
    ),
    (
        Strings = []
    ;
        Strings = [_ | _],
        output_n_indents(Indent, !IO),
        io.write_string(": ", !IO),
        io.write_list(Strings, ", ", io.write_string, !IO),
        io.nl(!IO)
    ).

:- pred interface_to_string(mlds_interface_id::in, string::out) is det.

interface_to_string(Interface, String) :-
    ( if
        Interface = mlds_class_type(QualClassName, Arity, _)
    then
        QualClassName = qual_class_name(ModuleQualifier, _QualKind, ClassName),
        SymName = mlds_module_name_to_sym_name(ModuleQualifier),
        mangle_sym_name_for_csharp(SymName, module_qual, ".", ModuleNameStr),

        % Check if the interface is one of the ones in the runtime system.
        % If it is, we don't need to output the arity.
        ( if interface_is_special_for_csharp(ClassName) then
            String = string.format("%s.%s", [s(ModuleNameStr), s(ClassName)])
        else
            String = string.format("%s.%s%d",
                [s(ModuleNameStr), s(ClassName), i(Arity)])
        )
    else
        unexpected($pred, "interface was not a class")
    ).

%---------------------------------------------------------------------------%
%
% Additional code for generating enumerations.
%

:- pred output_enum_constants_for_csharp(csharp_out_info::in, indent::in,
    mlds_type_name::in, list(mlds_field_var_defn)::in, io::di, io::uo) is det.

output_enum_constants_for_csharp(Info, Indent, EnumName, EnumConsts, !IO) :-
    io.write_list(EnumConsts, "\n",
        output_enum_constant_for_csharp(Info, Indent, EnumName), !IO),
    io.nl(!IO).

:- pred output_enum_constant_for_csharp(csharp_out_info::in, indent::in,
    mlds_type_name::in, mlds_field_var_defn::in, io::di, io::uo) is det.

output_enum_constant_for_csharp(Info, Indent, _EnumName, FieldVarDefn, !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, _Context, _Flags,
        _Type, Initializer, _GCStmt),
    (
        Initializer = init_obj(Rval),
        % The name might require mangling.
        output_n_indents(Indent, !IO),
        output_field_var_name_for_csharp(FieldVarName, !IO),
        io.write_string(" = ", !IO),
        ( if
            Rval = ml_const(mlconst_enum(N, _))
        then
            io.write_int(N, !IO)
        else if
            Rval = ml_const(mlconst_foreign(lang_csharp, String, Type))
        then
            io.write_string("(", !IO),
            output_type_for_csharp(Info, Type, !IO),
            io.write_string(") ", !IO),
            io.write_string(String, !IO)
        else
            unexpected($pred, string(Rval))
        ),
        io.write_string(",", !IO)
    ;
        ( Initializer = no_initializer
        ; Initializer = init_struct(_, _)
        ; Initializer = init_array(_)
        ),
        unexpected($pred, string(Initializer))
    ).

%---------------------------------------------------------------------------%
%
% Code to output data declarations/definitions.
%

:- pred output_global_var_decls_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

output_global_var_decls_for_csharp(_, _, [], !IO).
output_global_var_decls_for_csharp(Info, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, Flags,
        Type, _Initializer, _GCStmt),
    output_n_indents(Indent, !IO),
    % We can't honour _Constness here as the variable is assigned separately.
    Flags = mlds_global_var_decl_flags(Access, _Constness),
    NonConstFlags = mlds_global_var_decl_flags(Access, modifiable),
    output_global_var_decl_flags_for_csharp(Info, NonConstFlags, !IO),
    output_global_var_decl_for_csharp(Info, GlobalVarName, Type, !IO),
    io.write_string(";\n", !IO),
    output_global_var_decls_for_csharp(Info, Indent, GlobalVarDefns, !IO).

:- pred output_global_var_decl_for_csharp(csharp_out_info::in,
    mlds_global_var_name::in, mlds_type::in, io::di, io::uo) is det.

output_global_var_decl_for_csharp(Info, GlobalVarName, Type, !IO) :-
    output_type_for_csharp(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_global_var_name_for_csharp(GlobalVarName, !IO).

:- pred output_local_var_decl_for_csharp(csharp_out_info::in,
    mlds_local_var_name::in, mlds_type::in, io::di, io::uo) is det.

output_local_var_decl_for_csharp(Info, LocalVarName, Type, !IO) :-
    output_type_for_csharp(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_local_var_name_for_csharp(LocalVarName, !IO).

:- pred output_field_var_decl_for_csharp(csharp_out_info::in,
    mlds_field_var_name::in, mlds_type::in, io::di, io::uo) is det.

output_field_var_decl_for_csharp(Info, FieldVarName, Type, !IO) :-
    output_type_for_csharp(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_field_var_name_for_csharp(FieldVarName, !IO).

:- pred output_init_global_var_method_for_csharp(csharp_out_info::in,
    indent::in, list(mlds_global_var_defn)::in, io::di, io::uo) is det.

output_init_global_var_method_for_csharp(Info, Indent, GlobalVarDefns, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("private static void MR_init_data() {\n", !IO),
    output_init_global_var_statements_for_csharp(Info, Indent + 1,
        GlobalVarDefns, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_init_global_var_statements_for_csharp(csharp_out_info::in,
    indent::in, list(mlds_global_var_defn)::in, io::di, io::uo) is det.

output_init_global_var_statements_for_csharp(_, _, [], !IO).
output_init_global_var_statements_for_csharp(Info, Indent,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, _Context, _Flags,
        Type, Initializer, _GCStmt),
    output_n_indents(Indent, !IO),
    output_global_var_name_for_csharp(GlobalVarName, !IO),
    output_initializer_for_csharp(Info, oa_none, Type, Initializer, !IO),
    io.write_string(";\n", !IO),
    output_init_global_var_statements_for_csharp(Info, Indent,
        GlobalVarDefns, !IO).

%---------------------------------------------------------------------------%
%
% Code to output common data.
%

:- pred output_scalar_common_data_for_csharp(csharp_out_info::in, indent::in,
    ml_scalar_cell_map::in, io::di, io::uo) is det.

output_scalar_common_data_for_csharp(Info, Indent, ScalarCellGroupMap, !IO) :-
    % Elements of scalar data arrays may reference elements in higher-numbered
    % arrays, or elements of the same array, so we must initialise them
    % separately in a static initialisation block, and we must ensure that
    % elements which are referenced by other elements are initialised first.
    map.foldl3(output_scalar_defns_for_csharp(Info, Indent),
        ScalarCellGroupMap, digraph.init, Graph, map.init, Map, !IO),

    ( if digraph.tsort(Graph, SortedScalars0) then
        output_n_indents(Indent, !IO),
        io.write_string("private static void MR_init_scalar_common_data() {\n",
            !IO),
        list.reverse(SortedScalars0, SortedScalars),
        list.foldl(output_scalar_init_for_csharp(Info, Indent + 1, Map),
            SortedScalars, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)
    else
        unexpected($pred, "digraph.tsort failed")
    ).

:- pred output_scalar_defns_for_csharp(csharp_out_info::in, indent::in,
    ml_scalar_common_type_num::in, ml_scalar_cell_group::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out, io::di, io::uo) is det.

output_scalar_defns_for_csharp(Info, Indent, TypeNum, CellGroup,
        !Graph, !Map, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, _InitArraySize, _Counter, _Members,
        RowInitsCord),
    ArrayType = mlds_array_type(Type),
    RowInits = cord.list(RowInitsCord),

    output_n_indents(Indent, !IO),
    io.write_string("private static readonly ", !IO),
    output_type_for_csharp(Info, Type, !IO),
    io.format("[] MR_scalar_common_%d = ", [i(TypeRawNum)], !IO),
    output_initializer_alloc_only_for_csharp(Info, init_array(RowInits),
        yes(ArrayType), !IO),
    io.write_string(";\n", !IO),

    MLDS_ModuleName = Info ^ csoi_module_name,
    list.foldl3(add_scalar_inits(MLDS_ModuleName, Type, TypeNum),
        RowInits, 0, _, !Graph, !Map).

:- pred output_scalar_init_for_csharp(csharp_out_info::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in, mlds_scalar_common::in,
    io::di, io::uo) is det.

output_scalar_init_for_csharp(Info, Indent, Map, Scalar, !IO) :-
    map.lookup(Map, Scalar, Initializer),
    Scalar = ml_scalar_common(_, Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    output_n_indents(Indent, !IO),
    io.format("MR_scalar_common_%d[%d] = ", [i(TypeRawNum), i(RowNum)], !IO),
    output_initializer_body_for_csharp(Info, Initializer, yes(Type), !IO),
    io.write_string(";\n", !IO).

:- pred output_vector_common_data_for_csharp(csharp_out_info::in, indent::in,
    ml_vector_cell_map::in, io::di, io::uo) is det.

output_vector_common_data_for_csharp(Info, Indent, VectorCellGroupMap, !IO) :-
    map.foldl(output_vector_cell_decl_for_csharp(Info, Indent),
        VectorCellGroupMap, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("private static void MR_init_vector_common_data() {\n",
        !IO),
    map.foldl(output_vector_cell_init_for_csharp(Info, Indent + 1),
        VectorCellGroupMap, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_vector_cell_decl_for_csharp(csharp_out_info::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_decl_for_csharp(Info, Indent, TypeNum, CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, ClassDefn, _FieldIds, _NextRow,
        _RowInits),
    output_class_defn_for_csharp(Info, Indent, ClassDefn, !IO),

    output_n_indents(Indent, !IO),
    io.write_string("private static /* readonly */ ", !IO),
    output_type_for_csharp(Info, Type, !IO),
    io.format("[] MR_vector_common_%d;\n", [i(TypeRawNum)], !IO).

:- pred output_vector_cell_init_for_csharp(csharp_out_info::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_init_for_csharp(Info, Indent, TypeNum, CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, _ClassDefn, _FieldIds, _NextRow,
        RowInits),

    output_n_indents(Indent, !IO),
    io.format("MR_vector_common_%d = new ", [i(TypeRawNum)], !IO),
    output_type_for_csharp(Info, Type, !IO),
    io.write_string("[] {\n", !IO),
    output_n_indents(Indent + 1, !IO),
    output_initializer_body_list_for_csharp(Info, cord.list(RowInits), !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),
    io.write_string("};\n", !IO).

%---------------------------------------------------------------------------%

    % We need to provide initializers for local variables to avoid problems
    % with undefined variables.
    %
:- func get_type_initializer(csharp_out_info, mlds_type) = string.

get_type_initializer(Info, Type) = Initializer :-
    (
        Type = mercury_type(_, CtorCat, _),
        (
            ( CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int))
            ; CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int8))
            ; CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int16))
            ; CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int32))
            % C# byte and ushort literals don't have a suffix.
            ; CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint8))
            ; CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint16))
            ; CtorCat = ctor_cat_builtin(cat_builtin_float)
            ),
            Initializer = "0"
        ;
            ( CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint))
            ; CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint32))
            ),
            Initializer = "0U"
        ;
            CtorCat = ctor_cat_builtin(cat_builtin_char),
            Initializer = "'\\u0000'"
        ;
            ( CtorCat = ctor_cat_builtin(cat_builtin_string)
            ; CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ),
            Initializer = "null"
        ;
            ( CtorCat = ctor_cat_enum(_)
            ; CtorCat = ctor_cat_user(_)
            ; CtorCat = ctor_cat_builtin_dummy
            ),
            type_to_string_for_csharp(Info, Type, TypeString, _),
            Initializer = "default(" ++ TypeString ++ ")"
        )
    ;
        ( Type = mlds_native_int_type
        ; Type = mlds_native_float_type
        ),
        Initializer = "0"
    ;
        Type = mlds_native_uint_type,
        Initializer = "0U"
    ;
        Type = mlds_native_char_type,
        Initializer = "'\\u0000'"
    ;
        Type = mlds_native_bool_type,
        Initializer = "false"
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_class_type(_, _, _)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ),
        Initializer = "null"
    ;
        Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = csharp(csharp_type(CsharpType)),
            Initializer = "default(" ++ CsharpType ++ ")"
        ;
            ( ForeignType = c(_)
            ; ForeignType = java(_)
            ; ForeignType = erlang(_)
            ),
            unexpected($pred, "wrong foreign language type")
        )
    ;
        Type = mlds_unknown_type,
        unexpected($pred, "variable has unknown_type")
    ).

%---------------------------------------------------------------------------%

:- pred output_initializer_for_csharp(csharp_out_info::in, output_aux::in,
    mlds_type::in, mlds_initializer::in, io::di, io::uo) is det.

output_initializer_for_csharp(Info, OutputAux, Type, Initializer, !IO) :-
    (
        ( Initializer = init_obj(_)
        ; Initializer = init_struct(_, _)
        ; Initializer = init_array(_)
        ),
        io.write_string(" = ", !IO),
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures.  If InitStyle is alloc_only
        % then we output an initializer to allocate a structure without filling
        % in the fields.
        (
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_)
            ; OutputAux = oa_force_init
            ),
            output_initializer_body_for_csharp(Info, Initializer,
                yes(Type), !IO)
        ;
            OutputAux = oa_alloc_only,
            output_initializer_alloc_only_for_csharp(Info, Initializer,
                yes(Type), !IO)
        )
    ;
        Initializer = no_initializer,
        (
            OutputAux = oa_force_init,
            % Local variables need to be initialised to avoid warnings.
            io.write_string(" = ", !IO),
            io.write_string(get_type_initializer(Info, Type), !IO)
        ;
            ( OutputAux = oa_none
            ; OutputAux = oa_cname(_)
            ; OutputAux = oa_alloc_only
            )
        )
    ).

:- pred output_initializer_alloc_only_for_csharp(csharp_out_info::in,
    mlds_initializer::in, maybe(mlds_type)::in, io::di, io::uo) is det.

output_initializer_alloc_only_for_csharp(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected($pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        ( if
            StructType = mercury_type(_Type, CtorCat, _),
            type_category_is_array(CtorCat) = is_array
        then
            Size = list.length(FieldInits),
            io.format("object[%d]", [i(Size)], !IO)
        else
            output_type_for_csharp(Info, StructType, !IO),
            io.write_string("()", !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        Size = list.length(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            type_to_string_for_csharp(Info, Type, String, ArrayDims),
            io.write_string(String, !IO),
            % Replace the innermost array dimension by the known size.
            ( if list.split_last(ArrayDims, Heads, 0) then
                output_array_dimensions(Heads ++ [Size], !IO)
            else
                unexpected($pred, "missing array dimension")
            )
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.format("/* XXX init_array */ object[%d]", [i(Size)], !IO)
        )
    ).

:- pred output_initializer_body_for_csharp(csharp_out_info::in,
    mlds_initializer::in, maybe(mlds_type)::in, io::di, io::uo) is det.

output_initializer_body_for_csharp(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($pred, "no_initializer")
    ;
        Initializer = init_obj(Rval),
        output_rval_for_csharp(Info, Rval, !IO)
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        output_type_for_csharp(Info, StructType, !IO),
        IsArray = type_is_array_for_csharp(StructType),
        io.write_string(if IsArray = is_array then " {" else "(", !IO),
        output_initializer_body_list_for_csharp(Info, FieldInits, !IO),
        io.write_char(if IsArray = is_array then '}' else ')', !IO)
    ;
        Initializer = init_array(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            output_type_for_csharp(Info, Type, !IO)
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.write_string("/* XXX init_array */ object[]", !IO)
        ),
        io.write_string(" {\n\t\t", !IO),
        output_initializer_body_list_for_csharp(Info, ElementInits, !IO),
        io.write_string("}", !IO)
    ).

:- pred output_initializer_body_list_for_csharp(csharp_out_info::in,
    list(mlds_initializer)::in, io::di, io::uo) is det.

output_initializer_body_list_for_csharp(Info, Inits, !IO) :-
    io.write_list(Inits, ",\n\t\t",
        (pred(Init::in, !.IO::di, !:IO::uo) is det :-
            output_initializer_body_for_csharp(Info, Init, no, !IO)),
        !IO).

%---------------------------------------------------------------------------%
%
% Code to output RTTI data assignments.
%

:- pred output_rtti_assignments_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

output_rtti_assignments_for_csharp(Info, Indent, Defns, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("static void MR_init_rtti() {\n", !IO),
    OrderedDefns = order_mlds_rtti_defns(Defns),
    list.foldl(output_rtti_defns_assignments_for_csharp(Info, Indent + 1),
        OrderedDefns, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_rtti_defns_assignments_for_csharp(csharp_out_info::in,
    indent::in, list(mlds_global_var_defn)::in, io::di, io::uo) is det.

output_rtti_defns_assignments_for_csharp(Info, Indent, Defns, !IO) :-
    % Separate cliques.
    output_n_indents(Indent, !IO),
    io.write_string("//\n", !IO),
    list.foldl(output_rtti_defn_assignments_for_csharp(Info, Indent),
        Defns, !IO).

:- pred output_rtti_defn_assignments_for_csharp(csharp_out_info::in,
    indent::in, mlds_global_var_defn::in, io::di, io::uo) is det.

output_rtti_defn_assignments_for_csharp(Info, Indent, GlobalVarDefn, !IO) :-
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
            output_n_indents(Indent, !IO),
            output_global_var_name_for_csharp(GlobalVarName, !IO),
            io.write_string(".init(", !IO),
            output_initializer_body_list_for_csharp(Info, FieldInits, !IO),
            io.write_string(");\n", !IO)
        ;
            IsArray = is_array,
            % Not encountered in practice.
            unexpected($pred, "is_array")
        )
    ;
        Initializer = init_array(ElementInits),
        list.foldl2(
            output_rtti_array_assignments_for_csharp(Info, Indent,
                GlobalVarName),
            ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments_for_csharp(csharp_out_info::in,
    indent::in, mlds_global_var_name::in, mlds_initializer::in,
    int::in, int::out, io::di, io::uo) is det.

output_rtti_array_assignments_for_csharp(Info, Indent, GlobalVarName,
        ElementInit, Index, Index + 1, !IO) :-
    output_n_indents(Indent, !IO),
    output_global_var_name_for_csharp(GlobalVarName, !IO),
    io.write_string("[", !IO),
    io.write_int(Index, !IO),
    io.write_string("] = ", !IO),
    output_initializer_body_for_csharp(Info, ElementInit, no, !IO),
    io.write_string(";\n", !IO).

%---------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- pred output_func_for_csharp(csharp_out_info::in, indent::in,
    mlds_function_name::in, output_aux::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func_for_csharp(Info, Indent, FuncName, OutputAux, Context, Signature,
        MaybeBody, !IO) :-
    (
        MaybeBody = body_defined_here(Body),
        output_func_decl_for_csharp(Info, Indent, FuncName, OutputAux,
            Signature, !IO),
        io.write_string("\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        FuncInfo = func_info_csj(Signature),
        output_stmt_for_csharp(Info, Indent + 1, FuncInfo, Body,
            _ExitMethods, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO),    % end the function
        cs_output_default_context(Info ^ csoi_line_numbers, !IO)
    ;
        MaybeBody = body_external
    ).

:- pred output_func_decl_for_csharp(csharp_out_info::in, indent::in,
    mlds_function_name::in, output_aux::in, mlds_func_params::in,
    io::di, io::uo) is det.

output_func_decl_for_csharp(Info, Indent, FuncName, OutputAux, Signature,
        !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    ( if
        OutputAux = oa_cname(ClassName),
        FuncName = mlds_function_export("<constructor>")
    then
        output_type_name_for_csharp(ClassName, !IO),
        OutParams = []
    else
        output_return_types_for_csharp(Info, RetTypes, RestRetTypes, !IO),
        io.write_char(' ', !IO),
        output_function_name_for_csharp(FuncName, !IO),
        list.map_foldl(make_out_param, RestRetTypes, OutParams, 2, _)
    ),
    output_params_for_csharp(Info, Indent, Parameters ++ OutParams, !IO).

:- pred make_out_param(mlds_type::in, mlds_argument::out, int::in, int::out)
    is det.

make_out_param(Type, Argument, Num, Num + 1) :-
    VarName = lvn_comp_var(lvnc_out_param(Num)),
    Argument = mlds_argument(VarName, mlds_ptr_type(Type), gc_no_stmt).

:- pred output_return_types_for_csharp(csharp_out_info::in,
    mlds_return_types::in, list(mlds_type)::out, io::di, io::uo) is det.

output_return_types_for_csharp(Info, RetTypes, OutParams, !IO) :-
    (
        RetTypes = [],
        io.write_string("void", !IO),
        OutParams = []
    ;
        RetTypes = [RetType | OutParams],
        % The first return value is returned directly.  Any further return
        % values are returned via out parameters.
        output_type_for_csharp(Info, RetType, !IO)
    ).

:- pred output_params_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

output_params_for_csharp(Info, Indent, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            output_param_for_csharp(Info, Indent + 1), !IO)
    ),
    io.write_char(')', !IO).

:- pred output_param_for_csharp(csharp_out_info::in, indent::in,
    mlds_argument::in, io::di, io::uo) is det.

output_param_for_csharp(Info, Indent, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GCStmt),
    output_n_indents(Indent, !IO),
    ( if Type = mlds_ptr_type(_) then
        io.write_string("out ", !IO)
    else
        true
    ),
    output_type_for_csharp(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_local_var_name_for_csharp(Name, !IO).

%---------------------------------------------------------------------------%
%
% Code to output names of various entities.
%
% XXX Much of the code in this section will not work when we start enforcing
% names properly.
%

:- pred output_maybe_qualified_global_var_name_for_csharp(csharp_out_info::in,
    qual_global_var_name::in, io::di, io::uo) is det.

output_maybe_qualified_global_var_name_for_csharp(Info, QualGlobalVarName,
        !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    QualGlobalVarName = qual_global_var_name(ModuleName, GlobalVarName),
    CurrentModuleName = Info ^ csoi_module_name,
    ( if ModuleName = CurrentModuleName then
        true
    else
        output_qual_name_prefix_cs(ModuleName, module_qual, !IO)
    ),
    output_global_var_name_for_csharp(GlobalVarName, !IO).

:- pred output_maybe_qualified_function_name_for_csharp(csharp_out_info::in,
    qual_function_name::in, io::di, io::uo) is det.

output_maybe_qualified_function_name_for_csharp(Info, QualFuncName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    QualFuncName = qual_function_name(ModuleName, FuncName),
    CurrentModuleName = Info ^ csoi_module_name,
    ( if ModuleName = CurrentModuleName then
        true
    else
        output_qual_name_prefix_cs(ModuleName, module_qual, !IO)
    ),
    output_function_name_for_csharp(FuncName, !IO).

:- pred output_qual_name_prefix_cs(mlds_module_name::in, mlds_qual_kind::in,
    io::di, io::uo) is det.

output_qual_name_prefix_cs(ModuleName, QualKind, !IO) :-
    qualifier_to_string_for_csharp(ModuleName, QualKind, QualifierString),
    io.write_string(QualifierString, !IO),
    io.write_string(".", !IO).

:- pred qualifier_to_string_for_csharp(mlds_module_name::in,
    mlds_qual_kind::in, string::out) is det.

qualifier_to_string_for_csharp(MLDS_ModuleName, QualKind, String) :-
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % The part of the qualifier that corresponds to a top-level class.
    % Remove the outermost mercury qualifier.
    MangledOuterName = strip_mercury_and_mangle_sym_name_for_csharp(OuterName),

    % The later parts of the qualifier correspond to nested classes.
    ( if OuterName = InnerName then
        MangledSuffix = ""
    else
        remove_sym_name_prefix(InnerName, OuterName, Suffix),
        mangle_sym_name_for_csharp(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix0),
        MangledSuffix = "." ++ MangledSuffix0
    ),

    String = MangledOuterName ++ MangledSuffix.

:- pred output_unqual_class_name_for_csharp(mlds_class_name::in, arity::in,
    io::di, io::uo) is det.

output_unqual_class_name_for_csharp(Name, Arity, !IO) :-
    unqual_class_name_to_string_for_csharp(Name, Arity, String),
    write_identifier_string_for_csharp(String, !IO).

:- pred unqual_class_name_to_string_for_csharp(mlds_class_name::in, arity::in,
    string::out) is det.

unqual_class_name_to_string_for_csharp(Name, Arity, String) :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    String = UppercaseMangledName ++ "_" ++ string.from_int(Arity).

:- pred qual_class_name_to_string_for_csharp(qual_class_name::in, arity::in,
    string::out) is det.

qual_class_name_to_string_for_csharp(QualName, Arity, String) :-
    QualName = qual_class_name(MLDS_ModuleName, QualKind, ClassName),
    ( if
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = csharp_mercury_runtime_package_name
    then
        % Don't mangle runtime class names.
        String = "runtime." ++ ClassName
    else
        % XXX maybe duplicated code
        qualifier_to_string_for_csharp(MLDS_ModuleName, QualKind, QualString),
        unqual_class_name_to_string_for_csharp(ClassName, Arity, UnqualString),
        String = QualString ++ "." ++ UnqualString
    ).

:- pred output_function_name_for_csharp(mlds_function_name::in, io::di, io::uo)
    is det.

output_function_name_for_csharp(FunctionName, !IO) :-
    function_name_to_string_for_csharp(FunctionName, FunctionNameStr),
    write_identifier_string_for_csharp(FunctionNameStr, !IO).

:- pred output_type_name_for_csharp(mlds_type_name::in, io::di, io::uo)
    is det.

output_type_name_for_csharp(TypeName, !IO) :-
    type_name_to_string_for_csharp(TypeName, TypeNameStr),
    write_identifier_string_for_csharp(TypeNameStr, !IO).

:- pred write_identifier_string_for_csharp(string::in, io::di, io::uo) is det.

write_identifier_string_for_csharp(String, !IO) :-
    % Although the C# spec does not limit identifier lengths, the Microsoft
    % compiler restricts identifiers to 511 characters and Mono restricts
    % identifiers to 512 characters.
    % This assumes the identifier contains only ASCII characters.
    Length = string.length(String),
    ( if Length > 511 then
        Left = string.left(String, 251),
        Right = string.right(String, 250),
        Hash = string.hash(String) /\ 0xffffffff,
        io.format("%s_%08x_%s", [s(Left), i(Hash), s(Right)], !IO)
    else
        io.write_string(String, !IO)
    ).

:- pred type_name_to_string_for_csharp(mlds_type_name::in, string::out) is det.

type_name_to_string_for_csharp(TypeName, String) :-
    TypeName = mlds_type_name(Name, Arity),
    unqual_class_name_to_string_for_csharp(Name, Arity, String).

:- pred function_name_to_string_for_csharp(mlds_function_name::in, string::out)
    is det.

function_name_to_string_for_csharp(FunctionName, String) :-
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        pred_label_to_string_for_csharp(PredLabel, PredLabelStr),
        proc_id_to_int(ProcId, ModeNum),
        MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        string.format("%s_%d%s",
            [s(PredLabelStr), i(ModeNum), s(MaybeAuxSuffix)], String)
    ;
        FunctionName = mlds_function_export(Name),
        String = Name
    ).

:- pred pred_label_to_string_for_csharp(mlds_pred_label::in, string::out)
    is det.

pred_label_to_string_for_csharp(PredLabel, String) :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
            Name, PredArity, _, _),
        (
            PredOrFunc = pf_predicate,
            Suffix = "p",
            OrigArity = PredArity
        ;
            PredOrFunc = pf_function,
            Suffix = "f",
            OrigArity = PredArity - 1
        ),
        MangledName = name_mangle_no_leading_digit(Name),
        (
            MaybeDefiningModule = yes(DefiningModule),
            DefiningModuleStr = sym_name_mangle(DefiningModule),
            string.format("%s_%d_%s_in__%s",
                [s(MangledName), i(OrigArity), s(Suffix),
                    s(DefiningModuleStr)],
                String)
        ;
            MaybeDefiningModule = no,
            string.format("%s_%d_%s",
                [s(MangledName), i(OrigArity), s(Suffix)], String)
        )
    ;
        PredLabel = mlds_special_pred_label(PredName, MaybeTypeModule,
            TypeName, TypeArity),
        MangledPredName = name_mangle_no_leading_digit(PredName),
        MangledTypeName = name_mangle(TypeName),
        (
            MaybeTypeModule = yes(TypeModule),
            TypeModuleStr = sym_name_mangle(TypeModule),
            string.format("%s__%s__%s_%d",
                [s(TypeModuleStr), s(MangledPredName), s(MangledTypeName),
                    i(TypeArity)],
                String)
        ;
            MaybeTypeModule = no,
            string.format("%s__%s_%d",
                [s(MangledPredName), s(MangledTypeName), i(TypeArity)],
                String)
        )
    ).

:- pred output_global_var_name_for_csharp(mlds_global_var_name::in,
    io::di, io::uo) is det.

output_global_var_name_for_csharp(DataName, !IO) :-
    global_var_name_to_string_for_csharp(DataName, DataNameStr),
    write_identifier_string_for_csharp(DataNameStr, !IO).

:- pred global_var_name_to_string_for_csharp(mlds_global_var_name::in,
    string::out) is det.

global_var_name_to_string_for_csharp(GlobalVarName, String) :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        String = ml_global_const_var_name_to_string(ConstVar, Num)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        String = RttiAddrName
    ;
        GlobalVarName = gvn_tabling_var(_ProcLabel, _Id),
        unexpected($pred, "NYI: gvn_tabling_ref")
    ;
        GlobalVarName = gvn_dummy_var,
        String = "dummy_var"
    ).

:- pred output_local_var_name_for_csharp(mlds_local_var_name::in,
    io::di, io::uo) is det.

output_local_var_name_for_csharp(VarName, !IO) :-
    local_var_name_to_string_for_csharp(VarName, VarNameStr),
    write_identifier_string_for_csharp(VarNameStr, !IO).

:- pred local_var_name_to_string_for_csharp(mlds_local_var_name::in,
    string::out) is det.

local_var_name_to_string_for_csharp(LocalVarName, String) :-
    RawString = ml_local_var_name_to_string(LocalVarName),
    MangledString = name_mangle(RawString),
    String = make_valid_csharp_symbol_name(MangledString).

:- pred output_field_var_name_for_csharp(mlds_field_var_name::in,
    io::di, io::uo) is det.

output_field_var_name_for_csharp(VarName, !IO) :-
    field_var_name_to_string_for_csharp(VarName, VarNameStr),
    write_identifier_string_for_csharp(VarNameStr, !IO).

:- pred field_var_name_to_string_for_csharp(mlds_field_var_name::in,
    string::out) is det.

field_var_name_to_string_for_csharp(LocalVarName, String) :-
    RawString = ml_field_var_name_to_string(LocalVarName),
    MangledString = name_mangle(RawString),
    String = make_valid_csharp_symbol_name(MangledString).

%---------------------------------------------------------------------------%
%
% Code to output types.
%

:- pred output_type_for_csharp(csharp_out_info::in, mlds_type::in,
    io::di, io::uo) is det.

output_type_for_csharp(Info, MLDS_Type, !IO) :-
    output_type_for_csharp_dims(Info, MLDS_Type, [], !IO).

:- pred output_type_for_csharp_dims(csharp_out_info::in, mlds_type::in,
    list(int)::in, io::di, io::uo) is det.

output_type_for_csharp_dims(Info, MLDS_Type, ArrayDims0, !IO) :-
    type_to_string_for_csharp(Info, MLDS_Type, String, ArrayDims),
    io.write_string(String, !IO),
    output_array_dimensions(ArrayDims ++ ArrayDims0, !IO).

    % type_to_string_for_csharp(Info, MLDS_Type, String, ArrayDims)
    %
    % Generate the Java name for a type. ArrayDims are the array dimensions to
    % be written after the type name, if any, in reverse order to that of Java
    % syntax where a non-zero integer represents a known array size and zero
    % represents an unknown array size.
    %
    % e.g. ArrayDims = [0, 3] represents the Java array `Object[3][]',
    % which should be read as `(Object[])[3]'.
    %
    % XXX yet to check this for C#
    %
:- pred type_to_string_for_csharp(csharp_out_info::in, mlds_type::in,
    string::out, list(int)::out) is det.

type_to_string_for_csharp(Info, MLDS_Type, String, ArrayDims) :-
    (
        MLDS_Type = mercury_type(Type, CtorCat, _),
        ( if
            % We need to handle type_info (etc.) types specially --
            % they get mapped to types in the runtime rather than
            % in private_builtin.
            hand_defined_type_for_csharp(Type, CtorCat,
                SubstituteName, ArrayDims0)
        then
            String = SubstituteName,
            ArrayDims = ArrayDims0
        else if
            % io.state and store.store
            CtorCat = ctor_cat_builtin_dummy
        then
            String = "/* builtin_dummy */ object",
            ArrayDims = []
        else if
            Type = c_pointer_type
        then
            % The c_pointer type is used in the c back-end as a generic way
            % to pass foreign types to automatically generated Compare and
            % Unify code. When compiling to C# we must instead use
            % object.
            String = "/* c_pointer */ object",
            ArrayDims = []
        else
            mercury_type_to_string_for_csharp(Info, Type, CtorCat, String,
                ArrayDims)
        )
    ;
        MLDS_Type = mlds_mercury_array_type(ElementType),
        ( if ElementType = mercury_type(_, ctor_cat_variable, _) then
            String = "System.Array",
            ArrayDims = []
        else
            % For primitive element types we use arrays of the primitive type.
            % For non-primitive element types, we just use
            % `object []'. We used to use more specific types,
            % but then to create an array of the right type we need to use
            % reflection to determine the class of a representative element.
            % That doesn't work if the representative element is of a foreign
            % type, and has the value null.
            ( if csharp_builtin_type(ElementType, _) then
                type_to_string_for_csharp(Info, ElementType, String,
                    ArrayDims0),
                ArrayDims = [0 | ArrayDims0]
            else
                String = "object",
                ArrayDims = [0]
            )
        )
    ;
        MLDS_Type = mlds_native_int_type,
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_uint_type,
        String = "uint",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_float_type,
        String = "double",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_bool_type,
        String = "bool",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_char_type,
        % C# `char' not large enough for code points so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = csharp(csharp_type(String)),
            ArrayDims = []
        ;
            ForeignType = c(_),
            unexpected($pred, "c foreign_type")
        ;
            ForeignType = java(_),
            unexpected($pred, "java foreign_type")
        ;
            ForeignType = erlang(_),
            unexpected($pred, "erlang foreign_type")
        )
    ;
        MLDS_Type = mlds_class_type(Name, Arity, _ClassKind),
        qual_class_name_to_string_for_csharp(Name, Arity, String),
        ArrayDims = []
    ;
        MLDS_Type = mlds_ptr_type(Type),
        % XXX Should we report an error here, if the type pointed to
        % is not a class type?
        type_to_string_for_csharp(Info, Type, String, ArrayDims)
    ;
        MLDS_Type = mlds_array_type(Type),
        type_to_string_for_csharp(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_),
        Type = mlds_generic_type,
        type_to_string_for_csharp(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_func_type(mlds_func_params(Args, RetTypes)),
        ArgTypes = list.map(func(mlds_argument(_, Type, _)) = Type, Args),
        String = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_type,
        String = "/* generic_type */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        String = "/* env_ptr */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_type_info_type,
        String = "runtime.TypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        String = "runtime.PseudoTypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_cont_type(_),
        % XXX can we do better than this for C#?
        String = "/* cont_type */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_commit_type,
        String = "runtime.Commit",
        ArrayDims = []
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_csharp_type(RttiIdMaybeElement, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        % XXX C# is not Java
        tabling_id_java_type(TablingId, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "unknown type")
    ).

:- pred mercury_type_to_string_for_csharp(csharp_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_type_to_string_for_csharp(Info, Type, CtorCat, String, ArrayDims) :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        % C# `char' not large enough for code points so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(IntType)),
        String = int_type_to_csharp_type(IntType),
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        String = "string",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        String = "double",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_void,
        String = "builtin.Void_0",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_variable,
        % XXX C# has generics
        String = "object",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_tuple,
        String = "/* tuple */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_higher_order,
        String = "/* closure */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_system(_),
        mercury_type_to_string_for_csharp(Info, Type,
            ctor_cat_user(cat_user_general), String, ArrayDims)
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        mercury_user_type_to_string_csharp(Info, Type, CtorCat, String,
            ArrayDims)
    ).

:- func int_type_to_csharp_type(int_type) = string.

int_type_to_csharp_type(int_type_int) = "int".
int_type_to_csharp_type(int_type_uint) = "uint".
int_type_to_csharp_type(int_type_int8) = "sbyte".
int_type_to_csharp_type(int_type_uint8) = "byte".
int_type_to_csharp_type(int_type_int16) = "short".
int_type_to_csharp_type(int_type_uint16) = "ushort".
int_type_to_csharp_type(int_type_int32) = "int".
int_type_to_csharp_type(int_type_uint32) = "uint".

:- pred mercury_user_type_to_string_csharp(csharp_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_user_type_to_string_csharp(Info, Type, CtorCat, String, ArrayDims) :-
    type_to_ctor_and_args_det(Type, TypeCtor, ArgsTypes),
    ml_gen_type_name(TypeCtor, ClassName, ClassArity),
    ( if CtorCat = ctor_cat_enum(_) then
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum)
    else
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_class)
    ),
    type_to_string_for_csharp(Info, MLDS_Type, TypeString, ArrayDims),
    OutputGenerics = Info ^ csoi_output_generics,
    (
        OutputGenerics = do_output_generics,
        generic_args_types_to_string_for_csharp(Info, ArgsTypes,
            GenericsString),
        String = TypeString ++ GenericsString
    ;
        OutputGenerics = do_not_output_generics,
        String = TypeString
    ).

:- pred generic_args_types_to_string_for_csharp(csharp_out_info::in,
    list(mer_type)::in, string::out) is det.

generic_args_types_to_string_for_csharp(Info, ArgsTypes, String) :-
    (
        ArgsTypes = [],
        String = ""
    ;
        ArgsTypes = [_ | _],
        ToString =
            ( pred(ArgType::in, ArgTypeString::out) is det :-
                ModuleInfo = Info ^ csoi_module_info,
                MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
                boxed_type_to_string_for_csharp(Info, MLDS_ArgType,
                    ArgTypeString)
            ),
        list.map(ToString, ArgsTypes, ArgsTypesStrings),
        ArgsTypesString = string.join_list(", ", ArgsTypesStrings),
        String = "<" ++ ArgsTypesString ++ ">"
    ).

    % Return is_array if the corresponding C# type is an array type.
    %
:- func type_is_array_for_csharp(mlds_type) = is_array.

type_is_array_for_csharp(Type) = IsArray :-
    ( if Type = mlds_array_type(_) then
        IsArray = is_array
    else if Type = mlds_mostly_generic_array_type(_) then
        IsArray = is_array
    else if Type = mlds_mercury_array_type(_) then
        IsArray = is_array
    else if Type = mercury_type(_, CtorCat, _) then
        IsArray = type_category_is_array(CtorCat)
    else if Type = mlds_rtti_type(RttiIdMaybeElement) then
        rtti_id_maybe_element_csharp_type(RttiIdMaybeElement,
            _TypeName, IsArray)
    else
        IsArray = not_array
    ).

    % hand_defined_type_for_csharp(Type, CtorCat, SubstituteName, ArrayDims):
    %
    % We need to handle type_info (etc.) types specially -- they get mapped
    % to types in the runtime rather than in private_builtin.
    %
:- pred hand_defined_type_for_csharp(mer_type::in, type_ctor_category::in,
    string::out, list(int)::out) is semidet.

hand_defined_type_for_csharp(Type, CtorCat, SubstituteName, ArrayDims) :-
    require_complete_switch [CtorCat]
    (
        CtorCat = ctor_cat_system(CtorCatSystem),
        (
            CtorCatSystem = cat_system_type_info,
            SubstituteName = "runtime.TypeInfo_Struct",
            ArrayDims = []
        ;
            CtorCatSystem = cat_system_type_ctor_info,
            SubstituteName = "runtime.TypeCtorInfo_Struct",
            ArrayDims = []
        ;
            CtorCatSystem = cat_system_typeclass_info,
            SubstituteName = "/* typeclass_info */ object",
            ArrayDims = [0]
        ;
            CtorCatSystem = cat_system_base_typeclass_info,
            SubstituteName = "/* base_typeclass_info */ object",
            ArrayDims = [0]
        )
    ;
        CtorCat = ctor_cat_user(CtorCatUser),
        require_complete_switch [CtorCatUser]
        (
            CtorCatUser = cat_user_general,
            ( if Type = type_desc_type then
                SubstituteName = "runtime.TypeInfo_Struct"
            else if Type = pseudo_type_desc_type then
                SubstituteName = "runtime.PseudoTypeInfo"
            else if Type = type_ctor_desc_type then
                SubstituteName = "runtime.TypeCtorInfo_Struct"
            else
                fail
            ),
            ArrayDims = []
        ;
            ( CtorCatUser = cat_user_direct_dummy
            ; CtorCatUser = cat_user_notag
            ),
            fail
        )
    ;
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ),
        fail
    ).

:- pred boxed_type_to_string_for_csharp(csharp_out_info::in, mlds_type::in,
    string::out) is det.

boxed_type_to_string_for_csharp(Info, Type, String) :-
    type_to_string_for_csharp(Info, Type, String0, ArrayDims),
    list.map(array_dimension_to_string, ArrayDims, RevBrackets),
    list.reverse(RevBrackets, Brackets),
    string.append_list([String0 | Brackets], String).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred output_global_var_decl_flags_for_csharp(csharp_out_info::in,
    mlds_global_var_decl_flags::in, io::di, io::uo) is det.

output_global_var_decl_flags_for_csharp(Info, Flags, !IO) :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    output_global_var_access_for_csharp(Info, Access, !IO),
    output_per_instance_for_csharp(one_copy, !IO),
    output_constness_for_csharp(Constness, !IO).

:- pred output_field_var_decl_flags_for_csharp(mlds_field_var_decl_flags::in,
    io::di, io::uo) is det.

output_field_var_decl_flags_for_csharp(Flags, !IO) :-
    io.write_string("public ", !IO),
    output_per_instance_for_csharp(Flags ^ mfvdf_per_instance, !IO),
    output_constness_for_csharp(Flags ^ mfvdf_constness, !IO).

:- pred output_function_decl_flags_for_csharp(csharp_out_info::in,
    mlds_function_decl_flags::in, io::di, io::uo) is det.

output_function_decl_flags_for_csharp(Info, Flags, !IO) :-
    Access = get_function_access(Flags),
    PerInstance = get_function_per_instance(Flags),
    output_access_for_csharp(Info, Access, !IO),
    output_per_instance_for_csharp(PerInstance, !IO).

:- pred output_class_decl_flags_for_csharp(csharp_out_info::in,
    mlds_class_decl_flags::in, mlds_class_kind::in, io::di, io::uo) is det.

output_class_decl_flags_for_csharp(_Info, Flags, Kind, !IO) :-
    (
        (
            % `static' keyword not allowed on enumerations.
            Kind = mlds_enum
        ;
            % `static' not wanted on classes generated for Mercury types.
            Kind = mlds_class
        ),
        PerInstance = per_instance,
        Overridable = get_class_overridability(Flags)
    ;
        % `static' and `sealed' not wanted or allowed on structs.
        Kind = mlds_struct,
        PerInstance = per_instance,
        Overridable = overridable
    ;
        Kind = mlds_interface,
        PerInstance = one_copy,
        Overridable = get_class_overridability(Flags)
    ),
    Access = get_class_access(Flags),
    Constness = get_class_constness(Flags),
    output_class_access_for_csharp(Access, !IO),
    output_per_instance_for_csharp(PerInstance, !IO),
    output_overridability_for_csharp(Overridable, !IO),
    output_constness_for_csharp(Constness, !IO).

:- pred output_global_var_access_for_csharp(csharp_out_info::in,
    global_var_access::in, io::di, io::uo) is det.

output_global_var_access_for_csharp(_Info, Access, !IO) :-
    (
        Access = gvar_acc_whole_program,
        io.write_string("public ", !IO)
    ;
        Access = gvar_acc_module_only,
        io.write_string("private ", !IO)
    ).

:- pred output_access_for_csharp(csharp_out_info::in, access::in,
    io::di, io::uo) is det.

output_access_for_csharp(_Info, Access, !IO) :-
    (
        Access = acc_public,
        io.write_string("public ", !IO)
    ;
        Access = acc_private,
        io.write_string("private ", !IO)
%   ;
%       Access = acc_protected,
%       io.write_string("protected ", !IO)
%   ;
%       Access = acc_default,
%       maybe_output_comment_for_csharp(Info, "default", !IO)
    ;
        Access = acc_local
    ).

:- pred output_class_access_for_csharp(class_access::in,
    io::di, io::uo) is det.

output_class_access_for_csharp(Access, !IO) :-
    (
        Access = class_public,
        io.write_string("public ", !IO)
    ;
        Access = class_private,
        io.write_string("private ", !IO)
    ).

:- pred output_per_instance_for_csharp(per_instance::in,
    io::di, io::uo) is det.

output_per_instance_for_csharp(PerInstance, !IO) :-
    (
        PerInstance = per_instance
    ;
        PerInstance = one_copy,
        io.write_string("static ", !IO)
    ).

% :- pred output_virtuality_for_csharp(virtuality::in, io::di, io::uo) is det.
%
% output_virtuality_for_csharp(Virtual, !IO) :-
%     (
%         Virtual = virtual,
%         % In C#, methods are non-virtual by default.
%         io.write_string("virtual ", !IO)
%     ;
%         Virtual = non_virtual
%     ).

:- pred output_overridability_for_csharp(overridability::in,
    io::di, io::uo) is det.

output_overridability_for_csharp(Overridability, !IO) :-
    (
        Overridability = sealed,
        io.write_string("sealed ", !IO)
    ;
        Overridability = overridable
    ).

:- pred output_constness_for_csharp(constness::in, io::di, io::uo) is det.

output_constness_for_csharp(Constness, !IO) :-
    (
        Constness = const,
        io.write_string("readonly ", !IO)
    ;
        Constness = modifiable
    ).

% :- pred output_abstractness_for_csharp(abstractness::in,
%     io::di, io::uo) is det.
%
% output_abstractness_for_csharp(Abstractness, !IO) :-
%     (
%         Abstractness = abstract,
%         io.write_string("abstract ", !IO)
%     ;
%         Abstractness = concrete
%     ).

:- pred maybe_output_comment_for_csharp(csharp_out_info::in, string::in,
    io::di, io::uo) is det.

maybe_output_comment_for_csharp(Info, Comment, !IO) :-
    AutoComments = Info ^ csoi_auto_comments,
    (
        AutoComments = yes,
        io.write_string("/* ", !IO),
        io.write_string(Comment, !IO),
        io.write_string(" */", !IO)
    ;
        AutoComments = no
    ).

%---------------------------------------------------------------------------%
%
% Code to output statements.
%

:- pred output_stmts_for_csharp(csharp_out_info::in, indent::in,
    func_info_csj::in, list(mlds_stmt)::in, exit_methods::out,
    io::di, io::uo) is det.

output_stmts_for_csharp(_, _, _, [], ExitMethods, !IO) :-
    ExitMethods = set.make_singleton_set(can_fall_through).
output_stmts_for_csharp(Info, Indent, FuncInfo, [Stmt | Stmts],
        ExitMethods, !IO) :-
    output_stmt_for_csharp(Info, Indent, FuncInfo, Stmt,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        output_stmts_for_csharp(Info, Indent, FuncInfo, Stmts,
            StmtsExitMethods, !IO),
        ExitMethods0 = StmtExitMethods `set.union` StmtsExitMethods,
        ( if set.member(can_fall_through, StmtsExitMethods) then
            ExitMethods = ExitMethods0
        else
            % If the last statement could not complete normally
            % the current block can no longer complete normally.
            ExitMethods = ExitMethods0 `set.delete` can_fall_through
        )
    else
        % Don't output any more statements from the current list since
        % the preceding statement cannot complete.
        ExitMethods = StmtExitMethods
    ).

:- pred output_stmt_for_csharp(csharp_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in, exit_methods::out,
    io::di, io::uo) is det.

output_stmt_for_csharp(Info, Indent, FuncInfo, Stmt, ExitMethods, !IO) :-
    (
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, Stmts, Context),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        (
            LocalVarDefns = [_ | _],
            list.foldl(
                output_local_var_defn_for_csharp(Info, Indent + 1,
                    oa_force_init),
                LocalVarDefns, !IO),
            io.write_string("\n", !IO)
        ;
            LocalVarDefns = []
        ),
        (
            FuncDefns = [_ | _],
            list.foldl(
                output_function_defn_for_csharp(Info, Indent + 1,
                    oa_force_init),
                FuncDefns, !IO),
            io.write_string("\n", !IO)
        ;
            FuncDefns = []
        ),
        output_stmts_for_csharp(Info, Indent + 1, FuncInfo, Stmts,
            ExitMethods, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        Stmt = ml_stmt_while(Kind, Cond, BodyStmt, Context),
        (
            Kind = may_loop_zero_times,
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent, !IO),
            io.write_string("while (", !IO),
            output_rval_for_csharp(Info, Cond, !IO),
            io.write_string(")\n", !IO),
            % The contained statement is reachable iff the while statement is
            % reachable and the condition expression is not a constant
            % expression whose value is false.
            ( if Cond = ml_const(mlconst_false) then
                indent_line_after_context(Info ^ csoi_line_numbers, Context,
                    Indent, !IO),
                io.write_string("{  /* Unreachable code */  }\n", !IO),
                ExitMethods = set.make_singleton_set(can_fall_through)
            else
                output_stmt_for_csharp(Info, Indent + 1, FuncInfo,
                    BodyStmt, StmtExitMethods, !IO),
                ExitMethods =
                    while_exit_methods_for_csharp(Cond, StmtExitMethods)
            )
        ;
            Kind = loop_at_least_once,
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent, !IO),
            io.write_string("do\n", !IO),
            output_stmt_for_csharp(Info, Indent + 1, FuncInfo, BodyStmt,
                StmtExitMethods, !IO),
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent, !IO),
            io.write_string("while (", !IO),
            output_rval_for_csharp(Info, Cond, !IO),
            io.write_string(");\n", !IO),
            ExitMethods = while_exit_methods_for_csharp(Cond, StmtExitMethods)
        )
    ;
        Stmt = ml_stmt_if_then_else(Cond, Then0, MaybeElse, Context),
        % We need to take care to avoid problems caused by the dangling else
        % ambiguity.
        ( if
            % For examples of the form
            %
            %   if (...)
            %       if (...)
            %           ...
            %   else
            %       ...
            %
            % we need braces around the inner `if', otherwise they wouldn't
            % parse they way we want them to.

            MaybeElse = yes(_),
            Then0 = ml_stmt_if_then_else(_, _, no, ThenContext)
        then
            Then = ml_stmt_block([], [], [Then0], ThenContext)
        else
            Then = Then0
        ),

        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("if (", !IO),
        output_rval_for_csharp(Info, Cond, !IO),
        io.write_string(")\n", !IO),
        output_stmt_for_csharp(Info, Indent + 1, FuncInfo, Then,
            ThenExitMethods, !IO),
        (
            MaybeElse = yes(Else),
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent, !IO),
            io.write_string("else\n", !IO),
            output_stmt_for_csharp(Info, Indent + 1, FuncInfo, Else,
                ElseExitMethods, !IO),
            % An if-then-else statement can complete normally iff the
            % then-statement can complete normally or the else-statement
            % can complete normally.
            ExitMethods = ThenExitMethods `set.union` ElseExitMethods
        ;
            MaybeElse = no,
            % An if-then statement can complete normally iff it is reachable.
            ExitMethods = ThenExitMethods `set.union`
                set.make_singleton_set(can_fall_through)
        )
    ;
        Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default, Context),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("switch (", !IO),
        output_rval_for_csharp(Info, Val, !IO),
        io.write_string(") {\n", !IO),
        output_switch_cases_for_csharp(Info, Indent + 1, FuncInfo, Context,
            Cases, Default, ExitMethods, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        Stmt = ml_stmt_label(_, _),
        unexpected($pred, "labels not supported in C#.")
    ;
        Stmt = ml_stmt_goto(goto_label(_), _),
        unexpected($pred, "gotos not supported in C#.")
    ;
        Stmt = ml_stmt_goto(goto_break, Context),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = set.make_singleton_set(can_break)
    ;
        Stmt = ml_stmt_goto(goto_continue, Context),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("continue;\n", !IO),
        ExitMethods = set.make_singleton_set(can_continue)
    ;
        Stmt = ml_stmt_computed_goto(_, _, _),
        unexpected($pred, "computed gotos not supported in C#.")
    ;
        Stmt = ml_stmt_call(Signature, FuncRval, CallArgs,
            Results, IsTailCall, _Markers, Context),
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent + 1, !IO),
        (
            Results = [],
            OutArgs = []
        ;
            Results = [Lval | Lvals],
            output_lval_for_csharp(Info, Lval, !IO),
            io.write_string(" = ", !IO),
            OutArgs = list.map(func(X) = ml_mem_addr(X), Lvals)
        ),
        ( if FuncRval = ml_const(mlconst_code_addr(_)) then
            % This is a standard method call.
            CloseBracket = ""
        else
            % This is a call using a method pointer.
            TypeString = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
            io.format("((%s) ", [s(TypeString)], !IO),
            CloseBracket = ")"
        ),
        output_call_rval_for_csharp(Info, FuncRval, !IO),
        io.write_string(CloseBracket, !IO),
        io.write_string("(", !IO),
        io.write_list(CallArgs ++ OutArgs, ", ",
            output_rval_for_csharp(Info), !IO),
        io.write_string(");\n", !IO),

        % This is to tell the C# compiler that a call to an erroneous procedure
        % will not fall through. --pw
        (
            IsTailCall = ordinary_call
        ;
            IsTailCall = tail_call
        ;
            IsTailCall = no_return_call,
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent  + 1, !IO),
            io.write_string("throw new runtime.UnreachableDefault();\n", !IO)
        ),

        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Stmt = ml_stmt_return(Results, Context),
        (
            Results = [],
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent, !IO),
            io.write_string("return;\n", !IO)
        ;
            Results = [Rval | Rvals],
            % The first return value is returned directly.
            % Subsequent return values are assigned to out parameters.
            list.foldl2(output_assign_out_params(Info, Indent),
                Rvals, 2, _, !IO),
            indent_line_after_context(Info ^ csoi_line_numbers, Context,
                Indent, !IO),
            io.write_string("return ", !IO),
            output_rval_for_csharp(Info, Rval, !IO),
            io.write_string(";\n", !IO)
        ),
        ExitMethods = set.make_singleton_set(can_return)
    ;
        Stmt = ml_stmt_do_commit(Ref, Context),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        output_rval_for_csharp(Info, Ref, !IO),
        io.write_string(" = new runtime.Commit();\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("throw ", !IO),
        output_rval_for_csharp(Info, Ref, !IO),
        io.write_string(";\n", !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ;
        Stmt = ml_stmt_try_commit(_Ref, BodyStmt, HandlerStmt, Context),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("try\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        output_stmt_for_csharp(Info, Indent + 1, FuncInfo, BodyStmt,
            TryExitMethods0, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("catch (runtime.Commit commit_variable)\n",
            !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent + 1, !IO),
        output_stmt_for_csharp(Info, Indent + 1, FuncInfo, HandlerStmt,
            CatchExitMethods, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO),
        ExitMethods = (TryExitMethods0 `set.delete` can_throw)
            `set.union`  CatchExitMethods
    ;
        Stmt = ml_stmt_atomic(AtomicStatement, Context),
        output_atomic_stmt_for_csharp(Info, Indent, AtomicStatement,
            Context, !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ).

%---------------------------------------------------------------------------%
%
% Extra code for handling while-loops.
%

:- func while_exit_methods_for_csharp(mlds_rval, exit_methods) = exit_methods.

while_exit_methods_for_csharp(Cond, BlockExitMethods) = ExitMethods :-
    % A while statement cannot complete normally if its condition
    % expression is a constant expression with value true, and it
    % doesn't contain a reachable break statement that exits the
    % while statement.
    ( if
        % XXX This is not a sufficient way of testing for a Java
        % "constant expression", though determining these accurately
        % is a little difficult to do here.
        % XXX C# is not Java
        Cond = ml_const(mlconst_true),
        not set.member(can_break, BlockExitMethods)
    then
        % Cannot complete normally.
        ExitMethods0 = BlockExitMethods `set.delete` can_fall_through
    else
        ExitMethods0 = BlockExitMethods `set.insert` can_fall_through
    ),
    ExitMethods = (ExitMethods0 `set.delete` can_continue)
        `set.delete` can_break.

%---------------------------------------------------------------------------%
%
% Code for handling multiple return values.
%

:- pred output_assign_out_params(csharp_out_info::in, indent::in,
    mlds_rval::in, int::in, int::out, io::di, io::uo) is det.

output_assign_out_params(Info, Indent, Rval, Num, Num + 1, !IO) :-
    output_n_indents(Indent, !IO),
    io.format("out_param_%d = ", [i(Num)], !IO),
    output_rval_for_csharp(Info, Rval, !IO),
    io.write_string(";\n", !IO).

%---------------------------------------------------------------------------%
%
% Extra code for outputting switch statements.
%

:- pred output_switch_cases_for_csharp(csharp_out_info::in, indent::in,
    func_info_csj::in, prog_context::in, list(mlds_switch_case)::in,
    mlds_switch_default::in, exit_methods::out, io::di, io::uo) is det.

output_switch_cases_for_csharp(Info, Indent, FuncInfo, Context,
        [], Default, ExitMethods, !IO) :-
    output_switch_default_for_csharp(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO).
output_switch_cases_for_csharp(Info, Indent, FuncInfo, Context,
        [Case | Cases], Default, ExitMethods, !IO) :-
    output_switch_case_for_csharp(Info, Indent, FuncInfo, Context, Case,
        CaseExitMethods0, !IO),
    output_switch_cases_for_csharp(Info, Indent, FuncInfo, Context, Cases,
        Default, CasesExitMethods, !IO),
    ( if set.member(can_break, CaseExitMethods0) then
        CaseExitMethods = (CaseExitMethods0 `set.delete` can_break)
            `set.insert` can_fall_through
    else
        CaseExitMethods = CaseExitMethods0
    ),
    ExitMethods = CaseExitMethods `set.union` CasesExitMethods.

:- pred output_switch_case_for_csharp(csharp_out_info::in, indent::in,
    func_info_csj::in, prog_context::in, mlds_switch_case::in,
    exit_methods::out, io::di, io::uo) is det.

output_switch_case_for_csharp(Info, Indent, FuncInfo, Context, Case,
        ExitMethods, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Statement),
    output_case_cond_for_csharp(Info, Indent, Context, FirstCond, !IO),
    list.foldl(output_case_cond_for_csharp(Info, Indent, Context), LaterConds,
        !IO),
    output_stmt_for_csharp(Info, Indent + 1, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent + 1, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = (StmtExitMethods `set.insert` can_break)
            `set.delete` can_fall_through
    else
        % Don't output `break' since it would be unreachable.
        ExitMethods = StmtExitMethods
    ).

:- pred output_case_cond_for_csharp(csharp_out_info::in, indent::in,
    prog_context::in, mlds_case_match_cond::in, io::di, io::uo) is det.

output_case_cond_for_csharp(Info, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("case ", !IO),
        output_rval_for_csharp(Info, Val, !IO),
        io.write_string(":\n", !IO)
    ;
        Match = match_range(_, _),
        unexpected($pred, "cannot match ranges in C# cases")
    ).

:- pred output_switch_default_for_csharp(csharp_out_info::in, indent::in,
    func_info_csj::in, prog_context::in, mlds_switch_default::in,
    exit_methods::out, io::di, io::uo) is det.

output_switch_default_for_csharp(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO) :-
    (
        Default = default_do_nothing,
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Default = default_case(Statement),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("default:\n", !IO),
        output_stmt_for_csharp(Info, Indent + 1, FuncInfo, Statement,
            ExitMethods, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("break;\n", !IO)
    ;
        Default = default_is_unreachable,
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("default: /*NOTREACHED*/\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context, Indent + 1,
            !IO),
        io.write_string("throw new runtime.UnreachableDefault();\n", !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ).

%---------------------------------------------------------------------------%
%
% Code for outputting atomic statements.
%

:- pred output_atomic_stmt_for_csharp(csharp_out_info::in, indent::in,
    mlds_atomic_statement::in, prog_context::in, io::di, io::uo) is det.

output_atomic_stmt_for_csharp(Info, Indent, AtomicStmt, Context, !IO) :-
    (
        AtomicStmt = comment(Comment),
        % XXX We should escape any "*/"'s in the Comment. We should also split
        % the comment into lines and indent each line appropriately.
        output_n_indents(Indent, !IO),
        io.write_string("/* ", !IO),
        io.write_string(Comment, !IO),
        io.write_string(" */\n", !IO)
    ;
        AtomicStmt = assign(Lval, Rval),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        output_lval_for_csharp(Info, Lval, !IO),
        io.write_string(" = ", !IO),
        output_rval_for_csharp(Info, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(_, _),
        sorry($pred, "assign_if_in_heap")
    ;
        AtomicStmt = delete_object(_Lval),
        unexpected($pred, "delete_object not supported in C#.")
    ;
        AtomicStmt = new_object(Target, _MaybeTag, ExplicitSecTag, Type,
            _MaybeSize, MaybeCtorName, Args, ArgTypes, _MayUseAtomic,
            _AllocId),
        (
            ExplicitSecTag = yes,
            unexpected($pred, "explicit secondary tag")
        ;
            ExplicitSecTag = no
        ),

        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent + 1, !IO),
        output_lval_for_csharp(Info, Target, !IO),
        io.write_string(" = new ", !IO),
        % Generate class constructor name.
        ( if
            MaybeCtorName = yes(QualifiedCtorId),
            not (
                Type = mercury_type(MerType, CtorCat, _),
                hand_defined_type_for_csharp(MerType, CtorCat, _, _)
            )
        then
            output_type_for_csharp(Info, Type, !IO),
            io.write_char('.', !IO),
            QualifiedCtorId = qual_ctor_id(_ModuleName, _QualKind, CtorDefn),
            CtorDefn = ctor_id(CtorName, CtorArity),
            output_unqual_class_name_for_csharp(CtorName, CtorArity, !IO)
        else
            output_type_for_csharp(Info, Type, !IO)
        ),
        IsArray = type_is_array_for_csharp(Type),
        (
            IsArray = is_array,
            % The new object will be an array, so we need to initialise it
            % using array literals syntax.
            io.write_string(" {", !IO),
            output_init_args_for_csharp(Info, Args, ArgTypes, !IO),
            io.write_string("};\n", !IO)
        ;
            IsArray = not_array,
            % Generate constructor arguments.
            io.write_string("(", !IO),
            output_init_args_for_csharp(Info, Args, ArgTypes, !IO),
            io.write_string(");\n", !IO)
        ),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        AtomicStmt = gc_check,
        unexpected($pred, "gc_check not implemented.")
    ;
        AtomicStmt = mark_hp(_Lval),
        unexpected($pred, "mark_hp not implemented.")
    ;
        AtomicStmt = restore_hp(_Rval),
        unexpected($pred, "restore_hp not implemented.")
    ;
        AtomicStmt = trail_op(_TrailOp),
        unexpected($pred, "trail_ops not implemented.")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_csharp,
            output_pragma_warning_restore(!IO),
            list.foldl(output_target_code_component_for_csharp(Info),
                Components, !IO),
            output_pragma_warning_disable(!IO)
        ;
            ( TargetLang = ml_target_c
            ; TargetLang = ml_target_java
            ),
            unexpected($pred, "inline_target_code only works for lang_java")
        )
    ;
        AtomicStmt = outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
        unexpected($pred, "foreign language interfacing not implemented")
    ).

%---------------------------------------------------------------------------%

:- pred output_target_code_component_for_csharp(csharp_out_info::in,
    target_code_component::in, io::di, io::uo) is det.

output_target_code_component_for_csharp(Info, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        io.write_string("{\n", !IO),
        (
            MaybeUserContext = yes(ProgContext),
            cs_output_context(Info ^ csoi_foreign_line_numbers,
                ProgContext, !IO)
        ;
            MaybeUserContext = no
        ),
        io.write_string(CodeString, !IO),
        io.write_string("}\n", !IO),
        cs_output_default_context(Info ^ csoi_foreign_line_numbers, !IO)
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        output_rval_for_csharp(Info, Rval, !IO)
    ;
        TargetCode = target_code_output(Lval),
        output_lval_for_csharp(Info, Lval, !IO)
    ;
        TargetCode = target_code_type(Type),
        % XXX enable generics here
        output_type_for_csharp(Info, Type, !IO)
    ;
        TargetCode = target_code_function_name(FuncName),
        output_maybe_qualified_function_name_for_csharp(Info, FuncName, !IO)
    ;
        TargetCode = target_code_alloc_id(_),
        unexpected($pred, "target_code_alloc_id not implemented")
    ).

%---------------------------------------------------------------------------%

    % Output initial values of an object's fields as arguments for the
    % object's class constructor.
    %
:- pred output_init_args_for_csharp(csharp_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_init_args_for_csharp(_, [], [], !IO).
output_init_args_for_csharp(_, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch.").
output_init_args_for_csharp(_, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch.").
output_init_args_for_csharp(Info, [Arg | Args], [_ArgType | ArgTypes], !IO) :-
    output_rval_for_csharp(Info, Arg, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        io.write_string(", ", !IO)
    ),
    output_init_args_for_csharp(Info, Args, ArgTypes, !IO).

%---------------------------------------------------------------------------%
%
% Code to output expressions.
%

:- pred output_lval_for_csharp(csharp_out_info::in, mlds_lval::in,
    io::di, io::uo) is det.

output_lval_for_csharp(Info, Lval, !IO) :-
    (
        Lval = ml_field(_MaybeTag, PtrRval, FieldId, FieldType, _),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                ( FieldType = mlds_generic_type
                ; FieldType = mercury_type(type_variable(_, _), _, _)
                )
            then
                true
            else
                % The field type for field(_, _, offset(_), _, _) lvals
                % must be something that maps to MR_Box.
                unexpected($pred, "unexpected field type")
            ),
            % XXX We shouldn't need this cast here, but there are cases where
            % it is needed and the MLDS doesn't seem to generate it.
            io.write_string("((object[]) ", !IO),
            output_rval_for_csharp(Info, PtrRval, !IO),
            io.write_string(")[", !IO),
            output_rval_for_csharp(Info, OffsetRval, !IO),
            io.write_string("]", !IO)
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            QualFieldVarName = qual_field_var_name(_, _, FieldVarName),
            ( if FieldVarName = fvn_data_tag then
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval_for_csharp(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            else if PtrRval = ml_self(_) then
                % Suppress type cast on `this' keyword.  This makes a
                % difference when assigning to `final' member variables in
                % constructor functions.
                output_rval_for_csharp(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            else
                % Otherwise the field we are trying to access may be
                % in a derived class. Objects are manipulated as instances
                % of their base class, so we need to downcast to the derived
                % class to access some fields.
                io.write_string("((", !IO),
                output_type_for_csharp(Info, CtorType, !IO),
                io.write_string(") ", !IO),
                output_bracketed_rval_for_csharp(Info, PtrRval, !IO),
                io.write_string(").", !IO)
            ),
            output_field_var_name_for_csharp(FieldVarName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        output_bracketed_rval_for_csharp(Info, Rval, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVarName),
        io.write_string("mercury_envvar_", !IO),
        io.write_string(EnvVarName, !IO)
    ;
        Lval = ml_local_var(LocalVarName, _),
        output_local_var_name_for_csharp(LocalVarName, !IO)
    ;
        Lval = ml_global_var(QualGlobalVarName, _),
        output_maybe_qualified_global_var_name_for_csharp(Info,
            QualGlobalVarName, !IO)
    ).

:- pred output_call_rval_for_csharp(csharp_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

output_call_rval_for_csharp(Info, Rval, !IO) :-
    ( if
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    then
        IsCall = yes,
        mlds_output_code_addr_for_csharp(Info, CodeAddr, IsCall, !IO)
    else
        output_bracketed_rval_for_csharp(Info, Rval, !IO)
    ).

:- pred output_bracketed_rval_for_csharp(csharp_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

output_bracketed_rval_for_csharp(Info, Rval, !IO) :-
    ( if
        % If it is just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        output_rval_for_csharp(Info, Rval, !IO)
    else
        io.write_char('(', !IO),
        output_rval_for_csharp(Info, Rval, !IO),
        io.write_char(')', !IO)
    ).

:- pred output_rval_for_csharp(csharp_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

output_rval_for_csharp(Info, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval_for_csharp(Info, Lval, !IO)
    ;
        Rval = ml_mkword(_, _),
        unexpected($pred, "tags not supported in C#")
    ;
        Rval = ml_const(Const),
        output_rval_const_for_csharp(Info, Const, !IO)
    ;
        Rval = ml_unop(Op, RvalA),
        output_unop_for_csharp(Info, Op, RvalA, !IO)
    ;
        Rval = ml_binop(Op, RvalA, RvalB),
        output_binop_for_csharp(Info, Op, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(Lval),
        io.write_string("out ", !IO),
        output_lval_for_csharp(Info, Lval, !IO)
    ;
        Rval = ml_scalar_common(_),
        unexpected($pred, "ml_scalar_common")
    ;
        Rval = ml_scalar_common_addr(ScalarCommon),
        ScalarCommon = ml_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName =
            strip_mercury_and_mangle_sym_name_for_csharp(ModuleSymName),
        io.format("%s.MR_scalar_common_%d[%d]",
            [s(MangledModuleName), i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = ml_vector_common(_ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        % XXX Why do we print a "MangledModuleName." prefix for scalar common
        % addresses but not for vector common addresses?
        io.format("MR_vector_common_%d[%d + ",
            [i(TypeNum), i(StartRowNum)], !IO),
        output_rval_for_csharp(Info, RowRval, !IO),
        io.write_string("]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

:- pred output_unop_for_csharp(csharp_out_info::in, mlds_unary_op::in,
    mlds_rval::in, io::di, io::uo) is det.

output_unop_for_csharp(Info, Unop, Expr, !IO) :-
    (
        Unop = cast(Type),
        output_cast_rval_for_csharp(Info, Type, Expr, !IO)
    ;
        Unop = box(Type),
        ( if Type = mercury_type(comparison_result_type, _, _) then
            io.write_string("builtin.comparison_result_object[(int) ", !IO),
            output_rval_for_csharp(Info, Expr, !IO),
            io.write_string("]", !IO)
        else
            output_boxed_rval_for_csharp(Info, Type, Expr, !IO)
        )
    ;
        Unop = unbox(Type),
        output_unboxed_rval_for_csharp(Info, Type, Expr, !IO)
    ;
        Unop = std_unop(StdUnop),
        output_std_unop_for_csharp(Info, StdUnop, Expr, !IO)
    ).

:- pred output_cast_rval_for_csharp(csharp_out_info::in, mlds_type::in,
    mlds_rval::in, io::di, io::uo) is det.

output_cast_rval_for_csharp(Info, Type, Expr, !IO) :-
    % rtti_to_mlds.m generates casts from int to runtime.PseudoTypeInfo, but
    % for C# we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    ( if
        Type = mlds_pseudo_type_info_type,
        Expr = ml_const(mlconst_int(N))
    then
        maybe_output_comment_for_csharp(Info, "cast", !IO),
        ( if have_preallocated_pseudo_type_var_for_csharp(N) then
            io.write_string("runtime.PseudoTypeInfo.K", !IO),
            io.write_int(N, !IO)
        else
            io.write_string("new runtime.PseudoTypeInfo(", !IO),
            output_rval_for_csharp(Info, Expr, !IO),
            io.write_string(")", !IO)
        )
    else if
        ( Type = mercury_type(_, ctor_cat_system(cat_system_type_info), _)
        ; Type = mlds_type_info_type
        )
    then
        % XXX We really should be able to tell if we are casting a
        % TypeCtorInfo or a TypeInfo. Julien says that's probably going to
        % be rather difficult as the compiler doesn't keep track of where
        % type_ctor_infos are acting as type_infos properly.
        maybe_output_comment_for_csharp(Info, "cast", !IO),
        io.write_string("runtime.TypeInfo_Struct.maybe_new(",
            !IO),
        output_rval_for_csharp(Info, Expr, !IO),
        io.write_string(")", !IO)
    else if
        csharp_builtin_type(Type, "int")
    then
        io.write_string("(int) ", !IO),
        output_rval_for_csharp(Info, Expr, !IO)
    else
        io.write_string("(", !IO),
        output_type_for_csharp(Info, Type, !IO),
        io.write_string(") ", !IO),
        output_rval_for_csharp(Info, Expr, !IO)
    ).

:- pred have_preallocated_pseudo_type_var_for_csharp(int::in) is semidet.

have_preallocated_pseudo_type_var_for_csharp(N) :-
    % Corresponds to static members in class PseudoTypeInfo.
    N >= 1,
    N =< 5.

:- pred output_boxed_rval_for_csharp(csharp_out_info::in, mlds_type::in,
    mlds_rval::in, io::di, io::uo) is det.

output_boxed_rval_for_csharp(Info, _Type, Expr, !IO) :-
    % C# does implicit boxing.
    output_rval_for_csharp(Info, Expr, !IO).
    /*
    ( if csharp_builtin_type(Type, _JavaName, JavaBoxedName, _) then
        % valueOf may return cached instances instead of creating new objects.
        io.write_string(JavaBoxedName, !IO),
        io.write_string(".valueOf(", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    else
        io.write_string("((object) (", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string("))", !IO)
    ).
    */

:- pred output_unboxed_rval_for_csharp(csharp_out_info::in, mlds_type::in,
    mlds_rval::in, io::di, io::uo) is det.

output_unboxed_rval_for_csharp(Info, Type, Expr, !IO) :-
    io.write_string("((", !IO),
    output_type_for_csharp(Info, Type, !IO),
    io.write_string(") ", !IO),
    output_rval_for_csharp(Info, Expr, !IO),
    io.write_string(")", !IO).

:- pred csharp_builtin_type(mlds_type::in, string::out) is semidet.

csharp_builtin_type(Type, TargetType) :-
    require_complete_switch [Type] (
        Type = mlds_native_bool_type,
        TargetType = "bool"
    ;
        % C# `char' is not large enough for code points so we must use `int'.
        ( Type = mlds_native_int_type
        ; Type = mlds_native_char_type
        ),
        TargetType = "int"
    ;
        Type = mlds_native_uint_type,
        TargetType = "uint"
    ;
        Type = mlds_native_float_type,
        TargetType = "double"
    ;
        Type = mercury_type(MerType, TypeCtorCat, _),
        require_complete_switch [MerType] (
            MerType = builtin_type(BuiltinType),
            require_complete_switch [BuiltinType] (
                BuiltinType = builtin_type_char,
                TargetType = "int"
            ;
                BuiltinType = builtin_type_int(IntType),
                TargetType = int_type_to_csharp_type(IntType)
            ;
                BuiltinType = builtin_type_float,
                TargetType = "double"
            ;
                BuiltinType = builtin_type_string,
                fail
            )
    ;
            MerType = defined_type(_, _, _),
            require_complete_switch [TypeCtorCat] (
                % io.state and store.store(S) are dummy variables for which we
                % pass an arbitrary integer. For this reason they should have
                % the C# type `int'.
                TypeCtorCat = ctor_cat_builtin_dummy,
                TargetType = "int"
            ;
                ( TypeCtorCat = ctor_cat_builtin(_)
                ; TypeCtorCat = ctor_cat_higher_order
                ; TypeCtorCat = ctor_cat_tuple
                ; TypeCtorCat = ctor_cat_enum(_)
                ; TypeCtorCat = ctor_cat_variable
                ; TypeCtorCat = ctor_cat_system(_)
                ; TypeCtorCat = ctor_cat_void
                ; TypeCtorCat = ctor_cat_user(_)
                ),
                fail
            )
        ;
            ( MerType = type_variable(_, _)
            ; MerType = tuple_type(_, _)
            ; MerType = higher_order_type(_, _, _, _, _)
            ; MerType = apply_n_type(_, _, _)
            ; MerType = kinded_type(_, _)
            ),
            fail
        )
    ;
        ( Type = mlds_foreign_type(_)
        ; Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_class_type(_, _, _)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ),
        fail
    ;
        Type = mlds_unknown_type,
        unexpected($file, $pred, "unknown typed")
    ).

:- pred output_std_unop_for_csharp(csharp_out_info::in,
    builtin_ops.unary_op::in, mlds_rval::in, io::di, io::uo) is det.

output_std_unop_for_csharp(Info, UnaryOp, Expr, !IO) :-
    % For the C# backend, there are no tags, so all the tagging operators
    % are no-ops, except for `tag', which always returns zero (a tag of zero
    % means there is no tag).
    (
        UnaryOp = tag ,
        io.write_string("/* tag */  0", !IO)
    ;
        ( UnaryOp = mktag,     UnaryOpStr = "/* mktag */ "
        ; UnaryOp = unmktag,   UnaryOpStr = "/* unmktag */ "
        ; UnaryOp = strip_tag, UnaryOpStr = "/* strip_tag */ "
        ; UnaryOp = mkbody,    UnaryOpStr = "/* mkbody */ "
        ; UnaryOp = unmkbody,   UnaryOpStr = "/* unmkbody */ "
        ; UnaryOp = bitwise_complement(int_type_int), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_int32), UnaryOpStr = "~"
        ; UnaryOp = bitwise_complement(int_type_uint32), UnaryOpStr = "~"
        ; UnaryOp = logical_not, UnaryOpStr = "!"
        ; UnaryOp = hash_string,  UnaryOpStr = "mercury.String.hash_1_f_0"
        ; UnaryOp = hash_string2, UnaryOpStr = "mercury.String.hash2_1_f_0"
        ; UnaryOp = hash_string3, UnaryOpStr = "mercury.String.hash3_1_f_0"
        ; UnaryOp = hash_string4, UnaryOpStr = "mercury.String.hash4_1_f_0"
        ; UnaryOp = hash_string5, UnaryOpStr = "mercury.String.hash5_1_f_0"
        ; UnaryOp = hash_string6, UnaryOpStr = "mercury.String.hash6_1_f_0"
        ),
        io.write_string(UnaryOpStr, !IO),
        io.write_string("(", !IO),
        output_rval_for_csharp(Info, Expr, !IO),
        io.write_string(")", !IO)
    ;
        (
            UnaryOp = bitwise_complement(int_type_int8),
            CastStr = "(sbyte)"
        ;
            UnaryOp = bitwise_complement(int_type_uint8),
            CastStr = "(byte)"
        ;
            UnaryOp = bitwise_complement(int_type_int16),
            CastStr = "(short)"
        ;
            UnaryOp = bitwise_complement(int_type_uint16),
            CastStr = "(ushort)"
        ),
        UnaryOpStr = "~",
        io.write_string(CastStr, !IO),
        io.write_string(UnaryOpStr, !IO),
        io.write_string("(", !IO),
        output_rval_for_csharp(Info, Expr, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_binop_for_csharp(csharp_out_info::in, binary_op::in,
    mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.

output_binop_for_csharp(Info, Op, X, Y, !IO) :-
    (
        Op = array_index(_Type),
        output_bracketed_rval_for_csharp(Info, X, !IO),
        io.write_string("[", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string("]", !IO)
    ;
        Op = str_eq,
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(".Equals(", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = str_ne, OpStr = "!="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ),
        io.write_string("(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(".CompareOrdinal(", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(") ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" 0)", !IO)
    ;
        Op = str_cmp,
        io.write_string("(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(".CompareOrdinal(", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string("))", !IO)
    ;
        Op = pointer_equal_conservative,
        io.write_string("System.Object.ReferenceEquals(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(", ", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        % XXX Should we abort for some of these?
        ( Op = int_add(int_type_int)
        ; Op = int_sub(int_type_int)
        ; Op = int_mul(int_type_int)
        ; Op = int_div(int_type_int)
        ; Op = int_mod(int_type_int)
        ; Op = unchecked_left_shift(int_type_int)
        ; Op = unchecked_right_shift(int_type_int)
        ; Op = bitwise_and(int_type_int)
        ; Op = bitwise_or(int_type_int)
        ; Op = bitwise_xor(int_type_int)
        ; Op = logical_and
        ; Op = logical_or
        ; Op = eq(_)
        ; Op = ne(_)
        ; Op = body
        ; Op = string_unsafe_index_code_unit
        ; Op = offset_str_eq(_)
        ; Op = int_lt(_)
        ; Op = int_gt(_)
        ; Op = int_le(_)
        ; Op = int_ge(_)
        ; Op = unsigned_le
        ; Op = int_add(int_type_uint)
        ; Op = int_sub(int_type_uint)
        ; Op = int_mul(int_type_uint)
        ; Op = int_div(int_type_uint)
        ; Op = int_mod(int_type_uint)
        ; Op = bitwise_and(int_type_uint)
        ; Op = bitwise_or(int_type_uint)
        ; Op = bitwise_xor(int_type_uint)
        ; Op = unchecked_left_shift(int_type_uint)
        ; Op = unchecked_right_shift(int_type_uint)
        ; Op = int_add(int_type_int32)
        ; Op = int_sub(int_type_int32)
        ; Op = int_mul(int_type_int32)
        ; Op = int_div(int_type_int32)
        ; Op = int_mod(int_type_int32)
        ; Op = bitwise_and(int_type_int32)
        ; Op = bitwise_or(int_type_int32)
        ; Op = bitwise_xor(int_type_int32)
        ; Op = unchecked_left_shift(int_type_int32)
        ; Op = unchecked_right_shift(int_type_int32)
        ; Op = int_add(int_type_uint32)
        ; Op = int_sub(int_type_uint32)
        ; Op = int_mul(int_type_uint32)
        ; Op = int_div(int_type_uint32)
        ; Op = int_mod(int_type_uint32)
        ; Op = bitwise_and(int_type_uint32)
        ; Op = bitwise_or(int_type_uint32)
        ; Op = bitwise_xor(int_type_uint32)
        ; Op = unchecked_left_shift(int_type_uint32)
        ; Op = unchecked_right_shift(int_type_uint32)
        ; Op = float_plus
        ; Op = float_minus
        ; Op = float_times
        ; Op = float_divide
        ; Op = float_eq
        ; Op = float_ne
        ; Op = float_lt
        ; Op = float_gt
        ; Op = float_le
        ; Op = float_ge
        ; Op = float_word_bits
        ; Op = float_from_dword
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        io.write_string("(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_csharp(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_add(int_type_int8)
        ; Op = int_sub(int_type_int8)
        ; Op = int_mul(int_type_int8)
        ; Op = int_div(int_type_int8)
        ; Op = int_mod(int_type_int8)
        ; Op = bitwise_and(int_type_int8)
        ; Op = bitwise_xor(int_type_int8)
        ; Op = unchecked_left_shift(int_type_int8)
        ; Op = unchecked_right_shift(int_type_int8)
        ),
        io.write_string("(sbyte)(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_csharp(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        % The special treatment of bitwise-or here is necessary to avoid
        % warning CS0675 from the C# compiler.
        Op = bitwise_or(int_type_int8),
        io.write_string("(sbyte)((byte)", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_csharp(Op, !IO),
        io.write_string(" (byte)", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_add(int_type_uint8)
        ; Op = int_sub(int_type_uint8)
        ; Op = int_mul(int_type_uint8)
        ; Op = int_div(int_type_uint8)
        ; Op = int_mod(int_type_uint8)
        ; Op = bitwise_and(int_type_uint8)
        ; Op = bitwise_or(int_type_uint8)
        ; Op = bitwise_xor(int_type_uint8)
        ; Op = unchecked_left_shift(int_type_uint8)
        ; Op = unchecked_right_shift(int_type_uint8)
        ),
        io.write_string("(byte)(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_csharp(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_add(int_type_int16)
        ; Op = int_sub(int_type_int16)
        ; Op = int_mul(int_type_int16)
        ; Op = int_div(int_type_int16)
        ; Op = int_mod(int_type_int16)
        ; Op = bitwise_and(int_type_int16)
        ; Op = bitwise_or(int_type_int16)
        ; Op = bitwise_xor(int_type_int16)
        ; Op = unchecked_left_shift(int_type_int16)
        ; Op = unchecked_right_shift(int_type_int16)
        ),
        io.write_string("(short)(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_csharp(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = int_add(int_type_uint16)
        ; Op = int_sub(int_type_uint16)
        ; Op = int_mul(int_type_uint16)
        ; Op = int_div(int_type_uint16)
        ; Op = int_mod(int_type_uint16)
        ; Op = bitwise_and(int_type_uint16)
        ; Op = bitwise_or(int_type_uint16)
        ; Op = bitwise_xor(int_type_uint16)
        ; Op = unchecked_left_shift(int_type_uint16)
        ; Op = unchecked_right_shift(int_type_uint16)
        ),
        io.write_string("(ushort)(", !IO),
        output_rval_for_csharp(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op_for_csharp(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_for_csharp(Info, Y, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_binary_op_for_csharp(binary_op::in, io::di, io::uo) is det.

output_binary_op_for_csharp(Op, !IO) :-
    (
        ( Op = int_add(_), OpStr = "+"
        ; Op = int_sub(_), OpStr = "-"
        ; Op = int_mul(_), OpStr = "*"
        ; Op = int_div(_), OpStr = "/"
        ; Op = int_mod(_), OpStr = "%"
        ; Op = unchecked_left_shift(_), OpStr = "<<"
        ; Op = unchecked_right_shift(_), OpStr = ">>"
        ; Op = bitwise_and(_), OpStr = "&"
        ; Op = bitwise_or(_), OpStr = "|"
        ; Op = bitwise_xor(_), OpStr = "^"
        ; Op = logical_and, OpStr = "&&"
        ; Op = logical_or, OpStr = "||"
        ; Op = eq(_), OpStr = "=="
        ; Op = ne(_), OpStr = "!="
        ; Op = int_lt(_), OpStr = "<"
        ; Op = int_gt(_), OpStr = ">"
        ; Op = int_le(_), OpStr = "<="
        ; Op = int_ge(_), OpStr = ">="

        ; Op = float_eq, OpStr = "=="
        ; Op = float_ne, OpStr = "!="
        ; Op = float_le, OpStr = "<="
        ; Op = float_ge, OpStr = ">="
        ; Op = float_lt, OpStr = "<"
        ; Op = float_gt, OpStr = ">"

        ; Op = float_plus, OpStr = "+"
        ; Op = float_minus, OpStr = "-"
        ; Op = float_times, OpStr = "*"
        ; Op = float_divide, OpStr = "/"
        ),
        io.write_string(OpStr, !IO)
    ;
        ( Op = array_index(_)
        ; Op = body
        ; Op = float_from_dword
        ; Op = float_word_bits
        ; Op = offset_str_eq(_)
        ; Op = str_cmp
        ; Op = str_eq
        ; Op = str_ge
        ; Op = str_gt
        ; Op = str_le
        ; Op = str_lt
        ; Op = str_ne
        ; Op = string_unsafe_index_code_unit
        ; Op = pointer_equal_conservative
        ; Op = unsigned_le
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        unexpected($pred, "invalid binary operator")
    ).

:- pred output_rval_const_for_csharp(csharp_out_info::in, mlds_rval_const::in,
    io::di, io::uo) is det.

output_rval_const_for_csharp(Info, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string("true", !IO)
    ;
        Const = mlconst_false,
        io.write_string("false", !IO)
    ;
        Const = mlconst_int(N),
        output_int_const_for_csharp(N, !IO)
    ;
        Const = mlconst_uint(U),
        output_uint_const_for_csharp(U, !IO)
    ;
        Const = mlconst_int8(N),
        output_int8_const_for_csharp(N, !IO)
    ;
        Const = mlconst_uint8(N),
        output_uint8_const_for_csharp(N, !IO)
    ;
        Const = mlconst_int16(N),
        output_int16_const_for_csharp(N, !IO)
    ;
        Const = mlconst_uint16(N),
        output_uint16_const_for_csharp(N, !IO)
    ;
        Const = mlconst_int32(N),
        output_int32_const_for_csharp(N, !IO)
    ;
        Const = mlconst_uint32(N),
        output_uint32_const_for_csharp(N, !IO)
    ;
        Const = mlconst_char(N),
        io.write_string("( ", !IO),
        output_int_const_for_csharp(N, !IO),
        io.write_string(")", !IO)
    ;
        Const = mlconst_enum(N, EnumType),
        % Explicit cast required.
        output_cast_rval_for_csharp(Info, EnumType, ml_const(mlconst_int(N)),
            !IO)
    ;
        Const = mlconst_foreign(Lang, Value, Type),
        expect(unify(Lang, lang_csharp), $pred, "language other than C#."),
        % XXX Should we parenthesize this?
        io.write_string("(", !IO),
        output_type_for_csharp(Info, Type, !IO),
        io.write_string(") ", !IO),
        io.write_string(Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal_cur_stream(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_string_lang_cur_stream(literal_csharp,
            String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_lang_cur_stream(literal_csharp,
            String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_named_const(TargetPrefixes, NamedConst),
        io.write_string(TargetPrefixes ^ csharp_prefix, !IO),
        io.write_string(NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        map.lookup(Info ^ csoi_code_addrs, CodeAddr, Name),
        io.write_string(Name, !IO)
    ;
        Const = mlconst_data_addr_local_var(VarName),
        local_var_name_to_string_for_csharp(VarName, VarNameStr),
        write_identifier_string_for_csharp(VarNameStr, !IO)
    ;
        Const = mlconst_data_addr_global_var(ModuleName, VarName),
        MangledModuleName = strip_mercury_and_mangle_sym_name_for_csharp(
            mlds_module_name_to_sym_name(ModuleName)),
        io.write_string(MangledModuleName, !IO),
        io.write_string(".", !IO),
        global_var_name_to_string_for_csharp(VarName, VarNameStr),
        write_identifier_string_for_csharp(VarNameStr, !IO)
    ;
        Const = mlconst_data_addr_rtti(ModuleName, RttiId),
        MangledModuleName = strip_mercury_and_mangle_sym_name_for_csharp(
            mlds_module_name_to_sym_name(ModuleName)),
        io.write_string(MangledModuleName, !IO),
        io.write_string(".", !IO),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        write_identifier_string_for_csharp(RttiAddrName, !IO)
    ;
        Const = mlconst_data_addr_tabling(_QualProcLabel, _TablingId),
        unexpected($pred, "NYI: mlconst_data_addr_tabling")
    ;
        Const = mlconst_null(Type),
        Initializer = get_type_initializer(Info, Type),
        io.write_string(Initializer, !IO)
    ).

:- func strip_mercury_and_mangle_sym_name_for_csharp(sym_name) = string.

strip_mercury_and_mangle_sym_name_for_csharp(SymName) = MangledSymName :-
    % XXX could use global::mercury. instead of stripping it
    ( if strip_outermost_qualifier(SymName, "mercury", StrippedSymName) then
        mangle_sym_name_for_csharp(StrippedSymName, module_qual, "__",
            MangledSymName)
    else
        mangle_sym_name_for_csharp(SymName, module_qual, "__",
            MangledSymName)
    ).

:- pred output_int_const_for_csharp(int::in, io::di, io::uo) is det.

output_int_const_for_csharp(N, !IO) :-
    ( if
        N < 0
    then
        io.write_int(N, !IO)
    else if
        N >> 32 = 0,
        N /\ 0x80000000 = 0x80000000
    then
        % The bit pattern fits in 32 bits, but is too big for a positive
        % integer. The C# compiler will give an error about this, unless we
        % tell it otherwise.
        io.format("unchecked((int) 0x%x)", [i(N /\ 0xffffffff)], !IO)
    else
        io.write_int(N, !IO)
    ).

:- pred output_uint_const_for_csharp(uint::in, io::di, io::uo) is det.

output_uint_const_for_csharp(U, !IO) :-
    io.write_uint(U, !IO),
    io.write_string("U", !IO).

:- pred output_int8_const_for_csharp(int8::in, io::di, io::uo) is det.

output_int8_const_for_csharp(I8, !IO) :-
    io.write_int8(I8, !IO).

:- pred output_uint8_const_for_csharp(uint8::in, io::di, io::uo) is det.

output_uint8_const_for_csharp(U8, !IO) :-
    io.write_uint8(U8, !IO).

:- pred output_int16_const_for_csharp(int16::in, io::di, io::uo) is det.

output_int16_const_for_csharp(I16, !IO) :-
    io.write_int16(I16, !IO).

:- pred output_uint16_const_for_csharp(uint16::in, io::di, io::uo) is det.

output_uint16_const_for_csharp(U16, !IO) :-
    io.write_uint16(U16, !IO).

:- pred output_int32_const_for_csharp(int32::in, io::di, io::uo) is det.

output_int32_const_for_csharp(I32, !IO) :-
    io.write_int32(I32, !IO).

:- pred output_uint32_const_for_csharp(uint32::in, io::di, io::uo) is det.

output_uint32_const_for_csharp(U32, !IO) :-
    io.write_uint32(U32, !IO),
    io.write_string("U", !IO).

%---------------------------------------------------------------------------%

:- pred mlds_output_code_addr_for_csharp(csharp_out_info::in,
    mlds_code_addr::in, bool::in, io::di, io::uo) is det.

mlds_output_code_addr_for_csharp(Info, CodeAddr, IsCall, !IO) :-
    CodeAddr = mlds_code_addr(QualFuncLabel, Signature),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    (
        IsCall = no,
        % Not a function call, so we are taking the address of the
        % wrapper for that function (method).
        TypeString = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        io.format("(%s) ", [s(TypeString)], !IO)
    ;
        IsCall = yes
    ),
    QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
    output_qual_name_prefix_cs(ModuleName, module_qual, !IO),
    mlds_output_proc_label(MaybeAuxSuffix, ProcLabel, !IO).

:- func method_ptr_type_to_string(csharp_out_info, mlds_arg_types,
    mlds_return_types) = string.

method_ptr_type_to_string(Info, ArgTypes, RetTypes) = String :-
    Arity = list.length(ArgTypes),
    NumRets = list.length(RetTypes),
    list.map(boxed_type_to_string_for_csharp(Info), ArgTypes, ArgTypesStrings),
    list.map(boxed_type_to_string_for_csharp(Info), RetTypes, RetTypesStrings),
    TypesString = string.join_list(", ", ArgTypesStrings ++ RetTypesStrings),
    string.format("runtime.MethodPtr%d_r%d<%s>",
        [i(Arity), i(NumRets), s(TypesString)], String).

:- pred mlds_output_proc_label(string::in, mlds_proc_label::in, io::di, io::uo)
    is det.

mlds_output_proc_label(Suffix, mlds_proc_label(PredLabel, ProcId), !IO) :-
    pred_label_to_string_for_csharp(PredLabel, PredLabelStr),
    proc_id_to_int(ProcId, ModeNum),
    string.format("%s_%d%s", [s(PredLabelStr), i(ModeNum), s(Suffix)], String),
    write_identifier_string_for_csharp(String, !IO).

%---------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations.
%

:- pred cs_output_context(bool::in, prog_context::in,
    io::di, io::uo) is det.

cs_output_context(OutputLineNumbers, Context, !IO) :-
    (
        OutputLineNumbers = yes,
        Context = term.context(File, Line),
        ( if
            Line > 0,
            File \= ""
        then
            io.format("#line %d ""%s""\n", [i(Line), s(File)], !IO)
        else
            true
        )
    ;
        OutputLineNumbers = no
    ).

:- pred indent_line_after_context(bool::in, prog_context::in,
    indent::in, io::di, io::uo) is det.

indent_line_after_context(OutputLineNumbers, Context, N, !IO) :-
    cs_output_context(OutputLineNumbers, Context, !IO),
    output_n_indents(N, !IO).

:- pred cs_output_default_context(bool::in, io::di, io::uo) is det.

cs_output_default_context(OutputLineNumbers, !IO) :-
    (
        OutputLineNumbers = yes,
        io.write_string("#line default\n", !IO)
    ;
        OutputLineNumbers = no
    ).

:- pred output_pragma_warning_disable(io::di, io::uo) is det.

output_pragma_warning_disable(!IO) :-
    % CS0162: Unreachable code detected.
    % CS0168: The variable `foo' is declared but never used.
    % CS0169: The private method `foo' is never used.
    % CS0219: The variable `foo' is assigned but its value is never used.
    % CS1717: Assignment made to same variable.
    io.write_string("#pragma warning disable 162, 168, 169, 219, 1717\n", !IO).

:- pred output_pragma_warning_restore(io::di, io::uo) is det.

output_pragma_warning_restore(!IO) :-
    io.write_string("#pragma warning restore\n", !IO).

%---------------------------------------------------------------------------%

:- type csharp_out_info
    --->    csharp_out_info(
                % These are static.
                csoi_module_info            :: module_info,
                csoi_auto_comments          :: bool,
                csoi_line_numbers           :: bool,
                csoi_foreign_line_numbers   :: bool,
                csoi_module_name            :: mlds_module_name,
                csoi_source_filename        :: string,
                csoi_code_addrs             :: map(mlds_code_addr, string),

                % These are dynamic.
                csoi_output_generics        :: output_generics,
                csoi_univ_tvars             :: list(tvar)
            ).

:- func init_csharp_out_info(module_info, string, map(mlds_code_addr, string))
    = csharp_out_info.

init_csharp_out_info(ModuleInfo, SourceFileName, CodeAddrs) = Info :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    Info = csharp_out_info(ModuleInfo, AutoComments,
        LineNumbers, ForeignLineNumbers, MLDS_ModuleName, SourceFileName,
        CodeAddrs, do_not_output_generics, []).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs.
%---------------------------------------------------------------------------%
