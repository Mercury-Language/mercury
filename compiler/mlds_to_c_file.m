%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mlds_to_c.m.
% Main author: fjh.
%
% Convert MLDS to C/C++ code.
%
% TODO:
%   - RTTI for debugging (module_layout, proc_layout, internal_layout)
%   - trail ops
%   - foreign language interfacing for languages other than C
%     (handle `foreign_body_code' and `foreign_code_decl' --
%     actually perhaps this should be done in an earlier pass,
%     in which case the only thing that would need to be done here
%     is to change some calls to sorry/2 to unexpected/2).
%   - packages, classes and inheritance
%     (currently we just generate all classes as structs)
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_file.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_succeeded.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % output_c_mlds(MLDS, Globals, TargetOrDump, Suffix, Succeeded, !IO):
    %
    % Output C code to the appropriate C file and C declarations to the
    % appropriate header file. The file names are determined by the module
    % name, with the specified Suffix appended at the end. (The suffix is used
    % for debugging dumps. For normal output, the suffix should be the empty
    % string.)
    %
:- pred output_c_mlds(mlds::in, globals::in, target_or_dump::in,
    string::in, maybe_succeeded::out, io::di, io::uo) is det.

    % output_c_dump_preds(MLDS, Globals, TargetOrDump, Suffix, DumpPreds, !IO):
    %
    % Output C code for the predicates and/or functions whose names
    % occurs in DumpPreds. The file name to write to is determined similarly
    % to how output_c_mlds does it, but with ".mlds_dump" replacing ".c".
    %
:- pred output_c_dump_preds(mlds::in, globals::in, target_or_dump::in,
    string::in, list(string)::in, io::di, io::uo) is det.

:- pred func_defn_has_name_in_list(list(string)::in, mlds_function_defn::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.         % for pred_proc_id.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds_to_c_class.
:- import_module ml_backend.mlds_to_c_export.
:- import_module ml_backend.mlds_to_c_func.
:- import_module ml_backend.mlds_to_c_global.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.

:- import_module assoc_list.
:- import_module bool.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

output_c_mlds(MLDS, Globals, TargetOrDump, Suffix, Succeeded, !IO) :-
    % We output the source file before we output the header.
    % The reason why we need this order is that the mmake dependencies
    % we generate say that the header file depends on the source file.
    % If we wrote them out in the other order, we would get an unnecessary
    % recompilation next time mmake is run.
    %
    % XXX At some point we should also handle output of any non-C
    % foreign code (Ada, Fortran, etc.) to appropriate files.
    %
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
    Opts = init_mlds_to_c_opts(Globals, SourceFileName, TargetOrDump),
    output_c_file_opts(MLDS, Opts, Suffix, Succeeded0, !IO),
    (
        Succeeded0 = succeeded,
        output_c_header_file_opts(MLDS, Opts, Suffix, Succeeded, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred output_c_file_opts(mlds::in, mlds_to_c_opts::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

output_c_file_opts(MLDS, Opts, Suffix, Succeeded, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    Globals = Opts ^ m2co_all_globals,
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".c")), ModuleName, SourceFileName0, !IO),
    SourceFileName = SourceFileName0 ++ Suffix,
    Indent = 0,
    output_to_file_stream(Globals, ModuleName, SourceFileName,
        mlds_output_src_file(Opts, Indent, MLDS), Succeeded, !IO).

:- pred output_c_header_file_opts(mlds::in, mlds_to_c_opts::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

output_c_header_file_opts(MLDS, Opts, Suffix, !:Succeeded, !IO) :-
    % We write the header file out to <module>.mih.tmp and then call
    % `update_interface' to move the <module>.mih.tmp file to <module>.mih.
    % This avoids updating the timestamp on the `.mih' file if it has not
    % changed.

    ModuleName = mlds_get_module_name(MLDS),
    Globals = Opts ^ m2co_all_globals,
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".mih")), ModuleName, MihFileName, !IO),
    HeaderFileName = MihFileName ++ Suffix,
    TmpHeaderFileName = HeaderFileName ++ ".tmp",
    globals.lookup_bool_option(Globals, line_numbers_for_c_headers,
        LineNumbersForCHdrs),
    HdrOpts = ((Opts
        ^ m2co_line_numbers := LineNumbersForCHdrs)
        ^ m2co_foreign_line_numbers := LineNumbersForCHdrs),
    Indent = 0,
    output_to_file_stream(Globals, ModuleName, TmpHeaderFileName,
        mlds_output_hdr_file(HdrOpts, Indent, MLDS), !:Succeeded, !IO),
    (
        !.Succeeded = succeeded,
        update_interface_report_any_error(Globals, ModuleName, HeaderFileName,
            !:Succeeded, !IO)
    ;
        !.Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

output_c_dump_preds(MLDS, Globals, TargetOrDump, Suffix, DumpPredNames, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
    Opts = init_mlds_to_c_opts(Globals, SourceFileName, TargetOrDump),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".mlds_dump")), ModuleName, DumpBaseName, !IO),
    DumpFileName = DumpBaseName ++ Suffix,
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ProcDefns = MLDS ^ mlds_proc_defns,
    list.filter(func_defn_has_name_in_list(DumpPredNames), ProcDefns,
        SelectedProcDefns),
    list.sort(SelectedProcDefns, SortedSelectedProcDefns),
    output_to_file_stream(Globals, ModuleName, DumpFileName,
        output_c_dump_func_defns(Opts, MLDS_ModuleName,
            SortedSelectedProcDefns),
        _Succeeded, !IO).

func_defn_has_name_in_list(DumpPredNames, FuncDefn) :-
    FuncDefn ^ mfd_function_name = mlds_function_name(PlainFuncName),
    PlainFuncName = mlds_plain_func_name(FuncLabel, _),
    FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
    ProcLabel = mlds_proc_label(PredLabel, _ProcId),
    PredLabel = mlds_user_pred_label(_PredOrFunc, _DeclModule, Name,
        _Arity, _CodeModel, _MaybeReturnValue),
    list.member(Name, DumpPredNames).

:- pred output_c_dump_func_defns(mlds_to_c_opts::in,
    mlds_module_name::in, list(mlds_function_defn)::in,
    io.text_output_stream::in, list(string)::out, io::di, io::uo) is det.

output_c_dump_func_defns(_, _, [], _, [], !IO).
output_c_dump_func_defns(Opts, ModuleName, [FuncDefn | FuncDefns],
        Stream, Errors, !IO) :-
    Indent = 0,
    mlds_output_function_defn(Opts, Stream, Indent, ModuleName, FuncDefn, !IO),
    output_c_dump_func_defns(Opts, ModuleName, FuncDefns, Stream, Errors, !IO).

%---------------------------------------------------------------------------%

:- pred mlds_output_hdr_file(mlds_to_c_opts::in, indent::in, mlds::in,
    io.text_output_stream::in, list(string)::out, io::di, io::uo) is det.

mlds_output_hdr_file(Opts, Indent, MLDS, Stream, Errors, !IO) :-
    % The header file must contain _definitions_ of all public types, but only
    % _declarations_ of all public variables, constants, and functions.
    %
    % Note that we do not forward-declare the types here; the forward
    % declarations that we need for types used in function prototypes
    % are generated by mlds_output_type_forward_decls.
    %
    % We sort the definitions before we print them so that a change that
    % reorders some predicates in a module, which would normally lead
    % to a change in the order of the corresponding MLDS definitions,
    % won't lead to a change in the contents of the .mih file we are
    % generating. This way, we can avoid recompiling the .c files
    % that depend on that .mih file.
    %
    % We do not sort the exported class definitions, because
    %
    % - there aren't any when the MLDS backend generates C code
    %   using low level data; and
    % - as of 2020 apr 11, the MLDS backend does not generate C code
    %   using high level data.

    MLDS = mlds(ModuleName, Imports, GlobalData,
        ClassDefns, TableStructDefns, ProcDefns,
        InitPreds, FinalPreds, AllForeignCode, ExportEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        _ScalarCellGroupMap, _VectorCellGroupMap, _AllocSites,
        RttiDefns, CellDefns, ClosureWrapperFuncDefns),

    list.filter(class_defn_is_private, ClassDefns,
        _PrivateClassDefns, PublicClassDefns),
    % When the MLDS backends targets C, the only class definitions we generate
    % are the definitions of environment structures. These are always private.
    expect(unify(PublicClassDefns, []), $pred, "PublicClassDefns != []"),
    list.filter(global_var_defn_is_private,
        RttiDefns ++ CellDefns ++ TableStructDefns,
        _PrivateGlobalVarDefns, PublicGlobarVarDefns),
    list.filter(function_defn_is_private,
        ClosureWrapperFuncDefns ++ ProcDefns,
        _PrivateFuncDefns, PublicFuncDefns),
    list.sort(PublicGlobarVarDefns, SortedPublicGlobarVarDefns),
    list.sort(PublicFuncDefns, SortedPublicFuncDefns),

    mlds_output_hdr_start(Opts, Stream, Indent, ModuleName, !IO),
    io.nl(Stream, !IO),
    mlds_output_hdr_imports(Stream, Indent, Imports, !IO),
    io.nl(Stream, !IO),

    % Get the foreign code for C.
    ForeignCode = mlds_get_c_foreign_code(AllForeignCode),
    mlds_output_c_hdr_decls(Opts, Stream, Indent, MLDS_ModuleName, ForeignCode,
        Errors, !IO),
    io.nl(Stream, !IO),
    mlds_output_export_enums(Opts, Stream, Indent, ExportEnums, !IO),
    io.nl(Stream, !IO),

    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
% See above.
%   list.foldl(mlds_output_class_defn(Opts, Stream, Indent, MLDS_ModuleName),
%       PublicClassDefns, !IO),
%   io.nl(Stream, !IO),
    StdOpts = Opts ^ m2co_std_func_decl := yes,
    mlds_output_global_var_decls(StdOpts, Stream, Indent, MLDS_ModuleName,
        SortedPublicGlobarVarDefns, !IO),
    mlds_output_function_decls(StdOpts, Stream, Indent, MLDS_ModuleName,
        SortedPublicFuncDefns, !IO),
    io.nl(Stream, !IO),
    mlds_output_init_fn_decls(Stream, MLDS_ModuleName, InitPreds, FinalPreds,
        !IO),
    io.nl(Stream, !IO),
    mlds_output_hdr_end(Opts, Stream, Indent, ModuleName, !IO).

:- pred mlds_output_hdr_imports(io.text_output_stream::in, indent::in,
    list(mlds_import)::in, io::di, io::uo) is det.

% XXX currently we assume all imports are source imports, i.e. that the header
% file does not depend on any types defined in other header files.
mlds_output_hdr_imports(_Stream, _Indent, _Imports, !IO).

:- pred mlds_output_src_imports(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, list(mlds_import)::in, io::di, io::uo) is det.

mlds_output_src_imports(Opts, Stream, Indent, Imports, !IO) :-
    Target = Opts ^ m2co_target,
    (
        Target = target_c,
        list.sort(Imports, SortedImports),
        list.foldl(mlds_output_src_import(Opts, Stream, Indent),
            SortedImports, !IO)
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        unexpected($pred, "expected target c")
    ).

:- pred mlds_output_src_import(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_import::in, io::di, io::uo) is det.

mlds_output_src_import(Opts, Stream, _Indent, Import, !IO) :-
    Import = mlds_import(ImportType, ModuleName0),
    (
        ImportType = user_visible_interface,
        HeaderOtherExt = other_ext(".mh")
    ;
        ImportType = compiler_visible_interface,
        HeaderOtherExt = other_ext(".mih")
    ),

    % Strip off the "mercury" qualifier for standard library modules.
    ( if
        strip_outermost_qualifier(ModuleName0, "mercury", ModuleName1),
        mercury_std_library_module_name(ModuleName1)
    then
        ModuleName = ModuleName1
    else
        ModuleName = ModuleName0
    ),

    Globals = Opts ^ m2co_all_globals,
    module_name_to_search_file_name(Globals, $pred,
        ext_other(HeaderOtherExt), ModuleName, HeaderFile, !IO),
    io.format(Stream, "#include \"%s\"\n", [s(HeaderFile)], !IO).

    % Generate the `.c' file.
    %
    % (Calling it the "source" file is a bit of a misnomer, since in our case
    % it is actually the target file, but there is no obvious alternative term
    % to use which also has a clear and concise abbreviation, so never mind...)
    %
:- pred mlds_output_src_file(mlds_to_c_opts::in, indent::in, mlds::in,
    io.text_output_stream::in, list(string)::out, io::di, io::uo) is det.

mlds_output_src_file(Opts, Indent, MLDS, Stream, Errors, !IO) :-
    % The public types have already been defined in the header file, and the
    % public vars, consts, and functions have already been declared in the
    % header file. In the source file, we need to have
    %
    %   #1. definitions of the private types,
    %   #2. forward declarations of the private non-types
    %   #3. definitions of all the non-types
    %   #4. initialization functions
    %
    % in that order.
    %
    % #2 is needed to allow #3 to contain forward references, which can arise
    % for e.g. mutually recursive procedures. #1 is needed since #2 may refer
    % to the types.
    %
    % Note that we do not forward-declare the types here; the forward
    % declarations that we need for types used in function prototypes
    % are generated by mlds_output_type_forward_decls.

    MLDS = mlds(ModuleName, Imports, GlobalData,
        ClassDefns, TableStructDefns, ProcDefns,
        InitPreds, FinalPreds, AllForeignCode, _ExportEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, AllocSites,
        RttiDefns, CellDefns, ClosureWrapperFuncDefns),

    GlobalVarDefns = RttiDefns ++ CellDefns ++ TableStructDefns,
    list.filter(global_var_defn_is_private, GlobalVarDefns,
        PrivateGlobalVarDefns),
    FuncDefns = ClosureWrapperFuncDefns ++ ProcDefns,
    list.filter(function_defn_is_private, FuncDefns, PrivateFuncDefns),
    list.filter(class_defn_is_private, ClassDefns, PrivateClassDefns),
    list.filter(global_var_defn_is_type_ctor_info, RttiDefns,
        TypeCtorInfoDefns),

    map.to_assoc_list(ScalarCellGroupMap, ScalarCellGroups),
    map.to_assoc_list(VectorCellGroupMap, VectorCellGroups),

    ForeignCode = mlds_get_c_foreign_code(AllForeignCode),
    EnvVarNameSet = mlds_get_env_var_names(ProcDefns),
    set.to_sorted_list(EnvVarNameSet, EnvVarNames),
    mlds_output_src_start(Opts, Stream, Indent, ModuleName, ForeignCode,
        InitPreds, FinalPreds, EnvVarNames, !IO),
    io.nl(Stream, !IO),
    mlds_output_src_imports(Opts, Stream, Indent, Imports, !IO),
    io.nl(Stream, !IO),

    mlds_output_c_decls(Opts, Stream, Indent, ForeignCode,
        ForeignDeclErrors, !IO),
    io.nl(Stream, !IO),

    list.foldl(mlds_output_env_var_decl(Stream), EnvVarNames, !IO),

    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    list.foldl(mlds_output_class_defn(Opts, Stream, Indent, MLDS_ModuleName),
        PrivateClassDefns, !IO),
    io.nl(Stream, !IO),
    mlds_output_global_var_decls(Opts, Stream, Indent, MLDS_ModuleName,
        PrivateGlobalVarDefns, !IO),
    mlds_output_function_decls(Opts, Stream, Indent, MLDS_ModuleName,
        PrivateFuncDefns, !IO),
    io.nl(Stream, !IO),

    ModuleSymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
    MangledModuleName = sym_name_mangle(ModuleSymName),

    mlds_output_scalar_cell_group_decls(Opts, Stream, Indent,
        MangledModuleName, ScalarCellGroups, !IO),
    io.nl(Stream, !IO),
    mlds_output_vector_cell_group_decls(Opts, Stream, Indent, MLDS_ModuleName,
        MangledModuleName, VectorCellGroups, !IO),
    io.nl(Stream, !IO),
    mlds_output_alloc_site_decls(Stream, Indent, AllocSites, !IO),
    io.nl(Stream, !IO),

    mlds_output_scalar_cell_group_defns(Opts, Stream, Indent,
        MangledModuleName, ScalarCellGroups, !IO),
    io.nl(Stream, !IO),
    mlds_output_vector_cell_group_defns(Opts, Stream, Indent,
        MangledModuleName, VectorCellGroups, !IO),
    io.nl(Stream, !IO),
    mlds_output_alloc_site_defns(Opts, Stream, Indent, MLDS_ModuleName,
        AllocSites, !IO),
    io.nl(Stream, !IO),

    mlds_output_c_defns(Opts, Stream, MLDS_ModuleName, Indent, ForeignCode,
        ForeignCodeErrors, !IO),
    io.nl(Stream, !IO),
    mlds_output_global_var_defns(Opts, Stream, Indent, yes,
        MLDS_ModuleName, RttiDefns, !IO),
    mlds_output_function_defns(Opts, blank_line_start, Stream, Indent,
        MLDS_ModuleName, ClosureWrapperFuncDefns, !IO),
    mlds_output_global_var_defns(Opts, Stream, Indent, yes,
        MLDS_ModuleName, CellDefns, !IO),
    mlds_output_global_var_defns(Opts, Stream, Indent, yes,
        MLDS_ModuleName, TableStructDefns, !IO),
    mlds_output_function_defns(Opts, blank_line_start, Stream, Indent,
        MLDS_ModuleName, ProcDefns, !IO),
    io.nl(Stream, !IO),
    mlds_output_init_fn_defns(Opts, Stream, MLDS_ModuleName, FuncDefns,
        TypeCtorInfoDefns, AllocSites, InitPreds, FinalPreds, !IO),
    io.nl(Stream, !IO),
    mlds_output_grade_check_fn_defn(Stream, MLDS_ModuleName, !IO),
    io.nl(Stream, !IO),
    mlds_output_src_end(Stream, Indent, ModuleName, !IO),

    Errors = ForeignDeclErrors ++ ForeignCodeErrors.

:- func mlds_get_env_var_names(list(mlds_function_defn)) = set(string).

mlds_get_env_var_names(FuncDefns) = EnvVarNameSet :-
    list.map(mlds_get_env_var_names_from_defn, FuncDefns, EnvVarNameSets),
    EnvVarNameSet = set.union_list(EnvVarNameSets).

:- pred mlds_get_env_var_names_from_defn(mlds_function_defn::in,
    set(string)::out) is det.

mlds_get_env_var_names_from_defn(FuncDefn, EnvVarNameSet) :-
    EnvVarNameSet = FuncDefn ^ mfd_env_vars.

:- pred mlds_output_env_var_decl(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

mlds_output_env_var_decl(Stream, EnvVarName, !IO) :-
    io.format(Stream, "extern MR_Word %s;\n",
        [s(global_var_name(env_var_ref(EnvVarName)))], !IO).

:- pred mlds_output_hdr_start(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mercury_module_name::in, io::di, io::uo) is det.

mlds_output_hdr_start(Opts, Stream, Indent, ModuleName, !IO) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    MangledModuleNameStr = sym_name_mangle(ModuleName),
    mlds_output_auto_gen_comment(Opts, Stream, ModuleName, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "// :- module %s.\n",
        [s(ModuleNameStr)], !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "// :- interface.\n\n", !IO),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "#ifndef MR_HEADER_GUARD_%s\n",
        [s(MangledModuleNameStr)], !IO),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "#define MR_HEADER_GUARD_%s\n\n",
        [s(MangledModuleNameStr)], !IO),

    % If we are outputting C (rather than C++), then add a conditional
    % `extern "C"' wrapper around the header file, so that the header file
    % can be #included by C++ programs.
    Target = Opts ^ m2co_target,
    (
        Target = target_c,
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "#ifdef __cplusplus\n", !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "extern ""C"" {\n", !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "#endif\n", !IO),
        io.nl(Stream, !IO)
    ;
        ( Target = target_java
        ; Target = target_csharp
        )
    ),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "#include ""mercury.h""\n", !IO).

:- pred mlds_output_src_start(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mercury_module_name::in, mlds_foreign_code::in,
    list(string)::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

mlds_output_src_start(Opts, Stream, Indent, ModuleName, ForeignCode,
        InitPreds, FinalPreds, EnvVarNames, !IO) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    mlds_output_auto_gen_comment(Opts, Stream, ModuleName, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "// :- module %s.\n", [s(ModuleNameStr)], !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "// :- implementation.\n", !IO),
    mlds_output_src_bootstrap_defines(Stream, !IO),
    io.nl(Stream, !IO),
    output_init_c_comment(Stream, ModuleName, InitPreds, FinalPreds,
        EnvVarNames, !IO),

    CompilerImport = mlds_import(compiler_visible_interface, ModuleName),
    mlds_output_src_import(Opts, Stream, Indent, CompilerImport, !IO),

    % If there are `:- pragma foreign_export' declarations,
    % #include the `.mh' file.
    ForeignCode = mlds_foreign_code(_, _, _, Exports),
    (
        Exports = []
    ;
        Exports = [_ | _],
        UserImport = mlds_import(user_visible_interface, ModuleName),
        mlds_output_src_import(Opts, Stream, Indent, UserImport, !IO)
    ),
    io.nl(Stream, !IO).

    % Output any #defines which are required to bootstrap in the hlc grade.
    %
:- pred mlds_output_src_bootstrap_defines(io.text_output_stream::in,
    io::di, io::uo) is det.

mlds_output_src_bootstrap_defines(_, !IO).

:- pred mlds_output_hdr_end(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mercury_module_name::in, io::di, io::uo) is det.

mlds_output_hdr_end(Opts, Stream, Indent, ModuleName, !IO) :-
    Target = Opts ^ m2co_target,
    (
        Target = target_c,
        % Terminate the `extern "C"' wrapper.
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "#ifdef __cplusplus\n", !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "}\n", !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "#endif\n", !IO),
        io.nl(Stream, !IO)
    ;
        ( Target = target_csharp
        ; Target = target_java
        )
    ),
    ModuleNameStr = sym_name_to_string(ModuleName),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "#endif // MR_HEADER_GUARD_%s\n\n",
        [s(ModuleNameStr)], !IO),
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "// :- end_interface %s.\n",
        [s(ModuleNameStr)], !IO).

:- pred mlds_output_src_end(io.text_output_stream::in, indent::in,
    mercury_module_name::in, io::di, io::uo) is det.

mlds_output_src_end(Stream, Indent, ModuleName, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    io.format(Stream, "// :- end_module %s.\n",
        [s(sym_name_to_string(ModuleName))], !IO).

    % Output a C comment saying that the file was automatically generated
    % (and giving details such as the compiler version).
    %
:- pred mlds_output_auto_gen_comment(mlds_to_c_opts::in,
    io.text_output_stream::in, module_name::in, io::di, io::uo) is det.

mlds_output_auto_gen_comment(Opts, Stream, ModuleName, !IO) :-
    library.version(Version, Fullarch),
    Globals = Opts ^ m2co_all_globals,
    module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
    output_c_file_intro_and_grade(Globals, Stream, SourceFileName, Version,
        Fullarch, !IO),
    io.nl(Stream, !IO).

    % Output a reference to the mangled grade name for the grade that the C
    % file gets compiled with. This ensures that we do not try to link objects
    % files compiled in different grades.
    %
:- pred mlds_output_grade_check_fn_defn(io.text_output_stream::in,
    mlds_module_name::in, io::di, io::uo) is det.

mlds_output_grade_check_fn_defn(Stream, ModuleName, !IO) :-
    io.write_string(Stream,
        "// Ensure everything is compiled with the same grade.\n", !IO),
    GradeCheckDecl = grade_check_fn_decl_to_string(ModuleName),
    io.format(Stream, "%s\n", [s(GradeCheckDecl)], !IO),
    io.format(Stream, "{\n", [], !IO),
    io.format(Stream, "    return &MR_GRADE_VAR;\n", [], !IO),
    io.format(Stream, "}\n", [], !IO).

    % Get the foreign code for C.
    %
:- func mlds_get_c_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_c_foreign_code(AllForeignCode) = ForeignCode :-
    ( if map.search(AllForeignCode, lang_c, ForeignCode0) then
        ForeignCode = ForeignCode0
    else
        % This can occur when compiling to a non-C target using
        % "--mlds-dump all".
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%---------------------------------------------------------------------------%

    % Maybe output the function `mercury__<modulename>__init()'.
    % The body of the function consists of calls MR_init_entry(<function>)
    % for each function defined in the module.
    %
    % If there are any user-defined intialisation or finalisation predicates
    % then output the functions: `mercury__<modulename>__required_init()' and
    % `mercury__<modulename>__required_final()' as necessary.
    %
    % XXX The above comment seems to have suffered bit-rot.
    %
:- pred mlds_output_init_fn_decls(io.text_output_stream::in,
    mlds_module_name::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

mlds_output_init_fn_decls(Stream, ModuleName,
        InitFuncNames, FinalFuncNames, !IO) :-
    ModuleInitDecl = init_fn_decl_to_string(ModuleName, ""),
    TypeTablesDecl = init_fn_decl_to_string(ModuleName, "_type_tables"),
    DebuggerDecl = init_fn_decl_to_string(ModuleName, "_debugger"),
    io.format(Stream, "%s;\n", [s(ModuleInitDecl)], !IO),
    io.format(Stream, "%s;\n", [s(TypeTablesDecl)], !IO),
    io.format(Stream, "%s;\n", [s(DebuggerDecl)], !IO),
    (
        InitFuncNames = []
    ;
        InitFuncNames = [_ | _],
        ReqInitDecl = required_fn_decl_to_string(ModuleName, "init"),
        io.format(Stream, "%s;\n", [s(ReqInitDecl)], !IO)
    ),
    (
        FinalFuncNames = []
    ;
        FinalFuncNames = [_ | _],
        ReqFinalDecl = required_fn_decl_to_string(ModuleName, "final"),
        io.format(Stream, "%s;\n", [s(ReqFinalDecl)], !IO)
    ),
    GradeCheckDecl = grade_check_fn_decl_to_string(ModuleName),
    io.format(Stream, "%s;\n", [s(GradeCheckDecl)], !IO).

:- pred mlds_output_init_fn_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_module_name::in,
    list(mlds_function_defn)::in, list(mlds_global_var_defn)::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

mlds_output_init_fn_defns(Opts, Stream, ModuleName, FuncDefns,
        TypeCtorInfoDefns, AllocSites, InitFuncNames, FinalFuncNames, !IO) :-
    ModuleInitDecl = init_fn_decl_to_string(ModuleName, ""),
    TypeTablesDecl = init_fn_decl_to_string(ModuleName, "_type_tables"),
    DebuggerDecl = init_fn_decl_to_string(ModuleName, "_debugger"),
    io.format(Stream, "%s\n{\n", [s(ModuleInitDecl)], !IO),
    NeedToInit = Opts ^ m2co_need_to_init,
    ( if
        NeedToInit = yes,
        FuncDefns = [_ | _]
    then
        io.write_strings(Stream,
            ["\tstatic MR_bool initialised = MR_FALSE;\n",
            "\tif (initialised) return;\n",
            "\tinitialised = MR_TRUE;\n\n"], !IO),
        mlds_output_calls_to_init_entry(Stream, ModuleName, FuncDefns, !IO),
        mlds_output_call_to_register_alloc_sites(Stream, AllocSites, !IO)
    else
        true
    ),
    io.write_string(Stream, "}\n\n", !IO),

    io.format(Stream, "%s\n{\n", [s(TypeTablesDecl)], !IO),
    (
        TypeCtorInfoDefns = [_ | _],
        io.write_strings(Stream,
            ["\tstatic MR_bool initialised = MR_FALSE;\n",
            "\tif (initialised) return;\n",
            "\tinitialised = MR_TRUE;\n\n"], !IO),
        mlds_output_calls_to_register_tci(Stream, ModuleName,
            TypeCtorInfoDefns, !IO)
    ;
        TypeCtorInfoDefns = []
    ),
    io.write_string(Stream, "}\n\n", !IO),

    io.format(Stream, "%s\n{\n", [s(DebuggerDecl)], !IO),
    io.write_string(Stream,
        "\tMR_fatal_error(""debugger initialization in MLDS grade"");\n", !IO),
    io.write_string(Stream, "}\n", !IO),

    % Maybe write out wrapper functions that call user-defined intialisation
    % and finalisation predicates.
    (
        InitFuncNames = []
    ;
        InitFuncNames = [_ | _],
        ReqInitDecl = required_fn_decl_to_string(ModuleName, "init"),
        io.format(Stream, "\n%s\n{\n", [s(ReqInitDecl)], !IO),
        output_calls_to_void_funcs(Stream, InitFuncNames, !IO),
        io.write_string(Stream, "}\n", !IO)
    ),
    (
        FinalFuncNames = []
    ;
        FinalFuncNames = [_ | _],
        ReqFinalDecl = required_fn_decl_to_string(ModuleName, "final"),
        io.format(Stream, "\n%s\n{\n", [s(ReqFinalDecl)], !IO),
        output_calls_to_void_funcs(Stream, FinalFuncNames, !IO),
        io.write_string(Stream, "}\n", !IO)
    ).

:- pred output_calls_to_void_funcs(io.text_output_stream::in,
    list(string)::in, io::di, io::uo) is det.

output_calls_to_void_funcs(_, [], !IO).
output_calls_to_void_funcs(Stream, [FuncName | FuncNames], !IO) :-
    io.format(Stream, "\t%s();\n", [s(FuncName)], !IO),
    output_calls_to_void_funcs(Stream, FuncNames, !IO).

:- func init_fn_decl_to_string(mlds_module_name, string) = string.

init_fn_decl_to_string(ModuleName, Suffix) = Decl :-
    ModuleNameString = module_name_to_function_name(ModuleName),
    string.format("void %s__init%s(void)",
        [s(ModuleNameString), s(Suffix)], Decl).

:- func required_fn_decl_to_string(mlds_module_name, string) = string.

required_fn_decl_to_string(ModuleName, Suffix) = Str :-
    ModuleNameString = module_name_to_function_name(ModuleName),
    string.format("void %s__required_%s(void)",
        [s(ModuleNameString), s(Suffix)], Str).

:- func grade_check_fn_decl_to_string(mlds_module_name) = string.

grade_check_fn_decl_to_string(ModuleName) = Decl :-
    ModuleNameString = module_name_to_function_name(ModuleName),
    string.format("const char *%s__grade_check(void)",
        [s(ModuleNameString)], Decl).

    % Generate calls to MR_init_entry() for the specified functions.
    %
:- pred mlds_output_calls_to_init_entry(io.text_output_stream::in,
    mlds_module_name::in, list(mlds_function_defn)::in, io::di, io::uo) is det.

mlds_output_calls_to_init_entry(_, _, [], !IO).
mlds_output_calls_to_init_entry(Stream, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    FuncName = FuncDefn ^ mfd_function_name,
    io.write_string(Stream, "\tMR_init_entry(", !IO),
    mlds_output_fully_qualified_function_name(Stream,
        qual_function_name(ModuleName, FuncName), !IO),
    io.write_string(Stream, ");\n", !IO),
    mlds_output_calls_to_init_entry(Stream, ModuleName, FuncDefns, !IO).

    % Generate calls to MR_register_type_ctor_info() for the specified
    % type_ctor_infos.
    %
:- pred mlds_output_calls_to_register_tci(io.text_output_stream::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

mlds_output_calls_to_register_tci(__, _, [], !IO).
mlds_output_calls_to_register_tci(Stream, MLDS_ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarName = GlobalVarDefn ^ mgvd_name,
    io.write_string(Stream, "\tMR_register_type_ctor_info(&", !IO),
    mlds_output_maybe_qualified_global_var_name(Stream, MLDS_ModuleName,
        GlobalVarName, !IO),
    io.write_string(Stream, ");\n", !IO),
    mlds_output_calls_to_register_tci(Stream, MLDS_ModuleName,
        GlobalVarDefns, !IO).

    % Generate call to MR_register_alloc_sites.
    %
:- pred mlds_output_call_to_register_alloc_sites(io.text_output_stream::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

mlds_output_call_to_register_alloc_sites(Stream, AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        list.length(AllocSites, NumAllocSites),
        io.format(Stream,
            "\tMR_register_alloc_sites(MR_alloc_sites, %d);\n",
            [i(NumAllocSites)], !IO)
    ).

:- func module_name_to_function_name(mlds_module_name) = string.

module_name_to_function_name(ModuleName) = ModuleNameString :-
    % Ensure that we only get one "mercury__" at the start
    % of the function name.
    ModuleNameString0 = sym_name_mangle(
        mlds_module_name_to_sym_name(ModuleName)),
    ( if string.prefix(ModuleNameString0, "mercury__") then
        ModuleNameString = ModuleNameString0
    else
        ModuleNameString = "mercury__" ++ ModuleNameString0
    ).

%---------------------------------------------------------------------------%
%
% Foreign language interface stuff.
%

:- pred mlds_output_c_hdr_decls(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_module_name::in, mlds_foreign_code::in, list(string)::out,
    io::di, io::uo) is det.

mlds_output_c_hdr_decls(Opts, Stream, Indent, ModuleName, ForeignCode,
        Errors, !IO) :-
    ForeignCode = mlds_foreign_code(DeclCodes, _BodyCodes, _Imports,
        _ExportDefns),
    ( if is_std_lib_module(ModuleName, StdlibModuleName) then
        SymName = StdlibModuleName
    else
        SymName = mlds_module_name_to_sym_name(ModuleName)
    ),

    DeclGuard = decl_guard(SymName),
    io.format(Stream, "#ifndef %s\n#define %s\n",
        [s(DeclGuard), s(DeclGuard)], !IO),

    % We need to make sure we #include the .mih files for any ancestor modules
    % in cases any foreign_types defined in them are referenced by the extern
    % declarations required by mutables.

    AncestorModuleNames = get_ancestors(SymName),
    list.map(module_name_to_file_name_stem,
        AncestorModuleNames, AncestorFileNames),
    WriteAncestorInclude =
        ( pred(Ancestor::in, !.IO::di, !:IO::uo) is det :-
            io.format(Stream, "#include \"%s.mih\"\n", [s(Ancestor)], !IO)
        ),
    list.foldl(WriteAncestorInclude, AncestorFileNames, !IO),
    list.map_foldl(
        mlds_output_c_hdr_decl(Opts, Stream, Indent,
            yes(foreign_decl_is_exported)),
        DeclCodes, DeclResults, !IO),
    list.filter_map(maybe_is_error, DeclResults, Errors),
    io.write_string(Stream, "\n#endif\n", !IO).

:- pred mlds_output_c_hdr_decl(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, maybe(foreign_decl_is_local)::in, foreign_decl_code::in,
    maybe_error::out, io::di, io::uo) is det.

mlds_output_c_hdr_decl(Opts, Stream, _Indent, MaybeDesiredIsLocal, DeclCode,
        Res, !IO) :-
    DeclCode = foreign_decl_code(Lang, IsLocal, LiteralOrInclude, Context),
    % Only output C code in the C header file.
    (
        Lang = lang_c,
        ( if
            (
                MaybeDesiredIsLocal = no
            ;
                MaybeDesiredIsLocal = yes(DesiredIsLocal),
                IsLocal = DesiredIsLocal
            )
        then
            mlds_output_foreign_literal_or_include(Opts, Stream,
                LiteralOrInclude, Context, Res, !IO)
        else
            Res = ok
        )
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_c_decls(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_foreign_code::in, list(string)::out,
    io::di, io::uo) is det.

mlds_output_c_decls(Opts, Stream, Indent, ForeignCode, Errors, !IO) :-
    ForeignCode = mlds_foreign_code(HeaderCodes, _BodyCodes, _Imports,
        _ExportDefns),
    list.map_foldl(
        mlds_output_c_hdr_decl(Opts, Stream, Indent,
            yes(foreign_decl_is_local)),
        HeaderCodes, Results, !IO),
    list.filter_map(maybe_is_error, Results, Errors).

:- pred mlds_output_c_defns(mlds_to_c_opts::in, io.text_output_stream::in,
    mlds_module_name::in, indent::in, mlds_foreign_code::in, list(string)::out,
    io::di, io::uo) is det.

mlds_output_c_defns(Opts, Stream, ModuleName, Indent, ForeignCode,
        Errors, !IO) :-
    ForeignCode = mlds_foreign_code(_HeaderCodes, BodyCodes,
        Imports, ExportDefns),
    list.foldl(mlds_output_c_foreign_import_module(Opts, Stream, Indent),
        Imports, !IO),
    list.map_foldl(mlds_output_c_defn(Opts, Stream, Indent),
        BodyCodes, Results, !IO),
    io.write_string(Stream, "\n", !IO),
    write_out_list(mlds_output_pragma_export_defn(Opts, ModuleName, Indent),
        "\n", ExportDefns, Stream, !IO),
    list.filter_map(maybe_is_error, Results, Errors).

:- pred mlds_output_c_foreign_import_module(mlds_to_c_opts::in,
    io.text_output_stream::in, int::in, fim_spec::in, io::di, io::uo) is det.

mlds_output_c_foreign_import_module(Opts, Stream, Indent, FIMSpec, !IO) :-
    FIMSpec = fim_spec(Lang, Import),
    (
        Lang = lang_c,
        UserImport = mlds_import(user_visible_interface, Import),
        mlds_output_src_import(Opts, Stream, Indent, UserImport, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_c_defn(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, foreign_body_code::in, maybe_error::out,
    io::di, io::uo) is det.

mlds_output_c_defn(Opts, Stream, _Indent, ForeignBodyCode, Res, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    (
        Lang = lang_c,
        mlds_output_foreign_literal_or_include(Opts, Stream, LiteralOrInclude,
            Context, Res, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_foreign_literal_or_include(mlds_to_c_opts::in,
    io.text_output_stream::in, foreign_literal_or_include::in,
    prog_context::in, maybe_error::out, io::di, io::uo) is det.

mlds_output_foreign_literal_or_include(Opts, Stream, LiteralOrInclude,
        Context, Res, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        c_output_context(Stream, Opts ^ m2co_foreign_line_numbers,
            Context, !IO),
        io.write_string(Stream, Code, !IO),
        Res = ok
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        SourceFileName = Opts ^ m2co_source_filename,
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        c_output_file_line(Stream, Opts ^ m2co_foreign_line_numbers,
            IncludePath, 1, !IO),
        write_include_file_contents(Stream, IncludePath, Res, !IO)
    ),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_file.
%---------------------------------------------------------------------------%
