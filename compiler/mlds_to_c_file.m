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
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module bool.
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
    string::in, bool::out, io::di, io::uo) is det.

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
:- import_module backend_libs.name_mangle.
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
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
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
        Succeeded0 = yes,
        output_c_header_file_opts(MLDS, Opts, Suffix, Succeeded, !IO)
    ;
        Succeeded0 = no,
        Succeeded = no
    ).

:- pred output_c_file_opts(mlds::in, mlds_to_c_opts::in, string::in,
    bool::out, io::di, io::uo) is det.

output_c_file_opts(MLDS, Opts, Suffix, Succeeded, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    Globals = Opts ^ m2co_all_globals,
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".c")), ModuleName, SourceFileName0, !IO),
    SourceFileName = SourceFileName0 ++ Suffix,
    Indent = 0,
    output_to_file(Globals, SourceFileName,
        mlds_output_src_file(Opts, Indent, MLDS), Succeeded, !IO).

:- pred output_c_header_file_opts(mlds::in, mlds_to_c_opts::in, string::in,
    bool::out, io::di, io::uo) is det.

output_c_header_file_opts(MLDS, Opts, Suffix, Succeeded, !IO) :-
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
    output_to_file(Globals, TmpHeaderFileName,
        mlds_output_hdr_file(HdrOpts, Indent, MLDS), Succeeded, !IO),
    (
        Succeeded = yes,
        update_interface(Globals, HeaderFileName, !IO)
    ;
        Succeeded = no
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
    output_to_file(Globals, DumpFileName,
        mlds_output_named_function_defns(Opts, DumpPredNames, MLDS_ModuleName,
            ProcDefns),
        _Succeeded, !IO).

func_defn_has_name_in_list(DumpPredNames, FuncDefn) :-
    FuncDefn ^ mfd_function_name = mlds_function_name(PlainFuncName),
    PlainFuncName = mlds_plain_func_name(FuncLabel, _),
    FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
    ProcLabel = mlds_proc_label(PredLabel, _ProcId),
    PredLabel = mlds_user_pred_label(_PredOrFunc, _DeclModule, Name,
        _Arity, _CodeModel, _MaybeReturnValue),
    list.member(Name, DumpPredNames).

:- pred mlds_output_named_function_defns(mlds_to_c_opts::in,
    list(string)::in, mlds_module_name::in, list(mlds_function_defn)::in,
    list(string)::out, io::di, io::uo) is det.

mlds_output_named_function_defns(Opts, DumpPredNames, ModuleName, FuncDefns,
        Errors, !IO) :-
    (
        FuncDefns = [],
        Errors = []
    ;
        FuncDefns = [FuncDefn | FuncDefnsTail],
        ( if func_defn_has_name_in_list(DumpPredNames, FuncDefn) then
            Indent = 0,
            mlds_output_function_defn(Opts, Indent, ModuleName, FuncDefn, !IO)
        else
            true
        ),
        mlds_output_named_function_defns(Opts, DumpPredNames, ModuleName,
            FuncDefnsTail, Errors, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mlds_output_hdr_file(mlds_to_c_opts::in, indent::in, mlds::in,
    list(string)::out, io::di, io::uo) is det.

mlds_output_hdr_file(Opts, Indent, MLDS, Errors, !IO) :-
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
    % We do not sort the type (i.e. class) definitions, because
    % (a) we do not generate any in MLDS grades (such as hlc.gc)
    %     that use low-level data, so for them it does not matter
    %     whether we sort them or not, and
    % (b) I (zs) do not know whether the order is important in MLDS grades
    %     (such as hl.gc) that use high-level data.

    MLDS = mlds(ModuleName, Imports, GlobalData,
        TypeDefns, TableStructDefns, ProcDefns,
        InitPreds, FinalPreds, AllForeignCode, ExportEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        _ScalarCellGroupMap, _VectorCellGroupMap, _AllocSites,
        RttiDefns, CellDefns, ClosureWrapperFuncDefns),

    list.filter(class_defn_is_private, TypeDefns,
        _PrivateTypeDefns, PublicTypeDefns),
    list.filter(global_var_defn_is_private,
        RttiDefns ++ CellDefns ++ TableStructDefns,
        _PrivateGlobalVarDefns, PublicGlobarVarDefns),
    list.filter(function_defn_is_private,
        ClosureWrapperFuncDefns ++ ProcDefns,
        _PrivateFuncDefns, PublicFuncDefns),
    list.sort(PublicGlobarVarDefns, SortedPublicGlobarVarDefns),
    list.sort(PublicFuncDefns, SortedPublicFuncDefns),

    mlds_output_hdr_start(Opts, Indent, ModuleName, !IO),
    io.nl(!IO),
    mlds_output_hdr_imports(Indent, Imports, !IO),
    io.nl(!IO),

    % Get the foreign code for C.
    ForeignCode = mlds_get_c_foreign_code(AllForeignCode),
    mlds_output_c_hdr_decls(Opts, Indent, MLDS_ModuleName, ForeignCode,
        Errors, !IO),
    io.nl(!IO),
    mlds_output_export_enums(Opts, Indent, ExportEnums, !IO),
    io.nl(!IO),

    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    list.foldl(mlds_output_class_defn(Opts, Indent, MLDS_ModuleName),
        PublicTypeDefns, !IO),
    io.nl(!IO),
    StdOpts = Opts ^ m2co_std_func_decl := yes,
    mlds_output_global_var_decls(StdOpts, Indent, MLDS_ModuleName,
        SortedPublicGlobarVarDefns, !IO),
    mlds_output_function_decls(StdOpts, Indent, MLDS_ModuleName,
        SortedPublicFuncDefns, !IO),
    io.nl(!IO),
    mlds_output_init_fn_decls(MLDS_ModuleName, InitPreds, FinalPreds, !IO),
    io.nl(!IO),
    mlds_output_hdr_end(Opts, Indent, ModuleName, !IO).

:- pred mlds_output_hdr_imports(indent::in, list(mlds_import)::in,
    io::di, io::uo) is det.

% XXX currently we assume all imports are source imports, i.e. that the header
% file does not depend on any types defined in other header files.
mlds_output_hdr_imports(_Indent, _Imports, !IO).

:- pred mlds_output_src_imports(mlds_to_c_opts::in, indent::in,
    list(mlds_import)::in, io::di, io::uo) is det.

mlds_output_src_imports(Opts, Indent, Imports, !IO) :-
    Target = Opts ^ m2co_target,
    (
        Target = target_c,
        list.sort(Imports, SortedImports),
        list.foldl(mlds_output_src_import(Opts, Indent), SortedImports, !IO)
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        unexpected($pred, "expected target c")
    ).

:- pred mlds_output_src_import(mlds_to_c_opts::in, indent::in, mlds_import::in,
    io::di, io::uo) is det.

mlds_output_src_import(Opts, _Indent, Import, !IO) :-
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
    io.write_strings(["#include """, HeaderFile, """\n"], !IO).

    % Generate the `.c' file.
    %
    % (Calling it the "source" file is a bit of a misnomer, since in our case
    % it is actually the target file, but there is no obvious alternative term
    % to use which also has a clear and concise abbreviation, so never mind...)
    %
:- pred mlds_output_src_file(mlds_to_c_opts::in, indent::in, mlds::in,
    list(string)::out, io::di, io::uo) is det.

mlds_output_src_file(Opts, Indent, MLDS, Errors, !IO) :-
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
        TypeDefns, TableStructDefns, ProcDefns,
        InitPreds, FinalPreds, AllForeignCode, _ExportEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, AllocSites,
        RttiDefns, CellDefns, ClosureWrapperFuncDefns),

    GlobalVarDefns = RttiDefns ++ CellDefns ++ TableStructDefns,
    list.filter(global_var_defn_is_private, GlobalVarDefns,
        PrivateGlobalVarDefns),
    FuncDefns = ClosureWrapperFuncDefns ++ ProcDefns,
    list.filter(function_defn_is_private, FuncDefns, PrivateFuncDefns),
    list.filter(class_defn_is_private, TypeDefns, PrivateTypeDefns),
    list.filter(global_var_defn_is_type_ctor_info, RttiDefns,
        TypeCtorInfoDefns),

    map.to_assoc_list(ScalarCellGroupMap, ScalarCellGroups),
    map.to_assoc_list(VectorCellGroupMap, VectorCellGroups),

    ForeignCode = mlds_get_c_foreign_code(AllForeignCode),
    EnvVarNameSet = mlds_get_env_var_names(ProcDefns),
    set.to_sorted_list(EnvVarNameSet, EnvVarNames),
    mlds_output_src_start(Opts, Indent, ModuleName, ForeignCode,
        InitPreds, FinalPreds, EnvVarNames, !IO),
    io.nl(!IO),
    mlds_output_src_imports(Opts, Indent, Imports, !IO),
    io.nl(!IO),

    mlds_output_c_decls(Opts, Indent, ForeignCode, ForeignDeclErrors, !IO),
    io.nl(!IO),

    list.foldl(mlds_output_env_var_decl, EnvVarNames, !IO),

    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    list.foldl(mlds_output_class_defn(Opts, Indent, MLDS_ModuleName),
        PrivateTypeDefns, !IO),
    io.nl(!IO),
    mlds_output_global_var_decls(Opts, Indent, MLDS_ModuleName,
        PrivateGlobalVarDefns, !IO),
    mlds_output_function_decls(Opts, Indent, MLDS_ModuleName,
        PrivateFuncDefns, !IO),
    io.nl(!IO),

    ModuleSymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
    MangledModuleName = sym_name_mangle(ModuleSymName),

    mlds_output_scalar_cell_group_decls(Opts, Indent, MangledModuleName,
        ScalarCellGroups, !IO),
    io.nl(!IO),
    mlds_output_vector_cell_group_decls(Opts, Indent, MLDS_ModuleName,
        MangledModuleName, VectorCellGroups, !IO),
    io.nl(!IO),
    mlds_output_alloc_site_decls(Indent, AllocSites, !IO),
    io.nl(!IO),

    mlds_output_scalar_cell_group_defns(Opts, Indent, MangledModuleName,
        ScalarCellGroups, !IO),
    io.nl(!IO),
    mlds_output_vector_cell_group_defns(Opts, Indent, MangledModuleName,
        VectorCellGroups, !IO),
    io.nl(!IO),
    mlds_output_alloc_site_defns(Opts, Indent, MLDS_ModuleName, AllocSites,
        !IO),
    io.nl(!IO),

    mlds_output_c_defns(Opts, MLDS_ModuleName, Indent, ForeignCode,
        ForeignCodeErrors, !IO),
    io.nl(!IO),
    mlds_output_global_var_defns(Opts, Indent, yes, MLDS_ModuleName,
        RttiDefns, !IO),
    mlds_output_function_defns(Opts, Indent, MLDS_ModuleName,
        ClosureWrapperFuncDefns, !IO),
    mlds_output_global_var_defns(Opts, Indent, yes, MLDS_ModuleName,
        CellDefns, !IO),
    mlds_output_global_var_defns(Opts, Indent, yes, MLDS_ModuleName,
        TableStructDefns, !IO),
    mlds_output_function_defns(Opts, Indent, MLDS_ModuleName,
        ProcDefns, !IO),
    io.nl(!IO),
    mlds_output_init_fn_defns(Opts, MLDS_ModuleName, FuncDefns,
        TypeCtorInfoDefns, AllocSites, InitPreds, FinalPreds, !IO),
    io.nl(!IO),
    mlds_output_grade_check_fn_defn(MLDS_ModuleName, !IO),
    io.nl(!IO),
    mlds_output_src_end(Indent, ModuleName, !IO),

    Errors = ForeignDeclErrors ++ ForeignCodeErrors.

:- func mlds_get_env_var_names(list(mlds_function_defn)) = set(string).

mlds_get_env_var_names(FuncDefns) = EnvVarNameSet :-
    list.map(mlds_get_env_var_names_from_defn, FuncDefns, EnvVarNameSets),
    EnvVarNameSet = set.union_list(EnvVarNameSets).

:- pred mlds_get_env_var_names_from_defn(mlds_function_defn::in,
    set(string)::out) is det.

mlds_get_env_var_names_from_defn(FuncDefn, EnvVarNameSet) :-
    EnvVarNameSet = FuncDefn ^ mfd_env_vars.

:- pred mlds_output_env_var_decl(string::in, io::di, io::uo) is det.

mlds_output_env_var_decl(EnvVarName, !IO) :-
    io.write_string("extern MR_Word ", !IO),
    io.write_string(global_var_name(env_var_ref(EnvVarName)), !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_hdr_start(mlds_to_c_opts::in, indent::in,
    mercury_module_name::in, io::di, io::uo) is det.

mlds_output_hdr_start(Opts, Indent, ModuleName, !IO) :-
    mlds_output_auto_gen_comment(Opts, ModuleName, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("// :- module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("// :- interface.\n", !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),
    io.write_string("#ifndef MR_HEADER_GUARD_", !IO),
    MangledModuleName = sym_name_mangle(ModuleName),
    io.write_string(MangledModuleName, !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),
    io.write_string("#define MR_HEADER_GUARD_", !IO),
    io.write_string(MangledModuleName, !IO),
    io.nl(!IO),
    io.nl(!IO),

    % If we are outputting C (rather than C++), then add a conditional
    % `extern "C"' wrapper around the header file, so that the header file
    % can be #included by C++ programs.

    Target = Opts ^ m2co_target,
    (
        Target = target_c,
        output_n_indents(Indent, !IO),
        io.write_string("#ifdef __cplusplus\n", !IO),
        output_n_indents(Indent, !IO),
        io.write_string("extern ""C"" {\n", !IO),
        output_n_indents(Indent, !IO),
        io.write_string("#endif\n", !IO),
        io.nl(!IO)
    ;
        ( Target = target_java
        ; Target = target_csharp
        )
    ),
    output_n_indents(Indent, !IO),
    io.write_string("#include ""mercury.h""\n", !IO).

:- pred mlds_output_src_start(mlds_to_c_opts::in, indent::in,
    mercury_module_name::in, mlds_foreign_code::in,
    list(string)::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

mlds_output_src_start(Opts, Indent, ModuleName, ForeignCode,
        InitPreds, FinalPreds, EnvVarNames, !IO) :-
    mlds_output_auto_gen_comment(Opts, ModuleName, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("// :- module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("// :- implementation.\n", !IO),
    mlds_output_src_bootstrap_defines(!IO),
    io.nl(!IO),
    mlds_output_init_and_final_comments(ModuleName, InitPreds, FinalPreds,
        EnvVarNames, !IO),

    CompilerImport = mlds_import(compiler_visible_interface, ModuleName),
    mlds_output_src_import(Opts, Indent, CompilerImport, !IO),

    % If there are `:- pragma foreign_export' declarations,
    % #include the `.mh' file.
    ForeignCode = mlds_foreign_code(_, _, _, Exports),
    (
        Exports = []
    ;
        Exports = [_ | _],
        UserImport = mlds_import(user_visible_interface, ModuleName),
        mlds_output_src_import(Opts, Indent, UserImport, !IO)
    ),
    io.nl(!IO).

    % Output a comment to tell mkinit what module initialisation
    % predicates to call from <module>_init.c.
    %
:- pred mlds_output_init_and_final_comments(mercury_module_name::in,
    list(string)::in, list(string)::in, list(string)::in, io::di, io::uo)
    is det.

mlds_output_init_and_final_comments(ModuleName,
        UserInitPredCNames, UserFinalPredCNames, EnvVarNames, !IO) :-
    io.write_string("/*\n", !IO),
    % In profiling grades the module mercury__<modulename>__init predicate
    % is responsible for calling MR_init_entry, so the INIT comment must be
    % present.
    % XXX we could probably omit it in non-profiling grades.
    io.write_string("INIT ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init\n", !IO),
    (
        UserInitPredCNames = []
    ;
        UserInitPredCNames = [_ | _],
        io.write_string("REQUIRED_INIT ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_init\n", !IO)
    ),
    (
        UserFinalPredCNames = []
    ;
        UserFinalPredCNames = [_ | _],
        io.write_string("REQUIRED_FINAL ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_final\n", !IO)
    ),
    list.foldl(mlds_output_env_var_init, EnvVarNames, !IO),
    % We always write out ENDINIT so that mkinit does not scan the whole file.
    io.write_string("ENDINIT\n", !IO),
    io.write_string("*/\n\n", !IO).

:- pred mlds_output_env_var_init(string::in, io::di, io::uo) is det.

mlds_output_env_var_init(EnvVarName, !IO) :-
    io.write_string("ENVVAR ", !IO),
    io.write_string(EnvVarName, !IO),
    io.nl(!IO).

    % Output any #defines which are required to bootstrap in the hlc grade.
    %
:- pred mlds_output_src_bootstrap_defines(io::di, io::uo) is det.

mlds_output_src_bootstrap_defines(!IO).

:- pred mlds_output_hdr_end(mlds_to_c_opts::in, indent::in,
    mercury_module_name::in, io::di, io::uo) is det.

mlds_output_hdr_end(Opts, Indent, ModuleName, !IO) :-
    Target = Opts ^ m2co_target,
    (
        Target = target_c,
        % Terminate the `extern "C"' wrapper.
        output_n_indents(Indent, !IO),
        io.write_string("#ifdef __cplusplus\n", !IO),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO),
        output_n_indents(Indent, !IO),
        io.write_string("#endif\n", !IO),
        io.nl(!IO)
    ;
        ( Target = target_csharp
        ; Target = target_java
        )
    ),
    output_n_indents(Indent, !IO),
    io.write_string("#endif // MR_HEADER_GUARD_", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.nl(!IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),
    io.write_string("// :- end_interface ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

:- pred mlds_output_src_end(indent::in, mercury_module_name::in,
    io::di, io::uo) is det.

mlds_output_src_end(Indent, ModuleName, !IO) :-
    output_n_indents(Indent, !IO),
    io.write_string("// :- end_module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

    % Output a C comment saying that the file was automatically generated
    % (and giving details such as the compiler version).
    %
:- pred mlds_output_auto_gen_comment(mlds_to_c_opts::in, module_name::in,
    io::di, io::uo) is det.

mlds_output_auto_gen_comment(Opts, ModuleName, !IO) :-
    library.version(Version, Fullarch),
    Globals = Opts ^ m2co_all_globals,
    module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
    output_c_file_intro_and_grade(Globals, SourceFileName, Version,
        Fullarch, !IO),
    io.nl(!IO).

    % Output a reference to the mangled grade name for the grade that the C
    % file gets compiled with. This ensures that we do not try to link objects
    % files compiled in different grades.
    %
:- pred mlds_output_grade_check_fn_defn(mlds_module_name::in,
    io::di, io::uo) is det.

mlds_output_grade_check_fn_defn(ModuleName, !IO) :-
    io.write_string("// Ensure everything is compiled with the same grade.\n",
        !IO),
    output_grade_check_fn_name(ModuleName, !IO),
    io.write_string("\n{\n", !IO),
    io.write_string("    return &MR_GRADE_VAR;\n", !IO),
    io.write_string("}\n", !IO).

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
:- pred mlds_output_init_fn_decls(mlds_module_name::in, list(string)::in,
    list(string)::in, io::di, io::uo) is det.

mlds_output_init_fn_decls(ModuleName, InitPreds, FinalPreds, !IO) :-
    output_init_fn_name(ModuleName, "", !IO),
    io.write_string(";\n", !IO),
    output_init_fn_name(ModuleName, "_type_tables", !IO),
    io.write_string(";\n", !IO),
    output_init_fn_name(ModuleName, "_debugger", !IO),
    io.write_string(";\n", !IO),
    (
        InitPreds = []
    ;
        InitPreds = [_ | _],
        output_required_fn_name(ModuleName, "required_init", !IO),
        io.write_string(";\n", !IO)
    ),
    (
        FinalPreds = []
    ;
        FinalPreds = [_ | _],
        output_required_fn_name(ModuleName, "required_final", !IO),
        io.write_string(";\n", !IO)
    ),
    output_grade_check_fn_name(ModuleName, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_init_fn_defns(mlds_to_c_opts::in, mlds_module_name::in,
    list(mlds_function_defn)::in, list(mlds_global_var_defn)::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

mlds_output_init_fn_defns(Opts, ModuleName, FuncDefns, TypeCtorInfoDefns,
        AllocSites, InitPreds, FinalPreds, !IO) :-
    output_init_fn_name(ModuleName, "", !IO),
    io.write_string("\n{\n", !IO),
    NeedToInit = Opts ^ m2co_need_to_init,
    ( if
        NeedToInit = yes,
        FuncDefns = [_ | _]
    then
        io.write_strings(["\tstatic MR_bool initialised = MR_FALSE;\n",
            "\tif (initialised) return;\n",
            "\tinitialised = MR_TRUE;\n\n"], !IO),
        mlds_output_calls_to_init_entry(ModuleName, FuncDefns, !IO),
        mlds_output_call_to_register_alloc_sites(AllocSites, !IO)
    else
        true
    ),
    io.write_string("}\n\n", !IO),

    output_init_fn_name(ModuleName, "_type_tables", !IO),
    io.write_string("\n{\n", !IO),
    (
        TypeCtorInfoDefns = [_ | _],
        io.write_strings(["\tstatic MR_bool initialised = MR_FALSE;\n",
            "\tif (initialised) return;\n",
            "\tinitialised = MR_TRUE;\n\n"], !IO),
        mlds_output_calls_to_register_tci(ModuleName, TypeCtorInfoDefns, !IO)
    ;
        TypeCtorInfoDefns = []
    ),
    io.write_string("}\n\n", !IO),

    output_init_fn_name(ModuleName, "_debugger", !IO),
    io.write_string("\n{\n", !IO),
    io.write_string("\tMR_fatal_error(""debugger initialization " ++
        "in MLDS grade"");\n", !IO),
    io.write_string("}\n", !IO),

    % Maybe write out wrapper functions that call user-defined intialisation
    % and finalisation predicates.
    (
        InitPreds = []
    ;
        InitPreds = [_ | _],
        io.nl(!IO),
        output_required_fn_name(ModuleName, "required_init", !IO),
        io.write_string("\n{\n", !IO),
        output_required_calls(InitPreds, !IO),
        io.write_string("}\n", !IO)
    ),
    (
        FinalPreds = []
    ;
        FinalPreds = [_ | _],
        io.nl(!IO),
        output_required_fn_name(ModuleName, "required_final", !IO),
        io.write_string("\n{\n", !IO),
        output_required_calls(FinalPreds, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_required_calls(list(string)::in, io::di, io::uo) is det.

output_required_calls([], !IO).
output_required_calls([Call | Calls], !IO) :-
    io.write_string("\t" ++ Call ++ "();\n", !IO),
    output_required_calls(Calls, !IO).

:- pred output_init_fn_name(mlds_module_name::in, string::in,
    io::di, io::uo) is det.

output_init_fn_name(ModuleName, Suffix, !IO) :-
    % Here we ensure that we only get one "mercury__" at the start
    % of the function name.
    ModuleNameString0 = sym_name_mangle(
        mlds_module_name_to_sym_name(ModuleName)),
    ( if string.prefix(ModuleNameString0, "mercury__") then
        ModuleNameString = ModuleNameString0
    else
        ModuleNameString = "mercury__" ++ ModuleNameString0
    ),
    io.write_string("void ", !IO),
    io.write_string(ModuleNameString, !IO),
    io.write_string("__init", !IO),
    io.write_string(Suffix, !IO),
    io.write_string("(void)", !IO).

    % Output a function name of the form:
    %
    %   mercury__<modulename>__<suffix>
    %
:- pred output_required_fn_name(mlds_module_name::in, string::in,
    io::di, io::uo) is det.

output_required_fn_name(ModuleName, Suffix, !IO) :-
    % Here we ensure that we only get one "mercury__" at the start
    % of the function name.
    ModuleNameString0 = sym_name_mangle(
        mlds_module_name_to_sym_name(ModuleName)),
    ( if string.prefix(ModuleNameString0, "mercury__") then
        ModuleNameString = ModuleNameString0
    else
        ModuleNameString = "mercury__" ++ ModuleNameString0
    ),
    io.write_string("void ", !IO),
    io.write_string(ModuleNameString, !IO),
    io.write_string("__", !IO),
    io.write_string(Suffix, !IO),
    io.write_string("(void)", !IO).

:- pred output_grade_check_fn_name(mlds_module_name::in,
    io::di, io::uo) is det.

output_grade_check_fn_name(ModuleName, !IO) :-
    ModuleNameString0 = sym_name_mangle(
        mlds_module_name_to_sym_name(ModuleName)),
    ( if string.prefix(ModuleNameString0, "mercury__") then
        ModuleNameString = ModuleNameString0
    else
        ModuleNameString = "mercury__" ++ ModuleNameString0
    ),
    io.format("const char *%s__grade_check(void)",
        [s(ModuleNameString)], !IO).

    % Generate calls to MR_init_entry() for the specified functions.
    %
:- pred mlds_output_calls_to_init_entry(mlds_module_name::in,
    list(mlds_function_defn)::in, io::di, io::uo) is det.

mlds_output_calls_to_init_entry(_ModuleName, [], !IO).
mlds_output_calls_to_init_entry(ModuleName, [FuncDefn | FuncDefns], !IO) :-
    FuncName = FuncDefn ^ mfd_function_name,
    io.write_string("\tMR_init_entry(", !IO),
    mlds_output_fully_qualified_function_name(
        qual_function_name(ModuleName, FuncName), !IO),
    io.write_string(");\n", !IO),
    mlds_output_calls_to_init_entry(ModuleName, FuncDefns, !IO).

    % Generate calls to MR_register_type_ctor_info() for the specified
    % type_ctor_infos.
    %
:- pred mlds_output_calls_to_register_tci(mlds_module_name::in,
    list(mlds_global_var_defn)::in, io::di, io::uo) is det.

mlds_output_calls_to_register_tci(_MLDS_ModuleName, [], !IO).
mlds_output_calls_to_register_tci(MLDS_ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    GlobalVarName = GlobalVarDefn ^ mgvd_name,
    io.write_string("\tMR_register_type_ctor_info(&", !IO),
    mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName,
        GlobalVarName, !IO),
    io.write_string(");\n", !IO),
    mlds_output_calls_to_register_tci(MLDS_ModuleName, GlobalVarDefns, !IO).

    % Generate call to MR_register_alloc_sites.
    %
:- pred mlds_output_call_to_register_alloc_sites(
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

mlds_output_call_to_register_alloc_sites(AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        list.length(AllocSites, Length),
        io.write_string("\tMR_register_alloc_sites(MR_alloc_sites, ", !IO),
        io.write_int(Length, !IO),
        io.write_string(");\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Foreign language interface stuff.
%

:- pred mlds_output_c_hdr_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_foreign_code::in, list(string)::out,
    io::di, io::uo) is det.

mlds_output_c_hdr_decls(Opts, Indent, ModuleName, ForeignCode, Errors, !IO) :-
    ForeignCode = mlds_foreign_code(DeclCodes, _BodyCodes, _Imports,
        _ExportDefns),
    ( if is_std_lib_module(ModuleName, StdlibModuleName) then
        SymName = StdlibModuleName
    else
        SymName = mlds_module_name_to_sym_name(ModuleName)
    ),

    DeclGuard = decl_guard(SymName),
    io.write_strings(["#ifndef ", DeclGuard, "\n#define ", DeclGuard, "\n"],
        !IO),

    % We need to make sure we #include the .mih files for any ancestor modules
    % in cases any foreign_types defined in them are referenced by the extern
    % declarations required by mutables.

    AncestorModuleNames = get_ancestors(SymName),
    list.map(module_name_to_file_name_stem,
        AncestorModuleNames, AncestorFileNames),
    WriteAncestorInclude =
        ( pred(Ancestor::in, !.IO::di, !:IO::uo) is det :-
            io.write_strings(["#include \"", Ancestor, ".mih", "\"\n"], !IO)
        ),
    list.foldl(WriteAncestorInclude, AncestorFileNames, !IO),
    list.map_foldl(
        mlds_output_c_hdr_decl(Opts, Indent, yes(foreign_decl_is_exported)),
        DeclCodes, DeclResults, !IO),
    list.filter_map(maybe_is_error, DeclResults, Errors),
    io.write_string("\n#endif\n", !IO).

:- pred mlds_output_c_hdr_decl(mlds_to_c_opts::in, indent::in,
    maybe(foreign_decl_is_local)::in, foreign_decl_code::in, maybe_error::out,
    io::di, io::uo) is det.

mlds_output_c_hdr_decl(Opts, _Indent, MaybeDesiredIsLocal, DeclCode, Res,
        !IO) :-
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
            mlds_output_foreign_literal_or_include(Opts, LiteralOrInclude,
                Context, Res, !IO)
        else
            Res = ok
        )
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_c_decls(mlds_to_c_opts::in, indent::in,
    mlds_foreign_code::in, list(string)::out, io::di, io::uo) is det.

mlds_output_c_decls(Opts, Indent, ForeignCode, Errors, !IO) :-
    ForeignCode = mlds_foreign_code(HeaderCodes, _BodyCodes, _Imports,
        _ExportDefns),
    list.map_foldl(
        mlds_output_c_hdr_decl(Opts, Indent, yes(foreign_decl_is_local)),
        HeaderCodes, Results, !IO),
    list.filter_map(maybe_is_error, Results, Errors).

:- pred mlds_output_c_defns(mlds_to_c_opts::in, mlds_module_name::in,
    indent::in, mlds_foreign_code::in, list(string)::out, io::di, io::uo)
    is det.

mlds_output_c_defns(Opts, ModuleName, Indent, ForeignCode, Errors, !IO) :-
    ForeignCode = mlds_foreign_code(_HeaderCodes, BodyCodes,
        Imports, ExportDefns),
    list.foldl(mlds_output_c_foreign_import_module(Opts, Indent),
        Imports, !IO),
    list.map_foldl(mlds_output_c_defn(Opts, Indent), BodyCodes, Results, !IO),
    io.write_string("\n", !IO),
    io.write_list(ExportDefns, "\n",
        mlds_output_pragma_export_defn(Opts, ModuleName, Indent), !IO),
    list.filter_map(maybe_is_error, Results, Errors).

:- pred mlds_output_c_foreign_import_module(mlds_to_c_opts::in, int::in,
    fim_spec::in, io::di, io::uo) is det.

mlds_output_c_foreign_import_module(Opts, Indent, FIMSpec, !IO) :-
    FIMSpec = fim_spec(Lang, Import),
    (
        Lang = lang_c,
        UserImport = mlds_import(user_visible_interface, Import),
        mlds_output_src_import(Opts, Indent, UserImport, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_c_defn(mlds_to_c_opts::in, indent::in,
    foreign_body_code::in, maybe_error::out, io::di, io::uo) is det.

mlds_output_c_defn(Opts, _Indent, ForeignBodyCode, Res, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    (
        Lang = lang_c,
        mlds_output_foreign_literal_or_include(Opts, LiteralOrInclude, Context,
            Res, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_foreign_literal_or_include(mlds_to_c_opts::in,
    foreign_literal_or_include::in, prog_context::in, maybe_error::out,
    io::di, io::uo) is det.

mlds_output_foreign_literal_or_include(Opts, LiteralOrInclude, Context, Res,
        !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
        io.write_string(Code, !IO),
        Res = ok
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        SourceFileName = Opts ^ m2co_source_filename,
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        c_output_file_line(Opts ^ m2co_foreign_line_numbers,
            IncludePath, 1, !IO),
        write_include_file_contents_cur_stream(IncludePath, Res, !IO)
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_file.
%---------------------------------------------------------------------------%
