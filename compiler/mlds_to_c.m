%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012 The University of Melbourne.
% Copyright (C) 2013-2017 The Mercury team.
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

:- module ml_backend.mlds_to_c.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % output_c_mlds(MLDS, Globals, Suffix, Succeeded, !IO):
    %
    % Output C code to the appropriate C file and C declarations to the
    % appropriate header file. The file names are determined by the module
    % name, with the specified Suffix appended at the end. (The suffix is used
    % for debugging dumps. For normal output, the suffix should be the empty
    % string.)
    %
:- pred output_c_mlds(mlds::in, globals::in, string::in, bool::out,
    io::di, io::uo) is det.

    % output_c_dump_preds(MLDS, Globals, Suffix, DumpPreds, !IO):
    %
    % Output C code for the predicates and/or functions whose names
    % occurs in DumpPreds. The file name to write to is determined similarly
    % to how output_c_mlds does it, but with ".mlds_dump" replacing ".c".
    %
:- pred output_c_dump_preds(mlds::in, globals::in, string::in,
    list(string)::in, io::di, io::uo) is det.

:- pred func_defn_has_name_in_list(list(string)::in, mlds_function_defn::in)
    is semidet.

:- func mlds_tabling_data_name(mlds_proc_label, proc_tabling_struct_id)
    = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.name_mangle.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.         % for pred_proc_id.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
                                         % for ml_gen_public_field_decl_flags,
                                         % which is used by the code that
                                         % handles derived classes
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_type_gen. % for ml_gen_type_name
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- type output_type == pred(mlds_to_c_opts, mlds_type, io, io).
:- inst output_type == (pred(in, in, di, uo) is det).

%---------------------------------------------------------------------------%

    % This type concentrates the values of all the options that this module
    % needs, in a form that can be looked up much more quickly than by calling
    % lookup_bool_option.
    %
    % The first batch of fields contains all the non-enum fields, to allow
    % all the enum fields to be stored in a single word. (We need the whole
    % globals as a field, despite storing the option values we want separately,
    % because module_name_to_file_name takes Globals as an argument.)
    %
    % Each field in the second batch of fields is named after the option
    % whose value it holds, though the value of the m2co_line_numbers field
    % is overridden by the value of the line_numbers_for_c_headers when
    % generating C header files.
    %
    % m2co_need_to_init is set to `yes' if any of the profile_calls,
    % profile_memory and profile_time fields is `yes'. This is because
    % we need to output calls to MR_init_entry if any form of profiling
    % is enabled. (It would be OK to output the calls regardless,
    % since they will macro-expand to nothing if profiling is not enabled,
    % but for readability of the generated code we prefer not to.)
    %
    % m2co_std_func_decl is `yes' if want to use standard argument names
    % in function declarations.
    %
    % We use m2co_break_context to check whether goto_break_{switch,loop}s
    % are used in the intended contexts.
    %
:- type mlds_to_c_opts
    --->    mlds_to_c_opts(
                m2co_all_globals            :: globals,
                m2co_source_filename        :: string,

                m2co_line_numbers           :: bool,
                m2co_foreign_line_numbers   :: bool,
                m2co_auto_comments          :: bool,
                m2co_highlevel_data         :: bool,
                m2co_single_prec_float      :: bool,
                m2co_profile_calls          :: bool,
                m2co_profile_memory         :: bool,
                m2co_profile_time           :: bool,
                m2co_need_to_init           :: bool,
                m2co_target                 :: compilation_target,
                m2co_gc_method              :: gc_method,

                m2co_std_func_decl          :: bool,

                m2co_break_context          :: break_context
            ).

:- func init_mlds_to_c_opts(globals, string) = mlds_to_c_opts.

init_mlds_to_c_opts(Globals, SourceFileName) = Opts :-
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    globals.lookup_bool_option(Globals, auto_comments, Comments),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    globals.lookup_bool_option(Globals, single_prec_float, SinglePrecFloat),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    ( if
        ( ProfileCalls = yes
        ; ProfileMemory = yes
        ; ProfileTime = yes
        )
    then
        NeedToInit = yes
    else
        NeedToInit = no
    ),
    globals.get_target(Globals, Target),
    globals.get_gc_method(Globals, GCMethod),
    StdFuncDecls = no,
    BreakContext = bc_none,
    Opts = mlds_to_c_opts(Globals, SourceFileName,
        LineNumbers, ForeignLineNumbers, Comments, HighLevelData,
        SinglePrecFloat, ProfileCalls, ProfileMemory, ProfileTime, NeedToInit,
        Target, GCMethod, StdFuncDecls, BreakContext).

%---------------------------------------------------------------------------%

output_c_mlds(MLDS, Globals, Suffix, Succeeded, !IO) :-
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
    module_source_filename(Globals, ModuleName, SourceFileName, !IO),
    Opts = init_mlds_to_c_opts(Globals, SourceFileName),
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
    module_name_to_file_name(Globals, do_create_dirs, ".c" ++ Suffix,
        ModuleName, SourceFile, !IO),
    Indent = 0,
    output_to_file(Globals, SourceFile,
        mlds_output_src_file(Opts, Indent, MLDS), Succeeded, !IO).

:- pred output_c_header_file_opts(mlds::in, mlds_to_c_opts::in, string::in,
    bool::out, io::di, io::uo) is det.

output_c_header_file_opts(MLDS, Opts, Suffix, Succeeded, !IO) :-
    % We write the header file out to <module>.mih.tmp and then call
    % `update_interface' to move the <module>.mih.tmp file to <module>.mih.
    % This avoids updating the timestamp on the `.mih' file if it hasn't
    % changed.

    ModuleName = mlds_get_module_name(MLDS),
    Globals = Opts ^ m2co_all_globals,
    module_name_to_file_name(Globals, do_create_dirs,
        ".mih" ++ Suffix ++ ".tmp", ModuleName, TmpHeaderFile, !IO),
    module_name_to_file_name(Globals, do_create_dirs,
        ".mih" ++ Suffix, ModuleName, HeaderFile, !IO),
    globals.lookup_bool_option(Globals, line_numbers_for_c_headers,
        LineNumbersForCHdrs),
    HdrOpts = ((Opts
        ^ m2co_line_numbers := LineNumbersForCHdrs)
        ^ m2co_foreign_line_numbers := LineNumbersForCHdrs),
    Indent = 0,
    output_to_file(Globals, TmpHeaderFile,
        mlds_output_hdr_file(HdrOpts, Indent, MLDS), Succeeded, !IO),
    (
        Succeeded = yes,
        update_interface(Globals, HeaderFile, !IO)
    ;
        Succeeded = no
    ).

%---------------------------------------------------------------------------%

output_c_dump_preds(MLDS, Globals, Suffix, DumpPredNames, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    module_source_filename(Globals, ModuleName, SourceFileName, !IO),
    Opts = init_mlds_to_c_opts(Globals, SourceFileName),
    module_name_to_file_name(Globals, do_create_dirs, ".mlds_dump" ++ Suffix,
        ModuleName, DumpFileName, !IO),
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
    io::di, io::uo) is det.

mlds_output_named_function_defns(_Opts, _DumpPredNames, _ModuleName, [], !IO).
mlds_output_named_function_defns(Opts, DumpPredNames, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    ( if func_defn_has_name_in_list(DumpPredNames, FuncDefn) then
        Indent = 0,
        mlds_output_function_defn(Opts, Indent, ModuleName, FuncDefn, !IO)
    else
        true
    ),
    mlds_output_named_function_defns(Opts, DumpPredNames, ModuleName,
        FuncDefns, !IO).

%---------------------------------------------------------------------------%

:- pred mlds_output_hdr_file(mlds_to_c_opts::in, indent::in, mlds::in,
    io::di, io::uo) is det.

mlds_output_hdr_file(Opts, Indent, MLDS, !IO) :-
    % The header file must contain _definitions_ of all public types, but only
    % _declarations_ of all public variables, constants, and functions.
    %
    % Note that we don't forward-declare the types here; the forward
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
    % We don't sort the type (i.e. class) definitions, because
    % (a) we don't generate any in MLDS grades (such as hlc.gc)
    %     that use low-level data, so for them it doesn't matter
    %     whether we sort them or not, and
    % (b) I (zs) don't know whether the order is important in MLDS grades
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
    mlds_output_c_hdr_decls(Opts, Indent, MLDS_ModuleName, ForeignCode, !IO),
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

:- pred mlds_output_hdr_imports(indent::in, mlds_imports::in,
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
        ; Target = target_erlang
        ),
        unexpected($pred, "expected target c")
    ).

:- pred mlds_output_src_import(mlds_to_c_opts::in, indent::in, mlds_import::in,
    io::di, io::uo) is det.

mlds_output_src_import(Opts, _Indent, Import, !IO) :-
    Import = mercury_import(ImportType, ImportName),
    ModuleName0 = mlds_module_name_to_sym_name(ImportName),
    ( ImportType = user_visible_interface, HeaderExt = ".mh"
    ; ImportType = compiler_visible_interface, HeaderExt = ".mih"
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
    module_name_to_search_file_name(Globals, HeaderExt, ModuleName, HeaderFile,
        !IO),
    io.write_strings(["#include """, HeaderFile, """\n"], !IO).

    % Generate the `.c' file.
    %
    % (Calling it the "source" file is a bit of a misnomer, since in our case
    % it is actually the target file, but there is no obvious alternative term
    % to use which also has a clear and concise abbreviation, so never mind...)
    %
:- pred mlds_output_src_file(mlds_to_c_opts::in, indent::in, mlds::in,
    io::di, io::uo) is det.

mlds_output_src_file(Opts, Indent, MLDS, !IO) :-
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
    % Note that we don't forward-declare the types here; the forward
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

    mlds_output_c_decls(Opts, Indent, ForeignCode, !IO),
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

    mlds_output_c_defns(Opts, MLDS_ModuleName, Indent, ForeignCode, !IO),
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
    mlds_output_src_end(Indent, ModuleName, !IO).

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
        ; Target = target_erlang
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

    mlds_output_src_import(Opts, Indent,
        mercury_import(compiler_visible_interface,
            mercury_module_name_to_mlds(ModuleName)), !IO),

    % If there are `:- pragma foreign_export' declarations,
    % #include the `.mh' file.
    ForeignCode = mlds_foreign_code(_, _, _, Exports),
    (
        Exports = []
    ;
        Exports = [_ | _],
        mlds_output_src_import(Opts, Indent,
            mercury_import(user_visible_interface,
                mercury_module_name_to_mlds(ModuleName)),
            !IO)
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
    % We always write out ENDINIT so that mkinit doesn't scan the whole file.
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
        ; Target = target_erlang
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
    module_name_to_file_name(Globals, do_not_create_dirs, ".m",
        ModuleName, SourceFileName, !IO),
    output_c_file_intro_and_grade(Globals, SourceFileName, Version,
        Fullarch, !IO),
    io.nl(!IO).

    % Output a reference to the mangled grade name for the grade that the C
    % file gets compiled with. This ensures that we don't try to link objects
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
    mlds_module_name::in, mlds_foreign_code::in, io::di, io::uo) is det.

mlds_output_c_hdr_decls(Opts, Indent, ModuleName, ForeignCode, !IO) :-
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
    WriteAncestorInclude = (pred(Ancestor::in, !.IO::di, !:IO::uo) is det :-
        io.write_strings(["#include \"", Ancestor, ".mih", "\"\n"], !IO)
    ),
    list.foldl(WriteAncestorInclude, AncestorFileNames, !IO),
    io.write_list(DeclCodes, "\n",
        mlds_output_c_hdr_decl(Opts, Indent, yes(foreign_decl_is_exported)),
        !IO),
    io.write_string("\n#endif\n", !IO).

:- pred mlds_output_c_hdr_decl(mlds_to_c_opts::in, indent::in,
    maybe(foreign_decl_is_local)::in, foreign_decl_code::in,
    io::di, io::uo) is det.

mlds_output_c_hdr_decl(Opts, _Indent, MaybeDesiredIsLocal, DeclCode, !IO) :-
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
                Context, !IO)
        else
            true
        )
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_c_decls(mlds_to_c_opts::in, indent::in,
    mlds_foreign_code::in, io::di, io::uo) is det.

mlds_output_c_decls(Opts, Indent, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(HeaderCodes, _BodyCodes, _Imports,
        _ExportDefns),
    io.write_list(HeaderCodes, "\n",
        mlds_output_c_hdr_decl(Opts, Indent, yes(foreign_decl_is_local)), !IO).

:- pred mlds_output_c_defns(mlds_to_c_opts::in, mlds_module_name::in,
    indent::in, mlds_foreign_code::in, io::di, io::uo) is det.

mlds_output_c_defns(Opts, ModuleName, Indent, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(_HeaderCodes, BodyCodes,
        Imports, ExportDefns),
    list.foldl(mlds_output_c_foreign_import_module(Opts, Indent),
        Imports, !IO),
    io.write_list(BodyCodes, "\n", mlds_output_c_defn(Opts, Indent), !IO),
    io.write_string("\n", !IO),
    io.write_list(ExportDefns, "\n",
        mlds_output_pragma_export_defn(Opts, ModuleName, Indent), !IO).

:- pred mlds_output_c_foreign_import_module(mlds_to_c_opts::in, int::in,
    foreign_import_module_info::in, io::di, io::uo) is det.

mlds_output_c_foreign_import_module(Opts, Indent, ForeignImport, !IO) :-
    ForeignImport = foreign_import_module_info(Lang, Import),
    (
        Lang = lang_c,
        mlds_output_src_import(Opts, Indent,
            mercury_import(user_visible_interface,
                mercury_module_name_to_mlds(Import)), !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_c_defn(mlds_to_c_opts::in, indent::in,
    foreign_body_code::in, io::di, io::uo) is det.

mlds_output_c_defn(Opts, _Indent, ForeignBodyCode, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    (
        Lang = lang_c,
        mlds_output_foreign_literal_or_include(Opts, LiteralOrInclude, Context,
            !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_erlang
        ),
        sorry($pred, "foreign code other than C")
    ).

:- pred mlds_output_foreign_literal_or_include(mlds_to_c_opts::in,
    foreign_literal_or_include::in, prog_context::in, io::di, io::uo) is det.

mlds_output_foreign_literal_or_include(Opts, LiteralOrInclude, Context, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
        io.write_string(Code, !IO)
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        SourceFileName = Opts ^ m2co_source_filename,
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        c_output_file_line(Opts ^ m2co_foreign_line_numbers,
            IncludePath, 1, !IO),
        write_include_file_contents_cur_stream(IncludePath, !IO)
    ).

:- pred mlds_output_pragma_export_defn(mlds_to_c_opts::in,
    mlds_module_name::in, indent::in, mlds_pragma_export::in, io::di, io::uo)
    is det.

mlds_output_pragma_export_defn(Opts, ModuleName, Indent, PragmaExport, !IO) :-
    PragmaExport = ml_pragma_export(Lang, _ExportName, MLDS_Name,
        MLDS_Signature, _UnivQTVars, Context),
    expect(unify(Lang, lang_c), $pred,
        "foreign_export to language other than C."),
    mlds_output_pragma_export_func_name(Opts, ModuleName, Indent,
        PragmaExport, !IO),
    io.write_string("\n", !IO),
    c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_pragma_export_defn_body(Opts, MLDS_Name, MLDS_Signature, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_pragma_export_func_name(mlds_to_c_opts::in,
    mlds_module_name::in, indent::in, mlds_pragma_export::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_func_name(Opts, ModuleName, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _MLDSName, Signature,
        _UnivQTVars, Context),
    expect(unify(Lang, lang_c), $pred, "export to language other than C."),
    FuncName = mlds_function_export(ExportName),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    % For functions exported using `pragma foreign_export',
    % we use the default C calling convention.
    CallingConvention = "",
    mlds_output_func_decl_ho(Opts, Indent, QualFuncName, Context,
        CallingConvention, Signature,
        mlds_output_pragma_export_type_ignore_opts(prefix),
        mlds_output_pragma_export_type_ignore_opts(suffix), !IO).

:- pred mlds_output_pragma_export_type_prefix_suffix(mlds_type::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_type_prefix_suffix(Type, !IO) :-
    mlds_output_pragma_export_type(prefix, Type, !IO),
    mlds_output_pragma_export_type(suffix, Type, !IO).

:- type locn
    --->    prefix
    ;       suffix.

:- pred mlds_output_pragma_export_type_ignore_opts(locn::in,
    mlds_to_c_opts::in, mlds_type::in, io::di, io::uo) is det.

mlds_output_pragma_export_type_ignore_opts(PrefixSuffix, _Opts, MLDS_Type,
        !IO) :-
    mlds_output_pragma_export_type(PrefixSuffix, MLDS_Type, !IO).

:- pred mlds_output_pragma_export_type(locn::in,
    mlds_type::in, io::di, io::uo) is det.

mlds_output_pragma_export_type(PrefixSuffix, MLDS_Type, !IO) :-
    (
        PrefixSuffix = suffix
    ;
        PrefixSuffix = prefix,
        (
            MLDS_Type = mlds_mercury_array_type(_ElemType),
            io.write_string("MR_ArrayPtr", !IO)
        ;
            MLDS_Type = mercury_type(MerType, MaybeForeignType, _),
            TypeStr =
                maybe_foreign_type_to_c_string(MerType, MaybeForeignType),
            io.write_string(TypeStr, !IO)
        ;
            ( MLDS_Type = mlds_cont_type(_)
            ; MLDS_Type = mlds_commit_type
            ; MLDS_Type = mlds_class_type(_)
            ; MLDS_Type = mlds_array_type(_)
            ; MLDS_Type = mlds_mostly_generic_array_type(_)
            ; MLDS_Type = mlds_func_type(_)
            ; MLDS_Type = mlds_generic_type
            ; MLDS_Type = mlds_generic_env_ptr_type
            ; MLDS_Type = mlds_type_info_type
            ; MLDS_Type = mlds_pseudo_type_info_type
            ; MLDS_Type = mlds_rtti_type(_)
            ),
            io.write_string("MR_Word", !IO)
        ;
            MLDS_Type = mlds_native_bool_type,
            io.write_string("MR_bool", !IO)
        ;
            MLDS_Type = mlds_native_int_type,
            io.write_string("MR_Integer", !IO)
        ;
            MLDS_Type = mlds_native_uint_type,
            io.write_string("MR_Unsigned", !IO)
        ;
            MLDS_Type = mlds_native_float_type,
            io.write_string("MR_Float", !IO)
        ;
            MLDS_Type = mlds_native_char_type,
            io.write_string("MR_Char", !IO)
        ;
            MLDS_Type = mlds_foreign_type(ForeignType),
            (
                ForeignType = c(c_type(Name)),
                io.write_string(Name, !IO)
            ;
                ForeignType = java(_),
                unexpected($pred, "java foreign_type")
            ;
                ForeignType = csharp(_),
                unexpected($pred, "csharp foreign_type")
            ;
                ForeignType = erlang(_),
                unexpected($pred, "erlang foreign_type")
            )
        ;
            MLDS_Type = mlds_ptr_type(Type),
            mlds_output_pragma_export_type(prefix, Type, !IO),
            io.write_string(" *", !IO)
        ;
            MLDS_Type = mlds_tabling_type(_),
            % These types should never occur in procedures exported to C,
            % so the fact that we could generate a more accurate type
            % shouldn't matter.
            io.write_string("MR_Word", !IO)
        ;
            MLDS_Type = mlds_unknown_type,
            unexpected($pred, "unknown_type")
        )
    ).

    % Output the definition body for a pragma foreign_export.
    %
:- pred mlds_output_pragma_export_defn_body(mlds_to_c_opts::in,
    qual_function_name::in, mlds_func_params::in, io::di, io::uo) is det.

mlds_output_pragma_export_defn_body(Opts, FuncName, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),

    % Declare local variables corresponding to any foreign_type parameters.
    IsCForeignType =
        ( pred(Arg::in) is semidet :-
            Arg = mlds_argument(_Name, Type, _GCStmt),
            Type = mlds_foreign_type(c(_))
        ),
    IsCForeignTypePtr =
        ( pred(Arg::in) is semidet :-
            Arg = mlds_argument(_Name, Type, _GCStmt),
            Type = mlds_ptr_type(mlds_foreign_type(c(_)))
        ),
    CForeignTypeInputs = list.filter(IsCForeignType, Parameters),
    CForeignTypeOutputs = list.filter(IsCForeignTypePtr, Parameters),
    io.write_list(CForeignTypeInputs, "",
        mlds_output_pragma_export_input_defns(Opts), !IO),
    io.write_list(CForeignTypeOutputs, "",
        mlds_output_pragma_export_output_defns(Opts), !IO),

    % Declare a local variable or two for the return value, if needed.
    (
        RetTypes = [RetType1],
        ( if RetType1 = mlds_foreign_type(c(_)) then
            io.write_string("\t", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType1, !IO),
            io.write_string(" ret_value;\n", !IO),
            io.write_string("\t", !IO),
            mlds_output_type(Opts, RetType1, !IO),
            io.write_string(" boxed_ret_value;\n", !IO)
        else
            io.write_string("\t", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType1, !IO),
            io.write_string(" ret_value;\n", !IO)
        )
    ;
        RetTypes = []
    ;
        RetTypes = [_, _ | _]
    ),

    % Generate code to box any non-word-sized foreign_type input parameters;
    % these need to be converted to a uniform size before passing them
    % to Mercury code.
    io.write_list(CForeignTypeInputs, "", mlds_output_pragma_input_arg, !IO),

    % Generate code to actually call the Mercury procedure which
    % is being exported
    (
        RetTypes = [],
        io.write_string("\t", !IO),
        mlds_output_pragma_export_call(Opts, FuncName, Parameters, !IO)
    ;
        RetTypes = [RetType2],
        ( if RetType2 = mlds_foreign_type(c(_)) then
            io.write_string("\tboxed_ret_value = ", !IO)
        else
            io.write_string("\tret_value = (", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType2, !IO),
            io.write_string(")", !IO)
        ),
        mlds_output_pragma_export_call(Opts, FuncName, Parameters, !IO)
    ;
        RetTypes = [_, _ | _],
        % This is just for MLDS dumps when compiling to non-C targets.
        % So we don't need to worry about boxing/unboxing foreign types here.
        io.write_string("\treturn (", !IO),
        mlds_output_return_list(RetTypes,
            mlds_output_pragma_export_type_prefix_suffix, !IO),
        io.write_string(") ", !IO)
    ),

    % Generate code to unbox any foreign_type output parameters,
    % since we are returning those parameters to C code.
    io.write_list(CForeignTypeOutputs, "", mlds_output_pragma_output_arg, !IO),

    % Generate the final statement to unbox and return the return value,
    % if needed.
    (
        RetTypes = []
    ;
        RetTypes = [RetType3],
        ( if RetType3 = mlds_foreign_type(c(_)) then
            io.write_string("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType3, !IO),
            io.write_string(", boxed_ret_value, ret_value);\n", !IO)
        else
            true
        ),
        io.write_string("\treturn ret_value;\n", !IO)
    ;
        RetTypes = [_, _ | _]
    ).

:- pred mlds_output_pragma_input_arg(mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_input_arg(Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    io.write_string("\tMR_MAYBE_BOX_FOREIGN_TYPE(", !IO),
    mlds_output_pragma_export_type_prefix_suffix(Type, !IO),
    io.write_string(", ", !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    io.write_string(", ", !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    io.write_string(");\n", !IO).

:- pred mlds_output_pragma_output_arg(mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_output_arg(Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    io.write_string("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
    mlds_output_pragma_export_type_prefix_suffix(pointed_to_type(Type), !IO),
    io.write_string(", ", !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    io.write_string(", *", !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    io.write_string(");\n", !IO).

:- pred mlds_output_pragma_export_input_defns(mlds_to_c_opts::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_input_defns(Opts, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    io.write_string("\t", !IO),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    mlds_output_type_suffix_no_size(Opts, Type, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_pragma_export_output_defns(mlds_to_c_opts::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_output_defns(Opts, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    io.write_string("\t", !IO),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    PointedToType = pointed_to_type(Type),
    mlds_output_type_prefix(Opts, PointedToType, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    mlds_output_type_suffix_no_size(Opts, PointedToType, !IO),
    io.write_string(";\n", !IO).

:- func pointed_to_type(mlds_type) = mlds_type.

pointed_to_type(PtrType) =
    ( if PtrType = mlds_ptr_type(Type) then
        Type
    else
        unexpected($pred, "not pointer")
    ).

:- pred get_boxed_local_var_name(mlds_local_var_name::in,
    mlds_local_var_name::out) is det.

get_boxed_local_var_name(VarName, BoxedVarName) :-
    ( if VarName = lvn_prog_var(Name, Seq) then
        BoxedVarName = lvn_prog_var_boxed(Name, Seq)
    else
        NameStr = ml_local_var_name_to_string(VarName),
        BoxedVarName = lvn_comp_var(lvnc_non_prog_var_boxed(NameStr))
    ).

:- pred mlds_output_pragma_export_call(mlds_to_c_opts::in,
    qual_function_name::in, list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_pragma_export_call(Opts, FuncName, Parameters, !IO) :-
    mlds_output_fully_qualified_function_name(FuncName, !IO),
    io.write_string("(", !IO),
    io.write_list(Parameters, ", ", mlds_output_pragma_export_arg(Opts), !IO),
    io.write_string(");\n", !IO).

    % Output a fully qualified name preceded by a cast.
    %
:- pred mlds_output_pragma_export_arg(mlds_to_c_opts::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_arg(Opts, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    ( if Type = mlds_foreign_type(c(_)) then
        % This is a foreign_type input. Pass in the already-boxed value.
        mlds_output_local_var_name(BoxedLocalVarName, !IO)
    else if Type = mlds_ptr_type(mlds_foreign_type(c(_))) then
        % This is a foreign_type output. Pass in the address of the
        % local variable which will hold the boxed value.
        io.write_string("&", !IO),
        mlds_output_local_var_name(BoxedLocalVarName, !IO)
    else
        % Otherwise, no boxing or unboxing is needed.
        % Just cast the argument to the right type.
        mlds_output_cast(Opts, Type, !IO),
        mlds_output_local_var_name(LocalVarName, !IO)
    ).

:- pred mlds_output_export_enums(mlds_to_c_opts::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

mlds_output_export_enums(Opts, Indent, ExportedEnums, !IO) :-
    list.foldl(mlds_output_export_enum(Opts, Indent), ExportedEnums, !IO).

:- pred mlds_output_export_enum(mlds_to_c_opts::in, indent::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

mlds_output_export_enum(Opts, _Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, Context, _TypeCtor,
        ExportConstants),
    (
        Lang = lang_c,
        c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
        list.foldl(mlds_output_exported_enum_constant, ExportConstants, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ; Lang = lang_erlang
        )
    ).

:- pred mlds_output_exported_enum_constant(mlds_exported_enum_constant::in,
    io::di, io::uo) is det.

mlds_output_exported_enum_constant(ExportedConstant, !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    io.write_string("#define ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" ", !IO),
    ( if
        Initializer = init_obj(ml_const(mlconst_enum(Value, _)))
    then
        io.write_int(Value, !IO)
    else if
        Initializer = init_obj(ml_const(mlconst_foreign(Lang, Value, _)))
    then
        expect(unify(Lang, lang_c), $pred,
            "mlconst_foreign for language other than C."),
        io.write_string(Value, !IO)
    else
        unexpected($pred, "tag for export enumeration is not enum or foreign")
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%
%
% Code to output declarations and definitions.
%

:- pred mlds_output_local_var_defns(mlds_to_c_opts::in, indent::in, bool::in,
    list(mlds_local_var_defn)::in, io::di, io::uo) is det.

mlds_output_local_var_defns(_Opts, _Indent, _Separate, [], !IO).
mlds_output_local_var_defns(Opts, Indent, Separate,
        [LocalVarDefn | LocalVarDefns], !IO) :-
    mlds_output_local_var_defn(Opts, Indent, Separate, LocalVarDefn, !IO),
    mlds_output_local_var_defns(Opts, Indent, Separate, LocalVarDefns, !IO).

:- pred mlds_output_global_var_defns(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

mlds_output_global_var_defns(_Opts, _Indent, _Separate, _ModuleName, [], !IO).
mlds_output_global_var_defns(Opts, Indent, Separate, ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    mlds_output_global_var_defn(Opts, Indent, Separate, ModuleName,
        GlobalVarDefn, !IO),
    mlds_output_global_var_defns(Opts, Indent, Separate, ModuleName,
        GlobalVarDefns, !IO).

:- pred mlds_output_field_var_defns(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_module_name::in, list(mlds_field_var_defn)::in,
    io::di, io::uo) is det.

mlds_output_field_var_defns(_Opts, _Indent, _Separate, _ModuleName, [], !IO).
mlds_output_field_var_defns(Opts, Indent, Separate, ModuleName,
        [FieldVarDefn | FieldVarDefns], !IO) :-
    mlds_output_field_var_defn(Opts, Indent, Separate, ModuleName,
        FieldVarDefn, !IO),
    mlds_output_field_var_defns(Opts, Indent, Separate, ModuleName,
        FieldVarDefns, !IO).

:- pred mlds_output_class_defns(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_class_defn)::in,
    io::di, io::uo) is det.

mlds_output_class_defns(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_class_defns(Opts, Indent, ModuleName,
        [ClassDefn | ClassDefns], !IO) :-
    mlds_output_class_defn(Opts, Indent, ModuleName, ClassDefn, !IO),
    mlds_output_class_defns(Opts, Indent, ModuleName, ClassDefns, !IO).

:- pred mlds_output_function_defns(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_function_defn)::in,
    io::di, io::uo) is det.

mlds_output_function_defns(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_function_defns(Opts, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    mlds_output_function_defn(Opts, Indent, ModuleName, FuncDefn, !IO),
    mlds_output_function_defns(Opts, Indent, ModuleName, FuncDefns, !IO).

:- pred mlds_output_global_var_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_global_var_defn)::in,
    io::di, io::uo) is det.

mlds_output_global_var_decls(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_global_var_decls(Opts, Indent, ModuleName,
        [GlobalVarDefn | GlobalVarDefns], !IO) :-
    io.nl(!IO),
    mlds_output_global_var_decl_opts(Opts, Indent, ModuleName, GlobalVarDefn,
        !IO),
    mlds_output_global_var_decls(Opts, Indent, ModuleName, GlobalVarDefns,
        !IO).

:- pred mlds_output_function_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_function_defn)::in,
    io::di, io::uo) is det.

mlds_output_function_decls(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_function_decls(Opts, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    io.nl(!IO),
    mlds_output_function_decl_opts(Opts, Indent, ModuleName, FuncDefn, !IO),
    mlds_output_function_decls(Opts, Indent, ModuleName, FuncDefns, !IO).

:- pred mlds_output_global_var_decl_opts(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_global_var_defn::in, io::di, io::uo) is det.

mlds_output_global_var_decl_opts(Opts, Indent, MLDS_ModuleName, GlobalVarDefn,
        !IO) :-
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, Context, Flags,
        Type, Initializer, _GCStmt),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_global_var_decl_flags(Flags, forward_decl, !IO),
    mlds_output_global_var_decl(Opts, MLDS_ModuleName, GlobalVarName, Type,
        get_initializer_array_size(Initializer), !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_function_decl_opts(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_function_defn::in, io::di, io::uo) is det.

mlds_output_function_decl_opts(Opts, Indent, ModuleName, FunctionDefn, !IO) :-
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),

    % If we are using --high-level-data, then for function declarations,
    % we need to ensure that we forward-declare any types used in the
    % function parameters. This is because otherwise, for any struct names
    % whose first occurrence is in the function parameters, the scope of
    % such struct names is just that function declaration, which is never
    % right.
    %
    % We generate such forward declarations here, rather than generating
    % type declarations in a header file and #including that header file,
    % because doing the latter would significantly complicate the dependencies
    % (to avoid cyclic #includes, you would need to declare the types
    % in a *different* header file than the functions).
    HighLevelData = Opts ^ m2co_highlevel_data,
    (
        HighLevelData = yes,
        Params = mlds_func_params(Arguments, _RetTypes),
        ParamTypes = mlds_get_arg_types(Arguments),
        mlds_output_type_forward_decls(Opts, Indent, ParamTypes, !IO)
    ;
        HighLevelData = no
    ),

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_function_decl_flags(Opts, Flags, forward_decl, MaybeBody, !IO),
    QualFuncName = qual_function_name(ModuleName, FuncName),

    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        mlds_output_pred_proc_id(Opts, PredProcId, !IO)
    ),
    mlds_output_func_decl(Opts, Indent, QualFuncName, Context, Params, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_class_decl_opts(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_class_defn::in, io::di, io::uo) is det.
:- pragma consider_used(mlds_output_class_decl_opts/6).

mlds_output_class_decl_opts(Opts, Indent, ModuleName, ClassDefn, !IO) :-
    ClassDefn = mlds_class_defn(ClassName, Arity, Context, Flags, Kind,
        _Imports, _Inherits, _Implements,
        _TypeParams, _MemberFields, _MemberClasses, _MemberMethods, _Ctors),

    % ANSI C does not permit forward declarations of enumeration types.
    % So we just skip those. Currently they are not needed since we don't
    % actually use the enum types.

    ( if Kind = mlds_enum then
        true
    else
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        mlds_output_class_decl_flags(Opts, Flags, forward_decl, !IO),
        mlds_output_class_decl(Indent, ModuleName, ClassName, Arity, ClassDefn,
            !IO),
        io.write_string(";\n", !IO)
    ).

:- pred mlds_output_scalar_cell_group_decls(mlds_to_c_opts::in, indent::in,
    string::in,
    assoc_list(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

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

:- pred mlds_output_scalar_cell_group_struct_defn(mlds_to_c_opts::in, int::in,
    string::in, int::in, list(mlds_type)::in, io::di, io::uo) is det.

mlds_output_scalar_cell_group_struct_defn(Opts, Indent, MangledModuleName,
        TypeRawNum, ElemTypes, !IO) :-
    output_pragma_pack_push(!IO),
    io.format("struct %s_scalar_cell_group_%d {\n",
        [s(MangledModuleName), i(TypeRawNum)], !IO),
    list.foldl2(mlds_output_scalar_cell_group_struct_field(Opts, Indent + 1),
        ElemTypes, 1, _, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("};\n", !IO),
    output_pragma_pack_pop(!IO).

:- pred mlds_output_scalar_cell_group_struct_field(mlds_to_c_opts::in,
    indent::in, mlds_type::in, int::in, int::out, io::di, io::uo) is det.

mlds_output_scalar_cell_group_struct_field(Opts, Indent, FieldType,
        Num, Num + 1, !IO) :-
    output_n_indents(Indent, !IO),
    ( if
        % Ensure double-word float, int64 and uint64 structure members
        % are word-aligned, not double-aligned.
        (
            FieldType = mlds_native_float_type,
            TypeName = "MR_Float_Aligned"
        ;
            FieldType = mercury_type(builtin_type(BuiltinType), _, _),
            (
                BuiltinType = builtin_type_float,
                TypeName = "MR_Float_Aligned"
            ;
                (
                    BuiltinType = builtin_type_int(int_type_int64),
                    TypeName = "MR_Int64Aligned"
                ;
                    BuiltinType = builtin_type_int(int_type_uint64),
                    TypeName = "MR_Uint64Aligned"
                )
            )
        )
    then
        io.write_string(TypeName, !IO)
    else
        mlds_output_type_prefix(Opts, FieldType, !IO)
    ),
    io.format(" f%d;\n", [i(Num)], !IO).

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

:- pred mlds_output_vector_cell_group_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, string::in,
    assoc_list(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

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

:- pred mlds_output_scalar_cell_group_defns(mlds_to_c_opts::in, indent::in,
    string::in,
    assoc_list(ml_scalar_common_type_num, ml_scalar_cell_group)::in,
    io::di, io::uo) is det.

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

:- pred mlds_output_vector_cell_group_defns(mlds_to_c_opts::in, indent::in,
    string::in,
    assoc_list(ml_vector_common_type_num, ml_vector_cell_group)::in,
    io::di, io::uo) is det.

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

:- pred mlds_output_alloc_site_decls(indent::in,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::in, io::di, io::uo) is det.

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

:- pred mlds_output_alloc_site_defns(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, assoc_list(mlds_alloc_id, ml_alloc_site_data)::in,
    io::di, io::uo) is det.

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

:- pred mlds_output_type_forward_decls(mlds_to_c_opts::in, indent::in,
    list(mlds_type)::in, io::di, io::uo) is det.

mlds_output_type_forward_decls(Opts, Indent, ParamTypes, !IO) :-
    % Output forward declarations for all struct types
    % that are contained in the parameter types.

    solutions.aggregate(mlds_type_list_contains_type(ParamTypes),
        mlds_output_type_forward_decl(Opts, Indent), !IO).

    % mlds_type_list_contains_type(Types, SubType):
    %
    % True iff the type SubType occurs (directly or indirectly) in the
    % specified list of Types.
    %
:- pred mlds_type_list_contains_type(list(mlds_type)::in, mlds_type::out)
    is nondet.

mlds_type_list_contains_type(Types, SubType) :-
    list.member(Type, Types),
    mlds_type_contains_type(Type, SubType).

    % mlds_type_contains_type(Type, SubType):
    %
    % True iff the type Type contains the type SubType.
    %
:- pred mlds_type_contains_type(mlds_type::in, mlds_type::out) is multi.

mlds_type_contains_type(Type, Type).
mlds_type_contains_type(mlds_mercury_array_type(Type), Type).
mlds_type_contains_type(mlds_array_type(Type), Type).
mlds_type_contains_type(mlds_ptr_type(Type), Type).
mlds_type_contains_type(mlds_func_type(Parameters), Type) :-
    Parameters = mlds_func_params(Arguments, RetTypes),
    ( list.member(mlds_argument(_Name, Type, _GCStmt), Arguments)
    ; list.member(Type, RetTypes)
    ).

:- pred mlds_output_type_forward_decl(mlds_to_c_opts::in, indent::in,
    mlds_type::in, io::di, io::uo) is det.

mlds_output_type_forward_decl(Opts, Indent, Type, !IO) :-
    ( if
        (
            Type = mlds_class_type(ClassId),
            ClassId = mlds_class_id(_Name, _Arity, Kind),
            Kind \= mlds_enum,
            ClassType = Type
        ;
            Type = mercury_type(MercuryType, _, ctor_cat_user(_)),
            type_to_ctor(MercuryType, TypeCtor),
            ml_gen_type_name(TypeCtor, ClassName, ClassArity),
            ClassId = mlds_class_id(ClassName, ClassArity, mlds_class),
            ClassType = mlds_class_type(ClassId)
        )
    then
        output_n_indents(Indent, !IO),
        mlds_output_type(Opts, ClassType, !IO),
        io.write_string(";\n", !IO)
    else
        true
    ).

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

:- pred mlds_output_local_var_defn(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_local_var_defn::in, io::di, io::uo) is det.

mlds_output_local_var_defn(Opts, Indent, Separate, LocalVarDefn, !IO) :-
    LocalVarDefn = mlds_local_var_defn(LocalVarName, Context,
        Type, Initializer, GCStmt),
    (
        Separate = yes,
        io.nl(!IO)
    ;
        Separate = no
    ),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_local_var_decl(Opts, LocalVarName, Type,
        get_initializer_array_size(Initializer), !IO),
    mlds_output_initializer(Opts, Type, Initializer, !IO),
    io.write_string(";\n", !IO),
    mlds_output_gc_statement(Opts, Indent, GCStmt, "", !IO).

:- pred mlds_output_field_var_defn(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_module_name::in, mlds_field_var_defn::in, io::di, io::uo) is det.

mlds_output_field_var_defn(Opts, Indent, Separate, ModuleName, FieldVarDefn,
        !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, Context, Flags,
        Type, Initializer, GCStmt),
    (
        Separate = yes,
        io.nl(!IO)
    ;
        Separate = no
    ),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_field_var_decl_flags(Opts, Flags, definition, !IO),
    QualFieldVarName =
        qual_field_var_name(ModuleName, module_qual, FieldVarName),
    mlds_output_field_var_decl(Opts, QualFieldVarName, Type,
        get_initializer_array_size(Initializer), !IO),
    mlds_output_initializer(Opts, Type, Initializer, !IO),
    io.write_string(";\n", !IO),
    mlds_output_gc_statement(Opts, Indent, GCStmt, "", !IO).

:- pred mlds_output_function_defn(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_function_defn::in, io::di, io::uo) is det.

mlds_output_function_defn(Opts, Indent, ModuleName, FunctionDefn, !IO) :-
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),
    io.nl(!IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_function_decl_flags(Opts, Flags, definition, MaybeBody, !IO),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        mlds_output_pred_proc_id(Opts, PredProcId, !IO)
    ),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    mlds_output_func(Opts, Indent, QualFuncName, Context, Params,
        MaybeBody, !IO).

:- pred mlds_output_class_defn(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_class_defn::in, io::di, io::uo) is det.

mlds_output_class_defn(Opts, Indent, ModuleName, ClassDefn, !IO) :-
    ClassDefn = mlds_class_defn(_ClassName, _Arity, Context, Flags, _Kind,
        _Imports, _Inherits, _Implements, _TypeParams,
        _MemberFields, _MemberClasses, _MemberMethods, _Ctors),
    io.nl(!IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_class_decl_flags(Opts, Flags, definition, !IO),
    mlds_output_class(Opts, Indent, ModuleName, ClassDefn, !IO).

:- pred mlds_output_gc_statement(mlds_to_c_opts::in, indent::in,
    mlds_gc_statement::in, string::in, io::di, io::uo) is det.

mlds_output_gc_statement(Opts, Indent, GCStmt, MaybeNewLine, !IO) :-
    (
        GCStmt = gc_no_stmt
    ;
        (
            GCStmt = gc_trace_code(Stmt),
            Label = "#if 0 // GC trace code\n"
        ;
            GCStmt = gc_initialiser(Stmt),
            Label = "#if 0 // GC initialiser\n"
        ),
        io.write_string(MaybeNewLine, !IO),
        io.write_string(Label, !IO),
        % XXX This value for FuncInfo is bogus. However, this output is only
        % for debugging anyway, so it doesn't really matter.
        ModuleName = mercury_module_name_to_mlds(unqualified("")),
        FuncName = mlds_function_export("dummy"),
        QualFuncName = qual_function_name(ModuleName, FuncName),
        FuncInfo = func_info_c(QualFuncName, mlds_func_signature([], [])),
        mlds_output_statement(Opts, Indent, FuncInfo, Stmt, !IO),
        io.write_string("#endif\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Code to output type declarations/definitions
%

:- pred mlds_output_class_decl(indent::in, mlds_module_name::in,
    mlds_class_name::in, arity::in, mlds_class_defn::in,
    io::di, io::uo) is det.

mlds_output_class_decl(_Indent, ModuleName, ClassName, Arity, ClassDefn,
        !IO) :-
    ClassKind = ClassDefn ^ mcd_kind,
    (
        ClassKind = mlds_enum,
        io.write_string("enum ", !IO),
        output_qual_name_prefix_c(ModuleName, !IO),
        mlds_output_class_name_arity(ClassName, Arity, !IO),
        io.write_string("_e", !IO)
    ;
        ( ClassKind = mlds_class
        ; ClassKind = mlds_interface
        ; ClassKind = mlds_struct
        ),
        io.write_string("struct ", !IO),
        output_qual_name_prefix_c(ModuleName, !IO),
        mlds_output_class_name_arity(ClassName, Arity, !IO),
        io.write_string("_s", !IO)
    ).

:- pred mlds_output_class(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_class_defn::in,
    io::di, io::uo) is det.

mlds_output_class(Opts, Indent, ModuleName, ClassDefn, !IO) :-
    ClassDefn = mlds_class_defn(ClassName, ClassArity, Context, _Flags,
        Kind, _Imports, Inherits, _Implements, _TypeParams,
        MemberFields, MemberClasses, MemberMethods, Ctors),
    expect(unify(MemberMethods, []), $pred,
        "MemberMethods != []"),

    % To avoid name clashes, we need to qualify the names of the member
    % constants with the class name. (In particular, this is needed for
    % enumeration constants and for the nested classes that we generate for
    % constructors of discriminated union types.) Here we compute the
    % appropriate qualifier.
    ClassModuleName = mlds_append_class_qualifier_module_qual(ModuleName,
        ClassName, ClassArity),

    % Hoist out static members, since plain old C doesn't support
    % static members in structs (except for enumeration constants).
    %
    % XXX This should be conditional: only when compiling to C,
    % not when compiling to C++.

    (
        Kind = mlds_enum,
        StaticCtors = [],
        StructCtors = Ctors,
        StaticMemberFields = [],
        StructMemberFields = MemberFields
    ;
        ( Kind = mlds_class
        ; Kind = mlds_interface
        ; Kind = mlds_struct
        ),
        list.filter(function_defn_is_static_member, Ctors,
            StaticCtors, NonStaticCtors),
        list.filter(field_var_defn_is_static_member, MemberFields,
            StaticMemberFields, NonStaticMemberFields),
        StructCtors = NonStaticCtors,
        StructMemberFields = NonStaticMemberFields
    ),

    % Convert the base classes into member variables,
    % since plain old C doesn't support base classes.
    %
    % XXX this should be conditional: only when compiling to C,
    % not when compiling to C++

    (
        Inherits = inherits_nothing,
        BaseFieldVarDefns = []
    ;
        Inherits = inherits_class(BaseClassId),
        BaseVarName = fvn_base_class(1),
        Type = mlds_class_type(BaseClassId),
        % We only need GC tracing code for top-level variables,
        % not for base classes.
        GCStmt = gc_no_stmt,
        BaseFieldVarDefns = [mlds_field_var_defn(BaseVarName, Context,
            ml_gen_public_field_decl_flags, Type, no_initializer, GCStmt)]
    ;
        Inherits = inherits_generic_env_ptr_type,
        % This should happen only if the target language requires
        % put_nondet_env_on_heap to be "yes"; for C, it should be "no".
        unexpected($pred, "inherits_generic_env_ptr_type")
    ),

    % Output the class declaration and the class members.
    % We treat enumerations specially.
    %
    % Note that standard ANSI/ISO C does not allow empty structs. We could
    % handle empty structs here, by adding a dummy member, but that would
    % waste a lot of space, and would also cause incompatibilities between
    % the data layout for --high-level-data and --no-high-level-data.
    % So instead, we make it is the responsibility of the MLDS code generator
    % to not generate any. (E.g. ml_type_gen.m checks whether
    % `target_uses_empty_base_classes' before generating empty structs.)
    % Hence we don't need to check for empty structs here.

    mlds_output_class_decl(Indent, ModuleName, ClassName, ClassArity,
        ClassDefn, !IO),
    io.write_string(" {\n", !IO),
    (
        Kind = mlds_enum,
        mlds_output_enum_constants(Opts, Indent + 1, ClassModuleName,
            BaseFieldVarDefns, !IO),
        mlds_output_enum_constants(Opts, Indent + 1, ClassModuleName,
            StructMemberFields, !IO)
    ;
        ( Kind = mlds_class
        ; Kind = mlds_interface
        ; Kind = mlds_struct
        ),
        % XXX Why don't we output all the field vars in one block?
        list.foldl(
            mlds_output_field_var_defn(Opts, Indent + 1, no, ClassModuleName),
            BaseFieldVarDefns, !IO),
        list.foldl(
            mlds_output_function_defn(Opts, Indent + 1, ClassModuleName),
            StructCtors, !IO),
        list.foldl(
            mlds_output_field_var_defn(Opts, Indent + 1, no, ClassModuleName),
            StructMemberFields, !IO)
    ),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("};\n", !IO),
    mlds_output_function_defns(Opts, Indent, ClassModuleName,
        StaticCtors, !IO),
    mlds_output_field_var_defns(Opts, Indent, yes, ClassModuleName,
        StaticMemberFields, !IO),
    mlds_output_class_defns(Opts, Indent, ClassModuleName,
        MemberClasses, !IO).

:- pred field_var_defn_is_static_member(mlds_field_var_defn::in) is semidet.

field_var_defn_is_static_member(FieldVarDefn) :-
    FieldVarDefn ^ mfvd_decl_flags ^ mfvdf_per_instance = one_copy.

:- pred function_defn_is_static_member(mlds_function_defn::in) is semidet.

function_defn_is_static_member(FuncDefn) :-
    FuncDefn ^ mfd_decl_flags = mlds_function_decl_flags(_Access, PerInstance),
    PerInstance = one_copy.

    % Output the definitions of the enumeration constants
    % for an enumeration type.
    %
:- pred mlds_output_enum_constants(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_field_var_defn)::in, io::di, io::uo)
    is det.

mlds_output_enum_constants(Opts, Indent, EnumModuleName, MemberFields, !IO) :-
    % Select the enumeration constants from the list of members
    % for this enumeration type, and output them.
    list.filter(field_var_defn_is_enum_const, MemberFields,
        EnumConstMemberFields),
    io.write_list(EnumConstMemberFields, ",\n",
        mlds_output_enum_constant(Opts, Indent, EnumModuleName), !IO),
    io.nl(!IO).

    % Output the definition of a single enumeration constant.
    %
:- pred mlds_output_enum_constant(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_field_var_defn::in, io::di, io::uo) is det.

mlds_output_enum_constant(Opts, Indent, EnumModuleName, FieldVarDefn, !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, Context, _Flags,
        Type, Initializer, _GCStmt),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    QualFieldVarName =
        qual_field_var_name(EnumModuleName, type_qual, FieldVarName),
    mlds_output_fully_qualified_field_var_name(QualFieldVarName, !IO),
    mlds_output_initializer(Opts, Type, Initializer, !IO).

%---------------------------------------------------------------------------%
%
% Code to output data declarations/definitions.
%

:- pred mlds_output_global_var_decl(mlds_to_c_opts::in,
    mlds_module_name::in, mlds_global_var_name::in, mlds_type::in,
    initializer_array_size::in, io::di, io::uo) is det.

mlds_output_global_var_decl(Opts, MLDS_ModuleName, GlobalVarName, Type,
        InitializerSize, !IO) :-
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName, GlobalVarName,
        !IO),
    mlds_output_type_suffix(Opts, Type, InitializerSize, !IO).

:- pred mlds_output_local_var_decl(mlds_to_c_opts::in, mlds_local_var_name::in,
    mlds_type::in, initializer_array_size::in, io::di, io::uo) is det.

mlds_output_local_var_decl(Opts, LocalVarName, Type, InitializerSize, !IO) :-
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    mlds_output_type_suffix(Opts, Type, InitializerSize, !IO).

:- pred mlds_output_field_var_decl(mlds_to_c_opts::in, qual_field_var_name::in,
    mlds_type::in, initializer_array_size::in, io::di, io::uo) is det.

mlds_output_field_var_decl(Opts, FieldVarName, Type, InitializerSize, !IO) :-
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_fully_qualified_field_var_name(FieldVarName, !IO),
    mlds_output_type_suffix(Opts, Type, InitializerSize, !IO).

:- pred mlds_output_initializer(mlds_to_c_opts::in, mlds_type::in,
    mlds_initializer::in, io::di, io::uo) is det.

mlds_output_initializer(Opts, _Type, Initializer, !IO) :-
    NeedsInit = mlds_needs_initialization(Initializer),
    (
        NeedsInit = yes,
        io.write_string(" = ", !IO),
        mlds_output_initializer_body(Opts, 0, Initializer, !IO)
    ;
        NeedsInit = no
    ).

:- func mlds_needs_initialization(mlds_initializer) = bool.

mlds_needs_initialization(no_initializer) = no.
mlds_needs_initialization(init_obj(_)) = yes.
mlds_needs_initialization(init_struct(_Type, [])) = no.
mlds_needs_initialization(init_struct(_Type, [_|_])) = yes.
mlds_needs_initialization(init_array(_)) = yes.

:- pred mlds_output_initializer_body(mlds_to_c_opts::in, int::in,
    mlds_initializer::in, io::di, io::uo) is det.

mlds_output_initializer_body(Opts, Indent, Initializer, !IO) :-
    (
        Initializer = no_initializer
    ;
        Initializer = init_obj(Rval),
        output_n_indents(Indent, !IO),
        mlds_output_rval(Opts, Rval, !IO)
    ;
        Initializer = init_struct(_Type, FieldInitializers),
        % Note that standard ANSI/ISO C does not allow empty structs, and
        % it is the responsibility of the MLDS code generator to not generate
        % any such structs.
        (
            FieldInitializers = [],
            unexpected($pred, "FieldInitializers = []")
        ;
            FieldInitializers = [FieldInitializer],
            output_n_indents(Indent, !IO),
            io.write_string("{ ", !IO),
            mlds_output_initializer_body(Opts, Indent + 1,
                FieldInitializer, !IO),
            io.write_string(" }", !IO)
        ;
            FieldInitializers = [_, _ | _],
            output_n_indents(Indent, !IO),
            io.write_string("{\n", !IO),
            io.write_list(FieldInitializers, ",\n",
                mlds_output_initializer_body(Opts, Indent + 1), !IO),
            io.write_string("\n", !IO),
            output_n_indents(Indent, !IO),
            io.write_string("}", !IO)
        )
    ;
        Initializer = init_array(ElementInitializers),
        % Standard ANSI/ISO C does not allow empty arrays. But the MLDS does.
        % To keep the C compiler happy, we therefore convert zero-element MLDS
        % arrays into one-element C arrays. (The extra element is a minor waste
        % of space, but it will otherwise be ignored.) So if the initializer
        % list here is empty, we need to output a single initializer.
        % We can initialize the extra element with any value. We use "0",
        % since that is a valid initializer for any type.
        (
            ElementInitializers = [],
            output_n_indents(Indent, !IO),
            io.write_string("{ 0 }\n", !IO)
        ;
            ElementInitializers = [_ | _],
            output_n_indents(Indent, !IO),
            io.write_string("{\n", !IO),
            io.write_list(ElementInitializers, ",\n",
                mlds_output_initializer_body(Opts, Indent + 1), !IO),
            io.write_string("\n", !IO),
            output_n_indents(Indent, !IO),
            io.write_string("}", !IO)
        )
    ).

%---------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- pred mlds_output_pred_proc_id(mlds_to_c_opts::in, pred_proc_id::in,
    io::di, io::uo) is det.

mlds_output_pred_proc_id(Opts, proc(PredId, ProcId), !IO) :-
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        io.write_string("// pred_id: ", !IO),
        pred_id_to_int(PredId, PredIdNum),
        io.write_int(PredIdNum, !IO),
        io.write_string(", proc_id: ", !IO),
        proc_id_to_int(ProcId, ProcIdNum),
        io.write_int(ProcIdNum, !IO),
        io.nl(!IO)
    ;
        Comments = no
    ).

:- pred mlds_output_func(mlds_to_c_opts::in, indent::in,
    qual_function_name::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

mlds_output_func(Opts, Indent, QualFuncName, Context, Params,
        FunctionBody, !IO) :-
    mlds_output_func_decl(Opts, Indent, QualFuncName, Context, Params, !IO),
    (
        FunctionBody = body_external,
        io.write_string(";\n", !IO)
    ;
        FunctionBody = body_defined_here(Body),
        io.write_string("\n", !IO),

        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("{\n", !IO),

        ProfileTime = Opts ^ m2co_profile_time,
        (
            ProfileTime = yes,
            mlds_output_time_profile_instr(Opts, Context, Indent + 1,
                QualFuncName, !IO)
        ;
            ProfileTime = no
        ),

        Signature = mlds_get_func_signature(Params),
        FuncInfo = func_info_c(QualFuncName, Signature),
        mlds_output_statement(Opts, Indent + 1, FuncInfo, Body, !IO),

        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)    % end the function
    ).

:- pred mlds_output_func_decl(mlds_to_c_opts::in, indent::in,
    qual_function_name::in, prog_context::in, mlds_func_params::in,
    io::di, io::uo) is det.

mlds_output_func_decl(Opts, Indent, QualifiedName, Context, Signature, !IO) :-
    CallingConvention = "MR_CALL ",
    mlds_output_func_decl_ho(Opts, Indent, QualifiedName, Context,
        CallingConvention, Signature,
        mlds_output_type_prefix, mlds_output_type_suffix_no_size, !IO).

:- pred mlds_output_func_decl_ho(mlds_to_c_opts::in, indent::in,
    qual_function_name::in, prog_context::in, string::in,
    mlds_func_params::in,
    output_type::in(output_type), output_type::in(output_type),
    io::di, io::uo) is det.

mlds_output_func_decl_ho(Opts, Indent, QualFuncName, Context,
        CallingConvention, Signature, OutputPrefix, OutputSuffix, !IO) :-
    Signature = mlds_func_params(Parameters0, RetTypes),
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        OutputPrefix(Opts, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(RetTypes,
            mlds_output_prefix_suffix(Opts, OutputPrefix, OutputSuffix), !IO)
    ),
    io.write_char(' ', !IO),
    io.write_string(CallingConvention, !IO),
    io.nl(!IO),
    mlds_output_fully_qualified_function_name(QualFuncName, !IO),
    StdDecl = Opts ^ m2co_std_func_decl,
    (
        StdDecl = no,
        Parameters = Parameters0
    ;
        StdDecl = yes,
        list.map_foldl(standardize_param_names, Parameters0, Parameters, 1, _)
    ),
    mlds_output_params(Opts, OutputPrefix, OutputSuffix, Indent,
        Context, Parameters, !IO),
    (
        RetTypes = [RetType2],
        OutputSuffix(Opts, RetType2, !IO)
    ;
        RetTypes = []
    ;
        RetTypes = [_, _ | _]
    ).

:- pred standardize_param_names(mlds_argument::in, mlds_argument::out,
    int::in, int::out) is det.

standardize_param_names(!Argument, !ArgNum) :-
    VarName = lvn_comp_var(lvnc_param(!.ArgNum)),
    !.Argument = mlds_argument(_VarName0, Type, GCStmt),
    !:Argument = mlds_argument(VarName, Type, GCStmt),
    !:ArgNum = !.ArgNum + 1.

:- pred mlds_output_prefix_suffix(mlds_to_c_opts::in,
    output_type::in(output_type), output_type::in(output_type), mlds_type::in,
    io::di, io::uo) is det.

mlds_output_prefix_suffix(Opts, OutputPrefix, OutputSuffix, Value, !IO) :-
    OutputPrefix(Opts, Value, !IO),
    OutputSuffix(Opts, Value, !IO).

:- pred mlds_output_params(mlds_to_c_opts::in, output_type::in(output_type),
    output_type::in(output_type), indent::in,
    prog_context::in, list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_params(Opts, OutputPrefix, OutputSuffix, Indent, Context,
        Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = [],
        io.write_string("void", !IO)
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            mlds_output_param(Opts, OutputPrefix, OutputSuffix,
                Indent + 1, Context),
            !IO)
    ),
    io.write_char(')', !IO).

:- pred mlds_output_param(mlds_to_c_opts::in, output_type::in(output_type),
    output_type::in(output_type), indent::in,
    prog_context::in, mlds_argument::in, io::di, io::uo) is det.

mlds_output_param(Opts, OutputPrefix, OutputSuffix, Indent,
        Context, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, GCStmt),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    OutputPrefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    OutputSuffix(Opts, Type, !IO),
    mlds_output_gc_statement(Opts, Indent, GCStmt, "\n", !IO).

:- pred mlds_output_func_type_prefix(mlds_to_c_opts::in, mlds_func_params::in,
    io::di, io::uo) is det.

mlds_output_func_type_prefix(Opts, Params, !IO) :-
    Params = mlds_func_params(_Parameters, RetTypes),
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        mlds_output_type(Opts, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(RetTypes, mlds_output_type(Opts), !IO)
    ),
    % Note that mlds_func_type actually corresponds to a function _pointer_
    % type in C. This is necessary because function types in C are not first
    % class.
    io.write_string(" MR_CALL (*", !IO).

:- pred mlds_output_func_type_suffix(mlds_to_c_opts::in, mlds_func_params::in,
    io::di, io::uo) is det.

mlds_output_func_type_suffix(Opts, Params, !IO) :-
    Params = mlds_func_params(Parameters, _RetTypes),
    io.write_string(")", !IO),
    mlds_output_param_types(Opts, Parameters, !IO).

:- pred mlds_output_param_types(mlds_to_c_opts::in, list(mlds_argument)::in,
    io::di, io::uo) is det.

mlds_output_param_types(Opts, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = [],
        io.write_string("void", !IO)
    ;
        Parameters = [_ | _],
        io.write_list(Parameters, ", ", mlds_output_param_type(Opts), !IO)
    ),
    io.write_char(')', !IO).

:- pred mlds_output_param_type(mlds_to_c_opts::in, mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_param_type(Opts, Arg, !IO) :-
    Arg = mlds_argument(_Name, Type, _GCStmt),
    mlds_output_type(Opts, Type, !IO).

%---------------------------------------------------------------------------%
%
% Code to output names of various entities.
%

:- pred mlds_output_fully_qualified_function_name(
    qual_function_name::in, io::di, io::uo) is det.

mlds_output_fully_qualified_function_name(QualFuncName, !IO) :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    ( if
        (
            % Don't module-qualify main/2.
            FuncName = mlds_function_name(PlainFuncName),
            PlainFuncName = mlds_plain_func_name(FuncLabel, _),
            FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
            ProcLabel = mlds_proc_label(PredLabel, _ProcId),
            PredLabel = mlds_user_pred_label(pf_predicate, no, "main", 2,
                model_det, no)
        ;
            % We don't module qualify pragma foreign_export names.
            FuncName = mlds_function_export(_)
        )
    then
        true
    else
        output_qual_name_prefix_c(ModuleName, !IO)
    ),
    mlds_output_function_name(FuncName, !IO).

:- pred mlds_output_maybe_qualified_global_var_name(mlds_module_name::in,
    mlds_global_var_name::in, io::di, io::uo) is det.

mlds_output_maybe_qualified_global_var_name(ModuleName, GlobalVarName, !IO) :-
    ShouldModuleQual = should_module_qualify_global_var_name(GlobalVarName),
    (
        ShouldModuleQual = no
    ;
        ShouldModuleQual = yes,
        output_qual_name_prefix_c(ModuleName, !IO)
    ),
    mlds_output_global_var_name(GlobalVarName, !IO).

:- func should_module_qualify_global_var_name(mlds_global_var_name) = bool.

should_module_qualify_global_var_name(GlobalVarName) = ShouldModuleQual :-
    (
        GlobalVarName = gvn_rtti_var(RttiId),
        ShouldModuleQual = module_qualify_name_of_rtti_id(RttiId)
    ;
        ( GlobalVarName = gvn_const_var(_, _)
        ; GlobalVarName = gvn_tabling_var(_, _)
        ),
        ShouldModuleQual = no
    ;
        GlobalVarName = gvn_dummy_var,
        % The reference is to private_builtin.dummy_var, which is not in the
        % current module (unless we are compiling private_builtin.m).
        ShouldModuleQual = yes
    ).

:- pred mlds_output_fully_qualified_field_var_name(qual_field_var_name::in,
    io::di, io::uo) is det.

mlds_output_fully_qualified_field_var_name(QualFieldVarName, !IO) :-
    QualFieldVarName = qual_field_var_name(ModuleName, _, FieldVarName),
    output_qual_name_prefix_c(ModuleName, !IO),
    mlds_output_field_var_name(FieldVarName, !IO).

:- pred mlds_output_fully_qualified_proc_label(qual_proc_label::in,
    io::di, io::uo) is det.

mlds_output_fully_qualified_proc_label(QualProcLabel, !IO) :-
    QualProcLabel = qual_proc_label(ModuleName, Name),
    Name = mlds_proc_label(PredLabel, _ProcId),
    ( if
        % Don't module-qualify main/2.
        PredLabel = mlds_user_pred_label(pf_predicate, no, "main", 2,
            model_det, no)
    then
        true
    else
        output_qual_name_prefix_c(ModuleName, !IO)
    ),
    mlds_output_proc_label(Name, !IO).

:- pred output_qual_name_prefix_c(mlds_module_name::in, io::di, io::uo) is det.

output_qual_name_prefix_c(ModuleName, !IO) :-
    SymName = mlds_module_name_to_sym_name(ModuleName),
    MangledModuleName = sym_name_mangle(SymName),
    io.write_string(MangledModuleName, !IO),
    io.write_string("__", !IO).

:- func qual_name_prefix_c(mlds_module_name) = string.

qual_name_prefix_c(ModuleName) = ModuleNamePrefix :-
    SymName = mlds_module_name_to_sym_name(ModuleName),
    MangledModuleName = sym_name_mangle(SymName),
    ModuleNamePrefix = MangledModuleName ++ "__".

:- pred mlds_output_module_name(mercury_module_name::in, io::di, io::uo)
    is det.

mlds_output_module_name(ModuleName, !IO) :-
    MangledModuleName = sym_name_mangle(ModuleName),
    io.write_string(MangledModuleName, !IO).

:- pred mlds_output_class_name_arity(mlds_class_name::in, arity::in,
    io::di, io::uo) is det.

mlds_output_class_name_arity(ClassName, Arity, !IO) :-
    % XXX We should avoid appending the arity if it is not needed.
    MangledClassName = name_mangle(ClassName),
    io.write_string(MangledClassName, !IO),
    io.write_char('_', !IO),
    io.write_int(Arity, !IO).

:- pred mlds_output_function_name(mlds_function_name::in,
    io::di, io::uo) is det.

mlds_output_function_name(FunctionName, !IO) :-
    % XXX We should avoid appending the modenum, and seqnum
    % if they are not needed.
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        mlds_output_pred_label(PredLabel, !IO),
        proc_id_to_int(ProcId, ModeNum),
        io.write_char('_', !IO),
        io.write_int(ModeNum, !IO),
        io.write_string(mlds_maybe_aux_func_id_to_suffix(MaybeAux), !IO)
    ;
        FunctionName = mlds_function_export(Name),
        io.write_string(Name, !IO)
    ).

    % mlds_output_pred_label should be kept in sync with
    % mlds_pred_label_to_string and browser/name_mangle.m.
    %
:- pred mlds_output_pred_label(mlds_pred_label::in, io::di, io::uo) is det.

mlds_output_pred_label(PredLabel, !IO) :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
            Name, PredArity, _CodeModel, _NonOutputFunc),
        (
            PredOrFunc = pf_predicate,
            Suffix = "p",
            OrigArity = PredArity
        ;
            PredOrFunc = pf_function,
            Suffix = "f",
            OrigArity = PredArity - 1
        ),
        MangledName = name_mangle(Name),
        io.write_string(MangledName, !IO),
        io.write_char('_', !IO),
        io.write_int(OrigArity, !IO),
        io.write_char('_', !IO),
        io.write_string(Suffix, !IO),
        (
            MaybeDefiningModule = yes(DefiningModule),
            io.write_string("_in__", !IO),
            mlds_output_module_name(DefiningModule, !IO)
        ;
            MaybeDefiningModule = no
        )
    ;
        PredLabel = mlds_special_pred_label(PredName, MaybeTypeModule,
            TypeName, TypeArity),
        MangledPredName = name_mangle(PredName),
        MangledTypeName = name_mangle(TypeName),
        io.write_string(MangledPredName, !IO),
        io.write_string("__", !IO),
        (
            MaybeTypeModule = yes(TypeModule),
            mlds_output_module_name(TypeModule, !IO),
            io.write_string("__", !IO)
        ;
            MaybeTypeModule = no
        ),
        io.write_string(MangledTypeName, !IO),
        io.write_string("_", !IO),
        io.write_int(TypeArity, !IO)
    ).

    % mlds_pred_label_to_string should be kept in sync with
    % mlds_output_pred_label and browser/name_mangle.m.
    %
:- func mlds_pred_label_to_string(mlds_pred_label) = string.

mlds_pred_label_to_string(PredLabel) = Str :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
            Name, PredArity, _CodeModel, _NonOutputFunc),
        (
            PredOrFunc = pf_predicate,
            Suffix = "p",
            OrigArity = PredArity
        ;
            PredOrFunc = pf_function,
            Suffix = "f",
            OrigArity = PredArity - 1
        ),
        MangledName = name_mangle(Name),
        MainStr = MangledName ++ "_" ++ string.int_to_string(OrigArity)
            ++ "_" ++ Suffix,
        (
            MaybeDefiningModule = yes(DefiningModule),
            Str = MainStr ++ "_in__" ++ sym_name_mangle(DefiningModule)
        ;
            MaybeDefiningModule = no,
            Str = MainStr
        )
    ;
        PredLabel = mlds_special_pred_label(PredName, MaybeTypeModule,
            TypeName, TypeArity),
        MangledPredName = name_mangle(PredName),
        MangledTypeName = name_mangle(TypeName),
        PrefixStr = MangledPredName ++ "__",
        (
            MaybeTypeModule = yes(TypeModule),
            MidStr = sym_name_mangle(TypeModule) ++ "__"
        ;
            MaybeTypeModule = no,
            MidStr = ""
        ),
        Str = PrefixStr ++ MidStr ++ MangledTypeName ++ "_" ++
            int_to_string(TypeArity)
    ).

mlds_tabling_data_name(ProcLabel, TablingId) =
    tabling_info_id_str(TablingId) ++ "_for_" ++
        mlds_proc_label_to_string(mlds_std_tabling_proc_label(ProcLabel)).

:- pred mlds_output_global_var_name(mlds_global_var_name::in,
    io::di, io::uo) is det.

mlds_output_global_var_name(GlobalVarName, !IO) :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        mlds_output_mangled_name(
            ml_global_const_var_name_to_string(ConstVar, Num), !IO)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.write_string(RttiAddrName, !IO)
    ;
        GlobalVarName = gvn_tabling_var(ProcLabel, TablingId),
        io.write_string(mlds_tabling_data_name(ProcLabel, TablingId), !IO)
    ;
        GlobalVarName = gvn_dummy_var,
        io.write_string("dummy_var", !IO)
    ).

:- pred mlds_output_local_var_name(mlds_local_var_name::in,
    io::di, io::uo) is det.

mlds_output_local_var_name(LocalVarName, !IO) :-
    mlds_output_mangled_name(ml_local_var_name_to_string(LocalVarName), !IO).

:- pred mlds_output_field_var_name(mlds_field_var_name::in,
    io::di, io::uo) is det.

mlds_output_field_var_name(FieldVarName, !IO) :-
    mlds_output_mangled_name(ml_field_var_name_to_string(FieldVarName), !IO).

%---------------------------------------------------------------------------%
%
% Code to output types.
%

:- pred mlds_output_type(mlds_to_c_opts::in, mlds_type::in, io::di, io::uo)
    is det.

mlds_output_type(Opts, Type, !IO) :-
    % Because of the joys of C syntax, the code for outputting types
    % needs to be split into two parts; first the prefix, i.e. the part
    % of the type name that goes before the variable name in a variable
    % declaration, and then the suffix, i.e. the part which goes after
    % the variable name, e.g. the "[]" for array types.
    %
    % In the declaration of a type, as opposed to the declaration of a
    % variable, the variable name is not there, so we have just the prefix and
    % the suffix.

    mlds_output_type_prefix(Opts, Type, !IO),
    mlds_output_type_suffix(Opts, Type, no_size, !IO).

:- pred mlds_output_type_prefix(mlds_to_c_opts::in, mlds_type::in,
    io::di, io::uo) is det.

mlds_output_type_prefix(Opts, MLDS_Type, !IO) :-
    (
        MLDS_Type = mercury_type(Type, _, TypeCategory),
        mlds_output_mercury_type_prefix(Opts, Type, TypeCategory, !IO)
    ;
        MLDS_Type = mlds_mercury_array_type(_ElemType),
        HighLevelData = Opts ^ m2co_highlevel_data,
        (
            HighLevelData = yes,
            mlds_output_mercury_user_type_name(Opts,
                type_ctor(qualified(unqualified("array"), "array"), 1),
                ctor_cat_user(cat_user_general), !IO)
        ;
            HighLevelData = no,
            io.write_string("MR_ArrayPtr", !IO)
        )
    ;
        MLDS_Type = mlds_native_int_type,
        io.write_string("MR_Integer", !IO)
    ;
        MLDS_Type = mlds_native_uint_type,
        io.write_string("MR_Unsigned", !IO)
    ;
        MLDS_Type = mlds_native_float_type,
        io.write_string("MR_Float", !IO)
    ;
        MLDS_Type = mlds_native_bool_type,
        io.write_string("MR_bool", !IO)
    ;
        MLDS_Type = mlds_native_char_type,
        io.write_string("MR_Char", !IO)
    ;
        MLDS_Type = mlds_foreign_type(_ForeignType),
        % For binary compatibility with the --target asm back-end,
        % we need to output these as a generic type, rather than making
        % use of the C type name
        % XXX target asm no longer exists, so no longer need to do this.
        io.write_string("MR_Box", !IO)
    ;
        MLDS_Type = mlds_class_type(ClassId),
        ClassId = mlds_class_id(QualClassName, Arity, ClassKind),
        QualClassName = qual_class_name(ModuleName, _QualKind, ClassName),
        (
            ClassKind = mlds_enum,
            % We can't just use the enumeration type, since the enumeration
            % type's definition is not guaranteed to be in scope at this point.
            % (Fixing that would be somewhat complicated; it would require
            % writing enum definitions to a separate header file.) Also,
            % the enumeration might not be word-sized, which would cause
            % problems for e.g. `std_util.arg/2'. So we just use `MR_Integer',
            % and output the actual enumeration type as a comment.
            io.format("MR_Integer /* actually `enum %s%s_%d_e' */",
                [s(qual_name_prefix_c(ModuleName)), s(name_mangle(ClassName)),
                i(Arity)], !IO)
        ;
            ( ClassKind = mlds_class
            ; ClassKind = mlds_interface
            ; ClassKind = mlds_struct
            ),
            % For struct types, it is OK to output an incomplete type, since
            % we don't use these types directly; we only use pointers to them.
            io.format("struct %s%s_%d_s",
                [s(qual_name_prefix_c(ModuleName)), s(name_mangle(ClassName)),
                i(Arity)], !IO)
        )
    ;
        MLDS_Type = mlds_ptr_type(Type),
        mlds_output_type(Opts, Type, !IO),
        io.write_string(" *", !IO)
    ;
        MLDS_Type = mlds_array_type(Type),
        % Here we just output the element type. The "[]" goes in the type
        % suffix.
        mlds_output_type(Opts, Type, !IO)
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_),
        Type = mlds_generic_type,
        mlds_output_type(Opts, Type, !IO)
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        mlds_output_func_type_prefix(Opts, FuncParams, !IO)
    ;
        MLDS_Type = mlds_generic_type,
        io.write_string("MR_Box", !IO)
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        io.write_string("void *", !IO)
    ;
        MLDS_Type = mlds_type_info_type,
        io.write_string("MR_TypeInfo", !IO)
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        io.write_string("MR_PseudoTypeInfo", !IO)
    ;
        MLDS_Type = mlds_cont_type(ArgTypes),
        (
            ArgTypes = [],
            io.write_string("MR_Cont", !IO)
        ;
            ArgTypes = [_ | _],
            % This case only happens for --nondet-copy-out.
            io.write_string("void MR_CALL (*", !IO)
        )
    ;
        MLDS_Type = mlds_commit_type,
        io.write_string("jmp_buf", !IO)
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_c_type(RttiIdMaybeElement, CType, _IsArray),
        io.write_string(CType, !IO)
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        tabling_id_c_type(TablingId, CType, _IsArray),
        io.write_string(CType, !IO)
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "prefix has unknown type")
    ).

:- pred mlds_output_mercury_type_prefix(mlds_to_c_opts::in, mer_type::in,
    type_ctor_category::in, io::di, io::uo) is det.

mlds_output_mercury_type_prefix(Opts, Type, CtorCat, !IO) :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        io.write_string("MR_Char", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int)),
        io.write_string("MR_Integer", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint)),
        io.write_string("MR_Unsigned", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int8)),
        io.write_string("int8_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint8)),
        io.write_string("uint8_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int16)),
        io.write_string("int16_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint16)),
        io.write_string("uint16_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int32)),
        io.write_string("int32_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint32)),
        io.write_string("uint32_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int64)),
        io.write_string("int64_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint64)),
        io.write_string("uint64_t", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        io.write_string("MR_String", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        io.write_string("MR_Float", !IO)
    ;
        CtorCat = ctor_cat_void,
        io.write_string("MR_Word", !IO)
    ;
        CtorCat = ctor_cat_variable,
        io.write_string("MR_Box", !IO)
    ;
        CtorCat = ctor_cat_tuple,
        io.write_string("MR_Tuple", !IO)
    ;
        CtorCat = ctor_cat_higher_order,
        HighLevelData = Opts ^ m2co_highlevel_data,
        (
            HighLevelData = yes,
            io.write_string("MR_ClosurePtr", !IO)
        ;
            HighLevelData = no,
            io.write_string("MR_Word", !IO)
        )
    ;
        % runtime/mercury_hlc_types requires typeinfos, typeclass_infos etc
        % to be treated as user defined types.
        ( CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_system(_)
        ),
        mlds_output_mercury_user_type_prefix(Opts, Type, CtorCat, !IO)
    ).

:- pred mlds_output_mercury_user_type_prefix(mlds_to_c_opts::in, mer_type::in,
    type_ctor_category::in, io::di, io::uo) is det.

mlds_output_mercury_user_type_prefix(Opts, Type, CtorCat, !IO) :-
    HighLevelData = Opts ^ m2co_highlevel_data,
    (
        HighLevelData = yes,
        type_to_ctor_det(Type, TypeCtor),
        mlds_output_mercury_user_type_name(Opts, TypeCtor, CtorCat, !IO)
    ;
        HighLevelData = no,
        % In this case, we just treat everything as `MR_Word'.
        io.write_string("MR_Word", !IO)
    ).

:- pred mlds_output_mercury_user_type_name(mlds_to_c_opts::in, type_ctor::in,
    type_ctor_category::in, io::di, io::uo) is det.

mlds_output_mercury_user_type_name(Opts, TypeCtor, CtorCat, !IO) :-
    ml_gen_type_name(TypeCtor, ClassName, ClassArity),
    (
        CtorCat = ctor_cat_enum(_),
        ClassId = mlds_class_id(ClassName, ClassArity, mlds_enum),
        MLDS_Type = mlds_class_type(ClassId)
    ;
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(_)
        ),
        ClassId = mlds_class_id(ClassName, ClassArity, mlds_class),
        Type = mlds_class_type(ClassId),
        MLDS_Type = mlds_ptr_type(Type)
    ),
    mlds_output_type_prefix(Opts, MLDS_Type, !IO).

:- pred mlds_output_type_suffix_no_size(mlds_to_c_opts::in, mlds_type::in,
    io::di, io::uo) is det.

mlds_output_type_suffix_no_size(Opts, Type, !IO) :-
    mlds_output_type_suffix(Opts, Type, no_size, !IO).

:- pred mlds_output_type_suffix(mlds_to_c_opts::in, mlds_type::in,
    initializer_array_size::in, io::di, io::uo) is det.

mlds_output_type_suffix(Opts, MLDS_Type, ArraySize, !IO) :-
    (
        MLDS_Type = mlds_array_type(_),
        mlds_output_array_type_suffix(ArraySize, !IO)
    ;
        MLDS_Type = mlds_mostly_generic_array_type(_)
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        mlds_output_func_type_suffix(Opts, FuncParams, !IO)
    ;
        MLDS_Type = mlds_cont_type(ArgTypes),
        (
            ArgTypes = []
        ;
            ArgTypes = [_ | _],
            % This case only happens for --nondet-copy-out.
            io.write_string(")(", !IO),
            io.write_list(ArgTypes, ", ", mlds_output_type(Opts), !IO),
            % add the type for the environment parameter, if needed
            io.write_string(", void *)", !IO)
        )
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        IsArrayType = rtti_id_maybe_element_has_array_type(RttiIdMaybeElement),
        (
            IsArrayType = is_array,
            mlds_output_array_type_suffix(ArraySize, !IO)
        ;
            IsArrayType = not_array
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        IsArrayType = tabling_id_has_array_type(TablingId),
        (
            IsArrayType = is_array,
            mlds_output_array_type_suffix(ArraySize, !IO)
        ;
            IsArrayType = not_array
        )
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "unknown_type")
    ;
        ( MLDS_Type = mercury_type(_, _, _)
        ; MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_native_int_type
        ; MLDS_Type = mlds_native_uint_type
        ; MLDS_Type = mlds_native_float_type
        ; MLDS_Type = mlds_native_bool_type
        ; MLDS_Type = mlds_native_char_type
        % XXX Currently we can't output a type suffix.
        ; MLDS_Type = mlds_foreign_type(_)
        ; MLDS_Type = mlds_class_type(_)
        ; MLDS_Type = mlds_ptr_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ; MLDS_Type = mlds_commit_type
        )
    ).

:- pred mlds_output_array_type_suffix(initializer_array_size::in,
    io::di, io::uo) is det.

mlds_output_array_type_suffix(no_size, !IO) :-
    io.write_string("[]", !IO).
mlds_output_array_type_suffix(array_size(Size0), !IO) :-
    % Standard ANSI/ISO C does not allow arrays of size 0. But the MLDS does.
    % To keep the C compiler happy, we therefore convert zero-element MLDS
    % arrays into one-element C arrays.
    int.max(Size0, 1, Size),
    io.write_char('[', !IO),
    io.write_int(Size, !IO),
    io.write_char(']', !IO).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- type decl_or_defn
    --->    forward_decl
    ;       definition.

:- pred mlds_output_global_var_decl_flags(mlds_global_var_decl_flags::in,
    decl_or_defn::in, io::di, io::uo) is det.

mlds_output_global_var_decl_flags(Flags, DeclOrDefn, !IO) :-
    Flags = mlds_global_var_decl_flags(Access, Constness),
    % Everything that one may want to know about Flags is available
    % in the output of the next two calls, so printing comments is not useful.
    mlds_output_global_var_extern_or_static(Access, DeclOrDefn, !IO),
    mlds_output_constness(Constness, !IO).

:- pred mlds_output_field_var_decl_flags(mlds_to_c_opts::in,
    mlds_field_var_decl_flags::in, decl_or_defn::in, io::di, io::uo) is det.

mlds_output_field_var_decl_flags(Opts, Flags, DeclOrDefn, !IO) :-
    Constness = Flags ^ mfvdf_constness,

    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        % XXX We used to call mlds_output_extern_or_static
        % on mlds_data_decl_flags. This predicate pays attention to PerInstance
        % *only* when the access flag is acc_local, while field var's
        % access flags were always acc_public (which is why we don't need
        % to explicitly store that flag).
        PerInstance = Flags ^ mfvdf_per_instance,
        mlds_output_per_instance_comment(PerInstance, !IO)
    ;
        Comments = no
    ),
    mlds_output_field_var_extern(DeclOrDefn, !IO),
    mlds_output_constness(Constness, !IO).

:- pred mlds_output_function_decl_flags(mlds_to_c_opts::in,
    mlds_function_decl_flags::in, decl_or_defn::in, mlds_function_body::in,
    io::di, io::uo) is det.

mlds_output_function_decl_flags(Opts, Flags, DeclOrDefn, MaybeBody, !IO) :-
    Flags = mlds_function_decl_flags(Access, PerInstance),
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        mlds_output_access_comment(Access, !IO),
        mlds_output_per_instance_comment(PerInstance, !IO)
    ;
        Comments = no
    ),
    (
        MaybeBody = body_defined_here(_),
        DefnKind = dk_func_not_external
    ;
        MaybeBody = body_external,
        DefnKind = dk_func_external
    ),
    mlds_output_extern_or_static(Access, PerInstance, DeclOrDefn, DefnKind,
        !IO).

:- pred mlds_output_class_decl_flags(mlds_to_c_opts::in,
    mlds_class_decl_flags::in, decl_or_defn::in, io::di, io::uo) is det.

mlds_output_class_decl_flags(Opts, Flags, _DeclOrDefn, !IO) :-
    Flags = mlds_class_decl_flags(Access, Overridability, Constness),
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        mlds_output_class_access_comment(Access, !IO),
        mlds_output_per_instance_comment(one_copy, !IO)
    ;
        Comments = no
    ),
    mlds_output_overridability(Overridability, !IO),
    mlds_output_constness(Constness, !IO).

:- pred mlds_output_access_comment(function_access::in, io::di, io::uo) is det.

mlds_output_access_comment(func_public, !IO) :-
    io.write_string("/* public: */ ", !IO).
mlds_output_access_comment(func_private, !IO) :-
    io.write_string("/* private: */ ", !IO).
mlds_output_access_comment(func_local, !IO) :-
    io.write_string("/* local: */ ", !IO).

:- pred mlds_output_class_access_comment(class_access::in,
    io::di, io::uo) is det.

mlds_output_class_access_comment(class_public, !IO) :-
    io.write_string("/* public: */ ", !IO).
mlds_output_class_access_comment(class_private, !IO) :-
    io.write_string("/* private: */ ", !IO).

:- pred mlds_output_per_instance_comment(per_instance::in, io::di, io::uo)
    is det.

mlds_output_per_instance_comment(per_instance, !IO).
mlds_output_per_instance_comment(one_copy, !IO) :-
    io.write_string("/* one_copy */ ", !IO).

:- type defn_kind
    --->    dk_func_not_external
    ;       dk_func_external
    ;       dk_type.

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

    % mlds_output_field_var_extern does for field variables
    % what mlds_output_extern_or_static does for other entities.
    %
:- pred mlds_output_field_var_extern(decl_or_defn::in, io::di, io::uo) is det.

mlds_output_field_var_extern(DeclOrDefn, !IO) :-
    (
        DeclOrDefn = forward_decl,
        io.write_string("extern ", !IO)
    ;
        DeclOrDefn = definition
        % Print no storage class.
    ).

    % mlds_output_extern_or_static handles both the `access' and the
    % `per_instance' fields of the mlds_decl_flags. We have to handle them
    % together because C overloads `static' to mean both `private' and
    % `one_copy', rather than having separate keywords for each. To make it
    % clear which MLDS construct each `static' keyword means, our caller
    % should precede the call to this predicate with (optionally-enabled)
    % comments saying whether it is `private', `one_copy', or both.
    %
:- pred mlds_output_extern_or_static(function_access::in, per_instance::in,
    decl_or_defn::in, defn_kind::in, io::di, io::uo) is det.

mlds_output_extern_or_static(Access, PerInstance, DeclOrDefn, DefnKind, !IO) :-
    % XXX MLDS_DEFN This would be clearer as a nested switch
    % on DefnKind, DeclOrDefn and then Access and PerInstance.
    ( if
        (
            Access = func_private
        ;
            Access = func_local,
            PerInstance = one_copy
        ),
        % Don't output "static" on types.
        % Don't output "static" for functions that don't have a body,
        % which can happen for Mercury procedures that have a
        % `:- pragma external_{pred/func}'
        % Non-external functions (and global variables, which don't use
        % this predicate) are the only two kinds of definitions we *can* put
        % a "static" in front of.
        DefnKind = dk_func_not_external
    then
        io.write_string("static ", !IO)
    else if
        % Forward declarations for GNU C nested functions need to be prefixed
        % with "auto".
        DeclOrDefn = forward_decl,
        ( DefnKind = dk_func_not_external
        ; DefnKind = dk_func_external
        ),
        Access = func_local
    then
        io.write_string("auto ", !IO)
    else
        true
    ).

:- pred mlds_output_overridability(overridability::in, io::di, io::uo) is det.

mlds_output_overridability(sealed, !IO) :-
    io.write_string("/* sealed */ ", !IO).
mlds_output_overridability(overridable, !IO).

:- pred mlds_output_constness(constness::in, io::di, io::uo) is det.

mlds_output_constness(const, !IO) :-
    io.write_string("const ", !IO).
mlds_output_constness(modifiable, !IO).

%---------------------------------------------------------------------------%
%
% Code to output statements.
%

:- type func_info_c
    --->    func_info_c(qual_function_name, mlds_func_signature).

:- pred mlds_output_statements(mlds_to_c_opts::in, indent::in, func_info_c::in,
    list(mlds_stmt)::in, io::di, io::uo) is det.

mlds_output_statements(_Opts, _Indent, _FuncInfo, [], !IO).
mlds_output_statements(Opts, Indent, FuncInfo, [Stmt | Stmts], !IO) :-
    mlds_output_statement(Opts, Indent, FuncInfo, Stmt, !IO),
    mlds_output_statements(Opts, Indent, FuncInfo, Stmts, !IO).

:- pred mlds_output_statement(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in, io::di, io::uo) is det.

mlds_output_statement(Opts, Indent, FuncInfo, Stmt, !IO) :-
    c_output_stmt_context(Opts ^ m2co_line_numbers, Stmt, !IO),
    (
        Stmt = ml_stmt_block(_LocalVarDefns, _FuncDefns, _SubStmts, _Context),
        mlds_output_stmt_block(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_while(_Kind, _Cond, _BodyStmt, _LoopLocalVars, _Context),
        mlds_output_stmt_while(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_if_then_else(_Cond, _Then, _MaybeElse, _Context),
        mlds_output_stmt_if_then_else(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_switch(_Type, _Val, _Range, _Cases, _Default, _Context),
        mlds_output_stmt_switch(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_label(_LabelName, _Context),
        mlds_output_stmt_label(Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_goto(_Target, _Context),
        mlds_output_stmt_goto(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_computed_goto(_Expr, _Labels, _Context),
        mlds_output_stmt_computed_goto(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_call(_Signature, _FuncRval, _CallArgs,
            _Results, _IsTailCall, _Context),
        mlds_output_stmt_call(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_return(_Results, _Context),
        mlds_output_stmt_return(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_do_commit(_Ref, _Context),
        mlds_output_stmt_do_commit(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_try_commit(_Ref, _BodyStmt0, _HandlerStmt, _Context),
        mlds_output_stmt_try_commit(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_atomic(_AtomicStmt, _Context),
        mlds_output_stmt_atomic(Opts, Indent, Stmt, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output blocks.
%

:- pred mlds_output_stmt_block(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_block), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_block/6).

mlds_output_stmt_block(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context),
    BraceIndent = Indent,
    BlockIndent = Indent + 1,
    output_n_indents(BraceIndent, !IO),
    io.write_string("{\n", !IO),

    FuncInfo = func_info_c(FuncName, _),
    FuncName = qual_function_name(ModuleName, _),

    % Output forward declarations for any nested functions defined in
    % this block, in case they are referenced before they are defined.
    (
        FuncDefns = [_ | _],
        list.foldl(
            mlds_output_function_decl_opts(Opts, BlockIndent, ModuleName),
            FuncDefns, !IO),
        io.write_string("\n", !IO)
    ;
        FuncDefns = []
    ),
    (
        LocalVarDefns = [_ | _],
        mlds_output_local_var_defns(Opts, BlockIndent, no, LocalVarDefns, !IO),
        io.write_string("\n", !IO)
    ;
        LocalVarDefns = []
    ),
    (
        FuncDefns = [_ | _],
        mlds_output_function_defns(Opts, BlockIndent, ModuleName,
            FuncDefns, !IO),
        io.write_string("\n", !IO)
    ;
        FuncDefns = []
    ),
    mlds_output_statements(Opts, BlockIndent, FuncInfo, SubStmts, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(BraceIndent, !IO),
    io.write_string("}\n", !IO).

%---------------------------------------------------------------------------%
%
% Output while loops.
%

:- pred mlds_output_stmt_while(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_while), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_while/6).

mlds_output_stmt_while(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_while(Kind, Cond, BodyStmt, _LoopLocalVars, Context),
    scope_indent(BodyStmt, Indent, ScopeIndent),
    BodyOpts = Opts ^ m2co_break_context := bc_loop,
    (
        Kind = may_loop_zero_times,
        output_n_indents(Indent, !IO),
        io.write_string("while (", !IO),
        mlds_output_rval(Opts, Cond, !IO),
        io.write_string(")\n", !IO),
        mlds_output_statement(BodyOpts, ScopeIndent, FuncInfo, BodyStmt, !IO)
    ;
        Kind = loop_at_least_once,
        output_n_indents(Indent, !IO),
        io.write_string("do\n", !IO),
        mlds_output_statement(BodyOpts, ScopeIndent, FuncInfo, BodyStmt, !IO),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("while (", !IO),
        mlds_output_rval(Opts, Cond, !IO),
        io.write_string(");\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output if-then-elses.
%

:- pred mlds_output_stmt_if_then_else(mlds_to_c_opts::in, indent::in,
    func_info_c::in, mlds_stmt::in(ml_stmt_is_if_then_else),
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_if_then_else/6).

mlds_output_stmt_if_then_else(Opts, Indent, FuncInfo, Stmt, !IO) :-
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
        % we need braces around the inner `if', otherwise they wouldn't parse
        % they way we want them to: C would match the `else' with the
        % inner `if' rather than the outer `if'.

        MaybeElse = yes(_),
        Then0 = ml_stmt_if_then_else(_, _, no, ThenContext)
    then
        Then = ml_stmt_block([], [], [Then0], ThenContext)
    else if
        % For examples of the form
        %
        %   if (...)
        %       if (...)
        %           ...
        %       else
        %           ...
        %
        % we don't _need_ braces around the inner `if', since C will match
        % the else with the inner `if', but we add braces anyway, to avoid
        % a warning from gcc.

        MaybeElse = no,
        Then0 = ml_stmt_if_then_else(_, _, yes(_), ThenContext)
    then
        Then = ml_stmt_block([], [], [Then0], ThenContext)
    else
        Then = Then0
    ),

    output_n_indents(Indent, !IO),
    io.write_string("if (", !IO),
    mlds_output_rval(Opts, Cond, !IO),
    io.write_string(")\n", !IO),
    scope_indent(Then, Indent, ScopeIndent),
    mlds_output_statement(Opts, ScopeIndent, FuncInfo, Then, !IO),
    (
        MaybeElse = yes(Else),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("else\n", !IO),
        ( if Else = ml_stmt_if_then_else(_, _, _, _) then
            % Indent each if-then-else in a if-then-else chain
            % to the same depth.
            ElseScopeIndent = Indent
        else
            scope_indent(Else, Indent, ElseScopeIndent)
        ),
        mlds_output_statement(Opts, ElseScopeIndent, FuncInfo, Else, !IO)
    ;
        MaybeElse = no
    ).

%---------------------------------------------------------------------------%
%
% Output switch statements.
%

:- pred mlds_output_stmt_switch(mlds_to_c_opts::in, indent::in,
    func_info_c::in, mlds_stmt::in(ml_stmt_is_switch), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_switch/6).

mlds_output_stmt_switch(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default, Context),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("switch (", !IO),
    mlds_output_rval(Opts, Val, !IO),
    io.write_string(") {\n", !IO),
    CaseOpts = Opts ^ m2co_break_context := bc_switch,
    % We put the default case first, so that if it is unreachable,
    % it will get merged in with the first case.
    mlds_output_switch_default(CaseOpts, Indent + 1, FuncInfo, Context,
        Default, !IO),
    list.foldl(
        mlds_output_switch_case(CaseOpts, Indent + 1, FuncInfo, Context),
        Cases, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_switch_case(mlds_to_c_opts::in, indent::in,
    func_info_c::in, prog_context::in, mlds_switch_case::in,
    io::di, io::uo) is det.

mlds_output_switch_case(Opts, Indent, FuncInfo, Context, Case, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt),
    mlds_output_case_cond(Opts, Indent, Context, FirstCond, !IO),
    list.foldl(mlds_output_case_cond(Opts, Indent, Context), LaterConds, !IO),
    mlds_output_statement(Opts, Indent + 1, FuncInfo, Stmt, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("break;\n", !IO).

:- pred mlds_output_case_cond(mlds_to_c_opts::in, indent::in, prog_context::in,
    mlds_case_match_cond::in, io::di, io::uo) is det.

mlds_output_case_cond(Opts, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("case ", !IO),
        mlds_output_rval(Opts, Val, !IO),
        io.write_string(":\n", !IO)
    ;
        Match = match_range(Low, High),
        % This uses the GNU C extension `case <Low> ... <High>:'.
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("case ", !IO),
        mlds_output_rval(Opts, Low, !IO),
        io.write_string(" ... ", !IO),
        mlds_output_rval(Opts, High, !IO),
        io.write_string(":\n", !IO)
    ).

:- pred mlds_output_switch_default(mlds_to_c_opts::in, indent::in,
    func_info_c::in, prog_context::in, mlds_switch_default::in, io::di, io::uo)
    is det.

mlds_output_switch_default(Opts, Indent, FuncInfo, Context, Default, !IO) :-
    (
        Default = default_is_unreachable,
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("default: /*NOTREACHED*/ MR_assert(0);\n", !IO)
    ;
        Default = default_do_nothing
    ;
        Default = default_case(Stmt),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("default:\n", !IO),
        mlds_output_statement(Opts, Indent + 1, FuncInfo, Stmt, !IO),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        io.write_string("break;\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output labels.
%

:- pred mlds_output_stmt_label(indent::in, mlds_stmt::in(ml_stmt_is_label),
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_label/4).

mlds_output_stmt_label(Indent, Stmt, !IO) :-
    Stmt = ml_stmt_label(LabelName, _Context),
    % Note: MLDS allows labels at the end of blocks. C doesn't.
    % Hence we need to insert a semi-colon after the colon to ensure that
    % there is a statement to attach the label to.

    output_n_indents(Indent - 1, !IO),
    mlds_output_label_name(LabelName, !IO),
    io.write_string(":;\n", !IO).

:- pred mlds_output_label_name(mlds_label::in, io::di, io::uo) is det.

mlds_output_label_name(LabelName, !IO) :-
    mlds_output_mangled_name(LabelName, !IO).

%---------------------------------------------------------------------------%
%
% Output gotos.
%

:- pred mlds_output_stmt_goto(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_goto), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_goto/5).

mlds_output_stmt_goto(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_goto(Target, _Context),
    output_n_indents(Indent, !IO),
    (
        Target = goto_label(LabelName),
        io.write_string("goto ", !IO),
        mlds_output_label_name(LabelName, !IO),
        io.write_string(";\n", !IO)
    ;
        Target = goto_break_switch,
        BreakContext = Opts ^ m2co_break_context,
        (
            BreakContext = bc_switch,
            io.write_string("break;\n", !IO)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_loop
            ),
            unexpected($pred, "goto_break_switch not in switch")
        )
    ;
        Target = goto_break_loop,
        BreakContext = Opts ^ m2co_break_context,
        (
            BreakContext = bc_loop,
            io.write_string("break;\n", !IO)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_switch
            ),
            unexpected($pred, "goto_break_loop not in loop")
        )
    ;
        Target = goto_continue_loop,
        io.write_string("continue;\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output computed gotos.
%

:- pred mlds_output_stmt_computed_goto(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_computed_goto), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_computed_goto/5).

mlds_output_stmt_computed_goto(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_computed_goto(Expr, Labels, Context),
    % XXX For GNU C, we could output potentially more efficient code
    % by using an array of labels; this would tell the compiler that
    % it didn't need to do any range check.
    output_n_indents(Indent, !IO),
    io.write_string("switch (", !IO),
    mlds_output_rval(Opts, Expr, !IO),
    io.write_string(") {\n", !IO),
    list.foldl2(mlds_output_computed_goto_label(Opts, Context, Indent),
        Labels, 0, _FinalCount, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("default: /*NOTREACHED*/ MR_assert(0);\n", !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_computed_goto_label(mlds_to_c_opts::in, prog_context::in,
    int::in, mlds_label::in, int::in, int::out, io::di, io::uo) is det.

mlds_output_computed_goto_label(Opts, Context, Indent, Label, Count0, Count,
        !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("case ", !IO),
    io.write_int(Count0, !IO),
    io.write_string(": goto ", !IO),
    mlds_output_label_name(Label, !IO),
    io.write_string(";\n", !IO),
    Count = Count0 + 1.

%---------------------------------------------------------------------------%
%
% Output calls.
%

:- pred mlds_output_stmt_call(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_call), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_call/6).

mlds_output_stmt_call(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_call(Signature, FuncRval, CallArgs, Results,
        IsTailCall, Context),
    FuncInfo = func_info_c(CallerName, CallerSignature),

    % We need to ensure that we generate a single C statement here,
    % in case the generated code is e.g. one arm of an if-then-else.
    %
    % If we need to put profiling code before or after the call,
    % or if we want to put a return statement after the call, we must
    % enclose them, and the call, inside an extra pair of curly braces.
    % However, in the common case where none of that is needed,
    % we don't want the extra clutter of an unnecessary pair of braces.

    ProfileCalls = Opts ^ m2co_profile_calls,
    ProfileTime = Opts ^ m2co_profile_time,
    CallHasReturn = find_out_if_call_has_return(IsTailCall, Results,
        Signature, CallerSignature),
    ( if
        ProfileCalls = no,
        ProfileTime = no,
        ( CallHasReturn = call_has_no_return
        ; CallHasReturn = call_has_return_expr_prefix
        )
    then
        mlds_output_call(Opts, Context, Indent, CallHasReturn,
            FuncRval, CallArgs, Results, !IO)
    else
        BodyIndent = Indent + 1,

        output_n_indents(Indent, !IO),
        io.write_string("{\n", !IO),
        (
            ProfileCalls = yes,
            mlds_output_call_profile_instr(Opts, Context, BodyIndent,
                FuncRval, CallerName, !IO)
        ;
            ProfileCalls = no
        ),
        mlds_output_call(Opts, Context, BodyIndent, CallHasReturn,
            FuncRval, CallArgs, Results, !IO),
        (
            CallHasReturn = call_has_return_stmt_suffix,
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
            output_n_indents(BodyIndent, !IO),
            io.write_string("return;\n", !IO)
        ;
            ( CallHasReturn = call_has_no_return
            ; CallHasReturn = call_has_return_expr_prefix
            ),
            (
                ProfileTime = yes,
                mlds_output_time_profile_instr(Opts, Context, BodyIndent,
                    CallerName, !IO)
            ;
                ProfileTime = no
            )
        ),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred mlds_output_call(mlds_to_c_opts::in, prog_context::in, indent::in,
    maybe_call_has_return::in, mlds_rval::in,
    list(mlds_rval)::in, list(mlds_lval)::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_call/9).

mlds_output_call(Opts, Context, Indent, CallHasReturn, FuncRval,
        CallArgs, Results, !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    (
        CallHasReturn = call_has_return_expr_prefix,
        io.write_string("return ", !IO)
    ;
        ( CallHasReturn = call_has_no_return
        ; CallHasReturn = call_has_return_stmt_suffix
        )
    ),
    (
        Results = []
    ;
        Results = [Lval],
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(" = ", !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Results, mlds_output_lval(Opts), !IO),
        io.write_string(" = ", !IO)
    ),
    mlds_output_bracketed_rval(Opts, FuncRval, !IO),
    io.write_string("(", !IO),
    io.write_list(CallArgs, ", ", mlds_output_rval(Opts), !IO),
    io.write_string(");\n", !IO).

:- type maybe_call_has_return
    --->    call_has_no_return
    ;       call_has_return_expr_prefix
    ;       call_has_return_stmt_suffix.

    % "Optimize" general tail calls by asking our caller to give hints
    % to the C compiler in the form of "return" prefixes on call expressions.
    % XXX This optimization should be disable-able.
    %
    % If Results = [], i.e. the function has `void' return type, then this
    % would result in code that is not legal ANSI C (although it _is_ legal
    % in GNU C and in C++), so for that case, we return
    % call_has_return_stmt_suffix to ask our caller to put the return
    % statement after the call.
    %
    % Note that it is only safe to add such a return statement if the
    % calling procedure has the same return types as the callee, or if
    % the calling procedure has no return value. (Calls where the types
    % are different can be marked as tail calls if they are known
    % to never return.)
    % XXX That should be "marked as no_return_calls".
    %
:- func find_out_if_call_has_return(ml_call_kind, list(mlds_lval),
    mlds_func_signature, mlds_func_signature) = maybe_call_has_return.

find_out_if_call_has_return(IsTailCall, Results,
        CalleeSignature, CallerSignature) = CallHasReturn :-
    ( if
        ( IsTailCall = tail_call
        ; IsTailCall = no_return_call
        )
    then
        CalleeSignature = mlds_func_signature(_, CalleeRetTypes),
        CallerSignature = mlds_func_signature(_, CallerRetTypes),
        ( if
            Results = [_ | _],
            CalleeRetTypes = CallerRetTypes
        then
            CallHasReturn = call_has_return_expr_prefix
        else if
            CallerRetTypes = []
        then
            CallHasReturn = call_has_return_stmt_suffix
        else
            CallHasReturn = call_has_no_return
        )
    else
        CallHasReturn = call_has_no_return
    ).

    % Output an instruction to record an arc in the call profile
    % between the callee and caller.
    %
:- pred mlds_output_call_profile_instr(mlds_to_c_opts::in,
    prog_context::in, indent::in, mlds_rval::in,
    qual_function_name::in, io::di, io::uo) is det.

mlds_output_call_profile_instr(Opts, Context, Indent,
        CalleeFuncRval, CallerName, !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("MR_prof_call_profile(", !IO),
    mlds_output_bracketed_rval(Opts, CalleeFuncRval, !IO),
    io.write_string(", ", !IO),
    mlds_output_fully_qualified_function_name(CallerName, !IO),
    io.write_string(");\n", !IO).

    % Output an instruction which informs the runtime which procedure
    % we are currently located in.
    %
:- pred mlds_output_time_profile_instr(mlds_to_c_opts::in,
    prog_context::in, indent::in, qual_function_name::in,
    io::di, io::uo) is det.

mlds_output_time_profile_instr(Opts, Context, Indent, QualFuncName, !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("MR_set_prof_current_proc(", !IO),
    mlds_output_fully_qualified_function_name(QualFuncName, !IO),
    io.write_string(");\n", !IO).

%---------------------------------------------------------------------------%
%
% Output returns.
%

:- pred mlds_output_stmt_return(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_return), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_return/5).

mlds_output_stmt_return(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_return(Results, _Context),
    output_n_indents(Indent, !IO),
    io.write_string("return", !IO),
    (
        Results = []
    ;
        Results = [Rval],
        io.write_char(' ', !IO),
        mlds_output_rval(Opts, Rval, !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Results, mlds_output_rval(Opts), !IO)
    ),
    io.write_string(";\n", !IO).

%---------------------------------------------------------------------------%
%
% Output commits.
%

:- pred mlds_output_stmt_do_commit(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_do_commit), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_do_commit/5).

mlds_output_stmt_do_commit(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_do_commit(Ref, _Context),
    output_n_indents(Indent, !IO),
    % Output "MR_builtin_longjmp(<Ref>, 1)". This is a macro that expands
    % to either the standard longjmp() or the GNU C's __builtin_longjmp().
    % Note that the second argument to GNU C's __builtin_longjmp()
    % *must* be `1'.
    io.write_string("MR_builtin_longjmp(", !IO),
    mlds_output_rval(Opts, Ref, !IO),
    io.write_string(", 1);\n", !IO).

%---------------------------------------------------------------------------%
%
% Output try commits.
%

:- pred mlds_output_stmt_try_commit(mlds_to_c_opts::in, indent::in,
    func_info_c::in, mlds_stmt::in(ml_stmt_is_try_commit),
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_try_commit/6).

mlds_output_stmt_try_commit(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_try_commit(Ref, BodyStmt0, HandlerStmt, Context),
    % Output the following:
    %
    %   if (MR_builtin_setjmp(<Ref>) == 0)
    %       <Stmt>
    %   else
    %       <Handler>
    %
    % MR_builtin_setjmp() expands to either the standard setjmp()
    % or GNU C's __builtin_setjmp().
    %
    % Note that ISO C says that any non-volatile variables that are local
    % to the function containing the setjmp() and which are modified between
    % the setjmp() and the longjmp() become indeterminate after the longjmp().
    % The MLDS code generator handles that by generating each commit
    % in its own nested function, with the local variables remaining
    % in the containing function. This ensures that none of the variables
    % which get modified between the setjmp() and the longjmp() and which get
    % referenced after the longjmp() are local variables in the function
    % containing the setjmp(), so we don't need to mark them as volatile.
    % XXX Soon there won't be any nested functions.

    % We need to take care to avoid problems caused by the
    % dangling else ambiguity.
    ( if BodyStmt0 = ml_stmt_if_then_else(_, _, no, Context) then
        BodyStmt = ml_stmt_block([], [], [BodyStmt0], Context)
    else
        BodyStmt = BodyStmt0
    ),

    output_n_indents(Indent, !IO),
    io.write_string("if (MR_builtin_setjmp(", !IO),
    mlds_output_lval(Opts, Ref, !IO),
    io.write_string(") == 0)\n", !IO),

    mlds_output_statement(Opts, Indent + 1, FuncInfo, BodyStmt, !IO),

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("else\n", !IO),

    mlds_output_statement(Opts, Indent + 1, FuncInfo, HandlerStmt, !IO).

%---------------------------------------------------------------------------%
%
% Output atomic statements.
%

:- pred mlds_output_stmt_atomic(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_atomic), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_atomic/5).

mlds_output_stmt_atomic(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_atomic(AtomicStmt, Context),
    (
        AtomicStmt = comment(Comment),
        ( if Comment = "" then
            io.nl(!IO)
        else
            CommentLines = split_at_separator(char.is_line_separator, Comment),
            write_comment_lines(Indent, CommentLines, !IO)
        )
    ;
        AtomicStmt = assign(Lval, Rval),
        output_n_indents(Indent, !IO),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(" = ", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(Lval, Rval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_assign_if_in_heap(", !IO),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = delete_object(Rval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_free_heap(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = new_object(_Target, _Ptag, _ExplicitSecTag, _Type,
            _MaybeSize, _MaybeCtorName, _ArgRvalsTypes, _MayUseAtomic,
            _MaybeAllocId),
        mlds_output_stmt_atomic_new_object(Opts, Indent, AtomicStmt, Context,
            !IO)
    ;
        AtomicStmt = gc_check,
        output_n_indents(Indent, !IO),
        io.write_string("MR_GC_check();\n", !IO)
    ;
        AtomicStmt = mark_hp(Lval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_mark_hp(", !IO),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = restore_hp(Rval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_restore_hp(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = trail_op(_TrailOp),
        sorry($pred, "trail_ops not implemented")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_c,
            list.foldl(mlds_output_target_code_component(Opts, Context),
                Components, !IO)
        ;
            ( TargetLang = ml_target_csharp
            ; TargetLang = ml_target_java
            ),
            sorry($pred, "inline_target_code only works for language C")
        )
    ;
        AtomicStmt = outline_foreign_proc(_Lang, _Vs, _Lvals, _Code),
        unexpected($pred, "outline_foreign_proc is not used in C backend")
    ).

:- pred write_comment_lines(int::in, list(string)::in, io::di, io::uo) is det.

write_comment_lines(_Indent, [], !IO).
write_comment_lines(Indent, [CommentLine | CommentLines], !IO) :-
    ( if CommentLine = "" then
        io.nl(!IO)
    else
        output_n_indents(Indent, !IO),
        io.write_string("// ", !IO),
        io.write_string(CommentLine, !IO),
        io.nl(!IO)
    ),
    write_comment_lines(Indent, CommentLines, !IO).

:- pred mlds_output_stmt_atomic_new_object(mlds_to_c_opts::in, indent::in,
    mlds_atomic_statement::in(atomic_stmt_is_new_object), prog_context::in,
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_atomic_new_object/6).

mlds_output_stmt_atomic_new_object(Opts, Indent, AtomicStmt, Context, !IO) :-
    AtomicStmt = new_object(Target, Ptag, _ExplicitSecTag, Type,
        MaybeSize, _MaybeCtorName, ArgRvalsTypes, MayUseAtomic, MaybeAllocId),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),

    % When filling in the fields of a newly allocated cell, use a fresh
    % local variable as the base address for the field references in
    % preference to an lval that is more expensive to access. This yields
    % a speedup of about 0.3%.

    ( if Target = ml_local_var(_, _) then
        Base = ls_lval(Target)
    else
        % It doesn't matter what string we pick for BaseVarName,
        % as long as its declaration doesn't hide any of the variables
        % inside ArgRvalsTypes. This is not hard to ensure, since the printed
        % forms of the variables inside ArgRvalsTypes all include "__".
        % XXX Actually, they don't include "__" anymore.
        BaseVarName = "base",
        Base = ls_string(BaseVarName),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        mlds_output_type_prefix(Opts, Type, !IO),
        io.write_string(" ", !IO),
        io.write_string(BaseVarName, !IO),
        mlds_output_type_suffix(Opts, Type, no_size, !IO),
        io.write_string(";\n", !IO)
    ),

    % For --gc accurate, we need to insert a call to GC_check()
    % before every allocation.
    GC_Method = Opts ^ m2co_gc_method,
    (
        GC_Method = gc_accurate,
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        io.write_string("MR_GC_check();\n", !IO),
        % For types which hold RTTI that will be traversed by the collector
        % at GC-time, we need to allocate an extra word at the start,
        % to hold the forwarding pointer. Normally we would just overwrite
        % the first word of the object in the "from" space, but this
        % can't be done for objects which will be referenced during
        % the garbage collection process.
        NeedsForwardingSpace = type_needs_forwarding_pointer_space(Type),
        (
            NeedsForwardingSpace = yes,
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
            output_n_indents(Indent + 1, !IO),
            io.write_string("// reserve space for GC forwarding pointer\n",
                !IO),
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
            output_n_indents(Indent + 1, !IO),
            io.write_string("MR_hp_alloc(1);\n", !IO)
        ;
            NeedsForwardingSpace = no
        )
    ;
        ( GC_Method = gc_none
        ; GC_Method = gc_boehm
        ; GC_Method = gc_boehm_debug
        ; GC_Method = gc_hgc
        ; GC_Method = gc_automatic
        )
    ),

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    write_lval_or_string(Opts, Base, !IO),
    io.write_string(" = ", !IO),
    ( if Ptag = 0 then
        % XXX We shouldn't need the cast here, but currently the type that
        % we include in the call to MR_new_object() is not always correct.
        mlds_output_cast(Opts, Type, !IO),
        EndMkword = ""
    else
        mlds_output_cast(Opts, Type, !IO),
        io.write_string("MR_mkword(", !IO),
        mlds_output_ptag(Ptag, !IO),
        io.write_string(", ", !IO),
        EndMkword = ")"
    ),
    (
        MayUseAtomic = may_not_use_atomic_alloc,
        io.write_string("MR_new_object(", !IO)
    ;
        MayUseAtomic = may_use_atomic_alloc,
        io.write_string("MR_new_object_atomic(", !IO)
    ),
    mlds_output_type(Opts, Type, !IO),
    io.write_string(", ", !IO),
    (
        MaybeSize = yes(Size),
        io.write_string("(", !IO),
        mlds_output_rval(Opts, Size, !IO),
        io.write_string(" * sizeof(MR_Word))", !IO)
    ;
        MaybeSize = no,
        % XXX what should we do here?
        io.write_int(-1, !IO)
    ),
    io.write_string(", ", !IO),
    mlds_output_maybe_alloc_id(MaybeAllocId, !IO),
    io.write_string(", NULL)", !IO),
    io.write_string(EndMkword, !IO),
    io.write_string(";\n", !IO),
    (
        Base = ls_lval(_)
    ;
        Base = ls_string(BaseVarName1),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        mlds_output_lval(Opts, Target, !IO),
        io.write_string(" = ", !IO),
        io.write_string(BaseVarName1, !IO),
        io.write_string(";\n", !IO)
    ),
    mlds_output_init_args(ArgRvalsTypes, Context, 0, Base, Ptag,
        Opts, Indent + 1, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_maybe_alloc_id(maybe(mlds_alloc_id)::in, io::di, io::uo)
    is det.

mlds_output_maybe_alloc_id(MaybeAllocId, !IO) :-
    (
        MaybeAllocId = yes(mlds_alloc_id(Num)),
        io.format("&MR_alloc_sites[%d]", [i(Num)], !IO)
    ;
        MaybeAllocId = no,
        io.write_string("NULL", !IO)
    ).

:- pred mlds_output_target_code_component(mlds_to_c_opts::in, prog_context::in,
    target_code_component::in, io::di, io::uo) is det.

mlds_output_target_code_component(Opts, Context, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        (
            MaybeUserContext = yes(UserContext),
            c_output_context(Opts ^ m2co_line_numbers, UserContext, !IO)
        ;
            MaybeUserContext = no,
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO)
        ),
        io.write_string(CodeString, !IO),
        io.write_string("\n", !IO),
        c_reset_context(Opts ^ m2co_line_numbers, !IO)
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(" ", !IO)
    ;
        TargetCode = target_code_output(Lval),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(" ", !IO)
    ;
        TargetCode = target_code_type(Type),
        mlds_output_type(Opts, Type, !IO),
        io.write_string(" ", !IO)
    ;
        % Note: `target_code_name(Name)' target_code_components are used to
        % generate the #define for `MR_PROC_LABEL'.
        % The fact that they are used in a #define means that we can't do
        % an output_context(Context) here, since #line directives
        % aren't allowed inside #defines.
        % Similarly, all the target_code_components except user_target_code
        % can get emitted inside calls to the MR_BOX_FOREIGN_TYPE
        % or MR_UNBOX_FOREIGN_TYPE macros, which means that we can't output
        % the contexts for those either, since #line directives aren't
        % allowed inside macro invocations in standard C
        % (although some compilers, e.g. gcc 3.2, do allow it).

        TargetCode = target_code_function_name(FuncName),
        mlds_output_fully_qualified_function_name(FuncName, !IO),
        io.write_string("\n", !IO)
    ;
        TargetCode = target_code_alloc_id(AllocId),
        mlds_output_maybe_alloc_id(yes(AllocId), !IO)
    ).

:- func type_needs_forwarding_pointer_space(mlds_type) = bool.

type_needs_forwarding_pointer_space(Type) = NeedsForwardingPtrSpace :-
    (
        ( Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ),
        NeedsForwardingPtrSpace = yes
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_native_bool_type
        ; Type = mlds_native_int_type
        ; Type = mlds_native_uint_type
        ; Type = mlds_native_float_type
        ; Type = mlds_native_char_type
        ; Type = mlds_foreign_type(_)
        ; Type = mlds_class_type(_)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ),
        NeedsForwardingPtrSpace = no
    ;
        Type = mercury_type(_, _, TypeCategory),
        NeedsForwardingPtrSpace =
            is_introduced_type_info_type_category(TypeCategory)
    ;
        Type = mlds_rtti_type(_),
        % These should all be statically allocated, not dynamically allocated,
        % so we should never get here.
        unexpected($pred, "rtti_type")
    ;
        Type = mlds_tabling_type(_),
        % These should all be statically allocated, not dynamically allocated,
        % so we should never get here.
        unexpected($pred, "tabling_type")
    ;
        Type = mlds_unknown_type,
        unexpected($pred, "unknown_type")
    ).

:- type lval_or_string
    --->    ls_lval(mlds_lval)
    ;       ls_string(string).

:- pred mlds_output_init_args(list(mlds_typed_rval)::in,
    prog_context::in, int::in, lval_or_string::in, ptag::in,
    mlds_to_c_opts::in, indent::in, io::di, io::uo) is det.

mlds_output_init_args([], _, _, _, _, _, _, !IO).
mlds_output_init_args([ArgRvalType | ArgRvalsTypes], Context,
        ArgNum, Base, Ptag, Opts, Indent, !IO) :-
    % The MR_hl_field() macro expects its argument to have type MR_Box,
    % so we need to box the arguments if they aren't already boxed.
    % Hence the use of mlds_output_boxed_rval below.

    % XXX For --high-level-data, we ought to generate assignments to the fields
    % (or perhaps a call to a constructor function) rather than using the
    % MR_hl_field() macro.

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("MR_hl_field(", !IO),
    mlds_output_ptag(Ptag, !IO),
    io.write_string(", ", !IO),
    write_lval_or_string(Opts, Base, !IO),
    io.write_string(", ", !IO),
    io.write_int(ArgNum, !IO),
    io.write_string(") = ", !IO),
    ArgRvalType = ml_typed_rval(ArgRval, ArgType),
    mlds_output_boxed_rval(Opts, ArgType, ArgRval, !IO),
    io.write_string(";\n", !IO),
    mlds_output_init_args(ArgRvalsTypes, Context,
        ArgNum + 1, Base, Ptag, Opts, Indent, !IO).

:- pred write_lval_or_string(mlds_to_c_opts::in, lval_or_string::in,
    io::di, io::uo) is det.

write_lval_or_string(Opts, Base, !IO) :-
    (
        Base = ls_lval(Target),
        mlds_output_lval(Opts, Target, !IO)
    ;
        Base = ls_string(BaseVarName),
        io.write_string(BaseVarName, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Code to output expressions.
%

:- pred mlds_output_lval(mlds_to_c_opts::in, mlds_lval::in, io::di, io::uo)
    is det.

mlds_output_lval(Opts, Lval, !IO) :-
    (
        Lval = ml_field(MaybePtag, PtrRval, FieldId, FieldType, PtrType),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                (
                    FieldType = mlds_generic_type
                ;
                    FieldType = mercury_type(MercuryType, _, _),
                    MercuryType = type_variable(_, _)
                    % We could also accept other types that are the same size
                    % as MR_Box, such as builtin_type(builtin_type_int) and
                    % builtin_type(builtin_type_string).
                )
            then
                io.write_string("(", !IO),
                (
                    MaybePtag = yes(Ptag),
                    io.write_string("MR_hl_field(", !IO),
                    mlds_output_ptag(Ptag, !IO),
                    io.write_string(", ", !IO)
                ;
                    MaybePtag = no,
                    io.write_string("MR_hl_mask_field(", !IO),
                    io.write_string("(MR_Word) ", !IO)
                ),
                mlds_output_rval(Opts, PtrRval, !IO),
                io.write_string(", ", !IO),
                mlds_output_rval(Opts, OffsetRval, !IO),
                io.write_string("))", !IO)
            else
                % The field type for ml_lval_field(_, _, ml_field_offset(_),
                % _, _) lvals must be something that maps to MR_Box.
                unexpected($pred, "unexpected field type")
            )
        ;
            FieldId = ml_field_named(QualFieldVarName, CtorType),
            io.write_string("(", !IO),
            ( if MaybePtag = yes(0) then
                ( if PtrType = CtorType then
                    true
                else
                    mlds_output_cast(Opts, CtorType, !IO)
                ),
                ( if PtrRval = ml_mem_addr(PtrAddrLval) then
                    mlds_output_lval(Opts, PtrAddrLval, !IO),
                    io.write_string(").", !IO)
                else
                    mlds_output_bracketed_rval(Opts, PtrRval, !IO),
                    io.write_string(")->", !IO)
                )
            else
                mlds_output_cast(Opts, CtorType, !IO),
                (
                    MaybePtag = yes(Ptag),
                    io.write_string("MR_body(", !IO),
                    mlds_output_rval(Opts, PtrRval, !IO),
                    io.write_string(", ", !IO),
                    mlds_output_ptag(Ptag, !IO)
                ;
                    MaybePtag = no,
                    io.write_string("MR_strip_tag(", !IO),
                    mlds_output_rval(Opts, PtrRval, !IO)
                ),
                io.write_string("))->", !IO)
            ),
            mlds_output_fully_qualified_field_var_name(QualFieldVarName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        io.write_string("*", !IO),
        mlds_output_bracketed_rval(Opts, Rval, !IO)
    ;
        Lval = ml_target_global_var_ref(GlobalVar),
        io.write_string(global_var_name(GlobalVar), !IO)
    ;
        Lval = ml_global_var(QualGlobalVarName, _VarType),
        QualGlobalVarName =
            qual_global_var_name(MLDS_ModuleName, GlobalVarName),
        mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName,
            GlobalVarName, !IO)
    ;
        Lval = ml_local_var(LocalVarName, _VarType),
        mlds_output_local_var_name(LocalVarName, !IO)
    ).

:- func global_var_name(global_var_ref) = string.

% The calls to env_var_is_acceptable_char in parse_goal.m ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c
% and c_global_var_name in llds_out.m.
global_var_name(env_var_ref(EnvVarName)) = "mercury_envvar_" ++ EnvVarName.

:- pred mlds_output_mangled_name(string::in, io::di, io::uo) is det.

mlds_output_mangled_name(Name, !IO) :-
    io.write_string(name_mangle(Name), !IO).

:- pred mlds_output_bracketed_rval(mlds_to_c_opts::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_bracketed_rval(Opts, Rval, !IO) :-
    ( if
        % If it's just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_local_var(_,_))
        ; Rval = ml_lval(ml_global_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        mlds_output_rval(Opts, Rval, !IO)
    else
        io.write_char('(', !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_char(')', !IO)
    ).

    % mlds_output_return_list(List, OutputPred, IO0, IO) outputs a List
    % of return types/values using OutputPred.
    %
:- pred mlds_output_return_list(list(T)::in,
    pred(T, io, io)::in(pred(in, di, uo) is det),
    io::di, io::uo) is det.

mlds_output_return_list(List, OutputPred, !IO) :-
    % Even though C doesn't support multiple return types, this case needs
    % to be handled for e.g. MLDS dumps when compiling to Java. We generate
    % an "#error" directive to make the error message clearer, but then we go
    % ahead and generate C-like pseudo-code for the purposes of MLDS dumps.
    io.write_string("\n#error multiple return values\n", !IO),
    io.write_string("\t{", !IO),
    io.write_list(List, ", ", OutputPred, !IO),
    io.write_string("}", !IO).

:- pred mlds_output_rval_as_op_arg(mlds_to_c_opts::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_rval_as_op_arg(Opts, Rval, !IO) :-
    ( if
        ( Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        )
    then
        io.write_string("(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(")", !IO)
    else
        mlds_output_rval(Opts, Rval, !IO)
    ).

:- pred mlds_output_rval(mlds_to_c_opts::in, mlds_rval::in, io::di, io::uo)
    is det.

mlds_output_rval(Opts, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        mlds_output_lval(Opts, Lval, !IO)
        % XXX Do we need the commented out code below?
        % if a field is used as an rval, then we need to use
        % the MR_hl_const_field() macro, not the MR_hl_field() macro,
        % to avoid warnings about discarding const,
        % and similarly for MR_mask_field.
        %   ( if Lval = ml_lval_field(MaybePtag, Rval, FieldNum, _, _) then
        %       (
        %           MaybePtag = yes(Ptag),
        %           io.write_string("MR_hl_const_field(", !IO),
        %           mlds_output_ptag(Ptag, !IO),
        %           io.write_string(", ", !IO)
        %       ;
        %           MaybePtag = no,
        %           io.write_string("MR_hl_const_mask_field(", !IO)
        %       ),
        %       mlds_output_rval(Rval, !IO),
        %       io.write_string(", ", !IO),
        %       mlds_output_rval(FieldNum, !IO),
        %       io.write_string(")", !IO)
        %   else
        %       mlds_output_lval(Lval, !IO)
        %   ).
    ;
        Rval = ml_mkword(Ptag, BaseRval),
        io.write_string("MR_mkword(", !IO),
        mlds_output_ptag(Ptag, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Opts, BaseRval, !IO),
        io.write_string(")", !IO)
    ;
        Rval = ml_const(Const),
        mlds_output_rval_const(Opts, Const, !IO)
    ;
        Rval = ml_unop(UnOp, RvalA),
        mlds_output_unop(Opts, UnOp, RvalA, !IO)
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        mlds_output_binop(Opts, BinOp, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(Lval),
        % XXX Are parentheses needed?
        io.write_string("&", !IO),
        mlds_output_lval(Opts, Lval, !IO)
    ;
        (
            Rval = ml_scalar_common(ScalarCommon)
        ;
            Rval = ml_scalar_common_addr(ScalarCommon),
            io.write_string("&", !IO)
        ),
        ScalarCommon = ml_scalar_common(ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName = sym_name_mangle(ModuleSymName),
        io.format("%s_scalar_common_%d[%d]",
            [s(MangledModuleName), i(TypeNum), i(RowNum)], !IO)
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        VectorCommon = ml_vector_common(ModuleName, _Type,
            ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
        ModuleSymName = mlds_module_name_to_sym_name(ModuleName),
        MangledModuleName = sym_name_mangle(ModuleSymName),
        io.format("&%s_vector_common_%d[%d + ",
            [s(MangledModuleName), i(TypeNum), i(StartRowNum)], !IO),
        mlds_output_rval(Opts, RowRval, !IO),
        io.write_string("]", !IO)
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

:- pred mlds_output_unop(mlds_to_c_opts::in, mlds_unary_op::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_unop(Opts, Unop, Expr, !IO) :-
    (
        Unop = cast(Type),
        mlds_output_cast_rval(Opts, Type, Expr, !IO)
    ;
        Unop = box(Type),
        mlds_output_boxed_rval(Opts, Type, Expr, !IO)
    ;
        Unop = unbox(Type),
        mlds_output_unboxed_rval(Opts, Type, Expr, !IO)
    ;
        Unop = std_unop(StdUnop),
        mlds_output_std_unop(Opts, StdUnop, Expr, !IO)
    ).

:- pred mlds_output_cast_rval(mlds_to_c_opts::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_cast_rval(Opts, Type, Expr, !IO) :-
    mlds_output_cast(Opts, Type, !IO),
    ( if
        Opts ^ m2co_highlevel_data = yes,
        Expr = ml_const(mlconst_float(Float))
    then
        mlds_output_float_bits(Opts, Float, !IO)
    else
        mlds_output_rval(Opts, Expr, !IO)
    ).

:- pred mlds_output_cast(mlds_to_c_opts::in, mlds_type::in, io::di, io::uo)
    is det.

mlds_output_cast(Opts, Type, !IO) :-
    io.write_string("(", !IO),
    mlds_output_type(Opts, Type, !IO),
    io.write_string(") ", !IO).

    % Return `yes' if the specified rval is an address (possibly tagged and/or
    % cast to a different type).
    %
:- func is_an_address(mlds_rval) = bool.

is_an_address(Rval) = IsAddr :-
    (
        Rval = ml_mkword(_Ptag, SubRval),
        IsAddr = is_an_address(SubRval)
    ;
        Rval = ml_unop(UnOp, SubRval),
        ( if UnOp = cast(_) then
            IsAddr = is_an_address(SubRval)
        else
            IsAddr = no
        )
    ;
        ( Rval = ml_mem_addr(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_vector_common_row_addr(_, _)
        ),
        IsAddr = yes
    ;
        ( Rval = ml_lval(_)
        ; Rval = ml_binop(_, _, _)
        ; Rval = ml_self(_)
        ; Rval = ml_scalar_common(_)
        ),
        IsAddr = no
    ;
        Rval = ml_const(Const),
        (
            ( Const = mlconst_null(_)
            ; Const = mlconst_code_addr(_)
            ; Const = mlconst_data_addr_local_var(_)
            ; Const = mlconst_data_addr_global_var(_, _)
            ; Const = mlconst_data_addr_rtti(_, _)
            ; Const = mlconst_data_addr_tabling(_, _)
            ),
            IsAddr = yes
        ;
            ( Const = mlconst_false
            ; Const = mlconst_true
            ; Const = mlconst_named_const(_, _)
            ; Const = mlconst_enum(_, _)
            ; Const = mlconst_char(_)
            ; Const = mlconst_string(_)
            ; Const = mlconst_multi_string(_)
            ; Const = mlconst_int(_)
            ; Const = mlconst_uint(_)
            ; Const = mlconst_int8(_)
            ; Const = mlconst_uint8(_)
            ; Const = mlconst_int16(_)
            ; Const = mlconst_uint16(_)
            ; Const = mlconst_int32(_)
            ; Const = mlconst_uint32(_)
            ; Const = mlconst_int64(_)
            ; Const = mlconst_uint64(_)
            ; Const = mlconst_float(_)
            ; Const = mlconst_foreign(_, _, _)
            ),
            IsAddr = no
        )
    ).

%---------------------%

:- pred mlds_output_boxed_rval(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_boxed_rval(Opts, Type, Rval, !IO) :-
    ( if
        Rval = ml_unop(cast(OtherType), InnerRval),
        ( Type = OtherType
        ; is_an_address(InnerRval) = yes
        )
    then
        % Avoid unnecessary double-casting -- strip away the inner cast.
        % This is necessary for ANSI/ISO C conformance, to avoid casts
        % from pointers to integers in static initializers.
        mlds_output_boxed_rval(Opts, Type, InnerRval, !IO)
    else
        (
            Type = mlds_generic_type,
            mlds_output_boxed_rval_generic(Opts, Rval, !IO)
        ;
            Type = mlds_native_float_type,
            mlds_output_boxed_rval_float(Opts, Rval, !IO)
        ;
            ( Type = mlds_native_char_type
            ; Type = mlds_native_bool_type
            ; Type = mlds_native_int_type       % XXX ARG_PACK
            ),
            mlds_output_boxed_rval_smaller_than_word(Opts, Rval, !IO)
        ;
            ( Type = mlds_native_uint_type
            ; Type = mlds_array_type(_)
            ; Type = mlds_mercury_array_type(_)
            ; Type = mlds_mostly_generic_array_type(_)
            ; Type = mlds_class_type(_)
            ; Type = mlds_commit_type
            ; Type = mlds_cont_type(_)
            ; Type = mlds_foreign_type(_)
            ; Type = mlds_func_type(_)
            ; Type = mlds_generic_env_ptr_type
            ; Type = mlds_pseudo_type_info_type
            ; Type = mlds_ptr_type(_)
            ; Type = mlds_rtti_type(_)
            ; Type = mlds_tabling_type(_)
            ; Type = mlds_type_info_type
            ; Type = mlds_unknown_type
            ),
            mlds_output_boxed_rval_default(Opts, Rval, !IO)
        ;
            Type = mercury_type(MercuryType, _, _),
            (
                MercuryType = builtin_type(BuiltinType),
                (
                    BuiltinType = builtin_type_float,
                    mlds_output_boxed_rval_float(Opts, Rval, !IO)
                ;
                    BuiltinType = builtin_type_int(IntType),
                    (
                        IntType = int_type_int64,
                        mlds_output_boxed_rval_int64(Opts, Rval, !IO)
                    ;
                        IntType = int_type_uint64,
                        mlds_output_boxed_rval_uint64(Opts, Rval, !IO)
                    ;
                        ( IntType = int_type_int
                        ; IntType = int_type_uint
                        ),
                        mlds_output_boxed_rval_default(Opts, Rval, !IO)
                    ;
                        ( IntType = int_type_int8
                        ; IntType = int_type_uint8
                        ; IntType = int_type_int16
                        ; IntType = int_type_uint16
                        ; IntType = int_type_int32
                        ; IntType = int_type_uint32
                        ),
                        mlds_output_boxed_rval_smaller_than_word(Opts, Rval,
                            !IO)
                    )
                ;
                    BuiltinType = builtin_type_char,
                    mlds_output_boxed_rval_smaller_than_word(Opts, Rval, !IO)
                ;
                    BuiltinType = builtin_type_string,
                    mlds_output_boxed_rval_default(Opts, Rval, !IO)
                )
            ;
                MercuryType = type_variable(_, _),
                mlds_output_boxed_rval_generic(Opts, Rval, !IO)
            ;
                ( MercuryType = defined_type(_, _, _)
                ; MercuryType = tuple_type(_, _)
                ; MercuryType = higher_order_type(_, _, _, _, _)
                ; MercuryType = apply_n_type(_, _, _)
                ; MercuryType = kinded_type(_, _)
                ),
                mlds_output_boxed_rval_default(Opts, Rval, !IO)
            )
        )
    ).

:- pred mlds_output_boxed_rval_generic(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_generic/4).

mlds_output_boxed_rval_generic(Opts, Rval, !IO) :-
    % Rval already has type MR_Box, so no cast is needed.
    mlds_output_rval(Opts, Rval, !IO).

:- pred mlds_output_boxed_rval_float(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_float/4).

mlds_output_boxed_rval_float(Opts, Rval, !IO) :-
    ( if
        Rval = ml_const(mlconst_float(Float)),
        Opts ^ m2co_highlevel_data = yes
    then
        mlds_output_float_bits(Opts, Float, !IO)
    else
        io.write_string("MR_box_float(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(")", !IO)
    ).

:- pred mlds_output_boxed_rval_int64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_int64/4).

mlds_output_boxed_rval_int64(Opts, Rval, !IO) :-
    io.write_string("MR_box_int64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_boxed_rval_uint64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_uint64/4).

mlds_output_boxed_rval_uint64(Opts, Rval, !IO) :-
    io.write_string("MR_box_uint64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_boxed_rval_smaller_than_word(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_smaller_than_word/4).

mlds_output_boxed_rval_smaller_than_word(Opts, Rval, !IO) :-
    % We cast first to MR_Word, and then to MR_Box.
    % We do this to avoid spurious warnings from gcc about
    % "cast from integer to pointer of different size".
    io.write_string("((MR_Box) (MR_Word) (", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string("))", !IO).

:- pred mlds_output_boxed_rval_default(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_boxed_rval_default/4).

mlds_output_boxed_rval_default(Opts, Rval, !IO) :-
    io.write_string("((MR_Box) (", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string("))", !IO).

%---------------------%

:- pred mlds_output_unboxed_rval(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_unboxed_rval(Opts, Type, Rval, !IO) :-
    (
        Type = mlds_native_float_type,
        mlds_output_unboxed_rval_float(Opts, Rval, !IO)
    ;
        ( Type = mlds_native_char_type
        ; Type = mlds_native_bool_type
        ; Type = mlds_native_int_type   % XXX ARG_PACK
        ),
        mlds_output_unboxed_rval_smaller_than_word(Opts, Type, Rval, !IO)
    ;
        ( Type = mlds_native_uint_type
        ; Type = mlds_array_type(_)
        ; Type = mlds_mercury_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_class_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_cont_type(_)
        ; Type = mlds_foreign_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ; Type = mlds_type_info_type
        ; Type = mlds_unknown_type
        ),
        mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
    ;
        Type = mercury_type(MercuryType, _, _),
        (
            MercuryType = builtin_type(BuiltinType),
            (
                BuiltinType = builtin_type_float,
                mlds_output_unboxed_rval_float(Opts, Rval, !IO)
            ;
                BuiltinType = builtin_type_int(IntType),
                (
                    IntType = int_type_int64,
                    mlds_output_unboxed_rval_int64(Opts, Rval, !IO)
                ;
                    IntType = int_type_uint64,
                    mlds_output_unboxed_rval_uint64(Opts, Rval, !IO)
                ;
                    ( IntType = int_type_int
                    ; IntType = int_type_uint
                    ),
                    mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
                ;
                    % The following integer types are all (potentially) smaller
                    % than MR_Word.
                    ( IntType = int_type_int8
                    ; IntType = int_type_uint8
                    ; IntType = int_type_int16
                    ; IntType = int_type_uint16
                    ; IntType = int_type_int32
                    ; IntType = int_type_uint32
                    ),
                    mlds_output_unboxed_rval_smaller_than_word(Opts,
                        Type, Rval, !IO)
                )
            ;
                BuiltinType = builtin_type_char,
                mlds_output_unboxed_rval_smaller_than_word(Opts,
                    Type, Rval, !IO)
            ;
                BuiltinType = builtin_type_string,
                mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
            )
        ;
            ( MercuryType = type_variable(_, _)
            ; MercuryType = defined_type(_, _, _)
            ; MercuryType = tuple_type(_, _)
            ; MercuryType = higher_order_type(_, _, _, _, _)
            ; MercuryType = apply_n_type(_, _, _)
            ; MercuryType = kinded_type(_, _)
            ),
            mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO)
        )
    ).

:- pred mlds_output_unboxed_rval_float(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_float/4).

mlds_output_unboxed_rval_float(Opts, Rval, !IO) :-
    io.write_string("MR_unbox_float(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_int64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_int64/4).

mlds_output_unboxed_rval_int64(Opts, Rval, !IO) :-
    io.write_string("MR_unbox_int64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_uint64(mlds_to_c_opts::in,
    mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_uint64/4).

mlds_output_unboxed_rval_uint64(Opts, Rval, !IO) :-
    io.write_string("MR_unbox_uint64(", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_smaller_than_word(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_smaller_than_word/5).

mlds_output_unboxed_rval_smaller_than_word(Opts, Type, Rval, !IO) :-
    % We cast first to MR_Word, and then to the desired type.
    % This is done to avoid spurious warnings from gcc about
    % "cast from pointer to integer of different size".
    % XXX ARG_PACK We call this predicate both too much and not enough.
    % - We call it for builtin types that *may* be smaller than a word
    %   on some platforms, whether or not it is smaller than a word
    %   on *this* platform.
    % - We do *not* call it values of dummy and enum types, which *are*
    %   smaller than a word.
    io.write_string("(", !IO),
    mlds_output_cast(Opts, Type, !IO),
    io.write_string("(MR_Word) ", !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_unboxed_rval_default(mlds_to_c_opts::in,
    mlds_type::in, mlds_rval::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_unboxed_rval_default/5).

mlds_output_unboxed_rval_default(Opts, Type, Rval, !IO) :-
    io.write_string("(", !IO),
    mlds_output_cast(Opts, Type, !IO),
    mlds_output_rval(Opts, Rval, !IO),
    io.write_string(")", !IO).

%---------------------%

:- pred mlds_output_std_unop(mlds_to_c_opts::in, builtin_ops.unary_op::in,
    mlds_rval::in, io::di, io::uo) is det.

mlds_output_std_unop(Opts, UnaryOp, Expr, !IO) :-
    c_util.unary_prefix_op(UnaryOp, UnaryOpString),
    io.write_string(UnaryOpString, !IO),
    io.write_string("(", !IO),
    ( if UnaryOp = tag then
        % The MR_tag macro requires its argument to be of type `MR_Word'.
        % XXX Should we put this cast inside the definition of MR_tag?
        io.write_string("(MR_Word) ", !IO)
    else
        true
    ),
    mlds_output_rval(Opts, Expr, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_binop(mlds_to_c_opts::in, binary_op::in,
    mlds_rval::in, mlds_rval::in, io::di, io::uo) is det.

mlds_output_binop(Opts, Op, X, Y, !IO) :-
    (
        Op = array_index(_),
        mlds_output_bracketed_rval(Opts, X, !IO),
        io.write_string("[", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string("]", !IO)
    ;
        Op = string_unsafe_index_code_unit,
        io.write_string("MR_nth_code_unit(", !IO),
        mlds_output_bracketed_rval(Opts, X, !IO),
        io.write_string(", ", !IO),
        ( if Y = ml_const(mlconst_int(YN)) then
            io.write_int(YN, !IO)
        else
            mlds_output_rval(Opts, Y, !IO)
        ),
        io.write_string(")", !IO)
    ;
        ( Op = compound_lt
        ; Op = compound_eq
        ),
        % These operators are intended to be generated only when using
        % the Erlang backend.
        unexpected($pred, "compound_compare_binop")
    ;
        Op = pointer_equal_conservative,
        io.write_string("(((MR_Word) ", !IO),
        mlds_output_rval(Opts, X, !IO),
        io.write_string(") == ((MR_Word) ", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string("))", !IO)
    ;
        ( Op = str_eq, OpStr = "=="
        ; Op = str_ne, OpStr = "!="
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ),
        io.write_string("(strcmp(", !IO),
        mlds_output_rval(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string(")", !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        io.write_string("0)", !IO)
    ;
        ( Op = float_eq, OpStr = "=="
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
        io.write_string("(", !IO),
        mlds_output_bracketed_rval(Opts, X, !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        mlds_output_bracketed_rval(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = unsigned_le,
        io.write_string("(((MR_Unsigned) ", !IO),
        mlds_output_rval(Opts, X, !IO),
        io.write_string(") <= ((MR_Unsigned) ", !IO),
        mlds_output_rval(Opts, Y, !IO),
        io.write_string("))", !IO)
    ;
        ( Op = int_add(IntType), OpStr = "+"
        ; Op = int_sub(IntType), OpStr = "-"
        ; Op = int_mul(IntType), OpStr = "*"
        ),
        (
            (
                IntType = int_type_int,
                SignedType = "MR_Integer",
                UnsignedType = "MR_Unsigned"
            ;
                IntType = int_type_int8,
                SignedType = "int8_t",
                UnsignedType = "uint8_t"
            ;
                IntType = int_type_int16,
                SignedType = "int16_t",
                UnsignedType = "uint16_t"
            ;
                IntType = int_type_int32,
                SignedType = "int32_t",
                UnsignedType = "uint32_t"
            ;
                IntType = int_type_int64,
                SignedType = "int64_t",
                UnsignedType = "uint64_t"
            ),
            io.format("(%s) ((%s) ", [s(SignedType), s(UnsignedType)], !IO),
            mlds_output_rval_as_op_arg(Opts, X, !IO),
            io.format(" %s (%s) ", [s(OpStr), s(UnsignedType)], !IO),
            mlds_output_rval_as_op_arg(Opts, Y, !IO),
            io.write_string(")", !IO)
        ;
            ( IntType = int_type_uint
            ; IntType = int_type_uint8
            ; IntType = int_type_uint16
            ; IntType = int_type_uint32
            ; IntType = int_type_uint64
            ),
            % We could treat X + (-const) specially, but we don't.
            % The reason is documented in the equivalent code in
            % llds_out_data.m.
            io.write_string("(", !IO),
            mlds_output_rval_as_op_arg(Opts, X, !IO),
            io.format(" %s ", [s(OpStr)], !IO),
            mlds_output_rval_as_op_arg(Opts, Y, !IO),
            io.write_string(")", !IO)
        )
    ;
        ( Op = int_div(_), OpStr = "/"
        ; Op = int_mod(_), OpStr = "%"
        ; Op = eq(_), OpStr = "=="
        ; Op = ne(_), OpStr = "!="
        ; Op = int_lt(_), OpStr = "<"
        ; Op = int_gt(_), OpStr = ">"
        ; Op = int_le(_), OpStr = "<="
        ; Op = int_ge(_), OpStr = ">="
        ; Op = unchecked_left_shift(_), OpStr = "<<"
        ; Op = unchecked_right_shift(_), OpStr = ">>"
        ; Op = bitwise_and(_), OpStr = "&"
        ; Op = bitwise_or(_), OpStr = "|"
        ; Op = bitwise_xor(_), OpStr = "^"
        ; Op = logical_and, OpStr = "&&"
        ; Op = logical_or, OpStr = "||"
        ),
        % We could treat X + (-const) specially, but we don't.
        % The reason is documented in the equivalent code in llds_out_data.m.
        io.write_string("(", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = str_cmp,
        io.write_string("MR_strcmp(", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = offset_str_eq(N),
        io.write_string("MR_offset_streq(", !IO),
        io.write_int(N, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        Op = body,
        io.write_string("MR_body(", !IO),
        mlds_output_rval_as_op_arg(Opts, X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval_as_op_arg(Opts, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = float_from_dword,  OpStr = "MR_float_from_dword"
        ; Op = int64_from_dword,  OpStr = "MR_int64_from_dword"
        ; Op = uint64_from_dword, OpStr = "MR_uint64_from_dword"
        ),
        io.write_string(OpStr, !IO),
        ( if is_aligned_dword_field(X, Y, PtrRval) then
            % gcc produces faster code in this case.
            io.write_string("_ptr(MR_dword_ptr(", !IO),
            mlds_output_rval(Opts, PtrRval, !IO),
            io.write_string("))", !IO)
        else
            io.write_string("(", !IO),
            mlds_output_rval_as_op_arg(Opts, X, !IO),
            io.write_string(", ", !IO),
            mlds_output_rval_as_op_arg(Opts, Y, !IO),
            io.write_string(")", !IO)
        )
    ).

:- pred mlds_output_rval_const(mlds_to_c_opts::in, mlds_rval_const::in,
    io::di, io::uo) is det.

mlds_output_rval_const(_Opts, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string("MR_TRUE", !IO)
    ;
        Const = mlconst_false,
        io.write_string("MR_FALSE", !IO)
    ;
        ( Const = mlconst_int(N)
        ; Const = mlconst_enum(N, _)
        ),
        c_util.output_int_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint(U),
        c_util.output_uint_expr_cur_stream(U, !IO)
    ;
        Const = mlconst_int8(N),
        c_util.output_int8_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint8(N),
        c_util.output_uint8_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_int16(N),
        c_util.output_int16_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint16(N),
        c_util.output_uint16_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_int32(N),
        c_util.output_int32_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint32(N),
        c_util.output_uint32_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_int64(N),
        c_util.output_int64_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_uint64(N),
        c_util.output_uint64_expr_cur_stream(N, !IO)
    ;
        Const = mlconst_char(C),
        io.write_string("(MR_Char) ", !IO),
        io.write_int(C, !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_c), $pred,
            "mlconst_foreign for language other than C"),
        io.write_string("((int) ", !IO),
        io.write_string(Value, !IO),
        io.write_string(")", !IO)
    ;
        Const = mlconst_float(FloatVal),
        % The cast to (MR_Float) here lets the C compiler do arithmetic in
        % `float' rather than `double' if `MR_Float' is `float' not `double'.
        io.write_string("(MR_Float) ", !IO),
        c_util.output_float_literal_cur_stream(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        % The cast avoids the following gcc warning
        % "assignment discards qualifiers from pointer target type".
        io.write_string("(MR_String) ", !IO),
        io.write_string("""", !IO),
        c_util.output_quoted_string_cur_stream(String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_cur_stream(String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_named_const(_TargetPrefixes, NamedConst),
        % The target prefix for C is implicitly always empty.
        io.write_string(NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        mlds_output_code_addr(CodeAddr, !IO)
    ;
        Const = mlconst_data_addr_local_var(LocalVarName),
        io.write_string("&", !IO),
        mlds_output_mangled_name(ml_local_var_name_to_string(LocalVarName),
            !IO)
    ;
        (
            Const =
                mlconst_data_addr_global_var(MLDS_ModuleName, GlobalVarName),
            IsArray = not_array
        ;
            Const = mlconst_data_addr_rtti(MLDS_ModuleName, RttiId),
            GlobalVarName = gvn_rtti_var(RttiId),
            IsArray = rtti_id_has_array_type(RttiId)
        ;
            Const = mlconst_data_addr_tabling(QualProcLabel, TablingId),
            QualProcLabel = qual_proc_label(MLDS_ModuleName, ProcLabel),
            GlobalVarName = gvn_tabling_var(ProcLabel, TablingId),
            IsArray = tabling_id_has_array_type(TablingId)
        ),
        % If it is an array type, then we just use the name;
        % otherwise, we must prefix the name with `&'.
        (
            IsArray = is_array
        ;
            IsArray = not_array,
            io.write_string("&", !IO)
        ),
        mlds_output_maybe_qualified_global_var_name(MLDS_ModuleName,
            GlobalVarName, !IO)
    ;
        Const = mlconst_null(_),
        io.write_string("NULL", !IO)
    ).

    % Output the bit layout of a floating point literal as an integer, so that
    % it can be cast to a pointer type. We manage to avoid this in all but one
    % situation: in high-level data grades, when the program contains a ground
    % term, of which a sub-term is a no-tag wrapper around float.
    %
    % Technically we should avoid doing this when --cross-compiling is
    % enabled.
    %
    % XXX the problem is the field type in the C struct which is generated for
    % the type which has the no-tag argument. The generated field type is a
    % pointer to the struct for the no-tag type, yet the no-tag optimisation is
    % used, so the field type should either be the struct for the no-tag type
    % (not a pointer) or the type which the no-tag type wraps (which itself may
    % be a no-tag type, etc.)
    %
:- pred mlds_output_float_bits(mlds_to_c_opts::in, float::in, io::di, io::uo)
    is det.

mlds_output_float_bits(Opts, Float, !IO) :-
    expect(unify(Opts ^ m2co_highlevel_data, yes), $pred,
        "should only be required with --high-level-data"),
    SinglePrecFloat = Opts ^ m2co_single_prec_float,
    (
        SinglePrecFloat = yes,
        String = float32_bits_string(Float)
    ;
        SinglePrecFloat = no,
        String = float64_bits_string(Float)
    ),
    io.format("%s /* float-bits: %g */", [s(String), f(Float)], !IO).

:- pred is_aligned_dword_field(mlds_rval::in, mlds_rval::in, mlds_rval::out)
    is semidet.

is_aligned_dword_field(X, Y, ml_mem_addr(Lval)) :-
    X = ml_lval(ml_field(Ptag, Addr, FieldIdX, Type, PtrType) @ Lval),
    Y = ml_lval(ml_field(Ptag, Addr, FieldIdY, Type, PtrType)),
    FieldIdX = ml_field_offset(ml_const(mlconst_int(Offset))),
    FieldIdY = ml_field_offset(ml_const(mlconst_int(Offset + 1))),
    int.even(Offset).

%---------------------------------------------------------------------------%

:- pred mlds_output_ptag(ptag::in, io::di, io::uo) is det.

mlds_output_ptag(Ptag, !IO) :-
    io.write_string("MR_mktag(", !IO),
    io.write_int(Ptag, !IO),
    io.write_string(")", !IO).

%---------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds_code_addr::in, io::di, io::uo) is det.

mlds_output_code_addr(CodeAddr, !IO) :-
    CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
    QualFuncLabel = qual_func_label(ModuleName, FuncLabel),
    FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
    QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
    mlds_output_fully_qualified_proc_label(QualProcLabel, !IO),
    io.write_string(mlds_maybe_aux_func_id_to_suffix(MaybeAux), !IO).

:- pred mlds_output_proc_label(mlds_proc_label::in, io::di, io::uo) is det.

mlds_output_proc_label(mlds_proc_label(PredLabel, ProcId), !IO) :-
    mlds_output_pred_label(PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.write_char('_', !IO),
    io.write_int(ModeNum, !IO).

:- func mlds_proc_label_to_string(mlds_proc_label) = string.

mlds_proc_label_to_string(mlds_proc_label(PredLabel, ProcId)) =
    mlds_pred_label_to_string(PredLabel) ++ "_"
        ++ string.int_to_string(proc_id_to_int(ProcId)).

%---------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations (#line directives).
%

:- pred c_output_stmt_context(bool::in, mlds_stmt::in, io::di, io::uo) is det.

c_output_stmt_context(OutputLineNumbers, Stmt, !IO) :-
    (
        OutputLineNumbers = yes,
        Context = get_mlds_stmt_context(Stmt),
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        c_util.always_set_line_num_cur_stream(FileName, LineNumber, !IO)
    ;
        OutputLineNumbers = no
    ).

:- pred c_output_context(bool::in, prog_context::in, io::di, io::uo) is det.

c_output_context(OutputLineNumbers, Context, !IO) :-
    (
        OutputLineNumbers = yes,
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        c_util.always_set_line_num_cur_stream(FileName, LineNumber, !IO)
    ;
        OutputLineNumbers = no
    ).

:- pred c_output_file_line(bool::in, string::in, int::in,
    io::di, io::uo) is det.

c_output_file_line(OutputLineNumbers, FileName, LineNumber, !IO) :-
    (
        OutputLineNumbers = yes,
        c_util.always_set_line_num_cur_stream(FileName, LineNumber, !IO)
    ;
        OutputLineNumbers = no
    ).

:- pred c_reset_context(bool::in, io::di, io::uo) is det.

c_reset_context(OutputLineNumbers, !IO) :-
    (
        OutputLineNumbers = yes,
        c_util.always_reset_line_num_cur_stream(no, !IO)
    ;
        OutputLineNumbers = no
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c.
%---------------------------------------------------------------------------%
