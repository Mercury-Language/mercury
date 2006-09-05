%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%     (handle `user_foreign_code' and `foreign_code_decl' --
%     actually perhaps this should be done in an earlier pass,
%     in which case the only thing that would need to be done here
%     is to change some calls to sorry/2 to unexpected/2).
%   - packages, classes and inheritance
%     (currently we just generate all classes as structs)
%
%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c.
:- interface.

:- import_module ml_backend.mlds.
:- import_module backend_libs.rtti.

:- import_module io.

%-----------------------------------------------------------------------------%

    % output_mlds(MLDS, Suffix):
    %
    % Output C code the the appropriate C file and C declarations to the
    % appropriate header file. The file names are determined by the module
    % name, with the specified Suffix appended at the end. (The suffix is used
    % for debugging dumps. For normal output, the suffix should be the empty
    % string.)
    %
:- pred output_mlds(mlds::in, string::in, io::di, io::uo) is det.

    % output_header_file(MLDS, Suffix):
    %
    % Output C declarations for the procedures (etc.) in the specified MLDS
    % module to the appropriate .mih header file. See output_mlds for the
    % meaning of Suffix.
    %
:- pred output_header_file(mlds::in, string::in,
    io::di, io::uo) is det.

:- func mlds_tabling_data_name(mlds_proc_label, proc_tabling_struct_id)
    = string.

    % output_c_file(MLDS, Suffix):
    %
    % Output C code for the specified MLDS module to the appropriate C file.
    % See output_mlds for the meaning of Suffix.
    %
:- pred output_c_file(mlds::in, string::in, io::di, io::uo) is det.

    % Output an MLDS context in C #line format.
    % This is useful for other foreign language interfaces such as
    % managed extensions for C++.
    %
:- pred output_context(mlds_context::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.name_mangle.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.         % for pred_proc_id.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_code_util.
                                          % for ml_gen_public_field_decl_flags,
                                          % which is used by the code that
                                          % handles derived classes
:- import_module ml_backend.ml_type_gen. % for ml_gen_type_name
:- import_module ml_backend.ml_util.
:- import_module ml_backend.rtti_to_mlds.% for mlds_rtti_type_name.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type output_type == pred(mlds_type, io, io).
:- inst output_type == (pred(in, di, uo) is det).

%-----------------------------------------------------------------------------%

output_mlds(MLDS, Suffix, !IO) :-
    % We output the source file before outputting the header, since the Mmake
    % dependencies say the header file depends on the source file, and so if
    % we wrote them out in the other order, this might lead to unnecessary
    % recompilation next time Mmake is run.
    %
    % XXX At some point we should also handle output of any non-C
    % foreign code (Ada, Fortran, etc.) to appropriate files.
    %
    output_c_file(MLDS, Suffix, !IO),
    output_header_file(MLDS, Suffix, !IO).

output_c_file(MLDS, Suffix, !IO) :-
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(ModuleName, ".c" ++ Suffix, yes, SourceFile, !IO),
    Indent = 0,
    output_to_file(SourceFile, mlds_output_src_file(Indent, MLDS), !IO).

    % Generate the header file.
    %
output_header_file(MLDS, Suffix, !IO) :-
    % We write the header file out to <module>.mih.tmp and then call
    % `update_interface' to move the <module>.mih.tmp file to <module>.mih;
    % this avoids updating the timestamp on the `.mih' file if it hasn't
    % changed.

    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(ModuleName, ".mih" ++ Suffix ++ ".tmp", yes,
        TmpHeaderFile, !IO),
    module_name_to_file_name(ModuleName, ".mih" ++ Suffix, yes,
        HeaderFile, !IO),
    Indent = 0,
    output_to_file(TmpHeaderFile, mlds_output_hdr_file(Indent, MLDS), !IO),
    update_interface(HeaderFile, !IO).

:- pred mlds_output_hdr_file(indent::in, mlds::in, io::di, io::uo) is det.

mlds_output_hdr_file(Indent, MLDS, !IO) :-
    MLDS = mlds(ModuleName, AllForeignCode, Imports, Defns, InitPreds,
        FinalPreds),
    mlds_output_hdr_start(Indent, ModuleName, !IO),
    io.nl(!IO),
    mlds_output_hdr_imports(Indent, Imports, !IO),
    io.nl(!IO),

    % Get the foreign code for C
    ForeignCode = mlds_get_c_foreign_code(AllForeignCode),
    mlds_output_c_hdr_decls(MLDS_ModuleName, Indent, ForeignCode, !IO),
    io.nl(!IO),

    % The header file must contain _definitions_ of all public types, but only
    % _declarations_ of all public variables, constants, and functions.
    %
    % Note that we don't forward-declare the types here; the forward
    % declarations that we need for types used in function prototypes
    % are generated by mlds_output_type_forward_decls. See the comment
    % in mlds_output_decl.

    list.filter(defn_is_public, Defns, PublicDefns),
    list.filter(defn_is_type, PublicDefns, PublicTypeDefns,
        PublicNonTypeDefns),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    mlds_output_defns(Indent, yes, MLDS_ModuleName, PublicTypeDefns, !IO),
    io.nl(!IO),
    mlds_output_decls(Indent, MLDS_ModuleName, PublicNonTypeDefns, !IO),
    io.nl(!IO),
    mlds_output_init_fn_decls(MLDS_ModuleName, InitPreds, FinalPreds, !IO),
    io.nl(!IO),
    mlds_output_hdr_end(Indent, ModuleName, !IO).

:- pred mlds_output_hdr_imports(indent::in, mlds_imports::in,
    io::di, io::uo) is det.

% XXX currently we assume all imports are source imports, i.e. that the header
% file does not depend on any types defined in other header files.
mlds_output_hdr_imports(_Indent, _Imports, !IO).

:- pred mlds_output_src_imports(indent::in, mlds_imports::in,
    io::di, io::uo) is det.

mlds_output_src_imports(Indent, Imports, !IO) :-
    globals.io_get_target(Target, !IO),
    ( Target = target_asm ->
        % For --target asm, we don't create the header files for modules that
        % don't contain C code, so we'd better not include them, since they
        % might not exist.

        % XXX This is a hack; it may lead to warnings or errors when compiling
        % the generated code, since the functions that we call (e.g. for
        % `pragma export') may not have been declared.
        true
    ;
        list.foldl(mlds_output_src_import(Indent), Imports, !IO)
    ).

:- pred mlds_output_src_import(indent::in, mlds_import::in,
    io::di, io::uo) is det.

mlds_output_src_import(_Indent, Import, !IO) :-
    (
        Import = mercury_import(ImportType, ImportName),
        ModuleName0 = mlds_module_name_to_sym_name(ImportName),
        ( ImportType = user_visible_interface, HeaderExt = ".mh"
        ; ImportType = compiler_visible_interface, HeaderExt = ".mih"
        ),

        % Strip off the "mercury" qualifier for standard library modules.
        (
            ModuleName0 = qualified(unqualified("mercury"), ModuleName1),
            mercury_std_library_module(ModuleName1)
        ->
            ModuleName = unqualified(ModuleName1)
        ;
            ModuleName = ModuleName0
        )
    ;
        Import = foreign_import(ForeignImport),
        % This case shouldn't happen when compiling to C, but we need to handle
        % it for MLDS dumps when compiling to IL.
        ForeignImport = il_assembly_name(ImportName),
        ModuleName = mlds_module_name_to_sym_name(ImportName),
        HeaderExt = ".dll"
    ),

    module_name_to_search_file_name(ModuleName, HeaderExt, HeaderFile, !IO),
    io.write_strings(["#include """, HeaderFile, """\n"], !IO).

    % Generate the `.c' file.
    %
    % (Calling it the "source" file is a bit of a misnomer, since in our case
    % it is actually the target file, but there's no obvious alternative term
    % to use which also has a clear and concise abbreviation, so never mind...)
    %
:- pred mlds_output_src_file(indent::in, mlds::in, io::di, io::uo) is det.

mlds_output_src_file(Indent, MLDS, !IO) :-
    MLDS = mlds(ModuleName, AllForeignCode, Imports, Defns,
        InitPreds, FinalPreds),

    ForeignCode = mlds_get_c_foreign_code(AllForeignCode),
    EnvVarNameSet = mlds_get_env_var_names(Defns),
    set.to_sorted_list(EnvVarNameSet, EnvVarNames),
    mlds_output_src_start(Indent, ModuleName, ForeignCode,
        InitPreds, FinalPreds, EnvVarNames, !IO),
    io.nl(!IO),
    mlds_output_src_imports(Indent, Imports, !IO),
    io.nl(!IO),

    mlds_output_c_decls(Indent, ForeignCode, !IO),
    io.nl(!IO),

    list.foldl(mlds_output_env_var_decl, EnvVarNames, !IO),

    % The public types have already been defined in the header file, and the
    % public vars, consts, and functions have already been declared in the
    % header file. In the source file, we need to have
    %   #1. definitions of the private types,
    %   #2. forward-declarations of the private non-types
    %   #3. definitions of all the non-types
    %   #4. initialization functions
    % in that order.
    % #2 is needed to allow #3 to contain forward references, which can arise
    % for e.g. mutually recursive procedures. #1 is needed since #2 may refer
    % to the types.
    %
    % Note that we don't forward-declare the types here; the forward
    % declarations that we need for types used in function prototypes
    % are generated by mlds_output_type_forward_decls. See the comment in
    % mlds_output_decl.

    list.filter(defn_is_public, Defns, _PublicDefns, PrivateDefns),
    list.filter(defn_is_type, PrivateDefns, PrivateTypeDefns,
        PrivateNonTypeDefns),
    list.filter(defn_is_type, Defns, _TypeDefns, NonTypeDefns),
    list.filter(defn_is_function, NonTypeDefns, FuncDefns),
    list.filter(defn_is_type_ctor_info, NonTypeDefns, TypeCtorInfoDefns),

    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    mlds_output_defns(Indent, yes, MLDS_ModuleName, PrivateTypeDefns, !IO),
    io.nl(!IO),
    mlds_output_decls(Indent, MLDS_ModuleName, PrivateNonTypeDefns, !IO),
    io.nl(!IO),

    mlds_output_c_defns(MLDS_ModuleName, Indent, ForeignCode, !IO),
    io.nl(!IO),
    mlds_output_defns(Indent, yes, MLDS_ModuleName, NonTypeDefns, !IO),
    io.nl(!IO),
    mlds_output_init_fn_defns(MLDS_ModuleName, FuncDefns, TypeCtorInfoDefns,
        InitPreds, FinalPreds, !IO),
    io.nl(!IO),
    mlds_output_grade_var(!IO),
    io.nl(!IO),
    mlds_output_src_end(Indent, ModuleName, !IO).

:- func mlds_get_env_var_names(mlds_defns) = set(string).

mlds_get_env_var_names(Defns) = EnvVarNameSet :-
    list.filter_map(mlds_get_env_var_names_from_defn, Defns, EnvVarNameSets),
    EnvVarNameSet = set.union_list(EnvVarNameSets).

:- pred mlds_get_env_var_names_from_defn(mlds_defn::in, set(string)::out)
    is semidet.

mlds_get_env_var_names_from_defn(Defn, EnvVarNameSet) :-
    Defn = mlds_defn(_, _, _, mlds_function(_, _, _, _, EnvVarNameSet)).

:- pred mlds_output_env_var_decl(string::in, io::di, io::uo) is det.

mlds_output_env_var_decl(EnvVarName, !IO) :-
    io.write_string("extern MR_Word ", !IO),
    io.write_string(global_var_name(env_var_ref(EnvVarName)), !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_hdr_start(indent::in, mercury_module_name::in,
    io::di, io::uo) is det.

mlds_output_hdr_start(Indent, ModuleName, !IO) :-
    mlds_output_auto_gen_comment(ModuleName, !IO),
    mlds_indent(Indent, !IO),
    io.write_string("/* :- module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(". */\n", !IO),
    mlds_indent(Indent, !IO),
    io.write_string("/* :- interface. */\n", !IO),
    io.nl(!IO),
    mlds_indent(Indent, !IO),
    io.write_string("#ifndef MR_HEADER_GUARD_", !IO),
    MangledModuleName = sym_name_mangle(ModuleName),
    io.write_string(MangledModuleName, !IO),
    io.nl(!IO),
    mlds_indent(Indent, !IO),
    io.write_string("#define MR_HEADER_GUARD_", !IO),
    io.write_string(MangledModuleName, !IO),
    io.nl(!IO),
    io.nl(!IO),

    % If we're outputting C (rather than C++), then add a conditional
    % `extern "C"' wrapper around the header file, so that the header file
    % can be #included by C++ programs.

    globals.io_get_target(Target, !IO),
    ( Target = target_c ->
        mlds_indent(Indent, !IO),
        io.write_string("#ifdef __cplusplus\n", !IO),
        mlds_indent(Indent, !IO),
        io.write_string("extern ""C"" {\n", !IO),
        mlds_indent(Indent, !IO),
        io.write_string("#endif\n", !IO),
        io.nl(!IO)
    ;
        true
    ),
    mlds_indent(Indent, !IO),
    io.write_string("#include ""mercury.h""\n", !IO).

:- pred mlds_output_src_start(indent::in, mercury_module_name::in,
    mlds_foreign_code::in, list(string)::in, list(string)::in,
    list(string)::in, io::di, io::uo) is det.

mlds_output_src_start(Indent, ModuleName, ForeignCode, InitPreds, FinalPreds,
        EnvVarNames, !IO) :-
    mlds_output_auto_gen_comment(ModuleName, !IO),
    mlds_indent(Indent, !IO),
    io.write_string("/* :- module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(". */\n", !IO),
    mlds_indent(Indent, !IO),
    io.write_string("/* :- implementation. */\n", !IO),
    mlds_output_src_bootstrap_defines(!IO),
    io.nl(!IO),
    mlds_output_init_and_final_comments(ModuleName, InitPreds, FinalPreds,
        EnvVarNames, !IO),

    mlds_output_src_import(Indent,
        mercury_import(compiler_visible_interface,
            mercury_module_name_to_mlds(ModuleName)), !IO),

    % If there are `:- pragma foreign_export' declarations,
    % #include the `.mh' file.
    ( ForeignCode = mlds_foreign_code(_, _, _, []) ->
        true
    ;
        mlds_output_src_import(Indent,
            mercury_import(user_visible_interface,
                mercury_module_name_to_mlds(ModuleName)), !IO)
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

    % Output any #defines which are required to bootstrap in the hlc
    % grade.
    %
:- pred mlds_output_src_bootstrap_defines(io::di, io::uo) is det.

mlds_output_src_bootstrap_defines(!IO).

:- pred mlds_output_hdr_end(indent::in, mercury_module_name::in,
    io::di, io::uo) is det.

mlds_output_hdr_end(Indent, ModuleName, !IO) :-
    globals.io_get_target(Target, !IO),
    ( Target = target_c ->
        % Terminate the `extern "C"' wrapper.
        mlds_indent(Indent, !IO),
        io.write_string("#ifdef __cplusplus\n", !IO),
        mlds_indent(Indent, !IO),
        io.write_string("}\n", !IO),
        mlds_indent(Indent, !IO),
        io.write_string("#endif\n", !IO),
        io.nl(!IO)
    ;
        true
    ),
    mlds_indent(Indent, !IO),
    io.write_string("#endif /* MR_HEADER_GUARD_", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(" */\n", !IO),
    io.nl(!IO),
    mlds_indent(Indent, !IO),
    io.write_string("/* :- end_interface ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(". */\n", !IO).

:- pred mlds_output_src_end(indent::in, mercury_module_name::in,
    io::di, io::uo) is det.

mlds_output_src_end(Indent, ModuleName, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("/* :- end_module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(". */\n", !IO).

    % Output a C comment saying that the file was automatically generated
    % (and giving details such as the compiler version).
    %
:- pred mlds_output_auto_gen_comment(module_name::in, io::di, io::uo) is det.

mlds_output_auto_gen_comment(ModuleName, !IO) :-
    library.version(Version),
    module_name_to_file_name(ModuleName, ".m", no, SourceFileName, !IO),
    output_c_file_intro_and_grade(SourceFileName, Version, !IO),
    io.nl(!IO).

    % Output a reference to the mangled grade name for the grade that the C
    % file gets compiled with. This ensures that we don't try to link objects
    % files compiled in different grades.
    %
:- pred mlds_output_grade_var(io::di, io::uo) is det.

mlds_output_grade_var(!IO) :-
    io.write_string(
        "/* ensure everything is compiled with the same grade */\n",
        !IO),
    io.write_string(
        "static const void *const MR_grade = &MR_GRADE_VAR;\n",
        !IO).

    % Get the foreign code for C.
    %
:- func mlds_get_c_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_c_foreign_code(AllForeignCode) = ForeignCode :-
    ( map.search(AllForeignCode, lang_c, ForeignCode0) ->
        ForeignCode = ForeignCode0
    ;
        % This can occur when compiling to a non-C target using
        % "--mlds-dump all".
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%-----------------------------------------------------------------------------%

    % Maybe output the function `mercury__<modulename>__init()'.
    % The body of the function consists of calls MR_init_entry(<function>)
    % for each function defined in the module.
    %
    % If there are any user-defined intialisation or finalisation predicates
    % then output the functions: `mercury__<modulename>__required_init()' and
    % `mercury__<modulename>__required_final()' as necessary.
    %
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
    ).

:- pred mlds_output_init_fn_defns(mlds_module_name::in, mlds_defns::in,
    mlds_defns::in, list(string)::in, list(string)::in, io::di, io::uo)
    is det.

mlds_output_init_fn_defns(ModuleName, FuncDefns, TypeCtorInfoDefns, InitPreds,
        FinalPreds, !IO) :-
    output_init_fn_name(ModuleName, "", !IO),
    io.write_string("\n{\n", !IO),
    globals.io_get_globals(Globals, !IO),
    (
        need_to_init_entries(Globals),
        FuncDefns = [_ | _]
    ->
        io.write_strings(["\tstatic MR_bool initialised = MR_FALSE;\n",
            "\tif (initialised) return;\n",
            "\tinitialised = MR_TRUE;\n\n"], !IO),
        mlds_output_calls_to_init_entry(ModuleName, FuncDefns, !IO)
    ;
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
    %
    % Maybe write out wrapper functions that call user-defined intialisation
    % and finalisation predicates.
    %
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
output_required_calls([ Call | Calls ], !IO) :-
    io.write_string("\t" ++ Call ++ "();\n", !IO),
    output_required_calls(Calls, !IO).

:- pred output_init_fn_name(mlds_module_name::in, string::in,
    io::di, io::uo) is det.

output_init_fn_name(ModuleName, Suffix, !IO) :-
    % Here we ensure that we only get one "mercury__" at the start
    % of the function name.
    sym_name_to_string(mlds_module_name_to_sym_name(ModuleName), "__",
        ModuleNameString0),
    ( string.prefix(ModuleNameString0, "mercury__") ->
        ModuleNameString = ModuleNameString0
    ;
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
    sym_name_to_string(mlds_module_name_to_sym_name(ModuleName), "__",
        ModuleNameString0),
    ( string.prefix(ModuleNameString0, "mercury__") ->
        ModuleNameString = ModuleNameString0
    ;
        ModuleNameString = "mercury__" ++ ModuleNameString0
    ),
    io.write_string("void ", !IO),
    io.write_string(ModuleNameString, !IO),
    io.write_string("__", !IO),
    io.write_string(Suffix, !IO),
    io.write_string("(void)", !IO).

:- pred need_to_init_entries(globals::in) is semidet.

need_to_init_entries(Globals) :-
    % We only need to output calls to MR_init_entry() if profiling is enabled.
    % (It would be OK to output the calls regardless, since they will
    % macro-expand to nothing if profiling is not enabled, but for readability
    % of the generated code we prefer not to.)
    ( Option = profile_calls
    ; Option = profile_time
    ; Option = profile_memory
    ),
    globals.lookup_bool_option(Globals, Option, yes).

    % Generate calls to MR_init_entry() for the specified functions.
    %
:- pred mlds_output_calls_to_init_entry(mlds_module_name::in, mlds_defns::in,
    io::di, io::uo) is det.

mlds_output_calls_to_init_entry(_ModuleName, [], !IO).
mlds_output_calls_to_init_entry(ModuleName, [FuncDefn | FuncDefns], !IO) :-
    FuncDefn = mlds_defn(EntityName, _, _, _),
    io.write_string("\tMR_init_entry(", !IO),
    mlds_output_fully_qualified_name(qual(ModuleName, module_qual, EntityName),
        !IO),
    io.write_string(");\n", !IO),
    mlds_output_calls_to_init_entry(ModuleName, FuncDefns, !IO).

    % Generate calls to MR_register_type_ctor_info() for the specified
    % type_ctor_infos.
    %
:- pred mlds_output_calls_to_register_tci(mlds_module_name::in,
    mlds_defns::in, io::di, io::uo) is det.

mlds_output_calls_to_register_tci(_ModuleName, [], !IO).
mlds_output_calls_to_register_tci(ModuleName,
        [TypeCtorInfoDefn | TypeCtorInfoDefns], !IO) :-
    TypeCtorInfoDefn = mlds_defn(EntityName, _, _, _),
    io.write_string("\tMR_register_type_ctor_info(&", !IO),
    mlds_output_fully_qualified_name(qual(ModuleName, module_qual, EntityName),
        !IO),
    io.write_string(");\n", !IO),
    mlds_output_calls_to_register_tci(ModuleName, TypeCtorInfoDefns, !IO).

%-----------------------------------------------------------------------------%
%
% Foreign language interface stuff
%

:- pred mlds_output_c_hdr_decls(mlds_module_name::in, indent::in,
    mlds_foreign_code::in, io::di, io::uo) is det.

mlds_output_c_hdr_decls(ModuleName, Indent, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(RevHeaderCode, _RevImports,
        _RevBodyCode, _ExportDefns),
    HeaderCode = list.reverse(RevHeaderCode),
    ( is_std_lib_module(ModuleName, ModuleNameStr) ->
        SymName = unqualified(ModuleNameStr)
    ;
        SymName = mlds_module_name_to_sym_name(ModuleName)
    ),

    DeclGuard = decl_guard(SymName),
    io.write_strings(["#ifndef ", DeclGuard, "\n#define ", DeclGuard, "\n"],
        !IO),
    %
    % We need to make sure we #include the .mih files for any ancestor modules
    % in cases any foreign_types defined in them are referenced by the extern
    % declarations required by mutables.
    %
    AncestorModuleNames = get_ancestors(SymName),
    list.map(module_name_to_file_name, AncestorModuleNames, AncestorFileNames),
    WriteAncestorInclude = (pred(Ancestor::in, !.IO::di, !:IO::uo) is det :-
        io.write_strings(["#include \"", Ancestor, ".mih", "\"\n"], !IO)
    ),
    list.foldl(WriteAncestorInclude, AncestorFileNames, !IO),
    io.write_list(HeaderCode, "\n",
        mlds_output_c_hdr_decl(Indent, yes(foreign_decl_is_exported)), !IO),
    io.write_string("\n#endif\n", !IO).

:- pred mlds_output_c_hdr_decl(indent::in, maybe(foreign_decl_is_local)::in,
    foreign_decl_code::in, io::di, io::uo) is det.

mlds_output_c_hdr_decl(_Indent, MaybeDesiredIsLocal, DeclCode, !IO) :-
    DeclCode = foreign_decl_code(Lang, IsLocal, Code, Context),
    % Only output C code in the C header file.
    ( Lang = lang_c ->
        (
            (
                MaybeDesiredIsLocal = no
            ;
                MaybeDesiredIsLocal = yes(DesiredIsLocal),
                IsLocal = DesiredIsLocal
            )
        ->
            output_context(mlds_make_context(Context), !IO),
            io.write_string(Code, !IO)
        ;
            true
        )
    ;
        sorry(this_file, "foreign code other than C")
    ).

:- pred mlds_output_c_decls(indent::in, mlds_foreign_code::in,
    io::di, io::uo) is det.

mlds_output_c_decls(Indent, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(RevHeaderCode, _RevImports,
        _RevBodyCode, _ExportDefns),
    HeaderCode = list.reverse(RevHeaderCode),
    io.write_list(HeaderCode, "\n",
        mlds_output_c_hdr_decl(Indent, yes(foreign_decl_is_local)), !IO).

:- pred mlds_output_c_defns(mlds_module_name::in, indent::in,
    mlds_foreign_code::in, io::di, io::uo) is det.

mlds_output_c_defns(ModuleName, Indent, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(_RevHeaderCode, RevImports,
        RevBodyCode, ExportDefns),
    Imports = list.reverse(RevImports),
    list.foldl(mlds_output_c_foreign_import_module(Indent), Imports, !IO),
    BodyCode = list.reverse(RevBodyCode),
    io.write_list(BodyCode, "\n", mlds_output_c_defn(Indent), !IO),
    io.write_string("\n", !IO),
    io.write_list(ExportDefns, "\n",
        mlds_output_pragma_export_defn(ModuleName, Indent), !IO).

:- pred mlds_output_c_foreign_import_module(int::in,
    foreign_import_module_info::in, io::di, io::uo) is det.

mlds_output_c_foreign_import_module(Indent, ForeignImport, !IO) :-
    ForeignImport = foreign_import_module_info(Lang, Import, _),
    (
        Lang = lang_c,
        mlds_output_src_import(Indent,
            mercury_import(user_visible_interface,
                mercury_module_name_to_mlds(Import)), !IO)
    ;
        ( Lang = lang_il
        ; Lang = lang_csharp
        ; Lang = lang_managed_cplusplus
        ; Lang = lang_java
        ),
        sorry(this_file, "foreign code other than C")
    ).

:- pred mlds_output_c_defn(indent::in, user_foreign_code::in,
    io::di, io::uo) is det.

mlds_output_c_defn(_Indent, user_foreign_code(lang_c, Code, Context), !IO) :-
    output_context(mlds_make_context(Context), !IO),
    io.write_string(Code, !IO).
mlds_output_c_defn(_Indent, user_foreign_code(lang_managed_cplusplus, _, _),
        !IO) :-
    sorry(this_file, "foreign code other than C").
mlds_output_c_defn(_Indent, user_foreign_code(lang_csharp, _, _), !IO) :-
    sorry(this_file, "foreign code other than C").
mlds_output_c_defn(_Indent, user_foreign_code(lang_il, _, _), !IO) :-
    sorry(this_file, "foreign code other than C").
mlds_output_c_defn(_Indent, user_foreign_code(lang_java, _, _), !IO) :-
    sorry(this_file, "foreign code other than C").

:- pred mlds_output_pragma_export_defn(mlds_module_name::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

mlds_output_pragma_export_defn(ModuleName, Indent, PragmaExport, !IO) :-
    PragmaExport = ml_pragma_export(Lang, _ExportName, MLDS_Name,
        MLDS_Signature, Context),
    expect(unify(Lang, lang_c), this_file,
        "foreign_export to language other than C."),
    mlds_output_pragma_export_func_name(ModuleName, Indent, PragmaExport, !IO),
    io.write_string("\n", !IO),
    mlds_indent(Context, Indent, !IO),
    io.write_string("{\n", !IO),
    mlds_indent(Context, Indent, !IO),
    mlds_output_pragma_export_defn_body(ModuleName, MLDS_Name, MLDS_Signature,
        !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_pragma_export_func_name(mlds_module_name::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

mlds_output_pragma_export_func_name(ModuleName, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _MLDSName, Signature, Context),
    expect(unify(Lang, lang_c), this_file, "export to language other than C."),
    Name = qual(ModuleName, module_qual, entity_export(ExportName)),
    mlds_indent(Context, Indent, !IO),
    % For functions exported using `pragma export',
    % we use the default C calling convention.
    CallingConvention = "",
    mlds_output_func_decl_ho(Indent, Name, Context,
        CallingConvention, Signature,
        mlds_output_pragma_export_type(prefix),
        mlds_output_pragma_export_type(suffix), !IO).

:- pred mlds_output_pragma_export_type(mlds_type::in, io::di, io::uo) is det.

mlds_output_pragma_export_type(Type, !IO) :-
    mlds_output_pragma_export_type(prefix, Type, !IO),
    mlds_output_pragma_export_type(suffix, Type, !IO).

:- type locn
    --->    prefix
    ;       suffix.

:- pred mlds_output_pragma_export_type(locn::in, mlds_type::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_type(suffix, _Type, !IO).
mlds_output_pragma_export_type(prefix, mlds_mercury_array_type(_ElemType),
        !IO) :-
    io.write_string("MR_ArrayPtr", !IO).
mlds_output_pragma_export_type(prefix, mercury_type(_, _, ExportedType),
        !IO) :-
    io.write_string(foreign.to_type_string(lang_c, ExportedType), !IO).
mlds_output_pragma_export_type(prefix, mlds_cont_type(_), !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_commit_type, !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_native_bool_type, !IO) :-
    io.write_string("MR_bool", !IO).
mlds_output_pragma_export_type(prefix, mlds_native_int_type, !IO) :-
    io.write_string("MR_Integer", !IO).
mlds_output_pragma_export_type(prefix, mlds_native_float_type, !IO) :-
    io.write_string("MR_Float", !IO).
mlds_output_pragma_export_type(prefix, mlds_native_char_type, !IO) :-
    io.write_string("MR_Char", !IO).
mlds_output_pragma_export_type(prefix, mlds_foreign_type(ForeignType), !IO) :-
    (
        ForeignType = c(c_type(Name)),
        io.write_string(Name, !IO)
    ;
        ForeignType = il(_),
        unexpected(this_file, "mlds_output_type_prefix: il foreign_type")
    ;
        ForeignType = java(_),
        unexpected(this_file, "mlds_output_type_prefix: java foreign_type")
    ).
mlds_output_pragma_export_type(prefix, mlds_class_type(_, _, _), !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_array_type(_), !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_ptr_type(Type), !IO) :-
    mlds_output_pragma_export_type(prefix, Type, !IO),
    io.write_string(" *", !IO).
mlds_output_pragma_export_type(prefix, mlds_func_type(_), !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_generic_type, !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_generic_env_ptr_type, !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_type_info_type, !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_pseudo_type_info_type, !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_rtti_type(_), !IO) :-
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_tabling_type(_), !IO) :-
    % These types should never occur in procedures exported to C, so the fact
    % that we could generate a more accurate type shouldn't matter.
    io.write_string("MR_Word", !IO).
mlds_output_pragma_export_type(prefix, mlds_unknown_type, !IO) :-
    unexpected(this_file, "mlds_output_pragma_export_type: unknown_type").

    % Output the definition body for a pragma export.
    %
:- pred mlds_output_pragma_export_defn_body(mlds_module_name::in,
    mlds_qualified_entity_name::in, mlds_func_params::in, io::di, io::uo)
    is det.

mlds_output_pragma_export_defn_body(ModuleName, FuncName, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),

    % Declare local variables corresponding to any foreign_type parameters.
    IsCForeignType = (pred(Arg::in) is semidet :-
        Arg = mlds_argument(_Name, Type, _GCTraceCode),
        Type = mlds_foreign_type(c(_))
    ),
    IsCForeignTypePtr = (pred(Arg::in) is semidet :-
        Arg = mlds_argument(_Name, Type, _GCTraceCode),
        Type = mlds_ptr_type(mlds_foreign_type(c(_)))
    ),
    CForeignTypeInputs = list.filter(IsCForeignType, Parameters),
    CForeignTypeOutputs = list.filter(IsCForeignTypePtr, Parameters),
    io.write_list(CForeignTypeInputs, "",
        mlds_output_pragma_export_input_defns(ModuleName), !IO),
    io.write_list(CForeignTypeOutputs, "",
        mlds_output_pragma_export_output_defns(ModuleName), !IO),

    % Declare a local variable or two for the return value, if needed.
    ( RetTypes = [RetType1] ->
        ( RetType1 = mlds_foreign_type(c(_)) ->
            io.write_string("\t", !IO),
            mlds_output_pragma_export_type(RetType1, !IO),
            io.write_string(" ret_value;\n", !IO),
            io.write_string("\t", !IO),
            mlds_output_type(RetType1, !IO),
            io.write_string(" boxed_ret_value;\n", !IO)
        ;
            io.write_string("\t", !IO),
            mlds_output_pragma_export_type(RetType1, !IO),
            io.write_string(" ret_value;\n", !IO)
        )
    ;
        true
    ),

    % Generate code to box any non-word-sized foreign_type input parameters;
    % these need to be converted to a uniform size before passing them
    % to Mercury code.
    io.write_list(CForeignTypeInputs, "",
        mlds_output_pragma_input_arg(ModuleName), !IO),

    % Generate code to actually call the Mercury procedure which
    % is being exported
    ( RetTypes = [] ->
        io.write_string("\t", !IO),
        mlds_output_pragma_export_call(ModuleName, FuncName, Parameters, !IO)
    ; RetTypes = [RetType2] ->
        ( RetType2 = mlds_foreign_type(c(_)) ->
            io.write_string("\tboxed_ret_value = ", !IO)
        ;
            io.write_string("\tret_value = (", !IO),
            mlds_output_pragma_export_type(RetType2, !IO),
            io.write_string(")", !IO)
        ),
        mlds_output_pragma_export_call(ModuleName, FuncName, Parameters, !IO)
    ;
        % This is just for MLDS dumps when compiling to non-C targets.
        % So we don't need to worry about boxing/unboxing foreign types
        % here.
        io.write_string("\treturn (", !IO),
        mlds_output_return_list(RetTypes, mlds_output_pragma_export_type, !IO),
        io.write_string(") ", !IO)
    ),

    % Generate code to unbox any foreign_type output parameters,
    % since we are returning those parameters to C code.
    io.write_list(CForeignTypeOutputs, "",
        mlds_output_pragma_output_arg(ModuleName), !IO),

    % Generate the final statement to unbox and return the return value,
    % if needed.
    ( RetTypes = [RetType3] ->
        ( RetType3 = mlds_foreign_type(c(_)) ->
            io.write_string("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
            mlds_output_pragma_export_type(RetType3, !IO),
            io.write_string(", boxed_ret_value, ret_value);\n", !IO)
        ;
            true
        ),
        io.write_string("\treturn ret_value;\n", !IO)
    ;
        true
    ).

:- pred mlds_output_pragma_input_arg(mlds_module_name::in, mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_input_arg(ModuleName, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GC_TraceCode),
    QualName = qual(ModuleName, module_qual, Name),
    BoxedQualName = qual(ModuleName, module_qual, boxed_name(Name)),
    io.write_string("\tMR_MAYBE_BOX_FOREIGN_TYPE(", !IO),
    mlds_output_pragma_export_type(Type, !IO),
    io.write_string(", ", !IO),
    mlds_output_fully_qualified_name(QualName, !IO),
    io.write_string(", ", !IO),
    mlds_output_fully_qualified_name(BoxedQualName, !IO),
    io.write_string(");\n", !IO).

:- pred mlds_output_pragma_output_arg(mlds_module_name::in, mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_output_arg(ModuleName, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GC_TraceCode),
    QualName = qual(ModuleName, module_qual, Name),
    BoxedQualName = qual(ModuleName, module_qual, boxed_name(Name)),
    io.write_string("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
    mlds_output_pragma_export_type(pointed_to_type(Type), !IO),
    io.write_string(", ", !IO),
    mlds_output_fully_qualified_name(BoxedQualName, !IO),
    io.write_string(", *", !IO),
    mlds_output_fully_qualified_name(QualName, !IO),
    io.write_string(");\n", !IO).

:- pred mlds_output_pragma_export_input_defns(mlds_module_name::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_input_defns(ModuleName, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GC_TraceCode),
    io.write_string("\t", !IO),
    mlds_output_data_decl_ho(mlds_output_type_prefix, mlds_output_type_suffix,
        qual(ModuleName, module_qual, boxed_name(Name)), Type, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_pragma_export_output_defns(mlds_module_name::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_output_defns(ModuleName, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GC_TraceCode),
    io.write_string("\t", !IO),
    mlds_output_data_decl_ho(mlds_output_type_prefix, mlds_output_type_suffix,
        qual(ModuleName, module_qual, boxed_name(Name)), pointed_to_type(Type),
        !IO),
    io.write_string(";\n", !IO).

:- func pointed_to_type(mlds_type) = mlds_type.

pointed_to_type(PtrType) =
    ( PtrType = mlds_ptr_type(Type) ->
        Type
    ;
        unexpected(this_file, "pointed_to_type: not pointer")
    ).

:- func boxed_name(mlds_entity_name) = mlds_entity_name.

boxed_name(Name) = BoxedName :-
    ( Name = entity_data(var(mlds_var_name(VarName, Seq))) ->
        BoxedName = entity_data(var(mlds_var_name("boxed_" ++ VarName, Seq)))
    ;
        unexpected(this_file, "boxed_name called for non-var argument")
    ).

:- pred mlds_output_pragma_export_call(mlds_module_name::in,
    mlds_qualified_entity_name::in, mlds_arguments::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_call(ModuleName, FuncName, Parameters, !IO) :-
    mlds_output_fully_qualified_name(FuncName, !IO),
    io.write_string("(", !IO),
    io.write_list(Parameters, ", ", mlds_output_pragma_export_arg(ModuleName),
        !IO),
    io.write_string(");\n", !IO).

    % Output a fully qualified name preceded by a cast.
    %
:- pred mlds_output_pragma_export_arg(mlds_module_name::in, mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_arg(ModuleName, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GC_TraceCode),
    ( Type = mlds_foreign_type(c(_)) ->
        % This is a foreign_type input. Pass in the already-boxed value.
        BoxedName = boxed_name(Name),
        mlds_output_fully_qualified_name(
            qual(ModuleName, module_qual, BoxedName), !IO)
    ; Type = mlds_ptr_type(mlds_foreign_type(c(_))) ->
        % This is a foreign_type output.  Pass in the address of the
        % local variable which will hold the boxed value.
        io.write_string("&", !IO),
        BoxedName = boxed_name(Name),
        mlds_output_fully_qualified_name(
            qual(ModuleName, module_qual, BoxedName), !IO)
    ;
        % Otherwise, no boxing or unboxing is needed.
        % Just cast the argument to the right type.
        mlds_output_cast(Type, !IO),
        mlds_output_fully_qualified_name(qual(ModuleName, module_qual, Name),
            !IO)
    ).

    % Generates the signature for det functions in the forward mode.
    %
:- func det_func_signature(mlds_func_params) = mlds_func_params.

det_func_signature(mlds_func_params(Args, _RetTypes)) = Params :-
    list.length(Args, NumArgs),
    NumFuncArgs = NumArgs - 1,
    ( list.split_list(NumFuncArgs, Args, InputArgs0, [ReturnArg0]) ->
        InputArgs = InputArgs0,
        ReturnArg = ReturnArg0
    ;
        unexpected(this_file,
            "det_func_signature: function missing return value?")
    ),
    (
        ReturnArg = mlds_argument(_ReturnArgName,
            mlds_ptr_type(ReturnArgType0), _GC_TraceCode)
    ->
        ReturnArgType = ReturnArgType0
    ;
        unexpected(this_file,
            "det_func_signature: function return type!")
    ),
    Params = mlds_func_params(InputArgs, [ReturnArgType]).

%-----------------------------------------------------------------------------%
%
% Code to output declarations and definitions
%

:- pred mlds_output_decls(indent::in, mlds_module_name::in, mlds_defns::in,
    io::di, io::uo) is det.

mlds_output_decls(Indent, ModuleName, Defns, !IO) :-
    list.foldl(mlds_output_decl_blank_line(Indent, ModuleName), Defns, !IO).

:- pred mlds_output_defns(indent::in, bool::in, mlds_module_name::in,
    mlds_defns::in, io::di, io::uo) is det.

mlds_output_defns(Indent, Separate, ModuleName, Defns, !IO) :-
    OutputDefn = mlds_output_defn(Indent, Separate, ModuleName),
    globals.io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels, !IO),
    (
        GCC_LocalLabels = yes,
        % GNU C __label__ declarations must precede ordinary variable
        % declarations.

        list.filter(defn_is_commit_type_var, Defns, LabelDecls, OtherDefns),
        list.foldl(OutputDefn, LabelDecls, !IO),
        list.foldl(OutputDefn, OtherDefns, !IO)
    ;
        GCC_LocalLabels = no,
        list.foldl(OutputDefn, Defns, !IO)
    ).

:- pred mlds_output_decl_blank_line(indent::in, mlds_module_name::in,
    mlds_defn::in, io::di, io::uo) is det.

mlds_output_decl_blank_line(Indent, ModuleName, Defn, !IO) :-
    io.nl(!IO),
    mlds_output_decl(Indent, ModuleName, Defn, !IO).

:- pred mlds_output_decl(indent::in, mlds_module_name::in, mlds_defn::in,
    io::di, io::uo) is det.

mlds_output_decl(Indent, ModuleName, Defn, !IO) :-
    Defn = mlds_defn(Name, Context, Flags, DefnBody),
    (
        % ANSI C does not permit forward declarations of enumeration types.
        % So we just skip those. Currently they're not needed since we don't
        % actually use the enum types.

        DefnBody = mlds_class(ClassDefn),
        ClassDefn ^ kind = mlds_enum
    ->
        true
    ;
        % If we're using --high-level-data, then for function declarations,
        % we need to ensure that we forward-declare any types used in the
        % function parameters. This is because otherwise, for any struct names
        % whose first occurence is in the function parameters, the scope of
        % such struct names is just that function declaration, which is never
        % right.
        %
        % We generate such forward declarations here, rather than generating
        % type declarations in a header file and #including that header file,
        % because doing the latter would significantly complicate the
        % dependencies (to avoid cyclic #includes, you'd need to generate
        % the type declarations in a different header file than the function
        % declarations).

        globals.io_lookup_bool_option(highlevel_data, HighLevelData, !IO),
        (
            HighLevelData = yes,
            DefnBody = mlds_function(_, Params, _, _, _)
        ->
            Params = mlds_func_params(Arguments, _RetTypes),
            ParamTypes = mlds_get_arg_types(Arguments),
            mlds_output_type_forward_decls(Indent, ParamTypes, !IO)
        ;
            true
        ),

        % Now output the declaration for this mlds_defn.
        mlds_indent(Context, Indent, !IO),
        mlds_output_decl_flags(Flags, forward_decl, Name, DefnBody, !IO),
        mlds_output_decl_body(Indent, qual(ModuleName, module_qual, Name),
            Context, DefnBody, !IO)
    ).

:- pred mlds_output_type_forward_decls(indent::in, list(mlds_type)::in,
    io::di, io::uo) is det.

mlds_output_type_forward_decls(Indent, ParamTypes, !IO) :-
    % Output forward declarations for all struct types
    % that are contained in the parameter types.

    solutions.aggregate(mlds_type_list_contains_type(ParamTypes),
        mlds_output_type_forward_decl(Indent), !IO).

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
    ( list.member(mlds_argument(_Name, Type, _GC_TraceCode), Arguments)
    ; list.member(Type, RetTypes)
    ).

:- pred mlds_output_type_forward_decl(indent::in, mlds_type::in,
    io::di, io::uo) is det.

mlds_output_type_forward_decl(Indent, Type, !IO) :-
    (
        (
            Type = mlds_class_type(_Name, _Arity, Kind),
            Kind \= mlds_enum,
            ClassType = Type
        ;
            Type = mercury_type(MercuryType, type_cat_user_ctor, _),
            type_to_ctor_and_args(MercuryType, TypeCtor, _ArgsTypes),
            ml_gen_type_name(TypeCtor, ClassName, ClassArity),
            ClassType = mlds_class_type(ClassName, ClassArity, mlds_class)
        )
    ->
        mlds_indent(Indent, !IO),
        mlds_output_type(ClassType, !IO),
        io.write_string(";\n", !IO)
    ;
        true
    ).

:- pred mlds_output_defn(indent::in, bool::in, mlds_module_name::in,
    mlds_defn::in, io::di, io::uo) is det.

mlds_output_defn(Indent, Separate, ModuleName, Defn, !IO) :-
    Defn = mlds_defn(Name, Context, Flags, DefnBody),
    ( DefnBody \= mlds_data(_, _, _) ->
        io.nl(!IO)
    ; Separate = yes ->
        io.nl(!IO)
    ;
        true
    ),
    mlds_indent(Context, Indent, !IO),
    mlds_output_decl_flags(Flags, definition, Name, DefnBody, !IO),
    mlds_output_defn_body(Indent, qual(ModuleName, module_qual, Name),
        Context, DefnBody, !IO).

:- pred mlds_output_decl_body(indent::in, mlds_qualified_entity_name::in,
    mlds_context::in, mlds_entity_defn::in, io::di, io::uo) is det.

mlds_output_decl_body(Indent, Name, Context, DefnBody, !IO) :-
    (
        DefnBody = mlds_data(Type, Initializer, _GC_TraceCode),
        mlds_output_data_decl(Name, Type, initializer_array_size(Initializer),
            !IO)
    ;
        DefnBody = mlds_function(MaybePredProcId, Signature,
            _MaybeBody, _Attrs, _EnvVarNames),
        mlds_output_maybe(MaybePredProcId, mlds_output_pred_proc_id, !IO),
        mlds_output_func_decl(Indent, Name, Context, Signature, !IO)
    ;
        DefnBody = mlds_class(ClassDefn),
        mlds_output_class_decl(Indent, Name, ClassDefn, !IO)
    ),
    io.write_string(";\n", !IO).

:- pred mlds_output_defn_body(indent::in, mlds_qualified_entity_name::in,
    mlds_context::in, mlds_entity_defn::in, io::di, io::uo) is det.

mlds_output_defn_body(Indent, Name, Context, DefnBody, !IO) :-
    (
        DefnBody = mlds_data(Type, Initializer, Maybe_GC_TraceCode),
        mlds_output_data_defn(Name, Type, Initializer, !IO),
        mlds_output_maybe_gc_trace_code(Indent, Name, Maybe_GC_TraceCode, "",
            !IO)
    ;
        DefnBody = mlds_function(MaybePredProcId, Signature,
            MaybeBody, _Attributes, _EnvVarNames),
        mlds_output_maybe(MaybePredProcId, mlds_output_pred_proc_id, !IO),
        mlds_output_func(Indent, Name, Context, Signature, MaybeBody, !IO)
    ;
        DefnBody = mlds_class(ClassDefn),
        mlds_output_class(Indent, Name, Context, ClassDefn, !IO)
    ).

:- pred mlds_output_maybe_gc_trace_code(indent::in,
    mlds_qualified_entity_name::in, maybe(statement)::in,
    string::in, io::di, io::uo) is det.

mlds_output_maybe_gc_trace_code(Indent, Name, Maybe_GC_TraceCode, MaybeNewLine,
        !IO) :-
    (
        Maybe_GC_TraceCode = no
    ;
        Maybe_GC_TraceCode = yes(GC_TraceCode),
        io.write_string(MaybeNewLine, !IO),
        io.write_string("#if 0 /* GC trace code */\n", !IO),
        % XXX This value for FuncInfo is bogus. However, this output is only
        % for debugging anyway, so it doesn't really matter.
        FuncInfo = func_info(Name, mlds_func_signature([], [])),
        mlds_output_statement(Indent, FuncInfo, GC_TraceCode, !IO),
        io.write_string("#endif\n", !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Code to output type declarations/definitions
%

:- pred mlds_output_class_decl(indent::in, mlds_qualified_entity_name::in,
    mlds_class_defn::in, io::di, io::uo) is det.

mlds_output_class_decl(_Indent, Name, ClassDefn, !IO) :-
    ( ClassDefn^kind = mlds_enum ->
        io.write_string("enum ", !IO),
        mlds_output_fully_qualified_name(Name, !IO),
        io.write_string("_e", !IO)
    ;
        io.write_string("struct ", !IO),
        mlds_output_fully_qualified_name(Name, !IO),
        io.write_string("_s", !IO)
    ).

:- pred mlds_output_class(indent::in, mlds_qualified_entity_name::in,
    mlds_context::in, mlds_class_defn::in, io::di, io::uo) is det.

mlds_output_class(Indent, Name, Context, ClassDefn, !IO) :-
    % To avoid name clashes, we need to qualify the names of the member
    % constants with the class name. (In particular, this is needed for
    % enumeration constants and for the nested classes that we generate for
    % constructors of discriminated union types.) Here we compute the
    % appropriate qualifier.

    Name = qual(ModuleName, QualKind, UnqualName),
    ( UnqualName = entity_type(ClassName, ClassArity) ->
        globals.io_get_globals(Globals, !IO),
        ClassModuleName = mlds_append_class_qualifier(ModuleName,
            QualKind, Globals, ClassName, ClassArity)
    ;
        unexpected(this_file, "mlds_output_enum_constants")
    ),

    % Hoist out static members, since plain old C doesn't support
    % static members in structs (except for enumeration constants).
    %
    % XXX this should be conditional: only when compiling to C,
    % not when compiling to C++

    ClassDefn = mlds_class_defn(Kind, _Imports, BaseClasses, _Implements,
        Ctors, Members),

    AllMembers = Ctors ++ Members,

    ( Kind = mlds_enum ->
        StaticMembers = [],
        StructMembers = AllMembers
    ;
        list.filter(is_static_member, AllMembers,
            StaticMembers, NonStaticMembers),
        StructMembers = NonStaticMembers
    ),

    % Convert the base classes into member variables,
    % since plain old C doesn't support base classes.
    %
    % XXX this should be conditional: only when compiling to C,
    % not when compiling to C++

    list.map_foldl(mlds_make_base_class(Context), BaseClasses, BaseDefns,
        1, _),
    list.append(BaseDefns, StructMembers, BasesAndMembers),

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

    mlds_output_class_decl(Indent, Name, ClassDefn, !IO),
    io.write_string(" {\n", !IO),
    ( Kind = mlds_enum ->
        mlds_output_enum_constants(Indent + 1, ClassModuleName,
            BasesAndMembers, !IO)
    ;
        mlds_output_defns(Indent + 1, no, ClassModuleName,
            BasesAndMembers, !IO)
    ),
    mlds_indent(Context, Indent, !IO),
    io.write_string("};\n", !IO),
    mlds_output_defns(Indent, yes, ClassModuleName, StaticMembers, !IO).

:- pred is_static_member(mlds_defn::in) is semidet.

is_static_member(Defn) :-
    Defn = mlds_defn(Name, _, Flags, _),
    ( Name = entity_type(_, _)
    ; per_instance(Flags) = one_copy
    ).

    % Convert a base class class_id into a member variable
    % that holds the value of the base class.
    %
:- pred mlds_make_base_class(mlds_context::in, mlds_class_id::in,
    mlds_defn::out, int::in, int::out) is det.

mlds_make_base_class(Context, ClassId, MLDS_Defn, BaseNum0, BaseNum) :-
    BaseName = mlds_var_name(string.format("base_%d", [i(BaseNum0)]), no),
    Type = ClassId,
    % We only need GC tracing code for top-level variables,
    % not for base classes.
    GC_TraceCode = no,
    MLDS_Defn = mlds_defn(entity_data(var(BaseName)), Context,
        ml_gen_public_field_decl_flags,
        mlds_data(Type, no_initializer, GC_TraceCode)),
    BaseNum = BaseNum0 + 1.

    % Output the definitions of the enumeration constants
    % for an enumeration type.
    %
:- pred mlds_output_enum_constants(indent::in, mlds_module_name::in,
    mlds_defns::in, io::di, io::uo) is det.

mlds_output_enum_constants(Indent, EnumModuleName, Members, !IO) :-
    % Select the enumeration constants from the list of members
    % for this enumeration type, and output them.
    EnumConsts = list.filter(is_enum_const, Members),
    io.write_list(EnumConsts, ",\n",
        mlds_output_enum_constant(Indent, EnumModuleName), !IO),
    io.nl(!IO).

    % Test whether one of the members of an mlds_enum class
    % is an enumeration constant.
    %
:- pred is_enum_const(mlds_defn::in) is semidet.

is_enum_const(Defn) :-
    Defn = mlds_defn(_Name, _Context, Flags, _DefnBody),
    constness(Flags) = const.

    % Output the definition of a single enumeration constant.
    %
:- pred mlds_output_enum_constant(indent::in, mlds_module_name::in,
    mlds_defn::in, io::di, io::uo) is det.

mlds_output_enum_constant(Indent, EnumModuleName, Defn, !IO) :-
    Defn = mlds_defn(Name, Context, _Flags, DefnBody),
    ( DefnBody = mlds_data(Type, Initializer, _GC_TraceCode) ->
        mlds_indent(Context, Indent, !IO),
        mlds_output_fully_qualified_name(
            qual(EnumModuleName, type_qual, Name), !IO),
        mlds_output_initializer(Type, Initializer, !IO)
    ;
        unexpected(this_file,
            "mlds_output_enum_constant: constant is not data")
    ).

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions
%

:- pred mlds_output_data_decl(mlds_qualified_entity_name::in, mlds_type::in,
    initializer_array_size::in, io::di, io::uo) is det.

mlds_output_data_decl(Name, Type, InitializerSize, !IO) :-
    mlds_output_data_decl_ho(mlds_output_type_prefix,
        mlds_output_data_decl_2(InitializerSize), Name, Type, !IO).

:- pred mlds_output_data_decl_2(initializer_array_size::in, mlds_type::in,
    io::di, io::uo) is det.

mlds_output_data_decl_2(InitializerSize, Type, !IO) :-
    mlds_output_type_suffix(Type, InitializerSize, !IO).

:- pred mlds_output_data_decl_ho(output_type::in(output_type),
    output_type::in(output_type), mlds_qualified_entity_name::in,
    mlds_type::in, io::di, io::uo) is det.

mlds_output_data_decl_ho(OutputPrefix, OutputSuffix, Name, Type, !IO) :-
    OutputPrefix(Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_fully_qualified_name(Name, !IO),
    OutputSuffix(Type, !IO).

:- pred mlds_output_data_defn(mlds_qualified_entity_name::in, mlds_type::in,
    mlds_initializer::in, io::di, io::uo) is det.

mlds_output_data_defn(Name, Type, Initializer, !IO) :-
    mlds_output_data_decl(Name, Type, initializer_array_size(Initializer),
        !IO),
    mlds_output_initializer(Type, Initializer, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_maybe(maybe(T)::in,
    pred(T, io, io)::in(pred(in, di, uo) is det), io::di, io::uo) is det.

mlds_output_maybe(MaybeValue, OutputAction, !IO) :-
    (
        MaybeValue = yes(Value),
        OutputAction(Value, !IO)
    ;
        MaybeValue = no
    ).

:- pred mlds_output_initializer(mlds_type::in, mlds_initializer::in,
    io::di, io::uo) is det.

mlds_output_initializer(_Type, Initializer, !IO) :-
    ( mlds_needs_initialization(Initializer) = yes ->
        io.write_string(" = ", !IO),
        mlds_output_initializer_body(0, Initializer, !IO)
    ;
        true
    ).

:- func mlds_needs_initialization(mlds_initializer) = bool.

mlds_needs_initialization(no_initializer) = no.
mlds_needs_initialization(init_obj(_)) = yes.
mlds_needs_initialization(init_struct(_Type, [])) = no.
mlds_needs_initialization(init_struct(_Type, [_|_])) = yes.
mlds_needs_initialization(init_array(_)) = yes.

:- pred mlds_output_initializer_body(int::in, mlds_initializer::in,
    io::di, io::uo) is det.

mlds_output_initializer_body(_, no_initializer, !IO).
mlds_output_initializer_body(Indent, init_obj(Rval), !IO) :-
    mlds_indent(Indent, !IO),
    mlds_output_rval(Rval, !IO).
mlds_output_initializer_body(Indent, init_struct(_Type, FieldInits), !IO) :-
    % Note that standard ANSI/ISO C does not allow empty structs. But it is
    % the responsibility of the MLDS code generator to not generate any.
    % So we don't need to handle empty initializers specially here.
    mlds_indent(Indent, !IO),
    io.write_string("{\n", !IO),
    io.write_list(FieldInits, ",\n", mlds_output_initializer_body(Indent + 1),
        !IO),
    io.write_string("}", !IO).
mlds_output_initializer_body(Indent, init_array(ElementInits), !IO) :-
    % Standard ANSI/ISO C does not allow empty arrays. But the MLDS does.
    % To keep the C compiler happy, we therefore convert zero-element MLDS
    % arrays into one-element C arrays. (The extra element is a minor waste
    % of space, but it will otherwise be ignored.) So if the initializer list
    % here is empty, we need to output a single initializer. We can initialize
    % the extra element with any value; we use "0", since that is a valid
    % initializer for any type.
    io.write_string("{\n", !IO),
    (
        ElementInits = [],
        mlds_indent(Indent, !IO),
        io.write_string("0", !IO)
    ;
        ElementInits = [_ | _],
        io.write_list(ElementInits, ",\n",
            mlds_output_initializer_body(Indent + 1), !IO)
    ),
    io.write_string(" }", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions
%

:- pred mlds_output_pred_proc_id(pred_proc_id::in, io::di, io::uo) is det.

mlds_output_pred_proc_id(proc(PredId, ProcId), !IO) :-
    globals.io_lookup_bool_option(auto_comments, AddComments, !IO),
    (
        AddComments = yes,
        io.write_string("/* pred_id: ", !IO),
        pred_id_to_int(PredId, PredIdNum),
        io.write_int(PredIdNum, !IO),
        io.write_string(", proc_id: ", !IO),
        proc_id_to_int(ProcId, ProcIdNum),
        io.write_int(ProcIdNum, !IO),
        io.write_string(" */\n", !IO)
    ;
        AddComments = no
    ).

:- pred mlds_output_func(indent::in, mlds_qualified_entity_name::in,
    mlds_context::in, mlds_func_params::in, mlds_function_body::in,
    io::di, io::uo) is det.

mlds_output_func(Indent, Name, Context, Params, FunctionBody, !IO) :-
    mlds_output_func_decl(Indent, Name, Context, Params, !IO),
    (
        FunctionBody = body_external,
        io.write_string(";\n", !IO)
    ;
        FunctionBody = body_defined_here(Body),
        io.write_string("\n", !IO),

        mlds_indent(Context, Indent, !IO),
        io.write_string("{\n", !IO),

        mlds_maybe_output_time_profile_instr(Context, Indent + 1, Name, !IO),

        Signature = mlds_get_func_signature(Params),
        FuncInfo = func_info(Name, Signature),
        mlds_output_statement(Indent + 1, FuncInfo, Body, !IO),

        mlds_indent(Context, Indent, !IO),
        io.write_string("}\n", !IO)    % end the function
    ).

:- pred mlds_output_func_decl(indent::in, mlds_qualified_entity_name::in,
    mlds_context::in, mlds_func_params::in, io::di, io::uo) is det.

mlds_output_func_decl(Indent, QualifiedName, Context, Signature, !IO) :-
    CallingConvention = "MR_CALL ",
    mlds_output_func_decl_ho(Indent, QualifiedName, Context,
        CallingConvention, Signature,
        mlds_output_type_prefix, mlds_output_type_suffix, !IO).

:- pred mlds_output_func_decl_ho(indent::in, mlds_qualified_entity_name::in,
    mlds_context::in, string::in, mlds_func_params::in,
    output_type::in(output_type), output_type::in(output_type),
    io::di, io::uo) is det.

mlds_output_func_decl_ho(Indent, QualifiedName, Context, CallingConvention,
        Signature, OutputPrefix, OutputSuffix, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        OutputPrefix(RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(RetTypes,
            mlds_output_prefix_suffix(OutputPrefix, OutputSuffix), !IO)
    ),
    io.write_char(' ', !IO),
    io.write_string(CallingConvention, !IO),
    io.nl(!IO),
    mlds_output_fully_qualified_name(QualifiedName, !IO),
    QualifiedName = qual(ModuleName, _, _),
    mlds_output_params(OutputPrefix, OutputSuffix,
        Indent, ModuleName, Context, Parameters, !IO),
    ( RetTypes = [RetType2] ->
        OutputSuffix(RetType2, !IO)
    ;
        true
    ).

:- pred mlds_output_prefix_suffix(output_type::in(output_type),
    output_type::in(output_type), mlds_type::in, io::di, io::uo) is det.

mlds_output_prefix_suffix(OutputPrefix, OutputSuffix, Value, !IO) :-
    OutputPrefix(Value, !IO),
    OutputSuffix(Value, !IO).

:- pred mlds_output_params(output_type::in(output_type),
    output_type::in(output_type), indent::in, mlds_module_name::in,
    mlds_context::in, mlds_arguments::in, io::di, io::uo) is det.

mlds_output_params(OutputPrefix, OutputSuffix, Indent, ModuleName,
        Context, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = [],
        io.write_string("void", !IO)
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            mlds_output_param(OutputPrefix, OutputSuffix,
                Indent + 1, ModuleName, Context), !IO)
    ),
    io.write_char(')', !IO).

:- pred mlds_output_param(output_type::in(output_type),
    output_type::in(output_type), indent::in, mlds_module_name::in,
    mlds_context::in, mlds_argument::in, io::di, io::uo) is det.

mlds_output_param(OutputPrefix, OutputSuffix, Indent, ModuleName, Context,
        Arg, !IO) :-
    Arg = mlds_argument(Name, Type, Maybe_GC_TraceCode),
    QualName = qual(ModuleName, module_qual, Name),
    mlds_indent(Context, Indent, !IO),
    mlds_output_data_decl_ho(OutputPrefix, OutputSuffix, QualName, Type, !IO),
    mlds_output_maybe_gc_trace_code(Indent, QualName, Maybe_GC_TraceCode,
        "\n", !IO).

:- pred mlds_output_func_type_prefix(mlds_func_params::in, io::di, io::uo)
    is det.

mlds_output_func_type_prefix(Params, !IO) :-
    Params = mlds_func_params(_Parameters, RetTypes),
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        mlds_output_type(RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(RetTypes, mlds_output_type, !IO)
    ),
    % Note that mlds_func_type actually corresponds to a function _pointer_
    % type in C. This is necessary because function types in C are not first
    % class.
    io.write_string(" MR_CALL (*", !IO).

:- pred mlds_output_func_type_suffix(mlds_func_params::in, io::di, io::uo)
    is det.

mlds_output_func_type_suffix(Params, !IO) :-
    Params = mlds_func_params(Parameters, _RetTypes),
    io.write_string(")", !IO),
    mlds_output_param_types(Parameters, !IO).

:- pred mlds_output_param_types(mlds_arguments::in, io::di, io::uo) is det.

mlds_output_param_types(Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = [],
        io.write_string("void", !IO)
    ;
        Parameters = [_ | _],
        io.write_list(Parameters, ", ", mlds_output_param_type, !IO)
    ),
    io.write_char(')', !IO).

:- pred mlds_output_param_type(mlds_argument::in, io::di, io::uo) is det.

mlds_output_param_type(mlds_argument(_Name, Type, _GC_TraceCode), !IO) :-
    mlds_output_type(Type, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities
%

:- pred mlds_output_fully_qualified_name(mlds_qualified_entity_name::in,
    io::di, io::uo) is det.

mlds_output_fully_qualified_name(QualifiedName, !IO) :-
    QualifiedName = qual(_ModuleName, _QualKind, Name),
    (
        (
            % Don't module-qualify main/2.
            Name = entity_function(PredLabel, _, _, _),
            PredLabel = mlds_user_pred_label(predicate, no, "main", 2,
                model_det, no)
        ;
            Name = entity_data(mlds_rtti(RttiId)),
            module_qualify_name_of_rtti_id(RttiId) = no
        ;
            % We don't module qualify pragma export names.
            Name = entity_export(_)
        )
    ->
        mlds_output_name(Name, !IO)
    ;
        mlds_output_fully_qualified(QualifiedName, mlds_output_name, !IO)
    ).

:- pred mlds_output_fully_qualified_proc_label(mlds_qualified_proc_label::in,
    io::di, io::uo) is det.

mlds_output_fully_qualified_proc_label(QualifiedName, !IO) :-
    (
        % Don't module-qualify main/2.
        QualifiedName = qual(_ModuleName, _QualKind, Name),
        Name = mlds_proc_label(PredLabel, _ProcId),
        PredLabel = mlds_user_pred_label(predicate, no, "main", 2,
            model_det, no)
    ->
        mlds_output_proc_label(Name, !IO)
    ;
        mlds_output_fully_qualified(QualifiedName, mlds_output_proc_label, !IO)
    ).

:- pred mlds_output_fully_qualified(mlds_fully_qualified_name(T)::in,
    pred(T, io, io)::in(pred(in, di, uo) is det), io::di, io::uo) is det.

mlds_output_fully_qualified(qual(ModuleName, _QualKind, Name), OutputFunc,
        !IO) :-
    SymName = mlds_module_name_to_sym_name(ModuleName),
    MangledModuleName = sym_name_mangle(SymName),
    io.write_string(MangledModuleName, !IO),
    io.write_string("__", !IO),
    OutputFunc(Name, !IO).

:- pred mlds_output_module_name(mercury_module_name::in, io::di, io::uo)
    is det.

mlds_output_module_name(ModuleName, !IO) :-
    MangledModuleName = sym_name_mangle(ModuleName),
    io.write_string(MangledModuleName, !IO).

:- pred mlds_output_name(mlds_entity_name::in, io::di, io::uo) is det.

    % XXX We should avoid appending the arity, modenum, and seqnum
    % if they are not needed.
mlds_output_name(entity_type(Name, Arity), !IO) :-
    MangledName = name_mangle(Name),
    io.format("%s_%d", [s(MangledName), i(Arity)], !IO).
mlds_output_name(entity_data(DataName), !IO) :-
    mlds_output_data_name(DataName, !IO).
mlds_output_name(entity_function(PredLabel, ProcId, MaybeSeqNum, _PredId),
        !IO) :-
    mlds_output_pred_label(PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.format("_%d", [i(ModeNum)], !IO),
    (
        MaybeSeqNum = yes(SeqNum),
        io.format("_%d", [i(SeqNum)], !IO)
    ;
        MaybeSeqNum = no
    ).
mlds_output_name(entity_export(Name), !IO) :-
    io.write_string(Name, !IO).

    % mlds_output_pred_label should be kept in sync with
    % mlds_pred_label_to_string.
    %
:- pred mlds_output_pred_label(mlds_pred_label::in, io::di, io::uo) is det.

mlds_output_pred_label(mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
        Name, Arity, _CodeModel, _NonOutputFunc), !IO) :-
    ( PredOrFunc = predicate, Suffix = "p"
    ; PredOrFunc = function, Suffix = "f"
    ),
    MangledName = name_mangle(Name),
    io.format("%s_%d_%s", [s(MangledName), i(Arity), s(Suffix)], !IO),
    (
        MaybeDefiningModule = yes(DefiningModule),
        io.write_string("_in__", !IO),
        mlds_output_module_name(DefiningModule, !IO)
    ;
        MaybeDefiningModule = no
    ).
mlds_output_pred_label(mlds_special_pred_label(PredName, MaybeTypeModule,
        TypeName, TypeArity), !IO) :-
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
    io.write_int(TypeArity, !IO).

    % mlds_pred_label_to_string should be kept in sync with
    % mlds_output_pred_label.
    %
:- func mlds_pred_label_to_string(mlds_pred_label) = string.

mlds_pred_label_to_string(mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
        Name, Arity, _CodeModel, _NonOutputFunc)) = Str :-
    ( PredOrFunc = predicate, Suffix = "p"
    ; PredOrFunc = function, Suffix = "f"
    ),
    MangledName = name_mangle(Name),
    MainStr = string.format("%s_%d_%s", [s(MangledName), i(Arity), s(Suffix)]),
    (
        MaybeDefiningModule = yes(DefiningModule),
        Str = MainStr ++ "_in__" ++ sym_name_mangle(DefiningModule)
    ;
        MaybeDefiningModule = no,
        Str = MainStr
    ).
mlds_pred_label_to_string(mlds_special_pred_label(PredName, MaybeTypeModule,
        TypeName, TypeArity)) = Str :-
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
        int_to_string(TypeArity).

:- pred mlds_output_data_name(mlds_data_name::in, io::di, io::uo) is det.

mlds_output_data_name(var(Name), !IO) :-
    mlds_output_mangled_name(ml_var_name_to_string(Name), !IO).
mlds_output_data_name(mlds_common(Num), !IO) :-
    io.write_string("common_", !IO),
    io.write_int(Num, !IO).
mlds_output_data_name(mlds_rtti(RttiId), !IO) :-
    rtti.id_to_c_identifier(RttiId, RttiAddrName),
    io.write_string(RttiAddrName, !IO).
mlds_output_data_name(mlds_module_layout, !IO) :-
    sorry(this_file, "NYI: module_layout").
mlds_output_data_name(mlds_proc_layout(_ProcLabel), !IO) :-
    sorry(this_file, "NYI: proc_layout").
mlds_output_data_name(mlds_internal_layout(_ProcLabel, _FuncSeqNum), !IO) :-
    sorry(this_file, "NYI: internal_layout").
mlds_output_data_name(mlds_tabling_ref(ProcLabel, Id), !IO) :-
    io.write_string(mlds_tabling_data_name(ProcLabel, Id), !IO).

mlds_tabling_data_name(ProcLabel, Id) =
    tabling_info_id_str(Id) ++ "_for_" ++
        mlds_proc_label_to_string(mlds_std_tabling_proc_label(ProcLabel)).

%-----------------------------------------------------------------------------%
%
% Code to output types
%

:- pred mlds_output_type(mlds_type::in, io::di, io::uo) is det.

mlds_output_type(Type, !IO) :-
    % Because of the joys of C syntax, the code for outputting types
    % needs to be split into two parts; first the prefix, i.e. the part
    % of the type name that goes before the variable name in a variable
    % declaration, and then the suffix, i.e. the part which goes after
    % the variable name, e.g. the "[]" for array types.

    mlds_output_type_prefix(Type, !IO),
    mlds_output_type_suffix(Type, !IO).

:- pred mlds_output_type_prefix(mlds_type::in, io::di, io::uo) is det.

mlds_output_type_prefix(mercury_type(Type, TypeCategory, _), !IO) :-
    mlds_output_mercury_type_prefix(Type, TypeCategory, !IO).
mlds_output_type_prefix(mlds_mercury_array_type(_ElemType), !IO) :-
    globals.io_lookup_bool_option(highlevel_data, HighLevelData, !IO),
    (
        HighLevelData = yes,
        mlds_output_mercury_user_type_name(
            type_ctor(qualified(unqualified("array"), "array"), 1),
            type_cat_user_ctor, !IO)
    ;
        HighLevelData = no,
        io.write_string("MR_ArrayPtr", !IO)
    ).
mlds_output_type_prefix(mlds_native_int_type, !IO) :-
    io.write_string("int", !IO).
mlds_output_type_prefix(mlds_native_float_type, !IO) :-
    io.write_string("float", !IO).
mlds_output_type_prefix(mlds_native_bool_type, !IO) :-
    io.write_string("MR_bool", !IO).
mlds_output_type_prefix(mlds_native_char_type, !IO) :-
    io.write_string("char", !IO).
mlds_output_type_prefix(mlds_foreign_type(_ForeignType), !IO) :-
    % For binary compatibility with the --target asm back-end,
    % we need to output these as a generic type, rather than making
    % use of the C type name
    io.write_string("MR_Box", !IO).
mlds_output_type_prefix(mlds_class_type(Name, Arity, ClassKind), !IO) :-
    ( ClassKind = mlds_enum ->
        % We can't just use the enumeration type, since the enumeration type's
        % definition is not guaranteed to be in scope at this point. (Fixing
        % that would be somewhat complicated; it would require writing enum
        % definitions to a separate header file.) Also the enumeration might
        % not be word-sized, which would cause problems for e.g.
        % `std_util.arg/2'. So we just use `MR_Integer', and output the
        % actual enumeration type as a comment.

        io.write_string("MR_Integer /* actually `enum ", !IO),
        mlds_output_fully_qualified(Name, mlds_output_mangled_name, !IO),
        io.format("_%d_e", [i(Arity)], !IO),
        io.write_string("' */", !IO)
    ;
        % For struct types it's OK to output an incomplete type,
        % since don't use these types directly, we only use pointers to them.
        io.write_string("struct ", !IO),
        mlds_output_fully_qualified(Name, mlds_output_mangled_name, !IO),
        io.format("_%d_s", [i(Arity)], !IO)
    ).
mlds_output_type_prefix(mlds_ptr_type(Type), !IO) :-
    mlds_output_type(Type, !IO),
    io.write_string(" *", !IO).
mlds_output_type_prefix(mlds_array_type(Type), !IO) :-
    % Here we just output the element type. The "[]" goes in the type suffix.
    mlds_output_type(Type, !IO).
mlds_output_type_prefix(mlds_func_type(FuncParams), !IO) :-
    mlds_output_func_type_prefix(FuncParams, !IO).
mlds_output_type_prefix(mlds_generic_type, !IO) :-
    io.write_string("MR_Box", !IO).
mlds_output_type_prefix(mlds_generic_env_ptr_type, !IO) :-
    io.write_string("void *", !IO).
mlds_output_type_prefix(mlds_type_info_type, !IO) :-
    io.write_string("MR_TypeInfo", !IO).
mlds_output_type_prefix(mlds_pseudo_type_info_type, !IO) :-
    io.write_string("MR_PseudoTypeInfo", !IO).
mlds_output_type_prefix(mlds_cont_type(ArgTypes), !IO) :-
    (
        ArgTypes = [],
        globals.io_lookup_bool_option(gcc_nested_functions, GCC_NestedFuncs,
            !IO),
        (
            GCC_NestedFuncs = yes,
            io.write_string("MR_NestedCont", !IO)
        ;
            GCC_NestedFuncs = no,
            io.write_string("MR_Cont", !IO)
        )
    ;
        ArgTypes = [_ | _],
        % This case only happens for --nondet-copy-out
        io.write_string("void MR_CALL (*", !IO)
    ).
mlds_output_type_prefix(mlds_commit_type, !IO) :-
    globals.io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels, !IO),
    (
        GCC_LocalLabels = yes,
        io.write_string("__label__", !IO)
    ;
        GCC_LocalLabels = no,
        io.write_string("jmp_buf", !IO)
    ).
mlds_output_type_prefix(mlds_rtti_type(RttiIdMaybeElement), !IO) :-
    rtti_id_maybe_element_c_type(RttiIdMaybeElement, CType, _IsArray),
    io.write_string(CType, !IO).
mlds_output_type_prefix(mlds_tabling_type(TablingId), !IO) :-
    tabling_id_c_type(TablingId, CType, _IsArray),
    io.write_string(CType, !IO).
mlds_output_type_prefix(mlds_unknown_type, !IO) :-
    unexpected(this_file, "prefix has unknown type").

:- pred mlds_output_mercury_type_prefix(mer_type::in, type_category::in,
    io::di, io::uo) is det.

mlds_output_mercury_type_prefix(Type, TypeCategory, !IO) :-
    (
        TypeCategory = type_cat_char,
        io.write_string("MR_Char", !IO)
    ;
        TypeCategory = type_cat_int,
        io.write_string("MR_Integer", !IO)
    ;
        TypeCategory = type_cat_string,
        io.write_string("MR_String", !IO)
    ;
        TypeCategory = type_cat_float,
        io.write_string("MR_Float", !IO)
    ;
        TypeCategory = type_cat_void,
        io.write_string("MR_Word", !IO)
    ;
        TypeCategory = type_cat_variable,
        io.write_string("MR_Box", !IO)
    ;
        TypeCategory = type_cat_type_info,
        % runtime/mercury_hlc_types requires typeclass_infos
        % to be treated as user defined types.
        mlds_output_mercury_user_type_prefix(Type, type_cat_user_ctor, !IO)
    ;
        TypeCategory = type_cat_type_ctor_info,
        % runtime/mercury_hlc_types requires typeclass_infos
        % to be treated as user defined types.
        mlds_output_mercury_user_type_prefix(Type, type_cat_user_ctor, !IO)
    ;
        TypeCategory = type_cat_typeclass_info,
        % runtime/mercury_hlc_types requires typeclass_infos
        % to be treated as user defined types.
        mlds_output_mercury_user_type_prefix(Type, type_cat_user_ctor, !IO)
    ;
        TypeCategory = type_cat_base_typeclass_info,
        % runtime/mercury_hlc_types requires typeclass_infos
        % to be treated as user defined types.
        mlds_output_mercury_user_type_prefix(Type, type_cat_user_ctor, !IO)
    ;
        TypeCategory = type_cat_tuple,
        io.write_string("MR_Tuple", !IO)
    ;
        TypeCategory = type_cat_higher_order,
        globals.io_lookup_bool_option(highlevel_data, HighLevelData, !IO),
        (
            HighLevelData = yes,
            io.write_string("MR_ClosurePtr", !IO)
        ;
            HighLevelData = no,
            io.write_string("MR_Word", !IO)
        )
    ;
        TypeCategory = type_cat_enum,
        mlds_output_mercury_user_type_prefix(Type, TypeCategory, !IO)
    ;
        TypeCategory = type_cat_dummy,
        mlds_output_mercury_user_type_prefix(Type, TypeCategory, !IO)
    ;
        TypeCategory = type_cat_user_ctor,
        mlds_output_mercury_user_type_prefix(Type, TypeCategory, !IO)
    ).

:- pred mlds_output_mercury_user_type_prefix(mer_type::in,
    type_category::in, io::di, io::uo) is det.

mlds_output_mercury_user_type_prefix(Type, TypeCategory, !IO) :-
    globals.io_lookup_bool_option(highlevel_data, HighLevelData, !IO),
    (
        HighLevelData = yes,
        ( type_to_ctor_and_args(Type, TypeCtor, _ArgsTypes) ->
            mlds_output_mercury_user_type_name(TypeCtor, TypeCategory, !IO)
        ;
            unexpected(this_file, "mlds_output_mercury_user_type_prefix")
        )
    ;
        HighLevelData = no,
        % In this case, we just treat everything as `MR_Word'.
        io.write_string("MR_Word", !IO)
    ).

:- pred mlds_output_mercury_user_type_name(type_ctor::in, type_category::in,
    io::di, io::uo) is det.

mlds_output_mercury_user_type_name(TypeCtor, TypeCategory, !IO) :-
    ml_gen_type_name(TypeCtor, ClassName, ClassArity),
    ( TypeCategory = type_cat_enum ->
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum)
    ;
        MLDS_Type = mlds_ptr_type(
            mlds_class_type(ClassName, ClassArity, mlds_class))
    ),
    mlds_output_type_prefix(MLDS_Type, !IO).

:- pred mlds_output_type_suffix(mlds_type::in, io::di, io::uo) is det.

mlds_output_type_suffix(Type, !IO) :-
    mlds_output_type_suffix(Type, no_size, !IO).

:- type initializer_array_size
    --->    array_size(int)
    ;       no_size.            % Either the size is unknown,
                                % or the data is not an array.

:- func initializer_array_size(mlds_initializer) = initializer_array_size.

initializer_array_size(no_initializer) = no_size.
initializer_array_size(init_obj(_)) = no_size.
initializer_array_size(init_struct(_, _)) = no_size.
initializer_array_size(init_array(Elems)) = array_size(list.length(Elems)).

:- pred mlds_output_type_suffix(mlds_type::in, initializer_array_size::in,
    io::di, io::uo) is det.

mlds_output_type_suffix(mercury_type(_, _, _), _, !IO).
mlds_output_type_suffix(mlds_mercury_array_type(_), _, !IO).
mlds_output_type_suffix(mlds_native_int_type, _, !IO).
mlds_output_type_suffix(mlds_native_float_type, _, !IO).
mlds_output_type_suffix(mlds_native_bool_type, _, !IO).
mlds_output_type_suffix(mlds_native_char_type, _, !IO).
    % XXX Currently we can't output a type suffix.
mlds_output_type_suffix(mlds_foreign_type(_), _, !IO).
mlds_output_type_suffix(mlds_class_type(_, _, _), _, !IO).
mlds_output_type_suffix(mlds_ptr_type(_), _, !IO).
mlds_output_type_suffix(mlds_array_type(_), ArraySize, !IO) :-
    mlds_output_array_type_suffix(ArraySize, !IO).
mlds_output_type_suffix(mlds_func_type(FuncParams), _, !IO) :-
    mlds_output_func_type_suffix(FuncParams, !IO).
mlds_output_type_suffix(mlds_generic_type, _, !IO).
mlds_output_type_suffix(mlds_generic_env_ptr_type, _, !IO).
mlds_output_type_suffix(mlds_type_info_type, _, !IO).
mlds_output_type_suffix(mlds_pseudo_type_info_type, _, !IO).
mlds_output_type_suffix(mlds_cont_type(ArgTypes), _, !IO) :-
    (
        ArgTypes = []
    ;
        ArgTypes = [_ | _],
        % This case only happens for --nondet-copy-out.
        io.write_string(")(", !IO),
        io.write_list(ArgTypes, ", ", mlds_output_type, !IO),
        % add the type for the environment parameter, if needed
        globals.io_lookup_bool_option(gcc_nested_functions, GCC_NestedFuncs,
            !IO),
        (
            GCC_NestedFuncs = no,
            io.write_string(", void *", !IO)
        ;
            GCC_NestedFuncs = yes
        ),
        io.write_string(")", !IO)
    ).
mlds_output_type_suffix(mlds_commit_type, _, !IO).
mlds_output_type_suffix(mlds_rtti_type(RttiIdMaybeElement), ArraySize, !IO) :-
    IsArrayType = rtti_id_maybe_element_has_array_type(RttiIdMaybeElement),
    (
        IsArrayType = yes,
        mlds_output_array_type_suffix(ArraySize, !IO)
    ;
        IsArrayType = no
    ).
mlds_output_type_suffix(mlds_tabling_type(TablingId), ArraySize, !IO) :-
    IsArrayType = tabling_id_has_array_type(TablingId),
    (
        IsArrayType = yes,
        mlds_output_array_type_suffix(ArraySize, !IO)
    ;
        IsArrayType = no
    ).
mlds_output_type_suffix(mlds_unknown_type, _, !IO) :-
    unexpected(this_file, "mlds_output_type_suffix: unknown_type").

:- pred mlds_output_array_type_suffix(initializer_array_size::in,
    io::di, io::uo) is det.

mlds_output_array_type_suffix(no_size, !IO) :-
    io.write_string("[]", !IO).
mlds_output_array_type_suffix(array_size(Size0), !IO) :-
    % Standard ANSI/ISO C does not allow arrays of size 0. But the MLDS does.
    % To keep the C compiler happy, we therefore convert zero-element MLDS
    % arrays into one-element C arrays.
    int.max(Size0, 1, Size),
    io.format("[%d]", [i(Size)], !IO).

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers
%

:- type decl_or_defn
    --->    forward_decl
    ;       definition.

:- pred mlds_output_decl_flags(mlds_decl_flags::in, decl_or_defn::in,
    mlds_entity_name::in, mlds_entity_defn::in, io::di, io::uo) is det.

mlds_output_decl_flags(Flags, DeclOrDefn, Name, DefnBody, !IO) :-
    % mlds_output_extern_or_static handles both the `access' and the
    % `per_instance' fields of the mlds_decl_flags. We have to handle them
    % together because C overloads `static' to mean both `private' and
    % `one_copy', rather than having separate keywords for each. To make it
    % clear which MLDS construct each `static' keyword means, we precede the
    % `static' without (optionally-enabled) comments saying whether it is
    % `private', `one_copy', or both.
    %
    mlds_output_access_comment(access(Flags), !IO),
    mlds_output_per_instance_comment(per_instance(Flags), !IO),
    mlds_output_extern_or_static(access(Flags), per_instance(Flags),
        DeclOrDefn, Name, DefnBody, !IO),
    mlds_output_virtuality(virtuality(Flags), !IO),
    mlds_output_finality(finality(Flags), !IO),
    mlds_output_constness(constness(Flags), !IO),
    mlds_output_abstractness(abstractness(Flags), !IO).

:- pred mlds_output_access_comment(access::in, io::di, io::uo) is det.

mlds_output_access_comment(Access, !IO) :-
    globals.io_lookup_bool_option(auto_comments, Comments, !IO),
    (
        Comments = yes,
        mlds_output_access_comment_2(Access, !IO)
    ;
        Comments = no
    ).

:- pred mlds_output_access_comment_2(access::in, io::di, io::uo) is det.

mlds_output_access_comment_2(public, !IO) :-
    io.write_string("/* public: */ ", !IO).
mlds_output_access_comment_2(private, !IO) :-
    io.write_string("/* private: */ ", !IO).
mlds_output_access_comment_2(protected, !IO) :-
    io.write_string("/* protected: */ ", !IO).
mlds_output_access_comment_2(default, !IO) :-
    io.write_string("/* default access */ ", !IO).
mlds_output_access_comment_2(local, !IO) :-
    io.write_string("/* local: */ ", !IO).

:- pred mlds_output_per_instance_comment(per_instance::in, io::di, io::uo)
    is det.

mlds_output_per_instance_comment(PerInstance, !IO) :-
    globals.io_lookup_bool_option(auto_comments, Comments, !IO),
    (
        Comments = yes,
        mlds_output_per_instance_comment_2(PerInstance, !IO)
    ;
        Comments = no
    ).

:- pred mlds_output_per_instance_comment_2(per_instance::in, io::di, io::uo)
    is det.

mlds_output_per_instance_comment_2(per_instance, !IO).
mlds_output_per_instance_comment_2(one_copy, !IO) :-
    io.write_string("/* one_copy */ ", !IO).

:- pred mlds_output_extern_or_static(access::in, per_instance::in,
    decl_or_defn::in, mlds_entity_name::in, mlds_entity_defn::in,
    io::di, io::uo) is det.

mlds_output_extern_or_static(Access, PerInstance, DeclOrDefn, Name, DefnBody,
        !IO) :-
    (
        (
            Access = private
        ;
            Access = local,
            PerInstance = one_copy
        ),
        Name \= entity_type(_, _),
        % Don't output "static" for functions that don't have a body.
        % This can happen for Mercury procedures declared `:- external'
        DefnBody \= mlds_function(_, _, body_external, _, _)
    ->
        io.write_string("static ", !IO)
    ;
        DeclOrDefn = forward_decl,
        Name = entity_data(_)
    ->
        io.write_string("extern ", !IO)
    ;
        % Forward declarations for GNU C nested functions need to be prefixed
        % with "auto".
        DeclOrDefn = forward_decl,
        Name = entity_function(_, _, _, _),
        Access = local
    ->
        io.write_string("auto ", !IO)
    ;
        true
    ).

:- pred mlds_output_virtuality(virtuality::in, io::di, io::uo) is det.

mlds_output_virtuality(virtual, !IO) :-
    io.write_string("virtual ", !IO).
mlds_output_virtuality(non_virtual, !IO).

:- pred mlds_output_finality(finality::in, io::di, io::uo) is det.

mlds_output_finality(final, !IO) :-
    io.write_string("/* final */ ", !IO).
mlds_output_finality(overridable, !IO).

:- pred mlds_output_constness(constness::in, io::di, io::uo) is det.

mlds_output_constness(const, !IO) :-
    io.write_string("const ", !IO).
mlds_output_constness(modifiable, !IO).

:- pred mlds_output_abstractness(abstractness::in, io::di, io::uo) is det.

mlds_output_abstractness(abstract, !IO) :-
    io.write_string("/* abstract */ ", !IO).
mlds_output_abstractness(concrete, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output statements
%

:- type func_info
    --->    func_info(mlds_qualified_entity_name, mlds_func_signature).

:- pred mlds_output_statements(indent::in, func_info::in,
    list(statement)::in, io::di, io::uo) is det.

mlds_output_statements(Indent, FuncInfo, Statements, !IO) :-
    list.foldl(mlds_output_statement(Indent, FuncInfo), Statements, !IO).

:- pred mlds_output_statement(indent::in, func_info::in, statement::in,
    io::di, io::uo) is det.

mlds_output_statement(Indent, FuncInfo, statement(Statement, Context),
        !IO) :-
    output_context(Context, !IO),
    mlds_output_stmt(Indent, FuncInfo, Statement, Context, !IO).

:- pred mlds_output_stmt(indent::in, func_info::in, mlds_stmt::in,
    mlds_context::in, io::di, io::uo) is det.

mlds_output_stmt(Indent, FuncInfo, block(Defns, Statements), Context, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("{\n", !IO),
    (
        Defns = [_ | _],
        FuncInfo = func_info(FuncName, _),
        FuncName = qual(ModuleName, _, _),

        % Output forward declarations for any nested functions defined in
        % this block, in case they are referenced before they are defined.
        list.filter(defn_is_function, Defns, NestedFuncDefns),
        (
            NestedFuncDefns = [_ | _],
            mlds_output_decls(Indent + 1, ModuleName, NestedFuncDefns, !IO),
            io.write_string("\n", !IO)
        ;
            NestedFuncDefns = []
        ),

        mlds_output_defns(Indent + 1, no, ModuleName, Defns, !IO),
        io.write_string("\n", !IO)
    ;
        Defns = []
    ),
    mlds_output_statements(Indent + 1, FuncInfo, Statements, !IO),
    mlds_indent(Context, Indent, !IO),
    io.write_string("}\n", !IO).

mlds_output_stmt(Indent, FuncInfo, while(Cond, Statement, no), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("while (", !IO),
    mlds_output_rval(Cond, !IO),
    io.write_string(")\n", !IO),
    mlds_output_statement(Indent + 1, FuncInfo, Statement, !IO).
mlds_output_stmt(Indent, FuncInfo, while(Cond, Statement, yes), Context,
        !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("do\n", !IO),
    mlds_output_statement(Indent + 1, FuncInfo, Statement, !IO),
    mlds_indent(Context, Indent, !IO),
    io.write_string("while (", !IO),
    mlds_output_rval(Cond, !IO),
    io.write_string(");\n", !IO).

mlds_output_stmt(Indent, FuncInfo, if_then_else(Cond, Then0, MaybeElse),
        Context, !IO) :-
    % We need to take care to avoid problems caused by the dangling else
    % ambiguity.
    (
        % For examples of the form
        %
        %   if (...)
        %       if (...)
        %           ...
        %   else
        %       ...
        %
        % we need braces around the inner `if', otherwise they wouldn't parse
        % they way we want them to: C would match the `else' with the inner
        % `if' rather than the outer `if'.

        MaybeElse = yes(_),
        Then0 = statement(if_then_else(_, _, no), ThenContext)
    ->
        Then = statement(block([], [Then0]), ThenContext)
    ;
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
        Then0 = statement(if_then_else(_, _, yes(_)), ThenContext)
    ->
        Then = statement(block([], [Then0]), ThenContext)
    ;
        Then = Then0
    ),

    mlds_indent(Indent, !IO),
    io.write_string("if (", !IO),
    mlds_output_rval(Cond, !IO),
    io.write_string(")\n", !IO),
    mlds_output_statement(Indent + 1, FuncInfo, Then, !IO),
    (
        MaybeElse = yes(Else),
        mlds_indent(Context, Indent, !IO),
        io.write_string("else\n", !IO),
        mlds_output_statement(Indent + 1, FuncInfo, Else, !IO)
    ;
        MaybeElse = no
    ).
mlds_output_stmt(Indent, FuncInfo, switch(_Type, Val, _Range, Cases, Default),
        Context, !IO) :-
    mlds_indent(Context, Indent, !IO),
    io.write_string("switch (", !IO),
    mlds_output_rval(Val, !IO),
    io.write_string(") {\n", !IO),
    % we put the default case first, so that if it is unreachable,
    % it will get merged in with the first case.
    mlds_output_switch_default(Indent + 1, FuncInfo, Context, Default, !IO),
    list.foldl(mlds_output_switch_case(Indent + 1, FuncInfo, Context),
        Cases, !IO),
    mlds_indent(Context, Indent, !IO),
    io.write_string("}\n", !IO).

mlds_output_stmt(Indent, _FuncInfo, label(LabelName), _, !IO) :-
    % Note: MLDS allows labels at the end of blocks. C doesn't. Hence we need
    % to insert a semi-colon after the colon to ensure that there is a
    % statement to attach the label to.

    mlds_indent(Indent - 1, !IO),
    mlds_output_label_name(LabelName, !IO),
    io.write_string(":;\n", !IO).
mlds_output_stmt(Indent, _FuncInfo, goto(label(LabelName)), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("goto ", !IO),
    mlds_output_label_name(LabelName, !IO),
    io.write_string(";\n", !IO).
mlds_output_stmt(Indent, _FuncInfo, goto(break), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("break;\n", !IO).
mlds_output_stmt(Indent, _FuncInfo, goto(continue), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("continue;\n", !IO).
mlds_output_stmt(Indent, _FuncInfo, computed_goto(Expr, Labels), Context,
        !IO) :-
    % XXX For GNU C, we could output potentially more efficient code
    % by using an array of labels; this would tell the compiler that
    % it didn't need to do any range check.
    mlds_indent(Indent, !IO),
    io.write_string("switch (", !IO),
    mlds_output_rval(Expr, !IO),
    io.write_string(") {\n", !IO),
    list.foldl2(mlds_output_computed_goto_label(Context, Indent), Labels,
        0, _FinalCount, !IO),
    mlds_indent(Context, Indent + 1, !IO),
    io.write_string("default: /*NOTREACHED*/ MR_assert(0);\n", !IO),
    mlds_indent(Context, Indent, !IO),
    io.write_string("}\n", !IO).

mlds_output_stmt(Indent, CallerFuncInfo, Call, Context, !IO) :-
    Call = mlcall(Signature, FuncRval, MaybeObject, CallArgs,
        Results, IsTailCall),
    CallerFuncInfo = func_info(CallerName, CallerSignature),

    % We need to enclose the generated code inside an extra pair of curly
    % braces, in case we generate more than one statement (e.g. because we
    % generate extra statements for profiling or for tail call optimization)
    % and the generated code is e.g. inside an if-then-else.

    mlds_indent(Indent, !IO),
    io.write_string("{\n", !IO),

    mlds_maybe_output_call_profile_instr(Context, Indent + 1, FuncRval,
        CallerName, !IO),

    % Optimize general tail calls. We can't really do much here except to
    % insert `return' as an extra hint to the C compiler.
    % XXX These optimizations should be disable-able.
    %
    % If Results = [], i.e. the function has `void' return type, then this
    % would result in code that is not legal ANSI C (although it _is_ legal
    % in GNU C and in C++), so for that case, we put the return statement after
    % the call -- see below.
    %
    % Note that it's only safe to add such a return statement if the calling
    % procedure has the same return types as the callee, or if the calling
    % procedure has no return value. (Calls where the types are different
    % can be marked as tail calls if they are known to never return.)

    mlds_indent(Context, Indent + 1, !IO),
    Signature = mlds_func_signature(_, RetTypes),
    CallerSignature = mlds_func_signature(_, CallerRetTypes),
    (
        ( IsTailCall = tail_call
        ; IsTailCall = no_return_call
        ),
        Results = [_ | _],
        RetTypes = CallerRetTypes
    ->
        io.write_string("return ", !IO)
    ;
        true
    ),
    (
        MaybeObject = yes(Object),
        mlds_output_bracketed_rval(Object, !IO),
        io.write_string(".", !IO) % XXX should this be "->"?
    ;
        MaybeObject = no
    ),
    (
        Results = []
    ;
        Results = [Lval],
        mlds_output_lval(Lval, !IO),
        io.write_string(" = ", !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Results, mlds_output_lval, !IO),
        io.write_string(" = ", !IO)
    ),
    mlds_output_bracketed_rval(FuncRval, !IO),
    io.write_string("(", !IO),
    io.write_list(CallArgs, ", ", mlds_output_rval, !IO),
    io.write_string(");\n", !IO),

    (
        ( IsTailCall = tail_call
        ; IsTailCall = no_return_call
        ),
        CallerRetTypes = []
    ->
        mlds_indent(Context, Indent + 1, !IO),
        io.write_string("return;\n", !IO)
    ;
        mlds_maybe_output_time_profile_instr(Context, Indent + 1, CallerName,
            !IO)
    ),
    mlds_indent(Indent, !IO),
    io.write_string("}\n", !IO).

mlds_output_stmt(Indent, _FuncInfo, return(Results), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("return", !IO),
    (
        Results = []
    ;
        Results = [Rval],
        io.write_char(' ', !IO),
        mlds_output_rval(Rval, !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Results, mlds_output_rval, !IO)
    ),
    io.write_string(";\n", !IO).

mlds_output_stmt(Indent, _FuncInfo, do_commit(Ref), _, !IO) :-
    mlds_indent(Indent, !IO),
    globals.io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels, !IO),
    (
        GCC_LocalLabels = yes,
        % Output "goto <Ref>".
        io.write_string("goto ", !IO),
        mlds_output_rval(Ref, !IO)
    ;
        GCC_LocalLabels = no,
        % Output "MR_builtin_longjmp(<Ref>, 1)". This is a macro that expands
        % to either the standard longjmp() or the GNU C's __builtin_longjmp().
        % Note that the second argument to GNU C's __builtin_longjmp() *must*
        % be `1'.
        io.write_string("MR_builtin_longjmp(", !IO),
        mlds_output_rval(Ref, !IO),
        io.write_string(", 1)", !IO)
    ),
    io.write_string(";\n", !IO).
mlds_output_stmt(Indent, FuncInfo, try_commit(Ref, Stmt0, Handler), Context,
        !IO) :-
    globals.io_lookup_bool_option(gcc_local_labels, GCC_LocalLabels, !IO),
    (
        GCC_LocalLabels = yes,

        % Output the following:
        %
        %       <Stmt>
        %       goto <Ref>_done;
        %   <Ref>:
        %       <Handler>
        %   <Ref>_done:
        %       ;

        % Note that <Ref> should be just variable name, not a complicated
        % expression. If not, the C compiler will catch it.

        mlds_output_statement(Indent, FuncInfo, Stmt0, !IO),

        mlds_indent(Context, Indent, !IO),
        io.write_string("goto ", !IO),
        mlds_output_lval(Ref, !IO),
        io.write_string("_done;\n", !IO),

        mlds_indent(Context, Indent - 1, !IO),
        mlds_output_lval(Ref, !IO),
        io.write_string(":\n", !IO),

        mlds_output_statement(Indent, FuncInfo, Handler, !IO),

        mlds_indent(Context, Indent - 1, !IO),
        mlds_output_lval(Ref, !IO),
        io.write_string("_done:\t;\n", !IO)

    ;
        GCC_LocalLabels = no,

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
        % to the function containing the setjmp() and which are modified
        % between the setjmp() and the longjmp() become indeterminate after
        % the longjmp(). The MLDS code generator handles that by generating
        % each commit in its own nested function, with the local variables
        % remaining in the containing function. This ensures that none of the
        % variables which get modified between the setjmp() and the longjmp()
        % and which get referenced after the longjmp() are local variables
        % in the function containing the setjmp(), so we don't need to mark
        % them as volatile.

        % We need to take care to avoid problems caused by the dangling else
        % ambiguity.
        ( Stmt0 = statement(if_then_else(_, _, no), Context) ->
            Stmt = statement(block([], [Stmt0]), Context)
        ;
            Stmt = Stmt0
        ),

        mlds_indent(Indent, !IO),
        io.write_string("if (MR_builtin_setjmp(", !IO),
        mlds_output_lval(Ref, !IO),
        io.write_string(") == 0)\n", !IO),

        mlds_output_statement(Indent + 1, FuncInfo, Stmt, !IO),

        mlds_indent(Context, Indent, !IO),
        io.write_string("else\n", !IO),

        mlds_output_statement(Indent + 1, FuncInfo, Handler, !IO)
    ).

:- pred mlds_output_computed_goto_label(mlds_context::in, int::in,
    mlds_label::in, int::in, int::out, io::di, io::uo) is det.

mlds_output_computed_goto_label(Context, Indent, Label, Count0, Count, !IO) :-
    mlds_indent(Context, Indent + 1, !IO),
    io.write_string("case ", !IO),
    io.write_int(Count0, !IO),
    io.write_string(": goto ", !IO),
    mlds_output_label_name(Label, !IO),
    io.write_string(";\n", !IO),
    Count = Count0 + 1.

%-----------------------------------------------------------------------------%

%
% Extra code for outputting switch statements
%

:- pred mlds_output_switch_case(indent::in, func_info::in, mlds_context::in,
    mlds_switch_case::in, io::di, io::uo) is det.

mlds_output_switch_case(Indent, FuncInfo, Context, Case, !IO) :-
    Case = (Conds - Statement),
    list.foldl(mlds_output_case_cond(Indent, Context), Conds, !IO),
    mlds_output_statement(Indent + 1, FuncInfo, Statement, !IO),
    mlds_indent(Context, Indent + 1, !IO),
    io.write_string("break;\n", !IO).

:- pred mlds_output_case_cond(indent::in, mlds_context::in,
    mlds_case_match_cond::in, io::di, io::uo) is det.

mlds_output_case_cond(Indent, Context, match_value(Val), !IO) :-
    mlds_indent(Context, Indent, !IO),
    io.write_string("case ", !IO),
    mlds_output_rval(Val, !IO),
    io.write_string(":\n", !IO).
mlds_output_case_cond(Indent, Context, match_range(Low, High), !IO) :-
    % This uses the GNU C extension `case <Low> ... <High>:'.
    mlds_indent(Context, Indent, !IO),
    io.write_string("case ", !IO),
    mlds_output_rval(Low, !IO),
    io.write_string(" ... ", !IO),
    mlds_output_rval(High, !IO),
    io.write_string(":\n", !IO).

:- pred mlds_output_switch_default(indent::in, func_info::in,
    mlds_context::in, mlds_switch_default::in, io::di, io::uo) is det.

mlds_output_switch_default(Indent, _FuncInfo, Context, default_is_unreachable,
        !IO) :-
    mlds_indent(Context, Indent, !IO),
    io.write_string("default: /*NOTREACHED*/ MR_assert(0);\n", !IO).
mlds_output_switch_default(_Indent, _FuncInfo, _Context, default_do_nothing,
        !IO).
mlds_output_switch_default(Indent, FuncInfo, Context, default_case(Statement),
        !IO) :-
    mlds_indent(Context, Indent, !IO),
    io.write_string("default:\n", !IO),
    mlds_output_statement(Indent + 1, FuncInfo, Statement, !IO),
    mlds_indent(Context, Indent + 1, !IO),
    io.write_string("break;\n", !IO).

%-----------------------------------------------------------------------------%

    % If memory profiling is turned on, output an instruction to
    % record the heap allocation.
    %
:- pred mlds_maybe_output_heap_profile_instr(mlds_context::in,
    indent::in, list(mlds_rval)::in, mlds_qualified_entity_name::in,
    maybe(ctor_name)::in, io::di, io::uo) is det.

mlds_maybe_output_heap_profile_instr(Context, Indent, Args, FuncName,
        MaybeCtorName, !IO) :-
    globals.io_lookup_bool_option(profile_memory, ProfileMem, !IO),
    (
        ProfileMem = yes,
        mlds_indent(Context, Indent, !IO),
        io.write_string("MR_record_allocation(", !IO),
        io.write_int(list.length(Args), !IO),
        io.write_string(", ", !IO),
        mlds_output_fully_qualified_name(FuncName, !IO),
        io.write_string(", """, !IO),
        mlds_output_fully_qualified_name(FuncName, !IO),
        io.write_string(""", ", !IO),
        (
            MaybeCtorName = yes(CtorId),
            io.write_char('"', !IO),
            CtorId = qual(_ModuleName, _QualKind, CtorDefn),
            CtorDefn = ctor_id(CtorName, _CtorArity),
            c_util.output_quoted_string(CtorName, !IO),
            io.write_char('"', !IO)
        ;
            MaybeCtorName = no,
            % Just use an empty string.  Note that we can't use a null pointer
            % here, because MR_record_allocation() requires its string
            % arguments to not be NULL.
            io.write_string("\"\"", !IO)
        ),
        io.write_string(");\n", !IO)
    ;
        ProfileMem = no
    ).

    % If call profiling is turned on output an instruction to record
    % an arc in the call profile between the callee and caller.
    %
:- pred mlds_maybe_output_call_profile_instr(mlds_context::in, indent::in,
    mlds_rval::in, mlds_qualified_entity_name::in, io::di, io::uo) is det.

mlds_maybe_output_call_profile_instr(Context, Indent,
        CalleeFuncRval, CallerName, !IO) :-
    globals.io_lookup_bool_option(profile_calls, ProfileCalls, !IO),
    (
        ProfileCalls = yes,
        mlds_indent(Context, Indent, !IO),
        io.write_string("MR_prof_call_profile(", !IO),
        mlds_output_bracketed_rval(CalleeFuncRval, !IO),
        io.write_string(", ", !IO),
        mlds_output_fully_qualified_name(CallerName, !IO),
        io.write_string(");\n", !IO)
    ;
        ProfileCalls = no
    ).

    % If time profiling is turned on output an instruction which informs
    % the runtime which procedure we are currently located in.
    %
:- pred mlds_maybe_output_time_profile_instr(mlds_context::in,
    indent::in, mlds_qualified_entity_name::in, io::di, io::uo) is det.

mlds_maybe_output_time_profile_instr(Context, Indent, Name, !IO) :-
    globals.io_lookup_bool_option(profile_time, ProfileTime, !IO),
    (
        ProfileTime = yes,
        mlds_indent(Context, Indent, !IO),
        io.write_string("MR_set_prof_current_proc(", !IO),
        mlds_output_fully_qualified_name(Name, !IO),
        io.write_string(");\n", !IO)
    ;
        ProfileTime = no
    ).

%-----------------------------------------------------------------------------%

mlds_output_stmt(Indent, FuncInfo, atomic(AtomicStatement), Context, !IO) :-
    mlds_output_atomic_stmt(Indent, FuncInfo, AtomicStatement, Context, !IO).

:- pred mlds_output_label_name(mlds_label::in, io::di, io::uo) is det.

mlds_output_label_name(LabelName, !IO) :-
    mlds_output_mangled_name(LabelName, !IO).

:- pred mlds_output_atomic_stmt(indent::in, func_info::in,
    mlds_atomic_statement::in, mlds_context::in, io::di, io::uo) is det.

mlds_output_atomic_stmt(Indent, _FuncInfo, comment(Comment), _, !IO) :-
    % XXX We should escape any "*/"'s in the Comment. We should also split
    % the comment into lines and indent each line appropriately.
    mlds_indent(Indent, !IO),
    io.write_string("/* ", !IO),
    io.write_string(Comment, !IO),
    io.write_string(" */\n", !IO).

mlds_output_atomic_stmt(Indent, _FuncInfo, assign(Lval, Rval), _, !IO) :-
    mlds_indent(Indent, !IO),
    mlds_output_lval(Lval, !IO),
    io.write_string(" = ", !IO),
    mlds_output_rval(Rval, !IO),
    io.write_string(";\n", !IO).

mlds_output_atomic_stmt(_Indent, _FuncInfo, delete_object(_Lval), _, !IO) :-
    sorry(this_file, "delete_object not implemented").

mlds_output_atomic_stmt(Indent, FuncInfo, NewObject, Context, !IO) :-
    NewObject = new_object(Target, MaybeTag, _HasSecTag, Type, MaybeSize,
        MaybeCtorName, Args, ArgTypes, MayUseAtomic),
    mlds_indent(Indent, !IO),
    io.write_string("{\n", !IO),

    % When filling in the fields of a newly allocated cell, use a fresh
    % local variable as the base address for the field references in
    % preference to an lval that is more expensive to access. This yields
    % a speedup of about 0.3%.

    ( Target = var(_, _) ->
        Base = lval(Target)
    ;
        % It doesn't matter what string we pick for BaseVarName,
        % as long as its declaration doesn't hide any of the variables
        % inside Args. This is not hard to ensure, since the printed
        % forms of the variables inside Args all include "__".
        BaseVarName = "base",
        Base = string(BaseVarName),
        mlds_indent(Context, Indent + 1, !IO),
        mlds_output_type_prefix(Type, !IO),
        io.write_string(" ", !IO),
        io.write_string(BaseVarName, !IO),
        mlds_output_type_suffix(Type, !IO),
        io.write_string(";\n", !IO)
    ),

    % For --gc accurate, we need to insert a call to GC_check()
    % before every allocation.
    globals.io_get_gc_method(GC_Method, !IO),
    ( GC_Method = gc_accurate ->
        mlds_indent(Context, Indent + 1, !IO),
        io.write_string("MR_GC_check();\n", !IO),
        % For types which hold RTTI that will be traversed by the collector
        % at GC-time, we need to allocate an extra word at the start, to hold
        % the forwarding pointer. Normally we would just overwrite the first
        % word of the object in the "from" space, but this can't be done for
        % objects which will be referenced during the garbage collection
        % process.
        ( type_needs_forwarding_pointer_space(Type) = yes ->
            mlds_indent(Context, Indent + 1, !IO),
            io.write_string("/* reserve space for " ++
                "GC forwarding pointer*/\n", !IO),
            mlds_indent(Context, Indent + 1, !IO),
            io.write_string("MR_hp_alloc(1);\n", !IO)
        ;
            true
        )
    ;
        true
    ),

    FuncInfo = func_info(FuncName, _FuncSignature),
    mlds_maybe_output_heap_profile_instr(Context, Indent + 1, Args,
        FuncName, MaybeCtorName, !IO),

    mlds_indent(Context, Indent + 1, !IO),
    write_lval_or_string(Base, !IO),
    io.write_string(" = ", !IO),
    (
        MaybeTag = yes(Tag0),
        Tag = Tag0,
        mlds_output_cast(Type, !IO),
        io.write_string("MR_mkword(", !IO),
        mlds_output_tag(Tag, !IO),
        io.write_string(", ", !IO),
        EndMkword = ")"
    ;
        MaybeTag = no,
        Tag = 0,
        % XXX We shouldn't need the cast here, but currently the type that we
        % include in the call to MR_new_object() is not always correct.
        mlds_output_cast(Type, !IO),
        EndMkword = ""
    ),
    (
        MayUseAtomic = may_not_use_atomic_alloc,
        io.write_string("MR_new_object(", !IO)
    ;
        MayUseAtomic = may_use_atomic_alloc,
        io.write_string("MR_new_object_atomic(", !IO)
    ),
    mlds_output_type(Type, !IO),
    io.write_string(", ", !IO),
    (
        MaybeSize = yes(Size),
        io.write_string("(", !IO),
        mlds_output_rval(Size, !IO),
        io.write_string(" * sizeof(MR_Word))", !IO)
    ;
        MaybeSize = no,
        % XXX what should we do here?
        io.write_int(-1, !IO)
    ),
    io.write_string(", ", !IO),
    (
        MaybeCtorName = yes(QualifiedCtorId),
        io.write_char('"', !IO),
        QualifiedCtorId = qual(_ModuleName, _QualKind, CtorDefn),
        CtorDefn = ctor_id(CtorName, _CtorArity),
        c_util.output_quoted_string(CtorName, !IO),
        io.write_char('"', !IO)
    ;
        MaybeCtorName = no,
        io.write_string("NULL", !IO)
    ),
    io.write_string(")", !IO),
    io.write_string(EndMkword, !IO),
    io.write_string(";\n", !IO),
    (
        Base = lval(_)
    ;
        Base = string(BaseVarName1),
        mlds_indent(Context, Indent + 1, !IO),
        mlds_output_lval(Target, !IO),
        io.write_string(" = ", !IO),
        io.write_string(BaseVarName1, !IO),
        io.write_string(";\n", !IO)
    ),
    mlds_output_init_args(Args, ArgTypes, Context, 0, Base, Tag, Indent + 1,
        !IO),
    mlds_indent(Context, Indent, !IO),
    io.write_string("}\n", !IO).

mlds_output_atomic_stmt(Indent, _FuncInfo, gc_check, _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("MR_GC_check();\n", !IO).

mlds_output_atomic_stmt(Indent, _FuncInfo, mark_hp(Lval), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("MR_mark_hp(", !IO),
    mlds_output_lval(Lval, !IO),
    io.write_string(");\n", !IO).

mlds_output_atomic_stmt(Indent, _FuncInfo, restore_hp(Rval), _, !IO) :-
    mlds_indent(Indent, !IO),
    io.write_string("MR_restore_hp(", !IO),
    mlds_output_rval(Rval, !IO),
    io.write_string(");\n", !IO).

mlds_output_atomic_stmt(_Indent, _FuncInfo, trail_op(_TrailOp), _, !IO) :-
    sorry(this_file, "trail_ops not implemented").

mlds_output_atomic_stmt(_Indent, _FuncInfo,
    inline_target_code(TargetLang, Components), Context, !IO) :-
    ( TargetLang = lang_C ->
        list.foldl(mlds_output_target_code_component(Context), Components,
            !IO)
    ;
        sorry(this_file, "inline_target_code only works for lang_C")
    ).

mlds_output_atomic_stmt(_Indent, _FuncInfo,
        outline_foreign_proc(_Lang, _Vs, _Lvals, _Code), _Context, !IO) :-
    unexpected(this_file, "outline_foreign_proc is not used in C backend").

:- pred mlds_output_target_code_component(mlds_context::in,
    target_code_component::in, io::di, io::uo) is det.

    % Note: `name(Name)' target_code_components are used to
    % generate the #define for `MR_PROC_LABEL'.
    % The fact that they're used in a #define means that we can't do
    % an output_context(Context) here, since #line directives
    % aren't allowed inside #defines.
    % Similarly, all the target_code_components except user_target_code
    % can get emitted inside calls to the MR_BOX_FOREIGN_TYPE
    % or MR_UNBOX_FOREIGN_TYPE macros, which means that we can't output
    % the contexts for those either, since #line directives aren't
    % allowed inside macro invocations in standard C
    % (although some compilers, e.g. gcc 3.2, do allow it).
mlds_output_target_code_component(Context,
        user_target_code(CodeString, MaybeUserContext, _Attrs), !IO) :-
    (
        MaybeUserContext = yes(UserContext),
        output_context(mlds_make_context(UserContext), !IO)
    ;
        MaybeUserContext = no,
        output_context(Context, !IO)
    ),
    io.write_string(CodeString, !IO),
    io.write_string("\n", !IO),
    reset_context(!IO).
mlds_output_target_code_component(_Context, raw_target_code(CodeString,
        _Attrs), !IO) :-
    io.write_string(CodeString, !IO).
mlds_output_target_code_component(_Context, target_code_input(Rval), !IO) :-
    mlds_output_rval(Rval, !IO),
    io.write_string(" ", !IO).
mlds_output_target_code_component(_Context, target_code_output(Lval), !IO) :-
    mlds_output_lval(Lval, !IO),
    io.write_string(" ", !IO).
mlds_output_target_code_component(_Context, name(Name), !IO) :-
    mlds_output_fully_qualified_name(Name, !IO),
    io.write_string("\n", !IO).

:- func type_needs_forwarding_pointer_space(mlds_type) = bool.

type_needs_forwarding_pointer_space(mlds_type_info_type) = yes.
type_needs_forwarding_pointer_space(mlds_pseudo_type_info_type) = yes.
type_needs_forwarding_pointer_space(mercury_type(_, TypeCategory, _)) =
    is_introduced_type_info_type_category(TypeCategory).
type_needs_forwarding_pointer_space(mlds_mercury_array_type(_)) = no.
type_needs_forwarding_pointer_space(mlds_cont_type(_)) = no.
type_needs_forwarding_pointer_space(mlds_commit_type) = no.
type_needs_forwarding_pointer_space(mlds_native_bool_type) = no.
type_needs_forwarding_pointer_space(mlds_native_int_type) = no.
type_needs_forwarding_pointer_space(mlds_native_float_type) = no.
type_needs_forwarding_pointer_space(mlds_native_char_type) = no.
type_needs_forwarding_pointer_space(mlds_foreign_type(_)) = no.
type_needs_forwarding_pointer_space(mlds_class_type(_, _, _)) = no.
type_needs_forwarding_pointer_space(mlds_array_type(_)) = no.
type_needs_forwarding_pointer_space(mlds_ptr_type(_)) = no.
type_needs_forwarding_pointer_space(mlds_func_type(_)) = no.
type_needs_forwarding_pointer_space(mlds_generic_type) = no.
type_needs_forwarding_pointer_space(mlds_generic_env_ptr_type) = no.
type_needs_forwarding_pointer_space(mlds_rtti_type(_)) = _ :-
    % These should all be statically allocated, not dynamically allocated,
    % so we should never get here.
    unexpected(this_file,
        "type_needs_forwarding_pointer_space: rtti_type").
type_needs_forwarding_pointer_space(mlds_tabling_type(_)) = _ :-
    % These should all be statically allocated, not dynamically allocated,
    % so we should never get here.
    unexpected(this_file,
        "type_needs_forwarding_pointer_space: tabling_type").
type_needs_forwarding_pointer_space(mlds_unknown_type) = _ :-
    unexpected(this_file, "type_needs_forwarding_pointer_space: unknown_type").

:- type lval_or_string
    --->    lval(mlds_lval)
    ;       string(string).

:- pred mlds_output_init_args(list(mlds_rval)::in, list(mlds_type)::in,
    mlds_context::in, int::in, lval_or_string::in, mlds_tag::in,
    indent::in, io::di, io::uo) is det.

mlds_output_init_args([_ | _], [], _, _, _, _, _, !IO) :-
    unexpected(this_file, "mlds_output_init_args: length mismatch").
mlds_output_init_args([], [_ | _], _, _, _, _, _, !IO) :-
    unexpected(this_file, "mlds_output_init_args: length mismatch").
mlds_output_init_args([], [], _, _, _, _, _, !IO).
mlds_output_init_args([Arg | Args], [ArgType | ArgTypes], Context,
        ArgNum, Base, Tag, Indent, !IO) :-
    % The MR_hl_field() macro expects its argument to have type MR_Box,
    % so we need to box the arguments if they aren't already boxed.
    % Hence the use of mlds_output_boxed_rval below.

    % XXX For --high-level-data, we ought to generate assignments to the fields
    % (or perhaps a call to a constructor function) rather than using the
    % MR_hl_field() macro.

    mlds_indent(Context, Indent, !IO),
    io.write_string("MR_hl_field(", !IO),
    mlds_output_tag(Tag, !IO),
    io.write_string(", ", !IO),
    write_lval_or_string(Base, !IO),
    io.write_string(", ", !IO),
    io.write_int(ArgNum, !IO),
    io.write_string(") = ", !IO),
    mlds_output_boxed_rval(ArgType, Arg, !IO),
    io.write_string(";\n", !IO),
    mlds_output_init_args(Args, ArgTypes, Context,
        ArgNum + 1, Base, Tag, Indent, !IO).

:- pred write_lval_or_string(lval_or_string::in, io::di, io::uo) is det.

write_lval_or_string(Base, !IO) :-
    (
        Base = lval(Target),
        mlds_output_lval(Target, !IO)
    ;
        Base = string(BaseVarName),
        io.write_string(BaseVarName, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred mlds_output_lval(mlds_lval::in, io::di, io::uo) is det.

mlds_output_lval(field(MaybeTag, Rval, offset(OffsetRval),
        FieldType, _ClassType), !IO) :-
    (
        ( FieldType = mlds_generic_type
        ; FieldType = mercury_type(type_variable(_, _), _, _)
        )
    ->
        io.write_string("(", !IO)
    ;
        % The field type for field(_, _, offset(_), _, _) lvals
        % must be something that maps to MR_Box.
        unexpected(this_file, "unexpected field type")
    ),
    (
        MaybeTag = yes(Tag),
        io.write_string("MR_hl_field(", !IO),
        mlds_output_tag(Tag, !IO),
        io.write_string(", ", !IO)
    ;
        MaybeTag = no,
        io.write_string("MR_hl_mask_field(", !IO),
        io.write_string("(MR_Word) ", !IO)
    ),
    mlds_output_rval(Rval, !IO),
    io.write_string(", ", !IO),
    mlds_output_rval(OffsetRval, !IO),
    io.write_string("))", !IO).
mlds_output_lval(field(MaybeTag, PtrRval, named_field(FieldName, CtorType),
        _FieldType, PtrType), !IO) :-
    io.write_string("(", !IO),
    ( MaybeTag = yes(0) ->
        ( PtrType \= CtorType ->
            mlds_output_cast(CtorType, !IO)
        ;
            true
        ),
        ( PtrRval = mem_addr(Lval) ->
            mlds_output_lval(Lval, !IO),
            io.write_string(").", !IO)
        ;
            mlds_output_bracketed_rval(PtrRval, !IO),
            io.write_string(")->", !IO)
        )
    ;
        mlds_output_cast(CtorType, !IO),
        (
            MaybeTag = yes(Tag),
            io.write_string("MR_body(", !IO),
            mlds_output_rval(PtrRval, !IO),
            io.write_string(", ", !IO),
            mlds_output_tag(Tag, !IO)
        ;
            MaybeTag = no,
            io.write_string("MR_strip_tag(", !IO),
            mlds_output_rval(PtrRval, !IO)
        ),
        io.write_string("))->", !IO)
    ),
    mlds_output_fully_qualified(FieldName, mlds_output_mangled_name, !IO).
mlds_output_lval(mem_ref(Rval, _Type), !IO) :-
    io.write_string("*", !IO),
    mlds_output_bracketed_rval(Rval, !IO).
mlds_output_lval(global_var_ref(GobalVar), !IO) :-
    io.write_string(global_var_name(GobalVar), !IO).
mlds_output_lval(var(VarName, _VarType), !IO) :-
    mlds_output_var(VarName, !IO).

:- func global_var_name(global_var_ref) = string.

% The calls to env_var_is_acceptable_char in prog_io_goal.m  ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c
% and c_global_var_name in llds_out.m.
global_var_name(env_var_ref(EnvVarName)) = "mercury_envvar_" ++ EnvVarName.

:- pred mlds_output_var(mlds_var::in, io::di, io::uo) is det.

mlds_output_var(VarName, !IO) :-
    mlds_output_fully_qualified(VarName, mlds_output_var_name, !IO).

:- pred mlds_output_var_name(mlds_var_name::in, io::di, io::uo) is det.

mlds_output_var_name(VarName, !IO) :-
    mlds_output_mangled_name(ml_var_name_to_string(VarName), !IO).

:- pred mlds_output_mangled_name(string::in, io::di, io::uo) is det.

mlds_output_mangled_name(Name, !IO) :-
    io.write_string(name_mangle(Name), !IO).

:- pred mlds_output_bracketed_lval(mlds_lval::in, io::di, io::uo) is det.

mlds_output_bracketed_lval(Lval, !IO) :-
    (
        % If it's just a variable name, then we don't need parentheses.
        Lval = var(_, _)
    ->
        mlds_output_lval(Lval, !IO)
    ;
        io.write_char('(', !IO),
        mlds_output_lval(Lval, !IO),
        io.write_char(')', !IO)
    ).

:- pred mlds_output_bracketed_rval(mlds_rval::in, io::di, io::uo) is det.

mlds_output_bracketed_rval(Rval, !IO) :-
    (
        % If it's just a variable name, then we don't need parentheses.
        ( Rval = lval(var(_,_))
        ; Rval = const(mlconst_code_addr(_))
        )
    ->
        mlds_output_rval(Rval, !IO)
    ;
        io.write_char('(', !IO),
        mlds_output_rval(Rval, !IO),
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
    % ahead and generate C-like psuedo-code for the purposes of MLDS dumps.
    io.write_string("\n#error multiple return values\n", !IO),
    io.write_string("\t{", !IO),
    io.write_list(List, ", ", OutputPred, !IO),
    io.write_string("}", !IO).

:- pred mlds_output_rval(mlds_rval::in, io::di, io::uo) is det.

mlds_output_rval(lval(Lval), !IO) :-
    mlds_output_lval(Lval, !IO).

% XXX Do we need the commented out code below?
% mlds_output_rval(lval(Lval), !IO) :-
%   % if a field is used as an rval, then we need to use
%   % the MR_hl_const_field() macro, not the MR_hl_field() macro,
%   % to avoid warnings about discarding const,
%   % and similarly for MR_mask_field.
%   ( Lval = field(MaybeTag, Rval, FieldNum, _, _) ->
%       ( MaybeTag = yes(Tag) ->
%           io.write_string("MR_hl_const_field(", !IO),
%           mlds_output_tag(Tag, !IO),
%           io.write_string(", ", !IO)
%       ;
%           io.write_string("MR_hl_const_mask_field(", !IO)
%       ),
%       mlds_output_rval(Rval, !IO),
%       io.write_string(", ", !IO),
%       mlds_output_rval(FieldNum, !IO),
%       io.write_string(")", !IO)
%   ;
%       mlds_output_lval(Lval, !IO)
%   ).

mlds_output_rval(mkword(Tag, Rval), !IO) :-
    io.write_string("MR_mkword(", !IO),
    mlds_output_tag(Tag, !IO),
    io.write_string(", ", !IO),
    mlds_output_rval(Rval, !IO),
    io.write_string(")", !IO).

mlds_output_rval(const(Const), !IO) :-
    mlds_output_rval_const(Const, !IO).

mlds_output_rval(unop(Op, Rval), !IO) :-
    mlds_output_unop(Op, Rval, !IO).

mlds_output_rval(binop(Op, Rval1, Rval2), !IO) :-
    mlds_output_binop(Op, Rval1, Rval2, !IO).

mlds_output_rval(mem_addr(Lval), !IO) :-
    % XXX are parentheses needed?
    io.write_string("&", !IO),
    mlds_output_lval(Lval, !IO).

mlds_output_rval(self(_), !IO) :-
    io.write_string("this", !IO).

:- pred mlds_output_unop(mlds_unary_op::in, mlds_rval::in, io::di, io::uo)
    is det.

mlds_output_unop(cast(Type), Exprn, !IO) :-
    mlds_output_cast_rval(Type, Exprn, !IO).
mlds_output_unop(box(Type), Exprn, !IO) :-
    mlds_output_boxed_rval(Type, Exprn, !IO).
mlds_output_unop(unbox(Type), Exprn, !IO) :-
    mlds_output_unboxed_rval(Type, Exprn, !IO).
mlds_output_unop(std_unop(Unop), Exprn, !IO) :-
    mlds_output_std_unop(Unop, Exprn, !IO).

:- pred mlds_output_cast_rval(mlds_type::in, mlds_rval::in, io::di, io::uo)
    is det.

mlds_output_cast_rval(Type, Exprn, !IO) :-
    mlds_output_cast(Type, !IO),
    mlds_output_rval(Exprn, !IO).

:- pred mlds_output_cast(mlds_type::in, io::di, io::uo) is det.

mlds_output_cast(Type, !IO) :-
    io.write_string("(", !IO),
    mlds_output_type(Type, !IO),
    io.write_string(") ", !IO).

:- pred mlds_output_boxed_rval(mlds_type::in, mlds_rval::in, io::di, io::uo)
    is det.

mlds_output_boxed_rval(Type, Exprn, !IO) :-
    (
        ( Type = mlds_generic_type
        ; Type = mercury_type(_, type_cat_variable, _)
        )
    ->
        % It already has type MR_Box, so no cast is needed.
        mlds_output_rval(Exprn, !IO)
    ;
        Exprn = unop(cast(OtherType), InnerExprn),
        ( Type = OtherType
        ; is_an_address(InnerExprn)
        )
    ->
        % Avoid unnecessary double-casting -- strip away the inner cast.
        % This is necessary for ANSI/ISO C conformance, to avoid casts
        % from pointers to integers in static initializers.
        mlds_output_boxed_rval(Type, InnerExprn, !IO)
    ;
        ( Type = mercury_type(builtin_type(builtin_type_float), _, _)
        ; Type = mlds_native_float_type
        )
    ->
        io.write_string("MR_box_float(", !IO),
        mlds_output_rval(Exprn, !IO),
        io.write_string(")", !IO)
    ;
        ( Type = mercury_type(builtin_type(builtin_type_character), _, _)
        ; Type = mlds_native_char_type
        ; Type = mlds_native_bool_type
        ; Type = mlds_native_int_type
        )
    ->
        % We cast first to MR_Word, and then to MR_Box.
        % This is done to avoid spurious warnings about "cast from
        % integer to pointer of different size" from gcc.
        io.write_string("((MR_Box) (MR_Word) (", !IO),
        mlds_output_rval(Exprn, !IO),
        io.write_string("))", !IO)
    ;
        io.write_string("((MR_Box) (", !IO),
        mlds_output_rval(Exprn, !IO),
        io.write_string("))", !IO)
    ).

    % Succeed if the specified rval is an address (possibly tagged and/or
    % cast to a different type).
    %
:- pred is_an_address(mlds_rval::in) is semidet.

is_an_address(mkword(_Tag, Expr)) :-
    is_an_address(Expr).
is_an_address(unop(cast(_), Expr)) :-
    is_an_address(Expr).
is_an_address(mem_addr(_)).
is_an_address(const(mlconst_null(_))).
is_an_address(const(mlconst_code_addr(_))).
is_an_address(const(mlconst_data_addr(_))).

:- pred mlds_output_unboxed_rval(mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_unboxed_rval(Type, Exprn, !IO) :-
    (
        ( Type = mercury_type(builtin_type(builtin_type_float), _, _)
        ; Type = mlds_native_float_type
        )
    ->
        io.write_string("MR_unbox_float(", !IO),
        mlds_output_rval(Exprn, !IO),
        io.write_string(")", !IO)
    ;
        ( Type = mercury_type(builtin_type(builtin_type_character), _, _)
        ; Type = mlds_native_char_type
        ; Type = mlds_native_bool_type
        ; Type = mlds_native_int_type
        )
    ->
        % We cast first to MR_Word, and then to the desired type.
        % This is done to avoid spurious warnings about "cast from
        % pointer to integer of different size" from gcc.
        io.write_string("(", !IO),
        mlds_output_cast(Type, !IO),
        io.write_string("(MR_Word) ", !IO),
        mlds_output_rval(Exprn, !IO),
        io.write_string(")", !IO)
    ;
        io.write_string("(", !IO),
        mlds_output_cast(Type, !IO),
        mlds_output_rval(Exprn, !IO),
        io.write_string(")", !IO)
    ).

:- pred mlds_output_std_unop(builtin_ops.unary_op::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_std_unop(UnaryOp, Exprn, !IO) :-
    c_util.unary_prefix_op(UnaryOp, UnaryOpString),
    io.write_string(UnaryOpString, !IO),
    io.write_string("(", !IO),
    ( UnaryOp = tag ->
        % The MR_tag macro requires its argument to be of type `MR_Word'.
        % XXX Should we put this cast inside the definition of MR_tag?
        io.write_string("(MR_Word) ", !IO)
    ;
        true
    ),
    mlds_output_rval(Exprn, !IO),
    io.write_string(")", !IO).

:- pred mlds_output_binop(binary_op::in, mlds_rval::in, mlds_rval::in,
    io::di, io::uo) is det.

mlds_output_binop(Op, X, Y, !IO) :-
    (
        Op = array_index(_Type)
    ->
        mlds_output_bracketed_rval(X, !IO),
        io.write_string("[", !IO),
        mlds_output_rval(Y, !IO),
        io.write_string("]", !IO)
    ;
        Op = body
    ->
        io.write_string("MR_body(", !IO),
        mlds_output_rval(X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Y, !IO),
        io.write_string(")", !IO)
    ;
        c_util.string_compare_op(Op, OpStr)
    ->
        io.write_string("(strcmp(", !IO),
        mlds_output_rval(X, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Y, !IO),
        io.write_string(")", !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        io.write_string("0)", !IO)
    ;
        ( c_util.float_compare_op(Op, OpStr1) ->
            OpStr = OpStr1
        ; c_util.float_op(Op, OpStr2) ->
            OpStr = OpStr2
        ;
            fail
        )
    ->
        io.write_string("(", !IO),
        mlds_output_bracketed_rval(X, !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        mlds_output_bracketed_rval(Y, !IO),
        io.write_string(")", !IO)
    ;
% XXX Broken for C == minint, (since `NewC is 0 - C' overflows)
%       Op = (+),
%       Y = const(int_const(C)),
%       C < 0
%   ->
%       NewOp = (-),
%       NewC is 0 - C,
%       NewY = const(int_const(NewC)),
%       io.write_string("(", !IO),
%       mlds_output_rval(X, !IO),
%       io.write_string(" ", !IO),
%       mlds_output_binary_op(NewOp, !IO),
%       io.write_string(" ", !IO),
%       mlds_output_rval(NewY, !IO),
%       io.write_string(")", !IO)
%   ;
        io.write_string("(", !IO),
        mlds_output_rval(X, !IO),
        io.write_string(" ", !IO),
        mlds_output_binary_op(Op, !IO),
        io.write_string(" ", !IO),
        mlds_output_rval(Y, !IO),
        io.write_string(")", !IO)
    ).

:- pred mlds_output_binary_op(binary_op::in, io::di, io::uo) is det.

mlds_output_binary_op(Op, !IO) :-
    ( c_util.binary_infix_op(Op, OpStr) ->
        io.write_string(OpStr, !IO)
    ;
        unexpected(this_file,
            "mlds_output_binary_op: invalid binary operator")
    ).

:- pred mlds_output_rval_const(mlds_rval_const::in, io::di, io::uo) is det.

mlds_output_rval_const(mlconst_true, !IO) :-
    io.write_string("MR_TRUE", !IO).
mlds_output_rval_const(mlconst_false, !IO) :-
    io.write_string("MR_FALSE", !IO).
mlds_output_rval_const(mlconst_int(N), !IO) :-
    % We need to cast to (MR_Integer) to ensure things like 1 << 32 work
    % when `Integer' is 64 bits but `int' is 32 bits.
    io.write_string("(MR_Integer) ", !IO),
    io.write_int(N, !IO).
mlds_output_rval_const(mlconst_float(FloatVal), !IO) :-
    % The cast to (MR_Float) here lets the C compiler do arithmetic in `float'
    % rather than `double' if `MR_Float' is `float' not `double'.
    io.write_string("(MR_Float) ", !IO),
    c_util.output_float_literal(FloatVal, !IO).
mlds_output_rval_const(mlconst_string(String), !IO) :-
    % The cast avoids the following gcc warning
    % "assignment discards qualifiers from pointer target type".
    io.write_string("(MR_String) ", !IO),
    io.write_string("""", !IO),
    c_util.output_quoted_string(String, !IO),
    io.write_string("""", !IO).
mlds_output_rval_const(mlconst_multi_string(Length, String), !IO) :-
    io.write_string("""", !IO),
    c_util.output_quoted_multi_string(Length, String, !IO),
    io.write_string("""", !IO).
mlds_output_rval_const(mlconst_code_addr(CodeAddr), !IO) :-
    mlds_output_code_addr(CodeAddr, !IO).
mlds_output_rval_const(mlconst_data_addr(DataAddr), !IO) :-
    mlds_output_data_addr(DataAddr, !IO).
mlds_output_rval_const(mlconst_null(_), !IO) :-
    io.write_string("NULL", !IO).

%-----------------------------------------------------------------------------%

:- pred mlds_output_tag(mlds_tag::in, io::di, io::uo) is det.

mlds_output_tag(Tag, !IO) :-
    io.write_string("MR_mktag(", !IO),
    io.write_int(Tag, !IO),
    io.write_string(")", !IO).

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds_code_addr::in, io::di, io::uo) is det.

mlds_output_code_addr(code_addr_proc(Label, _Sig), !IO) :-
    mlds_output_fully_qualified_proc_label(Label, !IO).
mlds_output_code_addr(code_addr_internal(Label, SeqNum, _Sig), !IO) :-
    mlds_output_fully_qualified_proc_label(Label, !IO),
    io.write_string("_", !IO),
    io.write_int(SeqNum, !IO).

:- pred mlds_output_proc_label(mlds_proc_label::in, io::di, io::uo) is det.

mlds_output_proc_label(mlds_proc_label(PredLabel, ProcId), !IO) :-
    mlds_output_pred_label(PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.format("_%d", [i(ModeNum)], !IO).

:- func mlds_proc_label_to_string(mlds_proc_label) = string.

mlds_proc_label_to_string(mlds_proc_label(PredLabel, ProcId)) =
    mlds_pred_label_to_string(PredLabel) ++
        string.format("_%d", [i(proc_id_to_int(ProcId))]).

:- pred mlds_output_data_addr(mlds_data_addr::in, io::di, io::uo) is det.

mlds_output_data_addr(data_addr(ModuleName, DataName), !IO) :-
    % If its an array type, then we just use the name, otherwise we must
    % prefix the name with `&'.
    (
        DataName = mlds_rtti(RttiId),
        rtti_id_has_array_type(RttiId) = yes
    ->
        mlds_output_data_var_name(ModuleName, DataName, !IO)
    ;
        DataName = mlds_tabling_ref(_, TablingId),
        tabling_id_has_array_type(TablingId) = yes
    ->
        mlds_output_data_var_name(ModuleName, DataName, !IO)
    ;
        io.write_string("(&", !IO),
        mlds_output_data_var_name(ModuleName, DataName, !IO),
        io.write_string(")", !IO)
    ).

:- pred mlds_output_data_var_name(mlds_module_name::in, mlds_data_name::in,
    io::di, io::uo) is det.

mlds_output_data_var_name(ModuleName, DataName, !IO) :-
    (
        DataName = mlds_rtti(RttiId),
        module_qualify_name_of_rtti_id(RttiId) = no
    ->
        true
    ;
        mlds_output_module_name(mlds_module_name_to_sym_name(ModuleName), !IO),
        io.write_string("__", !IO)
    ),
    mlds_output_data_name(DataName, !IO).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations (#line directives).
%

output_context(Context, !IO) :-
    ProgContext = mlds_get_prog_context(Context),
    term.context_file(ProgContext, FileName),
    term.context_line(ProgContext, LineNumber),
    c_util.set_line_num(FileName, LineNumber, !IO).

:- pred reset_context(io::di, io::uo) is det.

reset_context(!IO) :-
    c_util.reset_line_num(!IO).

:- pred mlds_indent(mlds_context::in, indent::in, io::di, io::uo) is det.

mlds_indent(Context, N, !IO) :-
    output_context(Context, !IO),
    mlds_indent(N, !IO).

    % A value of type `indent' records the number of levels
    % of indentation to indent the next piece of code.
    % Currently we output two spaces for each level of indentation.
:- type indent == int.

:- pred mlds_indent(indent::in, io::di, io::uo) is det.

mlds_indent(N, !IO) :-
    ( N =< 0 ->
        true
    ;
        io.write_string("  ", !IO),
        mlds_indent(N - 1, !IO)
    ).

:- func this_file = string.

this_file = "mlds_to_c.m".

:- end_module mlds_to_c.

%-----------------------------------------------------------------------------%
