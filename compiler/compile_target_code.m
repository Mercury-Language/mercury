%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: compile_target_code.m.
% Main authors: fjh, stayl.
% 
% Code to compile the generated `.c', `.s', `.o', etc, files.
% 
%-----------------------------------------------------------------------------%

:- module backend_libs.compile_target_code.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.modules.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Are we generating position independent code (for use in a shared
    % library)? On some architectures, pic and non-pic code are incompatible,
    % so we need to generate `.o' and `.pic_o' files.
    %
:- type pic
    --->    pic
    ;       link_with_pic
    ;       non_pic.

    % compile_c_file(ErrorStream, PIC, CFile, ObjFile, Succeeded)
    %
:- pred compile_c_file(io.output_stream::in, pic::in, string::in, string::in,
    bool::out, io::di, io::uo) is det.

    % compile_c_file(ErrorStream, PIC, ModuleName, Succeeded)
    %
:- pred compile_c_file(io.output_stream::in, pic::in, module_name::in,
    bool::out, io::di, io::uo) is det.

    % assemble(ErrorStream, PIC, ModuleName, Succeeded)
    %
:- pred assemble(io.output_stream::in, pic::in, module_name::in,
    bool::out, io::di, io::uo) is det.

    % compile_java_file(ErrorStream, JavaFile, Succeeded)
    %
:- pred compile_java_file(io.output_stream::in, string::in, bool::out,
    io::di, io::uo) is det.

    % il_assemble(ErrorStream, ModuleName, HasMain, Succeeded)
    %
:- pred il_assemble(io.output_stream::in, module_name::in, has_main::in,
    bool::out, io::di, io::uo) is det.

    % il_assemble(ErrorStream, ILFile, DLLFile, HasMain, Succeeded)
    %
:- pred il_assemble(io.output_stream::in, file_name::in, file_name::in,
    has_main::in, bool::out, io::di, io::uo) is det.

    % compile_managed_cplusplus_file(ErrorStream, MCPPFile, DLLFile, Succeeded)
    %
:- pred compile_managed_cplusplus_file(io.output_stream::in,
    file_name::in, file_name::in, bool::out, io::di, io::uo) is det.

    % compile_csharp_file(ErrorStream, C#File, DLLFile, Succeeded)
    %
:- pred compile_csharp_file(io.output_stream::in, module_imports::in,
    file_name::in, file_name::in, bool::out, io::di, io::uo) is det.

    % make_init_file(ErrorStream, MainModuleName, ModuleNames, Succeeded):
    %
    % Make the `.init' file for a library containing the given modules.
    %
:- pred make_init_file(io.output_stream::in, module_name::in,
    list(module_name)::in, bool::out, io::di, io::uo) is det.

    % make_init_obj_file(ErrorStream, MainModuleName, AllModuleNames,
    %   MaybeInitObjFileName)
    %
:- pred make_init_obj_file(io.output_stream::in, module_name::in,
    list(module_name)::in, maybe(file_name)::out, io::di, io::uo) is det.

:- type linked_target_type
    --->    executable
    ;       static_library
    ;       shared_library
    ;       java_archive.

    % link(TargetType, MainModuleName, ObjectFileNames, Succeeded)
    %
:- pred link(io.output_stream::in, linked_target_type::in, module_name::in,
    list(string)::in, bool::out, io::di, io::uo) is det.

    % post_link_make_symlink_or_copy(TargetType, MainModuleName, Succeeded)
    %
    % If `--use-grade-subdirs' is enabled, link or copy the executable or
    % library into the user's directory after having successfully built it.
    %
:- pred post_link_make_symlink_or_copy(io.output_stream::in,
    linked_target_type::in, module_name::in, bool::out, io::di, io::uo) is det.

    % link_module_list(ModulesToLink, FactTableObjFiles, Succeeded):
    %
    % The elements of ModulesToLink are the output of
    % `module_name_to_filename(ModuleName, "", no, ModuleToLink)'
    % for each module in the program.
    %
:- pred link_module_list(list(string)::in, list(string)::in, bool::out,
    io::di, io::uo) is det.

    % shared_libraries_supported(SharedLibsSupported, !IO)
    %
    % Return whether or not shared libraries are supported on the current
    % platform.
    % 
:- pred shared_libraries_supported(bool::out, io::di, io::uo) is det.

    % get_object_code_type(TargetType, PIC):
    %
    % Work out whether we should be generating position-independent
    % object code.
    %
:- pred get_object_code_type(linked_target_type::in, pic::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

    % make_all_module_command(CommandName, MainModule, AllModuleNames,
    %   CommandString):
    %
    % Create a command string which passes the source file names
    % for AllModuleNames to CommandName, with MainModule given first.
    %
:- pred make_all_module_command(string::in, module_name::in,
    list(module_name)::in, string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % maybe_pic_object_file_extension(Globals, PIC, Ext) is true iff
    % Ext is the extension which should be used on object files according to
    % the value of PIC. The value of PIC should be obtained from a call to
    % `get_object_code_type'. In particular, on architectures for which
    % no special handling for PIC is necessary, only a value of `non_pic'
    % should be used. The `(in, out, in)' mode guarantees that the returned
    % value of PIC meets this requirement.
    %
:- pred maybe_pic_object_file_extension(globals, pic, string).
:- mode maybe_pic_object_file_extension(in, in, out) is det.
:- mode maybe_pic_object_file_extension(in, out, in) is semidet.

    % Same as above except the globals are obtained from the io.state.
    %
:- pred maybe_pic_object_file_extension(pic::in, string::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.

:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module dir.
:- import_module getopt_io.
:- import_module string.

%-----------------------------------------------------------------------------%

il_assemble(ErrorStream, ModuleName, HasMain, Succeeded, !IO) :-
    module_name_to_file_name(ModuleName, ".il", no, IL_File, !IO),
    module_name_to_file_name(ModuleName, ".dll", yes, DllFile, !IO),

    % If the module contains main/2 then we it should be built as an
    % executable. Unfortunately MC++ or C# code may refer to the dll
    % so we always need to build the dll.
    %
    il_assemble(ErrorStream, IL_File, DllFile, no_main, DllSucceeded, !IO),
    ( 
        HasMain = has_main,
        module_name_to_file_name(ModuleName, ".exe", yes, ExeFile, !IO),
        il_assemble(ErrorStream, IL_File, ExeFile, HasMain, ExeSucceeded, !IO),
        Succeeded = DllSucceeded `and` ExeSucceeded
    ;
        HasMain = no_main,
        Succeeded = DllSucceeded
    ).

il_assemble(ErrorStream, IL_File, TargetFile, HasMain, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(sign_assembly, SignAssembly, !IO),
    maybe_write_string(Verbose, "% Assembling `", !IO),
    maybe_write_string(Verbose, IL_File, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(il_assembler, ILASM, !IO),
    globals.io_lookup_accumulating_option(ilasm_flags, ILASMFlagsList, !IO),
    join_string_list(ILASMFlagsList, "", "", " ", ILASMFlags),
    (
        SignAssembly = yes,
        SignOpt = "/keyf=mercury.sn "
    ;
        SignAssembly = no,
        SignOpt = ""
    ),
    (
        Verbose = yes,
        VerboseOpt = ""
    ;
        Verbose = no,
        VerboseOpt = "/quiet "
    ),
    globals.io_lookup_bool_option(target_debug, Debug, !IO),
    (
        Debug = yes,
        DebugOpt = "/debug "
    ;
        Debug = no,
        DebugOpt = ""
    ),
    ( 
        HasMain = has_main,
        TargetOpt = ""
    ;
        HasMain = no_main,
        TargetOpt = "/dll "
    ),
    string.append_list([ILASM, " ", SignOpt, VerboseOpt, DebugOpt,
        TargetOpt, ILASMFlags, " /out=", TargetFile, " ", IL_File], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands, Command,
        Succeeded, !IO).

compile_managed_cplusplus_file(ErrorStream, MCPPFileName, DLLFileName,
        Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Compiling `", !IO),
    maybe_write_string(Verbose, MCPPFileName, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(mcpp_compiler, MCPP, !IO),
    globals.io_lookup_accumulating_option(mcpp_flags, MCPPFlagsList, !IO),
    join_string_list(MCPPFlagsList, "", "", " ", MCPPFlags),
    globals.io_lookup_bool_option(target_debug, Debug, !IO),
    (
        Debug = yes,
        DebugOpt = "/Zi "
    ;
        Debug = no,
        DebugOpt = ""
    ),

    % XXX Should we introduce a `--mcpp-include-directory' option?
    globals.io_lookup_accumulating_option(c_include_directory,
        C_Incl_Dirs, !IO),
    InclOpts = string.append_list(list.condense(list.map(
        (func(C_INCL) = ["-I", C_INCL, " "]), C_Incl_Dirs))),

    % XXX Should we use a separate dll_directories options?
    globals.io_lookup_accumulating_option(link_library_directories,
        DLLDirs, !IO),
    DLLDirOpts = "-AIMercury/dlls " ++
        string.append_list(list.condense(list.map(
            (func(DLLDir) = ["-AI", DLLDir, " "]), DLLDirs))),

    string.append_list([MCPP, " -CLR ", DebugOpt, InclOpts, DLLDirOpts,
        MCPPFlags, " ", MCPPFileName, " -LD -o ", DLLFileName], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands, Command,
        Succeeded, !IO).

compile_csharp_file(ErrorStream, Imports, CSharpFileName0, DLLFileName,
        Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Compiling `", !IO),
    maybe_write_string(Verbose, CSharpFileName, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(csharp_compiler, CSC, !IO),
    globals.io_lookup_accumulating_option(csharp_flags, CSCFlagsList, !IO),
    join_string_list(CSCFlagsList, "", "", " ", CSCFlags),

    % XXX This is because the MS C# compiler doesn't understand
    % / as a directory separator.
    CSharpFileName = string.replace_all(CSharpFileName0, "/", "\\\\"),

    globals.io_lookup_bool_option(target_debug, Debug, !IO),
    (
        Debug = yes,
        % XXX This needs testing before it can be enabled (see the comments
        % for install_debug_library in library/Mmakefile).

        % DebugOpt = "/debug+ /debug:full "
        DebugOpt = ""
    ;
        Debug = no,
        DebugOpt = ""
    ),

    % XXX Should we use a separate dll_directories options?
    globals.io_lookup_accumulating_option(link_library_directories, DLLDirs,
        !IO),
    DLLDirOpts = "/lib:Mercury/dlls " ++
        string.append_list(list.condense(list.map(
            (func(DLLDir) = ["/lib:", DLLDir, " "]), DLLDirs))),

    ( mercury_std_library_module_name(Imports ^ module_name) ->
        Prefix = "/addmodule:"
    ;
        Prefix = "/r:"
    ),
    ForeignDeps = list.map(
        (func(M) =
            foreign_import_module_name_from_module(M, Imports ^ module_name)),
        Imports ^ foreign_import_modules),
    ReferencedDlls = referenced_dlls(Imports ^ module_name,
        Imports ^ int_deps ++ Imports ^ impl_deps ++ ForeignDeps),
    list.map_foldl(
        (pred(Mod::in, Result::out, IO0::di, IO::uo) is det :-
            module_name_to_file_name(Mod, ".dll", no, FileName, IO0, IO),
            Result = [Prefix, FileName, " "]
        ), ReferencedDlls, ReferencedDllsList, !IO),
    ReferencedDllsStr = string.append_list(
        list.condense(ReferencedDllsList)),

    string.append_list([CSC, DebugOpt,
        " /t:library ", DLLDirOpts, CSCFlags, ReferencedDllsStr,
        " /out:", DLLFileName, " ", CSharpFileName], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands, Command,
        Succeeded, !IO).

%-----------------------------------------------------------------------------%

% WARNING: The code here duplicates the functionality of scripts/mgnuc.in.
% Any changes there may also require changes here, and vice versa.

:- type compiler_type
    --->    gcc
    ;       lcc
    ;       cl
    ;       unknown_compiler.

compile_c_file(ErrorStream, PIC, ModuleName, Succeeded, !IO) :-
    module_name_to_file_name(ModuleName, ".c", yes, C_File, !IO),
    maybe_pic_object_file_extension(PIC, ObjExt, !IO),
    module_name_to_file_name(ModuleName, ObjExt, yes, O_File, !IO),
    compile_c_file(ErrorStream, PIC, C_File, O_File, Succeeded, !IO).

compile_c_file(ErrorStream, PIC, C_File, O_File, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_string_option(c_flag_to_name_object_file,
        NameObjectFile, !IO),
    maybe_write_string(Verbose, "% Compiling `", !IO),
    maybe_write_string(Verbose, C_File, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(cc, CC, !IO),
    globals.io_lookup_accumulating_option(cflags, C_Flags_List, !IO),
    join_string_list(C_Flags_List, "", "", " ", CFLAGS),

    globals.io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
    (
        UseSubdirs = yes,
        % The source file (foo.c) will be compiled in a subdirectory
        % (either Mercury/cs, foo.dir, or Mercury/dirs/foo.dir, depending
        % on which of these two options is set) so we need to add `-I.'
        % so it can include header files in the source directory.
        SubDirInclOpt = "-I. "
    ;
        UseSubdirs = no,
        SubDirInclOpt = ""
    ),
    globals.io_lookup_accumulating_option(c_include_directory,
        C_Incl_Dirs, !IO),
    InclOpt = string.append_list(list.condense(list.map(
        (func(C_INCL) = ["-I", quote_arg(C_INCL), " "]), C_Incl_Dirs))),
    globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
    (
        HighLevelCode = yes,
        HighLevelCodeOpt = "-DMR_HIGHLEVEL_CODE "
    ;
        HighLevelCode = no,
        HighLevelCodeOpt = ""
    ),
    globals.io_lookup_bool_option(gcc_nested_functions,
        GCC_NestedFunctions, !IO),
    (
        GCC_NestedFunctions = yes,
        NestedFunctionsOpt = "-DMR_USE_GCC_NESTED_FUNCTIONS "
    ;
        GCC_NestedFunctions = no,
        NestedFunctionsOpt = ""
    ),
    globals.io_lookup_bool_option(highlevel_data, HighLevelData, !IO),
    (
        HighLevelData = yes,
        HighLevelDataOpt = "-DMR_HIGHLEVEL_DATA "
    ;
        HighLevelData = no,
        HighLevelDataOpt = ""
    ),
    globals.io_lookup_bool_option(gcc_global_registers, GCC_Regs, !IO),
    (
        GCC_Regs = yes,
        globals.io_lookup_string_option(cflags_for_regs, CFLAGS_FOR_REGS,
            !IO),
        RegOpt = "-DMR_USE_GCC_GLOBAL_REGISTERS "
    ;
        GCC_Regs = no,
        CFLAGS_FOR_REGS = "",
        RegOpt = ""
    ),
    globals.io_lookup_bool_option(gcc_non_local_gotos, GCC_Gotos, !IO),
    (
        GCC_Gotos = yes,
        GotoOpt = "-DMR_USE_GCC_NONLOCAL_GOTOS ",
        globals.io_lookup_string_option(cflags_for_gotos,
            CFLAGS_FOR_GOTOS, !IO)
    ;
        GCC_Gotos = no,
        GotoOpt = "",
        CFLAGS_FOR_GOTOS = ""
    ),
    globals.io_lookup_bool_option(asm_labels, ASM_Labels, !IO),
    (
        ASM_Labels = yes,
        AsmOpt = "-DMR_USE_ASM_LABELS "
    ;
        ASM_Labels = no,
        AsmOpt = ""
    ),
    globals.io_lookup_bool_option(parallel, Parallel, !IO),
    (
        Parallel = yes,
        globals.io_lookup_string_option(cflags_for_threads,
            CFLAGS_FOR_THREADS, !IO)
    ;
        Parallel = no,
        CFLAGS_FOR_THREADS = ""
    ),
    globals.io_get_gc_method(GC_Method, !IO),
    (
        GC_Method = gc_automatic,
        GC_Opt = ""
    ;
        GC_Method = gc_none,
        GC_Opt = ""
    ;
        GC_Method = gc_boehm,
        GC_Opt = "-DMR_CONSERVATIVE_GC -DMR_BOEHM_GC "
    ;
        GC_Method = gc_mps,
        GC_Opt = "-DMR_CONSERVATIVE_GC -DMR_MPS_GC "
    ;
        GC_Method = gc_accurate,
        GC_Opt = "-DMR_NATIVE_GC "
    ),
    globals.io_lookup_bool_option(profile_calls, ProfileCalls, !IO),
    (
        ProfileCalls = yes,
        ProfileCallsOpt = "-DMR_MPROF_PROFILE_CALLS "
    ;
        ProfileCalls = no,
        ProfileCallsOpt = ""
    ),
    globals.io_lookup_bool_option(profile_time, ProfileTime, !IO),
    (
        ProfileTime = yes,
        ProfileTimeOpt = "-DMR_MPROF_PROFILE_TIME "
    ;
        ProfileTime = no,
        ProfileTimeOpt = ""
    ),
    globals.io_lookup_bool_option(profile_memory, ProfileMemory, !IO),
    (
        ProfileMemory = yes,
        ProfileMemoryOpt = "-DMR_MPROF_PROFILE_MEMORY "
    ;
        ProfileMemory = no,
        ProfileMemoryOpt = ""
    ),
    globals.io_lookup_bool_option(profile_deep, ProfileDeep, !IO),
    (
        ProfileDeep = yes,
        ProfileDeepOpt = "-DMR_DEEP_PROFILING "
    ;
        ProfileDeep = no,
        ProfileDeepOpt = ""
    ),
    globals.io_lookup_bool_option(record_term_sizes_as_words,
        RecordTermSizesAsWords, !IO),
    globals.io_lookup_bool_option(record_term_sizes_as_cells,
        RecordTermSizesAsCells, !IO),
    (
        RecordTermSizesAsWords = yes,
        RecordTermSizesAsCells = yes,
        % this should have been caught in handle_options
        unexpected(this_file,
            "compile_c_file: inconsistent record term size options")
    ;
        RecordTermSizesAsWords = yes,
        RecordTermSizesAsCells = no,
        RecordTermSizesOpt = "-DMR_RECORD_TERM_SIZES "
    ;
        RecordTermSizesAsWords = no,
        RecordTermSizesAsCells = yes,
        RecordTermSizesOpt = "-DMR_RECORD_TERM_SIZES " ++
            "-DMR_RECORD_TERM_SIZES_AS_CELLS "
    ;
        RecordTermSizesAsWords = no,
        RecordTermSizesAsCells = no,
        RecordTermSizesOpt = ""
    ),
    (
        PIC = pic,
        globals.io_lookup_string_option(cflags_for_pic, CFLAGS_FOR_PIC, !IO),
        PIC_Reg = yes
    ;
        PIC = link_with_pic,
        CFLAGS_FOR_PIC = "",
        PIC_Reg = yes
    ;
        PIC = non_pic,
        CFLAGS_FOR_PIC = "",
        globals.io_lookup_bool_option(pic_reg, PIC_Reg, !IO)
    ),
    (
        PIC_Reg = yes,
        % This will be ignored for architectures/grades where use of position
        % independent code does not reserve a register.
        PIC_Reg_Opt = "-DMR_PIC_REG "
    ;
        PIC_Reg = no,
        PIC_Reg_Opt = ""
    ),

    globals.io_get_tags_method(Tags_Method, !IO),
    ( Tags_Method = tags_high ->
        TagsOpt = "-DMR_HIGHTAGS "
    ;
        TagsOpt = ""
    ),
    globals.io_lookup_int_option(num_tag_bits, NumTagBits, !IO),
    string.int_to_string(NumTagBits, NumTagBitsString),
    string.append_list(["-DMR_TAGBITS=", NumTagBitsString, " "],
        NumTagBitsOpt),
    globals.io_lookup_bool_option(decl_debug, DeclDebug, !IO),
    (
        DeclDebug = yes,
        DeclDebugOpt = "-DMR_DECL_DEBUG "
    ;
        DeclDebug = no,
        DeclDebugOpt = ""
    ),
    globals.io_lookup_bool_option(exec_trace, ExecTrace, !IO),
    (
        ExecTrace = yes,
        ExecTraceOpt = "-DMR_EXEC_TRACE "
    ;
        ExecTrace = no,
        ExecTraceOpt = ""
    ),
    globals.io_lookup_bool_option(extend_stacks_when_needed, Extend, !IO),
    (
        Extend = yes,
        ExtendOpt = "-DMR_EXTEND_STACKS_WHEN_NEEDED "
    ;
        Extend = no,
        ExtendOpt = ""
    ),
    globals.io_lookup_bool_option(target_debug, Target_Debug, !IO),
    (
        Target_Debug = yes,
        globals.io_lookup_string_option(cflags_for_debug, Target_DebugOpt0,
            !IO),
        string.append(Target_DebugOpt0, " ", Target_DebugOpt)
    ;
        Target_Debug = no,
        Target_DebugOpt = ""
    ),
    globals.io_lookup_bool_option(low_level_debug, LL_Debug, !IO),
    (
        LL_Debug = yes,
        LL_DebugOpt = "-DMR_LOW_LEVEL_DEBUG "
    ;
        LL_Debug = no,
        LL_DebugOpt = ""
    ),
    globals.io_lookup_bool_option(use_trail, UseTrail, !IO),
    (
        UseTrail = yes,
        UseTrailOpt = "-DMR_USE_TRAIL "
    ;
        UseTrail = no,
        UseTrailOpt = ""
    ),
    globals.io_lookup_bool_option(reserve_tag, ReserveTag, !IO),
    (
        ReserveTag = yes,
        ReserveTagOpt = "-DMR_RESERVE_TAG "
    ;
        ReserveTag = no,
        ReserveTagOpt = ""
    ),
    globals.io_lookup_bool_option(use_minimal_model_stack_copy,
        MinimalModelStackCopy, !IO),
    globals.io_lookup_bool_option(use_minimal_model_own_stacks,
        MinimalModelOwnStacks, !IO),
    (
        MinimalModelStackCopy = yes,
        MinimalModelOwnStacks = yes,
        % this should have been caught in handle_options
        unexpected(this_file,
            "compile_c_file: inconsistent minimal model options")
    ;
        MinimalModelStackCopy = yes,
        MinimalModelOwnStacks = no,
        MinimalModelBaseOpt = "-DMR_USE_MINIMAL_MODEL_STACK_COPY "
    ;
        MinimalModelStackCopy = no,
        MinimalModelOwnStacks = yes,
        MinimalModelBaseOpt = "-DMR_USE_MINIMAL_MODEL_OWN_STACKS "
    ;
        MinimalModelStackCopy = no,
        MinimalModelOwnStacks = no,
        MinimalModelBaseOpt = ""
    ),
    globals.io_lookup_bool_option(minimal_model_debug, MinimalModelDebug,
        !IO),
    (
        MinimalModelDebug = yes,
        ( MinimalModelBaseOpt = "" ->
            % We ignore the debug flag unless one of the base flags is set.
            MinimalModelOpt = MinimalModelBaseOpt
        ;
            MinimalModelOpt = MinimalModelBaseOpt ++ "-DMR_MINIMAL_MODEL_DEBUG"
        )
    ;
        MinimalModelDebug = no,
        MinimalModelOpt = MinimalModelBaseOpt
    ),
    globals.io_lookup_bool_option(type_layout, TypeLayoutOption, !IO),
    (
        TypeLayoutOption = no,
        TypeLayoutOpt = "-DMR_NO_TYPE_LAYOUT "
    ;
        TypeLayoutOption = yes,
        TypeLayoutOpt = ""
    ),
    globals.io_lookup_bool_option(c_optimize, C_optimize, !IO),
    (
        C_optimize = yes,
        globals.io_lookup_string_option(cflags_for_optimization, OptimizeOpt,
            !IO)
    ;
        C_optimize = no,
        OptimizeOpt = ""
    ),
    globals.io_lookup_bool_option(ansi_c, Ansi, !IO),
    (
        Ansi = yes,
        globals.io_lookup_string_option(cflags_for_ansi, AnsiOpt, !IO)
    ;
        Ansi = no,
        AnsiOpt = ""
    ),
    globals.io_lookup_bool_option(inline_alloc, InlineAlloc, !IO),
    (
        InlineAlloc = yes,
        % XXX disabled because inline allocation is broken in gc7.0 alpha6.
        % InlineAllocOpt = "-DMR_INLINE_ALLOC "
        InlineAllocOpt = ""
    ;
        InlineAlloc = no,
        InlineAllocOpt = ""
    ),
    globals.io_lookup_bool_option(warn_target_code, Warn, !IO),
    (
        Warn = yes,
        globals.io_lookup_string_option(cflags_for_warnings,
            WarningOpt, !IO)
    ;
        Warn = no,
        WarningOpt = ""
    ),

    % The -floop-optimize option is incompatible with the global
    % register code we generate on Darwin PowerPC.
    % See the hard_coded/ppc_bug test case for an example
    % program which fails with this optimization.

    globals.io_lookup_string_option(fullarch, FullArch, !IO),
    (
        HighLevelCode = no,
        GCC_Regs = yes,
        string.prefix(FullArch, "powerpc-apple-darwin")
    ->
        AppleGCCRegWorkaroundOpt = "-fno-loop-optimize"
    ;
        AppleGCCRegWorkaroundOpt = ""
    ),

    % Be careful with the order here!  Some options override others,
    % e.g. CFLAGS_FOR_REGS must come after OptimizeOpt so that
    % it can override -fomit-frame-pointer with -fno-omit-frame-pointer.
    % Also be careful that each option is separated by spaces.
    string.append_list([
        CC, " ", 
        SubDirInclOpt, InclOpt,
        OptimizeOpt, " ",
        HighLevelCodeOpt, 
        NestedFunctionsOpt, 
        HighLevelDataOpt,
        RegOpt, GotoOpt, AsmOpt,
        CFLAGS_FOR_REGS, " ", CFLAGS_FOR_GOTOS, " ",
        CFLAGS_FOR_THREADS, " ", CFLAGS_FOR_PIC, " ",
        GC_Opt, 
        ProfileCallsOpt, ProfileTimeOpt, 
        ProfileMemoryOpt, ProfileDeepOpt, 
        RecordTermSizesOpt, 
        PIC_Reg_Opt, 
        TagsOpt, NumTagBitsOpt, 
        ExtendOpt,
        Target_DebugOpt, LL_DebugOpt, DeclDebugOpt, ExecTraceOpt,
        UseTrailOpt, 
        ReserveTagOpt, 
        MinimalModelOpt, 
        TypeLayoutOpt,
        InlineAllocOpt, " ", 
        AnsiOpt, " ", 
        AppleGCCRegWorkaroundOpt, " ", 
        WarningOpt, " ", 
        CFLAGS, 
        " -c ", C_File, " ",
        NameObjectFile, O_File], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands,
        Command, Succeeded, !IO).

%-----------------------------------------------------------------------------%

compile_java_file(ErrorStream, JavaFile, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Compiling `", !IO),
    maybe_write_string(Verbose, JavaFile, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(java_compiler, JavaCompiler, !IO),
    globals.io_lookup_accumulating_option(java_flags, JavaFlagsList, !IO),
    join_string_list(JavaFlagsList, "", "", " ", JAVAFLAGS),

    globals.io_lookup_accumulating_option(java_classpath, Java_Incl_Dirs,
        !IO),
    % XXX PathSeparator should be ";" on Windows
    PathSeparator = ":",
    % We prepend the current CLASSPATH to preserve the accumulating
    % nature of this variable.
    get_env_classpath(EnvClasspath, !IO),
    join_string_list([EnvClasspath|Java_Incl_Dirs], "", "",
        PathSeparator, ClassPath),
    ( ClassPath = "" ->
        InclOpt = ""
    ;
        InclOpt = string.append_list([
            "-classpath ", quote_arg(ClassPath), " "])
    ),

    globals.io_lookup_bool_option(target_debug, Target_Debug, !IO),
    (
        Target_Debug = yes,
        Target_DebugOpt = "-g "
    ;
        Target_Debug = no,
        Target_DebugOpt = ""
    ),

    globals.io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    globals.io_lookup_string_option(fullarch, FullArch, !IO),
    globals.io_get_globals(Globals, !IO),
    (
        UseSubdirs = yes,
        (
            UseGradeSubdirs = yes,
            grade_directory_component(Globals, Grade),
            DirName = "Mercury"/Grade/FullArch/"Mercury"/"classs"
        ;
            UseGradeSubdirs = no,
            DirName = "Mercury"/"classs"
        ),
        % Javac won't create the destination directory for class files,
        % so we need to do it.
        dir.make_directory(DirName, _, !IO),
        % Set destination directory for class files.
        DestDir = "-d " ++ DirName ++ " "
    ;
        UseSubdirs = no,
        DestDir = ""
    ),

    % Be careful with the order here!  Some options may override others.
    % Also be careful that each option is separated by spaces.
    string.append_list([JavaCompiler, " ", InclOpt, DestDir,
        Target_DebugOpt, JAVAFLAGS, " ", JavaFile], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands, Command,
        Succeeded, !IO).

%-----------------------------------------------------------------------------%

assemble(ErrorStream, PIC, ModuleName, Succeeded, !IO) :-
    (
        PIC = pic,
        AsmExt = ".pic_s",
        GCCFLAGS_FOR_ASM = "-x assembler ",
        GCCFLAGS_FOR_PIC = "-fpic "
    ;
        PIC = link_with_pic,
        % `--target asm' doesn't support any grades for
        % which `.lpic_o' files are needed.
        unexpected(this_file, "assemble: link_with_pic")
    ;
        PIC = non_pic,
        AsmExt = ".s",
        GCCFLAGS_FOR_ASM = "",
        GCCFLAGS_FOR_PIC = ""
    ),
    module_name_to_file_name(ModuleName, AsmExt, no, AsmFile, !IO),
    maybe_pic_object_file_extension(PIC, ObjExt, !IO),
    module_name_to_file_name(ModuleName, ObjExt, yes, ObjFile, !IO),

    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Assembling `", !IO),
    maybe_write_string(Verbose, AsmFile, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    % XXX should we use new asm_* options rather than
    % reusing cc, cflags, c_flag_to_name_object_file?
    globals.io_lookup_string_option(cc, CC, !IO),
    globals.io_lookup_string_option(c_flag_to_name_object_file,
        NameObjectFile, !IO),
    globals.io_lookup_accumulating_option(cflags, C_Flags_List, !IO),
    join_string_list(C_Flags_List, "", "", " ", CFLAGS),
    % Be careful with the order here.
    % Also be careful that each option is separated by spaces.
    string.append_list([CC, " ", CFLAGS, " ", GCCFLAGS_FOR_PIC,
        GCCFLAGS_FOR_ASM, "-c ", AsmFile, " ", NameObjectFile, ObjFile],
        Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands, Command,
        Succeeded, !IO).

%-----------------------------------------------------------------------------%

make_init_file(ErrorStream, MainModuleName, AllModules, Succeeded, !IO) :-
    module_name_to_file_name(MainModuleName, ".init.tmp", yes, TmpInitFileName,
        !IO),
    io.open_output(TmpInitFileName, InitFileRes, !IO),
    (
        InitFileRes = ok(InitFileStream),
        ModuleNameToCFileName =
            (pred(ThisModule::in, CFileName::out, !.IO::di, !:IO::uo) is det :-
                module_name_to_file_name(ThisModule, ".c", no, CFileName, !IO)
        ),
        list.map_foldl(ModuleNameToCFileName, AllModules, AllCFilesList, !IO),
        join_quoted_string_list(AllCFilesList, "", "", " ", CFileNames),
        
        globals.io_lookup_string_option(mkinit_command, MkInit, !IO),
        MkInitCmd = string.append_list(
            [   MkInit,
                " -k ",
                " ", CFileNames
            ]),
        invoke_system_command(InitFileStream, cmd_verbose, MkInitCmd, MkInitOK,
            !IO),
       
        ( 
            MkInitOK =  yes,
            globals.io_lookup_maybe_string_option(extra_init_command,
                MaybeInitFileCommand, !IO),
            (
                MaybeInitFileCommand = yes(InitFileCommand),
                make_all_module_command(InitFileCommand, MainModuleName,
                    AllModules, CommandString, !IO),
                invoke_system_command(InitFileStream, cmd_verbose_commands,
                    CommandString, Succeeded0, !IO)
            ;
                MaybeInitFileCommand = no,
                Succeeded0 = yes
            )
        ;
            MkInitOK   = no,
            Succeeded0 = no
        ),

        io.close_output(InitFileStream, !IO),
        module_name_to_file_name(MainModuleName, ".init", yes, InitFileName,
            !IO),
        update_interface_return_succeeded(InitFileName, Succeeded1, !IO),
        Succeeded = Succeeded0 `and` Succeeded1
    ;
        InitFileRes = error(Error),
        io.progname_base("mercury_compile", ProgName, !IO),
        io.write_string(ErrorStream, ProgName, !IO),
        io.write_string(ErrorStream, ": can't open `", !IO),
        io.write_string(ErrorStream, TmpInitFileName, !IO),
        io.write_string(ErrorStream, "' for output:\n", !IO),
        io.nl(ErrorStream, !IO),
        io.write_string(ErrorStream, io.error_message(Error), !IO),
        io.nl(ErrorStream, !IO),
        Succeeded = no
    ).

%-----------------------------------------------------------------------------%

link_module_list(Modules, FactTableObjFiles, Succeeded, !IO) :-
    globals.io_lookup_string_option(output_file_name, OutputFileName0, !IO),
    ( OutputFileName0 = "" ->
        (
            Modules = [Module | _],
            OutputFileName = Module
        ;
            Modules = [],
            unexpected(this_file, "link_module_list: no modules")
        )
    ;
        OutputFileName = OutputFileName0
    ),

    file_name_to_module_name(OutputFileName, MainModuleName),

    globals.io_lookup_bool_option(compile_to_shared_lib, CompileToSharedLib,
        !IO),
    TargetType = (CompileToSharedLib = yes -> shared_library ; executable),
    get_object_code_type(TargetType, PIC, !IO),
    maybe_pic_object_file_extension(PIC, Obj, !IO),

    globals.io_get_target(Target, !IO),
    io.output_stream(OutputStream, !IO),
    ( Target = target_asm ->
        % For --target asm, we generate everything into a single object file.
        (
            Modules = [FirstModule | _],
            join_module_list([FirstModule], Obj, ObjectsList, !IO)
        ;
            Modules = [],
            unexpected(this_file, "link_module_list: no modules")
        )
    ;
        join_module_list(Modules, Obj, ObjectsList, !IO)
    ),
    ( TargetType = executable ->
        list.map(
            (pred(ModuleStr::in, ModuleName::out) is det :-
                file_name_to_module_name(dir.basename_det(ModuleStr),
                    ModuleName)
            ), Modules, ModuleNames),
        MustCompile = yes,
        make_init_obj_file(OutputStream, MustCompile, MainModuleName,
            ModuleNames, InitObjResult, !IO)
    ;
        InitObjResult = yes("")
    ),
    (
        InitObjResult = yes(InitObjFileName),
        globals.io_lookup_accumulating_option(link_objects,
            ExtraLinkObjectsList, !IO),
        AllObjects0 = ObjectsList ++ ExtraLinkObjectsList
            ++ FactTableObjFiles,
        AllObjects =
            ( InitObjFileName = "" ->
                AllObjects0
            ;
                [InitObjFileName | AllObjects0]
            ),
        link(OutputStream, TargetType, MainModuleName, AllObjects,
            Succeeded, !IO)
    ;
        InitObjResult = no,
        Succeeded = no
    ).

make_init_obj_file(ErrorStream, ModuleName, ModuleNames, Result, !IO) :-
    globals.io_lookup_bool_option(rebuild, MustCompile, !IO),
    make_init_obj_file(ErrorStream, MustCompile, ModuleName, ModuleNames,
        Result, !IO).

% WARNING: The code here duplicates the functionality of scripts/c2init.in.
% Any changes there may also require changes here, and vice versa.

:- pred make_init_obj_file(io.output_stream::in, bool::in,
    module_name::in, list(module_name)::in, maybe(file_name)::out,
    io::di, io::uo) is det.

make_init_obj_file(ErrorStream, MustCompile, ModuleName, ModuleNames, Result,
        !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(statistics, Stats, !IO),
    maybe_write_string(Verbose, "% Creating initialization file...\n", !IO),

    globals.io_get_globals(Globals, !IO),
    compute_grade(Globals, Grade),

    get_object_code_type(executable, PIC, !IO),
    maybe_pic_object_file_extension(PIC, ObjExt, !IO),
    InitObj = "_init" ++ ObjExt,

    module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName, !IO),
    module_name_to_file_name(ModuleName, InitObj, yes, InitObjFileName, !IO),

    list.map_foldl(
        (pred(ThisModule::in, CFileName::out, IO0::di, IO::uo) is det :-
            module_name_to_file_name(ThisModule, ".c", no, CFileName, IO0, IO)
        ), ModuleNames, CFileNameList, !IO),
    join_quoted_string_list(CFileNameList, "", "", " ", CFileNames),

    globals.io_lookup_accumulating_option(init_file_directories,
        InitFileDirsList, !IO),
    join_quoted_string_list(InitFileDirsList, "-I ", "", " ", InitFileDirs),

    globals.io_lookup_accumulating_option(init_files, InitFileNamesList0,
        !IO),
    globals.io_lookup_accumulating_option(trace_init_files,
        TraceInitFileNamesList0, !IO),
    globals.io_lookup_maybe_string_option(
        mercury_standard_library_directory, MaybeStdLibDir, !IO),
    (
        MaybeStdLibDir = yes(StdLibDir),
        InitFileNamesList1 = [StdLibDir/"modules"/"mer_rt.init",
            StdLibDir/"modules"/"mer_std.init" |
            InitFileNamesList0],
        TraceInitFileNamesList =
            [StdLibDir/"modules"/"mer_browser.init",
            StdLibDir/"modules"/"mer_mdbcomp.init" |
            TraceInitFileNamesList0]
    ;
        MaybeStdLibDir = no,
        InitFileNamesList1 = InitFileNamesList0,
        TraceInitFileNamesList = TraceInitFileNamesList0
    ),

    globals.io_get_trace_level(TraceLevel, !IO),
    ( given_trace_level_is_none(TraceLevel) = no ->
        TraceOpt = "-t",
        InitFileNamesList = InitFileNamesList1 ++ TraceInitFileNamesList
    ;
        TraceOpt = "",
        InitFileNamesList = InitFileNamesList1
    ),
    join_quoted_string_list(InitFileNamesList, "", "", " ", InitFileNames),

    globals.io_lookup_accumulating_option(runtime_flags, RuntimeFlagsList,
        !IO),
    join_quoted_string_list(RuntimeFlagsList, "-r ", "", " ", RuntimeFlags),

    globals.io_lookup_bool_option(extra_initialization_functions, ExtraInits,
        !IO),
    ExtraInitsOpt = ( ExtraInits = yes -> "-x" ; "" ),

    globals.io_lookup_bool_option(main, Main, !IO),
    NoMainOpt = ( Main = no -> "-l" ; "" ),

    globals.io_lookup_string_option(experimental_complexity,
        ExperimentalComplexity, !IO),
    ( ExperimentalComplexity = "" ->
        ExperimentalComplexityOpt = ""
    ;
        ExperimentalComplexityOpt = "-X " ++ ExperimentalComplexity
    ),

    globals.io_lookup_string_option(mkinit_command, Mkinit, !IO),
    TmpInitCFileName = InitCFileName ++ ".tmp",
    MkInitCmd = string.append_list(
        [   Mkinit,
            " -g ", Grade,
            " ", TraceOpt,
            " ", ExtraInitsOpt,
            " ", NoMainOpt,
            " ", ExperimentalComplexityOpt,
            " ", RuntimeFlags,
            " -o ", quote_arg(TmpInitCFileName),
            " ", InitFileDirs,
            " ", InitFileNames, 
            " ", CFileNames
        ]),
    invoke_system_command(ErrorStream, cmd_verbose, MkInitCmd, MkInitOK0, !IO),
    maybe_report_stats(Stats, !IO),
    (
        MkInitOK0 = yes,
        update_interface_return_succeeded(InitCFileName, MkInitOK1, !IO),
        (
            MkInitOK1 = yes,
            (
                MustCompile = yes,
                Compile = yes
            ;
                MustCompile = no,
                io.file_modification_time(InitCFileName,
                    InitCModTimeResult, !IO),
                io.file_modification_time(InitObjFileName,
                    InitObjModTimeResult, !IO),
                (
                    InitObjModTimeResult = ok(InitObjModTime),
                    InitCModTimeResult = ok(InitCModTime),
                    compare(TimeCompare, InitObjModTime, InitCModTime),
                    ( TimeCompare = (=)
                    ; TimeCompare = (>)
                    )
                ->
                    Compile = no
                ;
                    Compile = yes
                )
            ),
            (
                Compile = yes,
                maybe_write_string(Verbose,
                "% Compiling initialization file...\n", !IO),

                compile_c_file(ErrorStream, PIC, InitCFileName,
                    InitObjFileName, CompileOK, !IO),
                maybe_report_stats(Stats, !IO),
                (
                    CompileOK = no,
                    Result = no
                ;
                    CompileOK = yes,
                    Result = yes(InitObjFileName)
                )
            ;
                Compile = no,
                Result = yes(InitObjFileName)
            )
        ;
            MkInitOK1 = no,
            Result = no
        )
    ;
        MkInitOK0 = no,
        Result = no
    ).

% WARNING: The code here duplicates the functionality of scripts/ml.in.
% Any changes there may also require changes here, and vice versa.

link(ErrorStream, LinkTargetType, ModuleName, ObjectsList, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(statistics, Stats, !IO),

    maybe_write_string(Verbose, "% Linking...\n", !IO),
    link_output_filename(LinkTargetType, ModuleName, _Ext, OutputFileName, !IO),
    ( LinkTargetType = static_library ->
        create_archive(ErrorStream, OutputFileName, yes, ObjectsList,
            LinkSucceeded, !IO)
    ; LinkTargetType = java_archive ->
        create_java_archive(ErrorStream, ModuleName, OutputFileName,
            ObjectsList, LinkSucceeded, !IO)
    ;
        (
            LinkTargetType = shared_library,
            CommandOpt = link_shared_lib_command,
            RpathFlagOpt = shlib_linker_rpath_flag,
            RpathSepOpt = shlib_linker_rpath_separator,
            LDFlagsOpt = ld_libflags,
            ThreadFlagsOpt = shlib_linker_thread_flags,
            DebugFlagsOpt = shlib_linker_debug_flags,
            TraceFlagsOpt = shlib_linker_trace_flags,
            globals.io_lookup_bool_option(allow_undefined, AllowUndef, !IO),
            (
                AllowUndef = yes,
                globals.io_lookup_string_option(
                    linker_allow_undefined_flag, UndefOpt, !IO)
            ;
                AllowUndef = no,
                globals.io_lookup_string_option(
                    linker_error_undefined_flag, UndefOpt, !IO)
            )
        ;
            LinkTargetType = executable,
            CommandOpt = link_executable_command,
            RpathFlagOpt = linker_rpath_flag,
            RpathSepOpt = linker_rpath_separator,
            LDFlagsOpt = ld_flags,
            ThreadFlagsOpt = linker_thread_flags,
            DebugFlagsOpt = linker_debug_flags,
            TraceFlagsOpt = linker_trace_flags,
            UndefOpt = ""
        ;
            LinkTargetType = static_library,
            unexpected(this_file, "compile_target_code.link")
        ;
            LinkTargetType = java_archive,
            unexpected(this_file, "compile_target_code.link")
        ),

        % Should the executable be stripped?
        globals.io_lookup_bool_option(strip, Strip, !IO),
        (
            LinkTargetType = executable,
            Strip = yes
        ->
            globals.io_lookup_string_option(linker_strip_flag, StripOpt, !IO)
        ;
            StripOpt = ""
        ),

        globals.io_lookup_bool_option(target_debug, TargetDebug, !IO),
        (
            TargetDebug = yes,
            globals.io_lookup_string_option(DebugFlagsOpt, DebugOpts, !IO)
        ;
            TargetDebug = no,
            DebugOpts = ""
        ),

        % Should the executable be statically linked?
        globals.io_lookup_string_option(linkage, Linkage, !IO),
        (
            LinkTargetType = executable,
            Linkage = "static"
        ->
            globals.io_lookup_string_option(linker_static_flags, StaticOpts,
                !IO)
        ;
            StaticOpts = ""
        ),

        % Are the thread libraries needed?
        use_thread_libs(UseThreadLibs, !IO),
        (
            UseThreadLibs = yes,
            globals.io_lookup_string_option(ThreadFlagsOpt, ThreadOpts, !IO)
        ;
            UseThreadLibs = no,
            ThreadOpts = ""
        ),

        % Find the Mercury standard libraries.
        globals.io_lookup_maybe_string_option(
            mercury_standard_library_directory, MaybeStdLibDir, !IO),
        (
            MaybeStdLibDir = yes(StdLibDir),
            get_mercury_std_libs(LinkTargetType, StdLibDir, MercuryStdLibs,
                !IO)
        ;
            MaybeStdLibDir = no,
            MercuryStdLibs = ""
        ),

        % Find which system libraries are needed.
        get_system_libs(LinkTargetType, SystemLibs, !IO),

        join_quoted_string_list(ObjectsList, "", "", " ", Objects),
        globals.io_lookup_accumulating_option(LDFlagsOpt, LDFlagsList, !IO),
        join_string_list(LDFlagsList, "", "", " ", LDFlags),
        globals.io_lookup_accumulating_option(link_library_directories,
            LinkLibraryDirectoriesList, !IO),
        globals.io_lookup_string_option(linker_path_flag, LinkerPathFlag,
            !IO),
        join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
            " ", LinkLibraryDirectories),

        % Set up the runtime library path.
        globals.io_lookup_bool_option(shlib_linker_use_install_name,
            UseInstallName, !IO),
        shared_libraries_supported(SharedLibsSupported, !IO),
        (
            UseInstallName = no,
            SharedLibsSupported = yes,
            ( Linkage = "shared"
            ; LinkTargetType = shared_library
            )
        ->
            globals.io_lookup_accumulating_option(
                runtime_link_library_directories, RpathDirs, !IO),
            ( 
                RpathDirs = [],
                RpathOpts = ""
            ;
                RpathDirs = [_|_],
                globals.io_lookup_string_option(RpathSepOpt, RpathSep, !IO),
                globals.io_lookup_string_option(RpathFlagOpt, RpathFlag, !IO),
                RpathOpts0 = string.join_list(RpathSep, RpathDirs),
                RpathOpts = RpathFlag ++ RpathOpts0
            )
        ;
            RpathOpts = ""
        ),
                
        % Set up the install name for shared libraries.
        (
            UseInstallName = yes,
            LinkTargetType = shared_library
        ->  
            % NOTE: `ShLibFileName' must *not* be prefixed with a directory.
            %       get_install_name_option will prefix it with the correct
            %       directory which is the one where the library is going to
            %       be installed, *not* where it is going to be built. 
            %        
            BaseFileName = sym_name_to_string(ModuleName),
            globals.io_lookup_string_option(shared_library_extension,
                SharedLibExt, !IO),
            ShLibFileName = "lib" ++ BaseFileName ++ SharedLibExt,
            get_install_name_option(ShLibFileName, InstallNameOpt, !IO)
        ;
            InstallNameOpt = ""
        ),

        globals.io_get_trace_level(TraceLevel, !IO),
        ( given_trace_level_is_none(TraceLevel) = yes ->
            TraceOpts = ""
        ;
            globals.io_lookup_string_option(TraceFlagsOpt, TraceOpts, !IO)
        ),

        % Pass either `-llib' or `PREFIX/lib/GRADE/liblib.a',
        % depending on whether we are linking with static or shared
        % Mercury libraries.

        globals.io_lookup_accumulating_option(
            mercury_library_directories, MercuryLibDirs0, !IO),
        globals.io_get_globals(Globals, !IO),
        grade_directory_component(Globals, GradeDir),
        MercuryLibDirs = list.map(
            (func(LibDir) = LibDir/"lib"/GradeDir),
            MercuryLibDirs0),
        globals.io_lookup_accumulating_option(link_libraries,
            LinkLibrariesList0, !IO),
        list.map_foldl2(process_link_library(MercuryLibDirs),
            LinkLibrariesList0, LinkLibrariesList, yes,
            LibrariesSucceeded, !IO),

        globals.io_lookup_string_option(linker_opt_separator,
            LinkOptSep, !IO),
        (
            LibrariesSucceeded = yes,
            join_quoted_string_list(LinkLibrariesList, "", "", " ",
                LinkLibraries),

            % Note that LDFlags may contain `-l' options so it should come
            % after Objects.
            globals.io_lookup_string_option(CommandOpt, Command, !IO),
            string.append_list(
                [Command, " ",
                StaticOpts, " ", StripOpt, " ", UndefOpt, " ",
                ThreadOpts, " ", TraceOpts, " ",
                " -o ", OutputFileName, " ", Objects, " ",
                LinkOptSep, " ", LinkLibraryDirectories, " ",
                RpathOpts, " ", InstallNameOpt, " ", DebugOpts,
                " ", LDFlags, " ", LinkLibraries, " ",
                MercuryStdLibs, " ", SystemLibs],
                LinkCmd),

            globals.io_lookup_bool_option(demangle, Demangle, !IO),
            (
                Demangle = yes,
                globals.io_lookup_string_option(demangle_command,
                    DemamngleCmd, !IO),
                MaybeDemangleCmd = yes(DemamngleCmd)
            ;
                Demangle = no,
                MaybeDemangleCmd = no
            ),

            invoke_system_command(ErrorStream, cmd_verbose_commands, LinkCmd,
                MaybeDemangleCmd, LinkSucceeded, !IO)
        ;
            LibrariesSucceeded = no,
            LinkSucceeded = no
        )
    ),
    maybe_report_stats(Stats, !IO),
    (
        LinkSucceeded = yes,
        post_link_make_symlink_or_copy(ErrorStream, LinkTargetType,
            ModuleName, Succeeded, !IO)
    ;
        LinkSucceeded = no,
        Succeeded = no
    ).

:- pred link_output_filename(linked_target_type::in, module_name::in,
    string::out, string::out, io::di, io::uo) is det.

link_output_filename(LinkTargetType, ModuleName, Ext, OutputFileName, !IO) :-
    (
        LinkTargetType = static_library,
        globals.io_lookup_string_option(library_extension, Ext, !IO),
        module_name_to_lib_file_name("lib", ModuleName, Ext, yes,
            OutputFileName, !IO)
    ;
        LinkTargetType = shared_library,
        globals.io_lookup_string_option(shared_library_extension, Ext, !IO),
        module_name_to_lib_file_name("lib", ModuleName, Ext, yes,
            OutputFileName, !IO)
    ;
        LinkTargetType = java_archive,
        Ext = ".jar",
        module_name_to_file_name(ModuleName, Ext, yes, OutputFileName, !IO)
    ;
        LinkTargetType = executable,
        globals.io_lookup_string_option(executable_file_extension, Ext, !IO),
        module_name_to_file_name(ModuleName, Ext, yes, OutputFileName, !IO)
    ).

    % Find the standard Mercury libraries, and the system
    % libraries needed by them.
    %
:- pred get_mercury_std_libs(linked_target_type::in, dir_name::in, string::out,
    io::di, io::uo) is det.

get_mercury_std_libs(TargetType, StdLibDir, StdLibs, !IO) :-
    globals.io_get_gc_method(GCMethod, !IO),
    globals.io_lookup_string_option(library_extension, LibExt, !IO),
    globals.io_get_globals(Globals, !IO),
    grade_directory_component(Globals, GradeDir),

    % GC libraries.
    (
        GCMethod = gc_automatic,
        StaticGCLibs = "",
        SharedGCLibs = ""
    ;
        GCMethod = gc_none,
        StaticGCLibs = "",
        SharedGCLibs = ""
    ;
        GCMethod = gc_boehm,
        globals.io_lookup_bool_option(profile_time, ProfTime, !IO),
        globals.io_lookup_bool_option(profile_deep, ProfDeep, !IO),
        (
            ( ProfTime = yes
            ; ProfDeep = yes
            )
        ->
            GCGrade0 = "gc_prof"
        ;
            GCGrade0 = "gc"
        ),
        globals.io_lookup_bool_option(parallel, Parallel, !IO),
        (
            Parallel = yes,
            GCGrade = "par_" ++ GCGrade0
        ;
            Parallel = no,
            GCGrade = GCGrade0
        ),
        make_link_lib(TargetType, GCGrade, SharedGCLibs, !IO),
        StaticGCLibs = quote_arg(StdLibDir/"lib"/
            ("lib" ++ GCGrade ++ LibExt))
    ;
        GCMethod = gc_mps,
        make_link_lib(TargetType, "mps", SharedGCLibs, !IO),
        StaticGCLibs = quote_arg(StdLibDir/"lib"/
            ("libmps" ++ LibExt) )
    ;
        GCMethod = gc_accurate,
        StaticGCLibs = "",
        SharedGCLibs = ""
    ),

    % Trace libraries.
    globals.io_get_trace_level(TraceLevel, !IO),
    ( given_trace_level_is_none(TraceLevel) = yes ->
        StaticTraceLibs = "",
        SharedTraceLibs = ""
    ;
        StaticTraceLibs =
            quote_arg(StdLibDir/"lib"/GradeDir/
                ("libmer_trace" ++ LibExt)) ++
            " " ++
            quote_arg(StdLibDir/"lib"/GradeDir/
                ("libmer_browser" ++ LibExt)) ++
            " " ++
            quote_arg(StdLibDir/"lib"/GradeDir/
                ("libmer_mdbcomp" ++ LibExt)),
        make_link_lib(TargetType, "mer_trace", TraceLib, !IO),
        make_link_lib(TargetType, "mer_browser", BrowserLib, !IO),
        make_link_lib(TargetType, "mer_mdbcomp", MdbCompLib, !IO),
        SharedTraceLibs = string.join_list(" ",
            [TraceLib, BrowserLib, MdbCompLib])
    ),

    globals.io_lookup_string_option(mercury_linkage, MercuryLinkage, !IO),
    ( MercuryLinkage = "static" ->
        StdLibs = string.join_list(" ",
            [StaticTraceLibs,
            quote_arg(StdLibDir/"lib"/GradeDir/
                ("libmer_std" ++ LibExt)),
            quote_arg(StdLibDir/"lib"/GradeDir/
                ("libmer_rt" ++ LibExt)),
            StaticGCLibs])
    ; MercuryLinkage = "shared" ->
        make_link_lib(TargetType, "mer_std", StdLib, !IO),
        make_link_lib(TargetType, "mer_rt", RuntimeLib, !IO),
        StdLibs = string.join_list(" ",
            [SharedTraceLibs, StdLib, RuntimeLib, SharedGCLibs])
    ;
        unexpected(this_file, "unknown linkage " ++ MercuryLinkage)
    ).

:- pred make_link_lib(linked_target_type::in, string::in, string::out,
    io::di, io::uo) is det.

make_link_lib(TargetType, LibName, LinkOpt, !IO) :-
    (
        TargetType = executable,
        LinkLibFlag = linker_link_lib_flag,
        LinkLibSuffix = linker_link_lib_suffix
    ;
        TargetType = shared_library,
        LinkLibFlag = shlib_linker_link_lib_flag,
        LinkLibSuffix = shlib_linker_link_lib_suffix
    ;
        TargetType = java_archive,
        unexpected(this_file, "make_link_lib: java_archive")
    ;
        TargetType = static_library,
        unexpected(this_file, "make_link_lib: static_library")
    ),
    globals.io_lookup_string_option(LinkLibFlag, LinkLibOpt, !IO),
    globals.io_lookup_string_option(LinkLibSuffix, Suffix, !IO),
    LinkOpt = quote_arg(LinkLibOpt ++ LibName ++ Suffix).

:- pred get_system_libs(linked_target_type::in, string::out, io::di, io::uo)
    is det.

get_system_libs(TargetType, SystemLibs, !IO) :-
    % System libraries used when tracing.
    globals.io_get_trace_level(TraceLevel, !IO),
    ( given_trace_level_is_none(TraceLevel) = yes ->
        SystemTraceLibs = ""
    ;
        globals.io_lookup_string_option(trace_libs, SystemTraceLibs0, !IO),
        globals.io_lookup_bool_option(use_readline, UseReadline, !IO),
        (
            UseReadline = yes,
            globals.io_lookup_string_option(readline_libs, ReadlineLibs, !IO),
            SystemTraceLibs = SystemTraceLibs0 ++ " " ++ ReadlineLibs
        ;
            UseReadline = no,
            SystemTraceLibs = SystemTraceLibs0
        )
    ),

    % Thread libraries
    use_thread_libs(UseThreadLibs, !IO),
    (
        UseThreadLibs = yes,
        globals.io_lookup_string_option(thread_libs, ThreadLibs, !IO)
    ;
        UseThreadLibs = no,
        ThreadLibs = ""
    ),

    % Other system libraries.
    (
        TargetType = shared_library,
        globals.io_lookup_string_option(shared_libs, OtherSystemLibs, !IO)
    ;
        TargetType = static_library,
        unexpected(this_file, "get_std_libs: static library")
    ;
        TargetType = java_archive,
        unexpected(this_file, "get_std_libs: java archive")
    ;
        TargetType = executable,
        globals.io_lookup_string_option(math_lib, OtherSystemLibs, !IO)
    ),

    SystemLibs = string.join_list(" ",
        [SystemTraceLibs, OtherSystemLibs, ThreadLibs]).

:- pred use_thread_libs(bool::out, io::di, io::uo) is det.

use_thread_libs(UseThreadLibs, !IO) :-
    globals.io_lookup_bool_option(parallel, Parallel, !IO),
    globals.io_get_gc_method(GCMethod, !IO),
    UseThreadLibs = ( ( Parallel = yes ; GCMethod = gc_mps ) -> yes ; no ).

post_link_make_symlink_or_copy(ErrorStream, LinkTargetType, ModuleName,
        Succeeded, !IO) :-
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    (
        UseGradeSubdirs = yes,
        link_output_filename(LinkTargetType, ModuleName,
            Ext, OutputFileName, !IO),
        % Link/copy the executable into the user's directory.
        globals.io_set_option(use_subdirs, bool(no), !IO),
        globals.io_set_option(use_grade_subdirs, bool(no), !IO),
        ( LinkTargetType = executable ->
            module_name_to_file_name(ModuleName, Ext, no, UserDirFileName, !IO)
        ;
            module_name_to_lib_file_name("lib", ModuleName, Ext, no,
                UserDirFileName, !IO)
        ),
        globals.io_set_option(use_subdirs, bool(yes), !IO),
        globals.io_set_option(use_grade_subdirs, bool(yes), !IO),

        io.set_output_stream(ErrorStream, OutputStream, !IO),
        % Remove the target of the symlink/copy in case it already exists.
        io.remove_file(UserDirFileName, _, !IO),
        make_symlink_or_copy_file(OutputFileName, UserDirFileName, Succeeded,
            !IO),
        io.set_output_stream(OutputStream, _, !IO)
    ;
        UseGradeSubdirs = no,
        Succeeded = yes
    ).

shared_libraries_supported(Supported, !IO) :-
    % XXX This seems to be the standard way to check whether shared libraries
    % are supported but it's not very nice.
    globals.io_lookup_string_option(library_extension, LibExt, !IO),
    globals.io_lookup_string_option(shared_library_extension, SharedLibExt,
        !IO),
    Supported = (if LibExt \= SharedLibExt then yes else no).

%-----------------------------------------------------------------------------%

:- pred process_link_library(list(dir_name)::in, string::in, string::out,
    bool::in, bool::out, io::di, io::uo) is det.

process_link_library(MercuryLibDirs, LibName, LinkerOpt, !Succeeded, !IO) :-
    globals.io_lookup_string_option(mercury_linkage, MercuryLinkage, !IO),
    globals.io_lookup_accumulating_option(mercury_libraries, MercuryLibs,
        !IO),
    (
        MercuryLinkage = "static",
        list.member(LibName, MercuryLibs)
    ->
        % If we are linking statically with Mercury libraries, pass the
        % absolute pathname of the `.a' file for the library.
        globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),

        file_name_to_module_name(LibName, LibModuleName),
        globals.io_lookup_string_option(library_extension, LibExt, !IO),

        globals.io_set_option(use_grade_subdirs, bool(no), !IO),
        module_name_to_lib_file_name("lib", LibModuleName, LibExt, no,
            LibFileName, !IO),
        globals.io_set_option(use_grade_subdirs, bool(UseGradeSubdirs), !IO),

        io.input_stream(InputStream, !IO),
        search_for_file_returning_dir(MercuryLibDirs, LibFileName,
            SearchResult, !IO),
        (
            SearchResult = ok(DirName),
            LinkerOpt = DirName/LibFileName,
            io.set_input_stream(InputStream, LibInputStream, !IO),
            io.close_input(LibInputStream, !IO)
        ;
            SearchResult = error(Error),
            LinkerOpt = "",
            write_error_pieces_maybe_with_context(no, 0, [words(Error)], !IO),
            !:Succeeded = no
        )
    ;
        LinkerOpt = "-l" ++ LibName
    ).

:- pred create_archive(io.output_stream::in, file_name::in, bool::in,
    list(file_name)::in, bool::out, io::di, io::uo) is det.

create_archive(ErrorStream, LibFileName, Quote, ObjectList, Succeeded, !IO) :-
    globals.io_lookup_string_option(create_archive_command, ArCmd, !IO),
    globals.io_lookup_accumulating_option(create_archive_command_flags,
        ArFlagsList, !IO),
    join_string_list(ArFlagsList, "", "", " ", ArFlags),
    globals.io_lookup_string_option(create_archive_command_output_flag,
        ArOutputFlag, !IO),
    globals.io_lookup_string_option(ranlib_command, RanLib, !IO),
    (
        Quote = yes,
        join_quoted_string_list(ObjectList, "", "", " ", Objects)
    ;
        Quote = no,
        % Elements of ObjectList may contain shell wildcards, which
        % are intended to cause the element to expand to several words.
        % Quoting would prevent that.
        join_string_list(ObjectList, "", "", " ", Objects)
    ),
    MakeLibCmd = string.append_list([
        ArCmd, " ", ArFlags, " ", ArOutputFlag, " ",
        LibFileName, " ", Objects]),
    invoke_system_command(ErrorStream, cmd_verbose_commands,
        MakeLibCmd, MakeLibCmdSucceeded, !IO),
    (
        ( RanLib = ""
        ; MakeLibCmdSucceeded = no
        )
    ->
        Succeeded = MakeLibCmdSucceeded
    ;
        RanLibCmd = string.append_list([RanLib, " ", LibFileName]),
        invoke_system_command(ErrorStream, cmd_verbose_commands, RanLibCmd,
            Succeeded, !IO)
    ).

:- pred create_java_archive(io.output_stream::in, module_name::in,
    file_name::in, list(file_name)::in, bool::out, io::di, io::uo) is det.

create_java_archive(ErrorStream, ModuleName, JarFileName, ObjectList,
        Succeeded, !IO) :-
    % XXX Maybe these should be set up as options:
    Jar = "jar",
    JarCreateFlags = "cf",

    join_quoted_string_list(ObjectList, "", "", " ", Objects),
    list_class_files_for_jar(ModuleName, Objects, ListClassFiles, !IO),
    Cmd = string.append_list([
        Jar, " ", JarCreateFlags, " ", JarFileName, " ", ListClassFiles ]),

    invoke_system_command(ErrorStream, cmd_verbose_commands, Cmd, Succeeded,
        !IO).

get_object_code_type(FileType, ObjectCodeType, !IO) :-
    globals.io_lookup_string_option(pic_object_file_extension, PicObjExt, !IO),
    globals.io_lookup_string_option(link_with_pic_object_file_extension,
        LinkWithPicObjExt, !IO),
    globals.io_lookup_string_option(object_file_extension, ObjExt, !IO),
    globals.io_lookup_string_option(mercury_linkage, MercuryLinkage, !IO),
    globals.io_lookup_bool_option(gcc_global_registers, GCCGlobals, !IO),
    globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
    globals.io_lookup_bool_option(pic, PIC, !IO),
    globals.io_get_target(Target, !IO),
    (
        PIC = yes,
        % We've been explicitly told to use position independent code.
        ObjectCodeType = ( if PicObjExt = ObjExt then non_pic else pic )
    ;
        PIC = no,
        (
            FileType = static_library,
            ObjectCodeType = non_pic
        ;
            FileType = shared_library,
            ObjectCodeType = ( if PicObjExt = ObjExt then non_pic else pic )
        ;
            FileType = java_archive,
            ObjectCodeType = non_pic
        ;
            FileType = executable,
            ( MercuryLinkage = "shared" ->
                (
                    % We only need to create `.lpic' files if `-DMR_PIC_REG'
                    % has an effect, which currently is only with grades using
                    % GCC global registers on x86 Unix.
                    ( LinkWithPicObjExt = ObjExt
                    ; HighLevelCode = yes
                    ; GCCGlobals = no
                    ; Target \= target_c
                    )
                ->
                    ObjectCodeType = non_pic
                ;
                    LinkWithPicObjExt = PicObjExt
                ->
                    ObjectCodeType = pic
                ;
                    ObjectCodeType = link_with_pic
                )
            ; MercuryLinkage = "static" ->
                ObjectCodeType = non_pic
            ;
                % The linkage string is checked by options.m.
                unexpected(this_file, "unknown linkage " ++ MercuryLinkage)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred standard_library_directory_option(string::out, io::di, io::uo) is det.

standard_library_directory_option(Opt, !IO) :-
    globals.io_lookup_maybe_string_option(mercury_standard_library_directory,
        MaybeStdLibDir, !IO),
    globals.io_lookup_maybe_string_option(mercury_configuration_directory,
        MaybeConfDir, !IO),
    (
        MaybeStdLibDir = yes(StdLibDir),
        Opt0 = "--mercury-standard-library-directory " ++ StdLibDir ++ " ",
        (
            MaybeConfDir = yes(ConfDir),
            ConfDir \= StdLibDir
        ->
            Opt = Opt0 ++
                "--mercury-configuration-directory " ++ ConfDir ++ " "
        ;
            Opt = Opt0
        )
    ;
        MaybeStdLibDir = no,
        Opt = "--no-mercury-standard-library-directory "
    ).

%-----------------------------------------------------------------------------%

    % join_string_list(Strings, Prefix, Suffix, Serarator, Result):
    %
    % Appends the strings in the list `Strings' together into the string
    % Result. Each string is prefixed by Prefix, suffixed by Suffix and
    % separated by Separator.
    %
:- pred join_string_list(list(string)::in, string::in, string::in, string::in,
    string::out) is det.

join_string_list([], _Prefix, _Suffix, _Separator, "").
join_string_list([String | Strings], Prefix, Suffix, Separator, Result) :-
    (
        Strings = [],
        string.append_list([Prefix, String, Suffix], Result)
    ;
        Strings = [_ | _],
        join_string_list(Strings, Prefix, Suffix, Separator, Result0),
        string.append_list([Prefix, String, Suffix, Separator, Result0],
            Result)
    ).

    % As above, but quote the strings first. Note that the strings in values
    % of the *flags options are already quoted.
    %
:- pred join_quoted_string_list(list(string)::in, string::in, string::in,
    string::in, string::out) is det.

join_quoted_string_list(Strings, Prefix, Suffix, Separator, Result) :-
    join_string_list(map(quote_arg, Strings), Prefix, Suffix, Separator,
        Result).

    % join_module_list(ModuleNames, Extension, Result):
    %
    % The list of strings `Result' is computed from the list of strings
    % `ModuleNames', by removing any directory paths, and converting the
    % strings to file names and then back, adding the specified Extension.
    % (This conversion ensures that we follow the usual file naming
    % conventions.)

:- pred join_module_list(list(string)::in, string::in, list(string)::out,
    io::di, io::uo) is det.

join_module_list([], _Extension, [], !IO).
join_module_list([Module | Modules], Extension, [FileName | Rest], !IO) :-
    file_name_to_module_name(dir.basename_det(Module), ModuleName),
    module_name_to_file_name(ModuleName, Extension, no, FileName, !IO),
    join_module_list(Modules, Extension, Rest, !IO).

%-----------------------------------------------------------------------------%

make_all_module_command(Command0, MainModule, AllModules, Command, !IO) :-
    % Pass the main module first.
    list.map_foldl(
        (pred(Module::in, FileName::out, IO0::di, IO::uo) is det :-
            module_name_to_file_name(Module, ".m", no, FileName, IO0, IO)
        ),
        [MainModule | list.delete_all(AllModules, MainModule)],
        ModuleNameStrings, !IO),
    Command = string.join_list(" ",
        list.map(quote_arg, [Command0 | ModuleNameStrings])).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(maybe_pic_object_file_extension/3).

maybe_pic_object_file_extension(Globals::in, PIC::in, Ext::out) :-
    (
        PIC = non_pic,
        globals.lookup_string_option(Globals, object_file_extension, Ext)
    ;
        PIC = pic,
        globals.lookup_string_option(Globals, pic_object_file_extension, Ext)
    ;
        PIC = link_with_pic,
        globals.lookup_string_option(Globals,
            link_with_pic_object_file_extension, Ext)
    ).
maybe_pic_object_file_extension(Globals::in, PIC::out, Ext::in) :-
    (
        % This test must come first -- if the architecture doesn't
        % need special treatment for PIC, we should always return
        % `non_pic'.  `mmc --make' depends on this.
        globals.lookup_string_option(Globals, object_file_extension, Ext)
    ->
        PIC = non_pic
    ;
        globals.lookup_string_option(Globals, pic_object_file_extension, Ext)
    ->
        PIC = pic
    ;
        globals.lookup_string_option(Globals,
            link_with_pic_object_file_extension, Ext)
    ->
        PIC = link_with_pic
    ;
        fail
    ).

maybe_pic_object_file_extension(PIC, ObjExt, !IO) :-
    globals.io_get_globals(Globals, !IO),
    maybe_pic_object_file_extension(Globals, PIC, ObjExt).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "compile_target_code.m".

%-----------------------------------------------------------------------------%
:- end_module compile_target_code.
%-----------------------------------------------------------------------------%
