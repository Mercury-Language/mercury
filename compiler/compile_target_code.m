%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
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

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.file_util.
:- import_module parse_tree.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_data.
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

    % compile_java_files(ErrorStream, JavaFiles, Succeeded)
    %
:- pred compile_java_files(io.output_stream::in, list(string)::in, bool::out,
    io::di, io::uo) is det.

    % il_assemble(ErrorStream, ModuleName, HasMain, Succeeded)
    %
:- pred il_assemble(io.output_stream::in, module_name::in, has_main::in,
    bool::out, io::di, io::uo) is det.

    % il_assemble(ErrorStream, ILFile, DLLFile, HasMain, Succeeded)
    %
:- pred il_assemble(io.output_stream::in, file_name::in, file_name::in,
    has_main::in, bool::out, io::di, io::uo) is det.

    % compile_csharp_file(ErrorStream, C#File, DLLFile, Succeeded)
    %
:- pred compile_csharp_file(io.output_stream::in, module_imports::in,
    file_name::in, file_name::in, bool::out, io::di, io::uo) is det.

    % compile_erlang_file(ErrorStream, ErlangFile, Succeeded)
    %
:- pred compile_erlang_file(io.output_stream::in, file_name::in,
    bool::out, io::di, io::uo) is det.

    % make_library_init_file(ErrorStream, MainModuleName, ModuleNames,
    %   Succeeded):
    %
    % Make the `.init' file for a library containing the given modules.
    %
:- pred make_library_init_file(io.output_stream::in, module_name::in,
    list(module_name)::in, bool::out, io::di, io::uo) is det.

    % make_init_erlang_library(ErrorStream, MainModuleName, ModuleNames,
    %   Succeeded):
    %
    % Make the `.init' file for an Erlang library containing the given
    % modules.
    %
:- pred make_erlang_library_init_file(io.output_stream::in, module_name::in,
    list(module_name)::in, bool::out, io::di, io::uo) is det.

    % make_init_obj_file(ErrorStream, MainModuleName, AllModuleNames,
    %   MaybeInitObjFileName)
    %
:- pred make_init_obj_file(io.output_stream::in, module_name::in,
    list(module_name)::in, maybe(file_name)::out, io::di, io::uo) is det.

    % make_erlang_program_init_file(ErrorStream, MainModuleName,
    %   AllModuleNames, MaybeInitObjFileName)
    %
:- pred make_erlang_program_init_file(io.output_stream::in, module_name::in,
    list(module_name)::in, maybe(file_name)::out, io::di, io::uo) is det.

:- type linked_target_type
    --->    executable
    ;       static_library
    ;       shared_library
    ;       java_archive
    ;       erlang_archive.

    % link(TargetType, MainModuleName, ObjectFileNames, Succeeded)
    %
:- pred link(io.output_stream::in, linked_target_type::in, module_name::in,
    list(string)::in, bool::out, io::di, io::uo) is det.

    % post_link_make_symlink_or_copy(TargetType, MainModuleName, Succeeded,
    %   MadeSymlinkOrCopy)
    %
    % If `--use-grade-subdirs' is enabled, link or copy the executable or
    % library into the user's directory after having successfully built it,
    % if the target does not exist or is not up-to-date.
    %
:- pred post_link_make_symlink_or_copy(io.output_stream::in,
    linked_target_type::in, module_name::in, bool::out, bool::out,
    io::di, io::uo) is det.

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
%
% Stuff used for standalone interfaces
% 
    
    % make_standalone_interface(Basename, !IO):
    %
    % Create a standalone interface in the current directory.
    % 
:- pred make_standalone_interface(string::in, io::di, io::uo) is det.

    % Output the C compiler flags to the given stream.
    % This predicate is used to implement the `--output-cflags' option.
    %
:- pred output_c_compiler_flags(io.output_stream::in, io::di, io::uo) is det.

    % Output the list of flags required to link against the selected set
    % of Mercury libraries (the standard libraries, plus any other specified
    % via the --ml option) in the current grade.
    % This predicate is used to implement the `--output-library-link-flags'
    % option.
    %
:- pred output_library_link_flags(io.output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.

:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module libs.trace_params.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.write_deps_file.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module dir.
:- import_module getopt_io.
:- import_module string.

%-----------------------------------------------------------------------------%

il_assemble(ErrorStream, ModuleName, HasMain, Succeeded, !IO) :-
    module_name_to_file_name(ModuleName, ".il", do_not_create_dirs, IL_File,
        !IO),
    module_name_to_file_name(ModuleName, ".dll", do_create_dirs, DllFile, !IO),

    % If the module contains main/2 then we it should be built as an
    % executable. Unfortunately C# code may refer to the dll
    % so we always need to build the dll.
    %
    il_assemble(ErrorStream, IL_File, DllFile, no_main, DllSucceeded, !IO),
    ( 
        HasMain = has_main,
        module_name_to_file_name(ModuleName, ".exe", do_create_dirs, ExeFile,
            !IO),
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
            module_name_to_file_name(Mod, ".dll", do_not_create_dirs,
                FileName, IO0, IO),
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

compile_c_file(ErrorStream, PIC, ModuleName, Succeeded, !IO) :-
    module_name_to_file_name(ModuleName, ".c", do_create_dirs, C_File, !IO),
    maybe_pic_object_file_extension(PIC, ObjExt, !IO),
    module_name_to_file_name(ModuleName, ObjExt, do_create_dirs, O_File, !IO),
    compile_c_file(ErrorStream, PIC, C_File, O_File, Succeeded, !IO).

compile_c_file(ErrorStream, PIC, C_File, O_File, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_string_option(c_flag_to_name_object_file,
        NameObjectFile, !IO),
    maybe_write_string(Verbose, "% Compiling `", !IO),
    maybe_write_string(Verbose, C_File, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(cc, CC, !IO),
    gather_c_compiler_flags(PIC, AllCFlags, !IO),
    string.append_list([
        CC, " ", 
        AllCFlags,
        " -c ", C_File, " ",
        NameObjectFile, O_File], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands,
        Command, Succeeded, !IO).

:- pred gather_c_compiler_flags(pic::in, string::out, io::di, io::uo) is det.

gather_c_compiler_flags(PIC, AllCFlags, !IO) :-
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
    BoehmGC_Opt = "-DMR_CONSERVATIVE_GC -DMR_BOEHM_GC ",
    (
        GC_Method = gc_automatic,
        GC_Opt = ""
    ;
        GC_Method = gc_none,
        GC_Opt = ""
    ;
        GC_Method = gc_boehm,
        GC_Opt = BoehmGC_Opt
    ;
        GC_Method = gc_boehm_debug,
        GC_Opt = BoehmGC_Opt ++ "-DMR_BOEHM_GC_DEBUG -DGC_DEBUG -DKEEP_BACKPTRS "
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
        % This should have been caught in handle_options.
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
    (
        Tags_Method = tags_high,
        TagsOpt = "-DMR_HIGHTAGS "
    ;
        ( Tags_Method = tags_low
        ; Tags_Method = tags_none
        ),
        TagsOpt = ""
    ),
    globals.io_lookup_int_option(num_tag_bits, NumTagBits, !IO),
    string.int_to_string(NumTagBits, NumTagBitsString),
    NumTagBitsOpt = "-DMR_TAGBITS=" ++ NumTagBitsString ++ " ",
    globals.io_lookup_bool_option(decl_debug, DeclDebug, !IO),
    (
        DeclDebug = yes,
        DeclDebugOpt = "-DMR_DECL_DEBUG "
    ;
        DeclDebug = no,
        DeclDebugOpt = ""
    ),
    globals.io_lookup_bool_option(source_to_source_debug, SourceDebug, !IO),
    (
        SourceDebug = yes,
        SourceDebugOpt = "-DMR_SS_DEBUG "
    ;
        SourceDebug = no,
        SourceDebugOpt = ""
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
    globals.io_lookup_bool_option(stack_segments, StackSegments, !IO),
    (
        Extend = yes,
        StackSegments = no,
        ExtendOpt = "-DMR_EXTEND_STACKS_WHEN_NEEDED "
    ;
        Extend = no,
        StackSegments = yes,
        ExtendOpt = "-DMR_STACK_SEGMENTS "
    ;
        Extend = no,
        StackSegments = no,
        ExtendOpt = ""
    ;
        Extend = yes,
        StackSegments = yes,
        ExtendOpt = unexpected(this_file,
            "compile_c_file: --extend-stacks-when-needed and --stack-segments")
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
        LL_DebugOpt = "-DMR_LL_DEBUG "
    ;
        LL_Debug = no,
        LL_DebugOpt = ""
    ),
    globals.io_lookup_bool_option(use_trail, UseTrail, !IO),
    (
        UseTrail = yes,
        UseTrailOpt = "-DMR_USE_TRAIL ",
        % With tagged trail entries function trailing will not work unless the
        % C functions stored on the trail are aligned on word boundaries (or a
        % multiple thereof).  The assemblers on some systems, and some gcc
        % optimisation settings, do not align functions, so we need to
        % explicitly pass -falign-functions in trailing grades to ensure that
        % C functions are appropriately aligned.
        %
        % Note that this will also affect the untagged version of the trail,
        % but that shouldn't matter.
        %
        io_get_c_compiler_type(C_CompilerType, !IO),
        (
            C_CompilerType = cc_gcc(_, _, _),
            globals.io_lookup_int_option(bytes_per_word, BytesPerWord, !IO),
            C_FnAlignOpt = string.format("-falign-functions=%d ",
                [i(BytesPerWord)])
        ;
            % XXX Check whether we need to do anything for these C compilers?
            ( C_CompilerType = cc_lcc
            ; C_CompilerType = cc_cl
            ),
            C_FnAlignOpt = ""
        ;
            C_CompilerType = cc_unknown,
            C_FnAlignOpt = ""
        ),
        globals.io_lookup_bool_option(trail_segments, TrailSegments, !IO),
        (
            TrailSegments = yes,
            TrailSegOpt = "-DMR_TRAIL_SEGMENTS "
        ;
            TrailSegments = no,
            TrailSegOpt = ""
        )
    ;
        UseTrail = no,
        UseTrailOpt = "",
        C_FnAlignOpt = "",
        TrailSegOpt = ""
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
            MinimalModelOpt = MinimalModelBaseOpt ++
                "-DMR_MINIMAL_MODEL_DEBUG "
        )
    ;
        MinimalModelDebug = no,
        MinimalModelOpt = MinimalModelBaseOpt
    ),
    globals.io_lookup_bool_option(single_prec_float, SinglePrecFloat, !IO),
    (
        SinglePrecFloat = yes,
        SinglePrecFloatOpt = "-DMR_USE_SINGLE_PREC_FLOAT "
    ;
        SinglePrecFloat = no,
        SinglePrecFloatOpt = ""
    ),
    globals.io_lookup_bool_option(use_regions, UseRegions, !IO),
    (
        UseRegions = yes,
        UseRegionsOpt0 = "-DMR_USE_REGIONS ",
        globals.io_lookup_bool_option(use_regions_debug, UseRegionsDebug, !IO),
        (
            UseRegionsDebug = yes,
            UseRegionsOpt1 = UseRegionsOpt0 ++ "-DMR_RBMM_DEBUG "
        ;
            UseRegionsDebug = no,
            UseRegionsOpt1 = UseRegionsOpt0
        ),
        globals.io_lookup_bool_option(use_regions_profiling,
            UseRegionsProfiling, !IO),
        (
            UseRegionsProfiling = yes,
            UseRegionsOpt = UseRegionsOpt1 ++ "-DMR_RBMM_PROFILING "
        ;
            UseRegionsProfiling = no,
            UseRegionsOpt = UseRegionsOpt1
        )
    ;
        UseRegions = no,
        UseRegionsOpt = ""
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
        AppleGCCRegWorkaroundOpt = "-fno-loop-optimize "
    ;
        AppleGCCRegWorkaroundOpt = ""
    ),
    
    % Be careful with the order here!  Some options override others,
    % e.g. CFLAGS_FOR_REGS must come after OptimizeOpt so that
    % it can override -fomit-frame-pointer with -fno-omit-frame-pointer.
    % Also be careful that each option is separated by spaces.
    string.append_list([
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
        Target_DebugOpt, LL_DebugOpt, DeclDebugOpt,
        SourceDebugOpt,
        ExecTraceOpt,
        UseTrailOpt, 
        TrailSegOpt,
        MinimalModelOpt, 
        SinglePrecFloatOpt,
        UseRegionsOpt,
        TypeLayoutOpt,
        InlineAllocOpt,
        AnsiOpt, " ", 
        AppleGCCRegWorkaroundOpt,
        C_FnAlignOpt, 
        WarningOpt, " ", 
        CFLAGS], AllCFlags).

%-----------------------------------------------------------------------------%

compile_java_files(ErrorStream, JavaFiles, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    (
        JavaFiles = [JavaFile | MoreFiles],
        (
            Verbose = yes,
            io.write_string("% Compiling `", !IO),
            io.write_string(JavaFile, !IO),
            (
                MoreFiles = [],
                io.write_string("':\n", !IO)
            ;
                MoreFiles = [_ | _],
                io.write_string("', etc.:\n", !IO)
            )
        ;
            Verbose = no
        )
    ;
        JavaFiles = [],
        unexpected(this_file, "compile_java_files: empty list")
    ),

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
            SourceDirName = "Mercury"/Grade/FullArch/"Mercury"/"javas",
            DestDirName = "Mercury"/Grade/FullArch/"Mercury"/"classs"
        ;
            UseGradeSubdirs = no,
            SourceDirName = "Mercury"/"javas",
            DestDirName = "Mercury"/"classs"
        ),
        % Javac won't create the destination directory for class files,
        % so we need to do it.
        dir.make_directory(DestDirName, _, !IO),
        % Set directories for source and class files.
        DirOpts = string.append_list([
            "-sourcepath ", SourceDirName, " ",
            "-d ", DestDirName, " "
        ])
    ;
        UseSubdirs = no,
        DirOpts = ""
    ),

    % Be careful with the order here!  Some options may override others.
    % Also be careful that each option is separated by spaces.
    JoinedJavaFiles = string.join_list(" ", JavaFiles),
    string.append_list([JavaCompiler, " ", InclOpt, DirOpts,
        Target_DebugOpt, JAVAFLAGS, " ", JoinedJavaFiles], Command),
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
    module_name_to_file_name(ModuleName, AsmExt, do_not_create_dirs, AsmFile,
        !IO),
    maybe_pic_object_file_extension(PIC, ObjExt, !IO),
    module_name_to_file_name(ModuleName, ObjExt, do_create_dirs, ObjFile, !IO),

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

compile_erlang_file(ErrorStream, ErlangFile, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Compiling `", !IO),
    maybe_write_string(Verbose, ErlangFile, !IO),
    maybe_write_string(Verbose, "':\n", !IO),
    globals.io_lookup_string_option(erlang_compiler, ErlangCompiler, !IO),
    globals.io_lookup_accumulating_option(erlang_flags, ErlangFlagsList0, !IO),
    globals.io_lookup_bool_option(erlang_native_code, ErlangNativeCode, !IO),
    globals.io_lookup_bool_option(erlang_inhibit_trivial_warnings,
        ErlangInhibitTrivialWarnings, !IO),
    (
        ErlangNativeCode = yes,
        ErlangFlagsList1 = ["+native" | ErlangFlagsList0]
    ;
        ErlangNativeCode = no,
        ErlangFlagsList1 = ErlangFlagsList0
    ),
    (
        ErlangInhibitTrivialWarnings = yes,
        ErlangFlagsList = ["+nowarn_unused_vars", "+nowarn_unused_function"
            | ErlangFlagsList1]
    ;
        ErlangInhibitTrivialWarnings = no,
        ErlangFlagsList = ErlangFlagsList1
    ),
    ERLANGFLAGS = string.join_list(" ", ErlangFlagsList),

    globals.io_lookup_accumulating_option(erlang_include_directory,
        Erlang_Incl_Dirs, !IO),
    InclOpt = string.append_list(list.condense(list.map(
        (func(E_INCL) = ["-I", quote_arg(E_INCL), " "]), Erlang_Incl_Dirs))),

    globals.io_lookup_bool_option(use_subdirs, UseSubdirs, !IO),
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    globals.io_lookup_string_option(fullarch, FullArch, !IO),
    globals.io_get_globals(Globals, !IO),
    (
        UseSubdirs = yes,
        (
            UseGradeSubdirs = yes,
            grade_directory_component(Globals, Grade),
            DirName = "Mercury"/Grade/FullArch/"Mercury"/"beams"
        ;
            UseGradeSubdirs = no,
            DirName = "Mercury"/"beams"
        ),
        % Create the destination directory.
        dir.make_directory(DirName, _, !IO),
        % Set destination directory for .beam files.
        DestDir = "-o " ++ DirName ++ " "
    ;
        UseSubdirs = no,
        DestDir = ""
    ),

    string.append_list([ErlangCompiler, " ", InclOpt, DestDir, ERLANGFLAGS,
        " ", ErlangFile], Command),
    invoke_system_command(ErrorStream, cmd_verbose_commands, Command,
        Succeeded, !IO).

%-----------------------------------------------------------------------------%

make_library_init_file(ErrorStream, MainModuleName, AllModules, Succeeded,
        !IO) :-
    globals.io_lookup_string_option(mkinit_command, MkInit, !IO),
    make_library_init_file_2(ErrorStream, MainModuleName, AllModules, ".c",
        MkInit, Succeeded, !IO).

make_erlang_library_init_file(ErrorStream, MainModuleName, AllModules,
        Succeeded, !IO) :-
    globals.io_lookup_string_option(mkinit_erl_command, MkInit, !IO),
    make_library_init_file_2(ErrorStream, MainModuleName, AllModules, ".erl",
        MkInit, Succeeded, !IO).

:- pred make_library_init_file_2(io.output_stream::in, module_name::in,
    list(module_name)::in, string::in, string::in,
    bool::out, io::di, io::uo) is det.

make_library_init_file_2(ErrorStream, MainModuleName, AllModules, TargetExt,
        MkInit, Succeeded, !IO) :-
    module_name_to_file_name(MainModuleName, ".init.tmp", do_create_dirs,
        TmpInitFileName, !IO),
    io.open_output(TmpInitFileName, InitFileRes, !IO),
    (
        InitFileRes = ok(InitFileStream),
        list.map_foldl(
            module_name_to_file_name_ext(TargetExt, do_not_create_dirs),
            AllModules, AllTargetFilesList, !IO),

        invoke_mkinit(InitFileStream, cmd_verbose_commands,
            MkInit, " -k ", AllTargetFilesList, MkInitOK, !IO),

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
        module_name_to_file_name(MainModuleName, ".init", do_create_dirs,
            InitFileName, !IO),
        update_interface_return_succeeded(InitFileName, Succeeded1, !IO),
        Succeeded2 = Succeeded0 `and` Succeeded1,
        (
            Succeeded2 = yes,
            % Symlink or copy the .init files to the user's directory
            % if --use-grade-subdirs is enabled.
            globals.io_lookup_bool_option(use_grade_subdirs,
                UseGradeSubDirs, !IO),
            (
                UseGradeSubDirs = yes,
                io.set_output_stream(ErrorStream, OutputStream, !IO),
                globals.io_set_option(use_subdirs, bool(no), !IO),
                globals.io_set_option(use_grade_subdirs, bool(no), !IO),
                module_name_to_file_name(MainModuleName, ".init",
                    do_not_create_dirs, UserDirFileName, !IO),
                globals.io_set_option(use_subdirs, bool(yes), !IO),
                globals.io_set_option(use_grade_subdirs, bool(yes), !IO),
                % Remove the target of the symlink/copy in case it already
                % exists.
                io.remove_file(UserDirFileName, _, !IO),
                make_symlink_or_copy_file(InitFileName, UserDirFileName,
                    Succeeded, !IO),
                io.set_output_stream(OutputStream, _, !IO)
            ;
                UseGradeSubDirs = no,
                Succeeded = yes
            )
        ;
            Succeeded2 = no,
            Succeeded  = no
        )
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

:- pred module_name_to_file_name_ext(string::in, maybe_create_dirs::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_name_to_file_name_ext(Ext, MkDir, ModuleName, FileName, !IO) :-
    module_name_to_file_name(ModuleName, Ext, MkDir, FileName, !IO).

:- pred invoke_mkinit(io.output_stream::in, command_verbosity::in,
    string::in, string::in, list(file_name)::in, bool::out,
    io::di, io::uo) is det.

invoke_mkinit(InitFileStream, Verbosity,
        MkInit, Args, FileNames, MkInitOK, !IO) :-
    join_quoted_string_list(FileNames, "", "\n", "", TargetFileNames),

    io.make_temp(TmpFile, !IO),
    io.open_output(TmpFile, OpenResult, !IO),
    (
        OpenResult = ok(TmpStream),
        io.write_string(TmpStream, TargetFileNames, !IO),
        io.close_output(TmpStream, !IO),

        MkInitCmd = string.append_list([MkInit, " ", Args, " -f ", TmpFile]),
        invoke_system_command(InitFileStream, Verbosity,
            MkInitCmd, MkInitOK0, !IO),

        io.remove_file(TmpFile, RemoveResult, !IO),
        (
            RemoveResult = ok,
            MkInitOK = MkInitOK0
        ;
            RemoveResult = error(_),
            MkInitOK = no
        )
    ;
        OpenResult = error(_),
        MkInitOK = no
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
    (
        CompileToSharedLib = yes,
        TargetType = shared_library
    ;
        CompileToSharedLib = no,
        TargetType = executable
    ),
    get_object_code_type(TargetType, PIC, !IO),
    maybe_pic_object_file_extension(PIC, Obj, !IO),

    globals.io_get_target(Target, !IO),
    io.output_stream(OutputStream, !IO),
    ( 
        Target = target_asm,
        % For --target asm, we generate everything into a single object file.
        (
            Modules = [FirstModule | _],
            join_module_list([FirstModule], Obj, ObjectsList, !IO)
        ;
            Modules = [],
            unexpected(this_file, "link_module_list: no modules")
        )
    ;
        ( Target = target_c
        ; Target = target_java
        ; Target = target_il
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        join_module_list(Modules, Obj, ObjectsList, !IO)
    ),
    ( 
        TargetType = executable,
        list.map(
            (pred(ModuleStr::in, ModuleName::out) is det :-
                file_name_to_module_name(dir.basename_det(ModuleStr),
                    ModuleName)
            ), Modules, ModuleNames),
        MustCompile = yes,
        make_init_obj_file(OutputStream, MustCompile, MainModuleName,
            ModuleNames, InitObjResult, !IO)
    ;
        TargetType = shared_library,
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

%-----------------------------------------------------------------------------%

make_init_obj_file(ErrorStream, ModuleName, ModuleNames, Result, !IO) :-
    globals.io_lookup_bool_option(rebuild, MustCompile, !IO),
    make_init_obj_file(ErrorStream, MustCompile, ModuleName, ModuleNames,
        Result, !IO).

% WARNING: The code here duplicates the functionality of scripts/c2init.in.
% Any changes there may also require changes here, and vice versa.
% The code of make_standalone_interface/3 may also require updating.

:- pred make_init_obj_file(io.output_stream::in, bool::in,
    module_name::in, list(module_name)::in, maybe(file_name)::out,
    io::di, io::uo) is det.

make_init_obj_file(ErrorStream, MustCompile, ModuleName, ModuleNames, Result,
        !IO) :-
    globals.io_lookup_maybe_string_option(
        mercury_standard_library_directory, MaybeStdLibDir, !IO),
    globals.io_get_globals(Globals, !IO),
    grade_directory_component(Globals, GradeDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        ToGradeInit = (func(File) = StdLibDir / "modules" / GradeDir / File),
        StdInitFileNames = [
            ToGradeInit("mer_rt.init"),
            ToGradeInit("mer_std.init")
        ],
        StdTraceInitFileNames = [
            ToGradeInit("mer_browser.init"),
            ToGradeInit("mer_mdbcomp.init")
        ],
        SourceDebugInitFileNames = [
            ToGradeInit("mer_ssdb.init")
        ]
    ;
        MaybeStdLibDir = no,
        StdInitFileNames = [],
        StdTraceInitFileNames = [],
        SourceDebugInitFileNames = []
    ),

    globals.io_lookup_string_option(mkinit_command, MkInit, !IO),
    make_init_target_file(ErrorStream, MkInit, ModuleName, ModuleNames, ".c",
        StdInitFileNames, StdTraceInitFileNames, SourceDebugInitFileNames,
        "", MaybeInitTargetFile, !IO),

    get_object_code_type(executable, PIC, !IO),
    maybe_pic_object_file_extension(PIC, ObjExt, !IO),

    module_name_to_file_name(ModuleName, "_init" ++ ObjExt, do_create_dirs,
        InitObjFileName, !IO),
    CompileCInitFile =
        (pred(InitTargetFileName::in, Res::out, IO0::di, IO::uo) is det :-
            compile_c_file(ErrorStream, PIC, InitTargetFileName,
                InitObjFileName, Res, IO0, IO)
        ),
    maybe_compile_init_obj_file(MaybeInitTargetFile, MustCompile,
        CompileCInitFile, InitObjFileName, Result, !IO).

make_erlang_program_init_file(ErrorStream, ModuleName, ModuleNames, Result,
        !IO) :-
    globals.io_lookup_bool_option(rebuild, MustCompile, !IO),

    globals.io_lookup_maybe_string_option(
        mercury_standard_library_directory, MaybeStdLibDir, !IO),
    globals.io_get_globals(Globals, !IO),
    grade_directory_component(Globals, GradeDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        StdInitFileNames = [
            StdLibDir / "modules" / GradeDir / "mer_std.init"
        ],
        SourceDebugInitFileNames = [
            StdLibDir / "modules" / GradeDir / "mer_ssdb.init"
        ]
    ;
        MaybeStdLibDir = no,
        StdInitFileNames = [],
        SourceDebugInitFileNames = []
    ),
    % Tracing is not supported in Erlang backend.
    StdTraceInitFileNames = [],

    % We need to pass the module name to mkinit_erl.
    ErlangModuleName = qualify_mercury_std_library_module_name(ModuleName),
    ModuleNameStr = sym_name_to_string_sep(ErlangModuleName, "__") ++ "_init",
    ModuleNameOption = " -m " ++ quote_arg(ModuleNameStr),

    globals.io_lookup_string_option(mkinit_erl_command, MkInitErl, !IO),
    make_init_target_file(ErrorStream, MkInitErl, ModuleName, ModuleNames,
        ".erl", StdInitFileNames, StdTraceInitFileNames,
        SourceDebugInitFileNames, ModuleNameOption, MaybeInitTargetFile, !IO),

    module_name_to_file_name(ModuleName, "_init.beam", do_create_dirs,
        InitObjFileName, !IO),
    CompileErlangInitFile =
        (pred(InitTargetFileName::in, Res::out, IO0::di, IO::uo) is det :-
            compile_erlang_file(ErrorStream, InitTargetFileName, Res, IO0, IO)
        ),
    maybe_compile_init_obj_file(MaybeInitTargetFile, MustCompile,
        CompileErlangInitFile, InitObjFileName, Result, !IO).

:- pred make_init_target_file(io.output_stream::in, string::in,
    module_name::in, list(module_name)::in, string::in,
    list(file_name)::in, list(file_name)::in, list(file_name)::in,
    string::in, maybe(file_name)::out, io::di, io::uo) is det.

make_init_target_file(ErrorStream, MkInit, ModuleName, ModuleNames, TargetExt,
        StdInitFileNames, StdTraceInitFileNames, SourceDebugInitFileNames,
        ModuleNameOption, MaybeInitTargetFile, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(statistics, Stats, !IO),
    maybe_write_string(Verbose, "% Creating initialization file...\n", !IO),

    globals.io_get_globals(Globals, !IO),
    compute_grade(Globals, Grade),

    module_name_to_file_name(ModuleName, "_init" ++ TargetExt, do_create_dirs,
        InitTargetFileName, !IO),

    list.map_foldl(module_name_to_file_name_ext(TargetExt, do_not_create_dirs),
        ModuleNames, TargetFileNameList, !IO),

    globals.io_lookup_accumulating_option(init_file_directories,
        InitFileDirsList, !IO),
    join_quoted_string_list(InitFileDirsList, "-I ", "", " ", InitFileDirs),

    globals.io_lookup_accumulating_option(init_files, InitFileNamesList0,
        !IO),
    globals.io_lookup_accumulating_option(trace_init_files,
        TraceInitFileNamesList0, !IO),
    InitFileNamesList1 = StdInitFileNames ++ InitFileNamesList0,
    TraceInitFileNamesList = StdTraceInitFileNames ++ TraceInitFileNamesList0,

    globals.io_get_trace_level(TraceLevel, !IO),
    ( given_trace_level_is_none(TraceLevel) = no ->
        TraceOpt = "-t",
        InitFileNamesList2 = InitFileNamesList1 ++ TraceInitFileNamesList
    ;
        TraceOpt = "",
        InitFileNamesList2 = InitFileNamesList1
    ),

    globals.io_lookup_bool_option(source_to_source_debug, SourceDebug, !IO),
    (
        SourceDebug = yes,
        InitFileNamesList = InitFileNamesList2 ++ SourceDebugInitFileNames
    ;
        SourceDebug = no,
        InitFileNamesList = InitFileNamesList2
    ),

    globals.io_lookup_accumulating_option(runtime_flags, RuntimeFlagsList,
        !IO),
    join_quoted_string_list(RuntimeFlagsList, "-r ", "", " ", RuntimeFlags),

    globals.io_lookup_bool_option(extra_initialization_functions, ExtraInits,
        !IO),
    (
        ExtraInits = yes,
        ExtraInitsOpt = "-x"
    ;
        ExtraInits = no,
        ExtraInitsOpt = ""
    ),

    globals.io_lookup_bool_option(main, Main, !IO),
    (
        Main = no,
        NoMainOpt = "-l"
    ;
        Main = yes,
        NoMainOpt = ""
    ),

    globals.io_lookup_string_option(experimental_complexity,
        ExperimentalComplexity, !IO),
    ( ExperimentalComplexity = "" ->
        ExperimentalComplexityOpt = ""
    ;
        ExperimentalComplexityOpt = "-X " ++ ExperimentalComplexity
    ),

    TmpInitTargetFileName = InitTargetFileName ++ ".tmp",
    MkInitArgs = string.append_list(
        [   " -g ", Grade,
            " ", TraceOpt,
            " ", ExtraInitsOpt,
            " ", NoMainOpt,
            " ", ExperimentalComplexityOpt,
            " ", RuntimeFlags,
            " -o ", quote_arg(TmpInitTargetFileName),
            " ", InitFileDirs,
            ModuleNameOption
        ]),

    invoke_mkinit(ErrorStream, cmd_verbose_commands,
        MkInit, MkInitArgs, TargetFileNameList ++ InitFileNamesList,
        MkInitOk, !IO),

    maybe_report_stats(Stats, !IO),
    (
        MkInitOk = yes,
        update_interface_return_succeeded(InitTargetFileName, UpdateOk, !IO),
        (
            UpdateOk = yes,
            MaybeInitTargetFile = yes(InitTargetFileName)
        ;
            UpdateOk = no,
            MaybeInitTargetFile = no
        )
    ;
        MkInitOk = no,
        MaybeInitTargetFile = no
    ).

:- pred maybe_compile_init_obj_file(maybe(file_name)::in, bool::in,
    compile_init_file_pred::in(compile_init_file_pred),
    file_name::in, maybe(file_name)::out, io::di, io::uo) is det.

:- type compile_init_file_pred == pred(file_name, bool, io, io).
:- inst compile_init_file_pred == (pred(in, out, di, uo) is det).

maybe_compile_init_obj_file(MaybeInitTargetFile, MustCompile, Compile,
        InitObjFileName, Result, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(statistics, Stats, !IO),
    (
        MaybeInitTargetFile = yes(InitTargetFileName),
        file_as_new_as(InitObjFileName, Rel, InitTargetFileName, !IO),
        (
            ( MustCompile = yes
            ; Rel = is_not_as_new_as
            ; Rel = missing_timestamp
            )
        ->
            maybe_write_string(Verbose,
                "% Compiling initialization file...\n", !IO),
            Compile(InitTargetFileName, CompileOk, !IO),
            maybe_report_stats(Stats, !IO),
            (
                CompileOk = yes,
                Result = yes(InitObjFileName)
            ;
                CompileOk = no,
                Result = no
            )
        ;
            Result = yes(InitObjFileName)
        )
    ;
        MaybeInitTargetFile = no,
        Result = no
    ).

    % file_as_new_as(FileNameA, Rel, FileNameB, !IO)
    %
    % Check if file A has a timestamp at least as new as the timestamp of
    % file B.  Rel is `missing_timestamp' iff either timestamp could not
    % be retrieved.
    %
:- pred file_as_new_as(file_name::in, is_as_new_as::out, file_name::in,
    io::di, io::uo) is det.

:- type is_as_new_as
    --->    is_as_new_as
    ;       is_not_as_new_as
    ;       missing_timestamp.

file_as_new_as(FileNameA, Rel, FileNameB, !IO) :-
    compare_file_timestamps(FileNameA, FileNameB, MaybeCompare, !IO),
    (
        ( MaybeCompare = yes(=)
        ; MaybeCompare = yes(>)
        ),
        Rel = is_as_new_as 
    ;
        MaybeCompare = yes(<),
        Rel = is_not_as_new_as
    ;
        MaybeCompare = no,
        Rel = missing_timestamp
    ).

:- pred compare_file_timestamps(file_name::in, file_name::in,
    maybe(comparison_result)::out, io::di, io::uo) is det.

compare_file_timestamps(FileNameA, FileNameB, MaybeCompare, !IO) :-
    io.file_modification_time(FileNameA, TimeResultA, !IO),
    io.file_modification_time(FileNameB, TimeResultB, !IO),
    (
        TimeResultA = ok(TimeA),
        TimeResultB = ok(TimeB)
    ->
        compare(Compare, TimeA, TimeB),
        MaybeCompare = yes(Compare)
    ;
        MaybeCompare = no
    ).

%-----------------------------------------------------------------------------%

% WARNING: The code here duplicates the functionality of scripts/ml.in.
% Any changes there may also require changes here, and vice versa.

link(ErrorStream, LinkTargetType, ModuleName, ObjectsList, Succeeded, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    globals.io_lookup_bool_option(statistics, Stats, !IO),
    globals.io_get_target(Target, !IO),

    maybe_write_string(Verbose, "% Linking...\n", !IO),
    link_output_filename(LinkTargetType, ModuleName, _Ext, OutputFileName, !IO),
    (
        LinkTargetType = static_library,
        create_archive(ErrorStream, OutputFileName, yes, ObjectsList,
            LinkSucceeded, !IO)
    ;
        LinkTargetType = java_archive,
        create_java_archive(ErrorStream, ModuleName, OutputFileName,
            ObjectsList, LinkSucceeded, !IO)
    ;
        LinkTargetType = erlang_archive,
        create_erlang_archive(ErrorStream, ModuleName, OutputFileName,
            ObjectsList, LinkSucceeded, !IO)
    ;
        LinkTargetType = executable,
        (
            Target = target_erlang,
            create_erlang_shell_script(ModuleName, LinkSucceeded, !IO)
        ;
            ( Target = target_c
            ; Target = target_il
            ; Target = target_java
            ; Target = target_asm
            ; Target = target_x86_64
            ),
            link_exe_or_shared_lib(ErrorStream, LinkTargetType, ModuleName,
                OutputFileName, ObjectsList, LinkSucceeded, !IO)
        )
    ;
        LinkTargetType = shared_library,
        link_exe_or_shared_lib(ErrorStream, LinkTargetType, ModuleName,
            OutputFileName, ObjectsList, LinkSucceeded, !IO)
    ),
    maybe_report_stats(Stats, !IO),
    (
        LinkSucceeded = yes,
        post_link_make_symlink_or_copy(ErrorStream, LinkTargetType,
            ModuleName, Succeeded, _MadeSymlinkOrCopy, !IO)
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
        module_name_to_lib_file_name("lib", ModuleName, Ext, do_create_dirs,
            OutputFileName, !IO)
    ;
        LinkTargetType = shared_library,
        globals.io_lookup_string_option(shared_library_extension, Ext, !IO),
        module_name_to_lib_file_name("lib", ModuleName, Ext, do_create_dirs,
            OutputFileName, !IO)
    ;
        LinkTargetType = java_archive,
        Ext = ".jar",
        module_name_to_file_name(ModuleName, Ext, do_create_dirs,
            OutputFileName, !IO)
    ;
        LinkTargetType = erlang_archive,
        Ext = ".beams",
        module_name_to_lib_file_name("lib", ModuleName, Ext, do_create_dirs,
            OutputFileName, !IO)
    ;
        LinkTargetType = executable,
        globals.io_lookup_string_option(executable_file_extension, Ext, !IO),
        module_name_to_file_name(ModuleName, Ext, do_create_dirs,
            OutputFileName, !IO)
    ).

:- pred link_exe_or_shared_lib(io.output_stream::in,
    linked_target_type::in(bound(executable ; shared_library)),
    module_name::in, file_name::in, list(string)::in, bool::out,
    io::di, io::uo) is det.

link_exe_or_shared_lib(ErrorStream, LinkTargetType, ModuleName,
        OutputFileName, ObjectsList, LinkSucceeded, !IO) :-
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
        globals.io_lookup_string_option(linker_static_flags, StaticOpts, !IO)
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
    get_mercury_std_libs(LinkTargetType, MercuryStdLibs, !IO),

    % Find which system libraries are needed.
    get_system_libs(LinkTargetType, SystemLibs, !IO),

    globals.io_lookup_accumulating_option(LDFlagsOpt, LDFlagsList, !IO),
    join_string_list(LDFlagsList, "", "", " ", LDFlags),
    globals.io_lookup_accumulating_option(link_library_directories,
        LinkLibraryDirectoriesList, !IO),
    globals.io_lookup_string_option(linker_path_flag, LinkerPathFlag,
        !IO),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),

    % Set up the runtime library path.
    get_runtime_library_path_opts(LinkTargetType, RpathFlagOpt, RpathSepOpt,
        RpathOpts, !IO),

    % Set up the install name for shared libraries.
    globals.io_lookup_bool_option(shlib_linker_use_install_name,
        UseInstallName, !IO),
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

    get_link_libraries(MaybeLinkLibraries, !IO),
    globals.io_lookup_string_option(linker_opt_separator,
        LinkOptSep, !IO),
    (
        MaybeLinkLibraries = yes(LinkLibrariesList),
        join_quoted_string_list(LinkLibrariesList, "", "", " ",
            LinkLibraries),

        globals.io_lookup_bool_option(restricted_command_line,
            RestrictedCommandLine, !IO),
        (
            % If we have a restricted command line then it's possible
            % that following link command will call sub-commands its self
            % and thus overflow the command line, so in this case
            % we first create an archive of all of the 
            %
            RestrictedCommandLine = yes,
            io.make_temp(TmpFile, !IO),
            globals.io_lookup_string_option(library_extension, LibExt, !IO),
            TmpArchive = TmpFile ++ LibExt,
            create_archive(ErrorStream, TmpArchive, yes, ObjectsList,
                ArchiveSucceeded, !IO),
            MaybeDeleteTmpArchive = yes(TmpArchive),
            Objects = TmpArchive
        ;
            RestrictedCommandLine = no,
            ArchiveSucceeded = yes,
            MaybeDeleteTmpArchive = no,
            join_quoted_string_list(ObjectsList, "", "", " ", Objects)
        ),
        
        ( 
            ArchiveSucceeded = yes,

            % Note that LDFlags may contain `-l' options so it should come
            % after Objects.
            globals.io_lookup_string_option(CommandOpt, Command, !IO),
            string.append_list([
                    Command, " ",
                    StaticOpts, " ",
                    StripOpt, " ",
                    UndefOpt, " ",
                    ThreadOpts, " ",
                    TraceOpts, " ",
                    " -o ", OutputFileName, " ",
                    Objects, " ",
                    LinkOptSep, " ",
                    LinkLibraryDirectories, " ",
                    RpathOpts, " ",
                    InstallNameOpt, " ",
                    DebugOpts, " ",
                    LDFlags, " ",
                    LinkLibraries, " ",
                    MercuryStdLibs, " ",
                    SystemLibs], LinkCmd),

            globals.io_lookup_bool_option(demangle, Demangle, !IO),
            (
                Demangle = yes,
                globals.io_lookup_string_option(demangle_command,
                    DemangleCmd, !IO),
                MaybeDemangleCmd = yes(DemangleCmd)
            ;
                Demangle = no,
                MaybeDemangleCmd = no
            ),

            invoke_system_command_maybe_filter_output(ErrorStream,
                cmd_verbose_commands, LinkCmd, MaybeDemangleCmd,
                LinkSucceeded, !IO)
        ;
            ArchiveSucceeded = no,
            LinkSucceeded = no
        ),
        (
            MaybeDeleteTmpArchive = yes(FileToDelete),
            io.remove_file(FileToDelete, _, !IO)
        ;
            MaybeDeleteTmpArchive = no
        )
    ;
        MaybeLinkLibraries = no,
        LinkSucceeded = no
    ).
    
    % Find the standard Mercury libraries, and the system
    % libraries needed by them.
    % Return the empty string if --mercury-standard-library-directory
    % is not set.
    %
:- pred get_mercury_std_libs(linked_target_type::in, string::out,
    io::di, io::uo) is det.

get_mercury_std_libs(TargetType, StdLibs, !IO) :-
    globals.io_lookup_maybe_string_option(
        mercury_standard_library_directory, MaybeStdlibDir, !IO),
    (
        MaybeStdlibDir = yes(StdLibDir),
        globals.io_get_gc_method(GCMethod, !IO),
        (
            ( TargetType = executable
            ; TargetType = static_library
            ; TargetType = shared_library
            ),
            globals.io_lookup_string_option(library_extension, LibExt, !IO)
        ;
            TargetType = java_archive,
            unexpected(this_file, "get_mercury_std_libs: java_archive")
        ;
            TargetType = erlang_archive,
            unexpected(this_file, "get_mercury_std_libs: erlang_archive")
        ),
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
            (
                GCMethod = gc_boehm,
                GCGrade0 = "gc"
            ;
                GCMethod = gc_boehm_debug,
                GCGrade0 = "gc_debug"
            ),
            globals.io_lookup_bool_option(profile_time, ProfTime, !IO),
            globals.io_lookup_bool_option(profile_deep, ProfDeep, !IO),
            (
                ( ProfTime = yes
                ; ProfDeep = yes
                )
            ->
                GCGrade1 = GCGrade0 ++ "_prof"
            ;
                GCGrade1 = GCGrade0
            ),
            globals.io_lookup_bool_option(parallel, Parallel, !IO),
            (
                Parallel = yes,
                GCGrade = "par_" ++ GCGrade1
            ;
                Parallel = no,
                GCGrade = GCGrade1
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
                    ("libmer_eventspec" ++ LibExt)) ++
                " " ++
                quote_arg(StdLibDir/"lib"/GradeDir/
                    ("libmer_browser" ++ LibExt)) ++
                " " ++
                quote_arg(StdLibDir/"lib"/GradeDir/
                    ("libmer_mdbcomp" ++ LibExt)),
            make_link_lib(TargetType, "mer_trace", TraceLib, !IO),
            make_link_lib(TargetType, "mer_eventspec", EventSpecLib, !IO),
            make_link_lib(TargetType, "mer_browser", BrowserLib, !IO),
            make_link_lib(TargetType, "mer_mdbcomp", MdbCompLib, !IO),
            SharedTraceLibs = string.join_list(" ",
                [TraceLib, EventSpecLib, BrowserLib, MdbCompLib])
        ),

        % Source-to-source debugging libraries.
        globals.io_lookup_bool_option(source_to_source_debug, SourceDebug, !IO),
        (
            SourceDebug = yes,
            StaticSourceDebugLibs =
                quote_arg(StdLibDir/"lib"/GradeDir/
                    ("libmer_ssdb" ++ LibExt)),
            make_link_lib(TargetType, "mer_mdbcomp", SharedSourceDebugLibs, !IO)
        ;
            SourceDebug = no,
            StaticSourceDebugLibs = "",
            SharedSourceDebugLibs = ""
        ),

        globals.io_lookup_string_option(mercury_linkage, MercuryLinkage, !IO),
        ( MercuryLinkage = "static" ->
            StdLibs = string.join_list(" ", [
                StaticTraceLibs,
                StaticSourceDebugLibs,
                quote_arg(StdLibDir/"lib"/GradeDir/
                    ("libmer_std" ++ LibExt)),
                quote_arg(StdLibDir/"lib"/GradeDir/
                    ("libmer_rt" ++ LibExt)),
                StaticGCLibs
            ])
        ; MercuryLinkage = "shared" ->
            make_link_lib(TargetType, "mer_std", StdLib, !IO),
            make_link_lib(TargetType, "mer_rt", RuntimeLib, !IO),
            StdLibs = string.join_list(" ", [
                SharedTraceLibs,
                SharedSourceDebugLibs,
                StdLib,
                RuntimeLib,
                SharedGCLibs
            ])
        ;
            unexpected(this_file, "unknown linkage " ++ MercuryLinkage)
        )
    ;
        MaybeStdlibDir = no,
        StdLibs = ""
    ).
    
    % Pass either `-llib' or `PREFIX/lib/GRADE/liblib.a',
    % depending on whether we are linking with static or shared
    % Mercury libraries.
    %
:- pred get_link_libraries(maybe(list(string))::out, io::di, io::uo) is det.

get_link_libraries(MaybeLinkLibraries, !IO) :-
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
    (
        LibrariesSucceeded = yes,
        MaybeLinkLibraries = yes(LinkLibrariesList)
    ;
        LibrariesSucceeded = no,
        MaybeLinkLibraries = no
    ).

:- pred make_link_lib(linked_target_type::in, string::in, string::out,
    io::di, io::uo) is det.

make_link_lib(TargetType, LibName, LinkOpt, !IO) :-
    (
        (
            TargetType = executable,
            LinkLibFlag = linker_link_lib_flag,
            LinkLibSuffix = linker_link_lib_suffix
        ;
            TargetType = shared_library,
            LinkLibFlag = shlib_linker_link_lib_flag,
            LinkLibSuffix = shlib_linker_link_lib_suffix
        ),
        globals.io_lookup_string_option(LinkLibFlag, LinkLibOpt, !IO),
        globals.io_lookup_string_option(LinkLibSuffix, Suffix, !IO),
        LinkOpt = quote_arg(LinkLibOpt ++ LibName ++ Suffix)
    ;
        TargetType = java_archive,
        unexpected(this_file, "make_link_lib: java_archive")
    ;
        TargetType = erlang_archive,
        unexpected(this_file, "make_link_lib: erlang_archive")
    ;
        TargetType = static_library,
        unexpected(this_file, "make_link_lib: static_library")
    ).

:- pred get_runtime_library_path_opts(linked_target_type::in,
    option::in(bound(shlib_linker_rpath_flag ; linker_rpath_flag)),
    option::in(bound(shlib_linker_rpath_separator ; linker_rpath_separator)),
    string::out, io::di, io::uo) is det.

get_runtime_library_path_opts(LinkTargetType, RpathFlagOpt, RpathSepOpt,
        RpathOpts, !IO) :-
    globals.io_lookup_bool_option(shlib_linker_use_install_name,
        UseInstallName, !IO),
    shared_libraries_supported(SharedLibsSupported, !IO),
    globals.io_lookup_string_option(linkage, Linkage, !IO),
    (
        UseInstallName = no,
        SharedLibsSupported = yes,
        ( Linkage = "shared"
        ; LinkTargetType = shared_library
        )
    ->
        globals.io_lookup_accumulating_option(runtime_link_library_directories,
            RpathDirs0, !IO),
        RpathDirs = list.map(quote_arg, RpathDirs0),
        ( 
            RpathDirs = [],
            RpathOpts = ""
        ;
            RpathDirs = [_ | _],
            globals.io_lookup_string_option(RpathSepOpt, RpathSep, !IO),
            globals.io_lookup_string_option(RpathFlagOpt, RpathFlag, !IO),
            RpathOpts0 = string.join_list(RpathSep, RpathDirs),
            RpathOpts = RpathFlag ++ RpathOpts0
        )
    ;
        RpathOpts = ""
    ).

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
        TargetType = erlang_archive,
        unexpected(this_file, "get_std_libs: erlang archive")
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
        Succeeded, MadeSymlinkOrCopy, !IO) :-
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    (
        UseGradeSubdirs = yes,
        link_output_filename(LinkTargetType, ModuleName,
            Ext, OutputFileName, !IO),
        % Link/copy the executable into the user's directory.
        globals.io_set_option(use_subdirs, bool(no), !IO),
        globals.io_set_option(use_grade_subdirs, bool(no), !IO),
        ( 
            LinkTargetType = executable,
            module_name_to_file_name(ModuleName, Ext, do_not_create_dirs,
                UserDirFileName, !IO)
        ;
            ( LinkTargetType = static_library
            ; LinkTargetType = shared_library
            ; LinkTargetType = java_archive
            ; LinkTargetType = erlang_archive
            ),
            module_name_to_lib_file_name("lib", ModuleName, Ext,
                do_not_create_dirs, UserDirFileName, !IO)
        ),
        globals.io_set_option(use_subdirs, bool(yes), !IO),
        globals.io_set_option(use_grade_subdirs, bool(yes), !IO),

        same_timestamp(OutputFileName, UserDirFileName, SameTimestamp, !IO),
        (
            SameTimestamp = yes,
            Succeeded = yes,
            MadeSymlinkOrCopy = no
        ;
            SameTimestamp = no,
            io.set_output_stream(ErrorStream, OutputStream, !IO),
            % Remove the target of the symlink/copy in case it already exists.
            io.remove_file_recursively(UserDirFileName, _, !IO),
            make_symlink_or_copy_file(OutputFileName, UserDirFileName,
                Succeeded, !IO),
            io.set_output_stream(OutputStream, _, !IO),
            MadeSymlinkOrCopy = yes
        )
    ;
        UseGradeSubdirs = no,
        Succeeded = yes,
        MadeSymlinkOrCopy = no
    ).

:- pred same_timestamp(string::in, string::in, bool::out, io::di, io::uo)
    is det.

same_timestamp(FileNameA, FileNameB, SameTimestamp, !IO) :-
    compare_file_timestamps(FileNameA, FileNameB, MaybeCompare, !IO),
    ( MaybeCompare = yes(=) ->
        SameTimestamp = yes
    ;
        SameTimestamp = no
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
        module_name_to_lib_file_name("lib", LibModuleName, LibExt,
            do_not_create_dirs, LibFileName, !IO),
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

    MakeLibCmdArgs = string.append_list([
        ArFlags, " ", ArOutputFlag, " ",
        LibFileName, " ", Objects]),

    invoke_long_system_command(ErrorStream, cmd_verbose_commands,
        ArCmd, MakeLibCmdArgs, MakeLibCmdSucceeded, !IO),

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

%-----------------------------------------------------------------------------%

    % Create an "Erlang archive", which is simply a directory containing
    % `.beam' files.
    %
:- pred create_erlang_archive(io.output_stream::in, module_name::in,
    file_name::in, list(file_name)::in, bool::out, io::di, io::uo) is det.

create_erlang_archive(ErrorStream, _ModuleName, ErlangArchiveFileName,
        ObjectList, Succeeded, !IO) :-
    % Delete anything in the way first.
    io.remove_file_recursively(ErlangArchiveFileName, _, !IO),
    dir.make_directory(ErlangArchiveFileName, Res, !IO),
    (
        Res = ok,
        copy_erlang_archive_files(ErrorStream, ErlangArchiveFileName,
            ObjectList, Succeeded, !IO)
    ;
        Res = error(Error),
        io.write_string(ErrorStream, "Error creating `", !IO),
        io.write_string(ErrorStream, ErlangArchiveFileName, !IO),
        io.write_string(ErrorStream, "': ", !IO),
        io.write_string(ErrorStream, io.error_message(Error), !IO),
        io.nl(ErrorStream, !IO),
        Succeeded = no
    ).

:- pred copy_erlang_archive_files(io.output_stream::in, file_name::in,
    list(file_name)::in, bool::out, io::di, io::uo) is det.

copy_erlang_archive_files(_ErrorStream, _ErlangArchiveFileName, [], yes, !IO).
copy_erlang_archive_files(ErrorStream, ErlangArchiveFileName, [Obj | Objs],
        Succeeded, !IO) :-
    copy_file(Obj, ErlangArchiveFileName, Res, !IO),
    (
        Res = ok,
        copy_erlang_archive_files(ErrorStream, ErlangArchiveFileName, Objs,
            Succeeded, !IO)
    ;
        Res = error(Error),
        io.write_string(ErrorStream, "Error copying `", !IO),
        io.write_string(ErrorStream, Obj, !IO),
        io.write_string(ErrorStream, "': ", !IO),
        io.write_string(ErrorStream, io.error_message(Error), !IO),
        io.nl(ErrorStream, !IO),
        Succeeded = no
    ).

%-----------------------------------------------------------------------------%

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
            ( FileType = static_library
            ; FileType = java_archive
            ; FileType = erlang_archive
            ),
            ObjectCodeType = non_pic
        ;
            FileType = shared_library,
            ObjectCodeType = ( if PicObjExt = ObjExt then non_pic else pic )
        ;
            FileType = executable,
            ( MercuryLinkage = "shared" ->
                (
                    % We only need to create `.lpic' files if `-DMR_PIC_REG'
                    % has an effect, which is currently nowhere.
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

    % join_string_list(Strings, Prefix, Suffix, Separator, Result):
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
    %
:- pred join_module_list(list(string)::in, string::in, list(string)::out,
    io::di, io::uo) is det.

join_module_list([], _Extension, [], !IO).
join_module_list([Module | Modules], Extension, [FileName | Rest], !IO) :-
    file_name_to_module_name(dir.basename_det(Module), ModuleName),
    module_name_to_file_name(ModuleName, Extension, do_not_create_dirs,
        FileName, !IO),
    join_module_list(Modules, Extension, Rest, !IO).

%-----------------------------------------------------------------------------%

make_all_module_command(Command0, MainModule, AllModules, Command, !IO) :-
    % Pass the main module first.
    list.map_foldl(
        (pred(Module::in, FileName::out, IO0::di, IO::uo) is det :-
            module_name_to_file_name(Module, ".m", do_not_create_dirs,
                FileName, IO0, IO)
        ),
        [MainModule | list.delete_all(AllModules, MainModule)],
        ModuleNameStrings, !IO),
    Command = string.join_list(" ",
        list.map(quote_arg, [Command0 | ModuleNameStrings])).

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(maybe_pic_object_file_extension/3).

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
%
% Standalone interfaces
%

% NOTE: the following code is similar to that of make_init_obj/7.  Any
% changes here may need to be reflected there.

make_standalone_interface(Basename, !IO) :-
    make_standalone_int_header(Basename, HdrSucceeded, !IO),
    (
        HdrSucceeded = yes,
        make_standalone_int_body(Basename, !IO)
    ;
        HdrSucceeded = no
    ).

:- pred make_standalone_int_header(string::in, bool::out,
    io::di, io::uo) is det.

make_standalone_int_header(Basename, Succeeded, !IO) :-
    HdrFileName = Basename ++ ".h", 
    io.open_output(HdrFileName, OpenResult, !IO),
    (
        OpenResult = ok(HdrFileStream),
        io.write_strings(HdrFileStream, [
            "#ifndef ", to_upper(Basename), "_H\n",
            "#define ", to_upper(Basename), "_H\n",
            "\n",
            "#ifdef __cplusplus\n",
            "extern \"C\" {\n",
            "#endif\n",
            "\n",
            "extern void\n",
            "mercury_init(int argc, char **argv, void *stackbottom);\n",
            "\n",
            "extern int\n",
            "mercury_terminate(void);\n",
            "\n",
            "#ifdef __cplusplus\n",
            "}\n",
            "#endif\n",
            "\n",
            "#endif /* ", to_upper(Basename), "_H */\n"],
            !IO),
        io.close_output(HdrFileStream, !IO),
        Succeeded = yes
    ;
        OpenResult = error(Error),
        unable_to_open_file(HdrFileName, Error, !IO),
        Succeeded = no
    ).

:- pred make_standalone_int_body(string::in, io::di, io::uo) is det.

make_standalone_int_body(Basename, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_accumulating_option(Globals, init_files, InitFiles0),
    globals.lookup_accumulating_option(Globals, trace_init_files,
        TraceInitFiles0),
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    grade_directory_component(Globals, GradeDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        InitFiles1 = [
            StdLibDir / "modules" / GradeDir / "mer_rt.init",
            StdLibDir / "modules" / GradeDir / "mer_std.init" |
            InitFiles0
        ],
        TraceInitFiles = [
            StdLibDir / "modules" / GradeDir / "mer_browser.init",
            StdLibDir / "modules" / GradeDir / "mer_mdbcomp.init" |
            TraceInitFiles0
        ],
        SourceDebugInitFiles = [
            StdLibDir / "modules" / GradeDir / "mer_ssdb.init"
        ]
    ;
        % Supporting `--no-mercury-standard-library-directory' is necessary
        % in order to use `--generate-standalone-interface' with the
        % the lmc script.
        MaybeStdLibDir = no,
        InitFiles1 = InitFiles0,
        TraceInitFiles = TraceInitFiles0,
        SourceDebugInitFiles = []
    ),
    globals.get_trace_level(Globals, TraceLevel),
    ( given_trace_level_is_none(TraceLevel) = no ->
        TraceOpt = "-t",
        InitFiles2 = InitFiles1 ++ TraceInitFiles
    ;
        TraceOpt = "",
        InitFiles2 = InitFiles1
    ),
    globals.lookup_bool_option(Globals, source_to_source_debug, SourceDebug),
    (
        SourceDebug = yes,
        InitFiles = InitFiles2 ++ SourceDebugInitFiles
    ;
        SourceDebug = no,
        InitFiles = InitFiles2
    ),
    globals.lookup_accumulating_option(Globals, runtime_flags,
        RuntimeFlagsList),
    join_quoted_string_list(RuntimeFlagsList, "-r ", "", " ", RuntimeFlags),
    globals.io_lookup_accumulating_option(init_file_directories,
        InitFileDirsList, !IO),
    join_quoted_string_list(InitFileDirsList, "-I ", "", " ", InitFileDirs),
    globals.lookup_string_option(Globals, experimental_complexity,
        ExperimentalComplexity),
    ( ExperimentalComplexity = "" ->
        ExperimentalComplexityOpt = ""
    ;
        ExperimentalComplexityOpt = "-X " ++ ExperimentalComplexity
    ),
    compute_grade(Globals, Grade),
    globals.lookup_string_option(Globals, mkinit_command, MkInit),
    CFileName = Basename ++ ".c",
    io.output_stream(ErrorStream, !IO),
    MkInitArgs = string.append_list(
        [   " -g ", Grade,
            " ", TraceOpt,
            " ", ExperimentalComplexityOpt,
            " ", RuntimeFlags,
            " -o ", quote_arg(CFileName),
            " ", InitFileDirs,
            " -s "
        ]),

    invoke_mkinit(ErrorStream, cmd_verbose_commands,
        MkInit, MkInitArgs, InitFiles, MkInitCmdOk, !IO),
    (
        MkInitCmdOk = yes,
        get_object_code_type(executable, PIC, !IO),
        maybe_pic_object_file_extension(PIC, ObjExt, !IO),
        ObjFileName = Basename ++ ObjExt,
        compile_c_file(ErrorStream, PIC, CFileName, ObjFileName,
            CompileOk, !IO),
        (
            CompileOk = yes
        ;
            CompileOk = no,
            io.set_exit_status(1, !IO),
            io.write_string("mercury_compile: error while compiling ", !IO),
            io.write_string("standalone interface in `", !IO),
            io.write_string(CFileName, !IO),
            io.write_string("'\n", !IO)
        )
    ;
        MkInitCmdOk = no,
        io.set_exit_status(1, !IO),
        io.write_string("mercury_compile: error while creating ", !IO),
        io.write_string("standalone interface in `", !IO),
        io.write_string(CFileName, !IO),
        io.write_string("'\n", !IO)
    ).

%-----------------------------------------------------------------------------%

    %
    % invoke_long_system_command attempts to use the @file style of
    % calling to avoid command line length arguments on various systems.
    % If the underlying tool chain doesn't support this it just calls
    % the normal invoke_system_command and hopes the command isn't too
    % long.
    %
:- pred invoke_long_system_command(io.output_stream::in,
    command_verbosity::in, string::in, string::in,
    bool::out, io::di, io::uo) is det.

invoke_long_system_command(ErrorStream, Verbosity, Cmd, Args, Succeeded, !IO) :-
    invoke_long_system_command_maybe_filter_output(ErrorStream, Verbosity,
        Cmd, Args, no, Succeeded, !IO).

:- pred invoke_long_system_command_maybe_filter_output(io.output_stream::in,
    command_verbosity::in, string::in, string::in, maybe(string)::in,
    bool::out, io::di, io::uo) is det.

invoke_long_system_command_maybe_filter_output(ErrorStream, Verbosity,
        Cmd, Args, MaybeProcessOutput, Succeeded, !IO) :-
    globals.io_lookup_bool_option(restricted_command_line,
        RestrictedCommandLine, !IO),

    (
        RestrictedCommandLine = yes,

        %
        % Avoid generating very long command lines by using @files
        %
        io.make_temp(TmpFile, !IO),
        io.open_output(TmpFile, OpenResult, !IO),
        (
            OpenResult = ok(TmpStream),

            % We need to escape any \ before writing them to
            % the file, otherwise we lose them
            TmpFileArgs = string.replace_all(Args, "\\", "\\\\"),

            io.write_string(TmpStream, TmpFileArgs, !IO),
            io.close_output(TmpStream, !IO),

            globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
            (
                VeryVerbose = yes,
                io.write_string("% Args placed in @" ++ TmpFile ++ ": `", !IO),
                io.write_string(TmpFileArgs, !IO),
                io.write_string("'\n", !IO),
                io.flush_output(!IO)
            ;
                VeryVerbose = no
            ),

            FullCmd = string.append_list([Cmd, " @", TmpFile]),
            invoke_system_command(ErrorStream, Verbosity,
                FullCmd, Succeeded0, !IO),

            io.remove_file(TmpFile, RemoveResult, !IO),
            (
                RemoveResult = ok,
                Succeeded = Succeeded0
            ;
                RemoveResult = error(_),
                Succeeded = no
            )
        ;
            OpenResult = error(_),
            Succeeded = no
        )
        
    ;
        RestrictedCommandLine = no,
        FullCmd = Cmd ++ " " ++ Args,
        invoke_system_command_maybe_filter_output(ErrorStream, Verbosity,
            FullCmd, MaybeProcessOutput, Succeeded, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% C compiler flags
%

output_c_compiler_flags(Stream, !IO) :-
    get_object_code_type(executable, PIC, !IO),
    gather_c_compiler_flags(PIC, CFlags, !IO),
    io.write_string(Stream, CFlags, !IO).    

%-----------------------------------------------------------------------------%
%
% Library link flags
%

output_library_link_flags(Stream, !IO) :-
    
    % We output the library link flags as they are for when we are linking
    % an executable.
    LinkTargetType = executable,
    RpathFlagOpt = linker_rpath_flag,
    RpathSepOpt = linker_rpath_separator,
    
    globals.io_lookup_accumulating_option(link_library_directories,
        LinkLibraryDirectoriesList, !IO),
    globals.io_lookup_string_option(linker_path_flag, LinkerPathFlag,
        !IO),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),
    get_runtime_library_path_opts(LinkTargetType, RpathFlagOpt, RpathSepOpt,
        RpathOpts, !IO), 
    get_link_libraries(MaybeLinkLibraries, !IO),
    (   
        MaybeLinkLibraries = yes(LinkLibrariesList),
        join_quoted_string_list(LinkLibrariesList, "", "", " ",
            LinkLibraries)
    ;
        MaybeLinkLibraries = no,
        LinkLibraries = ""
    ),
    % Find the Mercury standard libraries.
    get_mercury_std_libs(LinkTargetType, MercuryStdLibs, !IO),
    get_system_libs(LinkTargetType, SystemLibs, !IO),
    string.append_list([
        LinkLibraryDirectories, " ",
        RpathOpts, " ",
        LinkLibraries, " ",
        MercuryStdLibs, " ",
        SystemLibs], LinkFlags),
    io.write_string(Stream, LinkFlags, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "compile_target_code.m".

%-----------------------------------------------------------------------------%
:- end_module compile_target_code.
%-----------------------------------------------------------------------------%
