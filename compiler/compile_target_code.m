%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: compile_target_code.m.
% Main authors: fjh, stayl.
%
% Code to compile the generated `.c', `.java', etc files.
%
%---------------------------------------------------------------------------%

:- module backend_libs.compile_target_code.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_dep_info.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Are we generating position independent code (for use in a shared
    % library)? On some architectures, pic and non-pic code are incompatible,
    % so we need to generate `.o' and `.pic_o' files.
    %
:- type pic
    --->    pic
    ;       non_pic.

    % compile_c_file(Globals, ProgressStream, PIC, ModuleName, Succeeded, !IO)
    %
:- pred compile_c_file(globals::in, io.text_output_stream::in, pic::in,
    module_name::in, maybe_succeeded::out, io::di, io::uo) is det.

    % do_compile_c_file(Globals, ProgressStream, PIC, CFile, ObjFile,
    %   Succeeded, !IO)
    %
:- pred do_compile_c_file(globals::in, io.text_output_stream::in, pic::in,
    string::in, string::in, maybe_succeeded::out, io::di, io::uo) is det.

    % compile_java_files(Globals, ProgressStream, HeadJavaFile, TailJavaFiles,
    %   Succeeded, !IO)
    %
:- pred compile_java_files(globals::in, io.text_output_stream::in,
    string::in, list(string)::in, maybe_succeeded::out, io::di, io::uo) is det.

    % compile_csharp_file(Globals, ProgressStream, ErrorStream,
    %   CSharpFile, DLLFile, Succeeded, !IO)
    %
:- pred compile_csharp_file(globals::in, io.text_output_stream::in,
    module_dep_info::in, file_name::in, file_name::in, maybe_succeeded::out,
    io::di, io::uo) is det.

    % make_library_init_file(Globals, ProgressStream,
    %   MainModuleName, ModuleNames, Succeeded, !IO):
    %
    % Make the `.init' file for a library containing the given modules.
    %
:- pred make_library_init_file(globals::in, io.text_output_stream::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

    % make_init_obj_file(Globals, ProgressStream,
    %   MainModuleName, AllModuleNames, MaybeInitObjFileName, !IO)
    %
:- pred make_init_obj_file(globals::in,
    io.text_output_stream::in,
    module_name::in, list(module_name)::in, maybe(file_name)::out,
    io::di, io::uo) is det.

    % link_module_list(ProgressStream, ModulesToLink, ExtraObjFiles,
    %   Globals, Specs, Succeeded, !IO):
    %
    % The elements of ModulesToLink are the output of
    % `module_name_to_filename(ModuleName, "", no, ModuleToLink)'
    % for each module in the program.
    %
    % The Globals are supplied late to allow mercury_compile.m to
    % partially apply the preceding arguments.
    %
:- pred link_module_list(io.text_output_stream::in,
    list(string)::in, list(string)::in, globals::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

:- type linked_target_type
    --->    executable
    ;       static_library
    ;       shared_library
    ;       csharp_executable
    ;       csharp_library
    ;       java_executable
    ;       java_archive.

    % link(Globals, ProgressStream, TargetType, MainModuleName,
    %   ObjectFileNames, Specs, Succeeded, !IO)
    %
:- pred link(globals::in, io.text_output_stream::in,
    linked_target_type::in, module_name::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

:- pred linked_target_file_name_full_curdir(globals::in, module_name::in,
    linked_target_type::in, file_name::out, file_name::out,
    io::di, io::uo) is det.

    % post_link_maybe_make_symlink_or_copy(Globals,
    %   ProgressStream, FullFileName, CurDirFileName,
    %   ModuleName, LinkTargetType, Succeeded, MadeSymlinkOrCopy, !IO):
    %
    % If FullFileName, which results from converting MainModuleName
    % and TargetType to a fully specified relative pathname, differs from
    % CurDirFileName, which is the same filename in the current directory,
    % then link or copy FileName into the current directory after having
    % successfully built it, if the target does not exist or is not up-to-date.
    %
:- pred post_link_maybe_make_symlink_or_copy(globals::in,
    io.text_output_stream::in, file_name::in, file_name::in,
    module_name::in, linked_target_type::in, maybe_succeeded::out, bool::out,
    io::di, io::uo) is det.

    % shared_libraries_supported(Globals, SharedLibsSupported)
    %
    % Return whether or not shared libraries are supported on the current
    % platform.
    %
:- pred shared_libraries_supported(globals::in, bool::out) is det.

    % get_object_code_type(Globals, TargetType, PIC):
    %
    % Work out whether we should be generating position-independent
    % object code.
    %
:- pred get_object_code_type(globals::in, linked_target_type::in, pic::out)
    is det.

    % get_linked_target_type(Globals, LinkedTargetType):
    %
    % Work out whether we should be generating an executable or a shared
    % object.
    %
:- pred get_linked_target_type(globals::in, linked_target_type::out) is det.

%---------------------------------------------------------------------------%

    % make_all_module_command(CommandName, MainModule, AllModuleNames,
    %   CommandString, !IO):
    %
    % Create a command string which passes the source file names
    % for AllModuleNames to CommandName, with MainModule given first.
    %
:- pred make_all_module_command(string::in, module_name::in,
    list(module_name)::in, string::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- inst ext_cur_ngs_gas_obj for ext_cur_ngs_gas/0
    --->    ext_cur_ngs_gas_obj_obj_opt
    ;       ext_cur_ngs_gas_obj_pic_obj_opt.
:- inst ext_cur_ngs_gas_init_obj for ext_cur_ngs_gas/0
    --->    ext_cur_ngs_gas_init_obj_obj_opt
    ;       ext_cur_ngs_gas_init_obj_pic_obj_opt.

    % maybe_pic_object_file_extension(PIC, ExtObj, ExtInitObj):
    %
    % ExtObj is the extension which should be used on object files
    % according to the value of PIC. The value of PIC should be obtained
    % from a call to `get_object_code_type'. In particular, on architectures
    % for which no special handling for PIC is necessary, only a value of
    % `non_pic' should be used.
    %
    % The value of ExtInitObj will specify the extension of
    % <mainmodule>_init's object file, whose "pic-ness' is handled
    % the same way as the other extensions we return.
    %
:- pred maybe_pic_object_file_extension(pic::in,
    ext_cur_ngs_gas::out(ext_cur_ngs_gas_obj),
    ext_cur_ngs_gas::out(ext_cur_ngs_gas_init_obj)) is det.

    % This predicate is sort-of the converse of pic_object_file_extension.
    % It tests whether the given extension string is an object file extension
    % or not, and if it is, it tells you whether such object files are
    % PIC or not.
    %
:- pred is_maybe_pic_object_file_extension(globals::in, string::in, pic::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Stuff used for standalone interfaces.
%

    % make_standalone_interface(Globals, ProgressStream, BaseName, !IO):
    %
    % Create a standalone interface in the current directory.
    %
:- pred make_standalone_interface(globals::in, io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

    % Output the C compiler flags to the given stream.
    % This predicate is used to implement the `--output-cflags' option.
    %
:- pred output_c_compiler_flags(globals::in, io.text_output_stream::in,
    io::di, io::uo) is det.

    % Output the C compiler flags that define the macros used to specify the
    % current compilation grade to the given stream.
    % This predicate is used to implement the `--output-grade-defines' option.
    %
:- pred output_c_grade_defines(globals::in, io.text_output_stream::in,
    io::di, io::uo) is det.

    % Output the C compiler flags that specify where the C compiler should
    % search for header files to the given stream.
    % This predicate is used to implement the `--output-c-include-dir-flags'
    % option.
    %
:- pred output_c_include_directory_flags(globals::in,
    io.text_output_stream::in, io::di, io::uo) is det.

    % Output the list of flags required to link against the selected set
    % of Mercury libraries (the standard libraries, plus any other specified
    % via the --ml option) in the current grade.
    % This predicate is used to implement the `--output-library-link-flags'
    % option.
    %
    % It is a bit strange that task this can generate error messages,
    % but it does do a search in the filesystem for libraries that are
    % named in the values of options, and those searches can fail.
    %
:- pred output_library_link_flags(globals::in, io.text_output_stream::in,
    list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.trace_params.
:- import_module parse_tree.find_module.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.

:- import_module dir.
:- import_module io.file.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

:- inst c_linked_target_type for linked_target_type/0
    --->    executable
    ;       static_library
    ;       shared_library.

:- inst c_exe_or_shared_lib for linked_target_type/0
    --->    executable
    ;       shared_library.

:- inst csharp_linked_target_type for linked_target_type/0
    --->    csharp_executable
    ;       csharp_library.

:- inst java_linked_target_type for linked_target_type/0
    --->    java_executable
    ;       java_archive.

:- inst c_or_csharp_linked_target_type for linked_target_type/0
    --->    executable
    ;       static_library
    ;       shared_library
    ;       csharp_executable
    ;       csharp_library.

:- inst c_or_csharp_exe_or_lib for linked_target_type/0
    --->    executable
    ;       shared_library
    ;       csharp_executable
    ;       csharp_library.

%---------------------------------------------------------------------------%

% WARNING: The code here duplicates the functionality of scripts/mgnuc.in.
% Any changes there may also require changes here, and vice versa.

compile_c_file(Globals, ProgressStream, PIC, ModuleName, Succeeded, !IO) :-
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c), ModuleName,
        C_File, _C_FileProposed, !IO),
    maybe_pic_object_file_extension(PIC, ExtObj, _),
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs_gas(ExtObj), ModuleName,
        O_File, _O_FileProposed, !IO),
    do_compile_c_file(Globals, ProgressStream, PIC, C_File, O_File,
        Succeeded, !IO).

do_compile_c_file(Globals, ProgressStream, PIC, C_File, O_File,
        Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_string_option(Globals, c_flag_to_name_object_file,
        NameObjectFile),
    (
        Verbose = no
    ;
        Verbose = yes,
        io.format(ProgressStream, "%% Compiling `%s':\n", [s(C_File)], !IO)
    ),
    globals.lookup_string_option(Globals, cc, CC),
    gather_c_compiler_flags(Globals, PIC, AllCFlags),
    string.append_list([
        CC, " ",
        AllCFlags,
        " -c ", quote_shell_cmd_arg(C_File), " ",
        NameObjectFile, quote_shell_cmd_arg(O_File)], Command),
    get_maybe_filtercc_command(Globals, MaybeFilterCmd),
    invoke_system_command_maybe_filter_output(Globals,
        ProgressStream, ProgressStream, cmd_verbose_commands,
        Command, MaybeFilterCmd, Succeeded, !IO),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred gather_c_compiler_flags(globals::in, pic::in, string::out) is det.

gather_c_compiler_flags(Globals, PIC, AllCFlags) :-
    globals.lookup_accumulating_option(Globals, cflags, C_Flags_List),
    join_string_list(C_Flags_List, "", "", " ", CFLAGS),
    gather_compiler_specific_flags(Globals, CC_Specific_CFLAGS),

    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        SubdirSetting = use_cur_dir,
        SubDirInclOpt = ""
    ;
        ( SubdirSetting = use_cur_ngs_subdir
        ; SubdirSetting = use_cur_ngs_gs_subdir
        ),
        % The source file (foo.c) will be compiled in a subdirectory
        % (either Mercury/cs, foo.dir, or Mercury/dirs/foo.dir, depending
        % on which of these two options is set) so we need to add `-I.'
        % so it can include header files in the source directory.
        SubDirInclOpt = "-I. "
    ),

    gather_c_include_dir_flags(Globals, InclOpt),
    get_framework_directories(Globals, FrameworkInclOpt),
    gather_c_grade_defines(Globals, GradeDefinesOpts),

    globals.lookup_bool_option(Globals, gcc_global_registers, GCC_Regs),
    (
        GCC_Regs = yes,
        globals.lookup_string_option(Globals, cflags_for_regs,
            CFLAGS_FOR_REGS)
    ;
        GCC_Regs = no,
        CFLAGS_FOR_REGS = ""
    ),
    globals.lookup_bool_option(Globals, gcc_non_local_gotos, GCC_Gotos),
    (
        GCC_Gotos = yes,
        globals.lookup_string_option(Globals, cflags_for_gotos,
            CFLAGS_FOR_GOTOS)
    ;
        GCC_Gotos = no,
        CFLAGS_FOR_GOTOS = ""
    ),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    (
        Parallel = yes,
        globals.lookup_string_option(Globals, cflags_for_threads,
            CFLAGS_FOR_THREADS)
    ;
        Parallel = no,
        CFLAGS_FOR_THREADS = ""
    ),
    (
        PIC = pic,
        globals.lookup_string_option(Globals, cflags_for_pic, CFLAGS_FOR_PIC)
    ;
        PIC = non_pic,
        CFLAGS_FOR_PIC = ""
    ),
    globals.lookup_string_option(Globals, cflags_for_lto, CFLAGS_FOR_LTO),
    globals.lookup_bool_option(Globals, target_debug, Target_Debug),
    (
        Target_Debug = yes,
        globals.lookup_string_option(Globals, cflags_for_debug,
            Target_DebugOpt0),
        Target_DebugOpt = Target_DebugOpt0 ++ " "
    ;
        Target_Debug = no,
        Target_DebugOpt = ""
    ),
    globals.lookup_string_option(Globals, cflags_for_sanitizers,
        SanitizerOpts),
    globals.get_c_compiler_type(Globals, C_CompilerType),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    (
        UseTrail = yes,
        % With tagged trail entries function trailing will not work unless the
        % C functions stored on the trail are aligned on word boundaries (or a
        % multiple thereof). The assemblers on some systems, and some gcc or
        % clang optimisation settings, do not align functions, so we need to
        % explicitly pass -falign-functions in trailing grades to ensure that
        % C functions are appropriately aligned.
        %
        % Note that this will also affect the untagged version of the trail,
        % but that shouldn't matter.
        (
            ( C_CompilerType = cc_gcc(_, _, _)
            ; C_CompilerType = cc_clang(_)
            ),
            globals.lookup_int_option(Globals, bytes_per_word, BytesPerWord),
            C_FnAlignOpt = string.format("-falign-functions=%d ",
                [i(BytesPerWord)])
        ;
            % XXX Check whether we need to do anything for these C compilers?
            ( C_CompilerType = cc_cl_x86(_)
            ; C_CompilerType = cc_cl_x64(_)
            ),
            C_FnAlignOpt = ""
        ;
            C_CompilerType = cc_unknown,
            C_FnAlignOpt = ""
        )
    ;
        UseTrail = no,
        C_FnAlignOpt = ""
    ),
    globals.lookup_bool_option(Globals, type_layout, TypeLayoutOption),
    (
        TypeLayoutOption = no,
        TypeLayoutOpt = "-DMR_NO_TYPE_LAYOUT "
    ;
        TypeLayoutOption = yes,
        TypeLayoutOpt = ""
    ),
    globals.get_opt_tuple(Globals, OptTuple),
    OptimizeC = OptTuple ^ ot_opt_c,
    (
        OptimizeC = opt_c,
        globals.lookup_string_option(Globals, cflags_for_optimization,
            OptimizeOpt)
    ;
        OptimizeC = do_not_opt_c,
        OptimizeOpt = ""
    ),
    InlineAlloc = OptTuple ^ ot_inline_alloc,
    (
        InlineAlloc = inline_alloc,
        % XXX disabled because inline allocation is broken in gc7.0 alpha6.
        % InlineAllocOpt = "-DMR_INLINE_ALLOC "
        InlineAllocOpt = ""
    ;
        InlineAlloc = do_not_inline_alloc,
        InlineAllocOpt = ""
    ),
    globals.lookup_bool_option(Globals, warn_target_code, Warn),
    (
        Warn = yes,
        globals.lookup_string_option(Globals, cflags_for_warnings, WarningOpt)
    ;
        Warn = no,
        WarningOpt = ""
    ),

    % Last resort workarounds for C compiler bugs.
    % Changes here need to be reflected in scripts/mgnuc.in.
    globals.lookup_bool_option(Globals, exec_trace, ExecTrace),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    ( if
        % We need to disable C compiler optimizations in debugging grades
        % in either of the two situations described below.
        ExecTrace = yes,
        (
            % 1. On Apple Darwin systems there are performance problems with
            % GCC that cause it to compile the C files generated in debugging
            % grades very slowly at -O1 or greater.
            %
            % XXX we are also enabling this for clang; does it have the
            % same performance problems?
            arch_is_apple_darwin(TargetArch)
        ;
            % 2. There is a bug in GCC 9.[12] that results in an internal error
            % in the LRA pass when compiling generated C files in debugging
            % grades that also use global registers on x86_64 machines.
            % See: <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=91430>
            GCC_Regs = yes,
            C_CompilerType = cc_gcc(yes(9), _, _),
            string.prefix(TargetArch, "x86_64")
        )
    then
        OverrideOpts = "-O0"
    else
        OverrideOpts = ""
    ),

    % Be careful with the order here! Some options override others,
    % e.g. CFLAGS_FOR_REGS must come after OptimizeOpt so that
    % it can override -fomit-frame-pointer with -fno-omit-frame-pointer.
    % Also be careful that each option is separated by spaces.
    %
    % In general, user supplied C compiler flags, i.e. CFLAGS and
    % CC_Specific_CFLAGS below, should be able to override those introduced by
    % the Mercury compiler.
    % In some circumstances we want to prevent the user doing this, typically
    % where we know the behaviour of a particular C compiler is buggy.
    % The last option, OverrideOpts, does this, and because of this,
    % it must be listed after CFLAGS and CC_Specific_CFLAGS.
    string.append_list([
        SubDirInclOpt, InclOpt, " ",
        FrameworkInclOpt, " ",
        OptimizeOpt, " ",
        GradeDefinesOpts,
        CFLAGS_FOR_REGS, " ", CFLAGS_FOR_GOTOS, " ",
        CFLAGS_FOR_THREADS, " ", CFLAGS_FOR_PIC, " ",
        CFLAGS_FOR_LTO, " ",
        Target_DebugOpt,
        SanitizerOpts, " ",
        TypeLayoutOpt,
        InlineAllocOpt,
        C_FnAlignOpt,
        WarningOpt, " ",
        CFLAGS, " ",
        CC_Specific_CFLAGS, " ",
        OverrideOpts], AllCFlags).

%---------------------------------------------------------------------------%

:- pred gather_c_grade_defines(globals::in, string::out) is det.

gather_c_grade_defines(Globals, GradeDefines) :-
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = yes,
        HighLevelCodeOpt = "-DMR_HIGHLEVEL_CODE "
    ;
        HighLevelCode = no,
        HighLevelCodeOpt = ""
    ),
    globals.lookup_bool_option(Globals, gcc_global_registers, GCC_Regs),
    (
        GCC_Regs = yes,
        RegOpt = "-DMR_USE_GCC_GLOBAL_REGISTERS "
    ;
        GCC_Regs = no,
        RegOpt = ""
    ),
    globals.lookup_bool_option(Globals, gcc_non_local_gotos, GCC_Gotos),
    (
        GCC_Gotos = yes,
        GotoOpt = "-DMR_USE_GCC_NONLOCAL_GOTOS "
    ;
        GCC_Gotos = no,
        GotoOpt = ""
    ),
    globals.lookup_bool_option(Globals, asm_labels, ASM_Labels),
    (
        ASM_Labels = yes,
        AsmOpt = "-DMR_USE_ASM_LABELS "
    ;
        ASM_Labels = no,
        AsmOpt = ""
    ),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    (
        Parallel = yes,
        ParallelOpt = "-DMR_THREAD_SAFE "
    ;
        Parallel = no,
        ParallelOpt = ""
    ),
    globals.lookup_bool_option(Globals, threadscope, Threadscope),
    (
        Threadscope = yes,
        ThreadscopeOpt = "-DMR_THREADSCOPE "
    ;
        Threadscope = no,
        ThreadscopeOpt = ""
    ),
    globals.get_gc_method(Globals, GC_Method),
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
        GC_Opt = BoehmGC_Opt ++
            "-DMR_BOEHM_GC_DEBUG -DGC_DEBUG -DKEEP_BACKPTRS "
    ;
        GC_Method = gc_hgc,
        GC_Opt = "-DMR_CONSERVATIVE_GC -DMR_HGC "
    ;
        GC_Method = gc_accurate,
        GC_Opt = "-DMR_NATIVE_GC "
    ),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    (
        ProfileCalls = yes,
        ProfileCallsOpt = "-DMR_MPROF_PROFILE_CALLS "
    ;
        ProfileCalls = no,
        ProfileCallsOpt = ""
    ),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    (
        ProfileTime = yes,
        ProfileTimeOpt = "-DMR_MPROF_PROFILE_TIME "
    ;
        ProfileTime = no,
        ProfileTimeOpt = ""
    ),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    (
        ProfileMemory = yes,
        ProfileMemoryOpt = "-DMR_MPROF_PROFILE_MEMORY "
    ;
        ProfileMemory = no,
        ProfileMemoryOpt = ""
    ),
    globals.lookup_bool_option(Globals, profile_deep, ProfileDeep),
    (
        ProfileDeep = yes,
        ProfileDeepOpt = "-DMR_DEEP_PROFILING "
    ;
        ProfileDeep = no,
        ProfileDeepOpt = ""
    ),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words,
        RecordTermSizesAsWords),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
        RecordTermSizesAsCells),
    (
        RecordTermSizesAsWords = yes,
        RecordTermSizesAsCells = yes,
        % This should have been caught in handle_options.
        unexpected($pred, "inconsistent record term size options")
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

    globals.lookup_int_option(Globals, num_ptag_bits, NumPtagBits),
    string.int_to_string(NumPtagBits, NumPtagBitsString),
    NumPtagBitsOpt = "-DMR_TAGBITS=" ++ NumPtagBitsString ++ " ",
    globals.lookup_bool_option(Globals, decl_debug, DeclDebug),
    (
        DeclDebug = yes,
        DeclDebugOpt = "-DMR_DECL_DEBUG "
    ;
        DeclDebug = no,
        DeclDebugOpt = ""
    ),
    globals.lookup_bool_option(Globals, source_to_source_debug, SourceDebug),
    (
        SourceDebug = yes,
        SourceDebugOpt = "-DMR_SS_DEBUG "
    ;
        SourceDebug = no,
        SourceDebugOpt = ""
    ),
    globals.lookup_bool_option(Globals, exec_trace, ExecTrace),
    (
        ExecTrace = yes,
        ExecTraceOpt = "-DMR_EXEC_TRACE "
    ;
        ExecTrace = no,
        ExecTraceOpt = ""
    ),
    globals.lookup_bool_option(Globals, extend_stacks_when_needed, Extend),
    globals.lookup_bool_option(Globals, stack_segments, StackSegments),
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
        % This should have been caught in handle_options,
        % but there is no code there to do so.
        % XXX Should we delete --extend-stacks-when-needed?
        % options.m has been listing it as "experimental" since 2007.
        ExtendOpt = unexpected($pred,
            "--extend-stacks-when-needed and --stack-segments")
    ),
    globals.lookup_bool_option(Globals, c_debug_grade, CDebugGrade),
    (
        CDebugGrade = yes,
        % This grade option tells the C compiler to turn on the generation
        % of debugging symbols and to disable the optimizations that
        % would make the executable harder to debug in a C debugger
        % such as gdb. However, here we gather only *macro* definitions,
        % not general compiler flags.
        CDebugGradeOpt = "-DMR_C_DEBUG_GRADE "
    ;
        CDebugGrade = no,
        CDebugGradeOpt = ""
    ),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    (
        UseTrail = yes,
        UseTrailOpt = "-DMR_USE_TRAIL "
    ;
        UseTrail = no,
        UseTrailOpt = ""
    ),
    globals.lookup_bool_option(Globals, use_minimal_model_stack_copy,
        MinimalModelStackCopy),
    globals.lookup_bool_option(Globals, use_minimal_model_own_stacks,
        MinimalModelOwnStacks),
    (
        MinimalModelStackCopy = yes,
        MinimalModelOwnStacks = yes,
        % This should have been caught in handle_options.
        unexpected($pred, "inconsistent minimal model options")
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
    globals.lookup_bool_option(Globals, minimal_model_debug,
        MinimalModelDebug),
    (
        MinimalModelDebug = yes,
        ( if MinimalModelBaseOpt = "" then
            % We ignore the debug flag unless one of the base flags is set.
            MinimalModelOpt = MinimalModelBaseOpt
        else
            MinimalModelOpt = MinimalModelBaseOpt ++
                "-DMR_MINIMAL_MODEL_DEBUG "
        )
    ;
        MinimalModelDebug = no,
        MinimalModelOpt = MinimalModelBaseOpt
    ),

    globals.lookup_bool_option(Globals, pregenerated_dist, PregeneratedDist),
    (
        PregeneratedDist = yes,
        PregeneratedDistOpt = "-DMR_PREGENERATED_DIST "
    ;
        PregeneratedDist = no,
        PregeneratedDistOpt = ""
    ),
    globals.lookup_bool_option(Globals, single_prec_float, SinglePrecFloat),
    (
        SinglePrecFloat = yes,
        SinglePrecFloatOpt = "-DMR_USE_SINGLE_PREC_FLOAT "
    ;
        SinglePrecFloat = no,
        SinglePrecFloatOpt = ""
    ),

    globals.lookup_bool_option(Globals, use_regions, UseRegions),
    (
        UseRegions = yes,
        UseRegionsOpt0 = "-DMR_USE_REGIONS ",
        globals.lookup_bool_option(Globals, use_regions_debug,
            UseRegionsDebug),
        (
            UseRegionsDebug = yes,
            UseRegionsOpt1 = UseRegionsOpt0 ++ "-DMR_RBMM_DEBUG "
        ;
            UseRegionsDebug = no,
            UseRegionsOpt1 = UseRegionsOpt0
        ),
        globals.lookup_bool_option(Globals, use_regions_profiling,
            UseRegionsProfiling),
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
    string.append_list([
        HighLevelCodeOpt,
        RegOpt, GotoOpt, AsmOpt,
        ParallelOpt,
        ThreadscopeOpt,
        GC_Opt,
        ProfileCallsOpt, ProfileTimeOpt, ProfileMemoryOpt, ProfileDeepOpt,
        RecordTermSizesOpt,
        NumPtagBitsOpt,
        ExtendOpt,
        CDebugGradeOpt, DeclDebugOpt,
        SourceDebugOpt,
        ExecTraceOpt,
        UseTrailOpt,
        MinimalModelOpt,
        PregeneratedDistOpt,
        SinglePrecFloatOpt,
        UseRegionsOpt], GradeDefines).

%---------------------------------------------------------------------------%

:- pred gather_c_include_dir_flags(globals::in, string::out) is det.

gather_c_include_dir_flags(Globals, InclOpt) :-
    globals.lookup_accumulating_option(Globals, c_include_directories,
        C_Incl_Dirs),
    InclOpt = string.append_list(list.condense(list.map(
        (func(C_INCL) = ["-I", quote_shell_cmd_arg(C_INCL), " "]),
        C_Incl_Dirs))).

%---------------------------------------------------------------------------%

:- pred gather_compiler_specific_flags(globals::in, string::out) is det.

gather_compiler_specific_flags(Globals, Flags) :-
    globals.get_c_compiler_type(Globals, C_CompilerType),
    (
        C_CompilerType = cc_gcc(_, _, _),
        globals.lookup_accumulating_option(Globals, gcc_flags, FlagsList)
    ;
        C_CompilerType = cc_clang(_),
        globals.lookup_accumulating_option(Globals, clang_flags, FlagsList)
    ;
        ( C_CompilerType = cc_cl_x86(_)
        ; C_CompilerType = cc_cl_x64(_)
        ),
        globals.lookup_accumulating_option(Globals, msvc_flags, FlagsList)
    ;
        C_CompilerType = cc_unknown,
        FlagsList = []
    ),
    join_string_list(FlagsList, "", "", " ", Flags).

:- pred get_maybe_filtercc_command(globals::in, maybe(string)::out) is det.

get_maybe_filtercc_command(Globals, MaybeFilterCmd) :-
    % At this time we only need to filter the compiler output when using
    % assembler labels with gcc 4.x. Mercury.config.bootstrap doesn't specify
    % the gcc version, so we don't check for it.
    ( if
        globals.lookup_bool_option(Globals, asm_labels, yes),
        globals.lookup_string_option(Globals, filtercc_command, FilterCmd),
        FilterCmd \= ""
    then
        MaybeFilterCmd = yes(FilterCmd)
    else
        MaybeFilterCmd = no
    ).

%---------------------------------------------------------------------------%

compile_java_files(Globals, ProgressStream, HeadJavaFile, TailJavaFiles,
        Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Verbose = yes,
        (
            TailJavaFiles = [],
            io.format(ProgressStream, "%% Compiling `%s':\n",
                [s(HeadJavaFile)], !IO)
        ;
            TailJavaFiles = [_ | _],
            io.format(ProgressStream, "%% Compiling `%s', etc.:\n",
                [s(HeadJavaFile)], !IO)
        )
    ;
        Verbose = no
    ),

    globals.lookup_string_option(Globals, java_compiler, JavaCompiler),
    globals.lookup_accumulating_option(Globals, java_compiler_flags,
        JavaFlagsList),
    globals.lookup_bool_option(Globals, restricted_command_line,
        RestrictedCommandLine),
    (
        RestrictedCommandLine = yes,
        % NOTE: the '-J' flag must not occur inside @files, so we need
        % to ensure that it is always passed on the command line.
        list.filter(is_minus_j_flag, JavaFlagsList,
            JRT_JavaFlagsList, NonJRT_JavaFlagsList),
        join_string_list(JRT_JavaFlagsList, "", "", " ",
            NonAtFileJAVAFLAGS),
        join_string_list(NonJRT_JavaFlagsList, "", "", " ",
            JAVAFLAGS)
    ;
        RestrictedCommandLine = no,
        join_string_list(JavaFlagsList, "", "", " ", JAVAFLAGS),
        NonAtFileJAVAFLAGS = ""
    ),

    get_mercury_std_libs_for_java(Globals, MercuryStdLibs),
    globals.lookup_accumulating_option(Globals, java_classpath, UserClasspath),
    Java_Incl_Dirs = MercuryStdLibs ++ UserClasspath,
    % We prepend the current CLASSPATH (if any) to preserve the accumulating
    % nature of this variable.
    get_env_classpath(EnvClasspath, !IO),
    ( if EnvClasspath = "" then
        ClassPathList = Java_Incl_Dirs
    else
        ClassPathList = [EnvClasspath | Java_Incl_Dirs]
    ),
    ClassPath = string.join_list(java_classpath_separator, ClassPathList),
    ( if ClassPath = "" then
        InclOpt = ""
    else
        InclOpt = string.append_list([
            "-classpath ", quote_shell_cmd_arg(ClassPath), " "])
    ),

    globals.lookup_bool_option(Globals, target_debug, TargetDebug),
    globals.lookup_bool_option(Globals, c_debug_grade, CDebugGrade),
    ( if ( TargetDebug = yes ; CDebugGrade = yes ) then
        TargetDebugOpt = "-g "
    else
        TargetDebugOpt = ""
    ),

    % XXX LEGACY
    get_java_dir_path(Globals, ext_cur_ngs_gs_java_java,
        SourceDirPath, _SourceDirPathProposed),
    get_java_dir_path(Globals, ext_cur_ngs_gs_java_class,
        DestDirPath, _DestDirPathProposed),
    (
        SourceDirPath = [],
        expect(unify(DestDirPath, []), $pred, "DestDirPath != []"),
        DirOpts = ""
    ;
        SourceDirPath = [_ | _],
        expect_not(unify(DestDirPath, []), $pred, "DestDirPath == []"),
        SourceDirName = dir.relative_path_name_from_components(SourceDirPath),
        DestDirName = dir.relative_path_name_from_components(DestDirPath),
        % Javac won't create the destination directory for class files,
        % so we need to do it.
        dir.make_directory(DestDirName, _, !IO),
        % Set directories for source and class files.
        DirOpts = string.append_list([
            "-sourcepath ", SourceDirName, " ",
            "-d ", DestDirName, " "
        ])
    ),

    globals.lookup_string_option(Globals, filterjavac_command, MFilterJavac),
    ( if MFilterJavac = "" then
        MaybeMFilterJavac = no
    else
        MaybeMFilterJavac = yes(MFilterJavac)
    ),

    NonAtFileCommandArgs = NonAtFileJAVAFLAGS,
    % Be careful with the order here! Some options may override others.
    % Also be careful that each option is separated by spaces.
    JoinedJavaFiles = string.join_list(" ", [HeadJavaFile | TailJavaFiles]),
    string.append_list([InclOpt, DirOpts,
        TargetDebugOpt, JAVAFLAGS, " ", JoinedJavaFiles], CommandArgs),
    invoke_long_system_command_maybe_filter_output(Globals,
        ProgressStream, ProgressStream, cmd_verbose_commands,
        JavaCompiler, NonAtFileCommandArgs, CommandArgs, MaybeMFilterJavac,
        Succeeded, !IO).

:- func java_classpath_separator = string.

java_classpath_separator = PathSeparator :-
    ( if
        ( dir.use_windows_paths
        ; io.have_cygwin
        )
    then
        PathSeparator = ";"
    else
        PathSeparator = ":"
    ).

:- pred is_minus_j_flag(string::in) is semidet.

is_minus_j_flag(FlagStr) :-
    string.prefix(FlagStr, "-J").

%---------------------------------------------------------------------------%

compile_csharp_file(Globals, ProgressStream, ModuleDepInfo,
        CSharpFileName0, DLLFileName, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Verbose = no
    ;
        Verbose = yes,
        io.format(ProgressStream, "%% Compiling `%s':\n",
            [s(CSharpFileName)], !IO)
    ),
    globals.lookup_string_option(Globals, csharp_compiler, CSC),
    globals.lookup_accumulating_option(Globals, csharp_flags, CSCFlagsList),
    join_string_list(CSCFlagsList, "", "", " ", CSCFlags),

    % XXX This is because the MS C# compiler doesn't understand
    % / as a directory separator.
    CSharpFileName = string.replace_all(CSharpFileName0, "/", "\\\\"),

    globals.lookup_bool_option(Globals, target_debug, Debug),
    (
        Debug = yes,
        % XXX This needs testing before it can be enabled (see the comments
        % for install_debug_library in library/Mmakefile).

        % DebugOpt = "-debug+ -debug:full "
        DebugOpt = ""
    ;
        Debug = no,
        DebugOpt = ""
    ),

    % XXX Should we use a separate dll_directories options?
    % NOTE: we use the -option style options in preference to the /option
    % style in order to avoid problems with POSIX style shells.
    globals.lookup_accumulating_option(Globals, link_library_directories,
        DLLDirs),
    DLLDirOpts = "-lib:Mercury/dlls " ++
        string.append_list(list.condense(list.map(
            (func(DLLDir) = ["-lib:", DLLDir, " "]), DLLDirs))),

    module_dep_info_get_module_name(ModuleDepInfo, ModuleName),
    ( if mercury_std_library_module_name(ModuleName) then
        Prefix = "-addmodule:"
    else
        Prefix = "-r:"
    ),
    module_dep_info_get_fims(ModuleDepInfo, FIMSpes),
    list.filter_map(
        ( pred(FS::in, M::out) is semidet :-
            FS = fim_spec(lang_csharp, _),
            M = fim_spec_module_name_from_module(FS, ModuleName)
        ), set.to_sorted_list(FIMSpes), ForeignDeps),
    module_dep_info_get_int_deps(ModuleDepInfo, IntDeps),
    module_dep_info_get_imp_deps(ModuleDepInfo, ImpDeps),
    set.union(IntDeps, ImpDeps, IntImpDeps),
    set.insert_list(ForeignDeps, IntImpDeps, IntImpForeignDeps),
    ReferencedDlls = referenced_dlls(ModuleName, IntImpForeignDeps),
    list.map(
        ( pred(Mod::in, Result::out) is det :-
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred,
                ext_cur_gs(ext_cur_gs_lib_cil_dll), Mod,
                FileName, _FileNameProposed),
            Result = [Prefix, FileName, " "]
        ), set.to_sorted_list(ReferencedDlls), ReferencedDllsList),
    ReferencedDllsStr = string.append_list(
        list.condense(ReferencedDllsList)),

    string.append_list([CSC, DebugOpt,
        " -t:library ", DLLDirOpts, CSCFlags, ReferencedDllsStr,
        " -out:", DLLFileName, " ", CSharpFileName], Command),
    invoke_system_command(Globals, ProgressStream, ProgressStream,
        cmd_verbose_commands, Command, Succeeded, !IO).

    % Generate the list of .NET DLLs which could be referred to by this module
    % (including the module itself).
    %
    % If we are compiling a module within the standard library we should
    % reference the runtime DLLs and all other library DLLs. If we are
    % outside the library we should just reference mercury.dll (which will
    % contain all the DLLs).
    %
:- func referenced_dlls(module_name, set(module_name)) = set(module_name).

referenced_dlls(Module, DepModules0) = Modules :-
    set.insert(Module, DepModules0, DepModules),

    % If we are not compiling a module in the mercury std library, then
    % replace all the std library dlls with one reference to mercury.dll.
    ( if mercury_std_library_module_name(Module) then
        % In the standard library we need to add the runtime dlls.
        AddedModules =
            [unqualified("mercury_dotnet"), unqualified("mercury_il")],
        set.insert_list(AddedModules, DepModules, Modules)
    else
        F = ( func(M) =
                ( if mercury_std_library_module_name(M) then
                    unqualified("mercury")
                else
                    % A sub module is located in the top level assembly.
                    unqualified(outermost_qualifier(M))
                )
            ),
        Modules = set.map(F, DepModules)
    ).

%---------------------------------------------------------------------------%

make_library_init_file(Globals, ProgressStream, MainModuleName, AllModules,
        Succeeded, !IO) :-
    globals.lookup_string_option(Globals, mkinit_command, MkInit),
    % XXX LEGACY
    module_name_to_file_name_full_curdir_create_dirs(Globals, $pred,
        ext_cur_gs(ext_cur_gs_lib_init), MainModuleName,
        FullInitFileName, _FullInitFileNameProposed, CurDirInitFileName, !IO),
    TmpFullInitFileName = FullInitFileName ++ ".tmp",
    io.open_output(TmpFullInitFileName, TmpInitFileOpenResult, !IO),
    (
        TmpInitFileOpenResult = ok(TmpInitFileStream),
        list.map2(
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred,
                ext_cur_ngs_gs(ext_cur_ngs_gs_target_c)),
                AllModules, AllTargetCFilesList, _AllTargetCFilesListProposed),
        invoke_mkinit(Globals, ProgressStream, TmpInitFileStream,
            cmd_verbose_commands, MkInit, " -k ", AllTargetCFilesList,
            TmpMkInitSucceeded0, !IO),
        (
            TmpMkInitSucceeded0 = succeeded,
            globals.lookup_maybe_string_option(Globals, extra_init_command,
                MaybeInitFileCommand),
            (
                MaybeInitFileCommand = yes(InitFileCommand),
                make_all_module_command(InitFileCommand,
                    MainModuleName, AllModules, CommandStr, !IO),
                invoke_system_command(Globals, ProgressStream,
                    TmpInitFileStream, cmd_verbose_commands, CommandStr,
                    TmpMkInitSucceeded, !IO)
            ;
                MaybeInitFileCommand = no,
                TmpMkInitSucceeded = succeeded
            )
        ;
            TmpMkInitSucceeded0 = did_not_succeed,
            TmpMkInitSucceeded = did_not_succeed
        ),

        io.close_output(TmpInitFileStream, !IO),
        copy_dot_tmp_to_base_file_return_succeeded(ProgressStream, Globals,
            FullInitFileName, CopyTmpSucceeded, !IO),
        MkInitSucceded = TmpMkInitSucceeded `and` CopyTmpSucceeded,
        (
            MkInitSucceded = succeeded,
            % Symlink or copy the .init files to the user's directory
            % if --use-grade-subdirs is enabled.
            ( if FullInitFileName = CurDirInitFileName then
                Succeeded = succeeded
            else
                % Remove the target of the symlink/copy in case it already
                % exists.
                io.file.remove_file(CurDirInitFileName, _, !IO),
                make_symlink_or_copy_file(Globals, ProgressStream,
                    FullInitFileName, CurDirInitFileName, Succeeded, !IO)
            )
        ;
            MkInitSucceded = did_not_succeed,
            Succeeded  = did_not_succeed
        )
    ;
        TmpInitFileOpenResult = error(Error),
        io.progname_base("mercury_compile", ProgName, !IO),
        ErrorMsg = io.error_message(Error),
        io.format(ProgressStream, "%s: can't open `%s' for output: %s\n",
            [s(ProgName), s(TmpFullInitFileName), s(ErrorMsg)], !IO),
        Succeeded = did_not_succeed
    ).

:- pred invoke_mkinit(globals::in, io.text_output_stream::in,
    io.text_output_stream::in, command_verbosity::in,
    string::in, string::in, list(file_name)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

invoke_mkinit(Globals, ProgressStream, InitFileStream, Verbosity,
        MkInit, Args, FileNames, MkInitSucceeded, !IO) :-
    % mkinit expects unquoted file names.
    join_string_list(FileNames, "", "\n", "", TargetFileNames),

    file_util.open_temp_output(TmpFileResult, !IO),
    (
        TmpFileResult = ok({TmpFile, TmpStream}),
        io.write_string(TmpStream, TargetFileNames, !IO),
        io.close_output(TmpStream, !IO),

        string.format("%s %s -f %s", [s(MkInit), s(Args), s(TmpFile)],
            MkInitCmd),
        invoke_system_command(Globals, ProgressStream, InitFileStream,
            Verbosity, MkInitCmd, MkInitSucceeded0, !IO),

        io.file.remove_file(TmpFile, RemoveResult, !IO),
        (
            RemoveResult = ok,
            MkInitSucceeded = MkInitSucceeded0
        ;
            RemoveResult = error(_),
            MkInitSucceeded = did_not_succeed
        )
    ;
        TmpFileResult = error(ErrorMessage),
        io.format(ProgressStream, "%s\n", [s(ErrorMessage)], !IO),
        MkInitSucceeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

make_init_obj_file(Globals, ProgressStream, ModuleName, ModuleNames,
        Result, !IO) :-
    globals.lookup_bool_option(Globals, rebuild, MustCompile),
    do_make_init_obj_file(Globals, ProgressStream,
        MustCompile, ModuleName, ModuleNames, Result, !IO).

% WARNING: The code here duplicates the functionality of scripts/c2init.in.
% Any changes there may also require changes here, and vice versa.
% The code of make_standalone_interface/5 may also require updating.

:- pred do_make_init_obj_file(globals::in, io.text_output_stream::in, bool::in,
    module_name::in, list(module_name)::in, maybe(file_name)::out,
    io::di, io::uo) is det.

do_make_init_obj_file(Globals, ProgressStream, MustCompile,
        ModuleName, ModuleNames, Result, !IO) :-
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    globals.get_grade_dir(Globals, GradeDir),
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

    globals.lookup_string_option(Globals, mkinit_command, MkInit),
    make_init_target_file(Globals, ProgressStream, MkInit,
        ModuleName, ModuleNames,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
        ext_cur_ngs_gs(ext_cur_ngs_gs_init_c),
        StdInitFileNames, StdTraceInitFileNames, SourceDebugInitFileNames,
        MaybeInitTargetFile, !IO),

    get_object_code_type(Globals, executable, PIC),
    maybe_pic_object_file_extension(PIC, _, ExtInitObj),

    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs_gas(ExtInitObj), ModuleName,
        InitObjFileName, _InitObjFileNameProposed, !IO),
    CompileCInitFile =
        ( pred(InitTargetFileName::in, Res::out, IO0::di, IO::uo) is det :-
            do_compile_c_file(Globals, ProgressStream, PIC,
                InitTargetFileName, InitObjFileName, Res, IO0, IO)
        ),
    maybe_compile_init_obj_file(Globals, ProgressStream, MaybeInitTargetFile,
        MustCompile, CompileCInitFile, InitObjFileName, Result, !IO).

:- pred make_init_target_file(globals::in, io.text_output_stream::in,
    string::in, module_name::in, list(module_name)::in, ext::in, ext::in,
    list(file_name)::in, list(file_name)::in, list(file_name)::in,
    maybe(file_name)::out, io::di, io::uo) is det.

make_init_target_file(Globals, ProgressStream, MkInit,
        ModuleName, ModuleNames, TargetOtherExt, InitTargetOtherExt,
        StdInitFileNames, StdTraceInitFileNames, SourceDebugInitFileNames,
        MaybeInitTargetFile, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_write_string(ProgressStream, Verbose,
        "% Creating initialization file...\n", !IO),

    compute_grade(Globals, Grade),

    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred, InitTargetOtherExt,
        ModuleName, InitTargetFileName, _InitTargetFileNameProposed, !IO),

    list.map2(
        % XXX LEGACY
        module_name_to_file_name(Globals, $pred, TargetOtherExt),
        ModuleNames, TargetFileNameList, _TargetFileNameListProposed),

    globals.lookup_accumulating_option(Globals, init_file_directories,
        InitFileDirsList),
    join_quoted_string_list(InitFileDirsList, "-I ", "", " ", InitFileDirs),

    globals.lookup_accumulating_option(Globals, init_files,
        InitFileNamesList0),
    % If we pass the same .init file to mkinit multiple times, we will
    % end up with multiple calls to the functions that write out proc statics
    % in deep profiling grades. This will cause the runtime code that
    % writes out the profile to abort.
    list.remove_dups(InitFileNamesList0, InitFileNamesList1),

    globals.lookup_accumulating_option(Globals, trace_init_files,
        TraceInitFileNamesList0),
    InitFileNamesList2 = StdInitFileNames ++ InitFileNamesList1,
    TraceInitFileNamesList = StdTraceInitFileNames ++ TraceInitFileNamesList0,

    globals.get_trace_level(Globals, TraceLevel),
    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    (
        TraceEnabled = exec_trace_is_enabled,
        TraceOpt = "-t",
        InitFileNamesList3 = InitFileNamesList2 ++ TraceInitFileNamesList
    ;
        TraceEnabled = exec_trace_is_not_enabled,
        TraceOpt = "",
        InitFileNamesList3 = InitFileNamesList2
    ),

    globals.lookup_bool_option(Globals, link_ssdb_libs, SourceDebug),
    (
        SourceDebug = yes,
        InitFileNamesList = InitFileNamesList3 ++ SourceDebugInitFileNames
    ;
        SourceDebug = no,
        InitFileNamesList = InitFileNamesList3
    ),

    globals.lookup_accumulating_option(Globals, runtime_flags,
        RuntimeFlagsList),
    join_quoted_string_list(RuntimeFlagsList, "-r ", "", " ", RuntimeFlags),

    globals.lookup_bool_option(Globals, extra_initialization_functions,
        ExtraInits),
    (
        ExtraInits = yes,
        ExtraInitsOpt = "-x"
    ;
        ExtraInits = no,
        ExtraInitsOpt = ""
    ),

    globals.lookup_bool_option(Globals, main, Main),
    (
        Main = no,
        NoMainOpt = "-l"
    ;
        Main = yes,
        NoMainOpt = ""
    ),

    globals.lookup_string_option(Globals, experimental_complexity,
        ExperimentalComplexity),
    ( if ExperimentalComplexity = "" then
        ExperimentalComplexityOpt = ""
    else
        ExperimentalComplexityOpt = "-X " ++ ExperimentalComplexity
    ),

    TmpInitTargetFileName = InitTargetFileName ++ ".tmp",
    MkInitArgs = string.append_list([
        " -g ", Grade,
        " ", TraceOpt,
        " ", ExtraInitsOpt,
        " ", NoMainOpt,
        " ", ExperimentalComplexityOpt,
        " ", RuntimeFlags,
        " -o ", quote_shell_cmd_arg(TmpInitTargetFileName),
        " ", InitFileDirs
    ]),

    % XXX STREAM Setting CmdOutputStream from ErrorStream looks VERY wrong,
    % but it preserves old behavior (or at least, it attempts to preserve it).
    % XXX ErrorStream has now been replaced by ProgressStream.
    CmdOutputStream = ProgressStream,
    invoke_mkinit(Globals, ProgressStream, CmdOutputStream,
        cmd_verbose_commands, MkInit, MkInitArgs,
        TargetFileNameList ++ InitFileNamesList, MkInitSucceeded, !IO),

    maybe_report_stats(ProgressStream, Stats, !IO),
    (
        MkInitSucceeded = succeeded,
        copy_dot_tmp_to_base_file_return_succeeded(ProgressStream, Globals,
            InitTargetFileName, UpdateResult, !IO),
        (
            UpdateResult = succeeded,
            MaybeInitTargetFile = yes(InitTargetFileName)
        ;
            UpdateResult = did_not_succeed,
            MaybeInitTargetFile = no
        )
    ;
        MkInitSucceeded = did_not_succeed,
        MaybeInitTargetFile = no
    ).

:- pred maybe_compile_init_obj_file(globals::in, io.text_output_stream::in,
    maybe(file_name)::in, bool::in,
    compile_init_file_pred::in(compile_init_file_pred),
    file_name::in, maybe(file_name)::out, io::di, io::uo) is det.

:- type compile_init_file_pred == pred(file_name, maybe_succeeded, io, io).
:- inst compile_init_file_pred == (pred(in, out, di, uo) is det).

maybe_compile_init_obj_file(Globals, ProgressStream, MaybeInitTargetFile,
        MustCompile, Compile, InitObjFileName, Result, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    (
        MaybeInitTargetFile = yes(InitTargetFileName),
        file_is_as_new_as(InitObjFileName, Rel, InitTargetFileName, !IO),
        ( if
            ( MustCompile = yes
            ; Rel = is_not_as_new_as
            ; Rel = missing_timestamp
            )
        then
            maybe_write_string(ProgressStream, Verbose,
                "% Compiling initialization file...\n", !IO),
            Compile(InitTargetFileName, CompileSucceeded, !IO),
            maybe_report_stats(ProgressStream, Stats, !IO),
            (
                CompileSucceeded = succeeded,
                Result = yes(InitObjFileName)
            ;
                CompileSucceeded = did_not_succeed,
                Result = no
            )
        else
            Result = yes(InitObjFileName)
        )
    ;
        MaybeInitTargetFile = no,
        Result = no
    ).

%---------------------------------------------------------------------------%

link_module_list(ProgressStream, Modules, ExtraObjFiles, Globals,
        Specs, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, output_file_name, OutputFileName0),
    ( if OutputFileName0 = "" then
        (
            Modules = [Module | _],
            OutputFileName = Module
        ;
            Modules = [],
            unexpected($pred, "no modules")
        )
    else
        OutputFileName = OutputFileName0
    ),

    file_name_to_module_name(OutputFileName, MainModuleName),

    globals.lookup_bool_option(Globals, compile_to_shared_lib,
        CompileToSharedLib),
    (
        CompileToSharedLib = yes,
        TargetType = shared_library
    ;
        CompileToSharedLib = no,
        TargetType = executable
    ),
    get_object_code_type(Globals, TargetType, PIC),
    maybe_pic_object_file_extension(PIC, ObjExt, _),

    join_module_list(Globals, ext_cur_ngs_gas(ObjExt),
        Modules, ObjectsList, !IO),
    (
        TargetType = executable,
        list.map(
            ( pred(ModuleStr::in, ModuleName::out) is det :-
                file_name_to_module_name(dir.det_basename(ModuleStr),
                    ModuleName)
            ), Modules, ModuleNames),
        MustCompile = yes,
        do_make_init_obj_file(Globals, ProgressStream, MustCompile,
            MainModuleName, ModuleNames, InitObjResult, !IO)
    ;
        TargetType = shared_library,
        InitObjResult = yes("")
    ),
    (
        InitObjResult = yes(InitObjFileName),
        globals.lookup_accumulating_option(Globals, link_objects,
            ExtraLinkObjectsList),
        AllObjects0 = ObjectsList ++ ExtraLinkObjectsList ++ ExtraObjFiles,
        ( if InitObjFileName = "" then
            AllObjects = AllObjects0
        else
            AllObjects = [InitObjFileName | AllObjects0]
        ),
        link(Globals, ProgressStream, TargetType, MainModuleName, AllObjects,
            Specs, Succeeded, !IO)
    ;
        InitObjResult = no,
        Specs = [],
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

% WARNING: The code here duplicates the functionality of scripts/ml.in.
% Any changes there may also require changes here, and vice versa.

link(Globals, ProgressStream, LinkTargetType, ModuleName, ObjectsList,
        Specs, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(ProgressStream, Verbose, "% Linking...\n", !IO),
    linked_target_file_name_full_curdir(Globals, ModuleName, LinkTargetType,
        FullOutputFileName, CurDirOutputFileName, !IO),
    (
        LinkTargetType = executable,
        link_exe_or_shared_lib(Globals, ProgressStream,
            LinkTargetType, ModuleName, FullOutputFileName, ObjectsList,
            Specs, LinkSucceeded, !IO)
    ;
        LinkTargetType = static_library,
        create_archive(Globals, ProgressStream,
            FullOutputFileName, yes, ObjectsList, LinkSucceeded, !IO),
        Specs = []
    ;
        LinkTargetType = shared_library,
        link_exe_or_shared_lib(Globals, ProgressStream,
            LinkTargetType, ModuleName, FullOutputFileName, ObjectsList,
            Specs, LinkSucceeded, !IO)
    ;
        ( LinkTargetType = csharp_executable
        ; LinkTargetType = csharp_library
        ),
        % XXX C# see also older predicate compile_csharp_file
        create_csharp_exe_or_lib(Globals, ProgressStream,
            LinkTargetType, ModuleName, FullOutputFileName, ObjectsList,
            Specs, LinkSucceeded, !IO)
    ;
        ( LinkTargetType = java_executable
        ; LinkTargetType = java_archive
        ),
        create_java_exe_or_lib(Globals, ProgressStream,
            LinkTargetType, ModuleName, FullOutputFileName, ObjectsList,
            LinkSucceeded, !IO),
        Specs = []
    ),
    maybe_report_stats(ProgressStream, Stats, !IO),
    (
        LinkSucceeded = succeeded,
        post_link_maybe_make_symlink_or_copy(Globals, ProgressStream,
            FullOutputFileName, CurDirOutputFileName,
            ModuleName, LinkTargetType, Succeeded, _MadeSymlinkOrCopy, !IO)
    ;
        LinkSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

linked_target_file_name_full_curdir(Globals, ModuleName, TargetType,
        FullFileName, CurDirFileName, !IO) :-
    (
        % Java archives and Java executables get the same filename.
        % XXX Then why make the distinction in linked_target_type?
        (
            TargetType = executable,
            Ext = ext_cur_gas(ext_cur_gas_exec_exec_opt)
        ;
            TargetType = csharp_executable,
            Ext = ext_cur_gas(ext_cur_gas_exec_exe)
        ;
            TargetType = csharp_library,
            Ext = ext_cur_gs(ext_cur_gs_lib_cil_dll)
        ;
            TargetType = java_archive,
            Ext = ext_cur_gs(ext_cur_gs_lib_jar)
        ;
            TargetType = java_executable,
            Ext = ext_cur_gs(ext_cur_gs_lib_jar)
        ),
        % XXX LEGACY
        module_name_to_file_name_full_curdir_create_dirs(Globals, $pred,
            Ext, ModuleName, FullFileName, _FullFileNameProposed,
            CurDirFileName, !IO)
    ;
        (
            TargetType = static_library,
            Ext = ext_cur_gas(ext_cur_gas_lib_lib_opt)
        ;
            TargetType = shared_library,
            Ext = ext_cur_gas(ext_cur_gas_lib_sh_lib_opt)
        ),
        % XXX LEGACY
        module_name_to_lib_file_name_full_curdir_create_dirs(Globals, $pred,
            "lib", Ext, ModuleName, FullFileName, _FullFileNameProposed,
            CurDirFileName, !IO)
    ).

:- pred get_launcher_script_extension(globals::in, ext::out) is det.

get_launcher_script_extension(Globals, Ext) :-
    globals.get_target_env_type(Globals, TargetEnvType),
    (
        % XXX we should actually generate a .ps1 file for PowerShell.
        ( TargetEnvType = env_type_win_cmd
        ; TargetEnvType = env_type_powershell
        ),
        Ext = ext_cur_gas(ext_cur_gas_exec_bat)
    ;
        ( TargetEnvType = env_type_posix
        ; TargetEnvType = env_type_cygwin
        ; TargetEnvType = env_type_msys
        ),
        Ext = ext_cur_gas(ext_cur_gas_exec_noext)
    ).

:- pred link_exe_or_shared_lib(globals::in, io.text_output_stream::in,
    linked_target_type::in(c_exe_or_shared_lib),
    module_name::in, file_name::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

link_exe_or_shared_lib(Globals, ProgressStream, LinkTargetType,
        ModuleName, FullOutputFileName, ObjectsList, Specs, Succeeded, !IO) :-
    (
        LinkTargetType = shared_library,
        CommandOpt = link_shared_lib_command,
        RpathFlagOpt = shlib_linker_rpath_flag,
        RpathSepOpt = shlib_linker_rpath_separator,
        LDFlagsOpt = ld_libflags,
        ThreadFlagsOpt = shlib_linker_thread_flags,
        DebugFlagsOpt = shlib_linker_debug_flags,
        TraceFlagsOpt = shlib_linker_trace_flags,
        globals.lookup_bool_option(Globals, allow_undefined, AllowUndef),
        (
            AllowUndef = bool.yes,
            globals.lookup_string_option(Globals, linker_allow_undefined_flag,
                UndefOpt)
        ;
            AllowUndef = bool.no,
            globals.lookup_string_option(Globals,
                linker_error_undefined_flag, UndefOpt)
        ),
        ReserveStackSizeOpt = ""
    ;
        LinkTargetType = executable,
        CommandOpt = link_executable_command,
        RpathFlagOpt = linker_rpath_flag,
        RpathSepOpt = linker_rpath_separator,
        LDFlagsOpt = ld_flags,
        ThreadFlagsOpt = linker_thread_flags,
        DebugFlagsOpt = linker_debug_flags,
        TraceFlagsOpt = linker_trace_flags,
        UndefOpt = "",
        ReserveStackSizeOpt = reserve_stack_size_flags(Globals)
    ),

    globals.lookup_string_option(Globals, linker_lto_flags, LTOOpts),

    % Should the executable be stripped?
    globals.lookup_bool_option(Globals, strip, Strip),
    ( if
        LinkTargetType = executable,
        Strip = bool.yes
    then
        globals.lookup_string_option(Globals, linker_strip_flag,
            LinkerStripOpt),
        globals.lookup_string_option(Globals, strip_executable_command,
            StripExeCommand),
        globals.lookup_string_option(Globals, mercury_linkage, MercuryLinkage),
        ( if MercuryLinkage = "shared" then
            StripExeFlagsOpt = strip_executable_shared_flags
        else
            StripExeFlagsOpt = strip_executable_static_flags
        ),
        globals.lookup_string_option(Globals, StripExeFlagsOpt, StripExeFlags)
    else
        LinkerStripOpt = "",
        StripExeCommand = "",
        StripExeFlags = ""
    ),

    globals.lookup_bool_option(Globals, target_debug, TargetDebug),
    (
        TargetDebug = bool.yes,
        globals.lookup_string_option(Globals, DebugFlagsOpt, DebugOpts)
    ;
        TargetDebug = bool.no,
        DebugOpts = ""
    ),

    globals.lookup_string_option(Globals, linker_sanitizer_flags,
        SanitizerOpts),

    % Should the executable be statically linked?
    globals.lookup_string_option(Globals, linkage, Linkage),
    ( if
        LinkTargetType = executable,
        Linkage = "static"
    then
        globals.lookup_string_option(Globals, linker_static_flags, StaticOpts)
    else
        StaticOpts = ""
    ),

    % Are the thread libraries needed?
    use_thread_libs(Globals, UseThreadLibs),
    (
        UseThreadLibs = bool.yes,
        globals.lookup_string_option(Globals, ThreadFlagsOpt, ThreadOpts),

        % Determine which options are needed to link to libhwloc, if
        % libhwloc is not used then the string option will be empty.
        ( if Linkage = "shared" then
            HwlocFlagsOpt = hwloc_libs
        else if Linkage = "static" then
            HwlocFlagsOpt = hwloc_static_libs
        else
            unexpected($pred, "Invalid linkage")
        ),
        globals.lookup_string_option(Globals, HwlocFlagsOpt, HwlocOpts)
    ;
        UseThreadLibs = bool.no,
        ThreadOpts = "",
        HwlocOpts = ""
    ),

    % Find the Mercury standard libraries.
    get_mercury_std_libs(Globals, LinkTargetType, MercuryStdLibs),

    % Find which system libraries are needed.
    get_system_libs(Globals, LinkTargetType, SystemLibs),

    % With --restricted-command-line we may need to some additional
    % options to the linker.
    % (See the comment above get_restricted_command_lin_link_opts/3 for
    % details.)
    get_restricted_command_line_link_opts(Globals, LinkTargetType,
        ResCmdLinkOpts),

    globals.lookup_accumulating_option(Globals, LDFlagsOpt, LDFlagsList),
    join_string_list(LDFlagsList, "", "", " ", LDFlags),
    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    globals.lookup_string_option(Globals, linker_path_flag, LinkerPathFlag),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),

    % Set up the runtime library path.
    get_runtime_library_path_opts(Globals, LinkTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts),

    % Set up any framework search paths.
    get_framework_directories(Globals, FrameworkDirectories),

    % Set up the install name for shared libraries.
    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    ( if
        UseInstallName = bool.yes,
        LinkTargetType = shared_library
    then
        % NOTE: `ShLibFileName' must *not* be prefixed with a directory.
        %       get_install_name_option will prefix it with the correct
        %       directory which is the one where the library is going to
        %       be installed, *not* where it is going to be built.
        %
        BaseFileName = sym_name_to_string(ModuleName),
        globals.lookup_string_option(Globals, shared_library_extension,
            SharedLibExt),
        ShLibFileName = "lib" ++ BaseFileName ++ SharedLibExt,
        get_install_name_option(Globals, ShLibFileName, InstallNameOpt)
    else
        InstallNameOpt = ""
    ),

    globals.get_trace_level(Globals, TraceLevel),
    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    (
        TraceEnabled = exec_trace_is_not_enabled,
        TraceOpts = ""
    ;
        TraceEnabled = exec_trace_is_enabled,
        globals.lookup_string_option(Globals, TraceFlagsOpt, TraceOpts)
    ),

    get_frameworks(Globals, Frameworks),
    get_link_opts_for_libraries(Globals, MaybeLinkLibraries, Specs, !IO),
    globals.lookup_string_option(Globals, linker_opt_separator, LinkOptSep),
    (
        MaybeLinkLibraries = maybe.yes(LinkLibrariesList),
        join_quoted_string_list(LinkLibrariesList, "", "", " ",
            LinkLibraries),

        globals.lookup_bool_option(Globals, restricted_command_line,
            RestrictedCommandLine),
        (
            % If we have a restricted command line then it's possible
            % that following link command will call sub-commands itself
            % and thus overflow the command line, so in this case
            % we first create an archive of all of the object files.
            RestrictedCommandLine = yes,
            globals.lookup_string_option(Globals, library_extension, LibExt),
            io.file.get_temp_directory(TempDir, !IO),
            io.file.make_temp_file(TempDir, "mtmp", LibExt,
                TmpArchiveResult, !IO),
            (
                TmpArchiveResult = ok(TmpArchive),
                % Only include actual object files in the temporary archive,
                % not other files such as other archives.
                filter_object_files(Globals, ObjectsList,
                    ProperObjectFiles, NonObjectFiles),
                % Delete the currently empty output file first, otherwise ar
                % will fail to recognise its file format.
                io.file.remove_file(TmpArchive, _, !IO),
                create_archive(Globals, ProgressStream, TmpArchive, yes,
                    ProperObjectFiles, ArchiveSucceeded, !IO),
                MaybeDeleteTmpArchive = yes(TmpArchive),
                join_quoted_string_list([TmpArchive | NonObjectFiles],
                    "", "", " ", Objects)
            ;
                TmpArchiveResult = error(Error),
                io.format(ProgressStream,
                    "Could not create temporary file: %s\n",
                    [s(error_message(Error))], !IO),
                ArchiveSucceeded = did_not_succeed,
                MaybeDeleteTmpArchive = no,
                join_quoted_string_list(ObjectsList, "", "", " ", Objects)
            )
        ;
            RestrictedCommandLine = no,
            ArchiveSucceeded = succeeded,
            MaybeDeleteTmpArchive = no,
            join_quoted_string_list(ObjectsList, "", "", " ", Objects)
        ),

        (
            ArchiveSucceeded = succeeded,

            % Note that LDFlags may contain `-l' options so it should come
            % after Objects.
            globals.lookup_string_option(Globals, CommandOpt, Command),
            get_linker_output_option(Globals, LinkTargetType, OutputOpt),
            string.append_list([
                Command, " ",
                StaticOpts, " ",
                LinkerStripOpt, " ",
                UndefOpt, " ",
                ThreadOpts, " ",
                LTOOpts, " ",
                TraceOpts, " ",
                ReserveStackSizeOpt, " ",
                OutputOpt, quote_shell_cmd_arg(FullOutputFileName), " ",
                Objects, " ",
                LinkOptSep, " ",
                LinkLibraryDirectories, " ",
                RpathOpts, " ",
                FrameworkDirectories, " ",
                InstallNameOpt, " ",
                DebugOpts, " ",
                SanitizerOpts, " ",
                Frameworks, " ",
                ResCmdLinkOpts, " ",
                LDFlags, " ",
                LinkLibraries, " ",
                MercuryStdLibs, " ",
                HwlocOpts, " ",
                SystemLibs], LinkCmd),

            globals.lookup_bool_option(Globals, demangle, Demangle),
            (
                Demangle = bool.yes,
                globals.lookup_string_option(Globals, demangle_command,
                    DemangleCmd),
                MaybeDemangleCmd = maybe.yes(DemangleCmd)
            ;
                Demangle = bool.no,
                MaybeDemangleCmd = maybe.no
            ),

            invoke_system_command_maybe_filter_output(Globals,
                ProgressStream, ProgressStream, cmd_verbose_commands,
                LinkCmd, MaybeDemangleCmd, LinkSucceeded, !IO),
            % Invoke strip utility separately if required.
            ( if
                LinkSucceeded = succeeded,
                LinkerStripOpt = "",
                StripExeCommand \= ""
            then
                string.format("%s %s %s",
                    [s(StripExeCommand), s(StripExeFlags),
                    s(quote_shell_cmd_arg(FullOutputFileName))], StripCmd),
                invoke_system_command_maybe_filter_output(Globals,
                    ProgressStream, ProgressStream, cmd_verbose_commands,
                    StripCmd, no, Succeeded, !IO)
            else
                Succeeded = LinkSucceeded
            )
        ;
            ArchiveSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        ),
        (
            MaybeDeleteTmpArchive = maybe.yes(FileToDelete),
            io.file.remove_file(FileToDelete, _, !IO)
        ;
            MaybeDeleteTmpArchive = maybe.no
        )
    ;
        MaybeLinkLibraries = maybe.no,
        Succeeded = did_not_succeed
    ).

    % Find the standard Mercury libraries, and the system
    % libraries needed by them.
    % Return the empty string if --mercury-standard-library-directory
    % is not set.
    % NOTE: changes here may require changes to get_mercury_std_libs_for_java.
    %
:- pred get_mercury_std_libs(globals::in,
    linked_target_type::in(c_or_csharp_exe_or_lib), string::out) is det.

get_mercury_std_libs(Globals, TargetType, StdLibs) :-
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_gc_method(Globals, GCMethod),
        (
            ( TargetType = executable
            ; TargetType = shared_library
            ),
            LibExt = ext_cur_gas(ext_cur_gas_lib_lib_opt),
            globals.lookup_string_option(Globals, mercury_linkage,
                MercuryOrCsharpLinkage)
        ;
            ( TargetType = csharp_executable
            ; TargetType = csharp_library
            ),
            LibExt = ext_cur_gs(ext_cur_gs_lib_cil_dll),
            MercuryOrCsharpLinkage = "csharp"
        ),
        globals.get_grade_dir(Globals, GradeDir),

        % GC libraries.
        % We always compile with hgc since it's home-grown and very small.
        (
            GCMethod = gc_automatic,
            StaticGCLibs = "",
            SharedGCLibs = ""
        ;
            GCMethod = gc_none,
            StaticGCLibs = "",
            SharedGCLibs = ""
        ;
            GCMethod = gc_hgc,
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
            globals.lookup_bool_option(Globals, c_debug_grade, CDebugGrade),
            (
                CDebugGrade = yes,
                GCGrade1 = GCGrade0 ++ "_c_debug"
            ;
                CDebugGrade = no,
                GCGrade1 = GCGrade0
            ),
            globals.lookup_bool_option(Globals, profile_time, ProfTime),
            globals.lookup_bool_option(Globals, profile_deep, ProfDeep),
            ( if
                ( ProfTime = yes
                ; ProfDeep = yes
                )
            then
                GCGrade2 = GCGrade1 ++ "_prof"
            else
                GCGrade2 = GCGrade1
            ),
            globals.lookup_bool_option(Globals, parallel, Parallel),
            (
                Parallel = yes,
                GCGrade = "par_" ++ GCGrade2
            ;
                Parallel = no,
                GCGrade = GCGrade2
            ),
            link_lib_args(Globals, TargetType, StdLibDir, "",
                LibExt, GCGrade, StaticGCLibs, SharedGCLibs)
        ;
            GCMethod = gc_accurate,
            StaticGCLibs = "",
            SharedGCLibs = ""
        ),

        % Trace libraries.
        globals.get_trace_level(Globals, TraceLevel),
        TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
        (
            TraceEnabled = exec_trace_is_not_enabled,
            StaticTraceLibs = "",
            SharedTraceLibs = ""
        ;
            TraceEnabled = exec_trace_is_enabled,
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_trace", StaticTraceLib, TraceLib),
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_eventspec", StaticEventSpecLib, EventSpecLib),
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_browser", StaticBrowserLib, BrowserLib),
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_mdbcomp", StaticMdbCompLib, MdbCompLib),
            StaticTraceLibs = string.join_list(" ",
                [StaticTraceLib, StaticEventSpecLib, StaticBrowserLib,
                StaticMdbCompLib]),
            SharedTraceLibs = string.join_list(" ",
                [TraceLib, EventSpecLib, BrowserLib, MdbCompLib])
        ),

        % Source-to-source debugging libraries.
        globals.lookup_bool_option(Globals, link_ssdb_libs, SourceDebug),
        (
            SourceDebug = yes,
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_ssdb", StaticSsdbLib, SsdbLib),
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_browser", StaticBrowserLib2, BrowserLib2),
            link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
                LibExt, "mer_mdbcomp", StaticMdbCompLib2, MdbCompLib2),
            StaticSourceDebugLibs = string.join_list(" ",
                [StaticSsdbLib, StaticBrowserLib2, StaticMdbCompLib2]),
            SharedSourceDebugLibs = string.join_list(" ",
                [SsdbLib, BrowserLib2, MdbCompLib2])
        ;
            SourceDebug = no,
            StaticSourceDebugLibs = "",
            SharedSourceDebugLibs = ""
        ),

        link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
            LibExt, "mer_std", StaticStdLib, StdLib),
        link_lib_args(Globals, TargetType, StdLibDir, GradeDir,
            LibExt, "mer_rt", StaticRuntimeLib, RuntimeLib),
        ( if MercuryOrCsharpLinkage = "static" then
            StdLibs = string.join_list(" ", [
                StaticTraceLibs,
                StaticSourceDebugLibs,
                StaticStdLib,
                StaticRuntimeLib,
                StaticGCLibs
            ])
        else if MercuryOrCsharpLinkage = "shared" then
            StdLibs = string.join_list(" ", [
                SharedTraceLibs,
                SharedSourceDebugLibs,
                StdLib,
                RuntimeLib,
                SharedGCLibs
            ])
        else if MercuryOrCsharpLinkage = "csharp" then
            StdLibs = string.join_list(" ", [
                SharedTraceLibs,
                SharedSourceDebugLibs,
                StdLib
            ])
        else
            unexpected($pred, "unknown linkage " ++ MercuryOrCsharpLinkage)
        )
    ;
        MaybeStdLibDir = no,
        StdLibs = ""
    ).

:- pred link_lib_args(globals::in,
    linked_target_type::in(c_or_csharp_exe_or_lib), string::in,
    string::in, ext::in, string::in, string::out, string::out) is det.

link_lib_args(Globals, TargetType, StdLibDir, GradeDir, Ext,
        Name, StaticArg, SharedArg) :-
    (
        ( TargetType = executable
        ; TargetType = shared_library
        ),
        LibPrefix = "lib"
    ;
        ( TargetType = csharp_executable
        ; TargetType = csharp_library
        ),
        LibPrefix = ""
    ),
    StaticLibName = LibPrefix ++ Name ++ extension_to_string(Globals, Ext),
    StaticArg = quote_shell_cmd_arg(StdLibDir/"lib"/GradeDir/StaticLibName),
    make_link_lib(Globals, TargetType, Name, SharedArg).

    % Pass either `-llib' or `PREFIX/lib/GRADE/liblib.a', depending on
    % whether we are linking with static or shared Mercury libraries.
    %
:- pred get_link_opts_for_libraries(globals::in, maybe(list(string))::out,
    list(error_spec)::out, io::di, io::uo) is det.

get_link_opts_for_libraries(Globals, MaybeLinkLibraries, Specs, !IO) :-
    globals.lookup_accumulating_option(Globals, link_libraries,
        LinkLibrariesList0),
    list.map2_foldl2(get_link_opts_for_library(Globals),
        LinkLibrariesList0, LinkLibrariesList, SpecsList,
        succeeded, LibrariesSucceeded, !IO),
    list.condense(SpecsList, Specs),
    (
        LibrariesSucceeded = succeeded,
        MaybeLinkLibraries = yes(LinkLibrariesList)
    ;
        LibrariesSucceeded = did_not_succeed,
        MaybeLinkLibraries = no
    ).

:- pred make_link_lib(globals::in,
    linked_target_type::in(c_or_csharp_exe_or_lib),
    string::in, string::out) is det.

make_link_lib(Globals, TargetType, LibName, LinkOpt) :-
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
        globals.lookup_string_option(Globals, LinkLibFlag, LinkLibOpt),
        globals.lookup_string_option(Globals, LinkLibSuffix, Suffix),
        LinkOpt = quote_shell_cmd_arg(LinkLibOpt ++ LibName ++ Suffix)
    ;
        ( TargetType = csharp_executable
        ; TargetType = csharp_library
        ),
        LinkLibOpt = "-r:",
        Suffix = ".dll",
        LinkOpt = quote_shell_cmd_arg(LinkLibOpt ++ LibName ++ Suffix)
    ).

:- pred get_runtime_library_path_opts(globals::in, linked_target_type::in,
    option::in(bound(shlib_linker_rpath_flag ; linker_rpath_flag)),
    option::in(bound(shlib_linker_rpath_separator ; linker_rpath_separator)),
    string::out) is det.

get_runtime_library_path_opts(Globals, LinkTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts) :-
    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    shared_libraries_supported(Globals, SharedLibsSupported),
    globals.lookup_string_option(Globals, linkage, Linkage),
    ( if
        UseInstallName = no,
        SharedLibsSupported = yes,
        ( Linkage = "shared"
        ; LinkTargetType = shared_library
        )
    then
        globals.lookup_accumulating_option(Globals,
            runtime_link_library_directories, RpathDirs0),
        RpathDirs = list.map(quote_shell_cmd_arg, RpathDirs0),
        (
            RpathDirs = [],
            RpathOpts = ""
        ;
            RpathDirs = [_ | _],
            globals.lookup_string_option(Globals, RpathSepOpt, RpathSep),
            globals.lookup_string_option(Globals, RpathFlagOpt, RpathFlag),
            RpathOpts0 = string.join_list(RpathSep, RpathDirs),
            RpathOpts = RpathFlag ++ RpathOpts0
        )
    else
        RpathOpts = ""
    ).

:- pred get_system_libs(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_system_libs(Globals, TargetType, SystemLibs) :-
    % System libraries used when tracing.
    globals.get_trace_level(Globals, TraceLevel),
    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    (
        TraceEnabled = exec_trace_is_not_enabled,
        SystemTraceLibs = ""
    ;
        TraceEnabled = exec_trace_is_enabled,
        globals.lookup_string_option(Globals, trace_libs, SystemTraceLibs0),
        globals.lookup_bool_option(Globals, use_readline, UseReadline),
        (
            UseReadline = yes,
            globals.lookup_string_option(Globals, readline_libs, ReadlineLibs),
            SystemTraceLibs = SystemTraceLibs0 ++ " " ++ ReadlineLibs
        ;
            UseReadline = no,
            SystemTraceLibs = SystemTraceLibs0
        )
    ),
    % Thread libraries
    use_thread_libs(Globals, UseThreadLibs),
    (
        UseThreadLibs = yes,
        globals.lookup_string_option(Globals, thread_libs, ThreadLibs)
    ;
        UseThreadLibs = no,
        ThreadLibs = ""
    ),
    % Other system libraries.
    (
        TargetType = shared_library,
        globals.lookup_string_option(Globals, shared_libs, OtherSystemLibs)
    ;
        TargetType = executable,
        globals.lookup_string_option(Globals, math_lib, OtherSystemLibs)
    ),
    SystemLibs = string.join_list(" ",
        [SystemTraceLibs, OtherSystemLibs, ThreadLibs]).

:- pred use_thread_libs(globals::in, bool::out) is det.

use_thread_libs(Globals, UseThreadLibs) :-
    globals.lookup_bool_option(Globals, parallel, UseThreadLibs).

    % When using --restricted-command-line with Visual C we add all the object
    % files to a temporary archive before linking an executable.
    % However, if only .lib files are given on the command line then
    % the linker needs to manually told some details that it usually infers
    % from the object files, for example the program entry point and the
    % target machine type. link.exe will print a warning (#4001) when this
    % occurs so we also need to shut that up.
    %
:- pred get_restricted_command_line_link_opts(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_restricted_command_line_link_opts(Globals, LinkTargetType,
        ResCmdLinkOpts) :-
    globals.lookup_bool_option(Globals, restricted_command_line,
        RestrictedCommandLine),
    (
        RestrictedCommandLine = yes,
        (
            LinkTargetType = executable,
            get_c_compiler_type(Globals, C_CompilerType),
            (
                C_CompilerType = cc_cl_x86(_),
                ResCmdLinkFlags = [
                    "-nologo",
                    "-ignore:4001",
                    "-subsystem:console",
                    "-machine:x86",
                    "-entry:wmainCRTStartup",
                    "-defaultlib:libcmt"
                ],
                join_string_list(ResCmdLinkFlags, "", "", " ", ResCmdLinkOpts)
            ;
                C_CompilerType = cc_cl_x64(_),
                ResCmdLinkFlags = [
                    "-nologo",
                    "-ignore:4001",
                    "-subsystem:console",
                    "-machine:x64",
                    "-entry:wmainCRTStartup",
                    "-defaultlib:libcmt"
                ],
                join_string_list(ResCmdLinkFlags, "", "", " ", ResCmdLinkOpts)
            ;
                ( C_CompilerType = cc_gcc(_, _, _)
                ; C_CompilerType = cc_clang(_)
                ; C_CompilerType = cc_unknown
                ),
                ResCmdLinkOpts = ""
            )
        ;
            LinkTargetType = shared_library,
            ResCmdLinkOpts = ""
        )
    ;
        RestrictedCommandLine = no,
        ResCmdLinkOpts = ""
    ).

    % Filter list of files into those with and without known object file
    % extensions.
    %
:- pred filter_object_files(globals::in, list(string)::in,
    list(string)::out, list(string)::out) is det.

filter_object_files(Globals, Files, ObjectFiles, NonObjectFiles) :-
    globals.lookup_string_option(Globals, object_file_extension, ObjExt),
    globals.lookup_string_option(Globals, pic_object_file_extension,
        PicObjExt),
    list.filter(has_object_file_extension(ObjExt, PicObjExt), Files,
        ObjectFiles, NonObjectFiles).

:- pred has_object_file_extension(string::in, string::in, string::in)
    is semidet.

has_object_file_extension(ObjExt, PicObjExt, FileName) :-
    ( string.suffix(FileName, ObjExt)
    ; string.suffix(FileName, PicObjExt)
    ).

post_link_maybe_make_symlink_or_copy(Globals, ProgressStream,
        FullFileName, CurDirFileName, ModuleName, LinkTargetType,
        Succeeded, MadeSymlinkOrCopy, !IO) :-
    ( if FullFileName = CurDirFileName then
        Succeeded = succeeded,
        MadeSymlinkOrCopy = no
    else
        do_full_curdir_timestamps_match(FullFileName, CurDirFileName,
            DoTimestampsMatch, !IO),
        (
            DoTimestampsMatch = yes,
            Succeeded0 = succeeded,
            MadeSymlinkOrCopy = no
        ;
            DoTimestampsMatch = no,
            % Remove the target of the symlink/copy in case it already exists.
            io.file.remove_file_recursively(CurDirFileName, _, !IO),

            make_symlink_or_copy_file(Globals, ProgressStream,
                FullFileName, CurDirFileName, Succeeded0, !IO),
            MadeSymlinkOrCopy = yes
        ),

        % For the Java and C# grades we also need to symlink or copy the
        % launcher scripts or batch files.
        ( if
            Succeeded0 = succeeded,
            (
                LinkTargetType = csharp_executable,
                % NOTE: we don't generate a launcher script for C# executables
                % on Windows -- it isn't necessary since they can be executed
                % directly.
                globals.get_target_env_type(Globals, TargetEnvType),
                TargetEnvType = env_type_posix
            ;
                LinkTargetType = java_executable
            )
        then
            get_launcher_script_extension(Globals, ScriptExt),
            % XXX LEGACY
            module_name_to_file_name_full_curdir(Globals, $pred, ScriptExt,
                ModuleName, FullLauncherName, _FullLauncherNameProposed,
                CurDirLauncherName),

            do_full_curdir_timestamps_match(FullLauncherName,
                CurDirLauncherName, DoLauncherTimestampsMatch, !IO),
            (
                DoLauncherTimestampsMatch = yes,
                Succeeded = succeeded
            ;
                DoLauncherTimestampsMatch = no,
                % Remove the target of the symlink/copy in case
                % it already exists.
                io.file.remove_file_recursively(CurDirLauncherName, _, !IO),
                make_symlink_or_copy_file(Globals, ProgressStream,
                    FullLauncherName, CurDirLauncherName, Succeeded, !IO)
            )
        else
            Succeeded = Succeeded0
        )
    ).

:- pred get_framework_directories(globals::in, string::out) is det.

get_framework_directories(Globals, FrameworkDirs) :-
    globals.lookup_accumulating_option(Globals, framework_directories,
        FrameworkDirs0),
    join_quoted_string_list(FrameworkDirs0, "-F", "", " ", FrameworkDirs).

:- pred get_frameworks(globals::in, string::out) is det.

get_frameworks(Globals, FrameworkOpts) :-
    globals.lookup_accumulating_option(Globals, frameworks, Frameworks),
    join_quoted_string_list(Frameworks, "-framework ", "", " ", FrameworkOpts).

shared_libraries_supported(Globals, Supported) :-
    % XXX This seems to be the standard way to check whether shared libraries
    % are supported, but it is not very nice.
    globals.lookup_string_option(Globals, library_extension, LibExt),
    globals.lookup_string_option(Globals, shared_library_extension,
        SharedLibExt),
    Supported = (if LibExt = SharedLibExt then no else yes).

:- pred get_linker_output_option(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_linker_output_option(Globals, LinkTargetType, OutputOpt) :-
    get_c_compiler_type(Globals, C_CompilerType),
    % XXX we should allow the user to override the compiler's choice of
    % output option here.
    % NOTE: the spacing around the value of OutputOpt here is important.
    % Any changes should be reflected in predicate link_exe_or_shared_lib/9.
    (
        ( C_CompilerType = cc_cl_x86(_)
        ; C_CompilerType = cc_cl_x64(_)
        ),
        ( if LinkTargetType = executable then
            % NOTE: -Fe _must not_ be separated from its argument by any
            % whitespace; the lack of a trailing space in the following
            % is deliberate.
            OutputOpt = " -Fe"
        else
            % XXX This is almost certainly wrong, but we don't currently
            % support building shared libraries with mmc on Windows
            % anyway.
            OutputOpt = " -o "
        )
    ;
        ( C_CompilerType = cc_gcc(_, _, _)
        ; C_CompilerType = cc_clang(_)
        ; C_CompilerType = cc_unknown
        ),
        OutputOpt = " -o "
    ).

:- func reserve_stack_size_flags(globals) = string.

reserve_stack_size_flags(Globals) = Flags :-
    globals.lookup_int_option(Globals, cstack_reserve_size, ReserveStackSize),
    ( if ReserveStackSize = -1 then
        Flags = ""
    else
        get_c_compiler_type(Globals, C_CompilerType),
        (
            ( C_CompilerType = cc_gcc(_, _, _)
            ; C_CompilerType = cc_clang(_)
            ; C_CompilerType = cc_unknown
            ),
            string.format("-Wl,--stack=%d", [i(ReserveStackSize)], Flags)
        ;
            ( C_CompilerType = cc_cl_x86(_)
            ; C_CompilerType = cc_cl_x64(_)
            ),
            string.format("-stack:%d", [i(ReserveStackSize)], Flags)
        )
    ).

%---------------------------------------------------------------------------%

:- pred get_link_opts_for_library(globals::in, string::in, string::out,
    list(error_spec)::out, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

get_link_opts_for_library(Globals, LibName, LinkerOpt,
        !:Specs, !Succeeded, !IO) :-
    !:Specs = [],
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_string_option(Globals, mercury_linkage, MercuryLinkage),
        globals.get_c_compiler_type(Globals, CCompilerType),
        (
            ( CCompilerType = cc_gcc(_, _, _)
            ; CCompilerType = cc_clang(_)
            ; CCompilerType = cc_unknown
            ),
            LinkOpt = "-l",
            LibSuffix = ""
        ;
            ( CCompilerType = cc_cl_x86(_)
            ; CCompilerType = cc_cl_x64(_)
            ),
            LinkOpt = "",
            LibSuffix = ".lib"
        )
    ;
        Target = target_csharp,
        MercuryLinkage = "shared",
        LinkOpt = "-r:",
        LibSuffix = ".dll"
    ;
        Target = target_java,
        unexpected($pred, "target_java")
    ),

    globals.lookup_accumulating_option(Globals, mercury_libraries,
        MercuryLibs),
    ( if
        MercuryLinkage = "static",
        list.member(LibName, MercuryLibs)
    then
        % If we are linking statically with Mercury libraries,
        % pass the absolute pathname of the `.a' file for the library.
        file_name_to_module_name(LibName, LibModuleName),
        % XXX LEGACY
        module_name_to_lib_file_name_full_curdir(Globals, $pred, "lib",
            ext_cur_gas(ext_cur_gas_lib_lib_opt), LibModuleName,
            _FullLibFileName, _FullLibFileNameProposed, LibFileName),
        SearchAuthDirs = get_search_auth_lib_dirs(le_a, Globals),
        search_for_file_returning_dir(SearchAuthDirs, LibFileName,
            _SearchDirs, MaybeDirName, !IO),
        (
            MaybeDirName = ok(DirName),
            LinkerOpt = DirName/LibFileName
        ;
            MaybeDirName = error(Error),
            LinkerOpt = "",
            Pieces = [words(Error), suffix("."), nl],
            % XXX SEARCH_ERROR _SearchDirs
            Spec = no_ctxt_spec($pred, severity_error,
                phase_find_files(LibFileName), Pieces),
            !:Specs = [Spec],
            !:Succeeded = did_not_succeed
        )
    else
        LinkerOpt = LinkOpt ++ LibName ++ LibSuffix
    ).

:- pred create_archive(globals::in, io.text_output_stream::in, file_name::in,
    bool::in, list(file_name)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

create_archive(Globals, ProgressStream, FullLibFileName, Quote,
        ObjectList, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, create_archive_command, ArCmd),
    globals.lookup_accumulating_option(Globals, create_archive_command_flags,
        ArFlagsList),
    join_string_list(ArFlagsList, "", "", " ", ArFlags),
    globals.lookup_string_option(Globals, create_archive_command_output_flag,
        ArOutputFlag),
    globals.lookup_string_option(Globals, ranlib_command, RanLib),
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

    % NOTE When using the Windows Library Manager Tool (lib),
    % there must _not_ be a space between the -OUT: option and its argument.
    % XXX We actually check the C compiler type here, since that is more
    % robust than using the values of the configuration options used for
    % archive creation.
    get_c_compiler_type(Globals, C_CompilerType),
    (
        % If we are using Visual C, then we must be using the Microsoft
        % lib tool.
        ( C_CompilerType = cc_cl_x86(_)
        ; C_CompilerType = cc_cl_x64(_)
        ),
        ArOutputSpace = ""
    ;
        ( C_CompilerType = cc_gcc(_, _, _)
        ; C_CompilerType = cc_clang(_)
        ; C_CompilerType = cc_unknown
        ),
        ArOutputSpace = " "
    ),

    MakeLibCmdArgs = string.append_list([
        ArFlags, " ",
        ArOutputFlag, ArOutputSpace, FullLibFileName, " ",
        Objects]
    ),

    invoke_long_system_command(Globals, ProgressStream, ProgressStream,
        cmd_verbose_commands, ArCmd, MakeLibCmdArgs, MakeLibCmdSucceeded, !IO),

    ( if
        ( RanLib = ""
        ; MakeLibCmdSucceeded = did_not_succeed
        )
    then
        Succeeded = MakeLibCmdSucceeded
    else
        RanLibCmd = string.append_list([RanLib, " ", FullLibFileName]),
        invoke_system_command(Globals, ProgressStream, ProgressStream,
            cmd_verbose_commands, RanLibCmd, Succeeded, !IO)
    ).

:- pred create_csharp_exe_or_lib(globals::in, io.text_output_stream::in,
    linked_target_type::in(csharp_linked_target_type),
    module_name::in, file_name::in, list(file_name)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

create_csharp_exe_or_lib(Globals, ProgressStream, LinkTargetType,
        MainModuleName, FullOutputFileName0, SourceList0,
        Specs, Succeeded, !IO) :-
    get_system_env_type(Globals, EnvType),
    get_csharp_compiler_type(Globals, CSharpCompilerType),

    FullOutputFileName = csharp_file_name(EnvType, CSharpCompilerType,
        FullOutputFileName0),
    SourceList = list.map(csharp_file_name(EnvType, CSharpCompilerType),
        SourceList0),

    % Suppress the MS C# compiler's banner message.
    (
        CSharpCompilerType = csharp_microsoft,
        NoLogoOpt = "-nologo "
    ;
        ( CSharpCompilerType = csharp_mono
        ; CSharpCompilerType = csharp_unknown
        ),
        NoLogoOpt = ""
    ),

    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    (
        % If we output line numbers, the mono C# compiler outputs lots of
        % spurious warnings about unused variables and unreachable code,
        % so disable these warnings. It also confuses #pragma warning,
        % which is why we make the options global.
        LineNumbers = yes,
        NoWarnLineNumberOpt = "-nowarn:162,219 "
    ;
        LineNumbers = no,
        NoWarnLineNumberOpt = ""
    ),

    % NOTE: we use the -option style options in preference to the /option
    % style in order to avoid problems with POSIX style shells.
    globals.lookup_bool_option(Globals, target_debug, Debug),
    (
        Debug = yes,
        DebugOpt = "-debug "
    ;
        Debug = no,
        DebugOpt = ""
    ),
    (
        LinkTargetType = csharp_executable,
        TargetOption = "-target:exe",
        SignAssemblyOpt = ""
    ;
        LinkTargetType = csharp_library,
        TargetOption = "-target:library",
        globals.lookup_string_option(Globals, sign_assembly, KeyFile),
        ( if KeyFile = "" then
            SignAssemblyOpt = ""
        else
            SignAssemblyOpt = "-keyfile:" ++ KeyFile ++ " "
        )
    ),

    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList0),
    LinkLibraryDirectoriesList =
        list.map(csharp_file_name(EnvType, CSharpCompilerType),
        LinkLibraryDirectoriesList0),
    LinkerPathFlag = "-lib:",
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),

    get_link_opts_for_libraries(Globals, MaybeLinkLibraries, Specs, !IO),
    (
        MaybeLinkLibraries = yes(LinkLibrariesList0),
        LinkLibrariesList =
            list.map(csharp_file_name(EnvType, CSharpCompilerType),
            LinkLibrariesList0),
        join_quoted_string_list(LinkLibrariesList, "", "", " ",
            LinkLibraries)
    ;
        MaybeLinkLibraries = no,
        LinkLibraries = ""
    ),

    globals.lookup_string_option(Globals, csharp_compiler, CSharpCompilerCmd),
    get_mercury_std_libs(Globals, LinkTargetType, MercuryStdLibs),
    globals.lookup_accumulating_option(Globals, csharp_flags, CSCFlagsList),
    CmdArgs = string.join_list(" ", [
        NoLogoOpt,
        NoWarnLineNumberOpt,
        DebugOpt,
        TargetOption,
        "-out:" ++ FullOutputFileName,
        SignAssemblyOpt,
        LinkLibraryDirectories,
        LinkLibraries,
        MercuryStdLibs] ++
        CSCFlagsList ++
        SourceList),
    invoke_long_system_command(Globals, ProgressStream, ProgressStream,
        cmd_verbose_commands, CSharpCompilerCmd, CmdArgs, Succeeded0, !IO),

    % Also create a shell script to launch it if necessary.
    globals.get_target_env_type(Globals, TargetEnvType),
    globals.lookup_string_option(Globals, cli_interpreter, CLI),
    ( if
        Succeeded0 = succeeded,
        LinkTargetType = csharp_executable,
        CLI \= "",
        TargetEnvType = env_type_posix
    then
        construct_cli_shell_script(Globals, FullOutputFileName, ContentStr),
        create_launcher_shell_script(ProgressStream, Globals, MainModuleName,
            ContentStr, Succeeded, !IO)
    else
        Succeeded = Succeeded0
    ).

    % Converts the given filename into a format acceptable to the C# compiler.
    %
    % Older MS C# compilers only allowed \ as the path separator, so we convert
    % all / into \ when using an MS C# compiler on Windows.
    %
:- func csharp_file_name(env_type, csharp_compiler_type, file_name)
    = file_name.

csharp_file_name(EnvType, CSharpCompiler, FileName0) = FileName :-
    (
        EnvType = env_type_posix,
        FileName = FileName0
    ;
        ( EnvType = env_type_cygwin
        ; EnvType = env_type_win_cmd
        ; EnvType = env_type_powershell
        ),
        (
            ( CSharpCompiler = csharp_microsoft
            ; CSharpCompiler = csharp_unknown
            ),
            FileName = convert_to_windows_path_format(FileName0)
        ;
            CSharpCompiler = csharp_mono,
            FileName = FileName0
        )
    ;
        EnvType = env_type_msys,
        (
            CSharpCompiler = csharp_microsoft,
            FileName = convert_to_windows_path_format(FileName0)
        ;
            ( CSharpCompiler = csharp_mono
            ; CSharpCompiler = csharp_unknown
            ),
            FileName = FileName0
        )
    ).

:- func convert_to_windows_path_format(file_name) = file_name.

convert_to_windows_path_format(FileName) =
    string.replace_all(FileName, "/", "\\\\").

:- pred construct_cli_shell_script(globals::in, string::in,
    string::out) is det.

construct_cli_shell_script(Globals, ExeFileName, ContentStr) :-
    globals.lookup_string_option(Globals, cli_interpreter, CLI),
    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    join_quoted_string_list(LinkLibraryDirectoriesList, "", "",
        ":", LinkLibraryDirectories),
    ContentStr = string.append_list([
        "#!/bin/sh\n",
        "DIR=${0%/*}\n",
        "MONO_PATH=$MONO_PATH:", LinkLibraryDirectories, "\n",
        "export MONO_PATH\n",
        "CLI_INTERPRETER=${CLI_INTERPRETER:-", CLI, "}\n",
        "exec \"$CLI_INTERPRETER\" \"$DIR/", ExeFileName, "\" \"$@\"\n"
    ]).

%---------------------------------------------------------------------------%
%
% Create Java "executables" or archives.
%

:- pred create_java_exe_or_lib(globals::in, io.text_output_stream::in,
    linked_target_type::in(java_linked_target_type),
    module_name::in, file_name::in, list(file_name)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

create_java_exe_or_lib(Globals, ProgressStream, LinkTargetType,
        MainModuleName, FullJarFileName, ObjectList, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, java_archive_command, Jar),

    list_class_files_for_jar(Globals, ObjectList, ClassSubDir, ListClassFiles,
        !IO),
    (
        ListClassFiles = [],
        unexpected($pred, "empty list of .class files")
    ;
        ListClassFiles = [_ | _]
    ),

    % Write the list of class files to a temporary file and pass the name of
    % the temporary file to jar using @syntax. The list of class files can be
    % extremely long. We create the temporary file in the current directory to
    % avoid problems under Cygwin, where absolute paths will be interpreted
    % incorrectly when passed to a non-Cygwin jar program.
    open_temp_output_with_naming_scheme(".", "mtmp", "", TempFileResult, !IO),
    (
        TempFileResult = ok({TempFileName, Stream}),
        list.foldl(write_jar_class_argument(Stream, ClassSubDir),
            ListClassFiles, !IO),
        io.close_output(Stream, !IO),

        Cmd = string.append_list(
            [Jar, " cf ", FullJarFileName, " @", TempFileName]),
        invoke_system_command(Globals, ProgressStream, ProgressStream,
            cmd_verbose_commands, Cmd, Succeeded0, !IO),
        io.file.remove_file(TempFileName, _, !IO),
        (
            Succeeded0 = succeeded
        ;
            Succeeded0 = did_not_succeed,
            io.file.remove_file(FullJarFileName, _, !IO)
        )
    ;
        TempFileResult = error(ErrorMessage),
        io.format(ProgressStream, "%s\n", [s(ErrorMessage)], !IO),
        Succeeded0 = did_not_succeed
    ),
    ( if
        Succeeded0 = succeeded,
        LinkTargetType = java_executable
    then
        create_java_shell_script(ProgressStream, Globals, MainModuleName,
            Succeeded, !IO)
    else
        Succeeded = Succeeded0
    ).

:- pred write_jar_class_argument(io.text_output_stream::in,
    string::in, string::in, io::di, io::uo) is det.

write_jar_class_argument(Stream, ClassSubDir, ClassFileName, !IO) :-
    ( if dir.path_name_is_absolute(ClassFileName) then
        io.format(Stream, "%s\n", [s(ClassFileName)], !IO)
    else
        io.format(Stream, "-C %s %s\n",
            [s(ClassSubDir), s(ClassFileName)], !IO)
    ).

%---------------------------------------------------------------------------%

get_object_code_type(Globals, FileType, ObjectCodeType) :-
    (
        ( FileType = executable
        ; FileType = static_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        ),
        ObjectCodeType = non_pic
    ;
        FileType = shared_library,
        globals.lookup_string_option(Globals, pic_object_file_extension,
            PicObjExt),
        globals.lookup_string_option(Globals, object_file_extension, ObjExt),
        ObjectCodeType = ( if PicObjExt = ObjExt then non_pic else pic )
    ).

get_linked_target_type(Globals, LinkedTargetType) :-
    globals.lookup_bool_option(Globals, compile_to_shared_lib, MakeSharedLib),
    (
        MakeSharedLib = yes,
        LinkedTargetType = shared_library
    ;
        MakeSharedLib = no,
        LinkedTargetType = executable
    ).

%---------------------------------------------------------------------------%

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
    join_string_list(map(quote_shell_cmd_arg, Strings), Prefix, Suffix,
        Separator, Result).

    % join_module_list(Globals, Extension, FileNames, ExtFileNames, !IO):
    %
    % The list of strings `ExtFileNames' is computed from the list of strings
    % `FileNames', by removing any directory paths, and converting the
    % strings to file names and then back, adding the specified Extension.
    % (This conversion ensures that we follow the usual file naming
    % conventions.)
    %
:- pred join_module_list(globals::in, ext::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

join_module_list(_Globals, _Ext, [], [], !IO).
join_module_list(Globals, Ext,
        [FileName | FileNames], [ExtFileName | ExtFileNames], !IO) :-
    file_name_to_module_name(dir.det_basename(FileName), ModuleName),
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred, Ext, ModuleName,
        ExtFileName, _ExtFileNameProposed),
    join_module_list(Globals, Ext, FileNames, ExtFileNames, !IO).

%---------------------------------------------------------------------------%

make_all_module_command(Command0, MainModule, AllModules, Command, !IO) :-
    % Pass the main module first.
    list.map_foldl(module_name_to_source_file_name,
        [MainModule | list.delete_all(AllModules, MainModule)],
        ModuleSrcFileNames, !IO),
    Command = string.join_list(" ",
        list.map(quote_shell_cmd_arg, [Command0 | ModuleSrcFileNames])).

%---------------------------------------------------------------------------%

maybe_pic_object_file_extension(PIC, ExtObj, ExtInitObj) :-
    (
        PIC = non_pic,
        ExtObj =     ext_cur_ngs_gas_obj_obj_opt,
        ExtInitObj = ext_cur_ngs_gas_init_obj_obj_opt
    ;
        PIC = pic,
        ExtObj =     ext_cur_ngs_gas_obj_pic_obj_opt,
        ExtInitObj = ext_cur_ngs_gas_init_obj_pic_obj_opt
    ).

is_maybe_pic_object_file_extension(Globals, ExtStr, PIC) :-
    ( if
        % This test must come first -- if the architecture doesn't need
        % special treatment for PIC, we should always return `non_pic'.
        % `mmc --make' depends on this.
        globals.lookup_string_option(Globals, object_file_extension, ExtStr)
    then
        PIC = non_pic
    else if
        globals.lookup_string_option(Globals, pic_object_file_extension,
            ExtStr)
    then
        PIC = pic
    else
        fail
    ).

%---------------------------------------------------------------------------%
%
% Standalone interfaces.
%

% NOTE: the following code is similar to that of make_init_obj/7.
% Any changes here may need to be reflected there.

make_standalone_interface(Globals, ProgressStream, BaseName, !IO) :-
    make_standalone_int_header(ProgressStream, BaseName, HdrSucceeded, !IO),
    (
        HdrSucceeded = succeeded,
        make_standalone_int_body(Globals, ProgressStream, BaseName, !IO)
    ;
        HdrSucceeded = did_not_succeed
    ).

:- pred make_standalone_int_header(io.text_output_stream::in,
    string::in, maybe_succeeded::out, io::di, io::uo) is det.

make_standalone_int_header(ProgressStream, BaseName, Succeeded, !IO) :-
    HdrFileName = BaseName ++ ".h",
    io.open_output(HdrFileName, OpenResult, !IO),
    (
        OpenResult = ok(HdrFileStream),
        io.write_strings(HdrFileStream, [
            "#ifndef ", to_upper(BaseName), "_H\n",
            "#define ", to_upper(BaseName), "_H\n",
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
            "#endif /* ", to_upper(BaseName), "_H */\n"],
            !IO),
        io.close_output(HdrFileStream, !IO),
        Succeeded = succeeded
    ;
        OpenResult = error(Error),
        report_unable_to_open_file(ProgressStream, HdrFileName, Error, !IO),
        Succeeded = did_not_succeed
    ).

:- pred make_standalone_int_body(globals::in, io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

make_standalone_int_body(Globals, ProgressStream, BaseName, !IO) :-
    globals.lookup_accumulating_option(Globals, init_files, InitFiles0),
    % See the similar code in make_init_target_file for an explanation
    % of why we must remove duplicates from this list.
    list.remove_dups(InitFiles0, InitFiles1),
    globals.lookup_accumulating_option(Globals, trace_init_files,
        TraceInitFiles0),
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    globals.get_grade_dir(Globals, GradeDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        InitFiles2 = [
            StdLibDir / "modules" / GradeDir / "mer_rt.init",
            StdLibDir / "modules" / GradeDir / "mer_std.init" |
            InitFiles1
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
        % lmc script.
        MaybeStdLibDir = no,
        InitFiles2 = InitFiles1,
        TraceInitFiles = TraceInitFiles0,
        SourceDebugInitFiles = []
    ),
    globals.get_trace_level(Globals, TraceLevel),
    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    (
        TraceEnabled = exec_trace_is_enabled,
        TraceOpt = "-t",
        InitFiles3 = InitFiles2 ++ TraceInitFiles
    ;
        TraceEnabled = exec_trace_is_not_enabled,
        TraceOpt = "",
        InitFiles3 = InitFiles2
    ),
    globals.lookup_bool_option(Globals, link_ssdb_libs, SourceDebug),
    (
        SourceDebug = yes,
        InitFiles = InitFiles3 ++ SourceDebugInitFiles
    ;
        SourceDebug = no,
        InitFiles = InitFiles3
    ),
    globals.lookup_accumulating_option(Globals, runtime_flags,
        RuntimeFlagsList),
    join_quoted_string_list(RuntimeFlagsList, "-r ", "", " ", RuntimeFlags),
    globals.lookup_accumulating_option(Globals, init_file_directories,
        InitFileDirsList),
    join_quoted_string_list(InitFileDirsList, "-I ", "", " ", InitFileDirs),
    globals.lookup_string_option(Globals, experimental_complexity,
        ExperimentalComplexity),
    ( if ExperimentalComplexity = "" then
        ExperimentalComplexityOpt = ""
    else
        ExperimentalComplexityOpt = "-X " ++ ExperimentalComplexity
    ),
    compute_grade(Globals, Grade),
    globals.lookup_string_option(Globals, mkinit_command, MkInit),
    CFileName = BaseName ++ ".c",
    MkInitArgs = string.append_list([
        " -g ", Grade,
        " ", TraceOpt,
        " ", ExperimentalComplexityOpt,
        " ", RuntimeFlags,
        " -o ", quote_shell_cmd_arg(CFileName),
        " ", InitFileDirs,
        " -s "
    ]),

    % XXX STREAM Setting CmdOutputStream from ErrorStream looks VERY wrong,
    % but it preserves old behavior (or at least, it attempts to preserve it).
    % XXX ErrorStream has now been replaced by ProgressStream.
    CmdOutputStream = ProgressStream,
    invoke_mkinit(Globals, ProgressStream, CmdOutputStream,
        cmd_verbose_commands, MkInit, MkInitArgs, InitFiles,
        MkInitCmdSucceeded, !IO),
    (
        MkInitCmdSucceeded = succeeded,
        get_object_code_type(Globals, executable, PIC),
        maybe_pic_object_file_extension(PIC, ExtObj, _),
        Ext = ext_cur_ngs_gas(ExtObj),
        ObjFileName = BaseName ++ extension_to_string(Globals, Ext),
        do_compile_c_file(Globals, ProgressStream, PIC,
            CFileName, ObjFileName, CompileSucceeded, !IO),
        (
            CompileSucceeded = succeeded
        ;
            CompileSucceeded = did_not_succeed,
            io.set_exit_status(1, !IO),
            io.write_string(ProgressStream,
                "mercury_compile: error while compiling", !IO),
            io.format(ProgressStream,
                "standalone interface in `%s'\n", [s(CFileName)], !IO)
        )
    ;
        MkInitCmdSucceeded = did_not_succeed,
        io.set_exit_status(1, !IO),
        io.write_string(ProgressStream,
            "mercury_compile: error while creating ", !IO),
        io.format(ProgressStream,
            "standalone interface in `%s'\n", [s(CFileName)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% C compiler flags.
%

output_c_compiler_flags(Globals, Stream, !IO) :-
    get_object_code_type(Globals, executable, PIC),
    gather_c_compiler_flags(Globals, PIC, CFlags),
    io.write_string(Stream, CFlags, !IO).

%---------------------------------------------------------------------------%
%
% Grade defines flags.
%

output_c_grade_defines(Globals, Stream, !IO) :-
    get_object_code_type(Globals, executable, _PIC),
    gather_c_grade_defines(Globals, GradeDefines),
    io.write_string(Stream, GradeDefines, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%
%
% C include directory flags.
%

output_c_include_directory_flags(Globals, Stream, !IO) :-
    gather_c_include_dir_flags(Globals, InclOpts),
    io.write_string(Stream, InclOpts, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%
%
% Library link flags.
%

output_library_link_flags(Globals, Stream, Specs, !IO) :-
    % We output the library link flags as they are for when we are linking
    % an executable.
    LinkTargetType = executable,
    RpathFlagOpt = linker_rpath_flag,
    RpathSepOpt = linker_rpath_separator,

    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    globals.lookup_string_option(Globals, linker_path_flag, LinkerPathFlag),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),
    get_runtime_library_path_opts(Globals, LinkTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts),
    get_link_opts_for_libraries(Globals, MaybeLinkLibraries, Specs, !IO),
    (
        MaybeLinkLibraries = yes(LinkLibrariesList),
        join_string_list(LinkLibrariesList, "", "", " ", LinkLibraries)
    ;
        MaybeLinkLibraries = no,
        LinkLibraries = ""
    ),
    % Find the Mercury standard libraries.
    get_mercury_std_libs(Globals, LinkTargetType, MercuryStdLibs),
    get_system_libs(Globals, LinkTargetType, SystemLibs),
    io.format(Stream, "%s %s %s %s %s\n",
        [s(LinkLibraryDirectories), s(RpathOpts), s(LinkLibraries),
        s(MercuryStdLibs), s(SystemLibs)], !IO).

%---------------------------------------------------------------------------%

:- type is_as_new_as
    --->    is_as_new_as
    ;       is_not_as_new_as
    ;       missing_timestamp.

    % file_is_as_new_as(FileNameA, Rel, FileNameB, !IO)
    %
    % Check if FileNameA has a timestamp that is at least as new as FileNameB.
    % Rel is `missing_timestamp' iff either timestamp could not be retrieved.
    %
:- pred file_is_as_new_as(file_name::in, is_as_new_as::out, file_name::in,
    io::di, io::uo) is det.

file_is_as_new_as(FileNameA, Rel, FileNameB, !IO) :-
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

:- pred do_full_curdir_timestamps_match(string::in, string::in, bool::out,
    io::di, io::uo) is det.

do_full_curdir_timestamps_match(FullFileName, CurDirFileName,
        SameTimestamp, !IO) :-
    ( if FullFileName = CurDirFileName then
        io.file.file_modification_time(FullFileName, FullTimeResult, !IO),
        (
            FullTimeResult = ok(_),
            SameTimestamp = yes
        ;
            FullTimeResult = error(_),
            % There are no timestamps at all.
            SameTimestamp = no
        )
    else
        compare_file_timestamps(FullFileName, CurDirFileName, MaybeCmp, !IO),
        ( if MaybeCmp = yes(=) then
            SameTimestamp = yes
        else
            SameTimestamp = no
        )
    ).

:- pred compare_file_timestamps(file_name::in, file_name::in,
    maybe(comparison_result)::out, io::di, io::uo) is det.

compare_file_timestamps(FileNameA, FileNameB, MaybeCompare, !IO) :-
    io.file.file_modification_time(FileNameA, TimeResultA, !IO),
    io.file.file_modification_time(FileNameB, TimeResultB, !IO),
    ( if
        TimeResultA = ok(TimeA),
        TimeResultB = ok(TimeB)
    then
        compare(Compare, TimeA, TimeB),
        MaybeCompare = yes(Compare)
    else
        MaybeCompare = no
    ).

%---------------------------------------------------------------------------%

    % Succeeds if the configuration name for this machine matches
    % *-apple-darwin*, i.e. its an x86 / x86_64 / ppc machine with Mac OS X.
    %
:- pred arch_is_apple_darwin(string::in) is semidet.

arch_is_apple_darwin(FullArch) :-
    ArchComponents = string.split_at_char(('-'), FullArch),
    % See the comments at the head of config.sub for details of how autoconf
    % handles configuration names.
    ArchComponents = [_CPU, Mfr, OS],
    Mfr = "apple",
    string.prefix(OS, "darwin").

%---------------------------------------------------------------------------%
:- end_module backend_libs.compile_target_code.
%---------------------------------------------------------------------------%
