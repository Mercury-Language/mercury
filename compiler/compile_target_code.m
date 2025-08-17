%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: compile_target_code.m.
% Main authors: fjh, stayl.
%
% Code to compile the generated `.c' and `.java' files.
%
% (This module does *not* handle compilation of `.cs' files; that is done
% in link_target_code.m.)
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
:- import_module parse_tree.file_names.

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

    % get_executable_object_code_type(Globals, PIC):
    %
    % Work out whether we should be generating position-independent
    % object code for C executables.
    %
:- pred get_executable_object_code_type(globals::in, pic::out) is det.

%---------------------------------------------------------------------------%

    % compile_c_file(Globals, ProgressStream, PIC, ModuleName, Succeeded, !IO)
    %
:- pred compile_c_file(globals::in, io.text_output_stream::in, pic::in,
    module_name::in, maybe_succeeded::out, io::di, io::uo) is det.

    % do_compile_c_file(Globals, ProgressStream, PIC, CFile, ObjFile,
    %   Succeeded, !IO)
    %
:- pred do_compile_c_file(globals::in, io.text_output_stream::in, pic::in,
    string::in, string::in, maybe_succeeded::out, io::di, io::uo) is det.

%---------------------%

% Compilation of C# files is done by create_exe_or_lib_for_csharp/10
% in link_target_code.m.

%---------------------%

    % compile_java_files(Globals, ProgressStream, HeadJavaFile, TailJavaFiles,
    %   Succeeded, !IO)
    %
:- pred compile_java_files(globals::in, io.text_output_stream::in,
    string::in, list(string)::in, maybe_succeeded::out, io::di, io::uo) is det.

%---------------------%

    % make_library_init_file(Globals, ProgressStream,
    %   MainModuleName, ModuleNames, Succeeded, !IO):
    %
    % Make the `.init' file for a library containing the given modules.
    %
:- pred make_library_init_file(globals::in, io.text_output_stream::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

    % make_init_obj_file(ProgressStream, Globals, MustCompile,
    %   MainModuleName, AllModuleNames, MaybeInitObjFileName, !IO):
    %
:- pred make_init_obj_file(io.text_output_stream::in, globals::in, bool::in,
    module_name::in, list(module_name)::in, maybe(file_name)::out,
    io::di, io::uo) is det.

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
    % from a call to `get_executable_object_code_type'. In particular,
    % on architectures for which no special handling for PIC is necessary,
    % only a value of `non_pic' should be used.
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
% Used for standalone interfaces.
%

    % make_standalone_interface(Globals, ProgressStream, BaseName, !IO):
    %
    % Create a standalone interface in the current directory.
    %
:- pred make_standalone_interface(globals::in, io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Return the C compiler flags.
    %
    % This predicate is used to implement the `--output-cflags' option.
    %
:- pred get_c_compiler_flags(globals::in, string::out) is det.

    % Return the C compiler flags that define the macros used to specify the
    % current compilation grade to the given stream.
    %
    % This predicate is used to implement the `--output-grade-defines' option.
    %
:- pred get_c_grade_defines(globals::in, string::out) is det.

    % Return the C compiler flags that specify where the C compiler should
    % search for header files.
    %
    % This predicate is used to implement the `--output-c-include-dir-flags'
    % option.
    %
:- pred get_c_include_dir_flags(globals::in, string::out) is det.

:- pred get_framework_directories_flags(globals::in, string::out) is det.

%---------------------------------------------------------------------------%
%
% XXX These two predicates belong elsewhere, in a module dedicated
% to utility operations for constructing command lines.
%

    % join_string_list(Strings, Prefix, Suffix, Separator, Result):
    %
    % Appends the strings in the list `Strings' together into the string
    % Result. Each string is prefixed by Prefix, suffixed by Suffix and
    % separated by Separator.
    %
:- pred join_string_list(list(string)::in, string::in, string::in, string::in,
    string::out) is det.

    % As above, but quote the strings first. Note that the strings in values
    % of the *flags options are already quoted.
    %
:- pred join_quoted_string_list(list(string)::in, string::in, string::in,
    string::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.trace_params.
:- import_module parse_tree.module_cmds.

:- import_module dir.
:- import_module io.file.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

get_executable_object_code_type(_Globals, PIC) :-
    PIC = non_pic.

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
    gather_specific_c_compiler_flags(Globals, CC_Specific_CFLAGS),

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

    get_c_include_dir_flags(Globals, InclOpt),
    get_framework_directories_flags(Globals, FrameworkInclOpt),
    get_c_grade_defines(Globals, GradeDefinesOpts),

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

:- pred gather_specific_c_compiler_flags(globals::in, string::out) is det.

gather_specific_c_compiler_flags(Globals, Flags) :-
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
        join_string_list(JRT_JavaFlagsList, "", "", " ", NonAtFileJAVAFLAGS),
        join_string_list(NonJRT_JavaFlagsList, "", "", " ", JAVAFLAGS)
    ;
        RestrictedCommandLine = no,
        join_string_list(JavaFlagsList, "", "", " ", JAVAFLAGS),
        NonAtFileJAVAFLAGS = ""
    ),

    get_mercury_std_libs_for_java(Globals, MercuryStdLibs),
    globals.lookup_accumulating_option(Globals, java_classpath, UserClasspath),
    JavaInclDirs = MercuryStdLibs ++ UserClasspath,
    % We prepend the current CLASSPATH (if any) to preserve the accumulating
    % nature of this variable.
    get_env_classpath(EnvClasspath, !IO),
    ( if EnvClasspath = "" then
        ClassPathList = JavaInclDirs
    else
        ClassPathList = [EnvClasspath | JavaInclDirs]
    ),
    ClassPath = string.join_list(java_classpath_separator, ClassPathList),
    ( if ClassPath = "" then
        InclOpts = ""
    else
        InclOpts = "-classpath " ++ quote_shell_cmd_arg(ClassPath) ++ " "
    ),

    globals.lookup_bool_option(Globals, target_debug, TargetDebug),
    (
        TargetDebug = yes,
        TargetDebugOpts = "-g "
    ;
        TargetDebug = no,
        TargetDebugOpts = ""
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
        DirOpts = "-sourcepath " ++ SourceDirName ++ " " ++
            "-d " ++ DestDirName ++ " "
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
    string.append_list([InclOpts, DirOpts,
        TargetDebugOpts, JAVAFLAGS, " ", JoinedJavaFiles], CommandArgs),
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
        PathSeparator = ";" % semicolon
    else
        PathSeparator = ":" % colon
    ).

:- pred is_minus_j_flag(string::in) is semidet.

is_minus_j_flag(FlagStr) :-
    string.prefix(FlagStr, "-J").

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

% WARNING: The code here duplicates the functionality of scripts/c2init.in.
% Any changes there may also require changes here, and vice versa.
% The code of make_standalone_interface/5 may also require updating.

make_init_obj_file(ProgressStream, Globals, MustCompile,
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

    get_executable_object_code_type(Globals, PIC),
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

    globals.lookup_bool_option(Globals, extra_init_functions, ExtraInits),
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
        UpperBaseName = string.to_upper(BaseName),
        io.write_strings(HdrFileStream, [
            "#ifndef ", UpperBaseName, "_H\n",
            "#define ", UpperBaseName, "_H\n",
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
            "#endif /* ", UpperBaseName, "_H */\n"],
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
        get_executable_object_code_type(Globals, PIC),
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

get_c_compiler_flags(Globals, CFlags) :-
    get_executable_object_code_type(Globals, PIC),
    gather_c_compiler_flags(Globals, PIC, CFlags).

%---------------------%

get_c_grade_defines(Globals, GradeDefines) :-
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
    globals.lookup_bool_option(Globals, target_debug_grade, TargetDebugGrade),
    (
        TargetDebugGrade = yes,
        % This grade option tells the C compiler to turn on the generation
        % of debugging symbols and to disable the optimizations that
        % would make the executable harder to debug in a C debugger
        % such as gdb. However, here we gather only *macro* definitions,
        % not general compiler flags.
        TargetDebugGradeOpt = "-DMR_TARGET_DEBUG_GRADE "
    ;
        TargetDebugGrade = no,
        TargetDebugGradeOpt = ""
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
        TargetDebugGradeOpt, DeclDebugOpt,
        SourceDebugOpt,
        ExecTraceOpt,
        UseTrailOpt,
        MinimalModelOpt,
        PregeneratedDistOpt,
        SinglePrecFloatOpt,
        UseRegionsOpt], GradeDefines).

%---------------------%

get_c_include_dir_flags(Globals, InclOpts) :-
    globals.lookup_accumulating_option(Globals, c_include_directories,
        CInclDirs),
    InclOpts = string.append_list(list.condense(list.map(
        (func(CIncl) = ["-I", quote_shell_cmd_arg(CIncl), " "]),
        CInclDirs))).

%---------------------%

get_framework_directories_flags(Globals, FrameworkDirOpts) :-
    globals.lookup_accumulating_option(Globals, framework_directories,
        FrameworkDirs0),
    join_quoted_string_list(FrameworkDirs0, "-F", "", " ", FrameworkDirOpts).

%---------------------------------------------------------------------------%

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

join_quoted_string_list(Strings, Prefix, Suffix, Separator, Result) :-
    QuotedStrings = list.map(quote_shell_cmd_arg, Strings),
    join_string_list(QuotedStrings, Prefix, Suffix, Separator, Result).

%---------------------------------------------------------------------------%
:- end_module backend_libs.compile_target_code.
%---------------------------------------------------------------------------%
