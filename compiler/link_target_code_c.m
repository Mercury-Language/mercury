%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: link_target_code_c.m.
%
% Code to create executables, shared objects and static libraries
% when targeting C.
%
%---------------------------------------------------------------------------%

:- module backend_libs.link_target_code_c.
:- interface.

:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.link_target_util.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%
%
% Section one: creating executables and shared libraries.
%

    % link_modules_into_executable_or_shared_library_for_c(ProgressStream,
    %   Globals, ModulesToLink, ExtraObjFileNames, Specs, Succeeded, !IO):
    %
    % Link the (PIC or non-PIC) object files of the given modules
    % into either an executable or a shared library (depending on the value
    % of the compile_to_shared_lib option). Name the executable or shared
    % library file after the value of the output_file_name option, if it is
    % specified, or if it not, then after the first module in ModulesToLink.
    %
    % This predicate is called mercury_compile_main.m during compiler
    % invocations which do NOT use "mmc --make".
    %
:- pred link_modules_into_executable_or_shared_library_for_c(
    io.text_output_stream::in, globals::in,
    list(module_name)::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

:- pred create_exe_or_shared_lib_for_c(io.text_output_stream::in, globals::in,
    linked_target_type::in(c_exe_or_shared_lib),
    module_name::in, file_name::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Section two: creating static libraries.
%

:- pred create_static_lib_for_c(io.text_output_stream::in, globals::in,
    file_name::in, bool::in, list(file_name)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Section three: link flags.
%
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
:- pred output_library_link_flags_for_c(globals::in, io.text_output_stream::in,
    list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Section four: minor utility predicates.
%

    % get_linked_target_type_for_c(Globals, LinkedTargetType):
    %
    % Work out whether we should be generating an executable or a shared
    % object.
    %
:- pred get_linked_target_type_for_c(globals::in,
    linked_target_type::out(c_exe_or_shared_lib)) is det.

%---------------------%

    % get_object_code_type(Globals, LinkedTargetType, PIC):
    %
    % Work out whether we should be generating position-independent
    % object code.
    %
:- pred get_object_code_type(globals::in, linked_target_type::in, pic::out)
    is det.

%---------------------%

:- type shared_library_support
    --->    shared_libraries_not_supported
    ;       shared_libraries_supported.

    % are_shared_libraries_supported(Globals, SharedLibsSupported)
    %
    % Return whether or not shared libraries are supported on the current
    % platform.
    %
:- pred are_shared_libraries_supported(globals::in,
    shared_library_support::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.trace_params.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.

:- import_module dir.
:- import_module io.file.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Section one.
%

link_modules_into_executable_or_shared_library_for_c(ProgressStream, Globals,
        ModuleNames, ExtraObjFileNames, Specs, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, output_file_name, OutputFileName),
    ( if OutputFileName = "" then
        list.det_head(ModuleNames, MainModuleName)
    else
        file_name_to_module_name(OutputFileName, MainModuleName)
    ),

    get_linked_target_type_for_c(Globals, LinkedTargetType),
    get_object_code_type(Globals, LinkedTargetType, PIC),
    maybe_pic_object_file_extension(PIC, ObjExt, _),
    % XXX LEGACY
    list.map2(
        module_name_to_file_name(Globals, $pred, ext_cur_ngs_gas(ObjExt)),
        ModuleNames, ModuleObjectFileNames, _ModuleObjectFileNamesProposed),
    globals.lookup_accumulating_option(Globals, link_objects,
        ExtraLinkFileNames),
    AllNonInitObjectFileNames =
        ModuleObjectFileNames ++ ExtraLinkFileNames ++ ExtraObjFileNames,
    (
        LinkedTargetType = executable,
        MustCompile = yes,
        make_init_obj_file(ProgressStream, Globals, MustCompile,
            MainModuleName, ModuleNames, InitObjResult, !IO),
        (
            InitObjResult = yes(InitObjFileName),
            AllObjectFileNames = [InitObjFileName | AllNonInitObjectFileNames],
            MaybeFilesToLink = yes(AllObjectFileNames)
        ;
            InitObjResult = no,
            MaybeFilesToLink = no
        )
    ;
        LinkedTargetType = shared_library,
        MaybeFilesToLink = yes(AllNonInitObjectFileNames)
    ),
    (
        MaybeFilesToLink = no,
        Specs = [],
        Succeeded = did_not_succeed
    ;
        MaybeFilesToLink = yes(FilesToLink),
        % The following is effectively a copy of
        % link_files_into_executable_or_library_for_c_cs_java
        % that is specialized for LinkedTargetType being either
        % executable or shared_library.
        pre_link_msg(ProgressStream, Globals, !IO),
        linked_target_file_name_full_curdir(Globals, MainModuleName,
            LinkedTargetType, FullOutputFileName, CurDirOutputFileName, !IO),
        create_exe_or_shared_lib_for_c(ProgressStream, Globals,
            LinkedTargetType, MainModuleName, FullOutputFileName, FilesToLink,
            Specs, LinkSucceeded, !IO),
        post_link_msg(ProgressStream, Globals, !IO),
        post_link_maybe_make_symlink(ProgressStream, Globals,
            LinkedTargetType, MainModuleName,
            FullOutputFileName, CurDirOutputFileName,
            LinkSucceeded, Succeeded, !IO)
    ).

%---------------------------------------------------------------------------%

create_exe_or_shared_lib_for_c(ProgressStream, Globals, LinkedTargetType,
        ModuleName, FullOutputFileName, ObjectsList, Specs, Succeeded, !IO) :-
    (
        LinkedTargetType = shared_library,
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
            globals.lookup_string_option(Globals, linker_error_undefined_flag,
                UndefOpt)
        ),
        ReserveStackSizeOpt = ""
    ;
        LinkedTargetType = executable,
        CommandOpt = link_executable_command,
        RpathFlagOpt = linker_rpath_flag,
        RpathSepOpt = linker_rpath_separator,
        LDFlagsOpt = ld_flags,
        ThreadFlagsOpt = linker_thread_flags,
        DebugFlagsOpt = linker_debug_flags,
        TraceFlagsOpt = linker_trace_flags,
        UndefOpt = "",
        ReserveStackSizeOpt = get_reserve_stack_size_flags_for_c(Globals)
    ),

    globals.lookup_string_option(Globals, linker_lto_flags, LTOOpts),

    % Should the executable be stripped?
    get_strip_flags_for_c(Globals, LinkedTargetType,
        LinkerStripOpt, StripExeCommand, StripExeFlags),

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
    globals.get_linkage(Globals, Linkage),
    ( if
        LinkedTargetType = executable,
        Linkage = sos_static
    then
        globals.lookup_string_option(Globals, linker_static_flags, StaticOpts)
    else
        StaticOpts = ""
    ),

    % Are the thread libraries needed?
    get_thread_flags_for_c(Globals, ThreadFlagsOpt, Linkage,
        ThreadOpts, HwlocOpts),

    % Find the Mercury standard libraries.
    get_mercury_std_libs_for_c(Globals, LinkedTargetType, MercuryStdLibs),

    % Find which system libraries are needed.
    get_system_libs_for_c(Globals, LinkedTargetType, SystemLibs),

    % With --restricted-command-line we may need to some additional
    % options to the linker. (For details, see the predicate comment for
    % the callee.
    get_restricted_command_line_link_opts_for_c(Globals, LinkedTargetType,
        RestrictedCmdLinkOpts),

    globals.lookup_accumulating_option(Globals, LDFlagsOpt, LDFlagsList),
    join_string_list(LDFlagsList, "", "", " ", LDFlags),
    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    globals.lookup_string_option(Globals, linker_path_flag, LinkerPathFlag),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),

    % Set up the runtime library path.
    get_runtime_library_path_opts_for_c(Globals, LinkedTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts),

    % Set up any framework search paths.
    get_framework_directories_flags(Globals, FrameworkDirectoriesOpts),

    % Set up the install name for shared libraries.
    get_install_name_opt_for_c(Globals, ModuleName, LinkedTargetType,
        InstallNameOpt),

    globals.get_trace_level(Globals, TraceLevel),
    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    (
        TraceEnabled = exec_trace_is_not_enabled,
        TraceOpts = ""
    ;
        TraceEnabled = exec_trace_is_enabled,
        globals.lookup_string_option(Globals, TraceFlagsOpt, TraceOpts)
    ),

    globals.lookup_accumulating_option(Globals, frameworks, Frameworks),
    join_quoted_string_list(Frameworks, "-framework ", "", " ", FrameworkOpts),

    get_link_opts_for_libraries_for_c(Globals, MaybeLinkLibraries, Specs, !IO),
    (
        MaybeLinkLibraries = maybe.yes(LinkLibrariesList),
        join_quoted_string_list(LinkLibrariesList, "", "", " ",
            LinkLibraries),
        prepare_for_link_exe_or_shared_lib_cmd_for_c(ProgressStream, Globals,
            ObjectsList, PrepareResult, !IO),
        (
            PrepareResult = prepare_succeeded(Objects, MaybeDemangleCmd,
                MaybeFileToDelete),

            % Note that LDFlags may contain `-l' options so it should come
            % after Objects.
            globals.lookup_string_option(Globals, CommandOpt, Command),
            get_linker_output_option_for_c(Globals, LinkedTargetType,
                OutputOpt),
            globals.lookup_string_option(Globals, linker_opt_separator,
                LinkOptSep),
            % XXX At least one (FrameworkDirectoriesOpts) and probably
            % others of the option strings being appended here
            % already contain a final space, so adding another is redundant,
            % and makes the command *less idiomatic* for any human readers.
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
                FrameworkDirectoriesOpts, " ",
                InstallNameOpt, " ",
                DebugOpts, " ",
                SanitizerOpts, " ",
                FrameworkOpts, " ",
                RestrictedCmdLinkOpts, " ",
                LDFlags, " ",
                LinkLibraries, " ",
                MercuryStdLibs, " ",
                HwlocOpts, " ",
                SystemLibs], LinkCmd),
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
            ),
            (
                MaybeFileToDelete = maybe.yes(FileToDelete),
                io.file.remove_file(FileToDelete, _, !IO)
            ;
                MaybeFileToDelete = maybe.no
            )
        ;
            PrepareResult = prepare_failed,
            Succeeded = did_not_succeed
        )
    ;
        MaybeLinkLibraries = maybe.no,
        Succeeded = did_not_succeed
    ).

%---------------------%

:- func get_reserve_stack_size_flags_for_c(globals) = string.

get_reserve_stack_size_flags_for_c(Globals) = Flags :-
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
            ; C_CompilerType = cc_cl_arm64(_)
            ),
            string.format("-stack:%d", [i(ReserveStackSize)], Flags)
        )
    ).

%---------------------%

:- pred get_strip_flags_for_c(globals::in, linked_target_type::in,
    string::out, string::out, string::out) is det.

get_strip_flags_for_c(Globals, LinkedTargetType,
        LinkerStripOpt, StripExeCommand, StripExeFlags) :-
    % Should the executable be stripped?
    globals.lookup_bool_option(Globals, strip, Strip),
    ( if
        LinkedTargetType = executable,
        Strip = yes
    then
        globals.lookup_string_option(Globals, linker_strip_flag,
            LinkerStripOpt),
        globals.lookup_string_option(Globals, strip_executable_command,
            StripExeCommand),
        globals.get_mercury_linkage(Globals, MercuryLinkage),
        (
            MercuryLinkage = sos_shared,
            StripExeFlagsOpt = strip_executable_shared_flags
        ;
            MercuryLinkage = sos_static,
            StripExeFlagsOpt = strip_executable_static_flags
        ),
        globals.lookup_string_option(Globals, StripExeFlagsOpt, StripExeFlags)
    else
        LinkerStripOpt = "",
        StripExeCommand = "",
        StripExeFlags = ""
    ).

%---------------------%

:- pred get_thread_flags_for_c(globals::in, option::in, static_or_shared::in,
    string::out, string::out) is det.

get_thread_flags_for_c(Globals, ThreadFlagsOpt, Linkage,
        ThreadOpts, HwlocOpts) :-
    use_thread_libs_for_c(Globals, UseThreadLibs),
    (
        UseThreadLibs = bool.yes,
        globals.lookup_string_option(Globals, ThreadFlagsOpt, ThreadOpts),

        % Determine which options are needed to link to libhwloc.
        % If libhwloc is not used (XXX by whom), then the string option
        % (XXX which string option?) will be empty.
        (
            Linkage = sos_shared,
            HwlocFlagsOpt = hwloc_libs
        ;
            Linkage = sos_static,
            HwlocFlagsOpt = hwloc_static_libs
        ),
        globals.lookup_string_option(Globals, HwlocFlagsOpt, HwlocOpts)
    ;
        UseThreadLibs = bool.no,
        ThreadOpts = "",
        HwlocOpts = ""
    ).

%---------------------%

    % When using --restricted-command-line with Visual C, we add all
    % the object files to a temporary archive before linking an executable.
    % However, if only .lib files are given on the command line, then
    % the linker needs to be manually told some details that it usually infers
    % from the object files, for example the program entry point and the
    % target machine type. link.exe will print a warning (#4001) when this
    % occurs, so we also need to shut that up.
    %
:- pred get_restricted_command_line_link_opts_for_c(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_restricted_command_line_link_opts_for_c(Globals, LinkedTargetType,
        RestrictedCmdLinkOpts) :-
    globals.lookup_bool_option(Globals, restricted_command_line,
        RestrictedCommandLine),
    (
        RestrictedCommandLine = yes,
        (
            LinkedTargetType = executable,
            get_c_compiler_type(Globals, C_CompilerType),
            (
                C_CompilerType = cc_cl_x86(_),
                RestrictedCmdLinkFlags = [
                    "-nologo",
                    "-ignore:4001",
                    "-subsystem:console",
                    "-machine:x86",
                    "-entry:wmainCRTStartup",
                    "-defaultlib:libcmt"
                ],
                join_string_list(RestrictedCmdLinkFlags, "", "", " ",
                    RestrictedCmdLinkOpts)
            ;
                C_CompilerType = cc_cl_x64(_),
                RestrictedCmdLinkFlags = [
                    "-nologo",
                    "-ignore:4001",
                    "-subsystem:console",
                    "-machine:x64",
                    "-entry:wmainCRTStartup",
                    "-defaultlib:libcmt"
                ],
                join_string_list(RestrictedCmdLinkFlags, "", "", " ",
                    RestrictedCmdLinkOpts)
            ;
                C_CompilerType = cc_cl_arm64(_),
                RestrictedCmdLinkFlags = [
                    "-nologo",
                    "-ignore:4001",
                    "-subsystem:console",
                    "-machine:arm64",
                    "-entry:wmainCRTStartup",
                    "-defaultlib:libcmt"
                ],
                join_string_list(RestrictedCmdLinkFlags, "", "", " ",
                    RestrictedCmdLinkOpts)
            ;
                ( C_CompilerType = cc_gcc(_, _, _)
                ; C_CompilerType = cc_clang(_)
                ; C_CompilerType = cc_unknown
                ),
                RestrictedCmdLinkOpts = ""
            )
        ;
            LinkedTargetType = shared_library,
            RestrictedCmdLinkOpts = ""
        )
    ;
        RestrictedCommandLine = no,
        RestrictedCmdLinkOpts = ""
    ).

%---------------------%

:- pred get_install_name_opt_for_c(globals::in, module_name::in,
    linked_target_type::in, string::out) is det.

get_install_name_opt_for_c(Globals, ModuleName, LinkedTargetType,
        InstallNameOpt) :-
    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    ( if
        UseInstallName = bool.yes,
        LinkedTargetType = shared_library
    then
        % NOTE: ShLibFileName must *not* be prefixed with a directory.
        % The call to get_install_name_option below will prefix it
        % with the correct directory, which is the one where the library
        % is going to be installed, *not* where it is going to be built.
        BaseFileName = sym_name_to_string(ModuleName),
        globals.lookup_string_option(Globals, shared_library_extension,
            SharedLibExt),
        ShLibFileName = "lib" ++ BaseFileName ++ SharedLibExt,
        get_install_name_option(Globals, ShLibFileName, InstallNameOpt)
    else
        InstallNameOpt = ""
    ).

%---------------------%

:- pred get_linker_output_option_for_c(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_linker_output_option_for_c(Globals, LinkedTargetType, OutputOpt) :-
    get_c_compiler_type(Globals, C_CompilerType),
    % XXX we should allow the user to override the compiler's choice of
    % output option here.
    % NOTE: the spacing around the value of OutputOpt here is important.
    % Any changes should be reflected in predicate link_exe_or_shared_lib/9.
    (
        ( C_CompilerType = cc_cl_x86(_)
        ; C_CompilerType = cc_cl_x64(_)
        ; C_CompilerType = cc_cl_arm64(_)
        ),
        ( if LinkedTargetType = executable then
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

%---------------------%

:- type prepare_to_link_result
    --->    prepare_succeeded(string, maybe(string), maybe(string))
            % Objects, MaybeDemangleCmd, MaybeTmpArchiveToDelete
    ;       prepare_failed.
            % Any error messages have already been printed.

:- pred prepare_for_link_exe_or_shared_lib_cmd_for_c(io.text_output_stream::in,
    globals::in, list(string)::in,
    prepare_to_link_result::out, io::di, io::uo) is det.

prepare_for_link_exe_or_shared_lib_cmd_for_c(ProgressStream, Globals,
        ObjectsList, PrepareResult, !IO) :-
    globals.lookup_bool_option(Globals, demangle, Demangle),
    (
        Demangle = yes,
        globals.lookup_string_option(Globals, demangle_command, DemangleCmd),
        MaybeDemangleCmd = yes(DemangleCmd)
    ;
        Demangle = no,
        MaybeDemangleCmd = no
    ),
    globals.lookup_bool_option(Globals, restricted_command_line,
        RestrictedCommandLine),
    (
        % If we have a restricted command line, then it is possible
        % that following link command will call sub-commands itself
        % and thus overflow the command line, so in this case
        % we first create an archive of all of the object files.
        % XXX Can someone who knows the actual reason for
        % restricted_command_line please clarify the above sentence?
        RestrictedCommandLine = yes,
        globals.lookup_string_option(Globals, library_extension, LibExt),
        io.file.get_temp_directory(TempDir, !IO),
        io.file.make_temp_file(TempDir, "mtmp", LibExt,
            TmpArchiveResult, !IO),
        (
            TmpArchiveResult = ok(TmpArchive),
            % Only include actual object files in the temporary archive,
            % not other files such as other archives.
            filter_object_files_for_c(Globals, ObjectsList,
                ProperObjectFiles, NonObjectFiles),
            % Delete the currently empty output file first, otherwise ar
            % will fail to recognise its file format.
            io.file.remove_file(TmpArchive, _, !IO),
            create_static_lib_for_c(ProgressStream, Globals, TmpArchive, yes,
                ProperObjectFiles, ArchiveSucceeded, !IO),
            (
                ArchiveSucceeded = succeeded,
                join_quoted_string_list([TmpArchive | NonObjectFiles],
                    "", "", " ", Objects),
                MaybeTmpArchiveToDelete = yes(TmpArchive),
                PrepareResult = prepare_succeeded(Objects, MaybeDemangleCmd,
                    MaybeTmpArchiveToDelete)
            ;
                ArchiveSucceeded = did_not_succeed,
                io.file.remove_file(TmpArchive, _, !IO),
                PrepareResult = prepare_failed
            )
        ;
            TmpArchiveResult = error(Error),
            io.format(ProgressStream,
                "Could not create temporary file: %s\n",
                [s(error_message(Error))], !IO),
            PrepareResult = prepare_failed
        )
    ;
        RestrictedCommandLine = no,
        join_quoted_string_list(ObjectsList, "", "", " ", Objects),
        MaybeTmpArchiveToDelete = no,
        PrepareResult = prepare_succeeded(Objects, MaybeDemangleCmd,
            MaybeTmpArchiveToDelete)
    ).

%---------------------%

    % Filter list of files into those with and without known object file
    % extensions.
    %
:- pred filter_object_files_for_c(globals::in, list(string)::in,
    list(string)::out, list(string)::out) is det.

filter_object_files_for_c(Globals, Files, ObjectFiles, NonObjectFiles) :-
    globals.lookup_string_option(Globals, object_file_extension, ObjExt),
    globals.lookup_string_option(Globals, pic_object_file_extension,
        PicObjExt),
    list.filter(has_object_file_extension_for_c(ObjExt, PicObjExt), Files,
        ObjectFiles, NonObjectFiles).

:- pred has_object_file_extension_for_c(string::in, string::in, string::in)
    is semidet.

has_object_file_extension_for_c(ObjExt, PicObjExt, FileName) :-
    ( string.suffix(FileName, ObjExt)
    ; string.suffix(FileName, PicObjExt)
    ).

%---------------------------------------------------------------------------%
%
% Section two.
%

create_static_lib_for_c(ProgressStream, Globals, FullLibFileName, Quote,
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
        ; C_CompilerType = cc_cl_arm64(_)
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

%---------------------------------------------------------------------------%
%
% Utility predicates needed for creating more than one kind of linked target.
%

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Library link flags.
%

output_library_link_flags_for_c(Globals, Stream, Specs, !IO) :-
    % We output the library link flags as they are for when we are linking
    % an executable.
    LinkedTargetType = executable,
    RpathFlagOpt = linker_rpath_flag,
    RpathSepOpt = linker_rpath_separator,

    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    globals.lookup_string_option(Globals, linker_path_flag, LinkerPathFlag),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),
    get_runtime_library_path_opts_for_c(Globals, LinkedTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts),
    get_link_opts_for_libraries_for_c(Globals, MaybeLinkLibraries,
        Specs, !IO),
    (
        MaybeLinkLibraries = yes(LinkLibrariesList),
        join_string_list(LinkLibrariesList, "", "", " ", LinkLibraries)
    ;
        MaybeLinkLibraries = no,
        LinkLibraries = ""
    ),
    % Find the Mercury standard libraries.
    get_mercury_std_libs_for_c(Globals, LinkedTargetType, MercuryStdLibs),
    get_system_libs_for_c(Globals, LinkedTargetType, SystemLibs),
    io.format(Stream, "%s %s %s %s %s\n",
        [s(LinkLibraryDirectories), s(RpathOpts), s(LinkLibraries),
        s(MercuryStdLibs), s(SystemLibs)], !IO).

:- pred get_system_libs_for_c(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_system_libs_for_c(Globals, LinkedTargetType, SystemLibs) :-
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
    use_thread_libs_for_c(Globals, UseThreadLibs),
    (
        UseThreadLibs = yes,
        globals.lookup_string_option(Globals, thread_libs, ThreadLibs)
    ;
        UseThreadLibs = no,
        ThreadLibs = ""
    ),
    % Other system libraries.
    (
        LinkedTargetType = shared_library,
        globals.lookup_string_option(Globals, shared_libs, OtherSystemLibs)
    ;
        LinkedTargetType = executable,
        globals.lookup_string_option(Globals, math_lib, OtherSystemLibs)
    ),
    SystemLibs = string.join_list(" ",
        [SystemTraceLibs, OtherSystemLibs, ThreadLibs]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Predicates that are needed by more than one of the section one,
% two and three.
%

    % Find the standard Mercury libraries, and the system
    % libraries needed by them.
    % Return the empty string if --mercury-standard-library-directory
    % is not set.
    % NOTE: This predicate and get_mercury_std_libs_for_{cs,java} do
    % quite similar jobs; changes to one may require changes to the others.
    %
:- pred get_mercury_std_libs_for_c(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_mercury_std_libs_for_c(Globals, LinkedTargetType, StdLibs) :-
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_gc_method(Globals, GCMethod),
        globals.get_grade_dir(Globals, GradeDir),
        LibExt = ext_cur_gas(ext_cur_gas_lib_lib_opt),

        % GC libraries.
        (
            GCMethod = gc_automatic,
            unexpected($pred, "gc_automatic")
        ;
            GCMethod = gc_none,
            StaticGCLibs = "",
            SharedGCLibs = ""
        ;
            GCMethod = gc_hgc,
            % We always compile with hgc since it's home-grown and very small.
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
            globals.lookup_bool_option(Globals, target_debug_grade,
                TargetDebugGrade),
            (
                TargetDebugGrade = yes,
                GCGrade1 = GCGrade0 ++ "_target_debug"
            ;
                TargetDebugGrade = no,
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
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir, "",
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
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_trace", StaticTraceLib, TraceLib),
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_eventspec", StaticEventSpecLib,
                EventSpecLib),
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir,
                GradeDir,
                LibExt, "mer_browser", StaticBrowserLib, BrowserLib),
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_mdbcomp", StaticMdbCompLib, MdbCompLib),
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
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_ssdb", StaticSsdbLib, SsdbLib),
            link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_browser",
                StaticBrowserLib2, BrowserLib2),
            link_lib_args_for_c(Globals, LinkedTargetType,
                StdLibDir, GradeDir, LibExt, "mer_mdbcomp",
                StaticMdbCompLib2, MdbCompLib2),
            StaticSourceDebugLibs = string.join_list(" ",
                [StaticSsdbLib, StaticBrowserLib2, StaticMdbCompLib2]),
            SharedSourceDebugLibs = string.join_list(" ",
                [SsdbLib, BrowserLib2, MdbCompLib2])
        ;
            SourceDebug = no,
            StaticSourceDebugLibs = "",
            SharedSourceDebugLibs = ""
        ),

        link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir, GradeDir,
            LibExt, "mer_std", StaticStdLib, StdLib),
        link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir, GradeDir,
            LibExt, "mer_rt", StaticRuntimeLib, RuntimeLib),
        globals.get_mercury_linkage(Globals, MercuryLinkage),
        (
            MercuryLinkage = sos_static,
            StdLibs = string.join_list(" ", [
                StaticTraceLibs,
                StaticSourceDebugLibs,
                StaticStdLib,
                StaticRuntimeLib,
                StaticGCLibs
            ])
        ;
            MercuryLinkage = sos_shared,
            StdLibs = string.join_list(" ", [
                SharedTraceLibs,
                SharedSourceDebugLibs,
                StdLib,
                RuntimeLib,
                SharedGCLibs
            ])
        )
    ;
        MaybeStdLibDir = no,
        StdLibs = ""
    ).

%---------------------%

:- pred use_thread_libs_for_c(globals::in, bool::out) is det.

use_thread_libs_for_c(Globals, UseThreadLibs) :-
    globals.lookup_bool_option(Globals, parallel, UseThreadLibs).

%---------------------%

:- pred link_lib_args_for_c(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::in,
    string::in, ext::in, string::in, string::out, string::out) is det.

link_lib_args_for_c(Globals, LinkedTargetType, StdLibDir, GradeDir, Ext,
        Name, StaticArg, SharedArg) :-
    StaticLibName = "lib" ++ Name ++ extension_to_string(Globals, Ext),
    StaticArg = quote_shell_cmd_arg(StdLibDir/"lib"/GradeDir/StaticLibName),
    make_link_lib_arg_for_c(Globals, LinkedTargetType, Name, SharedArg).

:- pred make_link_lib_arg_for_c(globals::in,
    linked_target_type::in(c_exe_or_shared_lib),
    string::in, string::out) is det.

make_link_lib_arg_for_c(Globals, LinkedTargetType, LibName, LinkOpt) :-
    (
        LinkedTargetType = executable,
        LinkLibFlag = linker_link_lib_flag,
        LinkLibSuffix = linker_link_lib_suffix
    ;
        LinkedTargetType = shared_library,
        LinkLibFlag = shlib_linker_link_lib_flag,
        LinkLibSuffix = shlib_linker_link_lib_suffix
    ),
    globals.lookup_string_option(Globals, LinkLibFlag, LinkLibOpt),
    globals.lookup_string_option(Globals, LinkLibSuffix, Suffix),
    LinkOpt = quote_shell_cmd_arg(LinkLibOpt ++ LibName ++ Suffix).

%---------------------%

:- pred get_runtime_library_path_opts_for_c(globals::in,
    linked_target_type::in,
    option::in(bound(shlib_linker_rpath_flag ; linker_rpath_flag)),
    option::in(bound(shlib_linker_rpath_separator ; linker_rpath_separator)),
    string::out) is det.

get_runtime_library_path_opts_for_c(Globals, LinkedTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts) :-
    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    are_shared_libraries_supported(Globals, SharedLibsSupported),
    globals.get_linkage(Globals, Linkage),
    ( if
        UseInstallName = no,
        SharedLibsSupported = shared_libraries_supported,
        ( Linkage = sos_shared
        ; LinkedTargetType = shared_library
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

%---------------------%

    % Pass either `-llib' or `PREFIX/lib/GRADE/liblib.a', depending on
    % whether we are linking with static or shared Mercury libraries.
    %
:- pred get_link_opts_for_libraries_for_c(globals::in,
    maybe(list(string))::out, list(error_spec)::out, io::di, io::uo) is det.

get_link_opts_for_libraries_for_c(Globals, MaybeLinkLibOpts, Specs, !IO) :-
    globals.lookup_accumulating_option(Globals, link_libraries, LinkLibraries),
    list.map2_foldl(get_link_opts_for_library_for_c(Globals),
        LinkLibraries, LinkLibOpts, SpecsList, !IO),
    list.condense(SpecsList, Specs),
    (
        Specs = [],
        MaybeLinkLibOpts = yes(LinkLibOpts)
    ;
        Specs = [_ | _],
        MaybeLinkLibOpts = no
    ).

:- pred get_link_opts_for_library_for_c(globals::in, string::in,
    string::out, list(error_spec)::out, io::di, io::uo) is det.

get_link_opts_for_library_for_c(Globals, LibName, LinkLibOpt, Specs, !IO) :-
    globals.get_mercury_linkage(Globals, MercuryLinkage),
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
        ; CCompilerType = cc_cl_arm64(_)
        ),
        LinkOpt = "",
        LibSuffix = ".lib"
    ),

    globals.lookup_accumulating_option(Globals, mercury_libraries,
        MercuryLibs),
    ( if
        MercuryLinkage = sos_static,
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
            LinkLibOpt = DirName/LibFileName,
            Specs = []
        ;
            MaybeDirName = error(Error),
            LinkLibOpt = "",
            Pieces = [words(Error), suffix("."), nl],
            % XXX SEARCH_ERROR _SearchDirs
            Spec = no_ctxt_spec($pred, severity_error,
                phase_find_files(LibFileName, no), Pieces),
            Specs = [Spec]
        )
    else
        Specs = [],
        LinkLibOpt = LinkOpt ++ LibName ++ LibSuffix
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Section four.
%

get_linked_target_type_for_c(Globals, LinkedTargetType) :-
    globals.lookup_bool_option(Globals, shared_lib_not_executable,
        MakeSharedLib),
    ( MakeSharedLib = yes, LinkedTargetType = shared_library
    ; MakeSharedLib = no,  LinkedTargetType = executable
    ).

%---------------------%

get_object_code_type(Globals, FileType, PIC) :-
    (
        FileType = executable,
        get_executable_object_code_type(Globals, PIC)
    ;
        ( FileType = static_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        ),
        PIC = non_pic
    ;
        FileType = shared_library,
        globals.lookup_string_option(Globals, pic_object_file_extension,
            PicObjExt),
        globals.lookup_string_option(Globals, object_file_extension, ObjExt),
        PIC = ( if PicObjExt = ObjExt then non_pic else pic )
    ).

%---------------------%

are_shared_libraries_supported(Globals, Supported) :-
    % XXX This seems to be the standard way to check whether shared libraries
    % are supported, but it is not very nice.
    globals.lookup_string_option(Globals, library_extension, LibExt),
    globals.lookup_string_option(Globals, shared_library_extension,
        SharedLibExt),
    ( if LibExt = SharedLibExt then
        Supported = shared_libraries_not_supported
    else
        Supported = shared_libraries_supported
    ).

%---------------------------------------------------------------------------%
:- end_module backend_libs.link_target_code_c.
%---------------------------------------------------------------------------%
