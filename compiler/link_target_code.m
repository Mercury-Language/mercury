%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: link_target_code.m.
% Main authors: fjh, stayl.
%
% Code to create executables and libraries.
%
%---------------------------------------------------------------------------%

:- module backend_libs.link_target_code.
:- interface.

:- import_module backend_libs.compile_target_code.
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

%---------------------%

:- type linked_target_type
    --->    executable
    ;       static_library
    ;       shared_library
    ;       csharp_executable
    ;       csharp_library
    ;       java_executable
    ;       java_archive.

    % get_linked_target_type(Globals, LinkedTargetType):
    %
    % Work out whether we should be generating an executable or a shared
    % object.
    %
:- pred get_linked_target_type(globals::in, linked_target_type::out) is det.

    % get_object_code_type(Globals, LinkedTargetType, PIC):
    %
    % Work out whether we should be generating position-independent
    % object code.
    %
:- pred get_object_code_type(globals::in, linked_target_type::in, pic::out)
    is det.

%---------------------------------------------------------------------------%

:- pred linked_target_file_name_full_curdir(globals::in, module_name::in,
    linked_target_type::in, file_name::out, file_name::out,
    io::di, io::uo) is det.

%---------------------%

    % link_modules_into_executable_or_shared_library(ProgressStream, Globals,
    %   ModulesToLink, ExtraObjFileNames, Specs, Succeeded, !IO):
    %
    % Link the (PIC or non-PIC) object files of the given modules
    % into either an executable or a shared library (depending on the value
    % of the compile_to_shared_lib option). Name the executable or shared
    % library file after the value of the output_file_name option, if it is
    % specified, or if it not, the after the first module in ModulesToLink.
    %
    % This predicate is called mercury_compile_main.m during compiler
    % invocations which do NOT use "mmc --make". This means that it is used
    % only during compiler invocations where the target language is C.
    %
:- pred link_modules_into_executable_or_shared_library(
    io.text_output_stream::in, globals::in,
    list(module_name)::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

    % link_files_into_executable_or_library(ProgressStream, Globals,
    %   LinkedTargetType, MainModuleName, FilesToLink, Specs, Succeeded, !IO):
    %
    % Link the given list of object files or object-like files into an
    % executable or library of some kind, which should be named after
    % MainModuleName.
    %
    % When targeting C, FilesToLink should be PIC or non-PIC object files.
    % When targeting Java, FilesToLink should be .jar files.
    % When targeting C#, FilesToLink should be CIL .dll files.
    %
    % Unlike the predicate above, we take as argument FilesToLink, not
    % ModulesToLink. This both allows and requires our callers (which are
    % in make.module_target.m and make.program_target.m) to handle
    % the inclusion in FilesToLink of every file to be linked that is NOT
    % the object file or object-like file of a Mercury module. This includes
    % the program's init file, and such auxiliary files as those used to
    % implement fact tables.
    %
    % Likewise, this predicate requires our caller to decide whether
    % we should create an executable or a library, and if the latter,
    % what kind of library. They pass that info to us in LinkedTargetType.
    %
    % Likewise, this predicate requires our caller to decide the name
    % of that executable or library. They pass that info in MainModuleName.
    %
    % XXX Having the names differ in the ending being "shared_library" vs
    % just "library" is not very effective in describing the differemce
    % between the this predicate and the one above. However, one cannot call
    % the above the non-mmc-make version and this one the mmc-make version,
    % because this one is used to help implement the one above.
    %
    % XXX If we moved the code for making the above decisions to this module
    % from make.program_target.m, we could maybe factor out commonalities with
    % the other predicate above. We would still need to export something like
    % this to make.module_target.m for creating .dll files.
    %
:- pred link_files_into_executable_or_library(io.text_output_stream::in,
    globals::in, linked_target_type::in, module_name::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

%---------------------%

    % post_link_maybe_make_symlink_or_copy(Globals,
    %   ProgressStream, FullFileName, CurDirFileName,
    %   ModuleName, LinkedTargetType, Succeeded, MadeSymlinkOrCopy, !IO):
    %
    % If FullFileName, which results from converting MainModuleName
    % and LinkedTargetType to a fully specified relative pathname, differs from
    % CurDirFileName, which is the same filename in the current directory,
    % then link or copy FileName into the current directory after having
    % successfully built it, if the target does not exist or is not up-to-date.
    %
:- pred post_link_maybe_make_symlink_or_copy(globals::in,
    io.text_output_stream::in, file_name::in, file_name::in,
    module_name::in, linked_target_type::in, maybe_succeeded::out, bool::out,
    io::di, io::uo) is det.

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

:- import_module libs.options.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.trace_params.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.module_cmds.

:- import_module dir.
:- import_module io.file.
:- import_module maybe.
:- import_module require.
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
%---------------------------------------------------------------------------%

get_linked_target_type(Globals, LinkedTargetType) :-
    globals.lookup_bool_option(Globals, compile_to_shared_lib, MakeSharedLib),
    (
        MakeSharedLib = yes,
        LinkedTargetType = shared_library
    ;
        MakeSharedLib = no,
        LinkedTargetType = executable
    ).

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

linked_target_file_name_full_curdir(Globals, ModuleName, LinkedTargetType,
        FullFileName, CurDirFileName, !IO) :-
    % This code should be in file_names.m. The only reason why it is here
    % is that the linked_target_type type is defined here.
    (
        % Java archives and Java executables get the same filename.
        % XXX Then why make the distinction in linked_target_type?
        (
            LinkedTargetType = executable,
            Ext = ext_cur_gas(ext_cur_gas_exec_exec_opt)
        ;
            LinkedTargetType = csharp_executable,
            Ext = ext_cur_gas(ext_cur_gas_exec_exe)
        ;
            LinkedTargetType = csharp_library,
            Ext = ext_cur_gs(ext_cur_gs_lib_cil_dll)
        ;
            LinkedTargetType = java_archive,
            Ext = ext_cur_gs(ext_cur_gs_lib_jar)
        ;
            LinkedTargetType = java_executable,
            Ext = ext_cur_gs(ext_cur_gs_lib_jar)
        ),
        % XXX LEGACY
        module_name_to_file_name_full_curdir_create_dirs(Globals, $pred,
            Ext, ModuleName, FullFileName, _FullFileNameProposed,
            CurDirFileName, !IO)
    ;
        (
            LinkedTargetType = static_library,
            Ext = ext_cur_gas(ext_cur_gas_lib_lib_opt)
        ;
            LinkedTargetType = shared_library,
            Ext = ext_cur_gas(ext_cur_gas_lib_sh_lib_opt)
        ),
        % XXX LEGACY
        module_name_to_lib_file_name_full_curdir_create_dirs(Globals, $pred,
            "lib", Ext, ModuleName, FullFileName, _FullFileNameProposed,
            CurDirFileName, !IO)
    ).

%---------------------------------------------------------------------------%

link_modules_into_executable_or_shared_library(ProgressStream, Globals,
        ModuleNames, ExtraObjFileNames, Specs, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, output_file_name, OutputFileName),
    ( if OutputFileName = "" then
        list.det_head(ModuleNames, MainModuleName)
    else
        file_name_to_module_name(OutputFileName, MainModuleName)
    ),

    globals.lookup_bool_option(Globals, compile_to_shared_lib,
        CompileToSharedLib),
    ( CompileToSharedLib = yes, LinkedTargetType = shared_library
    ; CompileToSharedLib = no,  LinkedTargetType = executable
    ),
    get_object_code_type(Globals, LinkedTargetType, PIC),
    maybe_pic_object_file_extension(PIC, ObjExt, _),
    module_names_to_file_names(Globals, ext_cur_ngs_gas(ObjExt),
        ModuleNames, ModuleObjectFileNames),
    globals.lookup_accumulating_option(Globals, link_objects,
        ExtraLinkFileNames),
    AllNonInitObjectFileNames =
        ModuleObjectFileNames ++ ExtraLinkFileNames ++ ExtraObjFileNames,
    (
        LinkedTargetType = executable,
        MustCompile = yes,
        do_make_init_obj_file(Globals, ProgressStream, MustCompile,
            MainModuleName, ModuleNames, InitObjResult, !IO),
        (
            InitObjResult = yes(InitObjFileName),
            AllObjectFileNames = [InitObjFileName | AllNonInitObjectFileNames],
            link_files_into_executable_or_library(ProgressStream, Globals,
                LinkedTargetType, MainModuleName, AllObjectFileNames,
                Specs, Succeeded, !IO)
        ;
            InitObjResult = no,
            Specs = [],
            Succeeded = did_not_succeed
        )
    ;
        LinkedTargetType = shared_library,
        link_files_into_executable_or_library(ProgressStream, Globals,
            LinkedTargetType, MainModuleName, AllNonInitObjectFileNames,
            Specs, Succeeded, !IO)
    ).

    % module_names_to_file_names(Globals, Ext, ModuleNames, ExtFileNames):
    %
    % Convert each ModuleName to the name of the associated Ext file.
    %
:- pred module_names_to_file_names(globals::in, ext::in,
    list(module_name)::in, list(string)::out) is det.

module_names_to_file_names(_Globals, _Ext, [], []).
module_names_to_file_names(Globals, Ext,
        [ModuleName | ModuleNames], [ExtFileName | ExtFileNames]) :-
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred, Ext,
        ModuleName, ExtFileName, _ExtFileNameProposed),
    module_names_to_file_names(Globals, Ext, ModuleNames, ExtFileNames).

%---------------------------------------------------------------------------%

% WARNING: The code here duplicates the functionality of scripts/ml.in.
% Any changes there may also require changes here, and vice versa.

link_files_into_executable_or_library(ProgressStream, Globals,
        LinkedTargetType, ModuleName, FilesToLink, Specs, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(ProgressStream, Verbose, "% Linking...\n", !IO),
    linked_target_file_name_full_curdir(Globals, ModuleName, LinkedTargetType,
        FullOutputFileName, CurDirOutputFileName, !IO),
    (
        ( LinkedTargetType = executable
        ; LinkedTargetType = shared_library
        ),
        link_exe_or_shared_lib(Globals, ProgressStream,
            LinkedTargetType, ModuleName, FullOutputFileName, FilesToLink,
            Specs, LinkSucceeded, !IO)
    ;
        LinkedTargetType = static_library,
        create_archive(Globals, ProgressStream,
            FullOutputFileName, yes, FilesToLink, LinkSucceeded, !IO),
        Specs = []
    ;
        ( LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ),
        % XXX C# see also older predicate compile_csharp_file
        create_csharp_exe_or_lib(Globals, ProgressStream,
            LinkedTargetType, ModuleName, FullOutputFileName, FilesToLink,
            Specs, LinkSucceeded, !IO)
    ;
        ( LinkedTargetType = java_executable
        ; LinkedTargetType = java_archive
        ),
        create_java_exe_or_lib(Globals, ProgressStream,
            LinkedTargetType, ModuleName, FullOutputFileName, FilesToLink,
            LinkSucceeded, !IO),
        Specs = []
    ),
    maybe_report_stats(ProgressStream, Stats, !IO),
    (
        LinkSucceeded = succeeded,
        post_link_maybe_make_symlink_or_copy(Globals, ProgressStream,
            FullOutputFileName, CurDirOutputFileName,
            ModuleName, LinkedTargetType, Succeeded, _MadeSymlinkOrCopy, !IO)
    ;
        LinkSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% Linking for C executables and shared libraries.
%

:- pred link_exe_or_shared_lib(globals::in, io.text_output_stream::in,
    linked_target_type::in(c_exe_or_shared_lib),
    module_name::in, file_name::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

link_exe_or_shared_lib(Globals, ProgressStream, LinkedTargetType,
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
            globals.lookup_string_option(Globals,
                linker_error_undefined_flag, UndefOpt)
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
        ReserveStackSizeOpt = get_reserve_stack_size_flags(Globals)
    ),

    globals.lookup_string_option(Globals, linker_lto_flags, LTOOpts),

    % Should the executable be stripped?
    get_strip_flags(Globals, LinkedTargetType,
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
    globals.lookup_string_option(Globals, linkage, Linkage),
    ( if
        LinkedTargetType = executable,
        Linkage = "static"
    then
        globals.lookup_string_option(Globals, linker_static_flags, StaticOpts)
    else
        StaticOpts = ""
    ),

    % Are the thread libraries needed?
    get_thread_flags(Globals, ThreadFlagsOpt, Linkage, ThreadOpts, HwlocOpts),

    % Find the Mercury standard libraries.
    get_mercury_std_libs(Globals, LinkedTargetType, MercuryStdLibs),

    % Find which system libraries are needed.
    get_system_libs(Globals, LinkedTargetType, SystemLibs),

    % With --restricted-command-line we may need to some additional
    % options to the linker.
    % (See the comment above get_restricted_command_lin_link_opts/3 for
    % details.)
    get_restricted_command_line_link_opts(Globals, LinkedTargetType,
        RestrictedCmdLinkOpts),

    globals.lookup_accumulating_option(Globals, LDFlagsOpt, LDFlagsList),
    join_string_list(LDFlagsList, "", "", " ", LDFlags),
    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    globals.lookup_string_option(Globals, linker_path_flag, LinkerPathFlag),
    join_quoted_string_list(LinkLibraryDirectoriesList, LinkerPathFlag, "",
        " ", LinkLibraryDirectories),

    % Set up the runtime library path.
    get_runtime_library_path_opts(Globals, LinkedTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts),

    % Set up any framework search paths.
    get_framework_directories(Globals, FrameworkDirectories),

    % Set up the install name for shared libraries.
    get_install_name_opt(Globals, ModuleName, LinkedTargetType,
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

    get_link_opts_for_libraries(Globals, MaybeLinkLibraries, Specs, !IO),
    (
        MaybeLinkLibraries = maybe.yes(LinkLibrariesList),
        join_quoted_string_list(LinkLibrariesList, "", "", " ",
            LinkLibraries),
        prepare_for_link_exe_or_shared_lib_cmd(ProgressStream, Globals,
            ObjectsList, PrepareResult, !IO),
        (
            PrepareResult = prepare_succeeded(Objects, MaybeDemangleCmd,
                MaybeFileToDelete),

            % Note that LDFlags may contain `-l' options so it should come
            % after Objects.
            globals.lookup_string_option(Globals, CommandOpt, Command),
            get_linker_output_option(Globals, LinkedTargetType, OutputOpt),
            globals.lookup_string_option(Globals, linker_opt_separator,
                LinkOptSep),
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

:- func get_reserve_stack_size_flags(globals) = string.

get_reserve_stack_size_flags(Globals) = Flags :-
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

%---------------------%

:- pred get_strip_flags(globals::in, linked_target_type::in,
    string::out, string::out, string::out) is det.

get_strip_flags(Globals, LinkedTargetType,
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
    ).

%---------------------%

:- pred get_thread_flags(globals::in, option::in, string::in,
    string::out, string::out) is det.

get_thread_flags(Globals, ThreadFlagsOpt, Linkage, ThreadOpts, HwlocOpts) :-
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
    ).

%---------------------%

    % Find the standard Mercury libraries, and the system
    % libraries needed by them.
    % Return the empty string if --mercury-standard-library-directory
    % is not set.
    % NOTE: changes here may require changes to get_mercury_std_libs_for_java.
    %
:- pred get_mercury_std_libs(globals::in,
    linked_target_type::in(c_or_csharp_exe_or_lib), string::out) is det.

get_mercury_std_libs(Globals, LinkedTargetType, StdLibs) :-
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_gc_method(Globals, GCMethod),
        (
            ( LinkedTargetType = executable
            ; LinkedTargetType = shared_library
            ),
            LibExt = ext_cur_gas(ext_cur_gas_lib_lib_opt),
            globals.lookup_string_option(Globals, mercury_linkage,
                MercuryOrCsharpLinkage)
        ;
            ( LinkedTargetType = csharp_executable
            ; LinkedTargetType = csharp_library
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
            link_lib_args(Globals, LinkedTargetType, StdLibDir, "",
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
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
                LibExt, "mer_trace", StaticTraceLib, TraceLib),
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
                LibExt, "mer_eventspec", StaticEventSpecLib, EventSpecLib),
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
                LibExt, "mer_browser", StaticBrowserLib, BrowserLib),
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
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
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
                LibExt, "mer_ssdb", StaticSsdbLib, SsdbLib),
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
                LibExt, "mer_browser", StaticBrowserLib2, BrowserLib2),
            link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
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

        link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
            LibExt, "mer_std", StaticStdLib, StdLib),
        link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir,
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

link_lib_args(Globals, LinkedTargetType, StdLibDir, GradeDir, Ext,
        Name, StaticArg, SharedArg) :-
    (
        ( LinkedTargetType = executable
        ; LinkedTargetType = shared_library
        ),
        LibPrefix = "lib"
    ;
        ( LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ),
        LibPrefix = ""
    ),
    StaticLibName = LibPrefix ++ Name ++ extension_to_string(Globals, Ext),
    StaticArg = quote_shell_cmd_arg(StdLibDir/"lib"/GradeDir/StaticLibName),
    make_link_lib(Globals, LinkedTargetType, Name, SharedArg).

:- pred make_link_lib(globals::in,
    linked_target_type::in(c_or_csharp_exe_or_lib),
    string::in, string::out) is det.

make_link_lib(Globals, LinkedTargetType, LibName, LinkOpt) :-
    (
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
        LinkOpt = quote_shell_cmd_arg(LinkLibOpt ++ LibName ++ Suffix)
    ;
        ( LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ),
        LinkLibOpt = "-r:",
        Suffix = ".dll",
        LinkOpt = quote_shell_cmd_arg(LinkLibOpt ++ LibName ++ Suffix)
    ).

%---------------------%

    % When using --restricted-command-line with Visual C we add all the object
    % files to a temporary archive before linking an executable.
    % However, if only .lib files are given on the command line then
    % the linker needs to be manually told some details that it usually infers
    % from the object files, for example the program entry point and the
    % target machine type. link.exe will print a warning (#4001) when this
    % occurs so we also need to shut that up.
    %
:- pred get_restricted_command_line_link_opts(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_restricted_command_line_link_opts(Globals, LinkedTargetType,
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

:- pred get_runtime_library_path_opts(globals::in, linked_target_type::in,
    option::in(bound(shlib_linker_rpath_flag ; linker_rpath_flag)),
    option::in(bound(shlib_linker_rpath_separator ; linker_rpath_separator)),
    string::out) is det.

get_runtime_library_path_opts(Globals, LinkedTargetType,
        RpathFlagOpt, RpathSepOpt, RpathOpts) :-
    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    are_shared_libraries_supported(Globals, SharedLibsSupported),
    globals.lookup_string_option(Globals, linkage, Linkage),
    ( if
        UseInstallName = no,
        SharedLibsSupported = shared_libraries_supported,
        ( Linkage = "shared"
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

:- pred get_install_name_opt(globals::in, module_name::in,
    linked_target_type::in, string::out) is det.

get_install_name_opt(Globals, ModuleName, LinkedTargetType, InstallNameOpt) :-
    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    ( if
        UseInstallName = bool.yes,
        LinkedTargetType = shared_library
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
    ).

%---------------------%

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

%---------------------%

:- pred get_linker_output_option(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_linker_output_option(Globals, LinkedTargetType, OutputOpt) :-
    get_c_compiler_type(Globals, C_CompilerType),
    % XXX we should allow the user to override the compiler's choice of
    % output option here.
    % NOTE: the spacing around the value of OutputOpt here is important.
    % Any changes should be reflected in predicate link_exe_or_shared_lib/9.
    (
        ( C_CompilerType = cc_cl_x86(_)
        ; C_CompilerType = cc_cl_x64(_)
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

:- pred prepare_for_link_exe_or_shared_lib_cmd(io.text_output_stream::in,
    globals::in, list(string)::in,
    prepare_to_link_result::out, io::di, io::uo) is det.

prepare_for_link_exe_or_shared_lib_cmd(ProgressStream, Globals, ObjectsList,
        PrepareResult, !IO) :-
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
            filter_object_files(Globals, ObjectsList,
                ProperObjectFiles, NonObjectFiles),
            % Delete the currently empty output file first, otherwise ar
            % will fail to recognise its file format.
            io.file.remove_file(TmpArchive, _, !IO),
            create_archive(Globals, ProgressStream, TmpArchive, yes,
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

%---------------------------------------------------------------------------%
%
% Linking for C static libraries.
%

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

%---------------------------------------------------------------------------%
%
% Linking for C#, both executables and libraries.
%

:- pred create_csharp_exe_or_lib(globals::in, io.text_output_stream::in,
    linked_target_type::in(csharp_linked_target_type),
    module_name::in, file_name::in, list(file_name)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

create_csharp_exe_or_lib(Globals, ProgressStream, LinkedTargetType,
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
        LinkedTargetType = csharp_executable,
        TargetOption = "-target:exe",
        SignAssemblyOpt = ""
    ;
        LinkedTargetType = csharp_library,
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
    get_mercury_std_libs(Globals, LinkedTargetType, MercuryStdLibs),
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
        LinkedTargetType = csharp_executable,
        CLI \= "",
        TargetEnvType = env_type_posix
    then
        construct_cli_shell_script(Globals, FullOutputFileName, ContentStr),
        create_launcher_shell_script(ProgressStream, Globals, MainModuleName,
            ContentStr, Succeeded, !IO)
    else
        Succeeded = Succeeded0
    ).

%---------------------%

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

%---------------------%

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
% Linking for Java, both executables and archives.
%

:- pred create_java_exe_or_lib(globals::in, io.text_output_stream::in,
    linked_target_type::in(java_linked_target_type),
    module_name::in, file_name::in, list(file_name)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

create_java_exe_or_lib(Globals, ProgressStream, LinkedTargetType,
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
        LinkedTargetType = java_executable
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
%---------------------------------------------------------------------------%

post_link_maybe_make_symlink_or_copy(Globals, ProgressStream,
        FullFileName, CurDirFileName, ModuleName, LinkedTargetType,
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
                LinkedTargetType = csharp_executable,
                % NOTE: we don't generate a launcher script for C# executables
                % on Windows -- it isn't necessary since they can be executed
                % directly.
                globals.get_target_env_type(Globals, TargetEnvType),
                TargetEnvType = env_type_posix
            ;
                LinkedTargetType = java_executable
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

%---------------------%

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

%---------------------%

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

%---------------------------------------------------------------------------%
%
% Utility predicates needed for creating more than one kind of linked target.
%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
%---------------------------------------------------------------------------%
%
% Library link flags.
%

output_library_link_flags(Globals, Stream, Specs, !IO) :-
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
    get_runtime_library_path_opts(Globals, LinkedTargetType,
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
    get_mercury_std_libs(Globals, LinkedTargetType, MercuryStdLibs),
    get_system_libs(Globals, LinkedTargetType, SystemLibs),
    io.format(Stream, "%s %s %s %s %s\n",
        [s(LinkLibraryDirectories), s(RpathOpts), s(LinkLibraries),
        s(MercuryStdLibs), s(SystemLibs)], !IO).

:- pred get_system_libs(globals::in,
    linked_target_type::in(c_exe_or_shared_lib), string::out) is det.

get_system_libs(Globals, LinkedTargetType, SystemLibs) :-
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
        LinkedTargetType = shared_library,
        globals.lookup_string_option(Globals, shared_libs, OtherSystemLibs)
    ;
        LinkedTargetType = executable,
        globals.lookup_string_option(Globals, math_lib, OtherSystemLibs)
    ),
    SystemLibs = string.join_list(" ",
        [SystemTraceLibs, OtherSystemLibs, ThreadLibs]).

:- pred use_thread_libs(globals::in, bool::out) is det.

use_thread_libs(Globals, UseThreadLibs) :-
    globals.lookup_bool_option(Globals, parallel, UseThreadLibs).

%---------------------------------------------------------------------------%
:- end_module backend_libs.link_target_code.
%---------------------------------------------------------------------------%
