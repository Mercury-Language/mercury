%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: link_target_code.m.
% Main authors: fjh, stayl.
%
% Code to create executables and libraries. This module creates them
% when targeting C# or Java; link_target_code_c.m creates them
% when targeting C.
%
% Many predicates both in this module and in link_target_code_c.m
% have names that end in "_for_c", "_for_cs", or "_for_java".
% Sets of predicates that have the same name except for these suffixes
% are intended to do the same job, but the compilation landscapes
% of these languages are so different that in some cases, their
% implementations do not even resemble each other. (In other cases, they do.)
% Nevertheless, if you modify one such predicate, it would be a good idea
% to check whether a change with a similar purpose should also be applied
% to the other predicate or predicates in the set.
%
%---------------------------------------------------------------------------%

:- module backend_libs.link_target_code.
:- interface.

:- import_module backend_libs.link_target_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % link_files_into_executable_or_library_for_c_cs_java(ProgressStream,
    %   Globals, LinkedTargetType, MainModuleName, FilesToLink,
    %   Specs, Succeeded, !IO):
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
:- pred link_files_into_executable_or_library_for_c_cs_java(
    io.text_output_stream::in, globals::in,
    linked_target_type::in, module_name::in, list(string)::in,
    list(error_spec)::out, maybe_succeeded::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.create_launchers.
:- import_module backend_libs.link_target_code_c.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module parse_tree.file_names.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module io.file.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% WARNING: The code here duplicates the functionality of scripts/ml.in.
% Any changes there may also require changes here, and vice versa.

link_files_into_executable_or_library_for_c_cs_java(ProgressStream, Globals,
        LinkedTargetType, ModuleName, FilesToLink, Specs, Succeeded, !IO) :-
    pre_link_msg(ProgressStream, Globals, !IO),
    linked_target_file_name_full_curdir(Globals, ModuleName, LinkedTargetType,
        FullOutputFileName, CurDirOutputFileName, !IO),
    (
        ( LinkedTargetType = c_executable
        ; LinkedTargetType = c_shared_library
        ),
        create_exe_or_shared_lib_for_c(ProgressStream, Globals,
            LinkedTargetType, ModuleName, FullOutputFileName, FilesToLink,
            Specs, LinkSucceeded, !IO)
    ;
        LinkedTargetType = c_static_library,
        create_static_lib_for_c(ProgressStream, Globals,
            FullOutputFileName, yes, FilesToLink, LinkSucceeded, !IO),
        Specs = []
    ;
        ( LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ),
        % XXX C# see also older predicate compile_csharp_file
        create_exe_or_lib_for_csharp(ProgressStream, Globals,
            LinkedTargetType, ModuleName, FullOutputFileName, FilesToLink,
            LinkSucceeded, !IO),
        Specs = []
    ;
        ( LinkedTargetType = java_executable
        ; LinkedTargetType = java_archive
        ),
        create_exe_or_lib_for_java(ProgressStream, Globals,
            LinkedTargetType, ModuleName, FullOutputFileName, FilesToLink,
            LinkSucceeded, !IO),
        Specs = []
    ),
    post_link_msg(ProgressStream, Globals, !IO),
    post_link_maybe_make_symlink(ProgressStream, Globals, LinkedTargetType,
        ModuleName, FullOutputFileName, CurDirOutputFileName,
        LinkSucceeded, Succeeded, !IO).

%---------------------------------------------------------------------------%
%
% Linking for C#, both executables and libraries.
%

:- pred create_exe_or_lib_for_csharp(io.text_output_stream::in, globals::in,
    linked_target_type::in(csharp_linked_target_type),
    module_name::in, file_name::in, list(file_name)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

create_exe_or_lib_for_csharp(ProgressStream, Globals, LinkedTargetType,
        MainModuleName, FullOutputFileName0, SourceList0,
        Succeeded, !IO) :-
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

    get_link_opts_for_libraries_for_cs(Globals, LinkLibOpts0),
    % XXX The call to csharp_file_name looks wrong, because the input
    % we give, and the output we expect from it, are NOT filenames,
    % but command line option strings that CONTAIN filenames.
    % The conversion to windows path format should still work, but ...
    LinkLibOpts =
        list.map(csharp_file_name(EnvType, CSharpCompilerType),
        LinkLibOpts0),
    join_quoted_string_list(LinkLibOpts, "", "", " ", LinkLibraries),

    globals.lookup_string_option(Globals, csharp_compiler, CSharpCompilerCmd),
    get_mercury_std_libs_for_cs(Globals, LinkedTargetType, MercuryStdLibs),
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
        construct_cli_shell_script_for_csharp(Globals, FullOutputFileName,
            ContentStr),
        create_shell_script_as_executable(ProgressStream, Globals,
            MainModuleName, ContentStr, Succeeded, !IO)
    else
        Succeeded = Succeeded0
    ).

%---------------------%

    % Converts the given filename into a format acceptable to the C# compiler.
    %
    % Older MS C# compilers only allowed \ as the path separator, so we convert
    % all / into \ when using an MS C# compiler on Windows.
    %
    % XXX do current MS C# compilers still have this behaviour?
    % - juliensf, 2025-08-17
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

:- pred get_link_opts_for_libraries_for_cs(globals::in,
    list(string)::out) is det.

get_link_opts_for_libraries_for_cs(Globals, LinkLibOpts) :-
    globals.lookup_accumulating_option(Globals, link_libraries,
        LinkLibraries),
    list.map(get_link_opts_for_library_for_cs, LinkLibraries, LinkLibOpts).

:- pred get_link_opts_for_library_for_cs(string::in, string::out) is det.

get_link_opts_for_library_for_cs(LibName, LinkerOpt) :-
    LinkerOpt = "-r:" ++ LibName ++ ".dll".

%---------------------%

    % Find the standard Mercury libraries, and the system
    % libraries needed by them.
    % Return the empty string if --mercury-standard-library-directory
    % is not set.
    % NOTE: This predicate and get_mercury_std_libs_for_{c,java} do
    % quite similar jobs; changes to one may require changes to the others.
    %
:- pred get_mercury_std_libs_for_cs(globals::in,
    linked_target_type::in(csharp_linked_target_type), string::out) is det.

get_mercury_std_libs_for_cs(Globals, LinkedTargetType, StdLibs) :-
    % NOTE The predicate body does not actually use LinkedTargetType.
    % All the references to it are in calls whose callees ignore that arg.
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_grade_dir(Globals, GradeDir),
        LibExt = ext_cur_gs(ext_cur_gs_lib_cil_dll),

        % Source-to-source debugging libraries.
        globals.lookup_bool_option(Globals, link_ssdb_libs, SourceDebug),
        (
            SourceDebug = yes,
            link_lib_args_for_cs(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_ssdb", SsdbLib),
            link_lib_args_for_cs(Globals, LinkedTargetType, StdLibDir,
                GradeDir, LibExt, "mer_browser", BrowserLib),
            link_lib_args_for_cs(Globals, LinkedTargetType,
                StdLibDir, GradeDir, LibExt, "mer_mdbcomp", MdbCompLib),
            SharedSourceDebugLibs = string.join_list(" ",
                [SsdbLib, BrowserLib, MdbCompLib])
        ;
            SourceDebug = no,
            SharedSourceDebugLibs = ""
        ),

        link_lib_args_for_cs(Globals, LinkedTargetType, StdLibDir, GradeDir,
            LibExt, "mer_std", StdLib),
        StdLibs = string.join_list(" ", [SharedSourceDebugLibs, StdLib])
    ;
        MaybeStdLibDir = no,
        StdLibs = ""
    ).

%---------------------%

:- pred link_lib_args_for_cs(globals::in,
    linked_target_type::in(csharp_linked_target_type), string::in,
    string::in, ext::in, string::in, string::out) is det.

link_lib_args_for_cs(_Globals, _LinkedTargetType, _StdLibDir, _GradeDir, _Ext,
        Name, SharedArg) :-
    make_link_lib_for_cs(Name, SharedArg).

:- pred make_link_lib_for_cs(string::in, string::out) is det.

make_link_lib_for_cs(LibName, LinkOpt) :-
    LinkLibOpt = "-r:",
    Suffix = ".dll",
    LinkOpt = quote_shell_cmd_arg(LinkLibOpt ++ LibName ++ Suffix).

%---------------------%

:- pred construct_cli_shell_script_for_csharp(globals::in, string::in,
    string::out) is det.

construct_cli_shell_script_for_csharp(Globals, ExeFileName, ContentStr) :-
    globals.lookup_string_option(Globals, cli_interpreter, CLI),
    globals.lookup_accumulating_option(Globals, link_library_directories,
        LinkLibraryDirectoriesList),
    globals.lookup_accumulating_option(Globals, mono_path_directories,
        MonoPathDirectoriesList),
    AllSearchPaths = LinkLibraryDirectoriesList ++ MonoPathDirectoriesList,
    join_quoted_string_list(AllSearchPaths, "", "",
        ":", MonoPathDirectories),
    ContentStr = string.append_list([
        "#!/bin/sh\n",
        "DIR=${0%/*}\n",
        "MONO_PATH=$MONO_PATH:", MonoPathDirectories, "\n",
        "export MONO_PATH\n",
        "CLI_INTERPRETER=${CLI_INTERPRETER:-", CLI, "}\n",
        "exec \"$CLI_INTERPRETER\" \"$DIR/", ExeFileName, "\" \"$@\"\n"
    ]).

%---------------------------------------------------------------------------%
%
% Linking for Java, both executables and archives.
%

:- pred create_exe_or_lib_for_java(io.text_output_stream::in, globals::in,
    linked_target_type::in(java_linked_target_type),
    module_name::in, file_name::in, list(file_name)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

create_exe_or_lib_for_java(ProgressStream, Globals, LinkedTargetType,
        MainModuleName, FullJarFileName, ObjectList, Succeeded, !IO) :-
    list_class_files_for_jar(Globals, ObjectList, ClassSubDir, ListClassFiles,
        !IO),
    (
        ListClassFiles = [],
        unexpected($pred, "empty list of .class files")
    ;
        ListClassFiles = [_ | _]
    ),

    % Write the list of class files to a temporary file and pass the name of
    % the temporary file to jar using @syntax. We do this because the list
    % of class files can be extremely long, and can cause problems with
    % limited argument vector lengths.
    %
    % We create the temporary file in the current directory to avoid problems
    % under Cygwin, where absolute paths will be interpreted incorrectly
    % when passed to a non-Cygwin jar program.
    open_temp_output_with_naming_scheme(".", "mtmp", "", TempFileResult, !IO),
    (
        TempFileResult = ok({TempFileName, Stream}),
        list.foldl(write_jar_class_argument(Stream, ClassSubDir),
            ListClassFiles, !IO),
        io.close_output(Stream, !IO),

        globals.lookup_string_option(Globals, java_archive_command, JarCmd),
        Cmd = string.append_list(
            [JarCmd, " cf ", FullJarFileName, " @", TempFileName]),
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

%---------------------%

    % Given a list .class files, return the list of .class files that should be
    % passed to `jar'. We need this because nested classes are in separate
    % files which we don't know about, so we have to scan the directory to
    % figure out which files were produced by `javac'.
    %
    % This does a similar job to list_class_files_for_jar_mmake in
    % generate_mmakefile_fragments.m, but the details are completely different.
    %
:- pred list_class_files_for_jar(globals::in, list(string)::in, string::out,
    list(string)::out, io::di, io::uo) is det.

list_class_files_for_jar(Globals, MainClassFiles, ClassSubDir,
        ListClassFiles, !IO) :-
    % XXX LEGACY
    get_java_dir_path(Globals, ext_cur_ngs_gs_java_class,
        ClassSubDirPath, _ClassSubDirPathProposed),
    ClassSubDir = dir.relative_path_name_from_components(ClassSubDirPath),

    list.filter_map(make_nested_class_prefix, MainClassFiles,
        NestedClassPrefixes),
    NestedClassPrefixesSet = set.list_to_set(NestedClassPrefixes),

    SearchDir = ClassSubDir / "jmercury",
    SubDir = enter_subdirs(follow_symlinks),
    FoldParams = fold_params(SubDir, on_error_keep_going),
    % Unfortunately, dir.general_foldl2 is not *quite* general enough
    % that we could tell it to not even try to open any file or directory
    % that does not start with a prefix in NestedClassPrefixesSet.
    dir.general_foldl2(FoldParams,
        accumulate_nested_class_files(NestedClassPrefixesSet),
        SearchDir, [], NestedClassFiles, Errors, !IO),
    list.filter(file_error_is_relevant(NestedClassPrefixesSet),
        Errors, RelevantErrors),
    (
        RelevantErrors = [],
        AllClassFiles0 = MainClassFiles ++ NestedClassFiles,
        % Remove the `Mercury/classes' prefix if present.
        ( if ClassSubDir = dir.this_directory then
            AllClassFiles = AllClassFiles0
        else
            ClassSubDirSep = ClassSubDir / "",
            AllClassFiles = list.map(
                string.remove_prefix_if_present(ClassSubDirSep),
                AllClassFiles0)
        ),
        list.sort(AllClassFiles, ListClassFiles)
    ;
        RelevantErrors = [file_error(_, _, Error) | _],
        unexpected($pred, io.error_message(Error))
    ).

:- pred make_nested_class_prefix(string::in, string::out) is semidet.

make_nested_class_prefix(ClassFileName, ClassPrefix) :-
    % Nested class files are named "Class$Nested_1$Nested_2.class".
    string.remove_suffix(ClassFileName, ".class", BaseName),
    ClassPrefix = BaseName ++ "$".

:- pred accumulate_nested_class_files(set(string)::in, string::in, string::in,
    io.file_type::in, bool::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

accumulate_nested_class_files(NestedClassPrefixes, DirName, BaseName,
        FileType, Continue, !Acc, IO, IO) :-
    % The I/O state arguments, which we do not use, are required
    % by dir.general_foldl2.
    (
        % These file types may be .class files.
        ( FileType = regular_file
        ; FileType = symbolic_link
        ),
        IsNestedCF =
            file_is_nested_class_file(NestedClassPrefixes, DirName, BaseName),
        (
            IsNestedCF = yes,
            !:Acc = [DirName / BaseName | !.Acc]
        ;
            IsNestedCF = no
        )
    ;
        % These file types cannot be .class files.
        ( FileType = directory
        ; FileType = named_pipe
        ; FileType = socket
        ; FileType = character_device
        ; FileType = block_device
        ; FileType = message_queue
        ; FileType = semaphore
        ; FileType = shared_memory
        ; FileType = unknown
        )
    ),
    Continue = yes.

:- func file_is_nested_class_file(set(string), string, string) = bool.

file_is_nested_class_file(NestedClassPrefixes, DirName, BaseName)
        = IsNestedCF :-
    ( if
        string.sub_string_search(BaseName, "$", Dollar),
        BaseNameToDollar = string.left(BaseName, Dollar + 1),
        set.contains(NestedClassPrefixes, DirName / BaseNameToDollar)
    then
        IsNestedCF = yes
    else
        IsNestedCF = no
    ).

%---------------------%

:- pred file_error_is_relevant(set(string)::in, file_error::in)
    is semidet.

file_error_is_relevant(NestedClassPrefixes, FileError) :-
    FileError = file_error(PathName, _Op, _IOError),
    ( if split_name(PathName, DirName, BaseName) then
        file_is_nested_class_file(NestedClassPrefixes, DirName, BaseName) = yes
    else
        % If we cannot read the top level SearchDir, that error is relevant.
        true
    ).

%---------------------%

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
:- end_module backend_libs.link_target_code.
%---------------------------------------------------------------------------%
