%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: module_cmds.m.
%
% This module handles the most of the commands generated by the
% parse_tree package.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.module_cmds.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % copy_dot_tmp_to_base_file_return_succeeded(ProgressStream, Globals,
    %   ModuleName, OutputFileName, Succeeded, !IO)
    %
    % Update the interface file FileName from FileName.tmp if it has changed,
    % and return whether the update succeeded (if it was needed).
    %
:- pred copy_dot_tmp_to_base_file_return_succeeded(io.text_output_stream::in,
    globals::in, file_name::in, maybe_succeeded::out, io::di, io::uo) is det.

    % copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
    %   FileKindStr, OutputFileName, Succeeded, !IO)
    %
    % As copy_dot_tmp_to_base_file_return_succeeded, but also print
    % an error message (which includes FileKindStr) if the update failed.
    %
:- pred copy_dot_tmp_to_base_file_report_any_error(io.text_output_stream::in,
    globals::in, string::in, file_name::in, maybe_succeeded::out,
    io::di, io::uo) is det.

:- type dot_tmp_copy_result
    --->    base_file_new_or_changed
    ;       base_file_unchanged
    ;       dot_tmp_copy_error.

    % copy_dot_tmp_to_base_file_return_changed(ProgressStream, Globals,
    %   FileName, Result, !IO):
    %
    % Update the interface file FileName from FileName.tmp if it has changed.
    % Report whether the update was needed, and if it was, whether it
    % succeeded.
    %
:- pred copy_dot_tmp_to_base_file_return_changed(io.text_output_stream::in,
    globals::in, file_name::in, dot_tmp_copy_result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % maybe_make_symlink(Globals, TargetFile, LinkName, Result, !IO):
    %
    % If `--use-symlinks' is set, attempt to make LinkName a symlink
    % pointing to LinkTarget.
    %
:- pred maybe_make_symlink(globals::in, file_name::in, file_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

    % definitely_make_symlink(TargetFile, LinkName, Result, !IO):
    %
    % Attempt to make LinkName a symlink pointing to LinkTarget.
    % Assumes that the caller has checked that `--use-symlinks' is set.
    %
:- pred definitely_make_symlink(file_name::in, file_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

    % make_symlink_or_copy_file(Globals, ProgressStream, LinkTarget, LinkName,
    %   Succeeded, !IO):
    %
    % Attempt to make LinkName a symlink pointing to LinkTarget, copying
    % LinkTarget to LinkName if that fails (or if `--use-symlinks' is not set).
    %
:- pred make_symlink_or_copy_file(globals::in, io.text_output_stream::in,
    file_name::in, file_name::in, maybe_succeeded::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % touch_module_ext_datestamp(Globals, ProgressStream,
    %   ModuleName, Ext, Succeeded, !IO):
    %
    % Touch the datestamp file `ModuleName.Ext'. Datestamp files are used
    % to record when each of the interface files was last updated.
    %
:- pred touch_module_ext_datestamp(globals::in, io.text_output_stream::in,
    module_name::in, ext::in, maybe_succeeded::out, io::di, io::uo) is det.

    % touch_file_datestamp(Globals, ProgressStream, FileName, Succeeded, !IO):
    %
    % Update the modification time for the given file,
    % clobbering the contents of the file.
    %
:- pred touch_file_datestamp(globals::in, io.text_output_stream::in,
    file_name::in, maybe_succeeded::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % If the argument is `did_not_succeed', set the exit status to 1.
    %
:- pred maybe_set_exit_status(maybe_succeeded::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Java command-line tools utilities.
%

    % Create a shell script with the same name as the given module to invoke
    % Java with the appropriate options on the class of the same name.
    %
:- pred create_java_shell_script(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, io::di, io::uo) is det.

    % Return the standard Mercury libraries needed for a Java program.
    % Return the empty list if --mercury-standard-library-directory
    % is not set.
    %
:- pred get_mercury_std_libs_for_java(globals::in, list(string)::out) is det.

    % Given a list .class files, return the list of .class files that should be
    % passed to `jar'. This is required because nested classes are in separate
    % files which we don't know about, so we have to scan the directory to
    % figure out which files were produced by `javac'.
    %
:- pred list_class_files_for_jar(globals::in, list(string)::in, string::out,
    list(string)::out, io::di, io::uo) is det.

    % Given a `mmake' variable reference to a list of .class files, return an
    % expression that generates the list of arguments for `jar' to reference
    % those class files.
    %
:- pred list_class_files_for_jar_mmake(globals::in, string::in, string::out)
    is det.

    % Get the value of the Java class path from the environment. (Normally
    % it will be obtained from the CLASSPATH environment variable, but if
    % that isn't present then the java.class.path variable may be used instead.
    % This is used for the Java back-end, which doesn't support environment
    % variables properly.)
    %
:- pred get_env_classpath(string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred create_launcher_shell_script(io.text_output_stream::in,
    globals::in, module_name::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

:- pred create_launcher_batch_file(io.text_output_stream::in,
    globals::in, module_name::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.copy_util.
:- import_module libs.options.
:- import_module parse_tree.java_names.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module io.call_system.
:- import_module io.environment.
:- import_module io.file.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

copy_dot_tmp_to_base_file_return_succeeded(ProgressStream, Globals,
        OutputFileName, Succeeded, !IO) :-
    copy_dot_tmp_to_base_file_return_changed(ProgressStream, Globals,
        OutputFileName, Result, !IO),
    (
        ( Result = base_file_new_or_changed
        ; Result = base_file_unchanged
        ),
        Succeeded = succeeded
    ;
        Result = dot_tmp_copy_error,
        Succeeded = did_not_succeed
    ).

copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        FileKindStr, OutputFileName, Succeeded, !IO) :-
    copy_dot_tmp_to_base_file_return_changed(ProgressStream, Globals,
        OutputFileName, Result, !IO),
    (
        Result = dot_tmp_copy_error,
        Succeeded = did_not_succeed,
        string.format("problem updating %s files.", [s(FileKindStr)], Msg),
        report_error(ProgressStream, Msg, !IO)
    ;
        ( Result = base_file_new_or_changed
        ; Result = base_file_unchanged
        ),
        Succeeded = succeeded
    ).

copy_dot_tmp_to_base_file_return_changed(ProgressStream, Globals,
        OutputFileName, Result, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(ProgressStream, Verbose,
        "% Updating interface:\n", !IO),
    TmpOutputFileName = OutputFileName ++ ".tmp",
    io.read_named_file_as_string(OutputFileName, OutputFileRes, !IO),
    (
        OutputFileRes = ok(OutputFileStr),
        io.read_named_file_as_string(TmpOutputFileName, TmpOutputFileRes, !IO),
        (
            TmpOutputFileRes = ok(TmpOutputFileStr),
            ( if OutputFileStr = TmpOutputFileStr then
                Result = base_file_unchanged,
                string.format("%% `%s' has not changed.\n",
                    [s(OutputFileName)], NoChangeMsg),
                maybe_write_string(ProgressStream, Verbose, NoChangeMsg, !IO),
                io.file.remove_file(TmpOutputFileName, _, !IO)
            else
                copy_dot_tmp_to_base_file_create_file(Globals, ProgressStream,
                    "CHANGED", OutputFileName, TmpOutputFileName, Result, !IO)
            )
        ;
            TmpOutputFileRes = error(TmpOutputFileError),
            io.error_message(TmpOutputFileError, TmpOutputFileErrorMsg),
            Result = dot_tmp_copy_error,
            % The error message is about TmpOutputFileName, but the
            % message we print does not mention that file name.
            io.format(ProgressStream, "Error creating `%s': %s\n",
                [s(OutputFileName), s(TmpOutputFileErrorMsg)], !IO)
        )
    ;
        OutputFileRes = error(_),
        copy_dot_tmp_to_base_file_create_file(Globals, ProgressStream,
            "been CREATED", OutputFileName, TmpOutputFileName, Result, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred copy_dot_tmp_to_base_file_create_file(globals::in,
    io.text_output_stream::in, string::in, string::in, string::in,
    dot_tmp_copy_result::out, io::di, io::uo) is det.

copy_dot_tmp_to_base_file_create_file(Globals, ProgressStream,
        ChangedStr, OutputFileName, TmpOutputFileName, Result, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    string.format("%% `%s' has %s.\n", [s(OutputFileName), s(ChangedStr)],
        ChangedMsg),
    maybe_write_string(ProgressStream, Verbose, ChangedMsg, !IO),
    copy_file_to_file_name(Globals, ProgressStream, TmpOutputFileName,
        OutputFileName, MoveRes, !IO),
    (
        MoveRes = succeeded,
        Result = base_file_new_or_changed
    ;
        MoveRes = did_not_succeed,
        Result = dot_tmp_copy_error
        % copy_file_to_file_name/7 writes an error message to ProgressStream
        % if the copy fails.
        % XXX FILE COPY: we used to generate the following error message
        % here, but I suspect it wasn't really possible to trigger it.
        %MoveRes = error(MoveError),
        %io.format(ProgressStream, "Error creating `%s': %s\n",
        %    [s(OutputFileName), s(io.error_message(MoveError))], !IO)
    ),
    io.file.remove_file(TmpOutputFileName, _, !IO).

%-----------------------------------------------------------------------------%

maybe_make_symlink(Globals, LinkTarget, LinkName, Result, !IO) :-
    globals.lookup_bool_option(Globals, use_symlinks, UseSymLinks),
    (
        UseSymLinks = yes,
        definitely_make_symlink(LinkTarget, LinkName, Result, !IO)
    ;
        UseSymLinks = no,
        Result = did_not_succeed
    ).

definitely_make_symlink(LinkTarget, LinkName, Result, !IO) :-
    io.file.remove_file_recursively(LinkName, _, !IO),
    io.file.make_symlink(LinkTarget, LinkName, LinkResult, !IO),
    Result = ( if LinkResult = ok then succeeded else did_not_succeed ).

make_symlink_or_copy_file(Globals, ProgressStream,
        SourceFileName, DestinationFileName, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, use_symlinks, UseSymLinks),
    globals.lookup_bool_option(Globals, verbose_commands, PrintCommand),
    (
        UseSymLinks = yes,
        LinkOrCopy = "linking",
        (
            PrintCommand = yes,
            io.format(ProgressStream, "%% Linking file `%s' -> `%s'\n",
                [s(SourceFileName), s(DestinationFileName)], !IO),
            io.flush_output(ProgressStream, !IO)
        ;
            PrintCommand = no
        ),
        io.file.make_symlink(SourceFileName, DestinationFileName, Result, !IO),
        (
            Result = ok,
            Succeeded = succeeded
        ;
            Result = error(Error),
            Succeeded = did_not_succeed,
            io.progname_base("mercury_compile", ProgName, !IO),
            io.error_message(Error, ErrorMsg),
            io.format(ProgressStream, "%s: error %s `%s' to `%s', %s\n",
                [s(ProgName), s(LinkOrCopy), s(SourceFileName),
                s(DestinationFileName), s(ErrorMsg)], !IO),
            io.flush_output(ProgressStream, !IO)
        )
    ;
        UseSymLinks = no,
        %LinkOrCopy = "copying",
        (
            PrintCommand = yes,
            io.format(ProgressStream, "%% Copying file `%s' -> `%s'\n",
                [s(SourceFileName), s(DestinationFileName)], !IO),
            io.flush_output(ProgressStream, !IO)
        ;
            PrintCommand = no
        ),
        % XXX FILE COPY
        % copy_file_to_file_name/7 will write an error message
        % to ProgressStream itself if the file copy fails. We used to generate
        % an error similar to the symlink case above here.
        copy_file_to_file_name(Globals, ProgressStream, SourceFileName,
            DestinationFileName, Succeeded, !IO)
    ).

%-----------------------------------------------------------------------------%

touch_module_ext_datestamp(Globals, ProgressStream, ModuleName, Ext,
        Succeeded, !IO) :-
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred, Ext,
        ModuleName, FileName, _FileNameProposed, !IO),
    touch_file_datestamp(Globals, ProgressStream, FileName, Succeeded, !IO).

touch_file_datestamp(Globals, ProgressStream, FileName, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(ProgressStream, Verbose,
        "% Touching `" ++ FileName ++ "'... ", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    io.open_output(FileName, Result, !IO),
    (
        Result = ok(FileStream),
        % This write does the "touching", i.e. the updating of the file's
        % time of last modification.
        io.write_string(FileStream, "\n", !IO),
        io.close_output(FileStream, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        Succeeded = succeeded
    ;
        Result = error(IOError),
        io.error_message(IOError, IOErrorMessage),
        io.format(ProgressStream, "\nError opening `%s' for output: %s.\n",
            [s(FileName), s(IOErrorMessage)], !IO),
        Succeeded = did_not_succeed
    ).

%-----------------------------------------------------------------------------%

maybe_set_exit_status(succeeded, !IO).
maybe_set_exit_status(did_not_succeed, !IO) :-
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
%
% Java command-line utilities.
%

create_java_shell_script(ProgressStream, Globals, MainModuleName,
        Succeeded, !IO) :-
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred, ext_cur_gs(ext_cur_gs_lib_jar),
        MainModuleName, JarFileName, _JarFileNameProposed),
    get_target_env_type(Globals, TargetEnvType),
    (
        ( TargetEnvType = env_type_posix
        ; TargetEnvType = env_type_cygwin
        ),
        io.environment.get_environment_var("MERCURY_STAGE2_LAUNCHER_BASE",
            MaybeStage2Base, !IO),
        construct_java_shell_script(Globals, MaybeStage2Base,
            MainModuleName, JarFileName, ContentStr),
        create_launcher_shell_script(ProgressStream, Globals, MainModuleName,
            ContentStr, Succeeded, !IO)
    ;
        TargetEnvType = env_type_msys,
        construct_java_msys_shell_script(Globals, MainModuleName, JarFileName,
            ContentStr),
        create_launcher_shell_script(ProgressStream, Globals, MainModuleName,
            ContentStr, Succeeded, !IO)
    ;
        % XXX should create a .ps1 file on PowerShell.
        ( TargetEnvType = env_type_win_cmd
        ; TargetEnvType = env_type_powershell
        ),
        construct_java_batch_file(Globals, MainModuleName, JarFileName,
            ContentStr),
        create_launcher_batch_file(ProgressStream, Globals, MainModuleName,
            ContentStr, Succeeded, !IO)
    ).

:- pred construct_java_shell_script(globals::in, maybe(string)::in,
    module_name::in, file_name::in, string::out) is det.

construct_java_shell_script(Globals, MaybeStage2Base,
        MainModuleName, JarFileName, ContentStr) :-
    (
        MaybeStage2Base = no,
        get_mercury_std_libs_for_java(Globals, MercuryStdLibs)
    ;
        MaybeStage2Base = yes(Stage2Base),
        MercuryStdLibs = [
            Stage2Base / "library/mer_rt.jar",
            Stage2Base / "library/mer_std.jar"
        ]
    ),
    globals.lookup_accumulating_option(Globals, java_classpath,
        UserClasspath),
    % We prepend the .class files' directory and the current CLASSPATH.
    Java_Incl_Dirs = ["\"$DIR/" ++ JarFileName ++ "\""] ++
        MercuryStdLibs ++ ["$CLASSPATH" | UserClasspath],
    ClassPath = string.join_list("${SEP}", Java_Incl_Dirs),

    globals.lookup_accumulating_option(Globals, java_runtime_flags,
        RuntimeFlags),
    RuntimeOpts0 = string.join_list(" ", RuntimeFlags),
    RuntimeOpts = escape_single_quotes_for_shell_script(RuntimeOpts0),

    globals.lookup_string_option(Globals, java_interpreter, Java),
    mangle_sym_name_for_java(MainModuleName, module_qual, ".", ClassName),

    ContentStr = string.append_list([
        "#!/bin/sh\n",
        "DIR=${0%/*}\n",
        "DIR=$( cd \"${DIR}\" && pwd -P )\n",
        "case $WINDIR in\n",
        "   '') SEP=':' ;;\n",
        "   *)  SEP=';' ;;\n",
        "esac\n",
        "CLASSPATH=", ClassPath, "\n",
        "export CLASSPATH\n",
        "MERCURY_JAVA=${MERCURY_JAVA:-'", Java, "'}\n",
        "MERCURY_JAVA_OPTIONS=${MERCURY_JAVA_OPTIONS:-'", RuntimeOpts, "'}\n",
        "exec \"$MERCURY_JAVA\" $MERCURY_JAVA_OPTIONS jmercury.", ClassName,
            " \"$@\"\n"
    ]).

    % For the MSYS version of the Java launcher script, there are a few
    % differences:
    %
    % 1. The value of the CLASSPATH environment variable we construct for the
    % Java interpreter must contain Windows style paths.
    %
    % 2. We use forward slashes as directory separators rather than back
    % slashes since the latter require escaping inside the shell script.
    %
    % 3. The path separator character, ';', in the value of CLASSPATH must be
    % escaped because it is a statement separator in sh.
    %
    % 4. The path of the Java interpreter must be a Unix style path as it will
    % be invoked directly from the MSYS shell.
    %
    % XXX TODO: handle MERCURY_STAGE2_LAUNCHER_BASE for this case.
    %
:- pred construct_java_msys_shell_script(globals::in, module_name::in,
    file_name::in, string::out) is det.

construct_java_msys_shell_script(Globals, MainModuleName, JarFileName,
        ContentStr) :-
    get_mercury_std_libs_for_java(Globals, MercuryStdLibs),
    globals.lookup_accumulating_option(Globals, java_classpath,
        UserClasspath),
    % We prepend the .class files' directory and the current CLASSPATH.
    Java_Incl_Dirs0 = ["\"$DIR/" ++ JarFileName ++ "\""] ++
        MercuryStdLibs ++ ["$CLASSPATH" | UserClasspath],
    Java_Incl_Dirs = list.map(func(S) = string.replace_all(S, "\\", "/"),
        Java_Incl_Dirs0),
    ClassPath = string.join_list("\\;", Java_Incl_Dirs),

    globals.lookup_accumulating_option(Globals, java_runtime_flags,
        RuntimeFlags),
    RuntimeOpts0 = string.join_list(" ", RuntimeFlags),
    RuntimeOpts = escape_single_quotes_for_shell_script(RuntimeOpts0),

    globals.lookup_string_option(Globals, java_interpreter, Java),
    mangle_sym_name_for_java(MainModuleName, module_qual, ".", ClassName),

    ContentStr = string.append_list([
        "#!/bin/sh\n",
        "DIR=${0%/*}\n",
        "DIR=$( cd \"${DIR}\" && pwd -W )\n",
        "CLASSPATH=", ClassPath, "\n",
        "export CLASSPATH\n",
        "MERCURY_JAVA=${MERCURY_JAVA:-'", Java, "'}\n",
        "MERCURY_JAVA_OPTIONS=${MERCURY_JAVA_OPTIONS:-'", RuntimeOpts, "'}\n",
        "exec \"$MERCURY_JAVA\" $MERCURY_JAVA_OPTIONS jmercury.", ClassName,
            " \"$@\"\n"
    ]).

:- func escape_single_quotes_for_shell_script(string) = string.

escape_single_quotes_for_shell_script(S) =
    ( if string.contains_char(S, '\'') then
        string.replace_all(S, "'", "'\\''")
    else
        S
    ).

:- pred construct_java_batch_file(globals::in, module_name::in, file_name::in,
    string::out) is det.

construct_java_batch_file(Globals, MainModuleName, JarFileName, ContentStr) :-
    get_mercury_std_libs_for_java(Globals, MercuryStdLibs),
    globals.lookup_accumulating_option(Globals, java_classpath,
        UserClasspath),
    % We prepend the .class files' directory and the current CLASSPATH.
    Java_Incl_Dirs = ["%DIR%\\" ++ JarFileName] ++ MercuryStdLibs ++
        ["%CLASSPATH%" | UserClasspath],
    ClassPath = string.join_list(";", Java_Incl_Dirs),

    globals.lookup_accumulating_option(Globals, java_runtime_flags,
        RuntimeFlags),
    RuntimeOpts = string.join_list(" ", RuntimeFlags),

    globals.lookup_string_option(Globals, java_interpreter, Java),
    mangle_sym_name_for_java(MainModuleName, module_qual, ".", ClassName),

    ContentStr = string.append_list([
        "@echo off\n",
        "rem Automatically generated by the Mercury compiler.\n",
        "setlocal enableextensions\n",
        "set DIR=%~dp0\n",
        "set CLASSPATH=", ClassPath, "\n",
        "if not defined MERCURY_JAVA_OPTIONS set MERCURY_JAVA_OPTIONS=",
            RuntimeOpts, "\n",
        Java, " %MERCURY_JAVA_OPTIONS% jmercury.", ClassName, " %*\n"
    ]).

get_mercury_std_libs_for_java(Globals, !:StdLibs) :-
    % NOTE: changes here may require changes to get_mercury_std_libs.

    !:StdLibs = [],
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_grade_dir(Globals, GradeDir),
        % Source-to-source debugging libraries.
        globals.lookup_bool_option(Globals, link_ssdb_libs,
            SourceDebug),
        (
            SourceDebug = yes,
            list.cons(StdLibDir/"lib"/GradeDir/"mer_browser.jar", !StdLibs),
            list.cons(StdLibDir/"lib"/GradeDir/"mer_mdbcomp.jar", !StdLibs),
            list.cons(StdLibDir/"lib"/GradeDir/"mer_ssdb.jar", !StdLibs)
        ;
            SourceDebug = no
        ),
        list.cons(StdLibDir/"lib"/GradeDir/"mer_std.jar", !StdLibs),
        list.cons(StdLibDir/"lib"/GradeDir/"mer_rt.jar", !StdLibs)
    ;
        MaybeStdLibDir = no
    ).

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

list_class_files_for_jar_mmake(Globals, ClassFiles, ListClassFiles) :-
    % XXX LEGACY
    get_java_dir_path(Globals, ext_cur_ngs_gs_java_class,
        ClassSubDirPath, _ClassSubDirPathProposed),
    (
        ClassSubDirPath = [],
        ListClassFiles = ClassFiles
    ;
        ClassSubDirPath = [_ | _],
        ClassSubDir = dir.relative_path_name_from_components(ClassSubDirPath),
        % Here we use the `-C' option of jar to change directory during
        % execution, then use sed to strip away the Mercury/classes/ prefix
        % to the class files.
        % Otherwise, the class files would be stored as
        %   Mercury/classes/*.class
        % within the jar file, which is not what we want.
        % XXX It would be nice to avoid this dependency on sed.
        ListClassFiles = "-C " ++ ClassSubDir ++ " \\\n" ++
            "\t\t`echo "" " ++ ClassFiles ++ """" ++
            " | sed 's| '" ++ ClassSubDir ++ "/| |'`"
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

%-----------------------------------------------------------------------------%

get_env_classpath(Classpath, !IO) :-
    io.environment.get_environment_var("CLASSPATH", MaybeCP, !IO),
    (
        MaybeCP = yes(Classpath)
    ;
        MaybeCP = no,
        io.environment.get_environment_var("java.class.path", MaybeJCP, !IO),
        (
            MaybeJCP = yes(Classpath)
        ;
            MaybeJCP = no,
            Classpath = ""
        )
    ).

%-----------------------------------------------------------------------------%

create_launcher_shell_script(ProgressStream, Globals, MainModuleName,
        ContentStr, Succeeded, !IO) :-
    Ext = ext_cur_gas(ext_cur_gas_exec_noext),
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred, Ext,
        MainModuleName, LauncherFileName, _LauncherFileNameProposed, !IO),

    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(ProgressStream, Verbose,
        "% Generating shell script `" ++ LauncherFileName ++ "'...\n", !IO),

    % Remove symlink in the way, if any.
    io.file.remove_file(LauncherFileName, _, !IO),
    io.open_output(LauncherFileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.write_string(Stream, ContentStr, !IO),
        io.close_output(Stream, !IO),
        io.call_system.call_system("chmod a+x " ++ LauncherFileName,
            ChmodResult, !IO),
        (
            ChmodResult = ok(Status),
            ( if Status = 0 then
                Succeeded = succeeded,
                maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO)
            else
                unexpected($pred, "chmod exit status != 0"),
                Succeeded = did_not_succeed
            )
        ;
            ChmodResult = error(Message),
            unexpected($pred, io.error_message(Message)),
            Succeeded = did_not_succeed
        )
    ;
        OpenResult = error(Message),
        unexpected($pred, io.error_message(Message)),
        Succeeded = did_not_succeed
    ).

%-----------------------------------------------------------------------------%

create_launcher_batch_file(ProgressStream, Globals, MainModuleName,
        ContentStr, Succeeded, !IO) :-
    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_gas(ext_cur_gas_exec_bat), MainModuleName,
        FileName, _FileNameProposed, !IO),

    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(ProgressStream, Verbose,
        "% Generating batch file `" ++ FileName ++ "'...\n", !IO),

    % Remove an existing batch file of the same name, if any.
    io.file.remove_file(FileName, _, !IO),
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.write_string(Stream, ContentStr, !IO),
        io.close_output(Stream, !IO),
        Succeeded = succeeded
    ;
        OpenResult = error(Message),
        unexpected($pred, io.error_message(Message)),
        Succeeded = did_not_succeed
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.module_cmds.
%-----------------------------------------------------------------------------%
