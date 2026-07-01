%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: create_launchers.m.
%
% This module creates launcher scripts that play the role of executables
% for the Java backend, and does the last step of creating such executables
% for the C# backend.
%
% The difference is that the C# backend is much newer. We export
% create_java_shell_script to two modules beyond its main user,
% link_target_code.m:
%
% - write_deps_file.m, to allow the creation of java launchers for a program
%   when we create its .dep file for Mmake, and
%
% - to mercury_compile_main.m, to allow it to create the launcher as part of
%   the opfam_target_object_and_executable op_mode.
%
% The C# backend was designed NOT to support Mmake, and mercury_compile_main.m
% flat out IGNORES the job of create launcher scripts for that op_mode when
% targeting C#.
%
% This is why the C# equivalent of create_java_shell_script is local to
% link_target_code.m. If and when we ever fix the IGNORE above (if that fix
% is even possible, without the initial setup from mmc --make), it would
% have to start with moving that code here.
%
%---------------------------------------------------------------------------%

:- module backend_libs.create_launchers.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.

%---------------------------------------------------------------------------%

    % Create a shell script with the same name as the given module to invoke
    % Java with the appropriate options on the class of the same name.
    %
:- pred create_java_shell_script(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred create_shell_script_as_executable(io.text_output_stream::in,
    globals::in, module_name::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.java_names.
:- import_module parse_tree.module_cmds.

:- import_module bool.
:- import_module dir.
:- import_module io.call_system.
:- import_module io.environment.
:- import_module io.file.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

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
        create_shell_script_as_executable(ProgressStream, Globals,
            MainModuleName, ContentStr, Succeeded, !IO)
    ;
        TargetEnvType = env_type_msys,
        construct_java_msys_shell_script(Globals, MainModuleName, JarFileName,
            ContentStr),
        create_shell_script_as_executable(ProgressStream, Globals,
            MainModuleName, ContentStr, Succeeded, !IO)
    ;
        % XXX should create a .ps1 file on PowerShell.
        ( TargetEnvType = env_type_win_cmd
        ; TargetEnvType = env_type_powershell
        ),
        construct_java_batch_file(Globals, MainModuleName, JarFileName,
            ContentStr),
        create_java_launcher_batch_file(ProgressStream, Globals,
            MainModuleName, ContentStr, Succeeded, !IO)
    ).

%---------------------%

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

%---------------------%

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

%---------------------%

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

%---------------------%

:- pred create_java_launcher_batch_file(io.text_output_stream::in,
    globals::in, module_name::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

create_java_launcher_batch_file(ProgressStream, Globals, MainModuleName,
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

%---------------------------------------------------------------------------%

create_shell_script_as_executable(ProgressStream, Globals,
        MainModuleName, ContentStr, Succeeded, !IO) :-
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

%---------------------------------------------------------------------------%
:- end_module backend_libs.create_launchers.
%---------------------------------------------------------------------------%
