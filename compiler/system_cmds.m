%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: system_cmds.m.
%
% This module provides predicates to invoke commands via the shell.
%
%-----------------------------------------------------------------------------%

:- module libs.system_cmds.
:- interface.

:- import_module libs.globals.
:- import_module libs.maybe_util.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type quote_char
    --->    forward     % '
    ;       double.     % "

:- type command_verbosity
    --->    cmd_verbose
            % Output the command line only with `--verbose'.

    ;       cmd_verbose_commands.
            % Output the command line with `--verbose-commands'. This should be
            % used for commands that may be of interest to the user.

    % invoke_system_command(Globals, ProgressStream, CmdOutputStream,
    %   Verbosity, Command, Succeeded):
    %
    % Invoke an executable. Progress messages, including error messages
    % that say why we cannot make progress, will go to ProgressStream.
    % Output from the invoked command will go to CmdOutputStream.
    %
:- pred invoke_system_command(globals::in, io.text_output_stream::in,
    io.text_output_stream::in,
    command_verbosity::in, string::in, maybe_succeeded::out,
    io::di, io::uo) is det.

    % invoke_system_command_maybe_filter_output(Globals,
    %   ProgressStream, CmdOutputStream, Verbosity, Command,
    %   MaybeProcessOutput, Succeeded):
    %
    % Invoke an executable. Progress messages, including error messages
    % that say why we cannot make progress, will go to ProgressStream.
    % Output from the invoked command, filtered if MaybeProcessOutput
    % is set to yes(...), will go to CmdOutputStream.
    %
:- pred invoke_system_command_maybe_filter_output(globals::in,
    io.text_output_stream::in, io.text_output_stream::in,
    command_verbosity::in, string::in, maybe(string)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

    % invoke_long_system_command(Globals, ProgressStream, CmdOutputStream,
    %     Verbosity, Command, Args, Succeeded):
    %
    % Invoke an executable with arguments Args, using the @file style of
    % calling to avoid command line length limits on various systems. If the
    % underlying tool chain does not support this, it just calls the normal
    % invoke_system_command and hopes the command is not too long. Progress
    % messages, including error messages that say why we cannot make progress,
    % will go to ProgressStream. Output from the invoked command will go to
    % CmdOutputStream.
    %
:- pred invoke_long_system_command(globals::in, io.text_output_stream::in,
    io.text_output_stream::in, command_verbosity::in, string::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

    % invoke_long_system_command_maybe_filter_output(Globals,
    %   ProgressStream, CmdOutputStream, Verbosity, Command,
    %   NonAtArgs, Args, MaybeProcessOutput, Succeeded):
    %
    % Invoke an executable with arguments NonAtArgs and Args, using the @file
    % style of calling for Args and passing NonAtArgs on the command line. If
    % the underlying tool chain does not support this, it just calls the normal
    % invoke_system_command and hopes the command is not too long. Progress
    % messages, including error messages that say why we cannot make progress,
    % will go to ProgressStream. Output from the invoked command, filtered if
    % MaybeProcessOutput is set to yes(...), will go to CmdOutputStream.
    %
:- pred invoke_long_system_command_maybe_filter_output(globals::in,
    io.text_output_stream::in, io.text_output_stream::in,
    command_verbosity::in, string::in, string::in, string::in,
    maybe(string)::in, maybe_succeeded::out, io::di, io::uo) is det.

    % Make a command string, which needs to be invoked in a shell environment.
    %
:- pred make_command_string(string::in, quote_char::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module libs.process_util.

:- import_module bool.
:- import_module io.call_system.
:- import_module io.file.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

invoke_system_command(Globals, ProgressStream,
        CmdOutputStream, Verbosity, Command, Succeeded, !IO) :-
    invoke_system_command_maybe_filter_output(Globals, ProgressStream,
        CmdOutputStream, Verbosity, Command, no, Succeeded, !IO).

invoke_system_command_maybe_filter_output(Globals, ProgressStream,
        CmdOutputStream, Verbosity, Command, MaybeProcessOutput,
        Succeeded, !IO) :-
    % This predicate shouldn't alter the exit status of mercury_compile.
    io.get_exit_status(OldStatus, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Verbosity = cmd_verbose,
        PrintCommand = Verbose
    ;
        Verbosity = cmd_verbose_commands,
        globals.lookup_bool_option(Globals, verbose_commands, PrintCommand)
    ),
    (
        PrintCommand = yes,
        io.format(ProgressStream,
            "%% Invoking system command `%s'...\n", [s(Command)], !IO),
        io.flush_output(ProgressStream, !IO)
    ;
        PrintCommand = no
    ),

    % The output from the command is written to a temporary file,
    % which is then written to the output stream. Without this,
    % the output from the command would go to the current C output
    % and error streams.

    io.file.make_temp_file(TmpFileResult, !IO),
    (
        TmpFileResult = ok(TmpFile),
        ( if use_dotnet then
            % XXX can't use Bourne shell syntax to redirect on .NET
            % XXX the output will go to the wrong place!
            CommandRedirected = Command
        else if use_win32 then
            % On windows, we can't in general redirect standard error
            % in the shell.
            CommandRedirected = string.format("%s > %s",
                [s(Command), s(TmpFile)])
        else
            CommandRedirected = string.format("%s > %s 2>&1",
                [s(Command), s(TmpFile)])
        ),
        io.call_system.call_system_return_signal(CommandRedirected,
            CmdResult, !IO),
        (
            CmdResult = ok(exited(Status)),
            maybe_write_string(ProgressStream, PrintCommand, "% done.\n", !IO),
            ( if Status = 0 then
                CommandSucceeded = succeeded
            else
                % The command should have produced output describing the error.
                CommandSucceeded = did_not_succeed
            )
        ;
            CmdResult = ok(signalled(Signal)),
            string.format("system command received signal %d.", [i(Signal)],
                ErrorMsg),
            report_error(ProgressStream, ErrorMsg, !IO),
            % Also report the error to standard output, because if we raise the
            % signal, this error may not ever been seen, the process stops, and
            % the user is confused.
            io.stdout_stream(StdOut, !IO),
            report_error(StdOut, ErrorMsg, !IO),

            % Make sure the current process gets the signal. Some systems (e.g.
            % Linux) ignore SIGINT during a call to system().
            raise_signal(Signal, !IO),
            CommandSucceeded = did_not_succeed
        ;
            CmdResult = error(Error),
            report_error(ProgressStream, io.error_message(Error), !IO),
            CommandSucceeded = did_not_succeed
        )
    ;
        TmpFileResult = error(Error),
        report_error(ProgressStream,
            "Could not create temporary file: " ++ error_message(Error), !IO),
        TmpFile = "",
        CommandSucceeded = did_not_succeed
    ),

    ( if
        MaybeProcessOutput = yes(ProcessOutput),
        % We can't do bash style redirection on .NET.
        not use_dotnet
    then
        io.file.make_temp_file(ProcessedTmpFileResult, !IO),
        (
            ProcessedTmpFileResult = ok(ProcessedTmpFile),

            % XXX we should get rid of use_win32
            ( if use_win32 then
                get_system_env_type(Globals, SystemEnvType),
                ( if SystemEnvType = env_type_powershell then
                    ProcessOutputRedirected = string.format(
                        "Get-context %s | %s > %s 2>&1",
                        [s(TmpFile), s(ProcessOutput), s(ProcessedTmpFile)])
                else
                    % On windows, we can't in general redirect standard
                    % error in the shell.
                    ProcessOutputRedirected = string.format("%s < %s > %s",
                        [s(ProcessOutput), s(TmpFile), s(ProcessedTmpFile)])
                )
            else
                ProcessOutputRedirected = string.format("%s < %s > %s 2>&1",
                    [s(ProcessOutput), s(TmpFile), s(ProcessedTmpFile)])
            ),
            (
                PrintCommand = yes,
                io.format(ProgressStream,
                    "%% Invoking system command `%s'...\n",
                        [s(ProcessOutputRedirected)], !IO),
                io.flush_output(ProgressStream, !IO)
            ;
                PrintCommand = no
            ),
            io.call_system.call_system_return_signal(ProcessOutputRedirected,
                ProcessOutputResult, !IO),
            io.file.remove_file(TmpFile, _, !IO),
            (
                ProcessOutputResult = ok(exited(ProcessOutputStatus)),
                maybe_write_string(ProgressStream, PrintCommand,
                    "% done.\n", !IO),
                ( if ProcessOutputStatus = 0 then
                    ProcessOutputSucceeded = succeeded
                else
                    % The command should have produced output
                    % describing the error.
                    ProcessOutputSucceeded = did_not_succeed
                )
            ;
                ProcessOutputResult = ok(signalled(ProcessOutputSignal)),
                % Make sure the current process gets the signal. Some systems
                % (e.g. Linux) ignore SIGINT during a call to system().
                raise_signal(ProcessOutputSignal, !IO),
                report_error(ProgressStream,
                    "system command received signal "
                    ++ int_to_string(ProcessOutputSignal) ++ ".", !IO),
                ProcessOutputSucceeded = did_not_succeed
            ;
                ProcessOutputResult = error(ProcessOutputError),
                ProcessOutputErrorMsg = io.error_message(ProcessOutputError),
                report_error(ProgressStream, ProcessOutputErrorMsg, !IO),
                ProcessOutputSucceeded = did_not_succeed
            )
        ;
            ProcessedTmpFileResult = error(ProcessTmpError),
            ProcessTmpErrorMsg = io.error_message(ProcessTmpError),
            report_error(ProgressStream, ProcessTmpErrorMsg, !IO),
            ProcessOutputSucceeded = did_not_succeed,
            ProcessedTmpFile = ""
        )
    else
        ProcessOutputSucceeded = succeeded,
        ProcessedTmpFile = TmpFile
    ),
    Succeeded = CommandSucceeded `and` ProcessOutputSucceeded,

    % Write the output to the error stream.

    % XXX Why do we try to do this EVEN WHEN the code above had not Succeeded?
    io.read_named_file_as_string(ProcessedTmpFile, TmpFileRes, !IO),
    (
        TmpFileRes = ok(TmpFileString),
        io.write_string(CmdOutputStream, TmpFileString, !IO)
    ;
        TmpFileRes = error(TmpFileError),
        report_error(ProgressStream,
            "error opening command output: " ++ io.error_message(TmpFileError),
            !IO)
    ),
    io.file.remove_file(ProcessedTmpFile, _, !IO),
    io.set_exit_status(OldStatus, !IO).

%-----------------------------------------------------------------------------%

invoke_long_system_command(Globals,
        ProgressStream, CmdOutputStream, Verbosity,
        Cmd, Args, Succeeded, !IO) :-
    invoke_long_system_command_maybe_filter_output(Globals,
        ProgressStream, CmdOutputStream, Verbosity,
        Cmd, "", Args, no, Succeeded, !IO).

invoke_long_system_command_maybe_filter_output(Globals,
        ProgressStream, CmdOutputStream, Verbosity,
        Cmd, NonAtArgs, Args, MaybeProcessOutput, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, restricted_command_line,
        RestrictedCommandLine),
    (
        RestrictedCommandLine = yes,

        % Avoid generating very long command lines by using @files.
        open_temp_output(TmpFileResult, !IO),
        (
            TmpFileResult = ok({TmpFile, TmpStream}),

            % We need to escape any \ before writing them to the file,
            % otherwise we lose them.
            TmpFileArgs = string.replace_all(Args, "\\", "\\\\"),

            io.write_string(TmpStream, TmpFileArgs, !IO),
            io.close_output(TmpStream, !IO),

            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            AtFileName = at_file_name(Globals, TmpFile),
            (
                VeryVerbose = yes,
                io.format(ProgressStream, "%% Args placed in %s: `%s'\n",
                    [s(AtFileName), s(TmpFileArgs)], !IO),
                io.flush_output(ProgressStream, !IO)
            ;
                VeryVerbose = no
            ),

            ( if NonAtArgs = "" then
                string.format("%s %s", [s(Cmd), s(AtFileName)], FullCmd)
            else
                string.format("%s %s %s",
                    [s(Cmd), s(NonAtArgs), s(AtFileName)], FullCmd)
            ),
            invoke_system_command_maybe_filter_output(Globals,
                ProgressStream, CmdOutputStream, Verbosity,
                FullCmd, MaybeProcessOutput, Succeeded0, !IO),

            io.file.remove_file(TmpFile, RemoveResult, !IO),
            (
                RemoveResult = ok,
                Succeeded = Succeeded0
            ;
                RemoveResult = error(_),
                Succeeded = did_not_succeed
            )
        ;
            TmpFileResult = error(ErrorMessage),
            io.format(ProgressStream, "%s\n", [s(ErrorMessage)], !IO),
            Succeeded = did_not_succeed
        )
    ;
        RestrictedCommandLine = no,
        ( if NonAtArgs = "" then
            string.format("%s %s", [s(Cmd), s(Args)], FullCmd)
        else
            string.format("%s %s %s", [s(Cmd), s(NonAtArgs), s(Args)], FullCmd)
        ),
        invoke_system_command_maybe_filter_output(Globals,
            ProgressStream, CmdOutputStream, Verbosity,
            FullCmd, MaybeProcessOutput, Succeeded, !IO)
    ).

    % Form the name of an @file given a file name.
    % On some systems we need to escape the `@' character.
    %
:- func at_file_name(globals, string) = string.

at_file_name(Globals, FileName) = AtFileName :-
    get_system_env_type(Globals, EnvType),
    (
        EnvType = env_type_powershell,
        AtFileName = "`@" ++ FileName
    ;
        ( EnvType = env_type_posix
        ; EnvType = env_type_cygwin
        ; EnvType = env_type_msys
        ; EnvType = env_type_win_cmd
        ),
        AtFileName = "@" ++ FileName
    ).

%-----------------------------------------------------------------------------%

make_command_string(String0, QuoteType, String) :-
    ( if use_win32 then
        (
            QuoteType = forward,
            Quote = " '"
        ;
            QuoteType = double,
            Quote = " """
        ),
        string.append_list(["sh -c ", Quote, String0, Quote], String)
    else
        String = String0
    ).

%-----------------------------------------------------------------------------%

    % Are we compiling in a .NET environment?
    %
:- pred use_dotnet is semidet.
:- pragma foreign_proc("C#",
    use_dotnet,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
% The following clause is only used if there is no matching foreign_proc.
use_dotnet :-
    semidet_fail.

    % Are we compiling in a win32 environment?
    %
    % If in doubt, use_win32 should succeed. This is only used to decide
    % whether to invoke Bourne shell command and shell scripts directly,
    % or whether to invoke them via `sh -c ...'. The latter should work
    % correctly in a Unix environment too, but is a little less efficient
    % since it invokes another process.
    %
:- pred use_win32 is semidet.
:- pragma foreign_proc("C",
    use_win32,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_WIN32
    SUCCESS_INDICATOR = 1;
#else
    SUCCESS_INDICATOR = 0;
#endif
").
% The following clause is only used if there is no matching foreign_proc.
% See comment above for why it is OK to just succeed here.
use_win32 :-
    semidet_succeed.

%-----------------------------------------------------------------------------%
:- end_module system_cmds.
%-----------------------------------------------------------------------------%
