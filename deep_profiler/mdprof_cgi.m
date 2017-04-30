%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mdprof_cgi.m.
% Author of initial version: conway.
% Author of this version: zs.
%
% This file contains the CGI "script" that is executed by the web server
% to handle web page requests implemented by the Mercury deep profiler server.
%
%---------------------------------------------------------------------------%

:- module mdprof_cgi.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module conf.
:- import_module interface.
:- import_module profile.
:- import_module query.
:- import_module startup.
:- import_module timeout.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

% The web server should always set QUERY_STRING. It may also pass its contents
% as arguments, but if any characters special to the shell occur in the query,
% they will screw up the argument list. We therefore look at the argument list
% only if QUERY_STRING isn't set, which means that the program was invoked
% from the command line for debugging.

main(!IO) :-
    write_html_header(!IO),
    io.get_environment_var("QUERY_STRING", MaybeQueryString, !IO),
    (
        MaybeQueryString = yes(QueryString0),
        getopt.process_options(
            option_ops_multi(short, long, defaults), [], _, MaybeOptions),
        (
            MaybeOptions = ok(Options)
        ;
            MaybeOptions = error(_Msg),
            error("mdprof_cgi: error parsing empty command line")
        ),
        string_to_maybe_query(QueryString0) = MaybeDeepQuery,
        (
            MaybeDeepQuery = yes(DeepQuery),
            DeepQuery = deep_query(MaybeCmd, DeepFileName, MaybePrefs),
            (
                MaybeCmd = yes(Cmd)
            ;
                MaybeCmd = no,
                Cmd = default_command
            ),
            process_query(Cmd, DeepFileName, MaybePrefs, Options, !IO)
        ;
            MaybeDeepQuery = no,
            io.set_exit_status(1, !IO),
            % Give the simplest URL in the error message.
            io.write_string("Bad URL; expected filename \n", !IO)
        )
    ;
        MaybeQueryString = no,
        process_command_line(!IO)
    ).

:- pred process_command_line(io::di, io::uo) is cc_multi.

process_command_line(!IO) :-
    io.progname_base(mdprof_cgi_progname, ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    trace [compiletime(flag("debug-args")), io(!DIO)] (
        io.write_string("command line: ", !DIO),
        io.write_list(Args0, " ", write_bracketed_string, !DIO),
        io.nl(!DIO)
    ),
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    (
        MaybeOptions = ok(Options),
        lookup_bool_option(Options, help, Help),
        lookup_bool_option(Options, version, Version),
        lookup_bool_option(Options, decode, Decode),
        lookup_bool_option(Options, decode_cmd, DecodeCmd),
        lookup_bool_option(Options, decode_prefs, DecodePrefs),
        (
            Help = yes,
            write_help_message(ProgName, !IO)
        ;
            Help = no
        ),
        (
            Version = yes,
            write_version_message(ProgName, !IO)
        ;
            Version = no
        ),
        ( if
            Decode = no,
            DecodeCmd = no,
            DecodePrefs = no
        then
            true
        else
            decode_input_lines(Decode, DecodeCmd, DecodePrefs, !IO)
        ),
        ( if
            Help = no,
            Version = no,
            Decode = no,
            DecodeCmd = no,
            DecodePrefs = no
        then
            process_args(ProgName, Args, Options, !IO)
        else
            true
        )
    ;
        MaybeOptions = error(Msg),
        io.set_exit_status(1, !IO),
        io.format("%s: error parsing options: %s\n",
            [s(ProgName), s(Msg)], !IO)
    ).

:- pred decode_input_lines(bool::in, bool::in, bool::in, io::di, io::uo)
    is det.

decode_input_lines(Decode, DecodeCmd, DecodePrefs, !IO) :-
    io.read_line_as_string(LineResult, !IO),
    (
        LineResult = ok(LineStr),
        (
            Decode = no
        ;
            Decode = yes,
            io.write_string("considering as query string:\n", !IO),
            string_to_maybe_query(LineStr) = MaybeQuery,
            (
                MaybeQuery = yes(deep_query(MaybeCmd, DeepFileName,
                    MaybePrefs)),
                io.write_string("Maybe Command:\n", !IO),
                io.write(MaybeCmd, !IO),
                io.nl(!IO),
                io.format("Deep File Name: %s\n", [s(DeepFileName)], !IO),
                % The preferences may fail to parse, in this case no
                % preferences are assumed.
                io.write_string("Maybe Preferences:\n", !IO),
                io.write(MaybePrefs, !IO),
                io.nl(!IO)
            ;
                MaybeQuery = no,
                io.write_string("invalid query string: " ++
                    "cannot split into components\n", !IO)
            )
        ),
        (
            DecodeCmd = no
        ;
            DecodeCmd = yes,
            io.write_string("considering as cmd string:\n", !IO),
            MaybeCmd1 = string_to_maybe_cmd(LineStr),
            (
                MaybeCmd1 = no,
                io.format("invalid command string %s\n", [s(LineStr)], !IO)
            ;
                MaybeCmd1 = yes(Cmd),
                io.write(Cmd, !IO),
                io.nl(!IO)
            )
        ),
        (
            DecodePrefs = no
        ;
            DecodePrefs = yes,
            io.write_string("considering as preference string:\n", !IO),
            MaybePref = string_to_maybe_pref(LineStr),
            (
                MaybePref = no,
                io.format("invalid preferences string %s\n", [s(LineStr)], !IO)
            ;
                MaybePref = yes(Pref),
                io.write(Pref, !IO),
                io.nl(!IO)
            )
        ),
        decode_input_lines(Decode, DecodeCmd, DecodePrefs, !IO)
    ;
        LineResult = error(Error),
        io.error_message(Error, Msg),
        io.format("%s\n", [s(Msg)], !IO)
    ;
        LineResult = eof
    ).

:- func mdprof_cgi_progname = string.

mdprof_cgi_progname = "mdprof_cgi".

:- pred write_version_message(string::in, io::di, io::uo) is det.

write_version_message(ProgName, !IO) :-
    library.version(Version, Fullarch),
    io.format("%s: Mercury deep profiler\n", [s(ProgName)], !IO),
    io.format("version: %s, on %s.\n",
        [s(Version), s(Fullarch)], !IO).

:- pred write_help_message(string::in, io::di, io::uo) is det.

write_help_message(ProgName, !IO) :-
    % The options are deliberately not documented; they change
    % quite rapidly, based on the debugging needs of the moment.
    % The optional filename argument is also for implementors only.
    io.format("Usage: %s\n", [s(ProgName)], !IO),
    io.format("This program doesn't expect any arguments;\n", [], !IO),
    io.format("instead it decides what to do based on the\n", [], !IO),
    io.format("QUERY_STRING environment variable.\n", [], !IO).

%---------------------------------------------------------------------------%

:- pred process_args(string::in, list(string)::in, option_table::in,
    io::di, io::uo) is cc_multi.

process_args(ProgName, Args, Options, !IO) :-
    ( if Args = [DeepFileName] then
        % Although this mode of usage is not intended for production use,
        % allowing the filename and a limited range of commands to be supplied
        % on the command line makes debugging very much easier.
        process_query(default_cmd(Options), DeepFileName, no, Options, !IO)
    else
        io.set_exit_status(1, !IO),
        write_help_message(ProgName, !IO),
        trace [compiletime(flag("debug-args")), io(!DIO)] (
            io.write_string("processed args: ", !DIO),
            io.write_list(Args, " ", write_bracketed_string, !DIO)
        )
    ).

% This predicate is for debugging the command line given to mdprof_cgi by the
% web server, should that be necessary.

:- pred write_bracketed_string(string::in, io::di, io::uo) is det.

write_bracketed_string(S, !IO) :-
    io.write_string("<", !IO),
    io.write_string(S, !IO),
    io.write_string(">", !IO).

:- pred write_html_header(io::di, io::uo) is det.

write_html_header(!IO) :-
    io.write_string(html_header_text, !IO),
    io.flush_output(!IO).

:- func html_header_text = string.

html_header_text = "Content-type: text/html\n\n".

%---------------------------------------------------------------------------%

:- pred process_query(cmd::in, string::in, maybe(preferences)::in,
    option_table::in, io::di, io::uo) is cc_multi.

process_query(Cmd0, DeepFileName0, MaybePref, Options0, !IO) :-
    ( if Cmd0 = deep_cmd_restart then
        % This process got started because there was no server, and this
        % process will become the new server, so the user just got the freshly
        % started server they asked for. There is no point in starting it
        % again. As it is, create_report would throw an exception for
        % deep_cmd_restart, expecting it to be filtered out by its usual caller
        % server_loop. To avoid the exception, we have to filter it out too.
        Cmd = deep_cmd_menu
    else
        Cmd = Cmd0
    ),
    (
        MaybePref = yes(Pref),
        PrefInd = given_pref(Pref)
    ;
        MaybePref = no,
        PrefInd = default_pref
    ),
    ( if
        string.remove_suffix(DeepFileName0, ".localhost", DeepFileNamePrime)
    then
        DeepFileName = DeepFileNamePrime,
        map.det_update(localhost, bool(yes), Options0, Options)
    else
        DeepFileName = DeepFileName0,
        Options = Options0
    ),
    ( if string.remove_suffix(DeepFileName, ".data", _BaseFileName) then
        ToServerPipe = to_server_pipe_name(DeepFileName),
        FromServerPipe = from_server_pipe_name(DeepFileName),
        StartupFile = server_startup_name(DeepFileName),
        MutexFile = mutex_file_name(DeepFileName),
        lookup_bool_option(Options, debug, Debug),
        WantFile = want_file_name,
        make_want_file(WantFile, !IO),
        get_lock(Debug, MutexFile, !IO),
        (
            Debug = yes
            % Do not set up any cleanups; leave all files around,
            % since they may be needed for postmortem examination.
        ;
            Debug = no,
            setup_signals(MutexFile, want_dir, want_prefix, !IO)
        ),
        check_for_existing_fifos(ToServerPipe, FromServerPipe, FifoCount, !IO),
        ( if FifoCount = 0 then
            handle_query_from_new_server(Cmd, PrefInd, DeepFileName,
                ToServerPipe, FromServerPipe, StartupFile, MutexFile, WantFile,
                Options, !IO)
        else if FifoCount = 2 then
            handle_query_from_existing_server(Cmd, PrefInd,
                ToServerPipe, FromServerPipe, MutexFile, WantFile, Options,
                !IO)
        else
            release_lock(Debug, MutexFile, !IO),
            remove_want_file(WantFile, !IO),
            io.set_exit_status(1, !IO),
            io.write_string("mdprof internal error: bad fifo count", !IO)
        )
    else
        io.set_exit_status(1, !IO),
        io.format("<h3> Invalid file name %s.<h3>\n\n",
            [s(DeepFileName)], !IO),
        io.write_string(
            "Deep profiling data files must have a .data suffix, " ++
            "to allow the deep profiler to locate any related files.\n", !IO)
    ).

    % This type is used to pass queries between the two servers.
    %
:- type cmd_pref
    --->    cmd_pref(cmd, preferences_indication).

    % Handle the given query using the existing server. Delete the mutex and
    % want files when we get out of the critical region.
    %
:- pred handle_query_from_existing_server(cmd::in, preferences_indication::in,
    string::in, string::in, string::in, string::in, option_table::in,
    io::di, io::uo) is det.

handle_query_from_existing_server(Cmd, PrefInd, ToServerPipe, FromServerPipe,
        MutexFile, WantFile, Options, !IO) :-
    lookup_bool_option(Options, debug, Debug),
    trace [compiletime(flag("debug_client_server")), io(!S)] (
        io.open_append("/tmp/deep_debug", Res1, !S),
        (
            Res1 = ok(DebugStream1),
            io.write_string(DebugStream1,
                "sending query to existing server.\n", !S),
            io.write(DebugStream1, cmd_pref(Cmd, PrefInd), !S),
            io.close_output(DebugStream1, !S)
        ;
            Res1 = error(_)
        )
    ),
    send_term(ToServerPipe, Debug, cmd_pref(Cmd, PrefInd), !IO),
    release_lock(Debug, MutexFile, !IO),
    remove_want_file(WantFile, !IO),
    recv_string(FromServerPipe, Debug, ResponseFileName, !IO),
    CatCmd = string.format("cat %s", [s(ResponseFileName)]),
    io.call_system(CatCmd, _, !IO),
    trace [compiletime(flag("debug_client_server")), io(!T)] (
        io.open_append("/tmp/deep_debug", Res2, !T),
        (
            Res2 = ok(DebugStream2),
            io.write_string(DebugStream2,
                "sending reply from existing server.\n", !T),
            io.close_output(DebugStream2, !T),
            DebugCatCmd = string.format("cat %s >> /tmp/deep_debug",
                [s(ResponseFileName)]),
            io.call_system(DebugCatCmd, _, !T)
        ;
            Res2 = error(_)
        )
    ),
    (
        Debug = yes
        % Leave the response file to be examined.
    ;
        Debug = no,
        io.remove_file(ResponseFileName, _, !IO)
    ).

    % Handle the given query and then become the new server. Delete the mutex
    % and want files when we get out of the critical region.
    %
:- pred handle_query_from_new_server(cmd::in, preferences_indication::in,
    string::in, string::in, string::in, string::in, string::in, string::in,
    option_table::in, io::di, io::uo) is cc_multi.

handle_query_from_new_server(Cmd, PrefInd, FileName, ToServerPipe,
        FromServerPipe, StartupFile, MutexFile, WantFile, Options, !IO) :-
    lookup_bool_option(Options, localhost, LocalHost),
    (
        LocalHost = no,
        server_name_port(Machine, !IO)
    ;
        LocalHost = yes,
        Machine = "localhost"
    ),
    script_name(ScriptName, !IO),
    lookup_bool_option(Options, canonical_clique, Canonical),
    lookup_bool_option(Options, server_process, ServerProcess),
    lookup_bool_option(Options, debug, Debug),
    lookup_bool_option(Options, record_startup, RecordStartup),
    (
        RecordStartup = yes,
        io.open_output(StartupFile, StartupStreamRes, !IO),
        (
            StartupStreamRes = ok(StartupStream0),
            MaybeStartupStream = yes(StartupStream0),
            register_file_for_cleanup(StartupFile, !IO)
        ;
            StartupStreamRes = error(_),
            error("cannot create startup file")
        )
    ;
        RecordStartup = no,
        MaybeStartupStream = no
    ),
    read_and_startup_default_deep_options(Machine, ScriptName, FileName,
        Canonical, MaybeStartupStream, [], StartupResult, !IO),
    (
        StartupResult = ok(Deep),
        Pref = solidify_preference(Deep, PrefInd),
        try_exec(Cmd, Pref, Deep, HTML),
        (
            MaybeStartupStream = yes(StartupStream1),
            io.format(StartupStream1, "query 0 output:\n%s\n", [s(HTML)], !IO),
            % If we don't flush the output before the fork, it will be flushed
            % twice, once by the parent process and once by the child process.
            io.flush_output(StartupStream1, !IO)
        ;
            MaybeStartupStream = no
        ),
        (
            ServerProcess = no,
            % --no-server-process should be specified only during debugging.
            release_lock(Debug, MutexFile, !IO),
            remove_want_file(WantFile, !IO),
            io.write_string(HTML, !IO)
        ;
            ServerProcess = yes,
            make_pipes(FileName, Success, !IO),
            (
                Success = yes,
                io.write_string(HTML, !IO),
                io.flush_output(!IO),
                start_server(Options, ToServerPipe, FromServerPipe,
                    MaybeStartupStream, MutexFile, WantFile, Deep, !IO)
            ;
                Success = no,
                release_lock(Debug, MutexFile, !IO),
                remove_want_file(WantFile, !IO),
                io.set_exit_status(1, !IO),
                io.write_string("could not make pipes\n", !IO)
            )
        )
    ;
        StartupResult = error(Error),
        release_lock(Debug, MutexFile, !IO),
        remove_want_file(WantFile, !IO),
        io.set_exit_status(1, !IO),
        io.format("%s\n", [s(Error)], !IO)
    ).

    % Become the new server. Delete the mutex and want files when we get out
    % of the critical region.
    %
:- pred start_server(option_table::in, string::in, string::in,
    maybe(io.output_stream)::in, string::in, string::in,
    deep::in, io::di, io::uo) is cc_multi.

start_server(Options, ToServerPipe, FromServerPipe, MaybeStartupStream,
        MutexFile, WantFile, Deep, !IO) :-
    lookup_bool_option(Options, detach_process, DetachProcess),
    lookup_bool_option(Options, record_loop, RecordLoop),
    lookup_bool_option(Options, debug, Debug),
    (
        DetachProcess = no,
        % We behave as if we were in the child, to allow the server
        % loop to be debugged.
        DetachRes = in_child(child_has_no_parent)
    ;
        DetachProcess = yes,
        detach_process(DetachRes, !IO)
    ),
    (
        DetachRes = in_child(ChildHasParent),
        % We are in the child; start serving queries.
        (
            ChildHasParent = child_has_parent,
            % Our parent process will perform the file removals needed to exit
            % the critical section; we don't want to duplicate them. We also
            % don't want to delete the pipes we need or the startup file.
            unregister_file_for_cleanup(MutexFile, !IO),
            unregister_file_for_cleanup(WantFile, !IO),

            % We need to close stdout and stderr to let the web server
            % know that there will be no further outputs on those streams.
            % We also close stdin, since that may also be a named pipe.
            %
            % The binary streams are clones of the text streams, and we must
            % close them too to let the web server finish displaying the page.
            io.stdin_stream(StdIn, !IO),
            io.close_input(StdIn, !IO),
            io.stdout_stream(StdOut, !IO),
            io.close_output(StdOut, !IO),
            io.stderr_stream(StdErr, !IO),
            io.close_output(StdErr, !IO),
            io.binary_input_stream(BinaryStdIn, !IO),
            io.close_binary_input(BinaryStdIn, !IO),
            io.binary_output_stream(BinaryStdOut, !IO),
            io.close_binary_output(BinaryStdOut, !IO)
        ;
            ChildHasParent = child_has_no_parent,
            % We don't actually have a parent process, so we need to perform
            % the file removals needed to exit the critical section ourselves.
            release_lock(Debug, MutexFile, !IO),
            remove_want_file(WantFile, !IO)
        ),
        (
            RecordLoop = yes,
            MaybeDebugStream = MaybeStartupStream
        ;
            RecordLoop = no,
            MaybeDebugStream = no
        ),
        lookup_int_option(Options, timeout, TimeOut),
        lookup_bool_option(Options, canonical_clique, Canonical),
        server_loop(ToServerPipe, FromServerPipe, TimeOut,
            MaybeDebugStream, Debug, Canonical, 0, Deep, !IO)
    ;
        DetachRes = in_parent,
        % We are in the parent after we spawned the child. We cause the process
        % to exit simply by not calling server_loop.
        %
        % We leave the pipes and the startup file; we clean up only the files
        % involved in the critical section.
        release_lock(Debug, MutexFile, !IO),
        remove_want_file(WantFile, !IO)
    ;
        DetachRes = fork_failed,
        % We are in the parent because the fork failed. Again we cause
        % the process to exit simply by not calling server_loop, but we also
        % report the failure through the exit status. We don't report it
        % via the generated web page, since the cause could be transitory
        % and may not recur.
        %
        % This deletes all the files created by the process, including
        % WantFile and MutexFile, with MutexFile being deleted last.
        delete_cleanup_files(!IO),
        io.set_exit_status(1, !IO)
    ).

:- pred server_loop(string::in, string::in, int::in,
    maybe(io.output_stream)::in, bool::in, bool::in, int::in,
    deep::in, io::di, io::uo) is cc_multi.

server_loop(ToServerPipe, FromServerPipe, TimeOut0, MaybeStartupStream,
        Debug, Canonical, QueryNum0, Deep0, !IO) :-
    setup_timeout(TimeOut0, !IO),
    QueryNum = QueryNum0 + 1,
    recv_term(ToServerPipe, Debug, CmdPref0, !IO),
    (
        MaybeStartupStream = yes(StartupStream0),
        io.format(StartupStream0, "server loop query %d\n",
            [i(QueryNum)], !IO),
        io.write(StartupStream0, CmdPref0, !IO),
        io.nl(StartupStream0, !IO),
        io.flush_output(StartupStream0, !IO)
    ;
        MaybeStartupStream = no
    ),
    CmdPref0 = cmd_pref(Cmd0, PrefInd0),

    ( if Cmd0 = deep_cmd_restart then
        read_and_startup_default_deep_options(Deep0 ^ server_name_port,
            Deep0 ^ script_name, Deep0 ^ data_file_name, Canonical,
            MaybeStartupStream, [], MaybeDeep, !IO),
        (
            MaybeDeep = ok(Deep),
            MaybeMsg = no,
            Cmd = deep_cmd_menu
        ;
            MaybeDeep = error(ErrorMsg),
            MaybeMsg = yes(ErrorMsg),
            Deep = Deep0,
            Cmd = deep_cmd_quit
        )
    else
        Deep = Deep0,
        MaybeMsg = no,
        Cmd = Cmd0
    ),
    Pref0 = solidify_preference(Deep, PrefInd0),
    (
        MaybeMsg = yes(HTML)
    ;
        MaybeMsg = no,
        try_exec(Cmd, Pref0, Deep, HTML)
    ),

    ResponseFileName = response_file_name(Deep0 ^ data_file_name, QueryNum),
    io.open_output(ResponseFileName, ResponseRes, !IO),
    (
        ResponseRes = ok(ResponseStream)
    ;
        ResponseRes = error(_),
        error("cannot open response file")
    ),
    io.write_string(ResponseStream, HTML, !IO),
    io.close_output(ResponseStream, !IO),

    send_string(FromServerPipe, Debug, ResponseFileName, !IO),

    (
        MaybeStartupStream = yes(StartupStream1),
        io.format(StartupStream1, "query %d output:\n%s\n",
            [i(QueryNum), s(HTML)], !IO),
        io.flush_output(StartupStream1, !IO)
    ;
        MaybeStartupStream = no
    ),

    ( if Cmd = deep_cmd_quit then
        % The lack of a recursive call here shuts down the server.
        %
        % This deletes all the files created by the process, including
        % WantFile and MutexFile, with MutexFile being deleted last.
        delete_cleanup_files(!IO)
    else if Cmd = deep_cmd_timeout(TimeOut) then
        server_loop(ToServerPipe, FromServerPipe, TimeOut, MaybeStartupStream,
            Debug, Canonical, QueryNum, Deep, !IO)
    else
        server_loop(ToServerPipe, FromServerPipe, TimeOut0, MaybeStartupStream,
            Debug, Canonical, QueryNum, Deep, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred make_pipes(string::in, bool::out, io::di, io::uo) is det.

make_pipes(FileName, Success, !IO) :-
    ToServerPipe = to_server_pipe_name(FileName),
    FromServerPipe = from_server_pipe_name(FileName),
    MakeToServerPipeCmd = make_pipe_cmd(ToServerPipe),
    MakeFromServerPipeCmd = make_pipe_cmd(FromServerPipe),
    io.call_system(MakeToServerPipeCmd, ToServerRes, !IO),
    io.call_system(MakeFromServerPipeCmd, FromServerRes, !IO),
    ( if
        ToServerRes = ok(0),
        FromServerRes = ok(0)
    then
        register_file_for_cleanup(ToServerPipe, !IO),
        register_file_for_cleanup(FromServerPipe, !IO),
        Success = yes
    else
        % In case one of the pipes *was* created. We ignore the return values
        % because at least one of these calls *will* fail (since we did not
        % create both pipes), and if we can't remove a named pipe we did
        % succeed in creating, then something is so screwed up that probably
        % there is nothing we can do to fix the situation.
        io.remove_file(ToServerPipe, _, !IO),
        io.remove_file(FromServerPipe, _, !IO),
        Success = no
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#ifdef  MR_DEEP_PROFILER_ENABLED
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <unistd.h>
#endif
").

:- pred check_for_existing_fifos(string::in, string::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    check_for_existing_fifos(Fifo1::in, Fifo2::in, FifoCount::out,
        S0::di, S::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef  MR_DEEP_PROFILER_ENABLED
    struct stat statbuf;
    int         status;

    FifoCount = 0;
    status = stat(Fifo1, &statbuf);
    if ((status == 0) && (S_ISFIFO(statbuf.st_mode))) {
        FifoCount++;
    }
    status = stat(Fifo2, &statbuf);
    if ((status == 0) && (S_ISFIFO(statbuf.st_mode))) {
        FifoCount++;
    }

    S = S0;
#else
    MR_fatal_error(""deep profiling not enabled"");
#endif
").

:- type child_has_parent
    --->    child_has_parent
    ;       child_has_no_parent.

:- type detach_process_result
    --->    in_child(child_has_parent)
    ;       in_parent
    ;       fork_failed.

:- pred detach_process(detach_process_result::out, io::di, io::uo) is cc_multi.

detach_process(Result, !IO) :-
    raw_detach_process(ResCode, !IO),
    ( if ResCode < 0 then
        Result = fork_failed
    else if ResCode > 0 then
        Result = in_parent
    else
        Result = in_child(child_has_parent)
    ).

    % Raw_detach_process performs a fork.
    %
    % If the fork succeeds, the result returned by detach_process is:
    %
    % - a positive number in the parent, and
    % - zero in the child.
    %
    % If the fork fails, the result returned by detach_process is:
    %
    % - a negative number in the parent (there is no child process).
    %
:- pred raw_detach_process(int::out, io::di, io::uo) is cc_multi.

:- pragma foreign_proc("C",
    raw_detach_process(ResCode::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"{
#ifdef  MR_DEEP_PROFILER_ENABLED
    pid_t   status;

    fflush(stdout);
    fflush(stderr);
    status = fork();
    if (status < 0) {
        ResCode = -1;
    } else if (status > 0) {
        ResCode = 1;
    } else {
#ifdef  MR_HAVE_SETPGID
        /*
        ** Try to detach the server process from the parent's process group,
        ** in case it uses the number of processes in the process group
        ** to decide when the cgi `script' is done.
        */
        setpgid(0, 0);
#else
        /* Hope that web server doesn't depend on the process group. */
#endif
        ResCode = 0;
    }

    S = S0;
#else
    MR_fatal_error(""deep profiling not enabled"");
#endif
}").

%---------------------------------------------------------------------------%

:- type option
    --->    canonical_clique
    ;       clique
    ;       debug
    ;       decode
    ;       decode_cmd
    ;       decode_prefs
    ;       detach_process
    ;       help
    ;       localhost
    ;       modules
    ;       proc
    ;       quit
    ;       root
    ;       record_startup
    ;       record_loop
    ;       server_process
    ;       timeout
    ;       version
    ;       write_query_string.

:- type option_table == (option_table(option)).

:- pred short(char::in, option::out) is semidet.

short('c',  canonical_clique).
short('C',  clique).
short('d',  debug).
short('m',  modules).
short('p',  proc).
short('q',  quit).
short('r',  root).
short('s',  server_process).
short('t',  timeout).
short('w',  write_query_string).

:- pred long(string::in, option::out) is semidet.

long("canonical-clique",    canonical_clique).
long("clique",              clique).
long("debug",               debug).
long("decode",              decode).
long("decode-cmd",          decode_cmd).
long("decode-prefs",        decode_prefs).
long("detach-process",      detach_process).
long("help",                help).
long("localhost",           localhost).
long("modules",             modules).
long("proc",                proc).
long("quit",                quit).
long("root",                root).
long("record-startup",      record_startup).
long("record-loop",         record_loop).
long("server-process",      server_process).
long("timeout",             timeout).
long("version",             version).
long("write-query-string",  write_query_string).

:- pred defaults(option::out, option_data::out) is multi.

defaults(canonical_clique,      bool(no)).
defaults(clique,                int(0)).
defaults(debug,                 bool(no)).
defaults(decode,                bool(no)).
defaults(decode_cmd,            bool(no)).
defaults(decode_prefs,          bool(no)).
defaults(detach_process,        bool(yes)).
defaults(help,                  bool(no)).
defaults(localhost,             bool(no)).
defaults(modules,               bool(no)).
defaults(proc,                  int(0)).
defaults(quit,                  bool(no)).
defaults(root,                  bool(no)).
defaults(record_loop,           bool(yes)).
defaults(record_startup,        bool(yes)).
defaults(server_process,        bool(yes)).
defaults(timeout,               int(30)).
defaults(version,               bool(no)).
defaults(write_query_string,    bool(yes)).

:- func default_cmd(option_table) = cmd.

default_cmd(Options) = Cmd :-
    lookup_bool_option(Options, quit, Quit),
    lookup_bool_option(Options, root, Root),
    lookup_bool_option(Options, modules, Modules),
    lookup_int_option(Options, clique, CliqueNum),
    lookup_int_option(Options, proc, ProcProcNum),
    ( if Root = yes then
        Cmd = deep_cmd_root(no)
    else if Modules = yes then
        Cmd = deep_cmd_program_modules
    else if CliqueNum > 0 then
        Cmd = deep_cmd_clique(clique_ptr(CliqueNum))
    else if ProcProcNum > 0 then
        Cmd = deep_cmd_proc(proc_static_ptr(ProcProcNum))
    else if Quit = yes then
        Cmd = deep_cmd_quit
    else
        Cmd = deep_cmd_menu
    ).

%---------------------------------------------------------------------------%
:- end_module mdprof_cgi.
%---------------------------------------------------------------------------%
