%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author of initial version: conway.
% Author of this version: zs.
%
% This file contains the CGI "script" that is executed by the web server
% to handle web page requests implemented by the Mercury deep profiler server.

:- module mdprof_cgi.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module profile, interface, startup, query, conf, timeout, util.
:- import_module bool, char, string, int, array, list, set.
:- import_module require, std_util, getopt.

:- import_module int, string, list, array, map, exception, require, library.

% The web server should always set QUERY_STRING. It may also pass its contents
% as arguments, but if any characters specials to the shell occur in the query,
% they will screw up the argument list. We therefore look at the argument list
% only if QUERY_STRING isn't set, which means that the program was invoked
% from the command line for debugging.

main -->
	write_html_header,
	io__get_environment_var("QUERY_STRING", MaybeQueryString),
	(
		{ MaybeQueryString = yes(QueryString0) },
		{ getopt__process_options(option_ops(short, long, defaults),
			[], _, MaybeOptions) },
		{
			MaybeOptions = ok(Options)
		;
			MaybeOptions = error(_Msg),
			error("mdprof_cgi: error parsing empty command line")
		},
		{ split(QueryString0, query_separator_char, Pieces) },
		( { Pieces = [CmdStr, PrefStr, FileName] } ->
			{ Cmd = url_component_to_cmd(CmdStr, menu) },
			process_query(Cmd, yes(PrefStr), FileName,
				Options)
		; { Pieces = [CmdStr, FileName] } ->
			{ Cmd = url_component_to_cmd(CmdStr, menu) },
			process_query(Cmd, no, FileName, Options)
		; { Pieces = [FileName] } ->
			process_query(menu, no, FileName, Options)
		;
			io__set_exit_status(1),
			% Give the simplest URL in the error message.
			io__write_string("Bad URL; expected filename\n")
		)
	;
		{ MaybeQueryString = no },
		process_command_line
	).


:- pred process_command_line(io__state::di, io__state::uo) is cc_multi.

process_command_line -->
	io__progname_base(mdprof_cgi_progname, ProgName),
	io__command_line_arguments(Args0),
	% io__write_string("Args0: "),
	% io__write_list(Args0, " ", write_bracketed_string),
	% io__nl,
	{ getopt__process_options(option_ops(short, long, defaults),
		Args0, Args, MaybeOptions) },
	(
		{ MaybeOptions = ok(Options) },
		{ lookup_bool_option(Options, help, Help) },
		{ lookup_bool_option(Options, version, Version) },
		(
			{ Help = yes },
			write_help_message(ProgName)
		;
			{ Help = no }
		),
		(
			{ Version = yes },
			write_version_message(ProgName)
		;
			{ Version = no }
		),
		( { Help = no, Version = no } ->
			process_args(ProgName, Args, Options)
		;
			[]
		)
	;
		{ MaybeOptions = error(Msg) },
		io__set_exit_status(1),
		io__format("%s: error parsing options: %s\n",
			[s(ProgName), s(Msg)])
	).

:- func mdprof_cgi_progname = string.

mdprof_cgi_progname = "mdprof_cgi".

:- pred write_version_message(string::in, io__state::di, io__state::uo) is det.

write_version_message(ProgName) -->
	{ library__version(Version) },
	io__write_string(ProgName),
	io__write_string(": Mercury deep profiler"),
	io__nl,
	io__write_string(Version),
	io__nl.

:- pred write_help_message(string::in, io__state::di, io__state::uo) is det.

write_help_message(ProgName) -->
	% The options are deliberately not documented; they change
	% quite rapidly, based on the debugging needs of the moment.
	% The optional filename argument is also for implementors only.
	io__format("Usage: %s\n", [s(ProgName)]),
	io__format("This program doesn't expect any arguments;\n", []),
	io__format("instead it decides what to do based on the\n", []),
	io__format("QUERY_STRING environment variable.\n", []).

%-----------------------------------------------------------------------------%

:- pred process_args(string::in, list(string)::in, option_table::in,
	io__state::di, io__state::uo) is cc_multi.

process_args(ProgName, Args, Options) -->
	( { Args = [FileName] } ->
		% Although this mode of usage is not intended for production
		% use, allowing the filename and a limited range of commands
		% to be supplied on the command line makes debugging very much
		% easier.
		process_query(default_cmd(Options), no, FileName, Options)
	;
		io__set_exit_status(1),
		write_help_message(ProgName)
		% io__write_list(Args, " ", write_bracketed_string)
	).

% This predicate is for debugging the command line given to mdprof_cgi by the
% web server, should that be necessary
%
% :- pred write_bracketed_string(string::in, io__state::di, io__state::uo)
% 	is det.
% 
% write_bracketed_string(S) -->
% 	io__write_string("<"),
% 	io__write_string(S),
% 	io__write_string(">").

:- pred write_html_header(io__state::di, io__state::uo) is det.

write_html_header -->
	io__write_string(html_header_text),
	io__flush_output.

:- func html_header_text = string.

html_header_text = "Content-type: text/html\n\n".

%-----------------------------------------------------------------------------%

:- pred process_query(cmd::in, maybe(string)::in, string::in,
	option_table::in, io__state::di, io__state::uo) is cc_multi.

process_query(Cmd, MaybePrefStr, DataFileName, Options) -->
	{
		MaybePrefStr = yes(PrefStr),
		MaybePref = url_component_to_maybe_pref(PrefStr)
	;
		MaybePrefStr = no,
		MaybePref = no
	},
	{
		MaybePref = yes(Pref)
	;
		MaybePref = no,
		Pref = default_preferences
	},
	{ ToServerPipe = to_server_pipe_name(DataFileName) },
	{ FromServerPipe = from_server_pipe_name(DataFileName) },
	{ StartupFile = server_startup_name(DataFileName) },
	{ MutexFile = mutex_file_name(DataFileName) },
	{ lookup_bool_option(Options, debug, Debug) },
	{ WantFile = want_file_name },
	make_want_file(WantFile),
	get_lock(Debug, MutexFile),
	(
		{ Debug = yes }
		% Do not set up any cleanups; leave all files around,
		% since they may be needed for postmortem examination.
	;
		{ Debug = no },
		setup_signals(MutexFile, want_dir, want_prefix)
	),
	check_for_existing_fifos(ToServerPipe, FromServerPipe, FifoCount),
	( { FifoCount = 0 } ->
		handle_query_from_new_server(Cmd, Pref, DataFileName,
			ToServerPipe, FromServerPipe, StartupFile,
			MutexFile, WantFile, Options)
	; { FifoCount = 2 } ->
		handle_query_from_existing_server(Cmd, Pref,
			ToServerPipe, FromServerPipe,
			MutexFile, WantFile, Options)
	;
		release_lock(Debug, MutexFile),
		remove_want_file(WantFile),
		io__set_exit_status(1),
		io__write_string("mdprof internal error: bad fifo count")
	).

% Handle the given query using the existing server. Delete the mutex and want
% files when we get out of the critical region.

:- pred handle_query_from_existing_server(cmd::in, preferences::in,
	string::in, string::in, string::in, string::in, option_table::in,
	io__state::di, io__state::uo) is det.

handle_query_from_existing_server(Cmd, Pref, ToServerPipe, FromServerPipe,
		MutexFile, WantFile, Options) -->
	{ lookup_bool_option(Options, debug, Debug) },
	send_term(ToServerPipe, Debug, cmd_pref(Cmd, Pref)),
	release_lock(Debug, MutexFile),
	remove_want_file(WantFile),
	recv_string(FromServerPipe, Debug, ResponseFileName),
	{ CatCmd = string__format("cat %s", [s(ResponseFileName)]) },
	io__call_system(CatCmd, _),
	(
		{ Debug = yes }
		% Leave the response file to be examined.
	;
		{ Debug = no },
		io__remove_file(ResponseFileName, _)
	).

% Handle the given query and then become the new server. Delete the mutex
% and want files when we get out of the critical region.

:- pred handle_query_from_new_server(cmd::in, preferences::in, string::in,
	string::in, string::in, string::in, string::in, string::in,
	option_table::in, io__state::di, io__state::uo) is cc_multi.

handle_query_from_new_server(Cmd, Pref, FileName, ToServerPipe, FromServerPipe,
		StartupFile, MutexFile, WantFile, Options) -->
	server_name(Machine),
	{ lookup_bool_option(Options, canonical_clique, Canonical) },
	{ lookup_bool_option(Options, server_process, ServerProcess) },
	{ lookup_bool_option(Options, debug, Debug) },
	{ lookup_bool_option(Options, record_startup, RecordStartup) },
	( 
		{ RecordStartup = yes },
		io__open_output(StartupFile, StartupStreamRes),
		(
			{ StartupStreamRes = ok(StartupStream0) },
			{ MaybeStartupStream = yes(StartupStream0) },
			register_file_for_cleanup(StartupFile)
		;
			{ StartupStreamRes = error(_) },
			{ error("cannot create startup file") }
		)
	;
		{ RecordStartup = no },
		{ MaybeStartupStream = no }
	),
	read_and_startup(Machine, [FileName], Canonical, MaybeStartupStream,
		Res),
	(
		{ Res = ok(Deep) },
		try_exec(Cmd, Pref, Deep, HTML),
		(
			{ MaybeStartupStream = yes(StartupStream1) },
			io__format(StartupStream1, "query 0 output:\n%s\n",
				[s(HTML)]),
			% If we don't flush the output before the fork, it will
			% be flushed twice, once by the parent process and
			% once by the child process.
			io__flush_output(StartupStream1)
		;
			{ MaybeStartupStream = no }
		),
		(
			{ ServerProcess = no },
			% --no-server process should be specified only during
			% debugging.
			release_lock(Debug, MutexFile),
			remove_want_file(WantFile),
			io__write_string(HTML)
		;
			{ ServerProcess = yes },
			make_pipes(FileName, Success),
			(
				{ Success = yes },
				io__write_string(HTML),
				io__flush_output,
				start_server(Options,
					ToServerPipe, FromServerPipe,
					MaybeStartupStream,
					MutexFile, WantFile, Deep)
			;
				{ Success = no },
				release_lock(Debug, MutexFile),
				remove_want_file(WantFile),
				io__set_exit_status(1),
				io__write_string("could not make pipes\n")
			)
		)
	;
		{ Res = error(Error) },
		release_lock(Debug, MutexFile),
		remove_want_file(WantFile),
		io__set_exit_status(1),
		io__format("error reading data file: %s\n", [s(Error)])
	).

% Become the new server. Delete the mutex and want files when we get out
% of the critical region.

:- pred start_server(option_table::in, string::in, string::in,
	maybe(io__output_stream)::in, string::in, string::in, deep::in,
	io__state::di, io__state::uo) is cc_multi.

start_server(Options, ToServerPipe, FromServerPipe, MaybeStartupStream,
		MutexFile, WantFile, Deep) -->
	{ lookup_bool_option(Options, detach_process, DetachProcess) },
	{ lookup_bool_option(Options, record_loop, RecordLoop) },
	{ lookup_bool_option(Options, debug, Debug) },
	(
		{ DetachProcess = no },
		% We behave as if we were in the child, to allow the server
		% loop to be debugged.
		{ DetachRes = in_child(child_has_no_parent) }
	;
		{ DetachProcess = yes },
		detach_process(DetachRes)
	),
	(
		{ DetachRes = in_child(ChildHasParent) } ->
		% We are in the child; start serving queries.
		(
			{ ChildHasParent = child_has_parent },
			% Our parent process will perform the file removals
			% needed to exit the critical section; we don't
			% want to duplicate them. We also don't want to delete
			% the pipes we need or the startup file.
			unregister_file_for_cleanup(MutexFile),
			unregister_file_for_cleanup(WantFile),

			% We need to close stdout and stderr to let the web
			% server know that there will be no further outputs
			% on those streams. We also close stdin, since that may
			% also be a named pipe.
			%
			% The binary streams are clones of the text streams,
			% and we must close them too to let the web server
			% finish displaying the page.
			io__stdin_stream(StdIn),
			io__close_input(StdIn),
			io__stdout_stream(StdOut),
			io__close_output(StdOut),
			io__stderr_stream(StdErr),
			io__close_output(StdErr),
			io__current_binary_input_stream(BinaryStdIn),
			io__close_binary_input(BinaryStdIn),
			io__current_binary_output_stream(BinaryStdOut),
			io__close_binary_output(BinaryStdOut)
		;
			{ ChildHasParent = child_has_no_parent },
			% We don't actually have a parent process, so we need
			% to perform the file removals needed to exit the
			% critical section ourselves.
			release_lock(Debug, MutexFile),
			remove_want_file(WantFile)
		),
		(
			{ RecordLoop = yes },
			{ MaybeDebugStream = MaybeStartupStream }
		;
			{ RecordLoop = no },
			{ MaybeDebugStream = no }
		),
		{ lookup_int_option(Options, timeout, TimeOut) },
		{ lookup_bool_option(Options, canonical_clique, Canonical) },
		server_loop(ToServerPipe, FromServerPipe, TimeOut,
			MaybeDebugStream, Debug, Canonical, 0, Deep)
	;
		{ DetachRes = in_parent } ->
		% We are in the parent after we spawned the child. We cause
		% the process to exit simply by not calling server_loop.
		% 
		% We leave the pipes and the startup file; we clean up only
		% the files involved in the critical section.
		release_lock(Debug, MutexFile),
		remove_want_file(WantFile)
	;
		% We are in the parent because the fork failed. Again we cause
		% the process to exit simply by not calling server_loop, but we
		% also report the failure through the exit status. We don't
		% report it via the generated web page, since the cause could
		% be transitory and may not recur.
		%
		% This deletes all the files created by the process, including
		% WantFile and MutexFile, with MutexFile being deleted last.
		delete_cleanup_files,
		io__set_exit_status(1)
	).

:- pred server_loop(string::in, string::in, int::in,
	maybe(io__output_stream)::in, bool::in, bool::in, int::in, deep::in,
	io__state::di, io__state::uo) is cc_multi.

server_loop(ToServerPipe, FromServerPipe, TimeOut0, MaybeStartupStream,
		Debug, Canonical, QueryNum0, Deep0) -->
	setup_timeout(TimeOut0),
	{ QueryNum = QueryNum0 + 1 },
	recv_term(ToServerPipe, Debug, CmdPref0),
	(
		{ MaybeStartupStream = yes(StartupStream0) },
		io__format(StartupStream0, "server loop query %d\n",
			[i(QueryNum)]),
		io__write(StartupStream0, CmdPref0),
		io__nl(StartupStream0),
		io__flush_output(StartupStream0)
	;
		{ MaybeStartupStream = no }
	),
	{ CmdPref0 = cmd_pref(Cmd0, Pref0) },
	( { Cmd0 = restart } ->
		read_and_startup(Deep0 ^ server_name, [Deep0 ^ data_file_name],
			Canonical, MaybeStartupStream, MaybeDeep),
		(
			{ MaybeDeep = ok(Deep) },
			{ MaybeMsg = no },
			{ Cmd = menu }
		;
			{ MaybeDeep = error(ErrorMsg) },
			{ MaybeMsg = yes(ErrorMsg) },
			{ Deep = Deep0 },
			{ Cmd = quit }
		)
	;
		{ Deep = Deep0 },
		{ MaybeMsg = no },
		{ Cmd = Cmd0 }
	),
	(
		{ MaybeMsg = yes(HTML) }
	;
		{ MaybeMsg = no },
		try_exec(Cmd, Pref0, Deep, HTML)
	),

	{ ResponseFileName =
		response_file_name(Deep0 ^ data_file_name, QueryNum) },
	io__open_output(ResponseFileName, ResponseRes),
	(
		{ ResponseRes = ok(ResponseStream) }
	;
		{ ResponseRes = error(_) },
		{ error("cannot open response file") }
	),
	io__write_string(ResponseStream, HTML),
	io__close_output(ResponseStream),

	send_string(FromServerPipe, Debug, ResponseFileName),

	(
		{ MaybeStartupStream = yes(StartupStream1) },
		io__format(StartupStream1, "query %d output:\n%s\n",
			[i(QueryNum), s(HTML)]),
		io__flush_output(StartupStream1)
	;
		{ MaybeStartupStream = no }
	),

	( { Cmd = quit } ->
		% The lack of a recursive call here shuts down the server.
		%
		% This deletes all the files created by the process, including
		% WantFile and MutexFile, with MutexFile being deleted last.
		delete_cleanup_files
	; { Cmd = timeout(TimeOut) } ->
		server_loop(ToServerPipe, FromServerPipe, TimeOut,
			MaybeStartupStream, Debug, Canonical, QueryNum, Deep)
	;
		server_loop(ToServerPipe, FromServerPipe, TimeOut0,
			MaybeStartupStream, Debug, Canonical, QueryNum, Deep)
	).

%-----------------------------------------------------------------------------%

:- pred make_pipes(string::in, bool::out, io__state::di, io__state::uo) is det.

make_pipes(FileName, Success) -->
	{ ToServerPipe = to_server_pipe_name(FileName) },
	{ FromServerPipe = from_server_pipe_name(FileName) },
	{ MakeToServerPipeCmd = make_pipe_cmd(ToServerPipe) },
	{ MakeFromServerPipeCmd = make_pipe_cmd(FromServerPipe) },
	io__call_system(MakeToServerPipeCmd, ToServerRes),
	io__call_system(MakeFromServerPipeCmd, FromServerRes),
	(
		{ ToServerRes = ok(0) },
		{ FromServerRes = ok(0) }
	->
		register_file_for_cleanup(ToServerPipe),
		register_file_for_cleanup(FromServerPipe),
		{ Success = yes }
	;
		% In case one of the pipes *was* created. We ignore the
		% return values because at least one of these calls *will*
		% fail (since we did not create both pipes), and if we can't
		% remove a named pipe we did succeed in creating, then
		% something is so screwed up that probably there is nothing
		% we can do to fix the situation.
		io__remove_file(ToServerPipe, _),
		io__remove_file(FromServerPipe, _),
		{ Success = no }
	).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#ifdef	MR_DEEP_PROFILER_ENABLED
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <unistd.h>
#endif
").

:- pred check_for_existing_fifos(string::in, string::in, int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	check_for_existing_fifos(Fifo1::in, Fifo2::in, FifoCount::out,
		S0::di, S::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef	MR_DEEP_PROFILER_ENABLED
	struct stat	statbuf;
	int		status;

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
	--->	child_has_parent
	;	child_has_no_parent.

:- type detach_process_result
	--->	in_child(child_has_parent)
	;	in_parent
	;	fork_failed.

:- pred detach_process(detach_process_result::out,
	io__state::di, io__state::uo) is cc_multi.

detach_process(Result) -->
	raw_detach_process(ResCode),
	{ ResCode < 0 ->
		Result = fork_failed
	; ResCode > 0 ->
		Result = in_parent
	;
		Result = in_child(child_has_parent)
	}.

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

:- pred raw_detach_process(int::out, io__state::di, io__state::uo) is cc_multi.

:- pragma foreign_proc("C",
	raw_detach_process(ResCode::out, S0::di, S::uo),
	[will_not_call_mercury, promise_pure],
"{
#ifdef	MR_DEEP_PROFILER_ENABLED
	pid_t	status;

	fflush(stdout);
	fflush(stderr);
	status = fork();
	if (status < 0) {
		ResCode = -1;
	} else if (status > 0) {
		ResCode = 1;
	} else {
#ifdef	MR_HAVE_SETPGID
		/*
		** Try to detach the server process from the parent's process
		** group, in case it uses the number of processes in the
		** process group to decide when the cgi `script' is done
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

%-----------------------------------------------------------------------------%

:- type option
	--->	canonical_clique
	;	clique
	;	debug
	;	detach_process
	;	help
	;	modules
	;	proc
	;	quit
	;	root
	;	record_startup
	;	record_loop
	;	server_process
	;	timeout
	;	version
	;	write_query_string.

:- type options ---> options.
:- type option_table == (option_table(option)).

:- pred short(char::in, option::out) is semidet.

short('c',	canonical_clique).
short('C',	clique).
short('d',	debug).
short('m',	modules).
short('p',	proc).
short('q',	quit).
short('r',	root).
short('s',	server_process).
short('t',	timeout).
short('w',	write_query_string).

:- pred long(string::in, option::out) is semidet.

long("canonical-clique",	canonical_clique).
long("clique",			clique).
long("debug",			debug).
long("detach-process",		detach_process).
long("help",			help).
long("modules",			modules).
long("proc",			proc).
long("quit",			quit).
long("root",			root).
long("record-startup",		record_startup).
long("record-loop",		record_loop).
long("server-process",		server_process).
long("timeout",			timeout).
long("version",			version).
long("write-query-string",	write_query_string).

:- pred defaults(option::out, option_data::out) is nondet.

defaults(Option, Data) :-
	semidet_succeed,
	defaults0(Option, Data).

:- pred defaults0(option::out, option_data::out) is multi.

defaults0(canonical_clique,	bool(no)).
defaults0(clique,		int(0)).
defaults0(debug,		bool(no)).
defaults0(detach_process,	bool(yes)).
defaults0(help,			bool(no)).
defaults0(modules,		bool(no)).
defaults0(proc,			int(0)).
defaults0(quit,			bool(no)).
defaults0(root,			bool(no)).
defaults0(record_loop,		bool(yes)).
defaults0(record_startup,	bool(yes)).
defaults0(server_process,	bool(yes)).
defaults0(timeout,		int(30)).
defaults0(version,		bool(no)).
defaults0(write_query_string,	bool(yes)).

:- func default_cmd(option_table) = cmd.

default_cmd(Options) = Cmd :-
	lookup_bool_option(Options, quit, Quit),
	lookup_bool_option(Options, root, Root),
	lookup_bool_option(Options, modules, Modules),
	lookup_int_option(Options, clique, CliqueNum),
	lookup_int_option(Options, proc, ProcNum),
	( Root = yes ->
		Cmd = root(no)
	; Modules = yes ->
		Cmd = modules
	; CliqueNum > 0 ->
		Cmd = clique(CliqueNum)
	; ProcNum > 0 ->
		Cmd = proc(ProcNum)
	; Quit = yes ->
		Cmd = quit
	;
		Cmd = menu
	).

%-----------------------------------------------------------------------------%
