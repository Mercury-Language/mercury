%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This file contains the CGI "script" that is executed by the web server
% to handle web page requests implemented by the Mercury deep profiler server.
%
% A shell script installed as /usr/lib/cgi-bin/mdprof should invoke this
% program after setting up the executable search path in the environment
% to include the directory that contains the server program, mdprof_server.

:- module mdprof_cgi.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module interface, util.
:- import_module char, string, int, list, set, require, std_util.

main -->
	io__command_line_arguments(Args),
	( { Args = [] } ->
		io__get_environment_var("QUERY_STRING", MaybeQueryString),
		(
			{ MaybeQueryString = yes(QueryString0) },
			{ split(QueryString0, ('$'), Pieces) },
			( { Pieces = [CmdStr, PrefStr, FileName] } ->
				process_query(CmdStr, yes(PrefStr), FileName)
			; { Pieces = [CmdStr, FileName] } ->
				process_query(CmdStr, no, FileName)
			; { Pieces = [FileName] } ->
				process_query("menu", no, FileName)
			;
				io__write_string(
					"Bad URL; expected query$/full/path/name\n")
			)
		;
			{ MaybeQueryString = no }
		)
	;
		io__write_string("Usage: mdprof_cgi\n")
	).

:- pred process_query(string::in, maybe(string)::in, string::in,
	io__state::di, io__state::uo) is det.

process_query(CmdStr, MaybePrefStr, DataFileName) -->
	{ ToServer = to_server_pipe_name(DataFileName) },
	{ FromServer = from_server_pipe_name(DataFileName) },
	{ TestCmd = string__format("test -p %s -a -p %s",
		[s(ToServer), s(FromServer)]) },
	io__call_system(TestCmd, TestRes),
	io__write_string("Content-type: text/html\n\n"),
	(
		{ TestRes = ok(ExitStatus) },
		( { ExitStatus = 0 } ->
			{ MaybeError = no }
		;
			create_server(DataFileName, MaybeError)
		),
		(
			{ MaybeError = no },
			handle_query(CmdStr, MaybePrefStr,
				ToServer, FromServer)
		;
			{ MaybeError = yes(Error) },
			io__write_string(Error)
		)
	;
		{ TestRes = error(Err) },
		{ io__error_message(Err, Msg) },
		io__write_string(Msg)
	).

:- pred create_server(string::in, maybe(string)::out,
	io__state::di, io__state::uo) is det.

create_server(DataFileName, MaybeError) -->
	{ StartupFileName = server_startup_name(DataFileName) },
	{ ServerCmd = string__format(
		"%s %s < /dev/null > /dev/null 2> %s",
		[s(server_path_name), s(DataFileName),
			s(StartupFileName)]) },
	io__call_system(ServerCmd, Res),
	(
		{ Res = ok(ExitStatus) },
		( { ExitStatus = 0 } ->
			{ MaybeError = no }
		;
			{ MaybeError = yes("Command to start server failed") },
			{ ToServer = to_server_pipe_name(DataFileName) },
			{ FromServer = from_server_pipe_name(DataFileName) },
			{ RemoveToServerCmd = string__format(
				"rm -f %s", [s(ToServer)]) },
			{ RemoveFromServerCmd = string__format(
				"rm -f %s", [s(FromServer)]) },
			{ RemoveStartupFileCmd = string__format(
				"rm -f %s", [s(StartupFileName)]) },
			% We ignore any errors since we can't do anything
			% about them anyway.
			io__call_system(RemoveToServerCmd, _),
			io__call_system(RemoveFromServerCmd, _),
			io__call_system(RemoveStartupFileCmd, _)
		)
	;
		{ Res = error(Err) },
		{ io__error_message(Err, Msg) },
		{ MaybeError = yes(Msg) }
	).

:- func server_path_name = string.

server_path_name = "mdprof_server".

:- pred handle_query(string::in, maybe(string)::in, string::in, string::in,
	io__state::di, io__state::uo) is det.

handle_query(CmdStr, MaybePrefStr, ToServer, FromServer) -->
	{ MaybeCmd = url_component_to_cmd(CmdStr) },
	{
		MaybePrefStr = yes(PrefStr),
		MaybePref = url_component_to_preferences(PrefStr)
	;
		MaybePrefStr = no,
		MaybePref = yes(default_preferences)
	},
	( { MaybeCmd = yes(Cmd), MaybePref = yes(Pref) } ->
		to(ToServer, cmd_pref(Cmd, Pref)),
		from(FromServer, html(Page)),
		io__write_string(Page)
	;
		io__write_string("mdprof: unknown URL format")
	).
