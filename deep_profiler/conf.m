%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This module contains primitives whose parameters are decided by
% ../configure.in. This module picks them up from the #defines put into
% runtime/mercury_conf.h by the configure script.

:- module conf.

:- interface.

:- import_module io.

	% Given a pathname, return a shell command that will create
	% a named pipe with that pathname.
:- func make_pipe_cmd(string) = string.

	% The name of the server on which mdprof is being run.
:- pred server_name(string::out, io__state::di, io__state::uo) is det.

:- implementation.

:- import_module string, list, require.

make_pipe_cmd(PipeName) = Cmd :-
	mkfifo_cmd(CmdName),
	( CmdName = "" ->
		error("make_pipe_cmd: do not know what command to use")
	;
		string__format("%s %s", [s(CmdName), s(PipeName)], Cmd)
	).

server_name(ServerName) -->
	io__make_temp(TmpFile),
	{ hostname_cmd(HostnameCmd) },
	{ ServerRedirectCmd =
		string__format("%s > %s", [s(HostnameCmd), s(TmpFile)]) },
	io__call_system(ServerRedirectCmd, Res1),
	( { Res1 = ok(0) } ->
		io__see(TmpFile, Res2),
		( { Res2 = ok } ->
			io__read_file(Res3),
			( { Res3 = ok(ServerNameChars0) } ->
				(
					{ list__remove_suffix(ServerNameChars0,
						['\n'], ServerNameChars) }
				->
					{ string__from_char_list(
						ServerNameChars, ServerName) },
					io__seen
				;
					{ error("malformed server name") }
				)
			;
				{ error("cannot read server's name") }
			)
		;
			{ error("cannot open file to out the server's name") }
		)
	;
		{ error("cannot execute cmd to find out the server's name") }
	).

:- pred mkfifo_cmd(string::out) is det.

:- pragma foreign_proc("C", mkfifo_cmd(Mkfifo::out),
	[will_not_call_mercury, promise_pure],
"
	/* shut up warnings about casting away const */
	Mkfifo = (MR_String) (MR_Integer) MR_MKFIFO;
").

:- pred hostname_cmd(string::out) is det.

:- pragma foreign_proc("C", hostname_cmd(Hostname::out),
	[will_not_call_mercury, promise_pure],
"
	/* shut up warnings about casting away const */
	Hostname = (MR_String) (MR_Integer) MR_HOSTNAMECMD;
").
