%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2005 The University of Melbourne.
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
:- pred server_name(string::out, io::di, io::uo) is det.

:- func getpid = int.

:- implementation.

:- import_module list.
:- import_module require.
:- import_module string.

make_pipe_cmd(PipeName) = Cmd :-
	mkfifo_cmd(CmdName),
	( CmdName = "" ->
		error("make_pipe_cmd: do not know what command to use")
	;
		string__format("%s %s", [s(CmdName), s(PipeName)], Cmd)
	).

server_name(ServerName, !IO) :-
	io__make_temp(TmpFile, !IO),
	hostname_cmd(HostnameCmd),
	ServerRedirectCmd =
		string__format("%s > %s", [s(HostnameCmd), s(TmpFile)]),
	io__call_system(ServerRedirectCmd, Res1, !IO),
	( Res1 = ok(0) ->
		io__open_input(TmpFile, TmpRes, !IO),
		( TmpRes = ok(TmpStream) ->
			io__read_file_as_string(TmpStream, TmpReadRes, !IO),
			(
				TmpReadRes = ok(ServerNameNl),
				(
					string__remove_suffix(ServerNameNl,
						"\n", ServerNamePrime)
				->
					ServerName = ServerNamePrime
				;
					error("malformed server name")
				)
			;
				TmpReadRes = error(_, _),
				error("cannot read server's name")
			),
			io__close_input(TmpStream, !IO)
		;
			error("cannot open file to find the server's name")
		),
		io__remove_file(TmpFile, _, !IO)
	;
		error("cannot execute cmd to find the server's name")
	).

:- pred mkfifo_cmd(string::out) is det.

:- pragma foreign_proc("C",
	mkfifo_cmd(Mkfifo::out),
	[will_not_call_mercury, promise_pure],
"
	/* shut up warnings about casting away const */
	Mkfifo = (MR_String) (MR_Integer) MR_MKFIFO;
").

:- pred hostname_cmd(string::out) is det.

:- pragma foreign_proc("C",
	hostname_cmd(Hostname::out),
	[will_not_call_mercury, promise_pure],
"
	/* shut up warnings about casting away const */
	Hostname = (MR_String) (MR_Integer) MR_HOSTNAMECMD;
").

:- pragma foreign_decl("C",
"
#ifdef	MR_DEEP_PROFILER_ENABLED
#include	<sys/types.h>
#include	<unistd.h>
#endif
").

:- pragma foreign_proc("C",
	getpid = (Pid::out),
	[will_not_call_mercury, promise_pure],
"
#ifdef	MR_DEEP_PROFILER_ENABLED
	Pid = getpid();
#else
	MR_fatal_error(""the deep profiler is not supported"");
#endif
").
