%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains the main server loop of the Mercury deep profiler:
% each iteration of the server loop serves up one web page.
%
% The module also contains test code for checking that all the web pages
% can be created without runtime aborts.

:- module server.

:- interface.

:- import_module profile, interface.
:- import_module bool, io.

:- pred test_server(string::in, preferences::in, deep::in,
	io__state::di, io__state::uo) is cc_multi.

:- pred server(int::in, bool::in, bool::in, deep::in,
	io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module query, startup, timeout.
:- import_module std_util, int, string, array, list, exception, require.

%-----------------------------------------------------------------------------%

test_server(DirName, Pref, Deep) -->
	{ string__format("test -d %s || mkdir -p %s",
		[s(DirName), s(DirName)], Cmd) },
	io__call_system(Cmd, _),
	{ array__max(Deep ^ clique_members, NumCliques) },
	test_cliques(1, NumCliques, DirName, Pref, Deep),
	{ array__max(Deep ^ proc_statics, NumProcStatics) },
	test_procs(1, NumProcStatics, DirName, Pref, Deep).

:- pred test_cliques(int::in, int::in, string::in, preferences::in, deep::in,
	io__state::di, io__state::uo) is cc_multi.

test_cliques(Cur, Max, DirName, Pref, Deep) -->
	( { Cur =< Max } ->
		try_exec(clique(Cur), Pref, Deep, HTML),
		write_html(DirName, "clique", Cur, HTML),
		test_cliques(Cur + 1, Max, DirName, Pref, Deep)
	;
		[]
	).

:- pred test_procs(int::in, int::in, string::in, preferences::in, deep::in,
	io__state::di, io__state::uo) is cc_multi.

test_procs(Cur, Max, DirName, Pref, Deep) -->
	( { Cur =< Max } ->
		try_exec(proc(Cur), Pref, Deep, HTML),
		write_html(DirName, "proc", Cur, HTML),
		test_procs(Cur + 1, Max, DirName, Pref, Deep)
	;
		[]
	).

:- pred write_html(string::in, string::in, int::in, string::in,
	io__state::di, io__state::uo) is det.

write_html(DirName, BaseName, Num, HTML) -->
	% For large programs such as the Mercury compiler, the profiler data
	% file may contain hundreds of thousands of cliques. We therefore put
	% each batch of pages in a different subdirectory, thus limiting the
	% number of files/subdirs in each directory.
	{ Bunch = (Num - 1) // 1000 },
	{ string__format("%s/%s_%04d",
		[s(DirName), s(BaseName), i(Bunch)], BunchName) },
	( { (Num - 1) rem 1000 = 0 } ->
		{ string__format("test -p %s || mkdir -p %s",
			[s(BunchName), s(BunchName)], Cmd) },
		io__call_system(Cmd, _)
	;
		[]
	),
	{ string__format("%s/%s_%06d.html",
		[s(BunchName), s(BaseName), i(Num)], FileName) },
	io__tell(FileName, _),
	io__write_string(HTML),
	io__told.

%-----------------------------------------------------------------------------%

server(TimeOut, Debug, CanonicalClique, Deep) -->
	{ DataFileName = Deep ^ data_file_name },
	{ InputPipe = to_server_pipe_name(DataFileName) },
	{ OutputPipe = from_server_pipe_name(DataFileName) },
	% Comment out the following line if you want to debug query processing.
	% Otherwise, the process started by mdb will exit before the first
	% query is read.
	detach_server_loop,
	server_loop(InputPipe, OutputPipe, TimeOut, Debug, CanonicalClique,
		0, Deep).

:- pragma foreign_decl("C", "
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
").

:- pred detach_server_loop(io__state::di, io__state::uo) is cc_multi.

:- pragma foreign_proc("C", detach_server_loop(S0::di, S::uo),
	[will_not_call_mercury, promise_pure], "
{
	int	status;

	S = S0;
	fflush(stdout);
	fflush(stderr);
	status = fork();
	if (status < 0) {
		/*
		** The fork failed; we cannot detach the server loop from the
		** startup process. The cgi script would therefore wait forever
		** if we did not exit now.
		*/

		exit(1);
	} else if (status > 0) {
		/*
		** The fork succeeded; we are in the parent. We therefore exit
		** now to let the io__call_system in the cgi script succeed.
		*/

		extern	bool	MP_process_is_detached_server;

		MP_process_is_detached_server = MR_TRUE;
		exit(0);
	}

	/*
	** Else the fork succeeded; we are in the child. We continue
	** executing, and start serving answers to queries.
	*/
}").

:- pred server_loop(string::in, string::in, int::in, bool::in,
	bool::in, int::in, deep::in, io__state::di, io__state::uo) is cc_multi.

server_loop(InputPipe, OutputPipe, TimeOut, Debug, CanonicalClique,
		QueryNum, Deep0) -->
	setup_timeout(TimeOut),
	io__see(InputPipe, SeeRes),
	(
		{ SeeRes = ok },
		io__read(ReadRes),
		stderr_stream(StdErr),
		(
			{ Debug = yes },
			io__write(StdErr, ReadRes),
			io__nl(StdErr)
		;
			{ Debug = no }
		),
		(
			{ ReadRes = eof },
			(
				{ Debug = yes },
				write_string(StdErr, "eof.\n")
			;
				{ Debug = no }
			),
			server_loop(InputPipe, OutputPipe, TimeOut, Debug,
				CanonicalClique, QueryNum + 1, Deep0)
		;
			{ ReadRes = error(Msg, Line) },
			(
				{ Debug = yes },
				io__format(StdErr,
					"error reading input line %d: %s\n",
					[i(Line), s(Msg)])
			;
				{ Debug = no }
			),
			server_loop(InputPipe, OutputPipe, TimeOut, Debug,
				CanonicalClique, QueryNum + 1, Deep0)
		;
			{ ReadRes = ok(CmdPref0) },
			{ CmdPref0 = cmd_pref(Cmd0, Pref0) },
			( { Cmd0 = restart } ->
				read_and_startup(Deep0 ^ server_name,
					[Deep0 ^ data_file_name],
					CanonicalClique, MaybeDeep),
				(
					{ MaybeDeep = ok(Deep) },
					{ Cmd = menu }
				;
					{ MaybeDeep = error(Msg) },
					io__tell(OutputPipe, _),
					io__write(html(Msg)),
					io__write_string(".\n"),
					io__told,
					{ Deep = Deep0 },
					{ Cmd = quit }
				)
			;
				{ Deep = Deep0 },
				{ Cmd = Cmd0 }
			),
			try_exec(Cmd, Pref0, Deep, HTML),
			(
				{ Debug = yes },
				io__format(StdErr, "query %d output:\n%s\n",
					[i(QueryNum), s(HTML)])
			;
				{ Debug = no }
			),

			% If we can't open the output pipe, then we have
			% no way to report our failure anyway.
			io__tell(OutputPipe, _),
			io__write(html(HTML)),
			io__write_string(".\n"),
			io__told,
			( { Cmd = quit } ->
				% The lack of a recursive call here shuts down
				% the server process.
				[]
			; { Cmd = timeout(NewTimeOut) } ->
				server_loop(InputPipe, OutputPipe, NewTimeOut,
					Debug, CanonicalClique,
					QueryNum + 1, Deep)
			;
				server_loop(InputPipe, OutputPipe, TimeOut,
					Debug, CanonicalClique,
					QueryNum + 1, Deep)
			)
		)
	;
		{ SeeRes = error(Error) },
		{ io__error_message(Error, Msg) },
		io__write_string(Msg),
		io__set_exit_status(1)
	).

%-----------------------------------------------------------------------------%

:- pred try_exec(cmd::in, preferences::in, deep::in, string::out,
	io__state::di, io__state::uo) is cc_multi.

try_exec(Cmd, Pref, Deep, HTML, IO0, IO) :-
	try_io(exec(Cmd, Pref, Deep), Result, IO0, IO),
	(
		Result = succeeded(HTML)
	;
		Result = exception(Exception),
		( univ_to_type(Exception, MsgPrime) ->
			Msg = MsgPrime
		; univ_to_type(Exception, software_error(MsgPrime)) ->
			Msg = MsgPrime
		;
			Msg = "unknown exception"
		),
		HTML =
			string__format(
				"<H3>AN EXCEPTION HAS OCCURRED: %s</H3>\n",
				[s(Msg)])
	).

%-----------------------------------------------------------------------------%
