%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This module implements timeouts for the deep profiler.
%
% The deep profiler uses timeouts to shut down the server process if the
% programmer has not sent it queries in a while. Before shutdown, we remove the
% named pipes that the CGI script and the server process use to communicate.
% Any later invocation of the CGI script will take the absence of the named
% pipes as indicating that there is no server process for the given data file,
% and will create a new server process, which will recreate the named pipes.
%
% Since the receipt of the alarm signal, the removal the pipes and exiting
% is not an atomic action, there is a potential race condition. However,
% there is no simple, portable way to eliminate the race condition, and the
% window of vulnerability is quite small.
%
% This module also provides a predicate for executing the timeout action
% explicitly, for use when the user explicitly shuts down the server.
% This avoids double maintenance of the shutdown actions.

:- module timeout.

:- interface.

:- import_module io.

:- pred setup_timeout(string::in, string::in, string::in, int::in,
	io__state::di, io__state::uo) is det.

:- pred execute_timeout_action(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

:- pragma foreign_decl("C",
"
#include <stdio.h>
#include <signal.h>
#include ""mercury_signal.h""

extern	char	*MP_timeout_file1;
extern	char	*MP_timeout_file2;
extern	char	*MP_timeout_file3;

extern	void	delete_timeout_files_and_exit(void);
").

:- pragma foreign_code("C",
"
char	*MP_timeout_file1;
char	*MP_timeout_file2;
char	*MP_timeout_file3;

void
delete_timeout_files_and_exit(void)
{
	if (remove(MP_timeout_file1) != 0) {
		perror(MP_timeout_file1);
	}

	if (remove(MP_timeout_file2) != 0) {
		perror(MP_timeout_file2);
	}

	if (remove(MP_timeout_file3) != 0) {
		perror(MP_timeout_file3);
	}

	exit(0);
}
").

:- pragma foreign_proc("C",
	setup_timeout(File1::in, File2::in, File3::in, Minutes::in,
		IO0::di, IO::uo),
	[will_not_call_mercury],
"
	int	seconds;

	seconds = Minutes * 60;
	MP_timeout_file1 = File1;
	MP_timeout_file2 = File2;
	MP_timeout_file3 = File3;
	MR_setup_signal(SIGALRM, delete_timeout_files_and_exit, FALSE,
		""Mercury deep profiler: cannot setup timeout"");
	(void) alarm(seconds);
	IO = IO0;
").

:- pragma foreign_proc("C",
	execute_timeout_action(IO0::di, IO::uo),
	[will_not_call_mercury],
"
	delete_timeout_files_and_exit();
	IO = IO0;
").
