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
% This module also sets up the automatic execution of the timeout action
% when the process exits, for use both when the user explicitly requests
% the shutdown of the server (which will of course happen after startup)
% and in case of program aborts (which may happen both before and after
% startup). However, immediately after startup is complete, the server
% process forks, with the parent exiting to let mdprof_cgi's wait finish,
% and the child entering a loop waiting for requests.
%
% We establish the exit action to clean up the files as soon as they are
% created, but we don't want the parent process after the fork to delete them
% while they are still in use by the child process. This is prevented by the
% boolean flag process_is_detached_server.

:- module timeout.

:- interface.

:- import_module io.

:- pred setup_exit(string::in, string::in, string::in,
	io__state::di, io__state::uo) is det.

:- pred setup_timeout(int::in, io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

:- pragma foreign_decl("C",
"
#include <stdio.h>
#include <signal.h>	/* for signal numbers */
#include <unistd.h>	/* for alarm() */
#include <stdlib.h>	/* for atexit() */
#include ""mercury_signal.h""

extern	char		*MP_timeout_file1;
extern	char		*MP_timeout_file2;
extern	char		*MP_timeout_file3;

extern	const int	MP_signal_numbers[];

extern	void		MP_delete_timeout_files(void);
extern	void		MP_delete_timeout_files_and_exit_success(void);
extern	void		MP_delete_timeout_files_and_exit_failure(void);
").

:- pragma foreign_code("C",
"
MR_bool	MP_process_is_detached_server = MR_FALSE;
char	*MP_timeout_file1;
char	*MP_timeout_file2;
char	*MP_timeout_file3;

/*
** SIGALRM alarm signal indicates a timeout. SIGTERM usually indicates the
** machine is being shut down. The others are there to catch forceful shutdowns
** during development, both intentional ones where the programmer sends the
** signal and those caused by bugs in the server code. We would like to include
** all catchable, fatal signals in this list, but that set is somewhat OS
** dependent. The set whose existence we test for here includes all the
** signals that are at all likely to be sent to server process.
**
** We don't test for the existence of SIGALRM, because we want compilation to
** fail if it does not exist. Without alarm signals, server processes will
** never be timed out, and thus constitute a resource leak (mostly of virtual
** memory/swap space).
**
** We could avoid this problem if we had a version of atexit that executed
** its actions even when the program exits after a signal.
*/

const int	MP_signal_numbers[] =
{
	SIGALRM,
#ifdef SIGTERM
	SIGTERM,
#endif
#ifdef SIGHUP
	SIGHUP,
#endif
#ifdef SIGINT
	SIGINT,
#endif
#ifdef SIGQUIT
	SIGQUIT,
#endif
#ifdef SIGILL
	SIGILL,
#endif
#ifdef SIGABRT
	SIGABRT,
#endif
#ifdef SIGBUS
	SIGBUS,
#endif
#ifdef SIGFPE
	SIGFPE,
#endif
#ifdef SIGSEGV
	SIGSEGV,
#endif
#ifdef SIGPIPE
	SIGPIPE,
#endif
	-1
};

void
MP_delete_timeout_files(void)
{
	if (! MP_process_is_detached_server) {
		if (remove(MP_timeout_file1) != 0) {
			perror(MP_timeout_file1);
		}

		if (remove(MP_timeout_file2) != 0) {
			perror(MP_timeout_file2);
		}

		/*
		if (remove(MP_timeout_file3) != 0) {
			perror(MP_timeout_file3);
		}
		*/
	}
}

void
MP_delete_timeout_files_and_exit_success(void)
{
	MP_delete_timeout_files();
	exit(EXIT_SUCCESS);
}

void
MP_delete_timeout_files_and_exit_failure(void)
{
	MP_delete_timeout_files();
	exit(EXIT_FAILURE);
}
").

:- pragma foreign_proc("C",
	setup_exit(File1::in, File2::in, File3::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	int	i;
	void	(*handler)(void);

	MP_timeout_file1 = File1;
	MP_timeout_file2 = File2;
	MP_timeout_file3 = File3;

	for (i = 0; MP_signal_numbers[i] >= 0; i++) {
		if (MP_signal_numbers[i] == SIGALRM) {
			handler = MP_delete_timeout_files_and_exit_success;
		} else {
			handler = MP_delete_timeout_files_and_exit_failure;
		}

		MR_setup_signal(MP_signal_numbers[i], handler, MR_FALSE,
			""Mercury deep profiler: cannot setup signal exit"");
	}

	if (atexit(MP_delete_timeout_files) != 0) {
		MR_fatal_error(""Mercury deep profiler: cannot setup exit"");
	}

	IO = IO0;
").

:- pragma foreign_proc("C",
	setup_timeout(Minutes::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	int	seconds;

	seconds = Minutes * 60;
	(void) alarm(seconds);
	IO = IO0;
").
