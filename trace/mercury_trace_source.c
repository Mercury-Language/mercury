/*
** Copyright (C) 2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_source.c
**
** This file implements routines to open and use a window to display the
** source code.  Using these requires an X server and a version of
** vim compiled with '+clientserver'.  If these are not available an
** error is returned.
**
** Main author: Mark Brown
*/

#include "mercury_imp.h"
#include "mercury_trace_source.h"
#include "mercury_trace_internal.h"

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_UNISTD_H
  #include <unistd.h>
#endif

#ifdef HAVE_SYS_TYPES_H
  #include <sys/types.h>	/* for getpid() */
#endif

#define MR_DEFAULT_SOURCE_WINDOW_COMMAND	"xterm -e"
#define MR_DEFAULT_SOURCE_SERVER_COMMAND	"vim"

/*
** When sent to a vim server, this string puts vim into "normal" mode.
** This has much the same effect as ESC, except that there is no bell if
** already in normal mode.  We send this before most of the other
** commands to ensure that they are interpreted correctly.
**
** See the vim help page for ctrl-\_ctrl-n (type ':he ** ctrl-\\_ctrl-n'
** in vim).
*/
#define MR_SOURCE_SERVER_RESET_STRING           "<C-\\><C-N>"

/*
** These vim commands split the screen, move to the top window, and
** move down one window, respectively.
*/
#define MR_SOURCE_SERVER_SPLIT_STRING		"<C-W>s"
#define MR_SOURCE_SERVER_TOP_STRING		"<C-W>t"
#define MR_SOURCE_SERVER_DOWN_STRING		"<C-W>j"

/*
** This vim command centres the current window on the cursor.
*/
#define MR_SOURCE_SERVER_CENTRE_STRING		"z."

/*
** This is the command we use to quit the server.  We have to close *all*
** windows, just in case we are in split screen mode.  This won't quit if
** the user has modified any file, which is just as well.
*/
#define MR_SOURCE_SERVER_QUIT_STRING		":qall<CR>"

/*
** The name used for the server will start with this basename.
*/
#define MR_SOURCE_SERVER_BASENAME		"mdb_source_server"

/*
** This defines the number of chars of the hostname to use to
** determine a unique server name.
*/
#define MR_SERVER_HOSTNAME_CHARS		32

/*
** The size of the buffer used to construct system calls in.
*/
#define MR_SYSCALL_BUFFER_SIZE			512

/*
** Checks whether $DISPLAY is set to something reasonable.  If so,
** returns NULL, otherwise returns a warning message.
*/
static const char *MR_trace_source_check_display(void);

/*
** Checks whether the server command is valid.  If so, returns NULL,
** otherwise returns an error message.
*/
static const char *MR_trace_source_check_server_cmd(const char *server_cmd,
		bool verbose);

/*
** Checks whether a server with the given name is accessible.  If so,
** returns NULL, otherwise returns an error message.
*/
static const char *MR_trace_source_check_server(const char *server_cmd,
		const char *server_name, bool verbose);

/*
** Tell the server to jump to the given file and line.  If the server has
** multiple windows open, use the current window.  If successful, returns
** NULL, otherwise returns an error message.
*/
static const char *MR_trace_source_jump(const char *server_cmd,
		const char *server_name, const char *filename, int lineno,
		bool verbose);

/*
** Send the given key sequence to the vim server.  Returns the status
** code of the shell command.
*/
static int MR_trace_source_send(const char *server_cmd,
		const char *server_name, const char *keys, bool verbose);

static int MR_verbose_system_call(const char *system_call, bool verbose);

static const char *
MR_trace_source_check_display(void)
{
	if (getenv("DISPLAY") == NULL) {
		return "warning: DISPLAY environment variable is not set";
	} else {
		return NULL;
	}
}

static const char *
MR_trace_source_check_server_cmd(const char *server_cmd, bool verbose)
{
	char		system_call[MR_SYSCALL_BUFFER_SIZE];
	int		status;

	/*
	** Try running the server with '--version', and see if the
	** output contains the string '+clientserver'.
	*/
	sprintf(system_call,
			"%s --version 2>&1 | fgrep -q '+clientserver' %s",
			server_cmd, (verbose ? "" : "> /dev/null 2>&1"));
	status = MR_verbose_system_call(system_call, verbose);
	if (status == 0) {
		return NULL;
	} else {
		return "error: source server command is not valid";
	}
}

static const char *
MR_trace_source_check_server(const char *server_cmd, const char *server_name,
		bool verbose)
{
	char		system_call[MR_SYSCALL_BUFFER_SIZE];
	int		status;

	/*
	** Try running a server with '--serverlist', and see if the
	** output contains our server name.  Server names are case
	** insensitive.
	*/
	sprintf(system_call, "%s --serverlist | fgrep -iq '%s' %s",
			server_cmd, server_name,
			(verbose ? "" : "> /dev/null 2>&1"));
	status = MR_verbose_system_call(system_call, verbose);
	if (status == 0) {
		return NULL;
	} else {
		return "error: source server not found";
	}
}

const char *
MR_trace_source_open_server(MR_Trace_Source_Server *server,
		const char *window_cmd, int timeout, bool verbose)
{
	const char 	*real_window_cmd;
	const char	*real_server_cmd;
	char		*name;
	const char	*msg;
	char		system_call[MR_SYSCALL_BUFFER_SIZE];
	int		status;
	int		base_len;
	int		i;

	if (window_cmd != NULL) {
		real_window_cmd = window_cmd;
	} else {
		real_window_cmd = MR_DEFAULT_SOURCE_WINDOW_COMMAND;
	}

	if (server->server_cmd != NULL) {
		real_server_cmd = server->server_cmd;
	} else {
		real_server_cmd = MR_DEFAULT_SOURCE_SERVER_COMMAND;
	}

	/*
	** 1) check that display is set;
	** 2) check that server is valid;
	** 3) start a server with a unique name;
	** 4) wait until the server is found;
	** 5) (if required) split the window.
	*/

	msg = MR_trace_source_check_display();
	if (msg != NULL) {
		return msg;
	}

	msg = MR_trace_source_check_server_cmd(real_server_cmd, verbose);
	if (msg != NULL) {
		return msg;
	}

	/*
	** If possible, we generate a unique name of the form
	** '<basename>.<hostname>.<pid>', but if the required functions
	** aren't available, we fall back to appending numbers to the
	** basename in sequence until one is found that is not being used.
	** This is quite a slow way of doing things, and there is also a
	** race condition, but it is difficult to see a better way.  We
	** should let the server pick a unique name for itself, but
	** how would you communicate this name back to this process?
	*/
	base_len = strlen(MR_SOURCE_SERVER_BASENAME);

#if defined(HAVE_GETPID) && defined(HAVE_GETHOSTNAME)
	/*
	** Need to leave enough room for the pid, two '.'s and the
	** terminating zero.
	*/
	name = MR_malloc(base_len + MR_SERVER_HOSTNAME_CHARS + 32);
	strcpy(name, MR_SOURCE_SERVER_BASENAME);

	/*
	** Append the '.' and hostname.
	*/
	name[base_len] = '.';
	gethostname(name + base_len + 1, MR_SERVER_HOSTNAME_CHARS);
	/* Do this just in case gethostname didn't terminate the string: */
	name[base_len + 1 + MR_SERVER_HOSTNAME_CHARS] = '\0';

	/*
	** Find the end of the string and append the '.' and pid.
	*/
	i = base_len + 1 + strlen(name + base_len + 1);
	sprintf(name + i, ".%ld", (long) getpid());
#else
	i = 0;
	/*
	** Need to leave enough room for a '.', the integer and the
	** terminating zero.
	*/
	name = MR_malloc(base_len + 10);
	do {
		i++;
		sprintf(name, "%s.%d", MR_SOURCE_SERVER_BASENAME, i);
		/*
		** This should fail if there is no server with this
		** name.
		*/
		msg = MR_trace_source_check_server(real_server_cmd, name,
				verbose);
	} while (msg == NULL);
#endif

	server->server_name = name;

	/*
	** Start the server in read-only mode, to discourage the user
	** from trying to edit the source.
	*/
	sprintf(system_call, "%s %s -R --servername \"%s\" %s &",
			real_window_cmd, real_server_cmd, name,
			(verbose ? "" : "> /dev/null 2>&1"));
	MR_verbose_system_call(system_call, verbose);

	/*
	** We need to wait until the source server has registered itself
	** with the X server.  If we don't, we may execute remote
	** commands before the server has started up, and this will
	** result in vim (rather inconveniently) starting its own server
	** on mdb's terminal.
	*/

	msg = MR_trace_source_attach(server, timeout, verbose);

	if (msg != NULL) {
		/*
		** Something went wrong, so we should free the server
		** name we allocated just above.
		*/
		MR_free(server->server_name);
		server->server_name = NULL;
		return msg;
	}

	if (server->split) {
		/* Split the window. */
		status = MR_trace_source_send(real_server_cmd,
				server->server_name,
				MR_SOURCE_SERVER_RESET_STRING
				MR_SOURCE_SERVER_SPLIT_STRING,
				verbose);
		if (status != 0) {
			server->split = FALSE;
			return "warning: unable to split source window";
		}
	}

	return NULL;
}

const char *
MR_trace_source_attach(MR_Trace_Source_Server *server, int timeout,
		bool verbose)
{
	const char	*real_server_cmd;
	const char	*msg;
	int		i;

	if (server->server_cmd != NULL) {
		real_server_cmd = server->server_cmd;
	} else {
		real_server_cmd = MR_DEFAULT_SOURCE_SERVER_COMMAND;
	}

	msg = MR_trace_source_check_server(real_server_cmd,
			server->server_name, verbose);
	if (msg == NULL) {
		return NULL;
	}

	for (i = 0; i < timeout; i++) {
		/*
		** XXX This is an inaccurate way of keeping time.
		*/
		sleep(1);

		msg = MR_trace_source_check_server(real_server_cmd,
				server->server_name, verbose);
		if (msg == NULL) {
			return NULL;
		}
	}

	return "timeout: source server not found";
}

const char *
MR_trace_source_sync(MR_Trace_Source_Server *server, const char *filename,
		int lineno, const char *parent_filename, int parent_lineno,
		bool verbose)
{
	const char	*real_server_cmd;
	const char	*msg;
	int		status;
	bool		have_parent;
	bool		have_current;

	have_parent = strdiff(parent_filename, "") && parent_lineno != 0;
	have_current = strdiff(filename, "") && lineno != 0;

	if (!have_parent && !have_current) {
		/* No point continuing. */
		return NULL;
	}

	if (server->server_cmd != NULL) {
		real_server_cmd = server->server_cmd;
	} else {
		real_server_cmd = MR_DEFAULT_SOURCE_SERVER_COMMAND;
	}

	msg = MR_trace_source_check_server(real_server_cmd,
			server->server_name, verbose);
	if (msg != NULL) {
		return msg;
	}

	if (server->split) {
		/*
		** When in split mode, we open two windows on the vim
		** server, a primary window and the secondary window.  The
		** primary window displays what would normally be shown in
		** non-split mode.
		**
		** If there is no parent context (e.g. at internal events)
		** we leave the secondary window alone.
		**
		** If we have a parent context it will be displayed in the
		** primary window, so in this case we show the current
		** context in the secondary window.
		**
		** The primary window is the one second from the top (which
		** will usually be the bottom window).  The secondary window
		** is at the top.
		*/

		if (have_parent && have_current) {
			/* Move to the secondary (top) window. */
			status = MR_trace_source_send(real_server_cmd,
					server->server_name,
					MR_SOURCE_SERVER_RESET_STRING
					MR_SOURCE_SERVER_TOP_STRING,
					verbose);
			if (status != 0) {
				return "warning: source synchronisation failed";
			}

			msg = MR_trace_source_jump(real_server_cmd,
					server->server_name, filename, lineno,
					verbose);
			if (msg != NULL) {
				return msg;
			}

			/* Move down one window to the primary. */
			status = MR_trace_source_send(real_server_cmd,
					server->server_name,
					MR_SOURCE_SERVER_RESET_STRING
					MR_SOURCE_SERVER_DOWN_STRING,
					verbose);
			if (status != 0) {
				return "warning: source synchronisation failed";
			}
		} else {
			/*
			** Move to the primary (second from top) window.
			*/
			status = MR_trace_source_send(real_server_cmd,
					server->server_name,
					MR_SOURCE_SERVER_RESET_STRING
					MR_SOURCE_SERVER_TOP_STRING
					MR_SOURCE_SERVER_DOWN_STRING,
					verbose);
			if (status != 0) {
				return "warning: source synchronisation failed";
			}
		}
	}

	/*
	** We show the parent context if we can, since if both are present
	** the parent context is usually more interesting.  Otherwise we
	** show the current context.
	*/
	if (have_parent) {
		msg = MR_trace_source_jump(real_server_cmd,
				server->server_name, parent_filename,
				parent_lineno, verbose);
		if (msg != NULL) {
			return msg;
		}
	} else {
		msg = MR_trace_source_jump(real_server_cmd,
				server->server_name, filename, lineno,
				verbose);
		if (msg != NULL) {
			return msg;
		}
	}

	return NULL;
}

static const char *
MR_trace_source_jump(const char *server_cmd, const char *server_name,
		const char *filename, int lineno, bool verbose)
{
	char		system_call[MR_SYSCALL_BUFFER_SIZE];
	int		status;

	/*
	** Point the source server to the given context.
	*/
	sprintf(system_call, "%s --servername \"%s\" --remote '+%d' %s",
			server_cmd, server_name, lineno, filename);
	status = MR_verbose_system_call(system_call, verbose);
	if (status != 0) {
		return "warning: source synchronisation failed";
	}

	/*
	** Center the current line in the vim window.  We need to put
	** the server in normal mode, just in case the user has changed
	** mode since the previous command was sent.
	*/
	status = MR_trace_source_send(server_cmd, server_name,
			MR_SOURCE_SERVER_RESET_STRING
			MR_SOURCE_SERVER_CENTRE_STRING, verbose);
	if (status != 0) {
		return "warning: source synchronisation failed";
	}

	return NULL;
}

const char *
MR_trace_source_close(MR_Trace_Source_Server *server, bool verbose)
{
	const char	*real_server_cmd;
	const char	*msg;

	if (server->server_cmd != NULL) {
		real_server_cmd = server->server_cmd;
	} else {
		real_server_cmd = MR_DEFAULT_SOURCE_SERVER_COMMAND;
	}

	msg = MR_trace_source_check_server(real_server_cmd,
			server->server_name, verbose);
	if (msg != NULL) {
		return msg;
	}

	MR_trace_source_send(real_server_cmd, server->server_name,
			MR_SOURCE_SERVER_RESET_STRING
			MR_SOURCE_SERVER_QUIT_STRING, verbose);

#if 0
	/*
	** XXX This doesn't work properly because the server takes some
	** time to close.  Sometimes the server is still open when we do
	** the test, and this leads to a false warning.
	*/
	/*
	** We expect the following to fail.  If it doesn't, the server
	** hasn't closed properly.
	*/
	msg = MR_trace_source_check_server(real_server_cmd,
			server->server_name, verbose);
	if (msg == NULL) {
		return "warning: failed to close source server";
	} else {
		return NULL;
	}
#else
	return NULL;
#endif
}

int MR_trace_source_send(const char *server_cmd, const char *server_name,
		const char *keys, bool verbose)
{
	char		system_call[MR_SYSCALL_BUFFER_SIZE];

	sprintf(system_call, "%s --servername \"%s\" --remote-send '%s'",
			server_cmd, server_name, keys);
	return MR_verbose_system_call(system_call, verbose);
}

int MR_verbose_system_call(const char *system_call, bool verbose)
{
	if (verbose) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "+ %s\n", system_call);
	}
	return system(system_call);
}

