/*
** Copyright (C) 2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_source.h
**
** This file provides routines to open and use a window to display the
** source code.  Using these requires an X server and a version of
** vim compiled with '+clientserver'.  If these are not available an
** error is returned.
**
** Main author: Mark Brown
*/

#ifndef MERCURY_TRACE_SOURCE_H
#define MERCURY_TRACE_SOURCE_H

#include "mercury_std.h"	/* for MR_bool */

/*
** This holds the information for one server that the program is
** attached to.  The fields are:
** 
** 	server_name	Name of the source server, or NULL if there
** 			is no server.
** 	server_cmd	Command to run to start the server, or connect
** 			to it, or NULL if to use the default of "vim".
**	split		Is the server in split screen mode?  If so, we
**			show both the parent and current contexts.
*/
typedef struct {
	char		*server_name;
	char		*server_cmd;
	MR_bool		split;
} MR_Trace_Source_Server;

/*
** Start a server in a new window.  The arguments are:
**
** 	source_server	Info for the new server.  If server_name is
** 			NULL, a new name is generated.  If the operation
** 			succeeds, memory for the name will be MR_malloc'd
** 			so the caller must MR_free it when no longer
** 			needed.
** 	window_cmd	Command to open a new window (NULL means
** 			default to "xterm -e").
** 	timeout		Maximum time to wait for the server to start,
** 			in seconds.  XXX don't rely on the times being
** 			accurate.
** 	verbose		If MR_TRUE, print out system calls before executing
** 			them and show their output.  If MR_FALSE the output
** 			is redirected to /dev/null.
**
** If successful it returns NULL, otherwise it returns a string describing
** the problem.
*/
const char *MR_trace_source_open_server(MR_Trace_Source_Server *server,
		const char *window_cmd, int timeout, MR_bool verbose);

/*
** Attach to an already running server.  If successful it returns NULL,
** otherwise it returns a string describing the problem.
*/
const char *MR_trace_source_attach(MR_Trace_Source_Server *server,
		int timeout, MR_bool verbose);

/*
** Synchronise the server with the current source location.  This first
** checks if the server is running (since the user could have exited
** from the server window manually).  If there is no such server, returns
** a warning message and does nothing else; returns NULL if there were no
** problems.
*/
const char *MR_trace_source_sync(MR_Trace_Source_Server *server,
		const char *filename, int lineno, const char *parent_filename,
		int parent_lineno, MR_bool verbose);

/*
** Close a server if possible.  If the server appears to be still
** running after this, returns a warning message.  This can happen if
** the user has modified the code in the source window, for example.
*/
const char *MR_trace_source_close(MR_Trace_Source_Server *server,
		MR_bool verbose);

#endif /* not MERCURY_TRACE_SOURCE_H */

