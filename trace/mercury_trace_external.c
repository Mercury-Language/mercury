/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the code that interfaces between the Mercury program
** being debugged and the external trace analysis process.
**
** For the general basis of trace analysis systems, see the paper
** "Opium: An extendable trace analyser for Prolog" by Mireille Ducasse,
** available from http://www.irisa.fr/lande/ducasse.
**
** The code for using an external debugger is conditionalized
** on MR_USE_EXTERNAL_DEBUGGER (which by default is not enabled)
** because it uses sockets, which are not portable.
** Ideally we ought to use autoconf for that...
**
** Main authors: Erwan Jahier and Fergus Henderson.
*/

#include "mercury_imp.h"

#ifdef MR_USE_EXTERNAL_DEBUGGER

#include "mercury_trace.h"
#include "mercury_trace_external.h"
#include "mercury_layout_util.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>

/*
** This type must match the definition of classify_request in
** library/debugger_interface.m.
*/

typedef enum {
	MR_REQUEST_HELLO_REPLY   = 0, /* initiate debugging session	      */
	MR_REQUEST_FORWARD_MOVE  = 1, /* go to the next matching trace event  */
	MR_REQUEST_CURRENT_VARS  = 2, /* report data for current_vars query   */
	MR_REQUEST_CURRENT_SLOTS = 3, /* report data for current_slots query  */
	MR_REQUEST_NO_TRACE      = 4, /* continue to end, not tracing	      */
	MR_REQUEST_ABORT_PROG    = 5, /* abort the current execution	      */
	MR_REQUEST_ERROR         = 6, /* something went wrong                 */
	MR_REQUEST_CURRENT_LIVE_VAR_NAMES  
				 = 7, /* report data for 
					 current_live_var_names query */
	MR_REQUEST_CURRENT_NTH_VAR 
				 = 8  /* report data for 
					 current_nth_var query */

} MR_debugger_request_type;

static MercuryFile MR_debugger_socket_in;
static MercuryFile MR_debugger_socket_out;

static void	MR_send_message_to_socket(const char *message);
static void	MR_read_request_from_socket(
			Word *debugger_request_ptr, 
			Integer *debugger_request_type_ptr);
	
static bool	MR_found_match(const MR_Stack_Layout_Label *layout,
			MR_Trace_Port port, Unsigned seqno, Unsigned depth,
			/* XXX registers */
			const char *path, Word search_data);
static void	MR_output_current_slots(const MR_Stack_Layout_Label *layout,
			MR_Trace_Port port, Unsigned seqno, Unsigned depth, 
			const char *path);
static void	MR_output_current_vars(Word var_list, Word string_list);
static void	MR_output_current_nth_var(Word var);
static void	MR_output_current_live_var_names(Word var_names_list, 
						 Word type_list);
static Word	MR_trace_make_var_names_list(
			const MR_Stack_Layout_Label *layout);
static Word	MR_trace_make_type_list(const MR_Stack_Layout_Label *layout,
			Word *saved_regs);
static Word	MR_trace_make_nth_var(const MR_Stack_Layout_Label *layout, 
			Word *saved_regs, Word debugger_request);
static int	MR_get_var_number(Word debugger_request);

#if 0
This pseudocode should go in the debugger process:

#define SOCKET_PATH "/var/tmp/" 	/* +5 for pid = 14 chars */
#define SOCKET_PERM S_IRWXU		/* rwx for user only */

sprintf(Name, "%s%05d", SOCKET_PATH, getpid());

socket(unix, stream, Sock),
bind(sock, Name, Socket_file),
if (do_it_manually) {
	printf( "user: you must do\n"
	        "	setenv MERCURY_INET_DEBUGGER_SOCKET Name\n"
		"and then run the program\n");
	... just wait for the user do it ...
} else {
	fork()
	if (child) {
		setenv(MERCURY_UNIX_DEBUGGER_SOCKET, Name);
		exec(user program)
	}
}
listen(Sock, 1),
accept(Sock, _, New_Sock).

#endif

#if 0
	
static void
MR_init_unix_address(const char *name, struct sockaddr_un *unix_addr)
{
	/*
 	** The code here is adapted from Stevens, "Advanced Programming
	** in the UNIX environment", page 501.
	** Don't blame me, I'm just copying this code from Stevens ;-)
	*/

	memset(unix_addr, 0, sizeof(unix_addr));
	unix_addr->sun_family = AF_UNIX;
	strcpy(unix_addr->sun_path, name);
	#ifdef SCM_RIGHTS
		len = sizeof(unix_addr->sun_len) +
			sizeof(unix_addr->sun_family) +
			strlen(unix_addr->sun_path) + 1;
		unix_addr->sun_len = len;
	#else
		len = strlen(unix_addr->sun_path) +
			sizeof(unix_addr->sun_family);
		if (len != 16) {
			fatal_error("unix socket: length != 16");
		}
	#endif
}
#endif

static bool MR_debug_socket = FALSE;

void
MR_trace_init_external(void)
{
	int fd;
	int len;
	FILE *file_in;
	FILE *file_out;
	int addr_family;
	char *unix_socket;
	char *inet_socket;
	struct sockaddr_un unix_address;
	struct sockaddr_in inet_address;
	struct sockaddr* addr;
	Word debugger_request;
	Integer debugger_request_type;

	/*
	** We presume that the user's program has been invoked from
	** within the debugger (e.g. Opium).
	** The debugger (or the user) should set the
	** MERCURY_DEBUGGER_UNIX_SOCKET or MERCURY_DEBUGGER_INET_SOCKET
	** environment variable to tell the user's program which socket
	** it needs to connect to.
	*/
	unix_socket = getenv("MERCURY_DEBUGGER_UNIX_SOCKET");
	inet_socket = getenv("MERCURY_DEBUGGER_INET_SOCKET");
	if (unix_socket == NULL && inet_socket == NULL) {
		fatal_error("you must set either the "
			"MERCURY_DEBUGGER_UNIX_SOCKET\n"
			"or MERCURY_DEBUGGER_INET_SOCKET "
			"environment variable");
	}
	if (unix_socket != NULL && inet_socket != NULL) {
		fatal_error("you must set only one of the "
			"MERCURY_DEBUGGER_UNIX_SOCKET "
			"and MERCURY_DEBUGGER_INET_SOCKET\n"
			"environment variables");
	}
	if (unix_socket) {
	
		addr_family = AF_UNIX;
		memset(&unix_address, 0, sizeof(unix_address));
		unix_address.sun_family = AF_UNIX;
		strcpy(unix_address.sun_path, unix_socket);
		addr = (struct sockaddr *) &unix_address;
		len = SUN_LEN(&unix_address);
	} else {
		char hostname[255];
		char port_string[255];
		unsigned short port;
		int host_addr;

		/*
		** Parse the MERCURY_DEBUGGER_INET_SOCKET environment variable.
		** It should be in the format "<hostname> <port>",
		** where <hostname> is numeric (e.g. "123.456.78.90").
		*/
		if (sscanf(inet_socket, "%254s %254s", hostname, port_string)
			!= 2)
		{
			fatal_error("MERCURY_DEBUGGER_INET_SOCKET invalid");
		}
		host_addr = inet_network(hostname);
		if (host_addr == -1) {
			fatal_error("MERCURY_DEBUGGER_INET_SOCKET: "
				"invalid address");
		}
		if (sscanf(port_string, "%hu", &port) != 1) {
			fatal_error("MERCURY_DEBUGGER_INET_SOCKET: "
				"invalid port");
		}

		fprintf(stderr, "Mercury runtime: host = %s, port = %d\n",
				hostname, port);
	
		inet_address.sin_family = AF_INET;
		inet_address.sin_addr.s_addr = host_addr;
		inet_address.sin_port = htons(port);
		addr_family = AF_INET;
		addr = (struct sockaddr *) &inet_address;
		len = sizeof(inet_address);
	}

	/*
	** Open the socket.
	*/

	fd = socket(addr_family, SOCK_STREAM, 0);
	if (fd < 0) {
		fprintf(stderr, "Mercury runtime: socket() failed: %s\n",
			strerror(errno));
		fatal_error("cannot open socket for debugger");
	} else if (MR_debug_socket) {
		fprintf(stderr,"Mercury runtime: creation of socket ok\n");
	}

	/*
	** Connect to the socket
	*/
	if (connect(fd, addr, len) < 0) {
		fprintf(stderr, "Mercury runtime: connect() failed: %s\n",
			strerror(errno));
		fatal_error("can't connect to debugger socket");
	} else if (MR_debug_socket) {
		fprintf(stderr, "Mercury runtime: connection to socket: ok\n");
	}

	/*
	** Convert the socket fd to a Mercury stream
	*/
	file_in = fdopen(fd, "r");
	file_out = fdopen(fd, "w");
	if ((file_in == NULL)||(file_out == NULL)) {
		fprintf(stderr, "Mercury runtime: fdopen() failed: %s\n",
			strerror(errno));
		fatal_error("cannot open debugger socket");
	} else if (MR_debug_socket) {
		fprintf(stderr, "Mercury runtime: fdopen(): ok\n");
	}

	MR_debugger_socket_in.file = file_in;
	MR_debugger_socket_in.line_number = 1;

	MR_debugger_socket_out.file = file_out;
	MR_debugger_socket_out.line_number = 1;

	/*
	** Send hello
	*/

	MR_send_message_to_socket("hello");
	if (MR_debug_socket) {
		fprintf(stderr, "Mercury runtime: Send hello\n");
	}

	/*
	** Wait for hello_reply  
	*/

	MR_read_request_from_socket(&debugger_request, &debugger_request_type);

	if (debugger_request_type != MR_REQUEST_HELLO_REPLY) {
		fatal_error("unexpected command on debugger socket");
	} else if (MR_debug_socket) {
		fprintf(stderr, "Mercury runtime: read hello_reply\n");
	}

	/*
	** Send start to start the synchronous communication with the debugger
	*/

	MR_send_message_to_socket("start");
	if (MR_debug_socket) {
		fprintf(stderr, "Mercury runtime: start send\n");
	}
}

void
MR_trace_final_external(void)
{
	/*
	** This can only happen during a forward_move(),
	** in which case we want to tell the debugger that
	** no match was found.
	*/
	MR_send_message_to_socket("forward_move_match_not_found");

	/*
	** Maybe we should loop to process requests from the
	** debugger socket here?  Currently we just return,
	** which will result in the debuggee terminating.
	** (This will need to change if the debuggee is to record
	** trace history and support a `move_backward' request.)
	*/
}

void
MR_trace_event_external(MR_Trace_Cmd_Info *cmd, 
	const MR_Stack_Layout_Label *layout, Word *saved_regs,
	MR_Trace_Port port, Unsigned seqno, Unsigned depth, const char *path)
{
	static bool searching = FALSE;
	static Word search_data;
	Word debugger_request;
	Integer debugger_request_type, live_var_number;
	Word var_list, var_names_list, type_list, var;

	if (searching) {
		/* XXX should also pass registers here,
		   since they're needed for checking for matches with the
		   arguments */
		if (MR_found_match(layout, port, seqno, depth,
			/* XXX registers */ path, search_data))
		{
			MR_send_message_to_socket("forward_move_match_found");
			searching = FALSE;
		} else {
			return;
		}
	}

	/* loop to process requests read from the debugger socket */
	for(;;) {
		MR_read_request_from_socket(
			&debugger_request, &debugger_request_type);
		switch((int) debugger_request_type) {
			case MR_REQUEST_ABORT_PROG:
				fatal_error("aborting the execution on "
					"user request");

			case MR_REQUEST_FORWARD_MOVE:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"FORWARD_MOVE\n");
				}
				search_data = debugger_request;
			        searching = TRUE;
				return;

			case MR_REQUEST_CURRENT_LIVE_VAR_NAMES:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"MR_REQUEST_CURRENT_LIVE_VAR"
						"_NAMES\n");
				}
				var_names_list = 
				  MR_trace_make_var_names_list(layout);
				type_list = MR_trace_make_type_list(layout,
						saved_regs);
				MR_output_current_live_var_names(var_names_list,
					type_list);
				break;

			case MR_REQUEST_CURRENT_VARS:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_CURRENT_VARS\n");
				}
				var_list = MR_make_var_list(layout, saved_regs);
				var_names_list = 
				  MR_trace_make_var_names_list(layout);
				MR_output_current_vars(var_list, 
						       var_names_list);
				break;

			case MR_REQUEST_CURRENT_NTH_VAR:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_NTH_CURRENT_VAR\n");
				}
				var = MR_trace_make_nth_var(layout, saved_regs,
							    debugger_request);
				MR_output_current_nth_var(var);
				break;			
			case MR_REQUEST_CURRENT_SLOTS:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_CURRENT_SLOTS\n");
				}
				MR_output_current_slots(layout, port, seqno, 
							depth, path);
				break;

			case MR_REQUEST_NO_TRACE:
				cmd->MR_trace_cmd = MR_CMD_TO_END;
				return;

			default:
				fatal_error("unexpected request read from "
					"debugger socket");
		}
	}
}



static void
MR_output_current_slots(const MR_Stack_Layout_Label *layout,
	MR_Trace_Port port, Unsigned seqno, Unsigned depth, const char *path)
{
	/*
	** XXX This function and the Mercury predicates it calls
	** ought to be generalized to handle inter-module inlining,
	** and either further generalized to handle compiler-generated
	** procedures or to explicitly discard events involving
	** compiler-generated procedures.
	*/

	MR_DI_output_current_slots(
		MR_trace_event_number,
		seqno,
		depth,
		port,
		layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
		layout->MR_sll_entry->MR_sle_user.MR_user_name,
		layout->MR_sll_entry->MR_sle_user.MR_user_arity,
		layout->MR_sll_entry->MR_sle_user.MR_user_mode,
		layout->MR_sll_entry->MR_sle_detism,
		(String) (Word) path,
		(Word) &MR_debugger_socket_out);
}

static void
MR_output_current_vars(Word var_list, Word string_list)
{
	MR_DI_output_current_vars(
		var_list,
		string_list,
		(Word) &MR_debugger_socket_out);
}

static void
MR_output_current_nth_var(Word var)
{
	MR_DI_output_current_nth_var(
		var,
		(Word) &MR_debugger_socket_out);
}

static void
MR_output_current_live_var_names(Word var_names_list, Word type_list)
{
	MR_DI_output_current_live_var_names(
		var_names_list,
		type_list,
		(Word) &MR_debugger_socket_out);
}

static void
MR_read_request_from_socket(
			Word *debugger_request_ptr, 
			Integer *debugger_request_type_ptr)
{		
	fflush(MR_debugger_socket_in.file);
	MR_DI_read_request_from_socket(
		(Word) &MR_debugger_socket_in, 
		debugger_request_ptr, 
		debugger_request_type_ptr);
}
 

static bool
MR_found_match(const MR_Stack_Layout_Label *layout,
	MR_Trace_Port port, Unsigned seqno, Unsigned depth,
	/* XXX live vars */
	const char *path, Word search_data)
{
	bool result;

	/*
	** XXX This function and the Mercury predicates it calls
	** ought to be generalized to handle inter-module inlining,
	** and either further generalized to handle compiler-generated
	** procedures or to explicitly discard events involving
	** compiler-generated procedures.
	*/

	/* XXX get live vars from registers */
	Word arguments = /* XXX FIXME!!! */ 0;
	result = MR_DI_found_match(
		MR_trace_event_number,
		seqno,
		depth,
		port,
		layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
		layout->MR_sll_entry->MR_sle_user.MR_user_name,
		layout->MR_sll_entry->MR_sle_user.MR_user_arity,
		layout->MR_sll_entry->MR_sle_user.MR_user_mode,
		layout->MR_sll_entry->MR_sle_detism,
		arguments,
		(String) (Word) path,
		search_data);
	return result;
}

static void
MR_send_message_to_socket(const char *message)
{
	fprintf(MR_debugger_socket_out.file, "%s.\n", message);
	fflush(MR_debugger_socket_out.file);
	MR_debugger_socket_out.line_number++;
}


/*
** This function returns the list of the internal names of currently live
** variables.
*/

static Word
MR_trace_make_var_names_list(const MR_Stack_Layout_Label *layout)
{
	int 				var_count;
	const MR_Stack_Layout_Vars 	*vars;
	int				i;
	const char			*name;

	Word				var_names_list;

	var_count = layout->MR_sll_var_count;
	vars = &layout->MR_sll_var_info;

	restore_transient_registers();
	var_names_list = list_empty();
	save_transient_registers();
	for (i = var_count - 1; i >= 0; i--) {

		name = MR_name_if_present(vars, i);
		restore_transient_registers();
		var_names_list = list_cons(name, var_names_list);
		save_transient_registers();
	}

	return var_names_list;
}


/*
** This function returns the list of types of currently live variables.
*/

static Word
MR_trace_make_type_list(const MR_Stack_Layout_Label *layout, Word *saved_regs)
{
	int 				var_count;
	const MR_Stack_Layout_Vars 	*vars;
	int				i;
	const char			*name;
	MR_Stack_Layout_Var*		var;
	Word				type_info;
	String		      		type_info_string;

	Word				type_list;

	var_count = layout->MR_sll_var_count;
	vars = &layout->MR_sll_var_info;

	restore_transient_registers();
	type_list = list_empty();
	save_transient_registers();
	for (i = var_count - 1; i >= 0; i--) {

		name = MR_name_if_present(vars, i);
		var = &vars->MR_slvs_pairs[i];

		if (! MR_get_type_filtered(var, saved_regs, name, &type_info))
		{
			continue;
		}

		restore_transient_registers();
		type_info_string = MR_type_name(type_info);
		type_list = list_cons(type_info_string, type_list);
		save_transient_registers();
	}

	return type_list;
}


/*
** This function returns the requested live variable.
*/

static Word
MR_trace_make_nth_var(const MR_Stack_Layout_Label *layout, Word *saved_regs,
		      Word debugger_request)
{
	int 				var_number;
	const MR_Stack_Layout_Vars 	*vars;
	const char			*name;
	MR_Stack_Layout_Var*		var;
	int				i;
	Word				value;
	Word				type_info;

	Word				univ;

	var_number = MR_get_var_number(debugger_request);
		/* debugger_request should be of the form: 
		   current_nth_var(var_number) */
	vars = &layout->MR_sll_var_info;
	name = MR_name_if_present(vars, var_number);
	var = &vars->MR_slvs_pairs[var_number];

	restore_transient_registers();
	incr_hp(univ, 2);


	if (MR_get_type_and_value_filtered(var, saved_regs, name,
			&type_info, &value))
	{
		field(mktag(0), univ, UNIV_OFFSET_FOR_TYPEINFO) = type_info;
		field(mktag(0), univ, UNIV_OFFSET_FOR_DATA) = value;
	} else {
		/*
		** Should never occur since we check in the external debugger
		** process if a variable is live before retrieving it.
		*/
		fatal_error("try to retrieve a non-live variable");
	}

	save_transient_registers();

	return univ;
}


/*
** This function is called only when debugger_request = current_nth_var(n).
** It returns the integer 'n'.  
*/

static int
MR_get_var_number(Word debugger_request)
{
	return MR_DI_get_var_number(debugger_request);
}
#endif /* MR_USE_EXTERNAL_DEBUGGER */
