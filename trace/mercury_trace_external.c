/*
** Copyright (C) 1998-1999 The University of Melbourne.
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
** on MR_USE_EXTERNAL_DEBUGGER which is enabled for systems that support
** sockets.
**
** Main authors: Erwan Jahier and Fergus Henderson.
*/

#include "mercury_imp.h"

#ifdef MR_USE_EXTERNAL_DEBUGGER

#include "mercury_trace.h"
#include "mercury_trace_external.h"
#include "mercury_trace_util.h"
#include "mercury_layout_util.h"
#include "mercury_trace_browse.h"

#include "debugger_interface.h"
#include "std_util.h"

#include <stdio.h>
#include <errno.h>
#include <stdarg.h>
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
					 current_live_var_names query	      */
	MR_REQUEST_CURRENT_NTH_VAR 
				 = 8, /* report data for 
					 current_nth_var query		      */
	MR_REQUEST_RETRY	 = 9, /* restart the execution to the call 
					 port of the current event	      */
	MR_REQUEST_STACK         = 10,/* print the ancestors list             */
	MR_REQUEST_NONDET_STACK  = 11,/* print the nondet stack		      */
	MR_REQUEST_STACK_REGS    = 12,/* prints the contents of the virtual
							   machine registers. */
	MR_REQUEST_INTERACTIVE_QUERY_NORMAL	 
				 = 13,/* wait for a normal interactive query  */
	MR_REQUEST_INTERACTIVE_QUERY_CC	 
				 = 14,/* wait for a cc interactive query      */
	MR_REQUEST_INTERACTIVE_QUERY_IO	 
				 = 15,/* wait for a io interactive query      */
	MR_REQUEST_MMC_OPTIONS	 = 16 /* pass down new options to compile
					 queries with			      */

} MR_debugger_request_type;

MercuryFile MR_debugger_socket_in;
MercuryFile MR_debugger_socket_out;

/*
** Use a GNU C extension to enforce static type checking
** for printf-style functions. 
** (See the "Function attributes" section of "C extensions"
** chapter of the GNU C manual for detailed documentation.)
*/
#ifdef __GNUC__
  #define MR_LIKE_PRINTF(format_argnum, vars_argnum) \
    __attribute__ ((format (printf, (format_argnum), (vars_argnum))))
#else
  #define MR_LIKE_PRINTF(n, m) /* nothing */
#endif
static void MR_send_message_to_socket_format(const char *format, ...)
	MR_LIKE_PRINTF(1, 2);

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
static void	MR_print_proc_id_to_socket(const MR_Stack_Layout_Entry *entry,
			const char *extra, Word *base_sp, Word *base_curfr);
static void	MR_dump_stack_record_print_to_socket(FILE *fp, 
			const MR_Stack_Layout_Entry *entry_layout, int count, 
			int start_level, Word *base_sp, Word *base_curfr);
static void	MR_get_list_modules_to_import(Word debugger_request, 
			Integer *modules_list_length_ptr, Word *modules_list_ptr);
static void	MR_get_mmc_options(Word debugger_request, 
			String *mmc_options_ptr);

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
		len = strlen(unix_address.sun_path) + 
			sizeof(unix_address.sun_family);
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

Code *
MR_trace_event_external(MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info)
{
	static bool	searching = FALSE;
	static Word	search_data;
	Integer		debugger_request_type;
	Integer		live_var_number;
	Word		debugger_request;
	Word		var_list;
	Word		var_names_list;
	Word		type_list;
	Word		var;
	Code		*jumpaddr = NULL;
	MR_Event_Details	event_details;
	const char	*message;
        bool		include_trace_data = TRUE;
	const MR_Stack_Layout_Label *layout = event_info->MR_event_sll;
	Unsigned	seqno = event_info->MR_call_seqno;
	Unsigned	depth = event_info->MR_call_depth;
	MR_Trace_Port	port = event_info->MR_trace_port;
	const char 	*path = event_info->MR_event_path;
	Word		*saved_regs = event_info->MR_saved_regs;
	Integer		modules_list_length;
	Word		modules_list;

/* 
** MR_mmc_options contains the options to pass to mmc when compiling queries.
** We initialise it to the String "".
*/
	static String	MR_mmc_options;
	MR_TRACE_CALL_MERCURY(ML_DI_init_mercury_string(&MR_mmc_options));

	event_details.MR_call_seqno = MR_trace_call_seqno;
	event_details.MR_call_depth = MR_trace_call_depth;
	event_details.MR_event_number = MR_trace_event_number;

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
			return jumpaddr;
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
				return jumpaddr;

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

			case MR_REQUEST_RETRY:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_RETRY\n");
				}
				message = MR_trace_retry(event_info, 
					&event_details, &jumpaddr);
				if (message == NULL) {
					MR_send_message_to_socket("ok");
					cmd->MR_trace_cmd = MR_CMD_GOTO;
					cmd->MR_trace_stop_event = 
						MR_trace_event_number + 1;
					return jumpaddr;
				} else {
					MR_send_message_to_socket_format(
						"error(\"%s\").\n", message);
				}
				break;
				
			case MR_REQUEST_STACK:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_STACK\n");
				}
				do_init_modules();
				message = MR_dump_stack_from_layout(
					stdout,
					layout->MR_sll_entry,
					MR_saved_sp(saved_regs),
					MR_saved_curfr(saved_regs),
					include_trace_data,
					&MR_dump_stack_record_print_to_socket);
				MR_send_message_to_socket("end_stack");
				if (message != NULL) {
					MR_send_message_to_socket_format(
						"error(\"%s\").\n", message);
				} else {
					MR_send_message_to_socket("ok");
				}
				break; 
			
			case MR_REQUEST_NONDET_STACK: 
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_NONDET_STACK\n");
				}
				do_init_modules();
				/* 
			        ** XXX As in stack dump, we could send the
				** output of this function on the socket. But
				** the outputs are done via fprintf() and
				** printlabel(), so we would need to define new
				** fprintf() and printlabel() and pass them
				** down as parameters of
				** MR_dump_nondet_stack_from_layout() (as we do
				** with MR_dump_stack_record_print()).
				*/						
				MR_dump_nondet_stack_from_layout(stdout,
					MR_saved_maxfr(saved_regs));
				MR_send_message_to_socket("ok");
				break;
			
			case MR_REQUEST_STACK_REGS:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_STACK_REGS\n");
				}
				MR_send_message_to_socket_format(
					"stack_regs(%lu, %lu, %lu).\n",
					(unsigned long)
					MR_saved_sp(saved_regs),
					(unsigned long)
					MR_saved_curfr(saved_regs),
					(unsigned long)
					MR_saved_maxfr(saved_regs));
				break;
			
			case MR_REQUEST_INTERACTIVE_QUERY_NORMAL:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_INTERACTIVE_QUERY"
						"_NORMAL\n");
				}
				MR_get_list_modules_to_import(
					debugger_request, &modules_list_length,
					&modules_list);
				MR_trace_query_external(MR_NORMAL_QUERY, 
					MR_mmc_options, modules_list_length, 
					modules_list);
				break;

			case MR_REQUEST_INTERACTIVE_QUERY_IO:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_INTERACTIVE_QUERY_IO\n");
				}
				MR_get_list_modules_to_import(
					debugger_request, &modules_list_length,
					&modules_list);
				MR_trace_query_external(MR_IO_QUERY, 
					MR_mmc_options, modules_list_length, 
					modules_list);
				break;

			case MR_REQUEST_INTERACTIVE_QUERY_CC:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_INTERACTIVE_QUERY_CC\n");
				}
				MR_get_list_modules_to_import(
					debugger_request, &modules_list_length,
					&modules_list);
				MR_trace_query_external(MR_CC_QUERY, 
					MR_mmc_options, modules_list_length, 
					modules_list);
				break;

			case MR_REQUEST_MMC_OPTIONS:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_MMC_OPTIONS\n");
				}
				MR_get_mmc_options(debugger_request, 
					&MR_mmc_options);
				MR_send_message_to_socket("mmc_options_ok");
				break;

			case MR_REQUEST_NO_TRACE:
				cmd->MR_trace_cmd = MR_CMD_TO_END;
				return jumpaddr;

			default:
				fatal_error("unexpected request read from "
					"debugger socket");
		}
	}

	cmd->MR_trace_must_check = (! cmd->MR_trace_strict) ||
			(cmd->MR_trace_print_level != MR_PRINT_LEVEL_NONE);

	MR_trace_call_seqno = event_details.MR_call_seqno;
	MR_trace_call_depth = event_details.MR_call_depth;
	MR_trace_event_number = event_details.MR_event_number;

	return jumpaddr;
}



static void
MR_output_current_slots(const MR_Stack_Layout_Label *layout,
	MR_Trace_Port port, Unsigned seqno, Unsigned depth, const char *path)
{
	if (MR_ENTRY_LAYOUT_COMPILER_GENERATED(layout->MR_sll_entry)) {
		MR_TRACE_CALL_MERCURY(
		    ML_DI_output_current_slots_comp(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_name,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_module,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_def_module,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_pred_name,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_arity,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_mode,
			layout->MR_sll_entry->MR_sle_detism,
			(String) (Word) path,
			(Word) &MR_debugger_socket_out);
		    );
	} else {
		MR_TRACE_CALL_MERCURY(
		    ML_DI_output_current_slots_user(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			layout->MR_sll_entry->MR_sle_user.MR_user_pred_or_func,
			(String)
			layout->MR_sll_entry->MR_sle_user.MR_user_decl_module,
			(String)
			layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
			(String)
			layout->MR_sll_entry->MR_sle_user.MR_user_name,
			layout->MR_sll_entry->MR_sle_user.MR_user_arity,
			layout->MR_sll_entry->MR_sle_user.MR_user_mode,
			layout->MR_sll_entry->MR_sle_detism,
			(String) (Word) path,
			(Word) &MR_debugger_socket_out);
		    );
	}
}

static void
MR_output_current_vars(Word var_list, Word string_list)
{
    MR_TRACE_CALL_MERCURY(
	ML_DI_output_current_vars(
		var_list,
		string_list,
		(Word) &MR_debugger_socket_out);
    );
}

static void
MR_output_current_nth_var(Word var)
{
    MR_TRACE_CALL_MERCURY(
	ML_DI_output_current_nth_var(
		var,
		(Word) &MR_debugger_socket_out);
    );
}

static void
MR_output_current_live_var_names(Word var_names_list, Word type_list)
{
    MR_TRACE_CALL_MERCURY(
	ML_DI_output_current_live_var_names(
		var_names_list,
		type_list,
		(Word) &MR_debugger_socket_out);
    );
}

static void
MR_read_request_from_socket(
			Word *debugger_request_ptr, 
			Integer *debugger_request_type_ptr)
{		
	fflush(MR_debugger_socket_in.file);

    MR_TRACE_CALL_MERCURY(
	ML_DI_read_request_from_socket(
		(Word) &MR_debugger_socket_in, 
		debugger_request_ptr, 
		debugger_request_type_ptr);
    );
}
 

static bool
MR_found_match(const MR_Stack_Layout_Label *layout,
	MR_Trace_Port port, Unsigned seqno, Unsigned depth,
	/* XXX live vars */
	const char *path, Word search_data)
{
	bool result;

	/* XXX get live vars from registers */
	Word arguments = /* XXX FIXME!!! */ 0;
	if (MR_ENTRY_LAYOUT_COMPILER_GENERATED(layout->MR_sll_entry)) {
		MR_TRACE_CALL_MERCURY(
		    result = ML_DI_found_match_comp(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_name,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_module,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_def_module,
			(String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_pred_name,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_arity,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_mode,
			layout->MR_sll_entry->MR_sle_detism,
			arguments,
			(String) (Word) path,
			search_data);
		    );
	} else {
		MR_TRACE_CALL_MERCURY(
		    result = ML_DI_found_match_user(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			layout->MR_sll_entry->MR_sle_user.MR_user_pred_or_func,
			(String)
			layout->MR_sll_entry->MR_sle_user.MR_user_decl_module,
			(String)
			layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
			(String)
			layout->MR_sll_entry->MR_sle_user.MR_user_name,
			layout->MR_sll_entry->MR_sle_user.MR_user_arity,
			layout->MR_sll_entry->MR_sle_user.MR_user_mode,
			layout->MR_sll_entry->MR_sle_detism,
			arguments,
			(String) path,
			search_data);
		    );
	}
	return result;
}

static void    
MR_send_message_to_socket_format(const char *format, ...)
{
	va_list args;

       	va_start(args, format);
       	vfprintf(MR_debugger_socket_out.file, format, args);
       	va_end(args);
       	fflush(MR_debugger_socket_out.file);
       	MR_debugger_socket_out.line_number++;
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
** The memory needed will be allocated on the Mercury heap.
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

    MR_TRACE_USE_HP(
	var_names_list = list_empty();
	for (i = var_count - 1; i >= 0; i--) {
		name = MR_name_if_present(vars, i);
		/*
		** XXX The printing of type_infos is buggy at the moment
		** due to the fake arity of the type private_builtin:typeinfo/1.
		*/
		if ((strncmp(name, "TypeInfo", 8) == 0)
		    || (strncmp(name, "TypeClassInfo", 13) == 0)
		    ) {
		  continue;
		}

		var_names_list = list_cons(name, var_names_list);
	}
    );

	return var_names_list;
}


/*
** This function returns the list of types of currently live variables.
** The memory needed will be allocated on the Mercury heap.
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
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;

	Word				type_list;

	var_count = layout->MR_sll_var_count;
	vars = &layout->MR_sll_var_info;

	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);

        MR_TRACE_USE_HP(
		type_list = list_empty();
        );

	/* 
	** If no information on live variables is available, return the 
	** empty list 
	*/
	if (layout->MR_sll_var_count <= 0) {
		return type_list;
	}

	type_params = MR_materialize_typeinfos_base(vars,
	      	saved_regs, base_sp, base_curfr);

	for (i = var_count - 1; i >= 0; i--) {

		name = MR_name_if_present(vars, i);
		var = &vars->MR_slvs_pairs[i];

		if (! MR_get_type_filtered(var, saved_regs, name, type_params, 
			&type_info))
		{
			continue;
		}

		MR_TRACE_CALL_MERCURY(
			type_info_string = ML_type_name(type_info);
		);
	        MR_TRACE_USE_HP(
			type_list = list_cons(type_info_string, type_list);
	        );
	}

	return type_list;
}


/*
** This function returns the requested live variable, as a univ.
** Any memory needed will be allocated on the Mercury heap.
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
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;

	Word				univ;

	var_number = MR_get_var_number(debugger_request);
		/* debugger_request should be of the form: 
		   current_nth_var(var_number) */
	vars = &layout->MR_sll_var_info;
	name = MR_name_if_present(vars, var_number);
	var = &vars->MR_slvs_pairs[var_number];
	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	
	/*
	** Should never occur since we check in the external debugger
	** process if a variable is live before retrieving it.
	*/
	if (layout->MR_sll_var_count <= 0) {
		fatal_error("try to retrieve a non-live variable");
	}

       	type_params = MR_materialize_typeinfos_base(vars,
	       	saved_regs, base_sp, base_curfr);

	MR_TRACE_USE_HP(
		incr_hp(univ, 2);
	);

	if (MR_get_type_and_value_filtered(var, saved_regs, name,
			type_params, &type_info, &value))
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

	return univ;
}


/*
** This function is called only when debugger_request = current_nth_var(n).
** It returns the integer 'n'.  
*/

static int
MR_get_var_number(Word debugger_request)
{
	int num;
	MR_TRACE_CALL_MERCURY(
		num = ML_DI_get_var_number(debugger_request);
	);
	return num;
}

/*
** The protocol between the debugged Mercury program and the external debugger
** is the following: 
** 1) The debugger sends "stack";
** 2) For each procedure in the stack that is not generated by the compiler, the
**    debuggee sends: 
**	- level(int) (the level of the procedure in the stack)
**	- detail(unsigned long, unsigned long, unsigned long) if available
**	  (the call event number, call sequence number and depth of the goal 
**	  of the procedure)
**	- the atom 'pred' or 'func' depending if the procedure is a function 
**	  or not
**	- proc('string:string'/long-long) (the name of the procedure)
**	- det(string) (the determinism of the procedure)
**	- def_module(string) (the name of the defining module if different from
**	  the current one)
**
**    For each compiler generated procedures, the debuggee sends:
**	- level(int) (as above)
**	- detail(unsigned long, unsigned long, unsigned long) (as above)
**	- proc('string for string:string'/long-long) (the name of the 
**	  compiler-generated procedure)
**	- def_module(string) (as above)
** 3) The debuggee sends "end_stack"
*/

static void
MR_dump_stack_record_print_to_socket(FILE *fp, 
	const MR_Stack_Layout_Entry *entry_layout, int count, int start_level, 
	Word *base_sp, Word *base_curfr)
{
	MR_send_message_to_socket_format("level(%d).\n", start_level);
	MR_print_proc_id_to_socket(entry_layout, NULL, base_sp, base_curfr);
}


static void
MR_print_proc_id_to_socket(const MR_Stack_Layout_Entry *entry,
	const char *extra, Word *base_sp, Word *base_curfr)
{
	if (! MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)) {
		fatal_error("cannot retrieve procedure id without layout");
	}

	if (base_sp != NULL && base_curfr != NULL) {
		bool print_details = FALSE;
		if (MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
			Integer maybe_from_full =
				entry->MR_sle_maybe_from_full;
			if (maybe_from_full > 0) {
				/*
				** for procedures compiled with shallow
				** tracing, the details will be valid only
				** if the value of MR_from_full saved in
				** the appropriate stack slot was TRUE.
			    	*/
				if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
					print_details = MR_based_stackvar(
						base_sp, maybe_from_full);
				} else {
					print_details = MR_based_framevar(
						base_curfr, maybe_from_full);
				}
			} else {
				/*
				** for procedures compiled with full tracing,
				** always print out the details
				*/
				print_details = TRUE;
			}
		}
		if (print_details) {
			if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
				MR_send_message_to_socket_format( 
					"detail(%lu, %lu, %lu).\n",
					(unsigned long)
					MR_event_num_stackvar(base_sp) + 1,
					(unsigned long)
					MR_call_num_stackvar(base_sp),
					(unsigned long)
					MR_call_depth_stackvar(base_sp));
			} else {
				MR_send_message_to_socket_format( 
					"detail(%lu, %lu, %lu).\n",
					(unsigned long)
					MR_event_num_framevar(base_curfr) + 1,
					(unsigned long)
					MR_call_num_framevar(base_curfr),
					(unsigned long)
					MR_call_depth_framevar(base_curfr));
			}
		} 
	}

	if (MR_ENTRY_LAYOUT_COMPILER_GENERATED(entry)) {
		MR_send_message_to_socket_format(
			/* XXX Names with ' may cause some problems here */
			"proc('%s for %s:%s'/%ld-%ld).\n",
			entry->MR_sle_comp.MR_comp_pred_name,
			entry->MR_sle_comp.MR_comp_type_module,
			entry->MR_sle_comp.MR_comp_type_name,
			(long) entry->MR_sle_comp.MR_comp_arity,
			(long) entry->MR_sle_comp.MR_comp_mode);

		if (strcmp(entry->MR_sle_comp.MR_comp_type_module,
				entry->MR_sle_comp.MR_comp_def_module) != 0)
		{
			MR_send_message_to_socket_format(
				"def_module(\"%s\").\n",
				entry->MR_sle_comp.MR_comp_def_module);
		}
	} else {
		if (entry->MR_sle_user.MR_user_pred_or_func == MR_PREDICATE) {
			MR_send_message_to_socket("pred");
		} else if (entry->MR_sle_user.MR_user_pred_or_func ==
				MR_FUNCTION)
		{
			MR_send_message_to_socket("func");
		} else {
			fatal_error("procedure is not pred or func");
		}
		
		MR_send_message_to_socket_format(
			/* XXX Names with ' may cause some problems here */
			"proc('%s:%s'/%ld-%ld).\n",
			entry->MR_sle_user.MR_user_decl_module,
			entry->MR_sle_user.MR_user_name,
			(long) entry->MR_sle_user.MR_user_arity,
			(long) entry->MR_sle_user.MR_user_mode);

		if (strcmp(entry->MR_sle_user.MR_user_decl_module,
				entry->MR_sle_user.MR_user_def_module) != 0)
		{
			MR_send_message_to_socket_format(
				"def_module(\"%s\").\n",
				entry->MR_sle_user.MR_user_def_module);
		}
	}

	MR_send_message_to_socket_format("det(\"%s\").\n", 
		MR_detism_names[entry->MR_sle_detism]);

}

static void
MR_get_list_modules_to_import(Word debugger_request, 
	Integer *modules_list_length_ptr, Word *modules_list_ptr)
{
	MR_TRACE_CALL_MERCURY(
		ML_DI_get_list_modules_to_import(
			debugger_request, 
			modules_list_length_ptr, 
			modules_list_ptr);
		);
}

static void
MR_get_mmc_options(Word debugger_request, String *mmc_options_ptr)
{
	MR_TRACE_CALL_MERCURY(
		ML_DI_get_mmc_options(
			debugger_request, 
			mmc_options_ptr);
		);
}

#endif /* MR_USE_EXTERNAL_DEBUGGER */
