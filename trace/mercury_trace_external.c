/*
** Copyright (C) 1998-2002 The University of Melbourne.
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
#include "mercury_trace_browse.h"
#include "mercury_trace_vars.h"

#include "mdb.debugger_interface.h"
#include "mdb.collect_lib.h"
#ifdef MR_HIGHLEVEL_CODE
  #include "mercury.type_desc.h"
#else
  #include "type_desc.h"
#endif

#include "mercury_deep_copy.h"

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
#include <stdlib.h>
#ifdef MR_HAVE_DLFCN_H
  #include <dlfcn.h>
#endif


/*
** This type must match the definition of classify_request in
** browser/debugger_interface.m.
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
	MR_REQUEST_MMC_OPTIONS	 = 16,/* pass down new options to compile
					 queries with			      */
	MR_REQUEST_BROWSE	 = 17,/* call the term browser	              */
	MR_REQUEST_LINK_COLLECT	 = 18,/* dynamically link the collect module  */
	MR_REQUEST_COLLECT	 = 19,/* collecting monitoring informations   */
	MR_REQUEST_CURRENT_GRADE = 20,/* retrieving the grade of the current
					 program has been compiled with       */
	MR_REQUEST_COLLECT_ARG_ON
				 = 21,/* switch the arguments collecting on   */
	MR_REQUEST_COLLECT_ARG_OFF
				 = 22 /* switch the arguments collecting off  */

} MR_debugger_request_type;

MercuryFile MR_debugger_socket_in;
MercuryFile MR_debugger_socket_out;

static MR_String	MR_mmc_options;

/*
** Type of a static variable that indicates in which mode the external 
** debugger is. When the external debugger is in mode:
** (1) `MR_searching', it tries to find an event that matches a forward 
**      move request,
** (2) `MR_reading_request', it reads a new request on the socket,
** (3) `MR_collecting', it is collecting information (after a `collect' request).
*/
typedef enum {
	MR_searching, MR_reading_request, MR_collecting
} MR_external_debugger_mode_type;

static	MR_external_debugger_mode_type 
	external_debugger_mode = MR_reading_request;

/*
** Global variable that is used to store the information collected during 
** a collect request.
*/

static  MR_Word	MR_accumulator_variable;

/*
** Global variable that is sent to collect caller.
*/

static  MR_Word	MR_collected_variable;

/*
** Function pointer used to post-process the result of the collect activity
*/

static void	(*post_process_ptr)(MR_Word, MR_Word *);

/*
** Function pointer used to sent the collecting variable to the external 
** debugger.
*/

static	void	(*send_collect_result_ptr)(MR_Word, MR_Word);

/*
** Variable generated during the dynamic linking that is needed to close
** this linking properly.
*/

static	MR_Word	collect_lib_maybe_handle;

/*
** Static variable that tells whether the list of arguments is available 
** within a collect module.
*/

static	MR_bool	MR_collect_arguments = MR_FALSE;

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
			MR_Word *debugger_request_ptr, 
			MR_Integer *debugger_request_type_ptr);
	
static MR_bool	MR_found_match(const MR_Label_Layout *layout,
			MR_Trace_Port port, MR_Unsigned seqno,
			MR_Unsigned depth,
			/* XXX registers */
			const char *path, MR_Word search_data);
static void	MR_output_current_slots(const MR_Label_Layout *layout,
			MR_Trace_Port port, MR_Unsigned seqno,
			MR_Unsigned depth, const char *path, int lineno);
static void	MR_output_current_vars(MR_Word var_list, MR_Word string_list);
static void	MR_output_current_nth_var(MR_Word var);
static void	MR_output_current_live_var_names(MR_Word var_names_list, 
						 MR_Word type_list);
static MR_Word	MR_trace_make_var_list(void);
static MR_Word	MR_trace_make_var_names_list(void);
static MR_Word	MR_trace_make_type_list(void);
static MR_Word	MR_trace_make_nth_var(MR_Word debugger_request);
static int	MR_get_var_number(MR_Word debugger_request);
static void	MR_print_proc_id_to_socket(const MR_Proc_Layout *entry,
			const char *extra,
			MR_Word *base_sp, MR_Word *base_curfr);
static void	MR_dump_stack_record_print_to_socket(FILE *fp, 
			const MR_Proc_Layout *entry_layout, int count,
			int start_level, MR_Word *base_sp, MR_Word *base_curfr,
			const char *filename, int linenumber,
			const char *goal_path, MR_bool context_mismatch);
static void	MR_get_list_modules_to_import(MR_Word debugger_request, 
			MR_Integer *modules_list_length_ptr,
			MR_Word *modules_list_ptr);
static void	MR_get_mmc_options(MR_Word debugger_request, 
			MR_String *mmc_options_ptr);
static void	MR_get_object_file_name(MR_Word debugger_request, 
			MR_String *object_file_name_ptr);
static void	MR_get_variable_name(MR_Word debugger_request,
			MR_String *var_name_ptr);
static void	MR_trace_browse_one_external(MR_Var_Spec which_var);
static void	MR_send_collect_result(void);

#if 0
This pseudocode should go in the debugger process:

#define SOCKET_PATH "/var/tmp/"		/* +5 for pid = 14 chars */
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
			MR_fatal_error("unix socket: length != 16");
		}
	#endif
}
#endif

static MR_bool MR_debug_socket = MR_FALSE;

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
	MR_Word debugger_request;
	MR_Integer debugger_request_type;

	/* 
	** MR_mmc_options contains the options to pass to mmc when compiling 
	** queries. We initialize it to the MR_String "".
	*/
	MR_TRACE_CALL_MERCURY(ML_DI_init_mercury_string(&MR_mmc_options));

	/*
	** We presume that the user's program has been invoked from
	** within the debugger (e.g. Morphine).
	** The debugger (or the user) should set the
	** MERCURY_DEBUGGER_UNIX_SOCKET or MERCURY_DEBUGGER_INET_SOCKET
	** environment variable to tell the user's program which socket
	** it needs to connect to.
	*/
	unix_socket = getenv("MERCURY_DEBUGGER_UNIX_SOCKET");
	inet_socket = getenv("MERCURY_DEBUGGER_INET_SOCKET");
	if (unix_socket == NULL && inet_socket == NULL) {
		MR_fatal_error("you must set either the "
			"MERCURY_DEBUGGER_UNIX_SOCKET\n"
			"or MERCURY_DEBUGGER_INET_SOCKET "
			"environment variable");
	}
	if (unix_socket != NULL && inet_socket != NULL) {
		MR_fatal_error("you must set only one of the "
			"MERCURY_DEBUGGER_UNIX_SOCKET "
			"and MERCURY_DEBUGGER_INET_SOCKET\n"
			"environment variables");
	}
	if (unix_socket) {
		/*
		** We use `(memset)(...)' rather than `memset'
		** to prevent macro expansion; this is needed because
		** with GNU libc 2.1.2 and gcc 2.95, gcc barfs on the latter,
		** with an error message about impossible asm due to a conflict
		** between our use of global register variables and the inline
		** assembler macro definition of memset.
		*/
		 
		addr_family = AF_UNIX;
		(memset)(&unix_address, 0, sizeof(unix_address));
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
			MR_fatal_error("MERCURY_DEBUGGER_INET_SOCKET invalid");
		}
		host_addr = inet_addr(hostname);
		if (host_addr == -1) {
			MR_fatal_error("MERCURY_DEBUGGER_INET_SOCKET: "
				"invalid address");
		}
		if (sscanf(port_string, "%hu", &port) != 1) {
			MR_fatal_error("MERCURY_DEBUGGER_INET_SOCKET: "
				"invalid port");
		}

		if (MR_debug_socket) {
			fprintf(stderr, "Mercury runtime: host = %s, port = %d\n",
				hostname, port);
		}
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
		MR_fatal_error("cannot open socket for debugger");
	} else if (MR_debug_socket) {
		fprintf(stderr,"Mercury runtime: creation of socket ok\n");
	}

	/*
	** Connect to the socket
	*/
	if (connect(fd, addr, len) < 0) {
		fprintf(stderr, "Mercury runtime: connect() failed: %s\n",
			strerror(errno));
		MR_fatal_error("can't connect to debugger socket");
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
		MR_fatal_error("cannot open debugger socket");
	} else if (MR_debug_socket) {
		fprintf(stderr, "Mercury runtime: fdopen(): ok\n");
	}

	MR_mercuryfile_init(file_in, 1, &MR_debugger_socket_in);
	MR_mercuryfile_init(file_out, 1, &MR_debugger_socket_out);

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
		MR_fatal_error("unexpected command on debugger socket");
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
	** This can only happen during a forward_move or a 
	** collect request. In the first case, we want to tell 
	** the debugger that no match was found; in the second 
	** one we send the result of the collect activity.
	*/

	switch(external_debugger_mode) {
		case MR_searching:
			MR_send_message_to_socket("forward_move_match_not_found");
			break;

		case MR_collecting:
			MR_send_collect_result();
			MR_send_message_to_socket("execution_terminated");
			break;

		default:
			MR_fatal_error("Error in the external debugger");
	}
	/*
	** Maybe we should loop to process requests from the
	** debugger socket here?  Currently we just return,
	** which will result in the debuggee terminating.
	** (This will need to change if the debuggee is to record
	** trace history and support a `move_backward' request.)
	*/
}

MR_Code *
MR_trace_event_external(MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info)
{
	static MR_Word	search_data;
	static void	(*initialize_ptr)(MR_Word *);
	static void	(*get_collect_var_type_ptr)(MR_Word *);
	static MR_bool    	collect_linked = MR_FALSE;
	MR_bool    	stop_collecting = MR_FALSE;
	MR_Integer		debugger_request_type;
	MR_Word			debugger_request;
	MR_Word			var_list;
	MR_Word			var_names_list;
	MR_Word			type_list;
	MR_Word			var;
	MR_Code			*jumpaddr = NULL;
	MR_Event_Details	event_details;
	const char		*message;
	MR_bool			include_trace_data = MR_TRUE;
	const MR_Label_Layout	*layout = event_info->MR_event_sll;
	MR_Unsigned		seqno = event_info->MR_call_seqno;
	MR_Unsigned		depth = event_info->MR_call_depth;
	MR_Trace_Port		port = event_info->MR_trace_port;
	const char		*path = event_info->MR_event_path;
	MR_Word			*saved_regs = event_info->MR_saved_regs;
	MR_Integer		modules_list_length;
	MR_Word			modules_list;
	MR_Retry_Result		retry_result;
	static MR_String	MR_object_file_name;
	int			lineno = 0;

	MR_trace_enabled = MR_FALSE;

	/*
	** These globals can be overwritten when we call Mercury code,
	** such as the code in browser/debugger_interface.m.
	** We therefore save them here and restore them before
	** exiting from this function.  However, we store the
	** saved values in a structure that we pass to MR_trace_debug_cmd,
	** to allow them to be modified by MR_trace_retry().
	*/
	event_details.MR_call_seqno = MR_trace_call_seqno;
	event_details.MR_call_depth = MR_trace_call_depth;
	event_details.MR_event_number = MR_trace_event_number;

	MR_trace_init_point_vars(event_info->MR_event_sll,
		event_info->MR_saved_regs, event_info->MR_trace_port,
		MR_FALSE);

	switch(external_debugger_mode) {
		case MR_searching:
			/* 
			** XXX should also pass registers here, since they're 
			** needed for checking for matches with the arguments 
			*/
			if (MR_found_match(layout, port, seqno, depth,
				/* XXX registers, */ path, search_data))
			{
				MR_send_message_to_socket(
					"forward_move_match_found");
				external_debugger_mode = MR_reading_request;
			} else {
				goto done;
			}
			break;

		case MR_collecting:
		        MR_send_collect_result(); 
			MR_send_message_to_socket("execution_continuing");
			break;

		case MR_reading_request:
			break;

		default:
	       		MR_fatal_error("Software error in the debugger.\n");
	}
	
	lineno = MR_get_line_number(event_info->MR_saved_regs, layout, port);

	/* loop to process requests read from the debugger socket */
	for(;;) {
		MR_read_request_from_socket(
			&debugger_request, &debugger_request_type);
		switch((int) debugger_request_type) {
			case MR_REQUEST_ABORT_PROG:
				exit(EXIT_SUCCESS);

			case MR_REQUEST_FORWARD_MOVE:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"FORWARD_MOVE\n");
				}
				search_data = debugger_request;
			        external_debugger_mode = MR_searching;
				goto done;

			case MR_REQUEST_CURRENT_LIVE_VAR_NAMES:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"MR_REQUEST_CURRENT_LIVE_VAR"
						"_NAMES\n");
				}
				var_names_list = 
					MR_trace_make_var_names_list();
				type_list = MR_trace_make_type_list();
				MR_output_current_live_var_names(var_names_list,
					type_list);
				break;

			case MR_REQUEST_CURRENT_VARS:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_CURRENT_VARS\n");
				}
				var_list = MR_trace_make_var_list();
				var_names_list = 
					MR_trace_make_var_names_list();
				MR_output_current_vars(var_list, 
						       var_names_list);
				break;

			case MR_REQUEST_CURRENT_NTH_VAR:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_NTH_CURRENT_VAR\n");
				}
				var = MR_trace_make_nth_var(debugger_request);
				MR_output_current_nth_var(var);
				break;			
			case MR_REQUEST_CURRENT_SLOTS:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_CURRENT_SLOTS\n");
				}
				MR_output_current_slots(layout, port, seqno, 
					depth, path, lineno);
				break;

			case MR_REQUEST_RETRY:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_RETRY\n");
				}
				retry_result = MR_trace_retry(event_info, 
					&event_details, 0, &message,
					NULL, NULL, &jumpaddr);
				if (retry_result == MR_RETRY_OK_DIRECT) {
					MR_send_message_to_socket("ok");
					cmd->MR_trace_cmd = MR_CMD_GOTO;
					cmd->MR_trace_stop_event = 
						MR_trace_event_number + 1;
					goto done;
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
				MR_trace_init_modules();
				message = MR_dump_stack_from_layout(
					stdout, layout,
					MR_saved_sp(saved_regs),
					MR_saved_curfr(saved_regs),
					include_trace_data, MR_FALSE,
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
				MR_trace_init_modules();
				/* 
			        ** XXX As in stack dump, we could send the
				** output of this function on the socket. But
				** the outputs are done via fprintf() and
				** printlabel(), so we would need to define new
				** fprintf() and printlabel() and pass them
				** down as parameters of
				** MR_dump_nondet_stack() (as we do
				** with MR_dump_stack_record_print()).
				*/						
				MR_dump_nondet_stack(stdout,
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

			case MR_REQUEST_BROWSE:
			  {
				char		*var_name;
				MR_Var_Spec	var_spec;

				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_BROWSE\n");
				}
				MR_get_variable_name(debugger_request, 
					&var_name);
				var_spec.MR_var_spec_kind = MR_VAR_SPEC_NAME;
				var_spec.MR_var_spec_name = var_name;
				MR_trace_browse_one_external(var_spec);
				MR_send_message_to_socket("browser_end");
				break;
			  }
			case MR_REQUEST_NO_TRACE:
				cmd->MR_trace_cmd = MR_CMD_TO_END;
				external_debugger_mode = MR_searching;
				goto done;

			case MR_REQUEST_LINK_COLLECT:
			  {
			        MR_Char	result;
				MR_Word	MR_accumulator_variable_type;

				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_LINK_COLLECT\n");
				}
				MR_get_object_file_name(debugger_request,
					    &MR_object_file_name);
				MR_TRACE_CALL_MERCURY(
					ML_CL_link_collect(
			       		    MR_object_file_name,
					    (MR_Word *) &cmd->MR_filter_ptr,
					    (MR_Word *) &initialize_ptr,
					    (MR_Word *) &post_process_ptr,
					    (MR_Word *) &send_collect_result_ptr,
					    (MR_Word *) &get_collect_var_type_ptr,
					    &collect_lib_maybe_handle,
					    &result
					    ));
				collect_linked = (result == 'y');
				if (collect_linked) {
					MR_send_message_to_socket(
						"link_collect_succeeded");
					MR_TRACE_CALL_MERCURY(
					    (*get_collect_var_type_ptr)(
						&MR_accumulator_variable_type));
					MR_accumulator_variable = 
					    MR_make_permanent(
						MR_accumulator_variable,
						(MR_TypeInfo) 
						MR_accumulator_variable_type);
				} else {
					MR_send_message_to_socket(
						"link_collect_failed");
				}
				break;
			  }
			case MR_REQUEST_COLLECT:
			  {
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_COLLECT\n");
				}
				if (collect_linked) {
					MR_send_message_to_socket(
						"collect_linked");
					external_debugger_mode = MR_collecting;
					MR_TRACE_CALL_MERCURY(
					  (*initialize_ptr)(&MR_accumulator_variable));

					/*
					** In order to perform the collect from
					** the current event, we need to call 
					** filter once here.
					*/
					MR_COLLECT_filter(cmd->MR_filter_ptr,
						seqno, depth, port, layout, path, 
						lineno, &stop_collecting);

					if (stop_collecting) {
						MR_send_collect_result();
						MR_send_message_to_socket(
							"execution_continuing");
						break;
					} else {
					/*
					** For efficiency, the remaining calls
					** to MR_COLLECT_filter() are done in 
					** MR_trace_real().
					*/
					        cmd->MR_trace_cmd =
							MR_CMD_COLLECT;
						cmd->MR_trace_must_check =
							MR_FALSE;
						cmd->MR_trace_strict = MR_TRUE;
						cmd->MR_trace_print_level = 
						 	MR_PRINT_LEVEL_NONE;
						goto done;
					}
				} else {
					MR_send_message_to_socket(
						"collect_not_linked");
					break;
				}
			  }

			case MR_REQUEST_CURRENT_GRADE:
			  {
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_CURRENT_GRADE\n");
				}
				MR_send_message_to_socket_format(
						"grade(\"%s\").\n", 
						MR_GRADE_OPT);
				break;
			  }

			case MR_REQUEST_COLLECT_ARG_ON:
			  {
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_COLLECT_ARG_ON\n");
				}
				MR_collect_arguments = MR_TRUE;
				MR_send_message_to_socket("collect_arg_on_ok");
				break;
			  }
			case MR_REQUEST_COLLECT_ARG_OFF:
			  {
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_COLLECT_ARG_OFF\n");
				}
				MR_collect_arguments = MR_FALSE;
				MR_send_message_to_socket("collect_arg_off_ok");
				break;
			  }
			default:
				MR_fatal_error("unexpected request read from "
					"debugger socket");
		}
	}

done:
	/*
	** Recalculate the `must_check' flag, in case we
	** changed the command strictness or print-level
	*/
	cmd->MR_trace_must_check = (! cmd->MR_trace_strict) ||
			(cmd->MR_trace_print_level != MR_PRINT_LEVEL_NONE);

	/*
	** Restore the event numbers, in case the Mercury
	** code that we call from the trace system
	** (e.g. browser/debugger_interface.m)
	** clobbered them.  That could happen if that code
	** had been compiled with debugging enabled.
	*/
	MR_trace_call_seqno = event_details.MR_call_seqno;
	MR_trace_call_depth = event_details.MR_call_depth;
	MR_trace_event_number = event_details.MR_event_number;

	MR_trace_enabled = MR_TRUE;

	return jumpaddr;
}

static void
MR_output_current_slots(const MR_Label_Layout *layout,
	MR_Trace_Port port, MR_Unsigned seqno, MR_Unsigned depth,
	const char *path, int lineno)
{
	if (MR_PROC_LAYOUT_COMPILER_GENERATED(layout->MR_sll_entry)) {
		MR_TRACE_CALL_MERCURY(
		    ML_DI_output_current_slots_comp(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_name,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_def_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_pred_name,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_arity,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_mode,
			layout->MR_sll_entry->MR_sle_detism,
			(MR_String) (MR_Word) path,
			lineno,
			(MR_Word) &MR_debugger_socket_out);
		    );
	} else {
		MR_TRACE_CALL_MERCURY(
		    ML_DI_output_current_slots_user(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			layout->MR_sll_entry->MR_sle_user.MR_user_pred_or_func,
			(MR_String)
			layout->MR_sll_entry->MR_sle_user.MR_user_decl_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_user.MR_user_name,
			layout->MR_sll_entry->MR_sle_user.MR_user_arity,
			layout->MR_sll_entry->MR_sle_user.MR_user_mode,
			layout->MR_sll_entry->MR_sle_detism,
			(MR_String) (MR_Word) path,
			lineno,
			(MR_Word) &MR_debugger_socket_out);
		    );
	}
}

static void
MR_output_current_vars(MR_Word var_list, MR_Word string_list)
{
    MR_TRACE_CALL_MERCURY(
	ML_DI_output_current_vars(
		var_list,
		string_list,
		(MR_Word) &MR_debugger_socket_out);
    );
}

static void
MR_output_current_nth_var(MR_Word var)
{
    MR_TRACE_CALL_MERCURY(
	ML_DI_output_current_nth_var(
		var,
		(MR_Word) &MR_debugger_socket_out);
    );
}

static void
MR_output_current_live_var_names(MR_Word var_names_list, MR_Word type_list)
{
    MR_TRACE_CALL_MERCURY(
	ML_DI_output_current_live_var_names(
		var_names_list,
		type_list,
		(MR_Word) &MR_debugger_socket_out);
    );
}

static void
MR_read_request_from_socket(
			MR_Word *debugger_request_ptr, 
			MR_Integer *debugger_request_type_ptr)
{		
	fflush(MR_file(MR_debugger_socket_in));

    MR_TRACE_CALL_MERCURY(
	ML_DI_read_request_from_socket(
		(MR_Word) &MR_debugger_socket_in, 
		debugger_request_ptr, 
		debugger_request_type_ptr);
    );
}
 
static MR_bool
MR_found_match(const MR_Label_Layout *layout,
	MR_Trace_Port port, MR_Unsigned seqno, MR_Unsigned depth,
	/* XXX live vars */
	const char *path, MR_Word search_data)
{
	MR_bool result;

	/* XXX get live vars from registers */
	MR_Word arguments = /* XXX FIXME!!! */ 0;
	if (MR_PROC_LAYOUT_COMPILER_GENERATED(layout->MR_sll_entry)) {
		MR_TRACE_CALL_MERCURY(
		    result = ML_DI_found_match_comp(
			MR_trace_event_number,
			seqno,
			depth,
			port,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_name,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_type_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_def_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_comp.MR_comp_pred_name,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_arity,
			layout->MR_sll_entry->MR_sle_comp.MR_comp_mode,
			layout->MR_sll_entry->MR_sle_detism,
			arguments,
			(MR_String) (MR_Word) path,
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
			(MR_String)
			layout->MR_sll_entry->MR_sle_user.MR_user_decl_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
			(MR_String)
			layout->MR_sll_entry->MR_sle_user.MR_user_name,
			layout->MR_sll_entry->MR_sle_user.MR_user_arity,
			layout->MR_sll_entry->MR_sle_user.MR_user_mode,
			layout->MR_sll_entry->MR_sle_detism,
			arguments,
			(MR_String) path,
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
	vfprintf(MR_file(MR_debugger_socket_out), format, args);
	va_end(args);
	fflush(MR_file(MR_debugger_socket_out));
	MR_line_number(MR_debugger_socket_out)++;
}

static void
MR_send_message_to_socket(const char *message)
{
	fprintf(MR_file(MR_debugger_socket_out), "%s.\n", message);
	fflush(MR_file(MR_debugger_socket_out));
	MR_line_number(MR_debugger_socket_out)++;
}

/*
** This function returns the list of the currently live variables
** as a list of univs.
** The memory needed will be allocated on the Mercury heap.
**
** If no information on live variables is available, or if there
** are no live variables, return the empty list.
*/

static MR_Word
MR_trace_make_var_list(void)
{
	const char	*problem;
	int		var_count;
	int		i;
	MR_TypeInfo	type_info;
	MR_Word		value;
	MR_Word		univ;
	MR_Word		var_list;

	var_count = MR_trace_var_count();

	MR_TRACE_USE_HP(
		var_list = MR_list_empty();
	);

	for (i = var_count; i > 0; i--) {
		problem = MR_trace_return_var_info(i, NULL,
				&type_info, &value);
		if (problem != NULL) {
			MR_fatal_error(problem);
		}

		MR_TRACE_USE_HP(
			MR_new_univ_on_hp(univ, type_info, value);
		);

		MR_TRACE_USE_HP(
			var_list = MR_list_cons(univ, var_list);
		);
	}

	return var_list;
}

/*
** This function returns the list of the internal names of currently live
** variables.
** The memory needed will be allocated on the Mercury heap.
**
** If no information on live variables is available, or if there
** are no live variables, return the empty list.
*/

static MR_Word
MR_trace_make_var_names_list(void)
{
	const char	*problem;
	int		var_count;
	int		i;
	const char	*name;
	MR_Word		var_names_list;

	var_count = MR_trace_var_count();

	MR_TRACE_USE_HP(
		var_names_list = MR_list_empty();
	);

	for (i = var_count; i > 0; i--) {
		problem = MR_trace_return_var_info(i, &name, NULL, NULL);
		if (problem != NULL) {
			MR_fatal_error(problem);
		}

		MR_TRACE_USE_HP(
			var_names_list = MR_list_cons((MR_Word) name,
				var_names_list);
		);
	}

	return var_names_list;
}

/*
** This function returns the list of types of currently live variables.
** The memory needed will be allocated on the Mercury heap.
**
** If no information on live variables is available, or if there
** are no live variables, return the empty list.
*/

static MR_Word
MR_trace_make_type_list(void)
{
	const char	*problem;
	int		var_count;
	int		i;
	MR_TypeInfo	type_info;
	MR_String	type_info_string;
	MR_Word		type_list;

	var_count = MR_trace_var_count();

        MR_TRACE_USE_HP(
		type_list = MR_list_empty();
        );

	for (i = var_count; i > 0; i--) {
		problem = MR_trace_return_var_info(i, NULL, &type_info, NULL);
		if (problem != NULL) {
			MR_fatal_error(problem);
		}

		MR_TRACE_CALL_MERCURY(
			type_info_string = ML_type_name((MR_Word) type_info);
		);
	        MR_TRACE_USE_HP(
			type_list = MR_list_cons((MR_Word) type_info_string,
				type_list);
	        );
	}

	return type_list;
}

/*
** This function returns the requested live variable, as a univ.
** Any memory needed will be allocated on the Mercury heap.
*/

static MR_Word
MR_trace_make_nth_var(MR_Word debugger_request)
{
	const char	*problem;
	int		var_number;
	MR_TypeInfo	type_info;
	MR_Word		value;
	MR_Word		univ;

	var_number = MR_get_var_number(debugger_request);
		/* debugger_request should be of the form: 
		   current_nth_var(var_number) */

	problem = MR_trace_return_var_info(var_number, NULL,
			&type_info, &value);
	if (problem == NULL) {
		MR_TRACE_USE_HP(
			MR_new_univ_on_hp(univ, type_info, value);
		);
	} else {
		/*
		** Should never occur since we check in the external debugger
		** process if a variable is live before retrieving it.
		*/
		MR_fatal_error(problem);
	}

	return univ;
}

/*
** This function is called only when debugger_request = current_nth_var(n).
** It returns the integer 'n'.  
*/

static int
MR_get_var_number(MR_Word debugger_request)
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
**	- proc(string, string, long, long) (the procedure)
**	- det(string) (the determinism of the procedure)
**	- def_module(string) (the name of the defining module) if different from
**	  the current one.
**
**    For each compiler generated procedures, the debuggee sends:
**	- level(int) (as above)
**	- detail(unsigned long, unsigned long, unsigned long) (as above)
**	- proc(string, string, string, long, long) (the name of the 
**	  compiler-generated procedure)
**	- det(string) (as above)
**	- def_module(string) (as above)
** 3) The debuggee sends "end_stack"
*/

static void
MR_dump_stack_record_print_to_socket(FILE *fp, 
	const MR_Proc_Layout *entry_layout, int count, int start_level, 
	MR_Word *base_sp, MR_Word *base_curfr,
	const char *filename, int linenumber,
	const char *goal_path, MR_bool context_mismatch)
{
	MR_send_message_to_socket_format("level(%d).\n", start_level);
	MR_print_proc_id_to_socket(entry_layout, NULL, base_sp, base_curfr);
}

static void
MR_print_proc_id_to_socket(const MR_Proc_Layout *entry,
	const char *extra, MR_Word *base_sp, MR_Word *base_curfr)
{
	if (! MR_PROC_LAYOUT_HAS_PROC_ID(entry)) {
		MR_fatal_error("cannot retrieve procedure id without layout");
	}

	if (base_sp != NULL && base_curfr != NULL) {
		MR_bool print_details = MR_FALSE;
		if (MR_PROC_LAYOUT_HAS_EXEC_TRACE(entry)) {
			MR_Integer maybe_from_full =
				entry->MR_sle_maybe_from_full;
			if (maybe_from_full > 0) {
				/*
				** for procedures compiled with shallow
				** tracing, the details will be valid only
				** if the value of MR_from_full saved in
				** the appropriate stack slot was MR_TRUE.
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
				print_details = MR_TRUE;
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

	if (MR_PROC_LAYOUT_COMPILER_GENERATED(entry)) {
		MR_send_message_to_socket_format(
			/* XXX Names with " may cause some problems here */
			"proc(\"%s\",\"%s\",\"%s\",%ld,%ld).\n",
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
			MR_fatal_error("procedure is not pred or func");
		}
		
		MR_send_message_to_socket_format(
			/* XXX Names with " may cause some problems here */
			"proc(\"%s\",\"%s\",%ld,%ld).\n",
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
MR_get_list_modules_to_import(MR_Word debugger_request, 
	MR_Integer *modules_list_length_ptr, MR_Word *modules_list_ptr)
{
	MR_TRACE_CALL_MERCURY(
		ML_DI_get_list_modules_to_import(
			debugger_request, 
			modules_list_length_ptr, 
			modules_list_ptr);
		);
}

static void
MR_get_mmc_options(MR_Word debugger_request, MR_String *mmc_options_ptr)
{
	MR_TRACE_CALL_MERCURY(
		ML_DI_get_mmc_options(
			debugger_request, 
			mmc_options_ptr);
		);
}

static void
MR_get_object_file_name(MR_Word debugger_request, MR_String *object_file_name_ptr)
{
	MR_TRACE_CALL_MERCURY(
		ML_DI_get_object_file_name(
			debugger_request, 
			object_file_name_ptr);
		);
}

static void
MR_get_variable_name(MR_Word debugger_request, MR_String *var_name_ptr)
{
	MR_TRACE_CALL_MERCURY(
		ML_DI_get_variable_name(
			debugger_request, 
			var_name_ptr);
		);
}

/*
** This function does the same thing as MR_trace_browse_one() defined in 
** mercury_trace_internal.c except it sends/receives program-readable terms
** from/to the socket instead of sending human-readable strings from/to 
** mdb_in/mdb_out.
*/

static void
MR_trace_browse_one_external(MR_Var_Spec var_spec)
{
	const char	*problem;

	problem = MR_trace_browse_one(NULL, var_spec, MR_trace_browse_external,
			MR_BROWSE_CALLER_BROWSE, MR_BROWSE_DEFAULT_FORMAT,
			MR_TRUE);

	if (problem != NULL) {
		MR_send_message_to_socket_format("error(\"%s\").\n", problem);
	}
}


/*
** This function calls the collect filtering predicate defined by the user
** and dynamically link with the execution.
*/
void
MR_COLLECT_filter(MR_FilterFuncPtr filter_ptr, MR_Unsigned seqno, 
	MR_Unsigned depth, MR_Trace_Port port, const MR_Label_Layout *layout, 
	const char *path, int lineno, MR_bool *stop_collecting)
{
	MR_Char	result;		
	MR_Word	arguments;

	/* 
	** Only pass the arguments list down filter
	** if required, i.e. if MR_collect_arguments
	** is set to MR_TRUE. We need to do that in 
	** order to not penalize the performance 
	** of collect in the cases where the argument
	** list (which might be very big) is not used.
	** 
	*/
	if (MR_collect_arguments) {
		arguments = MR_trace_make_var_list();
	} else {
		MR_TRACE_USE_HP(
			arguments = MR_list_empty()
		);
	}
	MR_TRACE_CALL_MERCURY((*filter_ptr)(
		MR_trace_event_number,
		seqno,
		depth,
		port,
		layout->MR_sll_entry->MR_sle_user.MR_user_pred_or_func,
		(MR_String) layout->MR_sll_entry->MR_sle_user.MR_user_decl_module,
		(MR_String) layout->MR_sll_entry->MR_sle_user.MR_user_def_module,
		(MR_String) layout->MR_sll_entry->MR_sle_user.MR_user_name,
		layout->MR_sll_entry->MR_sle_user.MR_user_arity,
		layout->MR_sll_entry->MR_sle_user.MR_user_mode,
		arguments,
		layout->MR_sll_entry->MR_sle_detism,
		(MR_String) path,
		lineno,
		MR_accumulator_variable,
		&MR_accumulator_variable,
		&result));
	*stop_collecting = (result == 'y');
}

/*
** This function retrieves the line number of the current goal.
*/
int
MR_get_line_number(MR_Word *saved_regs, const MR_Label_Layout *layout, 
	MR_Trace_Port port)
{
	const char		*filename;
	const MR_Label_Layout	*parent_layout;
	const char		*problem; 
	int			lineno = 0;
	MR_Word			*base_sp, *base_curfr;

	
	if MR_port_is_interface(port)
	/* 
	** At external events, we want the line number 
	** where the call is made, not the one where the 
	** procedure is defined.
	*/
	{
		base_sp = MR_saved_sp(saved_regs);
		base_curfr = MR_saved_curfr(saved_regs);
		parent_layout = MR_find_nth_ancestor(layout, 1,
			&base_sp, &base_curfr, &problem);
		if (parent_layout != NULL) {
			(void) MR_find_context(parent_layout, &filename, &lineno);
		}
	} else {
		(void) MR_find_context(layout, &filename, &lineno);
	} ;
	return lineno;
}

static void
MR_send_collect_result(void)
{
	MR_TRACE_CALL_MERCURY(
		(*post_process_ptr)(
			MR_accumulator_variable, 
			&MR_collected_variable);

		(*send_collect_result_ptr)(
			MR_collected_variable, 
			(MR_Word) &MR_debugger_socket_out));
#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLCLOSE)
	MR_TRACE_CALL_MERCURY(
       		ML_CL_unlink_collect(collect_lib_maybe_handle));
#endif
}
#endif /* MR_USE_EXTERNAL_DEBUGGER */
