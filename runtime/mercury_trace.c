/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace.c - implements the tracing subsystem.
**
** For the general basis of trace analysis systems, see the paper
** "Opium: An extendable trace analyser for Prolog" by Mireille Ducasse,
** available from http://www.irisa.fr/lande/ducasse.
**
** Main authors: Erwan Jahier and Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include <stdio.h>

/*
** MR_trace_call_seqno counts distinct calls. The prologue of every
** procedure assigns the current value of this counter as the sequence number
** of that invocation and increments the counter. This is the only way that
** MR_trace_call_seqno is modified.
**
** MR_trace_call_depth records the current depth of the call tree. The prologue
** of every procedure assigns the current value of this variable plus one
** as the depth of that invocation. Just before making a call, the caller
** will set MR_trace_call_depth to its own remembered depth value. 
** These are the only ways in which MR_trace_call_depth is modified.
**
** Although neither MR_trace_call_seqno nor MR_trace_call_depth are used
** directly in this module, the seqno and depth arguments of MR_trace
** always derive their values from the saved values of these two global
** variables.
*/

int	MR_trace_call_seqno = 0;
int	MR_trace_call_depth = 0;

/*
** MR_trace_event_number is a simple counter of events; currently we only
** use it for display.
*/

static	int	MR_trace_event_number = 0;

/*
** MR_trace_cmd and MR_trace_seqno are globals variables that we use
** to manage an interface between the tracer and the user.
**
** MR_trace_cmd says what mode the tracer is in, i.e. what the last
** trace command was.
**
** MR_trace_seqno is meaningful only when MR_trace_cmd is MR_SKIP or MR_JUMP.
** In those cases, it holds the sequence number of the call at whose exit
** control should be given back to the user.
*/

typedef enum {
	MR_CMD_CONT,	/* c: continue to end, not printing the trace	  */
	MR_CMD_DUMP,	/* d: continue to end, printing the trace	  */
	MR_CMD_NEXT,	/* n: go to the next trace event		  */
	MR_CMD_SKIP,	/* s: skip the current call, not printing trace	  */
	MR_CMD_JUMP	/* j: jump to end of current call, printing trace */
} MR_trace_cmd_type;

/*
** This type must match the definition of classify_request in
** library/debugger_interface.m.
*/
typedef enum {
	MR_REQUEST_HELLO_REPLY = 0,  /* initiate debugging session	    */
	MR_REQUEST_FORWARD_MOVE = 1, /* go to the next matching trace event */
	MR_REQUEST_CURRENT = 2,	     /* report data for current trace event */
	MR_REQUEST_NO_TRACE = 3,     /* continue to end, not tracing	    */
	MR_REQUEST_ABORT_PROG = 4,   /* abort the current execution	    */
	MR_REQUEST_ERROR = 5         /* something went wrong                */
} MR_debugger_request_type;

static	MR_trace_cmd_type	MR_trace_cmd = MR_CMD_NEXT;
static	int			MR_trace_seqno = 0;

typedef	enum {
	MR_INTERACT,
	MR_NO_INTERACT
} MR_trace_interact;

static void	MR_trace_event(MR_trace_interact interact,
			const MR_stack_layout_entry *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);
static void	MR_trace_display_user(MR_trace_interact interact,
			const MR_stack_layout_entry *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);
static void	MR_trace_browse(int var_count,
			const MR_stack_layout_vars *var_info);
static void	MR_trace_browse_var(char *name,
			const MR_stack_layout_var *var);
static int	MR_trace_get_cmd(void);
static void	MR_trace_help(void);

static Word	MR_trace_make_var_list(MR_trace_port port,
			const MR_stack_layout_entry *layout);
static Word	MR_trace_lookup_live_lval(MR_Live_Lval locn, bool *succeeded);
static bool	MR_trace_get_type_and_value(const MR_stack_layout_var *var,
			Word *type_info, Word *value);

/*
We could use
	if (MR_use_debugger) { ...
instead of #if; this would be better in the long run.
This would require changing mercury_wrapper.c to
check for an additional flag in the MERCURY_OPTIONS
environment variable and set MR_use_debugger accordingly.
*/
#ifdef MR_USE_DEBUGGER

static MercuryFile MR_debugger_socket_in;
static MercuryFile MR_debugger_socket_out;

static void	MR_send_message_to_socket(const char *message);
static void	MR_read_request_from_socket(
			Word *debugger_request_ptr, 
			Integer *debugger_request_type_ptr);
	
static void	MR_debugger_step(MR_trace_interact interact,
			const MR_stack_layout_entry *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);
static bool	MR_found_match(const MR_stack_layout_entry *layout,
			MR_trace_port port, int seqno, int depth,
			/* XXX registers */
			const char *path, Word search_data);
static void	MR_output_current(const MR_stack_layout_entry *layout,
			MR_trace_port port, int seqno, int depth,
			Word var_list,
			const char *path, Word current_request);

static void	MR_copy_saved_regs_to_regs(void);
static void	MR_copy_regs_to_saved_regs(void);

#endif

#define	MR_port_is_final(port)	(port == MR_PORT_EXIT || port == MR_PORT_FAIL)

/*
** This function is called from compiled code whenever an event to be traced
** occurs.
*/

void
MR_trace(const Word *layout_word, MR_trace_port port,
	int seqno, int depth, const char *path)
{
	const MR_stack_layout_entry	*layout;
	MR_trace_interact		interact;

	layout = (const MR_stack_layout_entry *) layout_word;

	MR_trace_event_number++;
	switch (MR_trace_cmd) {
		case MR_CMD_NEXT:
			MR_trace_event(MR_INTERACT, layout,
				port, seqno, depth, path);
			break;

		case MR_CMD_JUMP:
			if (MR_trace_seqno == seqno && MR_port_is_final(port))
			{
				interact = MR_INTERACT;
			} else {
				interact = MR_NO_INTERACT;
			}

			MR_trace_event(interact, layout,
				port, seqno, depth, path);

			break;

		case MR_CMD_SKIP:
			if (MR_trace_seqno == seqno && MR_port_is_final(port))
			{
				MR_trace_event(MR_INTERACT, layout,
					port, seqno, depth, path);
			}

			break;

		case MR_CMD_CONT:
			break;

		case MR_CMD_DUMP:
			MR_trace_event(MR_NO_INTERACT, layout,
				port, seqno, depth, path);
			break;

		default:
			fatal_error("MR_trace called with inappropriate port");
			break;
	}
}

static void
MR_trace_event(MR_trace_interact interact,
	const MR_stack_layout_entry *layout,
	MR_trace_port port, int seqno, int depth, const char *path)
{
#ifdef MR_USE_DEBUGGER
	MR_copy_regs_to_saved_regs();
	MR_debugger_step(interact, layout, port, seqno, depth, path);
	MR_copy_saved_regs_to_regs();
#else
	MR_trace_display_user(interact, layout, port, seqno, depth, path);
#endif
}

#ifdef MR_USE_DEBUGGER

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>

#if 0
This pseudocode should go in the debugger process:

#define SOCKET_PATH "/var/tmp/" 	/* +5 for pid = 14 chars */
#define SOCKET_PERM S_IRWXU		/* rwx for user only */

sprintf(Name, "%s%05d", SOCKET_PATH, getpid());

socket(unix, stream, Sock),
bind(sock, Name, Socket_file),
if (do_it_manually) {
	printf("user: you must do
			setenv MERCURY_INET_DEBUGGER_SOCKET Name
		and then run the program");
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
MR_trace_init(void)
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
MR_trace_end(void)
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

static void
MR_debugger_step(MR_trace_interact interact,
	const MR_stack_layout_entry *layout,
	MR_trace_port port, int seqno, int depth, const char *path)
{
	static bool searching = FALSE;
	static Word search_data;
	Word debugger_request;
	Integer debugger_request_type;
	Word var_list;

do_search:
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
				goto do_search;
			       			      		
			case MR_REQUEST_CURRENT:
				if (MR_debug_socket) {
					fprintf(stderr, "\nMercury runtime: "
						"REQUEST_CURRENT\n");
				}
				var_list = MR_trace_make_var_list(port, layout);
				MR_output_current(layout, port, seqno, depth,
					var_list, path, debugger_request);
				break;
				
			case MR_REQUEST_NO_TRACE:
				MR_trace_cmd = MR_CMD_CONT;
				return;

			default:
				fatal_error("unexpected request read from "
					"debugger socket");
		}
	}
}

static void
MR_output_current(const MR_stack_layout_entry *layout,
	MR_trace_port port, int seqno, int depth,
	Word var_list,
	const char *path, Word current_request)
{
	MR_DI_output_current(
		MR_trace_event_number,
		seqno,
		depth,
		port,
		layout->MR_sle_def_module,
		layout->MR_sle_name,
		layout->MR_sle_arity,
		layout->MR_sle_mode,
		layout->MR_sle_detism,
		var_list,
		(String) (Word) path,
		current_request,
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
MR_found_match(const MR_stack_layout_entry *layout,
	MR_trace_port port, int seqno, int depth,
	/* XXX live vars */
	const char *path, Word search_data)
{
	bool result;

	/* XXX get live vars from registers */
	Word arguments = /* XXX FIXME!!! */ 0; 
	result = MR_DI_found_match(
		MR_trace_event_number,
		seqno,
		depth,
		port,
		layout->MR_sle_def_module,
		layout->MR_sle_name,
		layout->MR_sle_arity,
		layout->MR_sle_mode,
		layout->MR_sle_detism,
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

#else /* !MR_USE_DEBUGGER */

void MR_trace_init(void) {}
void MR_trace_end(void) {}

#endif /* MR_USE_DEBUGGER */

static void
MR_trace_display_user(MR_trace_interact interact,
	const MR_stack_layout_entry *layout,
	MR_trace_port port, int seqno, int depth, const char *path)
{
	int	i;

	fflush(stdout);
	fprintf(stderr, "%8d: %6d %2d ", MR_trace_event_number, seqno, depth);

	for (i = 0; i < depth; i++) {
		putc(' ', stderr);
	}

	switch (port) {
		case MR_PORT_CALL:
			fprintf(stderr, "CALL ");
			break;

		case MR_PORT_EXIT:
			fprintf(stderr, "EXIT ");
			break;

		case MR_PORT_FAIL:
			fprintf(stderr, "FAIL ");
			break;

		case MR_PORT_THEN:
			fprintf(stderr, "THEN ");
			break;

		case MR_PORT_ELSE:
			fprintf(stderr, "ELSE ");
			break;

		case MR_PORT_DISJ:
			fprintf(stderr, "DISJ ");
			break;

		case MR_PORT_SWITCH:
			fprintf(stderr, "SWTC ");
			break;

		default:
			fatal_error("MR_trace_display_user called "
					"with bad port");
	}

	switch ((int) layout->MR_sle_detism) {
		case MR_DETISM_DET:
			fprintf(stderr, "DET   ");
			break;

		case MR_DETISM_SEMI:
			fprintf(stderr, "SEMI  ");
			break;

		case MR_DETISM_NON:
			fprintf(stderr, "NON   ");
			break;

		case MR_DETISM_MULTI:
			fprintf(stderr, "MUL   ");
			break;

		case MR_DETISM_ERRONEOUS:
			fprintf(stderr, "ERR   ");
			break;

		case MR_DETISM_FAILURE:
			fprintf(stderr, "FAIL  ");
			break;

		case MR_DETISM_CCNON:
			fprintf(stderr, "CCNON ");
			break;

		case MR_DETISM_CCMULTI:
			fprintf(stderr, "CCMUL ");
			break;
		
		default:
			fprintf(stderr, "mercury_trace.c: ???  ");
			break;
	}

	/*
	** The following should be a full identification of the procedure
	** provided (a) there was no intermodule optimization and (b) we are
	** not interested in tracing compiler-generated procedures.
	*/

	fprintf(stderr, "%s:%s/%ld-%ld %s\n",
		layout->MR_sle_def_module,
		layout->MR_sle_name,
		(long) layout->MR_sle_arity,
		(long) layout->MR_sle_mode,
		path);

	while (interact == MR_INTERACT) {
		fprintf(stderr, "mtrace> ");

		switch (MR_trace_get_cmd()) {
			case 'n':
			case '\n':
				MR_trace_cmd = MR_CMD_NEXT;
				break;

			case 'c':
				MR_trace_cmd = MR_CMD_CONT;
				break;

			case 'd':
				MR_trace_cmd = MR_CMD_DUMP;
				break;

			case 'j':
				if (MR_port_is_final(port)) {
					fprintf(stderr, "mtrace: cannot jump"
							" from this port\n");
					continue;
				} else {
					MR_trace_cmd = MR_CMD_JUMP;
					MR_trace_seqno = seqno;
				}

				break;

			case 'p':
				if (port == MR_PORT_CALL) {
					MR_trace_browse((int)
						layout->MR_sle_in_arg_count,
						&layout->MR_sle_in_arg_info);
				} else if (port == MR_PORT_EXIT) {
					MR_trace_browse((int)
						layout->MR_sle_out_arg_count,
						&layout->MR_sle_out_arg_info);
				} else {
					fprintf(stderr, "mtrace: cannot print"
							" from this port\n");
				}

				continue;

			case 's':
				if (MR_port_is_final(port)) {
					fprintf(stderr, "mtrace: cannot skip"
							" from this port\n");
					continue;
				} else {
					MR_trace_cmd = MR_CMD_SKIP;
					MR_trace_seqno = seqno;
				}

				break;

			case EOF:
			case 'a':
				fprintf(stderr, "mtrace: are you sure"
						" you want to abort? ");

				if (MR_trace_get_cmd() == 'y') {
					fatal_error("aborting the execution "
						"on user request");
				}
				continue;

			default:
				MR_trace_help();
				continue;
		}

		interact = MR_NO_INTERACT;
	}
}

static Word	MR_saved_regs[MAX_FAKE_REG];

static void
MR_copy_regs_to_saved_regs(void)
{
	/*
	** In the process of browsing, we call Mercury code,
	** which may clobber the contents of the registers,
	** both the control registers and the general purpose registers.
	** We must therefore save and restore these.
	**
	** XXX This is very inefficient!
	**
	** Some are in real machine registers; others in the fake_reg array.
	** We need to copy them all to the fake_reg array, because the
	** calling convention for calling Mercury functions exported to C
	** assumes that they will be in the fake_reg array.
	*/

	int i;

	restore_transient_registers();
	save_registers();
	for (i = 0; i < MAX_FAKE_REG; i++) {
		MR_saved_regs[i] = fake_reg[i];
	}
}

static void
MR_copy_saved_regs_to_regs(void)
{
	int i;

	for (i = 0; i < MAX_FAKE_REG; i++) {
		fake_reg[i] = MR_saved_regs[i];
	}
	restore_registers();
	save_transient_registers();
}

static Word
MR_trace_make_var_list(MR_trace_port port, const MR_stack_layout_entry *layout)
{
	int 				var_count;
	const MR_stack_layout_vars 	*vars;
	int				i;
	const char			*name;

	Word				univ_list;
	MR_stack_layout_var*		var;
	Word				univ, value;
	MR_Live_Type			live_type;
	Word				type_info;

	restore_transient_registers();

	if (port == MR_PORT_CALL) {
		var_count = layout->MR_sle_in_arg_count;
		vars = &layout->MR_sle_in_arg_info;
	} else if (port == MR_PORT_EXIT) {
		var_count = layout->MR_sle_out_arg_count;
		vars = &layout->MR_sle_out_arg_info;
	} else {
		return list_empty();
	}

	/* build up the live variable list, starting from the end */
	univ_list = list_empty();
	for (i = var_count - 1; i >= 0; i--) {
		/*
		** Look up the name, the type and value
		** (XXX we don't include the name or the inst
		** in the list that we return)
		*/
		if (vars->MR_slvs_names != NULL &&
				vars->MR_slvs_names[i] != NULL)
		{
			name = vars->MR_slvs_names[i];
		} else {
			name = "";
		}
		var = &vars->MR_slvs_pairs[i];
		if (!MR_trace_get_type_and_value(var, &type_info, &value)) {
			continue;
		}

		/* create a term of type `univ' to hold the type & value */
		incr_hp(univ, 2);
		field(mktag(0), univ, UNIV_OFFSET_FOR_TYPEINFO) = type_info;
		field(mktag(0), univ, UNIV_OFFSET_FOR_DATA) = value;
		
		univ_list = list_cons(univ, univ_list);
	}

	save_transient_registers();

	return univ_list;
}

static void
MR_trace_browse(int var_count, const MR_stack_layout_vars *vars)
{
	int	i;
	char	*name;

	if (var_count == 0) {
		printf("mtrace: no live variables\n");
		return;
	}

	MR_copy_regs_to_saved_regs();

	for (i = 0; i < var_count; i++) {
		if (vars->MR_slvs_names != NULL &&
				vars->MR_slvs_names[i] != NULL)
		{
			name = vars->MR_slvs_names[i];
		} else {
			name = NULL;
		}

		MR_trace_browse_var(name, &vars->MR_slvs_pairs[i]);
	}

	MR_copy_saved_regs_to_regs();
}

/* if you want to debug this code, you may want to set this var to TRUE */
static	bool	MR_trace_print_locn = FALSE;

static Word
MR_trace_lookup_live_lval(MR_Live_Lval locn, bool *succeeded)
{
	int	locn_num;
	Word	value;

	*succeeded = FALSE;
	value = 0;

	locn_num = (int) MR_LIVE_LVAL_NUMBER(locn);
	switch (MR_LIVE_LVAL_TYPE(locn)) {
		case MR_LVAL_TYPE_R:
			if (MR_trace_print_locn)
				printf("r%d", locn_num);
			value = saved_reg(MR_saved_regs, locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_F:
			if (MR_trace_print_locn)
				printf("f%d", locn_num);
			break;

		case MR_LVAL_TYPE_STACKVAR:
			if (MR_trace_print_locn)
				printf("stackvar%d", locn_num);
			/* XXX BUG! detstackvar() will give wrong results
			   because MR_sp is not valid */
			value = detstackvar(locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_FRAMEVAR:
			if (MR_trace_print_locn)
				printf("framevar%d", locn_num);
			/* XXX BUG! detstackvar() will give wrong results
			   because MR_currfr is not valid */
			value = framevar(locn_num);
			*succeeded = TRUE;
			break;

		case MR_LVAL_TYPE_SUCCIP:
			if (MR_trace_print_locn)
				printf("succip");
			break;

		case MR_LVAL_TYPE_MAXFR:
			if (MR_trace_print_locn)
				printf("maxfr");
			break;

		case MR_LVAL_TYPE_CURFR:
			if (MR_trace_print_locn)
				printf("curfr");
			break;

		case MR_LVAL_TYPE_HP:
			if (MR_trace_print_locn)
				printf("hp");
			break;

		case MR_LVAL_TYPE_SP:
			if (MR_trace_print_locn)
				printf("sp");
			break;

		case MR_LVAL_TYPE_UNKNOWN:
			if (MR_trace_print_locn)
				printf("unknown");
			break;

		default:
			if (MR_trace_print_locn)
				printf("DEFAULT");
			break;
	}

	return value;
}

static bool
MR_trace_get_type_and_value(const MR_stack_layout_var *var,
	Word *type_info, Word *value)
{
	bool succeeded;

	if (MR_LIVE_TYPE_IS_VAR(var->MR_slv_live_type)) {
		*type_info = MR_LIVE_TYPE_GET_VAR_TYPE(var->MR_slv_live_type);
	} else {
		return FALSE;
	}
	*value = MR_trace_lookup_live_lval(var->MR_slv_locn, &succeeded);
	return succeeded;
}

static void
MR_trace_browse_var(char *name, const MR_stack_layout_var *var)
{
	Word			value, type_info;
	bool			print_value;

	/* The initial blanks are to visually separate */
	/* the variable names from the prompt. */

	if (name != NULL) {
		printf("%10s%-21s\t", "", name);
	} else {
		printf("%10s%-21s\t", "", "anonymous variable");
	}

	if (MR_trace_get_type_and_value(var, &type_info, &value)) {
		printf("\t");

		/*
		** XXX It would be nice if we could call an exported C
		** function version of the browser predicate, and thus
		** avoid going through call_engine, but for some unknown
		** reason, that seemed to cause the Mercury code in the
		** browser to clobber part of the C stack.
		** Probably that was due to a bug which has since been
		** fixed, so we should change the code below back again...
		*/
		r1 = type_info;
		r2 = value;
		call_engine(MR_library_trace_browser);
	}

	printf("\n");
}

static int
MR_trace_get_cmd(void)
{
	int	cmd;
	int	c;

	cmd = getchar();	/* read the trace command */

	/* skip the rest of the line */
	c = cmd;
	while (c != EOF && c != '\n')
		c = getchar();

	return cmd;
}

static void
MR_trace_help(void)
{
	fprintf(stderr, "valid commands are:\n"
			" a: abort the current execution.\n"
			" c: continue to end, not printing the trace.\n"
			" d: continue to end, printing the trace.\n"
			" n: go to the next trace event.\n"
			" s: skip the current call, not printing trace.\n"
			" j: jump to end of current call, printing trace.\n"
			" p: print the variables live at this point.\n");
}
