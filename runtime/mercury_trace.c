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
#include "mercury_misc.h"
#include <stdio.h>
#include <ctype.h>

/*
** Do we want to use the debugger within this process, or do want to use
** the Opium-style trace analyzer debugger implemented by an external process.
** This variable is set in mercury_wrapper.c and never modified afterwards.
*/

MR_trace_type	MR_trace_handler = MR_TRACE_INTERNAL;

/*
** Compiler generated tracing code will check whether MR_trace_enabled is true,
** before calling MR_trace. For now, and until we implement interface tracing,
** MR_trace_enabled should keep the same value throughout the execution of
** the entire program after being set in mercury_wrapper.c. There is one
** exception to this: the Mercury routines called as part of the functionality
** of the tracer itself (e.g. the term browser) should always be executed
** with MR_trace_enabled set to FALSE.
*/

bool	MR_trace_enabled = FALSE;

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
** MR_trace_event_number is a simple counter of events. This is used in
** two places: here, for display to the user and for skipping a given number
** of events, and when printing an abort message, so that the programmer
** can zero in on the source of the problem more quickly.
*/

int	MR_trace_event_number = 0;

/*
** MR_trace_cmd and MR_trace_seqno are globals variables that we use
** to manage an interface between the tracer and the user.
**
** MR_trace_cmd says what mode the tracer is in, i.e. what the last
** trace command was. The details of the semantics are given by the
** code of MR_trace().
**
** MR_trace_seqno is meaningful only when MR_trace_cmd is MR_SKIP or MR_JUMP.
** In those cases, it holds the sequence number of the call at whose exit
** control should be given back to the user.
*/

typedef enum {
	MR_CMD_GOTO,		/* stop at an event with a given number    */
	MR_CMD_FINISH,		/* stop when exiting/failing out of a proc */
	MR_CMD_RESUME_FORWARD,	/* stop at the next non-final port         */
	MR_CMD_TO_END		/* do not stop until the end of execution  */
} MR_trace_cmd_type;

#define	MR_NAME_LEN		80
#define	MR_MAX_SPY_POINTS	100
#define	MR_LOG10_MAX_SPY_POINTS	20

typedef struct {
	bool	enabled;
	char	module_name[MR_NAME_LEN];
	char	pred_name[MR_NAME_LEN];
} MR_spy_point;

MR_spy_point	MR_spy_points[MR_MAX_SPY_POINTS];
int		MR_next_spy_point = 0;

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

static	MR_trace_cmd_type	MR_trace_cmd = MR_CMD_GOTO;
static	int			MR_trace_stop_seqno = 0;
static	int			MR_trace_stop_event = 0;
static	bool			MR_trace_print_intermediate = FALSE;

typedef	enum {
	MR_INTERACT,
	MR_NO_INTERACT
} MR_trace_interact;

static void	MR_trace_event(MR_trace_interact interact,
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path, int max_r_num);
static void	MR_copy_saved_regs_to_regs(int max_mr_num);
static void	MR_copy_regs_to_saved_regs(int max_mr_num);
static void	MR_trace_display_user(MR_trace_interact interact,
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);
static void	MR_trace_browse(int var_count,
			const MR_Stack_Layout_Vars *var_info);
static void	MR_trace_browse_var(const char *name,
			const MR_Stack_Layout_Var *var, Word *type_params);
static int	MR_trace_skip_spaces(int c);
static void	MR_trace_discard_to_eol(int c);
static int	MR_trace_get_word(int *c, char word[], int len);
static void	MR_trace_help(void);

static void	MR_add_spy_point(void);
static void	MR_list_spy_points(void);
static void	MR_change_spy_point_status(bool status);
static bool	MR_event_matches_spy_point(const MR_Stack_Layout_Label *layout);

static Word	MR_trace_make_var_list(MR_trace_port port,
			const MR_Stack_Layout_Label *layout);
static Word	MR_trace_lookup_live_lval(MR_Live_Lval locn, bool *succeeded);
static bool	MR_trace_get_type_and_value(const MR_Stack_Layout_Var *var,
			Word *type_params, Word *type_info, Word *value);

/*
We could use
	if (MR_use_debugger) { ...
instead of #if; this would be better in the long run.
This would require changing mercury_wrapper.c to
check for an additional flag in the MERCURY_OPTIONS
environment variable and set MR_use_debugger accordingly.
*/
#ifdef MR_USE_EXTERNAL_DEBUGGER

static MercuryFile MR_debugger_socket_in;
static MercuryFile MR_debugger_socket_out;

static void	MR_send_message_to_socket(const char *message);
static void	MR_read_request_from_socket(
			Word *debugger_request_ptr, 
			Integer *debugger_request_type_ptr);
	
static void	MR_debugger_step(MR_trace_interact interact,
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);
static bool	MR_found_match(const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			/* XXX registers */
			const char *path, Word search_data);
static void	MR_output_current(const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			Word var_list,
			const char *path, Word current_request);

#endif

#define	MR_port_is_final(port)	(port == MR_PORT_EXIT || port == MR_PORT_FAIL)

/*
** This function is called from compiled code whenever an event to be traced
** occurs.
*/

void
MR_trace(const MR_Stack_Layout_Label *layout, MR_trace_port port,
	int seqno, int depth, const char *path, int max_r_num)
{
	MR_trace_event_number++;
	switch (MR_trace_cmd) {
		case MR_CMD_FINISH:
			if (MR_trace_stop_seqno == seqno
			&& MR_port_is_final(port)) {
				MR_trace_event(MR_INTERACT, layout,
					port, seqno, depth, path, max_r_num);

			} else if (MR_trace_print_intermediate) {
				MR_trace_event(MR_NO_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		case MR_CMD_GOTO:
			if (MR_trace_event_number >= MR_trace_stop_event
			|| MR_event_matches_spy_point(layout)) {
				MR_trace_event(MR_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			} else if (MR_trace_print_intermediate) {
				MR_trace_event(MR_NO_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		case MR_CMD_RESUME_FORWARD:
			if (MR_port_is_final(port)) {
				MR_trace_event(MR_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			} else if (MR_trace_print_intermediate) {
				MR_trace_event(MR_NO_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		case MR_CMD_TO_END:
			if (MR_event_matches_spy_point(layout)) {
				MR_trace_event(MR_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			} else if (MR_trace_print_intermediate) {
				MR_trace_event(MR_NO_INTERACT, layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		default:
			fatal_error("invalid cmd in MR_trace");
			break;
	}
}

static bool
MR_event_matches_spy_point(const MR_Stack_Layout_Label *layout)
{
	const	MR_Stack_Layout_Entry	*entry;
	int				i;

	entry = layout->MR_sll_entry;

	for (i = 0; i < MR_next_spy_point; i++) {
		if (MR_spy_points[i].enabled
		&& streq(MR_spy_points[i].pred_name,
				entry->MR_sle_name)
		&& streq(MR_spy_points[i].module_name,
				entry->MR_sle_def_module)) {
			return TRUE;
		}
	}

	return FALSE;
}

static void
MR_trace_event(MR_trace_interact interact,
	const MR_Stack_Layout_Label *layout, MR_trace_port port,
	int seqno, int depth, const char *path, int max_r_num)
{
	int	max_mr_num;

	if (max_r_num + MR_NUM_SPECIAL_REG > MR_MAX_SPECIAL_REG_MR) {
		max_mr_num = max_r_num + MR_NUM_SPECIAL_REG;
	} else {
		max_mr_num = MR_MAX_SPECIAL_REG_MR;
	}

	MR_copy_regs_to_saved_regs(max_mr_num);
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_debugger == MR_TRACE_EXTERNAL) {
		MR_debugger_step(interact, layout, port, seqno, depth, path);
	} else {
		MR_trace_display_user(interact, layout, port, seqno, depth,
			path);
	}
#else
	/*
	** We should get here only if MR_trace_debugger == MR_TRACE_INTERNAL.
	** This is enforced by mercury_wrapper.c.
	*/

	MR_trace_display_user(interact, layout, port, seqno, depth, path);
#endif
	MR_copy_saved_regs_to_regs(max_mr_num);
}

static Word	MR_saved_regs[MAX_FAKE_REG];

static void
MR_copy_regs_to_saved_regs(int max_mr_num)
{
	/*
	** In the process of browsing, we call Mercury code,
	** which may clobber the contents of the virtual machine registers,
	** both control and general purpose, and both real and virtual
	** registers. We must therefore save and restore these.
	** We store them in the MR_saved_regs array.
	**
	** The call to MR_trace will clobber the transient registers
	** on architectures that have them. The compiler generated code
	** will therefore call save_transient_registers to save the transient
	** registers in the fake_reg array. We here restore them to the
	** real registers, save them with the other registers back in
	** fake_reg, and then copy all fake_reg entries to MR_saved_regs.
	**
	** If any code invoked by MR_trace is itself traced,
	** MR_saved_regs will be overwritten, leading to a crash later on.
	** This is one reason (but not the only one) why we turn off
	** tracing when we call back Mercury code from this file.
	*/

	int i;

	restore_transient_registers();
	save_registers();

	for (i = 0; i <= max_mr_num; i++) {
		MR_saved_regs[i] = fake_reg[i];
	}
}

static void
MR_copy_saved_regs_to_regs(int max_mr_num)
{
	/*
	** We execute the converse procedure to MR_copy_regs_to_saved_regs.
	** The save_transient_registers is there so that a call to the
	** restore_transient_registers macro after MR_trace will do the
	** right thing.
	*/

	int i;

	for (i = 0; i <= max_mr_num; i++) {
		fake_reg[i] = MR_saved_regs[i];
	}

	restore_registers();
	save_transient_registers();
}

void
MR_trace_report(FILE *fp)
{
	if (MR_trace_event_number > 0) {
		/*
		** This means that the executable was compiled with tracing,
		** which implies that the user wants trace info on abort.
		*/

		fprintf(fp, "Last trace event was event #%d.\n",
			MR_trace_event_number);
	}
}

void
MR_trace_report_raw(int fd)
{
	char	buf[80];	/* that ought to be more than long enough */

	if (MR_trace_event_number > 0) {
		/*
		** This means that the executable was compiled with tracing,
		** which implies that the user wants trace info on abort.
		*/

		sprintf(buf, "Last trace event was event #%d.\n",
			MR_trace_event_number);
		write(fd, buf, strlen(buf));
	}
}

void
MR_trace_init(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL)
		MR_trace_init_external();
#endif
}

void
MR_trace_end(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL)
		MR_trace_end_external();
#endif
}

#ifdef MR_USE_EXTERNAL_DEBUGGER

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
MR_trace_end_external(void)
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
	const MR_Stack_Layout_Label *layout,
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
				MR_trace_cmd = MR_CMD_TO_END;
				return;

			default:
				fatal_error("unexpected request read from "
					"debugger socket");
		}
	}
}

static void
MR_output_current(const MR_Stack_Layout_Label *layout,
	MR_trace_port port, int seqno, int depth,
	Word var_list,
	const char *path, Word current_request)
{
	MR_DI_output_current(
		MR_trace_event_number,
		seqno,
		depth,
		port,
		layout->MR_sll_entry->MR_sle_def_module,
		layout->MR_sll_entry->MR_sle_name,
		layout->MR_sll_entry->MR_sle_arity,
		layout->MR_sll_entry->MR_sle_mode,
		layout->MR_sll_entry->MR_sle_detism,
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
MR_found_match(const MR_Stack_Layout_Label *layout,
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
		layout->MR_sll_entry->MR_sle_def_module,
		layout->MR_sll_entry->MR_sle_name,
		layout->MR_sll_entry->MR_sle_arity,
		layout->MR_sll_entry->MR_sle_mode,
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

#endif /* MR_USE_EXTERNAL_DEBUGGER */

static void
MR_trace_display_user(MR_trace_interact interact,
	const MR_Stack_Layout_Label *layout,
	MR_trace_port port, int seqno, int depth, const char *path)
{
	int	i;
	int	c;
	int	count;
	bool	count_given;

	printf("%8d: %6d %2d ", MR_trace_event_number, seqno, depth);

	switch (port) {
		case MR_PORT_CALL:
			printf("CALL ");
			break;

		case MR_PORT_EXIT:
			printf("EXIT ");
			break;

		case MR_PORT_FAIL:
			printf("FAIL ");
			break;

		case MR_PORT_THEN:
			printf("THEN ");
			break;

		case MR_PORT_ELSE:
			printf("ELSE ");
			break;

		case MR_PORT_DISJ:
			printf("DISJ ");
			break;

		case MR_PORT_SWITCH:
			printf("SWTC ");
			break;

		case MR_PORT_PRAGMA_FIRST:
			printf("FRST ");
			break;

		case MR_PORT_PRAGMA_LATER:
			printf("LATR ");
			break;

		default:
			fatal_error("MR_trace_display_user called "
					"with bad port");
	}

	switch ((int) layout->MR_sll_entry->MR_sle_detism) {
		case MR_DETISM_DET:
			printf("DET   ");
			break;

		case MR_DETISM_SEMI:
			printf("SEMI  ");
			break;

		case MR_DETISM_NON:
			printf("NON   ");
			break;

		case MR_DETISM_MULTI:
			printf("MUL   ");
			break;

		case MR_DETISM_ERRONEOUS:
			printf("ERR   ");
			break;

		case MR_DETISM_FAILURE:
			printf("FAIL  ");
			break;

		case MR_DETISM_CCNON:
			printf("CCNON ");
			break;

		case MR_DETISM_CCMULTI:
			printf("CCMUL ");
			break;
		
		default:
			printf("BAD DETERMINISM\n");
			break;
	}

	/*
	** The following should be a full identification of the procedure
	** provided (a) there was no intermodule optimization and (b) we are
	** not interested in tracing compiler-generated procedures.
	*/

	printf("%s:%s/%ld-%ld %s\n",
		layout->MR_sll_entry->MR_sle_def_module,
		layout->MR_sll_entry->MR_sle_name,
		(long) layout->MR_sll_entry->MR_sle_arity,
		(long) layout->MR_sll_entry->MR_sle_mode,
		path);

	while (interact == MR_INTERACT) {
		printf("mtrace> ");

		count = 1;
		count_given = FALSE;
		MR_trace_print_intermediate = FALSE;

		c = MR_trace_skip_spaces(' ');
		if (isdigit(c)) {
			count_given = TRUE;
			count = c - '0';
			c = getchar();
			while (c != EOF && isdigit(c)) {
				count = (count * 10) + c - '0';
				c = getchar();
			}

			c = MR_trace_skip_spaces(c);
		}

		switch (c) {
			case 'S':
				MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 's':
			case '\n':
				MR_trace_cmd = MR_CMD_GOTO;
				MR_trace_stop_event =
					MR_trace_event_number + count;
				MR_trace_discard_to_eol(c);
				break;

			case 'G':
				MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 'g':
				if (! count_given) {
					MR_trace_discard_to_eol(c);
					printf("mtrace: no count given\n");
					continue;
				}

				MR_trace_cmd = MR_CMD_GOTO;
				MR_trace_stop_event = count;
				MR_trace_discard_to_eol(c);
				break;

			case 'F':
				MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 'f':
				if (MR_port_is_final(port)) {
					MR_trace_discard_to_eol(c);
					printf("mtrace: this port is "
						"already final\n");
					continue;
				} else {
					MR_trace_cmd = MR_CMD_FINISH;
					MR_trace_stop_seqno = seqno;
				}

				MR_trace_discard_to_eol(c);
				break;

			case 'C':
				MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 'c':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_trace_cmd = MR_CMD_TO_END;
				MR_trace_discard_to_eol(c);
				break;

			case 'p':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_trace_discard_to_eol(c);
				MR_trace_browse((int)
					layout->MR_sll_var_count,
					&layout->MR_sll_var_info);

				continue;

			case 'r':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_trace_cmd = MR_CMD_RESUME_FORWARD;
				MR_trace_discard_to_eol(c);
				break;

			case 'b':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_add_spy_point();
				continue;

			case '?':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_list_spy_points();
				continue;

			case '+':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_change_spy_point_status(TRUE);
				continue;

			case '-':
				if (count_given)
					printf("mtrace: count ignored\n");

				MR_change_spy_point_status(FALSE);
				continue;

			case 'a':
			case EOF:
				MR_trace_discard_to_eol(c);
				printf("mtrace: are you sure"
						" you want to abort? ");

				c = MR_trace_skip_spaces(' ');
				if (c == 'y' || c == EOF) {
					/*
					** We reset MR_trace_event_number
					** that fatal_error will not
					** print the last trace event number
					** (since in this case it is not
					** meaningful).
					*/

					MR_trace_event_number = 0;
					fatal_error("aborting the execution "
						"on user request");
				}

				MR_trace_discard_to_eol(c);
				continue;

			default:
				MR_trace_discard_to_eol(c);
				MR_trace_help();
				continue;
		}

		interact = MR_NO_INTERACT;
	}
}

static void
MR_add_spy_point(void)
{
	int	c;

	c = getchar();

	if (MR_next_spy_point >= MR_MAX_SPY_POINTS) {
		MR_trace_discard_to_eol(c);
		printf("mtrace: no room for more spy points\n");
		return;
	}

	if (MR_trace_get_word(&c, MR_spy_points[MR_next_spy_point].module_name,
			MR_NAME_LEN)
	&& MR_trace_get_word(&c, MR_spy_points[MR_next_spy_point].pred_name,
			MR_NAME_LEN)) {
		MR_trace_discard_to_eol(c);
		MR_spy_points[MR_next_spy_point].enabled = TRUE;
		MR_next_spy_point++;
	}
	else {
		printf("usage: \"b module_name pred_name\"\n");
	}
}

static void
MR_list_spy_points(void)
{
	int	i;

	for (i = 0; i < MR_next_spy_point; i++) {
		printf("%2d: %s %s:%s\n", i,
			MR_spy_points[i].enabled? "+": "-",
			MR_spy_points[i].module_name,
			MR_spy_points[i].pred_name);
	}

	MR_trace_discard_to_eol(getchar());
}

static void
MR_change_spy_point_status(bool status)
{
	char	buf[MR_LOG10_MAX_SPY_POINTS];
	int	c;
	int	i;

	c = getchar();

	if (MR_trace_get_word(&c, buf, MR_LOG10_MAX_SPY_POINTS)) {
		if (sscanf(buf, "%d", &i) == 1) {
			if (0 <= i && i < MR_next_spy_point) {
				MR_spy_points[i].enabled = status;
			} else {
				printf("spy point #%d does not exist\n", i);
			}
		} else if (strcmp(buf, "*") == 0) {
			for (i = 0; i < MR_next_spy_point; i++) {
				MR_spy_points[i].enabled = status;
			}
		} else {
			printf("garbled spy point number\n");
		}
	} else {
		printf("missing spy point number\n");
	}

	MR_trace_discard_to_eol(c);
}

static Word
MR_trace_make_var_list(MR_trace_port port, const MR_Stack_Layout_Label *layout)
{
	int 				var_count;
	const MR_Stack_Layout_Vars 	*vars;
	int				i;
	const char			*name;

	Word				univ_list;
	MR_Stack_Layout_Var*		var;
	Word				univ, value;
	MR_Live_Type			live_type;
	Word				type_info;

	restore_transient_registers();

	var_count = layout->MR_sll_var_count;
	vars = &layout->MR_sll_var_info;

	/* build up the live variable list, starting from the end */
	univ_list = list_empty();
	for (i = var_count - 1; i >= 0; i--) {
		/*
		** Look up the name, the type and value
		** (XXX we don't include the name or the inst
		** in the list that we return)
		*/

		name = MR_name_if_present(vars, i);
		var = &vars->MR_slvs_pairs[i];

		/*
		** XXX The printing of type_infos is buggy at the moment
		** due to the fake arity of mercury_builtin:typeinfo/1.
		**
		** XXX The printing of large data structures is painful
		** at the moment due to the lack of a true browser.
		**
		** "variables" representing the saved values of succip, hp etc,
		** which are the "variables" for which get_type_and_value
		** fails, are not of interest to the trace analyzer.
		*/

		if ((strncmp(name, "TypeInfo", 8) == 0)
		|| (strncmp(name, "ModuleInfo", 10) == 0)
		|| (strncmp(name, "HLDS", 4) == 0)
		|| !MR_trace_get_type_and_value(var, NULL, &type_info, &value))
		{
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
MR_trace_browse(int var_count, const MR_Stack_Layout_Vars *vars)
{
	Word	*type_params;
	bool	succeeded;
	int	i;

	if (var_count == 0) {
		printf("mtrace: no live variables\n");
		return;
	}

	type_params = checked_malloc((vars->MR_slvs_tvar_count + 1)
		* sizeof(Word));
	/* type_params should look like a typeinfo; type_params[0] is empty */
	for (i = 0; i < vars->MR_slvs_tvar_count; i++) {
		type_params[i+1] = MR_trace_lookup_live_lval(
			vars->MR_slvs_tvars[i], &succeeded);
		if (!succeeded) {
			fatal_error("missing type param in MR_trace_browse");
		}
	}

	for (i = 0; i < var_count; i++) {
		MR_trace_browse_var(MR_name_if_present(vars, i),
			&vars->MR_slvs_pairs[i], type_params);
	}

	free(type_params);
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

/* XXX fix this ref to the library */
extern	Word	*ML_create_type_info(Word *term_type_info,
			Word *arg_pseudo_type_info);

static bool
MR_trace_get_type_and_value(const MR_Stack_Layout_Var *var,
	Word *type_params, Word *type_info, Word *value)
{
	bool	succeeded;
	Word	*pseudo_type_info;
	int	i;

	if (!MR_LIVE_TYPE_IS_VAR(var->MR_slv_live_type)) {
		return FALSE;
	}

	pseudo_type_info = MR_LIVE_TYPE_GET_VAR_TYPE(var->MR_slv_live_type);
	*type_info = (Word) ML_create_type_info(type_params, pseudo_type_info);
	*value = MR_trace_lookup_live_lval(var->MR_slv_locn, &succeeded);
	return succeeded;
}

static void
MR_trace_browse_var(const char *name, const MR_Stack_Layout_Var *var,
	Word *type_params)
{
	Word	value, type_info;
	bool	print_value;
	int	i;

	/*
	** XXX The printing of type_infos is buggy at the moment
	** due to the fake arity of the type mercury_builtin:typeinfo/1.
	**
	** XXX The printing of large data structures is painful
	** at the moment due to the lack of a true browser.
	*/

	if ((strncmp(name, "TypeInfo", 8) == 0)
	|| (strncmp(name, "ModuleInfo", 10) == 0)
	|| (strncmp(name, "HLDS", 4) == 0))
		return;

	/* The initial blanks are to visually separate */
	/* the variable names from the prompt. */

	if (name != NULL) {
		printf("%10s%-21s\t", "", name);
	} else {
		printf("%10s%-21s\t", "", "anonymous variable");
	}

	fflush(stdout);

	/*
	** "variables" representing the saved values of succip, hp etc,
	** which are the "variables" for which get_type_and_value fails,
	** are not of interest to the user.
	*/

	if (MR_trace_get_type_and_value(var, type_params, &type_info, &value))
	{
		printf("\t");

		/*
		** XXX It would be nice if we could call an exported C
		** function version of the browser predicate, and thus
		** avoid going through call_engine, but for some unknown
		** reason, that seemed to cause the Mercury code in the
		** browser to clobber part of the C stack.
		**
		** Probably that was due to a bug which has since been
		** fixed, so we should change the code below back again...
		**
		** call_engine expects the transient registers to be
		** in fake_reg, others in their normal homes.
		** The code below works by placing r1, r2 and all other
		** transient registers both in their normal homes and
		** and in fake_reg as well.
		*/

		MR_trace_enabled = FALSE;
		for (i = 0; i < MR_MAX_SPECIAL_REG_MR; i++) {
			fake_reg[i] = MR_saved_regs[i];
		}
		restore_registers();
		r1 = type_info;
		r2 = value;
		save_transient_registers(); /* r1 or r2 may be transient */
		call_engine(MR_library_trace_browser);
		MR_trace_enabled = TRUE;
	}

	printf("\n");
}

static int
MR_trace_skip_spaces(int c)
{
	while (c != EOF && c != '\n' && isspace(c))
		c = getchar();

	return c;
}

static void
MR_trace_discard_to_eol(int c)
{
	while (c != EOF && c != '\n')
		c = getchar();
}

static int
MR_trace_get_word(int *cptr, char word[], int len)
{
	int	c;
	int	i;

	c = MR_trace_skip_spaces(*cptr);

	i = 0;
	while (c != EOF && (isalnum(c) || c == '_')) {
		if (i < len) {
			word[i++] = c;
		}

		c = getchar();
	}

	*cptr = c;

	if (i > 0) {
		word[i] = '\0';
		return TRUE;
	}

	return FALSE;
}

static void
MR_trace_help(void)
{
	printf("valid commands are:\n"
		"a, EOF:\t\t"
		"\tabort the current execution.\n"
		"b <module> <name>:"
		"\tset a breakpoint on the predicate\n"
		"\t\t\tor function <module>:<name>.\n"
		"c:\t\t"
		"\tcontinue to end of program, not printing the trace.\n"
		"C:\t\t"
		"\tcontinue to end of program, printing the trace.\n"
		"f:\t\t"
		"\tfinish this call, not printing the trace.\n"
		"F:\t\t"
		"\tfinish this call, printing the trace.\n"
		"<N> g:\t\t"
		"\tgo to event #N, not printing the trace.\n"
		"<N> G:\t\t"
		"\tgo to event #N, printing the trace.\n"
		"p:\t\t"
		"\tprint the variables live at this point.\n"
		"r:\t\t"
		"\tcontinue until forward execution is resumed.\n"
		"[<N>] [s]:\t"
		"\tskip N events, not printing the trace (default: N=1).\n"
		"[<N>] S:\t"
		"\tskip N events, printing the trace (default: N=1).\n"
		"?:\t\t"
		"\tlist all the breakpoints.\n"
		"+ <N>:\t\t"
		"\tenable breakpoint #N.\n"
		"- <N>:\t\t"
		"\tdisable breakpoint #N.\n"
	);
}
