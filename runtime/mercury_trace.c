/*
** Copyright (C) 1997 The University of Melbourne.
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
** Main author: Erwan Jahier.
** Significant adaptations by Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
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
** will set MR_trace_call_depth to its own remembered depth value. These
** are the only ways in which MR_trace_call_seqno is modified.
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

static	int			MR_trace_event_number = 0;

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
	MR_CMD_ABORT,	/* a: abort the current execution		  */
	MR_CMD_CONT,	/* c: continue to end, not printing the trace	  */
	MR_CMD_DUMP,	/* d: continue to end, printing the trace	  */
	MR_CMD_NEXT,	/* n: go to the next trace event		  */
	MR_CMD_SKIP,	/* s: skip the current call, not printing trace	  */
	MR_CMD_JUMP	/* j: jump to end of current call, printing trace */
} MR_trace_cmd_type;

static	MR_trace_cmd_type	MR_trace_cmd = MR_CMD_NEXT;
static	int			MR_trace_seqno = 0;

void MR_trace_display(MR_trace_port port, MR_trace_code_model model, int seqno,
	int depth, const char *modulename, const char *predname,
	int arity, int modenum, const char *path);
void MR_trace_interaction(MR_trace_port port, int seqno);
void MR_trace_help(void);

#define	port_is_final(port)	(port == MR_PORT_EXIT || port == MR_PORT_FAIL)

/*
** This function is called from compiled code whenever an event to be traced
** occurs.
*/

void
MR_trace(MR_trace_port port, MR_trace_code_model model, int seqno, int depth,
	 const char *modulename, const char *predname, int arity, int modenum,
	 const char *path)
{
	MR_trace_event_number++;
	switch (MR_trace_cmd) {
		case MR_CMD_NEXT:
			MR_trace_display(port, model, seqno, depth, modulename,
				predname, arity, modenum, path);
			MR_trace_interaction(port, seqno);
			break;

		case MR_CMD_JUMP:
			MR_trace_display(port, model, seqno, depth,
				modulename, predname, arity,
				modenum, path);

			if (MR_trace_seqno == seqno && port_is_final(port)) {
				MR_trace_interaction(port, seqno);
			}

			break;

		case MR_CMD_SKIP:
			if (MR_trace_seqno == seqno && port_is_final(port)) {
				MR_trace_display(port, model, seqno, depth,
					modulename, predname, arity,
					modenum, path);

				MR_trace_interaction(port, seqno);
			}

			break;

		case MR_CMD_CONT:
			break;

		case MR_CMD_DUMP:
			MR_trace_display(port, model, seqno, depth,
				modulename, predname, arity,
				modenum, path);
			break;

		case MR_CMD_ABORT:
			fatal_error("aborting the execution on user request");
			break;

		default:
			fatal_error("MR_trace called with inappropriate port");
			break;
	}
}

void MR_trace_display(MR_trace_port port, MR_trace_code_model model, int seqno,
	int depth, const char *modulename, const char *predname,
	int arity, int modenum, const char *path)
{
	int	i;

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
			fatal_error("MR_trace_display called with inappropriate port");
	}

	switch (model) {
		case MR_MODEL_DET:
			fprintf(stderr, "DET  ");
			break;

		case MR_MODEL_SEMI:
			fprintf(stderr, "SEMI ");
			break;

		case MR_MODEL_NON:
			fprintf(stderr, "NON  ");
			break;
	}

	fprintf(stderr, "%s:%s/%d-%d %s\n",
		modulename, predname, arity, modenum, path);
}

void
MR_trace_interaction(MR_trace_port port, int seqno)
{
	int	cmd;
	int	c;

	fprintf(stderr, "trace command [a|c|d|j|n|s] ");

	cmd = getchar();	/* read the trace command */

	/* skip the rest of the line */
	c = cmd;
	while (c != EOF && c != '\n')
		c = getchar();

	switch (cmd) {
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
			if (port_is_final(port)) {
				fprintf(stderr, "cannot jump from this port\n");
				MR_trace_interaction(port, seqno);
			} else {
				MR_trace_cmd = MR_CMD_JUMP;
				MR_trace_seqno = seqno;
			}

			break;

		case 's':
			if (port_is_final(port)) {
				fprintf(stderr, "cannot skip from this port\n");
				MR_trace_interaction(port, seqno);
			} else {
				MR_trace_cmd = MR_CMD_SKIP;
				MR_trace_seqno = seqno;
			}

			break;

		case 'a':
			MR_trace_cmd = MR_CMD_ABORT;
			break;

		default:
			MR_trace_help();
			MR_trace_interaction(port, seqno);
	}
}

void
MR_trace_help(void)
{
	fprintf(stderr, "valid commands are:\n");
	fprintf(stderr, " a: abort the current execution.\n");
	fprintf(stderr, " c: continue to end, not printing the trace.\n");
	fprintf(stderr, " d: continue to end, printing the trace.\n");
	fprintf(stderr, " n: go to the next trace event.\n");
	fprintf(stderr, " s: skip the current call, not printing trace.\n");
	fprintf(stderr, " j: jump to end of current call, printing trace.\n");
}
