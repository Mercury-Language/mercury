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

typedef	enum {
	MR_INTERACT,
	MR_NO_INTERACT
} MR_trace_interact;

void	MR_trace_display(MR_trace_interact interact,
		const MR_stack_layout_entry *layout,
		MR_trace_port port, int seqno, int depth, const char *path);
void	MR_trace_browse(int var_count, const MR_stack_layout_vars *var_info);
void	MR_trace_browse_var(char *name, const MR_stack_layout_var *var);
int	MR_trace_get_cmd(void);
void	MR_trace_help(void);

#define	port_is_final(port)	(port == MR_PORT_EXIT || port == MR_PORT_FAIL)

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
			MR_trace_display(MR_INTERACT, layout,
				port, seqno, depth, path);
			break;

		case MR_CMD_JUMP:
			if (MR_trace_seqno == seqno && port_is_final(port)) {
				interact = MR_INTERACT;
			} else {
				interact = MR_NO_INTERACT;
			}

			MR_trace_display(interact, layout,
				port, seqno, depth, path);

			break;

		case MR_CMD_SKIP:
			if (MR_trace_seqno == seqno && port_is_final(port)) {
				MR_trace_display(MR_INTERACT, layout,
					port, seqno, depth, path);
			}

			break;

		case MR_CMD_CONT:
			break;

		case MR_CMD_DUMP:
			MR_trace_display(MR_NO_INTERACT, layout,
				port, seqno, depth, path);
			break;

		case MR_CMD_ABORT:
			fatal_error("aborting the execution on user request");
			break;

		default:
			fatal_error("MR_trace called with inappropriate port");
			break;
	}
}

void MR_trace_display(MR_trace_interact interact,
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
			fatal_error("MR_trace_display called with bad port");
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
			fprintf(stderr, "???  ");
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
				if (port_is_final(port)) {
					fprintf(stderr, "mtrace: cannot jump");
					fprintf(stderr, " from this port\n");
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
					fprintf(stderr, "mtrace: cannot print");
					fprintf(stderr, " from this port\n");
				}

				continue;

			case 's':
				if (port_is_final(port)) {
					fprintf(stderr, "mtrace: cannot skip");
					fprintf(stderr, " from this port\n");
					continue;
				} else {
					MR_trace_cmd = MR_CMD_SKIP;
					MR_trace_seqno = seqno;
				}

				break;

			case EOF:
			case 'a':
				fprintf(stderr, "mtrace: are you sure");
				fprintf(stderr, " you want to abort? ");

				if (MR_trace_get_cmd() == 'y') {
					MR_trace_cmd = MR_CMD_ABORT;
					break;
				} else {
					continue;
				}

			default:
				MR_trace_help();
				continue;
		}

		interact = MR_NO_INTERACT;
	}
}

Word	saved_regs[MAX_FAKE_REG];

void
MR_trace_browse(int var_count, const MR_stack_layout_vars *vars)
{
	int	i;
	char	*name;

	if (var_count == 0) {
		printf("mtrace: no live variables\n");
		return;
	}

	/*
	** In the process of browsing, we call Mercury code,
	** which may clobber the contents of the control registers
	** and the contents of the gp registers up to r<maxreg>.
	** We must therefore save and restore these.
	** XXX The value of maxreg ought to be given to us by the compiler
	** through a parameter to MR_trace; for the time being, we use 10.
	*/

	save_regs_to_mem(saved_regs);
	for (i = 0; i < var_count; i++) {
		if (vars->MR_slvs_names != NULL &&
				vars->MR_slvs_names[i] != NULL)
			name = vars->MR_slvs_names[i];
		else
			name = NULL;

		MR_trace_browse_var(name, &vars->MR_slvs_pairs[i]);
	}

	restore_regs_from_mem(saved_regs);
}

/* if you want to debug this code, you may want to set this var to 1 */
static	int	MR_trace_print_locn = 0;

void
MR_trace_browse_var(char *name, const MR_stack_layout_var *var)
{
	Integer			locn;
	Word			value;
	int			print_value;
	int			locn_num;

	/* The initial blanks are to visually separate */
	/* the variable names from the prompt. */

	if (name != NULL)
		printf("%10s%-21s\t", "", name);
	else
		printf("%10s%-21s\t", "", "anonymous variable");

	value = 0; /* not used; this shuts up a compiler warning */
	print_value = FALSE;

	locn = var->MR_slv_locn;
	locn_num = (int) MR_LIVE_LVAL_NUMBER(locn);
	switch (MR_LIVE_LVAL_TYPE(locn)) {
		case MR_LVAL_TYPE_R:
			if (MR_trace_print_locn)
				printf("r%d", locn_num);
			value = saved_reg(saved_regs, locn_num);
			print_value = TRUE;
			break;

		case MR_LVAL_TYPE_F:
			if (MR_trace_print_locn)
				printf("f%d", locn_num);
			break;

		case MR_LVAL_TYPE_STACKVAR:
			if (MR_trace_print_locn)
				printf("stackvar%d", locn_num);
			value = detstackvar(locn_num);
			print_value = TRUE;
			break;

		case MR_LVAL_TYPE_FRAMEVAR:
			if (MR_trace_print_locn)
				printf("framevar%d", locn_num);
			value = framevar(locn_num);
			print_value = TRUE;
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

	if (print_value) {
		printf("\t");

		/*
		** XXX It would be nice if we could call an exported C
		** function version of the browser predicate, and thus
		** avoid going through call_engine, but that causes the
		** Mercury code in the browser to clobber part of the C stack.
		*/

		r1 = (Word) var->MR_slv_shape->MR_sls_type;
		r2 = (Word) value;
		call_engine(MR_library_trace_browser);
	}

	printf("\n");
}

int
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
	fprintf(stderr, " p: print the variables live at this point.\n");
}
