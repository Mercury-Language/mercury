/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the code of the internal, in-process debugger.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_util.h"
#include <stdio.h>
#include <ctype.h>

#define	MR_NAME_LEN		80
#define	MR_MAX_SPY_POINTS	100
#define	MR_LOG10_MAX_SPY_POINTS	20

typedef struct {
	bool	enabled;
	char	module_name[MR_NAME_LEN];
	char	pred_name[MR_NAME_LEN];
} MR_spy_point;

static	MR_spy_point	MR_spy_points[MR_MAX_SPY_POINTS];
static	int		MR_next_spy_point = 0;

static	void	MR_trace_browse(int var_count,
			const MR_Stack_Layout_Vars *var_info);
static	void	MR_trace_browse_var(const char *name,
			const MR_Stack_Layout_Var *var, Word *type_params);

static	void	MR_add_spy_point(void);
static	void	MR_list_spy_points(void);
static	void	MR_change_spy_point_status(bool status);

static	int	MR_trace_skip_spaces(int c);
static	void	MR_trace_discard_to_eol(int c);
static	int	MR_trace_get_word(int *c, char word[], int len);
static	void	MR_trace_help(void);

static	void	MR_trace_print_port(MR_trace_port port);
static	void	MR_trace_print_detism(Word detism);

void
MR_trace_event_internal(MR_trace_cmd_info *cmd,
	const MR_Stack_Layout_Label *layout,
	MR_trace_port port, int seqno, int depth, const char *path)
{
	int	i;
	int	c;
	int	count;
	bool	count_given;
	Word	saved_seqno;
	Word	saved_depth;
	Word	saved_event;

	MR_trace_event_internal_report(layout, port, seqno, depth, path);

	/* these globals can be overwritten when we call Mercury code */
	saved_seqno = MR_trace_call_seqno;
	saved_depth = MR_trace_call_depth;
	saved_event = MR_trace_event_number;

	for (;;) {
		printf("mtrace> ");

		count = 1;
		count_given = FALSE;
		cmd->MR_trace_print_intermediate = FALSE;

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
				cmd->MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 's':
			case '\n':
				cmd->MR_trace_cmd = MR_CMD_GOTO;
				cmd->MR_trace_stop_event =
					MR_trace_event_number + count;
				MR_trace_discard_to_eol(c);
				break;

			case 'G':
				cmd->MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 'g':
				if (! count_given) {
					MR_trace_discard_to_eol(c);
					printf("mtrace: no count given\n");
					continue;
				}

				cmd->MR_trace_cmd = MR_CMD_GOTO;
				cmd->MR_trace_stop_event = count;
				MR_trace_discard_to_eol(c);
				break;

			case 'F':
				cmd->MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 'f':
				if (MR_port_is_final(port)) {
					MR_trace_discard_to_eol(c);
					printf("mtrace: this port is "
						"already final\n");
					continue;
				} else {
					cmd->MR_trace_cmd = MR_CMD_FINISH;
					cmd->MR_trace_stop_seqno = seqno;
				}

				MR_trace_discard_to_eol(c);
				break;

			case 'C':
				cmd->MR_trace_print_intermediate = TRUE;
				/* fall through */

			case 'c':
				if (count_given)
					printf("mtrace: count ignored\n");

				cmd->MR_trace_cmd = MR_CMD_TO_END;
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

				cmd->MR_trace_cmd = MR_CMD_RESUME_FORWARD;
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

		break;
	}

	MR_trace_call_seqno = saved_seqno;
	MR_trace_call_depth = saved_depth;
	MR_trace_event_number = saved_event;
}

static void
MR_trace_browse(int var_count, const MR_Stack_Layout_Vars *vars)
{
	Word	*type_params;
	bool	succeeded;
	int	count;
	int	i;

	if (var_count == 0) {
		printf("mtrace: no live variables\n");
		return;
	}

	if (vars->MR_slvs_tvars != NULL) {
		count = (int) (Integer) vars->MR_slvs_tvars[0];
		type_params = checked_malloc((count + 1) * sizeof(Word));

		/*
		** type_params should look like a typeinfo;
		** type_params[0] is empty and will not be referred to
		*/
		for (i = 1; i <= count; i++) {
			if (vars->MR_slvs_tvars[i] != 0) {
				type_params[i] = MR_trace_lookup_live_lval(
					vars->MR_slvs_tvars[i], &succeeded);
				if (!succeeded) {
					fatal_error("missing type param in MR_trace_browse");
				}
			}
		}
	} else {
		type_params = NULL;
	}

	for (i = 0; i < var_count; i++) {
		MR_trace_browse_var(MR_name_if_present(vars, i),
			&vars->MR_slvs_pairs[i], type_params);
	}

	free(type_params);
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
	** due to the fake arity of the type private_builtin:typeinfo/1.
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
		** call_engine() expects the transient registers to be
		** in fake_reg, others in their normal homes.
		** That is the case on entry to this function.
		** But r1 or r2 may be transient, so we need to save/restore
		** transient regs around the assignments to them.
		*/

		MR_trace_enabled = FALSE;
		restore_transient_registers();
		r1 = type_info;
		r2 = value;
		save_transient_registers();
		call_engine(MR_library_trace_browser);
		MR_trace_enabled = TRUE;
	}

	printf("\n");
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

bool
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

void
MR_trace_event_internal_report(const MR_Stack_Layout_Label *layout,
	MR_trace_port port, int seqno, int depth, const char *path)
{
	printf("%8ld: %6ld %2ld ",
		(long) MR_trace_event_number, (long) seqno, (long) depth);

	MR_trace_print_port(port);
	MR_trace_print_detism(layout->MR_sll_entry->MR_sle_detism);

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
}

static void
MR_trace_print_port(MR_trace_port port)
{
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
			fatal_error("MR_trace_event_internal called "
					"with bad port");
	}
}

static void
MR_trace_print_detism(Word detism)
{
	switch ((int) detism) {
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
