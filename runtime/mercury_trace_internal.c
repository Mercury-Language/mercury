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

#define	MR_isdigit(c)	isdigit((unsigned char) (c))
#define	MR_isspace(c)	isspace((unsigned char) (c))

#define	MR_NAME_LEN		80
#define	MR_MAX_SPY_POINTS	100
#define	MR_LOG10_MAX_SPY_POINTS	20

#define	MR_MAX_LINE_LEN		256

typedef struct {
	bool	enabled;
	char	module_name[MR_NAME_LEN];
	char	pred_name[MR_NAME_LEN];
} MR_spy_point;

typedef enum {
	KEEP_INTERACTING,
	STOP_INTERACTING
} MR_next;

static	MR_spy_point	MR_spy_points[MR_MAX_SPY_POINTS];
static	int		MR_next_spy_point = 0;

static	MR_next	MR_trace_debug_cmd(MR_trace_cmd_info *cmd,
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path, int *ancestor_level);
static	void	MR_trace_list_vars(const MR_Stack_Layout_Label *top_layout,
			int ancestor_level);
static	void	MR_trace_browse_one(const MR_Stack_Layout_Label *top_layout,
			int ancestor_level, int which_var);
static	void	MR_trace_browse_all(const MR_Stack_Layout_Label *top_layout,
			int ancestor_level);
static	void	MR_trace_browse_var(const char *name,
			const MR_Stack_Layout_Var *var,
			bool saved_regs_valid, Word *base_sp, Word *base_curfr,
			Word *type_params);
static	void	MR_trace_help(void);

static	bool	MR_trace_is_number(char *word, int *value);
static	bool	MR_trace_is_number_prefix(char *word, char **suffix,
			int *value);
static	int	MR_trace_break_into_words(char line[], char *words[]);
static	int	MR_trace_getline(FILE *file, char line[], int line_max);

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
	int	ancestor_level;

	MR_trace_event_internal_report(layout, port, seqno, depth, path);

	/* these globals can be overwritten when we call Mercury code */
	saved_seqno = MR_trace_call_seqno;
	saved_depth = MR_trace_call_depth;
	saved_event = MR_trace_event_number;

	/* by default, print variables from the current procedure */
	ancestor_level = 0;

	while (MR_trace_debug_cmd(cmd, layout, port, seqno, depth, path,
			&ancestor_level) == KEEP_INTERACTING) {
		; /* all the work is done in MR_trace_debug_cmd */
	}

	MR_trace_call_seqno = saved_seqno;
	MR_trace_call_depth = saved_depth;
	MR_trace_event_number = saved_event;
}

static MR_next
MR_trace_debug_cmd(MR_trace_cmd_info *cmd, const MR_Stack_Layout_Label *layout,
	MR_trace_port port, int seqno, int depth, const char *path,
	int *ancestor_level)
{
	char	line[MR_MAX_LINE_LEN];
	char	count_buf[MR_MAX_LINE_LEN];
	char	*raw_words[MR_MAX_LINE_LEN / 2 + 1];
	char	**words;
	char	raw_word_count;
	char	word_count;
	char	*s;
	int	i, n;

	printf("mtrace> ");

	if (MR_trace_getline(stdin, line, MR_MAX_LINE_LEN) == 0) {
		/*
		** We got a line without even a newline character,
		** which must mean that the user typed EOF.
		** We arrange things so we don't have to treat this case
		** specially in the command interpreter below.
		*/

		(void) strcpy(line, "a\n");
	}

	/*
	** Handle a possible number prefix on the first word on the line,
	** separating it out into a word on its own.
	*/

	raw_word_count = MR_trace_break_into_words(line, raw_words + 1);

	if (raw_word_count > 0 && MR_isdigit(*raw_words[1])) {
		i = 0;
		s = raw_words[1];
		while (MR_isdigit(*s)) {
			count_buf[i] = *s;
			i++;
			s++;
		}

		count_buf[i] = '\0';

		if (*s == '\0') {
			/* all of the first word constitutes a number */
			/* exchange it with the command, if it exists */

			if (raw_word_count > 1) {
				s = raw_words[1];
				raw_words[1] = raw_words[2];
				raw_words[2] = s;
			}

			words = raw_words + 1;
			word_count = raw_word_count;
		} else {
			/* only part of the first word constitutes a number */
			/* put it in an extra word at the start */

			raw_words[0] = count_buf;
			raw_words[1] = s;
			words = raw_words;
			word_count = raw_word_count + 1;
		}
	} else {
		words = raw_words + 1;
		word_count = raw_word_count;
	}

	/*
	** If the first word is a number, try to exchange it
	** with the command word, to put the command word first.
	*/

	if (word_count > 1 && MR_trace_is_number(words[0], &n)
			&& ! MR_trace_is_number(words[1], &n)) {
		s = words[0];
		words[0] = words[1];
		words[1] = s;
	}

	/*
	** At this point, the first word_count members of the words
	** array contain the command.
	*/

	cmd->MR_trace_print_intermediate = FALSE;

	if (word_count == 0) {
		cmd->MR_trace_cmd = MR_CMD_GOTO;
		cmd->MR_trace_print_intermediate = FALSE;
		cmd->MR_trace_stop_event = MR_trace_event_number + 1;
		return STOP_INTERACTING;
	} else if (MR_trace_is_number(words[0], &n)) {
		if (word_count == 1) {
			cmd->MR_trace_cmd = MR_CMD_GOTO;
			cmd->MR_trace_print_intermediate = FALSE;
			cmd->MR_trace_stop_event = MR_trace_event_number + n;
			return STOP_INTERACTING;
		} else {
			printf("One of the first two words "
				"must be a command.\n");
		}
	} else if (streq(words[0], "S") || streq(words[0], "s")) {
		if (word_count == 1) {
			cmd->MR_trace_cmd = MR_CMD_GOTO;
			cmd->MR_trace_print_intermediate = streq(words[0], "S");
			cmd->MR_trace_stop_event = MR_trace_event_number + 1;
			return STOP_INTERACTING;
		} else if (word_count == 2
				&& MR_trace_is_number(words[1], &n)) {
			cmd->MR_trace_cmd = MR_CMD_GOTO;
			cmd->MR_trace_print_intermediate = streq(words[0], "S");
			cmd->MR_trace_stop_event = MR_trace_event_number + n;
			return STOP_INTERACTING;
		} else {
			printf("This command expects at most one argument,\n"
				"which must be a number.\n");
		}
	} else if (streq(words[0], "g") || streq(words[0], "G")) {
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			if (MR_trace_event_number < n) {
				cmd->MR_trace_cmd = MR_CMD_GOTO;
				cmd->MR_trace_print_intermediate =
					streq(words[0], "G");
				cmd->MR_trace_stop_event = n;
				return STOP_INTERACTING;
			} else {
				printf("The debugger cannot go "
					"to a past event.\n");
			}
		} else {
			printf("This command expects exactly one argument,\n");
			printf("which must be a number.\n");
		}
	} else if (streq(words[0], "f") || streq(words[0], "F")) {
		if (word_count == 1) {
			if (MR_port_is_final(port)) {
				printf("This command is a no-op "
					"from this port.\n");
			} else {
				cmd->MR_trace_cmd = MR_CMD_FINISH;
				cmd->MR_trace_print_intermediate =
					streq(words[0], "F");
				cmd->MR_trace_stop_seqno = seqno;
				return STOP_INTERACTING;
			}
		} else {
			printf("This command expects no argument.\n");
		}
	} else if (streq(words[0], "c") || streq(words[0], "C")) {
		if (word_count == 1) {
			cmd->MR_trace_cmd = MR_CMD_TO_END;
			cmd->MR_trace_print_intermediate =
				streq(words[0], "C");
			return STOP_INTERACTING;
		} else {
			printf("This command expects no argument.\n");
		}
	} else if (streq(words[0], "r") || streq(words[0], "R")) {
		if (word_count == 1) {
			cmd->MR_trace_cmd = MR_CMD_RESUME_FORWARD;
			cmd->MR_trace_print_intermediate =
				streq(words[0], "R");
			return STOP_INTERACTING;
		} else {
			printf("This command expects no argument.\n");
		}
	} else if (streq(words[0], "l")) {
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			*ancestor_level = n;
			printf("Ancestor level set to %d\n", n);
		} else {
			printf("This command expects one argument,\n");
			printf("a number denoting an ancestor level.\n");
		}
	} else if (streq(words[0], "v")) {
		if (word_count == 1) {
			MR_trace_list_vars(layout, *ancestor_level);
		} else {
			printf("This command expects no argument.\n");
		}
	} else if (streq(words[0], "p")) {
		if (word_count == 2) {
			if (MR_trace_is_number(words[1], &n)) {
				MR_trace_browse_one(layout, *ancestor_level, n);
			} else if streq(words[1], "*") {
				MR_trace_browse_all(layout, *ancestor_level);
			} else {
				printf("The argument of this command should be,\n");
				printf("a variable number or a '*' indicating all variables.\n");
			}
		} else {
			printf("This command expects one argument,\n");
			printf("a variable number or a '*' indicating all variables.\n");
		}
	} else if (streq(words[0], "d")) {
		if (word_count == 1) {
			const char	*result;

			do_init_modules();
			result = MR_dump_stack_from_layout(stdout,
					layout->MR_sll_entry,
					MR_saved_sp(MR_saved_regs),
					MR_saved_maxfr(MR_saved_regs));
			if (result != NULL) {
				printf("%s\n", result);
			}
		} else {
			printf("This command expects no argument.\n");
		}
	} else if (streq(words[0], "X")) {
		printf("sp = %p, curfr = %p, maxfr = %p\n",
			MR_saved_sp(MR_saved_regs),
			MR_saved_curfr(MR_saved_regs),
			MR_saved_maxfr(MR_saved_regs));
	} else if (streq(words[0], "b")) {
		if (word_count != 3) {
			printf("This command expects two arguments,\n");
			printf("a module name and a predicate name.\n");
		} else {
			if (MR_next_spy_point >= MR_MAX_SPY_POINTS) {
				printf("There is no room "
					"for any more spy points.\n");
			} else {
				printf("%2d: %s %s:%s\n", MR_next_spy_point,
					"+", words[1], words[2]);
				strcpy(MR_spy_points[MR_next_spy_point]
					.module_name, words[1]);
				strcpy(MR_spy_points[MR_next_spy_point]
					.pred_name, words[2]);
				MR_spy_points[MR_next_spy_point].enabled
					= TRUE;
				MR_next_spy_point++;
			}
		}
	} else if (streq(words[0], "?")) {
		for (i = 0; i < MR_next_spy_point; i++) {
			printf("%2d: %s %s:%s\n", i,
				MR_spy_points[i].enabled ? "+" : "-",
				MR_spy_points[i].module_name,
				MR_spy_points[i].pred_name);
		}
	} else if (streq(words[0], "+")) {
		if (word_count == 2) {
			if (MR_trace_is_number(words[1], &n)) {
				if (0 <= n && n < MR_next_spy_point) {
					MR_spy_points[n].enabled = TRUE;
					printf("%2d: %s %s:%s\n", n,
						"+",
						MR_spy_points[n].module_name,
						MR_spy_points[n].pred_name);
				} else {
					printf("Break point #%d "
						"does not exist.\n", n);
				}
			} else if (streq(words[1], "*")) {
				for (i = 0; i < MR_next_spy_point; i++) {
					MR_spy_points[i].enabled = TRUE;
					printf("%2d: %s %s:%s\n", i,
						"+",
						MR_spy_points[i].module_name,
						MR_spy_points[i].pred_name);
				}
			} else {
				printf("The argument of this command must be "
					"a break point number or a `*'.\n");
			}
		} else {
			printf("This command expects one argument,\n");
			printf("which must be a break point number "
				"or a `*'.\n");
		}
	} else if (streq(words[0], "-")) {
		if (word_count == 2) {
			if (MR_trace_is_number(words[1], &n)) {
				if (0 <= n && n < MR_next_spy_point) {
					MR_spy_points[n].enabled = FALSE;
					printf("%2d: %s %s:%s\n", n,
						"-",
						MR_spy_points[n].module_name,
						MR_spy_points[n].pred_name);
				} else {
					printf("Break point #%d "
						"does not exist.\n", n);
				}
			} else if (streq(words[1], "*")) {
				for (i = 0; i < MR_next_spy_point; i++) {
					MR_spy_points[i].enabled = FALSE;
					printf("%2d: %s %s:%s\n", i,
						"-",
						MR_spy_points[i].module_name,
						MR_spy_points[i].pred_name);
				}
			} else {
				printf("The argument of this command must be "
					"a break point number or a `*'.\n");
			}
		} else {
			printf("This command expects one argument,\n");
			printf("which must be a break point number "
				"or a `*'.\n");
		}
	} else if (streq(words[0], "h")) {
		if (word_count != 1) {
			printf("This command expects no argument.\n");
		}
		MR_trace_help();
	} else if (streq(words[0], "a")) {
		if (word_count == 1) {
			printf("mtrace: are you sure you want to abort? ");
			if (MR_trace_getline(stdin, line, MR_MAX_LINE_LEN)
					== 0) {
				/* This means the user input EOF. */
				exit(0);
			} else {
				for (i = 0; MR_isspace(line[i]); i++)
					;

				if (line[i] == 'y' || line[i] == 'Y') {
					exit(0);
				}
			}
		} else {
			printf("This command expects no argument.\n");
		}
	} else {
		printf("Command not recognized. "
			"Give the command `h' for help.\n");
	}

	return KEEP_INTERACTING;
}

static void
MR_trace_list_vars(const MR_Stack_Layout_Label *top_layout, int ancestor_level)
{
	const MR_Stack_Layout_Label	*level_layout;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	int				var_count;
	const MR_Stack_Layout_Vars	*vars;
	int				i;
	const char 			*problem;

	base_sp = MR_saved_sp(MR_saved_regs);
	base_curfr = MR_saved_curfr(MR_saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
				&base_sp, &base_curfr, &problem);

	if (level_layout == NULL) {
		printf("%s\n", problem);
		return;
	}

	var_count = (int) level_layout->MR_sll_var_count;
	if (var_count < 0) {
		printf("mtrace: there is no information about live variables\n");
		return;
	} else if (var_count == 0) {
		printf("mtrace: there are no live variables\n");
		return;
	}

	vars = &level_layout->MR_sll_var_info;
	for (i = 0; i < var_count; i++) {
		printf("%9d %s\n", i, MR_name_if_present(vars, i));
	}
}

static void
MR_trace_browse_one(const MR_Stack_Layout_Label *top_layout,
	int ancestor_level, int which_var)
{
	const MR_Stack_Layout_Label	*level_layout;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	bool				saved_regs_valid;
	int				var_count;
	const MR_Stack_Layout_Vars	*vars;
	const char 			*problem;

	base_sp = MR_saved_sp(MR_saved_regs);
	base_curfr = MR_saved_curfr(MR_saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
				&base_sp, &base_curfr, &problem);

	if (level_layout == NULL) {
		printf("%s\n", problem);
		return;
	}

	var_count = (int) level_layout->MR_sll_var_count;
	if (var_count < 0) {
		printf("mtrace: there is no information about live variables\n");
		return;
	} else if (which_var >= var_count) {
		printf("mtrace: there is no such variable\n");
		return;
	}

	vars = &level_layout->MR_sll_var_info;
	saved_regs_valid = (ancestor_level == 0);

	type_params = MR_trace_materialize_typeinfos_base(vars,
				saved_regs_valid, base_sp, base_curfr);
	MR_trace_browse_var(MR_name_if_present(vars, which_var),
		&vars->MR_slvs_pairs[which_var], saved_regs_valid,
		base_sp, base_curfr, type_params);
	free(type_params);
}

static void 
MR_trace_browse_all(const MR_Stack_Layout_Label *top_layout,
	int ancestor_level)
{
	const MR_Stack_Layout_Label	*level_layout;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	bool				saved_regs_valid;
	int				var_count;
	const MR_Stack_Layout_Vars	*vars;
	const char 			*problem;
	int				i;

	base_sp = MR_saved_sp(MR_saved_regs);
	base_curfr = MR_saved_curfr(MR_saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
				&base_sp, &base_curfr, &problem);

	if (level_layout == NULL) {
		printf("%s\n", problem);
		return;
	}

	var_count = (int) level_layout->MR_sll_var_count;
	if (var_count < 0) {
		printf("mtrace: there is no information about live variables\n");
		return;
	} else if (var_count == 0) {
		printf("mtrace: there are no live variables\n");
		return;
	}

	vars = &level_layout->MR_sll_var_info;
	saved_regs_valid = (ancestor_level == 0);

	type_params = MR_trace_materialize_typeinfos_base(vars,
				saved_regs_valid, base_sp, base_curfr);

	for (i = 0; i < var_count; i++) {
		MR_trace_browse_var(MR_name_if_present(vars, i),
			&vars->MR_slvs_pairs[i], saved_regs_valid,
			base_sp, base_curfr, type_params);
	}

	free(type_params);
}

static void
MR_trace_browse_var(const char *name, const MR_Stack_Layout_Var *var,
	bool saved_regs_valid, Word *base_sp, Word *base_curfr,
	Word *type_params)
{
	Word	value;
	Word	type_info;
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

	if (MR_trace_get_type_and_value_base(var, saved_regs_valid,
			base_sp, base_curfr, type_params, &type_info, &value))
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

/*
** Is the string pointed to by word an integer?
** If yes, return its value in *value.
*/

static bool
MR_trace_is_number(char *word, int *value)
{
	if (MR_isdigit(*word)) {
		*value = *word - '0';
		word++;
		while (MR_isdigit(*word)) {
			*value = (*value * 10) + *word - '0';
			word++;
		}

		if (*word == '\0') {
			return TRUE;
		}
	}

	return FALSE;
}

/*
** Given a text line, break it up into words composed of non-space characters
** separated by space characters. Make each word a NULL-terminated string
** (overwriting some spaces in the line array in the process), return pointers
** to them in the words array, and return the number of words in the return
** value of the function.
**
** This function assumes that the words array is as long as necessary.
** This can be (and is) ensured by making the words array have one element
** for every two characters in the line array (since you need at least one
** non-space and one space or newline character per word).
**
** This function also assumes that line[] is guaranteed to have a white space
** character (which will usually be a newline) just before the null character.
*/

static int
MR_trace_break_into_words(char line[], char *words[])
{
	int	token_number;
	int	char_pos;
	int	int_val;

	token_number = 0;
	char_pos = 0;

	/* each iteration of this loop processes one token, or end of line */
	for (;;) {
		while (line[char_pos] != '\0' && MR_isspace(line[char_pos])) {
			char_pos++;
		}

		if (line[char_pos] == '\0') {
			return token_number;
		}

		words[token_number] = line + char_pos;
		while (line[char_pos] != '\0' && !MR_isspace(line[char_pos])) {
			char_pos++;
		}

		line[char_pos] = '\0';
		char_pos++;
		token_number++;
	}
}

/*
**	Read a line from a file. If the line does not fit in the array,
**	read the whole line anyway but store only the first part.
**	If the last line ends without a newline, insert it.
**	Return the length of the (possibly truncated) line.
**	This will be zero only if getline is called at EOF;
**	it will be one only if line contains a single newline;
**	otherwise it will contain a newline terminated string.
*/

static int
MR_trace_getline(FILE *file, char line[], int line_max)
{
	int	c, i;

	i = 0;
	while ((c = getc(file)) != EOF && c != '\n') {
		if (i < line_max-1) {
			line[i++] = c;
		}
	}

	if (c == '\n' || i > 0) {
		line[i++] = '\n';
	}

	line[i] = '\0';
	return i;
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
		"v:\t\t"
		"\tlist the names of the variables live at this point.\n"
		"l <n>:\t\t"
		"\tset ancestor level to <n>\n"
		"p <n>:\t\t"
		"\tprint variable #n (or all vars if <n> is '*')\n"
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
