/*
** Copyright (C) 1998-1999 The University of Melbourne.
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
#include "mercury_trace_declarative.h"
#include "mercury_trace_alias.h"
#include "mercury_trace_help.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"
#include "mercury_trace_readline.h"
#include "mercury_layout_util.h"
#include "mercury_array_macros.h"
#include "mercury_getopt.h"

#include "browse.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

/* The initial size of the spy points table. */
#define	MR_INIT_SPY_POINTS	10

/* The initial size of arrays of words. */
#define	MR_INIT_WORD_COUNT	20

/* The initial number of lines in documentation entries. */
#define	MR_INIT_DOC_CHARS	800

/* An upper bound on the maximum number of characters in a number. */
/* If a number has more than this many chars, the user is in trouble. */
#define	MR_NUMBER_LEN		80

#define	MDBRC_FILENAME		".mdbrc"
#define	DEFAULT_MDBRC_FILENAME	"mdbrc"

/*
** XXX We should consider whether all the static variables in this module
** should be thread local.
*/
/*
** Debugger I/O streams.
** Replacements for stdin/stdout/stderr respectively.
**
** The distinction between MR_mdb_out and MR_mdb_err is analagous to
** the distinction between stdout and stderr: ordinary output, including
** information messages about conditions which are not errors, should
** go to MR_mdb_out, but error messages should go to MR_mdb_err.
**
** Note that MR_mdb_out and MR_mdb_err may both write to the same
** file, so we need to be careful to ensure that buffering does
** not stuff up the interleaving of error messages and ordinary output.
** To ensure this, we do two things:
**	- MR_mdb_err is unbuffered
**	- we always fflush(MR_mdb_out) before writing to MR_mdb_err
*/
FILE *MR_mdb_in;
FILE *MR_mdb_out;
FILE *MR_mdb_err;

static	MR_Trace_Print_Level	MR_default_print_level = MR_PRINT_LEVEL_SOME;

/*
** The table of spy points, with counters saying which is the next free slot
** and how many slots are allocated.
*/

static	MR_Spy_Point    	**MR_spy_points;
static	int			MR_spy_point_next = 0;
static	int			MR_spy_point_max  = 0;

/*
** These variables say (a) whether the printing of event sequences will pause
** after each screenful of events, (b) how may events constitute a screenful
** (although we count only events, not how many lines they take up), and (c)
** how many events we have printed so far in this screenful.
*/

static	bool			MR_scroll_control = FALSE;
static	int			MR_scroll_limit = 24;
static	int			MR_scroll_next = 0;

/*
** We echo each command just as it is executed iff this variable is TRUE.
*/

static	bool			MR_echo_commands = FALSE;

/*
** We print confirmation of commands (e.g. new aliases) if this is TRUE.
*/

static	bool			MR_trace_internal_interacting = FALSE;

typedef struct MR_Line_Struct {
	char			*MR_line_contents;
	struct MR_Line_Struct	*MR_line_next;
} MR_Line;

static	MR_Line			*MR_line_head = NULL;
static	MR_Line			*MR_line_tail = NULL;

typedef enum {
	KEEP_INTERACTING,
	STOP_INTERACTING
} MR_Next;

typedef	enum {
	VAR_NUMBER,
	VAR_NAME
} MR_Var_Spec_Kind;

typedef struct {
	MR_Var_Spec_Kind	MR_var_spec_kind;
	int			MR_var_spec_number; /* valid if VAR_NUMBER */
	const char		*MR_var_spec_name;  /* valid if VAR_NAME   */
} MR_Var_Spec;

#ifdef	MR_USE_DECLARATIVE_DEBUGGER

MR_Trace_Mode MR_trace_decl_mode = MR_TRACE_INTERACTIVE;

#endif	/* MR_USE_DECLARATIVE_DEBUGGER */

static	void	MR_trace_internal_ensure_init(void);
static	void	MR_trace_internal_init_from_env(void);
static	void	MR_trace_internal_init_from_local(void);
static	void	MR_trace_internal_init_from_home_dir(void);
static	MR_Next	MR_trace_debug_cmd(char *line, MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info, 
			MR_Event_Details *event_details, int *ancestor_level,
			Code **jumpaddr);
static	MR_Next	MR_trace_handle_cmd(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, int *ancestor_level,
			Code **jumpaddr);
static	bool	MR_trace_options_strict_print(MR_Trace_Cmd_Info *cmd,
			char ***words, int *word_count,
			const char *cat, const char *item);
static	bool	MR_trace_options_when_action(MR_Spy_When *when,
			MR_Spy_Action *action, char ***words, int *word_count,
			const char *cat, const char *item);
static	bool	MR_trace_options_quiet(bool *verbose,
			char ***words, int *word_count,
			const char *cat, const char *item);
static	bool	MR_trace_options_detailed(bool *detailed, char ***words,
			int *word_count, const char *cat, const char *item);
static	bool	MR_trace_options_confirmed(bool *confirmed, char ***words,
			int *word_count, const char *cat, const char *item);
static	void	MR_trace_usage(const char *cat, const char *item);
static	void	MR_trace_internal_add_spy_point(MR_Spy_When when,
			MR_Spy_Action action,
			const MR_Stack_Layout_Entry *entry,
			const MR_Stack_Layout_Label *label);
static	void	MR_print_spy_point(int i);
static	void	MR_trace_list_vars(const MR_Stack_Layout_Label *top_layout,
			Word *saved_regs, int ancestor_level);
static	void	MR_trace_set_level(const MR_Stack_Layout_Label *top_layout,
			Word *saved_regs, int *ancestor_level,
			int new_ancestor_level);
static	const char *MR_trace_browse_check_level(const MR_Stack_Layout_Label
			*top_layout, Word *saved_regs, int ancestor_level);
static	void	MR_trace_browse_one(const MR_Stack_Layout_Label *top_layout,
			Word *saved_regs, int ancestor_level,
			MR_Var_Spec which_var, bool browse);
static	void	MR_trace_browse_all(const MR_Stack_Layout_Label *top_layout,
			Word *saved_regs, int ancestor_level);
static	void	MR_trace_browse_var(const char *name,
			const MR_Stack_Layout_Var *var,
			Word *saved_regs, Word *base_sp, Word *base_curfr,
			Word *type_params, bool browse);
static	const char *MR_trace_validate_var_count(const MR_Stack_Layout_Label
			*layout, int *var_count_ptr);
static	const char *MR_trace_find_var(const MR_Stack_Layout_Label *layout,
			MR_Var_Spec var_spec, int *which_var_ptr);
static	void	MR_trace_do_noop(void);

static	const char *MR_trace_read_help_text(void);
static	bool	MR_trace_is_number(const char *word, int *value);
static	bool	MR_trace_is_number_prefix(char *word, char **suffix,
			int *value);
static	const char *MR_trace_parse_line(char *line,
			char ***words, int *word_max, int *word_count);
static	int	MR_trace_break_into_words(char *line,
			char ***words_ptr, int *word_max_ptr);
static	void	MR_trace_expand_aliases(char ***words,
			int *word_max, int *word_count);
static	bool	MR_trace_source(const char *filename);
static	void	MR_trace_source_from_open_file(FILE *fp);
static	char	*MR_trace_getline(const char *prompt);
static	char	*MR_trace_getline_queue(void);
static	void	MR_insert_line_at_head(const char *line);
static	void	MR_insert_line_at_tail(const char *line);

static	void	MR_trace_event_print_internal_report(
			MR_Event_Info *event_info);
static	void	MR_trace_print_port(MR_Trace_Port port);

static	bool	MR_trace_valid_command(const char *word);

Code *
MR_trace_event_internal(MR_Trace_Cmd_Info *cmd, bool interactive,
		MR_Event_Info *event_info)
{
	int			i;
	int			c;
	int			count;
	bool			count_given;
	int			ancestor_level;
	Code			*jumpaddr;
	char			*line;
	MR_Next			res;
	MR_Event_Details	event_details;

	if (! interactive) {
		return MR_trace_event_internal_report(cmd, event_info);
	}

#ifdef	MR_USE_DECLARATIVE_DEBUGGER
	if (MR_trace_decl_mode == MR_TRACE_WRONG_ANSWER) {
		return MR_trace_decl_wrong_answer(cmd, event_info);
	}
#endif	MR_USE_DECLARATIVE_DEBUGGER

	MR_trace_enabled = FALSE;
	MR_trace_internal_ensure_init();

	MR_trace_event_print_internal_report(event_info);

	/*
	** These globals can be overwritten when we call Mercury code,
	** such as the term browser. We therefore save and restore them
	** across calls to MR_trace_debug_cmd. However, we store the
	** saved values in a structure that we pass to MR_trace_debug_cmd,
	** to allow them to be modified by MR_trace_retry().
	*/

	event_details.MR_call_seqno = MR_trace_call_seqno;
	event_details.MR_call_depth = MR_trace_call_depth;
	event_details.MR_event_number = MR_trace_event_number;

	/* by default, print variables from the current call */
	ancestor_level = 0;

	/* by default, return where we came from */
	jumpaddr = NULL;

	do {
		line = MR_trace_getline("mdb> ");
		res = MR_trace_debug_cmd(line, cmd, event_info, &event_details,
				&ancestor_level, &jumpaddr);
	} while (res == KEEP_INTERACTING);

	cmd->MR_trace_must_check = (! cmd->MR_trace_strict) ||
			(cmd->MR_trace_print_level != MR_PRINT_LEVEL_NONE);

	MR_trace_call_seqno = event_details.MR_call_seqno;
	MR_trace_call_depth = event_details.MR_call_depth;
	MR_trace_event_number = event_details.MR_event_number;

	MR_scroll_next = 0;
	MR_trace_enabled = TRUE;
	return jumpaddr;
}

static const char MR_trace_banner[] =
"Melbourne Mercury Debugger, mdb version %s.\n\
Copyright 1998 The University of Melbourne, Australia.\n\
mdb is free software, covered by the GNU General Public License.\n\
There is absolutely no warranty for mdb.\n";

static FILE *
MR_try_fopen(const char *filename, const char *mode, FILE *default_file)
{
	if (filename == NULL) {
		return default_file;
	} else {
		FILE *f = fopen(filename, mode);
		if (f == NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: error opening `%s': %s\n",
				filename, strerror(errno));
			return default_file;
		} else {
			return f;
		}
	}
}

static void
MR_trace_internal_ensure_init(void)
{
	static	bool	MR_trace_internal_initialized = FALSE;

	if (! MR_trace_internal_initialized) {
		char	*env;
		int	n;

		MR_mdb_in = MR_try_fopen(MR_mdb_in_filename, "r", stdin);
		MR_mdb_out = MR_try_fopen(MR_mdb_out_filename, "w", stdout);
		MR_mdb_err = MR_try_fopen(MR_mdb_err_filename, "w", stderr);

		/* Ensure that MR_mdb_err is not buffered */
		setvbuf(MR_mdb_err, NULL, _IONBF, 0);

		if (getenv("MERCURY_SUPPRESS_MDB_BANNER") == NULL) {
			fprintf(MR_mdb_out, MR_trace_banner, MR_VERSION);
		}

		env = getenv("LINES");
		if (env != NULL && MR_trace_is_number(env, &n)) {
			MR_scroll_limit = n;
		}

		MR_trace_internal_init_from_env();
		MR_trace_internal_init_from_local();
		MR_trace_internal_init_from_home_dir();

		MR_trace_internal_initialized = TRUE;
	}
}

static void
MR_trace_internal_init_from_env(void)
{
	char	*init;

	init = getenv("MERCURY_DEBUGGER_INIT");
	if (init != NULL) {
		(void) MR_trace_source(init);
		/* If the source failed, the error message has been printed. */
	}
}

static void
MR_trace_internal_init_from_local(void)
{
	FILE	*fp;

	if ((fp = fopen(MDBRC_FILENAME, "r")) != NULL) {
		MR_trace_source_from_open_file(fp);
		fclose(fp);
	}
}

static void
MR_trace_internal_init_from_home_dir(void)
{
	char	*env;
	char	*buf;
	int	len;
	FILE	*fp;

	/* XXX This code is too Unix specific. */

	env = getenv("HOME");
	if (env == NULL) {
		return;
	}

	buf = checked_malloc(strlen(env) + strlen(MDBRC_FILENAME) + 2);
	(void) strcpy(buf, env);
	(void) strcat(buf, "/");
	(void) strcat(buf, MDBRC_FILENAME);
	if ((fp = fopen(buf, "r")) != NULL) {
		MR_trace_source_from_open_file(fp);
		fclose(fp);
	}

	free(buf);
}

static void
MR_trace_do_noop(void)
{
	fflush(MR_mdb_out);
	fprintf(MR_mdb_err,
		"This command is a no-op from this port.\n");
}

/*
** This function is just a wrapper for MR_print_proc_id_for_debugger,
** with the first argument type being `void *' rather than `FILE *',
** so that this function's address can be passed to
** MR_process_matching_procedures().
*/
static void
MR_mdb_print_proc_id(void *data, const MR_Stack_Layout_Entry *entry_layout)
{
	FILE *fp = data;
	MR_print_proc_id_for_debugger(fp, entry_layout);
}

/* Options to pass to mmc when compiling queries. */
static char *MR_mmc_options = NULL;

static MR_Next
MR_trace_debug_cmd(char *line, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	int *ancestor_level, Code **jumpaddr)
{
	char		**words;
	char		**orig_words = NULL;
	int		word_max;
	int		word_count;
	const char	*problem;
	char		*semicolon;
	MR_Next		next;

	if (line == NULL) {
		/*
		** We got an EOF.
		** We arrange things so we don't have to treat this case
		** specially in the command interpreter below.
		*/
		line = MR_copy_string("quit");
	}

	if ((semicolon = strchr(line, ';')) != NULL) {
		/*
		** The line contains at least two commands.
		** Execute only the first command now; put the others
		** back in the input to be processed later.
		*/
		*semicolon = '\0';
		MR_insert_line_at_head(MR_copy_string(semicolon + 1));
	}

	if (MR_echo_commands) {
		fputs(line, MR_mdb_out);
		putc('\n', MR_mdb_out);
	}

	problem = MR_trace_parse_line(line, &words, &word_max, &word_count);
	if (problem != NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s.\n", problem);
		return KEEP_INTERACTING;
	}

	MR_trace_expand_aliases(&words, &word_max, &word_count);

	/*
	** At this point, the first word_count members of the words
	** array contain the command. We save the value of words for
	** freeing just before return, since the variable words itself
	** can be overwritten by option processing.
	*/
	orig_words = words;

	/*
	** Now we check for a special case.
	*/
	if (word_count == 0) {
		/*
		** Normally EMPTY is aliased to "step", so this won't happen.
		** This can only occur if the user has unaliased EMPTY.
		** In that case, if we get an empty command line, we ignore it.
		*/
		next = KEEP_INTERACTING;
	} else {
		/*
		** Call the command dispatcher
		*/
		next = MR_trace_handle_cmd(words, word_count, cmd,
			event_info, event_details, ancestor_level, jumpaddr);
	}

	free(line);
	free(orig_words);

	return next;
}

/*
** IMPORTANT: if you add any new commands, you will need to
**	(a) include them in MR_trace_valid_command_list, defined below.
**	(b) document them in doc/user_guide.texi
*/
static MR_Next
MR_trace_handle_cmd(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	int *ancestor_level, Code **jumpaddr)
{
	const MR_Stack_Layout_Label
		*layout = event_info->MR_event_sll;
	Word 	*saved_regs = event_info->MR_saved_regs;

	if (streq(words[0], "step")) {
		int	n;

		cmd->MR_trace_strict = FALSE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "step"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			cmd->MR_trace_cmd = MR_CMD_GOTO;
			cmd->MR_trace_stop_event = MR_trace_event_number + 1;
			return STOP_INTERACTING;
		} else if (word_count == 2
				&& MR_trace_is_number(words[1], &n))
		{
			cmd->MR_trace_cmd = MR_CMD_GOTO;
			cmd->MR_trace_stop_event = MR_trace_event_number + n;
			return STOP_INTERACTING;
		} else {
			MR_trace_usage("forward", "step");
		}
	} else if (streq(words[0], "goto")) {
		int	n;

		cmd->MR_trace_strict = TRUE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "goto"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 2 && MR_trace_is_number(words[1], &n))
				{
			if (MR_trace_event_number < n) {
				cmd->MR_trace_cmd = MR_CMD_GOTO;
				cmd->MR_trace_stop_event = n;
				return STOP_INTERACTING;
			} else {
				/* XXX this message is misleading */
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err, "The debugger cannot go "
					"to a past event.\n");
			}
		} else {
			MR_trace_usage("forward", "goto");
		}
	} else if (streq(words[0], "finish")) {
		Unsigned	depth = event_info->MR_call_depth;
		int		stop_depth;
		int		n;

		cmd->MR_trace_strict = TRUE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "finish"))
		{
			; /* the usage message has already been printed */
			return KEEP_INTERACTING;
		} else if (word_count == 2 && MR_trace_is_number(words[1], &n))
		{
			stop_depth = depth - n;
		} else if (word_count == 1) {
			stop_depth = depth;
		} else {
			MR_trace_usage("forward", "finish");
			return KEEP_INTERACTING;
		}

		if (depth == stop_depth &&
			MR_port_is_final(event_info->MR_trace_port))
		{
			MR_trace_do_noop();
		} else {
			cmd->MR_trace_cmd = MR_CMD_FINISH;
			cmd->MR_trace_stop_depth = stop_depth;
			return STOP_INTERACTING;
		}
	} else if (streq(words[0], "return")) {
		cmd->MR_trace_strict = TRUE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "return"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			if (event_info->MR_trace_port == MR_PORT_EXIT) {
				cmd->MR_trace_cmd = MR_CMD_RETURN;
				return STOP_INTERACTING;
			} else {
				MR_trace_do_noop();
			}
		} else {
			MR_trace_usage("forward", "return");
		}
	} else if (streq(words[0], "forward")) {
		cmd->MR_trace_strict = TRUE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "forward"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			MR_Trace_Port	port = event_info->MR_trace_port;
			if (port == MR_PORT_FAIL ||
			    port == MR_PORT_REDO ||
			    port == MR_PORT_EXCEPTION)
			{
				cmd->MR_trace_cmd = MR_CMD_RESUME_FORWARD;
				return STOP_INTERACTING;
			} else {
				MR_trace_do_noop();
			}
		} else {
			MR_trace_usage("forward", "forward");
		}
	} else if (streq(words[0], "mindepth")) {
		int	newdepth;

		cmd->MR_trace_strict = TRUE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "mindepth"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 2 &&
				MR_trace_is_number(words[1], &newdepth))
		{
			cmd->MR_trace_cmd = MR_CMD_MIN_DEPTH;
			cmd->MR_trace_stop_depth = newdepth;
			return STOP_INTERACTING;
		} else {
			MR_trace_usage("forward", "mindepth");
		}
	} else if (streq(words[0], "maxdepth")) {
		int	newdepth;

		cmd->MR_trace_strict = TRUE;
		cmd->MR_trace_print_level = MR_default_print_level;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "maxdepth"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 2 &&
				MR_trace_is_number(words[1], &newdepth))
		{
			cmd->MR_trace_cmd = MR_CMD_MAX_DEPTH;
			cmd->MR_trace_stop_depth = newdepth;
			return STOP_INTERACTING;
		} else {
			MR_trace_usage("forward", "maxdepth");
		}
	} else if (streq(words[0], "continue")) {
		cmd->MR_trace_strict = FALSE;
		cmd->MR_trace_print_level = (MR_Trace_Cmd_Type) -1;
		if (! MR_trace_options_strict_print(cmd, &words, &word_count,
				"forward", "continue"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			cmd->MR_trace_cmd = MR_CMD_TO_END;
			if (cmd->MR_trace_print_level ==
					(MR_Trace_Cmd_Type) -1) {
				/*
				** The user did not specify the print level;
				** select the intelligent default.
				*/
				if (cmd->MR_trace_strict) {
					cmd->MR_trace_print_level =
						MR_PRINT_LEVEL_NONE;
				} else {
					cmd->MR_trace_print_level =
						MR_PRINT_LEVEL_SOME;
				}
			}
			return STOP_INTERACTING;
		} else {
			MR_trace_usage("forward", "continue");
		}
	} else if (streq(words[0], "retry")) {
		int		n;
		int		stop_depth;
		const char   	*message;
		Unsigned	depth = event_info->MR_call_depth;
		MR_Trace_Port	port = event_info->MR_trace_port;

		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			stop_depth = depth - n;
		} else if (word_count == 1) {
			stop_depth = depth;
		} else {
			MR_trace_usage("backward", "retry");
			return KEEP_INTERACTING;
		}

		if (stop_depth == depth && MR_port_is_final(port)) {
			message = MR_trace_retry(event_info, event_details,
					jumpaddr);
			if (message != NULL) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err, "%s\n", message);
				return KEEP_INTERACTING;
			}
			cmd->MR_trace_cmd = MR_CMD_GOTO;
			cmd->MR_trace_stop_event = MR_trace_event_number + 1;
			cmd->MR_trace_strict = FALSE;
			cmd->MR_trace_print_level = MR_default_print_level;
			return STOP_INTERACTING;
		} else if (stop_depth == depth && MR_port_is_entry(port)) {
			MR_trace_do_noop();
		} else {
			char	*retry_cmd;

			/* Finish the call to be retried. */
			cmd->MR_trace_cmd = MR_CMD_FINISH;
			cmd->MR_trace_stop_depth = stop_depth;
			cmd->MR_trace_strict = TRUE;
			cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;

			/* Arrange to retry the call once it is finished. */
			MR_insert_line_at_head("retry");
			return STOP_INTERACTING;
		}
	} else if (streq(words[0], "level")) {
		int	n;
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			MR_trace_set_level(layout, saved_regs, ancestor_level,
				n);
		} else {
			MR_trace_usage("browsing", "level");
		}
	} else if (streq(words[0], "up")) {
		int	n;
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			MR_trace_set_level(layout, saved_regs, ancestor_level,
				*ancestor_level + n);
		} else if (word_count == 1) {
			MR_trace_set_level(layout, saved_regs, ancestor_level,
				*ancestor_level + 1);
		} else {
			MR_trace_usage("browsing", "up");
		}
	} else if (streq(words[0], "down")) {
		int	n;
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			MR_trace_set_level(layout, saved_regs, ancestor_level,
				*ancestor_level - n);
		} else if (word_count == 1) {
			MR_trace_set_level(layout, saved_regs, ancestor_level,
				*ancestor_level - 1);
		} else {
			MR_trace_usage("browsing", "down");
		}
	} else if (streq(words[0], "vars")) {
		if (word_count == 1) {
			MR_trace_list_vars(layout, saved_regs,
				*ancestor_level);
		} else {
			MR_trace_usage("browsing", "vars");
		}
	} else if (streq(words[0], "print")) {
		if (word_count == 2) {
			MR_Var_Spec	var_spec;
			int		n;

			if streq(words[1], "*") {
				MR_trace_browse_all(layout, saved_regs,
					*ancestor_level);
			} else if (MR_trace_is_number(words[1], &n)) {
				var_spec.MR_var_spec_kind = VAR_NUMBER;
				var_spec.MR_var_spec_number = n;
				MR_trace_browse_one(layout, saved_regs,
					*ancestor_level, var_spec, FALSE);
			} else {
				var_spec.MR_var_spec_kind = VAR_NAME;
				var_spec.MR_var_spec_name = words[1];
				MR_trace_browse_one(layout, saved_regs,
					*ancestor_level, var_spec, FALSE);
			}
		} else {
			MR_trace_usage("browsing", "print");
		}
	} else if (streq(words[0], "browse")) {
		if (word_count == 2) {
			MR_Var_Spec	var_spec;
			int		n;

			if (MR_trace_is_number(words[1], &n)) {
				var_spec.MR_var_spec_kind = VAR_NUMBER;
				var_spec.MR_var_spec_number = n;
				MR_trace_browse_one(layout, saved_regs,
					*ancestor_level, var_spec, TRUE);
			} else {
				var_spec.MR_var_spec_kind = VAR_NAME;
				var_spec.MR_var_spec_name = words[1];
				MR_trace_browse_one(layout, saved_regs,
					*ancestor_level, var_spec, TRUE);
			}
		} else {
			MR_trace_usage("browsing", "browse");
		}
	} else if (streq(words[0], "stack")) {
		bool	include_trace_data;

		include_trace_data = FALSE;
		if (! MR_trace_options_detailed(&include_trace_data,
				&words, &word_count, "browsing", "stack"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			const char	*msg;
			do_init_modules();
			msg = MR_dump_stack_from_layout(MR_mdb_out,
					layout->MR_sll_entry,
					MR_saved_sp(saved_regs),
					MR_saved_curfr(saved_regs),
					include_trace_data,
					&MR_dump_stack_record_print);
			if (msg != NULL) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err, "%s.\n", msg);
			}
		} else {
			MR_trace_usage("browsing", "stack");
		}
	} else if (streq(words[0], "current")) {
		if (word_count == 1) {
			MR_trace_event_print_internal_report(event_info);
		} else {
			MR_trace_usage("browsing", "current");
		}
	} else if (streq(words[0], "break")) {
		MR_Proc_Spec	spec;
		MR_Spy_When	when;
		MR_Spy_Action	action;

		if (word_count == 2 && streq(words[1], "info")) {
			int i;
			for (i = 0; i < MR_spy_point_next; i++) {
				MR_print_spy_point(i);
			}

			return STOP_INTERACTING;
		}

		when = MR_SPY_INTERFACE;
		action = MR_SPY_STOP;
		if (! MR_trace_options_when_action(&when, &action,
				&words, &word_count, "breakpoint", "break"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 2 && streq(words[1], "here")) {
			MR_register_all_modules_and_procs(MR_mdb_out, TRUE);
			MR_trace_internal_add_spy_point(MR_SPY_SPECIFIC,
				action, layout->MR_sll_entry, layout);
		} else if (word_count == 2 &&
				MR_parse_proc_spec(words[1], &spec))
		{
			const MR_Stack_Layout_Entry	*spy_proc;
			bool				unique;

			MR_register_all_modules_and_procs(MR_mdb_out, TRUE);
			spy_proc = MR_search_for_matching_procedure(&spec,
					&unique);
			if (spy_proc != NULL) {
				if (unique) {
					MR_trace_internal_add_spy_point(when,
						action, spy_proc, NULL);
				} else {
					fflush(MR_mdb_out);
					fprintf(MR_mdb_err,
						"Ambiguous procedure "
						"specification. "
						"The matches are:\n");
					MR_process_matching_procedures(&spec,
						MR_mdb_print_proc_id,
						MR_mdb_err);
				}
			} else {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"There is no such procedure.\n");
			}
		} else {
			MR_trace_usage("breakpoint", "break");
		}
	} else if (streq(words[0], "enable")) {
		int	n;
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			if (0 <= n && n < MR_spy_point_next) {
				MR_spy_points[n]->spy_enabled = TRUE;
				MR_print_spy_point(n);
			} else {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"Break point #%d does not exist.\n",
					n);
			}
		} else if (word_count == 2 && streq(words[1], "*")) {
			int i;
			for (i = 0; i < MR_spy_point_next; i++) {
				MR_spy_points[i]->spy_enabled = TRUE;
				MR_print_spy_point(n);
			}

			if (MR_spy_point_next == 0) {
				fprintf(MR_mdb_out,
					"There are no break points yet.\n");
			}
		} else {
			MR_trace_usage("breakpoint", "enable");
		}
	} else if (streq(words[0], "disable")) {
		int	n;
		if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
			if (0 <= n && n < MR_spy_point_next) {
				MR_spy_points[n]->spy_enabled = FALSE;
				MR_print_spy_point(n);
			} else {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"Break point #%d does not exist.\n",
					n);
			}
		} else if (word_count == 2 && streq(words[1], "*")) {
			int i;
			for (i = 0; i < MR_spy_point_next; i++) {
				MR_spy_points[i]->spy_enabled = FALSE;
				MR_print_spy_point(n);
			}

			if (MR_spy_point_next == 0) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"There are no break points yet.\n");
			}
		} else {
			MR_trace_usage("breakpoint", "disable");
		}
	} else if (streq(words[0], "register")) {
		bool	verbose;

		if (! MR_trace_options_quiet(&verbose, &words, &word_count,
				"breakpoint", "register"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			MR_register_all_modules_and_procs(MR_mdb_out, verbose);
		} else {
			MR_trace_usage("breakpoint", "register");
		}
	} else if (streq(words[0], "modules")) {
		if (word_count == 1) {
			MR_register_all_modules_and_procs(MR_mdb_out, TRUE);
			MR_dump_module_list(MR_mdb_out);
		} else {
			MR_trace_usage("breakpoint", "modules");
		}
	} else if (streq(words[0], "procedures")) {
		if (word_count == 2) {
			MR_register_all_modules_and_procs(MR_mdb_out, TRUE);
			MR_dump_module_procs(MR_mdb_out, words[1]);
		} else {
			MR_trace_usage("breakpoint", "procedures");
		}
	} else if (streq(words[0], "printlevel")) {
		if (word_count == 2) {
			if (streq(words[1], "none")) {
				MR_default_print_level = MR_PRINT_LEVEL_NONE;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Default print level set to "
						"`none'.\n");
				}
			} else if (streq(words[1], "some")) {
				MR_default_print_level = MR_PRINT_LEVEL_SOME;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Default print level set to "
						"`some'.\n");
				}
			} else if (streq(words[1], "all")) {
				MR_default_print_level = MR_PRINT_LEVEL_ALL;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Default print level set to "
						"`all'.\n");
				}
			} else {
				MR_trace_usage("parameter",
					"printlevel");
			}
		} else if (word_count == 1) {
			fprintf(MR_mdb_out, "The default print level is ");
			switch (MR_default_print_level) {
				case MR_PRINT_LEVEL_NONE:
					fprintf(MR_mdb_out, "`none'.\n");
					break;
				case MR_PRINT_LEVEL_SOME:
					fprintf(MR_mdb_out, "`some'.\n");
					break;
				case MR_PRINT_LEVEL_ALL:
					fprintf(MR_mdb_out, "`all'.\n");
					break;
				default:
					MR_default_print_level =
						MR_PRINT_LEVEL_SOME;
					fprintf(MR_mdb_out, "invalid "
						"(now set to `some').\n");
					break;
			}
		} else {
			MR_trace_usage("parameter", "printlevel");
		}
	} else if (streq(words[0], "query")) {
		MR_trace_query(MR_NORMAL_QUERY, MR_mmc_options,
			word_count - 1, words + 1);
	} else if (streq(words[0], "cc_query")) {
		MR_trace_query(MR_CC_QUERY, MR_mmc_options,
			word_count - 1, words + 1);
	} else if (streq(words[0], "io_query")) {
		MR_trace_query(MR_IO_QUERY, MR_mmc_options,
			word_count - 1, words + 1);
	} else if (streq(words[0], "mmc_options")) {
		size_t len;
		size_t i;

		/* allocate the right amount of space */
		len = 0;
		for (i = 1; i < word_count; i++) {
			len += strlen(words[i]) + 1;
		}
		len++;
		MR_mmc_options = realloc(MR_mmc_options, len);

		/* copy the arguments to MR_mmc_options */
		MR_mmc_options[0] = '\0';
		for (i = 1; i < word_count; i++) {
			strcat(MR_mmc_options, words[i]);
			strcat(MR_mmc_options, " ");
		}
		MR_mmc_options[len] = '\0';

	} else if (streq(words[0], "scroll")) {
		int	n;
		if (word_count == 2) {
			if (streq(words[1], "off")) {
				MR_scroll_control = FALSE;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Scroll control disabled.\n");
				}
			} else if (streq(words[1], "on")) {
				MR_scroll_control = TRUE;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Scroll control enabled.\n");
				}
			} else if (MR_trace_is_number(words[1], &n)) {
				MR_scroll_limit = n;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Scroll window size set to "
						"%d.\n", MR_scroll_limit);
				}
			} else {
				MR_trace_usage("parameter", "scroll");
			}
		} else if (word_count == 1) {
			fprintf(MR_mdb_out, "Scroll control is ");
			if (MR_scroll_control) {
				fprintf(MR_mdb_out, "on");
			} else {
				fprintf(MR_mdb_out, "off");
			}
			fprintf(MR_mdb_out, ", scroll window size is %d.\n",
				MR_scroll_limit);
		} else {
			MR_trace_usage("parameter", "scroll");
		}
	} else if (streq(words[0], "echo")) {
		if (word_count == 2) {
			if (streq(words[1], "off")) {
				MR_echo_commands = FALSE;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Command echo disabled.\n");
				}
			} else if (streq(words[1], "on")) {
				MR_echo_commands = TRUE;
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Command echo enabled.\n");
				}
			} else {
				MR_trace_usage("parameter", "echo");
			}
		} else if (word_count == 1) {
			fprintf(MR_mdb_out, "Command echo is ");
			if (MR_echo_commands) {
				fprintf(MR_mdb_out, "on.\n");
			} else {
				fprintf(MR_mdb_out, "off.\n");
			}
		} else {
			MR_trace_usage("parameter", "echo");
		}
	} else if (streq(words[0], "alias")) {
		if (word_count == 1) {
			MR_trace_print_all_aliases(MR_mdb_out);
		} else if (word_count == 2) {
			MR_trace_print_alias(MR_mdb_out, words[1]);
		} else {
			if (MR_trace_valid_command(words[2])) {
				MR_trace_add_alias(words[1],
					words+2, word_count-2);
				if (MR_trace_internal_interacting) {
					MR_trace_print_alias(MR_mdb_out,
						words[1]);
				}
			} else {
				fprintf(MR_mdb_out,
					"`%s' is not a valid command.\n",
					words[2]);
			}
		}
	} else if (streq(words[0], "unalias")) {
		if (word_count == 2) {
			if (MR_trace_remove_alias(words[1])) {
				if (MR_trace_internal_interacting) {
					fprintf(MR_mdb_out,
						"Alias `%s' removed.\n",
						words[1]);
				}
			} else {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"Alias `%s' cannot be removed, "
					"since it does not exist.\n",
					words[1]);
			}
		} else {
			MR_trace_usage("parameter", "unalias");
		}
	} else if (streq(words[0], "document_category")) {
		int		slot;
		const char	*msg;
		const char	*help_text;

		help_text = MR_trace_read_help_text();
		if (word_count != 3) {
			MR_trace_usage("help", "document_category");
		} else if (! MR_trace_is_number(words[1], &slot)) {
			MR_trace_usage("help", "document_category");
		} else {
			msg = MR_trace_add_cat(words[2], slot, help_text);
			if (msg != NULL) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"Document category `%s' not added: "
					"%s.\n", words[2], msg);
			}
		}
	} else if (streq(words[0], "document")) {
		int		slot;
		const char	*msg;
		const char	*help_text;

		help_text = MR_trace_read_help_text();
		if (word_count != 4) {
			MR_trace_usage("help", "document");
		} else if (! MR_trace_is_number(words[2], &slot)) {
			MR_trace_usage("help", "document");
		} else {
			msg = MR_trace_add_item(words[1], words[3], slot,
				help_text);
			if (msg != NULL) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"Document item `%s' in category `%s' "
					"not added: %s.\n",
					words[3], words[1], msg);
			}
		}
	} else if (streq(words[0], "help")) {
		if (word_count == 1) {
			MR_trace_help();
		} else if (word_count == 2) {
			MR_trace_help_word(words[1]);
		} else if (word_count == 3) {
			MR_trace_help_cat_item(words[1], words[2]);
		} else {
			MR_trace_usage("help", "help");
		}
#ifdef	MR_TRACE_HISTOGRAM
	} else if (streq(words[0], "histogram_all")) {
		if (word_count == 2) {
			FILE	*fp;

			fp = fopen(words[1], "w");
			if (fp == NULL) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"mdb: cannot open file `%s' "
					"for output: %s.\n",
					words[1], strerror(errno));
			} else {
				MR_trace_print_histogram(fp, "All-inclusive",
					MR_trace_histogram_all,
					MR_trace_histogram_hwm);
				if (fclose(fp) != 0) {
					fflush(MR_mdb_out);
					fprintf(MR_mdb_err,
						"mdb: error closing "
						"file `%s': %s.\n",
						words[1], strerror(errno));
				}
			}
		} else {
			MR_trace_usage("exp", "histogram_all");
		}
	} else if (streq(words[0], "histogram_exp")) {
		if (word_count == 2) {
			FILE	*fp;

			fp = fopen(words[1], "w");
			if (fp == NULL) {
				fflush(MR_mdb_out);
				fprintf(MR_mdb_err,
					"mdb: cannot open file `%s' "
					"for output: %s.\n",
					words[1], strerror(errno));
			} else {
				MR_trace_print_histogram(fp, "Experimental",
					MR_trace_histogram_exp,
					MR_trace_histogram_hwm);
				if (fclose(fp) != 0) {
					fflush(MR_mdb_out);
					fprintf(MR_mdb_err,
						"mdb: error closing "
						"file `%s': %s.\n",
						words[1], strerror(errno));
				}
			}
		} else {
			MR_trace_usage("exp", "histogram_exp");
		}
	} else if (streq(words[0], "clear_histogram")) {
		if (word_count == 1) {
			int i;
			for (i = 0; i <= MR_trace_histogram_hwm; i++) {
				MR_trace_histogram_exp[i] = 0;
			}
		} else {
			MR_trace_usage("exp", "clear_histogram");
		}
#endif	/* MR_TRACE_HISTOGRAM */
	} else if (streq(words[0], "nondet_stack")) {
		if (word_count == 1) {
			do_init_modules();
			MR_dump_nondet_stack_from_layout(MR_mdb_out,
				MR_saved_maxfr(saved_regs));
		} else {
			MR_trace_usage("developer", "nondet_stack");
		}
	} else if (streq(words[0], "stack_regs")) {
		if (word_count == 1) {
			fprintf(MR_mdb_out, "sp = ");
			MR_print_detstackptr(MR_mdb_out,
				MR_saved_sp(saved_regs));
			fprintf(MR_mdb_out, ", curfr = ");
			MR_print_nondstackptr(MR_mdb_out,
				MR_saved_curfr(saved_regs));
			fprintf(MR_mdb_out, ", maxfr = ");
			MR_print_nondstackptr(MR_mdb_out,
				MR_saved_maxfr(saved_regs));
			fprintf(MR_mdb_out, "\n");
		} else {
			MR_trace_usage("developer", "stack_regs");
		}
	} else if (streq(words[0], "source")) {
		if (word_count == 2) {
			/*
			** If the source fails, the error message
			** will have already been printed by MR_trace_source.
			*/
			(void) MR_trace_source(words[1]);
		} else {
			MR_trace_usage("misc", "source");
		}
	} else if (streq(words[0], "quit")) {
		bool	confirmed;

		confirmed = FALSE;
		if (! MR_trace_options_confirmed(&confirmed,
				&words, &word_count, "misc", "quit"))
		{
			; /* the usage message has already been printed */
		} else if (word_count == 1) {
			if (! confirmed) {
				char	*line2;

				line2 = MR_trace_getline("mdb: "
					"are you sure you want to quit? ");
				if (line2 == NULL) {
					/* This means the user input EOF. */
					confirmed = TRUE;
				} else {
					int i = 0;
					while (line2[i] != '\0' &&
							MR_isspace(line2[i]))
					{
						i++;
					}

					if (line2[i] == 'y' || line2[i] == 'Y')
					{
						confirmed = TRUE;
					}

					free(line2);
				}
			}

			if (confirmed) {
				exit(EXIT_SUCCESS);
			}
		} else {
			MR_trace_usage("misc", "quit");
		}
#ifdef	MR_USE_DECLARATIVE_DEBUGGER
        } else if (streq(words[0], "dd_wrong")) {
		if (word_count != 1) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: dd_wrong requires no arguments.\n");
		} else if (port != MR_PORT_EXIT) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: wrong answer analysis is only "
				"available from EXIT events.\n");
		} else if (MR_trace_start_wrong_answer(cmd, event_info,
				event_details, jumpaddr)) {
			return STOP_INTERACTING;
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: unable to start declarative "
				"debugging.\n");
		}
#endif  /* MR_USE_DECLARATIVE_DEBUGGER */
	} else {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "Unknown command `%s'. "
			"Give the command `help' for help.\n", words[0]);
	}
	return KEEP_INTERACTING;
}

static struct MR_option MR_trace_strict_print_opts[] =
{
	{ "all",	FALSE,	NULL,	'a' },
	{ "none",	FALSE,	NULL,	'n' },
	{ "some",	FALSE,	NULL,	's' },
	{ "nostrict",	FALSE,	NULL,	'N' },
	{ "strict",	FALSE,	NULL,	'S' },
	{ NULL,		FALSE,	NULL,	0 }
};

static bool
MR_trace_options_strict_print(MR_Trace_Cmd_Info *cmd,
	char ***words, int *word_count, const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "NSans",
			MR_trace_strict_print_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'N':
				cmd->MR_trace_strict = FALSE;
				break;

			case 'S':
				cmd->MR_trace_strict = TRUE;
				break;

			case 'a':
				cmd->MR_trace_print_level = MR_PRINT_LEVEL_ALL;
				break;

			case 'n':
				cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;
				break;

			case 's':
				cmd->MR_trace_print_level = MR_PRINT_LEVEL_SOME;
				break;

			default:
				MR_trace_usage(cat, item);
				return FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return TRUE;
}

static struct MR_option MR_trace_when_action_opts[] =
{
	{ "all",	FALSE,	NULL,	'a' },
	{ "entry",	FALSE,	NULL,	'e' },
	{ "interface",	FALSE,	NULL,	'i' },
	{ "print",	FALSE,	NULL,	'P' },
	{ "stop",	FALSE,	NULL,	'S' },
	{ NULL,		FALSE,	NULL,	0 }
};

static bool
MR_trace_options_when_action(MR_Spy_When *when, MR_Spy_Action *action,
	char ***words, int *word_count, const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "PSaei",
			MR_trace_when_action_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'a':
				*when = MR_SPY_ALL;
				break;

			case 'e':
				*when = MR_SPY_ENTRY;
				break;

			case 'i':
				*when = MR_SPY_INTERFACE;
				break;

			case 'P':
				*action = MR_SPY_PRINT;
				break;

			case 'S':
				*action = MR_SPY_STOP;
				break;

			default:
				MR_trace_usage(cat, item);
				return FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return TRUE;
}

static struct MR_option MR_trace_detailed_opts[] =
{
	{ "detailed",	FALSE,	NULL,	'd' },
	{ NULL,		FALSE,	NULL,	0 }
};

static bool
MR_trace_options_detailed(bool *detailed, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "d",
			MR_trace_detailed_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'd':
				*detailed = TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return TRUE;
}

static bool
MR_trace_options_confirmed(bool *confirmed, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt(*word_count, *words, "NYny")) != EOF) {
		switch (c) {

			case 'n':
			case 'N':
				*confirmed = FALSE;
				break;

			case 'y':
			case 'Y':
				*confirmed = TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return TRUE;
}

static struct MR_option MR_trace_quiet_opts[] =
{
	{ "quiet",	FALSE,	NULL,	'q' },
	{ "verbose",	FALSE,	NULL,	'v' },
	{ NULL,		FALSE,	NULL,	0 }
};

static bool
MR_trace_options_quiet(bool *verbose, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "qv",
			MR_trace_quiet_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'q':
				*verbose = FALSE;
				break;

			case 'v':
				*verbose = TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return TRUE;
}

static void
MR_trace_usage(const char *cat, const char *item)
/* cat is unused now, but could be used later */
{
	fflush(MR_mdb_out);
	fprintf(MR_mdb_err,
		"mdb: %s: usage error -- type `help %s' for help.\n",
		item, item);
}


static void
MR_trace_internal_add_spy_point(MR_Spy_When when, MR_Spy_Action action,
	const MR_Stack_Layout_Entry *entry, const MR_Stack_Layout_Label *label)
{
	MR_Spy_Point	*spy_point;

	MR_ensure_room_for_next(MR_spy_point, MR_Spy_Point *,
		MR_INIT_SPY_POINTS);
	spy_point = MR_add_spy_point(when, action, entry, label);
	MR_spy_points[MR_spy_point_next] = spy_point;
	MR_print_spy_point(MR_spy_point_next);
	MR_spy_point_next++;
}

static void
MR_print_spy_point(int spy_point_num)
{
	fprintf(MR_mdb_out, "%2d: %1s %-5s %9s ",
		spy_point_num,
		MR_spy_points[spy_point_num]->spy_enabled ? "+" : "-",
		MR_spy_action_string(MR_spy_points[spy_point_num]->spy_action),
		MR_spy_when_string(MR_spy_points[spy_point_num]->spy_when));
	MR_print_proc_id(MR_mdb_out, MR_spy_points[spy_point_num]->spy_proc,
		NULL, NULL, NULL);
}

static void
MR_trace_list_vars(const MR_Stack_Layout_Label *top_layout, Word *saved_regs,
	int ancestor_level)
{
	const MR_Stack_Layout_Label	*level_layout;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	int				var_count;
	const MR_Stack_Layout_Vars	*vars;
	int				i;
	const char 			*problem;

	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
				&base_sp, &base_curfr, &problem);

	if (level_layout == NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s\n", problem);
		return;
	}

	problem = MR_trace_validate_var_count(level_layout, &var_count);
	if (problem != NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		return;
	}

	vars = &level_layout->MR_sll_var_info;
	for (i = 0; i < var_count; i++) {
		fprintf(MR_mdb_out, "%9d %s\n",
			i, MR_name_if_present(vars, i));
	}
}

static void
MR_trace_set_level(const MR_Stack_Layout_Label *layout,
	Word *saved_regs, int *ancestor_level, int new_ancestor_level)
{
	const char *problem;
	
	problem = MR_trace_browse_check_level(layout, saved_regs,
		new_ancestor_level);
	if (problem == NULL) {
		*ancestor_level = new_ancestor_level;
		fprintf(MR_mdb_out, "Ancestor level set to %d.\n",
			*ancestor_level);
	} else {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s.\n", problem);
	}
}

static const char *
MR_trace_browse_check_level(const MR_Stack_Layout_Label *top_layout,
	Word *saved_regs, int ancestor_level)
{
	Word				*base_sp;
	Word				*base_curfr;
	const char 			*problem;
	const MR_Stack_Layout_Label	*label_layout;
	const MR_Stack_Layout_Entry	*entry;

	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	label_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
			&base_sp, &base_curfr, &problem);

	if (label_layout == NULL) {
		return problem;
	} else {
		entry = label_layout->MR_sll_entry;
		fprintf(MR_mdb_out, "%4d ", ancestor_level);
		MR_print_proc_id(MR_mdb_out, entry, "", base_sp, base_curfr);
		if (MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
			return NULL;
		} else {
			return "this procedure does not have debugging info";
		}
	}
}

static void
MR_trace_browse_one(const MR_Stack_Layout_Label *top_layout,
	Word *saved_regs, int ancestor_level, MR_Var_Spec var_spec,
	bool browse)
{
	const MR_Stack_Layout_Label	*level_layout;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	Word				*valid_saved_regs;
	int				which_var;
	const MR_Stack_Layout_Vars	*vars;
	const char 			*problem;

	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
				&base_sp, &base_curfr, &problem);

	if (level_layout == NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		return;
	}

	problem = MR_trace_find_var(level_layout, var_spec, &which_var);
	if (problem != NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		return;
	}

	if (ancestor_level == 0) {
		valid_saved_regs = saved_regs;
	} else {
		valid_saved_regs = NULL;
	}

	vars = &level_layout->MR_sll_var_info;
	type_params = MR_materialize_typeinfos_base(vars,
				valid_saved_regs, base_sp, base_curfr);
	MR_trace_browse_var(MR_name_if_present(vars, which_var),
		&vars->MR_slvs_pairs[which_var], valid_saved_regs,
		base_sp, base_curfr, type_params, browse);
	free(type_params);
}

static void 
MR_trace_browse_all(const MR_Stack_Layout_Label *top_layout,
	Word *saved_regs, int ancestor_level)
{
	const MR_Stack_Layout_Label	*level_layout;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	Word				*valid_saved_regs;
	int				var_count;
	const MR_Stack_Layout_Vars	*vars;
	const char 			*problem;
	int				i;

	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	level_layout = MR_find_nth_ancestor(top_layout, ancestor_level,
				&base_sp, &base_curfr, &problem);

	if (level_layout == NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		return;
	}

	problem = MR_trace_validate_var_count(level_layout, &var_count);
	if (problem != NULL) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		return;
	}

	vars = &level_layout->MR_sll_var_info;
	if (ancestor_level == 0) {
		valid_saved_regs = saved_regs;
	} else {
		valid_saved_regs = NULL;
	}

	type_params = MR_materialize_typeinfos_base(vars,
				valid_saved_regs, base_sp, base_curfr);

	for (i = 0; i < var_count; i++) {
		MR_trace_browse_var(MR_name_if_present(vars, i),
			&vars->MR_slvs_pairs[i], valid_saved_regs,
			base_sp, base_curfr, type_params, FALSE);
	}

	free(type_params);
}

static void
MR_trace_browse_var(const char *name, const MR_Stack_Layout_Var *var,
	Word *saved_regs, Word *base_sp, Word *base_curfr, Word *type_params,
	bool browse)
{
	Word	value;
	Word	type_info;
	bool	print_value;
	int	i;

	/*
	** XXX The printing of type_infos is buggy at the moment
	** due to the fake arity of the type private_builtin:typeinfo/1.
	*/

	if ((strncmp(name, "TypeInfo", 8) == 0)
	|| (strncmp(name, "TypeClassInfo", 13) == 0))
		return;

	/* The initial blanks are to visually separate */
	/* the variable names from the prompt. */

	if (!browse) {
		if (name != NULL) {
			fprintf(MR_mdb_out, "%7s%-21s\t", "", name);
		} else {
			fprintf(MR_mdb_out, "%7s%-21s\t", "",
				"anonymous variable");
		}

		fflush(MR_mdb_out);
	}


	/*
	** "variables" representing the saved values of succip, hp etc,
	** which are the "variables" for which get_type_and_value fails,
	** are not of interest to the user.
	*/

	print_value = MR_get_type_and_value_base(var, saved_regs,
			base_sp, base_curfr, type_params, &type_info, &value);
	if (print_value) {
		if (browse) {
			/* XXX should use MR_mdb_in and MR_mdb_out */
			MR_trace_browse(type_info, value);
		} else {
			fprintf(MR_mdb_out, "\t");
			fflush(MR_mdb_out);
			/* XXX should use MR_mdb_out */
			MR_trace_print(type_info, value);
		}
	}
}

static const char *
MR_trace_validate_var_count(const MR_Stack_Layout_Label *layout,
	int *var_count_ptr)
{
	*var_count_ptr = (int) layout->MR_sll_var_count;
	if (*var_count_ptr < 0) {
		return "there is no information about live variables";
	} else if (*var_count_ptr == 0) {
		return "there are no live variables";
	} else {
		return NULL;
	}
}

/*
** Find and validate the number of a variable given by a variable
** specification in the given layout. If successful, return the
** number of the variable in *which_var_ptr, and a NULL string;
** otherwise return a string containing an error message.
*/

static const char *
MR_trace_find_var(const MR_Stack_Layout_Label *layout,
	MR_Var_Spec var_spec, int *which_var_ptr)
{
	int		var_count;
	const char 	*problem;

	problem = MR_trace_validate_var_count(layout, &var_count);
	if (problem != NULL) {
		return problem;
	}

	if (var_spec.MR_var_spec_kind == VAR_NUMBER) {
		*which_var_ptr = var_spec.MR_var_spec_number;
		if (*which_var_ptr >= var_count) {
			return "there is no such variable";
		} else {
			return NULL;	/* represents success */
		}
	} else if (var_spec.MR_var_spec_kind == VAR_NAME) {
		const MR_Stack_Layout_Vars	*vars;
		const char 			*name;
		bool				collision = FALSE;
		int				i;

		vars = &layout->MR_sll_var_info;
		*which_var_ptr = -1;
		name = var_spec.MR_var_spec_name;
		for (i = 0; i < var_count; i++) {
			if (streq(name, MR_name_if_present(vars, i))) {
				if (*which_var_ptr >= 0) {
					collision = TRUE;
				}

				*which_var_ptr = i;
			}
		}

		if (*which_var_ptr < 0) {
			return "there is no variable with that name";
		} else if (collision) {
			return "variable name is not unique";
		} else {
			return NULL;	/* represents success */
		}
	} else {
		return "internal error: bad var_spec kind";
	}
}

/*
** Read lines until we find one that contains only "end".
** Return the lines concatenated together.
*/

static const char *
MR_trace_read_help_text(void)
{
	char	*text;
	char	*doc_chars = NULL;
	int	doc_char_max = 0;
	int	next_char_slot;
	int	line_len;
	int	i;

	next_char_slot = 0;
	while ((text = MR_trace_getline("cat> ")) != NULL) {
		if (streq(text, "end")) {
			free(text);
			break;
		}

		line_len = strlen(text);
		MR_ensure_big_enough(next_char_slot + line_len + 2,
			doc_char, char, MR_INIT_DOC_CHARS);
		for (i = 0; i < line_len; i++) {
			doc_chars[next_char_slot + i] = text[i];
		}

		next_char_slot += line_len;
		doc_chars[next_char_slot] = '\n';
		next_char_slot += 1;
		free(text);
	}

	doc_chars[next_char_slot] = '\0';
	return doc_chars;
}

/*
** Is the string pointed to by word a natural number,
** i.e. a sequence of digits?
** If yes, return its value in *value.
*/

static bool
MR_trace_is_number(const char *word, int *value)
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
** separated by space characters. Make each word a NULL-terminated string,
** overwriting some spaces in the line array in the process.
**
** If the first word is a number but the second is not, swap the two.
** If the first word has a number prefix, separate it out.
**
** On return *words will point to an array of strings, with space for
** *words_max strings. The number of strings (words) filled in will be
** given by *word_count.
**
** The lifetime of the elements of the *words array expires when
** the line array is freed or further modified or when MR_trace_parse_line
** is called again, whichever comes first.
**
** The return value is NULL if everything went OK, and an error message
** otherwise.
*/

static const char *
MR_trace_parse_line(char *line, char ***words, int *word_max, int *word_count)
{
	char		**raw_words;
	int		raw_word_max;
	char		raw_word_count;
	static char	count_buf[MR_NUMBER_LEN];
	char		*s;
	int		i;

	/*
	** Handle a possible number prefix on the first word on the line,
	** separating it out into a word on its own.
	*/

	raw_word_count = MR_trace_break_into_words(line,
				&raw_words, &raw_word_max);

	if (raw_word_count > 0 && MR_isdigit(*raw_words[0])) {
		i = 0;
		s = raw_words[0];
		while (MR_isdigit(*s)) {
			if (i >= MR_NUMBER_LEN) {
				return "too large a number";
			}

			count_buf[i] = *s;
			i++;
			s++;
		}

		count_buf[i] = '\0';

		if (*s != '\0') {
			/* Only part of the first word constitutes a number. */
			/* Put it in an extra word at the start. */
			MR_ensure_big_enough(raw_word_count, raw_word,
				char **, MR_INIT_WORD_COUNT);

			for (i = raw_word_count; i > 0; i--) {
				raw_words[i] = raw_words[i-1];
			}

			raw_words[0] = count_buf;
			raw_words[1] = s;
			raw_word_count++;
		}
	}

	/*
	** If the first word is a number, try to exchange it
	** with the command word, to put the command word first.
	*/

	if (raw_word_count > 1 && MR_trace_is_number(raw_words[0], &i)
			&& ! MR_trace_is_number(raw_words[1], &i)) {
		s = raw_words[0];
		raw_words[0] = raw_words[1];
		raw_words[1] = s;
	}

	*words = raw_words;
	*word_max = raw_word_max;
	*word_count = raw_word_count;
	return NULL;
}

/*
** Given a text line, break it up into words composed of non-space characters
** separated by space characters. Make each word a NULL-terminated string,
** overwriting some spaces in the line array in the process.
**
** On return *words will point to an array of strings, with space for
** *words_max strings. The number of strings filled in will be given by
** the return value.
*/

static int
MR_trace_break_into_words(char *line, char ***words_ptr, int *word_max_ptr)
{
	int	word_max;
	char	**words;
	int	token_number;
	int	char_pos;
	int	int_val;

	token_number = 0;
	char_pos = 0;

	word_max = 0;
	words = NULL;

	/* each iteration of this loop processes one token, or end of line */
	for (;;) {
		while (line[char_pos] != '\0' && MR_isspace(line[char_pos])) {
			char_pos++;
		}

		if (line[char_pos] == '\0') {
			*words_ptr = words;
			*word_max_ptr = word_max;
			return token_number;
		}

		MR_ensure_big_enough(token_number, word, char **,
			MR_INIT_WORD_COUNT);
		words[token_number] = line + char_pos;

		while (line[char_pos] != '\0' && !MR_isspace(line[char_pos])) {
			char_pos++;
		}

		if (line[char_pos] != '\0') {
			line[char_pos] = '\0';
			char_pos++;
		}

		token_number++;
	}
}

static void
MR_trace_expand_aliases(char ***words, int *word_max, int *word_count)
{
	const char	*alias_key;
	char		**alias_words;
	int		alias_word_count;
	int		alias_copy_start;
	int		i;
	int		n;

	if (*word_count == 0) {
		alias_key = "EMPTY";
		alias_copy_start = 0;
	} else if (MR_trace_is_number(*words[0], &n)) {
		alias_key = "NUMBER";
		alias_copy_start = 0;
	} else {
		alias_key = *words[0];
		alias_copy_start = 1;
	}

	if (MR_trace_lookup_alias(alias_key, &alias_words, &alias_word_count))
	{
		MR_ensure_big_enough(*word_count + alias_word_count,
			*word, const char *, MR_INIT_WORD_COUNT);

		/* Move the original words (except the alias key) up. */
		for (i = *word_count - 1; i >= alias_copy_start; i--) {
			(*words)[i + alias_word_count - alias_copy_start]
				= (*words)[i];
		}

		/* Move the alias body to the words array. */
		for (i = 0; i < alias_word_count; i++) {
			(*words)[i] = alias_words[i];
		}

		*word_count += alias_word_count - alias_copy_start;
	}
}

static bool
MR_trace_source(const char *filename)
{
	FILE	*fp;

	if ((fp = fopen(filename, "r")) != NULL) {
		MR_trace_source_from_open_file(fp);
		fclose(fp);
		return TRUE;
	} else {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s: %s.\n", filename, strerror(errno));
		return FALSE;
	}
}

static void
MR_trace_source_from_open_file(FILE *fp)
{
	char	*line;

	while ((line = MR_trace_readline_raw(fp)) != NULL) {
		MR_insert_line_at_tail(line);
	}

	MR_trace_internal_interacting = FALSE;
}

/*
** If there any lines waiting in the queue, return the first of these.
** If not, print the prompt to MR_mdb_out, read a line from MR_mdb_in,
** and return it in a malloc'd buffer holding the line (without the final
** newline).
** If EOF occurs on a nonempty line, treat the EOF as a newline; if EOF
** occurs on an empty line, return NULL.
*/

static char *
MR_trace_getline(const char *prompt)
{
	char	*line;

	line = MR_trace_getline_queue();
	if (line != NULL) {
		return line;
	}

	MR_trace_internal_interacting = TRUE;

	return MR_trace_readline(prompt, MR_mdb_in, MR_mdb_out);
}

/*
** If there any lines waiting in the queue, return the first of these.
*/

static char *
MR_trace_getline_queue(void)
{
	if (MR_line_head != NULL) {
		MR_Line	*old;
		char	*contents;

		old = MR_line_head;
		contents = MR_line_head->MR_line_contents;
		MR_line_head = MR_line_head->MR_line_next;
		if (MR_line_head == NULL) {
			MR_line_tail = NULL;
		}

		free(old);
		return contents;
	} else {
		return NULL;
	}
}

static void
MR_insert_line_at_head(const char *contents)
{
	MR_Line	*line;

	line = checked_malloc(sizeof(MR_Line));
	line->MR_line_contents = MR_copy_string(contents);
	line->MR_line_next = MR_line_head;

	MR_line_head = line;
	if (MR_line_tail == NULL) {
		MR_line_tail = MR_line_head;
	}
}

static void
MR_insert_line_at_tail(const char *contents)
{
	MR_Line	*line;
	char	*copy;
	int	len;

	len = strlen(contents);
	copy = checked_malloc(len + 1);
	strcpy(copy, contents);

	line = checked_malloc(sizeof(MR_Line));
	line->MR_line_contents = copy;
	line->MR_line_next = NULL;

	if (MR_line_tail == NULL) {
		MR_line_tail = line;
		MR_line_head = line;
	} else {
		MR_line_tail->MR_line_next = line;
		MR_line_tail = line;
	}
}

Code *
MR_trace_event_internal_report(MR_Trace_Cmd_Info *cmd,
		MR_Event_Info *event_info)
{
	char	*buf;
	int	i;

	/* We try to leave one line for the prompt itself. */
	if (MR_scroll_control && MR_scroll_next >= MR_scroll_limit - 1) {
	try_again:
		buf = MR_trace_getline("--more-- ");
		if (buf != NULL) {
			for (i = 0; buf[i] != '\0' && MR_isspace(buf[i]); i++)
				;
			
			if (buf[i] != '\0' && !MR_isspace(buf[i])) {
				switch (buf[i]) {
					case 'a':
						cmd->MR_trace_print_level =
							MR_PRINT_LEVEL_ALL;
						break;

					case 'n':
						cmd->MR_trace_print_level =
							MR_PRINT_LEVEL_NONE;
						break;

					case 's':
						cmd->MR_trace_print_level =
							MR_PRINT_LEVEL_SOME;
						break;

					case 'q':
						free(buf);
						return MR_trace_event_internal(
								cmd, TRUE,
								event_info);

					default:
						fflush(MR_mdb_out);
						fprintf(MR_mdb_err,
							"unknown command, "
							"try again\n");
						free(buf);
						goto try_again;
				}
			}

			free(buf);
		}

		MR_scroll_next = 0;
	}

	MR_trace_event_print_internal_report(event_info);
	MR_scroll_next++;

	return NULL;
}

void
MR_trace_event_print_internal_report(MR_Event_Info *event_info)
{
	fprintf(MR_mdb_out, "%8ld: %6ld %2ld ",
		(long) event_info->MR_event_number, 
		(long) event_info->MR_call_seqno,
		(long) event_info->MR_call_depth);

	MR_trace_print_port(event_info->MR_trace_port);
	MR_print_proc_id(MR_mdb_out, event_info->MR_event_sll->MR_sll_entry,
			event_info->MR_event_path, NULL, NULL);
}

static void
MR_trace_print_port(MR_Trace_Port port)
{
	const char *port_name;
	switch (port) {
		case MR_PORT_CALL:
			port_name = "CALL";
			break;

		case MR_PORT_EXIT:
			port_name = "EXIT";
			break;

		case MR_PORT_REDO:
			port_name = "REDO";
			break;

		case MR_PORT_FAIL:
			port_name = "FAIL";
			break;

		case MR_PORT_THEN:
			port_name = "THEN";
			break;

		case MR_PORT_ELSE:
			port_name = "ELSE";
			break;

		case MR_PORT_DISJ:
			port_name = "DISJ";
			break;

		case MR_PORT_SWITCH:
			port_name = "SWTC";
			break;

		case MR_PORT_PRAGMA_FIRST:
			port_name = "FRST";
			break;

		case MR_PORT_PRAGMA_LATER:
			port_name = "LATR";
			break;

		case MR_PORT_EXCEPTION:
			port_name = "EXCP";
			break;

		default:
			port_name = "????";
			fatal_error("MR_trace_event_internal called "
					"with bad port");
	}
	fprintf(MR_mdb_out, "%s ", port_name);
}

typedef struct
{
	const char	*cat;
	const char	*item;
} MR_trace_cmd_cat_item;

static	MR_trace_cmd_cat_item MR_trace_valid_command_list[] =
{
	/*
	** The following block is mostly a verbatim copy of the file
	** doc/mdb_command_list. We do not use a #include to avoid
	** adding a dependency, and because we want to #ifdef the
	** experimental commands.
	*/

	{ "queries", "query" },
	{ "queries", "cc_query" },
	{ "queries", "io_query" },
	{ "forward", "step" },
	{ "forward", "goto" },
	{ "forward", "finish" },
	{ "forward", "return" },
	{ "forward", "forward" },
	{ "forward", "mindepth" },
	{ "forward", "maxdepth" },
	{ "forward", "continue" },
	{ "backward", "retry" },
	{ "browsing", "vars" },
	{ "browsing", "print" },
	{ "browsing", "browse" },
	{ "browsing", "stack" },
	{ "browsing", "up" },
	{ "browsing", "down" },
	{ "browsing", "level" },
	{ "browsing", "current" },
	{ "breakpoint", "break" },
	{ "breakpoint", "disable" },
	{ "breakpoint", "enable" },
	{ "breakpoint", "modules" },
	{ "breakpoint", "procedures" },
	{ "breakpoint", "register" },
	{ "parameter", "mmc_options" },
	{ "parameter", "printlevel" },
	{ "parameter", "echo" },
	{ "parameter", "scroll" },
	{ "parameter", "alias" },
	{ "parameter", "unalias" },
	{ "help", "document_category" },
	{ "help", "document" },
	{ "help", "help" },
#ifdef	MR_TRACE_HISTOGRAM
	{ "exp", "histogram_all" },
	{ "exp", "histogram_exp" },
	{ "exp", "clear_histogram" },
#endif
	{ "developer", "nondet_stack" },
	{ "developer", "stack_regs" },
	{ "misc", "source" },
	{ "misc", "quit" },
	/* End of doc/mdb_command_list. */
	{ NULL, "NUMBER" },
	{ NULL, "EMPTY" },
	{ NULL, NULL },
};

static bool
MR_trace_valid_command(const char *word)
{
	int	i;

	for (i = 0; MR_trace_valid_command_list[i].item != NULL; i++) {
		if (streq(MR_trace_valid_command_list[i].item, word)) {
			return TRUE;
		}
	}

	return FALSE;
}

