/*
** Copyright (C) 1998-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the code of the internal, in-process debugger.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_layout_util.h"
#include "mercury_array_macros.h"
#include "mercury_getopt.h"
#include "mercury_signal.h"

#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_declarative.h"
#include "mercury_trace_alias.h"
#include "mercury_trace_help.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"
#include "mercury_trace_vars.h"
#include "mercury_trace_readline.h"
#include "mercury_trace_source.h"

#include "mdb.browse.mh"
#include "mdb.browser_info.mh"
#include "mdb.program_representation.mh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#ifdef MR_HAVE_SYS_TYPES_H
  #include <sys/types.h>
#endif

#ifdef MR_HAVE_SYS_WAIT_H
  #include <sys/wait.h>
#endif

#ifdef MR_HAVE_TERMIOS_H
  #include <termios.h>
#endif

#ifdef MR_HAVE_FCNTL_H
  #include <fcntl.h>
#endif

#ifdef MR_HAVE_SYS_IOCTL_H
  #include <sys/ioctl.h>
#endif

#ifdef MR_HAVE_SYS_STROPTS_H
  #include <sys/stropts.h>
#endif

/* Special characters used in mdb commands. */
#define MR_MDB_QUOTE_CHAR	'\''
#define MR_MDB_ESCAPE_CHAR	'\\'

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
** The distinction between MR_mdb_out and MR_mdb_err is analogous to
** the distinction between stdout and stderr: ordinary output, including
** information messages about conditions which are not errors, should
** go to MR_mdb_out, but error messages should go to MR_mdb_err.
**
** Note that MR_mdb_out and MR_mdb_err may both write to the same
** file, so we need to be careful to ensure that buffering does
** not stuff up the interleaving of error messages and ordinary output.
** To ensure this, we do two things:
**
**	- MR_mdb_err is unbuffered
**	- we always fflush(MR_mdb_out) before writing to MR_mdb_err
*/

FILE	*MR_mdb_in;
FILE	*MR_mdb_out;
FILE	*MR_mdb_err;

static	MR_Trace_Print_Level	MR_default_print_level = MR_PRINT_LEVEL_SOME;

/*
** These variables say (a) whether the printing of event sequences will pause
** after each screenful of events, (b) how may events constitute a screenful
** (although we count only events, not how many lines they take up), and (c)
** how many events we have printed so far in this screenful.
*/

static	MR_bool			MR_scroll_control = MR_TRUE;
static	int			MR_scroll_limit = 24;
static	int			MR_scroll_next = 0;

/*
** We echo each command just as it is executed iff this variable is MR_TRUE.
*/

static	MR_bool			MR_echo_commands = MR_FALSE;

/*
** The details of the source server, if any.
*/
static	MR_Trace_Source_Server	MR_trace_source_server = { NULL, NULL };

/*
** We print confirmation of commands (e.g. new aliases) if this is MR_TRUE.
*/

static	MR_bool			MR_trace_internal_interacting = MR_FALSE;

static	MR_bool			MR_saved_io_tabling_enabled;

/*
** We include values of sometimes-useful types such as typeinfos in the set of
** variables whose values we collect at events for possible later printing
** only if MR_print_optionals is true.
*/

static	MR_bool			MR_print_optionals = MR_FALSE;

/*
** MR_context_position specifies whether we print context at events,
** and if so, where.
*/

static	MR_Context_Position	MR_context_position = MR_CONTEXT_AFTER;

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

static const char	*MR_context_set_msg[] = {
	"Contexts will not be printed.",
	"Contexts will be printed before, on the same line.",
	"Contexts will be printed after, on the same line.",
	"Contexts will be printed on the previous line.",
	"Contexts will be printed on the next line.",
};

static const char	*MR_context_report_msg[] = {
	"Contexts are not printed.",
	"Contexts are printed before, on the same line.",
	"Contexts are printed after, on the same line.",
	"Contexts are printed on the previous line.",
	"Contexts are printed on the next line.",
};

static	MR_Spy_When		MR_default_breakpoint_scope = MR_SPY_INTERFACE;

static const char	*MR_scope_set_msg[] = {
	"The default scope of `break' commands is now all matching events.",
	"The default scope of `break' commands is now all matching interface events.",
	"The default scope of `break' commands is now all matching entry events.",
	"MDB INTERNAL ERROR: scope set to MR_SPY_SPECIFIC",
	"MDB INTERNAL ERROR: scope set to MR_SPY_LINENO",
};

static const char	*MR_scope_report_msg[] = {
	"The default scope of `break' commands is all matching events.",
	"The default scope of `break' commands is all matching interface events.",
	"The default scope of `break' commands is all matching entry events.",
	"MDB INTERNAL ERROR: scope set to MR_SPY_SPECIFIC",
	"MDB INTERNAL ERROR: scope set to MR_SPY_LINENO",
};

#ifdef	MR_USE_DECLARATIVE_DEBUGGER

MR_Trace_Mode MR_trace_decl_mode = MR_TRACE_INTERACTIVE;

#endif	/* MR_USE_DECLARATIVE_DEBUGGER */

typedef enum {
	MR_MULTIMATCH_ASK, MR_MULTIMATCH_ALL, MR_MULTIMATCH_ONE
} MR_MultiMatch;

/*
** We keep a table of the available commands. The information we have about
** each command is stored in a value of type MR_Trace_Command_Info.
**
** The name of the command itself is stored in the name field; the category
** field contains name of the category to which the command belongs,
** e.g. "browsing".
**
** The code that the command loop should execute to handle a command of a given
** type is the function pointed to by the function field.
**
** Some commands take fixed strings as arguments. The arg_strings field
** is a NULL terminated array of those strings, or NULL if there are
** no fixed strings.
**
** The arg_completer field contains the address of a function for more
** arbitrary completion, e.g. on predicate names. This field should not be
** null; if the command cannot use a completion function, the field should
** contain MR_trace_null_completer.
*/

typedef MR_Next MR_Trace_Command_Function(char **words, int word_count,
	MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
	MR_Event_Details *event_details, MR_Code **jumpaddr);

typedef struct
{
	const char			*MR_cmd_category;
	const char			*MR_cmd_name;
	MR_Trace_Command_Function 	*MR_cmd_function;
	const char *const		*MR_cmd_arg_strings;
	const MR_Make_Completer		MR_cmd_arg_completer;
} MR_Trace_Command_Info;

static	void	MR_trace_internal_ensure_init(void);
static	MR_bool	MR_trace_internal_create_mdb_window(void);
static	void	MR_trace_internal_init_from_env(void);
static	void	MR_trace_internal_init_from_local(void);
static	void	MR_trace_internal_init_from_home_dir(void);
static	MR_Next	MR_trace_debug_cmd(char *line, MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static	MR_Next	MR_trace_handle_cmd(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);

static MR_Next	MR_trace_cmd_step(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_goto(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_next(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_finish(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_fail(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_exception(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_return(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_forward(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_mindepth(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_maxdepth(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_continue(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_retry(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_level(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_up(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_down(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_vars(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_print(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_browse(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_stack(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_current(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_set(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_view(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_break(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_ignore(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_enable(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_disable(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_delete(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_register(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_modules(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_procedures(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_query(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_cc_query(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_io_query(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_printlevel(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_mmc_options(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_scroll(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_context(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_scope(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_echo(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_alias(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_unalias(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_document_category(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_document(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_help(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);

#ifdef MR_TRACE_HISTORGRAM
static MR_Next	MR_trace_cmd_histogram_all(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_histogram_exp(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_clear_histogram(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
#endif	/* MR_TRACE_HISTORGRAM */

#ifdef MR_USE_MINIMAL_MODEL
static MR_Next	MR_trace_cmd_gen_stack(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
#endif /* MR_USE_MINIMAL_MODEL */

static MR_Next	MR_trace_cmd_nondet_stack(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_stack_regs(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_all_regs(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_table_io(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_proc_stats(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_label_stats(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_proc_body(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_print_optionals(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_save(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_quit(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);

#ifdef MR_USE_DECLARATIVE_DEBUGGER
static MR_Next	MR_trace_cmd_dd(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
static MR_Next	MR_trace_cmd_dd_dd(char **words, int word_count,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);
#endif /* MR_USE_DECLARATIVE_DEBUGGER */

static	void	MR_maybe_print_spy_point(int slot, const char *problem);
static	void	MR_print_unsigned_var(FILE *fp, const char *var,
			MR_Unsigned value);
static	MR_bool	MR_parse_source_locn(char *word, const char **file, int *line);
static	const char *MR_trace_new_source_window(const char *window_cmd,
			const char *server_cmd, const char *server_name,
			int timeout, MR_bool force, MR_bool verbose,
			MR_bool split);
static	void	MR_trace_maybe_sync_source_window(MR_Event_Info *event_info,
			MR_bool verbose);
static	void	MR_trace_maybe_close_source_window(MR_bool verbose);
static	MR_bool	MR_trace_options_strict_print(MR_Trace_Cmd_Info *cmd,
			char ***words, int *word_count,
			const char *cat, const char *item);
static	MR_bool	MR_trace_options_retry(MR_bool *allow_io,
			char ***words, int *word_count,
			const char *cat, const char *item);
static	MR_bool	MR_trace_options_when_action_multi_ignore(MR_Spy_When *when,
			MR_Spy_Action *action, MR_MultiMatch *multi_match,
			MR_Spy_Ignore_When *ignore_when, int *ignore_count,
			char ***words, int *word_count,
			const char *cat, const char *item);
static	MR_bool	MR_trace_options_ignore_count(MR_Spy_Ignore_When *ignore_when,
			int *ignore_count, char ***words, int *word_count,
			const char *cat, const char *item);
static	MR_bool	MR_trace_options_quiet(MR_bool *verbose, char ***words,
			int *word_count, const char *cat, const char *item);
static	MR_bool	MR_trace_options_ignore(MR_bool *ignore_errors, char ***words,
			int *word_count, const char *cat, const char *item);
static	MR_bool	MR_trace_options_detailed(MR_bool *detailed, char ***words,
			int *word_count, const char *cat, const char *item);
static	MR_bool	MR_trace_options_confirmed(MR_bool *confirmed, char ***words,
			int *word_count, const char *cat, const char *item);
static	MR_bool	MR_trace_options_format(MR_Browse_Format *format,
			char ***words, int *word_count, const char *cat,
			const char *item);
static	MR_bool	MR_trace_options_param_set(MR_Word *print_set,
			MR_Word *browse_set, MR_Word *print_all_set,
			MR_Word *flat_format, MR_Word *raw_pretty_format,
			MR_Word *verbose_format, MR_Word *pretty_format, 
			char ***words, int *word_count, const char *cat, 
			const char *item);
static	MR_bool	MR_trace_options_view(const char **window_cmd,
			const char **server_cmd, const char **server_name,
			int *timeout, MR_bool *force, MR_bool *verbose,
			MR_bool *split, MR_bool *close_window, char ***words,
			int *word_count, const char *cat, const char*item);
static	void	MR_trace_usage(const char *cat, const char *item);
static	void	MR_trace_do_noop(void);

static	void	MR_trace_set_level_and_report(int ancestor_level,
			MR_bool detailed, MR_bool print_optionals);
static	void	MR_trace_browse_internal(MR_Word type_info, MR_Word value,
			MR_Browse_Caller_Type caller, MR_Browse_Format format);
static	void	MR_trace_browse_goal_internal(MR_ConstString name,
			MR_Word arg_list, MR_Word is_func,
			MR_Browse_Caller_Type caller, MR_Browse_Format format);
static	const char *MR_trace_browse_exception(MR_Event_Info *event_info,
			MR_Browser browser, MR_Browse_Caller_Type caller,
			MR_Browse_Format format);

static	const char *MR_trace_read_help_text(void);
static	const char *MR_trace_parse_line(char *line,
			char ***words, int *word_max, int *word_count);
static	int	MR_trace_break_into_words(char *line,
			char ***words_ptr, int *word_max_ptr);
static	int	MR_trace_break_off_one_word(char *line, int char_pos);
static	void	MR_trace_expand_aliases(char ***words,
			int *word_max, int *word_count);
static	MR_bool	MR_trace_source(const char *filename, MR_bool ignore_errors);
static	void	MR_trace_source_from_open_file(FILE *fp);
static	char	*MR_trace_getline_queue(void);
static	MR_bool	MR_trace_continue_line(char *ptr, MR_bool *quoted);
static	void	MR_insert_line_at_head(const char *line);
static	void	MR_insert_line_at_tail(const char *line);

static	void	MR_trace_event_print_internal_report(
			MR_Event_Info *event_info);

static	const MR_Trace_Command_Info	*MR_trace_valid_command(
						const char *command);

static	char	*MR_trace_command_completer_next(const char *word,
			size_t word_len, MR_Completer_Data *data);

MR_Code *
MR_trace_event_internal(MR_Trace_Cmd_Info *cmd, MR_bool interactive,
		MR_Event_Info *event_info)
{
	MR_Code			*jumpaddr;
	char			*line;
	MR_Next			res;
	MR_Event_Details	event_details;
	MR_bool			saved_tabledebug;

	if (! interactive) {
		return MR_trace_event_internal_report(cmd, event_info);
	}

#ifdef	MR_USE_DECLARATIVE_DEBUGGER
	if (MR_trace_decl_mode != MR_TRACE_INTERACTIVE) {
		return MR_trace_decl_debug(cmd, event_info);
	}
#endif	/* MR_USE_DECLARATIVE_DEBUGGER */

	/*
	** We want to make sure that the Mercury code used to implement some
	** of the debugger's commands (a) doesn't generate any trace events,
	** (b) doesn't generate any unwanted debugging output, and (c) doesn't
	** do any I/O tabling.
	*/

	MR_trace_enabled = MR_FALSE;
	saved_tabledebug = MR_tabledebug;
	MR_tabledebug = MR_FALSE;
	MR_saved_io_tabling_enabled = MR_io_tabling_enabled;
	MR_io_tabling_enabled = MR_FALSE;

	MR_trace_internal_ensure_init();

	MR_trace_event_print_internal_report(event_info);
	MR_trace_maybe_sync_source_window(event_info, MR_FALSE);

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

	MR_trace_init_point_vars(event_info->MR_event_sll,
		event_info->MR_saved_regs, event_info->MR_trace_port,
		MR_print_optionals);

	/* by default, return where we came from */
	jumpaddr = NULL;

	do {
		line = MR_trace_get_command("mdb> ", MR_mdb_in, MR_mdb_out);
		res = MR_trace_debug_cmd(line, cmd, event_info, &event_details,
				&jumpaddr);
	} while (res == KEEP_INTERACTING);

	cmd->MR_trace_must_check = (! cmd->MR_trace_strict) ||
			(cmd->MR_trace_print_level != MR_PRINT_LEVEL_NONE);

	MR_trace_call_seqno = event_details.MR_call_seqno;
	MR_trace_call_depth = event_details.MR_call_depth;
	MR_trace_event_number = event_details.MR_event_number;

	MR_scroll_next = 0;
	MR_trace_enabled = MR_TRUE;
	MR_tabledebug = saved_tabledebug;
	MR_io_tabling_enabled = MR_saved_io_tabling_enabled;
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
	static	MR_bool	MR_trace_internal_initialized = MR_FALSE;

	if (! MR_trace_internal_initialized) {
		char	*env;
		int	n;

		if (MR_mdb_in_window) {
			/*
			** If opening the window fails, fall back on
			** using MR_mdb_*_filename, or stdin, stdout
			** and stderr.
			*/
			MR_mdb_in_window =
				MR_trace_internal_create_mdb_window();
			if (! MR_mdb_in_window) {
				MR_mdb_warning(
				"Try `mdb --program-in-window' instead.\n");
			}
		}

		if (! MR_mdb_in_window) {
			MR_mdb_in = MR_try_fopen(MR_mdb_in_filename,
					"r", stdin);
			MR_mdb_out = MR_try_fopen(MR_mdb_out_filename,
					"w", stdout);
			MR_mdb_err = MR_try_fopen(MR_mdb_err_filename,
					"w", stderr);
		}

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

		MR_saved_io_tabling_enabled = MR_TRUE;
		MR_io_tabling_phase = MR_IO_TABLING_BEFORE;
		MR_io_tabling_start = MR_IO_ACTION_MAX;
		MR_io_tabling_end = MR_IO_ACTION_MAX;

		MR_trace_internal_initialized = MR_TRUE;
	}
}

static volatile sig_atomic_t MR_got_alarm = MR_FALSE;

static void
MR_trace_internal_alarm_handler(void)
{
	MR_got_alarm = MR_TRUE;
}

static MR_bool
MR_trace_internal_create_mdb_window(void)
{
	/*
	** XXX The code to find and open a pseudo-terminal is nowhere
	** near as portable as I would like, but given the huge variety
	** of methods for allocating pseudo-terminals it will have to do.
	** Most systems seem to be standardising on this method (from UNIX98).
	** See the xterm or expect source for a more complete version
	** (it's a bit too entwined in the rest of the code to just lift
	** it out and use it here).
	**
	** XXX Add support for MS Windows.
	*/
#if defined(MR_HAVE_OPEN) && defined(O_RDWR) && defined(MR_HAVE_FDOPEN) && \
	defined(MR_HAVE_CLOSE) && defined(MR_HAVE_DUP) && \
	defined(MR_HAVE_DUP2) && defined(MR_HAVE_FORK) && \
	defined(MR_HAVE_EXECLP) && defined(MR_HAVE_DEV_PTMX) && \
	defined(MR_HAVE_GRANTPT) && defined(MR_HAVE_UNLOCKPT) && \
	defined(MR_HAVE_PTSNAME)

	int master_fd = -1;
	int slave_fd = -1;
	char *slave_name;
	pid_t child_pid;
#if defined(MR_HAVE_TERMIOS_H) && defined(MR_HAVE_TCGETATTR) && \
		defined(MR_HAVE_TCSETATTR) && defined(ECHO) && defined(TCSADRAIN)
	struct termios termio;
#endif
	master_fd = open("/dev/ptmx", O_RDWR);
	if (master_fd == -1 || grantpt(master_fd) == -1
			|| unlockpt(master_fd) == -1)
	{
		close(master_fd);
		MR_mdb_perror(
		    "error opening master pseudo-terminal for mdb window");
		return MR_FALSE;
	}
	if ((slave_name = ptsname(master_fd)) == NULL) {
		MR_mdb_perror(
		    "error getting name of pseudo-terminal for mdb window");
		close(master_fd);
		return MR_FALSE;
	}
	slave_fd = open(slave_name, O_RDWR);	
	if (slave_fd == -1) {
		close(master_fd);
		MR_mdb_perror(
		   "opening slave pseudo-terminal for mdb window failed");
		return MR_FALSE;
	}

#if defined(MR_HAVE_IOCTL) && defined(I_PUSH)
	/* Magic STREAMS incantations to make this work on Solaris. */
	ioctl(slave_fd, I_PUSH, "ptem");
	ioctl(slave_fd, I_PUSH, "ldterm");
	ioctl(slave_fd, I_PUSH, "ttcompat");
#endif

#if defined(MR_HAVE_TCGETATTR) && defined(MR_HAVE_TCSETATTR) && \
		defined(ECHO) && defined(TCSADRAIN)
	/*
	** Turn off echoing before starting the xterm so that
	** the user doesn't see the window ID printed by xterm
	** on startup (this behaviour is not documented in the
	** xterm manual).
	*/
	tcgetattr(slave_fd, &termio);
	termio.c_lflag &= ~ECHO;
	tcsetattr(slave_fd, TCSADRAIN, &termio);
#endif

	child_pid = fork();
	if (child_pid == -1) {
		MR_mdb_perror("fork() for mdb window failed"); 
		close(master_fd);
		close(slave_fd);
		return MR_FALSE;
	} else if (child_pid == 0) {
		/*
		** Child - exec() the xterm.
		*/
		char xterm_arg[50];

		close(slave_fd);

#if defined(MR_HAVE_SETPGID)
		/*
		** Put the xterm in a new process group so it won't be
		** killed by SIGINT signals sent to the program.
		*/
		if (setpgid(0, 0) < 0) {
			MR_mdb_perror("setpgid() failed");
			close(master_fd);
			exit(EXIT_FAILURE);
		}
#endif

		/*
		** The XX part is required by xterm, but it's not
		** needed for the way we are using xterm (it's meant
		** to be an identifier for the pseudo-terminal).
		** Different versions of xterm use different
		** formats, so it's best to just leave it blank.
		**
		** XXX Some versions of xterm (such as that distributed
		** with XFree86 3.3.6) give a warning about this (but it
		** still works). The latest version distributed with
		** XFree86 4 does not given a warning.
		*/
		sprintf(xterm_arg, "-SXX%d", master_fd);

		execlp("xterm", "xterm", "-T", "mdb", xterm_arg, NULL);
		MR_mdb_perror("execution of xterm failed");
		exit(EXIT_FAILURE);
	} else {
		/*
		** Parent - set up the mdb I/O streams to point
		** to the pseudo-terminal.
		*/
		MR_signal_action old_alarm_action;
		int wait_status;
		int err_fd = -1;
		int out_fd = -1;

		MR_mdb_in = MR_mdb_out = MR_mdb_err = NULL;

		close(master_fd);

		/*
		** Read the first line of output -- this is a window ID
		** written by xterm. The alarm() and associated signal handling
		** is to gracefully handle the case where the xterm failed to
		** start, for example because the DISPLAY variable was invalid.
		** We don't want to restart the read() below if it times out.
		*/
		MR_get_signal_action(SIGALRM, &old_alarm_action,
			"error retrieving alarm handler");
		MR_setup_signal_no_restart(SIGALRM,
			MR_trace_internal_alarm_handler, MR_FALSE,
			"error setting up alarm handler");
		alarm(10);	/* 10 second timeout */
		while (1) {
			char c;
			int status;
			status = read(slave_fd, &c, 1);
			if (status == -1) {
				if (errno != EINTR || MR_got_alarm) {
					MR_mdb_perror(
					"error reading from mdb window");
					goto parent_error;
				}
			} else if (status == 0 || c == '\n') {
				break;
			}
		}

		/* Reset the alarm handler. */
		alarm(0);
		MR_set_signal_action(SIGALRM, &old_alarm_action,
			"error resetting alarm handler");

#if defined(MR_HAVE_TCGETATTR) && defined(MR_HAVE_TCSETATTR) && \
			defined(ECHO) && defined(TCSADRAIN)
		/* Restore echoing. */
		termio.c_lflag |= ECHO;
		tcsetattr(slave_fd, TCSADRAIN, &termio);
#endif

		if ((out_fd = dup(slave_fd)) == -1) {
			MR_mdb_perror(
			    "opening slave pseudo-terminal for xterm failed");
			goto parent_error;
		}
		if ((err_fd = dup(slave_fd)) == -1) {
			MR_mdb_perror(
			    "opening slave pseudo-terminal for xterm failed");
			goto parent_error;
		}

		MR_mdb_in = fdopen(slave_fd, "r");
		if (MR_mdb_in == NULL) {
		    MR_mdb_perror(
			"opening slave pseudo-terminal for xterm failed");
		    goto parent_error;
		}
		MR_mdb_out = fdopen(out_fd, "w");
		if (MR_mdb_out == NULL) {
		    MR_mdb_perror(
			"opening slave pseudo-terminal for xterm failed");
		    goto parent_error;
		}
		MR_mdb_err = fdopen(err_fd, "w");
		if (MR_mdb_err == NULL) {
		    MR_mdb_perror(
			"opening slave pseudo-terminal for xterm failed");
		    goto parent_error;
		}

		MR_have_mdb_window = MR_TRUE;
		MR_mdb_window_pid = child_pid;
		return MR_TRUE;

parent_error:
#if defined(MR_HAVE_KILL) && defined(SIGTERM) && defined(MR_HAVE_WAIT)
		if (kill(child_pid, SIGTERM) != -1) {
			do {
				wait_status = wait(NULL);
				if (wait_status == -1 && errno != EINTR) {
					break;	
				}
			} while (wait_status != child_pid);
		}
#endif
		if (MR_mdb_in) fclose(MR_mdb_in);
		if (MR_mdb_out) fclose(MR_mdb_out);
		if (MR_mdb_err) fclose(MR_mdb_err);
		close(slave_fd);
		close(out_fd);
		close(err_fd);
		return MR_FALSE;

	}

#else 	/* !MR_HAVE_OPEN, etc. */
	MR_mdb_warning(
		"Sorry, `mdb --window' not supported on this platform.\n");
	return MR_FALSE;
#endif /* !MR_HAVE_OPEN, etc. */
}

static void
MR_trace_internal_init_from_env(void)
{
	char	*init;

	init = getenv("MERCURY_DEBUGGER_INIT");
	if (init != NULL) {
		(void) MR_trace_source(init, MR_FALSE);
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
	FILE	*fp;

	/* XXX This code is too Unix specific. */

	env = getenv("HOME");
	if (env == NULL) {
		return;
	}

	buf = MR_NEW_ARRAY(char, strlen(env) + strlen(MDBRC_FILENAME) + 2);
	(void) strcpy(buf, env);
	(void) strcat(buf, "/");
	(void) strcat(buf, MDBRC_FILENAME);
	if ((fp = fopen(buf, "r")) != NULL) {
		MR_trace_source_from_open_file(fp);
		fclose(fp);
	}

	MR_free(buf);
}

static void
MR_trace_set_level_and_report(int ancestor_level, MR_bool detailed,
	MR_bool print_optionals)
{
	const char		*problem;
	const MR_Proc_Layout	*entry;
	MR_Word			*base_sp;
	MR_Word			*base_curfr;
	const char		*filename;
	int			lineno;
	int			indent;

	problem = MR_trace_set_level(ancestor_level, print_optionals);
	if (problem == NULL) {
		fprintf(MR_mdb_out, "Ancestor level set to %d:\n",
			ancestor_level);
		MR_trace_current_level_details(&entry, &filename, &lineno,
			&base_sp, &base_curfr);
		fprintf(MR_mdb_out, "%4d ", ancestor_level);
		if (detailed) {
			/*
			** We want to print the trace info first regardless
			** of the value of MR_context_position.
			*/

			MR_print_call_trace_info(MR_mdb_out, entry,
				base_sp, base_curfr);
			indent = 26;
		} else {
			indent = 5;
		}

		MR_print_proc_id_trace_and_context(MR_mdb_out, MR_FALSE,
			MR_context_position, entry, base_sp, base_curfr, "",
			filename, lineno, MR_FALSE, "", 0, indent);
	} else {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s.\n", problem);
	}
}

static void
MR_trace_browse_internal(MR_Word type_info, MR_Word value,
		MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
	switch (caller) {
		
		case MR_BROWSE_CALLER_BROWSE:
			MR_trace_browse(type_info, value, format);
			break;

		case MR_BROWSE_CALLER_PRINT:
		case MR_BROWSE_CALLER_PRINT_ALL:
			fprintf(MR_mdb_out, "\t");
			fflush(MR_mdb_out);
			MR_trace_print(type_info, value, caller, format);
			break;

		default:
			MR_fatal_error("MR_trace_browse_internal:"
					" unknown caller type");
	}
}

static void
MR_trace_browse_goal_internal(MR_ConstString name, MR_Word arg_list,
	MR_Word is_func, MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
	switch (caller) {
		
		case MR_BROWSE_CALLER_BROWSE:
			MR_trace_browse_goal(name, arg_list, is_func, format);
			break;

		case MR_BROWSE_CALLER_PRINT:
			MR_trace_print_goal(name, arg_list, is_func,
				caller, format);
			break;

		case MR_BROWSE_CALLER_PRINT_ALL:
			MR_fatal_error("MR_trace_browse_goal_internal:"
				" bad caller type");

		default:
			MR_fatal_error("MR_trace_browse_goal_internal:"
				" unknown caller type");
	}
}

static const char *
MR_trace_browse_exception(MR_Event_Info *event_info, MR_Browser browser,
		MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
	MR_TypeInfo	type_info;
	MR_Word		value;
	MR_Word		exception;

	if (event_info->MR_trace_port != MR_PORT_EXCEPTION) {
		return "command only available from EXCP ports";
	}

	exception = MR_trace_get_exception_value();
	if (exception == (MR_Word) NULL) {
		return "missing exception value";
	}

	MR_unravel_univ(exception, type_info, value);

	(*browser)((MR_Word)type_info, value, caller, format);

	return (const char *) NULL;
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
MR_mdb_print_proc_id(void *data, const MR_Proc_Layout *entry_layout)
{
	FILE	*fp = data;
	MR_print_proc_id_for_debugger(fp, entry_layout);
}

/* Options to pass to mmc when compiling queries. */
static char *MR_mmc_options = NULL;

static MR_Next
MR_trace_debug_cmd(char *line, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	char		**words;
	char		**orig_words = NULL;
	int		word_max;
	int		word_count;
	const char	*problem;
	MR_Next		next;

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
			event_info, event_details, jumpaddr);
	}

	MR_free(line);
	MR_free(orig_words);

	return next;
}

/*
** IMPORTANT: if you add any new commands, you will need to
**	(a) include them in MR_trace_command_infos, defined below.
**	(b) document them in doc/user_guide.texi
*/

static MR_Next
MR_trace_handle_cmd(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	const MR_Trace_Command_Info	*cmd_info;

	/*
	** The code for many commands calls getopt, and getopt may print to
	** stderr. We flush MR_mdb_out here to make sure that all normal output
	** so far (including the echoed command, if echoing is turned on) gets
	** output first.
	*/

	fflush(MR_mdb_out);

	cmd_info = MR_trace_valid_command(words[0]);
	if (cmd_info != NULL) {
		return (*cmd_info->MR_cmd_function)(words, word_count, cmd,
			event_info, event_details, jumpaddr);
	} else {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "Unknown command `%s'. "
			"Give the command `help' for help.\n", words[0]);
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_step(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;

	cmd->MR_trace_strict = MR_FALSE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_goto(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;

	cmd->MR_trace_strict = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_next(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Unsigned	depth = event_info->MR_call_depth;
	int		stop_depth;
	int		n;

	cmd->MR_trace_strict = MR_TRUE;
	cmd->MR_trace_print_level = MR_default_print_level;
	if (! MR_trace_options_strict_print(cmd, &words, &word_count,
			"forward", "next"))
	{
		; /* the usage message has already been printed */
		return KEEP_INTERACTING;
	} else if (word_count == 2 && MR_trace_is_number(words[1], &n))
	{
		stop_depth = depth - n;
	} else if (word_count == 1) {
		stop_depth = depth;
	} else {
		MR_trace_usage("forward", "next");
		return KEEP_INTERACTING;
	}

	if (depth == stop_depth &&
		MR_port_is_final(event_info->MR_trace_port))
	{
		MR_trace_do_noop();
	} else {
		cmd->MR_trace_cmd = MR_CMD_NEXT;
		cmd->MR_trace_stop_depth = stop_depth;
		return STOP_INTERACTING;
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_finish(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Unsigned	depth = event_info->MR_call_depth;
	int		stop_depth;
	int		n;

	cmd->MR_trace_strict = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_fail(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Determinism	detism = event_info->MR_event_sll->
				MR_sll_entry->MR_sle_detism;
	MR_Unsigned	depth = event_info->MR_call_depth;
	int		stop_depth;
	int		n;

	cmd->MR_trace_strict = MR_TRUE;
	cmd->MR_trace_print_level = MR_default_print_level;
	if (! MR_trace_options_strict_print(cmd, &words, &word_count,
			"forward", "fail"))
	{
		; /* the usage message has already been printed */
		return KEEP_INTERACTING;
	} else if (word_count == 2 && MR_trace_is_number(words[1], &n))
	{
		stop_depth = depth - n;
	} else if (word_count == 1) {
		stop_depth = depth;
	} else {
		MR_trace_usage("forward", "fail");
		return KEEP_INTERACTING;
	}

	if (MR_DETISM_DET_STACK(detism)) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err,
			"mdb: cannot continue until failure: "
			"selected procedure has determinism %s.\n",
			MR_detism_names[detism]);
		return KEEP_INTERACTING;
	}

	if (depth == stop_depth &&
		event_info->MR_trace_port == MR_PORT_FAIL)
	{
		MR_trace_do_noop();
	} else if (depth == stop_depth &&
		event_info->MR_trace_port == MR_PORT_EXCEPTION)
	{
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err,
			"mdb: cannot continue until failure: "
			"the call has raised an exception.\n");
	} else {
		cmd->MR_trace_cmd = MR_CMD_FAIL;
		cmd->MR_trace_stop_depth = stop_depth;
		return STOP_INTERACTING;
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_exception(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	cmd->MR_trace_strict = MR_TRUE;
	cmd->MR_trace_print_level = MR_default_print_level;
	if (! MR_trace_options_strict_print(cmd, &words, &word_count,
			"forward", "exception"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		if (event_info->MR_trace_port != MR_PORT_EXCEPTION) {
			cmd->MR_trace_cmd = MR_CMD_EXCP;
			return STOP_INTERACTING;
		} else {
			MR_trace_do_noop();
		}
	} else {
		MR_trace_usage("forward", "return");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_return(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	cmd->MR_trace_strict = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_forward(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	cmd->MR_trace_strict = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_mindepth(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	newdepth;

	cmd->MR_trace_strict = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_maxdepth(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	newdepth;

	cmd->MR_trace_strict = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_continue(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	cmd->MR_trace_strict = MR_FALSE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_retry(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int		n;
	int		ancestor_level;
	MR_bool		force_retry;
	const char	*problem;
	MR_Retry_Result	result;

	force_retry = MR_FALSE;
	if (! MR_trace_options_retry(&force_retry,
			&words, &word_count, "backward", "retry"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
		ancestor_level = n;
	} else if (word_count == 1) {
		ancestor_level = 0;
	} else {
		MR_trace_usage("backward", "retry");
		return KEEP_INTERACTING;
	}

	if (ancestor_level == 0 &&
			MR_port_is_entry(event_info->MR_trace_port))
	{
		MR_trace_do_noop();
		return KEEP_INTERACTING;
	}

	result = MR_trace_retry(event_info, event_details,
			ancestor_level, force_retry, &problem,
			MR_mdb_in, MR_mdb_out, jumpaddr);
	switch (result) {

	case MR_RETRY_OK_DIRECT:
		cmd->MR_trace_cmd = MR_CMD_GOTO;
		cmd->MR_trace_stop_event = MR_trace_event_number + 1;
		cmd->MR_trace_strict = MR_FALSE;
		cmd->MR_trace_print_level = MR_default_print_level;
		return STOP_INTERACTING;

	case MR_RETRY_OK_FINISH_FIRST:
		cmd->MR_trace_cmd = MR_CMD_FINISH;
		cmd->MR_trace_stop_depth = event_info->MR_call_depth
						- ancestor_level;
		cmd->MR_trace_strict = MR_TRUE;
		cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;

		/* Arrange to retry the call once it is finished. */
		MR_insert_line_at_head("retry");
		return STOP_INTERACTING;

	case MR_RETRY_OK_FAIL_FIRST:
		cmd->MR_trace_cmd = MR_CMD_FAIL;
		cmd->MR_trace_stop_depth = event_info->MR_call_depth
						- ancestor_level;
		cmd->MR_trace_strict = MR_TRUE;
		cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;

		/* Arrange to retry the call once it is finished. */
		MR_insert_line_at_head("retry");
		return STOP_INTERACTING;

	case MR_RETRY_ERROR:
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s\n", problem);
		return KEEP_INTERACTING;
	}

	MR_fatal_error("unrecognized retry result");
}

static MR_Next
MR_trace_cmd_level(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;
	MR_bool	detailed;

	detailed = MR_FALSE;
	if (! MR_trace_options_detailed(&detailed,
			&words, &word_count, "browsing", "level"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2 &&
			MR_trace_is_number(words[1], &n))
	{
		MR_trace_set_level_and_report(n, detailed,
			MR_print_optionals);
	} else {
		MR_trace_usage("browsing", "level");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_up(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;
	MR_bool	detailed;

	detailed = MR_FALSE;
	if (! MR_trace_options_detailed(&detailed,
			&words, &word_count, "browsing", "up"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2 &&
			MR_trace_is_number(words[1], &n))
	{
		MR_trace_set_level_and_report(
			MR_trace_current_level() + n, detailed,
			MR_print_optionals);
	} else if (word_count == 1) {
		MR_trace_set_level_and_report(
			MR_trace_current_level() + 1, detailed,
			MR_print_optionals);
	} else {
		MR_trace_usage("browsing", "up");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_down(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;
	MR_bool	detailed;

	detailed = MR_FALSE;
	if (! MR_trace_options_detailed(&detailed,
			&words, &word_count, "browsing", "down"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2 &&
			MR_trace_is_number(words[1], &n))
	{
		MR_trace_set_level_and_report(
			MR_trace_current_level() - n, detailed,
			MR_print_optionals);
	} else if (word_count == 1) {
		MR_trace_set_level_and_report(
			MR_trace_current_level() - 1, detailed,
			MR_print_optionals);
	} else {
		MR_trace_usage("browsing", "down");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_vars(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		const char	*problem;

		problem = MR_trace_list_vars(MR_mdb_out);
		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else {
		MR_trace_usage("browsing", "vars");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_print(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Browse_Format	format;
	int			n;

	if (! MR_trace_options_format(&format, &words, &word_count,
				"browsing", "print"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		const char	*problem;

		problem = MR_trace_browse_one_goal(MR_mdb_out,
			MR_trace_browse_goal_internal,
			MR_BROWSE_CALLER_PRINT, format);

		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else if (word_count == 2) {
		const char	*problem;

		if (MR_streq(words[1], "*")) {
			problem = MR_trace_browse_all(MR_mdb_out,
				MR_trace_browse_internal, format);
		} else if (MR_streq(words[1], "goal")) {
			problem = MR_trace_browse_one_goal(MR_mdb_out,
				MR_trace_browse_goal_internal,
				MR_BROWSE_CALLER_PRINT, format);
		} else if (MR_streq(words[1], "exception")) {
			problem = MR_trace_browse_exception(event_info,
				MR_trace_browse_internal,
				MR_BROWSE_CALLER_PRINT, format);
		} else {
			problem = MR_trace_parse_browse_one(MR_mdb_out,
				words[1], MR_trace_browse_internal,
				MR_BROWSE_CALLER_PRINT, format,
				MR_FALSE);
		}

		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else if (word_count == 3 && MR_streq(words[1], "action")
			&& MR_trace_is_number(words[2], &n))
	{
		const char	*problem;

		problem = MR_trace_browse_action(MR_mdb_out, n,
				MR_trace_browse_goal_internal,
				MR_BROWSE_CALLER_PRINT, format);

		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else {
		MR_trace_usage("browsing", "print");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_browse(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Browse_Format	format;
	int			n;

	if (! MR_trace_options_format(&format, &words, &word_count,
				"browsing", "browse"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		const char	*problem;

		problem = MR_trace_browse_one_goal(MR_mdb_out,
			MR_trace_browse_goal_internal,
			MR_BROWSE_CALLER_BROWSE, format);

		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else if (word_count == 2) {
		const char	*problem;

		if (MR_streq(words[1], "goal")) {
			problem = MR_trace_browse_one_goal(MR_mdb_out,
				MR_trace_browse_goal_internal,
				MR_BROWSE_CALLER_BROWSE, format);
		} else if (MR_streq(words[1], "exception")) {
			problem = MR_trace_browse_exception(event_info,
				MR_trace_browse_internal,
				MR_BROWSE_CALLER_BROWSE, format);
		} else {
			problem = MR_trace_parse_browse_one(NULL,
				words[1], MR_trace_browse_internal,
				MR_BROWSE_CALLER_BROWSE, format,
				MR_TRUE);
		}

		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else if (word_count == 3 && MR_streq(words[1], "action")
			&& MR_trace_is_number(words[2], &n))
	{
		const char	*problem;

		problem = MR_trace_browse_action(MR_mdb_out, n,
				MR_trace_browse_goal_internal,
				MR_BROWSE_CALLER_BROWSE, format);

		if (problem != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", problem);
		}
	} else {
		MR_trace_usage("browsing", "browse");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_stack(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	const MR_Label_Layout	*layout;
	MR_Word 		*saved_regs;
	MR_bool			detailed;

	layout = event_info->MR_event_sll;
	saved_regs = event_info->MR_saved_regs;

	detailed = MR_FALSE;
	if (! MR_trace_options_detailed(&detailed,
			&words, &word_count, "browsing", "stack"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		const char	*msg;
		MR_trace_init_modules();
		msg = MR_dump_stack_from_layout(MR_mdb_out, layout,
				MR_saved_sp(saved_regs),
				MR_saved_curfr(saved_regs),
				detailed,
				MR_context_position !=
					MR_CONTEXT_NOWHERE,
				&MR_dump_stack_record_print);
		if (msg != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "%s.\n", msg);
		}
	} else {
		MR_trace_usage("browsing", "stack");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_current(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_trace_event_print_internal_report(event_info);
	} else {
		MR_trace_usage("browsing", "current");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_set(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Browse_Format	format;
	MR_Word			print_set;
	MR_Word			browse_set;
	MR_Word			print_all_set;
	MR_Word			flat_format;
	MR_Word			raw_pretty_format;
	MR_Word			verbose_format;
	MR_Word			pretty_format;

	if (! MR_trace_options_param_set(&print_set, &browse_set,
			&print_all_set, &flat_format, 
			&raw_pretty_format, &verbose_format, 
			&pretty_format, &words, &word_count, "browsing",
			"set"))
	{
		; /* the usage message has already been printed */
	}
	else if (word_count != 3 ||
			! MR_trace_set_browser_param(print_set,
				browse_set, print_all_set, flat_format,
				raw_pretty_format, verbose_format, 
				pretty_format, words[1], words[2]))
	{
		MR_trace_usage("browsing", "set");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_view(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	const char		*window_cmd = NULL;
	const char		*server_cmd = NULL;
	const char		*server_name = NULL;
	int			timeout = 8;	/* seconds */
	MR_bool			force = MR_FALSE;
	MR_bool			verbose = MR_FALSE;
	MR_bool			split = MR_FALSE;
	MR_bool			close_window = MR_FALSE;
	const char		*msg;

	if (! MR_trace_options_view(&window_cmd, &server_cmd,
			&server_name, &timeout, &force, &verbose,
			&split, &close_window, &words, &word_count,
			"browsing", "view"))
	{
		; /* the usage message has already been printed */
	} else if (word_count != 1) {
		MR_trace_usage("browsing", "view");
	} else if (close_window) {
		MR_trace_maybe_close_source_window(verbose);
	} else {
		msg = MR_trace_new_source_window(window_cmd,
				server_cmd, server_name, timeout,
				force, verbose, split);
		if (msg != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", msg);
		}

		MR_trace_maybe_sync_source_window(event_info, verbose);
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_break(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	const MR_Label_Layout	*layout;
	MR_Proc_Spec		spec;
	MR_Spy_When		when;
	MR_Spy_Action		action;
	MR_MultiMatch		multi_match;
	MR_Spy_Ignore_When	ignore_when;
	int			ignore_count;
	const char		*file;
	int			line;
	int			breakline;
	const char		*problem;

	layout = event_info->MR_event_sll;

	if (word_count == 2 && MR_streq(words[1], "info")) {
		int	i;
		int	count;

		count = 0;
		for (i = 0; i < MR_spy_point_next; i++) {
			if (MR_spy_points[i]->spy_exists) {
				MR_print_spy_point(MR_mdb_out, i);
				count++;
			}
		}

		if (count == 0) {
			fprintf(MR_mdb_out,
				"There are no break points.\n");
		}

		return KEEP_INTERACTING;
	}

	when = MR_default_breakpoint_scope;
	action = MR_SPY_STOP;
	multi_match = MR_MULTIMATCH_ASK;
	/*
	** The value of ignore_when doesn't matter
	** while ignore_count contains zero.
	*/
	ignore_when = MR_SPY_DONT_IGNORE;
	ignore_count = 0;
	if (! MR_trace_options_when_action_multi_ignore(&when, &action,
			&multi_match, &ignore_when, &ignore_count,
			&words, &word_count, "breakpoint", "break"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2 && MR_streq(words[1], "here")) {
		int		slot;
		MR_Trace_Port	port;

		port = event_info->MR_trace_port;
		if (ignore_count > 0 &&
			ignore_when == MR_SPY_IGNORE_ENTRY &&
			! MR_port_is_entry(port))
		{
			fprintf(MR_mdb_out, "That breakpoint "
				"would never become enabled.\n");
			return KEEP_INTERACTING;
		} else if (ignore_count > 0 &&
			ignore_when == MR_SPY_IGNORE_INTERFACE &&
			! MR_port_is_interface(port))
		{
			fprintf(MR_mdb_out, "That breakpoint "
				"would never become enabled.\n");
			return KEEP_INTERACTING;
		}

		MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
		slot = MR_add_proc_spy_point(MR_SPY_SPECIFIC, action,
				ignore_when, ignore_count,
				layout->MR_sll_entry, layout,
				&problem);
		MR_maybe_print_spy_point(slot, problem);
	} else if (word_count == 2 &&
			MR_parse_proc_spec(words[1], &spec))
	{
		MR_Matches_Info	matches;
		int		slot;

		MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
		matches = MR_search_for_matching_procedures(&spec);
		if (matches.match_proc_next == 0) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: there is no such procedure.\n");
		} else if (matches.match_proc_next == 1) {
			slot = MR_add_proc_spy_point(when, action,
				ignore_when, ignore_count,
				matches.match_procs[0], NULL,
				&problem);
			MR_maybe_print_spy_point(slot, problem);
		} else if (multi_match == MR_MULTIMATCH_ALL) {
			int	i;

			for (i = 0; i < matches.match_proc_next; i++) {
				slot = MR_add_proc_spy_point(
					when, action,
					ignore_when, ignore_count,
					matches.match_procs[i], NULL,
					&problem);
				MR_maybe_print_spy_point(slot,
					problem);
			}
		} else {
			char	buf[80];
			int	i;
			char	*line2;

			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"Ambiguous procedure specification. "
				"The matches are:\n");
			for (i = 0; i < matches.match_proc_next; i++)
			{
				fprintf(MR_mdb_out, "%d: ", i);
				MR_print_proc_id_for_debugger(
					MR_mdb_out,
					matches.match_procs[i]);
			}

			if (multi_match == MR_MULTIMATCH_ONE) {
				return KEEP_INTERACTING;
			}

			sprintf(buf, "\nWhich do you want to put "
				"a breakpoint on (0-%d or *)? ",
				matches.match_proc_next - 1);
			line2 = MR_trace_getline(buf,
				MR_mdb_in, MR_mdb_out);
			if (line2 == NULL) {
				/* This means the user input EOF. */
				fprintf(MR_mdb_out, "none of them\n");
			} else if (MR_streq(line2, "*")) {
				for (i = 0;
					i < matches.match_proc_next;
					i++)
				{
					slot = MR_add_proc_spy_point(
						when, action,
						ignore_when,
						ignore_count,
						matches.match_procs[i],
						NULL, &problem);
					MR_maybe_print_spy_point(
						slot, problem);
				}

				MR_free(line2);
			} else if (MR_trace_is_number(line2, &i)) {
				if (0 <= i &&
					i < matches.match_proc_next)
				{
					slot = MR_add_proc_spy_point(
						when, action,
						ignore_when,
						ignore_count,
						matches.match_procs[i],
						NULL, &problem);
					MR_maybe_print_spy_point(
						slot, problem);
				} else {
					fprintf(MR_mdb_out,
						"no such match\n");
				}
				MR_free(line2);
			} else {
				fprintf(MR_mdb_out, "none of them\n");
				MR_free(line2);
			}
		}
	} else if (word_count == 2 &&
			MR_parse_source_locn(words[1], &file, &line))
	{
		int	slot;

		slot = MR_add_line_spy_point(action, ignore_when,
			ignore_count, file, line, &problem);
		MR_maybe_print_spy_point(slot, problem);
	} else if (word_count == 2 &&
			MR_trace_is_number(words[1], &breakline))
	{
		int	slot;

		if (MR_find_context(layout, &file, &line)) {
			slot = MR_add_line_spy_point(action,
				ignore_when, ignore_count,
				file, breakline, &problem);
			MR_maybe_print_spy_point(slot, problem);
		} else {
			MR_fatal_error("cannot find current filename");
		}
	} else {
		MR_trace_usage("breakpoint", "break");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_ignore(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int			n;
	MR_Spy_Ignore_When	ignore_when;
	int			ignore_count;
	const char		*problem;

	ignore_when = MR_SPY_IGNORE_ENTRY;
	ignore_count = 1;
	if (! MR_trace_options_ignore_count(&ignore_when, &ignore_count,
			&words, &word_count, "breakpoint", "ignore"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2 && MR_trace_is_number(words[1], &n))
	{
		if (0 <= n && n < MR_spy_point_next
				&& MR_spy_points[n]->spy_exists)
		{
			problem = MR_ignore_spy_point(n, ignore_when,
				ignore_count);
			MR_maybe_print_spy_point(n, problem);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: break point #%d "
				"does not exist.\n",
				n);
		}
	} else if (word_count == 2 && MR_streq(words[1], "*")) {
		int	i;
		int	count;

		count = 0;
		for (i = 0; i < MR_spy_point_next; i++) {
			if (MR_spy_points[i]->spy_exists) {
				problem = MR_ignore_spy_point(n,
					ignore_when, ignore_count);
				MR_maybe_print_spy_point(n, problem);
				count++;
			}
		}

		if (count == 0) {
			fprintf(MR_mdb_err,
				"There are no break points.\n");
		}
	} else if (word_count == 1) {
		if (0 <= MR_most_recent_spy_point
			&& MR_most_recent_spy_point < MR_spy_point_next
			&& MR_spy_points[MR_most_recent_spy_point]->
				spy_exists)
		{
			n = MR_most_recent_spy_point;
			problem = MR_ignore_spy_point(n, ignore_when,
				ignore_count);
			MR_maybe_print_spy_point(n, problem);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: there is no "
				"most recent break point.\n");
		}
	} else {
		MR_trace_usage("breakpoint", "ignore");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_enable(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;
	if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
		if (0 <= n && n < MR_spy_point_next
				&& MR_spy_points[n]->spy_exists)
		{
			MR_spy_points[n]->spy_enabled = MR_TRUE;
			MR_print_spy_point(MR_mdb_out, n);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: break point #%d "
				"does not exist.\n",
				n);
		}
	} else if (word_count == 2 && MR_streq(words[1], "*")) {
		int	i;
		int	count;

		count = 0;
		for (i = 0; i < MR_spy_point_next; i++) {
			if (MR_spy_points[i]->spy_exists) {
				MR_spy_points[i]->spy_enabled = MR_TRUE;
				MR_print_spy_point(MR_mdb_out, i);
				count++;
			}
		}

		if (count == 0) {
			fprintf(MR_mdb_err,
				"There are no break points.\n");
		}
	} else if (word_count == 1) {
		if (0 <= MR_most_recent_spy_point
			&& MR_most_recent_spy_point < MR_spy_point_next
			&& MR_spy_points[MR_most_recent_spy_point]->
				spy_exists)
		{
			MR_spy_points[MR_most_recent_spy_point]
				->spy_enabled = MR_TRUE;
			MR_print_spy_point(MR_mdb_out,
				MR_most_recent_spy_point);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: there is no "
				"most recent break point.\n");
		}
	} else {
		MR_trace_usage("breakpoint", "enable");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_disable(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;

	if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
		if (0 <= n && n < MR_spy_point_next
				&& MR_spy_points[n]->spy_exists)
		{
			MR_spy_points[n]->spy_enabled = MR_FALSE;
			MR_print_spy_point(MR_mdb_out, n);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: break point #%d "
				"does not exist.\n",
				n);
		}
	} else if (word_count == 2 && MR_streq(words[1], "*")) {
		int	i;
		int	count;

		count = 0;
		for (i = 0; i < MR_spy_point_next; i++) {
			if (MR_spy_points[i]->spy_exists) {
				MR_spy_points[i]->spy_enabled =
					MR_FALSE;
				MR_print_spy_point(MR_mdb_out, i);
				count++;
			}
		}

		if (count == 0) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"There are no break points.\n");
		}
	} else if (word_count == 1) {
		if (0 <= MR_most_recent_spy_point
			&& MR_most_recent_spy_point < MR_spy_point_next
			&& MR_spy_points[MR_most_recent_spy_point]->
				spy_exists)
		{
			MR_spy_points[MR_most_recent_spy_point]
				->spy_enabled = MR_FALSE;
			MR_print_spy_point(MR_mdb_out,
				MR_most_recent_spy_point);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "There is no "
				"most recent break point.\n");
		}
	} else {
		MR_trace_usage("breakpoint", "disable");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_delete(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;

	if (word_count == 2 && MR_trace_is_number(words[1], &n)) {
		if (0 <= n && n < MR_spy_point_next
				&& MR_spy_points[n]->spy_exists)
		{
			MR_spy_points[n]->spy_exists = MR_FALSE;
			MR_print_spy_point(MR_mdb_out, n);
			MR_delete_spy_point(n);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: break point #%d "
				"does not exist.\n",
				n);
		}
	} else if (word_count == 2 && MR_streq(words[1], "*")) {
		int	i;
		int	count;

		count = 0;
		for (i = 0; i < MR_spy_point_next; i++) {
			if (MR_spy_points[i]->spy_exists) {
				MR_spy_points[i]->spy_exists = MR_FALSE;
				MR_print_spy_point(MR_mdb_out, i);
				MR_delete_spy_point(i);
				count++;
			}
		}

		if (count == 0) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"There are no break points.\n");
		}
	} else if (word_count == 1) {
		if (0 <= MR_most_recent_spy_point
			&& MR_most_recent_spy_point < MR_spy_point_next
			&& MR_spy_points[MR_most_recent_spy_point]->
				spy_exists)
		{
			int	slot;

			slot = MR_most_recent_spy_point;
			MR_spy_points[slot]-> spy_exists = MR_FALSE;
			MR_print_spy_point(MR_mdb_out, slot);
			MR_delete_spy_point(slot);
		} else {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: there is no "
				"most recent break point.\n");
		}
	} else {
		MR_trace_usage("breakpoint", "delete");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_register(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_bool	verbose;

	if (! MR_trace_options_quiet(&verbose, &words, &word_count,
			"breakpoint", "register"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		MR_register_all_modules_and_procs(MR_mdb_out, verbose);
	} else {
		MR_trace_usage("breakpoint", "register");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_modules(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
		MR_dump_module_list(MR_mdb_out);
	} else {
		MR_trace_usage("breakpoint", "modules");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_procedures(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 2) {
		MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
		MR_dump_module_procs(MR_mdb_out, words[1]);
	} else {
		MR_trace_usage("breakpoint", "procedures");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_query(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_trace_query(MR_NORMAL_QUERY, MR_mmc_options,
		word_count - 1, words + 1);

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_cc_query(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_trace_query(MR_CC_QUERY, MR_mmc_options,
		word_count - 1, words + 1);

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_io_query(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_trace_query(MR_IO_QUERY, MR_mmc_options,
		word_count - 1, words + 1);

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_printlevel(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 2) {
		if (MR_streq(words[1], "none")) {
			MR_default_print_level = MR_PRINT_LEVEL_NONE;
			if (MR_trace_internal_interacting) {
				fprintf(MR_mdb_out,
					"Default print level set to "
					"`none'.\n");
			}
		} else if (MR_streq(words[1], "some")) {
			MR_default_print_level = MR_PRINT_LEVEL_SOME;
			if (MR_trace_internal_interacting) {
				fprintf(MR_mdb_out,
					"Default print level set to "
					"`some'.\n");
			}
		} else if (MR_streq(words[1], "all")) {
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_mmc_options(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	size_t len;
	size_t i;

	/* allocate the right amount of space */
	len = 0;
	for (i = 1; i < word_count; i++) {
		len += strlen(words[i]) + 1;
	}
	len++;
	MR_mmc_options = MR_realloc(MR_mmc_options, len);

	/* copy the arguments to MR_mmc_options */
	MR_mmc_options[0] = '\0';
	for (i = 1; i < word_count; i++) {
		strcat(MR_mmc_options, words[i]);
		strcat(MR_mmc_options, " ");
	}
	MR_mmc_options[len] = '\0';

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_scroll(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	int	n;

	if (word_count == 2) {
		if (MR_streq(words[1], "off")) {
			MR_scroll_control = MR_FALSE;
			if (MR_trace_internal_interacting) {
				fprintf(MR_mdb_out,
					"Scroll control disabled.\n");
			}
		} else if (MR_streq(words[1], "on")) {
			MR_scroll_control = MR_TRUE;
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_context(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 2) {
		if (MR_streq(words[1], "none")) {
			MR_context_position = MR_CONTEXT_NOWHERE;
		} else if (MR_streq(words[1], "before")) {
			MR_context_position = MR_CONTEXT_BEFORE;
		} else if (MR_streq(words[1], "after")) {
			MR_context_position = MR_CONTEXT_AFTER;
		} else if (MR_streq(words[1], "prevline")) {
			MR_context_position = MR_CONTEXT_PREVLINE;
		} else if (MR_streq(words[1], "nextline")) {
			MR_context_position = MR_CONTEXT_NEXTLINE;
		} else {
			MR_trace_usage("parameter", "context");
			return KEEP_INTERACTING;
		}

		if (MR_trace_internal_interacting) {
			fprintf(MR_mdb_out, "%s\n",
				MR_context_set_msg[
					MR_context_position]);
		}
	} else if (word_count == 1) {
		switch (MR_context_position) {
		case MR_CONTEXT_NOWHERE:
		case MR_CONTEXT_BEFORE:
		case MR_CONTEXT_AFTER:
		case MR_CONTEXT_PREVLINE:
		case MR_CONTEXT_NEXTLINE:
			fprintf(MR_mdb_out, "%s\n",
				MR_context_report_msg[
					MR_context_position]);
			break;

		default:
			MR_fatal_error("invalid MR_context_position");
		}
	} else {
		MR_trace_usage("parameter", "context");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_scope(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 2) {
		if (MR_streq(words[1], "all")) {
			MR_default_breakpoint_scope = MR_SPY_ALL;
		} else if (MR_streq(words[1], "interface")) {
			MR_default_breakpoint_scope = MR_SPY_INTERFACE;
		} else if (MR_streq(words[1], "entry")) {
			MR_default_breakpoint_scope = MR_SPY_ENTRY;
		} else {
			MR_trace_usage("parameter", "scope");
			return KEEP_INTERACTING;
		}

		if (MR_trace_internal_interacting) {
			fprintf(MR_mdb_out, "%s\n",
				MR_scope_set_msg[
					MR_default_breakpoint_scope]);
		}
	} else if (word_count == 1) {
		switch (MR_default_breakpoint_scope) {
		case MR_SPY_ALL:
		case MR_SPY_INTERFACE:
		case MR_SPY_ENTRY:
			fprintf(MR_mdb_out, "%s\n",
				MR_scope_report_msg[
					MR_default_breakpoint_scope]);
			break;

		default:
			MR_fatal_error(
				"invalid MR_default_breakpoint_scope");
		}
	} else {
		MR_trace_usage("parameter", "scope");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_echo(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 2) {
		if (MR_streq(words[1], "off")) {
			MR_echo_commands = MR_FALSE;
			if (MR_trace_internal_interacting) {
				fprintf(MR_mdb_out,
					"Command echo disabled.\n");
			}
		} else if (MR_streq(words[1], "on")) {
			if (!MR_echo_commands) {
				/*
				** echo the `echo on' command
				** This is needed for historical
				** reasons (compatibility with
				** our existing test suite).
				*/
				fprintf(MR_mdb_out, "echo on\n");
				MR_echo_commands = MR_TRUE;
			}
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_alias(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_trace_print_all_aliases(MR_mdb_out, MR_FALSE);
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_unalias(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_document_category(char **words, int word_count,
	MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
	MR_Event_Details *event_details, MR_Code **jumpaddr)
{
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_document(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_help(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_trace_help();
	} else if (word_count == 2) {
		MR_trace_help_word(words[1]);
	} else if (word_count == 3) {
		MR_trace_help_cat_item(words[1], words[2]);
	} else {
		MR_trace_usage("help", "help");
	}

	return KEEP_INTERACTING;
}

#ifdef	MR_TRACE_HISTOGRAM

static MR_Next
MR_trace_cmd_histogram_all(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_histogram_exp(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
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

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_clear_histogram(char **words, int word_count,
	MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
	MR_Event_Details *event_details, MR_Code **jumpaddr)
{
	if (word_count == 1) {
		int i;

		for (i = 0; i <= MR_trace_histogram_hwm; i++) {
			MR_trace_histogram_exp[i] = 0;
		}
	} else {
		MR_trace_usage("exp", "clear_histogram");
	}

	return KEEP_INTERACTING;
}

#endif	/* MR_TRACE_HISTOGRAM */

#ifdef	MR_USE_MINIMAL_MODEL

static MR_Next
MR_trace_cmd_gen_stack(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_bool	saved_tabledebug;

		MR_trace_init_modules();
		saved_tabledebug = MR_tabledebug;
		MR_tabledebug = MR_TRUE;
		MR_print_gen_stack(MR_mdb_out);
		MR_tabledebug = saved_tabledebug;
	} else {
		MR_trace_usage("developer", "gen_stack");
	}

	return KEEP_INTERACTING;
}

#endif

static MR_Next
MR_trace_cmd_nondet_stack(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	const MR_Label_Layout	*layout;
	MR_Word 		*saved_regs;
	MR_bool			detailed;

	layout = event_info->MR_event_sll;
	saved_regs = event_info->MR_saved_regs;
	detailed = MR_FALSE;

	if (! MR_trace_options_detailed(&detailed,
		&words, &word_count, "browsing", "nondet_stack"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		MR_trace_init_modules();
		if (detailed) {
			int	saved_level;

			saved_level = MR_trace_current_level();
			MR_dump_nondet_stack_from_layout(MR_mdb_out,
				MR_saved_maxfr(saved_regs),
				layout,
				MR_saved_sp(saved_regs),
				MR_saved_curfr(saved_regs));
			MR_trace_set_level(saved_level,
				MR_print_optionals);
		} else {
			MR_dump_nondet_stack(MR_mdb_out,
				MR_saved_maxfr(saved_regs));
		}
	} else {
		MR_trace_usage("developer", "nondet_stack");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_stack_regs(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Word 		*saved_regs;

	saved_regs = event_info->MR_saved_regs;

	if (word_count == 1) {
		MR_print_stack_regs(MR_mdb_out, saved_regs);
	} else {
		MR_trace_usage("developer", "stack_regs");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_all_regs(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Word 		*saved_regs;

	saved_regs = event_info->MR_saved_regs;

	if (word_count == 1) {
		MR_print_stack_regs(MR_mdb_out, saved_regs);
		MR_print_heap_regs(MR_mdb_out, saved_regs);
		MR_print_tabling_regs(MR_mdb_out, saved_regs);
		MR_print_succip_reg(MR_mdb_out, saved_regs);
		MR_print_r_regs(MR_mdb_out, saved_regs);
#ifdef	MR_DEEP_PROFILING
		MR_print_deep_prof_vars(MR_mdb_out, "mdb all_regs");
#endif
	} else {
		MR_trace_usage("developer", "all_regs");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_table_io(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		if (MR_io_tabling_phase == MR_IO_TABLING_BEFORE)
		{
			fprintf(MR_mdb_out,
				"io tabling has not yet started\n");
		} else if (MR_io_tabling_phase == MR_IO_TABLING_DURING)
		{
			fprintf(MR_mdb_out,
				"io tabling has started\n");
		} else if (MR_io_tabling_phase == MR_IO_TABLING_AFTER)
		{
			fprintf(MR_mdb_out,
				"io tabling has finished\n");
		} else {
			MR_fatal_error(
				"io tabling in impossible phase\n");
		}
	} else if (word_count == 2 && MR_streq(words[1], "start")) {
		if (MR_io_tabling_phase == MR_IO_TABLING_BEFORE) {
			MR_io_tabling_phase = MR_IO_TABLING_DURING;
			MR_io_tabling_start = MR_io_tabling_counter;
			MR_io_tabling_end = MR_IO_ACTION_MAX;
#ifdef	MR_DEBUG_RETRY
			MR_io_tabling_debug = MR_TRUE;
#endif
			fprintf(MR_mdb_out, "io tabling started\n");
		} else if (MR_io_tabling_phase == MR_IO_TABLING_DURING)
		{
			fprintf(MR_mdb_out,
				"io tabling has already started\n");
		} else if (MR_io_tabling_phase == MR_IO_TABLING_AFTER)
		{
			fprintf(MR_mdb_out,
				"io tabling has already ended\n");
		} else {
			MR_fatal_error(
				"io tabling in impossible phase\n");
		}
	} else if (word_count == 2 && MR_streq(words[1], "end")) {
		if (MR_io_tabling_phase == MR_IO_TABLING_BEFORE)
		{
			fprintf(MR_mdb_out,
				"io tabling has not yet started\n");
		} else if (MR_io_tabling_phase == MR_IO_TABLING_DURING)
		{
			MR_io_tabling_phase = MR_IO_TABLING_AFTER;
			MR_io_tabling_end = MR_io_tabling_counter_hwm;
			fprintf(MR_mdb_out, "io tabling ended\n");
		} else if (MR_io_tabling_phase == MR_IO_TABLING_AFTER)
		{
			fprintf(MR_mdb_out,
				"io tabling has already ended\n");
		} else {
			MR_fatal_error(
				"io tabling in impossible phase\n");
		}
	} else if (word_count == 2 && MR_streq(words[1], "stats")) {
		fprintf(MR_mdb_out, "phase = %d\n",
			MR_io_tabling_phase);
		MR_print_unsigned_var(MR_mdb_out, "counter",
			MR_io_tabling_counter);
		MR_print_unsigned_var(MR_mdb_out, "hwm",
			MR_io_tabling_counter_hwm);
		MR_print_unsigned_var(MR_mdb_out, "start",
			MR_io_tabling_start);
		MR_print_unsigned_var(MR_mdb_out, "end",
			MR_io_tabling_end);
	} else {
		MR_trace_usage("developer", "table_io");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_proc_stats(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_proc_layout_stats(MR_mdb_out);
	} else if (word_count == 2) {
		FILE	*fp;

		fp = fopen(words[1], "w");
		if (fp == NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: error opening `%s': %s.\n",
				words[1], strerror(errno));
			return KEEP_INTERACTING;
		}

		MR_proc_layout_stats(fp);
		(void) fclose(fp);
	} else {
		MR_trace_usage("developer", "proc_stats");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_label_stats(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 1) {
		MR_label_layout_stats(MR_mdb_out);
	} else if (word_count == 2) {
		FILE	*fp;

		fp = fopen(words[1], "w");
		if (fp == NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: error opening `%s': %s.\n",
				words[1], strerror(errno));
			return KEEP_INTERACTING;
		}

		MR_label_layout_stats(fp);
		(void) fclose(fp);
	} else {
		MR_trace_usage("developer", "label_stats");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_proc_body(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	const MR_Proc_Layout	*entry;

	entry = event_info->MR_event_sll->MR_sll_entry;
	if (entry->MR_sle_proc_rep == NULL) {
		fprintf(MR_mdb_out,
			"current procedure has no body info\n");
	} else {
		MR_trace_browse_internal(
			ML_proc_rep_type(),
			(MR_Word) entry->MR_sle_proc_rep,
			MR_BROWSE_CALLER_PRINT,
			MR_BROWSE_DEFAULT_FORMAT);
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_print_optionals(char **words, int word_count,
	MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
	MR_Event_Details *event_details, MR_Code **jumpaddr)
{
	if (word_count == 2 && MR_streq(words[1], "off")) {
		MR_print_optionals = MR_FALSE;
		MR_trace_set_level(MR_trace_current_level(),
			MR_print_optionals);
	} else if (word_count == 2 && MR_streq(words[1], "on")) {
		MR_print_optionals = MR_TRUE;
		MR_trace_set_level(MR_trace_current_level(),
			MR_print_optionals);
	} else if (word_count != 1)  {
		MR_trace_usage("developer", "print_optionals");
	}

	fprintf(MR_mdb_out,
		"optional values are %s being printed\n",
		MR_print_optionals? "" : "not");

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_source(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_bool	ignore_errors;

	ignore_errors = MR_FALSE;
	if (! MR_trace_options_ignore(&ignore_errors,
		&words, &word_count, "misc", "source"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 2)
	{
		/*
		** If the source fails, the error message
		** will have already been printed by MR_trace_source
		** (unless ignore_errors suppresses the message).
		*/
		(void) MR_trace_source(words[1], ignore_errors);
	} else {
		MR_trace_usage("misc", "source");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_save(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count == 2) {
		FILE	*fp;
		MR_bool	found_error;

		fp = fopen(words[1], "w");
		if (fp == NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: error opening `%s': %s.\n",
				words[1], strerror(errno));
			return KEEP_INTERACTING;
		}

		MR_trace_print_all_aliases(fp, MR_TRUE);
		found_error = MR_save_spy_points(fp, MR_mdb_err);

		switch (MR_default_breakpoint_scope) {
			case MR_SPY_ALL:
				fprintf(fp, "scope all\n");
				break;

			case MR_SPY_INTERFACE:
				fprintf(fp, "scope interface\n");
				break;

			case MR_SPY_ENTRY:
				fprintf(fp, "scope entry\n");
				break;

			default:
				MR_fatal_error("save cmd: "
					"invalid default scope");
		}

		if (found_error) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: could not save "
				"debugger state to %s.\n",
				words[1]);
			(void) fclose(fp);
		} else if (fclose(fp) != 0) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err,
				"mdb: error closing `%s': %s.\n",
				words[1], strerror(errno));
		} else {
			fprintf(MR_mdb_out,
				"Debugger state saved to %s.\n",
				words[1]);
		}
	} else {
		MR_trace_usage("misc", "save");
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_quit(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_bool	confirmed;

	confirmed = MR_FALSE;
	if (! MR_trace_options_confirmed(&confirmed,
			&words, &word_count, "misc", "quit"))
	{
		; /* the usage message has already been printed */
	} else if (word_count == 1) {
		if (! confirmed) {
			char	*line2;

			line2 = MR_trace_getline("mdb: "
				"are you sure you want to quit? ",
				MR_mdb_in, MR_mdb_out);
			if (line2 == NULL) {
				/* This means the user input EOF. */
				confirmed = MR_TRUE;
			} else {
				int i = 0;
				while (line2[i] != '\0' &&
						MR_isspace(line2[i]))
				{
					i++;
				}

				if (line2[i] == 'y' || line2[i] == 'Y')
				{
					confirmed = MR_TRUE;
				}

				MR_free(line2);
			}
		}

		if (confirmed) {
			MR_trace_maybe_close_source_window(MR_FALSE);
			exit(EXIT_SUCCESS);
		}
	} else {
		MR_trace_usage("misc", "quit");
	}

	return KEEP_INTERACTING;
}

#ifdef	MR_USE_DECLARATIVE_DEBUGGER

static MR_Next
MR_trace_cmd_dd(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	if (word_count != 1) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err,
			"mdb: dd requires no arguments.\n");
	} else {
		if (MR_trace_start_decl_debug(MR_TRACE_DECL_DEBUG,
			NULL, cmd, event_info, event_details, jumpaddr))
		{
			return STOP_INTERACTING;
		}
	}

	return KEEP_INTERACTING;
}

static MR_Next
MR_trace_cmd_dd_dd(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
	MR_Event_Info *event_info, MR_Event_Details *event_details,
	MR_Code **jumpaddr)
{
	MR_Trace_Mode	trace_mode;
	const char	*filename;

	if (word_count > 2) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err,
			"mdb: dd_dd takes at most one argument.\n");
	} else {
		if (word_count == 2) {
			trace_mode = MR_TRACE_DECL_DEBUG_DUMP;
			filename = (const char *) words[1];
		} else {
			trace_mode = MR_TRACE_DECL_DEBUG_DEBUG;
			filename = (const char *) NULL;
		}

		if (MR_trace_start_decl_debug(trace_mode, filename,
			cmd, event_info, event_details, jumpaddr))
		{
			return STOP_INTERACTING;
		}
	}

	return KEEP_INTERACTING;
}

#endif  /* MR_USE_DECLARATIVE_DEBUGGER */

static void
MR_maybe_print_spy_point(int slot, const char *problem)
{
	if (slot < 0) {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "mdb: %s.\n", problem);
	} else {
		MR_print_spy_point(MR_mdb_out, slot);
	}
}

static void
MR_print_unsigned_var(FILE *fp, const char *var, MR_Unsigned value)
{
	fprintf(fp, "%s = %" MR_INTEGER_LENGTH_MODIFIER "u\n", var, value);
}

static MR_bool
MR_parse_source_locn(char *word, const char **file, int *line)
{
	char		*s;
	const char	*t;

	if ((s = strrchr(word, ':')) != NULL) {
		for (t = s+1; *t != '\0'; t++) {
			if (! MR_isdigit(*t)) {
				return MR_FALSE;
			}
		}

		*s = '\0';
		*file = word;
		*line = atoi(s+1);
		return MR_TRUE;
	}

	return MR_FALSE;
}

/*
** Implement the `view' command.  First, check if there is a server
** attached.  If so, either stop it or abort the command, depending
** on whether '-f' was given.  Then, if a server name was not supplied,
** start a new server with a unique name (which has been MR_malloc'd),
** otherwise attach to the server with the supplied name (and make a
** MR_malloc'd copy of the name).
*/
static const char *
MR_trace_new_source_window(const char *window_cmd, const char *server_cmd,
		const char *server_name, int timeout, MR_bool force,
		MR_bool verbose, MR_bool split)
{
	const char	*msg;

	if (MR_trace_source_server.server_name != NULL) {
		/*
		** We are already attached to a server.
		*/
		if (force) {
			MR_trace_maybe_close_source_window(verbose);
		} else {
			return "error: server already open (use '-f' to force)";
		}
	}

	MR_trace_source_server.split = split;
	if (server_cmd != NULL) {
		MR_trace_source_server.server_cmd = MR_copy_string(server_cmd);
	} else {
		MR_trace_source_server.server_cmd = NULL;
	}

	if (server_name == NULL)
	{
		msg = MR_trace_source_open_server(&MR_trace_source_server,
				window_cmd, timeout, verbose);
	}
	else
	{
		MR_trace_source_server.server_name =
				MR_copy_string(server_name);
		msg = MR_trace_source_attach(&MR_trace_source_server, timeout,
				verbose);
		if (msg != NULL) {
			/*
			** Something went wrong, so we should free the
			** strings we allocated just above.
			*/
			MR_free(MR_trace_source_server.server_name);
			MR_trace_source_server.server_name = NULL;
			MR_free(MR_trace_source_server.server_cmd);
			MR_trace_source_server.server_cmd = NULL;
		}
	}

	return msg;
}

/*
** If we are attached to a source server, then find the appropriate
** context and ask the server to point to it, otherwise do nothing.
*/
static	void
MR_trace_maybe_sync_source_window(MR_Event_Info *event_info, MR_bool verbose)
{
	const MR_Label_Layout	*parent;
	const char		*filename;
	int			lineno;
	const char		*parent_filename;
	int			parent_lineno;
	const char		*problem; /* not used */
	MR_Word			*base_sp, *base_curfr;
	const char		*msg;

	if (MR_trace_source_server.server_name != NULL) {
		lineno = 0;
		filename = "";
		parent_lineno = 0;
		parent_filename = "";

		/*
		** At interface ports we send both the parent context and
		** the current context.  Otherwise, we just send the current
		** context.
		*/
		if (MR_port_is_interface(event_info->MR_trace_port)) {
			base_sp = MR_saved_sp(event_info->MR_saved_regs);
			base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
			parent = MR_find_nth_ancestor(event_info->MR_event_sll,
				1, &base_sp, &base_curfr, &problem);
			if (parent != NULL) {
				(void) MR_find_context(parent,
					&parent_filename, &parent_lineno);
			}
		}

		if (filename[0] == '\0') {
			(void) MR_find_context(event_info->MR_event_sll,
					&filename, &lineno);
		}

		msg = MR_trace_source_sync(&MR_trace_source_server, filename,
				lineno, parent_filename, parent_lineno,
				verbose);
		if (msg != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", msg);
		}
	}
}

/*
** Close a source server, if there is one attached.
*/
static	void
MR_trace_maybe_close_source_window(MR_bool verbose)
{
	const char	*msg;

	if (MR_trace_source_server.server_name != NULL) {
		msg = MR_trace_source_close(&MR_trace_source_server, verbose);
		if (msg != NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: %s.\n", msg);
		}

		MR_free(MR_trace_source_server.server_name);
		MR_trace_source_server.server_name = NULL;
		MR_free(MR_trace_source_server.server_cmd);
		MR_trace_source_server.server_cmd = NULL;
	}
}

static struct MR_option MR_trace_strict_print_opts[] =
{
	{ "all",	MR_no_argument,	NULL,	'a' },
	{ "none",	MR_no_argument,	NULL,	'n' },
	{ "some",	MR_no_argument,	NULL,	's' },
	{ "nostrict",	MR_no_argument,	NULL,	'N' },
	{ "strict",	MR_no_argument,	NULL,	'S' },
	{ NULL,		MR_no_argument,	NULL,	0 }
};

static MR_bool
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
				cmd->MR_trace_strict = MR_FALSE;
				break;

			case 'S':
				cmd->MR_trace_strict = MR_TRUE;
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
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_retry_opts[] =
{
	{ "allow-io",	MR_no_argument,	NULL,	'a' },
	{ NULL,		MR_no_argument,	NULL,	0 }
};

static MR_bool
MR_trace_options_retry(MR_bool *force_retry,
	char ***words, int *word_count, const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "f",
			MR_trace_retry_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'f':
				*force_retry = MR_TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_when_action_multi_ignore_opts[] =
{
	{ "all",		MR_no_argument,		NULL,	'a' },
	{ "entry",		MR_no_argument,		NULL,	'e' },
	{ "interface",		MR_no_argument,		NULL,	'i' },
	{ "ignore-entry",	MR_required_argument,	NULL,	'E' },
	{ "ignore-interface",	MR_required_argument,	NULL,	'I' },
	{ "print",		MR_no_argument,		NULL,	'P' },
	{ "stop",		MR_no_argument,		NULL,	'S' },
	{ "select-all",		MR_no_argument,		NULL,	'A' },
	{ "select-one",		MR_no_argument,		NULL,	'O' },
	{ NULL,			MR_no_argument,		NULL,	0 }
};

static MR_bool
MR_trace_options_when_action_multi_ignore(MR_Spy_When *when,
	MR_Spy_Action *action, MR_MultiMatch *multi_match,
	MR_Spy_Ignore_When*ignore_when, int *ignore_count,
	char ***words, int *word_count, const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "AE:I:OPSaei",
			MR_trace_when_action_multi_ignore_opts, NULL)) != EOF)
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

			case 'E':
				if (sscanf(MR_optarg, "%d", ignore_count) != 1)
				{
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*ignore_when = MR_SPY_IGNORE_ENTRY;
				break;

			case 'I':
				if (sscanf(MR_optarg, "%d", ignore_count) != 1)
				{
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*ignore_when = MR_SPY_IGNORE_INTERFACE;
				break;

			case 'A':
				*multi_match = MR_MULTIMATCH_ALL;
				break;

			case 'O':
				*multi_match = MR_MULTIMATCH_ONE;
				break;

			case 'P':
				*action = MR_SPY_PRINT;
				break;

			case 'S':
				*action = MR_SPY_STOP;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_ignore_count_opts[] =
{
	{ "ignore-entry",	MR_required_argument,	NULL,	'E' },
	{ "ignore-interface",	MR_required_argument,	NULL,	'I' },
	{ NULL,			MR_no_argument,		NULL,	0 }
};

static MR_bool
MR_trace_options_ignore_count(MR_Spy_Ignore_When*ignore_when,
	int *ignore_count, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "E:I:",
			MR_trace_ignore_count_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'E':
				if (sscanf(MR_optarg, "%d", ignore_count) != 1)
				{
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*ignore_when = MR_SPY_IGNORE_ENTRY;
				break;

			case 'I':
				if (sscanf(MR_optarg, "%d", ignore_count) != 1)
				{
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*ignore_when = MR_SPY_IGNORE_INTERFACE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_detailed_opts[] =
{
	{ "detailed",	MR_no_argument,	NULL,	'd' },
	{ NULL,		MR_no_argument,	NULL,	0 }
};

static MR_bool
MR_trace_options_detailed(MR_bool *detailed, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "d",
			MR_trace_detailed_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'd':
				*detailed = MR_TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static MR_bool
MR_trace_options_confirmed(MR_bool *confirmed, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt(*word_count, *words, "NYny")) != EOF) {
		switch (c) {

			case 'n':
			case 'N':
				*confirmed = MR_FALSE;
				break;

			case 'y':
			case 'Y':
				*confirmed = MR_TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_quiet_opts[] =
{
	{ "quiet",	MR_no_argument,	NULL,	'q' },
	{ "verbose",	MR_no_argument,	NULL,	'v' },
	{ NULL,		MR_no_argument,	NULL,	0 }
};

static MR_bool
MR_trace_options_quiet(MR_bool *verbose, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "qv",
			MR_trace_quiet_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'q':
				*verbose = MR_FALSE;
				break;

			case 'v':
				*verbose = MR_TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_ignore_opts[] =
{
	{ "ignore-errors",	MR_no_argument,	NULL,	'i' },
	{ NULL,			MR_no_argument,	NULL,	0 }
};

static MR_bool
MR_trace_options_ignore(MR_bool *ignore_errors, char ***words, int *word_count,
	const char *cat, const char *item)
{
	int	c;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "i",
			MR_trace_ignore_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'i':
				*ignore_errors = MR_TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_format_opts[] = 
{
	{ "flat",	MR_no_argument,	NULL,	'f' },
	{ "raw_pretty",	MR_no_argument,	NULL,	'r' },
	{ "verbose",	MR_no_argument,	NULL,	'v' },
	{ "pretty",	MR_no_argument,	NULL,	'p' },
	{ NULL,		MR_no_argument,	NULL,	0 }
};

static MR_bool
MR_trace_options_format(MR_Browse_Format *format, char ***words,
	int *word_count, const char *cat, const char *item)
{
	int	c;

	*format = MR_BROWSE_DEFAULT_FORMAT;
	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "frvp",
			MR_trace_format_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'f':
				*format = MR_BROWSE_FORMAT_FLAT;
				break;

			case 'r':
				*format = MR_BROWSE_FORMAT_RAW_PRETTY;
				break;

			case 'v':
				*format = MR_BROWSE_FORMAT_VERBOSE;
				break;

			case 'p':
				*format = MR_BROWSE_FORMAT_PRETTY;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_param_set_opts[] = 
{
	{ "flat",	MR_no_argument,	NULL,	'f' },
	{ "raw_pretty",	MR_no_argument,	NULL,	'r' },
	{ "verbose",	MR_no_argument,	NULL,	'v' },
	{ "pretty",	MR_no_argument,	NULL,	'p' },	
	{ "print",	MR_no_argument,	NULL,	'P' },
	{ "browse",	MR_no_argument,	NULL,	'B' },
	{ "print-all",	MR_no_argument,	NULL,	'A' },
	{ NULL,		MR_no_argument,	NULL,	0 }
};

static MR_bool
MR_trace_options_param_set(MR_Word *print_set, MR_Word *browse_set,
	MR_Word *print_all_set, MR_Word *flat_format, 
	MR_Word *raw_pretty_format, MR_Word *verbose_format, 
	MR_Word *pretty_format, char ***words, int *word_count, const char *cat,
	const char *item)
{
	int	c;
	MR_Word	mercury_bool_yes;
	MR_Word	mercury_bool_no;

	MR_TRACE_CALL_MERCURY(
			mercury_bool_yes = ML_BROWSE_mercury_bool_yes();
			mercury_bool_no = ML_BROWSE_mercury_bool_no();
	);

	*print_set = mercury_bool_no;
	*browse_set = mercury_bool_no;
	*print_all_set = mercury_bool_no;
	*flat_format = mercury_bool_no;
	*raw_pretty_format = mercury_bool_no;
	*verbose_format = mercury_bool_no;
	*pretty_format = mercury_bool_no;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "PBAfrvp",
			MR_trace_param_set_opts, NULL)) != EOF)
	{
		switch (c) {

			case 'f':
				*flat_format = mercury_bool_yes;
				break;

			case 'r':
				*raw_pretty_format = mercury_bool_yes;
				break;

			case 'v':
				*verbose_format = mercury_bool_yes;
				break;

			case 'p':
				*pretty_format = mercury_bool_yes;
				break;

			case 'P':
				*print_set = mercury_bool_yes;
				break;

			case 'B':
				*browse_set = mercury_bool_yes;
				break;

			case 'A':
				*print_all_set = mercury_bool_yes;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
}

static struct MR_option MR_trace_view_opts[] =
{
	{ "close",		MR_no_argument,		NULL,	'c' },
	{ "window-command",	MR_required_argument,	NULL,	'w' },
	{ "server-command",	MR_required_argument,	NULL,	's' },
	{ "server-name",	MR_required_argument,	NULL,	'n' },
	{ "timeout",		MR_required_argument,	NULL,	't' },
	{ "force",		MR_no_argument,		NULL,	'f' },
	{ "verbose",		MR_no_argument,		NULL,	'v' },
	{ "split-screen",	MR_no_argument,		NULL,	'2' },
	{ NULL,			MR_no_argument,		NULL,	0 }
};

static MR_bool
MR_trace_options_view(const char **window_cmd, const char **server_cmd,
		const char **server_name, int *timeout, MR_bool *force,
		MR_bool *verbose, MR_bool *split, MR_bool *close_window,
		char ***words, int *word_count, const char *cat,
		const char *item)
{
	int	c;
	MR_bool	no_close = MR_FALSE;

	MR_optind = 0;
	while ((c = MR_getopt_long(*word_count, *words, "cw:s:n:t:fv2",
			MR_trace_view_opts, NULL)) != EOF)
	{
		/*
		** Option '-c' is mutually incompatible with '-f', '-t',
		** '-s', '-n', '-w' and '-2'.
		*/
		switch (c) {

			case 'c':
				if (no_close) {
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*close_window = MR_TRUE;
				break;

			case 'w':
				if (*close_window) {
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*window_cmd = MR_optarg;
				no_close = MR_TRUE;
				break;

			case 's':
				if (*close_window) {
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*server_cmd = MR_optarg;
				no_close = MR_TRUE;
				break;

			case 'n':
				if (*close_window) {
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*server_name = MR_optarg;
				no_close = MR_TRUE;
				break;

			case 't':
				if (*close_window ||
					sscanf(MR_optarg, "%d", timeout) != 1)
				{
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				no_close = MR_TRUE;
				break;

			case 'f':
				if (*close_window) {
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*force = MR_TRUE;
				no_close = MR_TRUE;
				break;

			case 'v':
				*verbose = MR_TRUE;
				break;

			case '2':
				if (*close_window) {
					MR_trace_usage(cat, item);
					return MR_FALSE;
				}
				*split = MR_TRUE;
				no_close = MR_TRUE;
				break;

			default:
				MR_trace_usage(cat, item);
				return MR_FALSE;
		}
	}

	*words = *words + MR_optind - 1;
	*word_count = *word_count - MR_optind + 1;
	return MR_TRUE;
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

/*
** Read lines until we find one that contains only "end".
** Return the lines concatenated together.
** The memory returned is allocated with MR_malloc();
** it is the caller's responsibility to MR_free() it when appropriate.
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
	while ((text = MR_trace_getline("cat> ", MR_mdb_in, MR_mdb_out))
			!= NULL)
	{
		if (MR_streq(text, "end")) {
			MR_free(text);
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
		MR_free(text);
	}

	MR_ensure_big_enough(next_char_slot, doc_char, char,
			MR_INIT_DOC_CHARS);
	doc_chars[next_char_slot] = '\0';
	return doc_chars;
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
** The space for the *words array is allocated with MR_malloc().
** It is the caller's responsibility to free it when appropriate.
** The elements of the *words array point to memory from the line array.
** The lifetime of the elements of the *words array expires when
** the line array is MR_free()'d or further modified or when
** MR_trace_parse_line is called again, whichever comes first.
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
				char *, MR_INIT_WORD_COUNT);

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
** Given a text line, break it up into words.  Words are composed of
** non-space characters separated by space characters, except where
** quotes (') or escapes (\) change the treatment of characters. Make
** each word a NULL-terminated string, and remove the quotes and escapes,
** overwriting some parts of the line array in the process.
**
** On return *words will point to an array of strings, with space for
** *words_max strings. The number of strings filled in will be given by
** the return value.  The memory for *words is allocated with MR_malloc(),
** and it is the responsibility of the caller to MR_free() it when appropriate.
*/

static int
MR_trace_break_into_words(char *line, char ***words_ptr, int *word_max_ptr)
{
	int	word_max;
	char	**words;
	int	token_number;
	int	char_pos;

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

		MR_ensure_big_enough(token_number, word, char *,
			MR_INIT_WORD_COUNT);
		words[token_number] = line + char_pos;
		char_pos = MR_trace_break_off_one_word(line, char_pos);

		token_number++;
	}
}

static int
MR_trace_break_off_one_word(char *line, int char_pos)
{
	int		lag = 0;
	MR_bool		quoted = MR_FALSE;
	MR_bool		another = MR_FALSE;

	while (line[char_pos] != '\0')
	{
		if (!quoted && MR_isspace(line[char_pos])) {
			another = MR_TRUE;
			break;
		}
		if (line[char_pos] == MR_MDB_QUOTE_CHAR) {
			lag++;
			char_pos++;
			quoted = !quoted;
		} else {
			if (line[char_pos] == MR_MDB_ESCAPE_CHAR) {
				lag++;
				char_pos++;
				if (line[char_pos] == '\0') {
					MR_fatal_error(
						"MR_trace_break_off_one_word: "
						"unhandled backslash");
				}
			}

			if (lag) {
				line[char_pos - lag] = line[char_pos];
			}
			char_pos++;
		}
	}

	if (quoted) {
		MR_fatal_error("MR_trace_break_off_one_word: unmatched quote");
	}

	line[char_pos - lag] = '\0';
	if (another) {
		char_pos++;
	}

	return char_pos;
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
			*word, char *, MR_INIT_WORD_COUNT);

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

static MR_bool
MR_trace_source(const char *filename, MR_bool ignore_errors)
{
	FILE	*fp;

	if ((fp = fopen(filename, "r")) != NULL) {
		MR_trace_source_from_open_file(fp);
		fclose(fp);
		return MR_TRUE;
	} else {
		fflush(MR_mdb_out);
		fprintf(MR_mdb_err, "%s: %s.\n", filename, strerror(errno));
		return MR_FALSE;
	}
}

static void
MR_trace_source_from_open_file(FILE *fp)
{
	char	*line;

	while ((line = MR_trace_readline_raw(fp)) != NULL) {
		MR_insert_line_at_tail(line);
	}

	MR_trace_internal_interacting = MR_FALSE;
}

/*
** Call MR_trace_getline to get the next line of input, then do some
** further processing.  If the input has reached EOF, return the command
** "quit".  If the line contains multiple commands then split it and
** only return the first one.  If the newline at the end is either quoted
** or escaped, read another line (using the prompt '>') and append it to
** the first.  The command is returned in a MR_malloc'd buffer.
*/

char *
MR_trace_get_command(const char *prompt, FILE *mdb_in, FILE *mdb_out)
{
	char		*line;
	char		*ptr;
	char		*cmd_chars;
	int		cmd_char_max;
	MR_bool		quoted;
	int		len, extra_len;

	line = MR_trace_getline(prompt, mdb_in, mdb_out);

	if (line == NULL) {
		/*
		** We got an EOF.
		** We arrange things so we don't have to treat this case
		** specially in the command interpreter.
		*/
		line = MR_copy_string("quit");
		return line;
	}

	len = strlen(line);
	ptr = line;
	cmd_chars = line;
	cmd_char_max = len + 1;
	quoted = MR_FALSE;
	while (MR_trace_continue_line(ptr, &quoted)) {
		/*
		** We were inside quotes when the end of the line was
		** reached, or the newline was escaped, so input continues
		** on the next line.  We append it to the first line,
		** allocating more space if necessary.
		*/
		line = MR_trace_getline("> ", mdb_in, mdb_out);
		extra_len = strlen(line);
		/* cmd_char_max is always > 0 */
		MR_ensure_big_enough(len + extra_len + 1, cmd_char, char, 0);
		ptr = cmd_chars + len;
		strcpy(ptr, line);
		len = len + extra_len;
	}

	return cmd_chars;
}

/*
** If there any lines waiting in the queue, return the first of these.
** If not, print the prompt to mdb_out, read a line from mdb_in,
** and return it in a MR_malloc'd buffer holding the line (without the final
** newline).
** If EOF occurs on a nonempty line, treat the EOF as a newline; if EOF
** occurs on an empty line, return NULL.
**
** Whether the line is read from the queue or from mdb_in, if this function
** returns a non-NULL value, then the memory for the line returned will have
** been allocated with MR_malloc(), and it is the caller's resposibility
** to MR_free() it when appropriate.
*/

char *
MR_trace_getline(const char *prompt, FILE *mdb_in, FILE *mdb_out)
{
	char	*line;

	line = MR_trace_getline_queue();
	if (line != NULL) {
		return line;
	}

	MR_trace_internal_interacting = MR_TRUE;

	line = MR_trace_readline(prompt, mdb_in, mdb_out);

	if (MR_echo_commands && line != NULL) {
		fputs(line, mdb_out);
		putc('\n', mdb_out);
	}

	return line;
}

/*
** If there any lines waiting in the queue, return the first of these.
** The memory for the line will have been allocated with MR_malloc(),
** and it is the caller's resposibility to MR_free() it when appropriate.
** If there are no lines in the queue, this function returns NULL.
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

		MR_free(old);
		return contents;
	} else {
		return NULL;
	}
}

static void
MR_insert_line_at_head(const char *contents)
{
	MR_Line	*line;

	line = MR_NEW(MR_Line);
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

	line = MR_NEW(MR_Line);
	line->MR_line_contents = MR_copy_string(contents);
	line->MR_line_next = NULL;

	if (MR_line_tail == NULL) {
		MR_line_tail = line;
		MR_line_head = line;
	} else {
		MR_line_tail->MR_line_next = line;
		MR_line_tail = line;
	}
}

/*
** This returns MR_TRUE iff the given line continues on to the next line,
** because the newline is in quotes or escaped.  The second parameter
** indicates whether we are inside quotes or not, and is updated by
** this function.  If an unquoted and unescaped semicolon is encountered,
** the line is split at that point.
*/

static MR_bool
MR_trace_continue_line(char *ptr, MR_bool *quoted)
{
	MR_bool		escaped = MR_FALSE;

	while (*ptr != '\0') {
		if (escaped) {
			/* do nothing special */
			escaped = MR_FALSE;
		} else if (*ptr == MR_MDB_ESCAPE_CHAR) {
			escaped = MR_TRUE;
		} else if (*ptr == MR_MDB_QUOTE_CHAR) {
			*quoted = !(*quoted);
		} else if (!(*quoted) && *ptr == ';') {
			/*
			** The line contains at least two commands.
			** Return only the first command now; put the
			** others back in the input to be processed later.
			*/
			*ptr = '\0';
			MR_insert_line_at_head(MR_copy_string(ptr + 1));
			return MR_FALSE;
		}

		++ptr;
	}

	if (escaped) {
		/*
		** Replace the escaped newline with a space.
		*/
		*(ptr - 1) = ' ';
	}

	return (*quoted || escaped);
}

MR_Code *
MR_trace_event_internal_report(MR_Trace_Cmd_Info *cmd,
		MR_Event_Info *event_info)
{
	char	*buf;
	int	i;

	/* We try to leave one line for the prompt itself. */
	if (MR_scroll_control && MR_scroll_next >= MR_scroll_limit - 1) {
	try_again:
		buf = MR_trace_getline("--more-- ", MR_mdb_in, MR_mdb_out);
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
								cmd, MR_TRUE,
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

static void
MR_trace_event_print_internal_report(MR_Event_Info *event_info)
{
	const MR_Label_Layout	*parent;
	const char		*filename, *parent_filename;
	int			lineno, parent_lineno;
	const char		*problem; /* not used */
	MR_Word			*base_sp, *base_curfr;
	int			indent;

	lineno = 0;
	parent_lineno = 0;
	filename = "";
	parent_filename = "";

	fprintf(MR_mdb_out, "%8ld: %6ld %2ld %s",
		(long) event_info->MR_event_number,
		(long) event_info->MR_call_seqno,
		(long) event_info->MR_call_depth,
		MR_port_names[event_info->MR_trace_port]);
	/* the printf printed 24 characters */
	indent = 24;

	(void) MR_find_context(event_info->MR_event_sll, &filename, &lineno);
	if (MR_port_is_interface(event_info->MR_trace_port)) {
		base_sp = MR_saved_sp(event_info->MR_saved_regs);
		base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
		parent = MR_find_nth_ancestor(event_info->MR_event_sll, 1,
			&base_sp, &base_curfr, &problem);
		if (parent != NULL) {
			(void) MR_find_context(parent, &parent_filename,
				&parent_lineno);
		}
	}

	MR_print_proc_id_trace_and_context(MR_mdb_out, MR_FALSE,
		MR_context_position, event_info->MR_event_sll->MR_sll_entry,
		base_sp, base_curfr, event_info->MR_event_path,
		filename, lineno,
		MR_port_is_interface(event_info->MR_trace_port),
		parent_filename, parent_lineno, indent);
}

static const char *const	MR_trace_movement_cmd_args[] =
		{"-N", "-S", "-a", "-n", "-s",
		"--none", "--some", "--all", "--strict", "--no-strict", NULL};

static const char *const	MR_trace_print_cmd_args[] =
		{"-f", "-p", "-v", "--flat", "--pretty", "--verbose",
		"exception", "goal", "*", NULL};

		/*
		** It's better to have a single completion where possible,
		** so don't include `-d' here.
		*/
static const char *const	MR_trace_stack_cmd_args[] =
		{"--detailed", NULL};

static const char *const	MR_trace_set_cmd_args[] =
		{"-A", "-B", "-P", "-f", "-p", "-v",
		"--print-all", "--print", "--browse",
		"--flat", "--pretty", "--verbose",
		"format", "depth", "size", "width", "lines",
		"flat", "pretty", "verbose", NULL};

static const char *const	MR_trace_view_cmd_args[] =
		{"-c", "-f", "-n", "-s", "-t", "-v", "-w", "-2",
		"--close", "--verbose", "--force", "--split-screen",
		"--window-command", "--server-command", "--server-name",
		"--timeout", NULL};

static const char *const	MR_trace_break_cmd_args[] =
		{"-A", "-E", "-I", "-O", "-P", "-S", "-a", "-e", "-i",
		"--all", "--entry", "--ignore-entry", "--ignore-interface",
		"--interface", "--print", "--select-all", "--select-one",
		"--stop", "here", "info", NULL};

static const char *const	MR_trace_ignore_cmd_args[] =
		{"-E", "-I", "--ignore-entry", "--ignore-interface", NULL};

static const char *const	MR_trace_printlevel_cmd_args[] =
		{"none", "some", "all", NULL};

static const char *const	MR_trace_on_off_args[] =
		{"on", "off", NULL};

static const char *const	MR_trace_context_cmd_args[] =
		{"none", "before", "after", "prevline", "nextline", NULL};

static const char *const	MR_trace_scope_cmd_args[] =
		{"all", "interface", "entry", NULL};

static const char *const	MR_trace_table_io_cmd_args[] =
		{"stats", "start", "end", NULL};

		/*
		** It's better to have a single completion where possible,
		** so don't include `-i' here.
		*/
static const char *const	MR_trace_source_cmd_args[] =
		{"--ignore-errors", NULL};

static const char *const	MR_trace_quit_cmd_args[] =
		{"-y", NULL};

static const MR_Trace_Command_Info	MR_trace_command_infos[] =
{
	/*
	** The first two fields of this block should be the same
	** as in the file doc/mdb_command_list.
	*/

	{ "forward", "step", MR_trace_cmd_step,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "goto", MR_trace_cmd_goto,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "next", MR_trace_cmd_next,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "finish", MR_trace_cmd_finish,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "exception", MR_trace_cmd_exception,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "return", MR_trace_cmd_return,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "forward", MR_trace_cmd_forward,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "mindepth", MR_trace_cmd_mindepth,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "maxdepth", MR_trace_cmd_maxdepth,
		MR_trace_movement_cmd_args, MR_trace_null_completer },
	{ "forward", "continue", MR_trace_cmd_continue,
		MR_trace_movement_cmd_args, MR_trace_null_completer },

	{ "backward", "retry", MR_trace_cmd_retry,
		NULL, MR_trace_null_completer },

	{ "browsing", "level", MR_trace_cmd_level,
		MR_trace_stack_cmd_args, MR_trace_null_completer },
	{ "browsing", "up", MR_trace_cmd_up,
		MR_trace_stack_cmd_args, MR_trace_null_completer },
	{ "browsing", "down", MR_trace_cmd_down,
		MR_trace_stack_cmd_args, MR_trace_null_completer },
	{ "browsing", "vars", MR_trace_cmd_vars,
		NULL, MR_trace_null_completer },
	{ "browsing", "print", MR_trace_cmd_print,
		MR_trace_print_cmd_args, MR_trace_var_completer },
	{ "browsing", "browse", MR_trace_cmd_browse,
		MR_trace_print_cmd_args, MR_trace_var_completer },
	{ "browsing", "stack", MR_trace_cmd_stack,
		MR_trace_stack_cmd_args, MR_trace_null_completer },
	{ "browsing", "current", MR_trace_cmd_current,
		NULL, MR_trace_null_completer },
	{ "browsing", "set", MR_trace_cmd_set,
		MR_trace_set_cmd_args, MR_trace_null_completer },
	{ "browsing", "view", MR_trace_cmd_view,
		MR_trace_view_cmd_args, MR_trace_null_completer },

	{ "breakpoint", "break", MR_trace_cmd_break,
		MR_trace_break_cmd_args, MR_trace_breakpoint_completer },
	{ "breakpoint", "ignore", MR_trace_cmd_ignore,
		MR_trace_ignore_cmd_args, MR_trace_null_completer },
	{ "breakpoint", "enable", MR_trace_cmd_enable,
		NULL, MR_trace_null_completer },
	{ "breakpoint", "disable", MR_trace_cmd_disable,
		NULL, MR_trace_null_completer },
	{ "breakpoint", "delete", MR_trace_cmd_delete,
		NULL, MR_trace_null_completer },
	{ "breakpoint", "register", MR_trace_cmd_register,
		NULL, MR_trace_null_completer },
	{ "breakpoint", "modules", MR_trace_cmd_modules,
		NULL, MR_trace_null_completer },
	{ "breakpoint", "procedures", MR_trace_cmd_procedures,
		NULL, MR_trace_module_completer },

	/*
	** XXX For queries we should complete on all modules, not
	** just those that were compiled with tracing enabled.
	*/
	{ "queries", "query", MR_trace_cmd_query,
		NULL, MR_trace_module_completer },
	{ "queries", "cc_query", MR_trace_cmd_cc_query,
		NULL, MR_trace_module_completer },
	{ "queries", "io_query", MR_trace_cmd_io_query,
		NULL, MR_trace_module_completer },

	{ "parameter", "printlevel", MR_trace_cmd_printlevel,
		MR_trace_printlevel_cmd_args, MR_trace_null_completer },
	{ "parameter", "mmc_options", MR_trace_cmd_mmc_options,
		NULL, MR_trace_null_completer },
	{ "parameter", "scroll", MR_trace_cmd_scroll,
		MR_trace_on_off_args, MR_trace_null_completer },
	{ "parameter", "context", MR_trace_cmd_context,
		MR_trace_context_cmd_args, MR_trace_null_completer },
	{ "parameter", "scope", MR_trace_cmd_scope,
		MR_trace_scope_cmd_args, MR_trace_null_completer },
	{ "parameter", "echo", MR_trace_cmd_echo,
		MR_trace_on_off_args, MR_trace_null_completer },
	{ "parameter", "alias", MR_trace_cmd_alias,
		NULL, MR_trace_command_completer },
	{ "parameter", "unalias", MR_trace_cmd_unalias,
		NULL, MR_trace_alias_completer },

	{ "help", "document_category", MR_trace_cmd_document_category,
		NULL, MR_trace_null_completer },
	{ "help", "document", MR_trace_cmd_document,
		NULL, MR_trace_null_completer },
	{ "help", "help", MR_trace_cmd_help,
		NULL, MR_trace_help_completer },

#ifdef	MR_TRACE_HISTOGRAM
	{ "exp", "histogram_all", MR_trace_cmd_histogram_all,
		NULL, MR_trace_filename_completer },
	{ "exp", "histogram_exp", MR_trace_cmd_histogram_exp,
		NULL, MR_trace_filename_completer },
	{ "exp", "clear_histogram", MR_trace_cmd_clear_histogram,
		NULL, MR_trace_null_completer },
#endif

#ifdef	MR_USE_MINIMAL_MODEL
	{ "developer", "gen_stack", MR_trace_cmd_gen_stack,
		NULL, MR_trace_null_completer },
#endif
	{ "developer", "nondet_stack", MR_trace_cmd_nondet_stack,
		MR_trace_stack_cmd_args, MR_trace_null_completer },
	{ "developer", "stack_regs", MR_trace_cmd_stack_regs,
		NULL, MR_trace_null_completer },
	{ "developer", "all_regs", MR_trace_cmd_all_regs,
		NULL, MR_trace_null_completer },
	{ "developer", "table_io", MR_trace_cmd_table_io,
		MR_trace_table_io_cmd_args, MR_trace_null_completer },
	{ "developer", "proc_stats", MR_trace_cmd_proc_stats,
		NULL, MR_trace_filename_completer },
	{ "developer", "label_stats", MR_trace_cmd_label_stats,
		NULL, MR_trace_filename_completer },
	{ "developer", "proc_body", MR_trace_cmd_proc_body,
		NULL, MR_trace_null_completer },
	{ "developer", "print_optionals", MR_trace_cmd_print_optionals,
		MR_trace_on_off_args, MR_trace_null_completer },

	{ "misc", "source", MR_trace_cmd_source,
		MR_trace_source_cmd_args, MR_trace_filename_completer },
	{ "misc", "save", MR_trace_cmd_save,
		NULL, MR_trace_filename_completer },
	{ "misc", "quit", MR_trace_cmd_quit,
		MR_trace_quit_cmd_args, NULL},

#ifdef MR_USE_DECLARATIVE_DEBUGGER
	{ "dd", "dd", MR_trace_cmd_dd,
		NULL, MR_trace_null_completer},
	{ "dd", "dd_dd", MR_trace_cmd_dd_dd,
		NULL, MR_trace_filename_completer},
#endif

	/* End of doc/mdb_command_list. */
	{ NULL, "NUMBER", NULL,
		NULL, MR_trace_null_completer },
	{ NULL, "EMPTY", NULL,
		NULL, MR_trace_null_completer },
	{ NULL, NULL, NULL,
		NULL, MR_trace_null_completer },
};

MR_bool 
MR_trace_command_completion_info(const char *word,
	MR_Make_Completer *completer, const char *const **fixed_args)
{
	const MR_Trace_Command_Info *command_info;

	command_info = MR_trace_valid_command(word);
	if (!command_info) {
		return MR_FALSE;
	} else {
		*completer = command_info->MR_cmd_arg_completer;
		*fixed_args = command_info->MR_cmd_arg_strings;
		return MR_TRUE;
	}
}

static const MR_Trace_Command_Info *
MR_trace_valid_command(const char *word)
{
	int	i;

	for (i = 0; MR_trace_command_infos[i].MR_cmd_name != NULL; i++) {
		if (MR_streq(MR_trace_command_infos[i].MR_cmd_name, word)) {
			return &MR_trace_command_infos[i];
		}
	}

	return NULL;
}

MR_Completer_List *
MR_trace_command_completer(const char *word, size_t word_len)
{
	return MR_new_completer_elem(&MR_trace_command_completer_next,
		(MR_Completer_Data) 0, MR_trace_no_free);
}

static char *
MR_trace_command_completer_next(const char *word, size_t word_len,
		MR_Completer_Data *data)
{
	int command_index;

	command_index = (int) *data;
	while (1) {
		const char *command;
		const char *category;

		category = MR_trace_command_infos[command_index].
					MR_cmd_category;
		command = MR_trace_command_infos[command_index].
					MR_cmd_name;
		command_index++;
		*data = (void *) command_index;

		/*
		** We don't complete on the "EMPTY" and "NUMBER" entries
		** in the list of commands (they have a category entry
		** of NULL).
		*/
		if (command == NULL) {
			return NULL;
		} else if (category != NULL &&
				MR_strneq(word, command, word_len))
		{
			return MR_copy_string(command);
		}	
	}
}

void
MR_trace_interrupt_message(void)
{
	fprintf(MR_mdb_out, "\nmdb: got interrupt signal\n");
}
