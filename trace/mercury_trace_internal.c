// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2012 The University of Melbourne.
// Copyright (C) 2013-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains the top level of the code of the internal, in-process
// debugger. The functions implementing the commands themselves are in the
// files mercury_trace_cmd_*.c.
//
// Main author: Zoltan Somogyi.

#ifndef _GNU_SOURCE
   // For the GNU C library we need to define the following in order to make
   // the declarations for the UNIX98 pseudoterminal functions visible.
   // Note that we need to define this *before* stdlib.h is included.
   #define _GNU_SOURCE
#endif

#include "mercury_imp.h"
#include "mercury_layout_util.h"
#include "mercury_array_macros.h"
#include "mercury_getopt.h"
#include "mercury_signal.h"
#include "mercury_builtin_types.h"
#include "mercury_deep_profiling.h"
#include "mercury_runtime_util.h"

#include "mercury_event_spec.h"

#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_declarative.h"
#include "mercury_trace_alias.h"
#include "mercury_trace_help.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"
#include "mercury_trace_vars.h"
#include "mercury_trace_hold_vars.h"
#include "mercury_trace_readline.h"
#include "mercury_trace_source.h"
#include "mercury_trace_command_queue.h"

#include "mercury_trace_cmd_forward.h"
#include "mercury_trace_cmd_backward.h"
#include "mercury_trace_cmd_browsing.h"
#include "mercury_trace_cmd_breakpoint.h"
#include "mercury_trace_cmd_queries.h"
#include "mercury_trace_cmd_table_io.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_cmd_help.h"
#include "mercury_trace_cmd_dd.h"
#include "mercury_trace_cmd_misc.h"
#include "mercury_trace_cmd_exp.h"
#include "mercury_trace_cmd_developer.h"

#include "mdb.browse.mh"
#include "mdb.listing.mh"
#include "mdb.diff.mh"
#include "mdb.browser_info.mh"
#include "mdb.declarative_execution.mh"
#include "mdbcomp.program_representation.mh"
#include "mdbcomp.slice_and_dice.mh"

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

// Special characters used in mdb commands.
#define DOUBLE_QUOTE_CHAR    '"'
#define SINGLE_QUOTE_CHAR    '\''
#define ESCAPE_CHAR          '\\'

// The initial size of arrays of words.
#define MR_INIT_WORD_COUNT  20

// An upper bound on the maximum number of characters in a number.
// If a number has more than this many chars, the user is in trouble.
#define MR_NUMBER_LEN       80

#define MDBRC_FILENAME          ".mdbrc"
#define DEFAULT_MDBRC_FILENAME  "mdbrc"

// XXX We should consider whether all the static variables in this module
// should be thread local.

// Debugger I/O streams.
// Replacements for stdin/stdout/stderr respectively.
//
// The distinction between MR_mdb_out and MR_mdb_err is analogous to
// the distinction between stdout and stderr: ordinary output, including
// information messages about conditions which are not errors, should
// go to MR_mdb_out, but error messages should go to MR_mdb_err.
//
// Note that MR_mdb_out and MR_mdb_err may both write to the same
// file, so we need to be careful to ensure that buffering does
// not stuff up the interleaving of error messages and ordinary output.
// To ensure this, we do two things:
//
//  - MR_mdb_err is unbuffered
//  - we always fflush(MR_mdb_out) before writing to MR_mdb_err

FILE    *MR_mdb_in;
FILE    *MR_mdb_out;
FILE    *MR_mdb_err;

// MR_have_mdb_window and MR_mdb_window_pid are set by mercury_trace_internal.c ** after the xterm window for mdb has been spawned. The window process is
// killed by MR_trace_internal_kill_mdb_window(), which is called by
// MR_trace_final() through the MR_trace_shutdown() pointer. This indirect call
// is used to avoid references to the non-ISO header file <unistd.h>
// (for pid_t) in the runtime headers.

static  MR_bool     MR_have_mdb_window = MR_FALSE;
static  pid_t       MR_mdb_window_pid = 0;

// The details of the source server, if any.

MR_TraceSourceServer  MR_trace_source_server =
    { NULL, NULL, MR_FALSE };

MR_bool             MR_trace_internal_interacting = MR_FALSE;

MR_bool             MR_trace_echo_queue_commands = MR_FALSE;

static  void        MR_trace_internal_ensure_init(void);
static  MR_bool     MR_trace_internal_create_mdb_window(void);
static  void        MR_trace_internal_kill_mdb_window(void);
static  void        MR_trace_internal_init_from_env(void);
static  void        MR_trace_internal_init_from_local(void);
static  void        MR_trace_internal_init_from_home_dir(void);
static  MR_Next     MR_trace_debug_cmd(char *line, MR_TraceCmdInfo *cmd,
                        MR_EventInfo *event_info, MR_Code **jumpaddr);

static  MR_TraceCmdFunc     MR_trace_handle_cmd;

static  void        MR_mdb_print_proc_id_and_nl(void *data,
                        const MR_ProcLayout *entry_layout);
static  int         MR_trace_var_print_list(MR_SpyPrintList print_list);

static  const char  *MR_trace_parse_line(char *line, char ***words,
                        int *word_max, int *word_count);
static  const char  *MR_trace_break_into_words(char *line, char ***words_ptr,
                        int *word_max_ptr, int *word_count_ptr);
static  const char  *MR_trace_break_off_one_word(char *line, int char_pos,
                        int *new_char_pos_ptr);
static  MR_bool     MR_trace_continue_line(char *ptr, MR_bool *single_quoted,
                        MR_bool *double_quoted);
static  MR_Code     *MR_trace_event_internal_report(MR_TraceCmdInfo *cmd,
                        MR_SpyPrintList print_list,
                        MR_EventInfo *event_info);

static  char        *MR_trace_command_completer_next(const char *word,
                        size_t word_len, MR_CompleterData *data);

MR_SavedDebugState  MR_saved_debug_state;

MR_Code *
MR_trace_event_internal(MR_TraceCmdInfo *cmd, MR_bool interactive,
    MR_SpyPrintList print_list, MR_EventInfo *event_info, const char *msg)
{
    MR_Code     *jumpaddr;
    char        *line;
    MR_Next     res;

    if (! interactive) {
        return MR_trace_event_internal_report(cmd, print_list, event_info);
    }

    // We want to make sure that the Mercury code used to implement some
    // of the debugger's commands (a) doesn't generate any trace events,
    // (b) doesn't generate any unwanted debugging output, and (c) doesn't
    // do any I/O tabling.

    MR_turn_off_debug(&MR_saved_debug_state, MR_FALSE);

    MR_trace_internal_ensure_init();
    MR_trace_browse_ensure_init();
    MR_trace_listing_path_ensure_init();

    if (MR_spy_point_cond_problem != NULL) {
        fprintf(MR_mdb_err, "mdb: couldn't evaluate break point condition\n");
        MR_print_spy_cond(MR_mdb_err, MR_spy_point_cond_bad);
        fprintf(MR_mdb_err, ": %s.\n", MR_spy_point_cond_problem);
        MR_spy_point_cond_bad = NULL;
        MR_spy_point_cond_problem = NULL;
    }

    if (msg != NULL) {
        fprintf(MR_mdb_out, "%s", msg);
    }

    MR_trace_event_print_internal_report(event_info);
    MR_trace_maybe_sync_source_window(event_info, MR_FALSE);

    MR_trace_init_point_vars(event_info->MR_event_sll,
        event_info->MR_saved_regs, event_info->MR_saved_f_regs,
        event_info->MR_trace_port, MR_print_optionals);

    (void) MR_trace_var_print_list(print_list);

    // By default, return where we came from.
    jumpaddr = NULL;

    do {
        line = MR_trace_get_command("mdb> ", MR_mdb_in, MR_mdb_out);
        res = MR_trace_debug_cmd(line, cmd, event_info, &jumpaddr);
        fflush(MR_mdb_err);
    } while (res == KEEP_INTERACTING);

    cmd->MR_trace_must_check = (! cmd->MR_trace_strict) ||
        (cmd->MR_trace_print_level != MR_PRINT_LEVEL_NONE);

#ifdef  MR_TRACE_CHECK_INTEGRITY
    cmd->MR_trace_must_check = cmd->MR_trace_must_check
        || cmd->MR_trace_check_integrity;
#endif

    MR_scroll_next = 0;
    MR_turn_debug_back_on(&MR_saved_debug_state);
    return jumpaddr;
}

static const char MR_trace_banner[] =
"Melbourne Mercury Debugger, mdb version %s.\n\
Copyright 1998-2012 The University of Melbourne.\n\
Copyright 2013-2019 The Mercury team.\n\
mdb is free software; there is absolutely no warranty for mdb.\n";

static FILE *
MR_try_fopen(const char *filename, const char *mode, FILE *default_file)
{
    if (filename == NULL) {
        return default_file;
    } else {
        FILE    *f;
        char    errbuf[MR_STRERROR_BUF_SIZE];

        f = fopen(filename, mode);
        if (f == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: error opening `%s': %s\n",
                filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
            return default_file;
        } else {
            return f;
        }
    }
}

static void
MR_trace_internal_ensure_init(void)
{
    static  MR_bool MR_trace_internal_initialized = MR_FALSE;

    if (! MR_trace_internal_initialized) {
        char        *env;
        MR_Unsigned n;

        if (MR_mdb_benchmark_silent) {
            (void) close(1);
            if (open("/dev/null", O_WRONLY) != 1) {
                fprintf(stderr, "cannot silence stdout");
                exit(1);
            }

            (void) close(2);
            if (open("/dev/null", O_WRONLY) != 2) {
                // There is nowhere to report the error.
                exit(1);
            }
        }

        if (MR_mdb_in_window) {
            // If opening the window fails, fall back on using
            // MR_mdb_*_filename, or stdin, stdout and stderr.

            MR_mdb_in_window = MR_trace_internal_create_mdb_window();
            if (! MR_mdb_in_window) {
                MR_mdb_warning("Try `mdb --program-in-window' instead.\n");
            }
        }

        if (! MR_mdb_in_window) {
            MR_mdb_in = MR_try_fopen(MR_mdb_in_filename, "r", stdin);
            MR_mdb_out = MR_try_fopen(MR_mdb_out_filename, "w", stdout);
            MR_mdb_err = MR_try_fopen(MR_mdb_err_filename, "w", stderr);
        }

        // Ensure that MR_mdb_err is not buffered.
        setvbuf(MR_mdb_err, NULL, _IONBF, 0);

        if (getenv("MERCURY_SUPPRESS_MDB_BANNER") == NULL) {
            fprintf(MR_mdb_out, MR_trace_banner, MR_VERSION);
        }

        if (getenv("MERCURY_DEBUG_ECHO_QUEUE_COMMANDS") != NULL) {
            MR_trace_echo_queue_commands = MR_TRUE;
        }

        env = getenv("LINES");
        if (env != NULL && MR_trace_is_natural_number(env, &n)) {
            MR_scroll_limit = n;
        }

        // We call these functions in this order because we want the .mdbrc
        // file in the current (local) directory to be able to override
        // any actions from the system's standard .mdbrc file, and the
        // .mdbrc file (if any) referred to by the current setting of the
        // MERCURY_DEBUGGER_INIT environment to override both.

        MR_trace_internal_init_from_home_dir();
        MR_trace_internal_init_from_local();
        MR_trace_internal_init_from_env();

        MR_saved_debug_state.MR_sds_io_tabling_enabled = MR_TRUE;
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
    // XXX The code to find and open a pseudo-terminal is nowhere
    // near as portable as I would like, but given the huge variety
    // of methods for allocating pseudo-terminals it will have to do.
    // Most systems seem to be standardising on this method (from UNIX98).
    // See the xterm or expect source for a more complete version
    // (it's a bit too entwined in the rest of the code to just lift
    // it out and use it here).
    //
    // XXX Add support for MS Windows.

#if defined(MR_HAVE_OPEN) && defined(O_RDWR) && defined(MR_HAVE_FDOPEN) && \
    defined(MR_HAVE_CLOSE) && defined(MR_HAVE_DUP) &&                      \
    defined(MR_HAVE_DUP2) && defined(MR_HAVE_FORK) &&                      \
    defined(MR_HAVE_EXECLP) &&                                             \
    defined(MR_HAVE_GRANTPT) && defined(MR_HAVE_UNLOCKPT) &&               \
    defined(MR_HAVE_PTSNAME) && defined(MR_HAVE_ACCESS) && defined(F_OK)

    int master_fd = -1;
    int slave_fd = -1;
    char *slave_name;
#if defined(MR_HAVE_TERMIOS_H) && defined(MR_HAVE_TCGETATTR) && \
        defined(MR_HAVE_TCSETATTR) && defined(ECHO) && defined(TCSADRAIN)
    struct termios termio;
#endif

    // First check whether /dev/ptmx even exists, so that we can give
    // a slightly better error message if it doesn't.

    if (access("/dev/ptmx", F_OK) != 0) {
        MR_mdb_perror("can't access /dev/ptmx");
        MR_mdb_warning(
            "Sorry, `mdb --window' not supported on this platform.\n");
        return MR_FALSE;
    }

    // OK, /dev/ptmx exists; now go ahead and open it.
    master_fd = open("/dev/ptmx", O_RDWR);
    if (master_fd == -1 || grantpt(master_fd) == -1
        || unlockpt(master_fd) == -1)
    {
        MR_mdb_perror("error opening master pseudo-terminal for mdb window");
        close(master_fd);
        return MR_FALSE;
    }
    if ((slave_name = ptsname(master_fd)) == NULL) {
        MR_mdb_perror("error getting name of pseudo-terminal for mdb window");
        close(master_fd);
        return MR_FALSE;
    }
    slave_fd = open(slave_name, O_RDWR);
    if (slave_fd == -1) {
        close(master_fd);
        MR_mdb_perror("opening slave pseudo-terminal for mdb window failed");
        return MR_FALSE;
    }

#if defined(MR_HAVE_IOCTL) && defined(I_PUSH)
    // Magic STREAMS incantations to make this work on Solaris.
    ioctl(slave_fd, I_PUSH, "ptem");
    ioctl(slave_fd, I_PUSH, "ldterm");
    ioctl(slave_fd, I_PUSH, "ttcompat");
#endif

#if defined(MR_HAVE_TCGETATTR) && defined(MR_HAVE_TCSETATTR) && \
        defined(ECHO) && defined(TCSADRAIN)
    // Turn off echoing before starting the xterm so that the user doesn't see
    // the window ID printed by xterm on startup (this behaviour is not
    // documented in the xterm manual).

    tcgetattr(slave_fd, &termio);
    termio.c_lflag &= ~ECHO;
    tcsetattr(slave_fd, TCSADRAIN, &termio);
#endif

    MR_mdb_window_pid = fork();
    if (MR_mdb_window_pid == -1) {
        MR_mdb_perror("fork() for mdb window failed");
        close(master_fd);
        close(slave_fd);
        return MR_FALSE;
    } else if (MR_mdb_window_pid == 0) {
        // Child - exec() the xterm.

        char xterm_arg[50];

        close(slave_fd);

#if defined(MR_HAVE_SETPGID)
        // Put the xterm in a new process group so it won't be killed
        // by SIGINT signals sent to the program.

        if (setpgid(0, 0) < 0) {
            MR_mdb_perror("setpgid() failed");
            close(master_fd);
            exit(EXIT_FAILURE);
        }
#endif

        // The XX part is required by xterm, but it's not needed for the way
        // we are using xterm (it's meant to be an identifier for the
        // pseudo-terminal). Different versions of xterm use different formats,
        // so it's best to just leave it blank.
        //
        // XXX Some versions of xterm (such as that distributed with
        // XFree86 3.3.6) give a warning about this (but it still works).
        // The latest version distributed with XFree86 4 does not give
        // a warning.

        sprintf(xterm_arg, "-SXX%d", master_fd);

        execlp("xterm", "xterm", "-T", "mdb", xterm_arg, NULL);
        MR_mdb_perror("execution of xterm failed");
        exit(EXIT_FAILURE);
    } else {
        // Parent - set up the mdb I/O streams to point to the pseudo-terminal.

        MR_signal_action    old_alarm_action;
        int                 err_fd = -1;
        int                 out_fd = -1;

        MR_mdb_in = MR_mdb_out = MR_mdb_err = NULL;
        MR_have_mdb_window = MR_TRUE;

        close(master_fd);

        // Read the first line of output -- this is a window ID written by
        // xterm. The alarm() and associated signal handling is to gracefully
        // handle the case where the xterm failed to start, for example
        // because the DISPLAY variable was invalid. We don't want to restart
        // the read() below if it times out.

        MR_get_signal_action(SIGALRM, &old_alarm_action,
            "error retrieving alarm handler");
        MR_setup_signal_no_restart(SIGALRM, MR_trace_internal_alarm_handler,
            MR_FALSE, "error setting up alarm handler");
        MR_got_alarm = MR_FALSE;
        alarm(10);  // 10 second timeout
        while (1) {
            char    c;
            int     status;

            status = read(slave_fd, &c, 1);
            if (status == -1) {
                if (MR_got_alarm) {
                    MR_mdb_warning("timeout starting mdb window");
                    goto parent_error;
                } else if (!MR_is_eintr(errno)) {
                    MR_mdb_perror("error reading from mdb window");
                    goto parent_error;
                }
            } else if (status == 0 || c == '\n') {
                break;
            }
        }

        // Reset the alarm handler.
        alarm(0);
        MR_set_signal_action(SIGALRM, &old_alarm_action,
            "error resetting alarm handler");

#if defined(MR_HAVE_TCGETATTR) && defined(MR_HAVE_TCSETATTR) && \
            defined(ECHO) && defined(TCSADRAIN)
        // Restore echoing.
        termio.c_lflag |= ECHO;
        tcsetattr(slave_fd, TCSADRAIN, &termio);
#endif

        if ((out_fd = dup(slave_fd)) == -1) {
            MR_mdb_perror("opening slave pseudo-terminal for xterm failed");
            goto parent_error;
        }
        if ((err_fd = dup(slave_fd)) == -1) {
            MR_mdb_perror("opening slave pseudo-terminal for xterm failed");
            goto parent_error;
        }

        MR_mdb_in = fdopen(slave_fd, "r");
        if (MR_mdb_in == NULL) {
            MR_mdb_perror("opening slave pseudo-terminal for xterm failed");
            goto parent_error;
        }
        MR_mdb_out = fdopen(out_fd, "w");
        if (MR_mdb_out == NULL) {
            MR_mdb_perror("opening slave pseudo-terminal for xterm failed");
            goto parent_error;
        }
        MR_mdb_err = fdopen(err_fd, "w");
        if (MR_mdb_err == NULL) {
            MR_mdb_perror("opening slave pseudo-terminal for xterm failed");
            goto parent_error;
        }

        MR_have_mdb_window = MR_TRUE;
        MR_trace_shutdown = MR_trace_internal_kill_mdb_window;
        return MR_TRUE;

parent_error:
        MR_trace_internal_kill_mdb_window();
        if (MR_mdb_in) {
            fclose(MR_mdb_in);
        }

        if (MR_mdb_out) {
            fclose(MR_mdb_out);
        }

        if (MR_mdb_err) {
            fclose(MR_mdb_err);
        }

        close(slave_fd);
        close(out_fd);
        close(err_fd);
        return MR_FALSE;

    }

#else   // !MR_HAVE_OPEN, etc.
    MR_mdb_warning("Sorry, `mdb --window' not supported on this platform.\n");
    return MR_FALSE;
#endif // !MR_HAVE_OPEN, etc.
}

static void
MR_trace_internal_kill_mdb_window(void)
{
#if defined(MR_HAVE_KILL) && defined(MR_HAVE_WAIT) && defined(SIGTERM)
    if (MR_have_mdb_window) {
        int status;
        status = kill(MR_mdb_window_pid, SIGTERM);
        if (status != -1) {
            do {
                status = wait(NULL);
                if (status == -1 && !MR_is_eintr(errno)) {
                    break;
                }
            } while (status != MR_mdb_window_pid);
        }
    }
#endif
}

static void
MR_trace_internal_init_from_env(void)
{
    char    *init;

    init = getenv("MERCURY_DEBUGGER_INIT");
    if (init != NULL && strlen(init) > 0) {
        (void) MR_trace_source(init, MR_FALSE, NULL, 0);
        // If the source failed, the error message has been printed.
    }
}

static void
MR_trace_internal_init_from_local(void)
{
    FILE        *fp;
    const char  *init;

    init = MDBRC_FILENAME;
    if ((fp = fopen(init, "r")) != NULL) {
        MR_trace_source_from_open_file(fp, NULL, 0);
        fclose(fp);
    }
}

static void
MR_trace_internal_init_from_home_dir(void)
{
    char    *env;
    char    *buf;
    FILE    *fp;

    // XXX This code is too Unix specific.

    env = getenv("HOME");
    if (env == NULL) {
        return;
    }

    buf = MR_NEW_ARRAY(char, strlen(env) + strlen(MDBRC_FILENAME) + 2);
    (void) strcpy(buf, env);
    (void) strcat(buf, "/");
    (void) strcat(buf, MDBRC_FILENAME);
    if ((fp = fopen(buf, "r")) != NULL) {
        MR_trace_source_from_open_file(fp, NULL, 0);
        fclose(fp);
    }

    MR_free(buf);
}

MR_bool
MR_trace_source(const char *filename, MR_bool ignore_errors,
    char **args, int num_args)
{
    FILE    *fp;
    char    errbuf[MR_STRERROR_BUF_SIZE];

    if ((fp = fopen(filename, "r")) != NULL) {
        MR_trace_source_from_open_file(fp, args, num_args);
        fclose(fp);
        return MR_TRUE;
    }

    if (! ignore_errors) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "%s: %s.\n",
            filename, MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    return MR_FALSE;
}

void
MR_trace_source_from_open_file(FILE *fp, char **args, int num_args)
{
    char        *contents;
    MR_CmdLines *line;
    MR_CmdLines *first_line;
    MR_CmdLines *prev_line;

    // Invariant: either both first_line and prev_line are NULL, or neither is.

    first_line = NULL;
    prev_line = NULL;

    // Insert the sourced commands at the front of the command queue,
    // preserving their order in the sourced file.

    while ((contents = MR_trace_readline_from_script(fp, args, num_args))
        != NULL)
    {
        line = MR_NEW(MR_CmdLines);
        line->MR_cmd_line_contents = MR_copy_string(contents);
        line->MR_cmd_line_next = NULL;

        if (first_line == NULL) {
            first_line = line;
        } else {
            MR_assert(prev_line != NULL);
            prev_line->MR_cmd_line_next = line;
        }

        prev_line = line;
    }

    MR_insert_command_lines_at_tail(first_line);

    MR_trace_internal_interacting = MR_FALSE;
}

void
MR_trace_do_noop(void)
{
    fflush(MR_mdb_out);
    fprintf(MR_mdb_err, "This command is a no-op from this port.\n");
}

void
MR_trace_do_noop_tail_rec(void)
{
    fflush(MR_mdb_out);
    fprintf(MR_mdb_err,
        "Due to the reuse of stack frames by tail recursive procedures,\n"
        "this command is a no-op from this port.\n");
}

// This function is just a wrapper for MR_print_proc_id_and_nl,
// with the first argument type being `void *' rather than `FILE *',
// so that this function's address can be passed to
// MR_process_matching_procedures().

MR_Next
MR_trace_cmd_shell(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    char*       command_string;
    size_t      command_string_length;
    int         word_num;

    command_string_length = 1;
    for (word_num = 1; word_num < word_count; word_num++) {
        command_string_length += strlen(words[word_num]) + 1;
    }
    command_string = (char*) MR_malloc(sizeof(char) * command_string_length);
    command_string[0] = '\0';
    for (word_num = 1; word_num < word_count; word_num++) {
        strcat(command_string, words[word_num]);
        strcat(command_string, " ");
    }

    MR_trace_call_system_display_error_on_failure(MR_mdb_err, command_string);
    MR_free(command_string);

    return KEEP_INTERACTING;
}

static void
MR_mdb_print_proc_id_and_nl(void *data, const MR_ProcLayout *entry_layout)
{
    FILE    *fp;

    fp = (FILE *) data;
    MR_print_proc_id_and_nl(fp, entry_layout);
}

static int
MR_trace_var_print_list(MR_SpyPrintList print_list)
{
    MR_SpyPrint     node;
    const char      *problem;
    MR_VarSpec      *after_var_spec;
    int             count;

    count = 0;
    for (; print_list != NULL; print_list = print_list->MR_pl_next) {
        count++;
        node = print_list->MR_pl_cur;
        after_var_spec = NULL;

        switch (node->MR_p_what) {
            case MR_SPY_PRINT_ALL:
                problem = MR_trace_browse_all(MR_mdb_out,
                    MR_trace_browse_internal, node->MR_p_format);
                break;

            case MR_SPY_PRINT_GOAL:
                problem = MR_trace_browse_one_goal(MR_mdb_out,
                    MR_trace_browse_goal_internal, MR_BROWSE_CALLER_PRINT,
                    node->MR_p_format);
                break;

            case MR_SPY_PRINT_ONE:
                problem = MR_trace_browse_one_path(MR_mdb_out, MR_TRUE,
                    node->MR_p_var_spec, node->MR_p_path,
                    MR_trace_browse_internal, MR_BROWSE_CALLER_PRINT,
                    node->MR_p_format, MR_FALSE);
                if (problem != NULL &&
                    MR_streq(problem, "there is no such variable"))
                {
                    if (node->MR_p_warn) {
                        problem = "there is no variable named";
                        after_var_spec = &node->MR_p_var_spec;
                    } else {
                        problem = NULL;
                    }
                }

                break;

            default:
                MR_fatal_error("invalid node->MR_p_what");
                break;
        }

        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s", problem);
            if (after_var_spec != NULL) {
                fprintf(MR_mdb_err, " ");
                MR_print_var_spec(MR_mdb_err, after_var_spec);
            }
            fprintf(MR_mdb_err, ".\n");
        }
    }

    return count;
}

static MR_Next
MR_trace_debug_cmd(char *line, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    char        **words;
    char        **orig_words = NULL;
    int         word_max;
    int         word_count;
    const char  *problem;
    MR_Next     next;

    problem = MR_trace_parse_line(line, &words, &word_max, &word_count);
    if (problem != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "%s.\n", problem);
        return KEEP_INTERACTING;
    }

    MR_trace_expand_aliases(&words, &word_max, &word_count);

    // At this point, the first word_count members of the words array contain
    // the command. We save the value of words for freeing just before return,
    // since the variable words itself can be overwritten by option processing.

    orig_words = words;

    // Now we check for a special case.

    if (word_count == 0) {
        // Normally EMPTY is aliased to "step", so this won't happen.
        // This can only occur if the user has unaliased EMPTY.
        // In that case, if we get an empty command line, we ignore it.

        next = KEEP_INTERACTING;
    } else {
        // Call the command dispatcher.
        next = MR_trace_handle_cmd(words, word_count, cmd, event_info,
            jumpaddr);
    }

    MR_free(line);
    MR_free(orig_words);

    return next;
}

static  const char  *MR_current_cmd_category;
static  const char  *MR_current_cmd_name;

// IMPORTANT: if you add any new commands, you will need to
//  (a) include them in MR_trace_command_table, defined below.
//  (b) document them in doc/user_guide.texi

static MR_Next
MR_trace_handle_cmd(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const MR_TraceCmdTableEntry *cmd_table_entry;

    // The code for many commands calls getopt, and getopt may print to stderr.
    // We flush MR_mdb_out here to make sure that all normal output so far
    // (including the echoed command, if echoing is turned on) gets output
    // first.

    fflush(MR_mdb_out);

    cmd_table_entry = MR_trace_valid_command(words[0]);
    if (cmd_table_entry != NULL) {
        MR_current_cmd_category = cmd_table_entry->MR_cmd_category;
        MR_current_cmd_name = cmd_table_entry->MR_cmd_name;

        return (*cmd_table_entry->MR_cmd_function)(words, word_count, cmd,
            event_info, jumpaddr);
    } else {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "Unknown command `%s'. "
            "Give the command `help' for help.\n", words[0]);
    }

    return KEEP_INTERACTING;
}

void
MR_trace_usage_cur_cmd(void)
{
    // MR_current_cmd_category is unused now, for but could be used later.
    fflush(MR_mdb_out);
    fprintf(MR_mdb_err,
        "mdb: %s: usage error -- type `help %s' for help.\n",
        MR_current_cmd_name, MR_current_cmd_name);
}

// Given a text line, break it up into words composed of non-space characters
// separated by space characters. Make each word a NULL-terminated string,
// overwriting some spaces in the line array in the process.
//
// If the first word is a number but the second is not, swap the two.
// If the first word has a number prefix, separate it out.
//
// On return *words will point to an array of strings, with space for
// *words_max strings. The number of strings (words) filled in will be
// given by *word_count.
//
// The space for the *words array is allocated with MR_malloc().
// It is the caller's responsibility to free it when appropriate.
// The elements of the *words array point to memory from the line array.
// The lifetime of the elements of the *words array expires when
// the line array is MR_free()'d or further modified or when
// MR_trace_parse_line is called again, whichever comes first.
//
// The return value is NULL if everything went OK, and an error message
// otherwise.

static const char *
MR_trace_parse_line(char *line, char ***words, int *word_max, int *word_count)
{
    char        **raw_words;
    int         raw_word_max;
    int         raw_word_count;
    static char count_buf[MR_NUMBER_LEN + 1];
    char        *s;
    MR_Unsigned i;
    const char  *problem;

    // Handle a possible number prefix on the first word on the line,
    // separating it out into a word on its own.

    problem = MR_trace_break_into_words(line, &raw_words, &raw_word_max,
        &raw_word_count);
    if (problem != NULL) {
        return problem;
    }

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
            // Only part of the first word constitutes a number.
            // Put it in an extra word at the start.
            MR_ensure_big_enough(raw_word_count, raw_word, char *,
                MR_INIT_WORD_COUNT);

            for (i = raw_word_count; i > 0; i--) {
                raw_words[i] = raw_words[i-1];
            }

            raw_words[0] = count_buf;
            raw_words[1] = s;
            raw_word_count++;
        }
    }

    // If the first word is a number, try to exchange it with the command word,
    // to put the command word first.

    if (raw_word_count > 1 && MR_trace_is_natural_number(raw_words[0], &i)
        && ! MR_trace_is_natural_number(raw_words[1], &i))
    {
        s = raw_words[0];
        raw_words[0] = raw_words[1];
        raw_words[1] = s;
    }

    *words = raw_words;
    *word_max = raw_word_max;
    *word_count = raw_word_count;
    return NULL;
}

// Given a text line, break it up into words. Words are composed of
// non-space characters separated by space characters, except where
// quotes (') or escapes (\) change the treatment of characters. Make
// each word a NULL-terminated string, and remove the quotes and escapes,
// overwriting some parts of the line array in the process.
// XXX The "condition" command would work better if single quotes were
// left in place; that way, users could type "condition X = '+'"
// instead of "condition X = \'+\'".
//
// On return *words will point to an array of strings, with space for
// *words_max strings. The number of strings filled in will be given by
// the return value. The memory for *words is allocated with MR_malloc(),
// and it is the responsibility of the caller to MR_free() it when appropriate.

static const char *
MR_trace_break_into_words(char *line, char ***words_ptr, int *word_max_ptr,
    int *word_count)
{
    int         word_max;
    char        **words;
    int         token_number;
    int         char_pos;
    int         new_char_pos;
    const char  *problem;

    token_number = 0;
    char_pos = 0;

    word_max = 0;
    words = NULL;

    // Each iteration of this loop processes one token, or end of line.
    for (;;) {
        while (line[char_pos] != '\0' && MR_isspace(line[char_pos])) {
            char_pos++;
        }

        if (line[char_pos] == '\0') {
            *words_ptr = words;
            *word_max_ptr = word_max;
            *word_count = token_number;
            return NULL;
        }

        MR_ensure_big_enough(token_number, word, char *, MR_INIT_WORD_COUNT);
        words[token_number] = line + char_pos;
        problem = MR_trace_break_off_one_word(line, char_pos, &new_char_pos);
        if (problem != NULL) {
            return problem;
        }

        char_pos = new_char_pos;
        token_number++;
    }
}

static const char *
MR_trace_break_off_one_word(char *line, int char_pos, int *new_char_pos_ptr)
{
    int         lag = 0;
    MR_bool     single_quoted = MR_FALSE;
    MR_bool     double_quoted = MR_FALSE;
    MR_bool     another = MR_FALSE;

    while (line[char_pos] != '\0') {
        if (! single_quoted && ! double_quoted && MR_isspace(line[char_pos])) {
            another = MR_TRUE;
            break;
        }

        if (! double_quoted && line[char_pos] == SINGLE_QUOTE_CHAR) {
            lag++;
            char_pos++;
            single_quoted = ! single_quoted;
        } else if (! single_quoted && line[char_pos] == DOUBLE_QUOTE_CHAR) {
            if (lag != 0) {
                line[char_pos - lag] = line[char_pos];
            }
            char_pos++;
            double_quoted = ! double_quoted;
        } else {
            if (line[char_pos] == ESCAPE_CHAR) {
                lag++;
                char_pos++;
                if (line[char_pos] == '\0') {
                    return "bad backslash";
                }
            }

            if (lag != 0) {
                line[char_pos - lag] = line[char_pos];
            }
            char_pos++;
        }
    }

    if (single_quoted) {
        return "unmatched single quote";
    }

    if (double_quoted) {
        return "unmatched double quote";
    }

    line[char_pos - lag] = '\0';
    if (another) {
        char_pos++;
    }

    *new_char_pos_ptr = char_pos;
    return NULL;
}

// Call MR_trace_getline to get the next line of input, then do some further
// processing. If the input has reached EOF, return the command "quit".
// If the line contains multiple commands then split it and only return
// the first one. If the newline at the end is either quoted or escaped,
// read another line (using the prompt '>') and append it to the first.
// The command is returned in a MR_malloc'd buffer.

char *
MR_trace_get_command(const char *prompt, FILE *mdb_in, FILE *mdb_out)
{
    char        *line;
    char        *ptr;
    char        *cmd_chars;
    int         cmd_char_max;
    MR_bool     single_quoted;
    MR_bool     double_quoted;
    size_t      len;
    size_t      extra_len;

    line = MR_trace_getline(prompt, mdb_in, mdb_out);

    if (line == NULL) {
        // We got an EOF. We arrange things so we don't have to treat this case
        // specially in the command interpreter.

        line = MR_copy_string("quit");
        return line;
    }

    len = strlen(line);
    ptr = line;
    cmd_chars = line;
    cmd_char_max = len + 1;
    single_quoted = MR_FALSE;
    double_quoted = MR_FALSE;
    while (MR_trace_continue_line(ptr, &single_quoted, &double_quoted)) {
        // We were inside quotes when the end of the line was reached,
        // or the newline was escaped, so input continues on the next line.
        // We append it to the first line, allocating more space if necessary.

        line = MR_trace_getline("> ", mdb_in, mdb_out);
        if (line == NULL) {
            // We got an EOF... we need to stop processing the input,
            // even though it is not syntactically correct, otherwise we might
            // get into an infinite loop if we keep getting EOF.

            break;
        }
        extra_len = strlen(line);
        // cmd_char_max is always > 0
        MR_ensure_big_enough(len + extra_len + 1, cmd_char, char, 0);
        ptr = cmd_chars + len;
        strcpy(ptr, line);
        MR_free(line);
        len = len + extra_len;
    }

    return cmd_chars;
}

// If there any lines waiting in the queue, return the first of these.
// If not, print the prompt to mdb_out, read a line from mdb_in,
// and return it in a MR_malloc'd buffer holding the line (without the final
// newline).
// If EOF occurs on a nonempty line, treat the EOF as a newline; if EOF
// occurs on an empty line, return NULL.
//
// Whether the line is read from the queue or from mdb_in, if this function
// returns a non-NULL value, then the memory for the line returned will have
// been allocated with MR_malloc(), and it is the caller's responsibility
// to MR_free() it when appropriate.

char *
MR_trace_getline(const char *prompt, FILE *mdb_in, FILE *mdb_out)
{
    char    *line;

    line = MR_trace_getline_command_queue();
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

// This returns MR_TRUE iff the given line continues on to the next line,
// because the newline is in quotes or escaped. The second parameter
// indicates whether we are inside quotes or not, and is updated by
// this function. If an unquoted and unescaped semicolon is encountered,
// the line is split at that point.

static MR_bool
MR_trace_continue_line(char *ptr, MR_bool *single_quoted,
    MR_bool *double_quoted)
{
    MR_bool     escaped = MR_FALSE;

    while (*ptr != '\0') {
        if (escaped) {
            // Do nothing special.
            escaped = MR_FALSE;
        } else if (*ptr == ESCAPE_CHAR) {
            escaped = MR_TRUE;
        } else if (! (*double_quoted) && *ptr == SINGLE_QUOTE_CHAR) {
            *single_quoted = ! (*single_quoted);
        } else if (! (*single_quoted) && *ptr == DOUBLE_QUOTE_CHAR) {
            *double_quoted = ! (*double_quoted);
        } else if (! (*single_quoted) && ! (*double_quoted) && *ptr == ';') {
            // The line contains at least two commands. Return only the first
            // command now; put the others back in the input to be processed
            // later.

            *ptr = '\0';
            MR_insert_command_line_at_head(MR_copy_string(ptr + 1));
            return MR_FALSE;
        }

        ++ptr;
    }

    if (escaped) {
        // Replace the escaped newline with a space.
        *(ptr - 1) = ' ';
    }

    return (*single_quoted || *double_quoted || escaped);
}

static MR_Code *
MR_trace_event_internal_report(MR_TraceCmdInfo *cmd,
    MR_SpyPrintList print_list, MR_EventInfo *event_info)
{
    char            *buf;
    int             i;
    int             len;
    MR_SpyPrintList list;

    list = print_list;
    len = 0;
    for (; list != NULL; list = list->MR_pl_next) {
        len++;
    }

    // We try to leave one line for the prompt itself.
    if (MR_scroll_control && MR_scroll_next + len >= MR_scroll_limit - 1) {
    try_again:
        buf = MR_trace_getline("--more-- ", MR_mdb_in, MR_mdb_out);
        if (buf != NULL) {
            for (i = 0; buf[i] != '\0' && MR_isspace(buf[i]); i++)
                ;

            if (buf[i] != '\0' && !MR_isspace(buf[i])) {
                switch (buf[i]) {
                    case 'a':
                        cmd->MR_trace_print_level_specified = MR_TRUE;
                        cmd->MR_trace_print_level = MR_PRINT_LEVEL_ALL;
                        break;

                    case 'n':
                        cmd->MR_trace_print_level_specified = MR_TRUE;
                        cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;
                        break;

                    case 's':
                        cmd->MR_trace_print_level_specified = MR_TRUE;
                        cmd->MR_trace_print_level = MR_PRINT_LEVEL_SOME;
                        break;

                    case 'q':
                        MR_free(buf);
                        return MR_trace_event_internal(cmd, MR_TRUE, NULL,
                            event_info, NULL);

                    default:
                        fflush(MR_mdb_out);
                        fprintf(MR_mdb_err, "unknown command, try again\n");
                        MR_free(buf);
                        goto try_again;
                }
            }

            MR_free(buf);
        }

        MR_scroll_next = 0;
    }

    MR_trace_event_print_internal_report(event_info);
    MR_scroll_next++;

    if (print_list != NULL) {
        MR_trace_init_point_vars(event_info->MR_event_sll,
            event_info->MR_saved_regs, event_info->MR_saved_f_regs,
            event_info->MR_trace_port, MR_print_optionals);
        MR_scroll_next += MR_trace_var_print_list(print_list);
    }

    return NULL;
}

void
MR_trace_event_print_internal_report(MR_EventInfo *event_info)
{
    const MR_LabelLayout    *label_layout;
    const MR_LabelLayout    *parent;
    const char              *filename;
    const char              *parent_filename;
    int                     lineno;
    int                     parent_lineno;
    const char              *problem;                   // Not used.
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    int                     indent;
    const char              *maybe_user_event_name;
    MR_Level                actual_level;

    lineno = 0;
    parent_lineno = 0;
    filename = "";
    parent_filename = "";

    // The code below does has a job that is very similar to the job
    // of the function MR_print_call_trace_info in
    // runtime/mercury_stack_trace.c. Any changes here will probably require
    // similar changes there.

    if (MR_standardize_event_details) {
        char        buf[64];
        MR_Unsigned event_num;
        MR_Unsigned call_num;

        // Do not print the context id. It contains the values of the arguments
        // cast to integers. Since those arguments may originally have been
        // addresses, their values may differ from run to run.

        event_num = MR_standardize_event_num(event_info->MR_event_number);
        call_num = MR_standardize_call_num(event_info->MR_call_seqno);
        MR_snprintf(buf, 64, "E%ld", (long) event_num);
        fprintf(MR_mdb_out, "%8s: ", buf);
        MR_snprintf(buf, 64, "C%ld", (long) call_num);
        fprintf(MR_mdb_out, "%6s ", buf);
        fprintf(MR_mdb_out, "%s",
            MR_simplified_port_names[event_info->MR_trace_port]);
    } else {
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
        MR_Generator    *generator;
        int             i;

        generator = MR_ENGINE(MR_eng_this_context)->MR_ctxt_owner_generator;
        if (generator != NULL) {
            fprintf(MR_mdb_out, "%s ", MR_gen_subgoal(generator));
        }
#endif

        fprintf(MR_mdb_out, "%8ld: %6ld %2ld %s",
            (long) event_info->MR_event_number,
            (long) event_info->MR_call_seqno,
            (long) event_info->MR_call_depth,
            MR_simplified_port_names[event_info->MR_trace_port]);
    }

    // The printf printed 24 characters.
    indent = 24;

    label_layout = event_info->MR_event_sll;
    (void) MR_find_context(label_layout, &filename, &lineno);
    if (MR_port_is_interface(event_info->MR_trace_port)) {
        base_sp = MR_saved_sp(event_info->MR_saved_regs);
        base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
        parent = MR_find_nth_ancestor(label_layout, 1, &base_sp, &base_curfr,
            &actual_level, &problem);
        if (actual_level == 1 && parent != NULL) {
            (void) MR_find_context(parent, &parent_filename, &parent_lineno);
        }
    }

    if (label_layout->MR_sll_port >= 0 &&
        (MR_TracePort) label_layout->MR_sll_port == MR_PORT_USER)
    {
        maybe_user_event_name =
            MR_user_event_spec(label_layout).MR_ues_event_name;
        fprintf(MR_mdb_out, " <%s>", maybe_user_event_name);
    } else {
        maybe_user_event_name = NULL;
    }

    MR_print_proc_id_trace_and_context(MR_mdb_out, MR_FALSE,
        MR_context_position, MR_user_event_context, label_layout->MR_sll_entry,
        maybe_user_event_name, base_sp, base_curfr,
        ( MR_print_goal_paths ? event_info->MR_event_path : "" ),
        filename, lineno, MR_port_is_interface(event_info->MR_trace_port),
        parent_filename, parent_lineno, indent);
}

static const MR_TraceCmdTableEntry  MR_trace_command_table[] =
{
    // The first two fields of this block should be the same
    // as in the file doc/mdb_command_list.

    { "forward", "step", MR_trace_cmd_step,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "goto", MR_trace_cmd_goto,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "next", MR_trace_cmd_next,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "finish", MR_trace_cmd_finish,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "fail", MR_trace_cmd_fail,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "exception", MR_trace_cmd_exception,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "return", MR_trace_cmd_return,
        MR_trace_movement_cmd_args, MR_trace_null_completer },
    { "forward", "user", MR_trace_cmd_user,
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
        MR_trace_retry_cmd_args, MR_trace_null_completer },

    { "browsing", "level", MR_trace_cmd_level,
        MR_trace_stack_cmd_args, MR_trace_null_completer },
    { "browsing", "up", MR_trace_cmd_up,
        MR_trace_stack_cmd_args, MR_trace_null_completer },
    { "browsing", "down", MR_trace_cmd_down,
        MR_trace_stack_cmd_args, MR_trace_null_completer },
    { "browsing", "vars", MR_trace_cmd_vars,
        NULL, MR_trace_null_completer },
    { "browsing", "held_vars", MR_trace_cmd_held_vars,
        NULL, MR_trace_null_completer },
    { "browsing", "print", MR_trace_cmd_print,
        MR_trace_print_cmd_args, MR_trace_var_completer },
    { "browsing", "browse", MR_trace_cmd_browse,
        MR_trace_print_cmd_args, MR_trace_var_completer },
    { "browsing", "stack", MR_trace_cmd_stack,
        MR_trace_stack_cmd_args, MR_trace_null_completer },
    { "browsing", "current", MR_trace_cmd_current,
        NULL, MR_trace_null_completer },
    { "browsing", "view", MR_trace_cmd_view,
        MR_trace_view_cmd_args, MR_trace_null_completer },
    { "browsing", "hold", MR_trace_cmd_hold,
        NULL, MR_trace_var_completer },
    { "browsing", "diff", MR_trace_cmd_diff,
        NULL, MR_trace_var_completer },
    { "browsing", "dump", MR_trace_cmd_dump,
        NULL, MR_trace_var_completer },
    { "browsing", "list", MR_trace_cmd_list,
        NULL, MR_trace_null_completer },

    { "breakpoint", "break", MR_trace_cmd_break,
        MR_trace_break_cmd_args, MR_trace_break_completer },
    { "breakpoint", "condition", MR_trace_cmd_condition,
        NULL, MR_trace_null_completer },
    { "breakpoint", "ignore", MR_trace_cmd_ignore,
        MR_trace_ignore_cmd_args, MR_trace_null_completer },
    { "breakpoint", "break_print", MR_trace_cmd_break_print,
        NULL, MR_trace_var_completer },
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

    // XXX For queries we should complete on all modules, not just those
    // that were compiled with tracing enabled.

    { "queries", "query", MR_trace_cmd_query,
        NULL, MR_trace_module_completer },
    { "queries", "cc_query", MR_trace_cmd_cc_query,
        NULL, MR_trace_module_completer },
    { "queries", "io_query", MR_trace_cmd_io_query,
        NULL, MR_trace_module_completer },

    { "table_io", "table_io", MR_trace_cmd_table_io,
        MR_trace_table_io_cmd_args, MR_trace_null_completer },

    { "parameter", "mmc_options", MR_trace_cmd_mmc_options,
        NULL, MR_trace_null_completer },
    { "parameter", "printlevel", MR_trace_cmd_printlevel,
        MR_trace_printlevel_cmd_args, MR_trace_null_completer },
    { "parameter", "scroll", MR_trace_cmd_scroll,
        MR_trace_on_off_args, MR_trace_null_completer },
    { "parameter", "stack_default_limit", MR_trace_cmd_stack_default_limit,
        NULL, MR_trace_null_completer },
    { "parameter", "goal_paths", MR_trace_cmd_goal_paths,
        MR_trace_on_off_args, MR_trace_null_completer },
    { "parameter", "scope", MR_trace_cmd_scope,
        MR_trace_scope_cmd_args, MR_trace_null_completer },
    { "parameter", "echo", MR_trace_cmd_echo,
        MR_trace_on_off_args, MR_trace_null_completer },
    { "parameter", "context", MR_trace_cmd_context,
        MR_trace_context_cmd_args, MR_trace_null_completer },
    { "parameter", "user_event_context", MR_trace_cmd_user_event_context,
        MR_trace_user_event_context_cmd_args, MR_trace_null_completer },
    { "parameter", "list_context_lines", MR_trace_cmd_list_context_lines,
        NULL, MR_trace_null_completer },
    { "parameter", "list_path", MR_trace_cmd_list_path,
        NULL, MR_trace_null_completer },
    { "parameter", "push_list_dir", MR_trace_cmd_push_list_dir,
        NULL, MR_trace_null_completer },
    { "parameter", "pop_list_dir", MR_trace_cmd_pop_list_dir,
        NULL, MR_trace_null_completer },
    { "parameter", "fail_trace_counts", MR_trace_cmd_fail_trace_counts,
        NULL, MR_trace_filename_completer },
    { "parameter", "pass_trace_counts", MR_trace_cmd_pass_trace_counts,
        NULL, MR_trace_filename_completer },
    { "parameter", "max_io_actions", MR_trace_cmd_max_io_actions,
        NULL, MR_trace_null_completer },
    { "parameter", "xml_browser_cmd", MR_trace_cmd_xml_browser_cmd,
        NULL, MR_trace_null_completer },
    { "parameter", "xml_tmp_filename", MR_trace_cmd_xml_tmp_filename,
        NULL, MR_trace_null_completer },
    { "parameter", "web_browser_cmd", MR_trace_cmd_web_browser_cmd,
        NULL, MR_trace_null_completer },
    { "parameter", "format", MR_trace_cmd_format,
        MR_trace_format_cmd_args, MR_trace_null_completer },
    { "parameter", "format_param", MR_trace_cmd_format_param,
        MR_trace_format_param_cmd_args, MR_trace_null_completer },
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

    { "dd", "dd", MR_trace_cmd_dd,
        MR_trace_dd_cmd_args, MR_trace_null_completer },
    { "dd", "trust", MR_trace_cmd_trust,
        NULL, MR_trace_proc_spec_completer },
    { "dd", "untrust", MR_trace_cmd_untrust,
        NULL, MR_trace_null_completer },
    { "dd", "trusted", MR_trace_cmd_trusted,
        NULL, MR_trace_null_completer },

    { "misc", "source", MR_trace_cmd_source,
        MR_trace_source_cmd_args, MR_trace_filename_completer },
    { "misc", "save", MR_trace_cmd_save,
        NULL, MR_trace_filename_completer },
    { "misc", "quit", MR_trace_cmd_quit,
        MR_trace_quit_cmd_args, MR_trace_null_completer },
    { "misc", "shell", MR_trace_cmd_shell,
        NULL, MR_trace_null_completer },

    { "exp", "histogram_all", MR_trace_cmd_histogram_all,
        NULL, MR_trace_filename_completer },
    { "exp", "histogram_exp", MR_trace_cmd_histogram_exp,
        NULL, MR_trace_filename_completer },
    { "exp", "clear_histogram", MR_trace_cmd_clear_histogram,
        NULL, MR_trace_null_completer },
    { "exp", "dice", MR_trace_cmd_dice,
        NULL, MR_trace_null_completer },

    { "developer", "var_details", MR_trace_cmd_var_details,
        NULL, MR_trace_null_completer },
    { "developer", "term_size", MR_trace_cmd_term_size,
        NULL, MR_trace_null_completer },
    { "developer", "flag", MR_trace_cmd_flag,
        NULL, MR_trace_null_completer },
    { "developer", "subgoal", MR_trace_cmd_subgoal,
        NULL, MR_trace_null_completer },
    { "developer", "consumer", MR_trace_cmd_consumer,
        NULL, MR_trace_null_completer },
    { "developer", "gen_stack", MR_trace_cmd_gen_stack,
        NULL, MR_trace_null_completer },
    { "developer", "cut_stack", MR_trace_cmd_cut_stack,
        NULL, MR_trace_null_completer },
    { "developer", "pneg_stack", MR_trace_cmd_pneg_stack,
        NULL, MR_trace_null_completer },
    { "developer", "mm_stacks", MR_trace_cmd_mm_stacks,
        NULL, MR_trace_null_completer },
    { "developer", "nondet_stack", MR_trace_cmd_nondet_stack,
        MR_trace_nondet_stack_cmd_args, MR_trace_null_completer },
    { "developer", "stack_regs", MR_trace_cmd_stack_regs,
        NULL, MR_trace_null_completer },
    { "developer", "all_regs", MR_trace_cmd_all_regs,
        NULL, MR_trace_null_completer },
    { "developer", "debug_vars", MR_trace_cmd_debug_vars,
        NULL, MR_trace_null_completer },
    { "developer", "stats", MR_trace_cmd_stats,
        MR_trace_stats_cmd_args, MR_trace_filename_completer },
    { "developer", "print_optionals", MR_trace_cmd_print_optionals,
        MR_trace_on_off_args, MR_trace_null_completer },
    { "developer", "unhide_events", MR_trace_cmd_unhide_events,
        MR_trace_on_off_args, MR_trace_null_completer },
    { "developer", "table", MR_trace_cmd_table,
        NULL, MR_trace_proc_spec_completer },
    { "developer", "type_ctor", MR_trace_cmd_type_ctor,
        NULL, MR_trace_null_completer },
    { "developer", "class_decl", MR_trace_cmd_class_decl,
        NULL, MR_trace_null_completer },
    { "developer", "all_type_ctors", MR_trace_cmd_all_type_ctors,
        NULL, MR_trace_null_completer },
    { "developer", "all_class_decls", MR_trace_cmd_all_class_decls,
        NULL, MR_trace_null_completer },
    { "developer", "all_procedures", MR_trace_cmd_all_procedures,
        NULL, MR_trace_filename_completer },
    { "developer", "ambiguity", MR_trace_cmd_ambiguity,
        NULL, MR_trace_filename_completer },
    { "developer", "trail_details", MR_trace_cmd_trail_details,
        NULL, MR_trace_filename_completer },

    // End of doc/mdb_command_list.
    { NULL, "NUMBER", NULL,
        NULL, MR_trace_null_completer },
    { NULL, "EMPTY", NULL,
        NULL, MR_trace_null_completer },
    { NULL, NULL, NULL,
        NULL, MR_trace_null_completer },
};

MR_bool
MR_trace_command_completion_info(const char *word,
    MR_MakeCompleter *completer, const char *const **fixed_args)
{
    const MR_TraceCmdTableEntry *command_info;

    command_info = MR_trace_valid_command(word);
    if (command_info == NULL) {
        return MR_FALSE;
    } else {
        *completer = command_info->MR_cmd_arg_completer;
        *fixed_args = command_info->MR_cmd_arg_strings;
        return MR_TRUE;
    }
}

const MR_TraceCmdTableEntry *
MR_trace_valid_command(const char *word)
{
    int i;

    for (i = 0; MR_trace_command_table[i].MR_cmd_name != NULL; i++) {
        if (MR_streq(MR_trace_command_table[i].MR_cmd_name, word)) {
            return &MR_trace_command_table[i];
        }
    }

    return NULL;
}

MR_CompleterList *
MR_trace_command_completer(const char *word, size_t word_len)
{
    return MR_new_completer_elem(&MR_trace_command_completer_next,
        (MR_CompleterData) 0, MR_trace_no_free);
}

static char *
MR_trace_command_completer_next(const char *word, size_t word_len,
    MR_CompleterData *data)
{
    MR_Integer command_index;

    command_index = (MR_Integer) *data;
    while (1) {
        const char *command;
        const char *category;

        category = MR_trace_command_table[command_index].MR_cmd_category;
        command = MR_trace_command_table[command_index].MR_cmd_name;
        command_index++;
        *data = (void *) command_index;

        // We don't complete on the "EMPTY" and "NUMBER" entries
        // in the list of commands (they have a category entry of NULL).

        if (command == NULL) {
            return NULL;
        } else if (category != NULL && MR_strneq(word, command, word_len)) {
            return MR_copy_string(command);
        }
    }
}

void
MR_trace_interrupt_message(void)
{
    fprintf(MR_mdb_out, "\nmdb: got interrupt signal\n");
}
