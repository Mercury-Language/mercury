// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2008, 2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018, 2022-2023 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace_base.c implements the interface between the main part
// of the runtime system (mainly mercury_wrapper.c) and the part of the
// tracing subsystem that has to be present even if no module in the program
// is compiled with execution tracing.
//
// Main author: Zoltan Somogyi.

/*
INIT mercury_sys_init_trace
ENDINIT
*/

#include "mercury_imp.h"
#include "mercury_trace_base.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include "mercury_hash_table.h"
#include "mercury_layout_util.h"    // for MR_generate_proc_name_from_layout
#include "mercury_runtime_util.h"   // for MR_strerror
#include "mercury_signal.h"         // for MR_setup_signal()
#include "mercury_builtin_types.h"  // for type_ctor_infos
#include "mercury_array_macros.h"   // for type_ctor_infos
#include <signal.h>                 // for SIGINT
#include <stdio.h>
#include <errno.h>

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>       // for the write system call
#endif

#ifdef MR_HAVE_SYS_WAIT_H
  #include <sys/wait.h>     // for the wait system call
#endif

#define MR_TRACE_COUNT_SUMMARY_MAX_DEFAULT  20

void                (*MR_trace_shutdown)(void) = NULL;

MR_bool             MR_trace_count_enabled = MR_FALSE;
MR_bool             MR_coverage_test_enabled = MR_FALSE;
const char          *MR_trace_count_summary_file = NULL;
const char          *MR_trace_count_summary_cmd = "mtc_union";
unsigned int        MR_trace_count_summary_max =
                        MR_TRACE_COUNT_SUMMARY_MAX_DEFAULT;
char                *MR_trace_counts_file = NULL;

MR_bool             MR_debug_ever_enabled = MR_FALSE;
MR_bool             MR_debug_enabled = MR_FALSE;
MR_bool             MR_trace_func_enabled = MR_FALSE;
MR_Code             *(*volatile MR_selected_trace_func_ptr)(
                        const MR_LabelLayout *);
MR_Unsigned         MR_trace_call_seqno = 0;
MR_Unsigned         MR_trace_call_depth = 0;
MR_Unsigned         MR_trace_event_number = 0;
MR_bool             MR_trace_from_full = MR_TRUE;
MR_bool             MR_standardize_event_details = MR_FALSE;
MR_Trace_Type       MR_trace_handler = MR_TRACE_INTERNAL;

MR_bool             MR_trace_unhide_events = MR_FALSE;
MR_bool             MR_trace_have_unhid_events = MR_FALSE;

MR_bool             MR_trace_tailrec_have_reused_frames = MR_FALSE;
MR_Unsigned         MR_trace_tailrec_num_reused_frames = 0;

// I/O tabling is documented in library/table_builtin.m.

MR_IoTablingPhase   MR_io_tabling_phase = MR_IO_TABLING_UNINIT;
MR_bool             MR_io_tabling_enabled = MR_FALSE;
MR_TableNode        MR_io_tabling_pointer = { 0 };
MR_IoActionNum      MR_io_tabling_counter = 0;
MR_IoActionNum      MR_io_tabling_counter_hwm = 0;
MR_IoActionNum      MR_io_tabling_start = 0;
MR_IoActionNum      MR_io_tabling_end = 0;
MR_Unsigned         MR_io_tabling_start_event_num = 0;
MR_Unsigned         MR_io_tabling_stop_event_num = 0;
MR_bool             MR_io_tabling_debug = MR_FALSE;

#ifdef  MR_EXEC_TRACE
  MR_bool           MR_io_tabling_allowed = MR_TRUE;
#else
  MR_bool           MR_io_tabling_allowed = MR_FALSE;
#endif

#ifdef  MR_TRACE_HISTOGRAM

  int       *MR_trace_histogram_all = NULL;
  int       *MR_trace_histogram_exp = NULL;
  int       MR_trace_histogram_max  = 0;
  int       MR_trace_histogram_hwm  = 0;

  #define   MR_TRACE_HISTOGRAM_FILENAME ".mercury_histogram"

#endif

const char  *MR_actual_port_names[] = { MR_TRACE_PORT_ACTUAL_NAMES };
const char  *MR_simplified_port_names[] = { MR_TRACE_PORT_SIMPLIFIED_NAMES };

static  const void  *MR_get_orig_number(const void *record);
static  int         MR_hash_orig_number(const void *orig_number);
static  MR_bool     MR_equal_orig_number(const void *orig_number1,
                        const void *orig_number2);
static  MR_Unsigned MR_standardize_num(MR_Unsigned num,
                        MR_Hash_Table *table_ptr, MR_bool *init_ptr,
                        int *next_ptr);

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with the actions executed at trace events.

MR_Code *
MR_trace(const MR_LabelLayout *layout)
{
    if (! MR_trace_func_enabled) {
        return NULL;
    }

    return (*MR_selected_trace_func_ptr)(layout);
}

MR_Code *
MR_user_trace(const MR_LabelLayout *layout)
{
    if (! MR_trace_func_enabled) {
        return NULL;
    }

    return (*MR_selected_trace_func_ptr)(layout);
}

void
MR_tracing_not_enabled(void)
{
    MR_fatal_error("This executable is not set up for debugging.\n"
        "Rebuild the <main>_init.c file, "
        "and give the `-t' (or `--trace')\n"
        "option to c2init when you do so. "
        "If you are using mmake, you\n"
        "can do this by including "
        "`-t' (or `--trace') in C2INITFLAGS.\n"
        "For further details, please see the \"Debugging\" chapter "
        "of the\n"
        "Mercury User's Guide.\n");
}

MR_Code *
MR_trace_fake(const MR_LabelLayout *layout)
{
    MR_tracing_not_enabled();
    /*NOTREACHED*/
    return NULL;
}

MR_Code *
MR_trace_count(const MR_LabelLayout *label_layout)
{
    MR_Unsigned     *exec_count;

    exec_count = MR_trace_lookup_trace_count(label_layout);

#ifdef  MR_TRACE_COUNT_DEBUG
    {
        const MR_LabelLayout    *call_label_layout;
        MR_uint_least16_t       call_label_number;
        const MR_ModuleLayout   *module_layout;
        const MR_ProcLayout     *proc_layout;

        proc_layout = label_layout->MR_sll_entry;
        module_layout = proc_layout->MR_sle_module_layout;
        call_label_layout = proc_layout->MR_sle_call_label;
        if (label_layout != call_label_layout) {
            // We should only get here if we have executed the call label,
            // which means its count should be nonzero.

            call_label_number = call_label_layout->MR_sll_label_num_in_module;
            if (call_label_number >=
                module_layout->MR_ml_num_label_exec_counts)
            {
                MR_fatal_error("MR_trace_count: invalid call label number");
            }

            if (module_layout->MR_ml_label_exec_count[call_label_number] == 0)
            {
                MR_fatal_error("MR_trace_count: call label count is zero");
            }
        }
    }
#endif

    *exec_count += 1;
    return NULL;
}

MR_Unsigned *
MR_trace_lookup_trace_count(const MR_LabelLayout *label_layout)
{
    const MR_ModuleLayout   *module_layout;
    const MR_ProcLayout     *proc_layout;
    MR_uint_least16_t       label_number;

    proc_layout = label_layout->MR_sll_entry;
    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)) {
        MR_fatal_error("MR_trace_lookup_trace_count: no exec trace");
    }

    module_layout = proc_layout->MR_sle_module_layout;
    label_number = label_layout->MR_sll_label_num_in_module;
    if (label_number >= module_layout->MR_ml_num_label_exec_counts) {
        MR_fatal_error("MR_trace_lookup_trace_count: invalid label number");
    }

    return &(module_layout->MR_ml_label_exec_count[label_number]);
}

////////////////////////////////////////////////////////////////////////////

#define INIT_MODULE_TABLE_SIZE  10

const MR_ModuleLayout   **MR_module_infos;
unsigned                MR_module_info_next = 0;
unsigned                MR_module_info_max  = 0;

void
MR_insert_module_info_into_module_table(const MR_ModuleLayout *module)
{
    int     slot;

    MR_GC_ensure_room_for_next(MR_module_info, const MR_ModuleLayout *,
        INIT_MODULE_TABLE_SIZE, MR_ALLOC_SITE_NONE);
    MR_prepare_insert_into_sorted(MR_module_infos, MR_module_info_next, slot,
        strcmp(MR_module_infos[slot]->MR_ml_name, module->MR_ml_name));
    MR_module_infos[slot] = module;
}

////////////////////////////////////////////////////////////////////////////

static  void        MR_trace_write_quoted_atom(FILE *fp, const char *atom);
static  void        MR_trace_write_string(FILE *fp, const char *atom);
static  unsigned    MR_trace_write_label_exec_counts_for_file(FILE *fp,
                        const MR_ModuleLayout *module,
                        const MR_ModuleFileLayout *file,
                        const char *module_name,
                        MR_bool coverage_test);

MR_PathPort         MR_named_count_port[MR_PORT_NONE + 1];

#define MERCURY_TRACE_COUNTS_PREFIX     "mercury_trace_counts"
#define TEMP_SUFFIX                     ".tmp"

#if defined(F_OK)
  // unistd.h version
  #define MR_PERMISSIONS    F_OK
#else
  // win32 version
  #define MR_PERMISSIONS    0
#endif

#define MR_FILE_EXISTS(filename)        (access(filename, MR_PERMISSIONS) == 0)

void
MR_trace_record_label_exec_counts(void *dummy)
{
    FILE        *fp;
    char        *name;
    unsigned    name_len;
    MR_bool     summarize;
    MR_bool     keep;
    char        *slash;
    const char  *program_name;
    char        errbuf[MR_STRERROR_BUF_SIZE];

    program_name = MR_get_program_basename(MR_progname);

    summarize = MR_FALSE;
    keep = MR_FALSE;
    if (MR_trace_count_summary_file != NULL) {
        if (MR_FILE_EXISTS(MR_trace_count_summary_file)) {
            int     i;

            // 30 bytes must be enough for the dot, the value of i, and '\0'.
            name_len = strlen(MR_trace_count_summary_file) + 30;
            name = MR_malloc(name_len);

            fp = NULL;
            // Search for a suffix that doesn't exist yet.
            for (i = 1; i <= MR_trace_count_summary_max; i++) {
                MR_snprintf(name, name_len, "%s.%d",
                    MR_trace_count_summary_file, i);
                if (! MR_FILE_EXISTS(name)) {
                    // File doesn't exist, commit to this one.
                    if (i == MR_trace_count_summary_max) {
                        summarize = MR_TRUE;
                    }

                    break;
                }
            }
        } else {
            // The summary file doesn't yet exist, create it.
            name = MR_copy_string(MR_trace_count_summary_file);
        }
    } else if (MR_trace_counts_file) {
        name = MR_copy_string(MR_trace_counts_file);
        keep = MR_TRUE;
    } else {
        char    *s;

        // If no trace counts file name is provided, then we generate
        // a file name.

        // 100 bytes must be enough for the process id, dots and '\0'.
        name_len = strlen(MERCURY_TRACE_COUNTS_PREFIX) + strlen(program_name)
            + 100;
        name = MR_malloc(name_len);
        MR_snprintf(name, name_len, ".%s.%s.%d", MERCURY_TRACE_COUNTS_PREFIX,
            program_name, getpid());

        // Make sure name is an acceptable filename.
        // XXX WINDOWS: this is not the right thing to do on Windows.
        for (s = name; *s != '\0'; s++) {
            if (*s == '/') {
                *s = '_';
            }
        }
    }

    // XXX WINDOWS: probably wants to be _wfopen on Windows.
    fp = fopen(name, "w");
    if (fp != NULL) {
        unsigned    num_written;

        num_written = MR_trace_write_label_exec_counts(fp,
            program_name, MR_coverage_test_enabled);
        (void) fclose(fp);

        if (num_written == 0 && !keep) {
            // We did not write out any trace counts, so there is nothing
            // to gather.
            (void) unlink(name);
            summarize = MR_FALSE;
        }
    } else {
        fprintf(stderr, "%s: %s\n", name,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
        // You can't summarize a file list if you can't create
        // one of its files.
        summarize = MR_FALSE;
    }

    free(name);

    // XXX WINDOWS: the following assumes the use of a POSIX-style shell.
    // It will not work on non-Cygwin Windows.
    if (summarize) {
        char        *cmd;
        unsigned    cmd_len;
        int         summary_status;
        int         mv_status;
        int         unlink_status;
        int         i;
        const char  *old_options;

        // 30 bytes must be enough for the dot, the value of i, and space.
        name_len = strlen(MR_trace_count_summary_file) + 30;
        name = MR_malloc(name_len);

        cmd_len = strlen(MR_trace_count_summary_cmd) + 4;
        cmd_len += strlen(MR_trace_count_summary_file)
            + strlen(TEMP_SUFFIX) + 1;
        cmd_len += (MR_trace_count_summary_max + 1) * name_len;
        cmd_len += 100;

        cmd = MR_malloc(cmd_len);

        strcpy(cmd, MR_trace_count_summary_cmd);
        strcat(cmd, " -o ");
        strcat(cmd, MR_trace_count_summary_file);
        strcat(cmd, TEMP_SUFFIX);
        strcat(cmd, " ");
        strcat(cmd, MR_trace_count_summary_file);

        for (i = 1; i <= MR_trace_count_summary_max; i++) {
            MR_snprintf(name, name_len, "%s.%d",
                MR_trace_count_summary_file, i);
            strcat(cmd, " ");
            strcat(cmd, name);
        }

        strcat(cmd, " > /dev/null 2>&1");

        old_options = getenv("MERCURY_OPTIONS");
        if (old_options != NULL) {
            (void) MR_setenv("MERCURY_OPTIONS", "", MR_TRUE);
            summary_status = system(cmd);
            (void) MR_setenv("MERCURY_OPTIONS", old_options, MR_TRUE);
        } else {
            summary_status = system(cmd);
        }

        if (summary_status == 0) {
            strcpy(cmd, "mv ");
            strcat(cmd, MR_trace_count_summary_file);
            strcat(cmd, TEMP_SUFFIX);
            strcat(cmd, " ");
            strcat(cmd, MR_trace_count_summary_file);
            mv_status = system(cmd);

            if (mv_status == 0) {
                // Delete all files whose data is now in the summary file.
                for (i = 1; i <= MR_trace_count_summary_max; i++) {
                    MR_snprintf(name, name_len, "%s.%d",
                        MR_trace_count_summary_file, i);
                    unlink_status = unlink(name);
                    if (unlink_status != 0) {
                        MR_fatal_error(
                            "couldn't create summary of trace data");
                    }
                }
            } else {
                MR_fatal_error("couldn't create summary of trace data");
            }
        } else {
            MR_fatal_error("couldn't create summary of trace data");
        }

        free(name);
        free(cmd);
    }
}

unsigned
MR_trace_write_label_exec_counts(FILE *fp, const char *progname,
    MR_bool coverage_test)
{
    const MR_ModuleLayout       *module;
    const MR_ModuleFileLayout   *file;
    int                         num_modules;
    int                         num_files;
    int                         module_num;
    int                         file_num;
    unsigned                    num_written;

    MR_trace_name_count_port_ensure_init();

    fprintf(fp, "%s", MR_TRACE_COUNT_FILE_ID);
    if (coverage_test) {
        fputs("single_file(base_count_file_type(user_all, ", fp);
    } else {
        fputs("single_file(base_count_file_type(user_nonzero, ", fp);
    }

    MR_trace_write_string(fp, progname);
    fputs(")).\n", fp);

    num_modules = MR_module_info_next;
    num_written = 0;
    for (module_num = 0; module_num < num_modules; module_num++) {
        module = MR_module_infos[module_num];
        num_files = module->MR_ml_filename_count;

        fputs("module ", fp);
        MR_trace_write_quoted_atom(fp, module->MR_ml_name);
        fputc('\n', fp);

        for (file_num = 0; file_num < num_files; file_num++) {
            file = module->MR_ml_module_file_layout[file_num];
            num_written += MR_trace_write_label_exec_counts_for_file(fp,
                module, file, module->MR_ml_name, coverage_test);
        }
    }

    return num_written;
}

static unsigned
MR_trace_write_label_exec_counts_for_file(FILE *fp,
    const MR_ModuleLayout *module, const MR_ModuleFileLayout *file,
    const char *module_name, MR_bool coverage_test)
{
    const MR_LabelLayout        *label;
    const MR_ProcLayout         *prev_proc;
    const MR_ProcLayout         *proc;
    const MR_UserProcId         *id;
    MR_TracePort                port;
    int                         num_labels;
    int                         label_num;
    int                         label_index;
    unsigned                    num_written;
    MR_Unsigned                 exec_count;
    MR_PathPort                 path_port;

    fputs("file ", fp);
    MR_trace_write_quoted_atom(fp, file->MR_mfl_filename);
    fputc('\n', fp);

    prev_proc = NULL;
    num_labels = file->MR_mfl_label_count;
    num_written = 0;
    for (label_num = 0; label_num < num_labels; label_num++) {
        label = file->MR_mfl_label_layout[label_num];
        proc = label->MR_sll_entry;
        label_index = label->MR_sll_label_num_in_module;
        exec_count = module->MR_ml_label_exec_count[label_index];
        if (! MR_PROC_LAYOUT_IS_UCI(proc) && label_index > 0 &&
            (exec_count > 0 || coverage_test))
        {
            num_written++;

            id = &proc->MR_sle_user;
            if (proc != prev_proc) {
                if (MR_strdiff(module_name, id->MR_user_def_module)) {
                    MR_fatal_error(
                        "MR_trace_write_label_exec_counts_for_file: "
                        "module name mismatch");
                }

                if (id->MR_user_pred_or_func == MR_PREDICATE) {
                    fputs("pproc", fp);
                } else {
                    fputs("fproc", fp);
                }

                if (MR_strdiff(module_name, id->MR_user_decl_module)) {
                    // Turn pproc/fproc into pprocdecl/fprocdecl.
                    fputs("decl ", fp);
                    MR_trace_write_quoted_atom(fp, id->MR_user_decl_module);
                }

                fputc(' ', fp);
                MR_trace_write_quoted_atom(fp, id->MR_user_name);
                fprintf(fp, " %d %d\n",
                    id->MR_user_pred_form_arity, id->MR_user_mode);
            }

            port = label->MR_sll_port;
            path_port = MR_named_count_port[port];

            switch (path_port) {

                case PORT_ONLY:
                    fputs(MR_actual_port_names[port], fp);
                    break;

                case PATH_ONLY:
                    putc('<', fp);
                    fputs(MR_label_goal_path(label), fp);
                    putc('>', fp);
                    break;

                case PORT_AND_PATH:
                    fputs(MR_actual_port_names[port], fp);
                    putc(' ', fp);
                    putc('<', fp);
                    fputs(MR_label_goal_path(label), fp);
                    putc('>', fp);
                    break;

                default:
                    MR_fatal_error(
                        "MR_trace_write_label_exec_counts_for_file: "
                        "bad path_port");
                    break;
            }

            putc(' ', fp);
            fprintf(fp, "%d", file->MR_mfl_label_lineno[label_num]);

            if (exec_count > 0) {
                putc(' ', fp);
                fprintf(fp, "%" MR_INTEGER_LENGTH_MODIFIER "u", exec_count);
            }

            putc('\n', fp);

            prev_proc = proc;
        }
    }

    return num_written;
}

void
MR_trace_name_count_port_ensure_init()
{
    static MR_bool  done = MR_FALSE;

    MR_do_init_modules_debugger();

    if (! done) {
        MR_TracePort    port;

        for (port = MR_PORT_CALL; port <= MR_PORT_NONE; port++) {
            MR_named_count_port[port] = PATH_ONLY;
        }

        MR_named_count_port[MR_PORT_CALL] = PORT_ONLY;
        MR_named_count_port[MR_PORT_EXIT] = PORT_ONLY;
        MR_named_count_port[MR_PORT_REDO] = PORT_ONLY;
        MR_named_count_port[MR_PORT_FAIL] = PORT_ONLY;

        MR_named_count_port[MR_PORT_NEG_ENTER] = PORT_AND_PATH;
        MR_named_count_port[MR_PORT_NEG_SUCCESS] = PORT_AND_PATH;
        MR_named_count_port[MR_PORT_NEG_FAILURE] = PORT_AND_PATH;

        done = MR_TRUE;
    }
}

// The output of this is supposed to be equivalent to term_io.quote_atom
// except that it always uses quotes, even if not strictly necessary.
//
// XXX term_io does *not* escape codepoints >= 0x80 whereas this always
// does - juliensf.
//
static void
MR_trace_write_quoted_atom(FILE *fp, const char *atom)
{
    const char *c;

    fputc('\'', fp);
    for (c = atom; *c != '\0'; c++) {
        switch (*c) {
            case '\'':
                fputs("\\'", fp);
                break;
            case '"':
                fputs("\\\"", fp);
                break;
            case '\\':
                fputs("\\\\", fp);
                break;
            case '\n':
                fputs("\\n", fp);
                break;
            case '\t':
                fputs("\\t", fp);
                break;
            case '\b':
                fputs("\\b", fp);
                break;
            case '\a':
                fputs("\\a", fp);
                break;
            case '\r':
                fputs("\\r", fp);
                break;
            case '\f':
                fputs("\\f", fp);
                break;
            case '\v':
                fputs("\\v", fp);
                break;
            default:
                // This assumes isalnum is the same as char.isalnum.
                // The line noise is the equivalent of
                // is_mercury_punctuation_char in library/term_io.m
                // and compiler/parse_tree_out_pragma.m; any changes here
                // may require similar changes there.

                if (MR_isalnum(*c) ||
                    strchr(" !@#$%^&*()-_+=`~{}[];:'\"<>.,/?\\|", *c))
                {
                    fputc(*c, fp);
                } else {
                    fprintf(fp, "\\%03o\\", *c);
                }
                break;
        }
    }

    fputc('\'', fp);
}

// The output of this is supposed to be equivalent to writing out a string.

static void
MR_trace_write_string(FILE *fp, const char *atom)
{
    const char *c;

    fputc('\"', fp);
    for (c = atom; *c != '\0'; c++) {
        switch (*c) {
            case '"':
                fputs("\\\"", fp);
                break;
            case '\\':
                fputs("\\\\", fp);
                break;
            case '\n':
                fputs("\\n", fp);
                break;
            case '\t':
                fputs("\\t", fp);
                break;
            case '\b':
                fputs("\\b", fp);
                break;
            case '\a':
                fputs("\\a", fp);
                break;
            case '\r':
                fputs("\\b", fp);
                break;
            case '\f':
                fputs("\\f", fp);
                break;
            case '\v':
                fputs("\\v", fp);
                break;
            default:
                fputc(*c, fp);
                break;
        }
    }

    fputc('\"', fp);
}

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with the actions executed at the start
// and end of execution.

#ifdef  MR_TABLE_DEBUG
MR_bool MR_saved_tabledebug;
#endif

void
MR_trace_init(void)
{
#ifdef  MR_TABLE_DEBUG
    // We don't want to see any tabling debugging messages from
    // initialization code about entering and leaving commit goals.

    MR_saved_tabledebug = MR_tabledebug;
    MR_tabledebug = MR_FALSE;
#endif

#ifdef MR_USE_EXTERNAL_DEBUGGER
    if (MR_trace_handler == MR_TRACE_EXTERNAL) {
        if (MR_address_of_trace_init_external != NULL) {
            MR_address_of_trace_init_external();
        } else {
            MR_tracing_not_enabled();
        }
    }
#endif
}

void
MR_trace_final(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
    if (MR_trace_handler == MR_TRACE_EXTERNAL) {
        if (MR_address_of_trace_final_external != NULL) {
            MR_address_of_trace_final_external();
        } else {
            MR_tracing_not_enabled();
        }
    }
#endif

    // If mdb started a window, make sure it dies now.
    if (MR_trace_shutdown != NULL) {
        (*MR_trace_shutdown)();
    }
}

void
MR_trace_start(MR_bool enabled)
{
    MR_trace_event_number = 0;
    MR_trace_call_seqno = 0;
    MR_trace_call_depth = 0;
    MR_trace_from_full = MR_TRUE;
    MR_debug_enabled = enabled;
    MR_update_trace_func_enabled();

#ifdef  MR_TABLE_DEBUG
    // Restore the value saved by MR_trace_init.

    MR_tabledebug = MR_saved_tabledebug;
#endif

    // Install the SIGINT signal handler.
    // We only do this if tracing is enabled, and only for the
    // internal debugger. (This is a bit conservative: it might work fine
    // for the external debugger too, but I'm just not certain of that.)

    if (enabled &&
        MR_address_of_trace_interrupt_handler != NULL &&
        MR_trace_handler == MR_TRACE_INTERNAL)
    {
        MR_setup_signal(SIGINT,
            (MR_Code *) MR_address_of_trace_interrupt_handler,
            MR_FALSE, "mdb: cannot install SIGINT signal handler");
    }
}

void
MR_trace_end(void)
{
    MR_debug_enabled = MR_FALSE;
    MR_update_trace_func_enabled();
}

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with the standardization of event and
// call sequence numbers. We use standardized event and call numbers to
// reduce the number of .exp files we need to create for debugger test cases.

#define MR_STANDARD_HASH_TABLE_SIZE 1024

typedef struct {
    MR_Unsigned   MR_std_orig_number;
    MR_Unsigned   MR_std_std_number;
} MR_Standard_Hash_Record;

static const void *
MR_get_orig_number(const void *record)
{
    return (const void *)
        ((MR_Standard_Hash_Record *) record)->MR_std_orig_number;
}

static int
MR_hash_orig_number(const void *orig_number)
{
    return (int) (((MR_Unsigned) orig_number) % MR_STANDARD_HASH_TABLE_SIZE);
}

static MR_bool
MR_equal_orig_number(const void *orig_number1, const void *orig_number2)
{
    return (MR_Unsigned) orig_number1 == (MR_Unsigned) orig_number2;
}

static MR_Hash_Table MR_standard_event_num_table = {
    MR_STANDARD_HASH_TABLE_SIZE, NULL,
    MR_get_orig_number, MR_hash_orig_number, MR_equal_orig_number
};

static MR_Hash_Table MR_standard_call_num_table = {
    MR_STANDARD_HASH_TABLE_SIZE, NULL,
    MR_get_orig_number, MR_hash_orig_number, MR_equal_orig_number
};

static MR_bool  MR_init_event_num_hash = MR_FALSE;
static MR_bool  MR_init_call_num_hash = MR_FALSE;

static int      MR_next_std_event_num = 1;
static int      MR_next_std_call_num = 1;

static MR_Unsigned
MR_standardize_num(MR_Unsigned num, MR_Hash_Table *table_ptr,
    MR_bool *init_ptr, int *next_ptr)
{
    const MR_Standard_Hash_Record   *record;
    MR_Standard_Hash_Record         *new_record;
    int                             std_num;

    if (! *init_ptr) {
        *init_ptr = MR_TRUE;
        MR_init_hash_table(*table_ptr);
    }

    record = MR_lookup_hash_table(*table_ptr, num);
    if (record != NULL) {
        return record->MR_std_std_number;
    }

    std_num = *next_ptr;
    (*next_ptr)++;

    new_record = MR_GC_NEW(MR_Standard_Hash_Record);
    new_record->MR_std_orig_number = num;
    new_record->MR_std_std_number = std_num;
    (void) MR_insert_hash_table(*table_ptr, new_record);
    return std_num;
}

MR_Unsigned
MR_standardize_event_num(MR_Unsigned event_num)
{
    return MR_standardize_num(event_num, &MR_standard_event_num_table,
        &MR_init_event_num_hash, &MR_next_std_event_num);
}

MR_Unsigned
MR_standardize_call_num(MR_Unsigned call_num)
{
    return MR_standardize_num(call_num, &MR_standard_call_num_table,
        &MR_init_call_num_hash, &MR_next_std_call_num);
}

////////////////////////////////////////////////////////////////////////////
// This section of this file reports on trace events so far,
// for use in messages about abnormal program termination.

char    *MR_trace_report_msg = NULL;

void
MR_trace_report(FILE *fp)
{
    if (MR_trace_event_number > 0) {
        // This means that the executable was compiled with tracing,
        // which implies that the user wants trace info on abort.

        if (MR_trace_report_msg != NULL) {
            fprintf(fp, "%s\n", MR_trace_report_msg);
        }

        if (MR_standardize_event_details) {
            fprintf(fp, "Last trace event was event #E%ld.\n",
                (long) MR_standardize_event_num(MR_trace_event_number));
        } else {
            fprintf(fp, "Last trace event was event #%ld.\n",
                (long) MR_trace_event_number);
        }

#ifdef  MR_TRACE_HISTOGRAM
        {
            FILE    *hfp;
            char    errbuf[MR_STRERROR_BUF_SIZE];

            hfp = fopen(MR_TRACE_HISTOGRAM_FILENAME, "w");
            if (hfp != NULL) {
                MR_trace_print_histogram(hfp, "All-inclusive",
                    MR_trace_histogram_all, MR_trace_histogram_hwm);
                if (fclose(hfp) == 0) {
                    fprintf(fp, "Event histogram put into file `%s'.\n",
                        MR_TRACE_HISTOGRAM_FILENAME);
                } else {
                    fprintf(fp, "Cannot put event histogram into `%s': %s."
                        MR_TRACE_HISTOGRAM_FILENAME,
                        MR_strerror(errno, errbuf, sizeof(errbuf)));
                }
            } else {
                fprintf(fp, "Cannot open `%s': %s.\n"
                    MR_TRACE_HISTOGRAM_FILENAME,
                    MR_strerror(errno, errbuf, sizeof(errbuf)));
            }
        }
#endif  // MR_TRACE_HISTOGRAM
    }
}

void
MR_trace_report_raw(int fd)
{
    char    buf[80];    // That ought to be more than long enough.
    int     ret;

    if (MR_trace_event_number > 0) {
        // This means that the executable was compiled with tracing,
        // which implies that the user wants trace info on abort.

        if (MR_trace_report_msg != NULL) {
            do {
                // XXX We don't handle successful but partial writes.
                ret = write(fd, MR_trace_report_msg,
                    strlen(MR_trace_report_msg));
            } while (ret == -1 && MR_is_eintr(errno));
        }

        if (MR_standardize_event_details) {
            sprintf(buf, "Last trace event was event #E%ld.\n",
                (long) MR_standardize_event_num(MR_trace_event_number));
        } else {
            sprintf(buf, "Last trace event was event #%ld.\n",
                (long) MR_trace_event_number);
        }
        do {
            // XXX We don't handle successful but partial writes.
            ret = write(fd, buf, strlen(buf));
        } while (ret == -1 && MR_is_eintr(errno));
    }
}

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with I/O actions.

MR_bool
MR_trace_get_action(MR_IoActionNum action_number, MR_ConstString *proc_name_ptr,
    MR_Word *is_func_ptr, MR_bool *have_arg_infos_ptr, MR_Word *arg_list_ptr)
{
    const MR_TableIoEntry   *table_io_entry;
    const MR_ProcLayout     *proc_layout;
    MR_ConstString          proc_name;
    MR_Word                 is_func;
    int                     arity;
    int                     hv;
    MR_TrieNode             answer_block_trie;
    MR_Word                 *answer_block;

    if (! (MR_io_tabling_start <= action_number
        && action_number < MR_io_tabling_counter_hwm))
    {
        return MR_FALSE;
    }

    MR_TABLE_START_INT(NULL, MR_tabledebug, MR_FALSE,
        answer_block_trie, (MR_TrieNode) &MR_io_tabling_pointer,
        MR_io_tabling_start, action_number);
    answer_block = answer_block_trie->MR_answerblock;

    if (answer_block == NULL) {
        return MR_FALSE;
    }

    table_io_entry = (const MR_TableIoEntry *) answer_block[0];
    proc_layout = table_io_entry->MR_table_io_entry_proc;
    MR_generate_proc_name_from_layout(proc_layout, &proc_name, &arity,
        &is_func);
    *proc_name_ptr = proc_name;
    *is_func_ptr = is_func;

    if (table_io_entry->MR_table_io_entry_have_arg_infos) {
        int         filtered_arity;
        MR_Word     arg_list;
        MR_Word     arg;
        MR_TypeInfo *type_params;
        MR_TypeInfo type_info;

        *have_arg_infos_ptr = MR_TRUE;
        filtered_arity = table_io_entry->MR_table_io_entry_num_ptis;
        type_params = MR_materialize_answer_block_type_params(
            table_io_entry->MR_table_io_entry_type_params, answer_block,
            filtered_arity);

        MR_restore_transient_hp();
        arg_list = MR_list_empty();
        MR_save_transient_hp();
        for (hv = filtered_arity; hv >= 1; hv--) {
            type_info = MR_create_type_info(type_params,
                table_io_entry->MR_table_io_entry_ptis[hv - 1]);
            MR_restore_transient_hp();
            MR_new_univ_on_hp(arg, type_info, answer_block[hv]);
            arg_list = MR_univ_list_cons(arg, arg_list);
            MR_save_transient_hp();
        }

        MR_free(type_params);
        *arg_list_ptr = arg_list;
    } else {
        *have_arg_infos_ptr = MR_FALSE;
        // *arg_list_ptr is not meaningful when *have_arg_infos_ptr is false,
        // but setting it to the empty list makes it easier to catch any
        // caller that ignores that fact.
        *arg_list_ptr = MR_list_empty();
    }

    return MR_TRUE;
}

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with switching debugging on and off.
//
// XXX The code here is very similar to MR_TRACE_CALL_MERCURY in the header
// file. Look into merging the two pieces of code.

void
MR_turn_off_debug(MR_SavedDebugState *saved_state,
    MR_bool include_counter_vars)
{
    int i;

    saved_state->MR_sds_debug_enabled = MR_debug_enabled;
    saved_state->MR_sds_io_tabling_enabled = MR_io_tabling_enabled;
    MR_debug_enabled = MR_FALSE;
    MR_update_trace_func_enabled();
    MR_io_tabling_enabled = MR_FALSE;

    for (i = 0; i < MR_MAXFLAG ; i++) {
        saved_state->MR_sds_debugflags[i] = MR_debugflag[i];
        MR_debugflag[i] = MR_FALSE;
    }

    saved_state->MR_sds_include_counter_vars = include_counter_vars;
    saved_state->MR_sds_trace_call_seqno = MR_trace_call_seqno;
    saved_state->MR_sds_trace_call_depth = MR_trace_call_depth;
    saved_state->MR_sds_trace_event_number = MR_trace_event_number;

#if defined(MR_DEEP_PROFILING) && defined(MR_EXEC_TRACE)
    MR_disable_deep_profiling_in_debugger = MR_TRUE;
#endif
}

void
MR_turn_debug_back_on(const MR_SavedDebugState *saved_state)
{
    int i;

    MR_debug_enabled = saved_state->MR_sds_debug_enabled;
    MR_update_trace_func_enabled();
    MR_io_tabling_enabled = saved_state->MR_sds_io_tabling_enabled;

    for (i = 0; i < MR_MAXFLAG; i++) {
        MR_debugflag[i] = saved_state->MR_sds_debugflags[i];
    }

    if (saved_state->MR_sds_include_counter_vars) {
        MR_trace_call_seqno = saved_state->MR_sds_trace_call_seqno;
        MR_trace_call_depth = saved_state->MR_sds_trace_call_depth;
        MR_trace_event_number = saved_state->MR_sds_trace_event_number;
    }

#if defined(MR_DEEP_PROFILING) && defined(MR_EXEC_TRACE)
    MR_disable_deep_profiling_in_debugger = MR_FALSE;
#endif
}

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with recording the value of an exception.

static  MR_Word     MR_trace_exception_value = (MR_Word) NULL;

void
MR_trace_set_exception_value(MR_Word exception)
{
    MR_trace_exception_value = exception;
}

MR_Word
MR_trace_get_exception_value(void)
{
    return MR_trace_exception_value;
}

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with measuring the distribution of events
// across depths.

#ifdef  MR_TRACE_HISTOGRAM

void
MR_trace_print_histogram(FILE *fp, const char *which, int *histogram, int max)
{
    int i;

    fprintf(fp, "%s histogram\n", which);
    for (i = 1; i <= max; i++) {
        fprintf(fp, "depth %4d: %10d", i, histogram[i]);
        if (i + 1 <= max && histogram[i] != 0) {
            fprintf(fp, ", branching factor %7.2f\n",
                (float) histogram[i+1] / (float) histogram[i]);
        } else {
            fprintf(fp, "\n");
        }
    }
}

#endif  // MR_TRACE_HISTOGRAM

////////////////////////////////////////////////////////////////////////////
// This section of this file deals with statistics about which procedures
// are responsible for what fraction of I/O table entries.

#define MR_IO_TABLE_STATS_HASH_TABLE_SIZE 1024

typedef struct {
    const MR_ProcLayout     *MR_io_tabling_stats_proc;
    MR_Unsigned             MR_io_tabling_stats_count;
} MR_IO_Table_Stats_Hash_Record;

static const void *
MR_get_proc_layout(const void *record)
{
    return (const void *)
        ((MR_IO_Table_Stats_Hash_Record *) record)->MR_io_tabling_stats_proc;
}

static int
MR_hash_proc_layout(const void *proc)
{
    return (((MR_Unsigned) proc) >> 5) % MR_STANDARD_HASH_TABLE_SIZE;
}

static MR_bool
MR_equal_proc_layout(const void *proc1, const void *proc2)
{
    return (const MR_ProcLayout *) proc1 == (const MR_ProcLayout *) proc2;
}

static MR_Hash_Table MR_io_tabling_stats_table = {
    MR_IO_TABLE_STATS_HASH_TABLE_SIZE, NULL,
    MR_get_proc_layout, MR_hash_proc_layout, MR_equal_proc_layout
};

static  int                             MR_io_tabling_stats_sort_arena_next;
static  MR_IO_Table_Stats_Hash_Record   *MR_io_tabling_stats_sort_arena;

static  void    MR_add_to_sort_arena(const void *addr);
static  int     MR_compare_in_sort_arena(const void *addr1, const void *addr2);

static void
MR_add_to_sort_arena(const void *addr)
{
    const MR_IO_Table_Stats_Hash_Record *record;
    int                                 next;

    record = (const MR_IO_Table_Stats_Hash_Record *) addr;
    next = MR_io_tabling_stats_sort_arena_next;
    MR_io_tabling_stats_sort_arena[next].MR_io_tabling_stats_proc =
        record->MR_io_tabling_stats_proc;
    MR_io_tabling_stats_sort_arena[next].MR_io_tabling_stats_count =
        record->MR_io_tabling_stats_count;
    MR_io_tabling_stats_sort_arena_next++;
}

static int
MR_compare_in_sort_arena(const void *addr1, const void *addr2)
{
    const MR_IO_Table_Stats_Hash_Record *record1;
    const MR_IO_Table_Stats_Hash_Record *record2;

    record1 = (const MR_IO_Table_Stats_Hash_Record *) addr1;
    record2 = (const MR_IO_Table_Stats_Hash_Record *) addr2;
    return record2->MR_io_tabling_stats_count -
        record1->MR_io_tabling_stats_count;
}

void
MR_io_tabling_stats(FILE *fp)
{
    const MR_TableIoEntry           *table_io_entry;
    const MR_ProcLayout             *proc_layout;
    MR_ConstString                  proc_name;
    int                             arity;
    MR_Word                         is_func;
    MR_TrieNode                     answer_block_trie;
    MR_Word                         *answer_block;
    MR_Hash_Table                   hash_table;
    MR_IO_Table_Stats_Hash_Record   *hash_record;
    MR_IO_Table_Stats_Hash_Record   *record;
    int                             num_entries;
    int                             count;
    int                             i;

    // Create a fresh new hash table, separate from the table created by
    // any previous call to this function. We can't use structure assignment,
    // as that causes gcc 3.2 to throw a fit.

    hash_table.MR_ht_size  = MR_io_tabling_stats_table.MR_ht_size;
    hash_table.MR_ht_store = NULL;
    hash_table.MR_ht_key   = MR_io_tabling_stats_table.MR_ht_key;
    hash_table.MR_ht_hash  = MR_io_tabling_stats_table.MR_ht_hash;
    hash_table.MR_ht_equal = MR_io_tabling_stats_table.MR_ht_equal;
    MR_init_hash_table(hash_table);
    num_entries = 0;

    for (i = MR_io_tabling_start; i < MR_io_tabling_counter_hwm; i++) {
        MR_TABLE_START_INT(NULL, MR_FALSE, MR_FALSE, answer_block_trie,
            (MR_TrieNode) &MR_io_tabling_pointer, MR_io_tabling_start, i);
        answer_block = answer_block_trie->MR_answerblock;

        if (answer_block == NULL) {
            continue;
        }

        table_io_entry = (const MR_TableIoEntry *) answer_block[0];
        proc_layout = table_io_entry->MR_table_io_entry_proc;

        hash_record = MR_lookup_hash_table(hash_table, proc_layout);
        if (hash_record == NULL) {
            hash_record = MR_GC_NEW(MR_IO_Table_Stats_Hash_Record);
            hash_record->MR_io_tabling_stats_proc = proc_layout;
            hash_record->MR_io_tabling_stats_count = 1;
            (void) MR_insert_hash_table(hash_table, hash_record);
            num_entries++;
        } else {
            hash_record->MR_io_tabling_stats_count++;
        }
    }

    MR_io_tabling_stats_sort_arena =
        MR_GC_NEW_ARRAY(MR_IO_Table_Stats_Hash_Record, num_entries);
    MR_io_tabling_stats_sort_arena_next = 0;
    MR_process_all_entries(hash_table, MR_add_to_sort_arena);

    if (MR_io_tabling_stats_sort_arena_next != num_entries) {
        MR_fatal_error("MR_io_tabling_stats: num_entries mismatch");
    }

    qsort(MR_io_tabling_stats_sort_arena, num_entries,
        sizeof(MR_IO_Table_Stats_Hash_Record), MR_compare_in_sort_arena);

    for (i = 0; i < num_entries; i++) {
        record = &MR_io_tabling_stats_sort_arena[i];
        proc_layout = record->MR_io_tabling_stats_proc;
        count = record->MR_io_tabling_stats_count;
        MR_generate_proc_name_from_layout(proc_layout, &proc_name, &arity,
            &is_func);

        fprintf(fp, "%8d %4s %s/%d\n", count, (is_func ? "func" : "pred"),
            proc_name, arity);
    }
}

////////////////////////////////////////////////////////////////////////////
// This section of this file maps proc layouts to materialized procedure
// body representations.
//
// We record information about procedure representations in a hash table
// that is indexed by the proc layout address.
//
// This table is used by the declarative debugger. Since the declarative
// debugger can be required in any grade, we always include this table, but
// it is initialized (and the bulk of its memory allocated) only if the
// declarative debugger is in fact invoked.

#define PROC_DEFN_REP_TABLE_SIZE (1 << 16)   // 64k

typedef struct {
    const MR_ProcLayout     *plr_layout;
    MR_Word                 plr_rep;
} MR_ProcLayoutRep;

static  void                MR_do_init_proc_defn_rep_table(void);
static  const void          *MR_proc_layout_rep_key(const void *proc_layout);
static  int                 MR_hash_proc_layout_addr(const void *addr);
static  MR_bool             MR_equal_proc_layouts(const void *addr1,
                                const void *addr2);

static  MR_Hash_Table       proc_defn_rep_table = {
                                PROC_DEFN_REP_TABLE_SIZE,
                                NULL,
                                MR_proc_layout_rep_key,
                                MR_hash_proc_layout_addr,
                                MR_equal_proc_layouts
                            };

static void
MR_do_init_proc_defn_rep_table(void)
{
    static  MR_bool done = MR_FALSE;

    if (!done) {
        MR_init_hash_table(proc_defn_rep_table);
        done = MR_TRUE;
    }
}

void
MR_insert_proc_defn_rep(const MR_ProcLayout *proc_layout,
    MR_Word proc_defn_rep)
{
    MR_ProcLayoutRep    *layout_rep;

    MR_do_init_proc_defn_rep_table();

    layout_rep = MR_GC_NEW(MR_ProcLayoutRep);
    layout_rep->plr_layout = proc_layout;
    layout_rep->plr_rep = proc_defn_rep;

    (void) MR_insert_hash_table(proc_defn_rep_table, layout_rep);

#ifdef  MR_DEBUG_PROC_REP
    if (MR_progdebug) {
        printf("insert: layout %p, rep %x, pair %p\n",
            proc_layout, proc_defn_rep, layout_rep);
    }
#endif
}

MR_Word
MR_lookup_proc_defn_rep(const MR_ProcLayout *proc_layout)
{
    const MR_ProcLayoutRep  *layout_rep;

    MR_do_init_proc_defn_rep_table();

    layout_rep = MR_lookup_hash_table(proc_defn_rep_table, proc_layout);
    if (layout_rep == NULL) {
#ifdef  MR_DEBUG_PROC_REP
        if (MR_progdebug) {
            printf("search for layout %p: not found\n", proc_layout);
        }
#endif

        return 0;
    }

#ifdef  MR_DEBUG_PROC_REP
    if (MR_progdebug) {
        printf("search for layout %p: found pair %p, rep %x\n",
            proc_layout, layout_rep, layout_rep->plr_rep);
    }
#endif

    return layout_rep->plr_rep;
}

static const void *
MR_proc_layout_rep_key(const void *pair)
{
    MR_ProcLayoutRep    *proc_layout_rep;

    proc_layout_rep = (MR_ProcLayoutRep *) pair;
    if (proc_layout_rep == NULL) {
        return NULL;
    } else {
        return (const void *) proc_layout_rep->plr_layout;
    }
}

static int
MR_hash_proc_layout_addr(const void *addr)
{
    return (((MR_Unsigned) addr) >> 5) % PROC_DEFN_REP_TABLE_SIZE;
}

static MR_bool
MR_equal_proc_layouts(const void *addr1, const void *addr2)
{
    return ((const MR_ProcLayout *) addr1) == ((const MR_ProcLayout *) addr2);
}

////////////////////////////////////////////////////////////////////////////
// This section of this file provides the code that generated redo events.
// Its labels are pointed to by the temp frames pushed onto the nondet stack
// by model_non procedures when they exit.

#ifndef MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_do_trace_redo_fail_shallow);
MR_define_extern_entry(MR_do_trace_redo_fail_deep);

MR_BEGIN_MODULE(MR_trace_labels_module)
    MR_init_entry_an(MR_do_trace_redo_fail_shallow);
    MR_init_entry_an(MR_do_trace_redo_fail_deep);
MR_BEGIN_CODE

MR_define_entry(MR_do_trace_redo_fail_shallow);
    // If this code ever needs changing, you may also need to change
    // the code in extras/exceptions/exception.m similarly.

    if (MR_redo_fromfull_framevar(MR_redofr_slot(MR_curfr))) {
        MR_Code *MR_jumpaddr;
        MR_save_transient_registers();
        MR_jumpaddr = MR_trace((const MR_LabelLayout *)
            MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
        MR_restore_transient_registers();
        if (MR_jumpaddr != NULL) {
            MR_GOTO(MR_jumpaddr);
        }
    }
    MR_fail();

MR_define_entry(MR_do_trace_redo_fail_deep);
#if 0
    // For use in case this ever needs to be debugged again.
    printf("MR_curfr = %p\n", MR_curfr);
    printf("MR_redofr_slot(MR_curfr) = %p\n", MR_redofr_slot(MR_curfr));
    printf("&MR_redo_layout_framevar(MR_redofr_slot(MR_curfr) = %p\n",
        &MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
    printf("MR_redo_layout_framevar(MR_redofr_slot(MR_curfr) = %p\n",
        MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
#endif
    // If this code ever needs changing, you may also need to change
    // the code in library/exception.m similarly.

    {
        MR_Code *MR_jumpaddr;

        MR_save_transient_registers();
        MR_jumpaddr = MR_trace((const MR_LabelLayout *)
            MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
        MR_restore_transient_registers();
        if (MR_jumpaddr != NULL) {
            MR_GOTO(MR_jumpaddr);
        }
    }
    MR_fail();

MR_END_MODULE

#endif // !MR_HIGHLEVEL_CODE

// Forward decls to suppress gcc warnings.
void mercury_sys_init_trace_init(void);
void mercury_sys_init_trace_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_trace_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_trace_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    MR_trace_labels_module();
#endif
}

void mercury_sys_init_trace_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_trace_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
}
#endif
