/*
** vim: ts=4 sw=4 expandtab
*/
/*
INIT mercury_sys_init_trace
ENDINIT
*/
/*
** Copyright (C) 1997-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_base.c implements the interface between the main part
** of the runtime system (mainly mercury_wrapper.c) and the part of the
** tracing subsystem that has to be present even if no module in the program
** is compiled with execution tracing.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace_base.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include "mercury_hash_table.h"
#include "mercury_layout_util.h"    /* for MR_generate_proc_name_from_layout */
#include "mercury_runtime_util.h"   /* for strerror() on some systems */
#include "mercury_signal.h"         /* for MR_setup_signal() */
#include "mercury_builtin_types.h"  /* for type_ctor_infos */
#include "mercury_array_macros.h"   /* for type_ctor_infos */
#include <signal.h>                 /* for SIGINT */
#include <stdio.h>
#include <errno.h>

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>       /* for the write system call */
#endif

#ifdef MR_HAVE_SYS_WAIT_H
  #include <sys/wait.h>     /* for the wait system call */
#endif

void        (*MR_trace_shutdown)(void) = NULL;

MR_bool         MR_debug_ever_enabled = MR_FALSE;
MR_bool         MR_debug_enabled = MR_FALSE;
MR_bool         MR_trace_count_enabled = MR_FALSE;
MR_bool         MR_trace_func_enabled = MR_FALSE;
MR_Code         *(*volatile MR_selected_trace_func_ptr)(
                    const MR_Label_Layout *);
MR_Unsigned     MR_trace_call_seqno = 0;
MR_Unsigned     MR_trace_call_depth = 0;
MR_Unsigned     MR_trace_event_number = 0;
MR_bool         MR_trace_from_full = MR_TRUE;
MR_bool         MR_standardize_event_details = MR_FALSE;
MR_Trace_Type   MR_trace_handler = MR_TRACE_INTERNAL;

MR_bool         MR_trace_unhide_events = MR_FALSE;
MR_bool         MR_trace_have_unhid_events = MR_FALSE;

/*
** I/O tabling is documented in library/table_builtin.m
*/

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

const char  *MR_port_names[] =
{
    "CALL",
    "EXIT",
    "REDO",
    "FAIL",
    "EXCP",
    "COND",
    "THEN",
    "ELSE",
    "NEGE",
    "NEGS",
    "NEGF",
    "DISJ",
    "SWTC",
    "FRST",
    "LATR",
    "NONE",
};

static  const void  *MR_get_orig_number(const void *record);
static  int         MR_hash_orig_number(const void *orig_number);
static  MR_bool     MR_equal_orig_number(const void *orig_number1,
                        const void *orig_number2);
static  MR_Unsigned MR_standardize_num(MR_Unsigned num,
                        MR_Hash_Table *table_ptr, MR_bool *init_ptr,
                        int *next_ptr);

MR_Code *
MR_trace(const MR_Label_Layout *layout)
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
        "option to c2init when you do so.  "
        "If you are using mmake, you\n"
        "can do this by including "
        "`-t' (or `--trace') in C2INITFLAGS.\n"
        "For further details, please see the \"Debugging\" chapter "
        "of the\n"
        "Mercury User's Guide.\n");
}

MR_Code *
MR_trace_fake(const MR_Label_Layout *layout)
{
    MR_tracing_not_enabled();
    /*NOTREACHED*/
    return NULL;
}

MR_Code *
MR_trace_count(const MR_Label_Layout *label_layout)
{
    const MR_Module_Layout  *module_layout;
    const MR_Proc_Layout    *proc_layout;
    MR_uint_least16_t       label_number;

    proc_layout = label_layout->MR_sll_entry;
    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)) {
        MR_fatal_error("MR_trace_count: no exec trace");
    }

    module_layout = proc_layout->MR_sle_module_layout;
    label_number = label_layout->MR_sll_label_num_in_module;
    if (label_number >= module_layout->MR_ml_num_label_exec_counts) {
        MR_fatal_error("MR_trace_count: invalid label number");
    }

#ifdef  MR_TRACE_COUNT_DEBUG
    {
        const MR_Label_Layout   *call_label_layout;
        MR_uint_least16_t       call_label_number;

        call_label_layout = proc_layout->MR_sle_call_label;
        if (label_layout != call_label_layout) {
            /*
            ** We should only get here if we have executed the call label,
            ** which means its count should be nonzero.
            */

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

    ++module_layout->MR_ml_label_exec_count[label_number];
    return NULL;
}

#define INIT_MODULE_TABLE_SIZE  10

const MR_Module_Layout  **MR_module_infos;
int                     MR_module_info_next = 0;
int                     MR_module_info_max  = 0;

void
MR_insert_module_info_into_module_table(const MR_Module_Layout *module)
{
    int     slot;

    MR_GC_ensure_room_for_next(MR_module_info, const MR_Module_Layout *,
        INIT_MODULE_TABLE_SIZE);
    MR_prepare_insert_into_sorted(MR_module_infos, MR_module_info_next, slot,
        strcmp(MR_module_infos[slot]->MR_ml_name, module->MR_ml_name));

    MR_module_infos[slot] = module;
}

typedef enum {
    PATH_ONLY, PORT_ONLY, PORT_AND_PATH
} MR_PathPort;

static MR_PathPort MR_named_count_port[MR_PORT_NONE + 1];

static  void    MR_trace_write_quoted_atom(FILE *fp, const char *atom);
static  void    MR_trace_write_label_exec_counts(FILE *fp);

#define MERCURY_TRACE_COUNTS_PREFIX  "mercury_trace_counts"

void
MR_trace_write_label_exec_counts_to_file(void *dummy)
{
    FILE	*fp;
    int     len;
    char    *name;
    char    *s;

    /* 100 bytes must be enough for the process id, dots and '\0' */
    len = strlen(MERCURY_TRACE_COUNTS_PREFIX) + strlen(MR_progname) + 100;
    name = MR_malloc(len);
    snprintf(name, len, ".%s.%s.%d", MERCURY_TRACE_COUNTS_PREFIX, MR_progname,
        getpid());

    /* make sure name is an acceptable filename */
    for (s = name; *s != '\0'; s++) {
        if (*s == '/') {
            *s = ':';
        }
    }

    fp = fopen(name, "w");
    if (fp != NULL) {
        MR_do_init_modules_debugger();
        MR_trace_write_label_exec_counts(fp);
        (void) fclose(fp);
    } else {
        fprintf(stderr, "%s: %s\n", name, strerror(errno));
    }
}

/*      
** For every label reachable from the module table, write the id of the label
** and the number of times it has been executed to the specified file, with the
** exception of labels that haven't been executed.
*/ 

static void
MR_trace_write_label_exec_counts(FILE *fp)
{
    const MR_Module_Layout      *module;
    const MR_Module_File_Layout *file;
    const MR_Label_Layout       *label;
    const MR_Proc_Layout        *prev_proc;
    const MR_Proc_Layout        *proc;
    const MR_User_Proc_Id       *id;
    MR_Trace_Port               port;
    int                         num_modules;
    int                         num_files;
    int                         num_labels;
    int                         module_num;
    int                         file_num;
    int                         label_num;
    int                         label_index;
    MR_Unsigned                 exec_count;
    MR_PathPort                 path_port;

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

    prev_proc = NULL;
    num_modules = MR_module_info_next;
    for (module_num = 0; module_num < num_modules; module_num++) {
        module = MR_module_infos[module_num];
        num_files = module->MR_ml_filename_count;

        for (file_num = 0; file_num < num_files; file_num++) {
            file = module->MR_ml_module_file_layout[file_num];
            num_labels = file->MR_mfl_label_count;
            for (label_num = 0; label_num < num_labels; label_num++) {
                label = file->MR_mfl_label_layout[label_num];
                proc = label->MR_sll_entry;
                label_index = label->MR_sll_label_num_in_module;
                exec_count = module->MR_ml_label_exec_count[label_index];
                if (! MR_PROC_LAYOUT_IS_UCI(proc) &&
                    exec_count > 0 && label_index > 0)
                {
                    id = &proc->MR_sle_user;
                    if (proc != prev_proc) {
                        fprintf(fp, "proc ");
                        MR_trace_write_quoted_atom(fp,
                            id->MR_user_def_module);
                        fprintf(fp, " %c ",
                            ( id->MR_user_pred_or_func == MR_PREDICATE
                                ? 'p' : 'f'));
                        MR_trace_write_quoted_atom(fp,
                            id->MR_user_decl_module);
                        fputc(' ', fp);
                        MR_trace_write_quoted_atom(fp,
                            id->MR_user_name);
                        fprintf(fp, " %d %d\n",
                            id->MR_user_arity,
                            id->MR_user_mode);
                    }

                    port = label->MR_sll_port;
                    path_port = MR_named_count_port[port];

                    switch (path_port) {

                        case PORT_ONLY:
                            fprintf(fp, "%s %u\n",
                                MR_port_names[port], exec_count);
                            break;

                        case PATH_ONLY:
                            fprintf(fp, "<%s> %u\n",
                                MR_label_goal_path(label), exec_count);
                            break;

                        case PORT_AND_PATH:
                            fprintf(fp, "%s <%s> %u\n",
                                MR_port_names[port], MR_label_goal_path(label),
                                exec_count);
                            break;

                        default:
                            MR_fatal_error("MR_trace_write_label_exec_counts: "
                                "bad path_port");
                            break;
                    }

                    prev_proc = proc;
                }
            }
        }
    }
}

/*
** The output of this is supposed to be equivalent to term_io__quote_atom
** except that it always uses quotes, even if not strictly necessary.
*/
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
            default:
                /* This assumes isalnum is the same as char__isalnum.
                ** The line noise is the equivalent of 
                ** is_mercury_punctuation_char in library/term_io.m
                ** and compiler/mercury_to_mercury.m; any changes here
                ** may require similar changes there.
                */
                if (isalnum(*c) ||
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

#ifdef  MR_TABLE_DEBUG
MR_bool MR_saved_tabledebug;
#endif

void
MR_trace_init(void)
{
#ifdef  MR_TABLE_DEBUG
    /*
    ** We don't want to see any tabling debugging messages from
    ** initialization code about entering and leaving commit goals.
    */

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

    /*
    ** If mdb started a window, make sure it dies now.
    */
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
    /*
    ** Restore the value saved by MR_trace_init.
    */

    MR_tabledebug = MR_saved_tabledebug;
#endif

    /*
    ** Install the SIGINT signal handler.
    ** We only do this if tracing is enabled, and only
    ** for the internal debugger.  (This is a bit conservative:
    ** it might work fine for the external debugger too,
    ** but I'm just not certain of that.)
    */
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

static int MR_next_std_event_num = 1;
static int MR_next_std_call_num = 1;

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

char    *MR_trace_report_msg = NULL;

void
MR_trace_report(FILE *fp)
{
    if (MR_trace_event_number > 0) {
        /*
        ** This means that the executable was compiled with tracing,
        ** which implies that the user wants trace info on abort.
        */

        if (MR_trace_report_msg != NULL) {
            fprintf(fp, "%s\n", MR_trace_report_msg);
        }

        if (MR_standardize_event_details) {
            fprintf(fp, "Last trace event was event #E%ld.\n",
                (long) MR_standardize_event_num(
                    MR_trace_event_number));
        } else {
            fprintf(fp, "Last trace event was event #%ld.\n",
                (long) MR_trace_event_number);
        }

#ifdef  MR_TRACE_HISTOGRAM
        {
            FILE    *hfp;

            hfp = fopen(MR_TRACE_HISTOGRAM_FILENAME, "w");
            if (hfp != NULL) {
                MR_trace_print_histogram(hfp, "All-inclusive",
                    MR_trace_histogram_all, MR_trace_histogram_hwm);
                if (fclose(hfp) == 0) {
                    fprintf(fp, "Event histogram put into file `%s'.\n",
                        MR_TRACE_HISTOGRAM_FILENAME);
                } else {
                    fprintf(fp, "Cannot put event histogram into `%s': %s."
                        MR_TRACE_HISTOGRAM_FILENAME, strerror(errno));
                }
            } else {
                fprintf(fp, "Cannot open `%s': %s.\n"
                    MR_TRACE_HISTOGRAM_FILENAME, strerror(errno));
            }
        }
#endif  /* MR_TRACE_HISTOGRAM */
    }
}

void
MR_trace_report_raw(int fd)
{
    char    buf[80];    /* that ought to be more than long enough */

    if (MR_trace_event_number > 0) {
        /*
        ** This means that the executable was compiled with tracing,
        ** which implies that the user wants trace info on abort.
        */

        if (MR_trace_report_msg != NULL) {
            write(fd, MR_trace_report_msg, strlen(MR_trace_report_msg));
        }

        if (MR_standardize_event_details) {
            sprintf(buf, "Last trace event was event #E%ld.\n",
                (long) MR_standardize_event_num(MR_trace_event_number));
        } else {
            sprintf(buf, "Last trace event was event #%ld.\n",
                (long) MR_trace_event_number);
        }
        write(fd, buf, strlen(buf));
    }
}

const char *
MR_trace_get_action(int action_number, MR_ConstString *proc_name_ptr,
    MR_Word *is_func_ptr, MR_Word *arg_list_ptr)
{
    const MR_Table_Io_Decl  *table_io_decl;
    const MR_Proc_Layout    *proc_layout;
    MR_ConstString          proc_name;
    MR_Word                 is_func;
    MR_Word                 arg_list;
    MR_Word                 arg;
    int                     filtered_arity;
    int                     arity;
    int                     hv;
    MR_TrieNode             answer_block_trie;
    MR_Word                 *answer_block;
    MR_TypeInfo             *type_params;
    MR_TypeInfo             type_info;

    if (! (MR_io_tabling_start <= action_number
        && action_number < MR_io_tabling_counter_hwm))
    {
        return "I/O action number not in range";
    }

    MR_DEBUG_NEW_TABLE_START_INT(answer_block_trie,
        (MR_TrieNode) &MR_io_tabling_pointer,
        MR_io_tabling_start, action_number);
    answer_block = answer_block_trie->MR_answerblock;

    if (answer_block == NULL) {
        return "I/O action number not in range";
    }

    table_io_decl = (const MR_Table_Io_Decl *) answer_block[0];
    proc_layout = table_io_decl->MR_table_io_decl_proc;
    filtered_arity = table_io_decl->MR_table_io_decl_filtered_arity;

    MR_generate_proc_name_from_layout(proc_layout, &proc_name, &arity,
        &is_func);

    type_params = MR_materialize_answer_block_type_params(
        table_io_decl->MR_table_io_decl_type_params, answer_block,
        filtered_arity);

    MR_restore_transient_hp();
    arg_list = MR_list_empty();
    MR_save_transient_hp();
    for (hv = filtered_arity; hv >= 1; hv--) {
        type_info = MR_create_type_info(type_params,
            table_io_decl->MR_table_io_decl_ptis[hv - 1]);
        MR_restore_transient_hp();
        MR_new_univ_on_hp(arg, type_info, answer_block[hv]);
        arg_list = MR_univ_list_cons(arg, arg_list);
        MR_save_transient_hp();
    }

    MR_free(type_params);

    *proc_name_ptr = proc_name;
    *is_func_ptr = is_func;
    *arg_list_ptr = arg_list;
    return NULL;
}

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
}

void
MR_turn_debug_back_on(MR_SavedDebugState *saved_state)
{
    int i;

    MR_debug_enabled = saved_state->MR_sds_debug_enabled;
    MR_update_trace_func_enabled();
    MR_io_tabling_enabled = saved_state->MR_sds_io_tabling_enabled;

    for (i = 0; i < MR_MAXFLAG ; i++) {
        MR_debugflag[i] = saved_state->MR_sds_debugflags[i];
    }

    if (saved_state->MR_sds_include_counter_vars) {
        MR_trace_call_seqno = saved_state->MR_sds_trace_call_seqno;
        MR_trace_call_depth = saved_state->MR_sds_trace_call_depth;
        MR_trace_event_number = saved_state->MR_sds_trace_event_number;
    }
}

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

#endif  /* MR_TRACE_HISTOGRAM */

#ifndef MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_do_trace_redo_fail_shallow);
MR_define_extern_entry(MR_do_trace_redo_fail_deep);

MR_BEGIN_MODULE(MR_trace_labels_module)
    MR_init_entry_an(MR_do_trace_redo_fail_shallow);
    MR_init_entry_an(MR_do_trace_redo_fail_deep);
MR_BEGIN_CODE

MR_define_entry(MR_do_trace_redo_fail_shallow);
    /*
    ** If this code ever needs changing, you may also need to change
    ** the code in extras/exceptions/exception.m similarly.
    */
    if (MR_redo_fromfull_framevar(MR_redofr_slot(MR_curfr)))
    {
        MR_Code *MR_jumpaddr;
        MR_save_transient_registers();
        MR_jumpaddr = MR_trace((const MR_Label_Layout *)
            MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
        MR_restore_transient_registers();
        if (MR_jumpaddr != NULL) {
            MR_GOTO(MR_jumpaddr);
        }
    }
    MR_fail();

MR_define_entry(MR_do_trace_redo_fail_deep);
#if 0
    /* For use in case this ever needs to be debugged again. */
    printf("MR_curfr = %p\n", MR_curfr);
    printf("MR_redofr_slot(MR_curfr) = %p\n", MR_redofr_slot(MR_curfr));
    printf("&MR_redo_layout_framevar(MR_redofr_slot(MR_curfr) = %p\n",
        &MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
    printf("MR_redo_layout_framevar(MR_redofr_slot(MR_curfr) = %p\n",
        MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
#endif
    /*
    ** If this code ever needs changing, you may also need to change
    ** the code in library/exception.m similarly.
    */
    {
        MR_Code *MR_jumpaddr;

        MR_save_transient_registers();
        MR_jumpaddr = MR_trace((const MR_Label_Layout *)
            MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
        MR_restore_transient_registers();
        if (MR_jumpaddr != NULL) {
            MR_GOTO(MR_jumpaddr);
        }
    }
    MR_fail();

MR_END_MODULE

#endif /* !MR_HIGHLEVEL_CODE */

/* forward decls to suppress gcc warnings */
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
    /* no types to register */
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_trace_write_out_proc_statics(FILE *fp)
{
    /* no proc_statics to write out */
}
#endif
