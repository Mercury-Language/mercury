// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2008, 2010-2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Deep profiling module
//
// See ../deep_profiler/README for some pointers to documentation
// on deep profiling.
//
// Authors: conway, zs

// Turn on assertions, to protect the integrity of the generated data files.
#define MR_DEEP_CHECKS

#include "mercury_imp.h"
#include "mercury_ho_call.h"
#include "mercury_stack_layout.h"
#include "mercury_timing.h"
#include "mercury_prof_time.h"
#include "mercury_runtime_util.h"
#include "mercury_deep_profiling.h"
#include "mercury_deep_profiling_hand.h"
#include "mercury_file.h"

#ifdef MR_DEEP_PROFILING

#include <stdio.h>
#include <stdint.h>
#include <errno.h>

#ifdef  MR_EXEC_TRACE
MR_bool MR_disable_deep_profiling_in_debugger = MR_FALSE;
#endif

MR_CallSiteStatic   MR_main_parent_call_site_statics[1] =
{
    { MR_callsite_callback, NULL, NULL, "Mercury runtime", 0, "" }
};

MR_ProcStatic   MR_main_parent_proc_static =
{
    "Mercury runtime",
    0,
    MR_TRUE,
    1,
    &MR_main_parent_call_site_statics[0],
#ifdef  MR_USE_ACTIVATION_COUNTS
    0,
#endif
    NULL,
    -1,
    -1,
    -1
};

MR_ProcLayoutUser MR_main_parent_proc_layout =
{
    { MR_do_not_reached, MR_LONG_LVAL_TYPE_UNKNOWN, -1, MR_DETISM_DET },
    { MR_PREDICATE, "Mercury runtime", "Mercury runtime",
        "Mercury runtime", 0, 0 },
    NULL,
    &MR_main_parent_proc_static
};

MR_CallSiteDynamic  *MR_main_parent_call_site_dynamics[1] =
{
    NULL
};

MR_ProcDynamic      MR_main_parent_proc_dynamic =
{
    (MR_ProcLayout *) &MR_main_parent_proc_layout,
    &MR_main_parent_call_site_dynamics[0]
};

MR_CallSiteDynamic  MR_main_grandparent_call_site_dynamic =
{
    &MR_main_parent_proc_dynamic,
    {
#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    1,
  #else
    // The call count is computed from the other counts.
  #endif
    1, 0, 0, 0,
#endif
#ifdef MR_DEEP_PROFILING_TIMING
    0,
#endif
#ifdef MR_DEEP_PROFILING_MEMORY
    0, 0
#endif
    },
    0
};

MR_CallSiteDynamic  *MR_current_call_site_dynamic =
                        &MR_main_grandparent_call_site_dynamic;
MR_CallSiteDynamic  *MR_next_call_site_dynamic = NULL;
MR_CallSiteDynList  **MR_current_callback_site = (MR_CallSiteDynList **)
                        &MR_main_parent_call_site_dynamics[0];

volatile MR_bool    MR_inside_deep_profiling_code = MR_FALSE;
volatile unsigned   MR_quanta_inside_deep_profiling_code = 0;
volatile unsigned   MR_quanta_outside_deep_profiling_code = 0;

#ifdef  MR_DEEP_PROFILING_CALL_SEQ
unsigned            MR_deep_prof_cur_call_seq = 0;
#endif

#ifdef  MR_DEEP_PROFILING_STATISTICS

int     MR_deep_num_csd_nodes = 0;
int     MR_deep_num_pd_nodes = 0;
int     MR_deep_num_pd_array_slots = 0;
int     MR_deep_num_dynlist_nodes = 0;

int     MR_dictionary_search_lengths[MR_MAX_CLOSURE_LIST_LENGTH];
int     MR_closure_search_lengths[MR_MAX_CLOSURE_LIST_LENGTH];
int     MR_method_search_lengths[MR_MAX_CLOSURE_LIST_LENGTH];

int     MR_deep_prof_prep_normal_new = 0;
int     MR_deep_prof_prep_normal_old = 0;
int     MR_deep_prof_prep_special_new = 0;
int     MR_deep_prof_prep_special_old = 0;
int     MR_deep_prof_prep_ho_new = 0;
int     MR_deep_prof_prep_ho_old = 0;
int     MR_deep_prof_prep_method_new = 0;
int     MR_deep_prof_prep_method_old = 0;
int     MR_deep_prof_prep_callback_new = 0;
int     MR_deep_prof_prep_callback_old = 0;
int     MR_deep_prof_prep_tail_old = 0;
int     MR_deep_prof_prep_tail_new = 0;

int     MR_deep_prof_call_new = 0;
int     MR_deep_prof_call_rec = 0;
int     MR_deep_prof_call_old = 0;
int     MR_deep_prof_call_builtin_new = 0;
int     MR_deep_prof_call_builtin_old = 0;

#endif  // MR_DEEP_PROFILING_STATISTICS

#ifdef MR_DEEP_PROFILING_LOG
FILE    *MR_deep_prof_log_file = NULL;
#endif

void
MR_deep_assert_failed(const MR_CallSiteDynamic *csd, const MR_ProcLayout *pl,
    const MR_ProcStatic *ps, const char *cond,
    const char *filename, int linenumber)
{
    char    bufcsd[64];
    char    bufps[64];

    if (csd != NULL) {
        sprintf(bufcsd, ", csd %p\n", csd);
    } else {
        strcpy(bufcsd, "");
    }

    if (pl != NULL) {
        sprintf(bufps, ", pl %p\n", pl);
    } else {
        strcpy(bufps, "");
    }

    if (ps != NULL) {
        sprintf(bufps, ", ps %p\n", ps);
    } else {
        strcpy(bufps, "");
    }

    MR_fatal_error("Deep profiling assertion failed, %s:%d\n%s%s%s\n",
        filename, linenumber, cond, bufcsd, bufps);
}

void
MR_setup_callback(void *entry)
{
    MR_CallSiteDynList  *csd_list;
    MR_CallSiteDynamic  *csd;

    MR_enter_instrumentation();
    csd_list = *MR_current_callback_site;
    while (csd_list != NULL)
    {
        if (csd_list->MR_csdlist_key == entry) {
            MR_next_call_site_dynamic = csd_list->MR_csdlist_call_site;
#ifdef  MR_DEEP_PROFILING_STATISTICS
            MR_deep_prof_prep_callback_old++;
#endif
            MR_leave_instrumentation();
            return;
        }

        csd_list = csd_list->MR_csdlist_next;
    }

#ifdef  MR_DEEP_PROFILING_STATISTICS
    MR_deep_prof_prep_callback_new++;
#endif

    MR_new_call_site_dynamic(csd);

    csd_list = MR_PROFILING_NEW(MR_CallSiteDynList);
    csd_list->MR_csdlist_key = entry;
    csd_list->MR_csdlist_call_site = csd;
    csd_list->MR_csdlist_next = *MR_current_callback_site;
    *MR_current_callback_site = csd_list;

    MR_next_call_site_dynamic = csd;
    MR_leave_instrumentation();
}

#ifdef MR_DEEP_PROFILING_STATISTICS

int MR_deep_prof_search_len;

void
MR_deep_profile_update_special_history(void)
{
    if (MR_deep_prof_search_len < MR_MAX_CLOSURE_LIST_LENGTH) {
        MR_dictionary_search_lengths[MR_deep_prof_search_len]++;
    }
}

void
MR_deep_profile_update_closure_history()
{
    if (MR_deep_prof_search_len < MR_MAX_CLOSURE_LIST_LENGTH) {
        MR_closure_search_lengths[MR_deep_prof_search_len]++;
    }
}

void
MR_deep_profile_update_method_history()
{
    if (MR_deep_prof_search_len < MR_MAX_CLOSURE_LIST_LENGTH) {
        MR_method_search_lengths[MR_deep_prof_search_len]++;
    }
}

#endif  // MR_DEEP_PROFILING_STATISTICS

////////////////////////////////////////////////////////////////////////////

// Functions for writing out the data at the end of the execution.

static  void    MR_deep_data_output_error(const char *msg, const char *file);
static  void    MR_write_out_profiling_tree_check_unwritten(FILE *check_fp);

static  void    MR_write_out_deep_id_string(FILE *fp);
static  void    MR_write_out_procrep_id_string(FILE *fp);
static  void    MR_write_out_program_name(FILE *fp);
static  void    MR_write_out_deep_flags(FILE *fp, MR_bool compress);

static  void    MR_write_out_call_site_static(FILE *fp,
                    const MR_CallSiteStatic *css);
static  void    MR_write_out_call_site_dynamic(FILE *fp,
                    const MR_CallSiteDynamic *csd);

static  void    MR_write_out_proc_dynamic(FILE *fp, const MR_ProcDynamic *pd);
static  void    MR_write_out_ho_call_site_ptrs(FILE *fp,
                    const MR_ProcDynamic *pd,
                    const MR_CallSiteDynList *dynlist);
static  void    MR_write_out_ho_call_site_nodes(FILE *fp,
                    MR_CallSiteDynList *dynlist);

static  void    MR_unwritten_css_handler(FILE *fp, const void *css);
static  void    MR_unwritten_csd_handler(FILE *fp, const void *csd);
static  void    MR_unwritten_pl_handler(FILE *fp, const void *ps);
static  void    MR_unwritten_pd_handler(FILE *fp, const void *pd);

typedef enum node_kind {
    kind_csd, kind_pd, kind_css, kind_ps
} MR_NodeKind;

// Must correspond to fixed_size_int_bytes in deep_profiler/read_profile.m.
#define MR_FIXED_SIZE_INT_BYTES 8

static  void    MR_write_csd_ptr(FILE *fp, const MR_CallSiteDynamic *csd);
#ifdef MR_DEEP_PROFILING_COVERAGE
static  void    MR_write_out_coverage_points_static(FILE *fp,
                    const MR_ProcStatic *ps);
static  void    MR_write_out_coverage_points_dynamic(FILE *fp,
                    const MR_ProcDynamic *pd);
#endif
static  void    MR_write_ptr(FILE *fp, MR_NodeKind kind, int node_id);
static  void    MR_write_kind(FILE *fp, MR_CallSiteKind kind);
static  void    MR_write_byte(FILE *fp, const char byte);
static  void    MR_write_num(FILE *fp, unsigned long num);
static  void    MR_write_fixed_size_int(FILE *fp, MR_uint_least64_t num);
static  void    MR_write_string(FILE *fp, const char *ptr);

////////////////////////////////////////////////////////////////////////////

// We need some hash tables, so here are the structures for handling them....

typedef struct MR_Profiling_Hash_Node_Struct {
    const void                              *item;
    int                                     id;
    MR_bool                                 written;
    struct MR_Profiling_Hash_Node_Struct    *next;
} MR_ProfilingHashNode;

typedef struct {
    int                     last_id;
    int                     length;
    MR_ProfilingHashNode    **nodes;
} MR_ProfilingHashTable;

static  MR_ProfilingHashTable   *MR_create_hash_table(int size);

static  MR_bool                 MR_insert_proc_layout(
                                    const MR_ProcLayout *pl, int *id,
                                    MR_bool *already_written,
                                    MR_bool init_written);
static  MR_bool                 MR_insert_proc_dynamic(
                                    const MR_ProcDynamic *pd, int *id,
                                    MR_bool *already_written,
                                    MR_bool init_written);
static  MR_bool                 MR_insert_call_site_static(
                                    const MR_CallSiteStatic *css, int *id,
                                    MR_bool *already_written,
                                    MR_bool init_written);
static  MR_bool                 MR_insert_call_site_dynamic(
                                    const MR_CallSiteDynamic *csd, int *id,
                                    MR_bool *already_written,
                                    MR_bool init_written);

static  void                    MR_flag_written_proc_layout(
                                    const MR_ProcLayout *pl);
static  void                    MR_flag_written_proc_dynamic(
                                    const MR_ProcDynamic *pd);
static  void                    MR_flag_written_call_site_static(
                                    const MR_CallSiteStatic *css);
static  void                    MR_flag_written_call_site_dynamic(
                                    const MR_CallSiteDynamic *csd);

static  MR_ProfilingHashTable   *MR_call_site_dynamic_table;
static  MR_ProfilingHashTable   *MR_call_site_static_table;
static  MR_ProfilingHashTable   *MR_proc_dynamic_table;
static  MR_ProfilingHashTable   *MR_proc_layout_table;

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

// A convenient prime for the size of the node hash tables.
// The compiler contains nearly 10,000 preds, so a width of 10007
// (requiring about 40K of storage - not onerous compared to the
// size of the tree) will yield chain lengths of about 1 for the
// MR_needed_proc_statics table. For the MR_seen_nodes table, which
// stores all the MR_ProcDynamic nodes that have been seen, the average
// chain length will be longer - a typical run of the compiler can have
// as many as 50,000 nodes, so we don't want the table any narrower than this.

static  const int   MR_hash_table_size = 10007;

#ifdef  MR_DEEP_PROFILING_DEBUG
static  FILE        *debug_fp;
#endif

#define MR_MDPROF_DATA_FILENAME     "Deep.data"
#define MR_MDPROF_PROCREP_FILENAME  "Deep.procrep"

void
MR_write_out_profiling_tree(void)
{
    int                     root_pd_id;
    FILE                    *deep_fp;
    FILE                    *procrep_fp;
    FILE                    *check_fp;
    int                     ticks_per_sec;
    unsigned                num_call_seqs;
    int64_t                 table_sizes_offset;
    char                    errbuf[MR_STRERROR_BUF_SIZE];

#ifdef MR_DEEP_PROFILING_STATISTICS
    int                     i;
#endif

    deep_fp = fopen(MR_MDPROF_DATA_FILENAME, "wb+");
    if (deep_fp == NULL) {
        MR_fatal_error("cannot open `%s' for writing: %s",
            MR_MDPROF_DATA_FILENAME,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    procrep_fp = fopen(MR_MDPROF_PROCREP_FILENAME, "wb+");
    if (procrep_fp == NULL) {
        MR_fatal_error("cannot open `%s' for writing: %s",
            MR_MDPROF_PROCREP_FILENAME,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

#ifdef  MR_DEEP_PROFILING_DEBUG
    if (MR_deep_prof_debug_file_flag) {
        debug_fp = fopen("Deep.debug", "w");
        if (debug_fp == NULL) {
            debug_fp = stderr;
        }
    }
#endif

    MR_write_out_deep_id_string(deep_fp);
    MR_write_out_program_name(deep_fp);

    MR_write_out_deep_flags(deep_fp, MR_FALSE);

    // We overwrite these zeros after seeking back to table_sizes_offset.
    table_sizes_offset = MR_ftell(deep_fp);
    if (table_sizes_offset == -1) {
        MR_deep_data_output_error("ftell failed for ",
            MR_MDPROF_DATA_FILENAME);
    }
    MR_write_fixed_size_int(deep_fp, 0);
    MR_write_fixed_size_int(deep_fp, 0);
    MR_write_fixed_size_int(deep_fp, 0);
    MR_write_fixed_size_int(deep_fp, 0);

    MR_write_out_procrep_id_string(procrep_fp);

#ifdef  MR_CLOCK_TICKS_PER_SECOND
    ticks_per_sec = MR_CLOCK_TICKS_PER_SECOND;
#else
    ticks_per_sec = 0;
#endif
#ifdef  MR_DEEP_PROFILING_CALL_SEQ
    num_call_seqs = MR_deep_prof_cur_call_seq;
#else
    num_call_seqs = 0;
#endif

    MR_write_num(deep_fp, ticks_per_sec);
    MR_write_num(deep_fp, MR_quanta_inside_deep_profiling_code);
    MR_write_num(deep_fp, MR_quanta_outside_deep_profiling_code);
    MR_write_num(deep_fp, num_call_seqs);

    MR_call_site_dynamic_table = MR_create_hash_table(MR_hash_table_size);
    MR_call_site_static_table  = MR_create_hash_table(MR_hash_table_size);
    MR_proc_dynamic_table = MR_create_hash_table(MR_hash_table_size);
    MR_proc_layout_table  = MR_create_hash_table(MR_hash_table_size);

    if (MR_insert_proc_dynamic(&MR_main_parent_proc_dynamic, &root_pd_id,
        NULL, MR_FALSE))
    {
        MR_fatal_error("MR_write_out_profiling_tree: root seen before");
    }

#ifdef MR_DEEP_PROFILING_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "root = %p, %d\n",
            &MR_main_parent_proc_dynamic, root_pd_id);
    }
#endif

    MR_write_ptr(deep_fp, kind_pd, root_pd_id);

    MR_write_out_proc_dynamic(deep_fp, &MR_main_parent_proc_dynamic);

    MR_write_out_user_proc_static(deep_fp, NULL, &MR_main_parent_proc_layout);
    MR_deep_assert(NULL, NULL, NULL,
        MR_address_of_write_out_proc_statics != NULL);
    (*MR_address_of_write_out_proc_statics)(deep_fp, procrep_fp);

    if (MR_fseek(deep_fp, table_sizes_offset, SEEK_SET) != 0) {
        MR_deep_data_output_error("cannot seek to header of",
            MR_MDPROF_DATA_FILENAME);
    }

    MR_write_fixed_size_int(deep_fp, MR_call_site_dynamic_table->last_id);
    MR_write_fixed_size_int(deep_fp, MR_call_site_static_table->last_id);
    MR_write_fixed_size_int(deep_fp, MR_proc_dynamic_table->last_id);
    MR_write_fixed_size_int(deep_fp, MR_proc_layout_table->last_id);

    if (fclose(deep_fp) != 0) {
        MR_deep_data_output_error("cannot close", MR_MDPROF_DATA_FILENAME);
    }

    putc(MR_no_more_modules, procrep_fp);
    if (fclose(procrep_fp) != 0) {
        MR_deep_data_output_error("cannot close",
            MR_MDPROF_PROCREP_FILENAME);
    }

#ifdef MR_DEEP_PROFILING_STATISTICS
    if (! MR_print_deep_profiling_statistics) {
        return;
    }

    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_normal_new:",
        MR_deep_prof_prep_normal_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_normal_old:",
        MR_deep_prof_prep_normal_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_normal all:",
        MR_deep_prof_prep_normal_new +
        MR_deep_prof_prep_normal_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_special_new:",
        MR_deep_prof_prep_special_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_special_old:",
        MR_deep_prof_prep_special_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_special all:",
        MR_deep_prof_prep_special_new +
        MR_deep_prof_prep_special_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_ho_new:",
        MR_deep_prof_prep_ho_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_ho_old:",
        MR_deep_prof_prep_ho_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_ho all:",
        MR_deep_prof_prep_ho_new +
        MR_deep_prof_prep_ho_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_method_new:",
        MR_deep_prof_prep_method_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_method_old:",
        MR_deep_prof_prep_method_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_method all:",
        MR_deep_prof_prep_method_new +
        MR_deep_prof_prep_method_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_callback_new:",
        MR_deep_prof_prep_callback_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_callback_old:",
        MR_deep_prof_prep_callback_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_callback all:",
        MR_deep_prof_prep_callback_new +
        MR_deep_prof_prep_callback_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_tail_new:",
        MR_deep_prof_prep_tail_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_tail_old:",
        MR_deep_prof_prep_tail_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_prep_tail all:",
        MR_deep_prof_prep_tail_new +
        MR_deep_prof_prep_tail_old);

    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call_new:",
        MR_deep_prof_call_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call_rec:",
        MR_deep_prof_call_rec);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call_old:",
        MR_deep_prof_call_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call all:",
        MR_deep_prof_call_new +
        MR_deep_prof_call_rec +
        MR_deep_prof_call_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call_builtin_new:",
        MR_deep_prof_call_builtin_new);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call_builtin_old:",
        MR_deep_prof_call_builtin_old);
    fprintf(stderr, "%-40s %10d\n",
        "MR_deep_prof_call_builtin all:",
        MR_deep_prof_call_builtin_new +
        MR_deep_prof_call_builtin_old);

    fprintf(stderr, "%-40s %10d\n",
        "call prepare:",
        MR_deep_prof_prep_normal_new +
        MR_deep_prof_prep_normal_old +
        MR_deep_prof_prep_special_new +
        MR_deep_prof_prep_special_old +
        MR_deep_prof_prep_ho_new +
        MR_deep_prof_prep_ho_old +
        MR_deep_prof_prep_method_new +
        MR_deep_prof_prep_method_old +
        MR_deep_prof_prep_callback_new +
        MR_deep_prof_prep_callback_old +
        MR_deep_prof_prep_tail_old +
        MR_deep_prof_prep_tail_new);
    fprintf(stderr, "%-40s %10d\n",
        "call arrival:",
        MR_deep_prof_prep_tail_old +
        MR_deep_prof_prep_tail_new +
        MR_deep_prof_call_new +
        MR_deep_prof_call_rec +
        MR_deep_prof_call_old +
        MR_deep_prof_call_builtin_new +
        MR_deep_prof_call_builtin_old);

    fprintf(stderr, "\ntotal size of profiling tree: %10d bytes\n",
        MR_deep_num_csd_nodes * sizeof(MR_CallSiteDynamic) +
        MR_deep_num_pd_nodes * sizeof(MR_ProcDynamic) +
        MR_deep_num_pd_array_slots * sizeof(MR_CallSiteDynamic *) +
        MR_deep_num_dynlist_nodes * sizeof(MR_CallSiteDynList));
    fprintf(stderr, "%10d CSD nodes at %4d bytes per node:   %10d bytes\n",
        MR_deep_num_csd_nodes,
        sizeof(MR_CallSiteDynamic),
        MR_deep_num_csd_nodes * sizeof(MR_CallSiteDynamic));
    fprintf(stderr, "%10d PD nodes at %4d bytes per node:    %10d bytes\n",
        MR_deep_num_pd_nodes,
        sizeof(MR_ProcDynamic),
        MR_deep_num_pd_nodes * sizeof(MR_ProcDynamic));
    fprintf(stderr, "%10d array slots at %4d bytes per node: %10d bytes\n",
        MR_deep_num_pd_array_slots,
        sizeof(MR_CallSiteDynamic *),
        MR_deep_num_pd_array_slots * sizeof(MR_CallSiteDynamic *));
    fprintf(stderr, "%10d list nodes at %4d bytes per node:  %10d bytes\n",
        MR_deep_num_dynlist_nodes,
        sizeof(MR_CallSiteDynList),
        MR_deep_num_dynlist_nodes * sizeof(MR_CallSiteDynList));

    fprintf(stderr, "\nTypeInfo search length histogram:\n");
    for (i = 0; i < MR_MAX_CLOSURE_LIST_LENGTH; i++) {
        if (MR_dictionary_search_lengths[i] > 0) {
            fprintf(stderr, "\t%3d: %12d\n", i,
                MR_dictionary_search_lengths[i]);
        }
    }

    fprintf(stderr, "\nClosure search length histogram:\n");
    for (i = 0; i < MR_MAX_CLOSURE_LIST_LENGTH; i++) {
        if (MR_closure_search_lengths[i] > 0) {
            fprintf(stderr, "\t%3d: %12d\n", i, MR_closure_search_lengths[i]);
        }
    }

    fprintf(stderr, "\nMethod search length histogram:\n");
    for (i = 0; i < MR_MAX_CLOSURE_LIST_LENGTH; i++) {
        if (MR_method_search_lengths[i] > 0) {
            fprintf(stderr, "\t%3d: %12d\n", i, MR_method_search_lengths[i]);
        }
    }
#endif // MR_DEEP_PROFILING_STATISTICS

#ifdef MR_DEEP_PROFILING_DEBUG
    check_fp = debug_fp;
#else
    check_fp = NULL;
#endif

    MR_write_out_profiling_tree_check_unwritten(check_fp);
}

static void
MR_deep_data_output_error(const char *op, const char *filename)
{
    char    errbuf[MR_STRERROR_BUF_SIZE];

    MR_warning("%s %s: %s", op, filename,
        MR_strerror(errno, errbuf, sizeof(errbuf)));

    // An incomplete profiling data file is useless. Removing it prevents
    // misunderstandings about that, and may also cure a disk-full condition,
    // if the close failure was caused by that.

    if (remove(MR_MDPROF_DATA_FILENAME) != 0) {
        MR_warning("cannot remove %s: %s",
            MR_MDPROF_DATA_FILENAME,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    if (remove(MR_MDPROF_PROCREP_FILENAME) != 0) {
        MR_warning("cannot remove %s: %s",
            MR_MDPROF_PROCREP_FILENAME,
            MR_strerror(errno, errbuf, sizeof(errbuf)));
    }

    exit(1);
}

static void
MR_write_out_deep_id_string(FILE *fp)
{
    // Must be the same as deep_id_string in deep_profiler/read_profile.m
    const char  *id_string = "Mercury deep profiler data version 8\n";

    fputs(id_string, fp);
}

static void
MR_write_out_program_name(FILE *fp)
{
    MR_write_string(fp, MR_progname);
}

// Flags in the deep profiler data file's header. Any bit without a meaning
// here must be set to zero as it it may be used in the future. The next line
// marks 16 bit boundaries in the 64 bit flags value:
//
//       48  32  16   0

#define MR_DEEP_FLAG_WORDSIZE_MASK \
    (0x00000000000000FF)
#define MR_DEEP_FLAG_WORDSIZE_SHIFT \
    (0)
#define MR_DEEP_FLAG_CANONICAL_MASK \
    (0x0000000000000300)
#define MR_DEEP_FLAG_CANONICAL_SHIFT \
    (8)
#define MR_DEEP_FLAG_COMPRESSION_MASK \
    (0x0000000000000C00)
#define MR_DEEP_FLAG_COMPRESSION_SHIFT \
    (10)
// This flag is two bits wide as it has three valid values.
#define MR_DEEP_FLAG_COVERAGE_DATA_TYPE_MASK \
    (0x0000000000003000)
#define MR_DEEP_FLAG_COVERAGE_DATA_TYPE_SHIFT \
    (12)

#if !defined(MR_DEEP_PROFILING_COVERAGE)
    #define MR_DEEP_FLAG_COVERAGE_DATA_TYPE_VALUE 0
#elif defined(MR_DEEP_PROFILING_COVERAGE_STATIC)
    #define MR_DEEP_FLAG_COVERAGE_DATA_TYPE_VALUE 1
#elif defined(MR_DEEP_PROFILING_COVERAGE_DYNAMIC)
    #define MR_DEEP_FLAG_COVERAGE_DATA_TYPE_VALUE 2
#endif

static void
MR_write_out_deep_flags(FILE *fp, MR_bool compress)
{
    MR_uint_least64_t       flags = 0;

    flags |= MR_DEEP_FLAG_WORDSIZE_MASK &
        (sizeof(MR_Word) << MR_DEEP_FLAG_WORDSIZE_SHIFT);

    flags |= MR_DEEP_FLAG_CANONICAL_MASK &
        (1 << MR_DEEP_FLAG_CANONICAL_SHIFT);

    // ignore compress for now

    flags |= MR_DEEP_FLAG_COVERAGE_DATA_TYPE_MASK &
        (MR_DEEP_FLAG_COVERAGE_DATA_TYPE_VALUE <<
            MR_DEEP_FLAG_COVERAGE_DATA_TYPE_SHIFT);

    MR_write_fixed_size_int(fp, flags);
}

static void
MR_write_out_procrep_id_string(FILE *fp)
{
    // Must be the same as procrep_id_string (or new_procrep_id_string) in
    // mdbcomp/program_representation.m.

    const char  *id_string = "Mercury deep profiler procrep version 6\n";

    fputs(id_string, fp);
}

void
MR_write_out_user_proc_static(FILE *deep_fp, FILE *procrep_fp,
    const MR_ProcLayoutUser *proc_layout)
{
    MR_write_out_proc_static(deep_fp, procrep_fp,
        (const MR_ProcLayout *) proc_layout);
}

void
MR_write_out_uci_proc_static(FILE *deep_fp, FILE *procrep_fp,
    const MR_ProcLayoutUCI *proc_layout)
{
    MR_write_out_proc_static(deep_fp, procrep_fp,
        (const MR_ProcLayout *) proc_layout);
}

#ifdef  MR_DEEP_PROFILING_LOG
MR_bool MR_deep_prof_doing_logging = MR_FALSE;

void
MR_deep_log_proc_statics(FILE *fp)
{
    MR_deep_prof_doing_logging = MR_TRUE;
    (*MR_address_of_write_out_proc_statics)(fp);
    MR_deep_prof_doing_logging = MR_FALSE;
}
#endif  // MR_DEEP_PROFILING_LOG

void
MR_write_out_module_proc_reps_start(FILE *procrep_fp,
    const MR_ModuleLayout *module_layout)
{
    const MR_uint_least8_t  *oisu_bytecode;
    const MR_uint_least8_t  *type_bytecode;
    int                     size;
    int                     bytenum;

    putc(MR_next_module, procrep_fp);
    MR_write_string(procrep_fp, module_layout->MR_ml_name);

    MR_write_num(procrep_fp, module_layout->MR_ml_string_table_size);
    size = module_layout->MR_ml_string_table_size;
    for (bytenum = 0; bytenum < size; bytenum++) {
        putc(module_layout->MR_ml_string_table[bytenum], procrep_fp);
    }

    MR_write_num(procrep_fp, module_layout->MR_ml_num_oisu_types);
    oisu_bytecode = module_layout->MR_ml_oisu_bytes;
    if (module_layout->MR_ml_num_oisu_types == 0) {
        if (oisu_bytecode != NULL) {
            MR_fatal_error("num_oisu_types == 0 but bytecode != NULL");
        }
    } else {
        if (oisu_bytecode == NULL) {
            MR_fatal_error("num_oisu_types != 0 but bytecode == NULL");
        }

        size = (oisu_bytecode[0] << 24) + (oisu_bytecode[1] << 16) +
            (oisu_bytecode[2] << 8) + oisu_bytecode[3];
        for (bytenum = 0; bytenum < size; bytenum++) {
            putc(oisu_bytecode[bytenum], procrep_fp);
        }
    }

    MR_write_num(procrep_fp, module_layout->MR_ml_num_table_types);
    type_bytecode = module_layout->MR_ml_type_table_bytes;
    if (module_layout->MR_ml_num_table_types == 0) {
        if (type_bytecode != NULL) {
            MR_fatal_error("num_types == 0 but bytecode != NULL");
        }
    } else {
        if (type_bytecode == NULL) {
            MR_fatal_error("num_types != 0 but bytecode == NULL");
        }

        size = (type_bytecode[0] << 24) + (type_bytecode[1] << 16) +
            (type_bytecode[2] << 8) + type_bytecode[3];
        for (bytenum = 0; bytenum < size; bytenum++) {
            putc(type_bytecode[bytenum], procrep_fp);
        }
    }
}

void
MR_write_out_module_proc_reps_end(FILE *procrep_fp)
{
    putc(MR_no_more_procs, procrep_fp);
}

void
MR_write_out_proc_static(FILE *deep_fp, FILE *procrep_fp,
    const MR_ProcLayout *proc_layout)
{
    const MR_ProcStatic *ps;
    const MR_ProcId     *procid;
    int                 ps_id;
    int                 css_id;
    MR_bool             already_written;
    int                 i;

    if (proc_layout == NULL) {
        MR_fatal_error("MR_write_out_proc_static: null proc_layout");
    }

    if (! MR_PROC_LAYOUT_HAS_PROC_ID(proc_layout)) {
        MR_fatal_error("MR_write_out_proc_static: no proc_id\n");
    }

    ps = proc_layout->MR_sle_proc_static;

#ifdef  MR_DEEP_PROFILING_LOG
    if (MR_deep_prof_doing_logging) {
        procid = &proc_layout->MR_sle_proc_id;
        if (MR_PROC_ID_IS_UCI(*procid)) {
            fprintf(deep_fp,
                "proc_static_uci(%ld,\"%s\",\"%s\",\"%s\",\"%s\",%d,%d,[",
                (long) proc_layout->MR_sle_proc_static,
                procid->MR_proc_uci.MR_uci_type_name,
                procid->MR_proc_uci.MR_uci_type_module,
                procid->MR_proc_uci.MR_uci_def_module,
                procid->MR_proc_uci.MR_uci_pred_name,
                procid->MR_proc_uci.MR_uci_type_arity,
                procid->MR_proc_uci.MR_uci_mode);
        } else {
            fprintf(deep_fp,
                "proc_static_user(%ld,%s,\"%s\",\"%s\",\"%s\",%d,%d,[",
                (long) proc_layout->MR_sle_proc_static,
                procid->MR_proc_user.MR_user_pred_or_func == MR_PREDICATE ?
                    "p" : "f",
                procid->MR_proc_user.MR_user_decl_module,
                procid->MR_proc_user.MR_user_def_module,
                procid->MR_proc_user.MR_user_name,
                procid->MR_proc_user.MR_user_pred_form_arity,
                procid->MR_proc_user.MR_user_mode);
        }

        for (i = 0; i < ps->MR_ps_num_call_sites; i++) {
            if (i == 0) {
                fputs("\n\t", deep_fp);
            } else {
                fputs(",\n\t", deep_fp);
            }

            switch (ps->MR_ps_call_sites[i].MR_css_kind) {
                case MR_normal_call:
                    fprintf(deep_fp, "css_normal(%ld, %ld)",
                        (long) &ps->MR_ps_call_sites[i],
                        (long) &ps->MR_ps_call_sites[i].
                            MR_css_callee_ptr_if_known->MR_sle_proc_static);
                    break;

                case MR_special_call:
                    fprintf(deep_fp, "css_special(%ld)",
                        (long) &ps->MR_ps_call_sites[i]);
                    break;

                case MR_higher_order_call:
                    fprintf(deep_fp, "css_higher_order(%ld)",
                        (long) &ps->MR_ps_call_sites[i]);
                    break;

                case MR_method_call:
                    fprintf(deep_fp, "css_method(%ld)",
                        (long) &ps->MR_ps_call_sites[i]);
                    break;

                case MR_callback:
                    fprintf(deep_fp, "css_callback(%ld)",
                        (long) &ps->MR_ps_call_sites[i]);
                    break;

                default:
                    fprintf(deep_fp, "css_unknown(%ld)",
                        (long) &ps->MR_ps_call_sites[i]);
                    break;
            }
        }

        fprintf(deep_fp, "]).\n");
        return;
    }
#endif

    if (ps == NULL) {
        procid = &proc_layout->MR_sle_proc_id;
        if (MR_PROC_ID_IS_UCI(*procid)) {
            fprintf(stderr, "uci %s/%s/%s/%s/%d/%d\n",
                procid->MR_proc_uci.MR_uci_type_name,
                procid->MR_proc_uci.MR_uci_type_module,
                procid->MR_proc_uci.MR_uci_def_module,
                procid->MR_proc_uci.MR_uci_pred_name,
                procid->MR_proc_uci.MR_uci_type_arity,
                procid->MR_proc_uci.MR_uci_mode);
        } else {
            fprintf(stderr, "user %d/%s/%s/%s/%d/%d\n",
                procid->MR_proc_user.MR_user_pred_or_func,
                procid->MR_proc_user.MR_user_decl_module,
                procid->MR_proc_user.MR_user_def_module,
                procid->MR_proc_user.MR_user_name,
                procid->MR_proc_user.MR_user_pred_form_arity,
                procid->MR_proc_user.MR_user_mode);
        }

        MR_fatal_error("MR_write_out_proc_static: null ps");
    }

    (void) MR_insert_proc_layout(proc_layout, &ps_id, &already_written,
         MR_TRUE);

#ifdef MR_DEEP_PROFILING_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "proc_static %p/%p/%d\n", proc_layout, ps, ps_id);
        fprintf(debug_fp, "  filename \"%s\", linenumber %d, "
            "interface %d, %d call sites\n",
        ps->MR_ps_file_name, ps->MR_ps_line_number,
        ps->MR_ps_is_in_interface, ps->MR_ps_num_call_sites);
    }
#endif

    if (already_written) {
        MR_fatal_error("MR_write_out_proc_static: seen ps");
    }

    MR_flag_written_proc_layout(proc_layout);

    MR_write_byte(deep_fp, MR_deep_item_proc_static);
    MR_write_ptr(deep_fp, kind_ps, ps_id);

    procid = &proc_layout->MR_sle_proc_id;
    MR_write_out_str_proc_label(deep_fp, procid);

    MR_write_string(deep_fp, ps->MR_ps_file_name);
    MR_write_num(deep_fp, ps->MR_ps_line_number);
    MR_write_byte(deep_fp, ps->MR_ps_is_in_interface);
    MR_write_num(deep_fp, ps->MR_ps_num_call_sites);

    // Write out pointers to Call Site Statics. These are read in with the
    // proc static.

    for (i = 0; i < ps->MR_ps_num_call_sites; i++) {
        (void) MR_insert_call_site_static(&ps->MR_ps_call_sites[i], &css_id,
            NULL, MR_FALSE);

#ifdef MR_DEEP_PROFILING_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp,
                "call site id %d in proc_static %p/%p/%d -> %d\n",
                i, proc_layout, ps, ps_id, css_id);
        }
#endif

        MR_write_ptr(deep_fp, kind_css, css_id);
    }

    // Write out coverage points. This is read in as part of the proc static.

#ifdef MR_DEEP_PROFILING_COVERAGE
    MR_write_out_coverage_points_static(deep_fp, ps);
#endif

    // Write out the actual call site statics,  These are read in after the
    // proc static, not as part of it.

    for (i = 0; i < ps->MR_ps_num_call_sites; i++) {
#ifdef MR_DEEP_PROFILING_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "in proc_static %p/%p/%d, call site %d\n",
                proc_layout, ps, ps_id, i);
        }
#endif

        MR_write_out_call_site_static(deep_fp, &ps->MR_ps_call_sites[i]);
    }

    const MR_uint_least8_t  *bytecode;

    // Some predicates in the Mercury standard library, such as
    // exception.builtin_catch, have Mercury declarations but no Mercury
    // implementation, even as foreign_proc code. We do still generate
    // proc_static structures for them, since we *want* the hand-written
    // C code to be able to collect deep profiling data (in this case,
    // to count the number of executions of the EXCP port). This means that
    // (a) they will have proc_layout structures, and (b) the bytecode
    // pointer field in these structures will be NULL.
    //
    // We handle such procedures by simply not including them in the
    // module representation. This is fine, as long as any code that reads
    // and processes the program representation is aware that the bodies
    // of procedures defined outside Mercury may be missing.

    bytecode = proc_layout->MR_sle_body_bytes;
    if (bytecode != NULL) {
        int size;
        int bytenum;

        putc(MR_next_proc, procrep_fp);
        MR_write_out_str_proc_label(procrep_fp, procid);

        size = (bytecode[0] << 24) + (bytecode[1] << 16) +
            (bytecode[2] << 8) + bytecode[3];
        for (bytenum = 0; bytenum < size; bytenum++) {
            putc(bytecode[bytenum], procrep_fp);
        }
    }
}

void
MR_write_out_str_proc_label(FILE *deep_fp, const MR_ProcId *procid)
{
    if (MR_PROC_ID_IS_UCI(*procid)) {
#ifdef MR_DEEP_PROFILING_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "  uci %s/%s/%s/%s/%d/%d\n",
                procid->MR_proc_uci.MR_uci_type_name,
                procid->MR_proc_uci.MR_uci_type_module,
                procid->MR_proc_uci.MR_uci_def_module,
                procid->MR_proc_uci.MR_uci_pred_name,
                procid->MR_proc_uci.MR_uci_type_arity,
                procid->MR_proc_uci.MR_uci_mode);
        }
#endif

        MR_write_byte(deep_fp, MR_proclabel_special);
        MR_write_string(deep_fp, procid->MR_proc_uci.MR_uci_type_name);
        MR_write_string(deep_fp, procid->MR_proc_uci.MR_uci_type_module);
        MR_write_string(deep_fp, procid->MR_proc_uci.MR_uci_def_module);
        MR_write_string(deep_fp, procid->MR_proc_uci.MR_uci_pred_name);
        MR_write_num(deep_fp, procid->MR_proc_uci.MR_uci_type_arity);
        MR_write_num(deep_fp, procid->MR_proc_uci.MR_uci_mode);
    } else {
#ifdef MR_DEEP_PROFILING_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "  user %d/%s/%s/%s/%d/%d\n",
                procid->MR_proc_user.MR_user_pred_or_func,
                procid->MR_proc_user.MR_user_decl_module,
                procid->MR_proc_user.MR_user_def_module,
                procid->MR_proc_user.MR_user_name,
                procid->MR_proc_user.MR_user_pred_form_arity,
                procid->MR_proc_user.MR_user_mode);
        }
#endif

        if (procid->MR_proc_user.MR_user_pred_or_func == MR_PREDICATE) {
            MR_write_byte(deep_fp, MR_proclabel_user_predicate);
        } else {
            MR_write_byte(deep_fp, MR_proclabel_user_function);
        }

        MR_write_string(deep_fp, procid->MR_proc_user.MR_user_decl_module);
        MR_write_string(deep_fp, procid->MR_proc_user.MR_user_def_module);
        MR_write_string(deep_fp, procid->MR_proc_user.MR_user_name);
        MR_write_num(deep_fp, procid->MR_proc_user.MR_user_pred_form_arity);
        MR_write_num(deep_fp, procid->MR_proc_user.MR_user_mode);
    }
}

static void
MR_write_out_call_site_static(FILE *fp, const MR_CallSiteStatic *css)
{
    int css_id;
    int ps_id;
    MR_bool already_written;

    if (css == NULL) {
        MR_fatal_error("MR_write_out_call_site_static: null css");
    }

    (void) MR_insert_call_site_static(css, &css_id, &already_written, MR_TRUE);

    if (already_written) {
        MR_fatal_error("MR_write_out_call_site_static: seen css");
        fflush(fp);
    }

    MR_flag_written_call_site_static(css);

#ifdef MR_DEEP_PROFILING_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "call_site_static %p/%d\n", css, css_id);
        fprintf(debug_fp,
            "  filename \"%s\", linenum %d, goal path %s, kind %d\n",
        css->MR_css_file_name, css->MR_css_line_number,
        css->MR_css_goal_path, css->MR_css_kind);
    }
#endif

    MR_write_byte(fp, MR_deep_item_call_site_static);
    MR_write_ptr(fp, kind_css, css_id);
    MR_write_kind(fp, css->MR_css_kind);
    if (css->MR_css_kind == MR_callsite_normal_call) {
        (void) MR_insert_proc_layout(css->MR_css_callee_ptr_if_known, &ps_id,
            NULL, MR_FALSE);
#ifdef MR_DEEP_PROFILING_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "  callee %p/%d\n",
                css->MR_css_callee_ptr_if_known, ps_id);
        }
#endif
        MR_write_num(fp, ps_id);
        if (css->MR_css_type_subst_if_known != NULL) {
            MR_write_string(fp, css->MR_css_type_subst_if_known);
        } else {
            MR_write_string(fp, "");
        }
    }
    // XXX MR_css_file_name
    MR_write_num(fp, css->MR_css_line_number);
    MR_write_string(fp, css->MR_css_goal_path);
}

static void
MR_write_out_call_site_dynamic(FILE *fp, const MR_CallSiteDynamic *csd)
{
    int bitmask = 0;
    int csd_id;
    int pd_id;

    if (csd == NULL) {
        return;
    }

#ifdef MR_DEEP_PROFILING_STATISTICS
    MR_deep_num_csd_nodes++;
#endif

    MR_deep_assert(csd, NULL, NULL, csd->MR_csd_callee_ptr != NULL);

#ifdef MR_DEEP_PROFILING_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "call_site_dynamic %p: callee proc_dynamic %p\n",
            csd, csd->MR_csd_callee_ptr);
    }
#endif

    MR_write_byte(fp, MR_deep_item_call_site_dynamic);
    if (! MR_insert_call_site_dynamic(csd, &csd_id, NULL, MR_FALSE)) {
        MR_fatal_error("MR_write_out_call_site_dynamic: insert succeeded");
    }

    MR_flag_written_call_site_dynamic(csd);

    MR_write_ptr(fp, kind_csd, csd_id);
    if (csd->MR_csd_callee_ptr == NULL) {
        pd_id = 0;
    } else {
        (void) MR_insert_proc_dynamic(csd->MR_csd_callee_ptr, &pd_id, NULL,
            MR_FALSE);
    }

    MR_write_ptr(fp, kind_pd, pd_id);

    // The masks here must exactly correspond with the masks in
    // predicate read_profile in deep_profiler/read_profile.m.

#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    if (csd->MR_csd_own.MR_own_calls != 0) {
        bitmask |= 0x0001;
    }
  #endif
    if (csd->MR_csd_own.MR_own_exits != 0) {
        bitmask |= 0x0002;
    }
    if (csd->MR_csd_own.MR_own_fails != 0) {
        bitmask |= 0x0004;
    }
    if (csd->MR_csd_own.MR_own_redos != 0) {
        bitmask |= 0x0040;
    }
    if (csd->MR_csd_own.MR_own_excps != 0) {
        bitmask |= 0x0080;
    }
#endif
#ifdef MR_DEEP_PROFILING_TIMING
    if (csd->MR_csd_own.MR_own_quanta != 0) {
        bitmask |= 0x0100;
    }
#endif
#ifdef MR_DEEP_PROFILING_CALL_SEQ
    if (csd->MR_csd_own.MR_own_call_seqs != 0) {
        bitmask |= 0x0008;
    }
#endif
#ifdef MR_DEEP_PROFILING_MEMORY
    if (csd->MR_csd_own.MR_own_allocs != 0) {
        bitmask |= 0x0010;
    }
    if (csd->MR_csd_own.MR_own_words != 0) {
        bitmask |= 0x0020;
    }
#endif

    MR_write_num(fp, bitmask);

#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    if (csd->MR_csd_own.MR_own_calls != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_calls);
    }
  #endif
    if (csd->MR_csd_own.MR_own_exits != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_exits);
    }
    if (csd->MR_csd_own.MR_own_fails != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_fails);
    }
    if (csd->MR_csd_own.MR_own_redos != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_redos);
    }
    if (csd->MR_csd_own.MR_own_excps != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_excps);
    }
#endif

#ifdef MR_DEEP_PROFILING_TIMING
    if (csd->MR_csd_own.MR_own_quanta != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_quanta);
    }
#endif

#ifdef MR_DEEP_PROFILING_CALL_SEQ
    if (csd->MR_csd_own.MR_own_call_seqs != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_call_seqs);
    }
#endif

#ifdef MR_DEEP_PROFILING_MEMORY
    if (csd->MR_csd_own.MR_own_allocs != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_allocs);
    }
    if (csd->MR_csd_own.MR_own_words != 0) {
        MR_write_num(fp, csd->MR_csd_own.MR_own_words);
    }
#endif

    MR_write_out_proc_dynamic(fp, csd->MR_csd_callee_ptr);
}

static void
MR_write_out_proc_dynamic(FILE *fp, const MR_ProcDynamic *pd)
{
    const MR_ProcStatic     *ps;
    const MR_ProcLayout     *pl;
    int                     pd_id;
    int                     ps_id;
    MR_bool                 already_written;
    int                     i;

    if (pd == NULL) {
        // This shouldn't really happen except that we don't have
        // correct handling of nondet pragma_foreign_code yet.

        return;
    }

    if (! MR_insert_proc_dynamic(pd, &pd_id, &already_written, MR_TRUE)) {
        MR_fatal_error("MR_write_out_proc_dynamic: unseen pd");
    }

    if (already_written) {
        return;
    }

    pl = pd->MR_pd_proc_layout;
    ps = pl->MR_sle_proc_static;

    MR_flag_written_proc_dynamic(pd);
    (void) MR_insert_proc_layout(pl, &ps_id, NULL, MR_FALSE);

#ifdef MR_DEEP_PROFILING_STATISTICS
    MR_deep_num_pd_nodes++;
    MR_deep_num_pd_array_slots += ps->MR_ps_num_call_sites;
#endif

    MR_write_byte(fp, MR_deep_item_proc_dynamic);
    MR_write_ptr(fp, kind_pd, pd_id);
    MR_write_ptr(fp, kind_ps, ps_id);
    MR_write_num(fp, ps->MR_ps_num_call_sites);

#ifdef MR_DEEP_PROFILING_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "proc_dynamic %p/%d, proc_static %p/%p/%d\n",
            pd, pd_id, pd->MR_pd_proc_layout, ps, ps_id);
    }
#endif

#ifdef MR_DEEP_PROFILING_COVERAGE
    MR_write_out_coverage_points_dynamic(fp, pd);
#endif

    for (i = 0; i < ps->MR_ps_num_call_sites; i++) {
        MR_write_kind(fp, ps->MR_ps_call_sites[i].MR_css_kind);
        switch (ps->MR_ps_call_sites[i].MR_css_kind)
        {
            case MR_callsite_normal_call:
#ifdef MR_DEEP_PROFILING_DEBUG
                if (debug_fp != NULL) {
                    fprintf(debug_fp,
                        "  normal call from pd %p to pd %p\n",
                        pd, pd->MR_pd_call_site_ptr_ptrs[i]);
                }
#endif
                MR_write_csd_ptr(fp, pd->MR_pd_call_site_ptr_ptrs[i]);
                break;

            case MR_callsite_special_call:
            case MR_callsite_higher_order_call:
            case MR_callsite_method_call:
            case MR_callsite_callback:
                MR_write_out_ho_call_site_ptrs(fp, pd,
                    (MR_CallSiteDynList *) pd->MR_pd_call_site_ptr_ptrs[i]);
                break;
        }
    }

    for (i = 0; i < ps->MR_ps_num_call_sites; i++) {
        switch (ps->MR_ps_call_sites[i].MR_css_kind)
        {
            case MR_callsite_normal_call:
                MR_write_out_call_site_dynamic(fp,
                    pd->MR_pd_call_site_ptr_ptrs[i]);
                break;

            case MR_callsite_special_call:
            case MR_callsite_higher_order_call:
            case MR_callsite_method_call:
            case MR_callsite_callback:
                MR_write_out_ho_call_site_nodes(fp,
                    (MR_CallSiteDynList *) pd->MR_pd_call_site_ptr_ptrs[i]);
                break;
        }
    }
}

static void
MR_write_out_ho_call_site_ptrs(FILE *fp, const MR_ProcDynamic *pd,
    const MR_CallSiteDynList *dynlist)
{
    while (dynlist != NULL) {
#ifdef MR_DEEP_PROFILING_STATISTICS
        MR_deep_num_dynlist_nodes++;
#endif
#ifdef MR_DEEP_PROFILING_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "  multi call from pd %p to pd %p\n",
                pd, dynlist->MR_csdlist_call_site);
        }
#endif
        MR_write_csd_ptr(fp, dynlist->MR_csdlist_call_site);
        dynlist = dynlist->MR_csdlist_next;
    }
    MR_write_byte(fp, MR_deep_item_end);
}

static void
MR_write_out_ho_call_site_nodes(FILE *fp, MR_CallSiteDynList *dynlist)
{
    while (dynlist != NULL) {
        MR_write_out_call_site_dynamic(fp, dynlist->MR_csdlist_call_site);
        dynlist = dynlist->MR_csdlist_next;
    }
}

static void
MR_write_csd_ptr(FILE *fp, const MR_CallSiteDynamic *csd)
{
    int csd_id;

    if (csd == NULL) {
        csd_id = 0;
    } else {
        (void) MR_insert_call_site_dynamic(csd, &csd_id, NULL, MR_FALSE);
    }

    MR_write_ptr(fp, kind_csd, csd_id);
}

#ifdef MR_DEEP_PROFILING_COVERAGE
static void
MR_write_out_coverage_points_static(FILE *fp, const MR_ProcStatic *ps)
{
    const MR_CoveragePointStatic *cps_static;
#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC
    const MR_Unsigned *cps;
#endif
    unsigned int i;

    cps_static = ps->MR_ps_coverage_points_static;
#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC
    cps = ps->MR_ps_coverage_points;
#endif

    MR_write_num(fp, ps->MR_ps_num_coverage_points);
    for (i = 0; i < ps->MR_ps_num_coverage_points; i++) {

#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "coverage point: %s,%d",
                cps_static[i].MR_cp_goal_path, cps_static[i].MR_cp_type);
#ifdef  MR_DEEP_PROFILING_COVERAGE_STATIC
            fprintf(debug_fp, ": %d\n", cps[i]);
#else
            fprintf(debug_fp, "\n");
#endif
        }
#endif

        MR_write_string(fp, cps_static[i].MR_cp_goal_path);
        MR_write_num(fp, cps_static[i].MR_cp_type);
#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC
        MR_write_num(fp, cps[i]);
#endif
    }
}

static void
MR_write_out_coverage_points_dynamic(FILE *fp, const MR_ProcDynamic *pd)
{
#ifdef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
    const MR_Unsigned *cps;
    unsigned int i;
    unsigned int ncps;

    cps = pd->MR_pd_coverage_points;
    ncps = pd->MR_pd_proc_layout->MR_sle_proc_static->
        MR_ps_num_coverage_points;

    MR_write_num(fp, ncps);
    for (i = 0; i < ncps; i++) {
#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
        if (debug_fp != NULL) {
            fprintf(debug_fp, "coverage point: %d",
                cps[i]);
        }
#endif
        MR_write_num(fp, cps[i]);
    }
#endif
};
#endif

static void
MR_write_ptr(FILE *fp, MR_NodeKind kind, int node_id)
{
#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "ptr: %d\n", node_id);
    }
#endif

    // MR_write_byte(fp, (int) kind);
    MR_write_num(fp, node_id);
}

static void
MR_write_kind(FILE *fp, MR_CallSiteKind kind)
{

#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "call_site_kind: %d\n", (int) kind);
    }
#endif

    MR_write_byte(fp, (const char) kind);
}

static void
MR_write_byte(FILE *fp, const char byte)
{
#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "byte: %d\n", (int) byte);
    }
#endif
    putc(byte, fp);
}

// Write out a (non-negative) integer. The format we use is a multibyte format
// which uses the least significant 7 bits as data bits and the most
// significant bit to indicate whether there are more bytes following.
// Numbers are written most significant byte first.

static void
MR_write_num(FILE *fp, unsigned long num)
{
    unsigned char   pieces[sizeof(unsigned long) * 8 / 7 + 1];
    int             i;

#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "num: %ld\n", num);
    }
#endif

    MR_deep_assert(NULL, NULL, NULL, (MR_Integer) num >= 0);

    i = 0;
    do {
        pieces[i] = num & 0x7f;
        num = num >> 7;
        i++;
    } while (num != 0);

    i--;

    while (i > 0) {
        putc(pieces[i--] | 0x80, fp);
    }
    putc(pieces[0], fp);
}

static void
MR_write_fixed_size_int(FILE *fp, MR_uint_least64_t num)
{
    int i;

#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "fixed_size_int: %ld\n", num);
    }
#endif

    for (i = 0; i < MR_FIXED_SIZE_INT_BYTES; i++) {
        putc(num & ((1 << 8) - 1), fp);
        num = num >> 8;
    }
}

static void
MR_write_string(FILE *fp, const char *ptr)
{
    int i;
    int len;

#ifdef  MR_DEEP_PROFILING_DETAIL_DEBUG
    if (debug_fp != NULL) {
        fprintf(debug_fp, "string: <%s>\n", ptr);
    }
#endif

    len = strlen(ptr);
    MR_write_num(fp, len);
    for (i = 0; i < len; i++) {
        putc(ptr[i], fp);
    }
}

////////////////////////////////////////////////////////////////////////////

// This section of the file implements the hash tables that turn the addresses
// of ProcDynamic, ProcDynamic, and CallSiteDynamic nodes into node ids.
// We use our own routines instead of reusing the hash table routines in
// mercury_hash_table.c for efficiency. By writing our own code, we avoid
// several sources of overhead: higher order calls, separate calls to lookup
// a pointer and insert it if it isn't there, and the use of doubly-linked
// lists. Efficiency is reasonably important, since the tables can have
// millions of entries. Eventually, they should be implemented using
// dynamically sized hash tables (extendible hashing or linear hashing).

static MR_ProfilingHashTable *
MR_create_hash_table(int size)
{
    MR_ProfilingHashTable *ptr;
    int i;

    ptr = MR_NEW(MR_ProfilingHashTable);
    ptr->length = size;
    ptr->last_id = 0;
    ptr->nodes = MR_NEW_ARRAY(MR_ProfilingHashNode *, size);

    for (i = 0; i < size; i++) {
        ptr->nodes[i] = NULL;
    }

    return ptr;
}

////////////////////////////////////////////////////////////////////////////
// Type safe interfaces to the generic hash table routines.
//
// We declare those generic routines here to ensure that any calls to them
// from above this point get error messages from mgnuc.

static  MR_bool                 MR_hash_table_insert_INTERNAL(
                                    MR_ProfilingHashTable *table,
                                    const void *ptr, int *id,
                                    MR_bool *already_written,
                                    MR_bool init_written);
static  void                    MR_hash_table_flag_written_INTERNAL(
                                    MR_ProfilingHashTable *table,
                                    const void *ptr);
static  int                     MR_hash_table_check_all_written_INTERNAL(
                                    FILE *fp, const char *type,
                                    MR_ProfilingHashTable *table,
                                    void write_func(FILE *fp, const void *));

static MR_bool
MR_insert_proc_layout(const MR_ProcLayout *pl, int *id,
    MR_bool *already_written, MR_bool init_written)
{
    return MR_hash_table_insert_INTERNAL(MR_proc_layout_table,
        (const void *) pl, id, already_written, init_written);
}

static MR_bool
MR_insert_proc_dynamic(const MR_ProcDynamic *pd, int *id,
    MR_bool *already_written, MR_bool init_written)
{
    return MR_hash_table_insert_INTERNAL(MR_proc_dynamic_table,
        (const void *) pd, id, already_written, init_written);
}

static MR_bool
MR_insert_call_site_static(const MR_CallSiteStatic *css, int *id,
    MR_bool *already_written, MR_bool init_written)
{
    return MR_hash_table_insert_INTERNAL(MR_call_site_static_table,
        (const void *) css, id, already_written, init_written);
}

static MR_bool
MR_insert_call_site_dynamic(const MR_CallSiteDynamic *csd, int *id,
    MR_bool *already_written, MR_bool init_written)
{
    return MR_hash_table_insert_INTERNAL(MR_call_site_dynamic_table,
        (const void *) csd, id, already_written, init_written);
}

static void
MR_flag_written_proc_layout(const MR_ProcLayout *pl)
{
    MR_hash_table_flag_written_INTERNAL(MR_proc_layout_table,
        (const void *) pl);
}

static void
MR_flag_written_proc_dynamic(const MR_ProcDynamic *pd)
{
    MR_hash_table_flag_written_INTERNAL(MR_proc_dynamic_table,
        (const void *) pd);
}

static void
MR_flag_written_call_site_static(const MR_CallSiteStatic *css)
{
    MR_hash_table_flag_written_INTERNAL(MR_call_site_static_table,
        (const void *) css);
}

static void
MR_flag_written_call_site_dynamic(const MR_CallSiteDynamic *csd)
{
    MR_hash_table_flag_written_INTERNAL(MR_call_site_dynamic_table,
        (const void *) csd);
}

#define MR_hash_ptr(ptr, table) (((MR_Unsigned) (ptr) >> 2) % (table)->length)

static MR_bool
MR_hash_table_insert_INTERNAL(MR_ProfilingHashTable *table, const void *ptr,
    int *id, MR_bool *already_written, MR_bool init_written)
{
    int                     hash;
    MR_ProfilingHashNode    *node;

    if (ptr == NULL) {
        MR_fatal_error("NULL ptr in MR_hash_table_insert");
    }

    hash = MR_hash_ptr(ptr, table);
    node = table->nodes[hash];
    while (node != NULL) {
        if (node->item == ptr) {
            *id = node->id;
            if (already_written != NULL) {
                *already_written = node->written;
            }
            return MR_TRUE;
        }
        node = node->next;
    }

    node = MR_NEW(MR_ProfilingHashNode);
    node->item = ptr;
    node->id = ++table->last_id;
    node->written = init_written;
    node->next = table->nodes[hash];
    table->nodes[hash] = node;

    *id = node->id;
    if (already_written != NULL) {
        *already_written = MR_FALSE;
    }

    return MR_FALSE;
}

static void
MR_hash_table_flag_written_INTERNAL(MR_ProfilingHashTable *table,
    const void *ptr)
{
    int                     hash;
    MR_ProfilingHashNode    *node;

    if (ptr == NULL) {
        MR_fatal_error("NULL ptr in MR_hash_table_flag_written");
    }

    hash = MR_hash_ptr(ptr, table);
    node = table->nodes[hash];
    while (node != NULL) {
        if (node->item == ptr) {
            node->written = MR_TRUE;
            return;
        }
        node = node->next;
    }

    MR_fatal_error("MR_hash_table_flag_written: did not find node");
}

static void
MR_write_out_profiling_tree_check_unwritten(FILE *check_fp)
{
    int unwritten_csd;
    int unwritten_css;
    int unwritten_pd;
    int unwritten_ps;
    int any_unwritten;

    unwritten_ps = MR_hash_table_check_all_written_INTERNAL(check_fp,
        "ProcLayout", MR_proc_layout_table,
        MR_unwritten_pl_handler);
    unwritten_pd = MR_hash_table_check_all_written_INTERNAL(check_fp,
        "ProcDynamic", MR_proc_dynamic_table,
        MR_unwritten_pd_handler);
    unwritten_css = MR_hash_table_check_all_written_INTERNAL(check_fp,
        "CallSiteStatic", MR_call_site_static_table,
        MR_unwritten_css_handler);
    unwritten_csd = MR_hash_table_check_all_written_INTERNAL(check_fp,
        "CallSiteDynamic", MR_call_site_dynamic_table,
        MR_unwritten_csd_handler);
    any_unwritten = unwritten_ps + unwritten_pd +
        unwritten_css + unwritten_csd;

    if (unwritten_ps > 0 && check_fp != NULL) {
        fprintf(check_fp, "%d unwritten proc statics\n",
            unwritten_ps);
    }

    if (unwritten_pd > 0 && check_fp != NULL) {
        fprintf(check_fp, "%d unwritten proc dynamics\n",
            unwritten_pd);
    }

    if (unwritten_css > 0 && check_fp != NULL) {
        fprintf(check_fp, "%d unwritten call site statics\n",
            unwritten_css);
    }

    if (unwritten_csd > 0 && check_fp != NULL) {
        fprintf(check_fp, "%d unwritten call site dynamics\n",
            unwritten_csd);
    }

    if (any_unwritten > 0) {
        MR_fatal_error("UNWRITTEN nodes: Deep.data file corrupted\n");
    }
}

static void
MR_unwritten_css_handler(FILE *fp, const void *css)
{
    fprintf(stderr, "UNWRITTEN call site static %p\n", css);
}

static void
MR_unwritten_csd_handler(FILE *fp, const void *csd)
{
    fprintf(stderr, "UNWRITTEN call site dynamic %p\n", csd);
}

static void
MR_unwritten_pl_handler(FILE *fp, const void *pl)
{
    const MR_ProcLayout *proc_layout;
    const MR_ProcId     *procid;

    proc_layout = (const MR_ProcLayout *) pl;

    if (! MR_PROC_LAYOUT_HAS_PROC_ID(proc_layout)) {
        MR_fatal_error("MR_write_out_proc_layout_from_void: no proc_id\n");
    }

    fprintf(stderr, "UNWRITTEN proc layout %p:\n", pl);

    procid = &proc_layout->MR_sle_proc_id;
    if (MR_PROC_ID_IS_UCI(*procid)) {
        fprintf(stderr, "uci %s/%s/%s/%s/%d/%d\n",
            procid->MR_proc_uci.MR_uci_type_name,
            procid->MR_proc_uci.MR_uci_type_module,
            procid->MR_proc_uci.MR_uci_def_module,
            procid->MR_proc_uci.MR_uci_pred_name,
            procid->MR_proc_uci.MR_uci_type_arity,
            procid->MR_proc_uci.MR_uci_mode);
    } else {
        fprintf(stderr, "user %d/%s/%s/%s/%d/%d\n",
            procid->MR_proc_user.MR_user_pred_or_func,
            procid->MR_proc_user.MR_user_decl_module,
            procid->MR_proc_user.MR_user_def_module,
            procid->MR_proc_user.MR_user_name,
            procid->MR_proc_user.MR_user_pred_form_arity,
            procid->MR_proc_user.MR_user_mode);
    }
}

static void
MR_unwritten_pd_handler(FILE *fp, const void *pd)
{
    fprintf(stderr, "UNWRITTEN proc dynamic %p\n", pd);
}

static int
MR_hash_table_check_all_written_INTERNAL(FILE *fp, const char *type,
    MR_ProfilingHashTable *table, void write_func(FILE *, const void *))
{
    int                     i;
    int                     errors;
    MR_ProfilingHashNode    *node;

    errors = 0;
    for (i = 0; i < table->length ; i++) {
        for (node = table->nodes[i]; node != NULL; node = node->next) {
            if (! node->written) {
                errors++;
                if (fp != NULL) {
                    fprintf(fp, "unwritten %s %d at %p\n",
                        type, node->id, node->item);
                    fflush(fp);
                    (*write_func)(fp, node->item);
                }
            }
        }
    }

    return errors;
}

////////////////////////////////////////////////////////////////////////////

void
MR_deep_prof_init(void)
{
#ifdef  MR_DEEP_PROFILING_TIMING
    MR_init_time_profile_method();
#endif
}

static  void    MR_deep_tick_handler(int signum);

void
MR_deep_prof_turn_on_time_profiling(void)
{
#ifdef  MR_DEEP_PROFILING_TIMING
    MR_turn_on_time_profiling(MR_deep_tick_handler);
#endif
}

void
MR_deep_prof_turn_off_time_profiling(void)
{
#ifdef  MR_DEEP_PROFILING_TIMING
    MR_turn_off_time_profiling();
#endif
}

#ifdef  MR_DEEP_PROFILING_TIMING

static void
MR_deep_tick_handler(/*unused */ int signum)
{
    if (MR_inside_deep_profiling_code) {
        MR_quanta_inside_deep_profiling_code++;
    } else {
        MR_quanta_outside_deep_profiling_code++;
        MR_current_call_site_dynamic->MR_csd_own.MR_own_quanta++;
    }
}

#endif

#endif  // MR_DEEP_PROFILING
