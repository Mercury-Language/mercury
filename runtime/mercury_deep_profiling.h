// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2004, 2006-2008, 2010 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_deep_profiling.h -- definitions for deep profiling.
//
// See ../deep_profiler/README for some pointers to documentation on
// deep profiling.

#ifndef MERCURY_DEEP_PROFILING_H
#define MERCURY_DEEP_PROFILING_H

#include "mercury_types.h"      // for MR_ConstString etc
#include "mercury_ho_call.h"
#include <stdio.h>

typedef enum {
    MR_callsite_normal_call,
    MR_callsite_special_call,
    MR_callsite_higher_order_call,
    MR_callsite_method_call,
    MR_callsite_callback
} MR_CallSiteKind;

struct MR_ProfilingMetrics_Struct {
#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
    unsigned                                MR_own_calls;
  #else
    // Calls are computed from the other fields.
  #endif
    unsigned                                MR_own_exits;
    unsigned                                MR_own_fails;
    unsigned                                MR_own_redos;
    unsigned                                MR_own_excps;
#endif
#ifdef MR_DEEP_PROFILING_TIMING
    volatile unsigned                       MR_own_quanta;
#endif
#ifdef MR_DEEP_PROFILING_CALL_SEQ
    unsigned                                MR_own_call_seqs;
#endif
#ifdef MR_DEEP_PROFILING_MEMORY
    unsigned                                MR_own_allocs;
    unsigned                                MR_own_words;
#endif

    // ANSI/ISO C requires non-empty structs
#if !defined(MR_DEEP_PROFILING_PORT_COUNTS) &&                          \
    !defined(MR_DEEP_PROFILING_TIMING) &&                               \
    !defined(MR_DEEP_PROFILING_MEMORY)
    unsigned                                dummy;
#endif
};

// The coverage point types. Please update the enum and type within
// mdbcomp/program_representation.m when updating this structure.

typedef enum {
    MR_cp_type_coverage_after,
    MR_cp_type_branch_arm
} MR_CPType;

typedef struct {
    const char                              *MR_cp_goal_path;
    const MR_CPType                         MR_cp_type;
} MR_CoveragePointStatic;

struct MR_CallSiteStatic_Struct {
    MR_CallSiteKind                         MR_css_kind;
    MR_ProcLayout                           *MR_css_callee_ptr_if_known;
    MR_ConstString                          MR_css_type_subst_if_known;
    MR_ConstString                          MR_css_file_name;
    int                                     MR_css_line_number;
    MR_ConstString                          MR_css_goal_path;
};

struct MR_ProcStatic_Struct {
    MR_ConstString                          MR_ps_file_name;
    int                                     MR_ps_line_number;
    int                                     MR_ps_is_in_interface;
    int                                     MR_ps_num_call_sites;
    const MR_CallSiteStatic                 *MR_ps_call_sites;
#ifdef MR_USE_ACTIVATION_COUNTS
    int                                     MR_ps_activation_count;
#endif
    MR_ProcDynamic                          *MR_ps_outermost_activation_ptr;
    int                                     MR_ps_cur_csd_stack_slot;
    int                                     MR_ps_next_csd_stack_slot;
    int                                     MR_ps_old_outermost_stack_slot;

#ifdef MR_DEEP_PROFILING_COVERAGE
    // The number of coverage points in a procedure and static information
    // about them are fixed at compile time, so they are associated with
    // proc statics rather than proc dynamics.

    const MR_Unsigned                       MR_ps_num_coverage_points;
    const MR_CoveragePointStatic * const    MR_ps_coverage_points_static;

#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC
    // Coverage data is kept in the ProcStatic structure if we are
    // collecting it statically. See also MR_dyn_coverage_points.

    MR_Unsigned * const                     MR_ps_coverage_points;
#endif
#endif
};

struct MR_CallSiteDynamic_Struct {
    MR_ProcDynamic                          *MR_csd_callee_ptr;
    MR_ProfilingMetrics                     MR_csd_own;
    unsigned                                MR_csd_depth_count;
};

struct MR_ProcDynamic_Struct {
    const MR_ProcLayout                     *MR_pd_proc_layout;
    MR_CallSiteDynamic                      **MR_pd_call_site_ptr_ptrs;
#ifdef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
    // Coverage data is kept in the ProcStatic structure initially, at a
    // later stage more fine-grained coverage data may be associated with
    // ProcDynamic if performance is not affected too much.

    MR_Unsigned                             *MR_pd_coverage_points;
#endif
};

struct MR_CallSiteDynList_Struct {
    MR_CallSiteDynamic                      *MR_csdlist_call_site;
    const void                              *MR_csdlist_key;
    MR_CallSiteDynList                      *MR_csdlist_next;
};

typedef enum {
    MR_deep_item_end = 0,
    MR_deep_item_call_site_static,
    MR_deep_item_call_site_dynamic,
    MR_deep_item_proc_static,
    MR_deep_item_proc_dynamic
} MR_DeepProfNextItem;

typedef enum {
    MR_no_more_modules,
    MR_next_module
} MR_MoreModules;

typedef enum {
    MR_no_more_procs,
    MR_next_proc
} MR_MoreProcs;

// The definition of this type should be kept in sync with the code of
// the string_proclabel_kind_* functions in compiler/prog_rep.m.

typedef enum {
    MR_proclabel_user_predicate,
    MR_proclabel_user_function,
    MR_proclabel_special
} MR_ProcLabelToken;

#define MR_enter_instrumentation()                                      \
    do { MR_inside_deep_profiling_code = MR_TRUE; } while (0)
#define MR_leave_instrumentation()                                      \
    do { MR_inside_deep_profiling_code = MR_FALSE; } while (0)

#ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
  #define MR_init_own_call_port(csd)                                    \
    do {                                                                \
        (csd)->MR_csd_own.MR_own_calls = 0;                             \
    } while (0)
#else
  #define MR_init_own_call_port(csd)                                    \
    ((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #define MR_init_own_ports(csd)                                        \
    do {                                                                \
        MR_init_own_call_port(csd);                                     \
        (csd)->MR_csd_own.MR_own_exits = 0;                             \
        (csd)->MR_csd_own.MR_own_fails = 0;                             \
        (csd)->MR_csd_own.MR_own_redos = 0;                             \
        (csd)->MR_csd_own.MR_own_excps = 0;                             \
    } while (0)
#else
  #define MR_init_own_ports(csd)                                        \
    ((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_TIMING
  #define MR_init_own_quanta(csd)                                       \
    do {                                                                \
        (csd)->MR_csd_own.MR_own_quanta = 0;                            \
    } while (0)
#else
  #define MR_init_own_quanta(csd)                                       \
    ((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_CALL_SEQ
  #define MR_init_own_call_seqs(csd)                                    \
    do {                                                                \
        (csd)->MR_csd_own.MR_own_call_seqs = 0;                         \
    } while (0)
#else
  #define MR_init_own_call_seqs(csd)                                    \
    ((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_MEMORY
  #define MR_init_own_memory(csd)                                       \
    do {                                                                \
        (csd)->MR_csd_own.MR_own_allocs = 0;                            \
        (csd)->MR_csd_own.MR_own_words = 0;                             \
    } while (0)
#else
  #define MR_init_own_memory(csd)                                       \
    ((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_TAIL_RECURSION
  #define MR_init_depth_count(csd)                                      \
    do {                                                                \
        (csd)->MR_csd_depth_count = 0;                                  \
    } while (0)
#else
  #define MR_init_depth_count(csd)                                      \
    ((void) 0)
#endif

#define MR_new_call_site_dynamic(newcsd)                                \
    do {                                                                \
        newcsd = MR_PROFILING_NEW(MR_CallSiteDynamic);                  \
                                                                        \
        newcsd->MR_csd_callee_ptr = NULL;                               \
        MR_init_own_ports(newcsd);                                      \
        MR_init_own_quanta(newcsd);                                     \
        MR_init_own_call_seqs(newcsd);                                  \
        MR_init_own_memory(newcsd);                                     \
        MR_init_depth_count(newcsd);                                    \
    } while (0)

#ifdef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
  #define MR_pd_init_coverage_points(pd, ps)                            \
    do {                                                                \
        int num_cps;                                                    \
        int cp_i;                                                       \
                                                                        \
        num_cps = (ps)->MR_ps_num_coverage_points;                      \
        if (num_cps) {                                                  \
            (pd)->MR_pd_coverage_points =                               \
              MR_PROFILING_NEW_ARRAY(MR_Unsigned, num_cps);             \
            for (cp_i = 0; cp_i < num_cps; cp_i++) {                    \
                (pd)->MR_pd_coverage_points[cp_i] = 0;                  \
            }                                                           \
        }                                                               \
    } while (0)
#else
  #define MR_pd_init_coverage_points(pd, ps)                            \
    ((void) 0)
#endif

// TODO: Consider merging these mallocs into one, this should improve
// efficiency.

#define MR_new_proc_dynamic(pd, pl)                                     \
    do {                                                                \
        MR_ProcStatic   *psl;                                           \
        int     npdi;                                                   \
                                                                        \
        (pd) = MR_PROFILING_NEW(MR_ProcDynamic);                        \
        (pd)->MR_pd_proc_layout = (pl);                                 \
        psl = (pl)->MR_sle_proc_static;                                 \
        (pd)->MR_pd_call_site_ptr_ptrs =                                \
            MR_PROFILING_NEW_ARRAY(MR_CallSiteDynamic *,                \
                psl->MR_ps_num_call_sites);                             \
                                                                        \
        for (npdi = 0; npdi < psl->MR_ps_num_call_sites; npdi++) {      \
            (pd)->MR_pd_call_site_ptr_ptrs[npdi] = NULL;                \
        }                                                               \
        MR_pd_init_coverage_points(pd, psl);                            \
    } while (0)

#ifdef  MR_DEEP_PROFILING_STATISTICS
  extern int    MR_deep_prof_search_len;
  extern void   MR_deep_profile_update_special_history(void);
  extern void   MR_deep_profile_update_closure_history(void);
  extern void   MR_deep_profile_update_method_history(void);

  #define MR_maybe_init_search_len()                                    \
    do { MR_deep_prof_search_len = 0; } while (0)
  #define MR_maybe_increment_search_len()                               \
    do { MR_deep_prof_search_len++; } while (0)
  #define MR_maybe_deep_profile_update_special_history()                \
    MR_deep_profile_update_special_history()
  #define MR_maybe_deep_profile_update_closure_history()                \
    MR_deep_profile_update_closure_history()
  #define MR_maybe_deep_profile_update_method_history()                 \
    MR_deep_profile_update_method_history()
#else
  #define MR_maybe_init_search_len()                                    \
    ((void) 0)
  #define MR_maybe_increment_search_len()                               \
    ((void) 0)
  #define MR_maybe_deep_profile_update_special_history()                \
    ((void) 0)
  #define MR_maybe_deep_profile_update_closure_history()                \
    ((void) 0)
  #define MR_maybe_deep_profile_update_method_history()                 \
    ((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_MOVE_TO_FRONT_LISTS
  #define MR_maybe_update_prev(csdlist, prev)                           \
    do { (prev) = (csdlist); } while (0)
  #define MR_maybe_move_to_front(csdlist, prev, pd, csn)                \
    do {                                                                \
        if (prev != NULL) {                                             \
            prev->MR_csdlist_next = csdlist->MR_csdlist_next;           \
            csdlist->MR_csdlist_next = (MR_CallSiteDynList *)           \
                pd->MR_pd_call_site_ptr_ptrs[(csn)];                    \
            pd->MR_pd_call_site_ptr_ptrs[(csn)] =                       \
                (MR_CallSiteDynamic *) csdlist;                         \
        }                                                               \
    } while (0)
#else
  #define MR_maybe_update_prev(csdlist, prev)                           \
        ((void) 0)
  #define MR_maybe_move_to_front(csdlist, prev, pd, csn)                \
        ((void) 0)
#endif

#define MR_search_csdlist(csdlist, prev, pd, csn, void_key)             \
    do {                                                                \
        (csdlist) = (MR_CallSiteDynList *) (pd)->                       \
            MR_pd_call_site_ptr_ptrs[(csn)];                            \
        MR_maybe_init_search_len();                                     \
        while ((csdlist) != NULL) {                                     \
            MR_maybe_increment_search_len();                            \
            if ((csdlist)->MR_csdlist_key == (void_key)) {              \
                MR_maybe_move_to_front((csdlist), (prev),               \
                    (pd), (csn));                                       \
                break;                                                  \
            }                                                           \
            MR_maybe_update_prev((csdlist), (prev));                    \
            (csdlist) = (csdlist)->MR_csdlist_next;                     \
        }                                                               \
    } while (0)

#define MR_make_and_link_csdlist(csdlist, newcsd, pd, csn, void_key)    \
    do {                                                                \
        (csdlist) = MR_PROFILING_NEW(MR_CallSiteDynList);               \
        (csdlist)->MR_csdlist_key = (void_key);                         \
        (csdlist)->MR_csdlist_call_site = (newcsd);                     \
        (csdlist)->MR_csdlist_next = (MR_CallSiteDynList *)             \
            (pd)->MR_pd_call_site_ptr_ptrs[(csn)];                      \
        pd->MR_pd_call_site_ptr_ptrs[(csn)]                             \
            = (MR_CallSiteDynamic *) (csdlist);                         \
    } while (0)

#define MR_make_and_link_csdlist_callback(csdlist, newcsd, void_key)    \
    do {                                                                \
        (csdlist) = MR_PROFILING_NEW(MR_CallSiteDynList);               \
        (csdlist)->MR_csdlist_key = (void_key);                         \
        (csdlist)->MR_csdlist_call_site = (newcsd);                     \
        (csdlist)->MR_csdlist_next = *MR_current_callback_site;         \
        *MR_current_callback_site = (csdlist);                          \
    } while (0)

#ifdef  MR_DEEP_CHECKS
  #define MR_deep_assert(csd, pl, ps, cond)                             \
    do {                                                                \
        if (!(cond)) {                                                  \
            MR_deep_assert_failed(csd, pl, ps,                          \
                MR_STRINGIFY(cond), __FILE__, __LINE__);                \
        }                                                               \
    } while (0)
#else
  #define MR_deep_assert(csd, pl, ps, cond)                             \
    ((void) 0)
#endif

#if defined(MR_DEEP_PROFILING) && defined(MR_EXEC_TRACE)
extern  MR_bool                 MR_disable_deep_profiling_in_debugger;
#endif

extern  MR_CallSiteDynamic      *MR_current_call_site_dynamic;
extern  MR_CallSiteDynamic      *MR_next_call_site_dynamic;
extern  MR_CallSiteDynList      **MR_current_callback_site;
extern  MR_CallSiteDynamic      *MR_root_call_sites[];

extern  volatile MR_bool        MR_inside_deep_profiling_code;
extern  volatile unsigned       MR_quanta_inside_deep_profiling_code;
extern  volatile unsigned       MR_quanta_outside_deep_profiling_code;

#ifdef MR_DEEP_PROFILING_CALL_SEQ
extern  unsigned                MR_deep_prof_cur_call_seq;
#endif

#ifdef MR_DEEP_PROFILING_STATISTICS

#define MR_MAX_CLOSURE_LIST_LENGTH 256

extern  int MR_deep_prof_prep_normal_new;
extern  int MR_deep_prof_prep_normal_old;
extern  int MR_deep_prof_prep_special_new;
extern  int MR_deep_prof_prep_special_old;
extern  int MR_deep_prof_prep_ho_new;
extern  int MR_deep_prof_prep_ho_old;
extern  int MR_deep_prof_prep_method_new;
extern  int MR_deep_prof_prep_method_old;
extern  int MR_deep_prof_prep_callback_new;
extern  int MR_deep_prof_prep_callback_old;
extern  int MR_deep_prof_prep_tail_new;
extern  int MR_deep_prof_prep_tail_old;

extern  int MR_deep_prof_call_new;
extern  int MR_deep_prof_call_rec;
extern  int MR_deep_prof_call_old;
extern  int MR_deep_prof_call_builtin_new;
extern  int MR_deep_prof_call_builtin_old;

#endif  // MR_DEEP_PROFILING_STATISTICS

#ifdef MR_DEEP_PROFILING_LOG
extern  FILE    *MR_deep_prof_log_file;

extern  void    MR_deep_log_proc_statics(FILE *fp);
#endif

extern  void    MR_deep_assert_failed(const MR_CallSiteDynamic *csd,
                    const MR_ProcLayout *pl, const MR_ProcStatic *ps,
                    const char *cond, const char *filename, int linenumber);
extern  void    MR_setup_callback(void *entry);

extern  void    MR_write_out_str_proc_label(FILE *deep_fp,
                    const MR_ProcId *procid);
extern  void    MR_write_out_user_proc_static(FILE *deep_fp, FILE *procrep_fp,
                    const MR_ProcLayoutUser *proc_layout);
extern  void    MR_write_out_uci_proc_static(FILE *deep_fp, FILE *procrep_fp,
                    const MR_ProcLayoutUCI *proc_layout);
extern  void    MR_write_out_proc_static(FILE *deep_fp, FILE *procrep_fp,
                    const MR_ProcLayout *proc_layout);
extern  void    MR_write_out_module_proc_reps_start(FILE *procrep_fp,
                    const MR_ModuleLayout *module_layout);
extern  void    MR_write_out_module_proc_reps_end(FILE *procrep_fp);
extern  void    MR_write_out_profiling_tree(void);

extern  void    MR_deep_prof_init(void);

extern  void    MR_deep_prof_turn_on_time_profiling(void);
extern  void    MR_deep_prof_turn_off_time_profiling(void);

#define MR_PROFILING_MALLOC(size)               MR_GC_malloc(size)
#define MR_PROFILING_NEW(type)                  MR_NEW(type)
#define MR_PROFILING_NEW_ARRAY(type, nelems)    MR_NEW_ARRAY(type, nelems)

#endif  // not MERCURY_DEEP_PROFILING_H
