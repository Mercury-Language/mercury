/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_deep_profiling.h -- definitions for deep profiling.
*/

#ifndef MERCURY_DEEP_PROFILING_H
#define MERCURY_DEEP_PROFILING_H

#include "mercury_stack_layout.h"
#include "mercury_ho_call.h"
#include <stdio.h>

typedef enum {
	MR_normal_call,
	MR_special_call,
	MR_higher_order_call,
	MR_method_call,
	MR_callback
} MR_CallSite_Kind;

struct MR_ProfilingMetrics_Struct {
#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
	unsigned				MR_own_calls;
  #else
	/* calls are computed from the other fields */
  #endif
	unsigned				MR_own_exits;
	unsigned				MR_own_fails;
	unsigned				MR_own_redos;
#endif
#ifdef MR_DEEP_PROFILING_TIMING
	volatile unsigned			MR_own_quanta;
#endif
#ifdef MR_DEEP_PROFILING_MEMORY
	unsigned				MR_own_allocs;
	unsigned				MR_own_words;
#endif
	/* ANSI/ISO C requires non-empty structs */
#if !defined(MR_DEEP_PROFILING_PORT_COUNTS) && \
	!defined(MR_DEEP_PROFILING_TIMING) && !defined(MR_DEEP_PROFILING_MEMORY)
	unsigned				dummy;
#endif
};

struct MR_CallSiteStatic_Struct {
    	MR_CallSite_Kind			MR_css_kind;
	MR_ProcStatic				*MR_css_callee_ptr_if_known;
	MR_ConstString				MR_css_type_subst_if_known;
	MR_ConstString				MR_css_file_name;
	int					MR_css_line_number;
	MR_ConstString				MR_css_goal_path;
};

struct MR_ProcStatic_Struct {
	MR_Proc_Id				MR_ps_proc_id;
	MR_ConstString				MR_ps_file_name;
	int					MR_ps_line_number;
	int					MR_ps_is_in_interface;
	int					MR_ps_num_call_sites;
	const MR_CallSiteStatic			*MR_ps_call_sites;
#ifdef MR_USE_ACTIVATION_COUNTS
	int					MR_ps_activation_count;
#endif
	MR_ProcDynamic				*MR_ps_outermost_activation_ptr;
};

struct MR_User_ProcStatic_Struct {
	MR_User_Proc_Id				MR_ps_proc_id;
	MR_ConstString				MR_ps_file_name;
	int					MR_ps_line_number;
	int					MR_ps_is_in_interface;
	int					MR_ps_num_call_sites;
	const MR_CallSiteStatic			*MR_ps_call_sites;
#ifdef MR_USE_ACTIVATION_COUNTS
	int					MR_ps_activation_count;
#endif
	MR_ProcDynamic				*MR_ps_outermost_activation_ptr;
};

struct MR_Compiler_ProcStatic_Struct {
	MR_Compiler_Proc_Id			MR_ps_proc_id;
	MR_ConstString				MR_ps_file_name;
	int					MR_ps_line_number;
	int					MR_ps_is_in_interface;
	int					MR_ps_num_call_sites;
	const MR_CallSiteStatic			*MR_ps_call_sites;
#ifdef MR_USE_ACTIVATION_COUNTS
	int					MR_ps_activation_count;
#endif
	MR_ProcDynamic				*MR_ps_outermost_activation_ptr;
};

struct MR_CallSiteDynamic_Struct {
	MR_ProcDynamic				*MR_csd_callee_ptr;
	MR_ProfilingMetrics			MR_csd_own;
	unsigned				MR_csd_depth_count;
};

struct MR_ProcDynamic_Struct {
	MR_ProcStatic				*MR_pd_proc_static;
	MR_CallSiteDynamic			**MR_pd_call_site_ptr_ptrs;
};

struct MR_CallSiteDynList_Struct {
	MR_CallSiteDynamic			*MR_csdlist_call_site;
	const void				*MR_csdlist_key;
	MR_CallSiteDynList			*MR_csdlist_next;
};

typedef enum {
	MR_deep_token_end = 0,
	MR_deep_token_call_site_static,
	MR_deep_token_call_site_dynamic,
	MR_deep_token_proc_static,
	MR_deep_token_proc_dynamic,
	MR_deep_token_normal_call,
	MR_deep_token_special_call,
	MR_deep_token_higher_order_call,
	MR_deep_token_method_call,
	MR_deep_token_callback,
	MR_deep_token_isa_predicate,
	MR_deep_token_isa_function,
	MR_deep_token_isa_compiler_generated
} MR_Profile_Encoding_Token;

#define	MR_enter_instrumentation()					\
	do { MR_inside_deep_profiling_code = MR_TRUE; } while (0)
#define	MR_leave_instrumentation()					\
	do { MR_inside_deep_profiling_code = MR_FALSE; } while (0)

#ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
  #define MR_init_own_call_port(csd)					\
  	do {								\
		(csd)->MR_csd_own.MR_own_calls = 0;			\
	} while (0)
#else
  #define MR_init_own_call_port(csd)					\
  	((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_PORT_COUNTS
  #define MR_init_own_ports(csd)					\
  	do {								\
		MR_init_own_call_port(csd);				\
		(csd)->MR_csd_own.MR_own_exits = 0;			\
		(csd)->MR_csd_own.MR_own_fails = 0;			\
		(csd)->MR_csd_own.MR_own_redos = 0;			\
	} while (0)
#else
  #define MR_init_own_ports(csd)					\
  	((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_TIMING
  #define MR_init_own_quanta(csd)					\
  	do {								\
		(csd)->MR_csd_own.MR_own_quanta = 0;			\
	} while (0)
#else
  #define MR_init_own_quanta(csd)					\
  	((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_MEMORY
  #define MR_init_own_memory(csd)					\
  	do {								\
		(csd)->MR_csd_own.MR_own_allocs = 0;			\
		(csd)->MR_csd_own.MR_own_words = 0;			\
	} while (0)
#else
  #define MR_init_own_memory(csd)					\
  	((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_TAIL_RECURSION
  #define MR_init_depth_count(csd)					\
  	do {								\
		(csd)->MR_csd_depth_count = 0;				\
	} while (0)
#else
  #define MR_init_depth_count(csd)					\
  	((void) 0)
#endif

#define	MR_new_call_site_dynamic(newcsd)				\
	do {								\
		newcsd = MR_PROFILING_MALLOC(MR_CallSiteDynamic);	\
									\
		newcsd->MR_csd_callee_ptr = NULL;			\
		MR_init_own_ports(newcsd);				\
		MR_init_own_quanta(newcsd);				\
		MR_init_own_memory(newcsd);				\
		MR_init_depth_count(newcsd);				\
	} while (0)

#define	MR_new_proc_dynamic(pd, ps)					\
	do {								\
		int	i;						\
									\
		(pd) = MR_PROFILING_MALLOC(MR_ProcDynamic);		\
		(pd)->MR_pd_proc_static = (ps);				\
		(pd)->MR_pd_call_site_ptr_ptrs =			\
			MR_PROFILING_MALLOC_ARRAY(MR_CallSiteDynamic *,	\
				(ps)->MR_ps_num_call_sites);		\
									\
		for (i = 0; i < (ps)->MR_ps_num_call_sites; i++) {	\
			(pd)->MR_pd_call_site_ptr_ptrs[i] = NULL;	\
		}							\
	} while (0)

#ifdef	MR_DEEP_PROFILING_STATISTICS
  extern int	MR_deep_prof_search_len;
  extern void	MR_deep_profile_update_special_history(void);
  extern void	MR_deep_profile_update_closure_history(void);
  extern void	MR_deep_profile_update_method_history(void);

  #define MR_maybe_init_search_len()					\
  	do { MR_deep_prof_search_len = 0; } while(0)
  #define MR_maybe_increment_search_len()				\
  	do { MR_deep_prof_search_len++; } while (0)
  #define MR_maybe_deep_profile_update_special_history()	 	\
	MR_deep_profile_update_special_history()
  #define MR_maybe_deep_profile_update_closure_history() 		\
	MR_deep_profile_update_closure_history()
  #define MR_maybe_deep_profile_update_method_history() 		\
	MR_deep_profile_update_method_history()
#else
  #define MR_maybe_init_search_len()					\
	((void) 0)
  #define MR_maybe_increment_search_len()				\
	((void) 0)
  #define MR_maybe_deep_profile_update_special_history()		\
	((void) 0)
  #define MR_maybe_deep_profile_update_closure_history()		\
	((void) 0)
  #define MR_maybe_deep_profile_update_method_history()			\
	((void) 0)
#endif

#ifdef MR_DEEP_PROFILING_MOVE_TO_FRONT_LISTS
  #define MR_maybe_update_prev(csdlist, prev)				\
	do { (prev) = (csdlist); } while (0)
  #define MR_maybe_move_to_front(csdlist, prev, pd, csn)		\
	do {								\
		if (prev != NULL) {					\
			prev->MR_csdlist_next = csdlist->MR_csdlist_next;\
			csdlist->MR_csdlist_next = (MR_CallSiteDynList *)\
				pd->MR_pd_call_site_ptr_ptrs[(csn)];	\
			pd->MR_pd_call_site_ptr_ptrs[(csn)] =		\
				(MR_CallSiteDynamic *) csdlist;		\
		}							\
	} while (0)
#else
  #define MR_maybe_update_prev(csdlist, prev)				\
		((void) 0)
  #define MR_maybe_move_to_front(csdlist, prev, pd, csn)		\
		((void) 0)
#endif

#define	MR_search_csdlist(csdlist, prev, pd, csn, void_key)		\
	do {								\
		(csdlist) = (MR_CallSiteDynList *) (pd)->		\
			MR_pd_call_site_ptr_ptrs[(csn)];		\
		MR_maybe_init_search_len();				\
		while ((csdlist) != NULL) {				\
			MR_maybe_increment_search_len();		\
			if ((csdlist)->MR_csdlist_key == (void_key)) {	\
				MR_maybe_move_to_front((csdlist), (prev),\
					(pd), (csn));			\
				break;					\
			}						\
			MR_maybe_update_prev((csdlist), (prev));	\
			(csdlist) = (csdlist)->MR_csdlist_next;		\
		}							\
	} while (0)

#define	MR_make_and_link_csdlist(csdlist, newcsd, pd, csn, void_key)	\
	do {								\
		(csdlist) = MR_PROFILING_MALLOC(MR_CallSiteDynList);	\
		(csdlist)->MR_csdlist_key = (void_key);			\
		(csdlist)->MR_csdlist_call_site = (newcsd);		\
		(csdlist)->MR_csdlist_next = (MR_CallSiteDynList *)	\
			(pd)->MR_pd_call_site_ptr_ptrs[(csn)];		\
		pd->MR_pd_call_site_ptr_ptrs[(csn)]			\
			= (MR_CallSiteDynamic *) (csdlist);		\
	} while (0)

#define	MR_make_and_link_csdlist_callback(csdlist, newcsd, void_key)	\
	do {								\
		(csdlist) = MR_PROFILING_MALLOC(MR_CallSiteDynList);	\
		(csdlist)->MR_csdlist_key = (void_key);			\
		(csdlist)->MR_csdlist_call_site = (newcsd);		\
		(csdlist)->MR_csdlist_next = *MR_current_callback_site;	\
		*MR_current_callback_site = (csdlist);			\
	} while (0)

#ifdef	MR_DEEP_CHECKS
  #define MR_deep_assert(csd, ps, cond)					\
 	do {								\
		if (!(cond)) {						\
			MR_deep_assert_failed(csd, ps, MR_STRINGIFY(cond),\
				__FILE__, __LINE__);			\
		}							\
	} while (0)
#else
  #define MR_deep_assert(csd, ps, cond)					\
  	((void) 0)
#endif

extern	MR_CallSiteDynamic		*MR_current_call_site_dynamic;
extern	MR_CallSiteDynamic		*MR_next_call_site_dynamic;
extern	MR_CallSiteDynList		**MR_current_callback_site;
extern	MR_CallSiteDynamic		*MR_root_call_sites[];

extern	volatile MR_bool		MR_inside_deep_profiling_code;
extern	volatile unsigned		MR_quanta_inside_deep_profiling_code;
extern	volatile unsigned		MR_quanta_outside_deep_profiling_code;

#ifdef MR_DEEP_PROFILING_STATISTICS

#define MR_MAX_CLOSURE_LIST_LENGTH 256

extern	int	MR_deep_prof_prep_normal_new;
extern	int	MR_deep_prof_prep_normal_old;
extern	int	MR_deep_prof_prep_special_new;
extern	int	MR_deep_prof_prep_special_old;
extern	int	MR_deep_prof_prep_ho_new;
extern	int	MR_deep_prof_prep_ho_old;
extern	int	MR_deep_prof_prep_method_new;
extern	int	MR_deep_prof_prep_method_old;
extern	int	MR_deep_prof_prep_callback_new;
extern	int	MR_deep_prof_prep_callback_old;
extern	int	MR_deep_prof_prep_tail_new;
extern	int	MR_deep_prof_prep_tail_old;

extern	int	MR_deep_prof_call_new;
extern	int	MR_deep_prof_call_rec;
extern	int	MR_deep_prof_call_old;
extern	int	MR_deep_prof_call_builtin_new;
extern	int	MR_deep_prof_call_builtin_old;

#endif	/* MR_DEEP_PROFILING_STATISTICS */

extern	void	MR_deep_assert_failed(const MR_CallSiteDynamic *csd,
			const MR_ProcStatic *ps, const char *cond,
			const char *filename, int linenumber);
extern	void	MR_setup_callback(void *entry);
extern	void	MR_write_out_proc_static(FILE *fp, const MR_ProcStatic *ptr);
extern	void	MR_write_out_profiling_tree(void);

extern	void	MR_deep_prof_init(void);

extern	void	MR_deep_prof_turn_on_time_profiling(void);
extern	void	MR_deep_prof_turn_off_time_profiling(void);

#define MR_PROFILING_MALLOC(type)		MR_NEW(type)
#define MR_PROFILING_MALLOC_ARRAY(type, nelems) MR_NEW_ARRAY(type, nelems)

#endif	/* not MERCURY_DEEP_PROFILING_H */
