/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file provides macros that library/profiling_builtin.m uses to compose
** the code fragments passed to mercury_deep_rec_depth_body.h to create the
** bodies of
**	save_recursion_depth{1..9}
**	restore_recursion_depth_exit{1..9}
**	restore_recursion_depth_fail{1..9}
**
** These macros assume that the environments of their invocation define
** the following variables:
** 
** pd:	should point to the ProcDynamic structure of the caller.
** ps: 	should point to the ProcStatic structure of the caller.
*/

#define	MR_SAVE_DEPTH_ACTION(outer_count, csn)				\
	do {								\
		MR_CallSiteDynamic	*inner_csd;			\
									\
		MR_deep_assert(NULL, ps, csn <= ps->MR_ps_num_call_sites);\
		inner_csd = pd->MR_pd_call_site_ptr_ptrs[csn];		\
									\
		if (inner_csd != NULL) {				\
			outer_count = inner_csd->MR_csd_depth_count;	\
		} else {						\
			outer_count = 0;				\
		}							\
	} while (0)

#define	MR_RESTORE_DEPTH_ACTION(inc_field, outer_count, csn)		\
	do {								\
		MR_CallSiteDynamic	*inner_csd;			\
									\
		MR_deep_assert(NULL, ps, csn <= ps->MR_ps_num_call_sites);\
		inner_csd = pd->MR_pd_call_site_ptr_ptrs[csn];		\
									\
		if (inner_csd != NULL) {				\
			int	inner_count;				\
									\
			inner_count = inner_csd->MR_csd_depth_count;	\
			/* calls are computed from the other counts */	\
			inner_csd->MR_csd_own.inc_field += inner_count;	\
			inner_csd->MR_csd_depth_count = outer_count;	\
		} else {						\
			MR_deep_assert(inner_csd, ps, outer_count == 0);\
		}							\
	} while (0)

#define	MR_RESTORE_DEPTH_EXIT(outer_count, csn)				\
	MR_RESTORE_DEPTH_ACTION(MR_own_exits, (outer_count), (csn))

#define	MR_RESTORE_DEPTH_FAIL(outer_count, csn)				\
	MR_RESTORE_DEPTH_ACTION(MR_own_fails, (outer_count), (csn))

#define	MR_csn_vector_field(csn_vector, field_num)			\
	MR_field(MR_mktag(0), (csn_vector), (field_num))
