/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This macro implements the following predicates:
**	save_recursion_depth{1..9}
**	restore_recursion_depth_exit{1..9}
**	restore_recursion_depth_fail{1..9}
**
** The code including this file should define the following macros:
** 
** MR_PROCNAME:		The name of the procedure whose body this is.
** MR_REC_DEPTH_BODY:	A sequence of statements to execute in the body
** 			after setting up the following variables:
**
** 			pd: points to the ProcDynamic structure of the caller.
** 			ps: points to the ProcStatic structure of the caller.
**
** The code including this file should have the following variable in scope:
**
** CSD:			The id of the current csd.
*/

#ifdef MR_DEEP_PROFILING
{
  #ifdef MR_DEEP_PROFILING_TAIL_RECURSION
	MR_CallSiteDynamic	*csd;
	MR_ProcDynamic		*pd;
	MR_ProcStatic		*ps;

	MR_enter_instrumentation();
	csd = (MR_CallSiteDynamic *) CSD;
	MR_deep_assert(csd, NULL, csd == MR_current_call_site_dynamic);
	pd = csd->MR_csd_callee_ptr;
	MR_deep_assert(csd, NULL, pd != NULL);
	ps = pd->MR_pd_proc_static;
	MR_deep_assert(csd, ps, ps != NULL);

	MR_REC_DEPTH_BODY

	MR_leave_instrumentation();
  #else
	MR_fatal_error(MR_PROCNAME ": tail recursion not enabled");
  #endif
}
#else
	MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
