/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** The implementation of {det,semi,non}_{exit,fail}_port_code_{ac,sr}.
**
** The code including this file should define the following macros:
** 
** MR_PROCNAME:			The name of the procedure whose body this is.
** MR_FAIL_PORT or MR_EXIT_PORT:
** 				Says which field to increment and whether the
**				procedure has detism det or failure.
** MR_VERSION_AC or MR_VERSION_SR:
** 				Says whether the procedure whose body this is
**				is intended for use with or without
**				MR_USE_ACTIVATION_COUNTS.
**
** The code including this file should have the following variables in scope:
**
** MiddleCSD:			The id of the current csd.
** TopCSD:			The id of the parent's csd.
** OldOutermostActivationPtr:	The id of the outermost activation of the
**				current user procedure before the current call
**				to it. Needed only with MR_VERSION_SR.
*/

#ifdef MR_DEEP_PROFILING
{
	MR_CallSiteDynamic	*csd;
	MR_ProcStatic		*ps;

	MR_enter_instrumentation();

	csd = (MR_CallSiteDynamic *) MiddleCSD;
	MR_deep_assert(csd, NULL, csd == MR_current_call_site_dynamic);

  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
	/* increment exit/fail count */
    #if defined(MR_FAIL_PORT)
	csd->MR_csd_own.MR_own_fails++;
    #elif defined(MR_EXIT_PORT)
	csd->MR_csd_own.MR_own_exits++;
    #else
      #error "mercury_deep_leave_port_body.h: neither MR_FAIL_PORT nor MR_EXIT_PORT"
    #endif
  #endif

	MR_deep_assert(csd, NULL, csd->MR_csd_callee_ptr != NULL);
	ps = csd->MR_csd_callee_ptr->MR_pd_proc_static;
	MR_deep_assert(csd, ps, ps != NULL);

  #if defined(MR_VERSION_AC)
    #ifdef MR_USE_ACTIVATION_COUNTS
	/* decrement activation count */
	ps->MR_ps_activation_count--;
	MR_deep_assert(csd, ps, ps->MR_ps_activation_count >= 0);
    #else
	MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS not enabled");
    #endif
  #elif defined(MR_VERSION_SR)
    #ifndef MR_USE_ACTIVATION_COUNTS
	/* set outermost activation pointer */
	ps->MR_ps_outermost_activation_ptr =
		(MR_ProcDynamic *) OldOutermostActivationPtr;
    #else
	MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS enabled");
    #endif
  #else
    #error "mercury_deep_leave_port_body.h: neither MR_VERSION_AC nor MR_VERSION_SR"
  #endif

	/* set current csd */
	MR_current_call_site_dynamic = (MR_CallSiteDynamic *) TopCSD;

	MR_leave_instrumentation();

  	/*
	** For MR_FAIL_PORT code, the failure we should execute here
	** is handled by code inserted by the compiler.
	*/
}
#else
	MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
