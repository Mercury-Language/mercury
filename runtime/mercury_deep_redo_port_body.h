/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** The implementation of non_redo_port_code_{ac,sr}.
**
** The code including this file should define the following macros:
** 
** MR_PROCNAME:			The name of the procedure whose body this is.
** MR_VERSION_AC or MR_VERSION_SR:
** 				Says whether the procedure whose body this is
**				is intended for use with or without
**				MR_USE_ACTIVATION_COUNTS.
**
** The code including this file should have the following variables in scope:
**
** MiddleCSD:			The id of the current csd.
** NewOutermostActivationPtr:	The id of the outermost activation of the
**				procedure being backtracked into after the
**				current call to it.
*/

#ifdef MR_DEEP_PROFILING
{
	MR_CallSiteDynamic	*csd;
	MR_ProcDynamic		*pd;
	MR_ProcStatic		*ps;

	MR_enter_instrumentation();
	csd = (MR_CallSiteDynamic *) MiddleCSD;
	MR_current_call_site_dynamic = csd;

  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
	csd->MR_csd_own.MR_own_redos++;
  #endif

	pd = csd->MR_csd_callee_ptr;
	MR_deep_assert(csd, NULL, pd != NULL);
	ps = pd->MR_pd_proc_static;
	MR_deep_assert(csd, ps, ps != NULL);

  #if defined(MR_VERSION_AC)
    #ifdef MR_USE_ACTIVATION_COUNTS
	/* increment activation count */
	ps->MR_ps_activation_count++;
	ps->MR_ps_outermost_activation_ptr =
		(MR_ProcDynamic *) NewOutermostActivationPtr;
    #else
	MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS not enabled");
    #endif
  #elif defined(MR_VERSION_SR)
    #ifndef MR_USE_ACTIVATION_COUNTS
	/* set outermost activation pointer */
	ps->MR_ps_outermost_activation_ptr =
		(MR_ProcDynamic *) NewOutermostActivationPtr;
    #else
	MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS enabled");
    #endif
  #else
    #error "mercury_deep_redo_port_body.h: neither MR_VERSION_AC nor MR_VERSION_SR"
  #endif

	MR_leave_instrumentation();
}
#else
	MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
