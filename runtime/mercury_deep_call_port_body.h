/*
** Copyright (C) 2001-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** The implementation of {det,semi,non}_call_port_code_{ac,sr}.
**
** The code including this file should define the following macros:
** 
** MR_PROCNAME:			The name of the procedure whose body this is.
** MR_VERSION_AC or MR_VERSION_SR:
** 				Says whether the procedure whose body this is
**				is intended for use with or without
**				MR_USE_ACTIVATION_COUNTS.
** MR_NEED_NEW_OUTERMOST:	Says whether we need to know the new value of
**				the outermost activation pointer. Should be
**				true only for non_call_port_code_*.
**
** The code including this file should have the following variables in scope:
**
** ProcStatic:			The proc_static of the procedure whose call
**				port we are at.
** MiddleCSD:			The id of the current csd.
** TopCSD:			The id of the parent's csd.
** OldOutermostActivationPtr:	The id of the outermost activation of the
**				current user procedure before the call.
**				Needed only with MR_VERSION_SR.
** NewOutermostActivationPtr:	The id of the outermost activation of the
**				current user procedure after the call.
**				Needed only with MR_NEED_NEW_OUTERMOST.
*/

#ifdef MR_DEEP_PROFILING
{
	MR_CallSiteDynamic	*csd;
	MR_ProcStatic		*ps;

	MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
	if (MR_calldebug && MR_lld_print_enabled) {
		MR_print_deep_prof_vars(stdout, MR_PROCNAME);
	}
  #endif

	TopCSD = (MR_Word) MR_current_call_site_dynamic;
	MiddleCSD = (MR_Word) MR_next_call_site_dynamic;
	csd = MR_next_call_site_dynamic;
	MR_current_call_site_dynamic = csd;
  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
    #ifdef MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
	csd->MR_csd_own.MR_own_calls++;
    #else
	/* calls are computed from the other counts */
    #endif
  #endif

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
	/* After we copy it, MR_next_call_site_dynamic is not meaningful; */
	/* zeroing it makes debugging output less cluttered. */
	MR_next_call_site_dynamic = NULL;
  #endif

	ps = (MR_ProcStatic *) ProcStatic;
  #ifdef MR_VERSION_SR
	OldOutermostActivationPtr =
		(MR_Word) ps->MR_ps_outermost_activation_ptr;
  #endif

  #if defined(MR_VERSION_AC)
    #ifdef MR_USE_ACTIVATION_COUNTS
	MR_deep_assert(csd, ps, ps->MR_ps_activation_count == 0
		|| ps->MR_ps_outermost_activation_ptr != NULL);

      #ifdef MR_DEEP_PROFILING_STATISTICS
	if (csd->MR_csd_callee_ptr != NULL) {
		MR_deep_prof_call_old++;
	} else if (ps->MR_ps_activation_count > 0) {
		MR_deep_prof_call_rec++;
	} else {
		MR_deep_prof_call_new++;
	}
      #endif

	if (csd->MR_csd_callee_ptr != NULL) {
		if (ps->MR_ps_activation_count == 0) {
			ps->MR_ps_outermost_activation_ptr =
				csd->MR_csd_callee_ptr;
		}
	} else if (ps->MR_ps_activation_count > 0) {
		csd->MR_csd_callee_ptr = ps->MR_ps_outermost_activation_ptr;
	} else {
		MR_ProcDynamic	*pd;

		MR_new_proc_dynamic(pd, ps);
		csd->MR_csd_callee_ptr = pd;
		ps->MR_ps_outermost_activation_ptr = pd;
	}

	ps->MR_ps_activation_count++;
    #else
	MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS not enabled");
    #endif
  #elif defined(MR_VERSION_SR)
    #ifndef MR_USE_ACTIVATION_COUNTS
      #ifdef MR_DEEP_PROFILING_STATISTICS
	if (csd->MR_csd_callee_ptr != NULL) {
		MR_deep_prof_call_old++;
	} else if (ps->MR_ps_outermost_activation_ptr != NULL) {
		MR_deep_prof_call_rec++;
	} else {
		MR_deep_prof_call_new++;
	}
      #endif

	if (csd->MR_csd_callee_ptr != NULL) {
		ps->MR_ps_outermost_activation_ptr = csd->MR_csd_callee_ptr;
	} else if (ps->MR_ps_outermost_activation_ptr != NULL) {
		csd->MR_csd_callee_ptr = ps->MR_ps_outermost_activation_ptr;
	} else {
		MR_ProcDynamic	*pd;

		MR_new_proc_dynamic(pd, ps);
		csd->MR_csd_callee_ptr = pd;
		ps->MR_ps_outermost_activation_ptr = csd->MR_csd_callee_ptr;
	}
    #else
	MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS enabled");
    #endif
  #else
    #error "mercury_deep_call_port_body.h: neither MR_VERSION_AC nor MR_VERSION_SR"
  #endif

  #ifdef MR_NEED_NEW_OUTERMOST
	NewOutermostActivationPtr =
		(MR_Word) ps->MR_ps_outermost_activation_ptr;
  #endif

	MR_leave_instrumentation();
}
#else
	MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
