// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2002, 2004, 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// The implementation of non_redo_port_code_{ac,sr}.
//
// The code including this file should define the following macros:
//
// MR_PROCNAME:                 The name of the procedure whose body this is.
// MR_VERSION_AC or MR_VERSION_SR:
//                              Says whether the procedure whose body this is
//                              is intended for use with or without
//                              MR_USE_ACTIVATION_COUNTS.
//
// The code including this file should have the following variables in scope:
//
// MiddleCSD:                   The id of the current csd.
// NewOutermostActivationPtr:   The id of the outermost activation of the
//                              procedure being backtracked into after the
//                              current call to it.

#ifdef MR_DEEP_PROFILING
{
    MR_CallSiteDynamic      *csd;
    const MR_ProcDynamic    *pd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

  #ifdef MR_EXEC_TRACE
    if (! MR_disable_deep_profiling_in_debugger) {
    // The matching parenthesis is at the end of the file.
  #endif

  #ifdef MR_DEEP_PROFILING_LOG
    if (MR_deep_prof_log_file != NULL) {
        MR_CallSiteDynamic  *midcsd;

        midcsd = (MR_CallSiteDynamic *) MiddleCSD;
        fprintf(MR_deep_prof_log_file, "redoport(%ld,%ld,%ld).\n",
            (long) MR_current_call_site_dynamic, (long) midcsd,
            (long) midcsd->MR_csd_callee_ptr->MR_pd_proc_layout->
                MR_sle_proc_static);
        fflush(MR_deep_prof_log_file);
    }
  #endif

    MR_enter_instrumentation();
    csd = (MR_CallSiteDynamic *) MiddleCSD;
    MR_current_call_site_dynamic = csd;

  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
    csd->MR_csd_own.MR_own_redos++;
  #endif

    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

  #if defined(MR_VERSION_AC)
    #ifdef MR_USE_ACTIVATION_COUNTS
    // Increment activation count.
    ps->MR_ps_activation_count++;
    ps->MR_ps_outermost_activation_ptr =
        (MR_ProcDynamic *) NewOutermostActivationPtr;
    #else
    MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS not enabled");
    #endif
  #elif defined(MR_VERSION_SR)
    #ifndef MR_USE_ACTIVATION_COUNTS
    // Set outermost activation pointer.
    ps->MR_ps_outermost_activation_ptr =
        (MR_ProcDynamic *) NewOutermostActivationPtr;
    #else
    MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS enabled");
    #endif
  #else
    #error "mercury_deep_redo_port_body.h: neither MR_VERSION_AC nor MR_VERSION_SR"
  #endif

  #ifdef MR_EXEC_TRACE
    // The matching parenthesis is at the start of the file.
    }
  #endif

    MR_leave_instrumentation();
}
#else
    MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
