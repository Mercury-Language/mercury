// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2002, 2004, 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// The implementation of {det,semi,non}_{exit,fail}_port_code_{ac,sr}.
//
// The code including this file should define the following macros:
//
// MR_PROCNAME:                 The name of the procedure whose body this is.
// MR_FAIL_PORT or MR_EXIT_PORT:
//                              Says which field to increment and whether the
//                              procedure has detism det or failure.
// MR_VERSION_AC or MR_VERSION_SR:
//                              Says whether the procedure whose body this is
//                              is intended for use with or without
//                              MR_USE_ACTIVATION_COUNTS.
//
// The code including this file should have the following variables in scope:
//
// MiddleCSD:                   The id of the current csd.
// TopCSD:                      The id of the parent's csd.
// OldOutermostActivationPtr:   The id of the outermost activation of the
//                              current user procedure before the current call
//                              to it. Needed only with MR_VERSION_SR.
//
// Note that the code in ML_trace_throw() in library/exception.m is based
// on the logic of this file, so if you make any changes here, you should
// consider similar changes there.

#ifdef MR_DEEP_PROFILING
{
    MR_CallSiteDynamic      *csd;
    const MR_ProcLayout     *pl;
    MR_ProcStatic           *ps;

  #ifdef MR_EXEC_TRACE
    if (! MR_disable_deep_profiling_in_debugger) {
    // The matching parenthesis is at the end of the file.
  #endif

    MR_enter_instrumentation();

    csd = (MR_CallSiteDynamic *) MiddleCSD;
    MR_deep_assert(csd, NULL, NULL, csd == MR_current_call_site_dynamic);

  #ifdef MR_DEEP_PROFILING_PORT_COUNTS
    // increment exit/fail count
    #if defined(MR_FAIL_PORT)
    csd->MR_csd_own.MR_own_fails++;
    #elif defined(MR_EXIT_PORT)
    csd->MR_csd_own.MR_own_exits++;
    #else
      #error "mercury_deep_leave_port_body.h: neither MR_FAIL_PORT nor MR_EXIT_PORT"
    #endif
  #endif

    MR_deep_assert(csd, NULL, NULL, csd->MR_csd_callee_ptr != NULL);
    pl = csd->MR_csd_callee_ptr->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

  #ifdef MR_DEEP_PROFILING_LOG
    if (MR_deep_prof_log_file != NULL) {
        const char  *portname;

    #if defined(MR_FAIL_PORT)
        portname = "failport";
    #elif defined(MR_EXIT_PORT)
        portname = "exitport";
    #else
        portname = "otherleaveport";
    #endif
        fprintf(MR_deep_prof_log_file, "%s(%ld,%ld,%ld).\n",
            portname, (long) MR_current_call_site_dynamic,
            (long) (MR_CallSiteDynamic *) TopCSD, (long) ps);
        fflush(MR_deep_prof_log_file);
    }
  #endif

  #if defined(MR_VERSION_AC)
    #ifdef MR_USE_ACTIVATION_COUNTS
    // Decrement activation count.
    ps->MR_ps_activation_count--;
    MR_deep_assert(csd, pl, ps, ps->MR_ps_activation_count >= 0);
    #else
    MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS not enabled");
    #endif
  #elif defined(MR_VERSION_SR)
    #ifndef MR_USE_ACTIVATION_COUNTS
    // Set outermost activation pointer.
    ps->MR_ps_outermost_activation_ptr =
        (MR_ProcDynamic *) OldOutermostActivationPtr;
    #else
    MR_fatal_error(MR_PROCNAME ": MR_USE_ACTIVATION_COUNTS enabled");
    #endif
  #else
    #error "mercury_deep_leave_port_body.h: neither MR_VERSION_AC nor MR_VERSION_SR"
  #endif

    // Set current csd.
    MR_current_call_site_dynamic = (MR_CallSiteDynamic *) TopCSD;

    MR_leave_instrumentation();

    // For MR_FAIL_PORT code, the failure we should execute here
    // is handled by code inserted by the compiler.

  #ifdef MR_EXEC_TRACE
    // The matching parenthesis is at the start of the file.
    }
  #endif
}
#else
    MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
