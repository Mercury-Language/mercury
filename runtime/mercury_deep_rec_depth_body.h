// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2002, 2004, 2006 The University of Melbourne.
// Copyright (C) 2016, 2018, 2026 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This macro implements the following predicates:
//  save_recursion_depth{1..9}
//  restore_recursion_depth_exit{1..9}
//  restore_recursion_depth_fail{1..9}
//
// The code including this file should define the following macros:
//
// MR_PROCNAME:
// The name of the procedure whose body this is.
//
// MR_CSD:
// The id of the current csd.
// (NOTE: We used to require a variable with a specific name (CSD)
// being in scope when this file is #included, but that does not work
// (because it gets a singleton variable warning) if that argument
// of that name is not visibly used in the body of the foreign_proc
// that does the #include. With the current arrangement, the definition
// of MR_CSD as CSD makes CSD a non-singleton variable.
//
// MR_REC_DEPTH_BODY:
// A sequence of statements to execute in the body after setting up
// the following variables:
//
//  pd: points to the ProcDynamic structure of the caller.
//  ps: points to the ProcStatic structure of the caller.
//
// The code including this file should have the following variable in scope:

#ifdef MR_DEEP_PROFILING
{
  #ifdef MR_DEEP_PROFILING_TAIL_RECURSION
    MR_CallSiteDynamic  *csd;
    MR_ProcDynamic      *pd;
    const MR_ProcLayout *pl;
    MR_ProcStatic       *ps;

    MR_enter_instrumentation();
    csd = MR_CSD;
    MR_deep_assert(csd, NULL, NULL, csd == MR_current_call_site_dynamic);
    pd = csd->MR_csd_callee_ptr;
    MR_deep_assert(csd, NULL, NULL, pd != NULL);
    pl = pd->MR_pd_proc_layout;
    MR_deep_assert(csd, pl, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(csd, pl, ps, ps != NULL);

    MR_REC_DEPTH_BODY

    MR_leave_instrumentation();
  #else
    MR_fatal_error(MR_PROCNAME ": tail recursion not enabled");
  #endif
}
#else
    MR_fatal_error(MR_PROCNAME ": deep profiling not enabled");
#endif
