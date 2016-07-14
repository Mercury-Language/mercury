// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

#ifndef MERCURY_PROF_TIME_H
#define MERCURY_PROF_TIME_H

#define MR_CLOCK_TICKS_PER_PROF_SIG 5
#define MR_USEC_PER_SEC             1000000

extern  const char  *MR_time_method;

#if defined(MR_MPROF_PROFILE_TIME) || defined(MR_DEEP_PROFILING_TIMING)

typedef void    MR_time_signal_handler(int signum);

extern  void    MR_turn_on_time_profiling(MR_time_signal_handler handler);
extern  void    MR_turn_off_time_profiling(void);

extern  void    MR_init_time_profile_method(void);

#endif  // MR_MPROF_PROFILE_TIMING || MR_DEEP_PROFILING_TIMING

#endif  // MERCURY_PROF_TIME_H
