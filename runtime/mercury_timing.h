// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-1995, 1997, 2000, 2006 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_timing.h - interface to timing routines.
//  Defines `MR_CLOCK_TICKS_PER_SECOND'
//  and `MR_get_user_cpu_milliseconds()'.

#ifndef MERCURY_TIMING_H
#define MERCURY_TIMING_H

#include "mercury_conf.h"

#ifdef MR_HAVE_SYS_PARAM_H
#include <sys/param.h>      // for HZ
#endif

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>       // for sysconf() and _SC_CLK_TCK
#endif

#include <limits.h>         // CLK_TCK is defined here, on some systems

//
// Get the number of clock ticks per second.
// It is used when converting a clock_t value to a time in seconds.
// It may be defined by <sys/time.h>, but if it is not defined there,
// we may be able to use `sysconf(_SC_CLK_TCK)' or CLOCKS_PER_SEC or
// CLK_TCK instead.

#if defined(MR_HAVE_SYSCONF) && defined(_SC_CLK_TCK)
  #define MR_CLOCK_TICKS_PER_SECOND ((int) sysconf(_SC_CLK_TCK))
#elif defined(CLOCKS_PER_SEC)
  #define MR_CLOCK_TICKS_PER_SECOND CLOCKS_PER_SEC
#elif defined(CLK_TCK)
  #define MR_CLOCK_TICKS_PER_SECOND CLK_TCK
#elif defined(HZ)
  #define MR_CLOCK_TICKS_PER_SECOND HZ
#else
  // just leave it undefined
#endif

// MR_get_user_cpu_milliseconds() returns the CPU time consumed by the
// process, in milliseconds, from an arbitrary initial time.

extern int  MR_get_user_cpu_milliseconds(void);

// MR_get_real_milliseconds() returns the real time
// in milliseconds, from an arbitrary initial time.

extern int  MR_get_real_milliseconds(void);

#endif // MERCURY_TIMING_H
