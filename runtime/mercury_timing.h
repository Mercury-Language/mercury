/*
** Copyright (C) 1993-1995, 1997, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_timing.h - interface to timing routines.
**	Defines `MR_CLOCK_TICKS_PER_SECOND'
**	and `MR_get_user_cpu_miliseconds()'.
*/

#ifndef MERCURY_TIMING_H
#define MERCURY_TIMING_H

#include "mercury_conf.h"

#ifdef MR_HAVE_SYS_PARAM_H
#include <sys/param.h>		/* for HZ */
#endif

#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>		/* for sysconf() and _SC_CLK_TCK */
#endif

#include <limits.h>		/* CLK_TCK defined here, on some systems */

/* 
** `HZ' is the number of clock ticks per second.
** It is used when converting a clock_t value to a time in seconds.
** It may be defined by <sys/time.h>, but if it is not defined there,
** we may be able to use `sysconf(_SC_CLK_TCK)' or CLK_TCK instead.
*/
#ifdef HZ
  #define MR_CLOCK_TICKS_PER_SECOND	HZ
#elif defined(MR_HAVE_SYSCONF) && defined(_SC_CLK_TCK)
  #define MR_CLOCK_TICKS_PER_SECOND	((int) sysconf(_SC_CLK_TCK))
#elif defined(CLK_TCK)
  #define MR_CLOCK_TICKS_PER_SECOND	CLK_TCK
#else
  /* just leave it undefined */
#endif

/*
** MR_get_user_cpu_miliseconds() returns the CPU time consumed by the
** process, in miliseconds, from an arbitrary initial time.
*/
int MR_get_user_cpu_miliseconds(void);

#endif /* MERCURY_TIMING_H */
