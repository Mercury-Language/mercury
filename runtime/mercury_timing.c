/*
** Copyright (C) 1993-1994,1997,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** file: timing.c
** main authors: fjh
**
**	Timing routines.
*/

#include "mercury_imp.h"

#ifdef HAVE_SYS_TIMES_H
  #include <sys/times.h>		/* for times() and `struct tms' */
#endif

#include "mercury_timing.h"

int
MR_get_user_cpu_miliseconds(void)
{
#ifndef MR_CLOCK_TICKS_PER_SECOND
	return -1;
#else
	const double ticks_per_milisecond = MR_CLOCK_TICKS_PER_SECOND / 1000.0;
	struct tms t;

	if (times(&t) == -1) {
		return -1;
	}
	return (int) (t.tms_utime / ticks_per_milisecond);
#endif
}
