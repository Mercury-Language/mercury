/*****************************************************************
  File     : timing.c
  RCS      : $Id: timing.c,v 1.1 2000-03-10 05:17:23 dmo Exp $
  Author   : 
  Origin   : Sat Aug 12 15:20:42 1995
  Purpose  : Provide timing information for benchmarking
  Copyright: © 1995 Peter Schachte.  All rights reserved.

*****************************************************************/

#include "timing.h"

#ifdef AMIGA
#include <time.h>

millisec milli_time(void)
    {
	return (millisec) (clock()/1000);
    }
#else
/* assume it's unix, and we'll be using getrusage() */

#include <sys/time.h>
#include <sys/resource.h>

#if 0
#include <sys/rusage.h>
#else
void getrusage(int, struct rusage *);
#endif

millisec milli_time(void)
    {
	struct rusage p;

	getrusage(RUSAGE_SELF, &p);
	return (millisec)(p.ru_utime.tv_sec * 1000) +
	       (millisec)(p.ru_utime.tv_usec / 1000);
    }
#endif
