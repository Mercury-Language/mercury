/*
** Copyright (C) 1995,2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*****************************************************************
  File     : timing.c
  Author   : 
  Origin   : Sat Aug 12 15:20:42 1995
  Purpose  : Provide timing information for benchmarking

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

int getrusage(int, struct rusage *);

millisec milli_time(void)
    {
	struct rusage p;

	if (getrusage(RUSAGE_SELF, &p) != 0) {
		fprintf(stderr, "getrusage failed");
		return 1;
	} else {
		return (millisec)(p.ru_utime.tv_sec * 1000) +
		       (millisec)(p.ru_utime.tv_usec / 1000);
	}
    }
#endif
